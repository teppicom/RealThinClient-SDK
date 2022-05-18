{
  "Message Client provider"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br>)

  @exclude
}
unit rtcMsgCliProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSystem,
  rtcThrPool,
  rtcLog,

  rtcInfo,
  rtcConn,
  rtcConnProv,
  rtcTransports;

const
  LOG_MSGCLI_EXCEPTIONS:boolean=False;

type
  TRtcMessageClientProvider = class;

  RtcMsgCliException = class(Exception);

  TRtcMsgClientThread = class(TRtcThread)
  public
    RtcConn:TRtcMessageClientProvider;
    Releasing:boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    function RunJob:boolean; override;

    procedure OpenConn;
    procedure CloseConn(_lost:boolean);
    end;

  TRtcMessageClientProvider = class(TRtcThrClientProvider)
  private
    Client_Thread:TRtcMsgClientThread;

    RequestStream, ResponseStream:TMemoryStream;

    FServer:IRTCMessageReceiver;

    Forc:boolean;

    FOnInvalidResponse:TRtcBasicEvent;

    FResponseBuffer:TRtcHugeByteArray;

    FReadBuffer:RtcByteArray;

    FMaxHeaderSize:integer;
    FMaxResponseSize:integer;

    FHeaderOut:boolean;
    LenToWrite:int64;

    FRequest:TRtcClientRequest;
    FResponse:TRtcClientResponse;

    FDataWasSent:boolean;
    FFixupRequest: TRtcClientRequestFixup;

  protected
    procedure CleanUp; override;

    function ClientThreadNIL:boolean;
    function GetClientThread:TRtcThread; override;

    procedure TriggerInvalidResponse; virtual;

    procedure AcceptResponse; virtual;

    function _Active:boolean;

    procedure OpenConnection;

  public
    constructor Create; override;

    procedure Connect(Force:boolean=False;Reconnecting:boolean=False); override;
    procedure Disconnect; override;
    function ReleaseMe(FromInside:boolean):boolean; override;

    procedure InternalDisconnect; override;

    procedure LeavingEvent; virtual;

    procedure SetTriggerInvalidResponse(Event:TRtcBasicEvent);

    procedure WriteHeader; overload; virtual;
    procedure WriteHeader(const Header_Text:RtcString); overload; virtual;

    procedure WriteEx(const s:RtcByteArray; SendNow:boolean=True); override;
    function ReadEx:RtcByteArray; override;

    function PeekEx:RtcByteArray; override;
    procedure PokeEx(const s:RtcByteArray); override;

    procedure Write(const s:RtcString; SendNow:boolean=True); override;
    function Read:RtcString; override;

    property Request:TRtcClientRequest read FRequest write FRequest;
    property Response:TRtcClientResponse read FResponse write FResponse;

    // Max. allowed size of the first (status) line in response header
    property MaxResponseSize:integer read FMaxResponseSize write FMaxResponseSize;
    // Max. allowed size of the complete response Header
    property MaxHeaderSize:integer read FMaxHeaderSize write FMaxHeaderSize;

    property Server:IRTCMessageReceiver read FServer write FServer;

    property FixupRequest:TRtcClientRequestFixup read FFixupRequest write FFixupRequest;
    end;

implementation

const
  CRLF = RtcString(#13#10);
  END_MARK = RtcString(#13#10+#13#10);

var
  Message_WSStop,
  Message_WSRelease,
  Message_WSOpenConn,
  Message_WSCloseConn:TRtcBaseMessage;

{ TRtcMessageClientProvider }

constructor TRtcMessageClientProvider.Create;
  begin
  inherited;

  RequestStream:=TMemoryStream.Create;
  ResponseStream:=TMemoryStream.Create;

  FResponseBuffer:=TRtcHugeByteArray.Create;

  FDataWasSent:=False;
  SetLength(FReadBuffer,32000);

  FFixupRequest:=nil;
  end;

procedure TRtcMessageClientProvider.CleanUp;
  begin
  try
    try
      Silent:=True;
      InternalDisconnect;
      TriggerConnectionClosing;

      if inThread then
        begin
        if not ReleaseMe(True) then
          Log('TRtcMessageClientProvider.CleanUp - ReleaseMe(TRUE)?','ERROR');
        end
      else
        begin
        if not ReleaseMe(False) then
          Log('TRtcMessageClientProvider.CleanUp - ReleaseMe(FALSE)?','ERROR');
        end;
    finally
      try
        inherited;
      finally
        FServer:=nil;
        RtcFreeAndNil(FResponseBuffer);
        RtcFreeAndNil(RequestStream);
        RtcFreeAndNil(ResponseStream);
        SetLength(FReadBuffer,0);
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcMessageClientProvider.CleanUp',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcMessageClientProvider.SetTriggerInvalidResponse(Event: TRtcBasicEvent);
  begin
  FOnInvalidResponse:=Event;
  end;

procedure TRtcMessageClientProvider.TriggerInvalidResponse;
  begin
  if assigned(FOnInvalidResponse) then
    FOnInvalidResponse;
  end;

function TRtcMessageClientProvider.ClientThreadNIL: boolean;
  begin
  Result:=Client_Thread=nil;
  end;

function TRtcMessageClientProvider.GetClientThread: TRtcThread;
  begin
  Result:=Client_Thread;
  end;

procedure TRtcMessageClientProvider.Connect(Force: boolean=False; Reconnecting:boolean=False);
  begin
  if assigned(Client_Thread) then
    begin
    if inThread then
      OpenConnection
    else
      TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
    end
  else
    begin
    if GetMultiThreaded then
      begin
      Client_Thread:=TRtcMsgClientThread.Create;
      Client_Thread.RtcConn:=self;
      Forc:=Force;
      TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
      end
    else
      OpenConnection;
    end;
  end;

procedure TRtcMessageClientProvider.OpenConnection;
  begin
  if State>=conActivating then Exit; // already connected !!!

  try
    Lost:=True;
    Closing:=False;
    Silent:=False;

    Request.Init;
    Response.Clear;

    State:=conActivating;

    TriggerConnectionOpening(Forc);

    if not assigned(FServer) then
      raise RtcMsgCliException.Create('Error connecting, Server component not assigned!');

    RequestStream.Clear;
    ResponseStream.Clear;

    State:=conActive;

    TriggerConnecting;
    TriggerConnect;
  except
    on E:Exception do
      begin
      TriggerConnectionClosing;
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    end;
  end;

procedure TRtcMessageClientProvider.Disconnect;
  begin
  Lost:=False;
  if assigned(Client_Thread) and not inThread then
    TRtcThread.PostJob(Client_Thread, Message_WSCloseConn)
  else
    InternalDisconnect;
  end;

procedure TRtcMessageClientProvider.InternalDisconnect;
  begin
  Closing:=True;

  if State<=conClosing then Exit;

  State:=conClosing;

  RequestStream.Clear;
  ResponseStream.Clear;

  if State=conClosing then
    begin
    TriggerDisconnecting;
    TriggerConnectionClosing;

    try
      if Lost then
        TriggerConnectLost // TriggerConnectLost will call TriggerDisconnect
      else
        TriggerDisconnect;
    except
      end;

    FHeaderOut:=False;
    
    TriggerReadyToRelease;
    end;
  end;

procedure TRtcMessageClientProvider.PokeEx(const s:RtcByteArray);
  begin
  if not _Active then Exit;
  if assigned(FResponseBuffer) then
    begin
    FResponseBuffer.Clear;
    if length(s)>0 then
      FResponseBuffer.AddEx(s);
    end;
  end;

function TRtcMessageClientProvider.PeekEx: RtcByteArray;
  begin
  if not _Active then
    begin
    SetLength(Result,0);
    Exit;
    end;

  if FResponseBuffer.Size>0 then
    Result:=FResponseBuffer.GetEx
  else
    SetLength(Result,0);
  end;

function TRtcMessageClientProvider.ReadEx: RtcByteArray;
  begin
  if not _Active then
    begin
    SetLength(Result,0);
    Exit;
    end;

  if FResponseBuffer.Size>0 then
    begin
    Result:=FResponseBuffer.GetEx;
    FResponseBuffer.Clear;
    end
  else
    SetLength(Result,0);
  end;

function TRtcMessageClientProvider.Read: RtcString;
  begin
  if not _Active then
    begin
    SetLength(Result,0);
    Exit;
    end;

  if FResponseBuffer.Size>0 then
    begin
    Result:=FResponseBuffer.Get;
    FResponseBuffer.Clear;
    end
  else
    SetLength(Result,0);
  end;

procedure TRtcMessageClientProvider.WriteHeader;
  var
    s:RtcString;
  begin
  SetLength(s,0);
  if not _Active then Exit;

  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  FixupRequest.Fixup(Request);

  if Request.Close then
    s:= Request.Method+' '+Request.URI+' HTTP/1.0'+CRLF+
       Request.HeaderText+'Connection: close'+CRLF+CRLF
  else
    s:= Request.Method+' '+Request.URI+' HTTP/1.1'+CRLF+
       Request.HeaderText+CRLF;

{$IFDEF RTC_BYTESTRING}
  RequestStream.Write(s[1],length(s));
{$ELSE}
  RequestStream.Write(RtcStringToBytes(s)[0],length(s));
{$ENDIF}

  FDataOut:=length(s);
  try
    TriggerDataOut;
  finally
    FDataOut:=0;
    end;

  Request.Started:=True;
  Request.Active:=True;

  LenToWrite:=Request.ContentLength;

  ResponseStream.Clear;

  FDataWasSent:=True;
  end;

procedure TRtcMessageClientProvider.WriteHeader(const Header_Text: RtcString);
  begin
  if not _Active then Exit;

  Response.HeaderText:=Header_Text;
  WriteHeader;
  end;

procedure TRtcMessageClientProvider.WriteEx(const s: RtcByteArray; SendNow:boolean=True);
  begin
  if not _Active then Exit;

  if length(s)=0 then Exit;

  if not Request.Active then
    raise Exception.Create('Sending data without header.');

  RequestStream.Write(s[0], length(s));

  FDataOut:=length(s);
  Dec(LenToWrite,FDataOut);
  Request.ContentOut:=Request.ContentOut + FDataOut;
  try
    TriggerDataOut;
  finally
    FDataOut:=0;
    end;

  FDataWasSent:=True; // will call DataSent
  end;

procedure TRtcMessageClientProvider.Write(const s: RtcString; SendNow:boolean=True);
  begin
  if not _Active then Exit;

  if length(s)=0 then Exit;

  if not Request.Active then
    raise Exception.Create('Sending data without header.');

  {$IFDEF RTC_BYTESTRING}
  RequestStream.Write(s[1], length(s));
  {$ELSE}
  RequestStream.Write(RtcStringToBytes(s)[0], length(s));
  {$ENDIF}

  FDataOut:=length(s);
  Dec(LenToWrite,FDataOut);
  Request.ContentOut:=Request.ContentOut + FDataOut;
  try
    TriggerDataOut;
  finally
    FDataOut:=0;
    end;

  FDataWasSent:=True; // will call DataSent
  end;

procedure TRtcMessageClientProvider.LeavingEvent;
  begin
  If _Active and FDataWasSent then
    begin
    FDataWasSent:=False;

    if LenToWrite=0 then
      begin
      Request.Complete:=True;
      TriggerDataSent;
      if Request.Complete and not Response.Done then
        AcceptResponse;
      end
    else
      TriggerDataSent;
    end;
  TriggerReadyToRelease;
  end;

procedure TRtcMessageClientProvider.AcceptResponse;
  var
    s:RtcByteArray;

    StatusLine,
    HeadStr:RtcString;

    len,len2,
    HeadLen,
    MyPos:integer;

    FChunked,
    FHaveResponse,
    FResponseLine:boolean;

    FChunkState:integer;

    LenToRead:int64;
    InBuffer:RtcByteArray;

  function HexToInt(const s:RtcString):integer;
    var
      i,len:integer;
      c:RtcChar;
    begin
    Result:=0;
    len:=length(s);
    i:=1;
    while len>0 do
      begin
      c:=s[len];
      {$IFDEF RTC_BYTESTRING}
      if c in ['1'..'9'] then
        Result:=Result+i*(Ord(c)-Ord('0'))
      else if c in ['A'..'F'] then
        Result:=Result+i*(Ord(c)-Ord('A')+10)
      else if c in ['a'..'f'] then
        Result:=Result+i*(Ord(c)-Ord('a')+10);
      {$ELSE}
      if Pos(c,'123456789')>0 then
        Result:=Result+i*(Ord(c)-Ord('0'))
      else if Pos(c,'ABCDEF')>0 then
        Result:=Result+i*(Ord(c)-Ord('A')+10)
      else if Pos(c,'abcdef')>0 then
        Result:=Result+i*(Ord(c)-Ord('a')+10);
      {$ENDIF}
      i:=i*16;Dec(len);
      end;
    end;

  procedure ResponseError;
    begin
    FResponseLine:=False;
    TriggerInvalidResponse;
    end;

  procedure ClearResponse;
    begin
    FResponseBuffer.Clear;

    FResponseLine:=False;
    FResponse.Clear;
    LenToRead:=-1;
    end;

  procedure ProcessData(const data:RtcByteArray);
    var
      InBuf,s:RtcString;
      FDone:boolean;
    begin
    FDone:=False;
    AddBytes(InBuffer,data);

    repeat
      if not FHaveResponse then // Don't have the header yet ...
        begin
        if not FResponseLine then
          begin
          if length(InBuffer)>5 then
            InBuf:=Upper_Case( RtcBytesToString(InBuffer,0,5) )
          else
            InBuf:=Upper_Case( RtcBytesToString(InBuffer) );
          // Accept streaming data as response
          if ((length(InBuf)>=5) and (InBuf<>'HTTP/')) or
             ((length(InBuf)=1) and (InBuf<>'H')) or
             ((length(InBuf)=2) and (InBuf<>'HT')) or
             ((length(InBuf)=3) and (InBuf<>'HTT')) or
             ((length(InBuf)=4) and (InBuf<>'HTTP')) then
            begin
            ClearResponse;

            Response.Receiving:=True;
            Response.Started:=True;

            FHaveResponse:=True;
            FResponseLine:=True;
            LenToRead:=-1; // Unlimited length (streaming data until disconnected)

            Continue;
            end;

          MyPos:=PosEx(CRLF,InBuffer);
          if (MaxResponseSize>0) and
             ( (MyPos>MaxResponseSize+1) or
               ((MyPos<=0) and (length(InBuffer)>MaxResponseSize+length(CRLF))) ) then
            begin
            ClearResponse;

            ResponseError;
            Exit;
            end
          else if (MyPos>0) then
            begin
            ClearResponse;

            StatusLine:= RtcBytesToString(InBuffer,0,MyPos-1);
            DelBytes(InBuffer,MyPos+length(CRLF)-1);

            if Upper_Case(Copy(StatusLine,1,5))<>'HTTP/' then
              begin
              ResponseError;
              Exit;
              end;

            Response.Receiving:=True;
            Response.Started:=True;

            { Our line probably looks like this:
              HTTP/1.1 200 OK }
            MyPos:=PosEx(' ',StatusLine); // first space before StatusCode
            if MyPos<=0 then
              begin
              ResponseError;
              Exit;
              end;
            Delete(StatusLine,1,MyPos); // remove 'HTTP/1.1 '

            MyPos:=PosEx(' ',StatusLine); // space after StatusCode
            if MyPos<=0 then
              begin
              ResponseError;
              Exit;
              end;

            s:=Copy(StatusLine,1,MyPos-1); // StatusCode
            Delete(StatusLine,1,MyPos); // StatusText

            if (s<>'') and (StatusLine<>'') then
              begin
              try
                Response.StatusCode:=Str2Int(s);
                Response.StatusText:=StatusLine;
              except
                // if there is something wrong with this, just ignore the exception
                end;
              end;

            FResponseLine:=True;
            end;
          end;

        if FResponseLine then
          begin
          // See if we can get the whole header ...
          HeadLen:=PosEx(CRLF, InBuffer);
          if HeadLen<>1 then
            HeadLen:=PosEx(END_MARK, InBuffer);

          if HeadLen=1 then
            begin
            // Delete CRLF from the body
            DelBytes(InBuffer,2);

            if Response.StatusCode=100 then
              begin // special handling of the "100:Continuing" Http status code
              FResponseLine:=False;
              Continue;
              end;

            // No Header: disconnect closes the response.
            Request.Close:=True;

            if (Request.Method='HEAD') or
               (Response.StatusCode=204) or
               (Response.StatusCode=304) or
               ( (Response.StatusCode>=100) and (Response.StatusCode<=199) ) then
              begin
              FChunked:=False;
              LenToRead:=0;
              end;

            FHaveResponse:=True;
            end
          else if (MaxHeaderSize>0) and
             ( (HeadLen>MaxHeaderSize) or
               ((HeadLen<=0) and (length(InBuffer)>MaxHeaderSize+length(END_MARK))) ) then
            begin
            ResponseError;
            Exit;
            end
          else if HeadLen>0 then
            begin
            // Separate header from the body
            HeadStr:= RtcBytesToString(InBuffer, 0, HeadLen+length(END_MARK)-1);
            DelBytes(InBuffer,HeadLen+length(END_MARK)-1);

            FHaveResponse:=True;

            // Scan for all header attributes ...
            MyPos:=Pos(CRLF, HeadStr);
            while (MyPos>1) do // at least 1 character inside line
              begin
              StatusLine:=Copy(HeadStr,1,MyPos-1);
              Delete(HeadStr,1,MyPos+Length(CRLF)-1);

              MyPos:=PosEx(':',StatusLine);
              if MyPos>0 then
                begin
                s:=Trim(Copy(StatusLine,1,MyPos-1));
                Delete(StatusLine,1,MyPos);
                StatusLine:=Trim(StatusLine);
                Response[s]:=StatusLine;
                end;

              MyPos:=Pos(CRLF, HeadStr);
              end;

            if Response.ValueCS['CONTENT-LENGTH']<>'' then
              LenToRead:=Response.ContentLength;

            s:=Response.ValueCS['TRANSFER-ENCODING'];
            if s<>'' then
              begin
              s:=Upper_Case(s);
              if s='CHUNKED' then
                begin
                FChunked:=True;
                FChunkState:=0;
                end;
              end;

            s:=Response.ValueCS['CONNECTION'];
            if s<>'' then
              begin
              s:=Upper_Case(s);
              if s='CLOSE' then
                Request.Close:=True
              else if s='KEEP-ALIVE' then
                Request.Close:=False;
              end;

            if LenToRead=-1 then
              Request.Close:=True;

            if (Request.Method='HEAD') or
               (Response.StatusCode=204) or
               (Response.StatusCode=304) or
               ( (Response.StatusCode>=100) and (Response.StatusCode<=199) ) then
              begin
              FChunked:=False;
              LenToRead:=0;
              end;

            StatusLine:='';
            HeadStr:='';
            end;
          end;
        end;

      if FHaveResponse then // Processing a response ...
        begin
        if FChunked then // Read data as chunks
          begin
          if (FChunkState=0) and (length(InBuffer)>0) then // 1.step = read chunk size
            begin
            MyPos:=PosEx(CRLF,InBuffer);
            if MyPos>0 then
              begin
              StatusLine:=Trim( RtcBytesToString(InBuffer,0,MyPos-1) );
              DelBytes(InBuffer,MyPos+1);

              LenToRead:=HexToInt(StatusLine);

              FChunkState:=1; // ready to read data
              end;
            end;

          if (FChunkState=1) and (length(InBuffer)>0) then // 2.step = read chunk data
            begin
            if (LenToRead>length(InBuffer)) then // need more than we have
              begin
              Response.ContentIn:=Response.ContentIn+length(InBuffer);

              if LenToRead>0 then
                Dec(LenToRead, length(InBuffer));

              FResponseBuffer.AddEx(InBuffer);
              SetLength(InBuffer,0);

              inherited TriggerDataReceived;

              Response.Started:=False;
              end
            else
              begin
              if LenToRead>0 then
                begin
                Response.ContentIn:=Response.ContentIn+LenToRead;

                FResponseBuffer.AddEx(InBuffer,LenToRead);

                DelBytes(InBuffer,LenToRead);
                LenToRead:=0;
                FChunkState:=2; // this is not the last chunk, ready to read CRLF
                end
              else
                FChunkState:=3; // this was last chunk, ready to read CRLF
              end;
            end;

          if (FChunkState>=2) and (length(InBuffer)>=2) then // 3.step = close chunk
            begin
            LenToRead:=-1;
            DelBytes(InBuffer,2); // Delete CRLF

            if FChunkState=2 then
              begin
              FChunkState:=0;
              end
            else
              begin
              Response.Done:=True;
              Request.Active:=False;
              FHaveResponse:=False; // get ready for next request
              FChunked:=False;
              FChunkState:=0;
              FResponseLine:=False;
              FHeaderOut:=False;

              FDone:=True;
              end;

            inherited TriggerDataReceived;

            Response.Started:=False;
            end;
          end
        else // Read data as stream or with predefined length
          begin
          if (LenToRead>0) or (LenToRead=-1) then
            begin
            if (LenToRead>length(InBuffer)) or
               (LenToRead=-1) then // need more than we have
              begin
              Response.ContentIn:=Response.ContentIn+length(InBuffer);

              if LenToRead>0 then
                Dec(LenToRead, length(InBuffer));

              FResponseBuffer.AddEx(InBuffer);

              SetLength(InBuffer,0);
              end
            else
              begin
              Response.ContentIn:=Response.ContentIn+LenToRead;

              FResponseBuffer.AddEx(InBuffer,LenToRead);

              DelBytes(InBuffer,LenToRead);

              LenToRead:=0;
              Response.Done:=True;
              Request.Active:=False;
              FHaveResponse:=False; // get ready for next request
              FChunked:=False;
              FResponseLine:=False;
              FHeaderOut:=False;

              FDone:=True;
              end;
            end
          else
            begin
            Response.Done:=True;
            Request.Active:=False;
            FHaveResponse:=False; // get ready for next request
            FChunked:=False;
            FResponseLine:=False;
            FHeaderOut:=False;

            FDone:=True;
            end;

          inherited TriggerDataReceived;

          Response.Started:=False;
          end;
        end
      else
        Break; // Failing to fetch a header will break the loop.

      until (length(InBuffer)=0) or FDone;
    end;

  begin
  if not _Active then Exit;

  if not assigned(FServer) then
    raise RtcMsgCliException.Create('Error! Server component removed!');

  FServer.ProcessMessage(RequestStream, ResponseStream);

  FResponseBuffer.Clear;

  FChunked:=False;
  FChunkState:=0;

  FHaveResponse:=False;
  FResponseLine:=False;
  LenToRead:=0;

  RequestStream.Clear;
  ResponseStream.Position:=0;

  try
    while (ResponseStream.Position<ResponseStream.Size) and not Response.Done do
      begin
      len:=ResponseStream.Size-ResponseStream.Position;
      if len>32000 then len:=32000;

      SetLength(s,len);
      len2:=ResponseStream.Read(s[0],len);

      FDataIn:=len;
      TriggerDataIn;

      ProcessData(s);
      if len2<len then Break;
      end;
  finally
    ResponseStream.Clear;
    if _Active and not Request.Active then
      FResponseBuffer.Clear;
    end;
  end;

function TRtcMessageClientProvider._Active: boolean;
  begin
  Result:=not Closing and (FState>=conActivating);
  end;

function TRtcMessageClientProvider.ReleaseMe(FromInside:boolean):boolean;
  begin
  Result:=False;
  if assigned(Client_Thread) then
    begin
    if FromInside then
      begin
      Client_Thread.Releasing:=True;
      Client_Thread.InternalKill;
      Client_Thread:=nil;
      end
    else if Silent then
      begin
      TRtcThread.PostJob(Client_Thread, Message_WSStop, True, True);
      if RtcWaitFor(ClientThreadNIL,ClientThreadNIL,RTC_WAITFOR_RELEASE_PROVIDER)<>wait_OK then
        if LOG_EXCEPTIONS then
          Log(RtcString(ClassName)+'.ReleaseMe ('+PeerAddr+':'+PeerPort+') TIMEOUT!','ERROR');
      Sleep(RTC_WAITFOR_PROVIDER_FINISH);
      Result:=True;
      end
    else
      TRtcThread.PostJob(Client_Thread, Message_WSRelease, True);
    end
  else
    Result:=inherited ReleaseMe(FromInside);
  end;

{ TRtcMsgClientThread }

constructor TRtcMsgClientThread.Create;
  begin
  inherited;
  Releasing:=False;
  RtcConn:=nil;
  end;

procedure TRtcMsgClientThread.OpenConn;
  begin
  RtcConn.OpenConnection;
  end;

procedure TRtcMsgClientThread.CloseConn(_lost:boolean);
  begin
  try
    if assigned(RtcConn) then
      begin
      RtcConn.Lost:=_lost;
      RtcConn.InternalDisconnect;
      end;
  except
    on E:Exception do
      if LOG_MSGCLI_EXCEPTIONS then
        Log('MsgClientThread.CloseConn : RtConn.InternalDisconnect',E);
    end;
  end;

destructor TRtcMsgClientThread.Destroy;
  begin
  try
    try
      if assigned(RtcConn) then
        begin
        if Releasing then
          begin
          RtcConn.Client_Thread:=nil;
          RtcFreeAndNil(RtcConn);
          end
        else
          begin
          RtcConn.InternalDisconnect;
          RtcConn.Client_Thread:=nil;
          RtcConn:=nil;
          end;
        end;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log('TRtcMsgClientThread.Destroy : RtConn.Free/InternalDisconnect',E,'ERROR');
      end;
    RtcConn:=nil;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcMsgClientThread.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcMsgClientThread.RunJob:boolean;
  begin
  try
    if Job=Message_WSOpenConn then
      begin
      OpenConn;
      Result:=False;
      end
    else if Job=Message_WSCloseConn then
      begin
      CloseConn(false);
      Result:=False;
      end
    else if Job=Message_WSStop then
      begin
      if assigned(RtcConn) then
        begin
        RtcConn.Client_Thread:=nil;
        RtcConn:=nil;
        end;
      Result:=True; // Free;
      end
    else if Job=Message_WSRelease then
      begin
      Releasing:=True;
      Result:=True; // Free;
      end
    else
      Result:=inherited RunJob;
  except
    on E:Exception do
      begin
      if LOG_MSGCLI_EXCEPTIONS then
        Log('MsgClientThread.RunJob',E);
      CloseConn(true);
      Result:=True; // raise;
      end;
    end;
  end;

type
  TRtcMsgCliProvUnit=class
    public
    constructor Create;
    destructor Destroy; override;
    end;

var
  MyMsgCli:TRtcMsgCliProvUnit;

{ TMyWinInet }

constructor TRtcMsgCliProvUnit.Create;
  begin
  inherited;
  Message_WSOpenConn:=TRtcBaseMessage.Create;
  Message_WSCloseConn:=TRtcBaseMessage.Create;
  Message_WSStop:=TRtcBaseMessage.Create;
  Message_WSRelease:=TRtcBaseMessage.Create;
  end;

destructor TRtcMsgCliProvUnit.Destroy;
  begin
  try
    RtcFreeAndNil(Message_WSOpenConn);
    RtcFreeAndNil(Message_WSCloseConn);
    RtcFreeAndNil(Message_WSStop);
    RtcFreeAndNil(Message_WSRelease);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcMsgCliProvUnit.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcMsgCliProv Initializing ...','DEBUG');{$ENDIF}

MyMsgCli:=TRtcMsgCliProvUnit.Create;

{$IFDEF RTC_DEBUG} Log('rtcMsgCliProv Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcMsgCliProv Finalizing ...','DEBUG');{$ENDIF}

CloseThreadPool;
RtcFreeAndNil(MyMsgCli);

{$IFDEF RTC_DEBUG} Log('rtcMsgCliProv Finalized.','DEBUG');{$ENDIF}
end.
