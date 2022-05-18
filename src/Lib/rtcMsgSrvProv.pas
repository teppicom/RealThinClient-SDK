{
  "Message Server Connection Provider"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br>)

  @exclude
}
unit rtcMsgSrvProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSystem,
  rtcLog,

  rtcInfo,
  rtcConn,
  rtcConnProv;

type
  TRtcMessageServerProvider = class(TRtcNoThrServerProvider)
  private
    FRequestLine,
    FHaveRequest:boolean;
    InBuffer:RtcByteArray;
    LenToRead:integer;

    OutStream:TStream;

    FRequest:TRtcServerRequest;
    FResponse:TRtcServerResponse;

    FRequestBuffer:TRtcHugeByteArray;

    LenToWrite:int64; // number of bytes to write out using inherited Write()
    LenToSend:int64;

    FHeaderOut:boolean;
    FFixupRequest: TRtcServerRequestFixup;

  protected
    procedure CleanUp; override;

    procedure CopyFrom(Dup:TRtcConnectionProvider);

  public
    constructor Create; override;

    function GetParent:TRtcConnectionProvider; override;

    procedure Connect;
    procedure ExecuteRequest(InStream, _OutStream:TStream);

    procedure Listen(Restarting:boolean=False); override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    procedure TriggerDataSent; override;
    procedure TriggerDataOut; override;

    procedure WriteHeader; overload;
    procedure WriteHeader(const Header_Text:RtcString); overload;

    procedure WriteEx(const ResultData:RtcByteArray; SendNow:boolean=True); override;
    function ReadEx:RtcByteArray; override;

    function PeekEx:RtcByteArray; override;
    procedure PokeEx(const s:RtcByteArray); override;

    procedure Write(const ResultData:RtcString; SendNow:boolean=True); override;
    function Read:RtcString; override;

    property Request:TRtcServerRequest read FRequest write FRequest;
    property Response:TRtcServerResponse read FResponse write FResponse;

    property FixupRequest:TRtcServerRequestFixup read FFixupRequest write FFixupRequest;
    end;

implementation

{ TRtcMessageServerProvider }

const
  CRLF = RtcString(#13#10);
  END_MARK = RtcString(#13#10#13#10);

constructor TRtcMessageServerProvider.Create;
  begin
  inherited;

  FRequestLine:=False;
  FHaveRequest:=False;
  SetLength(InBuffer,0);
  LenToRead:=0;

  FRequestBuffer:=TRtcHugeByteArray.Create;

  LenToWrite:=0;
  LenToSend:=0;
  FHeaderOut:=False;

  FRequest:=nil;
  FResponse:=nil;
  FFixupRequest:=nil;
  end;

procedure TRtcMessageServerProvider.CleanUp;
  begin
  try
    Enter;
    try
      LenToWrite:=0;
      LenToSend:=0;
      FHeaderOut:=False;
    finally
      Leave;
      try
        inherited;
      finally
        RtcFreeAndNil(FRequestBuffer);
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcMessageServerProvider.CleanUp',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcMessageServerProvider.WriteHeader;
  var
    s:RtcString;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  if Request.Close then
    s:= 'HTTP/1.0 '+Int2Str(Response.StatusCode)+' '+Response.StatusText+CRLF+
       Response.HeaderText+'Connection: close'+CRLF+CRLF
  else
    s:= 'HTTP/1.1 '+Int2Str(Response.StatusCode)+' '+Response.StatusText+CRLF+
       Response.HeaderText+CRLF;

  Response.Sending:=True;
  Response.Started:=True;

  if Response.SendContent then
    if (Request.Method='HEAD') or
       (Response.StatusCode=204) or
       (Response.StatusCode=304) or
       ( (Response.StatusCode>=100) and (Response.StatusCode<=199) ) then
      Response.SendContent:=False;

  if Response.SendContent and
    (Response.ValueCS['CONTENT-LENGTH']='')  then // streaming data
    begin
    raise Exception.Create('Streaming content not supported by a Message Server.');
    LenToWrite:=-1;
    LenToSend:=-1;
    end
  else
    begin
    if not Response.SendContent then
      Response['Content-Length']:='';

    LenToWrite:=Response.ContentLength;
    LenToSend:=LenToWrite;
    end;

  Response.Sent:=LenToWrite=0;
  if Response.Sent then
    TriggerLastWrite;

  FHeaderOut:=True;

{$IFDEF RTC_BYTESTRING}
  OutStream.Write(s[1], length(s));
{$ELSE}
  OutStream.Write(RtcStringToBytes(s)[0], length(s));
{$ENDIF}

  FDataOut:=0;
  if Response.Sent then
    try
      TriggerDataOut;
    finally
      TriggerDataSent;
    end;
  end;

procedure TRtcMessageServerProvider.WriteHeader(const Header_Text:RtcString);
  var
    s:RtcString;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  if Header_Text<>'' then
    begin
    Response.HeaderText:=Header_Text;

    if Request.Close then
      s:= 'HTTP/1.0 '+Int2Str(Response.StatusCode)+' '+Response.StatusText+CRLF+
         Response.HeaderText+'Connection: close'+CRLF+CRLF
    else
      s:= 'HTTP/1.1 '+Int2Str(Response.StatusCode)+' '+Response.StatusText+CRLF+
         Response.HeaderText+CRLF;
    end
  else
    begin
    raise Exception.Create('Streaming content not supported by a Message Server.');
    SetLength(s,0);
    Request.Close:=True;
    end;

  Response.Sending:=True;
  Response.Started:=True;

  if Response.SendContent then
    if (Request.Method='HEAD') or
       (Response.StatusCode=204) or
       (Response.StatusCode=304) or
       ( (Response.StatusCode>=100) and (Response.StatusCode<=199) ) then
      Response.SendContent:=False;

  if Response.SendContent and
    (Response.ValueCS['CONTENT-LENGTH']='')  then // streaming data
    begin
    raise Exception.Create('Streaming content not supported by ISAPI.');
    LenToWrite:=-1;
    LenToSend:=-1;
    end
  else
    begin
    if not Response.SendContent then
      Response['Content-Length']:='';

    LenToWrite:=Response.ContentLength;
    LenToSend:=LenToWrite;
    end;

  Response.Sent:=LenToWrite=0;
  if Response.Sent then
    TriggerLastWrite;

  FHeaderOut:=True;

{$IFDEF RTC_BYTESTRING}
  OutStream.Write(s[1], length(s));
{$ELSE}
  OutStream.Write(RtcStringToBytes(s)[0], length(s));
{$ENDIF}

  if Response.Sent then
    begin
    FDataOut:=0;
    try
      TriggerDataOut;
    finally
      TriggerDataSent;
      end;
    end;
  end;

procedure TRtcMessageServerProvider.WriteEx(const ResultData: RtcByteArray; SendNow:boolean=True);
  var
    len:cardinal;
  begin
  if length(ResultData)=0 then Exit;

  if not FHeaderOut then
    raise Exception.Create('Trying to send Data without Header. Call WriteHeader before Write.');

  if LenToWrite>=0 then
    begin
    if length(ResultData)>LenToWrite then
      raise Exception.Create('Trying to send more Data out than specified in Header.');

    Dec(LenToWrite, length(ResultData));
    end;

  Response.Sent:=LenToWrite=0;
  Response.ContentOut:=Response.ContentOut + length(ResultData);

  if Response.Sent then
    TriggerLastWrite;

  len:=length(ResultData);
  OutStream.Write(ResultData[0],length(ResultData));

  FDataOut:=len;
  try
    TriggerDataOut;
  finally
    FDataOut:=0;
    TriggerDataSent;
    end;
  end;

procedure TRtcMessageServerProvider.Write(const ResultData: RtcString; SendNow:boolean=True);
  var
    len:cardinal;
  begin
  if length(ResultData)=0 then Exit;

  if not FHeaderOut then
    raise Exception.Create('Trying to send Data without Header. Call WriteHeader before Write.');

  if LenToWrite>=0 then
    begin
    if length(ResultData)>LenToWrite then
      raise Exception.Create('Trying to send more Data out than specified in Header.');

    Dec(LenToWrite, length(ResultData));
    end;

  Response.Sent:=LenToWrite=0;
  Response.ContentOut:=Response.ContentOut + length(ResultData);

  if Response.Sent then
    TriggerLastWrite;

  len:=length(ResultData);
  {$IFDEF RTC_BYTESTRING}
  OutStream.Write(ResultData[1],length(ResultData));
  {$ELSE}
  OutStream.Write(RtcStringToBytes(ResultData)[0],length(ResultData));
  {$ENDIF}

  FDataOut:=len;
  try
    TriggerDataOut;
  finally
    FDataOut:=0;
    TriggerDataSent;
    end;
  end;

procedure TRtcMessageServerProvider.PokeEx(const s:RtcByteArray);
  begin
  if assigned(FRequestBuffer) then
    begin
    FRequestBuffer.Clear;
    if length(s)>0 then
      FRequestBuffer.AddEx(s);
    end;
  end;

function TRtcMessageServerProvider.PeekEx: RtcByteArray;
  begin
  if FRequestBuffer.Size>0 then
    Result:=FRequestBuffer.GetEx
  else
    SetLength(Result,0);
  end;

function TRtcMessageServerProvider.ReadEx: RtcByteArray;
  begin
  if FRequestBuffer.Size>0 then
    begin
    Result:=FRequestBuffer.GetEx;
    FRequestBuffer.Clear;
    end
  else
    SetLength(Result,0);
  end;

function TRtcMessageServerProvider.Read: RtcString;
  begin
  if FRequestBuffer.Size>0 then
    begin
    Result:=FRequestBuffer.Get;
    FRequestBuffer.Clear;
    end
  else
    SetLength(Result,0);
  end;

procedure TRtcMessageServerProvider.TriggerDataSent;
  begin
  if Response.Sending then
    Response.Started:=False;

  inherited TriggerDataSent;
  end;

procedure TRtcMessageServerProvider.TriggerDataOut;
  begin
  if Response.Sending then
    begin
    if LenToSend>=0 then
      begin
      if DataOut>LenToSend then
        begin
        LenToSend:=0;
      {$IFDEF RTC_DEBUG}
        Log('FATAL ERROR in TRtcMessageServerProvider.TriggerDataOut: DataOut > LenToSend','ERROR');
      {$ENDIF}
        end
      else
        Dec(LenToSend, DataOut);
      Response.Done := LenToSend<=0;
      end;

    if Response.Done then
      begin
      Request.Started:=False;
      Request.Active:=False;
      Response.Started:=False;
      Response.Sending:=False;
      FHeaderOut:=False;
      end;
    end;

  inherited TriggerDataOut;
  end;

function TRtcMessageServerProvider.GetParent: TRtcConnectionProvider;
  begin
  Result:=nil;
  end;

procedure TRtcMessageServerProvider.Listen;
  begin
  State:=conListening;
  TriggerListenStart;
  end;

procedure TRtcMessageServerProvider.Disconnect;
  begin
  InternalDisconnect;
  end;

procedure TRtcMessageServerProvider.InternalDisconnect;
  begin
  if State>conPrepared then
    begin
    if State=conActive then
      begin
      State:=conClosing;
      TriggerDisconnecting;
      TriggerDisconnect; // server needs to call Disconnect AND ConenctionLost
      TriggerConnectionLost;
      end
    else if State=conListening then
      begin
      State:=conClosing;
      TriggerListenStop;
      end;
    State:=conPrepared;
    end;
  end;

procedure TRtcMessageServerProvider.CopyFrom(Dup: TRtcConnectionProvider);
  begin
  //
  end;

procedure TRtcMessageServerProvider.Connect;
  begin
  // We need to cheat our component user, since we don't have that information ...

  LocalAddr:='127.0.0.1'; //ServerVariable('SERVER_ADDR',128);
  LocalPort:='80'; // ServerVariable('SERVER_PORT',32);
  PeerAddr:='127.0.0.1'; // ServerVariable('REMOTE_ADDR',128);
  PeerPort:='80'; // ServerVariable('REMOTE_PORT',32);

  State:=conActive;
  TriggerConnectionAccepted;
  TriggerConnecting;
  TriggerConnect;
  end;

procedure TRtcMessageServerProvider.ExecuteRequest(InStream, _OutStream:TStream);
  var
    StatusLine,
    HeadStr:RtcString;

    len,len2:int64;

    HeadLen,
    MyPos:integer;

    s:RtcString;
    data:RtcByteArray;

  procedure ClearRequest;
    begin
    FRequestBuffer.Clear;

    FRequestLine:=False;
    FRequest.Clear;
    FResponse.Clear;
    LenToRead:=0;
    end;

  procedure ProcessData(const data:RtcByteArray);
    begin
    AddBytes(InBuffer,data);

    while length(InBuffer)>0 do
      begin
      if not FHaveRequest then // Don't have the header yet ...
        begin
        if not FRequestLine then
          begin
          MyPos:=PosEx(CRLF,InBuffer);
          if (MyPos>0) then
            begin
            ClearRequest;
            StatusLine:= RtcBytesToString(InBuffer,0,MyPos-1);
            DelBytes(InBuffer,MyPos+length(CRLF)-1);

            MyPos:=PosEx(' HTTP/', Upper_Case(StatusLine));
            if MyPos<=0 then
              MyPos:=PosEx(' HTTPS/', Upper_Case(StatusLine));

            if MyPos<=0 then
              begin
              Request.FileName:=StatusLine;
              raise Exception.Create('HTTP Header error: Request Header missing!');
              end
            else
              begin
              Request.Started:=True;
              Request.Active:=True;

              // Request Method
              MyPos:=PosEx(' ',StatusLine);
              if MyPos<=0 then
                begin
                Request.FileName:=StatusLine;
                raise Exception.Create('HTTP Header error: Request method missing!');
                end;

              Request.Method:=Trim(Copy(StatusLine,1,MyPos-1));
              Delete(StatusLine,1,MyPos);

              // Request FileName
              MyPos:=PosEx(' ',StatusLine);
              if MyPos<=0 then
                begin
                Request.FileName:=StatusLine;
                raise Exception.Create('HTTP Header error: Request file name missing!');
                end;

              Request.FileName:=Copy(StatusLine,1,MyPos-1);
              Delete(StatusLine,1,MyPos);

              // Request HTTP type
              MyPos:=PosEx('/',StatusLine);
              if MyPos<=0 then
                raise Exception.Create('HTTP Header error: HTTP type missing!');

              if Copy(StatusLine,MyPos+1,3)='1.0' then
                Request.Close:=True;

              MyPos:=PosEx('?',Request.FileName);
              if MyPos>0 then
                begin
                Request.Query.Text:=Copy(Request.FileName,MyPos+1,length(Request.FileName)-MyPos);
                Request.FileName:=Copy(Request.FileName,1,MyPos-1);
                end
              else
                Request.Query.Clear;

              FRequestLine:=True;
              end;
            end;
          end;

        if FRequestLine then
          begin
          // See if we can get the whole header ...
          HeadLen:=PosEx(CRLF, InBuffer);
          if HeadLen<>1 then
            HeadLen:=PosEx(END_MARK, InBuffer);

          if HeadLen=1 then
            begin
            DelBytes(InBuffer,2);
            FHaveRequest:=True;

            FixupRequest.Fixup(Request);
            end
          else if HeadLen>0 then
            begin
            // Separate header from the body
            HeadStr:= RtcBytesToString(InBuffer, 0, HeadLen+length(END_MARK)-1);
            DelBytes(InBuffer,HeadLen+length(END_MARK)-1);

            FHaveRequest:=True;

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
                Request[s]:=StatusLine;
                end;

              MyPos:=Pos(CRLF, HeadStr);
              end;

            if Request.ValueCS['CONTENT-LENGTH']<>'' then
              LenToRead:=Request.ContentLength;

            s:=Request.ValueCS['CONNECTION'];
            if s<>'' then
              begin
              s:=Upper_Case(Trim(s));
              if s='CLOSE' then
                Request.Close:=True
              else if s='KEEP-ALIVE' then
                Request.Close:=False;
              end;

            s:=Request.ContentType;
            if s<>'' then
              begin
              if Upper_Case(Copy(s,1,10))='MULTIPART/' then
                begin
                // Get MULTIPART Boundary (Params.Delimiter)
                MyPos:=PosEx('BOUNDARY="',Upper_Case(s));
                if MyPos>0 then
                  begin
                  s:=Copy(s, MyPos+10, length(s)-MyPos-9);
                  MyPos:=PosEx('"',s);
                  if MyPos>0 then
                    Request.Params.Delimiter := Trim(Copy(s, 1, MyPos-1))
                  else
                    Request.Params.Delimiter := Trim(s);
                  end
                else
                  begin
                  MyPos:=PosEx('BOUNDARY=',Upper_Case(s));
                  if MyPos>0 then
                    begin
                    s:=Copy(s, MyPos+9, length(s)-MyPos-8);
                    MyPos:=PosEx(';',s);
                    if MyPos>0 then
                      Request.Params.Delimiter := Trim(Copy(s, 1, MyPos-1))
                    else
                      Request.Params.Delimiter := Trim(s);
                    end;
                  end;
                end;
              end;

            FixupRequest.Fixup(Request);

            StatusLine:='';
            HeadStr:='';
            end;
          end;
        end;

      if FHaveRequest then // Processing a request ...
        begin
        if LenToRead>0 then
          begin
          if LenToRead>length(InBuffer) then // need more than we have
            begin
            Request.ContentIn:=Request.ContentIn + length(InBuffer);

            FRequestBuffer.AddEx(InBuffer);

            Dec(LenToRead, length(InBuffer));

            SetLength(InBuffer,0);
            end
          else
            begin
            Request.ContentIn:=Request.ContentIn + LenToRead;

            FRequestBuffer.AddEx(InBuffer,LenToRead);

            DelBytes(InBuffer,LenToRead);

            LenToRead:=0;
            Request.Complete:=True;
            FHaveRequest:=False; // get ready for next request
            FRequestLine:=False;
            end;
          end
        else
          begin
          Request.Complete:=True;
          FHaveRequest:=False; // get ready for next request
          FRequestLine:=False;
          end;

        TriggerDataReceived;
        Request.Started:=False;

        if Request.Complete and not Response.Done then
          Break; // need to wait for the request to be processed, before we can go to the next one.
        end
      else
        Break; // Failing to fetch a header will break the loop.
      end;
    end;

  begin
  if State<>conActive then Connect;

  OutStream:=_OutStream;
  InStream.Position:=0;

  while InStream.Position<InStream.Size do
    begin
    len:=InStream.Size-InStream.Position;
    if len>32000 then len:=32000;

    SetLength(data,len);
    len2:=InStream.Read(data[0],len);

    FDataIn:=len;
    TriggerDataIn;

    ProcessData(data);
    if len2<len then Break;
    end;

  end;

end.
