{
  "HTTP Socket Client Provider"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br>)

  Using a TCP/IP Socket Client Provider to implement a HTTP Client provider

  @exclude
}
unit rtcSocketHttpCliProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcSystem,
  rtcLog,

  rtcInfo,
  rtcCrypt,
  rtcConn,
  rtcConnProv,

  rtcPlugins,
  rtcSocketCliProv;

type
  TRtcSocketHttpClientProvider = class(TRtcSocketClientProvider)
  private
    FOnInvalidResponse:TRtcBasicEvent;

    FMaxHeaderSize:integer;
    FMaxResponseSize:integer;

    FRequest:TRtcClientRequest;
    FResponse:TRtcClientResponse;

    FResponseBuffer:TRtcHugeByteArray;

    FResponseWaiting:boolean;

    ReqComplete:boolean; // internal Request.Complete indicator (to avoid problems with changing Request objects)

    FChunked:boolean;
    FChunkState:byte;

    FWantManualRead:boolean; // expecting manual read

    FResponseLine:boolean; // response line received
    InBuffer:RtcByteArray; // data received, including HTTP header (header will be stripped when read)
    FHaveResponse:boolean; // response header accepted, receiving request data.
    LenToRead:int64; // number of bytes left to read from last Request

    LenToWrite:int64; // number of bytes to write out using inherited Write()
    LenToSend:int64; // number of bytes left to send out (DataOut event)
    FHeaderOut:boolean;
    
    FFixupRequest: TRtcClientRequestFixup;

  protected
    procedure CleanUp; override;

    procedure ClearResponse;

    procedure TriggerConnect; override;
    procedure TriggerConnectLost; override;
    procedure TriggerDataReceived; override;
    procedure TriggerDataSent; override;
    procedure TriggerDataOut; override;

    procedure TriggerInvalidResponse; virtual;

    function InternalRead(FromDataReceived:boolean):boolean; virtual;

    function GetCryptProtocol:TRtcCryptPluginProtocol; override;
    procedure SetDataProtocol(res:TRtcCryptPluginState); override;

  public
    constructor Create; override;

    procedure SetTriggerInvalidResponse(Event:TRtcBasicEvent);

    procedure WriteHeader(SendNow:boolean=True); overload; virtual;
    procedure WriteHeader(const Header_Text:RtcString; SendNow:boolean=True); overload; virtual;

    procedure WriteEx(const ResultData:RtcByteArray; SendNow:boolean=True); override;
    function ReadEx:RtcByteArray; override;

    function PeekEx:RtcByteArray; override;
    procedure PokeEx(const s:RtcByteArray); override;

    procedure Write(const ResultData:RtcString; SendNow:boolean=True); override;
    function Read:RtcString; override;

    property Request:TRtcClientRequest read FRequest write FRequest;
    property Response:TRtcClientResponse read FResponse write FResponse;

    // Max. allowed size of the first (status) line in response header
    property MaxResponseSize:integer read FMaxResponseSize write FMaxResponseSize;
    // Max. allowed size of the complete response Header
    property MaxHeaderSize:integer read FMaxHeaderSize write FMaxHeaderSize;

    property FixupRequest:TRtcClientRequestFixup read FFixupRequest write FFixupRequest;

  protected // WEB SOCKETS

    procedure PrepareWSUpgradeRequestHeader;
    end;

  TRtcSocketHttp2ClientProvider = class(TRtcSocketHttpClientProvider)
  protected
    function GetCryptProtocol:TRtcCryptPluginProtocol; override;
    procedure SetDataProtocol(res:TRtcCryptPluginState); override;
    end;

implementation

const
  CRLF = RtcString(#13#10);
  END_MARK = RtcString(#13#10#13#10);
  S_WEBSOCKET=RtcString('WEBSOCKET');

{ TRtcSocketHttpClientProvider }

constructor TRtcSocketHttpClientProvider.Create;
  begin
  inherited;
  FResponseBuffer:=TRtcHugeByteArray.Create;

  SetLength(InBuffer,0);
  LenToWrite:=0;
  LenToSend:=0;
  FHeaderOut:=False;
  FResponseLine:=False;
  ReqComplete:=False;

  FFixupRequest:=nil;

  FHaveResponse:=False;
  end;

procedure TRtcSocketHttpClientProvider.CleanUp;
  begin
  try
    try
      LenToWrite:=0;
      LenToSend:=0;
      FResponseLine:=False;
      FHeaderOut:=False;
      FResponseWaiting:=False;

      FHaveResponse:=False;
    finally
      try
        inherited;
      finally
        SetLength(InBuffer,0);
        RtcFreeAndNil(FResponseBuffer);
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSocketHttpClientProvider.CleanUp',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcSocketHttpClientProvider.SetTriggerInvalidResponse(Event: TRtcBasicEvent);
  begin
  FOnInvalidResponse:=Event;
  end;

procedure TRtcSocketHttpClientProvider.TriggerInvalidResponse;
  begin
  if assigned(FOnInvalidResponse) then
    FOnInvalidResponse;
  end;

procedure TRtcSocketHttpClientProvider.ClearResponse;
  begin
  FResponseBuffer.Clear;

  FResponseLine:=False;

  FHaveResponse:=False;

  if IsWebSocket then
    begin
    FChunked:=False;
    FChunkState:=0;
    LenToRead:=0;
    end
  else
    begin
    FResponse.Clear;
    LenToRead:=-1;
    end;
  end;

procedure TRtcSocketHttpClientProvider.TriggerConnect;
  begin
  Request.Init;

  FResponseBuffer.Clear;

  SetLength(InBuffer,0);
  LenToWrite:=0;
  LenToSend:=0;
  FHeaderOut:=False;
  FResponseLine:=False;
  FResponseWaiting:=False;
  FHaveResponse:=False;
  FChunked:=False;
  FChunkState:=0;
  ReqComplete:=False;

  FIsWebSocketUpgraded:=False;

  ClearResponse;

  inherited;
  end;

procedure TRtcSocketHttpClientProvider.TriggerConnectLost;
  begin
  if ReqComplete then
    begin
    Response.ManualRead:=False;
    if FHaveResponse and not FChunked and (LenToRead=-1) then // No content-length and not chunked
      begin
      Response.Done:=True;
      Request.Active:=False;
      FHaveResponse:=False; // get ready for next request
      FResponseLine:=False;
      FHeaderOut:=False;
      FChunked:=False;
      FChunkState:=0;

      ReqComplete:=False; // DataReceived events have to wait until a new request has been sent out
      inherited TriggerDataReceived;
      end
    else
      TriggerDataReceived;
    end;
  inherited;

  FIsWebSocketUpgraded:=False;
  end;

procedure TRtcSocketHttpClientProvider.TriggerDataReceived;

  procedure ResponseError;
    begin
    ReqComplete:=False; // no more reading, please!
    FResponseLine:=False;

    FHaveResponse:=False;

    TriggerInvalidResponse;
    end;

  procedure PrepareHTTPResponse;
    var
      InBuf,
      s,
      StatusLine,
      HeadStr:RtcString;
      HeadLen,
      MyPos:integer;
    begin
    repeat
      if not FHaveResponse then // Don't have the header yet ...
        begin
        if not FResponseLine then
          begin
          if length(InBuffer)>5 then
            InBuf:=Upper_Case(RtcBytesToString(InBuffer,0,5))
          else
            InBuf:=Upper_Case(RtcBytesToString(InBuffer));
          // Accept streaming data as response
          if ((length(InBuf)=5) and (InBuf<>'HTTP/')) or
             ((length(InBuf)=4) and (InBuf<>'HTTP')) or
             ((length(InBuf)=3) and (InBuf<>'HTT')) or
             ((length(InBuf)=2) and (InBuf<>'HT')) or
             ((length(InBuf)=1) and (InBuf<>'H')) then
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
            if MyPos<=0 then // no Status Text
              begin
              s:=StatusLine;
              StatusLine:='';
              end
            else
              begin
              s:=Copy(StatusLine,1,MyPos-1); // StatusCode
              Delete(StatusLine,1,MyPos); // StatusText
              end;

            if s<>'' then
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

            if Response.StatusCode=100 then
              begin // special handling of the "100:Continuing" Http status code
              FResponseLine:=False;
              Continue;
              end;

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

            if (Request.Method='HEAD') or
               (Response.StatusCode=204) or
               (Response.StatusCode=304) or
               ( (Response.StatusCode>=100) and (Response.StatusCode<=199) ) then
              begin
              LenToRead:=0;
              Response.Done:=True;
              end
            else if Response.ValueCS['CONTENT-LENGTH']<>'' then
              LenToRead:=Response.ContentLength
            else
              LenToRead:=-1;

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
                Request.Close:=False
              else if Request.WSUpgrade then
                if PosEx('UPGRADE',s)>0 then
                  if Same_Text(Response.ValueCS['UPGRADE'],S_WEBSOCKET) then
                    if Request.ValueCS['SEC-WEBSOCKET-VERSION']<>'13' then
                      Response.WSUpgrade:=True
                    else if Response.StatusCode=101 then
                      Response.WSUpgrade:=
                        Response['Sec-WebSocket-Accept'] =
                          Mime_Encode ( SHA1_Digest (
                            Request['Sec-WebSocket-Key'] +
                                    '258EAFA5-E914-47DA-95CA-C5AB0DC85B11' ) ); // WebSocket "accept" string
              end;

            if (LenToRead=-1) and not FChunked then
              Request.Close:=True;

            StatusLine:='';
            HeadStr:='';
            end;
          end;
        end;

      if FHaveResponse then
        begin
        // Processing the Response
        if not InternalRead(True) then
          Break;
        end
      else
        begin
        // We do not have a complete header yet, break the loop
        NeedMoreData;
        Exit;
        end;
      until (length(InBuffer)=0) or not ReqComplete;

    if Request.Complete and not Response.Done then
      NeedMoreData;
    end;

  procedure PrepareWebSockResponse;
	  begin
    while length(InBuffer)>0 do
      begin
      // Prepare Response headers
      if not FHaveResponse then // Don't have the header yet ...
        begin
        LenToRead:=TRtcWSFrame.Check(InBuffer);
        if LenToRead<>0 then // user has to catch and handle data corruption issues
          begin
          FHaveResponse:=True;
          FResponseBuffer.Clear;
          end;
        end;
      if FHaveResponse then // Processing a response ...
        InternalRead(True)
      else
        begin
        NeedMoreData;
        Exit; // Failing to fetch a header will break the loop.
        end;
      end;
    NeedMoreData;
    end;

  begin
  if not ReqComplete then
    begin
    if assigned(CryptPlugin) then
      begin
      AddBytes(InBuffer,inherited ReadEx);

      if length(InBuffer)=0 then
        begin
        FResponseWaiting:=True;
        Exit;
        end
      else
        FResponseWaiting:=False;
      end
    else
      begin
      FResponseWaiting:=True;
      Exit;
      end;
    end
  else
    FResponseWaiting:=False;

  if IsWebSocket then
    begin
    if FWebSockManualRead then // receiving response manually
      begin
      FWantManualRead:=True;
      inherited TriggerDataReceived;
      end
    else
      begin
      FWantManualRead:=False;
      AddBytes(InBuffer,inherited ReadEx);
      PrepareWebSockResponse;
      end;
    end
  else if FHaveResponse and Response.ManualRead then
    begin
    FWantManualRead:=True;
    // We have response headers, using manual read ...
    inherited TriggerDataReceived;
    end
  else
    begin
    FWantManualRead:=False;
    // Read String from buffer
    AddBytes(InBuffer,inherited ReadEx);

    PrepareHTTPResponse;

    if IsWebSocket then
      begin
      if FWebSockManualRead then // receiving response manually
        begin
        FWantManualRead:=True;
        inherited TriggerDataReceived;
        end
      else
        begin
        FWantManualRead:=False;
        AddBytes(InBuffer,inherited ReadEx);
        PrepareWebSockResponse;
        end;
      end;
    end;
  end;

function TRtcSocketHttpClientProvider.InternalRead(FromDataReceived: boolean): boolean;
  var
    StatusLine:RtcString;
    MyPos:integer;

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

  begin
  Result:=False;
  if IsWebSocket then
    begin
    if LenToRead=0 then
      begin
      Result:=True;
      FHaveResponse:=False; // get ready for next response
      end
    else if LenToRead>0 then
      begin
      Result:=True;
      if LenToRead>length(InBuffer) then // need more than we have
        begin
        FResponseBuffer.AddEx(InBuffer);
        Dec(LenToRead, length(InBuffer));
        SetLength(InBuffer,0);
        end
      else
        begin
        FResponseBuffer.AddEx(InBuffer,LenToRead);
        DelBytes(InBuffer,LenToRead);
        LenToRead:=0;
        FHaveResponse:=False; // get ready for next response
        end;
      end
    else
      begin
      Result:=True;
      FResponseBuffer.AddEx(InBuffer);
      SetLength(InBuffer,0);
      end;
    if FromDataReceived then
      inherited TriggerDataReceived;
    end
  else if FChunked then // Read data as chunks
    begin
    if (FChunkState=0) and (length(InBuffer)>0) then // 1.step = read chunk size
      begin
      MyPos:=PosEx(CRLF,InBuffer);
      if MyPos>0 then
        begin
        Result:=True;
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
        Result:=True;
        Response.ContentIn:=Response.ContentIn+length(InBuffer);

        if LenToRead>0 then
          Dec(LenToRead, length(InBuffer));

        FResponseBuffer.AddEx(InBuffer);
        SetLength(InBuffer,0);

        if FromDataReceived then
          begin
          inherited TriggerDataReceived;
          Response.Started:=False;
          end;
        end
      else
        begin
        if LenToRead>0 then
          begin
          Result:=True;
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
      Result:=True;
      LenToRead:=-1;
      DelBytes(InBuffer,2); // Delete CRLF

      if FChunkState=2 then
        FChunkState:=0
      else
        begin
        Response.Done:=True;
        Request.Active:=False;
        FHaveResponse:=False; // get ready for next request
        FChunked:=False;
        FChunkState:=0;
        FResponseLine:=False;
        FHeaderOut:=False;

        ReqComplete:=False; // DataReceived events have to wait until a new request has been sent out
        end;

      if FromDataReceived then
        begin
        inherited TriggerDataReceived;
        Response.Started:=False;
        end;
      end;
    end
  else // Read data as stream or with predefined length
    begin
    if (LenToRead>0) or (LenToRead=-1) then
      begin
      if (LenToRead>length(InBuffer)) or
         (LenToRead=-1) then // need more than we have
        begin
        if length(InBuffer)>0 then
          begin
          Result:=True;
          Response.ContentIn:=Response.ContentIn+length(InBuffer);

          if LenToRead>0 then
            Dec(LenToRead, length(InBuffer));

          FResponseBuffer.AddEx(InBuffer);

          SetLength(InBuffer,0);
          end;
        end
      else
        begin
        Response.ContentIn:=Response.ContentIn+LenToRead;

        FResponseBuffer.AddEx(InBuffer,LenToRead);

        DelBytes(InBuffer,LenToRead);

        LenToRead:=0;
        Response.Done:=True;

        FHaveResponse:=False; // get ready for next request
        FChunked:=False;
        FResponseLine:=False;
        FHeaderOut:=False;

        if FromDataReceived and
           Request.WSUpgrade and
           Response.WSUpgrade then
          begin
          Result:=False;
          FIsWebSocketUpgraded:=True;
          FWebSockManualRead:=Response.ManualRead;
          end
        else
          begin
          Result:=True;
          Request.Active:=False;
          ReqComplete:=False; // DataReceived events have to wait until a new request has been sent out
          end;
        end;
      end
    else
      begin
      Response.Done:=True;

      FHaveResponse:=False; // get ready for next request
      FChunked:=False;
      FResponseLine:=False;
      FHeaderOut:=False;

      if FromDataReceived and
         Request.WSUpgrade and
         Response.WSUpgrade then
        begin
        Result:=False;
        FIsWebSocketUpgraded:=True;
        FWebSockManualRead:=Response.ManualRead;
        end
      else
        begin
        Result:=True;
        Request.Active:=False;
        ReqComplete:=False; // DataReceived events have to wait until a new request has been sent out
        end;
      end;

    if FromDataReceived then
      begin
      inherited TriggerDataReceived;
      Response.Started:=False;
      end;
    end;
  end;

procedure TRtcSocketHttpClientProvider.PrepareWSUpgradeRequestHeader;
  var
    nonce:RtcByteArray;
    i:integer;
  begin
  if Request.Method=''                           then Request.Method:='GET';
  if Request.Host=''                             then Request.Host:=GetAddr+':'+GetPort;
  if Request.ValueCS['UPGRADE']=''               then Request['Upgrade'] := 'websocket';
  if Request.ValueCS['CONNECTION']=''            then Request['Connection'] := 'Upgrade';

  if (Request.ValueCS['SEC-WEBSOCKET-VERSION']='') and
     (Request.ValueCS['SEC-WEBSOCKET-KEY']='') then
    begin
    Request['Sec-WebSocket-Version']:='13';
    SetLength(nonce,16);
    for i:=0 to length(nonce)-1 do
      nonce[i]:=Random(256) and $FF;
    Request['Sec-WebSocket-Key'] := RtcBytesToString(Mime_EncodeEx(nonce,True));
    end;

  Request.Close:=False;
  end;

procedure TRtcSocketHttpClientProvider.WriteHeader(SendNow:boolean=True);
  var
    s:RtcString;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  FixupRequest.Fixup(Request);

  if not Request.WSUpgrade then
    if Same_Text(Request.ValueCS['UPGRADE'],S_WEBSOCKET) then
      if PosEx('UPGRADE',Upper_Case(Request.ValueCS['CONNECTION']))>0 then
        Request.WSUpgrade:=True;

  if Request.WSUpgrade then
    PrepareWSUpgradeRequestHeader;

  if Request.Close then
    begin
    Request['Connection']:='close';
    s:=   Request.Method+' '+Request.URI+' HTTP/1.0'+CRLF+
          Request.HeaderText+CRLF;
    end
  else
    s:=   Request.Method+' '+Request.URI+' HTTP/1.1'+CRLF+
          Request.HeaderText+CRLF;

  Request.Started:=True;
  Request.Active:=True;

  LenToWrite:=Request.ContentLength;
  LenToSend:=length(s) + Request.ContentLength;

  FHeaderOut:=True;
  inherited Write(s, SendNow or (LenToWrite<=0));
  end;

procedure TRtcSocketHttpClientProvider.WriteHeader(const Header_Text:RtcString; SendNow:boolean=True);
  var
    s:RtcString;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  if Header_Text<>'' then
    Request.HeaderText:=Header_Text;

  FixupRequest.Fixup(Request);

  if not Request.WSUpgrade then
    if Same_Text(Request.ValueCS['UPGRADE'],S_WEBSOCKET) then
      if PosEx('UPGRADE',Upper_Case(Request.ValueCS['CONNECTION']))>0 then
        Request.WSUpgrade:=True;

  if Request.WSUpgrade then
    PrepareWSUpgradeRequestHeader;

  if Request.Close then
    begin
    Request['Connection']:='close';
    s:= Request.Method+' '+Request.URI+' HTTP/1.0'+CRLF+
          Request.HeaderText+CRLF;
    end
  else
    s:= Request.Method+' '+Request.URI+' HTTP/1.1'+CRLF+
          Request.HeaderText+CRLF;

  Request.Started:=True;
  Request.Active:=True;

  LenToWrite:=Request.ContentLength;
  LenToSend:=length(s) + Request.ContentLength;

  FHeaderOut:=True;
  inherited Write(s, SendNow or (LenToWrite<=0));
  end;

procedure TRtcSocketHttpClientProvider.WriteEx(const ResultData: RtcByteArray; SendNow:boolean=True);
  begin
  if length(ResultData)=0 then Exit;

  if IsWebSocket then
    inherited WriteEx(ResultData,SendNow)
  else
    begin
    if not FHeaderOut then
      raise Exception.Create('Trying to send Data without Header. Call WriteHeader before Write.');

    if length(ResultData)>LenToWrite then
      raise Exception.Create('Trying to send more Data out than specified in Header.');

    Dec(LenToWrite, length(ResultData));

    Request.ContentOut:=Request.ContentOut + length(ResultData);

    inherited WriteEx(ResultData, SendNow);
    end;
  end;

procedure TRtcSocketHttpClientProvider.Write(const ResultData: RtcString; SendNow:boolean=True);
  begin
  if length(ResultData)=0 then Exit;

  if IsWebSocket then
    inherited Write(ResultData,SendNow)
  else
    begin
    if not FHeaderOut then
      raise Exception.Create('Trying to send Data without Header. Call WriteHeader before Write.');

    if length(ResultData)>LenToWrite then
      raise Exception.Create('Trying to send more Data out than specified in Header.');

    Dec(LenToWrite, length(ResultData));

    Request.ContentOut:=Request.ContentOut + length(ResultData);

    inherited Write(ResultData, SendNow);
    end;
  end;

procedure TRtcSocketHttpClientProvider.PokeEx(const s:RtcByteArray);
  begin
  if assigned(FResponseBuffer) then
    begin
    FResponseBuffer.Clear;
    if length(s)>0 then
      FResponseBuffer.AddEx(s);
    end;
  end;

function TRtcSocketHttpClientProvider.PeekEx: RtcByteArray;
  begin
  if not assigned(FResponseBuffer) then
   SetLength(Result,0)
  else if FResponseBuffer.Size>0 then
    Result:=FResponseBuffer.GetEx
  else
    SetLength(Result,0);
  end;

function TRtcSocketHttpClientProvider.ReadEx: RtcByteArray;
  begin
  if FWantManualRead then
    begin
    FWantManualRead:=False;
    if IsWebSocket then
      begin
      if length(InBuffer)>0 then
        begin
        AddBytes(InBuffer,inherited ReadEx);
        Result:=InBuffer;
        SetLength(InBuffer,0);
        end
      else
        Result:=inherited ReadEx;
      NeedMoreData;
      end
    else
      begin
      AddBytes(InBuffer,inherited ReadEx);
      InternalRead(False);
      if FResponseBuffer.Size>0 then
        begin
        Result:=FResponseBuffer.GetEx;
        FResponseBuffer.Clear;
        end
      else
        SetLength(Result,0);
      if not Response.Done and Request.Complete then
        NeedMoreData;
      end;
    end
  else
    begin
    if FResponseBuffer.Size>0 then
      begin
      Result:=FResponseBuffer.GetEx;
      FResponseBuffer.Clear;
      end
    else
      SetLength(Result,0);
    end;
  end;

function TRtcSocketHttpClientProvider.Read: RtcString;
  begin
  if FWantManualRead then
    begin
    FWantManualRead:=False;
    if IsWebSocket then
      begin
      if length(InBuffer)>0 then
        begin
        Result:=RtcBytesToString(InBuffer);
        SetLength(InBuffer,0);
        Result:=Result+inherited Read;
        end
      else
        Result:=inherited Read;
      NeedMoreData;
      end
    else
      begin
      AddBytes(InBuffer,inherited ReadEx);
      InternalRead(False);
      if FResponseBuffer.Size>0 then
        begin
        Result:=FResponseBuffer.Get;
        FResponseBuffer.Clear;
        end
      else
        SetLength(Result,0);
      if not Response.Done and Request.Complete then
        NeedMoreData;
      end;
    end
  else
    begin
    if FResponseBuffer.Size>0 then
      begin
      Result:=FResponseBuffer.Get;
      FResponseBuffer.Clear;
      end
    else
      SetLength(Result,0);
    end;
  end;

procedure TRtcSocketHttpClientProvider.TriggerDataSent;
  begin
  if IsWebSocket then
    inherited TriggerDataSent
  else
    begin
    if Request.Active then
      Request.Started:=False;

    inherited TriggerDataSent;

    if FResponseWaiting then
      if ReqComplete then
        TriggerDataReceived;
    end;
  end;

procedure TRtcSocketHttpClientProvider.TriggerDataOut;
  var
    nmd:boolean;
  begin
  if IsWebSocket then
    inherited TriggerDataOut
  else
    begin
    nmd:=False;
    if not ReqComplete and assigned(Request) and Request.Active then
      begin
      if LenToSend>=0 then
        begin
        if DataOut>LenToSend then
          begin
          LenToSend:=0;
        {$IFDEF RTC_DEBUG}
          Log('FATAL ERROR in TRtcSocketHttpClientProvider.TriggerDataOut: DataOut > LenToSend','ERROR');
        {$ENDIF}
          end
        else
          Dec(LenToSend, DataOut);
        end;
      ReqComplete := LenToSend<=0;
      Request.Complete := ReqComplete;
      nmd:=Request.Complete and not Response.Done;
      end;

    inherited TriggerDataOut;

    if nmd then NeedMoreData;
    end;
  end;

function TRtcSocketHttpClientProvider.GetCryptProtocol: TRtcCryptPluginProtocol;
  begin
  Result:=cppHttp;
  end;

procedure TRtcSocketHttpClientProvider.SetDataProtocol(res: TRtcCryptPluginState);
  begin
  FDataProtocol:=cppHttp;
  end;

{ TRtcSocketHttp2ClientProvider }

function TRtcSocketHttp2ClientProvider.GetCryptProtocol: TRtcCryptPluginProtocol;
  begin
  Result:=cppHttp2;
  end;

procedure TRtcSocketHttp2ClientProvider.SetDataProtocol(res: TRtcCryptPluginState);
  begin
  if res=cpsReady2 then
    FDataProtocol:=cppHttp2
  else
    FDataProtocol:=cppHttp;
  end;

end.
