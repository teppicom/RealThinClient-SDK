{
  "ISAPI Server Connection Provider"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br>)

  This unit is ONLY for MS Windows.

  @exclude
}
unit rtcISAPISrvProv;

{$INCLUDE rtcDefs.inc}

interface

{$IFDEF WINDOWS}

uses
  Windows,
  SysUtils,

  {$IFNDEF FPC}
  isapi2,
  {$ENDIF}

  rtcTypes,
  rtcSystem,
  rtcLog,

  rtcInfo,
  rtcConn,
  rtcConnProv;

type
  TRtcISAPIServerProvider = class(TRtcNoThrServerProvider)
  private
    ECB: TEXTENSION_CONTROL_BLOCK;

    FRequest:TRtcServerRequest;
    FResponse:TRtcServerResponse;

    FRequestBuffer:TRtcHugeByteArray;

    LenToWrite:int64; // number of bytes to write out using inherited Write()
    LenToSend:int64;

    FHeaderOut:boolean;
    FRequestFixup: TRtcServerRequestFixup;

    amReadyToSend,
    amWaitingToSend:boolean;

  protected
    procedure CleanUp; override;

    procedure CopyFrom(Dup:TRtcConnectionProvider);

  public
    constructor Create; override;

    function ServerVariable(const s:RtcString; len:cardinal):RtcString;

    function GetParent:TRtcConnectionProvider; override;

    procedure Connect(var _ECB: TEXTENSION_CONTROL_BLOCK);
    procedure ExecuteRequest;

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

    property FixupRequest:TRtcServerRequestFixup read FRequestFixup write FRequestFixup;
    end;

{$ENDIF} // {$IFDEF Windows}

implementation

{$IFDEF WINDOWS}

{ TRtcISAPIServerProvider }

const
  CRLF=RtcString(#13#10);

constructor TRtcISAPIServerProvider.Create;
  begin
  inherited;
  FRequestBuffer:=TRtcHugeByteArray.Create;

  LenToWrite:=0;
  LenToSend:=0;
  FHeaderOut:=False;

  FRequest:=nil;
  FResponse:=nil;
  FRequestFixup:=nil;
  end;

procedure TRtcISAPIServerProvider.CleanUp;
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
        Log('TRtcISAPIServerProvider.CleanUp',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcISAPIServerProvider.WriteHeader;
  var
    s1,s2:RtcByteArray;
    len:DWORD;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  s1:= RtcStringToBytesZero( Int2Str(Response.StatusCode)+' '+Response.StatusText);

  if Request.Close then
    s2:= RtcStringToBytesZero(Response.HeaderText + 'Connection: close'+CRLF+CRLF)
  else
    s2:= RtcStringToBytesZero(Response.HeaderText+CRLF);

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

  len:=length(s1)-1;

  ECB.dwHttpStatusCode:=Response.StatusCode;
  ECB.ServerSupportFunction(ECB.ConnID, HSE_REQ_SEND_RESPONSE_HEADER, @s1[0], @len, @s2[0]);

  FDataOut:=0;
  end;

procedure TRtcISAPIServerProvider.WriteHeader(const Header_Text:RtcString);
  var
    s1,s2:RtcByteArray;
    len:DWORD;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  if Header_Text<>'' then
    begin
    Response.HeaderText:=Header_Text;

    s1:= RtcStringToBytesZero( Int2Str(Response.StatusCode)+' '+Response.StatusText );
    if Request.Close then
      s2:= RtcStringToBytesZero( Response.HeaderText+'Connection: close'+CRLF+CRLF)
    else
      s2:= RtcStringToBytesZero( Response.HeaderText+CRLF);
    end
  else
    begin
    SetLength(s1,0);
    SetLEngth(s2,0);
    raise Exception.Create('Streaming content not supported by ISAPI.');
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

  FHeaderOut:=True;

  len:=length(s1)-1;

  ECB.dwHttpStatusCode:=Response.StatusCode;
  ECB.ServerSupportFunction(ECB.ConnID, HSE_REQ_SEND_RESPONSE_HEADER, @s1[0], @len, @s2[0]);

  FDataOut:=0;
  end;

procedure TRtcISAPIServerProvider.WriteEx(const ResultData: RtcByteArray; SendNow:boolean=True);
  var
    len,len2,loc:cardinal;
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
  len2:=len;

  loc:=0;
  repeat
    if not ECB.WriteClient(ECB.ConnID,@ResultData[loc],len,0) then
      raise Exception.Create('Error sending data out (WriteClient returned FALSE).')
    else if len=0 then
      raise Exception.Create('Error sending data out (WriteClient length = 0).');
    Inc(loc,len); len2:=len2-len;
    len:=len2;
    until len2=0;

  FDataOut:=length(ResultData);
  if FDataOut>0 then
    try
      TriggerDataOut;
    finally
      FDataOut:=0;
      TriggerDataSent;
      end;
  end;

procedure TRtcISAPIServerProvider.Write(const ResultData: RtcString; SendNow:boolean=True);
  var
    len,len2,loc:cardinal;
  {$IFNDEF RTC_BYTESTRING}
    ResDat:RtcByteArray;
  {$ENDIF}
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
  len2:=len;

  {$IFDEF RTC_BYTESTRING}
  loc:=1;
  {$ELSE}
  ResDat:=RtcStringToBytes(ResultData);
  loc:=0;
  {$ENDIF}

  repeat
    {$IFDEF RTC_BYTESTRING}
    if not ECB.WriteClient(ECB.ConnID,@ResultData[loc],len,0) then
    {$ELSE}
    if not ECB.WriteClient(ECB.ConnID,@ResDat[loc],len,0) then
    {$ENDIF}
      raise Exception.Create('Error sending data out (WriteClient returned FALSE).')
    else if len=0 then
      raise Exception.Create('Error sending data out (WriteClient length = 0).');
    Inc(loc,len); len2:=len2-len;
    len:=len2;
    until len2=0;

  {$IFNDEF RTC_BYTESTRING}
  SetLength(ResDat,0);
  {$ENDIF}
  FDataOut:=length(ResultData);
  if FDataOut>0 then
    try
      TriggerDataOut;
    finally
      FDataOut:=0;
      TriggerDataSent;
      end;
  end;

function TRtcISAPIServerProvider.ReadEx: RtcByteArray;
  begin
  if FRequestBuffer.Size>0 then
    begin
    Result:=FRequestBuffer.GetEx;
    FRequestBuffer.Clear;
    end
  else
    SetLength(Result,0);
  end;

procedure TRtcISAPIServerProvider.PokeEx(const s:RtcByteArray);
  begin
  if assigned(FRequestBuffer) then
    begin
    FRequestBuffer.Clear;
    if length(s)>0 then
      FRequestBuffer.AddEx(s);
    end;
  end;

function TRtcISAPIServerProvider.PeekEx: RtcByteArray;
  begin
  if FRequestBuffer.Size>0 then
    Result:=FRequestBuffer.GetEx
  else
    SetLength(Result,0);
  end;

function TRtcISAPIServerProvider.Read: RtcString;
  begin
  if FRequestBuffer.Size>0 then
    begin
    Result:=FRequestBuffer.Get;
    FRequestBuffer.Clear;
    end
  else
    SetLength(Result,0);
  end;

procedure TRtcISAPIServerProvider.TriggerDataSent;
  begin
  if Response.Sending then
    Response.Started:=False;

  if amReadyToSend then
    begin
    amReadyToSend:=False;
    try
      inherited TriggerDataSent;
    finally
      amReadyToSend:=True;
      end;
    end
  else
    amWaitingToSend:=True;
  end;

procedure TRtcISAPIServerProvider.TriggerDataOut;
  begin
  if Response.Sending then
    begin
    if LenToSend>=0 then
      begin
      if DataOut>LenToSend then
        begin
        LenToSend:=0;
      {$IFDEF RTC_DEBUG}
        Log('FATAL ERROR in TRtcISAPIServerProvider.TriggerDataOut: DataOut > LenToSend','ERROR');
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

function TRtcISAPIServerProvider.GetParent: TRtcConnectionProvider;
  begin
  Result:=nil;
  end;

procedure TRtcISAPIServerProvider.Listen(Restarting:boolean=False);
  begin
  State:=conListening;
  TriggerListenStart;
  end;

procedure TRtcISAPIServerProvider.Disconnect;
  begin
  InternalDisconnect;
  end;

procedure TRtcISAPIServerProvider.InternalDisconnect;
  begin
  if State>conPrepared then
    begin
    State:=conClosing;
    if State=conActive then
      begin
      TriggerDisconnecting;
      TriggerDisconnect; // server needs to call Disconnect AND ConnectionLost
      TriggerConnectionLost;
      end
    else if State=conListening then
      TriggerListenStop;
    State:=conPrepared;
    end;
  end;

procedure TRtcISAPIServerProvider.CopyFrom(Dup: TRtcConnectionProvider);
  begin
  //
  end;

procedure TRtcISAPIServerProvider.Connect(var _ECB: TEXTENSION_CONTROL_BLOCK);
  begin
  ECB:=_ECB;

  LocalAddr:=ServerVariable('SERVER_ADDR',128);
  LocalPort:=ServerVariable('SERVER_PORT',32);
  PeerAddr:=ServerVariable('REMOTE_ADDR',128);
  PeerPort:=ServerVariable('REMOTE_PORT',32);

  if not (State=conActive) then
    begin
    State:=conActive;
    TriggerConnectionAccepted;
    TriggerConnecting;
    TriggerConnect;
    end;
  end;

procedure TRtcISAPIServerProvider.ExecuteRequest;
  var
    s:RtcString;
    data:RtcByteArray;
    len,len2:cardinal;
    MyPos:integer;
  begin
  amReadyToSend:=False;
  amWaitingToSend:=False;

  Request.Clear;
  Response.Clear;

  s:=ServerVariable('URL',1024);
  if s<>'' then
    begin
    Request.FileName:='/';
    Request.URI:=s;
    end;

  Request.Method:=RtcString(ECB.lpszMethod);
  Request.FileName:=RtcString(ECB.lpszPathInfo);

  Request.Query.Text:=RtcString(ECB.lpszQueryString);
  Request.ContentLength:=ECB.cbTotalBytes;

  s:=ServerVariable('ALL_RAW',8192);
  if s<>'' then Request.HeaderText:=s;

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

  if ECB.cbAvailable>0 then
    begin
    Request.ContentIn:=ECB.cbAvailable;
    SetLength(data,ECB.cbAvailable);
    Move(ECB.lpbData^,data[0],ECB.cbAvailable);
    len:=length(data);
    FrequestBuffer.AddEx(data);
    end;

  FDataIn:=length(Request.URI)+length(Request.HeaderText);
  TriggerDataIn;

  FixupRequest.Fixup(Request);

  Request.Started:=True;
  Request.Active:=True;
  Request.Complete:= Request.ContentLength=Request.ContentIn;

  if Request.FileName<>'' then
    TriggerDataReceived
  else if Request.Complete then
    begin
    Response.StatusCode:=301;
    Response.StatusText:='Moved Permanently';
    Response['Location']:= Request.URI+'/';
    Response.ContentLength:=0;
    WriteHeader;
    end;

  Request.Started:=False;
  while Request.ContentIn<Request.ContentLength do
    begin
    len:=Request.ContentLength-Request.ContentIn;
    if len>32000 then len:=32000;

    len2:=len;
    SetLength(data,len);
    if not ECB.ReadClient(ECB.ConnID,@data[0],len) then
      raise Exception.Create('Error receiving data (ReadClient returned FALSE).')
    else if len<len2 then
      begin
      if len>0 then
        SetLength(data,len)
      else
        raise Exception.Create('Error receiving data (ReadClient returned 0 bytes).');
      end;

    FDataIn:=len;
    TriggerDataIn;

    Request.ContentIn:=Request.ContentIn+len;
    FrequestBuffer.AddEx(data);
    Request.Complete:= Request.ContentIn=Request.ContentLength;

    if Request.FileName<>'' then
      begin
      TriggerDataReceived;
      Sleep(0);
      end
    else if Request.Complete then
      begin
      Response.StatusCode:=301;
      Response.StatusText:='Moved Permanently';
      Response['Location']:= Request.URI+'/';
      Response.ContentLength:=0;
      WriteHeader;
      end;
    end;

  amReadyToSend:=True;
  try
    while amWaitingToSend do
      begin
      amWaitingToSend:=False;
      TriggerDataSent;
      Sleep(0);
      end;
  finally
    amReadyToSend:=False;
    end;

  FHeaderOut:=False;
  end;

function TRtcISAPIServerProvider.ServerVariable(const s: RtcString; len:cardinal): RtcString;
  var
    len2:cardinal;
    res:RtcByteArray;
    xS:RtcByteArray;
  begin
  len2:=len;
  SetLength(Res,len);
  xS:=RtcStringToBytesZero(s);
  if not ECB.GetServerVariable(ECB.ConnID, @(xS[0]),@(Res[0]),len) then
    begin
    if len>len2 then // more data available
      begin
      SetLength(Res,len);
      if not ECB.GetServerVariable(ECB.ConnID, @(xS[0]),@(Res[0]),len) then
        len:=0;
      end
    else
      len:=0;
    end;
  if len=0 then
    Result:=''
  else
    Result:=RtcBytesZeroToString(Res,0,len);
  end;

{$ENDIF} // {$IFDEF Windows}
end.
