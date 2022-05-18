{
  "HTTP Client provider (WinInet)"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br>)

  Using WinInet API to implement a HTTP Client connection provider.

  This unit is ONLY for MS Windows.

  @exclude
}
unit rtcWInetHttpCliProv;

{$INCLUDE rtcDefs.inc}

interface

{$IFDEF WINDOWS}

uses
  Windows,
  SysUtils,

  rtcTypes,
  rtcSystem,
  rtcThrPool,
  rtcLog,
  
  rtcWInetHttp,

  rtcInfo,
  rtcPlugins,
  rtcConnProv,
  rtcConn;

var
  LOG_WINET_ERRORS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

const
  WINET_SECURITY_FLAG_IGNORE_REVOCATION             = $00000080;
  WINET_SECURITY_FLAG_IGNORE_UNKNOWN_CA             = $00000100;
  WINET_SECURITY_FLAG_IGNORE_WRONG_USAGE            = $00000200;
  WINET_SECURITY_FLAG_IGNORE_CERT_CN_INVALID        = $00001000;
  WINET_SECURITY_FLAG_IGNORE_CERT_DATE_INVALID      = $00002000;
  WINET_SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTPS      = $00004000;
  WINET_SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTP       = $00008000;

var
  { Security flags when using the WinInet API over SSL }
  RTC_WINET_SECURITY_FLAGS : Cardinal = WINET_SECURITY_FLAG_IGNORE_UNKNOWN_CA
                                     or WINET_SECURITY_FLAG_IGNORE_CERT_CN_INVALID
                                     or WINET_SECURITY_FLAG_IGNORE_CERT_DATE_INVALID
                                     or WINET_SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTPS
                                     or WINET_SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTP
                                     or WINET_SECURITY_FLAG_IGNORE_WRONG_USAGE
                                     or WINET_SECURITY_FLAG_IGNORE_REVOCATION;
  
type
  RtcWInetException = rtcWInetHttp.RtcWInetException;

  TRtcWInetHttpClientProvider = class;

  TRtcWInetClientThread = class(TRtcThread)
  public
    RtcConn:TRtcWInetHttpClientProvider;
    Releasing:boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    function RunJob:boolean; override;

    procedure OpenConn;
    procedure ReOpenConn;
    procedure CloseConn(_lost:boolean);

    // procedure Connect;
    // procedure Disconnect;
    // procedure Release;
    end;

  TRtcWInetHttpClientProvider = class(TRtcThrClientProvider)
  private
    Client_Thread:TRtcWInetClientThread;

    Forc:boolean;

    FBufferIn:INTERNET_BUFFERS;

    FOnInvalidResponse:TRtcBasicEvent;

    FResponseBuffer:TRtcHugeByteArray;

    FReadBuffer:RtcByteArray;

    FMaxHeaderSize:integer;
    FMaxResponseSize:integer;

    FHeaderOut:boolean;
    FHeaderEx:boolean;
    LenToWrite:int64;

    FMyLastMethod:RtcString;
    FMyLastURI:RtcString;
    FMyLastHTTP10:boolean;
    FMyLastFlags:DWORD;
    FMyLastLength:int64;

    FRequest:TRtcClientRequest;
    FResponse:TRtcClientResponse;

    FDataWasSent:boolean;

    hSession, hConnect, hRequest: hInternet;
	  hStore, pContext: pointer;
    hStoreReady: boolean;

    FUseHttps: boolean;
    FUserName: RtcString;
    FUserPassword: RtcString;
    FCertSubject: RtcString;
    FCertStoreType: TRtcCertStoreType;
    FProxyAddr: RtcString;
    FProxyUsername: RtcString;
    FProxyBypass: RtcString;
    FProxyPassword: RtcString;
    FFixupRequest: TRtcClientRequestFixup;

    FTimeoutsOfAPI: TRtcTimeoutsOfAPI;
  protected
    procedure CleanUp; override;

    function ClientThreadNIL:boolean;
    function GetClientThread:TRtcThread; override;

    procedure TriggerInvalidResponse; virtual;

    procedure AcceptResponse; virtual;

    function _Active:boolean;

    procedure OpenConnection(Reconnecting:boolean);

    function SetupCertificate:boolean;

    function SetupProxy:boolean;

    procedure SendHeaderOutEx(const s:RtcByteArray);
    procedure SendHeaderOut(const s:RtcString);

  public
    constructor Create; override;

    procedure Connect(Force:boolean=False;Reconnecting:boolean=False); override;
    procedure Disconnect; override;
    function ReleaseMe(FromInside:boolean):boolean; override;

    procedure InternalDisconnect; override;

    procedure LeavingEvent; virtual;

    procedure SetTriggerInvalidResponse(Event:TRtcBasicEvent); virtual;

    procedure WriteHeader(SendNow:boolean=True); overload; virtual;
    procedure WriteHeader(const Header_Text:RtcString; SendNow:boolean=True); overload; virtual;

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

    // Use HTTPS protocol instead of HTTP
    property useHttps:boolean read FUseHttps write FUseHttps;

    property ProxyAddr:RtcString read FProxyAddr write FProxyAddr;
    property ProxyBypass:RtcString read FProxyBypass write FProxyBypass;
    property ProxyUsername:RtcString read FProxyUsername write FProxyUsername;
    property ProxyPassword:RtcString read FProxyPassword write FProxyPassword;

    property UserName:RtcString read FUserName write FUserName;
    property UserPassword:RtcString read FUserPassword write FUserPassword;

    property CertStoreType:TRtcCertStoreType read FCertStoreType write FCertStoreType;
    property CertSubject:RtcString read FCertSubject write FCertSubject;

    property FixupRequest:TRtcClientRequestFixup read FFixupRequest write FFixupRequest;

    property TimeoutsOfAPI:TRtcTimeoutsOfAPI read FTimeoutsOfAPI write FTimeoutsOfAPI;
    end;

function HaveWinInet:boolean;

{$ENDIF} // {$IFDEF WINDOWS}

implementation

{$IFDEF WINDOWS}

const
  CRLF = RtcString(#13#10);

var
  Message_WSStop,
  Message_WSRelease,
  Message_WSOpenConn,
  Message_WSReOpenConn,
  Message_WSCloseConn:TRtcBaseMessage;

function HaveWinInet:boolean;
  begin
  Result:=Have_WinInet;
  end;

{ TRtcWInetHttpClientProvider }

constructor TRtcWInetHttpClientProvider.Create;
  begin
  inherited;
  FUseHttps:=False;

  FResponseBuffer:=TRtcHugeByteArray.Create;

  FDataWasSent:=False;
  SetLength(FReadBuffer,SOCK_READ_BUFFER_SIZE);

  FMyLastMethod:='';
  FMyLastURI:='';
  FMyLastFlags:=0;
  FMyLastHTTP10:=false;
  FMyLastLength:=0;

  FFixupRequest:=nil;

  Closing:=False;
  hStore:=nil;
  hStoreReady:=False;
  end;

procedure TRtcWInetHttpClientProvider.CleanUp;
  begin
  try
    try
      Silent:=True;
      InternalDisconnect;
      TriggerConnectionClosing;
      if hStore<>nil then
        begin
        try
          CertCloseStore(hStore,CERT_STORE_CLOSE_FORCE_FLAG);
        except
          end;
        hStore:=nil;
        hStoreReady:=False;
        end;

      if inThread then
        begin
        if not ReleaseMe(True) then
          Log('TRtcWInetHttpClientProvider.CleanUp - ReleaseMe(TRUE)?','ERROR');
        end
      else
        begin
        if not ReleaseMe(False) then
          Log('TRtcWInetHttpClientProvider.CleanUp - ReleaseMe(FALSE)?','ERROR');
        end;
    finally
      try
        inherited;
      finally
        RtcFreeAndNil(FResponseBuffer);
        SetLength(FReadBuffer,0);
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcWInetHttpClientProvider.CleanUp',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcWInetHttpClientProvider.SetTriggerInvalidResponse(Event: TRtcBasicEvent);
  begin
  FOnInvalidResponse:=Event;
  end;

procedure TRtcWInetHttpClientProvider.TriggerInvalidResponse;
  begin
  if assigned(FOnInvalidResponse) then
    FOnInvalidResponse;
  end;

function TRtcWInetHttpClientProvider.ClientThreadNIL: boolean;
  begin
  Result:=Client_Thread=nil;
  end;

function TRtcWInetHttpClientProvider.GetClientThread: TRtcThread;
  begin
  Result:=Client_Thread;
  end;

procedure TRtcWInetHttpClientProvider.Connect(Force: boolean=False; Reconnecting:boolean=False);
  begin
  if Reconnecting then
    begin
    if assigned(Client_Thread) then
      begin
      if inThread then
        OpenConnection(True)
      else
        TRtcThread.PostJob(Client_Thread, Message_WSReOpenConn);
      end
    else if GetMultiThreaded then
      begin
      Client_Thread := TRtcWInetClientThread.Create;
      Client_Thread.RtcConn:= self;
      TRtcThread.PostJob(Client_Thread, Message_WSReOpenConn);
      end
    else
      OpenConnection(True);
    end
  else
    begin
    if assigned(Client_Thread) then
      begin
      if inThread then
        OpenConnection(False)
      else
        TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
      end
    else if GetMultiThreaded then
      begin
      Client_Thread := TRtcWInetClientThread.Create;
      Client_Thread.RtcConn:= self;
      TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
      end
    else
      OpenConnection(False);
    end;
  end;

procedure TRtcWInetHttpClientProvider.OpenConnection(Reconnecting:boolean);
  var
    myPort:integer;
    xAddr,
    xUserName,
    xUserPassword,
    xProxyAddr,
    xProxyBypass:RtcByteArray;
  begin
  xAddr:=nil;
  xUserName:=nil;
  xUserPassword:=nil;
  xProxyAddr:=nil;
  xProxyBypass:=nil;

  if State>=conActivating then Exit; // already connected !!!

  if FUseHttps then
    myPort:=Str2IntDef(GetPort,INTERNET_DEFAULT_HTTPS_PORT)
  else
    myPort:=Str2IntDef(GetPort,INTERNET_DEFAULT_HTTP_PORT);

  LoadWinInet;

  try
    if Reconnecting then
      Sleep(RTC_WAIT_BEFORE_RECONNECT); // avoid flooding the CPU with reconnects

    if CertStoreType<>certNone then
      LoadWinCrypt;

    Lost:=True;
    Closing:=False;
    Silent:=False;

    Request.Init;
    Response.Clear;

    State:=conActivating;

    TriggerConnectionOpening(Forc);

    try
      if FProxyAddr='' then
        hSession := InternetOpen(nil, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0)
      else if FProxyBypass='' then
        begin
        xProxyAddr:=RtcStringToBytesZero(FProxyAddr);
        hSession := InternetOpen(nil, INTERNET_OPEN_TYPE_PROXY, @(xProxyAddr[0]), nil, 0)
        end
      else
        begin
        xProxyAddr:=RtcStringToBytesZero(FProxyAddr);
        xProxyBypass:=RtcStringToBytesZero(FProxyBypass);
        hSession := InternetOpen(nil, INTERNET_OPEN_TYPE_PROXY, @(xProxyAddr[0]), @(xProxyBypass[0]), 0);
        end;
    except
      hSession := nil;
      end;

    if hSession=nil then
      raise RtcWInetException.Create('Error initializing Internet API [Code #'+IntToStr(GetLastError)+'].');

    try
      xAddr:=RtcStringToBytesZero(GetAddr);
      xUserName:=RtcStringToBytesZero(FUserName);
      xUserPassword:=RtcStringToBytesZero(FUserPassword);
      hConnect := InternetConnect(hSession, @(xAddr[0]), myPort,
                                  @(xUserName[0]), @(xUserPassword[0]),
                                  INTERNET_SERVICE_HTTP, 0, 0);
    except
      hConnect := nil;
      end;

    if hConnect=nil then
      raise RtcWInetException.Create('Error opening Internet Connection [Code #'+IntToStr(GetLastError)+'].');

    State:=conActive;
    FDataCrypt:=FUseHttps;
    FDataProtocol:=cppHttp;

    TriggerConnecting;
    TriggerConnect;
  except
    on E:Exception do
      begin
      FDataCrypt:=False;
      FDataProtocol:=cppTcp;

      if hConnect<>nil then
        begin
        InternetCloseHandle(hConnect);
        hConnect:=nil;
        end;
      if hSession<>nil then
        begin
        InternetCloseHandle(hSession);
        hSession:=nil;
        end;

      TriggerConnectionClosing;
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    end;
  end;

procedure TRtcWInetHttpClientProvider.Disconnect;
  var
    hReq:HINTERNET;
  begin
  Lost:=False;
  if assigned(Client_Thread) and not inThread then
    begin
    hReq:=nil;
    if TRtcThread.Lock(Client_Thread) then
      try
        if hRequest<>nil then
          begin
          hReq:=hRequest;
          hRequest:=nil;
          end;
      finally
        TRtcThread.UnLock;
        end;
    if assigned(hReq) then
      try
        InternetCloseHandle(hReq);
      except
        end;
    TRtcThread.PostJob(Client_Thread, Message_WSCloseConn);
    end
  else
    InternalDisconnect;
  end;

procedure TRtcWInetHttpClientProvider.InternalDisconnect;
  var
    hReq:HINTERNET;
  begin
  Closing:=True;

  if State<=conClosing then Exit;

  State:=conClosing;

  if hRequest<>nil then
    begin
    try
      hReq:=hRequest;
      hRequest:=nil;
      InternetCloseHandle(hReq);
    except
      end;
    end;

  FDataCrypt:=False;
  FDataProtocol:=cppTcp;

  if hConnect<>nil then
    begin
    try
      InternetCloseHandle(hConnect);
    except
      end;
    hConnect:=nil;
    end;

  if hSession<>nil then
    begin
    try
      InternetCloseHandle(hSession);
    except
      end;
    hSession:=nil;
    end;

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
    FDataWasSent:=False;

    TriggerReadyToRelease;
    end;
  end;

procedure TRtcWInetHttpClientProvider.PokeEx(const s:RtcByteArray);
  begin
  if _Active then
    if assigned(FResponseBuffer) then
      begin
      FResponseBuffer.Clear;
      if length(s)>0 then
        FResponseBuffer.AddEx(s);
      end;
  end;

function TRtcWInetHttpClientProvider.PeekEx: RtcByteArray;
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

function TRtcWInetHttpClientProvider.ReadEx: RtcByteArray;
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

function TRtcWInetHttpClientProvider.Read: RtcString;
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

procedure TRtcWInetHttpClientProvider.SendHeaderOutEx(const s:RtcByteArray);
  var
    certOK,
    proxyOK:boolean;
    ex:Exception;
    lastErr:DWORD;
    sndHeader:RtcByteArray;
  begin
  SetupProxy;

  if FUseHttps and (FCertStoreType<>certNone) and not hStoreReady then
    SetupCertificate;

  FHeaderOut:=False;
  FHeaderEx:=False;
  certOK:=False;
  proxyOK:=False;

  sndHeader:=RtcStringToBytes(Request.HeaderText);
  repeat
    if hRequest=nil then
      Break
    else if Request.Contentlength=length(s) then // Send content out in 1 API call
      begin
      FHeaderEx:=False;

      if length(s)>0 then // Content in "s"
        begin
        if length(sndHeader)>0 then
          FHeaderOut:=HttpSendRequest(hRequest, Addr(sndHeader[0]), length(sndHeader), Addr(s[0]), length(s))
        else
          FHeaderOut:=HttpSendRequest(hRequest, nil, 0, Addr(s[0]), length(s));
        end
      else // No content
        begin
        if length(sndHeader)>0 then
          FHeaderOut:=HttpSendRequest(hRequest, Addr(sndHeader[0]), length(sndHeader), nil, 0)
        else
          FHeaderOut:=HttpSendRequest(hRequest, nil, 0, nil, 0);
        end;
      end
    else
      begin
      FBufferIn.dwStructSize := SizeOf(FBufferIn);
      FBufferIn.dwBufferTotal := Request.ContentLength;
      FBufferIn.dwBufferLength := length(s);
      FBufferIn.dwHeadersTotal := length(sndHeader);
      FBufferIn.dwHeadersLength := length(sndHeader);
      FBufferIn.dwOffsetHigh := 0;
      FBufferIn.dwOffsetLow := 0;
      if length(sndHeader)>0 then
        FBufferIn.lpcszHeader := Addr(sndHeader[0])
      else
        FBufferIn.lpcszHeader := nil;
      if length(s)>0 then
        FBufferIn.lpvBuffer := Addr(s[0])
      else
        FBufferIn.lpvBuffer := nil;
      FBufferIn.Next := nil;

      FHeaderOut := HttpSendRequestEx(hRequest, @FBufferIn, nil, HSR_INITIATE, 0);
      FHeaderEx := FHeaderOut;
      end;

    if hRequest=nil then
      begin
      FHeaderOut:=False;
      Break;
      end
    else if not FHeaderOut then
      begin
      lastErr:=GetLastError;
      if (lastErr = ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED) or
         (lastErr = ERROR_INTERNET_INVALID_CA) then
        begin
        if certOK or (FCertStoreType=certNone) then
          Break
        else
          begin
          certOK:=True;
          if not SetupCertificate then Break;
          end;
        end
      else
        begin
        if proxyOK then
          Break
        else
          begin
          proxyOK:=True;
          if not SetupProxy then Break;
          end;
        end;
      end;
    until FHeaderOut;

  if not FHeaderOut then
    begin
    if _Active then
      begin
      ex:=RtcWInetException.Create('Error Sending the Request [Code #'+IntToStr(GetLastError)+'].');
      try
        TriggerException(ex);
      finally
        RtcFreeAndNil(ex);
        end;
      InternalDisconnect;
      end;
    end
  else
    begin
    FDataOut:=length(Request.Method)+length(Request.URI)+10;
    LenToWrite:=Request.ContentLength-length(s);
    FDataOut:=FDataOut+length(sndHeader)+length(s);
    Request.ContentOut:=length(s);
    try
      TriggerDataOut;
    finally
      FDataOut:=0;
      end;

    FDataWasSent:=True; // will call DataSent
    end;
  end;

procedure TRtcWInetHttpClientProvider.SendHeaderOut(const s:RtcString);
  var
    certOK,
    proxyOK:boolean;
    ex:Exception;
    lastErr:DWORD;
    sndHeader:RtcByteArray;
  begin
  SetupProxy;

  if FUseHttps and (FCertStoreType<>certNone) and not hStoreReady then
    SetupCertificate;

  FHeaderOut:=False;
  FHeaderEx:=False;
  certOK:=False;
  proxyOK:=False;

  sndHeader:=RtcStringToBytes(Request.HeaderText);
  repeat
    if hRequest=nil then
      Break
    else if Request.Contentlength=length(s) then // Send content out in 1 API call
      begin
      FHeaderEx:=False;

      if length(s)>0 then
        begin
        if length(sndHeader)>0 then
          FHeaderOut:=HttpSendRequest(hRequest, Addr(sndHeader[0]), length(sndHeader), Addr(RtcStringToBytes(s)[0]), length(s))
        else
          FHeaderOut:=HttpSendRequest(hRequest, nil, 0, Addr(RtcStringToBytes(s)[0]), length(s));
        end
      else
        begin
        if length(sndHeader)>0 then
          FHeaderOut:=HttpSendRequest(hRequest, Addr(sndHeader[0]), length(sndHeader), nil, 0)
        else
          FHeaderOut:=HttpSendRequest(hRequest, nil, 0, nil, 0);
        end;
      end
    else
      begin
      FBufferIn.dwStructSize := SizeOf(FBufferIn);
      FBufferIn.dwBufferTotal := Request.ContentLength;
      FBufferIn.dwBufferLength := length(s);
      FBufferIn.dwHeadersTotal := length(sndHeader);
      FBufferIn.dwHeadersLength := length(sndHeader);
      FBufferIn.dwOffsetHigh := 0;
      FBufferIn.dwOffsetLow := 0;
      if length(sndHeader)>0 then
        FBufferIn.lpcszHeader := Addr(sndHeader[0])
      else
        FBufferIn.lpcszHeader := nil;
      if length(s)>0 then
        FBufferIn.lpvBuffer := Addr(RtcStringToBytes(s)[0])
      else
        FBufferIn.lpvBuffer := nil;
      FBufferIn.Next := nil;

      FHeaderOut := HttpSendRequestEx(hRequest, @FBufferIn, nil, HSR_INITIATE, 0);
      FHeaderEx := FHeaderOut;
      end;

    if hRequest=nil then
      begin
      FHeaderOut:=False;
      Break;
      end
    else if not FHeaderOut then
      begin
      lastErr:=GetLastError;
      if (lastErr = ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED) or
         (lastErr = ERROR_INTERNET_INVALID_CA) then
        begin
        if certOK or (FCertStoreType=certNone) then
          Break
        else
          begin
          certOK:=True;
          if not SetupCertificate then Break;
          end;
        end
      else
        begin
        if proxyOK then
          Break
        else
          begin
          proxyOK:=True;
          if not SetupProxy then Break;
          end;
        end;
      end;
    until FHeaderOut;

  if not FHeaderOut then
    begin
    if _Active then
      begin
      ex:=RtcWInetException.Create('Error Sending the Request [Code #'+IntToStr(GetLastError)+'].');
      try
        TriggerException(ex);
      finally
        RtcFreeAndNil(ex);
        end;
      InternalDisconnect;
      end;
    end
  else
    begin
    FDataOut:=length(Request.Method)+length(Request.URI)+10;
    LenToWrite:=Request.ContentLength-length(s);
    FDataOut:=FDataOut+length(sndHeader)+length(s);
    Request.ContentOut:=length(s);
    try
      TriggerDataOut;
    finally
      FDataOut:=0;
      end;

    FDataWasSent:=True; // will call DataSent
    end;
  end;

procedure TRtcWInetHttpClientProvider.WriteHeader(SendNow:boolean=True);
  var
    ex:Exception;
    hReq:HINTERNET;
    myFlags:DWORD;
    xLastMethod,
    xLastURI:RtcByteArray;
{$IFDEF RTC_USESETTIMEOUTS}
    myTimeout:DWORD;
{$ENDIF RTC_USESETTIMEOUTS}
  begin
  xLastMethod:=nil;
  xLastURI:=nil;
  if not _Active then Exit;

  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  myFlags:=INTERNET_FLAG_NO_AUTO_REDIRECT or INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE;
  if FUseHttps then myFlags:=myFlags or INTERNET_FLAG_SECURE;

  FixupRequest.Fixup(Request);

  if (hRequest=nil) or
     (Request.Method<>FMyLastMethod) or
     (Request.URI<>FMyLastURI) or
     (MyFlags<>FMyLastFlags) or
     (Request.Close<>FMyLastHTTP10) or
     (Request.ContentLength<>FMyLastLength) then
    begin
    if hRequest<>nil then
      begin
      try
        hReq:=hRequest;
        hRequest:=nil;
        InternetCloseHandle(hReq);
      except
        end;
      end;

    FMyLastMethod:=Request.Method;
    FMyLastURI:=Request.URI;
    FMyLastFlags:=MyFlags;
    FMyLastHTTP10:=Request.Close;
    FMyLastLength:=Request.ContentLength;

    xLastMethod:=RtcStringToBytesZero(FMyLastMethod);
    xLastURI:=RtcStringToBytesZero(FMyLastURI);

    if FMyLastHTTP10 then
      hRequest := HttpOpenRequest(hConnect, @(xLastMethod[0]), @(xLastURI[0]), 'HTTP/1.0', '', nil,  FMyLastFlags, 0)
    else
      hRequest := HttpOpenRequest(hConnect, @(xLastMethod[0]), @(xLastURI[0]), 'HTTP/1.1', '', nil,  FMyLastFlags, 0);

    if hRequest=nil then
      begin
      if _Active then
        begin
        ex:=RtcWInetException.Create('Error opening HTTP Request [Code #'+IntToStr(GetLastError)+'].');
        try
          TriggerException(ex);
        finally
          RtcFreeAndNil(ex);
          end;
        InternalDisconnect;
        end;
      Exit;
      end;

{$IFDEF RTC_USESETTIMEOUTS}
    if TimeoutsOfAPI.ConnectTimeout > 0 then
      begin
      myTimeout := TimeoutsOfAPI.ConnectTimeout * 1000;
      InternetSetOption(hRequest, INTERNET_OPTION_CONNECT_TIMEOUT, Addr(myTimeout), sizeof(myTimeout));
      end;
    if TimeoutsOfAPI.SendTimeout > 0 then
      begin
      myTimeout := TimeoutsOfAPI.SendTimeout * 1000;
      InternetSetOption(hRequest, INTERNET_OPTION_SEND_TIMEOUT, Addr(myTimeout), sizeof(myTimeout));
      end;
    if TimeoutsOfAPI.ReceiveTimeout > 0 then
      begin
      myTimeout := TimeoutsOfAPI.ReceiveTimeout * 1000;
      InternetSetOption(hRequest, INTERNET_OPTION_RECEIVE_TIMEOUT, Addr(myTimeout), sizeof(myTimeout));
      end;
{$ENDIF RTC_USESETTIMEOUTS}
    end;

  if SendNow or (Request.ContentLength=0) then
    SendHeaderOutEx(RtcEmptyByteArray);

  if hRequest=nil then
    begin
    if _Active then
      InternalDisconnect;
    Exit;
    end;

  if not FHeaderOut then
    begin
    LenToWrite:=Request.ContentLength;
    FDataWasSent:=True;
    end;

  Request.Started:=True;
  Request.Active:=True;
  end;

procedure TRtcWInetHttpClientProvider.WriteHeader(const Header_Text: RtcString; SendNow:boolean=True);
  begin
  if not _Active then Exit;

  Request.HeaderText:=Header_Text;
  WriteHeader(SendNow);
  end;

procedure TRtcWInetHttpClientProvider.WriteEx(const s: RtcByteArray; SendNow:boolean=True);
  var
    bOK:boolean;
    ex:Exception;
    bWritten:DWORD;
  begin
  if not _Active then Exit;

  if not Request.Active then
    raise Exception.Create('Sending data without header.');

  if not FHeaderOut then
    begin
    SendHeaderOutEx(s);
    Exit;
    end;

  if length(s)=0 then Exit;

  if FHeaderEx then
    begin
    bOK := InternetWriteFile(hRequest, Addr(s[0]), length(s), bWritten);
    if not bOK or (bWritten<>dword(length(s))) then
      if _Active then
        begin
        ex:=RtcWInetException.Create('Error Sending the Request [Code #'+IntToStr(GetLastError)+'].');
        try
          TriggerException(ex);
        finally
          RtcFreeAndNil(ex);
          end;
        InternalDisconnect;
        Exit;
        end;

    FDataOut:=length(s);
    LenToWrite:=LenToWrite-FDataOut;
    Request.ContentOut:=Request.ContentOut + FDataOut;
    try
      TriggerDataOut;
    finally
      FDataOut:=0;
      end;

    FDataWasSent:=True; // will call DataSent
    end;
  end;

procedure TRtcWInetHttpClientProvider.Write(const s: RtcString; SendNow:boolean=True);
  var
    bOK:boolean;
    ex:Exception;
    bWritten:DWORD;
  begin
  if not _Active then Exit;

  if not Request.Active then
    raise Exception.Create('Sending data without header.');

  if not FHeaderOut then
    begin
    SendHeaderOut(s);
    Exit;
    end;

  if length(s)=0 then Exit;

  if FHeaderEx then
    begin
    {$IFDEF RTC_BYTESTRING}
    bOK := InternetWriteFile(hRequest, Addr(s[1]), length(s), bWritten);
    {$ELSE}
    bOK := InternetWriteFile(hRequest, Addr(RtcStringToBytes(s)[0]), length(s), bWritten);
    {$ENDIF}
    if not bOK or (bWritten<>dword(length(s))) then
      if _Active then
        begin
        ex:=RtcWInetException.Create('Error Sending the Request [Code #'+IntToStr(GetLastError)+'].');
        try
          TriggerException(ex);
        finally
          RtcFreeAndNil(ex);
          end;
        InternalDisconnect;
        Exit;
        end;

    FDataOut:=length(s);
    LenToWrite:=LenToWrite-FDataOut;
    Request.ContentOut:=Request.ContentOut + FDataOut;
    try
      TriggerDataOut;
    finally
      FDataOut:=0;
      end;

    FDataWasSent:=True; // will call DataSent
    end;
  end;

procedure TRtcWInetHttpClientProvider.LeavingEvent;
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

procedure TRtcWInetHttpClientProvider.AcceptResponse;
  var
    dwBufLen,dwIndex:DWord;
    LenToRead:int64;

    AllReceived:boolean;

    hReq:HINTERNET;

    recvHeader,
    InBuffer:RtcByteArray;

    BytesRead:DWord;

    ex:Exception;

  function ReadNextBlock:boolean;
    var
      ReadNowBytes:int64;
    begin
    BytesRead:=0;

    if LenToRead>0 then
      begin
      ReadNowBytes:=LenToRead;
      if ReadNowBytes>length(FReadBuffer) then
        ReadNowBytes:=length(FReadBuffer);
      end
    else
      ReadNowBytes:=length(FReadBuffer);

    if Response.ExpectedBytes>0 then
      begin
      if ReadNowBytes>Response.ExpectedBytes then
        ReadNowBytes:=Response.ExpectedBytes;
      Response.ExpectedBytes:=Response.ExpectedBytes-ReadNowBytes;
      end;

    if hRequest=nil then
      Result:=False
    else
      Result:=InternetReadFile(hRequest, Addr(FReadBuffer[0]), ReadNowBytes, BytesRead);

    if Result then
      if BytesRead>0 then
        begin
        FDataIn:=BytesRead;
        TriggerDataIn;
        end;
    end;

  begin
  AllReceived:=False;
  try
    if not _Active then Exit;

    if not FHeaderOut then // This should not happen!
      raise Exception.Create('AcceptResponse was called before WriteHeader.');

    if FHeaderEx then
      HttpEndRequest(hRequest, nil, 0, 0);

    FHeaderOut:=False;
    Response.Started:=True;
    Response.Receiving:=True;

    FResponseBuffer.Clear;

    // Get Raw Header ...
    SetLength(recvHeader,1);
    dwBufLen:=1;
    dwIndex:=0;

    if hRequest=nil then
      begin
      InternalDisconnect;
      Exit;
      end;

    if not HttpQueryInfo(hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, Addr(recvHeader[0]), dwBufLen, dwIndex) then
      begin
      if not _Active then Exit;

      if GetLastError<>ERROR_INSUFFICIENT_BUFFER then
        begin
        if _Active then
          begin
          ex:=RtcWInetException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
          try
            TriggerException(ex);
          finally
            RtcFreeAndNil(ex);
            end;
          InternalDisconnect;
          end;
        Exit;
        end
      else if hRequest<>nil then
        begin
        SetLength(recvHeader, dwBufLen);
        if not HttpQueryInfo(hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, Addr(recvHeader[0]), dwBufLen, dwIndex) then
          begin
          if _Active then
            begin
            ex:=RtcWInetException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
            try
              TriggerException(ex);
            finally
              RtcFreeAndNil(ex);
              end;
            InternalDisconnect;
            end;
          Exit;
          end;
        end
      else
        begin
        InternalDisconnect;
        Exit;
        end;
      end
    else
      SetLength(recvHeader,dwBufLen);

    FDataIn:=length(recvHeader);
    TriggerDataIn;

    Response.HeaderText:= RtcBytesToString(recvHeader);

    if (Request.Method='HEAD') or
       (Response.StatusCode=204) or
       (Response.StatusCode=304) or
       ( (Response.StatusCode>=100) and (Response.StatusCode<=199) ) then
      begin
      LenToRead:=0;
      Response.Done:=True;
      if _Active then
        TriggerDataReceived;
      Exit;
      end
    else if Response.ValueCS['CONTENT-LENGTH']<>'' then
      begin
      LenToRead:=Response.ContentLength;
      if LenToRead=0 then
        begin
        Response.Done:=True;
        if _Active then
          TriggerDataReceived;
        Exit;
        end;
      end
    else
      LenToRead:=-1;

    SetLength(InBuffer,0);

    while _Active and not Response.Done do
      begin
      if not ReadNextBlock then
        begin
        if _Active then
          begin
          ex:=RtcWInetException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
          try
            TriggerException(ex);
          finally
            RtcFreeAndNil(ex);
            end;
          InternalDisconnect;
          end;
        Exit;
        end
      else if BytesRead>0 then
        AddBytes(InBuffer,FReadBuffer,0,BytesRead)
      else if (LenToRead>0) and (BytesRead=0) then
        begin
        if _Active then
          begin
          ex:=RtcWInetException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
          try
            TriggerException(ex);
          finally
            RtcFreeAndNil(ex);
            end;
          InternalDisconnect;
          end;
        Exit;
        end;

      if (LenToRead>0) or (LenToRead=-1) then
        begin
        if (LenToRead>length(InBuffer)) or // need more than we have
           (LenToRead=-1) then // size unknown
          begin
          Response.ContentIn:=Response.ContentIn + length(InBuffer);

          if LenToRead>0 then
            Dec(LenToRead, length(InBuffer))
          else if BytesRead=0 then // last byte read
            begin
            LenToRead:=0;
            Response.Done:=True;
            Request.Active:=False;

            AllReceived:=True;
            FHeaderOut:=False;
            end;

          FResponseBuffer.AddEx(InBuffer);

          SetLength(InBuffer,0);
          end
        else
          begin
          Response.ContentIn:=Response.ContentIn + LenToRead;

          FResponseBuffer.AddEx(InBuffer,LenToRead);

          DelBytes(InBuffer,LenToRead);

          LenToRead:=0;
          Response.Done:=True;
          Request.Active:=False;

          AllReceived:=True;
          FHeaderOut:=False;
          end;
        end
      else
        begin
        Response.Done:=True;
        Request.Active:=False;

        AllReceived:=True;
        FHeaderOut:=False;
        end;

      if not _Active then Exit;

      if Response.Done then
        begin
        TriggerDataReceived;
        Exit;
        end
      else
        begin
        TriggerDataReceived;
        Response.Started:=False;
        end;
      end;
  finally
    FResponseBuffer.Clear;
    if not (_Active and AllReceived) then // error, need to release request object
      begin
      if hRequest<>nil then
        begin
        try
          hReq:=hRequest;
          hRequest:=nil;
          InternetCloseHandle(hReq);
        except
          end;
        end;
      end;
    end;
  end;

function TRtcWInetHttpClientProvider._Active: boolean;
  begin
  Result:=not Closing and (FState>=conActivating);
  end;

function TRtcWInetHttpClientProvider.ReleaseMe(FromInside:boolean):boolean;
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

function TRtcWInetHttpClientProvider.SetupProxy:boolean;
  var
    xProxyUsername,
    xProxyPassword:RtcByteArray;
  begin
  Result:=False;
  xProxyUsername:=nil;
  xProxyPassword:=nil;

  if FProxyUsername='' then Exit;

  if FProxyUsername<>'' then
    begin
    xProxyUsername:=RtcStringToBytesZero(FProxyUsername);
    if not InternetSetOption(hRequest, INTERNET_OPTION_PROXY_USERNAME,
                              @(xProxyUsername[0]), length(FProxyUsername)) then Exit;
    end;

  if FProxyPassword<>'' then
    begin
    xProxyPassword:=RtcStringToBytesZero(FProxyPassword);
    if not InternetSetOption(hRequest, INTERNET_OPTION_PROXY_PASSWORD,
                              @(xProxyPassword[0]), length(FProxyPassword)) then Exit;
    end;
  {if FUsername<>'' then
    if not InternetSetOption(hRequest, INTERNET_OPTION_USERNAME,
                              RtcPtrAnsiChar(FUsername), length(FUsername)) then Exit;

  if FUserPassword<>'' then
    if not InternetSetOption(hRequest, INTERNET_OPTION_PASSWORD,
                              RtcPtrAnsiChar(FUserPassword), length(FUserPassword)) then Exit;}

  Result:=True;
  end;

function TRtcWInetHttpClientProvider.SetupCertificate:boolean;
  var
    lpszStoreName,
    lpszSubjectName:RtcByteArray;
    dwFlags, dwBuffLen:DWORD;
    pDWFlags:^DWORD;
    res:bool;
  begin
  Result:=False;

  if hStore<>nil then
    begin
    try
      CertCloseStore(hStore, CERT_STORE_CLOSE_FORCE_FLAG);
    except
      end;
    hStore:=nil;
    hStoreReady:=False;
    end;

  if FCertStoreType=certAny then
    begin
    dwBuffLen:=sizeof(dwFlags);
    pdwFlags:=addr(dwFlags);
    InternetQueryOption (hRequest, INTERNET_OPTION_SECURITY_FLAGS,
            pdwFlags, dwBuffLen);

    pdwFlags^ := pdwFlags^ or RTC_WINET_SECURITY_FLAGS;

    res := InternetSetOption (hRequest, INTERNET_OPTION_SECURITY_FLAGS,
                              pdwFlags, dwBuffLen );

    if res then
      begin
      // hStoreReady:=True;
      Result:=True;
      end;
    end
  else
    begin
    SetLength(lpszStoreName,0);
    SetLength(lpszSubjectName,0);
    case FCertStoreType of
      certMY: lpszStoreName := RtcStringToBytesZero('MY');
      certCA: lpszStoreName := RtcStringToBytesZero('CA');
      certROOT: lpszStoreName := RtcStringToBytesZero('ROOT');
      certSPC: lpszStoreName := RtcStringToBytesZero('SPC');
      else Exit;
      end;

    hStore := CertOpenSystemStore(nil, @(lpszStoreName[0]) );
    if hStore<>nil then
      begin
      lpszSubjectName:=RtcStringToBytesZero(FCertSubject);

      pContext := CertFindCertificateInStore(hStore,
          X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
          0, CERT_FIND_SUBJECT_STR_A, @(lpszSubjectName[0]) , nil);

      if (pContext<>nil) then
        begin
        if hRequest<>nil then
          begin
          res := InternetSetOption(hRequest,
                                   INTERNET_OPTION_CLIENT_CERT_CONTEXT,
                                   pContext, sizeof(CERT_CONTEXT));
          if res then
            begin
            hStoreReady:=True;
            Result:=True;
            end;
          end;
        end;
      end;
    end;
  end;

{ TRtcWInetClientThread }

constructor TRtcWInetClientThread.Create;
  begin
  inherited;
  Releasing:=False;
  RtcConn:=nil;
  NeedThread:=True;
  end;

procedure TRtcWInetClientThread.OpenConn;
  begin
  if assigned(RtcConn) and not Releasing then
    RtcConn.OpenConnection(False);
  end;

procedure TRtcWInetClientThread.ReOpenConn;
  begin
  if assigned(RtcConn) and not Releasing then
    RtcConn.OpenConnection(True);
  end;

procedure TRtcWInetClientThread.CloseConn(_lost:boolean);
  begin
  if assigned(RtcConn) and not Releasing then
    begin
    try
      RtcConn.Lost:=_lost;
      RtcConn.InternalDisconnect;
    except
      on E:Exception do
        if LOG_WINET_ERRORS then
          Log('WInetClientThread.CloseConn : RtConn.InternalDisconnect',E,'ERROR');
        // ignore exceptions
      end;
    end;
  end;

destructor TRtcWInetClientThread.Destroy;
  begin
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
    inherited;
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('TRtcWInetClientThread.Destroy',E,'ERROR');
    end;
  end;

function TRtcWInetClientThread.RunJob:boolean;
  begin
  try
    if Job=Message_WSOpenConn then
      begin
      OpenConn;
      Result:=False;
      end
    else if Job=Message_WSReOpenConn then
      begin
      ReOpenConn;
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
      Result:=True;
      end
    else if Job=Message_WSRelease then
      begin
      Releasing:=True;
      Result:=True;
      end
    else
      Result:=inherited RunJob;
  except
    on E:Exception do
      begin
      if LOG_WINET_ERRORS then
        Log('TRtcWInetClientThread.RunJob',E,'ERROR');
      CloseConn(true);
      Result:=True; // raise;
      end;
    end;
  end;

type
  TMyWinInet=class
    public
    constructor Create;
    destructor Destroy; override;
    end;

var
  MyWinInet:TMyWinInet;

{ TMyWinInet }

constructor TMyWinInet.Create;
  begin
  inherited;
  Message_WSOpenConn:=TRtcBaseMessage.Create;
  Message_WSReOpenConn:=TRtcBaseMessage.Create;
  Message_WSCloseConn:=TRtcBaseMessage.Create;
  Message_WSStop:=TRtcBaseMessage.Create;
  Message_WSRelease:=TRtcBaseMessage.Create;
  end;

destructor TMyWinInet.Destroy;
  begin
  try
    RtcFreeAndNil(Message_WSOpenConn);
    RtcFreeAndNil(Message_WSReOpenConn);
    RtcFreeAndNil(Message_WSCloseConn);
    RtcFreeAndNil(Message_WSStop);
    RtcFreeAndNil(Message_WSRelease);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TMyWinInet.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcWInetHttpCliProv Initializing ...','DEBUG');{$ENDIF}
MyWinInet:=TMyWinInet.Create;
{$IFDEF RTC_DEBUG} Log('rtcWInetHttpCliProv Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcWInetHttpCliProv Finalizing ...','DEBUG');{$ENDIF}
CloseThreadPool;
RtcFreeAndNil(MyWinInet);
{$IFDEF RTC_DEBUG} Log('rtcWInetHttpCliProv Finalized.','DEBUG');{$ENDIF}

{$ENDIF} // {$IFDEF WINDOWS}
end.
