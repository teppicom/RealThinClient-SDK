{
  "HTTP Client provider (WinHTTP)"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br>)

  Using WinHTTP API to implement a HTTP Client connection provider.

  This unit is ONLY for MS Windows.

  @exclude
}
unit rtcWinHttpCliProv;

{$INCLUDE rtcDefs.inc}

interface

{$IFDEF WINDOWS}

{.$DEFINE RTC_USESTATUSCALLBACK}

uses
  SysUtils,
  Windows,

  rtcTypes,
  rtcSystem,
  rtcThrPool,
  rtcLog,
  
  rtcWinHttp,

  rtcInfo,
  rtcPlugins,
  rtcConnProv,
  rtcConn;

var
  LOG_WINHTTP_ERRORS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

const
  WINHTTP_SECURITY_FLAG_IGNORE_REVOCATION         = $00000080;
  WINHTTP_SECURITY_FLAG_IGNORE_UNKNOWN_CA         = $00000100;
  WINHTTP_SECURITY_FLAG_IGNORE_CERT_DATE_INVALID  = $00002000;
  WINHTTP_SECURITY_FLAG_IGNORE_CERT_CN_INVALID    = $00001000;
  WINHTTP_SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE   = $00000200;

  WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM     = 0;
  WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW        = 1;
  WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH       = 2;
  WINHTTP_AUTOLOGON_SECURITY_LEVEL_DEFAULT    = WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM;

var
  { Security flags when using the WinHTTP API over SSL }
  RTC_WINHTTP_SECURITY_FLAGS: Cardinal = WINHTTP_SECURITY_FLAG_IGNORE_UNKNOWN_CA
									  or WINHTTP_SECURITY_FLAG_IGNORE_CERT_CN_INVALID
									  or WINHTTP_SECURITY_FLAG_IGNORE_CERT_DATE_INVALID
									  or WINHTTP_SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE;

  { Autologon Security level when using the WinHTTP API: 0 = MEDIUM (DEFAULT), 1 = LOW, 2 = HIGH }
  RTC_WINHTTP_AUTOLOGON_SECURITY_LEVEL: Cardinal = WINHTTP_AUTOLOGON_SECURITY_LEVEL_DEFAULT;

type
  RtcWinHttpException = rtcWinHttp.RtcWinHttpException;

  TRtcWinHttpClientProvider = class;

  TRtcWinHttpClientThread = class(TRtcThread)
  public
    RtcConn:TRtcWinHttpClientProvider;
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

  TRtcWinHttpClientProvider = class(TRtcThrClientProvider)
  private
    Client_Thread:TRtcWinHttpClientThread;

    Forc:boolean;

    FOnInvalidResponse:TRtcBasicEvent;

    FResponseBuffer:TRtcHugeByteArray;

    FReadBuffer:RtcByteArray;

    FMaxHeaderSize:integer;
    FMaxResponseSize:integer;

    FHeaderOut:boolean;
    FHeaderEx:boolean;
    LenToWrite:int64;

    FRequest:TRtcClientRequest;
    FResponse:TRtcClientResponse;

    FDataWasSent:boolean;

    FMyLastMethod:RtcString;
    FMyLastURI:RtcString;
    FMyLastHTTP10:boolean;
    FMyLastFlags:DWORD;
    FMyLastLength:int64;

    hSession, hConnect, hRequest: hInternet;
    hStore, pContext: pointer;
    hStoreReady: boolean;

    FUseHttps: boolean;
    FCertSubject: RtcString;
    FCertStoreType: TRtcCertStoreType;
    FProxyUsername: RtcString;
    FUserPassword: RtcString;
    FUsername: RtcString;
    FProxyBypass: RtcString;
    FProxyPassword: RtcString;
    FProxyAddr: RtcString;
    FFixupRequest: TRtcClientRequestFixup;

    sndHeader: RtcWideString;

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

    procedure SetTriggerInvalidResponse(Event:TRtcBasicEvent);

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

    property Username:RtcString read FUsername write FUsername;
    property UserPassword:RtcString read FUserPassword write FUserPassword;

    property CertStoreType:TRtcCertStoreType read FCertStoreType write FCertStoreType;
    property CertSubject:RtcString read FCertSubject write FCertSubject;

    property FixupRequest:TRtcClientRequestFixup read FFixupRequest write FFixupRequest;

    property TimeoutsOfAPI:TRtcTimeoutsOfAPI read FTimeoutsOfAPI write FTimeoutsOfAPI;
    end;

function HaveWinHTTP:boolean;

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

function HaveWinHTTP:boolean;
  begin
  Result:=Have_WinHttp;
  end;

{ TRtcWInetHttpClientProvider }

constructor TRtcWinHttpClientProvider.Create;
  begin
  inherited;
  FUseHttps:=False;

  FResponseBuffer:=TRtcHugeByteArray.Create;

  FDataWasSent:=False;
  SetLength(FReadBuffer,SOCK_READ_BUFFER_SIZE);

  hStore:=nil;
  hStoreReady:=False;

  fUsername:='';
  fUserPassword:='';
  fProxyUsername:='';
  fProxyPassword:='';

  FMyLastMethod:='';
  FMyLastURI:='';
  FMyLastFlags:=0;
  FMyLastHTTP10:=false;
  FMyLastLength:=0;

  FFixupRequest:=nil;

  Closing:=False;
  end;

procedure TRtcWinHttpClientProvider.CleanUp;
  begin
  try
    try
      Silent:=True;
      InternalDisconnect;
      TriggerConnectionClosing;
      sndHeader:='';
      SetLength(FReadBuffer,0);
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
          Log('TRtcWinHttpClientProvider.CleanUp - ReleaseMe(TRUE)?','ERROR');
        end
      else
        begin
        if not ReleaseMe(False) then
          Log('TRtcWinHttpClientProvider.CleanUp - ReleaseMe(FALSE)?','ERROR');
        end;
    finally
      try
        inherited;
      finally
        RtcFreeAndNil(FResponseBuffer);
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcWinHttpClientProvider.CleanUp',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcWinHttpClientProvider.SetTriggerInvalidResponse(Event: TRtcBasicEvent);
  begin
  FOnInvalidResponse:=Event;
  end;

procedure TRtcWinHttpClientProvider.TriggerInvalidResponse;
  begin
  if assigned(FOnInvalidResponse) then
    FOnInvalidResponse;
  end;

function TRtcWinHttpClientProvider.ClientThreadNIL: boolean;
  begin
  Result:=Client_Thread=nil;
  end;

function TRtcWinHttpClientProvider.GetClientThread: TRtcThread;
  begin
  Result:=Client_Thread;
  end;

procedure TRtcWinHttpClientProvider.Connect(Force: boolean=False; Reconnecting:boolean=False);
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
      Client_Thread := TRtcWinHttpClientThread.Create;
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
      Client_Thread := TRtcWinHttpClientThread.Create;
      Client_Thread.RtcConn:= self;
      TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
      end
    else
      OpenConnection(False);
    end;
  end;

{$IFDEF RTC_USESTATUSCALLBACK}
procedure HttpStatusCallback(hInternet:HINTERNET;
                             dwContext:PDWORD;
                             dwInternetStatus:DWORD;
                             lpvStatusInformation:Pointer;
                             dwStatusInformationLength:DWORD);stdcall;
    var
      s:String;
    begin
    case dwInternetStatus of
      WINHTTP_CALLBACK_STATUS_RESOLVING_NAME:
          s:='WINHTTP_CALLBACK_STATUS_RESOLVING_NAME';
      WINHTTP_CALLBACK_STATUS_NAME_RESOLVED:
          s:='WINHTTP_CALLBACK_STATUS_NAME_RESOLVED';
      WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER:
          s:='WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER';
      WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER:
          s:='WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER';
      WINHTTP_CALLBACK_STATUS_SENDING_REQUEST:
          s:='WINHTTP_CALLBACK_STATUS_SENDING_REQUEST';
      WINHTTP_CALLBACK_STATUS_REQUEST_SENT:
          s:='WINHTTP_CALLBACK_STATUS_REQUEST_SENT';
      WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE:
          s:='WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE';
      WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED:
          s:='WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED';
      WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION:
          s:='WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION';
      WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED:
          s:='WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED';
      WINHTTP_CALLBACK_STATUS_HANDLE_CREATED:
          s:='WINHTTP_CALLBACK_STATUS_HANDLE_CREATED';
      WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING:
          s:='WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING';
      WINHTTP_CALLBACK_STATUS_DETECTING_PROXY: begin
          s:='WINHTTP_CALLBACK_STATUS_DETECTING_PROXY';
      WINHTTP_CALLBACK_STATUS_REDIRECT: begin
          s:='WINHTTP_CALLBACK_STATUS_REDIRECT';
      WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE:
          s:='WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE';
      WINHTTP_CALLBACK_STATUS_SECURE_FAILURE:
          s:='WINHTTP_CALLBACK_STATUS_SECURE_FAILURE';
      WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE:
          s:='WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE';
      WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE:
          s:='WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE';
      WINHTTP_CALLBACK_STATUS_READ_COMPLETE:
          s:='WINHTTP_CALLBACK_STATUS_READ_COMPLETE';
      WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE:
          s:='WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE';
      WINHTTP_CALLBACK_STATUS_REQUEST_ERROR:
          s:='WINHTTP_CALLBACK_STATUS_REQUEST_ERROR';
      WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE:
          s:='WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE';
      else
          s:='UNKNOWN WINHTTP CALLBACK #'+IntToStr(dwInternetStatus);
    end;
  end;
{$ENDIF RTC_USESTATUSCALLBACK}

procedure TRtcWinHttpClientProvider.OpenConnection(Reconnecting:boolean);
  var
    myPort:integer;
    myWAddr,
    myWProxyAddr,
    myWProxyBypass:RtcWideString;
    myOption: DWORD;
    myResult: BOOL;
  begin
  if State>=conActivating then Exit; // already connected !!!

  if FUseHttps then
    myPort:=Str2IntDef(GetPort,INTERNET_DEFAULT_HTTPS_PORT)
  else
    myPort:=Str2IntDef(GetPort,INTERNET_DEFAULT_HTTP_PORT);

  LoadWinHttp;

  try
    if Reconnecting then
      Sleep(RTC_WAIT_BEFORE_RECONNECT); // short pause to avoid flooding the CPU with reconnects

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
        hSession := InternetOpen(nil, WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, nil, nil, 0)
      else if FProxyBypass='' then
        begin
        myWProxyAddr:=RtcWideString(FProxyAddr);
        hSession := InternetOpen(nil, WINHTTP_ACCESS_TYPE_NAMED_PROXY, RtcPtrWideChar(myWProxyAddr), nil, 0);
        end
      else
        begin
        myWProxyAddr:=RtcWideString(FProxyAddr);
        myWProxyBypass:=RtcWideString(FProxyBypass);
        hSession := InternetOpen(nil, WINHTTP_ACCESS_TYPE_NAMED_PROXY, RtcPtrWideChar(myWProxyAddr), RtcPtrWideChar(myWProxyBypass), 0);
        end;
    except
      hSession := nil;
      end;

    if hSession=nil then
      raise RtcWinHttpException.Create('Error initializing Internet API [Code #'+IntToStr(GetLastError)+'].');

    try
      myWAddr:=RtcWideString(GetAddr);
      hConnect := InternetConnect(hSession, RtcPtrWideChar(myWAddr), myPort, 0);
    except
      hConnect := nil;
      end;

    if hConnect=nil then
      raise RtcWinHttpException.Create('Error opening Internet Connection [Code #'+IntToStr(GetLastError)+'].');

    if RTC_WINHTTP_AUTOLOGON_SECURITY_LEVEL<>WINHTTP_AUTOLOGON_SECURITY_LEVEL_DEFAULT then
      begin
      try
        myOption := RTC_WINHTTP_AUTOLOGON_SECURITY_LEVEL;
        myResult := InternetSetOption(hSession, WINHTTP_OPTION_AUTOLOGON_POLICY, Addr(myOption), SizeOf(myOption));
      except
        myResult := False;
        end;
      if not myResult then
        raise RtcWinHttpException.Create('Error setting Autologon Security Level [Code #'+IntToStr(GetLastError)+'].');
      end;

    State:=conActive;

{$IFDEF RTC_USESTATUSCALLBACK}
    SetStatusCallback(hConnect, @HttpStatusCallback, WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, NIL);
{$ENDIF RTC_USESTATUSCALLBACK}

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
{$IFDEF RTC_USESTATUSCALLBACK}
        SetStatusCallback(hConnect, nil, 0, nil);
{$ENDIF RTC_USESTATUSCALLBACK}
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

procedure TRtcWinHttpClientProvider.Disconnect;
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
      {$IFDEF RTC_USESTATUSCALLBACK}
        SetStatusCallback(hReq, nil, 0, nil);
      {$ENDIF RTC_USESTATUSCALLBACK}
        InternetCloseHandle(hReq);
      except
        end;
    TRtcThread.PostJob(Client_Thread, Message_WSCloseConn);
    end
  else
    InternalDisconnect;
  end;

procedure TRtcWinHttpClientProvider.InternalDisconnect;
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
{$IFDEF RTC_USESTATUSCALLBACK}
      SetStatusCallback(hReq, nil, 0, nil);
{$ENDIF RTC_USESTATUSCALLBACK}
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

procedure TRtcWinHttpClientProvider.PokeEx(const s:RtcByteArray);
  begin
  if _Active then
    if assigned(FResponseBuffer) then
      begin
      FResponseBuffer.Clear;
      if length(s)>0 then
        FResponseBuffer.AddEx(s);
      end;
  end;

function TRtcWinHttpClientProvider.PeekEx: RtcByteArray;
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

function TRtcWinHttpClientProvider.ReadEx: RtcByteArray;
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

function TRtcWinHttpClientProvider.Read: RtcString;
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

procedure TRtcWinHttpClientProvider.SendHeaderOutEx(const s:RtcByteArray);
  var
    ex:Exception;
    certOK,proxyOK:boolean;
    lastErr:DWORD;
  begin
  SetupProxy;

  if FUseHttps and (FCertStoreType<>certNone) and not hStoreReady then
    SetupCertificate;

  FHeaderOut:=False;
  FHeaderEx:=False;
  certOK:=False;
  proxyOK:=False;

  sndHeader:=RtcWideString(Request.HeaderText);
  repeat
    if hRequest=nil then
      Break
    else
      begin
      if length(s)>0 then // Content in "s"
        begin
        if sndHeader<>'' then
          FHeaderOut:=HttpSendRequest(hRequest, RtcPtrWideChar(sndHeader), length(sndHeader), Addr(s[0]), length(s),
                                      Request.ContentLength, nil)
        else
          FHeaderOut:=HttpSendRequest(hRequest, nil, 0, Addr(s[0]), length(s),
                                      Request.ContentLength, nil);
        end
      else
        begin
        if sndHeader<>'' then
          FHeaderOut:=HttpSendRequest(hRequest, RtcPtrWideChar(sndHeader), length(sndHeader), nil, 0,
                                      Request.ContentLength, nil)
        else
          FHeaderOut:=HttpSendRequest(hRequest, nil, 0, nil, 0,
                                      Request.ContentLength, nil);
        end;
      FHeaderEx:=FHeaderOut and (Request.Contentlength>length(s));
      end;

    if hRequest=nil then
      begin
      FHeaderOut:=False;
      Break;
      end
    else if not FHeaderOut then
      begin
      lastErr:=GetLastError;
      if (lastErr = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) or
         (lastErr = ERROR_WINHTTP_SECURE_INVALID_CA) or
         (lastErr = ERROR_WINHTTP_SECURE_FAILURE) then
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
      ex:=RtcWinHttpException.Create('Error Sending the Request [Code #'+IntToStr(GetLastError)+'].');
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
    LenToWrite:=Request.ContentLength-length(s);

    FDataOut:=length(Request.Method)+length(Request.URI)+10;
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

procedure TRtcWinHttpClientProvider.SendHeaderOut(const s:RtcString);
  var
    ex:Exception;
    certOK,proxyOK:boolean;
    lastErr:DWORD;
  begin
  SetupProxy;

  if FUseHttps and (FCertStoreType<>certNone) and not hStoreReady then
    SetupCertificate;

  FHeaderOut:=False;
  FHeaderEx:=False;
  certOK:=False;
  proxyOK:=False;

  sndHeader:=RtcWideString(Request.HeaderText);
  repeat
    if hRequest=nil then
      Break
    else
      begin
      if length(s)>0 then
        begin
        {$IFDEF RTC_BYTESTRING}
        if sndHeader<>'' then
          FHeaderOut:=HttpSendRequest(hRequest, RtcPtrWideChar(sndHeader), length(sndHeader), Addr(s[1]), length(s),
                                      Request.ContentLength, nil)
        else
          FHeaderOut:=HttpSendRequest(hRequest, nil, 0, Addr(s[1]), length(s),
                                      Request.ContentLength, nil);
        {$ELSE}
        if sndHeader<>'' then
          FHeaderOut:=HttpSendRequest(hRequest, RtcPtrWideChar(sndHeader), length(sndHeader), Addr(RtcStringToBytes(s)[0]), length(s),
                                      Request.ContentLength, nil)
        else
          FHeaderOut:=HttpSendRequest(hRequest, nil, 0, Addr(RtcStringToBytes(s)[0]), length(s),
                                      Request.ContentLength, nil);
        {$ENDIF}
        end
      else
        begin
        if sndHeader<>'' then
          FHeaderOut:=HttpSendRequest(hRequest, RtcPtrWideChar(sndHeader), length(sndHeader), nil, 0,
                                      0, nil)
        else
          FHeaderOut:=HttpSendRequest(hRequest, nil, 0, nil, 0,
                                      0, nil);
        end;
      FHeaderEx:=FHeaderOut and (Request.Contentlength>length(s));
      end;

    if hRequest=nil then
      begin
      FHeaderOut:=False;
      Break;
      end
    else if not FHeaderOut then
      begin
      lastErr:=GetLastError;
      if (lastErr = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) or
         (lastErr = ERROR_WINHTTP_SECURE_INVALID_CA) or
         (lastErr = ERROR_WINHTTP_SECURE_FAILURE) then
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
      ex:=RtcWinHttpException.Create('Error Sending the Request [Code #'+IntToStr(GetLastError)+'].');
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
    LenToWrite:=Request.ContentLength-length(s);
    FDataOut:=length(Request.Method)+length(Request.URI)+10;
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

procedure TRtcWinHttpClientProvider.WriteHeader(SendNow:boolean=True);
  var
    ex:Exception;
    hReq:HINTERNET;
    myWMethod,
    myWURI,
    myWHTTP:RtcWideString;
    myDisableFeature:DWORD;
    myFlags:DWORD;
{$IFDEF RTC_USESETTIMEOUTS}
    myTimeout:DWORD;
{$ENDIF}

  begin
  if not _Active then Exit;

  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  if FUseHttps then
    myFlags:=WINHTTP_FLAG_REFRESH or WINHTTP_FLAG_SECURE
  else
    myFlags:=WINHTTP_FLAG_REFRESH;
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

    myWMethod:=RtcWideString(FMyLastMethod);
    myWURI:=RtcWideString(FMyLastURI);
    if Request.Close then
      myWHTTP:='HTTP/1.0'
    else
      myWHTTP:='HTTP/1.1';

    hRequest := HttpOpenRequest(hConnect, RtcPtrWideChar(myWMethod), RtcPtrWideChar(myWURI), RtcPtrWideChar(myWHTTP), '', nil, myFlags);

    if hRequest=nil then
      begin
      if _Active then
        begin
        ex:=RtcWinHttpException.Create('Error opening HTTP Request [Code #'+IntToStr(GetLastError)+'].');
        try
          TriggerException(ex);
        finally
          RtcFreeAndNil(ex);
          end;
        InternalDisconnect;
        end;
      Exit;
      end;

    myDisableFeature := WINHTTP_DISABLE_REDIRECTS;
    InternetSetOption(hRequest, WINHTTP_OPTION_DISABLE_FEATURE, Addr(myDisableFeature), SizeOf(myDisableFeature));

    myFlags := SOCK_SEND_BUFFER_SIZE;
    InternetSetOption(hRequest, WINHTTP_OPTION_WRITE_BUFFER_SIZE, Addr(myFlags), SizeOf(myFlags));

    myFlags := SOCK_READ_BUFFER_SIZE;
    InternetSetOption(hRequest, WINHTTP_OPTION_READ_BUFFER_SIZE, Addr(myFlags), SizeOf(myFlags));

  {$IFDEF RTC_USESETTIMEOUTS}
    if assigned(TimeoutsOfAPI) then
      begin
      if TimeoutsOfAPI.ResolveTimeout > 0 then
        begin
        myTimeout := TimeoutsOfAPI.ResolveTimeout * 1000;
        InternetSetOption(hRequest, WINHTTP_OPTION_RESOLVE_TIMEOUT, Addr(myTimeout), SizeOf(myTimeout));
        end;

      if TimeoutsOfAPI.ConnectTimeout > 0 then
        begin
        myTimeout := TimeoutsOfAPI.ConnectTimeout * 1000;
        InternetSetOption(hRequest, WINHTTP_OPTION_CONNECT_TIMEOUT, Addr(myTimeout), SizeOf(myTimeout));
        end;

      if TimeoutsOfAPI.SendTimeout > 0 then
        begin
        myTimeout := TimeoutsOfAPI.SendTimeout * 1000;
        InternetSetOption(hRequest, WINHTTP_OPTION_SEND_TIMEOUT, Addr(myTimeout), SizeOf(myTimeout));
        end;

      if TimeoutsOfAPI.ReceiveTimeout > 0 then
        begin
        myTimeout := TimeoutsOfAPI.ReceiveTimeout * 1000;
        InternetSetOption(hRequest, WINHTTP_OPTION_RECEIVE_TIMEOUT, Addr(myTimeout), SizeOf(myTimeout));
        end;

      if TimeoutsOfAPI.ResponseTimeout > 0 then
        begin
        myTimeout := TimeoutsOfAPI.ResponseTimeout * 1000;
        InternetSetOption(hRequest, WINHTTP_OPTION_RECEIVE_RESPONSE_TIMEOUT, Addr(myTimeout), SizeOf(myTimeout));
        end;
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

procedure TRtcWinHttpClientProvider.WriteHeader(const Header_Text: RtcString; SendNow:boolean=True);
  begin
  if not _Active then Exit;

  Request.HeaderText:=Header_Text;
  WriteHeader(SendNow);
  end;

procedure TRtcWinHttpClientProvider.WriteEx(const s: RtcByteArray; SendNow:boolean=True);
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
        ex:=RtcWinHttpException.Create('Error Sending the Request [Code #'+IntToStr(GetLastError)+'].');
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

procedure TRtcWinHttpClientProvider.Write(const s: RtcString; SendNow:boolean=True);
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
        ex:=RtcWinHttpException.Create('Error Sending the Request [Code #'+IntToStr(GetLastError)+'].');
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

procedure TRtcWinHttpClientProvider.LeavingEvent;
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

procedure TRtcWinHttpClientProvider.AcceptResponse;
  var
    dwBufLen,dwIndex:DWord;
    LenToRead:int64;

    hReq:HINTERNET;

    AllReceived:boolean;

    InBuffer:RtcByteArray;
    recvHeader:RtcWideString;

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

    // if FHeaderEx then
    if not HttpEndRequest(hRequest, nil) then
      begin
      InternalDisconnect;
      Exit;
      end;

    sndHeader:='';

    FHeaderOut:=False;
    Response.Started:=True;
    Response.Receiving:=True;

    FResponseBuffer.Clear;

    // Get Raw Header ...
    recvHeader:=' ';
    dwBufLen:=1;
    dwIndex:=0;

    if hRequest=nil then
      begin
      InternalDisconnect;
      Exit;
      end;

    if not HttpQueryInfo(hRequest, WINHTTP_QUERY_RAW_HEADERS_CRLF, nil, Addr(recvHeader[1]), dwBufLen, dwIndex) then
      begin
      if not _Active then Exit;

      if GetLastError<>ERROR_INSUFFICIENT_BUFFER then
        begin
        if _Active then
          begin
          ex:=RtcWinHttpException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
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
        SetLength(recvHeader, dwBufLen div 2);  // DIV 2 because we are receiving the Header as RtcWideString but dwBufLen is in bytes
        if not HttpQueryInfo(hRequest, WINHTTP_QUERY_RAW_HEADERS_CRLF, nil, Addr(recvHeader[1]), dwBufLen, dwIndex) then
          begin
          if _Active then
            begin
            ex:=RtcWinHttpException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
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
      SetLength(recvHeader,dwBufLen div 2); // DIV 2 because we are receiving the Header as RtcWideString but dwBufLen is in bytes

    FDataIn:=length(recvHeader);
    TriggerDataIn;

    Response.HeaderText:=RtcString(recvHeader);

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
          ex:=RtcWinHttpException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
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
          ex:=RtcWinHttpException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
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
    if not (_Active and AllReceived) then
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

function TRtcWinHttpClientProvider._Active: boolean;
  begin
  Result:=not Closing and (FState>=conActivating);
  end;

function TRtcWinHttpClientProvider.ReleaseMe(FromInside:boolean):boolean;
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

function TRtcWinHttpClientProvider.SetupProxy:boolean;
  var
    myWOption:RtcWideString;
  begin
  Result:=False;

  if (FProxyUsername='') and (FUsername='') then Exit;

  if FProxyUsername<>'' then
    begin
    myWOption:=RtcWideString(FProxyUsername);
    if not InternetSetOption(hRequest, WINHTTP_OPTION_PROXY_USERNAME,
                              RtcPtrWideChar(myWOption), length(FProxyUsername)) then Exit;
    end;

  if FProxyPassword<>'' then
    begin
    myWOption:=RtcWideString(FProxyPassword);
    if not InternetSetOption(hRequest, WINHTTP_OPTION_PROXY_PASSWORD,
                              RtcPtrWideChar(myWOption), length(FProxyPassword)) then Exit;
    end;

  if FUsername<>'' then
    begin
    myWOption:=RtcWideString(FUsername);
    if not InternetSetOption(hRequest, WINHTTP_OPTION_USERNAME,
                              RtcPtrWideChar(myWOption), length(FUsername)) then Exit;
    end;

  if FUserPassword<>'' then
    begin
    myWOption:=RtcWideString(FUserPassword);
    if not InternetSetOption(hRequest, WINHTTP_OPTION_PASSWORD,
                              RtcPtrWideChar(myWOption), length(FUserPassword)) then Exit;
    end;

  Result:=True;
  end;

function TRtcWinHttpClientProvider.SetupCertificate:boolean;
  var
    lpszStoreName,
    lpszSubjectName:RtcByteArray;
    dwFlags, dwBuffLen:DWORD;
    pDWFlags:^DWORD;
    res:bool;
  begin
  Result:=False;

  SetLength(lpszStoreName,0);
  SetLength(lpszSubjectName,0);

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
    InternetQueryOption (hRequest, WINHTTP_OPTION_SECURITY_FLAGS,
            pdwFlags, dwBuffLen);

    pdwFlags^ := pdwFlags^ or RTC_WINHTTP_SECURITY_FLAGS;

    res := InternetSetOption (hRequest, WINHTTP_OPTION_SECURITY_FLAGS,
                              pdwFlags, dwBuffLen );

    if res then
      begin
      // hStoreReady:=True;
      Result:=True;
      end;
    end
  else
    begin
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
      lpszSubjectName:= RtcStringToBytesZero(FCertSubject);

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

{ TRtcWinHttpClientThread }

constructor TRtcWinHttpClientThread.Create;
  begin
  inherited;
  Releasing:=False;
  RtcConn:=nil;
  NeedThread:=True;
  end;

procedure TRtcWinHttpClientThread.OpenConn;
  begin
  if assigned(RtcConn) and not Releasing then
    RtcConn.OpenConnection(False);
  end;

procedure TRtcWinHttpClientThread.ReOpenConn;
  begin
  if assigned(RtcConn) and not Releasing then
    RtcConn.OpenConnection(True);
  end;

procedure TRtcWinHttpClientThread.CloseConn(_lost:boolean);
  begin
  try
    if assigned(RtcConn) then
      begin
      RtcConn.Lost:=_lost;
      RtcConn.InternalDisconnect;
      end;
  except
    on E:Exception do
      if LOG_WINHTTP_ERRORS then
        Log('TRtcWinHttpClientThread.CloseConn: RtConn.InternalDisconnect',E,'ERROR');
    end;
  end;

destructor TRtcWinHttpClientThread.Destroy;
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
        Log('TRtcWinHttpClientThread.Destroy',E,'ERROR');
    end;
  end;

function TRtcWinHttpClientThread.RunJob:boolean;
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
      if LOG_WINHTTP_ERRORS then
        Log('TRtcWinHttpClientThread.RunJob',E,'ERROR');
      CloseConn(true);
      Result:=True;
      end;
    end;
  end;

type
  TMyWinHTTP=class
    public
    constructor Create;
    destructor Destroy; override;
    end;

var
  MyWinHTTP:TMyWinHTTP;

{ TMyWinInet }

constructor TMyWinHTTP.Create;
  begin
  inherited;
  Message_WSOpenConn:=TRtcBaseMessage.Create;
  Message_WSReOpenConn:=TRtcBaseMessage.Create;
  Message_WSCloseConn:=TRtcBaseMessage.Create;
  Message_WSStop:=TRtcBaseMessage.Create;
  Message_WSRelease:=TRtcBaseMessage.Create;
  end;

destructor TMyWinHTTP.Destroy;
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
        Log('TMyWinHTTP.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcWinHttpCliProv Initializing ...','DEBUG');{$ENDIF}
MyWinHTTP:=TMyWinHTTP.Create;
{$IFDEF RTC_DEBUG} Log('rtcWinHttpCliProv Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcWinHttpCliProv Finalizing ...','DEBUG');{$ENDIF}
CloseThreadPool;
RtcFreeAndNil(MyWinHTTP);
{$IFDEF RTC_DEBUG} Log('rtcWinHttpCliProv Finalized.','DEBUG');{$ENDIF}

{$ENDIF} // {$IFDEF WINDOWS}
end.
