{
  "Low-level WinInet HTTP Client API functions"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br>)

  This unit is ONLY for MS Windows.

  @exclude
}
unit rtcWInetHttp;

{$INCLUDE rtcDefs.inc}

interface

{$IFDEF WINDOWS}

uses
  Windows,
  SysUtils,

  rtcTypes,
  rtcSystem,
  rtcLog;

const
  WINET_SECURITY_FLAG_IGNORE_REVOCATION             = $00000080;
  WINET_SECURITY_FLAG_IGNORE_UNKNOWN_CA             = $00000100;
  WINET_SECURITY_FLAG_IGNORE_WRONG_USAGE            = $00000200;
  WINET_SECURITY_FLAG_IGNORE_CERT_CN_INVALID        = $00001000;
  WINET_SECURITY_FLAG_IGNORE_CERT_DATE_INVALID      = $00002000;
  WINET_SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTPS      = $00004000;
  WINET_SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTP       = $00008000;

type
  RtcWInetException = class(Exception);

  HINTERNET = Pointer;

{ INTERNET_BUFFERS - combines headers and data. May be chained for e.g. file }
{ upload or scatter/gather operations. For chunked read/write, lpcszHeader }
{ contains the chunked-ext }
  PInternetBuffers = ^INTERNET_BUFFERS;
  INTERNET_BUFFERS = record
    dwStructSize: DWORD;      { used for API versioning. Set to sizeof(INTERNET_BUFFERS) }
    Next: PInternetBuffers;   { chain of buffers }
    lpcszHeader: RtcPtrAnsiChar;       { pointer to headers (may be NULL) }
    dwHeadersLength: DWORD;   { length of headers if not NULL }
    dwHeadersTotal: DWORD;    { size of headers if not enough buffer }
    lpvBuffer: Pointer;       { pointer to data buffer (may be NULL) }
    dwBufferLength: DWORD;    { length of data buffer if not NULL }
    dwBufferTotal: DWORD;     { total size of chunk, or content-length if not chunked }
    dwOffsetLow: DWORD;       { used for read-ranges (only used in HttpSendRequest2) }
    dwOffsetHigh: DWORD;
  end;

const
  INTERNET_DEFAULT_HTTP_PORT = 80;                  {    "     "  HTTP   " }
  INTERNET_DEFAULT_HTTPS_PORT = 443;                {    "     "  HTTPS  " }

  INTERNET_OPEN_TYPE_PRECONFIG = 0;  { use registry configuration }
  INTERNET_OPEN_TYPE_PROXY = 3;

  INTERNET_OPTION_USERNAME = 28;
  INTERNET_OPTION_PASSWORD = 29;
  INTERNET_OPTION_PROXY_USERNAME = 43;
  INTERNET_OPTION_PROXY_PASSWORD = 44;

  INTERNET_SERVICE_HTTP = 3;

  INTERNET_OPTION_SECURITY_FLAGS              = 31;

  INTERNET_FLAG_NO_CACHE_WRITE = $04000000;  { don't write this item to the cache }
  INTERNET_FLAG_RELOAD = $80000000;                 { retrieve the original item }
  INTERNET_FLAG_SECURE = $00800000;  { use PCT/SSL if applicable (HTTP) }

  INTERNET_FLAG_IGNORE_CERT_CN_INVALID        = $00001000; { bad common name in X509 Cert. }
  INTERNET_FLAG_IGNORE_CERT_DATE_INVALID      = $00002000; { expired X509 Cert. }
  INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS      = $00004000; { ex: http:// to https:// }
  INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP       = $00008000; { ex: https:// to http:// }

  INTERNET_FLAG_NO_AUTO_REDIRECT      = $00200000;  { don't handle redirections automatically }

  SECURITY_FLAG_IGNORE_REVOCATION             = $00000080;
  SECURITY_FLAG_IGNORE_UNKNOWN_CA             = $00000100;
  SECURITY_FLAG_IGNORE_WRONG_USAGE            = $00000200;
  SECURITY_FLAG_IGNORE_CERT_CN_INVALID        = INTERNET_FLAG_IGNORE_CERT_CN_INVALID;
  SECURITY_FLAG_IGNORE_CERT_DATE_INVALID      = INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;
  SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTPS      = INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS;
  SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTP       = INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP;

  INTERNET_ERROR_BASE                         = 12000;
  ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED      = INTERNET_ERROR_BASE + 44;
  ERROR_INTERNET_INVALID_CA                   = INTERNET_ERROR_BASE + 45;
  HTTP_QUERY_RAW_HEADERS_CRLF                 = 22; { special: all headers }

const
  CERT_STORE_CLOSE_FORCE_FLAG = 1;

  X509_ASN_ENCODING:DWORD = 1;
  PKCS_7_ASN_ENCODING:DWORD = 65536;

  CERT_FIND_SUBJECT_STR_A = 458759;
  CERT_FIND_SUBJECT_STR_W = 524295;
  CERT_FIND_ISSUER_STR_A = 458756;
  CERT_FIND_ISSUER_STR_W = 524292;

  INTERNET_OPTION_CLIENT_CERT_CONTEXT = 84;

  HSR_INITIATE    = $00000008;                       { iterative operation (completed by HttpEndRequest) }

{$IFDEF RTC_USESETTIMEOUTS}
  INTERNET_OPTION_CONNECT_TIMEOUT = 2;
  INTERNET_OPTION_SEND_TIMEOUT = 5;
  INTERNET_OPTION_RECEIVE_TIMEOUT = 6;
{$ENDIF RTC_USESETTIMEOUTS}

type
  INTERNET_PORT = Word;

  EWinCryptException=Class(Exception);
  EWinInetException=Class(Exception);

  HCERTSTORE = pointer;
  PCERT_INFO = pointer;

  CERT_CONTEXT = packed record
	  dwCertEncodingType:DWORD;
	  pbCertEncoded:pointer;
	  cbCertEncoded:DWORD;
	  pCertInfo:PCERT_INFO;
	  hCertStore:HCERTSTORE;
    end;

  PCCERT_CONTEXT = ^CERT_CONTEXT;

  {CertOpenSystemStoreA}
  TCertOpenSystemStore =        function(hprov:pointer;
                                         szSubsystemProtocol:RtcPtrAnsiChar):HCERTSTORE; stdcall;
  {CertCloseStore}
  TCertCloseStore =             function(hCertStore:HCERTSTORE;
                                         dwFlags:DWORD):BOOL; stdcall;
  {CertFindCertificateInStore}
  TCertFindCertificateInStore = function(hCertStore:HCERTSTORE;
                                         dwCertEncodingType:DWORD;
                                         dwFindFlags:DWORD;
                                         dwFindType:DWORD;
                                         pvFindPara:RtcPtrAnsiChar;
                                         pPrevCertContext:PCCERT_CONTEXT):PCCERT_CONTEXT; stdcall;
  {CertFreeCertificateContext}
  TCertFreeCertificateContext = function(pCertContext:PCCERT_CONTEXT):BOOL; stdcall;

  {InternetOpen}
  TInternetOpen =               function(lpszAgent: RtcPtrAnsiChar; dwAccessType: DWORD;
                                         lpszProxy, lpszProxyBypass:
                                         RtcPtrAnsiChar; dwFlags: DWORD): HINTERNET; stdcall;

  {InternetConnect}
  TInternetConnect =             function(hInet: HINTERNET; lpszServerName: RtcPtrAnsiChar;
                                          nServerPort: INTERNET_PORT;
                                          lpszUsername: RtcPtrAnsiChar; lpszPassword: RtcPtrAnsiChar;
                                          dwService: DWORD; dwFlags: DWORD;
                                          dwContext: DWORD): HINTERNET; stdcall;

  {InternetCloseHandle}
  TInternetCloseHandle =         function(hInet: HINTERNET): BOOL; stdcall;

  {InternetQueryOption}
  TInternetQueryOption =         function(hInet: HINTERNET; dwOption: DWORD;
                                          lpBuffer: Pointer;
                                          var lpdwBufferLength: DWORD): BOOL; stdcall;

  {HttpOpenRequest}
  THttpOpenRequest =             function(hConnect: HINTERNET; lpszVerb: RtcPtrAnsiChar;
                                          lpszObjectName: RtcPtrAnsiChar;
                                          lpszVersion: RtcPtrAnsiChar; lpszReferrer: RtcPtrAnsiChar;
                                          lplpszAcceptTypes: PLPSTR; dwFlags: DWORD;
                                          dwContext: DWORD): HINTERNET; stdcall;

  {HttpSendRequest}
  THttpSendRequest =             function(hRequest: HINTERNET; lpszHeaders: RtcPtrAnsiChar;
                                          dwHeadersLength: DWORD; lpOptional: Pointer;
                                          dwOptionalLength: DWORD): BOOL; stdcall;

  {HttpSendRequestEx}
  THttpSendRequestEx =           function(hRequest: HINTERNET; lpBuffersIn: PInternetBuffers;
                                          lpBuffersOut: PInternetBuffers;
                                          dwFlags: DWORD; dwContext: DWORD): BOOL; stdcall;

  {HttpEndRequest}
  THttpEndRequest =              function(hRequest: HINTERNET;
                                          lpBuffersOut: PInternetBuffers; dwFlags: DWORD;
                                          dwContext: DWORD): BOOL; stdcall;

  {InternetReadFile}
  TInternetReadFile =            function(hFile: HINTERNET; lpBuffer: Pointer;
                                          dwNumberOfBytesToRead: DWORD;
                                          var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;

  {InternetWriteFile}
  TInternetWriteFile =           function(hFile: HINTERNET; lpBuffer: Pointer;
                                          dwNumberOfBytesToWrite: DWORD;
                                          var lpdwNumberOfBytesWritten: DWORD): BOOL; stdcall;

  {HttpQueryInfo}
  THttpQueryInfo =               function(hRequest: HINTERNET; dwInfoLevel: DWORD;
                                          lpvBuffer: Pointer; var lpdwBufferLength: DWORD;
                                          var lpdwReserved: DWORD): BOOL; stdcall;

  {InternetSetOption}
  TInternetSetOption =           function(hInet: HINTERNET; dwOption: DWORD;
                                          lpBuffer: Pointer;
                                          dwBufferLength: DWORD): BOOL; stdcall;


var
  CertOpenSystemStore: TCertOpenSystemStore;
  CertCloseStore: TCertCloseStore;
  CertFindCertificateInStore: TCertFindCertificateInStore;
  CertFreeCertificateContext: TCertFreeCertificateContext;

  InternetOpen: TInternetOpen;
  InternetConnect: TInternetConnect;
  InternetCloseHandle: TInternetCloseHandle;
  HttpOpenRequest: THttpOpenRequest;
  HttpSendRequest: THttpSendRequest;
  HttpSendRequestEx: THttpSendRequestEx;
  HttpEndRequest: THttpEndRequest;
  InternetReadFile: TInternetReadFile;
  InternetWriteFile: TInternetWriteFile;
  HttpQueryInfo: THttpQueryInfo;
  InternetSetOption: TInternetSetOption;
  InternetQueryOption: TInternetQueryOption;

procedure LoadWinInet;
procedure LoadWinCrypt;

function Have_WinInet:boolean;

{$ENDIF} // {$IFDEF WINDOWS}

implementation

{$IFDEF WINDOWS}

const
  winetdll = 'wininet.dll';
  wincrypt = 'crypt32.dll';

var
  LibCS:TRtcCritSec;

  TriedLoading:boolean = False;
  TriedLoading2:boolean = False;
  FDllHandle:THandle = 0;
  FDllHandle2:THandle = 0;

function WinCryptGetProc(const ProcName : RtcString) : Pointer;
  var
    xProcName:RtcByteArray;
  begin
  xProcName:=nil;
  if Length(ProcName) = 0 then
    Result := nil
  else
    begin
    xProcName:=RtcStringToBytesZero(ProcName);
    Result := GetProcAddress(FDllHandle, RtcPtrAnsiChar(@(xProcName[0])) );
    if Result = nil then
      raise EWinCryptException.Create('Procedure ' + String(ProcName) +
                                      ' not found in ' + wincrypt +
                                      ' Error #' + IntToStr(GetLastError));
    end;
  end;

procedure LoadWinCrypt;
  var
    xWinCrypt:RtcByteArray;
  begin
  xWinCrypt:=nil;
  LibCS.Acquire;
  try
    if FDllHandle = 0 then
      begin
      if not TriedLoading then
        begin
        TriedLoading:=True;
        xWinCrypt:=RtcStringToBytesZero(wincrypt);
        FDllHandle := LoadLibraryA( @(xWinCrypt[0]) );
        if FDllHandle = 0 then
          raise EWinCryptException.Create('Unable to load ' + wincrypt +
                                        ' Error #' + IntToStr(GetLastError));

        try
          CertOpenSystemStore := TCertOpenSystemStore(WinCryptGetProc('CertOpenSystemStoreA'));
          CertCloseStore := TCertCloseStore(WinCryptGetProc('CertCloseStore'));
          CertFindCertificateInStore := TCertFindCertificateInStore(WinCryptGetProc('CertFindCertificateInStore'));
          CertFreeCertificateContext := TCertFreeCertificateContext(WinCryptGetProc('CertFreeCertificateContext'));
        except
          FreeLibrary(FDllHandle);
          FDllHandle:=0;
          raise;
          end;
        end;
      end;
  finally
    LibCS.Release;
    end;
  end;

procedure UnloadWinCrypt;
  begin
  LibCS.Acquire;
  try
    if FDllHandle<>0 then
      begin
      FreeLibrary(FDllHandle);
      FDllHandle:=0;
      end;
  finally
    LibCS.Release;
    end;
  end;

function WinInetGetProc(const ProcName : RtcString) : Pointer;
  var
    xProcName:RtcByteArray;
  begin
  xProcName:=nil;
  if Length(ProcName) = 0 then
    Result := nil
  else
    begin
    xProcName:=RtcStringToBytesZero(ProcName);
    Result := GetProcAddress(FDllHandle2, RtcPtrAnsiChar(@(xProcName[0])) );
    if Result = nil then
      raise EWinInetException.Create('Procedure ' + String(ProcName) +
                                      ' not found in ' + winetdll +
                                      ' Error #' + IntToStr(GetLastError));
    end;
  end;

procedure LoadWinInet;
  var
    xWInetDll:RtcByteArray;
  begin
  xWInetDll:=nil;
  LibCS.Acquire;
  try
    if FDllHandle2 = 0 then
      begin
      if not TriedLoading2 then
        begin
        TriedLoading2:=True;
        xWInetDll:=RtcStringToBytesZero(winetdll);
        FDllHandle2 := LoadLibraryA( @(xWInetDll[0]) );
        if FDllHandle2 <> 0 then
          begin
          try
            InternetOpen := TInternetOpen(WinInetGetProc('InternetOpenA'));
            InternetConnect := TInternetConnect(WinInetGetProc('InternetConnectA'));
            InternetCloseHandle := TInternetCloseHandle(WinInetGetProc('InternetCloseHandle'));
            HttpOpenRequest := THttpOpenRequest(WinInetGetProc('HttpOpenRequestA'));
            HttpSendRequest := THttpSendRequest(WinInetGetProc('HttpSendRequestA'));
            HttpSendRequestEx := THttpSendRequestEx(WinInetGetProc('HttpSendRequestExA'));
            HttpEndRequest := THttpEndRequest(WinInetGetProc('HttpEndRequestA'));
            InternetReadFile := TInternetReadFile(WinInetGetProc('InternetReadFile'));
            InternetWriteFile := TInternetWriteFile(WinInetGetProc('InternetWriteFile'));
            HttpQueryInfo := THttpQueryInfo(WinInetGetProc('HttpQueryInfoA'));
            InternetSetOption := TInternetSetOption(WinInetGetProc('InternetSetOptionA'));
            InternetQueryOption := TInternetQueryOption(WinInetGetProc('InternetQueryOptionA'));
          except
            FreeLibrary(FDllHandle2);
            FDllHandle2:=0;
            end;
          end;
        end;
      end;
  finally
    LibCS.Release;
    end;
  end;

procedure UnloadWinInet;
  begin
  LibCS.Acquire;
  try
    if FDllHandle2<>0 then
      begin
      FreeLibrary(FDllHandle2);
      FDllHandle2:=0;
      end;
  finally
    LibCS.Release;
    end;
  end;

function Have_WinInet:boolean;
  begin
  LoadWinInet;
  LibCS.Acquire;
  try
    Result:=FDllHandle2<>0;
  finally
    LibCS.Release;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; {$ENDIF}
LibCS:=TRtcCritSec.Create;
finalization
{$IFDEF RTC_DEBUG} Log('rtcWInetHttp Finalizing ...','DEBUG');{$ENDIF}
UnloadWinInet;
UnloadWinCrypt;
RtcFreeAndNil(LibCS);
{$IFDEF RTC_DEBUG} Log('rtcWInetHttp Finalized.','DEBUG');{$ENDIF}

{$ENDIF} // {$IFDEF WINDOWS}
end.
