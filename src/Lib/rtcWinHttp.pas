{
  "Low-level Win HTTP Client API functions"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br>)

  This unit is ONLY for MS Windows.

  @exclude
}
unit rtcWinHttp;

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
  WINHTTP_SECURITY_FLAG_IGNORE_REVOCATION         = $00000080;
  WINHTTP_SECURITY_FLAG_IGNORE_UNKNOWN_CA         = $00000100;
  WINHTTP_SECURITY_FLAG_IGNORE_CERT_DATE_INVALID  = $00002000;
  WINHTTP_SECURITY_FLAG_IGNORE_CERT_CN_INVALID    = $00001000;
  WINHTTP_SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE   = $00000200;

  WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM     = 0;
  WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW        = 1;
  WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH       = 2;
  WINHTTP_AUTOLOGON_SECURITY_LEVEL_DEFAULT    = WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM;

type
  RtcWinHttpException = class(Exception);

  HINTERNET = Pointer;

  INTERNET_PORT = WORD;

Const
  INTERNET_DEFAULT_PORT              = 0;           // use the protocol-specific default
  INTERNET_DEFAULT_HTTP_PORT         = 80;          //    "     "  HTTP   "
  INTERNET_DEFAULT_HTTPS_PORT        = 443;         //    "     "  HTTPS  "

  WINHTTP_FLAG_ASYNC                 = $10000000;  // this session is asynchronous (where supported)

  WINHTTP_FLAG_SECURE                = $00800000;  // use SSL if applicable (HTTPS)
  WINHTTP_FLAG_ESCAPE_PERCENT        = $00000004;  // if escaping enabled, escape percent as well
  WINHTTP_FLAG_NULL_CODEPAGE         = $00000008;  // assume all symbols are ASCII, use fast convertion
  WINHTTP_FLAG_BYPASS_PROXY_CACHE    = $00000100; // add "pragma: no-cache" request header
  WINHTTP_FLAG_REFRESH               = WINHTTP_FLAG_BYPASS_PROXY_CACHE;
  WINHTTP_FLAG_ESCAPE_DISABLE        = $00000040;  // disable escaping
  WINHTTP_FLAG_ESCAPE_DISABLE_QUERY  = $00000080;  // if escaping enabled escape path part, but do not escape query

  SECURITY_FLAG_IGNORE_REVOCATION         = $00000080;
  SECURITY_FLAG_IGNORE_UNKNOWN_CA         = $00000100;
  SECURITY_FLAG_IGNORE_CERT_DATE_INVALID  = $00002000; // expired X509 Cert.
  SECURITY_FLAG_IGNORE_CERT_CN_INVALID    = $00001000; // bad common name in X509 Cert.
  SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE   = $00000200;

Type
  PWINHTTP_ASYNC_RESULT=^WINHTTP_ASYNC_RESULT;
  WINHTTP_ASYNC_RESULT = packed record
    dwResult:PDWORD;  // indicates which async API has encountered an error
    dwError:DWORD;       // the error code if the API failed
  End;

  PHTTP_VERSION_INFO = ^HTTP_VERSION_INFO;
  HTTP_VERSION_INFO = packed record
    dwMajorVersion:DWORD;
    dwMinorVersion:DWORD;
  End;

  PINTERNET_SCHEME = ^INTERNET_SCHEME;
  INTERNET_SCHEME = integer;

  PURL_COMPONENTS = ^URL_COMPONENTS;
  URL_COMPONENTS = packed record
    dwStructSize:DWORD;
    lpszScheme:RtcPtrWideChar;
    dwSchemeLength:DWORD;
    nScheme:INTERNET_SCHEME;
    lpszHostName:RtcPtrWideChar;
    dwHostNameLength:DWORD;
    nPort:INTERNET_PORT;
    lpszUserName:RtcPtrWideChar;
    dwUserNameLength:DWORD;
    lpszPassword:RtcPtrWideChar;
    dwPasswordLength:DWORD;
    lpszUrlPath:RtcPtrWideChar;
    dwUrlPathLength:DWORD;
    lpszExtraInfo:RtcPtrWideChar;
    dwExtraInfoLength:DWORD;
  End;

  PWINHTTP_PROXY_INFO = ^WINHTTP_PROXY_INFO;
  WINHTTP_PROXY_INFO = packed record
    dwAccessType:DWORD;      // see WINHTTP_ACCESS_* types below
    lpszProxy:RtcPtrWideChar;         // proxy server list
    lpszProxyBypass:RtcPtrWideChar;   // proxy bypass list
  End;

  PWINHTTP_AUTOPROXY_OPTIONS = ^WINHTTP_AUTOPROXY_OPTIONS;
  WINHTTP_AUTOPROXY_OPTIONS = packed record
    dwFlags:DWORD;
    dwAutoDetectFlags:DWORD;
    lpszAutoConfigUrl:RtcPtrWideChar;
    lpvReserved:Pointer;
    dwReserved:DWORD;
    fAutoLogonIfChallenged:BOOL;
  End;

Const
  WINHTTP_AUTOPROXY_AUTO_DETECT           = $00000001;
  WINHTTP_AUTOPROXY_CONFIG_URL            = $00000002;
  WINHTTP_AUTOPROXY_RUN_INPROCESS         = $00010000;
  WINHTTP_AUTOPROXY_RUN_OUTPROCESS_ONLY   = $00020000;

  WINHTTP_AUTO_DETECT_TYPE_DHCP           = $00000001;
  WINHTTP_AUTO_DETECT_TYPE_DNS_A          = $00000002;

Type
  PWINHTTP_CERTIFICATE_INFO = ^WINHTTP_CERTIFICATE_INFO;
  WINHTTP_CERTIFICATE_INFO = packed record
    ftExpiry:FILETIME;
    ftStart:FILETIME;
    lpszSubjectInfo:RtcPtrWideChar;
    lpszIssuerInfo:RtcPtrWideChar;
    lpszProtocolName:RtcPtrWideChar;
    lpszSignatureAlgName:RtcPtrWideChar;
    lpszEncryptionAlgName:RtcPtrWideChar;
    dwKeySize:DWORD;
  End;

Const
  WINHTTP_TIME_FORMAT_BUFSIZE             = 62;

  ICU_NO_ENCODE                                         = $20000000;  // Don't convert unsafe characters to escape sequence
  ICU_DECODE                                            = $10000000;  // Convert %XX escape sequences to characters
  ICU_NO_META                             = $08000000;  // Don't convert .. etc. meta path sequences
  ICU_ENCODE_SPACES_ONLY                               = $04000000;  // Encode spaces only
  ICU_BROWSER_MODE                        = $02000000; // Special encode/decode rules for browser
  ICU_ENCODE_PERCENT                      = $00001000;      // Encode any percent (ASCII25)

  ICU_ESCAPE                              = $80000000;  // (un)escape URL characters

  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY       = 0;
  WINHTTP_ACCESS_TYPE_NO_PROXY            = 1;
  WINHTTP_ACCESS_TYPE_NAMED_PROXY         = 3;

  WINHTTP_NO_PROXY_NAME                   = nil;
  WINHTTP_NO_PROXY_BYPASS                 = nil;

  WINHTTP_FIRST_OPTION                    = 1; //WINHTTP_OPTION_CALLBACK;

  WINHTTP_OPTION_CALLBACK                 = 1;
  WINHTTP_OPTION_RESOLVE_TIMEOUT          = 2;
  WINHTTP_OPTION_CONNECT_TIMEOUT          = 3;
  WINHTTP_OPTION_CONNECT_RETRIES          = 4;
  WINHTTP_OPTION_SEND_TIMEOUT             = 5;
  WINHTTP_OPTION_RECEIVE_TIMEOUT          = 6;
  WINHTTP_OPTION_RECEIVE_RESPONSE_TIMEOUT = 7;
  WINHTTP_OPTION_HANDLE_TYPE              = 9;
  WINHTTP_OPTION_READ_BUFFER_SIZE         = 12;
  WINHTTP_OPTION_WRITE_BUFFER_SIZE        = 13;
  WINHTTP_OPTION_PARENT_HANDLE            = 21;
  WINHTTP_OPTION_EXTENDED_ERROR           = 24;
  WINHTTP_OPTION_SECURITY_FLAGS           = 31;
  WINHTTP_OPTION_SECURITY_CERTIFICATE_STRUCT = 32;
  WINHTTP_OPTION_URL                      = 34;
  WINHTTP_OPTION_SECURITY_KEY_BITNESS     = 36;
  WINHTTP_OPTION_PROXY                    = 38;

  WINHTTP_OPTION_USER_AGENT               = 41;
  WINHTTP_OPTION_CONTEXT_VALUE            = 45;
  WINHTTP_OPTION_CLIENT_CERT_CONTEXT          = 47;
  WINHTTP_OPTION_REQUEST_PRIORITY             = 58;
  WINHTTP_OPTION_HTTP_VERSION                 = 59;
  WINHTTP_OPTION_DISABLE_FEATURE              = 63;

  WINHTTP_OPTION_CODEPAGE                     = 68;
  WINHTTP_OPTION_MAX_CONNS_PER_SERVER         = 73;
  WINHTTP_OPTION_MAX_CONNS_PER_1_0_SERVER     = 74;
  WINHTTP_OPTION_AUTOLOGON_POLICY             = 77;
  WINHTTP_OPTION_SERVER_CERT_CONTEXT          = 78;
  WINHTTP_OPTION_ENABLE_FEATURE               = 79;
  WINHTTP_OPTION_WORKER_THREAD_COUNT          = 80;
  WINHTTP_OPTION_PASSPORT_COBRANDING_TEXT     = 81;
  WINHTTP_OPTION_PASSPORT_COBRANDING_URL      = 82;
  WINHTTP_OPTION_CONFIGURE_PASSPORT_AUTH      = 83;
  WINHTTP_OPTION_SECURE_PROTOCOLS             = 84;
  WINHTTP_OPTION_ENABLETRACING                = 85;
  WINHTTP_OPTION_PASSPORT_SIGN_OUT            = 86;
  WINHTTP_OPTION_PASSPORT_RETURN_URL          = 87;
  WINHTTP_OPTION_REDIRECT_POLICY              = 88;
  WINHTTP_OPTION_MAX_HTTP_AUTOMATIC_REDIRECTS = 89;
  WINHTTP_OPTION_MAX_HTTP_STATUS_CONTINUE     = 90;
  WINHTTP_OPTION_MAX_RESPONSE_HEADER_SIZE     = 91;
  WINHTTP_OPTION_MAX_RESPONSE_DRAIN_SIZE      = 92;
  WINHTTP_LAST_OPTION                         = WINHTTP_OPTION_MAX_RESPONSE_DRAIN_SIZE;

  WINHTTP_OPTION_USERNAME                     = $1000;
  WINHTTP_OPTION_PASSWORD                     = $1001;
  WINHTTP_OPTION_PROXY_USERNAME               = $1002;
  WINHTTP_OPTION_PROXY_PASSWORD               = $1003;

  WINHTTP_CONNS_PER_SERVER_UNLIMITED              = $FFFFFFFF;

// values for WINHTTP_OPTION_REDIRECT_POLICY
  WINHTTP_OPTION_REDIRECT_POLICY_NEVER                       = 0;
  WINHTTP_OPTION_REDIRECT_POLICY_DISALLOW_HTTPS_TO_HTTP      = 1;
  WINHTTP_OPTION_REDIRECT_POLICY_ALWAYS                      = 2;

  WINHTTP_OPTION_REDIRECT_POLICY_LAST            = WINHTTP_OPTION_REDIRECT_POLICY_ALWAYS;
  WINHTTP_OPTION_REDIRECT_POLICY_DEFAULT         = WINHTTP_OPTION_REDIRECT_POLICY_DISALLOW_HTTPS_TO_HTTP;

  WINHTTP_DISABLE_PASSPORT_AUTH    = $00000000;
  WINHTTP_ENABLE_PASSPORT_AUTH     = $10000000;
  WINHTTP_DISABLE_PASSPORT_KEYRING = $20000000;
  WINHTTP_ENABLE_PASSPORT_KEYRING  = $40000000;

// values for WINHTTP_OPTION_DISABLE_FEATURE
  WINHTTP_DISABLE_COOKIES                   = $00000001;
  WINHTTP_DISABLE_REDIRECTS                 = $00000002;
  WINHTTP_DISABLE_AUTHENTICATION            = $00000004;
  WINHTTP_DISABLE_KEEP_ALIVE                = $00000008;

// values for WINHTTP_OPTION_ENABLE_FEATURE
  WINHTTP_ENABLE_SSL_REVOCATION             = $00000001;
  WINHTTP_ENABLE_SSL_REVERT_IMPERSONATION   = $00000002;

//
// winhttp handle types
//
  WINHTTP_HANDLE_TYPE_SESSION                 = 1;
  WINHTTP_HANDLE_TYPE_CONNECT                 = 2;
  WINHTTP_HANDLE_TYPE_REQUEST                 = 3;

//
// values for auth schemes
//
  WINHTTP_AUTH_SCHEME_BASIC      = $00000001;
  WINHTTP_AUTH_SCHEME_NTLM       = $00000002;
  WINHTTP_AUTH_SCHEME_PASSPORT   = $00000004;
  WINHTTP_AUTH_SCHEME_DIGEST     = $00000008;
  WINHTTP_AUTH_SCHEME_NEGOTIATE  = $00000010;

// WinHttp supported Authentication Targets

  WINHTTP_AUTH_TARGET_SERVER = $00000000;
  WINHTTP_AUTH_TARGET_PROXY  = $00000001;

//
// values for WINHTTP_OPTION_SECURITY_FLAGS
//

// query only
  SECURITY_FLAG_SECURE                    = $00000001; // can query only
  SECURITY_FLAG_STRENGTH_WEAK             = $10000000;
  SECURITY_FLAG_STRENGTH_MEDIUM           = $40000000;
  SECURITY_FLAG_STRENGTH_STRONG           = $20000000;

// Secure connection error status flags
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED         = $00000001;
  WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT            = $00000002;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED            = $00000004;
  WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA              = $00000008;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID         = $00000010;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID       = $00000020;
  WINHTTP_CALLBACK_STATUS_FLAG_CERT_WRONG_USAGE        = $00000040;
  WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR  = $80000000;

  WINHTTP_FLAG_SECURE_PROTOCOL_SSL2   = $00000008;
  WINHTTP_FLAG_SECURE_PROTOCOL_SSL3   = $00000020;
  WINHTTP_FLAG_SECURE_PROTOCOL_TLS1   = $00000080;
  WINHTTP_FLAG_SECURE_PROTOCOL_ALL    = (WINHTTP_FLAG_SECURE_PROTOCOL_SSL2 or
                                             WINHTTP_FLAG_SECURE_PROTOCOL_SSL3 or
                                             WINHTTP_FLAG_SECURE_PROTOCOL_TLS1);

//
// callback function for WinHttpSetStatusCallback
//

type
  PWINHTTP_STATUS_CALLBACK = ^WINHTTP_STATUS_CALLBACK;
  WINHTTP_STATUS_CALLBACK = procedure(
    hInternet: HINTERNET;
    dwContext: PDWORD;
    dwInternetStatus: DWORD;
    lpvStatusInformation: Pointer;
    dwStatusInformationLength: DWORD); stdcall;

Const
  WINHTTP_CALLBACK_STATUS_RESOLVING_NAME          = $00000001;
  WINHTTP_CALLBACK_STATUS_NAME_RESOLVED           = $00000002;
  WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER    = $00000004;
  WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER     = $00000008;
  WINHTTP_CALLBACK_STATUS_SENDING_REQUEST         = $00000010;
  WINHTTP_CALLBACK_STATUS_REQUEST_SENT            = $00000020;
  WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE      = $00000040;
  WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED       = $00000080;
  WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION      = $00000100;
  WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED       = $00000200;
  WINHTTP_CALLBACK_STATUS_HANDLE_CREATED          = $00000400;
  WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING          = $00000800;
  WINHTTP_CALLBACK_STATUS_DETECTING_PROXY         = $00001000;
  WINHTTP_CALLBACK_STATUS_REDIRECT                = $00004000;
  WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE   = $00008000;
  WINHTTP_CALLBACK_STATUS_SECURE_FAILURE          = $00010000;
  WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE       = $00020000;
  WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE          = $00040000;
  WINHTTP_CALLBACK_STATUS_READ_COMPLETE           = $00080000;
  WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE          = $00100000;
  WINHTTP_CALLBACK_STATUS_REQUEST_ERROR           = $00200000;
  WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE    = $00400000;

  WINHTTP_CALLBACK_FLAG_RESOLVE_NAME             = (WINHTTP_CALLBACK_STATUS_RESOLVING_NAME or WINHTTP_CALLBACK_STATUS_NAME_RESOLVED);
  WINHTTP_CALLBACK_FLAG_CONNECT_TO_SERVER        = (WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER or WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER);
  WINHTTP_CALLBACK_FLAG_SEND_REQUEST             = (WINHTTP_CALLBACK_STATUS_SENDING_REQUEST or WINHTTP_CALLBACK_STATUS_REQUEST_SENT);
  WINHTTP_CALLBACK_FLAG_RECEIVE_RESPONSE         = (WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE or WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED);
  WINHTTP_CALLBACK_FLAG_CLOSE_CONNECTION         = (WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION or WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED);
  WINHTTP_CALLBACK_FLAG_HANDLES                  = (WINHTTP_CALLBACK_STATUS_HANDLE_CREATED or WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING);
  WINHTTP_CALLBACK_FLAG_DETECTING_PROXY          = WINHTTP_CALLBACK_STATUS_DETECTING_PROXY;
  WINHTTP_CALLBACK_FLAG_REDIRECT                 = WINHTTP_CALLBACK_STATUS_REDIRECT;
  WINHTTP_CALLBACK_FLAG_INTERMEDIATE_RESPONSE    = WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE;
  WINHTTP_CALLBACK_FLAG_SECURE_FAILURE           = WINHTTP_CALLBACK_STATUS_SECURE_FAILURE;
  WINHTTP_CALLBACK_FLAG_SENDREQUEST_COMPLETE     = WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE;
  WINHTTP_CALLBACK_FLAG_HEADERS_AVAILABLE        = WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE;
  WINHTTP_CALLBACK_FLAG_DATA_AVAILABLE           = WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE;
  WINHTTP_CALLBACK_FLAG_READ_COMPLETE            = WINHTTP_CALLBACK_STATUS_READ_COMPLETE;
  WINHTTP_CALLBACK_FLAG_WRITE_COMPLETE           = WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE;
  WINHTTP_CALLBACK_FLAG_REQUEST_ERROR            = WINHTTP_CALLBACK_STATUS_REQUEST_ERROR;

  WINHTTP_CALLBACK_FLAG_ALL_COMPLETIONS          = (WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE
                                                        or WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE
                                                        or WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE
                                                        or WINHTTP_CALLBACK_STATUS_READ_COMPLETE
                                                        or WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE
                                                        or WINHTTP_CALLBACK_STATUS_REQUEST_ERROR);
  WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS         = $ffffffff;

//
// if the following value is returned by WinHttpSetStatusCallback, then
// probably an invalid (non-code) address was supplied for the callback
//

  WINHTTP_INVALID_STATUS_CALLBACK        = (Pointer(-1));

//
// WinHttpQueryHeaders info levels. Generally, there is one info level
// for each potential RFC822/HTTP/MIME header that an HTTP server
// may send as part of a request response.
//
// The WINHTTP_QUERY_RAW_HEADERS info level is provided for clients
// that choose to perform their own header parsing.
//

const
  WINHTTP_QUERY_MIME_VERSION                = 0;
  WINHTTP_QUERY_CONTENT_TYPE                = 1;
  WINHTTP_QUERY_CONTENT_TRANSFER_ENCODING   = 2;
  WINHTTP_QUERY_CONTENT_ID                  = 3;
  WINHTTP_QUERY_CONTENT_DESCRIPTION         = 4;
  WINHTTP_QUERY_CONTENT_LENGTH              = 5;
  WINHTTP_QUERY_CONTENT_LANGUAGE            = 6;
  WINHTTP_QUERY_ALLOW                       = 7;
  WINHTTP_QUERY_PUBLIC                      = 8;
  WINHTTP_QUERY_DATE                        = 9;
  WINHTTP_QUERY_EXPIRES                     = 10;
  WINHTTP_QUERY_LAST_MODIFIED               = 11;
  WINHTTP_QUERY_MESSAGE_ID                  = 12;
  WINHTTP_QUERY_URI                         = 13;
  WINHTTP_QUERY_DERIVED_FROM                = 14;
  WINHTTP_QUERY_COST                        = 15;
  WINHTTP_QUERY_LINK                        = 16;
  WINHTTP_QUERY_PRAGMA                      = 17;
  WINHTTP_QUERY_VERSION                     = 18;  // special: part of status line
  WINHTTP_QUERY_STATUS_CODE                 = 19;  // special: part of status line
  WINHTTP_QUERY_STATUS_TEXT                 = 20;  // special: part of status line
  WINHTTP_QUERY_RAW_HEADERS                 = 21;  // special: all headers as ASCIIZ
  WINHTTP_QUERY_RAW_HEADERS_CRLF            = 22;  // special: all headers
  WINHTTP_QUERY_CONNECTION                  = 23;
  WINHTTP_QUERY_ACCEPT                      = 24;
  WINHTTP_QUERY_ACCEPT_CHARSET              = 25;
  WINHTTP_QUERY_ACCEPT_ENCODING             = 26;
  WINHTTP_QUERY_ACCEPT_LANGUAGE             = 27;
  WINHTTP_QUERY_AUTHORIZATION               = 28;
  WINHTTP_QUERY_CONTENT_ENCODING            = 29;
  WINHTTP_QUERY_FORWARDED                   = 30;
  WINHTTP_QUERY_FROM                        = 31;
  WINHTTP_QUERY_IF_MODIFIED_SINCE           = 32;
  WINHTTP_QUERY_LOCATION                    = 33;
  WINHTTP_QUERY_ORIG_URI                    = 34;
  WINHTTP_QUERY_REFERER                     = 35;
  WINHTTP_QUERY_RETRY_AFTER                 = 36;
  WINHTTP_QUERY_SERVER                      = 37;
  WINHTTP_QUERY_TITLE                       = 38;
  WINHTTP_QUERY_USER_AGENT                  = 39;
  WINHTTP_QUERY_WWW_AUTHENTICATE            = 40;
  WINHTTP_QUERY_PROXY_AUTHENTICATE          = 41;
  WINHTTP_QUERY_ACCEPT_RANGES               = 42;
  WINHTTP_QUERY_SET_COOKIE                  = 43;
  WINHTTP_QUERY_COOKIE                      = 44;
  WINHTTP_QUERY_REQUEST_METHOD              = 45;  // special: GET/POST etc.
  WINHTTP_QUERY_REFRESH                     = 46;
  WINHTTP_QUERY_CONTENT_DISPOSITION         = 47;

//
// HTTP 1.1 defined headers
//

  WINHTTP_QUERY_AGE                         = 48;
  WINHTTP_QUERY_CACHE_CONTROL               = 49;
  WINHTTP_QUERY_CONTENT_BASE                = 50;
  WINHTTP_QUERY_CONTENT_LOCATION            = 51;
  WINHTTP_QUERY_CONTENT_MD5                 = 52;
  WINHTTP_QUERY_CONTENT_RANGE               = 53;
  WINHTTP_QUERY_ETAG                        = 54;
  WINHTTP_QUERY_HOST                        = 55;
  WINHTTP_QUERY_IF_MATCH                    = 56;
  WINHTTP_QUERY_IF_NONE_MATCH               = 57;
  WINHTTP_QUERY_IF_RANGE                    = 58;
  WINHTTP_QUERY_IF_UNMODIFIED_SINCE         = 59;
  WINHTTP_QUERY_MAX_FORWARDS                = 60;
  WINHTTP_QUERY_PROXY_AUTHORIZATION         = 61;
  WINHTTP_QUERY_RANGE                       = 62;
  WINHTTP_QUERY_TRANSFER_ENCODING           = 63;
  WINHTTP_QUERY_UPGRADE                     = 64;
  WINHTTP_QUERY_VARY                        = 65;
  WINHTTP_QUERY_VIA                         = 66;
  WINHTTP_QUERY_WARNING                     = 67;
  WINHTTP_QUERY_EXPECT                      = 68;
  WINHTTP_QUERY_PROXY_CONNECTION            = 69;
  WINHTTP_QUERY_UNLESS_MODIFIED_SINCE       = 70;

  WINHTTP_QUERY_PROXY_SUPPORT               = 75;
  WINHTTP_QUERY_AUTHENTICATION_INFO         = 76;
  WINHTTP_QUERY_PASSPORT_URLS               = 77;
  WINHTTP_QUERY_PASSPORT_CONFIG             = 78;

  WINHTTP_QUERY_MAX                         = 78;

//
// WINHTTP_QUERY_CUSTOM - if this special value is supplied as the dwInfoLevel
// parameter of WinHttpQueryHeaders() then the lpBuffer parameter contains the name
// of the header we are to query
//

  WINHTTP_QUERY_CUSTOM                       = 65535;

//
// WINHTTP_QUERY_FLAG_REQUEST_HEADERS - if this bit is set in the dwInfoLevel
// parameter of WinHttpQueryHeaders() then the request headers will be queried for the
// request information
//

  WINHTTP_QUERY_FLAG_REQUEST_HEADERS         = $80000000;

//
// WINHTTP_QUERY_FLAG_SYSTEMTIME - if this bit is set in the dwInfoLevel parameter
// of WinHttpQueryHeaders() AND the header being queried contains date information,
// e.g. the "Expires:" header then lpBuffer will contain a SYSTEMTIME structure
// containing the date and time information converted from the header String
//

  WINHTTP_QUERY_FLAG_SYSTEMTIME              = $40000000;

//
// WINHTTP_QUERY_FLAG_NUMBER - if this bit is set in the dwInfoLevel parameter of
// HttpQueryHeader(), then the value of the header will be converted to a number
// before being returned to the caller, if applicable
//

  WINHTTP_QUERY_FLAG_NUMBER                  = $20000000;

//
// HTTP Response Status Codes:
//

  HTTP_STATUS_CONTINUE           = 100; // OK to continue with request
  HTTP_STATUS_SWITCH_PROTOCOLS   = 101; // server has switched protocols in upgrade header

  HTTP_STATUS_OK                 = 200; // request completed
  HTTP_STATUS_CREATED            = 201; // object created, reason = new URI
  HTTP_STATUS_ACCEPTED           = 202; // async completion (TBS)
  HTTP_STATUS_PARTIAL            = 203; // partial completion
  HTTP_STATUS_NO_CONTENT         = 204; // no info to return
  HTTP_STATUS_RESET_CONTENT      = 205; // request completed, but clear form
  HTTP_STATUS_PARTIAL_CONTENT    = 206; // partial GET fulfilled
  HTTP_STATUS_WEBDAV_MULTI_STATUS= 207; // WebDAV Multi-Status

  HTTP_STATUS_AMBIGUOUS          = 300; // server couldn't decide what to return
  HTTP_STATUS_MOVED              = 301; // object permanently moved
  HTTP_STATUS_REDIRECT           = 302; // object temporarily moved
  HTTP_STATUS_REDIRECT_METHOD    = 303; // redirection w/ new access method
  HTTP_STATUS_NOT_MODIFIED       = 304; // if-modified-since was not modified
  HTTP_STATUS_USE_PROXY          = 305; // redirection to proxy, location header specifies proxy to use
  HTTP_STATUS_REDIRECT_KEEP_VERB = 307; // HTTP/1.1: keep same verb

  HTTP_STATUS_BAD_REQUEST        = 400; // invalid syntax
  HTTP_STATUS_DENIED             = 401; // access denied
  HTTP_STATUS_PAYMENT_REQ        = 402; // payment required
  HTTP_STATUS_FORBIDDEN          = 403; // request forbidden
  HTTP_STATUS_NOT_FOUND          = 404; // object not found
  HTTP_STATUS_BAD_METHOD         = 405; // method is not allowed
  HTTP_STATUS_NONE_ACCEPTABLE    = 406; // no response acceptable to client found
  HTTP_STATUS_PROXY_AUTH_REQ     = 407; // proxy authentication required
  HTTP_STATUS_REQUEST_TIMEOUT    = 408; // server timed out waiting for request
  HTTP_STATUS_CONFLICT           = 409; // user should resubmit with more info
  HTTP_STATUS_GONE               = 410; // the resource is no longer available
  HTTP_STATUS_LENGTH_REQUIRED    = 411; // the server refused to accept request w/o a length
  HTTP_STATUS_PRECOND_FAILED     = 412; // precondition given in request failed
  HTTP_STATUS_REQUEST_TOO_LARGE  = 413; // request entity was too large
  HTTP_STATUS_URI_TOO_LONG       = 414; // request URI too long
  HTTP_STATUS_UNSUPPORTED_MEDIA  = 415; // unsupported media type
  HTTP_STATUS_RETRY_WITH         = 449; // retry after doing the appropriate action.

  HTTP_STATUS_SERVER_ERROR       = 500; // internal server error
  HTTP_STATUS_NOT_SUPPORTED      = 501; // required not supported
  HTTP_STATUS_BAD_GATEWAY        = 502; // error response received from gateway
  HTTP_STATUS_SERVICE_UNAVAIL    = 503; // temporarily overloaded
  HTTP_STATUS_GATEWAY_TIMEOUT    = 504; // timed out waiting for gateway
  HTTP_STATUS_VERSION_NOT_SUP    = 505; // HTTP version not supported

  HTTP_STATUS_FIRST              = HTTP_STATUS_CONTINUE;
  HTTP_STATUS_LAST               = HTTP_STATUS_VERSION_NOT_SUP;

  WINHTTP_NO_REFERER             = nil;
  WINHTTP_DEFAULT_ACCEPT_TYPES   = nil;

  WINHTTP_ADDREQ_INDEX_MASK      = $0000FFFF;
  WINHTTP_ADDREQ_FLAGS_MASK      = $FFFF0000;

  WINHTTP_ADDREQ_FLAG_ADD_IF_NEW = $10000000;

  WINHTTP_ADDREQ_FLAG_ADD        = $20000000;

  WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA       = $40000000;
  WINHTTP_ADDREQ_FLAG_COALESCE_WITH_SEMICOLON   = $01000000;
  WINHTTP_ADDREQ_FLAG_COALESCE                  = WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA;

  WINHTTP_ADDREQ_FLAG_REPLACE    = $80000000;

  WINHTTP_NO_ADDITIONAL_HEADERS   = nil;
  WINHTTP_NO_REQUEST_DATA         = nil;

  WINHTTP_HEADER_NAME_BY_INDEX          = nil;
  WINHTTP_NO_OUTPUT_BUFFER              = nil;
  WINHTTP_NO_HEADER_INDEX               = nil;

Type
  PWINHTTP_CURRENT_USER_IE_PROXY_CONFIG = ^WINHTTP_CURRENT_USER_IE_PROXY_CONFIG;
  WINHTTP_CURRENT_USER_IE_PROXY_CONFIG = packed record
    fAutoDetect:BOOL;
    lpszAutoConfigUrl:RtcPtrWideChar;
    lpszProxy:RtcPtrWideChar;
    lpszProxyBypass:RtcPtrWideChar;
  End;

Const
  WINHTTP_ERROR_BASE                     = 12000;

  ERROR_WINHTTP_OUT_OF_HANDLES          = (WINHTTP_ERROR_BASE + 1);
  ERROR_WINHTTP_TIMEOUT                 = (WINHTTP_ERROR_BASE + 2);
  ERROR_WINHTTP_INTERNAL_ERROR          = (WINHTTP_ERROR_BASE + 4);
  ERROR_WINHTTP_INVALID_URL             = (WINHTTP_ERROR_BASE + 5);
  ERROR_WINHTTP_UNRECOGNIZED_SCHEME     = (WINHTTP_ERROR_BASE + 6);
  ERROR_WINHTTP_NAME_NOT_RESOLVED       = (WINHTTP_ERROR_BASE + 7);
  ERROR_WINHTTP_INVALID_OPTION          = (WINHTTP_ERROR_BASE + 9);
  ERROR_WINHTTP_OPTION_NOT_SETTABLE     = (WINHTTP_ERROR_BASE + 11);
  ERROR_WINHTTP_SHUTDOWN                = (WINHTTP_ERROR_BASE + 12);

  ERROR_WINHTTP_LOGIN_FAILURE           = (WINHTTP_ERROR_BASE + 15);
  ERROR_WINHTTP_OPERATION_CANCELLED     = (WINHTTP_ERROR_BASE + 17);
  ERROR_WINHTTP_INCORRECT_HANDLE_TYPE   = (WINHTTP_ERROR_BASE + 18);
  ERROR_WINHTTP_INCORRECT_HANDLE_STATE  = (WINHTTP_ERROR_BASE + 19);
  ERROR_WINHTTP_CANNOT_CONNECT          = (WINHTTP_ERROR_BASE + 29);
  ERROR_WINHTTP_CONNECTION_ERROR        = (WINHTTP_ERROR_BASE + 30);
  ERROR_WINHTTP_RESEND_REQUEST          = (WINHTTP_ERROR_BASE + 32);

  ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED = (WINHTTP_ERROR_BASE + 44);

//
// WinHttpRequest Component errors
//
  ERROR_WINHTTP_CANNOT_CALL_BEFORE_OPEN = (WINHTTP_ERROR_BASE + 100);
  ERROR_WINHTTP_CANNOT_CALL_BEFORE_SEND = (WINHTTP_ERROR_BASE + 101);
  ERROR_WINHTTP_CANNOT_CALL_AFTER_SEND  = (WINHTTP_ERROR_BASE + 102);
  ERROR_WINHTTP_CANNOT_CALL_AFTER_OPEN  = (WINHTTP_ERROR_BASE + 103);

  ERROR_WINHTTP_HEADER_NOT_FOUND            = (WINHTTP_ERROR_BASE + 150);
  ERROR_WINHTTP_INVALID_SERVER_RESPONSE     = (WINHTTP_ERROR_BASE + 152);
  ERROR_WINHTTP_INVALID_QUERY_REQUEST       = (WINHTTP_ERROR_BASE + 154);
  ERROR_WINHTTP_HEADER_ALREADY_EXISTS       = (WINHTTP_ERROR_BASE + 155);
  ERROR_WINHTTP_REDIRECT_FAILED             = (WINHTTP_ERROR_BASE + 156);

  ERROR_WINHTTP_AUTO_PROXY_SERVICE_ERROR  = (WINHTTP_ERROR_BASE + 178);
  ERROR_WINHTTP_BAD_AUTO_PROXY_SCRIPT     = (WINHTTP_ERROR_BASE + 166);
  ERROR_WINHTTP_UNABLE_TO_DOWNLOAD_SCRIPT = (WINHTTP_ERROR_BASE + 167);

  ERROR_WINHTTP_NOT_INITIALIZED         = (WINHTTP_ERROR_BASE + 172);
  ERROR_WINHTTP_SECURE_FAILURE          = (WINHTTP_ERROR_BASE + 175);

  ERROR_WINHTTP_SECURE_CERT_DATE_INVALID   = (WINHTTP_ERROR_BASE + 37);
  ERROR_WINHTTP_SECURE_CERT_CN_INVALID     = (WINHTTP_ERROR_BASE + 38);
  ERROR_WINHTTP_SECURE_INVALID_CA          = (WINHTTP_ERROR_BASE + 45);
  ERROR_WINHTTP_SECURE_CERT_REV_FAILED     = (WINHTTP_ERROR_BASE + 57);
  ERROR_WINHTTP_SECURE_CHANNEL_ERROR       = (WINHTTP_ERROR_BASE + 157);
  ERROR_WINHTTP_SECURE_INVALID_CERT        = (WINHTTP_ERROR_BASE + 169);
  ERROR_WINHTTP_SECURE_CERT_REVOKED        = (WINHTTP_ERROR_BASE + 170);
  ERROR_WINHTTP_SECURE_CERT_WRONG_USAGE    = (WINHTTP_ERROR_BASE + 179);

  ERROR_WINHTTP_AUTODETECTION_FAILED                 = (WINHTTP_ERROR_BASE + 180);
  ERROR_WINHTTP_HEADER_COUNT_EXCEEDED                = (WINHTTP_ERROR_BASE + 181);
  ERROR_WINHTTP_HEADER_SIZE_OVERFLOW                 = (WINHTTP_ERROR_BASE + 182);
  ERROR_WINHTTP_CHUNKED_ENCODING_HEADER_SIZE_OVERFLOW= (WINHTTP_ERROR_BASE + 183);
  ERROR_WINHTTP_RESPONSE_DRAIN_OVERFLOW              = (WINHTTP_ERROR_BASE + 184);

  WINHTTP_ERROR_LAST                                 = (WINHTTP_ERROR_BASE + 184);

const
  CERT_STORE_CLOSE_FORCE_FLAG = 1;

  X509_ASN_ENCODING:DWORD = 1;
  PKCS_7_ASN_ENCODING:DWORD = 65536;

  CERT_FIND_SUBJECT_STR_A = 458759;
  CERT_FIND_SUBJECT_STR_W = 524295;
  CERT_FIND_ISSUER_STR_A = 458756;
  CERT_FIND_ISSUER_STR_W = 524292;

  INTERNET_OPTION_CLIENT_CERT_CONTEXT = 84;

type
//Function WinHttpQueryDataAvailable(hRequest:HINTERNET;var lpdwNumberOfBytesAviable:DWORD):BOOL; stdcall external 'winhttp.dll' name 'WinHttpQueryDataAvailable';
//Function WinHttpAddRequestHeaders(hRequest:HINTERNET;pwszHeaders:RtcPtrWideChar;dwHeaderLength:DWORD;dwModifiers:DWORD):BOOL; stdcall external 'winhttp.dll' name 'WinHttpAddRequestHeaders';

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

  {InternetQueryOption}
  TInternetQueryOption = Function(hInet: HINTERNET; dwOption: DWORD;
                                  lpBuffer: Pointer;
                                  var lpdwBufferLength: DWORD): BOOL; stdcall;

  {InternetSetOption}
  TInternetSetOption = Function(hInet: HINTERNET; dwOption: DWORD;
                                lpBuffer: Pointer;
                                dwBufferLength: DWORD): BOOL; stdcall;

  {InternetOpen}
  TInternetOpen = Function(pwszUserAgent:RtcPtrWideChar;
                           dwAccessType:DWORD;
                           pwszProxyName:RtcPtrWideChar;
                           pwszProxyBypass:RtcPtrWideChar;
                           dwFlags:DWORD):HINTERNET;stdcall;

  {InternetConnect}
  TInternetConnect = Function(hSession:HINTERNET;
                              pswzServerName:RtcPtrWideChar;
                              nServerPort:WORD;
                              dwReserved:DWORD):HINTERNET; stdcall;

  {InternetCloseHandle}
  TInternetCloseHandle = Function(hInternet:HINTERNET):BOOL; stdcall;

  {HttpOpenRequest}
  THttpOpenRequest = Function(hConnect:HINTERNET;
                             pwszVerb:RtcPtrWideChar;
                             pwszObjectName:RtcPtrWideChar;
                             pwszVersion:RtcPtrWideChar;
                             pwszReferrer:RtcPtrWideChar;
                             ppwszAccessTypes:RtcPtrWideChar;
                             dwFlags:DWORD):HINTERNET; stdcall;

  {HttpSendRequest}
  THttpSendRequest = Function(hRequest:HINTERNET;
                              pwszHeaders:RtcPtrWideChar;
                              dwHeadersLength:DWORD;
                              lpOptional:Pointer;
                              dwOptionalLength:DWORD;
                              dwTotalLength:DWORD;
                              dwContext:PDWORD):BOOL; stdcall;

  {HttpEndRequest}
  THttpEndRequest = function(hRequest:HINTERNET;
                             lpReserved:Pointer):BOOL; stdcall;

  {InternetReadFile}
  TInternetReadFile = Function(hRequest:HINTERNET;
                               lpBuffer:Pointer;
                               dwNumberOfBytesToRead:DWORD;
                               var lpdwNumberOfBytesRead:DWORD):BOOL; stdcall;

  {InternetWriteFile}
  TInternetWriteFile = Function(hRequest:HINTERNET;
                                lpBuffer:Pointer;
                                dwNumberOfBytesToWrite:DWORD;
                                var lpdwNumberOfBytesWritten:DWORD):BOOL; stdcall;

  {HttpQueryInfo}
  THttpQueryInfo = Function(hRequest:HINTERNET;
                            dwInfoLevel:DWORD;
                            pwszName:RtcPtrWideChar;
                            lpBuffer:Pointer;
                            var lpdwBufferLength:DWORD;
                            var lpdwIndex:DWORD):BOOL; stdcall;

  THttpSetTimeouts = Function(hRequest:HINTERNET;
                              dwResolveTimeout:DWORD;
                              dwConnectTimeout:DWORD;
                              dwSendTimeout:DWORD;
                              dwReceiveTimeout:DWORD):BOOL; stdcall;

  THttpSetStatusCallback = Function(hRequest:HINTERNET;
                                    lpfnInternetCallback:PWINHTTP_STATUS_CALLBACK;
                                    dwNotificationFlags:DWORD;
                                    dwReserved:LPDWORD):WINHTTP_STATUS_CALLBACK; stdcall;

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
  HttpEndRequest: THttpEndRequest;
  InternetReadFile: TInternetReadFile;
  InternetWriteFile: TInternetWriteFile;
  HttpQueryInfo: THttpQueryInfo;
  InternetSetOption: TInternetSetOption;
  InternetQueryOption: TInternetQueryOption;
  SetTimeouts: THttpSetTimeouts;
  SetStatusCallback: THttpSetStatusCallback;

procedure LoadWinHttp;
procedure LoadWinCrypt;

function Have_WinHTTP:boolean;

{$ENDIF} // {$IFDEF WINDOWS}

implementation

{$IFDEF WINDOWS}

const
  winhttpdll = 'winhttp.dll';
  wincrypt = 'crypt32.dll';

var  
  LibCS:TRtcCritSec;
  
  TriedLoading:boolean = False;
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

function WinHttpGetProc(const ProcName : RtcString) : Pointer;
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
      raise RtcWinHttpException.Create('Procedure ' + String(ProcName) +
                                      ' not found in ' + winhttpdll +
                                      ' Error #' + IntToStr(GetLastError));
    end;
  end;

procedure LoadWinHttp;
  var
    xWinHttpDll:RtcByteArray;
  begin
  xWinHttpDll:=nil;
  LibCS.Acquire;
  try
    if FDllHandle2 = 0 then
      begin
      if not TriedLoading then
        begin
        TriedLoading:=True;
        xWinHttpDll:=RtcStringToBytesZero(winhttpdll);
        FDllHandle2 := LoadLibraryA( @(xWinHttpDll[0]) );
        if FDllHandle2 <> 0 then
          begin
          try
            InternetOpen := TInternetOpen(WinHttpGetProc('WinHttpOpen'));
            InternetConnect := TInternetConnect(WinHttpGetProc('WinHttpConnect'));
            InternetCloseHandle := TInternetCloseHandle(WinHttpGetProc('WinHttpCloseHandle'));
            HttpOpenRequest := THttpOpenRequest(WinHttpGetProc('WinHttpOpenRequest'));
            HttpSendRequest := THttpSendRequest(WinHttpGetProc('WinHttpSendRequest'));
            HttpEndRequest := THttpEndRequest(WinHttpGetProc('WinHttpReceiveResponse'));
            InternetReadFile := TInternetReadFile(WinHttpGetProc('WinHttpReadData'));
            InternetWriteFile := TInternetWriteFile(WinHttpGetProc('WinHttpWriteData'));
            HttpQueryInfo := THttpQueryInfo(WinHttpGetProc('WinHttpQueryHeaders'));
            SetTimeouts := THttpSetTimeouts(WinHttpGetProc('WinHttpSetTimeouts'));
            SetStatusCallback:= THttpSetStatusCallback(WinHttpGetProc('WinHttpSetStatusCallback'));
            InternetSetOption := TInternetSetOption(WinHttpGetProc('WinHttpSetOption'));
            InternetQueryOption := TInternetQueryOption(WinHttpGetProc('WinHttpQueryOption'));
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

procedure UnloadWinHttp;
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

function Have_WinHTTP:boolean;
  begin
  LoadWinHttp;
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
{$IFDEF RTC_DEBUG} Log('rtcWinHttp Finalizing ...','DEBUG');{$ENDIF}
UnloadWinHttp;
UnloadWinCrypt;
RtcFreeAndNil(LibCS);
{$IFDEF RTC_DEBUG} Log('rtcWinHttp Finalized.','DEBUG');{$ENDIF}

{$ENDIF} // {$IFDEF WINDOWS}
end.
