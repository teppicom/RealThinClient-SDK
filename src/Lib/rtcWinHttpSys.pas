{
  "Low-level Windows Http.sys Server API functions"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)

  This unit is ONLY for MS Windows.

  @exclude
}
unit rtcWinHttpSys;

{$INCLUDE rtcDefs.inc}

interface

{$IFDEF WINDOWS}

{$IFDEF WIN64}
  {$A8}
{$ELSE}
  {$A+}
{$ENDIF}

uses
  Windows,
  SysUtils,

  rtcTypes,
  rtcSystem,
  rtcLog,

  rtcWinSock;

{$MINENUMSIZE 4}

type
  HTTP_CONNECTION_ID = UINT64;
  HTTP_RAW_CONNECTION_ID = UINT64;
  HTTP_REQUEST_ID = UINT64;
  HTTP_URL_CONTEXT = UINT64;
  HTTP_SERVER_SESSION_ID = UINT64;
  HTTP_URL_GROUP_ID = UINT64;

  HTTPAPI_VERSION = packed record
    HttpApiMajorVersion:Word;
    HttpApiMinorVersion:Word;
    end;

  HTTP_VERSION = packed record
    MajorVersion:Word;
    MinorVersion:Word;
    end;

  HTTP_VERB = (
    HttpVerbUnparsed,
    HttpVerbUnknown,
    HttpVerbInvalid,
    HttpVerbOPTIONS,
    HttpVerbGET,
    HttpVerbHEAD,
    HttpVerbPOST,
    HttpVerbPUT,
    HttpVerbDELETE,
    HttpVerbTRACE,
    HttpVerbCONNECT,
    HttpVerbTRACK,
    HttpVerbMOVE,
    HttpVerbCOPY,
    HttpVerbPROPFIND,
    HttpVerbPROPPATCH,
    HttpVerbMKCOL,
    HttpVerbLOCK,
    HttpVerbUNLOCK,
    HttpVerbSEARCH);

  HTTP_COOKED_URL = record
    FullUrlLength:Word;
    HostLength:Word;
    AbsPathLength:Word;
    QueryStringLength:Word;
    pFullUrl:PWideChar;
    pHost:PWideChar;
    pAbsPath:PWideChar;
    pQueryString:PWideChar;
    end;

  HTTP_TRANSPORT_ADDRESS = record
    pRemoteAddress:PSOCKADDR;
    pLocalAddress:PSOCKADDR;
    end;

  HTTP_UNKNOWN_HEADER = record
    NameLength:Word;
    RawValueLength:Word;
    pName:PAnsiChar;
    pRawValue:PAnsiChar;
    end;
  PHTTP_UNKNOWN_HEADER = ^HTTP_UNKNOWN_HEADER;

  HTTP_KNOWN_HEADER = record
    RawValueLength:Word;
    pRawValue:PAnsiChar;
    end;

  HTTP_REQ_HEADER_ID = (
    HttpReqHeaderCacheControl,
    HttpReqHeaderConnection,
    HttpReqHeaderDate,
    HttpReqHeaderKeepAlive,
    HttpReqHeaderPragma,
    HttpReqHeaderTrailer,
    HttpReqHeaderTransferEncoding,
    HttpReqHeaderUpgrade,
    HttpReqHeaderVia,
    HttpReqHeaderWarning,
    HttpReqHeaderAllow,
    HttpReqHeaderContentLength,
    HttpReqHeaderContentType,
    HttpReqHeaderContentEncoding,
    HttpReqHeaderContentLanguage,
    HttpReqHeaderContentLocation,
    HttpReqHeaderContentMd5,
    HttpReqHeaderContentRange,
    HttpReqHeaderExpires,
    HttpReqHeaderLastModified,
    HttpReqHeaderAccept,
    HttpReqHeaderAcceptCharset,
    HttpReqHeaderAcceptEncoding,
    HttpReqHeaderAcceptLanguage,
    HttpReqHeaderAuthorization,
    HttpReqHeaderCookie,
    HttpReqHeaderExpect,
    HttpReqHeaderFrom,
    HttpReqHeaderHost,
    HttpReqHeaderIfMatch,
    HttpReqHeaderIfModifiedSince,
    HttpReqHeaderIfNoneMatch,
    HttpReqHeaderIfRange,
    HttpReqHeaderIfUnmodifiedSince,
    HttpReqHeaderMaxForwards,
    HttpReqHeaderProxyAuthorization,
    HttpReqHeaderReferer,
    HttpReqHeaderRange,
    HttpReqHeaderTe,
    HttpReqHeaderTranslate,
    HttpReqHeaderUserAgent);

  HTTP_REQUEST_HEADERS = record
    UnknownHeaderCount:Word;
    pUnknownHeaders:PHTTP_UNKNOWN_HEADER;
    TrailerCount:Word;
    pTrailers:PHTTP_UNKNOWN_HEADER;
    KnownHeaders:array[low(HTTP_REQ_HEADER_ID)..high(HTTP_REQ_HEADER_ID)] of HTTP_KNOWN_HEADER;
    end;

  HTTP_DATA_CHUNK_TYPE = (
    HttpDataChunkFromMemory,
    HttpDataChunkFromFileHandle,
    HttpDataChunkFromFragmentCache,
    HttpDataChunkFromFragmentCacheEx);

  HTTP_BYTE_RANGE = record
    StartingOffset:ULARGE_INTEGER;
    Length:ULARGE_INTEGER;
    end;

  HTTP_DATA_CHUNK_MEMORY = record
    DataChunkType:HTTP_DATA_CHUNK_TYPE; // => HttpDataChunkFromMemory
    _padding:Cardinal; // <- padding required for correct allignment
    pBuffer:Pointer;
    BufferLength:Cardinal
    end;
  HTTP_DATA_CHUNK_FILE = record
    DataChunkType:HTTP_DATA_CHUNK_TYPE; // => HttpDataChunkFromFileHandle
    ByteRange:HTTP_BYTE_RANGE;
    FileHandle:THandle
    end;
  PHTTP_DATA_CHUNK=^HTTP_DATA_CHUNK_MEMORY;

  HTTP_SSL_CLIENT_CERT_INFO = record
    CertFlags:Cardinal;
    CertEncodedSize:Cardinal;
    pCertEncoded:PUCHAR;
    Token:THandle;
    CertDeniedByMapper:BOOLEAN;
    end;
  PHTTP_SSL_CLIENT_CERT_INFO = ^HTTP_SSL_CLIENT_CERT_INFO;

  HTTP_SSL_INFO = record
    ServerCertKeySize:Word;
    ConnectionKeySize:Word;
    ServerCertIssuerSize:Cardinal;
    ServerCertSubjectSize:Cardinal;
    pServerCertIssuer:PAnsiChar;
    pServerCertSubject:PAnsiChar;
    pClientCertInfo:PHTTP_SSL_CLIENT_CERT_INFO;
    SslClientCertNegotiated:Cardinal;
    end;
  PHTTP_SSL_INFO = ^HTTP_SSL_INFO;

  HTTP_REQUEST_INFO_TYPE = (
    HttpRequestInfoTypeAuth);

  HTTP_REQUEST_INFO = record
    InfoType:HTTP_REQUEST_INFO_TYPE;
    InfoLength:Cardinal;
    pInfo:Pointer;
    end;
  PHTTP_REQUEST_INFO = ^HTTP_REQUEST_INFO;

  HTTP_REQUEST = record
    Flags:Cardinal;
    ConnectionId:HTTP_CONNECTION_ID;
    RequestId:HTTP_REQUEST_ID;
    UrlContext:HTTP_URL_CONTEXT;
    Version:HTTP_VERSION;
    Verb:HTTP_VERB;
    UnknownVerbLength:Word;
    RawUrlLength:Word;
    pUnknownVerb:PAnsiChar;
    pRawUrl:PAnsiChar;
    CookedUrl:HTTP_COOKED_URL;
    Address:HTTP_TRANSPORT_ADDRESS;
    Headers:HTTP_REQUEST_HEADERS;
    BytesReceived:UINT64;
    EntityChunkCount:Word;
    pEntityChunks:PHTTP_DATA_CHUNK;
    RawConnectionId:HTTP_RAW_CONNECTION_ID;
    pSslInfo:PHTTP_SSL_INFO;
    end;
  PHTTP_REQUEST = ^HTTP_REQUEST;

  HTTP_RESP_HEADER_ID = (
    HttpRespHeaderCacheControl,
    HttpRespHeaderConnection,
    HttpRespHeaderDate,
    HttpRespHeaderKeepAlive,
    HttpRespHeaderPragma,
    HttpRespHeaderTrailer,
    HttpRespHeaderTransferEncoding,
    HttpRespHeaderUpgrade,
    HttpRespHeaderVia,
    HttpRespHeaderWarning,
    HttpRespHeaderAllow,
    HttpRespHeaderContentLength,
    HttpRespHeaderContentType,
    HttpRespHeaderContentEncoding,
    HttpRespHeaderContentLanguage,
    HttpRespHeaderContentLocation,
    HttpRespHeaderContentMd5,
    HttpRespHeaderContentRange,
    HttpRespHeaderExpires,
    HttpRespHeaderLastModified,
    HttpRespHeaderAcceptRanges,
    HttpRespHeaderAge,
    HttpRespHeaderEtag,
    HttpRespHeaderLocation,
    HttpRespHeaderProxyAuthenticate,
    HttpRespHeaderRetryAfter,
    HttpRespHeaderServer,
    HttpRespHeaderSetCookie,
    HttpRespHeaderVary,
    HttpRespHeaderWwwAuthenticate);

  HTTP_RESPONSE_HEADERS = record
    UnknownHeaderCount:Word;
    pUnknownHeaders:PHTTP_UNKNOWN_HEADER;
    TrailerCount:Word;
    pTrailers:PHTTP_UNKNOWN_HEADER;
    KnownHeaders:array[low(HTTP_RESP_HEADER_ID)..high(HTTP_RESP_HEADER_ID)] of HTTP_KNOWN_HEADER;
    end;

  HTTP_RESPONSE_INFO_TYPE = (
    HttpResponseInfoTypeMultipleKnownHeaders,
    HttpResponseInfoTypeAuthenticationProperty,
    HttpResponseInfoTypeQosProperty,
    HttpResponseInfoTypeChannelBind);

  HTTP_RESPONSE_INFO = record
    Typ:HTTP_RESPONSE_INFO_TYPE;
    Length:Cardinal;
    pInfo:Pointer;
    end;
  PHTTP_RESPONSE_INFO = ^HTTP_RESPONSE_INFO;

  HTTP_RESPONSE = record
    Flags:Cardinal;
    Version:HTTP_VERSION;
    StatusCode:Word;
    ReasonLength:Word;
    pReason:PAnsiChar;
    Headers:HTTP_RESPONSE_HEADERS;
    EntityChunkCount:Word;
    pEntityChunks:PHTTP_DATA_CHUNK;
    end;
  PHTTP_RESPONSE = ^HTTP_RESPONSE;

  HTTP_LOG_DATA_TYPE = (
    HttpLogDataTypeFields);

  HTTP_LOG_DATA = record
    Typ:HTTP_LOG_DATA_TYPE;
    end;
  PHTTP_LOG_DATA = ^HTTP_LOG_DATA;

  HTTP_SERVICE_CONFIG_ID = (
    HttpServiceConfigIPListenList,
    HttpServiceConfigSSLCertInfo,
    HttpServiceConfigUrlAclInfo,
    HttpServiceConfigTimeout,
    HttpServiceConfigCache,
    HttpServiceConfigSslSniCertInfo,
    HttpServiceConfigSslCcsCertInfo,
    HttpServiceConfigMax);

  HTTP_SERVER_PROPERTY = (
    HttpServerAuthenticationProperty,
    HttpServerLoggingProperty,
    HttpServerQosProperty,
    HttpServerTimeoutsProperty,
    HttpServerQueueLengthProperty,
    HttpServerStateProperty,
    HttpServer503VerbosityProperty,
    HttpServerBindingProperty,
    HttpServerExtendedAuthenticationProperty,
    HttpServerListenEndpointProperty,
    HttpServerChannelBindProperty);

  HTTP_CACHE_POLICY_TYPE = (
    HttpCachePolicyNocache,
    HttpCachePolicyUserInvalidates,
    HttpCachePolicyTimeToLive,
    HttpCachePolicyMaximum);

  HTTP_CACHE_POLICY = record
    Policy:HTTP_CACHE_POLICY_TYPE;
    SecondsToLive:Cardinal;
    end;
  PHTTP_CACHE_POLICY = ^HTTP_CACHE_POLICY;

  HTTP_SERVICE_CONFIG_URLACL_KEY = record
    pUrlPrefix:PWideChar;
    end;

  HTTP_SERVICE_CONFIG_URLACL_PARAM = record
    pStringSecurityDescriptor:PWideChar;
    end;

  HTTP_SERVICE_CONFIG_URLACL_SET = record
    KeyDesc:HTTP_SERVICE_CONFIG_URLACL_KEY;
    ParamDesc:HTTP_SERVICE_CONFIG_URLACL_PARAM;
    end;

const
  HTTP_INITIALIZE_SERVER = 1;
  HTTP_INITIALIZE_CONFIG = 2;

  HTTP_SEND_RESPONSE_FLAG_DISCONNECT     = $1;
  HTTP_SEND_RESPONSE_FLAG_MORE_DATA      = $2;
  HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA    = $4;
  HTTP_SEND_RESPONSE_FLAG_ENABLE_NAGLING = $8;
  HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES = $20;

  // Only HTTP API 1.0 functions are loaded and used
  HTTPAPI_VERSION_1:HTTPAPI_VERSION=(HttpApiMajorVersion:1; HttpApiMinorVersion:0);

  HTTP_KNOWN_REQ_HEADER: array[low(HTTP_REQ_HEADER_ID)..high(HTTP_REQ_HEADER_ID)] of RtcString = (
    'Cache-Control',
    'Connection',
    'Date',
    'Keep-Alive',
    'Pragma',
    'Trailer',
    'Transfer-Encoding',
    'Upgrade',
    'Via',
    'Warning',
    'Allow',
    'Content-Length',
    'Content-Type',
    'Content-Encoding',
    'Content-Language',
    'Content-Location',
    'Content-Md5',
    'Content-Range',
    'Expires',
    'Last-Modified',
    'Accept',
    'Accept-Charset',
    'Accept-Encoding',
    'Accept-Language',
    'Authorization',
    'Cookie',
    'Expect',
    'From',
    'Host',
    'If-Match',
    'If-Modified-Since',
    'If-None-Match',
    'If-Range',
    'If-Unmodified-Since',
    'Max-Forwards',
    'Proxy-Authorization',
    'Referer',
    'Range',
    'Te',
    'Translate',
    'User-Agent');

  HTTP_KNOWN_RESP_HEADER: array[low(HTTP_RESP_HEADER_ID)..high(HTTP_RESP_HEADER_ID)] of RtcString = (
    'Cache-Control',
    'Connection',
    'Date',
    'Keep-Alive',
    'Pragma',
    'Trailer',
    'Transfer-Encoding',
    'Upgrade',
    'Via',
    'Warning',
    'Allow',
    'Content-Length',
    'Content-Type',
    'Content-Encoding',
    'Content-Language',
    'Content-Location',
    'Content-Md5',
    'Content-Range',
    'Expires',
    'Last-Modified',
    'Accept-Ranges',
    'Age',
    'Etag',
    'Location',
    'Proxy-Authenticate',
    'Retry-After',
    'Server',
    'Set-Cookie',
    'Vary',
    'Www-Authenticate');

    HTTP_KNOWN_VERB: array[low(HTTP_VERB)..high(HTTP_VERB)] of RtcString = (
    '*Unparsed*',
    '*Unknown*',
    '*Invalid*',
    'OPTIONS',
    'GET',
    'HEAD',
    'POST',
    'PUT',
    'DELETE',
    'TRACE',
    'CONNECT',
    'TRACK',
    'MOVE',
    'COPY',
    'PROPFIND',
    'PROPPATCH',
    'MKCOL',
    'LOCK',
    'UNLOCK',
    'SEARCH');

type
  // Initializes the HTTP Server API for use by the calling process.
  THttpInitialize = function
    ( Version:HTTPAPI_VERSION;
      Flags:Cardinal;
      pReserved:Pointer=NIL):Cardinal; stdcall;

  // Directs the HTTP Server API to clean up any resources associated with a particular process.
  THttpTerminate = function
    ( Flags:Cardinal;
      pReserved:Pointer=NIL):Cardinal; stdcall;

  // Creates an HTTP request queue and returns a handle to it.
  THttpCreateHttpHandle = function
    ( var pReqQueueHandle:THandle;
      Reserved:Cardinal=0):Cardinal; stdcall;

  // Deletes specified information from the HTTP configuration store.
  THttpDeleteServiceConfiguration = function
    ( ServiceHandle:THandle;
      ConfigId:HTTP_SERVICE_CONFIG_ID;
      pConfigInformation:Pointer;
      ConfigInformationLength:Cardinal;
      pOverlapped:Pointer=NIL):Cardinal; stdcall;

  // Sets specified values in the HTTP Server API configuration store.
  THttpSetServiceConfiguration = function
    ( ServiceHandle:THandle;
      ConfigId:HTTP_SERVICE_CONFIG_ID;
      pConfigInformation:Pointer;
      ConfigInformationLength:Cardinal;
      pOverlapped:Pointer=NIL):Cardinal; stdcall;

  // Retrieves an HTTP request from a specified request queue.
  THttpReceiveHttpRequest = function
    ( ReqQueueHandle:THandle;
      RequestId:HTTP_REQUEST_ID;
      Flags:Cardinal;
      pRequestBuffer:PHTTP_REQUEST;
      RequestBufferLength:Cardinal;
      var pBytesReceived:Cardinal;
      pOverlapped:Pointer=NIL):Cardinal; stdcall;

  // Retrieves entity-body data of a particular HTTP request.
  THttpReceiveRequestEntityBody = function
    ( ReqQueueHandle:THandle;
      RequestId:HTTP_REQUEST_ID;
      Flags:Cardinal;
      pBuffer:Pointer;
      BufferLength:Cardinal;
      var pBytesReceived:Cardinal;
      pOverlapped:Pointer=NIL):Cardinal; stdcall;

  // Sends an HTTP response for a particular HTTP request.
  THttpSendHttpResponse = function
    ( ReqQueueHandle:THandle;
      RequestId:HTTP_REQUEST_ID;
      Flags:Cardinal;
      pHttpResponse:PHTTP_RESPONSE;
      pCachePolicy:PHTTP_CACHE_POLICY;
      var pBytesSent:Cardinal;
      pReserved2:Pointer=NIL;
      Reserved3:Cardinal=0;
      pOverlapped:Pointer=NIL;
      pLogData:PHTTP_LOG_DATA=NIL):Cardinal; stdcall;

  // Sends entity-body data of an HTTP response.
  THttpSendResponseEntityBody = function
    ( ReqQueueHandle:THandle;
      RequestId:HTTP_REQUEST_ID;
      Flags:Cardinal;
      EntityChunkCount:Word;
      pEntityChunks:PHTTP_DATA_CHUNK;
      var pBytesSent:Cardinal;
      pReserved1:Pointer=NIL;
      Reserved2:Cardinal=0;
      pOverlapped:Pointer=NIL;
      pLogData:PHTTP_LOG_DATA=NIL):Cardinal; stdcall;

  // Retrieves the client certificate for an SSL connection.
  THttpReceiveClientCertificate = function
    ( ReqQueueHandle:THandle;
      ConnectionId:HTTP_CONNECTION_ID;
      Flags:Cardinal;
      pSslClientCertInfo:PHTTP_SSL_CLIENT_CERT_INFO;
      SslClientCertInfoSize:Cardinal;
      var pBytesReceived:Cardinal;
      pOverlapped:Pointer=NIL):Cardinal; stdcall;

  // Registers a URL so that HTTP requests for it are routed to a specified request queue.
  THttpAddUrl = function
    ( ReqQueueHandle:THandle;
      pFullyQualifiedUrl:PWideChar;
      pReserved:Pointer=NIL):Cardinal; stdcall;

  // Unregisters a specified URL, so that requests for it are no longer routed to a specified queue.
  THttpRemoveUrl = function
    ( ReqQueueHandle:THandle;
      pFullyQualifiedUrl:PWideChar):Cardinal; stdcall;

var
  _HttpInitialize:THttpInitialize;
  _HttpTerminate:THttpTerminate;
  _HttpCreateHttpHandle:THttpCreateHttpHandle;
  _HttpDeleteServiceConfiguration:THttpDeleteServiceConfiguration;
  _HttpSetServiceConfiguration:THttpSetServiceConfiguration;
  _HttpReceiveHttpRequest:THttpReceiveHttpRequest;
  _HttpReceiveRequestEntityBody:THttpReceiveRequestEntityBody;
  _HttpSendHttpResponse:THttpSendHttpResponse;
  _HttpSendResponseEntityBody:THttpSendResponseEntityBody;
  _HttpReceiveClientCertificate:THttpReceiveClientCertificate;
  _HttpAddUrl:THttpAddUrl;
  _HttpRemoveUrl:THttpRemoveUrl;

procedure LoadWinHttpSys;

type
  WHTTP_Request = record
    Flags:Cardinal;
    ReqQueueHandle:THandle;
    RequestID:HTTP_REQUEST_ID;
    ConnectionID:HTTP_CONNECTION_ID;
    RawConnectionID:HTTP_RAW_CONNECTION_ID;

    // Request Method (GET,POST,PUT,OPTIONS,...)
    Method:RtcString;
    // URI part of the Request
    URI:RtcString;
    // HTTP version
    Ver:HTTP_VERSION;
    // Raw Request Header Size in Bytes
    HeaderSize:UINT64;
    // Is the Connection SSL-Encrypted?
    SslEncrypted:boolean;

    // all Request Headers as "Name: Value" pairs, separated by <CR><LF> (#13#10)
    HeaderText:RtcString;
    // Remote Peer IP Address
    RemoteAddr:RtcString;
    // Local IP Address
    LocalAddr:RtcString;
    end;

{ Open a new HTTP Server Request Queue.
  If successful, returns 0 and sets the new
  Request Queue Handle in the "ReqQueueHandle" variable (parameter).
  In case of an error, returns the Windows Error Code as a result. }
function WHttp_OpenQueue(var ReqQueueHandle:THandle):Cardinal;

{ Close a Request Queue Handle, previously open with "WHttp_OpenQueue".
  Returns TRUE if success, FALSE if call failed.
  Sets the "ReqQueueHandle" variable to zero (0),
  regardless of the Result returned. }
function WHttp_CloseQueue(var ReqQueueHandle:THandle):boolean;

{ Clear the Access Level information from the specified URL.
  This reverts the URL to default Access Level set by Microsoft,
  which ONLY allows Administrators to start Servers for an URL.
  Check the description for "WHttp_SetUrlAcl" function for details.
  Returns 0 if successful, or a Windows Error Code in case of an Error.  }
function WHttp_ClearUrlACL(const URL:RtcWideString):Cardinal;

{ Set Access Level for the the URL "URL" to the "SD_String" specified.
  Returns 0 if successful, or a Windows Error Code in case of an Error.

  NOTE: This function returns an Error if you try to set a new Access Level 
  for an URL which already has an Access Level assigned. If you just want
  to update the Access Level if it was already set, call "WHttp_ClearUrlAcl"
  for that "URL" before calling the "WHttp_SetUrlAcl" function to set a new 
  Access Level for the same URL. 

  By default, only Windows Administators have the right to start Servers using
  the HTTP Server API (see "AddUrl" method), but this method allows a process
  logged in with Administrator priviliges to change the Access Level for an URL.

  A valid Windows Security Descriptor String is required for this method to work.  

  For Example, these two function calls ...

    WHttp_ClearUrlACL('http://*:80/'); 
    WHttp_SetUrlACL('http://*:80/','D:(A;;GA;;;S-1-1-0)');
    
  ... clear any previously set Access Level for the URL "http://*:80/", then
  sets the Access Level to ALLOW (A) GENERAL ACCESS (GA) to ALL (S-1-1-0) ...
  
  "D:dacl_flags(ace_type;ace_flags;rights;object_guid;inherit_object_guid;account_sid)"
  'D:          (    A   ;         ;  GA  ;           ;                   ;  S-1-1-0  )'
 
  If the Server you are using is also used by others, I would recommend you to read 
  what these Access Levels actually mean and find the option that best suits your needs. 
  You will find details about Windows Security Descriptors in MSDN articles linked below.

  * Windows Security Descriptor String Format:
      https://msdn.microsoft.com/en-us/library/windows/desktop/aa379570(v=vs.85).aspx
  * ACE Strings:
      https://msdn.microsoft.com/en-us/library/windows/desktop/aa374928(v=vs.85).aspx
  * SID Strings:
      https://msdn.microsoft.com/en-us/library/windows/desktop/aa379602(v=vs.85).aspx
  * Well-known SIDs:
      https://msdn.microsoft.com/en-us/library/windows/desktop/aa379649(v=vs.85).aspx }
function WHttp_SetUrlACL(const URL, SD_String:RtcWideString):Cardinal;

{ Create a Request Queue (if not already created) for the specified URL and
  Start Server (if not already started) on the Port specified in the URL.
  URL examples:
    'http://*:80/' = all HTTP Requests for all Hosts on Port 80
    'https://*:443/' = all HTTPS Requests for all Hosts on Port 443
    'http://www.mydomain.com:80/myURI' = Requests on Port 80 for Host 'www.mydomain.com' and URI "/myURI' }
function WHttp_AddUrl(var ReqQueueHandle:THandle; const URL:RtcWideString):Cardinal;

{ "WHttp_RemoveUrl" is the reverse of "WHttp_AddUrl" and can ONLY be used
  on URLs previously used with "WHttp_AddUrl" on this "ReqQueueHandle".
  Removes the URL from the Request Queue, and ... Stops the Server Listener
  on the Port specified in the URL if the last URL for that Port was removed.

  NOTE: Closing a Request Queue also removes all URLs previously added to
  that Request Queue, so there is no need to do manually remove URLS before
  closing the Request Queue by calling "RemoveUrl", but ... using the
  "WHttp_RemoveUrl" function allows "hot removal" of URLs, without closing
  the "Request Queue" and therefor without stopping the Server listener. }
function WHttp_RemoveUrl(const ReqQueueHandle:THandle; const URL:RtcWideString):Cardinal;


{ Call with "Retry=False" (default) to wait for the Next Request in the "ReqQueueHandle",
  or ... with "Retry=True" to try receiving Request Headers using a "RequestID"
  received after the last call to "WHttp_NextRequest", which returned 0 as the Result.
  This blocks the current Thread until a Request was received, or the Queue is closed.

  "RawRequestHeaders" array SHOULD BE initialized before calling this function
  to have enough space for raw Request Headers (usually between 1KB and 64KB).
  If the function is called with an "RawRequestHeaders" of size zero (0),
  it will be initialized to 17KB in size. And if the function is called with
  an array that is NOT empty, but is smaller than the absolute minimum required
  by the API, it will be initialized by this function to that absolute minimum.

  If there is an Error receiving the Request, this function sets "RequestID:=0"
  and then Returns with a negative Windows Error Code (<0) as the Result.

  If a Request is received with Headers which do NOT fit into the "RawRequestHeaders"
  array, this function returns with zero (0) as the Result, "RequestID" set (<>0)
  and "RawRequestHeaders" containing the first bytes of raw Request Headers.
  After that, you can either ...
  (A) Call the "WHttp_SendHeader" function to send a Response to the Client
      with a Status Code indicating the Error and/or close the connection, or ...
  (B) Increase the size of the "RawRequesttHeaders" array to "BytesReceived" and
      call "WHttp_NextRequest" with "Retry=TRUE" and the "RequestID" returned, or ...
  (C) Call "WHttp_NextRequest" again with "Retry=FALSE" (default) to start
      waiting for the next Request, leaving the last Client "hanging".

  If a Request is received with Headers which FIT into the "RawRequestHeaders"
  array, this function returns the number of Bytes received (>0), after which
  "RawRequestHeaders" can be used to access all Request Headers and the "RequestID"
  value can be used with "WHttp_ReadContent", "WHttp_SendHeader" and "WHttp_SendContent"
  functions to access the Request Content Body and send a Response to the Client. }
function WHttp_NextRequest(const ReqQueueHandle:THandle;
                           var RawRequestHeaders:RtcByteArray;
                           var RequestID:HTTP_REQUEST_ID;
                           var BytesReceived:Cardinal;
                           Retry:Boolean=False;
                           CustomFlags:Cardinal=0):Integer; overload;

{ Call with "Retry=False" (default) to wait for the Next Request in the "ReqQueueHandle",
  or ... with "Retry=True" to try receiving Request Headers using a "RequestID"
  received after the last call to "WHttp_NextRequest", which returned 0 as the Result.
  This blocks the current Thread until a Request was received, or the Queue is closed.

  "RawRequestHeaders" array SHOULD BE initialized before calling this function
  to have enough space for raw Request Headers (usually between 1KB and 64KB).
  If the function is called with an "RawRequestHeaders" of size zero (0),
  it will be initialized to 17KB in size. And if the function is called with
  an array that is NOT empty, but is smaller than the absolute minimum required
  by the API, it will be initialized by this function to that absolute minimum.

  If there is an Error receiving the Request, this function sets "RequestID:=0"
  and then Returns with a negative Windows Error Code (<0) as the Result.

  If a Request is received with Headers which do NOT fit into the "RawRequestHeaders"
  array, this function returns with zero (0) as the Result, "RequestID" set (<>0)
  and "RawRequestHeaders" containing the first bytes of raw Request Headers.
  After that, you can either ...
  (A) Call the "WHttp_SendHeader" function to send a Response to the Client
      with a Status Code indicating the Error and close the connection, or ...
  (B) Increase the size of the "RawRequesttHeaders" array and try calling
      "WHttp_NextRequest" using the "RequestID" returned and "Retry=TRUE", or ...
  (C) Call "WHttp_NextRequest" again with "Retry=FALSE" (default) to start
      waiting for the next Request, leaving the last Client "hanging".

  If a Request is received with Headers which FIT into the "RawRequestHeaders"
  array, this function returns the number of Bytes received (>0), after which
  "RawRequestHeaders" can be used to access all Request Headers and the "RequestID"
  value can be used with "WHttp_ReadContent", "WHttp_SendHeader" and "WHttp_SendContent"
  functions to access the Request Content Body and send a Response to the Client. }
function WHttp_NextRequest(const ReqQueueHandle:THandle;
                           var RawRequestHeaders:RtcByteArray;
                           var RequestID:HTTP_REQUEST_ID;
                           Retry:Boolean=False;
                           CustomFlags:Cardinal=0):Integer; overload;

{ Wait for the Next Request in the Request Queue "ReqQueueHandle". This blocks the
  current Thread until a "valid" Request is received, or the Request Queue is closed.

  "RawRequestHeaders" array will be initialized by this function and updated if
  needed, to hold raw Headers for Requests of up to "MaxHeaderSize" bytes in size.
  If the "MaxheaderSize" parameter is NOT set (=0), this function will allocate
  space for Request Headers with up to "SOCK_READ_BUFFER_SIZE" bytes in size.

  If a Request is received with Headers exceeding the "MaxHeaderSize" limit
  (or exceeding "SOCK_READ_BUFFER_SIZE" if "MaxHeaderSize=0"), this function will
  automatically send a Response with a Status Code "BadRequestCode" (default=400)
  and Status Text "BadRequestText" (default=''), then close the Connection
  used to receive that Request and start waiting for the next Request.

  Returns the number of Bytes received (>0) if a Request with (valid) Request
  Headers has been received, after which "RawRequestHeaders" can be used to
  access all Request Headers and "RequestID" can be used with "WHttp_ReadContent",
  "WHttp_SendHeader" and "WHttp_SendContent" functions (see descriptions below).

  Returns a negative Windows Error Code (<0) in case of an Error. }
function WHttp_NextRequest(const ReqQueueHandle:THandle;
                           var RequestID:HTTP_REQUEST_ID;
                           var RawRequestHeaders:RtcByteArray;
                           BadStatusCode:Integer=400;
                           BadStatusText:RtcString='';
                           MaxHeaderSize:Cardinal=0;
                           CustomFlags:Cardinal=0):Integer; overload;

{ Calls the "WHttp_NextRequest" function using a temporary "RequestID" and
  a temporary "RawRequestHeaders" variable, then calls "WHttp_UpdateRecord"
  to update the "Record" before returning (temporary variables are freed). }
function WHttp_NextRequest(const ReqQueueHandle:THandle;
                           var Request:WHttp_Request;
                           BadStatusCode:Integer=400;
                           BadStatusText:RtcString='';
                           MaxHeaderSize:Cardinal=0;
                           CustomFlags:Cardinal=0):Integer; overload;

{ Receive Request Content data from "ReqQueueHandle" for "RequestID" and write it into
  the "ReceiveBuffer" array. If the function succeeds, it will return the number of bytes
  received (>0) as a Result and data is written into the array (starting at ReceivingBuffer[0]).
  If all content has already been read (nothing more to read), the function returns 0.
  And in case of an Error, this function returns a negative Windows Error Code (<0).

  "ReceiveBuffer" SHOULD BE initialized before calling this function, to have enough
  space (length) to hold the maximum number of bytes you want to receive with this call.
  If the function is called with "Length(ReceiveBuffer)=0" or "ResizeReceiveBuffer=TRUE",
  "ReceiveBuffer" will be initialized with "SetLength(ReceiveBufer,SOCK_MAX_READ_SIZE)". }
function WHttp_ReadContent(const ReqQueueHandle:THandle;
                           const RequestID:HTTP_REQUEST_ID;
                           var ReceiveBuffer:RtcByteArray;
                           ResizeReceiveBuffer:boolean=True;
                           CustomFlags:Cardinal=0):Integer; overload;

{ Calls  WHttp_ReadContent(const Request.ReqQueueHandle:THandle;     // <- Request
                           const Request.RequestID:HTTP_REQUEST_ID;  // <- Request
                           var ReceiveBuffer:RtcByteArray;
                           ResizeReceiveBuffer:boolean=True):Integer; }
function WHttp_ReadContent(const Request:WHttp_Request;
                           var ReceiveBuffer:RtcByteArray;
                           ResizeReceiveBuffer:boolean=True;
                           CustomFlags:Cardinal=0):Integer; overload;

{ Send a Response to the Request Queue "ReqQueueHandle" for the "RequestID".
  This function should be called EXACTLY ONCE for ever Request!

  Call this function with "ResponseFinished=TRUE" if there is NO Content Body!
  Call with "ForceDisconnect=TRUE" if the connection shoud be closed after sending the Header.

  "StatusCode" and "StatusText" will be used as Response Status Code and Text, ONLY
  if "ResponseHeaderText" does NOT begin with "HTTP/1.x <code> reason" in the 1st line.

  Returns the numbef of Bytes sent, or -1 if there was an Error sending the Response
  (in which case you should NOT try to send any more content for the "RequestID"). }
function WHttp_SendHeader(const ReqQueueHandle:THandle;
                          const RequestID:HTTP_REQUEST_ID;
                          const ResponseHeaderText:RtcString='';
                          StatusCode:Integer=200;
                          const StatusText:RtcString='';
                          ResponseFinished:boolean=False;
                          ForceDisconnect:boolean=False;
                          CustomFlags:Cardinal=0):Integer; overload;

{ Calls  WHttp_SendHeader(const Request.ReqQueueHandle:THandle;     // <- Request
                          const Request.RequestID:HTTP_REQUEST_ID;  // <- Request
                          const ResponseHeaderText:RtcString='';
                          StatusCode:Integer=200;
                          const StatusText:RtcString='';
                          ResponseFinished:boolean=False;
                          ForceDisconnect:boolean=False;
                          CustomFlags:Cardinal=0):Integer; }
function WHttp_SendHeader(const Request:WHttp_Request;
                          const ResponseHeaderText:RtcString='';
                          StatusCode:Integer=200;
                          const StatusText:RtcString='';
                          ResponseFinished:boolean=False;
                          ForceDisconnect:boolean=False;
                          CustomFlags:Cardinal=0):Integer; overload;

{ Send "ResponseContentBody:RtcByteArray" as the next bytes of the Response Content Body for "Request".

  Should be called as many times as necessary to send the entire Content Body,
  but may ONLY be called if the previous call to "WHttp_SendHeader" and/or
  "WHttp_SendContent" was made with "ResponseFinished=FALSE" (default)!

  Should be called with "ResponseFinished=TRUE" if there is no more content
  for this "Request/Response" cycle (the entire Response Content Body was sent).

  If "ForceDisconnect=TRUE", the connection will be closed after this call.

  Returns the number of Bytes sent, or -1 if there was an Error sending the Content
  (in which case you should NOT try to send any more content for the "RequestID"). }
function WHttp_SendContent(const ReqQueueHandle:THandle;
                           const RequestID:HTTP_REQUEST_ID;
                           const ResponseContentBody:RtcByteArray;
                           ResponseFinished:boolean=False;
                           ForceDisconnect:boolean=False;
                           CustomFlags:Cardinal=0):Integer; overload;

{ Calls  WHttp_SendContent(const Request.ReqQueueHandle:THandle;     // <- Request
                           const Request.RequestID:HTTP_REQUEST_ID;  // <- Request
                           const ResponseContentBody:RtcByteArray;
                           ResponseFinished:boolean=False;
                           ForceDisconnect:boolean=False;
                           CustomFlags:Cardinal=0):Integer; }
function WHttp_SendContent(const Request:WHttp_Request;
                           const ResponseContentBody:RtcByteArray;
                           ResponseFinished:boolean=False;
                           ForceDisconnect:boolean=False;
                           CustomFlags:Cardinal=0):Integer; overload;

{ Updates the "Request" record with all the Request Header information from "RawRequestHeaders".
  "Request" record will contain all the information that can be extracted from "RawRequestHeaders",
  making that information safe for use, regardless of what happens to the "RawRequestHeaders" array.
  This function does NOT modify the "RawRequestHeaders" array in any way, but it should ONLY
  be used with a "RawRequestHeaders" array received after a call to "WHttp_NextRequest".
  NOTE: Then "Request.QueueHandle" will be set to the "ReqQueueHandle" parameter! }
procedure WHttp_UpdateRequest(const ReqQueueHandle:THandle;
                              const RawRequestHeaders:RtcByteArray;
                              var Request:WHttp_Request;
                              Get_RemoteAddr:boolean=True;
                              Get_LocalAddr:boolean=True;
                              Get_HeaderText:boolean=True);

{ Returns a pointer to a "HTTP_REQUEST_INFO" structure in "RawRequestHeaders".
  Can be used for direct access to Request Information, as it was sent from the API.

  WARNING! The pointer returned from this function is ONLY valid while
  the "RawRequestHeaders" array is NOT modified and remains in scope!
  Use this pointer directly after calling the function and set it
  to NIL immediately after use, to avoid Access Violations when the
  "RawRequestHeaders" array is changed, gets out of scope or is auto-freed. }
function WHttp_RequestInfo(const RawRequestHeaders:RtcByteArray):PHTTP_REQUEST;

{ Send File "FileName" as the next bytes of the Response Content Body for "Request",
  starting from "StartOffset" and sending "BytesToSend" bytes (-1 = until End-of-File).

  May ONLY be called if the previous call to "WHttp_SendHeader" and/or
  "WHttp_SendContent" was made with "ResponseFinished=FALSE" (default)!

  Should be called with "ResponseFinished=TRUE" if there is no more content
  for this "Request/Response" cycle (the entire Response Content Body was sent).

  If "ForceDisconnect=TRUE", the connection will be closed after this call.

  Returns the number of Bytes sent, or -1 if there was an Error sending the Content
  (in which case you should NOT try to send any more content for the "RequestID"). }
function WHttp_SendFile(const ReqQueueHandle:THandle;
                        const RequestID:HTTP_REQUEST_ID;
                        const FileName:RtcWideString;
                        ResponseFinished:boolean=False;
                        StartOffset:int64=0;
                        BytesToSend:int64=-1; // EOF
                        ForceDisconnect:boolean=False;
                        CustomFlags:Cardinal=0):int64; overload;

{ Calls WHttp_SendFile(const Request.ReqQueueHandle:THandle;     // <- Request
                        const Request.RequestID:HTTP_REQUEST_ID; // <- Request
                        const FileName:RtcWideString;
                        ResponseFinished:boolean=False;
                        StartOffset:int64=0;
                        BytesToSend:int64=-1; // EOF
                        ForceDisconnect:boolean=False;
                        CustomFlags:Cardinal=0):int64; }
function WHttp_SendFile(const Request:WHTTP_Request;
                        const FileName:RtcWideString;
                        ResponseFinished:boolean=False;
                        StartOffset:int64=0;
                        BytesToSend:int64=-1; // EOF
                        ForceDisconnect:boolean=False;
                        CustomFlags:Cardinal=0):int64; overload;

{ Send the contents from a File using a Windows File Handle as "FileHandle" parameter,
  starting from "StartOffset" and sending "BytesToSend" bytes (-1 = until End-of-File).
  The file can be opened using any Windows function returning a File Handle and should
  be closed by the caller when it is no longer needed (it is NOT closed by this call!).

  May ONLY be called if the previous call to "WHttp_SendHeader" and/or
  "WHttp_SendContent" was made with "ResponseFinished=FALSE" (default)!

  Should be called with "ResponseFinished=TRUE" if there is no more content
  for this "Request/Response" cycle (the entire Response Content Body was sent).

  If "ForceDisconnect=TRUE", the connection will be closed after this call.

  Returns the number of Bytes sent, or -1 if there was an Error sending the Content
  (in which case you should NOT try to send any more content for the "RequestID"). }
function WHttp_SendFile(const ReqQueueHandle:THandle;
                        const RequestID:HTTP_REQUEST_ID;
                        const FileHandle:THandle;
                        ResponseFinished:boolean=False;
                        StartOffset:int64=0;
                        BytesToSend:int64=-1; // EOF
                        ForceDisconnect:boolean=False;
                        CustomFlags:Cardinal=0):int64; overload;

{ Calls WHttp_SendFile(const Request.ReqQueueHandle:THandle;     // <- Request
                        const Request.RequestID:HTTP_REQUEST_ID; // <- Request
                        const FileName:RtcWideString;
                        ResponseFinished:boolean=False;
                        StartOffset:int64=0;
                        BytesToSend:int64=-1; // EOF
                        ForceDisconnect:boolean=False;
                        CustomFlags:Cardinal=0):int64; }
function WHttp_SendFile(const Request:WHTTP_Request;
                        const FileHandle:THandle;
                        ResponseFinished:boolean=False;
                        StartOffset:int64=0;
                        BytesToSend:int64=-1; // EOF
                        ForceDisconnect:boolean=False;
                        CustomFlags:Cardinal=0):int64; overload;

{$ENDIF} // {$IFDEF WINDOWS}

implementation

{$IFDEF WINDOWS}

const
  httpapidll = 'httpapi.dll';

  CRLF:RtcString=#13#10;
  MIN_REQBUFFER_SIZE=SizeOf(HTTP_REQUEST);
  DEF_REQBUFFER_SIZE=SizeOf(HTTP_REQUEST)+16000;

var
  FDllHandleHttp : THandle  = 0;
  LibCS: TRtcCritSec;
  
function WHttp_OpenQueue(var ReqQueueHandle:THandle):Cardinal;
  begin
  LoadWinHttpSys;

  Result:=
    _HttpCreateHttpHandle(
      ReqQueueHandle
      );
  end;

function WHttp_CloseQueue(var ReqQueueHandle:THandle):boolean;
  begin
  Result:=
    Windows.CloseHandle(
      ReqQueueHandle
      );

  ReqQueueHandle:=0;
  end;

function WHttp_ClearUrlACL(const URL:RtcWideString):Cardinal;
  var
    UrlAcl:HTTP_SERVICE_CONFIG_URLACL_SET;
  begin
  ZeroMemory(@UrlAcl,SizeOf(UrlAcl));
  LoadWinHttpSys;
  UrlAcl.KeyDesc.pUrlPrefix:=PWideChar(URL);

  Result:=
    _HttpDeleteServiceConfiguration(
      0,
      HttpServiceConfigUrlAclInfo,
      @UrlAcl,
      SizeOf(UrlAcl)
      );
  end;

function WHttp_SetUrlACL(const URL, SD_String:RtcWideString):Cardinal;
  var
    UrlAcl:HTTP_SERVICE_CONFIG_URLACL_SET;
  begin
  ZeroMemory(@UrlAcl,SizeOf(UrlAcl));
  LoadWinHttpSys;

  UrlAcl.KeyDesc.pUrlPrefix:=PWideChar(URL);
  UrlAcl.ParamDesc.pStringSecurityDescriptor:=PWideChar(SD_String);

  Result:=
    _HttpSetServiceConfiguration(
      0,
      HttpServiceConfigUrlAclInfo,
      @UrlAcl,
      Sizeof(UrlAcl)
      );
  end;

function WHttp_AddUrl(var ReqQueueHandle:THandle; const URL:RtcWideString):Cardinal;
  begin
  LoadWinHttpSys;

  if ReqQueueHandle=0 then
    begin
    Result:=
      _HttpCreateHttpHandle(
        ReqQueueHandle
        );
    if Result<>0 then Exit;
    end;

  Result:=
    _HttpAddUrl(
      ReqQueueHandle,
      PWideChar(URL)
      );
  end;

function WHttp_RemoveUrl(const ReqQueueHandle:THandle; const URL:RtcWideString):Cardinal;
  begin
  LoadWinHttpSys;

  if ReqQueueHandle=0 then
    Result:=ERROR_INVALID_HANDLE
  else
    Result:=
      _HttpRemoveUrl(
        ReqQueueHandle,
        PWideChar(URL)
        );
  end;

function PtrOK(const RawRequestData:RtcByteArray; var vPtr; ISize:RtcIntPtr):boolean;
  var
    IStart,IPtr,ILen:RtcIntPtr;
  begin
  ILen:=Length(RawRequestData);
  if ILen<ISize then
    Result:=False
  else
    begin
    IPtr:=RtcIntPtr(@vPtr);
    IStart:=RtcIntPtr(@RawRequestData[0]);
    if IPtr<IStart then
      Result:=False
    else if IPtr+ISize>IStart+ILen then
      Result:=False
    else
      Result:=True;
    end;
  end;

function WHttp_NextRequest(const ReqQueueHandle:THandle;
                           var RawRequestHeaders:RtcByteArray;
                           var RequestID:HTTP_REQUEST_ID;
                           var BytesReceived:Cardinal;
                           Retry:Boolean=False;
                           CustomFlags:Cardinal=0):Integer; overload;
  var
    BuffLen,
    LastError:Cardinal;
    FRequest:PHTTP_REQUEST;
  begin
  Result:=-ERROR_INVALID_HANDLE;
  if ReqQueueHandle=0 then
    begin
    BytesReceived:=0;
    RequestID:=0;
    Exit;
    end;

  if length(RawRequestHeaders)=0 then
    SetLength(RawRequestHeaders,DEF_REQBUFFER_SIZE)
  else if length(RawRequestHeaders)<MIN_REQBUFFER_SIZE then
    SetLength(RawRequestHeaders,MIN_REQBUFFER_SIZE);

  FRequest:=PHTTP_REQUEST(@RawRequestHeaders[0]);
  BuffLen:=Length(RawRequestHeaders);

  if not Retry then RequestID:=0;

  LastError:=
    _HttpReceiveHttpRequest(
      ReqQueueHandle,
      RequestID,
      CustomFlags,
      FRequest,
      BuffLen,
      BytesReceived
      );
  if LastError=NO_ERROR then
    begin
    RequestID:=FRequest^.RequestId;
    Result:=FRequest^.BytesReceived;
    end
  else if LastError=ERROR_MORE_DATA then
    begin
    RequestID:=FRequest^.RequestId;
    Result:=0;
    end
  else
    begin
    BytesReceived:=0;
    RequestID:=0;
    Result:=-LastError;
    end;
  end;

function WHttp_NextRequest(const ReqQueueHandle:THandle;
                           var RawRequestHeaders:RtcByteArray;
                           var RequestID:HTTP_REQUEST_ID;
                           Retry:Boolean=False;
                           CustomFlags:Cardinal=0):Integer; overload;
  var
    BytesReceived:Cardinal;
  begin
  BytesReceived:=0;

  Result:=
    WHttp_NextRequest(
      ReqQueueHandle,
      RawRequestHeaders,
      RequestID,
      BytesReceived,
      Retry,
      CustomFlags);
  end;

function WHttp_NextRequest(const ReqQueueHandle:THandle;
                           var RequestID:HTTP_REQUEST_ID;
                           var RawRequestHeaders:RtcByteArray;
                           BadStatusCode:Integer=400;
                           BadStatusText:RtcString='';
                           MaxHeaderSize:Cardinal=0;
                           CustomFlags:Cardinal=0):Integer;
  var
    BytesReceived,
    BuffLen,
    LastError:Cardinal;
    FRequest:PHTTP_REQUEST;
  begin
  Result:=-ERROR_INVALID_HANDLE;
  if ReqQueueHandle=0 then
    begin
    RequestID:=0;
    SetLength(RawRequestHeaders,0);
    Exit;
    end;

  if length(RawRequestHeaders)=0 then
    SetLength(RawRequestHeaders,DEF_REQBUFFER_SIZE)
  else if length(RawRequestHeaders)<MIN_REQBUFFER_SIZE then
    SetLength(RawRequestHeaders,MIN_REQBUFFER_SIZE);

  if MaxHeaderSize=0 then
    MaxHeaderSize:=SOCK_READ_BUFFER_SIZE;

  FRequest:=PHTTP_REQUEST(@RawRequestHeaders[0]);
  BuffLen:=Length(RawRequestHeaders);
  repeat
    RequestID:=0;

    LastError:=
      _HttpReceiveHttpRequest(
        ReqQueueHandle,
        RequestID,
        CustomFlags,
        FRequest,
        BuffLen,
        BytesReceived
        );

    if (LastError=ERROR_MORE_DATA) and (BytesReceived>BuffLen) then
      if (MaxHeaderSize=0) or (BytesReceived<=MaxHeaderSize) then
        begin
        RequestID:=FRequest^.RequestId;

        BuffLen:=BytesReceived;
        SetLength(RawRequestHeaders,BuffLen);
        FRequest:=PHTTP_REQUEST(@RawRequestHeaders[0]);

        LastError:=
          _HttpReceiveHttpRequest(
            ReqQueueHandle,
            RequestID,
            CustomFlags,
            FRequest,
            BuffLen,
            BytesReceived
            );
        end;

    if LastError=NO_ERROR then
      begin
      RequestID:=FRequest^.RequestId;
      Result:=FRequest^.BytesReceived;
      end
    else if LastError=ERROR_MORE_DATA then
      begin
      RequestID:=FRequest^.RequestId;
      WHttp_SendHeader(ReqQueueHandle,
                       RequestID,
                       '',
                       BadStatusCode,
                       BadStatusText,
                       True, True);
      end
    else if (LastError<>ERROR_CONNECTION_UNAVAIL) and
            (LastError<>ERROR_CONNECTION_INVALID) and
            (LastError<>ERROR_CONNECTION_ABORTED) then
      begin
      RequestID:=0;
      SetLength(RawRequestHeaders,0);
      Result:=-LastError;
      Break;
      end;
    until LastError=NO_ERROR;
  end;

procedure WHttp_UpdateRequest(const ReqQueueHandle:THandle;
                              const RawRequestHeaders:RtcByteArray;
                              var Request:WHttp_Request;
                              Get_RemoteAddr:boolean=True;
                              Get_LocalAddr:boolean=True;
                              Get_HeaderText:boolean=True);
  var
    FRequest:PHTTP_REQUEST;

  procedure Update_URI;
    begin
    if (FRequest^.RawUrlLength>0) and (FRequest^.pRawUrl<>nil) then
      if PtrOK(RawRequestHeaders,FRequest^.pRawUrl^,FRequest^.RawUrlLength) then
        Request.URI:=RtcPBytesToString(FRequest^.pRawUrl^,FRequest^.RawUrlLength);
    end;

  procedure Update_Method;
    begin
    if (FRequest^.UnknownVerbLength>0) and (FRequest^.pUnknownVerb<>nil) then
      if PtrOK(RawRequestHeaders,FRequest^.pUnknownVerb^,FRequest^.UnknownVerbLength) then
        begin
        Request.Method:=RtcPBytesToString(FRequest^.pUnknownVerb^,FRequest^.UnknownVerbLength);
        Exit;
        end;
    if (FRequest^.Verb>=low(HTTP_VERB)) and (FRequest^.Verb<=high(HTTP_VERB)) then
      Request.Method:=HTTP_KNOWN_VERB[FRequest^.Verb];
    end;

  procedure Update_LocalAddress;
    begin
    if FRequest^.Address.pRemoteAddress<>nil then
      if PtrOK(RawRequestHeaders,FRequest^.Address.pRemoteAddress^,SizeOf(TSockAddr)) then
        begin
        LoadWinSock;
        Request.RemoteAddr:=WSocket_GetSinIP(FRequest^.Address.pRemoteAddress^);
        end;
    end;

  procedure Update_RemoteAddress;
    begin
    if FRequest^.Address.pRemoteAddress<>nil then
      if PtrOK(RawRequestHeaders,FRequest^.Address.pLocalAddress^,SizeOf(TSockAddr)) then
        begin
        LoadWinSock;
        Request.LocalAddr:=WSocket_GetSinIP(FRequest^.Address.pLocalAddress^);
        end;
    end;

  procedure Update_HeaderText;
    var
      res:TRtcHugeString;
      a:HTTP_REQ_HEADER_ID;
      i:integer;
      UH:PHTTP_UNKNOWN_HEADER;
      KH:^HTTP_KNOWN_HEADER;
    begin
    if PtrOK(RawRequestHeaders,FRequest^.Headers,SizeOf(HTTP_REQUEST_HEADERS)) then
      begin
      res:=TRtcHugeString.Create;
      try
        KH:=@FRequest^.Headers.KnownHeaders;
        for a:=low(HTTP_REQ_HEADER_ID) to high(HTTP_REQ_HEADER_ID) do
          begin
          if (KH^.RawValueLength>0) and (KH^.pRawValue<>nil) then
            if PtrOK(RawRequestHeaders,KH^.pRawValue^,KH^.RawValueLength) then
              res.Add(HTTP_KNOWN_REQ_HEADER[a]+': '+
                      RtcPBytesToString(KH^.pRawValue^,KH^.RawValueLength)+
                      CRLF);
          Inc(KH);
          end;

        UH:=FRequest^.Headers.pUnknownHeaders;
        if (UH<>nil) and (FRequest^.Headers.UnknownHeaderCount>0) then
          if PtrOK(RawRequestHeaders,UH^,SizeOf(HTTP_UNKNOWN_HEADER)*FRequest^.Headers.UnknownHeaderCount) then
            for i:=1 to FRequest^.Headers.UnknownHeaderCount do
              begin
              if (UH^.NameLength>0) and (UH^.pName<>nil) then
                if (UH^.RawValueLength>0) and (UH^.pRawValue<>nil) then
                  if PtrOK(RawRequestHeaders,UH^.pRawValue^,UH^.RawValueLength) then
                    if PtrOK(RawRequestHeaders,UH^.pName^,UH^.NameLength) then
                      res.Add(RtcPBytesToString(UH^.pName^,UH^.NameLength)+': '+
                              RtcPBytesToString(UH^.pRawValue^,UH^.RawValueLength)+
                              CRLF);
              Inc(UH);
              end;

        Request.HeaderText:=res.Get;
      finally
        RtcFreeAndNil(res);
        end;
      end;
    end;

  begin
  // Initialize the Record with no data ...
  Request.Flags:=0;
  Request.RequestID:=0;
  Request.ConnectionID:=0;
  Request.RawConnectionID:=0;
  Request.Ver.MajorVersion:=0;
  Request.Ver.MinorVersion:=0;
  Request.HeaderSize:=0;
  Request.SslEncrypted:=False;
  Request.ReqQueueHandle:=ReqQueueHandle;

  Request.Method:='';
  Request.URI:='';
  Request.HeaderText:='';
  Request.RemoteAddr:='';
  Request.LocalAddr:='';

  if length(RawRequestHeaders)<MIN_REQBUFFER_SIZE then Exit;
  FRequest:=PHTTP_REQUEST(@RawRequestHeaders[0]);

  Request.Flags:=FRequest^.Flags;
  Request.RequestID:=FRequest^.RequestID;
  Request.ConnectionID:=FRequest^.ConnectionId;
  Request.RawConnectionID:=FRequest^.RawConnectionId;
  Request.Ver:=FRequest^.Version;
  Request.HeaderSize:=FRequest^.BytesReceived;

  if FRequest^.pSslInfo=nil then
    Request.SslEncrypted:=False
  else if PtrOK(RawRequestHeaders,FRequest^.pSslInfo^,SizeOf(HTTP_SSL_INFO)) then
    Request.SslEncrypted:=True;

  Update_Method;
  Update_URI;

  if Get_HeaderText then Update_HeaderText;
  if Get_LocalAddr  then Update_LocalAddress;
  if Get_RemoteAddr then Update_RemoteAddress;
  end;

function WHttp_NextRequest(const ReqQueueHandle:THandle;
                           var Request:WHttp_Request;
                           BadStatusCode:Integer=400;
                           BadStatusText:RtcString='';
                           MaxHeaderSize:Cardinal=0;
                           CustomFlags:Cardinal=0):Integer; overload;
  var
    RequestID:HTTP_REQUEST_ID;
    RawRequestHeaders:RtcByteArray;
  begin
  Result:=
    WHttp_NextRequest(
      ReqQueueHandle,
      RequestID,
      RawRequestHeaders,
      BadStatusCode,
      BadStatusText,
      MaxHeaderSize,
      CustomFlags);

  WHttp_UpdateRequest(
    ReqQueueHandle,
    RawRequestHeaders,
    Request);
  end;

function WHttp_ReadContent(const ReqQueueHandle:THandle;
                           const RequestID:HTTP_REQUEST_ID;
                           var ReceiveBuffer:RtcByteArray;
                           ResizeReceiveBuffer:boolean=True;
                           CustomFlags:Cardinal=0):Integer; overload;
  var
    LastError,
    BytesReceived:Cardinal;
  begin
  Result:=-ERROR_INVALID_HANDLE;
  if (ReqQueueHandle=0) or (RequestID=0) then
    begin
    if ResizeReceiveBuffer then SetLength(ReceiveBuffer,0);
    Exit;
    end;

  if length(ReceiveBuffer)=0 then
    SetLength(ReceiveBuffer,SOCK_MAX_READ_SIZE)
  else if ResizeReceiveBuffer and (Length(ReceiveBuffer)<SOCK_MAX_READ_SIZE) then
    SetLength(ReceiveBuffer,SOCK_MAX_READ_SIZE);

  LastError:=
    _HttpReceiveRequestEntityBody(
      ReqQueueHandle,
      RequestID,
      CustomFlags,
      @ReceiveBuffer[0],
      Length(ReceiveBuffer),
      BytesReceived
      );
  if LastError=NO_ERROR then
    begin
    if ResizeReceiveBuffer then SetLength(ReceiveBuffer,BytesReceived);
    Result:=BytesReceived;
    end
  else
    begin
    if ResizeReceiveBuffer then SetLength(ReceiveBuffer,0);
    if LastError=ERROR_HANDLE_EOF then
      Result:=0
    else
      Result:=-LastError;
    end;
  end;

function WHttp_ReadContent(const Request:WHttp_Request;
                           var ReceiveBuffer:RtcByteArray;
                           ResizeReceiveBuffer:boolean=True;
                           CustomFlags:Cardinal=0):Integer; overload;
  begin
  Result:=
    WHttp_ReadContent(
      Request.ReqQueueHandle,
      Request.RequestID,
      ReceiveBuffer,
      ResizeReceiveBuffer,
      CustomFlags);
  end;

function TrimCopy(const S: RtcString; I, L:integer): RtcString;
  begin
  L:=L+I-1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    Result := ''
  else
    begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
    end;
  end;

function WHttp_SendHeader(const ReqQueueHandle:THandle;
                          const RequestID:HTTP_REQUEST_ID;
                          const ResponseHeaderText:RtcString='';
                          StatusCode:Integer=200;
                          const StatusText:RtcString='';
                          ResponseFinished:boolean=False;
                          ForceDisconnect:boolean=False;
                          CustomFlags:Cardinal=0):Integer; overload;
  var
    Flags,
    LastError,
    BytesSent:Cardinal;

    Response:HTTP_RESPONSE;
    UnknownHeaders:array of HTTP_UNKNOWN_HEADER;
    iLoc:Integer;
    iTemp:RtcByteArray;

  function MakeStr(const xStr:RtcString):Pointer;
    var
      len:Integer;
    begin
    len:=length(xStr);
    // Copy string "xStr" at the next location in "iTemp" ...
    RtcStringToPBytes(xStr,iTemp[iLoc],len);
    // Return a Pointer to the 1st character ...
    Result:=@iTemp[iLoc];
    // Move forward
    Inc(iLoc,len);
    end;

  procedure SetUnknownHeader(const xName,xValue:RtcString);
    var
      loc:integer;
    begin
    loc:=length(UnknownHeaders);
    SetLength(UnknownHeaders,loc+1);
    ZeroMemory(@UnknownHeaders[loc],SizeOf(HTTP_UNKNOWN_HEADER));
    with UnknownHeaders[loc] do
      begin
      NameLength:=Length(xName);
          pName:=MakeStr(xName);

      RawValueLength:=Length(xValue);
          pRawValue:=MakeStr(xValue);
      end;
    end;

  procedure SetPseudoHeader(const xName,xValue:RtcString);
    begin
    if Same_Text(xName,'status') then
      Response.StatusCode:=Str2Int64Def(xValue,Response.StatusCode)
    else
      SetUnknownHeader(xName,xValue);
    end;

  procedure SetHeader(const xName,xValue:RtcString);
    var
      a:HTTP_RESP_HEADER_ID;
      Found:boolean;
    begin
    Found:=False;
    for a:=low(HTTP_RESP_HEADER_ID) to high(HTTP_RESP_HEADER_ID) do
      if Same_Text(xName,HTTP_KNOWN_RESP_HEADER[a]) then
        begin
        Response.Headers.KnownHeaders[a].RawValueLength:=length(xValue);
        Response.Headers.KnownHeaders[a].pRawValue:=MakeStr(xValue);
        Found:=True;
        Break;
        end;
    if not Found then
      SetUnknownHeader(xName,xValue);
    end;

  procedure ParseHeader(const pValue:RtcString);
    var
      MyPos:integer;
      StatusLine,
      HeadStr,
      left:RtcString;
    begin
    if pValue='' then Exit;

    HeadStr:=pValue+CRLF;

    if length(HeadStr)>6 then
      begin
      if ((HeadStr[5]='/') and (Upper_Case(Copy(HeadStr,1,4))='HTTP')) or
         ((HeadStr[6]='/') and (Upper_Case(Copy(HeadStr,1,5))='HTTPS')) then
        begin
        MyPos:=Pos(CRLF,HeadStr);
        StatusLine:=Copy(HeadStr,1,MyPos-1);
        Delete(HeadStr,1,MyPos+2-1);
        Inc(iLoc,MyPos+2-1);

        { Our line probably looks like this:
          HTTP/1.1 200 OK }
        MyPos:=PosEx(' ',StatusLine); // first space before StatusCode
        if MyPos>0 then
          begin
          Delete(StatusLine,1,MyPos);

          MyPos:=PosEx(' ',StatusLine); // space after StatusCode
          if MyPos>0 then
            begin
            left:=Copy(StatusLine,1,MyPos-1); // StatusCode
            Delete(StatusLine,1,MyPos); // StatusText

            if left<>'' then
              begin
              Response.StatusCode:=Str2Int64Def(left,0);
              if StatusLine<>'' then
                begin
                Response.ReasonLength:=Length(StatusLine);
                Response.pReason:=MakeStr(StatusLine);
                end;
              end;
            end
          else
            Response.StatusCode:=Str2Int64Def(StatusLine,0);
          end;
        end;
      end;

    // Scan for all header attributes ...
    MyPos:=PosEx(#10, HeadStr);
    while (MyPos>1) do // at least 1 character inside line
      begin
      if HeadStr[MyPos-1]=#13 then
        StatusLine:=Copy(HeadStr,1,MyPos-2)
      else
        StatusLine:=Copy(HeadStr,1,MyPos-1);

      Delete(HeadStr,1,MyPos);

      MyPos:=PosEx(':',StatusLine);
      if MyPos=1 then // HTTP/2 pseudo-header field
        begin
        Delete(StatusLine,1,1);
        MyPos:=PosEx(':',StatusLine);
        if MyPos>0 then
          begin
          left:=TrimCopy(StatusLine,1,MyPos-1);
          Delete(StatusLine,1,MyPos);
          StatusLine:=Trim(StatusLine);
          SetPseudoHeader(left,StatusLine);
          end;
        end
      else if MyPos>0 then
        begin
        left:=TrimCopy(StatusLine,1,MyPos-1);
        Delete(StatusLine,1,MyPos);
        StatusLine:=Trim(StatusLine);
        SetHeader(left,StatusLine);
        end;
      MyPos:=PosEx(#10, HeadStr);
      end;

    end;

  begin
  Result:=-ERROR_INVALID_HANDLE;

  ZeroMemory(@Response,SizeOf(Response));
  SetLength(UnknownHeaders,0);
  SetLength(iTemp,0);
  iLoc:=0;

  if (ReqQueueHandle=0) or (RequestID=0) then Exit;

  { Using "iTemp" and "iLoc" as a local storage for
    raw Strings sent as part of the Response Header.
    See the "MakeStr" function for details. }
  SetLength(iTemp,Length(ResponseHeaderText)+Length(StatusText));

  ParseHeader(ResponseHeaderText);

  { Using "UknownHeaders" as a local storage for all "unknown" Headers }
  if length(UnknownHeaders)>0 then
    begin
    Response.Headers.UnknownHeaderCount:=Length(UnknownHeaders);
    Response.Headers.pUnknownHeaders:=@UnknownHeaders[0];
    end;

  if (Response.StatusCode=0) and (Response.ReasonLength=0) then
    begin
    Response.StatusCode:=StatusCode;
    if StatusText<>'' then
      begin
      Response.ReasonLength:=Length(StatusText);
      Response.pReason:=MakeStr(StatusText);
      end;
    end;

  Response.Version.MinorVersion:=1;
  Response.Version.MajorVersion:=1;

  if ForceDisconnect then
    Flags:=HTTP_SEND_RESPONSE_FLAG_DISCONNECT or CustomFlags
  else if ResponseFinished then
    Flags:=CustomFlags
  else
    Flags:=HTTP_SEND_RESPONSE_FLAG_MORE_DATA or CustomFlags;

  LastError:=
    _HttpSendHttpResponse(
      ReqQueueHandle,
      RequestID,
      Flags,
      @Response,
      NIL,
      BytesSent
      );
  if LastError=NO_ERROR then
    Result:=BytesSent
  else
    Result:=-LastError;

  ZeroMemory(@Response,SizeOf(Response));
  SetLength(UnknownHeaders,0);
  SetLength(iTemp,0);
  end;

function WHttp_SendHeader(const Request:WHttp_Request;
                          const ResponseHeaderText:RtcString='';
                          StatusCode:Integer=200;
                          const StatusText:RtcString='';
                          ResponseFinished:boolean=False;
                          ForceDisconnect:boolean=False;
                          CustomFlags:Cardinal=0):Integer; overload;
  begin
  Result:=
    WHttp_SendHeader(
      Request.ReqQueueHandle,
      Request.RequestID,
      ResponseHeaderText,
      StatusCode,
      StatusText,
      ResponseFinished,
      ForceDisconnect,
      CustomFlags);
  end;

function WHttp_SendContent(const ReqQueueHandle:THandle;
                           const RequestID:HTTP_REQUEST_ID;
                           const ResponseContentBody:RtcByteArray;
                           ResponseFinished:boolean=False;
                           ForceDisconnect:boolean=False;
                           CustomFlags:Cardinal=0):Integer;
  var
    Flags,
    BytesSent,
    LastError:Cardinal;

    EntityChunks:HTTP_DATA_CHUNK_MEMORY;
  begin
  Result:=-ERROR_INVALID_HANDLE;
  ZeroMemory(@EntityChunks,SizeOf(EntityChunks));

  if (ReqQueueHandle=0) or (RequestID=0) then Exit;

  if ForceDisconnect then
    Flags:=HTTP_SEND_RESPONSE_FLAG_DISCONNECT or CustomFlags
  else if ResponseFinished then
    Flags:=CustomFlags
  else
    Flags:=HTTP_SEND_RESPONSE_FLAG_MORE_DATA or CustomFlags;

  EntityChunks.DataChunkType:=HttpDataChunkFromMemory;
  EntityChunks.pBuffer:=@ResponseContentBody[0];
  EntityChunks.BufferLength:=length(ResponseContentBody);

  LastError:=
    _HttpSendResponseEntityBody(
      ReqQueueHandle,
      RequestID,
      Flags,
      1,
      @EntityChunks,
      BytesSent
      );
  if LastError=NO_ERROR then
    Result:=BytesSent
  else
    Result:=-LastError;;

  ZeroMemory(@EntityChunks,SizeOf(EntityChunks));
  end;

function WHttp_SendContent(const Request:WHTTP_Request;
                           const ResponseContentBody:RtcByteArray;
                           ResponseFinished:boolean=False;
                           ForceDisconnect:boolean=False;
                           CustomFlags:Cardinal=0):Integer;
  begin
  Result:=
    WHttp_SendContent(
      Request.ReqQueueHandle,
      Request.RequestID,
      ResponseContentBody,
      ResponseFinished,
      ForceDisconnect,
      CustomFlags);
  end;

function WHttp_SendFile(const ReqQueueHandle:THandle;
                        const RequestID:HTTP_REQUEST_ID;
                        const FileName:RtcWideString;
                        ResponseFinished:boolean=False;
                        StartOffset:int64=0;
                        BytesToSend:int64=-1; // EOF
                        ForceDisconnect:boolean=False;
                        CustomFlags:Cardinal=0):int64; overload;
  var
    Flags,
    BytesSent,
    LastError:Cardinal;
    FileHdl:THandle;
    EntityChunks:HTTP_DATA_CHUNK_FILE;
  begin
  Result:=-ERROR_INVALID_HANDLE;
  ZeroMemory(@EntityChunks,SizeOf(EntityChunks));

  if (ReqQueueHandle=0) or (RequestID=0) or (FileName='') then Exit;

  if ForceDisconnect then
    Flags:=HTTP_SEND_RESPONSE_FLAG_DISCONNECT or CustomFlags
  else if ResponseFinished then
    Flags:=CustomFlags
  else
    Flags:=HTTP_SEND_RESPONSE_FLAG_MORE_DATA or CustomFlags;

  FileHdl:=Open_File(FileName);
  if FileHdl<>INVALID_HANDLE_VALUE then
    begin
    EntityChunks.DataChunkType:=HttpDataChunkFromFileHandle;
    EntityChunks.FileHandle:=FileHdl;
    EntityChunks.ByteRange.StartingOffset.QuadPart:=StartOffset;
    EntityChunks.ByteRange.Length.QuadPart:=BytesToSend;

    LastError:=
      _HttpSendResponseEntityBody(
        ReqQueueHandle,
        RequestID,
        Flags,
        1,
        @EntityChunks,
        BytesSent
        );

    if LastError=NO_ERROR then
      Result:=BytesSent
    else
      Result:=-LastError;;

    Close_File(FileHdl);
    end
  else
    Result:=-ERROR_FILE_NOT_FOUND;

  ZeroMemory(@EntityChunks,SizeOf(EntityChunks));
  end;

function WHttp_SendFile(const Request:WHTTP_Request;
                        const FileName:RtcWideString;
                        ResponseFinished:boolean=False;
                        StartOffset:int64=0;
                        BytesToSend:int64=-1; // EOF
                        ForceDisconnect:boolean=False;
                        CustomFlags:Cardinal=0):int64; overload;
  begin
  Result:=
    WHttp_SendFile(
      Request.ReqQueueHandle,
      Request.RequestID,
      FileName,
      ResponseFinished,
      StartOffset,
      BytesToSend,
      ForceDisconnect,
      CustomFlags);
  end;

function WHttp_SendFile(const ReqQueueHandle:THandle;
                        const RequestID:HTTP_REQUEST_ID;
                        const FileHandle:THandle;
                        ResponseFinished:boolean=False;
                        StartOffset:int64=0;
                        BytesToSend:int64=-1; // EOF
                        ForceDisconnect:boolean=False;
                        CustomFlags:Cardinal=0):int64; overload;
  var
    Flags,
    BytesSent,
    LastError:Cardinal;
    EntityChunks:HTTP_DATA_CHUNK_FILE;
  begin
  Result:=-ERROR_INVALID_HANDLE;
  ZeroMemory(@EntityChunks,SizeOf(EntityChunks));

  if (ReqQueueHandle=0) or
     (RequestID=0) or
     (FileHandle=INVALID_HANDLE_VALUE) then Exit;

  if ForceDisconnect then
    Flags:=HTTP_SEND_RESPONSE_FLAG_DISCONNECT or CustomFlags
  else if ResponseFinished then
    Flags:=CustomFlags
  else
    Flags:=HTTP_SEND_RESPONSE_FLAG_MORE_DATA or CustomFlags;

  EntityChunks.DataChunkType:=HttpDataChunkFromFileHandle;
  EntityChunks.FileHandle:=FileHandle;
  EntityChunks.ByteRange.StartingOffset.QuadPart:=StartOffset;
  EntityChunks.ByteRange.Length.QuadPart:=BytesToSend;

  LastError:=
    _HttpSendResponseEntityBody(
      ReqQueueHandle,
      RequestID,
      Flags,
      1,
      @EntityChunks,
      BytesSent
      );

  if LastError=NO_ERROR then
    Result:=BytesSent
  else
    Result:=-LastError;;

  ZeroMemory(@EntityChunks,SizeOf(EntityChunks));
  end;

function WHttp_SendFile(const Request:WHTTP_Request;
                        const FileHandle:THandle;
                        ResponseFinished:boolean=False;
                        StartOffset:int64=0;
                        BytesToSend:int64=-1; // EOF
                        ForceDisconnect:boolean=False;
                        CustomFlags:Cardinal=0):int64; overload;
  begin
  Result:=
    WHttp_SendFile(
      Request.ReqQueueHandle,
      Request.RequestID,
      FileHandle,
      ResponseFinished,
      StartOffset,
      BytesToSend,
      ForceDisconnect,
      CustomFlags);
  end;

function WHttp_RequestInfo(const RawRequestHeaders:RtcByteArray):PHTTP_REQUEST;
  begin
  if length(RawRequestHeaders)<MIN_REQBUFFER_SIZE then
    Result:=nil
  else
    Result:=PHTTP_REQUEST(@RawRequestHeaders[0]);
  end;

function HttpApiGetProc(const ProcName : RtcString) : Pointer;
  var
    xProcName:RtcByteArray;
  begin
  xProcName:=nil;
  if Length(ProcName) = 0 then
    Result := nil
  else
    begin
    xProcName:=RtcStringToBytesZero(ProcName);
    Result := GetProcAddress(FDllHandleHttp, PAnsiChar(@(xProcName[0])) );
    if Result = nil then
      raise ERtcFatalSockException.Create('Procedure ' + String(ProcName) +
                                          ' not found in Windows Http.sys API. ' +
                                          ' Error #' + IntToStr(GetLastError));
    end;
  end;

procedure LoadWinHttpSys;
  var
    LastErr:Cardinal;
    SrvInit:boolean;
  begin
  SrvInit:=False;
  LibCS.Acquire;
  try
    if FDllHandleHttp = 0 then
      begin
      FDllHandleHttp := LoadLibrary(PChar(httpapidll));
      if FDllHandleHttp = 0 then
        raise ERtcFatalSockException.Create('Unable to load Windows Http.sys DLL.'+
                                            ' Error #' + IntToStr(GetLastError));
      try
        _HttpCreateHttpHandle           :=THttpCreateHttpHandle(HttpApiGetProc('HttpCreateHttpHandle'));
        _HttpInitialize                 :=THttpInitialize(HttpApiGetProc('HttpInitialize'));
        _HttpTerminate                  :=THttpTerminate(HttpApiGetProc('HttpTerminate'));
        _HttpDeleteServiceConfiguration :=THttpDeleteServiceConfiguration(HttpApiGetProc('HttpDeleteServiceConfiguration'));
        _HttpSetServiceConfiguration    :=THttpSetServiceConfiguration(HttpApiGetProc('HttpSetServiceConfiguration'));
        _HttpReceiveHttpRequest         :=THttpReceiveHttpRequest(HttpApiGetProc('HttpReceiveHttpRequest'));
        _HttpReceiveRequestEntityBody   :=THttpReceiveRequestEntityBody(HttpApiGetProc('HttpReceiveRequestEntityBody'));
        _HttpSendHttpResponse           :=THttpSendHttpResponse(HttpApiGetProc('HttpSendHttpResponse'));
        _HttpSendResponseEntityBody     :=THttpSendResponseEntityBody(HttpApiGetProc('HttpSendResponseEntityBody'));
        _HttpReceiveClientCertificate   :=THttpReceiveClientCertificate(HttpApiGetProc('HttpReceiveClientCertificate'));
        _HttpAddUrl                     :=THttpAddUrl(HttpApiGetProc('HttpAddUrl'));
        _HttpRemoveUrl                  :=THttpRemoveUrl(HttpApiGetProc('HttpRemoveUrl'));

        LastErr:=_HttpInitialize(HTTPAPI_VERSION_1,HTTP_INITIALIZE_SERVER);
        if LastErr<>NO_ERROR then
          raise ERtcFatalSockException.Create('Unable to Initialize Windows Http.sys (Server).'+
                                            ' Error #' + IntToStr(GetLastError));
        SrvInit:=True;

        LastErr:=_HttpInitialize(HTTPAPI_VERSION_1,HTTP_INITIALIZE_CONFIG);
        if LastErr<>NO_ERROR then
          raise ERtcFatalSockException.Create('Unable to Initialize Windows Http.sys (Config).'+
                                              ' Error #' + IntToStr(GetLastError));
      except
        if SrvInit then
          _HttpTerminate(HTTP_INITIALIZE_SERVER);

        FreeLibrary(FDllHandleHttp);
        FDllHandleHttp:=0;
        raise;
        end;
      end;
  finally
    LibCS.Release;
    end;
  end;

procedure UnLoadWinHttpSys;
  begin
  LibCS.Acquire;
  try
    if FDllHandleHttp<>0 then
      begin
      _HttpTerminate(HTTP_INITIALIZE_SERVER);
      _HttpTerminate(HTTP_INITIALIZE_CONFIG);
      FreeLibrary(FDllHandleHttp);
      FDllHandleHttp:=0;
      end;
  finally
    LibCS.Release;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; {$ENDIF}
LibCS:=TRtcCritSec.Create;
finalization
{$IFDEF RTC_DEBUG} Log('rtcWinHttpSys Finalizing ...','DEBUG');{$ENDIF}
UnloadWinHttpSys;
RtcFreeAndNil(LibCS);
{$IFDEF RTC_DEBUG} Log('rtcWinHttpSys Finalized.','DEBUG');{$ENDIF}

{$ENDIF} // {$IFDEF WINDOWS}
end.
