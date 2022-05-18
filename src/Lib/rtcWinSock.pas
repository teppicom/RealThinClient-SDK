{
  "Low-level Windows Sockets API functions (WinSock 1 & 2)"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)

  Portions from ICS
  - Copyright (C) 1996-2001 by François PIETTE
  
  Portions from Synapse
  - Copyright (c) 1999-2002 by Lukas Gebauer

  This unit is ONLY for MS Windows.

  @exclude
}
unit rtcWinSock;

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
  rtcLog;

const
  SD_RECEIVE     = 0;
  SD_SEND        = 1;
  SD_BOTH        = 2;

  TXPROTO_UDP = 'udp';
  TXPROTO_TCP = 'tcp';

type
  u_char = byte;
  u_short = word;
  u_int = dword;
  u_long = dword;
{$IFDEF WIN32}
  size_t = dword;
  TSocket = dword;
{$ELSE}{$IFDEF WIN64}
  {$IFDEF DCC}
    size_t = NativeUInt;
    TSocket = NativeUInt;
  {$ELSE}
    size_t = uint64;
    TSocket = uint64;
  {$ENDIF}
{$ELSE}
  {$message error 'WinSock: Unsupported Platform'}
{$ENDIF}{$ENDIF}

const
  cLocalHostStr : RtcString = 'localhost';
  cLocalhost : RtcString = '127.0.0.1';
  cAnyHost : RtcString = '0.0.0.0';
  cBroadcast : RtcString = '255.255.255.255';
  c6Localhost : RtcString = '::1';
  c6AnyHost : RtcString = '::0';
  c6Broadcast : RtcString = 'ffff::1';
  cAnyPort : RtcString = '0';

  SOMAXCONN       = 200;
  SOCKET_ERROR    = -1;

  SOCK_STREAM     = 1;               { stream socket }
  SOCK_DGRAM      = 2;               { datagram socket }
  SOCK_RAW        = 3;               { raw-protocol interface }
  SOCK_RDM        = 4;               { reliably-delivered message }
  SOCK_SEQPACKET  = 5;               { sequenced packet stream }

  IP_MULTICAST_IF     = 2;           { set/get IP multicast interface   }
  IP_MULTICAST_TTL    = 3;           { set/get IP multicast timetolive  }
  IP_DEFAULT_MULTICAST_TTL   = 1;    { normally limit m'casts to 1 hop  }
  IP_ADD_MEMBERSHIP   = 5;           { add  an IP group membership      }

  IPPROTO_UDP    =  17;             { user datagram protocol }
  IPPROTO_TCP    =   6;             { tcp }
  IPPROTO_IP     =   0;             { dummy for IP }

  TCP_NODELAY     = $0001;

  AF_UNSPEC       = 0;               { unspecified }
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
  AF_INET6        = 23;              { Internetwork Version 6 }

  PF_INET         = AF_INET;

  SO_KEEPALIVE    = $0008;          { keep connections alive }
  SO_BROADCAST    = $0020;          { permit sending of broadcast msgs }
  SO_LINGER       = $0080;          { linger on close if data present }
  SO_REUSEADDR    = $0004;          { allow local address reuse }
  SO_SNDTIMEO     = $1005;          { socket sending timeout }
  SO_RCVTIMEO     = $1006;          { socket receiving timeout }

  INADDR_ANY       = $00000000;
  INADDR_LOOPBACK  = $7F000001;
  INADDR_BROADCAST = -1;
  INADDR_NONE      = -1;

  // Flags used in "hints" argument to getaddrinfo().
  AI_PASSIVE     = $1;  // Socket address will be used in bind() call.
  AI_CANONNAME   = $2;  // Return canonical name in first ai_canonname.
  AI_NUMERICHOST = $4;  // Nodename must be a numeric address String.

  // getnameinfo constants
  NI_MAXHOST	   = 1025;
  NI_MAXSERV	   = 32;
  NI_NOFQDN 	   = $1;
  NI_NUMERICHOST = $2;
  NI_NAMEREQD	   = $4;
  NI_NUMERICSERV = $8;
  NI_DGRAM       = $10;

  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;

  FD_READ         = $01;
  FD_WRITE        = $02;
  FD_OOB          = $04;
  FD_ACCEPT       = $08;
  FD_CONNECT      = $10;
  FD_CLOSE        = $20;

  FDX_OPEN        = $F1;
  FDX_READ        = $F2;
  FDX_WRITE       = $F3;
  FDX_CLOSE       = $F4;

{ All Windows Sockets error constants are biased by WSABASEERR from the "normal" }

  WSABASEERR              = 10000;
{ Windows Sockets definitions of regular Microsoft C error constants }
  WSAEINTR                = (WSABASEERR+4);
  WSAEBADF                = (WSABASEERR+9);
  WSAEACCES               = (WSABASEERR+13);
  WSAEFAULT               = (WSABASEERR+14);
  WSAEINVAL               = (WSABASEERR+22);
  WSAEMFILE               = (WSABASEERR+24);
{ Windows Sockets definitions of regular Berkeley error constants }
  WSAEWOULDBLOCK          = (WSABASEERR+35);
  WSAEINPROGRESS          = (WSABASEERR+36);
  WSAEALREADY             = (WSABASEERR+37);
  WSAENOTSOCK             = (WSABASEERR+38);
  WSAEDESTADDRREQ         = (WSABASEERR+39);
  WSAEMSGSIZE             = (WSABASEERR+40);
  WSAEPROTOTYPE           = (WSABASEERR+41);
  WSAENOPROTOOPT          = (WSABASEERR+42);
  WSAEPROTONOSUPPORT      = (WSABASEERR+43);
  WSAESOCKTNOSUPPORT      = (WSABASEERR+44);
  WSAEOPNOTSUPP           = (WSABASEERR+45);
  WSAEPFNOSUPPORT         = (WSABASEERR+46);
  WSAEAFNOSUPPORT         = (WSABASEERR+47);
  WSAEADDRINUSE           = (WSABASEERR+48);
  WSAEADDRNOTAVAIL        = (WSABASEERR+49);
  WSAENETDOWN             = (WSABASEERR+50);
  WSAENETUNREACH          = (WSABASEERR+51);
  WSAENETRESET            = (WSABASEERR+52);
  WSAECONNABORTED         = (WSABASEERR+53);
  WSAECONNRESET           = (WSABASEERR+54);
  WSAENOBUFS              = (WSABASEERR+55);
  WSAEISCONN              = (WSABASEERR+56);
  WSAENOTCONN             = (WSABASEERR+57);
  WSAESHUTDOWN            = (WSABASEERR+58);
  WSAETOOMANYREFS         = (WSABASEERR+59);
  WSAETIMEDOUT            = (WSABASEERR+60);
  WSAECONNREFUSED         = (WSABASEERR+61);
  WSAELOOP                = (WSABASEERR+62);
  WSAENAMETOOLONG         = (WSABASEERR+63);
  WSAEHOSTDOWN            = (WSABASEERR+64);
  WSAEHOSTUNREACH         = (WSABASEERR+65);
  WSAENOTEMPTY            = (WSABASEERR+66);
  WSAEPROCLIM             = (WSABASEERR+67);
  WSAEUSERS               = (WSABASEERR+68);
  WSAEDQUOT               = (WSABASEERR+69);
  WSAESTALE               = (WSABASEERR+70);
  WSAEREMOTE              = (WSABASEERR+71);
  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);
  WSAEDISCON              = (WSABASEERR+101);
  WSASYSNOTREADY          = (WSABASEERR+91);
  WSAVERNOTSUPPORTED      = (WSABASEERR+92);
  WSANOTINITIALISED       = (WSABASEERR+93);
  WSATRY_AGAIN            = (WSABASEERR+1002);
  WSANO_RECOVERY          = (WSABASEERR+1003);
  WSANO_DATA              = (WSABASEERR+1004);
  WSANO_ADDRESS           = WSANO_DATA;

  SOL_SOCKET      = $ffff;          {options for socket level }
  SO_SNDBUF       = $1001;          { send buffer size }
  SO_RCVBUF       = $1002;          { receive buffer size }

  IOCPARM_MASK = $7f;
  IOC_VOID     = $20000000;
  IOC_OUT      = $40000000;
  IOC_IN       = $80000000;
  IOC_INOUT    = (IOC_IN or IOC_OUT);

  FIONREAD     = IOC_OUT or { get # bytes to read }
    ((Longint(SizeOf(u_long)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 127;
  FIONBIO      = IOC_IN or { set/clear non-blocking i/o }
    ((Longint(SizeOf(u_long)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 126;
  FIOASYNC     = IOC_IN or { set/clear async i/o }
    ((Longint(SizeOf(u_long)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 125;

type
  SunB = packed record
    s_b1, s_b2, s_b3, s_b4: u_char;
  end;
  SunW = packed record
    s_w1, s_w2: u_short;
  end;

  in_addr = record
    case integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
  end;
  TInAddr = in_addr;
  PInAddr = ^TInAddr;

  in_addr6 = record
    case integer of
      0: (S6_addr: packed array [0..15] of byte);
      1: (u6_addr8: packed array [0..15] of byte);
      2: (u6_addr16: packed array [0..7] of word);
      3: (u6_addr32: packed array [0..3] of integer);
  end;
  TInAddr6=in_addr6;
  PInAddr6=^TInAddr6;

  sockaddr_in = record
    case Integer of
      0: (
          family: u_short;
          port: u_short;
          addr: TInAddr;
          zero: array[0..7] of byte;
          );
      1: (
          sa_family: u_short;
          sa_data: array[0..13] of byte;
          );
    end;
  TSockAddrIn = sockaddr_in;
  PSockAddrIn = ^TSockAddrIn;

  sockaddr_in6 = record
		family:   u_short;     // AF_INET6
		port:     u_short;     // Transport level port number
		flowinfo: u_long;	     // IPv6 flow information
		addr:     TInAddr6;    // IPv6 address
		scope_id: u_long;      // Scope Id: IF number for link-local
                           //           SITE id for site-local
    end;
  TSockAddrIn6 = sockaddr_in6;
  PSockAddrIn6 = ^TSockAddrIn6;

  TSockAddr = record
    case integer of
      0: (sin_family:u_short);
      1: (sin:TSockAddrIn);
      2: (sin6:TSockAddrIn6);
    end;
  PSockAddr = ^TSockAddr;

  WSAData = record // !!! also WSDATA
    wVersion: Word;
    wHighVersion: Word;
{$IFDEF WIN64}
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: RtcPtrAnsiChar;
    szDescription: array[0..WSADESCRIPTION_LEN] of Byte;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of Byte;
{$ELSE}{$IFDEF WIN32}
    szDescription: array[0..WSADESCRIPTION_LEN] of Byte;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of Byte;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: RtcPtrAnsiChar;
{$ELSE}
  {$message error 'WinSock: WSAData type undefined'}
{$ENDIF}{$ENDIF}
  end;
  TWSAData = WSAData;

  PServEnt = ^TServEnt;
  servent = record
    s_name: RtcPtrAnsiChar;
    s_aliases: ^RtcPtrAnsiChar;
{$IFDEF WIN64}
    s_proto: RtcPtrAnsiChar;
    s_port: Word;
{$ELSE}{$IFDEF WIN32}
    s_port: Word;
    s_proto: RtcPtrAnsiChar;
{$ELSE}
  {$message error 'WinSock: ServEnt type undefined'}
{$ENDIF}{$ENDIF}
  end;
  TServEnt = servent;

  PProtoEnt = ^TProtoEnt;
  protoent = record
    p_name: RtcPtrAnsiChar;
    p_aliases: ^RtcPtrAnsiChar;
    p_proto: Smallint;
  end;
  TProtoEnt = protoent;

  PHostEnt = ^THostEnt;
  hostent = record
    h_name: RtcPtrAnsiChar;
    h_aliases: ^RtcPtrAnsiChar;
    h_addrtype: Smallint;
    h_length: Smallint;
    h_addr_list: ^RtcPtrAnsiChar;
  end;
  THostEnt = hostent;

  PLinger = ^TLinger;
  linger = record
    l_onoff: u_short;
    l_linger: u_short;
  end;
  TLinger = linger;

  PAddrInfo = ^TAddrInfo;
  TAddrInfo = record
                ai_flags: integer;    // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST.
                ai_family: integer;   // PF_xxx.
                ai_socktype: integer; // SOCK_xxx.
                ai_protocol: integer; // 0 or IPPROTO_xxx for IPv4 and IPv6.
                ai_addrlen: size_t;    // Length of ai_addr.
                ai_canonname: RtcPtrAnsiChar;  // Canonical name for nodename.
                ai_addr: PSockAddr;   // Binary address.
                ai_next: PAddrInfo;     // Next structure in linked list.
              end;

  PTimeVal = ^TTimeVal;
  TTimeVal = record
    tv_sec: Longint;
    tv_usec: Longint;
  end;

const
  FD_SETSIZE     =   64;

type
  TFDSet = record
    fd_count: u_int;
    fd_array: array[0..FD_SETSIZE-1] of TSocket;
  end;
  PFDSet = ^TFDSet;

const
  WSA_WSOCKET_TIMEOUT       = 12001;

  winsock1dll = 'wsock32.dll';      { 32 bits TCP/IP system DLL }
  winsock2dll = 'ws2_32.dll';       { 32 bits TCP/IP system DLL version 2}
  wship6dll = 'wship6.dll';

  INVALID_SOCKET = TSocket(NOT(0));

const
    SIO_RCVALL = $98000001;

type
    TWSAStartup            = function (wVersionRequired: word; var WSData: TWSAData): Integer; stdcall;
    TWSACleanup            = function : Integer; stdcall;
    TWSASetLastError       = procedure (iError: Integer); stdcall;
    TWSAGetLastError       = function : Integer; stdcall;
    TWSACancelAsyncRequest = function (hAsyncTaskHandle: THandle): Integer; stdcall;
    TWSAAsyncGetHostByName = function (HWindow: HWND; wMsg: u_int; name, buf: RtcPtrAnsiChar; buflen: Integer): THandle; stdcall;
    TWSAAsyncGetHostByAddr = function (HWindow: HWND; wMsg: u_int; addr: RtcPtrAnsiChar; len, Struct: Integer; buf: RtcPtrAnsiChar; buflen: Integer): THandle; stdcall;
    TWSAAsyncSelect        = function (s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Longint): Integer; stdcall;
    TGetServByName         = function (name, proto: RtcPtrAnsiChar): PServEnt; stdcall;
    TGetProtoByName        = function (name: RtcPtrAnsiChar): PProtoEnt; stdcall;
    TGetHostByName         = function (name: RtcPtrAnsiChar): PHostEnt; stdcall;
    TGetHostByAddr         = function (addr: Pointer; len, Struct: Integer): PHostEnt; stdcall;
    TGetHostName           = function (name: RtcPtrAnsiChar; len: Integer): Integer; stdcall;
    TOpenSocket            = function (af, Struct, protocol: Integer): TSocket; stdcall;
    TShutdown              = function (s: TSocket; how: Integer): Integer; stdcall;
    TSetSockOpt            = function (s: TSocket; level, optname: Integer; optval: RtcPtrAnsiChar; optlen: Integer): Integer; stdcall;
    TGetSockOpt            = function (s: TSocket; level, optname: Integer; optval: RtcPtrAnsiChar; var optlen: Integer): Integer; stdcall;
    TSendTo                = function (s: TSocket; const Buf; len, flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer; stdcall;
    TSend                  = function (s: TSocket; const Buf; len, flags: Integer): Integer; stdcall;
    TRecv                  = function (s: TSocket; var Buf; len, flags: Integer): Integer; stdcall;
    TRecvFrom              = function (s: TSocket; var Buf; len, flags: Integer; var from: TSockAddr; var fromlen: Integer): Integer; stdcall;
    Tntohs                 = function (netshort: u_short): u_short; stdcall;
    Tntohl                 = function (netlong: u_long): u_long; stdcall;
    TListen                = function (s: TSocket; backlog: Integer): Integer; stdcall;
    TIoctlSocket           = function (s: TSocket; cmd: DWORD; var arg: Integer): Integer; stdcall;
    TInet_ntoa             = function (inaddr: TInAddr): RtcPtrAnsiChar; stdcall;
    TInet_addr             = function (cp: RtcPtrAnsiChar): u_long; stdcall;
    Thtons                 = function (hostshort: u_short): u_short; stdcall;
    Thtonl                 = function (hostlong: u_long): u_long; stdcall;
    TGetSockName           = function (s: TSocket; var name: TSockAddr; var namelen: Integer): Integer; stdcall;
    TGetPeerName           = function (s: TSocket; var name: TSockAddr; var namelen: Integer): Integer; stdcall;
    TConnect               = function (s: TSocket; var name: TSockAddr; namelen: Integer): Integer; stdcall;
    TCloseSocket           = function (s: TSocket): Integer; stdcall;
    TBind                  = function (s: TSocket; var addr: TSockAddr; namelen: Integer): Integer; stdcall;
    TAccept                = function (s: TSocket; var addr: TSockAddr; var addrlen: Integer): TSocket; stdcall;
    TGetAddrInfo           = function(NodeName: RtcPtrAnsiChar; ServName: RtcPtrAnsiChar; Hints: PAddrInfo; var Addrinfo: PAddrInfo): integer; stdcall;
    TFreeAddrInfo          = procedure(ai: PAddrInfo); stdcall;
    TGetNameInfo           = function(addr: PSockAddr; namelen: Integer; host: RtcPtrAnsiChar; hostlen: DWORD; serv: RtcPtrAnsiChar; servlen: DWORD; flags: integer): integer; stdcall;
    TGetProtoByNumber      = function(proto: Integer): PProtoEnt; stdcall;
    TSelect                = function(nfds: Integer; readfds, writefds, exceptfds: PFDSet; timeout: PTimeVal): Longint; stdcall;

var
    _WSAStartup            : TWSAStartup;
    _WSACleanup            : TWSACleanup;

    _WSAGetLastError       : TWSAGetLastError;
    _WSACancelAsyncRequest : TWSACancelAsyncRequest;
    _WSAAsyncSelect        : TWSAAsyncSelect;
    _GetServByName         : TGetServByName;
    _GetHostByName         : TGetHostByName;
    _Socket                : TOpenSocket;
    _Shutdown              : TShutdown;
    _SetSockOpt            : TSetSockOpt;
    _GetSockOpt            : TGetSockOpt;
    _SendTo                : TSendTo;
    _Send                  : TSend;
    _Recv                  : TRecv;
    _RecvFrom              : TRecvFrom;
    _ntohs                 : Tntohs;
    _ntohl                 : Tntohl;
    _Listen                : TListen;
    _IoctlSocket           : TIoctlSocket;
    _Inet_ntoa             : TInet_ntoa;
    _Inet_addr             : TInet_addr;
    _htons                 : Thtons;
    _GetSockName           : TGetSockName;
    _GetPeerName           : TGetPeerName;
    _Connect               : TConnect;
    _CloseSocket           : TCloseSocket;
    _Bind                  : TBind;
    _Accept                : TAccept;
    _GetAddrInfo           : TGetAddrInfo;
    _FreeAddrInfo          : TFreeAddrInfo;
    _GetNameInfo           : TGetNameInfo;
    _GetProtoByNumber      : TGetProtoByNumber;
    _Select                : TSelect;

// *** API calls not needed ...
//   _WSASetLastError       : TWSASetLastError;
//   _WSAAsyncGetHostByName : TWSAAsyncGetHostByName;
//   _WSAAsyncGetHostByAddr : TWSAAsyncGetHostByAddr;
//   _GetProtoByName        : TGetProtoByName;
//   _GetHostByAddr         : TGetHostByAddr;
//   _GetHostName           : TGetHostName;
//   _htonl                 : Thtonl;

procedure LoadWinSock;

function WSocket_ErrorDesc(error: integer) : String;

function WSocket_closesocket(s: TSocket): Integer;
function WSocket_shutdown(s: TSocket; how: Integer): Integer;

function WSocket_htons(hostshort: u_short): u_short;
function WSocket_ntohs(netshort: u_short): u_short;
function WSocket_ntohl(netlong: u_long): u_long;

function WSocket_ResolveHost(const InAddr : RtcString) : TInAddr;
function WSocket_ResolvePort(const Port : RtcString; const Proto : RtcString) : Word;

function SizeOfSockAddr(const sin: TSockAddr): integer;

function WSocket_GetSinPort(const Sin: TSockAddr): Integer;
function WSocket_GetSinIP(const Sin: TSockAddr): RtcString;
function WSocket_SetVarSin(var Sin: TSockAddr; const IP, Port: RtcString; Family, SockProtocol, SockType: integer; PreferIP4, PreferIPDef: Boolean): integer;

procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
procedure FD_ZERO(var FDSet: TFDSet);

{$ENDIF} // {$IFDEF WINDOWS}

implementation

{$IFDEF WINDOWS}

var
  GInitData      : TWSADATA;
  FDllHandle     : THandle  = 0;
  FDllHandle6    : THandle  = 0;
  SockEnhancedApi: Boolean;
  SockWship6Api: Boolean;
  LibCS: TRtcCritSec;

function IsNewApi(Family: integer): Boolean;
  begin
  Result := SockEnhancedApi;
  if not Result then
    Result := (Family in [AF_INET6,AF_UNSPEC]) and SockWship6Api;
  end;

function SizeOfSockAddr(const sin: TSockAddr): integer;
  begin
  case sin.sin_family of
    AF_INET:
            Result := SizeOf(TSockAddrIn);
    AF_INET6:
            Result := SizeOf(TSockAddrIn6);
    else
      Result := 0;
    end;
  end;

procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
  begin
  if FDSet.fd_count < FD_SETSIZE then
    begin
    FDSet.fd_array[FDSet.fd_count] := Socket;
    Inc(FDSet.fd_count);
    end;
  end;

procedure FD_ZERO(var FDSet: TFDSet);
  begin
  FDSet.fd_count := 0;
  end;

function WSocket_SetVarSin(var Sin: TSockAddr; const IP, Port: RtcString; Family, SockProtocol, SockType: integer; PreferIP4, PreferIPDef: Boolean): integer;
  type
    pu_long = ^u_long;
  var
    ProtoEnt: PProtoEnt;
    ServEnt: PServEnt;
    HostEnt: PHostEnt;
    Hints1, Hints2, Hints3: TAddrInfo;
    TwoPass: boolean;
    IP2,Port2:RtcByteArray;

  function Get_Addr(var Hints: TAddrInfo): integer;
    var
      NextAddr, Addr: PAddrInfo;
    begin
    Addr := nil;
    try
      FillChar(Sin, Sizeof(Sin), 0);
      if Hints.ai_socktype = SOCK_RAW then
        begin
        Hints.ai_socktype := 0;
        Hints.ai_protocol := 0;
        Result := _GetAddrInfo(@IP2[0], nil, @Hints, Addr);
        end
      else
        begin
        if (IP='') or (IP=cAnyHost) or (IP=c6AnyHost) then
          begin
          Hints.ai_flags := AI_PASSIVE;
          Result := _GetAddrInfo(nil, @Port2[0], @Hints, Addr);
          end
        else if (IP=cLocalhost) or (IP=c6Localhost) or (IP=cLocalHostStr) then
          begin
          Result := _GetAddrInfo(nil, @Port2[0], @Hints, Addr);
          end
        else
          begin
          Result := _GetAddrInfo(@IP2[0], @Port2[0], @Hints, Addr);
          end;
        end;
      if Result = 0 then
        if Addr <> nil then
          begin
          NextAddr:=Addr;
          repeat
            if (NextAddr^.ai_family=AF_INET) or
               (NextAddr^.ai_family=AF_INET6) then
              if (NextAddr^.ai_family=Hints.ai_family) or
                 (Hints.ai_family=AF_UNSPEC) then
                if assigned(NextAddr^.ai_addr) then
                  begin
                  Move(NextAddr^.ai_addr^, Sin, NextAddr^.ai_addrlen);
                  Break;
                  end
                else if assigned(NextAddr^.ai_canonname) then
                  begin
                  Move(NextAddr^.ai_canonname^, Sin, NextAddr^.ai_addrlen);
                  Break;
                  end;
            NextAddr:=NextAddr^.ai_next;
            until NextAddr=nil;
          if NextAddr=nil then
            Result := -1;
          end
        else
          Result:=-1;
    finally
      if Assigned(Addr) then
        _FreeAddrInfo(Addr);
      end;
    end;

  begin
  Result := 0;

  IP2:=RtcStringToBytesZero(IP);
  Port2:=RtcStringToBytesZero(Port);

  FillChar(Sin, Sizeof(Sin), 0);
  if not IsNewApi(family) then
    begin
    Sin.sin_family := AF_INET;
    ProtoEnt := _GetProtoByNumber(SockProtocol);
    ServEnt := nil;
    if ProtoEnt <> nil then
      ServEnt := _GetServByName(@Port2[0], ProtoEnt^.p_name);
    if ServEnt = nil then
      Sin.sin.port := _htons(Str2IntDef(Port, 0))
    else
      Sin.sin.port := ServEnt^.s_port;
    if IP=cBroadcast then
      Sin.sin.addr.s_addr := u_long(INADDR_BROADCAST)
    else
      begin
      Sin.sin.addr.s_addr := _inet_addr(@IP2[0]);
      if Sin.sin.addr.s_addr = u_long(INADDR_NONE) then
        begin
        HostEnt := _GetHostByName(@IP2[0]);
        Result := _WSAGetLastError;
        if HostEnt <> nil then
          Sin.sin.addr:=PInAddr(HostEnt^.h_addr_list^)^;
        end;
      end;
    end
  else
    begin
    FillChar(Hints1, Sizeof(Hints1), 0);
    FillChar(Hints2, Sizeof(Hints2), 0);
    FillChar(Hints3, Sizeof(Hints3), 0);

    TwoPass := False;
    if Family = AF_UNSPEC then
      begin
      TwoPass := True;
      if PreferIP4 then
        begin
        if PreferIPDef then
          begin
          Hints1.ai_family := AF_UNSPEC;
          Hints2.ai_family := AF_INET;
          end
        else
          begin
          Hints1.ai_family := AF_INET;
          Hints2.ai_family := AF_UNSPEC;
          end;
        Hints3.ai_family := AF_INET6;
        end
      else
        begin
        if PreferIPDef then
          begin
          Hints1.ai_family := AF_UNSPEC;
          Hints2.ai_family := AF_INET6;
          end
        else
          begin
          Hints1.ai_family := AF_INET6;
          Hints2.ai_family := AF_UNSPEC;
          end;
        Hints3.ai_family := AF_INET;
        end;
      end
    else
      Hints1.ai_family := Family;

    Hints1.ai_socktype := SockType;
    Hints1.ai_protocol := SockProtocol;

    Result := Get_Addr(Hints1);
    if Result <> 0 then
      if TwoPass then
        begin
        Hints2.ai_socktype := Hints1.ai_socktype;
        Hints2.ai_protocol := Hints1.ai_protocol;
        Result := Get_Addr(Hints2);
        if Result <> 0 then
          begin
          Hints3.ai_socktype := Hints1.ai_socktype;
          Hints3.ai_protocol := Hints1.ai_protocol;
          Result := Get_Addr(Hints3);
          end;
        end;
    end;
  end;

function atoi(const value : RtcString) : Word;
var
    i : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] >= '0') and (Value[i] <= '9')do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
end;

function IsDigit(Ch : Byte) : Boolean;
begin
    Result := (ch >= Byte('0')) and (ch <= Byte('9'));
end;

function WSocketIsDottedIP(const S : RtcString) : Boolean;
var
    I          : Integer;
    DotCount   : Integer;
    NumVal     : Integer;
begin
    Result     := FALSE;
    DotCount   := 0;
    NumVal     := 0;
    I          := 1;
    { Skip leading spaces }
    while (I <= Length(S)) and (S[I] = ' ') do
        Inc(I);
    { Can't begin with a dot }
    if (I <= Length(S)) and (S[I] = '.') then
        Exit;
    { Scan full String }
    while I <= Length(S) do begin
        if S[I] = '.' then begin
            Inc(DotCount);
            if (DotCount > 3) or (NumVal > 255) then
                Exit;
            NumVal := 0;
            { A dot must be followed by a digit }
            {$IFDEF RTC_BYTESTRING}
            if (I >= Length(S)) or (not (S[I + 1] in ['0'..'9'])) then Exit;
            {$ELSE}
            if (I >= Length(S)) or (Pos(S[I + 1],'0123456789')<=0) then Exit;
            {$ENDIF}
        end
        {$IFDEF RTC_BYTESTRING}
        else if S[I] in ['0'..'9'] then
        {$ELSE}
        else if Pos(S[I],'0123456789')>0 then
        {$ENDIF}
            NumVal := NumVal * 10 + Ord(S[I]) - Ord('0')
        else begin
            { Not a digit nor a dot. Accept spaces until end of String }
            while (I <= Length(S)) and (S[I] = ' ') do
                Inc(I);
            if I <= Length(S) then
                Exit;  { Not a space, do not accept }
            break;     { Only spaces, accept        }
        end;
        Inc(I);
    end;
    { We must have exactly 3 dots }
    if (DotCount <> 3) or (NumVal > 255) then
        Exit;
    Result := TRUE;
end;

function WSocket_GetSinPort(const Sin: TSockAddr): Integer;
  begin
  if (Sin.sin_family = AF_INET6) then
    Result := _ntohs(Sin.sin6.port)
  else
    Result := _ntohs(Sin.sin.port);
  end;

function WSocket_GetSinIP(const Sin: TSockAddr): RtcString;
  var
    p: Pointer;
    host, serv: RtcByteArray;
    hostlen, servlen: integer;
    r: integer;
  begin
  Result := '';
  if not IsNewApi(Sin.sin_family) then
    begin
    p := _inet_ntoa(Sin.sin.addr);
    if p <> nil then
      Result := RtcPBytesZeroToString(p^);
    end
  else
    begin
    hostlen := NI_MAXHOST;
    servlen := NI_MAXSERV;
    setlength(host, hostlen);
    setlength(serv, servlen);
    r := _GetNameInfo(@sin, SizeOfSockAddr(sin), @host[0], hostlen,
                      @serv[0], servlen, NI_NUMERICHOST + NI_NUMERICSERV);
    if r = 0 then
      Result := RtcBytesZeroToString(host);
    end;
  end;

function WSocket_closesocket(s: TSocket): Integer;
  begin
  if FDllHandle=0 then
    raise ERtcFatalSockException.Create('WinSock not loaded.');
  Result:=_CloseSocket(S);
  end;

function WSocket_shutdown(s: TSocket; how: Integer): Integer;
  begin
  if FDllHandle=0 then
    raise ERtcFatalSockException.Create('WinSock not loaded.');
  Result:=_Shutdown(S,how);
  end;

function WSocket_htons(hostshort: u_short): u_short;
  begin
  if FDllHandle=0 then
    raise ERtcFatalSockException.Create('WinSock not loaded.');
  Result:=_htons(hostshort);
  end;

function WSocket_ntohs(netshort: u_short): u_short;
  begin
  if FDllHandle=0 then
    raise ERtcFatalSockException.Create('WinSock not loaded.');
  Result:=_ntohs(netshort);
  end;

function WSocket_ntohl(netlong: u_long): u_long;
  begin
  if FDllHandle=0 then
    raise ERtcFatalSockException.Create('WinSock not loaded.');
  Result:=_ntohl(netlong);
  end;

function WSocket_ErrorDesc(error: integer) : String;
  begin
    case error of
    0,
    WSABASEERR:
      Result := 'No Error';
    WSAEINTR:
      Result := 'Interrupted system call';
    WSAEBADF:
      Result := 'Bad file number';
    WSAEACCES:
      Result := 'Permission denied';
    WSAEFAULT:
      Result := 'Bad address';
    WSAEINVAL:
      Result := 'Invalid argument';
    WSAEMFILE:
      Result := 'Too many open files';
    WSAEWOULDBLOCK:
      Result := 'Operation would block';
    WSAEINPROGRESS:
      Result := 'Operation now in progress';
    WSAEALREADY:
      Result := 'Operation already in progress';
    WSAENOTSOCK:
      Result := 'Socket operation on non-socket';
    WSAEDESTADDRREQ:
      Result := 'Destination address required';
    WSAEMSGSIZE:
      Result := 'Message too long';
    WSAEPROTOTYPE:
      Result := 'Protocol wrong type for socket';
    WSAENOPROTOOPT:
      Result := 'Protocol not available';
    WSAEPROTONOSUPPORT:
      Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT:
      Result := 'Socket type not supported';
    WSAEOPNOTSUPP:
      Result := 'Operation not supported on socket';
    WSAEPFNOSUPPORT:
      Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT:
      Result := 'Address family not supported by protocol family';
    WSAEADDRINUSE:
      Result := 'Address or Port already in use';
    WSAEADDRNOTAVAIL:
      Result := 'Address or Port not available';
    WSAENETDOWN:
      Result := 'Network is down';
    WSAENETUNREACH:
      Result := 'Network is unreachable';
    WSAENETRESET:
      Result := 'Network dropped connection on reset';
    WSAECONNABORTED:
      Result := 'Connection aborted';
    WSAECONNRESET:
      Result := 'Connection reset by peer';
    WSAENOBUFS:
      Result := 'No buffer space available';
    WSAEISCONN:
      Result := 'Socket is already connected';
    WSAENOTCONN:
      Result := 'Socket is not connected';
    WSAESHUTDOWN:
      Result := 'Can''t send after socket shutdown';
    WSAETOOMANYREFS:
      Result := 'Too many references: can''t splice';
    WSAETIMEDOUT:
      Result := 'Connection timed out';
    WSAECONNREFUSED:
      Result := 'Connection refused';
    WSAELOOP:
      Result := 'Too many levels of symbolic links';
    WSAENAMETOOLONG:
      Result := 'File name too long';
    WSAEHOSTDOWN:
      Result := 'Host is down';
    WSAEHOSTUNREACH:
      Result := 'No route to host';
    WSAENOTEMPTY:
      Result := 'Directory not empty';
    WSAEPROCLIM:
      Result := 'Too many processes';
    WSAEUSERS:
      Result := 'Too many users';
    WSAEDQUOT:
      Result := 'Disc quota exceeded';
    WSAESTALE:
      Result := 'Stale NFS file handle';
    WSAEREMOTE:
      Result := 'Too many levels of remote in path';
    WSASYSNOTREADY:
      Result := 'Network sub-system is unusable';
    WSAVERNOTSUPPORTED:
      Result := 'WinSock DLL cannot support this application';
    WSANOTINITIALISED:
      Result := 'WinSock not initialized';
    WSAHOST_NOT_FOUND:
      Result := 'Host not found';
    WSATRY_AGAIN:
      Result := 'Non-authoritative host not found';
    WSANO_RECOVERY:
      Result := 'Non-recoverable error';
    WSANO_DATA:
      Result := 'No Data';
    else
      Result := 'Not a WinSock error';
    end;
  end;

function WSocket_ResolveHost(const InAddr : RtcString) : TInAddr;
  var
    szData  : RtcByteArray;
    Phe     : Phostent;
    IPAddr  : u_long;
    lasterr : longint;
  begin
    if Length(InAddr) = 0 then
      raise ERtcSocketError.Create('WSocketResolveHost: ''' + String(InAddr) + ''' Invalid Hostname.');

    szData:= RtcStringToBytesZero(Trim(InAddr)); { Length already checked above }
    if WSocketIsDottedIP(InAddr) then begin
        { Address is a dotted numeric address like 192.161.124.32 }
        IPAddr := _inet_addr(@szData[0]);
        if IPAddr = u_long(INADDR_NONE) then begin
            if PosEx('255.255.255.255',szData)>0 then begin
                Result.s_addr := u_long(INADDR_BROADCAST);
                Exit;
            end;
            raise ERtcSocketError.Create('WSocketResolveHost: ''' + String(InAddr) + ''' Invalid IP address.');
        end;
        Result.s_addr := IPAddr;
        Exit;
    end;

    { Address is a hostname }
    Phe := _GetHostByName(@szData[0]);
    if Phe = nil then
        begin
        lasterr:=_WSAGetLastError;
        raise ERtcSocketError.CreateFmt(
                 'WSocketResolveHost: Cannot convert host address ''%s'', Error #%d',
                 [InAddr, lasterr]);
        end;
    Result.s_addr := PInAddr(Phe^.h_addr_list^)^.s_addr;
  end;

{ Convert port name or number to number in host order (ftp -> 21)           }
function WSocket_ResolvePort(const Port : RtcString; const Proto : RtcString) : Word;
  var
    szPort   : RtcByteArray;
    szProto  : RtcByteArray;
    Pse      : Pservent;
    lasterr  : longint;
  begin
    if (Length(Port) = 0) or (Length(Port) >= 32) then
        raise ERtcSocketError.Create('WSocketResolvePort: Invalid Port.');

    if (Length(Proto) = 0) or (Length(Proto) >= 32) then
        raise ERtcSocketError.Create('WSocketResolvePort: Invalid Proto.');

    if IsDigit(Byte(Port[1])) then
        Result := atoi(Port)
    else
      begin
        SetLength(szPort,0);
        SetLength(szProto,0);
        szPort:=RtcStringToBytesZero(Trim(Port));   { Length already checked above }
        szProto:=RtcStringToBytesZero(Trim(Proto)); { Length already checked above }
        if szProto[0] = 0 then
            Pse := _GetServByName(@szPort[0], nil)
        else
            Pse := _GetServByName(@szPort[0], @szProto[0]);
        if Pse = nil then
            begin
            lasterr:=_WSAGetLastError;
            raise ERtcSocketError.CreateFmt(
                     'WSocketResolvePort: Cannot convert port ''%s'', Error #%d',
                     [Port, lasterr]);
            end;
        Result := _ntohs(Pse^.s_port);
      end;
  end;

function WinSockGetProc(const ProcName : RtcString) : Pointer;
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
      raise ERtcFatalSockException.Create('Procedure ' + String(ProcName) +
                                     ' not found in WinSock. ' +
                                     ' Error #' + IntToStr(GetLastError));
    end;
  end;

function xGetProc(xdll: THandle; const ProcName : RtcString) : Pointer;
  var
    xProcName:RtcByteArray;
  begin
  xProcName:=nil;
  if Length(ProcName) = 0 then
    Result := nil
  else
    begin
    xProcName:=RtcStringToBytesZero(ProcName);
    Result := GetProcAddress(xdll, RtcPtrAnsiChar(@(xProcName[0])) );
    end;
  end;

procedure LoadWinSock;
  var
    LastError : LongInt;
    v2 : boolean;
  begin
  LibCS.Acquire;
  try
    if FDllHandle = 0 then
      begin
      SockEnhancedApi := False;
      SockWship6Api := False;
      v2:=True;
      FDllHandle := LoadLibrary(PChar(winsock2dll)); // first, try loading WinSock 2
      if FDllHandle = 0 then
        begin // WinSock 2 not available, try WinSock 1
        v2:=False;
        FDllHandle := LoadLibrary(PChar(winsock1dll));
        if FDllHandle = 0 then
          raise ERtcFatalSockException.Create('Unable to load WinSock DLL.'+
                                         ' Error #' + IntToStr(GetLastError));
        end;

      try
        _WSAStartup            := TWSAStartup(WinSockGetProc('WSAStartup'));
        _WSACleanup            := TWSACleanup(WinSockGetProc('WSACleanup'));
        _WSAGetLastError       := TWSAGetLastError(WinSockGetProc('WSAGetLastError'));
        _WSACancelAsyncRequest := TWSACancelAsyncRequest(WinSockGetProc('WSACancelAsyncRequest'));
        _WSAAsyncSelect        := TWSAAsyncSelect(WinSockGetProc('WSAAsyncSelect'));
        _GetServByName         := TGetServByName(WinSockGetProc('getservbyname'));
        _GetHostByName         := TGetHostByName(WinSockGetProc('gethostbyname'));
        _Socket                := TOpenSocket(WinSockGetProc('socket'));
        _Shutdown              := TShutdown(WinSockGetProc('shutdown'));
        _SetSockOpt            := TSetSockOpt(WinSockGetProc('setsockopt'));
        _GetSockOpt            := TGetSockOpt(WinSockGetProc('getsockopt'));
        _SendTo                := TSendTo(WinSockGetProc('sendto'));
        _Send                  := TSend(WinSockGetProc('send'));
        _Recv                  := TRecv(WinSockGetProc('recv'));
        _RecvFrom              := TRecvFrom(WinSockGetProc('recvfrom'));
        _ntohs                 := Tntohs(WinSockGetProc('ntohs'));
        _ntohl                 := Tntohl(WinSockGetProc('ntohl'));
        _Listen                := TListen(WinSockGetProc('listen'));
        _IoctlSocket           := TIoctlSocket(WinSockGetProc('ioctlsocket'));
        _Inet_ntoa             := TInet_ntoa(WinSockGetProc('inet_ntoa'));
        _Inet_addr             := TInet_addr(WinSockGetProc('inet_addr'));
        _htons                 := Thtons(WinSockGetProc('htons'));
        _GetSockName           := TGetSockName(WinSockGetProc('getsockname'));
        _GetPeerName           := TGetPeerName(WinSockGetProc('getpeername'));
        _Connect               := TConnect(WinSockGetProc('connect'));
        _CloseSocket           := TCloseSocket(WinSockGetProc('closesocket'));
        _Bind                  := TBind(WinSockGetProc('bind'));
        _Accept                := TAccept(WinSockGetProc('accept'));
        _GetProtoByNumber      := TGetProtoByNumber(WinSockGetProc('getprotobynumber'));
        _Select                := TSelect(WinSockGetProc('select'));

        // *** API calls not needed ...
        //_WSASetLastError       := TWSASetLastError(WinSockGetProc('WSASetLastError'));
        //_WSAAsyncGetHostByName := TWSAAsyncGetHostByName(WinSockGetProc('WSAAsyncGetHostByName'));
        //_WSAAsyncGetHostByAddr := TWSAAsyncGetHostByAddr(WinSockGetProc('WSAAsyncGetHostByAddr'));
        //_GetProtoByName        := TGetProtoByName(WinSockGetProc('getprotobyname'));
        //_GetHostByAddr         := TGetHostByAddr(WinSockGetProc('gethostbyaddr'));
        //_GetHostName           := TGetHostName(WinSockGetProc('gethostname'));
        //_htonl                 := Thtonl(WinSockGetProc('htonl'));

        _GetAddrInfo := xGetProc(FDllHandle, 'getaddrinfo');
        _FreeAddrInfo := xGetProc(FDllHandle, 'freeaddrinfo');
        _GetNameInfo := xGetProc(FDllHandle, 'getnameinfo');
        SockEnhancedApi := Assigned(_GetAddrInfo) and Assigned(_FreeAddrInfo) and Assigned(_GetNameInfo);
        if not SockEnhancedApi then
          begin
          FDllHandle6 := LoadLibrary(PChar(wship6dll));
          if FDllHandle6 <> 0 then
            begin
            _GetAddrInfo := xGetProc(FDllHandle6, 'getaddrinfo');
            _FreeAddrInfo := xGetProc(FDllHandle6, 'freeaddrinfo');
            _GetNameInfo := xGetProc(FDllHandle6, 'getnameinfo');
            SockWship6Api := Assigned(_GetAddrInfo) and Assigned(_FreeAddrInfo) and Assigned(_GetNameInfo);
            end;
          end;

      except
        FreeLibrary(FDllHandle);
        FDllHandle:=0;
        raise;
        end;

      if v2 then
        LastError := _WSAStartup($202, GInitData)
      else
        LastError := _WSAStartup($101, GInitData);

      if LastError <> 0 then
        begin
        if v2 then
          LastError := _WSAStartup($101, GInitData);
        if LastError<>0 then
          raise ERtcFatalSockException.CreateFmt('WinSock API: WSAStartup error #%d', [LastError]);
        end;
      end;
  finally
    LibCS.Release;
    end;
  end;

procedure UnloadWinSock;
  begin
  LibCS.Acquire;
  try
    if FDllHandle<>0 then
      begin
      try
        _WSACleanup;
      finally
        FreeLibrary(FDllHandle);
        FDllHandle:=0;
        end;
      end;
    if FDllHandle6<>0 then
      begin
      FreeLibrary(FDllHandle6);
      FDllHandle6:=0;
      end;
  finally
    LibCS.Release;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcWinSock Initializing ...','DEBUG');{$ENDIF}
LibCS:=TRtcCritSec.Create;
{$IFDEF RTC_DEBUG} Log('rtcWinSock Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcWinSock Finalizing ...','DEBUG');{$ENDIF}
UnloadWinSock;
RtcFreeAndNil(LibCS);
{$IFDEF RTC_DEBUG} Log('rtcWinSock Finalized.','DEBUG');{$ENDIF}

{$ENDIF} // {$IFDEF WINDOWS}
end.
