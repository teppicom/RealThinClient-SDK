{
  "Low-level Unix/Linux Sockets API functions (BSD Sockets from Libc.so.6)"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)

  Portions from Synapse - Copyright (c) 1999-2002 by Lukas Gebauer

  @exclude
}
unit rtcNixSock;

{$INCLUDE rtcDefs.inc}

interface

{$IFDEF RTC_NIX_SOCK}

uses
  SysUtils,
  Classes,
  SyncObjs,

  {$IFDEF FPC} dynlibs, {$ENDIF}

  rtcTypes,
  rtcSystem,
  rtcLog;

TYPE
  u_short = Word;
  u_int = LongInt;
  u_long = LongInt;
  TSocket = Integer;
TYPE
  __fd_mask = Cardinal;
CONST
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
  AF_INET6        = 10;              { Internetwork Version 6 }
  __FD_SETSIZE    = 1024;
  __NFDBITS       = 8 * sizeof(__fd_mask);
TYPE
  __fd_set = {packed} record
    fds_bits: packed array[0..(__FD_SETSIZE div __NFDBITS)-1] of __fd_mask;
    end;
  TFDSet = __fd_set;
  TInAddr = packed record
    case integer of
      0: (S_bytes: packed array [0..3] of byte);
      1: (S_addr: u_long);
  end;
  TInAddr6 = packed record
    case integer of
      0: (S6_addr: packed array [0..15] of byte);
      1: (u6_addr8: packed array [0..15] of byte);
      2: (u6_addr16: packed array [0..7] of word);
      3: (u6_addr32: packed array [0..3] of integer);
  end;
  TSockAddr = packed record
    case integer of
      0: (AddressFamily: u_short);
      1: (
        case sin_family: u_short of
          AF_INET: (sin_port: u_short;
                    sin_addr: TInAddr;
                    sin_zero: array[0..7] of AnsiChar);
          AF_INET6: (sin6_port:     u_short;
                		sin6_flowinfo: u_long;
      	    	      sin6_addr:     TInAddr6;
      		          sin6_scope_id: u_long);
          );
  end;

CONST
  INVALID_SOCKET		= TSocket(NOT(0));

  IPPROTO_IP     =   0;		{ Dummy					}
  IPPROTO_ICMP   =   1;		{ Internet Control Message Protocol }
  IPPROTO_IGMP   =   2;		{ Internet Group Management Protocol}
  IPPROTO_TCP    =   6;		{ TCP           			}
  IPPROTO_UDP    =   17;	{ User Datagram Protocol		}
  IPPROTO_IPV6   =   41;
  IPPROTO_ICMPV6 =   58;

  IPPROTO_RAW    =   255;
  IPPROTO_MAX    =   256;

  INADDR_ANY       = $00000000;
  INADDR_LOOPBACK  = $7F000001;
  INADDR_BROADCAST = $FFFFFFFF;
  INADDR_NONE      = $FFFFFFFF;
  ADDR_ANY		 = INADDR_ANY;
  SOCKET_ERROR			= -1;

  cLocalHostStr = 'localhost';
  cLocalhost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  cBroadcast = '255.255.255.255';
  c6Localhost = '::1';
  c6AnyHost = '::0';
  c6Broadcast = 'ffff::1';
  cAnyPort = '0';

  SOCK_STREAM     = 1;               { stream socket }
  SOCK_DGRAM      = 2;               { datagram socket }
  SOCK_RAW        = 3;               { raw-protocol interface }
  SOCK_RDM        = 4;               { reliably-delivered message }
  SOCK_SEQPACKET  = 5;               { sequenced packet stream }

  SHUT_WR = 2;

  TCP_NODELAY     = $0001;

TYPE
  PInAddr = ^TInAddr;
  PInAddr6 = ^TInAddr6;

VAR
  in6addr_any, in6addr_loopback : TInAddr6;

CONST
  DLLStackName = '/usr/lib/libc.dylib';
  DLLStackName2 = 'libc.so.6';

  SIGPIPE = 13;
  SIG_IGN = 1;

  FIONREAD        = $541B;
  FIONBIO         = $5421;
  FIOASYNC        = $5452;

  IP_TOS             = 1;  { int; IP type of service and precedence.  }
  IP_TTL             = 2;  { int; IP time to live.  }
  IP_HDRINCL         = 3;  { int; Header is included with data.  }
  IP_OPTIONS         = 4;  { ip_opts; IP per-packet options.  }
  IP_ROUTER_ALERT    = 5;  { bool }
  IP_RECVOPTS        = 6;  { bool }
  IP_RETOPTS         = 7;  { bool }
  IP_PKTINFO         = 8;  { bool }
  IP_PKTOPTIONS      = 9;
  IP_PMTUDISC        = 10; { obsolete name? }
  IP_MTU_DISCOVER    = 10; { int; see below }
  IP_RECVERR         = 11; { bool }
  IP_RECVTTL         = 12; { bool }
  IP_RECVTOS         = 13; { bool }
  IP_MULTICAST_IF    = 32; { in_addr; set/get IP multicast i/f }
  IP_MULTICAST_TTL   = 33; { u_char; set/get IP multicast ttl }
  IP_MULTICAST_LOOP  = 34; { i_char; set/get IP multicast loopback }
  IP_ADD_MEMBERSHIP  = 35; { ip_mreq; add an IP group membership }
  IP_DROP_MEMBERSHIP = 36; { ip_mreq; drop an IP group membership }

  SOL_SOCKET    = 1;

  SO_DEBUG      = 1;
  SO_REUSEADDR  = 2;
  SO_TYPE       = 3;
  SO_ERROR      = 4;
  SO_DONTROUTE  = 5;
  SO_BROADCAST  = 6;
  SO_SNDBUF     = 7;
  SO_RCVBUF     = 8;
  SO_KEEPALIVE  = 9;
  SO_OOBINLINE  = 10;
  SO_NO_CHECK   = 11;
  SO_PRIORITY   = 12;
  SO_LINGER     = 13;
  SO_BSDCOMPAT  = 14;
  SO_REUSEPORT  = 15;
  SO_PASSCRED   = 16;
  SO_PEERCRED   = 17;
  SO_RCVLOWAT   = 18;
  SO_SNDLOWAT   = 19;
  SO_RCVTIMEO   = 20;
  SO_SNDTIMEO   = 21;

{ Security levels - as per NRL IPv6 - don't actually do anything }
  SO_SECURITY_AUTHENTICATION       = 22;
  SO_SECURITY_ENCRYPTION_TRANSPORT = 23;
  SO_SECURITY_ENCRYPTION_NETWORK   = 24;
  SO_BINDTODEVICE                  = 25;
{ Socket filtering }
  SO_ATTACH_FILTER = 26;
  SO_DETACH_FILTER = 27;

  SOMAXCONN       = 128;

  IPV6_UNICAST_HOPS     = 16;
  IPV6_MULTICAST_IF     = 17;
  IPV6_MULTICAST_HOPS   = 18;
  IPV6_MULTICAST_LOOP   = 19;
  IPV6_JOIN_GROUP       = 20;
  IPV6_LEAVE_GROUP      = 21;

  // getnameinfo constants
  NI_MAXHOST	   = 1025;
  NI_MAXSERV	   = 32;
  NI_NOFQDN 	   = $4;
  NI_NUMERICHOST = $1;
  NI_NAMEREQD	   = $8;
  NI_NUMERICSERV = $2;
  NI_DGRAM       = $10;

{ Address families. }
  AF_UNSPEC       = 0;               { unspecified }
  AF_MAX          = 24;

{ Protocol families, same as address families for now. }
  PF_UNSPEC       = AF_UNSPEC;
  PF_INET         = AF_INET;
  PF_INET6        = AF_INET6;
  PF_MAX          = AF_MAX;

  // Flags used in "hints" argument to getaddrinfo().
  AI_PASSIVE     = $1;  // Socket address will be used in bind() call.
  AI_CANONNAME   = $2;  // Return canonical name in first ai_canonname.
  AI_NUMERICHOST = $4;  // Nodename must be a numeric address AnsiString.


{$IFDEF MACOSX}
  MSG_NOSIGNAL  = 0;
{$ELSE}{$IFDEF Darwin}
  MSG_NOSIGNAL  = 0;
{$ELSE}
  MSG_NOSIGNAL  = $4000;                // Do not generate SIGPIPE.
{$ENDIF}{$ENDIF}
  MSG_OOB       = $01;                  // Process out-of-band data.
  MSG_PEEK      = $02;                  // Peek at incoming messages.

  ESysEPERM       = 1;    { Operation not permitted }
  ESysENOENT      = 2;    { No such file or directory }
  ESysESRCH       = 3;    { No such process }
  ESysEINTR       = 4;    { Interrupted system call }
  ESysEIO = 5;    { I/O error }
  ESysENXIO       = 6;    { No such device or address }
  ESysE2BIG       = 7;    { Arg list too long }
  ESysENOEXEC     = 8;    { Exec format error }
  ESysEBADF       = 9;    { Bad file number }
  ESysECHILD      = 10;   { No child processes }
  ESysEAGAIN      = 11;   { Try again }
  ESysENOMEM      = 12;   { Out of memory }
  ESysEACCES      = 13;   { Permission denied }
  ESysEFAULT      = 14;   { Bad address }
  ESysENOTBLK     = 15;   { Block device required, NOT POSIX! }
  ESysEBUSY       = 16;   { Device or resource busy }
  ESysEEXIST      = 17;   { File exists }
  ESysEXDEV       = 18;   { Cross-device link }
  ESysENODEV      = 19;   { No such device }
  ESysENOTDIR     = 20;   { Not a directory }
  ESysEISDIR      = 21;   { Is a directory }
  ESysEINVAL      = 22;   { Invalid argument }
  ESysENFILE      = 23;   { File table overflow }
  ESysEMFILE      = 24;   { Too many open files }
  ESysENOTTY      = 25;   { Not a typewriter }
  ESysETXTBSY     = 26;   { Text file busy. The new process was
                            a pure procedure (shared text) file which was
                            open for writing by another process, or file
                            which was open for writing by another process,
                            or while the pure procedure file was being
                            executed an open(2) call requested write access
                            requested write access.}
  ESysEFBIG       = 27;   { File too large }
  ESysENOSPC      = 28;   { No space left on device }
  ESysESPIPE      = 29;   { Illegal seek }
  ESysEROFS       = 30;   { Read-only file system }
  ESysEMLINK      = 31;   { Too many links }
  ESysEPIPE       = 32;   { Broken pipe }
  ESysEDOM        = 33;   { Math argument out of domain of func }
  ESysERANGE      = 34;   { Math result not representable }


  ESysEDEADLK     = 35;   { Resource deadlock would occur }
  ESysENAMETOOLONG= 36;   { File name too long }
  ESysENOLCK      = 37;   { No record locks available }
  ESysENOSYS      = 38;   { Function not implemented }
  ESysENOTEMPTY= 39;      { Directory not empty }
  ESysELOOP       = 40;   { Too many symbolic links encountered }
  ESysEWOULDBLOCK = ESysEAGAIN;   { Operation would block }
  ESysENOMSG      = 42;   { No message of desired type }
  ESysEIDRM       = 43;   { Identifier removed }
  ESysECHRNG      = 44;   { Channel number out of range }
  ESysEL2NSYNC= 45;       { Level 2 not synchronized }
  ESysEL3HLT      = 46;   { Level 3 halted }
  ESysEL3RST      = 47;   { Level 3 reset }
  ESysELNRNG      = 48;   { Link number out of range }
  ESysEUNATCH     = 49;   { Protocol driver not attached }
  ESysENOCSI      = 50;   { No CSI structure available }
  ESysEL2HLT      = 51;   { Level 2 halted }
  ESysEBADE       = 52;   { Invalid exchange }
  ESysEBADR       = 53;   { Invalid request descriptor }
  ESysEXFULL      = 54;   { Exchange full }
  ESysENOANO      = 55;   { No anode }
  ESysEBADRQC     = 56;   { Invalid request code }
  ESysEBADSLT     = 57;   { Invalid slot }
  ESysEDEADLOCK= 58;      { File locking deadlock error }
  ESysEBFONT      = 59;   { Bad font file format }
  ESysENOSTR      = 60;   { Device not a stream }
  ESysENODATA     = 61;   { No data available }
  ESysETIME       = 62;   { Timer expired }
  ESysENOSR       = 63;   { Out of streams resources }
  ESysENONET      = 64;   { Machine is not on the network }
  ESysENOPKG      = 65;   { Package not installed }
  ESysEREMOTE     = 66;   { Object is remote }
  ESysENOLINK     = 67;   { Link has been severed }
  ESysEADV        = 68;   { Advertise error }
  ESysESRMNT      = 69;   { Srmount error }
  ESysECOMM       = 70;   { Communication error on send }
  ESysEPROTO      = 71;   { Protocol error }
  ESysEMULTIHOP= 72;      { Multihop attempted }
  ESysEDOTDOT     = 73;   { RFS specific error }
  ESysEBADMSG     = 74;   { Not a data message }
  ESysEOVERFLOW= 75;      { Value too large for defined data type }
  ESysENOTUNIQ= 76;       { Name not unique on network }
  ESysEBADFD      = 77;   { File descriptor in bad state }
  ESysEREMCHG     = 78;   { Remote address changed }
  ESysELIBACC     = 79;   { Can not access a needed shared library }
  ESysELIBBAD     = 80;   { Accessing a corrupted shared library }
  ESysELIBSCN     = 81;   { .lib section in a.out corrupted }
  ESysELIBMAX     = 82;   { Attempting to link in too many shared libraries }
  ESysELIBEXEC= 83;       { Cannot exec a shared library directly }
  ESysEILSEQ      = 84;   { Illegal byte sequence }
  ESysERESTART= 85;       { Interrupted system call should be restarted }
  ESysESTRPIPE= 86;       { Streams pipe error }
  ESysEUSERS      = 87;   { Too many users }
  ESysENOTSOCK= 88;       { Socket operation on non-socket }
  ESysEDESTADDRREQ= 89;   { Destination address required }
  ESysEMSGSIZE= 90;       { Message too long }
  ESysEPROTOTYPE= 91;     { Protocol wrong type for socket }
  ESysENOPROTOOPT= 92;    { Protocol not available }
  ESysEPROTONOSUPPORT= 93;        { Protocol not supported }
  ESysESOCKTNOSUPPORT= 94;        { Socket type not supported }
  ESysEOPNOTSUPP= 95;     { Operation not supported on transport endpoint }
  ESysEPFNOSUPPORT= 96;   { Protocol family not supported }
  ESysEAFNOSUPPORT= 97;   { Address family not supported by protocol }
  ESysEADDRINUSE= 98;     { Address already in use }
  ESysEADDRNOTAVAIL= 99;  { Cannot assign requested address }
  ESysENETDOWN= 100;      { Network is down }
  ESysENETUNREACH= 101;   { Network is unreachable }
  ESysENETRESET= 102;     { Network dropped connection because of reset }
  ESysECONNABORTED= 103;  { Software caused connection abort }
  ESysECONNRESET= 104;    { Connection reset by peer }
  ESysENOBUFS     = 105;  { No buffer space available }
  ESysEISCONN     = 106;  { Transport endpoint is already connected }
  ESysENOTCONN= 107;      { Transport endpoint is not connected }
  ESysESHUTDOWN= 108;     { Cannot send after transport endpoint shutdown }
  ESysETOOMANYREFS= 109;  { Too many references: cannot splice }
  ESysETIMEDOUT= 110;     { Connection timed out }
  ESysECONNREFUSED= 111;  { Connection refused }
  ESysEHOSTDOWN= 112;     { Host is down }
  ESysEHOSTUNREACH= 113;  { No route to host }
  ESysEALREADY= 114;      { Operation already in progress }
  ESysEINPROGRESS= 115;   { Operation now in progress }
  ESysESTALE      = 116;  { Stale NFS file handle }
  ESysEUCLEAN     = 117;  { Structure needs cleaning }
  ESysENOTNAM     = 118;  { Not a XENIX named type file }
  ESysENAVAIL     = 119;  { No XENIX semaphores available }
  ESysEISNAM      = 120;  { Is a named type file }
  ESysEREMOTEIO= 121;     { Remote I/O error }
  ESysEDQUOT      = 122;  { Quota exceeded }

  WSAEINTR = ESysEINTR;
  WSAEBADF = ESysEBADF;
  WSAEACCES = ESysEACCES;
  WSAEFAULT = ESysEFAULT;
  WSAEINVAL = ESysEINVAL;
  WSAEMFILE = ESysEMFILE;
  WSAEWOULDBLOCK = ESysEWOULDBLOCK;
  WSAEINPROGRESS = ESysEINPROGRESS;
  WSAEALREADY = ESysEALREADY;
  WSAENOTSOCK = ESysENOTSOCK;
  WSAEDESTADDRREQ = ESysEDESTADDRREQ;
  WSAEMSGSIZE = ESysEMSGSIZE;
  WSAEPROTOTYPE = ESysEPROTOTYPE;
  WSAENOPROTOOPT = ESysENOPROTOOPT;
  WSAEPROTONOSUPPORT = ESysEPROTONOSUPPORT;
  WSAESOCKTNOSUPPORT = ESysESOCKTNOSUPPORT;
  WSAEOPNOTSUPP = ESysEOPNOTSUPP;
  WSAEPFNOSUPPORT = ESysEPFNOSUPPORT;
  WSAEAFNOSUPPORT = ESysEAFNOSUPPORT;
  WSAEADDRINUSE = ESysEADDRINUSE;
  WSAEADDRNOTAVAIL = ESysEADDRNOTAVAIL;
  WSAENETDOWN = ESysENETDOWN;
  WSAENETUNREACH = ESysENETUNREACH;
  WSAENETRESET = ESysENETRESET;
  WSAECONNABORTED = ESysECONNABORTED;
  WSAECONNRESET = ESysECONNRESET;
  WSAENOBUFS = ESysENOBUFS;
  WSAEISCONN = ESysEISCONN;
  WSAENOTCONN = ESysENOTCONN;
  WSAESHUTDOWN = ESysESHUTDOWN;
  WSAETOOMANYREFS = ESysETOOMANYREFS;
  WSAETIMEDOUT = ESysETIMEDOUT;
  WSAECONNREFUSED = ESysECONNREFUSED;
  WSAELOOP = ESysELOOP;
  WSAENAMETOOLONG = ESysENAMETOOLONG;
  WSAEHOSTDOWN = ESysEHOSTDOWN;
  WSAEHOSTUNREACH = ESysEHOSTUNREACH;
  WSAENOTEMPTY = ESysENOTEMPTY;
  WSAEPROCLIM = -1;
  WSAEUSERS = ESysEUSERS;
  WSAEDQUOT = ESysEDQUOT;
  WSAESTALE = ESysESTALE;
  WSAEREMOTE = ESysEREMOTE;
  WSASYSNOTREADY = -2;
  WSAVERNOTSUPPORTED = -3;
  WSANOTINITIALISED = -4;
  WSAEDISCON = -5;
  WSAHOST_NOT_FOUND = 1;
  WSATRY_AGAIN = 2;
  WSANO_RECOVERY = 3;
  WSANO_DATA = -6;

  EAI_BADFLAGS    = -1;   { Invalid value for `ai_flags' field.  }
  EAI_NONAME      = -2;   { NAME or SERVICE is unknown.  }
  EAI_AGAIN       = -3;   { Temporary failure in name resolution.  }
  EAI_FAIL        = -4;   { Non-recoverable failure in name res.  }
  EAI_NODATA      = -5;   { No address associated with NAME.  }
  EAI_FAMILY      = -6;   { `ai_family' not supported.  }
  EAI_SOCKTYPE    = -7;   { `ai_socktype' not supported.  }
  EAI_SERVICE     = -8;   { SERVICE not supported for `ai_socktype'.  }
  EAI_ADDRFAMILY  = -9;   { Address family for NAME not supported.  }
  EAI_MEMORY      = -10;  { Memory allocation failure.  }
  EAI_SYSTEM      = -11;  { System error returned in `errno'.  }

type
  u_char = AnsiChar;
  pu_long = ^u_long;
  pu_short = ^u_short;
  TAddrFamily = integer;

  DWORD = Integer;

  PFDSet = ^TFDSet;

  PTimeVal = ^TTimeVal;
  TTimeVal = packed record
    tv_sec: Longint;
    tv_usec: Longint;
  end;

  PSockAddrIn = ^TSockAddrIn;
  TSockAddrIn = packed record
    case Integer of
      0: (sin_family: u_short;
          sin_port: u_short;
          sin_addr: TInAddr;
          sin_zero: array[0..7] of AnsiChar);
      1: (sa_family: u_short;
          sa_data: array[0..13] of AnsiChar)
  end;

  TIP_mreq =  record
    imr_multiaddr: TInAddr;     { IP multicast address of group }
    imr_interface: TInAddr;     { local IP address of interface }
  end;

  PSockAddrIn6 = ^TSockAddrIn6;
  TSockAddrIn6 = packed record
		sin6_family:   u_short;     // AF_INET6
		sin6_port:     u_short;     // Transport level port number
		sin6_flowinfo: u_long;	    // IPv6 flow information
		sin6_addr:     TInAddr6;    // IPv6 address
		sin6_scope_id: u_long;      // Scope Id: IF number for link-local
                                //           SITE id for site-local
  end;

  TIPv6_mreq = record
    ipv6mr_multiaddr: TInAddr6; // IPv6 multicast address.
    ipv6mr_interface: integer;   // Interface index.
    padding: u_long;
  end;

  PHostEnt = ^THostEnt;
  THostent = record
    h_name: PAnsiChar;
    h_aliases: PPChar;
    h_addrtype: Integer;
    h_length: Cardinal;
    case Byte of
      0: (h_addr_list: PPChar);
      1: (h_addr: PPChar);
  end;

  PNetEnt = ^TNetEnt;
  TNetEnt = record
    n_name: PAnsiChar;
    n_aliases: PPChar;
    n_addrtype: Integer;
    n_net: u_long;
  end;

  PServEnt = ^TServEnt;
  TServEnt = record
    s_name: PAnsiChar;
    s_aliases: PPChar;
    s_port: Integer;
    s_proto: PAnsiChar;
  end;

  PProtoEnt = ^TProtoEnt;
  TProtoEnt = record
    p_name: PAnsiChar;
    p_aliases: ^PAnsiChar;
    p_proto: u_short;
  end;

  { Structure used by kernel to store most addresses. }
  PSockAddr = ^TSockAddr;

  { Structure used by kernel to pass protocol information in raw sockets. }
  PSockProto = ^TSockProto;
  TSockProto = packed record
    sp_family: u_short;
    sp_protocol: u_short;
  end;

  PAddrInfo = ^TAddrInfo;
  TAddrInfo = record
                ai_flags: integer;    // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST.
                ai_family: integer;   // PF_xxx.
                ai_socktype: integer; // SOCK_xxx.
                ai_protocol: integer; // 0 or IPPROTO_xxx for IPv4 and IPv6.
                ai_addrlen: u_int;    // Length of ai_addr.
                ai_addr: PSockAddr;   // Binary address.
                ai_canonname: PAnsiChar;  // Canonical name for nodename.
                ai_next: PAddrInfo;     // Next structure in linked list.
              end;

{ Structure used for manipulating linger option. }
  PLinger = ^TLinger;
  TLinger = packed record
    l_onoff: integer;
    l_linger: integer;
  end;

type
  TWSAGetLastError = function: Integer; cdecl;
  TGetServByName = function(name, proto: PAnsiChar): PServEnt; cdecl;
  TGetServByPort = function(port: Integer; proto: PAnsiChar): PServEnt; cdecl;
  TGetProtoByName = function(name: PAnsiChar): PProtoEnt; cdecl;
  TGetProtoByNumber = function(proto: Integer): PProtoEnt; cdecl;
  TGetHostByName = function(name: PAnsiChar): PHostEnt; cdecl;
  TGetHostByAddr = function(addr: Pointer; len, Struc: Integer): PHostEnt; cdecl;
  TGetHostName = function(name: PAnsiChar; len: Integer): Integer; cdecl;
  TShutdown = function(s: TSocket; how: Integer): Integer; cdecl;
  TSetSockOpt = function(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; cdecl;
  TGetSockOpt = function(s: TSocket; level, optname: Integer; optval: PAnsiChar; var optlen: Integer): Integer; cdecl;
  TSendTo = function(s: TSocket; const Buf; len, flags: Integer; addrto: PSockAddr; tolen: Integer): Integer; cdecl;
  TSend = function(s: TSocket; const Buf; len, flags: Integer): Integer; cdecl;
  TRecv = function(s: TSocket; var Buf; len, flags: Integer): Integer; cdecl;
  TRecvFrom = function(s: TSocket; var Buf; len, flags: Integer; from: PSockAddr; var fromlen: Integer): Integer; cdecl;
  Tntohs = function(netshort: u_short): u_short; cdecl;
  Tntohl = function(netlong: u_long): u_long; cdecl;
  TListen = function(s: TSocket; backlog: Integer): Integer; cdecl;
  TIoctlSocket = function(s: TSocket; cmd: DWORD; var arg: integer): Integer; cdecl;
  TInet_ntoa = function(inaddr: TInAddr): PAnsiChar; cdecl;
  TInet_addr = function(cp: PAnsiChar): u_long; cdecl;
  Thtons = function(hostshort: u_short): u_short; cdecl;
  Thtonl = function(hostlong: u_long): u_long; cdecl;
  TGetSockName = function(s: TSocket; name: PSockAddr; var namelen: Integer): Integer; cdecl;
  TGetPeerName = function(s: TSocket; name: PSockAddr; var namelen: Integer): Integer; cdecl;
  TConnect = function(s: TSocket; name: PSockAddr; namelen: Integer): Integer; cdecl;
  TCloseSocket = function(s: TSocket): Integer; cdecl;
  TBind = function(s: TSocket; addr: PSockAddr; namelen: Integer): Integer; cdecl;
  TAccept = function(s: TSocket; addr: PSockAddr; var addrlen: Integer): TSocket; cdecl;
  TTSocket = function(af, Struc, Protocol: Integer): TSocket; cdecl;
  TSelect = function(nfds: Integer; readfds, writefds, exceptfds: PFDSet; timeout: PTimeVal): Longint; cdecl;
  TSignal = function(s: Integer; v: Integer): Longint; cdecl;
  TStrError = function(code: Integer): PAnsiChar; cdecl;

  TGetAddrInfo = function(NodeName: PAnsiChar; ServName: PAnsiChar; Hints: PAddrInfo; var Addrinfo: PAddrInfo): integer; cdecl;
  TFreeAddrInfo = procedure(ai: PAddrInfo); cdecl;
  TGetNameInfo = function( addr: PSockAddr; namelen: Integer; host: PAnsiChar; hostlen: DWORD; serv: PAnsiChar; servlen: DWORD; flags: integer): integer; cdecl;

var
  WSA_SetSockOpt: TSetSockOpt = nil;
  WSA_Send: TSend = nil;
  WSA_Recv: TRecv = nil;
  WSA_Listen: TListen = nil;
  WSA_IoCtlSocket: TIoctlSocket = nil;
  WSA_CloseSocket: TCloseSocket = nil;
  WSA_Socket: TTSocket = nil;
  WSA_Select: TSelect = nil;
  WSA_Shutdown: TShutdown = nil;
  WSA_Signal: TSignal = nil;
  WSA_StrError: TStrError = nil;

function WSA_GetLastError: Integer;

function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6): boolean;
procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);

function SizeOfVarSin(sin: TSockAddr): integer;

function __FDELT(Socket: TSocket): Integer;
function __FDMASK(Socket: TSocket): __fd_mask;
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
procedure FD_ZERO(var FDSet: TFDSet);

function WSA_Bind(s: TSocket; const addr: TSockAddr): Integer;
function WSA_Connect(s: TSocket; const name: TSockAddr): Integer;

function WSA_GetSockName(s: TSocket; var name: TSockAddr; PreferIP4: Boolean): Integer;
function WSA_GetPeerName(s: TSocket; var name: TSockAddr; PreferIP4: Boolean): Integer;

function WSA_Accept(s: TSocket; var addr: TSockAddr): TSocket;
function WSA_SetVarSin(var Sin: TSockAddr; IP, Port: AnsiString; Family, SockProtocol, SockType: integer; PreferIP4, PreferIPDef: Boolean): integer;

function WSA_GetSinIP(Sin: TSockAddr): AnsiString;
function WSA_GetSinPort(Sin: TSockAddr): Integer;

function WSA_ErrorDesc(ErrCode:integer):String;

procedure LoadNixSock;
procedure UnloadNixSock;

{$ENDIF} // {$IFDEF RTC_NIX_SOCK}

implementation

{$IFDEF RTC_NIX_SOCK}

var
  ssntohs: Tntohs = nil;
  ssInet_ntoa: TInet_ntoa = nil;
  ssInet_addr: TInet_addr = nil;
  sshtons: Thtons = nil;
  ssGetAddrInfo: TGetAddrInfo = nil;
  ssFreeAddrInfo: TFreeAddrInfo = nil;
  ssGetNameInfo: TGetNameInfo = nil;
  ssGetServByName: TGetServByName = nil;
  ssGetProtoByNumber: TGetProtoByNumber = nil;
  ssGetHostByName: TGetHostByName = nil;
  ssBind: TBind = nil;
  ssAccept: TAccept = nil;
  ssGetSockName: TGetSockName = nil;
  ssGetPeerName: TGetPeerName = nil;
  ssConnect: TConnect = nil;
(*
  ssGetHostName: TGetHostName = nil;
  ssSendTo: TSendTo = nil;
  ssRecvFrom: TRecvFrom = nil;
  ssGetHostByAddr: TGetHostByAddr = nil;

  xxIoctl: TWSAIoctl = nil;
  xxGetServByPort: TGetServByPort = nil;
  xxGetProtoByName: TGetProtoByName = nil;
  xxGetSockOpt: TGetSockOpt = nil;
  xxntohl: Tntohl = nil;
  xxhtonl: Thtonl = nil;
*)

  SynSockCS: SyncObjs.TCriticalSection;
  SockEnhancedApi: Boolean;
  SockWship6Api: Boolean;
  FSynLoaded:boolean=False;
  LibHandle: THandle = 0;
  Libwship6Handle: THandle = 0;
//  WSA_DataOnce: TWSADATA;

var
  errno_loc: function: PInteger cdecl = nil;

function WSA_GetLastError: Integer;
  var
    p: PInteger;
  begin
  if assigned(errno_loc) then
    begin
    p := errno_loc;
    Result := p^;
    end
  else
    Result := 0;
  end;

function IsNewApi(Family: integer): Boolean;
  begin
  Result := SockEnhancedApi;
  if not Result then
    Result := (Family = AF_INET6) and SockWship6Api;
  end;

function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
begin
  Result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and (a^.u6_addr32[3] = 0));
end;

function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
begin
  Result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and
             (a^.u6_addr8[12] = 0) and (a^.u6_addr8[13] = 0) and
             (a^.u6_addr8[14] = 0) and (a^.u6_addr8[15] = 1));
end;

function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
begin
  Result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $80));
end;

function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
begin
  Result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $C0));
end;

function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
begin
  Result := (a^.u6_addr8[0] = $FF);
end;

function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6): boolean;
begin
  Result := (CompareMem( a, b, sizeof(TInAddr6)));
end;

procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
end;

procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
  a^.u6_addr8[15] := 1;
end;

function SizeOfVarSin(sin: TSockAddr): integer;
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

function __FDELT(Socket: TSocket): Integer;
begin
  Result := Socket div __NFDBITS;
end;

function __FDMASK(Socket: TSocket): __fd_mask;
begin
  Result := Cardinal(1) shl (Socket mod __NFDBITS);
end;

procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
  begin
  fdset.fds_bits[__FDELT(Socket)] := fdset.fds_bits[__FDELT(Socket)] or __FDMASK(Socket);
  end;

procedure FD_ZERO(var FDSet: TFDSet);
  var
    I: Integer;
  begin
  with fdset do
    for I := Low(fds_bits) to High(fds_bits) do
      fds_bits[I] := 0;
  end;

function WSA_Bind(s: TSocket; const addr: TSockAddr): Integer;
  begin
  Result := ssBind(s, @addr, SizeOfVarSin(addr));
  end;

function WSA_Connect(s: TSocket; const name: TSockAddr): Integer;
  begin
  Result := ssConnect(s, @name, SizeOfVarSin(name));
  end;

function WSA_GetSockName(s: TSocket; var name: TSockAddr; PreferIP4: Boolean): Integer;
  var
    len: integer;
  begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := ssGetSockName(s, @name, Len);
  if name.sin_family>AF_MAX then
    if PreferIP4 then
      name.sin_family:=AF_INET
    else
      name.sin_family:=AF_INET6;
  end;

function WSA_GetPeerName(s: TSocket; var name: TSockAddr; PreferIP4: Boolean): Integer;
  var
    len: integer;
  begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := ssGetPeerName(s, @name, Len);
  if name.sin_family>AF_MAX then
    if PreferIP4 then
      name.sin_family:=AF_INET
    else
      name.sin_family:=AF_INET6;
  end;

function WSA_Accept(s: TSocket; var addr: TSockAddr): TSocket;
  var
    x: integer;
  begin
  x := SizeOf(addr);
  Result := ssAccept(s, @addr, x);
  end;

function WSA_SetVarSin(var Sin: TSockAddr; IP, Port: AnsiString; Family, SockProtocol, SockType: integer; PreferIP4, PreferIPDef: Boolean): integer;
type
  pu_long = ^u_long;
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  HostEnt: PHostEnt;
  Hints1, Hints2, Hints3: TAddrInfo;
  TwoPass: boolean;

  function GetAddr(Hints: TAddrInfo): integer;
  var
    Addr, NextAddr: PAddrInfo;
  begin
    Addr := nil;
    try
      FillChar(Sin, Sizeof(Sin), 0);
      if Hints.ai_socktype = SOCK_RAW then
      begin
        Hints.ai_socktype := 0;
        Hints.ai_protocol := 0;
        Result := ssGetAddrInfo(PAnsiChar(IP), nil, @Hints, Addr);
      end
      else
      begin
        if (IP = '') or (IP = cAnyHost) or (IP = c6AnyHost) then
        begin
          Hints.ai_flags := AI_PASSIVE;
          Result := ssGetAddrInfo(nil, PAnsiChar(Port), @Hints, Addr);
        end
        else
          if (IP = cLocalhost) or (IP = c6Localhost) or (IP = cLocalHostStr) then
          begin
            Result := ssGetAddrInfo(nil, PAnsiChar(Port), @Hints, Addr);
          end
          else
          begin
            Result := ssGetAddrInfo(PAnsiChar(IP), PAnsiChar(Port), @Hints, Addr);
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
          Result := -1;
    finally
      if Assigned(Addr) then
        ssFreeAddrInfo(Addr);
    end;
  end;

begin
  Result := 0;
  FillChar(Sin, Sizeof(Sin), 0);
    SynSockCS.Enter;
    try
      Sin.sin_family := AF_INET;
      ProtoEnt := ssGetProtoByNumber(SockProtocol);
      ServEnt := nil;
      if ProtoEnt <> nil then
        ServEnt := ssGetServByName(PAnsiChar(Port), ProtoEnt^.p_name);
      if ServEnt = nil then
        Sin.sin_port := sshtons(Str2Int(Port))
      else
        Sin.sin_port := ServEnt^.s_port;
      if IP = cBroadcast then
        Sin.sin_addr.s_addr := u_long(INADDR_BROADCAST)
      else
      begin
        Sin.sin_addr.s_addr := ssinet_addr(PAnsiChar(IP));
        if Sin.sin_addr.s_addr = u_long(INADDR_NONE) then
        begin
          if IsNewApi(family) then
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

            Result := GetAddr(Hints1);
            if Result <> 0 then
              if TwoPass then
                begin
                Hints2.ai_socktype := SockType;
                Hints2.ai_protocol := SockProtocol;
                Result := GetAddr(Hints2);
                if Result <> 0 then
                  begin
                  Hints3.ai_socktype := SockType;
                  Hints3.ai_protocol := SockProtocol;
                  Result := GetAddr(Hints3);
                  end;
                end;
            end
          else
            begin
            HostEnt := ssGetHostByName(PAnsiChar(IP));
            Result := WSA_GetLastError;
            if HostEnt <> nil then
              Sin.sin_addr.S_addr := u_long(Pu_long(HostEnt^.h_addr_list^)^);
            end;
        end;
      end;
    finally
      SynSockCS.Leave;
    end;
end;

function WSA_GetSinIP(Sin: TSockAddr): AnsiString;
var
  p: PAnsiChar;
  host, serv: AnsiString;
  hostlen, servlen: integer;
  r: integer;
begin
  Result := '';
  p := ssinet_ntoa(Sin.sin_addr);
  if p <> nil then
    Result := p
  else if IsNewApi(Sin.AddressFamily) then
    begin
    hostlen := NI_MAXHOST;
    servlen := NI_MAXSERV;
    setlength(host, hostlen);
    setlength(serv, servlen);
    r := ssGetNameInfo(@sin, SizeOfVarSin(sin), PAnsiChar(host), hostlen,
      PAnsiChar(serv), servlen, NI_NUMERICHOST + NI_NUMERICSERV);
    if r = 0 then
      Result := PAnsiChar(host);
    end;
end;

function WSA_GetSinPort(Sin: TSockAddr): Integer;
  begin
  if (Sin.sin_family = AF_INET6) then
    Result := ssntohs(Sin.sin6_port)
  else
    Result := ssntohs(Sin.sin_port);
  end;

function WSA_InitSocketInterface: Boolean;
  begin
  Result := False;
  SockEnhancedApi := False;
  SockWship6Api := False;
  LibHandle := LoadLibrary(DLLStackName);
  if LibHandle = 0 then
    LibHandle := LoadLibrary(DLLStackName2);
  if LibHandle <> 0 then
    begin
    WSA_Signal := GetProcAddress(LibHandle, PChar('signal'));
    if assigned(WSA_Signal) then
      WSA_Signal(SIGPIPE,SIG_IGN); // Install signal handler for SIGPIPE to avoid termination on socket errors

    WSA_StrError := GetProcAddress(LibHandle, PChar('strerror'));

    WSA_CloseSocket := GetProcAddress(LibHandle, PChar('close'));
    WSA_IoctlSocket := GetProcAddress(LibHandle, PChar('ioctl'));
    WSA_Listen := GetProcAddress(LibHandle, PChar('listen'));
    WSA_Recv := GetProcAddress(LibHandle, PChar('recv'));
    WSA_Select := GetProcAddress(LibHandle, PChar('select'));
    WSA_Send := GetProcAddress(LibHandle, PChar('send'));
    WSA_SetSockOpt := GetProcAddress(LibHandle, PChar('setsockopt'));
    WSA_Socket := GetProcAddress(LibHandle, PChar('socket'));
    WSA_ShutDown := GetProcAddress(LibHandle, PChar('shutdown'));

    errno_loc := GetProcAddress(LibHandle, PChar('__error'));
    if not assigned(errno_loc) then
      errno_loc := GetProcAddress(LibHandle, PChar('__errno_location'));

    ssAccept := GetProcAddress(LibHandle, PChar('accept'));
    ssBind := GetProcAddress(LibHandle, PChar('bind'));
    ssConnect := GetProcAddress(LibHandle, PChar('connect'));
    ssGetPeerName := GetProcAddress(LibHandle, PChar('getpeername'));
    ssGetSockName := GetProcAddress(LibHandle, PChar('getsockname'));
    ssHtons := GetProcAddress(LibHandle, PChar('htons'));
    ssInet_Addr := GetProcAddress(LibHandle, PChar('inet_addr'));
    ssInet_Ntoa := GetProcAddress(LibHandle, PChar('inet_ntoa'));
    ssNtohs := GetProcAddress(LibHandle, PChar('ntohs'));
    ssGetProtoByNumber := GetProcAddress(LibHandle, PChar('getprotobynumber'));
    ssGetServByName := GetProcAddress(LibHandle, PChar('getservbyname'));

  (*
    ssRecvFrom := GetProcAddress(LibHandle, PChar('recvfrom'));
    ssSendTo := GetProcAddress(LibHandle, PChar('sendto'));
    ssGetHostByAddr := GetProcAddress(LibHandle, PChar('gethostbyaddr'));
    ssGetHostByName := GetProcAddress(LibHandle, PChar('gethostbyname'));
    ssGetHostName := GetProcAddress(LibHandle, PChar('gethostname'));

    xxGetServByPort := GetProcAddress(LibHandle, PChar('getservbyport'));
    xxGetProtoByName := GetProcAddress(LibHandle, PChar('getprotobyname'));
    xxHtonl := GetProcAddress(LibHandle, PChar('htonl'));
    xxNtohl := GetProcAddress(LibHandle, PChar('ntohl'));
    xxGetSockOpt := GetProcAddress(LibHandle, PChar('getsockopt'));
  *)

    ssGetAddrInfo := GetProcAddress(LibHandle, PChar('getaddrinfo'));
    ssFreeAddrInfo := GetProcAddress(LibHandle, PChar('freeaddrinfo'));
    ssGetNameInfo := GetProcAddress(LibHandle, PChar('getnameinfo'));
    SockEnhancedApi := Assigned(ssGetAddrInfo) and Assigned(ssFreeAddrInfo) and Assigned(ssGetNameInfo);

    Result := True;
    end;
  end;

function WSA_DestroySocketInterface: Boolean;
  begin
  if LibHandle <> 0 then
    begin
    FreeLibrary(libHandle);
    LibHandle := 0;
    end;
  if LibWship6Handle <> 0 then
    begin
    FreeLibrary(LibWship6Handle);
    LibWship6Handle := 0;
    end;
  Result := True;
  end;

function WSA_ErrorDesc(ErrCode:integer):String;
  var
    res:PAnsiChar;
  begin
  if ErrCode=0 then
    begin
    Result:='';
    Exit;
    end;

  if assigned(WSA_StrError) then
    begin
    res := WSA_StrError(ErrCode);
    if assigned(res) then
      begin
      Result := res;
      res := nil;
      end
    else
      Result:='';
    end
  else
    Result:='';

  if Result='' then
    case ErrCode of
      WSAEINTR: {10004}
        Result := 'Interrupted system call';
      WSAEBADF: {10009}
        Result := 'Bad file number';
      WSAEACCES: {10013}
        Result := 'Permission denied';
      WSAEFAULT: {10014}
        Result := 'Bad address';
      WSAEINVAL: {10022}
        Result := 'Invalid argument';
      WSAEMFILE: {10024}
        Result := 'Too many open files';
      WSAEWOULDBLOCK: {10035}
        Result := 'Operation would block';
      WSAEINPROGRESS: {10036}
        Result := 'Operation now in progress';
      WSAEALREADY: {10037}
        Result := 'Operation already in progress';
      WSAENOTSOCK: {10038}
        Result := 'Socket operation on nonsocket';
      WSAEDESTADDRREQ: {10039}
        Result := 'Destination address required';
      WSAEMSGSIZE: {10040}
        Result := 'Message too long';
      WSAEPROTOTYPE: {10041}
        Result := 'Protocol wrong type for Socket';
      WSAENOPROTOOPT: {10042}
        Result := 'Protocol not available';
      WSAEPROTONOSUPPORT: {10043}
        Result := 'Protocol not supported';
      WSAESOCKTNOSUPPORT: {10044}
        Result := 'Socket not supported';
      WSAEOPNOTSUPP: {10045}
        Result := 'Operation not supported on Socket';
      WSAEPFNOSUPPORT: {10046}
        Result := 'Protocol family not supported';
      WSAEAFNOSUPPORT: {10047}
        Result := 'Address family not supported';
      WSAEADDRINUSE: {10048}
        Result := 'Address already in use';
      WSAEADDRNOTAVAIL: {10049}
        Result := 'Can''t assign requested address';
      WSAENETDOWN: {10050}
        Result := 'Network is down';
      WSAENETUNREACH: {10051}
        Result := 'Network is unreachable';
      WSAENETRESET: {10052}
        Result := 'Network dropped connection on reset';
      WSAECONNABORTED: {10053}
        Result := 'Software caused connection abort';
      WSAECONNRESET: {10054}
        Result := 'Connection reset by peer';
      WSAENOBUFS: {10055}
        Result := 'No Buffer space available';
      WSAEISCONN: {10056}
        Result := 'Socket is already connected';
      WSAENOTCONN: {10057}
        Result := 'Socket is not connected';
      WSAESHUTDOWN: {10058}
        Result := 'Can''t send after Socket shutdown';
      WSAETOOMANYREFS: {10059}
        Result := 'Too many references:can''t splice';
      WSAETIMEDOUT: {10060}
        Result := 'Connection timed out';
      WSAECONNREFUSED: {10061}
        Result := 'Connection refused';
      WSAELOOP: {10062}
        Result := 'Too many levels of symbolic links';
      WSAENAMETOOLONG: {10063}
        Result := 'File name is too long';
      WSAEHOSTDOWN: {10064}
        Result := 'Host is down';
      WSAEHOSTUNREACH: {10065}
        Result := 'No route to host';
      WSAENOTEMPTY: {10066}
        Result := 'Directory is not empty';
      WSAEPROCLIM: {10067}
        Result := 'Too many processes';
      WSAEUSERS: {10068}
        Result := 'Too many users';
      WSAEDQUOT: {10069}
        Result := 'Disk quota exceeded';
      WSAESTALE: {10070}
        Result := 'Stale NFS file handle';
      WSAEREMOTE: {10071}
        Result := 'Too many levels of remote in path';
      WSASYSNOTREADY: {10091}
        Result := 'Network subsystem is unusable';
      WSAVERNOTSUPPORTED: {10092}
        Result := 'Winsock DLL cannot support this application';
      WSANOTINITIALISED: {10093}
        Result := 'Winsock not initialized';
      WSAEDISCON: {10101}
        Result := 'Disconnect';
      WSAHOST_NOT_FOUND: {11001}
        Result := 'Host not found';
      WSATRY_AGAIN: {11002}
        Result := 'Non authoritative - host not found';
      WSANO_RECOVERY: {11003}
        Result := 'Non recoverable error';
      WSANO_DATA: {11004}
        Result := 'Valid name, no data record of requested type'
    else
        Result := 'Unknown Socket error #' + IntToStr(ErrCode);
      end;
  end;

procedure LoadNixSock;
  begin
  if not assigned(SynSockCS) then
    raise Exception.Create('Application terminating.');
  SynSockCS.Acquire;
  try
    if not FSynLoaded then
      if WSA_InitSocketInterface then
        FSynLoaded:=True
      else
        raise Exception.Create('Error loding Socket Library');
  finally
    SynSockCS.Release;
    end;
  end;

procedure UnloadNixSock;
  begin
  if not assigned(SynSockCS) then Exit;
  SynSockCS.Acquire;
  try
    if FSynLoaded then
      begin
      FSynLoaded:=False;
      WSA_DestroySocketInterface;
      end;
  finally
    SynSockCS.Release;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcNixSock Initializing ...','DEBUG');{$ENDIF}
  SynSockCS := SyncObjs.TCriticalSection.Create;

  SET_IN6_IF_ADDR_ANY (@in6addr_any);
  SET_LOOPBACK_ADDR6  (@in6addr_loopback);
{$IFDEF RTC_DEBUG} Log('rtcNixSock Initialized.','DEBUG');{$ENDIF}

finalization
{$IFDEF RTC_DEBUG} Log('rtcNixSock Finalizing ...','DEBUG');{$ENDIF}
  UnLoadNixSock;

  RtcFreeAndNil(SynSockCS);
{$IFDEF RTC_DEBUG} Log('rtcNixSock Finalized.','DEBUG');{$ENDIF}

{$ENDIF} // {$IFDEF RTC_NIX_SOCK}
end.
