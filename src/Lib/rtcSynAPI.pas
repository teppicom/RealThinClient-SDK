{
  RealThinClient SDK: Platform-independent Synchronous Socket API class
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)

  @exclude
}
unit rtcSynAPI;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcSystem,

{$IFDEF WINDOWS} //Win32 and Win64
  rtcWinSock;

type
  TAPISockAddr = TSockAddr;

{$ELSE}{$IFDEF POSIX} // Mac OSX
  Classes,
  Posix.Errno,
  Posix.Base, Posix.SysSocket, Posix.SysSelect,
  Posix.ArpaInet, Posix.NetinetIn, Posix.NetDB,
  Posix.Unistd, Posix.SysTime; // , PosixStrOpts;

type
  TSocket = integer;

  TSockAddrIn = sockaddr_in;
  TSockAddrIn6 = sockaddr_in6;
  TSockAddr = record
    case integer of
      0: ({$IFDEF MACOS} sa_len: UInt8; {$ENDIF}
          sa_family: sa_family_t;
          sa_port: in_port_t);
      1: (sin:TSockAddrIn);
      2: (sin6:TSockAddrIn6);
    end;

  TAPISockAddr = sockaddr;

  TFDSet = fd_set;

{$ELSE}{$IFDEF RTC_NIX_SOCK} // iOS (iPhone + iPad) on FPC
  rtcNixSock;

type
  TAPISockAddr = TSockAddr;

{$ELSE} // Anything else
  Classes,
  BaseUnix,
  Unix,
  termio,
  sockets,
  netdb;

type
  TAPISockAddr = TSockAddr;

{$ENDIF}{$ENDIF}{$ENDIF}

type
  TRtcSocket=class
  private
    FOSocket,
    FSocket: TSocket;
    NewSockID: TSocket;
    FFDSet: TFDSet;
    FErr: String;
    FErrCode: Integer;
    FSin, FLocalSin, FRemoteSin: TSockAddr;

  {$IFDEF WINDOWS}
    FLocalSinLen, FRemoteSinLen: integer;
  {$ELSE}{$IFDEF POSIX}
    FLocalSinLen, FRemoteSinLen: socklen_t;
    FTempBuffer: RtcByteArray;
  {$ELSE}{$IFDEF RTC_NIX_SOCK}
    FTempBuffer: RtcByteArray;
  {$ENDIF}{$ENDIF}{$ENDIF}

    procedure Sock_SetSin(var Sin: TSockAddr; const vAddr,vPort:RtcString; PreferIP4, PreferIPDef:boolean);
    procedure Sock_CreateSocket(Sin: TSockAddr);
    procedure Sock_SetLinger(vEnable: Boolean; vLinger: Integer);
    procedure Sock_SetDelay(reuseAddr:boolean);
    procedure Sock_SetTimeouts(const TOA:TRtcTimeoutsOfAPI);

    procedure Sock_Connect(const vAddr,vPort: RtcString; const TOA:TRtcTimeoutsOfAPI; PreferIP4, PreferIPDef:boolean);
    procedure Sock_Listen(const vAddr,vPort: RtcString; const TOA:TRtcTimeoutsOfAPI; PreferIP4, PreferIPDef:boolean);

    function Sock_Accept:TSocket;
    procedure Sock_SetSocket(sid:TSocket; PreferIP4:boolean);

    function Sock_Shutdown:boolean;
    function Sock_Close:boolean;

    function Sock_Invalid:boolean;
    function Sock_CheckError:boolean;
    function Sock_Err(res:Integer):boolean;

    procedure Sock_ResetError;
    procedure Sock_CheckLastError;
    procedure Sock_CheckLastErrorDesc;

    function Sock_GetConnInfo:String;
    function Sock_GetLocalSinIP:RtcString;
    function Sock_GetLocalSinPort:RtcString;
    function Sock_GetRemoteSinIP:RtcString;
    function Sock_GetRemoteSinPort:RtcString;

    function Sock_WaitingData:integer;
    function Sock_RecvBuffer(var Buffer; Len: Integer): Integer;
    function Sock_SendBuffer(var Buffer; Len: Integer): Integer;

    function Sock_CanRead(vTimeout:integer):boolean;
    function Sock_CanWrite(vTimeout:integer):boolean;

  public
    { Constructor }
    constructor Create;

    { Destructor }
    destructor Destroy; override;

    { Start using this socket as Server listener,
      listening on port "FPort", bound to local network addapter "FAddr".
      Leave "FAddr" empty to listen on all network addapters.
      TOA can be passed as parameter to set Timeouts on API.
      Send NIL as TOA parameter to use default timeout values.
      Returns TRUE if success, FALSE if error. }
    function Listen(const FAddr,FPort:RtcString; const TOA:TRtcTimeoutsOfAPI; PreferIP4, PreferIPDef:boolean):boolean;

    { Connect to address "FAddr" on port "FPort".
      TOA can be passed as parameter to set Timeouts on API.
      Send NIL as TOA parameter to use default timeout values.
      Returns TRUE if success, FALSE if error. }
    function Connect(const FAddr,FPort:RtcString; const TOA:TRtcTimeoutsOfAPI; PreferIP4, PreferIPDef:boolean): boolean;

    { Shut down input and output connection, preparing for close.
      Returns TRUE if success, FALSE if conneciton was already closed. }
    function Shut_down:boolean;

    { Close socket connection.
      If it was a Listening socket, listener is closed but NOT connected clients.
      If it was a Client socket, it will be disconnected from Server.
      Returns TRUE if success, FALSE if error. }
    function Close: boolean;

    { Check if there are new client sockets waiting to be accepted.
      Returns the number of waiting sockets if there are sockets waiting,
      0 if no sockets were waiting after "vTimeout" (ms), or -1 if error. }
    function WaitingSockets(vTimeout:integer): integer;

    { After WaitingSockets has returned a positive result,
      use GetNewSocket to accept one socket and receive
      a new TRtcSocket component for the new socket.
      Returns a new TRtcSocket object if OK, or NIL if error. }
    function GetNewSocket: TRtcSocket;

    { Has to be called on a socket received from GetNewSocket
      before the new TRtcSocket component can be used.
      Returns TRUE is the socket can be used, FALSE if error. }
    function StartNewSocket(PreferIP4:boolean): boolean;

    { Try to receive data from the other side.
      If data is available, will read as much as can be read without blocking.
      If no data is available, will wait up to "vTimeout" ms for new data.
      If no data after "vTimeout", returns TRUE and an empty string in "Str".
      If data was read, "Str" will contain the data received, result will be TRUE.
      If there was an error, "Str" will be empty and FALSE will be returned as Result. }
    function ReceiveEx(var Str: RtcByteArray; vTimeout:integer): boolean;

    { Try to send as much data from "Str" starting at character location "at"
      as possible without blocking. If can not read (buffer full), will wait
      up to "vTimeout" ms to be able to send at least something. If something
      was sent, will return the number of characters (bytes) sent (to buffer).
      If can not send yet but connection seems OK, will return 0.
      If connection is not working anymore, will return -1. }
    function SendEx(var Str: RtcByteArray; at: integer; vTimeout:integer): Integer;

    { If any of the methods of this class returns FALSE or -1,
      signaling that there was an error, GetLastErrorText will
      return a more-or-less descriptive error message (text). }
    function GetLastErrorText: String;

    { Local Address to which this socket is connected. }
    function GetLocalAddr: RtcString;

    { Local Port to which this socket is connected. }
    function GetLocalPort: RtcString;

    { Peer (remote) Address to which this socket is connected.
      Does NOT apply to Listening sockets (server).  }
    function GetPeerAddr: RtcString;

    { Peer (remote) Port to which this socket is connected.
      Does NOT apply to Listening sockets (server).  }
    function GetPeerPort: RtcString;
    end;

implementation

(*** Declare missing functions and types ***)

{$IFDEF WINDOWS}

  // all declarations in the "rtcWinSock.pas" unit

{$ELSE} {$IFDEF FPSOCK}

CONST
  INVALID_SOCKET		= TSocket(NOT(0));
  SOCKET_ERROR			= -1;

  INADDR_LOOPBACK  = $7F000001;
  INADDR_BROADCAST = $FFFFFFFF;
  ADDR_ANY		 = INADDR_ANY;

  cLocalHostStr = 'localhost';
  cLocalhost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  cBroadcast = '255.255.255.255';
  c6Localhost = '::1';
  c6AnyHost = '::0';
  c6Broadcast = 'ffff::1';
  cAnyPort = '0';

  WSAECONNRESET = ESysECONNRESET;

CONST
{$IFDEF Darwin}
  MSG_NOSIGNAL  = 0;
{$ENDIF}

  WSAEPROCLIM = -101;
  WSASYSNOTREADY = -102;
  WSAVERNOTSUPPORTED = -103;
  WSANOTINITIALISED = -104;
  WSAEDISCON = -105;
  WSAHOST_NOT_FOUND = -106;
  WSATRY_AGAIN = -107;
  WSANO_RECOVERY = -108;
  WSANO_DATA = -109;

TYPE
  PSockAddrIn = ^TSockAddrIn;
  PSockAddrIn6 = ^TSockAddrIn6;

  TAddrFamily = integer;
  PFDSet = ^TFDSet;
  TSockAddrIn = TInetSockAddr;
  TIP_mreq =  record
    imr_multiaddr: TInAddr;     // IP multicast address of group
    imr_interface: TInAddr;     // local IP address of interface
  end;
  TSockAddrIn6 = TInetSockAddr6;
  TIPv6_mreq = record
    ipv6mr_multiaddr: TIn6Addr; // IPv6 multicast address.
    ipv6mr_interface: integer;   // Interface index.
  end;

function IN6_IS_ADDR_UNSPECIFIED(const a: PIn6Addr): boolean;
begin
  Result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and (a^.u6_addr32[3] = 0));
end;


function IN6_IS_ADDR_LOOPBACK(const a: PIn6Addr): boolean;
begin
  Result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and
             (a^.u6_addr8[12] = 0) and (a^.u6_addr8[13] = 0) and
             (a^.u6_addr8[14] = 0) and (a^.u6_addr8[15] = 1));
end;

function IN6_IS_ADDR_LINKLOCAL(const a: PIn6Addr): boolean;
begin
  Result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $80));
end;

function IN6_IS_ADDR_SITELOCAL(const a: PIn6Addr): boolean;
begin
  Result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $C0));
end;

function IN6_IS_ADDR_MULTICAST(const a: PIn6Addr): boolean;
begin
  Result := (a^.u6_addr8[0] = $FF);
end;

function IN6_ADDR_EQUAL(const a: PIn6Addr; const b: PIn6Addr): boolean;
begin
  Result := (CompareMem( a, b, sizeof(TIn6Addr)));
end;

procedure SET_IN6_IF_ADDR_ANY (const a: PIn6Addr);
begin
  FillChar(a^, sizeof(TIn6Addr), 0);
end;

procedure SET_LOOPBACK_ADDR6 (const a: PIn6Addr);
begin
  FillChar(a^, sizeof(TIn6Addr), 0);
  a^.u6_addr8[15] := 1;
end;

function SizeOfVarSin(sin: TSockAddr): integer;
  begin
  case sin.sin_family of
    AF_INET: Result := SizeOf(TSockAddrIn);
    AF_INET6: Result := SizeOf(TSockAddrIn6);
    else
      Result := 0;
    end;
  end;

procedure ResolveNameToIP(Name: AnsiString; Family, SockProtocol, SockType: integer; const IPList: TStrings);
var
  x, n: integer;
  a4: array [1..255] of in_addr;
  a6: array [1..255] of Tin6_addr;
begin
  IPList.Clear;
  if (family = AF_INET) or (family = AF_UNSPEC) then
  begin
    if lowercase(name) = cLocalHostStr then
      IpList.Add(cLocalHost)
    else
    begin
      a4[1] := StrTonetAddr(name);
      if a4[1].s_addr = INADDR_ANY then
        x := Resolvename(name, a4)
      else
        x := 1;
      for n := 1  to x do
        IpList.Add(netaddrToStr(a4[n]));
    end;
  end;

  if (family = AF_INET6) or (family = AF_UNSPEC) then
  begin
    if lowercase(name) = cLocalHostStr then
      IpList.Add(c6LocalHost)
    else
    begin
      a6[1] := StrTonetAddr6(name);
      if IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then
        x := Resolvename6(name, a6)
      else
        x := 1;
      for n := 1  to x do
        IpList.Add(netaddrToStr6(a6[n]));
    end;
  end;

  if IPList.Count = 0 then
    IPList.Add(cLocalHost);
end;

function ResolvePort(Port: AnsiString; Family, SockProtocol, SockType: integer): Word;
var
  ProtoEnt: TProtocolEntry;
  ServEnt: TServiceEntry;
begin
  Result := htons(StrToIntDef(Port, 0));
  if Result = 0 then
  begin
    ProtoEnt.Name := '';
    GetProtocolByNumber(SockProtocol, ProtoEnt);
    ServEnt.port := 0;
    GetServiceByName(Port, ProtoEnt.Name, ServEnt);
    Result := ServEnt.port;
  end;
end;

function ResolveIPToName(IP: AnsiString; Family, SockProtocol, SockType: integer): AnsiString;
var
  n: integer;
  a4: array [1..1] of in_addr;
  a6: array [1..1] of Tin6_addr;
  a: array [1..1] of AnsiString;
begin
  Result := IP;
  a4[1] := StrToNetAddr(IP);
  if a4[1].s_addr <> INADDR_ANY then
  begin
//why ResolveAddress need address in HOST order? :-O
    n := ResolveAddress(nettohost(a4[1]), a);
    if n > 0 then
      Result := a[1];
  end
  else
  begin
    a6[1] := StrToNetAddr6(IP);
    n := ResolveAddress6(a6[1], a);
    if n > 0 then
      Result := a[1];
  end;
end;

function WSA_GetSockName(s: TSocket; var name: TSockAddr; PreferIP4:Boolean): Integer;
  var
    len: integer;
  begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := fpGetSockName(s, @name, @Len);
  end;

function WSA_GetPeerName(s: TSocket; var name: TSockAddr; PreferIP4:Boolean): Integer;
  var
    len: integer;
  begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := fpGetPeerName(s, @name, @Len);
  end;

function WSA_Accept(s: TSocket; var addr: TSockAddr): TSocket;
  var
    x: integer;
  begin
  x := SizeOf(addr);
  Result := fpAccept(s, @addr, @x);
  end;

function WSA_SetVarSin(var Sin: TSockAddr; IP, Port: AnsiString; Family, SockProtocol, SockType: integer; PreferIP4, PreferIPDef: Boolean): integer;
  var
    TwoPass: boolean;
    f1, f2: integer;

  function GetAddr(f:integer): integer;
    var
      a4: array [1..1] of in_addr;
      a6: array [1..1] of Tin6_addr;
    begin
    Result := ESysEPROTONOSUPPORT;
    case f of
      AF_INET, AF_UNSPEC:
          begin
          if (IP='') or (IP = cAnyHost) then
            begin
            Sin.sin_family := AF_INET;
            Result := 0;
            end
          else
            begin
            if lowercase(IP) = cLocalHostStr then
              a4[1].s_addr := htonl(INADDR_LOOPBACK)
            else
              begin
              a4[1].s_addr := 0;
              Result := WSAHOST_NOT_FOUND;
              a4[1] := StrTonetAddr(IP);
              if a4[1].s_addr = INADDR_ANY then
                Resolvename(ip, a4);
              end;
            if a4[1].s_addr <> INADDR_ANY then
              begin
              Sin.sin_family := AF_INET;
              Sin.sin_addr := a4[1];
              Result := 0;
              end;
            end;
          end;
      AF_INET6:
          begin
          if (IP='') or (IP = c6AnyHost) then
            begin
            Sin.sin_family := AF_INET6;
            Result := 0;
            end
          else
            begin
            if lowercase(IP) = cLocalHostStr then
              SET_LOOPBACK_ADDR6(@a6[1])
            else
              begin
              Result := WSAHOST_NOT_FOUND;
              SET_IN6_IF_ADDR_ANY(@a6[1]);
              a6[1] := StrTonetAddr6(IP);
              if IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then
                Resolvename6(ip, a6);
              end;
            if IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then
              begin
              Sin.sin_family := AF_INET6;
              PSockAddrIn6(@Sin)^.sin6_addr := a6[1];
              Result := 0;
              end;
            end;
          end;
      end;
    end;

  begin
  Result := 0;
  FillChar(Sin, Sizeof(Sin), 0);
  Sin.sin_port := Resolveport(port, family, SockProtocol, SockType);
  TwoPass := False;
  if Family = AF_UNSPEC then
    begin
    if PreferIP4 then
      begin
      f1 := AF_INET;
      f2 := AF_INET6;
      TwoPass := True;
      end
    else
      begin
      f1 := AF_INET6;
      f2 := AF_INET;
      TwoPass := True;
      end;
    end
  else
    f1 := Family;
  Result := GetAddr(f1);
  if (Result <> 0) and TwoPass then
    begin
    Result := GetAddr(f2);
    if Result <> 0 then
      Result := GetAddr(f1);
    end;
  end;

function WSA_GetSinIP(Sin: TSockAddr): AnsiString;
  begin
  Result := '';
  case sin.sin_family of
    AF_INET:  Result := NetAddrToStr(sin.sin_addr);
    AF_INET6: Result := NetAddrToStr6(PSockAddrIn6(@sin)^.sin6_addr);
    end;
  end;

function WSA_GetSinPort(Sin: TSockAddr): Integer;
  begin
  if (Sin.sin_family = AF_INET6) then
    Result := Sockets.ntohs(PSockAddrIn6(@Sin)^.sin6_port)
  else
    Result := Sockets.ntohs(Sin.sin_port);
  end;

(* function WSA_InitSocketInterface(stack: String): Boolean;
  begin
  Libc.Signal(Libc.SIGPIPE, TSignalHandler(Libc.SIG_IGN));
  end; *)

function WSA_ErrorDesc(ErrCode:integer):String;
  begin
  case ErrCode of
    0:Result := '';
    ESysEINTR: {10004}
      Result := 'Interrupted system call';
    ESysEBADF: {10009}
      Result := 'Bad file number';
    ESysEACCES: {10013}
      Result := 'Permission denied';
    ESysEFAULT: {10014}
      Result := 'Bad address';
    ESysEINVAL: {10022}
      Result := 'Invalid argument';
    ESysEMFILE: {10024}
      Result := 'Too many open files';
    ESysEWOULDBLOCK: {10035}
      Result := 'Operation would block';
    ESysEINPROGRESS: {10036}
      Result := 'Operation now in progress';
    ESysEALREADY: {10037}
      Result := 'Operation already in progress';
    ESysENOTSOCK: {10038}
      Result := 'Socket operation on nonsocket';
    ESysEDESTADDRREQ: {10039}
      Result := 'Destination address required';
    ESysEMSGSIZE: {10040}
      Result := 'Message too long';
    ESysEPROTOTYPE: {10041}
      Result := 'Protocol wrong type for Socket';
    ESysENOPROTOOPT: {10042}
      Result := 'Protocol not available';
    ESysEPROTONOSUPPORT: {10043}
      Result := 'Protocol not supported';
    ESysESOCKTNOSUPPORT: {10044}
      Result := 'Socket not supported';
    ESysEOPNOTSUPP: {10045}
      Result := 'Operation not supported on Socket';
    ESysEPFNOSUPPORT: {10046}
      Result := 'Protocol family not supported';
    ESysEAFNOSUPPORT: {10047}
      Result := 'Address family not supported';
    ESysEADDRINUSE: {10048}
      Result := 'Address already in use';
    ESysEADDRNOTAVAIL: {10049}
      Result := 'Can''t assign requested address';
    ESysENETDOWN: {10050}
      Result := 'Network is down';
    ESysENETUNREACH: {10051}
      Result := 'Network is unreachable';
    ESysENETRESET: {10052}
      Result := 'Network dropped connection on reset';
    ESysECONNABORTED: {10053}
      Result := 'Software caused connection abort';
    ESysECONNRESET: {10054}
      Result := 'Connection reset by peer';
    ESysENOBUFS: {10055}
      Result := 'No Buffer space available';
    ESysEISCONN: {10056}
      Result := 'Socket is already connected';
    ESysENOTCONN: {10057}
      Result := 'Socket is not connected';
    ESysESHUTDOWN: {10058}
      Result := 'Can''t send after Socket shutdown';
    ESysETOOMANYREFS: {10059}
      Result := 'Too many references:can''t splice';
    ESysETIMEDOUT: {10060}
      Result := 'Connection timed out';
    ESysECONNREFUSED: {10061}
      Result := 'Connection refused';
    ESysELOOP: {10062}
      Result := 'Too many levels of symbolic links';
    ESysENAMETOOLONG: {10063}
      Result := 'File name is too long';
    ESysEHOSTDOWN: {10064}
      Result := 'Host is down';
    ESysEHOSTUNREACH: {10065}
      Result := 'No route to host';
    ESysENOTEMPTY: {10066}
      Result := 'Directory is not empty';
    ESysEUSERS: {10068}
      Result := 'Too many users';
    ESysEDQUOT: {10069}
      Result := 'Disk quota exceeded';
    ESysESTALE: {10070}
      Result := 'Stale NFS file handle';
    ESysEREMOTE: {10071}
      Result := 'Too many levels of remote in path';
    WSAEPROCLIM: {10067}
      Result := 'Too many processes';
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

{$ELSE} {$IFDEF POSIX}

type
  TLinger=Linger;
  TTimeVal=TimeVal;

const
  INVALID_SOCKET = TSocket(NOT(0));

  SOCKET_ERROR=-1;

  SIGPIPE = 13;
  SIG_IGN = 1;

  EPROTONOSUPPORT=-1;
  ECONNRESET=-2;

  WSAECONNRESET = ECONNRESET;

  NI_MAXHOST	   = 1025;
  NI_MAXSERV	   = 32;

  cLocalHostStr : RtcString = 'localhost';
  cLocalhost : RtcString = '127.0.0.1';
  cAnyHost : RtcString = '0.0.0.0';
  cBroadcast : RtcString = '255.255.255.255';
  c6Localhost : RtcString = '::1';
  c6AnyHost : RtcString = '::0';
  c6Broadcast : RtcString = 'ffff::1';
  cAnyPort : RtcString = '0';

{$IFNDEF MACOS}
  AF_MAX = AF_INET6;
  SO_NOSIGPIPE = MSG_NOSIGNAL;
{$ENDIF}

(*
function __error: PInteger; cdecl
{$IFDEF MACOSX}
external libc name _PU + '__error';
{$ENDIF}
function errno: Integer;
begin
  Result := __error()^;
end;
*)

function WSA_SetVarSin(var Sin: TSockAddr; IP, Port: RtcString; Family, SockProtocol, SockType: integer; PreferIP4, PreferIPDef: Boolean): integer;
  var
    Hints1, Hints2, Hints3: AddrInfo;
    TwoPass: boolean;
    {$IFDEF NEXTGEN} M1,M2:TMarshaller; {$ENDIF}
    IP2,Port2: RtcPtrAnsiChar;

  function GetAddr(Hints: AddrInfo): integer;
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
        Result := GetAddrInfo(IP2, nil, Hints, Addr);
        end
      else
        begin
        if (IP='') or (IP=cAnyHost) or (IP=c6AnyHost) then
          begin
          Hints.ai_flags := AI_PASSIVE;
          Result := GetAddrInfo(nil, Port2, Hints, Addr);
          end
        else
          begin
          if (IP=cLocalhost) or (IP=c6Localhost) or (IP=cLocalHostStr) then
            Result := GetAddrInfo(nil, Port2, Hints, Addr)
          else
            Result := GetAddrInfo(IP2, Port2, Hints, Addr);
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
        FreeAddrInfo(Addr^);
      end;
    end;

  begin
    FillChar(Sin, Sizeof(Sin), 0);
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

    {$IFDEF NEXTGEN} IP2:=MarshaledAString(M1.AsAnsi(IP,CP_UTF8).ToPointer);
                     Port2:=MarshaledAString(M2.AsAnsi(Port,CP_UTF8).ToPointer);
    {$ELSE}
                     IP2:=RtcPtrAnsiChar(@(RtcStringToBytesZero(IP)[0]));
                     Port2:=RtcPtrAnsiChar(@(RtcStringToBytesZero(Port)[0]));
    {$ENDIF}

    Result := GetAddr(Hints1);
    if Result <> 0 then
      if TwoPass then
        begin
        Hints2.ai_socktype := Hints1.ai_socktype;
        Hints2.ai_protocol := Hints1.ai_protocol;
        Result := GetAddr(Hints2);
        if Result <> 0 then
          begin
          Hints3.ai_socktype := Hints1.ai_socktype;
          Hints3.ai_protocol := Hints1.ai_protocol;
          Result := GetAddr(Hints3);
          end;
        end;
  end;

function WSA_GetSinIP(Sin: TSockAddr): RtcString;
  var
    host, serv: RtcByteArray;
    hostlen, servlen: integer;
    r, size: integer;
    vSin:TAPISockAddr absolute Sin;
  begin
  Result := '';
  hostlen := NI_MAXHOST;
  servlen := NI_MAXSERV;
  setlength(host, hostlen);
  setlength(serv, servlen);
  {$IFNDEF MACOS}
  if sin.sa_family=AF_INET6 then
    size:=SizeOf(SockAddr_In6)
  else
    size:=SizeOf(SockAddr_In);
  {$ELSE}
  size:=sin.sa_len;
  {$ENDIF}
  r := GetNameInfo(vSin, size,
                   @host[0], hostlen,
                   @serv[0], servlen,
                   NI_NUMERICHOST + NI_NUMERICSERV);
  if r = 0 then
    Result := RtcBytesZeroToString(host);
  end;

function WSA_GetSinPort(Sin: TSockAddr): Integer;
  begin
  if (Sin.sa_family = AF_INET6) then
    Result := ntohs(PSockAddr_In6(@Sin)^.sin6_port)
  else
    Result := ntohs(PSockAddr_In(@Sin)^.sin_port);
  end;

function WSA_ErrorDesc(error:integer):String;
  begin
  case error of
    0:  Result:='';
{$IFDEF MACOS}
    1:  Result:='Operation not permitted';
    2:  Result:='No such file or directory';
    3:  Result:='No such process';
    4:  Result:='Interrupted system call';
    5:  Result:='Input/output error';
    6:  Result:='Device not configured';
    7:  Result:='Argument list too long';
    8:  Result:='Exec format error';
    9:  Result:='Bad file descriptor';
    10: Result:='No child processes';
    11: Result:='Resource deadlock avoided';
    12: Result:='Cannot allocate memory';
    13: Result:='Permission denied';
    14: Result:='Bad address';
    15: Result:='Block device required';
    16: Result:='Device / Resource busy';
    17: Result:='File exists';
    18: Result:='Cross-device link';
    19: Result:='Operation not supported by device';
    20: Result:='Not a directory';
    21: Result:='Is a directory';
    22: Result:='Invalid argument';
    23: Result:='Too many open files in system';
    24: Result:='Too many open files';
    25: Result:='Inappropriate ioctl for device';
    26: Result:='Text file busy';
    27: Result:='File too large';
    28: Result:='No space left on device';
    29: Result:='Illegal seek';
    30: Result:='Read-only file system';
    31: Result:='Too many links';
    32: Result:='Broken pipe';
    33: Result:='Numerical argument out of domain';
    34: Result:='Result too large';
    35: Result:='Resource temporarily unavailable';
    36: Result:='Operation now in progress';
    37: Result:='Operation already in progress';
    38: Result:='Socket operation on non-socket';
    39: Result:='Destination address required';
    40: Result:='Message too long';
    41: Result:='Protocol wrong type for socket';
    42: Result:='Protocol not available';
    43: Result:='Protocol not supported';
    44: Result:='Socket type not supported';
    45: Result:='Operation not supported';
    46: Result:='Protocol family not supported';
    47: Result:='Address family not supported by protocol family';
    48: Result:='Address already in use';
    49: Result:='Can''t assign requested address';
    50: Result:='Network is down';
    51: Result:='Network is unreachable';
    52: Result:='Network dropped connection on reset';
    53: Result:='Software caused connection abort';
    54: Result:='Connection reset by peer';
    55: Result:='No buffer space available';
    56: Result:='Socket is already connected';
    57: Result:='Socket is not connected';
    58: Result:='Can''t send after socket shutdown';
    59: Result:='Too many references: can''t splice';
    60: Result:='Operation timed out';
    61: Result:='Connection refused';
    62: Result:='Too many levels of symbolic links';
    63: Result:='File name too long';
    64: Result:='Host is down';
    65: Result:='No route to host';
    66: Result:='Directory not empty';
    67: Result:='Too many processes';
    68: Result:='Too many users';
    69: Result:='Disc quota exceeded';
    70: Result:='Stale NFS file handle';
    71: Result:='Too many levels of remote in path';
    72: Result:='RPC struct is bad';
    73: Result:='RPC version wrong';
    74: Result:='RPC prog. not avail';
    75: Result:='Program version wrong';
    76: Result:='Bad procedure for program';
    77: Result:='No locks available';
    78: Result:='Function not implemented';
    79: Result:='Inappropriate file type or format';
    80: Result:='Authentication error';
    81: Result:='Need authenticator';
    82: Result:='Device power is off';
    83: Result:='Device error, e.g. paper out';
    84: Result:='Value too large to be stored in data type';
    85: Result:='Bad executable';
    86: Result:='Bad CPU type in executable';
    87: Result:='Shared library version mismatch';
    88: Result:='Malformed Macho file';
    89: Result:='Operation canceled';
    90: Result:='Identifier removed';
    91: Result:='No message of desired type';
    92: Result:='Illegal byte sequence';
    93: Result:='Attribute not found';
    94: Result:='Bad message';
    95: Result:='Reserved';
    96: Result:='No message available on STREAM';
    97: Result:='Reserved';
    98: Result:='No STREAM resources';
    99: Result:='Not a STREAM';
    100:Result:='Protocol error';
    101:Result:='STREAM ioctl timeout';
    102:Result:='Operation not supported on socket';
    103:Result:='No such policy registered';
{$ELSE} {$IFDEF ANDROID}
    1:  Result:='Operation not permitted';
    2:  Result:='No such file or directory';
    3:  Result:='No such process';
    4:  Result:='Interrupted system call';
    5:  Result:='I/O error';
    6:  Result:='No such device or address';
    7:  Result:='Argument list too long';
    8:  Result:='Exec format error';
    9:  Result:='Bad file number';
    10: Result:='No child processes';
    11: Result:='Try again';
    12: Result:='Out of memory';
    13: Result:='Permission denied';
    14: Result:='Bad address';
    15: Result:='Block device required';
    16: Result:='Device or resource busy';
    17: Result:='File exists';
    18: Result:='Cross-device link';
    19: Result:='No such device';
    20: Result:='Not a directory';
    21: Result:='Is a directory';
    22: Result:='Invalid argument';
    23: Result:='File table overflow';
    24: Result:='Too many open files';
    25: Result:='Not a typewriter';
    26: Result:='Text file busy';
    27: Result:='File too large';
    28: Result:='No space left on device';
    29: Result:='Illegal seek';
    30: Result:='Read-only file system';
    31: Result:='Too many links';
    32: Result:='Broken pipe';
    33: Result:='Math argument out of domain of func';
    34: Result:='Math result not representable';
    35: Result:='Resource deadlock would occur';
    36: Result:='File name too long';
    37: Result:='No record locks available';
    38: Result:='Function not implemented';
    39: Result:='Directory not empty';
    40: Result:='Too many symbolic links encountered';
    42: Result:='No message of desired type';
    43: Result:='Identifier removed';
    44: Result:='Channel number out of range';
    45: Result:='Level 2 not synchronized';
    46: Result:='Level 3 halted';
    47: Result:='Level 3 reset';
    48: Result:='Link number out of range';
    49: Result:='Protocol driver not attached';
    50: Result:='No CSI structure available';
    51: Result:='Level 2 halted';
    52: Result:='Invalid exchange';
    53: Result:='Invalid request descriptor';
    54: Result:='Exchange full';
    55: Result:='No anode';
    56: Result:='Invalid request code';
    57: Result:='Invalid slot';
    59: Result:='Bad font file format';
    60: Result:='Device not a stream';
    61: Result:='No data available';
    62: Result:='Timer expired';
    63: Result:='Out of streams resources';
    64: Result:='Machine is not on the network';
    65: Result:='Package not installed';
    66: Result:='Object is remote';
    67: Result:='Link has been severed';
    68: Result:='Advertise error';
    69: Result:='Srmount error';
    70: Result:='Communication error on send';
    71: Result:='Protocol error';
    72: Result:='Multihop attempted';
    73: Result:='RFS specific error';
    74: Result:='Not a data message';
    75: Result:='Value too large for defined data type';
    76: Result:='Name not unique on network';
    77: Result:='File descriptor in bad state';
    78: Result:='Remote address changed';
    79: Result:='Can not access a needed shared library';
    80: Result:='Accessing a corrupted shared library';
    81: Result:='.lib section in a.out corrupted';
    82: Result:='Attempting to link in too many shared libraries';
    83: Result:='Cannot exec a shared library directly';
    84: Result:='Illegal byte sequence';
    85: Result:='Interrupted system call should be restarted';
    86: Result:='Streams pipe error';
    87: Result:='Too many users';
    88: Result:='Socket operation on non-socket';
    89: Result:='Destination address required';
    90: Result:='Message too long';
    91: Result:='Protocol wrong type for socket';
    92: Result:='Protocol not available';
    93: Result:='Protocol not supported';
    94: Result:='Socket type not supported';
    95: Result:='Operation not supported on transport endpoint';
    96: Result:='Protocol family not supported';
    97: Result:='Address family not supported by protocol';
    98: Result:='Address already in use';
    99: Result:='Cannot assign requested address';
    100:Result:='Network is down';
    101:Result:='Network is unreachable';
    102:Result:='Network dropped connection because of reset';
    103:Result:='Software caused connection abort';
    104:Result:='Connection reset by peer';
    105:Result:='No buffer space available';
    106:Result:='Transport endpoint is already connected';
    107:Result:='Transport endpoint is not connected';
    108:Result:='Cannot send after transport endpoint shutdown';
    109:Result:='Too many references: cannot splice';
    110:Result:='Connection timed out';
    111:Result:='Connection refused';
    112:Result:='Host is down';
    113:Result:='No route to host';
    114:Result:='Operation already in progress';
    115:Result:='Operation now in progress';
    116:Result:='Stale NFS file handle';
    117:Result:='Structure needs cleaning';
    118:Result:='Not a XENIX named type file';
    119:Result:='No XENIX semaphores available';
    120:Result:='Is a named type file';
    121:Result:='Remote I/O error';
    122:Result:='Quota exceeded';
    123:Result:='No medium found';
    124:Result:='Wrong medium type';
    125:Result:='Operation Canceled';
    126:Result:='Required key not available';
    127:Result:='Key has expired';
    128:Result:='Key has been revoked';
    129:Result:='Key was rejected by service';
    130:Result:='Owner died';
    131:Result:='State not recoverable';
{$ENDIF} {$ENDIF}
    else
      Result:=String(gai_strerror(error));
    end;
  end;

{$ELSE} {$IFDEF RTC_NIX_SOCK}

  // all declarations in the "rtcNixSock.pas" unit

{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

constructor TRtcSocket.Create;
  begin
  inherited;
{$IFDEF WINDOWS}
  LoadWinSock;
{$ELSE}{$IFDEF POSIX}
  SetLength(FTempBuffer,65000);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  LoadNixSock;
  SetLength(FTempBuffer,65000);
{$ENDIF}{$ENDIF}{$ENDIF}
  FSocket:=INVALID_SOCKET;
  FOSocket:=FSocket;
  NewSockID:=FSocket;
  end;

destructor TRtcSocket.Destroy;
  begin
{$IFDEF POSIX}
  SetLength(FTempBuffer,0);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  SetLength(FTempBuffer,0);
{$ENDIF}{$ENDIF}
  Sock_Close;
  inherited;
  end;

function TRtcSocket.Listen(const FAddr,FPort:RtcString; const TOA:TRtcTimeoutsOfAPI; PreferIP4, PreferIPDef:boolean): boolean;
  begin
  Sock_ResetError;

  Sock_Listen(FAddr,FPort,TOA,PreferIP4,PreferIPDef);
  Result:= not Sock_CheckError;
  if not Result then
    Sock_Close;
  end;

function TRtcSocket.Connect(const FAddr,FPort:RtcString; const TOA:TRtcTimeoutsOfAPI; PreferIP4, PreferIPDef:boolean): boolean;
  begin
  Sock_ResetError;

  Sock_Connect(FAddr,FPort,TOA,PreferIP4,PreferIPDef);
  Result:= not Sock_CheckError;

  if not Result then
    Sock_Close;
  end;

function TRtcSocket.WaitingSockets(vTimeout:integer): integer;
  begin
  Result:=-1;
  Sock_ResetError;

  if Sock_CanRead(vTimeout) then
    Result:=1
  else if not Sock_CheckError then
    Result:=0;
  end;

function TRtcSocket.GetNewSocket: TRtcSocket;
  var
    Sck:TSocket;
  begin
  Result:=nil;
  Sock_ResetError;

  Sck:=Sock_Accept;
  if Sock_CheckError then Exit;

  Result:=TRtcSocket.Create;
  Result.NewSockID:=Sck;
  end;

function TRtcSocket.StartNewSocket(PreferIP4:boolean): boolean;
  begin
  Sock_ResetError;

  Sock_SetSocket(NewSockID,PreferIP4);
  Result:= not Sock_CheckError;
  end;

function TRtcSocket.Close: boolean;
  begin
  Result:=False;
  Sock_ResetError;
  if Sock_Close then
    Result:=True;
  end;

function TRtcSocket.Shut_down:boolean;
  begin
  Result:=Sock_Shutdown;
  end;

function TRtcSocket.ReceiveEx(var Str: RtcByteArray; vTimeout:integer): boolean;
  var
    r,l: integer;
  begin
  Sock_ResetError;

  l:=Sock_WaitingData;
  if l<0 then
    begin
    SetLength(Str,0);
    Result:=False;
    end
  else
    begin
    if l=0 then // nothing to read yet?
      if not Sock_CanRead(vTimeout) then // wait for new data
        begin
        SetLength(Str,0);
        Result:=not Sock_CheckError;
        Exit;
        end
      else
        l:=Sock_WaitingData;

    if l>0 then
      begin
      if l>SOCK_MAX_READ_SIZE then
        l:=SOCK_MAX_READ_SIZE;
      SetLength(Str,l);
      r:=Sock_RecvBuffer(Str[0],l);
      if r<>l then // received size has to be equal to "WaitingData"
        begin
        if r>=0 then
          begin
          FErrCode:=-1;
          FErr:='Reading error';
          end;
        SetLength(Str,0);
        Result:=False;
        end
      else
        Result:=not Sock_CheckError;
      end
    else // can read but nothing to read? error!
      begin
      SetLength(Str,0);
      FErrCode:=-1;
      FErr:='Connection error';
      Result:=False;
      end;
    end;
  end;

function TRtcSocket.SendEx(var Str: RtcByteArray; at: integer; vTimeout:integer): Integer;
  begin
  Sock_ResetError;

{ Sock_SendBuffer() is a blocking call.
  If the output is not ready, it would block infinitely. }
  if Sock_CanWrite(vTimeout) then
    begin
    Result:=Sock_SendBuffer(Str[at-1],length(Str)-at+1);
    if Result=0 then
      Result:=-1; // error!
    end
  else if Sock_CheckError then
    Result:=-1 // error!
  else
    Result:=0;

  if Result>=0 then
    if Sock_CheckError then
      Result:=-1;
  end;

function TRtcSocket.GetLastErrorText: String;
  begin
  if FErrCode=0 then
    Result:=''
  else
    Result:='#'+IntToStr(FErrCode)+': '+FErr;
  end;

function TRtcSocket.GetLocalAddr: RtcString;
  begin
  if not Sock_Invalid then
    Result:=Sock_GetLocalSinIP
  else
    Result:='';
  end;

function TRtcSocket.GetLocalPort: RtcString;
  begin
  if not Sock_Invalid then
    Result:=Sock_GetLocalSinPort
  else
    Result:='';
  end;

function TRtcSocket.GetPeerAddr: RtcString;
  begin
  if not Sock_Invalid then
    Result:=Sock_GetRemoteSinIP
  else
    Result:='';
  end;

function TRtcSocket.GetPeerPort: RtcString;
  begin
  if not Sock_Invalid then
    Result:=Sock_GetRemoteSinPort
  else
    Result:='';
  end;

function TRtcSocket.Sock_Err(res: Integer):boolean;
  begin
  if res>=0 then
    Result:=False
  else
    begin
    Result:=True;
    Sock_CheckLastError;
    end;
  end;

function TRtcSocket.Sock_Invalid: boolean;
  begin
  Result := FOSocket=INVALID_SOCKET;
  end;

(*** SOCKET-SPECIFIC METHODS ***)

function TRtcSocket.Sock_CheckError: boolean;
  begin
{$IFDEF WINDOWS}
  Result := (FErrCode <> 0) and
            (FErrCode <> WSAEINPROGRESS) and
            (FErrCode <> WSAEWOULDBLOCK);
{$ELSE}{$IFDEF FPSOCK}
  Result := (FErrCode <> 0) and
            (FErrCode <> ESysEINPROGRESS) and
            (FErrCode <> ESysEWOULDBLOCK);
{$ELSE}{$IFDEF POSIX}
  Result := (FErrCode <> 0) and
            (FErrCode <> EINPROGRESS) and
            (FErrCode <> EWOULDBLOCK);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  Result := (FErrCode <> 0) and
            (FErrCode <> WSAEINPROGRESS) and
            (FErrCode <> WSAEWOULDBLOCK);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

procedure TRtcSocket.Sock_CheckLastErrorDesc;
  begin
{$IFDEF WINDOWS}
  FErr := WSocket_ErrorDesc(FErrCode);
{$ELSE}{$IFDEF FPSOCK}
  FErr := WSA_ErrorDesc(FErrCode);
{$ELSE}{$IFDEF POSIX}
  FErr := WSA_ErrorDesc(FErrCode);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  FErr := WSA_ErrorDesc(FErrCode);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  if FErr<>'' then
    FErr:=FErr+' '+Sock_GetConnInfo;
  end;

procedure TRtcSocket.Sock_CheckLastError;
  begin
{$IFDEF WINDOWS}
  FErrCode:=_WSAGetLastError;
{$ELSE}{$IFDEF FPSOCK}
  FErrCode:=fpgeterrno;
{$ELSE}{$IFDEF POSIX}
  FErrCode:=GetLastError;
  if FErrCode=0 then FErr:='' else
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  FErrCode:=WSA_GetLastError;
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  Sock_CheckLastErrorDesc;
  end;

procedure TRtcSocket.Sock_SetSin(var Sin: TSockAddr; const vAddr,vPort:RtcString; PreferIP4, PreferIPDef:boolean);
  begin
  FillChar(FSin,SizeOf(FSin),0);
{$IFDEF WINDOWS}
  FErrCode := WSocket_SetVarSin(sin, vAddr, vPort, AF_UNSPEC, IPPROTO_TCP, SOCK_STREAM, PreferIP4, PreferIPDef);
{$ELSE}{$IFDEF FPSOCK}
  FErrCode := WSA_SetVarSin(sin, vAddr, vPort, AF_UNSPEC, IPPROTO_TCP, SOCK_STREAM, PreferIP4, PreferIPDef);
{$ELSE}{$IFDEF POSIX}
  FErrCode := WSA_SetVarSin(sin, vAddr, vPort, AF_UNSPEC, IPPROTO_TCP, SOCK_STREAM, PreferIP4, PreferIPDef);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  FErrCode := WSA_SetVarSin(sin, vAddr, vPort, AF_UNSPEC, IPPROTO_TCP, SOCK_STREAM, PreferIP4, PreferIPDef);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  FSin:=Sin;
  Sock_CheckLastErrorDesc;
  end;

procedure TRtcSocket.Sock_CreateSocket(Sin: TSockAddr);
  begin
  FSocket:=INVALID_SOCKET;
{$IFDEF WINDOWS}
  FOSocket := _Socket(Sin.sin_family, SOCK_STREAM, IPPROTO_TCP);
  if FOSocket = INVALID_SOCKET then
    Sock_CheckLastError
  else
    begin
    FSocket:=FOSocket;
    FD_ZERO(FFDSet);
    FD_SET(FSocket, FFDSet);
    end;
{$ELSE}{$IFDEF FPSOCK}
  FOSocket := fpSocket(integer(Sin.sin_family), SOCK_STREAM, IPPROTO_TCP);
  if FOSocket = INVALID_SOCKET then
    Sock_CheckLastError
  else
    begin
    FSocket:=FOSocket;
    fpFD_ZERO(FFDSet);
    fpFD_SET(FSocket, FFDSet);
    end;
{$ELSE}{$IFDEF POSIX}
  FOSocket := socket(Sin.sa_family, SOCK_STREAM, IPPROTO_TCP);
  if FOSocket = INVALID_SOCKET then
    Sock_CheckLastError
  else
    begin
    FSocket:=FOSocket;
    __FD_ZERO(FFDSet);
    __FD_SET(FSocket, FFDSet);
    end;
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  FOSocket := WSA_Socket(integer(Sin.AddressFamily), SOCK_STREAM, IPPROTO_TCP);
  if FOSocket = INVALID_SOCKET then
    Sock_CheckLastError
  else
    begin
    FSocket:=FOSocket;
    FD_ZERO(FFDSet);
    FD_SET(FSocket, FFDSet);
    end;
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

procedure TRtcSocket.Sock_SetLinger(vEnable: Boolean; vLinger: Integer);
  var
    li: TLinger;
    buf: pointer;
  begin
  if FOSocket=INVALID_SOCKET then Exit;
  if vEnable then
    li.l_onoff := 1
  else
    li.l_onoff := 0;
  li.l_linger := vLinger;
  buf := @li;
{$IFDEF WINDOWS}
  Sock_Err(_SetSockOpt(FSocket, SOL_SOCKET, SO_LINGER, buf, SizeOf(li)));
{$ELSE}{$IFDEF FPSOCK}
  Sock_Err(fpSetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_LINGER), buf, SizeOf(li)));
{$ELSE}{$IFDEF POSIX}
  Sock_Err(SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_LINGER), buf, SizeOf(li)));
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  {$IFNDEF DARWIN} // SetLinger does NOT work on iOS
    Sock_Err(WSA_SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_LINGER), buf, SizeOf(li)));
  {$ENDIF}
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

procedure TRtcSocket.Sock_SetDelay(reuseAddr:boolean);
  var
    optval: integer;
    buf: pointer;
  begin
  if FOSocket=INVALID_SOCKET then Exit;
  buf := @optval;
{$IFDEF WINDOWS}
  // NO DELAY
  optval := -1; { -1=true, 0=false }
  _SetSockOpt(FSocket, IPPROTO_TCP, TCP_NODELAY, buf, SizeOf(optval));
  // KEEP-ALIVE
  optval  := -1;
  _SetSockOpt(FSocket, SOL_SOCKET, SO_KEEPALIVE, buf, SizeOf(optval));
  // REUSE ADDR.
  if reuseAddr then
    begin
    optval  := -1;
    _SetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, buf, SizeOf(optval));
    end;
  // Set READ Buffer
  optval := SOCK_READ_BUFFER_SIZE;
  _SetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, buf, SizeOf(optval));
  // Set SEND Buffer
  optval := SOCK_SEND_BUFFER_SIZE;
  _SetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, buf, SizeOf(optval));
{$ELSE}{$IFDEF FPSOCK}
  // NO DELAY
  optval := -1; { -1=true, 0=false }
  fpSetSockOpt(FSocket, IPPROTO_TCP, TCP_NODELAY, buf, SizeOf(optval));
  // KEEP-ALIVE
  optval  := -1;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_KEEPALIVE, buf, SizeOf(optval));
  // REUSE ADDR.
  if reuseAddr then
    begin
    optval  := -1;
    fpSetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, buf, SizeOf(optval));
    end;
  // Set READ Buffer
  optval := SOCK_READ_BUFFER_SIZE;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, buf, SizeOf(optval));
  // Set SEND Buffer
  optval := SOCK_SEND_BUFFER_SIZE;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, buf, SizeOf(optval));
{$ELSE}{$IFDEF POSIX}
  // NO SEND DELAY
  optval := 1;
  SetSockOpt(FSocket, SOL_SOCKET, SO_SNDLOWAT, buf, SizeOf(optval));
  // KEEP-ALIVE
  optval  := -1;
  SetSockOpt(FSocket, SOL_SOCKET, SO_KEEPALIVE, buf, SizeOf(optval));
  // REUSE ADDR.
  if reuseAddr then
    begin
    optval  := -1;
    SetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, buf, SizeOf(optval));
    end;
  // Set READ Buffer
  optval := SOCK_READ_BUFFER_SIZE;
  SetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, buf, SizeOf(optval));
  // Set SEND Buffer
  optval := SOCK_SEND_BUFFER_SIZE;
  SetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, buf, SizeOf(optval));
  // Do NOT generate SIGPIPE
  optval := -1;
  SetSockOpt(FSocket, SOL_SOCKET, SO_NOSIGPIPE, buf, SizeOf(optval));
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  // NO DELAY
  optval := -1; { -1=true, 0=false }
  WSA_SetSockOpt(FSocket, IPPROTO_TCP, TCP_NODELAY, buf, SizeOf(optval));
  // KEEP-ALIVE
  optval  := -1;
  WSA_SetSockOpt(FSocket, SOL_SOCKET, SO_KEEPALIVE, buf, SizeOf(optval));
  // REUSE ADDR.
  if reuseAddr then
    begin
    optval  := -1;
    WSA_SetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, buf, SizeOf(optval));
    end;
  // Set READ Buffer
  optval := SOCK_READ_BUFFER_SIZE;
  WSA_SetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, buf, SizeOf(optval));
  // Set SEND Buffer
  optval := SOCK_SEND_BUFFER_SIZE;
  WSA_SetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, buf, SizeOf(optval));
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

procedure TRtcSocket.Sock_SetTimeouts(const TOA:TRtcTimeoutsOfAPI);
  var
    optval: integer;
    buf: pointer;
  begin
  if FOSocket=INVALID_SOCKET then Exit;
  buf:=@optval;
{$IFDEF WINDOWS}
  {$IFDEF RTC_USESETTIMEOUTS}
  if assigned(TOA) then
    begin
    // Set RECV_TIMEO
    if TOA.ReceiveTimeout>0 then
      begin
      optval := TOA.ReceiveTimeout*1000;
      _SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, buf, SizeOf(optval));
      end;
    // Set SND_TIMEO
    if TOA.SendTimeout>0 then
      begin
      optval := TOA.SendTimeout*1000;
      _SetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, buf, SizeOf(optval));
      end;
    end;
  {$ELSE}
  // Set RECV_TIMEO
  optval := SOCK_RECV_TIMEOUT;
  _SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, buf, SizeOf(optval));
  // Set SND_TIMEO
  optval := SOCK_SEND_TIMEOUT;
  _SetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, buf, SizeOf(optval));
  {$ENDIF}
{$ELSE}{$IFDEF FPSOCK}
  {$IFDEF RTC_USESETTIMEOUTS}
  if assigned(TOA) then
    begin
    // Set RECV_TIMEO
    if TOA.ReceiveTimeout>0 then
      begin
      optval := TOA.ReceiveTimeout*1000;
      fpSetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, buf, SizeOf(optval));
      end;
    // Set SND_TIMEO
    if TOA.SendTimeout>0 then
      begin
      optval := TOA.SendTimeout*1000;
      fpSetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, buf, SizeOf(optval));
      end;
    end;
  {$ELSE}
  // Set RECV_TIMEO
  optval := SOCK_RECV_TIMEOUT;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, buf, SizeOf(optval));
  // Set SND_TIMEO
  optval := SOCK_SEND_TIMEOUT;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, buf, SizeOf(optval));
  {$ENDIF}
{$ELSE}{$IFDEF POSIX}
  {$IFDEF RTC_USESETTIMEOUTS}
  if assigned(TOA) then
    begin
    // Set RECV_TIMEO
    if TOA.ReceiveTimeout>0 then
      begin
      optval := TOA.ReceiveTimeout*1000;
      SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, buf, SizeOf(optval));
      end;
    // Set SND_TIMEO
    if TOA.SendTimeout>0 then
      begin
      optval := TOA.SendTimeout*1000;
      SetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, buf, SizeOf(optval));
      end;
    end;
  {$ELSE}
  // Set RECV_TIMEO
  optval := SOCK_RECV_TIMEOUT;
  WSA_SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, buf, SizeOf(optval));
  // Set SND_TIMEO
  optval := SOCK_SEND_TIMEOUT;
  WSA_SetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, buf, SizeOf(optval));
  {$ENDIF}
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  {$IFDEF RTC_USESETTIMEOUTS}
  if assigned(TOA) then
    begin
    // Set RECV_TIMEO
    if TOA.ReceiveTimeout>0 then
      begin
      optval := TOA.ReceiveTimeout*1000;
      WSA_SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, buf, SizeOf(optval));
      end;
    // Set SND_TIMEO
    if TOA.SendTimeout>0 then
      begin
      optval := TOA.SendTimeout*1000;
      WSA_SetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, buf, SizeOf(optval));
      end;
    end;
  {$ELSE}
  // Set RECV_TIMEO
  optval := SOCK_RECV_TIMEOUT;
  WSA_SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, buf, SizeOf(optval));
  // Set SND_TIMEO
  optval := SOCK_SEND_TIMEOUT;
  WSA_SetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, buf, SizeOf(optval));
  {$ENDIF}
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

function TRtcSocket.Sock_GetLocalSinIP: RtcString;
  begin
{$IFDEF WINDOWS}
  Result := WSocket_GetSinIP(FLocalSin);
{$ELSE}{$IFDEF FPSOCK}
  Result := WSA_GetSinIP(FLocalSin);
{$ELSE}{$IFDEF POSIX}
  Result := WSA_GetSinIP(FLocalSin);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  Result := WSA_GetSinIP(FLocalSin);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

function TRtcSocket.Sock_GetLocalSinPort: RtcString;
  begin
{$IFDEF WINDOWS}
  Result := Int2Str(WSocket_GetSinPort(FLocalSin));
{$ELSE}{$IFDEF FPSOCK}
  Result := Int2Str(WSA_GetSinPort(FLocalSin));
{$ELSE}{$IFDEF POSIX}
  Result := Int2Str(WSA_GetSinPort(FLocalSin));
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  Result := Int2Str(WSA_GetSinPort(FLocalSin));
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

function TRtcSocket.Sock_GetConnInfo: String;
  begin
{$IFDEF WINDOWS}
  case FSin.sin_family of
    AF_INET:Result:='v4';
    AF_INET6:Result:='v6';
    else Result:='v#'+IntToStr(FSin.sin_family)+'?';
    end;
  Result := '['+String(WSocket_GetSinIP(FSin))+'@'+IntToStr(WSocket_GetSinPort(FSin))+']'+Result;
{$ELSE}{$IFDEF FPSOCK}
  case FSin.sin_family of
    AF_INET:Result:='v4';
    AF_INET6:Result:='v6';
    else Result:='v#'+IntToStr(FSin.sin_family)+'?';
    end;
  Result := '['+String(WSA_GetSinIP(FSin))+'@'+IntToStr(WSA_GetSinPort(FSin))+']'+Result;
{$ELSE}{$IFDEF POSIX}
  case FSin.sa_family of
    AF_INET:Result:='v4';
    AF_INET6:Result:='v6';
    else Result:='v#'+IntToStr(FSin.sa_family)+'?';
    end;
  Result := '['+String(WSA_GetSinIP(FSin))+'@'+IntToStr(WSA_GetSinPort(FSin))+']'+Result;
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  case FSin.sin_family of
    AF_INET:Result:='v4';
    AF_INET6:Result:='v6';
    else Result:='v#'+IntToStr(FSin.sin_family)+'?';
    end;
  Result := '['+String(WSA_GetSinIP(FSin))+'@'+IntToStr(WSA_GetSinPort(FSin))+']'+Result;
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

function TRtcSocket.Sock_GetRemoteSinIP: RtcString;
  begin
{$IFDEF WINDOWS}
  Result := WSocket_GetSinIP(FRemoteSin);
{$ELSE}{$IFDEF FPSOCK}
  Result := WSA_GetSinIP(FRemoteSin);
{$ELSE}{$IFDEF POSIX}
  Result := WSA_GetSinIP(FRemoteSin);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  Result := WSA_GetSinIP(FRemoteSin);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

function TRtcSocket.Sock_GetRemoteSinPort: RtcString;
  begin
{$IFDEF WINDOWS}
  Result := Int2Str(WSocket_GetSinPort(FRemoteSin));
{$ELSE}{$IFDEF FPSOCK}
  Result := Int2Str(WSA_GetSinPort(FRemoteSin));
{$ELSE}{$IFDEF POSIX}
  Result := Int2Str(WSA_GetSinPort(FRemoteSin));
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  Result := Int2Str(WSA_GetSinPort(FRemoteSin));
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

procedure TRtcSocket.Sock_Listen(const vAddr, vPort: RtcString; const TOA:TRtcTimeoutsOfAPI; PreferIP4, PreferIPDef:boolean);
  var
    Sin: TSockAddr;
  {$IFDEF POSIX}
    vSin: TAPISockAddr absolute Sin;
    LocSin: TSockAddr;
    vLocSin: TAPISockAddr absolute LocSin;
  {$ENDIF}
    blog:integer;
  begin
  Sock_SetSin(Sin,vAddr,vPort,PreferIP4,PreferIPDef);
  if FErrCode<>0 then Exit;
  if FOSocket = INVALID_SOCKET then
    begin
    Sock_CreateSocket(Sin);
    if FErrCode<>0 then Exit;
    end;
  Sock_SetDelay(false);
  Sock_SetTimeouts(TOA);
  Sock_SetLinger(False,0);

  // BIND socket ...

{$IFDEF WINDOWS}
  if Sock_Err(_Bind(FSocket, Sin, SizeOfSockAddr(Sin))) then Exit;
  FLocalSinLen:=SizeOf(FLocalSin);
  Sock_Err(_GetSockName(FSocket, FLocalSin, FLocalSinLen));
{$ELSE}{$IFDEF FPSOCK}
  if Sock_Err(fpBind(FSocket, @Sin, SizeOfVarSin(Sin))) then Exit;
  Sock_Err(WSA_GetSockName(FSocket, FLocalSin, PreferIP4));
{$ELSE}{$IFDEF POSIX}
  {$IFDEF MACOS}
    if Sock_Err(Bind(FSocket, vSin, sin.sa_len)) then Exit;
  {$ELSE}
    if sin.sa_family=AF_INET6 then
      begin
      if Sock_Err(Bind(FSocket, vSin, SizeOf(SockAddr_In6))) then Exit;
      end
    else
      begin
      if Sock_Err(Bind(FSocket, vSin, SizeOf(SockAddr_In))) then Exit;
      end;
  {$ENDIF}
  FLocalSinLen:=SizeOf(FLocalSin);
  Sock_Err(GetSockName(FSocket, vLocSin, FLocalSinLen));
  FLocalSin:=LocSin;
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  if Sock_Err(WSA_Bind(FSocket, Sin)) then Exit;
  Sock_Err(WSA_GetSockName(FSocket, FLocalSin, PreferIP4));
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

  // Start socket Listener ...

  blog:=SOCK_LISTEN_BACKLOG;
  if blog>SOMAXCONN then blog:=SOMAXCONN;
{$IFDEF WINDOWS}
  if Sock_Err(_Listen(FSocket, blog)) then Exit;
  FLocalSinLen:=SizeOf(FLocalSin);
  if Sock_Err(_GetSockName(FSocket, FLocalSin, FLocalSinLen)) then Exit;
{$ELSE}{$IFDEF FPSOCK}
  if Sock_Err(fpListen(FSocket, blog)) then Exit;
  if Sock_Err(WSA_GetSockName(FSocket, FLocalSin, PreferIP4)) then Exit;
{$ELSE}{$IFDEF POSIX}
  if Sock_Err(Posix.SysSocket.Listen(FSocket, blog)) then Exit;

  FLocalSinLen:=SizeOf(FLocalSin);
  if Sock_Err(GetSockName(FSocket, vLocSin, FLocalSinLen)) then Exit;
  FLocalSin:=LocSin;
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  if Sock_Err(WSA_Listen(FSocket, blog)) then Exit;
  if Sock_Err(WSA_GetSockName(FSocket, FLocalSin, PreferIP4)) then Exit;
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

procedure TRtcSocket.Sock_Connect(const vAddr, vPort: RtcString; const TOA:TRtcTimeoutsOfAPI; PreferIP4, PreferIPDef:boolean);
  var
    Sin: TSockAddr;
  {$IFDEF POSIX}
    vSin: TAPISockAddr absolute Sin;
    LocSin, RemSin: TSockAddr;
    vLocSin: TAPISockAddr absolute LocSin;
    vRemSin: TAPISockAddr absolute RemSin;
  {$ENDIF}
  begin
  Sock_SetSin(Sin, vAddr, vPort, PreferIP4, PreferIPDef);
  if FErrCode<>0 then Exit;
  if FOSocket = INVALID_SOCKET then
    begin
    Sock_CreateSocket(Sin);
    if FErrCode<>0 then Exit;
    end;
  Sock_SetDelay(true);
  Sock_SetTimeouts(TOA);
  Sock_SetLinger(False,0);

{$IFDEF WINDOWS}
  if Sock_Err(_Connect(FSocket, Sin, SizeOfSockAddr(Sin))) then Exit;
  FLocalSinLen:=SizeOf(FLocalSin);
  if Sock_Err(_GetSockName(FSocket, FLocalSin, FLocalSinLen)) then Exit;
  FRemoteSinLen:=SizeOf(FRemoteSin);
  if Sock_Err(_GetPeerName(FSocket, FRemoteSin, FRemoteSinLen)) then Exit;
{$ELSE}{$IFDEF FPSOCK}
  if Sock_Err(fpConnect(FSocket, @Sin, SizeOfVarSin(Sin))) then Exit;
  if Sock_Err(WSA_GetSockName(FSocket, FLocalSin, PreferIP4)) then Exit;
  if Sock_Err(WSA_GetPeerName(FSocket, FRemoteSin, PreferIP4)) then Exit;
{$ELSE}{$IFDEF POSIX}
  {$IFDEF MACOS}
    if Sock_Err(Posix.SysSocket.Connect(FSocket, vSin, sin.sa_len)) then Exit;
  {$ELSE}
    if sin.sa_family=AF_INET6 then
      begin
      if Sock_Err(Posix.SysSocket.Connect(FSocket, vSin, SizeOf(SockAddr_In6))) then Exit;
      end
    else
      begin
      if Sock_Err(Posix.SysSocket.Connect(FSocket, vSin, SizeOf(SockAddr_In))) then Exit;
      end;
  {$ENDIF}
  FLocalSinLen:=SizeOf(FLocalSin);
  if Sock_Err(GetSockName(FSocket, vLocSin, FLocalSinLen)) then Exit;
  FLocalSin:=LocSin;

  FRemoteSinLen:=SizeOf(FRemoteSin);
  if Sock_Err(GetPeerName(FSocket, vRemSin, FRemoteSinLen)) then Exit;
  FRemoteSin:=RemSin;
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  if Sock_Err(WSA_Connect(FSocket, Sin)) then Exit;
  if Sock_Err(WSA_GetSockName(FSocket, FLocalSin, PreferIP4)) then Exit;
  if Sock_Err(WSA_GetPeerName(FSocket, FRemoteSin, PreferIP4)) then Exit;
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

function TRtcSocket.Sock_Accept: TSocket;
{$IFDEF POSIX}
  var
    RemSin:TSockAddr;
    vRemSin:TAPISockAddr absolute RemSin;
{$ENDIF}
  begin
  if FOSocket=INVALID_SOCKET then
    begin
    Result:=INVALID_SOCKET;
    FErrCode:=-1;
    FErr:='Socket not listening';
    Exit;
    end;
{$IFDEF WINDOWS}
  FRemoteSinLen:=SizeOf(FRemoteSin);
  Result := _Accept(FSocket, FRemoteSin, FRemoteSinLen);
{$ELSE}{$IFDEF FPSOCK}
  Result := WSA_Accept(FSocket, FRemoteSin);
{$ELSE}{$IFDEF POSIX}
  FRemoteSinLen:=SizeOf(FRemoteSin);
  Result := Accept(FSocket, vRemSin, FRemoteSinLen);
  FRemoteSin:=RemSin;
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  Result := WSA_Accept(FSocket, FRemoteSin);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  if Result=INVALID_SOCKET then
    begin
    Sock_CheckLastError;
    if FErrCode=0 then
      begin
      FErrCode:=-1;
      FErr:='No socket waiting';
      end;
    end;
  end;

procedure TRtcSocket.Sock_SetSocket(sid: TSocket; PreferIP4:boolean);
{$IFDEF POSIX}
  var
    LocSin,RemSin:TSockAddr;
    vLocSin:TAPISockAddr absolute LocSin;
    vRemSin:TAPISockAddr absolute RemSin;
{$ENDIF}
  begin
  FOSocket := sid;
  FSocket := sid;
{$IFDEF WINDOWS}
  FD_ZERO(FFDSet);
  FD_SET(FSocket, FFDSet);
  FLocalSinLen:=SizeOf(FLocalSin);
  if Sock_Err(_GetSockName(FSocket, FLocalSin, FLocalSinLen)) then Exit;
  FRemoteSinLen:=SizeOf(FRemoteSin);
  if Sock_Err(_GetPeerName(FSocket, FRemoteSin, FRemoteSinLen)) then Exit;
{$ELSE}{$IFDEF FPSOCK}
  fpFD_ZERO(FFDSet);
  fpFD_SET(FSocket, FFDSet);
  if Sock_Err(WSA_GetSockName(FSocket, FLocalSin, PreferIP4)) then Exit;
  if Sock_Err(WSA_GetPeerName(FSocket, FRemoteSin, PreferIP4)) then Exit;
{$ELSE}{$IFDEF POSIX}
  __FD_ZERO(FFDSet);
  __FD_SET(FSocket, FFDSet);

  FLocalSinLen:=SizeOf(FLocalSin);
  if Sock_Err(GetSockName(FSocket, vLocSin, FLocalSinLen)) then Exit;
  FLocalSin:=LocSin;

  FRemoteSinLen:=SizeOf(FRemoteSin);
  if Sock_Err(GetPeerName(FSocket, vRemSin, FRemoteSinLen)) then Exit;
  FRemoteSin:=RemSin;
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  FD_ZERO(FFDSet);
  FD_SET(FSocket, FFDSet);
  if Sock_Err(WSA_GetSockName(FSocket, FLocalSin, PreferIP4)) then Exit;
  if Sock_Err(WSA_GetPeerName(FSocket, FRemoteSin, PreferIP4)) then Exit;
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  Sock_SetLinger(False,0);
  end;

function TRtcSocket.Sock_Shutdown:boolean;
  begin
  if FOSocket=INVALID_SOCKET then
    Result:=False
  else
    begin
{$IFDEF WINDOWS}
    _Shutdown(FOSocket,SD_BOTH);
{$ELSE}{$IFDEF FPSOCK}
    fpShutdown(FOSocket,SHUT_RDWR);
{$ELSE} {$IFDEF POSIX}
    Shutdown(FOSocket,SHUT_RDWR);
{$ELSE} {$IFDEF RTC_NIX_SOCK}
    WSA_Shutdown(FOSocket,SHUT_RDWR);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
    Result:=True;
    end;
  end;

function TRtcSocket.Sock_Close:boolean;
  begin
  if Sock_Shutdown then
    begin
{$IFDEF WINDOWS}
    _CloseSocket(FOSocket);
{$ELSE}{$IFDEF FPSOCK}
    fpClose(FOSocket);
{$ELSE} {$IFDEF POSIX}
    __close(FOSocket);
{$ELSE} {$IFDEF RTC_NIX_SOCK}
    WSA_CloseSocket(FOSocket);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
    FOSocket := INVALID_SOCKET;
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcSocket.Sock_CanRead(vTimeout: integer): boolean;
  var
    TimeP: PTimeVal;
    TimeV: TTimeVal;
    x: Integer;
    FDSet: TFDSet;
  begin
  if FOSocket=INVALID_SOCKET then
    begin
    FErrCode := -1;
    FErr := 'Socket closed';
    Result:=False;
    Exit;
    end;
  if vTimeout = -1 then
    TimeP := nil
  else
    begin
    TimeV.tv_usec := (vTimeout mod 1000) * 1000;
    TimeV.tv_sec := vTimeout div 1000;
    TimeP := @TimeV;
    end;
  FDSet := FFdSet;
{$IFDEF WINDOWS}
  x := _Select(FSocket + 1, @FDSet, nil, nil, TimeP);
{$ELSE}{$IFDEF FPSOCK}
  x := fpSelect(FSocket + 1, @FDSet, nil, nil, TimeP);
{$ELSE}{$IFDEF POSIX}
  x := Select(FSocket + 1, @FDSet, nil, nil, TimeP);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  x := WSA_Select(FSocket + 1, @FDSet, nil, nil, TimeP);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  if x>0 then
    Result:=True
  else
    begin
    Sock_Err(x);
    Result:=False;
    end;
  end;

function TRtcSocket.Sock_CanWrite(vTimeout: integer): boolean;
  var
    TimeP: PTimeVal;
    TimeV: TTimeVal;
    x: Integer;
    FDSet: TFDSet;
  begin
  if FOSocket=INVALID_SOCKET then
    begin
    FErrCode := -1;
    FErr := 'Socket closed';
    Result:=False;
    Exit;
    end;
  if vTimeout = -1 then
    TimeP := nil
  else
    begin
    TimeV.tv_usec := (vTimeout mod 1000) * 1000;
    TimeV.tv_sec := vTimeout div 1000;
    TimeP := @TimeV;
    end;
  FDSet := FFdSet;
{$IFDEF WINDOWS}
  x := _Select(FSocket + 1, nil, @FDSet, nil, TimeP);
{$ELSE}{$IFDEF FPSOCK}
  x := fpSelect(FSocket + 1, nil, @FDSet, nil, TimeP);
{$ELSE}{$IFDEF POSIX}
  x := Select(FSocket + 1, nil, @FDSet, nil, TimeP);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  x := WSA_Select(FSocket + 1, nil, @FDSet, nil, TimeP);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  if x>0 then
    Result:=True
  else
    begin
    Sock_Err(x);
    Result:=False;
    end;
  end;

function TRtcSocket.Sock_WaitingData: integer;
  var
    x: Integer;
  begin
  if FOSocket=INVALID_SOCKET then
    begin
    FErrCode := -1;
    FErr := 'Socket closed';
    Result:=-1;
    Exit;
    end;
  Result := 0;
{$IFDEF WINDOWS}
  if not Sock_Err(_IoctlSocket(FSocket, FIONREAD, x)) then
    Result := x;
{$ELSE}{$IFDEF FPSOCK}
  if not Sock_Err(fpIoctl(FSocket, FIONREAD, @x)) then
    Result := x;
{$ELSE}{$IFDEF POSIX}
  if Sock_CanRead(1) then
    begin
    x := Recv(FSocket, FTempBuffer[0], length(FTempBuffer), MSG_PEEK);
    if x>0 then
      Result:=x
    else if x=0 then
      begin
      Result:=-1;
      FErrCode := WSAECONNRESET;
      Sock_CheckLastErrorDesc;
      end
    else
      begin
      Result:=-1;
      Sock_CheckLastError;
      end;
    end;
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  {$IFDEF DARWIN}
    if Sock_CanRead(1) then
      begin
      x := WSA_Recv(FSocket, FTempBuffer[0], length(FTempBuffer), MSG_PEEK);
      if x>0 then
        Result:=x
      else if x=0 then
        begin
        Result:=-1;
        FErrCode := WSAECONNRESET;
        Sock_CheckLastErrorDesc;
        end
      else
        begin
        Result:=-1;
        Sock_CheckLastError;
        end;
      end;
  {$ELSE}
    if not Sock_Err(WSA_IoctlSocket(FSocket, FIONREAD, x)) then
      Result := x;
  {$ENDIF}
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

function TRtcSocket.Sock_RecvBuffer(var Buffer; Len: Integer): Integer;
  begin
  if FOSocket=INVALID_SOCKET then
    begin
    FErrCode := -1;
    FErr := 'Socket closed';
    Result := -1;
    Exit;
    end;
{$IFDEF WINDOWS}
  Result := _Recv(FSocket, Buffer, Len, 0);
{$ELSE}{$IFDEF FPSOCK}
  Result := fpRecv(FSocket, @Buffer, Len, MSG_NOSIGNAL);
{$ELSE} {$IFDEF POSIX}
  Result := Recv(FSocket, Buffer, Len, 0); // MSG_WAITALL);
{$ELSE} {$IFDEF RTC_NIX_SOCK}
  Result := WSA_Recv(FSocket, Buffer, Len, MSG_NOSIGNAL);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  if Result<=0 then
    if Result<0 then
      Sock_CheckLastError
    else
      begin
      Result:=-1;
      FErrCode := WSAECONNRESET;
      Sock_CheckLastErrorDesc;
      end;
  end;

function TRtcSocket.Sock_SendBuffer(var Buffer; Len: Integer): Integer;
  begin
  if FOSocket=INVALID_SOCKET then
    begin
    FErrCode := -1;
    FErr := 'Socket closed';
    Result:=-1;
    Exit;
    end;
  if Len>SOCK_MAX_SEND_SIZE then
    Len:=SOCK_MAX_SEND_SIZE;
{$IFDEF WINDOWS}
  Result := _Send(FSocket, Buffer, Len, 0);
{$ELSE}{$IFDEF FPSOCK}
  Result := fpSend(FSocket, @Buffer, Len, MSG_NOSIGNAL);
{$ELSE}{$IFDEF POSIX}
  Result := Send(FSocket, Buffer, Len, 0);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  Result := WSA_Send(FSocket, Buffer, Len, MSG_NOSIGNAL);
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  if Result<=0 then
    if Result<0 then
      Sock_CheckLastError
    else
      begin
      Result:=-1;
      FErrCode := WSAECONNRESET;
      Sock_CheckLastErrorDesc;
      end;
  end;

procedure TRtcSocket.Sock_ResetError;
  begin
{$IFDEF WINDOWS}
  FErrCode:=0; FErr:='';
{$ELSE}{$IFDEF FPSOCK}
  FErrCode:=0; FErr:='';
{$ELSE}{$IFDEF POSIX}
  FErrCode:=0; SetLength(FErr,0);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  FErrCode:=0; FErr:='';
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;

end.
