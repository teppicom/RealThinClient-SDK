{
  RealThinClient Synchronous Socket implementation
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)

  @exclude
}
unit rtcSynSocket;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcSrcList,
  rtcLog,
  rtcSystem,
  rtcThrPool,

  rtcSynAPI,
  rtcSockBase;

const
  RTC_SYNSOCK_LISTEN_WAIT:integer=100;
  RTC_SYNSOCK_READ_WAIT:integer=20;
  RTC_SYNSOCK_SEND_WAIT:integer=20;

const
  MSG_LISTEN=0; // Signal "Listening"
  MSG_CONNECT=1; // Signal "Connected"
  MSG_CLOSE=2; // Signal "Closed"
  MSG_SENT=3; // Signal "Data Sent"
  MSG_READ=4; // Signal "Data Read"
  MSG_ACCEPT=5; // Signal "new client"
  MSG_LISTEN_ERR=6; // Signal "Listen Error"
  MSG_CONNECT_ERR=7; // Signal "Connect Error"
  MSG_LISTEN_LOST=8; // Signal "Listen Lost"
  MSG_CONNECT_LOST=9; // Signal "Connect Lost"

type
  TRtcSocketBaseSyn=class;

  TRtcSockSynThread=class(TRtcThread)
  public
    Sock:TRtcSocketBaseSyn;

    constructor Create; override;
    destructor Destroy; override;

    function RunJob:boolean; override;
    end;

  TRtcSocketBaseSyn=class(TRtcSocketBase)
  protected
    Thr:TRtcSockSynThread;

    FDataIn,FDataOut,
    FDataToSend:Cardinal;
    Socks:TXObjList;

    MsgQ:TXList;
    MsgLevel:integer;

    FCS:TRtcCritSec;
    FReading,FSending:boolean;
    FReadBuff:TRtcHugeByteArray;
    FSendBuff:TRtcHugeByteArray;

    FState:TRtcSocketState;

    FLastErrText:String;

    FLocalAddr,
    FLocalPort,
    FPeerAddr,
    FPeerPort:RtcString;

    inReadLoop,
    inSendLoop,
    isServer,
    isClient,
    isActive:boolean;

    FSend:RtcByteArray;
    FSendAt:integer;

(*
    FMultiCastAddr: RtcString;
    FAddr: RtcString;
    FPort: RtcString;
    FMultiThreaded: boolean;
    FMultiCast: Boolean;
    FReuseAddr: Boolean;
    FMultiCastIpTTL: Integer;
    FProtocol: TRtcSocketProtocol;
    FTimeoutsOfAPI: TRtcTimeoutsOfAPI;

    function On_ChangeState(NewState : TRtcSocketState):boolean; virtual;
    function On_NewSocket(ErrCode: Word):boolean; virtual;
    function On_DataReceived(ErrCode: Word):boolean; virtual;
    function On_DataSent(ErrCode: Word):boolean; virtual;
    function On_DnsLookupDone(ErrCode: Word):boolean; virtual;
    function On_Error(ErrCode: Word):boolean; virtual;
    function On_DataOut(Len:Cardinal):boolean; virtual;
    function On_DataIn(Len:Cardinal):boolean; virtual;
    function On_BgException(E : Exception; var CanClose : Boolean):boolean; virtual;
    function On_Message(Msg, ErrCode: Word):boolean; virtual;
*)

    procedure Do_Listen;
    procedure Do_ListenError;
    procedure Do_ListenLost;
    procedure Do_Connect;
    procedure Do_ConnectError;
    procedure Do_ConnectLost;
    procedure Do_Close;
    procedure Do_Sent;
    procedure Do_Read;
    procedure Do_Accept;

    procedure Call_Listen;
    procedure Call_ListenError;
    procedure Call_ListenLost;
    procedure Call_Connect;
    procedure Call_ConnectError;
    procedure Call_ConnectLost;
    procedure Call_Close;
    procedure Call_Sent(len:Cardinal);
    procedure Call_ReadEx(const data:RtcByteArray);
    procedure Call_Accept;

    function GetDataToSend:RtcByteArray;

    { Start Listener (Server).
      Return TRUE if everyting OK and Listener started. }
    function api_Listen:boolean; virtual; abstract;

    { How many Sockets are waiting to connect on this listening Server?
      Return -1 if Listener is NOT working anymore and should be terminated,
      Return 0 if there were no new Sockets waiting but Listener is OK,
      Return the number of waiting Sockets if there are new Sockets waiting. }
    function api_WaitingSockets:integer; virtual; abstract;

    { Get New Socket from Server Listener.
      If error no Socket waiting, return NIL. }
    function api_GetNewSocket:TRtcSocketBase; virtual; abstract;

    { This Socket was created with api_GetNewSocket.
      Using api_StartNewSocket, the connection has to be prepared for use (Bind).
      Return TRUE if connection is now ready for use, FALSE if error. }
    function api_StartNewSocket:boolean; virtual; abstract;

    { Open a new Connection (client).
      Return TRUE if connection is Open, FALSE if error connecting. }
    function api_Connect:boolean; virtual; abstract;

    { Shut down Socket (Client or Server), prepare for Closing.
      Return TRUE if success, FALSE if socket was already closed. }
    function api_Shutdown:boolean; virtual; abstract;

    { Close Socket (Client or Server).
      Return TRUE if socket closed, FALSE if error. }
    function api_Close:boolean; virtual; abstract;

    { Receive as much data as you can.
      Return TRUE if everything OK and put read data into "Str".
      Return empty Str if nothing to read but connection is OK.
      Return FALSE if error while reading.  }
    function api_ReceiveEx(var Str : RtcByteArray): boolean; virtual; abstract;

    { Send as much data a possible from "Str", starting from position "at" (first char at=1).
      Return the number of characters sent, 0 if can not send now and -1 if error. }
    function api_SendEx(var Str : RtcByteArray; at:integer):Integer; virtual; abstract;

    { Address of connected Peer }
    function api_GetPeerAddr: RtcString; virtual; abstract;
    { Port number of connected Peer }
    function api_GetPeerPort: RtcString; virtual; abstract;

    { Local connection address }
    function api_GetLocalAddr: RtcString; virtual; abstract;
    { Local connection port }
    function api_GetLocalPort: RtcString; virtual; abstract;

    { Text of the Last error }
    function api_GetLastErrorText: String; virtual; abstract;

    procedure Exec_ServerListen;
    procedure Exec_LoopListening;
    procedure Exec_NewClient;
    procedure Exec_ClientConnect;
    procedure Exec_LoopReading;
    procedure Exec_StartReading;
    procedure Exec_LoopSending;
    procedure Exec_StartSending;
    procedure Exec_PreClose;
    procedure Exec_Close;
    procedure Exec_Clear;

    function Exec_Message(id:integer):boolean;

    function GetMsg:Cardinal;
    procedure PostMsg(ID:Cardinal);

    procedure MsgEnter;
    procedure MsgLeave;

    procedure Call_GUI(meth:TRtcSyncEvent);

    procedure NeedThr;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Listen; override;
    function  GetNewSocket: TRtcSocketBase; override;
    procedure StartNewSocket; override;

    procedure Connect; override;
    procedure Close; override;

    procedure NeedMoreData; override;

    procedure Release; override;

    procedure DoMessage(Msg:word; Data:Cardinal); override;

    function ReceiveEx(var Str : RtcByteArray): Integer; override;
    function SendEx(const Str : RtcByteArray): Integer; override;
    function BuffEx(const Str : RtcByteArray): Integer; override;

    function ReceiveStr(var Str : RtcString): Integer; override;
    function SendStr(const Str : RtcString): Integer; override;
    function BuffStr(const Str : RtcString): Integer; override;

    function GetPeerAddr: RtcString; override;
    function GetPeerPort: RtcString; override;

    function GetLocalAddr: RtcString; override;
    function GetLocalPort: RtcString; override;

    function GetLastErrorText: String; override;
  end;

  TRtcSynSocket=class(TRtcSocketBaseSyn)
    (*
    FAddr: RtcString;
    FPort: RtcString;

    FProtocol: TRtcSocketProtocol;
    FReuseAddr: Boolean;
    FMultiCastAddr: RtcString;
    FMultiThreaded: boolean;
    FMultiCast: Boolean;
    FMultiCastIpTTL: Integer;
    FTimeoutsOfAPI: TRtcTimeoutsOfAPI;
    *)
  protected

    HSock:TRtcSocket;

    procedure Need_HSock;

    { Start Listener (Server).
      Return TRUE if everyting OK and Listener started. }
    function api_Listen:boolean; override;

    { How many Sockets are waiting to connect on this listening Server?
      Return -1 if Listener is NOT working anymore and should be terminated,
      Return 0 if there were no new Sockets waiting but Listener is OK,
      Return the number of waiting Sockets if there are new Sockets waiting. }
    function api_WaitingSockets:integer; override;

    { Get New Socket from Server Listener.
      If error no Socket waiting, return NIL. }
    function api_GetNewSocket:TRtcSocketBase; override;

    { This Socket was created with api_GetNewSocket.
      Using api_StartNewSocket, the connection has to be prepared for use (Bind).
      Return TRUE if connection is now ready for use, FALSE if error. }
    function api_StartNewSocket:boolean; override;

    { Open a new Connection (client).
      Return TRUE if connection is Open, FALSE if error connecting. }
    function api_Connect:boolean; override;

    { Shut down Socket (Client or Server), preparing for Close.
      Return TRUE if success, FALSE if socket was already closed. }
    function api_Shutdown:boolean; override;

    { Close Socket (Client or Server).
      Return TRUE if socket closed, FALSE if error. }
    function api_Close:boolean; override;

    { Receive as much data as you can.
      Return TRUE if everything OK and put read data into "Str".
      Return empty Str if nothing to read but connection is OK.
      Return FALSE if error while reading.  }
    function api_ReceiveEx(var Str : RtcByteArray): boolean; override;

    { Send as much data a possible from "Str", starting from position "at" (first char at=1).
      Return the number of characters sent, 0 if can not send now and -1 if error. }
    function api_SendEx(var Str : RtcByteArray; at:integer):Integer; override;

    { Address of connected Peer }
    function api_GetPeerAddr: RtcString; override;
    { Port number of connected Peer }
    function api_GetPeerPort: RtcString; override;

    { Local connection address }
    function api_GetLocalAddr: RtcString; override;
    { Local connection port }
    function api_GetLocalPort: RtcString; override;

    { Text of the Last error }
    function api_GetLastErrorText: String; override;

  public
    constructor Create; override;
    destructor Destroy; override;
    end;

implementation

const
  ID_ServerListen=0;
  ID_LoopListening=1;
  ID_NewClient=2;

  ID_ClientConnect=3;

  ID_StartReading=4;
  ID_LoopReading=5;

  ID_StartSending=6;
  ID_LoopSending=7;

  ID_Close=8;
  ID_Release=9;
  ID_MAX=9;

type
  TBaseSockMessage=class(TObject)
  public
    id:byte;
    end;

var
  Msgs:array[0..ID_Max] of TBaseSockMessage;

{ TRtcSocketBaseSyn }

constructor TRtcSocketBaseSyn.Create;
  begin
  inherited;

  FCS:=TRtcCritSec.Create;

  Socks:=tXObjList.Create(16);
  MsgQ:=tXList.Create(16);

  Thr:=nil;

  FReadBuff:=TRtcHugeByteArray.Create;
  FSendBuff:=TRtcHugeByteArray.Create;
  FDataToSend:=0;
  FDataIn:=0;
  FDataOut:=0;

  FState:=wsInvalidState;
  FLocalAddr:='';
  FLocalPort:='';
  FPeerAddr:='';
  FPeerPort:='';

  MsgLevel:=0;

  inReadLoop:=False;
  inSendLoop:=False;
  isServer:=False;
  isClient:=False;
  isActive:=False;
  SetLength(FSend,0);
  FSendAt:=0;
  end;

destructor TRtcSocketBaseSyn.Destroy;
  var
    o:TObject;
  begin
  try
    if assigned(Thr) then
      raise Exception.Create('Destroy directly!?');

    Exec_Clear;

    while not Socks.Empty do
      begin
      Socks.extractFirst(o);
      TRtcSocketBase(o).Release;
      end;
    RtcFreeAndNil(Socks);
    RtcFreeAndNil(MsgQ);

    FLocalAddr:='';
    FLocalPort:='';
    FPeerAddr:='';
    FPeerPort:='';

    RtcFreeAndNil(FReadBuff);
    RtcFreeAndNil(FSendBuff);

    RtcFreeAndNil(FCS);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSocketBaseSyn.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcSocketBaseSyn.Release;
  begin
  Exec_PreClose;
  PostMsg(ID_Release);
  end;

procedure TRtcSocketBaseSyn.Connect;
  begin
  // Client-side connections do not require a thread
  if MultiThreaded or not Blocking then NeedThr;

  PostMsg(ID_ClientConnect);
  end;

procedure TRtcSocketBaseSyn.Listen;
  begin
  // Server needs a thread for the listening loop
  if MultiThreaded or not Blocking then NeedThr;

  PostMsg(ID_ServerListen);
  end;

procedure TRtcSocketBaseSyn.Close;
  begin
  Exec_PreClose;
  PostMsg(ID_Close);
  end;

function TRtcSocketBaseSyn.GetNewSocket: TRtcSocketBase;
  var
    ores:TObject absolute Result;
  begin
  FCS.Acquire;
  try
    Socks.extractFirst(ores);
  finally
    FCS.Release;
    end;
  end;

procedure TRtcSocketBaseSyn.StartNewSocket;
  begin
  // Server-side connections need a Thread for the reading loop
  if MultiThreaded or not Blocking then NeedThr;

  PostMsg(ID_NewClient);
  end;

function TRtcSocketBaseSyn.GetLocalAddr: RtcString;
  begin
  FCS.Acquire;
  try
    Result:=FLocalAddr;
  finally
    FCS.Release;
    end;
  end;

function TRtcSocketBaseSyn.GetLocalPort: RtcString;
  begin
  FCS.Acquire;
  try
    Result:=FLocalPort;
  finally
    FCS.Release;
    end;
  end;

function TRtcSocketBaseSyn.GetPeerAddr: RtcString;
  begin
  FCS.Acquire;
  try
    Result:=FPeerAddr;
  finally
    FCS.Release;
    end;
  end;

function TRtcSocketBaseSyn.GetPeerPort: RtcString;
  begin
  FCS.Acquire;
  try
    Result:=FPeerPort;
  finally
    FCS.Release;
    end;
  end;

function TRtcSocketBaseSyn.GetLastErrorText: String;
  begin
  FCS.Acquire;
  try
    Result:=FLastErrText;
  finally
    FCS.Release;
    end;
  end;

function TRtcSocketBaseSyn.ReceiveEx(var Str: RtcByteArray): Integer;
  begin
  FCS.Acquire;
  try
    Result:=FReadBuff.Size;
    if Result>0 then
      begin
      Str:=FReadBuff.GetEx;
      FReadBuff.Clear;
      end
    else
      SetLength(Str,0);
  finally
    FCS.Release;
    end;
  end;

function TRtcSocketBaseSyn.ReceiveStr(var Str: RtcString): Integer;
  begin
  FCS.Acquire;
  try
    Result:=FReadBuff.Size;
    if Result>0 then
      begin
      Str:=FReadBuff.Get;
      FReadBuff.Clear;
      end
    else
      SetLength(Str,0);
  finally
    FCS.Release;
    end;
  end;

function TRtcSocketBaseSyn.BuffEx(const Str: RtcByteArray): Integer;
  begin
  Result:=length(Str);
  if Result>0 then
    begin
    FCS.Acquire;
    try
      FDataToSend:=FDataToSend+Cardinal(Result);
      FSendBuff.AddEx(Str);
    finally
      FCS.Release;
      end;
    end;
  end;

function TRtcSocketBaseSyn.BuffStr(const Str: RtcString): Integer;
  begin
  Result:=length(Str);
  if Result>0 then
    begin
    FCS.Acquire;
    try
      FDataToSend:=FDataToSend+Cardinal(Result);
      FSendBuff.Add(Str);
    finally
      FCS.Release;
      end;
    end;
  end;

function TRtcSocketBaseSyn.SendEx(const Str: RtcByteArray): Integer;
  begin
  Result:=length(Str);

  MsgEnter;
  FCS.Acquire;
  try
    if Result>0 then
      begin
      FSendBuff.AddEx(Str);
      FDataToSend:=FDataToSend+Cardinal(Result);
      end;
    if (FSendBuff.Size>0) and not FSending then
      begin
      FSending:=True;
      PostMsg(ID_StartSending);
      end;
  finally
    FCS.Release;
    MsgLeave;
    end;
  end;

function TRtcSocketBaseSyn.SendStr(const Str: RtcString): Integer;
  begin
  Result:=length(Str);

  MsgEnter;
  FCS.Acquire;
  try
    if Result>0 then
      begin
      FSendBuff.Add(Str);
      FDataToSend:=FDataToSend+Cardinal(Result);
      end;
    if (FSendBuff.Size>0) and not FSending then
      begin
      FSending:=True;
      PostMsg(ID_StartSending);
      end;
  finally
    FCS.Release;
    MsgLeave;
    end;
  end;

procedure TRtcSocketBaseSyn.DoMessage(Msg: word; Data: cardinal);
  begin
  case Msg of
    MSG_LISTEN: Do_Listen;
    MSG_CONNECT: Do_Connect;
    MSG_CLOSE: Do_Close;
    MSG_SENT: Do_Sent;
    MSG_READ: Do_Read;
    MSG_ACCEPT: Do_Accept;
    MSG_LISTEN_ERR: Do_ListenError;
    MSG_LISTEN_LOST: Do_ListenLost;
    MSG_CONNECT_ERR: Do_ConnectError;
    MSG_CONNECT_LOST: Do_ConnectLost;
    end;
  end;

procedure TRtcSocketBaseSyn.Do_Accept;
  begin
  On_NewSocket(0); // Signal "new client"
  end;

procedure TRtcSocketBaseSyn.Do_Close;
  begin
  if FState<>wsClosed then
    begin
    FState:=wsClosed;
    On_ChangeState(wsClosed); // Signal "Closed"
    end;
  end;

procedure TRtcSocketBaseSyn.Do_Connect;
  begin
  if FState<>wsConnected then
    begin
    FState:=wsConnected;
    On_ChangeState(wsConnected); //Signal "Connected"
    { Need to call "On_DataSent" once after connect
      to signal the connection is ready for use.}
    On_DataSent(0);
    end;
  end;

procedure TRtcSocketBaseSyn.Do_Listen;
  begin
  if FState<>wsListening then
    begin
    FState:=wsListening;
    On_ChangeState(wsListening); // Signal "Listening"
    end;
  end;

procedure TRtcSocketBaseSyn.Do_ListenError;
  begin
  if FState<>wsClosed then
    begin
    FState:=wsClosed;
    On_ChangeState(wsListenError); // Signal "ListenError"
    end;
  end;

procedure TRtcSocketBaseSyn.Do_ConnectError;
  begin
  if FState<>wsClosed then
    begin
    FState:=wsClosed;
    On_ChangeState(wsConnectError); // Signal "ConnectError"
    end;
  end;

procedure TRtcSocketBaseSyn.Do_ListenLost;
  begin
  if FState<>wsClosed then
    begin
    FState:=wsClosed;
    On_ChangeState(wsListenLost); // Signal "ListenLost"
    end;
  end;

procedure TRtcSocketBaseSyn.Do_ConnectLost;
  begin
  if FState<>wsClosed then
    begin
    FState:=wsClosed;
    On_ChangeState(wsConnectLost); // Signal "ConnectLost"
    end;
  end;

procedure TRtcSocketBaseSyn.Do_Read;
  var
    l:cardinal;
  begin
  if FState=wsConnected then
    begin
    FCS.Acquire;
    try
      l:=FDataIn;
      FDataIn:=0;
    finally
      FCS.Release;
      end;
    if l>0 then
      begin
      On_DataIn(l);
      On_DataReceived(0); // Signal "data can be Read"
      end;
    end;
  end;

procedure TRtcSocketBaseSyn.Do_Sent;
  var
    allout:boolean;
    l:cardinal;
  begin
  if FState=wsConnected then
    begin
    FCS.Acquire;
    try
      l:=FDataOut;
      FDataOut:=0;
      Dec(FDataToSend,l);
      allout:=FDataToSend=0;
    finally
      FCS.Release;
      end;
    if l>0 then
      begin
      On_DataOut(l);
      if allout then
        On_DataSent(0); // Signal "all data Sent"
      end;
    end;
  end;

procedure TRtcSocketBaseSyn.Call_Accept;
  var
    s:TRtcSocketBase;
  begin
  s:=api_GetNewSocket;
  if assigned(s) then
    begin
    FCS.Acquire;
    try
      Socks.addLast(s);
    finally
      FCS.Release;
      end;
    if not Call_ThrMessage(MSG_ACCEPT,0) then
      Call_GUI(Do_Accept);
    end;
  end;

procedure TRtcSocketBaseSyn.Call_Close;
  begin
  if not Call_ThrMessage(MSG_CLOSE,0) then
    Call_GUI(Do_Close);
  end;

procedure TRtcSocketBaseSyn.Call_Connect;
  begin
  FCS.Acquire;
  try
    FPeerAddr:=api_GetPeerAddr;
    FPeerPort:=api_GetPeerPort;
    FLocalAddr:=api_GetLocalAddr;
    FLocalPort:=api_GetLocalPort;
  finally
    FCS.Release;
    end;
  if not Call_ThrMessage(MSG_CONNECT,0) then
    Call_GUI(Do_Connect);
  end;

procedure TRtcSocketBaseSyn.Call_Listen;
  begin
  FCS.Acquire;
  try
    FPeerAddr:='';
    FPeerPort:='';
    FLocalAddr:=api_GetLocalAddr;
    FLocalPort:=api_GetLocalPort;
  finally
    FCS.Release;
    end;
  if not Call_ThrMessage(MSG_LISTEN,0) then
    Call_GUI(Do_Listen);
  end;

procedure TRtcSocketBaseSyn.Call_ListenError;
  begin
  FCS.Acquire;
  try
    FLastErrText:=api_GetLastErrorText;
  finally
    FCS.Release;
    end;
  if not Call_ThrMessage(MSG_LISTEN_ERR,0) then
    Call_GUI(Do_ListenError);
  end;

procedure TRtcSocketBaseSyn.Call_ConnectError;
  begin
  FCS.Acquire;
  try
    FLastErrText:=api_GetLastErrorText;
  finally
    FCS.Release;
    end;
  if not Call_ThrMessage(MSG_CONNECT_ERR,0) then
    Call_GUI(Do_ConnectError);
  end;

procedure TRtcSocketBaseSyn.Call_ConnectLost;
  begin
  FCS.Acquire;
  try
    FLastErrText:=api_GetLastErrorText;
  finally
    FCS.Release;
    end;
  if not Call_ThrMessage(MSG_CONNECT_LOST,0) then
    Call_GUI(Do_ConnectLost);
  end;

procedure TRtcSocketBaseSyn.Call_ListenLost;
  begin
  FCS.Acquire;
  try
    FLastErrText:=api_GetLastErrorText;
  finally
    FCS.Release;
    end;
  if not Call_ThrMessage(MSG_LISTEN_LOST,0) then
    Call_GUI(Do_ListenLost);
  end;

procedure TRtcSocketBaseSyn.Call_ReadEx(const data:RtcByteArray);
  begin
  FCS.Acquire;
  try
    // Reset "reading" flag (exiting reading loop)
    FReading:=False;
    // Add received data to reading buffer
    FReadBuff.AddEx(data);
    FDataIn:=FDataIn+Cardinal(length(data));
  finally
    FCS.Release;
    end;
  if not Call_ThrMessage(MSG_READ,0) then
    Call_GUI(Do_Read);
  end;

procedure TRtcSocketBaseSyn.Call_Sent(Len:Cardinal);
  begin
  FCS.Acquire;
  try
    FDataOut:=FDataOut+Len;
  finally
    FCS.Release;
    end;
  if not Call_ThrMessage(MSG_SENT,0) then
    Call_GUI(Do_Sent);
  end;

procedure TRtcSocketBaseSyn.NeedMoreData;
  begin
  MsgEnter;
  FCS.Acquire;
  try
    if not FReading then
      begin
      FReading:=True;
      PostMsg(ID_StartReading);
      end;
  finally
    FCS.Release;
    MsgLeave;
    end;
  end;

function TRtcSocketBaseSyn.GetDataToSend: RtcByteArray;
  begin
  FCS.Acquire;
  try
    if FSendBuff.Size>0 then
      begin
      Result:=FSendBuff.GetEx;
      FSendBuff.Clear;
      end
    else
      begin
      FSending:=False;
      SetLength(Result,0);
      end;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcSocketBaseSyn.Exec_ServerListen;
  begin
  if isServer then Exit;

  if Reconnecting then
    begin
    Reconnecting:=False;
    Sleep(RTC_WAIT_BEFORE_RESTART);
    end;

  if api_Listen then
    begin
    isServer:=True;
    isActive:=True;
    Call_Listen;
    PostMsg(ID_LoopListening);
    end
  else
    Call_ListenError;
  end;

procedure TRtcSocketBaseSyn.Exec_LoopListening;
  var
    cnt,a:shortint;
  begin
  if not isServer then Exit;

  cnt:=api_WaitingSockets;
  if cnt>=0 then
    begin
    if cnt>0 then
      for a:=1 to cnt do
        Call_Accept;
    PostMsg(ID_LoopListening)
    end
  else
    begin
    isServer:=False;
    Call_ListenLost;
    end;
  end;

procedure TRtcSocketBaseSyn.Exec_NewClient;
  begin
  if isClient then Exit;

  isClient:=True;
  if api_StartNewSocket then
    begin
    isActive:=True;
    SetLength(FSend,0);
    FSendAt:=0;
    Call_Connect;
    { Call_Connect will use "NeedMoreData" if we have to go into the reading loop
      or "Send" if we have data for sending and need to go into the sending loop. }
    end
  else
    begin
    isClient:=False;
    Call_ConnectError;
    end;
  end;

procedure TRtcSocketBaseSyn.Exec_ClientConnect;
  begin
  if isClient then Exit;

  if Reconnecting then
    begin
    Reconnecting:=False;
    Sleep(RTC_WAIT_BEFORE_RECONNECT); // avoid flooding the CPU with reconnects
    end;

  isClient:=True;
  if api_Connect then
    begin
    isActive:=True;
    SetLength(FSend,0);
    FSendAt:=0;
    Call_Connect;
    { Call_Connect will use "NeedMoreData" if we have to go into the reading loop
      or "Send" if we have data for sending need to go into the sending loop. }
    end
  else
    begin
    isClient:=False;
    Call_ConnectError;
    end;
  end;

procedure TRtcSocketBaseSyn.Exec_LoopReading;
  var
    tmp:RtcByteArray;
  begin
  if not inReadLoop then Exit;

  SetLength(tmp,0);
  {if inSendLoop then // sending? skip 1 reading pass ...
    PostMsg(ID_LoopReading)
  else} if api_ReceiveEx(tmp) then
    begin
    if length(tmp)>0 then
      begin
      inReadLoop:=False;
    { Call_Read will put "tmp" data into receiving buffer and
      reset the "FReading" flag prior to calling connection events.
      Connection events will then use "NeedMoreData" to start a new loop if needed. }
      Call_ReadEx(tmp);
      end
    else if inReadLoop then // nothing read, but connection still OK. Continue loop ...
      PostMsg(ID_LoopReading);
    end
  else // error reading. Stop looping and disconnect ...
    begin
    inReadLoop:=False;
    isClient:=False; // do not trigger "disconnect" events
    Call_ConnectLost;
    end;
  end;

procedure TRtcSocketBaseSyn.Exec_StartReading;
  begin
  if inReadLoop then Exit;

  inReadLoop:=True;
  Exec_LoopReading;
  end;

procedure TRtcSocketBaseSyn.Exec_LoopSending;
  var
    fout:integer;
  begin
  if not inSendLoop then Exit;

  if FSendAt=0 then // local sending buffer empty
    begin
    { GetDataToSend will set "FSending" flag to FALSE
      and return an empty string if nothing left to send. }
    FSend:=GetDataToSend;
    if length(FSend)>0 then
      FSendAt:=1;
    end;

  if FSendAt=0 then // all data sent and "FSending" flag set to FALSE
    { Connection events can use "Send" to prepare more data and start a new sending loop. }
    inSendLoop:=False
  else
    begin
    fout:=api_SendEx(FSend,FSendAt);
    if fout<0 then // error sending. Stop looping and disconnect ...
      begin
      inSendLoop:=False;
      isClient:=False; // do not trigger "disconnect" events
      Call_ConnectLost;
      end
    else
      begin
      if fout>0 then
        begin
        Inc(FSendAt,fout);
        if FSendAt>length(FSend) then
          begin
          { GetDataToSend will set "FSending" flag to FALSE
            and return an empty string if nothing left to send. }
          FSend:=GetDataToSend;
          if length(FSend)>0 then
            FSendAt:=1
          else
            FSendAt:=0;
          end;
        end;
      if FSendAt=0 then // all sent, buffer empty
        { Connection events can use "Send" to prepare more data and start a new sending loop. }
        inSendLoop:=False
      else if inSendLoop then
        PostMsg(ID_LoopSending);
      if fout>0 then
        Call_Sent(fout);
      end;
    end;
  end;

procedure TRtcSocketBaseSyn.Exec_StartSending;
  begin
  if inSendLoop then Exit;

  inSendLoop:=True;
  Exec_LoopSending;
  end;

procedure TRtcSocketBaseSyn.Exec_PreClose;
  begin
  // Shutdown connection
  api_Shutdown;
  // stop all loops
  inReadLoop:=False;
  inSendLoop:=False;
  end;

procedure TRtcSocketBaseSyn.Exec_Close;
  begin
  if isActive then
    begin
    isActive:=False;
    api_Close;
    end;

  // stop all loops
  inReadLoop:=False;
  inSendLoop:=False;

  if isClient or isServer then
    begin
    isClient:=False;
    isServer:=False;
    Call_Close;
    end;
  end;

procedure TRtcSocketBaseSyn.Exec_Clear;
  begin
  inReadLoop:=False;
  inSendLoop:=False;
  isServer:=False;
  isClient:=False;
  // This will close the socket connection if open
  if isActive then
    begin
    isActive:=False;
    api_Close;
    end;
  end;

function TRtcSocketBaseSyn.GetMsg: Cardinal;
  var
    ores:RtcIntPtr;
  begin
  MsgQ.extractFirst(ores);
  Result:=ores;
  end;

procedure TRtcSocketBaseSyn.PostMsg(ID: Cardinal);
  begin
  if assigned(Thr) then
    TRtcThread.PostJob(Thr,Msgs[ID])
  else
    begin
    MsgEnter;
    MsgQ.addLast(ID);
    MsgLeave;
    end;
  end;

procedure TRtcSocketBaseSyn.MsgEnter;
  begin
  Inc(MsgLevel);
  end;

procedure TRtcSocketBaseSyn.MsgLeave;
  function LoopMsg:boolean;
    begin
    Inc(MsgLevel);
    try
      repeat
        Result:=Exec_Message(GetMsg);
        until Result or MsgQ.Empty;
    finally
      Dec(MsgLevel);
      end;
    end;
  begin
  Dec(MsgLevel);
  if MsgLevel=0 then
    if not MsgQ.Empty then
      if LoopMsg then
        {$IFDEF NEXTGEN} DisposeOf; {$ELSE} Free; {$ENDIF}
  end;

procedure TRtcSocketBaseSyn.Call_GUI(meth: TRtcSyncEvent);
  begin
  if assigned(Thr) then
    begin
    if InsideMainThread then
      meth
    else
      TRtcThread.Sync(meth);
    end
  else
    meth;
  end;

function TRtcSocketBaseSyn.Exec_Message(id: integer): boolean;
  begin
  Result:=False;
  case id of
    ID_ServerListen:    Exec_ServerListen;
    ID_LoopListening:   Exec_LoopListening;

    ID_NewClient:       Exec_NewClient;
    ID_ClientConnect:   Exec_ClientConnect;

    ID_StartReading:    Exec_StartReading;
    ID_LoopReading:     Exec_LoopReading;

    ID_StartSending:    Exec_StartSending;
    ID_LoopSending:     Exec_LoopSending;

    ID_Close:           Exec_Close;

    ID_Release:         begin
                        Exec_Close;
                        Result:=True;
                        end;
    else
      raise Exception.Create('Unknown ID '+IntToStr(id));
    end;
  end;

procedure TRtcSocketBaseSyn.NeedThr;
  begin
  if not assigned(Thr) then
    begin
    Thr:=TRtcSockSynThread.Create;
    Thr.Sock:=self;
    end;
  end;

{ TRtcSockSynThread }

constructor TRtcSockSynThread.Create;
  begin
  inherited;
  end;

destructor TRtcSockSynThread.Destroy;
  begin
  try
    if assigned(Sock) then
      begin
      Sock.Exec_Clear;
      Sock.Thr:=nil;
      RtcFreeAndNil(Sock);
      end;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSockSynThread.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcSockSynThread.RunJob: boolean;
  begin
  try
    if Job is TBaseSockMessage then
      begin
      if assigned(Sock) then
        begin
        try
          Result:=Sock.Exec_Message(TBaseSockMessage(Job).id);
        except
          on E:Exception do
            begin
            if LOG_AV_ERRORS then
              Log('TRtcSockSynThread.RunJob Sock.Exec_Message('+RtcString(IntToStr(TBaseSockMessage(Job).id))+')',E,'ERROR');
            Result:=True;
            end;
          end;
        end
      else
        Result:=True; // if Sock is not assigned, something went wrong.
      end
    else
      try
        Result:=inherited RunJob;
      except
        on E:Exception do
          begin
          if LOG_AV_ERRORS then
            Log('TRtcSockSynThread.RunJob('+RtcString(Job.ClassName)+')',E,'ERROR');
          Result:=True;
          end;
        end;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSockSynThread.RunJob',E,'ERROR');
      Result:=True; // raise;
      end;
    end;
  end;

type
  TMySockBaseSyn=class
    constructor Create;
    destructor Destroy; override;
    end;

var
  MySock:TMySockBaseSyn;

{ TMySockBaseSyn }

constructor TMySockBaseSyn.Create;
  var
    a:integer;
  begin
  inherited;
  for a:=0 to ID_MAX do
    begin
    Msgs[a]:=TBaseSockMessage.Create;
    Msgs[a].id:=a;
    end;
  end;

destructor TMySockBaseSyn.Destroy;
  var
    a:integer;
  begin
  try
    for a:=0 to ID_MAX do
      RtcFreeAndNil(Msgs[a]);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TMySockBaseSyn.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcSynSocket }

constructor TRtcSynSocket.Create;
  begin
  inherited;
  HSock:=nil;
  end;

destructor TRtcSynSocket.Destroy;
  begin
  try
    RtcFreeAndNil(HSock);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSynSocket.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcSynSocket.Need_HSock;
  begin
  if not assigned(HSock) then
    HSock:=TRtcSocket.Create;
  end;

function TRtcSynSocket.api_Connect: boolean;
  begin
  Need_HSock;
  Result:=HSock.Connect(FAddr,FPort,TimeoutsOfAPI,PreferIP4,PreferIPDef);
  end;

function TRtcSynSocket.api_Listen: boolean;
  begin
  Need_HSock;
  Result:=HSock.Listen(FAddr,FPort,TimeoutsOfAPI,PreferIP4,PreferIPDef);
  end;

function TRtcSynSocket.api_WaitingSockets: integer;
  begin
  Result:=HSock.WaitingSockets(RTC_SYNSOCK_LISTEN_WAIT);
  end;

function TRtcSynSocket.api_GetNewSocket: TRtcSocketBase;
  var
    cl:TRtcSocketBaseClass;
    Sck:TRtcSocket;
  begin
  Result:=nil;
  Sck:=HSock.GetNewSocket;
  if assigned(Sck) then
    begin
    cl:=TRtcSocketBaseClass(ClassType);
    Result:=cl.Create;
    TRtcSynSocket(Result).HSock:=Sck;
    end;
  end;

function TRtcSynSocket.api_StartNewSocket: boolean;
  begin
  Result:=HSock.StartNewSocket(PreferIP4);
  end;

function TRtcSynSocket.api_Close: boolean;
  begin
  if assigned(HSock) then
    Result:=HSock.Close
  else
    Result:=False;
  end;

function TRtcSynSocket.api_Shutdown: boolean;
  begin
  if assigned(HSock) then
    Result:=HSock.Shut_down
  else
    Result:=False;
  end;

function TRtcSynSocket.api_ReceiveEx(var Str: RtcByteArray): boolean;
  begin
  Result:=HSock.ReceiveEx(Str,RTC_SYNSOCK_READ_WAIT);
  end;

function TRtcSynSocket.api_SendEx(var Str: RtcByteArray; at: integer): Integer;
  begin
  Result:=HSock.SendEx(Str,at,RTC_SYNSOCK_SEND_WAIT);
  end;

function TRtcSynSocket.api_GetLastErrorText: String;
  begin
  Result:=HSock.GetLastErrorText;
  end;

function TRtcSynSocket.api_GetLocalAddr: RtcString;
  begin
  Result:=HSock.GetLocalAddr;
  end;

function TRtcSynSocket.api_GetLocalPort: RtcString;
  begin
  Result:=HSock.GetLocalPort;
  end;

function TRtcSynSocket.api_GetPeerAddr: RtcString;
  begin
  Result:=HSock.GetPeerAddr;
  end;

function TRtcSynSocket.api_GetPeerPort: RtcString;
  begin
  Result:=HSock.GetPeerPort;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcSynSocket Initializing ...','DEBUG');{$ENDIF}

MySock:=TMySockBaseSyn.Create;

{$IFDEF RTC_DEBUG} Log('rtcSynSocket Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcSynSocket Finalizing ...','DEBUG');{$ENDIF}

CloseThreadPool;
RtcFreeAndNil(MySock);

{$IFDEF RTC_DEBUG} Log('rtcSynSocket Finalized.','DEBUG');{$ENDIF}
end.
