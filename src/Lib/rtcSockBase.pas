{
  Base Socket class (abstract)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @exclude
}
unit rtcSockBase;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcSystem,
  rtcThrPool,
  rtcLog;

const
  // Maximum MSG ID in OnMessage()
  RTC_SOCKET_MAXMSG=20;

var
  LOG_SOCKET_ERRORS         :boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};
  LOG_MESSAGE_ERRORS        :boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};
  LOG_EVENT_ERRORS          :boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};
  LOG_REFUSED_CONNECTIONS   :boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};
  LOG_PLUGIN_ERRORS         :boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

  { Common socket errors are errors which are expected to happen if your network
    is under heavy load, or if your clients are accessing your server over the internet.
    Logging these errors will cause your "SOCK.log" file to grow rapidly and is
    recommended ONLY if all your Clients are running from inside LAN and you want
    to test your Network if it can handle the load without a single connection
    getting aborted or connect attempts getting refused by the server. }
  LOG_COMMON_SOCKET_ERRORS  :boolean=False;

type
  TRtcSockMessage=class(TRtcBaseMessage)
  public
    Msg:word;
    end;

var
  Message_WsSock:array[0..RTC_SOCKET_MAXMSG] of TRtcSockMessage;

type
  TRtcSocketProtocol = (spTcp, spUdp);

  TRtcSocketState       = (wsInvalidState,
                           wsOpened,     wsBound,
                           wsConnecting, wsSocksConnected, wsConnected,
                           wsAccepting,  wsListening, wsClosed,
                           // used by Synchronous Sockets to notify about connection errors
                           wsListenError, wsListenLost, wsConnectError, wsConnectLost
                           );

  TRtcSocketEvent = procedure (Sender: TObject; ErrCode: Word) of object;
  TRtcSocketMsgEvent = procedure (Sender: TObject; Msg: Word; Data: Cardinal) of object;
  TRtcSocketDataEvent = procedure (Sender: TObject; Len:Cardinal) of object;
  TRtcSocketStateEvent = procedure (Sender: TObject; NewState : TRtcSocketState) of object;
  TRtcSocketExceptEvent  = procedure (Sender : TObject; E : Exception; var CanClose : Boolean) of object;

  TRtcSocketBase = class;
  TRtcSocketBaseClass = class of TRtcSocketBase;

  TRtcSocketMesDataJob=class(TRtcJob)
  public
    Sock:TRtcSocketBase;
    Msg:word;
    Data:Cardinal;

    destructor Destroy; override;
    function Run(Thr:TRtcThread):boolean; override;
    end;

  TRtcSocketBase=class(TObject)
  private
    FOnChangeState      : TRtcSocketStateEvent;
    FOnNewSocket        : TRtcSocketEvent;
    FOnDataReceived     : TRtcSocketEvent;
    FOnDataSent         : TRtcSocketEvent;
    FOnDnsLookupDone    : TRtcSocketEvent;
    FOnError            : TRtcSocketEvent;
    FOnDataOut          : TRtcSocketDataEvent;
    FOnDataIn           : TRtcSocketDataEvent;
    FOnBgException      : TRtcSocketExceptEvent;

    FEventsOff: boolean;

    FReconnecting: boolean;
    FMessageThread: TRtcThread;

    function GetEventsOff: boolean;
    procedure SetEventsOff(const Value: boolean);

    function GetMessageThread: TRtcThread;
    procedure SetMessageThread(const Value: TRtcThread);

  protected
    FBlocking: boolean;
    FMultiCastAddr: RtcString;
    FAddr: RtcString;
    FPort: RtcString;
    FMultiThreaded: boolean;
    FMultiCast: Boolean;
    FReuseAddr: Boolean;
    FMultiCastIpTTL: Integer;
    FProtocol: TRtcSocketProtocol;
    FPreferIP4: boolean;
    FPreferIPDef: boolean;

    FTimeoutsOfAPI: TRtcTimeoutsOfAPI;

    procedure SetAddr(const Value: RtcString); virtual;
    procedure SetMultiCast(const Value: Boolean); virtual;
    procedure SetMultiCastAddr(const Value: RtcString); virtual;
    procedure SetMultiCastIpTTL(const Value: Integer); virtual;
    procedure SetMultiThreaded(const Value: boolean); virtual;
    procedure SetPort(const Value: RtcString); virtual;
    procedure SetProtocol(const Value: TRtcSocketProtocol); virtual;
    procedure SetReuseAddr(const Value: Boolean); virtual;
    procedure SetPreferIP4(const Value: Boolean); virtual;
    procedure SetPreferIPDef(const Value: Boolean); virtual;

  { Always use On_* events to call an event. This will allow any
    TRtcSocketBase class to be extended with proprietary functionality
    or use breakpoints inside this class for easy debugging. }

    function On_ChangeState(NewState : TRtcSocketState):boolean; virtual;
    function On_NewSocket(ErrCode: Word):boolean; virtual;
    function On_DataReceived(ErrCode: Word):boolean; virtual;
    function On_DataSent(ErrCode: Word):boolean; virtual;
    function On_DnsLookupDone(ErrCode: Word):boolean; virtual;
    function On_Error(ErrCode: Word):boolean; virtual;
    function On_DataOut(Len:Cardinal):boolean; virtual;
    function On_DataIn(Len:Cardinal):boolean; virtual;
    function On_BgException(E : Exception; var CanClose : Boolean):boolean; virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure NeedMoreData; virtual; abstract;

    procedure Listen; virtual; abstract;
    function  GetNewSocket: TRtcSocketBase; virtual; abstract;
    procedure StartNewSocket; virtual; abstract;

    procedure Connect; virtual; abstract;
    procedure Close; virtual; abstract;
    procedure Release; virtual; abstract;

    { Call_Message and Call_ThrMessage are a forwarders for the DoMessage method,
      which will make sure the code in the DoMessage method is executed from the
      thread associated with this socket connection.

      1) If  MessageThread is NOT set, DoMessage method has to be executed from the
         MAIN THREAD and NEVER from any background threads created by the component.
         If the component is creating background threads but MessageThread is NOT set,
         the component will need to *SYNCHRONIZE* all DoMessage calls!

      2) You may either call other events (OnDataOut, OnDataIn, OnDataReceived, etc) from
         methods specified on this abstract class (provided you do NOT call these methods directly),
         since these methods will be called from the thread associated with the connection object,
         or ... you may call other events from code called (directly or indirectly) by DoMessage.

      3) If using a background thread for communication, waiting or processing,
         make sure to ALWAYS do this through CallMessage!  }
    procedure Call_Message(Msg:word; Data:Cardinal; HighPriority:boolean=False; ForceThread:boolean=False);
    function Call_ThrMessage(Msg:word; Data:Cardinal; HighPriority:boolean=False; ForceThread:boolean=False):boolean;
    
    procedure DoMessage(Msg:word; Data:Cardinal); virtual; abstract;

    function ReceiveEx(var Str : RtcByteArray): Integer; virtual; abstract;
    function SendEx(const Str : RtcByteArray): Integer; virtual; abstract;
    function BuffEx(const Str : RtcByteArray): Integer; virtual; abstract;

    function ReceiveStr(var Str : RtcString): Integer; virtual; abstract;
    function SendStr(const Str : RtcString): Integer; virtual; abstract;
    function BuffStr(const Str : RtcString): Integer; virtual; abstract;

    function GetPeerAddr: RtcString; virtual; abstract;
    function GetPeerPort: RtcString; virtual; abstract;

    function GetLocalAddr: RtcString; virtual; abstract;
    function GetLocalPort: RtcString; virtual; abstract;

    function GetLastErrorText: String; virtual; abstract;

    property MultiThreaded: boolean        read FMultiThreaded write SetMultiThreaded;
    property Protocol: TRtcSocketProtocol  read FProtocol write SetProtocol;

    property Addr: RtcString        read FAddr write SetAddr;
    property Port: RtcString        read FPort write SetPort;
    property PreferIP4: boolean     read FPreferIP4 write SetPreferIP4;
    property PreferIPDef: boolean   read FPreferIPDef write SetPreferIPDef;

    property UdpMultiCast        : Boolean      read  FMultiCast write SetMultiCast;
    property UdpMultiCastAddr    : RtcString   read  FMultiCastAddr write SetMultiCastAddr;
    property UdpMultiCastIpTTL   : Integer      read  FMultiCastIpTTL write SetMultiCastIpTTL;
    property UdpReuseAddr        : Boolean      read  FReuseAddr write SetReuseAddr;

    property LastErrorText: String read GetLastErrorText;

    property Blocking: boolean read FBlocking write FBlocking;
    property Reconnecting: boolean read FReconnecting write FReconnecting;

    property TimeoutsOfAPI:TRtcTimeoutsOfAPI read FTimeoutsOfAPI write FTimeoutsOfAPI;

    property EventsOff: boolean read GetEventsOff write SetEventsOff;

    property MessageThread:TRtcThread read GetMessageThread write SetMessageThread;

    property OnDataOut : TRtcSocketDataEvent    read  FOnDataOut write FOnDataOut;
    property OnDataIn : TRtcSocketDataEvent     read  FOnDataIn write FOnDataIn;
    property OnDataReceived : TRtcSocketEvent   read  FOnDataReceived write FOnDataReceived;
    property OnDataSent : TRtcSocketEvent       read  FOnDataSent write FOnDataSent;
    property OnNewSocket : TRtcSocketEvent      read  FOnNewSocket write FOnNewSocket;
    property OnDnsLookupDone : TRtcSocketEvent  read  FOnDnsLookupDone write FOnDnsLookupDone;
    property OnError : TRtcSocketEvent          read  FOnError write FOnError;

    property OnChangeState : TRtcSocketStateEvent   read FOnChangeState write FOnChangeState;
    property OnBgException : TRtcSocketExceptEvent  read FOnBgException write FOnBgException;
  end;

implementation

{ TRtcSocketBase }

procedure TRtcSocketBase.Call_Message(Msg: word; Data: Cardinal; HighPriority:boolean=False; ForceThread:boolean=False);
  var
    job:TRtcSocketMesDataJob;
    mt:TRtcThread;
  begin
  if not FEventsOff then
    begin
    mt:=FMessageThread;
    if assigned(mt) then
      begin
      if (Data=0) and (Msg<=RTC_SOCKET_MAXMSG) then
        TRtcThread.PostJob(mt,Message_WsSock[Msg],HighPriority,ForceThread)
      else
        begin
        job:=TRtcSocketMesDataJob.Create;
        job.Sock:=self;
        job.Msg:=Msg;
        job.Data:=Data;
        if not TRtcThread.PostJob(mt,job,HighPriority,ForceThread) then
          if job.SingleUse then
            RtcFreeAndNil(job);
        end;
      end
    else
      DoMessage(Msg,Data);
    end;
  end;

function TRtcSocketBase.Call_ThrMessage(Msg: word; Data: Cardinal; HighPriority:boolean=False; ForceThread:boolean=False):boolean;
  var
    job:TRtcSocketMesDataJob;
    mt:TRtcThread;
  begin
  if FEventsOff then
    Result:=True // events off, do not try to execute
  else
    begin
    mt:=FMessageThread;
    if assigned(mt) then
      begin
      Result:=True; // executed, so you do not have to
      if (Data=0) and (Msg<=RTC_SOCKET_MAXMSG) then
        TRtcThread.PostJob(mt,Message_WsSock[Msg],HighPriority,ForceThread)
      else
        begin
        job:=TRtcSocketMesDataJob.Create;
        job.Sock:=self;
        job.Msg:=Msg;
        job.Data:=Data;
        if not TRtcThread.PostJob(mt,job,HighPriority,ForceThread) then
          if job.SingleUse then
            RtcFreeAndNil(job);
        end;
      end
    else
      Result:=False; // not executed, please execute yourself
    end;
  end;

constructor TRtcSocketBase.Create;
  begin
  inherited;
  FEventsOff:=False;
  FBlocking:=False;
  FReconnecting:=False;
  end;

destructor TRtcSocketBase.Destroy;
  begin
  try
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSocketBase.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcSocketBase.GetEventsOff: boolean;
  begin
  Result:=FEventsOff;
  end;

function TRtcSocketBase.GetMessageThread: TRtcThread;
  begin
  Result:=FMessageThread;
  end;

function TRtcSocketBase.On_BgException(E: Exception; var CanClose: Boolean):boolean;
  begin
  if FEventsOff then
    Result:=True
  else
    begin
    Result:=assigned(FOnBgException);
    if Result then
      FOnBgException(self,E,CanClose);
    end;
  end;

function TRtcSocketBase.On_ChangeState(NewState: TRtcSocketState):boolean;
  begin
  if FEventsOff then
    Result:=True
  else
    begin
    Result:=assigned(FOnChangeState);
    if Result then
      FOnChangeState(self,NewState);
    end;
  end;

function TRtcSocketBase.On_DataIn(Len: Cardinal):boolean;
  begin
  if FEventsOff then
    Result:=True
  else
    begin
    Result:=assigned(FOnDataIn);
    if Result then
      FOnDataIn(self,Len);
    end;
  end;

function TRtcSocketBase.On_DataOut(Len: Cardinal):boolean;
  begin
  if FEventsOff then
    Result:=True
  else
    begin
    Result:=assigned(FOnDataOut);
    if Result then
      FOnDataOut(self,Len);
    end;
  end;

function TRtcSocketBase.On_DataReceived(ErrCode: Word):boolean;
  begin
  if FEventsOff then
    Result:=True
  else
    begin
    Result:=assigned(FOnDataReceived);
    if Result then
      FOnDataReceived(self,ErrCode);
    end;
  end;

function TRtcSocketBase.On_DataSent(ErrCode: Word):boolean;
  begin
  if FEventsOff then
    Result:=True
  else
    begin
    Result:=assigned(FOnDataSent);
    if Result then
      FOnDataSent(self,ErrCode);
    end;
  end;

function TRtcSocketBase.On_DnsLookupDone(ErrCode: Word):boolean;
  begin
  if FEventsOff then
    Result:=True
  else
    begin
    Result:=assigned(FOnDnsLookupDone);
    if Result then
      FOnDnsLookupDone(self,ErrCode);
    end;
  end;

function TRtcSocketBase.On_Error(ErrCode: Word):boolean;
  begin
  if FEventsOff then
    Result:=True
  else
    begin
    Result:=assigned(FOnError);
    if Result then
      FOnError(self,ErrCode);
    end;
  end;

function TRtcSocketBase.On_NewSocket(ErrCode: Word):boolean;
  begin
  if FEventsOff then
    Result:=True
  else
    begin
    Result:=assigned(FOnNewSocket);
    if Result then
      FOnNewSocket(self,ErrCode);
    end;
  end;

procedure TRtcSocketBase.SetAddr(const Value: RtcString);
begin
  FAddr := Value;
end;

procedure TRtcSocketBase.SetEventsOff(const Value: boolean);
begin
  FEventsOff:=Value;
end;

procedure TRtcSocketBase.SetMessageThread(const Value: TRtcThread);
begin
  FMessageThread:=Value;
end;

procedure TRtcSocketBase.SetMultiCast(const Value: Boolean);
begin
  FMultiCast := Value;
end;

procedure TRtcSocketBase.SetMultiCastAddr(const Value: RtcString);
begin
  FMultiCastAddr := Value;
end;

procedure TRtcSocketBase.SetMultiCastIpTTL(const Value: Integer);
begin
  FMultiCastIpTTL := Value;
end;

procedure TRtcSocketBase.SetMultiThreaded(const Value: boolean);
begin
  FMultiThreaded := Value;
end;

procedure TRtcSocketBase.SetPort(const Value: RtcString);
begin
  FPort := Value;
end;

procedure TRtcSocketBase.SetPreferIP4(const Value: Boolean);
begin
  FPreferIP4 := Value;
end;

procedure TRtcSocketBase.SetPreferIPDef(const Value: Boolean);
begin
  FPreferIPDef := Value;
end;

procedure TRtcSocketBase.SetProtocol(const Value: TRtcSocketProtocol);
begin
  FProtocol := Value;
end;

procedure TRtcSocketBase.SetReuseAddr(const Value: Boolean);
begin
  FReuseAddr := Value;
end;

{ TRtcSocketMesDataJob }

destructor TRtcSocketMesDataJob.Destroy;
  begin
  Sock:=nil;
  inherited;
  end;

function TRtcSocketMesDataJob.Run(Thr: TRtcThread):boolean;
  begin
  if assigned(Sock) then
    begin
    Sock.DoMessage(Msg,Data);
    Sock:=nil;
    end;
  Result:=True;
  end;

type
  TRtcSockBaseUnit=class
    public
    constructor Create;
    destructor Destroy; override;
    end;

var
  mySock:TRtcSockBaseUnit;

{ TMySocketSrvProv }

constructor TRtcSockBaseUnit.Create;
  var
    a:integer;
  begin
  inherited;
  for a:=0 to RTC_SOCKET_MAXMSG do
    begin
    Message_WSSock[a]:=TRtcSockMessage.Create;
    Message_WsSock[a].Msg:=a;
    end;
  end;

destructor TRtcSockBaseUnit.Destroy;
  var
    a:integer;
  begin
  try
    for a:=0 to RTC_SOCKET_MAXMSG do
      RtcFreeAndNil(Message_WsSock[a]);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSockBaseUnit.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcSockBase Initializing ...','DEBUG');{$ENDIF}

MySock:=TRtcSockBaseUnit.Create;

{$IFDEF RTC_DEBUG} Log('rtcSockBase Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcSockBase Finalizing ...','DEBUG');{$ENDIF}

CloseThreadPool;
RtcFreeAndNil(MySock);

{$IFDEF RTC_DEBUG} Log('rtcSockBase Finalized.','DEBUG');{$ENDIF}
end.
