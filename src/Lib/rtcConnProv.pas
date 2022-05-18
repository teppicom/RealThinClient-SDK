{
  "Connection Provider wrapper"
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br>)

  This unit defines the connection provider component wrappers, which define the methods
  and properties every connection provider has to implement. RealThinClient SDK
  uses Connection Providers to implement connection functionality using low-level
  system code, while the Connection components themselves do not have any
  system-dependant code and therefor are 100% portable between all systems.
  @html(<br><br>)

  Connections create and use connection providers internaly and completely transparent
  from the connection component user. This lose coupling between the connection component
  and the connection provider makes it relatively easy to port the RTC connection components
  to any system, and ... port user code to other systems without code modifications.

  @exclude
}
unit rtcConnProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcSystem,
  rtcThrPool,
  rtcPlugins,
  rtcLog;

type
  // Supported connection states
  TRtcConnectionState = (
                        // Connection inactive, no connection provider
                        conInactive,
                        // Connection provider created, but not connected
                        conPrepared,
                        // Connection was active, now closing
                        conClosing,
                        // Trying to connect
                        conActivating,
                        // Connection Handshake in progress
                        conHandshake,
                        // Connection active
                        conActive,
                        // Listener (waiting for connections)
                        conListening
                        );

  { Exception raised when the limit for client connections (specified in rtcConn)
    was reached and therefor a new connection could not be opened. }
  EClientLimitReached = class(Exception);

  // Standard connection provider event (no parameters)
  TRtcBasicEvent = TRtcSyncEvent;
  // Connection provider event with a boolean parameter
  TRtcBoolEvent = procedure(Value:boolean) of object;
  // Exception handling event for the connection provider
  TRtcExceptEvent = procedure(E:Exception) of object;

  // Error handling event for the connection provider
  TRtcErrEvent = procedure(const Err:String) of object;
  // Event for creating a new connection provider
  TRtcProviderEvent = procedure(var Provider:TObject) of object;

  { @name is a basic wrapper for any connection provider component.
    It which defines abstract methods and some properties every connection provider
    has to implement. RTC Component suite uses Connection Providers to implement connection
    functionality using low-level system code, while the Connection components themselves
    do not have any system-dependant code and therefor are 100% portable between all systems.

    Connections create and use connection providers internaly and completely transparent
    from the connection component user. This lose coupling between the connection component
    and the connection provider makes it relatively easy to port the RTC connection components
    to any system. And even more important, port user's code to other systems,
    without major code modifications. }
  TRtcConnectionProvider = class(TObject)
  private
    FInSync:boolean;
    FMultiThreaded:boolean;

    FAddr:RtcString;
    FPort:RtcString;
    FIPV:RtcIPV;

    FLost,
    FClosing,
    FSilent:boolean;

    // Used for triggering events (provider declares those as virtual abstract)

    FError:TRtcErrEvent;
    FCS:TRtcCritSec;

    FOnReadyToRelease:TRtcBasicEvent;

    FOnBeforeCreate:TRtcBasicEvent;
    FOnAfterDestroy:TRtcBasicEvent;

    FOnConnecting:TRtcBasicEvent;
    FOnConnect:TRtcBasicEvent;
    FOnDisconnect:TRtcBasicEvent;
    FOnDisconnecting:TRtcBasicEvent;

    FOnDataSent:TRtcBasicEvent;
    FOnDataOut:TRtcBasicEvent;
    FOnDataIn:TRtcBasicEvent;
    FOnLastWrite:TRtcBasicEvent;
    FOnDataReceived:TRtcBasicEvent;
    FOnDataLost:TRtcBasicEvent;

    FOnException:TrtcExceptEvent;

    procedure SetLost(a:boolean);
    function GetLost:boolean;

    procedure SetClosing(a:boolean);
    function GetClosing:boolean;

    procedure SetSilent(a:boolean);
    function GetSilent:boolean;

  protected
    { Connection state }
    FState:TRtcConnectionState;
    { Conncetion provider has to set FDataOut to the number of byte sent out,
      before it calls the TriggerDataOut method. }
    FDataOut:int64;
    { Conncetion provider has to set FDataIn to the number of byte read in,
      before it calls the TriggerDataIn method. }
    FDataIn:int64;
    { Are we using Encryption? }
    FDataCrypt:boolean;
    { Negotiated Protocol }
    FDataProtocol:TRtcCryptPluginProtocol;

    procedure CleanUp; virtual;

    procedure Enter;
    procedure Leave;

    { Properties ready for usage by the connection provider (not used directly by the connection) }

    property Lost:boolean read GetLost write SetLost;
    property Closing:boolean read GetClosing write SetClosing;

    { Triggers to be used by the ConncetionProvider.
      Connection sets those triggers using SetTrigger_ methods (below).
      Connection provider should use those methods to trigger
      all or most of those events, when they happen. }

    procedure Error(const text:String); virtual;

    procedure TriggerReadyToRelease; virtual;

    procedure TriggerBeforeCreate; virtual;
    procedure TriggerAfterDestroy; virtual;

    procedure TriggerConnecting; virtual;
    procedure TriggerConnect; virtual;
    procedure TriggerDisconnecting; virtual;
    procedure TriggerDisconnect; virtual;

    procedure TriggerDataSent; virtual;
    procedure TriggerDataOut; virtual;
    procedure TriggerDataIn; virtual;
    procedure TriggerLastWrite; virtual;
    procedure TriggerDataReceived; virtual;
    procedure TriggerDataLost; virtual;
    procedure TriggerException(E:Exception); virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    { ********************************************* }

    function GetThread:TRtcThread; virtual; abstract;

    { Methods which have to be implemented by the Connection Provider }

    function ReleaseMe(FromInside:boolean):boolean; virtual;

    property Silent:boolean read GetSilent write SetSilent;

    // Called when user calls Disconnect (wanted disconnect)
    procedure Disconnect; virtual; abstract;
    // Called when timeout happens or connection gets lost (unwanted disconnect)
    procedure InternalDisconnect; virtual; abstract;

    procedure Check; virtual; abstract;

    function GetParent:TRtcConnectionProvider; virtual; abstract;
    function GetState:TRtcConnectionState; virtual; abstract;

    { Support for multithreaded processing -> }
    function PostJob(var myJob; HighPriority:boolean=False; ForceThread:boolean=False):boolean; virtual; abstract;

    function inThread:boolean; virtual; abstract;

    { <- multithreading }

    procedure WriteEx(const s:RtcByteArray; SendNow:boolean=True); virtual; abstract;
    function ReadEx:RtcByteArray; virtual; abstract;

    function PeekEx:RtcByteArray; virtual; abstract;
    procedure PokeEx(const s:RtcByteArray); virtual; abstract;

    procedure Write(const s:RtcString; SendNow:boolean=True); virtual; abstract;
    function Read:RtcString; virtual; abstract;

    function GetPeerAddr:RtcString; virtual; abstract;
    function GetPeerPort:RtcString; virtual; abstract;
    function GetLocalAddr:RtcString; virtual; abstract;
    function GetLocalPort:RtcString; virtual; abstract;

    { ********************************************* }

    { Methods to be used by the Connection Provider ...
      Component which uses this connection provider has to use Set_ methods (below)
      before calling Connect or Listen, to set this properties. }

    function GetAddr:RtcString;
    function GetPort:RtcString;
    function GetIPV:RtcIPV;

    function GetMultiThreaded:boolean;

    { Methods used by the Connection component,
      to set connection properties,
      before calling Connect or Listen. }

    procedure SetAddr(const val:RtcString);
    procedure SetPort(const val:RtcString);
    procedure SetIPV(const val:RtcIPV);
    procedure SetMultiThreaded(val:boolean);

    { Methods that set Triggers before using the connection provider.
      Those methods are used by the Conncetion component. }

    procedure SetError(Event:TRtcErrEvent);

    procedure SetTriggerReadyToRelease(Event:TRtcBasicEvent);

    procedure SetTriggerBeforeCreate(Event:TRtcBasicEvent);
    procedure SetTriggerAfterDestroy(Event:TRtcBasicEvent);

    procedure SetTriggerConnecting(Event:TRtcBasicEvent);
    procedure SetTriggerConnect(Event:TRtcBasicEvent);
    procedure SetTriggerDisconnect(Event:TRtcBasicEvent);
    procedure SetTriggerDisconnecting(Event:TRtcBasicEvent);

    procedure SetTriggerDataSent(Event:TRtcBasicEvent);
    procedure SetTriggerDataOut(Event:TRtcBasicEvent);
    procedure SetTriggerDataIn(Event:TRtcBasicEvent);
    procedure SetTriggerLastWrite(Event:TRtcBasicEvent);
    procedure SetTriggerDataReceived(Event:TRtcBasicEvent);
    procedure SetTriggerDataLost(Event:TRtcBasicEvent);
    procedure SetTriggerException(Event:TrtcExceptEvent);

    { This property is used to check the number of bytes just sent out.
      It is used by the TriggerDataOut method. }
    property DataOut:int64 read FDataOut;
    { This property is used to check the number of bytes just read in.
      It is used by the TriggerDataIn method. }
    property DataIn:int64 read FDataIn;

    property DataCrypt:boolean read FDataCrypt;
    property DataProtocol:TRtcCryptPluginProtocol read FDataProtocol;

  protected // WEB SOCKETS

    // Websocket connection?
    FIsWebSocketUpgraded:boolean;
    // WebSocket Manual-Read option?
    FWebSockManualRead:boolean;

  public // WEB SOCKETS

    property isWSManualRead:boolean read FWebSockManualRead write FWebSockManualRead;

    property IsWebSocket:boolean read FIsWebSocketUpgraded;
    end;

  { @name is a wrapper for the Server-side connection provider.
    It declares abstract methods which all server-side connection providers
    have to implement, so they can be used by the Server-side connecion components. }
  TRtcServerProvider = class(TRtcConnectionProvider)
  private
    // Used for counting connections in use
    FOnConnectionAccepted:TRtcBasicEvent; // we have acepted a new connection
    FOnConnectionAccepting:TRtcBasicEvent; // check if we can accept a new connection
    FOnConnectionLost:TRtcBasicEvent; // we have lost the connection

    // Used for triggering events (provider declares those as virtual abstract)
    FOnNewProvider:TRtcProviderEvent;

    FOnListenStart:TRtcBasicEvent;
    FOnListenStop:TRtcBasicEvent;

    FOnListenLost:TRtcBasicEvent;
    FOnListenError:TrtcExceptEvent;

  public

    function ClientCount:integer; virtual;

    { Triggers to be used by the ConncetionProvider.
      Connection component, which uses the provider,
      will set those triggers using SetTrigger_ methods (below). }

    procedure TriggerNewProvider(var Provider:TObject); virtual;

    procedure TriggerConnectionAccepting; virtual;
    procedure TriggerConnectionAccepted; virtual;
    procedure TriggerConnectionLost; virtual;

    procedure TriggerListenStart; virtual;
    procedure TriggerListenStop; virtual;

    procedure TriggerListenError(E:Exception); virtual;
    procedure TriggerListenLost; virtual;

  public
    { *** Methods which have to be implemented by the Connection Provider *** }

    procedure Listen(Restarting:boolean=False); virtual; abstract;

    { *** Methods used by the connection to set Triggers before using the connection provider. *** }

    procedure SetTriggerConnectionAccepting(Event:TRtcBasicEvent); // check if we can acept a new connection
    procedure SetTriggerConnectionAccepted(Event:TRtcBasicEvent); // we have acepted a new connection
    procedure SetTriggerConnectionLost(Event:TRtcBasicEvent); // we have lost the connection

    procedure SetTriggerNewProvider(Event:TrtcProviderEvent);

    procedure SetTriggerListenStart(Event:TRtcBasicEvent);
    procedure SetTriggerListenStop(Event:TRtcBasicEvent);

    procedure SetTriggerListenError(Event:TrtcExceptEvent);
    procedure SetTriggerListenLost(Event:TRtcBasicEvent);
    end;

  { @name is a wrapper for the Client-side connection provider.
    It declares abstract methods which all server-side connection providers
    have to implement, so they can be used by the Server-side connecion components. }
  TRtcClientProvider = class(TRtcConnectionProvider) // provides basic connection functionality
  private
    // Used for counting connections in use
    FOnConnectionOpening:TrtcBoolEvent; // we are opening a new connection
    FOnConnectionClosing:TRtcBasicEvent; // we are closing the connection

    // Used for triggering events (provider declares those as virtual abstract)
    FOnConnectFail:TRtcBasicEvent;
    FOnConnectLost:TRtcBasicEvent;
    FOnConnectError:TrtcExceptEvent;

  protected

    { Triggers to be used by the ConncetionProvider.
      Connection component, which uses the provider,
      will set those triggers using SetTrigger_ methods (below). }

    procedure TriggerConnectionOpening(Force:boolean); virtual;
    procedure TriggerConnectionClosing; virtual;

    procedure TriggerConnectError(E:Exception); virtual;
    procedure TriggerConnectFail; virtual;
    procedure TriggerConnectLost; virtual;

  public
    { *** Methods which have to be implemented by the Connection Provider *** }

    procedure Connect(Force:boolean=False;Reconnecting:boolean=False); virtual; abstract;

    { *** Methods used by the Connection to set Triggers before using the connection provider. *** }

    procedure SetTriggerConnectionOpening(Event:TrtcBoolEvent); // we are opening a new connection
    procedure SetTriggerConnectionClosing(Event:TRtcBasicEvent); // we are closing the connection

    procedure SetTriggerConnectFail(Event:TRtcBasicEvent);
    procedure SetTriggerConnectLost(Event:TRtcBasicEvent);
    procedure SetTriggerConnectError(Event:TrtcExceptEvent);
    end;

  TRtcBasicClientProvider = class(TRtcClientProvider)
  protected
    FPeerAddr,
    FPeerPort,
    FLocalAddr,
    FLocalPort:RtcString;

    procedure SetLocalAddr(const Value: RtcString);
    procedure SetLocalPort(const Value: RtcString);
    procedure SetPeerAddr(const Value: RtcString);
    procedure SetPeerPort(const Value: RtcString);
    procedure SetState(value:TRtcConnectionState);

    property State:TRtcConnectionState read GetState write SetState;
    property PeerAddr:RtcString read GetPeerAddr write SetPeerAddr;
    property PeerPort:RtcString read GetPeerPort write SetPeerPort;
    property LocalAddr:RtcString read GetLocalAddr write SetLocalAddr;
    property LocalPort:RtcString read GetLocalPort write SetLocalPort;

  public
    constructor Create; override;

    function GetParent:TRtcConnectionProvider; override;
    function GetState:TRtcConnectionState; override;

    function GetPeerAddr:RtcString; override;
    function GetPeerPort:RtcString; override;
    function GetLocalAddr:RtcString; override;
    function GetLocalPort:RtcString; override;

    procedure Check; override;
    end;

  TRtcBasicServerProvider = class(TRtcServerProvider)
  protected
    FPeerAddr,
    FPeerPort,
    FLocalAddr,
    FLocalPort:RtcString;

    procedure SetLocalAddr(const Value: RtcString);
    procedure SetLocalPort(const Value: RtcString);
    procedure SetPeerAddr(const Value: RtcString);
    procedure SetPeerPort(const Value: RtcString);
    procedure SetState(value:TRtcConnectionState);

    property State:TRtcConnectionState read GetState write SetState;
    property PeerAddr:RtcString read GetPeerAddr write SetPeerAddr;
    property PeerPort:RtcString read GetPeerPort write SetPeerPort;
    property LocalAddr:RtcString read GetLocalAddr write SetLocalAddr;
    property LocalPort:RtcString read GetLocalPort write SetLocalPort;

  public
    constructor Create; override;

    function GetState:TRtcConnectionState; override;

    function GetPeerAddr:RtcString; override;
    function GetPeerPort:RtcString; override;
    function GetLocalAddr:RtcString; override;
    function GetLocalPort:RtcString; override;

    procedure Check; override;
    end;

type
  TRtcThrClientProvider = class(TRtcBasicClientProvider)
  protected
    function GetClientThread:TRtcThread; virtual; abstract;

  public

    function GetThread:TRtcThread; override;

    function inThread:boolean; override;

    function PostJob(var _Job; HighPriority:boolean; ForceThread:boolean=False):boolean; override;

  (*** Methods that have to be implemented by the connection provider: *** ->

  protected
    function GetClientThread:TRtcThread; override;

  public
    procedure Connect(Force:boolean=False); override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    procedure WriteEx(const s:RtcByteArray); override;
    function ReadEx:RtcByteArray; override;

    procedure Write(const s:RtcString); override;
    function Read:RtcString; override;

    <- *** end ***)
    end;

  TRtcNoThrClientProvider = class(TRtcBasicClientProvider)
  public
    function inThread:boolean; override;

    function GetThread:TRtcThread; override;

    function PostJob(var _Job; HighPriority:boolean; ForceThread:boolean=False):boolean; override;

  (*** Methods that have to be implemented by the connection provider: *** ->

  public
    procedure Connect(Force:boolean=False); override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    procedure WriteEx(const s:RtcByteArray); override;
    function ReadEx:RtcByteArray; override;

    procedure Write(const s:RtcString); override;
    function Read:RtcString; override;

    <- *** end ***)
    end;

  TRtcThrServerProvider = class(TRtcBasicServerProvider)
  protected
    function GetServerThread:TRtcThread; virtual; abstract;
    function GetClientThread:TRtcThread; virtual; abstract;

  public
    function inThread:boolean; override;

    function GetThread:TRtcThread; override;

    function PostJob(var _Job; HighPriority:boolean; ForceThread:boolean=False):boolean; override;

  (*** Methods that have to be implemented by the connection provider: ***

  protected
    procedure CopyFrom(Dup:TRtcConnectionProvider);

    function GetClientThread:TRtcThread; override;
    function GetServerThread:TRtcThread; override;

  public
    procedure Listen; override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    function GetParent:TRtcConnectionProvider; override;

    procedure WriteEx(const s:RtcByteArray); override;
    function ReadEx:RtcByteArray; override;

    procedure Write(const s:RtcString); override;
    function Read:RtcString; override;

  *** end ***)
    end;

  TRtcNoThrServerProvider = class(TRtcBasicServerProvider)
  public
    function inThread:boolean; override;

    function GetThread:TRtcThread; override;

    function PostJob(var _Job; HighPriority:boolean; ForceThread:boolean=False):boolean; override;

  (*** Methods that have to be implemented by the connection provider: ***

  protected
    procedure CopyFrom(Dup:TRtcConnectionProvider);

  public
    procedure Listen; override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    function GetParent:TRtcConnectionProvider; override;

    procedure WriteEx(const s:RtcByteArray); override;
    function ReadEx:RtcByteArray; override;

    procedure Write(const s:RtcString); override;
    function Read:RtcString; override;

  *** end ***)
    end;

implementation

{ TRtcConnectionProvider }

procedure TRtcConnectionProvider.CleanUp;
  begin
  FState:=conInactive;
  FDataCrypt:=False;
  FDataProtocol:=cppTcp;
  end;

constructor TRtcConnectionProvider.Create;
  begin
  inherited;
  FCS:=TRtcCritSec.Create;
  FState:=conPrepared;
  FDataCrypt:=False;
  FDataProtocol:=cppTcp;

  FInSync:=False;
  FClosing:=False;
  FSilent:=False;
  FLost:=True; // if connection is closing and we didnt call disconnect, we lost it.

  FDataOut:=0;
  FDataIn:=0;

  TriggerBeforeCreate;
  end;

destructor TRtcConnectionProvider.Destroy;
  begin
  try
    CleanUp;
    TriggerAfterDestroy;
    RtcFreeAndNil(FCS);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcConnectionProvider.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcConnectionProvider.ReleaseMe(FromInside:boolean):boolean;
  begin
  Result:=True;
  end;

function TRtcConnectionProvider.GetClosing: boolean;
  begin
  Result:=FClosing;
  end;

function TRtcConnectionProvider.GetLost: boolean;
  begin
  Result:=FLost;
  end;

function TRtcConnectionProvider.GetSilent: boolean;
  begin
  Result:=FSilent;
  end;

procedure TRtcConnectionProvider.SetClosing(a: boolean);
  begin
  FClosing:=a;
  end;

procedure TRtcConnectionProvider.SetLost(a: boolean);
  begin
  FLost:=a;
  end;

procedure TRtcConnectionProvider.SetSilent(a: boolean);
  begin
  FSilent:=a;
  end;

procedure TRtcConnectionProvider.Error(const text: String);
  begin
  if assigned(FError) then
    FError(text);
  end;

function TRtcConnectionProvider.GetAddr: RtcString;
  begin
  Result:=FAddr;
  end;

function TRtcConnectionProvider.GetPort: RtcString;
  begin
  Result:=FPort;
  end;

function TRtcConnectionProvider.GetIPV: RtcIPV;
  begin
  Result:=FIPV;
  end;

procedure TRtcConnectionProvider.SetAddr(const val: RtcString);
  begin
  FAddr:=val;
  end;

procedure TRtcConnectionProvider.SetError(Event: TRtcErrEvent);
  begin
  FError:=Event;
  end;

procedure TRtcConnectionProvider.SetPort(const val: RtcString);
  begin
  FPort:=val;
  end;

procedure TRtcConnectionProvider.SetIPV(const val: RtcIPV);
  begin
  FIPV:=val;
  end;

procedure TRtcConnectionProvider.SetTriggerAfterDestroy(Event: TRtcBasicEvent);
  begin
  FOnAfterDestroy:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerBeforeCreate(Event: TRtcBasicEvent);
  begin
  FOnBeforeCreate:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerConnect(Event: TRtcBasicEvent);
  begin
  FOnConnect:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerConnecting(Event: TRtcBasicEvent);
  begin
  FOnConnecting:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDisconnecting(Event: TRtcBasicEvent);
  begin
  FOnDisconnecting:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDataReceived(Event: TRtcBasicEvent);
  begin
  FOnDataReceived:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDataLost(Event: TRtcBasicEvent);
  begin
  FOnDataLost:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDataSent(Event: TRtcBasicEvent);
  begin
  FOnDataSent:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDataOut(Event: TRtcBasicEvent);
  begin
  FOnDataOut:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDataIn(Event: TRtcBasicEvent);
  begin
  FOnDataIn:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerLastWrite(Event: TRtcBasicEvent);
  begin
  FOnLastWrite:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDisconnect(Event: TRtcBasicEvent);
  begin
  FOnDisconnect:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerException(Event: TrtcExceptEvent);
  begin
  FOnException:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerReadyToRelease(Event: TRtcBasicEvent);
  begin
  FOnReadyToRelease:=Event;
  end;

procedure TRtcConnectionProvider.TriggerAfterDestroy;
  begin
  if assigned(FOnAfterDestroy) then
    FOnAfterDestroy;
  end;

procedure TRtcConnectionProvider.TriggerBeforeCreate;
  begin
  if assigned(FOnBeforeCreate) then
    FOnBeforeCreate;
  end;

procedure TRtcConnectionProvider.TriggerConnect;
  begin
  if assigned(FOnConnect) then
    FOnConnect;
  end;

procedure TRtcConnectionProvider.TriggerConnecting;
  begin
  if assigned(FOnConnecting) then
    FOnConnecting;
  end;

procedure TRtcConnectionProvider.TriggerDisconnecting;
  begin
  if assigned(FOnDisconnecting) then
    FOnDisconnecting;
  end;

procedure TRtcConnectionProvider.TriggerDataReceived;
  begin
  if assigned(FOnDataReceived) then
    FOnDataReceived;
  end;

procedure TRtcConnectionProvider.TriggerDataLost;
  begin
  if assigned(FOnDataLost) then
    FOnDataLost;
  end;

procedure TRtcConnectionProvider.TriggerDataSent;
  begin
  if assigned(FOnDataSent) then
    FOnDataSent;
  end;

procedure TRtcConnectionProvider.TriggerDataOut;
  begin
  if assigned(FOnDataOut) then
    FOnDataOut;
  end;

procedure TRtcConnectionProvider.TriggerDataIn;
  begin
  if assigned(FOnDataIn) then
    FOnDataIn;
  end;

procedure TRtcConnectionProvider.TriggerLastWrite;
  begin
  if assigned(FOnLastWrite) then
    FOnLastWrite;
  end;

procedure TRtcConnectionProvider.TriggerDisconnect;
  begin
  if assigned(FOnDisconnect) then
    FOnDisconnect;
  end;

procedure TRtcConnectionProvider.TriggerException(E: Exception);
  begin
  if assigned(FOnException) then
    FOnException(E);
  end;

procedure TRtcConnectionProvider.TriggerReadyToRelease;
  begin
  if FState<=conClosing then FState:=conPrepared;
  if assigned(FOnReadyToRelease) then
    FOnReadyToRelease;
  end;

function TRtcConnectionProvider.GetMultiThreaded: boolean;
  begin
  Result:=FMultiThreaded;
  end;

procedure TRtcConnectionProvider.SetMultiThreaded(val: boolean);
  begin
  FMultiThreaded:=val;
  end;

procedure TRtcConnectionProvider.Enter;
  begin
  FCS.Acquire;
  end;

procedure TRtcConnectionProvider.Leave;
  begin
  FCS.Release;
  end;

{ TRtcServerProvider }

procedure TRtcServerProvider.SetTriggerConnectionAccepting(Event: TRtcBasicEvent);
  begin
  FOnConnectionAccepting:=Event;
  end;

procedure TRtcServerProvider.SetTriggerConnectionAccepted(Event: TRtcBasicEvent);
  begin
  FOnConnectionAccepted:=Event;
  end;

procedure TRtcServerProvider.SetTriggerListenStart(Event: TRtcBasicEvent);
  begin
  FOnListenStart:=Event;
  end;

procedure TRtcServerProvider.SetTriggerListenStop(Event: TRtcBasicEvent);
  begin
  FOnListenStop:=Event;
  end;

procedure TRtcServerProvider.SetTriggerListenLost(Event: TRtcBasicEvent);
  begin
  FOnListenLost:=Event;
  end;

procedure TRtcServerProvider.SetTriggerNewProvider(Event: TrtcProviderEvent);
  begin
  FOnNewProvider:=Event;
  end;

procedure TRtcServerProvider.TriggerConnectionAccepting;
  begin
  if assigned(FOnConnectionAccepting) then
    FOnConnectionAccepting;
  end;

procedure TRtcServerProvider.TriggerConnectionAccepted;
  begin
  if assigned(FOnConnectionAccepted) then
    FOnConnectionAccepted;
  end;

procedure TRtcServerProvider.TriggerListenStart;
  begin
  if assigned(FOnListenStart) then
    FOnListenStart;
  end;

procedure TRtcServerProvider.TriggerListenStop;
  begin
  if assigned(FOnListenStop) then
    FOnListenStop;
  end;

procedure TRtcServerProvider.TriggerListenLost;
  begin
  if assigned(FOnListenLost) then
    FOnListenLost;
  end;

procedure TRtcServerProvider.SetTriggerConnectionLost(Event: TRtcBasicEvent);
  begin
  FOnConnectionLost:=Event;
  end;

procedure TRtcServerProvider.TriggerConnectionLost;
  begin
  if assigned(FOnConnectionLost) then
    FOnConnectionLost;
  end;

procedure TRtcServerProvider.TriggerListenError(E: Exception);
  begin
  if assigned(FOnListenError) then
    FOnListenError(E);
  end;

procedure TRtcServerProvider.SetTriggerListenError(Event: TrtcExceptEvent);
  begin
  FOnListenError:=Event;
  end;

procedure TRtcServerProvider.TriggerNewProvider(var Provider: TObject);
  begin
  if assigned(FOnNewProvider) then
    FOnNewProvider(Provider);
  end;

function TRtcServerProvider.ClientCount: integer;
  begin
  Result:=0;
  end;

{ TRtcClientProvider }

procedure TRtcClientProvider.SetTriggerConnectError(Event: TrtcExceptEvent);
  begin
  FOnConnectError:=Event;
  end;

procedure TRtcClientProvider.SetTriggerConnectFail(Event: TRtcBasicEvent);
  begin
  FOnConnectFail:=Event;
  end;

procedure TRtcClientProvider.SetTriggerConnectLost(Event: TRtcBasicEvent);
  begin
  FOnConnectLost:=Event;
  end;

procedure TRtcClientProvider.SetTriggerConnectionOpening(Event: TrtcBoolEvent);
  begin
  FOnConnectionOpening:=Event;
  end;

procedure TRtcClientProvider.SetTriggerConnectionClosing(Event: TRtcBasicEvent);
  begin
  FOnConnectionClosing:=Event;
  end;

procedure TRtcClientProvider.TriggerConnectionClosing;
  begin
  if assigned(FOnConnectionClosing) then
    FOnConnectionClosing;
  end;

procedure TRtcClientProvider.TriggerConnectError(E: Exception);
  begin
  if assigned(FOnConnectError) then
    FOnConnectError(E);
  end;

procedure TRtcClientProvider.TriggerConnectFail;
  begin
  if assigned(FOnConnectFail) then
    FOnConnectFail;
  end;

procedure TRtcClientProvider.TriggerConnectLost;
  begin
  if assigned(FOnConnectLost) then
    FOnConnectLost;
  end;

procedure TRtcClientProvider.TriggerConnectionOpening(Force: boolean);
  begin
  if assigned(FOnConnectionOpening) then
    FOnConnectionOpening(Force);
  end;

{ TRtcBasicClientProvider }

constructor TRtcBasicClientProvider.Create;
  begin
  inherited;
  FLocalAddr:='';
  FLocalPort:='';
  FPeerAddr:='';
  FPeerPort:='';
  end;

procedure TRtcBasicClientProvider.Check;
  begin
  // do nothing
  end;

function TRtcBasicClientProvider.GetParent: TRtcConnectionProvider;
  begin
  Result:=nil;
  end;

function TRtcBasicClientProvider.GetLocalAddr: RtcString;
  begin
  Result:=FLocalAddr;
  end;

function TRtcBasicClientProvider.GetLocalPort: RtcString;
  begin
  Result:=FLocalPort;
  end;

function TRtcBasicClientProvider.GetPeerAddr: RtcString;
  begin
  Result:=FPeerAddr;
  end;

function TRtcBasicClientProvider.GetPeerPort: RtcString;
  begin
  Result:=FPeerPort;
  end;

function TRtcBasicClientProvider.GetState: TRtcConnectionState;
  begin
  Result:=FState;
  end;

procedure TRtcBasicClientProvider.SetLocalAddr(const Value: RtcString);
  begin
  FLocalAddr:=Value;
  end;

procedure TRtcBasicClientProvider.SetLocalPort(const Value: RtcString);
  begin
  FLocalPort:=Value;
  end;

procedure TRtcBasicClientProvider.SetPeerAddr(const Value: RtcString);
  begin
  FPeerAddr:=Value;
  end;

procedure TRtcBasicClientProvider.SetPeerPort(const Value: RtcString);
  begin
  FPeerPort:=Value;
  end;

procedure TRtcBasicClientProvider.SetState(value: TRtcConnectionState);
  begin
  FState:=Value;
  end;

{ TRtcBasicServerProvider }

constructor TRtcBasicServerProvider.Create;
  begin
  inherited;
  FLocalAddr:='';
  FLocalPort:='';
  FPeerAddr:='';
  FPeerPort:='';
  end;

procedure TRtcBasicServerProvider.Check;
  begin
  // do nothing
  end;

function TRtcBasicServerProvider.GetLocalAddr: RtcString;
  begin
  Result:=FLocalAddr;
  end;

function TRtcBasicServerProvider.GetLocalPort: RtcString;
  begin
  Result:=FLocalPort;
  end;

function TRtcBasicServerProvider.GetPeerAddr: RtcString;
  begin
  Result:=FPeerAddr;
  end;

function TRtcBasicServerProvider.GetPeerPort: RtcString;
  begin
  Result:=FPeerPort;
  end;

function TRtcBasicServerProvider.GetState: TRtcConnectionState;
  begin
  Result:=FState;
  end;

procedure TRtcBasicServerProvider.SetLocalAddr(const Value: RtcString);
  begin
  FLocalAddr:=Value;
  end;

procedure TRtcBasicServerProvider.SetLocalPort(const Value: RtcString);
  begin
  FLocalPort:=Value;
  end;

procedure TRtcBasicServerProvider.SetPeerAddr(const Value: RtcString);
  begin
  FPeerAddr:=Value;
  end;

procedure TRtcBasicServerProvider.SetPeerPort(const Value: RtcString);
  begin
  FPeerPort:=Value;
  end;

procedure TRtcBasicServerProvider.SetState(value: TRtcConnectionState);
  begin
  FState:=Value;
  end;

{ TRtcThrClientProvider }

function TRtcThrClientProvider.inThread: boolean;
  begin
  if GetClientThread<>nil then
    Result:=GetClientThread.InsideThread
  else if GetMultiThreaded then
    Result:=InsideMainThread
  else
    Result:=True;
  end;

function TRtcThrClientProvider.PostJob(var _Job; HighPriority: boolean; ForceThread:boolean=False): boolean;
  var
    Job:TObject absolute _Job;
    xJob:TRtcJob absolute _Job;
  begin
  if Job=nil then
    Result:=True
  else if GetClientThread<>nil then
    begin
    Result:=TRtcThread.PostJob(GetClientThread,Job,HighPriority,ForceThread);
    end
  else if (Job is TRtcJob) and not GetMultiThreaded then
    begin
    if xJob.Run(nil) then
      if xJob.SingleUse then
        RtcFreeAndNil(Job);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcThrClientProvider.GetThread: TRtcThread;
  begin
  Result:=GetClientThread;
  end;

{ TRtcNoThrClientProvider }

function TRtcNoThrClientProvider.inThread: boolean;
  begin
  Result:=True; // inMainThread;
  end;

function TRtcNoThrClientProvider.PostJob(var _Job; HighPriority: boolean; ForceThread:boolean=False): boolean;
  var
    Job:TObject absolute _Job;
    xJob:TRtcJob absolute _Job;
  begin
  if Job=nil then
    Result:=True
  else if Job is TRtcJob then
    begin
    if xJob.Run(nil) then
      if xJob.SingleUse then
        RtcFreeAndNil(Job);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcNoThrClientProvider.GetThread: TRtcThread;
  begin
  Result:=nil;
  end;

{ TRtcThrServerProvider }

function TRtcThrServerProvider.inThread: boolean;
  begin
  if GetClientThread<>nil then
    Result:=GetClientThread.InsideThread
  else if GetServerThread<>nil then
    Result:=GetServerThread.InsideThread
  else if GetMultiThreaded then
    Result:=InsideMainThread
  else
    Result:=True;
  end;

function TRtcThrServerProvider.PostJob(var _Job; HighPriority: boolean; ForceThread:boolean=False): boolean;
  var
    Job:TObject absolute _Job;
    xJob:TRtcJob absolute _Job;
  begin
  if Job=nil then
    Result:=True
  else if GetClientThread<>nil then
    begin
    Result:=TRtcThread.PostJob(GetClientThread,Job,HighPriority,ForceThread);
    end
  else if GetServerThread<>nil then
    begin
    Result:=TRtcThread.PostJob(GetServerThread,Job,HighPriority,ForceThread);
    end
  else if (Job is TRtcJob) and not GetMultiThreaded then
    begin
    if xJob.Run(nil) then
      if xJob.SingleUse then
        RtcFreeAndNil(xJob);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcThrServerProvider.GetThread: TRtcThread;
  begin
  Result:=GetClientThread;
  if not assigned(Result) then
    Result:=GetServerThread;
  end;

{ TRtcNoThrServerProvider }

function TRtcNoThrServerProvider.inThread: boolean;
  begin
  Result:=True;
  end;

function TRtcNoThrServerProvider.PostJob(var _Job; HighPriority: boolean; ForceThread:boolean=False): boolean;
  var
    Job:TObject absolute _Job;
    xJob:TRtcJob absolute _Job;
  begin
  if Job=nil then
    Result:=True
  else if Job is TRtcJob then
    begin
    if xJob.Run(nil) then
      if xJob.SingleUse then
        RtcFreeAndNil(Job);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcNoThrServerProvider.GetThread: TRtcThread;
  begin
  Result:=nil;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; {$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcConnProv Finalizing ...','DEBUG');{$ENDIF}
CloseThreadPool;
{$IFDEF RTC_DEBUG} Log('rtcConnProv Finalized.','DEBUG');{$ENDIF}
end.
