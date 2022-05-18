{
  @html(<b>)
  Data Server Components
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit implements a set of Server-side Data components. @Link(TRtcDataServer)
  implements a wrapper for all Request/Response based server-side connection components.
  By linking one or more @Link(TRtcDataServerLink) components, which are linked to one or
  more @Link(TRtcDataProvider) components, you add functionality to your server for
  specific Request handlers. @html(<br>)
  @Link(TRtcHttpServer) implements a HTTP-based TCP/IP server connection with uses a
  HTTP connection provider, so you can compile your DataProvider/ServerModule/Function
  components into a stand-alone HTTP server executable.
}
unit rtcDataSrv;

{$INCLUDE rtcDefs.inc}

interface

// When "UseGUIDs" is defined, CoCreateGUID() API function is used to create Session IDs.
{.$DEFINE UseGUIDs}

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSrcList,
  rtcSystem,
  rtcThrPool,
  rtcLog,

  rtcInfo,
  rtcLink,
  rtcConnProv,
  rtcConn;

var
  { Default Session Live Time (in seconds).
    @html(<br><br>)

    Before a session defines its KeepAlive time,
    the session will live for RTC_SESSION_TIMEOUT seconds after each call. }
  RTC_SESSION_TIMEOUT:integer=60;

const
{$IFNDEF UseGUIDs}
  // @exclude
  RTC_SESSIONID_LENGTH=28;
{$ELSE}
  // @exclude
  RTC_SESSIONID_LENGTH=32;
{$ENDIF}

type
  TRtcWaitForStopListenResult=TRtcWaitForConditionResult;

  { @abstract(Components used in DataServer to implement server's functionality) }
  TRtcServerComponent=class(TRtcComponent)
  protected
    // @exclude
    function GetOrder: integer; virtual;
    end;

  // @exclude
  TRtcServerComponentList = class(TObject)
  private
    FList:TRtcObjectList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Value:TRtcServerComponent);
    procedure Remove(Value:TRtcServerComponent);

    procedure RemoveAll;

    function Count:integer;
    function Get(index:integer):TRtcServerComponent;
    end;

  TRtcAbsDataServerLink = class;

  // @exclude
  TRtcDataServerLinkList = class(TRtcServerComponentList)
  public
    function GetLink(index:integer):TRtcAbsDataServerLink;
    end;

  { @abstract(Server Session information) }
  TRtcServerSession=class(TRtcSession)
  protected
    // @exclude
    class function Find(const _ID:RtcString; const _PeerAddr,_ForwardedFor:RtcString):TRtcServerSession;
    // @exclude
    class function Have(const _ID:RtcString; const _PeerAddr,_ForwardedFor:RtcString):boolean;
    // @exclude
    class function Open(_LockType:TRtcSessionLockType; const _PeerAddr,_ForwardedFor:RtcString):TRtcServerSession; overload;
    // @exclude
    class function CloseID(const _ID:RtcString; const _PeerAddr,_ForwardedFor:RtcString; _Event:TRtcSimpleEvent):boolean;

    // @exclude
    procedure LockSessionID;
    // @exclude
    procedure UnlockSession(_Event:TRtcSimpleEvent);

    // @exclude
    procedure UnLock(_Event:TRtcSimpleEvent);

    // @exclude
    function DenyAccess(const _PeerAddr,_ForwardedFor:RtcString):boolean;

  public
    // Close this session: This is same as setting "FinalExpire" to "Now".
    procedure Close; override;

    // Returns TRUE if this Session is Closing
    function isClosing:boolean; override;
    end;

  { @Abstract(Universal Data Server Connection component)

    By using methods provided by this DataProvider component, you ensure that
    your code will be compatible with different connection providers,
    which makes it possible to write your code once and use it to compile
    it for different servers. @html(<br>)

    By using methods, events and properties available from TRtcDataServer,
    you can easily respond to requests by sending an appropriate result
    which will be readable by the connected client, be it a standard Web
    Browser or any application written by using the @Link(TRtcDataClient)
    connection component.

    @Link(TRtcDataServer) also makes sure that you receive requests one by one
    and get the chance to answer them one-by-one, even if the client side
    sends all the requests at once (as one big request list), so
    you can relax and process all incomming requests, without worrying
    about overlapping your responses for different requests.
    @html(<br><br>)

    Component which calls 'Accept' will gain complete control
    over the connection, until a complete response is sent out
    (or connection is closed).

    After the response is done, DataServer again takes over and waits for the
    next request, then the process repeats itself, until all requests have been
    processed or the connection has been closed.

    Properties to check first:
    @html(<br>)
    @Link(TRtcConnection.ServerAddr) - Local Address to bind the server to (leave empty for ALL)
    @html(<br>)
    @Link(TRtcConnection.ServerPort) - Port to listen on and wait for connections
    @html(<br><br>)

    Methods to check first:
    @html(<br>)
    @Link(TRtcServer.Listen) - Start server
    @html(<br>)
    @Link(TRtcDataServer.Accept), @Link(TRtcDataServer.Request), @Link(TRtcConnection.Read) - Read and Accept client Request
    @html(<br>)
    @Link(TRtcDataServer.WriteHeader), @Link(TRtcConnection.Write) - Write result to client
    @html(<br>)
    @Link(TRtcConnection.Disconnect) - Disconnect client
    @html(<br>)
    @Link(TRtcServer.StopListen) - Stop server
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcServer.OnListenStart) - Server started
    @html(<br>)
    @Link(TRtcConnection.OnConnecting) - new Client connecting
    @html(<br>)
    @Link(TRtcDataServer.OnRequestNotAccepted) - Request has been received but not accepted by any DataProvider component.
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent to client (buffer now empty)
    @html(<br>)
    @Link(TRtcConnection.OnDisconnecting) - one Client disconnecting
    @html(<br>)
    @Link(TRtcServer.OnListenStop) - Server stopped
    @html(<br><br>)

    Check @Link(TRtcServer) and @Link(TRtcConnection) for more info.
    }
  TRtcDataServer = class(TRtcServer)
  private
    FOnPeekRequest:TRtcNotifyEvent;

    FOnRequestAccepted:TRtcNotifyEvent;
    FOnRequestNotAccepted:TRtcNotifyEvent;
    FOnResponseDone:TRtcNotifyEvent;
    FOnSessionOpen:TRtcNotifyEvent;
    FOnSessionClose:TRtcNotifyEvent;

    FSession:TRtcServerSession;

    FActiveLink:TRtcAbsDataServerLink;

    FDataServerLinks:TRtcDataServerLinkList;
    FDataServerLinks_Owner:boolean;

    FMyRequest, FRequest:TRtcServerRequest;
    FMyResponse, FResponse:TRtcServerResponse;

    FRequestFixup: TRtcServerRequestFixup;

  protected

    // @exclude
    procedure CopyFrom(Dup: TRtcServer); override;

    // @exclude
    procedure AddDataServerLink(Value:TRtcAbsDataServerLink);
    // @exclude
    procedure RemoveDataServerLink(Value:TRtcAbsDataServerLink);
    // @exclude
    procedure RemoveAllDataServerLinks;

    // @exclude
    procedure CallSessionOpen;
    // @exclude
    procedure CallSessionClose(Sender:TObject);

    // @exclude
    procedure CallListenStart; override;
    // @exclude
    procedure CallListenStop; override;

    // @exclude
    procedure CallDataReceived; override;
    // @exclude
    procedure CallDataOut; override;
    // @exclude
    procedure CallDataIn; override;
    // @exclude
    procedure CallDataSent; override;
    // @exclude
    procedure CallReadyToSend; override;
    // @exclude
    procedure CallDisconnect; override;
    // @exclude
    procedure CallLastWrite; override;

    // @exclude
    function GetRequest:TRtcRequest; override;
    // @exclude
    function GetResponse:TRtcResponse; override;

    // @exclude
    procedure SetRequest(const Value: TRtcRequest); override;
    // @exclude
    procedure SetResponse(const Value: TRtcResponse); override;

    // @exclude
    procedure SetSrvRequest(const Value: TRtcServerRequest); virtual;
    // @exclude
    procedure SetSrvResponse(const Value: TRtcServerResponse); virtual;

    // @exclude
    procedure SetRequestFixup(const Value: TRtcServerRequestFixup);

    { @exclude }
    procedure InitSession;

    { @exclude }
    function DoWaitForStopListen(UserInteractionAllowed: boolean=False; AllowMessageProcessing:boolean=True; _Timeout:cardinal=0): TRtcWaitForStopListenResult;

    { @exclude }
    function GetSession:TRtcSession; override;

    { **************************************************** }

    { NEW METHODS TO BE IMPLEMENTED BY THE CONNECTION COMPONENT }

  public

    { **************************************************** }

    // @exclude
    constructor Create(AOwner: TComponent); override;
    // @exclude
    destructor Destroy; override;

    // @exclude
    class function New:TRtcDataServer;

    // @exclude
    procedure CallRequestAccepted; virtual;
    // @exclude
    procedure CallRequestNotAccepted; virtual;
    // @exclude
    procedure CallResponseDone; virtual;

    { Used by DataServerLink components to tell DataServer that
      they are currently checking the request.
      Only needs to be called from OnCheckRequest,
      before the Request has been accepted.
      Once the request is accepted, ActiveDataServerLink will be
      used to process all future Data-related events, until
      the request has been processed or connection closed.
      @exclude }
    procedure SetActiveLink(Link:TRtcAbsDataServerLink);

    { Call "StopListen" and wait for the Listener to stop listening.
      This is the recommended way to close the Server listener before
      closing the Application or manually destroying the component.
      Use a "_Timeout" parameter (seconds, default = infinite) to specify 
      how long you are willing to wait for the listener to stop.
      Returns "wait_OK" in case of success (listener stopped). }
    function StopListenNow(_Timeout:cardinal=0; UserInteractionAllowed: boolean=False; AllowMessageProcessing:boolean=True): TRtcWaitForStopListenResult;

    { Request handler has to call Accept before it starts processing
      the request, so that all further events remain mapped to the
      active event handlers and don't switch before a reply has been sent. }
    procedure Accept; override;

    { Find an existing Session with this ID.
      If Session with this ID does not exist,
      or session has expired or session is currently locked,
      returns FALSE. Otherwise, prepares the Session variable
      for use and returns TRUE. }
    function FindSession(const ID:RtcString):boolean; override;

    { If there is a session with this ID, returns TRUE,
      even if that session is locked. }
    function HaveSession(const ID:RtcString):boolean; override;

    { If you do not need the Session anymore and do not want to keep the
      session locked until request completes, you can release the Session
      Lock by calling "UnLockSession". After this call, you will no longer
      have access to the Session object, until you lock it again using FindSession. }
    procedure UnLockSession; override;

    { Create a new Session, with a new and unique Session ID. }
    procedure OpenSession(LockType:TRtcSessionLockType=sesFwdLock); override;

    { If there is a session with this ID,
      returns TRUE and closes the session. }
    function CloseSession(const ID:RtcString):boolean; override;

    { Activate current Session's Object Manager for use in the current Thread.
      Call with "False" as parameter if the Object Manager should already exist,
      or with "True" if a new Object Manager should be created if it wasn't
      already and a new Session opened if no Session is currently active. }
    procedure ActivateObjectManager(xCreate:boolean=True); override;

    { Returns the "Linked Objects" Manager of the currently active Session.
      Returns NIL if the active Session does NOT have an "Object Manager"
      assigned or if no Session was activated for the current request. }
    function GetObjectManager:TRtcRemoteObjectManager; override;

    { Total number of open Sessions }
    function TotalSessionsCount:cardinal; override;

    { Total number of Sessions currently locked.
      A session is locked after a call to FindSession() and
      unlocked after the Event is done executing. }
    function TotalSessionsLocked:cardinal; override;

    { Total number of Sessions currently unlocked.
      A session is locked after a call to FindSession() and
      unlocked after the Event is done executing. }
    function TotalSessionsUnlocked:cardinal; override;

    { Current Request's Session info.
      @html(<br>)
      Before you can access the Session for the Request,
      you have to find the appropriate Session by using the
      FindSession function or create a new session by calling the
      OpenSession method. }
    property Session:TRtcServerSession read FSession;

    { Access to current request information.
      Use Request property to read the request information received.
      Here is all the info that was available in request header.
      To read request's body, use the Read function. }
    property Request:TRtcServerRequest read FRequest write SetSrvRequest;

    { Access to current response information.
      Use Response property to prepare the response header.
      Here you can set all header variables and parameters.
      If there is no content body to send out (only header), you will at
      least have to call 'WriteHeader', or 'Write' without parameters once. }
    property Response:TRtcServerResponse read FResponse write SetSrvResponse;

  published
    { If you are NOT using this component for writing a HTTP Proxy, in which case you
      would need the original and unmodified URI receveid from the Client, you can use
      the RequestFixup properties to specify which automatic operations should be done
      on the Request automatically by the component, so you do not have to do it manually. }
    property FixupRequest:TRtcServerRequestFixup read FRequestFixup write SetRequestFixup;

    { Called when content comes from a Client for a Request accepted through this component,
      BEFORE the "OnDataReceived" event on the component assigned to handle the Request.
      You can use this event for centralized logging and monitoring of all request content
      received through this component, but do NOT use Read or ReadEx methods here, beause
      they will clear receiving buffers, making it inaccessible by all other components!
      To check request content data in this event, use the "PeekEx" method instead. }
    property OnPeekRequest:TRtcNotifyEvent read FOnPeekRequest write FOnPeekRequest;

    { Called after a new request has been accepted.
      You can use this event handler to create a DataTunel and
      assign it to Tunel, in case the request has to be tunelled. }
    property OnRequestAccepted:TRtcNotifyEvent read FOnRequestAccepted write FOnRequestAccepted;
    { Called after a new request has been received, but NOT ACCEPTED.
      You can use this event handler to respond to requests with no DataProvider. }
    property OnRequestNotAccepted:TRtcNotifyEvent read FOnRequestNotAccepted write FOnRequestNotAccepted;
    { Called after a processed response was Done sending data out. }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { Called after a new session has been opened.
      You can use the Session property from this event to get session ID. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { Called before an old session has been closed.
      You can use the Session property from this event to get Session ID. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;

    { This event will be triggered every time a chunk of your data
      prepared for sending has just been sent out. To know
      exactly how much of it is on the way, use the @Link(TRtcConnection.DataOut) property.
      @html(<br><br>)

      NOTE: Even though data has been sent out, it doesn't mean that
      the other side already received it. It could also be that connection will
      break before this package reaches the other end. }
    property OnDataOut;
    { This event will be triggered every time a chunk of data
      has just come in (received). To know exactly how much of it
      has just arrived, use the @Link(TRtcConnection.DataIn) property. }
    property OnDataIn;
    end;

  TRtcDataServerLink=class;

  { @abstract(DataServer Link wrapper) }
  TRtcAbsDataServerLink=class(TRtcServerComponent)
  private
    FServer: TRtcDataServer;
    FLink: TRtcDataServerLink;
    FOrder: integer;

  protected // HTTP

    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    function CheckLink(Value:TRtcAbsDataServerLink):boolean; virtual;
    // @exclude
    procedure RemoveLink(Value:TRtcAbsDataServerLink); virtual;
    // @exclude
    procedure RemoveServer(Value:TRtcDataServer); virtual;

    // @exclude
    function GetServer: TRtcDataServer; virtual;
    // @exclude
    procedure SetServer(const Value: TRtcDataServer); virtual;

    // @exclude
    function GetLink: TRtcDataServerLink; virtual;
    // @exclude
    procedure SetLink(const Value: TRtcDataServerLink); virtual;

    // @exclude
    function GetOrder: integer; override;
    // @exclude
    procedure SetOrder(const Value: integer); virtual;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); virtual;

    // @exclude
    procedure Call_ListenStart(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_ListenStop(Sender:TRtcConnection); virtual;

    // @exclude
    procedure Call_CheckRequest(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_RequestAccepted(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); virtual;

    // @exclude
    procedure Call_DataReceived(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_DataOut(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_DataIn(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_DataSent(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_ReadyToSend(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_Disconnect(Sender:TRtcConnection); virtual;

    // @exclude
    function GetObjectManager(Sender:TRtcConnection):TRtcRemoteObjectManager; virtual;
    // @exclude
    procedure ActivateObjectManager(Sender:TRtcConnection; xCreate:boolean=True); virtual;

  public // HTTP

    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

  published // HTTP

    { You can link your components (one or more) to a DataServerLink component
      by assigning your @Link(TRtcDataServerLink) component to this Link property.
      Doing this, you only have to set the Server property for the master
      DataServerLink component and don't need to do it for every single
      DataSource component. }
    property Link:TRtcDataServerLink read GetLink write SetLink;
    { You can also link your components (one or more) directly to your
      DataServer connection component by assigning your
      @Link(TRtcDataServer) connection component to this Server property.
      This is useful if you have some complex functionality implemented
      in a single DataSource/DataServerLink component and don't need to drag
      the component through another DataServerLink component to get to
      your DataServer. }
    property Server:TRtcDataServer read GetServer write SetServer;
    { This is the Order in which the components will be asked to
      process a request. The smaller this Order number, the sooner
      a component will be asked to process a request, compared to
      other components connected to the DataServer at the same level.
      Since we could have more levels (more DataServerLink components
      connected to each other), Order only defines the priority
      at the same level.
      @html(<br><br>)

      For example, if DataSourceA has Order=50 and DataServerLinkB has Order=60,
      when both components are assigned to the same parent DataServerLink or directly
      to the DataServer, DataSourceA will receive requests for checking before any
      component assigned to DataServerLinkB, no matter which Order the components
      assigned to DataServerLinkB have. This is because the Order property only
      defines the order in the list of components assigned to the same parent
      component (DataServerLink or DataSource).
      @html(<br><br>)

      To make this simpler, just think of this Order as you would of TabOrder
      in a Form, where DataServer is the Form, DataServerLink are the Panels and
      DataSource are edit controls. TabOrder is defined for each control
      inside its parent (TEdit inside TPanel, child TPanel inside parent TPanel
      or master TPanel directly on the TForm).
      @html(<br><br>)

      Order is especially important for components which could handle
      same requests, like the PHP source handler and File source handler.
      If FileSource handler would have lower order number than the
      PHPSource handler, then the PHP soruce handler would never be executed,
      because all PHP files would be simply sent out in their source code form
      by the File Source handler. This is why the File Source handler has to have
      order number bigger that the PHPSource handler and (preferably) be
      connected to the same component (DataServerLink or DataServer). }
    property CheckOrder:integer read GetOrder write SetOrder default 0;

 protected // WEB SOCKETS

    // @exclude
    procedure Call_WSConnect(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_WSDataIn(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_WSDataOut(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_WSDataReceived(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_WSDataSent(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_WSDisconnect(Sender:TRtcConnection); virtual;

    end;

  { @abstract(DataServer Link, used to group Data Providers)

    You can use TRtcDataServerLink components to group several related
    @Link(TRtcDataProvider) components. Simply set this component as the
    Link property for all your RtcDataSource components, so that
    you don't have to set the Server property for every single
    TRtcDataProvider component separately. This is useful especially
    when the component is used in a datamodule or a form without
    dataserver and you need to link all the components to
    a DataServer which is on another datamodule or form.
    @html(<br><br>)

    Check @Link(TRtcAbsDataServerLink) for more info. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}

  TRtcDataServerLink=class(TRtcAbsDataServerLink)
  private // HTTP

    FOnPeekRequest:TRtcNotifyEvent;
    FOnFilterRequest:TRtcNotifyEvent;

    FOnListenStart:TRtcNotifyEvent;
    FOnListenStop:TRtcNotifyEvent;
    FOnRequestAccepted:TRtcNotifyEvent;
    FOnResponseDone:TRtcNotifyEvent;
    FOnDisconnect:TRtcNotifyEvent;
    FOnSessionOpen:TRtcNotifyEvent;
    FOnSessionClose:TRtcNotifyEvent;

  protected // HTTP

    // @exclude
    FDataServerLinks:TRtcDataServerLinkList;

    { Other Call_ methods are not implemented here,
      since only CheckRequest event will be called on the Link,
      while all the other events are being called directly on
      the AbsDataServerLink component which implements them
      (for example, TRtcDataProvider).
      @exclude }
    procedure Call_CheckRequest(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_DataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataOut(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataSent(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ReadyToSend(Sender:TRtcConnection); override;

    // @exclude
    procedure AddChildLink(Value:TRtcAbsDataServerLink);
    // @exclude
    procedure RemoveChildLink(Value:TRtcAbsDataServerLink);
    // @exclude
    procedure RemoveAllChildLinks;

  public // HTTP

    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    // @exclude
    procedure Call_PeekReceived(Sender:TRtcConnection); virtual;

    { ListenStart and ListenStop methods are used for
      component initialization on server start and
      deinitialization on server stop.
      @exclude }
    procedure Call_ListenStart(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ListenStop(Sender:TRtcConnection); override;
    { Accepted can be used to prepare the Request after it has been accepted.
      @exclude }
    procedure Call_RequestAccepted(Sender:TRtcConnection); override;
    { ResponseDone can be used to complete things prepared with RequestAccepted.
      @exclude }
    procedure Call_ResponseDone(Sender:TRtcConnection); override;
    { Disconnect can be used for request deinitialization.
      @exclude }
    procedure Call_Disconnect(Sender:TRtcConnection); override;
    { New Session Open.
      @exclude }
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    { Existing Session Closing.
      @exclude }
    procedure Call_SessionClose(Sender:TRtcConnection); override;

  published // HTTP

    { You can implement this event if you want to Filter requests for
      linked "child" components. When the event is assigned, you should
      call the "Sender.Accept" method inside that event if you want the
      request to be forwarded to linked "child" components. If you do 
      NOT want a request to be forwarded to child components (filter it out),
      you should NOT call the "Sender.Accept" method for that request. 
      NOTE: If this event is NOT assigned, ALL requests reaching this 
      component are forwarded to linked "child" components (no filtering). }
    property OnFilterRequest:TRtcNotifyEvent read FOnFilterRequest write FOnFilterRequest;

    { You can use this event to monitor request content received through this component,
      but do NOT use Read or ReadEx methods here, because they will clear receiving
      buffers. ONLY use the PeekEx method to access the content body from this event. }
    property OnPeekRequest:TRtcNotifyEvent read FOnPeekRequest write FOnPeekRequest;

    { Event to be triggered when the Server starts listening on the connection.
      You can use this event for component initialization. }
    property OnListenStart:TRtcNotifyEvent read FOnListenStart write FOnListenStart;
    { Event to be triggered when the Server stops listening on the connection.
      You can use this event for component deinitialization. }
    property OnListenStop:TRtcNotifyEvent read FOnListenStop write FOnListenStop;
    { Event to be triggered when a child DataProvider component accepts the Request.
      You can use this event for request initialization. For example,
      you could create a DataTunel and assign it to Tunel, to have reuqest data tunneled. }
    property OnRequestAccepted:TRtcNotifyEvent read FOnRequestAccepted write FOnRequestAccepted;
    { Event to be triggered when a response to server has been sent (Response.Done) }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { Event to be triggered when connection gets lost after a request was accepted.
      You can use this event for component deinitialization. }
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
    { Event to be triggered after new Session was opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { Event to be triggered before existing Session closes. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;

  private // WEB SOCKETS

    FOnWSConnect:TRtcNotifyEvent;
    FOnWSDisconnect:TRtcNotifyEvent;

  protected // WEB SOCKETS

    // @exclude
    procedure Call_WSDataIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDataOut(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDataSent(Sender:TRtcConnection); override;

  public // WEB SOCKETS

    // @exclude
    procedure Call_WSConnect(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDisconnect(Sender:TRtcConnection); override;

  published // WEB SOCKETS

    { Event triggered when a Web Socket connection upgraded by
      a component linked to this DataServerLink is established. }
    property OnWSConnect:TRtcNotifyEvent read FOnWSConnect write FOnWSConnect;
    { Event triggered when a Web Socket connection upgraded by
      a component linked to this DataServerLink was closed. }
    property OnWSDisconnect:TRtcNotifyEvent read FOnWSDisconnect write FOnWSDisconnect;
    end;

  { @abstract(DualDataServerLink, used to link Data Providers to two Servers)

    You can use TRtcDualDataServerLink components to link several related
    @Link(TRtcDataProvider) components to two Servers (for example,
    HTTP and HTTPS). Simply set this component as the Link property
    for all your RtcDataSource components, then set both Servers as
    this components "Server" and "Server2", or "Link" and "Link2" properties.
    You can also combine multiple DualDataServerLink components if
    you want your code running with more Servers.
    @html(<br><br>)

    Check @Link(TRtcAbsDataServerLink) for more info. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcDualDataServerLink=class(TRtcDataServerLink)
  private
    FServer2: TRtcDataServer;
    FLink2: TRtcDataServerLink;

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    function CheckLink(Value:TRtcAbsDataServerLink):boolean; override;
    // @exclude
    procedure RemoveLink(Value:TRtcAbsDataServerLink); override;
    // @exclude
    procedure RemoveServer(Value:TRtcDataServer); override;

    // @exclude
    function GetServer2: TRtcDataServer; virtual;
    // @exclude
    procedure SetServer2(const Value: TRtcDataServer); virtual;

    // @exclude
    function GetLink2: TRtcDataServerLink; virtual;
    // @exclude
    procedure SetLink2(const Value: TRtcDataServerLink); virtual;

    // @exclude
    procedure SetServer(const Value: TRtcDataServer); override;
    // @exclude
    procedure SetLink(const Value: TRtcDataServerLink); override;

    // @exclude
    procedure SetOrder(const Value: integer); override;

  public // HTTP

    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    // @exclude
    procedure Call_PeekReceived(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_RequestAccepted(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_Disconnect(Sender:TRtcConnection); override;

  published // HTTP

    { You can link your components (one or more) to a DataServerLink component
      by assigning your @Link(TRtcDataServerLink) component to this Link property.
      Doing this, you only have to set the Server property for the master
      DataServerLink component and don't need to do it for every single
      DataSource component. }
    property Link2:TRtcDataServerLink read GetLink2 write SetLink2;
    { You can also link your components (one or more) directly to your
      DataServer connection component by assigning your
      @Link(TRtcDataServer) connection component to this Server property.
      This is useful if you have some complex functionality implemented
      in a single DataSource/DataServerLink component and don't need to drag
      the component through another DataServerLink component to get to
      your DataServer. }
    property Server2:TRtcDataServer read GetServer2 write SetServer2;

  public // WEB SOCKETS

    // @exclude
    procedure Call_WSConnect(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDisconnect(Sender:TRtcConnection); override;
    end;

  { @abstract(Data Provider, used to implement events for processing Requests from DataServer)

    You can use TRtcDataProvider components to implement event handlers
    for different requests and combine them to compile a
    Server which can handle any request implemented in those handlers.
    @html(<br><br>)

    By implementing events specified by this component, then
    assigning your @Link(TRtcDataServer) connection component to this
    component's @Link(TRtcAbsDataServerLink.Server) property, or
    @Link(TRtcDataServerLink) (which also has to be somewhere connected to
    the DataServer connection component) to this component's
    @Link(TRtcAbsDataServerLink.Link) property, you can simply integrate
    diferent request handlers into your DataServer. For example,
    a File source (to send files from disk) and a PHP source
    (to read php files from disk, process them by PHP parser and
    send the resulting page out).
    @html(<br><br>)

    Check @Link(TRtcAbsDataServerLink) for more info. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcDataProvider=class(TRtcAbsDataServerLink)
  protected // HTTP
    // @exclude
    FOnListenStart: TRtcNotifyEvent;
    // @exclude
    FOnListenStop: TRtcNotifyEvent;

    // @exclude
    FOnCheckRequest: TRtcNotifyEvent;
    // @exclude
    FOnResponseDone: TRtcNotifyEvent;
    // @exclude
    FOnReadyToSend: TRtcNotifyEvent;

    // @exclude
    FOnDisconnect: TRtcNotifyEvent;
    // @exclude
    FOnDataReceived: TRtcNotifyEvent;
    // @exclude
    FOnDataOut: TRtcNotifyEvent;
    // @exclude
    FOnDataIn: TRtcNotifyEvent;
    // @exclude
    FOnDataSent: TRtcNotifyEvent;

    // @exclude
    FOnSessionOpen: TRtcNotifyEvent;
    // @exclude
    FOnSessionClose: TRtcNotifyEvent;

    // @exclude
    procedure Call_RequestAccepted(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ListenStart(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ListenStop(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_CheckRequest(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_DataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataOut(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataSent(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ReadyToSend(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_Disconnect(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

  public // HTTP

    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

  published // HTTP

    { This event will be mapped as @Link(TRtcServer.OnListenStart) event
      to the assigned Server component and called AFTER the Server's
      OnListenStart event, for all components. This event can be used
      to initialize the component after server starts listening. }
    property OnListenStart:TRtcNotifyEvent read FOnListenStart write FOnListenStart;
    { This event will be mapped as @Link(TRtcServer.OnListenStop) event
      to the assigned Server component and called BEFORE the Server's
      OnListenStop event, for all components. This event can be used
      to de-initialize the component before server stops listening. }
    property OnListenStop:TRtcNotifyEvent read FOnListenStop write FOnListenStop;

    { This event will be called after new session has been opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { This event will be called before existing session is about to close. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;

    { This event will be called when a new Request comes from a client and its
      headers have been read (but not content body). What this event has to do
      is to check the request information available, without reading the request
      body (!do not call 'Read' here) and decide wether it wants to process this
      request or not. If it wants to process the request, it just has to accept it
      here. The processing has to be done from other events defined for this DataProvider.
      @html(<br><br>)
      To accept the request as its own (and so be able to respond to the client),
      DataPrivder has to call "Accept" from it's 'OnCheckRequest' event handler.
      DataProvider has to be able to recognize the Request as it's own,
      simply by checking the Request property of the Sender (as TRtcDataServer),
      without reading the request body (content part).
      @html(<br><br>)

      Example of one OnCheckRequest and OnDataReceived events implementation:
      @longcode(#
        procedure TWebServer.TimeSourceCheckRequest(Sender: TRtcConnection);
          begin
          with Sender do
            if UpperCase(Request.FileName)='/TIME' then
              Accept; // Accept the request.
          // After accepting the request, all your other events will be mapped
          // to the connection, so you will have complete control over the
          // received data and the art of your response.
          end;
        procedure TWebServer.TimeSourceDataReceived(Sender: TRtcConnection);
          begin
          // If the request body is small or of no special interest to you,
          // You can simply respond to the request after it has been completely loaded.
          with Sender do
            if Request.Complete then
              Write(FormatDateTime('dddd, dd.mm.yyyy. hh:nn:ss',Now));
          end; #)
      This simple implementation is a complete implementation for a HTML page
      that shows current date and time on the server when user asks for "/TIME".
      To see what the above example does, click here: http://www.realthinclient.com/time
      @html(<br><br>)

      If your component doesn't Accept the request when it first receives
      it in its CheckRequest event, the same request will be passed to the
      next component in the list, until one component accepts the request.
      If the request was not accepted after all CheckRequest events from all
      components assigned to the Server were passed, then Server's
      OnRequestNotAccepted event will be called. If a component accepts a request,
      all furure events regarding this request will be mapped to the
      component which accepted the request.
      @html(<br><br>)

      This means that CheckRequest is the only event which will be called
      for all DataProvider components, until a component is found which
      wants to process the request (the one that Accepts the request).
      All other request-related events (all but OnListenStart and
      OnListenStop) will ONLY be called by the event handlers defined
      by the component which accepted the request. }
    property OnCheckRequest:TRtcNotifyEvent read FOnCheckRequest write FOnCheckRequest;

    { This event will be mapped as TRtcConnection.OnDataReceived event
      to the assigned Server component and called for all DataReceived
      events for the accepted request. This means that, after you
      have accepted a request from your component's CheckRequest event
      handler, all Server's OnDataReceived events will be only mapped to
      your component's OnDataReceived event. No other component, including
      the DataServer, will receive those events. }
    property OnDataReceived:TRtcNotifyEvent read FOnDataReceived write FOnDataReceived;

    { This event will be mapped as @Link(TRtcConnection.OnDataOut) event
      to the assigned Server component and called for all DataOut
      events for the accepted request. After you have accepted a request from
      your component's CheckRequest event handler, all Server's OnDataOut events
      will be mapped to your component's OnDataOut event. No other component,
      except for the DataServer, will receive those events. @html(<br><br>)

      You can use this event to count how many bytes have been written out. }
    property OnDataOut:TRtcNotifyEvent read FOnDataOut write FOnDataOut;

    { This event will be mapped as @Link(TRtcConnection.OnDataIn) event
      to the assigned Server component and called for all DataIn
      events for the accepted request. After you have accepted a request from
      your component's CheckRequest event handler, all Server's OnDataIn events
      will be mapped to your component's OnDataIn event. No other component,
      except for the DataServer, will receive those events.

      You can use this event to count how many bytes have been read in. }
    property OnDataIn:TRtcNotifyEvent read FOnDataIn write FOnDataIn;

    { This event will be mapped as @Link(TRtcConnection.OnDataSent) event
      to the assigned Server component and called for all DataSent
      events for the accepted request. This means that, after you
      have accepted a request from your component's CheckRequest event
      handler, all Server's OnDataSent events will be only mapped to
      your component's OnDataSent event. No other component, including
      the DataServer, will receive those events. }
    property OnDataSent:TRtcNotifyEvent read FOnDataSent write FOnDataSent;

    { This event will be mapped as @Link(TRtcConnection.OnReadyToSend) event
      to the assigned Server component and called for all ReadyToSend
      events for the accepted request. This means that, after you
      have accepted a request from your component's CheckRequest event
      handler, all Server's OnReadyToSend events will be only mapped to
      your component's OnReadyToSend event. No other component, including
      the DataServer, will receive those events, until you process the
      request completely.
      NOTE: If a connection was upgraded to a Web Socket by this Data Provider,
      the "OnReadyToSend" event will NOT be called (use "OnWSDataSet" instead). }
    property OnReadyToSend:TRtcNotifyEvent read FOnReadyToSend write FOnReadyToSend;

    { This event will be triggered after the Response was sent (Response.Done).
      This is the last event triggered for this request/response cycle.
      If a connection was upgraded to a Web Socket by this Data Provider,
      "Sender.isWebSocket" will be returning TRUE in this event, and the 
      next event to trigger immediately after this will be "OnWSConnect",
      to let you know that a new Web Socket connection is ready for use. }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;

    { This event will be mapped as @Link(TRtcConnection.OnDisconnect) event
      to the assigned Server component and called if your connection gets
      closed while you are still processing the request you accepted.
      This means that, after you have accepted a request from your component's
      CheckRequest event handler, if connection closes before your complete result
      has been sent out to the client, your component will be the only one
      to receive this OnDisconnect event. No other component, including the
      DataServer, will receive the Disconnect event if your component did
      not finish processing the request and sending the resulting data.
      @html(<br><br>)

      If you have stored the "Sender:TRtcConnection" object anywhere,
      remove it in this event and make sure you will NO LONGER use it.
      "Sender:TRtcConnection" component is destroyed after this event!

      NOTE: If you want to react to clients connecting and disconnecting to
      your Server regardless of those event mappings, use the OnConnecting
      and OnDisconnecting events instead of OnConnect/OnDisconnect.

      PS. After a connection has been upgraded to a Web Socket, the
      "OnDisconnect" event will NOT be called anymore! When a Web Socket
      connection closes, the "OnWSDisconnect" even is called instead. }
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;

  protected // WEB SOCKETS

    // @exclude
    FOnWSConnect: TRtcNotifyEvent;
    // @exclude
    FOnWSDataIn: TRtcNotifyEvent;
    // @exclude
    FOnWSDataOut: TRtcNotifyEvent;
    // @exclude
    FOnWSDataReceived: TRtcNotifyEvent;
    // @exclude
    FOnWSDataSent: TRtcNotifyEvent;
    // @exclude
    FOnWSDisconnect: TRtcNotifyEvent;

    // @exclude
    FWS:TRtcWSManager;

    // @exclude
    procedure Call_WSConnect(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDataIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDataOut(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDataSent(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDisconnect(Sender:TRtcConnection); override;

  public // WEB SOCKETS

    { WEB SOCKET MANAGER:
      Web Socket Manager created and used by this TRtcDataProvider
      component to automatically add and remove all Web Sockets
      upgraded from events implemented on this TRtcDataProvider. }
    property wsManager:TRtcWSManager read FWS;

    // WEB SOCKET MANAGER: First connection (returns ID, 0 = no connections)
    function wsFirst:RtcIntPtr;
    // WEB SOCKET MANAGER: Next connection (returns ID, 0 = no more connecitons)
    function wsNext(id:RtcIntPtr):RtcIntPtr;
    // WEB SOCKET MANAGER: Prior connection (returns ID, 0 = no more connections)
    function wsPrior(id:RtcIntPtr):RtcIntPtr;
    // WEB SOCKET MANAGER: Last connection (returns ID, 0 = no connections)
    function wsLast:RtcIntPtr;

    // WEB SOCKET MANAGER: Web Socket Connection count
    function wsCount:integer;

    { WEB SOCKET MANAGER:
      Add Frame "iFrame" to the SENDING QUEUE of the connection "id". @html(<br><br>)

      The "iFrame" object used in this call will be destroyed immediately if this
      call fails, or auto-freed by the connection component if the call succeeds. @html(<br><br>)

      If called with iName<>'', the "iFrame" object will be stored as "Frame[iName]" in
      the connection component and available as "Sender.Frame[iName]" in all RTC events. @html(<br><br>)

      By using SendTo, the conneciton component becomes the *Owner* of the "iFrame" object,
      which means that the "iFrame" object will be managed and destroyed by the connection.
      All pointers and references to this object should be set to NIL after this call. @html(<br><br>)

      Returns 1 if "iFrame" object was added to the queue, or 0 if this call fails. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.  }
    function wSend(id:RtcIntPtr; iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { WEB SOCKET MANAGER:
      Add more "Payload" data to Frame "iFrame", being sent or queued for sending to connection "id".
      Call with "vFinal=TRUE" if this is the final Payload for this Frame.
      Returns 1 if "Payload" was added to the "iFrame" object, or 0 if this call failed. }
    function wSendMore(id:RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcString; vFinal:boolean=False):integer; overload;

    { WEB SOCKET MANAGER:
      Add more "Payload" data to Frame "iFrame", being sent or queued for sending to connection "id".
      Call with "vFinal=TRUE" if this is the final Payload for this Frame.
      Returns 1 if "Payload" was added to the "iFrame" object, or 0 if this call failed. }
    function wSendMore(id:RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcByteArray; vFinal:boolean=False):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id", initialized with: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side
      connections(with a randomly generated "MaskingKey"), everything else is 0. @html(<br><br>)

      This method call is identical to Send(id, TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSend(id:RtcIntPtr; vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id", initialized with: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to Send(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSend(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id", initialized with: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to Send(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSend(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add Frame "iFrame" to the SENDING QUEUE of the connection "id", but ONLY if NOTHING
      is being sent through connection 'id' and the sending queue of connection "id" is EMPTY. @html(<br><br>)

      The "iFrame" object used in this call will be destroyed immediately if this
      call fails, or auto-freed by the connection component if the call succeeds. @html(<br><br>)

      If called with iName<>'', the "iFrame" object will be stored as "Frame[iName]" in
      the connection component and available as "Sender.Frame[iName]" in all RTC events. @html(<br><br>)

      By using SendTo, the conneciton component becomes the *Owner* of the "iFrame" object,
      which means that the "iFrame" object will be managed and destroyed by the connection.
      All pointers and references to this object should be set to NIL after this call. @html(<br><br>)

      Returns 1 if "iFrame" object was added to the queue, or 0 if this call fails. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.  }
    function wSendIfIdle(id:RtcIntPtr; iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id",
      initialized with the values listed below, but ONLY if NOTHING is being sent
      through connection 'id' and the sending queue of connection 'id' is EMPTY: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0. @html(<br><br>)

      This method call is identical to SendIfIdle(id, TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSendIfIdle(id:RtcIntPtr; vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id",
      initialized with the values listed below, but ONLY if NOTHING is being sent
      through connection 'id' and the sending queue of connection 'id' is EMPTY: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to SendIfIdle(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSendIfIdle(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connection "id",
      initialized with the values listed below, but ONLY if NOTHING is being sent
      through connection 'id' and the sending queue of connection 'id' is EMPTY: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to SendIfIdle(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns 1 if the Frame was added to the queue, or 0 if this call has failed. }
    function wSendIfIdle(id:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add Frame "iFrame" to the SENDING QUEUE of all connections managed here.
      The original "iFrame" object is destroyed automatically by this method call. @html(<br><br>)

      If called with iName<>'', a copy of "iFrame" will be created for
      every connection component and stored in the "Frame[iName]" property
      and available in all events triggered by the connection component,
      where it will be accessible using "Sender.Frame[iName]" and
      will be managed (and destroyed) by the "Sender.Frame" property.  @html(<br><br>)

      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.  }
    function wSendToAll(iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all managed connections,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to SendToAll( TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToAll(vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all managed connection,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0.  @html(<br><br>)

      This method call is identical to SendToAll( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToAll(vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all managed connections,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey"), everything else is 0. @html(<br><br>)

      This method call is identical to SendToAll( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToAll(vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add Frame "iFrame" to the SENDING QUEUE of all connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY. @html(<br><br>)

      The original "iFrame" object is destroyed automatically by this method call. @html(<br><br>)

      If called with iName<>'', a copy of "iFrame" will be created for
      every connection component and stored in the "Frame[iName]" property
      and available in all events triggered by the connection component,
      where it will be accessible using "Sender.Frame[iName]" and
      will be managed (and destroyed) by the "Sender.Frame" property.  @html(<br><br>)

      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.  }
    function wSendToIdle(iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0.  @html(<br><br>)

      This method call is identical to SendToIdle( TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToIdle(vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0.  @html(<br><br>)

      This method call is identical to SendToIdle( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToIdle(vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of connections managed by this
      Web Socket Manager where NOTHING is being sent and the sending queue is EMPTY,
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0. @html(<br><br>)

      This method call is identical to SendToIdle( TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Web Socket Frame
      was added to the sending queue, or 0 if this call has failed. }
    function wSendToIdle(vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add Frame "iFrame" to the SENDING QUEUE of all connections managed by this
      Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"). @html(<br><br>)

      The "iFrame" object used in this call will be destroyed immediately if this
      call fails, or auto-freed by the connection component if the call succeeds. @html(<br><br>)

      If called with iName<>'', the "iFrame" object will be stored as "Frame[iName]" in
      the connection component and available as "Sender.Frame[iName]" in all RTC events. @html(<br><br>)

      By using SendTo, the conneciton component becomes the *Owner* of the "iFrame" object,
      which means that the "iFrame" object will be managed and destroyed by the connection.
      All pointers and references to this object should be set to NIL after this call. @html(<br><br>)

      Returns the number of connections where the "iFrame" object was
      added to the queue, or 0 if this call has failed. @html(<br><br>)

      NOTE: The "iFrame.Masked" property is automatically set to TRUE and the
      "iFrame.MaskingKey" property is randomly generated if the Frame object
      is being added to the sending queue of a Client-side connection component.  }
    function wSendToOthers(xid:RtcIntPtr; iFrame:TRtcWSFrame; const iName:RtcWideString=''):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all connections managed by
      this Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"),
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=0),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0. @html(<br><br>)

      This method call is identical to SendToOthers(id, TRtcWSFrame.Create(vOpcode,vFinal,vPayloadLength) ).
      Returns the number of connections where this Frame was added to the queue, or 0 if this call failed. }
    function wSendToOthers(xid:RtcIntPtr; vOpcode:Rtc4Bit; vFinal:boolean=True; vPayloadLength:int64=0):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all connections managed by
      this Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"),
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0.  @html(<br><br>)

      This method call is identical to SendToOthers(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Frame was added to the queue, or 0 if this call failed. }
    function wSendToOthers(xid:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcString; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Add a new Web Socket Frame to the SENDING QUEUE of all connections managed by
      this Web Socket Manager, but NOT to the connection "xid" (send to all except "xid"),
      initialized with the following values: @html(<br><br>)

      Opcode:="vOpcode", Payload = "vPayload", Final:="vFinal" (default=True), PayloadLength:="vPayloadLength" (default=length(vPayload)),
      Masked := "FALSE" for Server-side connections or "TRUE" for Client-side connections
      (with a randomly generated "MaskingKey") and everything else set to 0.  @html(<br><br>)

      This method call is identical to SendToOthers(id, TRtcWSFrame.Create(vOpcode,vPayload,vFinal,vPayloadLength) ).
      Returns the number of connections where this Frame was added to the queue, or 0 if this call failed. }
    function wSendToOthers(xid:RtcIntPtr; vOpcode:Rtc4Bit; const vPayload:RtcByteArray; vFinal:Boolean=True; vPayloadLength:int64=-1):integer; overload;

    { WEB SOCKET MANAGER:
      Disconnect the Web Socket connection "id".
      Returns 1 if a connection with "id" was found (and will be closed),
      or 0 if this call has failed (connection could not be found). }
    function wsDisconnect(id:RtcIntPtr):integer;

    { WEB SOCKET MANAGER:
      Disconnect all Web Socket connections managed here.
      Returns the number of connections found (and now being closed),
      or 0 if this call has failed (no connections found). }
    function wsDisconnectAll:integer;

    { WEB SOCKET MANAGER:
      Clear the SENDING QUEUE for the Web Socket connection "id".
      Returns 1 if a connection with "id" was found (queued Frames will be removed),
      or 0 if this call has failed (connection could not be found).
      This does NOT cancel Web Socket Frames already being SENT out.
      It only removes Web Socket Frames waiting in the queue! }
    function wsClearSendingQueue(id:RtcIntPtr):integer;

    { WEB SOCKET MANAGER:
      Clear SENDING QUEUES of all Web Socket connections managed here.
      Returns 1 if a connection with "id" was found (queued Frames will be removed),
      or 0 if this call has failed (connection could not be found).
      This does NOT cancel Web Socket Frames already being SENT out.
      It only removes Web Socket Frames waiting in the queue! }
    function wsClearAllSendingQueues:integer;

    { WEB SOCKET MANAGER:
      Returns the "ID" of a "Sender:TRtcConnection" component if the
      "Sender:TRtcConnection" is managed by this Web Socket Manager.
      The "ID" returned by "wsGetID" can be used with all methods of
      this Web Socket Manager which require "id:RtcIntPtr" as parameter
      (like "wSendTo", "wsClearSendingQueue" or "wsDisconnect" methods). }
    function wsGetID(const Sender:TRtcConnection):RtcIntPtr;

  published // WEB SOCKETS

    { WEB SOCKETS:
      This is the first event to be called after a HTTP connection
      is upgraded to a Web Socket connection by this Data Provider.
      From here on, the "Sender:TRtcConnection" component can be
      used to send Web Socket Frames though this Web Socket connection
      and/or any other connection upgraded by the same Data Provider. }
    property OnWSConnect:TRtcNotifyEvent read FOnWSConnect write FOnWSConnect;

    { WEB SOCKETS:
      This event will be called every time Web Socket data is received
      for a Web Socket connection upgraded by this Data Provider.
      NOTE: After a connection is upgraded to a Web Socket,
      this event will be called instead of "OnDataReceived". }
    property OnWSDataReceived:TRtcNotifyEvent read FOnWSDataReceived write FOnWsDataReceived;

    { WEB SOCKETS:
      This event will be called after data was written out to
      a Web Socket connection upgraded by this Data Provider.
      You can use this event to count how many bytes have been
      written out to this connection after a Web Socket upgrade.
      NOTE: After a connection is upgraded to a Web Socket,
      this event will be called instead of "OnDataOut". }
    property OnWSDataOut:TRtcNotifyEvent read FOnWSDataOut write FOnWSDataOut;

    { WEB SOCKETS:
      This event will be called when data arrives through a
      Web Socket connection upgraded by this Data Provider.
      You can use this event to count how many bytes have arrived
      through this connection after a Web Socket upgrade.
      NOTE: After a connection is upgraded to a Web Socket,
      this event will be called instead of "OnDataIn". }
    property OnWSDataIn:TRtcNotifyEvent read FOnWSDataIn write FOnWsDataIn;

    { WEB SOCKETS: 
      This event will be called when data has been sent through
      a Web Socket connection upgraded by this Data Provider.
      You can use this event to get notified when the last chunk
      of data has been sent, so you can continue sending more data
      for the current Web Socket Frame or start sending a new Frame.
      NOTE: After a connection is upgraded to a Web Socket,
      this event will be called instead of "OnDataSent". }
    property OnWSDataSent:TRtcNotifyEvent read FOnWSDataSent write FOnWSDataSent;

    { WEB SOCKETS:
      This event will be called when a Web Socket previously upgraded
      by this Data Provider component gets disconnected (closed).
      If you have stored the "Sender:TRtcConnection" object anywhere,
      remove it in this event and make sure you will NO LONGER use it.
      "Sender:TRtcConnection" component will be removed from this
      components Web Socket Manager and DESTROYED *after* this event!
      NOTE: After a connection is upgraded to a Web Socket,
      this event will be called instead of "OnDisconnect". }
    property OnWSDisconnect:TRtcNotifyEvent read FOnWSDisconnect write FOnWSDisconnect;
    end;

implementation

var
  SessCS:TRtcCritSec;
  SessExpList:tStrObjList; // Unlocked Sessions, sorted by Expiring date+time, PeerAddr & Session ID
  SessUnLockList:tStrObjList; // UnLocked Sessions, sorted by Session ID
  SessLockList:tStrObjList; // Locked Sessions, sorted by Session ID

procedure InitSessions;
  begin
  SessCS:=TRtcCritSec.Create;
  SessExpList:=tStrObjList.Create(128);
  SessUnLockList:=tStrObjList.Create(128);
  SessLockList:=tStrObjList.Create(128);
  end;

function GetTotalSessCount:cardinal;
  begin
  SessCS.Acquire;
  try
    if assigned(SessUnLockList) then
      Result:=SessUnLockList.Count
    else
      Result:=0;
    if assigned(SessLockList) then
      Result:=Result+SessLockList.Count;
  finally
    SessCS.Release;
    end;
  end;

function GetTotalLockSessCount:cardinal;
  begin
  SessCS.Acquire;
  try
    if assigned(SessLockList) then
      Result:=SessLockList.Count
    else
      Result:=0;
  finally
    SessCS.Release;
    end;
  end;

function GetTotalUnlockSessCount:cardinal;
  begin
  SessCS.Acquire;
  try
    if assigned(SessUnLockList) then
      Result:=SessUnLockList.Count
    else
      Result:=0;
  finally
    SessCS.Release;
    end;
  end;

procedure DoneSessions;
  var
    o:TObject;
    id:RtcString;
  begin
  {$IFDEF RTC_DEBUG} Log('DoneSessions Begin ...','DEBUG');{$ENDIF}
  if not assigned(SessCS) then
    begin
    {$IFDEF RTC_DEBUG} Log('DoneSessions SessCS = NIL!? End.','DEBUG');{$ENDIF}
    Exit;
    end;

  SessCS.Acquire;
  try
    {$IFDEF RTC_DEBUG} Log('DoneSessions Acquired','DEBUG');{$ENDIF}
    if assigned(SessUnLockList) then
      begin
      {$IFDEF RTC_DEBUG} Log('DoneSessions UnLockList begin ('+Int2Str(SessUnLockList.Count)+')...','DEBUG');{$ENDIF}
      id:=SessUnLockList.search_min(o);
      while (id<>'') and (o<>nil) do
        begin
        {$IFDEF RTC_DEBUG} Log('DoneSessions UnLocked Remove ('+Int2Str(SessUnLockList.Count)+': '+id+')','DEBUG');{$ENDIF}
        try
          SessUnLockList.remove(id);
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('DoneSessions UnLocked.remove("'+id+'")',E,'ERROR');
          end;
        {$IFDEF RTC_EXTDEBUG} Log('   - RtcFreeAndNil','DEBUG');{$ENDIF}
        try
          RtcFreeAndNil(o);
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('DoneSessions UnLocked.RtcFreeAndNil("'+id+'")',E,'ERROR');
          end;
        {$IFDEF RTC_EXTDEBUG} Log('   - Search_Min','DEBUG');{$ENDIF}
        try
          id:=SessUnLockList.search_min(o);
        except
          on E:Exception do
            begin
            id:=''; o:=nil;
            if LOG_AV_ERRORS then
              Log('DoneSessions UnLocked.Search_Min',E,'ERROR');
            end;
          end;
        {$IFDEF RTC_EXTDEBUG} Log('   - OK','DEBUG');{$ENDIF}
        end;
      {$IFDEF RTC_DEBUG} Log('DoneSessions UnLockList free ('+Int2Str(SessUnLockList.Count)+') ...','DEBUG');{$ENDIF}
      try
        RtcFreeAndNil(SessUnLockList);
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('DoneSessions SesUnLockList.FREE!',E,'ERROR');
        end;
      SessUnLockList:=nil;
      {$IFDEF RTC_DEBUG} Log('DoneSessions UnLockList end.','DEBUG');{$ENDIF}
      end;

    if assigned(SessLockList) then
      begin
      {$IFDEF RTC_DEBUG} Log('DoneSessions LockList begin','DEBUG');{$ENDIF}
      id:=SessLockList.search_min(o);
      while (id<>'') and assigned(o) do
        begin
        {$IFDEF RTC_DEBUG} Log('DoneSessions Locked Remove "'+id+'"','DEBUG');{$ENDIF}
        try
          SessLockList.remove(id);
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('DoneSessions Locked.remove("'+id+'")',E,'ERROR');
          end;
        {$IFDEF RTC_EXTDEBUG} Log('   - RtcFreeAndNil','DEBUG');{$ENDIF}
        try
          RtcFreeAndNil(o);
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('DoneSessions Locked.RtcFreeAndNil("'+id+'")',E,'ERROR');
          end;
        {$IFDEF RTC_EXTDEBUG} Log('   - Search_Min','DEBUG');{$ENDIF}
        try
          id:=SessLockList.search_min(o);
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              begin
              id:='';o:=nil;
              Log('DoneSessions Locked.RtcFreeAndNil("'+id+'")',E,'ERROR');
              end;
          end;
        {$IFDEF RTC_EXTDEBUG} Log('   - OK','DEBUG');{$ENDIF}
        end;
      {$IFDEF RTC_DEBUG} Log('DoneSessions LockList free ...','DEBUG');{$ENDIF}
      try
        RtcFreeAndNil(SessLockList);
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('DoneSessions SesLockList.FREE!',E,'ERROR');
        end;
      SessLockList:=nil;
      {$IFDEF RTC_DEBUG} Log('DoneSessions LockList end.','DEBUG');{$ENDIF}
      end;

    if assigned(SessExpList) then
      begin
      {$IFDEF RTC_DEBUG} Log('DoneSessions ExpList free ...','DEBUG');{$ENDIF}
      try
        RtcFreeAndNil(SessExpList);
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('DoneSessions SesExpList.FREE!',E,'ERROR');
        end;
      SessExpList:=nil;
      {$IFDEF RTC_DEBUG} Log('DoneSessions ExpList end.','DEBUG');{$ENDIF}
      end;
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('DoneSessions',E,'ERROR');
    end;
  {$IFDEF RTC_DEBUG} Log('DoneSessions Releasing ...','DEBUG');{$ENDIF}
  SessCS.Release;
  {$IFDEF RTC_DEBUG} Log('DoneSessions Released.','DEBUG');{$ENDIF}

  try
    RtcFreeAndNil(SessCS);
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('DoneSessions RtcFreeAndNil(SessCS)',E,'ERROR');
    end;
  {$IFDEF RTC_DEBUG} Log('DoneSessions end.','DEBUG');{$ENDIF}
  end;

{$IFDEF UseGUIDs}
function NewSessionID:RtcString;
  var
    GUID: TGUID;
  function GuidToStr:RtcString;
    begin
    Result:= RtcString( IntToHex(GUID.D1,8)+IntToHex(GUID.D2,4)+IntToHex(GUID.D3,4)+
             IntToHex(GUID.D4[0],2)+IntToHex(GUID.D4[1],2)+IntToHex(GUID.D4[2],2)+
             IntToHex(GUID.D4[3],2)+IntToHex(GUID.D4[4],2)+IntToHex(GUID.D4[5],2)+
             IntToHex(GUID.D4[6],2)+IntToHex(GUID.D4[7],2) );
    end;
  begin
  if CreateGuid(GUID) = S_OK then
    Result := GUIDToStr
  else
    Result := '';
  end;
{$ELSE}
function NewSessionID(const OldID:RtcString; const PeerAddr:RtcString):RtcString;
  var
    tmp:RtcString;
  function PeerAddrToSessionID:RtcString;
    const
      code:array[0..15] of RtcChar =
                ('a','A','b','B','c','C','d','D','e','E','f','F','g','G','h','H');
    var
      ip,loc:integer;
      myIP:byte;
      st:array[1..4] of RtcString;
    begin
    Result:='';
    for ip:=1 to 4 do
      st[ip]:='';

    // Sort out numbers from the IP address
    ip:=1; loc:=1;
    while length(PeerAddr)>=loc do
      begin
      if PeerAddr[loc] in ['0'..'9'] then
        st[ip]:=st[ip]+PeerAddr[loc]
      else if PeerAddr[loc]='.' then
        Inc(ip);
      Inc(loc);
      end;

    // Convert IP numbers to Hex RtcString
    for ip:=1 to 4 do
      begin
      if st[ip]<>'' then
        begin
        myIP:=Str2IntDef(st[ip],0);
        Result := Result +
                  code[myIP shr 4 and $F]+
                  code[myIP and $F];
        end
      else
        Result:=Result+code[0]+code[0];
      end;

    // Add 10 random letters/numbers
    for loc:=1 to 10 do
      begin
      ip:=random(10+26+26);
      if ip<10 then
        Result:=Result+RtcChar(Ord('0')+ip)
      else if ip<36 then
        Result:=Result+RtcChar(Ord('A')+ip-10)
      else
        Result:=Result+RtcChar(Ord('a')+ip-36)
      end;
    end;
  procedure IncID(var ID:RtcString);
    var
      loc:integer;
      ok:boolean;
    begin
    ok:=False;
    loc:=length(ID);
    while loc>0 do
      begin
      if ID[loc]='9' then
        begin
        ID[loc]:='A';
        ok:=True;
        Break;
        end
      else if ID[loc]='Z' then
        begin
        ID[loc]:='a';
        ok:=True;
        Break;
        end
      else if ID[loc]='z' then
        ID[loc]:='0' // carry 1 forward
      else
        begin
        ID[loc]:=RtcChar(Ord(ID[loc])+1);
        ok:=True;
        Break;
        end;
      Dec(Loc);
      end;
    if not ok then
      ID:='1'+ID;
    end;

  begin
  if OldID='' then
    Result:='1'+PeerAddrToSessionID
  else
    begin
    Result:=PeerAddrToSessionID;
    tmp:=Copy(OldID,1,length(OldID)-length(Result));
    IncID(tmp);
    Result:=tmp+Result;
    end;
  end;
{$ENDIF}

function SessionTimeToStr(v:TDateTime):RtcString;
  var
    y,m,d,hh,mm,ss,ms:word;
    p:integer;
    str:RtcString;
    len:word;
  begin
  Result:='00000000000000000';

  DecodeDate(v, y,m,d);
  DecodeTime(v, hh,mm,ss,ms);

  p:=1;
  str:=Int2Str(y); len:=length(str);
  Inc(p,4-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(m); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(d); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(hh); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(mm); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(ss); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len*SizeOf(RtcChar)); Inc(p,len);

  str:=Int2Str(ms); len:=length(str);
  Inc(p,3-len); Move(str[1],Result[p],len*SizeOf(RtcChar));
  end;

{function SessionTimeToStr(DT:TDateTime):RtcString;
  begin
  Result:=FormatDateTime('yyyymmddhhnnss',DT);
  end;}

{ Find Session by SessionID and Lock the Session:
   - remove from UnLocked and Expiring list
   - add to Locked list }
function FindSession(const ID,PeerAddr,ForwardedFor: RtcString):TRtcServerSession;
  var
    id2:RtcString;
    o:TObject;
  begin
  if ID='' then
    Result:=nil
  else
    begin
    SessCS.Acquire;
    try
      if assigned(SessUnLockList) then
        begin
        id2:=ID;
        {$IFNDEF UseGUIDs}
        // Set ID to desired length, so it will be sorted as a numeric value.
        while length(id2)<RTC_SESSIONID_LENGTH do
          id2:=' '+id2;
        {$ENDIF}

        // Find Session with Session ID
        o:=SessUnLockList.search(id2);
        if o<>nil then
          begin
          Result:=TRtcServerSession(o);

          // Check if we have the right to access this session
          if Result.DenyAccess(PeerAddr,ForwardedFor) then
            Result:=nil
          else
            begin
            // Remove from UnLocked list
            SessUnLockList.remove(id2);
            // Add to Locked list
            SessLockList.insert(id2,Result);

            // Remove from Expiring list
            SessExpList.remove(SessionTimeToStr(Result.ExpireTime)+Result.ID);
            end;
          end
        else
          Result:=nil;
        end
      else
        Result:=nil;
    finally
      SessCS.Release;
      end;
    end;
  end;

{ Check if Session with SessionID exists }
function HaveSession(const ID,PeerAddr,ForwardedFor: RtcString):boolean;
  var
    id2:RtcString;
    o:TObject;
    sess:TRtcServerSession;
  begin
  if ID='' then
    Result:=false
  else
    begin
    SessCS.Acquire;
    try
      if assigned(SessUnLockList) then
        begin
        id2:=ID;
        {$IFNDEF UseGUIDs}
        // Set ID to desired length, so it will be sorted as a numeric value.
        while length(id2)<RTC_SESSIONID_LENGTH do
          id2:=' '+id2;
        {$ENDIF}

        // Find Session with Session ID inside unlocked sessions list
        o:=SessUnLockList.search(id2);
        if o<>nil then
          begin
          sess:=TRtcServerSession(o);
          // Check if we have the right to access this session
          Result:=not sess.DenyAccess(PeerAddr,ForwardedFor);
          end
        else
          begin
          // Find Session with Session ID inside Locked Sessions list
          o:=SessLockList.search(id2);
          if o<>nil then
            begin
            sess:=TRtcServerSession(o);
            // Check if we have the right to access this session
            Result:=not sess.DenyAccess(PeerAddr,ForwardedFor);
            end
          else
            Result:=False;
          end;
        end
      else
        Result:=False;
    finally
      SessCS.Release;
      end;
    end;
  end;

{ Find Session by SessionID and Close the Session }
function CloseSessionID(const ID,PeerAddr,ForwardedFor: RtcString; _Event:TRtcSimpleEvent):boolean;
  var
    id2:RtcString;
    o:TObject;
    sess:TRtcServerSession;
  begin
  if ID='' then
    Result:=false
  else
    begin
    SessCS.Acquire;
    try
      if assigned(SessUnLockList) then
        begin
        id2:=ID;
        {$IFNDEF UseGUIDs}
        // Set ID to desired length, so it will be sorted as a numeric value.
        while length(id2)<RTC_SESSIONID_LENGTH do
          id2:=' '+id2;
        {$ENDIF}

        // Find Session with Session ID inside unlocked sessions list
        o:=SessUnLockList.search(id2);
        if o<>nil then
          begin
          sess:=TRtcServerSession(o);
          // Check if we have the right to access this session
          if sess.DenyAccess(PeerAddr,ForwardedFor) then
            Result:=False
          else
            begin
            // Remove from UnLocked list
            SessUnLockList.remove(id2);
            // Remove from Expiring list
            SessExpList.remove(SessionTimeToStr(sess.ExpireTime)+sess.ID);
            // Call SessionClose event.
            if assigned(_Event) then
              _Event(sess);
            // Free session object
            RtcFreeAndNil(sess);
            Result:=True;
            end;
          end
        else
          begin
          // Find Session with Session ID inside Locked Sessions list
          o:=SessLockList.search(id2);
          if o<>nil then
            begin
            sess:=TRtcServerSession(o);
            // Check if we have the right to access this session
            if sess.DenyAccess(PeerAddr,ForwardedFor) then
              Result:=False
            else
              begin
              sess.FinalExpire:=Now;
              Result:=True;
              end;
            end
          else
            Result:=False;
          end;
        end
      else
        Result:=False;
    finally
      SessCS.Release;
      end;
    end;
  end;

{ TRtcDataServer }

class function TRtcDataServer.New: TRtcDataServer;
  begin
  Result:=Create(nil);
  end;

function TRtcDataServer.GetSession:TRtcSession;
  begin
  Result:=FSession;
  end;

constructor TRtcDataServer.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FMyRequest:=TRtcServerRequest.Create;
  FMyResponse:=TRtcServerResponse.Create;

  FRequest:=FMyRequest;
  FResponse:=FMyResponse;

  FRequestFixup:=TRtcServerRequestFixup.Create;

  FActiveLink:=nil;
  FDataServerLinks:=nil;
  FDataServerLinks_Owner:=False;
  FSession:=nil;
  end;

destructor TRtcDataServer.Destroy;
  begin
  try
    if FDataServerLinks_Owner and assigned(FDataServerLinks) then
      begin
      RemoveAllDataServerLinks;
      FDataServerLinks_Owner:=False;
      RtcFreeAndNil(FDataServerLinks);
      end;
    InitSession;
    FActiveLink:=nil;

    RtcFreeAndNil(FMyRequest); FRequest:=nil;
    RtcFreeAndNil(FMyResponse); FResponse:=nil;
    RtcFreeAndNil(FRequestFixup);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataServer.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataServer.CopyFrom(Dup: TRtcServer);
  begin
  inherited CopyFrom(Dup);

  FActiveLink:=nil;
  InitSession;

  FixupRequest:=TRtcDataServer(Dup).FixupRequest;

  OnRequestAccepted:=TRtcDataServer(Dup).OnRequestAccepted;
  OnRequestNotAccepted:=TRtcDataServer(Dup).OnRequestNotAccepted;
  OnResponseDone:=TRtcDataServer(Dup).OnResponseDone;
  OnPeekRequest:=TRtcDataServer(Dup).OnPeekRequest;
  OnSessionOpen:=TRtcDataServer(Dup).OnSessionOpen;
  OnSessionClose:=TRtcDataServer(Dup).OnSessionClose;

  if FDataServerLinks_Owner then
    RtcFreeAndNil(FDataServerLinks);
  FDataServerLinks_Owner:=False;
  FDataServerLinks:=TRtcDataServer(Dup).FDataServerLinks;
  end;

procedure TRtcDataServer.CallListenStart;
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  inherited;
  FActiveLink:=nil;
  InitSession;
  if assigned(FDataServerLinks) then
    for idx:=0 to FDataServerLinks.Count-1 do
      begin
      FMyLink:=FDataServerLinks.GetLink(idx);
      FMyLink.Call_ListenStart(self);
      end;
  end;

procedure TRtcDataServer.CallListenStop;
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  if assigned(FDataServerLinks) then
    for idx:=0 to FDataServerLinks.Count-1 do
      begin
      FMyLink:=FDataServerLinks.GetLink(idx);
      FMyLink.Call_ListenStop(self);
      end;
  inherited;
  end;

function TRtcDataServer.DoWaitForStopListen(UserInteractionAllowed: boolean; AllowMessageProcessing:boolean; _Timeout:cardinal): TRtcWaitForStopListenResult;
  begin
  Result:=RtcWaitFor( ServerConnectionsClosed,ServerConnectionsClosed,_Timeout,
                      UserInteractionAllowed,AllowMessageProcessing);
  end;

function TRtcDataServer.StopListenNow(_Timeout:cardinal=0; UserInteractionAllowed: boolean=False; AllowMessageProcessing:boolean=True): TRtcWaitForStopListenResult;
  begin
  if self=nil then
    Result:=wait_OK
  else if State>conPrepared then
    begin
    StopListen;
    Result:=DoWaitForStopListen(UserInteractionAllowed,AllowMessageProcessing,_Timeout);
    if Result=wait_OK then Sleep(50);
    end
  else
    Result:=wait_OK;
  end;

{ DataServer.OnDataReceived event will be called only if
  there is no DataServerLink component to accept the request. }
procedure TRtcDataServer.CallDataReceived;
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  if Request.Accepted then
    begin
    if not assigned(FActiveLink) then
      CallRequestNotAccepted
    else if isWebSocket then
      begin
      FWebSocketData:=True;
      FActiveLink.Call_WSDataReceived(self);
      ws_ClearTempFrames;
      end
    else
      begin
      if assigned(FOnPeekRequest) then
        FOnPeekRequest(self);
      FActiveLink.Call_DataReceived(self);
      end;
    Flush;
    end
  else
    begin
    FActiveLink:=nil;
    InitSession;

    if assigned(FDataServerLinks) then
      for idx:=0 to FDataServerLinks.Count-1 do
        begin
        FMyLink:=FDataServerLinks.GetLink(idx);
        FMyLink.Call_CheckRequest(self);
        if Request.Accepted or not Request.Active then
          Break;
        end;

    if Request.Active then
      if not Request.Accepted then
        begin
        InitSession;
        FActiveLink:=nil;
        CallRequestNotAccepted;
        Flush;
        end
      else if not Response.Sent then
        begin
        if assigned(FActiveLink) then
          begin
          if assigned(FOnPeekRequest) then
            FOnPeekRequest(self);
          FActiveLink.Call_DataReceived(self);
          end
        else
          CallRequestNotAccepted;
        Flush;
        end;
    end;
  end;

{ DataServer.OnDataOut event will be called only if
  the request was not accepted by any DataServerLink component
  (DataServer is probably sending the response). }
procedure TRtcDataServer.CallDataOut;
  begin
  if assigned(FActiveLink) then
    if isWebSocket then
      FActiveLink.Call_WSDataOut(self)
    else
      FActiveLink.Call_DataOut(self);

  inherited;

  Flush;
  end;

{ DataServer.OnDataIn event will be called only if
  the request was not accepted by any DataServerLink component
  (DataServer is probably receiving a request). }
procedure TRtcDataServer.CallDataIn;
  begin
  if assigned(FActiveLink) then
    if isWebSocket then
      FActiveLink.Call_WSDataIn(self)
    else
      FActiveLink.Call_DataIn(self);

  inherited;
  end;

{ DataServer.OnDataSent event will be called only if
  the request was not accepted by any DataServerLink component
  (DataServer is probably sending the response). }
procedure TRtcDataServer.CallDataSent;
  begin
  if not assigned(FActiveLink) then
    begin
    inherited;
    Flush;
    end
  else if isWebSocket then
    begin
    FActiveLink.Call_WSDataSent(self);
    Flush;
    end
  else
    begin
    FActiveLink.Call_DataSent(self);
    Flush;
    if Response.Done then
      begin
      if Con.isWebSocket then
        FWebSocket:=True;
      CallResponseDone;
      if isWebSocket then
        begin
        FActiveLink.Call_WSConnect(self);
        Con.isWSManualRead:=Request.ManualRead;
        end
      else
        begin
        Request.Accepted:=False;
        Request.Active:=False;
        FActiveLink:=nil;
        InitSession;
        end;
      end;
    end;
  end;

{ DataServer.OnReadyToSend event will be called if:
  1. DataServer.OnDataSent event was just triggered,
     which means that DataServer is processing a request, or
  2. Response just sent out for the last Request being processed. }
procedure TRtcDataServer.CallReadyToSend;
  begin
  if not assigned(FActiveLink) then
    inherited
  else if isWebSocket then
    ws_SendNextQueuedFrame(True)
  else
    FActiveLink.Call_ReadyToSend(self);
  Flush;
  end;

{ DataServer.OnDisconnect event will ONLY be called if
  DataServer was the one processing a request when
  conncetion got lost. This event means that data
  sent out was most likely not delivered.
  DataServer.OnDisconnect event will NOT be called
  if the request was not accepted.
  To catch all connect and disconnect events for DataServer,
  use OnClientConnect and onClientDisconnect. }
procedure TRtcDataServer.CallDisconnect;
  begin
  if Request.Accepted then
    if assigned(FActiveLink) then
      if isWebSocket then
        FActiveLink.Call_WSDisconnect(self)
      else
        FActiveLink.Call_Disconnect(self);

  inherited; // call DataServer's OnDisconnect event

  FActiveLink:=nil;
  InitSession;

  ws_Clear;
  end;

procedure TRtcDataServer.AddDataServerLink(Value: TRtcAbsDataServerLink);
  begin
  if not assigned(FDataServerLinks) then
    begin
    FDataServerLinks:=TRtcDataServerLinkList.Create;
    FDataServerLinks_Owner:=True;
    end;
  FDataServerLinks.Add(Value);
  end;

procedure TRtcDataServer.RemoveDataServerLink(Value: TRtcAbsDataServerLink);
  begin
  if assigned(FDataServerLinks) then
    FDataServerLinks.Remove(Value);
  end;

procedure TRtcDataServer.RemoveAllDataServerLinks;
  var
    Link:TRtcAbsDataServerLink;
  begin
  if FDataServerLinks_Owner then
    if assigned(FDataServerLinks) then
      while FDataServerLinks.Count>0 do
        begin
        Link:=TRtcAbsDataServerLink(FDataServerLinks.Get(0));
        Link.RemoveServer(self);
        end;
  end;

procedure TRtcDataServer.SetActiveLink(Link: TRtcAbsDataServerLink);
  begin
  FActiveLink:=Link;
  end;

{ TRtcDataServer }

procedure TRtcDataServer.Accept;
  begin
  if not Request.Accepted then
    begin
    Request.Accepted:=True;

    if not Request.Filtering then
      begin
      CallRequestAccepted;
      Flush;
      end;
    end;
  end;

procedure TRtcDataServer.CallRequestAccepted;
  begin
  if assigned(FActiveLink) then
    FActiveLink.Call_RequestAccepted(self);
  if assigned(FOnRequestAccepted) then
    FOnRequestAccepted(self);
  end;

procedure TRtcDataServer.CallResponseDone;
  begin
  if assigned(FActiveLink) then
    FActiveLink.Call_ResponseDone(self);
  if assigned(FOnResponseDone) then
    FOnResponseDone(self);
  end;

procedure TRtcDataServer.CallRequestNotAccepted;
  begin
  if assigned(FOnRequestNotAccepted) then
    FOnRequestNotAccepted(self)
  else
    Disconnect;
  end;

procedure TRtcDataServer.InitSession;
  begin
  if assigned(FSession) then
    begin
    FSession.UnLock(CallSessionClose);
    FSession:=nil;
    end;
  end;

function TRtcDataServer.HaveSession(const ID: RtcString): boolean;
  begin
  Result:=TRtcServerSession.Have(ID,PeerAddr,Request.ForwardedFor);
  end;

function TRtcDataServer.CloseSession(const ID: RtcString): boolean;
  begin
  Result:=TRtcServerSession.CloseID(ID,PeerAddr,Request.ForwardedFor,CallSessionClose);
  end;

function TRtcDataServer.FindSession(const ID: RtcString): boolean;
  begin
  InitSession;
  FSession:=TRtcServerSession.Find(ID,PeerAddr,Request.ForwardedFor);
  Result:=assigned(FSession);
  end;

procedure TRtcDataServer.OpenSession(LockType:TRtcSessionLockType);
  begin
  InitSession;
  FSession:=TRtcServerSession.Open(LockType,PeerAddr,Request.ForwardedFor);
  if assigned(FSession) then
    CallSessionOpen;
  end;

procedure TRtcDataServer.CallSessionClose(Sender: TObject);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
    FOldSess:TRtcServerSession;
  begin
  FOldSess:=FSession;
  EnterEvent;
  try
    FSession:=TRtcServerSession(Sender);

    if assigned(FDataServerLinks) then
      for idx:=0 to FDataServerLinks.Count-1 do
        begin
        FMyLink:=FDataServerLinks.GetLink(idx);
        FMyLink.Call_SessionClose(self);
        end;

    if assigned(FOnSessionClose) then
      FOnSessionClose(self);
  finally
    FSession:=FOldSess;
    LeaveEvent;
    end;
  end;

procedure TRtcDataServer.CallSessionOpen;
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  EnterEvent;
  try
    if assigned(FOnSessionOpen) then
      FOnSessionOpen(self);

    if assigned(FDataServerLinks) then
      for idx:=0 to FDataServerLinks.Count-1 do
        begin
        FMyLink:=FDataServerLinks.GetLink(idx);
        FMyLink.Call_SessionOpen(self);
        end;
  finally
    LeaveEvent;
    end;
  end;

function TRtcDataServer.GetRequest:TRtcRequest;
  begin
  Result:=FRequest;
  end;

function TRtcDataServer.GetResponse:TRtcResponse;
  begin
  Result:=FResponse;
  end;

procedure TRtcDataServer.SetRequest(const Value: TRtcRequest);
  begin
  if Value=nil then
    FRequest:=nil
  else if Value is TRtcServerRequest then
    FRequest := TRtcServerRequest(Value)
  else
    raise ERtcConnection.Create('Invalid "Request" assignment!');
  end;

procedure TRtcDataServer.SetResponse(const Value: TRtcResponse);
  begin
  if Value=nil then
    FResponse:=nil
  else if Value is TRtcServerResponse then
    FResponse := TRtcServerResponse(Value)
  else
    raise ERtcConnection.Create('Invalid "Response" assignment!');
  end;

procedure TRtcDataServer.SetSrvRequest(const Value: TRtcServerRequest);
  begin
  FRequest := Value;
  end;

procedure TRtcDataServer.SetSrvResponse(const Value: TRtcServerResponse);
  begin
  FResponse := Value;
  end;

procedure TRtcDataServer.CallLastWrite;
  begin
  inherited;
  InitSession;
  end;

function TRtcDataServer.TotalSessionsCount: cardinal;
  begin
  Result:=GetTotalSessCount;
  end;

function TRtcDataServer.TotalSessionsLocked: cardinal;
  begin
  Result:=GetTotalLockSessCount;
  end;

function TRtcDataServer.TotalSessionsUnlocked: cardinal;
  begin
  Result:=GetTotalUnlockSessCount;
  end;

procedure TRtcDataServer.UnLockSession;
  begin
  InitSession;
  end;

procedure TRtcDataServer.SetRequestFixup(const Value: TRtcServerRequestFixup);
  begin
  if assigned(Value) then
    FRequestFixup.Assign(Value);
  end;

function TRtcDataServer.GetObjectManager: TRtcRemoteObjectManager;
  begin
  if assigned(FActiveLink) then
    Result:=FActiveLink.GetObjectManager(self)
  else
    raise Exception.Create('Active component does NOT have automated "Linked Objects" support.');
  end;

procedure TRtcDataServer.ActivateObjectManager(xCreate:boolean=True);
  begin
  if assigned(FActiveLink) then
    FActiveLink.ActivateObjectManager(self,xCreate)
  else
    raise Exception.Create('Active component does NOT have automated "Linked Objects" support.');
  end;

{ TRtcServerComponentList }

constructor TRtcServerComponentList.Create;
  begin
  inherited;
  FList:=TRtcObjectList.Create;
  end;

destructor TRtcServerComponentList.Destroy;
  begin
  try
    RtcFreeAndNil(FList);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcServerComponentList.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcServerComponentList.Add(Value: TRtcServerComponent);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx>=0 then
    FList.Delete(idx);

  idx:=0;
  while idx<FList.Count do
    if Get(idx).GetOrder>=Value.GetOrder then
      Break // insert here!
    else
      Inc(idx);

  FList.Insert(idx, Value);
  end;

function TRtcServerComponentList.Count: integer;
  begin
  Result:=FList.Count;
  end;

procedure TRtcServerComponentList.Remove(Value: TRtcServerComponent);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx>=0 then
    FList.Delete(idx);
  end;

procedure TRtcServerComponentList.RemoveAll;
  begin
  if assigned(FList) then
    FList.Clear;
  end;

function TRtcServerComponentList.Get(index:integer): TRtcServerComponent;
  begin
  if (index>=0) and (index<FList.Count) then
    Result:=TRtcServerComponent(FList.Items[index])
  else
    Result:=nil;
  end;

{ TRtcDataServerLinkList }

function TRtcDataServerLinkList.GetLink(index:integer): TRtcAbsDataServerLink;
  begin
  if (index>=0) and (index<Count) then
    Result:=TRtcAbsDataServerLink(Get(index))
  else
    Result:=nil;
  end;

{ TRtcAbsDataServerLink }

constructor TRtcAbsDataServerLink.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FServer:=nil;
  FLink:=nil;
  FOrder:=0;
  end;

destructor TRtcAbsDataServerLink.Destroy;
  begin
  try
    Server:=nil; // remove from DataServer
    Link:=nil; // remove from parent DataServerLink
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcAbsDataServerLink.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcAbsDataServerLink.SetLink(const Value: TRtcDataServerLink);
  begin
  if Value<>FLink then
    begin
    if assigned(FLink) then
      begin
      FLink.RemoveChildLink(self);
      FLink:=nil;
      end;

    if assigned(Value) then
      begin
      Server:=nil; // can not be maped to DataServer and to DataServerLink at the same time.

      // Check for circular reference before assigning!
      if Value=self then
        raise Exception.Create('Circular DataServerLink reference!');
      if Value.CheckLink(self) then
        raise Exception.Create('Circular DataServerLink reference!');
      if CheckLink(Value) then
        raise Exception.Create('Circular DataServerLink reference!');

      FLink:=Value;
      FLink.AddChildLink(self);
      end;
    end;
  end;

function TRtcAbsDataServerLink.GetLink: TRtcDataServerLink;
  begin
  Result:=FLink;
  end;

procedure TRtcAbsDataServerLink.SetServer(const Value: TRtcDataServer);
  begin
  if Value<>FServer then
    begin
    if assigned(FServer) then
      begin
      FServer.RemoveDataServerLink(self);
      FServer:=nil;
      end;

    if assigned(Value) then
      begin
      Link:=nil; // can not be linked to DataServerLink and DataServer at the same time.
      FServer:=Value;
      FServer.AddDataServerLink(self);
      end;
    end;
  end;

function TRtcAbsDataServerLink.GetServer: TRtcDataServer;
  begin
  Result:=FServer;
  end;

procedure TRtcAbsDataServerLink.SetOrder(const Value: integer);
  begin
  if Value<>FOrder then
    begin
    FOrder:=Value;
    // Update order
    if assigned(FLink) then
      FLink.AddChildLink(self)
    else if assigned(FServer) then
      FServer.AddDataServerLink(self);
    end;
  end;

function TRtcAbsDataServerLink.GetOrder: integer;
  begin
  Result:=FOrder;
  end;

procedure TRtcAbsDataServerLink.RemoveLink(Value: TRtcAbsDataServerLink);
  begin
  if Value=FLink then Link:=nil;
  end;

procedure TRtcAbsDataServerLink.RemoveServer(Value: TRtcDataServer);
  begin
  if Value=FServer then Server:=nil;
  end;

function TRtcAbsDataServerLink.CheckLink(Value: TRtcAbsDataServerLink):boolean;
  begin
  if Value=FLink then
    Result:=True
  else if assigned(FLink) then
    Result:=FLink.CheckLink(Value)
  else
    Result:=False;
  end;

procedure TRtcAbsDataServerLink.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FServer then
      SetServer(nil)
    else if AComponent=FLink then
      SetLink(nil);
  end;

procedure TRtcAbsDataServerLink.ActivateObjectManager(Sender:TRtcConnection; xCreate:boolean=True);
  begin
  raise Exception.Create('This component does NOT have automated "Linked Objects" support.');
  end;

function TRtcAbsDataServerLink.GetObjectManager(Sender:TRtcConnection): TRtcRemoteObjectManager;
  begin
  {$IFDEF FPC}
  Result:=nil;
  {$ENDIF}
  raise Exception.Create('This component does NOT have automated "Linked Objects" support.');
  end;

procedure TRtcAbsDataServerLink.Call_CheckRequest(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_DataIn(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_DataOut(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_DataReceived(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_DataSent(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_Disconnect(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_ListenStart(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_ListenStop(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_RequestAccepted(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_ResponseDone(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_SessionClose(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_SessionOpen(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_WSConnect(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_WSDataIn(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_WSDataOut(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_WSDataReceived(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_WSDataSent(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataServerLink.Call_WSDisconnect(Sender: TRtcConnection);
  begin
  end;

{ TRtcDataServerLink }

constructor TRtcDataServerLink.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FDataServerLinks:=TRtcDataServerLinkList.Create;
  end;

destructor TRtcDataServerLink.Destroy;
  begin
  try
    RemoveAllChildLinks;
    RtcFreeAndNil(FDataServerLinks);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataServerLink.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataServerLink.AddChildLink(Value: TRtcAbsDataServerLink);
  begin
  FDataServerLinks.Add(Value);
  end;

procedure TRtcDataServerLink.RemoveChildLink(Value: TRtcAbsDataServerLink);
  begin
  FDataServerLinks.Remove(Value);
  end;

procedure TRtcDataServerLink.RemoveAllChildLinks;
  var
    _Link:TRtcAbsDataServerLink;
  begin
  while FDataServerLinks.Count>0 do
    begin
    _Link:=TRtcAbsDataServerLink(FDataServerLinks.Get(0));
    _Link.RemoveLink(self);
    end;
  end;

procedure TRtcDataServerLink.Call_CheckRequest(Sender:TRtcConnection);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
    Rejected:boolean;
  begin
  if assigned(FOnFilterRequest) then
    begin
    Sender.Request.Filtering:=True;
    try
      FOnFilterRequest(Sender);
      Rejected:=not Sender.Request.Accepted;
    finally
      Sender.Request.Filtering:=False;
      Sender.Request.Accepted:=False;
      end;
    if Rejected then
      Exit;
    end;

  for idx:=0 to FDataServerLinks.Count-1 do
    begin
    FMyLink:=FDataServerLinks.GetLink(idx);
    FMyLink.Call_CheckRequest(Sender);
    if Sender.Request.Accepted or not Sender.Request.Active then
      Exit;
    end;

  end;

procedure TRtcDataServerLink.Call_ListenStart(Sender: TRtcConnection);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  if assigned(FOnListenStart) then
    FOnListenStart(Sender);

  for idx:=0 to FDataServerLinks.Count-1 do
    begin
    FMyLink:=FDataServerLinks.GetLink(idx);
    FMyLink.Call_ListenStart(Sender);
    end;
  end;

procedure TRtcDataServerLink.Call_ListenStop(Sender: TRtcConnection);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  for idx:=0 to FDataServerLinks.Count-1 do
    begin
    FMyLink:=FDataServerLinks.GetLink(idx);
    FMyLink.Call_ListenStop(Sender);
    end;

  if assigned(FOnListenStop) then
    FOnListenStop(Sender);
  end;

procedure TRtcDataServerLink.Call_RequestAccepted(Sender: TRtcConnection);
  begin
  if assigned(FOnRequestAccepted) then
    FOnRequestAccepted(Sender);

  if assigned(Link) then
    Link.Call_RequestAccepted(Sender);
  end;

procedure TRtcDataServerLink.Call_PeekReceived(Sender: TRtcConnection);
  begin
  if assigned(Link) then
    Link.Call_PeekReceived(Sender);

  if assigned(FOnPeekRequest) then
    FOnPeekRequest(Sender);
  end;

procedure TRtcDataServerLink.Call_ResponseDone(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseDone) then
    FOnResponseDone(Sender);

  if assigned(Link) then
    Link.Call_ResponseDone(Sender);
  end;

procedure TRtcDataServerLink.Call_WSConnect(Sender: TRtcConnection);
  begin
  if assigned(FOnWSConnect) then
    FOnWSConnect(Sender);

  if assigned(Link) then
    Link.Call_WSConnect(Sender);
  end;

procedure TRtcDataServerLink.Call_Disconnect(Sender:TRtcConnection);
  begin
  if assigned(FOnDisconnect) then
    FOnDisconnect(Sender);

  if assigned(Link) then
    Link.Call_Disconnect(Sender);
  end;

procedure TRtcDataServerLink.Call_WSDisconnect(Sender: TRtcConnection);
  begin
  if assigned(FOnWSDisconnect) then
    FOnWSDisconnect(Sender);

  if assigned(Link) then
    Link.Call_WSDisconnect(Sender);
  end;

procedure TRtcDataServerLink.Call_DataReceived(Sender:TRtcConnection);
  begin
  // left empty
  end;

procedure TRtcDataServerLink.Call_WSDataReceived(Sender: TRtcConnection);
  begin
  end;

procedure TRtcDataServerLink.Call_DataOut(Sender:TRtcConnection);
  begin
  // left empty
  end;

procedure TRtcDataServerLink.Call_WSDataOut(Sender: TRtcConnection);
  begin
  end;

procedure TRtcDataServerLink.Call_DataIn(Sender:TRtcConnection);
  begin
  // left empty
  end;

procedure TRtcDataServerLink.Call_WSDataIn(Sender: TRtcConnection);
  begin
  end;

procedure TRtcDataServerLink.Call_DataSent(Sender:TRtcConnection);
  begin
  // left empty
  end;

procedure TRtcDataServerLink.Call_WSDataSent(Sender: TRtcConnection);
  begin
  end;

procedure TRtcDataServerLink.Call_ReadyToSend(Sender:TRtcConnection);
  begin
  // left empty
  end;

procedure TRtcDataServerLink.Call_SessionClose(Sender: TRtcConnection);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  for idx:=0 to FDataServerLinks.Count-1 do
    begin
    FMyLink:=FDataServerLinks.GetLink(idx);
    FMyLink.Call_SessionClose(Sender);
    end;

  if assigned(FOnSessionClose) then
    FOnSessionClose(Sender);
  end;

procedure TRtcDataServerLink.Call_SessionOpen(Sender: TRtcConnection);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  if assigned(FOnSessionOpen) then
    FOnSessionOpen(Sender);

  for idx:=0 to FDataServerLinks.Count-1 do
    begin
    FMyLink:=FDataServerLinks.GetLink(idx);
    FMyLink.Call_SessionOpen(Sender);
    end;
  end;

{ TRtcDualDataServerLink }

constructor TRtcDualDataServerLink.Create(AOwner: TComponent);
  begin
  inherited;
  FServer2:=nil;
  FLink2:=nil;
  end;

destructor TRtcDualDataServerLink.Destroy;
  begin
  try
    Server2:=nil;
    Link2:=nil;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDualDataServerLink.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcDualDataServerLink.GetServer2: TRtcDataServer;
  begin
  Result:=FServer2;
  end;

function TRtcDualDataServerLink.GetLink2: TRtcDataServerLink;
  begin
  Result:=FLink2;
  end;

procedure TRtcDualDataServerLink.SetServer2(const Value: TRtcDataServer);
  begin
  if Value<>FServer2 then
    begin
    if assigned(FServer) and (FServer=Value) then
      Server:=nil;

    if assigned(FServer2) then
      begin
      FServer2.RemoveDataServerLink(self);
      FServer2:=nil;
      end;

    if assigned(Value) then
      begin
      Link2:=nil;
      FServer2:=Value;
      FServer2.AddDataServerLink(self);
      end;
    end;
  end;

procedure TRtcDualDataServerLink.SetLink2(const Value: TRtcDataServerLink);
  begin
  if Value<>FLink2 then
    begin
    if assigned(FLink) and (FLink=Value) then
      Link:=nil;

    if assigned(FLink2) then
      begin
      FLink2.RemoveChildLink(self);
      FLink2:=nil;
      end;

    if assigned(Value) then
      begin
      Server2:=nil; // can not be maped to Server2 and to Link2 at the same time.

      // Check for circular reference before assigning!
      if Value=self then
        raise Exception.Create('Circular DataServerLink reference!');
      if Value.CheckLink(self) then
        raise Exception.Create('Circular DataServerLink reference!');
      if CheckLink(Value) then
        raise Exception.Create('Circular DataServerLink reference!');

      FLink2:=Value;
      FLink2.AddChildLink(self);
      end;
    end;
  end;

procedure TRtcDualDataServerLink.SetServer(const Value: TRtcDataServer);
  begin
  if Value<>FServer then
    begin
    if assigned(FServer2) and (FServer2=Value) then
      Server2:=nil;

    inherited SetServer(Value);
    end;
  end;

procedure TRtcDualDataServerLink.SetLink(const Value: TRtcDataServerLink);
  begin
  if Value<>FLink then
    begin
    if assigned(FLink2) and (FLink2=Value) then
      Link2:=nil;

    inherited SetLink(Value);
    end;
  end;

procedure TRtcDualDataServerLink.SetOrder(const Value: integer);
  begin
  if Value<>FOrder then
    begin
    FOrder:=Value;
    // Update order
    if assigned(FLink2) then
      FLink2.AddChildLink(self)
    else if assigned(FServer2) then
      FServer2.AddDataServerLink(self);
    if assigned(FLink) then
      FLink.AddChildLink(self)
    else if assigned(FServer) then
      FServer.AddDataServerLink(self);
    end;
  end;

procedure TRtcDualDataServerLink.RemoveLink(Value: TRtcAbsDataServerLink);
  begin
  inherited RemoveLink(Value);
  if Value=FLink2 then Link2:=nil;
  end;

function TRtcDualDataServerLink.CheckLink(Value: TRtcAbsDataServerLink):boolean;
  begin
  Result:=inherited CheckLink(Value);
  if not Result then
    if Value=FLink2 then
      Result:=True
    else if assigned(FLink2) then
      Result:=FLink2.CheckLink(Value)
    else
      Result:=False;
  end;

procedure TRtcDualDataServerLink.RemoveServer(Value: TRtcDataServer);
  begin
  inherited RemoveServer(Value);
  if Value=FServer2 then Server2:=nil;
  end;

procedure TRtcDualDataServerLink.Call_RequestAccepted(Sender: TRtcConnection);
  begin
  inherited Call_RequestAccepted(Sender);

  if assigned(Link2) then
    Link2.Call_RequestAccepted(Sender);
  end;

procedure TRtcDualDataServerLink.Call_PeekReceived(Sender: TRtcConnection);
  begin
  if assigned(Link2) then
    Link2.Call_PeekReceived(Sender);

  inherited Call_PeekReceived(Sender);
  end;

procedure TRtcDualDataServerLink.Call_ResponseDone(Sender: TRtcConnection);
  begin
  inherited Call_ResponseDone(Sender);

  if assigned(Link2) then
    Link2.Call_ResponseDone(Sender);
  end;

procedure TRtcDualDataServerLink.Call_Disconnect(Sender:TRtcConnection);
  begin
  inherited Call_Disconnect(Sender);

  if assigned(Link2) then
    Link2.Call_Disconnect(Sender);
  end;

procedure TRtcDualDataServerLink.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FServer2 then
      SetServer2(nil)
    else if AComponent=FLink2 then
      SetLink2(nil);
  end;

procedure TRtcDualDataServerLink.Call_WSConnect(Sender: TRtcConnection);
  begin
  inherited Call_WSConnect(Sender);

  if assigned(Link2) then
    Link2.Call_WSConnect(Sender);
  end;

procedure TRtcDualDataServerLink.Call_WSDisconnect(Sender: TRtcConnection);
  begin
  inherited Call_WSDisconnect(Sender);

  if assigned(Link2) then
    Link2.Call_WSDisconnect(Sender);
  end;

{ TRtcDataProvider }

constructor TRtcDataProvider.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FOnDisconnect:=nil;
  FOnCheckRequest:=nil;
  FOnDataReceived:=nil;
  FOnReadyToSend:=nil;
  FOnDataOut:=nil;
  FOnDataIn:=nil;
  FOnDataSent:=nil;

  FOnWSConnect:=nil;
  FOnWSDataIn:=nil;
  FOnWSDataOut:=nil;
  FOnWSDataReceived:=nil;
  FOnWSDataSent:=nil;
  FOnWSDisconnect:=nil;

  FWS:=TRtcWSManager.Create;
  end;

destructor TRtcDataProvider.Destroy;
  begin
  try
    FOnDisconnect:=nil;
    FOnCheckRequest:=nil;
    FOnDataReceived:=nil;
    FOnReadyToSend:=nil;
    FOnDataOut:=nil;
    FOnDataIn:=nil;
    FOnDataSent:=nil;

    FOnWSConnect:=nil;
    FOnWSDataIn:=nil;
    FOnWSDataOut:=nil;
    FOnWSDataReceived:=nil;
    FOnWSDataSent:=nil;
    FOnWSDisconnect:=nil;

    RtcFreeAndNil(FWS);

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataProvider.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataProvider.Call_CheckRequest(Sender:TRtcConnection);
  begin
  if assigned(OnCheckRequest) then
    begin
    TRtcDataServer(Sender).SetActiveLink(self);
    OnCheckRequest(Sender);
    end;
  end;

procedure TRtcDataProvider.Call_ListenStart(Sender:TRtcConnection);
  begin
  if assigned(OnListenStart) then
    OnListenStart(Sender);
  end;

procedure TRtcDataProvider.Call_ListenStop(Sender:TRtcConnection);
  begin
  if assigned(OnListenStop) then
    OnListenStop(Sender);
  end;

procedure TRtcDataProvider.Call_RequestAccepted(Sender: TRtcConnection);
  begin
  if assigned(Link) then
    Link.Call_RequestAccepted(Sender);
  end;

procedure TRtcDataProvider.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(OnSessionOpen) then
    OnSessionOpen(Sender);
  end;

procedure TRtcDataProvider.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(OnSessionClose) then
    OnSessionClose(Sender);
  end;

procedure TRtcDataProvider.Call_ResponseDone(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseDone) then
    FOnResponseDone(Sender);
  if assigned(Link) then
    Link.Call_ResponseDone(Sender);
  end;

procedure TRtcDataProvider.Call_WSConnect(Sender: TRtcConnection);
  begin
  if assigned(FWS) then
    FWS.wsAdd(Sender);
  if assigned(FOnWSConnect) then
    FOnWSConnect(Sender);
  if assigned(Link) then
    Link.Call_WSConnect(Sender);
  end;

procedure TRtcDataProvider.Call_DataReceived(Sender:TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_PeekReceived(Sender);

  if assigned(OnDataReceived) then
    OnDataReceived(Sender);
  end;

procedure TRtcDataProvider.Call_WSDataReceived(Sender:TRtcConnection);
  begin
  if assigned(OnWSDataReceived) then
    OnWSDataReceived(Sender);
  end;

procedure TRtcDataProvider.Call_DataOut(Sender:TRtcConnection);
  begin
  if assigned(OnDataOut) then
    OnDataOut(Sender);
  end;

procedure TRtcDataProvider.Call_WSDataOut(Sender:TRtcConnection);
  begin
  if assigned(OnWSDataOut) then
    OnWSDataOut(Sender);
  end;

procedure TRtcDataProvider.Call_DataIn(Sender:TRtcConnection);
  begin
  if assigned(OnDataIn) then
    OnDataIn(Sender);
  end;

procedure TRtcDataProvider.Call_WSDataIn(Sender:TRtcConnection);
  begin
  if assigned(OnWSDataIn) then
    OnWSDataIn(Sender);
  end;

procedure TRtcDataProvider.Call_DataSent(Sender:TRtcConnection);
  begin
  if assigned(OnDataSent) then
    OnDataSent(Sender);
  end;

procedure TRtcDataProvider.Call_WSDataSent(Sender:TRtcConnection);
  begin
  if assigned(OnWSDataSent) then
    OnWSDataSent(Sender);
  end;

procedure TRtcDataProvider.Call_ReadyToSend(Sender:TRtcConnection);
  begin
  if assigned(OnReadyToSend) then
    OnReadyToSend(Sender);
  end;

procedure TRtcDataProvider.Call_Disconnect(Sender:TRtcConnection);
  begin
  if assigned(OnDisconnect) then
    OnDisconnect(Sender);
  if assigned(Link) then
    Link.Call_Disconnect(Sender);
  end;

procedure TRtcDataProvider.Call_WSDisconnect(Sender:TRtcConnection);
  begin
  if assigned(OnWSDisconnect) then
    OnWSDisconnect(Sender);
  if assigned(Link) then
    Link.Call_WSDisconnect(Sender);
  if assigned(FWS) then
    FWS.wsRemove(Sender);
  end;

function TRtcDataProvider.wsGetID(const Sender:TRtcConnection):RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) or (Sender=nil) then
    Result:=0
  else
    Result:=FWS.wsGetID(Sender);
  end;

function TRtcDataProvider.wsCount: integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsCount;
  end;

function TRtcDataProvider.wsFirst: RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsFirst;
  end;

function TRtcDataProvider.wsLast: RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsLast;
  end;

function TRtcDataProvider.wsNext(id: RtcIntPtr): RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsNext(id);
  end;

function TRtcDataProvider.wsPrior(id: RtcIntPtr): RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsPrior(id);
  end;

function TRtcDataProvider.wSend(id: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (id=0) or (iFrame=nil) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,iFrame,iName);
  end;

function TRtcDataProvider.wSendMore(id: RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcByteArray; vFinal:boolean=False): integer;
  begin
  if (id=0) or (iFrame=nil) or (self=nil) or (FWS=nil) or ((vFinal=False) and (length(vPayload)=0)) then
    Result:=0
  else
    Result:=FWS.wSendMore(id,iFrame,vPayload,vFinal);
  end;

function TRtcDataProvider.wSendMore(id: RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcString; vFinal:boolean=False): integer;
  begin
  if (id=0) or (iFrame=nil) or (self=nil) or (FWS=nil) or ((vFinal=False) and (length(vPayload)=0)) then
    Result:=0
  else
    Result:=FWS.wSendMore(id,iFrame,vPayload,vFinal);
  end;

function TRtcDataProvider.wSendIfIdle(id: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,iFrame,iName);
  end;

function TRtcDataProvider.wSendToAll(iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (self=nil) or (FWS=nil) then
    begin
    Result:=0;
    RtcFreeAndNil(iFrame);
    end
  else
    Result:=FWS.wSendToAll(iFrame,iName);
  end;

function TRtcDataProvider.wSendToIdle(iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (self=nil) or (FWS=nil) then
    begin
    Result:=0;
    RtcFreeAndNil(iFrame);
    end
  else
    Result:=FWS.wSendToIdle(iFrame,iName);
  end;

function TRtcDataProvider.wSendToOthers(xid: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (self=nil) or (FWS=nil) then
    begin
    Result:=0;
    RtcFreeAndNil(iFrame);
    end
  else
    Result:=FWS.wSendToOthers(xid,iFrame,iName);
  end;

function TRtcDataProvider.wsDisconnect(id: RtcIntPtr): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsDisconnect(id);
  end;

function TRtcDataProvider.wsDisconnectAll: integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsDisconnectAll;
  end;

function TRtcDataProvider.wsClearSendingQueue(id: RtcIntPtr): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsClearSendingQueue(id);
  end;

function TRtcDataProvider.wsClearAllSendingQueues: integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsClearAllSendingQueues;
  end;

function TRtcDataProvider.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,vOpcode,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,vOpcode,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendToAll(vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToAll(vOpcode,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendToAll(vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToAll(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendToAll(vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToAll(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendToIdle(vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToIdle(vOpcode,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendToIdle(vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToIdle(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendToIdle(vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToIdle(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToOthers(xid,vOpcode,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToOthers(xid,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataProvider.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToOthers(xid,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

{ TRtcServerSession }

class function TRtcServerSession.Find(const _ID,_PeerAddr,_ForwardedFor: RtcString): TRtcServerSession;
  begin
  Result:=FindSession(_ID,_PeerAddr,_ForwardedFor);
  end;

class function TRtcServerSession.Have(const _ID,_PeerAddr,_ForwardedFor: RtcString): boolean;
  begin
  Result:=HaveSession(_ID,_PeerAddr,_ForwardedFor);
  end;

class function TRtcServerSession.CloseID(const _ID,_PeerAddr,_ForwardedFor: RtcString; _Event:TRtcSimpleEvent): boolean;
  begin
  Result:=CloseSessionID(_ID,_PeerAddr,_ForwardedFor,_Event);
  end;

{ Define Session ID and Lock the Session }
procedure TRtcServerSession.LockSessionID;
{$IFNDEF UseGUIDs}
  var
    id1,id2:RtcString;
    o:TObject;
{$ENDIF}
  begin
  SessCS.Acquire;
  try
    if assigned(SessLockList) then
      begin
      {$IFDEF UseGUIDs}
      FID:=NewSessionID;
      // Add Session to Locked list
      SessLockList.insert(FID, self);
      {$ELSE}
      // Find max. session ID in use
      if SessLockList.search_max(o)<>'' then
        id1:=TRtcServerSession(o).ID
      else
        id1:='';
      if SessUnlockList.search_max(o)<>'' then
        id2:=TRtcServerSession(o).ID
      else
        id2:='';
      if id2>id1 then id1:=id2;

      // Define new session ID
      id2:=NewSessionID(id1, PeerAddr);
      FID:=id2;

      // Set ID to desired length, so it will be sorted as a numeric value.
      while length(id2)<RTC_SESSIONID_LENGTH do // 10 for SessionID + 8 for PeerAddr + 10 random
        id2:=' '+id2;

      // Add Session to Locked list
      SessLockList.insert(id2, self);
      {$ENDIF}
      end;
  finally
    SessCS.Release;
    end;
  end;

class function TRtcServerSession.Open(_LockType:TRtcSessionLockType; const _PeerAddr,_ForwardedFor: RtcString): TRtcServerSession;
  begin
  // Create a new Session object
  Result:=TRtcServerSession.Create;
  Result.FLockType:=_LockType;
  Result.FPeerAddr:=_PeerAddr;
  Result.FForwardedFor:=_ForwardedFor;
  Result.FCreated:=Now;
  Result.FLastUsed:=Result.FCreated;

  // Define Session ID and Lock the Session object
  Result.LockSessionID;
  if Result.ID='' then
    begin
    RtcFreeAndNil(Result);
    raise Exception.Create('Unable to initialize session');
    end;
  end;

function TRtcServerSession.DenyAccess(const _PeerAddr, _ForwardedFor: RtcString): boolean;
  begin
  if LockType=sesNoLock then
    Result:=False
  else if (LockType=sesFwdLock) and
          (ForwardedFor='') and // "X-FORWARDED-FOR" was NOT set by creator - check for IP
          (PeerAddr<>_PeerAddr) then
    Result:=True
  else if (LockType in [sesFwdLock, sesIPFwdLock]) and
          (ForwardedFor<>'') and // "X-FORWARDED-FOR" was set by creator - use value set
          (ForwardedFor<>_ForwardedFor) then
    Result:=True
  else if (LockType in [sesIPLock, sesIPFwdLock]) and
          (PeerAddr<>_PeerAddr) then
    Result:=True
  else
    Result:=False;
  end;

{ UnLock Session and remove all sessions that have expired:
   - remove from Locked list
   - add to UnLocked and Expiring list }
procedure TRtcServerSession.UnLockSession(_Event:TRtcSimpleEvent);
  var
    id2,ex:RtcString;
    o:TObject;
  begin
  if not assigned(SessCS) then Exit;

  SessCS.Acquire;
  try
    if assigned(SessUnLockList) then
      begin
      // Check if there are Sessions that have expired but are still in our session list
      ex:=SessExpList.search_min(o);
      while (ex<>'') and (TRtcServerSession(o).ExpireTime<=Now) do
        begin
        // Remove from Expiring list
        SessExpList.remove(ex);

        // Set ID to desired length, so it will be sorted as a numeric value.
        id2:=TRtcServerSession(o).ID;
        {$IFNDEF UseGUIDs}
        while length(id2)<RTC_SESSIONID_LENGTH do
          id2:=' '+id2;
        {$ENDIF}

        // Remove from UnLocked list
        SessUnLockList.remove(id2);

        if assigned(_Event) then
          _Event(o);
        // Free the Session object
        RtcFreeAndNil(o);

        ex:=SessExpList.search_min(o);
        end;

      id2:=FID;
      {$IFNDEF UseGUIDs}
      // Set ID to desired length, so it will be sorted as a numeric value.
      while length(id2)<RTC_SESSIONID_LENGTH do
        id2:=' '+id2;
      {$ENDIF}

      // Remove from Locked list
      SessLockList.remove(id2);

      // Free if expired, add to expiring list if not.
      if ExpireTime<=Now then
        begin
        if assigned(_Event) then
          _Event(self);
        {$IFDEF NEXTGEN} DisposeOf; {$ELSE} Free; {$ENDIF}
        end
      else
        begin
        // Add to UnLocked list
        SessUnLockList.insert(id2, self);
        // Add to Expiring list
        SessExpList.insert(SessionTimeToStr(ExpireTime)+ID, self);
        end;
      end;
  finally
    SessCS.Release;
    end;
  end;

procedure TRtcServerSession.UnLock(_Event:TRtcSimpleEvent);
  begin
  FLastUsed:=Now;

  // First update ExpireTime with KeepAlive time
  if FKeepAlive=0 then
    FExpireTime:=Now + RTC_SESSION_TIMEOUT/24/60/60
  else
    FExpireTime:=Now + FKeepAlive/24/60/60;

  // Check if session has to expire (FinalExpire value defined)
  if FFinalExpire<>0 then
    if FExpireTime>FFinalExpire then
      FExpireTime:=FFinalExpire;

  UnLockSession(_Event);
  end;

procedure TRtcServerSession.Close;
  begin
  FFinalExpire:=Now;
  end;

function TRtcServerSession.isClosing: boolean;
  begin
  if FFinalExpire=0 then
    Result:=False
  else
    Result:=FFinalExpire<=Now;
  end;

type
  TRtcDataSrvUnit=class
  public
    constructor Create;
    destructor Destroy; override;
    end;

var
  MyUnit:TRtcDataSrvUnit;

{ TRtcDataSrvUnit }

constructor TRtcDataSrvUnit.Create;
  begin
  inherited;
  InitSessions;
  end;

destructor TRtcDataSrvUnit.Destroy;
  begin
  try
    DoneSessions;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataSrvUnit.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcServerComponent }

function TRtcServerComponent.GetOrder: integer;
  begin
  Result:=0;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcDataSrv Initializing ...','DEBUG');{$ENDIF}
MyUnit:=TRtcDataSrvUnit.Create;
{$IFDEF RTC_DEBUG} Log('rtcDataSrv Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcDataSrv Finalizing ...','DEBUG');{$ENDIF}
CloseThreadPool;
RtcFreeAndNil(MyUnit);
{$IFDEF RTC_DEBUG} Log('rtcDataSrv Finalized.','DEBUG');{$ENDIF}
end.
