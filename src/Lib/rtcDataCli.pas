{
  @html(<b>)
  Data Client Components
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit implements a set of Client-side Data components. @Link(TRtcDataClient)
  is the base class for all Request/Response based client connections. It implements the
  mechanisms used by components like @Link(TRtcDataClientLink) @Link(TRtcDataRequest) and
  @Link(TRtcClientModule) and so prepares the road for different connection providers,
  which will all be able to use all higher-level RTC components. You won't be creating
  TRtcDataClient components, since it only implements the mechanism for working with
  other Data-related RTC components, but it doesn't implement a communication protocol.
  First RTC component which implements the connection is @Link(TRtcHttpClient). You should
  use that component if you want to communicate with a Http Server over TCP/IP.
}
unit rtcDataCli;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSrcList,
  rtcThrPool,
  rtcSystem,
  rtcLog,

  rtcInfo,
  rtcConnProv,
  rtcConn;

type
  TRtcWaitForCompletionResult=TRtcWaitForConditionResult;

  TRtcWaitForDisconnectResult=TRtcWaitForConditionResult;

  ERtcWaitForCompletion=class(Exception);

  { @abstract(Basic Request Info)

    Objects of this type (inherited) are created for every request passed to DataClient
    and destroyed by DataClient after the Request has been processed or rejected.

    @exclude }
  TRtcClientRequestInfo=class(TObject)
  private
    FWasInjected: boolean;
    FWasAborted: boolean;

  protected
    // @exclude
    FRequest:TRtcClientRequest;

  public // HTTP

    // @exclude
    constructor Create; overload;
    // @exclude
    destructor Destroy; override;

    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_ResponseData(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_ResponseAbort(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_ResponseReject(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection); virtual;

    // @exclude
    procedure Call_DataOut(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_DataIn(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_DataSent(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_ReadyToSend(Sender:TRtcConnection); virtual;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); virtual;

    // @exclude
    procedure Call_DataReceived(Sender:TRtcConnection); virtual;

    // @exclude
    procedure Call_ConnectLost(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_RepostCheck(Sender:TRtcConnection); virtual;

    // @exclude
    function Get_Request:TRtcClientRequest; virtual;

    // @exclude
    property WasInjected:boolean read FWasInjected write FWasInjected default false;
    // @exclude
    property WasAborted:boolean read FWasAborted write FWasAborted default false;

  public // WEB SOCKETS

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
    procedure Call_WSConnectLost(Sender:TRtcConnection); virtual;
    end;

  { @abstract(Client Session) }
  TRtcClientSession=class(TRtcSession)
  protected
    // @exclude
    FCon:TRtcConnection;

  public
    // @exclude
    procedure Init; override;

    // Open a new session with this ID
    procedure Open(const _ID:RtcString); override;

    // Close this Session.
    procedure Close; override;
    end;

  // @abstract(All Components used by the DataClient are derived from this class)
  TRtcClientComponent = class(TRtcComponent);

  TRtcAbsDataClientLink = class; // forward

  // @exclude
  TRtcDataClientLinkList = class(TObject)
  private
    FList:TRtcObjectList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const Value:TRtcAbsDataClientLink);
    procedure Remove(const Value:TRtcAbsDataClientLink);

    procedure RemoveAll;

    function Count:integer;
    function Get(index:integer):TRtcAbsDataClientLink;
    end;

  { @Abstract(Data Client Connection component)

    Most of the events published by TRtcDataClient will NOT be defined directly on the
    @Link(TRtcDataClient), but instead of that will be passed on to posted DataRequest components,
    so that each DataRequest can process its request (send request out and accept response).
    Posted requests will be processed one-by-one, even if they are posted all at the same time.

    Properties to check first:
    @html(<br>)
    @Link(TRtcConnection.ServerAddr) - Address to connect to
    @html(<br>)
    @Link(TRtcConnection.ServerPort) - Port to connect to
    @html(<br><br>)

    Methods to check first:
    @html(<br>)
    @Link(TRtcClient.Connect) - Connect to Server
    @html(<br>)
    @Link(TRtcDataClient.Request), @Link(TRtcDataClient.WriteHeader), @Link(TRtcConnection.Write) - Write result to server
    @html(<br>)
    @Link(TRtcDataClient.Response), @Link(TRtcConnection.Read) - Read Server's response
    @html(<br>)
    @Link(TRtcConnection.Disconnect) - Disconnect from Server
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - Connected to Server
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent to server (buffer now empty)
    @html(<br>)
    @Link(TRtcDataClient.OnResponseAbort) - Connection Lost while receiving response, but Repost was not triggered.
    @html(<br>)
    @Link(TRtcConnection.OnDisconnect) - Disconnected from Server
    @html(<br><br>)

    Check @Link(TRtcClient) and @Link(TRtcConnection) for more info.
    }
  TRtcDataClient = class(TRtcClient)
  private
    FOnPeekResponse:TRtcNotifyEvent;

    FOnRepostCheck:TRtcNotifyEvent;
    FOnBeginRequest:TRtcNotifyEvent;
    FOnResponseDone:TRtcNotifyEvent;
    FOnResponseData:TRtcNotifyEvent;
    FOnResponseAbort:TRtcNotifyEvent;
    FOnResponseReject:TRtcNotifyEvent;

    FOnSessionOpen:TRtcNotifyEvent;
    FOnSessionClose:TRtcNotifyEvent;

    FMayInsertRequest:boolean;
    FRequestInserted:boolean;

    FRequestSkipped:integer;

    FAutoConnect:boolean;
    FToDisconnect:boolean;

    FCS:TRtcCritSec;
    FSession:TRtcClientSession;

    FRequestList:TXObjList;
    FActiveRequest:TRtcClientRequestInfo;

    FIdleCS:TRtcEvent;
    FIdle:boolean;

    FDataClientLinks:TRtcDataClientLinkList;

    FMyRequest, FRequest:TRtcClientRequest;
    FMyResponse, FResponse:TRtcClientResponse;

    FRequestFixup: TRtcClientRequestFixup;

    procedure SetRequestFixup(const Value: TRtcClientRequestFixup);

    procedure CheckRequestSkipped;

    function GetAutoConnect: boolean;
    procedure SetAutoConnect(const Value: boolean);

  protected
    // @exclude
    procedure ReconnectAfterSkip;

    // @exclude
    function isEventDriven:boolean; virtual;

    // @exclude
    function isConnectionRequired:boolean; override;

    // @exclude
    procedure AddDataClientLink(const Value:TRtcAbsDataClientLink);
    // @exclude
    procedure RemoveDataClientLink(const Value:TRtcAbsDataClientLink);
    // @exclude
    procedure RemoveAllDataClientLinks;

    // @exclude
    procedure SetRequest(const Value: TRtcRequest); override;
    // @exclude
    procedure SetResponse(const Value: TRtcResponse); override;

    // @exclude
    procedure SetCliRequest(const Value: TRtcClientRequest); virtual;
    // @exclude
    procedure SetCliResponse(const Value: TRtcClientResponse); virtual;

    // @exclude
    function GetRequest:TRtcRequest; override;
    // @exclude
    function GetResponse:TRtcResponse; override;

    // @exclude
    procedure CallConnect; override;
    // @exclude
    procedure CallAfterManualRead; override;
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
    procedure CallConnectLost; override;
    // @exclude
    procedure CallConnectFail; override;
    // @exclude
    procedure CallConnectError(E: Exception); override;

    // @exclude
    procedure AddRequest(var Req:TRtcClientRequestInfo);
    // @exclude
    procedure RemoveAllRequests;
    // @exclude
    procedure RemoveRequest;
    // @exclude
    procedure StartRequest;

    // @exclude
    procedure PrepareNextRequest;

    // @exclude
    function CheckRequestWork:boolean;

    { @exclude
      Post a 'StartRequest' Job }
    procedure PostStartRequest;

    { @exclude }
    function DoWaitForDisconnect(UserInteractionAllowed:boolean=False; AllowMessageProcessing:boolean=True; _Timeout:cardinal=0):TRtcWaitForDisconnectResult; virtual;

    { Check if the request queue is empty.
      @exclude }
    function NoMoreRequests:boolean; virtual;

    { @exclude }
    function GetSession:TRtcSession; override;

    // @exclude
    function GetRequestInserted:boolean; override;

  public
    // @exclude
    constructor Create(AOwner: TComponent); override;
    // @exclude
    destructor Destroy; override;

    { Check if connection is Idle (no requests waiting or disconnected)
      @exclude }
    function isIdle:boolean; virtual;

    { @exclude
      Internal function: Post a new Request Object.
      When posting from inside a RTC event or a remote function,
      "FromInsideEvent" parameter has to be TRUE to avoid memory consumption. }
    procedure PostRequest(var myReq; FromInsideEvent:boolean=False); override;

    { @exclude
      Internal function: Insert a Request before active request.
      This procedure may ONLY be called from BeginRequest event
      to place another request before the active request. }
    procedure InsertRequest(const Req:TObject); override;

    { Close the connection (gracefuly, asynchronous).
      All events that need to be triggered will be triggered. }
    procedure Disconnect; override;

    { Call Disconnect and wait for the connection to get closed.
      This is the preffered way for closing the connection if you
      plan to destroy the component or close the Application.
      Use a _Timeout (seconds) parameter to define how long you 
      are willing to wait for the connection to close (0=no timeout).
      Returns "wait_OK" in case of success (connection closed). }
    function DisconnectNow(SkipPendingRequests:boolean=False; _Timeout:cardinal=0; UserInteractionAllowed:boolean=False; AllowMessageProcessing:boolean=True):TRtcWaitForDisconnectResult;

    // @exclude
    procedure CallBeginRequest; virtual;
    // @exclude
    procedure CallResponseDone; virtual;
    // @exclude
    procedure CallResponseData; virtual;
    // @exclude
    procedure CallResponseAbort; virtual;
    // @exclude
    procedure CallResponseReject; virtual;
    // @exclude
    procedure CallSessionOpen; virtual;
    // @exclude
    procedure CallSessionClose; virtual;
    // @exclude
    procedure CallRepostCheck; virtual;

    // Skip all requests (RequestAborted events will NOT BE triggered!!!)
    procedure SkipRequests; override;
    // Cancel all requests (will fire all RequestAborted events)
    procedure CancelRequests; override;

    // Check request count (number of requests waiting to be processed)
    function RequestCount:integer; override;

    { Wait for all posted requests and function calls to complete,
      be aborted, be calceled, or for the connection to close. @html(<br>)
      Using a timeout (seconds) you can specify how long you want to wait (0=forever).
      Returns wait_OK if there are no more requests waiting.
      Returns wait_Timeout if not finished, but Timed out.
      Returns wait_Msg if not finished, but unknown message received.
      Returns wait_Quit if not finished, but application terminating.
      Returns wait_Error if not finished because of a connection problem.
      NOTE: Returns immediately with wait_OK for all upgraded Web Socket connections! }
    function DoWaitForCompletion(UserInteractionAllowed:boolean=False; _Timeout:cardinal=0; AllowMessageProcessing:boolean=True):TRtcWaitForCompletionResult; virtual;

    { Wait for all posted requests and function calls to complete,
      be aborted, be calceled, or for the connection to close. @html(<br>)
      Using a timeout (seconds) you can specify how long you want to wait.
      (0=forever). Returns TRUE only if there are no more requests waiting.
      Returns FALSE if timed-out or terminating or connection can't be open
      or connection closed on purpose.
      NOTE: Returns immediately with TRUE for all upgraded Web Socket connections! }
    function WaitForCompletion(UserInteractionAllowed:boolean=False; _Timeout:cardinal=0; AllowMessageProcessing:boolean=True):boolean; virtual;

    { Call WaitForCompletion, but instead of returning TRUE or FALSE depending on the result,
      raise an Exception if Result=FALSE with a message describing the type of the error.
      NOTE: Returns immediately for all upgraded Web Socket connections! }
    procedure WaitForCompletionEx(UserInteractionAllowed:boolean=False; _Timeout:cardinal=0; AllowMessageProcessing:boolean=True); virtual;

    { This connection's Session info.
      If you will be using multiple client connections and
      need to store session information, you have to start a
      separate session for every client connection.
      @html(<br><br>)
      There is a difference between this DataClient's Session object and
      the DataServer's Session object. DataClient's Session object stays
      permamently defined all the time, from the point of object creation to its destruction.
      Different from the Server connection, }
    property Session:TRtcClientSession read FSession;

    { Access to current request information.
      Use Request property to prepare the request header.
      Here you can set all header variables and parameters.
      If there is no content body to send out (only header), you will at
      least have to call 'WriteHeader' or 'Write' without parameters once. }
    property Request:TRtcClientRequest read FRequest write SetCliRequest;

    { Access to current response information.
      Use Response property to read the response information received.
      Here is all the info that was available in response header.
      To read response's body, use the Read function. }
    property Response:TRtcClientResponse read FResponse write SetCliResponse;

  published
    { If you are NOT using this component for writing a HTTP Proxy, in which case you
      would need the original and unmodified URI to be sent out to the Server, you can use
      the RequestFixup properties to specify which automatic operations should be done
      on the Request automatically before sending each request to the Server. }
    property FixupRequest:TRtcClientRequestFixup read FRequestFixup write SetRequestFixup;

    { You have two ways of working with connections. One is to open a connection
      on application start by calling "Connect" and using "ReconnectOn" to keep the
      connection open until your application closes, the other is to use implicit
      connects when a connection is required (when you post a request of any kind). @html(<br>)
      You should set AutoConnect to TRUE if you do not want your connection to remain
      open all the time, but also do not want to call "Connect" when you need a connection.
      For connection to remain open as long as there are requests waiting to be processed,
      you will also need to set the appropriate ReconnectOn parameters. @html(<br><br>)

      When AutoConnect is TRUE, connection will be automaticaly opened when you
      post a request (no need to call Connect) and "ReconnectOn" parameters will be
      used to reconnect only if there are requests waiting in the queue. By using Timeout
      parameters, your connection will close after specified timeout and stay closed
      until needed again, when a new connection will be automaticaly initiated. @html(<br><br>)

      When AutoConnect is FALSE, connection has to be opened explicitly by calling "Connect"
      and "ReconnectOn" parameters will be used to keep the connection open, even if there
      are no requests waiting in the queue. }
    property AutoConnect:boolean read GetAutoConnect write SetAutoConnect default False;

    { Called before each request start.
      You can use this event to prepare the request. }
    property OnBeginRequest:TRtcNotifyEvent read FOnBeginRequest write FOnBeginRequest;

    { Called every time response content comes from Server through this component, BEFORE the
      "OnDataReceived" event is triggered on the component assigned to handle the Response.
      You can use this event for centralized logging and monitoring of all response content
      received through this component, but do NOT use Read or ReadEx methods here, beause
      they will clear receiving buffers, making it inaccessible by all other components!
      To check response content data in this event, use the "PeekEx" method instead. }
    property OnPeekResponse:TRtcNotifyEvent read FOnPeekResponse write FOnPeekResponse;

    { Called after the last DataReceived event for the response, when a response is done.
      You can use this event to react to the final response. }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { Called every time a data package comes from Server, immediatelly after OnDataReceived. }
    property OnResponseData:TRtcNotifyEvent read FOnResponseData write FOnResponseData;
    { Called after OnConnectLost,OnConnectFail and OnConnectError if Request was not reposted. }
    property OnRepostCheck:TRtcNotifyEvent read FOnRepostCheck write FOnRepostCheck;
    { Called after OnRepostCheck if Request was not reposted. }
    property OnResponseAbort:TRtcNotifyEvent read FOnResponseAbort write FOnResponseAbort;
    { Called if response has been rejected by calling Response.Reject. }
    property OnResponseReject:TRtcNotifyEvent read FOnResponseReject write FOnResponseReject;

    { Called after a new session has been opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { Called before an existing session is about to get closed. }
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

  TRtcDataClientLink=class; // forward

  { @abstract(DataClient Link wrapper) }
  TRtcAbsDataClientLink=class(TRtcClientComponent)
  private
    FClient: TRtcDataClient;
    FLink: TRtcDataClientLink;
    FAutoSync: boolean;

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    function CheckLink(const Value:TRtcAbsDataClientLink):boolean; virtual;
    // @exclude
    procedure RemoveLink(const Value:TRtcAbsDataClientLink); virtual;
    // @exclude
    procedure RemoveClient(const Value:TRtcDataClient); virtual;

    // @exclude
    function GetClient: TRtcDataClient; virtual;
    // @exclude
    procedure SetClient(const Value: TRtcDataClient); virtual;

    // @exclude
    function GetLink: TRtcDataClientLink; virtual;
    // @exclude
    procedure SetLink(const Value: TRtcDataClientLink); virtual;

    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_ResponseData(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_ResponseAbort(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_ResponseReject(Sender:TRtcConnection); virtual;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); virtual;

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
    procedure Call_ConnectLost(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_RepostCheck(Sender:TRtcConnection); virtual;

  public // HTTP

    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    { Check if the connection component is MultiThreaded }
    function isMultiThreaded:boolean;

    { Check if connection is Idle (no requests waiting or disconnected)
      NOTE: This method is ONLY for HTTP connections. If a connection
      was upgraded to a Web Socket, this method always returns TRUE!
      @exclude }
    function isIdle:boolean; virtual;

    { Post a new Request Object.
      NOTE: This method is ONLY for HTTP connections. If a connection
      was upgraded to a Web Socket, this method should NOT be used!
      @exclude }
    procedure PostRequest(var myReq; FromInsideEvent:boolean=False); virtual;

    { Insert a Request before active request.
      NOTE: This procedure may ONLY be called from BeginRequest event
      to place another request before the active request and should
      NOT be used if a connection was upgraded to a Web Socket!
      @exclude }
    procedure InsertRequest(const Req:TRtcClientRequestInfo); virtual;

    { Skip all requests posted to the Client Connection used.
      RequestAborted events will NOT BE triggered!!!
      NOTE: If called on an upgraded Web Socket connection,
      this will close the Web Socket and this connection! }
    procedure SkipRequests; virtual;

    { Cancel all requests posted to the Client Connection used and fire RequestAborted events.
      NOTE: If called on an upgraded Web Socket connection,
      this will close the Web Socket and this connection! }
    procedure CancelRequests; virtual;

    { Check request count at Client Connection used
      (number of requests waiting to be processed).
      NOTE: This method returns -1 for upgraded Web Socket connections! }
    function RequestCount:integer; virtual;

    { Wait for all posted requests and function calls to complete,
      be aborted, be calceled, or for the connection to close. @html(<br>)
      Using a timeout (seconds) you can specify how long you want to wait (0=forever).
      Returns wait_OK if there are no more requests waiting.
      Returns wait_Timeout if not finished, but Timed out.
      Returns wait_Msg if not finished, but unknown message received.
      Returns wait_Quit if not finished, but application terminating.
      Returns wait_Error if not finished because of a connection problem.
      NOTE: Returns immediately with wait_OK for all upgraded Web Socket connections! }
    function DoWaitForCompletion(UserInteractionAllowed:boolean=False; _Timeout:cardinal=0; AllowMessageProcessing:boolean=True):TRtcWaitForCompletionResult; virtual;

    { Wait for all posted requests and function calls to complete,
      be aborted, be calceled, or for the connection to close. @html(<br>)
      Using a timeout (seconds) you can specify how long you want to wait.
      (0=forever). Returns TRUE only if there are no more requests waiting.
      Returns FALSE if timed-out or terminating or connection can't be open
      or connection closed on purpose.
      NOTE: Returns immediately with TRUE for all upgraded Web Socket connections! }
    function WaitForCompletion(UserInteractionAllowed:boolean=False; _Timeout:cardinal=0; AllowMessageProcessing:boolean=True):boolean; virtual;

    { Call WaitForCompletion, but instead of returning TRUE or FALSE depending on the result,
      raise an Exception if Result=FALSE with a message describing the type of the error.
      NOTE: Returns immediately for all upgraded Web Socket connections! }
    procedure WaitForCompletionEx(UserInteractionAllowed:boolean=False; _Timeout:cardinal=0; AllowMessageProcessing:boolean=True); virtual;

  published
    { If you are using a Client connection in Multi-Threaded mode, but want all your events
      to be executed from the Main Thread (for example: if they are accessing the GUI),
      instead of using "if Sender.Sync(myEvent,...) then Exit;" at the top of every event,
      you can simply set the "AutoSyncEvent" property to TRUE. Doing this ensures that every
      event triggered by this component will be executed from the Main Thread (synchronized). }
    property AutoSyncEvents:boolean read FAutoSync write FAutoSync default false;
    { You can link your components (one or more) to a DataClientLink component
      by assigning your @Link(TRtcDataClientLink) component to chind component's Link property.
      Doing this, you only have to set the Client property for the master
      DataClientLink component and don't need to do it for every single
      DataRequest component. }
    property Link:TRtcDataClientLink read GetLink write SetLink;
    { You can also link your components (one or more) directly to your
      DataClient connection component by assigning your
      @Link(TRtcDataClient) connection component to this child component's Client property.
      This is useful if you don't want to use a DataClientLink. }
    property Client:TRtcDataClient read GetClient write SetClient;

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
    procedure Call_WSConnectLost(Sender:TRtcConnection); virtual;

    end;

  { @abstract(DataClient Link, used to group Data Requests)

    You can use TRtcDataClientLink components to group several related
    @Link(TRtcDataRequest) components. Simply set this component as the
    Link property for all your RtcDataSource components, so that
    you don't have to set the Client property for every single
    TRtcDataRequest component separately. This is useful especially
    when the component is used in a datamodule or a form without
    DataClient and you need to link all the components to
    a DataClient which is on another datamodule or form.
    @html(<br><br>)

    Check @Link(TRtcAbsDataClientLink) for more info. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcDataClientLink=class(TRtcAbsDataClientLink)
  private
    FOnPeekResponse:TRtcNotifyEvent;

    FOnBeginRequest:TRtcNotifyEvent;
    FOnResponseDone:TRtcNotifyEvent;
    FOnResponseData:TRtcNotifyEvent;
    FOnResponseAbort:TRtcNotifyEvent;
    FOnResponseReject:TRtcNotifyEvent;
    FOnSessionOpen:TRtcNotifyEvent;
    FOnSessionClose:TRtcNotifyEvent;
    FOnRepostCheck:TRtcNotifyEvent;

  protected // HTTP

    // @exclude
    FDataClientLinks:TRtcDataClientLinkList;

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
    procedure Call_ConnectLost(Sender:TRtcConnection); override;

    // @exclude
    procedure AddChildLink(Value:TRtcAbsDataClientLink);
    // @exclude
    procedure RemoveChildLink(Value:TRtcAbsDataClientLink);
    // @exclude
    procedure RemoveAllChildLinks;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    // @exclude
    procedure Call_PeekReceived(Sender:TRtcConnection); virtual;

    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseData(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseAbort(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseReject(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_RepostCheck(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

  published
    { You can use this event to monitor response content received through this component,
      but do NOT use Read or ReadEx methods here, because they will clear receiving
      buffers. ONLY use the PeekEx method to access the content body from this event. }
    property OnPeekResponse:TRtcNotifyEvent read FOnPeekResponse write FOnPeekResponse;
    
    { Use this event to initialize child requests linked to this DataLink }
    property OnBeginRequest:TRtcNotifyEvent read FOnBeginRequest write FOnBeginRequest;
    { Use this event to add additional control after each OnDataReceived package. }
    property OnResponseData:TRtcNotifyEvent read FOnResponseData write FOnResponseData;
    { Use this event to finalize child requests linked to this DataLink }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { Called after OnConnectLost, OnConnectFail and OnConnectError if request is not finished and not marked for reposting }
    property OnRepostCheck:TRtcNotifyEvent read FOnRepostCheck write FOnRepostCheck;
    { Called after OnRepostCheck if request is not finished and is not marked for reposting. }
    property OnResponseAbort:TRtcNotifyEvent read FOnResponseAbort write FOnResponseAbort;
    { Called after Response has been rejected by calling Response.Reject }
    property OnResponseReject:TRtcNotifyEvent read FOnResponseReject write FOnResponseReject;
    { Called after a new Session has been opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { Called before an existing session is about to be closed. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;

  protected // WEB SOCKETS

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
    procedure Call_WSConnectLost(Sender:TRtcConnection); override;

    end;

  { @abstract(Dual DataClient Link, used to create a connection pool)

    You can use TRtcDualDataClientLink components to create a small pool
    of client connections. When posting requests through this component,
    you will never really know which connection the request will be posted
    from, since the component itself will determine that, depending on 
    the number of currently active requests on each connection component.
    @html(<br><br>)

    Check @Link(TRtcDataClientLink) for more info. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcDualDataClientLink=class(TRtcDataClientLink)
  private
    FClient2: TRtcDataClient;
    FLink2: TRtcDataClientLink;

  protected // HTTP
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    function CheckLink(const Value:TRtcAbsDataClientLink):boolean; override;
    // @exclude
    procedure RemoveLink(const Value:TRtcAbsDataClientLink); override;
    // @exclude
    procedure RemoveClient(const Value:TRtcDataClient); override;

    // @exclude
    function GetClient2: TRtcDataClient; virtual;
    // @exclude
    procedure SetClient2(const Value: TRtcDataClient); virtual;
    // @exclude
    procedure SetClient(const Value: TRtcDataClient); override;

    // @exclude
    function GetLink2: TRtcDataClientLink; virtual;
    // @exclude
    procedure SetLink2(const Value: TRtcDataClientLink); virtual;
    // @exclude
    procedure SetLink(const Value: TRtcDataClientLink); override;

  public // HTTP

    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    { Check if connection is Idle (no requests waiting or disconnected)
      @exclude }
    function isIdle:boolean; override;

    // @exclude
    procedure Call_ConnectLost(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_DataReceived(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_DataOut(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_DataIn(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_DataSent(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ReadyToSend(Sender: TRtcConnection); override;

    // @exclude
    procedure Call_PeekReceived(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_BeginRequest(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ResponseData(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_RepostCheck(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ResponseAbort(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ResponseReject(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_SessionOpen(Sender: TRtcConnection); override;

    // Post a new Request Object
    procedure PostRequest(var myReq; FromInsideEvent:boolean=False); override;
    { Insert a Request before active request.
      This procedure may ONLY be called from BeginRequest event
      to place another request before the active request. }
    procedure InsertRequest(const Req:TRtcClientRequestInfo); override;

    // Skip all requests posted to the Client Connection used (RequestAborted events will NOT BE triggered!!!)
    procedure SkipRequests; override;
    // Cancel all requests posted to the Client Connection used and fire RequestAborted events
    procedure CancelRequests; override;

    // Check request count at Client Connection used (number of requests waiting to be processed)
    // NOTE: Returns -1 for upgraded Web Socket connections
    function RequestCount:integer; override;

    { Wait for all posted requests and function calls to complete,
      be aborted, be calceled, or for the connection to close. @html(<br>)
      Using a timeout (seconds) you can specify how long you want to wait (0=forever).
      Returns wait_OK if there are no more requests waiting.
      Returns wait_Timeout if not finished, but Timed out.
      Returns wait_Msg if not finished, but unknown message received.
      Returns wait_Quit if not finished, but application terminating.
      Returns wait_Error if not finished because of a connection problem. 
      NOTE: Returns immediately with wait_OK for all upgraded Web Socket connections! }
    function DoWaitForCompletion(UserInteractionAllowed:boolean=False; _Timeout:cardinal=0; AllowMessageProcessing:boolean=True):TRtcWaitForCompletionResult; override;

    { Wait for all posted requests and function calls to complete,
      be aborted, be calceled, or for the connection to close. @html(<br>)
      Using a timeout (seconds) you can specify how long you want to wait.
      (0=forever). Returns TRUE only if there are no more requests waiting.
      Returns FALSE if timed-out or terminating or connection can't be open
      or connection closed on purpose.
      NOTE: Returns immediately with TRUE for all upgraded Web Socket connections! }
    function WaitForCompletion(UserInteractionAllowed:boolean=False; _Timeout:cardinal=0; AllowMessageProcessing:boolean=True):boolean; override;

    { Call WaitForCompletion, but instead of returning TRUE or FALSE depending on the result,
      raise an Exception if Result=FALSE with a message describing the type of the error.
      NOTE: Returns immediately for all upgraded Web Socket connections! }
    procedure WaitForCompletionEx(UserInteractionAllowed:boolean=False; _Timeout:cardinal=0; AllowMessageProcessing:boolean=True); override;

  published
    { You can link your components (one or more) to a DataClientLink component
      by assigning your @Link(TRtcDataClientLink) component to child component's Link property.
      Doing this, you only have to set the Client property for the master
      DataClientLink component and don't need to do it for every single
      DataRequest component. }
    property Link2:TRtcDataClientLink read GetLink2 write SetLink2;
    { You can also link your components (one or more) directly to your
      DataClient connection component by assigning your
      @Link(TRtcDataClient) connection component to this child component's Client property.
      This is useful if you don't want to use a DataClientLink. }
    property Client2:TRtcDataClient read GetClient2 write SetClient2;

  protected // WEB SOCKETS

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
    procedure Call_WSConnectLost(Sender:TRtcConnection); override;

    end;

  { @abstract(DataRequest Info Object)

    This object is created and filled by TRtcDataRequest,
    when you Post or Insert a DataRequest.

    @exclude }
  TRtcDataRequestInfo=class(TRtcClientRequestInfo)
  protected
    // @exclude
    FEvents:TRtcAbsDataClientLink;

  public
    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseData(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseAbort(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseReject(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

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
    procedure Call_ConnectLost(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_RepostCheck(Sender:TRtcConnection); override;

  public
    // Standard constructor
    constructor Create; virtual;

    // @exclude
    destructor Destroy; override;

    { Request Info, will be destroyed after the Request has been processed,
      has to be assigned a valid TRtcClientRequest object before posting. }
    property Request:TRtcClientRequest read FRequest write FRequest;

    { This DataClient Link component will be used to call events. }
    property Events:TRtcAbsDataClientLink read FEvents write FEvents;

  public // WEB SOCKETS

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
    procedure Call_WSConnectLost(Sender:TRtcConnection); override;

    end;

  // @exclude
  TRtcDataRequestData=class
  public
    FRequest:TRtcClientRequest;

    constructor Create; virtual;
    destructor Destroy; override;
    end;

  { @abstract(Data Request, used to Prepare+Post or Insert+Prepare Requests for DataClient)

    This is the component you have to use to work with DataClient.
    Put TRtcDataRequest on a form or a datamodule and define events
    at design-time, which will be used for default request processing.
    @html(<br><br>)
    Then, at runtime, there are 2 ways you can use this DataRequest:
    @html(<br>)
    1.) From the Main thread, you can prepare the request using the
       @Link(TRtcDataRequest.Request) property and call @link(TRtcDataRequest.Post)
       to post the prepared request to its associated DataClient, or ...
    @html(<br>)
    2.) From inside a BeginRequest event handler (which is called by the DataClient
       connection component), you can insert this DataRequest, so that your active
       DataRequest is put on the 2nd position and this request is put on the first
       place, by calling the @Link(TRtcDataRequest.Insert) method. After calling
       @link(TRtcDataRequest.Insert), immediatelly a new @Link(TRtcDataClient.Request)
       is created for this inserted DataRequest, so you can initialize the request
       before exiting your event handler.
    @html(<br><br>)

    Check @Link(TRtcAbsDataClientLink) for more info. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcDataRequest=class(TRtcAbsDataClientLink)
  private
    FCS:TRtcCritSec;
    FMyData:TObjList;
    FMainThrData:TRtcDataRequestData;
    FHyperThreading: boolean;

    function CheckMyData:TRtcDataRequestData;
    function GetMyData:TRtcDataRequestData;
    procedure ClearMyData;

    function GetRequest: TRtcClientRequest;

  protected // HTTP
    // @exclude
    FAutoRepost:integer;

    // @exclude
    FOnBeginRequest: TRtcNotifyEvent;
    // @exclude
    FOnResponseData: TRtcNotifyEvent;
    // @exclude
    FOnResponseDone: TRtcNotifyEvent;
    // @exclude
    FOnResponseAbort: TRtcNotifyEvent;
    // @exclude
    FOnResponseReject: TRtcNotifyEvent;

    // @exclude
    FOnRepostCheck: TRtcNotifyEvent;
    // @exclude
    FOnReadyToSend: TRtcNotifyEvent;

    // @exclude
    FOnSessionOpen: TRtcNotifyEvent;
    // @exclude
    FOnSessionClose: TRtcNotifyEvent;

    // @exclude
    FOnConnectLost: TRtcNotifyEvent;
    // @exclude
    FOnDataReceived: TRtcNotifyEvent;
    // @exclude
    FOnDataOut: TRtcNotifyEvent;
    // @exclude
    FOnDataIn: TRtcNotifyEvent;
    // @exclude
    FOnDataSent: TRtcNotifyEvent;

    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseData(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseAbort(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseReject(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

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
    procedure Call_ConnectLost(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_RepostCheck(Sender:TRtcConnection); override;

  public // HTTP

    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    { Post this DataRequest to assigned DataClient.
      @html(<br><br>)

      After a call to Post, an object will be created to hold the
      prepared Request info, after which the Request property will be cleared,
      so that you can prepare and post new requests immediatelly,
      without waiting for the last request to complete.
      @html(<br><br>)

      When posting a new Request from inside an Event of a running request/response loop,
      "FromInsideEvent" parameter has to be TRUE to avoid memory consumption and
      always pass the Sender:TRtcConnection parameter through.

      Events assigned to this TRtcDataRequest will not be removed nor cleared,
      so you can define them at design-time and not worry about them at runtime. }
    procedure Post(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil); virtual;

    { This "PostMethod" call replaces these 3 lines of code: @html(<br><br>)
        if Request.Method='' then Request.Method:='GET'; // 1. @html(<br>)
        Write(''); // 2. @html(<br>)
        Post(FromInsideEvent,Sender); // 3. }
    procedure PostMethod(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil); overload;

    { This "PostMethod" call replaces these 4 lines of code: @html(<br><br>)
        if MyMethod<>'' then Request.Method:=MyMethod // 1. @html(<br>)
        else if Request.Method='' then Request.Method:='GET'; // 2. @html(<br><br>)
        Write(''); // 3. @html(<br>)
        Post(FromInsideEvent,Sender); // 4. }
    procedure PostMethod(const MyMethod:RtcString; FromInsideEvent:boolean=False; Sender:TRtcConnection=nil); overload;

    { Insert this request before the active request.
      @html(<br><br>)

      DataRequest objects which are used for automatic initialisation
      can be inserted before the active request, but ONLY from inside
      the active request's BeginRequest event handler. After calling
      this Insert procedure, a new Request is created for the inserted
      DataRequest and can/should be modified/prepared before exiting the
      BeginRequest event. After BeginRequest event exists, the inserted
      DataRequest's BeginRequest events will be called, as if this event
      was posted before the one that inserted it.

      To make sure the request will be inserted to same connection,
      always pass the Sender:TRtcConnection parameter through. }
    procedure Insert(Sender:TRtcConnection=nil); virtual;

    { If your request doesn't have any content body ("GET" request?),
      or if the entire request content body fits into Client's Memory
      without causing problems, you can use the Write or WriteEx method
      while PREPARING the Request for posting - BEFORE you POST it.
      If the OnBeginRequest event is implemented, anything you prepare
      this way will be written after the OnBeginRequest event returns.
      Using this method also allows you to skip implementing the
      OnBeginRequest event entirely, allowing you to prepare the
      entire Request including any content body before using POST. }
    procedure WriteEx(const s:RtcByteArray); virtual;

    { If your request doesn't have any content body ("GET" request?),
      or if the entire request content body fits into Client's Memory
      without causing problems, you can use the Write or WriteEx method
      while PREPARING the Request for posting - BEFORE you POST it.
      If the OnBeginRequest event is implemented, anything you prepare
      this way will be written after the OnBeginRequest event returns.
      Using this method also allows you to skip implementing the
      OnBeginRequest event entirely, allowing you to prepare the
      entire Request including any content body before using POST. }
    procedure Write(const s:RtcString=''); virtual;

    { ONLY use this Request property to prepare a request BEFORE posting.
      @html(<br>)
      DO NOT directly use this property when processing the request.
      After a request has been posted, it is moved to the DataClient,
      so you can access it (from events) using Sender's Request property. }
    property Request:TRtcClientRequest read GetRequest;

  published // HTTP

    { If you want to enable the possibility to use this Data Request to send requests
      from multiple threads AT THE SAME TIME, where this component will acs as if
      it were X components, one for each thread, simply set HyperThreading to TRUE. @html(<br><br>)

      This is useful if you need to send requests to another Server from
      within your Server running in multi-threaded mode and want to use only one set of
      rtcHttpClient/rtcDataRequest components for all clients connected to your Server.
      Even in HyperThreading mode, only properties and methods needed to prepare and post
      the request (Request and Post) will use a separate copy for each thread, while all
      other properties and methods exist only once for all threads, so don't try to modify
      them while your application is actively using the component in multi-threaded mode. @html(<br><br>)

      Leave HyperThreading as FALSE to use this component "the stadard way" (for example,
      when you're writing a client application where requests are posted from the main thread
      or if you are creating a separate component for every thread that needs it). }
    property HyperThreading:boolean read FHyperThreading write FHyperThreading default False;

    { Set this property to a value other than 0 (zero) if you want the DataRequest to
      auto-repost any request up to "AutoRepost" times, in case the connection gets lost
      while sending data to server or receiving data from server.
      AutoRepost = -1 means that request should be reposted infinitely. }
    property AutoRepost:integer read FAutoRepost write FAutoRepost default 0;

    { This event will be called when DataClient component is
      ready to start sending the request out.
      @html(<br><br>)
      If the request is already in memory (not a large file on disk),
      send it out using Sender's WriteHeader and/or Write methods.
      If the request is too large to be sent out at once (maybe a big file
      on disk), you should at least send the Request Header out in this event.
      @html(<br><br>)
      If you DO NOT call Write and/or WriteHeader from this event,
      then this Request will be skipped by the DataClient. }
    property OnBeginRequest:TRtcNotifyEvent read FOnBeginRequest write FOnBeginRequest;
    { This event will be called after each DataReceived event for this request
      and can be used to access info prepared by the OnDataReceived event. }
    property OnResponseData:TRtcNotifyEvent read FOnResponseData write FOnResponseData;
    { This event will be called after the last DataReceived event for this request,
      after the request has been sent out and a complete response was received (Response.Done).
      If a connection was upgraded to a Web Socket by this Data Request component,
      "OnWSConnect" event will be triggered immediately after this event,
      to let you know that a new Web Socket connection is ready for use. }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { This event will be called after ConnectLost, ConnectFail and ConnectError events if request was not marked for reposting.

      If you want to re-post the request, you should call Request.Repost from
      this event, or the Request will NOT be reposted and OnResponseAbort
      event will be trigered. You can also use the AutoRepost property to tell the
      component to repost requests automaticaly (for a specified number of times or unlimited). }
    property OnRepostCheck:TRtcNotifyEvent read FOnRepostCheck write FOnRepostCheck;
    { This event will be called after the OnRepostCheck event if request was not marked for reposting. }
    property OnResponseAbort:TRtcNotifyEvent read FOnResponseAbort write FOnResponseAbort;

    { This event will be called after the response has been rejected by calling Response.Reject }
    property OnResponseReject:TRtcNotifyEvent read FOnResponseReject write FOnResponseReject;

    { This event will be called after a new Session has been opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { This event will be called before an existing Session is about to close. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;

    { This event will be mapped as TRtcDataClient.OnDataReceived event
      to the assigned DataClient component and called for all DataReceived
      events until the request is processed in full, skipped or rejected. }
    property OnDataReceived:TRtcNotifyEvent read FOnDataReceived write FOnDataReceived;

    { This event will be mapped as @Link(TRtcConnection.OnDataOut) event
      to the assigned DataClient component and called for all DataOut
      events until the request is processed in full, skipped or rejected. }
    property OnDataOut:TRtcNotifyEvent read FOnDataOut write FOnDataOut;

    { This event will be mapped as @Link(TRtcConnection.OnDataIn) event
      to the assigned DataClient component and called for all DataIn
      events until the request is processed in full, skipped or rejected. }
    property OnDataIn:TRtcNotifyEvent read FOnDataIn write FOnDataIn;

    { This event will be mapped as @Link(TRtcConnection.OnDataSent) event
      to the assigned DataClient component and called for all DataSent
      events until the request is processed in full, skipped or rejected. }
    property OnDataSent:TRtcNotifyEvent read FOnDataSent write FOnDataSent;

    { This event will be mapped as @Link(TRtcConnection.OnReadyToSend) event
      to the assigned DataClient component and called for all ReadyToSend
      events until the request is processed in full, skipped or rejected. }
    property OnReadyToSend:TRtcNotifyEvent read FOnReadyToSend write FOnReadyToSend;

    { This event will be mapped as @Link(TRtcClient.OnConnectLost) event
      to the assigned DataClient component and called if your connection gets
      closed while you are processing your request (sending or receiving data).
      @html(<br><br>)

      If you want to re-send the request, you can call Request.Repost from this event,
      or use the OnRepostCheck or OnResponseAborted events to do so.

      NOTE: After a connection has been upgraded to a Web Socket, the
      "OnConnectLost" event will NOT be called anymore! When a Web Socket
      connection closes, the "OnWSDisconnect" even is called instead. }
    property OnConnectLost:TRtcNotifyEvent read FOnConnectLost write FOnConnectLost;

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
    FWS: TRtcWSManager;

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
    procedure Call_WSConnectLost(Sender:TRtcConnection); override;

  public // WEB SOCKETS

    { WEB SOCKET MANAGER:
      Web Socket Manager created and used by this TRtcDataRequest
      component to automatically add and remove all Web Sockets
      upgraded from events implemented on this TRtcDataRequest. }
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

    { This is the first event to be called after a HTTP connection
      is upgraded to a Web Socket connection by this component.
      From here on, the "Sender:TRtcConnection" component can be
      used as a Web Socket - to send and receive Web Socket Frames. }
    property OnWSConnect:TRtcNotifyEvent read FOnWSConnect write FOnWSConnect;

    { This event will be called every time Web Socket data is received
      for a Web Socket connection upgraded by this component.
      NOTE: After a connection is upgraded to a Web Socket,
      this event will be called instead of "OnDataReceived". }
    property OnWSDataReceived:TRtcNotifyEvent read FOnWSDataReceived write FOnWsDataReceived;

    { This event will be called after data was written out to
      a Web Socket connection upgraded by this component.
      You can use this event to count how many bytes have been
      written out to this connection after a Web Socket upgrade.
      NOTE: After a connection is upgraded to a Web Socket,
      this event will be called instead of "OnDataOut". }
    property OnWSDataOut:TRtcNotifyEvent read FOnWSDataOut write FOnWSDataOut;

    { This event will be called when data arrives through a
      Web Socket connection upgraded by this component.
      You can use this event to count how many bytes have arrived
      through this connection after a Web Socket upgrade.
      NOTE: After a connection is upgraded to a Web Socket,
      this event will be called instead of "OnDataIn". }
    property OnWSDataIn:TRtcNotifyEvent read FOnWSDataIn write FOnWsDataIn;

    { This event will be called when data has been sent through
      a Web Socket connection upgraded by this component.
      You can use this event to get notified when the last chunk
      of data has been sent, so you can continue sending more data
      for the current Web Socket Frame or start sending a new Frame.
      NOTE: After a connection is upgraded to a Web Socket,
      this event will be called instead of "OnDataSent". }
    property OnWSDataSent:TRtcNotifyEvent read FOnWSDataSent write FOnWSDataSent;

    { This event will be called when a Web Socket previously upgraded
      by this Data Request component gets disconnected (closed).
      If you have stored the "Sender:TRtcConnection" object anywhere,
      remove it in this event and make sure you will NO LONGER use it.
      "Sender:TRtcConnection" could get destroyed after this event!
      NOTE: After a connection is upgraded to a Web Socket,
      this event will be called instead of "OnConnectLost". }
    property OnWSDisconnect:TRtcNotifyEvent read FOnWSDisconnect write FOnWSDisconnect;
    end;

implementation

const
  DATAREQ_BODY='DATAREQ.BODY$';

type
  { @abstract(DataRequest Job)

    This Job is used by TRtcDataClient to post a signal to start a request

    @exclude }
  TRtcStartRequestJob=class(TRtcJob)
    Client:TRtcDataClient;

    destructor Destroy; override;

    function Run(Thr:TRtcThread):boolean; override;
    end;

{ TRtcDataClientLinkList }

constructor TRtcDataClientLinkList.Create;
  begin
  inherited;
  FList:=TRtcObjectList.Create;
  end;

destructor TRtcDataClientLinkList.Destroy;
  begin
  try
    RtcFreeAndNil(FList);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataClientLinkList.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataClientLinkList.Add(const Value: TRtcAbsDataClientLink);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx<0 then
    FList.Add(Value);
  end;

function TRtcDataClientLinkList.Count: integer;
  begin
  Result:=FList.Count;
  end;

function TRtcDataClientLinkList.Get(index:integer): TRtcAbsDataClientLink;
  begin
  if (index>=0) and (index<FList.Count) then
    Result:=TRtcAbsDataClientLink(FList.Items[index])
  else
    Result:=nil;
  end;

procedure TRtcDataClientLinkList.Remove(const Value: TRtcAbsDataClientLink);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx>=0 then
    FList.Delete(idx);
  end;

procedure TRtcDataClientLinkList.RemoveAll;
  begin
  if assigned(FList) then
    FList.Clear;
  end;

{ TRtcDataClient }

constructor TRtcDataClient.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FMyRequest:=TRtcClientRequest.Create;
  FMyResponse:=TRtcClientResponse.Create;

  FRequestFixup:=TRtcClientRequestFixup.Create;

  FRequest:=FMyRequest;
  FResponse:=FMyResponse;

  FCS:=TRtcCritSec.Create;
  FSession:=TRtcClientSession.Create;
  FSession.FCon:=self;
  FMayInsertRequest:=False;
  FRequestInserted:=False;
  FToDisconnect:=False;

  FDataClientLinks:=TRtcDataClientLinkList.Create;

  FRequestList:=TXObjList.Create(16);
  FActiveRequest:=nil;
  FRequestSkipped:=0;

  // ManualReset, start with "Set" (no wait)
  FIdleCS:=TRtcEvent.Create(True,True);
  FIdle:=True;
  end;

destructor TRtcDataClient.Destroy;
  begin
  try
    RemoveAllDataClientLinks;
    RtcFreeAndNil(FDataClientLinks);

    if assigned(FRequestList) then
      begin
      RemoveAllRequests;
      RemoveRequest;
      RtcFreeAndNil(FRequestList);
      end;

    FActiveRequest:=nil;

    FIdleCS.SetEvent;
    FIdle:=True;

    FRequest:=nil; RtcFreeAndNil(FMyRequest);
    FResponse:=nil; RtcFreeAndNil(FMyResponse);

    RtcFreeAndNil(FRequestFixup);

    RtcFreeAndNil(FSession);
    RtcFreeAndNil(FCS);
    RtcFreeAndNil(FIdleCS);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataClient.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcDataClient.GetRequestInserted:boolean;
  begin
  Result:=FRequestInserted;
  end;

function TRtcDataClient.GetSession:TRtcSession;
  begin
  Result:=FSession;
  end;

procedure TRtcDataClient.SetRequestFixup(const Value: TRtcClientRequestFixup);
  begin
  if assigned(Value) then
    FRequestFixup.Assign(Value);
  end;

procedure TRtcDataClient.CallConnect;
  begin
  ws_Clear;

  FToDisconnect:=False;
  inherited;
  if RequestCount>0 then
    if MultiThreaded then
      PostStartRequest
    else
      StartRequest;
  end;

procedure TRtcDataClient.CallReadyToSend;
  begin
  if assigned(FActiveRequest) then
    begin
    if isWebSocket then
      ws_SendNextQueuedFrame(True)
    else
      FActiveRequest.Call_ReadyToSend(self);
    Flush;
    CheckRequestWork;
    end
  else
    inherited;
  end;

procedure TRtcDataClient.CallDataSent;
  begin
  if assigned(FActiveRequest) then
    begin
    if isWebSocket then
      FActiveRequest.Call_WSDataSent(self)
    else
      FActiveRequest.Call_DataSent(self);
    Flush;
    CheckRequestWork;
    end
  else
    inherited;
  end;

procedure TRtcDataClient.CallDataOut;
  begin
  if assigned(FActiveRequest) then
    if isWebSocket then
      FActiveRequest.Call_WSDataOut(self)
    else
      FActiveRequest.Call_DataOut(self);

  inherited;

  Flush;
  end;

procedure TRtcDataClient.CallDataIn;
  begin
  if assigned(FActiveRequest) then
    if isWebSocket then
      FActiveRequest.Call_WSDataIn(self)
    else
      FActiveRequest.Call_DataIn(self);

  inherited;
  end;

procedure TRtcDataClient.CallDataReceived;
  begin
  if assigned(FActiveRequest) then
    begin
    if isWebSocket then
      begin
      FWebSocketData:=True;
      FActiveRequest.Call_WSDataReceived(self);
      end
    else
      begin
      if assigned(FOnPeekResponse) then
        FOnPeekResponse(self);
      FActiveRequest.Call_DataReceived(self);
      end;
    Flush;
    CallAfterManualRead;
    end
  else
    inherited;
  end;

procedure TRtcDataClient.CallAfterManualRead;
  begin
  if CheckRequestWork then Exit;

  if assigned(FActiveRequest) then
    if isWebSocket then
      ws_ClearTempFrames
    else
      begin
      FActiveRequest.Call_ResponseData(self);
      Flush;
      if CheckRequestWork then Exit;

      if assigned(Request.OnData) then
        begin
        Request.OnData(self);
        Flush;
        if CheckRequestWork then Exit;
        end;

      if Response.Done then
        begin
        if Con.isWebSocket then FWebSocket:=True;

        FActiveRequest.Call_ResponseDone(self);
        if CheckRequestWork then Exit;

        if assigned(Request.OnDone) then
          Request.OnDone(self);
        if CheckRequestWork then Exit;

        if isWebSocket then
          begin
          FActiveRequest.Call_WSConnect(self);
          Con.isWSManualRead:=Response.ManualRead;
          end
        else
          begin
          RemoveRequest;

          if RequestCount>0 then
            if MultiThreaded then
              PostStartRequest
            else
              StartRequest;
          end;
        end;
      end;
  end;

procedure TRtcDataClient.CallConnectLost;
  begin
  if not assigned(FActiveRequest) then
    inherited
  else
    begin
    if isWebSocket then
      begin
      FActiveRequest.Call_WSConnectLost(self);
      ws_Clear;
      end
    else
      FActiveRequest.Call_ConnectLost(self);

    if not (Response.Done or Request.Reposting) then
      begin
      CheckRequestSkipped;
      if not (Request.Skipped or Request.Cancelled) then
        begin
        FActiveRequest.Call_RepostCheck(self);
        if not Response.Done and not Request.Reposting then
          begin
          FActiveRequest.Call_ResponseAbort(self);
          if assigned(Request.OnAbort) then
            if not Request.Reposting then
              Request.OnAbort(self);
          end;
        end;
      end;

    inherited;

    if Request.Skipped then
      RemoveRequest
    else if Request.Cancelled then
      begin
      if assigned(FActiveRequest) then
        FActiveRequest.Call_ResponseAbort(self);
      if assigned(Request.OnAbort) then
        Request.OnAbort(self);
      RemoveRequest;
      end
    else if Response.Rejected then
      begin
      if assigned(FActiveRequest) then
        FActiveRequest.Call_ResponseReject(self);
      if assigned(Request.OnReject) then
        Request.OnReject(self);
      RemoveRequest;
      end
    else if Request.Reposting then
      begin
      Request.Reposting:=False;
      Response.Clear;
      if not (FConnecting or FReconnecting or ReconnectOn.ConnectLost) then
        begin
        if assigned(FActiveRequest) then
          FActiveRequest.Call_ResponseAbort(self);
        if assigned(Request.OnAbort) then
          Request.OnAbort(self);
        RemoveRequest;
        end;
      end
    else if Response.Done then
      RemoveRequest
    else
      begin
      RemoveRequest;
      // We will cancel all remaining requests
      CancelRequests;
      end;

    // This will remove all skipped requests
    PrepareNextRequest;
    end;
  end;

procedure TRtcDataClient.CallConnectFail;
  begin
  PrepareNextRequest;

  if assigned(FActiveRequest) then
    begin
    CheckRequestSkipped;
    if not (Request.Skipped or Request.Cancelled) then
      begin
      FActiveRequest.Call_RepostCheck(self);
      if not (Response.Done or Request.Reposting) then
        begin
        FActiveRequest.Call_ResponseAbort(self);
        if assigned(Request.OnAbort) then
          if not Request.Reposting then
            Request.OnAbort(self);
        end;
      end;

    inherited;

    if Request.Skipped then
      RemoveRequest
    else if Request.Cancelled then
      begin
      if assigned(FActiveRequest) then
        FActiveRequest.Call_ResponseAbort(self);
      if assigned(Request.OnAbort) then
        Request.OnAbort(self);
      RemoveRequest;
      end
    else if Response.Rejected then
      begin
      if assigned(FActiveRequest) then
        FActiveRequest.Call_ResponseReject(self);
      if assigned(Request.OnReject) then
        Request.OnReject(self);
      RemoveRequest;
      end
    else if Request.Reposting then
      begin
      Request.Reposting:=False;
      Response.Clear;
      if not (FConnecting or FReconnecting or ReconnectOn.ConnectFail) then
        begin
        if assigned(FActiveRequest) then
          FActiveRequest.Call_ResponseAbort(self);
        if assigned(Request.OnAbort) then
          Request.OnAbort(self);
        RemoveRequest;
        end;
      end
    else if Response.Done then
      RemoveRequest
    else
      begin
      RemoveRequest;
      // We will cancel all remaining requests
      CancelRequests;
      end;

    // This will remove all skipped requests
    PrepareNextRequest;
    end
  else
    inherited;
  end;

procedure TRtcDataClient.CallConnectError(E:Exception);
  begin
  PrepareNextRequest;

  if assigned(FActiveRequest) then
    begin
    CheckRequestSkipped;
    if not (Request.Skipped or Request.Cancelled) then
      begin
      FActiveRequest.Call_RepostCheck(self);
      if not (Response.Done or Request.Reposting) then
        begin
        FActiveRequest.Call_ResponseAbort(self);
        if assigned(Request.OnAbort) then
          if not Request.Reposting then
            Request.OnAbort(self);
        end;
      end;

    inherited;

    if Request.Skipped then
      RemoveRequest
    else if Request.Cancelled then
      begin
      if assigned(FActiveRequest) then
        FActiveRequest.Call_ResponseAbort(self);
      if assigned(Request.OnAbort) then
        Request.OnAbort(self);
      RemoveRequest;
      end
    else if Response.Rejected then
      begin
      if assigned(FActiveRequest) then
        FActiveRequest.Call_ResponseReject(self);
      if assigned(Request.OnReject) then
        Request.OnReject(self);
      RemoveRequest;
      end
    else if Request.Reposting then
      begin
      Request.Reposting:=False;
      Response.Clear;
      if not (FConnecting or FReconnecting or ReconnectOn.ConnectError) then
        begin
        if assigned(FActiveRequest) then
          FActiveRequest.Call_ResponseAbort(self);
        if assigned(Request.OnAbort) then
          Request.OnAbort(self);
        RemoveRequest;
        end;
      end
    else if Response.Done then
      RemoveRequest
    else
      begin
      RemoveRequest;
      // We will cancel all remaining requests
      CancelRequests;
      end;

    // This will remove all skipped requests
    PrepareNextRequest;
    end
  else
    inherited;
  end;

{ Called from TRtcDataRequest to add a new request,
  without interfering with other connection component operations. }
procedure TRtcDataClient.AddRequest(var Req: TRtcClientRequestInfo);
  begin
  FCS.Acquire;
  try
    FRequestList.AddLast(Req);
    Req:=nil;

    FIdleCS.ResetEvent;
    FIdle:=False;
  finally
    FCS.Release;
    end;
  end;

{ Called from TRtcDataRequest to add a new request,
  without interfering with other connection component operations. }
procedure TRtcDataClient.InsertRequest(const Req: TObject);
  begin
  FCS.Acquire;
  try
    if not FMayInsertRequest then
      raise Exception.Create('You are not allowed to insert requests.')
    else if FRequestSkipped>0 then
      raise Exception.Create('Operation Interrupted by a "RequestSkipped" call.')
    else if not (Req is TRtcClientRequestInfo) then
      raise ERtcConnection.Create('"InsertRequest" method requires "TRtcClientRequestInfo" as parameter.');

    if assigned(FActiveRequest) then
      if isWebSocket then
        raise ERtcConnection.Create('Connection upgraded to Web Socket!')
      else
        FRequestList.AddFirst(FActiveRequest);

    FRequestInserted:=True;

    FActiveRequest:=TRtcClientRequestInfo(Req);
    FActiveRequest.WasInjected:=True;
    Request:=FActiveRequest.Get_Request;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcDataClient.PrepareNextRequest;
  var
    endloop:boolean;
    areq:TObject;
  begin
  if not assigned(FActiveRequest) then
    begin
    FCS.Acquire;
    try
      repeat
        endloop:=True;
        if FRequestList.Count>0 then
          begin
          FRequestList.extractFirst(areq);
          FActiveRequest:= TRtcClientRequestInfo(areq);
          areq:=nil;
          Request:=FActiveRequest.Get_Request;
          end;
        if FRequestSkipped>0 then
          begin
          FCS.Release;
          try
            EnterEvent;
            try
              if assigned(FActiveRequest) then
                begin
                if not FActiveRequest.WasAborted then
                  begin
                  FActiveRequest.Call_ResponseAbort(self);
                  if assigned(Request.OnAbort) then
                    Request.OnAbort(self);
                  end;
                end
              else
                CallResponseAbort;
            finally
              LeaveEvent;
              end;
          finally
            FCS.Acquire;
            end;
          RemoveRequest;
          endloop:=False;
          end;
        until endloop;
    finally
      FCS.Release;
      end;
    end;
  end;

{ Start Request if no request active }
procedure TRtcDataClient.StartRequest;
  var
    endmainloop:boolean;
  begin
  if not isConnected then
    begin
    if AutoConnect and not (FConnecting or FReconnecting) then
      Connect;
    Exit;
    end;

  if FToDisconnect then Exit;

  repeat
    endmainloop:=True;

    PrepareNextRequest;

    EnterEvent;
    try

      if isConnected and assigned(FActiveRequest) and not Request.Active and not Request.Complete then
        begin
        if (Session.ID<>'') and
           (Session.PeerAddr<>PeerAddr) then
          begin
          Session.Init;
          end;

        CheckRequestSkipped;

        if not (Request.Skipped or Request.Cancelled) then
          begin
          FMayInsertRequest:=True;
          try
            // use a loop to simplify repeating the BeginRequest calls if a new request has been inserted
            repeat
              FRequestInserted:=False;

              if assigned(Request.OnBegin) then
                Request.OnBegin(self);

              if RequestInserted then
                Continue;

              FActiveRequest.Call_BeginRequest(self);

              until not RequestInserted;
          finally
            FMayInsertRequest:=False;
            end;

          Flush;
          end;

        if not isConnected then
          Break
        else if not Request.Active then
          begin
          CheckRequestSkipped;

          if Request.Cancelled then
            begin
            EnterEvent;
            try
              if assigned(FActiveRequest) then
                FActiveRequest.Call_ResponseAbort(self);
              if assigned(Request.OnAbort) then
                Request.OnAbort(self);
            finally
              LeaveEvent;
              end;
            end
          else if Response.Rejected then
            begin
            EnterEvent;
            try
              if assigned(FActiveRequest) then
                FActiveRequest.Call_ResponseReject(self);
              if assigned(Request.OnReject) then
                Request.OnReject(self);
            finally
              LeaveEvent;
              end;
            end;
          RemoveRequest;
          endmainloop:=False;
          end
        else if not Response.Done then
          begin
          { Request has started sending something out }

          CheckRequestSkipped;

          if Request.Skipped then
            begin
            RemoveRequest;
            ReconnectAfterSkip;
            end
          else if Request.Cancelled then
            begin
            EnterEvent;
            try
              if assigned(FActiveRequest) then
                FActiveRequest.Call_ResponseAbort(self);
              if assigned(Request.OnAbort) then
                Request.OnAbort(self);
            finally
              LeaveEvent;
              end;
            RemoveRequest;
            ReconnectAfterSkip;
            end
          else if Response.Rejected then
            begin
            EnterEvent;
            try
              if assigned(FActiveRequest) then
                FActiveRequest.Call_ResponseReject(self);
              if assigned(Request.OnReject) then
                Request.OnReject(self);
            finally
              LeaveEvent;
              end;
            RemoveRequest;
            ReconnectAfterSkip;
            end
          else if Request.Reposting then
            begin
            Request.Reposting:=False;
            Response.Clear;
            ReconnectAfterSkip;
            end;
          end
        else
          begin
          CheckRequestSkipped;

          if Request.Skipped then
            RemoveRequest
          else if Request.Cancelled then
            begin
            if assigned(FActiveRequest) then
              FActiveRequest.Call_ResponseAbort(self);
            if assigned(Request.OnAbort) then
              Request.OnAbort(self);
            RemoveRequest;
            end
          else if Response.Rejected then
            begin
            if assigned(FActiveRequest) then
              FActiveRequest.Call_ResponseReject(self);
            if assigned(Request.OnReject) then
              Request.OnReject(self);
            RemoveRequest;
            end
          else if Request.Reposting then
            begin
            Request.Reposting:=False;
            Response.Clear;
            end
          else
            endmainloop:=False;
          end;
        end;
    finally
      LeaveEvent;
      end;
    until endmainloop;
  TriggerReadyToRelease;
  end;

{ Remove all requests from Memory }
procedure TRtcDataClient.RemoveAllRequests;
  var
    areq:TObject;
  begin
  FCS.Acquire;
  try
    FRequestSkipped:=0;
    while FRequestList.Count>0 do
      begin
      FRequestList.extractFirst(areq);
      RtcFreeAndNil(areq);
      end;
    if assigned(FActiveRequest) then
      begin
      FActiveRequest.WasAborted:=True;
      FRequestSkipped:=1;
      end
    else
      begin
      FIdleCS.SetEvent;
      FIdle:=True;
      end;
  finally
    FCS.Release;
    end;
  end;

{ Remove the active Request from Memory }
procedure TRtcDataClient.RemoveRequest;
  var
    Remove_Next, WantClose:boolean;
    xreq:TRtcClientRequestInfo;
  begin
  if assigned(FActiveRequest) then
    begin
    if isWebSocket then 
      begin
      ws_Clear;
      Request.Close:=True;
      end;

    WantClose:=Request.Close;
    xreq:=nil;
    try
      FCS.Acquire;
      try
        if FRequestSkipped>0 then
          Dec(FRequestSkipped);

        Remove_Next:=FActiveRequest.WasInjected and FActiveRequest.WasAborted;

        xreq:=FActiveRequest;
        FActiveRequest:=nil;
        if FRequestList.Count=0 then
          begin
          FIdleCS.SetEvent;
          FIdle:=True;
          end;
      finally
        FCS.Release;
        end;
      Request:=nil;

      Request.Clear;
      Response.Clear;

      if Remove_Next then
        begin
        PrepareNextRequest;
        if assigned(FActiveRequest) then
          begin
          if not FActiveRequest.WasAborted then
            FActiveRequest.Call_ResponseAbort(self);
          RemoveRequest;
          end;
        end;

      if WantClose then
        InternalDisconnect;
    finally
      if assigned(xreq) then xreq.Free;
      end;
    end;
  end;

{ Check the Request after any standard working event. }
function TRtcDataClient.CheckRequestWork:boolean;
  begin
  CheckRequestSkipped;

  if Request.Skipped or 
     Request.Cancelled or
     Response.Rejected then
    begin
    Result:=True;
    if Request.Cancelled then
      begin
      EnterEvent;
      try
        if assigned(FActiveRequest) then
          FActiveRequest.Call_ResponseAbort(self);
        if assigned(Request.OnAbort) then
          Request.OnAbort(self);
      finally
        LeaveEvent;
        end;
      end
    else if Response.Rejected then
      begin
      EnterEvent;
      try
        if assigned(FActiveRequest) then
          FActiveRequest.Call_ResponseReject(self);
        if assigned(Request.OnReject) then
          Request.OnReject(self);
      finally
        LeaveEvent;
        end;
      end;

    if Request.Active then
      begin
      { Request has started sending something out }
      RemoveRequest;
      ReconnectAfterSkip;
      end
    else
      begin
      RemoveRequest;

      // Check if there are more requests waiting
      if RequestCount>0 then
        if MultiThreaded then
          PostStartRequest
        else
          StartRequest;
      end;
    end
  else if Request.Reposting then
    begin
    Result:=True;
    Request.Reposting:=False;
    if not Response.Done then
      begin
      Response.Clear;
      ReconnectAfterSkip;
      end
    else
      begin
      Response.Clear;
      if MultiThreaded then
        PostStartRequest
      else
        StartRequest;
      end;
    end
  else if not assigned(FActiveRequest) then
    Result:=True
  else
    Result:=False;
  end;

// Skip all requests (do not fire more RequestAborted events)
procedure TRtcDataClient.SkipRequests;
  begin
  RemoveAllRequests;
  end;

// Cancel all requests (fire all RequestAborted events)
procedure TRtcDataClient.CancelRequests;
  begin
  FCS.Acquire;
  try
    FRequestSkipped:=FRequestList.Count;
    if assigned(FActiveRequest) then
      Inc(FRequestSkipped)
    else
      begin
      FIdleCS.SetEvent;
      FIdle:=True;
      end;
  finally
    FCS.Release;
    end;
  end;

// Post a StartRequest job
procedure TRtcDataClient.PostStartRequest;
  var
    ReqJob:TRtcStartRequestJob;
  begin
  if AutoConnect then // StartRequest will be calling Connect.
    FWantConnect:=True;
  ReqJob:=TRtcStartRequestJob.Create;
  ReqJob.Client:=self;
  if not PostJob(ReqJob) then
    begin
    if AutoConnect then
      Connect;
    if not PostJob(ReqJob) then
      begin
      ReqJob.Client:=nil;
      if ReqJob.SingleUse then
        RtcFreeAndNil(ReqJob);
      end;
    end;
  end;

// Post a new request
procedure TRtcDataClient.PostRequest(var myReq; FromInsideEvent:boolean=False);
  var
    Req:TRtcClientRequestInfo absolute myReq;
  begin
  AddRequest(Req);
  if not FromInsideEvent then
    if MultiThreaded then
      PostStartRequest
    else
      StartRequest;
  end;

function TRtcDataClient.RequestCount: integer;
  begin
  FCS.Acquire;
  try
    if FWebSocket then
      Result:=-1
    else
      begin
      if assigned(FRequestList) then
        Result:=FRequestList.Count
      else
        Result:=0;

      if assigned(FActiveRequest) then
        Result:=Result+1-FRequestSkipped;
      end;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcDataClient.CallBeginRequest;
  begin
  if assigned(FOnBeginRequest) then
    FOnBeginRequest(self);
  end;

procedure TRtcDataClient.CallResponseDone;
  begin
  if assigned(FOnResponseDone) then
    FOnResponseDone(self);
  end;

procedure TRtcDataClient.CallRepostCheck;
  begin
  if assigned(FOnRepostCheck) then
    FOnRepostCheck(self);
  end;

procedure TRtcDataClient.CallResponseData;
  begin
  if assigned(FOnResponseData) then
    FOnResponseData(self);
  end;

procedure TRtcDataClient.CallResponseAbort;
  begin
  if assigned(FActiveRequest) then
    FActiveRequest.WasAborted:=True;
  if assigned(FOnResponseAbort) then
    FOnResponseAbort(self);
  end;

procedure TRtcDataClient.CallResponseReject;
  begin
  if assigned(FOnResponseReject) then
    FOnResponseReject(self);
  end;

procedure TRtcDataClient.CallSessionClose;
  begin
  if assigned(FOnSessionClose) then
    FOnSessionClose(self);
  end;

procedure TRtcDataClient.CallSessionOpen;
  begin
  if assigned(FOnSessionOpen) then
    FOnSessionOpen(self);
  end;

procedure TRtcDataClient.AddDataClientLink(const Value: TRtcAbsDataClientLink);
  begin
  FDataClientLinks.Add(Value);
  end;

procedure TRtcDataClient.RemoveAllDataClientLinks;
  var
    Link:TRtcAbsDataClientLink;
  begin
  if assigned(FDataClientLinks) then
    while FDataClientLinks.Count>0 do
      begin
      Link:=TRtcAbsDataClientLink(FDataClientLinks.Get(0));
      Link.RemoveClient(self);
      end;
  end;

procedure TRtcDataClient.RemoveDataClientLink(const Value: TRtcAbsDataClientLink);
  begin
  FDataClientLinks.Remove(Value);
  end;

procedure TRtcDataClient.CheckRequestSkipped;
  begin
  FCS.Acquire;
  try
    if FRequestSkipped>0 then
      if assigned(FActiveRequest) then
        if FActiveRequest.WasAborted then
          Request.Skip
        else
          Request.Cancel;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcDataClient.SetRequest(const Value: TRtcRequest);
  begin
  if Value=nil then
    FRequest:=FMyRequest
  else if Value is TRtcClientRequest then
    FRequest := TRtcClientRequest(Value)
  else
    raise ERtcConnection.Create('Invalid "Request" assignment!');
  end;

procedure TRtcDataClient.SetResponse(const Value: TRtcResponse);
  begin
  if Value=nil then
    FResponse:=FMyResponse
  else if Value is TRtcClientResponse then
    FResponse := TRtcClientResponse(Value)
  else
    raise ERtcConnection.Create('Invalid "Response" assignment!');
  end;

procedure TRtcDataClient.SetCliRequest(const Value: TRtcClientRequest);
  begin
  if Value=nil then
    FRequest:=FMyRequest
  else
    FRequest := Value;
  end;

procedure TRtcDataClient.SetCliResponse(const Value: TRtcClientResponse);
  begin
  if Value=nil then
    FResponse:=FMyResponse
  else
    FResponse := Value;
  end;

function TRtcDataClient.GetRequest:TRtcRequest;
  begin
  Result:=FRequest;
  end;

function TRtcDataClient.GetResponse:TRtcResponse;
  begin
  Result:=FResponse;
  end;

function TRtcDataClient.isIdle: boolean;
  begin
  FCS.Acquire;
  try
    if FIdle or isWebSocket then
      Result:=True
    else if isConnecting then
      Result:=False // connected or connecting or waiting for a reconnect
    else
      Result:=True; // no point in waiting. disconnected and not reconnecting
  finally
    FCS.Release;
    end;
  end;

function TRtcDataClient.NoMoreRequests:boolean;
  begin
  Result:=RequestCount<=0;
  end;

function TRtcDataClient.DoWaitForCompletion(UserInteractionAllowed: boolean; _Timeout: cardinal; AllowMessageProcessing:boolean): TRtcWaitForCompletionResult;
  begin
  Result:=RtcWaitFor( isIdle,NoMoreRequests,_Timeout,
                      UserInteractionAllowed,AllowMessageProcessing);
  end;

function TRtcDataClient.DoWaitForDisconnect(UserInteractionAllowed: boolean; AllowMessageProcessing:boolean; _Timeout:cardinal): TRtcWaitForDisconnectResult;
  begin
  Result:=RtcWaitFor( ConnectionClosed,ConnectionClosed,_Timeout,
                      UserInteractionAllowed,AllowMessageProcessing);
  end;

procedure TRtcDataClient.WaitForCompletionEx(UserInteractionAllowed: boolean; _Timeout: cardinal; AllowMessageProcessing:boolean);
  begin
  case DoWaitForCompletion(UserInteractionAllowed,_Timeout,AllowMessageProcessing) of
    wait_Error:   raise ERtcWaitForCompletion.Create('WaitForCompletionEx: Connection error.');
    wait_Timeout: raise ERtcWaitForCompletion.Create('WaitForCompletionEx: Request timed out.');
    wait_Quit:    raise ERtcWaitForCompletion.Create('WaitForCompletionEx: Application terminating.');
    wait_Msg:     raise ERtcWaitForCompletion.Create('WaitForCompletionEx: Loop terminated, unknown Message received.');
    end;
  end;

function TRtcDataClient.WaitForCompletion(UserInteractionAllowed: boolean; _Timeout: cardinal; AllowMessageProcessing:boolean):boolean;
  begin
  Result:=DoWaitForCompletion(UserInteractionAllowed,_Timeout,AllowMessageProcessing)=wait_OK;
  end;

function TRtcDataClient.isConnectionRequired: boolean;
  begin
  if FAmSilent then
    Result:=False
  else if not FAutoConnect then
    Result:=True
  else
    Result:=RequestCount>0;
  end;

function TRtcDataClient.GetAutoConnect: boolean;
  begin
  Result:=FAutoConnect;
  end;

procedure TRtcDataClient.SetAutoConnect(const Value: boolean);
  begin
  if Value<>FAutoConnect then
    begin
    FAutoConnect:=Value;
    if FAutoConnect and isConnectionRequired then
      Connect;
    end;
  end;

function TRtcDataClient.isEventDriven: boolean;
  begin
  Result:=True;
  end;

procedure TRtcDataClient.Disconnect;
  begin
  FToDisconnect:=True;
  inherited;
  end;

function TRtcDataClient.DisconnectNow(SkipPendingRequests:boolean=False; _Timeout:cardinal=0; UserInteractionAllowed:boolean=False; AllowMessageProcessing:boolean=True):TRtcWaitForDisconnectResult;
  var
    ac,ce,cf,cl:boolean;
  begin
  if self=nil then
    Result:=wait_OK
  else if (State>conPrepared) or isConnecting or isConnected then
    begin
    ac:=AutoConnect;
    ce:=ReconnectOn.ConnectError;
    cf:=ReconnectOn.ConnectFail;
    cl:=ReconnectOn.ConnectLost;
    try
      if not SkipPendingRequests then
        WaitForCompletion;

      ReconnectOn.ConnectError:=False;
      ReconnectOn.ConnectFail:=False;
      ReconnectOn.ConnectLost:=False;
      AutoConnect:=False;
      Disconnect;
      
      SkipRequests;

      Result:=DoWaitForDisconnect(UserInteractionAllowed,AllowMessageProcessing,_Timeout);
      if Result=wait_OK then Sleep(50);
    finally
      AutoConnect:=ac;
      ReconnectOn.ConnectError:=ce;
      ReconnectOn.ConnectFail:=cf;
      ReconnectOn.ConnectLost:=cl;
      end;
    end
  else 
	begin
    Result:=wait_OK;
    if SkipPendingRequests then SkipRequests;
    end;
  end;

procedure TRtcDataClient.ReconnectAfterSkip;
  begin
  // Need to Reset the connection
  if isConnected and not FToDisconnect then
    begin
    Disconnect;
    if AutoConnect then
      begin
      if isConnectionRequired then
        Reconnect;
      end
    else
      begin
      if ReconnectOn.ConnectLost or
         ReconnectOn.ConnectError or
         ReconnectOn.ConnectFail then
        Reconnect;
      end;
    end;
  end;

{ TRtcAbsDataClientLink }

constructor TRtcAbsDataClientLink.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FClient:=nil;
  FLink:=nil;
  end;

destructor TRtcAbsDataClientLink.Destroy;
  begin
  try
    Client:=nil; // remove from DataClient
    Link:=nil; // remove from parent DataClientLink
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcAbsDataClientLink.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcAbsDataClientLink.SetLink(const Value: TRtcDataClientLink);
  var
    MyLink:TRtcDataClientLink;
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
      Client:=nil; // can not be maped to DataClient and to DataClientLink at the same time.

      // Check for circular reference before assigning!
      MyLink:=Value;
      while (MyLink<>nil) and (MyLink<>self) do
        MyLink:=MyLink.Link;

      if MyLink=self then
        raise Exception.Create('Circular DataClientLink reference!');

      FLink:=Value;
      FLink.AddChildLink(self);
      end;
    end;
  end;

function TRtcAbsDataClientLink.GetLink: TRtcDataClientLink;
  begin
  Result:=FLink;
  end;

procedure TRtcAbsDataClientLink.SetClient(const Value: TRtcDataClient);
  begin
  if Value<>FClient then
    begin
    if assigned(FClient) then
      begin
      FClient.RemoveDataClientLink(self);
      FClient:=nil;
      end;

    if assigned(Value) then
      begin
      Link:=nil; // can not be linked to DataClientLink and DataClient at the same time.
      FClient:=Value;
      FClient.AddDataClientLink(self);
      end;
    end;
  end;

function TRtcAbsDataClientLink.GetClient: TRtcDataClient;
  begin
  Result:=FClient;
  end;

procedure TRtcAbsDataClientLink.PostRequest(var myReq; FromInsideEvent:boolean=False);
  var
    Req:TRtcClientRequestInfo absolute myReq;
  begin
  if assigned(FClient) then
    FClient.PostRequest(Req,FromInsideEvent)
  else if assigned(FLink) then
    FLink.PostRequest(Req,FromInsideEvent)
  else
    raise Exception.Create('PostRequest: Client connection undefined.');
  end;

procedure TRtcAbsDataClientLink.InsertRequest(const Req: TRtcClientRequestInfo);
  begin
  if assigned(FClient) then
    FClient.InsertRequest(Req)
  else if assigned(FLink) then
    FLink.InsertRequest(Req)
  else
    raise Exception.Create('InsertRequest: Client connection undefined.');
  end;

function TRtcAbsDataClientLink.RequestCount: integer;
  begin
  if assigned(FClient) then
    Result:=FClient.RequestCount
  else if assigned(FLink) then
    Result:=FLink.RequestCount
  else
    raise Exception.Create('RequestCount: Client connection undefined.');
  end;

procedure TRtcAbsDataClientLink.SkipRequests;
  begin
  if assigned(FClient) then
    FClient.SkipRequests
  else if assigned(FLink) then
    FLink.SkipRequests
  else
    raise Exception.Create('SkipRequests: Client connection undefined.');
  end;

procedure TRtcAbsDataClientLink.CancelRequests;
  begin
  if assigned(FClient) then
    FClient.CancelRequests
  else if assigned(FLink) then
    FLink.CancelRequests
  else
    raise Exception.Create('CancelRequests: Client connection undefined.');
  end;

function TRtcAbsDataClientLink.DoWaitForCompletion(UserInteractionAllowed: boolean; _Timeout: cardinal; AllowMessageProcessing:boolean): TRtcWaitForCompletionResult;
  begin
  if assigned(FClient) then
    Result:=FClient.DoWaitForCompletion(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
  else if assigned(FLink) then
    Result:=FLink.DoWaitForCompletion(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
  else
    raise Exception.Create('DoWaitForCompletion: Client connection undefined.');
  end;

function TRtcAbsDataClientLink.WaitForCompletion(UserInteractionAllowed: boolean; _Timeout: cardinal; AllowMessageProcessing:boolean): boolean;
  begin
  if assigned(FClient) then
    Result:=FClient.WaitForCompletion(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
  else if assigned(FLink) then
    Result:=FLink.WaitForCompletion(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
  else
    raise Exception.Create('WaitForCompletion: Client connection undefined.');
  end;

procedure TRtcAbsDataClientLink.WaitForCompletionEx(UserInteractionAllowed: boolean; _Timeout: cardinal; AllowMessageProcessing:boolean);
  begin
  if assigned(FClient) then
    FClient.WaitForCompletionEx(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
  else if assigned(FLink) then
    FLink.WaitForCompletionEx(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
  else
    raise Exception.Create('WaitForCompletionEx: Client connection undefined.');
  end;

function TRtcAbsDataClientLink.CheckLink(const Value: TRtcAbsDataClientLink): boolean;
  begin
  if Value=FLink then
    Result:=True
  else if assigned(FLink) then
    Result:=FLink.CheckLink(Value)
  else
    Result:=False;
  end;

procedure TRtcAbsDataClientLink.RemoveClient(const Value: TRtcDataClient);
  begin
  if Value=FClient then Client:=nil;
  end;

procedure TRtcAbsDataClientLink.RemoveLink(const Value: TRtcAbsDataClientLink);
  begin
  if Value=FLink then Link:=nil;
  end;

procedure TRtcAbsDataClientLink.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FClient then
      SetClient(nil)
    else if AComponent=FLink then
      SetLink(nil);
  end;

function TRtcAbsDataClientLink.isIdle: boolean;
  begin
  if assigned(FClient) then
    Result:=FClient.isIdle
  else if assigned(FLink) then
    Result:=FLink.isIdle
  else
    Result:=True;
  end;

function TRtcAbsDataClientLink.isMultiThreaded: boolean;
  begin
  if assigned(FClient) then
    Result:=FClient.MultiThreaded
  else if assigned(FLink) then
    Result:=FLink.isMultiThreaded
  else
    Result:=False;
  end;

procedure TRtcAbsDataClientLink.Call_BeginRequest(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_ConnectLost(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_DataIn(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_DataOut(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_DataReceived(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_DataSent(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_RepostCheck(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_ResponseData(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_ResponseDone(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_ResponseReject(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_SessionClose(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_SessionOpen(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_WSConnect(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_WSConnectLost(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_WSDataIn(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_WSDataOut(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_WSDataReceived(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsDataClientLink.Call_WSDataSent(Sender: TRtcConnection);
  begin
  end;

{ TRtcDataRequest }

constructor TRtcDataRequestInfo.Create;
  begin
  inherited;
  FEvents:=nil;
  end;

destructor TRtcDataRequestInfo.Destroy;
  begin
  try
    FEvents:=nil;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataRequestInfo.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRequestInfo.Call_DataReceived(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_DataReceived(Sender);
  end;

procedure TRtcDataRequestInfo.Call_DataOut(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_DataOut(Sender);
  end;

procedure TRtcDataRequestInfo.Call_DataIn(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_DataIn(Sender);
  end;

procedure TRtcDataRequestInfo.Call_DataSent(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_DataSent(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ConnectLost(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ConnectLost(Sender);
  end;

procedure TRtcDataRequestInfo.Call_RepostCheck(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_RepostCheck(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ReadyToSend(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ReadyToSend(Sender);
  end;

procedure TRtcDataRequestInfo.Call_BeginRequest(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_BeginRequest(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ResponseDone(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ResponseDone(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ResponseData(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ResponseData(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ResponseAbort(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ResponseAbort(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ResponseReject(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ResponseReject(Sender);
  end;

procedure TRtcDataRequestInfo.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_SessionClose(Sender);
  end;

procedure TRtcDataRequestInfo.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_SessionOpen(Sender);
  end;

procedure TRtcDataRequestInfo.Call_WSConnect(Sender: TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_WSConnect(Sender);
  end;

procedure TRtcDataRequestInfo.Call_WSConnectLost(Sender: TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_WSConnectLost(Sender);
  end;

procedure TRtcDataRequestInfo.Call_WSDataIn(Sender: TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_WSDataIn(Sender);
  end;

procedure TRtcDataRequestInfo.Call_WSDataOut(Sender: TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_WSDataOut(Sender);
  end;

procedure TRtcDataRequestInfo.Call_WSDataReceived(Sender: TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_WSDataReceived(Sender);
  end;

procedure TRtcDataRequestInfo.Call_WSDataSent(Sender: TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_WSDataSent(Sender);
  end;

{ TRtcDataRequestData }

constructor TRtcDataRequestData.Create;
  begin
  inherited;
  FRequest:=nil;
  end;

destructor TRtcDataRequestData.Destroy;
  begin
  try
    RtcFreeAndNil(FRequest);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataRequestData.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcDataRequest }

procedure TRtcDataRequest.Call_ConnectLost(Sender: TRtcConnection);
  begin
  if assigned(FOnConnectLost) then
    if not AutoSyncEvents or not Sender.Sync(FOnConnectLost) then
      FOnConnectLost(Sender);
  end;

procedure TRtcDataRequest.Call_WSConnectLost(Sender: TRtcConnection);
  begin
  if assigned(FOnWSDisconnect) then
    if not AutoSyncEvents or not Sender.Sync(FOnWSDisconnect) then
      FOnWSDisconnect(Sender);
  if assigned(FWS) then
    FWS.wsRemove(Sender);
  end;

procedure TRtcDataRequest.Call_DataReceived(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_PeekReceived(Sender);

  if assigned(FOnDataReceived) then
    if not AutoSyncEvents or not Sender.Sync(FOnDataReceived) then
      FOnDataReceived(Sender);
  end;

procedure TRtcDataRequest.Call_WSDataReceived(Sender: TRtcConnection);
  begin
  if assigned(FOnWSDataReceived) then
    if not AutoSyncEvents or not Sender.Sync(FOnWSDataReceived) then
      FOnWSDataReceived(Sender);
  end;

procedure TRtcDataRequest.Call_DataOut(Sender: TRtcConnection);
  begin
  if assigned(FOnDataOut) then
    if not AutoSyncEvents or not Sender.Sync(FOnDataOut) then
      FOnDataOut(Sender);
  end;

procedure TRtcDataRequest.Call_WSDataOut(Sender: TRtcConnection);
  begin
  if assigned(FOnWSDataOut) then
    if not AutoSyncEvents or not Sender.Sync(FOnWSDataOut) then
      FOnWSDataOut(Sender);
  end;

procedure TRtcDataRequest.Call_DataIn(Sender: TRtcConnection);
  begin
  if assigned(FOnDataIn) then
    if not AutoSyncEvents or not Sender.Sync(FOnDataIn) then
      FOnDataIn(Sender);
  end;

procedure TRtcDataRequest.Call_WSDataIn(Sender: TRtcConnection);
  begin
  if assigned(FOnWSDataIn) then
    if not AutoSyncEvents or not Sender.Sync(FOnWSDataIn) then
      FOnWSDataIn(Sender);
  end;

procedure TRtcDataRequest.Call_DataSent(Sender: TRtcConnection);
  begin
  if assigned(FOnDataSent) then
    if not AutoSyncEvents or not Sender.Sync(FOnDataSent) then
      FOnDataSent(Sender);
  end;

procedure TRtcDataRequest.Call_WSDataSent(Sender: TRtcConnection);
  begin
  if assigned(FOnWSDataSent) then
    if not AutoSyncEvents or not Sender.Sync(FOnWSDataSent) then
      FOnWSDataSent(Sender);
  end;

procedure TRtcDataRequest.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  if assigned(FOnReadyToSend) then
    if not AutoSyncEvents or not Sender.Sync(FOnReadyToSend) then
      FOnReadyToSend(Sender);
  end;

procedure TRtcDataRequest.Call_BeginRequest(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_BeginRequest(Sender)
  else if assigned(FClient) then
    FClient.CallBeginRequest;

  if not (Sender.RequestInserted or
          Sender.Request.Skipped or
          Sender.Request.Cancelled or
          Sender.Response.Rejected) then
    begin
    if assigned(FOnBeginRequest) then
      if not AutoSyncEvents or not Sender.Sync(FOnBeginRequest) then
        FOnBeginRequest(Sender);
    if Sender.Request.Info.isType[DATAREQ_BODY]=rtc_ByteStream then
      Sender.WriteEx(Sender.Request.Info.asByteArray[DATAREQ_BODY]);
    end;
  end;

procedure TRtcDataRequest.Call_ResponseData(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseData) then
    if not AutoSyncEvents or not Sender.Sync(FOnResponseData) then
      FOnResponseData(Sender);

  if not (Sender.Request.Skipped or
          Sender.Request.Cancelled or
          Sender.Response.Rejected) then
    if assigned(FLink) then
      FLink.Call_ResponseData(Sender)
    else if assigned(FClient) then
      FClient.CallResponseData;
  end;

procedure TRtcDataRequest.Call_ResponseDone(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseDone) then
    if not AutoSyncEvents or not Sender.Sync(FOnResponseDone) then
      FOnResponseDone(Sender);

  if not (Sender.Request.Skipped or
          Sender.Request.Cancelled or
          Sender.Response.Rejected) then
    if assigned(FLink) then
      FLink.Call_ResponseDone(Sender)
    else if assigned(FClient) then
      FClient.CallResponseDone;
  end;

procedure TRtcDataRequest.Call_WSConnect(Sender: TRtcConnection);
  begin
  if assigned(FWS) then
    FWS.wsAdd(Sender);
  if assigned(FOnWSConnect) then
    if not AutoSyncEvents or not Sender.Sync(FOnWSConnect) then
      FOnWSConnect(Sender);
  end;

procedure TRtcDataRequest.Call_RepostCheck(Sender: TRtcConnection);
  begin
  if ((AutoRepost<0) or (Sender.Request.Reposted<AutoRepost)) then
    Sender.Request.Repost
  else
    begin
    if assigned(FOnRepostCheck) then
      if not Sender.Request.Reposting then
        FOnRepostCheck(Sender);

    if not Sender.Request.Reposting then
      if assigned(FLink) then
        FLink.Call_RepostCheck(Sender)
      else if assigned(FClient) then
        FClient.CallRepostCheck;
    end;
  end;

procedure TRtcDataRequest.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseAbort) then
    if not AutoSyncEvents or not Sender.Sync(FOnResponseAbort) then
      FOnResponseAbort(Sender);

  if not Sender.Request.Reposting then
    if assigned(FLink) then
      FLink.Call_ResponseAbort(Sender)
    else if assigned(FClient) then
      FClient.CallResponseAbort;
  end;

procedure TRtcDataRequest.Call_ResponseReject(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseReject) then
    if not AutoSyncEvents or not Sender.Sync(FOnResponseReject) then
      FOnResponseReject(Sender);

  if assigned(FLink) then
    FLink.Call_ResponseReject(Sender)
  else if assigned(FClient) then
    FClient.CallResponseReject;
  end;

procedure TRtcDataRequest.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionOpen) then
    if not AutoSyncEvents or not Sender.Sync(FOnSessionOpen) then
      FOnSessionOpen(Sender);

  if assigned(FLink) then
    FLink.Call_SessionOpen(Sender)
  else if assigned(FClient) then
    FClient.CallSessionOpen;
  end;

procedure TRtcDataRequest.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionClose) then
    if not AutoSyncEvents or not Sender.Sync(FOnSessionClose) then
      FOnSessionClose(Sender);

  if assigned(FLink) then
    FLink.Call_SessionClose(Sender)
  else if assigned(FClient) then
    FClient.CallSessionClose;
  end;

constructor TRtcDataRequest.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FCS:=TRtcCritSec.Create;
  FMyData:=tObjList.Create(16);
  FMainThrData:=TRtcDataRequestData.Create;

  FWS:=TRtcWSManager.Create;
  end;

destructor TRtcDataRequest.Destroy;
  begin
  try
    RtcFreeAndNil(FMainThrData);
    RtcFreeAndNil(FMyData);
    RtcFreeAndNil(FCS);

    RtcFreeAndNil(FWS);

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataRequest.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcDataRequest.CheckMyData: TRtcDataRequestData;
  var
    id:RtcThrID;
    obj:TObject;
  begin
  if not FHyperThreading then
    Result:=FMainThrData
  else
    begin
    if InsideMainThread then
      Result:=FMainThrData
    else
      begin
      id:=GetMyThreadID;
      FCS.Acquire;
      try
        obj:=FMyData.search(id);
        if obj<>nil then
          Result:=TRtcDataRequestData(obj)
        else
          Result:=nil;
      finally
        FCS.Release;
        end;
      end;
    end;
  end;

procedure TRtcDataRequest.ClearMyData;
  var
    id:RtcThrID;
    obj:TObject;
  begin
  if FHyperThreading then
    begin
    if not InsideMainThread then
      begin
      id:=GetMyThreadId;
      FCS.Acquire;
      try
        obj:=FMyData.search(id);
        if obj<>nil then
          begin
          FMyData.remove(id);
          RtcFreeAndNil(obj);
          end;
      finally
        FCS.Release;
        end;
      end;
    end;
  end;

function TRtcDataRequest.GetMyData: TRtcDataRequestData;
  var
    id:RtcThrID;
    obj:TObject;
  begin
  if not FHyperThreading then
    Result:=FMainThrData
  else
    begin
    if InsideMainThread then
      Result:=FMainThrData
    else
      begin
      id:=GetMyThreadId;
      FCS.Acquire;
      try
        obj:=FMyData.search(id);
        if obj=nil then
          begin
          obj:=TRtcDataRequestData.Create;
          FMyData.insert(id, obj);
          end;
        Result:=TRtcDataRequestData(obj);
      finally
        FCS.Release;
        end;
      end;
    end;
  end;

procedure TRtcDataRequest.PostMethod(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil);
  begin
  if Request.Method='' then Request.Method:='GET';
  if Request.Info.isNull[DATAREQ_BODY] then
    Request.Info.NewByteStream(DATAREQ_BODY);
  Post(FromInsideEvent,Sender);
  end;

procedure TRtcDataRequest.PostMethod(const MyMethod:RtcString; FromInsideEvent:boolean=False; Sender:TRtcConnection=nil);
  begin
  if MyMethod<>'' then Request.Method:=MyMethod
  else if Request.Method='' then Request.Method:='GET';
  if Request.Info.isNull[DATAREQ_BODY] then
    Request.Info.NewByteStream(DATAREQ_BODY);
  Post(FromInsideEvent,Sender);
  end;

procedure TRtcDataRequest.Post(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil);
  var
    myData:TRtcDataRequestData;
    DataReq:TRtcDataRequestInfo;
  begin
  myData:=CheckMyData;

  if myData=nil then
    raise Exception.Create('Prepare your request using the "Request" property before callind "Post".');

  if myData.FRequest=nil then
    raise Exception.Create('Prepare your request using the "Request" property before callind "Post".');

  with myData do
    begin
    DataReq:=TRtcDataRequestInfo.Create;
    DataReq.Request:=FRequest;
    DataReq.Events:=Self;
    FRequest:=nil;
    end;

{$IFDEF AUTOREFCOUNT}
  myData:=nil;
{$ENDIF}

  if assigned(Sender) and (Sender is TRtcDataClient) then
    Sender.PostRequest(DataReq,FromInsideEvent)
  else
    PostRequest(DataReq,FromInsideEvent);

  // Free temporary object from memory
  ClearMyData;
  end;

procedure TRtcDataRequest.Insert(Sender:TRtcConnection);
  var
    DataReq:TRtcDataRequestInfo;
  begin
  DataReq:=TRtcDataRequestInfo.Create;
  DataReq.Request:=TRtcClientRequest.Create;
  DataReq.Events:=Self;
  if assigned(Sender) and (Sender is TRtcDataClient) then
    Sender.InsertRequest(DataReq)
  else
    InsertRequest(DataReq);
  end;

function TRtcDataRequest.GetRequest: TRtcClientRequest;
  var
    myData:TRtcDataRequestData;
  begin
  myData:=GetMyData;
  if not assigned(myData.FRequest) then
    myData.FRequest:=TRtcClientRequest.Create;
  Result:=myData.FRequest;
  end;

procedure TRtcDataRequest.WriteEx(const s:RtcByteArray);
  begin
  if Request.Info.isNull[DATAREQ_BODY] then
    Request.Info.NewByteStream(DATAREQ_BODY);
  if length(s)>0 then
    if Request.Info.isType[DATAREQ_BODY]=rtc_ByteStream then
      Request.Info.asByteStream[DATAREQ_BODY].Write(s[0],length(s));
  end;

procedure TRtcDataRequest.Write(const s:RtcString='');
  var
    ar:RtcByteArray;
  begin
  if Request.Info.isNull[DATAREQ_BODY] then
    Request.Info.NewByteStream(DATAREQ_BODY);
  if length(s)>0 then
    if Request.Info.isType[DATAREQ_BODY]=rtc_ByteStream then
      begin
      ar:=RtcStringToBytes(s);
      Request.Info.asByteStream[DATAREQ_BODY].Write(ar[0],length(s));
      SetLength(ar,0);
      end;
  end;

function TRtcDataRequest.wsGetID(const Sender:TRtcConnection):RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) or (Sender=nil) then
    Result:=0
  else
    Result:=FWS.wsGetID(Sender);
  end;

function TRtcDataRequest.wsCount: integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsCount;
  end;

function TRtcDataRequest.wsFirst: RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsFirst;
  end;

function TRtcDataRequest.wsLast: RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsLast;
  end;

function TRtcDataRequest.wsNext(id: RtcIntPtr): RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsNext(id);
  end;

function TRtcDataRequest.wsPrior(id: RtcIntPtr): RtcIntPtr;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsPrior(id);
  end;

function TRtcDataRequest.wSend(id: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (id=0) or (iFrame=nil) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,iFrame,iName);
  end;

function TRtcDataRequest.wSendMore(id: RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcByteArray; vFinal:boolean=False): integer;
  begin
  if (id=0) or (iFrame=nil) or (self=nil) or (FWS=nil) or ((vFinal=False) and (length(vPayload)=0)) then
    Result:=0
  else
    Result:=FWS.wSendMore(id,iFrame,vPayload,vFinal);
  end;

function TRtcDataRequest.wSendMore(id: RtcIntPtr; iFrame:TRtcWSFrame; const vPayload:RtcString; vFinal:boolean=False): integer;
  begin
  if (id=0) or (iFrame=nil) or (self=nil) or (FWS=nil) or ((vFinal=False) and (length(vPayload)=0)) then
    Result:=0
  else
    Result:=FWS.wSendMore(id,iFrame,vPayload,vFinal);
  end;

function TRtcDataRequest.wSendIfIdle(id: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,iFrame,iName);
  end;

function TRtcDataRequest.wSendToAll(iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (self=nil) or (FWS=nil) then
    begin
    Result:=0;
    RtcFreeAndNil(iFrame);
    end
  else
    Result:=FWS.wSendToAll(iFrame,iName);
  end;

function TRtcDataRequest.wSendToIdle(iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (self=nil) or (FWS=nil) then
    begin
    Result:=0;
    RtcFreeAndNil(iFrame);
    end
  else
    Result:=FWS.wSendToIdle(iFrame,iName);
  end;

function TRtcDataRequest.wSendToOthers(xid: RtcIntPtr; iFrame: TRtcWSFrame; const iName: RtcWideString=''): integer;
  begin
  if (self=nil) or (FWS=nil) then
    begin
    Result:=0;
    RtcFreeAndNil(iFrame);
    end
  else
    Result:=FWS.wSendToOthers(xid,iFrame,iName);
  end;

function TRtcDataRequest.wsDisconnect(id: RtcIntPtr): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsDisconnect(id);
  end;

function TRtcDataRequest.wsDisconnectAll: integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsDisconnectAll;
  end;

function TRtcDataRequest.wsClearSendingQueue(id: RtcIntPtr): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsClearSendingQueue(id);
  end;

function TRtcDataRequest.wsClearAllSendingQueues: integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wsClearAllSendingQueues;
  end;

function TRtcDataRequest.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,vOpcode,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSend(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSend(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,vOpcode,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendIfIdle(id: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (id=0) or (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendIfIdle(id,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendToAll(vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToAll(vOpcode,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendToAll(vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToAll(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendToAll(vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToAll(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendToIdle(vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToIdle(vOpcode,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendToIdle(vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToIdle(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendToIdle(vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToIdle(vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; vFinal: boolean=True; vPayloadLength:int64=0): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToOthers(xid,vOpcode,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcByteArray; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToOthers(xid,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

function TRtcDataRequest.wSendToOthers(xid: RtcIntPtr; vOpcode: Rtc4Bit; const vPayload: RtcString; vFinal: Boolean=True; vPayloadLength:int64=-1): integer;
  begin
  if (self=nil) or (FWS=nil) then
    Result:=0
  else
    Result:=FWS.wSendToOthers(xid,vOpcode,vPayload,vFinal,vPayloadLength);
  end;

{ TRtcStartRequestJob }

destructor TRtcStartRequestJob.Destroy;
  begin
  Client:=nil;
  inherited;
  end;

function TRtcStartRequestJob.Run(Thr:TRtcThread):boolean;
  begin
  try
    Client.StartRequest;
  except
    // ignore exceptions
    end;
  Client:=nil;
  Result:=True;
  end;

{ TRtcClientSession }

procedure TRtcClientSession.Init;
  begin
  Close;
  Clear;
  FID:='';
  FCreated:=0;
  FPeerAddr:='';
  end;

procedure TRtcClientSession.Open(const _ID: RtcString);
  begin
  if FID<>'' then Close;
  Clear;
  FID:=_ID;
  FCreated:=Now;
  if assigned(FCon) then
    FPeerAddr:=FCon.PeerAddr
  else
    FPeerAddr:='';
  if assigned(FCon) and (FCon is TRtcDataClient) then
    with TRtcDataClient(FCon) do
      if assigned(FActiveRequest) then
        FActiveRequest.Call_SessionOpen(FCon)
      else
        CallSessionOpen;
  end;

procedure TRtcClientSession.Close;
  begin
  if FID<>'' then
    begin
    if assigned(FCon) and (FCon is TRtcDataClient) then
      with TRtcDataClient(FCon) do
        if assigned(FActiveRequest) then
          FActiveRequest.Call_SessionClose(FCon)
        else
          CallSessionClose;
    FCreated:=0;
    FID:='';
    Clear;
    end;
  end;

{ TRtcDataClientLink }

constructor TRtcDataClientLink.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FDataClientLinks:=TRtcDataClientLinkList.Create;
  end;

destructor TRtcDataClientLink.Destroy;
  begin
  try
    RemoveAllChildLinks;
    RtcFreeAndNil(FDataClientLinks);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDataClientLink.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataClientLink.AddChildLink(Value: TRtcAbsDataClientLink);
  begin
  FDataClientLinks.Add(Value);
  end;

procedure TRtcDataClientLink.RemoveAllChildLinks;
  var
    _Link:TRtcAbsDataClientLink;
  begin
  while FDataClientLinks.Count>0 do
    begin
    _Link:=TRtcAbsDataClientLink(FDataClientLinks.Get(0));
    _Link.RemoveLink(self);
    end;
  end;

procedure TRtcDataClientLink.RemoveChildLink(Value: TRtcAbsDataClientLink);
  begin
  FDataClientLinks.Remove(Value);
  end;

procedure TRtcDataClientLink.Call_ConnectLost(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_ConnectLost(Sender);
  end;

procedure TRtcDataClientLink.Call_WSConnectLost(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_WSConnectLost(Sender);
  end;

procedure TRtcDataClientLink.Call_DataReceived(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_DataReceived(Sender);
  end;

procedure TRtcDataClientLink.Call_WSDataReceived(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_WSDataReceived(Sender);
  end;

procedure TRtcDataClientLink.Call_DataOut(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_DataOut(Sender);
  end;

procedure TRtcDataClientLink.Call_WSDataOut(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_WSDataOut(Sender);
  end;

procedure TRtcDataClientLink.Call_DataIn(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_DataIn(Sender);
  end;

procedure TRtcDataClientLink.Call_WSDataIn(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_WSDataIn(Sender);
  end;

procedure TRtcDataClientLink.Call_DataSent(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_DataSent(Sender);
  end;

procedure TRtcDataClientLink.Call_WSDataSent(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_WSDataSent(Sender);
  end;

procedure TRtcDataClientLink.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_ReadyToSend(Sender);
  end;

procedure TRtcDataClientLink.Call_WSConnect(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_WSConnect(Sender);
  end;

procedure TRtcDataClientLink.Call_PeekReceived(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_PeekReceived(Sender);

  if assigned(FOnPeekResponse) then
    if not AutoSyncEvents or not Sender.Sync(FOnPeekResponse) then
      FOnPeekResponse(Sender);
  end;

procedure TRtcDataClientLink.Call_BeginRequest(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_BeginRequest(Sender)
  else if assigned(FClient) then
    if Sender=FClient then FClient.CallBeginRequest;

  if not Sender.RequestInserted then
    if assigned(FOnBeginRequest) then
      if not AutoSyncEvents or not Sender.Sync(FOnBeginRequest) then
        FOnBeginRequest(Sender);
  end;

procedure TRtcDataClientLink.Call_ResponseData(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseData) then
    if not AutoSyncEvents or not Sender.Sync(FOnResponseData) then
      FOnResponseData(Sender);

  if not (Sender.Request.Skipped or
          Sender.Request.Cancelled or
          Sender.Response.Rejected) then
    if assigned(FLink) then
      FLink.Call_ResponseData(Sender)
    else if assigned(FClient) then
      if Sender=FClient then FClient.CallResponseData;
  end;

procedure TRtcDataClientLink.Call_ResponseDone(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseDone) then
    if not AutoSyncEvents or not Sender.Sync(FOnResponseDone) then
      FOnResponseDone(Sender);

  if not (Sender.Request.Skipped or
          Sender.Request.Cancelled or
          Sender.Response.Rejected) then
    if assigned(FLink) then
      FLink.Call_ResponseDone(Sender)
    else if assigned(FClient) then
      if Sender=FClient then FClient.CallResponseDone;
  end;

procedure TRtcDataClientLink.Call_RepostCheck(Sender: TRtcConnection);
  begin
  if assigned(FOnRepostCheck) then
    if not AutoSyncEvents or not Sender.Sync(FOnRepostCheck) then
      FOnRepostCheck(Sender);

  if not Sender.Request.Reposting then
    if assigned(FLink) then
      FLink.Call_RepostCheck(Sender)
    else if assigned(FClient) then
      if Sender=FClient then FClient.CallRepostCheck;
  end;

procedure TRtcDataClientLink.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseAbort) then
    if not AutoSyncEvents or not Sender.Sync(FOnResponseAbort) then
      FOnResponseAbort(Sender);

  if not Sender.Request.Reposting then
    if assigned(FLink) then
      FLink.Call_ResponseAbort(Sender)
    else if assigned(FClient) then
      if Sender=FClient then FClient.CallResponseAbort;
  end;

procedure TRtcDataClientLink.Call_ResponseReject(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseReject) then
    if not AutoSyncEvents or not Sender.Sync(FOnResponseReject) then
      FOnResponseReject(Sender);

  if assigned(FLink) then
    FLink.Call_ResponseReject(Sender)
  else if assigned(FClient) then
    if Sender=FClient then FClient.CallResponseReject;
  end;

procedure TRtcDataClientLink.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionClose) then
    if not AutoSyncEvents or not Sender.Sync(FOnSessionClose) then
      FOnSessionClose(Sender);

  if assigned(FLink) then
    FLink.Call_SessionClose(Sender)
  else if assigned(FClient) then
    if Sender=FClient then FClient.CallSessionClose;
  end;

procedure TRtcDataClientLink.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionOpen) then
    if not AutoSyncEvents or not Sender.Sync(FOnSessionOpen) then
      FOnSessionOpen(Sender);

  if assigned(FLink) then
    FLink.Call_SessionOpen(Sender)
  else if assigned(FClient) then
    if Sender=FClient then FClient.CallSessionOpen;
  end;

{ TRtcDualDataClientLink }

constructor TRtcDualDataClientLink.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FClient2:=nil;
  FLink2:=nil;
  end;

destructor TRtcDualDataClientLink.Destroy;
  begin
  try
    Client2:=nil; // remove from DataClient
    Link2:=nil; // remove from parent DataClientLink
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDualDataClientLink.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDualDataClientLink.CancelRequests;
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.CancelRequests
  else if assigned(FClient2) then
    FClient2.CancelRequests
  else
    raise Exception.Create('CancelRequests: 2nd Client connection undefined.');
  end;

function TRtcDualDataClientLink.GetClient2: TRtcDataClient;
  begin
  Result:=FClient2;
  end;

function TRtcDualDataClientLink.GetLink2: TRtcDataClientLink;
  begin
  Result:=FLink2;
  end;

procedure TRtcDualDataClientLink.InsertRequest(const Req: TRtcClientRequestInfo);
  var
    c1,c2:integer;
  begin
  if assigned(FLink) then
    c1:=FLink.RequestCount
  else if assigned(FClient) then
    c1:=FClient.RequestCount
  else
    c1:=RtcMaxLongInt;

  if assigned(FLink2) then
    c2:=FLink2.RequestCount
  else if assigned(FClient2) then
    c2:=FClient2.RequestCount
  else
    c2:=RtcMaxLongInt;

  if c1<=c2 then
    begin
    if assigned(FLink) then
      FLink.InsertRequest(Req)
    else if assigned(FClient) then
      FClient.InsertRequest(Req)
    else
      raise Exception.Create('InsertRequest: Client connection undefined.');
    end
  else
    begin
    if assigned(FLink2) then
      FLink2.InsertRequest(Req)
    else if assigned(FClient2) then
      FClient2.InsertRequest(Req)
    else
      raise Exception.Create('InsertRequest: 2nd Client connection undefined.');
    end;
  end;

procedure TRtcDualDataClientLink.PostRequest(var myReq; FromInsideEvent: boolean);
  var
    Req:TRtcClientRequestInfo absolute myReq;
    c1,c2:integer;
  begin
  if assigned(FLink) then
    c1:=FLink.RequestCount
  else if assigned(FClient) then
    c1:=FClient.RequestCount
  else
    c1:=RtcMaxLongInt;

  if assigned(FLink2) then
    c2:=FLink2.RequestCount
  else if assigned(FClient2) then
    c2:=FClient2.RequestCount
  else
    c2:=RtcMaxLongInt;

  if c1<=c2 then
    begin
    if assigned(FLink) then
      FLink.PostRequest(Req,FromInsideEvent)
    else if assigned(FClient) then
      FClient.PostRequest(Req,FromInsideEvent)
    else
      raise Exception.Create('PostRequest: Client connection undefined.');
    end
  else
    begin
    if assigned(FLink2) then
      FLink2.PostRequest(Req,FromInsideEvent)
    else if assigned(FClient2) then
      FClient2.PostRequest(Req,FromInsideEvent)
    else
      raise Exception.Create('PostRequest: 2nd Client connection undefined.');
    end;
  end;

function TRtcDualDataClientLink.RequestCount: integer;
  begin
  Result:=inherited RequestCount;
  if assigned(FLink2) then
    Result:=Result+FLink2.RequestCount
  else if assigned(FClient2) then
    Result:=Result+FClient2.RequestCount;
  end;

procedure TRtcDualDataClientLink.SetClient(const Value: TRtcDataClient);
  begin
  if Value<>FClient then
    begin
    if assigned(FClient2) and (FClient2=Value) then
      Client2:=nil;

    inherited SetClient(Value);
    end;
  end;

procedure TRtcDualDataClientLink.SetLink(const Value: TRtcDataClientLink);
  begin
  if Value<>FLink then
    begin
    if assigned(FLink2) and (FLink2=Value) then
      Link2:=nil;

    inherited SetLink(Value);
    end;
  end;

procedure TRtcDualDataClientLink.SetClient2(const Value: TRtcDataClient);
  begin
  if Value<>FClient2 then
    begin
    if assigned(FClient2) then
      begin
      FClient2.RemoveDataClientLink(self);
      FClient2:=nil;
      end;

    if assigned(Value) then
      begin
      Link2:=nil; // can not be linked to DataClientLink and DataClient at the same time.
      FClient2:=Value;
      FClient2.AddDataClientLink(self);
      end;
    end;
  end;

procedure TRtcDualDataClientLink.SetLink2(const Value: TRtcDataClientLink);
  begin
  if Value<>FLink2 then
    begin
    if assigned(FLink2) then
      begin
      FLink2.RemoveChildLink(self);
      FLink2:=nil;
      end;

    if assigned(Value) then
      begin
      Client2:=nil; // can not be maped to DataClient and to DataClientLink at the same time.

      // Check for circular reference before assigning!
      if Value=self then
        raise Exception.Create('Circular DataClientLink reference!');
      if Value.CheckLink(self) then
        raise Exception.Create('Circular DataClientLink reference!');
      if CheckLink(Value) then
        raise Exception.Create('Circular DataClientLink reference!');

      FLink2:=Value;
      FLink2.AddChildLink(self);
      end;
    end;
  end;

procedure TRtcDualDataClientLink.SkipRequests;
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.SkipRequests
  else if assigned(FClient2) then
    FClient2.SkipRequests
  else
    raise Exception.Create('SkipRequests: 2nd Client connection undefined.');
  end;

function TRtcDualDataClientLink.DoWaitForCompletion(UserInteractionAllowed: boolean; _Timeout: cardinal; AllowMessageProcessing:boolean): TRtcWaitForCompletionResult;
  begin
  Result:=inherited DoWaitForCompletion(UserInteractionAllowed, _Timeout, AllowMessageProcessing);
  if Result=wait_OK then
    begin
    if assigned(FLink2) then
      Result:=FLink2.DoWaitForCompletion(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
    else if assigned(FClient2) then
      Result:=FClient2.DoWaitForCompletion(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
    else
      raise Exception.Create('DoWaitForCompletion: 2nd Client connection undefined.');
    end;
  end;

function TRtcDualDataClientLink.WaitForCompletion(UserInteractionAllowed: boolean; _Timeout: cardinal; AllowMessageProcessing:boolean): boolean;
  begin
  Result:=inherited WaitForCompletion(UserInteractionAllowed, _Timeout, AllowMessageProcessing);
  if Result then
    begin
    if assigned(FLink2) then
      Result:=FLink2.WaitForCompletion(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
    else if assigned(FClient2) then
      Result:=FClient2.WaitForCompletion(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
    else
      raise Exception.Create('WaitForCompletion: 2nd Client connection undefined.');
    end;
  end;

procedure TRtcDualDataClientLink.WaitForCompletionEx(UserInteractionAllowed: boolean; _Timeout: cardinal; AllowMessageProcessing:boolean);
  begin
  inherited WaitForCompletionEx(UserInteractionAllowed, _Timeout, AllowMessageProcessing);
  if assigned(FLink2) then
    FLink2.WaitForCompletionEx(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
  else if assigned(FClient2) then
    FClient2.WaitForCompletionEx(UserInteractionAllowed, _Timeout, AllowMessageProcessing)
  else
    raise Exception.Create('WaitForCompletionEx: 2nd Client connection undefined.');
  end;

function TRtcDualDataClientLink.CheckLink(const Value: TRtcAbsDataClientLink): boolean;
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

procedure TRtcDualDataClientLink.RemoveClient(const Value: TRtcDataClient);
  begin
  inherited RemoveClient(Value);
  if Value=FClient2 then Client2:=nil;
  end;

procedure TRtcDualDataClientLink.RemoveLink(const Value: TRtcAbsDataClientLink);
  begin
  inherited RemoveLink(Value);
  if Value=FLink2 then Link2:=nil;
  end;

procedure TRtcDualDataClientLink.Call_ConnectLost(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_ConnectLost(Sender);
  end;

procedure TRtcDualDataClientLink.Call_WSConnectLost(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_WSConnectLost(Sender);
  end;

procedure TRtcDualDataClientLink.Call_DataReceived(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_DataReceived(Sender);
  end;

procedure TRtcDualDataClientLink.Call_WSDataReceived(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_WSDataReceived(Sender);
  end;

procedure TRtcDualDataClientLink.Call_DataOut(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_DataOut(Sender);
  end;

procedure TRtcDualDataClientLink.Call_WSDataOut(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_WSDataOut(Sender);
  end;

procedure TRtcDualDataClientLink.Call_DataIn(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_DataIn(Sender);
  end;

procedure TRtcDualDataClientLink.Call_WSDataIn(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_WSDataIn(Sender);
  end;

procedure TRtcDualDataClientLink.Call_DataSent(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_DataSent(Sender);
  end;

procedure TRtcDualDataClientLink.Call_WSDataSent(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_WSDataSent(Sender);
  end;

procedure TRtcDualDataClientLink.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_ReadyToSend(Sender);
  end;

procedure TRtcDualDataClientLink.Call_WSConnect(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_WSConnect(Sender);
  end;

procedure TRtcDualDataClientLink.Call_BeginRequest(Sender: TRtcConnection);
  begin
  if assigned(FLink2) then
    FLink2.Call_BeginRequest(Sender)
  else if assigned(FClient2) then
    if Sender=FClient2 then FClient2.CallBeginRequest;

  inherited;
  end;

procedure TRtcDualDataClientLink.Call_PeekReceived(Sender: TRtcConnection);
  begin
  if assigned(FLink2) then
    FLink2.Call_PeekReceived(Sender);

  inherited;
  end;

procedure TRtcDualDataClientLink.Call_ResponseData(Sender: TRtcConnection);
  begin
  inherited;

  if not (Sender.Request.Skipped or
          Sender.Request.Cancelled or
          Sender.Response.Rejected) then
    if assigned(FLink2) then
      FLink2.Call_ResponseData(Sender)
    else if assigned(FClient2) then
      if Sender=FClient2 then FClient2.CallResponseData;
  end;

procedure TRtcDualDataClientLink.Call_ResponseDone(Sender: TRtcConnection);
  begin
  inherited;

  if not (Sender.Request.Skipped or
          Sender.Request.Cancelled or
          Sender.Response.Rejected) then
    if assigned(FLink2) then
      FLink2.Call_ResponseDone(Sender)
    else if assigned(FClient2) then
      if Sender=FClient2 then FClient2.CallResponseDone;
  end;

procedure TRtcDualDataClientLink.Call_RepostCheck(Sender: TRtcConnection);
  begin
  inherited;

  if not Sender.Request.Reposting then
    if assigned(FLink2) then
      FLink2.Call_RepostCheck(Sender)
    else if assigned(FClient2) then
      if Sender=FClient2 then FClient2.CallRepostCheck;
  end;

procedure TRtcDualDataClientLink.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  inherited;

  if not Sender.Request.Reposting then
    if assigned(FLink2) then
      FLink2.Call_ResponseAbort(Sender)
    else if assigned(FClient2) then
      if Sender=FClient2 then FClient2.CallResponseAbort;
  end;

procedure TRtcDualDataClientLink.Call_ResponseReject(Sender: TRtcConnection);
  begin
  inherited;

  if assigned(FLink2) then
    FLink2.Call_ResponseReject(Sender)
  else if assigned(FClient2) then
    if Sender=FClient2 then FClient2.CallResponseReject;
  end;

procedure TRtcDualDataClientLink.Call_SessionClose(Sender: TRtcConnection);
  begin
  inherited;

  if assigned(FLink2) then
    FLink2.Call_SessionClose(Sender)
  else if assigned(FClient2) then
    if Sender=FClient2 then FClient2.CallSessionClose;
  end;

procedure TRtcDualDataClientLink.Call_SessionOpen(Sender: TRtcConnection);
  begin
  inherited;

  if assigned(FLink2) then
    FLink2.Call_SessionOpen(Sender)
  else if assigned(FClient2) then
    if Sender=FClient2 then FClient2.CallSessionOpen;
  end;

procedure TRtcDualDataClientLink.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FClient2 then
      SetClient2(nil)
    else if AComponent=FLink2 then
      SetLink2(nil);
  end;

function TRtcDualDataClientLink.isIdle: boolean;
  begin
  if inherited isIdle then
    begin
    if assigned(FClient2) then
      Result:=FClient2.isIdle
    else
      Result:=True;
    end
  else
    Result:=False;
  end;

{ TRtcClientRequestInfo }

procedure TRtcClientRequestInfo.Call_BeginRequest(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_ConnectLost(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_DataIn(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_DataOut(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_DataReceived(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_DataSent(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_RepostCheck(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_ResponseData(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_ResponseDone(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_ResponseReject(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_SessionClose(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_SessionOpen(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_WSConnect(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_WSConnectLost(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_WSDataIn(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_WSDataOut(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_WSDataReceived(Sender: TRtcConnection);
  begin
  end;

procedure TRtcClientRequestInfo.Call_WSDataSent(Sender: TRtcConnection);
  begin
  end;

constructor TRtcClientRequestInfo.Create;
  begin
  inherited;
  FRequest:=nil;
  end;

destructor TRtcClientRequestInfo.Destroy;
  begin
  try
    RtcFreeAndNil(FRequest);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcClientRequestInfo.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcClientRequestInfo.Get_Request: TRtcClientRequest;
  begin
  Result:=FRequest;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; {$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcDataCli Finalizing ...','DEBUG');{$ENDIF}
CloseThreadPool;
{$IFDEF RTC_DEBUG} Log('rtcDataCli Finalized.','DEBUG');{$ENDIF}
end.
