{
  @html(<b>)
  Load Balancer component
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit implements the TRtcLoadBalancer component
  used for writing HTTP/S Load Balancers.
}
unit rtcLoadBalance;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSystem,
  rtcSrcList,

  rtcInfo,
  rtcConn,
  rtcDataCli,
  rtcDataSrv,
  rtcHttpCli,
  rtcDataRoute;

type
  TRtcDataClientClass = class of TRtcDataClient;

var
  TRtcLoadBalanceClient:TRtcDataClientClass=TRtcHttpClient;

type
  TRtcLoadBalanceType = (lbt_SessionBalance, lbt_RequestBalance, lbt_RoundRobin);
  TRtcLoadBalanceOrder = (lbo_FIFO, lbo_LIFO, lbo_Random, lbo_Chance);

  TRtcLoadBalancerSession = class(TRtcRouteObject)
    public
    ID:RtcString;
    Close:boolean;
    end;

  TRtcLoadBalancerRequestSessionBodyEvent = procedure(Sender:TRtcDataServer; var NeedContent:boolean) of object;
  TRtcLoadBalancerResponseSessionBodyEvent = procedure(Sender:TRtcDataClient; var NeedContent:boolean) of object;

  TRtcLoadBalancerRequestSessionSetEvent = procedure(Sender:TRtcDataServer; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession) of object;
  TRtcLoadBalancerResponseSessionSetEvent = procedure(Sender:TRtcDataClient; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession) of object;

  TRtcLoadBalancerRequestOrderEvent = procedure(Sender:TRtcDataServer; var RequestOrder:TRtcLoadBalanceOrder) of object;

{$IFDEF RTC_ANON_METHODS}
  TRtcLoadBalancerRequestSessionBodyAnonMethod = reference to procedure(Sender:TRtcDataServer; var NeedContent:boolean);
  TRtcLoadBalancerResponseSessionBodyAnonMethod = reference to procedure(Sender:TRtcDataClient; var NeedContent:boolean);

  TRtcLoadBalancerRequestSessionSetAnonMethod = reference to procedure(Sender:TRtcDataServer; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession);
  TRtcLoadBalancerResponseSessionSetAnonMethod = reference to procedure(Sender:TRtcDataClient; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession);

  TRtcLoadBalancerRequestOrderAnonMethod = reference to procedure(Sender:TRtcDataServer; var RequestOrder:TRtcLoadBalanceOrder);
{$ENDIF}

  { @abstract(RTC Load Balancer component: Can be used for balancing one web application or any number of static web pages) }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcLoadBalancer=class(TRtcDataRouter)
  private
    Req:TRtcMultiClientPool;
    ReqCS:TRtcCritSec;
    ReqClosing:boolean;

    FLastIDX:integer;
    FBalanceType:TRtcLoadBalanceType;
    FRequestOrder:TRtcLoadBalanceOrder;

    FResponseBuffer: boolean;
    FRequestBuffer: boolean;
    FVirtualPathLen:integer;
    FVirtualPath,
    FVirtualPath2,
    FVirtualPathCS: RtcString;
    FVirtualHost,
    FVirtualHostCS: RtcString;

    FOnPrepareConnection: TRtcRouterClientEvent;
    FOnPostCheckOrder: TRtcLoadBalancerRequestOrderEvent;

    FRequestBodyHasSessionID: TRtcLoadBalancerRequestSessionBodyEvent;
    FResponseBodyHasSessionID: TRtcLoadBalancerResponseSessionBodyEvent;
    FRequestPrepareSessionID: TRtcLoadBalancerRequestSessionSetEvent;
    FResponsePrepareSessionID: TRtcLoadBalancerResponseSessionSetEvent;

    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    procedure SetVirtualHost(const Value: RtcString);
    procedure SetVirtualPath(const Value: RtcString);

  protected
    // @exclude
    procedure Event_CheckRequest(Sender:TRtcDataServer); override;

    // @exclude
    procedure Event_PostNewRequest(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; var AddToQueue:integer; var MoveToBottom:boolean); override;
    // @exclude
    procedure Event_PostOldRequest(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; var AddToQueue:integer; var MoveToBottom:boolean); override;
    // @exclude
    procedure Event_QueuedRequest(Sender:TRtcDataServer; AddedToQueue:integer; MovedToBottom:boolean); override;
    // @exclude
    procedure Event_PostReturn(DataRequest:TRtcDataRequest); override;

    // @exclude
    procedure Event_RequestBegin(Sender:TRtcDataClient); override;
    // @exclude
    procedure Event_RequestReceived(Sender:TRtcDataServer; Content:TRtcRouterContentBody); override;
    // @exclude
    procedure Event_RequestSent(Sender:TRtcDataClient); override;

    // @exclude
    procedure Event_ResponseBegin(Sender:TRtcDataClient); override;
    // @exclude
    procedure Event_ResponseReceived(Sender:TRtcDataClient; Content:TRtcRouterContentBody); override;
    // @exclude
    procedure Event_ResponseSent(Sender:TRtcDataServer); override;

    // @exclude
    procedure Event_RequestReceiveAbort(Sender:TRtcDataServer); override;
    // @exclude
    procedure Event_RequestSendAbort(Sender:TRtcDataClient); override;
    // @exclude
    procedure Event_ResponseReceiveAbort(Sender:TRtcDataClient); override;
    // @exclude
    procedure Event_ResponseSendAbort(Sender:TRtcDataServer); override;

    // @exclude
    procedure Event_PrepareConnection(Sender:TRtcDataClient); virtual;

    // @exclude
    procedure Event_RequestBodyHasSessionID(Sender:TRtcDataServer; var NeedContent:boolean); virtual;
    // @exclude
    procedure Event_RequestPrepareSessionID(Sender:TRtcDataServer; Content: TRtcRouterContentBody; Session:TRtcLoadBalancerSession); virtual;
    // @exclude
    procedure Event_ResponseBodyHasSessionID(Sender:TRtcDataClient; var NeedContent:boolean); virtual;
    // @exclude
    procedure Event_ResponsePrepareSessionID(Sender:TRtcDataClient; Content: TRtcRouterContentBody; Session:TRtcLoadBalancerSession); virtual;

    // @exclude
    procedure Event_PostCheckOrder(Sender:TRtcDataServer; var RequestOrder:TRtcLoadBalanceOrder); virtual;

    // @exclude
    procedure Client_WriteHeader(Sender:TRtcDataClient); override;

    // @exclude
    function GetRequestSessionID(Sender:TRtcDataServer):RtcString;

    // @exclude
    procedure GetDataRequestQueue(Sender:TRtcDataServer; var AddToQueue:integer);

    // @exclude
    function FindDataRequestID(Sender:TRtcDataServer; var sid:RtcString; var AddToQueue:integer):integer;

    // @exclude
    procedure GetDataRequest(Sender:TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer);

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    { Add a new Server to our Server pool and return Server Index (starting from 0) }
    function AddServer(const ServerAddr,ServerPort,RootURI:RtcString;
                       MaxConnections, MaxSessions:integer;
                       SessionTimeout: TDateTime;
                       ServerIPV:RtcIPV=rtc_IPVDefault):integer;

    { Update the state (True=Enabled / False=Disables) for Servers matching "ServerAddr:ServerPort".
      Use ServerAddr='' for all Server Addresses and/or ServerPort='' for all Server Ports. }
    procedure UpdateServerState(const ServerAddr,ServerPort:RtcString; ServerEnabled:boolean);

    { Check if Server "ServerAddr:ServerPort" is currently Enabled.
      Returns TRUE only if Server matching "ServerAddr:ServerPort" was found and is Enabled.
      Use ServerAddr='' for all Server Addresses and/or ServerPort='' for all Server Ports. }
    function IsServerEnabled(const ServerAddr, ServerPort: RtcString):boolean;

    { Returns the number of Servers matching "ServerAddr:ServerPort"
      with "Enabled" state set to "SererEnabled" (True/False).
      If called with "ServerEnabled=True", returns the number of ENABLED Servers.
      If called with "ServerEnabled=False", returns the number of DISABLED Servers.
      Use ServerAddr='' for all Server Addresses and/or ServerPort='' for all Server Ports. }
    function ServerStateCount(const ServerAddr, ServerPort: RtcString; ServerEnabled:boolean):integer;

    { Check if we have the Session with ID "SessionID" stored in our Session pool
      and return the Server Index to which that Session is linked if we have it
      or return -1 if the Session is NOT in our Session pool. }
    function HaveSession(SessionID:RtcString):integer;

    { Find the Session with ID "SessionID" in our Session pool and return the
      Server Index and update Session expiration time if the SessionID was found,
      or return -1 if the Session was NOT found in our Session pool. }
    function UpdateSession(SessionID:RtcString):integer;

    { Returns the number Servers registered with this Load Balancer }
    function ServerCount:integer;

    { Returns the list of all Servers registered with this Load Balancer, in the
      form "ServerAddr:ServerPort" with each Server separated by CR+LF (=#13#10). }
    function ServerList:RtcString;

    { Return the Server Index to which the TRtcDataClient component is linked. }
    function ServerIndex(Sender:TRtcDataClient):integer;

    { Return TRUE if the Server with Index "index" is ready to accept new requests. }
    function ServerReady(index:integer):boolean;

    { Number of currently active clients (connections to Servers) }
    function ActiveClients:integer;

    { Number of currently idle clients (connection components not used) }
    function IdleClients:integer;

    { Number of currently active Sessions }
    function ActiveSessions:integer;

    { Close all connections in our Connection Pool }
    procedure ClosePool;

    { Release the Connection pool }
    procedure ReleasePool;

    { Get Load Balancer Debug Info }
    function GetBalanceDebugInfo:RtcString;

    { Return the TRtcLoadBalancer component which has created this TRtcDataClient component. }
    class function GetLoadBalancer(Sender:TRtcDataClient):TRtcLoadBalancer;

    { Return the VirtualPath associated with the TRtcLoadBalancer component
      which was used to create the Sender:TRtcDataClient component }
    class function GetVirtualPath(Sender:TRtcDataClient):RtcString;

    { Return the RootURI associated with the Server for
      which the Sender:TRtcDataClient component was created. }
    class function GetRootURI(Sender:TRtcDataClient):RtcString;

    { Set Active to FALSE before you want to start closing connections
      to make sure the Load Balancer won't start re-opening them.
      Set Active to TRUE before starting the Server Listener. }
    property Active:boolean read GetActive write SetActive default True;

    { Check/Set the Enabled state for all Servers matching "ServerAddr:ServerPort". }
    property ServerEnabled[const ServerAddr,ServerPort:RtcString]:boolean read IsServerEnabled write UpdateServerState;

  {$IFDEF RTC_ANON_METHODS}

    { Get the Load Balancer Request Session Body event for anonymous method
        procedure(Sender:TRtcDataServer; var NeedContent:boolean) }
    function Anon(const Event:TRtcLoadBalancerRequestSessionBodyAnonMethod):TRtcLoadBalancerRequestSessionBodyEvent; overload;

    { Get the Load Balancer Response Session Body event for anonymous method
        procedure(Sender:TRtcDataClient; var NeedContent:boolean) }
    function Anon(const Event:TRtcLoadBalancerResponseSessionBodyAnonMethod):TRtcLoadBalancerResponseSessionBodyEvent; overload;

    { Get the Load Balancer Request Session Set event for anonymous method
        procedure(Sender:TRtcDataServer; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession) }
    function Anon(const Event:TRtcLoadBalancerRequestSessionSetAnonMethod):TRtcLoadBalancerRequestSessionSetEvent; overload;

    { Get the Load Balancer Response Session Set event for anonymous method
         procedure(Sender:TRtcDataClient; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession) }
    function Anon(const Event:TRtcLoadBalancerResponseSessionSetAnonMethod):TRtcLoadBalancerResponseSessionSetEvent; overload;

    { Get the Load Balancer Request Order event for anonymous method
         procedure(Sender:TRtcDataServer; var RequestOrder:TRtcLoadBalanceOrder) }
    function Anon(const Event:TRtcLoadBalancerRequestOrderAnonMethod):TRtcLoadBalancerRequestOrderEvent; overload;

  {$ENDIF}

  published
    { You want the TRtcLoadBalancer component to automatically accept Requests for a specific Virtual Host?
      Set the "VirtualHost" property to the string you want the TRtcLoadBalancer to look for in "Request.Host".
      If a Request arrives which contains the specified "VirtualHost" value (anywhere inside "Request.Host"),
      the Request will be accepted and processed by this TRtcLoadBalancer component. @html(<br><br>)

      NOTE: "VirtualHost" and "VirtualPath" properties always work together. If both properties are set,
      then only those Requests will be accepted for which "VirtualPath" *and* "VirtualHost" match. @html(<br><br>)

      IMPORTANT: "VirtualHost" property is NOT case-sensitive (www.host.com = Www.Host.coM = WWW.HOST.COM). }
    property VirtualHost:RtcString read FVirtualHostCS write SetVirtualHost;

    { You want the TRtcLoadBalancer component to automatically accept Requests with a specific Virtual Path
      and *remove* that "VirtualPath" from "Request.FileName" afterwards, so yu can host multiple web applications
      on different virtual paths using a single Load Balancer application (one per TRtcLoadBalancer component), then
      set the "VirtualPath" property to the string you want the TRtcLoadBalancer to look for in "Request.FileName". @html(<br><br>)

      "VirtualPath" property is NOT case-sensitive (/path = /Path = /PATH). Also, if you want
      to accept all Requests regardless of their path, set "VirtualPath" to '/' (slash). @html(<br><br>)

      If a Request arrives which starts with the specified "VirtualPath" value inside "Request.FileName" (NOT case sensitive),
      the Request will be accepted and the "VirtualPath" string will be removed from "Request.FileName", so you do NOT have
      to bother about virtual path changes nor do you need to change the code inside your events to host differet applications
      based on the same framework using the same methods for sending SessionIDs back and forth between your Clients and Servers.

      If the Server where the Requests will be forwarded to hosts the web application on a "virtual path",
      use the "AddServer" method with the "RootURI" parameter to set the correct path for the Server. (<br><br>)

      "VirtualHost" and "VirtualPath" properties always work together. If both properties are set,
      then only those Requests will be accepted for which "VirtualPath" *and* "VirtualHost" match. @html(<br><br>)

      If either one of the "VirtualHost" and "VirtualPath" properties are set, the "OnCheckRequest" event will
      only be called for Requests which have been accepted (depending on "VirtualHost" and "VirtualPath" values).
      If "VirtualHost" *and* "VirtualPath" properties are both empty, the "OnCheckRequest" event will be called
      for *all* Requests and you should use the "OnCheckRequest" event to manually accept Requests for this component. }
    property VirtualPath:RtcString read FVirtualPathCS write SetVirtualPath;

    { How should Load Balancing be performed? }
    property BalanceType:TRtcLoadBalanceType read FBalanceType write FBalanceType default lbt_SessionBalance;

    { How should new Requests which can't be sent immediatelly be Queued? }
    property RequestOrder:TRtcLoadBalanceOrder read FRequestOrder write FRequestOrder default lbo_FIFO;

    { This event is called before each "OnPostNewRequest" event and can be used to modify the "RequestOrder".
      If you want to use the same "RequestOrder" for all requests, simply set the "RequestOrder" property
      on the TRtcLoadBalancer component and all requests will be queued using the same order.
      If you want to use a different "RequestOrder" for some Requests, implement this event and set the
      "RequestOrder" parameter (sent to the event) to the order you want to use for the current request. }
    property OnPostCheckOrder:TRtcLoadBalancerRequestOrderEvent read FOnPostCheckOrder write FOnPostCheckOrder;

    { Use a Request Buffer to load complete Requests from Clients into memory before forwarding them to the Server?
      When "RequestBuffer" is TRUE, all Requests will be buffered in memory before being forwarded to the Sever.
      Setting "RequestBuffer" to TRUE will increase memory requirements of the Load Balancer and is NOT recommended. }
    property RequestBuffer:boolean read FRequestBuffer write FRequestBuffer default False;

    { Use a Response Buffer to load complete Responses from Servers into memory before forwarding them to Clients?
      When "ResponseBuffer" is TRUE, all Requests will be buffered in memory before being forwarded to the Sever.
      Setting "ResponseBuffer" to TRUE will increase memory requirements of the Load Balancer and is NOT recommended. }
    property ResponseBuffer:boolean read FResponseBuffer write FResponseBuffer default False;

    { TRtcLoadBalancer component will be creating TRtcHttpClient components depending on
      values used in the "AddServer" method. All the values sent to the AddServer method will
      be set by the TRtcLoadBalancer component as well as the AutoConnect property (TRUE). @html(<br><br>)

      If there are any other properties required on the TRtcHttpClient component, they can
      be set using this event. The Sender (TRtcHttpClient) component sent to the event will
      already have its ServerAddr, ServerPort and AutoConnect properties set. }
    property PrepareConnection:TRtcRouterClientEvent read FOnPrepareConnection write FOnPrepareConnection;

    { This event is required for stateful web applications, but can be left unimplemented for stateless web
      applications and static web pages. When the TRtcLoadBalancer component is used for balancing a stateful
      web application, implement this event when SessionIDs are sent from Clients inside Request Content Body. @html(<br><br>)

      For Requests where SessionIDs are expected inside Request Content Body, set "NeedContent" to TRUE.
      Then, the complete request content will be buffered and sent to the "RequestPrepareSessionID".
      Otherwise, the "RequestPrepareSessionID" event will be called with "Content" parameter NIL
      immediately when Request headers arrive, without waiting for the complete content body. }
    property RequestBodyHasSessionID:TRtcLoadBalancerRequestSessionBodyEvent read FRequestBodyHasSessionID write FRequestBodyHasSessionID;

    { This event is only required for stateful web applications. If a Request from the Client contains a SessionID,
      this event has to be implemented to extract the SessionID and set the "Session.ID" parameter, so the Request
      can be forwarded to the correct Server. If you have set the "NeedContent" parameter for this Request in the
      "RequestBodyHasSessionID" event to TRUE, this event will be called with "Content" parameter assigned and will
      containing the complete Request content body. If "NeedContent" was left as FALSE, "Content" value will be NIL. }
    property RequestPrepareSessionID:TRtcLoadBalancerRequestSessionSetEvent read FRequestPrepareSessionID write FRequestPrepareSessionID;

    { This event is required for stateful web applications. When the TRtcLoadBalancer component is used for balancing
      a stateful web application, implement this event if SessionIDs are sent from Servers inside Response Content Body. @html(<br><br>)

      For Responses where SessionIDs are expected inside Response Content Body, set "NeedContent" to TRUE. When
      "NeedContent" is TRUE, the complete Request content body will be loaded and sent to the "ResponsePrepareSessionID"
      in the "Content" parameter. Otherwise, the "ResponsePrepareSessionID" event will be called with "Content=NIL",
      immediately when the Response headers arrive, without waiting or buffering the complete Response Content body. }
    property ResponseBodyHasSessionID:TRtcLoadBalancerResponseSessionBodyEvent read FResponseBodyHasSessionID write FResponseBodyHasSessionID;

    { This event is only required for stateful web applications. If a Response from the Server contains a SessionID,
      this event has to be implemented to extract the SessionID and set the "Session.ID" and "Session.Close" parameters.
      When "Session.ID" is set inside this event, the TRtcLoadBalancer will open a new Session and bind it to the Server
      from which the Response was received if the Session was not already open, or update the Session expiration time
      if the Session was already open. If the Session was now closed by the Server, make sure to set "Session.Close" to
      TRUE so the TRtcLoadBalancer can remove the Session from its Session list. @html(<br><br>)

      If you have set the "NeedContent" parameter for this Response in the "ResponseBodyHasSessionID" event to TRUE,
      this event will be called with "Content" parameter assigned and will containing the complete Response Content Body.
      If "NeedContent" was left as FALSE, the "Content" parameter sent to this event value will be NIL. }
    property ResponsePrepareSessionID:TRtcLoadBalancerResponseSessionSetEvent read FResponsePrepareSessionID write FResponsePrepareSessionID;
    end;

implementation

type
  TRtcClientConPool=class(TRtcClientPool)
  public
    FServerAddr:RtcString;
    FServerPort:RtcString;
    FServerIPV:RtcIPV;
    FRootURI:RtcString;

    SessionTimeout:TDateTime;
    MaxSessionCount,
    MaxReqCount:integer;

    FOnNewClient:TRtcRouterClientEvent;

    function NewClient:TRtcDataClient; override;
    end;

{ TRtcClientConPool }

function TRtcClientConPool.NewClient: TRtcDataClient;
  begin
  if Disabled then
    Result:=nil
  else if (MaxReqCount>0) and (ActiveClients>=MaxReqCount) then
    Result:=nil
  else if (MaxSessionCount>0) and (ActiveSessions>=MaxSessionCount) then
    Result:=nil
  else
    begin
    Result:=TRtcLoadBalanceClient.Create(nil);
    Result.ServerAddr:=FServerAddr;
    Result.ServerPort:=FServerPort;
    Result.ServerIPV:=FServerIPV;
    Result.Info.asString['$root']:=FRootURI;
    if assigned(FOnNewClient) then
      FOnNewClient(Result);
    end;
  end;

{ TRtcLoadBalancer }

procedure TRtcLoadBalancer.Event_PrepareConnection(Sender: TRtcDataClient);
  begin
  Sender.Info.asPtr['$owner']:=self;
  if assigned(FOnPrepareConnection) then
    FOnPrepareConnection(Sender);
  end;

procedure TRtcLoadBalancer.Event_CheckRequest(Sender: TRtcDataServer);
  procedure PrepSes;
    var
      NeedContent:boolean;
      ses:TRtcLoadBalancerSession;
    begin
    if RequestBuffer then
      Sender.Request.ManualRead:=False;

    NeedContent:=False;
    Event_RequestBodyHasSessionID(Sender,NeedContent);
    if NeedContent then
      begin
      Sender.Request.ManualRead:=False;
      Sender.Request.Info.asBoolean['$prep']:=True;
      end
    else
      begin
      ses:=TRtcLoadBalancerSession.Create;
      try
        Event_RequestPrepareSessionID(Sender,nil,ses);
        Sender.Request.Info.asString['$sid']:=ses.ID;
        Sender.Request.Info.asBoolean['$sidclos']:=ses.Close;
      finally
        ses.Free;
        end;
      end;
    end;
  begin
  if FVirtualPath<>'' then
    begin
    if length(Sender.Request.FileName)<FVirtualPathLen then
      Exit // Path too short
    else if FVirtualHost<>'' then
      if Pos(FVirtualHost,Upper_Case(Sender.Request.Host))<=0 then
        Exit; // Virtual host does not match

    if FVirtualPath='/' then // accepting All Requests
      begin
      Sender.Accept;
      Sender.Request.ManualRead:=True;
      inherited;
      end
    else
      begin
      if length(Sender.Request.FileName)>FVirtualPathLen then
        begin
        if Copy(Upper_Case(Sender.Request.FileName),1,FVirtualPathLen+1)=FVirtualPath2 then
          begin
          Sender.Request.FileName:=Copy(Sender.Request.FileName,FVirtualPathLen+1,length(Sender.Request.FileName)-FVirtualPathLen);
          Sender.Accept;
          Sender.Request.ManualRead:=True;
          inherited;
          end
        end
      else if Upper_Case(Sender.Request.FileName)=FVirtualPath then
        begin
        Sender.Request.FileName:='/';
        Sender.Accept;
        Sender.Request.ManualRead:=True;
        inherited;
        end;
      end;
    end
  else if FVirtualHost<>'' then
    begin
    if Pos(FVirtualHost,Upper_Case(Sender.Request.Host))>0 then
      begin
      Sender.Accept;
      Sender.Request.ManualRead:=True;
      inherited;
      end;
    end
  else
    inherited;

  if Sender.Request.Accepted then PrepSes;
  end;

procedure TRtcLoadBalancer.Event_PostNewRequest(Sender: TRtcDataServer; var DataRequest: TRtcDataRequest;
    var AddToQueue: integer; var MoveToBottom: boolean);
  var
    ReqOrder:TRtcLoadBalanceOrder;
  begin
  if ReqClosing then Exit;

  ReqOrder:=FRequestOrder;
  Event_PostCheckOrder(Sender, ReqOrder);

  if ReqOrder=lbo_Chance then
    GetDataRequest(Sender, DataRequest, AddToQueue)
  else
    begin
    case ReqOrder of
      lbo_FIFO: MoveToBottom:=True; // Standard order (FIFO)
      lbo_LIFO: MoveToBottom:=False; // Reverse order (LIFO)
      lbo_Random: MoveToBottom:=random(2)=1; // Randomized
      end;
    if not MoveToBottom then
      GetDataRequest(Sender, DataRequest, AddToQueue)
    else
      begin
      GetDataRequestQueue(Sender,AddToQueue);
      if WaitingRequests(AddToQueue)=0 then
        begin
        if AddToQueue=0 then
          GetDataRequest(Sender, DataRequest, AddToQueue)
        else if WaitingRequests(0)=0 then
          GetDataRequest(Sender, DataRequest, AddToQueue);
        end;
      end;
    end;

  inherited;
  end;

procedure TRtcLoadBalancer.Event_PostOldRequest(Sender: TRtcDataServer;
    var DataRequest: TRtcDataRequest; var AddToQueue: integer; var MoveToBottom: boolean);
  begin
  if ReqClosing then Exit;

  GetDataRequest(Sender, DataRequest, AddToQueue);

  inherited;
  end;

procedure TRtcLoadBalancer.Event_PostReturn(DataRequest: TRtcDataRequest);
  var
    FromQueue,FromQueue2:integer;
  begin
  if ReqClosing then
    begin
    inherited;
    ReqCS.Acquire;
    try
      Req.PutDataRequest(DataRequest);
      Req.RemoveExpiredSessions(Now);
    finally
      ReqCS.Release;
      end;
    end
  else if FRequestOrder=lbo_Chance then
    begin
    // We will be using "random" to avoid prioritizing sticky sessions to
    // give non-Sessioned Requests a chance to get through during "peek times"
    if random(2)>0 then
      begin
      FromQueue:=Req.GetPoolIndex(DataRequest.Client)+1;
      FromQueue2:=0;
      end
    else
      begin
      FromQueue2:=Req.GetPoolIndex(DataRequest.Client)+1;
      FromQueue:=0;
      end;

    inherited;
    ReqCS.Acquire;
    try
      Req.PutDataRequest(DataRequest);
      Req.RemoveExpiredSessions(Now);
    finally
      ReqCS.Release;
      end;

    if not ReadyForNextRequest(FromQueue) then
      ReadyForNextRequest(FromQueue2);
    end
  else
    begin
    // We will be using "random" to avoid prioritizing sticky sessions to
    // give non-Sessioned Requests a chance to get through during "peek times"
    if random(2)>0 then
      begin
      FromQueue:=Req.GetPoolIndex(DataRequest.Client)+1;
      FromQueue2:=0;
      end
    else
      begin
      FromQueue2:=Req.GetPoolIndex(DataRequest.Client)+1;
      FromQueue:=0;
      end;

    { DataRequest object returned, check if there are Requests
      waiting and Post one using this DataRequest object. }
    if PostNextRequest(DataRequest,FromQueue) then
      inherited Event_PostReturn(nil)
    else if PostNextRequest(DataRequest,FromQueue2) then
      inherited Event_PostReturn(nil)
    else
      begin
      // No requests waiting, place the DataRequest object back into our DataRequest object Pool ...
      inherited;
      ReqCS.Acquire;
      try
        Req.PutDataRequest(DataRequest);
        Req.RemoveExpiredSessions(Now);
      finally
        ReqCS.Release;
        end;
      if not ReadyForNextRequest(FromQueue) then
        ReadyForNextRequest(FromQueue2);
      end;
    end;
  end;

procedure TRtcLoadBalancer.Event_QueuedRequest(Sender: TRtcDataServer; AddedToQueue: integer; MovedToBottom: boolean);
  var
    ok:boolean;
    sid:RtcString;
  begin
  if ReqClosing then Exit;

  inherited;

  ReqCS.Acquire;
  try
    ok:= FindDataRequestID(Sender,sid,AddedToQueue)>=0;
  finally
    ReqCS.Release;
    end;

  if ok then // Server is now ready to post this request
    ReadyForNextRequest(AddedToQueue);
  end;

procedure TRtcLoadBalancer.Event_RequestBegin(Sender: TRtcDataClient);
  begin
  inherited;
  end;

procedure TRtcLoadBalancer.Event_RequestReceiveAbort(Sender: TRtcDataServer);
  begin
  inherited;
  end;

procedure TRtcLoadBalancer.Event_RequestReceived(Sender: TRtcDataServer; Content: TRtcRouterContentBody);
  procedure Prep;
    var
      ses:TRtcLoadBalancerSession;
    begin
    ses:=TRtcLoadBalancerSession.Create;
    try
      Event_RequestPrepareSessionID(Sender,Content,ses);
      Sender.Request.Info.asString['$sid']:=ses.ID;
      Sender.Request.Info.asBoolean['$sidclos']:=ses.Close;
      Sender.Request.Info.asBoolean['$prep']:=False;
    finally
      ses.Free;
      end;
    end;
  begin
  if assigned(Content) and (Sender.Request.Info.asBoolean['$prep']) then Prep;

  inherited;
  end;

procedure TRtcLoadBalancer.Event_RequestSendAbort(Sender: TRtcDataClient);
  begin
  inherited;
  end;

procedure TRtcLoadBalancer.Event_RequestSent(Sender: TRtcDataClient);
  begin
  inherited;
  end;

procedure TRtcLoadBalancer.Event_ResponseBegin(Sender: TRtcDataClient);
  var
    NeedContent:boolean;
    ses:TRtcLoadBalancerSession;
  begin
  Sender.Response.ManualRead := not ResponseBuffer;

  inherited;

  NeedContent:=False;
  Event_ResponseBodyHasSessionID(Sender,NeedContent);
  if NeedContent then
    begin
    Sender.Request.Info.asBoolean['$prep']:=True;
    Sender.Response.ManualRead:=False;
    end
  else
    begin
    ses:=TRtcLoadBalancerSession.Create;
    try
      Event_ResponsePrepareSessionID(Sender,nil,ses);
      if (ses.Close) and (ses.ID='') then
        ses.ID:=Sender.Info.asString['$sid'];
      Sender.Request.Info.asString['$sid']:=ses.ID;
      Sender.Request.Info.asBoolean['$sidclose']:=ses.Close;
    finally
      ses.Free;
      end;
    end;
  end;

procedure TRtcLoadBalancer.Event_ResponseReceiveAbort(Sender: TRtcDataClient);
  begin
  inherited;
  end;

procedure TRtcLoadBalancer.Event_ResponseReceived(Sender: TRtcDataClient; Content: TRtcRouterContentBody);
  var
    sid:RtcString;
    clos:boolean;
    idx:integer;
    Con:TRtcClientConPool;
    ses:TRtcLoadBalancerSession;
  begin
  if assigned(Content) and Sender.Request.Info.asBoolean['$prep'] then // SessionID expected
    begin
    ses:=TRtcLoadBalancerSession.Create;
    try
      Event_ResponsePrepareSessionID(Sender,Content,ses);
      if ses.Close and (ses.ID='') then
        ses.ID:=Sender.Info.asString['$sid'];
      Sender.Request.Info.asString['$sid']:=ses.ID;
      Sender.Request.Info.asBoolean['$prep']:=False;
      Sender.Request.Info.asBoolean['$sidclose']:=ses.Close;
      clos:=ses.Close;
      sid:=ses.ID;
    finally
      ses.Free;
      end;
    end
  else
    begin
    sid:=Sender.Request.Info.asString['$sid'];
    clos:=Sender.Request.Info.asBoolean['$sidclos'];
    end;

  if sid='' then
    begin
    sid:=Sender.Info.asString['$sid'];
    clos:=False;
    end;

  if sid<>'' then
    begin
    ReqCS.Acquire;
    try
      idx:=Req.HaveSession(sid);

      if idx<0 then // SessionID does not exist yet
        idx:=Req.GetPoolIndex(Sender) // we will link Session ID to our Pool
      else if idx<>req.GetPoolIndex(Sender) then // SessionID exists on a different Server?
        idx:=-1; // That can't be right. SessionIDs have to be unique across all Servers.

      if idx>=0 then
        begin
        Con:=TRtcClientConPool(Req.Pool[idx]);
        if assigned(Con) then
          begin
          if clos then
            Con.CloseSession(sid)
          else if Con.SessionTimeout=0 then
            Con.OpenSession(sid,Now+1) // Sessions with no timeout will expire 1 day after being created
          else if not Con.OpenSession(sid,Now+Con.SessionTimeout) then
            Con.UpdateSessionIfExpiresBefore(sid, Now+Con.SessionTimeout, Now+Con.SessionTimeout*2);
          end;
        end;
    finally
      ReqCS.Release;
      end;
    end;

  inherited;
  end;

procedure TRtcLoadBalancer.Event_ResponseSendAbort(Sender: TRtcDataServer);
  begin
  inherited;
  end;

procedure TRtcLoadBalancer.Event_ResponseSent(Sender: TRtcDataServer);
  begin
  inherited;
  end;

constructor TRtcLoadBalancer.Create(AOwner: TComponent);
  begin
  inherited;
  ReqCS:=TRtcCritSec.Create;
  Req:=TRtcMultiClientPool.Create;
  ReqClosing:=False;
  FLastIDX:=0;
  FBalanceType:=lbt_SessionBalance;
  FRequestOrder:=lbo_FIFO;
  end;

destructor TRtcLoadBalancer.Destroy;
  begin
  RtcFreeAndNil(Req);
  RtcFreeAndNil(ReqCS);
  inherited;
  end;

function TRtcLoadBalancer.AddServer(const ServerAddr, ServerPort, RootURI: RtcString;
      MaxConnections, MaxSessions: integer; SessionTimeout: TDateTime;
      ServerIPV:RtcIPV=rtc_IPVDefault):integer;
  var
    Con:TRtcClientConPool;
  begin
  ReqCS.Acquire;
  try
    Con:=TRtcClientConPool.Create(64);
    Con.FServerAddr:=ServerAddr;
    Con.FServerPort:=ServerPort;
    Con.FServerIPV:=ServerIPV;
    Con.MaxReqCount:=MaxConnections;
    Con.MaxSessionCount:=MaxSessions;
    Con.SessionTimeout:=SessionTimeout;
    Con.FOnNewClient:=Event_PrepareConnection;
    Con.FRootURI:=RootURI;
    if (RootURI='') or (RootURI='/') then
      Con.FRootURI:=''
    else
      begin
      if Copy(Con.FRootURI,1,1)<>'/' then
        Con.FRootURI:='/'+Con.FRootURI;
      if Copy(Con.FRootURI,length(Con.FRootURI),1)='/' then
        Delete(Con.FRootURI,length(Con.FRootURI),1);
      end;
    Result:=Req.AddPool(Con);
  finally
    ReqCS.Release;
    end;
  end;

procedure TRtcLoadBalancer.UpdateServerState(const ServerAddr, ServerPort: RtcString; ServerEnabled:boolean);
  var
    Con:TRtcClientConPool;
    i:integer;
  begin
  ReqCS.Acquire;
  try
    for i:=0 to Req.PoolCount-1 do
      begin
      Con:=TRtcClientConPool(Req.Pool[i]);
      if ((ServerAddr='') or Same_Text(Con.FServerAddr,ServerAddr)) and
         ((ServerPort='') or Same_Text(Con.FServerPort,ServerPort)) then
        Con.Disabled:=not ServerEnabled;
      end;
  finally
    ReqCS.Release;
    end;
  end;

function TRtcLoadBalancer.IsServerEnabled(const ServerAddr, ServerPort: RtcString):boolean;
  var
    Con:TRtcClientConPool;
    i:integer;
    found:boolean;
  begin
  Result:=True;
  found:=False;
  ReqCS.Acquire;
  try
    for i:=0 to Req.PoolCount-1 do
      begin
      Con:=TRtcClientConPool(Req.Pool[i]);
      if ((ServerAddr='') or Same_Text(Con.FServerAddr,ServerAddr)) and
         ((ServerPort='') or Same_Text(Con.FServerPort,ServerPort)) then
        begin
        found:=True;
        if Con.Disabled then
          Result:=False;
        end;
      end;
  finally
    ReqCS.Release;
    end;
  if not found then Result:=False;
  end;

function TRtcLoadBalancer.ServerStateCount(const ServerAddr, ServerPort: RtcString; ServerEnabled: boolean): integer;
  var
    Con:TRtcClientConPool;
    i:integer;
  begin
  Result:=0;
  ReqCS.Acquire;
  try
    for i:=0 to Req.PoolCount-1 do
      begin
      Con:=TRtcClientConPool(Req.Pool[i]);
      if ((ServerAddr='') or Same_Text(Con.FServerAddr,ServerAddr)) and
         ((ServerPort='') or Same_Text(Con.FServerPort,ServerPort)) then
        if Con.Disabled=not ServerEnabled then
          Inc(Result);
      end;
  finally
    ReqCS.Release;
    end;
  end;

function TRtcLoadBalancer.ServerIndex(Sender: TRtcDataClient): integer;
  begin
  // "GetIndexPool" method is thread-safe, if can even be  used from multiple threads at the same time
  Result := Req.GetPoolIndex(Sender);
  end;

function TRtcLoadBalancer.HaveSession(SessionID: RtcString): integer;
  begin
  ReqCS.Acquire;
  try
    Result := Req.HaveSession(SessionID);
  finally
    ReqCS.Release;
    end;
  end;

function TRtcLoadBalancer.UpdateSession(SessionID: RtcString): integer;
  var
    Con:TRtcClientConPool;
  begin
  ReqCS.Acquire;
  try
    Result:=Req.HaveSession(SessionID);
    if Result>=0 then
      begin
      Con:=TRtcClientConPool(Req.Pool[Result]);
      if Con.SessionTimeout>0 then
        Con.UpdateSessionIfExpiresBefore(SessionID, Now + Con.SessionTimeout, Now + Con.SessionTimeout*2);
      end;
  finally
    ReqCS.Release;
    end;
  end;

function TRtcLoadBalancer.ServerReady(index: integer): boolean;
  var
    Con:TRtcClientConPool;
  begin
  ReqCS.Acquire;
  try
    if (index>=0) and (index<Req.PoolCount) then
      begin
      Con:=TRtcClientConPool(Req.Pool[index]);
      Result:= not Con.Disabled and
               ( (Con.MaxSessionCount=0) or (Con.ActiveSessions<Con.MaxSessionCount) ) and
               ( (Con.MaxReqCount=0) or (Con.ActiveClients<Con.MaxReqCount) );
      end
    else
      Result:=False;
  finally
    ReqCS.Release;
    end;
  end;

function TRtcLoadBalancer.GetRequestSessionID(Sender: TRtcDataServer): RtcString;
  begin
  if Sender<>nil then
    Result := Sender.Request.Info.asString['$sid']
  else
    Result := '';
  end;

procedure TRtcLoadBalancer.GetDataRequestQueue(Sender:TRtcDataServer; var AddToQueue:integer);
  var
    idx:integer;
    sid:RtcString;
  begin
  if Sender<>nil then
    begin
    sid:=GetRequestSessionID(Sender);
    if sid<>'' then
      begin
      ReqCS.Acquire;
      try
        idx:=Req.HaveSession(sid);
      finally
        ReqCS.Release;
        end;
      if idx>=0 then
        AddToQueue:=idx+1;
      end;
    end;
  end;

function TRtcLoadBalancer.FindDataRequestID(Sender:TRtcDataServer; var sid:RtcString; var AddToQueue:integer):integer;
  var
    a,idx,
    cnt1,cnt2,
    cnt3,cnt4:integer;
    found:boolean;
    con:TRtcClientConPool;
  begin
  Result:=-1;
  if Sender<>nil then
    begin
    sid:=GetRequestSessionID(Sender);
    if sid<>'' then
      begin
      idx:=Req.HaveSession(sid);
      if idx>=0 then
        begin
        AddToQueue:=idx+1;
        Con:=TRtcClientConPool(Req.Pool[idx]);
        if not Con.Disabled and
           ((Con.MaxSessionCount=0) or (Con.ActiveSessions<Con.MaxSessionCount)) and
           ((Con.MaxReqCount=0) or (Con.ActiveClients<Con.MaxReqCount)) then
          begin
          if Con.SessionTimeout>0 then
            Con.UpdateSessionIfExpiresBefore(sid, Now + Con.SessionTimeout, Now + Con.SessionTimeout*2);
          Result:=idx;
          end
        else
          Exit; // Result:=-1
        end;
      end
    else
      idx:=-1;
    end
  else
    begin
    sid:='';
    idx:=-1;
    end;

  if idx<0 then
    begin
    idx:=FLastIDX+1;
    if idx>=Req.PoolCount then idx:=0;

    Con:=TRtcClientConPool(Req.Pool[idx]);
    if Con.Disabled or
       ((Con.MaxSessionCount>0) and (Con.ActiveSessions>=Con.MaxSessionCount)) or
       ((Con.MaxReqCount>0) and (Con.ActiveClients>=Con.MaxReqCount)) then
      idx:=-1;

    Result:=idx;

    case FBalanceType of
      lbt_RequestBalance:
        begin
        Con:=TRtcClientConPool(Req.Pool[idx]);
        if assigned(Con) then
          begin
          cnt1:=Con.ActiveClients;
          cnt2:=Con.MaxReqCount;
          end
        else
          begin
          cnt1:=-1;
          cnt2:=-1;
          end;

        for a:=0 to Req.PoolCount-1 do
          begin
          found:=False;
          Con:=TRtcClientConPool(Req.Pool[a]);

          if not Con.Disabled and
            ((Con.MaxReqCount=0) or (Con.ActiveClients<Con.MaxReqCount)) then
            begin
            if (cnt2=0) or (Con.MaxReqCount=0) then
              found := cnt1>Con.ActiveClients
            else
              found:= (cnt1/cnt2)>(Con.ActiveClients/Con.MaxReqCount);
            end;

          if found then
            begin
            idx:=a;
            Result:=idx;
            cnt1:=Con.ActiveClients;
            cnt2:=Con.MaxReqCount;
            end;
          end;

        end;

      lbt_SessionBalance:
        begin
        Con:=TRtcClientConPool(Req.Pool[idx]);
        if assigned(Con) then
          begin
          cnt1:=Con.ActiveClients;
          cnt2:=Con.MaxReqCount;
          cnt3:=Con.ActiveSessions;
          cnt4:=Con.MaxSessionCount;
          end
        else
          begin
          cnt1:=-1;
          cnt2:=-1;
          cnt3:=-1;
          cnt4:=-1;
          end;

        for a:=0 to Req.PoolCount-1 do
          begin
          found:=False;
          Con:=TRtcClientConPool(Req.Pool[a]);

          if not Con.Disabled and
            ((Con.MaxSessionCount=0) or (Con.ActiveSessions<Con.MaxSessionCount)) then
            begin
            if (cnt4=0) or (Con.MaxSessionCount=0) then
              found := cnt3>Con.ActiveSessions
            else
              found:= (cnt3/cnt4)>(Con.ActiveSessions/Con.MaxSessionCount);

            if not found and (cnt3=Con.ActiveSessions) then
              begin
              if not Con.Disabled and
                 ((Con.MaxReqCount=0) or (Con.ActiveClients<Con.MaxReqCount)) then
                begin
                if cnt2=0 then
                  found := cnt1>Con.ActiveClients
                else
                  begin
                  if Con.MaxReqCount=0 then
                    found:= cnt1>Con.ActiveClients
                  else
                    found:= (cnt1/cnt2)>(Con.ActiveClients/Con.MaxReqCount);
                  end;
                end;
              end;
            end;

          if found then
            begin
            idx:=a;
            Result:=idx;
            cnt1:=Con.ActiveClients;
            cnt2:=Con.MaxReqCount;
            cnt3:=Con.ActiveSessions;
            cnt4:=Con.MaxSessionCount;
            end;
          end;

        end;
      end;
    end;
  end;

procedure TRtcLoadBalancer.GetDataRequest(Sender:TRtcDataServer; var DataRequest: TRtcDataRequest; var AddToQueue:integer);
  var
    idx:integer;
    con:TRtcClientConPool;
    sid:RtcString;
  begin
  if ReqClosing then Exit;

  ReqCS.Acquire;
  try
    idx:=FindDataRequestID(Sender,sid,AddToQueue);
    if idx>=0 then
      begin
      FLastIDX:=idx;
      Con:=TRtcClientConPool(Req.Pool[idx]);
      DataRequest:=Con.GetDataRequest;
      if assigned(DataRequest) then
        { Store the Session ID in the "sid" variable,
          so we can use it when we receive the Response
          (for example, to close the Session if expired). }
        DataRequest.Client.Info.asString['$sid']:=sid;
      end;
  finally
    ReqCS.Release;
    end;
  end;

procedure TRtcLoadBalancer.Event_RequestBodyHasSessionID(Sender: TRtcDataServer; var NeedContent: boolean);
  begin
  if assigned(FRequestBodyHasSessionID) then
    FRequestBodyHasSessionID(Sender,NeedContent);
  end;

procedure TRtcLoadBalancer.Event_RequestPrepareSessionID(Sender: TRtcDataServer; Content: TRtcRouterContentBody; Session:TRtcLoadBalancerSession);
  begin
  if assigned(FRequestPrepareSessionID) then
    FRequestPrepareSessionID(Sender,Content,Session);
  end;

procedure TRtcLoadBalancer.Event_ResponseBodyHasSessionID(Sender: TRtcDataClient; var NeedContent: boolean);
  begin
  if assigned(FResponseBodyHasSessionID) then
    FResponseBodyHasSessionID(Sender,NeedContent);
  end;

procedure TRtcLoadBalancer.Event_ResponsePrepareSessionID(Sender: TRtcDataClient; Content: TRtcRouterContentBody; Session:TRtcLoadBalancerSession);
  begin
  if assigned(FResponsePrepareSessionID) then
    FResponsePrepareSessionID(Sender,Content,Session);
  end;

procedure TRtcLoadBalancer.Event_PostCheckOrder(Sender: TRtcDataServer; var RequestOrder: TRtcLoadBalanceOrder);
  begin
  if assigned(FOnPostCheckOrder) then
    FOnPostCheckOrder(Sender,RequestOrder);
  end;

function TRtcLoadBalancer.ActiveClients: integer;
  begin
  ReqCS.Acquire;
  try
    Result:=Req.ActiveClients;
  finally
    ReqCS.Release;
    end;
  end;

function TRtcLoadBalancer.IdleClients: integer;
  begin
  ReqCS.Acquire;
  try
    Result:=Req.IdleClients;
  finally
    ReqCS.Release;
    end;
  end;

function TRtcLoadBalancer.ActiveSessions: integer;
  begin
  ReqCS.Acquire;
  try
    Result:=Req.ActiveSessions;
  finally
    ReqCS.Release;
    end;
  end;

procedure TRtcLoadBalancer.ClosePool;
  begin
  ReqCS.Acquire;
  try
    Req.CloseAll;
  finally
    ReqCS.Release;
    end;
  end;

procedure TRtcLoadBalancer.ReleasePool;
  begin
  ReqCS.Acquire;
  try
    RtcFreeAndNil(Req);
    Req:=TRtcMultiClientPool.Create;
  finally
    ReqCS.Release;
    end;
  end;

function TRtcLoadBalancer.GetBalanceDebugInfo: RtcString;
  var
    a:integer;
    Con:TRtcClientConPool;
    enb:RtcString;
  begin
  ReqCS.Acquire;
  try
    Result:='';
    for a:=0 to Req.PoolCount-1 do
      begin
      Con:=TRtcClientConPool(Req.Pool[a]);
      if Con.Disabled then enb:='Disabled' else enb:='Enabled';
      Result:=Result+'Server['+Int2Str(a)+'] '+Con.FServerAddr+':'+Con.FServerPort+Con.FRootURI+'/ ('+enb+') = '+
               Int2Str(Con.ActiveClients+Con.IdleClients)+
               ' Clients (Max='+Int2Str(Con.MaxReqCount)+', Active='+Int2Str(Con.ActiveClients)+', Idle='+Int2Str(Con.IdleClients)+') / '+
               Int2Str(Req.Pool[a].ActiveSessions)+' Sessions (Max='+Int2Str(Con.MaxSessionCount)+') <br>'+#13#10;
      end;
  finally
    ReqCS.Release;
    end;
  end;

function TRtcLoadBalancer.ServerList: RtcString;
  var
    a:integer;
    Con:TRtcClientConPool;
  begin
  ReqCS.Acquire;
  try
    Result:='';
    for a:=0 to Req.PoolCount-1 do
      begin
      Con:=TRtcClientConPool(Req.Pool[a]);
      if Result<>'' then
        Result:=Result+#13#10+Con.FServerAddr+':'+Con.FServerPort
      else
        Result:=Con.FServerAddr+':'+Con.FServerPort;
      end;
  finally
    ReqCS.Release;
    end;
  end;

function TRtcLoadBalancer.ServerCount: integer;
  begin
  ReqCS.Acquire;
  try
    Result:=Req.PoolCount;
  finally
    ReqCS.Release;
    end;
  end;

function TRtcLoadBalancer.GetActive: boolean;
  begin
  Result:=not ReqClosing;
  end;

procedure TRtcLoadBalancer.SetActive(const Value: boolean);
  begin
  ReqClosing:=not Value;
  end;

procedure TRtcLoadBalancer.SetVirtualHost(const Value: RtcString);
  begin
  FVirtualHostCS := Value;
  FVirtualHost := Upper_Case(Value);
  end;

procedure TRtcLoadBalancer.SetVirtualPath(const Value: RtcString);
  begin
  if Value='' then
    begin
    FVirtualPathLen:=0;
    FVirtualPath:='';
    FVirtualPath2:='';
    FVirtualPathCS:='';
    end
  else if Value='/' then
    begin
    FVirtualPathLen:=1;
    FVirtualPath:='/';
    FVirtualPath2:='/';
    FVirtualPathCS:='/';
    end
  else
    begin
    FVirtualPathLen := length(Value);
    FVirtualPathCS := Value;
    if Copy(FVirtualPathCS,1,1)<>'/' then
      FVirtualPathCS:='/'+FVirtualPathCS;

    FVirtualPath := Upper_Case(FVirtualPathCS);
    if Copy(FVirtualPath,length(FVirtualPath),1)='/' then
      begin
      FVirtualPath2:=FVirtualPath;
      if FVirtualPathLen>1 then
        begin
        Delete(FVirtualPath,FVirtualPathLen,1);
        Dec(FVirtualPathLen);
        end;
      end
    else
      FVirtualPath2:=FVirtualPath+'/';
    end;
  end;

procedure TRtcLoadBalancer.Client_WriteHeader(Sender: TRtcDataClient);
  var
    uri:RtcString;
  begin
  if Sender.Info.asString['$root']='' then
    inherited
  else
    begin
    { We will inject "RootURI" to Request.FileName just
      before calling "WriteHeader" and remove it afterwards. }
    uri:=Sender.Request.FileName;
    if uri='/' then
      Sender.Request.FileName:=Sender.Info.asString['$root']
    else
      Sender.Request.FileName:=Sender.Info.asString['$root']+uri;
    inherited;
    Sender.Request.FileName:=uri;
    end;
  end;

class function TRtcLoadBalancer.GetLoadBalancer(Sender: TRtcDataClient): TRtcLoadBalancer;
  begin
  if assigned(Sender.Info.asPtr['$owner']) then
    Result:=TRtcLoadBalancer(Sender.Info.asPtr['$owner'])
  else
    Result:=nil;
  end;

class function TRtcLoadBalancer.GetRootURI(Sender: TRtcDataClient): RtcString;
  begin
  Result:=Sender.Info.asString['$root'];
  end;

class function TRtcLoadBalancer.GetVirtualPath(Sender: TRtcDataClient): RtcString;
  begin
  if assigned(Sender.Info.asPtr['$owner']) then
    Result:=TRtcLoadBalancer(Sender.Info.asPtr['$owner']).VirtualPath
  else
    Result:='';
  end;

{$IFDEF RTC_ANON_METHODS}

// procedure(Sender:TRtcDataServer; var NeedContent:boolean)
type
  TRtcLoadBalancerRequestSessionBodyAMContainer = class(TObject)
  public
    MyMethod:TRtcLoadBalancerRequestSessionBodyAnonMethod;
    constructor Create(const AMethod:TRtcLoadBalancerRequestSessionBodyAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcDataServer; var NeedContent:boolean);
    end;

constructor TRtcLoadBalancerRequestSessionBodyAMContainer.Create(const AMethod:TRtcLoadBalancerRequestSessionBodyAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcLoadBalancerRequestSessionBodyAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcLoadBalancerRequestSessionBodyAMContainer.MyEvent(Sender:TRtcDataServer; var NeedContent:boolean);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender, NeedContent);
  end;

function TRtcLoadBalancer.Anon(const Event:TRtcLoadBalancerRequestSessionBodyAnonMethod):TRtcLoadBalancerRequestSessionBodyEvent;
  var
    obj:TRtcLoadBalancerRequestSessionBodyAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcLoadBalancerRequestSessionBodyAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(Sender:TRtcDataClient; var NeedContent:boolean)
type
  TRtcLoadBalancerResponseSessionBodyAMContainer = class(TObject)
  public
    MyMethod:TRtcLoadBalancerResponseSessionBodyAnonMethod;
    constructor Create(const AMethod:TRtcLoadBalancerResponseSessionBodyAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcDataClient; var NeedContent:boolean);
    end;

constructor TRtcLoadBalancerResponseSessionBodyAMContainer.Create(const AMethod:TRtcLoadBalancerResponseSessionBodyAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcLoadBalancerResponseSessionBodyAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcLoadBalancerResponseSessionBodyAMContainer.MyEvent(Sender:TRtcDataClient; var NeedContent:boolean);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender, NeedContent);
  end;

function TRtcLoadBalancer.Anon(const Event:TRtcLoadBalancerResponseSessionBodyAnonMethod):TRtcLoadBalancerResponseSessionBodyEvent;
  var
    obj:TRtcLoadBalancerResponseSessionBodyAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcLoadBalancerResponseSessionBodyAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(Sender:TRtcDataServer; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession)
type
  TRtcLoadBalancerRequestSessionSetAMContainer = class(TObject)
  public
    MyMethod:TRtcLoadBalancerRequestSessionSetAnonMethod;
    constructor Create(const AMethod:TRtcLoadBalancerRequestSessionSetAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcDataServer; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession);
    end;

constructor TRtcLoadBalancerRequestSessionSetAMContainer.Create(const AMethod:TRtcLoadBalancerRequestSessionSetAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcLoadBalancerRequestSessionSetAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcLoadBalancerRequestSessionSetAMContainer.MyEvent(Sender:TRtcDataServer; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender, Content, Session);
  end;

function TRtcLoadBalancer.Anon(const Event:TRtcLoadBalancerRequestSessionSetAnonMethod):TRtcLoadBalancerRequestSessionSetEvent;
  var
    obj:TRtcLoadBalancerRequestSessionSetAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcLoadBalancerRequestSessionSetAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(Sender:TRtcDataClient; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession)
type
  TRtcLoadBalancerResponseSessionSetAMContainer = class(TObject)
  public
    MyMethod:TRtcLoadBalancerResponseSessionSetAnonMethod;
    constructor Create(const AMethod:TRtcLoadBalancerResponseSessionSetAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcDataClient; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession);
    end;

constructor TRtcLoadBalancerResponseSessionSetAMContainer.Create(const AMethod:TRtcLoadBalancerResponseSessionSetAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcLoadBalancerResponseSessionSetAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcLoadBalancerResponseSessionSetAMContainer.MyEvent(Sender:TRtcDataClient; Content:TRtcRouterContentBody; Session:TRtcLoadBalancerSession);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender, Content, Session);
  end;

function TRtcLoadBalancer.Anon(const Event:TRtcLoadBalancerResponseSessionSetAnonMethod):TRtcLoadBalancerResponseSessionSetEvent;
  var
    obj:TRtcLoadBalancerResponseSessionSetAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcLoadBalancerResponseSessionSetAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(Sender:TRtcDataServer; var RequestOrder:TRtcLoadBalanceOrder)
type
  TRtcLoadBalancerRequestOrderAMContainer = class(TObject)
  public
    MyMethod:TRtcLoadBalancerRequestOrderAnonMethod;
    constructor Create(const AMethod:TRtcLoadBalancerRequestOrderAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcDataServer; var RequestOrder:TRtcLoadBalanceOrder);
    end;

constructor TRtcLoadBalancerRequestOrderAMContainer.Create(const AMethod:TRtcLoadBalancerRequestOrderAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcLoadBalancerRequestOrderAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcLoadBalancerRequestOrderAMContainer.MyEvent(Sender:TRtcDataServer; var RequestOrder:TRtcLoadBalanceOrder);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender, RequestOrder);
  end;

function TRtcLoadBalancer.Anon(const Event:TRtcLoadBalancerRequestOrderAnonMethod):TRtcLoadBalancerRequestOrderEvent;
  var
    obj:TRtcLoadBalancerRequestOrderAMContainer;
    o:TObject absolute obj;
  begin
  if not assigned(Event) then
    Result:=nil
  else
    begin
    FAMCS.Acquire;
    try
      if FAMethods=nil then
        FAMethods:=tObjList.Create(8);
      o:=FAMethods.search(MethRefToRtcPtr(Event));
      if not assigned(o) then
        begin
        obj:=TRtcLoadBalancerRequestOrderAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

{$ENDIF}

end.
