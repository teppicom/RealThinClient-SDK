{
  @html(<b>)
  Data Router component
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit implements the TRtcDataRouter component, which can be used
  for writing HTTP/S Routers, Proxy Servers and Load Balancers.
}
unit rtcDataRoute;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSrcList,
  rtcSystem,
  rtcLog,

  rtcInfo,
  rtcConn,
  rtcDataCli,
  rtcDataSrv;

const
  RLOG_EVENT='route';
  RLOG_PROXY='proxy';
  RLOG_ROUTE='route';

type
  { @abstract(RTC "Route Data" class) }
  TRtcRouteObject = class(TRtcDataObject);

  { @abstract(Used for sending Content Body to allow modifications
    before the Content is forwarded to the other side) }
  TRtcRouterContentBody = class(TRtcRouteObject)
  protected
    FBody:RtcByteArray;

    function GetBodyTxt:RtcString;
    procedure SetBodyTxt(Value:RtcString);

  public
    property BodyEx:RtcByteArray read FBody write FBody;
    property Body:RtcString read GetBodyTxt write SetBodyTxt;
    end;

  { @abstract(Used for sending Debug information) }
  TRtcRouterDebugInfo = class(TRtcRouteObject)
  public
    Text:RtcString;
    Name:String;
    end;

  // Genera-purpose Data Router Notification event
  TRtcRouterClientEvent = procedure(Sender:TRtcDataClient) of object;
  // Genera-purpose Data Router Notification event
  TRtcRouterServerEvent = procedure(Sender:TRtcDataServer) of object;

  // "Request Received" Data Router Notification event
  TRtcRouterRequestReceivedEvent = procedure(Sender:TRtcDataServer; Content:TRtcRouterContentBody) of object;
  // "Response Received" Data Router Notification event
  TRtcRouterResponseReceivedEvent = procedure(Sender:TRtcDataClient; Content:TRtcRouterContentBody) of object;

  // Event used by Data Router to ask for a DataRequest component
  TRtcRouterGetConnectionEvent = procedure(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; var AddToQueue:integer; var MoveToBottom:boolean) of object;
  // Event used by Data Router to notify about a Requst being placed in the Queue
  TRtcRouterQueConnectionEvent = procedure(Sender:TRtcDataServer; AddedToQueue:integer; MovedToBottom:boolean) of object;
  // Event used by Data Router to notify that a DataRequst component is no longer used
  TRtcRouterPutConnectionEvent = procedure(DataRequest:TRtcDataRequest) of object;

  // Event used by Data Router for Debug logging
  TRtcRouterDebugLogEvent = procedure(Info:TRtcRouterDebugInfo) of object;

{$IFDEF RTC_ANON_METHODS}

  // Genera-purpose Data Router Notification event
  TRtcRouterClientAnonMethod = reference to procedure(Sender:TRtcDataClient);
  // Genera-purpose Data Router Notification event
  TRtcRouterServerAnonMethod = reference to procedure(Sender:TRtcDataServer);

  // "Request Received" Data Router Notification event
  TRtcRouterRequestReceivedAnonMethod = reference to procedure(Sender:TRtcDataServer; Content:TRtcRouterContentBody);
  // "Response Received" Data Router Notification event
  TRtcRouterResponseReceivedAnonMethod = reference to procedure(Sender:TRtcDataClient; Content:TRtcRouterContentBody);

  // Event used by Data Router to ask for a DataRequest component
  TRtcRouterGetConnectionAnonMethod = reference to procedure(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; var AddToQueue:integer; var MoveToBottom:boolean);
  // Event used by Data Router to notify about a Requst being placed in the Queue
  TRtcRouterQueConnectionAnonMethod = reference to procedure(Sender:TRtcDataServer; AddedToQueue:integer; MovedToBottom:boolean);
  // Event used by Data Router to notify that a DataRequst component is no longer used
  TRtcRouterPutConnectionAnonMethod = reference to procedure(DataRequest:TRtcDataRequest);

  // Event used by Data Router for Debug logging
  TRtcRouterDebugLogAnonMethod = reference to procedure(Info:TRtcRouterDebugInfo);

{$ENDIF}

  // @exclude
  TRtcProxyData = class;

  { @abstract(The main RTC Data Router class, making the task of routing
    requests and responses between HTTP/S Clients and Servers easy) }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcDataRouter = class(TRtcAbsDataServerLink)
  private
    ProxyDataCS:TRtcCritSec;

    QueueCnt:integer;
    My_Queues:array of TXObjList;
    My_Queued:array of Cardinal;

    Total_Queued,

    TotalCreated,
    TotalPending,
    CurrentNr:Cardinal;

    FOnCheckRequest: TRtcRouterServerEvent;

    FOnPostNewRequest: TRtcRouterGetConnectionEvent;
    FOnPostOldRequest: TRtcRouterGetConnectionEvent;
    FOnQueuedRequest: TRtcRouterQueConnectionEvent;
    FOnPostReturn: TRtcRouterPutConnectionEvent;

    FOnRequestBegin: TRtcRouterClientEvent;
    FOnRequestReceived: TRtcRouterRequestReceivedEvent;
    FOnRequestSent: TRtcRouterClientEvent;

    FOnResponseBegin: TRtcRouterClientEvent;
    FOnResponseReceived: TRtcRouterResponseReceivedEvent;
    FOnResponseSent: TRtcRouterServerEvent;

    FOnRequestReceiveAbort: TRtcRouterServerEvent;
    FOnRequestSendAbort: TRtcRouterClientEvent;
    FOnResponseReceiveAbort: TRtcRouterClientEvent;
    FOnResponseSendAbort: TRtcRouterServerEvent;

    FOnDebugLog: TRtcRouterDebugLogEvent;

    FPostReturnBeforeResponseSent: boolean;

    FTimeout_RequestReceived: integer;
    FTimeout_ResponseBegin: integer;
    FTimeout_QueuedRequest: integer;
    FTimeout_CheckRequest: integer;
    FTimeout_PostNewRequest: integer;
    FTimeout_RequestBegin: integer;
    FTimeout_RequestSent: integer;
    FTimeout_PostOldRequest: integer;
    FTimeout_ResponseReceived: integer;
    FTimeout_ResponseSent: integer;
    FTimeout_RequestDataIn: integer;
    FTimeout_ResponseDataIn: integer;
    FTimeout_RequestDataOut: integer;
    FTimeout_ResponseDataOut: integer;

  protected
    // @exclude
    FOnListenStart: TRtcNotifyEvent;
    // @exclude
    FOnListenStop: TRtcNotifyEvent;
    // @exclude
    FOnSessionOpen: TRtcNotifyEvent;
    // @exclude
    FOnSessionClose: TRtcNotifyEvent;

    // @exclude
    procedure Proxy_Clear(pd:TRtcProxyData; Evnt:TRtcNotifyEvent; _done:boolean);

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
    procedure Call_DataSent(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_Disconnect(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

    // @exclude
    function Proxy_PostRequest(Srv:TRtcDataServer; const _data:RtcByteArray; _notify:boolean):TRtcProxyData;

    // @exclude
    procedure Server_PostRequest(Sender: TRtcConnection);

    // @exclude
    procedure ClientCanRead(Sender: TRtcConnection);
    // @exclude
    procedure ClientCanWrite(Sender: TRtcConnection);
    // @exclude
    procedure ClientStop(Sender: TRtcConnection);

    // @exclude
    procedure ClientWSCanRead(Sender: TRtcConnection);
    // @exclude
    procedure ClientWSCanWrite(Sender: TRtcConnection);

    // @exclude
    procedure ServerCanRead(Sender: TRtcConnection);
    // @exclude
    procedure ServerCanWrite(Sender: TRtcConnection);
    // @exclude
    procedure ServerCanWriteFirst(Sender: TRtcConnection);
    // @exclude
    procedure ServerWriteDone(Sender: TRtcConnection);
    // @exclude
    procedure ServerStop(Sender: TRtcConnection);

    // @exclude
    procedure ServerWSCanRead(Sender: TRtcConnection);
    // @exclude
    procedure ServerWSCanWrite(Sender: TRtcConnection);

    // @exclude
    procedure Client_BeginRequest(Sender: TRtcConnection);
    // @exclude
    procedure Client_DataSent(Sender: TRtcConnection);
    // @exclude
    procedure Client_DataReceived(Sender: TRtcConnection);
    // @exclude
    procedure Client_RepostCheck(Sender: TRtcConnection);
    // @exclude
    procedure Client_ResponseDone(Sender: TRtcConnection);

    // @exclude
    procedure Client_WriteHeader(Sender:TRtcDataClient); virtual;

    // @exclude
    procedure Event_CheckRequest(Sender:TRtcDataServer); virtual;

    // @exclude
    procedure Event_PostNewRequest(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; var AddToQueue:integer; var MoveToBottom:boolean); virtual;
    // @exclude
    procedure Event_PostOldRequest(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; var AddToQueue:integer; var MoveToBottom:boolean); virtual;
    // @exclude
    procedure Event_QueuedRequest(Sender:TRtcDataServer; AddedToQueue:integer; MovedToBottom:boolean); virtual;
    // @exclude
    procedure Event_PostReturn(DataRequest:TRtcDataRequest); virtual;

    // @exclude
    procedure Event_RequestBegin(Sender:TRtcDataClient); virtual;
    // @exclude
    procedure Event_RequestReceived(Sender:TRtcDataServer; Content:TRtcRouterContentBody); virtual;
    // @exclude
    procedure Event_RequestSent(Sender:TRtcDataClient); virtual;

    // @exclude
    procedure Event_ResponseBegin(Sender:TRtcDataClient); virtual;
    // @exclude
    procedure Event_ResponseReceived(Sender:TRtcDataClient; Content:TRtcRouterContentBody); virtual;
    // @exclude
    procedure Event_ResponseSent(Sender:TRtcDataServer); virtual;

    // @exclude
    procedure Event_RequestReceiveAbort(Sender:TRtcDataServer); virtual;
    // @exclude
    procedure Event_RequestSendAbort(Sender:TRtcDataClient); virtual;
    // @exclude
    procedure Event_ResponseReceiveAbort(Sender:TRtcDataClient); virtual;
    // @exclude
    procedure Event_ResponseSendAbort(Sender:TRtcDataServer); virtual;

    // @exclude
    procedure Debug(const _text:RtcString; _name:String);

    // @exclude ("index" is the queue index which needs to exist)
    procedure ExpandQueue(index:integer);

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    { Call this method when you are ready to send one more request from Queue "FromQueue",
      but want to check Request parameters before you make the decision if you want to post
      the next request in line from the Queue or not. If there are Requests waiting in the
      Request queue "FromQueue", "OnPostOldRequest" event will be triggered using the first
      Request found in the Request Queue "FromQueue". If a Request was posted, the Result
      of "ReadyForNextRequest" will be TRUE. }
    function ReadyForNextRequest(FromQueue:integer):boolean;

    { Call this method to post the next Request from Queue with index "FromQueue" using "DataRequest" object.

      If there are no requests waiting in the Request Queue "FromQueue", return FALSE and do nothing.

      If at least one Request is currently waiting in the Request Queue "FromQueue", post the
      first topmost Request from the Request Queue "FromQueue" using "DataRequest" and return TRUE.

      This method can be used to Post the next request from the Queue "FromQueue" regardless
      of the Request contents. If you need to check request contents before deciding
      which component should be used to post it, use "ReadyForNextRequest" instead. }
    function PostNextRequest(DataRequest:TRtcDataRequest; FromQueue:integer):boolean;

    { Returns the number of Requests currently waiting in the Request Queue "FromQueue". }
    function WaitingRequests(FromQueue:integer):integer;

    { Return a String containing basic router debug info. }
    function GetDebugInfo:RtcString;

    { Use this method to clean up dead Request objects from all Request queues and to
      completely remove all Request Queues (clean-up memory) all Request Queues are empty. }
    procedure CleanUpQueues;

  {$IFDEF RTC_ANON_METHODS}

    { Get the Router Client Notification event for anonymous method
        procedure(Sender:TRtcDataClient) }
    function Anon(const Event:TRtcRouterClientAnonMethod):TRtcRouterClientEvent; overload;

    { Get the Router Server Notification event for anonymous method
        procedure(Sender:TRtcDataServer); }
    function Anon(const Event:TRtcRouterServerAnonMethod):TRtcRouterServerEvent; overload;

    { Get the "Request Received" Data Router Notification event for anonymous method
        procedure(Sender:TRtcDataServer; Content:TRtcRouterContentBody) }
    function Anon(const Event:TRtcRouterRequestReceivedAnonMethod):TRtcRouterRequestReceivedEvent; overload;

    { Get the "Response Received" Data Router Notification event for anonymous method
        procedure(Sender:TRtcDataClient; Content:TRtcRouterContentBody) }
    function Anon(const Event:TRtcRouterResponseReceivedAnonMethod):TRtcRouterResponseReceivedEvent; overload;

    { Get the Data Router requesting a DataRequest component for anonymous method
        procedure(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; var AddToQueue:integer; var MoveToBottom:boolean) }
    function Anon(const Event:TRtcRouterGetConnectionAnonMethod):TRtcRouterGetConnectionEvent; overload;

    { Get the Data Router event about a Requst being placed in the Queue for anonymous method
        procedure(Sender:TRtcDataServer; AddedToQueue:integer; MovedToBottom:boolean) }
    function Anon(const Event:TRtcRouterQueConnectionAnonMethod):TRtcRouterQueConnectionEvent; overload;

    { Get the Data Router event about a DataRequst component no longer used for anonymous method
        procedure(DataRequest:TRtcDataRequest) }
    function Anon(const Event:TRtcRouterPutConnectionAnonMethod):TRtcRouterPutConnectionEvent; overload;

    { Get the Data Router "Debug logging" event for anonymous method
        procedure(Info:TRtcRouterDebugInfo) }
    function Anon(const Event:TRtcRouterDebugLogAnonMethod):TRtcRouterDebugLogEvent; overload;

  {$ENDIF}

  published
    { Call OnPostReturn immediately after the Response is received from the Server (incomming) connection
      (once "DataRequest" object becomes idle), even if the Response was NOT sent yet. Doing this would improve
      performance when Requests are waiting in the Request queue, but it will also increase Router and Server load.
      This property can be changed at ANYTIME, even when the TRtcDataRouter component is actively used. 
      Doing so will result in all Requests posted *after* the change to use the new property setting. }
    property PostReturnBeforeResponseSent:boolean read FPostReturnBeforeResponseSent write FPostReturnBeforeResponseSent default False;

    { Incomming connection Timeout (seconds) after a Request from a Client was accepted. @html(<br>)
      Depending on the waiting Request Queue, it could take some time before the Request
      starts being forwarded to a Server, so you should NOT use too short timeout values here. @html(<br><br>)

      NOTE: For more precise Timeout control or if you want to set Timeouts differently depending on
      the Request or Response contents, you can leave all "Timeout" properties at their default values (0)
      and directly call the "Sender.Timeout.Enable(time)" method from inside events triggered by this component. }
    property TimeoutAfterCheckRequestI:integer read FTimeout_CheckRequest write FTimeout_CheckRequest default 0;

    { Incomming connection Timeout (seconds) after a NEW Request from a Client was successfully posted,
      after a call to OnPostNewRequest has returned a TRtcDataRequest component linked to a TRtcDataClient. @html(<br>)
      This is the first step to sending a Request to the Server and could also be seen as maximum
      time you are willing to wait for opening a connection to the Server for NEW Requests. @html(<br><br>)

      NOTE: For more precise Timeout control of if you want to set Timeouts differently depending on
      the Request or Response contents, you can leave all "Timeout" properties at their default values (0)
      and directly call the "Sender.Timeout.Enable(time)" method from inside events triggered by this component. }
    property TimeoutAfterPostNewRequestI:integer read FTimeout_PostNewRequest write FTimeout_PostNewRequest  default 0;

    { Incomming connection Timeout (seconds) after an OLD Request from a Client was successfully posted,
      after a call to OnPostOldRequest has returned a TRtcDataRequest component linked to a TRtcDataClient. @html(<br>)
      This is the first step to sending a Request to the Server and could also be seen as maximum
      time you are willing to wait for opening a connection to the Server for OLD Requests. @html(<br><br>)

      NOTE: For more precise Timeout control of if you want to set Timeouts differently depending on
      the Request or Response contents, you can leave all "Timeout" properties at their default values (0)
      and directly call the "Sender.Timeout.Enable(time)" method from inside events triggered by this component. }
    property TimeoutAfterPostOldRequestI:integer read FTimeout_PostOldRequest write FTimeout_PostOldRequest  default 0;

    { Incomming connection Timeout (seconds) after a Request from a Client was placed into a Queue,
      because a call to OnPostNewRequest or OnPostOldRequest has NOT returned a TRtcDataRequest component. @html(<br>)
      This will be the maximum amount of time you would allow a Request to remain in the Request Queue
      or maximum time for the complete Queue to be turned arround in case the Request has to be re-queued. @html(<br><br>)

      NOTE: For more precise Timeout control of if you want to set Timeouts differently depending on
      the Request or Response contents, you can leave all "Timeout" properties at their default values (0)
      and directly call the "Sender.Timeout.Enable(time)" method from inside events triggered by this component. }
    property TimeoutAfterQueuedRequestI:integer read FTimeout_QueuedRequest write FTimeout_QueuedRequest  default 0;

    { Outgoing connection Timeout (seconds) after we Begin forwarding a Request to a Server. @html(<br>)
      Since the Timer would be reset between each chunk is being sent, this Timeout defines the maximum
      Time allowed for about 64KB of data to be sent to the Server (NOT the complete Request content body). @html(<br><br>)

      NOTE: For more precise Timeout control of if you want to set Timeouts differently depending on
      the Request or Response contents, you can leave all "Timeout" properties at their default values (0)
      and directly call the "Sender.Timeout.Enable(time)" method from inside events triggered by this component. }
    property TimeoutAfterRequestBeginO:integer read FTimeout_RequestBegin write FTimeout_RequestBegin  default 0;

    { Incomming connection Timeout (seconds) after each chunk of Request data is received from a Client. }
    property TimeoutAfterRequestDataIn:integer read FTimeout_RequestDataIn write FTimeout_RequestDataIn default 0;

    { Outgoing connection Timeout (seconds) after each chunk of Request data is forwarded to the Server. }
    property TimeoutAfterRequestDataOut:integer read FTimeout_RequestDataOut write FTimeout_RequestDataOut default 0;

    { Incomming connection Timeout (seconds) after a complete Request was received from a Client. @html(<br>)
      Once we have received the complete Request from the Client, the Request will need to be forwarded
      to the Server and a Response will need to at least start being received from the Server and begin
      being forwarded to the Client (incomming) connection before anything else happens with the incomming
      (Client) connection, which means that this particular Timeout should either be long enough to allow
      the Server (outgoing) connection to receive the complete Request from the Client (incomming) connection
      and for the Response to start being forwarded to the incomming (Client) connection, or set this Timeout
      to -1 if you want to disable Timeouts for the incomming connection while waiting for the Server. @html(<br><br>)

      NOTE: For more precise Timeout control of if you want to set Timeouts differently depending on
      the Request or Response contents, you can leave all "Timeout" properties at their default values (0)
      and directly call the "Sender.Timeout.Enable(time)" method from inside events triggered by this component. }
    property TimeoutAfterRequestReceivedI:integer read FTimeout_RequestReceived write FTimeout_RequestReceived  default 0;

    { Outgoing connection Timeout (seconds) after a complete Request was forwarded to a Server. @html(<br>)
      This Timeout defines the maximum duration you are willing to wait for a Response from the Server
      after the complete Request was sent. Time specified here should allow the Server to process any
      Request and at least start sending a Response (or "-1" to disable Timeouts while waiting for a Response). @html(<br><br>)

      NOTE: For more precise Timeout control of if you want to set Timeouts differently depending on
      the Request or Response contents, you can leave all "Timeout" properties at their default values (0)
      and directly call the "Sender.Timeout.Enable(time)" method from inside events triggered by this component. }
    property TimeoutAfterRequestSentO:integer read FTimeout_RequestSent write FTimeout_RequestSent default 0;

    { Outgoing connection Timeout (seconds) after we Begin receiving a Response from a Server. @html(<br>)
      Since the Timer would be reset between each chunk of data received, this Timeout defines the maximum
      Time allowed for about 64KB of data to be received from the Server (NOT the complete Response content body).
      Should it take longer than specified Timeout for the next block of data to arrive, outgoing connection to
      the Server will be closed, which automatically Results in the incomming connection to the Client to be closed. @html(<br><br>)

      NOTE: For more precise Timeout control of if you want to set Timeouts differently depending on
      the Request or Response contents, you can leave all "Timeout" properties at their default values (0)
      and directly call the "Sender.Timeout.Enable(time)" method from inside events triggered by this component. }
    property TimeoutAfterResponseBeginO:integer read FTimeout_ResponseBegin write FTimeout_ResponseBegin default 0;

    { Outgoing connection Timeout (seconds) after each chunk of Response data is received from the Server. }
    property TimeoutAfterResponseDataIn:integer read FTimeout_ResponseDataIn write FTimeout_ResponseDataIn default 0;

    { Incomming connection Timeout (seconds) after each chunk of Response data is forwarded to a Client. }
    property TimeoutAfterResponseDataOut:integer read FTimeout_ResponseDataOut write FTimeout_ResponseDataOut default 0;

    { Outgoing connection Timeout (seconds) after a complete Response was received from a Server. @html(<br>)
      If this Timeout is set to -1, connection Timeouts will be disabled after receiving a Response. If "Timeout>0",
      connection to the Server will be closed if no Requests are posted using the connection before Timeout. @html(<br><br>)

      NOTE: For more precise Timeout control of if you want to set Timeouts differently depending on
      the Request or Response contents, you can leave all "Timeout" properties at their default values (0)
      and directly call the "Sender.Timeout.Enable(time)" method from inside events triggered by this component. }
    property TimeoutAfterResponseReceivedO:integer read FTimeout_ResponseReceived write FTimeout_ResponseReceived default 0;

    { Incomming connection Timeout (seconds) after a complete Response was forwarded. @html(<br>)
      If this Timeout is not set -1, connection Timeouts will be disabled after forwarding a Response. If "Timeout>0",
      connection to the Client will be closed if no new Requests are received through the connection before Timeout. @html(<br><br>)

      NOTE: For more precise Timeout control of if you want to set Timeouts differently depending on
      the Request or Response contents, you can leave all "Timeout" properties at their default values (0)
      and directly call the "Sender.Timeout.Enable(time)" method from inside events triggered by this component. }
    property TimeoutAfterResponseSentI:integer read FTimeout_ResponseSent write FTimeout_ResponseSent default 0;

    { This event will be called when a new Request comes from a Client and its
      headers have been received (content body was not yet received). From here,
      you need to check the request information available and decide wether you
      want to "route" this request or not. If you want to route this request, you
      have to accept the request by calling "Sender.Accept;" @html(<br><br>)

      If NO changes are required to the Request, or if ONLY Request Headers need to be modified
      before the Request is forwarded to the Server, "Sender.Request.ManualRead" property should
      be set to TRUE from within THIS event to allow request content body to be forwarded as
      it arrives, without buffering the complete request content body in memory. @html(<br><br>)

      But ... if Request Content Body *also* needs to be modified for this Request before
      it is forwarded to the Server, "Sender.Request.ManualRead" property has to be FALSE.
      Then, you will be able to make modifications to Request Header *and* Request Content Body
      from within the "OnRequestComplete" event, which will be fired once the complete request
      with the complete content body arrives from the Client. @html(<br><br>)

      NOTE: If your component doesn't Accept the request when it first receives
      it inside this OnCheckRequest event, the same request will be passed to
      the next component in line, until one component accepts the request. @html(<br><br>)

      If the request was NOT accepted after all CheckRequest events from all
      components assigned to the HttpServer component were passed, then HtpServer's
      OnRequestNotAccepted event will be called. If a component accepts a request,
      all future events regarding this request will be mapped to that component.
      If your component DOES Accept a request, it can NOT un-accept it later. Once a
      request was accepted, it becomes bound to the component which has accepted it. }
    property OnCheckRequestI:TRtcRouterServerEvent read FOnCheckRequest write FOnCheckRequest;

    { This event is called after a newly accepted Request (from the "OnCheckRequest" event)
      is ready to be sent to the Server and a connection components is now required.
      The event is called with "MoveToBottom=TRUE", "AddToQueue=0" and "DataRequest=nil". @html(<br><br>)

      In other words, a new Request is ready for posting to the Server and the TRtcDataRouter
      component is now asking for a TRtcDataRequest component linked to a TRtcHttpClient which
      you want to use for posting the Request to the Server. @html(<br><br>)

      If you want the Request to be sent to the Server now, simply return a "DataRequest" component
      linked to a "TRtcDataClient" (through its "Client" property). The "TRtcDataClient" component
      needs to be prepared for connecting (set at least "ServerAddr" and "ServerPort" properties). @html(<br><br>)

      If the Request should NOT be posted yet, leave DataRequest unchanged (NIL) and
      set "AddToQueue" to the outgoing Request Queue number (0...) to which you want this
      Request linked and set "MoveToBottom" to TRUE if you want this "Sender.Request" to be moved to the
      BOTTOM of that Request Queue, or set it to FALSE if the Request should be placed at the TOP.
      Default "MoveToBottom" value for NEW Requests is always TRUE and AddToQueue is 0, so you do
      NOT need to change anything if you want to use the default (first-in, first-out) behavior. @html(<br><br>)

      If you do NOT want to send the Request to the Server now, you do NOT have to do anything
      in this event except setting the "MoveToBottom" parameter to FALSE in case you want the Request
      to be placed at the TOP of the Request Queue instead of BOTTOM (default for new requests).
      Requests are always picked up from the TOP of the request Queue when "ReadyForNextRequest"
      or "PostNextRequest" methods are called on the "TRtcDataRouter" component. }
    property OnPostNewRequestI:TRtcRouterGetConnectionEvent read FOnPostNewRequest write FOnPostNewRequest;

    { This event will be triggered after you call "ReadyForNewRequest" in case there was a request waiting
      in the Request Queue. The event will be called using the first topmost Request from the Request Queue
      and with "MoveToBottom=FALSE" and "DataRequest=nil" as default parameters. @html(<br><br>)

      In other words, after "ReadyForNextRequest" method was called (for example from "OnQueuedRequest" event)
      and TRtcDataRouter has found a Request waiting in its Request Queue, THIS event is called to allow
      you to check if a DataRequest component is *still* available (outgoing Server is ready for requests)
      and return a "DataRequest" component if "Sender.Request" should be sent to the Server now.

      If a DataRequest component is available and should be used to post this Request, this event has to
      return a "TRtcDataRequest" linked to a "TRtcHttpClient" component, just like the "OnPostNewRequestI" event.
      If you do NOT want to send the Request to the Server now, you do NOT have to do anything
      in this event except setting the "MoveToBottom" parameter to TRUE in case you want the Request
      to be placed at the BOTTOM of the Request Queue instead of TOP (default for old requests). @html(<br><br>)

      In other words, if a DataRequest component is NOT available, leave the "DataRequest" parameter
      unchanged (NIL) and set the "MoveToBottom" parameter to TRUE if you want the Request to be
      moved to the BOTTOM of the Request queue, or leave "MoveToBottom" unchanged (FALSE) if you
      want the Request to remain at the TOP of the Request queue, so it can be picked up first when
      "ReadyForNextRequest" or "PostNextRequest" method is called again on this TRtcDataRouter component.
      Default "MoveToBottom" value for OLD Requests is always FALSE, so you do NOT need to change
      it if you want OLD Requests to remain at the TOP of to request queue if they can't be sent. }
    property OnPostOldRequestI:TRtcRouterGetConnectionEvent read FOnPostOldRequest write FOnPostOldRequest;

    { This event is called if a Request could NOT be posted after the last "OnPostNewRequest"
      or "OnPostOldRequest" event and was now stored in the Request Queue "AddedToQueue". @html(<br><br>)

      If "MovedToBottom" parameter sent to this event is TRUE, the Request was added at
      the BOTTOM of the Request queue, which means that it will only be taken out after
      all the other requests from "AddedToQueue" Queue are already sent. @html(<br><br>)

      You can NOT force THIS particular request to be taken out of the Queue anymore, since
      other Requests could also have ended up in the Queue in the meantime, or the Request
      could have already removed from the Queue by the time this event was called, but ...
      you can use the "ReadyForNextRequest" or "PostNextRequest" method to signal the TRtcDataRouter
      component that the next request from the Request queue can now be sent to the Server. @html(<br><br>)

      If you want to Post the next available Request in the Queue (topmost Request if THIS Request
      was placed aside), you can use the "PostNextRequest" or "ReadyForNewRequest" method from here.
      In other words ... @html(<br><br>)

      This event is called immediately after the Request was placed in the Request Queue,
      which happened either after a call to "DataRouterPostNewRequest" or "DataRouterPostOldRequest"
      in case the event did NOT return a valid "TRtcDataRouter" object and thus the Request
      could NOT be sent to the Server (outgoing connection) at that time. @html(<br><br>)

      The purpose of this event (QueuedRequest) is to allow you to check if a connection
      (DataRequest + HttpClient component pair) is now available, in which case you should
      use the "ReadyForNextRequest" or "PostNextRequest" method to signal the TRtcDataRouter
      that you are now ready.  @html(<br><br>)

      If you use "PostNextRequest" on the TRtcDataRouter component, topmost Request from the Request
      Queue will be sent using the "DataRequest" component passed to the "PostNextRequest" method,
      without giving you the chance to check Request Headers.  @html(<br><br>)

      You should ONLY use the "PostNextRequest" method if your TRtcDataRouter component is working
      with a single Server, or if all the Web Applications handled with this TRtcDataRouter component
      are stateless so that any Request can be send to any Server. @html(<br><br>)

      If your TRtcDataRouter component has to work with multiple Servers, but - depending on Request
      headers - some Requests need to be sent to a specific Server and you need to check Request
      Headers before you can decide to which Server which Request should be sent to, always use
      the "ReadyForNextRequest" method with that particular TRtcDataRouter component. @html(<br><br>)

      If you use the "ReadyForNextRequest" method, "DataRouterPostOldRequest" event will be
      triggered for the topmost Request in the Request Queue of that TRtcDataRouter component,
      allowing you to check Request Headers before deciding where the Request should be sent. @html(<br><br>)

      NOTE: You can use any number of TRtcDataRouter components in the same application if
      request headers are enough for you to decide where which request should be forwarded to.
      For example, if by checking Request Headers you can already know if a Request is being
      sent to a Stateful Web Application, because of which all future Requests from that
      Client should be forwarded to the same Web Application, you can use a separate
      TRtcDataRouter component for every Stateful Server and move the decision-making
      process directly into the "OnCheckRequest" event. }
    property OnQueuedRequestI:TRtcRouterQueConnectionEvent read FOnQueuedRequest write FOnQueuedRequest;

    { This event is called when a TRtcDataRequest and its associated TRtcDataClient components
      received from "OnPostNewRequest", "OnPostNewRequest" or "OnPostQueuedRequest" events are being
      returned because they are no longer actively used. This could either be because the request
      was succesfully forwarded to the Server and a response forwarded to the Client, or because
      the request and/or response were aborted (see other events for more info). @html(<br><br>)

      You can either destroy the component, or place it inside your DataRequest object queue,
      and/or use "ReadyForNextRequest" or "PostNextRequest" method to signal the TRtcDataRouter
      component that you are now ready to post the next request from the Request Queue. @html(<br><br>)

      The connection to the Server is NOT actively closed before returning the components,
      but they are in a consistent state so that returned "TRtcDataRequest" and its associated
      "TRtcDataClient" component can be re-used for posting more requests to the same Server. @html(<br><br>)

      You are also allowed to Destroy the returned components after closing the connection. }
    property OnPostReturn:TRtcRouterPutConnectionEvent read FOnPostReturn write FOnPostReturn;

    { This event will be called before we begin forwarding a Request to the Server. 
      You can use this event to make final adjustments to Request Headers. @html(<br><br>)

      For any Request for which Request Content buffering was ENABLED from the "OnCheckRequest" event
      (by leaving the "Sender.Request.ManualRead=FALSE"), this event will allways be triggered AFTER
      the "OnRequestReceived" event. This is becuse Requests are buffered in memory when Request Content
      buffering is enabled, and a connection to the Server is requested only AFTER the complete
      request was received from the Client, allowing you to modify the Request Content Body and
      Request Headers from the "OnRequestReceived" event, before anything is sent to the Server.
      This allows the TRtcDataRouter component to automatically recalculate the "Content-Length"
      header depending on the new "Content.Body" string, before the Request is forwarded to the Server. @html(<br><br>)

      If Request Content buffering was DISABLED in the "OnCheckRequest" event by setting
      "Sender.Request.ManualRead := TRUE", this event could either get called BEFORE or AFTER
      the "OnRequestReceived" event, depending on how much Content was sent from the Client. @html(<br><br>)

      Independent of Content Buffering settings for this Request, you can always use THIS event to
      modify Request Headers (except "Content-Length") before the Request is forwarded to the Server. }
    property OnRequestBeginO:TRtcRouterClientEvent read FOnRequestBegin write FOnRequestBegin;

    { This event will be called if connection from the Client (incomming) was lost
      before the complete Request could be received. This event is used ONLY for notification.
      There is nothing special you need to do in this event (you do NOT even have to implement it),
      but you can use it for decision-making or logging or anything else you might need. }
    property OnRequestReceiveAbortI:TRtcRouterServerEvent read FOnRequestReceiveAbort write FOnRequestReceiveAbort;

    { This event will be called after the *complete* Request was received from the Client (incoming connection). @html(<br><br>)

      If Request Content buffering was DISABLED by setting "Sender.Request.ManualRead := TRUE" for this
      Request from the "OnCheckRequest" event, "Content" parameter sent to this event will be NIL regardless
      of the actual Content Body received. In that case, changes to Request Headers HERE will be ignored,
      but you can use the event for logging and other non-intrusive operations. @html(<br><br>)

      On the other hand, if Request Content buffering was ENABLED in the "OnCheckRequest" event by leaving
      the "Sender.Request.ManualRead" property at its default value (FALSE), THIS event will be called BEFORE
      the "OnRequestBegin" event, containing the complete Request Content Body in the "Content.Body" parameter.
      Make sure to check if "Content<>nil" BEFORE you try accessing it's "Body" value to avoid Access Violations.
      If "Content" parameter is not NIL, you can modify Request Headers *and* Request "Content.Body" from THIS event
      and your modified Request will be forwarded to the Server instead of the original received from the Client. @html(<br><br>)

      IMPORTANT NOTE: You should NEVER make changes to the "CONTENT-LENGTH" Request Header value. If Content
      buffering was ENABLED, "Content<>nil" and you modify the "Content.Body" string, "Content-Length" will be
      updated automatically by the TRtcDataRouter component. But in case content buffering was DISABLED, changing
      the "Content-Length" Request Header would mess up the Request and result in serious errors. }
    property OnRequestReceivedI:TRtcRouterRequestReceivedEvent read FOnRequestReceived write FOnRequestReceived;

    { This event will be called if a connection to the Server (outgoing) was lost
      before the complete Request could be sent. This event is used ONLY for notification.
      There is nothing special you need to do in this event (you do NOT even have to implement it),
      but you can use this event for decision-making or logging or anything else you might need. }
    property OnRequestSendAbortO:TRtcRouterClientEvent read FOnRequestSendAbort write FOnRequestSendAbort;

    { This event will be called once the Request was sent (forwarded) to the Server.
      You can use this event for decision-making or logging or anything else you might need,
      but it does NOT have to be implemented for the TRtcDataRouter component to function.
      This is only a notification event, do NOT make changes to the Request or Response here. }
    property OnRequestSentO:TRtcRouterClientEvent read FOnRequestSent write FOnRequestSent;

    { This event will be called when the Response Headers have been received from the Server,
      for a Request which was Accepted and forwarded by this component. If you want to modify
      Response Headers before they are forwarded to the Client, and you do NOT need nor want
      to change the Response Content Body, you can do it from inside THIS event. @html(<br><br>)

      This event is almost as important as the "OnCheckRequestI" event, because you get to
      decide here whether you want to buffer the Response Content Body so you can modify it before
      it is forwarded to the Client, or DISABLE Response Content buffering in case you do NOT need
      to modify the Response Content body. You can also make changes to Response Headers here,
      regardless of whether you enable or disable Response Content buffering or not. @html(<br><br>)

      If you do NOT need to modify the Response Content Body for this Request, you should also set
      "Sender.Response.ManualRead := TRUE" inside THIS event to make sure that Response Content Body
      will start being forwarded to the Client immediately instead of first being buffered in memory. @html(<br><br>)

      If you need to modify the Response Content Body and not *only* Response Headers, you have to
      leave the "Sender.Response.ManualRead" property at its default value (FALSE). If you do so, you
      will be able to modify the complete Response Content Body and Response Headers from within
      the "OnResponseReceived" event, which will be triggered when the complete Response is received. }
    property OnResponseBeginO:TRtcRouterClientEvent read FOnResponsebegin write FOnResponseBegin;

    { This event will be called if a connection to the Server was lost  before the complete Response was received.
      There is ONLY a notification Event. There is nothing you need to do in here (you can leave it unimplemented),
      but you can use this event for decision-making or logging or anything else you might need. }
    property OnResponseReceiveAbortO:TRtcRouterClientEvent read FOnResponseReceiveAbort write FOnResponseReceiveAbort;

    { This event will be called once the complete Response Content Body was received from Server (outgoing connection). @html(<br><br>)

      If Response Content Buffering was DISABLED for this Response by setting "Sender.Response.ManualRead:=True"
      from the "OnResponseBegin" event, "Content" parameter sent to this event will be *NIL* and any changes
      you might make to Response Headers will be ignored. @html(<br><bt>)

      On the other hand, if you leave the "Sender.Response.ManualRead=FALSE" (default value) for this
      Response in the "OnResponseBegin" event, Response Content buffering will be ENABLED for this Response,
      so the "Content" parameter will be assigned (not NIL) and will contain the complete Response Content
      Body in its "Content.Body" variable, which you can modify here if you want a different Response
      Content to be sent to the Client. If the "Content" parameter is assigned, you can also modify
      Response Headers. If you modify Response Headers or "Content.Body", your modified Response Headers
      and "Content.Body" will be sent to the Server instead of the original content received. @html(<br><br>)

      If the "Content" parameter is NOT assigned, you should NOT make any modifications to the Response
      Headers either, but you can use the event for logging and other non-intrusive operations. @html(<br><br>)

      IMPORTANT NOTE: Before you try to access the "Content.Body" value, make sure to check if "Content<>nil" to avoid
      Access Violations. You can also check if "Content<>nil" to know if Response Content buffering was enabled from the
      "OnResponseReceived" event for this Response, which is a good way to avoid duplicating your own code used to decide
      when Response Content Body should be modified before it is forwarded to the Server and when not. }
    property OnResponseReceivedO:TRtcRouterResponseReceivedEvent read FOnResponseReceived write FOnResponseReceived;

    { This event will be called if a connection from the Client (incoming) was lost
      before the complete Response could be sent. This event is used ONLY for notification.
      There is nothing special you need to do in this event (you do NOT even have to implement it),
      but you can use this event for decision-making or logging or anything else you might need. }
    property OnResponseSendAbortI:TRtcRouterServerEvent read FOnResponseSendAbort write FOnResponseSendAbort;

    { This event will be called once the complete Response was sent (forwarded) to the Client.
      You can use this event for decision-making or logging or anything else you might need, but this
      is ONLY a notification event and you should NOT make any changes to the Request or Response here. }
    property OnResponseSentI:TRtcRouterServerEvent read FOnResponseSent write FOnResponseSent;

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

    { This event is used by the "TRtcDataRouter" component for Debug Logging. It does NOT
      need to be implemented if you do NOT need detailed TRtcDataRouter Debug LOGs to be created. @html(<br><br>)

      WARNING: Debug Logging takes a considerable amount of time and decreases overall performance.
      Especially when writing LOG entries to a File, it will takes a LOT of time.
      If Debug logging is NOT required, this event should NOT be assigned. }
    property OnDebugLog:TRtcRouterDebugLogEvent read FOnDebugLog write FOnDebugLog;

  public // WEB SOCKETS

    // @exclude
    procedure Call_WSConnect(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDataSent(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_WSDisconnect(Sender:TRtcConnection); override;

    // @exclude
    procedure Client_WSConnect(Sender:TRtcConnection);
    // @exclude
    procedure Client_WSDataReceived(Sender:TRtcConnection);
    // @exclude
    procedure Client_WSDataSent(Sender:TRtcConnection);
    // @exclude
    procedure Client_WSDisconnect(Sender:TRtcConnection);

    end;

  // @exclude
  TRtcProxyData=class(TObject)
  public
    cs:TRtcCritSec;
    active:boolean;
    queued:byte;
    parent:TRtcDataRouter;
    con:TRtcConnection;
    req:TRtcDataRequest;
    done,ready,cleared,notify:boolean;
    proxy:TRtcProxyData;
    uri,head,stext:RtcString;
    xdata:TRtcHugeByteArray;
    scode:integer;
    xclose:boolean;
    nr:RtcString;

    QueueIndex:integer;

    constructor Create; virtual;
    destructor Destroy; override;

    function PostNotifyEvent(Evnt:TRtcNotifyEvent):boolean;
    function PostEvent(Evnt:TRtcNotifyEvent; _done:boolean):boolean;
    function PostDataFirstEvent(Evnt:TRtcNotifyEvent; const _data:RtcByteArray; const _head,_stext:RtcString; _scode:integer; _xclose:boolean; _done:boolean; _notify:boolean):boolean;
    function PostDataEvent(Evnt:TRtcNotifyEvent; const _data:RtcByteArray; _done:boolean; _notify:boolean):boolean;
    function StartNow:boolean;
    function ReStartNow:boolean;
    procedure PostDone(Evnt:TRtcNotifyEvent);
    procedure PostWSOpen(Evnt:TRtcNotifyEvent);

    function GetData2:RtcByteArray;
    function GetData(var closenow:boolean):RtcByteArray;

    function Get_Data2(var _notify:boolean):RtcByteArray;
    function Get_Data(var closenow:boolean; var _notify:boolean):RtcByteArray;
    end;

  { @abstract(Pool of Client connections towards one Server) }
  TRtcClientPool=class(TObject)
  protected
    FDisabled:boolean;
    FID:integer;
    FIdle:TXObjList;
    FCount:integer;
    FSessExp:tStrIntList; // Sessions sorted by Expiring date+time & Session ID
    FSessList:tStrList; // Sessions sorted by Session ID

    function GetIdleCount: integer;

    { This method needs to be implemented by a descendant class.
      The purpose of this method is to create a new TRtcDataClient component
      and set all its properties so that a connection can be made to the Server. @html(<br><br>)

      If a connection component can NOT be created at this point (for example,
      the Server is too busy and can not handle new connections yet), return NIL. @html(<br><br>)

      If the Server can accept a new connection, then create a new TRtcHttpClient component
      and set all its properties required for opening a connection to the Server. }
    function NewClient:TRtcDataClient; virtual;

  public
    // Standard constructor
    constructor Create(StackSize:integer); virtual;
    // Standard destructor
    destructor Destroy; override;

    { Calls "Disconnect" on all idle Client connections inside the Pool. }
    procedure Close;

    { If there are idle DataRequest components inside the Pool, returns the topmost
      idle DataRequest component from the Pool and removes that component from the Pool,
      incrementing the value of "ActiveClients" and decrementing the value of "IdleClients".

      If the Pool is empty, calls "NewClient" to create a new connection. If "NewClient"
      returns a valid connection component, a DataRequest component is created and returned,
      incrementing the value of "ActiveClients" without changing the value of "IdleClients".

      If the Pool is empty but "NewClient" returns NIL, GetDataRequest will also return NIL,
      without changing either "ActiveClients" nor "IdleClients" values. }
    function GetDataRequest:TRtcDataRequest;

    { Places a DataRequest component received from GetDataRequest back to the top of our Pool,
      incrementing the value of "IdleClients" and decrementing the value of "ActiveClients".
      Returns the Pool index where the component was placed or -1 if the Pool does not exist. }
    procedure PutDataRequest(DataRequest:TRtcDataRequest; ToBottom:boolean=False);

    { If Session "SID" already exists, update its expiration time.
      If Session "SID" does not exist, open a new session "SID" and set its expiration time. }
    procedure OpenOrUpdateSession(const SID:RtcString; ExpireTime:TDateTime);

    { If Session "SID" already exists, return FALSE.
      If Session "SID" does NOT yet exist, Open new Session "SID",
        set Sessions expiration time to "ExpireTime" and return TRUE. }
    function OpenSession(const SID:RtcString; ExpireTime:TDateTime):boolean;

    { If Session "SID" does NOT exist, return FALSE.
      If Session "SID" exists, update its expiration time to "ExpireTime" and return TRUE. }
    function UpdateSession(const SID:RtcString; ExpireTime:TDateTime):boolean;

    { If Session "SID" does NOT exist, return FALSE.
      If Session "SID" exists and expires before "BeforeTime",
      update its expiration time to "ExpireTime" and return TRUE. }
    function UpdateSessionIfExpiresBefore(const SID:RtcString; BeforeTime,ExpireTime:TDateTime):boolean;

    { If Session "SID" exists and it would expire before "ExpireTime", return TRUE.
      If Session "SID" does not exist or if it would expire after "ExpireTime", return FALSE. }
    function SessionExpiresBefore(const SID:RtcString; ExpireTime:TDateTime):boolean;

    { If Session "SID" exist, return TRUE.
      If Session "SID" does NOT exist, return FALSE. }
    function HaveSession(const SID:RtcString):boolean;

    { If Session "SID" exist, Close the Session and return TRUE.
      If Session "SID" did NOT exist, return FALSE. }
    function CloseSession(const SID:RtcString):boolean;

    { Total number of active Sessions in this Pool. }
    function ActiveSessions:integer;

    { Remove all Sessions with expiration time before "ExpireTime".
      Returns the number of Sessions removed. }
    function RemoveExpiredSessions(ExpireTime:TDateTime):integer;

    { If this Pool is inside a MultiClientPool,
      this ID is the "index" where it can be found in the MultiClientPool.
      For a stand-alone Pool (not part of a MultiClientPool), ID will be -1. }
    property ID:integer read FID;

    { Total number of Clients currently Active (in use). }
    property ActiveClients:integer read FCount;

    { Total number of Clients currently idle (NOT in use). }
    property IdleClients:integer read GetIdleCount;

    { Flag to disable this connection pool }
    property Disabled:boolean read FDisabled write FDisabled default False;
    end;

  { @abstract(Multiple Pools of Client connections, each Pool pointing at a different Server) }
  TRtcMultiClientPool=class(TObject)
  protected
    FPools:array of TRtcClientPool;
    FPoolCnt:integer;

    function GetActiveClients: integer;
    function GetIdleClients: integer;
    function GetPool(index: integer): TRtcClientPool;

  public
    // Standard constructor
    constructor Create; virtual;
    // Standard destructor
    destructor Destroy; override;

    { Add a new Client Pool to our "Multi Client Pool".
      Returns the index at which the new Pool is now. }
    function AddPool(NewPool:TRtcClientPool):integer;

    { Extract "Pool Index" from Client connection component }
    function GetPoolIndex(Con:TRtcDataClient):integer;

    // Close Client connections in all Pools
    procedure CloseAll;

    { Places a DataRequest component received from GetDataRequest back to the originating Pool,
      incrementing the value of "IdleClients" and decrementing the value of "ActiveClients". }
    function PutDataRequest(DataRequest:TRtcDataRequest; ToBottom:boolean=False):integer;

    { Update Session "SID" to expire after "ExpireTime".
      Returns the index of the Pool where the Session was updated,
      or -1 if Session "SID" was not found. }
    function UpdateSession(const SID:RtcString; ExpireTime:TDateTime):integer;

    { If Session "SID" exists and it would expire before "ExpireTime", return the Pool index where the Session is.
      If Session "SID" does not exist or if it would expire after "ExpireTime", return -1. }
    function SessionExpiresBefore(const SID:RtcString; ExpireTime:TDateTime):integer;

    { If Session "SID" does NOT exist or if it would expire after "BeforeTime", return -1.
      If Session "SID" exists and would expire before "BeforeTime",
      update its expiration time to "ExpireTime" and return the Pool index where the Session is. }
    function UpdateSessionIfExpiresBefore(const SID:RtcString; BeforeTime,ExpireTime:TDateTime):integer;

    { Check if Session "SID" exists in one of the Pools.
      Returns the index of the Pool where the Session was found,
      or -1 if Session "SID" could not be found. }
    function HaveSession(const SID:RtcString):integer;

    { Close Session "SID".
      Returns the index of the Pool where the Session was found,
      or -1 if the Session "SID" could not be found. }
    function CloseSession(const SID:RtcString):integer;

    { Returns the total number of active Sessions (sum of all Pools). }
    function ActiveSessions:integer;

    { Removes all Sessions with expiration time before "ExpireTime" (from all Pools). }
    function RemoveExpiredSessions(ExpireTime:TDateTime):integer;

    { Total number of active Clients (all Pools). }
    property ActiveClients:integer read GetActiveClients;

    { Total number of idle Clients (all Pools). }
    property IdleClients:integer read GetIdleClients;

    { Number of Pools in this MultiClientPool }
    property PoolCount:integer read FPoolCnt;

    { Pool at position "index" (starting at 0, max = PoolCount-1) }
    property Pool[index:integer]:TRtcClientPool read GetPool;
    end;

implementation

{ TRtcRouterContentBody }

function TRtcRouterContentBody.GetBodyTxt:RtcString;
  begin
  Result:= RtcBytesToString(FBody);
  end;

procedure TRtcRouterContentBody.SetBodyTxt(Value:RtcString);
  begin
  FBody := RtcStringToBytes(Value);
  end;

{ TRtcProxyData }

constructor TRtcProxyData.Create;
  begin
  try
    inherited;
    xdata:=TRtcHugeByteArray.Create;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.Create',E,'ERROR');
      raise;
      end;
    end;
  end;

destructor TRtcProxyData.Destroy;
  begin
  try
    if assigned(parent.OnDebugLog) then
      parent.Debug(nr+'. Destroy Proxy '+uri,RLOG_PROXY);
    parent.Proxy_Clear(self,nil,True);
    uri:='';
    RtcFreeAndNil(xdata);
    head:='';
    stext:='';
    xclose:=False;
    inherited;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcProxyData.GetData2: RtcByteArray;
  begin
  try
    CS.Acquire;
    try
      Result:=xdata.GetEx;
      xdata.Clear;
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.GetData2',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcProxyData.GetData(var closenow:boolean): RtcByteArray;
  begin
  try
    CS.Acquire;
    try
      Result:=xdata.GetEx;
      xdata.Clear;
      closenow:=done;
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.GetData',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcProxyData.Get_Data2(var _notify:boolean): RtcByteArray;
  begin
  try
    CS.Acquire;
    try
      Result:=xdata.GetEx;
      _notify:=notify;
      notify:=False;
      xdata.Clear;
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.Get_Data2',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcProxyData.Get_Data(var closenow:boolean; var _notify:boolean): RtcByteArray;
  begin
  try
    CS.Acquire;
    try
      Result:=xdata.GetEx;
      xdata.Clear;
      closenow:=done;
      _notify:=notify;
      notify:=False;
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.Get_Data',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcProxyData.PostEvent(Evnt: TRtcNotifyEvent; _done:boolean):boolean;
  begin
  try
    Result:=False;
    CS.Acquire;
    try
      if assigned(parent.OnDebugLog) then
        parent.Debug('$$ '+nr+'. PostEvent '+uri,RLOG_EVENT);
      if assigned(proxy) then
        begin
        if _done then
          begin
          proxy.done:=True;
          proxy.notify:=False;
          end;
        if ready and not done then
          Result:=con.PostEvent(Evnt)
        else
          Result:=True;
        if not Result then
          if assigned(parent.OnDebugLog) then
            parent.Debug('§§§§§§§§ '+nr+'. PostEvent - FAILED! '+uri,RLOG_EVENT);
        end
      else if done then
        begin
        Result:=True;
        if assigned(parent.OnDebugLog) then
          parent.Debug('!! '+nr+'. PostEvent - DONE. '+uri,RLOG_EVENT);
        end
      else
        if assigned(parent.OnDebugLog) then
          parent.Debug('?? '+nr+'. PostEvent - PROXY not assigned! '+uri,RLOG_EVENT);
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.PostEvent',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcProxyData.PostNotifyEvent(Evnt: TRtcNotifyEvent):boolean;
  begin
  try
    Result:=False;
    CS.Acquire;
    try
      if assigned(parent.OnDebugLog) then
        parent.Debug('$$ '+nr+'. PostNotifyEvent '+uri,RLOG_EVENT);
      notify:=False;
      if assigned(proxy) then
        begin
        if ready and not done then
          Result:=con.PostEvent(Evnt)
        else
          Result:=True;
        if not Result then
          if assigned(parent.OnDebugLog) then
            parent.Debug('§§§§§§§§ '+nr+'. PostNotifyEvent - FAILED! '+uri,RLOG_EVENT);
        end
      else if done then
        begin
        Result:=True;
        if assigned(parent.OnDebugLog) then
          parent.Debug('!! '+nr+'. PostNotifyEvent - DONE. '+uri,RLOG_EVENT);
        end
      else
        if assigned(parent.OnDebugLog) then
          parent.Debug('?? '+nr+'. PostNotifyEvent - PROXY not assigned! '+uri,RLOG_EVENT);
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.PostNotifyEvent',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcProxyData.PostDone(Evnt:TRtcNotifyEvent);
  begin
  try
    CS.Acquire;
    try
      if assigned(parent.OnDebugLog) then
        parent.Debug('$$ '+nr+'. PostDone '+uri,RLOG_EVENT);
      if assigned(proxy) then
        begin
        proxy.done:=True;
        proxy.notify:=False;
        if ready and not done and assigned(Evnt) then
          con.PostEvent(Evnt);
        end;
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.PostDone',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcProxyData.PostWSOpen(Evnt:TRtcNotifyEvent);
  begin
  try
    CS.Acquire;
    try
      if assigned(parent.OnDebugLog) then
        parent.Debug('$$ '+nr+'. PostWSOpen '+uri,RLOG_EVENT);
      if assigned(proxy) then
        begin
        proxy.done:=False;
        proxy.notify:=True;
        if assigned(Evnt) then
          con.PostEvent(Evnt);
        end;
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.PostWSOpen',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcProxyData.PostDataFirstEvent(Evnt:TRtcNotifyEvent; const _data:RtcByteArray; const _head,_stext:RtcString; _scode:integer; _xclose:boolean; _done:boolean; _notify:boolean):boolean;
  begin
  try
    Result:=False;
    CS.Acquire;
    try
      if assigned(parent.OnDebugLog) then
        parent.Debug('$$ '+nr+'. PostDataFirstEvent '+uri,RLOG_EVENT);
      if assigned(proxy) then
        begin
        if _done then
          proxy.done:=True;
        proxy.notify:=_notify;
        proxy.xdata.AddEx(_data);
        proxy.head:=_head;
        proxy.stext:=_stext;
        proxy.scode:=_scode;
        proxy.xclose:=_xclose;
        if ready and not done then
          Result:=con.PostEvent(Evnt)
        else
          Result:=True;
        if not Result then
          if assigned(parent.OnDebugLog) then
            parent.Debug('§§§§§§§§ '+nr+'. PostDataFirstEvent FAILED! '+uri,RLOG_EVENT);
        end
      else
        if assigned(parent.OnDebugLog) then
          parent.Debug('§§§§§§§§ '+nr+'. PostDataFirstEvent - PROXY not assigned! '+uri,RLOG_EVENT);
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.PostDataFirstEvent',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcProxyData.PostDataEvent(Evnt: TRtcNotifyEvent; const _data: RtcByteArray; _done:boolean; _notify:boolean):boolean;
  begin
  try
    Result:=False;
    CS.Acquire;
    try
      if assigned(parent.OnDebugLog) then
        parent.Debug('$$ '+nr+'. PostDataEvent '+uri,RLOG_EVENT);
      if assigned(proxy) then
        begin
        if _done then
          proxy.done:=True;
        proxy.notify:=_notify;
        proxy.xdata.AddEx(_data);
        if ready and not done then
          Result:=con.PostEvent(Evnt)
        else
          Result:=True;
        if not Result then
          if assigned(parent.OnDebugLog) then
            parent.Debug('§§§§§§§§ '+nr+'. PostDataEvent - FAILED! '+uri,RLOG_EVENT);
        end
      else
        if assigned(parent.OnDebugLog) then
          parent.Debug('§§§§§§§§ '+nr+'. PostDataEvent - PROXY not assigned! '+uri,RLOG_EVENT);
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.PostDataEvent',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcProxyData.StartNow:boolean;
  begin
  try
    Result:=False;
    CS.Acquire;
    try
      if assigned(proxy) then
        begin
        if proxy.ready then
          begin
          if assigned(parent.OnDebugLog) then
            parent.Debug('§§§§§§§§ '+nr+'. StartNow - Already started??? '+uri,RLOG_EVENT);
          end
        else
          begin
          if assigned(parent.OnDebugLog) then
            parent.Debug('$$ '+nr+'. StartNow '+uri,RLOG_EVENT);
          proxy.ready:=True;
          Result:=True;
          end;
        end
      else
        if assigned(parent.OnDebugLog) then
          parent.Debug('§§§§§§§§ '+nr+'. StartNow - no PROXY! '+uri,RLOG_EVENT);
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.StartNow',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcProxyData.ReStartNow:boolean;
  begin
  try
    Result:=False;
    CS.Acquire;
    try
      if assigned(proxy) then
        begin
        if assigned(parent.OnDebugLog) then
          parent.Debug('$$ '+nr+'. ReStartNow '+uri,RLOG_EVENT);
        proxy.ready:=False;
        Result:=True;
        end
      else
        if assigned(parent.OnDebugLog) then
          parent.Debug('§§§§§§§§ '+nr+'. ReStartNow - no PROXY! '+uri,RLOG_EVENT);
    finally
      CS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcProxyData.ReStartNow',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcDataRouter }

constructor TRtcDataRouter.Create(AOwner: TComponent);
  begin
  try
    inherited Create(AOwner);
    ProxyDataCS:=TRtcCritSec.Create;
    QueueCnt:=0;
    SetLength(My_Queues,0);
    SetLength(My_Queued,0);
    CurrentNr:=0;
    TotalCreated:=0;
    TotalPending:=0;
    Total_Queued:=0;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Create',E,'ERROR');
      raise;
      end;
    end;
  end;

destructor TRtcDataRouter.Destroy;
  begin
  try
    FOnCheckRequest:=nil;

    FOnPostNewRequest:=nil;
    FOnPostOldRequest:=nil;
    FOnQueuedRequest:=nil;
    FOnPostReturn:=nil;

    FOnRequestBegin:=nil;
    FOnRequestReceived:=nil;
    FOnRequestSent:=nil;

    FOnResponseBegin:=nil;
    FOnResponseReceived:=nil;
    FOnResponseSent:=nil;

    CleanUpQueues;

    RtcFreeAndNil(ProxyDataCS);
    inherited;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ExpandQueue(index: integer);
  var
    a:integer;
  begin
  if index>=QueueCnt then
    begin
    SetLength(My_Queued,index+1);
    SetLength(My_Queues,index+1);
    for a:=QueueCnt to index do
      begin
      My_Queued[a]:=0;
      My_Queues[a]:=tXObjList.Create(16);
      end;
    QueueCnt:=index+1;
    end;
  end;

procedure TRtcDataRouter.CleanUpQueues;
  var
    oreq:TObject;
    obj:TRtcProxyData absolute oreq;
    MyQueue,x:tXObjList;
    a:integer;
  begin
  try
    x:=nil;

    ProxyDataCS.Acquire;
    try
      if Total_Queued=0 then
        begin
        for a:=0 to QueueCnt-1 do
          begin
          MyQueue:=My_Queues[a];
          My_Queues[a]:=nil;
          while MyQueue.Count>0 do
            begin
            MyQueue.extractFirst(oreq);
            RtcFreeAndNil(obj);
            end;
          RtcFreeAndNil(MyQueue);
          end;
        SetLength(My_Queues,0);
        SetLength(My_Queued,0);
        QueueCnt:=0;
        end
      else
        begin
        for a:=0 to QueueCnt-1 do
          begin
          MyQueue:=My_Queues[a];
          while MyQueue.Count>My_Queued[a] do
            begin
            obj:=TRtcProxyData(MyQueue.Last);
            if obj.proxy=nil then
              begin
              MyQueue.removeLast;
              if x=nil then
                x:=tXObjList.Create(8);
              x.addLast(obj);
              end
            else
              begin
              while MyQueue.Count>My_Queued[a] do
                begin
                obj:=TRtcProxyData(MyQueue.First);
                if obj.proxy<>nil then
                  Break;
                MyQueue.removeFirst;
                if x=nil then
                  x:=tXObjList.Create(8);
                x.addLast(obj);
                end;
              Break;
              end;
            end;
          end;
        end;
    finally
      ProxyDataCS.Release;
      end;

    if assigned(x) then
      begin
      while x.Count>0 do
        begin
        x.extractFirst(oreq);
        RtcFreeAndNil(oreq);
        end;
      RtcFreeAndNil(x);
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.CleanUpQueue',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcDataRouter.Proxy_PostRequest(Srv:TRtcDataServer; const _data:RtcByteArray; _notify:boolean):TRtcProxyData;
  var
    cli_pd, srv_pd:TRtcProxyData;
    DataCli:TRtcDataClient;
    DataReq:TRtcDataRequest;
    ToBottom:boolean;
    ToQueue:integer;
  begin
  try
    srv_pd:=TRtcProxyData.Create;
    cli_pd:=TRtcProxyData.Create;

    srv_pd.active:=False;
    cli_pd.active:=False;

    srv_pd.parent:=self;
    cli_pd.parent:=self;

    srv_pd.cs:=ProxyDataCS;
    cli_pd.cs:=ProxyDataCS;

    srv_pd.proxy:=cli_pd;
    cli_pd.proxy:=srv_pd;

    srv_pd.done:=False;
    cli_pd.done:=False;

    srv_pd.queued:=1;

    srv_pd.uri:='(Server) '+Srv.Request.URI;

    cli_pd.con:=Srv;
    cli_pd.uri:=Srv.Request.URI;
    cli_pd.xclose:=Srv.Request.Close;
    cli_pd.stext:=Srv.Request.Method;
    cli_pd.head:=Srv.Request.HeaderText;
    cli_pd.xdata.AddEx(_data);
    cli_pd.notify:=_notify;
    cli_pd.ready:=True; // client can already send to server

    cli_pd.QueueIndex:=0;
    srv_pd.QueueIndex:=0;

    Srv.Request.Info.asObj['$con']:=srv_pd;

    ProxyDataCS.Acquire;
    try
      Inc(TotalCreated,2);
      Inc(CurrentNr);
      srv_pd.nr:='['+Srv.PeerAddr+':'+Srv.PeerPort+'] '+Int2Str(CurrentNr)+'S';
      cli_pd.nr:='['+Srv.PeerAddr+':'+Srv.PeerPort+'] '+Int2Str(CurrentNr)+'C';

      if assigned(OnDebugLog) then
        begin
        Debug('## '+cli_pd.nr+'. NEW PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_PROXY);
        Debug('## '+cli_pd.nr+'. NEW PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_ROUTE);
        end;
    finally
      ProxyDataCS.Release;
      end;

    DataCli:=nil;
    DataReq:=nil;

    ToBottom:=True;
    ToQueue:=0;

    Event_PostNewRequest(Srv,DataReq,ToQueue,ToBottom);
    if assigned(DataReq) then
      DataCli:=DataReq.Client;

    if assigned(DataCli) and assigned(DataReq) then
      begin
      if FTimeout_PostNewRequest<>0 then
        Srv.Timeout.Enable(FTimeout_PostNewRequest);

      if not assigned(DataReq.OnBeginRequest) then
        begin
        DataCli.AutoConnect:=True;
        DataCli.ReconnectOn.ConnectError:=True;
        DataCli.ReconnectOn.ConnectLost:=True;
        DataCli.ReconnectOn.ConnectFail:=True;

        DataReq.OnBeginRequest:=Client_BeginRequest;
        DataReq.OnRepostCheck:=Client_RepostCheck;
        DataReq.OnDataReceived:=Client_DataReceived;
        DataReq.OnDataSent:=Client_DataSent;
        DataReq.OnResponseDone:=Client_ResponseDone;

        DataReq.OnWSConnect:=Client_WSConnect;
        DataReq.OnWSDataSent:=Client_WSDataSent;
        DataReq.OnWSDataReceived:=Client_WSDataReceived;
        DataReq.OnWSDisconnect:=Client_WSDisconnect;
        end;

      ProxyDataCS.Acquire;
      try
        Inc(TotalPending);
        if assigned(OnDebugLog) then
          begin
          Debug('## '+cli_pd.nr+'. START PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_PROXY);
          Debug('## '+cli_pd.nr+'. START PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_ROUTE);
          end;
      finally
        ProxyDataCS.Release;
        end;

      srv_pd.con:=DataCli;
      cli_pd.req:=DataReq;
      if not FPostReturnBeforeResponseSent then
        srv_pd.req:=DataReq;

      srv_pd.queued:=0;
      cli_pd.active:=True;

      DataReq.Request.Info.asObj['$con']:=cli_pd;
      DataReq.Request.Close:=cli_pd.xclose;
      DataReq.Request.Method:=cli_pd.stext;
      DataReq.Request.URI:=cli_pd.uri;
      DataReq.Request.HeaderText:=cli_pd.head;
      DataReq.Post;
      end
    else
      begin
      ProxyDataCS.Acquire;
      try
        cli_pd.QueueIndex:=ToQueue;
        srv_pd.QueueIndex:=ToQueue;

        ExpandQueue(ToQueue);
        Inc(Total_Queued);
        Inc(My_Queued[ToQueue]);

        if ToBottom then
          My_Queues[ToQueue].addLast(cli_pd)
        else
          My_Queues[ToQueue].addFirst(cli_pd);

        if assigned(OnDebugLog) then
          begin
          Debug('## '+cli_pd.nr+'. QUEUE PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_PROXY);
          Debug('## '+cli_pd.nr+'. QUEUE PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_ROUTE);
          end;
      finally
        ProxyDataCS.Release;
        end;

      if FTimeout_QueuedRequest<>0 then
        Srv.Timeout.Enable(FTimeout_QueuedRequest);

      Event_QueuedRequest(Srv,ToQueue,ToBottom);
      end;

    Result:=srv_pd;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Proxy_PostRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Server_PostRequest(Sender:TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    cli_pd, srv_pd:TRtcProxyData;
    DataCli:TRtcDataClient;
    DataReq:TRtcDataRequest;
    ToBottom:boolean;
    FromQueue,
    ToQueue:integer;
  begin
  try
    if assigned(Srv.Request) then
      srv_pd:=TRtcProxyData(Srv.Request.Info.asObj['$con'])
    else
      srv_pd:=nil;

    cli_pd:=nil;
    if assigned(srv_pd) then
      begin
      ProxyDataCS.Acquire;
      try
        if assigned(srv_pd) and (srv_pd.queued>0) and assigned(srv_pd.proxy) then
          begin
          srv_pd.queued:=1;
          cli_pd:=srv_pd.proxy;
          end;
      finally
        ProxyDataCS.Release;
        end;
      end;

    if assigned(cli_pd) then
      begin
      DataCli:=nil;
      DataReq:=nil;
      ToBottom:=False;
      FromQueue:=cli_pd.QueueIndex;
      ToQueue:=FromQueue;

      Event_PostOldRequest(Srv,DataReq,ToQueue,ToBottom);
      if assigned(DataReq) then
        DataCli:=DataReq.Client;

      if assigned(DataCli) and assigned(DataReq) then
        begin
        if FTimeout_PostOldRequest<>0 then
          Srv.Timeout.Enable(FTimeout_PostOldRequest);

        if not assigned(DataReq.OnBeginRequest) then
          begin
          DataCli.AutoConnect:=True;
          DataCli.ReconnectOn.ConnectError:=True;
          DataCli.ReconnectOn.ConnectLost:=True;
          DataCli.ReconnectOn.ConnectFail:=True;

          DataReq.OnBeginRequest:=Client_BeginRequest;
          DataReq.OnDataReceived:=Client_DataReceived;
          DataReq.OnDataSent:=Client_DataSent;
          DataReq.OnRepostCheck:=Client_RepostCheck;
          DataReq.OnResponseDone:=Client_ResponseDone;

          DataReq.OnWSConnect:=Client_WSConnect;
          DataReq.OnWSDataSent:=Client_WSDataSent;
          DataReq.OnWSDataReceived:=Client_WSDataReceived;
          DataReq.OnWSDisconnect:=Client_WSDisconnect;
          end;

        ProxyDataCS.Acquire;
        try
          srv_pd.queued:=0;
          cli_pd.active:=True;

          srv_pd.con:=DataCli;
          cli_pd.req:=DataReq;
          if not FPostReturnBeforeResponseSent then
            srv_pd.req:=DataReq;

          Inc(TotalPending);
          
          Dec(Total_Queued);
          Dec(My_Queued[FromQueue]);

          if assigned(OnDebugLog) then
            begin
            Debug('## '+cli_pd.nr+'. START QUEUED PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_PROXY);
            Debug('## '+cli_pd.nr+'. START QUEUED PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_ROUTE);
            end;
        finally
          ProxyDataCS.Release;
          end;

        DataReq.Request.Info.asObj['$con']:=cli_pd;
        DataReq.Request.Close:=cli_pd.xclose;
        DataReq.Request.Method:=cli_pd.stext;
        DataReq.Request.URI:=cli_pd.uri;
        DataReq.Request.HeaderText:=cli_pd.head;
        DataReq.Post;
        end
      else
        begin
        ProxyDataCS.Acquire;
        try
          ExpandQueue(ToQueue);
          if FromQueue<>ToQueue then
            begin
            Dec(My_Queued[FromQueue]);
            Inc(My_Queued[ToQueue]);
            cli_pd.QueueIndex:=ToQueue;
            srv_pd.QueueIndex:=ToQueue;
            end;

          if ToBottom then
            My_Queues[ToQueue].addLast(cli_pd)
          else
            My_Queues[ToQueue].addFirst(cli_pd);

          if assigned(OnDebugLog) then
            begin
            Debug('## '+cli_pd.nr+'. RE-QUEUED PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_PROXY);
            Debug('## '+cli_pd.nr+'. RE-QUEUED PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_ROUTE);
            end;
        finally
          ProxyDataCS.Release;
          end;

        if FTimeout_QueuedRequest<>0 then
          Srv.Timeout.Enable(FTimeout_QueuedRequest);

        Event_QueuedRequest(Srv,ToQueue,ToBottom);
        end;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Server_PostRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcDataRouter.PostNextRequest(DataRequest:TRtcDataRequest; FromQueue:integer):boolean;
  var
    obj, cli_pd, srv_pd:TRtcProxyData;
    oreq:TObject absolute obj;
    DataCli:TRtcDataClient;
    x:TXObjList;
    MyQueue:tXObjList;

  begin
  try
    Result:=False;
    if not assigned(DataRequest) then
      Exit
    else
      begin
      DataCli:=DataRequest.Client;
      if not assigned(DataCli) then Exit;
      end;

    if not assigned(DataRequest.OnBeginRequest) then
      begin
      DataCli.AutoConnect:=True;
      DataCli.ReconnectOn.ConnectError:=True;
      DataCli.ReconnectOn.ConnectLost:=True;
      DataCli.ReconnectOn.ConnectFail:=True;

      DataRequest.OnBeginRequest:=Client_BeginRequest;
      DataRequest.OnDataReceived:=Client_DataReceived;
      DataRequest.OnDataSent:=Client_DataSent;
      DataRequest.OnRepostCheck:=Client_RepostCheck;
      DataRequest.OnResponseDone:=Client_ResponseDone;

      DataRequest.OnWSConnect:=Client_WSConnect;
      DataRequest.OnWSDataSent:=Client_WSDataSent;
      DataRequest.OnWSDataReceived:=Client_WSDataReceived;
      DataRequest.OnWSDisconnect:=Client_WSDisconnect;
      end;

    x:=nil;
    cli_pd:=nil;

    ProxyDataCS.Acquire;
    try
      if (FromQueue>=0) and (FromQueue<QueueCnt) then
        begin
        MyQueue:=My_Queues[FromQueue];
        while MyQueue.Count>My_Queued[FromQueue] do
          begin
          obj:=TRtcProxyData(MyQueue.Last);
          if obj.proxy=nil then
            begin
            MyQueue.removeLast;
            if x=nil then
              x:=tXObjList.Create(8);
            x.addLast(obj);
            end
          else
            begin
            while MyQueue.Count>My_Queued[FromQueue] do
              begin
              MyQueue.extractFirst(oreq);
              if obj.proxy<>nil then
                begin
                cli_pd:=obj;
                srv_pd:=cli_pd.proxy;

                srv_pd.con:=DataCli;
                cli_pd.req:=DataRequest;
                if not FPostReturnBeforeResponseSent then
                  srv_pd.req:=DataRequest;

                srv_pd.queued:=0;
                cli_pd.active:=True;

                Inc(TotalPending);

                Dec(Total_Queued);
                Dec(My_Queued[FromQueue]);

                Result:=True;

                if assigned(OnDebugLog) then
                  begin
                  Debug('## '+cli_pd.nr+'. START QUEUED PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_PROXY);
                  Debug('## '+cli_pd.nr+'. START QUEUED PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+cli_pd.uri,RLOG_ROUTE);
                  end;
                Break;
                end;
              if x=nil then
                x:=tXObjList.Create(8);
              x.addLast(obj);
              end;
            Break;
            end;
          end;
        end;
    finally
      ProxyDataCS.Release;
      end;

    if assigned(x) then
      begin
      while x.Count>0 do
        begin
        x.extractFirst(oreq);
        RtcFreeAndNil(oreq);
        end;
      RtcFreeAndNil(x);
      end;

    if assigned(cli_pd) then
      begin
      DataRequest.Request.Info.asObj['$con']:=cli_pd;
      DataRequest.Request.Close:=cli_pd.xclose;
      DataRequest.Request.Method:=cli_pd.stext;
      DataRequest.Request.URI:=cli_pd.uri;
      DataRequest.Request.HeaderText:=cli_pd.head;
      DataRequest.Post;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.PostNextRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Proxy_Clear(pd:TRtcProxyData; Evnt:TRtcNotifyEvent; _done:boolean);
  var
    DataReq:TRtcDataRequest;
    xcon:TRtcConnection;
    toFree,obj:TRtcProxyData;
    xready,xproready,xproreq:boolean;
  begin
  try
    toFree:=nil;
    DataReq:=nil;

    ProxyDataCS.Acquire;
    try
      with pd do
        begin
        if not cleared then
          begin
          cleared:=True;
          Dec(TotalCreated);
          if assigned(proxy) then // 1st to call Clear
            begin
            obj:=proxy; proxy:=nil;
            if _done then
              obj.done:=True;
            xproready:=obj.ready;
            xproreq:=assigned(obj.req);
            obj.ready:=False; // no more posting here
            obj.proxy:=nil;
            obj.con:=nil;
            xcon:=con; con:=nil;
            xready:=ready; ready:=false;

            if active then // outgoing
              begin
              active:=False;
              if not xproreq then // only we have the Request object
                begin
                Dec(TotalPending);
                DataReq:=req;
                if assigned(OnDebugLog) then
                  Debug('## '+nr+'. DONE PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+uri,RLOG_PROXY);
                end
              else if assigned(OnDebugLog) then
                Debug('## '+nr+'. KILL PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+uri,RLOG_PROXY);
              req:=nil;
              if _done and not xproready then // Request not Posted
                xcon.PostEvent(ServerStop);
              end
            else if queued>0 then
              begin
              req:=nil;

              Dec(Total_Queued);
              Dec(My_Queued[QueueIndex]);

              if assigned(OnDebugLog) then
                Debug('## '+nr+'. SKIP PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+uri,RLOG_PROXY);

              if queued=2 then
                begin
                queued:=0;
                toFree:=obj;
                end
              else
                begin
                queued:=0;
                if My_Queues[QueueIndex].First=obj then
                  begin
                  My_Queues[QueueIndex].removeFirst;
                  toFree:=obj;
                  end
                else if My_Queues[QueueIndex].Last=obj then
                  begin
                  My_Queues[QueueIndex].removeLast;
                  toFree:=obj;
                  end;
                end;
              end
            else
              begin
              req:=nil;
              if assigned(OnDebugLog) then
                Debug('## '+nr+'. KILL PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+uri,RLOG_PROXY);
              end;

            if xready and assigned(Evnt) then
              xcon.PostEvent(Evnt);
            end
          else if assigned(req) then
            begin
            active:=False;
            Dec(TotalPending);
            DataReq:=req;
            req:=nil;
            if assigned(OnDebugLog) then
              Debug('## '+nr+'. DONE PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+uri,RLOG_PROXY);
            end
          else
            if assigned(OnDebugLog) then
              Debug('## '+nr+'. CANCEL PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+uri,RLOG_PROXY);
          end
        else
          if assigned(OnDebugLog) then
            Debug('## '+nr+'. CLOSED PROXY (objects='+Int2Str(TotalCreated)+', pending='+Int2Str(TotalPending)+', queued='+Int2Str(Total_Queued)+') '+uri,RLOG_PROXY);
        end;
    finally
      ProxyDataCS.Release;
      end;

    RtcFreeAndNil(toFree);

    if assigned(DataReq) then // component not used anymore
      Event_PostReturn(DataReq);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Proxy_Clear',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcDataRouter.ReadyForNextRequest(FromQueue:integer):boolean;
  var
    cli_pd:TRtcProxyData;
    ocli:TObject absolute cli_pd;
    x,MyQueue:TXObjList;
  begin
  try
    Result:=False;

    x:=nil;
    ProxyDataCS.Acquire;
    try
      if (FromQueue>=0) and (FromQueue<QueueCnt) then
        begin
        MyQueue:=My_Queues[FromQueue];
        while MyQueue.Count>My_Queued[FromQueue] do
          begin
          cli_pd:=TRtcProxyData(MyQueue.Last);
          if cli_pd.proxy=nil then
            begin
            MyQueue.removeLast;
            if x=nil then
              x:=TXObjList.Create(8);
            x.AddLast(cli_pd);
            end
          else
            Break;
          end;

        while MyQueue.Count>0 do
          begin
          MyQueue.extractFirst(ocli);
          if cli_pd.proxy<>nil then
            begin
            cli_pd.proxy.queued:=2;
            Result:=cli_pd.con.PostEvent(Server_PostRequest);
            if Result then Break;
            end
          else
            begin
            if x=nil then
              x:=TXObjList.Create(8);
            x.AddLast(cli_pd);
            end;
          end;
        end;
    finally
      ProxyDataCS.Release;
      end;
    if assigned(x) then
      begin
      while x.Count>0 do
        begin
        x.extractFirst(ocli);
        RtcFreeAndNil(ocli);
        end;
      RtcFreeAndNil(x);
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.ReadyForNextRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcDataRouter.WaitingRequests(FromQueue:integer): integer;
  var
    obj:TRtcProxyData;
    oreq:TObject absolute obj;
    MyQueue,x:TXObjList;
  begin
  try
    x:=nil;
    ProxyDataCS.Acquire;
    try
      if (FromQueue>=0) and (FromQueue<QueueCnt) then
        begin
        MyQueue:=My_Queues[FromQueue];
        while MyQueue.Count>My_Queued[FromQueue] do
          begin
          obj:=TRtcProxyData(MyQueue.Last);
          if not assigned(obj.proxy) then
            begin
            MyQueue.removeLast;
            if x=nil then
              x:=tXObjList.Create(8);
            x.AddLast(obj);
            end
          else
            begin
            while MyQueue.Count>My_Queued[FromQueue] do
              begin
              obj:=TRtcProxyData(MyQueue.First);
              if obj.proxy<>nil then Break;
              MyQueue.removeFirst;
              if x=nil then
                x:=tXObjList.Create(8);
              x.AddLast(obj);
              end;
            Break;
            end;
          end;
        Result:=My_Queued[FromQueue];
        end
      else
        Result:=0;
    finally
      ProxyDataCS.Release;
      end;
    if assigned(x) then
      begin
      while x.Count>0 do
        begin
        x.extractFirst(oreq);
        RtcFreeAndNil(oreq);
        end;
      RtcFreeAndNil(x);
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.WaitingRequests',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_CheckRequest(Sender:TRtcConnection);
  begin
  try
    TRtcDataServer(Sender).SetActiveLink(self);
    Event_CheckRequest(TRtcDataServer(Sender));

    with Sender do
      if Request.Accepted then
        begin
        if Request.ManualRead then
          Request.Info.asBoolean['$man']:=True;
        if Request.ChunkedTransferEncoding then
          Request.ManualRead:=False;

        if FTimeout_CheckRequest<>0 then
          Timeout.Enable(FTimeout_CheckRequest);
        end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_CheckRequest',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_RequestAccepted(Sender: TRtcConnection);
  begin
  try
    if assigned(Link) then
      Link.Call_RequestAccepted(Sender);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_RequestAccepted',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_DataReceived(Sender:TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    CB:TRtcRouterContentBody;
    sent:boolean;
  begin
  try
    SetLength(s,0);
    if Srv.Request.Complete then
      begin
      if not Srv.Request.Info.asBoolean['$srv-out'] then // request not posted
        begin
        if not Srv.Request.Info.asBoolean['$man'] then // Content buffering was enabled
          begin
          CB:=TRtcRouterContentBody.Create;
          try
            CB.BodyEx:=Srv.ReadEx;

            if FTimeout_RequestReceived<>0 then
              Srv.Timeout.Enable(FTimeout_RequestReceived)
            else if FTimeout_RequestDataIn<>0 then
              Srv.Timeout.Enable(FTimeout_RequestDataIn);

            Event_RequestReceived(Srv,CB);

            Srv.Request.ContentLength:=length(CB.BodyEx);
            Srv.Request.ChunkedTransferEncoding:=False;

            Srv.Request.Info.asBoolean['$srv-out']:=True;

            obj:=Proxy_PostRequest(Srv,CB.BodyEx,False);

            if assigned(OnDebugLog) then
              Debug('<< '+obj.nr+'. SRV : Request.Data ('+Int2Str(length(CB.BodyEx))+') - complete (buffered) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          finally
            RtcFreeAndNil(CB);
            end;
          end
        else
          begin
          if FTimeout_RequestReceived<>0 then
            Srv.Timeout.Enable(FTimeout_RequestReceived)
          else if FTimeout_RequestDataIn<>0 then
            Srv.Timeout.Enable(FTimeout_RequestDataIn);

          Event_RequestReceived(Srv,nil);

          s:=Srv.ReadEx;

          Srv.Request.ContentLength:=length(s);
          Srv.Request.ChunkedTransferEncoding:=False;

          Srv.Request.Info.asBoolean['$srv-out']:=True;
          obj:=Proxy_PostRequest(Srv,s,False);

          if assigned(OnDebugLog) then
            Debug('<< '+obj.nr+'. SRV : Request.Data ('+Int2Str(length(s))+') - complete '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          end;

        sent:=True;
        end
      else
        begin
        obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
        if assigned(obj) then
          begin
          if FTimeout_RequestReceived<>0 then
            Srv.Timeout.Enable(FTimeout_RequestReceived)
          else if FTimeout_RequestDataIn<>0 then
            Srv.Timeout.Enable(FTimeout_RequestDataIn);

          Event_RequestReceived(Srv,nil);

          s:=Srv.ReadEx;
          if length(s)>0 then
            begin
            if assigned(OnDebugLog) then
              Debug('<< '+obj.nr+'. SRV : Request.Data ('+Int2Str(length(s))+') - complete '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
            sent:=obj.PostDataEvent(ClientCanWrite,s,False, False);
            end
          else
            begin
            if assigned(OnDebugLog) then
              Debug('<< '+obj.nr+'. SRV : Request.Data (NO DATA) - complete '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
            sent:=True;
            end;
          end
        else
          begin
          if assigned(OnDebugLog) then
            Debug('§§§§ SRV : Request.Data - OBJ is NULL for '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          sent:=False;
          end;
        end;
      end
    else if not Srv.Request.ManualRead then
      begin
      if FTimeout_RequestDataIn<>0 then
        Srv.Timeout.Enable(FTimeout_RequestDataIn);

      obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('<< '+obj.nr+'. SRV : Request.Data - Buffering ... '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
        else
          Debug('<< SRV : Request.Data - Buffering ... '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      sent:=True;
      end
    else if not Srv.Request.Info.asBoolean['$srv-out'] then // request not posted
      begin
      s:=Srv.ReadEx;

      if Srv.Request.Complete then
        begin
        if FTimeout_RequestReceived<>0 then
          Srv.Timeout.Enable(FTimeout_RequestReceived)
        else if FTimeout_RequestDataIn<>0 then
          Srv.Timeout.Enable(FTimeout_RequestDataIn);

        Event_RequestReceived(Srv,nil);

        Srv.Request.ContentLength:=length(s);
        Srv.Request.ChunkedTransferEncoding:=False;
        end
      else
        begin
        if FTimeout_RequestDataIn<>0 then
          Srv.Timeout.Enable(FTimeout_RequestDataIn);
        end;

      Srv.Request.Info.asBoolean['$srv-out']:=True;
      obj:=Proxy_PostRequest(Srv,s,Srv.Request.ManualRead and not Srv.Request.Complete);

      if assigned(OnDebugLog) then
        Debug('<< '+obj.nr+'. SRV : Request.Data ('+Int2Str(length(s))+') - started '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);

      sent:=True;
      end
    else if Srv.Request.Info.asBoolean['$cli-wait'] then
      begin
      Srv.Request.Info.asBoolean['$cli-wait']:=False;

      obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
      if assigned(obj) then
        begin
        s:=Srv.ReadEx;

        if Srv.Request.Complete then
          begin
          if FTimeout_RequestReceived<>0 then
            Srv.Timeout.Enable(FTimeout_RequestReceived)
          else if FTimeout_RequestDataIn<>0 then
            Srv.Timeout.Enable(FTimeout_RequestDataIn);

          Event_RequestReceived(Srv,nil);
          end
        else
          begin
          if FTimeout_RequestDataIn<>0 then
            Srv.Timeout.Enable(FTimeout_RequestDataIn);
          end;

        if length(s)>0 then
          begin
          if assigned(OnDebugLog) then
            Debug('<< '+obj.nr+'. SRV : Request.Data ('+Int2Str(length(s))+') - client waiting '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          sent:=obj.PostDataEvent(ClientCanWrite,s,False, Srv.Request.ManualRead and not Srv.Request.Complete);
          end
        else
          begin
          if assigned(OnDebugLog) then
            Debug('<< '+obj.nr+'. SRV : Request.Data (NO DATA) - client waiting '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          sent:=True;
          Srv.Request.Info.asBoolean['$cli-wait']:=True;
          end;
        end
      else
        begin
        if assigned(OnDebugLog) then
          Debug('§§§§ SRV : Request.Data - OBJ is NULL for '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        sent:=False;
        end;
      end
    else
      begin
      if FTimeout_RequestDataIn<>0 then
        Srv.Timeout.Enable(FTimeout_RequestDataIn);
        
      obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('<< '+obj.nr+'. SRV : Request.Data - server ready (signalled) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
        else
          Debug('<< SRV : Request.Data - server ready (signalled) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      Srv.Request.Info.asBoolean['$srv-ready']:=True;
      sent:=True;
      end;

    if not sent then
      begin
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('§§§§§§§§§§ << '+obj.nr+'. SRV : Request.Data DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
        else
          Debug('§§§§§§§§§§ << SRV : Request.Data DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      Srv.Disconnect;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_DataReceived',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_DataSent(Sender:TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    sent,closenow,notify:boolean;
  begin
  try
    SetLength(s,0);
    if Srv.Response.Done then
      begin
      obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
      if assigned(obj) then
        begin
        if assigned(OnDebugLog) then
          Debug('** '+obj.nr+'. SRV : Response.Done '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        obj.PostDone(nil);
        end
      else
        if assigned(OnDebugLog) then
          Debug('** SRV : Response.Done '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      end
    else
      begin
      obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
      if assigned(obj) then
        begin
        if FTimeout_ResponseDataOut<>0 then
          Srv.Timeout.Enable(FTimeout_ResponseDataOut);

        s:=obj.Get_Data(closenow,notify);
        if length(s)>0 then
          begin
          if assigned(OnDebugLog) then
            Debug('>> '+obj.nr+'. SRV : Response.Sent (WRITE '+Int2Str(length(s))+', left='+Int2Str(Srv.Response.ContentLength-Srv.Response.ContentOut)+'/'+Srv.Response.ValueCS['CONTENT-LENGTH']+') '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          Srv.WriteEx(s);
          Srv.Flush;
          end
        else
          if assigned(OnDebugLog) then
            Debug('>> '+obj.nr+'. SRV : Response.Sent (NO WRITE, left='+Int2Str(Srv.Response.ContentLength-Srv.Response.ContentOut)+'/'+Srv.Response.ValueCS['CONTENT-LENGTH']+') '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        if closenow and Srv.Request.Close then
          begin
          sent:=True;
          if assigned(OnDebugLog) then
            Debug('§§ '+obj.nr+'. SRV : Response.Sent DISCONNECTING (requested) after '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          Srv.Disconnect;
          end
        else if Srv.Response.Done or
           ( not Srv.Request.Close and (Srv.Response.ContentLength-Srv.Response.ContentOut=0) ) then
          begin
          sent:=True;
          obj.PostDone(nil);
          end
        else if notify then
          sent:=obj.PostNotifyEvent(ClientCanRead)
        else
          sent:=True;
        end
      else
        begin
        if assigned(OnDebugLog) then
          Debug('§§§§ SRV : Response.Sent - OBJ is NULL for '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        sent:=false;
        end;

      if not sent then
        begin
        if assigned(OnDebugLog) then
          if assigned(obj) then
            Debug('§§§§§§§§§§ >> '+obj.nr+'. SRV : Response.Sent DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
          else
            Debug('§§§§§§§§§§ >> SRV : Response.Sent DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.Disconnect;
        end;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_DataSent',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_ResponseDone(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
  begin
  try
    if FTimeout_ResponseSent<>0 then
      Srv.Timeout.Enable(FTimeout_ResponseSent)
    else if FTimeout_ResponseDataOut<>0 then
      Srv.Timeout.Enable(FTimeout_ResponseDataOut);

    Event_ResponseSent(Srv);

    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if assigned(OnDebugLog) then
      if assigned(obj) then
        Debug(obj.nr+'. <<<<<<<<<<<<<<================== SRV : Response.Done '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
      else
        Debug('SRV : Response.Done '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);

    if assigned(Link) then
      Link.Call_ResponseDone(Sender);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_ResponseDone',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_Disconnect(Sender:TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
  begin
  try
    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if Srv.Response.Done then
      begin
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('§§ '+obj.nr+'. SRV : Disconnected. '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
        else
          Debug('§§ SRV : Disconnected. '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      end
    else
      begin
      if not Srv.Request.Info.asBoolean['$stop'] then
        begin
        if Srv.Request.Complete then
          Event_ResponseSendAbort(Srv)
        else
          Event_RequestReceiveAbort(Srv);
        end;
      if assigned(obj) then
        begin
        if assigned(OnDebugLog) then
          Debug('§§§§§§§§§§ '+obj.nr+'. SRV : Disconnect - DISCONNECTED! '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        if Srv.Request.Info.asBoolean['$stop'] then
          Proxy_Clear(obj,nil,False)
        else
          Proxy_Clear(obj,ClientStop,False);
        end
      else
        if assigned(OnDebugLog) then
          Debug('§§§§§§§§§§ SRV : Disconnect - DISCONNECTED! '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      end;

    if assigned(Link) then
      Link.Call_Disconnect(Sender);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_Disconnect',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_WSConnect(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
  begin
  try
    Srv.Timeout.Disable;
    Srv.Request.ManualRead:=True;

    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if assigned(OnDebugLog) then
      if assigned(obj) then
        Debug(obj.nr+'. <<<<<<<<<<<<<<================== SRV : WebSock.Connect '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
      else
        Debug('SRV : WebSock.Connect '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);

    if assigned(obj) then
      obj.PostWSOpen(ClientWSCanRead);

    if assigned(Link) then
      Link.Call_WSConnect(Sender);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_WSConnect',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_WSDataReceived(Sender:TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    sent:boolean;
  begin
  try
    SetLength(s,0);
    if Srv.Request.Info.asBoolean['$cli-wait'] then
      begin
      Srv.Request.Info.asBoolean['$cli-wait']:=False;
      obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
      if assigned(obj) then
        begin
        s:=Srv.ReadEx;
        if length(s)>0 then
          begin
          if assigned(OnDebugLog) then
            Debug('<< '+obj.nr+'. SRV : WebSock.Data ('+Int2Str(length(s))+') - client waiting '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          sent:=obj.PostDataEvent(ClientWSCanWrite,s,False,Srv.Request.ManualRead);
          end
        else
          begin
          if assigned(OnDebugLog) then
            Debug('<< '+obj.nr+'. SRV : WebSock.Data (NO DATA) - client waiting '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          sent:=True;
          Srv.Request.Info.asBoolean['$cli-wait']:=True;
          end;
        end
      else
        begin
        if assigned(OnDebugLog) then
          Debug('§§§§ SRV : WebSock.Data - OBJ is NULL for '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        sent:=False;
        end;
      end
    else
      begin
      obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('<< '+obj.nr+'. SRV : WebSock.Data - server ready (signalled) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
        else
          Debug('<< SRV : WebSock.Data - server ready (signalled) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      Srv.Request.Info.asBoolean['$srv-ready']:=True;
      sent:=True;
      end;

    if not sent then
      begin
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('§§§§§§§§§§ << '+obj.nr+'. SRV : WebSock.Data DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
        else
          Debug('§§§§§§§§§§ << SRV : WebSock.Data DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      Srv.Disconnect;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_WSDataReceived',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_WSDataSent(Sender:TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    sent,closenow,notify:boolean;
  begin
  try
    SetLength(s,0);

    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      s:=obj.Get_Data(closenow,notify);
      if length(s)>0 then
        begin
        if assigned(OnDebugLog) then
          Debug('>> '+obj.nr+'. SRV : WebSock.Sent (WRITE '+Int2Str(length(s))+') '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.WriteEx(s);
        end
      else if assigned(OnDebugLog) then
        Debug('>> '+obj.nr+'. SRV : WebSock.Sent (NO WRITE) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);

      if closenow and Srv.Request.Close then
        begin
        sent:=True;
        if assigned(OnDebugLog) then
          Debug('§§ '+obj.nr+'. SRV : WebSock.Sent DISCONNECTING (requested) after '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.Disconnect;
        end
      else if notify then
        sent:=obj.PostNotifyEvent(ClientWSCanRead)
      else
        sent:=True;
      end
    else
      begin
      if assigned(OnDebugLog) then
        Debug('§§§§ SRV : WebSock.Sent - OBJ is NULL for '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      sent:=false;
      end;

    if not sent then
      begin
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('§§§§§§§§§§ >> '+obj.nr+'. SRV : WebSock.Sent DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
        else
          Debug('§§§§§§§§§§ >> SRV : WebSock.Sent DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      Srv.Disconnect;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_WSDataSent',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_WSDisconnect(Sender:TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
  begin
  try
    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      if assigned(OnDebugLog) then
        Debug('§§§§§§§§§§ '+obj.nr+'. SRV : WebSock.Disconnect - DISCONNECTED! '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      if Srv.Request.Info.asBoolean['$stop'] then
        Proxy_Clear(obj,nil,False)
      else
        Proxy_Clear(obj,ClientStop,False);
      end
    else if assigned(OnDebugLog) then
      Debug('§§§§§§§§§§ SRV : WebSock.Disconnect - DISCONNECTED! '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);

    if assigned(Link) then
      Link.Call_WSDisconnect(Sender);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_WSDisconnect',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_ListenStart(Sender:TRtcConnection);
  begin
  try
    if assigned(FOnListenStart) then
      FOnListenStart(Sender);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_ListenStart',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_ListenStop(Sender:TRtcConnection);
  begin
  try
    if assigned(FOnListenStop) then
      FOnListenStop(Sender);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_ListenStop',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_SessionOpen(Sender: TRtcConnection);
  begin
  try
    if assigned(FOnSessionOpen) then
      FOnSessionOpen(Sender);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_SessionOpen',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Call_SessionClose(Sender: TRtcConnection);
  begin
  try
    if assigned(FOnSessionClose) then
      FOnSessionClose(Sender);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Call_SessionClose',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ClientCanRead(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    sent:boolean;
  begin // Sender = Client connection
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(obj) and Cli.Request.Active then
      begin
      if Cli.Request.Info.asBoolean['$cli-ready'] then
        begin
        Cli.Request.Info.asBoolean['$cli-ready']:=False;
        s:=Cli.ReadEx;

        if Cli.Response.Done then
          begin
          if FTimeout_ResponseReceived<>0 then
            Cli.Timeout.Enable(FTimeout_ResponseReceived)
          else if FTimeout_ResponseDataIn<>0 then
            Cli.Timeout.Enable(FTimeout_ResponseDataIn);

          Event_ResponseReceived(Cli,nil);
          end
        else
          begin
          if FTimeout_ResponseDataIn<>0 then
            Cli.Timeout.Enable(FTimeout_ResponseDataIn);
          end;

        if length(s)>0 then
          begin
          if assigned(OnDebugLog) then
            Debug('>> '+obj.nr+'. CLI : Client.CanRead ('+Int2Str(length(s))+') client ready '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
          sent:=obj.PostDataEvent(ServerCanWrite,s,Cli.Response.Done, Cli.Response.ManualRead and not Cli.Response.Done);
          end
        else
          begin
          if assigned(OnDebugLog) then
            Debug('>> '+obj.nr+'. CLI : Client.CanRead (NO DATA) client ready '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
          if Cli.Response.Done then
            obj.PostDone(ServerWriteDone)
          else
            Cli.Request.Info.asBoolean['$srv-wait']:=True;
          sent:=True;
          end;
        Cli.AfterManualRead;
        end
      else
        begin
        if FTimeout_ResponseDataIn<>0 then
          Cli.Timeout.Enable(FTimeout_ResponseDataIn);

        if assigned(OnDebugLog) then
          Debug('>> '+obj.nr+'. CLI : Client.CanRead - server waiting (signalled) '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        Cli.Request.Info.asBoolean['$srv-wait']:=True;
        sent:=True;
        end;

      if not sent then
        begin
        if assigned(OnDebugLog) then
          if assigned(obj) then
            Debug('§§§§§§§§§§ >> '+obj.nr+'. CLI : Client.CanRead - RECONNECTING '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE)
          else
            Debug('§§§§§§§§§§ >> CLI : Client.CanRead - RECONNECTING '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        Cli.Request.Skip;
        Cli.Reconnect;
        end;
      end
    else if assigned(OnDebugLog) then
      Debug('§§§§ CLI : Client.CanRead - IGNORING Leftovers (request NOT active)',RLOG_ROUTE);

  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.ClientCanRead',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ClientCanWrite(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
  begin // Sender = Client connection
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(obj) and Cli.Request.Active then
      begin
      if FTimeout_RequestDataOut<>0 then
        Cli.Timeout.Enable(FTimeout_RequestDataOut);

      s:=obj.GetData2;
      if length(s)>0 then
        begin
        if assigned(OnDebugLog) then
          Debug('<< '+obj.nr+'. CLI : Client.CanWrite ('+Int2Str(length(s))+') '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        Cli.WriteEx(s);
        Cli.Flush;
        end
      else
        if assigned(OnDebugLog) then
          Debug('<< '+obj.nr+'. CLI : Client.CanWrite (NO DATA) '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      end
    else if assigned(OnDebugLog) then
      Debug('§§§ CLI : IGNORING Client.CanWrite for '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.ClientCanWrite',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ClientStop(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
  begin
  try
    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      if Cli.Request.Active then
        begin
        if assigned(OnDebugLog) then
          Debug('§§§§§§§§§§ >> '+obj.nr+'. CLI : Client.Stop (DISCONNECTING) '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);

        Cli.Request.Info.asBoolean['$stop']:=True;
        Cli.Request.Skip;
        Cli.Reconnect;
        end
      else
        begin
        if assigned(OnDebugLog) then
          Debug('§§§§§§§§§§ >> '+obj.nr+'. CLI : Client.Stop (SKIPPING) '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);

        Cli.Request.Info.asBoolean['$stop']:=True;
        Cli.Request.Skip;
        end;
      end
    else if assigned(OnDebugLog) then
      Debug('§§§§§§§§§§ >> CLI : Client.Stop (IGNORING) '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.ClientStop',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ClientWSCanRead(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    sent:boolean;
  begin // Sender = Client connection
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(obj) and Cli.Request.Active then
      begin
      if Cli.Request.Info.asBoolean['$cli-ready'] then
        begin
        Cli.Request.Info.asBoolean['$cli-ready']:=False;
        s:=Cli.ReadEx;
        if length(s)>0 then
          begin
          if assigned(OnDebugLog) then
            Debug('>> '+obj.nr+'. CLI : WebSock.CanRead ('+Int2Str(length(s))+') client ready '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
          sent:=obj.PostDataEvent(ServerWSCanWrite,s, False, Cli.Response.ManualRead);
          end
        else
          begin
          if assigned(OnDebugLog) then
            Debug('>> '+obj.nr+'. CLI : WebSock.CanRead (NO DATA) client ready '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
          Cli.Request.Info.asBoolean['$srv-wait']:=True;
          sent:=True;
          end;
        Cli.AfterManualRead;
        end
      else
        begin
        if assigned(OnDebugLog) then
          Debug('>> '+obj.nr+'. CLI : WebSock.CanRead - server waiting (signalled) '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        Cli.Request.Info.asBoolean['$srv-wait']:=True;
        sent:=True;
        end;

      if not sent then
        begin
        if assigned(OnDebugLog) then
          if assigned(obj) then
            Debug('§§§§§§§§§§ >> '+obj.nr+'. CLI : WebSock.CanRead - RECONNECTING '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE)
          else
            Debug('§§§§§§§§§§ >> CLI : WebSock.CanRead - RECONNECTING '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        Cli.Request.Skip;
        Cli.Reconnect;
        end;
      end
    else if assigned(OnDebugLog) then
      Debug('§§§§ CLI : WebSock.CanRead - IGNORING Leftovers (request NOT active)',RLOG_ROUTE);

  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.ClientWSCanRead',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ClientWSCanWrite(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
  begin // Sender = Client connection
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(obj) and Cli.Request.Active then
      begin
      if not Cli.isWebSocket then Exit;

      s:=obj.GetData2;
      if length(s)>0 then
        begin
        if assigned(OnDebugLog) then
          Debug('<< '+obj.nr+'. CLI : WebSock.CanWrite ('+Int2Str(length(s))+') '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        Cli.WriteEx(s);
        end
      else if assigned(OnDebugLog) then
        Debug('<< '+obj.nr+'. CLI : WebSock.CanWrite (NO DATA) '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      end
    else if assigned(OnDebugLog) then
      Debug('§§§ CLI : IGNORING WebSock.CanWrite for '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.ClientWSCanWrite',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ServerCanRead(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    sent:boolean;
  begin // Sender = Server connection
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      if Srv.Request.Info.asBoolean['$srv-ready'] then
        begin
        Srv.Request.Info.asBoolean['$srv-ready']:=False;
        s:=Srv.ReadEx;
        if Srv.Request.Complete then
          begin
          if FTimeout_RequestReceived<>0 then
            Srv.Timeout.Enable(FTimeout_RequestReceived)
          else if FTimeout_RequestDataIn<>0 then
            Srv.Timeout.Enable(FTimeout_RequestDataIn);

          Event_RequestReceived(Srv,nil);
          end
        else
          begin
          if FTimeout_RequestDataIn<>0 then
            Srv.Timeout.Enable(FTimeout_RequestDataIn);
          end;

        if length(s)>0 then
          begin
          if assigned(OnDebugLog) then
            Debug('<< '+obj.nr+'. SRV : Server.CanRead ('+Int2Str(length(s))+' bytes) - server ready '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          sent:=obj.PostDataEvent(ClientCanWrite,s,False, Srv.Request.ManualRead and not Srv.Request.Complete);
          end
        else
          begin
          if assigned(OnDebugLog) then
            Debug('<< '+obj.nr+'. SRV : Server.CanRead (NO DATA) - server ready '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          Srv.Request.Info.asBoolean['$cli-wait']:=True;
          sent:=True;
          end;
        Srv.AfterManualRead;
        end
      else
        begin
        if FTimeout_RequestDataIn<>0 then
          Srv.Timeout.Enable(FTimeout_RequestDataIn);

        if assigned(OnDebugLog) then
          Debug('<< '+obj.nr+'. SRV : Server.CanRead - client waiting (signalled) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.Request.Info.asBoolean['$cli-wait']:=True;
        sent:=True;
        end;
      end
    else
      begin
      if assigned(OnDebugLog) then
        Debug('§§§§ SRV : Server.CanRead - OBJ is NULL for '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      sent:=False;
      end;

    if not sent then
      begin
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('§§§§§§§§§§ << '+obj.nr+'. SRV : Server.CanRead DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
        else
          Debug('§§§§§§§§§§ << SRV : Server.CanRead DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      Srv.Disconnect;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Server.CanRead',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ServerCanWriteFirst(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    closenow:boolean;
  begin // Sender = Server connection
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      Srv.Response.HeaderText:=obj.head;
      Srv.Response.Status(obj.scode,obj.stext);

      if obj.xclose then
        begin
        if not Srv.Request.Close then
          begin
          if (Srv.Response.ValueCS['CONTENT-LENGTH']='') then
            begin
            Srv.Request.Close:=True;
            Srv.Response['Connection']:='close';
            end
          else if Srv.Response.ValueCS['CONNECTION']<>'' then
            Srv.Response['Connection']:='';
          end
        else
          Srv.Request.Close:=obj.xclose;
        end;

      if FTimeout_ResponseDataOut<>0 then
        Srv.Timeout.Enable(FTimeout_ResponseDataOut);

      s:=obj.GetData(closenow);
      if assigned(OnDebugLog) then
        Debug('>> '+obj.nr+'. SRV : Server.CanWriteFirst ('+Int2Str(length(s))+', len='+Srv.Response.ValueCS['CONTENT-LENGTH']+') '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      Srv.WriteHeader;
      if length(s)>0 then
        Srv.WriteEx(s);
      Srv.Flush;
      if closenow and Srv.Request.Close then
        begin
        if assigned(OnDebugLog) then
          Debug('§§ '+obj.nr+'. SRV : Server.CanWriteFirst DISCONNECTING (requested) after '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.Disconnect;
        end;
      end
    else
      if assigned(OnDebugLog) then
        Debug('?? SRV : IGNORING Server.CanWrite for '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Server.CanWriteFirst',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ServerCanWrite(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    closenow:boolean;
  begin // Sender = Server connection
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      if FTimeout_ResponseDataOut<>0 then
        Srv.Timeout.Enable(FTimeout_ResponseDataOut);

      s:=obj.GetData(closenow);
      if length(s)>0 then
        begin
        if assigned(OnDebugLog) then
          Debug('>> '+obj.nr+'. SRV : Server.CanWrite ('+Int2Str(length(s))+') '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.WriteEx(s);
        Srv.Flush;
        end
      else
        if assigned(OnDebugLog) then
          Debug('>> '+obj.nr+'. SRV : Server.CanWrite (NO DATA) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      if closenow and Srv.Request.Close then
        begin
        if assigned(OnDebugLog) then
          Debug('§§ '+obj.nr+'. SRV : Server.CanWrite DISCONNECTING (requested) after '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.Disconnect;
        end;
      end
    else
      if assigned(OnDebugLog) then
        Debug('?? SRV : IGNORING Server.CanWrite for '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Server.CanWrite',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ServerWriteDone(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    closenow:boolean;
  begin // Sender = Server connection
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      if FTimeout_ResponseDataOut<>0 then
        Srv.Timeout.Enable(FTimeout_ResponseDataOut);

      s:=obj.GetData(closenow);
      if length(s)>0 then
        begin
        if assigned(OnDebugLog) then
          Debug('>> '+obj.nr+'. SRV : ServerWriteDone ('+Int2Str(length(s))+') '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.WriteEx(s);
        Srv.Flush;
        end
      else
        if assigned(OnDebugLog) then
          Debug('>> '+obj.nr+'. SRV : ServerWriteDone (NO DATA) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      if closenow and Srv.Request.Close then
        begin
        if assigned(OnDebugLog) then
          Debug('§§ '+obj.nr+'. SRV : ServerWriteDone DISCONNECTING (requested) after '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.Disconnect;
        end;
      end
    else
      if assigned(OnDebugLog) then
        Debug('?? IGNORING ServerWriteDone for '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.ServerWriteDone',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ServerStop(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
  begin
  try
    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if assigned(OnDebugLog) then
      if assigned(obj) then
        Debug('§§§§§§§§§§ >> '+obj.nr+'. SRV : ServerStop (DISCONNECTING) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
      else
        Debug('§§§§§§§§§§ >> SRV : ServerStop (DISCONNECTING) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
    Srv.Request.Info.asBoolean['$stop']:=True;
    Srv.Disconnect;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.ServerStop',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ServerWSCanRead(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    sent:boolean;
  begin // Sender = Server connection
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      if Srv.Request.Info.asBoolean['$srv-ready'] then
        begin
        Srv.Request.Info.asBoolean['$srv-ready']:=False;
        s:=Srv.ReadEx;
        if length(s)>0 then
          begin
          if assigned(OnDebugLog) then
            Debug('<< '+obj.nr+'. SRV : WebSock.CanRead ('+Int2Str(length(s))+' bytes) - server ready '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          sent:=obj.PostDataEvent(ClientWSCanWrite,s, False, Srv.Request.ManualRead);
          end
        else
          begin
          if assigned(OnDebugLog) then
            Debug('<< '+obj.nr+'. SRV : WebSock.CanRead (NO DATA) - server ready '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
          Srv.Request.Info.asBoolean['$cli-wait']:=True;
          sent:=True;
          end;
        Srv.AfterManualRead;
        end
      else
        begin
        if assigned(OnDebugLog) then
          Debug('<< '+obj.nr+'. SRV : WebSock.CanRead - client waiting (signalled) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.Request.Info.asBoolean['$cli-wait']:=True;
        sent:=True;
        end;
      end
    else
      begin
      if assigned(OnDebugLog) then
        Debug('§§§§ SRV : WebSock.CanRead - OBJ is NULL for '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      sent:=False;
      end;

    if not sent then
      begin
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('§§§§§§§§§§ << '+obj.nr+'. SRV : WebSock.CanRead DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE)
        else
          Debug('§§§§§§§§§§ << SRV : WebSock.CanRead DISCONNECTING '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
      Srv.Disconnect;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.ServerWSCanRead',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.ServerWSCanWrite(Sender: TRtcConnection);
  var
    Srv:TRtcDataServer absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    closenow:boolean;
  begin // Sender = Server connection
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Srv.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      if not Srv.isWebSocket then Exit;
      
      s:=obj.GetData(closenow);
      if length(s)>0 then
        begin
        if assigned(OnDebugLog) then
          Debug('>> '+obj.nr+'. SRV : WebSock.CanWrite ('+Int2Str(length(s))+') '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.WriteEx(s);
        end
      else if assigned(OnDebugLog) then
        Debug('>> '+obj.nr+'. SRV : WebSock.CanWrite (NO DATA) '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);

      if closenow and Srv.Request.Close then
        begin
        if assigned(OnDebugLog) then
          Debug('§§ '+obj.nr+'. SRV : WebSock.CanWrite DISCONNECTING (requested) after '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
        Srv.Disconnect;
        end;
      end
    else if assigned(OnDebugLog) then
      Debug('?? SRV : IGNORING WebSock.CanWrite for '+Srv.Request.Method+' '+Srv.Request.URI,RLOG_ROUTE);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.ServerWSCanWrite',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Client_BeginRequest(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
  begin
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      if assigned(OnDebugLog) then
        Debug('** '+obj.nr+'. ================>>>>>>>>>>>>>>> BEGIN REQUEST',RLOG_ROUTE);

      if obj.StartNow then
        begin
        if assigned(OnDebugLog) then
          Debug('<< '+obj.nr+'. CLI : Request.Begin (Len='+Int2Str(Cli.Request.ContentLength)+')'+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);

        if FTimeout_RequestBegin<>0 then
          Cli.Timeout.Enable(FTimeout_RequestBegin)
        else if FTimeout_RequestDataOut<>0 then
          Cli.Timeout.Enable(FTimeout_RequestDataOut);

        Event_RequestBegin(Cli);

        s:=obj.GetData2;

        Client_WriteHeader(Cli);
        if length(s)>0 then
          Cli.WriteEx(s);
        Cli.Flush;
        end
      else
        begin
        if assigned(OnDebugLog) then
          Debug('<< '+obj.nr+'. CLI : Request.Begin - SKIPPING '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);

        Cli.Request.Skip;
        end;
      end
    else
      begin
      if assigned(OnDebugLog) then
        begin
        Debug('<< CLI : Request.Begin - SKIPPING '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        Log('<< CLI : Request.Begin - SKIPPING '+Cli.Request.Method+' '+Cli.Request.URI,'ERROR');
        end;

      Cli.Request.Skip;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Client_BeginRequest "'+Cli.Request.Method+' '+Cli.Request.URI+'"',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Client_DataSent(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    sent,notify:boolean;
  begin
  try
    SetLength(s,0);
    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if Cli.Request.Complete then
      begin
      if not Cli.Request.Info.asBoolean['$done'] then
        begin
        Cli.Request.Info.asBoolean['$done']:=True;

        if FTimeout_RequestSent<>0 then
          Cli.Timeout.Enable(FTimeout_RequestSent)
        else if FTimeout_RequestDataOut<>0 then
          Cli.Timeout.Enable(FTimeout_RequestDataOut);

        Event_RequestSent(Cli);

        if assigned(OnDebugLog) then
          if assigned(obj) then
            Debug('** '+obj.nr+'. CLI : Request.Complete',RLOG_ROUTE)
          else
            Debug('** CLI : Request.Complete',RLOG_ROUTE);
        end
      else if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('** '+obj.nr+'. CLI : Request.Complete (2nd time)',RLOG_ROUTE)
        else
          Debug('** CLI : Request.Complete (2nd time)',RLOG_ROUTE);
      Sent:=True;
      end
    else
      begin
      if assigned(obj) then
        begin
        if assigned(OnDebugLog) then
          Debug('<< '+obj.nr+'. CLI : Request.Sent (left='+Int2Str(Cli.Request.ContentLength-Cli.Request.ContentOut)+'/'+Cli.Request.ValueCS['CONTENT-LENGTH']+') '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);

        if FTimeout_RequestDataOut<>0 then
          Cli.Timeout.Enable(FTimeout_RequestDataOut);

        s:=obj.Get_Data2(notify);
        if length(s)>0 then
          begin
          if assigned(OnDebugLog) then
            Debug('<< '+obj.nr+'. CLI : Request.Write ('+Int2Str(length(s))+') '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
          Cli.WriteEx(s);
          Cli.Flush;
          end;
        if notify then
          sent:=obj.PostNotifyEvent(ServerCanRead)
        else
          sent:=True;
        end
      else
        begin
        if assigned(OnDebugLog) then
          Debug('§§§§ CLI : Request.Sent - OBJ is NULL for '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        sent:=False;
        end;
      end;

    if not sent then
      begin
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('§§§§§§§§§§ '+obj.nr+'. << CLI : Request.Sent - RECONNECTING - '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE)
        else
          Debug('§§§§§§§§§§ << CLI : Request.Sent - RECONNECTING - '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      Cli.Request.Skip;
      Cli.Reconnect;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Client_DataSent',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Client_DataReceived(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    sent:boolean;
    CB:TRtcRouterContentBody;
  begin
  try
    SetLength(s,0);
    if Cli.Response.Started then
      begin
      if FTimeout_ResponseBegin<>0 then
        Cli.Timeout.Enable(FTimeout_ResponseBegin);

      Event_ResponseBegin(Cli);

      if Cli.Response.ManualRead then
        Cli.Request.Info.asBoolean['$man']:=True;
      if Cli.Response.ChunkedTransferEncoding then
        Cli.Response.ManualRead:=False;
      end;

    if Cli.Response.Done then
      begin
      obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
      if assigned(obj) then
        begin
        if not Cli.Request.Info.asBoolean['$cli-out'] then
          begin
          if not Cli.Request.Info.asBoolean['$man'] then
            begin
            CB:=TRtcRouterContentBody.Create;
            try
              CB.BodyEx:=Cli.ReadEx;

              if FTimeout_ResponseReceived<>0 then
                Cli.Timeout.Enable(FTimeout_ResponseReceived)
              else if FTimeout_ResponseDataIn<>0 then
                Cli.Timeout.Enable(FTimeout_ResponseDataIn);

              Event_ResponseReceived(Cli,CB);

              Cli.Response.ContentLength:=length(CB.BodyEx);
              Cli.Response.ChunkedTransferEncoding:=False;
              Cli.Request.Info.asBoolean['$cli-out']:=True;

              sent:=obj.PostDataFirstEvent(ServerCanWriteFirst,CB.BodyEx,Cli.Response.HeaderText,Cli.Response.StatusText,Cli.Response.StatusCode,Cli.Request.Close,True,False);
              if assigned(OnDebugLog) then
                Debug('>> '+obj.nr+'. CLI : Response.Data ('+Int2Str(length(CB.BodyEx))+'/'+Cli.Response.ValueCS['CONTENT-LENGTH']+') done (buffered) '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
            finally
              RtcFreeAndNil(CB);
              end;
            end
          else
            begin
            if FTimeout_ResponseReceived<>0 then
              Cli.Timeout.Enable(FTimeout_ResponseReceived)
            else if FTimeout_ResponseDataIn<>0 then
              Cli.Timeout.Enable(FTimeout_ResponseDataIn);

            Event_ResponseReceived(Cli,nil);

            s:=Cli.ReadEx;
            Cli.Response.ContentLength:=length(s);
            Cli.Response.ChunkedTransferEncoding:=False;
            Cli.Request.Info.asBoolean['$cli-out']:=True;

            sent:=obj.PostDataFirstEvent(ServerCanWriteFirst,s,Cli.Response.HeaderText,Cli.Response.StatusText,Cli.Response.StatusCode,Cli.Request.Close,True,False);
            if assigned(OnDebugLog) then
              Debug('>> '+obj.nr+'. CLI : Response.Data ('+Int2Str(length(s))+'/'+Cli.Response.ValueCS['CONTENT-LENGTH']+') done '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
            end;
          end
        else
          begin
          if FTimeout_ResponseReceived<>0 then
            Cli.Timeout.Enable(FTimeout_ResponseReceived)
          else if FTimeout_ResponseDataIn<>0 then
            Cli.Timeout.Enable(FTimeout_ResponseDataIn);

          Event_ResponseReceived(Cli,nil);

          s:=Cli.ReadEx;
          if length(s)>0 then
            begin
            sent:=obj.PostDataEvent(ServerCanWrite,s,True,False);
            if assigned(OnDebugLog) then
              Debug('>> '+obj.nr+'. CLI : Response.Data ('+Int2Str(length(s))+', left='+Int2Str(Cli.Response.ContentLength-Cli.Response.ContentIn)+'/'+Cli.Response.ValueCS['CONTENT-LENGTH']+') done '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
            end
          else
            begin
            obj.PostDone(ServerWriteDone);
            if assigned(OnDebugLog) then
              Debug('>> '+obj.nr+'. CLI : Response.Data (NO DATA, left='+Int2Str(Cli.Response.ContentLength-Cli.Response.ContentIn)+'/'+Cli.Response.ValueCS['CONTENT-LENGTH']+') done '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
            sent:=True;
            end;
          end;
        end
      else
        begin
        if assigned(OnDebugLog) then
          Debug('§§§§ CLI : Response.Data - OBJ is NULL for '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        sent:=False;
        end;
      end
    else if not Cli.Response.ManualRead then
      begin
      if FTimeout_ResponseDataIn<>0 then
        Cli.Timeout.Enable(FTimeout_ResponseDataIn);

      obj:=nil;
      if assigned(OnDebugLog) then
        Debug('>> CLI : Response.Data - Buffering ... '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      sent:=True;
      end
    else
      begin
      obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
      if assigned(obj) then
        begin
        if not Cli.Request.Info.asBoolean['$cli-out'] then // started receiving response. Here comes the header ...
          begin
          s:=Cli.ReadEx;

          if assigned(OnDebugLog) then
            if length(s)>0 then
              Debug('>> '+obj.nr+'. CLI : Response.Data ('+Int2Str(length(s))+', left='+Int2Str(Cli.Response.ContentLength-Cli.Response.ContentIn)+'/'+Cli.Response.ValueCS['CONTENT-LENGTH']+') started '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE)
            else
              Debug('>> '+obj.nr+'. CLI : Response.Data (NO DATA, left='+Int2Str(Cli.Response.ContentLength-Cli.Response.ContentIn)+'/'+Cli.Response.ValueCS['CONTENT-LENGTH']+') started '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);

          if Cli.Response.Done then
            begin
            if FTimeout_ResponseReceived<>0 then
              Cli.Timeout.Enable(FTimeout_ResponseReceived)
            else if FTimeout_ResponseDataIn<>0 then
              Cli.Timeout.Enable(FTimeout_ResponseDataIn);

            Event_ResponseReceived(Cli,nil);

            Cli.Response.ContentLength:=length(s);
            Cli.Response.ChunkedTransferEncoding:=False;
            end
          else
            begin
            if FTimeout_ResponseDataIn<>0 then
              Cli.Timeout.Enable(FTimeout_ResponseDataIn);
            end;

          Cli.Request.Info.asBoolean['$cli-out']:=True;
          sent:=obj.PostDataFirstEvent(ServerCanWriteFirst,s,Cli.Response.HeaderText,Cli.Response.StatusText,Cli.Response.StatusCode,Cli.Request.Close,Cli.Response.Done, Cli.Response.ManualRead and not Cli.Response.Done);
          end
        else if Cli.Request.Info.asBoolean['$srv-wait'] then
          begin
          Cli.Request.Info.asBoolean['$srv-wait']:=False;
          s:=Cli.ReadEx;

          if Cli.Response.Done then
            begin
            if FTimeout_ResponseReceived<>0 then
              Cli.Timeout.Enable(FTimeout_ResponseReceived)
            else if FTimeout_ResponseDataIn<>0 then
              Cli.Timeout.Enable(FTimeout_ResponseDataIn);
            Event_ResponseReceived(Cli,nil);
            end
          else
            begin
            if FTimeout_ResponseDataIn<>0 then
              Cli.Timeout.Enable(FTimeout_ResponseDataIn);
            end;

          if length(s)>0 then
            begin
            if assigned(OnDebugLog) then
              Debug('>> '+obj.nr+'. CLI : Response.Data ('+Int2Str(length(s))+', left='+Int2Str(Cli.Response.ContentLength-Cli.Response.ContentIn)+'/'+Cli.Response.ValueCS['CONTENT-LENGTH']+') server waiting '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
            sent:=obj.PostDataEvent(ServerCanWrite,s,Cli.Response.Done, Cli.Response.ManualRead and not Cli.Response.Done);
            end
          else
            begin
            if assigned(OnDebugLog) then
              Debug('>> '+obj.nr+'. CLI : Response.Data (NO DATA, left='+Int2Str(Cli.Response.ContentLength-Cli.Response.ContentIn)+'/'+Cli.Response.ValueCS['CONTENT-LENGTH']+') server waiting '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
            if Cli.Response.Done then
              obj.PostDone(ServerWriteDone)
            else
              Cli.Request.Info.asBoolean['$srv-wait']:=True;
            sent:=True;
            end;
          end
        else
          begin
          if assigned(OnDebugLog) then
            Debug('>> '+obj.nr+'. CLI : Response.Data (left='+Int2Str(Cli.Response.ContentLength-Cli.Response.ContentIn)+'/'+Cli.Response.ValueCS['CONTENT-LENGTH']+') DATA READY '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);

          if FTimeout_ResponseDataIn<>0 then
            Cli.Timeout.Enable(FTimeout_ResponseDataIn);

          Cli.Request.Info.asBoolean['$cli-ready']:=True;
          sent:=True;
          end;
        end
      else
        begin
        if assigned(OnDebugLog) then
          Debug('§§§§ CLI : Response.Data - OBJ is NULL for '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        sent:=False;
        end;
      end;

    if not sent then
      begin
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('§§§§§§§§§§ >> '+obj.nr+'. CLI : Response.Data - RECONNECTING '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE)
        else
          Debug('§§§§§§§§§§ >> CLI : Response.Data - RECONNECTING '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      Cli.Request.Skip;
      Cli.Reconnect;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Client_DataReceived',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Client_RepostCheck(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
  begin
  try
    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(obj) and not Cli.Request.Active and (Cli.Request.Reposted<2) then
      begin
      Cli.Request.Repost;
      if assigned(OnDebugLog) then
        Debug('§§ '+obj.nr+'. CLI : REPOST '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      end
    else
      begin
      if not Cli.Request.Info.asBoolean['$stop'] then
        begin
        if Cli.Request.Complete then
          Event_ResponseReceiveAbort(Cli)
        else
          Event_RequestSendAbort(Cli);
        end;
      if assigned(obj) then
        begin
        if assigned(OnDebugLog) then
          Debug('§§§§§§§§§§ '+obj.nr+'. CLI : Disconnect '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        if Cli.Request.Info.asBoolean['$stop'] then
          Proxy_Clear(obj,nil,False)
        else
          Proxy_Clear(obj,ServerStop,False);
        end
      else
        if assigned(OnDebugLog) then
          Debug('§§§§§§§§§§ CLI : Disconnect '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Client_RepostCheck',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Client_ResponseDone(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
  begin
  try
    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      if assigned(OnDebugLog) then
        Debug('!! '+obj.nr+'. CLI : Response.Done '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      obj.PostDone(nil);
      end
    else
      if assigned(OnDebugLog) then
        Debug('!! CLI : Response.Done '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Client_ResponseDone',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Client_WSConnect(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
  begin
  try
    Cli.Timeout.Disable;
    Cli.Response.ManualRead:=True;

    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(OnDebugLog) then
      if assigned(obj) then
        Debug('!! '+obj.nr+'. CLI : WebSock.Connect '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE)
      else
        Debug('!! CLI : WebSock.Connect '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);

    if assigned(obj) then
      obj.PostWSOpen(ServerWSCanRead);
      
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Client_WSConnect',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Client_WSDataSent(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    sent,notify:boolean;
  begin
  try
    SetLength(s,0);

    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      s:=obj.Get_Data2(notify);
      if length(s)>0 then
        begin
        if assigned(OnDebugLog) then
          Debug('<< '+obj.nr+'. CLI : WebSock.Write ('+Int2Str(length(s))+') '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
        Cli.WriteEx(s);
        end;
      if notify then
        sent:=obj.PostNotifyEvent(ServerWSCanRead)
      else
        sent:=True;
      end
    else
      begin
      if assigned(OnDebugLog) then
        Debug('§§§§ CLI : WebSock.Sent - OBJ is NULL for '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      sent:=False;
      end;

    if not sent then
      begin
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('§§§§§§§§§§ '+obj.nr+'. << CLI : WebSock.Sent - RECONNECTING - '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE)
        else
          Debug('§§§§§§§§§§ << CLI : WebSock.Sent - RECONNECTING - '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      Cli.SkipRequests;
      Cli.Reconnect;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Client_WSDataSent',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Client_WSDataReceived(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
    s:RtcByteArray;
    sent:boolean;
  begin
  try
    SetLength(s,0);

    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      if Cli.Request.Info.asBoolean['$srv-wait'] then
        begin
        Cli.Request.Info.asBoolean['$srv-wait']:=False;
        s:=Cli.ReadEx;
        if length(s)>0 then
          begin
          if assigned(OnDebugLog) then
            Debug('>> '+obj.nr+'. CLI : WebSock.Data ('+Int2Str(length(s))+') server waiting '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
          sent:=obj.PostDataEvent(ServerWSCanWrite,s,False,Cli.Response.ManualRead);
          end
        else
          begin
          if assigned(OnDebugLog) then
            Debug('>> '+obj.nr+'. CLI : WebSock.Data (NO DATA) server waiting '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
          Cli.Request.Info.asBoolean['$srv-wait']:=True;
          sent:=True;
          end;
        end
      else
        begin
        if assigned(OnDebugLog) then
          Debug('>> '+obj.nr+'. CLI : WebSock.Data DATA READY '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);

        Cli.Request.Info.asBoolean['$cli-ready']:=True;
        sent:=True;
        end;
      end
    else
      begin
      if assigned(OnDebugLog) then
        Debug('§§§§ CLI : WebSock.Data - OBJ is NULL for '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      sent:=False;
      end;

    if not sent then
      begin
      if assigned(OnDebugLog) then
        if assigned(obj) then
          Debug('§§§§§§§§§§ >> '+obj.nr+'. CLI : WebSock.Data - RECONNECTING '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE)
        else
          Debug('§§§§§§§§§§ >> CLI : WebSock.Data - RECONNECTING '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      Cli.Request.Skip;
      Cli.Reconnect;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Client_WSDataReceived',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Client_WSDisconnect(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    obj:TRtcProxyData;
  begin
  try
    obj:=TRtcProxyData(Cli.Request.Info.asObj['$con']);
    if assigned(obj) then
      begin
      if assigned(OnDebugLog) then
        Debug('§§§§§§§§§§ '+obj.nr+'. CLI : WebSock.Disconnect '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
      if Cli.Request.Info.asBoolean['$stop'] then
        Proxy_Clear(obj,nil,False)
      else
        Proxy_Clear(obj,ServerStop,False);
      end
    else if assigned(OnDebugLog) then
      Debug('§§§§§§§§§§ CLI : WebSock.Disconnect '+Cli.Request.Method+' '+Cli.Request.URI,RLOG_ROUTE);
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Client_WSDisconnect',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcDataRouter.GetDebugInfo: RtcString;
  var
    a:integer;
  begin
  try
    ProxyDataCS.Acquire;
    try
      Result:='Done='+Int2Str(CurrentNr)+
              '; Objects='+Int2Str(TotalCreated)+
              '; Pending='+Int2Str(TotalPending)+
              '; Queued='+Int2Str(Total_Queued);
      if QueueCnt>0 then
        begin
        Result:=Result+' ('+Int2Str(My_Queued[0])+'/'+Int2Str(My_Queues[0].Count);
        for a:=1 to QueueCnt-1 do
          Result:=Result+' + '+Int2Str(My_Queued[a])+'/'+Int2Str(My_Queues[a].Count);
        Result:=Result+')';
        end;
    finally
      ProxyDataCS.Release;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.GetDebugInfo',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Debug(const _text: RtcString; _name: String);
  var
    tmp:TRtcRouterDebugInfo;
  begin
  try
    if not assigned(FOnDebugLog) then
      Log('TRtcDataRouter.Debug: '+_text,'ERROR')
    else
      begin
      tmp:=TRtcRouterDebugInfo.Create;
      try
        tmp.Text:=_text;
        tmp.Name:=_name;
        FOnDebugLog(tmp);
      finally
        RtcFreeAndNil(tmp);
        end;
      end;
  except
    on E:Exception do
      begin
      Log('TRtcDataRouter.Debug: '+_text,E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDataRouter.Event_CheckRequest(Sender: TRtcDataServer);
  begin
  if assigned(FOnCheckRequest) then
    FOnCheckRequest(Sender);
  end;

procedure TRtcDataRouter.Event_PostNewRequest(Sender: TRtcDataServer;
    var DataRequest: TRtcDataRequest; var AddToQueue: integer; var MoveToBottom: boolean);
  begin
  if assigned(FOnPostNewRequest) then
    FOnPostNewRequest(Sender,DataRequest,AddToQueue,MoveToBottom);
  end;

procedure TRtcDataRouter.Event_PostOldRequest(Sender: TRtcDataServer;
    var DataRequest: TRtcDataRequest; var AddToQueue: integer; var MoveToBottom: boolean);
  begin
  if assigned(FOnPostOldRequest) then
    FOnPostOldRequest(Sender,DataRequest,AddToQueue,MoveToBottom);
  end;

procedure TRtcDataRouter.Event_PostReturn(DataRequest: TRtcDataRequest);
  begin
  if assigned(FOnPostReturn) then
    FOnPostReturn(DataRequest);
  end;

procedure TRtcDataRouter.Event_QueuedRequest(Sender: TRtcDataServer;
    AddedToQueue: integer; MovedToBottom: boolean);
  begin
  if assigned(FOnQueuedRequest) then
    FOnQueuedRequest(Sender,AddedToQueue,MovedToBottom);
  end;

procedure TRtcDataRouter.Event_RequestBegin(Sender: TRtcDataClient);
  begin
  if assigned(FOnRequestBegin) then
    FOnRequestBegin(Sender);
  end;

procedure TRtcDataRouter.Event_RequestReceiveAbort(Sender: TRtcDataServer);
  begin
  if assigned(FOnRequestReceiveAbort) then
    FOnRequestReceiveAbort(Sender);
  end;

procedure TRtcDataRouter.Event_RequestReceived(Sender: TRtcDataServer; Content: TRtcRouterContentBody);
  begin
  if assigned(FOnRequestReceived) then
    FOnRequestReceived(Sender,Content);
  end;

procedure TRtcDataRouter.Event_RequestSendAbort(Sender: TRtcDataClient);
  begin
  if assigned(FOnRequestSendAbort) then
    FOnRequestSendAbort(Sender);
  end;

procedure TRtcDataRouter.Event_RequestSent(Sender: TRtcDataClient);
  begin
  if assigned(FOnRequestSent) then
    FOnRequestSent(Sender);
  end;

procedure TRtcDataRouter.Event_ResponseBegin(Sender: TRtcDataClient);
  begin
  if assigned(FOnResponseBegin) then
    FOnResponseBegin(Sender);
  end;

procedure TRtcDataRouter.Event_ResponseReceiveAbort(Sender: TRtcDataClient);
  begin
  if assigned(FOnResponseReceiveAbort) then
    FOnResponseReceiveAbort(Sender);
  end;

procedure TRtcDataRouter.Event_ResponseReceived(Sender: TRtcDataClient; Content: TRtcRouterContentBody);
  begin
  if assigned(FOnResponseReceived) then
    FOnResponseReceived(Sender, Content);
  end;

procedure TRtcDataRouter.Event_ResponseSendAbort(Sender: TRtcDataServer);
  begin
  if assigned(FOnResponseSendAbort) then
    FOnResponseSendAbort(Sender);
  end;

procedure TRtcDataRouter.Event_ResponseSent(Sender: TRtcDataServer);
  begin
  if assigned(FOnResponseSent) then
    FOnResponseSent(Sender);
  end;

procedure TRtcDataRouter.Client_WriteHeader(Sender: TRtcDataClient);
  begin
  Sender.WriteHeader;
  end;

{$IFDEF RTC_ANON_METHODS}

// procedure(Sender:TRtcDataClient)
type
  TRtcRouterClientAMContainer = class(TObject)
  public
    MyMethod:TRtcRouterClientAnonMethod;
    constructor Create(const AMethod:TRtcRouterClientAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcDataClient);
    end;

constructor TRtcRouterClientAMContainer.Create(const AMethod:TRtcRouterClientAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcRouterClientAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcRouterClientAMContainer.MyEvent(Sender:TRtcDataClient);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender);
  end;

function TRtcDataRouter.Anon(const Event:TRtcRouterClientAnonMethod):TRtcRouterClientEvent;
  var
    obj:TRtcRouterClientAMContainer;
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
        obj:=TRtcRouterClientAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(Sender:TRtcDataServer)
type
  TRtcRouterServerAMContainer = class(TObject)
  public
    MyMethod:TRtcRouterServerAnonMethod;
    constructor Create(const AMethod:TRtcRouterServerAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcDataServer);
    end;

constructor TRtcRouterServerAMContainer.Create(const AMethod:TRtcRouterServerAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcRouterServerAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcRouterServerAMContainer.MyEvent(Sender:TRtcDataServer);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender);
  end;

function TRtcDataRouter.Anon(const Event:TRtcRouterServerAnonMethod):TRtcRouterServerEvent;
  var
    obj:TRtcRouterServerAMContainer;
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
        obj:=TRtcRouterServerAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(Sender:TRtcDataServer; Content:TRtcRouterContentBody)
type
  TRtcRouterRequestReceivedAMContainer = class(TObject)
  public
    MyMethod:TRtcRouterRequestReceivedAnonMethod;
    constructor Create(const AMethod:TRtcRouterRequestReceivedAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcDataServer; Content:TRtcRouterContentBody);
    end;

constructor TRtcRouterRequestReceivedAMContainer.Create(const AMethod:TRtcRouterRequestReceivedAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcRouterRequestReceivedAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcRouterRequestReceivedAMContainer.MyEvent(Sender:TRtcDataServer; Content:TRtcRouterContentBody);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender, Content);
  end;

function TRtcDataRouter.Anon(const Event:TRtcRouterRequestReceivedAnonMethod):TRtcRouterRequestReceivedEvent;
  var
    obj:TRtcRouterRequestReceivedAMContainer;
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
        obj:=TRtcRouterRequestReceivedAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(Sender:TRtcDataClient; Content:TRtcRouterContentBody)
type
  TRtcRouterResponseReceivedAMContainer = class(TObject)
  public
    MyMethod:TRtcRouterResponseReceivedAnonMethod;
    constructor Create(const AMethod:TRtcRouterResponseReceivedAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcDataClient; Content:TRtcRouterContentBody);
    end;

constructor TRtcRouterResponseReceivedAMContainer.Create(const AMethod:TRtcRouterResponseReceivedAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcRouterResponseReceivedAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcRouterResponseReceivedAMContainer.MyEvent(Sender:TRtcDataClient; Content:TRtcRouterContentBody);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender, Content);
  end;

function TRtcDataRouter.Anon(const Event:TRtcRouterResponseReceivedAnonMethod):TRtcRouterResponseReceivedEvent;
  var
    obj:TRtcRouterResponseReceivedAMContainer;
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
        obj:=TRtcRouterResponseReceivedAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; var AddToQueue:integer; var MoveToBottom:boolean)
type
  TRtcRouterGetConnectionAMContainer = class(TObject)
  public
    MyMethod:TRtcRouterGetConnectionAnonMethod;
    constructor Create(const AMethod:TRtcRouterGetConnectionAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; var AddToQueue:integer; var MoveToBottom:boolean);
    end;

constructor TRtcRouterGetConnectionAMContainer.Create(const AMethod:TRtcRouterGetConnectionAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcRouterGetConnectionAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcRouterGetConnectionAMContainer.MyEvent(Sender:TRtcDataServer; var DataRequest:TRtcDataRequest; var AddToQueue:integer; var MoveToBottom:boolean);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender, DataRequest, AddToQueue, MoveToBottom);
  end;

function TRtcDataRouter.Anon(const Event:TRtcRouterGetConnectionAnonMethod):TRtcRouterGetConnectionEvent;
  var
    obj:TRtcRouterGetConnectionAMContainer;
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
        obj:=TRtcRouterGetConnectionAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(Sender:TRtcDataServer; AddedToQueue:integer; MovedToBottom:boolean)
type
  TRtcRouterQueConnectionAMContainer = class(TObject)
  public
    MyMethod:TRtcRouterQueConnectionAnonMethod;
    constructor Create(const AMethod:TRtcRouterQueConnectionAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcDataServer; AddedToQueue:integer; MovedToBottom:boolean);
    end;

constructor TRtcRouterQueConnectionAMContainer.Create(const AMethod:TRtcRouterQueConnectionAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcRouterQueConnectionAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcRouterQueConnectionAMContainer.MyEvent(Sender:TRtcDataServer; AddedToQueue:integer; MovedToBottom:boolean);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender, AddedToQueue, MovedToBottom);
  end;

function TRtcDataRouter.Anon(const Event:TRtcRouterQueConnectionAnonMethod):TRtcRouterQueConnectionEvent;
  var
    obj:TRtcRouterQueConnectionAMContainer;
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
        obj:=TRtcRouterQueConnectionAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(DataRequest:TRtcDataRequest)
type
  TRtcRouterPutConnectionAMContainer = class(TObject)
  public
    MyMethod:TRtcRouterPutConnectionAnonMethod;
    constructor Create(const AMethod:TRtcRouterPutConnectionAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(DataRequest:TRtcDataRequest);
    end;

constructor TRtcRouterPutConnectionAMContainer.Create(const AMethod:TRtcRouterPutConnectionAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcRouterPutConnectionAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcRouterPutConnectionAMContainer.MyEvent(DataRequest:TRtcDataRequest);
  begin
  if assigned(MyMethod) then
    MyMethod(DataRequest);
  end;

function TRtcDataRouter.Anon(const Event:TRtcRouterPutConnectionAnonMethod):TRtcRouterPutConnectionEvent;
  var
    obj:TRtcRouterPutConnectionAMContainer;
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
        obj:=TRtcRouterPutConnectionAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(Info:TRtcRouterDebugInfo)
type
  TRtcRouterDebugLogAMContainer = class(TObject)
  public
    MyMethod:TRtcRouterDebugLogAnonMethod;
    constructor Create(const AMethod:TRtcRouterDebugLogAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Info:TRtcRouterDebugInfo);
    end;

constructor TRtcRouterDebugLogAMContainer.Create(const AMethod:TRtcRouterDebugLogAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcRouterDebugLogAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcRouterDebugLogAMContainer.MyEvent(Info:TRtcRouterDebugInfo);
  begin
  if assigned(MyMethod) then
    MyMethod(Info);
  end;

function TRtcDataRouter.Anon(const Event:TRtcRouterDebugLogAnonMethod):TRtcRouterDebugLogEvent;
  var
    obj:TRtcRouterDebugLogAMContainer;
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
        obj:=TRtcRouterDebugLogAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
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

{ TRtcClientPool }

constructor TRtcClientPool.Create(StackSize:integer);
  begin
  inherited Create;
  FCount:=0;
  FID:=-1;
  FIdle:=tXObjList.Create(StackSize); // Idle TRtcDataRequest components
  FSessExp:=tStrIntList.Create(StackSize); // Sessions sorted by Expiring date+time & Session ID
  FSessList:=tStrList.Create(StackSize); // Sessions sorted by Session ID
  end;

destructor TRtcClientPool.Destroy;
  var
    a:integer;
    oreq:TObject;
    req:TRtcDataRequest absolute oreq;
    cli:TRtcDataClient;
  begin
  for a:=1 to FIdle.Count do
    begin
    FIdle.extractFirst(oreq);

    cli:=req.Client;
    req.Client:=nil;
    RtcFreeAndNil(cli);
    RtcFreeAndNil(req);
    end;
  RtcFreeAndNil(FIdle);
  RtcFreeAndNil(FSessExp);
  RtcFreeAndNil(FSessList);
  inherited;
  end;

procedure TRtcClientPool.Close;
  var
    a:integer;
    oreq:TObject;
    req:TRtcDataRequest absolute oreq;
    cli:TRtcDataClient;
  begin
  for a:=1 to FIdle.Count do
    begin
    FIdle.extractFirst(oreq);
    FIdle.addLast(req);
    cli:=req.Client;
    cli.AutoConnect:=False;
    cli.Disconnect;
    end;
  end;

function TRtcClientPool.GetIdleCount: integer;
  begin
  Result:=FIdle.Count;
  end;

function TRtcClientPool.GetDataRequest: TRtcDataRequest;
  var
    Cli:TRtcDataClient;
    ores:TObject absolute Result;
  begin
  if FIdle.Count>0 then
    begin
    FIdle.extractFirst(ores);
    Inc(FCount);
    end
  else
    begin
    Cli:=NewClient;
    if assigned(Cli) then
      begin
      Result:=TRtcDataRequest.Create(nil);
      Result.Tag:=FID+1;
      Result.Client:=Cli;
      Cli.Tag:=FID+1;
      Inc(FCount);
      end
    else
      Result:=nil;
    end;
  end;

procedure TRtcClientPool.PutDataRequest(DataRequest: TRtcDataRequest; ToBottom:boolean=False);
  begin
  if ToBottom then
    FIdle.addLast(DataRequest)
  else
    FIdle.addFirst(DataRequest);
  Dec(FCount);
  end;

function TRtcClientPool.NewClient: TRtcDataClient;
  begin
  // Descendant class needs to implement this
  // to return a fully set up Client component
  Result:=nil;
  end;

function TRtcClientPool.ActiveSessions: integer;
  begin
  Result:=FSessList.Count;
  end;

procedure TRtcClientPool.OpenOrUpdateSession(const SID: RtcString; ExpireTime: TDateTime);
  var
    exp:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp='' then
    begin
    exp:=SessionTimeToStr(ExpireTime);
    FSessList.insert(SID,exp);
    FSessExp.insert(exp+'-'+SID,1);
    end
  else
    begin
    FSessExp.remove(exp+'-'+SID);
    exp:=SessionTimeToStr(ExpireTime);
    FSessList.change(SID,exp);
    FSessExp.insert(exp+'-'+SID,1);
    end;
  end;

function TRtcClientPool.OpenSession(const SID: RtcString; ExpireTime: TDateTime):boolean;
  var
    exp:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp='' then
    begin
    exp:=SessionTimeToStr(ExpireTime);
    FSessList.insert(SID,exp);
    FSessExp.insert(exp+'-'+SID,1);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcClientPool.UpdateSession(const SID: RtcString; ExpireTime: TDateTime):boolean;
  var
    exp:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp<>'' then
    begin
    FSessExp.remove(exp+'-'+SID);
    exp:=SessionTimeToStr(ExpireTime);
    FSessList.change(SID,exp);
    FSessExp.insert(exp+'-'+SID,1);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcClientPool.CloseSession(const SID: RtcString): boolean;
  var
    exp:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp<>'' then
    begin
    FSessExp.remove(exp+'-'+SID);
    FSessList.remove(SID);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcClientPool.HaveSession(const SID: RtcString): boolean;
  begin
  Result:=FSessList.search(SID)<>'';
  end;

function TRtcClientPool.RemoveExpiredSessions(ExpireTime: TDateTime):integer;
  var
    exp,exp_sid,sid:RtcString;
    loc,i:Longint;
  begin
  Result:=0;
  exp:=SessionTimeToStr(ExpireTime);

  exp_sid:=FSessExp.search_min(i);
  while (exp_sid<>'') and (exp_sid<exp) do
    begin
    Inc(Result);
    loc:=PosEx('-',exp_sid);
    sid:=Copy(exp_sid, loc+1, length(exp_sid)-loc);

    FSessList.remove(sid);
    FSessExp.remove(exp_sid);

    exp_sid:=FSessExp.search_min(i);
    end;
  end;

function TRtcClientPool.SessionExpiresBefore(const SID: RtcString; ExpireTime: TDateTime): boolean;
  var
    exp,exp2:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp<>'' then
    begin
    exp2:=SessionTimeToStr(ExpireTime);
    Result:=exp<exp2;
    end
  else
    Result:=False;
  end;

function TRtcClientPool.UpdateSessionIfExpiresBefore(const SID: RtcString; BeforeTime, ExpireTime: TDateTime): boolean;
  var
    exp,exp2:RtcString;
  begin
  exp:=FSessList.search(SID);
  if exp<>'' then
    begin
    exp2:=SessionTimeToStr(BeforeTime);
    if exp<exp2 then
      begin
      FSessExp.remove(exp+'-'+SID);
      exp:=SessionTimeToStr(ExpireTime);
      FSessList.change(SID,exp);
      FSessExp.insert(exp+'-'+SID,1);
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

{ TRtcMultiClientPool }

constructor TRtcMultiClientPool.Create;
  begin
  inherited;
  FPoolCnt:=0;
  SetLength(FPools,0);
  end;

destructor TRtcMultiClientPool.Destroy;
  var
    a:integer;
  begin
  for a:=0 to FPoolCnt-1 do
    RtcFreeAndNil(FPools[a]);
  SetLength(FPools,0);
  FPoolCnt:=0;
  inherited;
  end;

function TRtcMultiClientPool.AddPool(NewPool: TRtcClientPool):integer;
  begin
  SetLength(FPools,FPoolCnt+1);
  FPools[FPoolCnt]:=NewPool;
  NewPool.FID:=FPoolCnt;
  Result:=FPoolCnt;
  Inc(FPoolCnt);
  end;

procedure TRtcMultiClientPool.CloseAll;
  var
    a:integer;
  begin
  for a:=0 to FPoolCnt-1 do
    FPools[a].Close;
  end;

function TRtcMultiClientPool.GetActiveClients: integer;
  var
    a:integer;
  begin
  Result:=0;
  for a:=0 to FPoolCnt-1 do
    Result:=Result+FPools[a].ActiveClients;
  end;

function TRtcMultiClientPool.GetIdleClients: integer;
  var
    a:integer;
  begin
  Result:=0;
  for a:=0 to FPoolCnt-1 do
    Result:=Result+FPools[a].IdleClients;
  end;

function TRtcMultiClientPool.GetPool(index: integer): TRtcClientPool;
  begin
  if (index>=0) and (index<FPoolCnt) then
    Result:=FPools[index]
  else
    Result:=nil;
  end;

function TRtcMultiClientPool.ActiveSessions: integer;
  var
    a:integer;
  begin
  Result:=0;
  for a:=0 to FPoolCnt-1 do
    Result:=Result+FPools[a].ActiveSessions;
  end;

function TRtcMultiClientPool.RemoveExpiredSessions(ExpireTime: TDateTime):integer;
  var
    a:integer;
  begin
  Result:=0;
  for a:=0 to FPoolCnt-1 do
    Result:=Result+FPools[a].RemoveExpiredSessions(ExpireTime);
  end;

function TRtcMultiClientPool.CloseSession(const SID: RtcString): integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=0 to FPoolCnt-1 do
    if FPools[a].CloseSession(SID) then
      begin
      Result:=a;
      Break;
      end;
  end;

function TRtcMultiClientPool.HaveSession(const SID: RtcString): integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=0 to FPoolCnt-1 do
    if FPools[a].HaveSession(SID) then
      begin
      Result:=a;
      Break;
      end;
  end;

function TRtcMultiClientPool.UpdateSession(const SID: RtcString; ExpireTime: TDateTime): integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=0 to FPoolCnt-1 do
    if FPools[a].UpdateSession(SID,ExpireTime) then
      begin
      Result:=a;
      Break;
      end;
  end;

function TRtcMultiClientPool.SessionExpiresBefore(const SID: RtcString; ExpireTime: TDateTime): integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=0 to FPoolCnt-1 do
    if FPools[a].SessionExpiresBefore(SID,ExpireTime) then
      begin
      Result:=a;
      Break;
      end;
  end;

function TRtcMultiClientPool.UpdateSessionIfExpiresBefore(const SID: RtcString; BeforeTime, ExpireTime: TDateTime): integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=0 to FPoolCnt-1 do
    if FPools[a].UpdateSessionIfExpiresBefore(SID,BeforeTime,ExpireTime) then
      begin
      Result:=a;
      Break;
      end;
  end;

function TRtcMultiClientPool.PutDataRequest(DataRequest: TRtcDataRequest; ToBottom:boolean=False):integer;
  var
    id:integer;
  begin
  id:=DataRequest.Tag-1;
  if (id<0) or (id>=FPoolCnt) then
    Result:=-1
  else
    begin
    FPools[id].PutDataRequest(DataRequest,ToBottom);
    Result:=id;
    end
  end;

function TRtcMultiClientPool.GetPoolIndex(Con: TRtcDataClient): integer;
  begin
  if (Con.Tag>0) and (Con.Tag<=FPoolCnt) then
    Result:=Con.Tag-1
  else
    Result:=-1;
  end;

end.
