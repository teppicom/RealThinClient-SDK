{
  @html(<b>)
  Client Module for Remote Functions
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit introduces @Link(TRtcClientModule), the client-side component for ENABLING remote functions.
  By using @Link(TRtcClientModule), you can easily call remote server functions and get results
  in form of objects, passed to the event handlers you define. Also, by assigning a
  @Link(TRtcFunctionGroup) component to your @Link(TRtcClientModule) component,
  server can (as a result of any remote function call from the client) return functions which
  will be executed on the client side before the result object is passed to the local event handler.
  Implementing a RTC Remote Function is as easy as writing a local function.
}
unit rtcCliModule;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSrcList,
  rtcSystem,
  rtcThrPool,
  rtcTimer,
  rtcLog,

{$IFDEF COMPRESS}
  rtcZLib,
{$ENDIF}

  rtcCrypt,
  rtcInfo,
  rtcLink,
  rtcConn,

  rtcDataCli,
  rtcFunction;

var
  LOG_CLIENTMODULE_ERRORS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

type
  ERtcExecuteError = class(Exception)
    protected
      FErrCode:TRtcWaitForCompletionResult;

    public
      constructor Create(const Msg:String; ECode:TRtcWaitForCompletionResult);

      property ErrorCode:TRtcWaitForCompletionResult read FErrCode;
    end;

  // @exclude
  EPostInteractive = class(EAbort);

  TRtcClientModule = class; // forward

  { @abstract(Client Remote RTC Object Manager implementation) }
  TRtcClientObjectManager = class(TRtcBasicRemoteObjectManager);

  { Supported Encryption Modes: Basic and ISAAC }
  TRtcEncryptionMode=(// Basic Encryption
                      rem_Basic,
                      // ISAAC Encryption
                      rem_Isaac);

  // @exclude
  TRtcInteractiveResult = class(TObject)
  public
    FEvent:TRtcResult;
    Data,Result:TRtcValue;

    destructor Destroy; override;
    end;

  { @abstract(Used to store all calls for a single Post from a ClientModule)

    @exclude }
  TRtcClientModuleCallsArray=class(TRtcArray)
  private
    FEvents:array of TRtcResult;

    function GetEvent(index: integer): TRtcResult;
    procedure SetEvent(index: integer; const _Value: TRtcResult);

  public
    constructor Create; overload;
    destructor Destroy; override;

    function EventCount:integer;

    property Event[index:integer]:TRtcResult read GetEvent write SetEvent;
    end;

  // @exclude
  TRtcCryptClient=class(TObject)
  public
    HaveHello,HaveStart,AllReady:boolean;
    ControlCounter:integer;
    ClientHello,ServerHello,
    ClientKey,ServerKey,
    ControlKey:RtcString;

    CRead,CWrite:TRtcCrypt;

  {$IFDEF RTC_RSA}
    Mode:TRtcEncryptionMode;
    RSAKey:integer;
    CReadEx,CWriteEx:TRtcISAACrypt;
    CReadRSA:TRtcRSA;
  {$ENDIF}

    constructor Create(m:TRtcEncryptionMode; rk:integer);
    destructor Destroy; override;

    procedure Reset;
    procedure SecureCode(const key:RtcString; var scode:RtcByteArray);

    function NewKey1(KeySize:integer):RtcByteArray;
    procedure SetupRead1(var code:RtcByteArray);
    procedure SetupWrite1;

    function NewKey2(KeySize:integer):RtcByteArray;
    procedure SetupRead2(var code:RtcByteArray);
    procedure SetupWrite2;

    function CanRead:boolean;
    function CanWrite:boolean;

    procedure Init(m:TRtcEncryptionMode; rk:integer);
    end;

  // @exclude
  TRtcClientModuleData=class
  public
    FRequest:TRtcClientRequest;
    FData:TRtcValue;
    FPostLevel:integer;
    FCalls:TRtcClientModuleCallsArray;

    constructor Create; virtual;
    destructor Destroy; override;
    end;

  { @abstract(Use to call remote functions and receive their results)

    ClientModule is used to prepare remote function calls, post them to
    the server, accept server's response and call local event handlers with
    the result received for each call. You can post a single call or multiple
    function calls with one request, or even use a timer-based trigger to post
    all the requests prepared until now. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcClientModule=class(TRtcAbsDataClientLink)
  private
    FMyData:TObjList;
    FMainThrData:TRtcClientModuleData;

    FExecuteResult:TRtcValue;
    FExecuteError:String;
    FExecuteHandler:TRtcResult;

    // Used for "PostInteractiveResult"
    FIntCS:TRtcCritSec;

    // Used for "FMyData" when HyperThreading=TRUE
    FCS:TRtcCritSec;

    FIntTimer:TRtcTimer;
    FIntRes:TXObjList;

    FPingTimer:TRtcTimer;

    FFunctions:TRtcFunctionGroup;
    FRelease:boolean;

    FModuleFileName:RtcString;
    FModuleFileREST:RtcString;
    
    FModuleHost:RtcString;
    FCryptSesName:RtcString;
    FObjManSesName:RtcString;
    FAutoRepost:integer;
    FAutoSessions: boolean;
    FAutoSessionMode: TRtcSessionsMode;

    FOnPeekResponse: TRtcNotifyEvent;
    FOnBeginRequest: TRtcNotifyEvent;
    FOnResponseAbort: TRtcNotifyEvent;
    FOnResponseDone: TRtcNotifyEvent;
    FOnResponseReject: TRtcNotifyEvent;
    FOnConnectLost: TRtcNotifyEvent;
    FOnSessionExpired: TRtcNotifyEvent;
    FOnRepostCheck: TRtcNotifyEvent;

    FOnSessionOpen: TRtcNotifyEvent;
    FOnSessionClose: TRtcNotifyEvent;

    FAutoEncrypt: integer;
    FForceEncrypt: boolean;
    FOnResponseError: TRtcNotifyEvent;
    FSecureKey: RtcString;
    FEncryptMode: TRtcEncryptionMode;
    FRSAKey: integer;

    FDataFormat: TRtcDataFormat;
    FDataReqMode: TRtcDataReqMode;
    FAutoSessionField,
    FAutoSessionFieldUPC,
    FDataContentType,
    FDataCustomQuery: RtcString;

    FCompress: TRtcCompressLevel;

    FOnNoEncryption: TRtcNotifyEvent;
    FOnNeedEncryption: TRtcNotifyEvent;
    FOnWrongEncryption: TRtcNotifyEvent;

    FOnLoginResult: TRtcResultEvent;
    FOnLoginAborted: TRtcResultEvent;
    FOnResultReady: TRtcResultEvent;
    FOnResultError: TRtcResultErrorEvent;

    FOnObjectLinkAborted: TRtcResultEvent;
    FOnObjectLinkErrors: TRtcResultEvent;
    FOnObjectDataOut: TRtcDataEvent;
    FOnObjectDataIn: TRtcDataEvent;
    FOnObjectCreate: TRtcObjectCreateEvent;

    FHyperThreading: boolean;
    FOnLogin: TRtcFunctionPrepareEvent;

    FResetLogin,
    FAutoLogin: boolean;
    FLoginResult: TRtcResult;
    FPingResult: TRtcResult;

    FObjectManager: TRtcClientObjectManager;

    FObjectLinkSupport: TRtcObjectLinkSupport;
    FObjectLinkResult: TRtcResult;
    FObjectLinksOut: boolean;
    FAutoSessionsPing: integer;
    FOnPing: TRtcFunctionPrepareEvent;
    FOnPingAborted: TRtcResultEvent;
    FOnPingResult: TRtcResultEvent;

    FOnPrepare:TRtcResultEvent;

    function CheckMyData:TRtcClientModuleData;
    function GetMyData:TRtcClientModuleData;
    procedure ClearMyData;

    function SessIDField:RtcString;
    function SessIDFieldCS:RtcString;

    procedure ClearAllThreadsData;

    function IsRemoteCallRequest(Sender:TRtcConnection):boolean;

    procedure NotifyResultAborted(Sender:TRtcConnection);

    procedure Response_Problem(Sender:TRtcConnection);

    procedure Call_SessionExpired(Sender:TRtcConnection);
    procedure Call_NoEncryption(Sender:TRtcConnection);
    procedure Call_NeedEncryption(Sender:TRtcConnection);
    procedure Call_WrongResponse(Sender:TRtcConnection);
    procedure Call_WrongEncryption(Sender:TRtcConnection);
    procedure Call_WrongEncryptionStart(Sender:TRtcConnection);
    procedure Call_ResultError(Sender:TRtcConnection; Data,Result:TRtcValue; E:Exception);

    function GetCrypt(Sender:TRtcConnection):TRtcCryptClient;
    procedure NewCrypt(Sender:TRtcConnection);
    procedure DelCrypt(Sender:TRtcConnection);
    procedure ResetCrypt(Sender:TRtcConnection);

    function GetFunctionGroup: TRtcFunctionGroup;
    procedure SetFunctionGroup(const Value: TRtcFunctionGroup);

    function GetModuleFileName: RtcString;
    procedure SetModuleFileName(const Value: RtcString);

    function GetModuleHost: RtcString;
    procedure SetModuleHost(const Value: RtcString);
    procedure SetAutoEncrypt(const Value: integer);
    procedure SetEncryptMode(const Value: TRtcEncryptionMode);
    procedure SetAutoSessions(const Value: boolean);
    procedure SetForceEncrypt(const Value: boolean);

    procedure PostInteractiveResult(Event:TRtcResult; Data,Result:TRtcValue);

    procedure DoInteractiveResult;

    procedure DoSessionPing;

    function Get_Data: TRtcValue;
    function GetPostLevel: integer;
    function GetRequest: TRtcClientRequest;

    procedure SetCompress(const Value: TRtcCompressLevel);

    procedure SetAutoSessionField(const Value:RtcString);

    procedure SetDataFormat(const Value: TRtcDataFormat);
    procedure SetDataReqMode(const Value: TRtcDataReqMode);

    procedure SetAutoLogin(const Value: boolean);

    procedure SetupCallRequest(FRequest:TRtcRequest);

    procedure LoginCall(ResultHandler: TRtcResult; Sender:TRtcConnection; Insert:boolean=False); virtual;
    procedure PingCall(ResultHandler: TRtcResult; Sender:TRtcConnection); virtual;

    procedure Call_LoginResult(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_LoginAborted(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_PingResult(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_PingAborted(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);

    procedure Call_ObjectLinkResult(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_ObjectLinkAborted(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_ObjectLinkErrors(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);

    procedure Call_ExecuteResult(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_ExecuteAbort(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);

    function GetLastCallParams: TRtcFunctionInfo;

    procedure SetObjectLinkSupport(const Value: TRtcObjectLinkSupport);
    procedure SetAutoSessionsPing(const Value: integer);

    procedure DoExecute(Sender:TRtcConnection; var MyData:TRtcValueObject);
    procedure DoObjectManagerDataReady(Sender:TRtcRemoteObjectManager);
    procedure DoObjectManagerSend(Sender:TRtcRemoteObjectManager; Conn:TRtcConnection);

    procedure DoObjectCreate(Sender:TObject; Param:TRtcObjectCall);

    procedure DoReactivateObjectManager(Sender:TRtcConnection);
    procedure DoDeactivateObjectManager(Sender:TRtcConnection);

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

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

    // @exclude
    procedure ProcessDataReceived(Sender:TRtcConnection); virtual;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    { Return this TRtcClientModule's Object Manager.

      If this TRtcClientModule does NOT have an Object Manager,
      calling "GetObjectManager(True)" will create an Object Manager,
      while calling "GetObjectManager(False)" will return NIL. }
    function GetObjectManager(xCreate:boolean=False): TRtcRemoteObjectManager;

    { Activate the Object Manager associated with this TRtcClientModule. @html(<br><br>)

      If this TRtcClientModule does NOT have an Object Manager,
      calling "ActiveObjectManager(True)" will create an Object Manager,
      while calling "ActiveObjectManager(False)" will raise an exception. @html(<br><br>)

      If the "ObjectLinks" property is set to "ol_Manual", this method needs to be called
      before calling any remote functions on the Server which might return "Linked Objects" data.
      It also needs to be called before creating any Linked Objects in code on the Client-side. @html(<br><br>) }
    procedure ActivateObjectManager(xCreate:boolean=True);

    { Use this method if you want to force the removal of any
      Object Manager associated with this TRtcClientModule - now. }
    procedure RemoveObjectManager;

    { Use this method if you want to remove all Objects linked to
      the Object Manager associated with this TRtcClientModule - now. }
    procedure RemoveManagedObjects;

    // You can call this method from Interactive result function to destroy the ClientModule object.
    procedure Release;

    { Use this method when you want to force the next remote call to make a new login attempt,
      even if the component thinks the user is already logged in. }
    procedure ResetLogin;

    { If you want to post multiple calls in one request (and not each in its own request),
      you can use the "StartCalls" methods to open a new "call transaction", which is closed
      by a call to "Post". Each "StartCalls" has to be closed by a call to "Post". Using
      StartCalls/Call/Call/Post, you can combine a number of remote calls in one request.
      Another thing you don't have to worry about when using StartCalls/Post is clearing
      of the Data object in case of an exception during Data preparation. }
    procedure StartCalls; virtual;

    { After you have used the "Data" property to prepare the objects and functions
      you want to send to the server, use the "Call" method to define the event handler
      which has to receive the result after the data has been posted to the server.
      If you are not interested in the result values of your request, but just
      want to send this to the server as a "procedure call" and ignore the result,
      you can use "Call(nil)" and any result received will be simply thrown away.
      A result WILL be received in any case, which ensures that the function was executed.
      But, even in case of an exception, result will be ignored. @html(<br><br>)

      After "Call()", an object will be created to hold the prepared "Data" with a
      pointer to the TRtcResult component, after which the Data property will be cleared,
      so that you can prepare and Call new remote functions immediatelly. @html(<br><br>)

      If you are calling a remote function from inside other remote functions event,
      "FromInsideEvent" parameter has to be TRUE to avoid memory consumption and
      you should pass the Sender:TRtcConnection parameter to the Call() method. @html(<br><br>)

      If you didn't start a separate call transaction using "StartCalls", your call will be
      automaticaly posted to the server in a single request. To post multiple calls in
      one request, simply call "StartCalls" first, prepare "Data" before each "Call" and
      after the last call, use the "Post" method to send everything out. }
    procedure Call(ResultHandler:TRtcResult; FromInsideEvent:boolean=False; Sender:TRtcConnection=nil); overload;

    { "Post" will decrease the call transaction level which was increased by a "StartCalls"
      and if the last StartCalls was closed by calling this Post, an object will be created
      to hold the prepared Request info and a list of remote calls will be sent to the server
      if connection with server is established. @html(<br><br>)

      When posting from inside a RTC event or a remote function,
      "FromInsideEvent" parameter has to be TRUE to avoid memory consumption and
      you should pass the Sender:TRtcConnection parameter to the Post() method. @html(<br><br>)

      Events assigned to this TRtcClientModule will not be removed nor cleared,
      so you can define them at design-time and not worry about them at runtime. }
    procedure Post(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil); virtual;

    { Cancel *all* unsent calls prepared since "StartCalls", and decrement CallsLevel to zero (0). }
    procedure CancelCalls; virtual;

    { ONLY use this Request property if you need to prepare a request BEFORE posting.
      @html(<br>)
      DO NOT directly use this property when processing function calls.
      After a request has been posted, it is moved to the DataClient,
      so you can access it (from events) using Sender's Request property. }
    property Request:TRtcClientRequest read GetRequest;

    { To prepare a remote call, use this "Data" property to define and/or assign object(s)
      you want to send to the server. After you have defined the "Data" property to hold
      all information you want to send to the server, use the "Call" method to store that
      data and define the event handler to be used when a result is received. @html(<br>)
      ONLY use this Data property to prepare data for a remote call. @html(<br>)
      DO NOT directly use this property when processing the received result. }
    property Data:TRtcValue read Get_Data;

    { As an alternative to using the Data property directly for preparing data for
      the next function call, if you only want to prepare a single remote function call
      for which you would normally use "Data.newFunction()", you can also use "Prepare". @html(<br><br>)

      The "Prepare" method returns a pointer to the TRtcFunctionInfo object, which you
      can use to prepare all function parameters. Using "Prepare" is also easier than
      using the "Data" property directly, because each call to "Prepare" will first
      make a call to "Data.Clear" to make sure there are no left-overs from
      possible problems from any prior use of the "Data" or "Prepare" methods.
      That is, as long as you only use the "Prepare" method and do NOT use "Data" directly.

      Using "Prepare" is identical to using "Data.Clear" and "Data.NewFunction()"; }
    function Prepare(const FunctionName:RtcWideString):TRtcFunctionInfo;

    { If you have used the "Data" object or the "Prepare" method to start preparing a remote
      function call, but your preparations were interrupted for whatever reason and you won't
      be using the Execute nor the Call method to send that data to the Server, you can
      use "ClearData" to clear the memory of all the data prepared for that remote call. }
    procedure ClearData;

    { After using "Prepare" or "Data.newFunction" to create a new Function call object,
      you can use the "Param" property to prepare all your function call parameters. @html(<br><br>)

      Using "Param" is identical to using "Data.asFunction" }
    property Param:TRtcFunctionInfo read GetLastCallParams;

    { Using this property, you can check at which Calls level your ClientModule currently is.
      CallsLevel will be 0 outside of StartCalls, increased by 1 after each StartCalls
      and decreased by 1 after each Post. }
    property CallsLevel:integer read GetPostLevel;

    { If you want to send a remote function call to the Server and wait for the result without
      using a separate TRtcResult component and without writing the OnResult and OnAbort events,
      you can prepare a synchronized (blocking) call by using the "Prepare" method and then
      (once prepared) execute the remote call and get a result by using the "Execute" method. @html(<br><br>)

      In case of a communications error during the remote call, the "Execute" function will
      raise an exception with the appropriate error text. If everything goes well (no exeception),
      you will get your result directly from the "Execute" method in form of the TRtcValue object. @html(<br><br>)

      There are two ways one can use "Execute": @html(<br>)
        1) The easy way is to use "Execute" or "Execute(TRUE)" (meaning that AutoFreeResult=TRUE),
           in which case the Result object you receive will be automatically destroyed the next time
           you use the "Execute" method, or when the TRtcClientModule component is being destroyed. @html(<br>)
        2) The advanced way is to use "Execute(FALSE)", in which case you will have to FREE the
           result object received from "Execute(FALSE)" manually, once you are finished using it. @html(<br><br>)

      Using the "Execute" method is easier than using the "Call" method because: @html(<br>)
        (A) You will have access to the result data immediately after the "Execute" line,
            in the same code segment where you have "executed" your remote function call. @html(<br>)
        (B) You can either keep the result object for as long as you want and free it manually
            when using "Execute(FALSE)", or ... you can let the TRtcClientModule component free
            the result object automatically for you by using "Execute" or "Execute(TRUE)". @html(<br><br>)

      You can also specify a timeout (in seconds) how long you are ready to wait for a result.
      Should you get no result in your specified time period, request will be cancelled and an exception will be raised. @html(<br><br>)

      And ... if you are using a MultiThreaded connection and you do NOT WANT paint and other non-user messages to
      be processed while waiting for the result, call the "Execute" method with "AllowMessageProcessing=FALSE".
      For example: Execute(True, 30, False); // Auto free result, wait max 30 seconds, do NOT allow message processing while waiting. @html(<br><br>)

      Also take a look at the "LastResult" property, which is available to you if you call Execute with
      its default value for the AutoFreeResult parameter (TRUE) and gives you direct access to the result
      object received from the Execute method without having to use local variables or the "with" statement. @html(<br><br>)

      WARNING: Because of its blocking implementation, "Execute" method can NOT be used from events triggered
      by the RTC SDK (for example, from the OnResult or OnAbort events triggered by a remote call issued by
      using the "Call" method) and ... the "Execute" method can ONLY be used from one thread at a time. @html(<br><br>)

      Here are three usage examples, all of which are basically
      doing the same thing, but each using a different syntax.
      You can combine them any way you want: @html(<br><br>)

      @longCode(#
      ** EXAMPLE 1 **

      with MyClientModule do
        begin
        Prepare('myfunctionname');
        Param.asString['par1']:='Hi';
        Param.asInteger['par2']:=12345;
        Execute;
        // use the "LastResult" property to access the result data
        end;

      ** EXAMPLE 2 **

      with MyClientModule do
        begin
        with Prepare('myfunctionname') do
          begin
          asString['par1']:='Hi';
          asInteger['par2']:=12345;
          end;
        with Execute do // result object will be Freed automatically
          begin
          // ... access the result data here ...
          end;
        end;

      ** EXAMPLE 3 **

      with MyClientModule do
        begin
        Data.Clear;
        with Data.newFunction('myfunctionname') do
          begin
          asString['par1']:='Hi';
          asInteger['par2']:=12345;
          end;
        myRes:=Execute(False); // you will need to Free "myRes"
        try
          .. use myRes ...
        finally
          myRes.Free;
          end;
        end;
      #)
      }
    function Execute(AutoFreeResult:boolean=True; _Timeout:cardinal=0; AllowMessageProcessing:boolean=True):TRtcValue;

    { After the last "Execute" call, if you have used the default value (TRUE) for the "AutoFreeResult" parameter
      ("Execute" or "Execute(True)") to signal the component that you do NOT want to keep the result object to yourself
      and that you want the result object to be freed automatically by the component before the next "Execute" call,
      you can use the "LastResult" property to access the last object returned from the "Execute" method. @html(<br><br>)

      Please note that the "LastResult" property will point to the result returned from the LAST "Execute" call.
      "LastResult" will return NIL if there was an error during the last Execute call (the call was not finished)
      or if you have last used Execute(FALSE) -> using "AutoFreeResult=FALSE" in your Execute call parameters. }
    property LastResult:TRtcValue read FExecuteResult;

  published
    { Use this property to define what compression level you want to use when sending
      data from Client to Server. Default Compression value is "cNone" (no compression).
      You can use different compression levels between client and server (for example,
      fast or no compression from client and max from server). If your client has to
      work with servers which don't support compression, you have to use "cNone".

      RTC Compression is ONLY used if DataFormat="fmt_RTC" and DataReqMode="req_contentBodyALL"! }
    property Compression:TRtcCompressLevel read FCompress write SetCompress default cNone;

    { Use this property to define what data format you want to use when sending data from
      Client to Server. If your client will only be talking to a Server written using RTC
      components, it is recommended to use DataFormat="fmt_RTC" & DataReqMode="req_contentBodyALL",
      which supports all RTC data types, automatic compression, automatic encryption and automatic
      Sessions, as well as nested function calls and multiple function calls in a single request.
      On the other hand, if your client has to communicate with a Server which does NOT support
      the RTC format, you can use on one the XML-RPC or JSON-based formats, depending on
      the requirements specified by the Server you have to communicate with. @html(<br><br>)
      NOTE: Since advanced functionality like automatic Compression, auto Encryption
      and automatic Sessions are native to the RTC format and other Servers do not implement it,
      those advanced properties will be ignored and assumed to be FALSE with all formats other
      then the "fmt_RTC" format. If you need those advanced options, use the "fmt_RTC" format. }
    property DataFormat:TRtcDataFormat read FDataFormat write SetDataFormat default fmt_RTC;

    { Use this property to define what data request mode you want to use when sending data
      from Client to Server. If your client will only be talking to a Server written using RTC
      components, it is recommended to use DataReqMode="req_contentBodyALL" & DataFormat="fmt_RTC",
      which supports all RTC data types, automatic compression, automatic encryption and automatic
      Sessions, as well as nested function calls and multiple function calls in a single request.
      On the other hand, if your client has to communicate with a Server which does NOT support
      the RTC format, you can use of the XML-RPC or JSON-based Data formats using the "DataFormat"
      property and select the required data request mode using the "DataReqMode" property. @html(<br><br>)
      NOTE: Since advanced functionality like automatic Compression, auto Encryption
      and automatic Sessions are native to the RTC format and other Servers do not implement it,
      those advanced properties will be ignored and assumed to be FALSE with all formats other
      then the "fmt_RTC" format. If you need those advanced options, use the "fmt_RTC" format. }
    property DataReqMode:TRtcDataReqMode read FDataReqMode write SetDataReqMode default req_contentBodyALL;

    { "AutoSessionField" property defines the Field Name in the Request "Query", Request "Cookie" 
      and  Response "Cookie" which ClientModule and ServerModule should use for the "Session ID".
      If not assigned, field name 'ID' will be used by default. This property has to be identical
      on TRtcClientModule and TRtcServerModule components for automatic Session handling to work. }
    property AutoSessionField:RtcString read FAutoSessionField write SetAutoSessionField;

    { Set 'AutoSessionMode:=rsm_Cookie' if the Session ID should be sent in the
      Request "Cookie" instead of the Request "Query" (URI). @html(<br><br>)

      By default (AutoSessionMode=rsm_Query), Session IDs are sent in the Request "Query". }
    property AutoSessionMode:TRtcSessionsMode read FAutoSessionMode write FAutoSessionMode default rsm_Query;

    { By default, TRtcClientModule will be using Content Type "text/xml" for XML-RPC calls,
      "application/json-rpc" for "fmt_JSONrpc1" and "fmt_JSONrpc2" calls and
      "application/json" for other calls using the "fmt_JSON" format. In case
      you need to access a 3rd-party API which requires a different HTTP Content-Type,
      make sure to use this property and assign the Content-Type required by the API. }
    property DataContentType:RtcString read FDataContentType write FDataContentType;

    { Some APIs require some sort of authentication in the Request "Query",
      which has to be included with every request sent out to the Server.
      You can use this property to assign a custom String, which will be included in
      the "Query" for every Request sent out using this "TRtcClientModule" component. }
    property DataCustomQuery:RtcString read FDataCustomQuery write FDataCustomQuery;

    { If you want to enable the possibility to use this Client Module to send remote function
      calls from multiple threads AT THE SAME TIME, where this component will acs as if
      it were X components, one for each thread, simply set HyperThreading to TRUE. @html(<br><br>)

      This is useful if you need to send remote function calls to another Server from
      within your Server running in multi-threaded mode and want to use only one set of
      rtcHttpClient/rtcClientModule components for all clients connected to your Server.
      Even in HyperThreading mode, only properties and methods needed to prepare and post remote
      function calls (Data, Request, Call, StartCalls, PostLevel and Post) will use a separate
      copy for each thread, while all other properties and methods exist only once for all threads,
      so don't try to modify them while your application is actively using the component in
      multi-threaded mode. @html(<br><br>)

      Leave HyperThreading as FALSE to use this component "the stadard way" (for example,
      when you're writing a client application where remote calls are done from the main thread
      or if you are creating a separate component for every thread that needs it). }
    property HyperThreading:boolean read FHyperThreading write FHyperThreading default False;

    { - Set "ObjectLinks" to "ol_None" (default) to completely disable the "RTC Linked Objects"
        feature for this RTC Client Module. When "ObjectLinks=ol_None", calling the
        "ActivateObjectManager" method will raise an exception, because any RTC Linked Objects
        created for that Object Manager would NOT be sent to the Server. @html(<br><br>)

        When writing Client Applications using the "RTC Linked Objects" feature, only
        *one* "TRtcClientModule" should be used for all "RTC Linked Objects" communication.
        Leave the "ObjectLinks" property on all other TRtcClientModule components at "ol_None".
        Following that rule eliminates the need for the Client to "switch" between Object Managers,
        which keeps the Client-side code a lot easier to implement and to maintain. @html(<br><br>)

      - Set "ObjectLinks" to "ol_Manual" if you want to call the "ActivateObjectManager" method
        manually before any "Linked Objects" may be created by using this TRtcClientModule.

      - Set "ObjectLinks" to "ol_AutoServer" if you want an Object Manager to be created automatically
        after "Linked Objects" data is received from the Server, which allows the Server to create
        Linked Objects without having to wait for the Client to call "ActivateObjectManager". @html(<br><br>)

        With "ObjectLinks=ol_AutoServer", the Client will still have to call "ActivateObjectManager"
        before creating Linked Objects locally on the Client-side, because the "ol_AutoServer" setting
        only has an effect on data received for Linked Objects from the Server. @html(<br><br>)

        "Linked Objects" are usually created on the Server, while Clients only re-create them.
        TRtClientModule in that setup should set its "LinkedObjects" property to "ol_AutoServer"
        and the TRtcServerModule (Server-side) should use "ol_Manual" or "ol_AutoServer". @html(<br><br>)

      - Set "ObjectLinks" to "ol_AutoClient" or "ol_AutoBoth" if you want an Object Manager to be
        created for this TRtcClientModule immediatelly (when this property is set), even if there is
        no connection to the Server. This will allow you to start creating Linked Objects on the Client
        immediatelly, without having to call the "ActivateObjectManager" method manually. It will also
        ensure that a new Object Manager is created automatically in case this one is closed or removed. @html(<br><br>)

        If Linked Objects need to be created manually on the Client, while the Server only recreates
        them, the best option is to set the "ObjectLinks" property on the Client (TRtcClientModule)
        and on the Server (TRtcServerModule) to "ol_AutoClient". This will create the Object Manager
        on the Client-side immediatelly, so that the Client can start creating Linked Objects locally
        and the Server will create and activate an Object Manager when receiving Linked Objects data. @html(<br><br>)

        NOTE: Destroying the Server-side "Linked Object" instance will also destroy the
        Client-side instance and vice-versa, so you shouldn't keep a reference to a "Linked Object"
        inside global variables - unless you have set up destruction notification events
        to clear these refferences in case the instance is being destroyed. }
    property ObjectLinks:TRtcObjectLinkSupport read FObjectLinkSupport write SetObjectLinkSupport default ol_None;

    { Set this property to a value other than 0 if you want to use automatic Encryption
      with a random generated key of "EncryptKey" bytes. One byte stands for
      encryption strength of 8 bits. For strong 256-bit encryption, use 32 bytes. @html(<br><br>)

      The final encryption key is combined from a client-side key and a key
      received from the server, where server decides about its encryption strength.
      If server doesn't support Encryption, data will not be encrypted,
      regardless of the value you use for EncryptionKey. @html(<br><br>)

      EncryptionKey uses sessions to keep the encryption keys. When you set EncryptionKey
      to a value other than 0 (turn it ON), AutoSessions will be set to TRUE.
      Also, setting AutoSessions to FALSE will set EncryptionKey to 0 (OFF).

      RTC Encryption is ONLY used if DataFormat="fmt_RTC" and DataReqMode="req_contentBodyALL"! }
    property EncryptionKey:integer read FAutoEncrypt write SetAutoEncrypt default 0;

    { Data Encryption Mode (used ONLY if the "EncryptionKey" property is greater than zero).
      Available modes are "rem_Basic" (old, using a single set of randomized keys) and "rem_ISAAC"
      (new, increased security by continually randomizing keys using ISAAC randomization algorithms).
      Check the "EncryptionKey", "RSAKey" and "SecureKey" properties for more details.

      RTC Encryption is ONLY used if DataFormat="fmt_RTC" and DataReqMode="req_contentBodyALL"! }
    property EncryptionMode:TRtcEncryptionMode read FEncryptMode write SetEncryptMode default rem_Basic;

    { If you want to make sure that your communication is Securely encrypted without the
      need to use a "SecureKey" (which has to be the same on the Client and the Server side),
      you can enable the generation of a random RSA Key Pair for the initial Key exchange
      by setting the RSAKey property to a value greater than 0. The value is set in Bytes
      of Key Strength, where each byte stands for 8 bits of RSA Key Strength. For strong
      RSA Key generation, use RSAKey values between 32 and 128 bytes (= 256 to 1024 Bits).

      NOTE: Unlike the "EncryptionKey", which is generated very fast for any size (it only
      takes some time to send the Key), the time it takes for a RSA Key Pair to be generated
      on the Client grows EXPONENTIALLY with each additional byte of Key strength, so you
      should find the RSAKey value that works best for your Clients. For example, even older
      mobile devices should be able to generate a 32 Byte RSA Key (256 bits) almost instantly,
      but generating a 512 Byte RSA Key (4096 bits) can take several minutes on a modern CPU.
      Because a new random RSA Key Pair will be generated EVERY TIME the Client connects to
      the Server and opens a new Session, there is no real need for extremely large keys.

      RSAKey will be used in the encryption initialisation handshake,
      to generate a public Key which is sent to the Server, so the Server
      can use it to encrypt the first Server-side Key sent from the ServerModule.
      Since all other data packages are already sent using some kind of encryption,
      by defining a RSAKey, you will encrypt the only key part which would have
      been sent out without encryption. NOTE: RSAKey property is used
      ONLY if the EncryptionKey property is also greater than zero.

      RTC Encryption is ONLY used if DataFormat="fmt_RTC" and DataReqMode="req_contentBodyALL"! }
    property RSAKey:integer read FRSAKey write FRSAKey default 0;

    { If you need a 100% secure connection, define a Secure Key string
      (combination of letters, numbers and special characters) for each
      ServerModule/ClientModule pair, additionally to the EncryptionKey value.
      ClientModule will be able to communicate with the ServerModule ONLY if
      they both use the same SecureKey. Default value for the SecureKey is
      an empty string (means: no secure key). @html(<br><br>)

      SecureKey will be used in the encryption initialisation handshake,
      to encrypt the first random Key sent by the ClientModule, or the
      RSA Public Key when using the "RSAKey" property (for RSA encryption).
      Since all other data packages are already sent using some kind of encryption,
      by defining a SecureKey, you encrypt the only key part which would have
      been sent out without special encryption.

      RTC Encryption is ONLY used if DataFormat="fmt_RTC" and DataReqMode="req_contentBodyALL"! }
    property SecureKey:RtcString read FSecureKey write FSecureKey;

    { Setting this property to TRUE will tell the ClientModule to work with the
      Server ONLY if Server supports encryption. If AutoEncryptKey is > 0 and
      server doesn't support encryption, function calls will not be passed to
      the server and any response coming from the server will be rejected, until
      server enables encryption.

      RTC Encryption is ONLY used if DataFormat="fmt_RTC" and DataReqMode="req_contentBodyALL"! }
    property ForceEncryption:boolean read FForceEncrypt write SetForceEncrypt default False;

    { Set this property to TRUE if you want ClientModule to request a new session
      automatically if the Session.ID is not set when posting a request.
      Session handling is built into the ClientModule and uses Request.Params to
      send the Session.ID to the server and Response.Cookie[AutoSessionField] to receive a
      new session ID from the server and initialize the session object. @html(<br><br>)

      Since session ID's are communicated automaticaly by the ClientModule and
       ServerModule components, all TRtcFunction and TRtcResult components used by
      this ClientModule will have direct access to the session object.
      When AutoSessions is set to true, a new session will be requested if
      no session exists or when a session expires. @html(<br><br>)

      When AutoSessions is FALSE, you have to request a new session by calling
      a remote server function to generate a new session and return the session ID. }
    property AutoSessions:boolean read FAutoSessions write SetAutoSessions default false;

    { Use "AutoSessionsPing" to enable automatic "PING" calls to the Server when there
      is an active Session but no Remote Calls have been sent to the Server using
      THIS TRtcClientModule for longer than "AutoSessionsPing" seconds. }
    property AutoSessionsPing:integer read FAutoSessionsPing write SetAutoSessionsPing default 0;

    { Set this property to a value other than 0 (zero) if you want the ClientModule to
      auto-repost any request up to "AutoRepost" times, in case the connection gets lost
      while sending data to server or receiving data from server.
      If value is lower than zero, requests will be reposted unlimited number of times. }
    property AutoRepost:integer read FAutoRepost write FAutoRepost default 0;

    { Set this property to TRUE if you want to enable the use of the
      OnLogin, OnLoginResult and OnLoginAborted events to implement automatic login. }
    property AutoLogin:boolean read FAutoLogin write SetAutoLogin default false;

    { "Request.Host" will be assigned this property before sending the request out. @html(<br>)
      It is not necessary to set this property if your server's ServerModule component
      left its ModuleHost property blank and there's no remote Functions used by that
      ServerModule which would explicitly use the "Request.Host" header. On the other hand,
      for servers which serve multiple hosts, mostly where ServerModule has assigned its
      ModuleHost property, it is very important to set this ClientModule's ModuleHost
      property to the appropriate host name. }
    property ModuleHost:RtcString read GetModuleHost write SetModuleHost;
    { To be able to call remote functions, this ClientModule's "ModuleFileName"
      property has to be identical to the "ModuleFileName" property of the ServerModule
      which you want to use. Before sending the request out to the Server, any value assigned
      to the "ModuleFileName" property will be injected into the "Request.FileName" property. }
    property ModuleFileName:RtcString read GetModuleFileName write SetModuleFileName;
    { Set this property to tell the RtcClientModule to use this TRtcFunctionGroup
      component to execute all functions received as a response from server, for
      any request sent from this TRtcClientModule component. }
    property FunctionGroup:TRtcFunctionGroup read GetFunctionGroup write SetFunctionGroup;

    { This event will be called if your SecureKey does not match the SecureKey
      specified by the ServerModule you're connecting to.
      On this event, you can decide not to work with that server (Response.Reject or Disconnect),
      or to update your SecureKey property to mirror the SecureKey of your ServerModule. }
    property OnEncryptWrongKey:TRtcNotifyEvent read FOnWrongEncryption write FOnWrongEncryption;
    { This event will be called if your EncryptionKey>0 and ForceEncryption=TRUE,
      but the Server says it does not support encryption for this ServerModule.
      On this event, you can decide not to work with that server (Response.Reject or Disconnect),
      or to set your ForceEncryption property to False and repost the request. }
    property OnEncryptNotSupported:TRtcNotifyEvent read FOnNoEncryption write FOnNoEncryption;
    { This event will be called if your EncryptionKey=0,
      but the Server wants to ForceEncryption for this ServerModule.
      On this event, you can decide to not work with that server (Response.Reject or Disconnect),
      or to activate encryption by setting the EncryptionKey. }
    property OnEncryptRequired:TRtcNotifyEvent read FOnNeedEncryption write FOnNeedEncryption;

    { This event will be called if we receave invalid response from the Server,
      which could mean that our Client or our Server are not up to date. }
    property OnResponseError:TRtcNotifyEvent read FOnResponseError write FOnResponseError;
    { This event will be called if you have called a remote function with a Session ID that
      has expired. You can choose to clear the local Session object and Repost the request
      with an empty session ID to receive a new session ID, or reject the Request.
      If you do not implement this event, Session ID will be cleared and the request
      will be reposted, so your client will receive a new Session ID. }
    property OnSessionExpired:TRtcNotifyEvent read FOnSessionExpired write FOnSessionExpired;
    { This event will be called after ClientModule component has prepared the request for sending,
      but before the request has been sent out (no writing has been done yet).
      You can use this event to check or update the request object before it is sent out. @html(<br>)
      This event does not have to be defined for the ClientModule to work. }
    property OnBeginRequest:TRtcNotifyEvent read FOnBeginRequest write FOnBeginRequest;

    { Called after a complete response has been received from the Server for THIS component,
      after the content has been decrypted and decompressed by the component (ready for parsing),
      but BEFORE the response content body has been processed and removed from receiving buffers.
      You can use this event for centralized logging and monitoring of complete responses
      received, decompressed, decrypted (and later processed) by this components, but do
      NOT use Read or ReadEx methods here, because they will immediately clear receiving buffers!
      To access the response content body from this event, use the "PeekEx" method instead. }
    property OnPeekResponse:TRtcNotifyEvent read FOnPeekResponse write FOnPeekResponse;

    { This event will be called after the last DataReceived event for this request,
      read after the request has been sent out and a complete response was received (Response.Done). @html(<br>)
      This event does not have to be defined for the ClientModule to work. }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { This event will be called after the OnConnectLost, OnConnectFail and OnConnectError events,
      if the request was NOT marked for reposting. }
    property OnRepostCheck:TRtcNotifyEvent read FOnRepostCheck write FOnRepostCheck;
    { This event will be called after the OnRepostCheck event, if the request was not marked for reposting.
      If this event gets triggered, it means that there is a major problem with the server and
      user has to be notified about that problem and consulted about further actions. }
    property OnResponseAbort:TRtcNotifyEvent read FOnResponseAbort write FOnResponseAbort;
    { This event will be called after the response has been rejected by calling "Response.Reject" }
    property OnResponseReject:TRtcNotifyEvent read FOnResponseReject write FOnResponseReject;

    { This event will be called after a new Session has been opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { This event will be called before an existing Session is going to be closed. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;

    { This event will be mapped as @Link(TRtcClient.OnConnectLost) event
      to the assigned DataClient component and called if your connection gets
      closed while you are still processing your request. }
    property OnConnectLost:TRtcNotifyEvent read FOnConnectLost write FOnConnectLost;

    { Use this event to implement automatic user login. Set AutoLogin to TRUE and
      this event will be fired immediately after the initial connection handshake.
      To prepare the remote function, use the "Data" object passed as a parameter.
      After the function call has returned, the OnLoginResult event will be triggered.
      If there was an error and the request was aborted, OnLoginAborted will be called. }
    property OnLogin:TRtcFunctionPrepareEvent read FOnLogin write FOnLogin;

    { Use this event to implement automatic user login. See "OnLogin" for more info. }
    property OnLoginResult:TRtcResultEvent read FOnLoginResult write FOnLoginResult;

    { Use this event to implement automatic user login. See "OnLogin" for more info. }
    property OnLoginAborted:TRtcResultEvent read FOnLoginAborted write FOnLoginAborted;

    { Use this event to implement automatic Sessin Pinging. Set AutoSessionsPing to
      the number of seconds a connection may remain inactive before we need to send a
      "PING call" to the Server to ensure that our Session will not Expire.
      This event is fired every time when a PING request has to be sent to the Server.
      To prepare the PING call, use the "Data" object passed as a parameter.
      After the PING call has returned, the OnPingResult event will be triggered.
      If there was an error and the request was aborted, OnPingAborted will be called. }
    property OnPing:TRtcFunctionPrepareEvent read FOnPing write FOnPing;

    { Use this event to implement automatic Session Pinging. See "OnPing" for more info. }
    property OnPingResult:TRtcResultEvent read FOnPingResult write FOnPingResult;

    { Use this event to implement automatic Session Pinging. See "OnPing" for more info. }
    property OnPingAborted:TRtcResultEvent read FOnPingAborted write FOnPingAborted;

    { This event will be triggered after the "OnBeginRequest" event,
      for every remote call being prepared for sending through this component,
      before triggering the "PreparingCall" event on the "TRtcResult" component
      (assigned to the "Call" method to receive the Result from the Server).

      This event can be used to monitor, modify and/or skip remote calls before sending.

      The Sender parameter (connection component) can be used to access Session,
      Request or Response information, but NOT to Read the content Body or
      to Write the content out directly to the connection component.

      Data parameter will contain the remote call,
      Result parameter will be NIL.

      Set "Data.isNull:=TRUE" in this event if you want to SKIP sending this remote call.
      This will also SKIP triggering the "PreparingCall" event on the "TRtcResult" component. }
    property OnPrepareCall:TRtcResultEvent read FOnPrepare write FOnPrepare;

    { This event will be triggered if a request for synchronizing "Linked Objects" failed. }
    property OnObjectLinkAborted:TRtcResultEvent read FOnObjectLinkAborted write FOnObjectLinkAborted;

    { This event will be triggered if there were Errors processing data to synchronize "Linked Objects".
      Any exceptions raised from THIS event will be silently handled. }
    property OnObjectLinkErrors:TRtcResultEvent read FOnObjectLinkErrors write FOnObjectLinkErrors;

    { This event is triggered when data is received from a remote "Object Manager".
      The main purpose of this event is to allow you to *monitor* all received "Linked Objects"
      packages without changing anything, but it could also be used to modify received data
      before it is forwarded to the local "Object Manager" for processing/execution.

      @param(Sender - NIL, or the connection component through which data was received)
      @param(Data - Data received from the remote "Object Manager".
             Unless you know exactly what you are doing and understand the format which
             local and remote "Object Manager" instances are using for communication,
             it is highly recommended that you do NOT make ANY changes to this instance.
             This is the instance that will be passed over to our "Object Manager" later) }
    property OnObjectDataIn:TRtcDataEvent read FOnObjectDataIn write FOnObjectDataIn;

    { This event is triggered *before* we send data prepared by our "Object Manager".
      The main purpose of this event is to allow you to *monitor* all "Linked Objects"
      packages before they are sent out, but it could also be used to modify prepared data.

      @param(Sender - NIL if using the default connection; "Sender" parameter for the Call method)
      @param(Data - Data prepared by our local "Object Manager" for sending to remote "Object Manager".
             Unless you know exactly what you are doing and understand the format which
             local and remote "Object Manager" instances are using for communication,
             it is highly recommended that you do NOT make ANY changes to this instance.
             This is the instance that will be sent over to remote "Object Manager") }
    property OnObjectDataOut:TRtcDataEvent read FOnObjectDataOut write FOnObjectDataOut;

    { This event is triggered when the remote Object Manager has requested
      our Object Manager to create a new Object (remote instance was already created)
      and allows you to create Objects which don't have a global constructor
      registered (through the global "RegisterRtcObjectConstructor" procedure).

      Objects which you do NOT want to have created automatically by the remote
      side, but where you still want to allow controlled creation should NOT have
      a global constructor registered and should be created from THIS event instead. }
    property OnObjectCreate:TRtcObjectCreateEvent read FOnObjectCreate write FOnObjectCreate;

    { This event will be called when a Result has been received from a Server for a
      remote function call sent using this component, after executing local functions
      called by the Server directly (using the assigned "FunctionGroup"), but BEFORE
      calling the "OnReturn" event of the component assigned to process the Result. }
    property OnResultReady:TRtcResultEvent read FOnResultReady write FOnResultReady;

    { This event will be called if an Exception is raised while (A) processing the Result
      received from the Server, or (B) memory can not be cleaned up afterwards. }
    property OnResultError:TRtcResultErrorEvent read FOnResultError write FOnResultError;
    end;

{ Call this procedure if user interaction is required anywhere inside your result event.
  When this procedure is called, the event will be posted to the main thread outside of
  the client connection's context, so that the connection can continue functioning
  and receiving new data, without being blocked by a window waiting for user input.
  Without using "PostInteractive" to post the event, the connection would be blocked until
  the event returns. This could take several minutes if user doesn't notice your window,
  which would most likely result in a session timeout on the server, so the user would
  be automaticaly logged out after he responded to your questions. @html(<br><br>)

  Even though events are posted outside of the connection context, a mechanism integrated
  into TRtcClientModule will take care that all events posted interactively from the same
  ClientModule's result event, do not overlap or get called in a different order. So,
  if you need your result events to block any upcoming resuts, you can post all your
  dependent events interactively to ensure they will get called AFTER the user responded,
  while the connection continues receiving new data from the server and keeps the session alive. @html(<br><br>)

  NOTE: This procedure works only when called from inside TRtcResult event
  which was triggered by TRtcClientModule to return a result from a remote function call.
  When a function is called asynchtonously outside of the connection context,
  Sender parameter is NIL. This has two reasons: @html(<br>)
  1. You can check "Sender" to know if your event would block a connection. @html(<br>)
  2. You can not expect the connection to remain in the same state forever and you
     can not use the connection directly from an interactive event. }
procedure PostInteractive;

implementation

const
  CLIMOD_ACTIVE = 'ClientModule$';
  CLIMOD_CALLS  = 'CLIMOD.CALL$';
  CLIMOD_LOGIN  = 'CLIMOD.LOGIN$';
  CLIMOD_READY  = 'CLIMOD.READY$'; 

procedure PostInteractive;
  begin
  raise EPostInteractive.Create('');
  end;

{ TRtcClientModuleData }

constructor TRtcClientModuleData.Create;
  begin
  inherited;
  FRequest:=nil;
  FData:=nil;
  FCalls:=nil;
  FPostLevel:=0;
  end;

destructor TRtcClientModuleData.Destroy;
  begin
  try
    RtcFreeAndNil(FRequest);
    RtcFreeAndNil(FData);
    RtcFreeAndNil(FCalls);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcClientModuleData.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcClientModule }

var
  ActiveGlobalManager:boolean=False;

constructor TRtcClientModule.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FDataFormat:=fmt_RTC;
  FDataReqMode:=req_contentBodyALL;
  FAutoSessionField:='';
  FAutoSessionFieldUPC:='';
  FDataContentType:='';
  FDataCustomQuery:='';
  FAutoSessionMode:=rsm_Query;

  FHyperThreading:=False;

  FIntCS:=TRtcCritSec.Create;
  FIntRes:=TXObjList.Create(16);
  FIntTimer:=nil;

  FPingTimer:=nil;

  FRelease:=False;
  FFunctions:=nil;
  FModuleFileName:='';
  FModuleFileREST:='';
  FModuleHost:='';
  FCryptSesName:='.$CLI-CRYPT$';
  FObjManSesName:=RTCL_CLISESSION;
  FAutoRepost:=0;

  FAutoEncrypt:=0;
  FEncryptMode:=rem_Basic;

  FCS:=TRtcCritSec.Create;
  FMyData:=tObjList.Create(16);
  FMainThrData:=TRtcClientModuleData.Create;

  FExecuteResult:=nil;
  FExecuteError:='';
  FExecuteHandler:=TRtcResult.Create(nil);
  FExecuteHandler.OnReturn:=Call_ExecuteResult;
  FExecuteHandler.RequestAborted:=Call_ExecuteAbort;

  FLoginResult:=TRtcResult.Create(nil);
  FLoginResult.OnReturn:=Call_LoginResult;
  FLoginResult.RequestAborted:=Call_LoginAborted;

  FPingResult:=TRtcResult.Create(nil);
  FPingResult.OnReturn:=Call_PingResult;
  FPingResult.RequestAborted:=Call_PingAborted;

  FObjectLinkResult:=TRtcResult.Create(nil);
  FObjectLinkResult.OnReturn:=Call_ObjectLinkResult;
  FObjectLinkResult.RequestAborted:=Call_ObjectLinkAborted;

  FObjectLinksOut:=False; // Linked Objects state: idle
  FObjectLinkSupport:=ol_None;
  end;

destructor TRtcClientModule.Destroy;
  begin
  try
    FRelease:=True;

    if assigned(FPingTimer) then
      begin
      TRtcTimer.Stop(FPingTimer);
      FPingTimer:=nil;
      end;

    if ActiveGlobalManager and (FObjectLinkSupport in [ol_AutoClient,ol_AutoBoth]) then
      ActiveGlobalManager:=False;
    RemoveObjectManager;

    FFunctions:=nil;
    FModuleFileName:='';
    FModuleFileREST:='';
    FCryptSesName:='';
    FObjManSesName:='';
    FModuleHost:='';
    FAutoSessionField:='';
    FAutoSessionFieldUPC:='';
    FDataContentType:='';
    FDataCustomQuery:='';

    ClearAllThreadsData;

    RtcFreeAndNil(FMainThrData);
    RtcFreeAndNil(FMyData);
    RtcFreeAndNil(FExecuteResult);
    FExecuteError:='';

    RtcFreeAndNil(FExecuteHandler);
    RtcFreeAndNil(FLoginResult);
    RtcFreeAndNil(FPingResult);
    RtcFreeAndNil(FObjectLinkResult);

    RtcFreeAndNil(FIntRes);
    RtcFreeAndNil(FIntCS);
    RtcFreeAndNil(FCS);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcClientModule.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcClientModule.SetAutoSessionField(const Value:RtcString);
  begin
  FAutoSessionField:=URL_Encode(Value);
  FAutoSessionFieldUPC:=Upper_Case(FAutoSessionField);
  end;

function TRtcClientModule.SessIDField:RtcString;
  begin
  if FAutoSessionField='' then
    Result:='ID'
  else
    Result:=FAutoSessionField;
  end;

function TRtcClientModule.SessIDFieldCS:RtcString;
  begin
  if FAutoSessionFieldUPC='' then
    Result:='ID'
  else
    Result:=FAutoSessionFieldUPC;
  end;

function TRtcClientModule.GetCrypt(Sender:TRtcConnection): TRtcCryptClient;
  begin
  Result:=TRtcCryptClient(Sender.Session._asObj[FCryptSesName]);
  end;

procedure TRtcClientModule.NewCrypt(Sender:TRtcConnection);
  var
    ob:TObject;
  begin
  with Sender.Session do
    begin
    ob:=_asObj[FCryptSesName];
    if ob<>nil then
      TRtcCryptClient(ob).Init(EncryptionMode,RSAKey)
    else
      _asObj[FCryptSesName]:=TRtcCryptClient.Create(EncryptionMode,RSAKey);
    end;
  end;

procedure TRtcClientModule.DelCrypt(Sender:TRtcConnection);
  var
    ob:TObject;
  begin
  with Sender do
    begin
    ob:=Session._asObj[FCryptSesName];
    if ob<>nil then
      begin
      Session._asObj[FCryptSesName]:=nil;
      ob.Free;
      end;
    end;
  end;

procedure TRtcClientModule.ResetCrypt(Sender:TRtcConnection);
  var
    o:TRtcCryptClient;
  begin
  with Sender do
    begin
    o:=TRtcCryptClient(Session._asObj[FCryptSesName]);
    if o<>nil then
      begin
      if o.AllReady and Request.Reposting and (Request.Reposted<2) then
        Request.Query.ValueCS['ACTION']:='RESET'
      else
        begin
        Session._asObj[FCryptSesName]:=nil;
        RtcFreeAndNil(o);
        end;
      end;
    end;
  end;

procedure TRtcClientModule.Call_ConnectLost(Sender: TRtcConnection);
  begin
  if assigned(FOnConnectLost) then
    if not AutoSyncEvents or not Sender.Sync(FOnConnectLost) then
      FOnConnectLost(Sender);
  end;

procedure CryptReadEx(Crypt:TRtcCryptClient; var Data:RtcByteArray);
  begin
  if assigned(Crypt) then
    if assigned(Crypt.CRead) then
      Crypt.CRead.DeCryptEx(Data)
  {$IFDEF RTC_RSA}
    else if assigned(Crypt.CReadEx) then
      Crypt.CReadEx.DeCrypt(Data){$ENDIF};
  end;

procedure CryptWriteEx(Crypt:TRtcCryptClient; var Data:RtcByteArray);
  begin
  if assigned(Crypt) then
    if assigned(Crypt.CWrite) then
      Crypt.CWrite.CryptEx(Data)
  {$IFDEF RTC_RSA}
    else if assigned(Crypt.CWriteEx) then
      Crypt.CWriteEx.Crypt(Data){$ENDIF};
  end;

function GenerateControlKey(var Counter:integer):RtcString;
  var
    len,a,b,c:integer;
  begin
  Inc(Counter);
  if Counter>99 then Counter:=1;

  len:=5+random(5);
  SetLength(Result,len+4);
  b:=(10-len)*9+8;
  for a:=5 to len+4 do
    begin
    c:=random(10); Inc(b,c);
    Result[a]:=RtcChar(c+Ord('0'));
    end;
  Result[1]:=RtcChar(b div 10 + Ord('0'));
  Result[2]:=RtcChar(b mod 10 + Ord('0'));
  Result[3]:=RtcChar(Counter div 10 + Ord('0'));
  Result[4]:=RtcChar(Counter mod 10 + Ord('0'));
  end;

function SlashName(const fname1,fname2:RtcString):RtcString;
  begin
  if fname2='' then
    begin
    if fname1='' then
      Result:='/'
    else if fname1[1]<>'/' then
      Result:='/'+fname1
    else
      Result:=fname1;
    end
  else if fname2[1]='/' then
    begin
    if fname1='' then
      Result:=fname2
    else if fname1[1]<>'/' then
      begin
      if fname1[length(fname1)]='/' then
        Result:='/'+Copy(fname1,1,length(fname1)-1)+fname2
      else
        Result:='/'+fname1+fname2;
      end
    else
      begin
      if fname1[length(fname1)]='/' then
        Result:=Copy(fname1,1,length(fname1)-1)+fname2
      else
        Result:=fname1+fname2;
      end;
    end
  else if fname1='' then // fname2[1] <> '/'
    Result:='/'+fname2
  else if fname1[1]<>'/' then
    begin
    if fname1[length(fname1)]<>'/' then
      Result:='/'+fname1+'/'+fname2
    else
      Result:='/'+fname1+fname2;
    end
  else
    begin
    if fname1[length(fname1)]<>'/' then
      Result:=fname1+'/'+fname2
    else
      Result:=fname1+fname2;
    end;
  end;

procedure TRtcClientModule.Call_BeginRequest(Sender: TRtcConnection);
  var
    idx,i:integer;
    MyCalls:TRtcClientModuleCallsArray;
    compressed:boolean;
    fname:RtcWideString;
    code:RtcString;
    crypt:TRtcCryptClient;
    DataReq:TRtcDataRequestInfo;
    MyRequest:TRtcClientRequest;
    output:TRtcHugeByteArray;
    codeEx:RtcByteArray;
  {$IFDEF COMPRESS}
    tempEx:RtcByteArray;
  {$ENDIF}
    val:TRtcValue;
    HaveCleared:integer;
  function isNewRequest:boolean;
    begin
    Result:=not Sender.Request.Info.asBoolean[CLIMOD_READY];
    if Result then
      Sender.Request.Info.asBoolean[CLIMOD_READY]:=True;
    end;
  begin
  if FResetLogin then
    begin
    FResetLogin:=False;
    Sender.Session.Close;
    end;

  if (FDataFormat=fmt_RTC) and
     (FDataReqMode=req_contentBodyALL) and
     (EncryptionKey>0) then
    begin
    with Sender do
      begin
      crypt:=GetCrypt(Sender);
      code:=Request.Query.ValueCS['ACTION'];
      if (code='HELLO') or // Basic
         (code='HELO') or  // ISAAC
         (code='HELOB') or // RSA + Basic
         (code='HELOI') or // RSA + ISAAC
         (code='HELOR') then // RSA Max -> Sending HELLO to the Server
        begin
        if AutoSessionMode=rsm_Cookie then
          begin
          if Session.ID<>'' then
            Request.Cookie.Value[SessIDField]:=Session.ID
          else if AutoSessions then
            Request.Cookie.Value[SessIDField]:='NEW'
          else
            Request.Cookie.Value[SessIDField]:='';
          end
        else
          begin
          if Session.ID<>'' then
            Request.Query.Value[SessIDField]:=Session.ID
          else if AutoSessions then
            Request.Query.Value[SessIDField]:='NEW'
          else
            Request.Query.Value[SessIDField]:='';
          end;

        // Initialize encryption for this ClientModule
        NewCrypt(Sender);
        crypt:=GetCrypt(Sender);

        // Generate Client-Hello
        codeEx:=crypt.NewKey1(EncryptionKey);

        if SecureKey<>'' then
          crypt.SecureCode(SecureKey,codeEx);

        Request.Info.asBoolean[CLIMOD_ACTIVE]:=True;

        Request.Method:='POST';
        Request.FileName:=SlashName(FModuleFileName,'');
        // Send ClientHello + ControlKey
        WriteEx(codeEx);
        Exit;
        end
      else if (crypt=nil) or not crypt.HaveHello then
        begin
        MyRequest:=TRtcClientRequest.Create;
        SetupCallRequest(MyRequest);

      {$IFDEF RTC_RSA}
        case EncryptionMode of
          rem_Basic:if RSAKey=0 then MyRequest.Query.ValueCS['ACTION']:='HELLO'
                    else             MyRequest.Query.ValueCS['ACTION']:='HELOB';
          rem_Isaac:if RSAKey=0 then MyRequest.Query.ValueCS['ACTION']:='HELO'
                    else             MyRequest.Query.ValueCS['ACTION']:='HELOI';
          end;
      {$ELSE}
        MyRequest.Query.ValueCS['ACTION']:='HELLO';
      {$ENDIF}

        if ModuleHost<>'' then
          MyRequest.Host:=ModuleHost;

        DataReq:=TRtcDataRequestInfo.Create;
        DataReq.Request:=MyRequest;
        DataReq.Events:=Self;
        try
          InsertRequest(DataReq);
        except
          on E:Exception do
            if LOG_CLIENTMODULE_ERRORS then
              Log('TRtcClientModule.BeginRequest HELLO',E,'ERROR');
          end;
        Exit;
        end
      else if (Request.Query.ValueCS['ACTION']='START') then
        begin
        if AutoSessionMode=rsm_Cookie then
          begin
          if Session.ID<>'' then
            Request.Cookie.Value[SessIDField]:=Session.ID
          else if AutoSessions then
            Request.Cookie.Value[SessIDField]:='NEW'
          else
            Request.Cookie.Value[SessIDField]:='';
          end
        else
          begin
          if Session.ID<>'' then
            Request.Query.Value[SessIDField]:=Session.ID
          else if AutoSessions then
            Request.Query.Value[SessIDField]:='NEW'
          else
            Request.Query.Value[SessIDField]:='';
          end;

        // Generate Client-Key
        codeEx := crypt.NewKey2(EncryptionKey);

        Request.Info.asBoolean[CLIMOD_ACTIVE]:=True;

        Request.Method:='POST';
        Request.FileName:=SlashName(FModuleFileName,'');
        // Send ClientKey + ControlKey
        WriteEx( codeEx );
        Exit;
        end
      else if not crypt.HaveStart then
        begin
        MyRequest:=TRtcClientRequest.Create;
        SetupCallRequest(MyRequest);
        
        MyRequest.Query.ValueCS['ACTION']:='START';
        if ModuleHost<>'' then
          MyRequest.Host:=ModuleHost;

        DataReq:=TRtcDataRequestInfo.Create;
        DataReq.Request:=MyRequest;
        DataReq.Events:=Self;
        try
          InsertRequest(DataReq);
        except
          on E:Exception do
            if LOG_CLIENTMODULE_ERRORS then
              Log('TRtcClientModule.BeginRequest START',E,'ERROR');
          end;
        Exit;
        end;
      end;
    end;

  if FAutoLogin then
    if not assigned(FOnLogin) then
      raise Exception.Create('OnLogin event missing for ClientModule "'+String(ModuleFileName)+'", but AutoLogin is TRUE.')
    else if not Sender.Session.asBoolean[CLIMOD_LOGIN] and
            not Sender.Request.Info.asBoolean[CLIMOD_LOGIN] then
      begin
      LoginCall(FLoginResult,Sender,True);
      Exit;
      end;

  with Sender do
    if AutoSessionMode=rsm_Cookie then
      begin
      if Session.ID<>'' then
        Request.Cookie.Value[SessIDField]:=Session.ID
      else if AutoSessions then
        Request.Cookie.Value[SessIDField]:='NEW'
      else
        Request.Cookie.Value[SessIDField]:='';
      end
    else
      begin
      if Session.ID<>'' then
        Request.Query.Value[SessIDField]:=Session.ID
      else if AutoSessions then
        Request.Query.Value[SessIDField]:='NEW'
      else
        Request.Query.Value[SessIDField]:='';
      end;

  if assigned(Link) then
    Link.Call_BeginRequest(Sender)
  else if assigned(Client) then
    Client.CallBeginRequest;

  if not Sender.RequestInserted and
     not Sender.Request.Skipped and
     not Sender.Response.Rejected then
    begin
    if assigned(FOnBeginRequest) then
      if not AutoSyncEvents or not Sender.Sync(FOnBeginRequest) then
        FOnBeginRequest(Sender);

    if not Sender.RequestInserted and
       not Sender.Request.Skipped and
       not Sender.Response.Rejected then
      begin
      with Sender do
        begin
        MyCalls:=TRtcClientModuleCallsArray(Request.Info.asObj[CLIMOD_CALLS]);
        if not assigned(MyCalls) then
          raise Exception.Create('Internal error! ClientModule objects undefined!');

        HaveCleared:=0;
        for idx:=0 to MyCalls.EventCount-1 do
          if not MyCalls.isNull[idx] then
            if MyCalls.asObject[idx] is TRtcValue then
              begin
              val:=TRtcValue(MyCalls.asObject[idx]);
              if assigned(FOnPrepare) then
                begin
                if not AutoSyncEvents or not Sync(FOnPrepare,val,nil) then
                  FOnPrepare(Sender,val,nil);
                if val.isNull then
                  begin
                  MyCalls.Event[idx]:=nil;
                  Inc(HaveCleared);
                  end;
                end;
              if assigned(MyCalls.Event[idx]) then
                if assigned(MyCalls.Event[idx].PreparingCall) then
                  begin
                  if not AutoSyncEvents or not Sync(MyCalls.Event[idx].Call_Prepare,val,nil) then
                    MyCalls.Event[idx].Call_Prepare(Sender,val,nil);
                  if val.isNull then
                    begin
                    MyCalls.Event[idx]:=nil;
                    Inc(HaveCleared);
                    end;
                  end;
              end;

        if HaveCleared=MyCalls.EventCount then
          begin
          Request.Skip;
          Exit;
          end;

        if (FDataFormat=fmt_RTC) and
           (FDataReqMode=req_contentBodyALL) then
          begin
          crypt:=GetCrypt(Sender);
          if assigned(crypt) then
            if Request.Query.ValueCS['ACTION']='RESET' then
              crypt.Reset;
          Request.Info.asBoolean[CLIMOD_ACTIVE]:=True;
          end
        else
          crypt:=nil;

        compressed:=False;

        {$IFDEF COMPRESS}
        if (FDataFormat=fmt_RTC) and
           (FDataReqMode=req_contentBodyALL) and
           (FCompress<>cNone) then
          begin
          if MyCalls.EventCount=1 then
            codeEx:=MyCalls.asObject[0].toCodeEx
          else
            begin
            output:=TRtcHugeByteArray.Create;
            try
              for idx:=0 to MyCalls.EventCount-1 do
                output.AddEx(MyCalls.asObject[idx].toCodeEx);
              codeEx:= output.GetEx;
            finally
              RtcFreeAndNil(output);
              end;
            end;

          if isNewRequest then
            begin
            if Request.Method='' then
              Request.Method:='POST';
            Request.FileName:=SlashName(FModuleFileName,Request.FileName);
            end;

          if length(codeEx)<RTC_MIN_COMPRESS_SIZE then
            begin
            CryptWriteEx(crypt, codeEx);
            WriteEx(codeEx);
            end
          else
            begin
            { Using compression, need to compress all data now. }
            case FCompress of
              cFast: tempEx:=ZCompress_Ex(codeEx,zcFastest);
              cMax: tempEx:=ZCompress_Ex(codeEx,zcMax);
              else tempEx:=ZCompress_Ex(codeEx,zcDefault);
              end;
            // use compressed version ONLY if smaller than uncompressed
            if length(tempEx)<length(codeEx)-1 then
              begin
              SetLength(codeEx,0);
              CryptWriteEx(crypt, tempEx);
              WriteEx(tempEx);

              SetLength(tempEx,1);
              tempEx[0]:=0;
              CryptWriteEx(crypt, tempEx);
              WriteEx(tempEx);
              SetLength(tempEx,0);

              compressed:=True;
              end
            else
              begin
              SetLength(tempEx,0);
              CryptWriteEx(crypt, codeEx);
              WriteEx(codeEx);
              SetLength(codeEx,0);
              end;
            end;
          end
        else
        {$ENDIF}
          begin
          if (FDataReqMode=req_contentBodyALL) or (MyCalls.EventCount>1) then
            begin
            if isNewRequest then
              begin
              if Request.Method='' then
                Request.Method:='POST';
              Request.FileName:=SlashName(FModuleFileName,Request.FileName);
              if Request.Host='' then
                if FDataFormat<>fmt_RTC then
                  Request.Host:=ServerAddr;
              end;
          { req_contentBodyALL: -> used by default if NOT sending 1 method call.
              FunctionName and Parameters in the Content Body (default) }
            if FDataFormat in [fmt_RTC,fmt_XMLRPC] then
              begin
              for idx:=0 to MyCalls.EventCount-1 do
                begin
                if MyCalls.CheckType(idx,rtc_Function) then
                  MyCalls.asFunction[idx].reqVer:=1;
                case FDataFormat of
                  fmt_XMLRPC:
                    codeEx:=MyCalls.asObject[idx].toXMLrpcRequestEx;
                  else
                    begin
                    codeEx:=MyCalls.asObject[idx].toCodeEx;
                    if assigned(crypt) then
                      CryptWriteEx(crypt, codeEx);
                    end;
                  end;
                WriteEx(codeEx);
                end;
              end
            else // JSON
              begin
              for idx:=0 to MyCalls.EventCount-1 do
                begin
                if MyCalls.CheckType(idx,rtc_Function) then
                  begin
                  case FDataFormat of
                    fmt_JSONrpc1: MyCalls.asFunction[idx].reqVer:=1;
                    fmt_JSONrpc2: MyCalls.asFunction[idx].reqVer:=2;
                    else          MyCalls.asFunction[idx].reqVer:=3;
                    end;
                  if FDataFormat<>fmt_JSON then
                    MyCalls.asFunction[idx].reqID:=RtcWideString(IntToStr(idx+1));
                  end;
                end;
              if MyCalls.EventCount=1 then
                codeEx:=MyCalls.asObject[0].toJSONEx
              else
                codeEx:=MyCalls.toJSONEx;
              WriteEx(codeEx);
              end;
            end
          else if FDataReqMode=req_contentBodyParams then
            begin
          { req_contentBodyParams,
              FunctionName in the request 'FileName' (URI)
              with (mandatory) Parameters in the Content Body. }
            idx:=0;
            if MyCalls.CheckType(idx,rtc_Function) then
              begin
              MyCalls.asFunction[idx].reqVer:=0;
              if isNewRequest then
                Request.FileName:=SlashName(
                                    SlashName( FModuleFileREST,
                                               URL_Encode(Utf8Encode(MyCalls.asFunction[idx].FunctionName)) ),
                                            Request.FileName);
              end
            else if isNewRequest then
              Request.FileName:=SlashName(FModuleFileName,Request.FileName);

            case FDataFormat of
              fmt_RTC:
                begin
                codeEx:=MyCalls.asObject[idx].toCodeEx;
                if assigned(crypt) then
                  CryptWriteEx(crypt, codeEx);
                end;
              fmt_XMLRPC:
                codeEx:=MyCalls.asObject[idx].toXMLRESTRequestEx
              else
                codeEx:=MyCalls.asObject[idx].toJSONEx;
              end;

            if Request.Method='' then
              Request.Method:='POST';
            if Request.Host='' then
              if FDataFormat<>fmt_RTC then
                Request.Host:=ServerAddr;

            WriteEx(codeEx);
            end
          else if FDataReqMode=req_contentBodyOptional then
            begin
          { req_contentBodyParams,
              FunctionName in the request 'FileName' (URI)
              with (optional) Parameters in the Content Body. }
            idx:=0;
            if MyCalls.CheckType(idx,rtc_Function) then
              begin
              MyCalls.asFunction[idx].reqVer:=0;
              if isNewRequest then
                Request.FileName:=SlashName(
                                    SlashName(FModuleFileREST,
                                              URL_Encode(Utf8Encode(MyCalls.asFunction[idx].FunctionName)) ),
                                            Request.FileName);
              if MyCalls.asFunction[idx].Count=0 then
                idx:=1; // no parameters
              end
            else
              begin
              if MyCalls.CheckType(idx,rtc_Null) then
                idx:=1; // no parameters
              if isNewRequest then
                Request.FileName:=SlashName(FModuleFileName,Request.FileName);
              end;

            if Request.Host='' then
              if FDataFormat<>fmt_RTC then
                Request.Host:=ServerAddr;

            if idx=0 then
              begin
              case FDataFormat of
                fmt_RTC:
                  begin
                  codeEx:=MyCalls.asObject[idx].toCodeEx;
                  if assigned(crypt) then
                    CryptWriteEx(crypt, codeEx);
                  end;
                fmt_XMLRPC:
                  codeEx:=MyCalls.asObject[idx].toXMLRESTRequestEx
                else
                  codeEx:=MyCalls.asObject[idx].toJSONEx;
                end;
              if Request.Method='' then
                Request.Method:='POST';
              WriteEx(codeEx);
              end
            else
              begin
              if Request.Method='' then
                Request.Method:='GET';
              Write;
              end;
            end
          else // all paremters in the URI
            begin
            idx:=0;
            if MyCalls.CheckType(idx,rtc_Function) then
              begin
              if Request.Query.Text<>'' then
                code:='?'+Request.Query.Text
              else
                code:='';
              MyCalls.asFunction[idx].reqVer:=0;

              case FDataReqMode of
                req_directJSON:
                { FunctionName in the request 'FileName' (URI) directly followed by
                  all parameters (no '/' separator), URL-Encoded and serialized as
                  a single JSON object or a JSON array (stored in "params" array). }
                  if MyCalls.asFunction[idx].Count>0 then // have parameters
                    if (MyCalls.asFunction[idx].Count=1) and // "params" array
                       (MyCalls.asFunction[idx].CheckType('params',rtc_Array)) then
                      code:=URL_Encode(Utf8Encode(MyCalls.asFunction[idx].asJSON['params']))+code
                    else
                      code:=URL_Encode(Utf8Encode(MyCalls.asJSON[idx]))+code;

                req_queryJSON:
                { FunctionName in the Request 'FileName' (URI) with all Parameters
                  in the request Query (URI, after '?') as a single JSON object or array. }
                  if MyCalls.asFunction[idx].Count>0 then // have parameters
                    begin
                    if code='' then code:='?'
                    else code:=code+'&';
                    if (MyCalls.asFunction[idx].Count=1) and // "params" array
                       (MyCalls.asFunction[idx].CheckType('params',rtc_Array)) then
                      code:=code+URL_Encode(Utf8Encode(MyCalls.asFunction[idx].asJSON['params']))
                    else
                      code:=code+URL_Encode(Utf8Encode(MyCalls.asJSON[idx]));
                    end;

                req_queryNameJSON:
                { FunctionName in the Request 'FileName' (URI) with each
                  parameter in the request Query (URI, after '?') as 'Name=JSON' pair. }
                  for i:=0 to MyCalls.asFunction[idx].Count-1 do
                    begin
                    if code='' then code:='?'
                    else code:=code+'&';
                    fname:=MyCalls.asFunction[idx].FieldName[i];
                    code:=code+URL_Encode(Utf8Encode(fname))+
                          '='+URL_Encode(Utf8Encode(MyCalls.asFunction[idx].asJSON[fname]));
                    end;

                req_queryNameText:
                { FunctionName in the Request 'FileName' (URI) with each
                  parameter in the request Query (URI, after '?') as 'Name=Text' pair. }
                  for i:=0 to MyCalls.asFunction[idx].Count-1 do
                    begin
                    if code='' then code:='?'
                    else code:=code+'&';
                    fname:=MyCalls.asFunction[idx].FieldName[i];
                    code:=code+URL_Encode(Utf8Encode(fname))+
                          '='+URL_Encode(Utf8Encode(MyCalls.asFunction[idx].asText[fname]));
                    end;

                req_uriParamsJSON:
                { FunctionName in the request 'FileName' (URI) with all parameters
                  separated by '/' using the JSON format for every parameter (URL_Encoded).
                  Parameters stored in the "params" array, starting from 0. }
                  if MyCalls.asFunction[idx].Count>0 then
                    begin
                    if (MyCalls.asFunction[idx].Count=1) and // "params" array
                       (MyCalls.asFunction[idx].CheckType('params',rtc_Array)) then
                      begin
                      code:='';
                      for i:=0 to MyCalls.asFunction[idx].asArray['params'].Count-1 do
                        code:=code+'/'+
                              URL_Encode(Utf8Encode(MyCalls.asFunction[idx].asArray['params'].asJSON[i]));
                      end
                    else
                      code:='/'+URL_Encode(Utf8Encode(MyCalls.asJSON[idx]));
                    if Request.Query.Text<>'' then
                      code:=code+'?'+Request.Query.Text;
                    end;

                req_uriParamsText:
                { FunctionName in the request 'FileName' (URI) with each parameter
                  separated by '/' and stored as a string in basic URL encoding/decoding,
                  stored in the "params" array, starting from 0 (default for REST). }
                  if MyCalls.asFunction[idx].Count>0 then
                    begin
                    if (MyCalls.asFunction[idx].Count=1) and // "params" array
                       (MyCalls.asFunction[idx].CheckType('params',rtc_Array)) then
                      begin
                      code:='';
                      for i:=0 to MyCalls.asFunction[idx].asArray['params'].Count-1 do
                        code:=code+'/'+
                              URL_Encode(Utf8Encode(MyCalls.asFunction[idx].asArray['params'].asText[i]));
                      end
                    else // no "params" array, send each named parameter as text
                      begin
                      code:='';
                      for i:=0 to MyCalls.asFunction[idx].Count-1 do
                        code:=code+'/'+
                              URL_Encode(Utf8Encode(
                                MyCalls.asFunction[idx].
                                          asText[ MyCalls.asFunction[idx].FieldName[i] ] ));
                      end;
                    if Request.Query.Text<>'' then
                      code:=code+'?'+Request.Query.Text;
                    end;

                else
                  raise Exception.Create('Selected combination of "DataFormat" and "DataReqMode" is not supported.');

                end;

              if isNewRequest then
                Request.URI:= SlashName(
                                SlashName(FModuleFileREST,
                                          URL_Encode(Utf8Encode(MyCalls.asFunction[idx].FunctionName)) ),
                                       Request.FileName) +
                                  code;
              code:='';
              end
            else if MyCalls.CheckType(idx,rtc_Record) then
              begin
              if Request.Query.Text<>'' then
                code:='?'+Request.Query.Text
              else
                code:='';

              case FDataReqMode of
                req_directJSON:
                { FunctionName in the request 'FileName' (URI) directly followed by
                  all parameters (no '/' separator), URL-Encoded and serialized as
                  a single JSON object or a JSON array (stored in "params" array). }
                  if MyCalls.asRecord[idx].Count>0 then // have parameters
                    if (MyCalls.asRecord[idx].Count=1) and // "params" array
                       (MyCalls.asRecord[idx].CheckType('params',rtc_Array)) then
                      code:=URL_Encode(Utf8Encode(MyCalls.asRecord[idx].asJSON['params']))+code
                    else
                      code:=URL_Encode(Utf8Encode(MyCalls.asJSON[idx]))+code;

                req_queryJSON:
                { FunctionName in the Request 'FileName' (URI) with all Parameters
                  in the request Query (URI, after '?') as a single JSON object or array. }
                  if MyCalls.asRecord[idx].Count>0 then // have parameters
                    begin
                    if code='' then code:='?'
                    else code:=code+'&';
                    if (MyCalls.asRecord[idx].Count=1) and // "params" array
                       (MyCalls.asRecord[idx].CheckType('params',rtc_Array)) then
                      code:=code+URL_Encode(Utf8Encode(MyCalls.asRecord[idx].asJSON['params']))
                    else
                      code:=code+URL_Encode(Utf8Encode(MyCalls.asJSON[idx]));
                    end;

                req_queryNameJSON:
                { FunctionName in the Request 'FileName' (URI) with each
                  parameter in the request Query (URI, after '?') as 'Name=JSON' pair. }
                  for i:=0 to MyCalls.asRecord[idx].Count-1 do
                    begin
                    if code='' then code:='?'
                    else code:=code+'&';
                    fname:=MyCalls.asRecord[idx].FieldName[i];
                    code:=code+URL_Encode(Utf8Encode(fname))+
                          '='+URL_Encode(Utf8Encode(MyCalls.asRecord[idx].asJSON[fname]));
                    end;

                req_queryNameText:
                { FunctionName in the Request 'FileName' (URI) with each
                  parameter in the request Query (URI, after '?') as 'Name=Text' pair. }
                  for i:=0 to MyCalls.asRecord[idx].Count-1 do
                    begin
                    if code='' then code:='?'
                    else code:=code+'&';
                    fname:=MyCalls.asRecord[idx].FieldName[i];
                    code:=code+URL_Encode(Utf8Encode(fname))+
                          '='+URL_Encode(Utf8Encode(MyCalls.asRecord[idx].asText[fname]));
                    end;

                req_uriParamsJSON:
                { FunctionName in the request 'FileName' (URI) with all parameters
                  separated by '/' using the JSON format for every parameter (URL_Encoded).
                  Parameters stored in the "params" array, starting from 0. }
                  if MyCalls.asRecord[idx].Count>0 then
                    begin
                    if (MyCalls.asRecord[idx].Count=1) and // "params" array
                       (MyCalls.asRecord[idx].CheckType('params',rtc_Array)) then
                      begin
                      code:='';
                      for i:=0 to MyCalls.asRecord[idx].asArray['params'].Count-1 do
                        code:=code+'/'+
                              URL_Encode(Utf8Encode(MyCalls.asRecord[idx].asArray['params'].asJSON[i]));
                      end
                    else
                      code:='/'+URL_Encode(Utf8Encode(MyCalls.asJSON[idx]));
                    if Request.Query.Text<>'' then
                      code:=code+'?'+Request.Query.Text;
                    end;

                req_uriParamsText:
                { FunctionName in the request 'FileName' (URI) with each parameter
                  separated by '/' and stored as a string in basic URL encoding/decoding,
                  stored in the "params" array, starting from 0 (default for REST). }
                  if MyCalls.asRecord[idx].Count>0 then
                    begin
                    if (MyCalls.asRecord[idx].Count=1) and // "params" array
                       (MyCalls.asRecord[idx].CheckType('params',rtc_Array)) then
                      begin
                      code:='';
                      for i:=0 to MyCalls.asRecord[idx].asArray['params'].Count-1 do
                        code:=code+'/'+
                              URL_Encode(Utf8Encode(MyCalls.asRecord[idx].asArray['params'].asText[i]));
                      end
                    else
                      code:='/'+URL_Encode(Utf8Encode(MyCalls.asText[idx]));
                    if Request.Query.Text<>'' then
                      code:=code+'?'+Request.Query.Text;
                    end;

                else
                  raise Exception.Create('Selected combination of "DataFormat" and "DataReqMode" is not supported.');

                end;

              if isNewRequest then
                Request.URI:=SlashName(FModuleFileName,Request.FileName) +
                             code;
              code:='';
              end
            else if MyCalls.CheckType(idx,rtc_Array) then
              begin
              if Request.Query.Text<>'' then
                code:='?'+Request.Query.Text
              else
                code:='';

              case FDataReqMode of
                req_directJSON:
                { FunctionName in the request 'FileName' (URI) directly followed by
                  all parameters (no '/' separator), URL-Encoded and serialized as
                  a single JSON object or a JSON array (stored in "params" array). }
                  if MyCalls.asArray[idx].Count>0 then // have parameters
                    code:=URL_Encode(Utf8Encode(MyCalls.asJSON[idx]))+code;

                req_queryJSON:
                { FunctionName in the Request 'FileName' (URI) with all Parameters
                  in the request Query (URI, after '?') as a single JSON object or array. }
                  if MyCalls.asArray[idx].Count>0 then // have parameters
                    begin
                    if code='' then code:='?'
                    else code:=code+'&';
                    code:=code+URL_Encode(Utf8Encode(MyCalls.asJSON[idx]));
                    end;

                req_queryNameJSON:
                { FunctionName in the Request 'FileName' (URI) with each
                  parameter in the request Query (URI, after '?') as 'Name=JSON' pair. }
                  if MyCalls.asArray[idx].Count>0 then // have parameters
                    begin
                    if code='' then code:='?'
                    else code:=code+'&';
                    code:=code+'params='+URL_Encode(Utf8Encode(MyCalls.asJSON[idx]));
                    end;

                req_queryNameText:
                { FunctionName in the Request 'FileName' (URI) with each
                  parameter in the request Query (URI, after '?') as 'Name=Text' pair. }
                  for i:=0 to MyCalls.asArray[idx].Count-1 do
                    begin
                    if code='' then code:='?'
                    else code:=code+'&';
                    code:=code+'param'+Int2Str(i)+'='+URL_Encode(Utf8Encode(MyCalls.asArray[idx].asText[i]));
                    end;

                req_uriParamsJSON:
                { FunctionName in the request 'FileName' (URI) with all parameters
                  separated by '/' using the JSON format for every parameter (URL_Encoded).
                  Parameters stored in the "params" array, starting from 0. }
                  if MyCalls.asArray[idx].Count>0 then
                    begin
                    code:='';
                    for i:=0 to MyCalls.asArray[idx].Count-1 do
                      code:=code+'/'+
                            URL_Encode(Utf8Encode(MyCalls.asArray[idx].asJSON[i]));
                    if Request.Query.Text<>'' then
                      code:=code+'?'+Request.Query.Text;
                    end;

                req_uriParamsText:
                { FunctionName in the request 'FileName' (URI) with each parameter
                  separated by '/' and stored as a string in basic URL encoding/decoding,
                  stored in the "params" array, starting from 0 (default for REST). }
                  if MyCalls.asArray[idx].Count>0 then
                    begin
                    code:='';
                    for i:=0 to MyCalls.asArray[idx].Count-1 do
                      code:=code+'/'+
                            URL_Encode(Utf8Encode(MyCalls.asArray[idx].asText[i]));
                    if Request.Query.Text<>'' then
                      code:=code+'?'+Request.Query.Text;
                    end;

                else
                  raise Exception.Create('Selected combination of "DataFormat" and "DataReqMode" is not supported.');

                end;

              if isNewRequest then
                Request.URI:=SlashName(FModuleFileName,Request.FileName) +
                             code;
              code:='';
              end
            else
              begin
              code:='';
              if isNewRequest then
                Request.FileName:=SlashName(FModuleFileName,Request.FileName);
              end;

            if Request.Method='' then
              if length(code)=0 then
                Request.Method:='GET'
              else
                Request.Method:='POST';
            if FDataFormat<>fmt_RTC then
              if Request.Host='' then
                Request.Host:=ServerAddr;

            Write(code);
            end;
          end;

        if (FDataFormat=fmt_RTC) and
           (FDataReqMode=req_contentBodyALL) then
          begin
          if assigned(crypt) and crypt.CanWrite then
            begin
            { Add random control number at the end of the request,
              so we can check if the response is correctly encrypted. }
            crypt.ControlKey := GenerateControlKey(crypt.ControlCounter);

            if not compressed then
              codeEx:=RtcStringToBytes(#13+crypt.ControlKey)
            else // #0 was allready added
              codeEx:=RtcStringToBytes(crypt.ControlKey); // we just need to add the control number

            CryptWriteEx(crypt, codeEx);

            if Request.Method='' then
              Request.Method:='POST';
            WriteEx(codeEx);
            end;
          end;

        SetLength(codeEx,0);
        SetLength(code,0);
        end;
      end;
    end;
  end;

function TRtcClientModule.GetObjectManager(xCreate:boolean=False): TRtcRemoteObjectManager;
  begin
  if not assigned(FObjectManager) and xCreate then
    begin
    FObjectManager:=TRtcClientObjectManager.Create(False);
    FObjectManager.BroadcastGroup:=RtcWideString(FObjManSesName);
    FObjectManager.OnDataReady:=DoObjectManagerDataReady;
    end;
  Result:=FObjectManager;
  end;

procedure TRtcClientModule.ActivateObjectManager(xCreate:boolean=True);
  begin
  if FRelease then Exit;

  if ObjectLinks=ol_None then
    raise ERtcObjectLinks.Create('ActivateObjectManager: ObjectLinks = ol_None')
  else if not assigned(FObjectManager) then
    if xCreate then
      begin
      FObjectManager:=TRtcClientObjectManager.Create(False);
      FObjectManager.BroadcastGroup:=RtcWideString(FObjManSesName);
      FObjectManager.OnDataReady:=DoObjectManagerDataReady;
      end;

  if not assigned(FObjectManager) then
    raise ERtcObjectLinks.Create('ActivateObjectManager failed')
  else
    begin
    SetRtcObjectManager(FObjectManager);
    FObjectManager.ExecuteBroadcast(nil);
    end;
  end;

procedure TRtcClientModule.RemoveObjectManager;
  begin
  if assigned(FObjectManager) then
    begin
    FObjectManager.OnDataReady:=nil;
    if CheckRtcObjectManager=FObjectManager then
      SetRtcObjectManager(nil);
    RtcFreeAndNil(FObjectManager);
    end;
  end;

procedure TRtcClientModule.RemoveManagedObjects;
  begin
  if assigned(FObjectManager) then
    FObjectManager.FreeObjects;
  end;

procedure TRtcClientModule.DoExecute(Sender: TRtcConnection; var MyData: TRtcValueObject);
  var
    MyResult,TmpData,TmpRes:TRtcValueObject;
    TmpMan:TRtcObjectManager;
    ObjMan:TRtcClientObjectManager;
    TmpV:TRtcValue;
    xData:TRtcValue;
  begin
  if FObjectLinkSupport=ol_None then
    begin
    if assigned(FFunctions) then
      begin
      MyResult:=FFunctions.ExecuteData(Sender, MyData);
      if MyData<>MyResult then
        begin
        RtcFreeAndNil(MyData);
        MyData:=MyResult;
        end;
      end;
    end
  else if (TRtcValue(MyData).isType=rtc_Function) and
          (TRtcValue(MyData).asFunction.FunctionName=RTCL_FUNCTION) then
    begin
    TmpData:=TRtcValue(MyData).asFunction.asObject[RTCL_DATA];
    MyResult:=TRtcValue(MyData).asFunction.asObject[RTCL_RESULT];
    if MyResult=nil then
      MyResult:=TRtcValue.Create;
    TRtcValue(MyData).asFunction.asObject[RTCL_RESULT]:=nil;
    try
      if assigned(TmpData) then
        begin
        ObjMan:=TRtcClientObjectManager(FObjectManager);
        if not assigned(ObjMan) then
          if FObjectLinkSupport in [ol_AutoServer,ol_AutoBoth] then
            begin
            FObjectManager:=TRtcClientObjectManager.Create(False);
            FObjectManager.BroadcastGroup:=RtcWideString(FObjManSesName);
            FObjectManager.OnDataReady:=DoObjectManagerDataReady;
            ObjMan:=FObjectManager;
            end
          else
            Exit;
        TmpMan:=SetRtcObjectManager(ObjMan);
        try
          if assigned(FOnObjectDataIn) then
            begin
            xData:=TRtcValue.Create;
            try
              xData.asObject:=TmpData;
              TRtcValue(MyData).asFunction.asObject[RTCL_DATA]:=nil;
              FOnObjectDataIn(Sender,xData);
              if assigned(FOnObjectCreate) then
                begin
                ObjMan.OnObjectCreate:=DoObjectCreate;
                try
                  TmpRes:=ObjMan.ExecuteWithBroadcast(Sender,xData);
                finally
                  ObjMan.OnObjectCreate:=nil;
                  end;
                end
              else
                TmpRes:=ObjMan.ExecuteWithBroadcast(Sender,xData);
            finally
              xData.Free;
              end;
            end
          else
            begin
            if assigned(FOnObjectCreate) then
              begin
              ObjMan.OnObjectCreate:=DoObjectCreate;
              try
                TmpRes:=ObjMan.ExecuteWithBroadcast(Sender,TmpData);
              finally
                ObjMan.OnObjectCreate:=nil;
                end;
              end
            else
              TmpRes:=ObjMan.ExecuteWithBroadcast(Sender,TmpData);
            end;
          if assigned(TmpRes) then
            begin
            TmpV:=TRtcValue.Create;
            try
              TmpV.asObject:=TmpRes;
              Call_ObjectLinkErrors(Sender,TRtcValue(MyData),TmpV);
            finally
              TmpV.Free;
              end;
            end;
        finally
          SetRtcObjectManager(TmpMan);
          end;
        end;
    finally
      RtcFreeAndNil(MyData);
      MyData:=MyResult;
      end;

    if assigned(FFunctions) then
      if not isSimpleValue(MyData) then
        begin
        MyResult:=FFunctions.ExecuteData(Sender, MyData);
        if MyData<>MyResult then
          begin
          RtcFreeAndNil(MyData);
          MyData:=MyResult;
          end;
        end;
    end
  else if assigned(FFunctions) then
    begin
    MyResult:=FFunctions.ExecuteData(Sender, MyData);
    if MyData<>MyResult then
      begin
      RtcFreeAndNil(MyData);
      MyData:=MyResult;
      end;
    end;
  end;

procedure TRtcClientModule.Call_DataReceived(Sender: TRtcConnection);
  begin
  if assigned(Link) then
    Link.Call_PeekReceived(Sender);

  if Sender.Response.Done then
    if not AutoSyncEvents or not Sender.Sync(ProcessDataReceived) then
      ProcessDataReceived(Sender);
  end;

procedure TRtcClientModule.ProcessDataReceived(Sender: TRtcConnection);
  var
    idx,ida:integer;
    code:RtcString;
    codeEx:RtcByteArray;
    at,atLast:integer;
    MyTemp:TRtcValue;
    crypt:TRtcCryptClient;
    c1,c2,c4:RtcString;
    c3:integer;
    MyArr:TRtcArray;
    MyData:TRtcValueObject;
    MyCalls:TRtcClientModuleCallsArray;
    rVer:byte;
    closingSession:RtcString;
  begin
  with Sender do
    begin
    code:=Request.Query.ValueCS['ACTION'];
    if (FDataFormat=fmt_RTC) and (Response.StatusCode<>200) then
      begin
      if Response.StatusCode=410 then // Status 410 = Gone: Session ID invalid, clear local Session info.
        Call_SessionExpired(Sender)
      else if Response.StatusCode=412 then // Status 412 = Precondition Failed: Encryption required
        Call_NeedEncryption(Sender)
      else if Response.StatusCode=409 then // Status 409 = Conflict: Wrong Encryption Key
        Call_WrongEncryption(Sender)
      else if Response.StatusCode=417 then // Status 417 = Expectation Failed: Encryption Key initialization error
        Call_WrongEncryptionStart(Sender)
      else // Accept only responses with status 200 OK.
        Call_WrongResponse(Sender);
      end
    else if (FDataFormat=fmt_RTC) and
            (EncryptionKey>0) and
            ( (code='HELLO') or
              (code='HELO') or
              (code='HELOB') or
              (code='HELOI') or
              (code='HELOR') ) then
      begin
      // Prepare Session
      if (Session.ID='') then
        begin
        if (Response.Cookie.ValueCS[SessIDFieldCS]='') then
          begin
          if ForceEncryption then
            begin
            Call_WrongResponse(Sender);
            Exit;
            end
          else
            begin
            NewCrypt(Sender);
            crypt:=GetCrypt(Sender);
            end;
          end
        else
          begin
          crypt:=GetCrypt(Sender);
          c1:=crypt.ClientHello;
          c2:=crypt.ControlKey;
          c3:=crypt.ControlCounter;
          c4:=crypt.ClientKey;
          Session.Open(Response.Cookie.ValueCS[SessIDFieldCS]); // Set new Session ID
          NewCrypt(Sender);
          crypt:=GetCrypt(Sender);
          crypt.ClientHello:=c1;
          crypt.ControlKey:=c2;
          crypt.ControlCounter:=c3;
          crypt.ClientKey:=c4;
          end;
        end
      else if (Response.Cookie.ValueCS[SessIDFieldCS]<>'') and
              (Session.ID<>Response.Cookie.ValueCS[SessIDFieldCS]) then
        begin
        crypt:=GetCrypt(Sender);
        c1:=crypt.ClientHello;
        c2:=crypt.ControlKey;
        c3:=crypt.ControlCounter;
        c4:=crypt.ClientKey;
        Session.Open(Response.Cookie.ValueCS[SessIDFieldCS]); // Set new Session ID
        NewCrypt(Sender);
        crypt:=GetCrypt(Sender);
        crypt.ClientHello:=c1;
        crypt.ControlKey:=c2;
        crypt.ControlCounter:=c3;
        crypt.ClientKey:=c4;
        end
      else
        crypt:=GetCrypt(Sender);

      codeEx:=ReadEx;
      crypt.HaveHello:=True;

      if length(codeEx)=0 then // Server does not support encryption
        begin
        if ForceEncryption then
          begin
          Call_NoEncryption(Sender);
          Exit;
          end
        else
          begin
          crypt.Init(EncryptionMode,RSAKey);
          crypt.HaveHello:=True;
          crypt.HaveStart:=True;
          end;
        end
      else if length(codeEx)<=length(crypt.ControlKey) then // Wrong response from server
        begin
        Call_WrongEncryptionStart(Sender);
        Exit;
        end
      else
        begin
        // Prepare the Encryption object for Reading
        crypt.SetupRead1(codeEx);

        // Check if response ends with sent control key
        if RtcBytesToString(codeEx, length(codeEx)-length(crypt.ControlKey), length(crypt.ControlKey))
            <> crypt.ControlKey then
          begin
          Call_WrongEncryptionStart(Sender);
          Exit;
          end;

        crypt.ServerHello:= RtcBytesToString(codeEx,0,length(codeEx)-length(crypt.ControlKey));

        // Prepare the Encryption object for Writing
        crypt.SetupWrite1;
        end;
      end
    else if (FDataFormat=fmt_RTC) and
            (EncryptionKey>0) and
            (code='START') then
      begin
      crypt:=GetCrypt(Sender);

      codeEx:=ReadEx;
      crypt.HaveStart:=True;

      if length(codeEx)=0 then // Server canceled encryption
        begin
        if ForceEncryption then
          begin
          Call_NoEncryption(Sender);
          Exit;
          end
        else
          begin
          crypt.Init(EncryptionMode,RSAKey);
          crypt.HaveHello:=True;
          crypt.HaveStart:=True;
          end;
        end
      else if length(codeEx)<=length(crypt.ControlKey) then // Wrong response from server
        begin
        Call_WrongEncryptionStart(Sender);
        Exit;
        end
      else
        begin
        // Set new Reading Key
        crypt.SetupRead2(codeEx);

        // Check if response ends with sent Control Key
        if RtcBytesToString(codeEx, length(codeEx)-length(crypt.ControlKey), length(crypt.ControlKey))
            <> crypt.ControlKey then
          begin
          Call_WrongEncryptionStart(Sender);
          Exit;
          end;

        crypt.ServerKey:= RtcBytesToString(codeEx, 0, length(codeEx)-length(crypt.ControlKey));

        // Set new Writing Key
        crypt.SetupWrite2;

        crypt.AllReady:=True;
        end;
      end
    else
      begin
      closingSession:='';
      try
        if Response.Cookie.ValueCS[SessIDFieldCS]<>'' then
          if AutoSessionMode=rsm_Cookie then
            begin
            if Response.Cookie.ValueCS[SessIDFieldCS] = '-'+Request.Cookie.ValueCS[SessIDFieldCS] then
              closingSession:=Session.ID // Received "Session Closing" info, close local session
            else if Request.Cookie.ValueCS[SessIDFieldCS] = 'NEW' then
              Session.Open(Response.Cookie.ValueCS[SessIDFieldCS]) // we have requested a new Session ID
            else if Session.ID<>Response.Cookie.ValueCS[SessIDFieldCS] then // we have received a different Session ID
              Session.Open(Response.Cookie.ValueCS[SessIDFieldCS]);
            end
          else
            begin
            if Response.Cookie.ValueCS[SessIDFieldCS] = '-'+Request.Query.ValueCS[SessIDFieldCS] then
              closingSession:=Session.ID // Received "Session Closing" info, close local session
            else if Request.Query.ValueCS[SessIDFieldCS]='NEW' then
              Session.Open(Response.Cookie.ValueCS[SessIDFieldCS]) // we have requested a new Session ID
            else if Session.ID<>Response.Cookie.ValueCS[SessIDFieldCS] then // we have received a different Session ID
              Session.Open(Response.Cookie.ValueCS[SessIDFieldCS]);
            end;

        MyCalls:=TRtcClientModuleCallsArray(Request.Info.asObj[CLIMOD_CALLS]);
        if not assigned(MyCalls) then
          raise Exception.Create('Internal error! ClientModule objects undefined!');

        if FDataFormat=fmt_RTC then
          begin
          codeEx:=ReadEx;

          crypt:=GetCrypt(Sender);
          if assigned(crypt) and crypt.CanRead then
            begin
            if length(codeEx)=0 then
              begin
              Call_WrongEncryption(Sender);
              Exit;
              end
            else if length(codeEx)<=length(crypt.ControlKey) then // Wrong response from server
              begin
              Call_WrongEncryption(Sender);
              Exit;
              end
            else
              begin
              CryptReadEx(crypt, codeEx);
              // Response has to END with our ControlKey
              if RtcBytesToString(codeEx, length(codeEx)-length(crypt.ControlKey), length(crypt.ControlKey))
                  <> crypt.ControlKey then
                begin
                Call_WrongEncryption(Sender);
                Exit;
                end
              else
                begin
                {$IFDEF COMPRESS}
                // There is #0 before the Control Key, data is compressed
                if codeEx[length(codeEx)-length(crypt.ControlKey)-1]=0 then
                  begin
                  try
                    codeEx:=ZDecompress_Ex(codeEx, length(codeEx)-length(crypt.ControlKey)-1);
                  except
                    on E:Exception do
                      begin
                      if LOG_CLIENTMODULE_ERRORS then
                        Log('TRtcClientModule.DataReceived DECOMPRESS-1',E,'ERROR');
                      Call_WrongResponse(Sender);
                      Exit;
                      end;
                    end;
                  end
                else
                  SetLength(codeEx, length(codeEx)-length(crypt.ControlKey));
                {$ELSE}
                // There is #0 before the Control Key, data is compressed
                if codeEx[length(codeEx)-length(crypt.ControlKey)-1]=0 then
                  begin
                  Call_WrongResponse(Sender);
                  Exit;
                  end
                else
                  SetLength(codeEx, length(codeEx)-length(crypt.ControlKey));
                {$ENDIF}
                end;
              end;
            end
          else if ForceEncryption and (EncryptionKey>0) then
            begin
            Call_NoEncryption(Sender);
            Exit;
            end
          else if codeEx[length(codeEx)-1]=0 then // compressed data
            begin
            {$IFDEF COMPRESS}
            try
              codeEx:=ZDecompress_Ex(codeEx, length(codeEx)-1);
            except
              on E:Exception do
                begin
                if LOG_CLIENTMODULE_ERRORS then
                  Log('TRtcClientModule.DataReceived DECOMPRESS-2',E,'ERROR');
                Call_WrongResponse(Sender);
                Exit;
                end;
              end;
            {$ELSE}
            Call_WrongResponse(Sender);
            Exit;
            {$ENDIF}
            end;

          if assigned(FOnPeekResponse) then
            begin
            Sender.PokeEx(codeEx);
            SetLength(codeEx,0);
            FOnPeekResponse(Sender);
            code:=Sender.Read;
            end
          else
            begin
            code:=RtcBytesToString(codeEx);
            SetLength(codeEx,0);
            end;
          end
        else
          begin
          if assigned(FOnPeekResponse) then
            FOnPeekResponse(Sender);
          code:=Read;
          end;

      finally
        if closingSession<>'' then
          begin
          if closingSession=Session.ID then
            Session.Close;
          closingSession:='';
          end;
        end;

      idx:=0;
      ida:=0;
      at:=0;
      MyData:=nil;
      MyArr:=nil;
      try
        if length(code)=0 then
          begin
          if Response.StatusCode<>200 then
            Call_WrongResponse(Sender)
          else if MyCalls.EventCount>0 then
            raise Exception.Create('Response missing Result(s).');
          end
        else
          begin
          // Loop through all responses
          atLast:=at-1;
          while (at>atLast) and (at<length(code)) and (idx<MyCalls.EventCount) do
            begin
            atLast:=at;

            // Convert result to RTC objects
            case FDataFormat of
              fmt_RTC:    MyData:=TRtcValueResult.FromCode(code,at);
              fmt_XMLRPC: MyData:=TRtcValueResult.FromXMLrpc(code,at);
              else
                begin
                rVer:=0;
                case FDataFormat of
                  fmt_JSONrpc1: rVer:=3; // JSON-RPC 1.0 or 2.0 format
                  fmt_JSONrpc2: rVer:=3; // JSON-RPC 1.0 or 2.0 format
                  fmt_JSON:     rVer:=0; // plain JSON
                  end;
                MyData:=TRtcValueResult.FromJSONrpc(rVer,Utf8Decode(code),at);
                end;
              end;

            if (FDataFormat>fmt_XMLRPC) and // calls sent using JSON
               (MyCalls.EventCount>idx+1) and // multiple calls waiting for a Result
               TRtcValueResult(MyData).CheckType(rtc_Array) then // received Array
              begin
              ida:=idx;
              MyArr:=TRtcValueResult(MyData).asArray;
              TRtcValueResult(MyData).Extract;
              RtcFreeAndNil(MyData);
              if idx-ida<MyArr.Count then
                begin
                MyData:=MyArr.asObject[idx-ida];
                MyArr.asObject[idx-ida]:=nil;
                end
              else
                RtcFreeAndNil(MyArr);
              end
            else
              MyArr:=nil;

            repeat
              try
                // Execute local remote functions if result contains them
                if not isSimpleValue(MyData) then
                  DoExecute(Sender,MyData);
                if idx<MyCalls.EventCount then
                  begin
                  if not assigned(MyData) then
                    MyData:=TRtcValue.Create;
                  if assigned(MyCalls.Event[idx]) then
                    begin
                    if not (MyData is TRtcValue) then
                      begin
                      MyTemp:=TRtcValue.Create;
                      MyTemp.asObject:=MyData;
                      MyData:=MyTemp;
                      end;
                    try
                      if assigned(FOnResultReady) then
                        FOnResultReady(Sender,
                                       TRtcValue(MyCalls.asObject[idx]),
                                       TRtcValue(MyData));
                      MyCalls.Event[idx].
                          Call_Return(Sender,
                                      TRtcValue(MyCalls.asObject[idx]),
                                      TRtcValue(MyData));
                    except
                      on E:EPostInteractive do
                        begin
                        PostInteractiveResult(MyCalls.Event[idx],
                                              TRtcValue(MyCalls.asObject[idx]),
                                              TRtcValue(MyData));
                        MyCalls.Event[idx]:=nil;
                        MyCalls.asObject[idx]:=nil;
                        MyData:=nil;
                        end;
                      end;
                    end;
                  end
                else if not (MyData is TRtcValue) then
                  raise Exception.Create('More Results received than Calls sent.')
                else if not TRtcValue(MyData).isNull then
                  raise Exception.Create('More Results received than Calls sent.');
              except
                on E:Exception do
                  begin
                  try
                    if LOG_CLIENTMODULE_ERRORS then
                      Log('TRtcClientModule.DataReceived RESULT',E,'ERROR');
                    Call_ResultError(Sender,
                                     TRtcValue(MyCalls.asObject[idx]),
                                     TRtcValue(MyData), E);
                  except
                    // ignore user exceptions
                    end;
                  end;
                end;

              Inc(idx);
              if assigned(MyData) then
                begin
                try
                  RtcFreeAndNil(MyData);
                except
                  on E:Exception do
                    if LOG_CLIENTMODULE_ERRORS then
                      Log('TRtcClientModule.DataReceived FREE ResultData',E,'ERROR');
                  end;
                MyData:=nil;
                end;

              if assigned(MyArr) then
                begin
                if idx-ida<MyArr.Count then
                  begin
                  MyData:=MyArr.asObject[idx-ida];
                  MyArr.asObject[idx-ida]:=nil;
                  end
                else // all results extracted
                  RtcFreeAndNil(MyArr);
                end;

              until MyArr=nil;

            end; // while at<length(code);

          SetLength(code,0);

          // All Results processed, clear "Call Data" ...
          for idx:=0 to MyCalls.EventCount-1 do
            begin
            MyData:=MyCalls.asObject[idx];
            MyCalls.Event[idx]:=nil;
            MyCalls.asObject[idx]:=nil;
            if assigned(MyData) then
              begin
              try
                RtcFreeAndNil(MyData);
              except
                on E:Exception do
                  if LOG_CLIENTMODULE_ERRORS then
                    Log('TRtcClientModule.DataReceived FREE Call Data',E,'ERROR');
                end;
              MyData:=nil;
              end;
            end;

          end; // Code<>''
      except
        on E:Exception do
          begin
          if LOG_CLIENTMODULE_ERRORS then
            Log('TRtcClientModule.DataReceived EXCEPTION',E,'ERROR');
          Response.StatusCode:=0; // Internal exception
          Response.StatusText:=RtcString(E.Message);
          Call_WrongResponse(Sender);
          end;
        end;
      end;
    end;
  end;

procedure TRtcClientModule.SetCompress(const Value: TRtcCompressLevel);
  begin
  FCompress := Value;
  end;

procedure TRtcClientModule.Call_DataOut(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_DataIn(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_DataSent(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_ResponseData(Sender: TRtcConnection);
  begin
  if not Sender.Request.Skipped and
     not Sender.Response.Rejected then
    if assigned(Link) then
      Link.Call_ResponseData(Sender)
    else if assigned(Client) then
      Client.CallResponseData;
  end;

procedure TRtcClientModule.Call_ResponseDone(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseDone) then
    if not AutoSyncEvents or not Sender.Sync(FOnResponseDone) then
      FOnResponseDone(Sender);

  if not Sender.Request.Skipped and
     not Sender.Response.Rejected then
    if assigned(Link) then
      Link.Call_ResponseDone(Sender)
    else if assigned(Client) then
      Client.CallResponseDone;
  end;

procedure TRtcClientModule.Call_RepostCheck(Sender: TRtcConnection);
  begin
  if ((AutoRepost<0) or (Sender.Request.Reposted<AutoRepost)) then
    Sender.Request.Repost;

  if not Sender.Request.Reposting then
    begin
    if assigned(FOnRepostCheck) then
      FOnRepostCheck(Sender);

    if not Sender.Request.Reposting then
      begin
      if assigned(Link) then
        Link.Call_RepostCheck(Sender)
      else if assigned(Client) then
        Client.CallRepostCheck;
      end;
    end;

  with Sender do
    if Request.Info.asBoolean[CLIMOD_ACTIVE] then
      if IsRemoteCallRequest(Sender) then
        ResetCrypt(Sender) // encryption most likely broken
      else
        DelCrypt(Sender);
  end;

procedure TRtcClientModule.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  if IsRemoteCallRequest(Sender) then
    begin
    if assigned(FOnResponseAbort) then
      if not AutoSyncEvents or not Sender.Sync(FOnResponseAbort) then
        FOnResponseAbort(Sender);

    if not Sender.Request.Reposting then
      begin
      if assigned(Link) then
        Link.Call_ResponseAbort(Sender)
      else if assigned(Client) then
        Client.CallResponseAbort;

      if not Sender.Request.Reposting then
        if not AutoSyncEvents or not Sender.Sync(NotifyResultAborted) then
          NotifyResultAborted(Sender);
      end;
    end
  else
    begin
    if assigned(Link) then
      Link.Call_ResponseAbort(Sender)
    else if assigned(Client) then
      Client.CallResponseAbort;
    end;
  end;

procedure TRtcClientModule.Call_ResponseReject(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseReject) then
    if not AutoSyncEvents or not Sender.Sync(FOnResponseReject) then
      FOnResponseReject(Sender);

  if assigned(Link) then
    Link.Call_ResponseReject(Sender)
  else if assigned(Client) then
    Client.CallResponseReject;
  end;

procedure TRtcClientModule.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FPingTimer) then
    begin
    TRtcTimer.Stop(FPingTimer);
    FPingTimer:=nil;
    end;

  if FObjectLinkSupport in [ol_AutoClient,ol_AutoBoth] then
    begin
    if not AutoSyncEvents or not Sender.Sync(DoReactivateObjectManager) then
      begin
      RemoveObjectManager;
      ActivateObjectManager(True);
      end;
    end
  else if FObjectLinkSupport<>ol_None then
    begin
    if not AutoSyncEvents or not Sender.Sync(DoDeactivateObjectManager) then
      RemoveObjectManager;
    end;

  if assigned(FOnSessionClose) then
    if not AutoSyncEvents or not Sender.Sync(FOnSessionClose) then
      FOnSessionClose(Sender);

  if assigned(Link) then
    Link.Call_SessionClose(Sender)
  else if assigned(Client) then
    Client.CallSessionClose;
  end;

procedure TRtcClientModule.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionOpen) then
    if not AutoSyncEvents or not Sender.Sync(FOnSessionOpen) then
      FOnSessionOpen(Sender);

  if assigned(Link) then
    Link.Call_SessionOpen(Sender)
  else if assigned(Client) then
    Client.CallSessionOpen;

  if FAutoSessionsPing>0 then
    begin
    if not assigned(FPingTimer) then
      if assigned(FOnPing) and AutoSyncEvents then
        FPingTimer:=TRtcTimer.Create(False)
      else if assigned(Sender) then
        FPingTimer:=TRtcTimer.Create(Sender.MultiThreaded)
      else if assigned(Client) then
        FPingTimer:=TRtcTimer.Create(Client.MultiThreaded)
      else if assigned(Link) then
        FPingTimer:=TRtcTimer.Create(Link.isMultiThreaded)
      else
        Exit; // No connection!??
    TRtcTimer.Enable(FPingTimer,FAutoSessionsPing*1000,DoSessionPing,True);
    end
  else if assigned(FPingTimer) then
    begin
    TRtcTimer.Stop(FPingTimer);
    FPingTimer:=nil;
    end;
  end;

procedure TRtcClientModule.Call(ResultHandler: TRtcResult; FromInsideEvent:boolean=False; Sender:TRtcConnection=nil);
  var
    idx:integer;
    myData:TRtcClientModuleData;
  begin
  myData:=CheckMyData;

  if myData=nil then
    raise Exception.Create('No Data defined. Use the "Data" property before "Call".')
  else if (myData.FRequest=nil) or (FDataReqMode in [req_contentBodyALL, req_contentBodyParams]) then
    begin
    if myData.FData=nil then
      raise Exception.Create('No Data defined. Use the "Data" property before "Call".')
    else if myData.FData.isNull then
      raise Exception.Create('No Data defined. Use the "Data" property before "Call".');
    end;

  {if not assigned(ResultHandler) then
    raise Exception.Create('Can not use "Call" with NIL as parameter.');
  if not assigned(ResultHandler.OnReturn) then
    raise Exception.Create('OnReturn event undefined for given TRtcResult component.'); }

  with myData do
    begin
    if not assigned(FCalls) then
      FCalls:=TRtcClientModuleCallsArray.Create;

    // Add Data and ResultHandler to our list of calls
    idx:=FCalls.EventCount;
    if FData=nil then // "NULL" placeholder
      FCalls.asObject[idx]:=TRtcValue.Create
    else
      FCalls.asObject[idx]:=FData;
    FCalls.Event[idx]:=ResultHandler;
    // set to NIL
    FData:=nil;

    if FPostLevel=0 then
      begin
      Inc(FPostLevel);
      Post(FromInsideEvent, Sender);
      end;
    end;
  end;

procedure TRtcClientModule.SetupCallRequest(FRequest:TRtcRequest);
  begin
  if FDataCustomQuery<>'' then
    FRequest.Query.Text:=FDataCustomQuery;

  if FRequest.Host='' then
    if ModuleHost<>'' then
      FRequest.Host:=ModuleHost;

  if FRequest.Agent='' then
    if FDataFormat<>fmt_RTC then
      FRequest.Agent:='RTC';

  if FRequest.ContentType='' then
    if FDataContentType<>'' then
      FRequest.ContentType:=FDataContentType
    else if FDataFormat<>fmt_RTC then
      begin
      case FDataFormat of
          fmt_XMLRPC:   FRequest.ContentType:='text/xml';
          fmt_JSONrpc1,
          fmt_JSONrpc2: FRequest.ContentType:='application/json-rpc';
          fmt_JSON:     FRequest.ContentType:='application/json';
          end;
      end
    else if FDataReqMode<>req_contentBodyALL then
      FRequest.ContentType:='RTC';
  end;

procedure TRtcClientModule.LoginCall(ResultHandler: TRtcResult; Sender:TRtcConnection; Insert:boolean=False);
  var
    DataReq:TRtcDataRequestInfo;
    FCalls:TRtcClientModuleCallsArray;
    FRequest:TRtcClientRequest;
    FTempRequest:TRtcRequest;
    CallData:TRtcValue;
  begin
  // Skip the next "PING" call ...
  if assigned(FPingTimer) then
    TRtcTimer.Reset(FPingTimer);

  CallData:=TRtcValue.Create;
  try
    // Create a new "Request" object
    FRequest:=TRtcClientRequest.Create;
    try
      SetupCallRequest(FRequest);
      if assigned(Sender) then
        begin
        FTempRequest:=Sender.Request;
        Sender.Request:=FRequest; // Sender.Request => our request object
        try
          if not AutoSyncEvents or not Sender.Sync(FOnLogin, CallData) then
            FOnLogin(Sender, CallData);
        finally
          Sender.Request:=FTempRequest;
          end;
        end
      else
        FOnLogin(Sender, CallData);
    except
      RtcFreeAndNil(FRequest);
      raise;
      end;
  except
    RtcFreeAndNil(CallData);
    raise;
    end;

  // Create the "Calls" object with our remote function call
  FCalls:=TRtcClientModuleCallsArray.Create;
  FCalls.asObject[0]:=CallData;
  FCalls.Event[0]:=ResultHandler;

  // Assign our "Calls" object to the Request object, so we can access it after we post it.
  FRequest.Info.asObj[CLIMOD_CALLS]:=FCalls;
  FRequest.Info.asBoolean[CLIMOD_LOGIN]:=True;

  // Create a "DataRequest" object and store the "Request" object into it
  DataReq:=TRtcDataRequestInfo.Create;
  DataReq.Request:=FRequest;
  DataReq.Events:=Self;

  // Insert the Request
  if Insert then
    Sender.InsertRequest(DataReq)
  else
    Sender.PostRequest(DataReq,True);
  end;

procedure TRtcClientModule.PingCall(ResultHandler:TRtcResult; Sender:TRtcConnection);
  var
    DataReq:TRtcDataRequestInfo;
    FCalls:TRtcClientModuleCallsArray;
    FRequest:TRtcClientRequest;
    CallData:TRtcValue;
  begin
  CallData:=TRtcValue.Create;
  try
    if assigned(FOnPing) then
      FOnPing(Sender, CallData);
  except
    RtcFreeAndNil(CallData);
    raise;
    end;
  // Create the "Calls" object with our remote function call
  FCalls:=TRtcClientModuleCallsArray.Create;
  FCalls.asObject[0]:=CallData;
  FCalls.Event[0]:=ResultHandler;

  // Create a new "Request" object
  FRequest:=TRtcClientRequest.Create;
  SetupCallRequest(FRequest);

  // Assign our "Calls" object to the Request object, so we can access it after we post it.
  FRequest.Info.asObj[CLIMOD_CALLS]:=FCalls;

  // Create a "DataRequest" object and store the "Request" object into it
  DataReq:=TRtcDataRequestInfo.Create;
  DataReq.Request:=FRequest;
  DataReq.Events:=Self;

  // Post the Request
  if assigned(Sender) then
    Sender.PostRequest(DataReq,False)
  else
    PostRequest(DataReq,False);
  end;

procedure TRtcClientModule.StartCalls;
  begin
  with GetMyData do
    begin
    Inc(FPostLevel);
    if assigned(FData) then FData.Clear;
    end;
  end;

procedure TRtcClientModule.ClearMyData;
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

procedure TRtcClientModule.ClearData;
  var
    id:RtcThrID;
    obj:TObject;
  begin
  if not FHyperThreading then
    begin
    if assigned(FMainThrData) then
      RtcFreeAndNil(FMainThrData.FData);
    end
  else
    begin
    if InsideMainThread then
      begin
      if assigned(FMainThrData) then
        RtcFreeAndNil(FMainThrData.FData);
      end
    else
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

procedure TRtcClientModule.ClearAllThreadsData;
  var
    id:RtcThrID;
    obj:TObject;
  begin
  if assigned(FMainThrData) then
    RtcFreeAndNil(FMainThrData.FData);
 
  if assigned(FMyData) then
	begin   
    id:=FMyData.search_min(obj);
    while (id>0) and (obj<>nil) do
      begin
      FMyData.remove(id);
      RtcFreeAndNil(obj);
      id:=FMyData.search_min(obj);
      end;
    end;
  end;

function TRtcClientModule.CheckMyData: TRtcClientModuleData;
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
        if obj<>nil then
          Result:=TRtcClientModuleData(obj)
        else
          Result:=nil;
      finally
        FCS.Release;
        end;
      end;
    end;
  end;

function TRtcClientModule.GetMyData: TRtcClientModuleData;
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
          obj:=TRtcClientModuleData.Create;
          FMyData.insert(id, obj);
          end;
        Result:=TRtcClientModuleData(obj);
      finally
        FCS.Release;
        end;
      end;
    end;
  end;

function TRtcClientModule.Get_Data: TRtcValue;
  var
    myData:TRtcClientModuleData;
  begin
  myData:=GetMyData;
  if not assigned(myData.FData) then
    myData.FData:=TRtcValue.Create;
  Result:=myData.FData;
  end;

function TRtcClientModule.GetPostLevel: integer;
  var
    myData:TRtcClientModuleData;
  begin
  myData:=GetMyData;
  Result:=myData.FPostLevel;
  end;

function TRtcClientModule.GetRequest: TRtcClientRequest;
  var
    myData:TRtcClientModuleData;
  begin
  myData:=GetMyData;
  if not assigned(myData.FRequest) then
    begin
    myData.FRequest:=TRtcClientRequest.Create;
    SetupCallRequest(myData.FRequest);
    end;
  Result:=myData.FRequest;
  end;

procedure TRtcClientModule.Post(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil);
  var
    DataReq:TRtcDataRequestInfo;
    myData:TRtcClientModuleData;
  begin
  myData:=CheckMyData;

  if myData=nil then
    raise Exception.Create('Have to use "StartCalls" before "Post"'#13#10+
                           'to post multiple calls in one request.');

  if myData.FPostLevel<=0 then
    raise Exception.Create('Have to use "StartCalls" before "Post"'#13#10+
                           'to post multiple calls in one request.');

  with myData do
    begin
    Dec(FPostLevel);
    if FPostLevel>0 then Exit;

    if assigned(FCalls) then
      begin
      // Skip the next "PING" call ...
      if assigned(FPingTimer) then
        TRtcTimer.Reset(FPingTimer);

      if not assigned(FRequest) then
        begin
        FRequest:=TRtcClientRequest.Create;
        SetupCallRequest(FRequest);
        end;

      // Assign our Calls to the Request object, so we can access it after we post it.
      FRequest.Info.asObj[CLIMOD_CALLS]:=FCalls;
      FCalls:=nil;

      DataReq:=TRtcDataRequestInfo.Create;
      DataReq.Request:=FRequest;
      DataReq.Events:=Self;
      FRequest:=nil;

      if assigned(Sender) then
        Sender.PostRequest(DataReq,FromInsideEvent)
      else
        PostRequest(DataReq,FromInsideEvent);
      end;
    end;

  // Free ClientModuleData and remove it from the list
  ClearMyData;
  end;

procedure TRtcClientModule.CancelCalls;
  var
    myData:TRtcClientModuleData;
  begin
  myData:=CheckMyData;

  if myData=nil then
    raise Exception.Create('"CancelCalls" can only be used in pair with "StartCalls"');

  with myData do
    begin
    if FPostLevel<=0 then
      raise Exception.Create('"CancelCalls" can only be used in pair with "StartCalls"');

    FPostLevel:=0;

    if assigned(FCalls) then
      RtcFreeAndNil(FCalls);
    end;

  ClearMyData;
  end;

function TRtcClientModule.GetFunctionGroup: TRtcFunctionGroup;
  begin
  try
    Result:=FFunctions;
    if not (Result is TRtcFunctionGroup) then
      Result:=nil;
  except
    Result:=nil;
    end;
  end;

procedure TRtcClientModule.SetFunctionGroup(const Value: TRtcFunctionGroup);
  begin
  FFunctions:=Value;
  end;

function TRtcClientModule.GetModuleFileName: RtcString;
  begin
  Result:=FModuleFileName;
  end;

procedure TRtcClientModule.SetModuleFileName(const Value: RtcString);
  begin
  if FModuleFileName<>Value then
    begin
    FModuleFileName:=Value;
    if FModuleFileName<>'' then
      begin
      // FileName has to start with '/'
      if Copy(FModuleFileName,1,1)<>'/' then
        FModuleFileName:='/'+FModuleFileName;
      if Copy(FModuleFileName,length(FModuleFileName),1)<>'/' then
        FModuleFileREST:=FModuleFileName+'/'
      else
        FModuleFileREST:=FModuleFileName;
      end
    else
      FModuleFileREST:='';
    FCryptSesName:=FModuleHost+FModuleFileName+'.$CLI-CRYPT$';
    FObjManSesName:=FModuleHost+FModuleFileName+RTCL_CLISESSION;
    if assigned(FObjectManager) then
      FObjectManager.BroadcastGroup:=RtcWideString(FObjManSesName);
    end;
  end;

function TRtcClientModule.GetModuleHost: RtcString;
  begin
  Result:=FModuleHost;
  end;

procedure TRtcClientModule.SetModuleHost(const Value: RtcString);
  begin
  if FModuleHost<>Value then
    begin
    FModuleHost:=Value;
    FCryptSesName:=FModuleHost+FModuleFileName+'.$CLI-CRYPT$';
    FObjManSesName:=FModuleHost+FModuleFileName+RTCL_CLISESSION;
    if assigned(FObjectManager) then
      FObjectManager.BroadcastGroup:=RtcWideString(FObjManSesName);
    end;
  end;

procedure TRtcClientModule.Response_Problem(Sender: TRtcConnection);
  begin
  with Sender do
    begin
    if not Request.Reposting and not Response.Rejected then
      begin
      Call_RepostCheck(Sender);
      if not Request.Reposting and not Response.Rejected then
        Call_ResponseAbort(Sender);
      end;
    end;
  end;

procedure TRtcClientModule.Call_SessionExpired(Sender: TRtcConnection);
  begin
  DelCrypt(Sender);
  if assigned(FOnSessionExpired) then
    FOnSessionExpired(Sender);
  with Sender do
    begin
    Session.Init;
    if AutoSessionMode=rsm_Cookie then
      Request.Cookie.Value[SessIDField]:=''
    else
      Request.Query.Value[SessIDField]:='';
    if not Request.Reposting and not Response.Rejected then
      if Request.Reposted<1 then // if Session expires, we will try to repost 1 time ...
        Request.Repost
      else // ... and leave all other decisions to the user
        Response_Problem(Sender);
    end;
  end;

procedure TRtcClientModule.Call_WrongResponse(Sender: TRtcConnection);
  begin
  DelCrypt(Sender);
  if assigned(FOnResponseError) then
    FOnResponseError(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.Call_ResultError(Sender: TRtcConnection; Data,Result:TRtcValue; E:Exception);
  begin
  if assigned(FOnResultError) then
    FOnResultError(Sender,Data,Result,E);
  end;

procedure TRtcClientModule.Call_WrongEncryption(Sender: TRtcConnection);
  begin
  ResetCrypt(Sender);
  if assigned(FOnWrongEncryption) then
    FOnWrongEncryption(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.Call_WrongEncryptionStart(Sender: TRtcConnection);
  begin
  DelCrypt(Sender);
  if assigned(FOnWrongEncryption) then
    FOnWrongEncryption(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.Call_NoEncryption(Sender: TRtcConnection);
  begin
  DelCrypt(Sender);
  if assigned(FOnNoEncryption) then
    FOnNoEncryption(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.Call_NeedEncryption(Sender: TRtcConnection);
  begin
  DelCrypt(Sender);
  if assigned(FOnNeedEncryption) then
    FOnNeedEncryption(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.SetAutoEncrypt(const Value: integer);
  begin
  if Value<0 then
    raise Exception.Create('Negative values not allowed for EncryptionKey.');
  FAutoEncrypt := Value;
  if FAutoEncrypt > 0 then
    FAutoSessions:=True
  else
    FForceEncrypt:=False;
  end;

procedure TRtcClientModule.SetEncryptMode(const Value: TRtcEncryptionMode);
  begin
  FEncryptMode := Value;
  end;

procedure TRtcClientModule.SetAutoSessions(const Value: boolean);
  begin
  FAutoSessions := Value;
  if not FAutoSessions then
    begin
    FAutoEncrypt:=0;
    FAutoLogin:=False;
    FForceEncrypt:=False;
    end;
  end;

procedure TRtcClientModule.SetForceEncrypt(const Value: boolean);
  begin
  FForceEncrypt := Value;
  if FForceEncrypt then
    begin
    FAutoSessions:=True;
    if FAutoEncrypt=0 then
      FAutoEncrypt:=16;
    end;
  end;

procedure TRtcClientModule.SetObjectLinkSupport(const Value: TRtcObjectLinkSupport);
  begin
  if (Value<>FObjectLinkSupport) then
    begin
    if ActiveGlobalManager and (FObjectLinkSupport in [ol_AutoClient,ol_AutoBoth]) then
      begin
      ActiveGlobalManager:=False;
      RemoveObjectManager;
      FObjectLinkSupport:=ol_None;
      end;
    if Value in [ol_AutoClient,ol_AutoBoth] then
      begin
      if ActiveGlobalManager then
        raise ERtcObjectLinks.Create('Only one TRtcClientModule with ObjectLinks=ol_AutoClient/ol_AutoBoth allowed per Application.');
      FObjectLinkSupport := Value;
      if not (csDesigning in ComponentState) then
        begin
        ActiveGlobalManager:=True;
        ActivateObjectManager(True);
        end;
      end
    else
      FObjectLinkSupport := Value;
    end;
  end;

procedure TRtcClientModule.PostInteractiveResult(Event: TRtcResult; Data, Result: TRtcValue);
  var
    res:TRtcInteractiveResult;
  begin
  FIntCS.Acquire;
  try
    res:=TRtcInteractiveResult.Create;
    res.FEvent:=Event;
    res.Data:=Data;
    res.Result:=Result;

    FIntRes.AddLast(res);

    if not assigned(FIntTimer) then
      begin
      FIntTimer:=TRtcTimer.Create(False);
      TRtcTimer.Enable(FIntTimer,1,DoInteractiveResult,True);
      end;
  finally
    FIntCS.Release;
    end;
  end;

procedure TRtcClientModule.DoInteractiveResult;
  var
    ores:TObject;
    res:TRtcInteractiveResult absolute ores;
  begin
  FIntCS.Acquire;
  try
    FIntRes.extractFirst(ores);
  finally
    FIntCS.Release;
    end;

  try
    res.FEvent.Call_Return(nil, res.Data, res.Result);
  finally
    RtcFreeAndNil(ores);

    FIntCS.Acquire;
    try
      if FIntRes.Count>0 then
        TRtcTimer.Enable(FIntTimer,1,DoInteractiveResult,True)
      else
        begin
        TRtcTimer.Stop(FIntTimer);
        FIntTimer:=nil;
        end;
    finally
      FIntCS.Release;
      end;
    end;
  if FRelease then
    {$IFDEF NEXTGEN} DisposeOf; {$ELSE} Free; {$ENDIF}
  end;

procedure TRtcClientModule.Release;
  begin
  FRelease:=True;
  end;

procedure TRtcClientModule.NotifyResultAborted(Sender: TRtcConnection);
  var
    MyCalls:TRtcClientModuleCallsArray;
    event:TRtcResult;
    data:TRtcValue;
    a:integer;
  begin
  MyCalls:=TRtcClientModuleCallsArray(Sender.Request.Info.asObj[CLIMOD_CALLS]);
  if assigned(MyCalls) then
    begin
    for a:=0 to MyCalls.EventCount-1 do
      begin
      event:=MyCalls.Event[a];
      if assigned(event) then
        begin
        data:=TRtcValue(MyCalls.AsObject[a]);
        event.Call_Aborted(Sender,data,nil);
        end;
      end;
    end;
  end;

function TRtcClientModule.IsRemoteCallRequest(Sender:TRtcConnection): boolean;
  begin
  Result := assigned(Sender.Request.Info.asObj[CLIMOD_CALLS]);
  end;

procedure TRtcClientModule.SetDataReqMode(const Value: TRtcDataReqMode);
  begin
  FDataReqMode := Value;
  end;

procedure TRtcClientModule.SetDataFormat(const Value: TRtcDataFormat);
  begin
  FDataFormat := Value;
  end;

procedure TRtcClientModule.SetAutoLogin(const Value: boolean);
  begin
  FAutoLogin := Value;
  if FAutoLogin then
    FAutoSessions:=True;
  end;

procedure TRtcClientModule.Call_LoginAborted(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if assigned(FOnLoginAborted) then
    FOnLoginAborted(Sender,Data,Result);
  end;

procedure TRtcClientModule.Call_LoginResult(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if assigned(FOnLoginResult) then
    FOnLoginResult(Sender,Data,Result);

  if Result.isType<>rtc_Exception then
    Sender.Session.asBoolean[CLIMOD_LOGIN]:=True;
  end;

procedure TRtcClientModule.Call_PingAborted(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if assigned(FOnPingAborted) then
    FOnPingAborted(Sender,Data,Result);
  if not Sender.Request.Reposting then
    begin
    RemoveObjectManager;
    if assigned(Sender.Session) then
      Sender.Session.Close;
    end;
  end;

procedure TRtcClientModule.Call_PingResult(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  try
    if assigned(FOnPingResult) then
      FOnPingResult(Sender,Data,Result);
  finally
    if assigned(FPingTimer) then
      if FAutoSessionsPing>0 then
        TRtcTimer.Enable(FPingTimer,FAutoSessionsPing*1000,DoSessionPing,True)
      else
        begin
        TRtcTimer.Stop(FPingTimer);
        FPingTimer:=nil;
        end;
    end;
  end;

procedure TRtcClientModule.ResetLogin;
  begin
  FResetLogin:=True;
  end;

function TRtcClientModule.Prepare(const FunctionName: RtcWideString): TRtcFunctionInfo;
  begin
  Data.Clear;
  Result:=Data.NewFunction(FunctionName);
  end;

function TRtcClientModule.GetLastCallParams: TRtcFunctionInfo;
  begin
  if Data.isType=rtc_Function then
    Result:=Data.asFunction
  else
    raise Exception.Create('Use Prepare() or Data.newFunction() to prepare a function object first.');
  end;

function TRtcClientModule.Execute(AutoFreeResult:boolean; _Timeout:cardinal; AllowMessageProcessing:boolean): TRtcValue;
  begin
  RtcFreeAndNil(FExecuteResult);
  FExecuteError:='';
  Call(FExecuteHandler);
  case DoWaitForCompletion(False,_Timeout,AllowMessageProcessing) of
    wait_OK:
      begin
      if FExecuteError<>'' then
        begin
        CancelRequests;
        raise ERtcExecuteError.Create(FExecuteError,wait_OK);
        end
      else if FExecuteResult=nil then
        begin
        CancelRequests;
        raise ERtcExecuteError.Create('Error: Connection problems, no response received.',wait_OK);
        end;
      Result:=FExecuteResult;
      if not AutoFreeResult then
        FExecuteResult:=nil; // set to NIL only if we do NOT want to Free the object!
      end;
    wait_Timeout:
      begin
      CancelRequests;
      raise ERtcExecuteError.Create('Error: Connection problems, response timed out.',wait_Timeout);
      end;
    wait_Quit:
      begin
      CancelRequests;
      raise ERtcExecuteError.Create('Error: Application Terminating.',wait_Quit);
      end;
    wait_Msg:
      begin
      CancelRequests;
      raise ERtcExecuteError.Create('Error: Unknown message received.',wait_Msg);
      end;
    else
      begin
      CancelRequests;
      raise ERtcExecuteError.Create('Error: Connection problems.',wait_Error);
      end;
    end;
  end;

procedure TRtcClientModule.Call_ExecuteResult(Sender: TRtcConnection; Data,Result: TRtcValue);
  begin
  FExecuteResult:=TRtcValue.Create;
  FExecuteResult.asObject:=Result.asObject;
  Result.asObject:=nil;
  end;

procedure TRtcClientModule.Call_ExecuteAbort(Sender: TRtcConnection; Data,Result: TRtcValue);
  begin
  if FExecuteError='' then
    if Sender.Response.StatusText<>'' then
      if Sender.Response.StatusCode<>0 then
        FExecuteError:='Error response #'+IntToStr(Sender.Response.StatusCode)+': '+String(Sender.Response.StatusText)
      else
        FExecuteError:='Error: Bad response. '+String(Sender.Response.StatusText);
  end;

procedure TRtcClientModule.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent,Operation);
  if Operation=opRemove then
    if AComponent=FFunctions then
      SetFunctionGroup(nil);
  end;

procedure TRtcClientModule.DoObjectManagerDataReady(Sender: TRtcRemoteObjectManager);
  begin
  if not FObjectLinksOut then
    DoObjectManagerSend(Sender,nil);
  end;

procedure TRtcClientModule.DoObjectManagerSend(Sender: TRtcRemoteObjectManager; Conn:TRtcConnection);
  var
    MyData:TRtcValue;
  begin
  if not Sender.Idle then
    begin
    Sender.ExecuteBroadcast(Conn);
    MyData:=Sender.GetData;
    if assigned(MyData) then
      begin
      FObjectLinksOut:=True;
      if assigned(FOnObjectDataOut) then
        FOnObjectDataOut(Conn,MyData);
      Data.NewFunction(RTCL_FUNCTION).asObject[RTCL_DATA]:=MyData;
      Call(FObjectLinkResult,assigned(Conn),Conn);
      end
    else
      FObjectLinksOut:=False;
    end
  else
    FObjectLinksOut:=False;
  end;

procedure TRtcClientModule.Call_ObjectLinkAborted(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  FObjectLinksOut:=False;
  if assigned(FOnObjectLinkAborted) then
    FOnObjectLinkAborted(Sender,Data,Result);
  if not Sender.Request.Reposting then
    begin
    RemoveObjectManager;
    if assigned(Sender.Session) then
      Sender.Session.Close;
    end;
  end;

procedure TRtcClientModule.Call_ObjectLinkResult(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if Result.isType<>rtc_Null then
    Call_ObjectLinkErrors(Sender,Data,Result);
  DoObjectManagerSend(FObjectManager,Sender);
  end;

procedure TRtcClientModule.Call_ObjectLinkErrors(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if assigned(FOnObjectLinkErrors) then
    FOnObjectLinkErrors(Sender,Data,Result);
  end;

procedure TRtcClientModule.DoObjectCreate(Sender: TObject; Param: TRtcObjectCall);
  begin
  if assigned(FOnObjectCreate) then
    FOnObjectCreate(TRtcConnection(Sender),Param);
  end;

procedure TRtcClientModule.SetAutoSessionsPing(const Value: integer);
  begin
  FAutoSessionsPing := Value;
  if FAutoSessionsPing<=0 then
    begin
    if assigned(FPingTimer) then
      begin
      TRtcTimer.Stop(FPingTimer);
      FPingTimer:=nil;
      end;
    end
  else if assigned(FPingTimer) then
    TRtcTimer.Enable(FPingTimer,FAutoSessionsPing*1000,DoSessionPing,True);
  end;

procedure TRtcClientModule.DoSessionPing;
  begin
  PingCall(FPingResult,nil);
  end;

procedure TRtcClientModule.DoReactivateObjectManager(Sender: TRtcConnection);
  begin
  RemoveObjectManager;
  ActivateObjectManager(True);
  end;

procedure TRtcClientModule.DoDeactivateObjectManager(Sender: TRtcConnection);
  begin
  RemoveObjectManager;
  end;

{ TRtcInteractiveResult }

destructor TRtcInteractiveResult.Destroy;
  begin
  try
    RtcFreeAndNil(Data);
    RtcFreeAndNil(Result);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcInteractiveResult.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcClientModuleCallsArray }

constructor TRtcClientModuleCallsArray.Create;
  begin
  inherited;
  SetLength(FEvents,0);
  end;

destructor TRtcClientModuleCallsArray.Destroy;
  begin
  try
    SetLength(FEvents,0);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcClientModuleCallsArray.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcClientModuleCallsArray.EventCount:integer;
  begin
  Result:=length(FEvents);
  end;

function TRtcClientModuleCallsArray.GetEvent(index: integer): TRtcResult;
  begin
  if (index>=0) and (index<=length(FEvents)) then
    Result:=FEvents[index]
  else
    Result:=nil;
  end;

procedure TRtcClientModuleCallsArray.SetEvent(index: integer; const _Value: TRtcResult);
  begin
  if length(FEvents)<index+1 then
    SetLength(FEvents, index+1);
  FEvents[index]:=_Value;
  end;

{ TRtcCryptClient }

constructor TRtcCryptClient.Create(m:TRtcEncryptionMode;rk:integer);
  begin
  inherited Create;

  HaveHello:=False;
  HaveStart:=False;
  AllReady:=False;
  ControlCounter:=0;
  ClientHello:='';
  ServerHello:='';
  ClientKey:='';
  ServerKey:='';
  CRead:=nil;
  CWrite:=nil;
{$IFDEF RTC_RSA}
  Mode:=m;
  RSAKey:=rk;
  CReadEx:=nil;
  CWriteEx:=nil;
  CReadRSA:=nil;
{$ENDIF}
  end;
  
destructor TRtcCryptClient.Destroy;
  begin
  try
    Init(rem_Basic,0);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcCryptClient.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcCryptClient.Init(m:TRtcEncryptionMode;rk:integer);
  begin
  HaveHello:=False;
  HaveStart:=False;
  AllReady:=False;
  ControlCounter:=0;
  ClientHello:='';
  ServerHello:='';
  ClientKey:='';
  ServerKey:='';
  RtcFreeAndNil(CRead);
  RtcFreeAndNil(CWrite);
{$IFDEF RTC_RSA}
  Mode:=m;
  RSAKey:=rk;
  RtcFreeAndNil(CReadEx);
  RtcFreeAndNil(CWriteEx);
  RtcFreeAndNil(CReadRsa);
{$ENDIF}
  end;

procedure TRtcCryptClient.Reset;
  begin
  ControlCounter:=0;
  RtcFreeAndNil(CWrite);
  RtcFreeAndNil(CRead);

{$IFDEF RTC_RSA}
  RtcFreeAndNil(CWriteEx);
  RtcFreeAndNil(CReadEx);
  RtcFreeAndNil(CReadRsa);
  case Mode of
    rem_Basic:
      begin
      CWrite:=TRtcCrypt.Create;
      CRead:=TRtcCrypt.Create;
      if RSAKey=0 then
        begin
        CWrite.Key:= ServerKey + ClientHello;
        CRead.Key:= ClientKey + ServerHello;
        end
      else
        begin
        CWrite.Key:= ServerKey;
        CRead.Key:= ClientKey + ServerHello;
        end;
      end;
    rem_Isaac:
      begin
      CWriteEx:=TRtcISAACrypt.Create;
      CReadEx:=TRtcISAACrypt.Create;
      if RSAKey=0 then
        begin
        CWriteEx.Key:= RtcStringToBytes(ServerKey + ClientHello);
        CReadEx.Key:= RtcStringToBytes(ClientKey + ServerHello);
        end
      else
        begin
        CWriteEx.Key:= RtcStringToBytes(ServerKey);
        CReadEx.Key:= RtcStringToBytes(ClientKey + ServerHello);
        end;
      end;
    end;
{$ELSE}
  CWrite:=TRtcCrypt.Create;
  CRead:=TRtcCrypt.Create;
  CWrite.Key:= ServerKey + ClientHello;
  CRead.Key:= ClientKey + ServerHello;
{$ENDIF}
  end;

{$IFNDEF RTC_RSA}
function RandomKey(len:integer):RtcString;
  var
    a:integer;
  begin
  SetLength(Result,len);
  for a:=1 to len do
    Result[a]:=RtcChar(random(256));
  end;
{$ENDIF}

function TRtcCryptClient.NewKey1(KeySize:integer):RtcByteArray;
  begin
{$IFDEF RTC_RSA}
  RtcFreeAndNil(CReadRSA);
  CReadRSA:=TRtcRSA.Create;
  if RSAKey=0 then
    ClientHello:=RtcBytesToString(CReadRSA.RND(KeySize))
  else
    begin
    CReadRSA.GenerateKeyPair(RSAKey);
    ClientKey:=RtcBytesToString(CReadRSA.PrivateKey);
    ClientHello:=RtcBytesToString(CReadRSA.PublicKey);
    end;
{$ELSE}
  ClientHello:=RandomKey(KeySize);
{$ENDIF}
  { Generate randoml control number to add at the end of the request,
    so we can check if the response is correctly encrypted. }
  ControlKey := GenerateControlKey(ControlCounter);
  Result:=RtcStringToBytes(ClientHello+#13+ControlKey);
  end;

procedure TRtcCryptClient.SecureCode(const key:RtcString; var scode:RtcByteArray);
  var
    c:TRtcCrypt;
{$IFDEF RTC_RSA}
    cx:TRtcISAACrypt;
{$ENDIF}
  begin
{$IFDEF RTC_RSA}
  case Mode of
    rem_Basic:
      begin
      c:=TRtcCrypt.Create;
      try
        c.Key:=key;
        c.CryptEx(scode);
      finally
        c.Free;
        end;
      end;
    rem_Isaac:
      begin
      cx:=TRtcISAACrypt.Create;
      try
        cx.Key:=RtcStringToBytes(key);
        cx.Crypt(scode);
      finally
        cx.Free;
        end;
      end;
    end;
{$ELSE}
  c:=TRtcCrypt.Create;
  try
    c.Key:=key;
    c.CryptEx(scode);
  finally
    c.Free;
    end;
{$ENDIF}
  end;

procedure TRtcCryptClient.SetupRead1(var code:RtcByteArray);
  begin
  RtcFreeAndNil(CRead);
{$IFDEF RTC_RSA}
  RtcFreeAndNil(CReadEx);
  RtcFreeAndNil(CReadRsa);
  if RSAKey=0 then
    begin
    case Mode of
      rem_Basic:
        begin
        CRead:=TRtcCrypt.Create;
        CRead.Key:=ClientHello;
        CRead.DeCryptEx(code);
        end;
      rem_Isaac:
        begin
        CReadEx:=TRtcISAACrypt.Create;
        CReadEx.Key:=RtcStringToBytes(ClientHello);
        CReadEx.DeCrypt(code);
        end;
      end;
    end
  else
    begin
    CReadRSA:=TRtcRSA.Create;
    CReadRSA.PrivateKey:=RtcStringToBytes(ClientKey);
    code:=CReadRSA.Decrypt(code);
    end;
{$ELSE}
  CRead:=TRtcCrypt.Create;
  CRead.Key:=ClientHello;
  CRead.DeCryptEx(code);
{$ENDIF}
  end;

procedure TRtcCryptClient.SetupWrite1;
  begin
  RtcFreeAndNil(CWrite);
{$IFDEF RTC_RSA}
  RtcFreeAndNil(CWriteEx);
  case Mode of
    rem_Basic:
      begin
      CWrite:=TRtcCrypt.Create;
      CWrite.Key:=ServerHello;
      end;
    rem_Isaac:
      begin
      CWriteEx:=TRtcISAACrypt.Create;
      CWriteEx.Key:=RtcStringToBytes(ServerHello);
      end;
    end;
{$ELSE}
  CWrite:=TRtcCrypt.Create;
  CWrite.Key:=ServerHello;
{$ENDIF}
  end;

function TRtcCryptClient.NewKey2(KeySize:integer):RtcByteArray;
  begin
{$IFDEF RTC_RSA}
  RtcFreeAndNil(CReadRSA);
  CReadRSA:=TRtcRSA.Create;
  ClientKey:=RtcBytesToString(CReadRSA.RND(KeySize));
{$ELSE}
  ClientKey:=RandomKey(KeySize);
{$ENDIF}
  { Generate a random control number to add at the end of the request,
    so we can check if the response is correctly encrypted. }
  ControlKey := GenerateControlKey(ControlCounter);
  Result:=RtcStringToBytes(ClientKey+#13+ControlKey);
  CryptWriteEx(self, Result);
  end;

procedure TRtcCryptClient.SetupRead2(var code:RtcByteArray);
  begin
{$IFDEF RTC_RSA}
  case Mode of
    rem_Basic:
      begin
      if CRead=nil then CRead:=TRtcCrypt.Create;
      CRead.Key:= ClientKey + ServerHello;
      CRead.DeCryptEx(code);
      end;
    rem_Isaac:
      begin
      if CReadEx=nil then CReadEx:=TRtcISAACrypt.Create;
      CReadEx.Key:= RtcStringToBytes(ClientKey + ServerHello);
      CReadEx.DeCrypt(code);
      end;
    end;
{$ELSE}
  if CRead=nil then CRead:=TRtcCrypt.Create;
  CRead.Key:= ClientKey + ServerHello;
  CRead.DeCryptEx(code);
{$ENDIF}
  end;

procedure TRtcCryptClient.SetupWrite2;
  begin
{$IFDEF RTC_RSA}
  case Mode of
    rem_Basic:
      begin
      if CWrite=nil then CWrite:=TRtcCrypt.Create;
      if RSAKey=0 then
        CWrite.Key:= ServerKey + ClientHello
      else
        CWrite.Key:= ServerKey;
      end;
    rem_Isaac:
      begin
      if CWriteEx=nil then CWriteEx:=TRtcISAACrypt.Create;
      if RSAKey=0 then
        CWriteEx.Key:= RtcStringToBytes(ServerKey + ClientHello)
      else
        CWriteEx.Key:= RtcStringToBytes(ServerKey);
      end;
    end;
{$ELSE}
  if CWrite=nil then CWrite:=TRtcCrypt.Create;
  CWrite.Key:= ServerKey + ClientHello;
{$ENDIF}
  end;

function TRtcCryptClient.CanRead:boolean;
  begin
  Result:=assigned(CRead) {$IFDEF RTC_RSA} or assigned(CReadEx) {$ENDIF};
  end;

function TRtcCryptClient.CanWrite:boolean;
  begin
  Result:=assigned(CWrite) {$IFDEF RTC_RSA} or assigned(CWriteEx) {$ENDIF};
  end;

{ ERtcExecuteError }

constructor ERtcExecuteError.Create(const Msg: String; ECode: TRtcWaitForCompletionResult);
  begin
  inherited Create(Msg);
  FErrCode:=ECode;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; {$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcCliModule Finalizing ...','DEBUG');{$ENDIF}
CloseTimerPool;
{$IFDEF RTC_DEBUG} Log('rtcCliModule Finalized.','DEBUG');{$ENDIF}
end.
