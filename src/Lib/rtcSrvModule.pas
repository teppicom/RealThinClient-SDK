{
  @html(<b>)
  Server Module for Remote Functions
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit introduces @Link(TRtcServerModule), a server-side component for ENABLING remote functions.
  Implementing RTC Remote Functions is as easy as writing local functions.
}
unit rtcSrvModule;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSystem,
  rtcSrcList,
  rtcThrPool,
  rtcTimer,
  rtcLog,

{$IFDEF COMPRESS}
  rtcZLib,
{$ENDIF}

  rtcInfo,
  rtcCrypt,
  rtcLink,
  rtcConn,

  rtcDataSrv,
  rtcFunction;

const
  // @exclude
  RTC_SERVERMODULE_DELAYED_CALL='$RTC_SERVERMODULE_DELAYED_CALL$';

type
  { @abstract(Server Remote RTC Object Manager implementation) }
  TRtcServerObjectManager = class(TRtcBasicRemoteObjectManager);

  { Server-side Encryption mode @exclude }
  TRtcServerCryptMode=(rscm_Basic,rscm_Isaac,rscm_RSABasic,rscm_RSAIsaac);

  // @exclude
  TRtcCryptServer=class(TObject)
  public
    HaveHello,HaveStart:boolean;
    ControlCounter:integer;

    ClientHello,ServerHello,
    ClientKey,ServerKey,
    ControlKey:RtcString;

    CRead,CWrite:TRtcCrypt;

  {$IFDEF RTC_RSA}
    Mode:TRtcServerCryptMode;
    CReadEx,CWriteEx:TRtcISAACrypt;
    CWriteRsa:TRtcRSA;
  {$ENDIF}

    constructor Create(m:TRtcServerCryptMode);
    destructor Destroy; override;

    procedure Reset;

    procedure SetupWrite1;
    function SetupRead1(KeySize:integer):RtcByteArray;

    procedure SetupWrite2;
    function SetupRead2(KeySize:integer):RtcByteArray;

    function CanRead:boolean;
    function CanWrite:boolean;

    procedure Init(m:TRtcServerCryptMode);
    end;

  { @abstract(Delayed Function Call object)
    This object is returned from the PrepareDelayedCall function and
    can be used to Wake up the delayed call. The only method you are
    allowed to use on this object is "WakeUp", and this one may
    only be called once, after which you have to "forget" about that object
    (set your variable's pointer to NIL), so you don't go and call it often. }
  TRtcDelayedCall = class(TObject)
  protected
    SessionID:RtcString;
    crypt:TRtcCryptServer;
    output:TRtcHugeString;
    Compress:TRtcCompressLevel;
    FMT:TRtcDataFormat;

    msDelay:integer;
    Param:TRtcFunctionInfo;
    CallEvent:TRtcFunctionCallEvent;

    ObjectLinkSupport:TRtcObjectLinkSupport;
    ObjectDataOutEvent: TRtcDataEvent;

    Called:boolean;
    Timer:TRtcTimer;
    WaitEvent:TRtcEvent;
    Conn:TRtcConnection;

    procedure Post(Multi_Threaded:boolean);

    procedure Execute;

  public
    // @exclude
    constructor Create;
    // @exclude
    destructor Destroy; override;

    // Use this method to wake the delayed call up,
    // so it will be called NOW! instead of when the timeout period has expired.
    procedure WakeUp;
    end;

  // @exclude
  EDelayedCall = class(EAbort)
  public
    call:TRtcDelayedCall;
    end;

  { @abstract(Accepts the request and uses a TRtcFunctionGroup component to
    execute received functions and prepares the result)

    ModuleProvider is the Remote Object execution point, that enables the Client(s) to
    send one or more objects to the server and get executed object(s) as a result.
    If there are any function calls found inside the objects received, those functions
    will be executed, so that the resulting object contains only data. That resulting
    object (or a set of objects) will be sent back to the client who sent the request.
    In case of an exception, execution will be aborted and the last object sent to the
    client will be an exception message: isType = rtc_Exception; asException = error message; @html(<br><br>)

    Raising an exception in any event implemented for the Module Provider,
    will result in sending an Exception object back to the client. }
  TRtcBaseServerModule=class(TRtcAbsDataServerLink)
  private
    FFunctions:TRtcFunctionGroup;
    FModuleFileName:RtcString;
    FModuleFileREST:RtcString;
    FModuleHost:RtcString;
    FCryptSesName:RtcString;
    FObjManSesName:RtcString;

    FAutoSessions: boolean;
    FAutoSessionsLive: integer;
    FAutoSessionsLock: TRtcSessionLockType;

    FAutoSessionCheck: boolean;
    FAutoSessionField,
    FAutoSessionFieldUPC: RtcString;
    FAutoSessionMode: TRtcSessionsModeSupport;

    FOnPeekRequest: TRtcNotifyEvent;
    FOnListenStart:TRtcNotifyEvent;
    FOnListenStop:TRtcNotifyEvent;
    FOnRequestAccepted:TRtcNotifyEvent;
    FOnResponseDone:TRtcNotifyEvent;
    FOnDisconnect:TRtcNotifyEvent;
    FOnSessionOpen:TRtcNotifyEvent;
    FOnSessionClose:TRtcNotifyEvent;

    FOnObjectDataOut: TRtcDataEvent;
    FOnObjectDataIn: TRtcDataEvent;
    FOnObjectCreate: TRtcObjectCreateEvent;

    FAutoEncrypt: integer;
    FForceEncrypt: boolean;
    FSecureKey: RtcString;
    FCompress: TRtcCompressLevel;

    FDataFormats: TRtcDataFormatSupport;
    FDataReqModes: TRtcDataReqModeSupport;
    FObjectLinkSupport: TRtcObjectLinkSupport;

    function SessIDField:RtcString;
    function SessIDFieldCS:RtcString;

    procedure SetAutoSessionField(const Value:RtcString);
    procedure SetAutoSessions(const Value: boolean);
    procedure SetAutoSessionsLock(const Value: TRtcSessionLockType);

    function GetCrypt(Session:TRtcSession):TRtcCryptServer;
    procedure NewCrypt(Session:TRtcSession; m:TRtcServerCryptMode);
    procedure DelCrypt(Session:TRtcSession);

    function GetFunctionGroup: TRtcFunctionGroup;
    procedure SetFunctionGroup(const Value: TRtcFunctionGroup);
    function GetModuleFileName: RtcString;
    procedure SetModuleFileName(const Value: RtcString);
    function GetModuleHost: RtcString;
    procedure SetModuleHost(const Value: RtcString);
    procedure SetAutoEncrypt(const Value: integer);
    procedure SetForceEncrypt(const Value: boolean);
    procedure SetCompress(const Value: TRtcCompressLevel);
    procedure SetObjectLinkSupport(const Value: TRtcObjectLinkSupport);

    procedure DoExecute(Sender:TRtcConnection; var MyData:TRtcValueObject);
    procedure DoObjectCreate(Sender:TObject; Param:TRtcObjectCall);

  protected

    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure Call_ListenStart(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ListenStop(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_CheckRequest(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_RequestAccepted(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;

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
    procedure ProcessDataReceived(Sender:TRtcConnection); virtual;

    // @exclude
    function GetObjectManager(Sender:TRtcConnection):TRtcRemoteObjectManager; override;
    // @exclude
    procedure ActivateObjectManager(Sender:TRtcConnection; xCreate:boolean=True); override;
    // @exclude
    function MakeObjectManager(Sender:TRtcConnection; xCreate:boolean=True):TRtcRemoteObjectManager;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

  protected
    { Use this property to define what compression level you want to use when sending
      data from Server to client. Default Compression value is "cNone" (no compression).
      You can use different compression levels between client and server (for example,
      fast or no compression from client and max from server). If your server has to
      work with clients which don't support compression, you have to use "cNone". @html(<br><br>)

      If you have to work with Clients which support compression and those which don't,
      you should use two TRtcServerModule components (one with and one without compression)
      and configure Clients to use the correct TRtcServerModule (see "ModuleFileName" property). }
    property Compression:TRtcCompressLevel read FCompress write SetCompress default cNone;

    { Use this property to define what Data Formats you want to accept for this ServerModule.
      Since this is a set, you can choose one supported format or many supported formats. }
    property DataFormats:TRtcDataFormatSupport read FDataFormats write FDataFormats default [fmt_RTC];

    { Use this property to define what Data Request Modes you want to accept for this ServerModule.
      Since this is a set, you can choose which mode(s) you want your ServerModule to accept. }
    property DataReqModes:TRtcDataReqModeSupport read FDataReqModes write FDataReqModes default [req_contentBodyALL];

    { Set this property to a value other than 0 if you want to enable automatic
      Encryption for clients which have their EncryptionKey option activated.
      EncryptionKey value defines the length on the encryption key, in bytes.
      One byte represents encryption strength of 8 bits. To have 256-bit encryption,
      set EncryptionKey=32. @html(<br><br>)

      The final encryption key is combined from a key received by the client
      and a key generated by this ServerModule. When EncryptionKey is >0 by the
      ClientModule doing the call AND by this ServerModule, Encryption handshake will
      be done automatically by those two components, so that the user only has to set
      the values and use the components as if there is no encryption.

      If ServerModule's EncryptionKey property is 0 (zero), server will not allow data to be
      encrypted and all communication with all clients will flow without encryption.
      Clients which have ForceEncryption set to True, will not work if the server
      doesn't want to support encryption. If you need to work with Clients which require
      encryption and clients which don't, you should use two TRtcServerModule components. }
    property EncryptionKey:integer read FAutoEncrypt write SetAutoEncrypt default 0;

    { - Set "ObjectLinks" to "ol_None" (default) to completely disable the "RTC Linked Objects"
        feature for this RTC Server Module. When "ObjectLinks=ol_None", calling the
        "Sender.ActivateObjectManager;" method will also raise an exception
        because any RTC Linked Objects created this way would NOT be sent to the Client. @html(<br><br>)

        Because a single Server can potentially host any number of different Applications
        and handle requests from any number of different Clients, there is no reason why
        a single Server shouldn't have more than one "TRtcServerModule" component using the
        "RTC Linked Objects" feature. But, to keep the Client-side implementation simple,
        it is best to use only *one* "TRtcServerModule" per "Client Application" and
        customize each "TRtcServerModule" to specific needs of each Client Application. @html(<br><br>)

      - Set "ObjectLinks" to "ol_Manual" if you want to force the Client to call a remote
        function on the Server which will execute the "Sender.ActivateObjectManager;"
        method before any "Linked Objects" can be created (from Client or Server side). If there is no
        active Session and you use "ActiveObjectManager(True)", a new Session will also be created,
        after which a new Object Manager will be created and assigned to the Session. @html(<br><br>)

      - Set "ObjectLinks" to "ol_AutoClient" (or "ol_AutoBoth") if you want an Object Manager to be created
        automatically by the Server if "Linked Objects" data is received from a Client, allowing Clients to
        start creating Linked Objects without having to call a remote function and use "ActivateObjectManager"
        first (see "ol_Manual"). Please note that a Session is also required before an Object Manager can
        be created, since the Object Manager is stored inside a Session. If you also want the Session to
        be created automatically for the Client, set the "AutoSessions" property to TRUE.

      - Set "ObjectLinks" to "ol_AutoServer" (or "ol_AutoBoth") if an Object Manager should be created
        and/or activated automatically before each remote function linked to this Server Module gets executed,
        so there will be no need to explicitly call "ActivateObjectManager" from these functions OnExecute events.
        If "ObjectLinks" is "ol_AutoServer" (or "ol_AutoBoth"), because an Object Manager requires an active Session,
        the AutoSessions property also has to be TRUE for a Session to be created automatically, or a Session
        has to be opened manually by using a remote function linked to another Server Module.

      - Setting "ObjectLinks" to "ol_AutoBoth" is the equivalent of combining "ol_AutoClient" and "ol_AutoServer". }
    property ObjectLinks:TRtcObjectLinkSupport read FObjectLinkSupport write SetObjectLinkSupport default ol_None;

    { If you need a 100% secure connection, define a Secure Key String
      (combination of letters, numbers and special characters) for each
      ServerModule/ClientModule pair, additionally to the EncryptionKey value.
      ClientModule will be able to communicate with the ServerModule ONLY if
      they both use the same SecureKey. Default value for the SecureKey is
      an empty String (means: no secure key). @html(<br><br>)

      SecureKey will be used in the encryption initialisation handshake,
      to decrypt the first key combination received by the ClientModule.
      Since all other data packages are already sent using some kind of encryption,
      by defining a SecureKey, you encrypt the only key part which would have
      been sent out without special encryption. }
    property SecureKey:RtcString read FSecureKey write FSecureKey;

    { Setting this property to TRUE will tell the ServerModule to work with
      Clients ONLY if they requested to use encryption. If AutoEncryptKey is > 0 and
      Client doesn't request encryption, function calls will not be processed
      and any request coming from the client will be rejected, until
      client requests and initializes encryption. }
    property ForceEncryption:boolean read FForceEncrypt write SetForceEncrypt default False;

    { Set this property to TRUE if you want ClientModule's to be able to
      reqest a new session automatically by using the NEWID parameter,
      or if you want a Session to be created automatically when "Linked Objects"
      data is received and the "ObjectLinks" property is set to "ol_Auto*". @html(<br><br>)

      Session handling is built into the ClientModule and uses Request.Params to
      send the Session.ID to the server and Response.Cookie[AutoSessionField] to receive a
      new session ID from the server and initialize the session object. @html(<br><br>)

      Since session ID's are communicated automaticaly by the ClientModule and
      ServerModule components, all TRtcFunction and TRtcResult components used
      by this ClientModule will have direct access to the session object.
      When AutoSessions is set to TRUE, Client's can automaticaly request a new
      Session if no Session exists, or when a Session expires. @html(<br><br>)

      When AutoSessions is FALSE, you have to request a new Session by calling
      a remote server function to generate a new Session. Opening a Session from
      a remote function will notify the Client about the new Session (no need to
      manually send the Session ID to the Client as a result of a remote function). }
    property AutoSessions:boolean read FAutoSessions write SetAutoSessions default false;

    { For the ServerModule to automatically Lock a Session when a valid "Session ID" is
      received from the Client, and to support Automatic Sessions when "NEW" is received
      in the "Session ID" field from Clients, you have to define where this component should
      check for the "Session ID". [rsm_Query] = Request Query, [rsm_Cookie] = Request Cookie,
      or [rsm_Query, rsm_Cookie] = both, first in the Request Query and then Request Cookie. }
    property AutoSessionMode:TRtcSessionsModeSupport read FAutoSessionMode write FAutoSessionMode default [rsm_Query];

    { "AutoSessionField" property defines the Field Name in the Request "Query", Request "Cookie" 
      and  Response "Cookie" which ClientModule and ServerModule should use for the "Session ID".
      If not assigned, field name 'ID' will be used by default. This property has to be identical
      on TRtcClientModule and TRtcServerModule components for automatic Session handling to work. }
    property AutoSessionField:RtcString read FAutoSessionField write SetAutoSessionField;

    { Using this property, you define how long a session will live (in seconds)
      when there are no requests from this client and the session was
      created by a call from ClientModule that has its AutoSessions property enabled.
      The higher this value, the longer a session will stay valid when there
      are no requests coming from the client for which the session was created.
      This value will be used only after the Client has sent a valid request
      which produces a valid response from the server. Before that, a default
      Session Live time of @Link(RTC_SESSION_TIMEOUT) seconds will be used. @html(<br><br>)

      Session Live counter is reset each time a new request is received from the same client,
      so that this parameter only removes sessions which are inactive longer than
      AutoSessionsLive seconds. To keep your server from nasty clients creating tons of
      sessions and leaving them inactive, keep this property under 600 seconds,
      even if you want your session to stay alive for a long time. You do not have to
      overexagurate this value, since every session consumes memory and client sessions which are not
      needed will ONLY THEN be removed from memory when this AutoSessionsLive timeout expires. }
    property AutoSessionsLive:integer read FAutoSessionsLive write FAutoSessionsLive default 0;

    { When AutoSessions are used, you can define what Client data you want to use to
      lock the Sessions for this Client and deny access to Session data from other Clients. }
    property AutoSessionsLock:TRtcSessionLockType read FAutoSessionsLock write SetAutoSessionsLock default sesFwdLock;

    { Set 'AutoSessionCheck:=TRUE' if ... @html(<br>)
      (A) you have set "AutoSessions:=TRUE" and want this component to check every
          accepted Request as soon as the Request headers arrive to see if it contains
          a valid "Session ID" and make sure the Session will be Locked automatically
          BEFORE the Server starts receiving the rest of the Request, or if ...  @html(<br>)
      (B) you are manually creating RTC Sessions and have set "AutoSessions:=FALSE",
          but want this ServerModule to automatically Lock Sessions if a "Session ID"
          is received from the Client and you also want to automatically send a "Session ID"
          back to the Client (in a Response Cookie) if a Session was used by the Server.  @html(<br><br>)

      With "AutoSessionCheck=FALSE" (default), this component will check the "Session ID"
      ONLY if "AutoSessions=TRUE" and ONLY after receiving the entire Request content body. }
    property AutoSessionCheck:boolean read FAutoSessionCheck write FAutoSessionCheck default False;

    { If ModuleHost is specified, then Request.Host will be compared to the ModuleHost
      property to decide if this request should be processed by this ServerModule. @html(<br>)
      If your DataServer has to serve more different hosts, while your ServerModule
      is not supposed to react to requests from all those hosts, you can assign the
      host name to which this ServerModule belongs to. If ModuleHost is left blank,
      then this ServerModule will respond to any request asking for this servermodule's
      ModuleFileName, regardless of the HOST header. @html(<br><br>)

      To process all requests for a domain and all of its sub-domains, enter domain name ONLY
      (example: "realthinclient.com" for "realthinclient.com" and any sub-domain like
      "www.realthinclient.com", "mymod.myapp.realthinclient.com", etc). @html(<br>)
      To limit the requests to a sub-domain and its sub-sub-domains, enter the name
      of the highest sub-domain (example: "myapp.realthinclient.com" for
      "myapp.realthinclient.com" and any of its sub-domains like
      "mymod.myapp.realthinclient.com"). @html(<br>)
      To process ONLY requests pointed exactly to ONE HOST, add "." in front of your
      host name (example: ".realthinclient.com" will ONLY react to requests with
      "realthinclient.com" in their HOST header). }
    property ModuleHost:RtcString read GetModuleHost write SetModuleHost;

    { This property will be compared to Request.FileName to decide if the
      request we just received was pointed to this ServerModule. Any request asking
      for this FileName will be processed by this ServerModule component.
      Since parameters are passed to the server module through request's Content
      body (rather than headers), we do not need to check the request for anything
      else than it's FileName to know if the request is directed to this module. }
    property ModuleFileName:RtcString read GetModuleFileName write SetModuleFileName;

    { Set this property to tell the RtcServerModule to use this TRtcFunctionGroup
      component to execute all functions received as a request from clients. }
    property FunctionGroup:TRtcFunctionGroup read GetFunctionGroup write SetFunctionGroup;

    { This event is triggered when data is received from a remote "Object Manager".
      The main purpose of this event is to allow you to *monitor* all received "Linked Objects"
      packages without changing anything, but it could also be used to modify received data
      before it is forwarded to the local "Object Manager" for processing/execution.

      @param(Sender - Connection component through which data was received)
      @param(Data - Data received from remote "Object Manager".
             Unless you know exactly what you are doing and understand the format which
             local and remote "Object Manager" instances are using for communication,
             it is highly recommended that you do NOT make ANY changes to this instance.
             This is the instance that will be passed over to our "Object Manager" later) }
    property OnObjectDataIn:TRtcDataEvent read FOnObjectDataIn write FOnObjectDataIn;

    { This event is triggered *before* we send data prepared by our "Object Manager".
      The main purpose of this event is to allow you to *monitor* all "Linked Objects"
      packages before they are sent out, but it could also be used to modify prepared data.

      @param(Sender - Connection component which will be used to send the data out)
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

    { Called when a complete request has been received from the Client for THIS component
      and after the request content has been decrypted, decompressed and prepared for parsing,
      but BEFORE the request content body has been processed and removed from receiving buffers. @html(<br><br>)

      You can use this event for centralized logging and monitoring of all request
      contents received (and later processed) by this components, but do NOT use
      Read or ReadEx methods here, because they will immediately clear receiving buffers!
      To access the request content body from this event, use the "PeekEx" method instead. }
    property OnPeekRequest:TRtcNotifyEvent read FOnPeekRequest write FOnPeekRequest;

    { Event to be triggered after a new Session was opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { Event to be triggered before an existing Session was to be closed. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;
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
    { Event to be triggered after the response was sent out (Response.Done) }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { Event to be triggered when connection gets lost after a request was accepted.
      You can use this event for component deinitialization. }
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
    end;

  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcServerModule=class(TRtcBaseServerModule)
  published
    { Use this property to define what compression level you want to use when sending
      data from Server to client. Default Compression value is "cNone" (no compression).
      You can use different compression levels between client and server (for example,
      fast or no compression from client and max from server). If your server has to
      work with clients which don't support compression, you have to use "cNone". }
    property Compression;

    { Use this property to define what Data Formats you want to accept for this ServerModule.
      Since this is a set, you can choose one supported format or many/all supported formats. }
    property DataFormats;

    { Use this property to define what Data Request Formats you want to accept for this ServerModule.
      Since this is a set, you can choose one supported format or many/all supported formats. }
    property DataReqModes;

    { "AutoSessionField" property defines the Field Name in the Request "Query", Request "Cookie" 
      and  Response "Cookie" which ClientModule and ServerModule should use for the "Session ID".
      If not assigned, field name 'ID' will be used by default. This property has to be identical
      on TRtcClientModule and TRtcServerModule components for automatic Session handling to work. }
    property AutoSessionField;

    { - Set "ObjectLinks" to "ol_None" (default) to completely disable the "RTC Linked Objects"
        feature for this RTC Server Module. When "ObjectLinks=ol_None", calling the
        "Sender.ActivateObjectManager;" method will also raise an exception
        because any RTC Linked Objects created this way would NOT be sent to the Client. @html(<br><br>)

        Because a single Server can potentially host any number of different Applications
        and handle requests from any number of different Clients, there is no reason why
        a single Server shouldn't have more than one "TRtcServerModule" component using the
        "RTC Linked Objects" feature. But, to keep the Client-side implementation simple,
        it is best to use only *one* "TRtcServerModule" per "Client Application" and
        customize each "TRtcServerModule" to specific needs of each Client Application. @html(<br><br>)

      - Set "ObjectLinks" to "ol_Manual" if you want to force the Client to call a remote
        function on the Server which will execute the "Sender.ActivateObjectManager;"
        method before any "Linked Objects" can be created (from Client or Server side). If there is no
        active Session and you use "ActiveObjectManager(True)", a new Session will also be created,
        after which a new Object Manager will be created and assigned to the Session. @html(<br><br>)

      - Set "ObjectLinks" to "ol_AutoClient" (or "ol_AutoBoth") if you want an Object Manager to be created
        automatically by the Server if "Linked Objects" data is received from a Client, allowing Clients to
        start creating Linked Objects without having to call a remote function and use "ActivateObjectManager"
        first (see "ol_Manual"). Please note that a Session is also required before an Object Manager can
        be created, since the Object Manager is stored inside a Session. If you also want the Session to
        be created automatically for the Client, set the "AutoSessions" property to TRUE.

      - Set "ObjectLinks" to "ol_AutoServer" (or "ol_AutoBoth") if an Object Manager should be created
        and/or activated automatically before each remote function linked to this Server Module gets executed,
        so there will be no need to explicitly call "ActivateObjectManager" from these functions OnExecute events.
        If "ObjectLinks" is "ol_AutoServer" (or "ol_AutoBoth"), because an Object Manager requires an active Session,
        the AutoSessions property also has to be TRUE for a Session to be created automatically, or a Session
        has to be opened manually by using a remote function linked to another Server Module.

      - Setting "ObjectLinks" to "ol_AutoBoth" is the equivalent of combining "ol_AutoClient" and "ol_AutoServer". }
    property ObjectLinks;

    { Set this property to a value other than 0 if you want to enable automatic
      Encryption for clients which have their EncryptionKey option activated.
      EncryptionKey value defines the length on the encryption key, in bytes.
      One byte represents encryption strength of 8 bits. To have 256-bit encryption,
      set EncryptionKey=32. @html(<br><br>)

      The final encryption key is combined from a key received by the client
      and a key generated by this ServerModule. When EncryptionKey is >0 by the
      ClientModule doing the call AND by this ServerModule, Encryption handshake will
      be done automatically by those two components, so that the user only has to set
      the values and use the components as if there is no encryption.

      If ServerModule's EncryptionKey property is 0 (zero), server will not allow data to be
      encrypted and all communication with all clients will flow without encryption.
      Clients which have ForceEncryption set to True, will not work if the server
      doesn't want to support encryption. }
    property EncryptionKey;

    { If you need a 100% secure connection, define a Secure Key String
      (combination of letters, numbers and special characters) for each
      ServerModule/ClientModule pair, additionally to the EncryptionKey value.
      ClientModule will be able to communicate with the ServerModule ONLY if
      they both use the same SecureKey. Default value for the SecureKey is
      an empty String (means: no secure key). @html(<br><br>)

      SecureKey will be used in the encryption initialisation handshake,
      to decrypt the first key combination received by the ClientModule.
      Since all other data packages are already sent using some kind of encryption,
      by defining a SecureKey, you encrypt the only key part which would have
      been sent out without special encryption. }
    property SecureKey;

    { Setting this property to TRUE will tell the ServerModule to work with
      Clients ONLY if they requested to use encryption. If AutoEncryptKey is > 0 and
      Client doesn't request encryption, function calls will not be processed
      and any request coming from the client will be rejected, until
      client requests and initializes encryption. }
    property ForceEncryption;

    { Set this property to TRUE if you want ClientModule's to be able to
      reqest a new session automatically by using the NEWID parameter.
      Session handling is built into the ClientModule and uses Request.Params to
      send the Session.ID to the server and Response.Cookie[AutoSessionField] to receive a
      new session ID from the server and initialize the session object. @html(<br><br>)

      Since session ID's are communicated automaticaly by the ClientModule and
      ServerModule components, all TRtcFunction and TRtcResult components used by
      this ClientModule will have direct access to the session object.
      When AutoSessions is set to true, Client's can automaticaly request a new
      session is no session exists or when a session expires. @html(<br><br>)

      When AutoSessions is FALSE, you have to request a new session by calling
      a remote server function to generate a new session and return the session ID. }
    property AutoSessions;

    { For the ServerModule to automatically Lock a Session when a valid "Session ID" is
      received from the Client, and to support Automatic Sessions when "NEW" is received
      in the "Session ID" field from Clients, you have to define where this component should
      check for the "Session ID". [rsm_Query] = Request Query, [rsm_Cookie] = Request Cookie,
      or [rsm_Query, rsm_Cookie] = both, first in the Request Query and then Request Cookie. }
    property AutoSessionMode;

    { Using this property, you define how long a session will live (in seconds)
      when there are no requests from this client and the session was
      created by a call from ClientModule that has its AutoSessions property enabled.
      The higher this value, the longer a session will stay valid when there
      are no requests coming from the client for which the session was created.
      This value will be used only after the Client has sent a valid request
      which produces a valid response from the server. Before that, a default
      Session Live time of @Link(RTC_SESSION_TIMEOUT) seconds will be used. @html(<br><br>)

      Session Live counter is reset each time a new request is received from the same client,
      so that this parameter only removes sessions which are inactive longer than
      AutoSessionsLive seconds. To keep your server from nasty clients creating tons of
      sessions and leaving them inactive, keep this property under 600 seconds,
      even if you want your session to stay alive for a long time. You do not have to
      overexagurate this value, since every session consumes memory and client sessions which are not
      needed will ONLY THEN be removed from memory when this AutoSessionsLive timeout expires. }
    property AutoSessionsLive;

    { When AutoSessions are used, you can define what Client data you want to use to
      lock the Sessions for this Client and deny access to Session data from other Clients. }
    property AutoSessionsLock;

    { Set 'AutoSessionCheck:=TRUE' if ... @html(<br>)
      (A) you have set "AutoSessions:=TRUE" and want this component to check every
          accepted Request as soon as the Request headers arrive to see if it contains
          a valid "Session ID" and make sure the Session will be Locked automatically
          BEFORE the Server starts receiving the rest of the Request, or if ...  @html(<br>)
      (B) you are manually creating RTC Sessions and have set "AutoSessions:=FALSE",
          but want this ServerModule to automatically Lock Sessions if a "Session ID"
          is received from the Client and you also want to automatically send a "Session ID"
          back to the Client (in a Response Cookie) if a Session was used by the Server.  @html(<br><br>)

      With "AutoSessionCheck=FALSE" (default), this component will check the "Session ID"
      ONLY if "AutoSessions=TRUE" and ONLY after receiving the entire Request content body. }
    property AutoSessionCheck;

    { If ModuleHost is specified, then Request.Host will be compared to the ModuleHost
      property to decide if this request should be processed by this ServerModule. @html(<br>)
      If your DataServer has to serve more different hosts, while your ServerModule
      is not supposed to react to requests from all those hosts, you can assign the
      host name to which this ServerModule belongs to. If ModuleHost is left blank,
      then this ServerModule will respond to any request asking for this servermodule's
      ModuleFileName, regardless of the HOST header. @html(<br><br>)

      To process all requests for a domain and all of its sub-domains, enter domain name ONLY
      (example: "realthinclient.com" for "realthinclient.com" and any sub-domain like
      "www.realthinclient.com", "mymod.myapp.realthinclient.com", etc). @html(<br>)
      To limit the requests to a sub-domain and its sub-sub-domains, enter the name
      of the highest sub-domain (example: "myapp.realthinclient.com" for
      "myapp.realthinclient.com" and any of its sub-domains like
      "mymod.myapp.realthinclient.com"). @html(<br>)
      To process ONLY requests pointed exactly to ONE HOST, add "." in front of your
      host name (example: ".realthinclient.com" will ONLY react to requests with
      "realthinclient.com" in their HOST header). }
    property ModuleHost;

    { This property will be compared to Request.FileName to decide if the
      request we just received was pointed to this ServerModule. Any request asking
      for this FileName will be processed by this ServerModule component.
      Since parameters are passed to the server module through request's Content
      body (rather than headers), we do not need to check the request for anything
      else than it's FileName to know if the request is directed to this module. }
    property ModuleFileName;
    
    { Set this property to tell the RtcServerModule to use this TRtcFunctionGroup
      component to execute all functions received as a request from clients. }
    property FunctionGroup;

    { This event is triggered when data is received from a remote "Object Manager".
      The main purpose of this event is to allow you to *monitor* all received "Linked Objects"
      packages without changing anything, but it could also be used to modify received data
      before it is forwarded to the local "Object Manager" for processing/execution.

      @param(Sender - NIL, or the connection component through which data was received)
      @param(Data - Data received from remote "Object Manager".
             Unless you know exactly what you are doing and understand the format which
             local and remote "Object Manager" instances are using for communication,
             it is highly recommended that you do NOT make ANY changes to this instance.
             This is the instance that will be passed over to our "Object Manager" later) }
    property OnObjectDataIn;

    { This event is triggered *before* we send data prepared by our "Object Manager".
      The main purpose of this event is to allow you to *monitor* all "Linked Objects"
      packages before they are sent out, but it could also be used to modify prepared data.

      @param(Sender - NIL if using the default connection; "Sender" parameter for the Call method)
      @param(Data - Data prepared by our local "Object Manager" for sending to remote "Object Manager".
             Unless you know exactly what you are doing and understand the format which
             local and remote "Object Manager" instances are using for communication,
             it is highly recommended that you do NOT make ANY changes to this instance.
             This is the instance that will be sent over to remote "Object Manager") }
    property OnObjectDataOut;

    { This event is triggered when the remote Object Manager has requested
      our Object Manager to create a new Object (remote instance was already created)
      and allows you to create Objects which don't have a global constructor
      registered (through the global "RegisterRtcObjectConstructor" procedure).

      Objects which you do NOT want to have created automatically by the remote
      side, but where you still want to allow controlled creation should NOT have
      a global constructor registered and should be created from THIS event instead. }
    property OnObjectCreate;

    { Called when a complete request has been received from the Client for THIS component
      and after the request content has been decrypted, decompressed and prepared for parsing,
      but BEFORE the request content body has been processed and removed from receiving buffers. @html(<br><br>)

      You can use this event for centralized logging and monitoring of all request
      contents received (and later processed) by this components, but do NOT use
      Read or ReadEx methods here, because they will immediately clear receiving buffers!
      To access the request content body from this event, use the "PeekEx" method instead. }
    property OnPeekRequest;

    { Event to be triggered after a new Session was opened. }
    property OnSessionOpen;
    { Event to be triggered before an existing Session was to be closed. }
    property OnSessionClose;
    { Event to be triggered when the Server starts listening on the connection.
      You can use this event for component initialization. }
    property OnListenStart;
    { Event to be triggered when the Server stops listening on the connection.
      You can use this event for component deinitialization. }
    property OnListenStop;
    { Event to be triggered when a child DataProvider component accepts the Request.
      You can use this event for request initialization. For example,
      you could create a DataTunel and assign it to Tunel, to have reuqest data tunneled. }
    property OnRequestAccepted;
    { Event to be triggered after the response was sent out (Response.Done) }
    property OnResponseDone;
    { Event to be triggered when connection gets lost after a request was accepted.
      You can use this event for component deinitialization. }
    property OnDisconnect;
    end;

{ Post a function call to "Event" using "Param" parameters, delayed for "msDelay" miliseconds.
  If you need to make changes to parameters, do it BEFORE calling PostDelayedCall.
  This procedure ONLY works if  called from a function which was called by TRtcBaseServerModule. }
procedure PostDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent); overload;

{ Prepare a function call to "Event" using "Param" paremeters, which should be delayed for "msDelay" miliseconds.
  Use "PostDelayedCall" with the object returned from PrepareDelayedCall (TRtcDelayedCall) to post this call.
  You can also use the Result (TRtcDelayedCall) to execute the delayed call sooner (from any thread) by calling "TRtcDelayedCall.WakeUp".
  You can NOT CANCEL THE CALL after you have Posted it with PostDelayedCall.
  If connection should get lost while a Delayed Call is waiting, delayed call will be canceled automaticaly.
  If you need to make any changes to the parameters passed to the delayed call,
  you have to do it BEFORE using PrepareDelayedCall, because PrepareDelayedCall creates a copy of all parameters
  and any change to the original Params will not be reflected in that copy later. }
function PrepareDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent):TRtcDelayedCall;

{ Post delayed call which was prepared using PrepareDelayedCall.
  You can use the Result (TRtcDelayedCall) to execute the delayed call sooner (from any thread) by using the "TRtcDelayedCall.WakeUp" method.
  You can NOT CANCEL THE CALL after you have Posted it by using PortDelayedCall, you can only Wake it up sooner.
  If connection should get lost while a Delayed Call is waiting, delayed call will be canceled automaticaly. }
procedure PostDelayedCall(cb:TRtcDelayedCall); overload;

{ If you prepared a delayed call, but do not want to Post it anymore,
  use this procedure to cancel it before calling PostDelayedCall. @html(<br>)
  DO NOT CALL THIS PROCEDURE AFTER YOU HAVE POSTED THE CALL!!!!
  You can NOT CANCEL THE CALL after you have Posted it (with PostDelayedCall).
  If connection should get lost while a Delayed Call is waiting, delayed call will be canceled automaticaly. }
procedure CancelDelayedCall(cb:TRtcDelayedCall);

{ Call "WakeUp" on all "DelayedCall" objects }
procedure WakeUpAllDelayedCalls;

implementation

const
  SERVE_XML='$XML.';
  SERVE_JSON='$JSON.';
  SERVE_JSON_ID='$JSON.ID.';

  RTC_ERROR_INVALIDREQUEST=-32600;
  RTC_ERROR_BADFUNCTION=-32601;
  RTC_ERROR_PARSE=-32700;
  RTC_ERROR_INTERNAL=-32603;

  RTC_ERROR_INVALIDRESPONSE=RTC_ERROR_INTERNAL;
  RTC_ERROR_DISCONNECT=RTC_ERROR_INTERNAL;

{ Prepare a function call to "Event" using "Param" paremeters, which should be delayed for "msDelay" miliseconds.
  Use "PostDelayedCall" to post this delayed call, after you've stored the Result returned by this function.
  You can use the Result (TRtcDelayedCall) to execute the delayed call sooner (from any thread) by using the "TRtcDelayedCall.Call" method. }
function PrepareDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent):TRtcDelayedCall;
  begin
  Result:=TRtcDelayedCall.Create;
  Result.msDelay:=msDelay;
  Result.Param:=TRtcFunctionInfo(Param.copyOf);
  Result.CallEvent:=Event;
  end;

{ Post delayed call which was prepared using PrepareDelayedCall. }
procedure PostDelayedCall(cb:TRtcDelayedCall); overload;
  var
    e:EDelayedCall;
  begin
  e:=EDelayedCall.Create('');
  e.call:=cb;
  raise e;
  end;

procedure PostDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent);
  var
    e:EDelayedCall;
  begin
  e:=EDelayedCall.Create('');
  e.call:=PrepareDelayedCall(msDelay,Param,Event);
  raise e;
  end;

{ TRtcBaseServerModule }

constructor TRtcBaseServerModule.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FDataFormats:=[fmt_RTC];
  FDataReqModes:=[req_contentBodyALL];
  FAutoSessionField:='';
  FAutoSessionFieldUPC:='';
  FObjectLinkSupport:=ol_None;
  FAutoSessions:=False;
  FAutoSessionsLock:=sesFwdLock;
  FAutoSessionCheck:=False;
  FAutoSessionMode:=[rsm_Query];
  FFunctions:=nil;
  FModuleFileName:='';
  FModuleFileREST:='';
  FModuleHost:='';
  FCryptSesName:='.$CRYPT$';
  FObjManSesName:=RTCL_SRVSESSION;
  end;

destructor TRtcBaseServerModule.Destroy;
  begin
  try
    FFunctions:=nil;
    FModuleFileName:='';
    FModuleFileREST:='';
    FModuleHost:='';
    FCryptSesName:='';
    FObjManSesName:='';
    FAutoSessionField:='';
    FAutoSessionFieldUPC:='';
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcBaseServerModule.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcBaseServerModule.SetAutoSessionField(const Value:RtcString);
  begin
  FAutoSessionField:=URL_Encode(Value);
  FAutoSessionFieldUPC:=Upper_Case(FAutoSessionField);
  end;

function TRtcBaseServerModule.SessIDField:RtcString;
  begin
  if FAutoSessionField='' then
    Result:='ID'
  else
    Result:=FAutoSessionField;
  end;

function TRtcBaseServerModule.SessIDFieldCS:RtcString;
  begin
  if FAutoSessionFieldUPC='' then
    Result:='ID'
  else
    Result:=FAutoSessionFieldUPC;
  end;

procedure TRtcBaseServerModule.Call_CheckRequest(Sender: TRtcConnection);
  function haveREST:boolean;
    var
      len,len2,i:integer;
    begin
    if (DataReqModes<>[req_contentBodyALL]) and
       (DataReqModes<>[]) then
      begin
      len2:=length(FModuleFileREST);
      if len2=0 then
        Result:=False
      else
        begin
        len:=length(Sender.Request.FileName);
        if len<=len2 then
          Result:=False
        else
          begin
          Result:=True;
          with Sender.Request do
            for i:=1 to len2 do
              if FileName[i]<>FModuleFileREST[i] then
                begin
                Result:=False;
                Break;
                end;
          end;
        end;
      end
    else
      Result:=False;
    end;
  begin
  TRtcDataServer(Sender).SetActiveLink(self);
  with Sender do
    begin
    if (Request.FileName=FModuleFileName) or haveREST then
      begin
      if FModuleHost<>'' then
        begin
        if Copy(FModuleHost,1,1)<>'.' then // accepting domain with any sub-domain
          begin
          if Same_Text(Request.Host,FModuleHost) then // host = domain name
            Accept
          else if ( (length(Request.Host)>length(FModuleHost)) and // could be sub-domain
                    (Copy(Request.Host,length(Request.Host)-length(FModuleHost),1)='.') and // has '.' in the right place
                    Same_Text(Copy(Request.Host,length(Request.Host)-length(FModuleHost)+1,length(FModuleHost)),FModuleHost) ) then // is sub-domain
            Accept;
          end
        else if Same_Text(Request.Host,Copy(FModuleHost,2,length(FModuleHost)-1)) then
          Accept; // accepting a specific sub-domain only
        end
      else // Accept the request. It has our ModuleFileName as Request.FileName, we accept all hosts
        Accept;
      end;
    end;
  end;

procedure TRtcBaseServerModule.Call_ListenStart(Sender: TRtcConnection);
  begin
  if FModuleFileName='' then
    ModuleFileName:='/';
  if assigned(FOnListenStart) then
    FOnListenStart(Sender);
  end;

procedure TRtcBaseServerModule.Call_ListenStop(Sender: TRtcConnection);
  begin
  if assigned(FOnListenStop) then
    FOnListenStop(Sender);
  end;

procedure TRtcBaseServerModule.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionClose) then
    FOnSessionClose(Sender);
  end;

procedure TRtcBaseServerModule.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionOpen) then
    FOnSessionOpen(Sender);
  end;

procedure TRtcBaseServerModule.Call_RequestAccepted(Sender: TRtcConnection);
  begin
  try
    if assigned(FOnRequestAccepted) then
      FOnRequestAccepted(Sender);

    if assigned(Link) then
      Link.Call_RequestAccepted(Sender);
  except
    on E:Exception do
      with TRtcValueResult.Create(nil) do
        try
          asException:=RtcWideString(E.Message);
          if Sender.Request.Info.isType[SERVE_XML]=rtc_Integer then
            begin
            if asErrorCode=0 then
              asErrorCode:=RTC_ERROR_INVALIDREQUEST;
            reqVer:=Sender.Request.Info.asInteger[SERVE_XML];
            if reqVer=0 then
              Sender.WriteEx(toXMLRESTResponseEx)
            else
              Sender.WriteEx(toXMLrpcResponseEx);
            end
          else if Sender.Request.Info.isType[SERVE_JSON]=rtc_Integer then
            begin
            if asErrorCode=0 then
              asErrorCode:=RTC_ERROR_INVALIDREQUEST;
            reqVer:=Sender.Request.Info.asInteger[SERVE_JSON];
            reqID:=Sender.Request.Info.asText[SERVE_JSON_ID];
            Sender.WriteEx(toJSONEx);
            end
          else
            Sender.WriteEx(toCodeEx);
        finally
          Free;
        end;
    end;
  end;

procedure TRtcBaseServerModule.Call_ResponseDone(Sender: TRtcConnection);
  begin
  try
    if assigned(FOnResponseDone) then
      FOnResponseDone(Sender);

    if assigned(Link) then
      Link.Call_ResponseDone(Sender);
  except
    on E:Exception do
      with TRtcValueResult.Create(nil) do
        try
          asException:=RtcWideString(E.Message);
          if Sender.Request.Info.isType[SERVE_XML]=rtc_Integer then
            begin
            if asErrorCode=0 then
              asErrorCode:=RTC_ERROR_INVALIDRESPONSE;
            reqVer:=Sender.Request.Info.asInteger[SERVE_XML];
            if reqVer=0 then
              Sender.WriteEx(toXMLRESTResponseEx)
            else
              Sender.WriteEx(toXMLrpcResponseEx);
            end
          else if Sender.Request.Info.isType[SERVE_JSON]=rtc_Integer then
            begin
            if asErrorCode=0 then
              asErrorCode:=RTC_ERROR_INVALIDRESPONSE;
            reqVer:=Sender.Request.Info.asInteger[SERVE_JSON];
            reqID:=Sender.Request.Info.asText[SERVE_JSON_ID];
            Sender.WriteEx(toJSONEx);
            end
          else
            Sender.WriteEx(toCodeEx);
        finally
          Free;
        end;
    end;
  end;

procedure TRtcBaseServerModule.Call_Disconnect(Sender: TRtcConnection);
  begin
  try
    if assigned(FOnDisconnect) then
      FOnDisconnect(Sender);

    if assigned(Link) then
      Link.Call_Disconnect(Sender);
  except
    on E:Exception do
      with TRtcValueResult.Create(nil) do
        try
          asException:=RtcWideString(E.Message);
          if Sender.Request.Info.isType[SERVE_XML]=rtc_Integer then
            begin
            if asErrorCode=0 then
              asErrorCode:=RTC_ERROR_DISCONNECT;
            reqVer:=Sender.Request.Info.asInteger[SERVE_XML];
            if reqVer=0 then
              Sender.WriteEx(toXMLRESTResponseEx)
            else
              Sender.WriteEx(toXMLrpcResponseEx);
            end
          else if Sender.Request.Info.isType[SERVE_JSON]=rtc_Integer then
            begin
            if asErrorCode=0 then
              asErrorCode:=RTC_ERROR_DISCONNECT;
            reqVer:=Sender.Request.Info.asInteger[SERVE_JSON];
            reqID:=Sender.Request.Info.asText[SERVE_JSON_ID];
            Sender.WriteEx(toJSONEx);
            end
          else
            Sender.WriteEx(toCodeEx);
        finally
          Free;
        end;
    end;
  end;

procedure CryptReadEx(Crypt:TRtcCryptServer; var Data:RtcByteArray);
  begin
  if assigned(Crypt) then
    if assigned(Crypt.CRead) then
      Crypt.CRead.DeCryptEx(Data)
  {$IFDEF RTC_RSA}
    else if assigned(Crypt.CReadEx) then
      Crypt.CReadEx.DeCrypt(Data){$ENDIF};
  end;

procedure CryptWriteEx(Crypt:TRtcCryptServer; var Data:RtcByteArray);
  begin
  if assigned(Crypt) then
    if assigned(Crypt.CWrite) then
      Crypt.CWrite.CryptEx(Data)
  {$IFDEF RTC_RSA}
    else if assigned(Crypt.CWriteEx) then
      Crypt.CWriteEx.Crypt(Data){$ENDIF};
  end;

function TRtcBaseServerModule.GetObjectManager(Sender:TRtcConnection): TRtcRemoteObjectManager;
  begin
  if FObjectLinkSupport=ol_None then
    Result:=nil
  else if not assigned(Sender.Session) then
    Result:=nil
  else if not assigned(Sender.Session._asObj[FObjManSesName]) then
    Result:=nil
  else if Sender.Session._asObj[FObjManSesName] is TRtcServerObjectManager then
    Result:=TRtcServerObjectManager(Sender.Session._asObj[FObjManSesName])
  else
    raise ERtcObjectLinks.Create('Session.asObj['+String(FObjManSesName)+'] is '+
                                  Sender.Session._asObj[FObjManSesName].ClassName+
                                  ', but should be TRtcServerObjectManager.');
  end;

procedure TRtcBaseServerModule.ActivateObjectManager(Sender:TRtcConnection; xCreate:boolean=True);
  var
    om:TRtcServerObjectManager;
  begin
  om:=TRtcServerObjectManager(MakeObjectManager(Sender,xCreate));
  if assigned(om) then
    begin
    SetRtcObjectManager(om);
    om.ExecuteBroadcast(Sender);
    end
  else
    raise ERtcObjectLinks.Create('"ActivateObjectManager" failed: Object Manager unavailable.');
  end;

function TRtcBaseServerModule.MakeObjectManager(Sender:TRtcConnection; xCreate:boolean=True):TRtcRemoteObjectManager;
  begin
  if FObjectLinkSupport=ol_None then
    raise ERtcObjectLinks.Create('ActiveObjectManager: ObjectLinks property = ol_None')
  else if not assigned(Sender.Session) then
    begin
    if xCreate then
      begin
      Sender.OpenSession(AutoSessionsLock);
      Sender.Session.KeepAlive:=AutoSessionsLive;
      Sender.Response.Cookie.Value[SessIDField]:=Sender.Session.ID;
      Result:=TRtcServerObjectManager.Create(True);
      Result.BroadcastGroup:=RtcWideString(FObjManSesName);
      Sender.Session._asObj[FObjManSesName]:=Result;
      end
    else
      raise ERtcObjectLinks.Create('ActivateObjectManager: No active Session.');
    end
  else if not assigned(Sender.Session._asObj[FObjManSesName]) then
    begin
    if xCreate then
      begin
      Result:=TRtcServerObjectManager.Create(True);
      Result.BroadcastGroup:=RtcWideString(FObjManSesName);
      Sender.Session._asObj[FObjManSesName]:=Result;
      end
    else
      raise ERtcObjectLinks.Create('ActivateObjectManager: Active Session does NOT have an Object Manager assigned.');
    end
  else if Sender.Session._asObj[FObjManSesName] is TRtcServerObjectManager then
    Result:=TRtcServerObjectManager(Sender.Session._asObj[FObjManSesName])
  else
    raise ERtcObjectLinks.Create('Session.asObj['+String(FObjManSesName)+'] is '+
                                  Sender.Session._asObj[FObjManSesName].ClassName+
                                  ', but should be TRtcServerObjectManager.');
  if not assigned(Result) then
    raise ERtcObjectLinks.Create('"ActivateObjectManager" failed: Object Manager unavailable.');
  end;

procedure TRtcBaseServerModule.DoExecute(Sender:TRtcConnection; var MyData:TRtcValueObject);
  var
    MyResult,TmpData,TmpRes:TRtcValueObject;
    TmpMan:TRtcObjectManager;
    ObjMan:TRtcServerObjectManager;
    xData:TRtcValue;
  begin
  if isSimpleValue(MyData) then
    begin
    if FObjectLinkSupport in [ol_AutoServer, ol_AutoBoth] then
      begin
      if FAutoSessions or assigned(Sender.Session) then
        ObjMan:=TRtcServerObjectManager(MakeObjectManager(Sender,True))
      else
        ObjMan:=nil;
      end
    else if FObjectLinkSupport<>ol_None then
      ObjMan:=TRtcServerObjectManager(Sender.GetObjectManager)
    else
      ObjMan:=nil;
    if assigned(ObjMan) then
      begin
      xData:=ObjMan.GetBroadcast;
      if assigned(xData) then
        try
          TmpMan:=SetRtcObjectManager(ObjMan);
          try
            ObjMan.ExecuteBroadcast(Sender,xData);
          finally
            SetRtcObjectManager(TmpMan);
            end;
        finally
          RtcFreeAndNil(xData);
          end;
      if not ObjMan.Idle then
        begin
        xData:=ObjMan.GetData;
        if assigned(FOnObjectDataOut) then
          FOnObjectDataOut(Sender,xData);
        TmpData:=TRtcFunctionInfo.Create;
        with TRtcFunctionInfo(TmpData) do
          begin
          FunctionName:=RTCL_FUNCTION;
          asObject[RTCL_RESULT]:=MyData;
          asObject[RTCL_DATA]:=xData;
          end;
        MyData:=TmpData;
        end;
      end;
    end
  else if FObjectLinkSupport=ol_None then
    begin
    if assigned(FFunctions) then
      begin
      MyResult:=FFunctions.ExecuteData(Sender, MyData);
      if MyData<>MyResult then
        begin
        RtcFreeAndNil(MyData);
        MyData:=MyResult;
        end;
      end
    else
      raise Exception.Create('FunctionGroup property undefined, can not execute call.');
    end
  else if (TRtcValue(MyData).isType=rtc_Function) and
          (TRtcValue(MyData).asFunction.FunctionName=RTCL_FUNCTION) then
    begin
    TmpData:=TRtcValue(MyData).asFunction.asObject[RTCL_DATA];
    ObjMan:=TRtcServerObjectManager(Sender.GetObjectManager);
    if not assigned(TmpData) then
      begin
      RtcFreeAndNil(MyData);
      MyData:=TRtcExceptionValue.Create('No data received.');
      end
    else
      begin
      if not assigned(ObjMan) then
        begin
        if FObjectLinkSupport in [ol_AutoClient, ol_AutoBoth] then
          begin
          if not assigned(Sender.Session) and (AutoSessions=False) then
            begin
            RtcFreeAndNil(MyData);
            MyData:=TRtcExceptionValue.Create('Session not found, can not create Object Manager.');
            Exit;
            end
          else
            ObjMan:=TRtcServerObjectManager(MakeObjectManager(Sender,True));
          end
        else
          begin
          RtcFreeAndNil(MyData);
          MyData:=TRtcExceptionValue.Create('Object Manager not found.');
          Exit;
          end;
        end;
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
        xData:=ObjMan.GetBroadcast;
        if assigned(xData) then
          try
            TmpMan:=SetRtcObjectManager(ObjMan);
            try
              ObjMan.ExecuteBroadcast(Sender,xData);
            finally
              SetRtcObjectManager(TmpMan);
              end;
          finally
            RtcFreeAndNil(xData);
            end;
        if not ObjMan.Idle then
          begin
          xData:=ObjMan.GetData;
          if assigned(FOnObjectDataOut) then
            FOnObjectDataOut(Sender,xData);
          TmpData:=TRtcFunctionInfo.Create;
          with TRtcFunctionInfo(TmpData) do
            begin
            FunctionName:=RTCL_FUNCTION;
            asObject[RTCL_RESULT]:=TmpRes;
            asObject[RTCL_DATA]:=xData;
            end;
          TmpRes:=TmpData;
          end;
        RtcFreeAndNil(MyData);
        if assigned(TmpRes) then
          MyData:=TmpRes
        else
          MyData:=TRtcValue.Create;
      finally
        SetRtcObjectManager(TmpMan);
        end;
      end;
    end
  else if assigned(FFunctions) then
    begin
    if (FObjectLinkSupport in [ol_AutoServer, ol_AutoBoth]) then
      begin
      if FAutoSessions or assigned(Sender.Session) then
        begin
        ObjMan:=TRtcServerObjectManager(MakeObjectManager(Sender,True));
        TmpMan:=SetRtcObjectManager(ObjMan);
        try
          if assigned(ObjMan) then
            ObjMan.ExecuteBroadcast(Sender);
          MyResult:=FFunctions.ExecuteData(Sender,MyData);
        finally
          SetRtcObjectManager(TmpMan);
          end;
        end
      else
        MyResult:=TRtcExceptionValue.Create('ObjectLinks=ol_AutoServer; No active Session and AutoSessions=False.');
      end
    else
      begin
      TmpMan:=CheckRtcObjectManager;
      try
        MyResult:=FFunctions.ExecuteData(Sender, MyData);
      finally
        SetRtcObjectManager(TmpMan); // return the original object manager
        end;
      end;
    if MyData<>MyResult then
      begin
      RtcFreeAndNil(MyData);
      MyData:=MyResult;
      end;
    ObjMan:=TRtcServerObjectManager(Sender.GetObjectManager);
    if assigned(ObjMan) then
      begin
      xData:=ObjMan.GetBroadcast;
      if assigned(xData) then
        try
          TmpMan:=SetRtcObjectManager(ObjMan);
          try
            ObjMan.ExecuteBroadcast(Sender,xData);
          finally
            SetRtcObjectManager(TmpMan);
            end;
        finally
          RtcFreeAndNil(xData);
          end;
      if not ObjMan.Idle then
        begin
        xData:=ObjMan.GetData;
        if assigned(FOnObjectDataOut) then
          FOnObjectDataOut(Sender,xData);
        TmpData:=TRtcFunctionInfo.Create;
        with TRtcFunctionInfo(TmpData) do
          begin
          FunctionName:=RTCL_FUNCTION;
          asObject[RTCL_RESULT]:=MyData;
          asObject[RTCL_DATA]:=xData;
          end;
        MyData:=TmpData;
        end;
      end;
    end
  else
    raise Exception.Create('FunctionGroup property undefined, can not execute call.');
  end;

procedure TRtcBaseServerModule.Call_DataReceived(Sender: TRtcConnection);
  begin
  with Sender do
    if Request.Started and AutoSessions and AutoSessionCheck then
      if (rsm_Query in AutoSessionMode) and
         (Request.Query.ValueCS[SessIDFieldCS]='NEW') then
        begin
        // MySessionID:='NEW';
        end
      else if (rsm_Query in AutoSessionMode) and
              (length(Request.Query.ValueCS[SessIDFieldCS])=RTC_SESSIONID_LENGTH) then
        begin
        // Need to handle now, because there could be a lot of data coming,
        // during which our Session could time out.
        FindSession(Request.Query.ValueCS[SessIDFieldCS]);
        end
      else if (rsm_Cookie in AutoSessionMode) and
              (Request.Cookie.ValueCS[SessIDFieldCS]='NEW') then
        begin
        // MySessionID:='NEW';
        end
      else if (rsm_Cookie in AutoSessionMode) and
              (length(Request.Cookie.ValueCS[SessIDFieldCS])=RTC_SESSIONID_LENGTH) then
        begin
        // Need to handle now, because there could be a lot of data coming,
        // during which our Session could time out.
        FindSession(Request.Cookie.ValueCS[SessIDFieldCS]);
        end;

  if assigned(Link) then
    Link.Call_PeekReceived(Sender);

  if Sender.Request.Complete then
    ProcessDataReceived(Sender);
  end;

procedure TRtcBaseServerModule.ProcessDataReceived(Sender: TRtcConnection);
  var
    cmode:TRtcServerCryptMode;
    crypt:TRtcCryptServer;
    tmpcnt:integer;
    code,tmp:RtcByteArray;
    data:RtcString;
    cmd,ctype,temp:RtcString;
    output:TRtcHugeString;
    at,atLast:integer;
    MyData:TRtcValueObject;
    mycall:TRtcDelayedCall;
    FMT:TRtcDataFormat;
    parseError:boolean;
    mySessionID:RtcString;
    rVer:byte;

  function IsValidControlKey(var Counter:integer; const s:RtcString):boolean;
  {$IFDEF RtcDoNotCheckCryptControlSums}
    begin
    // Old implementation uses 3 - 8 byte control keys,
    // New implementation uses 9 - 14 byte control keys.
    Result:= length(s) in [3..14];
    end;
  {$ELSE}
    var
      i,a,b,Chk:integer;
    begin
    Inc(Counter);
    if Counter>99 then Counter:=1;

    // New implementation sends a counter and a checksum,
    // which can be used to validate the control key.
    if length(s) in [9..14] then
      begin
      b:=(14-length(s))*9+8;
      for i:=5 to length(s) do
        Inc(b, Ord(s[i])-Ord('0'));
      a:=(Ord(s[1])-Ord('0'))*10 +
         (Ord(s[2])-Ord('0'));
      Chk:=(Ord(s[3])-Ord('0'))*10 +
           (Ord(s[4])-Ord('0'));
      Result:= (a=b) and (Chk=Counter);
      end
    else
      Result:=False;
    end;
  {$ENDIF}

  function ExtractControlKey(var Counter:integer):RtcString;
    var
      i,at,len:integer;
    begin
    Result:='';
    at:=0;len:=0;
    { Data will be closed with #13+ControlCode }
    for i:=length(code)-1 downto 0 do
      case code[i] of
        Byte('0')..Byte('9'):
          begin
          if len<14 then // we will never use control keys longer than 14 digits
            begin
            Result:=RtcChar(code[i])+Result;
            Inc(len);
            end
          else
            begin
            // Return empty result, which will tell the client to reinitialize encryption
            Result:='';
            Exit;
            end;
          end;
        13:begin
          at:=i+1;
          Break;
          end;
        else
          begin
          Result:='';
          Exit;
          end;
        end;

    if at>0 then // number found
      begin
      if not IsValidControlKey(Counter, Result) then
        Result:=''
      else
        SetLength(code,at-1);
      end
    else
      Result:='';
    end;

  function ExtractFinalControlKey(var Counter:integer):RtcString;
    var
      DecompressTo,
      i,at,len:integer;
    begin
    Result:='';len:=0;
    at:=0;
    DecompressTo:=0;
    { Data will be closed with:
      > #13+ControlCode if not compressed
      > #0+ControlCode if compressed. }
    for i:=length(code)-1 downto 0 do
      case code[i] of
        Byte('0')..Byte('9'):
          if len<14 then // we will never use control keys longer than 14 digits
            begin
            Result:=RtcChar(code[i])+Result;
            Inc(len);
            end
          else
            begin
            // Return empty result, which will tell the client to reinitialize encryption
            Result:='';
            Exit;
            end;
        13:
          begin
          at:=i+1;
          // data is NOT compressed
          Break;
          end;
        0:
          begin
          at:=i+1;
          // data is compressed
          DecompressTo:=at-1;
          Break;
          end;
        else
          begin
          Result:='';
          Exit;
          end;
        end;

    if at>0 then // number found
      begin
      if not IsValidControlKey(Counter, Result) then
        Result:=''
      else
        begin
        if DecompressTo>0 then
          begin
          {$IFDEF COMPRESS}
          try
            code:=ZDecompress_Ex(code,DecompressTo);
          except
            on E:Exception do
              Result:='';
            end;
          {$ELSE}
          Result:='';
          // raise Exception.Create('Compressed data received, but compression not supported.');
          {$ENDIF}
          end
        else
          SetLength(code,at-1);
        end;
      end
    else
      Result:='';
    end;

  procedure ExecuteMyData;
    begin
    try
      {$IFDEF COMPRESS}
      if (FMT=fmt_RTC) and (FCompress<>cNone) and not assigned(output) and
         (at<length(code)) then
        output:=TRtcHugeString.Create;
      {$ENDIF}

      DoExecute(Sender,MyData);

      if assigned(MyData) then
        begin
        if assigned(output) then
          MyData.to_Code(output)
        else
          begin
          {$IFDEF COMPRESS}
          if (FMT=fmt_RTC) and (FCompress<>cNone) then
            begin
            { We want to send data compressed, but we haven't created the
              "output" object until now, which means that this is the last result,
              so we don't need "code" anymore. }
            code:= myData.toCodeEx;
            if length(code)<RTC_MIN_COMPRESS_SIZE then
              begin
              CryptWriteEx(crypt,code);
              Sender.WriteEx(code);
              SetLength(code,0);
              end
            else
              begin
              SetLength(tmp,0);
              case FCompress of
                cFast: tmp := ZCompress_Ex(code, zcFastest);
                cMax: tmp := ZCompress_Ex(code, zcMax);
                else tmp := ZCompress_Ex(code, zcDefault);
                end;

              if length(tmp)>=length(code)-1 then
                begin
                SetLength(tmp,0);
                CryptWriteEx(crypt,code);
                Sender.WriteEx(code);
                SetLength(code,0);
                end
              else
                begin
                SetLength(code,0);
                CryptWriteEx(crypt,tmp);
                Sender.WriteEx(tmp);

                SetLength(tmp,1);
                tmp[0]:=0;
                CryptWriteEx(crypt,tmp);
                Sender.WriteEx(tmp);
                SetLength(tmp,0);
                end;
              end;
            end
          else
          {$ENDIF}
            begin
            case FMT of
              fmt_XMLRPC:
                  begin
                  if Sender.Request.Info.asInteger[SERVE_XML]=0 then
                    Sender.WriteEx(MyData.toXMLRESTResponseEx)
                  else
                    Sender.WriteEx(MyData.toXMLrpcResponseEx);
                  end;
              fmt_JSONrpc1,
              fmt_JSONrpc2,
              fmt_JSON:
                  Sender.WriteEx(MyData.toJSONEx);
              else
                  begin
                  if assigned(crypt) and crypt.CanWrite then
                    begin
                    tmp:=MyData.toCodeEx;
                    CryptWriteEx(crypt,tmp);
                    Sender.WriteEx(tmp);
                    end
                  else
                    Sender.WriteEx(MyData.toCodeEx);
                  end;
              end;
            end;
          end;
        end
      else
        Sender.Write; // no content.
    finally
      RtcFreeAndNil(MyData);
      end;
    end;

  begin
  with Sender do
    begin

    if (rsm_Query in AutoSessionMode) and
       (Request.Query.ValueCS[SessIDFieldCS]='NEW') then
      MySessionID:='NEW'
    else if (rsm_Query in AutoSessionMode) and
            (length(Request.Query.ValueCS[SessIDFieldCS])=RTC_SESSIONID_LENGTH) then
      MySessionID:=Request.Query.ValueCS[SessIDFieldCS]
    else if (rsm_Cookie in AutoSessionMode) and
            (Request.Cookie.ValueCS[SessIDFieldCS]='NEW') then
      MySessionID:='NEW'
    else if (rsm_Cookie in AutoSessionMode) and
            (length(Request.Cookie.ValueCS[SessIDFieldCS])=RTC_SESSIONID_LENGTH) then
      MySessionID:=Request.Cookie.ValueCS[SessIDFieldCS]
    else
      MySessionID:='';

    if (Session=nil) and (MySessionID<>'') and (MySessionID<>'NEW') then
      if AutoSessionCheck or AutoSessions then
        if not FindSession(MySessionID) then
          if AutoSessions then
            begin
            if HaveSession(MySessionID) then
              Response.Status(410,'Session locked')
            else
              Response.Status(410,'Session not found');
            Write;
            Exit;
            end;

    crypt:=nil;
    code:=ReadEx;

    cmd:=Request.Query.ValueCS['ACTION'];
    ctype:=Upper_Case(Request.ContentType);

    if (fmt_RTC in DataFormats) and
       ( (cmd='HELLO') or (cmd='HELO') or (cmd='HELOB') or (cmd='HELOI') ) and
       ( (ctype='') or (PosEx('RTC',ctype)>0) ) then
      begin
      if cmd='HELLO' then
        cmode:=rscm_Basic
      else if cmd='HELOB' then
        cmode:=rscm_RSABasic
      else if cmd='HELOI' then
        cmode:=rscm_RSAIsaac
      else
        cmode:=rscm_Isaac;

      if (EncryptionKey=0) {$IFNDEF RTC_RSA} or (cmode<>rscm_Basic) {$ENDIF} then // Encryption not supported
        begin
        // Find Client Session
        if assigned(Session) then
          DelCrypt(Session)
        else if AutoSessions and (MySessionID='NEW') then
          begin
          OpenSession(AutoSessionsLock);
          Session.KeepAlive:=AutoSessionsLive;
          Response.Cookie.Value[SessIDField]:=Session.ID;
          end
        else if MySessionID<>'' then
          begin
          if FindSession(MySessionID) then
            DelCrypt(Session)
          else if HaveSession(MySessionID) then
            Response.Status(410,'Session locked')
          else
            Response.Status(410,'Session not found');
          end;
        Write; // send response with empty content
        Exit;
        end
      else if length(code)=0 then
        begin
        Write;
        Exit;
        end
      else
        begin
        // Find Client Session
        if assigned(Session) then
          DelCrypt(Session)
        else if (MySessionID<>'NEW') and
                (MySessionID<>'') then
          begin
          if FindSession(MySessionID) then
            DelCrypt(Session)
          else
            begin
            if HaveSession(MySessionID) then
              Response.Status(410,'Session locked')
            else
              Response.Status(410,'Session not found');
            Write;
            Exit;
            end;
          end
        else if AutoSessions then
          begin
          OpenSession(AutoSessionsLock);
          Session.KeepAlive:=AutoSessionsLive;
          Response.Cookie.Value[SessIDField]:=Session.ID;
          end
        else
          begin
          Response.Status(410,'Session not initialized.');
          Write;
          Exit;
          end;

        if SecureKey<>'' then
          begin
        {$IFDEF RTC_RSA}
          case cmode of
            rscm_Basic,
            rscm_RSABasic:
              begin
              with TRtcCrypt.Create do
                begin
                Key:=SecureKey;
                DeCryptEx(code);
                Free;
                end;
              end;
            rscm_Isaac,
            rscm_RSAIsaac:
              begin
              with TRtcISAACrypt.Create do
                begin
                Key:=RtcStringToBytes(SecureKey);
                DeCrypt(code);
                Free;
                end;
              end;
            end;
        {$ELSE}
          with TRtcCrypt.Create do
            begin
            Key:=SecureKey;
            DeCryptEx(code);
            Free;
            end;
        {$ENDIF}
          end;

        tmpcnt:=0;
        temp:=ExtractControlKey(tmpcnt);
        if (length(code)=0) or (temp='') then
          begin
          Response.Status(417,'Encryption Key error.');
          Write;
          Exit;
          end;

        NewCrypt(Session,cmode);
        crypt:=GetCrypt(Session);
        crypt.ControlCounter:=tmpcnt;
        crypt.ControlKey:=temp;
        crypt.ClientHello:= RtcBytesToString(code);

        // prepares Encryption object for Write operation
        crypt.SetupWrite1;

        // prepares Encryption object for Read operation
        // and returns encrypted ServerHello+ControlKey
        code:=crypt.SetupRead1(EncryptionKey);

        // Send encrypted ServerHello+ControlKey
        WriteEx(code);
        end;
      end
    else if (fmt_RTC in DataFormats) and (cmd='START') and // Initialize Encryption (RTC format)
            ( (ctype='') or (PosEx('RTC',ctype)>0) ) then
      begin
      if not assigned(Session) then
        if MySessionID='' then
          begin
          Response.Status(410,'Session not initialized.');
          Write;
          Exit;
          end
        else if not FindSession(MySessionID) then
          begin
          if HaveSession(MySessionID) then
            Response.Status(410,'Session locked')
          else
            Response.Status(410,'Session not found');
          Write;
          Exit;
          end;

      if EncryptionKey=0 then // Encryption not supported
        Write // send response with empty content
      else if length(code)=0 then
        Write
      else
        begin
        crypt:=GetCrypt(Session);

        if crypt=nil then
          begin
          Response.Status(410,'Session not initialized.');
          Write;
          Exit;
          end
        else if not crypt.HaveHello then
          begin
          Response.Status(410,'Session handshake error.');
          Write;
          Exit;
          end;

        // Decode client request
        CryptReadEx(crypt, code);

        crypt.ControlKey:=ExtractControlKey(crypt.ControlCounter);
        if (crypt.ControlKey='') or (length(code)=0) then
          begin
          Response.Status(417,'Encryption Key error.');
          Write;
          Exit;
          end;

        crypt.ClientKey:= RtcBytesToString(code);

        // Set Encryption keys for Reading and Writing
        crypt.SetupWrite2;

        code:=crypt.SetupRead2(EncryptionKey);

        WriteEx(code);
        end;
      end
    else
      begin

      if AutoSessions and (MySessionID='NEW') then
        begin
        if ForceEncryption and
           (fmt_RTC in DataFormats) and
           (EncryptionKey>0) and
           ( (ctype='') or (PosEx('RTC',ctype)>0) ) then
          begin
          // 412 = Precondition Failed (Encryption required)
          Response.Status(412,'Encryption required.');
          Write;
          Exit;
          end
        else if (fmt_RTC in DataFormats) and (cmd='RESET') and
                ( (ctype='') or (PosEx('RTC',ctype)>0) ) then
          begin
          // 417 = Expectation Failed (Encryption error)
          Response.Status(417,'Encryption error.');
          Write;
          Exit;
          end
        else if AutoSessions then
          begin
          OpenSession(AutoSessionsLock);
          Session.KeepAlive:=AutoSessionsLive;
          Response.Cookie.Value[SessIDField]:=Session.ID;
          end;
        end
      else if (MySessionID<>'') and (MySessionID<>'NEW') then
        begin
        if Response.StatusCode=410 then
          begin
          // Return empty String if Session expired or non-existing
          Write;
          Exit;
          end
        else
          begin
          if not assigned(Session) then
            if not FindSession(MySessionID) then
              begin
              if HaveSession(MySessionID) then
                Response.Status(410,'Session locked')
              else
                Response.Status(410,'Session not found');
              Write;
              Exit;
              end;

          if (fmt_RTC in DataFormats) and (cmd='RESET') and
             ( (ctype='') or (PosEx('RTC',ctype)>0) ) then
            begin
            crypt:=GetCrypt(Session);
            if assigned(crypt) then
              begin
              if not (crypt.HaveHello and crypt.HaveStart) then
                begin
                // Encryption not fully initialized
                Response.Status(417,'Encryption error.');
                Write;
                Exit;
                end
              else
                crypt.Reset;
              end;
            end;
          end;
        end
      else if (fmt_RTC in DataFormats) and (cmd='RESET') then
        begin
        // No Session ID? No encryption.
        // 417 = Expectation Failed (Encryption error)
        Response.Status(417,'Encryption error.');
        Write;
        Exit;
        end;

      if length(code)=0 then // No content body
        if (length(Request.FileName)<=length(FModuleFileREST)) then // No FileName in URI
          begin
          Response.Status(404,'Bad Request.');
          Write;
          Exit;
          end;

      if (DataReqModes=[]) or
         (DataReqModes=[req_contentBodyALL]) then // Only "POST" support
        begin
        if (length(Request.FileName)>length(FModuleFileREST)) then
          begin
          Response.Status(404,'REST support disabled.');
          Write;
          Exit;
          end;
        end
      else if not (req_contentBodyALL in DataReqModes) then // Only "REST" support
        begin
        if (length(Request.FileName)<=length(FModuleFileREST)) then
          begin
          Response.Status(404,'RPC support disabled.');
          Write;
          Exit;
          end
        else if (length(code)>0) and
            not ( (req_contentBodyParams in DataReqModes) or
                  (req_contentBodyOptional in DataReqModes) ) then
          begin
          Response.Status(404,'POST support disabled.');
          Write;
          Exit;
          end;
        end
      else if not ( (req_contentBodyParams in DataReqModes) or
                    (req_contentBodyOptional in DataReqModes) ) then // No mixed RPC+REST support
        begin
        if (length(code)>0) and
           (length(Request.FileName)>length(FModuleFileREST)) then
          begin
          Response.Status(404,'Mixed RPC+REST support disabled.');
          Write;
          Exit;
          end
        end;

      if (fmt_XMLRPC in FDataFormats) and
         (PosEx('XML',ctype)>0) then
        begin
        if length(code)=0 then
          begin
          FMT:=fmt_XMLRPC;
          Request.Info.asInteger[SERVE_XML]:=0;
          end
        else if isXMLString(code) then
          begin
          FMT:=fmt_XMLRPC;
          Request.Info.asInteger[SERVE_XML]:=1;
          end
        else
          begin
          Response.Status(404,'XML Data Format not supported.');
          Write;
          Exit;
          end;
        crypt:=nil;
        end
      else if ( (fmt_JSONrpc1 in FDataFormats) or
                (fmt_JSONrpc2 in FDataFormats) or
                (fmt_JSON in FDataFormats) ) and
              ( (PosEx('JSON',ctype)>0) or
                (PosEx('JAVASCRIPT',ctype)>0) ) then
        begin
        if length(code)=0 then
          begin
          if (fmt_JSON in FDataFormats) then
            begin
            FMT:=fmt_JSON;
            Request.Info.asInteger[SERVE_JSON]:=0;
            end
          else if (fmt_JSONrpc1 in FDataFormats) then
            begin
            FMT:=fmt_JSONrpc1;
            Request.Info.asInteger[SERVE_JSON]:=1;
            end
          else
            begin
            FMT:=fmt_JSONrpc2;
            Request.Info.asInteger[SERVE_JSON]:=2;
            end;
          end
        else if isJSONString(code) then
          begin
          if (fmt_JSON in FDataFormats) and
             (length(Request.FileName)>length(FModuleFileREST)) then
            begin
            FMT:=fmt_JSON;
            Request.Info.asInteger[SERVE_JSON]:=0;
            end
          else if (fmt_JSONrpc1 in FDataFormats) then
            begin
            FMT:=fmt_JSONrpc1;
            Request.Info.asInteger[SERVE_JSON]:=1;
            end
          else if (fmt_JSONrpc2 in FDataFormats) then
            begin
            FMT:=fmt_JSONrpc2;
            Request.Info.asInteger[SERVE_JSON]:=2;
            end
          else
            begin
            FMT:=fmt_JSON;
            Request.Info.asInteger[SERVE_JSON]:=0;
            end;
          end
        else
          begin
          Response.Status(404,'JSON Data Format not supported.');
          Write;
          Exit;
          end;
        crypt:=nil;
        end
      else if (length(code)=0) and
              ( (ctype='') or (PosEx('TEXT',ctype)>0) ) then
        begin
        if (fmt_JSON in FDataFormats) then
          begin
          FMT:=fmt_JSON;
          Request.Info.asInteger[SERVE_JSON]:=0;
          end
        else if (fmt_JSONrpc1 in FDataFormats) then
          begin
          FMT:=fmt_JSONrpc1;
          Request.Info.asInteger[SERVE_JSON]:=1;
          end
        else if (fmt_JSONrpc2 in FDataFormats) then
          begin
          FMT:=fmt_JSONrpc2;
          Request.Info.asInteger[SERVE_JSON]:=2;
          end
        else if (fmt_XMLRPC in FDataFormats) then
          begin
          FMT:=fmt_XMLRPC;
          Request.Info.asInteger[SERVE_XML]:=0;
          end
        else if (fmt_RTC in FDataFormats) then
          FMT:=fmt_RTC
        else
          begin
          Response.Status(404,'REST Data Format not support.');
          Write;
          Exit;
          end;
        end
      else if (fmt_RTC in FDataFormats) and
              ( (ctype='') or (PosEx('RTC',ctype)>0) ) then
        begin
        FMT:=fmt_RTC;

        crypt:=GetCrypt(Session);
        if assigned(crypt) and crypt.CanRead then
          begin
          CryptReadEx(crypt, code);

          try
            crypt.ControlKey:=ExtractFinalControlKey(crypt.ControlCounter);
          except
            on E:Exception do
              begin
              Response.Status(409,'Encryption Error.');
              Write;
              Exit;
              end;
            end;

          if crypt.ControlKey='' then
            begin
            Response.Status(409,'Encryption Key Error.');
            Write;
            Exit;
            end;
          end
        else if ForceEncryption and (EncryptionKey>0) then
          begin
          // 412 = Precondition Failed (Encryption required)
          Response.Status(412,'Encryption required.');
          Write;
          Exit;
          end
        else if (length(Code)>0) and (Code[length(code)-1]=0) then // compressed
          begin
          {$IFDEF COMPRESS}
          try
            code:=ZDecompress_Ex(code,length(code)-1);
          except
            on E:Exception do
              begin
              Response.Status(409,'Decompression error.');
              Write;
              Exit;
              end;
            end;
          {$ELSE}
          Response.Status(409,'Compression not supported.');
          Write;
          Exit;
          {$ENDIF}
          end;
        end
      else
        begin
        // 404 = Request not supported.
        Response.Status(404,'Data Format not supported.');
        Write;
        Exit;
        end;

      output:=nil;

      at:=0;
      parseError:=False;
      try
        if assigned(FOnPeekRequest) then
          begin
          Sender.PokeEx(code);
          SetLength(code,0);
          try
            FOnPeekRequest(Sender);
          finally
            data:=Sender.Read;
            end;
          end
        else
          begin
          data:=RtcBytesToString(code);
          SetLength(code,0);
          end;

        if length(data)=0 then
          begin
          if (length(Request.FileName)>length(FModuleFileREST)) then
            begin
            parseError:=True;
            if rsm_Query in AutoSessionMode then
              MyData:=TRtcFunctionInfo.CreateREST(Sender.Request,SessIDField,FDataReqModes,length(FModuleFileREST)+1)
            else
              MyData:=TRtcFunctionInfo.CreateREST(Sender.Request,'',FDataReqModes,length(FModuleFileREST)+1);
            if FMT in [fmt_JSON,fmt_JSONrpc1,fmt_JSONrpc2] then
              TRtcFunctionInfo(MyData).reqVer:=Sender.Request.Info.asInteger[SERVE_JSON]
            else if FMT=fmt_XMLRPC then
              TRtcFunctionInfo(MyData).reqVer:=Sender.Request.Info.asInteger[SERVE_XML];
            parseError:=False;
            ExecuteMyData;
            end
          else
            begin
            Write;
            Exit;
            end;
          end
        else
          begin
          atLast:=at-1;
          while (at>atLast) and (at<length(data)) do
            begin
            atLast:=at;
            parseError:=True;
            case FMT of
              fmt_XMLRPC:
                begin
                if (length(Request.FileName)>length(FModuleFileREST)) then
                  begin
                  if rsm_Query in AutoSessionMode then
                    MyData:=TRtcFunctionInfo.FromXMLREST(Sender.Request,SessIDField,FDataReqModes,length(FModuleFileREST)+1,data,at)
                  else
                    MyData:=TRtcFunctionInfo.FromXMLREST(Sender.Request,'',FDataReqModes,length(FModuleFileREST)+1,data,at);
                  TRtcFunctionInfo(MyData).reqVer:=Sender.Request.Info.asInteger[SERVE_XML];
                  end
                else
                  begin
                  Sender.Request.FileName:='';
                  MyData:=TRtcValue.FromXMLrpc(data,at);
                  if TRtcValue(MyData).CheckType(rtc_Function) then
                    TRtcValue(MyData).asFunction.reqVer:=Sender.Request.Info.asInteger[SERVE_XML];
                  end;
                end;
              fmt_JSONrpc1,
              fmt_JSONrpc2,
              fmt_JSON:
                begin
                if (length(Request.FileName)>length(FModuleFileREST)) then
                  begin
                  if rsm_Query in AutoSessionMode then
                    MyData:=TRtcFunctionInfo.FromJSONREST(Sender.Request,SessIDField,FDataReqModes,length(FModuleFileREST)+1,Utf8Decode(data),at)
                  else
                    MyData:=TRtcFunctionInfo.FromJSONREST(Sender.Request,'',FDataReqModes,length(FModuleFileREST)+1,Utf8Decode(data),at);
                  TRtcFunctionInfo(MyData).reqVer:=Sender.Request.Info.asInteger[SERVE_JSON];
                  Sender.Request.Info.asText[SERVE_JSON_ID]:=TRtcFunctionInfo(MyData).reqID;
                  end
                else
                  begin
                  rVer:=0;
                  if (fmt_JSONrpc1 in FDataFormats) then
                    rVer:=rVer or 1;
                  if (fmt_JSONrpc2 in FDataFormats) then
                    rVer:=rVer or 2;
                  if (fmt_JSON in FDataFormats) then
                    rVer:=rVer or 4;

                  Sender.Request.FileName:='';
                  MyData:=TRtcValue.FromJSONrpc(rVer,Utf8Decode(data),at);
                  if TRtcValue(MyData).CheckType(rtc_Function) then
                    begin
                    Sender.Request.Info.asInteger[SERVE_JSON]:=TRtcValue(MyData).asFunction.reqVer;
                    Sender.Request.Info.asText[SERVE_JSON_ID]:=TRtcValue(MyData).asFunction.reqID;
                    end;
                  end;
                end;
              else
                begin
                if (length(Request.FileName)>length(FModuleFileREST)) then
                  begin
                  if rsm_Query in AutoSessionMode then
                    MyData:=TRtcFunctionInfo.FromCodeREST(Sender.Request,SessIDField,FDataReqModes,length(FModuleFileREST)+1,data,at)
                  else
                    MyData:=TRtcFunctionInfo.FromCodeREST(Sender.Request,'',FDataReqModes,length(FModuleFileREST)+1,data,at);
                  end
                else
                  begin
                  Sender.Request.FileName:='';
                  MyData:=TRtcValue.FromCode(data,at);
                  end;
                end;
              end;
            parseError:=False;
            ExecuteMyData;
            end;
          end;

        if AutoSessions or AutoSessionCheck then
          if assigned(Session) then // Session active
            begin
            if Session.isClosing then // Session closing
              Response.Cookie.Value[SessIDField]:='-'+MySessionID
            else if MySessionID<>Session.ID then // Last Session ID received from Client different from current Session ID
              if Response.Cookie.ValueCS[SessIDFieldCS]<>Session.ID then // Session ID to be sent to the Client different from current Session ID
                Response.Cookie.Value[SessIDField]:=Session.ID;
            end;
      except
        on E:EDelayedCall do
          begin
          // Create and Post the DelayedCall
          mycall:=e.call;
          mycall.Conn:=Sender;
          mycall.output:=output;
          mycall.FMT:=FMT;
          mycall.Compress:=Compression;
          mycall.ObjectLinkSupport:=FObjectLinkSupport;
          mycall.ObjectDataOutEvent:=FOnObjectDataOut;
          if assigned(Session) then
            begin
            mycall.SessionID:=Session.ID;
            mycall.crypt:=crypt;
            end;

          Info.asObj[RTC_SERVERMODULE_DELAYED_CALL]:=mycall;
          if Sender.isExtension then
            mycall.Execute
          else
            mycall.Post(MultiThreaded);
          Exit; // not sending the reponse now!
          end;
        on E:Exception do
          with TRtcValueResult.Create(nil) do
            try
              asException:=RtcWideString(E.Message);
              if asErrorCode=0 then
                if parseError then
                  asErrorCode:=RTC_ERROR_PARSE
                else if E is ERtcBadFunctionName then
                  asErrorCode:=RTC_ERROR_BADFUNCTION
                else if FMT>fmt_RTC then
                  asErrorCode:=RTC_ERROR_INTERNAL;
              if assigned(output) then
                to_Code(output)
              else
                begin
                case FMT of
                  fmt_XMLRPC:
                      begin
                      if Sender.Request.Info.asInteger[SERVE_XML]=0 then
                        Sender.WriteEx(toXMLRESTREsponseEx)
                      else
                        Sender.WriteEx(toXMLrpcResponseEx);
                      end;
                  fmt_JSONrpc1,
                  fmt_JSONrpc2,
                  fmt_JSON:
                      begin
                      reqID:=Sender.Request.Info.asText[SERVE_JSON_ID];
                      reqVer:=Sender.Request.Info.asInteger[SERVE_JSON];
                      Sender.WriteEx(toJSONEx);
                      end;
                  else
                      begin
                      if assigned(crypt) and crypt.CanWrite then
                        begin
                        tmp:=toCodeEx;
                        CryptWriteEx(crypt,tmp);
                        Sender.WriteEx(tmp);
                        end
                      else
                        Sender.WriteEx(toCodeEx);
                      end;
                  end;
                end;
            finally
              Free;
            end;
        end;

      {$IFDEF COMPRESS}
      if assigned(output) then
        begin
        { we have stored uncompressed data in "output",
          now we need to compress it all and send it out. }
        try
          code:=output.GetEx;
        finally
          RtcFreeAndNil(output);
          end;

        if length(code)<RTC_MIN_COMPRESS_SIZE then
          begin
          CryptWriteEx(crypt,code);
          WriteEx(code);
          SetLength(code,0);
          end
        else
          begin
          SetLength(tmp,0);
          case FCompress of
            cFast: tmp := ZCompress_Ex(code, zcFastest);
            cMax: tmp := ZCompress_Ex(code, zcMax);
            else tmp := ZCompress_Ex(code, zcDefault);
            end;

          if length(tmp)>=length(code)-1 then
            begin
            SetLength(tmp,0);
            CryptWriteEx(crypt,code);
            WriteEx(code);
            SetLength(code,0);
            end
          else
            begin
            SetLength(code,0);
            CryptWriteEx(crypt,tmp);
            WriteEx(tmp);

            SetLength(tmp,1);
            tmp[0]:=0;
            CryptWriteEx(crypt,tmp);
            WriteEx(tmp);
            SetLength(tmp,0);
            end;
          end;
        end;
      {$ENDIF}
      
      if assigned(crypt) and crypt.CanWrite then
        begin
        // Add control key to the end of our response
        code:=RtcStringToBytes(crypt.ControlKey);
        CryptWriteEx(crypt, code);
        WriteEx(code);
        end;
      end;
    end;
  end;

procedure TRtcBaseServerModule.Call_DataOut(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcBaseServerModule.Call_DataIn(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcBaseServerModule.Call_DataSent(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcBaseServerModule.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  // empty
  end;

function TRtcBaseServerModule.GetFunctionGroup: TRtcFunctionGroup;
  begin
  try
    Result:=FFunctions;
    if not (Result is TRtcFunctionGroup) then
      Result:=nil;
  except
    Result:=nil;
    end;
  end;

procedure TRtcBaseServerModule.SetFunctionGroup(const Value: TRtcFunctionGroup);
  begin
  FFunctions:=Value;
  end;

function TRtcBaseServerModule.GetModuleFileName: RtcString;
  begin
  Result:=FModuleFileName;
  end;

procedure TRtcBaseServerModule.SetModuleFileName(const Value: RtcString);
  begin
  if FModuleFileName<>Value then
    begin
    FModuleFileName:=Value;
    if FModuleFileName<>'' then
      begin
      // Module FileName has to start with '/'
      if Copy(FModuleFileName,1,1)<>'/' then
        FModuleFileName:='/'+FModuleFileName;
      // REST Module FileName has to end with '/'
      if Copy(FModuleFileName,length(FModuleFileName),1)<>'/' then
        FModuleFileREST:=FModuleFileName+'/'
      else
        FModuleFileREST:=FModuleFileName;
      end
    else
      FModuleFileREST:='';
    FCryptSesName:=FModuleHost+FModuleFileName+'.$CRYPT$';
    FObjManSesName:=FModuleHost+FModuleFileName+RTCL_SRVSESSION;
    end;
  end;

function TRtcBaseServerModule.GetModuleHost: RtcString;
  begin
  Result:=FModuleHost;
  end;

procedure TRtcBaseServerModule.SetModuleHost(const Value: RtcString);
  begin
  if FModuleHost<>Value then
    begin
    FModuleHost:=Value;
    FCryptSesName:=FModuleHost+FModuleFileName+'.$CRYPT$';
    FObjManSesName:=FModuleHost+FModuleFileName+RTCL_SRVSESSION;
    end;
  end;

function TRtcBaseServerModule.GetCrypt(Session: TRtcSession): TRtcCryptServer;
  var
    obj:TObject;
  begin
  if not assigned(Session) then
    Result:=nil
  else
    begin
    obj:=Session._asObj[FCryptSesName];
    if assigned(obj) then
      Result:=TRtcCryptServer(obj)
    else
      Result:=nil;
    end;
  end;

procedure TRtcBaseServerModule.NewCrypt(Session: TRtcSession; m:TRtcServerCryptMode);
  var
    obj:TObject;
  begin
  obj:=Session._asObj[FCryptSesName];
  if obj<>nil then
    TRtcCryptServer(obj).Init(m)
  else
    Session._asObj[FCryptSesName]:=TRtcCryptServer.Create(m);
  end;

procedure TRtcBaseServerModule.DelCrypt(Session: TRtcSession);
  var
    obj:TObject;
  begin
  if assigned(Session) then
    begin
    obj:=Session._asObj[FCryptSesName];
    if obj<>nil then
      begin
      Session._asObj[FCryptSesName]:=nil;
      obj.Free;
      end;
    end;
  end;

procedure TRtcBaseServerModule.SetAutoEncrypt(const Value: integer);
  begin
  if Value<0 then
    raise Exception.Create('Negative values not allowed for EncryptionKey.');
  FAutoEncrypt := Value;
  if FAutoEncrypt>0 then
    FAutoSessions:=True
  else
    FForceEncrypt:=False;
  end;

procedure TRtcBaseServerModule.SetAutoSessions(const Value: boolean);
  begin
  if not Value and (FAutoEncrypt>0) then
    raise Exception.Create('Set EncryptionKey to 0 before setting AutoSessions to False.');
  FAutoSessions := Value;
  if not FAutoSessions then
    begin
    FAutoEncrypt:=0;
    FForceEncrypt:=False;
    end;
  end;

procedure TRtcBaseServerModule.SetForceEncrypt(const Value: boolean);
  begin
  FForceEncrypt := Value;
  if FForceEncrypt then
    begin
    FAutoSessions:=True;
    if FAutoEncrypt=0 then
      FAutoEncrypt:=16;
    end;
  end;

procedure TRtcBaseServerModule.SetCompress(const Value: TRtcCompressLevel);
  begin
  FCompress := Value;
  end;

procedure TRtcBaseServerModule.SetAutoSessionsLock(const Value: TRtcSessionLockType);
  begin
  FAutoSessionsLock := Value;
  end;

procedure TRtcBaseServerModule.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FFunctions then
      SetFunctionGroup(nil);
  end;

procedure TRtcBaseServerModule.SetObjectLinkSupport(const Value: TRtcObjectLinkSupport);
  begin
  FObjectLinkSupport := Value;
  end;

procedure TRtcBaseServerModule.DoObjectCreate(Sender:TObject; Param: TRtcObjectCall);
  begin
  if assigned(FOnObjectCreate) then
    FOnObjectCreate(TRtcConnection(Sender),Param);
  end;

{ TRtcCryptServer }

constructor TRtcCryptServer.Create(m:TRtcServerCryptMode);
  begin
  inherited Create;
  HaveHello:=False;
  HaveStart:=False;
  ControlKey:='';
  ClientHello:='';
  ServerHello:='';
  ClientKey:='';
  ServerKey:='';
  ControlCounter:=0;
  CRead:=nil;
  CWrite:=nil;
{$IFDEF RTC_RSA}
  Mode:=m;
  CReadEx:=nil;
  CWriteEx:=nil;
  CWriteRsa:=nil;
{$ENDIF}
  end;

destructor TRtcCryptServer.Destroy;
  begin
  try
    Init(rscm_Basic);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcCryptServer.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcCryptServer.Init(m:TRtcServerCryptMode);
  begin
  HaveHello:=False;
  HaveStart:=False;
  ControlKey:='';
  ClientHello:='';
  ServerHello:='';
  ClientKey:='';
  ServerKey:='';
  ControlCounter:=0;
  RtcFreeAndNil(CRead);
  RtcFreeAndNil(CWrite);

{$IFDEF RTC_RSA}
  Mode:=m;
  RtcFreeAndNil(CReadEx);
  RtcFreeAndNil(CWriteEx);
  RtcFreeAndNil(CWriteRsa);
{$ENDIF}
  end;

procedure TRtcCryptServer.Reset;
  begin
  ControlCounter:=0;
  RtcFreeAndNil(CWrite);
  RtcFreeAndNil(CRead);

{$IFDEF RTC_RSA}
  RtcFreeAndNil(CWriteEx);
  RtcFreeAndNil(CReadEx);
  RtcFreeAndNil(CWriteRsa);
  case Mode of
    rscm_Basic,
    rscm_RSABasic:
      begin
      // Encryption object for Write operation
      CWrite:=TRtcCrypt.Create;
      CWrite.Key:=ClientKey+ServerHello;
      // Encryption object for Read operation
      CRead:=TRtcCrypt.Create;
      if Mode=rscm_Basic then
        CRead.Key:=ServerKey+ClientHello
      else
        CRead.Key:=ServerKey;
      end;
    rscm_Isaac,
    rscm_RSAIsaac:
      begin
      // Encryption object for Write operation
      CWriteEx:=TRtcISAACrypt.Create;
      CWriteEx.Key:=RtcStringToBytes(ClientKey+ServerHello);
      // Encryption object for Read operation
      CReadEx:=TRtcISAACrypt.Create;
      if Mode=rscm_Isaac then
        CReadEx.Key:=RtcStringToBytes(ServerKey+ClientHello)
      else
        CReadEx.Key:=RtcStringToBytes(ServerKey);
      end;
    end;
{$ELSE}
  // Encryption object for Write operation
  CWrite:=TRtcCrypt.Create;
  CWrite.Key:=ClientKey+ServerHello;
  // Encryption object for Read operation
  CRead:=TRtcCrypt.Create;
  CRead.Key:=ServerKey+ClientHello
{$ENDIF}
  end;

procedure TRtcCryptServer.SetupWrite1;
  begin
  RtcFreeAndNil(CWrite);

{$IFDEF RTC_RSA}
  RtcFreeAndNil(CWriteEx);
  RtcFreeAndNil(CWriteRsa);
  CWriteRsa:=TRtcRSA.Create;
  case Mode of
    rscm_Basic:
      begin
      CWrite:=TRtcCrypt.Create;
      CWrite.Key:=ClientHello;
      end;
    rscm_Isaac:
      begin
      CWriteEx:=TRtcISAACrypt.Create;
      CWriteEx.Key:=RtcStringToBytes(ClientHello);
      end;
    rscm_RSABasic,
    rscm_RSAIsaac:
      begin
      CWriteRsa.PublicKey:=RtcStringToBytes(ClientHello);
      end;
    end;
{$ELSE}
  CWrite:=TRtcCrypt.Create;
  CWrite.Key:=ClientHello;
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

function TRtcCryptServer.SetupRead1(KeySize:integer):RtcByteArray;
  begin
  RtcFreeAndNil(CRead);

{$IFDEF RTC_RSA}
  RtcFreeAndNil(CReadEx);
  ServerHello:=RtcBytesToString(CWriteRsa.RND(KeySize));
  case Mode of
    rscm_Basic,
    rscm_RSABasic:
      begin
      CRead:=TRtcCrypt.Create;
      CRead.Key:=ServerHello;
      end;
    rscm_Isaac,
    rscm_RSAIsaac:
      begin
      CReadEx:=TRtcISAACrypt.Create;
      CReadEx.Key:=RtcStringToBytes(ServerHello);
      end;
    end;
  Result:= RtcStringToBytes(ServerHello+ControlKey);
  case Mode of
    rscm_Basic:    CWrite.CryptEx(Result);
    rscm_Isaac:    CWriteEx.Crypt(Result);
    rscm_RSABasic,
    rscm_RSAIsaac: Result:=CWriteRsa.Encrypt(Result);
    end;
{$ELSE}
  ServerHello:=RandomKey(KeySize);
  CRead:=TRtcCrypt.Create;
  CRead.Key:=ServerHello;
  Result:= RtcStringToBytes(ServerHello+ControlKey);
  CWrite.CryptEx(Result);
{$ENDIF}
  HaveHello:=True;
  end;

procedure TRtcCryptServer.SetupWrite2;
  begin
  RtcFreeAndNil(CWrite);
{$IFDEF RTC_RSA}
  RtcFreeAndNil(CWriteEx);
  case Mode of
    rscm_Basic,
    rscm_RSABasic:
      begin
      CWrite:=TRtcCrypt.Create;
      CWrite.Key:= ClientKey+ServerHello;
      end;
    rscm_Isaac,
    rscm_RSAIsaac:
      begin
      CWriteEx:=TRtcISAACrypt.Create;
      CWriteEx.Key:= RtcStringToBytes(ClientKey+ServerHello);
      end;
    end;
{$ELSE}
  CWrite:=TRtcCrypt.Create;
  CWrite.Key:= ClientKey+ServerHello;
{$ENDIF}
  end;

function TRtcCryptServer.SetupRead2(KeySize:integer):RtcByteArray;
  begin
  RtcFreeAndNil(CRead);
{$IFDEF RTC_RSA}
  RtcFreeAndNil(CReadEx);
  RtcFreeAndNil(CWriteRsa);
  CWriteRsa:=TRtcRSA.Create;
  // Increase ServerKey size by ClientHello size when using RSA, to compensate for the reduced Key strength
  case Mode of
    rscm_Basic,rscm_Isaac:       ServerKey:=RtcBytesToString(CWriteRsa.RND(KeySize));
    rscm_RSABasic,rscm_RSAIsaac: ServerKey:=RtcBytesToString(CWriteRsa.RND(KeySize + length(ClientHello)));
    end;
  case Mode of
    rscm_Basic:
      begin
      CRead:=TRtcCrypt.Create;
      CRead.Key := ServerKey+ClientHello;
      end;
    rscm_Isaac:
      begin
      CReadEx:=TRtcISAACrypt.Create;
      CReadEx.Key:= RtcStringToBytes(ServerKey+ClientHello);
      end;
    rscm_RSABasic:
      begin
      CRead:=TRtcCrypt.Create;
      CRead.Key := ServerKey;
      end;
    rscm_RSAIsaac:
      begin
      CReadEx:=TRtcISAACrypt.Create;
      CReadEx.Key:= RtcStringToBytes(ServerKey);
      end;
    end;
{$ELSE}
  ServerKey:=RandomKey(KeySize);
  CRead:=TRtcCrypt.Create;
  CRead.Key := ServerKey+ClientHello;
{$ENDIF}
  Result:= RtcStringToBytes(ServerKey+ControlKey);
  CryptWriteEx(self, Result);
  HaveStart:=True;
  end;

function TRtcCryptServer.CanRead:boolean;
  begin
  Result:=assigned(CRead) {$IFDEF RTC_RSA} or assigned(CReadEx) {$ENDIF};
  end;

function TRtcCryptServer.CanWrite:boolean;
  begin
  Result:=assigned(CWrite) {$IFDEF RTC_RSA} or assigned(CWriteEx) {$ENDIF};
  end;

{ TRtcDelayedCall }

type
  TRtcDelayedCallJob=class(TRtcJob)
  public
    Conn:TRtcConnection;
    constructor Create(Con: TRtcConnection);

    function Run(Thr:TRtcThread):boolean; override;
    end;

type
  TZeroObj=class(TObject)
  constructor Create;
  end;

{ TZeroObj }

constructor TZeroObj.Create;
  begin
  end;

var
  List:tObjList;
  CS:TRtcCritSec;
  zeroObj:TObject;

procedure AddObj(o:TObject);
  begin
  CS.Acquire;
  try
    List.insert(RtcIntPtr(o),o);
  finally
    CS.Release;
    end;
  end;

function DelObj(o:TObject):boolean;
  begin
  CS.Acquire;
  try
    if List=nil then
      Result:=False
    else if List.search(RtcIntPtr(o))=nil then
      Result:=False
    else
      begin
      List.remove(RtcIntPtr(o));
      Result:=True;
      end;
  finally
    CS.Release;
    end;
  end;

procedure SetObj(o:TObject; num:TObject);
  begin
  CS.Acquire;
  try
    if List.search(RtcIntPtr(o))<>nil then
      List.change(RtcIntPtr(o),num);
  finally
    CS.Release;
    end;
  end;

procedure KillObjs;
  var
    ob:TObject;
    i:RtcIntPtr;
  begin
  CS.Acquire;
  try
    if assigned(List) then
      begin
      i:=List.search_min(ob);
      while assigned(ob) do
        begin
        List.remove(i);
        if (ob<>nil) and (ob<>zeroObj) then
          RtcFreeAndNil(ob);
        i:=List.search_g(i, ob);
        end;
      RtcFreeAndNil(List);
      end;
  finally
    CS.Release;
    end;
  end;

procedure WakeUpAllDelayedCalls;
  var
    ob:TObject;
    i:RtcIntPtr;
  begin
  CS.Acquire;
  try
    if assigned(List) then
      begin
      i:=List.search_min(ob);
      while assigned(ob) do
        begin
        if (ob<>nil) and (ob<>zeroObj) and (ob is TRtcDelayedCall) then
          TRtcDelayedCall(ob).WakeUp;
        i:=List.search_g(i, ob);
        end;
      end;
  finally
    CS.Release;
    end;
  end;
  
function CheckObj(o:TObject):boolean;
  var
    o2:TObject;
  begin
  CS.Acquire;
  try
    o2:=List.search(RtcIntPtr(o));
    Result:=(o2<>nil) and (o2<>zeroObj);
  finally
    CS.Release;
    end;
  end;

function HaveObj(o:TObject):boolean;
  begin
  CS.Acquire;
  try
    Result:=List.search(RtcIntPtr(o))<>nil;
  finally
    CS.Release;
    end;
  end;

procedure CancelDelayedCall(cb:TRtcDelayedCall);
  begin
  if DelObj(cb) then
    RtcFreeAndNil(cb);
  end;

constructor TRtcDelayedCall.Create;
  begin
  inherited Create;
  WaitEvent:=nil;
  Timer:=nil;
  Called:=False;
  output:=nil;

  AddObj(Self);
  end;

destructor TRtcDelayedCall.Destroy;
  begin
  try
    CS.Acquire;
    try
      DelObj(self);
      if assigned(WaitEvent) then
        WaitEvent.SetEvent;
      if assigned(Timer) then
        TRtcTimer.Stop(Timer);
    finally
      CS.Release;
      end;

    RtcFreeAndNil(output);

    RtcFreeAndNil(Param);
    SessionID:='';
    Conn:=nil;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDelayedCall.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDelayedCall.Post(Multi_Threaded:boolean);
  var
    wait:boolean;
  begin
  CS.Acquire;
  try
    if not HaveObj(self) then Exit;

    wait:=not Called;
    if wait then
      begin
      Timer:=TRtcTimer.Create(Multi_Threaded);
      TRtcTimer.Enable(Timer,msDelay,Execute,True);
      end;
  finally
    CS.Release;
    end;
  if not wait then
    Execute;
  end;

procedure TRtcDelayedCall.Execute;
  var
    ob:TRtcDelayedCallJob;
    wait:boolean;
  begin
  wait:=False;

  CS.Acquire;
  try
    if not HaveObj(self) then Exit;

    // Stop Timer if timer active
    if assigned(Timer) then
      begin
      TRtcTimer.Stop(Timer);
      Timer:=nil;
      end
    // Start Waiting Event if no timer
    else
      begin
      wait:=not Called;
      if wait then
        WaitEvent:=TRtcEvent.Create(True,False);
      end;
  finally
    CS.Release;
    end;

  if wait then
    begin
    WaitEvent.WaitFor(msDelay);
    CS.Acquire;
    try
      if not HaveObj(self) then Exit;

      RtcFreeAndNil(WaitEvent);
    finally
      CS.Release;
      end;
    end;

  // We don't want to have a situation where Execute would be called twice (for example, when Timer triggers and another thread uses "Call")
  CS.Acquire;
  try
    if not CheckObj(self) then Exit;

    SetObj(self,zeroObj);
  finally
    CS.Release;
    end;

  if assigned(Conn) then
    begin
    ob:=TRtcDelayedCallJob.Create(Conn);
    if not Conn.PostJob(ob) then
      if ob.SingleUse then
        RtcFreeAndNil(ob);
    end;
  end;

procedure TRtcDelayedCall.WakeUp;
  var
    needtocall:boolean;
  begin
  needtocall:=False;

  CS.Acquire;
  try
    if not CheckObj(self) then Exit;
    Called:=True;

    if assigned(Timer) then
      begin
      // Timer active. Need to stop the timer and call Execute
      needtocall:=True;
      TRtcTimer.Stop(Timer);
      Timer:=nil;
      end
    else if assigned(WaitEvent) then
      // Wait Event active. Only need to Set the Event flag
      WaitEvent.SetEvent;
  finally
    CS.Release;
    end;

  if needtocall then
    Execute;
  end;

{ TRtDelayedCallJob }

constructor TRtcDelayedCallJob.Create(Con: TRtcConnection);
  begin
  inherited Create;
  Conn:=Con;
  end;

function TRtcDelayedCallJob.Run(Thr:TRtcThread):boolean;
  var
    call,mycall:TRtcDelayedCall;
    MyResult,TmpData:TRtcValueResult;
    {$IFDEF COMPRESS}
    code:RtcByteArray;
    {$ENDIF}
    temp:RtcByteArray;
    released:boolean;

    TmpMan:TRtcObjectManager;
    ObjMan:TRtcServerObjectManager;
    xData:TRtcValue;
    MyReqVer:byte;
    MyReqID:RtcWideString;
  begin
  Result:=False;
  released:=False;
  call:=nil;
  try
    if assigned(Conn) then with Conn do
      begin
      call:=TRtcDelayedCall(Info.asObj[RTC_SERVERMODULE_DELAYED_CALL]);
      if not assigned(call) then Exit;
      Info.asObj[RTC_SERVERMODULE_DELAYED_CALL]:=nil;

      if (call.SessionID<>'') then
        if not assigned(Session) or (Session.ID<>call.SessionID) then
          begin
          Disconnect;
          Exit;
          end;

      MyReqVer:=call.Param.reqVer;
      MyReqID:=call.Param.reqID;

      EnterEvent;
      try
        try
          MyResult:=TRtcValueResult.Create(call.Param);
          try
            call.CallEvent(call.Conn, call.Param, myResult);

            if (call.ObjectLinkSupport<>ol_None) and assigned(call.Conn) then
              begin
              ObjMan:=TRtcServerObjectManager(call.Conn.GetObjectManager);
              if assigned(ObjMan) then
                begin
                xData:=ObjMan.GetBroadcast;
                if assigned(xData) then
                  try
                    TmpMan:=SetRtcObjectManager(ObjMan);
                    try
                      ObjMan.ExecuteBroadcast(call.Conn,xData);
                    finally
                      SetRtcObjectManager(TmpMan);
                      end;
                  finally
                    RtcFreeAndNil(xData);
                    end;
                if not ObjMan.Idle then
                  begin
                  xData:=ObjMan.GetData;
                  if assigned(call.ObjectDataOutEvent) then
                    call.ObjectDataOutEvent(call.Conn,xData);
                  TmpData:=TRtcValueResult.Create(call.Param);
                  with TmpData.newFunction(RTCL_FUNCTION) do
                    begin
                    asObject[RTCL_RESULT]:=MyResult;
                    asObject[RTCL_DATA]:=xData;
                    end;
                  MyResult:=TmpData;
                  end;
                end;
              end;

            if assigned(call.output) then
              myResult.to_Code(call.output)
            else
              begin
              {$IFDEF COMPRESS}
              if (call.FMT=fmt_RTC) and (call.Compress<>cNone) then
                begin
                code:=myResult.toCodeEx;
                if length(code)<RTC_MIN_COMPRESS_SIZE then
                  begin
                  CryptWriteEx(call.crypt, code);
                  WriteEx(code);
                  end
                else
                  begin
                  { we have stored uncompressed data in "output",
                    now we need to compress it all and send it out. }
                  SetLength(temp,0);
                  case call.Compress of
                    cFast: temp:=ZCompress_Ex(code,zcFastest);
                    cMax: temp:=ZCompress_Ex(code,zcMax);
                    else temp:=ZCompress_Ex(code,zcDefault);
                    end;

                  if length(temp)>=length(code)-1 then
                    begin
                    SetLength(temp,0);
                    CryptWriteEx(call.crypt,code);
                    WriteEx(code);
                    SetLength(code,0);
                    end
                  else
                    begin
                    SetLength(code,0);
                    CryptWriteEx(call.crypt,temp);
                    WriteEx(temp);

                    SetLength(temp,1);
                    temp[0]:=0;
                    CryptWriteEx(call.crypt,temp);
                    WriteEx(temp);
                    SetLength(temp,0);
                    end;
                  end;
                end
              else
              {$ENDIF}
                begin
                case call.FMT of
                  fmt_XMLRPC:
                      begin
                      if MyResult.reqVer=0 then
                        WriteEx(MyResult.toXMLRESTResponseEx)
                      else
                        WriteEx(MyResult.toXMLrpcResponseEx);
                      end;
                  fmt_JSONrpc1,
                  fmt_JSONrpc2,
                  fmt_JSON:
                      WriteEx(MyResult.toJSONEx);
                  else        
                      begin
                      if assigned(call.crypt) and call.crypt.CanWrite then
                        begin
                        temp:=MyResult.toCodeEx;
                        CryptWriteEx(call.crypt,temp);
                        WriteEx(temp);
                        end
                      else
                        WriteEx(MyResult.toCodeEx);
                      end;
                  end;
                end;
              end;
          finally
            RtcFreeAndNil(MyResult);
            end;
        except
          on E:EDelayedCall do
            begin
            // Create and Post the DelayedCall
            mycall:=E.call;
            Info.asObj[RTC_SERVERMODULE_DELAYED_CALL]:=mycall;

            mycall.Conn:=Conn;
            mycall.output:=call.output;
            mycall.FMT:=call.FMT;
            mycall.Compress:=call.Compress;
            mycall.crypt:=call.crypt;
            mycall.SessionID:=call.SessionID;
            mycall.ObjectDataOutEvent:=call.ObjectDataOutEvent;

            call.crypt:=nil;
            call.output:=nil;

            RtcFreeAndNil(call);

            if Conn.isExtension then
              begin
              released:=True;
              {$IFDEF AUTOREFCOUNT} DisposeOf; {$ELSE} Free; {$ENDIF}
              mycall.Execute;
              end
            else
              mycall.Post(MultiThreaded);

            Exit; // not sending the reponse from here!
            end;
          on E:Exception do
            with TRtcValueResult.Create(nil) do
              try
                asException:=RtcWideString(E.Message);
                if assigned(call.output) then
                  to_Code(call.output)
                else
                  begin
                  reqVer:=MyReqVer;
                  reqID:=MyReqID;
                  if asErrorCode=0 then
                    if E is ERtcBadFunctionName then
                      asErrorCode:=RTC_ERROR_BADFUNCTION
                    else if call.FMT>fmt_RTC then
                      asErrorCode:=RTC_ERROR_INTERNAL;
                  case call.FMT of
                    fmt_XMLRPC:
                        begin
                        if reqVer=0 then
                          WriteEx(toXMLRESTResponseEx)
                        else
                          WriteEx(toXMLrpcResponseEx);
                        end;
                    fmt_JSONrpc1,
                    fmt_JSONrpc2,
                    fmt_JSON:
                        WriteEx(toJSONEx);
                    else
                        begin
                        if assigned(call.crypt) and call.crypt.CanWrite then
                          begin
                          temp:=toCodeEx;
                          CryptWriteEx(call.crypt,temp);
                          WriteEx(temp);
                          end
                        else
                          WriteEx(toCodeEx);
                        end;
                    end;
                  end;
              finally
                Free;
              end;
          end;

        {$IFDEF COMPRESS}
        if assigned(call.output) then
          begin
          try
            code:=call.output.GetEx;
          finally
            RtcFreeAndNil(call.output);
            end;

          if length(code)<RTC_MIN_COMPRESS_SIZE then
            begin
            CryptWriteEx(call.crypt, code);
            WriteEx(code);
            end
          else
            begin
            { we have stored uncompressed data in "output",
              now we need to compress it all and send it out. }

            SetLength(temp,0);
            case call.Compress of
              cFast: temp:=ZCompress_Ex(code,zcFastest);
              cMax: temp:=ZCompress_Ex(code,zcMax);
              else temp:=ZCompress_Ex(code,zcDefault);
              end;

            if length(temp)>=length(code)-1 then
              begin
              SetLength(temp,0);
              CryptWriteEx(call.crypt,code);
              WriteEx(code);
              SetLength(code,0);
              end
            else
              begin
              SetLength(code,0);
              CryptWriteEx(call.crypt,temp);
              WriteEx(temp);

              SetLength(temp,1);
              temp[0]:=0;
              CryptWriteEx(call.crypt,temp);
              WriteEx(temp);
              SetLength(temp,0);
              end;
            end;
          end;
        {$ENDIF}
        
        if assigned(call.crypt) and call.crypt.CanWrite then
          begin
          // Add control key to the end of our response
          temp:=RtcStringToBytes(call.crypt.ControlKey);
          CryptWriteEx(call.crypt, temp);
          WriteEx(temp);
          SetLength(temp,0);
          end;

        Flush;

      finally
        LeaveEvent;
        end;
      end;
  finally
    RtcFreeAndNil(call);
    if not released then
      {$IFDEF AUTOREFCOUNT} DisposeOf; {$ELSE} Free; {$ENDIF}
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcSrvModule Initializing ...','DEBUG');{$ENDIF}

zeroObj:=TZeroObj.Create;
CS:=TRtcCritSec.Create;
List:=tObjList.Create(128);

{$IFDEF RTC_DEBUG} Log('rtcSrvModule Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcSrvModule Finalizing ...','DEBUG');{$ENDIF}

CloseTimerPool;

KillObjs;

RtcFreeAndNil(List);
RtcFreeAndNil(CS);
RtcFreeAndNil(zeroObj);

{$IFDEF RTC_DEBUG} Log('rtcSrvModule Finalized.','DEBUG');{$ENDIF}
end.
