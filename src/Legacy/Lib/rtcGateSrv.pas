{
  @html(<b>)
  Gateway (Legacy)
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This is a LEGACY unit, which means that continued use of this unit is discouraged.
  If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
  released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.
  
  This unit implements Server-side RTC Gateway components.
}
unit rtcGateSrv;

{$INCLUDE rtcDefs.inc}

interface

// Debug the TRtcGateway component?
{.$DEFINE RTCGATE_DEBUG}

// Write more details to Gateway Debug LOGs?
{.$DEFINE RTCGATE_FULLDEBUG}

{$IFDEF RTCGATE_FULLDEBUG} {$DEFINE RTCGATE_DEBUG} {$ENDIF}

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSystem,
  rtcSrcList,
  rtcThrPool,
  rtcTimer,
  rtcLog,

  rtcInfo,
  rtcCrypt,

  rtcConn,
  rtcConnProv,
  rtcDataSrv,
  rtcHttpSrv,
  rtcPlugins,

  rtcGateConst;

type
  ERtcGateway = class(Exception);
  ERtcGateFatal = class(Exception);
  ERtcGateClosed = class(Exception);

  TRtcGatewayLoginEvent = procedure(Sender:TRtcConnection; UserID:Cardinal; var UserAuth,UserInfo,SecondaryKey:RtcString) of object;
  TRtcGatewayNotifyEvent = procedure(Sender:TRtcConnection; UserID:Cardinal; const UserAuth,UserInfo:RtcString) of object;

{$IFDEF RTC_ANON_METHODS}

  TRtcGatewayLoginAnonMethod = reference to procedure(Sender:TRtcConnection; UserID:Cardinal; var UserAuth,UserInfo,SecondaryKey:RtcString);
  TRtcGatewayNotifyAnonMethod = reference to procedure(Sender:TRtcConnection; UserID:Cardinal; const UserAuth,UserInfo:RtcString);

{$ENDIF}

  { @Abstract(Abstract RTC Gateway component) }
  TRtcAbsGateway = class(TRtcServerComponent)
  private
    FServer: TRtcDataServer;
    FLink: TRtcDataServerLink;

    PingUserList:array of TObject;
    PingJobList:array of byte;
    PingUserCnt:integer;

    CleanUserList:array of TObject;
    CleanJobList:array of byte;
    CleanUserCnt:integer;

    FBeforeUserLogin: TRtcGatewayLoginEvent;

    FOnUserReady,
    FOnUserNotReady,
    FBeforeUserLogout:TRtcGatewayNotifyEvent;

    FCUsr,FCStr:TRtcCritSec;
    FxUsers,FxRemoval:TObjList;
    FxReserved:tBinList;

    FzStreamers:TStrObjList;
    FzListeners:TStrObjList;

    FzHosts:TStrObjList;
    FzControls:TStrObjList;
    FzPublic:TStrObjList;

    PingProvider: TRtcDataProvider;
    InputStream: TRtcDataProvider;
    OutputStream: TRtcDataProvider;
    LoginProvider: TRtcDataProvider;

    isMultiThreaded:boolean;
    PingTimer,
    CleanTimer:TRtcTimer;
    GatePingJob,
    GateCleanJob:TRtcJob;

    FGateFileName: RtcString;

    FGATE_MAXINFOLEN:integer;
    FMaxUsersActive,
    FMaxUsersTotal:integer;

  {$IFDEF RTC_RSA}
    rnd:TRtcISAAC;
  {$ENDIF}

    FGATEURI_PING,
    FGATEURI_LOGIN,
    FGATEURI_INPUT,
    FGATEURI_OUTPUT,
    FGATEURI_INPUTRESET,
    FGATEURI_OUTPUTRESET,
    FGATEURI_STATUS,
    FGATE_PRIMARY_KEY:RtcString;

    FWriteLimit:Cardinal;
    FReadLimit:Cardinal;

    procedure SetGateFileName(const Value: RtcString);
    procedure SetGatePrimaryKey(const Value: RtcString);
    procedure SetMaxInfoLen(const Value: integer);

    procedure SetServer(const Value: TRtcDataServer);
    procedure SetLink(const Value: TRtcDataServerLink);

    procedure PingActiveUsers;
    procedure RemoveInactiveUsers;

    procedure PingProviderListenStart(Sender: TRtcConnection);
    procedure PingProviderListenStop(Sender: TRtcConnection);

    procedure PingProviderCheckRequest(Sender: TRtcConnection);

    procedure LoginProviderCheckRequest(Sender: TRtcConnection);
    procedure LoginProviderDataReceived(Sender: TRtcConnection);
    procedure LoginProviderDisconnect(Sender: TRtcConnection);

    procedure InputStreamCheckRequest(Sender: TRtcConnection);
    procedure InputStreamDataReceived(Sender: TRtcConnection);
    procedure InputStreamDisconnect(Sender: TRtcConnection);

    procedure OutputStreamCheckRequest(Sender: TRtcConnection);
    procedure OutputStreamDataReceived(Sender: TRtcConnection);
    procedure OutputStreamDataSent(Sender: TRtcConnection);
    procedure OutputStreamDisconnect(Sender: TRtcConnection);

    function GetNextUID:TGateUID;
    procedure ReleaseUID(var uid:TGateUID);

    function CreateUser(Sender:TRtcConnection; UserID:TGateUID; const UserAuth,UserInfo:RtcString; const CryptKey:RtcString):boolean;

    function FindGateUser(uid:TGateUID):TObject;

    function FindUser4Input(i:byte; conn:RtcIntPtr; var obj:TObject; level:shortint=0):boolean; overload;
    function FindUser4Output(i:byte; conn:RtcIntPtr; var obj:TObject; level:shortint=0):boolean; overload;

    //function FindUser4Input(i:byte; conn:RtcIntPtr; uid:TGateUID; var obj:TObject; level:shortint=0):boolean; overload;
    //function FindUser4Output(i:byte; conn:RtcIntPtr; uid:TGateUID; var obj:TObject; level:shortint=0):boolean; overload;

    procedure ReadAndProcessData(direct:boolean; Conn:RtcIntPtr; obj:TObject);

    function GetUserAddr(const Sender: TRtcConnection): RtcString;
    function crcGetUserID(i:byte; const Sender: TRtcConnection; warn:boolean=False): TGateUID;

    procedure EvOn(Sender: TRtcConnection; UserID:TGateUID; const UserAuth,UserInfo:RtcString);
    procedure EvOff(Sender: TRtcConnection; UserID:TGateUID; const UserAuth,UserInfo:RtcString);

  protected

    // @exclude
    function GetOrder: integer; override;
    // @exclude
    procedure SetOrder(const Value: integer);

    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure DoRemoveAllUsers;
    // @exclude
    procedure DoRemoveUser(UserID:TGateUID);
    // @exclude
    procedure DoLogOutUser(UserID:TGateUID);

    // @exclude
    procedure DoCloseAllUserChannels;

    // @exclude
    procedure ErrorResponse(const Sender: TRtcConnection; const meth: RtcString; const E: Exception);

    // @exclude
    procedure SilentResponse(const Sender: TRtcConnection; const lname:String; const meth, err: RtcString; fatal:boolean);

    // TRtcHttpServer component -> set before calling "Listen"
    property Server:TRtcDataServer read FServer write SetServer;

    // TRtcDataServerLink or TRtcDualDataServerLink component,
    // if you need to connect the Gateway to one or more TRtcHttpServer components
    property Link:TRtcDataServerLink read FLink write SetLink;

    { This is the Order in which the components will be asked to
      process a request. The smaller this Order number, the sooner
      a component will be asked to process a request, compared to
      other components connected to the DataServer at the same level.
      Since we could have more levels (more DataServerLink components
      connected to each other), Order only defines the priority at the same level. }
    property CheckOrder:integer read GetOrder write SetOrder default 0;

  public

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Returns HTML-formatted information about this Gateway.
      Used in the "/check" request to generate a response for Web Browsers. }
    function CheckGatewayStatus:RtcString;

    { Use this method if you want to add User "UserID" to Group "GroupID" owned by user "OwnerID".
      If NotifyUser=True, the user will receive the info that he was added to the Group.
      If NotifyOwner=True, the Owner will receive the info that the user was added to its Group.

      This is a Gateway-side call, so Clients won't keep track of used GroupIDs opened by the Gateaway.
      To avoid GroupID conflicts with Clients, use only high GroupIDs here. For example, from 247 to 201. }
    procedure AddUserToGroup(OwnerID, GroupID, UserID:TGateUID; NotifyUser, NotifyOwner: boolean);

    { Use this method if you want to remove User "UserID" from Group "GroupID" owned by user "OwnerID".
      If NotifyUser=True, user will receive the info that he was removed from the Group.
      If NotifyOwner=True, the Owner will receive the info that the user was removed from its Group. }
    procedure RemoveUserFromGroup(OwnerID, GroupID, UserID:TGateUID; NotifyUser, NotifyOwner: boolean);

    { Use this method if you want to remove Group "GroupID" owned by user "OwnerID".
      If NotifyUsers=True, all users in that group will receive the info that they were removed from the Group.
      If NotifyOwner=True, the Owner will receive the info about every user who was removed from the Group. }
    procedure RemoveGroup(OwnerID, GroupID:TGateUID; NotifyUsers, NotifyOwner: boolean);

    { Use this method to Add user "UserID" to channel "Channel".
      If UserIsHost=True, the User will be added to the list of Hosts on this Channel.
        If HostGroupID>0, the User will be notified on "HostGroupID" when Listeners join or leave this Channel.
        If NotifyHosts=True, Hosts already in this channel will get notified.
      If UserIsListener=True, the User will be added to the list of Listeners on this Channel.
        If ListenerGroupID>0, the User will be notified on "ListenerGroupID" when Hosts join or leave this Channel.
        If NotifyListeners=True, Listeners who have already joined this Channel will be notified. }
    procedure AddUserToChannel(UserID:TGateUID; const Channel: RtcString;
                               HostGroupID:TGateUID; UserIsHost, NotifyHosts:boolean;
                               ListenerGroupID:TGateUID; UserIsListener, NotifyListeners:boolean);
    { Use this method to Add a Host "UserID" to channel "Channel".
      User will be added to the list of Hosts on this Channel.
      If HostGroupID>0, the User will be notified on "HostGroupID" when Listeners join or leave this Channel.
      If NotifyListeners=True, Listeners who have already joined this Channel will be notified.
      If NotifyHost=True, the Host will get notified which Listeners have already joined the Channel. }
    procedure AddHostToChannel(UserID:TGateUID; const Channel: RtcString;
                               HostGroupID:TGateUID; NotifyHost:boolean;
                               NotifyListeners:boolean);
    { Use this method to Add a Listener "UserID" to channel "Channel".
      User will be added to the list of Listeners on this Channel.
      If ListenerGroupID>0, the User will be notified on "ListenerGroupID" when Hosts join or leave this Channel.
      If NotifyListener=True, the Listener will be notified of all Hosts in this Channel.
      If NotifyHosts=True, Hosts in this Channel will get notified about the Listener joining the Channel. }
    procedure AddListenerToChannel(UserID:TGateUID; const Channel: RtcString;
                               ListenerGroupID:TGateUID; NotifyHosts:boolean;
                               NotifyListener:boolean);

    { Use this method to Remove the user "UserID" from channel "Channel".
      If RemoveAsHost=True (default), the user will be removed as a Host (if registered as Host) on this channel.
        If NotifyListeners=True, all Listeners on this channel will get notified if this user was a Host.
      If RemoveAsListener=True (default), the user will be removed as a Listener (if registered as Listener) on this channel.
        If NotifyHosts=True, all Hosts on this channel will get notified if this user was a Listener. }
    procedure RemoveUserFromChannel(UserID:TGateUID; const Channel:RtcString; NotifyHosts, NotifyListeners: boolean; RemoveAsHost:boolean=True; RemoveAsListener:boolean=True);

  {$IFDEF RTC_ANON_METHODS}

    { Get the Gateway "Login" event for anonymous method
        procedure(Sender:TRtcConnection; UserID:Cardinal; var UserAuth,UserInfo,SecondaryKey:RtcString) }
    function Anon(const Event:TRtcGatewayLoginAnonMethod):TRtcGatewayLoginEvent; overload;

    { Get the Geteway "Notify" event for anonymous method
        procedure(Sender:TRtcConnection; UserID:Cardinal; const UserAuth,UserInfo:RtcString) }
    function Anon(const Event:TRtcGatewayNotifyAnonMethod):TRtcGatewayNotifyEvent; overload;

  {$ENDIF}

  published

    // "Root" FileName for this Gateway component
    property GateFileName:RtcString read FGateFileName write SetGateFileName;

    // Primary Encryption Key
    property GatePrimaryKey:RtcString read FGATE_PRIMARY_KEY write SetGatePrimaryKey;

    { Maximum allowed length for UserAuth and UserInfo (combined) sent from a Client.
      If MaxUserInfoLen=0, there is no limit (Gateway accepts anything from Clients). }
    property MaxUserInfoLen:integer read FGATE_MAXINFOLEN write SetMaxInfoLen default 0;

    { Maximum number of ACTIVE Users allowed on the Gateway at the same time.
      If the limit is reached, Gateway will stop accepting logins from new Users.
      If MaxUsersActive=0, the default limit will be applied (=100.000 users). }
    property MaxUsersActive:integer read FMaxUsersActive write FMaxUsersActive default 0;

    { Maximum number of TOTAL Users (active and expired) allowed on the Gateway.
      If the limit is reached, Gateway will stop accepting logins from new Users.
      If MaxUsersTotal=0, the default limit will be applied (=200.000 users). }
    property MaxUsersTotal:integer read FMaxUsersTotal write FMaxUsersTotal default 0;

    { Speed limit (in KBits per second, 0 = unlimited) for streaming data through this Gateway to each Client.
      By setting a "StreamSpeedLimit", the Gateway will wait before sending more data out to a Client,
      if that would result in the last sending operation to that Client to exceed the specified limit,
      limiting the amount of Gateway's UPLOAD bandwidth available to each connected Client.
      Setting this property to zero (0) disables the upload speed limiter. }
    property StreamSpeedLimit:Cardinal read FWriteLimit write FWriteLimit default 0;

    { Speed limit (in KBits per second, 0 = unlimited) for receiving data through this Gateway from each Client.
      By setting a "ReceiveSpeedLimit", the Gateway will wait before reading more data from a Client,
      if the amount of data already read from that Client has exceeded the specified speed limit,
      limiting the amount of Gateway's DOWNLOAD bandwidth available to each connected Client.
      Setting this property to zero (0) disables the download speed limiter. }
    property ReceiveSpeedLimit:Cardinal read FReadLimit write FReadLimit default 0;

    { Event called when a user is logging in.
      Here, you can check if the user is allowed to log in and set the "SecondaryKey" parameter
      to mach the "GateSecondaryKey" property set for the "TRtcHttpGateClient" component on the Client.

    INPUT:
      Sender = Connection object
      UserAuth = "GateUserAuth" String received from the Client
      UserInfo = "GateUserInfo" String received from the Client
      UserID = UserID generated by the Gateway for this Client

    OUTPUT:
      SecondaryKey = Set this to the "GateSecondaryKey" parameter defined on the Client side (based on UserAuth).
                Leave empty if the Client should use the default encryption Key.
                Default encryption Key is generated from "GatePrimaryKey" and Client's IP address.

      UserInfo = You can upate this with additional user information or clean up content to only include data
                 which should be kept and sent to other users with Subscriptions and Account-related notifications.

      UserAuth = You can update this based on received UserAuth parameter if you want some
                 other user-related information to be available in other Gateway events.

    DISALLOW ACCESS?
      In case the user may NOT log in, raise an exception here with the appropriate error message for the Client.
    }
    property BeforeUserLogin:TRtcGatewayLoginEvent read FBeforeUserLogin write FBeforeUserLogin;

    { Event called when the user is logged in and ready to receive data. }
    property OnUserReady:TRtcGatewayNotifyEvent read FOnUserReady write FOnUserReady;

    { Event called when the user is no longer ready to receive data. }
    property OnUserNotReady:TRtcGatewayNotifyEvent read FOnUserNotReady write FOnUserNotReady;

    { Event called before the User is logged out and its User ID becomes invalid (garbage collected). }
    property BeforeUserLogout:TRtcGatewayNotifyEvent read FBeforeUserLogout write FBeforeUserLogout;
    end;

  { @Abstract(RTC Gateway component)
    Assign a TRtcHttpServer component to the "Server" property, or assign a
    TRtcDataServerLink or TRtcDualDataServerLink component to the "Link" property,
    configure the other published properties if necessary, and you
    have a fully functional Gateway running on your Server(s). }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcGateway = class(TRtcAbsGateway)
  published
    // TRtcHttpServer component -> set before calling "Listen"
    property Server;
    // Link to a TRtcDataServerLink or TRtcDualDataServerLink component,
    // when you want to connect the Gateway component to multiple TRtcHttpServer components.
    property Link;
    { This is the Order in which this component will be asked to
      process its requests. The smaller this Order number, the sooner
      the component will be asked to process a request, compared to
      other components connected to the DataServer at the same level.
      Since we could have more levels (more DataServerLink components
      connected to each other), Order only defines the priority at the same level. }
    property CheckOrder;
    end;

  { @Abstract(RTC HTTP Gateway component)
    Gateway component with a TRtcHttpServer component created internally.
    Configure all the necessary published properties, set ACTIVE:=TRUE
    and you have a fully functional Gateway running on your machine. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcHttpGateway = class(TRtcAbsGateway)
  private
    procedure MyInvalidRequest(Sender:TRtcConnection);
    procedure MyRequestNotAccepted(Sender:TRtcConnection);
    procedure MyException(Sender:TRtcConnection; E:Exception);

    function GetActive: boolean;
    function GetMultiThreaded: boolean;
    function GetCryptPlugin: TRtcCryptPlugin;
    function GetOnClose: TRtcNotifyEvent;
    function GetOnListenError: TRtcErrorEvent;
    function GetOnListenLost: TRtcNotifyEvent;
    function GetOnOpen: TRtcNotifyEvent;
    function GetOnRestart: TRtcNotifyEvent;
    function GetServer: TRtcHttpServer;
    function GetServerAddr: RtcString;
    function GetServerPort: RtcString;
    function GetServerIPV: RtcIPV;

    procedure SetActive(const Value: boolean);
    procedure SetMultiThreaded(const Value: boolean);
    procedure SetCryptPlugin(const Value: TRtcCryptPlugin);
    procedure SetOnClose(const Value: TRtcNotifyEvent);
    procedure SetOnListenError(const Value: TRtcErrorEvent);
    procedure SetOnListenLost(const Value: TRtcNotifyEvent);
    procedure SetOnOpen(const Value: TRtcNotifyEvent);
    procedure SetOnRestart(const Value: TRtcNotifyEvent);
    procedure SetServerAddr(const Value: RtcString);
    procedure SetServerPort(const Value: RtcString);
    procedure SetServerIPV(const Value: RtcIPV);

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    // TRtcHttpServer component, created internally
    property Server:TRtcHttpServer read GetServer;

  published
    { Set Active:=TRUE to start the Gateway listener (same as calling "Server.Listen"),
      set Active:=FALSE to stop the Gateway listener (same as calling "Server.StopListenNow").
      (Alias for the "Server.isListening" function). }
    property Active:boolean read GetActive write SetActive default False;

    { Set MultiThreaded:=TRUE before setting Active:=TRUE if you want the Gateway to be Multi-Threaded.
      Set MultiThreaded:=FALSE before setting Active:=TRUE if you want the Gateway to be Single-Threaded. }
    property MultiThreaded:boolean read GetMultiThreaded write SetMultiThreaded default False;

    { Gateway Network Addapter Address.
      Leave empty (default) to bind to all installed Network Addapters.
      (Alias for the "Server.ServerAddr" property). }
    property GateAddr:RtcString read GetServerAddr write SetServerAddr;

    { Gateway Port number.
      (Alias for the "Server.ServerPort" property). }
    property GatePort:RtcString read GetServerPort write SetServerPort;

    { Gateway "IP Version".prefference.
      (Alias for the "Server.ServerIPV" property). }
    property GateIPV:RtcIPV read GetServerIPV write SetServerIPV default rtc_IPvDefault;

    { To use SSL/SSH encryption with third-party components, assign the 3rd-party encryption
      plug-in here before you start using the Gateway (before setting "Active" to TRUE).
      (Alias for the "Server.CryptPlugin" property). }
    property UseCryptPlugin:TRtcCryptPlugin read GetCryptPlugin write SetCryptPlugin;

    { Listener was started. This means that we're waiting on a
      specific port for incomming connections. This event will be
      triggered if setting "Active" to TRUE was succesfull.
      (Alias for the "Server.OnListenStart" event). }
    property OnListenStart:TRtcNotifyEvent read GetOnOpen write SetOnOpen;

    { Gateway stopped listening. This means that we're no longer waiting on
      a specific port for incomming Client connections. This event will be
      triggered if setting "Active" to FALSE was succesfull.
      (Alias for the "Server.OnListenStop" event). }
    property OnListenStop:TRtcNotifyEvent read GetOnClose write SetOnClose;

    { This event will be called when our listener closes,
      without us manually setting the "Active" property to FALSE.
      (Alias for the "Server.OnListenLost" event). }
    property OnListenLost:TRtcNotifyEvent read GetOnListenLost write SetOnListenLost;

    { This event will be called when listener can not start because of an error.
      (Alias for the "Server.OnListenError" event). }
    property OnListenError:TRtcErrorEvent read GetOnListenError write SetOnListenError;

    { This event will be triggered just before starting a new listening
      attempt, when a listener had to be restarted (listener killed by OS).
      (Alias for the "Server.OnRestart" event). }
    property OnRestart:TRtcNotifyEvent read GetOnRestart write SetOnRestart;
    end;

implementation

const
  INFO_USERID = 'I';
  INFO_OWNERIP = 'O';

type
  // forward
  TRtcGateOneUser = class;

  TRtcGateUser = class(TObject)
  public
    function ozSendFirst(oFromUser:TRtcGateOneUser;
                       SenderUID,FromUID:TGateUID; TotalSize:Cardinal;
                       const data:RtcByteArray;
                       len, loc:Cardinal;
                       ext:byte):integer; virtual; abstract;
    function ozSendMore(oFromUser:TRtcGateOneUser;
                       SenderUID,FromUID:TGateUID; LeftToSend:Cardinal;
                       const data:RtcByteArray;
                       len, loc:Cardinal;
                       ext:byte):integer; virtual; abstract;
    function oPostNote(Cmd:byte; FromUID:TGateUID; warn:boolean=True):integer; virtual; abstract;
    function oPostInfo(Cmd:byte; FromUID:TGateUID; const FromInfo:RtcString):integer; virtual; abstract;
    function oSendPing(Cmd:byte; FromUID:TGateUID):integer; virtual; abstract;
  end;

  TRtcGateOneUser = class(TRtcGateUser)
  protected
    oWriteDelta,
    oWriteMax,
    oWriteSpeed,
    oWriteTime,
    oWriteCount:int64;
    oWriteLimit:Cardinal;

    iPaused, // Paused reading (bandiwdth limit exceeded)
    iStreaming, // DATA sent, waiting for pick-up by all RECEIVERS
    iWaiting, // "DataReceived" event triggered (data waiting to be read)

    oPaused, // Paused sending (bandwidth limit exceeded)
    oPrepared, // Data prepared for sending
    oSending:boolean; // Sending (expecting a "DataSent" event)

    iLeftToRead,
    oLeftToSend:int64;

    iContinue,
    oContinue:int64;

    iReadDelta,
    iReadMax,
    iReadSpeed,
    iReadTime,
    iReadCount:int64;
    iReadLimit:Cardinal;

  (* Read-Only after LOGIN *)

    xUID:TGateUID;
    xOwnerIP:RtcString;
    xGate:TRtcAbsGateway;

    // UserAuth String received from the Client during Login
    xAuth:RtcString;
    // UserInfo String received from the Client during Login
    xInfo:RtcString;

  (* INPUT Read/Write *)

    // Input Critical Section
    inCS:TRtcCritSec;

    // Input encryption
    iCryptIn:TRtcCrypt;

    // User sending data to these Groups (Owner)
    iOwnGroups:TObjList;

    // Users added these users as Friends
    iOwnFriends:TObjList;

    // our Public Channel subscriptions
    iPublicChannels:tStrIntList;

    // our Host Channel subscriptions
    iHostChannels:tStrIntList;

    // our Control/Listener channel subscriptions
    iControlChannels:tStrIntList;

    // Input Stream connection
    iInput:RtcIntPtr;
    iFrom:RtcString;
    iInThr:TRtcThread;
    iInputReady,
    iInputClear:boolean;

    oiAllReady,
    oiUsed,
    oiRemoved:boolean;

    // Input Buffer
    iInBuff:TRtcHugeByteArray;
    iInSize:Cardinal;

    // Senders that need to notify us
    iWaitingOnSenders:TObjList;

    // Last package received
    iLastInput:TAppRunTime;

  (* INPUT -> SENDING *)

    // Last package was not sent completely?
    iSending:boolean;
    // Number of bytes that need to be sent
    iLeftToSend:Cardinal;
    // Packet being sent is still valid (all parts sent so far)
    iPacketValid:boolean;
    // Receiver is my Group
    iReceiverIsGroup:boolean;
    // User/Group waiting to receive more data
    iReceiver,
    iSender:TGateUID;
    // Extended byte
    iExt:byte;

  (* OUTPUT Read/Write *)

    // Output Critical Section
    outCS:TRtcCritSec;

    // Output encryption
    oCryptOut:TRtcCrypt;

    // User receiving data for these Groups
    oInGroups:TObjList;

    // User was added as Friend by these users
    oInFriends:TObjList;

    // Output Stream connection
    oOutput:RtcIntPtr;
    oFrom:RtcString;
    oOutThr:TRtcThread;
    oOutputReady,
    oOutputClear:boolean;

    // Paused receivers that need to be notified when
    // we are ready to send more data
    oWaitingReceivers:TObjList;

    oOutSize:Cardinal;
    oOutBuff:TRtcHugeByteArray;

    // Last package sent
    oLastOutput,
    oLastPing:TAppRunTime;

    JobCheckPing,
    JobStartSending,
    JobStartReceiving,
    JobContinueInput,
    JobContinueOutput:TRtcJob;

    procedure ThrCheckPing;
    procedure ThrStartSending;
    procedure ThrStartReceiving;
    procedure ThrContinueInput;
    procedure ThrContinueOutput;

    procedure PostInputReady;
    procedure PostOutputReady;
    procedure PostStartSending;
    procedure PostStartReceiving;
    procedure PostContinueInput;
    procedure PostContinueOutput;
    procedure PostCheckPing;

    function findUser(var userList:tObjList; UID:TGateUID):TObject;
    function insertUser(var userList:tObjList; UID:TGateUID; obj:TObject):boolean;
    function removeUser(var userList:tObjList; UID:TGateUID):boolean;
    function removedLastUser(var userList:tObjList; UID:TGateUID):boolean;

    procedure prepareBuffer(var buff:TRtcHugeByteArray);

    procedure CleanUp(perRequest, clearConn, removed:boolean; Conn:RtcIntPtr; resInput,resOutput:boolean; Releasing:boolean=False);

    {$IFDEF RTCGATE_DEBUG}
    procedure LockInput(i:byte);
    procedure LockOutput(i:byte);
    procedure UnLockInput;
    procedure UnLockOutput;
    {$ENDIF}

  public
    constructor Create(ReadSpeedLimit,
                       WriteSpeedLimit:Cardinal;
                       UID:TGateUID;
                       const UserAuth:RtcString;
                       const UserInfo:RtcString;
                       const ForIP:RtcString;
                       const CryptKey:RtcString;
                       Gate:TRtcAbsGateway);
    destructor Destroy; override;

    function ozSendFirst(oFromUser:TRtcGateOneUser;
                        SenderUID,FromUID:TGateUID; TotalSize:Cardinal;
                        const data:RtcByteArray;
                        len, loc:Cardinal;
                        ext:byte):integer; override;
    function ozSendMore(oFromUser:TRtcGateOneUser;
                       SenderUID,FromUID:TGateUID; LeftToSend:Cardinal;
                       const data:RtcByteArray;
                       len, loc:Cardinal;
                       ext:byte):integer; override;
    function oPostNote(Cmd:byte; FromUID:TGateUID; warn:boolean=True):integer; override;
    function oPostInfo(Cmd:byte; FromUID:TGateUID; const FromInfo:RtcString):integer; override;
    function oSendPing(Cmd:byte; FromUID:TGateUID):integer; override;

    procedure oRemoveReceiver(UID:TGateUID);
    procedure ioRemoveSenders;

    procedure iNotifyTheReceiver(UID:TGateUID);
    procedure oiNotifyReceivers;

    procedure izNewWaitingReceiver(ReceiverUID:TGateUID; oReceiver:TRtcGateOneUser);

    procedure ioOpenInput(Conn:RtcIntPtr);
    procedure iCloseInput(Conn:RtcIntPtr);
    procedure ixoResetInput(i:byte; Conn:RtcIntPtr);

    procedure oOpenOutput(Conn:RtcIntPtr);
    procedure ixoActivateOutput(Conn:RtcIntPtr);
    procedure oiDataSent(Conn:RtcIntPtr; direct:boolean=False);
    procedure oCloseOutput(Conn:RtcIntPtr);
    procedure ixoResetOutput(i:byte; Conn:RtcIntPtr);

    procedure ixoProcessData(Conn:RtcIntPtr; var data:RtcByteArray);

    procedure ixoResetInOutStreams(i:byte);

    procedure ioRemoveAllGroupsOwnedByUser(NotifyUsers, NotifyOwner, Releasing: boolean);
    procedure ioRemoveGroup(GroupID: TGateUID; NotifyUsers, NotifyOwner:boolean);

    procedure oNotifyRemovalFromGroup(gid:TGateUID; Notify:boolean);
    procedure ooRemoveUserFromGroup(GroupID: TGateUID; NotifyUser, NotifyOwner: boolean);
    procedure ooRemoveUserFromAllGroups(NotifyOwners, NotifyUser: boolean);

    procedure oNotifyRemovalFromFriends(fid:TGateUID; Notify:boolean);
    procedure ioRemoveUserAsFriend(FriendID: TGateUID; NotifyFriend, NotifyUser: boolean);
    procedure ioRemoveAllFriends(NotifyFriends, NotifyUser:boolean);

    procedure ioNotifyRemovalAsFriend(fid:TGateUID; Notify:boolean);
    procedure oiRemoveUserFromAllFriends(NotifyFriends, NotifyUser:boolean);

    procedure xoInternalAddHost(const Channel:RtcString; GroupID:TGateUID;
                                NotifyOthers,NotifyUser:boolean);
    procedure xoInternalAddControl(const Channel:RtcString; GroupID:TGateUID;
                                  NotifyOthers,NotifyUser:boolean);
    procedure xoInternalAddPublic(const Channel:RtcString; GroupID:TGateUID;
                                 NotifyOthers,NotifyUser:boolean);
    procedure xoInternalRemoveHost(const Channel:RtcString;
                                  NotifyOthers,NotifyUser:boolean);
    procedure xoInternalRemoveControl(const Channel:RtcString;
                                     NotifyOthers,NotifyUser:boolean);
    procedure xoInternalRemovePublic(const Channel:RtcString;
                                    NotifyOthers,NotifyUser:boolean);

    procedure ixoSubscribeToChannel(const Channel:RtcString;
                                  GroupID:TGateUID;
                                  NotifyOthers,NotifyUser:boolean);
    procedure ixoUnSubscribeFromChannel(const Channel:RtcString;
                                      NotifyOthers,NotifyUser:boolean);
    procedure ixoUnSubscribeFromAll(NotifyOthers,NotifyUser:boolean);

    // @exclude
    procedure ixoInternalAddUserToGroup(OwnerID, GroupID, UserID:TGateUID; NotifyUser, NotifyOwner, Remote: boolean);
    // @exclude
    procedure xooInternalRemoveUserFromGroup(GroupID, UserID:TGateUID; NotifyUser, NotifyOwner: boolean);
    // @exclude
    procedure ixoInternalRemoveGroup(OwnerID, GroupID:TGateUID; NotifyUsers, NotifyOwner: boolean);

    // @exclude
    procedure ixoInternalAddUserAsFriend(UserID, FriendID:TGateUID; NotifyFriend, NotifyUser: boolean);
    // @exclude
    procedure ixoInternalRemoveUserAsFriend(UserID, FriendID:TGateUID; NotifyFriend, NotifyUser: boolean);
    // @exclude
    procedure ixoInternalRemoveFriends(UserID:TGateUID; NotifyFriends, NotifyUser: boolean);

    // @exclude
    procedure ixoInternalSubscribeToChannel(UserID, GroupID:TGateUID; const data:RtcByteArray; len,loc:Cardinal; NotifyOthers, NotifyUser: boolean);
    // @exclude
    procedure ixoInternalUnSubscribeFromChannel(UserID:TGateUID; const data:RtcByteArray; len,loc:Cardinal; NotifyOthers, NotifyUser: boolean);
    // @exclude
    procedure ixoInternalUnSubscribeFromAll(UserID:TGateUID; NotifyOthers, NotifyUser: boolean);

    // @exclude
    function iBeforeRead:boolean;
    // @exclude
    procedure iAfterRead(const Len:int64);
    // @exclude
    function oBeforeWrite:boolean;
    // @exclude
    procedure oAfterWrite(const Len:int64);
  end;

  TRtcGateUserGroup=class(TRtcGateUser)
  protected
    grOwner:TRtcGateOneUser;
    grUsers:TObjList;

    grpCS:TRtcCritSec;
    grUID:TGateUID;

  public
    constructor Create(Owner:TRtcGateOneUser; UID:TGateUID);
    destructor Destroy; override;

    procedure LockGroup(i:byte);
    procedure UnLockGroup;

    function ozSendFirst(oFromUser:TRtcGateOneUser;
                        SenderUID,FromUID:TGateUID; TotalSize:Cardinal;
                        const data:RtcByteArray;
                        len, loc:Cardinal;
                        ext:byte):integer; override;
    function ozSendMore(oFromUser:TRtcGateOneUser;
                       SenderUID,FromUID:TGateUID; LeftToSend:Cardinal;
                       const data:RtcByteArray;
                       len, loc:Cardinal;
                       ext:byte):integer; override;
    function oPostNote(Cmd:byte; FromUID:TGateUID; warn:boolean=True):integer; override;
    function oPostInfo(Cmd:byte; FromUID:TGateUID; const FromInfo:RtcString):integer; override;
    function oSendPing(Cmd:byte; FromUID:TGateUID):integer; override;
  end;

function LID2Text(i:byte):RtcString;
  begin
  Result:='';
  case i of
    61: Result:='PostContinueInput';
    62: Result:='PostContinueOutput';
    63: Result:='ThrContinueInput';
    64: Result:='ThrContinueOutput';
    1: Result:='ioRemoveSenders';
    2: Result:='iNotifyTheReceiver';
    3: Result:='ioNotifyRemovalAsFriend';
    4: Result:='ioRemoveAllGroupsOwnedByUser';
    5: Result:='ioRemoveGroup';
    6: Result:='ioRemoveUserAsFriend';
    7: Result:='ioRemoveAllFriends';
    20,21,22: Result:='ixoSubscribeToChannel';
    23,24,25: Result:='ixoUnSubscribeFromChannel';
    26,27,28,29: Result:='ixoUnSubscribeFromAll';
    31,32,33,34: Result:='ixoInternalAddUserToGroup';
    35,36,37: Result:='ixoInternalAddUserAsFriend';
    41: Result:='oRemoveReceiver';
    42,43: Result:='oiNotifyReceivers';
    44: Result:='oNotifyRemovalFromGroup';
    45: Result:='oNotifyRemovalFromFriends';
    46,47: Result:='ooRemoveUserFromAllGroups';
    48: Result:='ooRemoveUserFromGroup';
    49,50: Result:='oiRemoveUserFromAllFriends';
    98,99: Result:='CheckGatewayStatus';
    100: Result:='InternalOpenInput';
    101: Result:='ioOpenInput';
    105: Result:='InternalResetInput';
    123,124: Result:='DoStartReceiving';
    120,121: Result:='InputStreamDataReceived';
    132: Result:='PD.FindGroupUser';
    133: Result:='PD.FindFriendUser';
    131,134,135: Result:='ixoProcessData';
    130,136: Result:='ReadAndProcessData';
    146,147: Result:='ixoResetInput';
    148,149: Result:='InputStreamDisconnect';
    150: Result:='iCloseInput';
    201: Result:='oOpenOutput';
    202: Result:='ixoActivateOutput';
    205: Result:='InternalResetOutput';
    200: Result:='InternalOpenOutput';
    210: Result:='InternalActivateOutput';
    223,224: Result:='DoStartSending';
    225: Result:='DoCheckPing';
    220,221: Result:='OutputStreamDataSent';
    231: Result:='ozSendFirst';
    232: Result:='ozSendMore';
    233: Result:='oPostNote';
    234: Result:='oPostInfo';
    235: Result:='oSendPing';
    236: Result:='ThrCheckPing';
    237: Result:='PostCheckPing';
    239,240: Result:='oiDataSent';
    246,247: Result:='ixoResetOutput';
    248,249: Result:='OutputStreamDisconnect';
    255: Result:='CleanUp';
    250: Result:='oCloseOutput';
    254: Result:='ixoResetInOutStreams';

    151: Result:='INPUT idle max.Timeout';
    152: Result:='INPUT + OUTPUT idle Timeout';
    153: Result:='OUTPUT idle max.Timeout';
    154: Result:='OUTPUT connect Timeout+';
    155: Result:='INPUT idle max.Timeout';
    156: Result:='INPUT idle + OUTPUT connect Timeout';
    157: Result:='INPUT connect Timeout+';
    158: Result:='OUTPUT idle max.Timeout';
    159: Result:='INPUT connect + OUTPUT idle Timeout';
    160: Result:='OUTPUT connect Timeout+';
    161: Result:='INPUT connect Timeout+';
    162: Result:='INPUT + OUTPUT connect Timeout';
    163: Result:='OUTPUT idle Timeout';
    164: Result:='OUTPUT connect Timeout';
    165: Result:='INPUT idle Timeout';
    166: Result:='INPUT connect Timeout';

    else Result:='???';
    end;
  Result:=Int2Str(i)+'('+Result+')';
  end;

procedure TRtcAbsGateway.EvOn(Sender:TRtcConnection; UserID:TGateUID; const UserAuth,UserInfo:RtcString);
  begin
  if assigned(FOnUserReady) then
    FOnUserReady(Sender,UserID,UserAuth,UserInfo);
  end;

procedure TRtcAbsGateway.EvOff(Sender:TRtcConnection; UserID:TGateUID; const UserAuth,UserInfo:RtcString);
  begin
  if assigned(FOnUserNotReady) then
    FOnUserNotReady(Sender,UserID,UserAuth,UserInfo);
  end;

function TRtcAbsGateway.GetUserAddr(const Sender: TRtcConnection): RtcString;
  begin
  Result:=Sender.Request.Info.asString[INFO_OWNERIP];
  if Result='' then
    begin
    if Sender.Request.ForwardedFor<>'' then
      Result:=Sender.PeerAddr+'/'+Sender.Request.ForwardedFor
    else
      Result:=Sender.PeerAddr;
    Sender.Request.Info.asString[INFO_OWNERIP]:=Result;
    end;
  end;

function FromAddr(Sender:TRtcConnection): RtcString;
  begin
  if RtcIntPtr(Sender)=0 then
    Result:=''
  else
    try
      Result:='{'+Sender.PeerPort+'}';
    except
      on E:Exception do
        Result:=' {!'+RtcString(E.ClassName)+':'+Utf8Encode(RtcWideString(E.Message))+'!}';
      end;
  end;

function TRtcAbsGateway.crcGetUserID(i:byte; const Sender: TRtcConnection; warn:boolean=False): TGateUID;
  var
    UID:RtcString;
    UIDB:RtcByteArray;
  begin
  try
    UID:='';
    SetLength(UIDB,0);

    Result:=0;

    if not assigned(Sender) then Exit;
    if not assigned(Sender.Request) then Exit;
    if not assigned(Sender.Request.Info) then Exit;

    Result:=Sender.Request.Info.asCardinal[INFO_USERID];
    if (Result<MinInternalUserID) or (Result>MaxInternalUserID) then
      begin
      UID:=Sender.Request.Query['u'];
      if UID<>'' then
        begin
        UIDB:=Int2Bytes(LWordFrom6bit(UID)); // 4 byte
        DeCryptEx(UIDB,RtcStringToBytes(GetUserAddr(Sender)));
        Result:=crcBytes2GateID(UIDB);  // 3 byte + CRC
        if (Result>=MinUserID) and (Result<=MaxUserID) then
          begin
          Result:=Result shl UserIDShift;
          Sender.Request.Info.asCardinal[INFO_USERID]:=Result;
          end
        else
          begin
          if warn and LOG_GATEWAY_STATUS then
            case Result of
              errNoUID: Log('crcGetUserID '+Int2Str(Result)+' "'+UID+'" @'+LID2Text(i)+' -> No UID'+FromAddr(Sender),GATE_LOG+'.UID');
              errBadUID: Log('crcGetUserID '+Int2Str(Result)+' "'+UID+'" @'+LID2Text(i)+' -> Bad UID'+FromAddr(Sender),GATE_LOG+'.UID');
              errBadCRC: Log('crcGetUserID '+Int2Str(Result)+' "'+UID+'" @'+LID2Text(i)+' -> Bad UID CRC'+FromAddr(Sender),GATE_LOG+'.UID');
              else Log('crcGetUserID '+Int2Str(Result)+' "'+UID+'" @'+LID2Text(i)+' -> Bad UID Result '+FromAddr(Sender),GATE_LOG+'.UID');
              end;
          end;
        end
      else
        begin
        if warn and LOG_GATEWAY_STATUS then
          Log('crcGetUserID '+Int2Str(Result)+' "'+UID+'" -> '+Sender.Request.URI+' @'+LID2Text(i)+' -> UID Empty'+FromAddr(Sender),GATE_LOG+'.UID');
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_GATEWAY_ERRORS then
        Log('crcGetUserID @'+LID2Text(i)+' -> '+RtcString(E.ClassName)+':'+Utf8Encode(RtcWideString(E.Message))+FromAddr(Sender),GATE_LOG+'.UID');
      Result:=0;
      end;
    end;
  end;

{$IFDEF RTC_ANON_METHODS}

// procedure(Sender:TRtcConnection; UserID:Cardinal; var UserAuth,UserInfo,SecondaryKey:RtcString)

type
  TRtcGatewayLoginAMContainer = class(TObject)
  public
    MyMethod:TRtcGatewayLoginAnonMethod;
    constructor Create(const AMethod:TRtcGatewayLoginAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection; UserID:Cardinal; var UserAuth,UserInfo,SecondaryKey:RtcString);
    end;

constructor TRtcGatewayLoginAMContainer.Create(const AMethod:TRtcGatewayLoginAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcGatewayLoginAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcGatewayLoginAMContainer.MyEvent(Sender:TRtcConnection; UserID:Cardinal; var UserAuth,UserInfo,SecondaryKey:RtcString);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender, UserID, UserAuth, UserInfo, SecondaryKey);
  end;

function TRtcAbsGateway.Anon(const Event:TRtcGatewayLoginAnonMethod):TRtcGatewayLoginEvent;
  var
    obj:TRtcGatewayLoginAMContainer;
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
        obj:=TRtcGatewayLoginAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// procedure(Sender:TRtcConnection; UserID:Cardinal; const UserAuth,UserInfo:RtcString)

type
  TRtcGatewayNotifyAMContainer = class(TObject)
  public
    MyMethod:TRtcGatewayNotifyAnonMethod;
    constructor Create(const AMethod:TRtcGatewayNotifyAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Sender:TRtcConnection; UserID:Cardinal; const UserAuth,UserInfo:RtcString);
    end;

constructor TRtcGatewayNotifyAMContainer.Create(const AMethod:TRtcGatewayNotifyAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcGatewayNotifyAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcGatewayNotifyAMContainer.MyEvent(Sender:TRtcConnection; UserID:Cardinal; const UserAuth,UserInfo:RtcString);
  begin
  if assigned(MyMethod) then
    MyMethod(Sender, UserID, UserAuth, UserInfo);
  end;

function TRtcAbsGateway.Anon(const Event:TRtcGatewayNotifyAnonMethod):TRtcGatewayNotifyEvent;
  var
    obj:TRtcGatewayNotifyAMContainer;
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
        obj:=TRtcGatewayNotifyAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

{$ENDIF}

{ --- Error procedures --- }

procedure RaiseError(txt: RtcString);
  begin
  raise ERtcGateway.Create(String(txt));
  end;

procedure RaiseFatalError(txt: RtcString);
  begin
  raise ERtcGateFatal.Create(String(txt));
  end;

procedure RaiseErrorClosed(txt: RtcString);
  begin
  raise ERtcGateClosed.Create(String(txt));
  end;

{ TRtcGateOneUser }

constructor TRtcGateOneUser.Create( ReadSpeedLimit,
                                    WriteSpeedLimit:Cardinal;
                                    UID:TGateUID;
                                    const UserAuth:RtcString;
                                    const UserInfo:RtcString;
                                    const ForIP:RtcString;
                                    const CryptKey:RtcString;
                                    Gate:TRtcAbsGateway);
  begin
  inherited Create;

  oWriteCount:=0;
  oWriteTime:=0;
  oWriteLimit:=WriteSpeedLimit;
  oWriteMax:=oWriteLimit*150;

  iReadCount:=0;
  iReadTime:=0;
  iReadLimit:=ReadSpeedLimit;
  iReadMax:=iReadLimit*150;

  inCS:=TRtcCritSec.Create;
  outCS:=TRtcCritSec.Create;

  xGate:=Gate;
  xOwnerIP:=ForIP;

  xUID:=UID;
  xAuth:=UserAuth;
  xInfo:=UserInfo;

  oLastOutput:=GetAppRunTime;
  iLastInput:=oLastOutput;
  oLastPing:=oLastOutput;

  oOutput:=0;
  oFrom:='';
  oOutThr:=nil;
  oOutputReady:=False;
  oCryptOut:=TRtcCrypt.Create;
  oCryptOut.Key:=CryptKey;
  oCryptOut.Init;
  oOutputClear:=False;

  iPaused:=False;
  iWaiting:=False;
  iStreaming:=False;
  iSending:=False;

  oPaused:=False;
  oSending:=False;
  oPrepared:=False;

  iLeftToRead:=0;
  oLeftToSend:=0;

  oiAllReady:=False;
  oiUsed:=False;
  oiRemoved:=False;

  oOutBuff:=nil; // TRtcHugeByteArray.Create;
  oOutSize:=0;
  oWaitingReceivers:=nil; // tObjList.Create(16);
  oInGroups:=nil; // tObjList.Create(16);
  oInFriends:=nil; // tObjList.Create(16);

  iInput:=0;
  iFrom:='';
  iInThr:=nil;
  iInputReady:=False;
  iCryptIn:=TRtcCrypt.Create;
  iCryptIn.Key:=CryptKey;
  iCryptIn.Init;
  iInputClear:=False;

  iInBuff:=nil; // TRtcHugeByteArray.Create;
  iInSize:=0;
  iWaitingOnSenders:=nil; // tObjList.Create(16);
  iOwnGroups:=nil; // tObjList.Create(16);
  iOwnFriends:=nil; // tObjList.Create(16);
  iHostChannels:=nil; // tStrIntList.Create(16);
  iControlChannels:=nil; // tStrIntList.Create(16);
  iPublicChannels:=nil; // tStrIntList.Create(16);

  JobCheckPing:=TRtcThread.GetJobForEvent(ThrCheckPing);
  JobStartSending:=TRtcThread.GetJobForEvent(ThrStartSending);
  JobStartReceiving:=TRtcThread.GetJobForEvent(ThrStartReceiving);
  JobContinueInput:=TRtcThread.GetJobForEvent(ThrContinueInput);
  JobContinueOutput:=TRtcThread.GetJobForEvent(ThrContinueOutput);
  end;

procedure TRtcGateOneUser.CleanUp(perRequest, clearConn, removed:boolean; Conn:RtcIntPtr; resInput,resOutput:boolean; Releasing:boolean=False);
  var
    Sender:TRtcConnection absolute Conn;
    wasOpen:boolean;
  begin
  if not (resInput or resOutput) then Exit;

  if resInput and (perRequest or (iInput=Conn) or (iInput=0) or (Conn=0)) then
    begin
    ioRemoveAllFriends(True,False);
    ioRemoveAllGroupsOwnedByUser(True,False,Releasing);
    ixoUnSubscribeFromAll(True,False);
    ioRemoveSenders;
    end;

  if resOutput and (perRequest or (oOutput=Conn) or (oOutput=0) or (Conn=0)) then
    begin
    oiRemoveUserFromAllFriends(True,False);
    ooRemoveUserFromAllGroups(True,False);
    oiNotifyReceivers;
    end;

  {$IFDEF RTCGATE_DEBUG}LockInput(255);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    {$IFDEF RTCGATE_DEBUG}LockOutput(255);{$ELSE}outCS.Acquire;{$ENDIF}
    try
      wasOpen:=oiAllReady;

      if resInput and (perRequest or (iInput=Conn) or (iInput=0) or (Conn=0)) then
        begin
        if removed then oiRemoved:=True;

        if clearConn then
          begin
          oiAllReady:=False;
          iInputReady:=False;
          iInput:=0;
          iFrom:='';
          iInThr:=nil;
          iLeftToRead:=0;
          iWaiting:=False;
          iSending:=False;
          iStreaming:=False;
          iPaused:=False;
          {$IFDEF RTCGATE_DEBUG}xLog('CleanUp(I+): AllReady:=FALSE; iInput:=0; iWaiting:=FALSE, iPaused:=FALSE'+FromAddr(Sender),IntToStr(xUID shr UserIDShift));{$ENDIF}
          end
        else
          begin
          oiAllReady:=False;
          iInputReady:=False;
          iWaiting:=False;
          iSending:=False;
          iStreaming:=False;
          iPaused:=False;
          {$IFDEF RTCGATE_DEBUG}xLog('CleanUp(I-): AllReady:=FALSE; iInput:={'+IntToStr(iInput)+'}; iWaiting:=FALSE, iPaused:=FALSE'+FromAddr(Sender),IntToStr(xUID shr UserIDShift));{$ENDIF}
          end;

        RtcFreeAndNil(iInBuff);
        iInSize:=0;

        RtcFreeAndNil(iOwnFriends);
        RtcFreeAndNil(iHostChannels);
        RtcFreeAndNil(iControlChannels);
        RtcFreeAndNil(iPublicChannels);
        RtcFreeAndNil(iWaitingOnSenders);
        if Releasing then RtcFreeAndNil(iOwnGroups);

        iCryptIn.Init;
        iInputClear:=perRequest and (iInput<>0);
        {$IFDEF RTCGATE_FULLDEBUG}
        if iInputClear then
          xLog('CleanUp(I): iInputClear:=TRUE'+FromAddr(Sender),IntToStr(xUID shr UserIDShift))
        else
          xLog('CleanUp(.): iInputClear:=FALSE'+FromAddr(Sender),IntToStr(xUID shr UserIDShift));
        {$ENDIF}
        end;

      if resOutput and (perRequest or (oOutput=Conn) or (oOutput=0) or (Conn=0)) then
        begin
        if removed then oiRemoved:=True;

        if clearConn then
          begin
          oiAllReady:=False;
          oOutputReady:=False;
          oOutput:=0;
          oFrom:='';
          oOutThr:=nil;
          oLeftToSend:=0;
          oSending:=False;
          oPrepared:=False;
          oPaused:=False;
          {$IFDEF RTCGATE_DEBUG}xLog('CleanUp(O+): AllReady:=FALSE; oOutput:=0; oSending:=False; oPaused:=False'+FromAddr(Sender),IntToStr(xUID shr UserIDShift));{$ENDIF}
          end
        else
          begin
          oiAllReady:=False;
          oOutputReady:=False;
          oSending:=False;
          oPrepared:=False;
          oPaused:=False;
          {$IFDEF RTCGATE_DEBUG}xLog('CleanUp(O-): AllReady:=FALSE; oOutput:={'+IntToStr(oOutput)+'}; oSending:=False; oPaused:=False'+FromAddr(Sender),IntToStr(xUID shr UserIDShift));{$ENDIF}
          end;

        RtcFreeAndNil(oInGroups);
        RtcFreeAndNil(oWaitingReceivers);
        RtcFreeAndNil(oInFriends);
        RtcFreeAndNil(oOutBuff);
        oOutSize:=0;

        oCryptOut.Init;
        oOutputClear:=perRequest and (oOutput<>0);
        {$IFDEF RTCGATE_FULLDEBUG}
        if oOutputClear then
          xLog('CleanUp(O): oOutputClear:=TRUE'+FromAddr(Sender),IntToStr(xUID shr UserIDShift))
        else
          xLog('CleanUp(.): oOutputClear:=FALSE'+FromAddr(Sender),IntToStr(xUID shr UserIDShift));
        {$ENDIF}
        end;

    finally
      {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;

  if wasOpen then
    if assigned(xGate) then
      xGate.EvOff(Sender,xUID shr UserIDShift,xAuth,xInfo);
  end;

destructor TRtcGateOneUser.Destroy;
  begin
  CleanUp(False,True,True,0,True,True,True);

  RtcFreeAndNil(oCryptOut);
  RtcFreeAndNil(iCryptIn);

  RtcFreeAndNil(outCS);
  RtcFreeAndNil(inCS);

  RtcFreeAndNil(JobCheckPing);
  RtcFreeAndNil(JobStartSending);
  RtcFreeAndNil(JobStartReceiving);
  RtcFreeAndNil(JobContinueInput);
  RtcFreeAndNil(JobContinueOutput);

  xGate:=nil;

  inherited;
  end;

{$IFDEF RTCGATE_DEBUG}
procedure TRtcGateOneUser.LockInput(i:byte);
  begin
  try
    if assigned(inCS) then
      inCS.Acquire
    else
      raise ERtcGateFatal.Create('inCS=NIL @'+LID2Text(i));
    xLog('+I'+LID2Text(i)+iFrom,IntToStr(xUID shr UserIDShift))
  except
    on E:Exception do
      begin
      if LOG_GATEWAY_ERRORS then
        Log('LockInput @'+LID2Text(i)+iFrom,E,GATE_LOG+'.LOCK');
      raise;
      end;
    end;
  end;
procedure TRtcGateOneUser.LockOutput(i:byte);
  begin
  try
    if assigned(outCS) then
      outCS.Acquire
    else
      raise ERtcGateFatal.Create('outCS=NIL @'+LID2Text(i));
    xLog('+O'+LID2Text(i)+oFrom,IntToStr(xUID shr UserIDShift));
  except
    on E:Exception do
      begin
      if LOG_GATEWAY_ERRORS then
        Log('LockOutput @'+LID2Text(i)+oFrom,E,GATE_LOG+'.LOCK');
      raise;
      end;
    end;
  end;
procedure TRtcGateOneUser.UnLockInput;
  begin
  try
    xLog('-i'+iFrom,IntToStr(xUID shr UserIDShift));
    if assigned(inCS) then
      inCS.Release
    else
      raise ERtcGateFatal.Create('inCS=NIL');
  except
    on E:Exception do
      begin
      if LOG_GATEWAY_ERRORS then
        Log('UnLockInput'+iFrom,E,GATE_LOG+'.LOCK');
      raise;
      end;
    end;
  end;
procedure TRtcGateOneUser.UnLockOutput;
  begin
  try
    xLog('-o'+oFrom,IntToStr(xUID shr UserIDShift));
    if assigned(outCS) then
      outCS.Release
    else
      raise ERtcGateFatal.Create('outCS=NIL');
  except
    on E:Exception do
      begin
      if LOG_GATEWAY_ERRORS then
        Log('UnLockOutput'+oFrom,E,GATE_LOG+'.LOCK');
      raise;
      end;
    end;
  end;
{$ENDIF}

procedure TRtcGateOneUser.ThrStartSending;
  var
    Conn:RtcIntPtr;
    Sender:TRtcConnection absolute Conn;
    doit:boolean;
  begin
  Conn:=0;
  try
    {$IFDEF RTCGATE_DEBUG}LockOutput(224);{$ELSE}outCS.Acquire;{$ENDIF}
    try
      doit:=oiAllReady and oPrepared and (oOutput<>0) and (oLeftToSend>0) and
            assigned(xGate) and not (oSending or oPaused);
      if doit then Conn:=oOutput;
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
      end;
    if doit then
      oiDataSent(Conn);
  except
    on E:Exception do
      if assigned(xGate) and (Conn<>0) then
        xGate.ErrorResponse(Sender,'ThrStartSending',E);
    end;
  end;

procedure TRtcGateOneUser.ThrStartReceiving;
  var
    Conn:RtcIntPtr;
    Sender:TRtcConnection absolute Conn;
    doit:boolean;
  begin
  Conn:=0;
  try
    {$IFDEF RTCGATE_DEBUG}LockInput(124);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      doit:=oiAllReady and iWaiting and (iInput<>0) and
            assigned(xGate) and not (iStreaming or iPaused);
      if doit then Conn:=iInput;
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      end;
    if doit then
      xGate.ReadAndProcessData(False,Conn,self);
  except
    on E:Exception do
      if assigned(xGate) and (Conn<>0) then
        xGate.ErrorResponse(Sender,'ThrStartReceiving',E);
    end;
  end;

procedure TRtcGateOneUser.PostInputReady;
  begin
  if oiAllReady and iWaiting and not (iStreaming or iPaused) and (iInput<>0) then
    begin
    {$IFDEF RTCGATE_DEBUG}xLog('!InReady'+iFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
    if assigned(iInThr) then
      TRtcThread.PostJob(iInThr,JobStartReceiving)
    else if not xGate.isMultiThreaded then
      ThrStartReceiving;
    end{$IFDEF RTCGATE_DEBUG} else xLog('NP!InReady'+iFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
  end;

procedure TRtcGateOneUser.PostStartReceiving;
  begin
  if iStreaming then
    begin
    iStreaming:=False;
    {$IFDEF RTCGATE_DEBUG}xLog('P-StartRec: iStreaming:=False'+iFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
    if oiAllReady and iWaiting and not iPaused and (iInput<>0) then
      begin
      {$IFDEF RTCGATE_DEBUG}xLog('!StartRec'+iFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
      if assigned(iInThr) then
        TRtcThread.PostJob(iInThr,JobStartReceiving)
      else if not xGate.isMultiThreaded then
        ThrStartReceiving;
      end;
    end{$IFDEF RTCGATE_DEBUG} else xLog('NP!StartRec iStreaming=False'+iFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
  end;

procedure TRtcGateOneUser.PostOutputReady;
  begin
  if oiAllReady and oPrepared and not (oSending or oPaused) and (oOutput<>0) then
    begin
    {$IFDEF RTCGATE_DEBUG}xLog('!OutReady'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
    if assigned(oOutThr) then
      TRtcThread.PostJob(oOutThr,JobStartSending)
    else if not xGate.isMultiThreaded then
      ThrStartSending;
    end{$IFDEF RTCGATE_DEBUG} else xLog('NP!OutReady'+oFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
  end;

procedure TRtcGateOneUser.PostStartSending;
  begin
  if not oPrepared then
    begin
    oPrepared:=True;
    {$IFDEF RTCGATE_DEBUG}xLog('P-StartSend: oPrepared:=True'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
    if oiAllReady and not (oSending or oPaused) and (oOutput<>0) then
      begin
      {$IFDEF RTCGATE_DEBUG}xLog('!StartSend'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
      if assigned(oOutThr) then
        TRtcThread.PostJob(oOutThr,JobStartSending)
      else if not xGate.isMultiThreaded then
        ThrStartSending;
      end;
    end{$IFDEF RTCGATE_DEBUG} else xLog('NP!StartSend oPrepared=True'+oFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
  end;

procedure TRtcGateOneUser.ThrCheckPing;
  var
    MyTime:TAppRunTime;
  begin
  MyTime:=GetAppRunTime;
  {$IFDEF RTCGATE_DEBUG}LockOutput(236);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if (MyTime>oLastPing+SENDPING_INTERVAL) and
       (MyTime>oLastOutput+SENDPING_INTERVAL) then
      oSendPing(Cmd_UserOn, xUID);
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ThrContinueInput;
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput( 63);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if iPaused then
      begin
      iPaused:=False;
      {$IFDEF RTCGATE_DEBUG}xLog('P-ContIN: iPaused:=False'+iFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
      if oiAllReady and iWaiting and (iInput<>0) then
        begin
        iLastInput:=GetAppRunTime;
        if not iStreaming then
          begin
          {$IFDEF RTCGATE_DEBUG}xLog('!ContIN'+iFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
          if assigned(iInThr) then
            TRtcThread.PostJob(iInThr,JobStartReceiving)
          else if not xGate.isMultiThreaded then
            ThrStartReceiving;
          end;
        end;
      end{$IFDEF RTCGATE_DEBUG} else xLog('NP!ContIN iStreaming=False'+iFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ThrContinueOutput;
  begin
  {$IFDEF RTCGATE_DEBUG}LockOutput( 64);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if oPaused then
      begin
      oPaused:=False;
      {$IFDEF RTCGATE_DEBUG}xLog('P-ContOUT: oPaused:=False'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
      if oiAllReady and not oSending and (oOutput<>0) then
        begin
        oLastOutput:=GetAppRunTime;
        if oPrepared then
          begin
          {$IFDEF RTCGATE_DEBUG}xLog('!ContOUT'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
          if assigned(oOutThr) then
            TRtcThread.PostJob(oOutThr,JobStartSending)
          else if not xGate.isMultiThreaded then
            ThrStartSending;
          end;
        end;
      end{$IFDEF RTCGATE_DEBUG} else xLog('NP!ContOUT oPaused=False'+oFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.PostCheckPing;
  begin
  {$IFDEF RTCGATE_DEBUG}LockOutput(237);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if assigned(oOutThr) then
      TRtcThread.PostJob(oOutThr,JobCheckPing)
    else if not xGate.isMultiThreaded then
      ThrCheckPing;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.PostContinueInput;
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput( 61);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if assigned(iInThr) then
      TRtcThread.PostJob(iInThr,JobContinueInput)
    else if not xGate.isMultiThreaded then
      ThrContinueInput;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.PostContinueOutput;
  begin
  {$IFDEF RTCGATE_DEBUG}LockOutput( 62);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if assigned(oOutThr) then
      TRtcThread.PostJob(oOutThr,JobContinueOutput)
    else if not xGate.isMultiThreaded then
      ThrContinueOutput;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.oRemoveReceiver(UID:TGateUID);
  begin
  {$IFDEF RTCGATE_DEBUG}LockOutput( 41);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    removeUser(oWaitingReceivers,UID);
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ioRemoveSenders;
  var
    sndID:TGateUID;
    objSnd:TObject;
    oSend:TRtcGateOneUser absolute objSnd;
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput(  1);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if assigned(iWaitingOnSenders) then
      begin
      sndID:=iWaitingOnSenders.search_min(objSnd);
      while (sndID>0) and assigned(objSnd) do
        begin
        iWaitingOnSenders.remove(sndID);
        oSend.oRemoveReceiver(xUID);
        if not assigned(iWaitingOnSenders) then Break;
        sndID:=iWaitingOnSenders.search_min(objSnd);
        end;
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.iNotifyTheReceiver(UID:TGateUID);
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput(  2);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if removedLastUser(iWaitingOnSenders,UID) then
      PostStartReceiving
  {$IFDEF RTCGATE_DEBUG}else
      xLog('OLD WaitingSender (='+IntToStr(iWaitingOnSenders.Count)+') #'+
                         IntToStr(UID shr UserIDShift)+iFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.oiNotifyReceivers;
  var
    recID:TGateUID;
    objRec:TObject;
    oRecv:TRtcGateOneUser absolute objRec;
  begin
  {$IFDEF RTCGATE_DEBUG}LockOutput( 42);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if assigned(oWaitingReceivers) then
      begin
      recID:=oWaitingReceivers.search_min(objRec);
      while (recID>0) and assigned(objRec) do
        begin
        oWaitingReceivers.remove(recID);
        {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
        try
          oRecv.iNotifyTheReceiver(xUID);
        finally
          {$IFDEF RTCGATE_DEBUG}LockOutput( 43);{$ELSE}outCS.Acquire;{$ENDIF}
          end;
        if not assigned(oWaitingReceivers) then Break;
        recID:=oWaitingReceivers.search_min(objRec);
        end;
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

function TRtcGateOneUser.insertUser(var userList:tObjList; UID:TGateUID; obj:TObject):boolean;
  begin
  Result:=False;
  if not assigned(userList) then
    userList:=tObjList.Create(16);
  if userList.search(UID)=nil then
    begin
    userList.insert(UID, obj);
    Result:=True;
    end;
  end;

function TRtcGateOneUser.removeUser(var userList:tObjList; UID:TGateUID):boolean;
  begin
  Result:=False;
  if assigned(userList) then
    if userList.search(UID)<>nil then
      begin
      userList.remove(UID);
      Result:=True;
      end;
  end;

function TRtcGateOneUser.findUser(var userList:tObjList; UID:TGateUID):TObject;
  begin
  if assigned(userList) then
    Result:=userList.search(UID)
  else
    Result:=nil;
  end;

function TRtcGateOneUser.removedLastUser(var userList:tObjList; UID:TGateUID):boolean;
  begin
  Result:=False;
  if not assigned(userList) then
    Result:=True
  else if removeUser(userList,UID) then
    Result:=userList.Empty
  end;

procedure TRtcGateOneUser.prepareBuffer(var buff:TRtcHugeByteArray);
  begin
  if not assigned(buff) then
    buff:=TRtcHugeByteArray.Create;
  end;

procedure TRtcGateOneUser.izNewWaitingReceiver(ReceiverUID:TGateUID; oReceiver:TRtcGateOneUser);
  begin
  if insertUser(iWaitingOnSenders,ReceiverUID,oReceiver) then
    begin
    iStreaming:=True;
    {$IFDEF RTCGATE_DEBUG}xLog('iStreaming:=True! WaitingOnSender (='+IntToStr(iWaitingOnSenders.Count)+') #'+
                               IntToStr(ReceiverUID shr UserIDShift)+iFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
    end
  {$IFDEF RTCGATE_DEBUG}else if iStreaming then
    xLog('iStreaming==TRUE / was WaitingOnSender (='+IntToStr(iWaitingOnSenders.Count)+') #'+
          IntToStr(ReceiverUID shr UserIDShift)+iFrom,IntToStr(xUID shr UserIDShift))
  else
    xLog('iStreaming==FALSE / was WaitingOnSender (='+IntToStr(iWaitingOnSenders.Count)+') #'+
          IntToStr(ReceiverUID shr UserIDShift)+iFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
  end;

function TRtcGateOneUser.ozSendFirst(oFromUser:TRtcGateOneUser;
                                    SenderUID, FromUID: TGateUID; TotalSize: Cardinal;
                                    const data: RtcByteArray;
                                    len, loc: Cardinal;
                                    ext:byte):integer;
  begin
  Result:=0;
  if ext>0 then
    begin
    if TotalSize=len then
      ext:=Cmd_Send_AllG
    else
      ext:=Cmd_Send_FirstG;
    end
  else
    begin
    if TotalSize=len then
      ext:=Cmd_SendAll
    else
      ext:=Cmd_SendFirst;
    end;

  {$IFDEF RTCGATE_DEBUG}LockOutput(231);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if oiAllReady then
      begin
      prepareBuffer(oOutBuff);
      oOutBuff.AddEx(crcGateCmd2Bytes(ext,FromUID));
      oOutBuff.AddEx(crcInt2Bytes(len));
      oOutBuff.AddEx(data,len,loc);
      oOutSize:=oOutBuff.Size;

      if insertUser(oWaitingReceivers,SenderUID,oFromUser) then
        oFromUser.izNewWaitingReceiver(xUID,self)
      {$IFDEF RTCGATE_DEBUG} else
        xLog('ALREADY WaitingReceiver (='+IntToStr(oWaitingReceivers.Count)+') #'+
             IntToStr(SenderUID shr UserIDShift)+oFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};

      Result:=1;
      PostStartSending;
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

function TRtcGateOneUser.ozSendMore(oFromUser:TRtcGateOneUser;
                                   SenderUID,FromUID: TGateUID; LeftToSend: Cardinal;
                                   const data: RtcByteArray;
                                   len, loc: Cardinal;
                                   ext:byte):integer;
  begin
  Result:=0;
  if ext>0 then
    begin
    if LeftToSend=len then
      ext:=Cmd_Send_LastG
    else
      ext:=Cmd_Send_MoreG;
    end
  else
    begin
    if LeftToSend=len then
      ext:=Cmd_SendLast
    else
      ext:=Cmd_SendMore;
    end;

  {$IFDEF RTCGATE_DEBUG}LockOutput(232);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if oiAllReady then
      begin
      prepareBuffer(oOutBuff);
      oOutBuff.AddEx(crcGateCmd2Bytes(ext,FromUID));
      oOutBuff.AddEx(crcInt2Bytes(len));
      oOutBuff.AddEx(data,len,loc);
      oOutSize:=oOutBuff.Size;

      if insertUser(oWaitingReceivers,SenderUID,oFromUser) then
        oFromUser.izNewWaitingReceiver(xUID,self)
      {$IFDEF RTCGATE_DEBUG} else
        xLog('ALREADY WaitingReceiver (='+IntToStr(oWaitingReceivers.Count)+') #'+
             IntToStr(SenderUID shr UserIDShift)+oFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};

      Result:=1;
      PostStartSending;
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

function TRtcGateOneUser.oPostNote(Cmd:byte; FromUID: TGateUID; warn:boolean=True):integer;
  begin
  Result:=0;
  {$IFDEF RTCGATE_DEBUG}LockOutput(233);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if oiAllReady then
      begin
      prepareBuffer(oOutBuff);
      oOutBuff.AddEx(crcGateCmd2Bytes(Cmd,FromUID));
      oOutSize:=oOutBuff.Size;
      
      Result:=1;
      PostStartSending;
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

function TRtcGateOneUser.oPostInfo(Cmd:byte; FromUID: TGateUID; const FromInfo:RtcString):integer;
  begin
  Result:=0;
  {$IFDEF RTCGATE_DEBUG}LockOutput(234);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if oiAllReady then
      begin
      prepareBuffer(oOutBuff);
      oOutBuff.AddEx(crcGateCmd2Bytes(Cmd,FromUID)); // 6 bytes
      oOutBuff.AddEx(crcInt2Bytes(length(FromInfo))); // 4 bytes
      oOutBuff.Add(FromInfo);
      oOutSize:=oOutBuff.Size;

      Result:=1;
      PostStartSending;
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

function TRtcGateOneUser.oSendPing(Cmd:byte; FromUID:TGateUID):integer;
  begin
  Result:=0;
  {$IFDEF RTCGATE_DEBUG}LockOutput(235);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    oLastPing:=GetAppRunTime;
    if oiAllReady and (oOutput<>0) and not (oSending or oPrepared or oPaused) and (oOutSize=0) then
      begin
      prepareBuffer(oOutBuff);
      if oOutBuff.Size=0 then
        begin
        Result:=1;
        oOutBuff.AddEx(crcGateCmd2Bytes(Cmd,FromUID));
        oOutSize:=oOutBuff.Size;

        PostStartSending;
        end;
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.oNotifyRemovalFromGroup(gid:TGateUID; Notify:boolean);
  begin
  {$IFDEF RTCGATE_DEBUG}LockOutput( 44);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if removeUser(oInGroups,gid) then
      if Notify then
        oPostNote(Cmd_UserOut,gid,False);
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.oNotifyRemovalFromFriends(fid:TGateUID; Notify:boolean);
  begin
  {$IFDEF RTCGATE_DEBUG}LockOutput( 45);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if removeUser(oInFriends,fid) then
      if Notify then
        oPostNote(Cmd_UserUnFriend,fid,False);
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ioNotifyRemovalAsFriend(fid:TGateUID; Notify:boolean);
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput(  3);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if removeUser(iOwnFriends,fid) then
      if Notify then
        oPostNote(Cmd_FriendRemove,fid,False);
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ioRemoveAllGroupsOwnedByUser(NotifyUsers, NotifyOwner, Releasing: boolean);
  var
    gid,uid:TGateUID;
    objGr,objUsr:TObject;
    oGroup:TRtcGateUserGroup absolute objGr;
    oUser:TRtcGateOneUser absolute objUsr;
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput(  4);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if not assigned(iOwnGroups) then Exit;

    gid:=iOwnGroups.search_min(ObjGr);
    while (gid>0) and assigned(objGr) do
      begin
      oGroup.LockGroup(1);
      try
        if assigned(oGroup.grUsers) then
          begin
          uid:=oGroup.grUsers.search_min(objUsr);
          while (uid>0) and assigned(objUsr) do
            begin
            oGroup.grUsers.remove(uid);
            oUser.oNotifyRemovalFromGroup(gid,
                  (NotifyUsers and (uid<>xUID)) or (NotifyOwner and (uid=xUID)) );
            if NotifyOwner then
              oPostNote(Cmd_UserOff,(gid and GroupIDMask) or uid,False);
            if not assigned(oGroup.grUsers) then Break;
            uid:=oGroup.grUsers.search_min(objUsr);
            end;
          end;
      finally
        oGroup.UnLockGroup;
        end;
      if Releasing then
        begin
        iOwnGroups.remove(gid);
        RtcFreeAndNil(objGr);
        gid:=iOwnGroups.search_min(ObjGr);
        end
      else
        begin
        ObjGr:=nil;
        gid:=iOwnGroups.search_g(gid,ObjGr);
        end;
      end;
    if Releasing then
      RtcFreeAndNil(iOwnGroups);
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ooRemoveUserFromAllGroups(NotifyOwners, NotifyUser:boolean);
  var
    gid,OwnerID:TGateUID;
    objGr,objOwn:TObject;
    oGroup:TRtcGateUserGroup absolute objGr;
    oUserOwn:TRtcGateOneUser absolute objOwn;
  begin
  {$IFDEF RTCGATE_DEBUG}LockOutput( 46);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if not assigned(oInGroups) then Exit;

    gid:=oInGroups.search_min(objGr);
    while (gid>0) and assigned(objGr) do
      begin
      OwnerID:=gid and UserIDMask;
      {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
      try
        oGroup.LockGroup(  2);
        try
          if assigned(oGroup.grUsers) then
            if oGroup.grUsers.search(xUID)<>nil then
              begin
              oGroup.grUsers.remove(xUID);
              oNotifyRemovalFromGroup(gid,
                      (NotifyUser and (xUID<>OwnerID)) or
                      (NotifyOwners and (xUID=OwnerID)) );
              if NotifyOwners then
                begin
                objOwn:=oGroup.grOwner;
                if assigned(ObjOwn) then
                  oUserOwn.oPostNote(Cmd_UserOff,(gid and GroupIDMask) or xUID,False);
                end;
              end;
        finally
          oGroup.UnLockGroup;
          end;
      finally
        {$IFDEF RTCGATE_DEBUG}LockOutput( 47);{$ELSE}outCS.Acquire;{$ENDIF}
        end;
      if not assigned(oInGroups) then Break;
      gid:=oInGroups.search_min(objGr);
      end;

    RtcFreeAndNil(oInGroups);
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ooRemoveUserFromGroup(GroupID: TGateUID;
                                               NotifyUser, NotifyOwner: boolean);
  var
    objGr,objOwn:TObject;
    OwnerID:TGateUID;
    oGroup:TRtcGateUserGroup absolute objGr;
    oUserOwn:TRtcGateOneUser absolute objOwn;
    found:boolean;
  begin
  if (GroupID and UserIDMask)=GroupID then
    raise ERtcGateway.Create('InternalRemoveUserFromGroup - GroupID=OwnerID');

  OwnerID:=GroupID and UserIDMask;

  {$IFDEF RTCGATE_DEBUG}LockOutput( 48);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if not assigned(oInGroups) then
      begin
      objGr:=nil;
      found:=False;
      end
    else
      begin
      objGr:=oInGroups.search(GroupID);
      found:=assigned(objGr);
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;

  if found then
    begin
    oGroup.LockGroup(3);
    try
      if oGroup.grUsers.search(xUID)<>nil then
        begin
        oGroup.grUsers.remove(xUID);
        oNotifyRemovalFromGroup(GroupID,
            (NotifyUser and (xUID<>OwnerID)) or
            (NotifyOwner and (xUID=OwnerID)) );
        if NotifyOwner then
          begin
          objOwn:=oGroup.grOwner;
          if assigned(ObjOwn) then
            oUserOwn.oPostNote(Cmd_UserOff,(GroupID and GroupIDMask) or xUID,False);
          end;
        end;
    finally
      oGroup.UnLockGroup;
      end;
    end;
  end;

procedure TRtcGateOneUser.ioRemoveGroup(GroupID: TGateUID;
                                       NotifyUsers, NotifyOwner:boolean);
  var
    uid:TGateUID;
    objGr,objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
    oGroup:TRtcGateUserGroup absolute objGr;
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput(  5);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if not assigned(iOwnGroups) then Exit;

    objGr:=iOwnGroups.search(GroupID);
    if assigned(objGr) then
      begin
      oGroup.LockGroup(4);
      try
        if assigned(oGroup.grUsers) then
          begin
          uid:=oGroup.grUsers.search_min(objUsr);
          while (uid>0) and assigned(objUsr) do
            begin
            oGroup.grUsers.remove(uid);
            oUser.oNotifyRemovalFromGroup(GroupID,
                  (NotifyUsers and (uid<>xUID)) or
                  (NotifyOwner and (uid=xUID)) );
            if NotifyOwner then
              oPostNote(Cmd_UserOff,(GroupID and GroupIDMask) or uid,False);
            if not assigned(oGroup.grUsers) then Break;
            uid:=oGroup.grUsers.search_min(objUsr);
            end;
          end;
      finally
        oGroup.UnLockGroup;
        end;
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ioRemoveUserAsFriend(FriendID: TGateUID; NotifyFriend, NotifyUser: boolean);
  var
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
  begin
  if (FriendID and UserIDMask)<>FriendID then
    raise ERtcGateway.Create('InternalRemoveUserAsFriend - Bad FriendID');

  {$IFDEF RTCGATE_DEBUG}LockInput(  6);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if not assigned(iOwnFriends) then Exit;

    objUsr:=iOwnFriends.search(FriendID);
    if assigned(objUsr) then
      begin
      iOwnFriends.remove(FriendID);
      oUser.oNotifyRemovalFromFriends(FriendID,NotifyFriend);
      if NotifyUser then
        oPostNote(Cmd_FriendRemove,FriendID,False);
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ioRemoveAllFriends(NotifyFriends, NotifyUser: boolean);
  var
    FriendID:TGateUID;
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput(  7);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if not assigned(iOwnFriends) then Exit;

    FriendID:=iOwnFriends.search_min(objUsr);
    while (FriendID>0) and assigned(objUsr) do
      begin
      iOwnFriends.remove(FriendID);
      oUser.oNotifyRemovalFromFriends(xUID,NotifyFriends);
      if NotifyUser then
        oPostNote(Cmd_FriendRemove,FriendID,False);
      if not assigned(iOwnFriends) then Break;
      FriendID:=iOwnFriends.search_min(objUsr);
      end;

    RtcFreeAndNil(iOwnFriends);
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.oiRemoveUserFromAllFriends(NotifyFriends, NotifyUser: boolean);
  var
    FriendID:TGateUID;
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
  begin
  {$IFDEF RTCGATE_DEBUG}LockOutput( 49);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if not assigned(oInFriends) then Exit;

    FriendID:=oInFriends.search_min(objUsr);
    while (FriendID>0) and assigned(objUsr) do
      begin
      oInFriends.remove(FriendID);
      if NotifyUser then
        oPostNote(Cmd_UserUnFriend,FriendID,False);
      {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
      try
        oUser.ioNotifyRemovalAsFriend(xUID,NotifyFriends);
      finally
        {$IFDEF RTCGATE_DEBUG}LockOutput( 50);{$ELSE}outCS.Acquire;{$ENDIF}
        end;
      if not assigned(oInFriends) then Break;
      FriendID:=oInFriends.search_min(objUsr);
      end;

    RtcFreeAndNil(oInFriends);
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ioOpenInput(Conn:RtcIntPtr);
  var
    Sender:TRtcConnection absolute Conn;
    wasOpen,isOpen:boolean;
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput(101);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if iInput<>Conn then
      RaiseError('IN CONN Changed'+FromAddr(Sender)+'/'+iFrom)
    else if not iInputClear then
      RaiseError('Input NOT Clear'+FromAddr(Sender)+'/'+iFrom);

    iPaused:=False;
    iWaiting:=False;
    {$IFDEF RTCGATE_FULLDEBUG}xLog('OpenInput: iWaiting:=False, iPaused:=False'+iFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
    iLastInput:=GetAppRunTime;
    iLeftToRead:=Sender.Request.ContentLength;
    {$IFDEF RTCGATE_DEBUG}LockOutput(101);{$ELSE}outCS.Acquire;{$ENDIF}
    try
      wasOpen:=oiAllReady;
      iInputReady:=iInputClear;
      oiAllReady:=iInputReady and oOutputReady;
      isOpen:=oiAllReady;
      if isOpen and not wasOpen then
        begin
        oiUsed:=True;
        {$IFDEF RTCGATE_DEBUG}xLog('OpenInput: AllReady:=TRUE'+iFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
        PostOutputReady;
        end;
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;

  if isOpen and not wasOpen then
    if assigned(xGate) then
      xGate.EVOn(Sender,xUID shr UserIDShift,xAuth,xInfo);
  end;

procedure TRtcGateOneUser.oOpenOutput(Conn:RtcIntPtr);
  var
    Sender:TRtcConnection absolute Conn;
  begin
  {$IFDEF RTCGATE_DEBUG}LockOutput(201);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if oOutput<>Conn then
      RaiseError('OUT CONN Changed'+FromAddr(Sender)+'/'+oFrom)
    else if not oOutputClear then
      RaiseError('Output NOT Clear'+FromAddr(Sender)+'/'+oFrom);

    oSending:=False;
    oPaused:=False;
    {$IFDEF RTCGATE_FULLDEBUG}xLog('oOpenOutput: oSending:False; oPaused:=False'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}

    oLastOutput:=GetAppRunTime;
    oLastPing:=oLastOutput;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ixoActivateOutput(Conn:RtcIntPtr);
  var
    Sender:TRtcConnection absolute Conn;
    wasOpen,isOpen:boolean;
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput(202);{$ELSE}inCS.Acquire;{$ENDIF}
  {$IFDEF RTCGATE_DEBUG}LockOutput(202);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    try
      if oOutput<>Conn then
        RaiseError('OUT CONN Changed'+FromAddr(Sender)+'/'+oFrom)
      else if not oOutputClear then
        RaiseError('Output NOT Clear'+FromAddr(Sender)+'/'+oFrom);
      oLastOutput:=GetAppRunTime;
      oLastPing:=oLastOutput;

      wasOpen:=oiAllReady;
      oOutputReady:=oOutputClear;
      oiAllReady:=iInputReady and oOutputReady;
      isOpen:=oiAllReady;

      if isOpen and not wasOpen then
        begin
        oiUsed:=True;
        {$IFDEF RTCGATE_DEBUG}xLog('ActivateOutput: AllReady:=TRUE'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
        PostInputReady;
        end;
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      end;

    if Sender.Request.ContentLength=0 then
      oLeftToSend:=0
    else
      begin
      oLeftToSend:=LWordFrom6bit(Sender.Read);
      if oLeftToSend=0 then
        oLeftToSend:=DEF_OUTSTREAM_SIZE;
      prepareBuffer(oOutBuff);
      if oOutBuff.Size<PING_PACKET_SIZE then
        oOutBuff.AddEx(crcGateCmd2Bytes(Cmd_UserOn,xUID)); // add a "PING"
      oOutSize:=oOutBuff.Size;
      oPrepared:=True;
      if oOutSize>oLeftToSend then
        oLeftToSend:=oOutBuff.Size;
      if oLeftToSend<=PING_PACKET_SIZE then // we have only 1 "PING" worth of data left to send?
        oLeftToSend:=oLeftToSend+PING_PACKET_SIZE; // extend the buffer for 1 more PING
      end;

    oSending:=True;
    Sender.Response.ContentLength:=oLeftToSend;
    {$IFDEF RTCGATE_DEBUG}xLog('Send! Len='+IntToStr(oLeftToSend)+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;

  Sender.Write; // send header

  if isOpen and not wasOpen then
    if assigned(xGate) then
      xGate.EVOn(Sender,xUID shr UserIDShift,xAuth,xInfo);
  end;

procedure TRtcGateOneUser.oiDataSent(Conn:RtcIntPtr; direct:boolean=False);
  var
    Sender:TRtcConnection absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    maxLen:Cardinal;
    data:RtcByteArray;
    lData:Cardinal;
    tellRecv:boolean;
  begin
  SetLength(data,0);
  lData:=0;

  {$IFDEF RTCGATE_DEBUG}LockOutput(240);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if oOutput<>Conn then
      begin
      if direct and oiAllReady then
        RaiseError('OUT CONN Changed'+FromAddr(Sender)+'/'+oFrom)
      else
        Exit;
      end
    else if direct<>oSending then
      begin
      {$IFDEF RTCGATE_DEBUG}xLog('oiDataSent: direct<>oSending! #EXIT'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
      Exit;
      end;

    if direct then
      begin
      oSending:=False;
      {$IFDEF RTCGATE_FULLDEBUG}xLog('oiDataSent: oSending:=FALSE'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
      end;

    if oiAllReady and (oLeftToSend>0) then
      if assigned(oOutBuff) and (oOutBuff.Size>0) then // Output buffer waiting?
        if oBeforeWrite then
          begin
          maxLen:=oLeftToSend;
          if maxLen>Cardinal(SOCK_MAX_SEND_SIZE) then // send 1 packet at a time
            maxLen:=SOCK_MAX_SEND_SIZE;
          if oOutBuff.Size<=maxLen then // Can send everything from Buffer
            begin
            data:=oOutBuff.GetEx;
            lData:=length(data);
            RtcFreeAndNil(oOutBuff);
            oOutSize:=0;
            oPrepared:=False;
            end
          else if maxLen>0 then // Can send part of the Buffer
            begin
            data:=oOutBuff.GetStartEx(maxLen);
            lData:=length(data);
            oOutBuff.DelStart(maxLen);
            oOutSize:=oOutBuff.Size;
            end;
          if lData>0 then
            begin
            oSending:=True;
            oCryptOut.CryptEx(data);
            oLeftToSend:=oLeftToSend-Cardinal(length(data));
            {$IFDEF RTCGATE_DEBUG}xLog('Send('+IntToStr(length(data))+') Len='+IntToStr(oLeftToSend)+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
            oLastOutput:=GetAppRunTime;
            if oLeftToSend=0 then // done with this response, prepare for the next
              oCloseOutput(Conn);
            end;
          end;
    tellRecv:=not (oPrepared or oSending or oPaused);
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;

  if tellRecv then
    oiNotifyReceivers;

  if lData>0 then
    begin
    Sender.WriteEx(data);
    oAfterWrite(lData);
    SetLength(data,0);
    end;

  if Sender.Response.StatusCode<>GATE_OK_CODE then
    ixoResetOutput(239,Conn);
  end;

procedure TRtcGateOneUser.iCloseInput(Conn:RtcIntPtr);
  var
    Sender:TRtcConnection absolute Conn;
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput(150);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if iInput=Conn then
      begin
      iInput:=0;
      iInThr:=nil;
      iLeftToRead:=0;
      iPaused:=False;
      {$IFDEF RTCGATE_FULLDEBUG}xLog('CloseInput: iInput:=0; iPaused:=False'+FromAddr(Sender)+'/'+iFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
      iFrom:='';
      end{$IFDEF RTCGATE_DEBUG}
    else if (iInput<>0) and (Conn<>0) then
      xLog('BAD CloseInput: iInput{'+IntToStr(iInput)+'}!={'+IntToStr(Conn)+'}/'+iFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.oCloseOutput(Conn:RtcIntPtr);
  var
    Sender:TRtcConnection absolute Conn;
  begin
  {$IFDEF RTCGATE_DEBUG}LockOutput(250);{$ELSE}outCS.Acquire;{$ENDIF}
  try
    if oOutput=Conn then
      begin
      oOutput:=0;
      oOutThr:=nil;
      oSending:=False;
      oPaused:=False;
      {$IFDEF RTCGATE_FULLDEBUG}xLog('oCloseOutput: oOutput:=0; oSending=False; oPaused:=False'+FromAddr(Sender)+'/'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
      oFrom:='';
      end{$IFDEF RTCGATE_DEBUG}
    else if (oOutput<>0) and (Conn<>0) then
      xLog('BAD CloseOutput: oOutput{'+IntToStr(oOutput)+'}!={'+IntToStr(Conn)+'}/'+oFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ixoResetInput(i:byte; Conn:RtcIntPtr);
  var
    Sender:TRtcConnection absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    amClosing,wasOpen,
    resInput,resOutput:boolean;
  begin
  amClosing:=False;
  wasOpen:=False;
  resInput:=False;
  resOutput:=False;

  {$IFDEF RTCGATE_DEBUG}LockInput(146);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    {$IFDEF RTCGATE_DEBUG}LockOutput(147);{$ELSE}outCS.Acquire;{$ENDIF}
    try
      if iInput=Conn then
        begin
        wasOpen:=oiAllReady;

        iInput:=0;
        iInThr:=nil;
        iInputClear:=False;
        iLeftToRead:=0;
        iPaused:=False;
        if iInputReady then
          begin
          amClosing:=True;
          iInputReady:=False;
          oiAllReady:=False;
          resInput:=True;
          {$IFDEF RTCGATE_DEBUG}xLog('ixoResetInput(I+): AllReady:=FALSE; iInput:=0; iInputClear:=FALSE; iPaused:=False'+FromAddr(Sender),IntToStr(xUID shr UserIDShift));{$ENDIF}
          end{$IFDEF RTCGATE_FULLDEBUG}
        else
          xLog('ixoResetInput(I-): iInput:=0; iInputClear:=FALSE; iPaused:=False'+FromAddr(Sender)+'/'+iFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
        iFrom:='';

        if oOutput=Conn then
          begin
          oOutput:=0;
          oOutThr:=nil;
          oOutputClear:=False;
          oLeftToSend:=0;
          oPaused:=False;
          if oOutputReady then
            begin
            amClosing:=True;
            oOutputReady:=False;
            oiAllReady:=False;
            resOutput:=True;
            {$IFDEF RTCGATE_DEBUG}xLog('ixoResetInput(O+): AllReady:=FALSE; oOutput:=0; oOutputClear:=FALSE; oPaused:=False'+FromAddr(Sender),IntToStr(xUID shr UserIDShift));{$ENDIF}
            end{$IFDEF RTCGATE_FULLDEBUG}
          else
            xLog('ixoResetInput(O-): oOutput:=0; oOutputClear:=FALSE; oPaused:=False'+FromAddr(Sender)+'/'+oFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
          oFrom:='';
          end;
        end;
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;

  CleanUp(False,False,False,Conn,resInput,resOutput);

  if amClosing and wasOpen then
    if assigned(xGate) then
      xGate.EVOff(Sender,xUID shr UserIDShift,xAuth,xInfo);
  end;

procedure TRtcGateOneUser.ixoResetOutput(i:byte; Conn:RtcIntPtr);
  var
    Sender:TRtcConnection absolute Conn;
    amClosing,wasOpen,
    resOutput,resInput:boolean;
  begin
  amClosing:=False;
  wasOpen:=False;
  resInput:=False;
  resOutput:=False;

  {$IFDEF RTCGATE_DEBUG}LockInput(246);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    {$IFDEF RTCGATE_DEBUG}LockOutput(247);{$ELSE}outCS.Acquire;{$ENDIF}
    try
      if oOutput=Conn then
        begin
        wasOpen:=oiAllReady;

        oOutput:=0;
        oOutThr:=nil;
        oOutputClear:=False;
        oLeftToSend:=0;
        oPaused:=False;
        if oOutputReady then
          begin
          amClosing:=True;
          oOutputReady:=False;
          oiAllReady:=False;
          resOutput:=True;
          {$IFDEF RTCGATE_DEBUG}xLog('ixoResetOutput(O+): AllReady:=FALSE; oOutput:=0; oOutputClear:=FALSE; oPaused:=False'+FromAddr(Sender),IntToStr(xUID shr UserIDShift));{$ENDIF}
          end{$IFDEF RTCGATE_FULLDEBUG}
        else
          xLog('ixoResetOutput(O-): oOutput:=0; oOutputClear:=FALSE; oPaused:=False'+FromAddr(Sender)+'/'+oFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
        oFrom:='';

        if iInput=Conn then
          begin
          iInput:=0;
          iInThr:=nil;
          iInputClear:=False;
          iLeftToRead:=0;
          iPaused:=False;
          if iInputReady then
            begin
            amClosing:=True;
            iInputReady:=False;
            oiAllReady:=False;
            {$IFDEF RTCGATE_DEBUG}xLog('ixoResetOutput(I+): AllReady:=FALSE; iInput:=0; iInputClear:=FALSE; iPaused:=False'+FromAddr(Sender),IntToStr(xUID shr UserIDShift));{$ENDIF}
            resInput:=True;
            end{$IFDEF RTCGATE_FULLDEBUG}
          else
            xLog('ixoResetOutput(I-): iInput:=0; iInputClear:=FALSE; iPaused:=False'+FromAddr(Sender)+'/'+iFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
          iFrom:='';
          end;
        end;
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;

  CleanUp(False,False,False,Conn,resInput,resOutput);

  if amClosing and wasOpen then
    if assigned(xGate) then
      xGate.EVOff(Sender,xUID shr UserIDShift,xAuth,xInfo);
  end;

procedure TRtcGateOneUser.ixoResetInOutStreams(i:byte);
  var
    wasOpen,amClosing,
    resInput,resOutput:boolean;
  begin
  amClosing:=False;
  resInput:=False;
  resOutput:=False;

  {$IFDEF RTCGATE_DEBUG}LockInput(254);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    {$IFDEF RTCGATE_DEBUG}LockOutput(254);{$ELSE}outCS.Acquire;{$ENDIF}
    try
      wasOpen:=oiAllReady;
      iInput:=0;
      iInThr:=nil;
      iInputClear:=False;
      iLeftToRead:=0;
      iPaused:=False;
      if iInputReady then
        begin
        amClosing:=True;
        iInputReady:=False;
        oiAllReady:=False;
        resInput:=True;
        {$IFDEF RTCGATE_DEBUG}xLog('ixoResetInOut(I+): AllReady:=FALSE; iInput:=0; iInputClear:=FALSE; iPaused:=False',IntToStr(xUID shr UserIDShift));{$ENDIF}
        end{$IFDEF RTCGATE_FULLDEBUG}
      else
        xLog('ixoResetInOut(I-): iInput:=0; iInputClear:=FALSE; iPaused:=False'+iFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
      iFrom:='';

      oOutput:=0;
      oOutThr:=nil;
      oOutputClear:=False;
      oLeftToSend:=0;
      oPaused:=False;
      if oOutputReady then
        begin
        amClosing:=True;
        oOutputReady:=False;
        oiAllReady:=False;
        resOutput:=True;
        {$IFDEF RTCGATE_DEBUG}xLog('ixoResetInOut(O+): AllReady:=FALSE; oOutput:=0; oOutputClear:=FALSE; oPaused:=False',IntToStr(xUID shr UserIDShift));{$ENDIF}
        end{$IFDEF RTCGATE_FULLDEBUG}
      else
        xLog('ixoResetInOut(O-): oOutput:=0; oOutputClear:=FALSE; oPaused:=False'+oFrom,IntToStr(xUID shr UserIDShift)){$ENDIF};
      oFrom:='';

      oiUsed:=False;
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;

  CleanUp(False,False,False,0,resInput,resOutput);

  if amClosing and wasOpen then
    if assigned(xGate) then
      xGate.EVOff(nil,xUID shr UserIDShift,xAuth,xInfo);
  end;

procedure TRtcGateOneUser.xoInternalAddHost(const Channel:RtcString; GroupID:TGateUID;
                                           NotifyOthers,NotifyUser:boolean);
  var
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
    obj:TObject;
    chan:tBinList absolute obj;
    uid,gid:TGateUID;
  begin
  xGate.FCStr.Acquire;
  try
    // Find Hosts channel
    obj:=xGate.FzHosts.search(Channel);
    if obj=nil then // Hosts channel not found
      begin
      // Create Hosts channel
      chan:=tBinList.Create(16);
      xGate.FzHosts.insert(Channel,chan);
      end;

    // Find User in Hosts channel
    if chan.search(xUID)=0 then // User not found
      begin
      // Add user to Hosts channel
      chan.insert(xUID, GroupID+1);

      // Find Controls channel
      obj:=xGate.FzControls.search(Channel);
      if Assigned(obj) then
        begin
        // Enumarate through all Controls on this Channel
        uid:=chan.search_min(gid);
        while (uid>0) and (gid>0) do
          begin
          if xUID<>uid then
            begin
            xGate.FCStr.Release;
            try
              xGate.FCUsr.Acquire;
              try
                objUsr:=xGate.FxUsers.search(uid);
              finally
                xGate.FCUsr.Release;
                end;
              if assigned(objUsr) then
                begin
                  if NotifyUser then
                    oPostInfo(Cmd_UserReady,(GroupID and GroupIDMask) or uid, oUser.xInfo);
                  if NotifyOthers then
                    oUser.oPostInfo(Cmd_UserReady,((gid-1) and GroupIDMask) or xUID, xInfo);
                end;
            finally
              xGate.FCStr.Acquire;
              end;
            end;
          uid:=chan.search_g(uid,gid);
          end;
        end;
      end;
  finally
    xGate.FCStr.Release;
    end;
  end;

procedure TRtcGateOneUser.xoInternalAddControl(const Channel:RtcString; GroupID:TGateUID;
                                              NotifyOthers,NotifyUser:boolean);
  var
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
    obj:TObject;
    chan:tBinList absolute obj;
    uid,gid:TGateUID;
  begin
  xGate.FCStr.Acquire;
  try
    // Find Controls channel
    obj:=xGate.FzControls.search(Channel);
    if obj=nil then // Controls channel not found
      begin
      // Create Controls channel
      chan:=tBinList.Create(16);
      xGate.FzControls.insert(Channel,chan);
      end;
    // Find User in Controls channel
    if chan.search(xUID)=0 then // User not found
      begin
      // Add user to Controls channel
      chan.insert(xUID, GroupID+1);
      // Find Hosts channel
      obj:=xGate.FzHosts.search(Channel);
      if Assigned(obj) then
        begin
        // Enumarate through all Hosts on this Channel
        uid:=chan.search_min(gid);
        while (uid>0) and (gid>0) do
          begin
          if xUID<>uid then
            begin
            xGate.FCStr.Release;
            try
              xGate.FCUsr.Acquire;
              try
                objUsr:=xGate.FxUsers.search(uid);
              finally
                xGate.FCUsr.Release;
                end;
              if assigned(objUsr) then
                begin
                if NotifyUser then
                  oPostInfo(Cmd_UserReady,(GroupID and GroupIDMask) or uid, oUser.xInfo);
                if NotifyOthers then
                  oUser.oPostInfo(Cmd_UserReady,((gid-1) and GroupIDMask) or xUID, xInfo);
                end;
            finally
              xGate.FCStr.Acquire;
              end;
            end;
          uid:=chan.search_g(uid,gid);
          end;
        end;
      end;
  finally
    xGate.FCStr.Release;
    end;
  end;

procedure TRtcGateOneUser.xoInternalAddPublic(const Channel:RtcString; GroupID:TGateUID;
                                             NotifyOthers,NotifyUser:boolean);
  var
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
    obj:TObject;
    chan:tBinList absolute obj;
    uid,gid:TGateUID;
  begin
  xGate.FCStr.Acquire;
  try
    // Find Public channel
    obj:=xGate.FzPublic.search(Channel);
    if obj=nil then // Public channel not found
      begin
      // Create Public channel
      chan:=tBinList.Create(16);
      xGate.FzPublic.insert(Channel,chan);
      end;

    // Find User in Public channel
    if chan.search(xUID)=0 then // User not found
      begin
      // Add user to Public channel
      chan.insert(xUID, GroupID+1);

      // Enumarate through all Users on this Channel
      uid:=chan.search_min(gid);
      while (uid>0) and (gid>0) do
        begin
        if xUID<>uid then
          begin
          xGate.FCStr.Release;
          try
            xGate.FCUsr.Acquire;
            try
              objUsr:=xGate.FxUsers.search(uid);
            finally
              xGate.FCUsr.Release;
              end;
            if assigned(objUsr) then
              begin
              if NotifyUser then
                oPostInfo(Cmd_UserReady,(GroupID and GroupIDMask) or uid, oUser.xInfo);
              if NotifyOthers then
                oUser.oPostInfo(Cmd_UserReady,((gid-1) and GroupIDMask) or xUID, xInfo);
              end;
          finally
            xGate.FCStr.Acquire;
            end;
          end;
        uid:=chan.search_g(uid,gid);
        end;
      end;
  finally
    xGate.FCStr.Release;
    end;
  end;

procedure TRtcGateOneUser.xoInternalRemoveHost(const Channel:RtcString;
                                              NotifyOthers,NotifyUser:boolean);
  var
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
    obj:TObject;
    chan:tBinList absolute obj;
    GroupID,uid,gid:TGateUID;
  begin
  xGate.FCStr.Acquire;
  try
    // Find Hosts channel
    obj:=xGate.FzHosts.search(Channel);
    if obj=nil then  // Hosts channel not found
      begin
      if LOG_GATEWAY_STATUS then
        Log('oUser.xoInternalRemoveHost (from "'+Channel+'") - NOT FOUND',GATE_LOG+'.HINT');
      Exit;
      end;

    // Find User in Hosts channel
    GroupID:=chan.search(xUID);
    if GroupID>0 then // User found
      begin
      Dec(GroupID);
      // Remove user from Hosts channel
      chan.remove(xUID);
      if chan.Count=0 then // Last Host in this Channel?
        begin
        // Close the Hosts Channel
        if xGate.FzHosts.search(Channel)=chan then
          begin
          xGate.FzHosts.remove(Channel);
          RtcFreeAndNil(chan);
          end;
        end;

      // Find Controls channel
      obj:=xGate.FzControls.search(Channel);
      if Assigned(obj) then
        begin
        // Enumarate through all Controls on this Channel
        uid:=chan.search_min(gid);
        while (uid>0) and (gid>0) do
          begin
          if xUID<>uid then
            begin
            xGate.FCStr.Release;
            try
              xGate.FCUsr.Acquire;
              try
                objUsr:=xGate.FxUsers.search(uid);
              finally
                xGate.FCUsr.Release;
                end;
              if assigned(objUsr) then
                begin
                if NotifyUser then
                  oPostNote(Cmd_UserNotReady,(GroupID and GroupIDMask) or uid,False);
                if NotifyOthers then
                  oUser.oPostNote(Cmd_UserNotReady,((gid-1) and GroupIDMask) or xUID,False);
              end;
            finally
              xGate.FCStr.Acquire;
              end;
            end;
          uid:=chan.search_g(uid,gid);
          end;
        end;
      end;
  finally
    xGate.FCStr.Release;
    end;
  end;

procedure TRtcGateOneUser.xoInternalRemoveControl(const Channel:RtcString;
                                                 NotifyOthers,NotifyUser:boolean);
  var
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
    obj:TObject;
    chan:tBinList absolute obj;
    GroupID,uid,gid:TGateUID;
  begin
  xGate.FCStr.Acquire;
  try
    // Find Controls channel
    obj:=xGate.FzControls.search(Channel);
    if obj=nil then // Controls channel not found
      begin
      if LOG_GATEWAY_STATUS then
        Log('oUser.xoInternalRemoveControl (from "'+Channel+'") - NOT FOUND',GATE_LOG+'.HINT');
      Exit;
      end;

    // Find User in Controls channel
    GroupID:=chan.search(xUID);
    if GroupID>0 then // User found
      begin
      Dec(GroupID);
      // Remove user from Controls channel
      chan.remove(xUID);
      if chan.Count=0 then // Last Control in this Channel?
        begin
        // Close the Controls Channel
        if xGate.FzControls.search(Channel)=chan then
          begin
          xGate.FzControls.remove(Channel);
          RtcFreeAndNil(chan);
          end;
        end;

      // Find Hosts channel
      obj:=xGate.FzHosts.search(Channel);
      if Assigned(obj) then
        begin
        // Enumarate through all Hosts on this Channel
        uid:=chan.search_min(gid);
        while (uid>0) and (gid>0) do
          begin
          if xUID<>uid then
            begin
            xGate.FCStr.Release;
            try
              xGate.FCUsr.Acquire;
              try
                objUsr:=xGate.FxUsers.search(uid);
              finally
                xGate.FCUsr.Release;
                end;
              if assigned(objUsr) then
                begin
                if NotifyUser then
                  oPostNote(Cmd_UserNotReady,(GroupID and GroupIDMask) or uid,False);
                if NotifyOthers then
                  oUser.oPostNote(Cmd_UserNotReady,((gid-1) and GroupIDMask) or xUID,False);
                end;
            finally
              xGate.FCStr.Acquire;
              end;
            end;
          uid:=chan.search_g(uid,gid);
          end;
        end;
      end;
  finally
    xGate.FCStr.Release;
    end;
  end;

procedure TRtcGateOneUser.xoInternalRemovePublic(const Channel:RtcString;
                                                NotifyOthers,NotifyUser:boolean);
  var
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
    obj:TObject;
    chan:tBinList absolute obj;
    GroupID,uid,gid:TGateUID;
  begin
  xGate.FCStr.Acquire;
  try
    // Find Public channel
    obj:=xGate.FzPublic.search(Channel);
    if obj=nil then // Public channel not found
      begin
      if LOG_GATEWAY_STATUS then
        Log('oUser.xoInternalRemovePublic (from "'+Channel+'") - NOT FOUND',GATE_LOG+'.HINT');
      Exit;
      end;

    // Find User in Public channel
    GroupID:=chan.search(xUID);
    if GroupID>0 then // User found
      begin
      Dec(GroupID);
      // Remove user from Public channel
      chan.remove(xUID);
      if chan.Count=0 then // Last User in this Channel?
        begin
        // Close the Public Channel
        if xGate.FzPublic.search(Channel)=chan then
          begin
          xGate.FzPublic.remove(Channel);
          RtcFreeAndNil(chan);
          end;
        end
      else
        begin
        // Enumarate through all Users on this Channel
        uid:=chan.search_min(gid);
        while (uid>0) and (gid>0) do
          begin
          if xUID<>uid then
            begin
            xGate.FCStr.Release;
            try
              xGate.FCUsr.Acquire;
              try
                objUsr:=xGate.FxUsers.search(uid);
              finally
                xGate.FCUsr.Release;
                end;
              if assigned(objUsr) then
                begin
                if NotifyUser then
                  oPostNote(Cmd_UserNotReady,(GroupID and GroupIDMask) or uid,False);
                if NotifyOthers then
                  oUser.oPostNote(Cmd_UserNotReady,((gid-1) and GroupIDMask) or xUID,False);
                end;
            finally
              xGate.FCStr.Acquire;
              end;
            end;
          uid:=chan.search_g(uid,gid);
          end;
        end;
      end;
  finally
    xGate.FCStr.Release;
    end;
  end;

procedure TRtcGateOneUser.ixoSubscribeToChannel(const Channel:RtcString;
                                              GroupID:TGateUID;
                                              NotifyOthers,NotifyUser:boolean);
  var
    chan:RtcString;
    done:boolean;
  begin
  if rtcKeyPublic(Channel) then
    begin
    chan:=UpperCase(Channel);
    {$IFDEF RTCGATE_DEBUG}LockInput( 20);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      if not assigned(iPublicChannels) then
        iPublicChannels:=tStrIntList.Create(16);
      done:=iPublicChannels.search(chan)<=0;
      if done then // not in channel
        iPublicChannels.insert(chan,1);
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      end;
    if done then
      xoInternalAddPublic(chan,GroupID,NotifyOthers,NotifyUser);
    end
  else if rtcLockValid(Channel) then
    begin
    chan:=UpperCase(Channel);
    {$IFDEF RTCGATE_DEBUG}LockInput( 21);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      if not assigned(iHostChannels) then
        iHostChannels:=tStrIntList.Create(16);
      done:=iHostChannels.search(chan)<=0;
      if done then // not in channel
        iHostChannels.insert(chan,1);
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      end;
    if done then
      xoInternalAddHost(chan,GroupID,NotifyOthers,NotifyUser);
    end
  else if rtcKeyValid(Channel) then
    begin
    chan:=UpperCase(rtcCalculateLock(Channel));
    {$IFDEF RTCGATE_DEBUG}LockInput( 22);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      if not assigned(iControlChannels) then
        iControlChannels:=tStrIntList.Create(16);
      done:=iControlChannels.search(chan)<=0;
      if done then // not in channel
        iControlChannels.insert(chan,1);
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      end;
    if done then
      xoInternalAddControl(chan,GroupID,NotifyOthers,NotifyUser);
    end;
  end;

procedure TRtcGateOneUser.ixoUnSubscribeFromChannel(const Channel:RtcString;
                                                  NotifyOthers,NotifyUser:boolean);
  var
    chan:RtcString;
    done:boolean;
  begin
  if rtcKeyPublic(Channel) then
    begin
    done:=False;
    chan:=UpperCase(Channel);
    {$IFDEF RTCGATE_DEBUG}LockInput( 23);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      if assigned(iPublicChannels) then
        begin
        done:=iPublicChannels.search(chan)>0;
        if done then // in channel
          iPublicChannels.remove(chan);
        end;
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      end;
    if done then
      xoInternalRemovePublic(chan,NotifyOthers,NotifyUser);
    end
  else if rtcLockValid(Channel) then
    begin
    done:=False;
    chan:=UpperCase(Channel);
    {$IFDEF RTCGATE_DEBUG}LockInput( 24);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      if assigned(iHostChannels) then
        begin
        done:=iHostChannels.search(chan)>0;
        if done then // in channel
          iHostChannels.remove(chan);
        end;
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      end;
    if done then
      xoInternalRemoveHost(chan,NotifyOthers,NotifyUser);
    end
  else if rtcKeyValid(Channel) then
    begin
    done:=False;
    chan:=UpperCase(rtcCalculateLock(Channel));
    {$IFDEF RTCGATE_DEBUG}LockInput( 25);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      if assigned(iControlChannels) then
        begin
        done:=iControlChannels.search(chan)>0;
        if done then // in channel
          iControlChannels.remove(chan);
        end;
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      end;
    if done then
      xoInternalRemoveControl(chan,NotifyOthers,NotifyUser);
    end;
  end;

procedure TRtcGateOneUser.ixoUnSubscribeFromAll(NotifyOthers,NotifyUser:boolean);
  var
    chan:RtcString;
    gid:longint;
  begin
  {$IFDEF RTCGATE_DEBUG}LockInput( 26);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if assigned(iPublicChannels) then
      begin
      chan:=iPublicChannels.search_min(gid);
      while (gid>0) and (chan<>'') do
        begin
        iPublicChannels.remove(chan);
        {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
        try
          xoInternalRemovePublic(chan,NotifyOthers,NotifyUser);
        finally
          {$IFDEF RTCGATE_DEBUG}LockInput( 27);{$ELSE}inCS.Acquire;{$ENDIF}
          end;
        if not assigned(iPublicChannels) then Break;
        chan:=iPublicChannels.search_min(gid);
        end;
      RtcFreeAndNil(iPublicChannels);
      end;

    if assigned(iHostChannels) then
      begin
      chan:=iHostChannels.search_min(gid);
      while (gid>0) and (chan<>'') do
        begin
        iHostChannels.remove(chan);
        {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
        try
          xoInternalRemoveHost(chan,NotifyOthers,NotifyUser);
        finally
          {$IFDEF RTCGATE_DEBUG}LockInput( 28);{$ELSE}inCS.Acquire;{$ENDIF}
          end;
        if not assigned(iHostChannels) then Break;
        chan:=iHostChannels.search_min(gid);
        end;
      RtcFreeAndNil(iHostChannels);
      end;

    if assigned(iControlChannels) then
      begin
      chan:=iControlChannels.search_min(gid);
      while (gid>0) and (chan<>'') do
        begin
        iControlChannels.remove(chan);
        {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
        try
          xoInternalRemoveControl(chan,NotifyOthers,NotifyUser);
        finally
          {$IFDEF RTCGATE_DEBUG}LockInput( 29);{$ELSE}inCS.Acquire;{$ENDIF}
          end;
        if not assigned(iControlChannels) then Break;
        chan:=iControlChannels.search_min(gid);
        end;
      RtcFreeAndNil(iControlChannels);
      end;
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;
  end;

procedure TRtcGateOneUser.ixoInternalAddUserToGroup(OwnerID, GroupID, UserID: TGateUID;
                                     NotifyUser, NotifyOwner, Remote: boolean);
  var
    objGr,objUsr,objOwn:TObject;
    oOwner:TRtcGateOneUser absolute objOwn;
    oUser:TRtcGateOneUser absolute objUsr;
    oGroup:TRtcGateUserGroup absolute objGr;
    notInside,canAdd:boolean;
  begin
  if (OwnerID and UserIDMask)<>OwnerID then
    raise ERtcGateway.Create('InternalAddUserToGroup - Bad OwnerID')
  else if (OwnerID=GroupID) then
    raise ERtcGateway.Create('InternalAddUserToGroup - GroupID=OwnerID')
  else if (OwnerID xor GroupID)>MaxGID then
    raise ERtcGateway.Create('InternalAddUserToGroup - GroupID out of Range')
  else if (UserID and UserIDMask)<>UserID then
    raise ERtcGateway.Create('InternalAddUserToGroup - Bad UserID');

  if OwnerID=xUID then
    objOwn:=self
  else
    objOwn:=xGate.FindGateUser(OwnerID);
  if assigned(objOwn) then
    begin
    if Remote and GATECHECK_FRIEND4GROUPS then
      begin
      oOwner.{$IFDEF RTCGATE_DEBUG}LockOutput( 31);{$ELSE}outCS.Acquire;{$ENDIF}
      try
        objUsr:=findUser(oOwner.oInFriends,UserID);
      finally
        oOwner.{$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
        end;
      end
    else if UserID=xUID then
      objUsr:=self
    else
      objUsr:=xGate.FindGateUser(UserID);
    if assigned(objUsr) then
      begin
      oUser.{$IFDEF RTCGATE_DEBUG}LockOutput( 32);{$ELSE}outCS.Acquire;{$ENDIF}
      try
        canAdd:=oUser.oiAllReady;
        if canAdd then
          notInside:=findUser(oUser.oInGroups,GroupID)=nil
        else
          notInside:=True;
      finally
        oUser.{$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
        end;
      if notInside and canAdd then
        begin
        oOwner.{$IFDEF RTCGATE_DEBUG}LockInput( 33);{$ELSE}inCS.Acquire;{$ENDIF}
        try
          if not oiAllReady then
            objGr:=nil
          else
            begin
            objGr:=findUser(oOwner.iOwnGroups,GroupID);
            if not assigned(objGr) then
              begin
              objGr:=TRtcGateUserGroup.Create(oOwner,GroupID);
              if assigned(objGr) then
                insertUser(oOwner.iOwnGroups,GroupID,oGroup);
              end;
            if assigned(objGr) then
              begin
              oGroup.LockGroup(5);
              try
                if oGroup.grUsers.search(UserID)=nil then
                  begin
                  oUser.{$IFDEF RTCGATE_DEBUG}LockOutput( 34);{$ELSE}outCS.Acquire;{$ENDIF}
                  try
                    if oUser.oiAllReady then
                      notInside:=insertUser(oUser.oInGroups,GroupID,objGr)
                    else
                      notInside:=False;
                  finally
                    oUser.{$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
                    end;
                  if notInside then
                    insertUser(oGroup.grUsers,UserID,objUsr);
                  end;
              finally
                oGroup.UnLockGroup;
                end;
              if notInside then
                begin
                if (NotifyUser and (UserID<>OwnerID)) or
                   (NotifyOwner and (UserID=OwnerID)) then
                  oUser.oPostNote(Cmd_UserIn,GroupID);
                if NotifyOwner then
                  oOwner.oPostNote(Cmd_UserOn,(GroupID and GroupIDMask) or UserID);
                end;
              end;
            end;
        finally
          oOwner.{$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
          end;
        end
      else if notInside and NotifyOwner then
        oOwner.oPostNote(Cmd_UserOff,UserID,False);
      end
    else if NotifyOwner then
      oOwner.oPostNote(Cmd_UserOff,UserID,False);
    end;
  end;

procedure TRtcGateOneUser.xooInternalRemoveUserFromGroup(GroupID, UserID: TGateUID;
                                          NotifyUser, NotifyOwner: boolean);
  var
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
  begin
  if (GroupID and UserIDMask)=GroupID then
    raise ERtcGateway.Create('InternalRemoveUserFromGroup - GroupID=OwnerID')
  else if (UserID and UserIDMask)<>UserID then
    raise ERtcGateway.Create('InternalRemoveUserFromGroup - Bad UserID');

  if UserID=xUID then
    objUsr:=self
  else
    objUsr:=xGate.FindGateUser(UserID);
  if assigned(objUsr) then
    oUser.ooRemoveUserFromGroup(GroupID,NotifyUser,NotifyOwner);
  end;

procedure TRtcGateOneUser.ixoInternalRemoveGroup(OwnerID, GroupID: TGateUID;
                                  NotifyUsers, NotifyOwner:boolean);
  var
    objOwn:TObject;
    oOwner:TRtcGateOneUser absolute objOwn;
  begin
  if (OwnerID and UserIDMask)<>OwnerID then
    raise ERtcGateway.Create('InternalRemoveGroup - Bad OwnerID')
  else if (OwnerID=GroupID) then
    raise ERtcGateway.Create('InternalRemoveGroup - GroupID=OwnerID')
  else if (OwnerID xor GroupID)>MaxGID then
    raise ERtcGateway.Create('InternalRemoveGroup - GroupID out of Range');

  if OwnerID=xUID then
    objOwn:=self
  else
    objOwn:=xGate.FindGateUser(OwnerID);
  if assigned(objOwn) then
    oOwner.ioRemoveGroup(GroupID,NotifyUsers,NotifyOwner);
  end;

procedure TRtcGateOneUser.ixoInternalAddUserAsFriend(UserID, FriendID: TGateUID; NotifyFriend, NotifyUser: boolean);
  var
    objUsr,objOwn:TObject;
    oOwner:TRtcGateOneUser absolute objOwn;
    oUser:TRtcGateOneUser absolute objUsr;
    notInside,canAdd:boolean;
  begin
  if (UserID and UserIDMask)<>UserID then
    raise ERtcGateway.Create('InternalAddUserAsFriend - Bad UserID')
  else if (FriendID and UserIDMask)<>FriendID then
    raise ERtcGateway.Create('InternalAddUserAsFriend - Bad FriendID');

  if UserID=xUID then
    objOwn:=self
  else
    objOwn:=xGate.FindGateUser(UserID);
  if assigned(objOwn) then
    begin
    if FriendID=xUID then
      objUsr:=self
    else
      objUsr:=xGate.FindGateUser(FriendID);
    if assigned(objUsr) then
      begin
      oUser.{$IFDEF RTCGATE_DEBUG}LockOutput( 35);{$ELSE}outCS.Acquire;{$ENDIF}
      try
        canAdd:=oUser.oiAllReady;
        if canAdd then
          notInside:=findUser(oUser.oInFriends,UserID)=nil
        else
          notInside:=True;
      finally
        oUser.{$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
        end;
      if canAdd and notInside then
        begin
        oOwner.{$IFDEF RTCGATE_DEBUG}LockInput( 36);{$ELSE}inCS.Acquire;{$ENDIF}
        try
          if findUser(oOwner.iOwnFriends,FriendID)=nil then
            begin
            oUser.{$IFDEF RTCGATE_DEBUG}LockOutput( 37);{$ELSE}outCS.Acquire;{$ENDIF}
            try
              if oUser.oiAllReady then
                notInside:=insertUser(oUser.oInFriends,UserID,objOwn)
              else
                notInside:=False;
            finally
              oUser.{$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
              end;
            if notInside then
              insertUser(oOwner.iOwnFriends,FriendID,objUsr);
            end
          else
            notInside:=False;
        finally
          oOwner.{$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
          end;
        if notInside then
          begin
          if NotifyUser then
            oOwner.oPostNote(Cmd_FriendAdd,FriendID);
          if NotifyFriend then
            oUser.oPostNote(Cmd_UserBeFriend,UserID);
          end;
        end
      else if notInside and NotifyUser then
        oOwner.oPostNote(Cmd_UserOff,FriendID,False);
      end
    else if NotifyUser then
      oOwner.oPostNote(Cmd_UserOff,FriendID,False);
    end;
  end;

procedure TRtcGateOneUser.ixoInternalRemoveUserAsFriend(UserID, FriendID: TGateUID; NotifyFriend, NotifyUser: boolean);
  var
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
  begin
  if (UserID and UserIDMask)<>UserID then
    raise ERtcGateway.Create('InternalRemoveUserAsFriend - Bad UserID')
  else if (FriendID and UserIDMask)<>FriendID then
    raise ERtcGateway.Create('InternalRemoveUserAsFriend - Bad FriendID');

  if UserID=xUID then
    objUsr:=self
  else
    objUsr:=xGate.FindGateUser(UserID);
  if assigned(objUsr) then
    oUser.ioRemoveUserAsFriend(FriendID,NotifyFriend,NotifyUser);
  end;

procedure TRtcGateOneUser.ixoInternalRemoveFriends(UserID: TGateUID; NotifyFriends, NotifyUser: boolean);
  var
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
  begin
  if (UserID and UserIDMask)<>UserID then
    raise ERtcGateway.Create('InternalRemoveFriends - Bad UserID');

  if UserID=xUID then
    objUsr:=self
  else
    objUsr:=xGate.FindGateUser(UserID);
  if assigned(objUsr) then
    oUser.ioRemoveAllFriends(NotifyFriends,NotifyUser);
  end;

procedure TRtcGateOneUser.ixoInternalSubscribeToChannel(UserID, GroupID: TGateUID; const data:RtcByteArray; len,loc:Cardinal; NotifyOthers, NotifyUser: boolean);
  var
    Channel:RtcString;
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
  begin
  if (UserID and UserIDMask)<>UserID then
    raise ERtcGateway.Create('InternalSubscribeToChannel - Bad UserID');

  if UserID=xUID then
    objUsr:=self
  else
    objUsr:=xGate.FindGateUser(UserID);
  if assigned(objUsr) then
    begin
    Channel:=RtcBytesToString(Copy(data,loc,len));
    oUser.ixoSubscribeToChannel(Channel,GroupID,NotifyOthers,NotifyUser);
    Channel:='';
    end;
  end;

procedure TRtcGateOneUser.ixoInternalUnSubscribeFromChannel(UserID: TGateUID; const data:RtcByteArray; len,loc:Cardinal; NotifyOthers, NotifyUser: boolean);
  var
    Channel:RtcString;
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
  begin
  if (UserID and UserIDMask)<>UserID then
    raise ERtcGateway.Create('InternalUnSubscribeFromChannel - Bad UserID');

  if UserID=xUID then
    objUsr:=self
  else
    objUsr:=xGate.FindGateUser(UserID);
  if assigned(objUsr) then
    begin
    Channel:=RtcBytesToString(Copy(data,loc,len));
    oUser.ixoUnSubscribeFromChannel(Channel,NotifyOthers,NotifyUser);
    Channel:='';
    end;
  end;

procedure TRtcGateOneUser.ixoInternalUnSubscribeFromAll(UserID: TGateUID; NotifyOthers, NotifyUser: boolean);
  var
    objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
  begin
  if (UserID and UserIDMask)<>UserID then
    raise ERtcGateway.Create('InternalUnSubscribeFromAll - Bad UserID');

  if UserID=xUID then
    objUsr:=self
  else
    objUsr:=xGate.FindGateUser(UserID);
  if assigned(objUsr) then
    oUser.ixoUnSubscribeFromAll(NotifyOthers,NotifyUser);
  end;

procedure TRtcGateOneUser.ixoProcessData(Conn:RtcIntPtr; var data:RtcByteArray);
  var
    Sender:TRtcConnection absolute Conn;
    ldata,loc,len,xLeft,xLength:Cardinal;
    xUserID,xToUserID:TGateUID;
    xExt:byte;
    xCommand:byte;
    OK:boolean;

    objTo:TObject;
    oUserTo:TRtcGateUser absolute objTo;

  function FindGroupUser(uid:TGateUID):TObject;
    begin
    {$IFDEF RTCGATE_DEBUG}LockInput(132);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      if iInput<>Conn then
        RaiseError('IN CONN Changed'+FromAddr(Sender)+'/'+iFrom) // input connection changed
      else if not oiAllReady then
        RaiseError('CONN Lost'+FromAddr(Sender)+'/'+iFrom); // connection lost
      Result:=findUser(iOwnGroups,uid);
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      end;
    end;

  function FindFriendUser(uid:TGateUID):TObject;
    begin
    {$IFDEF RTCGATE_DEBUG}LockInput(133);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      if iInput<>Conn then
        RaiseError('IN CONN Changed'+FromAddr(Sender)+'/'+iFrom) // input connection changed
      else if not oiAllReady then
        RaiseError('CONN Lost'+FromAddr(Sender)+'/'+iFrom); // connection lost
      Result:=findUser(iOwnFriends,uid);
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      end;
    if Result=nil then
      Result:=xGate.FindGateUser(uid);
    end;

  begin
  {$IFDEF RTCGATE_DEBUG}LockInput(131);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if iInput<>Conn then
      RaiseError('IN CONN Changed'+FromAddr(Sender)+'/'+iFrom) // input connection changed
    else if not oiAllReady then
      RaiseError('CONN Lost'+FromAddr(Sender)+'/'+iFrom); // connection lost

    // Decrypt data
    iCryptIn.DeCryptEx(data);

    if iSending then // Already sending a package
      begin
      if assigned(iInBuff) then
        if iInBuff.Size>0 then
          begin
          iInBuff.AddEx(data);
          data:=iInBuff.GetEx;
          RtcFreeAndNil(iInBuff);
          end;
      lData:=length(Data);
      iInSize:=lData;
      if lData>=iLeftToSend then // Received the rest of the package
        begin
        if iPacketValid then
          begin
          if iReceiverIsGroup then // Receiver = my Group
            objTo:=findUser(iOwnGroups,iReceiver)
          else if iReceiver=xUID then // Receiver = my UID
            objTo:=self
          else
            objTo:=FindFriendUser(iReceiver);
          if not assigned(objTo) then
            iPacketValid:=False
          else if oUserTo.ozSendMore(self, xUID, iSender, iLeftToSend, data, iLeftToSend, 0, iExt)<=0 then
            iPacketValid:=False;
          end;
        if iLeftToSend=lData then
          SetLength(data,0)
        else
          data:=Copy(data, iLeftToSend, lData-iLeftToSend); // data to be processed next
        iLeftToSend:=0;
        iReceiver:=0;
        iSender:=0;
        iSending:=False;
        iPacketValid:=False;
        iInSize:=length(data);
        end
      else if lData>=Cardinal(SOCK_MAX_SEND_SIZE-10) then // have at least 1 full packet
        begin
        if iPacketValid then
          begin
          if iReceiverIsGroup then // Receiver = my Group
            objTo:=findUser(iOwnGroups,iReceiver)
          else if iReceiver=xUID then // Receives = my UID
            objTo:=self
          else
            objTo:=FindFriendUser(iReceiver);
          if not assigned(objTo) then
            iPacketValid:=False
          else if oUserTo.ozSendMore(self, xUID, iSender, iLeftToSend, data, lData, 0, iExt)<=0 then
            iPacketValid:=False;
          end;
        iLeftToSend:=iLeftToSend-lData;
        SetLength(data,0);
        iInSize:=0;
        end
      else if length(data)>0 then
        begin
        prepareBuffer(iInBuff);
        iInBuff.AddEx(data);
        iInSize:=iInBuff.Size;
        SetLength(data,0);
        end;
      end
    else if assigned(iInBuff) then
      begin
      if iInBuff.Size>0 then
        begin
        iInBuff.AddEx(data);
        data:=iInBuff.GetEx;
        end;
      iInSize:=length(data);
      RtcFreeAndNil(iInBuff);
      end
    else
      iInSize:=length(data);
  finally
    {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;

  len:=length(data);
  if len=0 then Exit;

  loc:=0;

  while (len >= loc+6) do // Have Command with UserID or GroupID
    begin
    xUserID:=crcBytes2GateCmd(xCommand,data,loc); // 6 bytes
    case xCommand of
      Cmd_SendData:
        begin
        if (len >= loc+10) then // Have Length
          begin
          xLength:=crcBytes2Int(data,loc+6); // 4 bytes
          xLeft:=len-loc-10;
          if (xLeft>=xLength) or // Have the complete package, or ...
             (xLeft>=Cardinal(SOCK_MAX_SEND_SIZE-10)) then // Have at least 1 full packet
            begin
            xExt:=0;
            if (xUserID and UserIDMask)<>xUID then // Sending to another User, with or without "ToGroupID"
              begin
              xToUserID:=xUserID and UserIDMask; // receiver ID
              if xUserID<>xToUserID then // have "ToGroupID"
                begin
                xExt:=1;
                xUserID:=xUID or (xUserID and GroupIDMask); // my ID + receiver Group ID
                end
              else
                xUserID:=xUID; // my ID
              if xToUserID=xUID then
                objTo:=self
              else
                objTo:=FindFriendUser(xToUserID);
              end
            else if (xUserID and UserIDMask)=xUserID then // sending to self
              begin
              xToUserID:=xUID;
              objTo:=self;
              end
            else // sending to my Group
              begin
              xToUserID:=xUserID;
              objTo:=nil;
              end;

            {$IFDEF RTCGATE_DEBUG}LockInput(134);{$ELSE}inCS.Acquire;{$ENDIF}
            try
              if iInput<>Conn then
                RaiseError('IN CONN Changed'+FromAddr(Sender)+'/'+iFrom) // input connection changed
              else if not oiAllReady then
                RaiseError('CONN Lost'+FromAddr(Sender)+'/'+iFrom); // connection lost

              if (objTo=nil) and (xUserID=xToUserID) then
                objTo:=findUser(iOwnGroups,xToUserID);
              if assigned(objTo) then
                begin
                if xLeft>=xLength then // We have the complete package
                  oUserTo.ozSendFirst(self, xUID, xUserID, xLength, data, xLength, loc+10, xExt)
                else if oUserTo.ozSendFirst(self, xUID, xUserID, xLength, data, xLeft, loc+10, xExt)>0 then
                  iPacketValid:=True
                else
                  iPacketValid:=False;
                end
              else
                iPacketValid:=False;
              Inc(loc,10);
              if xLeft<xLength then // Did not get the whole package yet
                begin
                iExt:=xExt;
                iSending:=True;
                iReceiver:=xToUserID;
                iReceiverIsGroup:=(xToUserID and UserIDMask)<>xToUserID;
                iSender:=xUserID;
                iLeftToSend:=xLength-xLeft;
                Inc(loc,xLeft);
                end
              else
                Inc(loc,xLength);
            finally
              {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
              end;
            end
          else
            Break;
          end
        else
          Break;
        end;
      Cmd_UserOn:
        begin
        if xUserID<>xUID then // my UserID = PING
          begin
          OK:=False;
          if (xUserID and UserIDMask)=xUserID then // Send to User
            begin
            objTo:=FindFriendUser(xUserID);
            if assigned(objTo) then
              ok:=oUserTo.oPostNote(Cmd_UserOn,xUID)>0;
            end
          else if (xUserID and UserIDMask)=xUID then // Send to my Group
            begin
            objTo:=FindGroupUser(xUserID);
            if assigned(objTo) then
              ok:=oUserTo.oPostNote(Cmd_UserOn,xUserID)>0;
            end
          else
            ok:=True;
          if not OK then
            oPostNote(Cmd_UserOff,(xUserID and UserIDMask),False);
          end;
        Inc(loc,6);
        end;
      Cmd_UserOff:
        begin
        OK:=False;
        if (xUserID and UserIDMask)=xUserID then // Send to User
          begin
          if xUserID=xUID then
            objTo:=self
          else
            objTo:=FindFriendUser(xUserID);
          if assigned(objTo) then
            ok:=oUserTo.oPostNote(Cmd_UserOff,xUID,False)>0;
          end
        else if (xUserID and UserIDMask)=xUID then // Send to my Group
          begin
          objTo:=FindGroupUser(xUserID);
          if assigned(objTo) then
            ok:=oUserTo.oPostNote(Cmd_UserOff,xUserID,False)>0;
          end
        else
          ok:=True;
        if not OK then
          oPostNote(Cmd_UserOff,(xUserID and UserIDMask),False);
        Inc(loc,6);
        end;
      Cmd_AddUserToGroup:
        begin
        if (len >= loc+10) then
          begin
          xToUserID:=crcBytes2GateID(data,loc+6); // 4 bytes
          if (xToUserID>=MinUserID) and (xToUserID<=MaxUserID) then
            begin
            xToUserID:=xToUserID shl UserIDShift;
            ixoInternalAddUserToGroup(xUID, xUserID, xToUserID, True, True, True);
            end
          else
            case xToUserID of
              errNoUID: raise ERtcOutOfBounds.Create('Cmd_AddUserToGroup - No UID');
              errBadUID: raise ERtcOutOfBounds.Create('Cmd_AddUserToGroup - Bad UID');
              errBadCRC: raise ERtcOutOfBounds.Create('Cmd_AddUserToGroup - Bad CRC');
              else raise ERtcOutOfBounds.Create('Cmd_AddUserToGroup - Bad Data');
              end;
          Inc(loc,10);
          end
        else
          Break;
        end;
      Cmd_RemoveUserFromGroup:
        begin
        if (len >= loc+10) then
          begin
          xToUserID:=crcBytes2GateID(data,loc+6); // 4 bytes
          if (xToUserID>=MinUserID) and (xToUserID<=MaxUserID) then
            begin
            xToUserID:=xToUserID shl UserIDShift;
            if (xToUserID=xUID) then // removing self?
              xooInternalRemoveUserFromGroup(xUserID, xToUserID, True, True)
            else if (xUserID and UserIDMask)=xUID then // removing from my Group?
              xooInternalRemoveUserFromGroup(xUserID, xToUserID, True, True);
            end
          else
            case xToUserID of
              errNoUID: raise ERtcOutOfBounds.Create('Cmd_RemoveUserFromGroup - No UID');
              errBadUID: raise ERtcOutOfBounds.Create('Cmd_RemoveUserFromGroup - Bad UID');
              errBadCRC: raise ERtcOutOfBounds.Create('Cmd_RemoveUserFromGroup - Bad CRC');
              else raise ERtcOutOfBounds.Create('Cmd_RemoveUserFromGroup - Bad Data');
              end;
          Inc(loc,10);
          end
        else
          Break;
        end;
      Cmd_RemoveGroup:
        begin
        if (xUserID and UserIDMask)=xUID then // removing one of MY Groups?
          ixoInternalRemoveGroup(xUID, xUserID, True, True)
        else
          raise ERtcOutOfBounds.Create('Cmd_RemoveGroup - Bad UID');
        Inc(loc,6);
        end;
      Cmd_FriendAdd:
        begin
        ixoInternalAddUserAsFriend(xUID, xUserID, True, True);
        Inc(loc,6);
        end;
      Cmd_FriendRemove:
        begin
        ixoInternalRemoveUserAsFriend(xUID, xUserID, True, True);
        Inc(loc,6);
        end;
      Cmd_RemoveFriends:
        begin
        if (xUserID and UserIDMask)=xUID then // removing my friends?
          ixoInternalRemoveFriends(xUID, True, True)
        else
          raise ERtcOutOfBounds.Create('Cmd_RemoveFriends - Bad UID');
        Inc(loc,6);
        end;
      Cmd_Subscribe:
        begin
        if (len >= loc+10) then // Have Length
          begin
          xLength:=crcBytes2Int(data,loc+6); // 4 bytes
          if xLength>MAX_RTCKEYLOCK_LENGTH then
            RaiseError('Subscribe: Invalid Channel value');
          xLeft:=len-loc-10;
          if (xLeft>=xLength) then // Have the complete package ...
            begin
            if (xUserID and UserIDMask)=xUID then // adding own subscription
              ixoInternalSubscribeToChannel(xUID, xUserID, data, xLength, loc+10, True, True);
            Inc(loc,10);
            Inc(loc,xLength);
            end
          else
            Break;
          end
        else
          Break;
        end;
      Cmd_UnSubscribe:
        begin
        if (len >= loc+10) then // Have Length
          begin
          xLength:=crcBytes2Int(data,loc+6); // 4 bytes
          if xLength>MAX_RTCKEYLOCK_LENGTH then
            RaiseError('UnSubscribe: Invalid Channel value');
          xLeft:=len-loc-10;
          if (xLeft>=xLength) then // Have the complete package ...
            begin
            if (xUserID and UserIDMask)=xUID then // removing own subscription
              ixoInternalUnSubscribeFromChannel(xUID, data, xLength, loc+10, True, True);
            Inc(loc,10);
            Inc(loc,xLength);
            end
          else
            Break;
          end
        else
          Break;
        end;
      else
        RaiseError('Input Stream error'+FromAddr(Sender)+'/'+iFrom);
      end;
    end;

  xLeft:=len-loc;
  if xLeft>0 then
    begin
    {$IFDEF RTCGATE_DEBUG}LockInput(135);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      if iInput<>Conn then
        RaiseError('IN CONN Changed'+FromAddr(Sender)+'/'+iFrom) // input connection changed
      else if not oiAllReady then
        RaiseError('CONN Lost'+FromAddr(Sender)+'/'+iFrom); // connection lost
      prepareBuffer(iInBuff);
      iInBuff.AddEx(data,xLeft,loc);
      iInSize:=iInBuff.Size;
    finally
      {$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      end;
    end;

  SetLength(data,0);
  end;

{ TGateUserGroup }

constructor TRtcGateUserGroup.Create(Owner:TRtcGateOneUser; UID:TGateUID);
  begin
  grpCS:=TRtcCritSec.Create;
  grUID:=UID;
  grOwner:=Owner;
  grUsers:=tObjList.Create(16);
  end;

destructor TRtcGateUserGroup.Destroy;
  begin
  grpCS.Acquire;
  try
    grOwner:=nil;
    RtcFreeAndNil(grUsers);
  finally
    grpCS.Release;
    end;

  grpCS.Acquire;
  grpCS.Release;

  RtcFreeAndNil(grpCS);

  inherited;
  end;

procedure TRtcGateUserGroup.LockGroup(i:byte);
  begin
  try
    grpCS.Acquire;
  except
    on E:Exception do
      begin
      if LOG_GATEWAY_ERRORS then
        Log('LockGroup @'+Int2Str(i),E,GATE_LOG+'.LOCK');
      raise;
      end;
    end;
  end;

procedure TRtcGateUserGroup.UnLockGroup;
  begin
  try
    grpCS.Release;
  except
    on E:Exception do
      begin
      if LOG_GATEWAY_ERRORS then
        Log('UnLockGroup',E,GATE_LOG+'.LOCK');
      raise;
      end;
    end;
  end;

function TRtcGateUserGroup.ozSendFirst(oFromUser:TRtcGateOneUser;
                                      SenderUID,FromUID: TGateUID; TotalSize: Cardinal;
                                      const data: RtcByteArray;
                                      len, loc: Cardinal;
                                      ext:byte):integer;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
  begin
  Result:=0;
  LockGroup(6);
  try
    oid:=grUsers.search_min(obj);
    while (oid>0) and assigned(obj) do
      begin
      Inc(Result, oUser.ozSendFirst(oFromUser,SenderUID,FromUID,TotalSize,data,len,loc,ext) );
      oid:=grUsers.search_g(oid,obj);
      end;
  finally
    UnLockGroup;
    end;
  end;

function TRtcGateUserGroup.ozSendMore(oFromUser:TRtcGateOneUser;
                                     SenderUID,FromUID: TGateUID; LeftToSend:Cardinal;
                                     const data: RtcByteArray;
                                     len, loc: Cardinal;
                                     ext:byte):integer;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
  begin
  Result:=0;
  LockGroup(7);
  try
    oid:=grUsers.search_min(obj);
    while (oid>0) and assigned(obj) do
      begin
      Inc(Result, oUser.ozSendMore(oFromUser,SenderUID,FromUID,LeftToSend,data,len,loc,ext) );
      oid:=grUsers.search_g(oid,obj);
      end;
  finally
    UnLockGroup;
    end;
  end;

function TRtcGateUserGroup.oPostNote(Cmd:byte; FromUID: TGateUID; warn:boolean=True):integer;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
  begin
  Result:=0;
  LockGroup(9);
  try
    oid:=grUsers.search_min(obj);
    while (oid>0) and assigned(obj) do
      begin
      Inc(Result, oUser.oPostNote(Cmd,FromUID,warn) );
      oid:=grUsers.search_g(oid,obj);
      end;
  finally
    UnLockGroup;
    end;
  end;

function TRtcGateUserGroup.oPostInfo(Cmd:byte; FromUID: TGateUID; const FromInfo:RtcString):integer;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
  begin
  Result:=0;
  LockGroup(10);
  try
    oid:=grUsers.search_min(obj);
    while (oid>0) and assigned(obj) do
      begin
      Inc(Result, oUser.oPostInfo(Cmd,FromUID,FromInfo) );
      oid:=grUsers.search_g(oid,obj);
      end;
  finally
    UnLockGroup;
    end;
  end;

function TRtcGateUserGroup.oSendPing(Cmd:byte; FromUID:TGateUID):integer;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
  begin
  Result:=0;
  LockGroup(11);
  try
    oid:=grUsers.search_min(obj);
    while (oid>0) and assigned(obj) do
      begin
      Inc(Result, oUser.oSendPing(Cmd,FromUID) );
      oid:=grUsers.search_g(oid,obj);
      end;
  finally
    UnLockGroup;
    end;
  end;

{ TRtcAbsGateway }

constructor TRtcAbsGateway.Create(AOwner:TComponent);
  begin
  inherited;

  FGateFileName:='/';

  FGATEURI_STATUS:='/'+GATEURI_STATUS;
  FGATEURI_PING:='/'+GATEURI_PING;
  FGATEURI_LOGIN:='/'+GATEURI_LOGIN;
  FGATEURI_INPUT:='/'+GATEURI_INPUT;
  FGATEURI_OUTPUT:='/'+GATEURI_OUTPUT;
  FGATEURI_INPUTRESET:='/'+GATEURI_INPUTRESET;
  FGATEURI_OUTPUTRESET:='/'+GATEURI_OUTPUTRESET;

  FGATE_PRIMARY_KEY:='';

  FGATE_MAXINFOLEN:=0;
  FMaxUsersActive:=0;
  FMaxUsersTotal:=0;

  FCUsr:=TRtcCritSec.Create;
  FCStr:=TRtcCritSec.Create;
  FxUsers:=tObjList.Create(128);
  FxRemoval:=tObjList.Create(128);
  FxReserved:=tBinList.Create(128);

  FzStreamers:=tStrObjList.Create(128);
  FzListeners:=tStrObjList.Create(128);

  FzHosts:=tStrObjList.Create(128);
  FzControls:=tStrObjList.Create(128);
  FzPublic:=tStrObjList.Create(128);

  PingProvider:=TRtcDataProvider.Create(self);
  PingProvider.OnListenStart:=PingProviderListenStart;
  PingProvider.OnListenStop:=PingProviderListenStop;
  PingProvider.OnCheckRequest:=PingProviderCheckRequest;

  LoginProvider:=TRtcDataProvider.Create(self);
  LoginProvider.OnCheckRequest:=LoginProviderCheckRequest;
  LoginProvider.OnDataReceived:=LoginProviderDataReceived;
  LoginProvider.OnDisconnect:=LoginProviderDisconnect;

  InputStream:=TRtcDataProvider.Create(self);
  InputStream.OnCheckRequest:=InputStreamCheckRequest;
  InputStream.OnDataReceived:=InputStreamDataReceived;
  InputStream.OnDisconnect:=InputStreamDisconnect;

  OutputStream:=TRtcDataProvider.Create(self);
  OutputStream.OnCheckRequest:=OutputStreamCheckRequest;
  OutputStream.OnDataReceived:=OutputStreamDataReceived;
  OutputStream.OnDataSent:=OutputStreamDataSent;
  OutputStream.OnDisconnect:=OutputStreamDisconnect;

  SetLength(PingJobList,0);
  SetLength(PingUserList,0);
  PingUserCnt:=0;

  SetLength(CleanJobList,0);
  SetLength(CleanUserList,0);
  CleanUserCnt:=0;

{$IFDEF RTC_RSA}
  rnd:=TRtcISAAC.Create(True);
{$ELSE}
  Randomize;
{$ENDIF}
  end;

destructor TRtcAbsGateway.Destroy;
  begin
  FNotSilent:=False;
  FAmSilent:=True;

  PingProviderListenStop(Server);

  DoCloseAllUserChannels;
  DoRemoveAllUsers;

  Server:=nil;

  RtcFreeAndNil(PingProvider);
  RtcFreeAndNil(LoginProvider);
  RtcFreeAndNil(InputStream);
  RtcFreeAndNil(OutputStream);

  RtcFreeAndNil(FCUsr);
  RtcFreeAndNil(FCStr);
  RtcFreeAndNil(FxUsers);
  RtcFreeAndNil(FxRemoval);
  RtcFreeAndNil(FxReserved);
  RtcFreeAndNil(FzStreamers);
  RtcFreeAndNil(FzListeners);

  RtcFreeAndNil(FzHosts);
  RtcFreeAndNil(FzControls);
  RtcFreeAndNil(FzPublic);

  SetLength(PingJobList,0);
  SetLength(PingUserList,0);
  PingUserCnt:=0;

  SetLength(CleanJobList,0);
  SetLength(CleanUserList,0);
  CleanUserCnt:=0;

{$IFDEF RTC_RSA}
  RtcFreeAndNil(rnd);
{$ENDIF}

  inherited;
  end;

procedure TRtcAbsGateway.ErrorResponse(const Sender: TRtcConnection; const meth: RtcString; const E: Exception);
  begin
  if E is ERtcGateway then
    SilentResponse(Sender, '.ERR', meth, Utf8Encode(RtcWideString(E.Message)), False)
  else if E is ERtcGateClosed then
    SilentResponse(Sender, '.ERR_C', meth, Utf8Encode(RtcWideString(E.Message)), False)
  else if E is ERtcGateFatal then
    SilentResponse(Sender, '.ERR_F', meth, Utf8Encode(RtcWideString(E.Message)), True)
  else
    SilentResponse(Sender, '.ERR_'+E.ClassName, meth, '('+RtcString(E.ClassName)+') '+Utf8Encode(RtcWideString(E.Message)),False);
  end;

procedure TRtcAbsGateway.SilentResponse(const Sender: TRtcConnection; const lname:String; const meth, err: RtcString; fatal:boolean);
  begin
  try
    if Sender=nil then
      begin
      if LOG_GATEWAY_STATUS then
        Log(meth+' '+err,GATE_LOG+lname+'.NIL');
      end
    else if Sender.State<>conActive then
      begin
      if LOG_GATEWAY_STATUS then
        Log(meth+' '+err+' ('+Int2Str(crcGetUserID(1,Sender) shr UserIDShift)+' at '+ Sender.PeerAddr+':'+Sender.PeerPort+') - #CLOSED',GATE_LOG+lname);
      end
    else if Sender.Request.Complete and
      (Sender.Response.ContentOut=0) and
      (Sender.Response.ContentLength=0) and
      NOT (Sender.Response.Done or
           Sender.Response.Started or
           Sender.Response.Sending or
           Sender.Response.Sent) then
      begin
      if LOG_GATEWAY_STATUS then
        Log(meth+' '+err+' ('+Int2Str(crcGetUserID(3,Sender) shr UserIDShift)+' at '+ Sender.PeerAddr+':'+Sender.PeerPort+') - #REPLY',GATE_LOG+lname);

      if fatal then
        Sender.Response.Status(GATE_FATALERROR_CODE,GATE_FATALERROR_TEXT)
      else
        Sender.Response.Status(GATE_ERROR_CODE,GATE_ERROR_TEXT);

      Sender.Response.ContentLength:=length(err);
      Sender.Write(err);
      end
    else if LOG_GATEWAY_STATUS then
      Log(meth+' '+err+' ('+Int2Str(crcGetUserID(4,Sender) shr UserIDShift)+' at '+ Sender.PeerAddr+':'+Sender.PeerPort+') - #NOTCLOSED',GATE_LOG+lname);
  except
    on EX:Exception do
      try
        if LOG_GATEWAY_ERRORS then
          Log('#ERROR '+meth+' '+err+' / '+RtcString(EX.ClassName)+':'+Utf8Encode(RtcWideString(EX.Message)),'ERR.REPLY');
      except
        on EX2:Exception do
          try
            if LOG_GATEWAY_ERRORS then
              Log('#ERROR-2 '+meth+' '+RtcString(EX2.ClassName)+':'+Utf8Encode(RtcWideString(EX2.Message)),'ERR.ERR');
          except
            try
              if LOG_GATEWAY_ERRORS then
                Log('#ERROR-3 '+meth,'ERR.ERR');
            except
              if LOG_GATEWAY_ERRORS then
                Log('#ERROR-4','ERR.ERR');
            end;
          end;
      end;
    end;
  end;

type
  TRtcGatePingJob=class(TRtcJob)
  public
    Gate:TRtcAbsGateway;
    constructor Create(Owner:TRtcAbsGateway);

    function Run(Thr:TRtcThread):boolean; override;
    function SingleUse:boolean; override;
    end;

constructor TRtcGatePingJob.Create(Owner: TRtcAbsGateway);
  begin
  inherited Create;
  Gate:=Owner;
  end;

function TRtcGatePingJob.SingleUse:boolean;
  begin
  // do not destroy
  Result:=False;
  end;

function TRtcGatePingJob.Run(Thr:TRtcThread):boolean;
  begin
  Result:=False;
  try
    if assigned(Gate) then
      begin
      Gate.PingActiveUsers;
      //set up next trigger
      end;
  except
    on E:Exception do
      if LOG_GATEWAY_ERRORS then
        Log('PING Run',E,GATE_LOG+'.ERR.PING');
    end;
  end;

type
  TRtcGateCleanJob=class(TRtcJob)
  public
    Gate:TRtcAbsGateway;
    constructor Create(Owner:TRtcAbsGateway);

    function Run(Thr:TRtcThread):boolean; override;
    function SingleUse:boolean; override;
    end;

constructor TRtcGateCleanJob.Create(Owner: TRtcAbsGateway);
  begin
  inherited Create;
  Gate:=Owner;
  end;

function TRtcGateCleanJob.SingleUse:boolean;
  begin
  // do not destroy
  Result:=False;
  end;

function TRtcGateCleanJob.Run(Thr:TRtcThread):boolean;
  begin
  Result:=False;
  try
    if assigned(Gate) then
      begin
      Gate.RemoveInactiveUsers;
      //set up next trigger
      end;
  except
    on E:Exception do
      if LOG_GATEWAY_ERRORS then
        Log('CLEAN-UP Run',E,GATE_LOG+'.ERR.CLEAN');
    end;
  end;

procedure TRtcAbsGateway.PingProviderListenStart(Sender: TRtcConnection);
  begin
  if Sender.MultiThreaded then
    begin
    isMultiThreaded:=True;

    FCUsr.Acquire;
    try
      if PingTimer=nil then
        begin
        PingTimer:=TRtcTimer.Create(True);
        PingTimer.Thread.NeedThread:=True; // High priority thread
        GatePingJob:=TRtcGatePingJob.Create(self);
        TRtcTimer.Enable(PingTimer,PING_INTERVAL_inMS,GatePingJob,nil,True);

        CleanTimer:=TRtcTimer.Create(True);
        GateCleanJob:=TRtcGateCleanJob.Create(self);
        TRtcTimer.Enable(CleanTimer,CLEAN_INTERVAL_inMS,GateCleanJob,nil,True);
        end;
    finally
      FCUsr.Release;
      end;
    end
  else
    begin
    isMultiThreaded:=False;

    FCUsr.Acquire;
    try
      if PingTimer=nil then
        begin
        PingTimer:=TRtcTimer.Create(False);
        TRtcTimer.Enable(PingTimer,PING_INTERVAL_inMS,PingActiveUsers,True);

        CleanTimer:=TRtcTimer.Create(False);
        TRtcTimer.Enable(CleanTimer,CLEAN_INTERVAL_inMS,RemoveInactiveUsers,True);
        end;
    finally
      FCUsr.Release;
      end;
    end;
  end;

procedure TRtcAbsGateway.PingProviderListenStop(Sender: TRtcConnection);
  begin
  FCUsr.Acquire;
  try
    if assigned(PingTimer) then
      begin
      TRtcTimer.Stop(PingTimer);
      PingTimer:=nil;
      end;
    if assigned(GatePingJob) then
      RtcFreeAndNil(GatePingJob);

    if assigned(CleanTimer) then
      begin
      TRtcTimer.Stop(CleanTimer);
      CleanTimer:=nil;
      end;
    if assigned(GateCleanJob) then
      RtcFreeAndNil(GateCleanJob);
  finally
    FCUsr.Release;
    end;
  end;

procedure TRtcAbsGateway.ReadAndProcessData(direct:boolean; Conn:RtcIntPtr; obj:TObject);
  var
    Sender:TRtcConnection absolute Conn;
    oUser:TRtcGateOneUser absolute obj;
    dat:RtcByteArray;
    lenRead:integer;
    done:boolean;
    toRead:int64;
  begin
  done:=False;
  toRead:=0;
  SetLength(dat,0);

  oUser.{$IFDEF RTCGATE_DEBUG}LockInput(130);{$ELSE}inCS.Acquire;{$ENDIF}
  try
    if oUser.iInput<>Conn then
      begin
      if direct then
        RaiseError('IN CONN Changed'+FromAddr(Sender)+'/'+oUser.iFrom)
      else
        Exit;
      end
    else if direct=oUser.iWaiting then
      begin
      {$IFDEF RTCGATE_DEBUG}xLog('Read&Process: direct=iWaiting! #EXIT'+FromAddr(Sender)+'/'+oUser.iFrom,IntToStr(oUser.xUID shr UserIDShift));{$ENDIF}
      Exit;
      end;

    if not direct then
      begin
      oUser.iWaiting:=False;
      {$IFDEF RTCGATE_FULLDEBUG}xLog('Read&Process: iWaiting:=False'+FromAddr(Sender)+'/'+oUser.iFrom,IntToStr(oUser.xUID shr UserIDShift));{$ENDIF}
      end;

    if (oUser.iLeftToRead>0) and oUser.oiAllReady then
      begin
      if oUser.iBeforeRead then
        toRead:=oUser.iLeftToRead;
      end
    else if oUser.iLeftToRead=0 then
      done:=Sender.Request.Complete and
       not (Sender.Response.Started or
            Sender.Response.Sending or
            Sender.Response.Sent or
            Sender.Response.Done);
  finally
    oUser.{$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
    end;

  if toRead>0 then
    begin
    dat:=Sender.ReadEx;
    lenRead:=length(dat);
    oUser.iAfterRead(lenRead);
    {$IFDEF RTCGATE_DEBUG}xLog('Read('+IntToStr(lenRead)+'/'+IntToStr(toRead)+')'+FromAddr(Sender)+'/'+oUser.iFrom,IntToStr(oUser.xUID shr UserIDShift));{$ENDIF}
    toRead:=toRead-lenRead;
    if toRead<=0 then
      begin
      {$IFDEF RTCGATE_FULLDEBUG}xLog('Read: DONE'+FromAddr(Sender)+'/'+oUser.iFrom,IntToStr(oUser.xUID shr UserIDShift));{$ENDIF}
      done:=True;
      end;
    oUser.iLeftToRead:=toRead;
    if lenRead>0 then
      try
        oUser.ixoProcessData(Conn,dat);
      finally
        SetLength(dat,0);
        end;
    end;

  if Sender.Response.StatusCode<>GATE_OK_CODE then
    oUser.ixoResetInput(136,Conn)
  else if done then
    begin
    Sender.Response.ContentLength:=0;
    Sender.Write;
    oUser.iCloseInput(Conn);
    end;
  end;

procedure TRtcAbsGateway.ReleaseUID(var uid:TGateUID);
  begin
  FCUsr.Acquire;
  try
    if FxReserved.search(uid)>0 then
      FxReserved.remove(uid);
    uid:=0;
  finally
    FCUsr.Release;
    end;
  end;

function TRtcAbsGateway.GetNextUID: TGateUID;
  var
    loop,found:boolean;
    cnt:integer;
  begin
  Result:=0;
  loop:=false;
  FCUsr.Acquire;
  try
    cnt:=FxUsers.Count+FxReserved.Count;
    if FMaxUsersActive>0 then
      begin
      if cnt>=FMaxUsersActive then Exit;
      end
    else if cnt>=DEF_GATEWAY_ACTIVEUSER_LIMIT then Exit;
    Inc(cnt,FxRemoval.Count);
    if FMaxUsersTotal>0 then
      begin
      if cnt>=FMaxUsersTotal then Exit;
      end
    else if cnt>=DEF_GATEWAY_TOTALUSER_LIMIT then Exit;
  finally
    FCUsr.Release;
    end;

  repeat
  {$IFDEF RTC_RSA}
    if loop then
      Result:= (MinLowID + TGateUID(rnd.Random(CntLowIDs))) shl UserIDShift
    else
      Result:= (MinHigID + TGateUID(rnd.Random(CntHigIDs))) shl UserIDShift;
  {$ELSE}
    if loop then
      Result:= (MinLowID + TGateUID(Random(CntLowIDs))) shl UserIDShift
    else
      Result:= (MinHigID + TGateUID(Random(CntHigIDs))) shl UserIDShift;
  {$ENDIF}
    FCUsr.Acquire;
    try
      found:=(FxUsers.search(Result)=nil) and
             (FxRemoval.search(Result)=nil) and
             (FxReserved.search(Result)=0);
      if found then
        FxReserved.insert(Result,1);
    finally
      FCUsr.Release;
      end;
    loop:=not loop;
    until found;
  end;

function TRtcAbsGateway.CreateUser(Sender:TRtcConnection;
                                 UserID:TGateUID;
                                 const UserAuth:RtcString;
                                 const UserInfo:RtcString;
                                 const CryptKey:RtcString):boolean;
  var
    obj:TRtcGateOneUser;
  begin
  Result:=False;
  try
    obj:=TRtcGateOneUser.Create(ReceiveSpeedLimit,
                                StreamSpeedLimit,
                                UserID,
                                UserAuth,
                                UserInfo,
                                GetUserAddr(Sender),
                                CryptKey,
                                self);
  except
    on E:Exception do
      begin
      FCUsr.Acquire;
      try
        FxReserved.remove(UserID);
      finally
        FCUsr.Release;
        end;
      raise;
      end;
    end;

  if assigned(obj) then
    begin
    FCUsr.Acquire;
    try
      FxReserved.remove(UserID);
      FxUsers.insert(UserID,obj);
    finally
      FCUsr.Release;
      end;
    Result:=True;
    end
  else
    begin
    FCUsr.Acquire;
    try
      FxReserved.remove(UserID);
    finally
      FCUsr.Release;
      end;
    end;
  end;

procedure TRtcAbsGateway.DoLogOutUser(UserID: TGateUID);
  var
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    removed,found:boolean;
  begin
  found:=False;
  removed:=False;
  FCUsr.Acquire;
  try
    obj:=FxUsers.search(UserID);
    if assigned(obj) then
      begin
      found:=True;
      if FxRemoval.search(UserID)=nil then
        begin
        FxRemoval.insert(UserID,obj);
        removed:=True;
        end;
      FxUsers.remove(UserID);
      end;
  finally
    FCUsr.Release;
    end;
  if found then
    if removed then
      oUser.CleanUp(False,True,True,0,True,True,False)
    else
      RtcFreeAndNil(obj);
  end;

procedure TRtcAbsGateway.DoRemoveUser(UserID: TGateUID);
  var
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    found:boolean;
  begin
  found:=False;
  FCUsr.Acquire;
  try
    obj:=FxUsers.search(UserID);
    if assigned(obj) then
      begin
      FxUsers.remove(UserID);
      found:=True;
      end
    else
      begin
      obj:=FxRemoval.search(UserID);
      if assigned(obj) then
        begin
        FxRemoval.remove(UserID);
        found:=True;
        end;
      end;
  finally
    FCUsr.Release;
    end;
  if found then
    RtcFreeAndNil(obj);
  end;

procedure TRtcAbsGateway.DoRemoveAllUsers;
  var
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    uid:TGateUID;
    found:boolean;
  begin
  found:=True;
  repeat
    FCUsr.Acquire;
    try
      uid:=FxUsers.search_min(obj);
      if (uid>0) and assigned(obj) then
        FxUsers.remove(uid)
      else
        begin
        uid:=FxRemoval.search_min(obj);
        if (uid>0) and assigned(obj) then
          FxRemoval.remove(uid)
        else
          found:=False;
        end;
    finally
      FCUsr.Release;
      end;
    if found then
      RtcFreeAndNil(obj);
    until not found;
  end;

procedure TRtcAbsGateway.PingActiveUsers;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
    MyTime,MyTime2,MyTime3,MyTime4:TAppRunTime;
    MyPingTime:TAppRunTime;
    i:integer;
    MyJob:byte;
    UserWasPaused,
    CanPingUser:boolean;
  procedure ToSendPing;
    begin
    if UserWasPaused then
      PingJobList[PingUserCnt-1]:=PingJobList[PingUserCnt-1] or 4 // send PING
    else
      begin
      if length(PingUserList)<=PingUserCnt then
        begin
        SetLength(PingUserList,PingUserCnt+1000);
        SetLength(PingJobList,PingUserCnt+1000);
        end;
      PingUserList[PingUserCnt]:=oUser;
      PingJobList[PingUserCnt]:=4; // send PING
      Inc(PingUserCnt);
      end;
    end;
  procedure ExecSendPing;
    begin
    oUser.PostCheckPing;
    end;
  procedure ToContinueInput;
    begin
    if length(PingUserList)<=PingUserCnt then
      begin
      SetLength(PingUserList,PingUserCnt+1000);
      SetLength(PingJobList,PingUserCnt+1000);
      end;
    PingUserList[PingUserCnt]:=oUser;
    PingJobList[PingUserCnt]:=1; // oUser.PostContinueInput;
    Inc(PingUserCnt);
    UserWasPaused:=True;
    end;
  procedure ExecContinueInput;
    begin
    oUser.PostContinueInput;
    end;
  procedure ToContinueOutput;
    begin
    if UserWasPaused then
      PingJobList[PingUserCnt-1]:=3 // oUser.PostContinueInput + oUser.PostContinueOutput
    else
      begin
      if length(PingUserList)<=PingUserCnt then
        begin
        SetLength(PingUserList,PingUserCnt+1000);
        SetLength(PingJobList,PingUserCnt+1000);
        end;
      PingUserList[PingUserCnt]:=oUser;
      PingJobList[PingUserCnt]:=2; // oUser.PostContinueOutput;
      Inc(PingUserCnt);
      UserWasPaused:=True;
      end;
    end;
  procedure ExecContinueOutput;
    begin
    oUser.PostContinueOutput;
    end;
  begin
  PingUserCnt:=0;
  MyTime:=GetAppRunTime;
  MyTime2:=MyTime;
  MyPingTime:=MyTime-SENDPING_INTERVAL;
  try
    FCUsr.Acquire;
    try
      oid:=FxUsers.search_min(obj);
      MyTime2:=GetAppRunTime;
      while (oid>0) and assigned(Obj) do
        begin
        if oUser.oiAllReady then
          begin
          //FCUsr.Release;
          //try
            UserWasPaused:=False;
            if oUser.iPaused and (MyTime>=oUser.iContinue) then
              ToContinueInput;
            if not oUser.oPaused then
              CanPingUser:=oUser.oOutput<>0
            else if MyTime>=oUser.oContinue then
              begin
              CanPingUser:=oUser.oOutput<>0;
              ToContinueOutput;
              end
            else
              CanPingUser:=False;
            if CanPingUser then
              if (MyPingTime>oUser.oLastPing) and
                 (MyPingTime>oUser.oLastOutput) then
                ToSendPing;
          //finally
          //  FCUsr.Acquire;
          //  end;
          end;
        oid:=FxUsers.search_g(oid,obj);
        end;
    finally
      FCUsr.Release;
      end;
  except
    on E:Exception do
      if LOG_GATEWAY_ERRORS then
        Log('Ping.Check '+RtcString(E.ClassName)+':'+RtcString(E.Message),GATE_LOG+'.ERR.PING');
    end;
  MyTime3:=GetAppRunTime;
  if PingUserCnt>0 then
    begin
    try
      // Ping active users
      for i:=0 to PingUserCnt-1 do
        begin
        MyJob:=PingJobList[i];
        obj:=PingUserList[i];
        PingUserList[i]:=nil;
        PingJobList[i]:=0;
        try
          if MyJob and 1 = 1 then
            ExecContinueInput;
          if MyJob and 2 = 2 then
            ExecContinueOutput;
          if MyJob and 4 = 4 then
            ExecSendPing;
        except
          on E:Exception do
            if LOG_GATEWAY_ERRORS then
              try
                Log('Ping.Run '+Int2Str(MyJob)+' to #'+Int2Str(oUser.xUID shr UserIDShift)+' -> '+RtcString(E.ClassName)+':'+RtcString(E.Message),GATE_LOG+'.ERR.PING');
              except
                on E:Exception do
                  Log('Ping.Run ### '+RtcString(E.ClassName)+':'+RtcString(E.Message),GATE_LOG+'.ERR.PING');
                end;
          end;
        end;
    except
      on E:Exception do
        if LOG_GATEWAY_ERRORS then
          Log('Ping.Run '+RtcString(E.ClassName)+':'+RtcString(E.Message),GATE_LOG+'.ERR.PING');
      end;
    PingUserCnt:=0;
    end;
  MyTime4:=GetAppRunTime;

  if LOG_GATEWAY_STATUS then
    if MyTime4-MyTime>=1000 then // PING check took more than 1 second?
      Log('Ping Time: '+Int2Str(MyTime4-MyTime)+'ms ('+
                        Int2Str(MyTime2-MyTime)+'+'+
                        Int2Str(MyTime3-MyTime2)+'+'+
                        Int2Str(MyTime4-MyTime3)+')',GATE_LOG+'.PING');

  if assigned(PingTimer) then
    if isMultiThreaded then
      begin
      if assigned(GatePingJob) then
        TRtcTimer.Enable(PingTimer,PING_INTERVAL_inMS,GatePingJob,nil,True);
      end
    else
      TRtcTimer.Enable(PingTimer,PING_INTERVAL_inMS,PingActiveUsers,True);
  end;

procedure TRtcAbsGateway.RemoveInactiveUsers;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
    MyTime,MyTime2,MyTime3,MyTime4,MyTime5:TAppRunTime;
    MyInTime,MyOutTime,MyMaxInTime,MyMaxOutTime,
    MyOpenTime,MyLogOutTime,MyDoneTime:TAppRunTime;
    i:integer;
    MyJob:byte;
  function ResID2Txt(i:byte):RtcString;
    begin
    case i of
      151: Result:='INPUT idle max.Timeout';
      152: Result:='INPUT + OUTPUT idle Timeout';
      153: Result:='OUTPUT idle max.Timeout';
      154: Result:='OUTPUT connect Timeout+';
      155: Result:='INPUT idle max.Timeout';
      156: Result:='INPUT idle + OUTPUT connect Timeout';
      157: Result:='INPUT connect Timeout+';
      158: Result:='OUTPUT idle max.Timeout';
      159: Result:='INPUT connect + OUTPUT idle Timeout';
      160: Result:='OUTPUT connect Timeout+';
      161: Result:='INPUT connect Timeout+';
      162: Result:='INPUT + OUTPUT connect Timeout';
      163: Result:='OUTPUT idle Timeout';
      164: Result:='OUTPUT connect Timeout';
      165: Result:='INPUT idle Timeout';
      166: Result:='INPUT connect Timeout';
      else Result:='CLEAN Timeout';
      end;
    end;
  procedure ExecResetInOut(i:byte);
    var
      pinfo:RtcString;
      MyTime2:TAppRunTime;
    begin
    MyTime2:=GetAppRunTime-MyTime;
    pinfo:='';
    if oUser.iPaused then pinfo:=pinfo+'Ip'+Int2Str(oUser.iContinue-MyTime);
    if oUser.oPaused then pinfo:=pinfo+'Op'+Int2Str(oUser.oContinue-MyTime);
    if LOG_GATEWAY_TIMEOUTS then
      Log(ResID2Txt(i)+' ['+pinfo+'] '+Int2Str(oUser.xUID shr UserIDShift)+
          ' ('+Float2Str((MyTime-oUser.iLastInput)/RUN_TIMER_PRECISION)+
          '/'+ Float2Str((MyTime-oUser.oLastOutput)/RUN_TIMER_PRECISION)+' s) '+
          '[c'+Int2Str(MyTime2)+'ms] i='+oUser.iFrom+'; o='+oUser.oFrom,GATE_LOG+'.TIMEOUT');
    oUser.ixoResetInOutStreams(i);
    end;
  procedure ToResetInOut(i:byte);
    begin
    if length(CleanUserList)<=CleanUserCnt then
      begin
      SetLength(CleanUserList,CleanUserCnt+1000);
      SetLength(CleanJobList,CleanUserCnt+1000);
      end;
    CleanUserList[CleanUserCnt]:=oUser;
    CleanJobList[CleanUserCnt]:=i; // reset in+out
    Inc(CleanUserCnt);
    end;
  procedure ToLogOutUser;
    begin
    if length(CleanUserList)<=CleanUserCnt then
      begin
      SetLength(CleanUserList,CleanUserCnt+1000);
      SetLength(CleanJobList,CleanUserCnt+1000);
      end;
    CleanUserList[CleanUserCnt]:=oUser;
    CleanJobList[CleanUserCnt]:=8;
    Inc(CleanUserCnt);
    end;
  procedure ExecLogOutUser;
    begin
    if LOG_GATEWAY_TIMEOUTS then
      Log('USER Logged OUT '+Int2Str(oUser.xUID shr UserIDShift)+
          ' ('+Float2Str((MyTime-oUser.iLastInput)/RUN_TIMER_PRECISION)+
           '/'+Float2Str((MyTime-oUser.oLastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG+'.TIMEOUT');
    try
      if assigned(FBeforeUserLogout) then
        FBeforeUserLogout(nil,oUser.xUID shr UserIDShift,oUser.xAuth,oUser.xInfo);
    finally
      DoLogOutUser(oUser.xUID);
      end;
    end;
  procedure ToRemoveUser;
    begin
    if length(CleanUserList)<=CleanUserCnt then
      begin
      SetLength(CleanUserList,CleanUserCnt+1000);
      SetLength(CleanJobList,CleanUserCnt+1000);
      end;
    CleanUserList[CleanUserCnt]:=oUser;
    CleanJobList[CleanUserCnt]:=9;
    Inc(CleanUserCnt);
    end;
  procedure ExecRemoveUser;
    begin
    if LOG_GATEWAY_TIMEOUTS then
      Log('USER Removed '+Int2Str(oUser.xUID shr UserIDShift)+
          ' ('+Float2Str((MyTime-oUser.iLastInput)/RUN_TIMER_PRECISION)+
           '/'+Float2Str((MyTime-oUser.oLastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG+'.TIMEOUT');
    DoRemoveUser(oUser.xUID);
    end;
  begin
  CleanUserCnt:=0;
  MyTime:=GetAppRunTime;

  MyInTime    :=MyTime-GATECHECKIN_TIMEOUT;
  MyOutTime   :=MyTime-GATECHECKOUT_TIMEOUT;
  MyMaxInTime :=MyTime-GATECHECKMAXIN_TIMEOUT;
  MyMaxOutTime:=MyTime-GATECHECKMAXOUT_TIMEOUT;
  MyOpenTime  :=MyTime-GATECHECKOPEN_TIMEOUT;
  MyLogOutTime:=MyTime-GATECHECKLOGOUT_TIMEOUT;
  MyDoneTime  :=MyTime-GATECHECKDONE_TIMEOUT;

  MyTime2:=MyTime;
  MyTime3:=MyTime;
  try
    FCUsr.Acquire;
    try
      oid:=FxUsers.search_min(obj);
    finally
      FCUsr.Release;
      end;
    MyTime2:=GetAppRunTime;
    while (oid>0) and assigned(Obj) do
      begin
      if oUser.oiAllReady then
        begin
        if oUser.iInput<>0 then
          begin
          if oUser.oOutput<>0 then // have Input & Output
            begin
            if not (oUser.iStreaming or oUser.iPaused) and
                   (MyMaxInTime>oUser.iLastInput) then
              ToResetInOut(151)
            else if not oUser.oPaused then
              begin
              if not (oUser.iStreaming or oUser.iPaused) and
                     (MyInTime>oUser.iLastInput) and
                     (MyOutTime>oUser.oLastOutput) then
                ToResetInOut(152)
              else if (MyMaxOutTime>oUser.oLastOutput) then
                ToResetInOut(153);
              end;
            end
          else // have Input
            begin
            if (MyOutTime>oUser.oLastOutput) then
              ToResetInOut(154)
            else if not (oUser.iStreaming or oUser.iPaused) then
              begin
              if (MyMaxInTime>oUser.iLastInput) then
                ToResetInOut(155)
              else if (MyInTime>oUser.iLastInput) and
                      (MyOpenTime>oUser.oLastOutput) then
                ToResetInOut(156)
              end;
            end;
          end
        else if oUser.oOutput<>0 then // have Output
          begin
          if (MyInTime>oUser.iLastInput) then
            ToResetInOut(157)
          else if not oUser.oPaused then
            begin
            if (MyMaxOutTime>oUser.oLastOutput) then
              ToResetInOut(158)
            else if (MyOpenTime>oUser.iLastInput) and
                    (MyOutTime>oUser.oLastOutput) then
              ToResetInOut(159);
            end;
          end
        else // no Input/Output
          begin
          if (MyOutTime>oUser.oLastOutput) then
            ToResetInOut(160)
          else if (MyInTime>oUser.iLastInput) then
            ToResetInOut(161)
          else if (MyOpenTime>oUser.iLastInput) and
                  (MyOpenTime>oUser.oLastOutput) then
            ToResetInOut(162);
          end;
        end
      else if oUser.oOutputReady and oUser.oiUsed then
        begin
        if oUser.oOutput<>0 then
          begin
          if (MyOutTime>oUser.oLastOutput) then
            ToResetInOut(163);
          end
        else if (MyOpenTime>oUser.oLastOutput) then
          ToResetInOut(164);
        end
      else if oUser.iInputReady and oUser.oiUsed then
        begin
        if oUser.iInput<>0 then
          begin
          if (MyInTime>oUser.iLastInput) then
            ToResetInOut(165);
          end
        else if (MyOpenTime>oUser.oLastOutput) then
          ToResetInOut(166);
        end
      else if (MyLogOutTime>oUser.oLastOutput) or
              (MyLogOutTime>oUser.iLastInput) then
        ToLogOutUser;
      FCUsr.Acquire;
      try
        oid:=FxUsers.search_g(oid,obj);
      finally
        FCUsr.Release;
        end;
      end;

    MyTime3:=GetAppRunTime;

    FCUsr.Acquire;
    try
      oid:=FxRemoval.search_min(obj);
    finally
      FCUsr.Release;
      end;
    while (oid>0) and assigned(Obj) do
      begin
      if (MyDoneTime>oUser.oLastOutput) or
         (MyDoneTime>oUser.iLastInput) then
        ToRemoveUser;
      FCUsr.Acquire;
      try
        oid:=FxRemoval.search_g(oid,obj);
      finally
        FCUsr.Release;
        end;
      end;
  except
    on E:Exception do
      if LOG_GATEWAY_ERRORS then
        Log('Clean.Check '+RtcString(E.ClassName)+':'+RtcString(E.Message),GATE_LOG+'.ERR.CLEAN');
    end;

  MyTime4:=GetAppRunTime;

  if CleanUserCnt>0 then
    begin
    try
      // Close inactive connections, log-out and remove users
      for i:=0 to CleanUserCnt-1 do
        begin
        MyJob:=CleanJobList[i];
        obj:=CleanUserList[i];
        CleanUserList[i]:=nil;
        CleanJobList[i]:=0;
        try
          case MyJob of
            8: ExecLogOutUser;
            9: ExecRemoveUser;
            else ExecResetInOut(MyJob);
            end;
        except
          on E:Exception do
            if LOG_GATEWAY_ERRORS then
              try
                Log('Clean.Run '+Int2Str(MyJob)+' to #'+Int2Str(oUser.xUID shr UserIDShift)+' -> '+RtcString(E.ClassName)+':'+RtcString(E.Message),GATE_LOG+'.ERR.CLEAN');
              except
                on E:Exception do
                  Log('Clean.Run ### '+RtcString(E.ClassName)+':'+RtcString(E.Message),GATE_LOG+'.ERR.CLEAN');
                end;
          end;
        end;
    except
      on E:Exception do
        if LOG_GATEWAY_ERRORS then
          Log('Clean.Run '+RtcString(E.ClassName)+':'+RtcString(E.Message),GATE_LOG+'.ERR.CLEAN');
      end;
    end;

  MyTime5:=GetAppRunTime;

  if LOG_GATEWAY_STATUS then
    if MyTime5-MyTime>=1000 then // CLEAN check took more than 1 second?
      Log('Clean Time: '+Int2Str(MyTime5-MyTime)+'ms ('+
                        Int2Str(MyTime2-MyTime)+'+'+
                        Int2Str(MyTime3-MyTime2)+'+'+
                        Int2Str(MyTime4-MyTime3)+'+'+
                        Int2Str(MyTime5-MyTime4)+')',GATE_LOG+'.CLEAN');

  if assigned(CleanTimer) then
    if isMultiThreaded then
      begin
      if assigned(GateCleanJob) then
        TRtcTimer.Enable(CleanTimer,CLEAN_INTERVAL_inMS,GateCleanJob,nil,True);
      end
    else
      TRtcTimer.Enable(CleanTimer,CLEAN_INTERVAL_inMS,RemoveInactiveUsers,True);
  end;

function TRtcAbsGateway.CheckGatewayStatus:RtcString;
  const
    USER_STATES=10;
    UserCaptions:array[0..USER_STATES] of RtcString =
      ('NEW', 'EXPIRED', 'OFFLINE', 'INPUT', 'OUTPUT', 'INACTIVE', 'PAUSED', 'PAUSE-IN', 'PAUSE-OUT', 'ACTIVE', 'TOTAL');
  var
    obj:TObject;
    oid:TGateUID;
    sid:RtcString;
    oUser:TRtcGateOneUser absolute obj;
    chan:tBinList absolute obj;
    MyRes:RtcString;
    MyTime,MyTime2,MyTime3,MyTime4,MyTime5:TAppRunTime;
    a,User_Stat:integer;
    UsersTotal,
    ConnTotal,
    ChStream,UserStream,
    ChListen,UserListen,
    ChHost,UserHost,
    ChControl,UserControl,
    ChPublic,UserPublic:Cardinal;
    l_in,l_out:TAppRunTime;
    b_in,b_out:Int64;
    UsersCnt:array[0..USER_STATES] of integer;
    UsersINB,UsersOUTB,
    UsersINB1,UsersOUTB1,
    UsersINB2,UsersOUTB2:array[0..USER_STATES] of int64;
    UsersMAXIN,
    UsersMAXOUT,
    UsersMININ,
    UsersMINOUT:array[0..USER_STATES] of TAppRunTime;
  begin
  MyTime:=GetAppRunTime;

  for a:=0 to USER_STATES do
    begin
    UsersCnt[a]:=0;
    UsersMAXIN[a]:=0;
    UsersMAXOUT[a]:=0;
    UsersMININ[a]:=RtcMaxLargeInt;
    UsersMINOUT[a]:=RtcMaxLargeInt;
    UsersINB[a]:=0;
    UsersOUTB[a]:=0;
    UsersINB1[a]:=RtcMaxLargeInt;
    UsersOUTB1[a]:=RtcMaxLargeInt;
    UsersINB2[a]:=0;
    UsersOUTB2[a]:=0;
    end;
  UserStream:=0;
  UserListen:=0;
  UserHost:=0;
  UserControl:=0;
  UserPublic:=0;

  ConnTotal:=rtcServerConnectionCount;

  UsersMININ[0]:=0;
  UsersMINOUT[0]:=0;
  UsersINB1[0]:=0;
  UsersOUTB1[0]:=0;

  FCUsr.Acquire;
  try
    UsersTotal:=FxUsers.Count + FxReserved.Count + FxRemoval.Count;
    Inc(UsersCnt[0],FxReserved.Count);
    oid:=FxRemoval.search_min(obj);
  finally
    FCUsr.Release;
    end;

  User_Stat:=1;
  while (oid>0) and assigned(obj) do
    begin
    l_in:=oUser.iLastInput;
    l_out:=oUser.oLastOutput;
    b_in:=oUser.iInSize;
    b_out:=oUser.oOutSize;

    if l_in<UsersMININ[User_Stat] then UsersMININ[User_Stat]:=l_in;
    if l_in>UsersMAXIN[User_Stat] then UsersMAXIN[User_Stat]:=l_in;
    if l_out<UsersMINOUT[User_Stat] then UsersMINOUT[User_Stat]:=l_out;
    if l_out>UsersMAXOUT[User_Stat] then UsersMAXOUT[User_Stat]:=l_out;

    Inc(UsersINB[User_Stat],b_in);
    Inc(UsersOUTB[User_Stat],b_out);
    if b_in<UsersINB1[User_Stat] then UsersINB1[User_Stat]:=b_in;
    if b_in>UsersINB2[User_Stat] then UsersINB2[User_Stat]:=b_in;
    if b_out<UsersOUTB1[User_Stat] then UsersOUTB1[User_Stat]:=b_out;
    if b_out>UsersOUTB2[User_Stat] then UsersOUTB2[User_Stat]:=b_out;

    Inc(UsersCnt[User_Stat]);
    FCUsr.Acquire;
    try
      oid:=FxRemoval.search_g(oid,obj);
    finally
      FCUsr.Release;
      end;
    end;

  MyTime2:=GetAppRunTime;

  FCUsr.Acquire;
  try
    oid:=FxUsers.search_min(obj);
  finally
    FCUsr.Release;
    end;

  while (oid>0) and assigned(obj) do
    begin
    User_Stat:=2;
    if oUser.oiAllReady then
      begin
      Inc(User_Stat,4);
      if not oUser.oPaused then Inc(User_Stat);
      if not oUser.iPaused then Inc(User_Stat,2);
      end
    else
      begin
      if oUser.iInputReady then Inc(User_Stat);
      if oUser.oOutputReady then Inc(User_Stat,2);
      end;

    l_in:=oUser.iLastInput;
    l_out:=oUser.oLastOutput;
    b_in:=oUser.iInSize;
    b_out:=oUser.oOutSize;

    if l_in<UsersMININ[User_Stat] then UsersMININ[User_Stat]:=l_in;
    if l_in>UsersMAXIN[User_Stat] then UsersMAXIN[User_Stat]:=l_in;
    if l_out<UsersMINOUT[User_Stat] then UsersMINOUT[User_Stat]:=l_out;
    if l_out>UsersMAXOUT[User_Stat] then UsersMAXOUT[User_Stat]:=l_out;

    Inc(UsersINB[User_Stat],b_in);
    Inc(UsersOUTB[User_Stat],b_out);
    if b_in<UsersINB1[User_Stat] then UsersINB1[User_Stat]:=b_in;
    if b_in>UsersINB2[User_Stat] then UsersINB2[User_Stat]:=b_in;
    if b_out<UsersOUTB1[User_Stat] then UsersOUTB1[User_Stat]:=b_out;
    if b_out>UsersOUTB2[User_Stat] then UsersOUTB2[User_Stat]:=b_out;

    Inc(UsersCnt[User_Stat]);

    FCUsr.Acquire;
    try
      oid:=FxUsers.search_g(oid,obj);
    finally
      FCUsr.Release;
      end;
    end;

  MyTime3:=GetAppRunTime;

  FCStr.Acquire;
  try
    ChStream:=FzStreamers.Count;
    ChListen:=FzListeners.Count;
    ChHost:=FzHosts.Count;
    ChControl:=FzControls.Count;
    ChPublic:=FzPublic.Count;

    sid:=FzStreamers.search_min(obj);
    while (sid<>'') and assigned(obj) do
      begin
      Inc(UserStream,chan.Count);
      sid:=FzStreamers.search_g(sid,obj);
      end;
    sid:=FzListeners.search_min(obj);
    while (sid<>'') and assigned(obj) do
      begin
      Inc(UserListen,chan.Count);
      sid:=FzListeners.search_g(sid,obj);
      end;
    sid:=FzHosts.search_min(obj);
    while (sid<>'') and assigned(obj) do
      begin
      Inc(UserHost,chan.Count);
      sid:=FzHosts.search_g(sid,obj);
      end;
    sid:=FzControls.search_min(obj);
    while (sid<>'') and assigned(obj) do
      begin
      Inc(UserControl,chan.Count);
      sid:=FzControls.search_g(sid,obj);
      end;
    sid:=FzPublic.search_min(obj);
    while (sid<>'') and assigned(obj) do
      begin
      Inc(UserPublic,chan.Count);
      sid:=FzPublic.search_g(sid,obj);
      end;
  finally
    FCStr.Release;
    end;

  MyTime4:=GetAppRunTime;

  for a:=0 to USER_STATES-1 do
    begin
    if UsersCnt[a]>0 then
      begin
      if UsersMININ[a]<UsersMININ[USER_STATES]   then UsersMININ[USER_STATES]:=UsersMININ[a];
      if UsersMAXIN[a]>UsersMAXIN[USER_STATES]   then UsersMAXIN[USER_STATES]:=UsersMAXIN[a];
      if UsersMINOUT[a]<UsersMINOUT[USER_STATES] then UsersMINOUT[USER_STATES]:=UsersMINOUT[a];
      if UsersMAXOUT[a]>UsersMAXOUT[USER_STATES] then UsersMAXOUT[USER_STATES]:=UsersMAXOUT[a];

      if UsersINB1[a]<UsersINB1[USER_STATES]   then UsersINB1[USER_STATES]:=UsersINB1[a];
      if UsersINB2[a]>UsersINB2[USER_STATES]   then UsersINB2[USER_STATES]:=UsersINB2[a];
      if UsersOUTB1[a]<UsersOUTB1[USER_STATES] then UsersOUTB1[USER_STATES]:=UsersOUTB1[a];
      if UsersOUTB2[a]>UsersOUTB2[USER_STATES] then UsersOUTB2[USER_STATES]:=UsersOUTB2[a];

      Inc(UsersCnt[USER_STATES],UsersCnt[a]);
      Inc(UsersINB[USER_STATES],UsersINB[a]);
      Inc(UsersOUTB[USER_STATES],UsersOUTB[a]);
      end;
    end;

  MyTime5:=GetAppRunTime;

  MyRes:='Clients: <b>'+Int2Str(UsersTotal)+'</b> <br>'#13#10+
         'Sockets: <b>'+Int2Str(ConnTotal)+'</b> <br>'#13#10+
         'Threads: <b>'+Int2Str(RtcTotalThreadsIdle)+'</b> idle / <b>'+
                        Int2Str(RtcTotalThreadsBusy)+'</b> busy / <b>'+
                        Int2Str(RtcTotalJobsQueued)+'</b> queue<br>'+
         'Scan Time: <b>'+Float2Str((MyTime5-MyTime)/RUN_TIMER_PRECISION)+'</b> s ('+
         Float2Str((MyTime2-MyTime)/RUN_TIMER_PRECISION)+' + '+
         Float2Str((MyTime3-MyTime2)/RUN_TIMER_PRECISION)+' + '+
         Float2Str((MyTime4-MyTime3)/RUN_TIMER_PRECISION)+' + '+
         Float2Str((MyTime5-MyTime4)/RUN_TIMER_PRECISION)+')<br>'#13#10+
         'Server Up-Time: <b>'+Float2Str(MyTime3/RUN_TIMER_PRECISION)+'</b> s <br><br>'#13#10;

  if UsersTotal>0 then
    begin
    MyRes:=MyRes+'<table border=1><tr>'+
                 '<td> Client Status </td>'+
                 '<td> Client Count </td>'+
                 '<td> Input Buffer </td>'+
                 '<td> Output Buffer </td>'+
                 '<td> Last Input </td>'+
                 '<td> Last Output </td></tr>'#13#10;

    for a:=0 to USER_STATES do
      if UsersCnt[a]>0 then
        MyRes := MyRes + ' <tr><td> '+UserCaptions[a]+'</td><td>'+Int2Str(UsersCnt[a])+
                         ' </td><td> '+Int2Str(UsersINB[a] div 1024)+' KB ('+Int2Str(UsersINB1[a])+' - '+Int2Str(UsersINB2[a])+' B)'+
                         ' </td><td> '+Int2Str(UsersOUTB[a] div 1024)+' KB ('+Int2Str(UsersOUTB1[a])+' - '+Int2Str(UsersOUTB2[a])+' B)'+
                         ' </td><td> '+
                               Float2Str((MyTime5-UsersMAXIN[a])/RUN_TIMER_PRECISION)+
                         ' - '+Float2Str((MyTime5-UsersMININ[a])/RUN_TIMER_PRECISION)+' s'+
                         ' </td><td> '+
                               Float2Str((MyTime5-UsersMAXOUT[a])/RUN_TIMER_PRECISION)+
                         ' - '+Float2Str((MyTime5-UsersMINOUT[a])/RUN_TIMER_PRECISION)+
                         ' s </td></tr>'#13#10;
    MyRes:=MyRes+'</table><br>'#13#10;
    end;

  if ChListen+ChStream+ChHost+ChControl+ChPublic>0 then
    begin
    MyRes:=MyRes+'<table border=1><tr><td>Subs@Channels</td>';
    if ChStream>0 then
      MyRes:=MyRes+'<td>Stream</td>';
    if ChListen>0 then
      MyRes:=MyRes+'<td>Listen</td>';
    if ChPublic>0 then
      MyRes:=MyRes+'<td>Public</td>';
    if ChControl>0 then
      MyRes:=MyRes+'<td>Private</td> ';
    if ChHost>0 then
      MyRes:=MyRes+'<td>Linked</td>';
    MyRes:=MyRes+'<td>TOTAL</td></tr>';

    MyRes:=MyRes+'<tr><td>Subscriptions</td>';
    if ChStream>0 then
      MyRes:=MyRes+'<td>'+Int2Str(UserStream)+'</td>';
    if ChListen>0 then
      MyRes:=MyRes+'<td>'+Int2Str(UserListen)+'</td>';
    if ChPublic>0 then
      MyRes:=MyRes+'<td>'+Int2Str(UserPublic)+'</td>';
    if ChControl>0 then
      MyRes:=MyRes+'<td>'+Int2Str(UserControl)+'</td> ';
    if ChHost>0 then
      MyRes:=MyRes+'<td>'+Int2Str(UserHost)+'</td>';
    MyRes:=MyRes+'<td><b>'+Int2Str(UserListen+UserStream+UserHost+UserControl+UserPublic)+'</b></td></tr>';

    MyRes:=MyRes+'<tr><td>Channels</td>';
    if ChStream>0 then
      MyRes:=MyRes+'<td>'+Int2Str(ChStream)+'</td>';
    if ChListen>0 then
      MyRes:=MyRes+'<td>'+Int2Str(ChListen)+'</td>';
    if ChPublic>0 then
      MyRes:=MyRes+'<td>'+Int2Str(ChPublic)+'</td>';
    if ChControl>0 then
      MyRes:=MyRes+'<td>'+Int2Str(ChControl)+'</td> ';
    if ChHost>0 then
      MyRes:=MyRes+'<td>'+Int2Str(ChHost)+'</td>';
    MyRes:=MyRes+'<td><b>'+Int2Str(ChListen+ChStream+ChHost+ChControl+ChPublic)+'</b></td></tr></table>';
    end;

  Result:=MyRes;
  end;

procedure TRtcAbsGateway.PingProviderCheckRequest(Sender: TRtcConnection);
  var
    s:RtcString;
  begin
  if (Sender.Request.Method='GET') and
     (Sender.Request.ContentLength=0) then
    begin
    if (Sender.Request.FileName=FGATEURI_PING) then
      begin
      try
        Sender.Accept;
        Sender.Write(GetUserAddr(Sender));
      except
        on E:Exception do
          ErrorResponse(Sender,'PING Check',E);
        end;
      end
    else if (Sender.Request.FileName=FGATEURI_STATUS) then
      begin
      try
        Sender.Accept;
        s:=CheckGatewayStatus;
        Sender.Write('<html><body>'+s+'</body></html>');
        if LOG_GATEWAY_STATUS then
          Log(Sender.PeerAddr,'CHECK');
      except
        on E:Exception do
          ErrorResponse(Sender,'STATUS Check',E);
        end;
      end;
    end;
  end;

procedure TRtcAbsGateway.LoginProviderCheckRequest(Sender: TRtcConnection);
  begin
  if (Sender.Request.FileName=FGATEURI_LOGIN) and
     (Sender.Request.Method='POST') and
     ( (FGATE_MAXINFOLEN<=0) or
       (Sender.Request.ContentLength<=4+FGATE_MAXINFOLEN) ) then
    Sender.Accept;
  end;

procedure TRtcAbsGateway.LoginProviderDataReceived(Sender: TRtcConnection);
  var
    auth,info,ccode,newKey:RtcString;
    res:RtcByteArray;
    newUID,UID:TGateUID;
    len:integer;
  begin
  newUID:=0;
  try
    if Sender.Request.Complete then
      begin
      ccode:=FGATE_PRIMARY_KEY;
      Crypt(ccode, GetUserAddr(Sender));

      auth:=Sender.Read;
      if length(auth)>4 then
        begin
        DeCrypt(auth, ccode);

        // last 4 bytes have "UserAuth" length, the rest = "UserInfo"
        len:=crcBytes2Int(RtcStringToBytes(Copy(auth,length(auth)-3,4)));

        if length(auth)>len+4 then
          info:=Copy(auth,len+1,length(auth)-len-4)
        else
          info:='';
        if (len>0) and (length(auth)>=len+4) then
          auth:=Copy(auth,1,len)
        else
          auth:='';
        end
      else
        begin
        auth:='';
        info:='';
        end;

      newKey:='';
      newUID:=GetNextUID;

      if newUID=0 then
        begin
        info:='Gateway full';
        Sender.Response.Status(GATE_FATALERROR_CODE,GATE_FATALERROR_TEXT);
        Sender.Response.ContentLength:=length(info);
        Sender.Write(info);
        Exit;
        end;

      if assigned(FBeforeUserLogin) then
        FBeforeUserLogin(Sender,newUID shr UserIDShift,auth,info,newKey);

      if newKey='' then
        newKey:=ccode
      else
        begin
        Crypt(ccode,newKey);
        newKey:=ccode;
        end;

      if CreateUser(Sender,newUID,auth,info,newKey) then
        begin
        UID:=newUID; newUID:=0;
        res:=crcGateID2Bytes(UID shr UserIDShift);
        CryptEx(res,RtcStringToBytes(newKey));
        Sender.Response.ContentLength:=length(res);
        Sender.WriteEx(res);
        end
      else
        SilentResponse(Sender,'.LOGIN','LOGIN DataReceived','Error Creating user',False);
      end;
  except
    on E:Exception do
      begin
      if newUID>0 then ReleaseUID(newUID);
      ErrorResponse(Sender,'LOGIN Data',E);
      end;
    end;
  end;

procedure TRtcAbsGateway.LoginProviderDisconnect(Sender: TRtcConnection);
  begin
  if LOG_GATEWAY_STATUS then
    Log('LOGIN Disconnect '+Int2Str(crcGetUserID(9,Sender))+' at '+
        Int2Str(Sender.Request.ContentIn)+'/'+Int2Str(Sender.Response.ContentOut)+
        ' ('+Sender.PeerAddr+':'+Sender.PeerPort+')',GATE_LOG+'.DISCO');
  end;

procedure TRtcAbsGateway.InputStreamCheckRequest(Sender: TRtcConnection);
  var
    Conn:RtcIntPtr absolute Sender;
  procedure InternalResetInput;
    var
      obj:TObject;
      oUser:TRtcGateOneUser absolute obj;
    begin
    if FindUser4Input(105,Conn,obj,3) then
      begin
      oUser.CleanUp(True,False,False,Conn,True,False);
      oUser.ioOpenInput(Conn);
      end;
    end;
  procedure InternalOpenInput;
    var
      obj:TObject;
      oUser:TRtcGateOneUser absolute obj;
    begin
    if FindUser4Input(100,Conn,obj,2) then
      oUser.ioOpenInput(Conn);
    end;
  begin
  if Sender.Request.Method='POST' then
    if Sender.Request.FileName=FGATEURI_INPUT then
      begin
      try
        Sender.Accept;
        Sender.Request.ManualRead:=True;
        InternalOpenInput;
      except
        on E:Exception do
          ErrorResponse(Sender,'INPUT Open',E);
        end;
      end
    else if Sender.Request.FileName=FGATEURI_INPUTRESET then
      begin
      try
        Sender.Accept;
        Sender.Request.ManualRead:=True;
        InternalResetInput;
      except
        on E:Exception do
          ErrorResponse(Sender,'INPUT Reset',E);
        end;
      end;
  end;

procedure TRtcAbsGateway.InputStreamDataReceived(Sender: TRtcConnection);
  var
    Conn:RtcIntPtr absolute Sender;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    doit:boolean;
    level:shortint;
  begin
  try
    if Sender.Request.ContentLength<>Sender.Request.ContentIn then level:=1 else level:=0;
    if FindUser4Input(120,Conn,obj,level) then
      begin
      doit:=False;
      oUser.{$IFDEF RTCGATE_DEBUG}LockInput(121);{$ELSE}inCS.Acquire;{$ENDIF}
      try
        if oUser.iInput<>Conn then
          RaiseError('IN CONN Changed'+FromAddr(Sender)+'/'+oUser.iFrom)
        else if level=0 then
          doit:=True
        else if oUser.iStreaming then
          begin
          oUser.iWaiting:=True;
          {$IFDEF RTCGATE_DEBUG}xLog('InData: iStreaming? iWaiting:=True'+FromAddr(Sender)+'/'+oUser.iFrom,IntToStr(oUser.xUID shr UserIDShift));{$ENDIF}
          end
        else if oUser.iPaused then
          begin
          oUser.iWaiting:=True;
          {$IFDEF RTCGATE_DEBUG}xLog('InData: iPaused? iWaiting:=True'+FromAddr(Sender)+'/'+oUser.iFrom,IntToStr(oUser.xUID shr UserIDShift));{$ENDIF}
          end
        else if oUser.oiAllReady then
          doit:=True
        else
          begin
          oUser.iWaiting:=True;
          {$IFDEF RTCGATE_DEBUG}xLog('InData: NOT Ready? iWaiting:=True'+FromAddr(Sender)+'/'+oUser.iFrom,IntToStr(oUser.xUID shr UserIDShift));{$ENDIF}
          end;
        oUser.iLastInput:=GetAppRunTime;
      finally
        oUser.{$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
        end;
      if doit then ReadAndProcessData(True,Conn,obj);
      end;
  except
    on E:Exception do
      ErrorResponse(Sender,'INPUT Data',E);
    end;
  end;

procedure TRtcAbsGateway.InputStreamDisconnect(Sender: TRtcConnection);
  var
    Conn:RtcIntPtr absolute Sender;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
  begin
  try
    if FindUser4Input(148,Conn,obj,-1) then
      oUser.ixoResetInput(149,Conn);
  except
    on E:Exception do
      ErrorResponse(Sender,'INPUT Close',E);
    end;
  end;

procedure TRtcAbsGateway.OutputStreamCheckRequest(Sender: TRtcConnection);
  var
    Conn:RtcIntPtr absolute Sender;
  procedure InternalResetOutput;
    var
      obj:TObject;
      oUser:TRtcGateOneUser absolute obj;
    begin
    if FindUser4Output(205,Conn,obj,3) then
      begin
      oUser.CleanUp(True,False,False,Conn,False,True);
      oUser.oOpenOutput(Conn);
      end;
    end;
  procedure InternalOpenOutput;
    var
      obj:TObject;
      oUser:TRtcGateOneUser absolute obj;
    begin
    if FindUser4Output(200,Conn,obj,2) then
      oUser.oOpenOutput(Conn);
    end;
  begin
  if (Sender.Request.Method='POST') and (Sender.Request.ContentLength<64) then
    if Sender.Request.FileName=FGATEURI_OUTPUT then
      begin
      try
        Sender.Accept;
        InternalOpenOutput;
      except
        on E:Exception do
          ErrorResponse(Sender,'OUTPUT Open',E);
        end;
      end
    else if Sender.Request.FileName=FGATEURI_OUTPUTRESET then
      begin
      try
        Sender.Accept;
        InternalResetOutput;
      except
        on E:Exception do
          ErrorResponse(Sender,'OUTPUT Reset',E);
        end;
      end;
  end;

procedure TRtcAbsGateway.OutputStreamDataReceived(Sender: TRtcConnection);
  var
    Conn:RtcIntPtr absolute Sender;
  procedure InternalActivateOutput;
    var
      obj:TObject;
      oUser:TRtcGateOneUser absolute obj;
    begin
    if FindUser4Output(210,Conn,obj,1) then
      oUser.ixoActivateOutput(Conn);
    end;
  begin
  try
    if Sender.Request.Complete then
      InternalActivateOutput;
  except
    on E:Exception do
      ErrorResponse(Sender,'OUTPUT Activate',E);
    end;
  end;

procedure TRtcAbsGateway.OutputStreamDataSent(Sender: TRtcConnection);
  var
    Conn:RtcIntPtr absolute Sender;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    doit:boolean;
  begin
  try
    if Sender.Response.ContentLength>Sender.Response.ContentOut then
      if FindUser4Output(220,Conn,obj) then
        begin
        doit:=False;
        oUser.{$IFDEF RTCGATE_DEBUG}LockOutput(221);{$ELSE}outCS.Acquire;{$ENDIF}
        try
          if oUser.oLeftToSend<>Sender.Response.ContentLength-Sender.Response.ContentOut then
            doit:=False
          else if oUser.oOutput<>Conn then
            RaiseError('OUT CONN Changed'+FromAddr(Sender)+'/'+oUser.oFrom)
          else if oUser.oPaused then
            begin
            oUser.oSending:=False;
            {$IFDEF RTCGATE_DEBUG}xLog('OutSent (Paused): Sending=False'+FromAddr(Sender)+'/'+oUser.oFrom,IntToStr(ouser.xUID shr UserIDShift));{$ENDIF}
            end
          else if oUser.oiAllReady then
            doit:=True
          else
            begin
            oUser.oSending:=False;
            {$IFDEF RTCGATE_DEBUG}xLog('OutSent (NOT Ready): Sending=False'+FromAddr(Sender)+'/'+oUser.oFrom,IntToStr(ouser.xUID shr UserIDShift));{$ENDIF}
            end;
          oUser.oLastOutput:=GetAppRunTime;
        finally
          oUser.{$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
          end;
        if doit then
          oUser.oiDataSent(Conn,True);
        end;
  except
    on E:Exception do
      ErrorResponse(Sender,'OUTPUT Sent',E);
    end;
  end;

procedure TRtcAbsGateway.OutputStreamDisconnect(Sender: TRtcConnection);
  var
    Conn:RtcIntPtr absolute Sender;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
  begin
  try
    if FindUser4Output(248,Conn,obj,-1) then
      oUser.ixoResetOutput(249,Conn);
  except
    on E:Exception do
      ErrorResponse(Sender,'OUTPUT Closed',E);
    end;
  end;

procedure TRtcAbsGateway.SetServer(const Value: TRtcDataServer);
  begin
  if FServer<>Value then
    begin
    FServer := Value;

    PingProvider.Server:=FServer;
    InputStream.Server:=FServer;
    OutputStream.Server:=FServer;
    LoginProvider.Server:=FServer;

    if assigned(FServer) then
      FLink:=nil;
    end;
  end;

procedure TRtcAbsGateway.SetOrder(const Value: integer);
  begin
  if assigned(PingProvider) then
    begin
    PingProvider.CheckOrder:=Value;
    InputStream.CheckOrder:=Value;
    OutputStream.CheckOrder:=Value;
    LoginProvider.CheckOrder:=Value;
    end;
  end;

function TRtcAbsGateway.GetOrder: integer;
  begin
  if assigned(PingProvider) then
    Result:=PingProvider.CheckOrder
  else
    Result:=0;
  end;

procedure TRtcAbsGateway.SetGateFileName(const Value: RtcString);
  begin
  if FGateFileName<>Value then
    begin
    FGateFileName := Value;
    if Copy(FGateFileName,1,1)<>'/' then
      FGateFileName:='/'+FGateFileName;

    FGATEURI_STATUS:=FGateFileName+GATEURI_STATUS;
    FGATEURI_PING:=FGateFileName+GATEURI_PING;
    FGATEURI_LOGIN:=FGateFileName+GATEURI_LOGIN;
    FGATEURI_INPUT:=FGateFileName+GATEURI_INPUT;
    FGATEURI_OUTPUT:=FGateFileName+GATEURI_OUTPUT;
    FGATEURI_INPUTRESET:=FGateFileName+GATEURI_INPUTRESET;
    FGATEURI_OUTPUTRESET:=FGateFileName+GATEURI_OUTPUTRESET;
    end;
  end;

procedure TRtcAbsGateway.SetGatePrimaryKey(const Value: RtcString);
  begin
  FGATE_PRIMARY_KEY := Value;
  end;

procedure TRtcAbsGateway.SetMaxInfoLen(const Value: integer);
  begin
  FGATE_MAXINFOLEN := Value;
  end;

procedure TRtcAbsGateway.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent,Operation);
  if Operation=opRemove then
    if AComponent=FServer then
      SetServer(nil)
    else if AComponent=FLink then
      SetLink(nil);
  end;

procedure TRtcAbsGateway.AddUserToGroup(OwnerID, GroupID, UserID: TGateUID; NotifyUser, NotifyOwner: boolean);
  var
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
  begin
  if GroupID>MaxGID then
    raise ERtcOutOfBounds.Create('AddUserToGroup: GroupID out of range');
  obj:=FindGateUser(OwnerID shl UserIDShift);
  if assigned(obj) then
    oUser.ixoInternalAddUserToGroup(OwnerID shl UserIDShift, (OwnerID shl UserIDShift) or GroupID, UserID shl UserIDShift, NotifyUser, NotifyOwner, False);
  end;

procedure TRtcAbsGateway.RemoveUserFromGroup(OwnerID, GroupID, UserID: TGateUID; NotifyUser, NotifyOwner: boolean);
  var
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
  begin
  if GroupID>MaxGID then
    raise ERtcOutOfBounds.Create('RemoveUserFromGroup: GroupID out of range');
  obj:=FindGateUser(OwnerID shl UserIDShift);
  if assigned(obj) then
    oUser.xooInternalRemoveUserFromGroup((OwnerID shl UserIDShift) or GroupID, UserID shl UserIDShift, NotifyUser, NotifyOwner);
  end;

procedure TRtcAbsGateway.RemoveGroup(OwnerID, GroupID: TGateUID; NotifyUsers, NotifyOwner: boolean);
  var
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
  begin
  if GroupID>MaxGID then
    raise ERtcOutOfBounds.Create('RemoveGroup: GroupID out of range');
  obj:=FindGateUser(OwnerID shl UserIDShift);
  if assigned(obj) then
    oUser.ixoInternalRemoveGroup(OwnerID shl UserIDShift, (OwnerID shl UserIDShift) or GroupID, NotifyUsers, NotifyOwner);
  end;

procedure TRtcAbsGateway.AddUserToChannel(UserID:TGateUID; const Channel: RtcString;
                                       HostGroupID:TGateUID; UserIsHost, NotifyHosts:boolean;
                                       ListenerGroupID:TGateUID; UserIsListener, NotifyListeners:boolean);
  var
    obj:TObject;
    chan:tBinList absolute obj;
    uid,gid:TGateUID;
  begin
  if UserID=0 then
    begin
    if LOG_GATEWAY_STATUS then
      Log('TRtcAbsGateway.AddUserToChannel - NO UID',GATE_LOG+'.HINT');
    Exit;
    end;

  if HostGroupID>MaxGID then
    raise ERtcOutOfBounds.Create('AddUserToChannel: HostGroupID out of range');
  if ListenerGroupID>MaxGID then
    raise ERtcOutOfBounds.Create('AddUserToChannel: ListenerGroupID out of range');

  FCStr.Acquire;
  try
    if UserIsHost then
      begin
      // Find Streamer channel
      obj:=FzStreamers.search(Channel);
      if obj=nil then // Streamer channel not found
        begin
        // Create Streamer channel
        chan:=tBinList.Create(16);
        FzStreamers.insert(Channel,chan);
        end;

      // Find User in Streamer channel
      if chan.search(UserID)=0 then // User not found
        begin
        // Add user to Streamer channel
        chan.insert(UserID,HostGroupID+1);

        // Find Listener channel
        obj:=FzListeners.search(Channel);
        if Assigned(obj) then
          begin
          // Enumarate through all Listeners on this Channel
          uid:=chan.search_min(gid);
          while (uid>0) and (gid>0) do
            begin
            if UserID<>uid then
              begin
              FCStr.Release;
              try
                if HostGroupID>0 then
                  AddUserToGroup(UserID,HostGroupID,uid,NotifyListeners,NotifyHosts);
                if gid>1 then
                  AddUserToGroup(uid,gid-1,UserID,NotifyHosts,NotifyListeners);
              finally
                FCStr.Acquire;
                end;
              end;
            uid:=chan.search_g(uid,gid);
            end;
          end;
        end;
      end;

    if UserIsListener then
      begin
      // Find Listener channel
      obj:=FzListeners.search(Channel);
      if obj=nil then // Listener channel not found
        begin
        // Create Listener channel
        chan:=tBinList.Create(16);
        FzListeners.insert(Channel,chan);
        end;

      // Find User in Listener channel
      if chan.search(UserID)<=0 then // User not found
        begin
        // Add user to Listener channel
        chan.insert(UserID,ListenerGroupID+1);

        // Find Streamer channel
        obj:=FzStreamers.search(Channel);
        if Assigned(obj) then
          begin
          // Enumarate through all Streamers on this Channel
          uid:=chan.search_min(gid);
          while (uid>0) and (gid>0) do
            begin
            if UserID<>uid then
              begin
              FCStr.Release;
              try
                if ListenerGroupID>0 then
                  AddUserToGroup(UserID,ListenerGroupID,uid,NotifyHosts,NotifyListeners);
                if gid>1 then
                  AddUserToGroup(uid,gid-1,UserID,NotifyListeners,NotifyHosts);
              finally
                FCStr.Acquire;
                end;
              end;
            uid:=chan.search_g(uid,gid);
            end;
          end;
        end;
      end;
  finally
    FCStr.Release;
    end;
  end;

procedure TRtcAbsGateway.AddHostToChannel(UserID: TGateUID; const Channel: RtcString; HostGroupID: TGateUID; NotifyHost, NotifyListeners: boolean);
  begin
  if HostGroupID>MaxGID then
    raise ERtcOutOfBounds.Create('AddHostToChannel: HostGroupID out of range');
  AddUserToChannel(UserID,Channel,HostGroupID,True,NotifyHost,0,False,NotifyListeners);
  end;

procedure TRtcAbsGateway.AddListenerToChannel(UserID: TGateUID; const Channel: RtcString; ListenerGroupID: TGateUID; NotifyHosts, NotifyListener: boolean);
  begin
  if ListenerGroupID>MaxGID then
    raise ERtcOutOfBounds.Create('AddListenerToChannel: ListenerGroupID out of range');
  AddUserToChannel(UserID,Channel,0,False,NotifyHosts,ListenerGroupID,True,NotifyListener);
  end;

procedure TRtcAbsGateway.RemoveUserFromChannel(UserID: TGateUID; const Channel: RtcString; NotifyHosts, NotifyListeners: boolean; RemoveAsHost:boolean=True; RemoveAsListener:boolean=True);
  var
    obj:TObject;
    chan:tBinList absolute obj;
    oldGID,uid,gid:TGateUID;
  begin
  if UserID=0 then
    begin
    if LOG_GATEWAY_STATUS then
      Log('TRtcAbsGateway.RemoveUserFromChannel - NO UID',GATE_LOG+'.HINT');
    Exit;
    end;

  FCStr.Acquire;
  try
    if RemoveAsHost then
      begin
      // Find Streamer channel
      obj:=FzStreamers.search(Channel);
      if assigned(obj) then // Streamer channel found
        begin
        // Find User in Streamer channel
        oldGID:=chan.search(UserID);
        if oldGID>0 then // User found
          begin
          // Remove user from Streamer channel
          chan.remove(UserID);
          if chan.Count=0 then // Last Streamer?
            begin
            // Release Streamer channel ...
            if FzStreamers.search(Channel)=chan then
              begin
              FzStreamers.remove(Channel);
              RtcFreeAndNil(chan);
              end;
            end;

          // Find Listener channel
          obj:=FzListeners.search(Channel);
          if Assigned(obj) then
            begin
            // Enumarate through all Listeners on this Channel
            uid:=chan.search_min(gid);
            while (uid>0) and (gid>0) do
              begin
              if UserID<>uid then
                begin
                FCStr.Release;
                try
                  if oldGID>1 then
                    RemoveUserFromGroup(UserID,oldGID-1,uid,NotifyListeners,NotifyHosts);
                  if gid>1 then
                    RemoveUserFromGroup(uid,gid-1,UserID,NotifyHosts,NotifyListeners);
                finally
                  FCStr.Acquire;
                  end;
                end;
              uid:=chan.search_g(uid,gid);
              end;
            end;
          end;
        end;
      end;

    if RemoveAsListener then
      begin
      // Find Listener channel
      obj:=FzListeners.search(Channel);
      if assigned(obj) then // Listener channel found
        begin
        // Find User in Listener channel
        oldGID:=chan.search(UserID);
        if oldGID>0 then // User found
          begin
          // Remove user from Listener channel
          chan.remove(UserID);
          if chan.Count=0 then // Last Listener?
            begin
            // Release Listener channel ...
            if FzListeners.search(Channel)=chan then
              begin
              FzListeners.remove(Channel);
              RtcFreeAndNil(chan);
              end;
            end;

          // Find Streamer channel
          obj:=FzStreamers.search(Channel);
          if Assigned(obj) then
            begin
            // Enumarate through all Streamers on this Channel
            uid:=chan.search_min(gid);
            while (uid>0) and (gid>0) do
              begin
              if UserID<>uid then
                begin
                if oldGID>1 then
                  RemoveUserFromGroup(UserID,oldGID-1,uid,NotifyHosts,NotifyListeners);
                if gid>1 then
                  RemoveUserFromGroup(uid,gid-1,UserID,NotifyListeners,NotifyHosts);
                end;
              uid:=chan.search_g(uid,gid);
              end;
            end;
          end;
        end;
      end;
  finally
    FCStr.Release;
    end;
  end;

procedure TRtcAbsGateway.DoCloseAllUserChannels;
  var
    obj:TObject;
    Channel:RtcString;
  begin
  FCStr.Acquire;
  try
    Channel:=FzStreamers.search_min(obj);
    while (Channel<>'') and assigned(obj) do
      begin
      FzStreamers.remove(Channel);
      if assigned(obj) then
        RtcFreeAndNil(obj);
      Channel:=FzStreamers.search_min(obj);
      end;

    Channel:=FzListeners.search_min(obj);
    while (Channel<>'') and assigned(obj) do
      begin
      FzListeners.remove(Channel);
      if assigned(obj) then
        RtcFreeAndNil(obj);
      Channel:=FzListeners.search_min(obj);
      end;

    Channel:=FzPublic.search_min(obj);
    while (Channel<>'') and assigned(obj) do
      begin
      FzPublic.remove(Channel);
      if assigned(obj) then
        RtcFreeAndNil(obj);
      Channel:=FzPublic.search_min(obj);
      end;

    Channel:=FzHosts.search_min(obj);
    while (Channel<>'') and assigned(obj) do
      begin
      FzHosts.remove(Channel);
      if assigned(obj) then
        RtcFreeAndNil(obj);
      Channel:=FzHosts.search_min(obj);
      end;

    Channel:=FzControls.search_min(obj);
    while (Channel<>'') and assigned(obj) do
      begin
      FzControls.remove(Channel);
      if assigned(obj) then
        RtcFreeAndNil(obj);
      Channel:=FzControls.search_min(obj);
      end;
  finally
    FCStr.Release;
    end;
  end;

procedure TRtcAbsGateway.SetLink(const Value: TRtcDataServerLink);
  begin
  if FLink<>Value then
    begin
    FLink := Value;

    PingProvider.Link:=FLink;
    InputStream.Link:=FLink;
    OutputStream.Link:=FLink;
    LoginProvider.Link:=FLink;

    if assigned(FLink) then
      FServer:=nil;
    end;
  end;

procedure TRtcGateOneUser.oAfterWrite(const Len:int64);
  begin
  if oWriteLimit>0 then
    if oWriteCount=0 then
      begin
      oWriteCount:=Len;
      oWriteTime:=GetAppRunTime;
      end
    else
      Inc(oWriteCount,Len);
  end;

procedure TRtcGateOneUser.iAfterRead(const Len:int64);
  begin
  if iReadLimit>0 then
    if iReadCount=0 then
      begin
      iReadCount:=Len;
      iReadTime:=GetAppRunTime;
      end
    else
      Inc(iReadCount,Len);
  end;

function TRtcGateOneUser.oBeforeWrite:boolean;
  begin
  Result:=True;
  if oWriteLimit>0 then
    if oWriteCount>0 then
      begin
      oWriteDelta:=GetAppRunTime-oWriteTime;
      if oWriteDelta>0 then
        begin
        oWriteSpeed:=oWriteDelta*oWriteLimit shr 3; // allowed Bytes OUT - until now
        if oWriteCount>oWriteSpeed then // sent more than allowed
          begin
          oWriteSpeed:=((oWriteCount-oWriteSpeed) shl 3) div oWriteLimit; // expected time to send extra bytes
          if oWriteSpeed>=2200 then // more than 2.2 seconds overtime
            begin
            // Pause sending for at least 1 second
            Result:=False;
            oWriteCount:=oWriteMax;
            Dec(oWriteSpeed,1200);
            Inc(oWriteTime,oWriteDelta);
            Inc(oWriteTime,oWriteSpeed);
            oContinue:=oWriteTime;
            {$IFDEF RTCGATE_DEBUG}xLog('oBeforeWrite: oPaused('+IntToStr(oWriteSpeed)+'):=TRUE'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
            oPaused:=True;
            end;
          end
        else
          oWriteCount:=0;
        end
      else if oWriteCount>oWriteMax then
        begin
        oWriteSpeed:=(oWriteCount shl 3) div oWriteLimit;
        if oWriteSpeed>=2200 then // more than 2.2 seconds overtime
          begin
          // Pause sending for at least 1 second
          Result:=False;
          oWriteCount:=oWriteMax;
          Dec(oWriteSpeed,1200);
          Inc(oWriteTime,oWriteSpeed);
          oContinue:=oWriteTime;
          {$IFDEF RTCGATE_DEBUG}xLog('oBeforeWrite: oPaused('+IntToStr(oWriteSpeed)+'):=TRUE'+oFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
          oPaused:=True;
          end;
        end;
      end;
  end;

function TRtcGateOneUser.iBeforeRead:boolean;
  begin
  Result:=True;
  if iReadLimit>0 then
    if iReadCount>0 then
      begin
      iReadDelta:=GetAppRunTime-iReadTime;
      if iReadDelta>0 then
        begin
        iReadSpeed:=iReadDelta*iReadLimit shr 3;
        if iReadCount>iReadSpeed then // Read more than allowed?
          begin
          iReadSpeed:=((iReadCount-iReadSpeed) shl 3) div iReadLimit;
          if iReadSpeed>=2200 then // more than 2.2 seconds overtime
            begin
            // Pause reading for at least 1 second
            Result:=False;
            iReadCount:=iReadMax;
            Dec(iReadSpeed,1200);
            Inc(iReadTime,iReadDelta);
            Inc(iReadTime,iReadSpeed);
            iContinue:=iReadTime;
            {$IFDEF RTCGATE_DEBUG}xLog('iBeforeRead: iWaiting:=TRUE, iPaused('+IntToStr(iReadSpeed)+'):=TRUE'+
                                       iFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
            iWaiting:=True;
            iPaused:=True;
            end;
          end
        else
          iReadCount:=0;
        end
      else if iReadCount>iReadMax then
        begin
        iReadSpeed:=(iReadCount shl 3) div iReadLimit;
        if iReadSpeed>=2200 then // more than 2.2 seconds overtime
          begin
          // Pause reading for at least 1 second
          Result:=False;
          iReadCount:=iReadMax;
          Dec(iReadSpeed,1200);
          Inc(iReadTime,iReadDelta);
          Inc(iReadTime,iReadSpeed);
          iContinue:=iReadTime;
          {$IFDEF RTCGATE_DEBUG}xLog('iBeforeRead: iWaiting:=TRUE, iPaused('+IntToStr(iReadSpeed)+'):=TRUE'+
                                     iFrom,IntToStr(xUID shr UserIDShift));{$ENDIF}
          iWaiting:=True;
          iPaused:=True;
          end;
        end;
      end;
  end;

function TRtcAbsGateway.FindUser4Input(i:byte; Conn:RtcIntPtr; var obj:TObject; level:shortint=0):boolean;
  var
    Sender:TRtcConnection absolute Conn;
    oUser:TRtcGateOneUser absolute obj;
    isActive,isRemoved:boolean;
    UID:TGateUID;
  begin
  Result:=False; obj:=nil;
  if Conn=0 then
    if (level>=1) {$IFDEF RTCGATE_FULLDEBUG} or (level=0) {$ENDIF}then
      RaiseFatalError('No CONN @'+LID2Text(i))
    else
      Exit;

  UID:=crcGetUserID(i,Sender,True);
  if UID=0 then
    if (level>=1) {$IFDEF RTCGATE_FULLDEBUG} or (level=0) {$ENDIF}then
      RaiseFatalError('No UID @'+LID2Text(i)+FromAddr(Sender))
    else
      Exit;

  isActive:=False;
  isRemoved:=False;
  FCUsr.Acquire;
  try
    obj:=FxUsers.search(UID);
    if assigned(obj) then
      isActive:=True
    else if assigned(FxRemoval.search(UID)) then
      isRemoved:=True;
  finally
    FCUsr.Release;
    end;

  if isActive then
    begin
    oUser.{$IFDEF RTCGATE_DEBUG}LockInput(i);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      if oUser.oiRemoved then
        begin
        if (level>=1) {$IFDEF RTCGATE_FULLDEBUG} or (level=0) {$ENDIF}then
          RaiseFatalError('Logged OUT @'+LID2Text(i)+' #'+Int2Str(UID shr UserIDShift)+
                          FromAddr(Sender)+'/'+oUser.iFrom);
        end
      else if oUser.xOwnerIP<>GetUserAddr(Sender) then
        begin
        if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
          RaiseFatalError('Bad IN IP @'+LID2Text(i)+'! "'+
                          GetUserAddr(Sender)+'"<>"'+oUser.xOwnerIP+'" for #'+
                          Int2Str(UID shr UserIDShift)+FromAddr(Sender)+'/'+oUser.iFrom);
        end
      else
        begin
        if (level>=3) or
          ((level>=2) and ((oUser.iInput=0) or (oUser.iLeftToRead=0))) then
          begin
          {$IFDEF RTCGATE_DEBUG}
            xLog('Set@'+LID2Text(i)+'! iInput{'+Int2Str(oUser.iInput)+'}:={'+
                 Int2Str(Conn)+'}'+FromAddr(Sender)+'/'+oUser.iFrom,
                 IntToStr(oUser.xUID shr UserIDShift));
          {$ENDIF}
          oUser.iInput:=Conn;
          oUser.iFrom:=FromAddr(Sender);
          oUser.iInThr:=Sender.Thread;
          oUser.iLeftToRead:=0;
          Result:=True;
          end
        else
          begin
          Result:=oUser.iInput=Conn;
          if not Result then
            if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
              RaiseError('Bad IN CONN @'+LID2Text(i)+' for #'+Int2Str(UID shr UserIDShift)+
                         '{'+Int2Str(oUser.iInput)+'}!={'+Int2Str(Conn)+'}'+
                         FromAddr(Sender)+'/'+oUser.iFrom);
          end;
        end;
    finally
      oUser.{$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      if not Result then obj:=nil;
      end;
    end
  else if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
    if isRemoved then
      RaiseFatalError('Logged OUT @'+LID2Text(i)+' #'+
                      Int2Str(UID shr UserIDShift)+FromAddr(Sender))
    else
      RaiseFatalError('NOT Found @'+LID2Text(i)+' #'+
                      Int2Str(UID shr UserIDShift)+FromAddr(Sender));
  end;

function TRtcAbsGateway.FindUser4Output(i:byte; Conn:RtcIntPtr; var obj:TObject; level:shortint=0):boolean;
  var
    Sender:TRtcConnection absolute Conn;
    oUser:TRtcGateOneUser absolute obj;
    isActive,isRemoved:boolean;
    UID:TGateUID;
  begin
  Result:=False; obj:=nil;
  if Conn=0 then
    if (level>=1) {$IFDEF RTCGATE_FULLDEBUG} or (level=0) {$ENDIF}then
      RaiseFatalError('No CONN @'+LID2Text(i))
    else
      Exit;

  UID:=crcGetUserID(i,Sender,True);
  if UID=0 then
    if (level>=1) {$IFDEF RTCGATE_FULLDEBUG} or (level=0) {$ENDIF}then
      RaiseFatalError('No UID @'+LID2Text(i)+FromAddr(Sender))
    else
      Exit;

  isActive:=False;
  isRemoved:=False;
  FCUsr.Acquire;
  try
    obj:=FxUsers.search(UID);
    if assigned(obj) then
      isActive:=True
    else if assigned(FxRemoval.search(UID)) then
      isRemoved:=True;
  finally
    FCUsr.Release;
    end;

  if isActive then
    begin
    oUser.{$IFDEF RTCGATE_DEBUG}LockOutput(i);{$ELSE}outCS.Acquire;{$ENDIF}
    try
      if oUser.oiRemoved then
        begin
        if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
          RaiseFatalError('Logged OUT @'+LID2Text(i)+' #'+
                          Int2Str(UID shr UserIDShift)+
                          FromAddr(Sender)+'/'+oUser.oFrom);
        end
      else if oUser.xOwnerIP<>GetUserAddr(Sender) then
        begin
        if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
          RaiseFatalError('Bad OUT IP @'+LID2Text(i)+'! "'+
                          GetUserAddr(Sender)+'"<>"'+oUser.xOwnerIP+
                          '" for #'+Int2Str(UID shr UserIDShift)+
                          FromAddr(Sender)+'/'+oUser.oFrom);
        end
      else
        begin
        if (level>=3) or
          ((level>=2) and ((oUser.oOutput=0) or (oUser.oLeftToSend=0))) then
          begin
          {$IFDEF RTCGATE_FULLDEBUG}
            xLog('Set@'+LID2Text(i)+'!oOutput{'+
                 Int2Str(oUser.oOutput)+'}:={'+
                 Int2Str(Conn)+'}'+FromAddr(Sender)+'/'+oUser.oFrom,
                 IntToStr(oUser.xUID shr UserIDShift));{$ENDIF}
          oUser.oOutput:=Conn;
          oUser.oFrom:=FromAddr(Sender);
          oUser.oOutThr:=Sender.Thread;
          oUser.oLeftToSend:=0;
          Result:=True;
          end
        else
          begin
          Result:=oUser.oOutput=Conn;
          if not Result then
            if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
              RaiseError('Bad OUT CONN @'+LID2Text(i)+' for #'+Int2Str(UID shr UserIDShift)+
                          '{'+Int2Str(oUser.oOutput)+'}!={'+Int2Str(Conn)+'}'+
                          FromAddr(Sender)+'/'+oUser.oFrom);
          end;
        end;
    finally
      oUser.{$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
      if not Result then obj:=nil;
      end;
    end
  else if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
    if isRemoved then
      RaiseFatalError('Logged OUT @'+LID2Text(i)+' #'+
                      Int2Str(UID shr UserIDShift)+FromAddr(Sender))
    else
      RaiseFatalError('NOT Found @'+LID2Text(i)+' #'+
                      Int2Str(UID shr UserIDShift)+FromAddr(Sender));
  end;

(*
function TRtcAbsGateway.FindUser4Input(i:byte; Conn:RtcIntPtr; UID:TGateUID; var obj:TObject; level:shortint=0):boolean;
  var
    oUser:TRtcGateOneUser absolute obj;
    isActive,isRemoved:boolean;
  begin
  Result:=False; obj:=nil;
  if Conn=0 then
    if (level>=1) {$IFDEF RTCGATE_FULLDEBUG} or (level=0) {$ENDIF}then
      RaiseFatalError('No CONN @'+LID2Text(i))
    else
      Exit;
  if UID=0 then
    if (level>=1) {$IFDEF RTCGATE_FULLDEBUG} or (level=0) {$ENDIF}then
      RaiseFatalError('No UID @'+LID2Text(i))
    else
      Exit;

  isActive:=False;
  isRemoved:=False;
  FCUsr.Acquire;
  try
    obj:=FxUsers.search(UID);
    if assigned(obj) then
      isActive:=True
    else if assigned(FxRemoval.search(UID)) then
      isRemoved:=True;
  finally
    FCUsr.Release;
    end;

  if isActive then
    begin
    oUser.{$IFDEF RTCGATE_DEBUG}LockInput(i);{$ELSE}inCS.Acquire;{$ENDIF}
    try
      if oUser.oiRemoved then
        begin
        if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
          RaiseFatalError('Logged OUT @'+LID2Text(i)+' #'+
                          Int2Str(UID shr UserIDShift)+'/'+oUser.iFrom);
        end
      else
        begin
        Result:=oUser.iInput=Conn;
        if not Result then
          if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
            RaiseError('Bad IN CONN @'+LID2Text(i)+' for #'+Int2Str(UID shr UserIDShift)+
                       '{'+Int2Str(oUser.iInput)+'}!={'+Int2Str(Conn)+'}/'+oUser.iFrom);
        end;
    finally
      oUser.{$IFDEF RTCGATE_DEBUG}UnLockInput;{$ELSE}inCS.Release;{$ENDIF}
      if not Result then obj:=nil;
      end;
    end
  else if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
    if isRemoved then
      RaiseFatalError('Logged OUT @'+LID2Text(i)+' #'+Int2Str(UID shr UserIDShift))
    else
      RaiseFatalError('NOT Found @'+LID2Text(i)+' #'+Int2Str(UID shr UserIDShift));
  end;

function TRtcAbsGateway.FindUser4Output(i:byte; Conn:RtcIntPtr; UID:TGateUID; var obj:TObject; level:shortint=0):boolean;
  var
    oUser:TRtcGateOneUser absolute obj;
    isActive,isRemoved:boolean;
  begin
  Result:=False; obj:=nil;
  if Conn=0 then
    if (level>=1) {$IFDEF RTCGATE_FULLDEBUG} or (level=0) {$ENDIF}then
      RaiseFatalError('No CONN @'+LID2Text(i))
    else
      Exit;
  if UID=0 then
    if (level>=1) {$IFDEF RTCGATE_FULLDEBUG} or (level=0) {$ENDIF}then
      RaiseFatalError('No UID @'+LID2Text(i))
    else
      Exit;

  isActive:=False;
  isRemoved:=False;
  FCUsr.Acquire;
  try
    obj:=FxUsers.search(UID);
    if assigned(obj) then
      isActive:=True
    else if assigned(FxRemoval.search(UID)) then
      isRemoved:=True;
  finally
    FCUsr.Release;
    end;

  if isActive then
    begin
    oUser.{$IFDEF RTCGATE_DEBUG}LockOutput(i);{$ELSE}outCS.Acquire;{$ENDIF}
    try
      if oUser.oiRemoved then
        begin
        if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
          RaiseFatalError('Logged OUT @'+LID2Text(i)+' #'+
                          Int2Str(UID shr UserIDShift)+'/'+oUser.oFrom);
        end
      else
        begin
        Result:=oUser.oOutput=Conn;
        if not Result then
          if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
            RaiseError('Bad OUT CONN @'+LID2Text(i)+' for #'+Int2Str(UID shr UserIDShift)+
                       '{'+Int2Str(oUser.oOutput)+'}!={'+Int2Str(Conn)+'}/'+oUser.oFrom);
        end;
    finally
      oUser.{$IFDEF RTCGATE_DEBUG}UnLockOutput;{$ELSE}outCS.Release;{$ENDIF}
      if not Result then obj:=nil;
      end;
    end
  else if (level>=1) {$IFDEF RTCGATE_FULLDEBUG}or (level=0){$ENDIF} then
    if isRemoved then
      RaiseFatalError('Logged OUT @'+LID2Text(i)+' #'+Int2Str(UID shr UserIDShift))
    else
      RaiseFatalError('NOT Found @'+LID2Text(i)+' #'+Int2Str(UID shr UserIDShift));
  end;
*)

function TRtcAbsGateway.FindGateUser(uid: TGateUID): TObject;
  begin
  FCUsr.Acquire;
  try
    Result:=FxUsers.search(uid);
  finally
    FCUsr.Release;
    end;
  end;

{ TRtcHttpGateway }

constructor TRtcHttpGateway.Create(AOwner: TComponent);
  begin
  inherited;
  SetServer(TRtcHttpServer.Create(self));
  with FServer as TRtcHttpServer do
    begin
    MultiThreaded:=False;
    RestartOn.ListenLost:=True;
    RestartOn.ListenError:=True;
    FixupRequest.RemovePrefix:=True;
    MaxHeaderSize:=2048;
    TimeoutsOfAPI.ReceiveTimeout:=240;
    TimeoutsOfAPI.SendTimeout:=240;
    OnInvalidRequest:=MyInvalidRequest;
    OnRequestNotAccepted:=MyRequestNotAccepted;
    OnException:=MyException;
    end;
  end;

destructor TRtcHttpGateway.Destroy;
  begin
  FNotSilent:=False;
  FAmSilent:=True;

  PingProviderListenStop(FServer);

  RtcFreeAndNil(FServer);
  inherited;
  end;

function TRtcHttpGateway.GetActive: boolean;
  begin
  if assigned(FServer) then
    Result:=FServer.isListening
  else
    Result:=False;
  end;

function TRtcHttpGateway.GetMultiThreaded: boolean;
  begin
  if assigned(FServer) then
    Result:=FServer.MultiThreaded
  else
    Result:=False;
  end;

function TRtcHttpGateway.GetCryptPlugin: TRtcCryptPlugin;
  begin
  if assigned(FServer) then
    Result:=TRtcHttpServer(FServer).CryptPlugin
  else
    Result:=nil;
  end;

function TRtcHttpGateway.GetOnClose: TRtcNotifyEvent;
  begin
  if assigned(FServer) then
    Result:=FServer.OnListenStop
  else
    Result:=nil;
  end;

function TRtcHttpGateway.GetOnListenError: TRtcErrorEvent;
  begin
  if assigned(FServer) then
    Result:=FServer.OnListenError
  else
    Result:=nil;
  end;

function TRtcHttpGateway.GetOnListenLost: TRtcNotifyEvent;
  begin
  if assigned(FServer) then
    Result:=FServer.OnListenLost
  else
    Result:=nil;
  end;

function TRtcHttpGateway.GetOnOpen: TRtcNotifyEvent;
  begin
  if assigned(FServer) then
    Result:=FServer.OnListenStart
  else
    Result:=nil;
  end;

function TRtcHttpGateway.GetOnRestart: TRtcNotifyEvent;
  begin
  if assigned(FServer) then
    Result:=FServer.OnRestart
  else
    Result:=nil;
  end;

function TRtcHttpGateway.GetServer: TRtcHttpServer;
  begin
  if assigned(FServer) then
    Result:=TRtcHttpServer(FServer)
  else
    Result:=nil;
  end;

function TRtcHttpGateway.GetServerAddr: RtcString;
  begin
  if assigned(FServer) then
    Result:=FServer.ServerAddr
  else
    Result:='';
  end;

function TRtcHttpGateway.GetServerPort: RtcString;
  begin
  if assigned(FServer) then
    Result:=FServer.ServerPort
  else
    Result:='';
  end;

function TRtcHttpGateway.GetServerIPV: RtcIPV;
  begin
  if assigned(FServer) then
    Result:=FServer.ServerIPV
  else
    Result:=rtc_IPVDefault;
  end;

procedure TRtcHttpGateway.MyException(Sender: TRtcConnection; E: Exception);
  begin
  if LOG_GATEWAY_ERRORS then
    Log(Sender.PeerAddr+':'+Sender.PeerPort+#9' ERROR '+
        RtcString(E.ClassName)+':'+Utf8Encode(RtcWideString(E.Message)),GATE_LOG+'.ERROR');
  end;

procedure TRtcHttpGateway.MyInvalidRequest(Sender: TRtcConnection);
  begin
  if LOG_GATEWAY_STATUS then
    Log(Sender.PeerAddr+':'+Sender.PeerPort+#9' Invalid: '+Sender.Request.Method+' '+Sender.Request.URI,GATE_LOG+'.HTTP');
  if Sender.Request.Complete then
    begin
    Sender.Response.Status(404,'*');
    Sender.Write;
    end
  else
    Sender.Disconnect;
  end;

procedure TRtcHttpGateway.MyRequestNotAccepted(Sender: TRtcConnection);
  begin
  if LOG_GATEWAY_STATUS then
    Log(Sender.PeerAddr+':'+Sender.PeerPort+#9' NOT Accepted: '+Sender.Request.Method+' '+Sender.Request.URI+' ('+Sender.Request['CONTENT-LENGTH']+' bytes)',GATE_LOG+'.HTTP');
  if Sender.Request.Complete then
    begin
    Sender.Response.Status(404,'*');
    Sender.Write;
    end
  else
    Sender.Disconnect;
  end;

procedure TRtcHttpGateway.SetActive(const Value: boolean);
  begin
  if assigned(FServer) then
    if Value<>FServer.isListening then
      if Value then
        FServer.Listen
      else
        FServer.StopListenNow;
  end;

procedure TRtcHttpGateway.SetMultiThreaded(const Value: boolean);
  begin
  if assigned(FServer) then
    FServer.MultiThreaded:=Value;
  end;

procedure TRtcHttpGateway.SetCryptPlugin(const Value: TRtcCryptPlugin);
  begin
  if assigned(FServer) then
    TRtcHttpServer(FServer).CryptPlugin:=Value;
  end;

procedure TRtcHttpGateway.SetOnClose(const Value: TRtcNotifyEvent);
  begin
  if assigned(FServer) then
    FServer.OnListenStop:=Value;
  end;

procedure TRtcHttpGateway.SetOnListenError(const Value: TRtcErrorEvent);
  begin
  if assigned(FServer) then
    FServer.OnListenError:=Value;
  end;

procedure TRtcHttpGateway.SetOnListenLost(const Value: TRtcNotifyEvent);
  begin
  if assigned(FServer) then
    FServer.OnListenLost:=Value;
  end;

procedure TRtcHttpGateway.SetOnOpen(const Value: TRtcNotifyEvent);
  begin
  if assigned(FServer) then
    FServer.OnListenStart:=Value;
  end;

procedure TRtcHttpGateway.SetOnRestart(const Value: TRtcNotifyEvent);
  begin
  if assigned(FServer) then
    FServer.OnRestart:=Value;
  end;

procedure TRtcHttpGateway.SetServerAddr(const Value: RtcString);
  begin
  if assigned(FServer) then
    FServer.ServerAddr:=Value;
  end;

procedure TRtcHttpGateway.SetServerPort(const Value: RtcString);
  begin
  if assigned(FServer) then
    FServer.ServerPort:=Value;
  end;

procedure TRtcHttpGateway.SetServerIPV(const Value: RtcIPV);
  begin
  if assigned(FServer) then
    FServer.ServerIPV:=Value;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; {$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcGateSrv Finalizing ...','DEBUG');{$ENDIF}
CloseTimerPool;
{$IFDEF RTC_DEBUG} Log('rtcGateSrv Finalized.','DEBUG');{$ENDIF}
end.
