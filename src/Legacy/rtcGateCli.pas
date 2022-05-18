{
  @html(<b>)
  Gate Client (Legacy)
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This is a LEGACY unit, which means that continued use of this unit is discouraged.
  If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
  released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.
  
  This unit implements Client-side RTC Gateway components.
}
unit rtcGateCli;

{$INCLUDE rtcDefs.inc}

interface

// Disable PING when debugging, to avoid timeout-related disconnects
{.$DEFINE DISABLE_PING}

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSrcList,
  rtcSystem,
  rtcThrPool,
  rtcTimer,
  rtcLog,

  rtcCrypt,
  rtcInfo,
  rtcThrJobs,

  rtcConn,
  rtcDataCli,
  rtcHttpCli,
  rtcPlugins,

  rtcGateConst;

const
  // highest "custom" ID allowed
  cid_HighestCustom = 65500;

  // Internal Call IDs
  cid_VerifyAccount       = 65535;
  cid_AccountVerification = 65534;
  cid_AccountTransfer     = 65533;
  cid_AccountReceived     = 65532;

var
  // Maximum allowed data size for internal calls
  MAX_INTERNAL_CALL_SIZE : Integer = 128000;

type
  ERtcGateClient = class(Exception);

  TRtcGateClientInState = (ins_Closed, ins_Connecting, ins_Prepare,
                           ins_Start, ins_Idle, ins_Recv, ins_Done);

  TRtcGateClientOutState = (outs_Closed, outs_Connecting, outs_Prepare,
                            outs_Start, outs_Idle, outs_Send, outs_Done);

  TRtcGateClientCommand = (// Error
                           gc_Error,
                           // received a complete packet
                           gc_SendAll,
                           // received first chunk of a new packet
                           gc_SendFirst,
                           // received next chunk of the new packet
                           gc_SendMore,
                           // received last chunk of the new packet
                           gc_SendLast,
                           // User "UserID" Joind our Group "GroupID"
                           gc_UserJoined,
                           // User "UserID" left our Group "GroupID"
                           gc_UserLeft,
                           // We have joined group "GroupID" owned by user "UserID"
                           gc_JoinedUser,
                           // We have left group "GroupID" owned by user "UserID"
                           gc_LeftUser,
                           // User "UserID" is now Online.
                           // Received after sending a PING if the user is connected to the Gateway.
                           gc_UserOnline,
                           // User "UserID" is now offline.
                           // Received after sending a PING, if the user is NOT connected to the Gateway.
                           gc_UserOffline,
                           // User "UserID" added us as a Friend
                           gc_BeFriend,
                           // User "UserID" removed us as a Friend
                           gc_UnFriend,
                           // User "UserID" was added to our Friends list
                           gc_FriendAdd,
                           // User "UserID" was removed from our Friends list
                           gc_FriendRemove,
                           // User "UserID" is logged in and Subscribed to a Channel using "GroupID"
                           gc_SubscribeLogIn,
                           // User "UserID" logged out or UnSubscribed from a Channel using "GroupID"
                           gc_SubscribeLogOut,
                           // User logged in with the Account "AccountID".
                           // This notification is received for all "AccountManager" accounts
                           gc_AccountLogIn,
                           // Account logged out with the Account "AccountID"
                           // This notification is received for all "AccountManager" accounts
                           gc_AccountLogOut,
                           // Account "AccountID" ownership has been verified
                           // This notification is received for Private and Linked "AccountManager" accounts
                           gc_AccountVerified,
                           // Account "AccountID" ownership verification has failed (wrong Account).
                           // This notification is received for Private and Linked "AccountManager" accounts
                           gc_AccountBadVerify,
                           // Account "AccountID" was just received from the User "UserID"
                           gc_AccountReceived,
                           // Account "AccountID" successfully transferred to the User "UserID"
                           gc_AccountTransferred,
                           // Account Data received from User "UserID" was bad, probably because
                           // the currently valid "UserAuth" does not match the "UsersAuthCode"
                           // used by the Sender to encrypt the Account Data before sending
                           gc_AccountBadReceive,
                           // Account transfer to the User "UserID" was bad (wrong AuthCode?)
                           gc_AccountBadTransfer);

  // @exclude
  RtcGateClientData=record
    Header: boolean;
    Footer: boolean;
    ToBuffer: boolean;
    Position: integer;
    Location:integer;
    Length:integer;
    ErrCode: Cardinal;
    MyUID: TGateUID;
    UserID: TGateUID;
    GroupID: TGateUID;
    ToGroupID: TGateUID;
    Command: TRtcGateClientCommand;
    CallID: word;
    end;

  { @abstract(RTC "Gate" Object) }
  TRtcGateObject = class(TRtcDataObject);

  { @Abstract(Data received info)
    This class is used to make the information received from the Gateway
    available to the Client inside OnDataReceived and OnInfoReceived events. }
  TRtcGateClientData = class(TRtcGateObject)
  protected
    // @exclude
    F:RtcGateClientData;
    // @exclude
    FBytes:RtcByteArray;
    // @exclude
    FContent: RtcByteArray;
    // @exclude
    FGateAddr: RtcString;
    // @exclude
    FGatePort: RtcString;
    // @exclude
    FAccountID: RtcString;
    // @exclude
    FUserInfo: RtcString;

    // @exclude
    function GetUserAddr: RtcString;
    // @exclude
    function GetMyAddr: RtcString;

    // @exclude
    procedure CopyFrom(const FromData:TRtcGateClientData); overload;

  public
    constructor Create;
    destructor Destroy; override;

    // Sender's User Addres (UserID @ GateAddr : GatePort)
    property UserAddr:RtcString read GetUserAddr;

    { My User Address on the Gateway (MyUID @ GateAddr : GatePort),
      available if we are logged on to the Gateway }
    property MyAddr:RtcString read GetMyAddr;

    { Sender's Gateway Addres }
    property GateAddr:RtcString read FGateAddr;

    { Sender's Gateway Port }
    property GatePort:RtcString read FGatePort;

    { Sender's User Info.
      It will be populated in the OnInfoReceived events if the
      Sender had its "GateUserInfo" parameter set and the "Command"
      is "gc_SubscribeLogIn" or "gc_AccountLogIn" (user online notification). }
    property UserInfo:RtcString read FUserInfo;

    { Users Account "ID".
      It will be populated in the OnInfoReceived events when the event is triggered
      by the "AccountManager" component ( "Command" = gc_Account... ) }
    property AccountID:RtcString read FAccountID;

    { Sender's User ID }
    property UserID:TGateUID read F.UserID;

    { My UserID on the Gateway,
      available if we are logged on to the Gateway }
    property MyUID:TGateUID read F.MyUID;

    // Received Command
    property Command:TRtcGateClientCommand read F.Command;

    // Error Code if Command=gc_Error
    property ErrCode:Cardinal read F.ErrCode;

    { Sender's Group ID.
      It will be populated (>0) if data was sent to a User Group created by the Sender
      (the User Group could have multiple recipients) and we are part of that User Group. }
    property GroupID:TGateUID read F.GroupID;

    { "To" Group ID.
      It will be populated (>0) if data was sent to our User ID (we are the ONLY data receiver)
      and the Sender has specified a GroupID in the "SendBytes" method. }
    property ToGroupID:TGateUID read F.ToGroupID;

    // "Content" has the first bytes of the package (Header) ?
    property Header:boolean read F.Header;

    // "Content" has the last bytes of the package (Footer) ?
    property Footer:boolean read F.Footer;

    { This method checks if "Header=True" and "Footer=True".
      If it returs TRUE, we have the whole data "Content". }
    function HeaderAndFooter:boolean;

    { "CallID" -> available if Header=True or ToBuffer=True (otherwise, 0).
      When checking the data received, make sure that "CallID", "GroupID" and
      "ToGroupID" have the values you are expecting. The same "CallID" can be used
      for a number of different purpuses, depending on the "GroupID" and/or "ToGroupID". }
    property CallID:word read F.CallID;

    // "Position" of the currently received chunk inside the Buffer -> available if ToBuffer=True.
    property Position:integer read F.Position;

    { You should set "ToBuffer:=True" either from the "OnDataFilter", 
      "OnDataReceived" or "OnDataReceivedGUI" event if content should be 
      buffered, so you can access the complete package using the "Content" 
      property when "Header=True" and "Footer=True" (Command is "gc_SendAll" or "gc_SendLast").

      Changing the "ToBuffer" property from events triggered on the 
      "BackThread:TRtcGCThread" component has no effect, because 
      these events are called asynchronously and will
      ONLY have a temporary COPY of the "Data" object! }
    property ToBuffer:boolean read F.ToBuffer write F.ToBuffer;

    { If Header=True & Footer=True then "Content" has the complete package.
      Otherwise, "Content" has ONLY the currently received package chunk. }
    property Content:RtcByteArray read FContent;
    end;

  // @exclude
  RtcGateClientStateInfo=record
    LoggedIn: boolean;
    InputReady: boolean;
    OutputReady: boolean;
    OutputResetTime: TAppRunTime;
    InputResetAt: TAppRunTime;
    LastInput: TAppRunTime;
    LastOutput: TAppRunTime;
    LastPing: TAppRunTime;
    InputResetTime: TAppRunTime;
    LastContact: TAppRunTime;
    OutputResetAt: TAppRunTime;
    TotalReceived: int64;
    TotalSent: int64;
    PingInCnt: integer;
    PingOutCnt: integer;
    LeftToSend: Cardinal;
    ReadyToSend: boolean;
    InputState: TRtcGateClientInState;
    OutputState: TRtcGateClientOutState;
    MyUID: TGateUID;
    end;

  { @Abstract(RTC Gate Client State information) }
  TRtcGateClientStateInfo=class(TRtcGateObject)
  private
    // @exclude
    F:RtcGateClientStateInfo;
    // @exclude
    FGateAddr: RtcString;
    // @exclude
    FGatePort: RtcString;
    // @exclude
    FLastError: RtcString;

    procedure CopyFrom(const FromState:TRtcGateClientStateInfo); overload;

    function GetMyAddr: RtcString;

  public
    constructor Create;
    destructor Destroy; override;

    // My User Address (MyUID @ GateAddr : GatePort), available after login
    property MyAddr:RtcString read GetMyAddr;

    { Last connection-related Error message }
    property LastError:RtcString read FLastError;

    { Sender's Gateway Addres }
    property GateAddr:RtcString read FGateAddr;

    { Sender's Gateway Port }
    property GatePort:RtcString read FGatePort;

    // My User ID returned from the Gateway after login
    property MyUID:TGateUID read F.MyUID;

    // Input stream state
    property InputState:TRtcGateClientInState read F.InputState;
    // Output stream state
    property OutputState:TRtcGateClientOutState read F.OutputState;

    // Are we logged in to the Gateway?
    property LoggedIn:boolean read F.LoggedIn;
    // Is the output stream ready?
    property OutputReady:boolean read F.OutputReady;
    // Is the input stream ready?
    property InputReady:boolean read F.InputReady;

    // Last contact time (in "GetAppRunTime" ticks)
    property LastContact:TAppRunTime read F.LastContact;
    // Last input received time (in "GetAppRunTime" ticks)
    property LastInput:TAppRunTime read F.LastInput;
    // Last output sent time (in "GetAppRunTime" ticks)
    property LastOutput:TAppRunTime read F.LastOutput;
    // Last ping sent time (in "GetAppRunTime" ticks)
    property LastPing:TAppRunTime read F.LastPing;

    // Ping IN counter (0..3)
    property PingInCnt:integer read F.PingInCnt;
    // Ping OUT counter (0..3)
    property PingOutCnt:integer read F.PingOutCnt;

    // Bytes left to send
    property LeftToSend:Cardinal read F.LeftToSend;

    // Ready to send more data (output stream ready and idle)
    property ReadyToSend:boolean read F.ReadyToSend;

    // Total bytes sent since last login
    property TotalSent:int64 read F.TotalSent;
    // Total bytes received since last login
    property TotalReceived:int64 read F.TotalReceived;

    // Number of Bytes sent with the last request, before the Output stream was reset
    property OutputResetAt:TAppRunTime read F.OutputResetAt;
    // Number of Bytes received with the last response, before the Input stream was reset
    property InputResetAt:TAppRunTime read F.InputResetAt;

    // Output stream reset Time ("GetAppRunTime" ticks since "LastOutput")
    property OutputResetTime:TAppRunTime read F.OutputResetTime;
    // Input stream reset Time ("GetAppRunTime" ticks since "LastInput")
    property InputResetTime:TAppRunTime read F.InputResetTime;
    end;

  { @Abstract(RTC User Group State Management) }
  TRtcGateUserGroupStates = class(TRtcGateObject)
  private
    FGroups:TBinList;
    FCS:TRtcCritSec;

  public
    constructor Create;
    destructor Destroy; override;

    // Set status for User "UserID" and Group "GroupID"
    procedure SetStatus(UserID,GroupID,Status:TGateUID);
    { Clear status for User "UserID" and Group "GroupID".
      This is the same as calling SetStatus with Status=0. }
    procedure ClearStatus(UserID,GroupID:TGateUID);
    { Clear status of all Users and Groups.
      Use this method from AfterLoggedIn and OnStreamReset events. }
    procedure ClearAllStates;

    { Get status for User "UserID" and Group "GroupID".
      Returns 0 if no status stored for UserID + GroupID }
    function GetStatus(UserID,GroupID:TGateUID):TGateUID;
    { Get min status value (>0) for User "UserID".
      Returns 0 if no status stored for user "UserID". }
    function GetMinStatus(UserID:TGateUID):TGateUID;
    { Get max status value (>0) for User "UserID".
      Returns 0 if no status stored for user "UserID" }
    function GetMaxStatus(UserID:TGateUID):TGateUID;
    { Get min GroupID value (>0) for User "UserID".
      Returns 0 if no status stored for user "UserID" }
    function GetMinID(UserID:TGateUID):TGateUID;
    { Get the next UserID and GroupID with the current Status.
      Returns 0 if there are no more status records stored. }
    function GetNextStatus(var UserID,GroupID:TGateUID):TGateUID;
    end;

  { @Abstract(Gate User Group ID Management) }
  TRtcGateUserGroupIDs=class(TRtcGateObject)
  private
    FUsedGroupIDs:array[MinGID..MaxGID] of boolean;
    FCS:TRtcCritSec;

  public
    constructor Create;
    destructor Destroy; override;

    { Allocate next free Group ID, starting from "from" (default = 1)
      If the "from" ID and all higher IDs have been taken, raise an exception. }
    function AllocNextID(from:TGateUID=MinGID): TGateUID;
    { Allocate previous free Group ID, starting from "from" (default = 4095)
      If the "from" ID and all lower IDs have been taken, raise an exception. }
    function AllocPrevID(from:TGateUID=MaxGID): TGateUID;
    { Allocate a free Group ID, starting from "from" (default = 1).
      If "from" ID is already taken and all higher IDs are also taken,
      try all IDs lower than "from" (down to 1).
      If all IDs have already been taken, raise an exception. }
    function AllocFreeID(from:TGateUID=MinGID): TGateUID;

    { Allocate the Group ID "GroupID" (1 .. 4095),
      without checking if the ID has already been taken. }
    procedure AllocID(GroupID: TGateUID);
    // Release Group ID "GroupID" (mark as "not taken")
    procedure ReleaseID(GroupID: TGateUID);

    // Release all Group IDs
    procedure ReleaseAllIDs;
    end;

  TRtcAbsGateClient = class;

  TRtcHttpGateClient = class;

  TRtcHttpMultiGateClient = class;

  // Gate Client "Info" and "Data" Filter events
  TRtcGateClientFilterEvent = procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var Wanted:boolean) of object;

  // Gate Client "Info" and "Data" Events
  TRtcGateClientDataEvent = procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI, WantBackThread:boolean) of object;
  // Gate Client "Status" change Event
  TRtcGateClientStateEvent = procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean) of object;

  // Gate Client GUI "Data" and "Info" Event
  TRtcGateClientGUIDataEvent = procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData) of object;
  // Gate Client GUI "Status" change Event
  TRtcGateClientGUIStateEvent = procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo) of object;

  // Gate Client "Info" and "Data" Events
  TRtcGateClientBTDataEvent = procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI:boolean) of object;
  // Gate Client "Status" change Event
  TRtcGateClientBTStateEvent = procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI:boolean) of object;

{$IFDEF RTC_ANON_METHODS}

  // Gate Client "Info" and "Data" Filter events
  TRtcGateClientFilterAnonMethod = reference to procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var Wanted:boolean);

  // Gate Client "Info" and "Data" Events
  TRtcGateClientDataAnonMethod = reference to procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI, WantBackThread:boolean);
  // Gate Client "Status" change Event
  TRtcGateClientStateAnonMethod = reference to procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean);

  // Gate Client GUI "Data" and "Info" Event
  TRtcGateClientGUIDataAnonMethod = reference to procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData);
  // Gate Client GUI "Status" change Event
  TRtcGateClientGUIStateAnonMethod = reference to procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo);

  // Gate Client "Info" and "Data" Events
  TRtcGateClientBTDataAnonMethod = reference to procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI:boolean);
  // Gate Client "Status" change Event
  TRtcGateClientBTStateAnonMethod = reference to procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI:boolean);

{$ENDIF}

  { @Abstract(Abstract RTC Gate Client component) }
  TRtcAbsGateClient = class(TRtcClientComponent)
  protected
    // @exclude
    procedure Call_BeforeLogIn(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_AfterLoggedIn(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_AfterLoginFail(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_AfterLogOut(Sender:TRtcConnection); virtual;

    // @exclude
    procedure Call_OnDataReceived(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_OnInfoReceived(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_OnReadyToSend(Sender:TRtcConnection); virtual;
    // @exclude
    procedure Call_OnReadyAfterReset(Sender:TRtcConnection); virtual;

    // @exclude
    procedure Call_OnStreamReset(Sender:TRtcConnection); virtual;

   { Copy/paste the lines below to implement events for processing 
     data and override the "Call_*" methods listed above to use 
     "Filter(" and "Call(" methods (listed below) to call your methods.

    procedure DoDataFilter(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var Wanted:boolean);
    procedure DoInfoFilter(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var Wanted:boolean);
    procedure DoDataReceived(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI, WantBackThread:boolean);
    procedure DoInfoReceived(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI, WantBackThread:boolean);
    procedure DoAfterLogOut(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean);
    procedure DoReadyToSend(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean);
    procedure DoAfterLoginFail(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean);
    procedure DoAfterLoggedIn(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean);
    procedure DoReadyAfterReset(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean);
    procedure DoBeforeLogIn(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean);
    procedure DoStreamReset(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean);

    procedure DoDataReceivedGUI(Client:TRtcHttpGateClient; Data:TRtcGateClientData);
    procedure DoInfoReceivedGUI(Client:TRtcHttpGateClient; Data:TRtcGateClientData);
    procedure DoAfterLogOutGUI(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo);
    procedure DoReadyToSendGUI(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo);
    procedure DoAfterLoginFailGUI(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo);
    procedure DoAfterLoggedInGUI(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo);
    procedure DoReadyAfterResetGUI(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo);
    procedure DoBeforeLogInGUI(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo);
    procedure DoStreamResetGUI(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo);
    }

  public
    { Call "BTEvent" from the current (Sender's) thread - NOW.
      This method should ONLY be used from "Sender:TRtcConnection's" Background Thread!
      You can use "Sender.inBackThread" to check if we are in the right thread.

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      and the "Cliet.Data" property to be passed as a "Data" parameter to the Event.

      Returns TRUE if "BTEvent" or "Sender" parameters are NOT assignend.
      Returns FALSE if "BTEvent" and "Sender" parameters are both assigned,
      but the "Wanted" parameter was NOT set to "TRUE" inside "BTEvent".

      This method call is synchronous, which means that the Thread from which this
      method is called will be PAUSED to WAIT for the "GUIEvent" to finish executing. }
    function Filter(BTEvent: TRtcGateClientFilterEvent; Sender:TRtcConnection):boolean; overload;

    { Call "BTEvent" from the current thread. If "BTEvent" is NOT assigned or "BTEvent"
      returns with "WantGUI=True", "GUIEvent" will be executed from the Main (GUI) Thread.

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      and the "Cliet.Data" property to be passed as a "Data" parameter to the Event.

      Returns TRUE if "BTEvent" is NOT implemented or returns with "WantBackThread=True".

      This method call is synchronous, which means that the Thread from which this
      method is called will be PAUSED to WAIT for the "GUIEvent" to finish executing. }
    function Call(BTEvent:TRtcGateClientDataEvent; GUIEvent:TRtcGateClientGUIDataEvent; Sender:TRtcConnection):boolean; overload;

    { Call "BTEvent" from the current thread. If "BTEvent" is NOT assigned or "BTEvent"
      returns with "WantGUI=True", "GUIEvent" will be executed from the Main (GUI) Thread.

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter and
      the "Cliet.State" property to be passed on as a "State" parameter to the Event.

      Returns TRUE if "BTEvent" is NOT implemented or returns with "WantBackThread=True".

      This method call is synchronous, which means that the Thread from which this
      method is called will be PAUSED to WAIT for the "GUIEvent" to finish executing. }
    function Call(BTEvent:TRtcGateClientStateEvent; GUIEvent:TRtcGateClientGUIStateEvent; Sender: TRtcConnection):boolean; overload;

    { Call "BTEvent" from the current thread and ignore the "WantGUI" parameter (no GUI event).

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      and the "Cliet.Data" property to be passed as a "Data" parameter to the Event.

      Returns TRUE if "BTEvent" is NOT implemented or returns with "WantBackThread=True".

      This method call is synchronous, which means that the Thread from which this
      method is called will be PAUSED to WAIT for the "GUIEvent" to finish executing. }
    function Call(BTEvent:TRtcGateClientDataEvent; Sender:TRtcConnection):boolean; overload;

    { Call "BTEvent" from the current thread and ignore the "WantGUI" parameter (no GUI event).

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter and
      the "Cliet.State" property to be passed on as a "State" parameter to the Event.

      Returns TRUE if "BTEvent" is NOT implemented or returns with "WantBackThread=True".

      This method call is synchronous, which means that the Thread from which this
      method is called will be PAUSED to WAIT for the "GUIEvent" to finish executing. }
    function Call(BTEvent:TRtcGateClientStateEvent; Sender: TRtcConnection):boolean; overload;

    { Synchronize "GUIEvent" with the Main Thread. This method should ONLY be used
      from the "Sender:TRtcConnection's" Background Thread or the Main Thread!

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      and the "Cliet.Data" property to be passed as a "Data" parameter to the Event.

      This method call is synchronous, which means that the Thread from which this
      method is called will be PAUSED to WAIT for the "GUIEvent" to finish executing.

      Returns TRUE if the event was executed. }
    function CallGUI(GUIEvent: TRtcGateClientGUIDataEvent; Sender:TRtcConnection):boolean; overload;

    { Synchronize "GUIEvent" with the Main Thread. This method should ONLY be used
      from the "Sender:TRtcConnection's" Background Thread or the Main Thread!

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter and
      the "Cliet.State" property to be passed on as a "State" parameter to the Event.

      This method call is synchronous, which means that the Thread from which this
      method is called will be PAUSED to WAIT for the "GUIEvent" to finish executing.

      Returns TRUE if the event was executed. }
    function CallGUI(GUIEvent: TRtcGateClientGUIStateEvent; Sender:TRtcConnection):boolean; overload;

  {$IFDEF RTC_ANON_METHODS}

    { Get the Gate Client "Info" or "Data" Filter event for anonymous method
        procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var Wanted:boolean) }
    function Anon(const Event:TRtcGateClientFilterAnonMethod):TRtcGateClientFilterEvent; overload;

    { Get the Gate Client "Info" or "Data" Event for anonymous method
        procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI, WantBackThread:boolean) }
    function Anon(const Event:TRtcGateClientDataAnonMethod):TRtcGateClientDataEvent; overload;

    { Get the Gate Client "Status" change Event for anonymous method
        procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean) }
    function Anon(const Event:TRtcGateClientStateAnonMethod):TRtcGateClientStateEvent; overload;

    { Gate the Gate Client GUI "Data" or "Info" Event for anonymous method
        procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData) }
    function Anon(const Event:TRtcGateClientGUIDataAnonMethod):TRtcGateClientGUIDataEvent; overload;

    { Get the Gate Client GUI "Status" change Event for anonymous method
        procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo) }
    function Anon(const Event:TRtcGateClientGUIStateAnonMethod):TRtcGateClientGUIStateEvent; overload;

    { Get the Gate Client Back-Thread "Info" or "Data" Event for anonymous method
        procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI:boolean) }
    function Anon(const Event:TRtcGateClientBTDataAnonMethod):TRtcGateClientBTDataEvent; overload;

    { Get the Gate Client Back-Thread "Status" change Event for anonymous method
        procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI:boolean) }
    function Anon(const Event:TRtcGateClientBTStateAnonMethod):TRtcGateClientBTStateEvent; overload;

  {$ENDIF}
    end;

  { @Abstract(Abstract RTC Gate Client Thread component) }
  TRtcAbsGateClientThread=class(TRtcAbsGateClient)
  private
    FThr:TRtcThread;

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Post "BTEvent" to be executed in THIS components background Thread
      and/or "GUIEvent" to be executed (directly after "BTEvent") from the Main Thread,
      synchronized with THIS components background thread. "GUIEvent" will ONLY be
      executed if "BTEvent" is NOT provided or "BTEvent" returns with "WantGUI=TRUE".

      "Client" and "Data" parameters are both required (mandatory).
      The provided "Client" parameter will be sent directly to both Events,
      but the "Data" object will be copied to a temporary "TRtcGateClientData"
      instance and passed on as a paremeter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting
      for "BTEvent" or "GUIEvent" to start or finish executing. }
    procedure Post(BTEvent:TRtcGateClientBTDataEvent;
                   GUIEvent:TRtcGateClientGUIDataEvent;
                   Client:TRtcHttpGateClient;
                   Data: TRtcGateClientData); overload;

    { Post "BTEvent" to be executed in THIS components background Thread
      and/or "GUIEvent" to be executed (directly after "BTEvent") from the Main Thread,
      synchronized with THIS components background thread. "GUIEvent" will ONLY be
      executed if "BTEvent" is NOT provided or "BTEvent" returns with "WantGUI=TRUE".

      "Client" and "State" parameters are both required (mandatory).
      The provided "Client" parameter will be sent directly to both Events,
      but the "State" object will be copied to a temporary "TRtcGateClientStateInfo"
      instance and passed on as a paremeter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting
      for "BTEvent" or "GUIEvent" to start or finish executing. }
    procedure Post(BTEvent:TRtcGateClientBTStateEvent;
                   GUIEvent:TRtcGateClientGUIStateEvent;
                   Client:TRtcHttpGateClient;
                   State: TRtcGateClientStateInfo); overload;

    { Post "BTEvent" to be executed in THIS components background Thread
      and/or "GUIEvent" to be executed (directly after "BTEvent") from the Main Thread,
      synchronized with THIS components background thread. "GUIEvent" will ONLY be
      executed if "BTEvent" is NOT provided or "BTEvent" returns with "WantGUI=TRUE".

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      to both Events and make a copy of the "Cliet.Data" property, which will be
      passed on as a "Data:TRtcGateClientData" parameter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting
      for "BTEvent" or "GUIEvent" to start or finish executing. }
    procedure Post(BTEvent:TRtcGateClientBTDataEvent;
                   GUIEvent:TRtcGateClientGUIDataEvent;
                   Sender:TRtcConnection); overload;

    { Post "BTEvent" to be executed in THIS components background Thread
      and/or "GUIEvent" to be executed (directly after "BTEvent") from the Main Thread,
      synchronized with THIS components background thread. "GUIEvent" will ONLY be
      executed if "BTEvent" is NOT provided or "BTEvent" returns with "WantGUI=TRUE".

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      to both Events and make a copy of the "Client.State" property, which will be
      passed on as a "State:TRtcGateClientStateInfo" parameter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting
      for "BTEvent" or "GUIEvent" to start or finish executing. }
    procedure Post(BTEvent:TRtcGateClientBTStateEvent;
                   GUIEvent:TRtcGateClientGUIStateEvent;
                   Sender: TRtcConnection); overload;

    { Are we currently inside THIS components Thread? }
    function InsideThread:boolean;

    { Are we currently inside THIS components Background thread? }
    function InBackThread:boolean;

    { Are we curretly inside the Main Thread? }
    function InMainThread:boolean;

    { Stops this components virtual thread. }
    procedure StopThread;

    { Returns TRUE if there is an event running in this components virtual thread }
    function EventRunning:boolean;

    { Returns the current number of events in the queue, waiting to be
      executed in this components virtual thread, but not executing yet. }
    function EventsInQueue:integer;

    { Returns the total number of events currently running or in the
      queue (waiting to be executed) of this components virtual thread. }
    function EventsTotal:integer;

    { Virtual Thread used by THIS components to execute Background and GUI events. }
    property Thread:TRtcThread read FThr;
    end;

  { @Abstract(RTC Gate Client Thread)
    This component provides a Virtual Thread, place-holders for Gate-Client events
    and methods for postig events to be executed in THIS components Backgroud Thread 
    and the Main Thread, synchroized with THIS components Backgroud Thread. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcGCThread=class(TRtcAbsGateClientThread)
  private
    FRunBeforeLogIn: TRtcGateClientBTStateEvent;
    FRunAfterLoggedIn: TRtcGateClientBTStateEvent;
    FRunAfterLoginFail: TRtcGateClientBTStateEvent;
    FRunAfterLogOut: TRtcGateClientBTStateEvent;
    FRunDataReceived: TRtcGateClientBTDataEvent;
    FRunInfoReceived: TRtcGateClientBTDataEvent;
    FRunReadyToSend: TRtcGateClientBTStateEvent;
    FRunReadyAfterReset: TRtcGateClientBTStateEvent;
    FRunStreamReset: TRtcGateClientBTStateEvent;

    FSynBeforeLogIn: TRtcGateClientGUIStateEvent;
    FSynAfterLoggedIn: TRtcGateClientGUIStateEvent;
    FSynAfterLoginFail: TRtcGateClientGUIStateEvent;
    FSynAfterLogOut: TRtcGateClientGUIStateEvent;
    FSynDataReceived: TRtcGateClientGUIDataEvent;
    FSynInfoReceived: TRtcGateClientGUIDataEvent;
    FSynReadyToSend: TRtcGateClientGUIStateEvent;
    FSynReadyAfterReset: TRtcGateClientGUIStateEvent;
    FSynStreamReset: TRtcGateClientGUIStateEvent;

  public
    { Post THIS components "BeforeLogIn" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "BeforeLogInGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "BeforeLoginGUI" event will ONLY be called if "BeforeLogIn" event is
      NOT implemented or if the "BeforeLogIn" event returns with "WantGUI=TRUE".

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      to both Events and make a copy of the "Client.State" property, which will be
      passed on as a "State:TRtcGateClientStateInfo" parameter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting. }
    procedure Post_BeforeLogIn(Sender:TRtcConnection); overload;

    { Post THIS components "BeforeLogIn" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "BeforeLogInGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "BeforeLoginGUI" event will ONLY be called if "BeforeLogIn" event is
      NOT implemented or if the "BeforeLogIn" event returns with "WantGUI=TRUE".

      "Client" and "State" parameters are both required (mandatory).
      The provided "Client" parameter will be sent directly to both Events,
      but the "State" object will be copied to a temporary "TRtcGateClientStateInfo"
      instance and passed on as a paremeter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting
      for "BTEvent" or "GUIEvent" to start or finish executing. }
    procedure Post_BeforeLogIn(Client:TRtcHttpGateClient; State: TRtcGateClientStateInfo); overload;

    { Post THIS components "AfterLoggedIn" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "AfterLoggedInGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "AfterLoggedInGUI" event will ONLY be called if "AfterLoggedIn" event is
      NOT implemented or if the "AfterLoggedIn" event returns with "WantGUI=TRUE".

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      to both Events and make a copy of the "Client.State" property, which will be
      passed on as a "State:TRtcGateClientStateInfo" parameter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting. }
    procedure Post_AfterLoggedIn(Sender:TRtcConnection); overload;

    { Post THIS components "AfterLoggedIn" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "AfterLoggedInGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "AfterLoggedInGUI" event will ONLY be called if "AfterLoggedIn" event is
      NOT implemented or if the "AfterLoggedIn" event returns with "WantGUI=TRUE".

      "Client" and "State" parameters are both required (mandatory).
      The provided "Client" parameter will be sent directly to both Events,
      but the "State" object will be copied to a temporary "TRtcGateClientStateInfo"
      instance and passed on as a paremeter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting
      for "BTEvent" or "GUIEvent" to start or finish executing. }
    procedure Post_AfterLoggedIn(Client:TRtcHttpGateClient; State: TRtcGateClientStateInfo); overload;

    { Post THIS components "AfterLoginFail" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "AfterLoginFailGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "AfterLoginFailGUI" event will ONLY be called if "AfterLoginFail" event is
      NOT implemented or if the "AfterLoginFail" event returns with "WantGUI=TRUE".

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      to both Events and make a copy of the "Client.State" property, which will be
      passed on as a "State:TRtcGateClientStateInfo" parameter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting. }
    procedure Post_AfterLoginFail(Sender:TRtcConnection); overload;

    { Post THIS components "AfterLoginFail" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "AfterLoginFailGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "AfterLoginFailGUI" event will ONLY be called if "AfterLoginFail" event is
      NOT implemented or if the "AfterLoginFail" event returns with "WantGUI=TRUE".

      "Client" and "State" parameters are both required (mandatory).
      The provided "Client" parameter will be sent directly to both Events,
      but the "State" object will be copied to a temporary "TRtcGateClientStateInfo"
      instance and passed on as a paremeter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting
      for "BTEvent" or "GUIEvent" to start or finish executing. }
    procedure Post_AfterLoginFail(Client:TRtcHttpGateClient; State: TRtcGateClientStateInfo); overload;

    { Post THIS components "AfterLogOut" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "AfterLogOutGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "AfterLogOutGUI" event will ONLY be called if "AfterLogOut" event is
      NOT implemented or if the "AfterLogOut" event returns with "WantGUI=TRUE".

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      to both Events and make a copy of the "Client.State" property, which will be
      passed on as a "State:TRtcGateClientStateInfo" parameter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting. }
    procedure Post_AfterLogOut(Sender:TRtcConnection); overload;

    { Post THIS components "AfterLogOut" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "AfterLogOutGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "AfterLogOutGUI" event will ONLY be called if "AfterLogOut" event is
      NOT implemented or if the "AfterLogOut" event returns with "WantGUI=TRUE".

      "Client" and "State" parameters are both required (mandatory).
      The provided "Client" parameter will be sent directly to both Events,
      but the "State" object will be copied to a temporary "TRtcGateClientStateInfo"
      instance and passed on as a paremeter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting
      for "BTEvent" or "GUIEvent" to start or finish executing. }
    procedure Post_AfterLogOut(Client:TRtcHttpGateClient; State: TRtcGateClientStateInfo); overload;

    { Post THIS components "OnDataReceived" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "OnDataReceivedGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "OnDataReceivedGUI" event will ONLY be called if "OnDataReceived" event is
      NOT implemented or if the "OnDataReceived" event returns with "WantGUI=TRUE".

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      to both Events and make a copy of the "Cliet.Data" property, which will be
      passed on as a "Data:TRtcGateClientData" parameter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting. }
    procedure Post_DataReceived(Sender:TRtcConnection); overload;

    { Post THIS components "OnDataReceived" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "OnDataReceivedGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "OnDataReceivedGUI" event will ONLY be called if "OnDataReceived" event is
      NOT implemented or if the "OnDataReceived" event returns with "WantGUI=TRUE".

      "Client" and "Data" parameters are both required (mandatory).
      The provided "Client" parameter will be sent directly to both Events,
      but the "Data" object will be copied to a temporary "TRtcGateClientData"
      instance and passed on as a paremeter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting. }
    procedure Post_DataReceived(Client:TRtcHttpGateClient; Data: TRtcGateClientData); overload;

    { Post THIS components "OnInfoReceived" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "OnInfoReceivedGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "OnInfoReceivedGUI" event will ONLY be called if "OnInfoReceived" event is
      NOT implemented or if the "OnInfoReceived" event returns with "WantGUI=TRUE".

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      to both Events and make a copy of the "Cliet.Data" property, which will be
      passed on as a "Data:TRtcGateClientData" parameter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting. }
    procedure Post_InfoReceived(Sender:TRtcConnection); overload;

    { Post THIS components "OnInfoReceived" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "OnInfoReceivedGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "OnInfoReceivedGUI" event will ONLY be called if "OnInfoReceived" event is
      NOT implemented or if the "OnInfoReceived" event returns with "WantGUI=TRUE".

      "Client" and "Data" parameters are both required (mandatory).
      The provided "Client" parameter will be sent directly to both Events,
      but the "Data" object will be copied to a temporary "TRtcGateClientData"
      instance and passed on as a paremeter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting. }
    procedure Post_InfoReceived(Client:TRtcHttpGateClient; Data: TRtcGateClientData); overload;

    { Post THIS components "OnReadyAfterReset" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "OnReadyAfterResetGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "OnReadyAfterResetGUI" event will ONLY be called if "OnReadyAfterReset" event is
      NOT implemented or if the "OnReadyAfterReset" event returns with "WantGUI=TRUE".

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      to both Events and make a copy of the "Client.State" property, which will be
      passed on as a "State:TRtcGateClientStateInfo" parameter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting. }
    procedure Post_ReadyAfterReset(Sender:TRtcConnection); overload;

    { Post THIS components "OnReadyAfterReset" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "OnReadyAfterResetGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "OnReadyAfterResetGUI" event will ONLY be called if "OnReadyAfterReset" event is
      NOT implemented or if the "OnReadyAfterReset" event returns with "WantGUI=TRUE".

      "Client" and "State" parameters are both required (mandatory).
      The provided "Client" parameter will be sent directly to both Events,
      but the "State" object will be copied to a temporary "TRtcGateClientStateInfo"
      instance and passed on as a paremeter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting
      for "BTEvent" or "GUIEvent" to start or finish executing. }
    procedure Post_ReadyAfterReset(Client:TRtcHttpGateClient; State: TRtcGateClientStateInfo); overload;

    { Post THIS components "OnReadyToSend" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "OnReadyToSendGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "OnReadyToSendGUI" event will ONLY be called if "OnReadyToSend" event is
      NOT implemented or if the "OnReadyToSend" event returns with "WantGUI=TRUE".

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      to both Events and make a copy of the "Client.State" property, which will be
      passed on as a "State:TRtcGateClientStateInfo" parameter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting. }
    procedure Post_ReadyToSend(Sender:TRtcConnection); overload;

    { Post THIS components "OnReadyToSend" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "OnReadyToSendGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "OnReadyToSendGUI" event will ONLY be called if "OnReadyToSend" event is
      NOT implemented or if the "OnReadyToSend" event returns with "WantGUI=TRUE".

      "Client" and "State" parameters are both required (mandatory).
      The provided "Client" parameter will be sent directly to both Events,
      but the "State" object will be copied to a temporary "TRtcGateClientStateInfo"
      instance and passed on as a paremeter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting
      for "BTEvent" or "GUIEvent" to start or finish executing. }
    procedure Post_ReadyToSend(Client:TRtcHttpGateClient; State: TRtcGateClientStateInfo); overload;

    { Post THIS components "OnStreamReset" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "OnStreamResetGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "OnStreamResetGUI" event will ONLY be called if "OnStreamReset" event is
      NOT implemented or if the "OnStreamReset" event returns with "WantGUI=TRUE".

      "Sender" parameter is required (mandatory). It will be used to access the
      "TRtcHttpGateClient(Seder.Owner)" property to be passed as a "Client" parameter
      to both Events and make a copy of the "Client.State" property, which will be
      passed on as a "State:TRtcGateClientStateInfo" parameter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting. }
    procedure Post_StreamReset(Sender:TRtcConnection); overload;

    { Post THIS components "OnStreamReset" event to be executed (asynchronously) from
      THIS components Background Thread and/or the "OnStreamResetGUI" event to be executed
      from the Main Thread, synchronized with THIS components Background Thread.

      "OnStreamResetGUI" event will ONLY be called if "OnStreamReset" event is
      NOT implemented or if the "OnStreamReset" event returns with "WantGUI=TRUE".

      "Client" and "State" parameters are both required (mandatory).
      The provided "Client" parameter will be sent directly to both Events,
      but the "State" object will be copied to a temporary "TRtcGateClientStateInfo"
      instance and passed on as a paremeter to both Events, then destroyed.

      This method call is asynchronous, which means that the Thread from
      which this method is called will continue running without waiting
      for "BTEvent" or "GUIEvent" to start or finish executing. }
    procedure Post_StreamReset(Client:TRtcHttpGateClient; State: TRtcGateClientStateInfo); overload;

  published
    { Event called from THIS components Background Thread after using the "Post_BeforeLogIn" method. }
    property BeforeLogIn:TRtcGateClientBTStateEvent read FRunBeforeLogIn write FRunBeforeLogIn;

    { Event called from THIS components Background Thread after using the "Post_AfterLoggedIn" method. }
    property AfterLoggedIn:TRtcGateClientBTStateEvent read FRunAfterLoggedIn write FRunAfterLoggedIn;

    { Event called from THIS components Background Thread after using the "Post_AfterLoginFail" method. }
    property AfterLoginFail:TRtcGateClientBTStateEvent read FRunAfterLoginFail write FRunAfterLoginFail;

    { Event called from THIS components Background Thread after using the "Post_AfterLogOut" method. }
    property AfterLogOut:TRtcGateClientBTStateEvent read FRunAfterLogOut write FRunAfterLogOut;

    { Event called from THIS components Background Thread after using the "Post_DataReceived" method. }
    property OnDataReceived:TRtcGateClientBTDataEvent read FRunDataReceived write FRunDataReceived;

    { Event called from THIS components Background Thread after using the "Post_InfoReceived" method. }
    property OnInfoReceived:TRtcGateClientBTDataEvent read FRunInfoReceived write FRunInfoReceived;

    { Event called from THIS components Background Thread after using the "Post_ReadyAfterReset" method. }
    property OnReadyAfterReset:TRtcGateClientBTStateEvent read FRunReadyAfterReset write FRunReadyAfterReset;

    { Event called from THIS components Background Thread after using the "Post_ReadyToSend" method. }
    property OnReadyToSend:TRtcGateClientBTStateEvent read FRunReadyToSend write FRunReadyToSend;

    { Event called from THIS components Background Thread after using the "Post_StreamReset" method. }
    property OnStreamReset:TRtcGateClientBTStateEvent read FRunStreamReset write FRunStreamReset;

    { Event called from the Main (GUI) Thread, synchronized with THIS components Background Thread, after using
      the "Post_BeforeLogIn" method if the "BeforeLogin" event is NOT implemented or returns with "WantGUI=TRUE". }
    property BeforeLogInGUI:TRtcGateClientGUIStateEvent read FSynBeforeLogIn write FSynBeforeLogIn;

    { Event called from the Main (GUI) Thread, synchronized with THIS components Background Thread, after using
      the "Post_AfterLoggedIn" method if the "AfterLoggedIn" event is NOT implemented or returns with "WantGUI=TRUE". }
    property AfterLoggedInGUI:TRtcGateClientGUIStateEvent read FSynAfterLoggedIn write FSynAfterLoggedIn;

    { Event called from the Main (GUI) Thread, synchronized with THIS components Background Thread, after using
      the "Post_AfterLoginFail" method if the "AfterLoginFail" event is NOT implemented or returns with "WantGUI=TRUE". }
    property AfterLoginFailGUI:TRtcGateClientGUIStateEvent read FSynAfterLoginFail write FSynAfterLoginFail;

    { Event called from the Main (GUI) Thread, synchronized with THIS components Background Thread, after using
      the "Post_AfterLogOut" method if the "AfterLogOut" event is NOT implemented or returns with "WantGUI=TRUE". }
    property AfterLogOutGUI:TRtcGateClientGUIStateEvent read FSynAfterLogOut write FSynAfterLogOut;

    { Event called from the Main (GUI) Thread, synchronized with THIS components Background Thread, after using
      the "Post_DataReceived" method if the "OnDataReceived" event is NOT implemented or returns with "WantGUI=TRUE". }
    property OnDataReceivedGUI:TRtcGateClientGUIDataEvent read FSynDataReceived write FSynDataReceived;

    { Event called from the Main (GUI) Thread, synchronized with THIS components Background Thread, after using
      the "Post_InfoReceived" method if the "OnInfoReceived" event is NOT implemented or returns with "WantGUI=TRUE". }
    property OnInfoReceivedGUI:TRtcGateClientGUIDataEvent read FSynInfoReceived write FSynInfoReceived;

    { Event called from the Main (GUI) Thread, synchronized with THIS components Background Thread, after using
      the "Post_ReadyAfterReset" method if the "OnReadyAfterReset" event is NOT implemented or returns with "WantGUI=TRUE". }
    property OnReadyAfterResetGUI:TRtcGateClientGUIStateEvent read FSynReadyAfterReset write FSynReadyAfterReset;

    { Event called from the Main (GUI) Thread, synchronized with THIS components Background Thread, after using
      the "Post_ReadyToSend" method if the "OnReadyToSend" event is NOT implemented or returns with "WantGUI=TRUE". }
    property OnReadyToSendGUI:TRtcGateClientGUIStateEvent read FSynReadyToSend write FSynReadyToSend;

    { Event called from the Main (GUI) Thread, synchronized with THIS components Background Thread, after using
      the "Post_StreamReset" method if the "OnStreamReset" event is NOT implemented or returns with "WantGUI=TRUE". }
    property OnStreamResetGUI:TRtcGateClientGUIStateEvent read FSynStreamReset write FSynStreamReset;
    end;

  { @Abstract(Abstract RTC Gate Client Link class)
    Implements basic functionality required by Gate Client Link components.

    Every component based on TRtcAbsGateClientLink allocates one "MyGroupID"
    for its exlusive use. Because the max number of available Group IDs is limited to 4095,
    it will NOT be possible to have more than 4095 components based on TRtcAbsGateClientLink
    working with the exact same TRtcHttpGateClient component, but ... since 4095 is quite a
    big number, it is highly unlikely that we will ever need so many different components
    processing data coming from a single TRtcHttpGateClient component.

    If this TRtcAbsGateClientLink component is linked to a TRtcHttpGateClient component
    that was created by a TRtcHttpMultiGateClient component, "MyGroupID" will be
    allocated to work with all "TRtcHttpGateClient" components created or linked to
    the same "TRtcHttpMultiGateClient" component (using the same "SharedGroups" property). }
  TRtcAbsGateClientLink = class(TRtcAbsGateClient)
  private
    FClient: TRtcHttpGateClient;
    FUserGroups: TRtcGateUserGroupStates;
    FMyGroupID: TGateUID;
    FLocked: integer;

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure RemoveClient(Value:TRtcHttpGateClient); virtual;

    // @exclude
    function GetClient: TRtcHttpGateClient; virtual;
    // @exclude
    procedure SetClient(const Value: TRtcHttpGateClient); virtual;

    // @exclude
    function GetMyGroupID:TGateUID; virtual;
    // @exclude
    procedure SetMyGroupID(const Value: TGateUID); virtual;

    // @exclude
    function GetUserGroups: TRtcGateUserGroupStates; virtual;

    // @exclude
    procedure Call_ClientAssigned; virtual;
    // @exclude
    procedure Call_ClientRemoved; virtual;
    // @exclude
    procedure Sync_ClientRemoved; virtual;

  public

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Uses "UserInfo" property on the "Client" (TRtcHttpGateClient) component
      to return the last received "UserInfo" string for the User "UserID".
      The "UserInfo" string is received from the Gateway with Account notifications,
      contains the "GateUserInfo" property assigned on the Client for which the notification
      was sent and remains populated while the User is logged in with at least one
      Account managed by the "AccountManager" assigned to the "Client" component. }
    function UserInfo(const UserID:TGateUID):RtcString;

    { Uses "Ping" method on the "Client" (TRtcHttpGateClient) component to
      Ping the user "UserID" and get notified if the User is OFF-LINE.
      Returns TRUE if the PING command was placed into sending buffers. }
    function PingUser(const UserID:TGateUID):boolean;

    { Uses "RequestUserVerifications" method on the "Client" (TRtcHttpGateClient) component to
      Request verifications for all "Linked" accounts logged in with "UserID" which have NOT beed verified.
      Returns TRUE if at least one verification request was sent. "OnInfoReceived" events will be triggered
      with "Data.Command" = "gc_AccountVerified" for all remote Accounts where verification was successful. }
    function VerifyUser(const UserID:TGateUID):boolean;

    { Uses "UserNotVerified" property on the "Client" (TRtcHttpGateClient) component to
      Check if the User "UserID" is NOT (fully) verified.
      Returns TRUE if the User is logged in on at least one "Linked" (remote Private)
      account for which we have NOT yet received a positive account verification.
      If this property returns TRUE, you can use the "VerifyUser" method
      to ask the User to verify all logged in but NOT yet verified "Linked" accounts. }
    function UserNotVerified(const UserID:TGateUID):boolean;

    { Uses "UserAllowed" method on the "Client" (TRtcHttpGateClient) component to
      Check if the User "UserID" is allowed to perform Action "action",
      Based on Permissions for "Public" accounts and *VERIFIED* "Private" or "Linked" Accounts. }
    function UserAllowed(const UserID:TGateUID; const action:word):boolean;

    { Uses "AddUserToMyGroup" method on the "Client" (TRtcHttpGateClient) component to
      Add the User "UserID" to My Group. Returns TRUE if command was placed into sending buffers.
      If the User is NOT online, or the User goes Off-Line, or we lose connection to the
      Gateway where the User was logged in, the User will automatically be removed from our Group. }
    function AddUserToGroup(const UserID:TGateUID):boolean;

    { Uses "IsUserInMyGroup" method on the "Client" (TRtcHttpGateClient) component to
      Return TRUE if the user "UserID" is in My Group. }
    function IsUserInGroup(const UserID:TGateUID):boolean;

    { Uses "RemoveUserFromMyGroup" method on the "Client" (TRtcHttpGateClient) component to
      Remove User "UserID" from My Group. Returns TRUE if command was placed into sending buffers. }
    function RemoveUserFromGroup(const UserID:TGateUID):boolean;

    { Uses "UsersInMyGroup" method on the "Client" (TRtcHttpGateClient) component to
      Return the current total number of users in My Group. }
    function UsersInGroup:integer;

    { Uses "DisbandMyGroup" method on the "Client" (TRtcHttpGateClient) component to
      Remove ALL USERS from My Group and Disband My Group, in case "AddUserToGroup" method
      was used on THIS component, or the "AddUsersToMyGroup" method was used on the
      "Client" (TRtcHttpGateClient) component with "MyGroupID" as parameter.
      Returns TRUE if connection command was placed into sending buffers. }
    function DisbandGroup:boolean;

    { Uses "SendToMyGroup" method on the "Client" (TRtcHttpGateClient) component to
      Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to My Group. @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000!
      Returns TRUE if the package was placed into sending buffers. }
    function SendToGroup(CallID:word; const data:RtcByteArray):boolean; overload;

    { Uses "SendToMyGroup" method on the "Client" (TRtcHttpGateClient) component to
      Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to My Group. @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000!
      Returns TRUE if the package was placed into sending buffers. }
    function SendToGroup(CallID:word; const data:RtcString):boolean; overload;

    { Uses "SendToMyGroup" method on the "Client" (TRtcHttpGateClient) component to
      Send "CallID" (0..65000) with no extra content to My Group. @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use IDs above 65000!
      Returns TRUE if the package was placed into sending buffers. }
    function SendToGroup(CallID:word):boolean; overload;

    { Uses "SendBytes" method on the "Client" (TRtcHttpGateClient) component to
      Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to "UserID" and "GroupID". @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000!
      Returns TRUE if the package was placed into sending buffers. }
    function SendBytes(const UserID:TGateUID; GroupID:TGateUID; CallID:word; const data:RtcByteArray):boolean; overload;

    { Uses "SendBytes" method on the "Client" (TRtcHttpGateClient) component to
      Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to "UserID" and "GroupID". @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000!
      Returns TRUE if the package was placed into sending buffers. }
    function SendBytes(const UserID:TGateUID; GroupID:TGateUID; CallID:word; const data:RtcString):boolean; overload;

    { Uses "SendBytes" method on the "Client" (TRtcHttpGateClient) component to
      Send "CallID" (0..65000) with no extra content to "UserID" and "GroupID". @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use IDs above 65000!
      Returns TRUE if the package was placed into sending buffers. }
    function SendBytes(const UserID:TGateUID; GroupID:TGateUID; CallID:word):boolean; overload;

    { Uses "RemoveFriend" method on the "Client" (TRtcHttpGateClient) component to
      Remove User "UserID" from our Friends list, revoking Users
      permissions to add us to his User Group(s) on the Gateway.
      If user "UserID" was in our Friends list on the Gateway,
      we will receive the "gc_FriendRemove" command from the Gateway
      and the user "UserID" will receive the "gc_UnFriend" command.
      Removing a User from our Friends list will not close open User Groups,
      but it will prevent the User from adding us to more Groups.
      NOTE: Removing Friends is not necessary, because our Frields list will
      be cleared on the Gateway if a User logs out or looses connection. }
    function RemoveFriend(const UserID:TGateUID):boolean;

    { Uses "AddFriend" method on the "Client" (TRtcHttpGateClient) component to
      Add User "UserID" as our Friend, giving the User permission to add us to his User Group(s)
      on the Gateway. If the user "UserID" is online and was NOT already in our Friends list,
      we will receive the "gc_FriendAdd" command from the Gateway and the user "UserID" will receive
      the "gc_BeFriend" command. If the user is off-line, we will receive a "gc_UserOffLine" command.
      If the user is already in our friends list, nothing changes and the Gateway won't send any
      notification messages to us or the user. Returns TRUE if the command was placed into sending buffers. }
    function AddFriend(const UserID:TGateUID):boolean;

    { Uses "LeaveUsersGroup" method on the "Client" (TRtcHttpGateClient) component to
      Leave Group "GroupID" owned by user "OwnerID". Returns TRUE if command was placed into sending buffers. }
    function LeaveUsersGroup(const OwnerID:TGateUID; const GroupID:TGateUID):boolean;

    { Is the "Client" connection Ready to send and receive? }
    function Ready:boolean;

    { Returns My "UserID" on the Gateway. }
    function MyUID:TGateUID;

    { User Group State Management }
    property Groups:TRtcGateUserGroupStates read GetUserGroups;

  published
    { "My" Group ID, allocated automatically when a new Client component is
      assigned and de-allocated automatically when the Client component is removed.

      Assigning a new value to this property automatically de-allocates any previously
      allocated "My" GroupID and allocates the closest currently unused Group ID,
      starting from the value being assigned, then checking higher and lower values.
      Should the search for an available Group ID fail, an exception will be reaised.

      When a Client property is NOT assigned, this property is always zero (0). }
    property MyGroupID:TGateUID read GetMyGroupID write SetMyGroupID stored False;

    { Set the Client property to a RtcHttpGateClient component to listen to
      all events from that component. Setting the Client property also allocates
      a new "MyGroupID". If another "Client" component was previously assigned,
      the old "MyGroupID" will be de-allocated before allocating a new "MyGroupID".
      If the Client component being assigned does NOT have any available Group IDs,
      this Client property will be set to NIL and an exception will be raised.
      Set "Client:=NIL" and make sure it is NIL, before destroying THIS component.
      "AfterClientRemoved" event is triggered when the component is safe to destroy. }
    property Client:TRtcHttpGateClient read GetClient write SetClient;
    end;

  { @Abstract(RTC Gate Client Link)
    Link this component to a TRtcHttpGateClient to handle user events. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcGateClientLink = class(TRtcAbsGateClientLink)
  private
    FClientAssigned: TRtcSyncEvent;
    FClientRemoved: TRtcSyncEvent;
    FThread: TRtcGCThread;

    FOnDataFilter: TRtcGateClientFilterEvent;
    FOnInfoFilter: TRtcGateClientFilterEvent;
    FConDataReceived: TRtcGateClientDataEvent;
    FConInfoReceived: TRtcGateClientDataEvent;
    FConAfterLogOut: TRtcGateClientStateEvent;
    FConReadyToSend: TRtcGateClientStateEvent;
    FConAfterLoginFail: TRtcGateClientStateEvent;
    FConAfterLoggedIn: TRtcGateClientStateEvent;
    FConReadyAfterReset: TRtcGateClientStateEvent;
    FConBeforeLogIn: TRtcGateClientStateEvent;
    FConStreamReset: TRtcGateClientStateEvent;

    FGuiDataReceived: TRtcGateClientGUIDataEvent;
    FGuiInfoReceived: TRtcGateClientGUIDataEvent;
    FGuiAfterLogOut: TRtcGateClientGUIStateEvent;
    FGuiReadyToSend: TRtcGateClientGUIStateEvent;
    FGuiAfterLoginFail: TRtcGateClientGUIStateEvent;
    FGuiAfterLoggedIn: TRtcGateClientGUIStateEvent;
    FGuiReadyAfterReset: TRtcGateClientGUIStateEvent;
    FGuiBeforeLogIn: TRtcGateClientGUIStateEvent;
    FGuiStreamReset: TRtcGateClientGUIStateEvent;

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure Call_ClientAssigned; override;
    // @exclude
    procedure Call_ClientRemoved; override;

    // @exclude
    procedure Call_BeforeLogIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLoggedIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLoginFail(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLogOut(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnDataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnInfoReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnReadyToSend(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnReadyAfterReset(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnStreamReset(Sender:TRtcConnection); override;

  published
    { You would like some events triggered by THIS component to be executed asynchronously,
      either inside a separate Backgrdound Thread or asynchronously from the Main Thread?

      Assign a "TRtcGCThread" component to the "BackThread" property, implement events on the
      "BackThread" component which you want executed asynchronously from the Backround Thread
      or asynchronously from the Main Thread (GUI), then leave these events without implementation
      on THIS component, or implement events on THIS component and set "WantBackThread:=True" from
      inside any event on THIS component if you want the same event posted for asynchonous
      execution on "BackThread" after events on THIS component finish executing.

      If an event is NOT implementated on THIS component, but an event with the same name
      is implemented on the "BackThread" component, event on the "BackThread" component will
      automatically be posted for (asynchronous) execution after the "GUI" event (optional) finishes.

      For example, if you want the "AfterLoggedIn" event on the "BackThread" component executed
      asynchronously after the user logs in, you can either set "WantBackThread:=True" from the
      "AfterLoggedIn" event on THIS component, or leave THIS components "AfterLoggedIn" event
      unassigned (NOT implemented). This rule applies to all events with a "WantBackThread" parameter. }
    property BackThread:TRtcGCThread read FThread write FThread;

    { Event called after a new "TRtcHttpGateClient" component is assigned to the "Client" property. }
    property AfterClientAssigned:TRtcSyncEvent read FClientAssigned write FClientAssigned;

    { Event called from the Main (GUI) Thread after the "TRtcHttpGateClient" component is removed
      from THIS components "Client" property. It is safe to destroy the component from this event. }
    property AfterClientRemoved:TRtcSyncEvent read FClientRemoved write FClientRemoved;

    { Event called BEFORE the user Logs in to the Gateway. It can be used for custom component setup.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "BeforeLoginGUI" event will be executed from the Main Thread after this event. }
    property BeforeLogIn:TRtcGateClientStateEvent read FConBeforeLogIn write FConBeforeLogIn;

    { Event called after the Client logs in to the Gateway.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLoggedInGUI" event will be executed from the Main Thread after this event. }
    property AfterLoggedIn:TRtcGateClientStateEvent read FConAfterLoggedIn write FConAfterLoggedIn;

    { Event called if the last Login attempt has failed.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLoginFailGUI" event will be executed from the Main Thread after this event. }
    property AfterLoginFail:TRtcGateClientStateEvent read FConAfterLoginFail write FConAfterLoginFail;

    { Event called if the Client has been logged out of the Gateway.
      This can either be because the Client has permanently lost connection
      and the UserID belonging to this Client is no longer valid,
      or because the Active property was set to FALSE to close the connection.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLogOutGUI" event will be executed from the Main Thread after this event. }
    property AfterLogOut:TRtcGateClientStateEvent read FConAfterLogOut write FConAfterLogOut;

    { Data was received from the Gateway. Use provided parameters to check what was received
      and set "Wanted:=TRUE" if THIS components "OnDataReceived" event should be called.
      If "OnDataFilter" event is NOT implemented, "OnDataReceived" and/or "OnDataRecievedGUI" event is called for ALL data received.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread. }
    property OnDataFilter:TRtcGateClientFilterEvent read FOnDataFilter write FOnDataFilter;

    { Info was received from the Gateway. Use provided parameters to check what was received
      and set "Wanted:=TRUE" if THIS components "OnInfoReceived" event should be called.
      If "OnInfoFilter" event is NOT implemented, "OnInfoReceived" event is called for ALL info received.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread. }
    property OnInfoFilter:TRtcGateClientFilterEvent read FOnInfoFilter write FOnInfoFilter;

    { Received data from the Gateway. Triggered ONLY if the "OnDataFilter" event
      is NOT implemented or if the "OnDataFilter" event returns with "Wanted=True".
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnDataReceivedGUI" event will be executed from the Main Thread after this event. }
    property OnDataReceived:TRtcGateClientDataEvent read FConDataReceived write FConDataReceived;

    { Received info from the Gateway. Triggered ONLY if the "OnInfoFilter" event
      is NOT implemented or if the "OnInfoFilter" event returns with "Wanted=True".
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnInfoReceivedGUI" event will be executed from the Main Thread after this event. }
    property OnInfoReceived:TRtcGateClientDataEvent read FConInfoReceived write FConInfoReceived;

    { Client is again ready to send data to the Gateway after the last Stream Reset.
      Use this event to re-invite Users to your Groups and fix broken User relations.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnReadyAfterResetGUI" event will be executed from the Main Thread after this event. }
    property OnReadyAfterReset:TRtcGateClientStateEvent read FConReadyAfterReset write FConReadyAfterReset;

    { Client is ready to send more data to the Gateway. Use this event to implement
      the process of sending larger data "streams" by breaking them up in smaller packets.
      Keep in mind that the maximum single packet size is 16 MB.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnReadyToSendGUI" event will be executed from the Main Thread after this event. }
    property OnReadyToSend:TRtcGateClientStateEvent read FConReadyToSend write FConReadyToSend;

    { Input and Output Streams have been reset, either because the "ResetStreams"
      method was called, or because Clients connection to the Gateway was lost.
      A new connection attempt will be made after this event.
      Use this event for cleaning up Group Status information.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnStreamResetGUI" event will be executed from the Main Thread after this event. }
    property OnStreamReset:TRtcGateClientStateEvent read FConStreamReset write FConStreamReset;

    { Event called from the Main (GUI) Thread, synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "BeforeLogIn" event is NOT implemented or returns with "WantGUI=True". }
    property BeforeLogInGUI:TRtcGateClientGUIStateEvent read FGuiBeforeLogIn write FGuiBeforeLogIn;

    { Event called from the Main (GUI) Thread, synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLoggedIn" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLoggedInGUI:TRtcGateClientGUIStateEvent read FGuiAfterLoggedIn write FGuiAfterLoggedIn;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLoginFail" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLoginFailGUI:TRtcGateClientGUIStateEvent read FGuiAfterLoginFail write FGuiAfterLoginFail;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLogOut" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLogOutGUI:TRtcGateClientGUIStateEvent read FGuiAfterLogOut write FGuiAfterLogOut;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnDataReceived" event is NOT implemented or returns with "WantGUI=True". }
    property OnDataReceivedGUI:TRtcGateClientGUIDataEvent read FGuiDataReceived write FGuiDataReceived;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnInfoReceived" event is NOT implemented or returns with "WantGUI=True". }
    property OnInfoReceivedGUI:TRtcGateClientGUIDataEvent read FGuiInfoReceived write FGuiInfoReceived;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnReadyAfterReset" event is NOT implemented or returns with "WantGUI=True". }
    property OnReadyAfterResetGUI:TRtcGateClientGUIStateEvent read FGuiReadyAfterReset write FGuiReadyAfterReset;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnReadyToSend" event is NOT implemented or returns with "WantGUI=True". }
    property OnReadyToSendGUI:TRtcGateClientGUIStateEvent read FGuiReadyToSend write FGuiReadyToSend;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnStreamReset" event is NOT implemented or returns with "WantGUI=True". }
    property OnStreamResetGUI:TRtcGateClientGUIStateEvent read FGuiStreamReset write FGuiStreamReset;
    end;

  // @exclude
  TRtcGateClientLinkList = class(TObject)
  private
    FList:TObjList;
    CS:TRtcCritSec;

  public
    constructor Create;
    destructor Destroy; override;

    function Count:integer;
    procedure RemoveAll;

    procedure Add(Value:TRtcAbsGateClientLink);
    procedure Remove(Value:TRtcAbsGateClientLink);

    function LockFirst:TRtcAbsGateClientLink;
    procedure LockNext(var Value:TRtcAbsGateClientLink);

    procedure DoBeforeLogIn(Sender:TRtcConnection);
    procedure DoAfterLoggedIn(Sender:TRtcConnection);
    procedure DoAfterLoginFail(Sender:TRtcConnection);
    procedure DoAfterLogOut(Sender:TRtcConnection);

    procedure DoDataReceived(Sender:TRtcConnection);
    procedure DoInfoReceived(Sender:TRtcConnection);
    procedure DoReadyToSend(Sender:TRtcConnection);
    procedure DoReadyAfterReset(Sender:TRtcConnection);

    procedure DoStreamReset(Sender:TRtcConnection);
    end;

  { @Abstract(RTC Gate Account Permissions) }
  TRtcGatePermissions = array of word;

  { @Abstract(RTC Gate Account Types) }
  TRtcGateAccountType=(// NOT a RTC Gate Account
                       gat_None,
                       // Public Account
                       gat_Public,
                       // Private Account
                       gat_Private,
                       // Link to a Private Account
                       gat_Linked);

  // @exclude
  TRtcGateAccountAction=(// Account registered and activated
                         gaa_Registered,
                         // Account activated (was deactivated before)
                         gaa_Activated,
                         // Account deactivated (was activate before)
                         gaa_Deactivated,
                         // Account deleted (active or inactive)
                         gaa_Deleted);

  { @Abstract(RTC Gate Account Manager)
    Provides all methods and properties required for Gateway Account Management.

    All events on this component are called from within a thread context of the
    connection component triggering each event, which means that components events
    will be called from multiple threads (depending on what triggered the event) and
    all the code you write for handling this components events should be thread-safe.

    All methods and properties provided by this component are thread-safe and
    may be used from a multi-threaded Application, without any special safety
    precautions. "Acquire" and "Release" methods are provided ONLY in case
    longer access to Gate Accounts data is needed, when changes to the Accounts
    data should NOT be allowed during access. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcGateAccountManager=class(TRtcAbsGateClient)
  private
    GateCS:TRtcCritSec;
    FACC:TStrObjList;
    FUsedGIDs:word;
    FGIDs:array[MinGID..MaxGID] of RtcString;
    FGIDTime:array[MinGID..MaxGID] of TAppRunTime;
    FCli:tObjList;
    FMinGroupID,
    FMaxGroupID,
    FNextGroupID:TGateUID;
    FPublicID:RtcString;
  {$IFDEF RTC_RSA}
    skey:TRtcRSA;
    rnd:TRtcISAAC;
  {$ENDIF}

    FThread: TRtcGCThread;
    FSharedGroups: TRtcGateUserGroupIDs;

    FAuthCode:RtcString;
    FAuthCodeTimeout:TAppRunTime;

    FOnDataFilter: TRtcGateClientFilterEvent;
    FOnInfoFilter: TRtcGateClientFilterEvent;
    FConDataReceived: TRtcGateClientDataEvent;
    FConInfoReceived: TRtcGateClientDataEvent;
    FConAfterLogOut: TRtcGateClientStateEvent;
    FConReadyToSend: TRtcGateClientStateEvent;
    FConAfterLoginFail: TRtcGateClientStateEvent;
    FConAfterLoggedIn: TRtcGateClientStateEvent;
    FConReadyAfterReset: TRtcGateClientStateEvent;
    FConBeforeLogIn: TRtcGateClientStateEvent;
    FConStreamReset: TRtcGateClientStateEvent;

    FGuiDataReceived: TRtcGateClientGUIDataEvent;
    FGuiInfoReceived: TRtcGateClientGUIDataEvent;
    FGuiAfterLogOut: TRtcGateClientGUIStateEvent;
    FGuiReadyToSend: TRtcGateClientGUIStateEvent;
    FGuiAfterLoginFail: TRtcGateClientGUIStateEvent;
    FGuiAfterLoggedIn: TRtcGateClientGUIStateEvent;
    FGuiReadyAfterReset: TRtcGateClientGUIStateEvent;
    FGuiBeforeLogIn: TRtcGateClientGUIStateEvent;
    FGuiStreamReset: TRtcGateClientGUIStateEvent;

    FVerifyAccounts:boolean;

    procedure NewGroupID(const id:RtcString; var group:TGateUID);
    procedure RemGroupID(const id:RtcString; var group:TGateUID);

    function GetType(const id: RtcString): TRtcGateAccountType;

    function GetActive(const id: RtcString): boolean;
    procedure SetActive(const id: RtcString; const Value: boolean);

    function GetAllowed(const id: RtcString; const action: word): boolean;
    procedure SetAllowed(const id: RtcString; const action: word; const Value: boolean);

    function GetDisplayName(const id: RtcString): RtcWideString;
    procedure SetDisplayName(const id: RtcString; const Value: RtcWideString);

    function GetLocalName(const id: RtcString): RtcWideString;
    procedure SetLocalName(const id: RtcString; const Value: RtcWideString);

    function GetRemoteName(const id: RtcString): RtcWideString;
    procedure SetRemoteName(const id: RtcString; const Value: RtcWideString);

    function GetPermissions(const id: RtcString): TRtcGatePermissions;
    procedure SetPermissions(const id: RtcString; const Value: TRtcGatePermissions);

    procedure SetMaxGroupID(const Value: TGateUID);
    procedure SetMinGroupID(const Value: TGateUID);

    procedure NotifyClients(const id: RtcString; action:TRtcGateAccountAction);

    procedure RegisterClient(cli:TRtcHttpGateClient);
    procedure RefreshClient(cli:TRtcHttpGateClient);
    procedure UnregisterClient(cli:TRtcHttpGateClient);
    procedure UnregisterAllClients;

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure Call_BeforeLogIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLoggedIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLoginFail(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLogOut(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnDataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnInfoReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnReadyToSend(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnReadyAfterReset(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnStreamReset(Sender:TRtcConnection); override;

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Save all Account Data to "Code". This includes private data,
      permissions and the "active" state for each Account.
      Returns "code:RtcByteArray" for use with "RegisterFromCode".
      Can be stored to a File with the "Write_FileEx" method
      and loaded with "Read_FileEx" for local Account data storage. }
    function SaveToCode:RtcByteArray;
    { Register Accounts stored in "code" (generated using "SaveToCode" method).
      Permissions will be restored only if "withPermissions=True".
      Accounts which were active when stored will be activated now ONLY if "activate=True". }
    procedure RegisterFromCode(const code:RtcByteArray; withPermissions,activate:boolean);

    { Acquire exclusive access to Accounts data.
      NOTE: All methods and properties provided by this component are
      thread-safe and may be used from a multi-threaded Application,
      without any special safety precautions. "Acquire" and "Release" methods
      are provided ONLY in case longer access to the Accounts data is needed
      and changes to the Accounts data should NOT be allowed during access. }
    procedure Acquire;
    { Release exclusive access to Accounts data
      (used in combination with "Acquire"). }
    procedure Release;

    { Get the First Account ID.
      Returns empty String if there are no Accounts. }
    function FirstID:RtcString;
    { Get the Last Account ID.
      Returns empty String if there are no Accounts. }
    function LastID:RtcString;
    { Get the Next Account ID after "id".
      Returns empty string if there are no Accounts after "id". }
    function NextID(const id:RtcString):RtcString;
    { Get the Previous Account ID before "id".
      Returns empty string if there are no Accounts before "id". }
    function PrevID(const id:RtcString):RtcString;
    // Delete Account "id"
    procedure DeleteID(const id:RtcString);
    // Delete all Accounts
    procedure DeleteAll;
    // Total Number of Accounts stored in this Account Manager
    function TotalCount:integer;
    // Number of Accounts currently Active
    function ActiveCount:integer;

    { Get the First Client component linked to this Account Manager.
      Returns NIL if there are no linked Client components.
      Acquires a Lock on the Client (can't be destroyed when locked) if AcquireLock=True.
      If called with AcquireLock=True,
         use UnLockClient(Client:TRtcHttpGateClient) to release lock on the Client.  }
    function FirstClient(AcquireLock:boolean=False):TRtcHttpGateClient;
    { Get the Last Client component linked to this Account Manager.
      Returns NIL if there are no linked Client components.
      Acquires a Lock on the Client (can't be destroyed when locked) if AcquireLock=True.
      If called with AcquireLock=True,
         use UnLockClient(Client:TRtcHttpGateClient) to release lock on the Client.  }
    function LastClient(AcquireLock:boolean=False):TRtcHttpGateClient;
    { Get the Next Client component linked to this Account Manager after "cli".
      Returns NIL if there are no linked Client components after "cli".
      Acquires a Lock on the Client (can't be destroyed when locked) if AcquireLock=True.
      If called with AcquireLock=True,
         use UnLockClient(Client:TRtcHttpGateClient) to release lock on the Client.  }
    function NextClient(const cli:TRtcHttpGateClient; AcquireLock:boolean=False):TRtcHttpGateClient;
    { Get the Previous Client component linked to this Account Manager before "cli".
      Returns NIL if there are no linked Client components before "cli".
      Acquires a Lock on the Client (can't be destroyed when locked) if AcquireLock=True.
      If called with AcquireLock=True,
         use UnLockClient(Client:TRtcHttpGateClient) to release lock on the Client.  }
    function PrevClient(const cli:TRtcHttpGateClient; AcquireLock:boolean=False):TRtcHttpGateClient;
    // Total Number of Client components linked to this Account Manager
    function TotalClients:integer;

    { Is "UserID" on "GateAddr:GatePort" used by one of the Clients managed by this component? }
    function IsMyUserID(UserID:TGateUID; const GateAddr,GatePort:RtcString):boolean;

    { Is "UserAddr" (UserID@GateAddr:GatePort) used by one of the Client managed by this component? }
    function IsMyUserAddr(const UserAddr:RtcString):boolean;

    { Find the Client component linked to this Account Manager which is
      set up to connect to the Gateway at "GateAddr:GatePort" and uses
      (or can communicate with) the specified "UserID".
      Returns NIL if no Client component matches the search criteria.
      Acquires a Lock on the Client (can't be destroyed when locked) if AcquireLock=True.
      If called with AcquireLock=True,
         use UnLockClient(Client:TRtcHttpGateClient) to release lock on the Client.  }
    function FindClient(UserID:TGateUID; const GateAddr,GatePort:RtcString; AcquireLock:boolean=False):TRtcHttpGateClient; overload;

    { Find the Client component linked to this Account Manager
      set up to connect to the Gateway at "GateAddr:GatePort".
      Returns NIL if no Client component matches the search criteria.
      Acquires a Lock on the Client (can't be destroyed when locked) if AcquireLock=True.
      If called with AcquireLock=True,
         use UnLockClient(Client:TRtcHttpGateClient) to release lock on the Client.  }
    function FindClient(const GateAddr,GatePort:RtcString; AcquireLock:boolean=False):TRtcHttpGateClient; overload;

    { Find the Client component linked to this Account Manager which currently uses
      (or can communicate with) the specified "UserAddr" (UserID @ GateAddr : GatePort).
      Returns NIL if no Client component matches the search criteria.
      Acquires a Lock on the Client (can't be destroyed when locked) if AcquireLock=True.
      If called with AcquireLock=True,
         use UnLockClient(Client:TRtcHttpGateClient) to release lock on the Client.  }
    function FindClient(const UserAddr:RtcString; AcquireLock:boolean=False):TRtcHttpGateClient; overload;

    { Find MY UserAddr (MyUID @ GateAddr : GatePort) on the Client component linked to this Account Manager
      which currently uses (or can communicate with) the specified "UserAddr" (UserID @ GateAddr : GatePort).
      Returns '' (empty string) if no Client component matches the search criteria. }
    function FindMyUserAddr(const UserAddr:RtcString):RtcString;

    { Release a Lock on the "Client" previously acquired with "AcquireLock=True" using one of the
      following methods: "FindClient", "FirstClient", "LastClient", "NextClient" or "PrevClient".
      After releasing the Client lock, set the "Client" variable to NIL (to avoid accidental use). }
    procedure UnLockClient(Client:TRtcHttpGateClient);

    // Returns the Account "ID" currently using the specified "groupID"
    // Returns an empty String if the Group ID is NOT in use by an Account stored here
    function FindAccountID(groupID:TGateUID):RtcString;

    // Returns the GroupID currently in use by the specified Account "id"
    // Returns zero (0) if the Account "id" is NOT active, or if it does NOT exist
    function FindGroupID(const id:RtcString):TGateUID;

    { Find Public Account generated using "DisplayName".
      If the Account does NOT exist, returns an empty string.  }
    function FindPublic(const DisplayName:RtcWideString):RtcString;

    { Find the Private Account used to calculate a MD5Code8Bit encoded "LinkID".
      If the Account does NOT exist, returns an empty string.  }
    function FindPrivate(const MD5Code8BitLinkID:RtcString):RtcString;

    { Find the Account used to calculate a MD5Code8Bit encoded "ID" or "LinkID".
      If the Account does NOT exist, returns an empty string.  }
    function FindLink(const MD5Code8BitID: RtcString): RtcString;

    { Register a new Public Account with "DisplayName".
      If "activate=True", the account will be activated immediately.
      Returns Account ID. isType[id] = gat_Public.
      Account ID is the "Channel" used with TRtcHttpGateClient's "Subscribe" method.
      If Account with "id" already exists, it will be replaced. }
    function RegisterPublic(const DisplayName:RtcWideString; activate:boolean):RtcString;

    { Generate a new Private Account with "DisplayName".
      Returns Account ID. isType[id] = gat_Private.
      If "activate=True", the account will be activated immediately.
      Account ID is the "Channel" used with TRtcHttpGateClient's "Subscribe" method.
      If Account with "id" already exists, it will be replaced. }
    function GeneratePrivate(const DisplayName:RtcWideString; activate:boolean):RtcString;

    { Prepare a complete Copy of the Account "id" for remote Transfer (including Private data).
      Returns "Data" for use with "RegisterAccount" remotely. }
    function PrepareCopy(const id:RtcString; withPermissions:boolean):RtcByteArray;
    { Prepare a Link to the Account "id" for remote Registration (as "gat_Linked" in remote Account Manager).
      Returns "Data" for use with "RegisterAccount" remotely. }
    function PrepareLink(const id:RtcString):RtcByteArray;

    { Check Account from "Data" prepared with "PrepareCopy" or "PrepareLink" method.
      Returns "DisplayName" if data check was successful.
      Raises an Exception if "Data" is corrupt. }
    function CheckAccount(const Data:RtcByteArray):RtcWideString;

    { Register Account from "Data" prepared with a "PrepareCopy" or "PrepareLink" method.
      If "withPermissions=True", permissions from the remote Account will also be added locally.
      If "activate=True", the account will be activated immediately.
      Returns Account ID. isType[id] = gat_Private, gat_Linked or gat_Public (depending on "Data").
      Account ID is the "Channel" used with TRtcHttpGateClient's "Subscribe" method.
      If an Account with the "id" from "Data" already exists, the old Account will be replaced.
      Raises an Exception if "Data" is corrupt. }
    function RegisterAccount(const Data:RtcByteArray; withPermissions, activate:boolean):RtcString;

    { Make a Private Signature for remote verification by a linked account (ONLY "gat_Private" Accounts):
      "id" = Private Account ID used for signing (isType[id] = "gat_Private"),
      "content" = content used by both sides for signature verification.
      * Returns the "Signature" for use with "VerifySignature" by a linked account. }
    function MakeSignature(const id:RtcString; const content:RtcByteArray):RtcByteArray;
    { Verify remote Private Account Signature (ONLY "gat_Linked"  Accounts):
      "id" = Account ID used for verification (isType[id] = "gat_Linked"),
      "content" = content used by both sides for signature verification.
      * Returns TRUE if the Signature is correct. }
    function VerifySignature(const id:RtcString; const content:RtcByteArray; const Signature:RtcByteArray):boolean;

    { Encrypt package for secure transfer to a remote Private Account (ONLY "gat_Linked" Accounts):
      "id" = Link account ID used for encryption (isType[id] = "gat_Linked"),
      "content" = content to be encrypted, so that only the linked Private account can decrypt it.
      * Returns the encrypted content, which can only be decrypted with "PrivateDecrypt" on the Private account. }
    function PrivateEncrypt(const id:RtcString; const content:RtcByteArray):RtcByteArray;
    { Decrypt package received from a remote account Link (ONLY "gat_Private" Accounts):
      "id" = Private account ID to be used for decryption (isType[id] = "gat_Private"),
      "content" = content to be decrypted (encrypted using "PrivateEncrypt").
      * Returns the original content (decrypted). }
    function PrivateDecrypt(const id:RtcString; const content:RtcByteArray):RtcByteArray;

    { Encrypt package using the provided (Public) Key:
      "key" = PUBLIC Key to be used for encryption
              (previously received from the recipient of the encrypted content),
      "content" = content which has to be encrypted using the Public KEY
                  (to be sent to the user who sent us the Public Key),
      * Returns the encrypted content, which can only be decrypted using
        the "PublicDecrypt" method by the Public Key "owner". }
    function PublicEncrypt(const key:RtcByteArray; const content:RtcByteArray):RtcByteArray;
    { Decrypt package (content received from a remote account) encrypted with OUR "PublicKey" :
      "content" = content which was previously encrypted using our "PublicKey" and has to be decrypted now.
      * Returns the original content (decrypted). }
    function PublicDecrypt(const content:RtcByteArray):RtcByteArray;

    { PUBLIC Key, which can be sent to anyone when secure communication is required.
      Using this Public Key, the recipient can encrypt any content with its "PublicEncrypt" method and the
      ONLY way to decrypt that encrypted content is by using the "PublicDecrypt" method on this component.
      Public Key is generated automatically on first access and never changes, as long as the component exists.
      Public Key is "temporary" and will be different for every TRtcGateAccountManager component created. }
    function PublicKey:RtcByteArray;

    { Public ID is automatically generated on first access and never changes, as long as the component exists.
      It will be different for every "TRtcGateAccountManager" component created, so it can be used to uniquely
      identify this "Client" (Application instance) on all Gateways, without giving away user-specific info. }
    function PublicID:RtcString;

    { "AuthCode" generated on-demand and valid for "RTC_AUTHCODE_VALID" seconds (default=90).
      When the last "AuthCode" returned by this method becomes invalid (use the "AuthCodeValid" method
      to check how many seconds before that happens), this method will generate and return a new code. }
    function AuthCode:RtcString;

    { Number of seconds before the current "AuthCode" becomes invalid.
      If this method returns 0, the last code returned by "AuthCode" will expire in less than a second.
      If this method returns -1, the last code returned by "AuthCode" has already expired and a new
      code will be generated by the "AuthCode" method, the next time it is used. }
    function AuthCodeValid:integer;

    { Generate "AuthCode" and reset the "AuthCodeValid" expiration timer. }
    procedure AuthCodeReset;

    { Type of the Account "id".
      Returns "gat_None" if there is no Account with this ID. }
    property isType[const id:RtcString]:TRtcGateAccountType read GetType;
    { Is the Account Active?
      Active Accounts receive "gc_AccountLogIn" and "gc_AccountLogOut" info for all
      account types, as well as "gc_AccountVerified" or "gc_AccountBadVerify" info
      for "Private" and "Linked" accounts (after remote account verification).
      When the Account is Active, notifications are sent to all TRtcHttpGateClient components
      linked to this component to subscribe the Account on all the connected Gateways.
      When the Account changes from Active to Inactive, notifications are sent to
      all TRtcHttpGateClient components to unsubscribe the Account from their Gateways. }
    property isActive[const id:RtcString]:boolean read GetActive write SetActive;
    { Is Account "id" allowed to perform Action "action"?
      Returns FALSE if the "action" is not allowed by Account "id", or if the Account "id" does not exist. }
    property isAllowed[const id:RtcString; const action:word]:boolean read GetAllowed write SetAllowed;

    { Human-friendly Display Name for Account "id".
      Display Name is a combination of "LocalName" and "RemoteName".
      Returns empty string if there is no account with this "id". }
    property DisplayName[const id:RtcString]:RtcWideString read GetDisplayName write SetDisplayName;

    { Human-friendly Local Display Name for Account "id".
      Returns empty string if there is no account with this "id". }
    property LocalName[const id:RtcString]:RtcWideString read GetLocalName write SetLocalName;

    { Human-friendly Remote Display Name for Account "id".
      Returns empty string if there is no account with this "id". }
    property RemoteName[const id:RtcString]:RtcWideString read GetRemoteName write SetRemoteName;

    { Get or Set all Permissions for Account "id" as an array of words, containing all *allowed* actions.
      Returns an empty array if no actions are allowed for account "id", or if the account does not exist. }
    property Permissions[const id:RtcString]:TRtcGatePermissions read GetPermissions write SetPermissions;

    {  Shared Group ID Management. All components using this Multi-Gate-Client component to
       communicate through one or more Gateways with a Group of Users HAVE TO use this property
       to manage Group IDs (allocate and deallocate), to avoid using the same ID for different things.

       The ONLY exception to this rule are "MyGroupID" provided by "TRtcAbsGateClientLink"
       and "TRtcAbsMultiGateClientLink" components, since these Group IDs are automatically
       allocated and de-allocated internally by using the "SharedGroups" property. }
    property SharedGroups:TRtcGateUserGroupIDs read FSharedGroups;

  published
    { Min GroupID allowed for use by this Account Manager (Default=1).
      One unique GroupID is required for each Active Account.
      If the Account Manager runs out of GroupIDs, new Accounts can NOT be
      activated until one of the already active accounts are deactivated. }
    property MinGroupID:TGateUID read FMinGroupID write SetMinGroupID default MinGID;

    { Max GroupID allowed for use by this Account Manager (Default=4095).
      One unique GroupID is required for each Active Account.
      If the Account Manager runs out of GroupIDs, new Accounts can NOT be
      activated until one of the already active accounts are deactivated. }
    property MaxGroupID:TGateUID read FMaxGroupID write SetMaxGroupID default MaxGID;

    { You would like some events triggered by THIS component to be executed asynchronously,
      either inside a separate Backgrdound Thread or asynchronously from the Main Thread?

      Assign a "TRtcGCThread" component to the "BackThread" property, implement events on the
      "BackThread" component which you want executed asynchronously from the Backround Thread
      or asynchronously from the Main Thread (GUI), then leave these events without implementation
      on THIS component, or implement events on THIS component and set "WantBackThread:=True" from
      inside any event on THIS component if you want the same event posted for asynchonous
      execution on "BackThread" after events on THIS component finish executing.

      If an event is NOT implementated on THIS component, but an event with the same name
      is implemented on the "Thread" component, event on the "Thread" component will automatically
      be posted for (asynchronous) execution after the "GUI" event (optional) finishes.

      For example, if you want the "AfterLoggedIn" event on the "Thread" component executed
      asynchronously after the user logs in, you can either set "WantBackThread:=True" from the
      "AfterLoggedIn" event on THIS component, or leave THIS components "AfterLoggedIn" event
      unassigned (NOT implemented). This rule applies to all events with a "WantBackThread" parameter. }
    property BackThread:TRtcGCThread read FThread write FThread;

    { Automatically request verifications for all "gat_Linked" accounts (remote Private) when they log in?
      As an alternative (less traffic on login), you can make manual calls to
      the "RequestAccountVerification" or "RequestUserVerifications" methods on the
      "TRtcHttpGateClient" component when access to User Account Permissions is "expected". }
    property AutoVerifyAccounts:boolean read FVerifyAccounts write FVerifyAccounts default False;

    { Event called BEFORE the user Logs in to the Gateway. It can be used for custom component setup.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "BeforeLoginGUI" event will be executed from the Main Thread after this event. }
    property BeforeLogIn:TRtcGateClientStateEvent read FConBeforeLogIn write FConBeforeLogIn;

    { Event called after the Client logs in to the Gateway.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLoggedInGUI" event will be executed from the Main Thread after this event. }
    property AfterLoggedIn:TRtcGateClientStateEvent read FConAfterLoggedIn write FConAfterLoggedIn;

    { Event called if the last Login attempt has failed.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLoginFailGUI" event will be executed from the Main Thread after this event. }
    property AfterLoginFail:TRtcGateClientStateEvent read FConAfterLoginFail write FConAfterLoginFail;

    { Event called if the Client has been logged out of the Gateway.
      This can either be because the Client has permanently lost connection
      and the UserID belonging to this Client is no longer valid,
      or because the Active property was set to FALSE to close the connection.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLogOutGUI" event will be executed from the Main Thread after this event. }
    property AfterLogOut:TRtcGateClientStateEvent read FConAfterLogOut write FConAfterLogOut;

    { Data was received from the Gateway. Use provided parameters to check what was received
      and set "Wanted:=TRUE" if THIS components "OnDataReceived" event should be called.
      If "OnDataFilter" event is NOT implemented, "OnDataReceived" and/or "OnDataRecievedGUI" event is called for ALL data received.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread. }
    property OnDataFilter:TRtcGateClientFilterEvent read FOnDataFilter write FOnDataFilter;

    { Info was received from the Gateway. Use provided parameters to check what was received
      and set "Wanted:=TRUE" if THIS components "OnInfoReceived" event should be called.
      If "OnInfoFilter" event is NOT implemented, "OnInfoReceived" event is called for ALL info received.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread. }
    property OnInfoFilter:TRtcGateClientFilterEvent read FOnInfoFilter write FOnInfoFilter;

    { Received data from the Gateway. Triggered ONLY if the "OnDataFilter" event
      is NOT implemented or if the "OnDataFilter" event returns with "Wanted=True".
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnDataReceivedGUI" event will be executed from the Main Thread after this event. }
    property OnDataReceived:TRtcGateClientDataEvent read FConDataReceived write FConDataReceived;

    { Received info from the Gateway. Triggered ONLY if the "OnInfoFilter" event
      is NOT implemented or if the "OnInfoFilter" event returns with "Wanted=True".
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnInfoReceivedGUI" event will be executed from the Main Thread after this event. }
    property OnInfoReceived:TRtcGateClientDataEvent read FConInfoReceived write FConInfoReceived;

    { Client is again ready to send data to the Gateway after the last Stream Reset.
      Use this event to re-invite Users to your Groups and fix broken User relations.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnReadyAfterResetGUI" event will be executed from the Main Thread after this event. }
    property OnReadyAfterReset:TRtcGateClientStateEvent read FConReadyAfterReset write FConReadyAfterReset;

    { Client is ready to send more data to the Gateway. Use this event to implement
      the process of sending larger data "streams" by breaking them up in smaller packets.
      Keep in mind that the maximum single packet size is 16 MB.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnReadyToSendGUI" event will be executed from the Main Thread after this event. }
    property OnReadyToSend:TRtcGateClientStateEvent read FConReadyToSend write FConReadyToSend;

    { Input and Output Streams have been reset, either because the "ResetStreams"
      method was called, or because Clients connection to the Gateway was lost.
      A new connection attempt will be made after this event.
      Use this event for cleaning up Group Status information.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnStreamResetGUI" event will be executed from the Main Thread after this event. }
    property OnStreamReset:TRtcGateClientStateEvent read FConStreamReset write FConStreamReset;

    { Event called from the Main (GUI) Thread, synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "BeforeLogIn" event is NOT implemented or returns with "WantGUI=True". }
    property BeforeLogInGUI:TRtcGateClientGUIStateEvent read FGuiBeforeLogIn write FGuiBeforeLogIn;

    { Event called from the Main (GUI) Thread, synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLoggedIn" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLoggedInGUI:TRtcGateClientGUIStateEvent read FGuiAfterLoggedIn write FGuiAfterLoggedIn;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLoginFail" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLoginFailGUI:TRtcGateClientGUIStateEvent read FGuiAfterLoginFail write FGuiAfterLoginFail;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLogOut" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLogOutGUI:TRtcGateClientGUIStateEvent read FGuiAfterLogOut write FGuiAfterLogOut;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnDataReceived" event is NOT implemented or returns with "WantGUI=True". }
    property OnDataReceivedGUI:TRtcGateClientGUIDataEvent read FGuiDataReceived write FGuiDataReceived;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnInfoReceived" event is NOT implemented or returns with "WantGUI=True". }
    property OnInfoReceivedGUI:TRtcGateClientGUIDataEvent read FGuiInfoReceived write FGuiInfoReceived;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnReadyAfterReset" event is NOT implemented or returns with "WantGUI=True". }
    property OnReadyAfterResetGUI:TRtcGateClientGUIStateEvent read FGuiReadyAfterReset write FGuiReadyAfterReset;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnReadyToSend" event is NOT implemented or returns with "WantGUI=True". }
    property OnReadyToSendGUI:TRtcGateClientGUIStateEvent read FGuiReadyToSend write FGuiReadyToSend;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnStreamReset" event is NOT implemented or returns with "WantGUI=True". }
    property OnStreamResetGUI:TRtcGateClientGUIStateEvent read FGuiStreamReset write FGuiStreamReset;
    end;

  TRtcGateLoginStatus=(gls_Offline, gls_Retry, gls_Relogin, gls_Login, gls_Connect, gls_Active);

  { @Abstract(RTC Gate HTTP Client component)
    HTTP Client component for communication with a RTC Gateway.

    This component has two (2) TRtcHttpClient components internally
    for handling output and input data streams, each running in
    Multi-Threaded mode and using one virtual thread per connection.

    All events on this component are called from within a thread context of the
    connection component triggering each event, which means that components events
    will be called from multiple threads (depending on what triggered the event) and
    all the code you write for handling this components events should be thread-safe. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcHttpGateClient = class(TRtcAbsGateClient)
  private
    ClientOUT: TRtcHttpClient;
    ClientIN: TRtcHttpClient;
    ClientDummy: TRtcHttpClient;

    DataLogin1: TRtcDataRequest;
    DataLogin2: TRtcDataRequest;
    DataIN: TRtcDataRequest;
    DataOUT: TRtcDataRequest;

    FWriteTime,
    FWriteDelta,
    FWriteSpeed,
    FWriteMax,
    FWriteCount:int64;
    FWriteLimit:Cardinal;

    FThread: TRtcGCThread;

    FGateFileName,
    FGATEURI_PING,
    FGATEURI_LOGIN,
    FGATEURI_INPUT,
    FGATEURI_OUTPUT,
    FGATEURI_INPUTRESET,
    FGATEURI_OUTPUTRESET,

    FGATE_PRIMARY_KEY,
    FGATE_SECONDARY_KEY,
    FGATE_USERAUTH,
    FGATE_USERINFO: RtcString;

    FAutoRetry:integer;
    FNeedLogin:boolean;
    FAutoRelogin:boolean;

    FData: TRtcGateClientData;

    FUseBlocking: boolean;
    FUseWinHTTP: boolean;
    FUseProxy: boolean;
    FUseSSL: boolean;

    FUserLogin: TRtcUserLoginInfo;
    FCryptPlugin:TRtcCryptPlugin;

    FWasReset: boolean;
    FBlocking: boolean;

    FOnDataFilter: TRtcGateClientFilterEvent;
    FOnInfoFilter: TRtcGateClientFilterEvent;
    FConDataReceived: TRtcGateClientDataEvent;
    FConInfoReceived: TRtcGateClientDataEvent;
    FConAfterLogOut: TRtcGateClientStateEvent;
    FConReadyToSend: TRtcGateClientStateEvent;
    FConAfterLoginFail: TRtcGateClientStateEvent;
    FConAfterLoggedIn: TRtcGateClientStateEvent;
    FConReadyAfterReset: TRtcGateClientStateEvent;
    FConBeforeLogIn: TRtcGateClientStateEvent;
    FConStreamReset: TRtcGateClientStateEvent;

    FGuiDataReceived: TRtcGateClientGUIDataEvent;
    FGuiInfoReceived: TRtcGateClientGUIDataEvent;
    FGuiAfterLogOut: TRtcGateClientGUIStateEvent;
    FGuiReadyToSend: TRtcGateClientGUIStateEvent;
    FGuiAfterLoginFail: TRtcGateClientGUIStateEvent;
    FGuiAfterLoggedIn: TRtcGateClientGUIStateEvent;
    FGuiReadyAfterReset: TRtcGateClientGUIStateEvent;
    FGuiBeforeLogIn: TRtcGateClientGUIStateEvent;
    FGuiStreamReset: TRtcGateClientGUIStateEvent;

    FGateAddr: RtcString;
    FGatePort: RtcString;
    FGateIPV: RtcIPV;

    FStreamBlockSizeIn,
    FStreamBlockSizeOut:Cardinal;

    CS,ECS:TRtcCritSec;
    FEV:TRtcEvent;

    FPingTimer:TRtcTimer;
    FPingJob:TRtcJob;

    FBuffRcv:TObjList;

    FMyIP:RtcString;
    FMyKey:RtcString;
    FMyUID:TGateUID;

    FCryIn,
    FCryOut:TRtcCrypt;

    FBuffIn,
    FBuffOut:TRtcHugeByteArray;

    FLoginStatus:TRtcGateLoginStatus;
    FReqToSend:Cardinal;

    FNeedInput:integer;
    FNeedOutput:boolean;

    FInputReset,
    FOutputReset:integer;

    FDataWaiting,
    FSpaceToSend,
    FNowSending,
    FInputWasLost,
    FOutputWasLost:boolean;

    FInsideEventIN,
    FInsideEventOUT,
    FInsideEventDummy,
    FInsideEventMain,
    FConnCount:integer;

    FGateClientLinks:TRtcGateClientLinkList;

    FUserGroups: TRtcGateUserGroupStates;
    FSharedGroups: TRtcGateUserGroupIDs;

    FState: TRtcGateClientStateInfo;

    FAccounts:TRtcGateAccountManager;

    FUsers,
    FGroups:tObjList;

    function Login_Status:TRtcGateLoginStatus;

  protected

    // @exclude
    procedure StartPingTimer;

    // @exclude
    procedure PausePingTimer;

    // @exclude
    procedure StopPingTimer;

    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    function GetSharedGroups: TRtcGateUserGroupIDs;

    // @exclude
    procedure SetAccounts(const Value: TRtcGateAccountManager);

    // @exclude
    function GetAutoRelogin: boolean;
    // @exclude
    procedure SetAutoRelogin(const Value: boolean);

    // @exclude
    procedure AddGateClientLink(Value:TRtcAbsGateClientLink);
    // @exclude
    procedure RemoveGateClientLink(Value:TRtcAbsGateClientLink);
    // @exclude
    procedure RemoveAllGateClientLinks;

    // @exclude
    procedure DataLogin1BeginRequest(Sender: TRtcConnection);
    // @exclude
    procedure DataLogin1DataReceived(Sender: TRtcConnection);
    // @exclude
    procedure DataLogin2BeginRequest(Sender: TRtcConnection);
    // @exclude
    procedure DataLogin2DataReceived(Sender: TRtcConnection);
    // @exclude
    procedure DataResponseAbort(Sender: TRtcConnection);
    // @exclude
    procedure DataINBeginRequest(Sender: TRtcConnection);
    // @exclude
    procedure DataINDataReceived(Sender: TRtcConnection);
    // @exclude
    procedure DataINDataIn(Sender: TRtcConnection);
    // @exclude
    procedure DataINDataOut(Sender: TRtcConnection);
    // @exclude
    procedure DataOUTBeginRequest(Sender: TRtcConnection);
    // @exclude
    procedure DataOUTDataReceived(Sender: TRtcConnection);
    // @exclude
    procedure DataOUTDataSent(Sender: TRtcConnection);
    // @exclude
    procedure DataOUTDataOut(Sender: TRtcConnection);
    // @exclude
    procedure DataOUTDataIn(Sender: TRtcConnection);

    // @exclude
    procedure ClientBeforeDestroy(Sender: TRtcConnection);
    // @exclude
    procedure ClientAfterDestroy(Sender: TRtcConnection);

    // @exclude
    function AllClientsReleased:boolean;

    // @exclude
    procedure SetGateUserAuth(const Value: RtcString);
    // @exclude
    procedure SetGateUserInfo(const Value: RtcString);
    // @exclude
    procedure SetGateFileName(const Value: RtcString);
    // @exclude
    procedure SetGatePrimaryKey(const Value: RtcString);
    // @exclude
    procedure SetGateSecondaryKey(const Value: RtcString);

    // @exclude
    procedure InitInput;
    // @exclude
    procedure InitOutput;

    // @exclude
    procedure User_LogIn(Sender:TRtcConnection);
    // @exclude
    procedure User_LogOut(Sender:TRtcConnection);
    // @exclude
    procedure UserStreamLost(Sender:TRtcConnection);
    // @exclude
    procedure UserStreamSoftReset;
    // @exclude
    procedure UserStreamHardReset;

    // @exclude
    function ReadyToSend(Sender:TRtcConnection):boolean;
    // @exclude
    procedure ReadyToSendNow(Sender:TRtcConnection);

    // @exclude
    function ProcessInput(Sender:TRtcConnection):int64;

    // @exclude
    procedure PingCheck;

    // @exclude
    procedure Call_BeforeLogIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLoggedIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLoginFail(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLogOut(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnDataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnInfoReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnReadyToSend(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnReadyAfterReset(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnStreamReset(Sender:TRtcConnection); override;

    // @exclude
    procedure SetGateAddr(const Value: RtcString);
    // @exclude
    procedure SetGatePort(const Value: RtcString);
    // @exclude
    procedure SetGateIPV(const Value: RtcIPV);

    // @exclude
    function GetActive: boolean;
    // @exclude
    function GetReady: boolean;
    // @exclude
    procedure SetActive(const Value: boolean);

    // @exclude
    function GetMyAddr: RtcString;

    // @exclude
    function GetStreamBlockSizeIn: Cardinal;
    // @exclude
    function GetStreamBlockSizeOut: Cardinal;
    // @exclude
    procedure SetStreamBlockSizeIn(const Value: Cardinal);
    // @exclude
    procedure SetStreamBlockSizeOut(const Value: Cardinal);

    // @exclude
    procedure SetUseProxy(const Value: boolean);
    // @exclude
    procedure SetUseSSL(const Value: boolean);
    // @exclude
    procedure SetUseWinHTTP(const Value: boolean);
    // @exclude
    procedure SetUseBlocking(const Value: boolean);

    // @exclude
    procedure SetCryptPlugin(const Value: TRtcCryptPlugin);
    // @exclude
    procedure SetUserLogin(const Value: TRtcUserLoginInfo);

    // @exclude
    procedure RetryLogin;

    // @exclude
    procedure Call_AccountChange(const id:RtcString; action:TRtcGateAccountAction);

    // @exclude
    function AddUserToAccount(const uid,gid:TGateUID; const acc, uinfo:RtcString; verified:boolean):boolean;
    // @exclude
    function RemoveUserFromAccount(const uid:TGateUID; const acc:RtcString):boolean;
    // @exclude
    function CleanUpUserInfo(const uid:TGateUID):boolean;
    // @exclude
    function RemoveAccountVerification(const uid:TGateUID; const acc:RtcString):boolean;

    // @exclude
    function CheckNotVerifiedAccount(const UserID:TGateUID; const AccountID:RtcString):boolean;
    // @exclude
    function CheckNotVerifiedUser(const UserID:TGateUID):boolean;

    // @exclude
    function UserAddedToMyGroup(const GroupID,UserID:TGateUID):boolean;
    // @exclude
    function UserRemovedFromMyGroup(const GroupID,UserID:TGateUID):boolean;
    // @exclude
    function UsersGroupClosed(const GroupID:TGateUID):boolean;

    // @exclude
    function CheckUserInfo(const UserID:TGateUID):RtcString;
    // @exclude
    function CheckUserInAccount(const UserID:TGateUID; const AccountID:RtcString):boolean;
    // @exclude
    function CheckMaybeInAccount(const UserID:TGateUID; const AccountID:RtcString):boolean;
    // @exclude
    function CheckUserInVerifiedAccount(const UserID:TGateUID; const AccountID:RtcString):boolean;

    // @exclude
    function GetVerifiedAllowed(const UserID:TGateUID; const action:word):boolean;
    // @exclude
    function GetMaybeAllowed(const UserID:TGateUID; const action:word):boolean;
    // @exclude
    function GetUserAllowed(const UserID:TGateUID; const action:word):boolean;

    // @exclude
    procedure EnterEvent(Sender:TRtcConnection);
    // @exclude
    procedure LeaveEvent(Sender:TRtcConnection);

    // @exclude
    procedure BeforeWrite(const Len:int64); virtual;

  public

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    // Reset Input and Output data streams (reset both connections)
    // If "HardReset=True", Log OUT will be used instead of a soft reset
    procedure ResetStreams(HardReset:boolean=False);

    { Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to "UserID" and "GroupID" (0..4095). @html(<br><br>)

      To send data to one recipient, call this method with UserID=receiverID and GroupID=optGroupID (GroupID is optional).
      When data is sent to one user, the user will receive "Data" with UserID=MyUID and ToGroupID=optGroupID. @html(<br><br>)

      To send data to a User Group created by us, call this method with UserID=MyUID and GroupID=MyGroupID.
      When data is sent to a Group, all recipients will get "Data" with UserID=MyUID and GroupID=MyGroupID. @html(<br><br>)

      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000,
      they will be used in future updates to extend the components with new features. }
    function SendBytes(const UserID, GroupID:TGateUID; const CallID:word; const data:RtcByteArray):boolean; overload;

    { Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to "UserID" and "GroupID" (0..4095). @html(<br><br>)

      To send data to one recipient, call this method with UserID=receiverID and GroupID=optGroupID (GroupID is optional).
      When data is sent to one user, the user will receive "Data" with UserID=MyUID and ToGroupID=optGroupID. @html(<br><br>)

      To send data to a User Group created by us, call this method with UserID=MyUID and GroupID=MyGroupID.
      When data is sent to a Group, all recipients will get "Data" with UserID=MyUID and GroupID=MyGroupID. @html(<br><br>)

      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000,
      they will be used in future updates to extend the components with new features. }
    function SendBytes(const UserID, GroupID:TGateUID; const CallID:word; const data:RtcString):boolean; overload;

    { Send "CallID" (0..65000) with no extra content to "UserID" and "GroupID" (0..4095) through the Gateway. @html(<br><br>)

      To send the CallID to one recipient, call this method with UserID=receiverID and GroupID=optGroupID (GroupID is optional).
      When CallID is sent to one user, the user will receive "Data" with UserID=MyUID and ToGroupID=optGroupID. @html(<br><br>)

      To send CallID to a User Group created by us, call this method with UserID=MyUID and GroupID=MyGroupID.
      When CallID is sent to a Group, all recipients will get "Data" with UserID=MyUID and GroupID=MyGroupID. @html(<br><br>)

      You can use any CallID from 0 - 65000, but do NOT use IDs above 65000,
      they will be used in future updates to extend the components with new features. }
    function SendBytes(const UserID, GroupID:TGateUID; const CallID:word):boolean; overload;

    // Ping user "UserID" (through the Gateway)
    function PingUser(const UserID:TGateUID):boolean;

    // Add User "UserID" to group "GroupID" owned by user "OwnerID" (on the Gateway)
    // If "Managed=True" and "OwnerID=MyUID", User will be added to "My Groups"
    function AddUserToGroup(const OwnerID, GroupID, UserID:TGateUID; Managed:boolean=False):boolean;

    // Remove User "UserID" from group "GroupID" owned by user "OwnerID"  (on the Gateway)
    function RemUserFromGroup(const OwnerID, GroupID, UserID:TGateUID):boolean;

    // Remove Group "GroupID" owned by user "OwnerID" (on the Gateway)
    function RemoveGroup(const OwnerID, GroupID:TGateUID):boolean;

    { Add User "UserID" as our Friend, giving the User
      permission to add us to his User Group(s) on the Gateway.
      If the user "UserID" is online and was NOT already in our Friends list,
      we will receive the "gc_FriendAdd" command from the Gateway and
      the user "UserID" will receive the "gc_BeFriend" command.
      If the user is off-line, we will receive a "gc_UserOffLine" command.
      If the user is already in our friends list, nothing changes and
      the Gateway won't send any notification messages to us or the user. }
    function AddFriend(const UserID:TGateUID):boolean;

    { Remove User "UserID" from our Friends list, revoking Users
      permissions to add us to his User Group(s) on the Gateway.
      If user "UserID" was in our Friends list on the Gateway,
      we will receive the "gc_FriendRemove" command from the Gateway
      and the user "UserID" will receive the "gc_UnFriend" command.
      Removing a User from our Friends list will not close open User Groups,
      but it will prevent the User from adding us to more Groups.
      NOTE: Removing Friends is not necessary, because our Frields list will
      be cleared on the Gateway if a User logs out or looses connection. }
    function RemoveFriend(const UserID:TGateUID):boolean;

    { Remove all Users from our Friends list on the Gateway, revoking all
      Users permissions for adding us to their User Group(s) on the Gateway.
      If our Friends list on the Gateway wasn't empty, we will receive
      the "gc_FriendRemove" command for every user removed from our list
      and all removed users will receive the "gc_UnFriend" command.
      Removing Users from our Friends list will not close open User Groups,
      but it will prevent Users from adding us to more Groups.
      NOTE: Removing Friends is not necessary, because our Frields list will
      be cleared on the Gateway if a User logs out or looses connection. }
    function RemoveAllFriends:boolean;

    { Subscribe to channel "Channel" with (optional) Group ID "GroupID" (0..4095). @html(<br><br>)

      There are two types of "Channels": @html(<br><br>)

      1. Public channels, where the "Channel" parameter is calculated
      from a freely chosen text using the "rtcMakePublicKey" function (rtcKeyHash unit).@html(<br><br>)

      In Public channels, there is no difference between Hosts and Listeners/Controls. All Users are Equal.
      For a user to join a PUBLIC channel, the "Channel" parameter is calculated using the "rtcMakePublicKey"
      function, where the name of the Public channel (like "Lobby") is used as the "Prefix" parameter. @html(<br><br>)

      In Public channels, all Users get notified when
         any other User joins or leaves the channel (gc_SubscibeLogIn/gc_SusbcribeLogOut).  @html(<br><br>)

      .... and then, there are ... @html(<br><br>)

      2. Private channels, where the "Channel" parameter (the "Key") for Listeners/Controls is calculated
      using the "rtcGenerateKey" function (rtcKeyHash unit), while the "Channel" parameter for Hosts (the "Lock")
      is calculated with the "rtcCalculateLock" function from the Listener/Controls "Channel" (the "Key"). @html(<br><br>)

      For a User to join a private channel as Listener/Control, a "Key" generated with the
      "rtcGenerateKey" function (rtcKeyHash unit) has to be used as the "Channel" parameter. @html(<br><br>)

      For a User to join a private channel as a Host, a "Lock" calculated with the "rtcCalculateLock" function
      from the "Key" (used by Listeners/Controls) has to be used as the "Channel" parameter. @html(<br><br>)

      Listeners/Controls get notified when a
        Host joins or leaves their channel (gc_SubscribeLogIn/gc_SubscribeLogOut). @html(<br><br>)

      Hosts get notified when a
        Listener/Control joins or leaves their channel (gc_SubscribeLogIn/gc_SubscribeLogOut).  @html(<br><br>)

      If the same User subscribes to multiple channels,
        a sepatate notification will be sent for each Channel. @html(<br><br>)

      NOTE: Subscribe and Unsubscribe methods are ALSO used internally by the Gate Account Manager
            assigned to the "AccountManager" property. Account "ID" used by the Account Manager is
            the "Channel" used by the Subscribe method. Do NOT manually subscribe/unsubscribe to
            channels used by the Account Manager, this will be done by the component. }
    function Subscribe(const Channel:RtcString; GroupID:TGateUID = 0):boolean;

    { UnSubscribe from channel "Channel". @html(<br><br>)

      In Public channels, all Users get notified when any other User leaves the channel (gc_SubscribeLogOut).  @html(<br><br>)

      In Private channels,
        Listeners/Controls get notified when a
          Host unsubscribes from their channel (gc_SubscribeLogOut)
        and Hosts get notified when a
          Listener/Control unsusbcribes from their channel (gc_SubscribeLogOut).  @html(<br><br>)

      Unsubscribing from channels is not necessary, because Users are automatically
      unsubscribed from all channels on the Gateway when they log out or loose connection.  @html(<br><br>)

      Same as with User Groups and Friend Lists, subscriptions to channels on the
      Gateway are removed automatically when the user logs out or looses connection. @html(<br><br>)

      NOTE: Subscribe and Unsubscribe methods are ALSO used internally by the Gate Account Manager
            assigned to the "AccountManager" property. Account "ID" used by the Account Manager is
            the "Channel" used by the Unsubscribe method. Do NOT manually subscribe/unsubscribe to
            channels used by the Account Manager, this will be done by the component. }
    function UnSubscribe(const Channel:RtcString):boolean;

    { Add "UserID" to my Group "GroupID",
      to receive all packages sent to "GroupID" with the "SendToMyGroup" method.
      Returns TRUE if the command was placed in sending buffers.
      If the User is NOT online, or the User goes Off-Line, or we lose connection
      to the Gateway, the User will automatically be removed from the Group. }
    function AddUserToMyGroup(const GroupID,UserID:TGateUID):boolean;

    { Remove User "UserID" from my Group "GroupID". After this call, the User "UserID" will NOT
      be receiving any more packages sent to my Group "GroupID" with the "SendToMyGroup" method.
      Returns TRUE if the command was placed in sending buffers. }
    function RemoveUserFromMyGroup(const GroupID,UserID:TGateUID):boolean;

    { Disband my Group with "GroupID" and remove all users from the Group on THIS Gateway
      (if the "AddUserToMyGroup" method was used with "GroupID" to add users since the last login).
      Returns TRUE if the command was placed in sending buffers. }
    function DisbandMyGroup(const GroupID:TGateUID):boolean;

    { Returns TRUE if the user "UserID" is in my Group "GroupID" on THIS Gateway,
      added with the "AddUserToMyGroup" method, NOT removed and probably connected to the Gateway. }
    function IsUserInMyGroup(const GroupID,UserID:TGateUID):boolean;

    { Returns the current TOTAL number of users in my Group "GroupID" on THIS Gateway,
      added with the "AddUserToMyGroup" method, NOT removed and probably connected to the Gateway. }
    function UsersInMyGroup(const GroupID:TGateUID):integer;

    { Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to all users in my Group
      "GroupID" on THIS Gateway. This method will be sending data to the Gateway ONLY if we have
      at least one user added with the "AddUserToMyGroup" method and connected to THIS Gateway.

      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000,
      they will be used in future updates to extend the components with new features.

      Returns TRUE if the package was placed in sending buffers. }
    function SendToMyGroup(const GroupID:TGateUID; const CallID:word; const data:RtcByteArray):boolean; overload;

    { Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to all users in my Group
      "GroupID" on THIS Gateway. This method will be sending data to the Gateway ONLY if we have
      at least one user added with the "AddUserToMyGroup" method and connected to THIS Gateway.

      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000,
      they will be used in future updates to extend the components with new features.

      Returns TRUE if the package was placed in its sending buffers. }
    function SendToMyGroup(const GroupID:TGateUID; const CallID:word; const data:RtcString):boolean; overload;

    { Send "CallID" (0..65000) with no content to all users in my Group "GroupID" on THIS Gateway.
      This method will be sending data to the Gateway ONLY if we have at least one User
      added with the "AddUserToMyGroup" method and still connected to THIS Gateway.

      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000,
      they will be used in future updates to extend the components with new features.

      Returns TRUE if the package was placed in its sending buffers. }
    function SendToMyGroup(const GroupID:TGateUID; const CallID:word):boolean; overload;

    { Leave Group "GroupID" owned by the User "OwnerID" on THIS Gateway.
      Returns TRUE if the command was placed in its sending buffers. }
    function LeaveUsersGroup(const OwnerID:TGateUID; const GroupID:TGateUID):boolean;

    { Request verification for all "Linked" accounts logged in with "UserID" which have NOT beed verified.
      Returns TRUE if at least one verification request was sent. "OnInfoReceived" events will be triggered
      with "Data.Command" = "gc_AccountVerified" for all remote Accounts where verification was successful. @html(<br><br>)

      This is used for manual User Account verification, in case you do NOT want all Accounts to be verified.
      If you DO want ALL "Linked" (remote Private) accounts to be verified immediately on Login,
      just set the "AutoVerifyAccounts:=TRUE" on the "AccountManager" assigned to this component. }
    function RequestUserVerifications(const UserID:TGateUID):boolean;

    { Request verification for the "Linked" account "AccountID" logged in with "UserID", but NOT yet verified.
      Returns TRUE if the verification request was sent (account was found but NOT verified).
      "OnInfoReceived" event will be triggered with "Data.Command" = "gc_AccountVerified"
      if remote Account verification was successful. @html(<br><br>)

      This is used for manual Account verification, in case you do NOT want all Accounts to be verified.
      If you DO want ALL "Linked" (remote Private) accounts to be verified immediately on Login,
      just set the "AutoVerifyAccounts:=TRUE" on the "AccountManager" assigned to this component. }
    function RequestAccountVerification(const UserID:TGateUID; const AccountID:RtcString):boolean;

    { Send Account Data to the User "UserID", generated with
        AccountManager's "PrepareCopy" or "PrepareLink" method, called with a currently
        valid "AuthCode" generated by receiver's "AccountManager" or "MultiGateClient" component.

      Returns TRUE if the Account Data was sent.

      "OnInfoReceived" event will be triggered with "Data.Command" = "gc_AccountReceived" on the receiver side
      and "Data.AccountID" containing the newly registered Account ID if the Account was received correctly,
      or ... with "Data.Command" = "gc_AccountBadReceive" if Account Data was corrupt or the "AuthCode" was wrong.

      "OnInfoReceived" event will be triggered with "Data.Command" = "gc_AccountTransferred" on our side and
      "Data.AccountID" containing the Account ID (or Link) that was transferred successfully, or ... with
      "Data.Command" = "gc_AccountBadTransfer" if transfer confirmation was wrong or the Account was deleted. }
    function SendAccountData(const UserID:TGateUID; const ReceiverAuthCode:RtcString; const AccountData:RtcByteArray):boolean;

    // My IP address returned from the Gateway after login
    property MyIP:RtcString read FMyIP;

    // My Encryption Key returned from the Gateway after login
    property MyKey:RtcString read FMyKey;

    // My User ID returned from the Gateway after login
    property MyUID:TGateUID read FMyUID;

    // My User Address (MyUID @ GateAddr : GatePort), available after login
    property MyAddr:RtcString read GetMyAddr;

    { User Group State Management }
    property Groups:TRtcGateUserGroupStates read FUserGroups;

    {  Shared Group ID Management. All components using this Gate-Client component to
       communicate through a Gateway with a Group of Users HAVE TO use this property to manage
       Group IDs (allocate and deallocate), to avoid using the same ID for different things.

       The ONLY exception to this rule are "MyGroupID" provided by "TRtcAbsGateClientLink"
       and "TRtcAbsMultiGateClientLink" components, since these Group IDs are automatically
       allocated and de-allocated internally by using the "SharedGroups" property. }
    property SharedGroups:TRtcGateUserGroupIDs read GetSharedGroups;

    // Connected and ready to send and receive?
    property Ready:boolean read GetReady;

    { Detailed connection state information }
    property State:TRtcGateClientStateInfo read FState;

    { Is the User "UserID" NOT (fully) verified? Returns TRUE if the User is logged in on at least one
      "Linked" (remote Private) account for which we have NOT yet received a positive account verification.
      If this property returns TRUE, you can use the "RequestUserVerifications" method
      to ask the User to verify all logged in but NOT yet verified "Linked" accounts. }
    property UserNotVerified[const UserID:TGateUID]:boolean read CheckNotVerifiedUser;

    { Is the User "UserID" allowed to perform Action "action"?
      Based on Permissions for "Public" accounts and VERIFIED "Private" or "Linked" Accounts. }
    property UserAllowed[const UserID:TGateUID; const action:word]:boolean read GetUserAllowed;

    { Is the User "UserID" *verified* allowed to perform Action "action"?
      Based on Permissions for logged in and *VERIFIED* "Private" and "Linked" accounts
      ("Public" permissions excluded). }
    property VerifiedAllowed[const UserID:TGateUID; const action:word]:boolean read GetVerifiedAllowed;

    { Is the User "UserID" *maybe* (account logged in but NOT verified) allowed to perform Action "action"?
      Based on Permissions for all logged in accounts (Public, Linked and Private),
      regardless of their verification status. }
    property UnVerifiedAllowed[const UserID:TGateUID; const action:word]:boolean read GetMaybeAllowed;

    { Is the User "UserID" logged in on a NOT yet VERIFIED account "AccountID"?
      Returns TRUE if the User is logged in with the Account "AccountID", the account is of type
      "Linked" (remote Private) or "Private", but a verification has NOT been received for that account.
      If this property returns TRUE and the account is "Linked" (remote Private), you can use
      the "RequestAccountVerification" method to ask for account verification from the User.
      For "Private" accounts, the remote User has to request account verification from us. }
    property AccountNotVerified[const UserID:TGateUID; const AccountID:RtcString]:boolean read CheckNotVerifiedAccount;

    { Is the User "UserID" logged in with the Account "AccountID"?
      Returns TRUE for logged in Public accounts and VERIFIED "Private" or "Linked" accounts. }
    property UserAccount[const UserID:TGateUID; const AccountID:RtcString]:boolean read CheckUserInAccount;

    { Returns the last received "UserInfo" string for the User "UserID".
      The "UserInfo" string is received from the Gateway with Account notifications, contains the
      "GateUserInfo" property assigned on the Client for which the notification was sent and remains
      populated while the User is logged in with at least one Account managed by our AccountManager. }
    property UserInfo[const UserID:TGateUID]:RtcString read CheckUserInfo;

    { Is the User "UserID" logged in with a VERIFIED account "AccountID"?
      Returns TRUE only for logged in and VERIFIED "Private" and "Linked" accounts.
      Returns FALSE for "Public" accounts and NOT verified "Private" or "Linked" accounts. }
    property VerifiedAccount[const UserID:TGateUID; const AccountID:RtcString]:boolean read CheckUserInVerifiedAccount;

    { Is the User "UserID" *maybe* logged in (account NOT verified) with the Account "AccountID"?
      Returns TRUE for all logged in Accounts ("gc_AccountLogIn" notification has been received). }
    property UnVerifiedAccount[const UserID:TGateUID; const AccountID:RtcString]:boolean read CheckMaybeInAccount;

  published
    { Set Active=TRUE to connect the user to the Gateway and stay connected until LogOut or LoginFail.
      Set Active=FALSE to disconnect the user from the Gateway. If AutoLogin=True,
          a new connection will be established imediately after LogOut, with a new User ID. }
    property Active:boolean read GetActive write SetActive default False;

    { Set AutoLogin=TRUE to make sure the user is connected
          and stays connected, even after LogOut or LoginFail.
      Set AutoLogin=FALSE to disconnect the user from the Gateway
          and stop reconnecting after LogOut and LoginFail. }
    property AutoLogin:boolean read GetAutoRelogin write SetAutoRelogin default False;

    { Properties to set before use }

    // Gateway Address (IP or Domain name, without the "http://" or "https://" prefix)
    property GateAddr:RtcString read FGateAddr write SetGateAddr;

    // Gateway Port number
    property GatePort:RtcString read FGatePort write SetGatePort;

    // Gateway "IP Version" prefference
    property GateIPV:RtcIPV read FGateIPV write SetGateIPV default rtc_IPVDefault;

    // FileName ("/" for root) where the TRtcGateway component is accessible
    property GateFileName:RtcString read FGateFileName write SetGateFileName;

    // Primary Encryption Key set up on the TRtcGateway component (leave empty for default encryption)
    property GatePrimaryKey:RtcString read FGATE_PRIMARY_KEY write SetGatePrimaryKey;

   { Secondary Encryption Key, specific to this Client.
      Leave Empty for default key generation, or implement the BeforeUserLogin event
      on the TRtcGateway component to set the same value on the Gateway for this Client. }
    property GateSecondaryKey:RtcString read FGATE_SECONDARY_KEY write SetGateSecondaryKey;

    { User Authentication information, packed into a single String,
      which will be made available in all events on the TRtcGateway component and
      can be used to identify this Client or add the Client to specific User Groups. }
    property GateUserAuth:RtcString read FGATE_USERAUTH write SetGateUserAuth;

    { *PUBLIC* User information (no Passwords!), packed into a single String,
      which will be made available in all events on the TRtcGateway component,
      forwarded to all Clients using Subscription and Account notification mechanics
      and can be used to identify the Client or add the Client to specific User Groups. }
    property GateUserInfo:RtcString read FGATE_USERINFO write SetGateUserInfo;

    // Use Blocking connection provider
    property UseBlocking:boolean read FUseBlocking write SetUseBlocking default False;

    // Use Proxy-compatible connection provider
    property UseProxy:boolean read FUseProxy write SetUseProxy default False;

    // Use WinHTTP on Windows
    property UseWinHTTP:boolean read FUseWinHTTP write SetUseWinHTTP default False;

    // Use SSL-compatible connection provider
    property UseSSL:boolean read FUseSSL write SetUseSSL default False;

    { To use SSL/SSH encryption with third-party components, simply assign the encryption
      plug-in here before you start using the Client connection (before first connect). }
    property UseCryptPlugin:TRtcCryptPlugin read FCryptPlugin write SetCryptPlugin;

    { UserLogin data is ignored when UseCryptPlugin is assigned. @html(<br><br>)

      If UseCryptPlugin is NOT assigned (not using third-party components for encryption) ... @html(<br><br>)

      Using this property, you can define login information for a server which
      requires user authentication and/or a client certificate (using WinInet API).
      If you want to use Third-party SSL/SSH components for encryption,
      simply assign the plug-in to the UseCryptPlugin property. }
    property UserLogin:TRtcUserLoginInfo read FUserLogin write SetUserLogin;

    { Input Stream Block Size. By default (0) use DEF_OUTSTREAM_SIZE = 2 GB @html(<br><br>)

      For best possible performance, use 0 (default) or large blocks. To avoid problems
      with Anti-Virus software and Proxy Servers, use a small number higher than zero. @html(<br><br>)

      If more than the specified "StreamBlockSizeIn" bytes need to be sent from the Gateway
      when a new response is being prepared, the content length for the current response
      will be set to include the complete current output buffer on the Gateway and avoid
      unnecessary packet splitting, improving streaming performance where possible.  }
    property StreamBlockSizeIn:Cardinal read GetStreamBlockSizeIn write SetStreamBlockSizeIn default 0;

    { Output Stream Block Size. By default (0) use DEF_OUTSTREAM_SIZE = 2 GB @html(<br><br>)

      For best possible performance, use 0 (default) or large blocks. To avoid problems
      with Anti-Virus software and Proxy Servers, use a small number higher than zero. @html(<br><br>)

      If the Client has to work on Systems with Anti-Virus Software checking internet traffic,
      set the "StreamBlockSizeOut" property to a small value higher than zero for automatic packet
      size calculation. Keep the value below the maximum number of bytes you want to allow Anti-Virus
      Software to store in its internal buffers before sending it out to the Gateway. @html(<br><br>)

      This limit is achieved by limiting the outgoing request size and thus forcing
      the Client to wait for a response from the Gateway before more data is sent.  @html(<br><br>)

      If more than the specified "StreamBlockSizeOut" bytes need to be sent out when a new
      request is being prepared, the content length for the current request will be set to
      include the complete current output buffer instead of limiting the request size to the
      specified value, to avoid unnecessary packet splitting and improve streaming performance. }
    property StreamBlockSizeOut:Cardinal read GetStreamBlockSizeOut write SetStreamBlockSizeOut default 0;

    { Speed limit (in KBits per second, 0 = unlimited) for data packets streamed from THIS Client.
      By setting a speed limit, the component will wait before sending more data out if sending
      more data would result in the last sending operation to exceed the specified speed limit.
      This speed limit does NOT affect the way "SendBytes" or "SendToGroup" methods work (they are 
      always non-blocking and asynchronous) and it does NOT split larger packets into even smaller 
      chunks to reduce the bandwidth usage, but ONLY forces a delay in the background thread 
      where data is being sent, to keep the outgoing bandwidth usage below the specified limit. }
    property StreamSpeedLimit:Cardinal read FWriteLimit write FWriteLimit default 0;

    { Gate Account Manager. When assigned, the Subscribe method will be used by the component to register
      activated Gate Accounts and the Unsubscribe method to unregister deactivated Accounts on the Gateway,
      using Account IDs as "Channels" with automatically assigned GroupIDs. }
    property AccountManager:TRtcGateAccountManager read FAccounts write SetAccounts;

    { You would like some events triggered by THIS component to be executed asynchronously,
      either inside a separate Backgrdound Thread or asynchronously from the Main Thread?

      Assign a "TRtcGCThread" component to the "BackThread" property, implement events on the
      "BackThread" component which you want executed asynchronously from the Backround Thread
      or asynchronously from the Main Thread (GUI), then leave these events without implementation
      on THIS component, or implement events on THIS component and set "WantBackThread:=True" from
      inside any event on THIS component if you want the same event posted for asynchonous
      execution on "BackThread" after events on THIS component finish executing.

      If an event is NOT implementated on THIS component, but an event with the same name
      is implemented on the "Thread" component, event on the "Thread" component will automatically
      be posted for (asynchronous) execution after the "GUI" event (optional) finishes.

      For example, if you want the "AfterLoggedIn" event on the "Thread" component executed
      asynchronously after the user logs in, you can either set "WantBackThread:=True" from the
      "AfterLoggedIn" event on THIS component, or leave THIS components "AfterLoggedIn" event
      unassigned (NOT implemented). This rule applies to all events with a "WantBackThread" parameter. }
    property BackThread:TRtcGCThread read FThread write FThread;

    { Event called BEFORE the user Logs in to the Gateway. It can be used for custom component setup.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "BeforeLoginGUI" event will be executed from the Main Thread after this event. }
    property BeforeLogIn:TRtcGateClientStateEvent read FConBeforeLogIn write FConBeforeLogIn;

    { Event called after the Client logs in to the Gateway.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLoggedInGUI" event will be executed from the Main Thread after this event. }
    property AfterLoggedIn:TRtcGateClientStateEvent read FConAfterLoggedIn write FConAfterLoggedIn;

    { Event called if the last Login attempt has failed.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLoginFailGUI" event will be executed from the Main Thread after this event. }
    property AfterLoginFail:TRtcGateClientStateEvent read FConAfterLoginFail write FConAfterLoginFail;

    { Event called if the Client has been logged out of the Gateway.
      This can either be because the Client has permanently lost connection
      and the UserID belonging to this Client is no longer valid,
      or because the Active property was set to FALSE to close the connection.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLogOutGUI" event will be executed from the Main Thread after this event. }
    property AfterLogOut:TRtcGateClientStateEvent read FConAfterLogOut write FConAfterLogOut;

    { Data was received from the Gateway. Use provided parameters to check what was received
      and set "Wanted:=TRUE" if THIS components "OnDataReceived" event should be called.
      If "OnDataFilter" event is NOT implemented, "OnDataReceived" and/or "OnDataRecievedGUI" event is called for ALL data received.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread. }
    property OnDataFilter:TRtcGateClientFilterEvent read FOnDataFilter write FOnDataFilter;

    { Info was received from the Gateway. Use provided parameters to check what was received
      and set "Wanted:=TRUE" if THIS components "OnInfoReceived" event should be called.
      If "OnInfoFilter" event is NOT implemented, "OnInfoReceived" event is called for ALL info received.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread. }
    property OnInfoFilter:TRtcGateClientFilterEvent read FOnInfoFilter write FOnInfoFilter;

    { Received data from the Gateway. Triggered ONLY if the "OnDataFilter" event
      is NOT implemented or if the "OnDataFilter" event returns with "Wanted=True".
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnDataReceivedGUI" event will be executed from the Main Thread after this event. }
    property OnDataReceived:TRtcGateClientDataEvent read FConDataReceived write FConDataReceived;

    { Received info from the Gateway. Triggered ONLY if the "OnInfoFilter" event
      is NOT implemented or if the "OnInfoFilter" event returns with "Wanted=True".
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnInfoReceivedGUI" event will be executed from the Main Thread after this event. }
    property OnInfoReceived:TRtcGateClientDataEvent read FConInfoReceived write FConInfoReceived;

    { Client is again ready to send data to the Gateway after the last Stream Reset.
      Use this event to re-invite Users to your Groups and fix broken User relations.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnReadyAfterResetGUI" event will be executed from the Main Thread after this event. }
    property OnReadyAfterReset:TRtcGateClientStateEvent read FConReadyAfterReset write FConReadyAfterReset;

    { Client is ready to send more data to the Gateway. Use this event to implement
      the process of sending larger data "streams" by breaking them up in smaller packets.
      Keep in mind that the maximum single packet size is 16 MB.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnReadyToSendGUI" event will be executed from the Main Thread after this event. }
    property OnReadyToSend:TRtcGateClientStateEvent read FConReadyToSend write FConReadyToSend;

    { Input and Output Streams have been reset, either because the "ResetStreams"
      method was called, or because Clients connection to the Gateway was lost.
      A new connection attempt will be made after this event.
      Use this event for cleaning up Group Status information.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnStreamResetGUI" event will be executed from the Main Thread after this event. }
    property OnStreamReset:TRtcGateClientStateEvent read FConStreamReset write FConStreamReset;

    { Event called from the Main (GUI) Thread, synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "BeforeLogIn" event is NOT implemented or returns with "WantGUI=True". }
    property BeforeLogInGUI:TRtcGateClientGUIStateEvent read FGuiBeforeLogIn write FGuiBeforeLogIn;

    { Event called from the Main (GUI) Thread, synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLoggedIn" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLoggedInGUI:TRtcGateClientGUIStateEvent read FGuiAfterLoggedIn write FGuiAfterLoggedIn;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLoginFail" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLoginFailGUI:TRtcGateClientGUIStateEvent read FGuiAfterLoginFail write FGuiAfterLoginFail;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLogOut" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLogOutGUI:TRtcGateClientGUIStateEvent read FGuiAfterLogOut write FGuiAfterLogOut;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnDataReceived" event is NOT implemented or returns with "WantGUI=True". }
    property OnDataReceivedGUI:TRtcGateClientGUIDataEvent read FGuiDataReceived write FGuiDataReceived;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnInfoReceived" event is NOT implemented or returns with "WantGUI=True". }
    property OnInfoReceivedGUI:TRtcGateClientGUIDataEvent read FGuiInfoReceived write FGuiInfoReceived;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnReadyAfterReset" event is NOT implemented or returns with "WantGUI=True". }
    property OnReadyAfterResetGUI:TRtcGateClientGUIStateEvent read FGuiReadyAfterReset write FGuiReadyAfterReset;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnReadyToSend" event is NOT implemented or returns with "WantGUI=True". }
    property OnReadyToSendGUI:TRtcGateClientGUIStateEvent read FGuiReadyToSend write FGuiReadyToSend;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnStreamReset" event is NOT implemented or returns with "WantGUI=True". }
    property OnStreamResetGUI:TRtcGateClientGUIStateEvent read FGuiStreamReset write FGuiStreamReset;
  end;

  { @Abstract(Abstract RTC Multi-Gate Client Link class)
    Implements basic functionality required by components using a Multi-Gate-Client component.

    All events on this component are called from within a thread context of the
    connection component triggering each event, which means that components events
    will be called from multiple threads (depending on what triggered the event) and
    all the code you write for handling this components events should be thread-safe.

    Every component based on TRtcAbsMultiGateClientLink allocates one "MyGroupID"
    for its exlusive use. Because the max number of available Group IDs is limited to 4095,
    it will NOT be possible to have more than 4095 components based on TRtcAbsMultiGateClientLink
    working with the exact same TRtcHttpMultiGateClient component, but ... since 4095 is quite a
    big number, it is highly unlikely that we will ever need so many different components
    processing data coming from a single TRtcHttpMultiGateClient component. }
  TRtcAbsMultiGateClientLink = class(TRtcAbsGateClient)
  private
    FMultiClient: TRtcHttpMultiGateClient;
    FMyGroupID: TGateUID;
    FLocked: integer;

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure RemoveMultiClient(Value:TRtcHttpMultiGateClient); virtual;

    // @exclude
    function GetMultiClient: TRtcHttpMultiGateClient; virtual;
    // @exclude
    procedure SetMultiClient(const Value: TRtcHttpMultiGateClient); virtual;

    // @exclude
    function GetMyGroupID:TGateUID; virtual;
    // @exclude
    procedure SetMyGroupID(const Value: TGateUID); virtual;

    // @exclude
    procedure Call_MultiClientAssigned; virtual;
    // @exclude
    procedure Call_MultiClientRemoved; virtual;
    // @exclude
    procedure Sync_MultiClientRemoved; virtual;

  public

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Uses "UserInfo" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      return the last received "UserInfo" string for the User "UserAddr" (UserID@GateAddr:GatePort).
      The "UserInfo" string is received from the Gateway with Account notifications, contains the
      "GateUserInfo" property assigned on the Client for which the notification was sent and remains
      populated while the User is logged in with at least one Account managed by the "MultiClient" component. }
    function UserInfo(const UserAddr:RtcString):RtcString;

    { Uses "Ping" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Ping the user "UserAddr" (UserID@GateAddr:GatePort) and get notified if the User is OFF-LINE.
      Returns TRUE if connection component was found and the PING command was placed in its sending buffers. }
    function PingUser(const UserAddr:RtcString):boolean;

    { Uses "RequestUserVerifications" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Request verifications for all "Linked" accounts logged in with "UserAddr" which have NOT beed verified.
      Returns TRUE if at least one verification request was sent. "OnInfoReceived" events will be triggered
      with "Data.Command" = "gc_AccountVerified" for all remote Accounts where verification was successful. }
    function VerifyUser(const UserAddr:RtcString):boolean;

    { Uses "UserNotVerified" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Check if the User "UserAddr" (UserID@GateAddr:GatePort) is NOT (fully) verified.
      Returns TRUE if the User is logged in on at least one "Linked" (remote Private)
      account for which we have NOT yet received a positive account verification.
      If this property returns TRUE, you can use the "VerifyUser" method
      to ask the User to verify all logged in but NOT yet verified "Linked" accounts. }
    function UserNotVerified(const UserAddr:RtcString):boolean;

    { Uses "UserAllowed" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Check if the User "UserAddr" (UserID@GateAddr:GatePort) is allowed to perform Action "action",
      Based on Permissions for "Public" accounts and *VERIFIED* "Private" or "Linked" Accounts. }
    function UserAllowed(const UserAddr:RtcString; const action:word):boolean;

    { Uses "AddUserToMyGroup" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Add User "UserAddr" (UserID@GateAddr:GatePort) to My Group.
      Returns TRUE if connection component was found and the command was placed in its sending buffers.
      If the User is NOT online, or the User goes Off-Line, or we lose connection to the
      Gateway where the User was logged in, the User will automatically be removed from our Group. }
    function AddUserToGroup(const UserAddr:RtcString):boolean;

    { Uses "IsUserInMyGroup" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Return TRUE if the user "UserAddr" (UserID@GateAddr:GatePort) is in My Group. }
    function IsUserInGroup(const UserAddr:RtcString):boolean;

    { Uses "RemoveUserFromMyGroup" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Remove User "UserAddr" (UserID@GateAddr:GatePort) from My Group.
      Returns TRUE if connection component was found and the command was placed in its sending buffers. }
    function RemoveUserFromGroup(const UserAddr:RtcString):boolean;

    { Uses "UsersInMyGroup" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Return the current total number of users in My Group. }
    function UsersInGroup:integer;

    { Uses "DisbandMyGroup" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Remove ALL USERS from My Group and Disband My Group, if "AddUserToGroup" method was used on
      THIS component or "AddUserToMyGroup" was used on the "MultiClient" (TRtcHttpGateClient) component.
      Returns TRUE if connection component was found and the command was placed in its sending buffers. }
    function DisbandGroup:boolean;

    { Uses "SendToMyGroup" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to My Group. @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000!
      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendToGroup(CallID:word; const data:RtcByteArray):boolean; overload;

    { Uses "SendToMyGroup" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to My Group. @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000!
      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendToGroup(CallID:word; const data:RtcString):boolean; overload;

    { Uses "SendToMyGroup" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Send "CallID" (0..65000) with no extra content to My Group. @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use IDs above 65000!
      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendToGroup(CallID:word):boolean; overload;

    { Uses "SendBytes" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to "UserAddr" and "GroupID". @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000!
      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendBytes(const UserAddr:RtcString; GroupID:TGateUID; CallID:word; const data:RtcByteArray):boolean; overload;

    { Uses "SendBytes" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to "UserAddr" and "GroupID". @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000!
      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendBytes(const UserAddr:RtcString; GroupID:TGateUID; CallID:word; const data:RtcString):boolean; overload;

    { Uses "SendBytes" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Send "CallID" (0..65000) with no extra content to "UserAddr" and "GroupID". @html(<br><br>)
      You can use any CallID from 0 - 65000, but do NOT use IDs above 65000!
      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendBytes(const UserAddr:RtcString; GroupID:TGateUID; CallID:word):boolean; overload;

    { Uses "AddFriend" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Add User "UserAddr" as our Friend, giving the User permission to add us to his User Group(s)
      on the Gateway. If the user "UserAddr" is online and was NOT already in our Friends list,
      we will receive the "gc_FriendAdd" command from the Gateway and the user "UserAddr" will receive
      the "gc_BeFriend" command. If the user is off-line, we will receive a "gc_UserOffLine" command.
      If the user is already in our friends list, nothing changes and the Gateway
      won't send any notification messages to us or the user.
      Returns TRUE if connection component was found and the command was placed in its sending buffers. }
    function AddFriend(const UserAddr:RtcString):boolean;

    { Uses "RemoveFriend" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Remove User "UserAddr" from our Friends list, revoking Users
      permissions to add us to his User Group(s) on the Gateway.
      If user "UserAddr" was in our Friends list on the Gateway,
      we will receive the "gc_FriendRemove" command from the Gateway
      and the user "UserAddr" will receive the "gc_UnFriend" command.
      Removing a User from our Friends list will not close open User Groups,
      but it will prevent the User from adding us to more Groups.
      NOTE: Removing Friends is not necessary, because our Frields list will
      be cleared on the Gateway if a User logs out or looses connection.

      Returns TRUE if connection component was found and
      the command was placed in its sending buffers. }
    function RemoveFriend(const UserAddr:RtcString):boolean;

    { Uses "LeaveUsersGroup" method on the "MultiClient" (TRtcHttpMultiGateClient) component to
      Leave Group "GroupID" owned by user "OwnerAddr" (OwnerID@GateAddr:GatePort).
      Returns TRUE if connection component was found and the command was placed in its sending buffers. }
    function LeaveUsersGroup(const OwnerAddr:RtcString; GroupID:TGateUID):boolean;

  published
    { "My" Group ID, allocated automatically when a new MultiClient component is
      assigned and removed automatically when the MultiClient component is removed.

      Assigning a new value to this property automatically removes any previously
      assigned "My" GroupID and allocates the closest currently unused Group ID,
      starting from the value being assigned, then checking higher and lower values.
      Should the search for an available Group ID fail, an exception will be reaised.

      When a MultiClient property is NOT assigned, this property is always zero (0). }
    property MyGroupID:TGateUID read GetMyGroupID write SetMyGroupID stored False;

    { Set the MultiClient property to a RtcHttpMultiGateClient component to listen to
      all events from that component. Setting the MultiClient property also allocates
      a new "MyGroupID". If another "MultiClient" component was previously assigned,
      the old "MyGroupID" will also be released, before allocating a new "MyGroupID".
      If the MultiClient component being assigned does NOT have any available Group IDs,
      this MultiClient property will be set to NIL and an exception will be raised.
      Set "MultiClient:=NIL" and make sure it is NIL, before destroying THIS component.
      "AfterClientRemoved" event is triggered when the component is safe to destroy. }
    property MultiClient:TRtcHttpMultiGateClient read GetMultiClient write SetMultiClient;
    end;

  { @Abstract(RTC Multi-Gate Client Link)
    Link this component to a TRtcHttpMultiGateClient to handle user events. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcMultiGateClientLink = class(TRtcAbsMultiGateClientLink)
  private
    FClientAssigned: TRtcSyncEvent;
    FClientRemoved: TRtcSyncEvent;
    FThread: TRtcGCThread;

    FOnDataFilter: TRtcGateClientFilterEvent;
    FOnInfoFilter: TRtcGateClientFilterEvent;
    FConDataReceived: TRtcGateClientDataEvent;
    FConInfoReceived: TRtcGateClientDataEvent;
    FConAfterLogOut: TRtcGateClientStateEvent;
    FConReadyToSend: TRtcGateClientStateEvent;
    FConAfterLoginFail: TRtcGateClientStateEvent;
    FConAfterLoggedIn: TRtcGateClientStateEvent;
    FConReadyAfterReset: TRtcGateClientStateEvent;
    FConBeforeLogIn: TRtcGateClientStateEvent;
    FConStreamReset: TRtcGateClientStateEvent;

    FGuiDataReceived: TRtcGateClientGUIDataEvent;
    FGuiInfoReceived: TRtcGateClientGUIDataEvent;
    FGuiAfterLogOut: TRtcGateClientGUIStateEvent;
    FGuiReadyToSend: TRtcGateClientGUIStateEvent;
    FGuiAfterLoginFail: TRtcGateClientGUIStateEvent;
    FGuiAfterLoggedIn: TRtcGateClientGUIStateEvent;
    FGuiReadyAfterReset: TRtcGateClientGUIStateEvent;
    FGuiBeforeLogIn: TRtcGateClientGUIStateEvent;
    FGuiStreamReset: TRtcGateClientGUIStateEvent;

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure Call_MultiClientAssigned; override;
    // @exclude
    procedure Call_MultiClientRemoved; override;

    // @exclude
    procedure Call_BeforeLogIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLoggedIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLoginFail(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLogOut(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnDataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnInfoReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnReadyToSend(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnReadyAfterReset(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnStreamReset(Sender:TRtcConnection); override;

  published
    { You would like some events triggered by THIS component to be executed asynchronously,
      either inside a separate Backgrdound Thread or asynchronously from the Main Thread?

      Assign a "TRtcGCThread" component to the "BackThread" property, implement events on the
      "BackThread" component which you want executed asynchronously from the Backround Thread
      or asynchronously from the Main Thread (GUI), then leave these events without implementation
      on THIS component, or implement events on THIS component and set "WantBackThread:=True" from
      inside any event on THIS component if you want the same event posted for asynchonous
      execution on "BackThread" after events on THIS component finish executing.

      If an event is NOT implementated on THIS component, but an event with the same name
      is implemented on the "Thread" component, event on the "Thread" component will automatically
      be posted for (asynchronous) execution after the "GUI" event (optional) finishes.

      For example, if you want the "AfterLoggedIn" event on the "Thread" component executed
      asynchronously after the user logs in, you can either set "WantBackThread:=True" from the
      "AfterLoggedIn" event on THIS component, or leave THIS components "AfterLoggedIn" event
      unassigned (NOT implemented). This rule applies to all events with a "WantBackThread" parameter. }
    property BackThread:TRtcGCThread read FThread write FThread;

    { Event called after a new "TRtcHttpMultiGateClient" component is assigned to the
      "MultiClient" property, directly from within the thread making the assignment. }
    property AfterClientAssigned:TRtcSyncEvent read FClientAssigned write FClientAssigned;

    { Event called from the Main (GUI) Thread after the "TRtcHttpMultiGateClient" component is
      removed from THIS components "MultiClient" property. It is safe to destroy the component here. }
    property AfterClientRemoved:TRtcSyncEvent read FClientRemoved write FClientRemoved;

    { Event called BEFORE the user Logs in to the Gateway. It can be used for custom component setup.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "BeforeLoginGUI" event will be executed from the Main Thread after this event. }
    property BeforeLogIn:TRtcGateClientStateEvent read FConBeforeLogIn write FConBeforeLogIn;

    { Event called after the Client logs in to the Gateway.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLoggedInGUI" event will be executed from the Main Thread after this event. }
    property AfterLoggedIn:TRtcGateClientStateEvent read FConAfterLoggedIn write FConAfterLoggedIn;

    { Event called if the last Login attempt has failed.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLoginFailGUI" event will be executed from the Main Thread after this event. }
    property AfterLoginFail:TRtcGateClientStateEvent read FConAfterLoginFail write FConAfterLoginFail;

    { Event called if the Client has been logged out of the Gateway.
      This can either be because the Client has permanently lost connection
      and the UserID belonging to this Client is no longer valid,
      or because the Active property was set to FALSE to close the connection.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "AfterLogOutGUI" event will be executed from the Main Thread after this event. }
    property AfterLogOut:TRtcGateClientStateEvent read FConAfterLogOut write FConAfterLogOut;

    { Data was received from the Gateway. Use provided parameters to check what was received
      and set "Wanted:=TRUE" if THIS components "OnDataReceived" event should be called.
      If "OnDataFilter" event is NOT implemented, "OnDataReceived" and/or "OnDataRecievedGUI" event is called for ALL data received.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread. }
    property OnDataFilter:TRtcGateClientFilterEvent read FOnDataFilter write FOnDataFilter;

    { Info was received from the Gateway. Use provided parameters to check what was received
      and set "Wanted:=TRUE" if THIS components "OnInfoReceived" event should be called.
      If "OnInfoFilter" event is NOT implemented, "OnInfoReceived" event is called for ALL info received.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread. }
    property OnInfoFilter:TRtcGateClientFilterEvent read FOnInfoFilter write FOnInfoFilter;

    { Received data from the Gateway. Triggered ONLY if the "OnDataFilter" event
      is NOT implemented or if the "OnDataFilter" event returns with "Wanted=True".
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnDataReceivedGUI" event will be executed from the Main Thread after this event. }
    property OnDataReceived:TRtcGateClientDataEvent read FConDataReceived write FConDataReceived;

    { Received info from the Gateway. Triggered ONLY if the "OnInfoFilter" event
      is NOT implemented or if the "OnInfoFilter" event returns with "Wanted=True".
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnInfoReceivedGUI" event will be executed from the Main Thread after this event. }
    property OnInfoReceived:TRtcGateClientDataEvent read FConInfoReceived write FConInfoReceived;

    { Client is again ready to send data to the Gateway after the last Stream Reset.
      Use this event to re-invite Users to your Groups and fix broken User relations.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnReadyAfterResetGUI" event will be executed from the Main Thread after this event. }
    property OnReadyAfterReset:TRtcGateClientStateEvent read FConReadyAfterReset write FConReadyAfterReset;

    { Client is ready to send more data to the Gateway. Use this event to implement
      the process of sending larger data "streams" by breaking them up in smaller packets.
      Keep in mind that the maximum single packet size is 16 MB.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnReadyToSendGUI" event will be executed from the Main Thread after this event. }
    property OnReadyToSend:TRtcGateClientStateEvent read FConReadyToSend write FConReadyToSend;

    { Input and Output Streams have been reset, either because the "ResetStreams"
      method was called, or because Clients connection to the Gateway was lost.
      A new connection attempt will be made after this event.
      Use this event for cleaning up Group Status information.
      * This event will be executed from within "Sender:TRtcConnection" component's Background Thread.
        If a part of your event code requires access to the GUI or Main Thread, set "WantGUI:=True" inside
        this event and the "OnStreamResetGUI" event will be executed from the Main Thread after this event. }
    property OnStreamReset:TRtcGateClientStateEvent read FConStreamReset write FConStreamReset;

    { Event called from the Main (GUI) Thread, synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "BeforeLogIn" event is NOT implemented or returns with "WantGUI=True". }
    property BeforeLogInGUI:TRtcGateClientGUIStateEvent read FGuiBeforeLogIn write FGuiBeforeLogIn;

    { Event called from the Main (GUI) Thread, synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLoggedIn" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLoggedInGUI:TRtcGateClientGUIStateEvent read FGuiAfterLoggedIn write FGuiAfterLoggedIn;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLoginFail" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLoginFailGUI:TRtcGateClientGUIStateEvent read FGuiAfterLoginFail write FGuiAfterLoginFail;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "AfterLogOut" event is NOT implemented or returns with "WantGUI=True". }
    property AfterLogOutGUI:TRtcGateClientGUIStateEvent read FGuiAfterLogOut write FGuiAfterLogOut;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnDataReceived" event is NOT implemented or returns with "WantGUI=True". }
    property OnDataReceivedGUI:TRtcGateClientGUIDataEvent read FGuiDataReceived write FGuiDataReceived;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnInfoReceived" event is NOT implemented or returns with "WantGUI=True". }
    property OnInfoReceivedGUI:TRtcGateClientGUIDataEvent read FGuiInfoReceived write FGuiInfoReceived;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnReadyAfterReset" event is NOT implemented or returns with "WantGUI=True". }
    property OnReadyAfterResetGUI:TRtcGateClientGUIStateEvent read FGuiReadyAfterReset write FGuiReadyAfterReset;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnReadyToSend" event is NOT implemented or returns with "WantGUI=True". }
    property OnReadyToSendGUI:TRtcGateClientGUIStateEvent read FGuiReadyToSend write FGuiReadyToSend;

    { Event called from the Main (GUI) Thread synchronized with "Sender:TRtcConnection" components
      Background Thread, if the "OnStreamReset" event is NOT implemented or returns with "WantGUI=True". }
    property OnStreamResetGUI:TRtcGateClientGUIStateEvent read FGuiStreamReset write FGuiStreamReset;
    end;

  // @exclude
  TRtcMultiGateClientLinkList = class(TObject)
  private
    FList:TObjList;
    CS:TRtcCritSec;

  public
    constructor Create;
    destructor Destroy; override;

    function Count:integer;
    procedure RemoveAll;

    procedure Add(Value:TRtcAbsMultiGateClientLink);
    procedure Remove(Value:TRtcAbsMultiGateClientLink);

    function LockFirst:TRtcAbsMultiGateClientLink;
    procedure LockNext(var Value:TRtcAbsMultiGateClientLink);

    procedure DoBeforeLogIn(Sender:TRtcConnection);
    procedure DoAfterLoggedIn(Sender:TRtcConnection);
    procedure DoAfterLoginFail(Sender:TRtcConnection);
    procedure DoAfterLogOut(Sender:TRtcConnection);

    procedure DoDataReceived(Sender:TRtcConnection);
    procedure DoInfoReceived(Sender:TRtcConnection);
    procedure DoReadyToSend(Sender:TRtcConnection);
    procedure DoReadyAfterReset(Sender:TRtcConnection);

    procedure DoStreamReset(Sender:TRtcConnection);
    end;

  { @Abstract(RTC Multi-Gate HTTP Client component)
    HTTP Client component with an integrated Account Manager for communication with multiple
    RTC Gateways, using multiple TRtcHttpGateClient components created and managed internally. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcHttpMultiGateClient = class(TRtcGateAccountManager)
  private
    ActiveClient:TRtcHttpGateClient;
    FMultiGateClientLinks:TRtcMultiGateClientLinkList;

    function GetActiveClient: TRtcHttpGateClient;
    function GetAutoRelogin: boolean;
    function GetClActive: boolean;
    function GetClReady: boolean;
    function GetCryptPlugin: TRtcCryptPlugin;
    function GetGateAddr: RtcString;
    function GetGateFileName: RtcString;
    function GetGatePort: RtcString;
    function GetGateIPV: RtcIPV;
    function GetGatePrimaryKey: RtcString;
    function GetGateSecondaryKey: RtcString;
    function GetGateUserAuth: RtcString;
    function GetGateUserInfo: RtcString;
    function GetStreamBlockSizeIn: Cardinal;
    function GetStreamBlockSizeOut: Cardinal;
    function GetStreamSpeedLimit: Cardinal;
    function GetUseBlocking: boolean;
    function GetUseProxy: boolean;
    function GetUserLogin: TRtcUserLoginInfo;
    function GetUseSSL: boolean;
    function GetUseWinHTTP: boolean;

    procedure SetActiveClient(const Value: TRtcHttpGateClient);
    procedure SetAutoRelogin(const Value: boolean);
    procedure SetClActive(const Value: boolean);
    procedure SetCryptPlugin(const Value: TRtcCryptPlugin);
    procedure SetGateAddr(const Value: RtcString);
    procedure SetGateFileName(const Value: RtcString);
    procedure SetGatePort(const Value: RtcString);
    procedure SetGateIPV(const Value: RtcIPV);
    procedure SetGatePrimaryKey(const Value: RtcString);
    procedure SetGateSecondaryKey(const Value: RtcString);
    procedure SetGateUserAuth(const Value: RtcString);
    procedure SetGateUserInfo(const Value: RtcString);
    procedure SetStreamBlockSizeIn(const Value: Cardinal);
    procedure SetStreamBlockSizeOut(const Value: Cardinal);
    procedure SetStreamSpeedLimit(const Value: Cardinal);
    procedure SetUseBlocking(const Value: boolean);
    procedure SetUseProxy(const Value: boolean);
    procedure SetUserLogin(const Value: TRtcUserLoginInfo);
    procedure SetUseSSL(const Value: boolean);
    procedure SetUseWinHTTP(const Value: boolean);

    function GetState:TRtcGateClientStateInfo;
    function GetMyIP:RtcString;
    function GetMyKey:RtcString;
    function GetMyUID:TGateUID;
    function GetMyAddr:RtcString;

  protected

    // @exclude
    procedure AddMultiGateClientLink(Value:TRtcAbsMultiGateClientLink);
    // @exclude
    procedure RemoveMultiGateClientLink(Value:TRtcAbsMultiGateClientLink);
    // @exclude
    procedure RemoveAllMultiGateClientLinks;

    // @exclude
    procedure Call_BeforeLogIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLoggedIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLoginFail(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLogOut(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnDataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnInfoReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnReadyToSend(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnReadyAfterReset(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnStreamReset(Sender:TRtcConnection); override;

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Find the Client which is set up to connect to the Gateway at "GateAddr:GatePort"
      and uses (or can communicate with) the specified "UserID", then select it
      so it can be modified using this components properties.

      * 1. If a Client with matching UserID, GateAddr and GatePort properties is NOT found, then:
      > 1.A) If the currently selected Client's "Active" and "AutoLogin" properties are FALSE,
             then "GateAddr" and "GatePort" properties of the currently selected Client are updated.
      > 1.B) If the currently selected Client's "Active" or "AutoLogin" property is TRUE,
             a NEW Client component will be created, all properties from the currently selected
             Client will be copied to the NEW Client and the NEW Client will then be selected.

      * 2. If a Client with matching UserID, GateAddr and GatePort properties is found, then:
      > 2.A) If the currently selected Client's "Active" and "AutoLogin" properties are FALSE,
             the Client with matching GateAddr and GatePort properties will be selected
             and the Client component which was previously selected will be DESTROYED (garbage-collected).
      > 2.B) If the currently selected Client's "Active" or "AutoLogin" properties are TRUE,
             then the Client with matching GateAddr and GatePort properties becomes selected,
             without making any changes to (or destroying) the previously selected Client component. }
    procedure SelectClient(UserID:TGateUID; const GateAddr, GatePort:RtcString); overload;

    { Find the Client which is set up to connect to the Gateway at "GateAddr:GatePort",
      then select it so it can be modified using this components properties.

      * 1. If a Client with matching GateAddr and GatePort properties is NOT found, then:
      > 1.A) If the currently selected Client's "Active" and "AutoLogin" properties are FALSE,
             then "GateAddr" and "GatePort" properties of the currently selected Client are updated.
      > 1.B) If the currently selected Client's "Active" or "AutoLogin" property is TRUE,
             a NEW Client component will be created, all properties from the currently selected
             Client will be copied to the NEW Client and the NEW Client will then be selected.

      * 2. If a Client with matching GateAddr and GatePort properties is found, then:
      > 2.A) If the currently selected Client's "Active" and "AutoLogin" properties are FALSE,
             the Client with matching GateAddr and GatePort properties will be selected
             and the Client component which was previously selected will be DESTROYED (garbage-collected).
      > 2.B) If the currently selected Client's "Active" or "AutoLogin" properties are TRUE,
             then the Client with matching GateAddr and GatePort properties becomes selected,
             without making any changes to (or destroying) the previously selected Client component. }
    procedure SelectClient(const GateAddr, GatePort:RtcString); overload;

    { Find a Client with the specified "UserAddr" (UserID @ GateAddr : GatePort)
      and select it, so it can be modified using this components properties.

      * 1. If a Client with matching "UserAddr" (UserID @ GateAddr : GatePort) is NOT found, then:
      > 1.A) If the currently selected Client's "Active" and "AutoLogin" properties are FALSE,
             then "GateAddr" and "GatePort" properties of the currently selected Client are updated.
      > 1.B) If the currently selected Client's "Active" or "AutoLogin" property is TRUE,
             a NEW Client component will be created, all properties from the currently selected
             Client will be copied to the NEW Client and the NEW Client will then be selected.

      * 2. If a Client with matching "UserAddr" (UserID @ GateAddr : GatePort) is found, then:
      > 2.A) If the currently selected Client's "Active" and "AutoLogin" properties are FALSE,
             the Client with matching GateAddr and GatePort properties will be selected
             and the Client component which was previously selected will be DESTROYED (garbage-collected).
      > 2.B) If the currently selected Client's "Active" or "AutoLogin" properties are TRUE,
             then the Client with matching GateAddr and GatePort properties becomes selected,
             without making any changes to (or destroying) the previously selected Client component. }
    procedure SelectClient(const UserAddr:RtcString); overload;

    { Set the AutoLogin property for ALL Clients to FALSE
      and Clean up (Destroy all inactive Client components). }
    procedure LogOutAndCleanUp;

    { Send "data" (Content, max 16MB in size) with "CallID" (0..65000)
        to "UserAddr" (UserID@GateAddr:GatePort) and "GroupID" (0..4095). @html(<br><br>)

      To send data to one recipient, call this method with UserAddr=receiverAddr and GroupID=optGroupID (GroupID is optional).
      When data is sent to one user, the user will receive "Data" with UserAddr=MyAddr and ToGroupID=GroupID. @html(<br><br>)

      To send data to a User Group created by us, call this method with UserAddr=MyAddr and GroupID=MyGroupID.
      When data is sent to a Group, all recipients will get "Data" with UserAddr=MyAddr and GroupID=MyGroupID. @html(<br><br>)

      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000,
      they will be used in future updates to extend the components with new features.

      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendBytes(const UserAddr:RtcString; GroupID:TGateUID; CallID:word; const data:RtcByteArray):boolean; overload;

    { Send "data" (Content, max 16MB in size) with "CallID" (0..65000)
        to "UserAddr" (UserID@GateAddr:GatePort) and "GroupID" (0..4095). @html(<br><br>)

      To send data to one recipient, call this method with UserAddr=receiverAddr and GroupID=GroupID (GroupID is optional).
      When data is sent to one user, the user will receive "Data" with UserAddr=MyAddr and ToGroupID=optGroupID. @html(<br><br>)

      To send data to a User Group created by us, call this method with UserAddr=MyAddr and GroupID=MyGroupID.
      When data is sent to a Group, all recipients will get "Data" with UserAddr=MyAddr and GroupID=MyGroupID. @html(<br><br>)

      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000,
      they will be used in future updates to extend the components with new features.

      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendBytes(const UserAddr:RtcString; GroupID:TGateUID; CallID:word; const data:RtcString):boolean; overload;

    { Send "CallID" (0..65000) with no extra content
        to "UserAddr" (UserID@GateAddr:GatePort) and "GroupID" (0..4095) through the Gateway. @html(<br><br>)

      To send the CallID to one recipient, call this method with UserAddr=receiverAddr and GroupID=optGroupID (GroupID is optional).
      When CallID is sent to one user, the user will receive "Data" with UserAddr=MyAddr and ToGroupID=optGroupID. @html(<br><br>)

      To send CallID to a User Group created by us, call this method with UserAddr=MyAddr and GroupID=MyGroupID.
      When CallID is sent to a Group, all recipients will get "Data" with UserAddr=MyAddr and GroupID=MyGroupID. @html(<br><br>)

      You can use any CallID from 0 - 65000, but do NOT use IDs above 65000,
      they will be used in future updates to extend the components with new features.

      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendBytes(const UserAddr:RtcString; GroupID:TGateUID; CallID:word):boolean; overload;

    { Ping user "UserAddr" (UserID@GateAddr:GatePort).
      Returns TRUE if connection component was found and the PING command was placed in its sending buffers. }
    function PingUser(const UserAddr:RtcString):boolean;

    { Add User "UserAddr" (UserID@GateAddr:GatePort)
        to group "GroupID" owned by user "OwnerAddr" (OwnerID@GateAddr:GatePort).
      NOTE: Only users connected to the same Gateway (GateAddr:GatePort) can be added to a Group.
      Returns TRUE if connection component was found and the command was placed in its sending buffers. }
    function AddUserToGroup(const OwnerAddr:RtcString; GroupID:TGateUID; const UserAddr:RtcString):boolean;

    { Remove User "UserAddr" (UserID@GateAddr:GatePort)
        from group "GroupID" owned by user "OwnerAddr" (OwnerID@GateAddr:GatePort)
      Returns TRUE if connection component was found and the command was placed in its sending buffers. }
    function RemUserFromGroup(const OwnerAddr:RtcString; GroupID:TGateUID; const UserAddr:RtcString):boolean;

    { Remove Group "GroupID" owned by user "OwnerAddr" (OwnerID@GateAddr:GatePort)
      Returns TRUE if connection component was found and the command was placed in its sending buffers. }
    function RemoveGroup(const OwnerAddr:RtcString; GroupID:TGateUID):boolean;

    { Add User "UserAddr" (UserID@GateAddr:GatePort) to my Group "GroupID",
      to receive all packages sent to the "GroupID" with the "SendToMyGroup" method.
      If users from different Gateways are added to the same GroupID, "SendToMyGroup"
      method will be sending packages to all the Gateways with at least one user online.
      Returns TRUE if connection component was found and the command was placed in its sending buffers.
      If the User is NOT online, or the User goes Off-Line, or we lose connection to the
      Gateway where the User was logged in, the User will automatically be removed from our Group. }
    function AddUserToMyGroup(GroupID:TGateUID; const UserAddr:RtcString):boolean;

    { Remove User "UserAddr" (UserID@GateAddr:GatePort) from my Group "GroupID", so that User will
      NOT be receiving any more packages sent to my Group "GroupID" with the "SendToMyGroup" method.
      Returns TRUE if connection component was found and the command was placed in its sending buffers. }
    function RemoveUserFromMyGroup(GroupID:TGateUID; const UserAddr:RtcString):boolean;

    { Disband my Group with "GroupID", removing all users from the Group on all
      connected Gateways for which the "AddUserToMyGroup" method was used with "GroupID".
      Returns TRUE if at least one connection component was found and the command was placed in sending buffers. }
    function DisbandMyGroup(GroupID:TGateUID):boolean;

    { Returns TRUE if the user "UserAddr" (UserID@GateAddr:GatePort) is in my Group "GroupID",
      added with the "AddUserToMyGroup" method, NOT removed and probably connected to the Gateway. }
    function IsUserInMyGroup(GroupID:TGateUID; const UserAddr:RtcString):boolean;

    { Returns the current total number of users in my Group "GroupID"
      (added using the "AddUserToMyGroup" method). }
    function UsersInMyGroup(GroupID:TGateUID):integer;

    { Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to all users in my Group "GroupID".
      This method works across multiple Gateways by sending data to every connected Gateway where we
      have at least one user connected and added to our GroupID with the "AddUserToMyGroup" method.

      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000,
      they will be used in future updates to extend the components with new features.

      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendToMyGroup(GroupID:TGateUID; CallID:word; const data:RtcByteArray):boolean; overload;

    { Send "data" (Content, max 16MB in size) with "CallID" (0..65000) to all users in my Group "GroupID".
      This method works across multiple Gateways by sending data to every connected Gateway where we
      have at least one user connected and added to our GroupID with the "AddUserToMyGroup" method.

      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000,
      they will be used in future updates to extend the components with new features.

      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendToMyGroup(GroupID:TGateUID; CallID:word; const data:RtcString):boolean; overload;

    { Send "CallID" (0..65000) with no content to all users in my Group "GroupID".
      This method works across multiple Gateways by sending data to every connected Gateway where we
      have at least one user connected and added to our GroupID with the "AddUserToMyGroup" method.

      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000,
      they will be used in future updates to extend the components with new features.

      Returns TRUE if connection component was found and the package was placed in its sending buffers. }
    function SendToMyGroup(GroupID:TGateUID; CallID:word):boolean; overload;

    { Leave Group "GroupID" owned by user "OwnerAddr" (OwnerID@GateAddr:GatePort).
      Returns TRUE if connection component was found and the command was placed in its sending buffers. }
    function LeaveUsersGroup(const OwnerAddr:RtcString; GroupID:TGateUID):boolean;

    { Add User "UserAddr" as our Friend, giving the User
      permission to add us to his User Group(s) on the Gateway.
      If the user "UserAddr" is online and was NOT already in our Friends list,
      we will receive the "gc_FriendAdd" command from the Gateway and
      the user "UserAddr" will receive the "gc_BeFriend" command.
      If the user is off-line, we will receive a "gc_UserOffLine" command.
      If the user is already in our friends list, nothing changes and
      the Gateway won't send any notification messages to us or the user.

      Returns TRUE if connection component was found and
      the command was placed in its sending buffers. }
    function AddFriend(const UserAddr:RtcString):boolean;

    { Remove User "UserAddr" from our Friends list, revoking Users
      permissions to add us to his User Group(s) on the Gateway.
      If user "UserAddr" was in our Friends list on the Gateway,
      we will receive the "gc_FriendRemove" command from the Gateway
      and the user "UserAddr" will receive the "gc_UnFriend" command.
      Removing a User from our Friends list will not close open User Groups,
      but it will prevent the User from adding us to more Groups.
      NOTE: Removing Friends is not necessary, because our Frields list will
      be cleared on the Gateway if a User logs out or looses connection.

      Returns TRUE if connection component was found and
      the command was placed in its sending buffers. }
    function RemoveFriend(const UserAddr:RtcString):boolean;

    { Remove all Users from our Friends list (MyAddr = MyUID@GateAddr:GatePort) on the Gateway,
      revoking all Users permissions for adding us to their User Group(s) on the Gateway.
      If our Friends list on the Gateway wasn't empty, we will receive
      the "gc_FriendRemove" command for every user removed from our list
      and all removed users will receive the "gc_UnFriend" command.
      Removing Users from our Friends list will not close open User Groups,
      but it will prevent Users from adding us to more Groups.
      NOTE: Removing Friends is not necessary, because our Frields list will
      be cleared on the Gateway if a User logs out or looses connection.

      Returns TRUE if connection component was found and
      the command was placed in its sending buffers. }
    function RemoveAllFriends(const MyAddr:RtcString):boolean;

    { Remove all Users from all our Friends lists on all the connected Gateways,
      revoking all Users permissions for adding us to their User Group(s).
      If our Friends list on a Gateway wasn't empty, we will receive
      the "gc_FriendRemove" command for every user removed from our list
      and all removed users will receive the "gc_UnFriend" command.
      Removing Users from our Friends list will not close open User Groups,
      but it will prevent Users from adding us to more Groups.
      NOTE: Removing Friends is not necessary, because our Frields list will
      be cleared on each Gateway if a User logs out or looses connection.

      Returns TRUE if at least one connection component was found
      and the command was placed in its sending buffers. }
    function RemoveAllMyFriends:boolean;

    { Request verification for all "Linked" accounts logged in with "UserAddr" which have NOT beed verified.
      Returns TRUE if at least one verification request was sent. "OnInfoReceived" events will be triggered
      with "Data.Command" = "gc_AccountVerified" for all remote Accounts where verification was successful. @html(<br><br>)

      This is used for manual User Account verification, in case you do NOT want all Accounts to be verified.
      If you DO want ALL "Linked" (remote Private) accounts to be verified immediately on Login,
      just set the "AutoVerifyAccounts:=TRUE" on the "AccountManager" assigned to this component.

      Returns TRUE if connection component was found and the command was placed in its sending buffers. }
    function RequestUserVerifications(const UserAddr:RtcString):boolean;

    { Request verification for the "Linked" account "AccountID" logged in with "UserAddr" (UserID@GateAddr:GatePort),
      but NOT yet verified. Returns TRUE if the verification request was sent (account was found but NOT verified).
      "OnInfoReceived" event will be triggered with "Data.Command" = "gc_AccountVerified"
      if remote Account verification was successful. @html(<br><br>)

      This is used for manual Account verification, in case you do NOT want all Accounts to be verified.
      If you DO want ALL "Linked" (remote Private) accounts to be verified immediately on Login,
      just set the "AutoVerifyAccounts:=TRUE" on the "AccountManager" assigned to this component.

      Returns TRUE if connection component was found and the command was placed in its sending buffers. }
    function RequestAccountVerification(const UserAddr:RtcString; const AccountID:RtcString):boolean;

    { Send Account Data to User "UserAddr" (UserID@GateAddr:GatePort),
      generated with "PrepareCopy" or "PrepareLink" method.

      "ReceiverAuthCode" parameter should be a currently valid "AuthCode",
      generated by receiver's "AccountManager" or "MultiGateClient" component.

      Returns TRUE if the Account Data was sent.

      "OnInfoReceived" event will be triggered with "Data.Command" = "gc_AccountReceived" on the receiver side
      and "Data.AccountID" containing the newly registered Account ID if the Account was received correctly,
      or ... with "Data.Command" = "gc_AccountBadReceive" if Account Data was corrupt or the "AuthCode" was wrong.

      "OnInfoReceived" event will be triggered with "Data.Command" = "gc_AccountTransferred" on our side and
      "Data.AccountID" containing the Account ID (or Link) that was transferred successfully, or ... with
      "Data.Command" = "gc_AccountBadTransfer" if transfer confirmation was wrong or the Account was deleted. }
    function SendAccountData(const UserAddr:RtcString; const ReceiverAuthCode:RtcString; const AccountData:RtcByteArray):boolean;

    { Is the User "UserAddr" (UserID@GateAddr:GatePort) NOT (fully) verified?
      Returns TRUE if the User is logged in on at least one "Linked" (remote Private)
      account for which we have NOT yet received a positive account verification.
      If this property returns TRUE, you can use the "RequestUserVerifications" method
      to ask the User to verify all logged in but NOT yet verified "Linked" accounts. }
    function UserNotVerified(const UserAddr:RtcString):boolean;

    { Is the User "UserAddr" (UserID@GateAddr:GatePort) allowed to perform Action "action"?
      Based on Permissions for "Public" accounts and VERIFIED "Private" or "Linked" Accounts. }
    function UserAllowed(const UserAddr:RtcString; const action:word):boolean;

    { Is the User "UserAddr" (UserID@GateAddr:GatePort) *verified* allowed to perform Action "action"?
      Based on Permissions for logged in and *VERIFIED* "Private" and "Linked" accounts
      ("Public" permissions excluded). }
    function VerifiedAllowed(const UserAddr:RtcString; const action:word):boolean;

    { Is the User "UserAddr" (UserID@GateAddr:GatePort)
        *maybe* (account logged in but NOT verified) allowed to perform Action "action"?
      Based on Permissions for all logged in accounts (Public, Linked and Private),
      regardless of their verification status. }
    function UnVerifiedAllowed(const UserAddr:RtcString; const action:word):boolean;

    { Is the User "UserAddr" (UserID@GateAddr:GatePort) logged in on a NOT yet VERIFIED account "AccountID"?
      Returns TRUE if the User is logged in with the Account "AccountID", the account is of type
      "Linked" (remote Private) or "Private", but a verification has NOT been received for that account.
      If this property returns TRUE and the account is "Linked" (remote Private), you can use
      the "RequestAccountVerification" method to ask for account verification from the User.
      For "Private" accounts, the remote User has to request account verification from us. }
    function AccountNotVerified(const UserAddr:RtcString; const AccountID:RtcString):boolean;

    { Is the User "UserAddr" (UserID@GateAddr:GatePort) logged in with the Account "AccountID"?
      Returns TRUE for logged in Public accounts and VERIFIED "Private" or "Linked" accounts. }
    function UserAccount(const UserAddr:RtcString; const AccountID:RtcString):boolean;

    { Returns the last received "UserInfo" string for the User "UserAddr" (UserID@GateAddr:GatePort).
      The "UserInfo" string is received from the Gateway with Account notifications, contains the
      "GateUserInfo" property assigned on the Client for which the notification was sent and remains
      populated while the User is logged in with at least one Account managed by this component. }
    function UserInfo(const UserAddr:RtcString):RtcString;

    { Is the User "UserAddr" (UserID@GateAddr:GatePort) logged in with a VERIFIED account "AccountID"?
      Returns TRUE only for logged in and VERIFIED "Private" and "Linked" accounts.
      Returns FALSE for "Public" accounts and NOT verified "Private" or "Linked" accounts. }
    function VerifiedAccount(const UserAddr:RtcString; const AccountID:RtcString):boolean;

    { Is the User "UserAddr" (UserID@GateAddr:GatePort)
        *maybe* logged in (account NOT verified) with the Account "AccountID"?
      Returns TRUE for all logged in Accounts ("gc_AccountLogIn" notification has been received). }
    function UnVerifiedAccount(const UserAddr:RtcString; const AccountID:RtcString):boolean;

    { Detailed connection state information for the currently selected Client }
    property State:TRtcGateClientStateInfo read GetState;

    { My IP address of the currently selected Client,
      returned from the Gateway after login }
    property MyIP:RtcString read GetMyIP;

    { My Encryption Key of the currently selected Client,
      returned from the Gateway after login }
    property MyKey:RtcString read GetMyKey;

    { My User ID of the currently selected Client,
      returned from the Gateway after login }
    property MyUID:TGateUID read GetMyUID;

    { My User Address (MyUID @ GateAddr : GatePort)
      of the currently selected Client, available after login }
    property MyAddr:RtcString read GetMyAddr;

    { Get: returns the currently selected Client component.
      Set: sets this HttpMultiClient as the AccountManager for the
           assigned Client component and makes the Client component selected. }
    property Client:TRtcHttpGateClient read GetActiveClient write SetActiveClient;

    { Returns "Ready" state (ready to send and receive)
      for the currently selected Client. }
    property Ready:boolean read GetClReady;

  published
    { Set Active=TRUE to connect the user to the Gateway and stay connected until LogOut or LoginFail.
      Set Active=FALSE to disconnect the user from the Gateway.
          If AutoLogin=True, a new connection will be established imediately after LogOut.
          If AutoLogin=False, this becomes the "Default" Client component. }
    property Active:boolean read GetClActive write SetClActive default False;

    { Set AutoLogin=TRUE to make sure the user is connected
          and stays connected, even after LogOut or LoginFail.
      Set AutoLogin=FALSE to disconnect the user from the Gateway
          and stop reconnecting after LogOut and LoginFail.
      Setting AutoLogin=FALSE will also make this the "Default" Client component. }
    property AutoLogin:boolean read GetAutoRelogin write SetAutoRelogin default False;

    { Properties to set before use }

    { Gateway Address (IP or Domain name, without the "http://" or "https://" prefix).
      Changing this property triggers a search for a
      Client component with matching GateAddr and GatePort properties.
      * 1. If a Client with matching GateAddr and GatePort properties is NOT found, then:
      > 1.A) If the currently selected Client's "Active" and "AutoLogin" properties are FALSE,
             the "GateAddr" property of the currently selected Client is updated.
      > 1.B) If the currently selected Client's "Active" or "AutoLogin" property is TRUE,
             a NEW Client component will be created, all properties from the currently selected
             Client will be copied to the NEW Client and the NEW Client will then be selected.
      * 2. If a Client with matching GateAddr and GatePort properties is found, then:
      > 2.A) If the currently selected Client's "Active" and "AutoLogin" properties are FALSE,
             the Client with matching GateAddr and GatePort properties will be selected
             and the Client component which was previously selected will be DESTROYED (garbage-collected).
      > 2.B) If the currently selected Client's "Active" or "AutoLogin" properties are TRUE,
             then the Client with matching GateAddr and GatePort properties becomes selected,
             without making any changes to (or destroying) the previously selected Client component. }
    property GateAddr:RtcString read GetGateAddr write SetGateAddr;

    { Gateway Port number.
      Changing this property triggers a search for a
      Client component with matching GateAddr and GatePort properties.
      * 1. If a Client with matching GateAddr and GatePort properties is NOT found, then:
      > 1.A) If the currently selected Client's "Active" and "AutoLogin" properties are FALSE,
             the "GateAddr" property of the currently selected Client is updated.
      > 1.B) If the currently selected Client's "Active" or "AutoLogin" property is TRUE,
             a NEW Client component will be created, all properties from the currently selected
             Client will be copied to the NEW Client and the NEW Client will then be selected.
      * 2. If a Client with matching GateAddr and GatePort properties is found, then:
      > 2.A) If the currently selected Client's "Active" and "AutoLogin" properties are FALSE,
             the Client with matching GateAddr and GatePort properties will be selected
             and the Client component which was previously selected will be DESTROYED (garbage-collected).
      > 2.B) If the currently selected Client's "Active" or "AutoLogin" properties are TRUE,
             then the Client with matching GateAddr and GatePort properties becomes selected,
             without making any changes to (or destroying) the previously selected Client component. }
    property GatePort:RtcString read GetGatePort write SetGatePort;

    // Gateway "IP Version" prefference
    property GateIPV:RtcIPV read GetGateIPV write SetGateIPV default rtc_IPVDefault;

    // FileName ("/" for root) where the TRtcGateway component is accessible
    property GateFileName:RtcString read GetGateFileName write SetGateFileName;

    // Primary Encryption Key set up on the TRtcGateway component (leave empty for default encryption)
    property GatePrimaryKey:RtcString read GetGatePrimaryKey write SetGatePrimaryKey;

   { Secondary Encryption Key, specific to this Client.
      Leave Empty for default key generation, or implement the BeforeUserLogin event
      on the TRtcGateway component to set the same value on the Gateway for this Client. }
    property GateSecondaryKey:RtcString read GetGateSecondaryKey write SetGateSecondaryKey;

    { User Authentication information, packed into a single String,
      which will be made available in all events on the TRtcGateway component and
      can be used to identify this Client or add the Client to specific User Groups. }
    property GateUserAuth:RtcString read GetGateUserAuth write SetGateUserAuth;

    { *PUBLIC* User information (no Passwords!), packed into a single String,
      which will be made available in all events on the TRtcGateway component,
      forwarded to all Clients using Subscription and Account notification mechanics
      and can be used to identify the Client or add the Client to specific User Groups. }
    property GateUserInfo:RtcString read GetGateUserInfo write SetGateUserInfo;

    // Use Blocking connection provider
    property UseBlocking:boolean read GetUseBlocking write SetUseBlocking default False;

    // Use Proxy-compatible connection provider
    property UseProxy:boolean read GetUseProxy write SetUseProxy default False;

    // Use WinHTTP on Windows
    property UseWinHTTP:boolean read GetUseWinHTTP write SetUseWinHTTP default False;

    // Use SSL-compatible connection provider
    property UseSSL:boolean read GetUseSSL write SetUseSSL default False;

    { To use SSL/SSH encryption with third-party components, simply assign the encryption
      plug-in here before you start using the Client connection (before first connect). }
    property UseCryptPlugin:TRtcCryptPlugin read GetCryptPlugin write SetCryptPlugin;

    { UserLogin data is ignored when UseCryptPlugin is assigned. @html(<br><br>)

      If UseCryptPlugin is NOT assigned (not using third-party components for encryption) ... @html(<br><br>)

      Using this property, you can define login information for a server which
      requires user authentication and/or a client certificate (using WinInet API).
      If you want to use Third-party SSL/SSH components for encryption,
      simply assign the plug-in to the UseCryptPlugin property. }
    property UserLogin:TRtcUserLoginInfo read GetUserLogin write SetUserLogin;

    { Input Stream Block Size. By default (0) use DEF_OUTSTREAM_SIZE = 2 GB @html(<br><br>)

      For best possible performance, use 0 (default) or large blocks. To avoid problems
      with Anti-Virus software and Proxy Servers, use a small number higher than zero. @html(<br><br>)

      If more than the specified "StreamBlockSizeIn" bytes need to be sent from the Gateway
      when a new response is being prepared, the content length for the current response
      will be set to include the complete current output buffer on the Gateway and avoid
      unnecessary packet splitting, improving streaming performance where possible.  }
    property StreamBlockSizeIn:Cardinal read GetStreamBlockSizeIn write SetStreamBlockSizeIn default 0;

    { Output Stream Block Size. By default (0) use DEF_OUTSTREAM_SIZE = 2 GB @html(<br><br>)

      For best possible performance, use 0 (default) or large blocks. To avoid problems
      with Anti-Virus software and Proxy Servers, use a small number higher than zero. @html(<br><br>)

      If the Client has to work on Systems with Anti-Virus Software checking internet traffic,
      set the "StreamBlockSizeOut" property to a small value higher than zero for automatic packet
      size calculation. Keep the value below the maximum number of bytes you want to allow Anti-Virus
      Software to store in its internal buffers before sending it out to the Gateway. @html(<br><br>)

      This limit is achieved by limiting the outgoing request size and thus forcing
      the Client to wait for a response from the Gateway before more data is sent.  @html(<br><br>)

      If more than the specified "StreamBlockSizeOut" bytes need to be sent out when a new
      request is being prepared, the content length for the current request will be set to
      include the complete current output buffer instead of limiting the request size to the
      specified value, to avoid unnecessary packet splitting and improve streaming performance. }
    property StreamBlockSizeOut:Cardinal read GetStreamBlockSizeOut write SetStreamBlockSizeOut default 0;

    { Speed limit (in KBits per second, 0 = unlimited) for streaming data from the currently selected Client.
      By setting a speed limit, the component will wait before sending more data out if sending
      more data would result in the last sending operation to exceed the specified speed limit.
      This speed limit does NOT affect the way "SendBytes" or "SendToGroup" methods work (they are 
      always non-blocking and asynchronous) and it does NOT split larger packets into even smaller 
      chunks to reduce the bandwidth usage, but ONLY forces a delay in the background thread 
      where data is being sent, to keep the outgoing bandwidth usage below the specified limit. }
    property StreamSpeedLimit:Cardinal read GetStreamSpeedLimit write SetStreamSpeedLimit default 0;
  end;

{ Split "UserAddr" (UserID @ GateAddr : GatePort) into GateAddr, GatePort and return the UserID. }
function SplitUserAddr(const UserAddr:RtcString; var GateAddr,GatePort:RtcString):TGateUID;

{ Extract the UserID from "UserAddr" (UserID @ GateAddr : GatePort) }
function ExtractUserID(const UserAddr:RtcString):TGateUID;

implementation

{ TRtcMCDataJob }

type
  TRtcMCDataJob=class(TRtcJob)
  private
    FData:TRtcGateClientData;
    FBTEvent:TRtcGateClientBTDataEvent;
    FGUIEvent:TRtcGateClientGUIDataEvent;
    FComp:TRtcAbsGateClientThread;
    FClient:TRtcHttpGateClient;
    FThr:TRtcThread;
  public
    destructor Destroy; override;

    function BT_Execute:boolean;
    procedure GUI_Execute;

    function Run(Thr:TRtcThread):boolean; override;
    end;

destructor TRtcMCDataJob.Destroy;
  begin
  RtcFreeAndNil(FData);
  inherited;
  end;

function TRtcMCDataJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    if BT_Execute then
      if assigned(FGUIEvent) then
        TRtcThread.Sync(GUI_Execute);
  finally
    if assigned(FData) then
      RtcFreeAndNil(FData);
    Result:=True;
    end;
  end;

function TRtcMCDataJob.BT_Execute:boolean;
  begin
  try
    if assigned(FBTEvent) then
      begin
      Result:=False;
      if TRtcThread.Exists(FThr) then
        if FComp.FNotSilent then
          FBTEvent(FClient,FData,Result);
      end
    else
      Result:=True;
  except
    // ignore all exceptions here
    end;
  end;

procedure TRtcMCDataJob.GUI_Execute;
  begin
  try
    if TRtcThread.Exists(FThr) then
      if FComp.FNotSilent then
        FGUIEvent(FClient,FData);
  except
    // ignore all exceptions here
    end;
  end;

procedure PostGCJob(BTEvent:TRtcGateClientBTDataEvent;
                    GUIEvent:TRtcGateClientGUIDataEvent;
                    Comp:TRtcAbsGateClientThread;
                    Client:TRtcHttpGateClient;
                    Data:TRtcGateClientData=nil); overload;
  var
    Job:TRtcMCDataJob;
  begin
  if not assigned(BTEvent) and not assigned(GUIEvent) then Exit;
  if not assigned(Comp) then Exit;
  if not assigned(Client) then Exit;
  if not TRtcThread.Exists(Comp.Thread) then Exit;

  Job:=TRtcMCDataJob.Create;
  if assigned(Data) then
    begin
    Job.FData:=TRtcGateClientData.Create;
    Job.FData.CopyFrom(Data);
    end;
  Job.FBTEvent:=BTEvent;
  Job.FGUIEvent:=GUIEvent;
  Job.FComp:=Comp;
  Job.FClient:=Client;
  Job.FThr:=Comp.Thread;

  if not TRtcThread.PostJob(Job.FThr,Job) then
    if Job.SingleUse then
      RtcFreeAndNil(Job);
  end;

procedure PostGCJob(BTEvent:TRtcGateClientBTDataEvent;
                    GUIEvent:TRtcGateClientGUIDataEvent;
                    Comp:TRtcAbsGateClientThread;
                    Sender:TRtcConnection); overload;
  var
    Client:TRtcHttpGateClient;
  begin
  if not assigned(Sender) then Exit;
  if not assigned(Sender.Owner) then Exit;
  if not (Sender.Owner is TRtcHttpGateClient) then Exit;
  Client:=TRtcHttpGateClient(Sender.Owner);
  PostGCJob(BTEvent,GUIEvent,Comp,Client,Client.FData);
  end;

{ TRtcMCStateJob }

type
  TRtcMCStateJob=class(TRtcJob)
  private
    FData:TRtcGateClientStateInfo;
    FBTEvent:TRtcGateClientBTStateEvent;
    FGUIEvent:TRtcGateClientGUIStateEvent;
    FComp:TRtcAbsGateClientThread;
    FClient:TRtcHttpGateClient;
    FThr:TRtcThread;
  public
    destructor Destroy; override;

    function BT_Execute:boolean;
    procedure GUI_Execute;

    function Run(Thr:TRtcThread):boolean; override;
    end;

destructor TRtcMCStateJob.Destroy;
  begin
  RtcFreeAndNil(FData);
  inherited;
  end;

function TRtcMCStateJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    if BT_Execute then
      if assigned(FGUIEvent) then
        TRtcThread.Sync(GUI_Execute);
  finally
    if assigned(FData) then
      RtcFreeAndNil(FData);
    Result:=True;
    end;
  end;

function TRtcMCStateJob.BT_Execute:boolean;
  begin
  try
    if assigned(FBTEvent) then
      begin
      Result:=False;
      if TRtcThread.Exists(FThr) then
        if FComp.FNotSilent then
          FBTEvent(FClient,FData,Result);
      end
    else
      Result:=True;
  except
    // ignore all exceptions here
    end;
  end;

procedure TRtcMCStateJob.GUI_Execute;
  begin
  try
    if TRtcThread.Exists(FThr) then
      if FComp.FNotSilent then
        FGUIEvent(FClient,FData);
  except
    // ignore all exceptions here
    end;
  end;

procedure PostGCJob(BTEvent:TRtcGateClientBTStateEvent;
                    GUIEvent:TRtcGateClientGUIStateEvent;
                    Comp:TRtcAbsGateClientThread;
                    Client:TRtcHttpGateClient;
                    Data:TRtcGateClientStateInfo=nil); overload;
  var
    Job:TRtcMCStateJob;
  begin
  if not assigned(BTEvent) and not assigned(GUIEvent) then Exit;
  if not assigned(Comp) then Exit;
  if not assigned(Client) then Exit;
  if not TRtcThread.Exists(Comp.Thread) then Exit;

  Job:=TRtcMCStateJob.Create;
  if assigned(Data) then
    begin
    Job.FData:=TRtcGateClientStateInfo.Create;
    Job.FData.CopyFrom(Data);
    end;
  Job.FBTEvent:=BTEvent;
  Job.FGUIEvent:=GUIEvent;
  Job.FComp:=Comp;
  Job.FClient:=Client;
  Job.FThr:=Comp.Thread;

  if not TRtcThread.PostJob(Job.FThr,Job) then
    if Job.SingleUse then
      RtcFreeAndNil(Job);
  end;

procedure PostGCJob(BTEvent:TRtcGateClientBTStateEvent;
                    GUIEvent:TRtcGateClientGUIStateEvent;
                    Comp:TRtcAbsGateClientThread;
                    Sender:TRtcConnection); overload;
  var
    Client:TRtcHttpGateClient;
  begin
  if Sender=nil then Exit;
  if Sender.Owner=nil then Exit;
  if not (Sender.Owner is TRtcHttpGateClient) then Exit;
  Client:=TRtcHttpGateClient(Sender.Owner);
  PostGCJob(BTEvent,GUIEvent,Comp,Client,Client.State);
  end;

{ TRtcGateClientLinkList }

constructor TRtcGateClientLinkList.Create;
  begin
  inherited;
  FList:=TObjList.Create(16);
  CS:=TRtcCritSec.Create;
  end;

destructor TRtcGateClientLinkList.Destroy;
  begin
  try
    RtcFreeAndNil(FList);
    RtcFreeAndNil(CS);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcGateClientLinkList.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcGateClientLinkList.Count: integer;
  begin
  if (self=nil) or not assigned(CS) then
    Result:=0
  else
    begin
    CS.Acquire;
    try
      if assigned(FList) then
        Result:=FList.Count
      else
        Result:=0;
    finally
      CS.Release;
      end;
    end;
  end;

procedure TRtcGateClientLinkList.Add(Value: TRtcAbsGateClientLink);
  begin
  if (self=nil) or not assigned(CS) then Exit;
  CS.Acquire;
  try
    if assigned(FList) then
      if FList.search(RtcIntPtr(Value))=nil then
        FList.insert(RtcIntPtr(Value),Value);
  finally
    CS.Release;
    end;
  end;

function TRtcGateClientLinkList.LockFirst:TRtcAbsGateClientLink;
  var
    i:RtcIntPtr;
    a:TObject;
    l:TRtcAbsGateClientLink absolute a;
  begin
  Result:=nil;
  if (self=nil) or not assigned(CS) then Exit;
  CS.Acquire;
  try
    if assigned(FList) then
      begin
      i:=FList.search_min(a);
      if (i>0) and assigned(a) then
        begin
        Result:=l;
        Inc(Result.FLocked);
        end;
      end;
  finally
    CS.Release;
    end;
  end;

procedure TRtcGateClientLinkList.LockNext(var Value:TRtcAbsGateClientLink);
  var
    i:RtcIntPtr;
    a:TObject;
    l:TRtcAbsGateClientLink absolute a;
    Removed:TRtcAbsGateClientLink;
  begin
  Removed:=nil;
  if (self=nil) or not assigned(CS) then
    begin
    Value:=nil;
    Exit;
    end;
  CS.Acquire;
  try
    Dec(Value.FLocked);
    if Value.FLocked=0 then
      if FList.search(RtcIntPtr(Value))=nil then
        Removed:=Value;
    if assigned(FList) then
      begin
      i:=FList.search_g(RtcIntPtr(Value),a);
      if (i>0) and assigned(a) then
        begin
        Value:=l;
        Inc(Value.FLocked);
        end
      else
        Value:=nil;
      end
    else
      Value:=nil;
  finally
    CS.Release;
    end;
  if assigned(Removed) then
    Removed.Sync_ClientRemoved;
  end;

procedure TRtcGateClientLinkList.Remove(Value: TRtcAbsGateClientLink);
  var
    Removed:TRtcAbsGateClientLink;
  begin
  Removed:=nil;
  if (self=nil) or not assigned(CS) then Exit;
  CS.Acquire;
  try
    if assigned(FList) then
      if FList.search(RtcIntPtr(Value))=Value then
        begin
        FList.remove(RtcIntPtr(Value));
        if Value.FLocked=0 then
          Removed:=Value;
        end;
  finally
    CS.Release;
    end;
  if assigned(Removed) then
    Removed.Sync_ClientRemoved;
  end;

procedure TRtcGateClientLinkList.RemoveAll;
  var
    Link:TRtcAbsGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    Remove(Link);
    LockNext(Link);
    end;
  end;

procedure TRtcGateClientLinkList.DoAfterLoggedIn(Sender: TRtcConnection);
  var
    Link:TRtcAbsGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_AfterLoggedIn(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcGateClientLinkList.DoAfterLoginFail(Sender: TRtcConnection);
  var
    Link:TRtcAbsGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_AfterLoginFail(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcGateClientLinkList.DoAfterLogOut(Sender: TRtcConnection);
  var
    Link:TRtcAbsGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_AfterLogOut(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcGateClientLinkList.DoBeforeLogIn(Sender: TRtcConnection);
  var
    Link:TRtcAbsGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_BeforeLogIn(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcGateClientLinkList.DoDataReceived(Sender: TRtcConnection);
  var
    Link:TRtcAbsGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_OnDataReceived(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcGateClientLinkList.DoInfoReceived(Sender: TRtcConnection);
  var
    Link:TRtcAbsGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_OnInfoReceived(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcGateClientLinkList.DoReadyToSend(Sender: TRtcConnection);
  var
    Link:TRtcAbsGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_OnReadyToSend(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcGateClientLinkList.DoReadyAfterReset(Sender: TRtcConnection);
  var
    Link:TRtcAbsGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_OnReadyAfterReset(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcGateClientLinkList.DoStreamReset(Sender: TRtcConnection);
  var
    Link:TRtcAbsGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_OnStreamReset(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

{ TRtcAbsGateClientLink }

constructor TRtcAbsGateClientLink.Create(AOwner: TComponent);
  begin
  inherited;
  FMyGroupID:=0;
  FClient:=nil;
  FUserGroups:=TRtcGateUserGroupStates.Create;
  end;

destructor TRtcAbsGateClientLink.Destroy;
  begin
  Client:=nil;
  RtcFreeAndNil(FUserGroups);
  inherited;
  end;

function TRtcAbsGateClientLink.GetClient: TRtcHttpGateClient;
  begin
  Result:=FClient;
  end;

procedure TRtcAbsGateClientLink.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FClient then
      begin
      FMyGroupID:=0;
      Sync_ClientRemoved;
      end;
  end;

function TRtcAbsGateClientLink.GetMyGroupID: TGateUID;
  begin
  Result:=FMyGroupID;
  end;

procedure TRtcAbsGateClientLink.SetMyGroupID(const Value: TGateUID);
  begin
  if Value<>FMyGroupID then
    begin
    if assigned(FClient) then
      begin
      if FMyGroupID>0 then
        begin
        if FClient.MyUID>0 then
          FClient.RemoveGroup(FClient.MyUID,FMyGroupID);
        FClient.SharedGroups.ReleaseID(FMyGroupID);
        FMyGroupID:=0;
        end;
      FMyGroupID:=FClient.SharedGroups.AllocFreeID(Value);
      end;
    end;
  end;

procedure TRtcAbsGateClientLink.RemoveClient(Value: TRtcHttpGateClient);
  begin
  if Value=FClient then Client:=nil;
  end;

procedure TRtcAbsGateClientLink.SetClient(const Value: TRtcHttpGateClient);
  begin
  if Value<>FClient then
    begin
    if assigned(FClient) then
      begin
      if FMyGroupID>0 then
        begin
        if FClient.MyUID>0 then
          FClient.RemoveGroup(FClient.MyUID,FMyGroupID);
        FClient.SharedGroups.ReleaseID(FMyGroupID);
        FMyGroupID:=0;
        end;
      FClient.RemoveGateClientLink(self);
      end;
    if assigned(Value) and not assigned(FClient) then
      begin
      { Allocate the first available Group ID.
        If Group ID allocation fails, which can happen if all
        4095 Group IDs have already been taken, an exception will
        be raised and the "MultiClient" property will remain "NIL". }
      FMyGroupID:=Value.SharedGroups.AllocFreeID(0);
      FClient:=Value;
      FClient.AddGateClientLink(self);
      Call_ClientAssigned;
      end;
    end;
  end;

function TRtcAbsGateClientLink.GetUserGroups: TRtcGateUserGroupStates;
  begin
  Result:=FUserGroups;
  end;

procedure TRtcAbsGateClientLink.Call_ClientAssigned;
  begin
  end;

procedure TRtcAbsGateClientLink.Call_ClientRemoved;
  begin
  FClient:=nil;
  end;

procedure TRtcAbsGateClientLink.Sync_ClientRemoved;
  begin
  if InsideMainThread then
    Call_ClientRemoved
  else
    PostGUIJob(Call_ClientRemoved);
  end;

function TRtcAbsGateClientLink.PingUser(const UserID: TGateUID): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.PingUser(UserID);
  end;

function TRtcAbsGateClientLink.AddUserToGroup(const UserID: TGateUID): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.AddUserToMyGroup(FMyGroupID,UserID);
  end;

function TRtcAbsGateClientLink.RemoveUserFromGroup(const UserID: TGateUID): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.RemoveUserFromMyGroup(FMyGroupID,UserID);
  end;

function TRtcAbsGateClientLink.IsUserInGroup(const UserID: TGateUID): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.IsUserInMyGroup(FMyGroupID,UserID);
  end;

function TRtcAbsGateClientLink.UsersInGroup: integer;
  begin
  Result:=0;
  if FClient=nil then Exit;
  Result:=FClient.UsersInMyGroup(FMyGroupID);
  end;

function TRtcAbsGateClientLink.DisbandGroup: boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.DisbandMyGroup(FMyGroupID);
  end;

function TRtcAbsGateClientLink.SendBytes(const UserID: TGateUID;
                                         GroupID: TGateUID; CallID: word;
                                         const data: RtcByteArray): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.SendBytes(UserID,GroupID,CallID,data);
  end;

function TRtcAbsGateClientLink.SendBytes(const UserID: TGateUID;
                                         GroupID: TGateUID; CallID: word;
                                         const data: RtcString): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.SendBytes(UserID,GroupID,CallID,data);
  end;

function TRtcAbsGateClientLink.SendBytes(const UserID: TGateUID;
                                         GroupID: TGateUID; CallID: word): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.SendBytes(UserID,GroupID,CallID);
  end;

function TRtcAbsGateClientLink.SendToGroup(CallID: word; const data: RtcByteArray): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.SendToMyGroup(FMyGroupID,CallID,data);
  end;

function TRtcAbsGateClientLink.SendToGroup(CallID: word; const data: RtcString): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.SendToMyGroup(FMyGroupID,CallID,data);
  end;

function TRtcAbsGateClientLink.SendToGroup(CallID: word): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.SendToMyGroup(FMyGroupID,CallID);
  end;

function TRtcAbsGateClientLink.UserAllowed(const UserID: TGateUID; const action: word): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.UserAllowed[UserID,action];
  end;

function TRtcAbsGateClientLink.UserInfo(const UserID: TGateUID): RtcString;
  begin
  Result:='';
  if FClient=nil then Exit;
  Result:=FClient.UserInfo[UserID];
  end;

function TRtcAbsGateClientLink.UserNotVerified(const UserID: TGateUID): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.UserNotVerified[UserID];
  end;

function TRtcAbsGateClientLink.VerifyUser(const UserID: TGateUID): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.RequestUserVerifications(UserID);
  end;

function TRtcAbsGateClientLink.AddFriend(const UserID: TGateUID): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.AddFriend(UserID);
  end;

function TRtcAbsGateClientLink.RemoveFriend(const UserID: TGateUID): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.RemoveFriend(UserID);
  end;

function TRtcAbsGateClientLink.LeaveUsersGroup(const OwnerID: TGateUID; const GroupID: TGateUID): boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.LeaveUsersGroup(OwnerID,GroupID);
  end;

function TRtcAbsGateClientLink.MyUID: TGateUID;
  begin
  Result:=0;
  if FClient=nil then Exit;
  Result:=FClient.MyUID;
  end;

function TRtcAbsGateClientLink.Ready: boolean;
  begin
  Result:=False;
  if FClient=nil then Exit;
  Result:=FClient.Ready;
  end;

{ TRtcGateClientLink }

procedure TRtcGateClientLink.Call_AfterLoggedIn(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConAfterLoggedIn,FGuiAfterLoggedIn,Sender) then
    if assigned(FThread) then
      FThread.Post_AfterLoggedIn(Sender);
  end;

procedure TRtcGateClientLink.Call_AfterLoginFail(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConAfterLoginFail,FGuiAfterLoginFail,Sender) then
    if assigned(FThread) then
      FThread.Post_AfterLoginFail(Sender);
  end;

procedure TRtcGateClientLink.Call_AfterLogOut(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConAfterLogOut,FGuiAfterLogOut,Sender) then
    if assigned(FThread) then
      FThread.Post_AfterLogOut(Sender);
  end;

procedure TRtcGateClientLink.Call_BeforeLogIn(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConBeforeLogIn,FGuiBeforeLogIn,Sender) then
    if assigned(FThread) then
      FThread.Post_BeforeLogIn(Sender);
  end;

procedure TRtcGateClientLink.Call_OnDataReceived(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Filter(FOnDataFilter,Sender) then
    if Call(FConDataReceived,FGuiDataReceived,Sender) then
      if assigned(FThread) then
        FThread.Post_DataReceived(Sender);
  end;

procedure TRtcGateClientLink.Call_OnInfoReceived(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Filter(FOnInfoFilter,Sender) then
    if Call(FConInfoReceived,FGuiInfoReceived,Sender) then
      if assigned(FThread) then
        FThread.Post_InfoReceived(Sender);
  end;

procedure TRtcGateClientLink.Call_OnReadyToSend(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConReadyToSend,FGuiReadyToSend,Sender) then
    if assigned(FThread) then
      FThread.Post_ReadyToSend(Sender);
  end;

procedure TRtcGateClientLink.Call_OnReadyAfterReset(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConReadyAfterReset,FGuiReadyAfterReset,Sender) then
    if assigned(FThread) then
      FThread.Post_ReadyAfterReset(Sender);
  end;

procedure TRtcGateClientLink.Call_OnStreamReset(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConStreamReset,FGuiStreamReset,Sender) then
    if assigned(FThread) then
      FThread.Post_StreamReset(Sender);
  end;

procedure TRtcGateClientLink.Call_ClientAssigned;
  begin
  inherited;
  if FAmSilent then Exit;
  if assigned(FClientAssigned) then
    FClientAssigned;
  end;

procedure TRtcGateClientLink.Call_ClientRemoved;
  begin
  inherited;
  if FAmSilent then Exit;
  if assigned(FClientRemoved) then
    FClientRemoved;
  end;

{ TRtcGUIDataEvent }

type
  TRtcGUIDataEvent=class(TObject)
  public
    Event:TRtcGateClientGUIDataEvent;
    Client:TRtcHttpGateClient;
    Data:TRtcGateClientData;
    constructor Create(e:TRtcGateClientGUIDataEvent; s:TRtcConnection); overload;
    destructor Destroy; override;
    procedure Execute;
    end;

constructor TRtcGUIDataEvent.Create(e:TRtcGateClientGUIDataEvent; s:TRtcConnection);
  begin
  Event:=e;
  Client:=TRtcHttpGateClient(s.Owner);
  Data:=Client.FData;
  end;

destructor TRtcGUIDataEvent.Destroy;
  begin
  Event:=nil;
  Client:=nil;
  Data:=nil;
  inherited;
  end;

procedure TRtcGUIDataEvent.Execute;
  begin
  try
    if assigned(Event) then
      if Client.FNotSilent then
        Event(Client,Data);
  except
    // ignore all exceptions here
    end;
  end;

{ TRtcGUIStateEvent }

type
  TRtcGUIStateEvent=class(TObject)
  public
    Event:TRtcGateClientGUIStateEvent;
    Client:TRtcHttpGateClient;
    State:TRtcGateClientStateInfo;

    constructor Create(e:TRtcGateClientGUIStateEvent; s:TRtcConnection); overload;
    destructor Destroy; override;
    procedure Execute;
    end;

constructor TRtcGUIStateEvent.Create(e:TRtcGateClientGUIStateEvent; s:TRtcConnection);
  begin
  Event:=e;
  Client:=TRtcHttpGateClient(s.Owner);
  State:=Client.State;
  end;

destructor TRtcGUIStateEvent.Destroy;
  begin
  Event:=nil;
  Client:=nil;
  State:=nil;
  inherited;
  end;

procedure TRtcGUIStateEvent.Execute;
  begin
  try
    if assigned(Event) then
      if Client.FNotSilent then
        Event(Client,State);
  except
    // ignore all exceptions here
    end;
  end;

procedure TRtcGateClientLink.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FThread then
      FThread:=nil;
  end;

{ TRtcAbsGateClientThread }

constructor TRtcAbsGateClientThread.Create(AOwner:TComponent);
  begin
  inherited;
  FThr:=TRtcThread.Create;
  end;

destructor TRtcAbsGateClientThread.Destroy;
  begin
  TRtcThread.Stop(FThr);
  inherited;
  end;

procedure TRtcAbsGateClientThread.Post(BTEvent:TRtcGateClientBTDataEvent; GUIEvent: TRtcGateClientGUIDataEvent; Sender: TRtcConnection);
  begin
  if assigned(BTEvent) or assigned(GUIEvent) then
    PostGCJob(BTEvent, GUIEvent, self, Sender);
  end;

procedure TRtcAbsGateClientThread.Post(BTEvent:TRtcGateClientBTStateEvent; GUIEvent: TRtcGateClientGUIStateEvent; Sender: TRtcConnection);
  begin
  if assigned(BTEvent) or assigned(GUIEvent) then
    PostGCJob(BTEvent, GUIEvent, self, Sender);
  end;

procedure TRtcAbsGateClientThread.Post(BTEvent:TRtcGateClientBTDataEvent; GUIEvent: TRtcGateClientGUIDataEvent; Client: TRtcHttpGateClient; Data: TRtcGateClientData);
  begin
  if assigned(BTEvent) or assigned(GUIEvent) then
    PostGCJob(BTEvent, GUIEvent, self, Client, Data);
  end;

procedure TRtcAbsGateClientThread.Post(BTEvent:TRtcGateClientBTStateEvent; GUIEvent: TRtcGateClientGUIStateEvent; Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  if assigned(BTEvent) or assigned(GUIEvent) then
    PostGCJob(BTEvent, GUIEvent, self, Client, State);
  end;

function TRtcAbsGateClientThread.EventRunning: boolean;
  begin
  if assigned(FThr) then
    Result:=TRtcThread.JobRunning(FThr)
  else
    Result:=False;
  end;

function TRtcAbsGateClientThread.EventsInQueue: integer;
  begin
  if assigned(FThr) then
    Result:=TRtcThread.JobsInQueue(FThr)
  else
    Result:=0;
  end;

function TRtcAbsGateClientThread.EventsTotal: integer;
  begin
  if assigned(FThr) then
    Result:=TRtcThread.JobsTotal(FThr)
  else
    Result:=0;
  end;

function TRtcAbsGateClientThread.InBackThread: boolean;
  begin
  if assigned(Thread) then
    Result:=Thread.InBackThread
  else
    Result:=False;
  end;

function TRtcAbsGateClientThread.InMainThread: boolean;
  begin
  Result:=InsideMainThread;
  end;

function TRtcAbsGateClientThread.InsideThread: boolean;
  begin
  if assigned(Thread) then
    Result:=Thread.InsideThread
  else
    Result:=False;
  end;

procedure TRtcAbsGateClientThread.StopThread;
  begin
  TRtcThread.Stop(FThr);
  end;

{ TRtcAbsGateClient }

function TRtcAbsGateClient.Call(BTEvent: TRtcGateClientDataEvent; GUIEvent: TRtcGateClientGUIDataEvent; Sender: TRtcConnection):boolean;
  var
    WantGUI:boolean;
  begin
  Result:=True;
  if assigned(Sender) then
    if assigned(BTEvent) then
      begin
      Result:=False;
      WantGUI:=False;
      BTEvent(TRtcHttpGateClient(Sender.Owner),TRtcHttpGateClient(Sender.Owner).FData,WantGUI,Result);
      if WantGUI and assigned(GUIEvent) then
        if not CallGUI(GUIEvent,Sender) then
          raise ERtcGateClient.Create('GUI Data Event call failed');
      end
    else
      begin
      Result:=True;
      if assigned(GUIEvent) then
        if not CallGUI(GUIEvent,Sender) then
          raise ERtcGateClient.Create('GUI Data Event call failed');
      end;
  end;

function TRtcAbsGateClient.Call(BTEvent: TRtcGateClientStateEvent; GUIEvent: TRtcGateClientGUIStateEvent; Sender: TRtcConnection):boolean;
  var
    WantGUI:boolean;
  begin
  Result:=True;
  if assigned(Sender) then
    if assigned(BTEvent) then
      begin
      Result:=False;
      WantGUI:=False;
      BTEvent(TRtcHttpGateClient(Sender.Owner),TRtcHttpGateClient(Sender.Owner).FState,WantGUI,Result);
      if WantGUI and assigned(GUIEvent) then
        if not CallGUI(GUIEvent,Sender) then
          raise ERtcGateClient.Create('GUI State Event call failed');
      end
    else
      begin
      Result:=True;
      if assigned(GUIEvent) then
        if not CallGUI(GUIEvent,Sender) then
          raise ERtcGateClient.Create('GUI State Event call failed');
      end;
  end;

function TRtcAbsGateClient.Call(BTEvent: TRtcGateClientDataEvent; Sender: TRtcConnection):boolean;
  var
    WantGUI:boolean;
  begin
  if assigned(Sender) and assigned(BTEvent) then
    begin
    Result:=False;
    WantGUI:=False;
    BTEvent(TRtcHttpGateClient(Sender.Owner),TRtcHttpGateClient(Sender.Owner).FData,WantGUI,Result);
    end
  else
    Result:=True;
  end;

function TRtcAbsGateClient.Call(BTEvent: TRtcGateClientStateEvent; Sender: TRtcConnection):boolean;
  var
    WantGUI:boolean;
  begin
  if assigned(Sender) and assigned(BTEvent) then
    begin
    Result:=False;
    WantGUI:=False;
    BTEvent(TRtcHttpGateClient(Sender.Owner),TRtcHttpGateClient(Sender.Owner).FState,WantGUI,Result);
    end
  else
    Result:=True;
  end;

function TRtcAbsGateClient.Filter(BTEvent: TRtcGateClientFilterEvent; Sender: TRtcConnection):boolean;
  begin
  if assigned(BTEvent) and assigned(Sender) then
    begin
    Result:=False;
    BTEvent(TRtcHttpGateClient(Sender.Owner),TRtcHttpGateClient(Sender.Owner).FData,Result);
    end
  else
    Result:=True;
  end;

function TRtcAbsGateClient.CallGUI(GUIEvent: TRtcGateClientGUIDataEvent; Sender: TRtcConnection):boolean;
  var
    obj:TRtcGUIDataEvent;
  begin
  if assigned(GUIEvent) and assigned(Sender) then
    begin
    if InsideMainThread then
      begin
      GUIEvent(TRtcHttpGateClient(Sender.Owner),TRtcHttpGateClient(Sender.Owner).FData);
      Result:=True;
      end
    else
      begin
      obj:=TRtcGUIDataEvent.Create(GUIEvent,Sender);
      try
        Result:=TRtcThread.Sync(obj.Execute);
      finally
        RtcFreeAndNil(obj);
        end;
      end;
    end
  else
    Result:=False;
  end;

function TRtcAbsGateClient.CallGUI(GUIEvent: TRtcGateClientGUIStateEvent; Sender: TRtcConnection):boolean;
  var
    obj:TRtcGUIStateEvent;
  begin
  if assigned(GUIEvent) and assigned(Sender) then
    begin
    if InsideMainThread then
      begin
      GUIEvent(TRtcHttpGateClient(Sender.Owner),TRtcHttpGateClient(Sender.Owner).FState);
      Result:=True;
      end
    else
      begin
      obj:=TRtcGUIStateEvent.Create(GUIEvent,Sender);
      try
        Result:=TRtcThread.Sync(obj.Execute);
      finally
        RtcFreeAndNil(obj);
        end;
      end;
    end
  else
    Result:=False;
  end;

procedure TRtcAbsGateClient.Call_AfterLoggedIn(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClient.Call_AfterLoginFail(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClient.Call_AfterLogOut(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClient.Call_BeforeLogIn(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClient.Call_OnDataReceived(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClient.Call_OnInfoReceived(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClient.Call_OnReadyAfterReset(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClient.Call_OnReadyToSend(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClient.Call_OnStreamReset(Sender: TRtcConnection);
  begin
  end;

{$IFDEF RTC_ANON_METHODS}

// TRtcGateClientFilterAnonMethod = reference to procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var Wanted:boolean);

type
  TRtcGateClientFilterAMContainer = class(TObject)
  public
    MyMethod:TRtcGateClientFilterAnonMethod;
    constructor Create(const AMethod:TRtcGateClientFilterAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var Wanted:boolean);
    end;

constructor TRtcGateClientFilterAMContainer.Create(const AMethod:TRtcGateClientFilterAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcGateClientFilterAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcGateClientFilterAMContainer.MyEvent(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var Wanted:boolean);
  begin
  if assigned(MyMethod) then
    MyMethod(Client, Data, Wanted);
  end;

function TRtcAbsGateClient.Anon(const Event:TRtcGateClientFilterAnonMethod):TRtcGateClientFilterEvent;
  var
    obj:TRtcGateClientFilterAMContainer;
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
        obj:=TRtcGateClientFilterAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// TRtcGateClientDataAnonMethod = reference to procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI, WantBackThread:boolean);

type
  TRtcGateClientDataAMContainer = class(TObject)
  public
    MyMethod:TRtcGateClientDataAnonMethod;
    constructor Create(const AMethod:TRtcGateClientDataAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI, WantBackThread:boolean);
    end;

constructor TRtcGateClientDataAMContainer.Create(const AMethod:TRtcGateClientDataAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcGateClientDataAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcGateClientDataAMContainer.MyEvent(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI, WantBackThread:boolean);
  begin
  if assigned(MyMethod) then
    MyMethod(Client, Data, WantGUI, WantBackThread);
  end;

function TRtcAbsGateClient.Anon(const Event:TRtcGateClientDataAnonMethod):TRtcGateClientDataEvent;
  var
    obj:TRtcGateClientDataAMContainer;
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
        obj:=TRtcGateClientDataAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// TRtcGateClientStateAnonMethod = reference to procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean);

type
  TRtcGateClientStateAMContainer = class(TObject)
  public
    MyMethod:TRtcGateClientStateAnonMethod;
    constructor Create(const AMethod:TRtcGateClientStateAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean);
    end;

constructor TRtcGateClientStateAMContainer.Create(const AMethod:TRtcGateClientStateAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcGateClientStateAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcGateClientStateAMContainer.MyEvent(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI, WantBackThread:boolean);
  begin
  if assigned(MyMethod) then
    MyMethod(Client, State, WantGUI, WantBackThread);
  end;

function TRtcAbsGateClient.Anon(const Event:TRtcGateClientStateAnonMethod):TRtcGateClientStateEvent;
  var
    obj:TRtcGateClientStateAMContainer;
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
        obj:=TRtcGateClientStateAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// TRtcGateClientGUIDataAnonMethod = reference to procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData);

type
  TRtcGateClientGUIDataAMContainer = class(TObject)
  public
    MyMethod:TRtcGateClientGUIDataAnonMethod;
    constructor Create(const AMethod:TRtcGateClientGUIDataAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Client:TRtcHttpGateClient; Data:TRtcGateClientData);
    end;

constructor TRtcGateClientGUIDataAMContainer.Create(const AMethod:TRtcGateClientGUIDataAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcGateClientGUIDataAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcGateClientGUIDataAMContainer.MyEvent(Client:TRtcHttpGateClient; Data:TRtcGateClientData);
  begin
  if assigned(MyMethod) then
    MyMethod(Client, Data);
  end;

function TRtcAbsGateClient.Anon(const Event:TRtcGateClientGUIDataAnonMethod):TRtcGateClientGUIDataEvent;
  var
    obj:TRtcGateClientGUIDataAMContainer;
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
        obj:=TRtcGateClientGUIDataAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// TRtcGateClientGUIStateAnonMethod = reference to procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo);

type
  TRtcGateClientGUIStateAMContainer = class(TObject)
  public
    MyMethod:TRtcGateClientGUIStateAnonMethod;
    constructor Create(const AMethod:TRtcGateClientGUIStateAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo);
    end;

constructor TRtcGateClientGUIStateAMContainer.Create(const AMethod:TRtcGateClientGUIStateAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcGateClientGUIStateAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcGateClientGUIStateAMContainer.MyEvent(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo);
  begin
  if assigned(MyMethod) then
    MyMethod(Client, State);
  end;

function TRtcAbsGateClient.Anon(const Event:TRtcGateClientGUIStateAnonMethod):TRtcGateClientGUIStateEvent;
  var
    obj:TRtcGateClientGUIStateAMContainer;
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
        obj:=TRtcGateClientGUIStateAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// TRtcGateClientBTDataAnonMethod = reference to procedure(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI:boolean);

type
  TRtcGateClientBTDataAMContainer = class(TObject)
  public
    MyMethod:TRtcGateClientBTDataAnonMethod;
    constructor Create(const AMethod:TRtcGateClientBTDataAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI:boolean);
    end;

constructor TRtcGateClientBTDataAMContainer.Create(const AMethod:TRtcGateClientBTDataAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcGateClientBTDataAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcGateClientBTDataAMContainer.MyEvent(Client:TRtcHttpGateClient; Data:TRtcGateClientData; var WantGUI:boolean);
  begin
  if assigned(MyMethod) then
    MyMethod(Client, Data, WantGUI);
  end;

function TRtcAbsGateClient.Anon(const Event:TRtcGateClientBTDataAnonMethod):TRtcGateClientBTDataEvent;
  var
    obj:TRtcGateClientBTDataAMContainer;
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
        obj:=TRtcGateClientBTDataAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

// TRtcGateClientBTStateAnonMethod = reference to procedure(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI:boolean);

type
  TRtcGateClientBTStateAMContainer = class(TObject)
  public
    MyMethod:TRtcGateClientBTStateAnonMethod;
    constructor Create(const AMethod:TRtcGateClientBTStateAnonMethod);
    destructor Destroy; override;
    procedure MyEvent(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI:boolean);
    end;

constructor TRtcGateClientBTStateAMContainer.Create(const AMethod:TRtcGateClientBTStateAnonMethod);
  begin
  inherited Create;
  MyMethod:=AMethod;
  end;

destructor TRtcGateClientBTStateAMContainer.Destroy;
  begin
  MyMethod:=nil;
  inherited;
  end;

procedure TRtcGateClientBTStateAMContainer.MyEvent(Client:TRtcHttpGateClient; State:TRtcGateClientStateInfo; var WantGUI:boolean);
  begin
  if assigned(MyMethod) then
    MyMethod(Client, State, WantGUI);
  end;

function TRtcAbsGateClient.Anon(const Event:TRtcGateClientBTStateAnonMethod):TRtcGateClientBTStateEvent;
  var
    obj:TRtcGateClientBTStateAMContainer;
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
        obj:=TRtcGateClientBTStateAMContainer.Create(Event);
        FAMethods.insert(MethRefToRtcPtr(Event),obj);
        end;
      Result:=obj.MyEvent;
    finally
      FAMCS.Release;
      end;
    end;
  end;

{$ENDIF}

{ TRtcHttpGateClient }

type
  TRtcGatePingJob=class(TRtcJob)
  public
    Gate:TRtcHttpGateClient;
    constructor Create(Owner:TRtcHttpGateClient);

    function Run(Thr:TRtcThread):boolean; override;
    function SingleUse:boolean; override;
    end;

constructor TRtcGatePingJob.Create(Owner: TRtcHttpGateClient);
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
  if assigned(Gate) then
    Gate.PingCheck;
  end;

procedure TRtcHttpGateClient.StartPingTimer;
  begin
  CS.Acquire;
  try
    if not assigned(FPingTimer) then
      begin
      FPingTimer:=TRtcTimer.Create(True);
      // FPingTimer.Thread.NeedThread:=True; // High priority thread
      FPingJob:=TRtcGatePingJob.Create(self);
      end;
    TRtcTimer.Enable(FPingTimer,PING_INTERVAL_inMS,FPingJob);
    // TRtcTimer.Enable(FPingTimer,PING_INTERVAL_inMS,PingCheck);
  finally
    CS.Release;
    end;
  end;

procedure TRtcHttpGateClient.PausePingTimer;
  begin
  CS.Acquire;
  try
    if assigned(FPingTimer) then
      TRtcTimer.Disable(FPingTimer);
  finally
    CS.Release;
    end;
  end;

procedure TRtcHttpGateClient.StopPingTimer;
  begin
  CS.Acquire;
  try
    if assigned(FPingTimer) then
      begin
      TRtcTimer.Stop(FPingTimer);
      FPingTimer:=nil;
      end;
    if assigned(FPingJob) then
      RtcFreeAndNil(FPingJob);
  finally
    CS.Release;
    end;
  end;

function TRtcHttpGateClient.Login_Status:TRtcGateLoginStatus;
  begin
  Result:=FLoginStatus;
  end;

procedure TRtcHttpGateClient.DataLogin1BeginRequest(Sender: TRtcConnection);
  begin
  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<>gls_Login) then
      Sender.Request.Skip
    else
      Sender.Write;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataLogin1DataReceived(Sender: TRtcConnection);
  var
    res:RtcByteArray;
  begin
  SetLength(res,0);

  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<>gls_Login) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    if (Sender.Response.StatusCode<>GATE_OK_CODE) or (Sender.Response.ContentIn>1024) then
      begin
      if (Sender.Response.Done or (Sender.Response.ContentIn>1024)) then
        begin
        if (Sender.Response.StatusCode=GATE_ERROR_CODE) or
           (Sender.Response.StatusCode=GATE_FATALERROR_CODE) then
          begin
          res:=Sender.ReadEx;
          if length(res)>0 then
            Sender.Response.StatusText:=RtcBytesToString(res);
          end
        else if Sender.Response.StatusCode=GATE_OK_CODE then
          // Bad Response from Gateway
          Sender.Response.StatusText:='Bad Response from Gateway';
        User_LogOut(Sender);
        end;
      end
    else if Sender.Response.Done then
      begin
      res:=Sender.ReadEx;
      // Accept IP Addresses between 3 and 256 characters long
      if length(res)>256 then
        begin
        if LOG_GATECLIENT_EXITS then
          Log('#EXIT! DataLogin DataReceived: Response > 256',GATE_LOG);
        Sender.Response.StatusText:='Bad Response from Gateway';
        User_LogOut(Sender);
        Exit;
        end
      else if length(res)<3 then
        begin
        if LOG_GATECLIENT_EXITS then
          Log('#EXIT! DataLogin DataReceived: Response < 3',GATE_LOG);
        UserStreamLost(Sender);
        Exit;
        end
      else
        begin
        CS.Acquire;
        try
          FMyIP:=RtcBytesToString(res);
        finally
          CS.Release;
          end;

        if FNotSilent and (Login_Status=gls_Login) and assigned(DataLogin2.Client) then
          begin
          DataLogin2.Request.Method:='POST';
          DataLogin2.Request.FileName:=FGATEURI_LOGIN;
          if Sender=DataLogin2.Client then
            DataLogin2.Post(True,Sender)
          else
            DataLogin2.Post;
          end;
        end;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataLogin2BeginRequest(Sender: TRtcConnection);
  var
    auth,ccode:RtcString;
  begin
  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<>gls_Login) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    ccode:=FGATE_PRIMARY_KEY;

    CS.Acquire;
    try
      Crypt(ccode, FMyIP);
      FMyKey:=ccode;

      if FGATE_SECONDARY_KEY<>'' then
        Crypt(FMyKey, FGATE_SECONDARY_KEY);

      FCryIn.Key:=FMyKey;
      FCryIn.Init;

      FCryOut.Key:=FMyKey;
      FCryOut.Init;
    finally
      CS.Release;
      end;

    if (length(FGATE_USERAUTH)>0) or
       (Length(FGATE_USERINFO)>0) then
      begin
      auth:=FGATE_USERAUTH+
            FGATE_USERINFO+
            RtcBytesToString(crcInt2Bytes(length(FGATE_USERAUTH)));
      Crypt(auth, ccode);
      end
    else
      auth:='';

    Sender.Write(auth);
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataLogin2DataReceived(Sender: TRtcConnection);
  var
    res:RtcByteArray;
  begin
  SetLength(res,0);

  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<>gls_Login) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    if (Sender.Response.StatusCode<>GATE_OK_CODE) or (Sender.Response.ContentIn>1024) then
      begin
      if (Sender.Response.Done or (Sender.Response.ContentIn>1024)) then
        begin
        if (Sender.Response.StatusCode=GATE_ERROR_CODE) or
           (Sender.Response.StatusCode=GATE_FATALERROR_CODE) then
          begin
          res:=Sender.ReadEx;
          if length(res)>0 then
            Sender.Response.StatusText:=RtcBytesToString(res);
          end
        else if Sender.Response.StatusCode=GATE_OK_CODE then
          // Bad Response from Gateway
          Sender.Response.StatusText:='Bad Response from Gateway';
        User_LogOut(Sender);
        end;
      end
    else if Sender.Response.Done then
      begin
      res:=Sender.ReadEx;
      if length(res)>4 then
        begin
        if LOG_GATECLIENT_EXITS then
          Log('#EXIT! DataLogin DataReceived: Response > 4',GATE_LOG);
        Sender.Response.StatusText:='Bad Response from Gateway';
        User_LogOut(Sender);
        Exit;
        end
      else if length(res)=0 then
        begin
        if LOG_GATECLIENT_EXITS then
          Log('#EXIT! DataLogin DataReceived: Response = 0',GATE_LOG);
        Sender.Response.StatusText:='Gateway Access Denied';
        User_LogOut(Sender);
        Exit;
        end
      else if length(res)<4 then
        begin
        if LOG_GATECLIENT_EXITS then
          Log('#EXIT! DataLogin DataReceived: Response < 4',GATE_LOG);
        UserStreamLost(Sender);
        Exit;
        end
      else
        begin
        CS.Acquire;
        try
          DeCryptEx(res,RtcStringToBytes(MyKey));
          FMyUID:=crcBytes2GateID(res);
        finally
          CS.Release;
          end;
        if (FMyUID<MinUserID) or (FMyUID>MaxUserID) then
          begin
          if LOG_GATECLIENT_EXITS then
            case FMyUID of
              errNoUID: Log('#EXIT! DataLogin DataReceived: No UID',GATE_LOG);
              errBadUID: Log('#EXIT! DataLogin DataReceived: BAD UID',GATE_LOG);
              errBadCRC: Log('#EXIT! DataLogin DataReceived: UID CRC error',GATE_LOG);
              end;
          UserStreamLost(Sender);
          Exit;
          end;

        if FNotSilent and (Login_Status=gls_Login) and
            assigned(DataOUT.Client) and
            assigned(DataIN.Client) then
          begin
          FLoginStatus:=gls_Connect;
          FData.F.MyUID:=FMyUID;
          FState.F.MyUID:=FMyUID;
          // Prepare INPUT loop ...
          DataIN.Request.Method:='POST';
          DataIN.Request.FileName:=FGATEURI_OUTPUTRESET;
          // Prepare OUTPUT loop ...
          DataOUT.Request.Method:='POST';
          DataOUT.Request.FileName:=FGATEURI_INPUTRESET;
          // Start Input loop ...
          if Sender=DataIN.Client then
            DataIN.Post(True,Sender)
          else
            DataIN.Post;
          // Start Output loop ...
          if Sender=DataOUT.Client then
            DataOUT.Post(True,Sender)
          else
            DataOUT.Post;
          end;
        end;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataResponseAbort(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  UserStreamLost(Sender);
  end;

procedure TRtcHttpGateClient.DataOUTBeginRequest(Sender: TRtcConnection);
  var
    UID:RtcString;
    UIDB:RtcByteArray;
  begin
  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<gls_Connect) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    CS.Acquire;
    try
      UIDB:=crcGateID2Bytes(FMyUID); // 3 bytes ID + 1 byte CRC
      CryptEx(UIDB,RtcStringToBytes(MyIP));
      UID:=LWordTo6bit(Bytes2Int(UIDB)); // 4 bytes encrypted
      Sender.Request.Query['u']:=UID;

      if FStreamBlockSizeOut=0 then
        Sender.Request.ContentLength:=DEF_OUTSTREAM_SIZE
      else
        Sender.Request.ContentLength:=State.LeftToSend+FStreamBlockSizeOut+PING_PACKET_SIZE;

      FReqToSend:=Sender.Request.ContentLength;
      FSpaceToSend:=FReqToSend>0;

      if Sender.Request.FileName=FGATEURI_INPUTRESET then
        InitOutput;

      FNowSending:=False;
    finally
      CS.Release;
      end;

    CS.Acquire;
    try
      State.F.OutputState:=outs_Prepare;
      FNowSending:=True;
    finally
      CS.Release;
      end;
    Sender.Write;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataOUTDataSent(Sender: TRtcConnection);
  var
    needAfterLogin,needReadyToSend,haveSignal:boolean;
  begin
  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<gls_Connect) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    needAfterLogin:=False;
    needReadyToSend:=False;

    CS.Acquire;
    try
      if (State.LoggedIn or (Login_Status=gls_Connect)) then
        begin
        State.F.LastOutput:=GetAppRunTime;
        State.F.LastContact:=State.LastOutput;
        State.F.OutputReady:=True;

        FOutputWasLost:=False;
        FOutputReset:=0;

        FNowSending:=False;

        if State.InputReady and (Login_Status=gls_Connect) then
          begin
          needAfterLogin:=not State.LoggedIn;
          needReadyToSend:=True;
          FNeedOutput:=True;
          State.F.LoggedIn:=True;
          FLoginStatus:=gls_Active;
          end;
        end;
    finally
      CS.Release;
      end;

    if needAfterLogin then
      Call_AfterLoggedIn(Sender);
    if needReadyToSend and not FDataWaiting then
      Call_OnReadyToSend(Sender);

    if Sender.Request.Complete then
      begin
      CS.Acquire;
      try
        State.F.OutputReady:=False;
        State.F.OutputState:=outs_Done;
      finally
        CS.Release;
        end;
      end
    else if FNotSilent and (Login_Status>=gls_Connect) then
      begin
      if Sender.Request.Started then
        State.F.OutputState:=outs_Start
      else
        State.F.OutputState:=outs_Idle;

      if not ReadyToSend(Sender) then
        begin
        if FBlocking then
          begin
          repeat
            haveSignal:=FEV.WaitFor(CLIENT_WAITFOR_CHECKSTATE)=wr_Signaled;
            until haveSignal or FAmSilent or (Login_Status<gls_Connect);
          end
        else
          haveSignal:=FEV.WaitFor(0)=wr_Signaled;
        if haveSignal and (Login_Status>=gls_Connect) and FNotSilent then
          ReadyToSend(Sender);
        end;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataOUTDataOut(Sender: TRtcConnection);
  begin
  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<gls_Connect) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    CS.Acquire;
    try
      State.F.LastOutput:=GetAppRunTime;
      State.F.LastContact:=State.F.LastOutput;
    finally
      CS.Release;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataOUTDataIn(Sender: TRtcConnection);
  begin
  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<gls_Connect) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    CS.Acquire;
    try
      State.F.LastOutput:=GetAppRunTime;
      State.F.LastContact:=State.F.LastOutput;
    finally
      CS.Release;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataOUTDataReceived(Sender: TRtcConnection);
  var
    data:RtcByteArray;
  begin
  SetLength(data,0);
  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<gls_Connect) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    if (Sender.Response.StatusCode<>GATE_OK_CODE) or (Sender.Response.ContentIn>1024) then
      begin
      if Sender.Response.Done and
        ( (Sender.Response.StatusCode=GATE_ERROR_CODE) or
          (Sender.Response.StatusCode=GATE_FATALERROR_CODE) ) then
        begin
        data:=Sender.ReadEx;
        if length(data)>0 then
          Sender.Response.StatusText:=RtcBytesToString(data);
        end
      else if Sender.Response.StatusCode=GATE_OK_CODE then
        Sender.Response.StatusText:='Bad Response';

      if (Sender.Response.StatusCode=GATE_OK_CODE) or
         (Sender.Response.StatusCode=GATE_ERROR_CODE) then
        UserStreamLost(Sender)
      else // Bad Response from Gateway
        User_LogOut(Sender);
      end
    else
      begin
      data:=Sender.ReadEx;
      if length(data)>0 then
        begin
        if LOG_GATECLIENT_EXITS then
          Log('#EXIT! DataOUT DataReceived: Response > 0',GATE_LOG);
        Sender.Response.StatusText:='Bad Response';
        User_LogOut(Sender);
        Exit;
        end
      else if Sender.Response.Done then
        begin
        CS.Acquire;
        try
          State.F.OutputState:=outs_Done;
        finally
          CS.Release;
          end;

        if FNotSilent and (Login_Status>=gls_Connect) then
          begin
          // Continue OUTPUT loop ...
          if assigned(DataOUT.Client) then
            begin
            DataOUT.Request.Method:='POST';
            DataOUT.Request.FileName:=FGATEURI_INPUT;
            if Sender=DataOUT.Client then
              DataOUT.Post(True,Sender)
            else
              DataOUT.Post;
            end;
          end;
        end;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataINBeginRequest(Sender: TRtcConnection);
  var
    UID:RtcString;
    UIDB:RtcByteArray;
    s:RtcString;
    bts:int64;
  begin
  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<gls_Connect) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    CS.Acquire;
    try
      UIDB:=crcGateID2Bytes(FMyUID); // 3 bytes ID + 1 byte CRC
      CryptEx(UIDB,RtcStringToBytes(MyIP));
      UID:=LWordTo6bit(Bytes2Int(UIDB)); // 4 bytes encrypted
      Sender.Request.Query['u']:=UID;

      if FStreamBlockSizeIn<1 then
        s:=LWordTo6bit(DEF_OUTSTREAM_SIZE)
      else
        s:=LWordTo6bit(FStreamBlockSizeIn);

      if Sender.Request.FileName=FGATEURI_OUTPUTRESET then
        InitInput;

      State.F.InputState:=ins_Prepare;
    finally
      CS.Release;
      end;

    bts:=ProcessInput(Sender);
    if bts>=0 then
      begin
      Sender.Response.ExpectedBytes:=bts;
      Sender.Write(s);
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataINDataReceived(Sender: TRtcConnection);
  var
    data:RtcByteArray;
    needAfterLogin,needReadyToSend:boolean;
    lts,bts:int64;
  begin
  SetLength(data,0);
  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<gls_Connect) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    if Sender.Response.StatusCode<>GATE_OK_CODE then
      begin
      if Sender.Response.Done or (Sender.Response.ContentIn>1024) then
        begin
        if Sender.Response.Done and
          ( (Sender.Response.StatusCode=GATE_ERROR_CODE) or
            (Sender.Response.StatusCode=GATE_FATALERROR_CODE) ) then
          begin
          data:=Sender.ReadEx;
          if length(data)>0 then
            Sender.Response.StatusText:=RtcBytesToString(data);
          end
        else if Sender.Response.StatusCode=GATE_OK_CODE then
          Sender.Response.StatusText:='Bad Response from Gateway';
        if (Sender.Response.StatusCode=GATE_OK_CODE) or
           (Sender.Response.StatusCode=GATE_ERROR_CODE) then
          UserStreamLost(Sender)
        else // Bad Response from Gateway
          User_LogOut(Sender);
        end;
      end
    else if (Login_Status>=gls_Connect) and not FAmSilent then
      begin
      data:=Sender.ReadEx;
      lts:=Sender.Response.ContentLength-Sender.Response.ContentIn;
      if length(data)>0 then
        begin
        needAfterLogin:=False;
        needReadyToSend:=False;

        CS.Acquire;
        try
          if (State.LoggedIn or (Login_Status=gls_Connect)) then
            begin
            State.F.InputState:=ins_Start;
            State.F.InputReady:=True;
            if State.OutputReady and (Login_Status=gls_Connect) then
              begin
              needReadyToSend:=True;
              FNeedOutput:=True;
              needAfterLogin:=not State.LoggedIn;
              State.F.LoggedIn:=True;
              FLoginStatus:=gls_Active; // Logged in
              end;
            end
          else
            State.F.InputState:=ins_Recv;

          State.F.LastInput:=GetAppRunTime;
          State.F.LastContact:=State.LastInput;
          FInputWasLost:=False;
          FInputReset:=0;

          Inc(State.F.TotalReceived,length(Data));

          FCryIn.DeCryptEx(data);
          FBuffIn.AddEx(data);
          if length(data)>FNeedInput then
            FNeedInput:=0
          else
            Dec(FNeedInput,length(data));
        finally
          CS.Release;
          end;

        if Login_Status=gls_Active then
          begin
          if needAfterLogin then
            Call_AfterLoggedIn(Sender);
          if needReadyToSend then
            begin
            if not FDataWaiting then
              Call_OnReadyToSend(Sender)
            else if not FBlocking then
              if assigned(ClientOUT) then
                ClientOUT.PostEvent(ReadyToSendNow);
            end;
          end;

        bts:=ProcessInput(Sender);
        if bts>lts then
          bts:=lts;
        if bts>=0 then
          Sender.Response.ExpectedBytes:=bts
        else
          Exit;
        end;

      if FNotSilent and (Login_Status>=gls_Connect) and (lts=0) then
        begin
        CS.Acquire;
        try
          State.F.LastInput:=GetAppRunTime;
          State.F.LastContact:=State.F.LastInput;
          FInputWasLost:=False;
          FInputReset:=0;

          State.F.InputReady:=False;
          State.F.InputState:=ins_Done;
        finally
          CS.Release;
          end;

        if not FAmSilent and (Login_Status>=gls_Connect) then
          begin
          // Continue INPUT loop ...
          if assigned(DataIN.Client) then
            begin
            DataIN.Request.Method:='POST';
            DataIN.Request.FileName:=FGATEURI_OUTPUT;
            if Sender=DataIN.Client then
              DataIN.Post(True,Sender)
            else
              DataIN.Post;
            end;
          end;
        end;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataINDataIn(Sender: TRtcConnection);
  begin
  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<gls_Connect) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    CS.Acquire;
    try
      State.F.LastInput:=GetAppRunTime;
      State.F.LastContact:=State.F.LastInput;
    finally
      CS.Release;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DataINDataOut(Sender: TRtcConnection);
  begin
  EnterEvent(Sender);
  try
    if FAmSilent or (FLoginStatus<gls_Connect) then
      begin
      Sender.Request.Skip;
      Exit;
      end;

    CS.Acquire;
    try
      State.F.LastInput:=GetAppRunTime;
      State.F.LastContact:=State.F.LastInput;
    finally
      CS.Release;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.InitInput;
  var
    obj:TObject;
    oid:Cardinal;
    uid:RtcIntPtr;
  procedure Clear_UserGroups;
    begin
    // Clear Accounts List
    uid:=FUsers.search_min(obj);
    while (uid>0) and assigned(obj) do
      begin
      FUsers.remove(uid);
      RtcFreeAndNil(obj);
      uid:=FUsers.search_min(obj);
      end;
    FUsers.removeall;

    // Clear Groups List
    uid:=FGroups.search_min(obj);
    while (uid>0) and assigned(obj) do
      begin
      FGroups.remove(uid);
      RtcFreeAndNil(obj);
      uid:=FGroups.search_min(obj);
      end;
    FGroups.removeall;
    end;
  begin
  CS.Acquire;
  try
    State.F.InputReady:=False;
    SetLength(FData.FBytes,0);
    SetLength(FData.FContent,0);

    FCryIn.Init;
    FBuffIn.Clear;
    oid:=FBuffRcv.search_min(obj);
    while (oid>0) and assigned(obj) do
      begin
      FBuffRcv.remove(oid);
      if assigned(obj) then
        RtcFreeAndNil(obj);
      oid:=FBuffRcv.search_min(obj);
      end;
    FBuffRcv.removeall;
    FNeedInput:=0;

    if Login_Status>gls_Offline then
      State.F.InputState:=ins_Connecting;
    State.F.PingInCnt:=0;
    State.F.LastInput:=GetAppRunTime;
  finally
    CS.Release;
    end;

  if assigned(FAccounts) then
    begin
    FAccounts.GateCS.Acquire;
    try
      Clear_UserGroups;
    finally
      FAccounts.GateCS.Release;
      end;
    end
  else
    begin
    CS.Acquire;
    try
      Clear_UserGroups;
    finally
      CS.Release;
      end;
    end;
  end;

procedure TRtcHttpGateClient.InitOutput;
  begin
  CS.Acquire;
  try
    FDataWaiting:=False;
    FNeedOutput:=False;
    with State.F do
      begin
      OutputReady:=False;
      LeftToSend:=0;
      ReadyToSend:=False;
      if (Login_Status>gls_Offline) and not FAmSilent then
        OutputState:=outs_Connecting;
      PingOutCnt:=0;
      LastOutput:=GetAppRunTime;
      LastPing:=LastOutput;
      end;
    FCryOut.Init;
    FBuffOut.Clear;
  finally
    CS.Release;
    end;
  end;

procedure TRtcHttpGateClient.User_LogIn(Sender:TRtcConnection);
  begin
  if FAmSilent then Exit;
  EnterEvent(Sender);
  try
    CS.Acquire;
    try
      if State.LoggedIn then Exit;

      FAutoRetry:=-1;
      FInputWasLost:=False;
      FInputReset:=0;

      FOutputWasLost:=False;
      FOutputReset:=0;

      FMyIP:='';
      FMyKey:='';
      FMyUID:=0;

      FData.F.MyUID:=0;
      FState.F.MyUID:=0;

      with State.F do
        begin
        OutputState:=outs_Connecting;
        InputState:=ins_Connecting;
        PingOutCnt:=0;
        PingInCnt:=0;
        LastInput:=GetAppRunTime;
        LastContact:=LastInput;
        LastOutput:=LastInput;
        LastPing:=LastInput;
        TotalSent:=0;
        TotalReceived:=0;
        OutputReady:=False;
        InputReady:=False;
        end;

      InitInput;
      InitOutput;

      FLoginStatus:=gls_Relogin;
    finally
      CS.Release;
      end;

    if assigned(ClientIN) then
      begin
      ClientIN.AutoConnect:=False;
      ClientIN.ReconnectOn.ConnectError:=False;
      ClientIN.ReconnectOn.ConnectFail:=False;
      ClientIN.ReconnectOn.ConnectLost:=False;
      ClientIN.Disconnect;
      ClientIN.SkipRequests;
      end;
    if assigned(ClientOUT) then
      begin
      ClientOUT.AutoConnect:=False;
      ClientOUT.ReconnectOn.ConnectError:=False;
      ClientOUT.ReconnectOn.ConnectFail:=False;
      ClientOUT.ReconnectOn.ConnectLost:=False;
      ClientOUT.Disconnect;
      ClientOUT.SkipRequests;
      end;
    FEV.SetEvent;

    FData.FGateAddr:=FGateAddr;
    FData.FGatePort:=FGatePort;

    FState.FGateAddr:=FGateAddr;
    FState.FGatePort:=FGatePort;

    ClientDummy.ServerAddr:=FGateAddr;
    ClientDummy.ServerPort:=FGatePort;
    ClientDummy.ServerIPV:=FGateIPV;

    if Sender=nil then
      Call_BeforeLogIn(ClientDummy)
    else
      Call_BeforeLogIn(Sender);

    if assigned(ClientIN) then
      begin
      SetupConnectionTimeouts(ClientIN);
      if (FGatePort<>ClientIN.ServerPort) or
         (FGateAddr<>ClientIN.ServerAddr) or
         (FGateIPV<>ClientIN.ServerIPV) or
         (FUseBlocking<>ClientIN.Blocking) or
         (FUseProxy<>ClientIN.UseProxy) or
         (FUseWinHTTP<>ClientIN.UseWinHTTP) or
         (FUseSSL<>ClientIN.UseSSL) or
         (FCryptPlugin<>ClientIN.CryptPlugin) or
         (not ClientIN.UserLogin.IsEqual(FUserLogin)) then
        begin
        ClientIN.ReleaseProvider;
        ClientIN.ServerIPV:=FGateIPV;
        ClientIN.ServerAddr:=FGateAddr;
        ClientIN.ServerPort:=FGatePort;
        ClientIN.CryptPlugin:=FCryptPlugin;
        ClientIN.Blocking:=FUseBlocking;
        ClientIN.UseProxy:=FUseProxy;
        ClientIN.UseWinHTTP:=FUseWinHTTP;
        ClientIN.UseSSL:=FUseSSL;
        FUserLogin.AssignTo(ClientIN.UserLogin);
        end;
      end
    else
      Exit;

    if assigned(ClientOUT) then
      begin
      SetupConnectionTimeouts(ClientOUT);
      if (FGatePort<>ClientOUT.ServerPort) or
         (FGateAddr<>ClientOUT.ServerAddr) or
         (FGateIPV<>ClientOUT.ServerIPV) or
         (FUseBlocking<>ClientOUT.Blocking) or
         (FUseProxy<>ClientOUT.UseProxy) or
         (FUseWinHTTP<>ClientOUT.UseWinHTTP) or
         (FUseSSL<>ClientOUT.UseSSL) or
         (FCryptPlugin<>ClientOUT.CryptPlugin) or
         (not ClientOUT.UserLogin.IsEqual(FUserLogin)) then
        begin
        ClientOUT.ReleaseProvider;
        ClientOUT.ServerAddr:=FGateAddr;
        ClientOUT.ServerPort:=FGatePort;
        ClientOUT.ServerIPV:=FGateIPV;
        ClientOUT.CryptPlugin:=FCryptPlugin;
        ClientOUT.Blocking:=FUseBlocking;
        ClientOUT.UseProxy:=FUseProxy;
        ClientOUT.UseWinHTTP:=FUseWinHTTP;
        ClientOUT.UseSSL:=FUseSSL;
        FUserLogin.AssignTo(ClientOUT.UserLogin);
        end;
      end
    else
      Exit;

    {$IFDEF WINDOWS}
    FBlocking:=(FCryptPlugin=nil) and (FUseProxy or FUseSSL or FUseWinHTTP);
    {$ELSE}
    FBlocking:=False;
    {$ENDIF}

    FData.FGateAddr:=FGateAddr;
    FData.FGatePort:=FGatePort;

    FState.FGateAddr:=FGateAddr;
    FState.FGatePort:=FGatePort;

    ClientDummy.ServerAddr:=FGateAddr;
    ClientDummy.ServerPort:=FGatePort;
    ClientDummy.ServerIPV:=FGateIPV;

    CS.Acquire;
    try
      if FAmSilent or (Login_Status<>gls_Relogin) then Exit;
      FLoginStatus:=gls_Login;
    finally
      CS.Release;
      end;

    if assigned(ClientIN) then
      begin
      ClientIN.SkipRequests;
      ClientIN.AutoConnect:=True;
      if CLIENT_FORCED_RECONNECT then
        begin
        ClientIN.ReconnectOn.ConnectError:=True;
        ClientIN.ReconnectOn.ConnectFail:=True;
        ClientIN.ReconnectOn.ConnectLost:=True;
        end;
      end;
    if assigned(ClientOUT) then
      begin
      ClientOUT.SkipRequests;
      ClientOUT.AutoConnect:=True;
      if CLIENT_FORCED_RECONNECT then
       begin
       ClientOUT.ReconnectOn.ConnectError:=True;
        ClientOUT.ReconnectOn.ConnectFail:=True;
        ClientOUT.ReconnectOn.ConnectLost:=True;
        end;
      end;
    FEV.SetEvent;
    Sleep(1);
    FEV.ResetEvent;

    // Send PING to start the login procedure
    if FNotSilent and (Login_Status=gls_Login) and
       assigned(ClientIN) and
       assigned(ClientOUT) and
       assigned(DataLogin2.Client) and
       assigned(DataLogin1.Client) then
      begin
      DataLogin1.Request.Method:='GET';
      DataLogin1.Request.FileName:=FGATEURI_PING;
      if Sender=DataLogin1.Client then
        DataLogin1.Post(True,Sender)
      else
        DataLogin1.Post;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.User_LogOut(Sender:TRtcConnection);
  var
    fail:boolean;
    toPause:integer;
  begin
  EnterEvent(Sender);
  try
    if FAmSilent then Exit;
    CS.Acquire;
    try
      if Login_Status=gls_Offline then
        Exit;
      FLoginStatus:=gls_Offline;
      FAutoRetry:=-1;
    finally
      CS.Release;
      end;

    fail:=not State.LoggedIn;

    with State,F do
      begin
      LoggedIn:=False;
      InputReady:=False;
      OutputReady:=False;

      InputResetAt:=0;
      OutputResetAt:=0;
      if assigned(ClientOUT) and (Sender=ClientOUT) then
        OutputResetAt:=ClientOUT.Request.ContentOUT
      else if assigned(ClientIN) and (Sender=ClientIN) then
        InputResetAt:=ClientIN.Response.ContentIN;

      if LastInput>0 then
        InputResetTime:=GetAppRunTime-LastInput;
      if LastOutput>0 then
        OutputResetTime:=GetAppRunTime-LastOutput;

      FLastError:='';
      if assigned(Sender) then
        if Sender.Response.StatusText<>'OK' then
          begin
          FLastError:=Sender.Response.StatusText;
          if FLastError<>'' then
            if Sender=ClientIN then
              FLastError:='#IN '+FLastError
            else if Sender=ClientOUT then
              FLastError:='#OUT '+FLastError;
          end;
      end;

    toPause:=FInputReset+FOutputReset;

    if assigned(ClientIN) then
      begin
      ClientIN.AutoConnect:=False;
      ClientIN.ReconnectOn.ConnectError:=False;
      ClientIN.ReconnectOn.ConnectFail:=False;
      ClientIN.ReconnectOn.ConnectLost:=False;
      ClientIN.Disconnect;
      ClientIN.SkipRequests;
      end;
    if assigned(ClientOUT) then
      begin
      ClientOUT.AutoConnect:=False;
      ClientOUT.ReconnectOn.ConnectError:=False;
      ClientOUT.ReconnectOn.ConnectFail:=False;
      ClientOUT.ReconnectOn.ConnectLost:=False;
      ClientOUT.Disconnect;
      ClientOUT.SkipRequests;
      end;
    FEV.SetEvent;

    FInputWasLost:=False;
    FInputReset:=0;

    FOutputWasLost:=False;
    FOutputReset:=0;

    CS.Acquire;
    try
      with State,F do
        begin
        LoggedIn:=False;
        InputReady:=False;
        OutputReady:=False;
        InputState:=ins_Closed;
        OutputState:=outs_Closed;
        PingOutCnt:=0;
        PingInCnt:=0;
        end;
    finally
      CS.Release;
      end;

    InitInput;
    InitOutput;

    if Sender=nil then
      begin
      if fail then
        Call_AfterLoginFail(ClientDummy)
      else
        Call_AfterLogout(ClientDummy);
      end
    else
      begin
      if fail then
        Call_AfterLoginFail(Sender)
      else
        Call_AfterLogout(Sender);
      end;

    if FAutoRelogin and not FAmSilent and (Login_Status=gls_Offline) and
       assigned(ClientIN) and assigned(ClientOUT) then
      begin
      CS.Acquire;
      try
        if Login_Status=gls_Offline then
          begin
          if toPause>MAX_GATECLIENT_RECONNECT_WAIT then
            toPause:=MAX_GATECLIENT_RECONNECT_WAIT;

          FAutoRetry := toPause+1;
          FLoginStatus:=gls_Relogin;

          StartPingTimer;
          end;
      finally
        CS.Release;
        end;
      end
    else
      begin
      FMyIP:='';
      FMyKey:='';
      FMyUID:=0;
      FData.F.MyUID:=0;
      FState.F.MyUID:=0;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.UserStreamLost(Sender:TRtcConnection);
  var
    toPause:integer;
  begin
  if GetAppRunTime>State.LastContact+CLIENTCHECKLOGOUT_TIMEOUT then
    begin
    User_LogOut(Sender);
    Exit;
    end;

  EnterEvent(Sender);
  try
    if FAmSilent then Exit;

    CS.Acquire;
    try
      if Login_Status=gls_Retry then
        Exit;
      FLoginStatus:=gls_Retry;
      FAutoRetry:=-1;
    finally
      CS.Release;
      end;

    with State,F do
      begin
      FNeedLogIn:=not LoggedIn;
      InputResetAt:=0;
      OutputResetAt:=0;
      if assigned(ClientOUT) and (Sender=ClientOUT) then
        OutputResetAt:=ClientOUT.Request.ContentOUT
      else if assigned(ClientIN) and (Sender=ClientIN) then
        InputResetAt:=ClientIN.Response.ContentIN;

      if LastInput>0 then
        InputResetTime:=GetAppRunTime-LastInput;
      if LastOutput>0 then
        OutputResetTime:=GetAppRunTime-LastOutput;

      InputReady:=False;
      OutputReady:=False;

      FLastError:='';
      if assigned(Sender) then
        if Sender.Response.StatusText<>'OK' then
          begin
          FLastError:=Sender.Response.StatusText;
          if FLastError<>'' then
            if Sender=ClientIN then
              FLastError:='#IN '+FLastError
            else if Sender=ClientOUT then
              FLastError:='#OUT '+FLastError;
          end;
      end;

    if Sender=ClientIN then
      begin
      if FInputWasLost then
        Inc(FInputReset);
      FInputWasLost:=True;
      end
    else if Sender=ClientOUT then
      begin
      if FOutputWasLost then
        Inc(FOutputReset);
      FOutputWasLost:=True;
      end
    else
      begin
      if FInputWasLost then
        Inc(FInputReset);
      if FOutputWasLost then
        Inc(FOutputReset);
      FInputWasLost:=True;
      FOutputWasLost:=True;
      end;

    toPause:=FInputReset+FOutputReset;

    if assigned(ClientIN) then
      begin
      ClientIN.AutoConnect:=False;
      ClientIN.ReconnectOn.ConnectError:=False;
      ClientIN.ReconnectOn.ConnectFail:=False;
      ClientIN.ReconnectOn.ConnectLost:=False;
      ClientIN.Disconnect;
      ClientIN.SkipRequests;
      end;
    if assigned(ClientOUT) then
      begin
      ClientOUT.AutoConnect:=False;
      ClientOUT.ReconnectOn.ConnectError:=False;
      ClientOUT.ReconnectOn.ConnectFail:=False;
      ClientOUT.ReconnectOn.ConnectLost:=False;
      ClientOUT.Disconnect;
      ClientOUT.SkipRequests;
      end;
    FEV.SetEvent;

    InitInput;
    InitOutput;

    if Sender=nil then
      Call_OnStreamReset(ClientDummy)
    else
      Call_OnStreamReset(Sender);

    if toPause>MAX_GATECLIENT_RECONNECT_WAIT then
      toPause:=MAX_GATECLIENT_RECONNECT_WAIT;
    CS.Acquire;
    try
      if Login_Status=gls_Retry then
        FAutoRetry := toPause+1;
    finally
      CS.Release;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.RetryLogin;
  begin
  if FAmSilent then Exit
  else if GetAppRunTime>State.LastContact+CLIENTCHECKLOGOUT_TIMEOUT then
    begin
    UserStreamHardReset;
    Exit;
    end;

  if FNotSilent and assigned(ClientIN) then
    begin
    ClientIN.AutoConnect:=False;
    ClientIN.ReconnectOn.ConnectError:=False;
    ClientIN.ReconnectOn.ConnectFail:=False;
    ClientIN.ReconnectOn.ConnectLost:=False;
    ClientIN.Disconnect;
    ClientIN.SkipRequests;
    if (FGatePort<>ClientIN.ServerPort) or
       (FGateAddr<>ClientIN.ServerAddr) or
       (FGateIPV<>ClientIN.ServerIPV) or
       (FUseBlocking<>ClientIN.Blocking) or
       (FUseProxy<>ClientIN.UseProxy) or
       (FUseWinHTTP<>ClientIN.UseWinHTTP) or
       (FUseSSL<>ClientIN.UseSSL) or
       (FCryptPlugin<>ClientIN.CryptPlugin) or
       (not ClientIN.UserLogin.IsEqual(FUserLogin)) then
      begin
      ClientIN.ReleaseProvider;
      ClientIN.ServerAddr:=FGateAddr;
      ClientIN.ServerPort:=FGatePort;
      ClientIN.ServerIPV:=FGateIPV;
      ClientIN.Blocking:=FUseBlocking;
      ClientIN.UseProxy:=FUseProxy;
      ClientIN.UseWinHTTP:=FUseWinHTTP;
      ClientIN.UseSSL:=FUseSSL;
      ClientIN.CryptPlugin:=FCryptPlugin;
      FUserLogin.AssignTo(ClientIN.UserLogin);
      end;
    end
  else
    Exit;

  if FNotSilent and assigned(ClientOUT) then
    begin
    ClientOUT.AutoConnect:=False;
    ClientOUT.ReconnectOn.ConnectError:=False;
    ClientOUT.ReconnectOn.ConnectFail:=False;
    ClientOUT.ReconnectOn.ConnectLost:=False;
    ClientOUT.Disconnect;
    ClientOUT.SkipRequests;
    if (FGatePort<>ClientOUT.ServerPort) or
       (FGateAddr<>ClientOUT.ServerAddr) or
       (FGateIPV<>ClientOUT.ServerIPV) or
       (FUseBlocking<>ClientOUT.Blocking) or
       (FUseProxy<>ClientOUT.UseProxy) or
       (FUseWinHTTP<>ClientOUT.UseWinHTTP) or
       (FUseSSL<>ClientOUT.UseSSL) or
       (FCryptPlugin<>ClientOUT.CryptPlugin) or
       (not ClientOUT.UserLogin.IsEqual(FUserLogin)) then
      begin
      ClientOUT.ReleaseProvider;
      ClientOUT.ServerAddr:=FGateAddr;
      ClientOUT.ServerPort:=FGatePort;
      ClientOUT.ServerIPV:=FGateIPV;
      ClientOUT.Blocking:=FUseBlocking;
      ClientOUT.UseProxy:=FUseProxy;
      ClientOUT.UseWinHTTP:=FUseWinHTTP;
      ClientOUT.UseSSL:=FUseSSL;
      ClientOUT.CryptPlugin:=FCryptPlugin;
      FUserLogin.AssignTo(ClientOUT.UserLogin);
      end;
    end
  else
    Exit;

  FEV.SetEvent;

  if FAmSilent then Exit;

  FData.FGateAddr:=FGateAddr;
  FData.FGatePort:=FGatePort;

  FState.FGateAddr:=FGateAddr;
  FState.FGatePort:=FGatePort;

  ClientDummy.ServerAddr:=FGateAddr;
  ClientDummy.ServerPort:=FGatePort;
  ClientDummy.ServerIPV:=FGateIPV;

{$IFDEF WINDOWS}
  FBlocking:=(FCryptPlugin=nil) and (FUseProxy or FUseSSL or FUseWinHTTP);
{$ELSE}
  FBlocking:=False;
{$ENDIF}

  CS.Acquire;
  try
    if FAmSilent then
      Exit
    else if FNeedLogin then
      FLoginStatus:=gls_Login
    else
      FLoginStatus:=gls_Connect;
  finally
    CS.Release;
    end;

  if assigned(ClientIN) then
    begin
    ClientIN.SkipRequests;
    ClientIN.AutoConnect:=True;
    if CLIENT_FORCED_RECONNECT then
      begin
      ClientIN.ReconnectOn.ConnectError:=True;
      ClientIN.ReconnectOn.ConnectFail:=True;
      ClientIN.ReconnectOn.ConnectLost:=True;
      end;
    end;
  if assigned(ClientOUT) then
    begin
    ClientOUT.SkipRequests;
    ClientOUT.AutoConnect:=True;
    if CLIENT_FORCED_RECONNECT then
      begin
      ClientOUT.ReconnectOn.ConnectError:=True;
      ClientOUT.ReconnectOn.ConnectFail:=True;
      ClientOUT.ReconnectOn.ConnectLost:=True;
      end;
    end;
  FEV.SetEvent;
  Sleep(1);
  FEV.ResetEvent;

  if FAmSilent then
    Exit
  else if (Login_Status=gls_Login) then
    begin
    if FNotSilent and
       assigned(ClientIN) and
       assigned(ClientOUT) and
       assigned(DataLogin1.Client) and
       assigned(DataLogin2.Client) then
      begin
      with State.F do
        begin
        InputState:=ins_Connecting;
        OutputState:=outs_Connecting;
        PingOutCnt:=0;
        PingInCnt:=0;
        InputReady:=False;
        OutputReady:=False;
        end;
      // PING Gateway to start the login procedure
      DataLogin1.Request.Method:='GET';
      DataLogin1.Request.FileName:=FGATEURI_PING;
      DataLogin1.Post;
      end;
    end
  else if (Login_Status=gls_Connect) then
    begin
    if FNotSilent and
       assigned(ClientIN) and
       assigned(ClientOUT) and
       assigned(DataIN.Client) and
       assigned(DataOUT.Client) then
      begin
      // Prepare INPUT loop ...
      DataIN.Request.Method:='POST';
      DataIN.Request.FileName:=FGATEURI_OUTPUTRESET;
      // Prepare OUTPUT loop ...
      DataOUT.Request.Method:='POST';
      DataOUT.Request.FileName:=FGATEURI_INPUTRESET;
      // Start Input loop
      DataIN.Post;
      // Start Output loop
      DataOUT.Post;
      end;
    end;
  end;

function TRtcHttpGateClient.GetStreamBlockSizeIn: Cardinal;
  begin
  Result:=FStreamBlockSizeIn;
  end;

function TRtcHttpGateClient.GetStreamBlockSizeOut: Cardinal;
  begin
  Result:=FStreamBlockSizeOut;
  end;

procedure TRtcHttpGateClient.SetStreamBlockSizeIn(const Value: Cardinal);
  begin
  FStreamBlockSizeIn:=Value;
  end;

procedure TRtcHttpGateClient.SetStreamBlockSizeOut(const Value: Cardinal);
  begin
  FStreamBlockSizeOut:=Value;
  end;

function TRtcHttpGateClient.ProcessInput(Sender:TRtcConnection): int64;
  var
    len,loc:integer;
    xUserID:TGateUID;
    xCommand:byte;
    xLength,xLeft:Cardinal;
  begin
  if FAmSilent then
    begin
    Result:=-1;
    Exit;
    end
  else
    Result:=0; // minimum packet

  CS.Acquire;
  try
    if FNeedInput>0 then
      begin
      Result:=FNeedInput;
      if Result>Cardinal(SOCK_MAX_READ_SIZE) then
        Result:=SOCK_MAX_READ_SIZE;
      Exit;
      end
    else
      FData.FBytes:=FBuffIn.GetEx;
  finally
    CS.Release;
    end;

  len:=length(FData.FBytes);
  loc:=0;

  try
    while (len >= loc+6) do // Have Command with UserID or GroupID
      begin
      xUserID:=crcBytes2GateCmd(xCommand,FData.FBytes,loc); // 6 bytes
      case xCommand of
        Cmd_SendAll,   Cmd_Send_AllG,
        Cmd_SendFirst, Cmd_Send_FirstG,
        Cmd_SendMore,  Cmd_Send_MoreG,
        Cmd_SendLast,  Cmd_Send_LastG:
          begin
          if (len >= loc+10) then // Have Length
            begin
            xLength:=crcBytes2Int(FData.FBytes,loc+6); // 4 bytes
            xLeft:=len-loc-10;
            if xLeft>=xLength then // Have the expected size
              begin
              case xCommand of
                Cmd_SendAll,
                Cmd_Send_AllG:   FData.F.Command:=gc_SendAll;
                Cmd_SendFirst,
                Cmd_Send_FirstG: FData.F.Command:=gc_SendFirst;
                Cmd_SendMore,
                Cmd_Send_MoreG:  FData.F.Command:=gc_SendMore;
                Cmd_SendLast,
                Cmd_Send_LastG:  FData.F.Command:=gc_SendLast;
                end;
              case xCommand of
                Cmd_SendAll,
                Cmd_SendFirst,
                Cmd_SendMore,
                Cmd_SendLast:
                  begin
                  FData.F.ToGroupID:=0;
                  FData.F.GroupID:=xUserID and GroupIDMask;
                  end;
                Cmd_Send_AllG,
                Cmd_Send_FirstG,
                Cmd_Send_MoreG,
                Cmd_Send_LastG:
                  begin
                  FData.F.GroupID:=0;
                  FData.F.ToGroupID:=xUserID and GroupIDMask;
                  end;
                end;
              FData.FUserInfo:='';
              FData.FAccountID:='';
              FData.F.UserID:=(xUserID and UserIDMask) shr UserIDShift;
              FData.F.Length:=xLength;
              FData.F.Location:=loc+10;
              Call_OnDataReceived(Sender);
              Inc(loc,10);
              Inc(loc,xLength);
              end
            else
              begin
              Result:=xLength-xLeft;
              Break;
              end;
            end
          else
            begin
            Result:=(loc+10)-len;
            Break;
            end;
          end;
        Cmd_UserOn:
          begin
          if xUserID=(FMyUID shl UserIDShift) then
            State.F.PingInCnt:=(State.PingInCnt+1) mod 4
          else
            begin
            FData.FUserInfo:='';
            FData.FAccountID:='';
            FData.F.GroupID:=xUserID and GroupIDMask;
            FData.F.ToGroupID:=0;
            FData.F.UserID:=(xUserID and UserIDMask) shr UserIDShift;
            if FData.GroupID>0 then
              FData.F.Command:=gc_UserJoined
            else
              FData.F.Command:=gc_UserOnline;
            Call_OnInfoReceived(Sender);
            end;
          Inc(loc,6);
          end;
        Cmd_UserOff,
        Cmd_UserIn,
        Cmd_UserOut:
          begin
          FData.FUserInfo:='';
          FData.FAccountID:='';
          FData.F.GroupID:=xUserID and GroupIDMask;
          FData.F.ToGroupID:=0;
          FData.F.UserID:=(xUserID and UserIDMask) shr UserIDShift;
          if FData.GroupID>0 then
            begin
            case xCommand of
              Cmd_UserIn:  FData.F.Command:=gc_JoinedUser;
              Cmd_UserOut: FData.F.Command:=gc_LeftUser;
              Cmd_UserOff: FData.F.Command:=gc_UserLeft;
              else
                begin
                FData.F.ErrCode:=xCommand;
                FData.F.Command:=gc_Error;
                end;
              end;
            end
          else
            begin
            case xCommand of
              Cmd_UserOff: FData.F.Command:=gc_UserOffline;
              else
                begin
                FData.F.Command:=gc_Error;
                FData.F.ErrCode:=xCommand;
                end;
              end;
            end;
          if FData.Command=gc_Error then
            begin
            if LOG_GATECLIENT_EXITS then
              Log('#EXIT! ProcessInput(1) '+
                'Data Error: MyUID='+Int2Str(FData.MyUID)+
                ', UID='+Int2Str(FData.UserID)+
                ', GID='+Int2Str(FData.GroupID)+
                ', Cmd='+Int2Str(xCommand),GATE_LOG);
            Call_OnInfoReceived(Sender);

            Sender.Response.StatusText:=
              'Data Error: MyUID='+Int2Str(FData.MyUID)+
              ', UID='+Int2Str(FData.UserID)+
              ', GID='+Int2Str(FData.GroupID)+
              ', Cmd='+Int2Str(xCommand);
            // Unrecoverable error
            Result:=-1;
            UserStreamLost(Sender);
            Exit;
            end
          else
            begin
            Call_OnInfoReceived(Sender);
            Inc(loc,6);
            end;
          end;
        Cmd_UserBeFriend,
        Cmd_UserUnFriend,
        Cmd_FriendAdd,
        Cmd_FriendRemove:
          begin
          FData.FUserInfo:='';
          FData.FAccountID:='';
          FData.F.GroupID:=xUserID and GroupIDMask;
          FData.F.ToGroupID:=0;
          FData.F.UserID:=(xUserID and UserIDMask) shr UserIDShift;
          if FData.GroupID=0 then
            begin
            case xCommand of
              Cmd_UserBeFriend:  FData.F.Command:=gc_BeFriend;
              Cmd_UserUnFriend:  FData.F.Command:=gc_UnFriend;
              Cmd_FriendAdd:     FData.F.Command:=gc_FriendAdd;
              Cmd_FriendRemove:  FData.F.Command:=gc_FriendRemove;
              else
                begin
                FData.F.ErrCode:=xCommand;
                FData.F.Command:=gc_Error;
                end;
              end;
            end
          else
            begin
            FData.F.ErrCode:=xCommand;
            FData.F.Command:=gc_Error;
            end;
          if FData.Command=gc_Error then
            begin
            if LOG_GATECLIENT_EXITS then
              Log('#EXIT! ProcessInput(2) '+
                'Data Error: MyUID='+Int2Str(FData.MyUID)+
                ', UID='+Int2Str(FData.UserID)+
                ', GID='+Int2Str(FData.GroupID)+
                ', Cmd='+Int2Str(xCommand),GATE_LOG);

            Call_OnInfoReceived(Sender);

            Sender.Response.StatusText:=
              'Data Error: MyUID='+Int2Str(FData.MyUID)+
              ', UID='+Int2Str(FData.UserID)+
              ', GID='+Int2Str(FData.GroupID)+
              ', Cmd='+Int2Str(xCommand);
            // Unrecoverable error
            Result:=-1;
            UserStreamLost(Sender);
            Exit;
            end
          else
            begin
            Call_OnInfoReceived(Sender);
            Inc(loc,6);
            end;
          end;
        Cmd_UserNotReady:
          begin
          FData.FUserInfo:='';
          FData.FAccountID:='';
          FData.F.GroupID:=xUserID and GroupIDMask;
          FData.F.ToGroupID:=0;
          FData.F.UserID:=(xUserID and UserIDMask) shr UserIDShift;
          FData.F.Command:=gc_SubscribeLogOut;
          Call_OnInfoReceived(Sender);
          Inc(loc,6);
          end;
        Cmd_UserReady:
          begin
          if (len >= loc+10) then // Have Length
            begin
            xLength:=crcBytes2Int(FData.FBytes,loc+6); // 4 bytes
            xLeft:=len-loc-10;
            if xLeft>=xLength then // Have the expected size
              begin
              if xLength>0 then
                FData.FUserInfo:=RtcBytesToString(Copy(FData.FBytes,loc+10,xLength))
              else
                FData.FUserInfo:='';
              FData.FAccountID:='';
              FData.F.GroupID:=xUserID and GroupIDMask;
              FData.F.ToGroupID:=0;
              FData.F.UserID:=(xUserID and UserIDMask) shr UserIDShift;
              FData.F.Command:=gc_SubscribeLogIn;
              Call_OnInfoReceived(Sender);
              Inc(loc,10);
              Inc(loc,xLength);
              end
            else
              begin
              Result:=xLength-xLeft;
              Break;
              end;
            end
          else
            begin
            Result:=(loc+10)-len;
            Break;
            end;
          end;
        else
          begin
          FData.FUserInfo:='';
          FData.FAccountID:='';
          FData.F.GroupID:=xUserID and GroupIDMask;
          FData.F.ToGroupID:=0;
          FData.F.UserID:=(xUserID and UserIDMask) shr UserIDShift;
          FData.F.Command:=gc_Error;
          FData.F.ErrCode:=xCommand;

          if LOG_GATECLIENT_EXITS then
            Log('#EXIT! ProcessInput(3)'+
              'Data Error: MyUID='+Int2Str(FData.MyUID)+
              ', UID='+Int2Str(FData.UserID)+
              ', GID='+Int2Str(FData.GroupID)+
              ', Cmd='+Int2Str(xCommand),GATE_LOG);

          Call_OnInfoReceived(Sender);

          // Unrecoverable error
          Sender.Response.StatusText:=
            'Data Error: MyUID='+Int2Str(FData.MyUID)+
            ', UID='+Int2Str(FData.UserID)+
            ', GID='+Int2Str(FData.GroupID)+
            ', Cmd='+Int2Str(xCommand);
          Result:=-1;
          UserStreamLost(Sender);
          Exit;
          end;
        end;
      end;
  except
    on E:Exception do
      begin
      if LOG_GATECLIENT_EXITS then
        Log('#DATA! MyUID='+Int2Str(FData.MyUID),E,GATE_LOG);
      Sender.Response.StatusText:=
        '#DATA! '+RtcString(E.ClassName)+':'+RtcString(E.Message);
      Result:=-1;
      UserStreamLost(Sender);
      Exit;
      end;
    end;

  if FAmSilent or (Login_Status<gls_Connect) then Exit;

  CS.Acquire;
  try
    if loc>0 then
      FBuffIn.DelStart(loc);

    xLeft:=len-loc;
    if xLeft=0 then
      if State.InputState=ins_Recv then
        State.F.InputState:=ins_Idle;

    if Result=0 then
      Result:=6-xLeft;

    FNeedInput:=Result;
    if Result>Cardinal(SOCK_MAX_READ_SIZE) then
      Result:=SOCK_MAX_READ_SIZE;
  finally
    CS.Release;
    end;

  SetLength(FData.FBytes,0);
  SetLength(FData.FContent,0);
  end;

function TRtcHttpGateClient.ReadyToSend(Sender: TRtcConnection): boolean;
  var
    lData,xLen:Cardinal;
    data:RtcByteArray;
  begin
  Result:=False;
  EnterEvent(Sender);
  try
    if FAmSilent then Exit;
    if State.LoggedIn and (FLoginStatus=gls_Active) and State.OutputReady and FSpaceToSend and not (FNowSending or FAmSilent) then
      begin
      if not FDataWaiting then
        Call_OnReadyToSend(Sender);

      if FDataWaiting then
        begin
        SetLength(data,0);

        CS.Acquire;
        try
          { Send as much "registered" content (LeftToSend) as possible,
            without exceeding the Content Length or maximum allowed package size.
            More content could be in the buffer, prepared for sending later. }
          xLen:=State.F.LeftToSend;
          if xLen>0 then
            begin
            if xLen>FReqToSend then
              xLen:=FReqToSend;
            if xLen>Cardinal(SOCK_MAX_SEND_SIZE) then
              xLen:=SOCK_MAX_SEND_SIZE;
            if xLen<FBuffOut.Size then
              begin
              data:=FBuffOut.GetStartEx(xLen);
              FBuffOut.DelStart(xLen);
              end
            else
              begin
              data:=FBuffOut.GetEx;
              FBuffOut.Clear;
              end;
            FCryOut.CryptEx(data);

            FNowSending:=True;

            lData:=Cardinal(length(data));
            FReqToSend:=FReqToSend-lData;
            FSpaceToSend:=FReqToSend>0;

            with State.F do
              begin
              Inc(TotalSent,lData);
              Dec(LeftToSend,lData);

              FDataWaiting:=LeftToSend>0;
              OutputState:=outs_Send;
              LastOutput:=GetAppRunTime;
              end;

            if (Login_Status=gls_Active) and not (FDataWaiting or FAmSilent) then
              FEV.ResetEvent;
            end;
        finally
          CS.Release;
          end;

        if FNotSilent and (Login_Status=gls_Active) and (length(data)>0) then
          begin
          BeforeWrite(length(data));
          Sender.WriteEx(data);
          Result:=True;
          end;
        end;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.ReadyToSendNow(Sender: TRtcConnection);
  begin
  ReadyToSend(Sender);
  end;

function TRtcHttpGateClient.PingUser(const UserID: TGateUID):boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if (UserID>MaxUserID) or (UserID<MinUserID) then
    raise ERtcGateClient.Create('PingUser: UserID out of range');

  if Ready and (UserID<>MyUID) then
    begin
    CS.Acquire;
    try
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_UserOn,UserID shl UserIDShift)); // 6 bytes
      Inc(State.F.LeftToSend,6);

      State.F.ReadyToSend:=False;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.AddUserToGroup(const OwnerID, GroupID, UserID: TGateUID; Managed:boolean=False):boolean;
  var
    toManage:boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if (UserID>MaxUserID) or (UserID<MinUserID) then
    raise ERtcGateClient.Create('AddUserToGroup: UserID out of range');
  if (OwnerID>MaxUserID) or (OwnerID<MinUserID) then
    raise ERtcGateClient.Create('AddUserToGroup: OwnerID out of range');
  if GroupID>MaxGID then
    raise ERtcGateClient.Create('AddUserToGroup: GroupID out of range');

  if Ready then
    begin
    toManage:=Managed and (OwnerID=MyUID);

    CS.Acquire;
    try
      { Place "AddUserToGroup" command into the sending buffers,
        but do NOT register it for sending to avoid triggering the
        "ReadyToSend" event before we add the User to our internal Group list.

        This ensures that the "AddUserToGroup" command will be sent to the Gateway
        before anything else we might add to the sending buffers before adding the
        User to our User Group, while at the same time ensuring that a "ReadyToSend"
        event will be triggered at least once after we add the user to the Group list. }
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_AddUserToGroup,(OwnerID shl UserIDShift) or GroupID)); // 6 bytes
      FBuffOut.AddEx(crcGateID2Bytes(UserID)); // 4 bytes
    finally
      CS.Release;
      end;

    // if this is a managed user group, add the User to our internal Group list
    if toManage then
      UserAddedToMyGroup(GroupID,UserID);

    CS.Acquire;
    try
      { Place a "PING" into the output buffer and register both commands for sending,
        if our previously added command (above) is still waiting inside the buffer. }
      if FBuffOut.Size=State.F.LeftToSend+10 then
        begin
        Inc(State.F.LeftToSend,10);

        if toManage then
          begin // Ping User
          FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_UserOn,UserID shl UserIDShift)); // 6 bytes
          Inc(State.F.LeftToSend,6);
          end;

        State.F.ReadyToSend:=False;
        FDataWaiting:=True;
        FNeedOutput:=True;

        Result:=True;
        FEV.SetEvent;
        end;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);

    if toManage and not Result then // something interrupted the sending process, remove user from group
      UserRemovedFromMyGroup(GroupID,UserID);
    end;
  end;

function TRtcHttpGateClient.RemoveGroup(const OwnerID, GroupID: TGateUID):boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if (OwnerID>MaxUserID) or (OwnerID<MinUserID) then
    raise ERtcGateClient.Create('RemoveGroup: OwnerID out of range');
  if GroupID>MaxGID then
    raise ERtcGateClient.Create('RemoveGroup: GroupID out of range');

  if Ready then
    begin
    CS.Acquire;
    try
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_RemoveGroup,(OwnerID shl UserIDShift) or GroupID)); // 6 bytes
      Inc(State.F.LeftToSend,6);

      State.F.ReadyToSend:=False;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.RemUserFromGroup(const OwnerID, GroupID, UserID: TGateUID):boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if (UserID>MaxUserID) or (UserID<MinUserID) then
    raise ERtcGateClient.Create('RemUserFromGroup: UserID out of range');
  if (OwnerID>MaxUserID) or (OwnerID<MinUserID) then
    raise ERtcGateClient.Create('RemUserFromGroup: OwnerID out of range');
  if GroupID>MaxGID then
    raise ERtcGateClient.Create('RemUserFromGroup: GroupID out of range');

  if Ready then
    begin
    CS.Acquire;
    try
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_RemoveUserFromGroup,(OwnerID shl UserIDShift) or GroupID)); // 6 bytes
      FBuffOut.AddEx(crcGateID2Bytes(UserID)); // 4 bytes
      Inc(State.F.LeftToSend,10);

      State.F.ReadyToSend:=False;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.AddFriend(const UserID: TGateUID): boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if (UserID>MaxUserID) or (UserID<MinUserID) then
    raise ERtcGateClient.Create('AddFriend: UserID out of range');

  if Ready then
    begin
    CS.Acquire;
    try
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_FriendAdd,(UserID shl UserIDShift))); // 6 bytes
      Inc(State.F.LeftToSend,6);

      State.F.ReadyToSend:=False;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.RemoveFriend(const UserID: TGateUID): boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if (UserID>MaxUserID) or (UserID<MinUserID) then
    raise ERtcGateClient.Create('RemoveFriend: UserID out of range');

  if Ready then
    begin
    CS.Acquire;
    try
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_FriendRemove,(UserID shl UserIDShift))); // 6 bytes
      Inc(State.F.LeftToSend,6);

      State.F.ReadyToSend:=False;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.RemoveAllFriends: boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if Ready then
    begin
    CS.Acquire;
    try
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_RemoveFriends,(FMyUID shl UserIDShift))); // 6 bytes
      Inc(State.F.LeftToSend,6);

      State.F.ReadyToSend:=False;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.Subscribe(const Channel: RtcString; GroupID: TGateUID = 0):boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if GroupID>MaxGID then
    raise ERtcGateClient.Create('Subscribe: GroupID out of range');

  if length(Channel)=0 then Exit
  else if not(rtcKeyValid(Channel) or rtcLockValid(Channel)) then
    raise ERtcGateClient.Create('Invalid Channel value. Unable to Subscribe.');
  if Ready then
    begin
    CS.Acquire;
    try
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_Subscribe,(FMyUID shl UserIDShift) or GroupID)); // 6 bytes
      FBuffOut.AddEx(crcInt2Bytes(length(Channel))); // 4 bytes
      Inc(State.F.LeftToSend,10);

      FBuffOut.Add(Channel);
      Inc(State.F.LeftToSend,length(Channel));

      State.F.ReadyToSend:=False;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.UnSubscribe(const Channel: RtcString):boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if length(Channel)=0 then Exit
  else if not(rtcKeyValid(Channel) or rtcLockValid(Channel)) then
    raise ERtcGateClient.Create('Invalid Channel value. Unable to UnSubscribe.');
  if Ready then
    begin
    CS.Acquire;
    try
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_UnSubscribe,(FMyUID shl UserIDShift))); // 6 bytes
      FBuffOut.AddEx(crcInt2Bytes(length(Channel))); // 4 bytes
      Inc(State.F.LeftToSend,10);

      FBuffOut.Add(Channel);
      Inc(State.F.LeftToSend,length(Channel));

      State.F.ReadyToSend:=False;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

procedure TRtcHttpGateClient.SetGateAddr(const Value: RtcString);
  begin
  FGateAddr:=Value;
  end;

procedure TRtcHttpGateClient.SetGatePort(const Value: RtcString);
  begin
  FGatePort:=Value;
  end;

procedure TRtcHttpGateClient.SetGateIPV(const Value: RtcIPV);
  begin
  FGateIPV:=Value;
  end;

procedure TRtcHttpGateClient.PingCheck;
  var
    MyTime:TAppRunTime;
    pinged,doRun:boolean;
  begin
  if FAmSilent or (FLoginStatus<gls_Retry) then Exit;

  MyTime:=GetAppRunTime;
  if Login_Status=gls_Retry then
    begin
    doRun:=False;
    CS.Acquire;
    try
      if FAutoRetry>0 then
        if Login_Status=gls_Retry then
          begin
          Dec(FAutoRetry);
          if FAutoRetry=0 then
            begin
            FAutoRetry:=-1;
            doRun:=True;
            end;
          end;
    finally
      CS.Release;
      end;
    if doRun then
      RetryLogin;
    end
  else if Login_Status=gls_Relogin then
    begin
    doRun:=False;
    CS.Acquire;
    try
      if FAutoRetry>0 then
        if Login_Status=gls_Relogin then
          begin
          Dec(FAutoRetry);
          if FAutoRetry=0 then
            begin
            FAutoRetry:=-1;
            doRun:=True;
            end;
          end;
    finally
      CS.Release;
      end;
    if doRun then
      User_Login(nil);
    end
  else if Ready then
    begin
    if (MyTime>State.LastInput+CLIENTCHECKIN_TIMEOUT) and
       (MyTime>State.LastOutput+CLIENTCHECKOUT_TIMEOUT) then
      begin
      if LOG_GATECLIENT_TIMEOUTS then
        Log('#TIMEOUT! INPUT and OUTPUT '+Int2Str(MyUID)+
            ' ('+Float2Str((MyTime-State.LastInput)/RUN_TIMER_PRECISION)+
            '/'+ Float2Str((MyTime-State.LastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
    {$IFNDEF DISABLE_PING}
      ResetStreams;
    {$ENDIF}
      end
    else if MyTime>State.LastInput+CLIENTCHECKMAXIN_TIMEOUT then
      begin
      if LOG_GATECLIENT_TIMEOUTS then
        Log('#TIMEOUT! max.INPUT '+Int2Str(MyUID)+
            ' ('+Float2Str((MyTime-State.LastInput)/RUN_TIMER_PRECISION)+
            '/'+ Float2Str((MyTime-State.LastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
    {$IFNDEF DISABLE_PING}
      ResetStreams;
    {$ENDIF}
      end
    else if MyTime>State.LastOutput+CLIENTCHECKMAXOUT_TIMEOUT then
      begin
      if LOG_GATECLIENT_TIMEOUTS then
        Log('#TIMEOUT! max.OUTPUT '+Int2Str(MyUID)+
            ' ('+Float2Str((MyTime-State.LastInput)/RUN_TIMER_PRECISION)+
            '/'+ Float2Str((MyTime-State.LastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
    {$IFNDEF DISABLE_PING}
      ResetStreams;
    {$ENDIF}
      end
    else if not (FNowSending or FDataWaiting or FOutputWasLost or FInputWasLost or FNeedOutput) then
      begin
      if (MyTime>State.LastOutput+SENDPING_INTERVAL) and
         (MyTime>State.LastPing+SENDPING_INTERVAL) then
        begin
        CS.Acquire;
        try
          if not (FNowSending or FDataWaiting or FOutputWasLost or FInputWasLost or FNeedOutput) then
            begin
            pinged:=True;
            State.F.LastPing:=MyTime;
            State.F.PingOutCnt:=(State.PingOutCnt+1) mod 4;
            FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_UserOn,(FMyUID shl UserIDShift))); // 6 bytes
            Inc(State.F.LeftToSend,6);

            FNeedOutput:=True;
            FDataWaiting:=True;

            FEV.SetEvent;
            end
          else
            pinged:=False;
        finally
          CS.Release;
          end;
        if pinged and Ready and not FBlocking and assigned(ClientOUT) then
          ClientOUT.PostEvent(ReadyToSendNow);
        end;
      end;
    end
  else if (MyTime>State.LastInput+CLIENTCHECKLOGIN_TIMEOUT) then
    begin
    if LOG_GATECLIENT_TIMEOUTS then
      Log('#TIMEOUT! INPUT '+Int2Str(MyUID)+
          ' ('+Float2Str((MyTime-State.LastInput)/RUN_TIMER_PRECISION)+
          '/'+ Float2Str((MyTime-State.LastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
  {$IFNDEF DISABLE_PING}
    ResetStreams;
  {$ENDIF}
    end
  else if (MyTime>State.LastOutput+CLIENTCHECKLOGIN_TIMEOUT) then
    begin
    if LOG_GATECLIENT_TIMEOUTS then
      Log('#TIMEOUT! OUTPUT '+Int2Str(MyUID)+
          ' ('+Float2Str((MyTime-State.LastInput)/RUN_TIMER_PRECISION)+
          '/'+ Float2Str((MyTime-State.LastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
  {$IFNDEF DISABLE_PING}
    ResetStreams;
  {$ENDIF}
    end;
  end;

function TRtcHttpGateClient.GetActive: boolean;
  begin
  Result:=FLoginStatus>gls_Offline;
  end;

function TRtcHttpGateClient.GetReady: boolean;
  begin
  Result:=FLoginStatus=gls_Active;
  end;

procedure TRtcHttpGateClient.SetActive(const Value: boolean);
  begin
  if Value<>Active then
    if Value then
      begin
      if assigned(ClientIN) and ClientIN.inThread then
        User_LogIn(ClientIN)
      else if assigned(ClientOUT) and ClientOUT.inThread then
        User_LogIn(ClientOUT)
      else
        User_LogIn(nil);
      end
    else
      begin
      if assigned(ClientIN) and ClientIN.inThread then
        User_LogOut(ClientIN)
      else if assigned(ClientOUT) and ClientOUT.inThread then
        User_LogOut(ClientOUT)
      else
        User_LogOut(nil);
      end;
  end;

function TRtcHttpGateClient.SendBytes(const UserID, GroupID: TGateUID; const CallID:word; const data: RtcByteArray):boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if (UserID>MaxUserID) or (UserID<MinUserID) then
    raise ERtcGateClient.Create('SendBytes: UserID out of range');
  if GroupID>MaxGID then
    raise ERtcGateClient.Create('SendBytes: GroupID out of range');

  if Ready then
    begin
    CS.Acquire;
    try
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_SendData,(UserID shl UserIDShift or GroupID))); // 6 bytes
      FBuffOut.AddEx(crcInt2Bytes(2+length(data))); // 4 bytes
      FBuffOut.AddEx(Word2Bytes(CallID)); // 2 bytes
      Inc(State.F.LeftToSend,12);

      FBuffOut.AddPackEx(data,SOCK_MAX_SEND_SIZE);
      Inc(State.F.LeftToSend,length(data));

      State.F.ReadyToSend:=False;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.SendBytes(const UserID, GroupID: TGateUID; const CallID:word; const data: RtcString):boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if (UserID>MaxUserID) or (UserID<MinUserID) then
    raise ERtcGateClient.Create('SendBytes: UserID out of range');
  if GroupID>MaxGID then
    raise ERtcGateClient.Create('SendBytes: GroupID out of range');

  if Ready then
    begin
    CS.Acquire;
    try
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_SendData,(UserID shl UserIDShift) or GroupID)); // 6 bytes
      FBuffOut.AddEx(crcInt2Bytes(2+length(data))); // 4 bytes
      FBuffOut.AddEx(Word2Bytes(CallID)); // 2 bytes
      Inc(State.F.LeftToSend,12);

      FBuffOut.AddPack(data,SOCK_MAX_SEND_SIZE);
      Inc(State.F.LeftToSend,length(data));

      State.F.ReadyToSend:=False;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.SendBytes(const UserID, GroupID: TGateUID; const CallID:word):boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if (UserID>MaxUserID) or (UserID<MinUserID) then
    raise ERtcGateClient.Create('SendBytes: UserID out of range');
  if GroupID>MaxGID then
    raise ERtcGateClient.Create('SendBytes: GroupID out of range');

  if Ready then
    begin
    CS.Acquire;
    try
      FBuffOut.AddEx(crcGateCmd2Bytes(Cmd_SendData,(UserID shl UserIDShift) or GroupID)); // 6 bytes
      FBuffOut.AddEx(crcInt2Bytes(2)); // 4 bytes
      FBuffOut.AddEx(Word2Bytes(CallID)); // 2 bytes
      Inc(State.F.LeftToSend,12);

      State.F.ReadyToSend:=False;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      CS.Release;
      end;

    if Result and Ready and not FBlocking and assigned(ClientOUT) then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

procedure TRtcHttpGateClient.ClientBeforeDestroy(Sender: TRtcConnection);
  begin
  ECS.Acquire;
  try
    if assigned(ClientIN) and (Sender=ClientIN) then
      begin
      Dec(FConnCount);
      RemoveComponent(Sender);
      end
    else if assigned(ClientOUT) and (Sender=ClientOUT) then
      begin
      Dec(FConnCount);
      RemoveComponent(Sender);
      end
    else
      Exit; // do NOT count other connections
  finally
    ECS.Release;
    end;
  end;

procedure TRtcHttpGateClient.ClientAfterDestroy(Sender: TRtcConnection);
  begin
  ECS.Acquire;
  try
    if assigned(ClientIN) and (Sender=ClientIN) then
      ClientIN:=nil
    else if assigned(ClientOUT) and (Sender=ClientOUT) then
      ClientOUT:=nil
    else
      Exit; // do NOT count other connections
  finally
    ECS.Release;
    end;
  end;

constructor TRtcHttpGateClient.Create(AOwner:TComponent);
  begin
  inherited;
  FConnCount:=0;

  ClientDummy := TRtcHttpClient.Create(self);

  DataLogin1 := TRtcDataRequest.Create(nil);
  DataLogin1.HyperThreading:=True;
  DataLogin1.OnBeginRequest:=DataLogin1BeginRequest;
  DataLogin1.OnDataReceived:=DataLogin1DataReceived;
  DataLogin1.OnResponseAbort:=DataResponseAbort;

  DataLogin2 := TRtcDataRequest.Create(nil);
  DataLogin2.HyperThreading:=True;
  DataLogin2.OnBeginRequest:=DataLogin2BeginRequest;
  DataLogin2.OnDataReceived:=DataLogin2DataReceived;
  DataLogin2.OnResponseAbort:=DataResponseAbort;

  DataOUT := TRtcDataRequest.Create(nil);
  DataOUT.HyperThreading:=True;
  DataOUT.OnBeginRequest:=DataOUTBeginRequest;
  DataOUT.OnDataReceived:=DataOUTDataReceived;
  DataOUT.OnDataSent:=DataOUTDataSent;
  DataOUT.OnDataOut:=DataOUTDataOut;
  DataOUT.OnDataIn:=DataOUTDataIn;
  DataOUT.OnResponseAbort:=DataResponseAbort;

  DataIN := TRtcDataRequest.Create(nil);
  DataIN.HyperThreading:=True;
  DataIN.OnBeginRequest:=DataINBeginRequest;
  DataIN.OnDataReceived:=DataINDataReceived;
  DataIN.OnResponseAbort:=DataResponseAbort;
  DataIN.OnDataIn:=DataINDataIn;
  DataIN.OnDataOut:=DataINDataOut;

  ClientIN:=TRtcHttpClient.Create(self);
  ClientIN.AfterDestroy:=ClientAfterDestroy;
  ClientIN.BeforeDestroy:=ClientBeforeDestroy;
  ClientIN.MultiThreaded:=True;

  DataIN.Client:=ClientIN;
  DataLogin1.Client:=ClientIN;
  Inc(FConnCount);

  ClientOUT:=TRtcHttpClient.Create(self);
  ClientOUT.AfterDestroy:=ClientAfterDestroy;
  ClientOUT.BeforeDestroy:=ClientBeforeDestroy;
  ClientOUT.MultiThreaded:=True;

  DataOUT.Client:=ClientOUT;
  DataLogin2.Client:=ClientOUT;
  Inc(FConnCount);

  FUserGroups:=TRtcGateUserGroupStates.Create;
  FSharedGroups:=TRtcGateUserGroupIDs.Create;
  FState:=TRtcGateClientStateInfo.Create;

  FUserLogin:=TRtcUserLoginInfo.Create;

  FGateClientLinks:=TRtcGateClientLinkList.Create;

  FData:=TRtcGateClientData.Create;

  FGateFileName:='/';

  FGATEURI_PING:='/'+GATEURI_PING;
  FGATEURI_LOGIN:='/'+GATEURI_LOGIN;
  FGATEURI_INPUT:='/'+GATEURI_INPUT;
  FGATEURI_OUTPUT:='/'+GATEURI_OUTPUT;
  FGATEURI_INPUTRESET:='/'+GATEURI_INPUTRESET;
  FGATEURI_OUTPUTRESET:='/'+GATEURI_OUTPUTRESET;

  FGATE_PRIMARY_KEY:='';
  FGATE_USERAUTH:='';
  FGATE_USERINFO:='';
  FGATE_SECONDARY_KEY:='';

  CS:=TRtcCritSec.Create;
  ECS:=TRtcCritSec.Create;
  FEV:=TRtcEvent.Create(True,False);

  FUseBlocking:=False;
  FUseProxy:=False;
  FUseWinHTTP:=False;
  FUseSSL:=False;
  FCryptPlugin:=nil;

  FWasReset:=False;

{$IFDEF WINDOWS}
  FBlocking:=(FCryptPlugin=nil) and (FUseProxy or FUseSSL or FUseWinHTTP);
{$ELSE}
  FBlocking:=False;
{$ENDIF}

  FLoginStatus:=gls_Offline;

  with State.F do
    begin
    LoggedIn:=False;
    InputReady:=False;
    OutputReady:=False;

    InputState:=ins_Closed;
    OutputState:=outs_Closed;
    PingInCnt:=0;
    PingOutCnt:=0;
    end;

  FCryIn:=TRtcCrypt.Create;
  FBuffIn:=TRtcHugeByteArray.Create;
  FBuffRcv:=tObjList.Create(16);
  FNeedInput:=0;

  FCryOut:=TRtcCrypt.Create;
  FBuffOut:=TRtcHugeByteArray.Create;

  FUsers:=tObjList.Create(16);
  FGroups:=tObjList.Create(16);

  FInsideEventIN:=0;
  FInsideEventOUT:=0;
  FInsideEventDummy:=0;
  FInsideEventMain:=0;
  end;

function TRtcHttpGateClient.AllClientsReleased:boolean;
  begin
  if (FInsideEventIN=0) and (FInsideEventOUT=0) and
     (FInsideEventDummy=0) and (FInsideEventMain=0) and
     (FConnCount=0) then
    begin
    ECS.Acquire;
    try
      Result:=(FInsideEventIN=0) and (FInsideEventOUT=0) and
              (FInsideEventDummy=0) and (FInsideEventMain=0) and
              (FConnCount=0) and (ClientOUT=nil) and (ClientIN=nil);
    finally
      ECS.Release;
      end;
    end
  else
    Result:=False;
  end;

procedure TRtcHttpGateClient.EnterEvent(Sender:TRtcConnection);
  begin
  if Sender=nil then
    begin
    ECS.Acquire;
    try
      Inc(FInsideEventMain);
    finally
      ECS.Release;
      end;
    end
  else if Sender=ClientDummy then
    begin
    ECS.Acquire;
    try
      Inc(FInsideEventDummy);
    finally
      ECS.Release;
      end;
    end
  else if Sender=ClientIN then
    Inc(FInsideEventIN)
  else if Sender=ClientOUT then
    Inc(FInsideEventOUT);
  end;

procedure TRtcHttpGateClient.LeaveEvent(Sender:TRtcConnection);
  begin
  if Sender=nil then
    begin
    ECS.Acquire;
    try
      Dec(FInsideEventMain);
    finally
      ECS.Release;
      end;
    end
  else if Sender=ClientDummy then
    begin
    ECS.Acquire;
    try
      Dec(FInsideEventDummy);
    finally
      ECS.Release;
      end;
    end
  else if Sender=ClientIN then
    Dec(FInsideEventIN)
  else if Sender=ClientOUT then
    Dec(FInsideEventOUT);
  end;

destructor TRtcHttpGateClient.Destroy;
  begin
  FAmSilent:=True;
  FNotSilent:=False;

  AutoLogin:=False;

  PausePingTimer;
  if assigned(ClientIN) then ClientIN.Release;
  if assigned(ClientOUT) then ClientOUT.Release;

  if RtcWaitFor(AllClientsReleased,AllClientsReleased,CLIENTRELEASE_MAXWAIT)<>wait_OK then
    begin
    Sleep(RTC_WAITFOR_PROVIDER_FINISH);
    if LOG_EXCEPTIONS then
      Log(RtcString(ClassName)+'.Destroy - ReleaseClients ('+GateAddr+':'+GatePort+') TIMEOUT!','ERROR');
    try
      RtcFreeAndNil(ClientIN);
	  if LOG_EXCEPTIONS then
        Log(RtcString(ClassName)+'.Destroy - ClientIN destroyed','ERROR');
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log(RtcString(ClassName)+'.Destroy - ClientIN.Free',E,'ERROR');
      end;
    Sleep(RTC_WAITFOR_PROVIDER_FINISH);
    ClientIN:=nil;
    try
      RtcFreeAndNil(ClientOUT);
      if LOG_EXCEPTIONS then
        Log(RtcString(ClassName)+'.Destroy - ClientOUT destroyed','ERROR');
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log(RtcString(ClassName)+'.Destroy - ClientOUT.Free',E,'ERROR');
      end;
    Sleep(RTC_WAITFOR_PROVIDER_FINISH);
    ClientOUT:=nil;
    end;
  Sleep(RTC_WAITFOR_PROVIDER_FINISH);

  StopPingTimer;

  AccountManager:=nil;
  RemoveAllGateClientLinks;

  DataLogin1.Client:=nil;
  DataLogin2.Client:=nil;
  DataIN.Client:=nil;
  DataOUT.Client:=nil;

  RtcFreeAndNil(FGateClientLinks);

  if assigned(FData) then
    begin
    SetLength(FData.FBytes,0);
    SetLength(FData.FContent,0);
    RtcFreeAndNil(FData);
    end;

  RtcFreeAndNil(FUserLogin);

  RtcFreeAndNil(ClientOUT);
  RtcFreeAndNil(ClientIN);
  RtcFreeAndNil(ClientDummy);

  RtcFreeAndNil(DataLogin1);
  RtcFreeAndNil(DataLogin2);
  RtcFreeAndNil(DataIN);
  RtcFreeAndNil(DataOUT);

  RtcFreeAndNil(FCryIn);
  RtcFreeAndNil(FCryOut);
  RtcFreeAndNil(FBuffIn);
  RtcFreeAndNil(FBuffOut);
  RtcFreeAndNil(FBuffRcv);
  RtcFreeAndNil(FUsers);
  RtcFreeAndNil(FGroups);

  RtcFreeAndNil(FUserGroups);
  RtcFreeAndNil(FSharedGroups);
  RtcFreeAndNil(FState);

  RtcFreeAndNil(CS);
  RtcFreeAndNil(ECS);
  RtcFreeAndNil(FEV);

  inherited;
  end;

procedure TRtcHttpGateClient.BeforeWrite(const Len:int64);
  begin
  if FWriteLimit>0 then
    if FWriteCount=0 then
      begin
      FWriteCount:=Len;
      FWriteTime:=GetTickTime64;
      FWriteMax:=FWriteLimit*125 div 40; // check 40 times per second
      end
    else
      begin
      FWriteDelta:=GetTickTime64-FWriteTime;
      if FWriteDelta>0 then
        begin
        Inc(FWriteTime,FWriteDelta);
        FWriteSpeed:=FWriteDelta*FWriteLimit shr 3;
        if FWriteCount>FWriteSpeed then
          begin
          Dec(FWriteCount,FWriteSpeed); // above speed limit
          FWriteSpeed:=(FWriteCount shl 3) div FWriteLimit;
          Sleep(FWriteSpeed);
          Inc(FWriteTime,FWriteSpeed);
          end;
        FWriteCount:=Len; // below speed limit
        end
      else if FWriteCount>FWriteMax then
        begin
        FWriteSpeed:=(FWriteCount shl 3) div FWriteLimit;
        Sleep(FWriteSpeed);
        Inc(FWriteTime,FWriteSpeed);
        FWriteCount:=Len;
        end
      else
        Inc(FWriteCount,Len);
      end;
  end;

procedure TRtcHttpGateClient.SetUseBlocking(const Value: boolean);
  begin
  FUseBlocking := Value;
  end;

procedure TRtcHttpGateClient.SetUseProxy(const Value: boolean);
  begin
  FUseProxy := Value;
  end;

procedure TRtcHttpGateClient.SetUseSSL(const Value: boolean);
  begin
  FUseSSL := Value;
  end;

procedure TRtcHttpGateClient.SetUseWinHTTP(const Value: boolean);
  begin
  FUseWinHTTP := Value;
  end;

procedure TRtcHttpGateClient.Call_BeforeLogIn(Sender: TRtcConnection);
  begin
  EnterEvent(Sender);
  try
    if FAmSilent then Exit;

    if assigned(FAccounts) then
      FAccounts.Call_BeforeLogIn(Sender);
    FGateClientLinks.DoBeforeLogIn(Sender);

    if Call(FConBeforeLogIn,FGuiBeforeLogIn,Sender) then
      if assigned(FThread) then
        FThread.Post_BeforeLogIn(Sender);

    StartPingTimer;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.Call_AfterLoggedIn(Sender: TRtcConnection);
  begin
  EnterEvent(Sender);
  try
    if FAmSilent then Exit;

    if assigned(FAccounts) then
      begin
      FAccounts.RefreshClient(self);
      FAccounts.Call_AfterLoggedIn(Sender);
      end;
    FGateClientLinks.DoAfterLoggedIn(Sender);

    if Call(FConAfterLoggedIn,FGuiAfterLoggedIn,Sender) then
      if assigned(FThread) then
        FThread.Post_AfterLoggedIn(Sender);
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.Call_AfterLogout(Sender: TRtcConnection);
  begin
  EnterEvent(Sender);
  try
    if FAmSilent then Exit;

    PausePingTimer;

    if assigned(FAccounts) then
      FAccounts.Call_AfterLogOut(Sender);
    FGateClientLinks.DoAfterLogOut(Sender);

    if Call(FConAfterLogout,FGuiAfterLogout,Sender) then
      if assigned(FThread) then
        FThread.Post_AfterLogOut(Sender);
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.Call_AfterLoginFail(Sender: TRtcConnection);
  begin
  EnterEvent(Sender);
  try
    if FAmSilent then Exit;

    PausePingTimer;

    if assigned(FAccounts) then
      FAccounts.Call_AfterLoginFail(Sender);
    FGateClientLinks.DoAfterLoginFail(Sender);

    if Call(FConAfterLogInFail,FGuiAfterLogInFail,Sender) then
      if assigned(FThread) then
        FThread.Post_AfterLoginFail(Sender);
  finally
    LeaveEvent(Sender);
    end;
  end;

type
  // @exclude
  TRtcHugeByteArrayWithCallID = class(TRtcHugeByteArray)
  public
    CallID:word;
    end;

procedure TRtcHttpGateClient.Call_OnDataReceived(Sender: TRtcConnection);
  var
    Buff:TRtcHugeByteArrayWithCallID;
    inBuff:boolean;
  procedure ProcessInternalCall;
    {$IFDEF RTC_RSA}
    procedure ExecuteVerifyAccount;
      var
        i:byte;
        linkAcc,chckAcc,privAcc:RtcString;
        pke,pka,pkp:RtcByteArray;
        buff:TRtcHugeByteArray;
      begin
      SetLength(pke,0);
      SetLength(pkp,0);
      SetLength(pka,0);
      if assigned(FAccounts) then
        begin
        if length(FData.Content)>LengthOfMD5Code8Bit then
          begin
          linkAcc:=RtcBytesToString(FData.Content,0,LengthOfMD5Code8Bit);
          privAcc:=FAccounts.FindPrivate(linkAcc);
          if privAcc<>'' then
            begin
            if CheckNotVerifiedAccount(FData.UserID,privAcc) then
              begin
              pke:=Copy(FData.Content,LengthOfMD5Code8Bit,length(FData.Content)-LengthOfMD5Code8Bit);
              try
                pka:=FAccounts.PrivateDecrypt(privAcc,pke);
                // = MD5Code8Bit(PublicKey+AccountID) + AccountID + PublicKey
              except
                // Bad Encryption
                FData.FUserInfo:='';
                FData.FAccountID:=privAcc;
                FData.F.Command:=gc_AccountBadVerify;
                RemoveAccountVerification(FData.UserID,privAcc);
                Call_OnInfoReceived(Sender);
                Exit;
                end;
              chckAcc:=rtcCalculateLock(privAcc);
              i:=length(chckAcc);
              if (length(pka)>LengthOfMD5Code8Bit+i) and
                 (RtcBytesToString(pka,LengthOfMD5Code8Bit,i)=chckAcc) then
                begin
                pkp:=Copy(pka,i+LengthOfMD5Code8Bit,length(pka)-i-LengthOfMD5Code8Bit); // Public Key
                // Check data "CRC"
                if MD5Code8Bit(RtcBytesToString(pkp)+chckAcc)<>RtcBytesToString(Copy(pka,0,LengthOfMD5Code8Bit)) then
                  begin
                  // Bad CRC
                  FData.FUserInfo:='';
                  FData.FAccountID:=privAcc;
                  FData.F.Command:=gc_AccountBadVerify;
                  RemoveAccountVerification(FData.UserID,privAcc);
                  Call_OnInfoReceived(Sender);
                  Exit;
                  end
                else
                  begin
                  buff:=TRtcHugeByteArray.Create;
                  try
                    buff.Add(MD5Code4Bit(chckAcc));
                    buff.Add(MD5Code6Bit(RtcBytesToString(pkp)));
                    buff.AddEx(crcGateID2Bytes(FData.UserID));
                    buff.AddEx(crcGateID2Bytes(MyUID));
                    try
                      pke:=FAccounts.PublicEncrypt(pkp, FAccounts.MakeSignature(privAcc,buff.GetEx) );
                    except
                      // Bad Encryption
                      FData.FUserInfo:='';
                      FData.FAccountID:=privAcc;
                      FData.F.Command:=gc_AccountBadVerify;
                      RemoveAccountVerification(FData.UserID,privAcc);
                      Call_OnInfoReceived(Sender);
                      Exit;
                      end;
                  finally
                    RtcFreeAndNil(buff);
                    end;
                  if AddUserToAccount(FData.UserID,0,privAcc,FData.UserInfo,True) then // account verified
                    begin
                    FData.F.Command:=gc_AccountVerified;
                    Call_OnInfoReceived(Sender);
                    end;
                  SendBytes(FData.UserID,FData.ToGroupID,cid_AccountVerification,pke);
                  end;
                end
              else
                begin
                // Bad Account ID
                FData.FUserInfo:='';
                FData.FAccountID:=privAcc;
                FData.F.Command:=gc_AccountBadVerify;
                RemoveAccountVerification(FData.UserID,privAcc);
                Call_OnInfoReceived(Sender);
                end;
              end;
            end
          end;
        end;
      end;
    procedure ExecuteAccountVerification;
      var
        acc:RtcString;
        pke:RtcByteArray;
        buff:TRtcHugeByteArray;
        res:boolean;
      begin
      SetLength(pke,0);
      if assigned(FAccounts) and (FData.ToGroupID>0) and (length(FData.Content)>0) then
        begin
        acc:=FAccounts.FindAccountID(FData.ToGroupID);
        if acc<>'' then
          begin
          if CheckNotVerifiedAccount(FData.UserID,acc) then
            begin
            try
              pke:=FAccounts.PublicDecrypt(FData.Content);
            except
              FData.FUserInfo:='';
              FData.FAccountID:=acc;
              FData.F.Command:=gc_AccountBadVerify;
              RemoveAccountVerification(FData.UserID,acc);
              Call_OnInfoReceived(Sender);
              Exit;
              end;
            buff:=TRtcHugeByteArray.Create;
            try
              buff.Add(MD5Code4Bit(acc));
              buff.Add(MD5Code6Bit(RtcBytesToString(FAccounts.PublicKey)));
              buff.AddEx(crcGateID2Bytes(MyUID));
              buff.AddEx(crcGateID2Bytes(FData.UserID));
              try
                res:=FAccounts.VerifySignature(acc,buff.GetEx,pke);
              except
                res:=False;
                end;
            finally
              RtcFreeAndNil(buff);
              end;
            if res then
              begin
              if AddUserToAccount(FData.UserID,0,acc,FData.UserInfo,True) then // account verified
                begin
                FData.F.Command:=gc_AccountVerified;
                Call_OnInfoReceived(Sender);
                end;
              end
            else
              begin
              FData.FUserInfo:='';
              FData.FAccountID:=acc;
              FData.F.Command:=gc_AccountBadVerify;
              RemoveAccountVerification(FData.UserID,acc);
              Call_OnInfoReceived(Sender);
              end;
            end;
          end;
        end;
      end;
    {$ENDIF}
    procedure ExecuteAccountTransfer;
      var
        accKey,accKeyMD5,
        accData,accID:RtcString;
        adata:RtcByteArray;
      begin
      SetLength(adata,0);
      if assigned(FAccounts) then
        begin
        if length(FData.Content)>LengthOfMD5Code8Bit then
          begin
          accKey:=FAccounts.AuthCode;
          accData:=RtcBytesToString(FData.Content);
          DeCrypt(accData,MD5Code4Bit(accKey));
          accKeyMD5:=Copy(accData,length(accData)-LengthOfMD5Code8Bit+1,LengthOfMD5Code8Bit);
          if accKeyMD5<>MD5Code8bit(accKey) then
            begin
            // Bad sender's AuthCode?
            FData.F.Command:=gc_AccountBadReceive;
            Call_OnInfoReceived(Sender);
            Exit;
            end
          else
            begin
            adata:=RtcStringToBytes(accData,1,length(accData)-LengthOfMD5Code8Bit);
            try
              FAccounts.CheckAccount(adata);
              accID:=FAccounts.RegisterAccount(adata,True,False);
            except
              // Bad Encryption?
              FData.F.Command:=gc_AccountBadReceive;
              Call_OnInfoReceived(Sender);
              Exit;
              end;
            if accID<>'' then
              begin
              FData.FAccountID:=accID;
              FData.F.Command:=gc_AccountReceived;
              Call_OnInfoReceived(Sender);

              SendBytes(FData.UserID,0,cid_AccountReceived,MD5Code8bit(accID));
              end
            else
              begin
              // Bad Account Data?
              FData.F.Command:=gc_AccountBadReceive;
              Call_OnInfoReceived(Sender);
              end;
            end
          end;
        end;
      end;
    procedure ExecuteAccountReceived;
      var
        accIDMD5,accID:RtcString;
      begin
      if assigned(FAccounts) then
        begin
        if length(FData.Content)=LengthOfMD5Code8Bit then
          begin
          accIDMD5:=RtcBytesToString(FData.Content);
          accID:=FAccounts.FindLink(accIDMD5);
          if accID<>'' then
            begin
            FData.FAccountID:=accID;
            FData.F.Command:=gc_AccountTransferred;
            end
          else
            FData.F.Command:=gc_AccountBadTransfer;
          Call_OnInfoReceived(Sender);
          end;
        end;
      end;
    begin
    try
      case FData.CallID of
      {$IFDEF RTC_RSA}
        cid_VerifyAccount: ExecuteVerifyAccount;
        cid_AccountVerification: ExecuteAccountVerification;
      {$ENDIF}
        cid_AccountTransfer: ExecuteAccountTransfer;
        cid_AccountReceived: ExecuteAccountReceived;
        end;
    except
      // Ignore these exceptions. They are caused by wrong data received from another Client
      end;
    end;
  procedure Execute_DataReceived;
    begin
    if FNotSilent then
      if Filter(FOnDataFilter,Sender) then
        if Call(FConDataReceived,FGuiDataReceived,Sender) then
          if assigned(FThread) then
            FThread.Post_DataReceived(Sender);
    end;

  begin
  EnterEvent(Sender);
  try
    if FAmSilent then Exit;

    case FData.Command of
      gc_SendAll:
        begin
        FData.ToBuffer:=False;
        FData.F.Header:=True;
        FData.F.Footer:=True;
        FData.F.Position:=0;

        FData.F.CallID:=Bytes2Word(FData.FBytes, FData.F.Location);
        if FData.F.Length>2 then
          FData.FContent:=Copy(FData.FBytes, FData.F.Location+2, FData.F.Length-2)
        else
          SetLength(FData.FContent,0);

        if FData.CallID<=cid_HighestCustom then
          begin
          if FNotSilent then
            begin
            if assigned(FAccounts) then
              FAccounts.Call_OnDataReceived(Sender);
            FGateClientLinks.DoDataReceived(Sender);
            Execute_DataReceived;
            end;
          end
        else if FData.F.Length<=MAX_INTERNAL_CALL_SIZE then
          ProcessInternalCall;

        SetLength(FData.FContent,0);
        end;

      gc_SendFirst:
        begin
        FData.ToBuffer:=False;
        FData.F.Header:=True;
        FData.F.Footer:=False;
        FData.F.Position:=0;

        FData.F.CallID:=Bytes2Word(FData.FBytes, FData.F.Location);
        if FData.F.Length>2 then
          FData.FContent:=Copy(FData.FBytes, FData.F.Location+2, FData.F.Length-2)
        else
          SetLength(FData.FContent,0);

        if FData.CallID<=cid_HighestCustom then
          begin
          if FNotSilent then
            begin
            if assigned(FAccounts) then
              FAccounts.Call_OnDataReceived(Sender);
            FGateClientLinks.DoDataReceived(Sender);
            Execute_DataReceived;
            end;
          end
        else if FData.F.Length<=MAX_INTERNAL_CALL_SIZE then
          FData.ToBuffer:=True;

        SetLength(FData.FContent,0);

        if FData.ToBuffer then
          begin
          CS.Acquire;
          try
            Buff:=TRtcHugeByteArrayWithCallID(FBuffRcv.search((FData.UserID shl UserIDShift) or FData.GroupID));
            if assigned(Buff) then
              Buff.Clear
            else
              begin
              Buff:=TRtcHugeByteArrayWithCallID.Create;
              FBuffRcv.insert((FData.UserID shl UserIDShift) or FData.GroupID,Buff);
              end;
            Buff.CallID:=FData.CallID;
            if FData.F.Length>2 then
              Buff.AddEx(FData.FBytes, FData.F.Length-2, FData.F.Location+2);
          finally
            CS.Release;
            end;
          end;
        end;

      gc_SendMore:
        begin
        inBuff:=False;

        CS.Acquire;
        try
          Buff:=TRtcHugeByteArrayWithCallID(FBuffRcv.search((FData.UserID shl UserIDShift) or FData.GroupID));
          if assigned(Buff) then
            begin
            FData.ToBuffer:=True;
            FData.F.Header:=False;
            FData.F.Footer:=False;
            FData.F.Position:=Buff.Size;
            FData.F.CallID:=Buff.CallID;
            FData.FContent:=Copy(FData.FBytes, FData.F.Location, FData.F.Length);

            Buff.AddEx(FData.FBytes, FData.F.Length, FData.F.Location);
            inBuff:=True;

            if FData.CallID>cid_HighestCustom then
              if Buff.Size>MAX_INTERNAL_CALL_SIZE then
                FData.ToBuffer:=False; // invalid internal command (too big), need to clear buffers
            end
          else
            begin
            FData.ToBuffer:=False;
            FData.F.Header:=False;
            FData.F.Footer:=False;
            FData.F.Position:=0;
            FData.F.CallID:=0;
            FData.FContent:=Copy(FData.FBytes, FData.F.Location, FData.F.Length);
            end;
        finally
          CS.Release;
          end;

        if FData.CallID<=cid_HighestCustom then
          begin
          if FNotSilent then
            begin
            if assigned(FAccounts) then
              FAccounts.Call_OnDataReceived(Sender);
            FGateClientLinks.DoDataReceived(Sender);
            Execute_DataReceived;
            end;
          end;

        if inBuff and not FData.ToBuffer then
          begin // ToBuffer changed to FALSE? Stop buffering!
          CS.Acquire;
          try
            Buff:=TRtcHugeByteArrayWithCallID(FBuffRcv.search((FData.UserID shl UserIDShift) or FData.GroupID));
            if assigned(Buff) then
              begin
              FBuffRcv.remove((FData.UserID shl UserIDShift) or FData.GroupID);
              RtcFreeAndNil(Buff);
              end;
          finally
            CS.Release;
            end;
          end;

        SetLength(FData.FContent,0);
        end;

      gc_SendLast:
        begin
        CS.Acquire;
        try
          Buff:=TRtcHugeByteArrayWithCallID(FBuffRcv.search((FData.UserID shl UserIDShift) or FData.GroupID));
          if assigned(Buff) then
            begin
            FData.F.Position:=Buff.Size;

            Buff.AddEx(FData.FBytes, FData.F.Length, FData.F.Location);
            FBuffRcv.remove((FData.UserID shl UserIDShift) or FData.GroupID);

            FData.ToBuffer:=True;
            FData.F.Header:=True;
            FData.F.Footer:=True;
            FData.F.CallID:=Buff.CallID;
            FData.FContent:=Buff.GetEx;

            RtcFreeAndNil(Buff);
            end
          else
            begin
            FData.ToBuffer:=False;
            FData.F.Header:=False;
            FData.F.Footer:=True;
            FData.F.Position:=0;
            FData.F.CallID:=0;
            FData.FContent:=Copy(FData.FBytes, FData.F.Location, FData.F.Length);
            end;
        finally
          CS.Release;
          end;

        if FData.CallID<=cid_HighestCustom then
          begin
          if FNotSilent then
            begin
            if assigned(FAccounts) then
              FAccounts.Call_OnDataReceived(Sender);
            FGateClientLinks.DoDataReceived(Sender);
            Execute_DataReceived;
            end;
          end
        else if length(FData.FContent)<=MAX_INTERNAL_CALL_SIZE then
          ProcessInternalCall;

        SetLength(FData.FContent,0);
        end;
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

type
  // @exclude
  TRtcGateUserInfo=class(TObject)
  public
    accs:tStrIntList;
    info:RtcString;
    constructor Create;
    destructor Destroy; override;
  end;

function TRtcHttpGateClient.GetUserAllowed(const UserID:TGateUID; const action:word):boolean;
  var
    o:TObject;
    usr:TRtcGateUserInfo absolute o;
    acc:RtcString;
    stat:longint;
  begin
  Result:=False;
  if assigned(FAccounts) then
    begin
    FAccounts.GateCS.Acquire;
    try
      o:=FUsers.search(UserID);
      if assigned(o) then
        begin
        acc:=usr.accs.search_min(stat);
        while (acc<>'') and (stat>0) do
          begin
          if stat>=5 then // Public or verified account
            if FAccounts.isAllowed[acc,action] then
              begin
              Result:=True;
              Break;
              end;
          acc:=usr.accs.search_g(acc,stat);
          end;
        end;
    finally
      FAccounts.GateCS.Release;
      end;
    end;
  end;

function TRtcHttpGateClient.GetVerifiedAllowed(const UserID:TGateUID; const action:word):boolean;
  var
    o:TObject;
    usr:TRtcGateUserInfo absolute o;
    acc:RtcString;
    stat:longint;
  begin
  Result:=False;
  if assigned(FAccounts) then
    begin
    FAccounts.GateCS.Acquire;
    try
      o:=FUsers.search(UserID);
      if assigned(o) then
        begin
        acc:=usr.accs.search_min(stat);
        while (acc<>'') and (stat>0) do
          begin
          if stat=10 then // Verified account
            if FAccounts.isAllowed[acc,action] then
              begin
              Result:=True;
              Break;
              end;
          acc:=usr.accs.search_g(acc,stat);
          end;
        end;
    finally
      FAccounts.GateCS.Release;
      end;
    end;
  end;

function TRtcHttpGateClient.GetMaybeAllowed(const UserID:TGateUID; const action:word):boolean;
  var
    o:TObject;
    usr:TRtcGateUserInfo absolute o;
    acc:RtcString;
    stat:longint;
  begin
  Result:=False;
  if assigned(FAccounts) then
    begin
    FAccounts.GateCS.Acquire;
    try
      o:=FUsers.search(UserID);
      if assigned(o) then
        begin
        acc:=usr.accs.search_min(stat);
        while (acc<>'') and (stat>0) do
          begin
          if stat>1 then // Verified and unverified accounts
            if FAccounts.isAllowed[acc,action] then
              begin
              Result:=True;
              Break;
              end;
          acc:=usr.accs.search_g(acc,stat);
          end;
        end;
    finally
      FAccounts.GateCS.Release;
      end;
    end;
  end;

function TRtcHttpGateClient.CheckMaybeInAccount(const UserID:TGateUID; const AccountID:RtcString):boolean;
  procedure RunCheck;
    var
      o:TObject;
      usr:TRtcGateUserInfo absolute o;
    begin
    o:=FUsers.search(UserID);
    if assigned(o) then
      Result:=usr.accs.search(AccountID)>1;
    end;
  begin
  Result:=False;
  if assigned(FAccounts) then
    begin
    FAccounts.GateCS.Acquire;
    try
      RunCheck;
    finally
      FAccounts.GateCS.Release;
      end;
    end
  else
    begin
    CS.Acquire;
    try
      RunCheck;
    finally
      CS.Release;
      end;
    end;
  end;

function TRtcHttpGateClient.CheckNotVerifiedAccount(const UserID:TGateUID; const AccountID:RtcString):boolean;
  procedure RunCheck;
    var
      o:TObject;
      usr:TRtcGateUserInfo absolute o;
      stat:integer;
    begin
    o:=FUsers.search(UserID);
    if assigned(o) then
      begin
      stat:=usr.accs.search(AccountID);
      Result:=(stat>0) and (stat<5);
      end;
    end;
  begin
  Result:=False;
  if assigned(FAccounts) then
    begin
    FAccounts.GateCS.Acquire;
    try
      RunCheck;
    finally
      FAccounts.GateCS.Release;
      end;
    end
  else
    begin
    CS.Acquire;
    try
      RunCheck;
    finally
      CS.Release;
      end;
    end;
  end;

function TRtcHttpGateClient.CheckUserInAccount(const UserID:TGateUID; const AccountID:RtcString):boolean;
  procedure RunCheck;
    var
      o:TObject;
      usr:TRtcGateUserInfo absolute o;
    begin
    o:=FUsers.search(UserID);
    if assigned(o) then
      Result:=usr.accs.search(AccountID)>=5;
    end;
  begin
  Result:=False;
  if assigned(FAccounts) then
    begin
    FAccounts.GateCS.Acquire;
    try
      RunCheck;
    finally
      FAccounts.GateCS.Release;
      end;
    end
  else
    begin
    CS.Acquire;
    try
      RunCheck;
    finally
      CS.Release;
      end;
    end;
  end;

function TRtcHttpGateClient.CheckUserInfo(const UserID:TGateUID):RtcString;
  procedure RunCheck;
    var
      o:TObject;
      usr:TRtcGateUserInfo absolute o;
    begin
    o:=FUsers.search(UserID);
    if assigned(o) then
      Result:=usr.info;
    end;
  begin
  Result:='';
  if assigned(FAccounts) then
    begin
    FAccounts.GateCS.Acquire;
    try
      RunCheck;
    finally
      FAccounts.GateCS.Release;
      end;
    end
  else
    begin
    CS.Acquire;
    try
      RunCheck;
    finally
      CS.Release;
      end;
    end;
  end;

function TRtcHttpGateClient.CheckUserInVerifiedAccount(const UserID:TGateUID; const AccountID:RtcString):boolean;
  procedure RunCheck;
    var
      o:TObject;
      usr:TRtcGateUserInfo absolute o;
    begin
    o:=FUsers.search(UserID);
    if assigned(o) then
      Result:=usr.accs.search(AccountID)=10;
    end;
  begin
  Result:=False;
  if assigned(FAccounts) then
    begin
    FAccounts.GateCS.Acquire;
    try
      RunCheck;
    finally
      FAccounts.GateCS.Release;
      end;
    end
  else
    begin
    CS.Acquire;
    try
      RunCheck;
    finally
      CS.Release;
      end;
    end;
  end;

{$IFDEF RTC_RSA}
function TRtcHttpGateClient.RequestUserVerifications(const UserID:TGateUID):boolean;
  var
    buff:TRtcHugeByteArray;
    pk:RtcString;
    gid:TGateUID;
    AccountID:RtcString;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if assigned(FAccounts) and Ready then
    begin
    AccountID:=FAccounts.FirstID;
    while (AccountID<>'') do
      begin
      if FAccounts.isType[AccountID]=gat_Linked then
        if CheckNotVerifiedAccount(UserID,AccountID) then
          begin
          gid:=FAccounts.FindGroupID(AccountID);
          if gid>0 then
            begin
            buff:=TRtcHugeByteArray.Create;
            try
              pk:=RtcBytesToString(FAccounts.PublicKey);
              buff.Add(MD5Code8bit(AccountID));
              buff.AddEx(FAccounts.PrivateEncrypt(AccountID,RtcStringToBytes(MD5Code8Bit(pk+AccountID)+AccountID+pk))); // our Public Key encrypted with users Public Key
              if SendBytes(UserID,gid,cid_VerifyAccount,buff.GetEx) then
                Result:=True
              else
                Break;
            finally
              RtcFreeAndNil(buff);
              end;
            end;
          end;
      AccountID:=FAccounts.NextID(AccountID);
      end;
    end;
  end;
{$ELSE}
function TRtcHttpGateClient.RequestUserVerifications(const UserID:TGateUID):boolean;
  begin
  Result:=False;
  end;
{$ENDIF}

{$IFDEF RTC_RSA}
function TRtcHttpGateClient.CheckNotVerifiedUser(const UserID:TGateUID):boolean;
  var
    gid:TGateUID;
    AccountID:RtcString;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if assigned(FAccounts) then
    begin
    AccountID:=FAccounts.FirstID;
    while AccountID<>'' do
      begin
      if FAccounts.isType[AccountID]=gat_Linked then
        if CheckNotVerifiedAccount(UserID,AccountID) then
          begin
          gid:=FAccounts.FindGroupID(AccountID);
          if gid>0 then
            begin
            Result:=True;
            Break;
            end;
          end;
      AccountID:=FAccounts.NextID(AccountID);
      end;
    end;
  end;
{$ELSE}
function TRtcHttpGateClient.CheckNotVerifiedUser(const UserID:TGateUID):boolean;
  begin
  Result:=False;
  end;
{$ENDIF}

{$IFDEF RTC_RSA}
function TRtcHttpGateClient.RequestAccountVerification(const UserID:TGateUID; const AccountID:RtcString):boolean;
  var
    buff:TRtcHugeByteArray;
    pk:RtcString;
    gid:TGateUID;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if assigned(FAccounts) and Ready then
    if FAccounts.isType[AccountID]=gat_Linked then
      if CheckNotVerifiedAccount(UserID,AccountID) then
        begin
        gid:=FAccounts.FindGroupID(AccountID);
        if gid>0 then
          begin
          buff:=TRtcHugeByteArray.Create;
          try
            pk:=RtcBytesToString(FAccounts.PublicKey);
            buff.Add(MD5Code8bit(AccountID));
            buff.AddEx(FAccounts.PrivateEncrypt(AccountID,RtcStringToBytes(MD5Code8Bit(pk+AccountID)+AccountID+pk))); // our Public Key encrypted with users Public Key
            Result:=SendBytes(UserID,gid,cid_VerifyAccount,buff.GetEx);
          finally
            RtcFreeAndNil(buff);
            end;
          end;
        end;
  end;
{$ELSE}
function TRtcHttpGateClient.RequestAccountVerification(const UserID:TGateUID; const AccountID:RtcString):boolean;
  begin
  Result:=False;
  end;
{$ENDIF}

function TRtcHttpGateClient.SendAccountData(const UserID:TGateUID; const ReceiverAuthCode:RtcString; const AccountData:RtcByteArray):boolean;
  var
    data:RtcString;
  begin
  Result:=False;
  SetLength(data,0);
  if FAmSilent then Exit;

  if Ready then
    begin
    data:=RtcBytesToString(AccountData)+MD5Code8bit(ReceiverAuthCode);
    Crypt(data,MD5Code4Bit(ReceiverAuthCode));
    Result:=SendBytes(UserID,0,cid_AccountTransfer,data);
    end;
  end;

function TRtcHttpGateClient.AddUserToAccount(const uid,gid:TGateUID; const acc,uinfo:RtcString; verified:boolean):boolean;
  var
    o:TObject;
    usr:TRtcGateUserInfo absolute o;
    priv,toRequest:boolean;
  begin
  Result:=False;
  if FAmSilent then Exit;

  if assigned(FAccounts) and (acc<>'') then
    begin
    toRequest:=False;
    FAccounts.GateCS.Acquire;
    try
      priv:=FAccounts.isType[acc]<>gat_Public;
      o:=FUsers.search(uid);
      if o=nil then
        begin
        usr:=TRtcGateUserInfo.Create;
        FUsers.insert(uid,usr);
        end;
      if uinfo<>'' then
        usr.info:=uinfo;
      if usr.accs.search(acc)>0 then // account already added
        begin
        if verified then
          usr.accs.change(acc,10) // verified
        else if priv then
          usr.accs.change(acc,3) // requires verification
        else
          usr.accs.change(acc,5); // verification not required
        end
      else
        begin
        if verified then
          usr.accs.insert(acc,10) // verified
        else if priv then
          usr.accs.insert(acc,3) // requires verification
        else
          usr.accs.insert(acc,5); // verification not required
        end;
      FData.FAccountID:=acc;
      Result:=True;
      if (verified=False) and (gid>0) and (FAccounts.isType[acc]=gat_Linked) then
        if FAccounts.AutoVerifyAccounts then
          toRequest:=True;
    finally
      FAccounts.GateCS.Release;
      end;
    if toRequest then
      RequestAccountVerification(uid,acc);
    end;
  end;

function TRtcHttpGateClient.RemoveUserFromAccount(const uid:TGateUID; const acc:RtcString):boolean;
  procedure RunCheck;
    var
      o:TObject;
      usr:TRtcGateUserInfo absolute o;
    begin
    FData.FAccountID:=acc;
    o:=FUsers.search(uid);
    if assigned(o) then
      if usr.accs.search(acc)>0 then
        begin
        usr.accs.remove(acc);
        Result:=True;
        end;
    end;
  begin
  Result:=False;
  if acc<>'' then
    if assigned(FAccounts) then
      begin
      FAccounts.GateCS.Acquire;
      try
        RunCheck;
      finally
        FAccounts.GateCS.Release;
        end;
      end
    else
      begin
      CS.Acquire;
      try
        RunCheck;
      finally
        CS.Release;
        end;
      end;
  end;

function TRtcHttpGateClient.UserAddedToMyGroup(const GroupID,UserID:TGateUID):boolean;
  procedure RunCheck;
    var
      o:TObject;
      usr:tBinList absolute o;
    begin
    o:=FGroups.search(GroupID);
    if not assigned(o) then
      begin
      usr:=tBinList.Create(8);
      usr.insert(UserID,1);
      FGroups.insert(GroupID,usr);
      Result:=True;
      end
    else if usr.search(UserID)<=0 then
      begin
      usr.insert(UserID,1);
      Result:=True;
      end;
    end;
  begin
  Result:=False;
  if (GroupID>0) and (UserID>0) then
    if assigned(FAccounts) then
      begin
      FAccounts.GateCS.Acquire;
      try
        RunCheck;
      finally
        FAccounts.GateCS.Release;
        end;
      end
    else
      begin
      CS.Acquire;
      try
        RunCheck;
      finally
        CS.Release;
        end;
      end;
  end;

function TRtcHttpGateClient.UserRemovedFromMyGroup(const GroupID,UserID:TGateUID):boolean;
  procedure RunCheck;
    var
      o:TObject;
      usr:tBinList absolute o;
    begin
    o:=FGroups.search(GroupID);
    if assigned(o) then
      if usr.search(UserID)>0 then
        begin
        usr.remove(UserID);
        if usr.Count=0 then
          begin
          FGroups.remove(GroupID);
          RtcFreeAndNil(usr);
          end;
        Result:=True;
        end;
    end;
  begin
  Result:=False;
  if (GroupID>0) and (UserID>0) then
    if assigned(FAccounts) then
      begin
      FAccounts.GateCS.Acquire;
      try
        RunCheck;
      finally
        FAccounts.GateCS.Release;
        end;
      end
    else
      begin
      CS.Acquire;
      try
        RunCheck;
      finally
        CS.Release;
        end;
      end;
  end;

function TRtcHttpGateClient.UsersGroupClosed(const GroupID:TGateUID):boolean;
  procedure RunCheck;
    var
      o:TObject;
      usr:tBinList absolute o;
    begin
    o:=FGroups.search(GroupID);
    if assigned(o) then
      begin
      FGroups.remove(GroupID);
      RtcFreeAndNil(usr);
      Result:=True;
      end;
    end;
  begin
  Result:=False;
  if (GroupID>0) then
    if assigned(FAccounts) then
      begin
      FAccounts.GateCS.Acquire;
      try
        RunCheck;
      finally
        FAccounts.GateCS.Release;
        end;
      end
    else
      begin
      CS.Acquire;
      try
        RunCheck;
      finally
        CS.Release;
        end;
      end;
  end;

function TRtcHttpGateClient.IsUserInMyGroup(const GroupID,UserID:TGateUID):boolean;
  procedure RunCheck;
    var
      o:TObject;
      usr:tBinList absolute o;
    begin
    o:=FGroups.search(GroupID);
    if assigned(o) then
      Result:=usr.search(UserID)>0;
    end;
  begin
  Result:=False;
  if (GroupID>0) then
    if assigned(FAccounts) then
      begin
      FAccounts.GateCS.Acquire;
      try
        RunCheck;
      finally
        FAccounts.GateCS.Release;
        end;
      end
    else
      begin
      CS.Acquire;
      try
        RunCheck;
      finally
        CS.Release;
        end;
      end;
  end;

function TRtcHttpGateClient.UsersInMyGroup(const GroupID:TGateUID):integer;
  procedure RunCheck;
    var
      o:TObject;
      usr:tBinList absolute o;
    begin
    o:=FGroups.search(GroupID);
    if assigned(o) then
      Result:=usr.Count;
    end;
  begin
  Result:=0;
  if (GroupID>0) then
    if assigned(FAccounts) then
      begin
      FAccounts.GateCS.Acquire;
      try
        RunCheck;
      finally
        FAccounts.GateCS.Release;
        end;
      end
    else
      begin
      CS.Acquire;
      try
        RunCheck;
      finally
        CS.Release;
        end;
      end;
  end;

function TRtcHttpGateClient.CleanUpUserInfo(const uid:TGateUID):boolean;
  procedure RunCheck;
    var
      o:TObject;
      usr:TRtcGateUserInfo absolute o;
    begin
    o:=FUsers.search(uid);
    if assigned(o) then
      if usr.accs.Count=0 then
        begin
        FUsers.remove(uid);
        RtcFreeAndNil(usr);
        Result:=True;
        end;
    end;
  begin
  Result:=False;
  if (uid>0) then
    if assigned(FAccounts) then
      begin
      FAccounts.GateCS.Acquire;
      try
        RunCheck;
      finally
        FAccounts.GateCS.Release;
        end;
      end
    else
      begin
      CS.Acquire;
      try
        RunCheck;
      finally
        CS.Release;
        end;
      end;
  end;

function TRtcHttpGateClient.RemoveAccountVerification(const uid:TGateUID; const acc:RtcString):boolean;
  procedure RunCheck;
    var
      o:TObject;
      usr:TRtcGateUserInfo absolute o;
    begin
    FData.FAccountID:=acc;
    o:=FUsers.search(uid);
    if assigned(o) then
      if usr.accs.search(acc)>0 then
        begin
        usr.accs.change(acc,1);
        Result:=True;
        end;
    end;
  begin
  Result:=False;
  if (acc<>'') then
    if assigned(FAccounts) then
      begin
      FAccounts.GateCS.Acquire;
      try
        RunCheck;
      finally
        FAccounts.GateCS.Release;
        end;
      end
    else
      begin
      CS.Acquire;
      try
        RunCheck;
      finally
        CS.Release;
        end;
      end;
  end;

procedure TRtcHttpGateClient.Call_OnInfoReceived(Sender: TRtcConnection);
  procedure CleanUpBufferLeftovers;
    var
      Buff:TRtcHugeByteArrayWithCallID;
    begin
    // Clean up bufffer
    CS.Acquire;
    try
      Buff:=TRtcHugeByteArrayWithCallID(FBuffRcv.search((FData.UserID shl UserIDShift) or FData.GroupID));
      if assigned(Buff) then
        begin
        FBuffRcv.remove((FData.UserID shl UserIDShift) or FData.GroupID);
        RtcFreeAndNil(Buff);
        end;
    finally
      CS.Release;
      end;
    end;
  begin
  EnterEvent(Sender);
  try
    if FAmSilent then Exit;

    if assigned(FAccounts) then
      begin
      if FData.GroupID>0 then
        case FData.Command of
          gc_SubscribeLogIn:
            if AddUserToAccount(FData.UserID, FData.GroupID, FAccounts.FindAccountID(FData.GroupID), FData.UserInfo, False) then
              FData.F.Command:=gc_AccountLogIn;
          gc_SubscribeLogOut:
            if RemoveUserFromAccount(FData.UserID, FAccounts.FindAccountID(FData.GroupID)) then
              FData.F.Command:=gc_AccountLogOut;
          gc_UserLeft,
          gc_UserOffline:
            UserRemovedFromMyGroup(FData.GroupID, FData.UserID);
          end;
      FAccounts.Call_OnInfoReceived(Sender);
      end;
    FGateClientLinks.DoInfoReceived(Sender);
    if Filter(FOnInfoFilter,Sender) then
      if Call(FConInfoReceived,FGuiInfoReceived,Sender) then
        if assigned(FThread) then
          FThread.Post_InfoReceived(Sender);

    if FData.Command=gc_AccountLogOut then
      CleanUpUserInfo(FData.UserID);
    if FData.Command=gc_LeftUser then
      CleanUpBufferLeftovers;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.Call_OnReadyToSend(Sender: TRtcConnection);
  var
    notify:boolean;
  begin
  EnterEvent(Sender);
  try
    if FAmSilent then Exit;

    CS.Acquire;
    try
      if not (State.F.ReadyToSend or FDataWaiting) then
        begin
        State.F.ReadyToSend:=True;
        FNeedOutput:=False;
        notify:=True;
        end
      else
        begin
        FNeedOutput:=FDataWaiting;
        notify:=False;
        end;
    finally
      CS.Release;
      end;
    if notify then
      begin
      if FWasReset then
        Call_OnReadyAfterReset(Sender);

      if assigned(FAccounts) then
        FAccounts.Call_OnReadyToSend(Sender);
      FGateClientLinks.DoReadyToSend(Sender);

      if Call(FConReadyToSend,FGuiReadyToSend,Sender) then
        if assigned(FThread) then
          FThread.Post_ReadyToSend(Sender);
      end;
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.Call_OnReadyAfterReset(Sender: TRtcConnection);
  begin
  EnterEvent(Sender);
  try
    if not FWasReset then Exit;
    FWasReset:=False;
    if assigned(FAccounts) then
      begin
      FAccounts.RefreshClient(self);
      if assigned(FAccounts) then
        FAccounts.Call_OnReadyAfterReset(Sender);
      end;
    FGateClientLinks.DoReadyAfterReset(Sender);

    if Call(FConReadyAfterReset,FGuiReadyAfterReset,Sender) then
      if assigned(FThread) then
        FThread.Post_ReadyAfterReset(Sender);
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.Call_OnStreamReset(Sender: TRtcConnection);
  begin
  EnterEvent(Sender);
  try
    if FAmSilent then Exit;
    FWasReset:=True;
    if assigned(FAccounts) then
      FAccounts.Call_OnStreamReset(Sender);
    FGateClientLinks.DoStreamReset(Sender);

    if Call(FConStreamReset,FGuiStreamReset,Sender) then
      if assigned(FThread) then
        FThread.Post_StreamReset(Sender);
  finally
    LeaveEvent(Sender);
    end;
  end;

procedure TRtcHttpGateClient.SetGateFileName(const Value: RtcString);
  begin
  if Value<>FGateFileName then
    begin
    FGateFileName := Value;
    if Copy(FGateFileName,1,1)<>'/' then
      FGateFileName:='/'+FGateFileName;

    FGATEURI_PING:=FGateFileName+GATEURI_PING;
    FGATEURI_LOGIN:=FGateFileName+GATEURI_LOGIN;
    FGATEURI_INPUT:=FGateFileName+GATEURI_INPUT;
    FGATEURI_OUTPUT:=FGateFileName+GATEURI_OUTPUT;
    FGATEURI_INPUTRESET:=FGateFileName+GATEURI_INPUTRESET;
    FGATEURI_OUTPUTRESET:=FGateFileName+GATEURI_OUTPUTRESET;
    end;
  end;

procedure TRtcHttpGateClient.SetGatePrimaryKey(const Value: RtcString);
  begin
  FGATE_PRIMARY_KEY := Value;
  end;

procedure TRtcHttpGateClient.SetGateSecondaryKey(const Value: RtcString);
  begin
  FGATE_SECONDARY_KEY := Value;
  end;

procedure TRtcHttpGateClient.SetGateUserAuth(const Value: RtcString);
  begin
  FGATE_USERAUTH := Value;
  end;

procedure TRtcHttpGateClient.SetGateUserInfo(const Value: RtcString);
  begin
  FGATE_USERINFO := Value;
  end;

procedure TRtcHttpGateClient.UserStreamSoftReset;
  begin
  if FAmSilent then Exit;

  if assigned(ClientIN) and ClientIN.inThread then
    UserStreamLost(ClientIN)
  else if assigned(ClientOUT) and ClientOUT.inThread then
    UserStreamLost(ClientOUT)
  else
    UserStreamLost(nil);
  end;

procedure TRtcHttpGateClient.UserStreamHardReset;
  begin
  if FAmSilent then Exit;

  if assigned(ClientIN) and ClientIN.inThread then
    User_LogOut(ClientIN)
  else if assigned(ClientOUT) and ClientOUT.inThread then
    User_LogOut(ClientOUT)
  else
    User_LogOut(nil);
  end;

procedure TRtcHttpGateClient.ResetStreams(HardReset:boolean=False);
  begin
  if FAmSilent then Exit;

  if HardReset then
    UserStreamHardReset
  else
    UserStreamSoftReset;
  end;

procedure TRtcHttpGateClient.AddGateClientLink(Value: TRtcAbsGateClientLink);
  begin
  if (self=nil) or not assigned(FGateClientLinks) then Exit;
  FGateClientLinks.Add(Value);
  end;

procedure TRtcHttpGateClient.RemoveAllGateClientLinks;
  var
    Link:TRtcAbsGateClientLink;
  begin
  if (self=nil) or not assigned(FGateClientLinks) then Exit;
  Link:=FGateClientLinks.LockFirst;
  while assigned(Link) do
    begin
    Link.RemoveClient(self);
    if not assigned(FGateClientLinks) then Exit;
    FGateClientLinks.LockNext(Link);
    end;
  end;

procedure TRtcHttpGateClient.RemoveGateClientLink(Value: TRtcAbsGateClientLink);
  begin
  if (self=nil) or not assigned(FGateClientLinks) then Exit;
  FGateClientLinks.Remove(Value);
  end;

procedure TRtcHttpGateClient.SetCryptPlugin(const Value: TRtcCryptPlugin);
  begin
  FCryptPlugin := Value;
  end;

procedure TRtcHttpGateClient.SetUserLogin(const Value: TRtcUserLoginInfo);
  begin
  if Value<>FUserLogin then
  	FUserLogin.Assign(Value);
  end;

function TRtcHttpGateClient.GetAutoRelogin: boolean;
  begin
  Result:=FAutoRelogin;
  end;

procedure TRtcHttpGateClient.SetAutoRelogin(const Value: boolean);
  begin
  if Value<>FAutoRelogin then
    begin
    FAutoRelogin:=Value;
    Active:=FAutoRelogin and not FAmSilent;
    end;
  end;

procedure TRtcHttpGateClient.SetAccounts(const Value: TRtcGateAccountManager);
  begin
  if FAccounts<>Value then
    begin
    if assigned(FAccounts) then
      FAccounts.UnregisterClient(self);
    FAccounts:=Value;
    if assigned(FAccounts) then
      begin
      FAccounts.RegisterClient(self);
      if Ready then
        FAccounts.RefreshClient(self);
      end;
    end;
  end;

procedure TRtcHttpGateClient.Call_AccountChange(const id: RtcString; action:TRtcGateAccountAction);
  begin
  if (self=nil) or FAmSilent or (FAccounts=nil) or not Ready then Exit;

  case action of
    gaa_Registered,
    gaa_Activated:
      begin
      Subscribe(id,FAccounts.FindGroupID(id));
      end;
    gaa_Deleted,
    gaa_Deactivated:
      begin
      UnSubscribe(id);
      end;
    end;
  end;

procedure TRtcHttpGateClient.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FAccounts then
      FAccounts:=nil
    else if AComponent=FThread then
      FThread:=nil;
  end;

function TRtcHttpGateClient.GetMyAddr: RtcString;
  begin
  if (FGateAddr='') or (FGatePort='') then
    Result:=''
  else if (FMyUID=0) then
    begin
    if (FGatePort='80') then
      Result:=FGateAddr
    else
      Result:=FGateAddr+':'+FGatePort;
    end
  else
    begin
    if (FGatePort='80') then
      Result:=Int2Str(FMyUID)+'@'+FGateAddr
    else
      Result:=Int2Str(FMyUID)+'@'+FGateAddr+':'+FGatePort;
    end;
  end;

function TRtcHttpGateClient.GetSharedGroups: TRtcGateUserGroupIDs;
  begin
  if FAccounts=nil then
    Result:=FSharedGroups
  else
    Result:=FAccounts.SharedGroups;
  end;

function TRtcHttpGateClient.AddUserToMyGroup(const GroupID, UserID: TGateUID): boolean;
  begin
  Result:=AddUserToGroup(MyUID,GroupID,UserID,True);
  end;

function TRtcHttpGateClient.RemoveUserFromMyGroup(const GroupID, UserID: TGateUID): boolean;
  begin
  if UserRemovedFromMyGroup(GroupID,UserID) then
    Result:=RemUserFromGroup(MyUID,GroupID,UserID)
  else
    Result:=False;
  end;

function TRtcHttpGateClient.LeaveUsersGroup(const OwnerID, GroupID: TGateUID): boolean;
  begin
  Result:=RemUserFromGroup(OwnerID,GroupID,MyUID);
  end;

function TRtcHttpGateClient.DisbandMyGroup(const GroupID: TGateUID): boolean;
  begin
  if UsersGroupClosed(GroupID) then
    Result:=RemoveGroup(MyUID,GroupID)
  else
    Result:=False;
  end;

function TRtcHttpGateClient.SendToMyGroup(const GroupID: TGateUID; const CallID: word): boolean;
  begin
  if UsersInMyGroup(GroupID)>0 then
    Result:=SendBytes(MyUID,GroupID,CallID)
  else
    Result:=False;
  end;

function TRtcHttpGateClient.SendToMyGroup(const GroupID: TGateUID; const CallID: word; const data: RtcString): boolean;
  begin
  if UsersInMyGroup(GroupID)>0 then
    Result:=SendBytes(MyUID,GroupID,CallID,data)
  else
    Result:=False;
  end;

function TRtcHttpGateClient.SendToMyGroup(const GroupID: TGateUID; const CallID: word; const data: RtcByteArray): boolean;
  begin
  if UsersInMyGroup(GroupID)>0 then
    Result:=SendBytes(MyUID,GroupID,CallID,data)
  else
    Result:=False;
  end;

{ TRtcGateUserGroupStates }

constructor TRtcGateUserGroupStates.Create;
  begin
  inherited;
  FGroups:=tBinList.Create(16);
  FCS:=TRtcCritSec.Create;
  end;

destructor TRtcGateUserGroupStates.Destroy;
  begin
  RtcFreeAndNil(FGroups);
  RtcFreeAndNil(FCS);
  inherited;
  end;

procedure TRtcGateUserGroupStates.ClearAllStates;
  begin
  FCS.Acquire;
  try
    FGroups.removeall;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcGateUserGroupStates.ClearStatus(UserID, GroupID: TGateUID);
  begin
  FCS.Acquire;
  try
    if FGroups.search((UserID shl UserIDShift) or GroupID)>0 then
      FGroups.remove((UserID shl UserIDShift) or GroupID);
  finally
    FCS.Release;
    end;
  end;

function TRtcGateUserGroupStates.GetMaxStatus(UserID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  Result:=0;
  FCS.Acquire;
  try
    UID:=FGroups.search_ge(UserID shl UserIDShift, GID);
    while (GID>0) and (UID>0) and (UserID = UID shr UserIDShift) do
      begin
      if GID>Result then
        Result:=GID;
      UID:=FGroups.search_g(UID, GID);
      end;
  finally
    FCS.Release;
    end;
  end;

function TRtcGateUserGroupStates.GetMinID(UserID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  FCS.Acquire;
  try
    UID:=FGroups.search_ge(UserID shl UserIDShift, GID);
    if (UID>0) and (UserID = UID shr UserIDShift) then
      Result:=UID - (UserID shl UserIDShift)
    else
      Result:=0;
  finally
    FCS.Release;
    end;
  end;

function TRtcGateUserGroupStates.GetMinStatus(UserID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  Result:=0;
  FCS.Acquire;
  try
    UID:=FGroups.search_ge(UserID shl UserIDShift, GID);
    while (GID>0) and (UID>0) and (UserID = UID shr UserIDShift) do
      begin
      if (Result=0) or (GID<Result) then
        Result:=GID;
      UID:=FGroups.search_g(UID, GID);
      end;
  finally
    FCS.Release;
    end;
  end;

function TRtcGateUserGroupStates.GetNextStatus(var UserID, GroupID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  Result:=0;
  FCS.Acquire;
  try
    UID:=FGroups.search_g((UserID shl UserIDShift) or GroupID, GID);
    if (UID>0) and (GID>0) then
      begin
      UserID:=UID shr UserIDShift;
      GroupID:=UID - (UserID shl UserIDShift);
      Result:=GID;
      end;
  finally
    FCS.Release;
    end;
  end;

function TRtcGateUserGroupStates.GetStatus(UserID, GroupID: TGateUID): TGateUID;
  begin
  FCS.Acquire;
  try
    Result:=FGroups.search((UserID shl UserIDShift) or GroupID);
  finally
    FCS.Release;
    end;
  end;

procedure TRtcGateUserGroupStates.SetStatus(UserID, GroupID, Status: TGateUID);
  begin
  FCS.Acquire;
  try
    if Status>0 then
      begin
      if FGroups.search((UserID shl UserIDShift) or GroupID)<=0 then
        FGroups.insert((UserID shl UserIDShift) or GroupID,Status)
      else
        FGroups.change((UserID shl UserIDShift) or GroupID,Status);
      end
    else
      ClearStatus(UserID,GroupID);
  finally
    FCS.Release;
    end;
  end;

{ TRtcGateUserGroupIDs }

constructor TRtcGateUserGroupIDs.Create;
  begin
  inherited;
  FCS:=TRtcCritSec.Create;
  ReleaseAllIDs;
  end;

destructor TRtcGateUserGroupIDs.Destroy;
  begin
  RtcFreeAndNil(FCS);
  inherited;
  end;

procedure TRtcGateUserGroupIDs.AllocID(GroupID: TGateUID);
  begin
  if (GroupID<1) or (GroupID>MaxGID) then
    raise ERtcGateClient.Create('AllocID: GroupID out of range');
  FCS.Acquire;
  try
    FUsedGroupIDs[GroupID]:=True;
  finally
    FCS.Release;
    end;
  end;

function TRtcGateUserGroupIDs.AllocNextID(from:TGateUID=1): TGateUID;
  begin
  FCS.Acquire;
  try
    Result:=from;
    if Result<1 then
      Result:=1
    else if Result>MaxGID then
      raise ERtcGateClient.Create('AllocNextID: All Group IDs are already in use.');
    while FUsedGroupIDs[Result] do
      if Result<MaxGID then
        Inc(Result)
      else
        raise ERtcGateClient.Create('AllocNextID: All Group IDs are already in use.');
    FUsedGroupIDs[Result]:=True;
  finally
    FCS.Release;
    end;
  end;

function TRtcGateUserGroupIDs.AllocPrevID(from:TGateUID=MaxGID): TGateUID;
  begin
  FCS.Acquire;
  try
    Result:=from;
    if Result>MaxGID then
      Result:=MaxGID
    else if Result<1 then
      raise ERtcGateClient.Create('AllocPrevID: All Group IDs are already in use.');
    while FUsedGroupIDs[Result] do
      if Result>1 then
        Dec(Result)
      else
        raise ERtcGateClient.Create('AllocPrevID: All Group IDs are already in use.');
    FUsedGroupIDs[Result]:=True;
  finally
    FCS.Release;
    end;
  end;

function TRtcGateUserGroupIDs.AllocFreeID(from:TGateUID=1): TGateUID;
  begin
  FCS.Acquire;
  try
    Result:=from;
    if Result<1 then
      Result:=1
    else if Result>MaxGID then
      Result:=MaxGID;
    while FUsedGroupIDs[Result] do
      if Result<MaxGID then
        Inc(Result)
      else
        begin
        Result:=from;
        if Result<1 then
          Result:=1
        else if Result>MaxGID then
          Result:=MaxGID;
        Break;
        end;
    while FUsedGroupIDs[Result] do
      if Result>1 then
        Dec(Result)
      else
        raise ERtcGateClient.Create('AllocFreeID: All Group IDs are already in use.');
    FUsedGroupIDs[Result]:=True;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcGateUserGroupIDs.ReleaseID(GroupID: TGateUID);
  begin
  if (GroupID<1) or (GroupID>MaxGID) then
    raise ERtcGateClient.Create('ReleaseID: GroupID out of range');

  FCS.Acquire;
  try
    FUsedGroupIDs[GroupID]:=False;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcGateUserGroupIDs.ReleaseAllIDs;
  begin
  FCS.Acquire;
  try
    FillChar(FUsedGroupIDs,SizeOf(FUsedGroupIDs),0);
  finally
    FCS.Release;
    end;
  end;

{ TRtcGateClientStateInfo }

procedure TRtcGateClientStateInfo.CopyFrom(const FromState: TRtcGateClientStateInfo);
  begin
  if FromState=nil then Exit;

  // F:=FromState.F;
  Move(FromState.F,F,SizeOf(F));

  FGateAddr:=FromState.FGateAddr;
  FGatePort:=FromState.FGatePort;
  FLastError:=FromState.FLastError;
  end;

function TRtcGateClientStateInfo.GetMyAddr: RtcString;
  begin
  if (GateAddr='') or (GatePort='') then
    Result:=''
  else if (MyUID=0) then
    begin
    if (GatePort='80') then
      Result:=GateAddr
    else
      Result:=GateAddr+':'+GatePort;
    end
  else
    begin
    if (GatePort='80') then
      Result:=Int2Str(MyUID)+'@'+GateAddr
    else
      Result:=Int2Str(MyUID)+'@'+GateAddr+':'+GatePort;
    end;
  end;

constructor TRtcGateClientStateInfo.Create;
  begin
  inherited;
  end;

destructor TRtcGateClientStateInfo.Destroy;
  begin
  inherited;
  end;

type
  // @exclude
  TRtcGateAccountChangeEvent=procedure(const id:RtcString; action:TRtcGateAccountAction);

  // @exclude
  TRtcGateAccount=class(TObject)
  public
    Active:boolean;
    wasActive:boolean;
    GroupID:TGateUID;
    AType:TRtcGateAccountType;
    LocalName,
    RemoteName:RtcWideString;
    Permissions:tBinList;
    haveKey:boolean;
  {$IFDEF RTC_RSA}
    rsa:TRtcRSA;
  {$ENDIF}

    constructor Create(a_type:TRtcGateAccountType);
    destructor Destroy; override;
    end;

{ TRtcGateAccount }

constructor TRtcGateAccount.Create(a_type:TRtcGateAccountType);
  begin
  Active:=False;
  wasActive:=False;
  GroupID:=0;
  AType:=a_type;
  LocalName:='';
  RemoteName:='';
  Permissions:=tBinList.Create(16);
  haveKey:=False;
{$IFDEF RTC_RSA}
  if AType>=gat_Private then
    rsa:=TRtcRSA.Create
  else
    rsa:=nil;
{$ENDIF}
  end;

destructor TRtcGateAccount.Destroy;
  begin
  RtcFreeAndNil(Permissions);
  haveKey:=False;
  LocalName:='';
  RemoteName:='';
  Active:=False;
{$IFDEF RTC_RSA}
  RtcFreeAndNil(rsa);
{$ENDIF}
  inherited;
  end;

{ TRtcGateAccountManager }

constructor TRtcGateAccountManager.Create(AOwner:TComponent);
  var
    i:TGateUID;
  begin
  inherited;
  FMinGroupID:=MinGID;
  FMaxGroupID:=MaxGID;
  FNextGroupID:=MaxGID;
  FUsedGIDs:=0;
  GateCS:=TRtcCritSec.Create;
  FACC:=tStrObjList.Create(16);
  FCli:=tObjList.Create(16);
  for i:=FMinGroupID to FMaxGroupID do
    begin
    FGIDs[i]:='';
    FGIDTime[i]:=0;
    end;
  FPublicID:='';
{$IFDEF RTC_RSA}
  skey:=nil;
  rnd:=TRtcISAAC.Create(True);
{$ENDIF}
  FAuthCode:='';
  FAuthCodeTimeout:=0;

  FSharedGroups:=TRtcGateUserGroupIDs.Create;
  end;

destructor TRtcGateAccountManager.Destroy;
  var
    i:integer;
  begin
  UnregisterAllClients;

  DeleteAll;
  for i:=MinGID to MaxGID do
    begin
    FGIDs[i]:='';
    FGIDTime[i]:=0;
    end;
  FPublicID:='';
  RtcFreeAndNil(FACC);
  RtcFreeAndNil(FCli);
{$IFDEF RTC_RSA}
  RtcFreeAndNil(skey);
  RtcFreeAndNil(rnd);
{$ENDIF}
  RtcFreeAndNil(FSharedGroups);
  RtcFreeAndNil(GateCS);
  inherited;
  end;

function TRtcGateAccountManager.AuthCodeValid: integer;
  var
    i:TAppRunTime;
  begin
  i:=GetAppRunTime;
  Acquire;
  try
    if i<=FAuthCodeTimeout then
      Result:=(FAuthCodeTimeout - i) div RUN_TIMER_PRECISION
    else
      Result:=-1;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.AuthCodeReset;
  var
    i:integer;
  begin
  Acquire;
  try
    FAuthCode:='';
    for i:=1 to GATEACCMAN_AUTHCODE_DIGITS do
      FAuthCode:=FAuthCode+RtcChar(Ord('0')+
                 {$IFDEF RTC_RSA}rnd.{$ENDIF}Random(10));
    FAuthCodeTimeout:=GetAppRunTime + GATEACCMAN_AUTHCODE_VALID;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.AuthCode: RtcString;
  var
    i:TAppRunTime;
  begin
  i:=GetAppRunTime;
  Acquire;
  try
    if i>FAuthCodeTimeout then
      AuthCodeReset;
    Result:=FAuthCode;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.DeleteAll;
  begin
  Acquire;
  try
    while TotalCount>0 do
      DeleteID(FirstID);
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.Acquire;
  begin
  GateCS.Acquire;
  end;

procedure TRtcGateAccountManager.Release;
  begin
  GateCS.Release;
  end;

function TRtcGateAccountManager.TotalCount: integer;
  begin
  Acquire;
  try
    Result:=FACC.Count;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.ActiveCount: integer;
  begin
  Acquire;
  try
    Result:=FUsedGIDs;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.DeleteID(const id: RtcString);
  var
    a:TObject;
    old:TRtcGateAccount absolute a;
    deleted:boolean;
  begin
  deleted:=False;
  Acquire;
  try
    a:=FACC.search(id);
    if assigned(a) then
      begin
      deleted:=old.wasActive or (old.GroupID>0);
      RemGroupID(id,old.GroupID);
      FACC.remove(id);
      RtcFreeAndNil(a);
      end;
  finally
    Release;
    end;
  if deleted then NotifyClients(id,gaa_Deleted);
  end;

function TRtcGateAccountManager.FirstID: RtcString;
  var
    a:TObject;
  begin
  Acquire;
  try
    Result:=FACC.search_min(a);
    if a=nil then Result:='';
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.LastID: RtcString;
  var
    a:TObject;
  begin
  Acquire;
  try
    Result:=FACC.search_max(a);
    if a=nil then Result:='';
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.NextID(const id: RtcString): RtcString;
  var
    a:TObject;
  begin
  Acquire;
  try
    Result:=FACC.search_g(id,a);
    if a=nil then Result:='';
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.PrevID(const id: RtcString): RtcString;
  var
    a:TObject;
  begin
  Acquire;
  try
    Result:=FACC.search_l(id,a);
    if a=nil then Result:='';
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.GetActive(const id: RtcString): boolean;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
  begin
  Acquire;
  try
    Result:=False;
    a:=FACC.search(id);
    if a<>nil then
      Result:=o.Active;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.GetDisplayName(const id: RtcString): RtcWideString;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
  begin
  Acquire;
  try
    a:=FACC.search(id);
    if a=nil then
      Result:=''
    else if o.LocalName=o.RemoteName then
      Result:=o.LocalName
    else if o.LocalName='' then
      Result:=o.RemoteName
    else if o.RemoteName='' then
      Result:=o.LocalName
    else if o.AType=gat_Private then
      Result:=o.RemoteName+' by '+o.LocalName
    else if o.AType=gat_Linked then
      Result:=o.RemoteName+' for '+o.LocalName
    else
      Result:=o.LocalName+' @ '+o.RemoteName;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.GetLocalName(const id: RtcString): RtcWideString;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
  begin
  Acquire;
  try
    a:=FACC.search(id);
    if a=nil then
      Result:=''
    else
      Result:=o.LocalName;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.GetRemoteName(const id: RtcString): RtcWideString;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
  begin
  Acquire;
  try
    a:=FACC.search(id);
    if a=nil then
      Result:=''
    else
      Result:=o.RemoteName;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.GetPermissions(const id: RtcString): TRtcGatePermissions;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
    i:integer;
    p,r:RtcIntPtr;
  begin
  Acquire;
  try
    SetLength(Result,0);
    a:=FACC.search(id);
    if a<>nil then
      if o.Permissions.Count>0 then
        begin
        SetLength(Result, o.Permissions.Count);
        i:=0; p:=o.Permissions.search_min(r);
        while (p>0) and (r>0) do
          begin
          Result[i]:=p;
          p:=o.Permissions.search_g(p,r);
          Inc(i);
          end;
        end;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.GetType(const id: RtcString): TRtcGateAccountType;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
  begin
  Acquire;
  try
    a:=FACC.search(id);
    if a=nil then
      Result:=gat_None
    else
      Result:=o.AType;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.GetAllowed(const id: RtcString; const action: word): boolean;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
  begin
  Acquire;
  try
    Result:=False;
    a:=FACC.search(id);
    if a<>nil then
      Result:=o.Permissions.search(action)>0;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.SetDisplayName(const id: RtcString; const Value: RtcWideString);
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
  begin
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      begin
      o.LocalName:=Value;
      o.RemoteName:=Value;
      end;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.SetLocalName(const id: RtcString;const Value: RtcWideString);
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
  begin
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      o.LocalName:=Value;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.SetRemoteName(const id: RtcString;const Value: RtcWideString);
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
  begin
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      o.RemoteName:=Value;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.SetPermissions(const id: RtcString; const Value: TRtcGatePermissions);
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
    i:integer;
  begin
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      begin
      o.Permissions.Empty;
      if length(Value)>0 then
        for i:=0 to Length(Value)-1 do
          if o.Permissions.search(Value[i])<=0 then
            o.Permissions.insert(Value[i],1);
      end;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.SetAllowed(const id: RtcString; const action: word; const Value: boolean);
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
    have:boolean;
  begin
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      begin
      have:=o.Permissions.search(action)>0;
      if Value<>have then
        begin
        if Value then
          o.Permissions.insert(action,0)
        else
          o.Permissions.remove(action);
        end;
      end;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.SetActive(const id: RtcString; const Value: boolean);
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
    changed,initial:boolean;
  begin
  changed:=False;
  initial:=False;
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      if Value<>o.Active then
        begin
        if Value then
          NewGroupID(id,o.GroupID)
        else
          RemGroupID(id,o.GroupID);

        o.Active:=Value and (o.GroupID>0);

        if o.Active then
          begin
          if not o.wasActive then
            begin
            initial:=True;
            o.wasActive:=True;
            end;
          end;

        changed:= o.Active=Value;
        end;
  finally
    Release;
    end;
  if changed then
    if not Value then
      NotifyClients(id,gaa_Deactivated)
    else if initial then
      NotifyClients(id,gaa_Registered)
    else
      NotifyClients(id,gaa_Activated);
  end;

function TRtcGateAccountManager.FindGroupID(const id: RtcString): TGateUID;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
  begin
  Result:=0;
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      Result:=o.GroupID;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.FindAccountID(groupID: TGateUID): RtcString;
  begin
  Acquire;
  try
    if (groupID>=MinGID) and (GroupID<=MaxGID) then
      Result:=FGIDs[groupID];
  finally
    Release;
    end;
  end;

function SplitUserAddr(const UserAddr:RtcString; var GateAddr,GatePort:RtcString):TGateUID;
  var
    j,k:integer;
  begin
  Result:=0;
  j:=PosEx('@',UserAddr);
  k:=PosEx(':',UserAddr);
  if j>1 then
    Result:=Str2LWord(Copy(UserAddr,1,j-1));
  if j<0 then j:=0;
  if k>j then // have Port nr
    begin
    GateAddr:=Copy(UserAddr,j+1,k-j-1);
    GatePort:=Copy(UserAddr,k+1,length(UserAddr)-k);
    end
  else
    begin
    GateAddr:=Copy(UserAddr,j+1,length(UserAddr)-j);
    GatePort:='80';
    end;
  if GatePort='' then
    GatePort:='80';
  end;

function ExtractUserID(const UserAddr:RtcString):TGateUID;
  var
    j:integer;
  begin
  j:=PosEx('@',UserAddr);
  if j>1 then
    Result:=Str2LWord(Copy(UserAddr,1,j-1))
  else
    Result:=0;
  end;

function TRtcGateAccountManager.FirstClient(AcquireLock:boolean=False):TRtcHttpGateClient;
  var
    i:RtcIntPtr;
    a:TObject;
    c:TRtcHttpGateClient absolute a;
  begin
  Result:=nil;
  Acquire;
  try
    i:=FCli.search_min(a);
    if (i>0) and assigned(a) then
      begin
      Result:=c;
      if AcquireLock then
        Result.EnterEvent(nil);
      end;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.LastClient(AcquireLock:boolean=False):TRtcHttpGateClient;
  var
    i:RtcIntPtr;
    a:TObject;
    c:TRtcHttpGateClient absolute a;
  begin
  Result:=nil;
  Acquire;
  try
    i:=FCli.search_max(a);
    if (i>0) and assigned(a) then
      begin
      Result:=c;
      if AcquireLock then
        Result.EnterEvent(nil);
      end;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.NextClient(const cli:TRtcHttpGateClient; AcquireLock:boolean=False):TRtcHttpGateClient;
  var
    i:RtcIntPtr;
    a:TObject;
    c:TRtcHttpGateClient absolute a;
  begin
  Result:=nil;
  Acquire;
  try
    i:=FCli.search_g(RtcIntPtr(cli),a);
    if (i>0) and assigned(a) then
      begin
      Result:=c;
      if AcquireLock then
        Result.EnterEvent(nil);
      end;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.PrevClient(const cli:TRtcHttpGateClient; AcquireLock:boolean=False):TRtcHttpGateClient;
  var
    i:RtcIntPtr;
    a:TObject;
    c:TRtcHttpGateClient absolute a;
  begin
  Result:=nil;
  Acquire;
  try
    i:=FCli.search_l(RtcIntPtr(cli),a);
    if (i>0) and assigned(a) then
      begin
      Result:=c;
      if AcquireLock then
        Result.EnterEvent(nil);
      end;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.TotalClients:integer;
  begin
  Acquire;
  try
    Result:=FCli.Count;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.UnLockClient(Client:TRtcHttpGateClient);
  begin
  if assigned(Client) then
    Client.LeaveEvent(nil);
  end;

function TRtcGateAccountManager.IsMyUserID(UserID:TGateUID; const GateAddr,GatePort:RtcString):boolean;
  var
    i:RtcIntPtr;
    a:TObject;
    cli:TRtcHttpGateClient absolute a;
  begin
  Result:=False;
  Acquire;
  try
    if UserID>0 then
      begin
      i:=FCli.search_min(a);
      while (i>0) and assigned(a) do
        begin
        if (cli.MyUID=UserID) and (UpperCase(cli.GateAddr)=UpperCase(GateAddr)) and (cli.GatePort=GatePort) then
          begin
          Result:=True;
          Break;
          end;
        i:=FCli.search_g(i,a);
        end;
      end;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.IsMyUserAddr(const UserAddr:RtcString):boolean;
  var
    GateAddr,GatePort:RtcString;
    UserID:TGateUID;
  begin
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  Result:=IsMyUserID(UserID,GateAddr,GatePort);
  end;

function TRtcGateAccountManager.FindClient(UserID:TGateUID; const GateAddr,GatePort:RtcString; AcquireLock:boolean=False):TRtcHttpGateClient;
  var
    i:RtcIntPtr;
    a:TObject;
    cli:TRtcHttpGateClient absolute a;
  begin
  Result:=nil;
  Acquire;
  try
    if UserID>0 then
      begin
      i:=FCli.search_min(a);
      while (i>0) and assigned(a) do
        begin
        if (cli.MyUID=UserID) and (UpperCase(cli.GateAddr)=UpperCase(GateAddr)) and (cli.GatePort=GatePort) then
          begin
          Result:=cli;
          Break;
          end;
        i:=FCli.search_g(i,a);
        end;
      end;
    if Result=nil then
      begin
      i:=FCli.search_min(a);
      while (i>0) and assigned(a) do
        begin
        if (UpperCase(cli.GateAddr)=UpperCase(GateAddr)) and (cli.GatePort=GatePort) then
          begin
          Result:=cli;
          Break;
          end;
        i:=FCli.search_g(i,a);
        end;
      end;
    if AcquireLock and assigned(Result) then
      Result.EnterEvent(nil);
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.FindClient(const UserAddr:RtcString; AcquireLock:boolean=False):TRtcHttpGateClient;
  var
    GateAddr,GatePort:RtcString;
    UserID:TGateUID;
  begin
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  Result:=FindClient(UserID,GateAddr,GatePort,AcquireLock);
  end;

function TRtcGateAccountManager.FindMyUserAddr(const UserAddr: RtcString): RtcString;
  var
    GateAddr,GatePort:RtcString;
    UserID:TGateUID;
    cl:TRtcHttpGateClient;
  begin
  Result:='';
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  cl:=FindClient(UserID,GateAddr,GatePort,True);
  if assigned(cl) then
    try
      Result:=cl.MyAddr;
    finally
      UnLockClient(cl);
      end;
  end;

function TRtcGateAccountManager.FindClient(const GateAddr,GatePort:RtcString; AcquireLock:boolean=False):TRtcHttpGateClient;
  begin
  Result:=FindClient(0,GateAddr,GatePort,AcquireLock);
  end;

function TRtcGateAccountManager.RegisterPublic(const DisplayName: RtcWideString; activate:boolean): RtcString;
  var
    a:TObject;
    old:TRtcGateAccount absolute a;
    o:TRtcGateAccount;
    pf,id:RtcString;
    deleted:boolean;
  begin
  pf:=Utf8Encode(DisplayName);
  id:=rtcMakePublicKey(pf);

  o:=TRtcGateAccount.Create(gat_Public);
  o.LocalName:=DisplayName;
  o.RemoteName:=DisplayName;
  o.Active:=activate;
  o.wasActive:=activate;

  deleted:=False;
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      begin
      deleted:=old.wasActive or (old.GroupID>0);
      RemGroupID(id,old.GroupID);
      FACC.remove(id);
      RtcFreeAndNil(a);
      end;
    if activate then
      begin
      NewGroupID(id,o.GroupID);
      if o.GroupID=0 then
        begin
        o.Active:=False;
        o.wasActive:=False;
        activate:=False;
        end;
      end;
    FACC.insert(id,o);
  finally
    Release;
    end;

  if deleted then NotifyClients(id,gaa_Deleted);
  if activate then NotifyClients(id,gaa_Registered);

  Result:=id;
  end;

function TRtcGateAccountManager.FindPublic(const DisplayName: RtcWideString): RtcString;
  var
    pf,id:RtcString;
  begin
  pf:=Utf8Encode(DisplayName);
  id:=rtcMakePublicKey(pf);
  if isType[id]=gat_Public then
    Result:=id
  else
    Result:='';
  end;

function TRtcGateAccountManager.FindPrivate(const MD5Code8BitLinkID: RtcString): RtcString;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
    id:RtcString;
  begin
  Result:='';
  Acquire;
  try
    id:=FACC.search_min(a);
    while (id<>'') and assigned(a) do
      begin
      if o.AType=gat_Private then
        if rtcKeyMD5Code8BitLockMatch(id,MD5Code8BitLinkID) then
          begin
          Result:=id;
          Break;
          end;
      id:=FACC.search_g(id,a);
      end;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.FindLink(const MD5Code8BitID: RtcString): RtcString;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
    id:RtcString;
  begin
  Result:='';
  Acquire;
  try
    id:=FACC.search_min(a);
    while (id<>'') and assigned(a) do
      begin
      if MD5Code8BitID=MD5Code8Bit(id) then
        begin
        Result:=id;
        Break;
        end
      else if o.AType=gat_Private then
        if rtcKeyMD5Code8BitLockMatch(id,MD5Code8BitID) then
          begin
          Result:=id;
          Break;
          end;
      id:=FACC.search_g(id,a);
      end;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.GeneratePrivate(const DisplayName: RtcWideString; activate:boolean): RtcString;
  var
    a:TObject;
    old:TRtcGateAccount absolute a;
    o:TRtcGateAccount;
    pf,id:RtcString;
    deleted:boolean;
  begin
  pf:=Utf8Encode(DisplayName);
  id:=rtcGenerateKey(pf,GATEACCOUNT_RANDSEED,GATEACCOUNT_BIGKEY);

  o:=TRtcGateAccount.Create(gat_Private);
  o.LocalName:=DisplayName;
  o.RemoteName:=DisplayName;
  o.Active:=activate;
  o.wasActive:=activate;
{$IFDEF RTC_RSA}
  o.rsa.GenerateKeyPair(GATEACCOUNT_PRIVATE_RSAKEY_SIZE);
  o.haveKey:=True;
{$ENDIF}

  deleted:=False;
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      begin
      deleted:=old.wasActive or (old.groupID>0);
      RemGroupID(id,old.GroupID);
      FACC.remove(id);
      RtcFreeAndNil(a);
      end;
    if activate then
      begin
      NewGroupID(id,o.GroupID);
      if o.GroupID=0 then
        begin
        o.Active:=False;
        o.wasActive:=False;
        activate:=False;
        end;
      end;
    FACC.insert(id,o);
  finally
    Release;
    end;

  if deleted then NotifyClients(id,gaa_Deleted);
  if activate then NotifyClients(id,gaa_Registered);

  Result:=id;
  end;

function TRtcGateAccountManager.PrepareCopy(const id: RtcString; withPermissions:boolean): RtcByteArray;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
    r:TRtcRecord;
    i,p,q:RtcIntPtr;
    ar:TRtcArray;
  begin
  Acquire;
  try
    SetLength(Result,0);
    a:=FACC.search(id);
    if a<>nil then
      if o.AType=gat_Private then
        begin
        r:=TRtcRecord.Create;
        try
          r.asString['id']:=id;
          if o.LocalName=o.RemoteName then
            r.asText['dn']:=o.LocalName
          else
            begin
            r.asText['ln']:=o.LocalName;
            r.asText['rn']:=o.RemoteName;
            end;
        {$IFDEF RTC_RSA}
          if o.haveKey then
            r.asByteArray['key']:=o.rsa.PrivateKey;
        {$ENDIF}
          if withPermissions then
            begin
            p:=o.Permissions.search_min(q);
            if p>0 then
              begin
              i:=0; ar:=r.newArray('per');
              repeat
                ar.asInteger[i]:=p;
                p:=o.Permissions.search_g(p,q);
                Inc(i);
                until p<=0;
              end;
            end;
          Result:=r.toCodeEx;
        finally
          r.Free;
          end;
        end
      else if o.AType=gat_Linked then
        begin
        r:=TRtcRecord.Create;
        try
          r.asString['id']:=id;
          if o.LocalName=o.RemoteName then
            r.asText['dn']:=o.LocalName
          else
            begin
            r.asText['ln']:=o.LocalName;
            r.asText['rn']:=o.RemoteName;
            end;
        {$IFDEF RTC_RSA}
          if o.haveKey then
            r.asByteArray['key']:=o.rsa.PublicKey;
        {$ENDIF}
          if withPermissions then
            begin
            p:=o.Permissions.search_min(q);
            if p>0 then
              begin
              i:=0; ar:=r.newArray('per');
              repeat
                ar.asInteger[i]:=p;
                p:=o.Permissions.search_g(p,q);
                Inc(i);
                until p<=0;
              end;
            end;
          Result:=r.toCodeEx;
        finally
          r.Free;
          end;
        end
      else if o.AType=gat_Public then
        begin
        r:=TRtcRecord.Create;
        try
          r.asString['id']:=id;
          if o.LocalName=o.RemoteName then
            r.asText['dn']:=o.LocalName
          else
            begin
            r.asText['ln']:=o.LocalName;
            r.asText['rn']:=o.RemoteName;
            end;
          if withPermissions then
            begin
            p:=o.Permissions.search_min(q);
            if p>0 then
              begin
              i:=0; ar:=r.newArray('per');
              repeat
                ar.asInteger[i]:=p;
                p:=o.Permissions.search_g(p,q);
                Inc(i);
                until p<=0;
              end;
            end;
          Result:=r.toCodeEx;
        finally
          r.Free;
          end;
        end;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.PrepareLink(const id: RtcString): RtcByteArray;
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
    r:TRtcRecord;
  begin
  Acquire;
  try
    SetLength(Result,0);
    a:=FACC.search(id);
    if a<>nil then
      if o.AType=gat_Private then
        begin
        r:=TRtcRecord.Create;
        try
          r.asString['id']:=rtcCalculateLock(id);
          if o.LocalName=o.RemoteName then
            r.asText['dn']:=o.LocalName
          else
            begin
            r.asText['ln']:=o.RemoteName;
            r.asText['rn']:=o.LocalName;
            end;
        {$IFDEF RTC_RSA}
          if o.haveKey then
            r.asByteArray['key']:=o.rsa.PublicKey;
        {$ENDIF}
          Result:=r.toCodeEx;
        finally
          r.Free;
          end;
        end
      else if o.AType=gat_Linked then
        begin
        r:=TRtcRecord.Create;
        try
          r.asString['id']:=id;
          if o.LocalName=o.RemoteName then
            r.asText['dn']:=o.LocalName
          else
            begin
            r.asText['ln']:=o.LocalName;
            r.asText['rn']:=o.RemoteName;
            end;
        {$IFDEF RTC_RSA}
          if o.haveKey then
            r.asByteArray['key']:=o.rsa.PublicKey;
        {$ENDIF}
          Result:=r.toCodeEx;
        finally
          r.Free;
          end;
        end
      else if o.AType=gat_Public then
        begin
        r:=TRtcRecord.Create;
        try
          r.asString['id']:=id;
          if o.LocalName=o.RemoteName then
            r.asText['dn']:=o.LocalName
          else
            begin
            r.asText['ln']:=o.LocalName;
            r.asText['rn']:=o.RemoteName;
            end;
          Result:=r.toCodeEx;
        finally
          r.Free;
          end;
        end;
  finally
    Release;
    end;
  end;

function TRtcGateAccountManager.RegisterAccount(const Data: RtcByteArray; withPermissions, activate:boolean): RtcString;
  var
    a:TObject;
    old:TRtcGateAccount absolute a;
    o:TRtcGateAccount;
    r:TRtcRecord;

    id:RtcString;
    ln,rn:RtcWideString;
    att:TRtcGateAccountType;
    ar:TRtcArray;
    i:integer;
    deleted:boolean;
  begin
  r:=TRtcRecord.FromCode(RtcBytesToString(Data));
  try
    if r.CheckType('id',rtc_String) then
      id:=r.asString['id']
    else
      raise ERtcGateClient.Create('RegisterAccount: "Data" missing Account ID');
    if r.CheckType('ln',rtc_Text) or r.CheckType('rn',rtc_Text) then
      begin
      ln:=r.asText['ln'];
      rn:=r.asText['rn'];
      end
    else if r.CheckType('dn',rtc_Text) then
      begin
      ln:=r.asText['dn'];
      rn:=r.asText['dn'];
      end
    else
      raise ERtcGateClient.Create('RegisterAccount: "Data" missing Display Name');

    if rtcKeyPublic(id) then
      att:=gat_Public
    else if rtcKeyValid(id) then
      att:=gat_Private
    else if rtcLockValid(id) then
      att:=gat_Linked
    else
      raise ERtcGateClient.Create('RegisterAccount: "Data" contains invalid Account ID');

    o:=TRtcGateAccount.Create(att);
    try
      o.LocalName:=ln;
      o.RemoteName:=rn;
      o.Active:=activate;
      o.wasActive:=activate;
    {$IFDEF RTC_RSA}
      if r.CheckType('key',rtc_ByteArray) then
        if att=gat_Private then
          begin
          o.rsa.PrivateKey:=r.asByteArray['key'];
          o.haveKey:=True;
          end
        else if att=gat_Linked then
          begin
          o.rsa.PublicKey:=r.asByteArray['key'];
          o.haveKey:=True;
          end;
    {$ENDIF}
      if withPermissions and r.CheckType('per',rtc_Array) then
        begin
        ar:=r.asArray['per'];
        for i:=0 to ar.Count-1 do
          if o.Permissions.search(ar.asInteger[i])<=0 then
            o.Permissions.insert(ar.asInteger[i],1);
        end;
    except
      RtcFreeAndNil(o);
      raise;
      end;

  finally
    r.Free;
    end;

  deleted:=False;
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      begin
      deleted:=old.wasActive or (old.GroupID>0);
      RemGroupID(id,old.GroupID);
      FACC.remove(id);
      RtcFreeAndNil(a);
      end;
    if activate then
      begin
      NewGroupID(id,o.GroupID);
      if o.GroupID=0 then
        begin
        o.Active:=False;
        o.wasActive:=False;
        activate:=False;
        end;
      end;
    FACC.insert(id,o);
  finally
    Release;
    end;

  if deleted then NotifyClients(id, gaa_Deleted);
  if activate then NotifyClients(id, gaa_Registered);

  Result:=id;
  end;

function TRtcGateAccountManager.CheckAccount(const Data: RtcByteArray): RtcWideString;
  var
    o:TRtcGateAccount;
    r:TRtcRecord;

    id:RtcString;
    ln,rn:RtcWideString;
    att:TRtcGateAccountType;
    ar:TRtcArray;
    i:integer;
  begin
  r:=TRtcRecord.FromCode(RtcBytesToString(Data));
  try
    if r.CheckType('id',rtc_String) then
      id:=r.asString['id']
    else
      raise ERtcGateClient.Create('CheckAccount: "Data" missing Account ID');
    if r.CheckType('ln',rtc_Text) or r.CheckType('rn',rtc_Text) then
      begin
      ln:=r.asText['ln'];
      rn:=r.asText['rn'];
      end
    else if r.CheckType('dn',rtc_Text) then
      begin
      ln:=r.asText['dn'];
      rn:=r.asText['dn'];
      end
    else
      raise ERtcGateClient.Create('CheckAccount: "Data" missing Display Name');

    if rtcKeyPublic(id) then
      att:=gat_Public
    else if rtcKeyValid(id) then
      att:=gat_Private
    else if rtcLockValid(id) then
      att:=gat_Linked
    else
      raise ERtcGateClient.Create('CheckAccount: "Data" contains invalid Account ID');

    o:=TRtcGateAccount.Create(att);
    try
      o.LocalName:=ln;
      o.RemoteName:=rn;
    {$IFDEF RTC_RSA}
      if r.CheckType('key',rtc_ByteArray) then
        if att=gat_Private then
          begin
          o.rsa.PrivateKey:=r.asByteArray['key'];
          o.haveKey:=True;
          end
        else if att=gat_Linked then
          begin
          o.rsa.PublicKey:=r.asByteArray['key'];
          o.haveKey:=True;
          end;
    {$ENDIF}
      if r.CheckType('per',rtc_Array) then
        begin
        ar:=r.asArray['per'];
        for i:=0 to ar.Count-1 do
          if o.Permissions.search(ar.asInteger[i])<=0 then
            o.Permissions.insert(ar.asInteger[i],1);
        end;
    finally
      o.Free;
      end;
  finally
    r.Free;
    end;
  if ln=rn then
    Result:=ln
  else if ln='' then
    Result:=rn
  else if rn='' then
    Result:=ln
  else
    Result:=ln+' @ '+rn;
  end;

function TRtcGateAccountManager.SaveToCode: RtcByteArray;
  var
    acc:RtcString;
    s:RtcByteArray;
    r:TRtcDataSet;
  begin
  SetLength(Result,0);
  SetLength(s,0);
  r:=nil;
  Acquire;
  try
    acc:=FirstID;
    while (acc<>'') do
      begin
      s:=PrepareCopy(acc,True);
      if r=nil then r:=TRtcDataSet.Create;
      r.Append;
      r.asCodeEx['data']:=s;
      r.asBoolean['active']:=isActive[acc];
      acc:=NextID(acc);
      end;
    if Assigned(r) then
      Result:=r.toCodeEx;
  finally
    RtcFreeAndNil(r);
    Release;
    end;
  end;

procedure TRtcGateAccountManager.RegisterFromCode(const code: RtcByteArray; withPermissions,activate:boolean);
  var
    r:TRtcDataSet;
  begin
  if length(code)=0 then Exit;

  r:=TRtcDataSet.FromCode(RtcBytesToString(code));
  try
    r.First;
    while not r.Eof do
      begin
      if r.CheckType('data',rtc_Record) then
        RegisterAccount(r.asCodeEx['data'], withPermissions, r.asBoolean['active'] and activate);
      r.Next;
      end;
  finally
    RtcFreeAndNil(r);
    end;
  end;

function TRtcGateAccountManager.MakeSignature(const id: RtcString; const content:RtcByteArray): RtcByteArray;
{$IFDEF RTC_RSA}
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
    c:RtcString;
{$ENDIF}
  begin
  SetLength(Result,0);
{$IFDEF RTC_RSA}
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      if o.haveKey then
        if o.AType=gat_Private then // generate for LINK
          begin
          c:=MD5Code8bit(rtcCalculateLock(id)+RtcBytesToString(content)+RtcBytesToString(o.rsa.PublicKey));
          Result:=o.rsa.SignHash(rtch_MD5,RtcStringToBytes(c));
          end
        else if o.AType=gat_Linked then // generate for PRIVATE
          begin
          c:=MD5Code8Bit(id+RtcBytesToString(content));
          Result:=o.rsa.Encrypt(RtcStringToBytes(c));
          end;
  finally
    Release;
    end;
{$ENDIF}
  end;

function TRtcGateAccountManager.VerifySignature(const id: RtcString; const content:RtcByteArray; const Signature: RtcByteArray): boolean;
{$IFDEF RTC_RSA}
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
    c:RtcString;
{$ENDIF}
  begin
  Result:=False;
{$IFDEF RTC_RSA}
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      if o.haveKey then
        if o.AType=gat_Linked then // received from PRIVATE
          begin
          c:=MD5Code8bit(id+RtcBytesToString(content)+RtcBytesToString(o.rsa.PublicKey));
          Result:=o.rsa.VerifyHash(rtch_MD5,RtcStringToBytes(c),Signature);
          end
        else if o.AType=gat_Private then // received from LINK
          begin
          c:=MD5Code8Bit(rtcCalculateLock(id)+RtcBytesToString(content));
          Result:=RtcBytesToString(o.rsa.Decrypt(Signature))=c;
          end;
  finally
    Release;
    end;
{$ENDIF}
  end;

function TRtcGateAccountManager.PrivateEncrypt(const id: RtcString; const content: RtcByteArray): RtcByteArray;
{$IFDEF RTC_RSA}
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
{$ENDIF}
  begin
  SetLength(Result,0);
{$IFDEF RTC_RSA}
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      if o.haveKey then
        if o.AType=gat_Linked then // Encrypt content for PRIVATE
          Result:=o.rsa.Encrypt(content);
  finally
    Release;
    end;
{$ENDIF}
  end;

function TRtcGateAccountManager.PrivateDecrypt(const id: RtcString; const content: RtcByteArray): RtcByteArray;
{$IFDEF RTC_RSA}
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
{$ENDIF}
  begin
  SetLength(Result,0);
{$IFDEF RTC_RSA}
  Acquire;
  try
    a:=FACC.search(id);
    if a<>nil then
      if o.haveKey then
        if o.AType=gat_Private then // Decrypt content received from LINK
          Result:=o.rsa.Decrypt(content);
  finally
    Release;
    end;
{$ENDIF}
  end;

function TRtcGateAccountManager.PublicKey: RtcByteArray;
  begin
  SetLength(Result,0);
{$IFDEF RTC_RSA}
  Acquire;
  try
    if skey=nil then
      begin
      skey:=TRtcRSA.Create;
      skey.GenerateKeyPair(GATEACCOUNT_PUBLIC_RSAKEY_SIZE);
      end;
  finally
    Release;
    end;
  Result:=skey.PublicKey;
{$ENDIF}
  end;

function TRtcGateAccountManager.PublicID: RtcString;
  begin
  Acquire;
  try
    if FPublicID='' then
      FPublicID:=rtcGenerateKey('',GATEID_RANDSEED,GATEID_BIGKEY);
  finally
    Release;
    end;
  Result:=FPublicID;
  end;

function TRtcGateAccountManager.PublicEncrypt(const key,content: RtcByteArray): RtcByteArray;
{$IFDEF RTC_RSA}
  var
    mkey:TRtcRSA;
{$ENDIF}
  begin
  SetLength(Result,0);
{$IFDEF RTC_RSA}
  mkey:=TRtcRSA.Create;
  try
    mkey.PublicKey:=key;
    Result:=mkey.Encrypt(content);
  finally
    RtcFreeAndNil(mkey);
    end;
{$ENDIF}
  end;

function TRtcGateAccountManager.PublicDecrypt(const content: RtcByteArray): RtcByteArray;
  begin
  SetLength(Result,0);
{$IFDEF RTC_RSA}
  Acquire;
  try
    // if "skey" is not already assigned here, then "PublicDecrypt" was used before "PublicKey" was accessed.
    if skey=nil then
      raise ERtcGateClient.Create('PublicDecrypt: "PublicKey" has not been generated yet.');
  finally
    Release;
    end;
  Result:=skey.Decrypt(content);
{$ENDIF}
  end;

procedure TRtcGateAccountManager.RefreshClient(cli: TRtcHttpGateClient);
  var
    a:TObject;
    o:TRtcGateAccount absolute a;
    id:RtcString;
  begin
  Acquire;
  try
    // Send "Registered" notifications to the Client for all Active Accounts
    id:=FACC.search_min(a);
    while (id<>'') and (a<>nil) and (FCli.search(RtcIntPtr(cli))=cli) do
      begin
      if o.Active then
        begin
        Release;
        try
          cli.Call_AccountChange(id,gaa_Registered);
        finally
          Acquire;
          end;
        end;
      id:=FACC.search_g(id,a);
      end;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.RegisterClient(cli: TRtcHttpGateClient);
  begin
  Acquire;
  try
    if FCli.search(RtcIntPtr(cli))=nil then
      FCli.insert(RtcIntPtr(cli),cli);
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.UnregisterClient(cli: TRtcHttpGateClient);
  begin
  Acquire;
  try
    if FCli.search(RtcIntPtr(cli))=cli then
      FCli.remove(RtcIntPtr(cli));
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.NotifyClients(const id: RtcString; action: TRtcGateAccountAction);
  var
    i:RtcIntPtr;
    a:TObject;
    cli:TRtcHttpGateClient absolute a;
  begin
  Acquire;
  try
    i:=FCli.search_min(a);
    while (i>0) and assigned(a) do
      begin
      Release;
      try
        cli.Call_AccountChange(id,action);
      finally
        Acquire;
        end;
      i:=FCli.search_g(i,a);
      end;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.UnregisterAllClients;
  var
    i:RtcIntPtr;
    a:TObject;
    cli:TRtcHttpGateClient absolute a;
  begin
  Acquire;
  try
    i:=FCli.search_min(a);
    while (i>0) and assigned(a) do
      begin
      cli.AccountManager:=nil;
      i:=FCli.search_g(i,a);
      end;
  finally
    Release;
    end;
  end;

procedure TRtcGateAccountManager.NewGroupID(const id: RtcString; var group: TGateUID);
  var
    i:integer;
    found:boolean;
    art:TAppRunTime;
  begin
  // No need for Acquire/Release, because this method is
  // used only internally from inside Acquire/Release blocks
  if group=0 then
    begin
    art:=GetAppRunTime;
    if (FNextGroupID<FMinGroupID) or (FNextGroupID>FMaxGroupID) then FNextGroupID:=FMaxGroupID;
    found:=False;
    for i:=FNextGroupID downto FMinGroupID do
      if FGIDTime[i]<=art then
        begin
        found:=True;
        FNextGroupID:=i-1;
        Inc(FUsedGIDs);
        FGIDs[i]:=id;
        FGIDTime[i]:=RtcMaxLongWord;
        group:=i;
        Break;
        end;
    if not found then
      for i:=FMaxGroupID downto FNextGroupID+1 do
        if FGIDTime[i]<=art then
          begin
          FNextGroupID:=i-1;
          Inc(FUsedGIDs);
          FGIDs[i]:=id;
          FGIDTime[i]:=RtcMaxLongWord;
          group:=i;
          Break;
          end;
    end;
  end;

procedure TRtcGateAccountManager.RemGroupID(const id: RtcString; var group: TGateUID);
  begin
  // No need for Acquire/Release, because this method is
  // used only internally from inside Acquire/Release blocks
  if (group>=MinGID) and (group<=MaxGID) then
    if (FGIDs[group]=id) and (FGIDTime[group]=RtcMaxLongWord) then
      begin
      Dec(FUsedGIDs);
      // We will keep the Account ID, so we can still find it when we
      // receive "NotReady" and "NotWaiting" notifications from the Gateway.
      // -> FGIDs[group]:=''; <- Do NOT clear!!!
      // Mark this GroupID for reuse after the timeout
      FGIDTime[group]:=GetAppRunTime+GATEACCMAN_GROUPID_LOCK;
      group:=0;
      end;
  end;

procedure TRtcGateAccountManager.SetMaxGroupID(const Value: TGateUID);
  begin
  if Value>=MaxGID then
    FMaxGroupID:=MaxGID
  else if Value<MinGID then
    FMaxGroupID:=MinGID
  else
    FMaxGroupID := Value;
  if FMaxGroupID<FMinGroupID then
    FMinGroupID:=FMaxGroupID;
  end;

procedure TRtcGateAccountManager.SetMinGroupID(const Value: TGateUID);
  begin
  if Value>=MaxGID then
    FMinGroupID:=MaxGID
  else if Value<MinGID then
    FMinGroupID:=MinGID
  else
    FMinGroupID := Value;
  if FMinGroupID>FMaxGroupID then
    FMaxGroupID:=FMinGroupID;
  end;

procedure TRtcGateAccountManager.Call_AfterLoggedIn(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConAfterLoggedIn,FGuiAfterLoggedIn,Sender) then
    if assigned(FThread) then
      FThread.Post_AfterLoggedIn(Sender);
  end;

procedure TRtcGateAccountManager.Call_AfterLoginFail(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConAfterLoginFail,FGuiAfterLoginFail,Sender) then
    if assigned(FThread) then
      FThread.Post_AfterLoginFail(Sender);
  end;

procedure TRtcGateAccountManager.Call_AfterLogOut(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConAfterLogOut,FGuiAfterLogOut,Sender) then
    if assigned(FThread) then
      FThread.Post_AfterLogOut(Sender);
  end;

procedure TRtcGateAccountManager.Call_BeforeLogIn(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConBeforeLogIn,FGuiBeforeLogIn,Sender) then
    if assigned(FThread) then
      FThread.Post_BeforeLogIn(Sender);
  end;

procedure TRtcGateAccountManager.Call_OnDataReceived(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Filter(FOnDataFilter,Sender) then
    if Call(FConDataReceived,FGuiDataReceived,Sender) then
      if assigned(FThread) then
        FThread.Post_DataReceived(Sender);
  end;

procedure TRtcGateAccountManager.Call_OnInfoReceived(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Filter(FOnInfoFilter,Sender) then
    if Call(FConInfoReceived,FGuiInfoReceived,Sender) then
      if assigned(FThread) then
        FThread.Post_InfoReceived(Sender);
  end;

procedure TRtcGateAccountManager.Call_OnReadyToSend(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConReadyToSend,FGuiReadyToSend,Sender) then
    if assigned(FThread) then
      FThread.Post_ReadyToSend(Sender);
  end;

procedure TRtcGateAccountManager.Call_OnReadyAfterReset(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConReadyAfterReset,FGuiReadyAfterReset,Sender) then
    if assigned(FThread) then
      FThread.Post_ReadyAfterReset(Sender);
  end;

procedure TRtcGateAccountManager.Call_OnStreamReset(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConStreamReset,FGuiStreamReset,Sender) then
    if assigned(FThread) then
      FThread.Post_StreamReset(Sender);
  end;

procedure TRtcGateAccountManager.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FThread then
      FThread:=nil;
  end;

{ TRtcGateClientData }

constructor TRtcGateClientData.Create;
  begin
  inherited Create;
  SetLength(FBytes,0);
  SetLength(FContent,0);
  end;

destructor TRtcGateClientData.Destroy;
  begin
  SetLength(FBytes,0);
  SetLength(FContent,0);
  inherited;
  end;

procedure TRtcGateClientData.CopyFrom(const FromData: TRtcGateClientData);
  begin
  if FromData=nil then Exit;

  // F:=FromData.F;
  Move(FromData.F,F,SizeOf(F));

  FContent:=FromData.Content;
  FGateAddr:=FromData.FGateAddr;
  FGatePort:=FromData.FGatePort;
  FAccountID:=FromData.FAccountID;
  FUserInfo:=FromData.FUserInfo;
  end;

function TRtcGateClientData.GetUserAddr: RtcString;
  begin
  if GatePort='80' then
    Result:=Int2Str(UserID)+'@'+GateAddr
  else
    Result:=Int2Str(UserID)+'@'+GateAddr+':'+GatePort;
  end;

function TRtcGateClientData.GetMyAddr: RtcString;
  begin
  if GatePort='80' then
    Result:=Int2Str(MyUID)+'@'+GateAddr
  else
    Result:=Int2Str(MyUID)+'@'+GateAddr+':'+GatePort;
  end;

function TRtcGateClientData.HeaderAndFooter: boolean;
  begin
  Result:=F.Header and F.Footer;
  end;

{ TRtcMultiGateClientLinkList }

constructor TRtcMultiGateClientLinkList.Create;
  begin
  inherited;
  FList:=TObjList.Create(16);
  CS:=TRtcCritSec.Create;
  end;

destructor TRtcMultiGateClientLinkList.Destroy;
  begin
  try
    RtcFreeAndNil(FList);
    RtcFreeAndNil(CS);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcMultiGateClientLinkList.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcMultiGateClientLinkList.Count: integer;
  begin
  Result:=0;
  if (self=nil) or not assigned(CS) then Exit;
  CS.Acquire;
  try
    if assigned(FList) then
      Result:=FList.Count;
  finally
    CS.Release;
    end;
  end;

procedure TRtcMultiGateClientLinkList.Add(Value: TRtcAbsMultiGateClientLink);
  begin
  if (self=nil) or not assigned(CS) then Exit;
  CS.Acquire;
  try
    if assigned(FList) then
      if FList.search(RtcIntPtr(Value))=nil then
        FList.insert(RtcIntPtr(Value),Value);
  finally
    CS.Release;
    end;
  end;

function TRtcMultiGateClientLinkList.LockFirst:TRtcAbsMultiGateClientLink;
  var
    i:RtcIntPtr;
    a:TObject;
    l:TRtcAbsMultiGateClientLink absolute a;
  begin
  Result:=nil;
  if (self=nil) or not assigned(CS) then Exit;
  CS.Acquire;
  try
    if assigned(FList) then
      begin
      i:=FList.search_min(a);
      if (i>0) and assigned(a) then
        begin
        Result:=l;
        Inc(Result.FLocked);
        end;
      end;
  finally
    CS.Release;
    end;
  end;

procedure TRtcMultiGateClientLinkList.LockNext(var Value:TRtcAbsMultiGateClientLink);
  var
    i:RtcIntPtr;
    a:TObject;
    l:TRtcAbsMultiGateClientLink absolute a;
    Removed:TRtcAbsMultiGateClientLink;
  begin
  Removed:=nil;
  if (self=nil) or not assigned(CS) then
    begin
    Value:=nil;
    Exit;
    end;
  CS.Acquire;
  try
    Dec(Value.FLocked);
    if Value.FLocked=0 then
      if FList.search(RtcIntPtr(Value))=nil then
        Removed:=Value;
    if assigned(FList) then
      begin
      i:=FList.search_g(RtcIntPtr(Value),a);
      if (i>0) and assigned(a) then
        begin
        Value:=l;
        Inc(Value.FLocked);
        end
      else
        Value:=nil;
      end
    else
      Value:=nil;
  finally
    CS.Release;
    end;
  if assigned(Removed) then
    Removed.Sync_MultiClientRemoved;
  end;

procedure TRtcMultiGateClientLinkList.Remove(Value: TRtcAbsMultiGateClientLink);
  var
    Removed:TRtcAbsMultiGateClientLink;
  begin
  Removed:=nil;
  if (self=nil) or not assigned(CS) then Exit;
  CS.Acquire;
  try
    if assigned(FList) then
      if FList.search(RtcIntPtr(Value))=Value then
        begin
        FList.remove(RtcIntPtr(Value));
        if Value.FLocked=0 then
          Removed:=Value;
        end;
  finally
    CS.Release;
    end;
  if assigned(Removed) then
    Removed.Sync_MultiClientRemoved;
  end;

procedure TRtcMultiGateClientLinkList.RemoveAll;
  var
    Link:TRtcAbsMultiGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    Remove(Link);
    LockNext(Link);
    end;
  end;

procedure TRtcMultiGateClientLinkList.DoAfterLoggedIn(Sender: TRtcConnection);
  var
    Link:TRtcAbsMultiGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_AfterLoggedIn(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcMultiGateClientLinkList.DoAfterLoginFail(Sender: TRtcConnection);
  var
    Link:TRtcAbsMultiGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_AfterLoginFail(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcMultiGateClientLinkList.DoAfterLogOut(Sender: TRtcConnection);
  var
    Link:TRtcAbsMultiGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_AfterLogOut(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcMultiGateClientLinkList.DoBeforeLogIn(Sender: TRtcConnection);
  var
    Link:TRtcAbsMultiGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_BeforeLogIn(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcMultiGateClientLinkList.DoDataReceived(Sender: TRtcConnection);
  var
    Link:TRtcAbsMultiGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_OnDataReceived(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcMultiGateClientLinkList.DoInfoReceived(Sender: TRtcConnection);
  var
    Link:TRtcAbsMultiGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_OnInfoReceived(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcMultiGateClientLinkList.DoReadyToSend(Sender: TRtcConnection);
  var
    Link:TRtcAbsMultiGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_OnReadyToSend(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcMultiGateClientLinkList.DoReadyAfterReset(Sender: TRtcConnection);
  var
    Link:TRtcAbsMultiGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_OnReadyAfterReset(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

procedure TRtcMultiGateClientLinkList.DoStreamReset(Sender: TRtcConnection);
  var
    Link:TRtcAbsMultiGateClientLink;
  begin
  if (self=nil) then Exit;
  Link:=LockFirst;
  while assigned(Link) do
    begin
    try
      Link.Call_OnStreamReset(Sender);
    except
      // ignore exceptions
      end;
    LockNext(Link);
    end;
  end;

{ TRtcAbsMultiGateClientLink }

constructor TRtcAbsMultiGateClientLink.Create(AOwner: TComponent);
  begin
  inherited;
  FMyGroupID:=0;
  FMultiClient:=nil;
  end;

destructor TRtcAbsMultiGateClientLink.Destroy;
  begin
  MultiClient:=nil;
  inherited;
  end;

function TRtcAbsMultiGateClientLink.GetMultiClient: TRtcHttpMultiGateClient;
  begin
  Result:=FMultiClient;
  end;

function TRtcAbsMultiGateClientLink.GetMyGroupID: TGateUID;
  begin
  Result:=FMyGroupID;
  end;

procedure TRtcAbsMultiGateClientLink.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FMultiClient then
      begin
      FMyGroupID:=0;
      Sync_MultiClientRemoved;
      end;
  end;

procedure TRtcAbsMultiGateClientLink.RemoveMultiClient(Value: TRtcHttpMultiGateClient);
  begin
  if Value=FMultiClient then MultiClient:=nil;
  end;

procedure TRtcAbsMultiGateClientLink.SetMyGroupID(const Value: TGateUID);
  begin
  if Value<>FMyGroupID then
    begin
    if assigned(FMultiClient) then
      begin
      if FMyGroupID>0 then
        begin
        FMultiClient.DisbandMyGroup(FMyGroupID);
        FMultiClient.SharedGroups.ReleaseID(FMyGroupID);
        FMyGroupID:=0;
        end;
      FMyGroupID:=FMultiClient.SharedGroups.AllocFreeID(Value);
      end;
    end;
  end;

procedure TRtcAbsMultiGateClientLink.SetMultiClient(const Value: TRtcHttpMultiGateClient);
  begin
  if Value<>FMultiClient then
    begin
    if assigned(FMultiClient) then
      begin
      if FMyGroupID>0 then
        begin
        FMultiClient.DisbandMyGroup(FMyGroupID);
        FMultiClient.SharedGroups.ReleaseID(FMyGroupID);
        FMyGroupID:=0;
        end;
      FMultiClient.RemoveMultiGateClientLink(self);
      end;
    if assigned(Value) and not assigned(FMultiClient) then
      begin
      { Allocate the first available Group ID.
        If Group ID allocation fails, which can happen if all
        4095 Group IDs have already been taken, an exception will
        be raised and the "MultiClient" property will remain "NIL". }
      FMyGroupID:=Value.SharedGroups.AllocFreeID(0);
      FMultiClient:=Value;
      FMultiClient.AddMultiGateClientLink(self);
      Call_MultiClientAssigned;
      end;
    end;
  end;

function TRtcAbsMultiGateClientLink.PingUser(const UserAddr: RtcString): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.PingUser(UserAddr);
  end;

function TRtcAbsMultiGateClientLink.AddUserToGroup(const UserAddr: RtcString): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.AddUserToMyGroup(FMyGroupID,UserAddr);
  end;

function TRtcAbsMultiGateClientLink.RemoveUserFromGroup(const UserAddr: RtcString): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.RemoveUserFromMyGroup(FMyGroupID,UserAddr);
  end;

function TRtcAbsMultiGateClientLink.IsUserInGroup(const UserAddr: RtcString): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.IsUserInMyGroup(FMyGroupID,UserAddr);
  end;

function TRtcAbsMultiGateClientLink.UsersInGroup: integer;
  begin
  Result:=0;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.UsersInMyGroup(FMyGroupID);
  end;

function TRtcAbsMultiGateClientLink.DisbandGroup: boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.DisbandMyGroup(FMyGroupID);
  end;

function TRtcAbsMultiGateClientLink.SendBytes(const UserAddr:RtcString; GroupID:TGateUID; CallID: word; const data: RtcByteArray): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.SendBytes(UserAddr,GroupID,CallID,data);
  end;

function TRtcAbsMultiGateClientLink.SendBytes(const UserAddr:RtcString; GroupID:TGateUID; CallID: word; const data: RtcString): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.SendBytes(UserAddr,GroupID,CallID,data);
  end;

function TRtcAbsMultiGateClientLink.SendBytes(const UserAddr:RtcString; GroupID:TGateUID; CallID: word): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.SendBytes(UserAddr,GroupID,CallID);
  end;

function TRtcAbsMultiGateClientLink.SendToGroup(CallID: word; const data: RtcByteArray): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.SendToMyGroup(FMyGroupID,CallID,data);
  end;

function TRtcAbsMultiGateClientLink.SendToGroup(CallID: word; const data: RtcString): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.SendToMyGroup(FMyGroupID,CallID,data);
  end;

function TRtcAbsMultiGateClientLink.SendToGroup(CallID: word): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.SendToMyGroup(FMyGroupID,CallID);
  end;

function TRtcAbsMultiGateClientLink.UserAllowed(const UserAddr: RtcString; const action: word): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.UserAllowed(UserAddr,action);
  end;

function TRtcAbsMultiGateClientLink.UserInfo(const UserAddr: RtcString): RtcString;
  begin
  Result:='';
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.UserInfo(UserAddr);
  end;

function TRtcAbsMultiGateClientLink.UserNotVerified(const UserAddr: RtcString): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.UserNotVerified(UserAddr);
  end;

function TRtcAbsMultiGateClientLink.VerifyUser(const UserAddr: RtcString): boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.RequestUserVerifications(UserAddr);
  end;

function TRtcAbsMultiGateClientLink.AddFriend(const UserAddr:RtcString):boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.AddFriend(UserAddr);
  end;

function TRtcAbsMultiGateClientLink.RemoveFriend(const UserAddr:RtcString):boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.RemoveFriend(UserAddr);
  end;

function TRtcAbsMultiGateClientLink.LeaveUsersGroup(const OwnerAddr:RtcString; GroupID:TGateUID):boolean;
  begin
  Result:=False;
  if (self=nil) or (FMultiClient=nil) then Exit;
  Result:=FMultiClient.LeaveUsersGroup(OwnerAddr,GroupID);
  end;

procedure TRtcAbsMultiGateClientLink.Call_MultiClientAssigned;
  begin
  end;

procedure TRtcAbsMultiGateClientLink.Call_MultiClientRemoved;
  begin
  FMultiClient:=nil;
  end;

procedure TRtcAbsMultiGateClientLink.Sync_MultiClientRemoved;
  begin
  if (self=nil) then Exit;
  if InsideMainThread then
    Call_MultiClientRemoved
  else
    PostGUIJob(Call_MultiClientRemoved);
  end;

{ TRtcMultiGateClientLink }

procedure TRtcMultiGateClientLink.Call_AfterLoggedIn(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConAfterLoggedIn,FGuiAfterLoggedIn,Sender) then
    if assigned(FThread) then
      FThread.Post_AfterLoggedIn(Sender);
  end;

procedure TRtcMultiGateClientLink.Call_AfterLoginFail(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConAfterLoginFail,FGuiAfterLoginFail,Sender) then
    if assigned(FThread) then
      FThread.Post_AfterLoginFail(Sender);
  end;

procedure TRtcMultiGateClientLink.Call_AfterLogOut(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConAfterLogOut,FGuiAfterLogOut,Sender) then
    if assigned(FThread) then
      FThread.Post_AfterLogOut(Sender);
  end;

procedure TRtcMultiGateClientLink.Call_BeforeLogIn(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConBeforeLogIn,FGuiBeforeLogIn,Sender) then
    if assigned(FThread) then
      FThread.Post_BeforeLogIn(Sender);
  end;

procedure TRtcMultiGateClientLink.Call_OnDataReceived(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Filter(FOnDataFilter,Sender) then
    if Call(FConDataReceived,FGuiDataReceived,Sender) then
      if assigned(FThread) then
        FThread.Post_DataReceived(Sender);
  end;

procedure TRtcMultiGateClientLink.Call_OnInfoReceived(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Filter(FOnInfoFilter,Sender) then
    if Call(FConInfoReceived,FGuiInfoReceived,Sender) then
      if assigned(FThread) then
        FThread.Post_InfoReceived(Sender);
  end;

procedure TRtcMultiGateClientLink.Call_OnReadyToSend(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConReadyToSend,FGuiReadyToSend,Sender) then
    if assigned(FThread) then
      FThread.Post_ReadyToSend(Sender);
  end;

procedure TRtcMultiGateClientLink.Call_OnReadyAfterReset(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConReadyAfterReset,FGuiReadyAfterReset,Sender) then
    if assigned(FThread) then
      FThread.Post_ReadyAfterReset(Sender);
  end;

procedure TRtcMultiGateClientLink.Call_OnStreamReset(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  if Call(FConStreamReset,FGuiStreamReset,Sender) then
    if assigned(FThread) then
      FThread.Post_StreamReset(Sender);
  end;

procedure TRtcMultiGateClientLink.Call_MultiClientAssigned;
  begin
  inherited;
  if FAmSilent then Exit;
  if assigned(FClientAssigned) then
    FClientAssigned;
  end;

procedure TRtcMultiGateClientLink.Call_MultiClientRemoved;
  begin
  inherited;
  if FAmSilent then Exit;
  if assigned(FClientRemoved) then
    FClientRemoved;
  end;

procedure TRtcMultiGateClientLink.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FThread then
      FThread:=nil;
  end;

{ TRtcHttpMultiGateClient }

constructor TRtcHttpMultiGateClient.Create(AOwner: TComponent);
  begin
  inherited;
  FMultiGateClientLinks:=TRtcMultiGateClientLinkList.Create;

  ActiveClient:=TRtcHttpGateClient.Create(self);
  ActiveClient.AccountManager:=self;
  end;

destructor TRtcHttpMultiGateClient.Destroy;
  begin
  LogOutAndCleanUp;

  RtcFreeAndNil(ActiveClient);

  RemoveAllMultiGateClientLinks;
  RtcFreeAndNil(FMultiGateClientLinks);
  inherited;
  end;

function TRtcHttpMultiGateClient.GetActiveClient: TRtcHttpGateClient;
  begin
  Result:=ActiveClient;
  end;

function TRtcHttpMultiGateClient.GetAutoRelogin: boolean;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.AutoLogin
  else
    Result:=False;
  end;

function TRtcHttpMultiGateClient.GetState:TRtcGateClientStateInfo;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.State
  else
    Result:=nil;
  end;

function TRtcHttpMultiGateClient.GetMyIP:RtcString;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.MyIP
  else
    Result:='';
  end;

function TRtcHttpMultiGateClient.GetMyKey:RtcString;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.MyKey
  else
    Result:='';
  end;

function TRtcHttpMultiGateClient.GetMyUID:TGateUID;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.MyUID
  else
    Result:=0;
  end;

function TRtcHttpMultiGateClient.GetMyAddr:RtcString;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.MyAddr
  else
    Result:='';
  end;

function TRtcHttpMultiGateClient.GetClActive: boolean;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.Active
  else
    Result:=False;
  end;

function TRtcHttpMultiGateClient.GetClReady: boolean;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.Ready
  else
    Result:=False;
  end;

function TRtcHttpMultiGateClient.GetCryptPlugin: TRtcCryptPlugin;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.UseCryptPlugin
  else
    Result:=nil;
  end;

function TRtcHttpMultiGateClient.GetGateAddr: RtcString;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.GateAddr
  else
    Result:='';
  end;

function TRtcHttpMultiGateClient.GetGateFileName: RtcString;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.GateFileName
  else
    Result:='';
  end;

function TRtcHttpMultiGateClient.GetGatePort: RtcString;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.GatePort
  else
    Result:='';
  end;

function TRtcHttpMultiGateClient.GetGateIPV: RtcIPV;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.GateIPV
  else
    Result:=rtc_IPvDefault;
  end;

function TRtcHttpMultiGateClient.GetGatePrimaryKey: RtcString;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.GatePrimaryKey
  else
    Result:='';
  end;

function TRtcHttpMultiGateClient.GetGateSecondaryKey: RtcString;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.GateSecondaryKey
  else
    Result:='';
  end;

function TRtcHttpMultiGateClient.GetGateUserAuth: RtcString;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.GateUserAuth
  else
    Result:='';
  end;

function TRtcHttpMultiGateClient.GetGateUserInfo: RtcString;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.GateUserInfo
  else
    Result:='';
  end;

function TRtcHttpMultiGateClient.GetStreamBlockSizeIn: Cardinal;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.StreamBlockSizeIn
  else
    Result:=0;
  end;

function TRtcHttpMultiGateClient.GetStreamBlockSizeOut: Cardinal;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.StreamBlockSizeOut
  else
    Result:=0;
  end;

function TRtcHttpMultiGateClient.GetStreamSpeedLimit: Cardinal;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.StreamSpeedLimit
  else
    Result:=0;
  end;

function TRtcHttpMultiGateClient.GetUseBlocking: boolean;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.UseBlocking
  else
    Result:=False;
  end;

function TRtcHttpMultiGateClient.GetUseProxy: boolean;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.UseProxy
  else
    Result:=False;
  end;

function TRtcHttpMultiGateClient.GetUserLogin: TRtcUserLoginInfo;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.UserLogin
  else
    Result:=nil;
  end;

function TRtcHttpMultiGateClient.GetUseSSL: boolean;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.UseSSL
  else
    Result:=False;
  end;

function TRtcHttpMultiGateClient.GetUseWinHTTP: boolean;
  begin
  if assigned(ActiveClient) then
    Result:=ActiveClient.UseWinHTTP
  else
    Result:=False;
  end;

procedure TRtcHttpMultiGateClient.SetActiveClient(const Value: TRtcHttpGateClient);
  var
    ac:TRtcHttpGateClient;
  begin
  if assigned(Value) and (Value<>ActiveClient) then
    begin
    if Value.AccountManager<>self then
      Value.AccountManager:=self;
    ac:=ActiveClient; 
    ActiveClient:=Value;
    if assigned(ac) then
      if (ac.Active=False) and
         (ac.AutoLogin=False) then
        RtcFreeAndNil(ac);
    end;
  end;

procedure TRtcHttpMultiGateClient.SelectClient(UserID:TGateUID; const GateAddr, GatePort:RtcString);
  var
    ac,cli:TRtcHttpGateClient;
  begin
  if assigned(ActiveClient) then
    if (GatePort<>ActiveClient.GatePort) or
       (GateAddr<>ActiveClient.GateAddr) or
       (UserID<>ActiveClient.MyUID) then
      begin
      cli:=FindClient(UserID,GateAddr,GatePort);
      if assigned(cli) and (cli<>ActiveClient) then
        begin
        ac:=ActiveClient;
        ActiveClient:=cli;
        if (ac.Active=False) and
           (ac.AutoLogin=False) then
          RtcFreeAndNil(ac);
        end
      else
        begin
        if (ActiveClient.Active=True) or (ActiveClient.AutoLogin=True) then
          begin
          cli:=TRtcHttpGateClient.Create(self);
          cli.AccountManager:=self;
          cli.GateAddr:=GateAddr;
          cli.GatePort:=GatePort;
          cli.GateIPV:=ActiveClient.GateIPV;
          cli.GateFileName:=ActiveClient.GateFileName;
          cli.GatePrimaryKey:=ActiveClient.GatePrimaryKey;
          cli.GateSecondaryKey:=ActiveClient.GateSecondaryKey;
          cli.GateUserAuth:=ActiveClient.GateUserAuth;
          cli.GateUserInfo:=ActiveClient.GateUserInfo;
          cli.UseBlocking:=ActiveClient.UseBlocking;
          cli.UseProxy:=ActiveClient.UseProxy;
          cli.UseWinHTTP:=ActiveClient.UseWinHTTP;
          cli.UseSSL:=ActiveClient.UseSSL;
          cli.UseCryptPlugin:=ActiveClient.UseCryptPlugin;
          cli.UserLogin:=ActiveClient.UserLogin;
          cli.StreamBlockSizeIn:=ActiveClient.StreamBlockSizeIn;
          cli.StreamBlockSizeOut:=ActiveClient.StreamBlockSizeOut;
          cli.StreamSpeedLimit:=ActiveClient.StreamSpeedLimit;
          ActiveClient:=cli;
          end
        else
          begin
          ActiveClient.GatePort:=GatePort;
          ActiveClient.GateAddr:=GateAddr;
          end;
        end;
      end;
  end;

procedure TRtcHttpMultiGateClient.SelectClient(const UserAddr:RtcString);
  var
    GateAddr,GatePort:RtcString;
    UserID:TGateUID;
  begin
  if assigned(ActiveClient) then
    begin
    UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
    if UserID=0 then UserID:=ActiveClient.MyUID;
    SelectClient(UserID,GateAddr,GatePort);
    end;
  end;

procedure TRtcHttpMultiGateClient.SelectClient(const GateAddr,GatePort:RtcString);
  begin
  if assigned(ActiveClient) then
    SelectClient(ActiveClient.MyUID,GateAddr,GatePort);
  end;

procedure TRtcHttpMultiGateClient.SetGateAddr(const Value: RtcString);
  begin
  if assigned(ActiveClient) then
    SelectClient(ActiveClient.MyUID,Value,GatePort);
  end;

procedure TRtcHttpMultiGateClient.SetGatePort(const Value: RtcString);
  begin
  if assigned(ActiveClient) then
    SelectClient(ActiveClient.MyUID,GateAddr,Value);
  end;

procedure TRtcHttpMultiGateClient.SetGateIPV(const Value: RtcIPV);
  begin
  if assigned(ActiveClient) then
    ActiveClient.GateIPV:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetAutoRelogin(const Value: boolean);
  begin
  if assigned(ActiveClient) then
    ActiveClient.AutoLogin:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetClActive(const Value: boolean);
  begin
  if assigned(ActiveClient) then
    ActiveClient.Active:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetCryptPlugin(const Value: TRtcCryptPlugin);
  begin
  if assigned(ActiveClient) then
    ActiveClient.UseCryptPlugin:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetGateFileName(const Value: RtcString);
  begin
  if assigned(ActiveClient) then
    ActiveClient.GateFileName:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetGatePrimaryKey(const Value: RtcString);
  begin
  if assigned(ActiveClient) then
    ActiveClient.GatePrimaryKey:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetGateSecondaryKey(const Value: RtcString);
  begin
  if assigned(ActiveClient) then
    ActiveClient.GateSecondaryKey:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetGateUserAuth(const Value: RtcString);
  begin
  if assigned(ActiveClient) then
    ActiveClient.GateUserAuth:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetGateUserInfo(const Value: RtcString);
  begin
  if assigned(ActiveClient) then
    ActiveClient.GateUserInfo:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetStreamBlockSizeIn(const Value: Cardinal);
  begin
  if assigned(ActiveClient) then
    ActiveClient.StreamBlockSizeIn:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetStreamBlockSizeOut(const Value: Cardinal);
  begin
  if assigned(ActiveClient) then
    ActiveClient.StreamBlockSizeOut:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetStreamSpeedLimit(const Value: Cardinal);
  begin
  if assigned(ActiveClient) then
    ActiveClient.StreamSpeedLimit:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetUseBlocking(const Value: boolean);
  begin
  if assigned(ActiveClient) then
    ActiveClient.UseBlocking:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetUseProxy(const Value: boolean);
  begin
  if assigned(ActiveClient) then
    ActiveClient.UseProxy:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetUserLogin(const Value: TRtcUserLoginInfo);
  begin
  if assigned(ActiveClient) then
    ActiveClient.UserLogin:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetUseSSL(const Value: boolean);
  begin
  if assigned(ActiveClient) then
    ActiveClient.UseSSL:=Value;
  end;

procedure TRtcHttpMultiGateClient.SetUseWinHTTP(const Value: boolean);
  begin
  if assigned(ActiveClient) then
    ActiveClient.UseWinHTTP:=Value;
  end;

procedure TRtcHttpMultiGateClient.AddMultiGateClientLink(Value: TRtcAbsMultiGateClientLink);
  begin
  FMultiGateClientLinks.Add(Value);
  end;

procedure TRtcHttpMultiGateClient.RemoveAllMultiGateClientLinks;
  var
    Link:TRtcAbsMultiGateClientLink;
  begin
  Link:=FMultiGateClientLinks.LockFirst;
  while assigned(Link) do
    begin
    Link.RemoveMultiClient(self);
    FMultiGateClientLinks.LockNext(Link);
    end;
  end;

procedure TRtcHttpMultiGateClient.RemoveMultiGateClientLink(Value: TRtcAbsMultiGateClientLink);
  begin
  FMultiGateClientLinks.Remove(Value);
  end;

procedure TRtcHttpMultiGateClient.Call_AfterLoggedIn(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  inherited;
  FMultiGateClientLinks.DoAfterLoggedIn(Sender);
  end;

procedure TRtcHttpMultiGateClient.Call_AfterLoginFail(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  inherited;
  FMultiGateClientLinks.DoAfterLoginFail(Sender);
  end;

procedure TRtcHttpMultiGateClient.Call_AfterLogOut(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  inherited;
  FMultiGateClientLinks.DoAfterLogOut(Sender);
  end;

procedure TRtcHttpMultiGateClient.Call_BeforeLogIn(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  inherited;
  FMultiGateClientLinks.DoBeforeLogIn(Sender);
  end;

procedure TRtcHttpMultiGateClient.Call_OnDataReceived(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  inherited;
  FMultiGateClientLinks.DoDataReceived(Sender);
  end;

procedure TRtcHttpMultiGateClient.Call_OnInfoReceived(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  inherited;
  FMultiGateClientLinks.DoInfoReceived(Sender);
  end;

procedure TRtcHttpMultiGateClient.Call_OnReadyToSend(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  inherited;
  FMultiGateClientLinks.DoReadyToSend(Sender);
  end;

procedure TRtcHttpMultiGateClient.Call_OnReadyAfterReset(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  inherited;
  FMultiGateClientLinks.DoReadyAfterReset(Sender);
  end;

procedure TRtcHttpMultiGateClient.Call_OnStreamReset(Sender: TRtcConnection);
  begin
  if FAmSilent then Exit;
  inherited;
  FMultiGateClientLinks.DoStreamReset(Sender);
  end;

procedure TRtcHttpMultiGateClient.LogOutAndCleanUp;
  var
    cli:TRtcHttpGateClient;
  begin
  cli:=FirstClient;
  while assigned(cli) do
    begin
    Client:=cli;
    AutoLogin:=False;
    cli:=NextClient(Client);
    end;
  end;

function TRtcHttpMultiGateClient.AddUserToGroup(const OwnerAddr: RtcString; GroupID: TGateUID; const UserAddr: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID,OwnerID:TGateUID;
    GateAddr,GatePort,
    GateAddr2,GatePort2:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  OwnerID:=SplitUserAddr(OwnerAddr,GateAddr2,GatePort2);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') and
     (OwnerID>0) and (GateAddr2<>'') and (GatePort2<>'') and
     Same_Text(GateAddr,GateAddr2) and
     Same_Text(GatePort,GatePort2) then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.AddUserToGroup(OwnerID,GroupID,UserID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.RemUserFromGroup(const OwnerAddr: RtcString; GroupID: TGateUID; const UserAddr: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID,OwnerID:TGateUID;
    GateAddr,GatePort,
    GateAddr2,GatePort2:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  OwnerID:=SplitUserAddr(OwnerAddr,GateAddr2,GatePort2);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') and
     (OwnerID>0) and (GateAddr2<>'') and (GatePort2<>'') and
     Same_Text(GateAddr,GateAddr2) and
     Same_Text(GatePort,GatePort2) then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.RemUserFromGroup(OwnerID,GroupID,UserID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.RemoveGroup(const OwnerAddr: RtcString; GroupID: TGateUID): boolean;
  var
    cli:TRtcHttpGateClient;
    OwnerID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  OwnerID:=SplitUserAddr(OwnerAddr,GateAddr,GatePort);
  if (OwnerID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(OwnerID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.RemoveGroup(OwnerID,GroupID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.AddUserToMyGroup(GroupID:TGateUID; const UserAddr:RtcString):boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.AddUserToMyGroup(GroupID,UserID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.RemoveUserFromMyGroup(GroupID:TGateUID; const UserAddr:RtcString):boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.RemoveUserFromMyGroup(GroupID,UserID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.LeaveUsersGroup(const OwnerAddr:RtcString; GroupID:TGateUID):boolean;
  var
    cli:TRtcHttpGateClient;
    OwnerID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  OwnerID:=SplitUserAddr(OwnerAddr,GateAddr,GatePort);
  if (OwnerID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(OwnerID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.LeaveUsersGroup(OwnerID,GroupID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.DisbandMyGroup(GroupID:TGateUID):boolean;
  var
    cli:TRtcHttpGateClient;
  begin
  Result:=False;
  cli:=FirstClient(True);
  while assigned(cli) do
    begin
    try
      if cli.DisbandMyGroup(GroupID) then Result:=True;
    finally
      UnLockClient(cli);
      end;
    cli:=NextClient(cli,True);
    end;
  end;

function TRtcHttpMultiGateClient.IsUserInMyGroup(GroupID:TGateUID; const UserAddr:RtcString):boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.IsUserInMyGroup(GroupID,UserID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.UsersInMyGroup(GroupID:TGateUID):integer;
  var
    cli:TRtcHttpGateClient;
  begin
  Result:=0;
  cli:=FirstClient(True);
  while assigned(cli) do
    begin
    try
      Result:=Result+cli.UsersInMyGroup(GroupID);
    finally
      UnLockClient(cli);
      end;
    cli:=NextClient(cli,True);
    end;
  end;

function TRtcHttpMultiGateClient.SendToMyGroup(GroupID:TGateUID; CallID:word; const data:RtcByteArray):boolean;
  var
    cli:TRtcHttpGateClient;
  begin
  Result:=False;
  cli:=FirstClient(True);
  while assigned(cli) do
    begin
    try
      if cli.SendToMyGroup(GroupID,CallID,data) then Result:=True;
    finally
      UnLockClient(cli);
      end;
    cli:=NextClient(cli,True);
    end;
  end;

function TRtcHttpMultiGateClient.SendToMyGroup(GroupID:TGateUID; CallID:word; const data:RtcString):boolean;
  var
    cli:TRtcHttpGateClient;
  begin
  Result:=False;
  cli:=FirstClient(True);
  while assigned(cli) do
    begin
    try
      if cli.SendToMyGroup(GroupID,CallID,data) then Result:=True;
    finally
      UnLockClient(cli);
      end;
    cli:=NextClient(cli,True);
    end;
  end;

function TRtcHttpMultiGateClient.SendToMyGroup(GroupID:TGateUID; CallID:word):boolean;
  var
    cli:TRtcHttpGateClient;
  begin
  Result:=False;
  cli:=FirstClient(True);
  while assigned(cli) do
    begin
    try
      if cli.SendToMyGroup(GroupID,CallID) then Result:=True;
    finally
      UnLockClient(cli);
      end;
    cli:=NextClient(cli,True);
    end;
  end;

function TRtcHttpMultiGateClient.PingUser(const UserAddr: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.PingUser(UserID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.AddFriend(const UserAddr: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.AddFriend(UserID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.RemoveFriend(const UserAddr: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.RemoveFriend(UserID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.RemoveAllFriends(const MyAddr: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
  begin
  Result:=False;
  cli:=FindClient(MyAddr,True);
  if assigned(cli) then
    try
      Result:=cli.RemoveAllFriends;
    finally
      UnLockClient(cli);
      end;
  end;

function TRtcHttpMultiGateClient.RemoveAllMyFriends: boolean;
  var
    cli:TRtcHttpGateClient;
  begin
  Result:=False;
  cli:=FirstClient(True);
  while assigned(cli) do
    begin
    try
      Result:=cli.RemoveAllFriends;
    finally
      UnLockClient(cli);
      end;
    cli:=NextClient(cli,True);
    end;
  end;

function TRtcHttpMultiGateClient.RequestAccountVerification(const UserAddr, AccountID: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.RequestAccountVerification(UserID,AccountID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.SendAccountData(const UserAddr:RtcString; const ReceiverAuthCode:RtcString; const AccountData:RtcByteArray):boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.SendAccountData(UserID,ReceiverAuthCode,AccountData);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.RequestUserVerifications(const UserAddr: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.RequestUserVerifications(UserID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.SendBytes(const UserAddr: RtcString; GroupID: TGateUID; CallID: word; const data: RtcByteArray): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.SendBytes(UserID,GroupID,CallID,data);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.SendBytes(const UserAddr: RtcString; GroupID: TGateUID; CallID: word; const data: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.SendBytes(UserID,GroupID,CallID,data);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.SendBytes(const UserAddr: RtcString; GroupID: TGateUID; CallID: word): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.SendBytes(UserID,GroupID,CallID);
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.AccountNotVerified(const UserAddr, AccountID: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.AccountNotVerified[UserID,AccountID];
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.UnVerifiedAccount(const UserAddr, AccountID: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.UnVerifiedAccount[UserID,AccountID];
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.UnVerifiedAllowed(const UserAddr: RtcString; const action: word): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.UnVerifiedAllowed[UserID,action];
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.UserAccount(const UserAddr, AccountID: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.UserAccount[UserID,AccountID];
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.UserInfo(const UserAddr: RtcString): RtcString;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:='';
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.UserInfo[UserID];
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.UserAllowed(const UserAddr: RtcString; const action: word): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.UserAllowed[UserID,action];
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.UserNotVerified(const UserAddr: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.UserNotVerified[UserID];
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.VerifiedAccount(const UserAddr, AccountID: RtcString): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.VerifiedAccount[UserID,AccountID];
      finally
        UnLockClient(cli);
        end;
    end;
  end;

function TRtcHttpMultiGateClient.VerifiedAllowed(const UserAddr: RtcString; const action: word): boolean;
  var
    cli:TRtcHttpGateClient;
    UserID:TGateUID;
    GateAddr,GatePort:RtcString;
  begin
  Result:=False;
  UserID:=SplitUserAddr(UserAddr,GateAddr,GatePort);
  if (UserID>0) and (GateAddr<>'') and (GatePort<>'') then
    begin
    cli:=FindClient(UserID,GateAddr,GatePort,True);
    if assigned(cli) then
      try
        Result:=cli.VerifiedAllowed[UserID,action];
      finally
        UnLockClient(cli);
        end;
    end;
  end;

constructor TRtcGateUserInfo.Create;
  begin
  inherited;
  accs:=tStrIntList.Create(16);
  info:='';
  end;

destructor TRtcGateUserInfo.Destroy;
  begin
  RtcFreeAndNil(accs);
  info:='';
  inherited;
  end;

{ TRtcGCThread }

procedure TRtcGCThread.Post_AfterLoggedIn(Sender: TRtcConnection);
  begin
  Post(FRunAfterLoggedIn,FSynAfterLoggedIn,Sender);
  end;

procedure TRtcGCThread.Post_AfterLoggedIn(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  Post(FRunAfterLoggedIn,FSynAfterLoggedIn,Client,State);
  end;

procedure TRtcGCThread.Post_AfterLoginFail(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  Post(FRunAfterLoginFail,FSynAfterLoginFail,Client,State);
  end;

procedure TRtcGCThread.Post_AfterLoginFail(Sender: TRtcConnection);
  begin
  Post(FRunAfterLoginFail,FSynAfterLoginFail,Sender);
  end;

procedure TRtcGCThread.Post_AfterLogOut(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  Post(FRunAfterLogOut,FSynAfterLogOut,Client,State);
  end;

procedure TRtcGCThread.Post_AfterLogOut(Sender: TRtcConnection);
  begin
  Post(FRunAfterLogOut,FSynAfterLogOut,Sender);
  end;

procedure TRtcGCThread.Post_BeforeLogIn(Sender: TRtcConnection);
  begin
  Post(FRunBeforeLogIn,FSynBeforeLogIn,Sender);
  end;

procedure TRtcGCThread.Post_BeforeLogIn(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  Post(FRunBeforeLogIn,FSynBeforeLogIn,Client,State);
  end;

procedure TRtcGCThread.Post_DataReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
  begin
  Post(FRunDataReceived,FSynDataReceived,Client,Data);
  end;

procedure TRtcGCThread.Post_DataReceived(Sender: TRtcConnection);
  begin
  Post(FRunDataReceived,FSynDataReceived,Sender);
  end;

procedure TRtcGCThread.Post_InfoReceived(Client: TRtcHttpGateClient; Data: TRtcGateClientData);
  begin
  Post(FRunInfoReceived,FSynInfoReceived,Client,Data);
  end;

procedure TRtcGCThread.Post_InfoReceived(Sender: TRtcConnection);
  begin
  Post(FRunInfoReceived,FSynInfoReceived,Sender);
  end;

procedure TRtcGCThread.Post_ReadyAfterReset(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  Post(FRunReadyAfterReset,FSynReadyAfterReset,Client,State);
  end;

procedure TRtcGCThread.Post_ReadyAfterReset(Sender: TRtcConnection);
  begin
  Post(FRunReadyAfterReset,FSynReadyAfterReset,Sender);
  end;

procedure TRtcGCThread.Post_ReadyToSend(Sender: TRtcConnection);
  begin
  Post(FRunReadyToSend,FSynReadyToSend,Sender);
  end;

procedure TRtcGCThread.Post_ReadyToSend(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  Post(FRunReadyToSend,FSynReadyToSend,Client,State);
  end;

procedure TRtcGCThread.Post_StreamReset(Client: TRtcHttpGateClient; State: TRtcGateClientStateInfo);
  begin
  Post(FRunStreamReset,FSynStreamReset,Client,State);
  end;

procedure TRtcGCThread.Post_StreamReset(Sender: TRtcConnection);
  begin
  Post(FRunStreamReset,FSynStreamReset,Sender);
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; {$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcGateCli Finalizing ...','DEBUG');{$ENDIF}
CloseTimerPool;
{$IFDEF RTC_DEBUG} Log('rtcGateCli Finalized.','DEBUG');{$ENDIF}
end.
