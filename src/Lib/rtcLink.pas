{
  @html(<b>)
  Linked Objects
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This implementation is NOT specific to the RTC SDK, so it could be used by a
  3rd-Party to write a custom communication protocol for "RTC Linked Objects".
}
unit rtcLink;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSystem,
  rtcSrcList,
  rtcLog,

  rtcInfo;

const
  // @exclude
  RTCL_PFX_CREATE='C';
  // @exclude
  RTCL_PFX_DESTROY='D';
  // @exclude
  RTCL_PFX_SETPROP='P';
  // @exclude
  RTCL_PFX_SETEVENT='S';
  // @exclude
  RTCL_PFX_CALLEVENT='E';
  // @exclude
  RTCL_PFX_CALLMETHOD='M';
  // @exclude
  RTCL_PFX_BROADCAST='B';

  // @exclude
  RTCL_CLISESSION = '.$CLI-OBJM$.';
  // @exclude
  RTCL_SRVSESSION = '.$OBJM$.';
  // @exclude
  RTCL_FUNCTION = '.$.';
  // @exclude
  RTCL_DATA = '$';
  // @exclude
  RTCL_RESULT = '.';

type
  { @abstract(Basic RTC Object Link implementation)
    "TRtcBasicObjectLink" provides the basic event-based implementation of the TRtcObjectLink class. }
  TRtcBasicObjectLink = class(TRtcObjectLink)
  private
    FOnPropSet:TRtcObjectCallEvent;
    FOnEventSet:TRtcObjectCallEvent;
    FOnEventCall:TRtcObjectCallEvent;
    FOnMethodCall:TRtcObjectCallEvent;
    FOnBroadcast:TRtcObjectCallEvent;

  public
    { We have received information from our *remote* Linked Object counterpart
      that the *remote* Event "Param.xName" state is now "Param.asBoolean" (True=Active, False=Inactive).
      This will trigger the event "OnEventSet(Sender,Param);".
      [ *remote* Linked Object used the "SetEvent" method ] }
    procedure DoEventSet(Sender:TObject; xParam:TRtcObjectCall); override;

    { We have received a request from our *remote* Linked Object counterpart
      to Set our property "Param.xName" to value stored in "Param".
      This will trigger the event "OnPropSet(Sender,Param);".
      [ *remote* Linked Object used the "SetProp" method ] }
    procedure DoPropSet(Sender:TObject; xParam:TRtcObjectCall); override;

    { We have received a request from our *remote* Linked Object counterpart
      to Call our Event "Param.xName" using parameters stored in "Param".
      This will trigger the event "OnEventCall(Sender,Param);".
      [ *remote* Linked Object used the "CallEvent" method ] }
    procedure DoEventCall(Sender:TObject; xParam:TRtcObjectCall); override;

    { We have received a request from our *remote* Linked Object counterpart to:
      to Call our Method "Param.xName" using parameters stored in "Param".
      This will trigger the event "OnMethodCall(Sender,Param);".
      [ *remote* Linked Object used the "CallMethod" method ] }
    procedure DoMethodCall(Sender:TObject; xParam:TRtcObjectCall); override;

    { We have received a *local* Broadcast to
      Call our Method "Param.xName" using parameters stored in "Param".
      [ Linked Object used the "_Broadcast" method ] }
    procedure DoBroadcast(Sender:TObject; xParam:TRtcObjectCall); override;

    { We have received information from our *remote* Linked Object counterpart
      that the *remote* Event "Param.xName" state is now "Param.asBoolean" (True=Active, False=Inactive).
      [ *remote* Linked Object used the "SetEvent" method ] }
    property OnSetEvent:TRtcObjectCallEvent read FOnEventSet write FOnEventSet;

    { We have received a request from our *remote* Linked Object counterpart
      to Set our property "Param.xName" to value stored in "Param".
      [ *remote* Linked Object used the "SetProp" method ] }
    property OnSetProp:TRtcObjectCallEvent read FOnPropSet write FOnPropSet;

    { We have received a request from our *remote* Linked Object counterpart
      to Call our Event "Param.xName" using parameters stored in "Param".
      [ *remote* Linked Object used the "CallEvent" method ] }
    property OnCallEvent:TRtcObjectCallEvent read FOnEventCall write FOnEventCall;

    { We have received a request from our *remote* Linked Object counterpart to:
      to Call our Method "Param.xName" using parameters stored in "Param".
      [ *remote* Linked Object used the "CallMethod" method ] }
    property OnCallMethod:TRtcObjectCallEvent read FOnMethodCall write FOnMethodCall;

    { We have received a *local* Broadcast to
      Call our Method "Param.xName" using parameters stored in "Param".
      [ Linked Object used the "Broadcast" method ] }
    property OnBroadcast:TRtcObjectCallEvent read FOnBroadcast write FOnBroadcast;
    end;

  { @abstract(Custom RTC Object Link implementation)
    "TRtcCustomObjectLink" provides all the functionality required by RTC Linked Objects. }
  TRtcCustomObjectLink = class(TRtcBasicObjectLink)
  private
    FParam:TRtcObjectCall;

    function GetIsServer: boolean;
    function GetIsUpdating: boolean;

  public
    { xOwner = The Owner of this Link (will be destroyed if remote Object is destroyed)
      xManager = Object Manager where the object is being created.
        [ Example: MyCom:=TRtcObjectLink.Create(<the owner of this link>,GetRtcObjectManager); ] @html(<br><br>)
      NOTE: The Object will be linked to the current Thread's active Object Manager
        and the Object Managers "Get Next Object ID" method will be used to get the Object ID. @html(<br>)
      IMPORTANT: Linked Objects created in code also need to use the "Manager._RemoteCreate" method
        after creating a TRtcObjectLink instance to request the creation of a remote Linked Object. }
    constructor Create(xOwner:TObject; xManager:TRtcObjectManager); override;

    { Use "RemoteCreate" in combination with the "Param" property (below) to signal the remote
      Linked Object to create an instance of "xClassName" using parameters "Param".
      Call this method after "Create" to request the creation of a remote Linked Object.
        xClassName = ClassName registered on the remote side, used to create a connected object remotely.
        Param property (see below) = Parameters required by the remote Constructor. }
    procedure RemoteCreate(const xClassName:RtcWideString);

    { TRtcObjectLink object HAS TO BE destroyed by the Object which has created it.
      By destroying the TRtcObjectLink object, the remote side will also be notified and will destroy its copy. }
    destructor Destroy; override;

    { BeginUpdate and EndUpdate can be used in combination to
      postpone sending changes until all updates have been prepared }
    procedure BeginUpdate; virtual;

    { BeginUpdate and EndUpdate can be used in combination to
      postpone sending changes until all updates have been prepared }
    procedure EndUpdate; virtual;

    { Find OID for Object "xObject". Returns 0 if called with "xObject=nil",
      raises an Exception if "xObject<>nil" but Object was NOT found. }
    function FindOID(const xObject:TObject):TRtcObjectID; virtual;

    { Find Object with OID "xOID". Returns NIL if called with "xOID=0",
      raises an Exception if "xOID<>0" but Object was NOT found. }
    function FindObject(const xOID:TRtcObjectID):TObject; virtual;

    { Use "SetEvent" in combination with the "Param" property (below) to signal the remote
      Linked Object to set its "xEventName" event handler to the value in "Param".
      [ "DoEventSet(Sender, Param);" will be executed on the *remote* Linked Object ] }
    procedure SetEvent(const xEventName:RtcWideString);

    { Use "SetProp" in combination with the "Param" property (below) to signal the remote
      Linked Object to set its "xPropName" property (sent as "Param.xName") to value in "Param".
      If the property is being assigned a Linked Object, use "Param.asLinkedObject:=<Object>;".
      [ "DoPropSet(Sender, Param);" will be executed on the *remote* Linked Object ] }
    procedure SetProp(const xPropName:RtcWideString; xNow:boolean=False);

    { Use "CallEvent" in combination with the "Param" property (below) to call the remote
      Linked Objects event "xEventName" with parameters assigned to our "Param" property.
      [ "DoEventCall(Sender, Param);" will be executed on the *remote* Linked Object ] }
    procedure CallEvent(const xEventName:RtcWideString);

    { Use "CallMethod" in combination with the "Param" property (below) to call the remote
      Linked Objects method "xMethodName" with parameters assigned to our "Param" property.
      [ "DoMethodCall(Sender, Param);" will be executed on the *remote* Linked Object ] }
    procedure CallMethod(const xMethodName:RtcWideString);

    { Use "Broadcast" in combination with the "Param" property (below) to Broadcast
      the "xMethodName" with parameters from our "Param" property to all *local*
      Linked Objects currently subscribed to the channel "xChannel".
      [ "DoBroadcast(Sender, Param);" will be executed on all *local* subscribed Linked Objects ] }
    procedure Broadcast(const xChannel:RtcWideString; const xMethodName:RtcWideString);

    { Use "Param" before "SetEvent", "SetProp", "CallEvent", "CallMethod" and "Broadcast" to set Parameters for the next call.
      "Param" property will be cleared after each call to "SetEvent", "SetProp", "CallEvent", "CallMethod" and "Broadcast". }
    property Param:TRtcObjectCall read FParam;

    { "isServer" will be TRUE if the Object Manager was created on the Server
      where all Objects should be NON-Visual, FALSE if it was created on the Client. }
    property isServer:boolean read GetIsServer;

    { "isUpdating" returns TRUE if we are inside a BeginUpdate/EndUpdate block. }
    property isUpdating:boolean read GetIsUpdating;
    end;

  { @abstract(Used for implementing *Custom* Linked DataModules, Forms and Frames)
    Dropping this component on a DataModule, Form or a Frame will make the
    owner (DataModule, Form or a Frame) into a "RTC Linked Object".
    Only one instance of a TRtcLinkedModule may be used per "container". }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcLinkedModule = class(TRtc_Component)
  private
    FOLink:TRtcBasicObjectLink;

    FOnEventCall: TRtcObjectCallEvent;
    FOnEventSet: TRtcObjectCallEvent;
    FOnMethodCall: TRtcObjectCallEvent;
    FOnPropSet: TRtcObjectCallEvent;

    FParam: TRtcObjectCall;
    FRemoteClass: RtcWideString;
    FOnBroadcast: TRtcObjectCallEvent;

    function GetIsServer: boolean;
    function GetIsUpdating: boolean;

    procedure SetRemoteClass(const Value: RtcWideString);

  protected
    // @exclude
    procedure Loaded; override;

    { We have received information from our *remote* Linked Object counterpart
      that the *remote* Event "Param.xName" state is now "Param.asBoolean" (True=Active, False=Inactive).
      This will trigger the event "OnEventSet(Sender,Param);".
      [ *remote* Linked Object used the "SetEvent" method ] }
    procedure DoEventSet(Sender:TObject; xParam:TRtcObjectCall);

    { We have received a request from our *remote* Linked Object counterpart
      to Set our property "Param.xName" to value stored in "Param".
      This will trigger the event "OnPropSet(Sender,Param);".
      [ *remote* Linked Object used the "SetProp" method ] }
    procedure DoPropSet(Sender:TObject; xParam:TRtcObjectCall);

    { We have received a request from our *remote* Linked Object counterpart
      to Call our Event "Param.xName" using parameters stored in "Param".
      This will trigger the event "OnEventCall(Sender,Param);".
      [ *remote* Linked Object used the "CallEvent" method ] }
    procedure DoEventCall(Sender:TObject; xParam:TRtcObjectCall);

    { We have received a request from our *remote* Linked Object counterpart to:
      to Call our Method "Param.xName" using parameters stored in "Param".
      This will trigger the event "OnMethodCall(Sender,Param);".
      [ *remote* Linked Object used the "CallMethod" method ] }
    procedure DoMethodCall(Sender:TObject; xParam:TRtcObjectCall);

    { We have received a *local* Broadcast to
      call our Method "Param.xName" using parameters stored in "Param".
      [ Linked Object used the "Broadcast" method ] }
    procedure DoBroadcast(Sender:TObject; xParam:TRtcObjectCall);

  public
    { Standard constructor, used at design-time and runtime }
    constructor Create(AOwner:TComponent); override;

    { Standard component destructor. }
    destructor Destroy; override;

    { BeginUpdate and EndUpdate can be used in combination to
      postpone sending changes until all updates have been prepared }
    procedure BeginUpdate;

    { BeginUpdate and EndUpdate can be used in combination to
      postpone sending changes until all updates have been prepared }
    procedure EndUpdate;

    { Subscribe to receive *local* Broadcasts on channel "xChannel" }
    procedure Subscribe(const xChannel:RtcWideString); virtual;

    { Unsubscribe from receiving *local* Broadcasts on channel "xChannel".
      All subscriptions will be automatically removed (unsubsribed) when the object is destroyed. }
    procedure Unsubscribe(const xChannel:RtcWideString); virtual;

    { Unsubscribe from receiving *local* Broadcasts on ALL channels (remove ALL subscriptions). }
    procedure UnsubscribeAll; virtual;

    { Find OID for Object "xObject". Returns 0 if called with "xObject=nil",
      raises an Exception if "xObject<>nil" but Object was NOT found. }
    function FindOID(const xObject:TObject):TRtcObjectID; virtual;

    { Find Object with OID "xOID". Returns NIL if called with "xOID=0",
      raises an Exception if "xOID<>0" but Object was NOT found. }
    function FindObject(const xOID:TRtcObjectID):TObject; virtual;

    { Use "SetEvent" to signal the remote Linked Object that a handler for *our*
      event "xEventName" was assigned (Param.asBoolean=True) or removed (Param.asBoolean=False).
      [ "DoEventSet(xEventName, xActive);" will be executed on the *remote* Linked Object ] }
    procedure SetEvent(const xEventName:RtcWideString);

    { Use "SetProp" in combination with the "Param" property (below) to signal the remote
      Linked Object to set its "xPropName" property to the value set in our "Param" property.
      If the property is being assigned a Linked Object, use "Param.asLinkedObject:=<Object>;".
      [ "DoPropSet(xPropName, Param);" will be executed on the *remote* Linked Object ] }
    procedure SetProp(const xPropName:RtcWideString; xNow:boolean=False);

    { Use "CallEvent" in combination with the "Param" property (below) to call the remote
      Linked Objects event "xEventName" with parameters assigned to our "Param" property.
      [ "DoEventCall(xEventName, Param);" will be executed on the *remote* Linked Object ] }
    procedure CallEvent(const xEventName:RtcWideString);

    { Use "CallMethod" in combination with the "Param" property (below) to call the remote
      Linked Objects method "xMethodName" with parameters assigned to our "Param" property.
      [ "DoMethodCall(xMethodName, Param);" will be executed on the *remote* Linked Object ] }
    procedure CallMethod(const xMethodName:RtcWideString);

    { Use "Broadcast" in combination with the "Param" property (below) to Broadcast
      the "xMethodName" with parameters from our "Param" property to all *local*
      Linked Objects currently subscribed to the channel "xChannel".
      [ "DoBroadcast(Sender, Param);" will be executed on all *local* subscribed Linked Objects ] }
    procedure Broadcast(const xChannel:RtcWideString; const xMethodName:RtcWideString);

    { Use "Param" before "SetEvent", "SetProp", "CallEvent", "CallMethod" and "Broadcast" to prepare Parameters.
      "Param.Manager" is set to the "Object Manager" responsible for this component (at all times).
      "Param" values are cleared after "SetEvent", "SetProp", "CallEvent", "CallMethod" and "Broadcast" calls. }
    property Param:TRtcObjectCall read FParam;

    { "isServer" will be TRUE if the Object Manager was created on the Server
      where all Objects should be NON-Visual, FALSE if it was created on the Client. }
    property isServer:boolean read GetIsServer;

    { "isUpdating" returns TRUE if we are inside a BeginUpdate/EndUpdate block. }
    property isUpdating:boolean read GetIsUpdating;

  published
    { Name of the Remote Class (sent as "xClassName" to the remote side).
      If this property is NOT set to the RtcWideString known to the remote side,
      the remote Linked Data Module will NOT be created !!! }
    property RemoteClass:RtcWideString read FRemoteClass write SetRemoteClass;

    { We have received information from our *remote* Linked Object counterpart
      that the *remote* Event "Param.xName" state is now "Param.asBoolean" (True=Active, False=Inactive).
      [ *remote* Linked Object used the "SetEvent" method ] }
    property OnSetEvent:TRtcObjectCallEvent read FOnEventSet write FOnEventSet;

    { We have received a request from our *remote* Linked Object counterpart
      to Set our property "Param.xName" to value stored in "Param".
      [ *remote* Linked Object used the "SetProp" method ] }
    property OnSetProp:TRtcObjectCallEvent read FOnPropSet write FOnPropSet;

    { We have received a request from our *remote* Linked Object counterpart
      to Call our Event "Param.xName" using parameters stored in "Param".
      [ *remote* Linked Object used the "CallEvent" method ] }
    property OnCallEvent:TRtcObjectCallEvent read FOnEventCall write FOnEventCall;

    { We have received a request from our *remote* Linked Object counterpart
      to Call our Method "Param.xName" using parameters stored in "Param".
      [ *remote* Linked Object used the "CallMethod" method ] }
    property OnCallMethod:TRtcObjectCallEvent read FOnMethodCall write FOnMethodCall;

    { We have received a *local* Broadcast on channel "Param.xChannel" to
      Call our Method "Param.xName" using parameters stored in "Param".
      [ Linked Object used the "Broadcast" method ] }
    property OnBroadcast:TRtcObjectCallEvent read FOnBroadcast write FOnBroadcast;
    end;

type
  { @abstract(Remote RTC Object Manager definition/abstract class)
    All methods with a "_" prefix can be used for sending data to *remote* Linked Objects. }
  TRtcRemoteObjectManager = class(TRtcObjectManager)
  private
    FActive:boolean;
    FData:TRtcValue;
    FPrep:TRtcRecord;
    FExRes:TRtcArray;
    FUpdateSig:boolean;
    FExecuting:boolean;
    FUpdating:boolean;

    FBroadcastGroup:RtcWideString;
    FChannels:tStringObjList;
    FBroad:TRtcValue;
    FHaveBroad:boolean;

    procedure Prep(xParam:TRtcObjectCall; xNow:boolean=True);

    procedure PrepToData;
    procedure SetBroadcastGroup(const Value: RtcWideString);

  protected
    procedure Execute_BadFormat(const xMessage:RtcWideString); virtual;
    procedure Execute_ObjectNotFound(xOID:TRtcObjectID; const xMessage:RtcWideString); virtual;
    procedure Execute_NoConstructor(const xClassName:RtcWideString; const xMessage:RtcWideString); virtual;
    procedure Execute_Exception(const E:Exception; const xMessage:RtcWideString); virtual;

    procedure DoCreate(Sender:TObject; xOID:TRtcObjectID; xParam:TRtcObjectCall); virtual;
    procedure DoDestroy(Sender:TObject; xOID:TRtcObjectID); virtual;
    procedure DoPropSet(Sender:TObject; xOID:TRtcObjectID; xParam:TRtcObjectCall); virtual;
    procedure DoEventSet(Sender:TObject; xOID:TRtcObjectID; xParam:TRtcObjectCall); virtual;
    procedure DoEventCall(Sender:TObject; xOID:TRtcObjectID; xParam:TRtcObjectCall); virtual;
    procedure DoMethodCall(Sender:TObject; xOID:TRtcObjectID; xParam:TRtcObjectCall); virtual;
    procedure DoBroadcast(Sender:TObject; xOID:TRtcObjectID; xParam:TRtcObjectCall); virtual;

    { "DataReady" method will be called when Data is ready for sending.
      Use the "GetData" method to retrieve Data which has to be sent.
      After "GetData" is used to retrieve the Data, buffers will be cleared
      and "DataReady" will be called again when more Data is ready for sending.
      If "DataReady" method does NOT call "GetData", there will be NO consecutive
      calls to "DataReady" if more data is being added before "GetData" is called. }
    procedure DataReady; virtual; abstract;

    procedure _RemoteDestroy(const xLink:TRtcObjectLink); override;

    procedure _Subscribe(const xLink:TRtcObjectLink; const xChannel:RtcWideString); override;
    procedure _Unsubscribe(const xLink:TRtcObjectLink; const xChannel:RtcWideString); override;

    procedure AddBroadcast(const xChannel:RtcWideString; xParam:TRtcObjectCall);

  public
    constructor Create(xServer:boolean); override;
    destructor Destroy; override;

    procedure _RemoteCreate(const xLink:TRtcObjectLink; xParam:TRtcObjectCall); override;
    procedure _SetProp(const xLink:TRtcObjectLink; xParam:TRtcObjectCall; xNow:boolean=False); override;
    procedure _SetEvent(const xLink:TRtcObjectLink; xParam:TRtcObjectCall); override;
    procedure _CallEvent(const xLink:TRtcObjectLink; xParam:TRtcObjectCall); override;
    procedure _CallMethod(const xLink:TRtcObjectLink; xParam:TRtcObjectCall); override;
    procedure _Broadcast(const xChannel:RtcWideString; xParam:TRtcObjectCall); override;

    procedure EndUpdate; override;

    { Get Data prepared for sending.
      Sending buffer will be cleared after this call.
      You need to free the TRtcValue object received. }
    function GetData:TRtcValue; virtual;

    { Get all buffered Broadcast calls.
      Broadcast buffers will be cleared after this call.
      You need to free the TRtcValue object received. }
    function GetBroadcast:TRtcValue; virtual;

    { Return TRUE if this Object Manager is idle (no data waiting to be sent) }
    function Idle: boolean;

    { Execute "xData" and return NIL if everything was OK.
      Returns an Array of exceptions if there were errors.
      "xData" might be changed after this call, but it will NOT be freed. }
    function ExecuteNow(Sender:TObject; const xData:TRtcValueObject):TRtcArray; virtual;

    { This is a combination of ExecuteNow(GetBroadcast) and ExecuteNow, returning a single result }
    function ExecuteWithBroadcast(Sender:TObject; const xData:TRtcValueObject):TRtcArray; virtual;

    { This calls ExecuteNow(GetBroadcast) and leaves all error messages in the error message queue.
      If called with "xData" parameter assigned, "xData" will be executed instead of calling "GetBroadcast". }
    procedure ExecuteBroadcast(Sender:TObject; const xData:TRtcValueObject=nil); virtual;

    { Broadcast Group name. When using the "Broadcast" method, method calls are
      broadcasted to all Object Managers belonging to the same "BroadcastGroup".
      Using a Broadcast Group avoids broadcasting to Linked Objects belonging
      to Object Managers running inside different Applications. When used with
      TRtcClientModule and TRtcServerModule components, BroadcastGroup
      property is automatically set to the "ModuleName" property. }
    property BroadcastGroup:RtcWideString read FBroadcastGroup write SetBroadcastGroup;
    end;

  TRtcRemoteObjectManagerNotify = procedure(Sender:TRtcRemoteObjectManager) of object;

  { @abstract(Basic Remote RTC Object Manager implementation) }
  TRtcBasicRemoteObjectManager = class(TRtcRemoteObjectManager)
  private
    FOnDataReady:TRtcRemoteObjectManagerNotify;
    FOnObjectCreate:TRtcObjectCallEvent;
  protected
    procedure DataReady; override;
    procedure DoCreate(Sender:TObject; xOID:TRtcObjectID; xParam:TRtcObjectCall); override;
  public
    { "OnDataReady" will be called when Data is ready for sending.
      Use the "GetData" method to retrieve Data which has to be sent.
      After "GetData" is used to retrieve the Data, buffers will be cleared
      and "OnDataReady" will be called again when more Data is ready for sending.
      If "OnDataReady" method does NOT call "GetData", there will be NO consecutive
      calls to "OnDataReady" even if more data is added -> until "GetData" is called. }
    property OnDataReady:TRtcRemoteObjectManagerNotify read FOnDataReady write FOnDataReady;
    { "OnObjectCreate" will be called when the remote Object Manager has requested
      our Object Manager to create a new Object (remote instance was already created).
      The event allows you to create Objects which don't have a global constructor
      registered (using the global "RegisterRtcObjectConstructor" procedure),
      or ... to prevent the creation of globally registered objects by raising an exception. }
    property OnObjectCreate:TRtcObjectCallEvent read FOnObjectCreate write FOnObjectCreate;
    end;

  { @abstract(Local RTC Object Manager implementation)
    All methods with a "_" prefix can be used for sending data to *remote* Linked Objects. }
  TRtcLocalObjectManager = class(TRtcRemoteObjectManager)
  private
    FRemote:TRtcLocalObjectManager;
    FLocal:boolean;

  {$IFDEF FPC}public{$ENDIF}

    // Used internally by TRtcLocalObjectManager to create a "remote" Object Manager
    constructor CreateLinkedManager(xRemote:TRtcLocalObjectManager; xServer:boolean);

  protected
    property Remote:TRtcLocalObjectManager read FRemote;

    procedure DataReady; override;

  public
    constructor Create(xServer:boolean); override;
    destructor Destroy; override;
    end;

implementation

{ TRtcBasicObjectLink }

procedure TRtcBasicObjectLink.DoPropSet(Sender:TObject; xParam:TRtcObjectCall);
  begin
  if assigned(FOnPropSet) then
    FOnPropSet(Sender,xParam);
  end;

procedure TRtcBasicObjectLink.DoEventSet(Sender:TObject; xParam:TRtcObjectCall);
  begin
  if assigned(FOnEventSet) then
    FOnEventSet(Sender,xParam);
  end;

procedure TRtcBasicObjectLink.DoEventCall(Sender:TObject; xParam:TRtcObjectCall);
  begin
  if assigned(FOnEventCall) then
    FOnEventCall(Sender,xParam);
  end;

procedure TRtcBasicObjectLink.DoMethodCall(Sender:TObject; xParam:TRtcObjectCall);
  begin
  if assigned(FOnMethodCall) then
    FOnMethodCall(Sender,xParam);
  end;

procedure TRtcBasicObjectLink.DoBroadcast(Sender: TObject; xParam: TRtcObjectCall);
  begin
  if assigned(FOnBroadcast) then
    FOnBroadcast(Sender,xParam);
  end;

{ TRtcCustomObjectLink }

constructor TRtcCustomObjectLink.Create(xOwner:TObject;xManager:TRtcObjectManager);
  begin
  inherited Create(xOwner,xManager);
  FParam:=TRtcObjectCall.Create(xManager);
  end;

destructor TRtcCustomObjectLink.Destroy;
  begin
  RtcFreeAndNil(FParam);
  inherited;
  end;

procedure TRtcCustomObjectLink.RemoteCreate(const xClassName: RtcWideString);
  begin
  if assigned(Manager) and assigned(FParam) then
    begin
    FParam.xName:=xClassName;
    Manager._RemoteCreate(self,FParam);
    end;
  end;

procedure TRtcCustomObjectLink.SetEvent(const xEventName: RtcWideString);
  begin
  if assigned(Manager) and assigned(FParam) then
    begin
    FParam.xName:=xEventName;
    Manager._SetEvent(self,FParam);
    end;
  end;

procedure TRtcCustomObjectLink.CallEvent(const xEventName: RtcWideString);
  begin
  if assigned(Manager) and assigned(FParam) then
    begin
    FParam.xName:=xEventName;
    Manager._CallEvent(self,FParam);
    end;
  end;

procedure TRtcCustomObjectLink.CallMethod(const xMethodName: RtcWideString);
  begin
  if assigned(Manager) and assigned(FParam) then
    begin
    FParam.xName:=xMethodName;
    Manager._CallMethod(self,FParam);
    end;
  end;

procedure TRtcCustomObjectLink.SetProp(const xPropName: RtcWideString; xNow: boolean);
  begin
  if assigned(Manager) and assigned(FParam) then
    begin
    FParam.xName:=xPropName;
    Manager._SetProp(self,FParam,xNow);
    end;
  end;

procedure TRtcCustomObjectLink.Broadcast(const xChannel, xMethodName: RtcWideString);
  begin
  if assigned(Manager) and assigned(FParam) then
    begin
    FParam.xName:=xMethodName;
    Manager._Broadcast(xChannel,FParam);
    end;
  end;

procedure TRtcCustomObjectLink.BeginUpdate;
  begin
  if assigned(Manager) then
    Manager.BeginUpdate;
  end;

procedure TRtcCustomObjectLink.EndUpdate;
  begin
  if assigned(Manager) then
    Manager.EndUpdate;
  end;

function TRtcCustomObjectLink.GetIsServer: boolean;
  begin
  if assigned(Manager) then
    Result:=Manager.isServer
  else
    Result:=False;
  end;

function TRtcCustomObjectLink.GetIsUpdating: boolean;
  begin
  if assigned(Manager) then
    Result:=Manager.isUpdating
  else
    Result:=False;
  end;

function TRtcCustomObjectLink.FindObject(const xOID: TRtcObjectID): TObject;
  begin
  if assigned(Manager) then
    Result:=Manager.FindObject(xOID)
  else
    Result:=nil;
  end;

function TRtcCustomObjectLink.FindOID(const xObject: TObject): TRtcObjectID;
  begin
  if assigned(Manager) then
    Result:=Manager.FindOID(xObject)
  else
    Result:=RTC_NIL_OBJECT_ID;
  end;

{ TRtcLinkedModule }

constructor TRtcLinkedModule.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
    begin
    // This will only work if there is an active Object Manager for the current thread
    FParam:=TRtcObjectCall.Create(GetRtcObjectManager);
    if assigned(AOwner) then
      FOLink:=TRtcBasicObjectLink.Create(AOwner,FParam.Manager)
    else // If no Owner, we are the Object container
      FOLink:=TRtcBasicObjectLink.Create(self,FParam.Manager);
    FOLink.OnSetProp:=DoPropSet;
    FOLink.OnSetEvent:=DoEventSet;
    FOLink.OnCallEvent:=DoEventCall;
    FOLink.OnCallMethod:=DoMethodCall;
    FOLink.OnBroadcast:=DoBroadcast;
    end;
  end;

destructor TRtcLinkedModule.Destroy;
  begin
  RtcFreeAndNil(FOLink);
  RtcFreeAndNil(FParam);
  inherited;
  end;

procedure TRtcLinkedModule.SetRemoteClass(const Value: RtcWideString);
  begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    FRemoteClass := Value
  else if assigned(FOLink) and (Value<>'') then
    begin
    if FOLink.isCreator and (FRemoteClass='') then
      begin
      FRemoteClass:=Value;
      FParam.xName:=FRemoteClass;
      FParam.isNull:=True;
      FOLink.Manager._RemoteCreate(FOLink,FParam);
      end;
    end;
  end;

procedure TRtcLinkedModule.Loaded;
  begin
  inherited;
  if assigned(FOLink) then
    if FOLink.isCreator and (FRemoteClass<>'') then
      begin
      FParam.xName:=FRemoteClass;
      FParam.isNull:=True;
      FOLink.Manager._RemoteCreate(FOLink,FParam);
      end;
  end;

procedure TRtcLinkedModule.DoPropSet(Sender:TObject; xParam:TRtcObjectCall);
  begin
  if assigned(FOnPropSet) then
    FOnPropSet(Sender,xParam);
  end;

procedure TRtcLinkedModule.DoEventSet(Sender:TObject; xParam:TRtcObjectCall);
  begin
  if assigned(FOnEventSet) then
    FOnEventSet(Sender,xParam);
  end;

procedure TRtcLinkedModule.DoEventCall(Sender:TObject; xParam:TRtcObjectCall);
  begin
  if assigned(FOnEventCall) then
    FOnEventCall(Sender,xParam);
  end;

procedure TRtcLinkedModule.DoMethodCall(Sender:TObject; xParam:TRtcObjectCall);
  begin
  if assigned(FOnMethodCall) then
    FOnMethodCall(Sender,xParam);
  end;

procedure TRtcLinkedModule.DoBroadcast(Sender:TObject; xParam:TRtcObjectCall);
  begin
  if assigned(FOnBroadcast) then
    FOnBroadcast(Sender,xParam);
  end;

procedure TRtcLinkedModule.SetEvent(const xEventName: RtcWideString);
  begin
  if assigned(FOLink) then
    if assigned(FOLink.Manager) then
      begin
      FParam.xName:=xEventName;
      FOLink.Manager._SetEvent(FOLink,FParam);
      end;
  end;

procedure TRtcLinkedModule.CallEvent(const xEventName: RtcWideString);
  begin
  if assigned(FOLink) then
    if assigned(FOLink.Manager) then
      begin
      FParam.xName:=xEventName;
      FOLink.Manager._CallEvent(FOLink,FParam);
      end;
  end;

procedure TRtcLinkedModule.CallMethod(const xMethodName: RtcWideString);
  begin
  if assigned(FOLink) then
    if assigned(FOLink.Manager) then
      begin
      FParam.xName:=xMethodName;
      FOLink.Manager._CallMethod(FOLink,FParam);
      end;
  end;

procedure TRtcLinkedModule.SetProp(const xPropName: RtcWideString; xNow: boolean);
  begin
  if assigned(FOLink) then
    if assigned(FOLink.Manager) then
      begin
      FParam.xName:=xPropName;
      FOLink.Manager._SetProp(FOLink,FParam,xNow);
      end;
  end;

procedure TRtcLinkedModule.Broadcast(const xChannel, xMethodName: RtcWideString);
  begin
  if assigned(FOLink) then
    if assigned(FOLink.Manager) then
      begin
      FParam.xName:=xMethodName;
      FOLink.Manager._Broadcast(xChannel,FParam);
      end;
  end;

procedure TRtcLinkedModule.Subscribe(const xChannel: RtcWideString);
  begin
  if assigned(FOLink) then
    if assigned(FOLink.Manager) then
      FOLink.Subscribe(xChannel);
  end;

procedure TRtcLinkedModule.Unsubscribe(const xChannel: RtcWideString);
  begin
  if assigned(FOLink) then
    if assigned(FOLink.Manager) then
      FOLink.Unsubscribe(xChannel);
  end;

procedure TRtcLinkedModule.UnsubscribeAll;
  begin
  if assigned(FOLink) then
    if assigned(FOLink.Manager) then
      FOLink.UnsubscribeAll;
  end;

procedure TRtcLinkedModule.BeginUpdate;
  begin
  if assigned(FOLink) then
    if assigned(FOLink.Manager) then
      FOLink.Manager.BeginUpdate;
  end;

procedure TRtcLinkedModule.EndUpdate;
  begin
  if assigned(FOLink) then
    if assigned(FOLink.Manager) then
      FOLink.Manager.EndUpdate;
  end;

function TRtcLinkedModule.GetIsServer: boolean;
  begin
  if assigned(FOLink) then
    begin
    if assigned(FOLink.Manager) then
      Result:=FOLink.Manager.isServer
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcLinkedModule.GetIsUpdating: boolean;
  begin
  if assigned(FOLink) then
    begin
    if assigned(FOLink.Manager) then
      Result:=FOLink.Manager.isUpdating
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcLinkedModule.FindObject(const xOID: TRtcObjectID): TObject;
  begin
  if assigned(FOLink) then
    begin
    if assigned(FOLink.Manager) then
      Result:=FOLink.Manager.FindObject(xOID)
    else
      Result:=RTC_NIL_OBJECT;
    end
  else
    Result:=RTC_NIL_OBJECT;
  end;

function TRtcLinkedModule.FindOID(const xObject: TObject): TRtcObjectID;
  begin
  if assigned(FOLink) then
    begin
    if assigned(FOLink.Manager) then
      Result:=FOLink.Manager.FindOID(xObject)
    else
      Result:=RTC_NIL_OBJECT_ID;
    end
  else
    Result:=RTC_NIL_OBJECT_ID;
  end;

{ TRtcRemoteObjectManager }

constructor TRtcRemoteObjectManager.Create(xServer:boolean);
  begin
  inherited;
  FBroad:=nil;
  FHaveBroad:=False;
  FData:=nil;
  FPrep:=nil;
  FExRes:=nil;
  FActive:=True;
  end;

destructor TRtcRemoteObjectManager.Destroy;
  begin
  FActive:=False;
  FUpdateSig:=False;
  FHaveBroad:=False;

  FBroad.Free;
  FData.Free;
  FPrep.Free;
  FExRes.Free;
  inherited;
  end;

procedure TRtcRemoteObjectManager.Prep(xParam:TRtcObjectCall; xNow: boolean=True);
  begin
  if xParam.asObject=nil then
    xParam.asObject:=TRtcValue.Create; // NULL value
  try
    if assigned(FPrep) then
      begin
      if xNow then
        begin
        if FPrep.asObject[xParam.xName]=nil then
          FPrep.asObject[xParam.xName]:=xParam.asObject
        else
          begin
          PrepToData;
          FPrep:=TRtcRecord.Create;
          FPrep.asObject[xParam.xName]:=xParam.asObject;
          end;
        end
      else
        FPrep.asObject[xParam.xName]:=xParam.asObject;
      end
    else
      begin
      FPrep:=TRtcRecord.Create;
      FPrep.asObject[xParam.xName]:=xParam.asObject;
      end;
  finally
    xParam.asObject:=nil;
    end;

  if (FUpdateSig=False) and (isUpdating=False) then
    begin
    FUpdateSig:=True;
    if FExecuting=False then
      begin
      FExecuting:=True;
      try
        repeat
          FUpdating:=False;
          DataReady;
          until FUpdating=False;
      finally
        FExecuting:=False;
        end;
      end
    else
      FUpdating:=True;
    end;
  end;

procedure TRtcRemoteObjectManager.PrepToData;
  var
    arr:TRtcArray;
  begin
  if assigned(FPrep) then
    begin
    if assigned(FData) then
      begin
      if FData.isType<>rtc_Array then
        begin
        arr:=TRtcArray.Create;
        arr.asObject[0]:=FData.asObject;
        FData.asObject:=nil;
        FData.asObject:=arr;
        end;
      with FData.asArray do
        asObject[Count]:=FPrep;
      FPrep:=nil;
      end
    else
      begin
      FData:=TRtcValue.Create;
      FData.asObject:=FPrep;
      FPrep:=nil;
      end;
    end;
  end;

function TRtcRemoteObjectManager.GetData:TRtcValue;
  begin
  { Only return Data if this is the first call to "GetData"
    after our call to "DataReady" ("Update Signal" is still set). }
  if FUpdateSig then
    begin
    PrepToData;
    Result:=FData; FData:=nil;
    { Clear "Update Signal" to make sure the "DataReady" method will be
      called automatically again once more data is prepared for sending. }
    FUpdateSig:=False;
    end
  else
    Result:=nil;
  end;

function TRtcRemoteObjectManager.Idle: boolean;
  begin
  { "Update Signal" will be set when more data is ready for sending. }
  Result:= FUpdateSig=False;
  end;

procedure TRtcRemoteObjectManager.EndUpdate;
  begin
  if FActive then
    begin
    inherited;
    if (FUpdateSig=False) and (isUpdating=False) then
      if assigned(FPrep) or assigned(FData) then
        begin
        FUpdateSig:=True;
        if FExecuting=False then
          begin
          FExecuting:=True;
          try
            repeat
              FUpdating:=False;
              DataReady;
              until FUpdating=False;
          finally
            FExecuting:=False;
            end;
          end
        else
          FUpdating:=True;
        end;
    end;
  end;

procedure TRtcRemoteObjectManager._RemoteDestroy(const xLink: TRtcObjectLink);
  var
    xParam:TRtcObjectCall;
  begin
  if FActive then
    begin
    xParam:=TRtcObjectCall.Create(self);
    try
      xParam.xName:=RTCL_PFX_DESTROY+Int2WStr(xLink.OID);
      Prep(xParam);
    finally
      xParam.Free;
      end;
    end;
  end;

procedure TRtcRemoteObjectManager._RemoteCreate(const xLink: TRtcObjectLink; xParam:TRtcObjectCall);
  begin
  if FActive then
    begin
    xParam.xName:=RTCL_PFX_CREATE+Int2WStr(xLink.OID)+xParam.xName;
    Prep(xParam);
    end
  else
    xParam.isNull:=True;
  end;

procedure TRtcRemoteObjectManager._SetProp(const xLink: TRtcObjectLink; xParam:TRtcObjectCall; xNow: boolean);
  begin
  if FActive then
    begin
    xParam.xName:=RTCL_PFX_SETPROP+Int2WStr(xLink.OID)+xParam.xName;
    Prep(xParam,xNow);
    end
  else
    xParam.isNull:=True;
  end;

procedure TRtcRemoteObjectManager._SetEvent(const xLink: TRtcObjectLink; xParam:TRtcObjectCall);
  begin
  if FActive then
    begin
    xParam.xName:=RTCL_PFX_SETEVENT+Int2WStr(xLink.OID)+xParam.xName;
    Prep(xParam);
    end
  else
    xParam.isNull:=True;
  end;

procedure TRtcRemoteObjectManager._CallEvent(const xLink: TRtcObjectLink; xParam:TRtcObjectCall);
  begin
  if FActive then
    begin
    xParam.xName:=RTCL_PFX_CALLEVENT+Int2WStr(xLink.OID)+xParam.xName;
    Prep(xParam);
    end
  else
    xParam.isNull:=True;
  end;

procedure TRtcRemoteObjectManager._CallMethod(const xLink: TRtcObjectLink; xParam:TRtcObjectCall);
  begin
  if FActive then
    begin
    xParam.xName:=RTCL_PFX_CALLMETHOD+Int2WStr(xLink.OID)+xParam.xName;
    Prep(xParam);
    end
  else
    xParam.isNull:=True;
  end;

function TRtcRemoteObjectManager.ExecuteNow(Sender:TObject; const xData: TRtcValueObject):TRtcArray;
  procedure ExecuteRec(rec:TRtcRecord);
    var
      i:integer;
      fn:RtcWideString;
      xOID:TRtcObjectID;
      xName:RtcWideString;
      xVal:TRtcObjectCall;
    function SeparateFN:boolean;
      var
        a,l:integer;
      begin
      Result:=False;
      l:=length(fn);
      if l<2 then
        Execute_BadFormat('ExecuteNow['+Int2WStr(i)+'].'+fn)
      else
        begin
        a:=1;
        // Do we have a negative ID? (created by a Remote Object Manager)
        if fn[2]='-' then
          Inc(a);
        // Scan to include the complete ID
        while (a<l) and (fn[a+1]>='0') and (fn[a+1]<='9') do
          Inc(a);
        if a<=1 then
          Execute_BadFormat('ExecuteNow['+Int2WStr(i)+'].'+fn)
        else if a=l then
          begin
          xOID:=StrToInt(String(Copy(fn,2,a-1)));
          xName:='';
          Result:=True;
          end
        else
          begin
          xOID:=StrToInt(String(Copy(fn,2,a-1)));
          xName:=Copy(fn,a+1,l-a);
          Result:=True;
          end;
        end;
      end;
    begin
    for i:=0 to rec.Count-1 do
      begin
      fn:=rec.FieldName[i];
      if SeparateFN then
        begin
        case fn[1] of
          RTCL_PFX_DESTROY:
            begin
            DoDestroy(Sender,xOID);
            end;
          RTCL_PFX_CREATE:
            begin
            if xName='' then
              Execute_BadFormat('ExecuteNow['+Int2WStr(i)+'].'+fn)
            else
              begin
              xVal:=TRtcObjectCall.Create(self);
              try
                xVal.xName:=xName;
                xVal.asObject:=rec.asObject[fn];
                rec.asObject[fn]:=nil;
                DoCreate(Sender,xOID,xVal);
              finally
                xVal.Free;
                end;
              end;
            end;
          RTCL_PFX_SETPROP:
            begin
            if xName='' then
              Execute_BadFormat('ExecuteNow['+Int2WStr(i)+'].'+fn)
            else
              begin
              xVal:=TRtcObjectCall.Create(self);
              try
                xVal.xName:=xName;
                xVal.asObject:=rec.asObject[fn];
                rec.asObject[fn]:=nil;
                DoPropSet(Sender,xOID,xVal);
              finally
                xVal.Free;
                end;
              end;
            end;
          RTCL_PFX_SETEVENT:
            begin
            if xName='' then
              Execute_BadFormat('ExecuteNow['+Int2WStr(i)+'].'+fn)
            else
              begin
              xVal:=TRtcObjectCall.Create(self);
              try
                xVal.xName:=xName;
                xVal.asObject:=rec.asObject[fn];
                rec.asObject[fn]:=nil;
                DoEventSet(Sender,xOID,xVal);
              finally
                xVal.Free;
                end;
              end;
            end;
          RTCL_PFX_CALLEVENT:
            begin
            if xName='' then
              Execute_BadFormat('ExecuteNow['+Int2WStr(i)+'].'+fn)
            else
              begin
              xVal:=TRtcObjectCall.Create(self);
              try
                xVal.xName:=xName;
                xVal.asObject:=rec.asObject[fn];
                rec.asObject[fn]:=nil;
                DoEventCall(Sender,xOID,xVal);
              finally
                xVal.Free;
                end;
              end;
            end;
          RTCL_PFX_CALLMETHOD:
            begin
            if xName='' then
              Execute_BadFormat('ExecuteNow['+Int2WStr(i)+'].'+fn)
            else
              begin
              xVal:=TRtcObjectCall.Create(self);
              try
                xVal.xName:=xName;
                xVal.asObject:=rec.asObject[fn];
                rec.asObject[fn]:=nil;
                DoMethodCall(Sender,xOID,xVal);
              finally
                xVal.Free;
                end;
              end;
            end;
          RTCL_PFX_BROADCAST:
            begin
            if xName='' then
              Execute_BadFormat('ExecuteNow['+Int2WStr(i)+'].'+fn)
            else
              begin
              xVal:=TRtcObjectCall.Create(self);
              try
                xVal.xName:=xName;
                xVal.asObject:=rec.asObject[fn];
                rec.asObject[fn]:=nil;
                DoBroadcast(Sender,xOID,xVal);
              finally
                xVal.Free;
                end;
              end;
            end;
          else
            Execute_BadFormat('ExecuteNow['+Int2WStr(i)+'].'+fn);
          end;
        end;
      end;
    end;
  procedure ExecuteArr(arr:TRtcArray);
    var
      i:integer;
    begin
    for i:=0 to arr.Count-1 do
      if arr.isType[i]=rtc_Record then
        ExecuteRec(arr.asRecord[i])
      else
        Execute_BadFormat('ExecuteNow['+Int2WStr(i)+']');
    end;
  begin
  if FActive and assigned(xData) then
    begin
    if xData is TRtcValue then
      begin
      if TRtcValue(xData).isType=rtc_Record then
        ExecuteRec(TRtcValue(xData).asRecord)
      else if TRtcValue(xData).isType=rtc_Array then
        ExecuteArr(TRtcValue(xData).asArray)
      else
        Execute_BadFormat('ExecuteNow');
      end
    else if xData is TRtcRecord then
      ExecuteRec(TRtcRecord(xData))
    else if xData is TRtcArray then
      ExecuteArr(TRtcArray(xData))
    else
      Execute_BadFormat('ExecuteNow');
    end;
  Result:=FExRes;
  FExRes:=nil;
  end;

procedure TRtcRemoteObjectManager.DoCreate(Sender:TObject; xOID: TRtcObjectID; xParam:TRtcObjectCall);
  var
    Constr:TRtcObjectConstructor;
  begin
  try
    Constr:=FindRtcObjectConstructor(xParam.xName);
    if assigned(Constr) then
      begin
      CreatingObjectID:=xOID;
      try
        Constr(Sender,xParam);
      finally
        CreatingObjectID:=0;
        end;
      end
    else
      Execute_NoConstructor(xParam.xName, 'DoCreate('+Int2WStr(xOID)+')');
  except
    on E:Exception do
      Execute_Exception(E,'DoCreate('+Int2WStr(xOID)+','+xParam.xName+')');
    end;
  end;

procedure TRtcRemoteObjectManager.DoDestroy(Sender:TObject; xOID: TRtcObjectID);
  var
    xLink:TRtcObjectLink;
  begin
  try
    xLink:=FindLink(xOID);
    if assigned(xLink) then
      begin
      if assigned(xLink.Owner) then
        begin
        xLink.RemoteDestroyed;
        xLink.DestroyOwner;
        end
      else
        Execute_ObjectNotFound(xOID,'DoDestroy('+Int2WStr(xOID)+')');
      end
    else
      Execute_ObjectNotFound(xOID,'DoDestroy('+Int2WStr(xOID)+')');
  except
    on E:Exception do
      Execute_Exception(E,'DoDestroy('+Int2WStr(xOID)+')');
    end;
  end;

procedure TRtcRemoteObjectManager.DoPropSet(Sender:TObject; xOID: TRtcObjectID; xParam:TRtcObjectCall);
  var
    xLink:TRtcObjectLink;
  begin
  try
    xLink:=FindLink(xOID);
    if assigned(xLink) then
      xLink.DoPropSet(Sender,xParam)
    else
      Execute_ObjectNotFound(xOID,'DoPropSet('+Int2WStr(xOID)+','+xParam.xName+')');
  except
    on E:Exception do
      Execute_Exception(E,'DoPropSet('+Int2WStr(xOID)+','+xParam.xName+')');
    end;
  end;

procedure TRtcRemoteObjectManager.DoEventSet(Sender:TObject; xOID: TRtcObjectID; xParam:TRtcObjectCall);
  var
    xLink:TRtcObjectLink;
  begin
  try
    xLink:=FindLink(xOID);
    if assigned(xLink) then
      xLink.DoEventSet(Sender,xParam)
    else
      Execute_ObjectNotFound(xOID,'DoEventSet('+Int2WStr(xOID)+','+xParam.xName+')');
  except
    on E:Exception do
      Execute_Exception(E,'DoEventSet('+Int2WStr(xOID)+','+xParam.xName+')');
    end;
  end;

procedure TRtcRemoteObjectManager.DoEventCall(Sender:TObject; xOID: TRtcObjectID; xParam:TRtcObjectCall);
  var
    xLink:TRtcObjectLink;
  begin
  try
    xLink:=FindLink(xOID);
    if assigned(xLink) then
      xLink.DoEventCall(Sender,xParam)
    else
      Execute_ObjectNotFound(xOID,'DoEventCall('+Int2WStr(xOID)+','+xParam.xName+')');
  except
    on E:Exception do
      Execute_Exception(E,'DoEventCall('+Int2WStr(xOID)+','+xParam.xName+')');
    end;
  end;

procedure TRtcRemoteObjectManager.DoMethodCall(Sender:TObject; xOID: TRtcObjectID; xParam:TRtcObjectCall);
  var
    xLink:TRtcObjectLink;
  begin
  try
    xLink:=FindLink(xOID);
    if assigned(xLink) then
      xLink.DoMethodCall(Sender,xParam)
    else
      Execute_ObjectNotFound(xOID,'DoMethodCall('+Int2WStr(xOID)+','+xParam.xName+')');
  except
    on E:Exception do
      Execute_Exception(E,'DoMethodCall('+Int2WStr(xOID)+','+xParam.xName+')');
    end;
  end;

procedure TRtcRemoteObjectManager.DoBroadcast(Sender: TObject; xOID: TRtcObjectID; xParam: TRtcObjectCall);
  var
    xLink:TRtcObjectLink;
  begin
  try
    xLink:=FindLink(xOID);
    if assigned(xLink) then
      xLink.DoBroadcast(Sender,xParam)
    else
      Execute_ObjectNotFound(xOID,'DoBroadcast('+Int2WStr(xOID)+','+xParam.xName+')');
  except
    on E:Exception do
      Execute_Exception(E,'DoBroadcast('+Int2WStr(xOID)+','+xParam.xName+')');
    end;
  end;

var
  SubscriberCS:TRtcCritSec;
  SubscribeManagers:TStringObjList;

procedure TRtcRemoteObjectManager._Subscribe(const xLink: TRtcObjectLink; const xChannel: RtcWideString);
  var
    o:TObject;
    objs,objs2:TObjList;
  begin
  if assigned(SubscriberCS) then
    begin
    SubscriberCS.Acquire;
    try
      if not assigned(FChannels) then
        begin
        FChannels:=tStringObjList.Create(16);
        o:=nil;
        end
      else
        o:=FChannels.search(xChannel);
      if not assigned(o) then // 1st subscription for channel "xChannel" from this Object Manager
        begin
        objs:=tObjList.Create(8);
        FChannels.insert(xChannel,objs);

        o:=SubscribeManagers.search(FBroadcastGroup+'.'+xChannel);
        if not assigned(o) then // 1st subscription for channel "xChannel" in group "BroadcastGroup"
          begin
          objs2:=tObjList.Create(16);
          SubscribeManagers.insert(FBroadcastGroup+'.'+xChannel,objs2);
          end
        else // Object(s) from another Object Manager have already subscribed
          objs2:=tObjList(o);
        objs2.insert(RtcIntPtr(self),self);
        end
      else // At least 1 Object already subscribed to channel "xChannel" from this Object Manager
        objs:=tObjList(o);
      objs.insert(RtcIntPtr(xLink),xLink);
    finally
      SubscriberCS.Release;
      end;
    end;
  end;

procedure TRtcRemoteObjectManager._Unsubscribe(const xLink: TRtcObjectLink; const xChannel: RtcWideString);
  var
    o:TObject;
    objs,objs2:TObjList;
  begin
  if assigned(SubscriberCS) then
    begin
    SubscriberCS.Acquire;
    try
      if FChannels=nil then
        raise ERtcInfo.Create('Can not unsubscribe, no subscription active on this Object Manager.');
      o:=FChannels.search(xChannel);
      if o=nil then
        raise ERtcInfo.Create('Can not unsubscribe, Channel "'+String(xChannel)+'" not found on this Object Manager.');
      objs:=tObjList(o);
      o:=objs.search(RtcIntPtr(xLink));
      if o=nil then
        raise ERtcInfo.Create('Can not unsubscribe, Object Link #'+IntToStr(xLink.OID)+' not found.');

      objs.remove(RtcIntPtr(xLink));
      if objs.Empty then // Last Object Link removed from Channel
        begin
        FChannels.remove(xChannel);
        objs.Free;

        o:=SubscribeManagers.search(FBroadcastGroup+'.'+xChannel);
        if not assigned(o) then
          raise ERtcInfo.Create('Can not unsubscribe, Channel "'+String(xChannel)+'" not found for Group "'+String(FBroadcastGroup)+'".');
        objs2:=tObjList(o);
        o:=objs2.search(RtcIntPtr(self));
        if o=nil then
          raise ERtcInfo.Create('Can not unsubscribe, Object Manager not found.');

        objs2.remove(RtcIntPtr(self));
        if objs2.Empty then
          begin
          SubscribeManagers.remove(FBroadcastGroup+'.'+xChannel);
          objs2.Free;
          end;
        end;
    finally
      SubscriberCS.Release;
      end;
    end;
  end;

procedure TRtcRemoteObjectManager._Broadcast(const xChannel: RtcWideString; xParam: TRtcObjectCall);
  var
    o:TObject;
    objs:TObjList;
    om:TRtcRemoteObjectManager;
    i:RtcIntPtr;
  begin
  if assigned(SubscriberCS) then
    begin
    SubscriberCS.Acquire;
    try
      o:=SubscribeManagers.search(FBroadcastGroup+'.'+xChannel);
      if assigned(o) then
        begin
        objs:=TObjList(o);
        i:=objs.search_min(o);
        while assigned(o) do
          begin
          om:=TRtcRemoteObjectManager(o);
          om.AddBroadcast(xChannel,xParam);
          i:=objs.search_g(i,o);
          end;
        end;
    finally
      SubscriberCS.Release;
      xParam.isNull:=True;
      end;
    end
  else
    xParam.isNull:=True;
  end;

procedure TRtcRemoteObjectManager.AddBroadcast(const xChannel:RtcWideString; xParam: TRtcObjectCall);
  var
    o:TObject;
    objs:TObjList;
    i:RtcIntPtr;
    li:TRtcObjectLink;
    rec:TRtcRecord;
    arr:TRtcArray;
  begin
  if assigned(FChannels) then
    begin
    o:=FChannels.search(xChannel);
    if assigned(o) then
      begin
      if not assigned(FBroad) then
        begin
        FBroad:=TRtcValue.Create;
        rec:=FBroad.NewRecord;
        end
      else if FBroad.isType=rtc_Array then
        begin
        arr:=FBroad.asArray;
        rec:=arr.NewRecord(arr.Count);
        end
      else
        begin
        arr:=TRtcArray.Create;
        arr.asObject[0]:=FBroad;
        rec:=arr.NewRecord(1);
        FBroad:=TRtcValue.Create;
        FBroad.asObject:=arr;
        end;

      objs:=TObjList(o);
      i:=objs.search_min(o);
      while assigned(o) do
        begin
        li:=TRtcObjectLink(o);
        if xParam.asObject=nil then
          rec.asObject[RTCL_PFX_BROADCAST+Int2WStr(li.OID)+xParam.xName]:=TRtcValue.Create
        else
          rec.asObject[RTCL_PFX_BROADCAST+Int2WStr(li.OID)+xParam.xName]:=xParam.asObject.copyOf;
        i:=objs.search_g(i,o);
        end;

      FHaveBroad:=True;
      end;
    end;
  end;

function TRtcRemoteObjectManager.GetBroadcast: TRtcValue;
  begin
  if not FHaveBroad then
    Result:=nil
  else if assigned(SubscriberCS) then
    begin
    SubscriberCS.Acquire;
    try
      Result:=FBroad;
      FBroad:=nil;
      FHaveBroad:=False;
    finally
      SubscriberCS.Release;
      end;
    end
  else
    Result:=nil;
  end;

procedure TRtcRemoteObjectManager.Execute_ObjectNotFound(xOID: TRtcObjectID; const xMessage: RtcWideString);
  begin
  if not assigned(FExRes) then
    FExRes:=TRtcArray.Create;
  FExRes.asException[FExRes.Count]:=xMessage+': Object #'+Int2WStr(xOID)+' not found';
  end;

procedure TRtcRemoteObjectManager.Execute_BadFormat(const xMessage: RtcWideString);
  begin
  if not assigned(FExRes) then
    FExRes:=TRtcArray.Create;
  FExRes.asException[FExRes.Count]:=xMessage+': Bad format';
  end;

procedure TRtcRemoteObjectManager.Execute_NoConstructor(const xClassName, xMessage: RtcWideString);
  begin
  if not assigned(FExRes) then
    FExRes:=TRtcArray.Create;
  FExRes.asException[FExRes.Count]:=xMessage+': Missing Constructor for Class "'+xClassName+'"';
  end;

procedure TRtcRemoteObjectManager.Execute_Exception(const E: Exception; const xMessage: RtcWideString);
  begin
  if not assigned(FExRes) then
    FExRes:=TRtcArray.Create;
  FExRes.asException[FExRes.Count]:=xMessage+' - '+RtcWideString(E.ClassName)+': '+RtcWideString(E.Message);
  end;

procedure TRtcRemoteObjectManager.SetBroadcastGroup(const Value: RtcWideString);
  begin
  if FBroadcastGroup<>Value then
    begin
    FBroadcastGroup := Value;
    end;
  end;

function TRtcRemoteObjectManager.ExecuteWithBroadcast(Sender: TObject; const xData: TRtcValueObject): TRtcArray;
  var
    xBroad:TRtcValue;
  begin
  xBroad:=GetBroadcast;
  if assigned(xBroad) then
    try
      FExRes:=ExecuteNow(Sender,xBroad);
    finally
      xBroad.Free;
      end;
  if assigned(xData) then
    FExRes:=ExecuteNow(Sender,xData);
  Result:=FExRes;
  FExRes:=nil;
  end;

procedure TRtcRemoteObjectManager.ExecuteBroadcast(Sender: TObject; const xData:TRtcValueObject=nil);
  var
    xBroad:TRtcValue;
  begin
  if assigned(xData) then
    FExRes:=ExecuteNow(Sender,xData)
  else
    begin
    xBroad:=GetBroadcast;
    if assigned(xBroad) then
      try
        FExRes:=ExecuteNow(Sender,xBroad);
      finally
        xBroad.Free;
        end;
    end;
  end;

{ TRtcBasicRemoteObjectManager }

procedure TRtcBasicRemoteObjectManager.DataReady;
  begin
  if assigned(FOnDataReady) then
    FOnDataReady(self);
  end;


procedure TRtcBasicRemoteObjectManager.DoCreate(Sender:TObject; xOID: TRtcObjectID; xParam:TRtcObjectCall);
  begin
  if assigned(FOnObjectCreate) then
    begin
    CreatingObjectID:=xOID;
    try
      FOnObjectCreate(Sender,xParam);
    except
      CreatingObjectID:=0;
      end;
    if CreatingObjectID<>0 then
      inherited;
    end
  else
    inherited;
  end;

{ TRtcLocalObjectManager }

constructor TRtcLocalObjectManager.Create(xServer:boolean);
  begin
  inherited Create(xServer);
  FLocal:=True;
  FRemote:=TRtcLocalObjectManager.CreateLinkedManager(self, not xServer);
  end;

constructor TRtcLocalObjectManager.CreateLinkedManager(xRemote: TRtcLocalObjectManager; xServer:boolean);
  begin
  inherited Create(xServer);
  FLocal:=False;
  FRemote:=xRemote;
  end;

destructor TRtcLocalObjectManager.Destroy;
  var
    r:TRtcLocalObjectManager;
  begin
  r:=FRemote;
  FRemote:=nil;
  if FLocal then
    begin
    RemoteDestroyed; // Local instance
    r.RemoteDestroyed;
    RtcFreeAndNil(r);
    end
  else
    r:=nil;
  inherited;
  end;

procedure TRtcLocalObjectManager.DataReady;
  var
    xData:TRtcValue;
    xRes:TRtcArray;
    Tmp:TRtcObjectManager;
  begin
  if assigned(FRemote) then
    begin
    xData:=GetData;
    if assigned(xData) then
      try
        Tmp:=SetRtcObjectManager(FRemote);
        try
          xRes:=FRemote.ExecuteWithBroadcast(self,xData);
        finally
          SetRtcObjectManager(Tmp);
          end;
        xRes.Free;
      finally
        xData.Free;
      end;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; {$ENDIF}
SubscriberCS:=TRtcCritSec.Create;
SubscribeManagers:=tStringObjList.Create(32);
finalization
{$IFDEF RTC_DEBUG} Log('rtcLink Finalizing ...','DEBUG');{$ENDIF}
RtcFreeAndNil(SubscriberCS);
RtcFreeAndNil(SubscribeManagers);
{$IFDEF RTC_DEBUG} Log('rtcLink Finalized.','DEBUG');{$ENDIF}
end.
