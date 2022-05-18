{
  @html(<b>)
  Remote Functions
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit implements a set of components for WRITING remote functions
  and forming groups of functions, which can be used with @Link(TRtcDataClient)
  and/or @Link(TRtcDataServer) connection components, or directly by
  calling @Link(TRtcFunctionGroup.ExecuteData) or @Link(TRtcFunctionGroup.CallFunction) methods,
  provided by the @Link(TRtcFunctionGroup) component. By assigning a @Link(TRtcFunctionGroup)
  component to @Link(TRtcServerModule) on the server-side (and linking that ServerModule to a
  DataServer connection component), clients can remotely call all functions which that function
  group provides. To give the client access to server-side remote functions, you can use the
  @Link(TRtcClientModule) component.
  Implementing a RTC Remote Function is as easy as writing a local function.
}
unit rtcFunction;

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
  rtcConn;

type
  TRtcAbsCommand = class;
  TRtcAbsFunction = class;
  TRtcFunctionGroup = class;
  TRtcFunction = class;

  ERtcBadFunctionName = class(Exception);

  // @exclude
  TRtcAbsFunctionList = class(TObject)
  private
    FList:TRtcObjectList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Value:TRtcAbsFunction);
    procedure Remove(Value:TRtcAbsFunction);

    procedure RemoveAll;

    function Count:integer;
    function Get(index:integer):TRtcAbsFunction;
    end;

  // @exclude
  TRtcFunctionList = class(TObject)
  private
    FList:TRtcObjectList;
    FSorted:tStringObjList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Value:TRtcFunction);
    procedure Remove(Value:TRtcFunction);

    procedure RemoveAll;

    function Count:integer;
    function Get(index:integer):TRtcFunction;

    function NamedCount:integer;
    function Find(const byName:RtcWideString):TRtcFunction;
    end;

  // @abstract(Command Info)
  TRtcCommandInfo=class(TRtcDataObject)
  public
    Sender: TRtcConnection;
    Command: TRtcAbsCommand;
    Group: TRtcFunctionGroup;

    constructor Create;
    end;

  // @abstract(Abstract Commands class)
  TRtcAbsCommand=class(TRtcComponent)
  protected
    // @exclude
    function Call_Execute(const CmdInfo:TRtcCommandInfo; const Param:TRtcFunctionInfo; const Res:TRtcValue):boolean; virtual; abstract;

    // @exclude
    function Function_Exists(const Function_Name:RtcWideString):boolean; virtual; abstract;
    end;

  // @abstract(Abstract Function Provider class)
  TRtcAbsFunction=class(TRtcAbsCommand)
  private
    FGroup,
    FHelperGroup:TRtcFunctionGroup;

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    function GetGroup: TRtcFunctionGroup;
    // @exclude
    procedure SetGroup(const Value: TRtcFunctionGroup);

    // @exclude
    function GetHelperGroup: TRtcFunctionGroup;
    // @exclude
    procedure SetHelperGroup(const Value: TRtcFunctionGroup);

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;
    end;

  { @abstract(Provide access to a group of remote functions)

    To implement remote functions, you will need at least one FunctionGroup component
    and link one or more TRtcFunction components to it. Function Group provides the means
    to use function calls as parameters to other function calls from the same group.
    It is primarily used by the TRtcServerModule and TRtcClientModule components to
    hold implementations for their remote functions. @html(<br><br>)

    This FunctionGroup will provide direct access to: @html(<br>)
    1.) all functions which have this component as their ".Group" property set. @html(<br>)
    2.) all functions of all child RtcFunctionGroups, meaning: RtcFunctionGroup components
    which have this component assigned to their "ParentGroup" property. @html(<br>)
    3.) all functions of the RtcFunctionGroup which is directly assigned to this
    component's "HelperGroup" property, including its child components. It is safe to
    assign the same HelperGroup to function groups at different levels (child/parent groups). }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcFunctionGroup=class(TRtcAbsFunction)
  private
    FFunctions:TRtcFunctionList; // List of Child Functions
    FFuncGroups:TRtcAbsFunctionList; // List of FunctionGroups with this component as their ParentGroup
    FGlobalUse:TRtcAbsFunctionList; // List of FunctionGroups with this component as their HelperGroup

    FBeforeExecute: TRtcFunctionCallEvent;
    FAfterExecute: TRtcFunctionCallEvent;
    FExecuteError: TRtcFunctionErrorEvent;
    FOnWrongCall: TRtcFunctionCallEvent;

  protected
    // @exclude
    procedure UpdateStatus(const Value:RtcString);
    // @exclude
    function CheckStatus:RtcString;

    // @exclude
    procedure AddFunction(Value:TRtcAbsFunction);
    // @exclude
    procedure RemoveFunction(Value:TRtcAbsFunction);
    // @exclude
    procedure RemoveAllFunctions;

    // @exclude
    procedure AddGlobalFunction(Value:TRtcAbsFunction);
    // @exclude
    procedure RemoveGlobalFunction(Value:TRtcAbsFunction);
    // @exclude
    procedure RemoveAllGlobalFunctions;

    // @exclude
    function Call_Execute(const CmdInfo:TRtcCommandInfo; const Param:TRtcFunctionInfo; const Res:TRtcValue):boolean; override;

    // @exclude
    function Function_Exists(const Function_Name:RtcWideString):boolean; override;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    { Call Function and prepare the result.
      If the function called does not exist, exception will be raised.
      @param(Sender = connection component)
      @param(Call = function call with all parameters)
      @param(Res = object to receive the result)
      @param(recursive = if TRUE, all results returned from any function called here,
             will also be checked for functions and those functions will be executed)
      @param(command = when assigned, will be used to execute all commands)
      @return(TRUE if function found and executed) }
    function CallFunction(CmdInfo:TRtcCommandInfo; Call:TRtcFunctionInfo; Res:TRtcValue; recursive:boolean=False):boolean;

    { Execute all Functions you can find in "Data" and prepare the result.
      @param(CmdInfo = command info/parameters)
      @param(Data = object holding data and function calls with all parameters)
      @param(recursive = if TRUE, all results returned from any function called here,
             will also be checked for funcfunctions and those functions will be executed)
      @return(If there is a function that can not be found, exception will be raised.
              If everything went OK, but the result couldn't be stored back in Data,
              a new object will be created and returned as a result [Result<>Data],
              leaving it up to the caller to decide what to do with the "old" Data object.
              If you've used Data just to hold the call,
              you should release it if Result<>nil and Result<>Data.) }
    function ExecuteData(CmdInfo:TRtcCommandInfo; Data:TRtcValueObject; recursive:boolean=False):TRtcValueObject; overload;

    { Execute all Functions you can find in "Data" and prepare the result.
      @param(Sender = RTC connection object)
      @param(Data = object holding data and function calls with all parameters)
      @param(recursive = if TRUE, all results returned from any function called here,
             will also be checked for functions and those functions will be executed)
      @return(If there is a function that can not be found, exception will be raised.
              If everything went OK, but the result couldn't be stored back in Data,
              a new object will be created and returned as a result [Result<>Data],
              leaving it up to the caller to decide what to do with the "old" Data object.
              If you've used Data just to hold the call,
              you should release it if Result<>nil and Result<>Data.) }
    function ExecuteData(Sender:TRtcConnection; Data:TRtcValueObject; recursive:boolean=False):TRtcValueObject; overload;

    { Returns TRUE if function with name "FunctionName" exists }
    function FunctionExists(const FunctionName:RtcWideString):boolean;

  published
    { Using the ParentGroup property, you can define this group to be a child
      of another RtcFunctionGroup component. All parent groups have full access to all
      functions defined by any child group. }
    property ParentGroup:TRtcFunctionGroup read GetGroup write SetGroup;

    { Using the HelperGroup property, you can assign another group of functions
      to this group, to be able to use all functions from HelperGroup as
      parameters or in combination with your functions, when your group is used
      as the executing FunctionGroup. Executing function group is the group
      on which you call the ExecuteData or CallFunction methods directly, or
      for RtcClientModule and RtcServerModule, the one directly assigned to the
      component that uses it.
      HelperGroup should be a set of basic helper functions, which you want to
      be able to use from all or most of other functions when passing parameters. }
    property HelperGroup:TRtcFunctionGroup read GetHelperGroup write SetHelperGroup;

    { Number of Functions assigned to this Group (Func) + Number of other Groups
      which have assigned this Group as their HelperGroup (Hlp) + Number of other
      Groups assigning this Group as their ParentGroup (Par), linked to this component. }
    property StatusCheck:RtcString read CheckStatus write UpdateStatus stored False;

    { This event will be called every time BEFORE the function call is passed
      over to the TRtcFunction object and can be used for monitoring function calls
      before they happen. You can modify calling parameters in this event if needed,
      temper with the Result object or raise an exception if you do not want the
      function to be executed.

      The event will only be called for functions directly assigned to this function group.
      It will NOT be executed for functions assigned to the Helper or the Parent Function
      Group, nor in case a function is missing (not assigned to the group).

      The event will receive all parameters as the Function. }
    property BeforeExecute:TRtcFunctionCallEvent read FBeforeExecute write FBeforeExecute;

    { This event will be called every time AFTER a call to a Function assigned to this
      FunctionGroup has been executed without raising an exception. You can use this
      event to monitor function calls after they have been completed. Since the function
      has already completed execution, you will also have access to the Result parameters,
      which you can also modify - if you want. }
    property AfterExecute:TRtcFunctionCallEvent read FAfterExecute write FAfterExecute;

    { This event will be called if a call to a Function assigned to this FunctionGroup
      raised an Exception and left the Exception unhandled (no try/except block).
      You can use this event to monitor function calls which raise Exceptions and/or
      implement a custom Exception handler for these functions. To handle Exceptions,
      set "Handled:=TRUE" from within this event and update the Result object as needed.
      If an Exception is Handled within this event (Handled=TRUE), the Result object
      will be returned, instead of the Exception that was raised by the function. }
    property ExecuteError:TRtcFunctionErrorEvent read FExecuteError write FExecuteError;

    { This event will be executed if a Function to be called is missing (unassigned).
      You can use this event to log calls to functions which are not yet implemented,
      or function calls to undeclared/non-existend functions.

      Changes to Parameters or the Result object inside this event will have no effect.
      If a function is not assigned, an exception will be raised by the component and
      the error message (Function not found) will be returned to the calling Client. }
    property OnWrongCall:TRtcFunctionCallEvent read FOnWrongCall write FOnWrongCall;
    end;

  { @abstract(Remote Function component)

    By setting the "FunctionName" property, linking the component to a FunctionGroup and
    implementing the OnExecute event handler, your remote function is ready to be used.
    When writing a remote function, you don't have to think about anything but your function.
    In case of an exception (which you can also raise inside your OnExecute event handler),
    client will get your exception message as a result, so you don't event have to
    worry about that. @html(<br><br>)

    You can combine multiple function calls in one request, or pass function calls as
    parameters to other function calls. It makes no difference to the functions you implement,
    because your function will always receive pure data, after all function calls (which the
    client might have defined as parameters) are executed. @html(<br><br>)

    And in case of serial function calls (more than one function called in one request),
    if one call ends up with an exception, the result for THAT call will be rtc_Exception
    (with the appropriate eror message), while any prior functions return their result
    and the execution of the request is aborted. @html(<br><br>)

    You can also return a TRtcFunctionInfo object as a result of your function call, in
    which case that function will be sent to the other side and executed there.
    All objects received from the client or from a function call will be checked
    for Function Calls and any parameter that was a function call, will be replaced with
    the result of that function. If you send pure data to the server (without any function calls),
    you will receive that same data as a result to your request. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcFunction=class(TRtcAbsFunction)
  private
    FFuncName:RtcWideString;
    FOnExecute:TRtcFunctionCallEvent;

    function GetFuncName: RtcWideString;
    procedure SetFuncName(const Value: RtcWideString);

  public
    // @exclude
    function Call_Execute(const CmdInfo:TRtcCommandInfo; const Param:TRtcFunctionInfo; const Res:TRtcValue):boolean; override;

    // @exclude
    function Function_Exists(const Function_Name:RtcWideString):boolean; override;

  published
    { Assign this Function to a function group, so it can be used
      by the RtcClientModule and/or RtcServerModule components. }
    property Group:TRtcFunctionGroup read GetGroup write SetGroup;

    { Function Name (String, not case sensitive) }
    property FunctionName:RtcWideString read GetFuncName write SetFuncName;

    { This event will be called to execute the function and prepare the return value.
      You will receive the Result object as parameter, which will be initialy set to
      a NULL value (Result.isType = rtc_null) and should be used to fill-in any data
      your function has to return. For example, to return a String containing
      the text "HELLO, WORLD!", simply set:@html(<br>)
      Result.asString='HELLO, WORLD!'; @html(<br><br>)

      The Sender parameter (server connection component) can be used to access
      Session, Request or Response information, but NOT to Read the content Body
      or to Write the content out directly to the connection component. Even
      though there will be no exceptions raised in case you do use the Read or
      Write methods from the Sender, the result produced by such an action will
      result in content which Client will not be able to decode. In other words,
      even if your server would send the content out, the Client which called
      the function wouldn't be able to read the data and would most likely result
      in an exception at client's side. }
    property OnExecute:TRtcFunctionCallEvent read FOnExecute write FOnExecute;
    end;

  { @abstract(Use to define events which will receive results from remote function calls) }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcResult = class(TRtcComponent)
  private
    FOnReturn:TRtcResultEvent;
    FOnAborted:TRtcResultEvent;
    FOnPrepare:TRtcResultEvent;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    // @exclude
    function Valid:boolean; virtual;

    // @exclude
    procedure Call_Return(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue); virtual;
    // @exclude
    procedure Call_Aborted(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue); virtual;
    // @exclude
    procedure Call_Prepare(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue); virtual;

  published
    { When used by a client in combination with @Link(TRtcClientModule) to call
      remote functions using methods provided by @Link(TRtcClientModule), this event
      will be used to process the result returned from the remote function call,
      if you pass this TRtcResult component as parameter to the
      TRtcClientModule.@Link(TRtcClientModule.Call) method.

      The Sender parameter (connection component) can be used to access
      Session, Request or Response information, but NOT to Read the content Body
      or to Write the content out directly to the connection component. }
    property OnReturn:TRtcResultEvent read FOnReturn write FOnReturn;

    { When used by a client in combination with @Link(TRtcClientModule) to call remote functions,
      this event will be triggered when a remote call is being prepared for sending
      after triggering the "OnPrepareCall" event on the "TRtcClientModule" component,
      if you have passed this TRtcResult component as parameter to the
      TRtcClientModule.@Link(TRtcClientModule.Call) method and you did NOT set
      "Data.isNull:=TRUE" in the "OnPrepareCall" event of the "TRtcClientModule" component.

      This event can be used to monitor, modify and/or skip remote calls before sending.

      The Sender parameter (connection component) can be used to access
      Session, Request or Response information, but NOT to Read the content Body
      or to Write the content out directly to the connection component.

      Data parameter will contain the remote call,
      Result parameter will be NIL.

      Set "Data.isNull:=TRUE" in this event if you want to SKIP sending this remote call. }
    property PreparingCall:TRtcResultEvent read FOnPrepare write FOnPrepare;

    { When used by a client in combination with @Link(TRtcClientModule) to call remote
      functions using methods provided by @Link(TRtcClientModule), this event will be
      triggered if a remote Call was aborted and won't be re-sent.

      The Sender parameter (server connection component) can be used to access
      Session, Request or Response information, but NOT to Read the content Body
      or to Write the content out directly to the connection component.

      Result parameter will be NIL. }
    property RequestAborted:TRtcResultEvent read FOnAborted write FOnAborted;
    end;

implementation

{ TRtcFunctionList }

constructor TRtcFunctionList.Create;
  begin
  inherited;
  FList:=TRtcObjectList.Create;
  FSorted:=tStringObjList.Create(8);
  end;

destructor TRtcFunctionList.Destroy;
  begin
  try
    RtcFreeAndNil(FList);
    RtcFreeAndNil(FSorted);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcFunctionList.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcFunctionList.Add(Value: TRtcFunction);
  var
    idx:integer;
    UpName:RtcWideString;
  begin
  idx:=FList.IndexOf(Value);
  if idx>=0 then
    FList.Delete(idx);
  FList.Add(Value);
  if Value.FFuncName<>'' then
    begin
    UpName:=Upper_WCase(Value.FFuncName);
    if FSorted.search(UpName)=nil then
      FSorted.insert(UpName,Value);
    end;
  end;

procedure TRtcFunctionList.Remove(Value: TRtcFunction);
  var
    idx:integer;
    UpName:RtcWideString;
  begin
  idx:=FList.IndexOf(Value);
  if idx>=0 then
    FList.Delete(idx);
  if Value.FFuncName<>'' then
    begin
    UpName:=Upper_WCase(Value.FFuncName);
    if FSorted.search(UpName)=Value then
      FSorted.remove(UpName);
    end;
  end;

function TRtcFunctionList.Find(const byName:RtcWideString): TRtcFunction;
  var
    UpName:RtcWideString;
    Obj:TObject absolute Result;
  begin
  UpName:=Upper_WCase(byName);
  Obj:=FSorted.search(UpName);
  end;

function TRtcFunctionList.Count: integer;
  begin
  Result:=FList.Count;
  end;

function TRtcFunctionList.NamedCount: integer;
  begin
  Result:=FSorted.Count;
  end;

procedure TRtcFunctionList.RemoveAll;
  begin
  if assigned(FList) then
    FList.Clear;
  if assigned(FSorted) then
    FSorted.removeall;
  end;

function TRtcFunctionList.Get(index:integer): TRtcFunction;
  begin
  if (index>=0) and (index<FList.Count) then
    Result:=TRtcFunction(FList.Items[index])
  else
    Result:=nil;
  end;

{ TRtcAbsFunctionList }

constructor TRtcAbsFunctionList.Create;
  begin
  inherited;
  FList:=TRtcObjectList.Create;
  end;

destructor TRtcAbsFunctionList.Destroy;
  begin
  try
    RtcFreeAndNil(FList);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcAbsFunctionList.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcAbsFunctionList.Add(Value: TRtcAbsFunction);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx>=0 then
    FList.Delete(idx);
  FList.Add(Value);
  end;

procedure TRtcAbsFunctionList.Remove(Value: TRtcAbsFunction);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx>=0 then
    FList.Delete(idx);
  end;

function TRtcAbsFunctionList.Count: integer;
  begin
  Result:=FList.Count;
  end;

procedure TRtcAbsFunctionList.RemoveAll;
  begin
  if assigned(FList) then
    FList.Clear;
  end;

function TRtcAbsFunctionList.Get(index:integer): TRtcAbsFunction;
  begin
  if (index>=0) and (index<FList.Count) then
    Result:=TRtcAbsFunction(FList.Items[index])
  else
    Result:=nil;
  end;

{ TRtcAbsFunction }

constructor TRtcAbsFunction.Create(AOwner: TComponent);
  begin
  inherited;
  FGroup:=nil;
  FHelperGroup:=nil;
  end;

destructor TRtcAbsFunction.Destroy;
  begin
  try
    SetGroup(nil);
    SetHelperGroup(nil);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcAbsFunction.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcAbsFunction.GetGroup: TRtcFunctionGroup;
  begin
  Result:=FGroup;
  end;

procedure TRtcAbsFunction.SetGroup(const Value: TRtcFunctionGroup);
  var
    MyGroup:TRtcFunctionGroup;
  begin
  if Value<>FGroup then
    begin
    if assigned(FGroup) then
      begin
      FGroup.RemoveFunction(self);
      FGroup:=nil;
      end;

    if assigned(Value) then
      begin
      if Value=FHelperGroup then
        raise Exception.Create('Can not use same Group as ParentGroup and HelperGroup.');
      // Check for simple circular references before assigning!
      MyGroup:=Value;
      while (MyGroup<>nil) do
        begin
        if (MyGroup=self) or
           (MyGroup.GetGroup=self) or
           (MyGroup.GetHelperGroup=self) then
          raise Exception.Create('Circular FunctionGroup reference!');
        MyGroup:=MyGroup.ParentGroup;
        end;

      FGroup:=Value;
      FGroup.AddFunction(self);
      end;
    end;
  end;

function TRtcAbsFunction.GetHelperGroup: TRtcFunctionGroup;
  begin
  Result:=FHelperGroup;
  end;

procedure TRtcAbsFunction.SetHelperGroup(const Value: TRtcFunctionGroup);
  var
    MyGroup:TRtcFunctionGroup;
  begin
  if Value<>FHelperGroup then
    begin
    if assigned(FHelperGroup) then
      begin
      FHelperGroup.RemoveGlobalFunction(self);
      FHelperGroup:=nil;
      end;

    if assigned(Value) then
      begin
      // Check for simple circular reference before assigning!
      if Value=FGroup then
        raise Exception.Create('Can not use same Group as ParentGroup and HelperGroup.');
      MyGroup:=Value;
      while (MyGroup<>nil) do
        begin
        if (MyGroup=self) or
           (MyGroup.ParentGroup=self) or
           (MyGroup.HelperGroup=self) then
          raise Exception.Create('Circular FunctionGroup reference!');
        MyGroup:=MyGroup.ParentGroup;
        end;

      FHelperGroup:=Value;
      FHelperGroup.AddGlobalFunction(self);
      end;
    end;
  end;

procedure TRtcAbsFunction.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent,Operation);
  if Operation=opRemove then
    if AComponent=FGroup then
      SetGroup(nil)
    else if AComponent=FHelperGroup then
      SetHelperGroup(nil);
  end;

{ TRtcFunctionGroup }

constructor TRtcFunctionGroup.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FFunctions:=TRtcFunctionList.Create;
  FFuncGroups:=TRtcAbsFunctionList.Create;
  FGlobalUse:=TRtcAbsFunctionList.Create;
  end;

destructor TRtcFunctionGroup.Destroy;
  begin
  try
    RemoveAllFunctions;
    RemoveAllGlobalFunctions;

    RtcFreeAndNil(FFuncGroups);
    RtcFreeAndNil(FFunctions);
    RtcFreeAndNil(FGlobalUse);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcFunctionGroup.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcFunctionGroup.UpdateStatus(const Value:RtcString);
  begin
  // nothing to do here
  end;

function TRtcFunctionGroup.CheckStatus:RtcString;
  begin
  Result:='';
  if assigned(FFunctions) then
    if FFunctions.Count>0 then
      begin
      if FFunctions.NamedCount=FFunctions.Count then
        Result:='= '
      else
        Result:='!! ';
      Result:=Result+Int2Str(FFunctions.NamedCount)+'/'+Int2Str(FFunctions.Count)+' Func ';
      end;
  if assigned(FFuncGroups) then
    if FFuncGroups.Count>0 then
      Result:=Result+'+'+Int2Str(FFuncGroups.Count)+' Par ';
  if assigned(FGlobalUse) then
    if FGlobalUse.Count>0 then
      Result:=Result+'+'+Int2Str(FGlobalUse.Count)+' Hlp ';
  end;

procedure TRtcFunctionGroup.AddFunction(Value: TRtcAbsFunction);
  begin
  if Value is TRtcFunction then
    FFunctions.Add(TRtcFunction(Value))
  else
    FFuncGroups.Add(Value);
  end;

procedure TRtcFunctionGroup.RemoveFunction(Value: TRtcAbsFunction);
  begin
  if Value is TRtcFunction then
    FFunctions.Remove(TRtcFunction(Value))
  else
    FFuncGroups.Remove(Value);
  end;

procedure TRtcFunctionGroup.RemoveAllFunctions;
  var
    Func:TRtcAbsFunction;
  begin
  while FFunctions.Count>0 do
    begin
    Func:=FFunctions.Get(0);
    Func.SetGroup(nil);
    end;
  while FFuncGroups.Count>0 do
    begin
    Func:=FFuncGroups.Get(0);
    Func.SetGroup(nil);
    end;
  end;

procedure TRtcFunctionGroup.AddGlobalFunction(Value: TRtcAbsFunction);
  begin
  FGlobalUse.Add(Value);
  end;

procedure TRtcFunctionGroup.RemoveGlobalFunction(Value: TRtcAbsFunction);
  begin
  FGlobalUse.Remove(Value);
  end;

procedure TRtcFunctionGroup.RemoveAllGlobalFunctions;
  var
    Func:TRtcAbsFunction;
  begin
  while FGlobalUse.Count>0 do
    begin
    Func:=TRtcAbsFunction(FGlobalUse.Get(0));
    Func.SetHelperGroup(nil);
    end;
  end;

function TRtcFunctionGroup.Call_Execute(const CmdInfo: TRtcCommandInfo; const Param: TRtcFunctionInfo; const Res: TRtcValue):boolean;
  var
    idx:integer;
    Func:TRtcAbsFunction;
    Handled:boolean;

  function DoExecute(tested:boolean):boolean;
    begin
    Result:=False;
    if assigned(FBeforeExecute) then
      begin
      if tested or Func.Function_Exists(Param.FunctionName) then
        begin
        // "BeforeExecute" will be called only if we have a function with that name
        try
          FBeforeExecute(CmdInfo.Sender,Param,Res);
          if Func.Call_Execute(CmdInfo, Param, Res) then
            begin
            Result:=True;
            // "AfterExecute" will be called only if function call was successful
            if assigned(FAfterExecute) then
              FAfterExecute(CmdInfo.Sender,Param,Res);
            end
          else
            ERtcInfo.Create('Call_Execute failed on function "'+String(Param.FunctionName)+'".');
        except
          on E:Exception do
            begin
            Handled:=False;
            // "ExecuteError will be called if an exception was raised
            if assigned(FExecuteError) then
              FExecuteError(CmdInfo.Sender,Param,Res,E,Handled);
            if Handled then
              Result:=True
            else
              raise;
            end;
          end;
        end;
      end
    else
      begin
      try
        if Func.Call_Execute(CmdInfo, Param, Res) then
          begin
          Result:=True;
          // "AfterExecute" will be called only if function call was successful
          if assigned(FAfterExecute) then
            FAfterExecute(CmdInfo.Sender,Param,Res);
          end;
      except
        on E:Exception do
          begin
          Handled:=False;
          // "ExecuteError" will be called if an exception was raised
          if assigned(FExecuteError) then
            FExecuteError(CmdInfo.Sender,Param,Res,E,Handled);
          if Handled then
            Result:=True
          else
            raise;
          end;
        end;
      end;
    end;

  begin
  Func:=FFunctions.Find(Param.FunctionName);
  if assigned(Func) then
    Result:=DoExecute(True)
  else if FFunctions.Count<>FFunctions.NamedCount then
    begin
    Result:=False;
    for idx:=0 to FFunctions.Count-1 do
      begin
      Func:=FFunctions.Get(idx);
      if assigned(Func) then
        if DoExecute(False) then
          begin
          Result:=True;
          Break;
          end;
      end;
    end
  else
    Result:=False;
  if not Result and (FFuncGroups.Count>0) then
    begin
    for idx:=0 to FFuncGroups.Count-1 do
      begin
      Func:=FFuncGroups.Get(idx);
      if assigned(Func) then
        if DoExecute(False) then
          begin
          Result:=True;
          Break;
          end;
      end;
    end;
  if not Result and assigned(HelperGroup) then
    Result:=HelperGroup.Call_Execute(CmdInfo, Param, Res);
  end;

function TRtcFunctionGroup.Function_Exists(const Function_Name: RtcWideString): boolean;
  var
    idx:integer;
    Func:TRtcAbsFunction;
  begin
  Result:=FFunctions.Find(Function_Name)<>nil;
  if not Result and (FFunctions.Count<>FFunctions.NamedCount) then
    begin
    for idx:=0 to FFunctions.Count-1 do
      begin
      Func:=FFunctions.Get(idx);
      if assigned(Func) then
        if Func.Function_Exists(Function_Name) then
          begin
          Result:=True;
          Break;
          end;
      end;
    end;
  if not Result and (FFuncGroups.Count>0) then
    begin
    for idx:=0 to FFuncGroups.Count-1 do
      begin
      Func:=FFuncGroups.Get(idx);
      if assigned(Func) then
        if Func.Function_Exists(Function_Name) then
          begin
          Result:=True;
          Break;
          end;
      end;
    end;
  if not Result and assigned(HelperGroup) then
    Result:=HelperGroup.Function_Exists(Function_Name);
  end;

function TRtcFunctionGroup.CallFunction(CmdInfo: TRtcCommandInfo; Call: TRtcFunctionInfo;
                                        Res: TRtcValue; recursive:boolean=False):boolean;
  var
    idx2:integer;
    field:RtcWideString;
    obj,xres:TRtcValueObject;
  begin
  { Prepare all parameters before execution.
    It is of utmost importance to do this in reverse order,
    because fields could dissapear after their execution and
    corrupt the FieldCount property. }
  for idx2:=Call.FieldCount-1 downto 0 do
    begin
    field:=Call.FieldName[idx2];
    obj:=Call.asObject[field];
    if not isSimpleValue(obj) then
      begin
      xres:=CmdInfo.Group.ExecuteData(CmdInfo, obj, recursive);
      if obj<>xres then
        begin
        Call.isNull[field]:=true;
        Call.asObject[field]:=xres;
        end;
      end;
    end;
  Result:=Call_Execute(CmdInfo, Call, Res);
  // "OnWrongCall" will be called only if function does not exist.
  if not Result and assigned(FOnWrongCall) then
    FOnWrongCall(CmdInfo.Sender, Call, Res);
  end;

function TRtcFunctionGroup.ExecuteData(CmdInfo: TRtcCommandInfo; Data: TRtcValueObject;
                                       recursive:boolean=False): TRtcValueObject;
  var
    idx,row:integer;
    obj,res:TRtcValueObject;
    field:RtcWideString;
  begin
  if Data=nil then
    Result:=nil
  else if Data is TRtcFunctionInfo then
    begin
    Result:=TRtcValueResult.Create(TRtcFunctionInfo(Data));
    try
      if assigned(CmdInfo.Command) then
        begin
        if CmdInfo.Command.Function_Exists(TRtcFunctionInfo(Data).FunctionName) then
          CmdInfo.Command.Call_Execute(CmdInfo, TRtcFunctionInfo(Data), TRtcValue(Result))
        else if not CallFunction(CmdInfo, TRtcFunctionInfo(Data), TRtcValue(Result), recursive) then
          raise ERtcBadFunctionName.Create('Function or Command '#39+String(TRtcFunctionInfo(Data).FunctionName)+#39' not found.');
        end
      else if not CallFunction(CmdInfo, TRtcFunctionInfo(Data), TRtcValue(Result), recursive) then
        raise ERtcBadFunctionName.Create('Function '#39+String(TRtcFunctionInfo(Data).FunctionName)+#39' not found.');
    except
      RtcFreeAndNil(Result);
      raise;
      end;
    { Do not release "Data" object here.
      It will be released by the function which called ExecuteData. }

    { If our function call ended with a complex structure as a result,
      we need to check the result structure for recursive function calls. }
    if recursive and not isSimpleValue(Result) then
      begin
      { We don't need the Data pointer anymore,
        we can use it to call ExecuteData recursively. }
      Data:=Result;
      try
        Result:=CmdInfo.Group.ExecuteData(CmdInfo,Data,recursive); // Recursive call! This will Execute all function calls
      except
        RtcFreeAndNil(Data);
        raise;
        end;
      { Since we have called ExecuteData here,
        we need to release the old Result,
        in case we got another object in return. }
      if Data<>Result then RtcFreeAndNil(Data); // Release the old Result
      end;
    end
  else if Data is TRtcRecord then
    begin
    { It is of utmost importance to do this in reverse order,
      because fields could dissapear after their execution,
      corrupting the FieldCount property. }
    for idx:=TRtcRecord(Data).FieldCount-1 downto 0 do
      begin
      field:=TRtcRecord(Data).FieldName[idx];
      obj:=TRtcRecord(Data).asObject[field];
      if not isSimpleValue(obj) then
        begin
        res:=CmdInfo.Group.ExecuteData(CmdInfo, obj, recursive);
        if obj<>res then
          begin
          TRtcRecord(Data).isNull[field]:=true;
          TRtcRecord(Data).asObject[field]:=res;
          end;
        end;
      end;
    Result:=Data;
    end
  else if Data is TRtcArray then
    begin
    for idx:=0 to TRtcArray(Data).FieldCount-1 do
      begin
      obj:=TRtcArray(Data).asObject[idx];
      if not isSimpleValue(obj) then
        begin
        res:=CmdInfo.Group.ExecuteData(CmdInfo, obj, recursive);
        if obj<>res then
          begin
          TRtcArray(Data).isNull[idx]:=true;
          TRtcArray(Data).asObject[idx]:=res;
          end;
        end;
      end;
    Result:=Data;
    end
  else if Data is TRtcDataSet then
    begin
    for row:=0 to TRtcDataSet(Data).RowCount-1 do
      begin
      TRtcDataSet(Data).Row:=row;
      { It is of utmost importance to do this in reverse order,
        because fields could dissapear after their execution,
        corrupting the FieldCount property. }
      for idx:=TRtcDataSet(Data).FieldCount-1 downto 0 do
        begin
        field:=TRtcDataSet(Data).FieldName[idx];
        obj:=TRtcDataSet(Data).asObject[field];
        if not isSimpleValue(obj) then
          begin
          res:=CmdInfo.Group.ExecuteData(CmdInfo, obj, recursive);
          if obj<>res then
            begin
            TRtcDataSet(Data).isNull[field]:=true;
            TRtcDataSet(Data).asObject[field]:=res;
            end;
          end;
        end;
      end;
    Result:=Data;
    end
  else if Data is TRtcValue then
    begin
    res:=CmdInfo.Group.ExecuteData(CmdInfo, TRtcValue(Data).asObject, recursive);
    if res<>TRtcValue(Data).asObject then
      begin
      TRtcValue(Data).isNull:=True;
      TRtcValue(Data).asObject:=res;
      end;
    Result:=Data;
    end
  else if isSimpleValue(Data) then
    Result:=Data
  else
    raise Exception.Create('Unsupported Data Type.');
  end;

function TRtcFunctionGroup.ExecuteData(Sender: TRtcConnection; Data: TRtcValueObject;
                                       recursive:boolean=False): TRtcValueObject;
  var
    CmdInfo:TRtcCommandInfo;
  begin
  CmdInfo:=TRtcCommandInfo.Create;
  CmdInfo.Sender:=Sender;
  CmdInfo.Group:=self;
  try
    Result:=ExecuteData(CmdInfo, Data, recursive);
  finally
    RtcFreeAndNil(CmdInfo);
    end;
  end;

function TRtcFunctionGroup.FunctionExists(const FunctionName: RtcWideString): boolean;
  begin
  Result:=Function_Exists(FunctionName);
  end;

{ TRtcFunction }

function TRtcFunction.Call_Execute(const CmdInfo: TRtcCommandInfo;
                                   const Param: TRtcFunctionInfo;
                                   const Res: TRtcValue):boolean;
  begin
  if Same_WText(Param.FunctionName,FunctionName) then
    begin
    Result:=True;
    if assigned(FOnExecute) then
      FOnExecute(CmdInfo.Sender,Param,Res)
    else
      raise Exception.Create('OnExecute event missing for function "'+String(FunctionName)+'".');
    end
  else
    Result:=False;
  end;

function TRtcFunction.Function_Exists(const Function_Name: RtcWideString): boolean;
  begin
  Result:= Same_WText(FunctionName, Function_Name);
  end;

function TRtcFunction.GetFuncName: RtcWideString;
  begin
  Result:=FFuncName;
  end;

procedure TRtcFunction.SetFuncName(const Value: RtcWideString);
  begin
  if Value<>FFuncName then
    begin
    if assigned(FGroup) then FGroup.RemoveFunction(self);
    FFuncName:=Value;
    if assigned(FGroup) then FGroup.AddFunction(self);
    end;
  end;

{ TRtcResult }

var
  List:tBinList;
  CS:TRtcCritSec;

procedure AddObj(o:TObject);
  begin
  CS.Acquire;
  try
    List.insert(RtcIntPtr(o),1);
  finally
    CS.Release;
    end;
  end;

procedure DelObj(o:TObject);
  begin
  if assigned(CS) then
    begin
    CS.Acquire;
    try
      List.remove(RtcIntPtr(o));
    finally
      CS.Release;
      end;
    end;
  end;

function HaveObj(o:TObject):boolean;
  begin
  if assigned(CS) then
    begin
    CS.Acquire;
    try
      Result:=List.search(RtcIntPtr(o))>0;
    finally
      CS.Release;
      end;
    end
  else
    Result:=False;
  end;

procedure TRtcResult.Call_Aborted(Sender: TRtcConnection; Data,Result: TRtcValue);
  begin
  if HaveObj(self) then
    if assigned(FOnAborted) then
      FOnAborted(Sender,Data,Result);
  end;

procedure TRtcResult.Call_Return(Sender: TRtcConnection; Data,Result: TRtcValue);
  begin
  if HaveObj(self) then
    if assigned(FOnReturn) then
      FOnReturn(Sender,Data,Result);
  end;

procedure TRtcResult.Call_Prepare(Sender: TRtcConnection; Data,Result: TRtcValue);
  begin
  if HaveObj(self) then
    if assigned(FOnPrepare) then
      FOnPrepare(Sender,Data,Result);
  end;

function TRtcResult.Valid: boolean;
  begin
  Result:=HaveObj(self);
  end;

constructor TRtcResult.Create(AOwner:TComponent);
  begin
  inherited Create(AOwner);
  AddObj(self);
  end;

destructor TRtcResult.Destroy;
  begin
  try
    DelObj(self);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcResult.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcCommandInfo }

constructor TRtcCommandInfo.Create;
  begin
  inherited;
  Sender:=nil;
  Command:=nil;
  Group:=nil;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcFunction Initializing ...','DEBUG');{$ENDIF}

CS:=TRtcCritSec.Create;
List:=tBinList.Create(128);

{$IFDEF RTC_DEBUG} Log('rtcFunction Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcFunction Finalizing ...','DEBUG');{$ENDIF}

RtcFreeAndNil(List);
RtcFreeAndNil(CS);

{$IFDEF RTC_DEBUG} Log('rtcFunction Finalized.','DEBUG');{$ENDIF}
end.

