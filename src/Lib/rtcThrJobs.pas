{
  @html(<b>)
  Thread Jobs
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  "TRtcQuickJob" component, "TRtcThreadEx" class and "PostQuickJob" functions.
}
unit rtcThrJobs;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcThrPool,
  
  rtcInfo;

type
  { Event type used by the "PostQuickJob" procedure }
  TRtcQuickJobEvent = procedure(Data:TRtcValue) of object;
{$IFDEF RTC_ANON_METHODS}
  { Anonymous method type used by the "PostQuickJob" procedure }
  TRtcQuickAnonMethod = reference to procedure(Data:TRtcValue);
  { Anonymous method type used by "PostQuickJob" and "PostGUIJob" procedures }
  TRtcSyncAnonMethod = reference to procedure;
{$ENDIF}

  { @Abstract(Extended Threading class) }
  TRtcThreadEx = class(TRtcThread)
  private
    FInfo:TRtcInfo;
  public
    // Create a Thread. To give the thread something to do, you will have to post a job to it.
    constructor Create; override;

    { @exclude }
    function Finalize:boolean; override;

    // Post event for the thread (thread-safe call)
    // "Data" object will be destroyed by this method call!
    // This method can also be used with "TRtcThread" instance as "me:TObject" parameter
    class function PostEvent(me:TObject; Event:TRtcQuickJobEvent; Data:TRtcValueObject=nil; AccessGUI:boolean=False; HighPriority:boolean=False; ForceThread:boolean=False):boolean; overload;

    // Attach additional information to this Thread. May only be used from within the thread.
    property Info:TRtcInfo read FInfo;
    end;

  { Component used to implement Quick Jobs }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcQuickJob = class(TRtc_Component)
  private
    FEvent:TRtcQuickJobEvent;
    FAccessGUI: boolean;
    FSerialized: boolean;
    FThr:TRtcThread;
    FForceThread: boolean;

   procedure SetForceThread(Value:boolean);
   procedure SetSerialized(Value:boolean);

  public

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Post this Quick Job ("OnExecute" event) to the background Thread Pool
      (AccessGUI=False) or to the Main Thread (AccessGUI=True). "Data" can be NIL,
      or any TRtcValueObject which should be passed as parameter to the Event.

      Data object will be destroyed by this procedure, so you should ONLY
      pass objects here which you have created yourself, or use ".copyOf"
      to get a copy of an object which you did not create yourself.
      This method is thread-safe and can be used from any number of
      threads at the same time.

      If NeedThread=TRUE, a new worker thread will be created for
      this job if the thread pool is busy, even if that means
      increasing the thread pool size beyond the specified limit. }
    procedure Post(const Data:TRtcValueObject=nil; NeedThread:boolean=False);

    { If "Serialized=True",
         stops the virtual thread used to execute jobs. }
    procedure Stop;

    { If "Serialized=True", returns the current number of jobs in the queue,
                            waiting to be executed but not executing yet.
      If "Serialized=False", returns -1. }
    function JobsInQueue:integer;

    { If "Serialized=True", returns TRUE if there is currently a job running
                           (previously posted to this component's virtual thread).
      If "Serialized=False", returns FALSE. }
    function JobRunning:boolean;

    { If "Serialized=True", returns the total number of jobs currently running
                            and/or in the job queue (waiting to be executed).
      If "Serialized=False", returns -1. }
    function JobsTotal:integer;

  published
    { Does the Event need access to the GUI? If TRUE, Event will be called from the Main Thread }
    property AccessGUI:boolean read FAccessGUI write FAccessGUI default False;

    { Sould we create a new thread if all worker threads from the thread pool are busy,
      even if that means increasing the thread pool size beyond the specified limit? }
    property ForceThread:boolean read FForceThread write SetForceThread default False;

    { Serialized jobs use only 1 thread at a time. When Serialized=True, 
      all jobs posted using this component will be executed in order, one at a time.
      When Serialized=False, a new virtual thread will be created for every job
      posted, allowing all jobs to run at the same time, each in its own thread. }
    property Serialized:boolean read FSerialized write SetSerialized default False;

    { Event to be executed }
    property OnExecute:TRtcQuickJobEvent read FEvent write FEvent;
    end;

{ Post "Event" as a Quick Job to the Thread Pool (AccessGUI=False) or to the Main Thread (AccessGUI=True).
   @param(Event = Event to be called)
   @param(Data = any TRtcValueObject which should be passed as parameter to the Event.
                 Data object will be destroyed by this procedure, so you should ONLY
                 pass objects here which you have created yourself, or use ".copyOf"
                 to get a copy of an object which you did not create yourself )
   @param(AccessGUI = does the Event need access to the GUI? If TRUE, Event will be called from the Main Thread)
   @param(ForceThread = start a new thread if all worker threads are busy, even if that means increasing the thread pool size beyond the specified limit) }
procedure PostQuickJob(Event:TRtcQuickJobEvent; const Data:TRtcValueObject=nil; AccessGUI:boolean=False; UseThread:TRtcThread=nil; ForceThread:boolean=False); overload;

{ Post "Event" to be executed from the Main Thread, asynchronously. }
procedure PostGUIJob(Event:TRtcSyncEvent; UseThread:TRtcThread=nil); overload;

{$IFDEF RTC_ANON_METHODS}

{ Post "AnonMethod" as a Quick Job to the Thread Pool (AccessGUI=False) or to the Main Thread (AccessGUI=True).
   @param(AnonMethod = Anonymous Method to be called)
   @param(Data = any TRtcValueObject which should be passed as parameter to AnonMethod.
                 Data object will be destroyed by this procedure, so you should ONLY
                 pass objects here which you have created yourself, or use ".copyOf"
                 to get a copy of an object which you did not create yourself )
   @param(AccessGUI = does AnonMethod need access to the GUI? If TRUE, AnonMethod will be called from the Main Thread)
   @param(ForceThread = start a new thread if all worker threads are busy, even if that means increasing the thread pool size beyond the specified limit) }
procedure PostQuickJob(AnonMethod:TRtcQuickAnonMethod; const Data:TRtcValueObject=nil; AccessGUI:boolean=False; UseThread:TRtcThread=nil; ForceThread:boolean=False); overload;

{ Post "AnonMethod" as a Quick Job to the Thread Pool (AccessGUI=False) or to the Main Thread (AccessGUI=True).
   @param(AnonMethod = Anonymous Method to be called)
   @param(AccessGUI = does AnonMethod need access to the GUI? If TRUE, AnonMethod will be called from the Main Thread)
   @param(ForceThread = start a new thread if all worker threads are busy, even if that means increasing the thread pool size beyond the specified limit) }
procedure PostQuickJob(AnonMethod:TRtcSyncAnonMethod; AccessGUI:boolean=False; UseThread:TRtcThread=nil; ForceThread:boolean=False); overload;

{ Post "AnonMethod" to be executed from the Main Thread, asynchronously. }
procedure PostGUIJob(AnonMethod:TRtcSyncAnonMethod; UseThread:TRtcThread=nil); overload;

{$ENDIF}

implementation

{ TRtcQuickJob }

constructor TRtcQuickJob.Create(AOwner:TComponent);
  begin
  inherited Create(AOwner);
  FThr:=nil;
  FSerialized:=False;
  FForceThread:=False;
  end;

destructor TRtcQuickJob.Destroy;
  begin
  Stop;
  inherited;
  end;

procedure TRtcQuickJob.SetForceThread(Value:boolean);
  begin
  if Value<>FForceThread then
    begin
    FForceThread:=Value;
    if assigned(FThr) then
      FThr.NeedThread:=FForceThread;
    end;
  end;

procedure TRtcQuickJob.SetSerialized(Value:boolean);
  begin
  if Value<>FSerialized then
    begin
    FSerialized:=Value;
    if Value then
      if not assigned(FThr) then
        begin
        FThr:=TRtcThread.Create;
        FThr.NeedThread:=FForceThread;
        end;
    end;
  end;

procedure TRtcQuickJob.Post(const Data: TRtcValueObject=nil; NeedThread:boolean=False);
  begin
  if FSerialized then
    begin
    if assigned(FThr) then
      PostQuickJob(FEvent,Data,AccessGUI,FThr,NeedThread or ForceThread);
    end
  else
    PostQuickJob(FEvent,Data,AccessGUI,nil,NeedThread or ForceThread);
  end;

function TRtcQuickJob.JobsInQueue:integer;
  begin
  if FSerialized then
    begin
    if assigned(FThr) then
      Result:=TRtcThread.JobsInQueue(FThr)
    else
      Result:=-1;
    end
  else
    Result:=-1;
  end;

function TRtcQuickJob.JobsTotal:integer;
  begin
  if FSerialized then
    begin
    if assigned(FThr) then
      Result:=TRtcThread.JobsTotal(FThr)
    else
      Result:=-1;
    end
  else
    Result:=-1;
  end;

function TRtcQuickJob.JobRunning:boolean;
  begin
  if FSerialized then
    begin
    if assigned(FThr) then
      Result:=TRtcThread.JobRunning(FThr)
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

procedure TRtcQuickJob.Stop;
  begin
  if assigned(FThr) then
    begin
    TRtcThread.Stop(FThr);
    FThr:=nil;
    end;
  end;

{ TRtcMyQuickJob }

type
  TRtcMyQuickJob=class(TRtcJob)
  private
    FData:TRtcValue;
    FSync:boolean;
    FEvent:TRtcQuickJobEvent;
    FTemp:boolean;
  public
    destructor Destroy; override;
    procedure Execute;

    function Run(Thr:TRtcThread):boolean; override;
    end;

destructor TRtcMyQuickJob.Destroy;
  begin
  FEvent:=nil;
  RtcFreeAndNil(FData);
  inherited;
  end;

function TRtcMyQuickJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    if not RtcThreadPoolReady then
      RtcFreeAndNil(FData)
  	else if FSync then
      TRtcThread.Sync(Execute)
    else
      Execute;
  finally
    Result:=True;
    if FTemp then
      TRtcThread.Stop(Thr);
    end;
  end;

procedure TRtcMyQuickJob.Execute;
  begin
  try
    if assigned(FEvent) then
      FEvent(FData);
  except
    // ignore all exceptions here
    end;
  RtcFreeAndNil(FData);
  FEvent:=nil;
  end;

procedure PostQuickJob(Event:TRtcQuickJobEvent; const Data:TRtcValueObject=nil; AccessGUI:boolean=False; UseThread:TRtcThread=nil; ForceThread:boolean=False);
  var
    Thr:TRtcThread;
    Job:TRtcMyQuickJob;
  begin
  if not assigned(Event) then
    raise Exception.Create('"Event" required to execute the job');

  Job:=TRtcMyQuickJob.Create;
  if (Data<>nil) and (Data is TRtcValue) then
    Job.FData:=TRtcValue(Data)
  else
    begin
    Job.FData:=TRtcValue.Create;
    Job.FData.asObject:=Data;
    end;
  Job.FSync:=AccessGUI;
  Job.FEvent:=Event;

  if UseThread=nil then
    begin
    Thr:=TRtcThread.Create;
    Job.FTemp:=True;
    end
  else
    begin
    Thr:=UseThread;
    Job.FTemp:=False;
    end;

  if not TRtcThread.PostJob(Thr,Job,False,ForceThread) then
    begin
    if Job.FTemp then
      TRtcThread.Stop(Thr);
    RtcFreeAndNil(Job);
    end;
  end;

{ TRtcMyGUIJob }

type
  TRtcMyGUIJob=class(TRtcJob)
  private
    FEvent:TRtcSyncEvent;
    FTemp:boolean;
  public
    destructor Destroy; override;
    procedure Execute;

    function Run(Thr:TRtcThread):boolean; override;
    end;

function TRtcMyGUIJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    if RtcThreadPoolReady then
      TRtcThread.Sync(Execute);
  finally
    Result:=True;
    if FTemp then
      TRtcThread.Stop(Thr);
    end;
  end;

procedure TRtcMyGUIJob.Execute;
  begin
  try
    if assigned(FEvent) then
      FEvent;
  except
    // ignore all exceptions here
    end;
  FEvent:=nil;
  end;

destructor TRtcMyGUIJob.Destroy;
  begin
  FEvent:=nil;
  inherited;
  end;

procedure PostGUIJob(Event:TRtcSyncEvent; UseThread:TRtcThread=nil);
  var
    Job:TRtcMyGUIJob;
  begin
  if not assigned(Event) then
    raise Exception.Create('"Event" required to execute the job');

  Job:=TRtcMyGUIJob.Create;
  Job.FEvent:=Event;
  if UseThread=nil then
    begin
    UseThread:=TRtcThread.Create;
    Job.FTemp:=True;
    end
  else
    Job.FTemp:=False;

  if not TRtcThread.PostJob(UseThread,Job) then
    begin
    if Job.FTemp then
      TRtcThread.Stop(UseThread);
    RtcFreeAndNil(Job);
    end;
  end;

{$IFDEF RTC_ANON_METHODS}

{ TRtcMyQuickAnonJob }

type
  TRtcMyQuickAnonJob=class(TRtcJob)
  private
    FData:TRtcValue;
    FSync:boolean;
    FMethod:TRtcQuickAnonMethod;
    FTemp:boolean;
  public
    destructor Destroy; override;
    procedure Execute;

    function Run(Thr:TRtcThread):boolean; override;
    end;

destructor TRtcMyQuickAnonJob.Destroy;
  begin
  FMethod:=nil;
  RtcFreeAndNil(FData);
  inherited;
  end;

function TRtcMyQuickAnonJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    if not RtcThreadPoolReady then
      RtcFreeAndNil(FData)
  	else if FSync then
      TRtcThread.Sync(Execute)
    else
      Execute;
  finally
    Result:=True;
    if FTemp then
      TRtcThread.Stop(Thr);
    end;
  end;

procedure TRtcMyQuickAnonJob.Execute;
  begin
  try
    if assigned(FMethod) then
      FMethod(FData);
  except
    // ignore all exceptions here
    end;
  RtcFreeAndNil(FData);
  FMethod:=nil;
  end;

procedure PostQuickJob(AnonMethod:TRtcQuickAnonMethod; const Data:TRtcValueObject=nil; AccessGUI:boolean=False; UseThread:TRtcThread=nil; ForceThread:boolean=False);
  var
    Thr:TRtcThread;
    Job:TRtcMyQuickAnonJob;
  begin
  if not assigned(AnonMethod) then
    raise Exception.Create('"AnonMethod" required to execute the job');

  Job:=TRtcMyQuickAnonJob.Create;
  if (Data<>nil) and (Data is TRtcValue) then
    Job.FData:=TRtcValue(Data)
  else
    begin
    Job.FData:=TRtcValue.Create;
    Job.FData.asObject:=Data;
    end;
  Job.FSync:=AccessGUI;
  Job.FMethod:=AnonMethod;

  if UseThread=nil then
    begin
    Thr:=TRtcThread.Create;
    Job.FTemp:=True;
    end
  else
    begin
    Thr:=UseThread;
    Job.FTemp:=False;
    end;

  if not TRtcThread.PostJob(Thr,Job,False,ForceThread) then
    begin
    if Job.FTemp then
      TRtcThread.Stop(Thr);
    RtcFreeAndNil(Job);
    end;
  end;

{ TRtcMyQuickAnonJob }

type
  TRtcMySyncAnonJob=class(TRtcJob)
  private
    FSync:boolean;
    FMethod:TRtcSyncAnonMethod;
    FTemp:boolean;
  public
    destructor Destroy; override;
    procedure Execute;

    function Run(Thr:TRtcThread):boolean; override;
    end;

destructor TRtcMySyncAnonJob.Destroy;
  begin
  FMethod:=nil;
  inherited;
  end;

function TRtcMySyncAnonJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    if RtcThreadPoolReady then
  	  if FSync then
        TRtcThread.Sync(Execute)
      else
        Execute;
  finally
    Result:=True;
    if FTemp then
      TRtcThread.Stop(Thr);
    end;
  end;

procedure TRtcMySyncAnonJob.Execute;
  begin
  try
    if assigned(FMethod) then
      FMethod;
  except
    // ignore all exceptions here
    end;
  FMethod:=nil;
  end;

procedure PostQuickJob(AnonMethod:TRtcSyncAnonMethod; AccessGUI:boolean=False; UseThread:TRtcThread=nil; ForceThread:boolean=False);
  var
    Thr:TRtcThread;
    Job:TRtcMySyncAnonJob;
  begin
  if not assigned(AnonMethod) then
    raise Exception.Create('"AnonMethod" required to execute the job');

  Job:=TRtcMySyncAnonJob.Create;
  Job.FSync:=AccessGUI;
  Job.FMethod:=AnonMethod;

  if UseThread=nil then
    begin
    Thr:=TRtcThread.Create;
    Job.FTemp:=True;
    end
  else
    begin
    Thr:=UseThread;
    Job.FTemp:=False;
    end;

  if not TRtcThread.PostJob(Thr,Job,False,ForceThread) then
    begin
    if Job.FTemp then
      TRtcThread.Stop(Thr);
    RtcFreeAndNil(Job);
    end;
  end;

{ TRtcMyGUIAnonJob }

type
  TRtcMyGUIAnonJob=class(TRtcJob)
  private
    FMethod:TRtcSyncAnonMethod;
    FTemp:boolean;
  public
    destructor Destroy; override;

    procedure Execute;

    function Run(Thr:TRtcThread):boolean; override;
    end;

function TRtcMyGUIAnonJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    if RtcThreadPoolReady then
      TRtcThread.Sync(Execute);
  finally
    Result:=True;
    if FTemp then
      TRtcThread.Stop(Thr);
    end;
  end;

procedure TRtcMyGUIAnonJob.Execute;
  begin
  try
    if assigned(FMethod) then
      FMethod;
  except
    // ignore all exceptions here
    end;
  FMethod:=nil;
  end;

destructor TRtcMyGUIAnonJob.Destroy;
  begin
  FMethod:=nil;
  inherited;
  end;

procedure PostGUIJob(AnonMethod:TRtcSyncAnonMethod; UseThread:TRtcThread=nil);
  var
    Job:TRtcMyGUIAnonJob;
  begin
  if not assigned(AnonMethod) then
    raise Exception.Create('"AnonMethod" required to execute the job');

  Job:=TRtcMyGUIAnonJob.Create;
  Job.FMethod:=AnonMethod;

  if UseThread=nil then
    begin
    UseThread:=TRtcThread.Create;
    Job.FTemp:=True;
    end
  else
    Job.FTemp:=False;

  if not TRtcThread.PostJob(UseThread,Job) then
    begin
    if Job.FTemp then
      TRtcThread.Stop(UseThread);
    RtcFreeAndNil(Job);
    end;
  end;

{$ENDIF}

{ TRtcThreadQuickJob }

type
  TRtcThreadQuickJob=class(TRtcJob)
  private
    FData:TRtcValue;
    FSync:boolean;
    FEvent:TRtcQuickJobEvent;
  public
    destructor Destroy; override;
    procedure Execute;

    function Run(Thr:TRtcThread):boolean; override;
    end;

destructor TRtcThreadQuickJob.Destroy;
  begin
  FEvent:=nil;
  RtcFreeAndNil(FData);
  inherited;
  end;

function TRtcThreadQuickJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    if not RtcThreadPoolReady then
  	  RtcFreeAndNil(FData)
    else if FSync then
      TRtcThread.Sync(Execute)
    else
      Execute;
  finally
    Result:=True;
    end;
  end;

procedure TRtcThreadQuickJob.Execute;
  begin
  try
    if assigned(FEvent) then
      FEvent(FData);
  except
    // ignore all exceptions here
    end;
  RtcFreeAndNil(FData);
  FEvent:=nil;
  end;

{ TRtcThreadEx }

constructor TRtcThreadEx.Create;
  begin
  inherited;
  FInfo:=TRtcInfo.Create;
  end;

function TRtcThreadEx.Finalize:boolean;
  begin
  Result:=inherited Finalize;
  if Result then
    RtcFreeAndNil(FInfo);
  end;

class function TRtcThreadEx.PostEvent(me:TObject; Event:TRtcQuickJobEvent; Data:TRtcValueObject=nil; AccessGUI:boolean=False; HighPriority:boolean=False; ForceThread:boolean=False):boolean;
  var
    Job:TRtcThreadQuickJob;
  begin
  Result:=False;
  if not (RtcThreadPoolReady and assigned(Event) and assigned(me)) then
    begin
    RtcFreeAndNil(Data);
    Exit;
    end;
  Job:=TRtcThreadQuickJob.Create;
  try
    if (Data<>nil) and (Data is TRtcValue) then
      Job.FData:=TRtcValue(Data)
    else
      begin
      Job.FData:=TRtcValue.Create;
      Job.FData.asObject:=Data;
      end;
    Job.FSync:=AccessGUI;
    Job.FEvent:=Event;
    Result:=TRtcThread.PostJob(me,Job,HighPriority,ForceThread);
  finally
    if not Result then
      if Job.SingleUse then
        RtcFreeAndNil(Job)
    end;
  end;

end.
