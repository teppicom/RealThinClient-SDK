{
  @html(<b>)
  Thread Pool
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  Thread pooling mechanism used by all RTC connection components
  when component's @Link(TRtcConnection.MultiThreaded) property is set to True.
  @html(<br><br>)

  Unless you want to enhance the connection components or add your
  own connection providers, you will NEVER get in direct contact
  with this classes. They are being used internaly by most
  Connection Provider components to enable MultiThreaded execution.
  @html(<br><br>)

  The only thing you could get in contact with as a component user
  are the global Threading parameters @Link(RTC_THREAD_POOL_PLUS),
  @Link(RTC_THREAD_POOL_OVERSIZE), @Link(RTC_THREAD_POOL_MAX)
  and @Link(RTC_THREAD_POOL_LIMIT). @html(<br><br>)

  Or, in case you need to post jobs to a connection component
  to enhance its functionality, with the @Link(TRtcJob) class.
}
unit rtcThrPool;

{$INCLUDE rtcDefs.inc}

interface

uses
{$IFDEF WINDOWS}
  Windows,
  Messages,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}

  SysUtils,
  Classes,

  rtcTypes,
  rtcSystem,
  rtcSrcList,
  rtcLog;

var
  // Max. number of unused threads to keep active
  RTC_THREAD_POOL_OVERSIZE:word=256;
  // Max. number of "normal" Threads in our thread pool.
  RTC_THREAD_POOL_MAX:word=128;
  // Absolute Thread Pool Limit, including high priority threads.
  RTC_THREAD_POOL_LIMIT:word=256;
  // Maximum time (in seconds) allowed for all worker threads to close when shutting down
  RTC_THREAD_POOL_CLOSEWAIT:word=30;
  // Maximum time (in seconds) allowed for the last worker thread to close when shutting down
  RTC_THREAD_POOL_CLOSELAST:word=5;
  // "Sleep" time (in milliseconds) after every executed job
  RTC_THREAD_SLEEP:integer=0;
  // Time limit (milliseconds) for running multiple events in the Main Thread
  RTC_THREAD_SYNCLIMIT:integer=250;

  // Thread Priority
{$IFDEF WINDOWS}
  RTC_THREAD_PRIORITY:TThreadPriority=tpNormal;
{$ENDIF}

  // Log unhandled thread exceptions?
  LOG_THREAD_EXCEPTIONS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

  { "RtcWaitFor" Sleep time (ms) between "ConditionToStopWaiting" checks }
  RTC_WAITFORCONDITION_SLEEP:cardinal=16;

type
  { wait_OK = wait over, everything OK.
    wait_Timeout = wait timed out, but we are not finished yet.
    wait_Quit = application Terminating, but we are not finished yet.
    wait_Msg = loop terminated because of a user message which could not be processed.
    wait_Error = connection error. }
  TRtcWaitForConditionResult=(wait_OK, wait_Timeout, wait_Quit, wait_Msg, wait_Error);

  { @Abstract(Exception to be raised when System Thread limit was reached and not a single thread could be created)
    @exclude }
  ERtcThreadLimitReached = class(Exception);

  { @Abstract(Exception to be raised if the "Sync" method is used when the Thread Pool is closed or closing)
    @exclude }
  ERtcThreadPoolClosed = class(Exception);

  // Event for Synchronized calls
  TRtcSyncEvent = procedure of object;

  TRtcThread = class;

  // @exclude
  TRtcBaseMessage=class(TObject);

  { @Abstract(RTC Job class)

    To be able to post jobs to a threaded connection component,
    you can derive your own classes from @Link(TRtcJob). By implementing
    the methods @Link(TRtcJob.Run) and @Link(TRtcJob.SingleUse), you can post
    any job with your user-defined data to the connection component's thread. }
  TRtcJob = class(TObject)

    { Implement the "SingleUse" method to return TRUE if the object
      should be released from memory (single-use, one-time jobs), or
      return FALSE if the object should NOT be released from memory after use.
      By default, the "SingleUse" method of the "TRtcJob" class returns *TRUE*. }
    function SingleUse:boolean; virtual;

    { This method will be called ONCE to run (execute) the job.

      Return TRUE if the Job object should be released
      from memory after executing the Run method.

      If you post jobs to connection components,
      handle your expected exceptions properly.

      Exceptions caught by the Threading mechanism will
      not be passed any further. If exception gets raised and it
      returns to the Threading mechanism, the corresponding Thread
      object will be closed, all jobs will be Killed and the Thread
      will be released from memory, which will result in a disconnect. }
    function Run(Thr:TRtcThread):boolean; virtual; abstract;
    end;

  { @Abstract(Thread start/stop callback class) }
  TRtcThreadCallback = class(TObject)
  public
    { Called from inside each Thread, after it was started/created }
    procedure AfterThreadStart; virtual; abstract;
    { Called from inside each Thread, before it will be stopped/destroyed }
    procedure BeforeThreadStop; virtual; abstract;
    { Callled after all threads have been stopped.
      This is the method from which you should destroy the object by calling "Free" }
    procedure DestroyCallback; virtual; abstract;
    end;

  { @Abstract(Our threading class)

    We create threads ONLY using this class.
    This class implements all methods needed for synchronizing
    with the GUI, posting jobs and stopping the thread. }
  TRtcThread = class(TObject)
  private
    MsgList:TXObjList;
    Working,
    Waiting:boolean;
    Pending:integer;
    Active:boolean;
    Killed:boolean;
    FThr:TObject;

    FNeedThread:boolean;

    procedure GetJob;

    procedure Idle;

  protected
    Job:TObject;

    procedure InternalKill;

    function Finalize:boolean; virtual;

    { Called by the Worker Thread to execute the "Job".
      For user-defined jobs (the ones not derived from TRtcJob),
      you need to override this method and call the inherited RunJob.
      Return TRUE if Thread has to be released. }
    function RunJob:boolean; virtual;

    { Called by the Worket Thread to kill the currently assigned "Job".
      For user-defined jobs (the ones not derived from TRtcJob),
      you need to override this method and call the inherited KillJob method. }
    procedure KillJob; virtual;

  public
    // Create a Thread. To give the thread something to do, you will have to post a job to it.
    constructor Create; virtual;
    { @exclude }
    destructor Destroy; override;

    // Synchronize 'Event' with the Main Thread (for GUI access) - can be used from any RTC Thread.
    // If the "Sync" method is called from any RTC Thread, 'Event' is posted to the Main Thread and
    // the "Sync" method will wait until the event finishes, then it will return TRUE.
    // If the "Sync" method is called from the Main Thread, it will do nothing and return FALSE immediately.
    class function Sync(Event:TRtcSyncEvent):boolean;

    // Lock threads
    class function Lock(me:TObject):boolean;

    // Unlock threads
    class procedure UnLock;

    // add job "myJob" to "me" thread's execution queue (thread-safe call)
    class function PostJob(me:TObject; var myJob; HighPriority:boolean=False; ForceThread:boolean=False):boolean;

    // Create a TRtcJob to call an Event,
    // Can be used to speed up frequent calls to the same event
    // Returned TRtcJob instance can be used any number of times,
    // but should be destroyed manually when it is no longer needed.
    class function GetJobForEvent(Event:TRtcSyncEvent; AccessGUI:boolean=False):TRtcJob; overload;

    // Post event for the thread (thread-safe call)
    class function PostEvent(me:TObject; Event:TRtcSyncEvent; AccessGUI:boolean=False; HighPriority:boolean=False; ForceThread:boolean=False):boolean; overload;

    // Stop the thread (thread-safe call: will post a QUIT message to thread and destroy it from inside the thread)
    class procedure Stop(me:TObject);

    // returns the number of jobs in the virtual thread's job queue (waiting to be executed, but not yet running)
    class function JobsInQueue(me:TObject):integer;

    { returns the total number of jobs in the virtual thread's job list,
      which includes the queue and the job currently running. }
    class function JobsTotal(me:TObject):integer;

    // returns TRUE if the virtual thread is currently running (executing) a job
    class function JobRunning(me:TObject):boolean;

    // is thread "me" still valid (exist)?
    class function Exists(me:TObject):boolean;

    // are we currently inside the background thread "me" ?
    class function InsideBackThread(me:TObject):boolean;

    // Return TRUE if we are currently inside this thread
    function InsideThread:boolean;

    // Return TRUE if we are currently inside this thread, executing in the background
    function InBackThread:boolean;

    property NeedThread:boolean read FNeedThread write FNeedThread;
    end;

type
  // Event used with the "RtcWaitFor" function
  TRtcConditionEvent=function:boolean of object;

  TRtcSyncProc = procedure(Proc:TRtcSyncEvent);
  TRtcSyncCheckProc = procedure(var done:boolean);

var
  rtcSyncProc : TRtcSyncProc = nil;
  rtcSyncCheckProc : TRtcSyncCheckProc = nil;

{ Wait for a custom condition. @html(<br>)
  Using a "_Timeout" (seconds) you can specify how long you want to wait (0=unlimited).
  Returns wait_OK if "ConditionToStopWaiting" and "ConditionToReturnOK" returned TRUE.
  Returns wait_Timeout if Timed out, but "ConditionToStopWaiting" returned FALSE.
  Returns wait_Msg if unknown message received, but "ConditionToStopWaiting" returned FALSE.
  Returns wait_Quit if Application terminating, but "ConditionToStopWaiting" returned FALSE.
  Returns wait_Error if "ConditionToStopWaiting" returned TRUE, but "ContitionToReturnOK" returned FALSE. }
function RtcWaitFor(const ConditionToStopWaiting, ConditionToReturnOK: TRtcConditionEvent;
                    _Timeout:cardinal=0; UserInteractionAllowed:boolean=False;
                    AllowMessageProcessing:boolean=True): TRtcWaitForConditionResult;

{ Check if there are Sync() calls waiting and execute one burst.
  This procedure may ONLY be called from the MAIN Thread!
  Returns TRUE if at least one Sync call was executed. }
function rtcSyncCheck:boolean;

{ Add a new Thread Callback.

  Please note that you can NOT remove a callback and that you need
  to add all callbacks before a first thread was created, which is best
  done from your units "initialization" section. To avoid memory leaks on
  application close, you should also implement the "DestroyCallback" method. }
procedure AddThreadCallback(const Callback:TRtcThreadCallback);

{ Return TRUE if we are inside the Main Thread now }
function InsideMainThread:boolean;

{ Returns the number of Busy Worker Threads.
  This call is almost as expensive as posting a job to the Thread Pool,
  because it has to acquire and release a lock on the RTC Thread Pool,
  so ... do NOT call it very often, to avoid a performance penalty. }
function RtcTotalThreadsBusy:integer;

{ Returns the number of Idle Worker Threads.
  This call is almost as expensive as posting a Job to the Thread Pool,
  because it has to acquire and release a lock on the RTC Thread Pool,
  so ... do NOT call it very often, to avoid a performance penalty. }
function RtcTotalThreadsIdle:integer;

{ Is the RTC Thread Pool ready? This function ONLY has to check the
  state of a global boolean variable, so it is safe to be caled often. }
function RtcThreadPoolReady:boolean;

{ Returns the number of Jobs in the RTC Thread Queue, waiting for execution.
  This function ONLY has to check the state of a global integer variable,
  so it can be called often - without a serious performance penalty. }
function RtcTotalJobsQueued:int64;

procedure OpenThreadPool;
procedure CloseThreadPool;

implementation

type
  TRtcWorkerThread = class(TThread)
  private
    Work: TRtcThread;

  protected
    Run:TRtcEvent;
    FEvent:TRtcSyncEvent;
    FInsideMain:boolean;
    FFinished:boolean;
    FForced:boolean;

    procedure Execute; override;
    procedure Sync(Event:TRtcSyncEvent);

  public
    procedure MySyncEvent;

    class function InsideThread(me:TObject):boolean;
    class function InBackThread(me:TObject):boolean;

    procedure PostQuit;
    procedure PostWork(Thr:TRtcThread);

    constructor Create(CreateSuspended,Force:boolean);
    destructor Destroy; override;

    property Forced:boolean read FForced;
    end;

  TRtcEventEx=class(TRtcEvent)
  public
    X:TObject;
    end;
  TProcEv=class(TObject)
  public
    P:TRtcSyncEvent;
    E:TRtcEventEx;
    end;
  TSyThread=class(TThread)
  public
    constructor Create(CreateSuspended:boolean);
    destructor Destroy; override;

    procedure Execute; override;
    procedure SyncExecute;
  end;

var
  SyThr_Running:boolean;
  Threads_Running:boolean;

var
  SyReady:boolean;
  SyEV,
  SyOpen:TRtcEvent;
  SyList:tXObjList;
  SyCS:TRtcCritSec;

procedure rtcSyncQuit;
  var
    SyObj:TProcEv;
    xob:TObject absolute SyObj;
  begin
  xob:=nil;
  SyCS.Acquire;
  try
    SyList.extractFirst(xob);
  finally
    SyCS.Release;
    end;
  while assigned(xob) do
    begin
    SyObj.E.SetEvent;
    SyObj:=nil;

    SyCS.Acquire;
    try
      SyList.extractFirst(xob);
    finally
      SyCS.Release;
      end;
    end;
  end;

{ Is the RTC Thread Pool ready? }
function RtcThreadPoolReady:boolean;
  begin
  Result:=Threads_Running;
  end;

function RtcWaitFor(const ConditionToStopWaiting, ConditionToReturnOK: TRtcConditionEvent;
                    _Timeout:cardinal=0; UserInteractionAllowed:boolean=False;
                    AllowMessageProcessing:boolean=True): TRtcWaitForConditionResult;
{$IFDEF WINDOWS}
  var
    Msg:TMsg;
    useTimeout:boolean;
    inMain:boolean;

  function PeekMsgNoUser:boolean;
    begin
    Result:=PeekMessage(Msg,0,WM_USER,$FFFF,PM_REMOVE) or
            PeekMessage(Msg,0,0,WM_KEYFIRST-1,PM_REMOVE);
    end;

  function PeekMsgBackground:boolean;
    begin
    Result:=PeekMessage(Msg,0,0,WM_USER-1,PM_REMOVE) or
              PeekMessage(Msg,0,WM_USER,$FFFF,PM_NOREMOVE);
    end;

  function PeekMsgAll:boolean;
    begin
    Result:=PeekMessage(Msg,0,0,0,PM_REMOVE);
    end;

  begin
  if ConditionToStopWaiting then
    begin
    if ConditionToReturnOK then
      Result:=wait_OK
    else
      Result:=wait_Error;
    end
  else if not Threads_Running then
    Result:=wait_Error
  else if not (AllowMessageProcessing or UserInteractionAllowed) then
    begin
    Result:=wait_Error;
    inMain:=InsideMainThread;
    useTimeout:=_Timeout>0;
    if useTimeout then
      if RTC_WAITFORCONDITION_SLEEP>0 then
        _Timeout:=_Timeout*1000 div RTC_WAITFORCONDITION_SLEEP
      else
        _Timeout:=_Timeout*1000;
    repeat
      if useTimeout then
        if _Timeout>0 then
          Dec(_Timeout)
        else
          begin
          Result:=wait_Timeout;
          Break;
          end;
      if inMain then rtcSyncCheck;
      Sleep(RTC_WAITFORCONDITION_SLEEP);
      until ConditionToStopWaiting or not Threads_Running;
    if ConditionToReturnOK then
      Result:=wait_OK;
    end
  else
    begin
    // Need a Message Loop
    useTimeout:=_Timeout>0;
    if useTimeout then
      if RTC_WAITFORCONDITION_SLEEP>0 then
        _Timeout:=_Timeout*1000 div RTC_WAITFORCONDITION_SLEEP
      else
        _Timeout:=_Timeout*1000;

    if not InsideMainThread then // When used from a Service
      begin
      Result:=wait_Error;
      repeat
        if useTimeout then
          if _Timeout>0 then
            Dec(_Timeout)
          else
            begin
            Result:=wait_Timeout;
            Break;
            end;
        while PeekMsgBackground do
          begin
          if Msg.message>=WM_USER then
            begin
            Result:=wait_Msg;
            Break;
            end
          else if (Msg.message=WM_QUIT) then
            begin
            Result:=wait_Quit;
            Break;
            end
          else
            begin
            TranslateMessage( Msg );
            DispatchMessage( Msg );
            end;
          end;
        if ConditionToStopWaiting then
          Result:=wait_OK
        else if Result=wait_Error then
          Sleep(RTC_WAITFORCONDITION_SLEEP);
        until (Result<>wait_Error) or not Threads_Running;

      if Result=wait_OK then
        if not ConditionToReturnOK then
          Result:=wait_Error;
      end
    else if UserInteractionAllowed then
      begin
      Result:=wait_Error;
      repeat
        if useTimeout then
          if _Timeout>0 then
            Dec(_Timeout)
          else
            begin
            Result:=wait_Timeout;
            Break;
            end;
        rtcSyncCheck;
        while PeekMsgAll do
          begin
          if (Msg.message=WM_QUIT) then
            begin
            Result:=wait_Quit;
            Break;
            end
          else
            begin
            TranslateMessage( Msg );
            DispatchMessage( Msg );
            end;
          end;
        if ConditionToStopWaiting then
          Result:=wait_OK
        else if Result=wait_Error then
          Sleep(RTC_WAITFORCONDITION_SLEEP);
        until (Result<>wait_Error) or not Threads_Running;

      if Result=wait_OK then
        if not ConditionToReturnOK then
          Result:=wait_Error;
      end
    else
      begin
      Result:=wait_Error;
      repeat
        if useTimeout then
          if _Timeout>0 then
            Dec(_Timeout)
          else
            begin
            Result:=wait_Timeout;
            Break;
            end;
        rtcSyncCheck;
        while PeekMsgNoUser do
          begin
          if (Msg.message=WM_QUIT) then
            begin
            Result:=wait_Quit;
            Break;
            end
          else
            begin
            TranslateMessage( Msg );
            DispatchMessage( Msg );
            end;
          end;
        if ConditionToStopWaiting then
          Result:=wait_OK
        else if Result=wait_Error then
          Sleep(RTC_WAITFORCONDITION_SLEEP);
        until (Result<>wait_Error) or not Threads_Running;

      if Result=wait_OK then
        if not ConditionToReturnOK then
          Result:=wait_Error;
      end;
    end;
  end;
{$ELSE}
  {$IFDEF FPC_POSIX}
  var
    useTimeout,
    inMain:boolean;
  begin
  if ConditionToStopWaiting then
    begin
    if ConditionToReturnOK then
      Result:=wait_OK
    else
      Result:=wait_Error;
    end
  else
    begin
    Result:=wait_Error;
    inMain:=InsideMainThread;
    useTimeout:=_Timeout>0;
    if useTimeout then
      if RTC_WAITFORCONDITION_SLEEP>0 then
        _Timeout:=_Timeout*1000 div RTC_WAITFORCONDITION_SLEEP
      else
        _Timeout:=_Timeout*1000;
    repeat
      if useTimeout then
        if _Timeout>0 then
          Dec(_Timeout)
        else
          begin
          Result:=wait_Timeout;
          Break;
          end;
      if inMain then rtcSyncCheck;
      Sleep(RTC_WAITFORCONDITION_SLEEP);
      until ConditionToStopWaiting or not Threads_Running;
    if ConditionToReturnOK then
      Result:=wait_OK;
    end;
  end;
  {$ELSE}
  begin
  Result:=wait_Error;
  {$MESSAGE WARN 'rtcThrPool.pas unit -> "rtcWaitFor" implementation missing.'}
  end;
  {$ENDIF}
{$ENDIF}

function rtcSyncCheck:boolean;
  var
    res:boolean;
  begin
  if assigned(rtcSyncCheckProc) then
    begin
    rtcSyncCheckProc(Res);
    Result:=Res;
    end
  else
    Result:=False;
  end;

var
  { @exclude }
  MainThrID:RtcThrID;

function InsideMainThread:boolean;
  begin
  Result:=GetMyThreadID=MainThrID;
  end;

var
  Jobs_In_Queue:int64;

  ThreadPtrPool:TObjList; // all running threads (sorted by TRtcWorkerThread pointers)
  ThreadIdPool:TObjList; // all running threads (sorted by Thread IDs)
  NormalThreadCnt,
  ForcedThreadCnt:integer;
  ForcePool,           // "forced" threads not in use
  FreePool:TXObjList; // "normal" threads not in use (not sorted -> add/remove last)

  ThrList:tObjList; // list of all thread objects (sorted for fast searching)
  WaitList:tXObjList; // list of all thread objects waiting for execution

  Message_Quit:TRtcBaseMessage;

  CSThread:TRtcCritSec;

  InsideCallback:integer=0;
  ThreadCallbacks:array of TRtcThreadCallback;
  ThreadCallbackCount:integer=0;
  HaveThreadCallbacks:boolean=False;

  OpenCnt:integer;
  CSOpen:TRtcEvent;

function RtcTotalJobsQueued:int64;
  begin
  Result:=Jobs_In_Queue;
  end;

{ Add a new Thread Callback }
procedure AddThreadCallback(const Callback:TRtcThreadCallback);
  begin
  CSThread.Acquire;
  try
    HaveThreadCallbacks:=True;
    Inc(ThreadCallbackCount);
    SetLength(ThreadCallbacks, ThreadCallbackCount);
    ThreadCallbacks[ThreadCallbackCount-1]:=Callback;
  finally
    CSThread.Release;
    end;
  end;

{ Remove all Thread Callbacks }
procedure RemoveThreadCallbacks;
  var
    a:integer;
  begin
  if HaveThreadCallbacks then
    begin
    for a:=0 to ThreadCallbackCount-1 do
      begin
      try
        ThreadCallbacks[a].DestroyCallback;
      except
        on E:Exception do
          if LOG_THREAD_EXCEPTIONS then
            Log('RemoteThreadCallbacks TRtcThreadCallback.DestroyCallback',E,'THREAD');
        end;
      ThreadCallbacks[a]:=nil;
      end;
    SetLength(ThreadCallbacks,0);
    ThreadCallbackCount:=0;

    HaveThreadCallbacks:=False;
    end;
  end;

procedure DoAfterThreadStart;
  var
    i:integer;
  begin
  if HaveThreadCallbacks then
    begin
    CSThread.Acquire;
    try
      if ThreadCallbackCount>0 then
        begin
        Inc(InsideCallback);
        for i:=0 to ThreadCallbackCount-1 do
          try
            ThreadCallbacks[i].AfterThreadStart;
          except
            on E:Exception do
              if LOG_THREAD_EXCEPTIONS then
                Log('DoAfterThreadStart TRtcThreadCallback.AfterThreadStart',E,'THREAD');
            end;
        end;
    finally
      CSThread.Release;
      end;
    end;
  end;

procedure DoBeforeThreadStop;
  var
    i:integer;
  begin
  if HaveThreadCallbacks then
    begin
    CSThread.Acquire;
    try
      if ThreadCallbackCount>0 then
        begin
        for i:=ThreadCallbackCount-1 downto 0 do
          try
            ThreadCallbacks[i].BeforeThreadStop;
          except
            on E:Exception do
              if LOG_THREAD_EXCEPTIONS then
                Log('DoBeforeThreadStop TRtcThreadCallback.BeforeThreadStop',E,'THREAD');
            end;
        Dec(InsideCallback);
        if InsideCallback=0 then
          RemoveThreadCallbacks;
        end;
    finally
      CSThread.Release;
      end;
    end;
  end;

{ Work pool }

{ Returns the number of Busy Worker Threads. }
function RtcTotalThreadsBusy:integer;
  begin
  if Threads_Running then
    begin
    CSThread.Acquire;
    try
      Result:=ThreadPtrPool.Count-FreePool.Count-ForcePool.Count;
    finally
      CSThread.Release;
      end;
    end
  else
    Result:=0;
  end;

{ Returns the number of Idle Worker Threads. }
function RtcTotalThreadsIdle:integer;
  begin
  if Threads_Running then
    begin
    CSThread.Acquire;
    try
      Result:=FreePool.Count+ForcePool.Count;
    finally
      CSThread.Release;
      end;
    end
  else
    Result:=0;
  end;

function GetWork:TRtcThread; // get next waiting object (remove it from waiting list, add it to working list)
  var
    ores:TObject absolute Result;
  begin
  Result:=nil;
  if WaitList.Count>0 then
    begin
    repeat
      WaitList.extractFirst(ores); // extract from waiting list
      Result.Waiting:=False;
      Result.GetJob;
      until assigned(Result.Job) or (WaitList.Count=0);

    if assigned(Result.Job) then
      Result.Working:=True
    else
      Result:=nil;
    end;
  end;

{ Thread Pool }

procedure OpenThreadPool;
  begin
  Threads_Running:=True;
  end;

function ReturnThread(Thr:TRtcWorkerThread):boolean; // executed 1 object, returning for another
  var
    Work:TRtcThread;
  begin
  if not Threads_Running or Thr.FFinished then
    Result:=False
  else
    begin
    Work:=GetWork;
    if Work<>nil then // execution object waiting
      begin
      Thr.PostWork(Work);
      Result:=True;
      end
    else if Thr.Forced then
      begin
      ForcePool.AddFirst(Thr);
      Result:=True;
      end
    else if FreePool.Count<RTC_THREAD_POOL_OVERSIZE then
      begin
      FreePool.AddFirst(Thr);
      Result:=True;
      end
    else
      Result:=False;
    end;
  end;

function GetThread(Forced:boolean):TRtcWorkerThread;
  var
    ores:TObject absolute Result;
  begin
  Result:=nil;
  try
    if Forced and (ForcePool.Count>0) then
      ForcePool.extractLast(ores)
    else if FreePool.Count>0 then // threads available
      FreePool.extractLast(ores) // extract from free threads list
    else if Forced then
      begin
      if ThreadPtrPool.Count<RTC_THREAD_POOL_LIMIT then
        TRtcWorkerThread.Create(False,True);
      end
    else
      begin
      if ThreadPtrPool.Count<RTC_THREAD_POOL_MAX then
        TRtcWorkerThread.Create(False,False);
      end;
  except
    on E:Exception do
      begin
      if LOG_THREAD_EXCEPTIONS then
        Log('GetThread',E,'THREAD');
      end;
    end;
  end;

function WaitForClose(_timeout,_endtime:cardinal):boolean;
  var
  {$IFDEF WINDOWS}
    Msg:TMsg;
  {$ENDIF}
    MyTime,ChgTime:cardinal;
    NowCnt,TmpCnt:integer;
  begin
  NowCnt:=OpenCnt;
  Result:=False;
  MyTime:=_Timeout*100+Cardinal(NowCnt*10); // add 0,1 sec allowed waiting time per running thread
  _endtime:=_endtime*100;
  ChgTime:=MyTime;
  while (CSOpen.WaitFor(0)<>wr_Signaled) do
    begin
    if MyTime>0 then
      begin
      Sleep(10);
      TmpCnt:=OpenCnt;
      if TmpCnt<NowCnt then
        begin // at least 1 worker thread closed
        Inc(MyTime); // we can wait a bit longer
        ChgTime:=MyTime;
        NowCnt:=TmpCnt;
        end
      else // no threads closed, count down
        begin
        Dec(MyTime);
        if ChgTime>MyTime+_endtime then // no threads closed for too long
          Exit;
        end;
      end
    else
      Exit;
  {$IFDEF WINDOWS}
    while PeekMessage(Msg,0,0,0,PM_REMOVE) do
      begin
      if (Msg.message=WM_QUIT) then
        Exit
      else
        begin
        TranslateMessage( Msg );
        DispatchMessage( Msg );
        end;
      end;
  {$ENDIF}
    rtcSyncCheck;
    end;
  Sleep(10);
  Result:=CSOpen.WaitFor(0)=wr_Signaled;
  end;

function WaitForSynClose(_timeout:cardinal):boolean;
  var
  {$IFDEF WINDOWS}
    Msg:TMsg;
  {$ENDIF}
    MyTime:cardinal;
  begin
  Result:=False;
  MyTime:=_Timeout*100;
  while (SyOpen.WaitFor(0)<>wr_Signaled) do
    begin
  {$IFDEF WINDOWS}
    while PeekMessage(Msg,0,0,0,PM_REMOVE) do
      begin
      if (Msg.message=WM_QUIT) then
        Exit
      else
        begin
        TranslateMessage( Msg );
        DispatchMessage( Msg );
        end;
      end;
  {$ENDIF}
    rtcSyncCheck;
    if MyTime>0 then
      begin
      Dec(MyTime);
      Sleep(10);
      end
    else
      Exit;
    end;
  Sleep(10);
  Result:=SyOpen.WaitFor(0)=wr_Signaled;
  end;

procedure MySyncNone(Proc: TRtcSyncEvent);
  begin
  end;

procedure MySyncCheckNone(var done: boolean);
  begin
  done:=False;
  end;

procedure ReleaseVirtualThreads;
  var
    i:RtcIntPtr;
    o:TObject;
    Thr:TRtcThread absolute o;
    {$IFDEF RTC_DEBUG}rel:integer;{$ENDIF}
  begin
  {$IFDEF RTC_DEBUG}Log('Checking Virtual Threads ...','DEBUG');{$ENDIF}
  CSThread.Acquire;
  try
    if ThrList.Count>0 then
      begin
      rtcSyncCheck;
      {$IFDEF RTC_DEBUG}rel:=0;{$ENDIF}
      {$IFDEF RTC_DEBUG}Log('Virtual Threads left: '+IntToStr(ThrList.Count),'DEBUG');{$ENDIF}
      i:=ThrList.search_min(o);
      while (i>0) and assigned(o) do
        begin
        rtcSyncCheck;
        if Thr.Finalize then
          begin
          CSThread.Release;
          try
            try
              Thr.Free;
            except
              on E:Exception do
                if LOG_EXCEPTIONS then
                  Log('Release Virtual Thread',E,'ERROR');
              end;
            {$IFDEF RTC_DEBUG}Inc(rel);{$ENDIF}
          finally
            CSThread.Acquire;
            end;
          end;
        i:=ThrList.search_g(i,o);
        end;
      {$IFDEF RTC_DEBUG}Log('Virtual Threads released: '+IntToStr(rel)+' ('+IntToStr(ThrList.Count)+' left)','DEBUG');{$ENDIF}
      end
    {$IFDEF RTC_DEBUG} else
      Log('All Virtual Threads released.','DEBUG'){$ENDIF};
  finally
    CSThread.Release;
    end;
  end;

procedure CloseThreadPool;
  var
    wrk:RtcIntPtr;
    i:TObject;
    Work:TRtcWorkerThread absolute i;
    clr:RtcIntPtr absolute i;
    havetowait:boolean;
    haveto_removecallbacks:boolean;
  begin
  if not Threads_Running then Exit;
  CSThread.Acquire;
  try
    if not Threads_Running then Exit;
    Threads_Running:=False;
  finally
    CSThread.Release;
    end;

  {$IFDEF RTC_DEBUG}Log('CloseThreadPool ('+IntToStr(RtcTotalJobsQueued)+' Queued / '+IntToStr(RtcTotalThreadsBusy)+' Busy / '+IntToStr(RtcTotalThreadsIdle)+' Idle) begin ...','DEBUG');{$ENDIF}

  if SyThr_Running then
    begin
    SyThr_Running:=False;
    SyEV.SetEvent;

    {$IFDEF RTC_DEBUG}
    Log('Waiting for Sync Thread to close ...','DEBUG');
    if WaitForSynClose(RTC_THREAD_POOL_CLOSEWAIT) then
      Log('Done waiting, Sync thread closed.','DEBUG')
    else
      Log('Done waiting, Sync Thread still busy!','DEBUG');
    {$ELSE}
    WaitForSynClose(RTC_THREAD_POOL_CLOSEWAIT);
    {$ENDIF}

    rtcSyncQuit;

    rtcSyncProc:=MySyncNone;
    rtcSyncCheckProc:=MySyncCheckNone;
    end;

  CSThread.Acquire;
  try
    haveto_removecallbacks:=InsideCallback=0;
    havetowait:=ThreadPtrPool.Count>0;
    {$IFDEF RTC_DEBUG}Log('Worker Threads Runnning: '+IntToStr(ThreadPtrPool.Count),'DEBUG');{$ENDIF}
    wrk:=ThreadPtrPool.search_min(i);
    while (wrk>0) and assigned(i) do
      begin
      {$IFDEF RTC_DEBUG}Log('Post ThreadQuit '+IntToStr(Work.ThreadID),'DEBUG');{$ENDIF}
      Work.PostQuit;
      Sleep(10);
      wrk:=ThreadPtrPool.search_g(wrk,i);
      end;
  finally
    CSThread.Release;
    end;

  if havetowait then // Wait for all threads to close
    begin
    {$IFDEF RTC_DEBUG}
    Log('Waiting for all Worker Threads to close ...','DEBUG');
    if WaitForClose(RTC_THREAD_POOL_CLOSEWAIT,RTC_THREAD_POOL_CLOSELAST) then
      begin
      havetowait:=False;
      Log('Done waiting, all worker threads closed.','DEBUG');
      end
    else
      Log('Done waiting, '+IntToStr(OpenCnt)+' Worker Threads ('+IntToStr(NormalThreadCnt)+'+'+IntToStr(ForcedThreadCnt)+') still busy!','DEBUG');
    {$ELSE}
    if WaitForClose(RTC_THREAD_POOL_CLOSEWAIT,RTC_THREAD_POOL_CLOSELAST) then
      havetowait:=False;
    {$ENDIF}
    end;

  ReleaseVirtualThreads;

  if havetowait then // Wait for all threads to close
    begin
    {$IFDEF RTC_DEBUG}
    Log('Waiting for all Worker Threads to close (2nd run) ...','DEBUG');
    if WaitForClose(RTC_THREAD_POOL_CLOSEWAIT,RTC_THREAD_POOL_CLOSELAST) then
      Log('Done waiting, all worker threads closed.','DEBUG')
    else
      Log('Done waiting, '+IntToStr(OpenCnt)+' Worker Threads ('+IntToStr(NormalThreadCnt)+'+'+IntToStr(ForcedThreadCnt)+') still busy!','DEBUG');
    {$ELSE}
    WaitForClose(RTC_THREAD_POOL_CLOSEWAIT,RTC_THREAD_POOL_CLOSELAST);
    {$ENDIF}
    end;

  if haveto_removecallbacks then
    RemoveThreadCallbacks;

  {$IFDEF RTC_DEBUG}Log('CloseThreadPool ('+IntToStr(RtcTotalJobsQueued)+' Queued / '+IntToStr(RtcTotalThreadsBusy)+' Busy / '+IntToStr(RtcTotalThreadsIdle)+' Idle) end.','DEBUG');{$ENDIF}
  end;

{ TRtcThreadEventJob }

type
  TRtcThreadEventJob=class(TRtcJob)
  private
    FSync:boolean;
    FEvent:TRtcSyncEvent;
  public
    procedure Execute;

    function Run(Thr:TRtcThread):boolean; override;
    end;

function TRtcThreadEventJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    if Threads_Running then
      if FSync then
        TRtcThread.Sync(Execute)
      else
        Execute;
  finally
    Result:=True;
    end;
  end;

procedure TRtcThreadEventJob.Execute;
  begin
  try
    if assigned(FEvent) then
      FEvent;
  except
    // ignore all exceptions here
    end;
  FEvent:=nil;
  end;

{ TRtcMultiThreadEventJob }

type
  TRtcMultiThreadEventJob=class(TRtcThreadEventJob)
  private
    FSync:boolean;
    FEvent:TRtcSyncEvent;
  public
    function SingleUse:boolean; override;
    function Run(Thr:TRtcThread):boolean; override;
    end;

function TRtcMultiThreadEventJob.Run(Thr: TRtcThread): boolean;
  begin
  if Threads_Running then
    try
      if FSync then
        TRtcThread.Sync(FEvent)
      else
        FEvent;
    except
      // ignore all exceptions
      end;
  Result:=False;
  end;

function TRtcMultiThreadEventJob.SingleUse: boolean;
  begin
  Result:=False;
  end;

{ TRtcThread }

constructor TRtcThread.Create;
  begin
  inherited;

  Working:=False;
  Active:=True;
  Killed:=False;

  if not assigned(CSThread) then
    raise Exception.Create('Thread Pool already closed.');

  MsgList:=TXObjList.Create(16);
  FNeedThread:=False;

  CSThread.Acquire;
  try
    ThrList.insert(RtcIntPtr(self),self);
  finally
    CSThread.Release;
    end;
  end;

procedure TRtcThread.InternalKill;
  begin
  Killed:=True;
  end;

function TRtcThread.Finalize:boolean;
  begin
  Result:=False;
  if self=nil then Exit;
  if ThrList.search(RtcIntPtr(self))<>self then Exit;

  ThrList.remove(RtcIntPtr(self));
  Dec(Jobs_In_Queue,MsgList.Count);

  if Waiting then
    begin
    WaitList.removeThis(self);
    Waiting:=False;
    end;

  Active:=False;

  while MsgList.Count>0 do
    begin
    MsgList.extractFirst(Job);
    if Job<>Message_Quit then
      try
        KillJob;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcThread.Destroy (MsgList.KillJob)',E,'ERROR');
        end;
    end;
  Job:=nil;

  RtcFreeAndNil(MsgList);

  Result:=True;
  end;

destructor TRtcThread.Destroy;
  begin
  if Active then
    raise EInvalidOperation.Create('TRtcThread.Destroy called when Active=TRUE! Use TRtcThread.Stop() instead.');
  inherited;
  end;

class procedure TRtcThread.Stop(me:TObject);
  begin
  if Threads_Running and assigned(me) then
    TRtcThread.PostJob(me,Message_Quit,True);
  end;

class function TRtcThread.Lock(me: TObject): boolean;
  begin
  Result:=False;
  if Threads_Running and assigned(CSThread) and assigned(me) then
    begin
    CSThread.Acquire;
    try
      if ThrList.search(RtcIntPtr(me))=me then
        Result:=True;
    finally
      if not Result then
        CSThread.Release;
      end;
    end;
  end;

class function TRtcThread.Exists(me: TObject): boolean;
  begin
  Result:=False;
  if Threads_Running and assigned(CSThread) and assigned(me) then
    begin
    CSThread.Acquire;
    try
      if ThrList.search(RtcIntPtr(me))=me then
        Result:=True;
    finally
      CSThread.Release;
      end;
    end;
  end;

class function TRtcThread.InsideBackThread(me: TObject): boolean;
  begin
  Result:=False;
  if Threads_Running and assigned(me) then
    begin
    CSThread.Acquire;
    try
      if ThrList.search(RtcIntPtr(me))=me then
        if TRtcThread(me).InBackThread then
          Result:=True;
    finally
      CSThread.Release;
      end;
    end;
  end;

class procedure TRtcThread.UnLock;
  begin
  CSThread.Release;
  end;

class function TRtcThread.GetJobForEvent(Event:TRtcSyncEvent; AccessGUI:boolean=False):TRtcJob;
  var
    Job:TRtcMultiThreadEventJob;
  begin
  if assigned(Event) then
    begin
    Job:=TRtcMultiThreadEventJob.Create;
    Job.FSync:=AccessGUI;
    Job.FEvent:=Event;
    Result:=Job;
    end
  else
    Result:=nil;
  end;

class function TRtcThread.PostEvent(me:TObject; Event:TRtcSyncEvent; AccessGUI:boolean=False; HighPriority:boolean=False; ForceThread:boolean=False):boolean;
  var
    Job:TRtcThreadEventJob;
  begin
  Result:=False;
  if not (Threads_Running and assigned(Event) and assigned(me)) then Exit;
  Job:=TRtcThreadEventJob.Create;
  try
    Job.FSync:=AccessGUI;
    Job.FEvent:=Event;
    Result:=TRtcThread.PostJob(me,Job,HighPriority,ForceThread);
  finally
    if not Result then
      if Job.SingleUse then
        RtcFreeAndNil(Job);
    end;
  end;

class function TRtcThread.PostJob(me:TObject; var myJob; HighPriority:boolean=False; ForceThread:boolean=False):boolean;
  var
    _Job:TObject absolute myJob;
    MyThr:TRtcWorkerThread;
    meThr:TRtcThread absolute me;
  begin
  if _Job=nil then
    begin
    Result:=True;
    Exit;
    end;
  Result:=False;
  if not (Threads_Running and assigned(CSThread) and assigned(me)) then Exit;

  CSThread.Acquire;
  try
    if ThrList.search(RtcIntPtr(me))=me then
      if not meThr.Active then
        Exit
      else with meThr do
        begin
        if Working then
          begin
          if HighPriority then
            begin
            Inc(Jobs_In_Queue);
            MsgList.addFirst(_Job);
            if _Job is TRtcJob then
              if TRtcJob(_Job).SingleUse then
                _Job:=nil;
            Inc(Pending);
            Result:=True;
            end
          else if Threads_Running then
            begin
            Inc(Jobs_In_Queue);
            MsgList.addLast(_Job);
            if _Job is TRtcJob then
              if TRtcJob(_Job).SingleUse then
                _Job:=nil;
            Result:=True;
            end;
          end
        else if HighPriority then
          begin
          myThr:=GetThread(ForceThread or NeedThread);
          if assigned(myThr) then
            begin
            if Waiting then
              begin
              WaitList.removeThis(meThr);
              Waiting:=False;
              end;
            Job:=_Job;
            if _Job is TRtcJob then
              if TRtcJob(_Job).SingleUse then
                _Job:=nil;
            Working:=True;
            myThr.PostWork(meThr);
            end
          else
            begin
            Inc(Jobs_In_Queue);
            MsgList.addFirst(_Job);
            if _Job is TRtcJob then
              if TRtcJob(_Job).SingleUse then
                _Job:=nil;
            if not Waiting then
              begin
              Waiting:=True;
              WaitList.addFirst(meThr);
              end
            else if WaitList.First<>me then
              begin
              WaitList.removeThis(meThr);
              WaitList.addFirst(meThr);
              end;
            end;
          Result:=True;
          end
        else if not Threads_Running then
          Exit
        else if ForceThread or NeedThread then
          begin
          myThr:=GetThread(True);
          if assigned(myThr) then
            begin
            if Waiting then
              begin
              WaitList.removeThis(meThr);
              Waiting:=False;
              end;
            Job:=_Job;
            if _Job is TRtcJob then
              if TRtcJob(_Job).SingleUse then
                _Job:=nil;
            Working:=True;
            myThr.PostWork(meThr);
            end
          else
            begin
            Inc(Jobs_In_Queue);
            MsgList.addLast(_Job);
            if _Job is TRtcJob then
              if TRtcJob(_Job).SingleUse then
                _Job:=nil;
            if not Waiting then
              begin
              Waiting:=True;
              WaitList.addLast(me);
              end;
            end;
          Result:=True;
          end
        else if Waiting then
          begin
          Inc(Jobs_In_Queue);
          MsgList.addLast(_Job);
          if _Job is TRtcJob then
            if TRtcJob(_Job).SingleUse then
              _Job:=nil;
          Result:=True;
          end
        else
          begin
          myThr:=GetThread(False);
          if assigned(myThr) then
            begin
            Job:=_Job;
            if _Job is TRtcJob then
              if TRtcJob(_Job).SingleUse then
                _Job:=nil;
            Working:=True;
            myThr.PostWork(meThr);
            end
          else
            begin
            Inc(Jobs_In_Queue);
            MsgList.addLast(_Job);
            if _Job is TRtcJob then
              if TRtcJob(_Job).SingleUse then
                _Job:=nil;
            Waiting:=True;
            WaitList.addLast(me);
            end;
          Result:=True;
          end;
        end;
  finally
    CSThread.Release;
    end;
  end;

procedure TRtcThread.Idle;
  begin
  Working:=False;
  if MsgList.Count>0 then
    begin
    Waiting:=True;
    WaitList.addLast(self); // add to waiting list
    end;
  end;

class function TRtcThread.JobsInQueue(me:TObject):integer;
  begin
  Result:=-1;
  if not (Threads_Running and assigned(CSThread) and assigned(me)) then Exit;

  CSThread.Acquire;
  try
    if ThrList.search(RtcIntPtr(me))=me then
      with TRtcThread(me) do
        Result:=MsgList.Count;
  finally
    CSThread.Release;
    end;
  end;

class function TRtcThread.JobsTotal(me:TObject):integer;
  begin
  Result:=-1;
  if not (Threads_Running and assigned(CSThread) and assigned(me)) then Exit;

  CSThread.Acquire;
  try
    if ThrList.search(RtcIntPtr(me))=me then
      with TRtcThread(me) do
        begin
        Result:=MsgList.Count;
        If Working then Inc(Result);
        end;
  finally
    CSThread.Release;
    end;
  end;

class function TRtcThread.JobRunning(me:TObject):boolean;
  begin
  Result:=False;
  if not (Threads_Running and assigned(CSThread) and assigned(me)) then Exit;

  CSThread.Acquire;
  try
    if ThrList.search(RtcIntPtr(me))=me then
      Result:=TRtcThread(me).Working;
  finally
    CSThread.Release;
    end;
  end;

class function TRtcThread.Sync(Event: TRtcSyncEvent):boolean;
  var
    obj:TObject;
    MyThr:TRtcWorkerThread absolute obj;
  begin
  if not Threads_Running then
    raise ERtcThreadPoolClosed.Create('TRtcThread.Sync: RTC Thread Pool closed or closing.')
  else if InsideMainThread then
    Result:=False
  else if assigned(CSThread) and assigned(ThreadIdPool) then
    begin
    CSThread.Acquire;
    try
      obj:=ThreadIdPool.search(GetMyThreadID);
    finally
      CSThread.Release;
      end;
    if assigned(obj) then
      begin
      MyThr.Sync(Event);
      Result:=True;
      end
    else
      raise ERtcThreadPoolClosed.Create('TRtcThread.Sync can ONLY be used from a RTC Worker Thread or the Main Thread!');
    end
  else
    raise ERtcThreadPoolClosed.Create('TRtcThread.Sync: RTC Thread Pool closed or closing');
  end;

procedure TRtcThread.GetJob;
  begin
  MsgList.extractFirst(Job);
  Dec(Jobs_In_Queue);
  end;

function TRtcThread.RunJob:boolean;
  begin
  Result:=False;
  if Job is TRtcJob then
    begin
    if TRtcJob(Job).Run(self) then
      if TRtcJob(Job).SingleUse then
        RtcFreeAndNil(Job);
    end
  else
    raise Exception.Create('Error!! TRtcThread -> Unknown Job class: '+Job.ClassName);
  end;

procedure TRtcThread.KillJob;
  begin
  if Job<>nil then
    try
      if Job is TRtcJob then
        begin
        if TRtcJob(Job).SingleUse then
          RtcFreeAndNil(Job)
        else
          Job:=nil;
        end
      else
        Job:=nil;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          try
            Log('TRtcThread.KillJob ('+RtcString(Job.ClassName)+')',E,'ERROR');
          except
            Log('TRtcThread.KillJob (Unknown_class)',E,'ERROR');
            end;
      end;
  end;

function TRtcThread.InsideThread: boolean;
  begin
  if assigned(FThr) then
    Result:=TRtcWorkerThread.InsideThread(FThr)
  else
    Result:=False;
  end;

function TRtcThread.InBackThread: boolean;
  begin
  if assigned(FThr) then
    Result:=TRtcWorkerThread.InBackThread(FThr)
  else
    Result:=False;
  end;

{ TRtcWorkerThread }

constructor TRtcWorkerThread.Create(CreateSuspended, Force: boolean);
  begin
  FFinished:=False;
  FreeOnTerminate:=True;
  Run:=TRtcEvent.Create(False,False);
  FForced:=Force;

  inherited Create(CreateSuspended);
{$IFDEF WINDOWS}
  Priority:=RTC_THREAD_PRIORITY;
{$ENDIF}

  CSThread.Acquire;
  try
    Inc(OpenCnt);
    if Force then Inc(ForcedThreadCnt)
    else Inc(NormalThreadCnt);
    {$IFDEF RTC_DEBUG}Log('Thread Created: '+IntToStr(ThreadID)+' ('+IntToStr(NormalThreadCnt)+'+'+IntToStr(ForcedThreadCnt)+'='+IntToStr(OpenCnt)+' total)','DEBUG');{$ENDIF}
    if OpenCnt=1 then
      CSOpen.ResetEvent;
    ThreadPtrPool.insert(RtcIntPtr(self),self);
  finally
    CSThread.Release;
    end;
  end;

destructor TRtcWorkerThread.Destroy;
  begin
  try
    CSThread.Acquire;
    try
      ThreadPtrPool.remove(RtcIntPtr(self));
      if Forced then Dec(ForcedThreadCnt)
      else Dec(NormalThreadCnt);
      RtcFreeAndNil(Run);
      Dec(OpenCnt);
      {$IFDEF RTC_DEBUG}Log('Thread Destroyed: '+IntToStr(ThreadID)+' ('+IntToStr(NormalThreadCnt)+'+'+IntToStr(ForcedThreadCnt)+'='+IntToStr(OpenCnt)+' left)','DEBUG');{$ENDIF}
      if OpenCnt=0 then
        begin
        {$IFDEF RTC_DEBUG}Log('Last Thread Destroyed.','DEBUG');{$ENDIF}
        CSOpen.SetEvent;
        end;
    finally
      CSThread.Release;
      end;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcWorkerThread.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcWorkerThread.Execute;
  var
    ToFree:boolean;
    w:TRtcThread;
  function WorkPending:boolean;
    begin
    if Work.Pending>0 then
      begin
      Dec(Work.Pending);
      Work.GetJob;
      Result:=assigned(Work.Job);
      end
    else
      Result:=False;
    end;
  begin
  CSThread.Acquire;
  try
    if not ReturnThread(self) then Exit;
    ThreadIdPool.insert(GetMyThreadID,self);
  finally
    CSThread.Release;
    end;

  try
    DoAfterThreadStart;
  except
    // ignore all exceptions here
    end;
  try
    w:=nil;
    ToFree:=False;
    {$IFDEF RTC_DEBUG}Log('Thread Started: '+IntToStr(ThreadID),'DEBUG');{$ENDIF}
    try
      while Run.WaitFor(WAIT_INFINITE)=wr_Signaled do
        begin
        if assigned(Work) then
          begin
          try
            if Work.Job=Message_Quit then
              ToFree:=True
            else if Work.RunJob then
              ToFree:=True
            else
              ToFree:=Work.Killed;
          except
            on E:Exception do
              begin
              ToFree:=True;
              if LOG_THREAD_EXCEPTIONS then
                Log('TRtcWorkerThread Work.RunJob',E,'THREAD');
              // ignore exceptions (do not want to kill this thread)
              end;
            end;
          {$IFNDEF RTC_NOSLEEP}Sleep(RTC_THREAD_SLEEP);{$ENDIF} // give up some of your CPU time
          CSThread.Acquire;
          try
            if ToFree then
              begin
              try
                Work.FThr:=nil;
                Work.Job:=nil;
                Work.Active:=False;
                if Work.Finalize then // if Work.RunJob returned TRUE, the thread object is asking to be released
                  w:=Work;
              except
                on E:Exception do
                  if LOG_THREAD_EXCEPTIONS then
                    Log('TRtcWorkerThread Work.Finalize',E,'THREAD');
                end;
              Work:=nil;
              if not ReturnThread(self) then
                FFinished:=True;
              end
            else if WorkPending then
              Run.SetEvent
            else
              begin
              try
                Work.FThr:=nil;
                Work.Job:=nil;
                Work.Idle;
              except
                on E:Exception do
                  if LOG_THREAD_EXCEPTIONS then
                    Log('TRtcWorkerThread Work.Idle',E,'THREAD');
                end;
              Work:=nil;
              if not ReturnThread(self) then
                FFinished:=True;
              end;
          finally
            CSThread.Release;
            end;
          if assigned(w) then
            try
              RtcFreeAndNil(w);
            except
              on E:Exception do
                if LOG_THREAD_EXCEPTIONS then
                  Log('TRtcWorkerThread Work.Free',E,'THREAD');
              end;
          if FFinished then Break;
          end
        else
          begin
          FFinished:=True;
          Break;
          end;
        end;
    except
      on E:Exception do
        if LOG_THREAD_EXCEPTIONS then
          Log('TRtcWorkThread.Execute',E,'THREAD');
      end;
  finally
    if assigned(CSThread) then
      begin
      CSThread.Acquire;
      try
        if assigned(ThreadIdPool) then
          ThreadIdPool.remove(GetMyThreadID);
      finally
        CSThread.Release;
        end;
      end;
    end;
  {$IFDEF RTC_DEBUG}Log('Thread Stopped: '+IntToStr(ThreadID),'DEBUG');{$ENDIF}
  try
    DoBeforeThreadStop;
  except
    // ignore all exceptions here
    end;
  end;

class function TRtcWorkerThread.InsideThread(me:TObject):boolean;
  var
    Thr:TRtcWorkerThread absolute me;
  begin
  CSThread.Acquire;
  try
    if me=nil then
      Result:=False
    else if ThreadPtrPool.search(RtcIntPtr(me))<>me then
      Result:=False
    else if Thr.FInsideMain then
      Result:=InsideMainThread
    else
      Result:=GetMyThreadID=RtcThrID(Thr.ThreadID);
  finally
    CSThread.Release;
    end;
  end;

class function TRtcWorkerThread.InBackThread(me:TObject):boolean;
  var
    Thr:TRtcWorkerThread absolute me;
  begin
  CSThread.Acquire;
  try
    if me=nil then
      Result:=False
    else if ThreadPtrPool.search(RtcIntPtr(me))<>me then
      Result:=False
    else
      Result:=GetMyThreadID=RtcThrID(Thr.ThreadID);
  finally
    CSThread.Release;
    end;
  end;

procedure TRtcWorkerThread.MySyncEvent;
  begin
  FInsideMain:=True;
  try
    FEvent;
  finally
    FInsideMain:=False;
    end;
  end;

procedure TRtcWorkerThread.PostWork(Thr: TRtcThread);
  begin
  Work:=Thr;
  Work.FThr:=self;
  Run.SetEvent;
  end;

procedure TRtcWorkerThread.PostQuit;
  begin
  FFinished:=True;
  Run.SetEvent;
  end;

procedure TRtcWorkerThread.Sync(Event: TRtcSyncEvent);
  begin
  FEvent:=Event;
  if assigned(rtcSyncProc) then
    rtcSyncProc(MySyncEvent)
  else
    Synchronize(MySyncEvent);
  end;

{ TRtcJob }

function TRtcJob.SingleUse:boolean;
  begin
  Result:=True;
  end;

{ SyncCheckProc }

procedure rtcSyncExecute;
  var
    tim:int64;
    SyObj:TProcEv;
    xob:TObject absolute SyObj;
  begin
  xob:=nil;
  tim:=GetTickTime64;

  SyCS.Acquire;
  try
    SyList.extractFirst(xob);
    if xob=nil then
      begin
      SyEV.ResetEvent;
      SyReady:=False;
      end;
  finally
    SyCS.Release;
    end;

  while assigned(xob) do
    begin
    try
      SyObj.P;
    except
      { Acquire Exception Object here. We need to raise the
        Exception from the background Thread calling "Sync" }
      SyObj.E.X:=TObject(AcquireExceptionObject);
      end;
    SyObj.E.SetEvent;
    SyObj:=nil;

    if GetTickTime64-tim>RTC_THREAD_SYNCLIMIT then
      Break; // working too long

    SyCS.Acquire;
    try
      SyList.extractFirst(xob);
      if xob=nil then
        begin
        SyEV.ResetEvent;
        SyReady:=False;
        end;
    finally
      SyCS.Release;
      end;
    end;
  end;

procedure MySyncProc(Proc: TRtcSyncEvent);
  var
    EV:TRtcEventEx;
    X:TObject;
    SyObj:TProcEv;
  begin
  if not Threads_Running then Exit;

  EV:=TRtcEventEx.Create(True,False);
  EV.X:=nil;
  SyObj:=TProcEv.Create;
  with SyObj do
    begin
    P:=Proc;
    E:=EV;
    end;
  try
    SyCS.Acquire;
    try
      if not SyThr_Running then
        if Threads_Running then
          begin
          SyThr_Running:=True;
          TSyThread.Create(False);
          end;

      SyList.addLast(SyObj);
      SyReady:=True;
      SyEV.SetEvent;
    finally
      SyCS.Release;
      end;
    EV.WaitFor(WAIT_INFINITE);
    X:=EV.X;
  finally
    EV.X:=nil;
    SyObj.E:=nil;
    SyObj.P:=nil;
    EV.Free;
    SyObj.Free;
    end;

  if Assigned(X) then
    raise X;
  end;

procedure MySyncCheck(var done:boolean);
  begin
  done:=SyReady;
  if done then
    rtcSyncExecute;
  end;

{ TSyThread }

constructor TSyThread.Create(CreateSuspended: boolean);
  begin
  FreeOnTerminate:=True;
  SyOpen.ResetEvent;
  inherited Create(CreateSuspended);
  end;

destructor TSyThread.Destroy;
  begin
  SyOpen.SetEvent;
  end;

procedure TSyThread.SyncExecute;
  begin
  rtcSyncExecute;
  end;

procedure TSyThread.Execute;
  function ThrRun:boolean;
    begin
    Result:=SyThr_Running;
    end;
  function NowReady:boolean;
    begin
    Result:=SyReady;
    end;
  begin
  FreeOnTerminate:=True;
  while ThrRun do
    if SyEV.WaitFor(WAIT_INFINITE)=wr_Signaled then
      if NowReady and ThrRun then
        Synchronize(SyncExecute);
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcThrPool Initializing ...','DEBUG');{$ENDIF}

Jobs_In_Queue:=0;
MainThrID:=GetMyThreadID;

SyThr_Running:=False;
Threads_Running:=True;
ThreadCallbackCount:=0;
SetLength(ThreadCallbacks,0);
InsideCallback:=0;
CSThread:=TRtcCritSec.Create;
OpenCnt:=0;
NormalThreadCnt:=0;
ForcedThreadCnt:=0;
CSOpen:=TRtcEvent.Create(True,True);

ThreadPtrPool:=tObjList.Create(128);
ThreadIdPool:=tObjList.Create(128);
FreePool:=TXObjList.Create(128);
ForcePool:=TXObjList.Create(128);

Message_Quit:=TRtcBaseMessage.Create;
ThrList:=tObjList.Create(128);
WaitList:=tXObjList.Create(128);

SyReady:=False;
SyList:=tXObjList.Create(128);
SyCS:=TRtcCritSec.Create;
SyEV:=TRtcEvent.Create(True,False);
SyOpen:=TRtcEvent.Create(True,False);
rtcSyncProc:=MySyncProc;
rtcSyncCheckProc:=MySyncCheck;

{$IFDEF RTC_DEBUG} Log('rtcThrPool Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcThrPool Finalizing ...','DEBUG');{$ENDIF}

CloseThreadPool;

rtcSyncProc:=MySyncNone;
rtcSyncCheckProc:=MySyncCheckNone;

{$IFDEF RTC_DEBUG}Log('Releasing Thread Pool','DEBUG');{$ENDIF}
RtcFreeAndNil(CSOpen);
RtcFreeAndNil(CSThread);
RtcFreeAndNil(ThreadPtrPool);
RtcFreeAndNil(ThreadIdPool);
RtcFreeAndNil(FreePool);
RtcFreeAndNil(ForcePool);
{$IFDEF RTC_DEBUG}Log('Thread Pool released.','DEBUG');{$ENDIF}

{$IFDEF RTC_DEBUG}Log('Releasing Thread List','DEBUG');{$ENDIF}
RtcFreeAndNil(ThrList);
RtcFreeAndNil(WaitList);
RtcFreeAndNil(Message_Quit);
{$IFDEF RTC_DEBUG}Log('Thread List released.','DEBUG');{$ENDIF}

{$IFDEF RTC_DEBUG}Log('Releasing Sync List','DEBUG');{$ENDIF}
RtcFreeAndNil(SyCS);
RtcFreeAndNil(SyEV);
RtcFreeAndNil(SyOpen);
RtcFreeAndNil(SyList);
{$IFDEF RTC_DEBUG}Log('Sync List released.','DEBUG');{$ENDIF}

{$IFDEF RTC_DEBUG} Log('rtcThrPool Finalized.','DEBUG');{$ENDIF}
end.
