{
  @html(<b>)
  Timer
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  Thread-safe cross-platform timer class implemented using 2 background threads
  (one for background thread events, one for main thread events).
  @html(<br>)

  This class is used internally by TRtcConnection and all its descendant classes
  to implement the timeout, reconnect and restart functionality.
}
unit rtcTimer;

{$INCLUDE rtcDefs.inc}

interface

uses
{$IFDEF WINDOWS}
  Windows,
  Messages,
{$ENDIF}

  SysUtils,
  Classes,

  rtcTypes,
  rtcSystem,
  rtcSrcList,
  rtcThrPool,
  rtcLog;

var
  LOG_TIMER_EXCEPTIONS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

  // Max "Sleep Time" (milliseconds) while waiting for the next timer event
  RTC_TIMER_MAXSLEEP:Cardinal=$FFFF; // 65.5 seconds

type
  // @Abstract(Events used by RTC Timer)
  TRtcTimerEvent = TRtcSyncEvent;

  { @Abstract(RTC Timer class)

    This class ensures a Thread-Safe Timer by
    using the RTC Window Handle Pool and RTC Thread Pool
    instead of the TTimer class implementation. }
  TRtcTimer = class(TObject)
  private
    FAutoDisable:boolean;
    FAutoDestroy:boolean;

    FEvent:TRtcTimerEvent;

    FMyThr,FThr:TRtcThread;
    FJob:TObject;

    FMyThrOwner:boolean;
    FBackThread:boolean;

    FActive:boolean;
    FInterval:Cardinal;
    FLastTrigger:int64;

  protected
    { For internal use only!!!
      Called by the framework to call the Event for this timer.
      If the timer is still active after executing the event, returns interval until next trigger.
      If the timer was disabled and/or destroyed, returns 0 (zero).
      @exclude }
    class procedure Timer(me:TObject);

    function GetMyThr:TRtcThread;

    property LastTrigger:int64 read FLastTrigger write FLastTrigger;

  public
    // Create a Timer. To start the timer, use the @Link(Enable) method.
    constructor Create(Multi_Threaded:boolean; Back_Thread:TRtcThread=nil); virtual;

    { @exclude }
    destructor Destroy; override;

    { Allways use Stop instead of Free or Destroy! }
    class procedure Stop(var me);

    { Disable the Timer }
    class procedure Disable(me:TObject);

    { Enable the Timer to trigger 'Event' every 'Wait' miliseconds.
      If AutoDisable is True, Timer will be automatically disabled after the event has been triggered.
      If AutoDestroy is True, Timer object will be automaticaly destroyed after the event has been triggered. }
    class procedure Enable(me:TObject; Wait:Cardinal; Event:TRtcTimerEvent; AutoDisable:boolean=False; AutoDestroy:boolean=False); overload;

    { Enable the Timer to post the 'Job' to Thread 'Thr' every 'Wait' miliseconds.
      If AutoDisable is True, Timer will be automatically disabled after the event has been triggered.
      If AutoDestroy is True, Timer object will be automaticaly destroyed after the event has been triggered.
      If called with Thr=nil, internal RtcThread created only for use by this TRtcTimer will be used. }
    class procedure Enable(me:TObject; Wait:Cardinal; Job:TObject; Thr:TRtcThread=nil; AutoDisable:boolean=False; AutoDestroy:boolean=False); overload;

    { Reset elapsed time counter.
      This will make the Timer skip one upcoming event. }
    class procedure Reset(me:TObject);

    { Trigger Timer 'Event'/'Job' and Reset the elapsed time counter.
      This method executes the next Timer 'Event'/'Job' immediately (if the Timer is active)
      instead of waiting for the next "timed tick",  then resets the elapsed time counter 
      (if the Timer was created with AutoDestroy=FALSE and AutoDisable=FALSE). }
    class procedure Tick(me:TObject);

    { Access to the internal RTC Thread object, used if this class was created with Multi_Threaded=True }
    property Thread:TRtcThread read GetMyThr;
    end;

procedure CloseTimerPool;

implementation

var
  TimerThreads:integer;
  Timers_Running:boolean;

type
  TRtcTimerThread=class(TThread)
  private
    FSyncMode: boolean;
    FFinished: boolean;

    procedure SetFinished(const Value: boolean);

  protected
    FTriggers:tObjList64;
    FTimers:tBinList;
    FEvent:TRtcEvent;
    FCS:TRtcCritSec;

    Trig:tBinList;
    MyTime:int64;
    ENext:int64;

    procedure RemoveAllTriggers;
    procedure ExecuteTimer;

  public
    constructor Create(CreateSuspended:boolean);
    destructor Destroy; override;

    procedure Execute; override;

    procedure AddTrigger(Tim:TRtcTimer; TriggerTime:int64);
    procedure RemoveTrigger(Tim:TRtcTimer);
    procedure ChangeTrigger(Tim:TRtcTimer; TriggerTime:int64);

    property Finished:boolean read FFinished write SetFinished;
    property SyncMode:boolean read FSyncMode write FSyncMode;
    end;

var
  MainTimer:TRtcTimerThread=nil;
  BackTimer:TRtcTimerThread=nil;

var
  TimList:tBinList;
  TimCS:TRtcCritSec;
  _Outside:TRtcEvent;

procedure WaitForClose(_timeout:Cardinal);
{$IFDEF WINDOWS}
  var
    Msg:TMsg;
    MyTime:Cardinal;
  begin
  MyTime:=_Timeout*100;
  while _Outside.WaitFor(0)<>wr_Signaled do
    begin
    if MyTime>0 then
      Dec(MyTime)
    else
      Exit;
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
    Sleep(10);
    end;
  Sleep(10);
  end;
{$ELSE}
  begin
  _Outside.WaitFor(_Timeout*1000);
  Sleep(10);
  end;
{$ENDIF}

procedure CloseTimerPool;
  var
    Closing:integer;
  begin
  if not Timers_Running then Exit;
  TimCS.Acquire;
  try
    if not Timers_Running then Exit;
    Timers_Running:=False;
  finally
    TimCS.Release;
    end;

  CloseThreadPool;

  Closing:=0;
  if assigned(MainTimer) then
    begin
    {$IFDEF RTC_DEBUG} Log('Closing Main Timer Thread ...','DEBUG');{$ENDIF}
    TimCS.Acquire;
    try
      if assigned(MainTimer) then
        begin
        MainTimer.Finished:=True;
        MainTimer:=nil;
        Inc(Closing);
        end;
    finally
      TimCS.Release;
      end;
    end;
  if assigned(BackTimer) then
    begin
    {$IFDEF RTC_DEBUG} Log('Closing Back Timer Thread ...','DEBUG');{$ENDIF}
    TimCS.Acquire;
    try
      if assigned(BackTimer) then
        begin
        BackTimer.Finished:=True;
        BackTimer:=nil;
        Inc(Closing);
        end;
    finally
      TimCS.Release;
      end;
    end;
  if Closing>0 then
    begin
    {$IFDEF RTC_DEBUG} Log('Waiting for '+IntToStr(Closing)+' Timer Thread(s) to finish ...','DEBUG');{$ENDIF}
    WaitForClose(10);
    {$IFDEF RTC_DEBUG}
    if TimerThreads=0 then
      Log('Done Waiting, all Timer Threads closed.', 'DEBUG')
    else
      Log('Done Waiting, '+IntToStr(TimerThreads)+' Timer Thread(s) still busy!','DEBUG');
    {$ENDIF}
    end;
  end;

function rtcStoreTimer(const obj):boolean;
  begin
  Result:=False;
  if not assigned(TimCS) then Exit;

  TimCS.Acquire;
  try
    if Timers_Running then
      begin
      if not assigned(TimList) then
        TimList:=tBinList.Create(128);

      if assigned(TimList) then
        if TimList.search(RtcIntPtr(Obj))=0 then
          begin
          Result:=True;
          TimList.insert(RtcIntPtr(Obj), 1);
          end;
      end;
  finally
    TimCS.Release;
    end;
  end;

function rtcRemoveTimer(const obj):boolean;
  begin
  Result:=False;
  if not assigned(TimCS) then Exit;

  TimCS.Acquire;
  try
    if assigned(TimList) then
      if TimList.search(RtcIntPtr(Obj))>0 then
        begin
        TimList.remove(RtcIntPtr(Obj));
        Result:=True;
        end;
  finally
    if not Result then TimCS.Release;
    end;
  end;

function rtcEnterTimer(ID:RtcIntPtr):boolean;
  begin
  Result:=False;
  if not assigned(TimCS) then Exit;

  TimCS.Acquire;
  try
    if Timers_Running then
      if assigned(TimList) then
        if TimList.search(ID)>0 then
          Result:=True;
  finally
    if not Result then TimCS.Release;
    end;
  end;

procedure rtcLeaveTimer;
  begin
  if not assigned(TimCS) then Exit;
  TimCS.Release;
  end;

{ TRtcTimerThread }

constructor TRtcTimerThread.Create(CreateSuspended: boolean);
  begin
  FFinished:=False;
  FreeOnTerminate:=True;

  FCS:=TRtcCritSec.Create;
  FTriggers:=tObjList64.Create(128);
  FEvent:=TRtcEvent.Create(True,False);

  TimCS.Acquire;
  try
    Inc(TimerThreads);
    if TimerThreads=1 then
      _Outside.ResetEvent;
  finally
    TimCS.Release;
    end;

  inherited Create(CreateSuspended);
  end;

destructor TRtcTimerThread.Destroy;
  begin
  RemoveAllTriggers;

  RtcFreeAndNil(FTriggers);
  RtcFreeAndNil(FEvent);
  RtcFreeAndNil(FCS);

  TimCS.Acquire;
  try
    Dec(TimerThreads);
    if TimerThreads=0 then
      _Outside.SetEvent;
  finally
    TimCS.Release;
    end;

  inherited;
  end;

procedure TRtcTimerThread.ExecuteTimer;
  var
    Tmp,Tmp2:RtcIntPtr;
    Tmp3:TRtcTimer absolute Tmp2;
  begin
  Tmp2:=Trig.search_min(Tmp);
  while (Tmp2>0) and not FFinished do
    begin
    Trig.remove(Tmp2);
    if rtcEnterTimer(Tmp2) then
      TRtcTimer.Timer(Tmp3);
    Tmp2:=Trig.search_min(Tmp);
    end;
  end;

procedure TRtcTimerThread.Execute;
  var
    NextTrigger,
    SleepTime:int64;
    Tmp:TObject;
  begin
  Trig:=nil;
  SleepTime:=0;

  repeat
    MyTime:=GetTickTime64;

    FCS.Acquire;
    try
      NextTrigger:=FTriggers.search_min(Tmp);
      if NextTrigger<=0 then // Nobody waiting
        begin
        FEvent.ResetEvent;
        SleepTime:=RTC_TIMER_MAXSLEEP;
        end
      else if NextTrigger<=MyTime then // Triggers ready for execution
        begin
        Trig:=tBinList(Tmp);
        FTriggers.remove(NextTrigger);
        end
      else
        begin
        FEvent.ResetEvent;
        SleepTime:=NextTrigger-MyTime;
        if SleepTime>RTC_TIMER_MAXSLEEP then
          SleepTime:=RTC_TIMER_MAXSLEEP;
        end;
      Tmp:=nil;
    finally
      FCS.Release;
      end;

    if not FFinished then
      if assigned(Trig) then
        begin
        try
          if SyncMode then
            begin
            if assigned(rtcSyncProc) then
              rtcSyncProc(ExecuteTimer)
            else
              Synchronize(ExecuteTimer);
            end
          else
            ExecuteTimer;
        finally
          RtcFreeAndNil(Trig);
          end;
        end
      else
        FEvent.WaitFor(SleepTime);
    until FFinished;

  if assigned(Trig) then
    RtcFreeAndNil(Trig);
  end;

procedure TRtcTimerThread.SetFinished(const Value: boolean);
  begin
  FFinished := Value;
  if Value then
    begin
    RemoveAllTriggers;
    FEvent.SetEvent;
    end;
  end;

procedure TRtcTimerThread.RemoveAllTriggers;
  var
    Tmp2:int64;
    tl:TObject;
    bl:tBinList absolute tl;
  begin
  FCS.Acquire;
  try
    repeat
      Tmp2:=FTriggers.search_min(tl);
      if (Tmp2>0) and assigned(tl) then
        begin
        FTriggers.remove(Tmp2);
        RtcFreeAndNil(bl);
        end
      else
        Break;
      until False;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcTimerThread.AddTrigger(Tim: TRtcTimer; TriggerTime: int64);
  var
    tl:TObject;
    bl:tBinList absolute tl;
  begin
  Inc(TriggerTime,GetTickTime64);

  FCS.Acquire;
  try
    if Tim.FActive then
      begin
      Tim.LastTrigger:=TriggerTime;
      tl:=FTriggers.search(TriggerTime);
      if assigned(tl) then
        begin
        bl.insert(RtcIntPtr(Tim),1);
        tl:=nil;
        end
      else
        begin
        bl:=tBinList.Create(8);
        bl.insert(RtcIntPtr(Tim),1);
        FTriggers.insert(TriggerTime,tl);
        if FTriggers.search_min(tl)=TriggerTime then
          begin
          tl:=nil;
          FEvent.SetEvent;
          end;
        end;
      end;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcTimerThread.RemoveTrigger(Tim: TRtcTimer);
  var
    TriggerTime:int64;
    tl:TObject;
    bl:tBinList absolute tl;
  begin
  FCS.Acquire;
  try
    TriggerTime:=Tim.LastTrigger;
    if TriggerTime>0 then
      begin
      Tim.LastTrigger:=0;
      tl:=FTriggers.search(TriggerTime);
      if assigned(tl) then
        if bl.search(RtcIntPtr(Tim))>0 then
          begin
          bl.remove(RtcIntPtr(Tim));
          if bl.Count=0 then
            begin
            FTriggers.remove(TriggerTime);
            RtcFreeAndNil(bl);
            end;
          end;
      end;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcTimerThread.ChangeTrigger(Tim: TRtcTimer; TriggerTime: int64);
  var
    OldTriggerTime:int64;
    tl:TObject;
    bl:tBinList absolute tl;
  begin
  Inc(TriggerTime,GetTickTime64);
  FCS.Acquire;
  try
    if Tim.FActive then
      begin
      OldTriggerTime:=Tim.LastTrigger;
      if (OldTriggerTime>0) and (OldTriggerTime<>TriggerTime) then
        begin
        Tim.LastTrigger:=TriggerTime;
        tl:=FTriggers.search(OldTriggerTime);
        if Assigned(tl) then
          if bl.search(RtcIntPtr(Tim))>0 then
            begin
            bl.remove(RtcIntPtr(Tim));
            if bl.count=0 then
              begin
              FTriggers.remove(OldTriggerTime);
              RtcFreeAndNil(bl);
              end
            else
              bl:=nil;
            tl:=FTriggers.search(TriggerTime);
            if Assigned(tl) then
              bl.insert(RtcIntPtr(Tim),1)
            else
              begin
              bl:=tBinList.Create(8);
              bl.insert(RtcIntPtr(Tim),1);
              FTriggers.insert(TriggerTime,bl);
              bl:=nil;
              if FTriggers.search_min(tl)=TriggerTime then
                begin
                tl:=nil;
                FEvent.SetEvent;
                end;
              end;
            end;
        end;
      end;
  finally
    FCS.Release;
    end;
  end;

{ TRtcTimer }

constructor TRtcTimer.Create(Multi_Threaded:boolean; Back_Thread:TRtcThread=nil);
  begin
  if not assigned(TimCS) then
    raise Exception.Create('Timer Pool already closed.');

  inherited Create;

  FBackThread:=Multi_Threaded;

  TimCS.Acquire;
  try
    if FBackThread then
      begin
      if Back_Thread=nil then
        begin
        FMyThr:=nil;
        FMyThrOwner:=True;
        end
      else
        begin
        FMyThr:=Back_Thread;
        FMyThrOwner:=False;
        end;
      if Timers_Running and not assigned(BackTimer) then
        BackTimer:=TRtcTimerThread.Create(False);
      end
    else
      begin
      FMyThr:=Back_Thread;
      FMyThrOwner:=False;
      if Timers_Running and not assigned(MainTimer) then
        begin
        MainTimer:=TRtcTimerThread.Create(False);
        MainTimer.SyncMode:=True;
        end;
      end;
  finally
    TimCS.Release;
    end;

  FInterval := 0;
  FLastTrigger := 0;

  if not rtcStoreTimer(self) then
    raise Exception.Create('Timer Pool already closed.');
  end;

destructor TRtcTimer.Destroy;
  var
    TH:TRtcThread;
  begin
  TH:=nil;
  try
    { "rtcRemoveTimer" should be called from "TRtcTimer.Stop",
       but it is ALSO called here, just in case "Stop" method was NOT used.
       If "Stop" was already used on this instance, "rtcRemoveTimer" does nothing.
       If "Stop" was NOT used, "rtcRemoveTimer" removes this
       Timer from the list of created timers and acquires a lock.
       In either case, "rtcLeaveTimer" has to be called (see below). }
    rtcRemoveTimer(self); // <- this is just for "safety"
    try
      if FActive then
        begin
        FActive:=False;
        if Timers_Running then
          if FBackThread then
            BackTimer.RemoveTrigger(self)
          else
            MainTimer.RemoveTrigger(self);
        end;
      if FMyThrOwner then TH:=FMyThr;
      FMyThr:=nil;
    finally
      rtcLeaveTimer; // <- this is required!
      end;
    if assigned(TH) then
      TRtcThread.Stop(TH);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_TIMER_EXCEPTIONS then
        Log('TRtcTimer.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

class procedure TRtcTimer.Stop(var me);
  begin
  if pointer(me)<>nil then
    if rtcRemoveTimer(me) then
      RtcFreeAndNil(me);
  end;

class procedure TRtcTimer.Timer(me:TObject);
  var
    FE:TRtcTimerEvent;
    TH:TRtcThread;
    JO:TObject;
    intimer,autodes:boolean;
    tim:TRtcTimer;
  begin
  {$IFDEF NEXTGEN} FE:=nil; {$ENDIF}
  intimer:=True; autodes:=False;
  try
    tim:=TRtcTimer(me);
    if assigned(tim.FEvent) then
      begin
      FE:=tim.FEvent;
      if tim.FBackThread then
        begin
        TH:=tim.FMyThr;
        if tim.FAutoDisable then
          tim.FActive:=False
        else
          BackTimer.AddTrigger(tim,tim.FInterval);
        end
      else
        begin
        TH:=nil;
        if tim.FAutoDisable then
          tim.FActive:=False
        else
          MainTimer.AddTrigger(tim,tim.FInterval);
        end;
      if tim.FAutoDestroy then
        begin
        if assigned(TH) then
          begin
          autodes:=tim.FMyThrOwner;
          tim.FMyThr:=nil;
          end;
        RtcFreeAndNil(tim);
        end
      else
        tim:=nil;

      rtcLeaveTimer;
      intimer:=False;

      if assigned(TH) then // Back-Thread Event
        begin
        TRtcThread.PostEvent(TH,FE);
        if autodes then
          TRtcThread.Stop(TH);
        end
      else
        FE;
      end
    else if assigned(tim.FThr) then
      begin
      TH:=tim.FThr;
      JO:=tim.FJob;
      if tim.FAutoDisable then
        tim.FActive:=False
      else if tim.FBackThread then
        BackTimer.AddTrigger(tim,tim.FInterval)
      else
        MainTimer.AddTrigger(tim,tim.FInterval);
      if tim.FAutoDestroy then
        begin
        if assigned(TH) then
          if TH=tim.FMyThr then
            begin
            autodes:=tim.FMyThrOwner;
            tim.FMyThr:=nil;
            end;
        RtcFreeAndNil(tim);
        end
      else
        tim:=nil;

      rtcLeaveTimer;
      intimer:=False;

      if assigned(TH) then
        begin
        if not TRtcThread.PostJob(TH, JO) then
          if JO is TRtcJob then
            if TRtcJob(JO).SingleUse then
              RtcFreeAndNil(JO);
        if autodes then
          TRtcThread.Stop(TH);
        end;
      end
    else // Disable ...
      begin
      tim.FActive:=False;
      if tim.FAutoDestroy then
        RtcFreeAndNil(tim)
      else
        tim:=nil;
      end;
  finally
    if intimer then rtcLeaveTimer;
    end;
  end;

class procedure TRtcTimer.Disable(me:TObject);
  begin
  if pointer(me)<>nil then
    if rtcEnterTimer(RtcIntPtr(me)) then
      try
        if TRtcTimer(me).FActive then
          begin
          TRtcTimer(me).FActive:=False;
          if TRtcTimer(me).FBackThread then
            BackTimer.RemoveTrigger(TRtcTimer(me))
          else
            MainTimer.RemoveTrigger(TRtcTimer(me));
          end;
      finally
        rtcLeaveTimer;
        end;
  end;

class procedure TRtcTimer.Reset(me:TObject);
  begin
  if pointer(me)<>nil then
    if rtcEnterTimer(RtcIntPtr(me)) then
      try
        if TRtcTimer(me).FActive then
          if TRtcTimer(me).FBackThread then
            BackTimer.ChangeTrigger(TRtcTimer(me), TRtcTimer(me).FInterval)
          else
            MainTimer.ChangeTrigger(TRtcTimer(me), TRtcTimer(me).FInterval);
      finally
        rtcLeaveTimer;
        end;
  end;

class procedure TRtcTimer.Tick(me:TObject);
  begin
  if pointer(me)<>nil then
    if rtcEnterTimer(RtcIntPtr(me)) then
      try
        if TRtcTimer(me).FActive then
          if TRtcTimer(me).FBackThread then
            BackTimer.ChangeTrigger(TRtcTimer(me),0)
          else
            MainTimer.ChangeTrigger(TRtcTimer(me),0);
      finally
        rtcLeaveTimer;
        end;
  end;

class procedure TRtcTimer.Enable(me:TObject; Wait: Cardinal; Event: TRtcTimerEvent; AutoDisable:boolean=False; AutoDestroy:boolean=False);
  begin
  if pointer(me)<>nil then
    if rtcEnterTimer(RtcIntPtr(me)) then
      try
        with TRtcTimer(me) do
          begin
          FAutoDisable:=AutoDisable or AutoDestroy;
          FAutoDestroy:=AutoDestroy;
          if FMyThrOwner and (FMyThr=nil) then
            FMyThr:=TRtcThread.Create;
          FThr:=FMyThr;
          FJob:=nil;
          FEvent:=Event;

          FInterval:=Wait;
          if FInterval<1 then FInterval:=1;

          if FActive then
            begin
            if FBackThread then
              BackTimer.ChangeTrigger(TRtcTimer(me), FInterval)
            else
              MainTimer.ChangeTrigger(TRtcTimer(me), FInterval);
            end
          else
            begin
            FActive:=True;
            if FBackThread then
              BackTimer.AddTrigger(TRtcTimer(me), FInterval)
            else
              MainTimer.AddTrigger(TRtcTimer(me), FInterval);
            end;
          end;
      finally
        rtcLeaveTimer;
        end;
  end;

class procedure TRtcTimer.Enable(me:TObject; Wait:Cardinal; Job:TObject; Thr:TRtcThread=nil; AutoDisable:boolean=False; AutoDestroy:boolean=False);
  begin
  if pointer(me)<>nil then
    if rtcEnterTimer(RtcIntPtr(me)) then
      try
        with TRtcTimer(me) do
          begin
          FAutoDisable:=AutoDisable or AutoDestroy;
          FAutoDestroy:=AutoDestroy;
          FEvent:=nil;
          if Thr=nil then
            begin
            if FMyThrOwner and (FMyThr=nil) then
              FMyThr:=TRtcThread.Create;
            FThr:=FMyThr;
            end
          else
            FThr:=Thr;
          FJob:=Job;

          FInterval:=Wait;
          if FInterval<1 then FInterval:=1;

          if FActive then
            begin
            if FBackThread then
              BackTimer.ChangeTrigger(TRtcTimer(me), FInterval)
            else
              MainTimer.ChangeTrigger(TRtcTimer(me), FInterval);
            end
          else
            begin
            FActive:=True;
            if FBackThread then
              BackTimer.AddTrigger(TRtcTimer(me), FInterval)
            else
              MainTimer.AddTrigger(TRtcTimer(me), FInterval);
            end;
          end;
      finally
        rtcLeaveTimer;
        end;
  end;

function TRtcTimer.GetMyThr: TRtcThread;
  begin
  if assigned(FMyThr) then
    Result:=FMyThr
  else if FMyThrOwner then
    begin
    if rtcEnterTimer(RtcIntPtr(self)) then
      try
        if FMyThrOwner and (FMyThr=nil) then
          FMyThr:=TRtcThread.Create;
      finally
        rtcLeaveTimer;
        end;
    Result:=FMyThr;
    end
  else
    Result:=nil;
  end;

initialization
{$IFDEF RTC_DEBUG} StartLog; Log('rtcTimer Initializing ...','DEBUG');{$ENDIF}

Timers_Running:=True;
TimerThreads:=0;
MainTimer:=nil;
BackTimer:=nil;

TimCS:=TRtcCritSec.Create;
_Outside:=TRtcEvent.Create(True,True);

{$IFDEF RTC_DEBUG} Log('rtcTimer Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcTimer Finalizing ...','DEBUG');{$ENDIF}

CloseTimerPool;

RtcFreeAndNil(TimList);
RtcFreeAndNil(TimCS);
RtcFreeAndNil(_Outside);

{$IFDEF RTC_DEBUG} Log('rtcTimer Finalized.','DEBUG');{$ENDIF}
end.