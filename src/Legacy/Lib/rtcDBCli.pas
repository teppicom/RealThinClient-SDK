{
  @html(<b>)
  Client DataSet component (Legacy)
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This is a LEGACY unit, which means that continued use of this unit is discouraged.
  If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
  released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.
}
unit rtcDBCli;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,
  Variants,
  DB,
  DBClient,

  rtcTypes,
  rtcInfo,
  rtcDB;

type
  { @abstract(Client DataSet + Tracking Changes)
    TClientDataSet component extended with the feature to Track Changes,
    making it similar in functionality to the TRtcMemDataSet component but
    with all the features you can find in the TClientDataSet component from Delphi. 
    Changes made to the TDataSet are accessible through the ExtractChanges method,
    which returns a RTC Value object containing enough information to apply all 
    these changes (remotely) to another dataset or database (on the Server). }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcClientDataSet=class(TClientDataSet)
  private
    FCache:TRtcDataSetRecorder;

    FActive:boolean;
    FLinked:boolean;

    FBeforeEdit: TDataSetNotifyEvent;
    FBeforeDelete: TDataSetNotifyEvent;

    FAfterCancel: TDataSetNotifyEvent;
    FBeforeInsert: TDataSetNotifyEvent;

    FAfterPost: TDataSetNotifyEvent;
    FAfterDelete: TDataSetNotifyEvent;

    FOnDataChange: TNotifyEvent;

    procedure XBeforeEdit(DataSet: TDataSet);
    procedure XBeforeDelete(DataSet: TDataSet);

    procedure XAfterCancel(DataSet: TDataSet);
    procedure XBeforeInsert(DataSet: TDataSet);

    procedure XAfterPost(DataSet: TDataSet);
    procedure XAfterDelete(DataSet: TDataSet);

    procedure XCacheChange(Sender:TObject);

    procedure ActivateMonitoring;
    procedure DeactivateMonitoring;

    procedure SetActiveMonitor(Value:boolean);

    procedure SetRtcDataSet(Value:TRtcDataSet);

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Extract all DataSet changes into a RTC Value object & give-up ownership of extracted data.
      Returns NIL if no changes were made to DataSet since last extraction.
      Caller needs to destroy received TRtcValue instance (if Result<>NIL). }
    function ExtractChanges:TRtcValue;

    { Assign Field definition and Data from the TRtcDataSet object to this TClientDataSet's storage.
      No changes are made to the assigned TRtcDataSet instance (it will NOT be changed or destroyed).
      Clears Field definition and all stored Data by using "asDataSet:=nil". }
    property asDataSet:TRtcDataSet write SetRtcDataSet;

  published
    { Activate/Deactivate DataSet monitoring }
    property TrackChanges:boolean read FActive write SetActiveMonitor default False;

    { Event triggered every time after DataSet was modified.
      Use "ExtractChanges" to get a RTC Value object containing all changes. }
    property OnDataChange:TNotifyEvent read FOnDataChange write FOnDataChange;
    end;

implementation

{ TRtcClientDataSet }

constructor TRtcClientDataSet.Create(AOwner: TComponent);
  begin
  inherited;
  end;

destructor TRtcClientDataSet.Destroy;
  begin
  DeactivateMonitoring;
  inherited;
  end;

procedure TRtcClientDataSet.ActivateMonitoring;
  begin
  if FActive and not FLinked then
    begin
    if not (csDesigning in ComponentState) then
      begin
      FCache:=TRtcDataSetRecorder.Create(self);
      FCache.OnChange:=XCacheChange;

      FBeforeEdit:=self.BeforeEdit;
      FBeforeDelete:=self.BeforeDelete;
      FAfterCancel:=self.AfterCancel;
      FBeforeInsert:=self.BeforeInsert;
      FAfterPost:=self.AfterPost;
      FAfterDelete:=self.AfterDelete;

      self.BeforeEdit:=XBeforeEdit;
      self.BeforeDelete:=XBeforeDelete;
      self.AfterCancel:=XAfterCancel;
      self.BeforeInsert:=XBeforeInsert;
      self.AfterPost:=XAfterPost;
      self.AfterDelete:=XAfterDelete;
      end;

    FLinked:=True;
    end;
  end;

procedure TRtcClientDataSet.DeactivateMonitoring;
  begin
  if FLinked then
    begin
    FLinked:=False;

    if not (csDesigning in ComponentState) then
      begin
      self.BeforeEdit:=FBeforeEdit;
      self.BeforeDelete:=FBeforeDelete;
      self.AfterCancel:=FAfterCancel;
      self.BeforeInsert:=FBeforeInsert;
      self.AfterPost:=FAfterPost;
      self.AfterDelete:=FAfterDelete;

      FBeforeEdit:=nil;
      FBeforeDelete:=nil;
      FAfterCancel:=nil;
      FBeforeInsert:=nil;
      FAfterPost:=nil;
      FAfterDelete:=nil;

      RtcFreeAndNil(FCache);
      end;
    end;
  end;

procedure TRtcClientDataSet.SetActiveMonitor(Value:boolean);
  begin
  if FActive<>Value then
    begin
    FActive:=Value;
    if FActive then
      ActivateMonitoring
    else
      DeactivateMonitoring;
    end;
  end;

procedure TRtcClientDataSet.SetRtcDataSet(Value:TRtcDataSet);
  var
    tc:boolean;
  begin
  tc:=TrackChanges;
  TrackChanges:=False;
  self.DisableControls;
  try
    // Copy field definitions from RTC DataSet to our in-memory Client DataSet
    RtcDataSetFieldsToDelphi(Value, self);
    self.CreateDataSet;
    // Copy all data Rows from RTC DataSet to our in-memory Client DataSet
    RtcDataSetRowsToDelphi(Value, self);
  finally
    // Enable DataSet Monitoring and visual Controls afterwards
    try
      self.EnableControls;
    finally
      TrackChanges:=tc;
      // Move to the 1st row to update visual Controls
      self.First;
      end;
    end;
  end;

procedure TRtcClientDataSet.XBeforeEdit(DataSet: TDataSet);
  begin
  FCache.BeforeChange;
  if assigned(FBeforeEdit) then FBeforeEdit(DataSet);
  end;

procedure TRtcClientDataSet.XBeforeDelete(DataSet: TDataSet);
  begin
  FCache.BeforeChange;
  if assigned(FBeforeDelete) then FBeforeDelete(DataSet);
  end;

procedure TRtcClientDataSet.XAfterCancel(DataSet: TDataSet);
  begin
  FCache.CancelChange;
  if assigned(FAfterCancel) then FAfterCancel(DataSet);
  end;

procedure TRtcClientDataSet.XBeforeInsert(DataSet: TDataSet);
  begin
  FCache.CancelChange;
  if assigned(FBeforeInsert) then FBeforeInsert(DataSet);
  end;

procedure TRtcClientDataSet.XAfterPost(DataSet: TDataSet);
  begin
  FCache.AfterChange;
  if assigned(FAfterPost) then FAfterPost(DataSet);
  end;

procedure TRtcClientDataSet.XAfterDelete(DataSet: TDataSet);
  begin
  FCache.AfterChange;
  if assigned(FAfterDelete) then FAfterDelete(DataSet);
  end;

function TRtcClientDataSet.ExtractChanges: TRtcValue;
  begin
  if assigned(FCache) then
    Result:=FCache.ExtractChanges
  else
    Result:=nil;
  end;

procedure TRtcClientDataSet.XCacheChange(Sender: TObject);
  begin
  if assigned(FOnDataChange) then
    FOnDataChange(self);
  end;

end.
