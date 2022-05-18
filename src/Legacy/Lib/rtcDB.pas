{
  @html(<b>)
  Database related classes and functions (Legacy)
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This is a LEGACY unit, which means that continued use of this unit is discouraged.
  If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
  released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.
}
unit rtcDB;

{$INCLUDE rtcDefs.inc}

interface

{$DEFINE RTC_DATASET}

{$IFDEF UNICODE_2009up}
  {$DEFINE RTC_UNICODE}
{$ELSE}
  {$IFDEF FPC}
    {$DEFINE RTC_WIDECHAR}
    {$DEFINE RTC_NOUNICODE}
  {$ELSE}
    {$IFNDEF IDE_2009up}
      {$DEFINE RTC_ANSICHAR}
      {$DEFINE RTC_NOUNICODE}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF NEXTGEN}
  {$UNDEF RTC_DATASET}
{$ENDIF}

uses
  Classes, SysUtils, Variants, DB,

  {$IFDEF RTC_DATASET}
    {$IFNDEF FPC} SqlTimSt, {$ENDIF}
    FmtBcd,
    rtcZLib,
  {$ENDIF}

  rtcTypes,
  rtcSystem,
  rtcInfo;

type
  { Class used for all Exceptions raised from DB-related classes and functions }
  ERtcDB=class(Exception);

  { @abstract(DataSet Change Recorder)
    Used by TRtcDataSetMonitor, TRtcMemDataSet and TRtcClientDataSet to record DataSet changes.
    Can also be used by custom TDataSet classes to record changes by calling the
    "BeforeChange" method from TDataSet's BeforeEdit and BeforeDelete events,
    "AfterChange" method from TDataSet's AfterPost and AfterDelete events and
    "CancelChange" method from TDataSet's AfterCancel and BeforeInsert events. }
  TRtcDataSetRecorder=class(TObject)
  private
    FChanges:TRtcArray;
    FChangeList:TRtcArray; // pointer to FChanges.asArray[0]
    FChangeSet:TRtcDataSet; // pointer to FChanges.asDataSet[1]
    FLastSaved:boolean; // TRUE if record was saved to FDataSet.asDataSet[1]
    FLastRowCount:integer; // Last known FDataSet.RecordCount

    FDataSet:TDataSet;

    FOnChange:TNotifyEvent;

  protected
    procedure DoChanged; virtual;

  public
    constructor Create(DataSet:TDataSet);

    destructor Destroy; override;

    { Call BeforeEdit & BeforeDelete }
    procedure BeforeChange;
    { Call AfterCancel & BeforeInsert }
    procedure CancelChange;
    { Call AfterPost & AfterDelete }
    procedure AfterChange;

    { Changes ready for extraction? }
    function Changed:boolean;
    { Extract all changes into a RTC Value object & give-up ownership of extracted data.
      Returns NIL if no changes were made since last extraction.
      Caller needs to destroy received TRtcValue instance. }
    function ExtractChanges:TRtcValue;

    { Triggered after each AfterChange call, if changes were recorded. }
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    end;

  { @abstract(TDataSet Monitor)
    Link a TRtcDataSetMonitor to any TDataSet component to be notified of
    any changes made to the TDataSet and get access to a RTC Value object containing
    enough information to apply all these changes (remotely) to another dataset or database. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcDataSetMonitor=class(TRtc_Component)
  private
    FDataSet:TDataSet;
    FCache:TRtcDataSetRecorder;

    FActive:boolean;
    FLinked:boolean;

    FBeforeEdit: TDataSetNotifyEvent;
    FBeforeDelete: TDataSetNotifyEvent;

    FAfterCancel: TDataSetNotifyEvent;
    FBeforeINsert: TDataSetNotifyEvent;

    FAfterPost: TDataSetNotifyEvent;
    FAfterDelete: TDataSetNotifyEvent;

    FOnDataChange: TNotifyEvent;

    procedure BeforeEdit(DataSet: TDataSet);
    procedure BeforeDelete(DataSet: TDataSet);

    procedure AfterCancel(DataSet: TDataSet);
    procedure BeforeInsert(DataSet: TDataSet);

    procedure AfterPost(DataSet: TDataSet);
    procedure AfterDelete(DataSet: TDataSet);

    procedure DoCacheChange(Sender:TObject);

    procedure ActivateMonitoring;
    procedure DeactivateMonitoring;

    procedure SetDataSet(const Value: TDataSet);
    procedure SetActive(Value:boolean);

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Changes ready for extraction? }
    function Changed:boolean;

    { Extract all DataSet changes into a RTC Value object & give-up ownership of extracted data.
      Returns NIL if no changes were made to DataSet since last extraction.
      Caller needs to destroy received TRtcValue instance (if Result<>NIL). }
    function ExtractChanges:TRtcValue;

  published
    { Activate/Deactivate DataSet monitoring }
    property Active:boolean read FActive write SetActive default False;

    { DataSet to be monitored }
    property DataSet:TDataSet read FDataSet write SetDataSet;

    { Event triggered every time after DataSet was modified.
      Use "ExtractChanges" to get a RTC Value object containing all changes. }
    property OnDataChange:TNotifyEvent read FOnDataChange write FOnDataChange;
    end;

{$IFDEF RTC_DATASET}

{$IFDEF IDE_XE3up}
  // @exclude
  TRtcRecordBuffer=TRecordBuffer;
  TRtcValueBuffer=TValueBuffer;
{$ELSE}
  {$IFDEF IDE_2009up}
    // @exclude
    TRtcRecordBuffer=PByte;
    TRtcValueBuffer=Pointer;
  {$ELSE}
    // @exclude
    TRtcRecordBuffer=PChar;
    TRtcValueBuffer=Pointer;
  {$ENDIF}
{$ENDIF}

{$IFDEF IDE_XE2up}
  // @exclude
  TRtcNativeInt=NativeInt;
{$ELSE}
  {$IFDEF FPC}
    // @exclude
    TRtcNativeInt=Ptrint;
    {$DEFINE RTC_DSNOPS}
  {$ELSE}
    // @exclude
    TRtcNativeInt=Longint;
  {$ENDIF}
{$ENDIF}

  { Base DataSet implementation (stub)
    @exclude }
  TRtcBaseDataSet = class(TDataSet)
  protected
    { IProviderSupport }
    {$IFNDEF RTC_DSNOPS}
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer = nil): Integer; overload; override;

    {$IFDEF IDE_2009up}
    function PSGetCommandText: string; override;
    function PSGetCommandType: TPSCommandType; override;
    {$ENDIF}
    procedure PSSetCommandText(const CommandText: string); overload; override;

    function PSGetKeyFields: string; override;
    function PSGetQuoteChar: string; override;
    function PSGetTableName: string; override;

    procedure PSStartTransaction; override;
    function PSInTransaction: Boolean; override;
    procedure PSEndTransaction(Commit: Boolean); override;

    function PSGetParams: TParams; override;
    procedure PSSetParams(AParams: TParams); override;

    procedure PSExecute; override;
    procedure PSGetAttributes(List: TList); override;

    function PSGetDefaultOrder: TIndexDef; override;

    function PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs; override;

    procedure PSReset; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
    {$ENDIF}

  {$IFDEF FPC}public{$ELSE}protected{$ENDIF}
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); override;
    procedure SetFieldData(Field: TField; Buffer: TRtcValueBuffer); overload; override;

  protected
    procedure CheckActive; override;
    procedure CheckInactive; override;
    procedure ClearBuffers; override;
    procedure ClearCalcFields(Buffer: TRtcRecordBuffer); override;
    procedure CloseBlob(Field: TField); override;
    procedure DataEvent(Event: TDataEvent; Info: TRtcNativeInt); override;

    function FindRecord(Restart, GoForward: Boolean): Boolean; override;

    {$IFNDEF FPC}
      function CreateNestedDataSet(DataSetField: TDataSetField): TDataSet; override;
      procedure DefChanged(Sender: TObject); override;
      function GetAggregateValue(Field: TField): Variant; override;
      function GetAggRecordCount(Grp: TGroupPosInd): Integer; override;
      procedure ResetAggField(Field: TField); override;
      procedure BlockReadNext; override;
    {$ENDIF}

    procedure ActivateBuffers; override;
    function GetCanModify: Boolean; override;
    function GetDataSource: TDataSource; override;
    function GetIsIndexField(Field: TField): Boolean; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;

    procedure InternalCancel; override;
    procedure InternalEdit; override;
    procedure InternalInsert; override;
    procedure InternalRefresh; override;

    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    procedure SetRecNo(Value: Integer); override;
    procedure UpdateIndexDefs; override;

    procedure DoAfterCancel; override;
    procedure DoAfterClose; override;
    procedure DoAfterDelete; override;
    procedure DoAfterEdit; override;
    procedure DoAfterInsert; override;
    procedure DoAfterOpen; override;
    procedure DoAfterPost; override;
    procedure DoAfterRefresh; override;
    procedure DoAfterScroll; override;
    procedure DoBeforeCancel; override;
    procedure DoBeforeClose; override;
    procedure DoBeforeDelete; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeOpen; override;
    procedure DoBeforePost; override;
    procedure DoBeforeRefresh; override;
    procedure DoBeforeScroll; override;
    procedure DoOnCalcFields; override;
    procedure DoOnNewRecord; override;

  protected { Method stubs for UniDirectional/Readonly/Unbuffered datasets }
    function GetRecordSize: Word; override;
    function AllocRecordBuffer: TRtcRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRtcRecordBuffer); override;

    procedure GetBookmarkData(Buffer: TRtcRecordBuffer; Data: Pointer); overload; override;
    procedure SetBookmarkData(Buffer: TRtcRecordBuffer; Data: Pointer); overload; override;

    function GetBookmarkFlag(Buffer: TRtcRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRtcRecordBuffer; Value: TBookmarkFlag); override;

    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalInitRecord(Buffer: TRtcRecordBuffer); override;    
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRtcRecordBuffer); override;

  protected { abstract methods required for all datasets }
    function GetRecord(Buffer: TRtcRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;

    function IsCursorOpen: Boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;

  protected {indirect creation of internal objects}
    {$IFDEF IDE_2009up}
    function GetLookupListClass(Field: TField): TLookupListClass; override;
    function GetFieldDefsClass: TFieldDefsClass; override;
    function GetFieldDefListClass: TFieldDefListClass; override;
    function GetFieldsClass: TFieldsClass; override;
    function GetFieldListClass: TFieldListClass; override;
    function GetCheckConstraintsClass: TCheckConstraintsClass; override;
    function GetAggFieldsClass: TFieldsClass; override;
    function GetIndexDefsClass: TIndexDefsClass; override;
    function GetParamsClass: TParamsClass; override;
    {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;

    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    function GetCurrentRecord(Buffer: TRtcRecordBuffer): Boolean; override;
  {$IFDEF IDE_XE4up}
    function GetFieldData(Field: TField; var Buffer: TRtcValueBuffer): Boolean; override;
  {$ELSE}
    function GetFieldData(Field: TField; Buffer: TRtcValueBuffer): Boolean; override;
  {$ENDIF}

    {$IFNDEF FPC}
      procedure GetDetailDataSets(List: TList); override;
      procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
    {$ENDIF}

    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;

    function Translate(Src, Dest: RtcPtrAnsiChar; ToOem: Boolean): Integer; override;
    function UpdateStatus: TUpdateStatus; override;

    {$IFNDEF FPC}
      property AggFields; // : TFields read FAggFields;
      property DataSetField; // : TDataSetField read FDataSetField write SetDataSetField;
      property Designer; // : TDataSetDesigner read FDesigner;
      property BlockReadSize; // : Integer read FBlockReadSize write SetBlockReadSize;
      property FieldDefList; // : TFieldDefList read FFieldDefList;
      property FieldList; // : TFieldList read FFieldList;
      property ObjectView; // : Boolean read FObjectView write SetObjectView;
      property SparseArrays; // : Boolean read FSparseArrays write SetSparseArrays;
    {$ENDIF}

    property Bof; // : Boolean read FBOF;
    property Bookmark; // : TBookmark read GetBookmark write GotoBookmark;
    property CanModify; // : Boolean read GetCanModify;
    property DataSource; // : TDataSource read GetDataSource;
    {$IFNDEF IDE_XE6up}
      property DefaultFields; // : Boolean read FDefaultFields;
    {$ENDIF}
    property Eof; // : Boolean read FEOF; {Upper case Eof conflicts with C++}
    property FieldCount; // : Integer read GetFieldCount;
    property FieldDefs; // : TFieldDefs read FFieldDefs write SetFieldDefs;
    property Fields; // : TFields read FFields;
    property FieldValues; // [const FieldName: string]: Variant read GetFieldValue write SetFieldValue; default;
    property Found; // : Boolean read GetFound;
    property IsUniDirectional; // : Boolean read FIsUniDirectional default False;
    property Modified; // : Boolean read FModified;
    property RecordCount; // : Integer read GetRecordCount;
    property RecNo; // : Integer read GetRecNo write SetRecNo;
    property RecordSize; // : Word read GetRecordSize;
    property State; // : TDataSetState read FState;
    property Filter; // : string read FFilterText write SetFilterText;
    property Filtered; // : Boolean read FFiltered write SetFiltered default False;
    property FilterOptions; // : TFilterOptions read FFilterOptions write SetFilterOptions default [];
    property Active; // : Boolean read GetActive write SetActive default False;
    property AutoCalcFields; // : Boolean read FAutoCalcFields write FAutoCalcFields default True;
    property BeforeOpen; // : TDataSetNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen; // : TDataSetNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeClose; // : TDataSetNotifyEvent read FBeforeClose write FBeforeClose;
    property AfterClose; // : TDataSetNotifyEvent read FAfterClose write FAfterClose;
    property BeforeInsert; // : TDataSetNotifyEvent read FBeforeInsert write FBeforeInsert;
    property AfterInsert; // : TDataSetNotifyEvent read FAfterInsert write FAfterInsert;
    property BeforeEdit; // : TDataSetNotifyEvent read FBeforeEdit write FBeforeEdit;
    property AfterEdit; // : TDataSetNotifyEvent read FAfterEdit write FAfterEdit;
    property BeforePost; // : TDataSetNotifyEvent read FBeforePost write FBeforePost;
    property AfterPost; // : TDataSetNotifyEvent read FAfterPost write FAfterPost;
    property BeforeCancel; // : TDataSetNotifyEvent read FBeforeCancel write FBeforeCancel;
    property AfterCancel; // : TDataSetNotifyEvent read FAfterCancel write FAfterCancel;
    property BeforeDelete; // : TDataSetNotifyEvent read FBeforeDelete write FBeforeDelete;
    property AfterDelete; // : TDataSetNotifyEvent read FAfterDelete write FAfterDelete;
    property BeforeScroll; // : TDataSetNotifyEvent read FBeforeScroll write FBeforeScroll;
    property AfterScroll; // : TDataSetNotifyEvent read FAfterScroll write FAfterScroll;
    property BeforeRefresh; // : TDataSetNotifyEvent read FBeforeRefresh write FBeforeRefresh;
    property AfterRefresh; // : TDataSetNotifyEvent read FAfterRefresh write FAfterRefresh;
    property OnCalcFields; // : TDataSetNotifyEvent read FOnCalcFields write FOnCalcFields;
    property OnDeleteError; // : TDataSetErrorEvent read FOnDeleteError write FOnDeleteError;
    property OnEditError; // : TDataSetErrorEvent read FOnEditError write FOnEditError;
    property OnFilterRecord; // : TFilterRecordEvent read FOnFilterRecord write SetOnFilterRecord;
    property OnNewRecord; // : TDataSetNotifyEvent read FOnNewRecord write FOnNewRecord;
    property OnPostError; // : TDataSetErrorEvent read FOnPostError write FOnPostError;
  end;

  // @exclude
  PRtcRecordInfo = ^TRtcRecordInfo;
  // @exclude
  TRtcRecordInfo = packed record
    RowData: TRtcArray;
    Calc: array of byte;
    Linked: boolean;
    Bookmark: Integer;
    BookmarkFlag: TBookmarkFlag;
    end;

  { @abstract(DB-aware in-Memory DataSet, used to view and manipulate TRtcDataSet data)

    To work with data stored in a TRtcDataSet object, assign it to the
    @Link(TRtcMemDataSet.asObject) or @Link(TRtcMemDataSet.asDataSet) property
    before seting the Active property to true.

    If FileName is assigned, data will be read from a file when DataSet is being Opened,
    and stored to a File when DataSet is being closed. If you don't assign a TRtcDataSet
    component before opening TRtcMemDataSet, a new and empty TRtcDataSet will be created. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcMemDataSet = class(TRtcBaseDataSet)
  private
    FData: TRtcDataSet;
    FOrigData: TRtcArray;
    FFileName: String;

    FCache: TRtcDataSetRecorder;
    FOnDataChange: TNotifyEvent;
    FTrackChanges: boolean;

    function GetDataSet: TRtcDataSet;
    procedure SetDataSet(const Value: TRtcDataSet);
    procedure SetFileName(const Value: String);
    procedure SetDataSetCopy(const Value: TRtcDataSet);
    procedure SetTrackChanges(const Value: boolean);

    procedure DoCacheChange(Sender:TObject);

  protected
    // @exclude
    function AllocRecordBuffer: TRtcRecordBuffer; override;
    // @exclude
    procedure FreeRecordBuffer(var Buffer: TRtcRecordBuffer); override;
    // @exclude
    procedure GetBookmarkData(Buffer: TRtcRecordBuffer; Data: Pointer); override;
    // @exclude
    function GetBookmarkFlag(Buffer: TRtcRecordBuffer): TBookmarkFlag; override;
    // @exclude
    function GetRecord(Buffer: TRtcRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    // @exclude
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    // @exclude
    procedure InternalClose; override;
    // @exclude
    procedure InternalDelete; override;
    // @exclude
    procedure InternalFirst; override;
    // @exclude
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    // @exclude
    procedure InternalHandleException; override;
    // @exclude
    procedure InternalInitFieldDefs; override;
    // @exclude
    procedure InternalInitRecord(Buffer: TRtcRecordBuffer); override;
    // @exclude
    procedure InternalLast; override;
    // @exclude
    procedure InternalOpen; override;
    // @exclude
    procedure InternalPost; override;

    // @exclude
    procedure ClearCalcFields(Buffer: TRtcRecordBuffer); override;

    // @exclude
    procedure DoBeforeCancel; override;

    // @exclude
    procedure DoBeforeEdit; override;
    // @exclude
    procedure DoBeforeDelete; override;
    // @exclude
    procedure DoAfterCancel; override;
    // @exclude
    procedure DoBeforeInsert; override;
    // @exclude
    procedure DoAfterPost; override;
    // @exclude
    procedure DoAfterDelete; override;

    // @exclude
    procedure InternalSetToRecord(Buffer: TRtcRecordBuffer); override;
    // @exclude
    function IsCursorOpen: Boolean; override;
    // @exclude
    procedure SetBookmarkFlag(Buffer: TRtcRecordBuffer; Value: TBookmarkFlag); override;
    // @exclude
    procedure SetBookmarkData(Buffer: TRtcRecordBuffer; Data: Pointer); override;

    // @exclude
    function GetRecordCount: Integer; override;
    // @exclude
    function GetRecNo: Integer; override;
    // @exclude
    procedure SetRecNo(Value: Integer); override;

    // @exclude
    function GetActiveRecordBuffer: TRtcRecordBuffer;

  {$IFDEF FPC}public{$ELSE}protected{$ENDIF}
    // @exclude
    procedure SetFieldData(Field: TField; Buffer: TRtcValueBuffer); override;
    // @exclude
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); override;

  public
    // @exclude
    destructor Destroy; override;

    { Extract all changes made to DataSet into a RTC Value object and give up ownership of extracted data.
      Returns NIL if no changes were made to the DataSet since last extraction.
      Caller needs to destroy received TRtcValue instance. }
    function ExtractChanges:TRtcValue;

    // @exclude
  {$IFDEF IDE_XE4up}
    function GetFieldData(Field: TField; var Buffer: TRtcValueBuffer): Boolean; override;
  {$ELSE}
    function GetFieldData(Field: TField; Buffer: TRtcValueBuffer): Boolean; override;
  {$ENDIF}

    // Create a temporary Blob Stream for accessing the Blob field "Field"
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    { Assign a TRtcDataSet object directly to this TRtcMemDataSet's storage without making a copy.
      Remove the assignment without destroying the original, by using "asObject:=nil;".
      Unless you remove the assignment by using "asObject:=nil", the TRtcDataSet object assigned will
      be destroyed automatically with any new assignment, or when the TRtcMemDataSet's Active
      property is set to FALSE (DataSet closed) or when this TRtcMemDataSet component is destroyed. }
    property asObject:TRtcDataSet read GetDataSet write SetDataSet;

    { Assign a copy of the TRtcDataSet object to this TRtcMemDataSet's storage,
      destroying the last assigned TRtcDataSet instance, or destroy currently
      assigned TRtcDataSet instance by using "asDataSet:=nil". }
    property asDataSet:TRtcDataSet read GetDataSet write SetDataSetCopy;

  published
    { Set Active to TRUE to enable access to internal TRtcDataSet data if it was assigned to "asDataSet" or "asObject".
      If TRtcDataSet was NOT assigned to "asDataSet" nor "asObject", load data from disk if "FileName" property if set.
      Set Active to FALSE to disable access to internal TRtcDataSet, writing any changes to disk if "FileName" was set. }
    property Active;

    { File name from which Data will be read when Opened and to which data will be saved when Closed.
      By changing this FileName while DataSet is active, then closing the DataSet,
      all modifications will be stored into the last filename assigned. }
    property FileName: String read FFileName write SetFileName;

    { Set "TrackChanges" to TRUE if you want to track all changes made to this DataSet.
      Use the "OnDataChange" event to get notified when changes are ready and
      use the "ExtractChanges" function to extract data from "change cache". }
    property TrackChanges:boolean read FTrackChanges write SetTrackChanges default False;

    property Filter;
    property Filtered;
    property FilterOptions;

    property AutoCalcFields;

    { If you set the "TrackChanges" property to TRUE, this
      event will be triggered every time after DataSet was modified.
      Use "ExtractChanges" to get a TRtcValue object containing all changes.
      If "TrackChanges" is FALSE, this event will NOT be triggered. }
    property OnDataChange:TNotifyEvent read FOnDataChange write FOnDataChange;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;

    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;

  end;

  // @exclude
  TRtcBlobStream = class(TStream)
  private
    FDataSet : TRtcMemDataSet;
    FStream : TStream;
  protected
    {$IFDEF IDE_7up}function GetSize: Int64; override;{$ENDIF}
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;
{$ENDIF}

var
  GetRTCFieldType:function(ft:TFieldType):TRtcFieldTypes;

// Convert RTC Field Type to Delphi Field Type
function RTC_FIELD2DB_TYPE(val:TRtcFieldTypes):TFieldType;

// Convert Delphi Field Type to RTC Field Type
Function RTC_DB2FIELD_TYPE(val:TFieldType):TRtcFieldTypes;

// Copy data from a Delphi TDataSet into a TRtcDataSet (used for transport)
procedure DelphiDataSetToRtc(DelphiDS:TDataSet; rtcDS:TRtcDataSet; ClearFieldDefs:boolean=True; OnlyDataFields:boolean=True);

// Copy data from a Delphi TDataSet into a TRtcArray of TRtcRecords (used for transport)
procedure DelphiDataSetToRtcArray(DelphiDS:TDataSet; rtcArr:TRtcArray; OnlyDataFields:boolean=False);

// Copy field definition from a TRtcDataSet (used for transport ) to a Delphi TDataSet
procedure RtcDataSetFieldsToDelphi(rtcDS:TRtcDataSet; DelphiDS:TDataSet);

// Copy data rows from a TRtcDataSet (used for transport) to a Delphi TDataSet
procedure RtcDataSetRowsToDelphi(rtcDS:TRtcDataSet; DelphiDS:TDataSet);

{ Extend the SQL "WHERE" clause with a new record filter:
  Filter = record with filter names and values;
  ParamPrefix = prefix to be used for field names in Params;
  CompareOperator = compare operator to be used in the filter (=, <, >, <=, >=);
  TableName = Table Name;
  SqlWhere = SQL "WHERE" clause to be modified/extended }
procedure RtcPrepareSqlWhere(Filter:TRtcRecord;
                             ParamPrefix:String;
                             CompareOperator:String;
                             const TableName:String;
                             var SqlWhere:String);

{ Set SQL Where Params with record filter values (call "rtcPrepareSqlWhere" to prepare the SQL statement first!):
  qry = SQL Query Params to be extended;
  Filter = record with filter names and values;
  ParamPrefix = prefix to be used for field names in Params }
procedure RtcSetSqlWhereParams(Filter:TRtcRecord;
                               ParamPrefix:String;
                               qry:TParams);

{ Prepare Action SQL statement (INSERT/UPDATE/DELETE):
  chg = TRtcDataSetChanges object positioned at the action to be applied;
  TableName = Table Name;
  SqlWhere = Optional SQL "WHERE" clause;
  Result = SQL Statement for executing the Action }
function RtcPrepareSqlAction(chg:TRtcDataSetChanges;
                             const TableName:String;
                             SqlWhere:String=''):String;

{ Set SQL Action Params:
  qry = SQL Query Params to be extended;
  chg = TRtcDataSetChanges object positioned at the action to be applied }
procedure RtcSetSqlActionParams(chg:TRtcDataSetChanges;
                                qry:TParams);

{ Internal function for skipping TGraphicField headers inside a BlobStream.
  Can be used in custom functions for copying a Delphi DataSet to a RTC DataSet. }
procedure RtcSkipGraphicFieldHeader(BlobStream:TStream);

implementation

type
  TGraphicHeader = record
    Count: Word;                { Fixed at 1 }
    HType: Word;                { Fixed at $0100 }
    Size: Longint;              { Size not including header }
  end;

procedure RtcSkipGraphicFieldHeader(BlobStream:TStream);
  var
    Size: Longint;
    Header: TGraphicHeader;
  begin
  Size := BlobStream.Size;
  if Size >= SizeOf(TGraphicHeader) then
    begin
    BlobStream.Read(Header, SizeOf(Header));
    if (Header.Count <> 1) or (Header.HType <> $0100) or
       (Header.Size <> Size - SizeOf(Header)) then
      BlobStream.Position := 0;
    end;
  end;

function RTC_FIELD2DB_TYPE(val:TRtcFieldTypes):TFieldType;
  begin
  if Ord(val)<=Ord(high(TFieldType)) then
    Result:=TFieldType(Ord(val))
  else
    raise ERtcDB.Create('Unknown or unsupported Field Type');
  end;

Function RTC_DB2FIELD_TYPE(val:TFieldType):TRtcFieldTypes;
  begin
  if Ord(val)<=Ord(high(TRtcFieldTypes)) then
    Result:=TRtcFieldTypes(Ord(val))
  else
    raise ERtcDB.Create('Unknown or unsupported Field Type');
  end;

procedure RtcDataSetFieldsToDelphi(rtcDS:TRtcDataSet; DelphiDS:TDataSet);
  var
    flds:integer;
    fldname:RtcWideString;
  begin
  DelphiDS.Active:=False;
  DelphiDS.FieldDefs.Clear;
  if rtcDS=nil then Exit;

  for flds:=0 to rtcDS.FieldCount-1 do
    begin
    fldname:=rtcDS.FieldName[flds];
    DelphiDS.FieldDefs.Add(String(fldname),
                           RTC_FIELD2DB_TYPE(rtcDS.FieldType[fldname]),
                           rtcDS.FieldSize[fldname],
                           rtcDS.FieldRequired[fldname]);
    end;
  end;

procedure RtcDataSetRowsToDelphi(rtcDS:TRtcDataSet; DelphiDS:TDataSet);
  var
    flds:integer;
    fldname:RtcWideString;
    field:TField;
    fstream:TStream;
  begin
  DelphiDS.Active:=True;
  if rtcDS=nil then Exit;

  rtcDS.First;
  while not rtcDS.Eof do
    begin
    DelphiDS.Append;
    for flds:=0 to rtcDS.FieldCount-1 do
      begin
      fldname:=rtcDS.FieldName[flds];
      field:=DelphiDS.FindField(String(fldname));
      if assigned(field) then
        if not rtcDS.isNull[fldname] then
          if field.isBlob then
            begin
            fstream:=DelphiDS.CreateBlobStream(field,bmWrite);
            try
              fstream.CopyFrom(rtcDS.asByteStream[fldname], rtcDS.asByteStream[fldname].Size);
            finally
              fstream.Free;
              end;
            end
          else
            case RTC_FIELD2VALUE_TYPES[rtcDS.FieldType[fldname]] of
              rtc_Currency: field.AsCurrency:=rtcDS.asCurrency[fldname];
              rtc_DateTime: field.AsDateTime:=rtcDS.asDateTime[fldname];
              rtc_String: field.AsString:=String(rtcDS.asString[fldname]);
            {$IFDEF UNICODE}rtc_Text: field.AsWideString:=rtcDS.asText[fldname];{$ENDIF}
              else field.Value:=rtcDS.Value[fldname];
              end;
      end;
    DelphiDS.Post;
    rtcDS.Next;
    end;
  end;

procedure DelphiDataSetToRtcArray(DelphiDS:TDataSet; rtcArr:TRtcArray; OnlyDataFields:boolean=False);
  var
    flds:integer;
    fldname:RtcWideString;
    field:TField;
    fstream:TStream;
    idx:integer;
    rtcDS:TRtcRecord;
  begin
  idx:=0;
  DelphiDS.First;
  while not DelphiDS.Eof do
    begin
    rtcDS:=rtcArr.newRecord(idx);
    Inc(idx);
    for flds:=0 to DelphiDS.Fields.Count-1 do
      begin
      field:=DelphiDS.Fields[flds];
      if assigned(field) then
        begin
        fldname:=RtcWideString(field.FieldName);
        if not field.IsNull then
          if (OnlyDataFields=False) or (field.FieldKind=fkData) then
            if field.isBlob then
              begin
              fstream:=DelphiDS.CreateBlobStream(field,bmRead);
              try
                if {$IFNDEF FPC} TBlobField(field).GraphicHeader and {$ENDIF}
                  ( (field.DataType = ftGraphic) or
                    (field.DataType = ftTypedBinary) ) then
                  RtcSkipGraphicFieldHeader(fstream);
                rtcDS.NewByteStream(fldname).CopyFrom(fstream,fstream.Size-fstream.Position);
              finally
                fstream.Free;
                end;
              end
            else
              case RTC_FIELD2VALUE_TYPES[RTC_DB2FIELD_TYPE(field.DataType)] of
                rtc_Currency: rtcDS.asCurrency[fldname]:=field.AsCurrency;
                rtc_DateTime: rtcDS.asDateTime[fldname]:=field.AsDateTime;
                rtc_String: rtcDS.asString[fldname]:=RtcString(field.AsString);
              {$IFDEF UNICODE}rtc_Text: rtcDS.asText[fldname]:=field.AsWideString;{$ENDIF}
                else rtcDS.Value[fldname]:=field.Value;
                end;
        end;
      end;
    DelphiDS.Next;
    end;
  end;

procedure DelphiDataSetToRtc(DelphiDS:TDataSet; rtcDS:TRtcDataSet; ClearFieldDefs:boolean=True; OnlyDataFields:boolean=True);
  var
    flds:integer;
    fldname:RtcWideString;
    field:TField;
    fstream:TStream;
  begin
  if ClearFieldDefs then
    begin
    rtcDS.Clear;
    for flds:=0 to DelphiDS.Fields.Count-1 do
      begin
      field:=DelphiDS.Fields[flds];
      if assigned(field) then
        begin
        fldname:=RtcWideString(field.FieldName);
        if (OnlyDataFields=False) or (field.FieldKind=fkData) then
          rtcDS.SetField(fldname,
                         RTC_DB2FIELD_TYPE(field.DataType),
                         field.Size,
                         field.Required);
        end;
      end;
    end;

  DelphiDS.First;
  while not DelphiDS.Eof do
    begin
    rtcDS.Append;
    for flds:=0 to rtcDS.FieldCount-1 do
      begin
      fldname:=rtcDS.FieldName[flds];
      field:=DelphiDS.FindField(String(fldname));
      if assigned(field) then
        if not field.IsNull then
          if (OnlyDataFields=False) or (field.FieldKind=fkData) then
            if field.isBlob then
              begin
              fstream:=DelphiDS.CreateBlobStream(field,bmRead);
              try
                if {$IFNDEF FPC} TBlobField(field).GraphicHeader and {$ENDIF}
                  ( (field.DataType = ftGraphic) or
                    (field.DataType = ftTypedBinary) ) then
                  RtcSkipGraphicFieldHeader(fstream);
                rtcDS.NewByteStream(fldname).CopyFrom(fstream,fstream.Size-fstream.Position);
              finally
                fstream.Free;
                end;
              end
            else
              case RTC_FIELD2VALUE_TYPES[rtcDS.FieldType[fldname]] of
                rtc_Currency: rtcDS.asCurrency[fldname]:=field.AsCurrency;
                rtc_DateTime: rtcDS.asDateTime[fldname]:=field.AsDateTime;
                rtc_String: rtcDS.asString[fldname]:=RtcString(field.AsString);
              {$IFDEF UNICODE}rtc_Text: rtcDS.asText[fldname]:=field.AsWideString;{$ENDIF}
                else rtcDS.Value[fldname]:=field.Value;
                end;
      end;
    DelphiDS.Next;
    end;
  end;

procedure DelphiFieldsToRtc(const DelphiDS:TDataSet; const rtcDS:TRtcDataSet);
  var
    flds:integer;
    fldname:RtcWideString;
    field:TField;
  begin
  for flds:=0 to DelphiDS.Fields.Count-1 do
    begin
    field:=DelphiDS.Fields[flds];
    if assigned(field) then
      begin
      fldname:=RtcWideString(field.FieldName);
      if field.FieldKind=fkData then
        rtcDS.SetField(fldname,
                       RTC_DB2FIELD_TYPE(field.DataType),
                       field.Size,
                       field.Required);
      end;
    end;
  end;

procedure DelphiRowToRtc(const DelphiDS:TDataSet; const rtcDS:TRtcDataSet);
  var
    flds:integer;
    fldname:RtcWideString;
    field:TField;
    fstream:TStream;
  begin
  rtcDS.Append;
  for flds:=0 to rtcDS.FieldCount-1 do
    begin
    fldname:=rtcDS.FieldName[flds];
    field:=DelphiDS.FindField(String(fldname));
    if assigned(field) then
      if (field.FieldKind=fkData) and not field.IsNull then
        if field.isBlob then
          begin
          fstream:=DelphiDS.CreateBlobStream(field,bmRead);
          try
            if {$IFNDEF FPC} TBlobField(field).GraphicHeader and {$ENDIF}
              ( (field.DataType = ftGraphic) or
                (field.DataType = ftTypedBinary) ) then
              RtcSkipGraphicFieldHeader(fstream);
            rtcDS.NewByteStream(fldname).CopyFrom(fstream,fstream.Size-fstream.Position);
          finally
            fstream.Free;
            end;
          end
        else
          case RTC_FIELD2VALUE_TYPES[rtcDS.FieldType[fldname]] of
            rtc_Currency: rtcDS.asCurrency[fldname]:=field.AsCurrency;
            rtc_DateTime: rtcDS.asDateTime[fldname]:=field.AsDateTime;
            rtc_String: rtcDS.asString[fldname]:=RtcString(field.AsString);
          {$IFDEF UNICODE}rtc_Text: rtcDS.asText[fldname]:=field.AsWideString;{$ENDIF}
            else rtcDS.Value[fldname]:=field.Value;
            end;
    end;
  end;

procedure RtcClearBlobFields(const rtcDS:TRtcDataSet);
  var
    flds:integer;
    fldname:RtcWideString;
  begin
  for flds:=0 to rtcDS.FieldCount-1 do
    begin
    fldname:=rtcDS.FieldName[flds];
    if rtcDS.isType[fldname]=rtc_ByteStream then
      rtcDS.isNull[fldname]:=True;
    end;
  end;

{ Extend the SQL "WHERE" clause with a new record filter.
  Filter = record with filter names and values,
  ParamPrefix = prefix to be used for field names in Params
  CompareOperator = compare sign to be used in the filter (=, <, >, <=, >=)
  TableName = Table Name
  SqlWhere = SQL "WHERE" clause to be modified/extended }
procedure RtcPrepareSqlWhere(Filter:TRtcRecord;
                             ParamPrefix:String;
                             CompareOperator:String;
                             const TableName:String;
                             var SqlWhere:String);
  var
    fld:integer;
    fname:RtcWideString;
  begin
  if assigned(Filter) then
    for fld:=0 to Filter.FieldCount-1 do
      begin
      fname:=Filter.FieldName[fld];
      if SqlWhere='' then
        SqlWhere:=' WHERE '
      else
        SqlWhere:=SqlWhere+' AND ';
      if Filter.isNull[fname] then
        SqlWhere:=SqlWhere+'('+TableName+'."'+String(fname)+'" is NULL)'
      else
        SqlWhere:=SqlWhere+'('+TableName+'."'+String(fname)+'" '+CompareOperator+' :'+ParamPrefix+IntToStr(fld)+')';
      end;
  end;

{ Set SQL Where Params with record filter values (call "rtcPrepareSqlWhere" to prepare the SQL statement first!)
  qry = SQL Query Params to be extended
  Filter = record with filter names and values,
  ParamPrefix = prefix to be used for field names in Params }
procedure RtcSetSqlWhereParams(Filter:TRtcRecord;
                               ParamPrefix:String;
                               qry:TParams);
  var
    fld:integer;
    fname:RtcWideString;
  begin
  if assigned(Filter) then
    for fld:=0 to Filter.FieldCount-1 do
      begin
      fname:=Filter.FieldName[fld];
      if not Filter.isNull[fname] then
        qry.ParamByName(ParamPrefix+IntToStr(fld)).Value:=Filter.Value[fname];
      end;
  end;

{ Prepare Action SQL statement (INSERT/UPDATE/DELETE)
  chg = TRtcDataSetChanges object positioned at the action to be applied
  TableName = Table Name
  SqlWhere = Optional SQL "WHERE" clause
  Result = SQL Statement for executing the Action
}
function RtcPrepareSqlAction(chg:TRtcDataSetChanges;
                             const TableName:String;
                             SqlWhere:String=''):String;
  var
    fld:integer;
    fname:RtcWideString;
    s1,s2:String;
    nrow,orow:TRtcDataRow;
  begin
  case chg.Action of
    rds_Insert:
      begin
      s1:='';
      s2:='';
      nrow:=chg.NewRow;
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        if not nrow.isNull[fname] then
          begin
          if s1<>'' then s1:=s1+', ';
          if s2<>'' then s2:=s2+', ';
          s1:=s1+TableName+'."'+String(fname)+'"';
          s2:=s2+':v'+IntToStr(fld);
          end;
        end;
      Result:='INSERT INTO "'+TableName+'" ('+s1+') VALUES ('+s2+')' + SqlWhere;
      end;

    rds_Update:
      begin
      s2:='';
      orow:=chg.OldRow;
      nrow:=chg.NewRow;
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        if (orow.isType[fname]<>rtc_ByteStream) and
           ((orow.isType[fname]<>rtc_Float) or (frac(orow.asFloat[fname])=0)) then
          begin
          if SqlWhere='' then
            SqlWhere:=' WHERE '
          else
            SqlWhere:=SqlWhere+' AND ';
          if orow.isNull[fname] then
            SqlWhere:=SqlWhere+'('+TableName+'."'+String(fname)+'" is NULL)'
          else
            SqlWhere:=SqlWhere+'('+TableName+'."'+String(fname)+'" = :w'+IntToStr(fld)+')';
          end;
        end;
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        if orow.asCode[fname]<>nrow.asCode[fname] then
          begin
          if s2<>'' then s2:=s2+', ';
          if nrow.isNull[fname] then
            s2:=s2+TableName+'."'+String(fname)+'" = NULL'
          else
            s2:=s2+TableName+'."'+String(fname)+'" = :v'+IntToStr(fld);
          end;
        end;
      Result:='UPDATE "'+TableName+'" SET '+s2 + SqlWhere;
      end;

    rds_Delete:
      begin
      orow:=chg.OldRow;
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        if (orow.isType[fname]<>rtc_ByteStream) and
           ((orow.isType[fname]<>rtc_Float) or (frac(orow.asFloat[fname])=0)) then
          begin
          if SqlWhere='' then
            SqlWhere:=' WHERE '
          else
            SqlWhere:=SqlWhere+' AND ';
          if orow.isNull[fname] then
            SqlWhere:=SqlWhere+'('+TableName+'."'+String(fname)+'" is NULL)'
          else
            SqlWhere:=SqlWhere+'('+TableName+'."'+String(fname)+'" = :w'+IntToStr(fld)+')';
          end;
        end;
      Result:='DELETE FROM "'+TableName+'"' + SqlWhere;
      end;
    end;
  end;

{ Set SQL Action Params
  qry = SQL Query Params to be extended
  chg = TRtcDataSetChanges object positioned at the action to be applied
}
procedure RtcSetSqlActionParams(chg:TRtcDataSetChanges;
                                qry:TParams);
  var
    fld:integer;
    fname:RtcWideString;
    nrow,orow:TRtcDataRow;
  begin
  case chg.Action of
    rds_Insert:
      begin
      nrow:=chg.NewRow;
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        if not nrow.isNull[fname] then
          with qry.ParamByName('v'+IntToStr(fld)) do
            begin
            DataType:=RTC_FIELD2DB_TYPE(nrow.FieldType[fname]);
            Value:=nrow.Value[fname];
            end;
        end;
      end;

    rds_Update:
      begin
      orow:=chg.OldRow;
      nrow:=chg.NewRow;
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        if (orow.isType[fname]<>rtc_ByteStream) and
           ((orow.isType[fname]<>rtc_Float) or (frac(orow.asFloat[fname])=0)) then
          if not orow.isNull[fname] then
            with qry.ParamByName('w'+IntToStr(fld)) do
              begin
              DataType:=RTC_FIELD2DB_TYPE(orow.FieldType[fname]);
              Value:=orow.Value[fname];
              end;
        end;
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        if orow.asCode[fname]<>nrow.asCode[fname] then
          if not nrow.isNull[fname] then
            with qry.ParamByName('v'+IntToStr(fld)) do
              begin
              DataType:=RTC_FIELD2DB_TYPE(nrow.FieldType[fname]);
              Value:=nrow.Value[fname];
              end;
        end;
      end;

    rds_Delete:
      begin
      orow:=chg.OldRow;
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        if (orow.isType[fname]<>rtc_ByteStream) and
           ((orow.isType[fname]<>rtc_Float) or (frac(orow.asFloat[fname])=0)) then
          if not orow.isNull[fname] then
            with qry.ParamByName('w'+IntToStr(fld)) do
              begin
              DataType:=RTC_FIELD2DB_TYPE(orow.FieldType[fname]);
              Value:=orow.Value[fname];
              end;
        end;
      end;
    end;
  end;

{ TRtcDataSetMonitor }

constructor TRtcDataSetMonitor.Create(AOwner: TComponent);
  begin
  inherited;
  end;

destructor TRtcDataSetMonitor.Destroy;
  begin
  DeactivateMonitoring;
  FDataSet:=nil;
  inherited;
  end;

procedure TRtcDataSetMonitor.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited;
  if (Operation=opRemove) and (AComponent=FDataSet) then
    begin
    DeactivateMonitoring;
    FDataSet:=nil;
    end;
  end;

procedure TRtcDataSetMonitor.SetDataSet(const Value: TDataSet);
  begin
  if FDataSet <> Value then
    begin
    DeactivateMonitoring;
    FDataSet:=Value;
    ActivateMonitoring;
    end;
  end;

procedure TRtcDataSetMonitor.ActivateMonitoring;
  begin
  if assigned(FDataSet) and FActive and not FLinked then
    begin
    if not (csDesigning in ComponentState) then
      begin
      FCache:=TRtcDataSetRecorder.Create(DataSet);
      FCache.OnChange:=DoCacheChange;

      FBeforeEdit:=DataSet.BeforeEdit;
      FBeforeDelete:=DataSet.BeforeDelete;
      FAfterCancel:=DataSet.AfterCancel;
      FBeforeInsert:=DataSet.BeforeInsert;
      FAfterPost:=DataSet.AfterPost;
      FAfterDelete:=DataSet.AfterDelete;

      DataSet.BeforeEdit:=BeforeEdit;
      DataSet.BeforeDelete:=BeforeDelete;
      DataSet.AfterCancel:=AfterCancel;
      DataSet.BeforeInsert:=BeforeInsert;
      DataSet.AfterPost:=AfterPost;
      DataSet.AfterDelete:=AfterDelete;
      end;

    FLinked:=True;
    end;
  end;

procedure TRtcDataSetMonitor.DeactivateMonitoring;
  begin
  if assigned(FDataSet) and FLinked then
    begin
    FLinked:=False;

    if not (csDesigning in ComponentState) then
      begin
      DataSet.BeforeEdit:=FBeforeEdit;
      DataSet.BeforeDelete:=FBeforeDelete;
      DataSet.AfterCancel:=FAfterCancel;
      DataSet.BeforeInsert:=FBeforeInsert;
      DataSet.AfterPost:=FAfterPost;
      DataSet.AfterDelete:=FAfterDelete;

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

procedure TRtcDataSetMonitor.SetActive(Value:boolean);
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

procedure TRtcDataSetMonitor.BeforeEdit(DataSet: TDataSet);
  begin
  FCache.BeforeChange;
  if assigned(FBeforeEdit) then FBeforeEdit(DataSet);
  end;

procedure TRtcDataSetMonitor.BeforeDelete(DataSet: TDataSet);
  begin
  FCache.BeforeChange;
  if assigned(FBeforeDelete) then FBeforeDelete(DataSet);
  end;

procedure TRtcDataSetMonitor.AfterCancel(DataSet: TDataSet);
  begin
  FCache.CancelChange;
  if assigned(FAfterCancel) then FAfterCancel(DataSet);
  end;

procedure TRtcDataSetMonitor.BeforeInsert(DataSet: TDataSet);
  begin
  FCache.CancelChange;
  if assigned(FBeforeInsert) then FBeforeInsert(DataSet);
  end;

procedure TRtcDataSetMonitor.AfterPost(DataSet: TDataSet);
  begin
  FCache.AfterChange;
  if assigned(FAfterPost) then FAfterPost(DataSet);
  end;

procedure TRtcDataSetMonitor.AfterDelete(DataSet: TDataSet);
  begin
  FCache.AfterChange;
  if assigned(FAfterDelete) then FAfterDelete(DataSet);
  end;

function TRtcDataSetMonitor.Changed: boolean;
  begin
  if assigned(FCache) then
    Result:=FCache.Changed
  else
    Result:=False;
  end;

function TRtcDataSetMonitor.ExtractChanges: TRtcValue;
  begin
  if assigned(FCache) then
    Result:=FCache.ExtractChanges
  else
    Result:=nil;
  end;

procedure TRtcDataSetMonitor.DoCacheChange(Sender: TObject);
  begin
  if assigned(FOnDataChange) then
    FOnDataChange(self);
  end;

{ TRtcDataSetRecorder }

constructor TRtcDataSetRecorder.Create(DataSet: TDataSet);
  begin
  inherited Create;

  if not assigned(DataSet) then
    raise ERtcDB.Create('Can not create TRtcDataSetRecorder without a DataSet');
  FDataSet:=DataSet;

  FChanges:=nil;
  FChangeList:=nil;
  FChangeSet:=nil;

  FLastSaved:=false;
  FLastRowCount:=0;
  end;

destructor TRtcDataSetRecorder.Destroy;
  begin
  FLastSaved:=false;
  FDataSet:=nil;

  RtcFreeAndNil(FChanges);
  FChangeList:=nil;
  FChangeSet:=nil;
  inherited;
  end;

procedure TRtcDataSetRecorder.BeforeChange;
  begin
  // mandatory: BeforeEdit, BeforeDelete
  if not assigned(FChanges) then
    begin
    FChanges:=TRtcArray.Create;
    FChangeList:=FChanges.NewArray(0);
    FChangeSet:=FChanges.NewDataSet(1);
    DelphiFieldsToRtc(FDataSet,FChangeSet);
    end;

  if FLastSaved then
    begin
    FLastSaved:=False;
    FChangeSet.Delete;
    end;

  FLastSaved:=True;
  FLastRowCount:=FDataSet.RecordCount;
  DelphiRowToRtc(FDataSet,FChangeSet);
  end;

procedure TRtcDataSetRecorder.CancelChange;
  begin
  // optional: AfterCancel and BeforeInsert
  if FLastSaved then
    begin
    FLastSaved:=False;
    FChangeSet.Delete;
    end;
  FLastRowCount:=FDataSet.RecordCount;
  end;

procedure TRtcDataSetRecorder.AfterChange;
  var
    rc:integer;
    orig,chng:RtcString;
  begin
  // mandatory: AfterPost, AfterDelete
  if not assigned(FChanges) then
    begin
    FChanges:=TRtcArray.Create;
    FChangeList:=FChanges.NewArray(0);
    FChangeSet:=FChanges.NewDataSet(1);
    DelphiFieldsToRtc(FDataSet,FChangeSet);
    FLastRowCount:=0;
    end;

  rc:=FDataSet.RecordCount;
  if rc>FLastRowCount then // Insert
    begin
    if FLastSaved then
      begin
      FLastSaved:=False;
      FChangeSet.Delete;
      end;
    FLastRowCount:=rc;
    DelphiRowToRtc(FDataSet,FChangeSet);
    FChangeList.asInteger[FChangeList.Count]:=Ord(rds_Insert);
    DoChanged;
    end
  else if rc<FLastRowCount then // Delete
    begin
    if not FLastSaved then
      raise ERtcDB.Create('Row deleted without calling BeforeChange');
    FLastSaved:=False;
    FLastRowCount:=rc;
    // RtcClearBlobFields(FChangeSet);
    FChangeList.asInteger[FChangeList.Count]:=Ord(rds_Delete);
    DoChanged;
    end
  else if FLastSaved then // Update
    begin
    FLastSaved:=False;
    orig:=FChangeSet.RowData.toCode;
    // RtcClearBlobFields(FChangeSet);
    DelphiRowToRtc(FDataSet,FChangeSet);
    chng:=FChangeSet.RowData.toCode;
    if orig<>chng then
      begin
      FChangeList.asInteger[FChangeList.Count]:=Ord(rds_Update);
      DoChanged;
      end
    else
      begin
      FChangeSet.Delete;
      FChangeSet.Delete;
      end;
    end;
  end;

function TRtcDataSetRecorder.Changed: boolean;
  begin
  if not assigned(FChanges) then
    Result:=False
  else
    Result:=FChangeList.Count>0;
  end;

function TRtcDataSetRecorder.ExtractChanges: TRtcValue;
  var
    ar:TRtcArray;
  begin
  if FLastSaved then
    begin
    if FChangeList.Count>0 then
      begin
      ar:=TRtcArray(FChangeSet.RowData.copyOf);

      Result:=TRtcValue.Create;
      Result.asObject:=FChanges;
      FChangeSet.Delete;

      FChanges:=TRtcArray.Create;
      FChangeList:=FChanges.NewArray(0);
      FChangeSet:=FChanges.NewDataSet(1);

      DelphiFieldsToRtc(FDataSet,FChangeSet);

      FChangeSet.Append;
      FChangeSet.RowData:=ar;
      end
    else
      Result:=nil;
    end
  else if assigned(FChanges) then
    begin
    if FChangeList.Count>0 then
      begin
      Result:=TRtcValue.Create;
      Result.asObject:=FChanges;
      end
    else
      begin
      Result:=nil;
      FChanges.Free;
      end;
    FChanges:=nil;
    FChangeList:=nil;
    FChangeSet:=nil;
    end
  else
    Result:=nil;
  end;

procedure TRtcDataSetRecorder.DoChanged;
  begin
  if assigned(FOnChange) then
    FOnChange(Self);
  end;

{$IFDEF RTC_DATASET}

{ TRtcBaseDataSet }

constructor TRtcBaseDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // code after create
end;

destructor TRtcBaseDataSet.Destroy;
begin
  // code before destroy
  inherited Destroy;
end;

{ Call CheckInactive to determine whether the connection to a database server is inactive.
  If the database connection is active, an ibxeDatabaseOpen exception is raised. }
procedure TRtcBaseDataSet.CheckInactive;
begin
  inherited;
  // If DataSet is active, try to Close it or raise exception.
end;

{ Call CheckActive to determine if the connection to a database server is active.
  If the database connection is inactive, an ibxeDatabaseClosed exception is raised. }
procedure TRtcBaseDataSet.CheckActive;
begin
  inherited;
  // if DataSet is not active, raise exception.
end;

{$IFNDEF FPC}

function TRtcBaseDataSet.CreateNestedDataSet(DataSetField: TDataSetField): TDataSet;
begin
  Result := inherited CreateNestedDataSet(DataSetField);
  // copy all required properties
end;

{ Provider helpers }

{ Datasets can represent master/detail relationships in two ways: as linked cursors or as nested dataset fields.
  GetDetailDataSets lists all detail datasets of the active record into List if they are not the value of a nested dataset field.
  To obtain a list of the detail datasets that are the values of nested dataset fields, use the NestedDataSets property instead. }
procedure TRtcBaseDataSet.GetDetailDataSets(List: TList);
begin
  inherited;
  { for I := FDataSources.Count - 1 downto 0 do
    with TDataSource(FDataSources[I]) do
      for J := FDataLinks.Count - 1 downto 0 do
        if (TDataLink(FDataLinks[J]) is TDetailDataLink) and
           (TDetailDataLink(FDataLinks[J]).DetailDataSet <> nil) and
           (TDetailDataLink(FDataLinks[J]).DetailDataSet.DataSetField = nil) then
          List.Add(TDetailDataLink(FDataLinks[J]).DetailDataSet); }
end;

{ As implemented in TDataSet, GetDetailLinkFields does nothing.
  Descendants override this method to fill the two lists with field components that define a
  master-detail relationship between this dataset and another (master) dataset.
  The MasterFields list is filled with fields from the master table whose values must equal the values
  of the fields in the DetailFields list. The DetailFields list is filled with fields from the calling dataset. }
procedure TRtcBaseDataSet.GetDetailLinkFields(MasterFields, DetailFields: TList);
begin
  // ???
end;

{ Field Management }

procedure TRtcBaseDataSet.DefChanged(Sender: TObject);
begin
  // ??
end;

{$ENDIF}

{ Most applications do not need to call GetFieldData.
  TField objects call this method to implement their GetData method.
  The Field or FieldNo parameter indicates the field whose data should be fetched.
  Field specifies the component itself, while FieldNo indicates its field number.
  The Buffer parameter is a memory buffer with sufficient space to accept the value of the field
  as it exists in the database (unformatted and untranslated).
  NativeFormat indicates whether the dataset fetches the field in the IDE's native format for the field type.
  When NativeFormat is false, the dataset must convert the field value to the native type.
  This allows the field to handle data from different types of datasets (ADO-based, BDE-based, and so on) in a uniform manner.

  GetFieldData returns a value that indicates whether the data was successfully fetched.

  As implemented in TDataSet, GetFieldData always returns false, indicating that no data was fetched from the specified field.
  Descendants override this method to fetch data in whatever way is appropriate to the implementation of the dataset. }
{$IFDEF IDE_XE4up}
function TRtcBaseDataSet.GetFieldData(Field: TField; var Buffer: TRtcValueBuffer): Boolean;
{$ELSE}
function TRtcBaseDataSet.GetFieldData(Field: TField; Buffer: TRtcValueBuffer): Boolean;
{$ENDIF}
begin
  Result := False;
  // ??
end;

procedure TRtcBaseDataSet.SetFieldData(Field: TField; Buffer: TRtcValueBuffer);
begin
  // ??
end;

procedure TRtcBaseDataSet.CloseBlob(Field: TField);
begin
  // ??
end;

{ Call CreateBlobStream to obtain a stream for reading and writing the value of the field specified by the Field parameter.
  The Mode parameter indicates whether the stream will be used for reading the field’s value (bmRead),
  writing the field’s value (bmWrite), or modifying the field’s value (bmReadWrite).

  Blob streams are created in a specific mode for a specific field on a specific record.
  Applications create a new blob stream every time the record in the dataset changes: do not reuse an existing blob stream.

  As implemented in TDataSet, CreateBlobStream always returns nil (Delphi) or NULL (C++).
  Descendants of TDataSet override this method to create the TStream descendant that reads and writes
  BLOB data in the format that dataset type uses to store BLOB fields.

  Tip: It is preferable to call CreateBlobStream rather than creating a blob stream directly in code.
  This ensures that the stream is appropriate to the dataset, and may also ensure that datasets that
  do not always store BLOB data in memory fetch the blob data before creating the stream. }
function TRtcBaseDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result:=nil;
  // ??
end;

procedure TRtcBaseDataSet.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);
begin
  inherited;
end;

{indirect creation of internal objects}

{$IFDEF IDE_2009up}

function TRtcBaseDataSet.GetLookupListClass(Field: TField): TLookupListClass;
begin
  Result:=inherited; // Result := TDefaultLookupList;
end;

function TRtcBaseDataSet.GetParamsClass: TParamsClass;
begin
  Result := DefaultParamsClass;
end;

function TRtcBaseDataSet.GetFieldsClass: TFieldsClass;
begin
  Result := DefaultFieldsClass;
end;

function TRtcBaseDataSet.GetAggFieldsClass: TFieldsClass;
begin
  Result := GetFieldsClass;
end;

function TRtcBaseDataSet.GetFieldListClass: TFieldListClass;
begin
  Result := DefaultFieldListClass;
end;

function TRtcBaseDataSet.GetFieldDefsClass: TFieldDefsClass;
begin
  Result := DefaultFieldDefsClass;
end;

function TRtcBaseDataSet.GetIndexDefsClass: TIndexDefsClass;
begin
  Result := DefaultIndexDefsClass;
end;

function TRtcBaseDataSet.GetFieldDefListClass: TFieldDefListClass;
begin
  Result := DefaultFieldDefListClass;
end;

function TRtcBaseDataSet.GetCheckConstraintsClass: TCheckConstraintsClass;
begin
  Result := DefaultCheckConstraintsClass;
end;

{$ENDIF}

{ Index Related }

function TRtcBaseDataSet.GetIsIndexField(Field: TField): Boolean;
begin
  Result := False;
  // ??
end;

procedure TRtcBaseDataSet.UpdateIndexDefs;
begin
  // ??
end;

{ Datasource/Datalink Interaction }

{ GetDataSource is exposed as a protected method so that descendants of TDSTableProducer
  can override the implementation of the DataSource property.
  The DataSource property for TDSTableProducer is an internally generated TDataSource object,
  not an external component that can be shared with other objects.
  It is surfaced only for internal APIs that require a TDataSource object rather than working directly with TDataSet. }
function TRtcBaseDataSet.GetDataSource: TDataSource;
begin
  Result := nil;
  // ??
end;

{ DataEvent is called automatically when various data events occur.
  Event indicates the event that just occurred. Info provides additional context information for some events.

  DataEvent dispatches these events by calling the appropriate methods, as indicated in the following table.

  Data event           Info    Method called
   deFieldChange
   deRecordChange      - The field that changed	RecordChanged
   deDataSetChange     - Current index into the record buffer	DataSetChanged
   deDataSetScroll     - Current index into the record buffer	DataSetScrolled
   deLayoutChange      - Current index into the record buffer	LayoutChanged
   deUpdateRecord      - UpdateRecord
   deUpdateState       - EditingChanged or ActiveChanged
   deCheckBrowseMode   - CheckBrowseMode
   deFocusControl      - FocusControl }
procedure TRtcBaseDataSet.DataEvent(Event: TDataEvent; Info: TRtcNativeInt);
begin
  inherited;
end;

{ Buffer Management }

function TRtcBaseDataSet.AllocRecordBuffer: TRtcRecordBuffer;
begin
  Result := nil;
  // ??
end;

procedure TRtcBaseDataSet.FreeRecordBuffer(var Buffer: TRtcRecordBuffer);
begin
  // ??
end;

procedure TRtcBaseDataSet.ClearBuffers;
begin
  inherited;
  // Buffers are clear
end;

procedure TRtcBaseDataSet.ActivateBuffers;
begin
  inherited;
  // Buffers are activated
end;

{ This function always returns false, indicating failure.
  For TDataSet descendants that override this method to retrieve the value of the current record,
  GetCurrentRecord returns true if Buffer is successfully filled with the value of the current record.

  TBDEDataSet: Most applications should not need to call GetCurrentRecord.
  TDataSet automatically allocates a buffer for the active record.
  Call GetCurrentRecord to copy the current record into a buffer allocated by the application.
  Buffer must be at least as big as the record size indicated by the RecordSize property.

  TClientDataSet: Most applications should not need to call GetCurrentRecord.
  TCustomClientDataSet automatically allocates a buffer for the active record.
  Call GetCurrentRecord to copy the current record into a buffer allocated by the application.
  Buffer must be at least as big as the number of bytes indicated by the RecordSize property. }
function TRtcBaseDataSet.GetCurrentRecord(Buffer: TRtcRecordBuffer): Boolean;
begin
  Result := False;
  // ??
end;

procedure TRtcBaseDataSet.ClearCalcFields(Buffer: TRtcRecordBuffer);
begin
  // ??
end;

{ Navigation }

procedure TRtcBaseDataSet.InternalFirst;
begin
  // ??
end;

procedure TRtcBaseDataSet.InternalLast;
begin
  // ??
end;

{ Streamed connection components use InternalOpen to handle the details of
  establishing a connection to the application server.
  This method is called by the DoConnect method (which implements the Connected property)
  and at other times when the connection component must establish a connection. }
procedure TRtcBaseDataSet.InternalOpen;
begin
  // ?? abstract
end;

{$IFNDEF FPC}
procedure TRtcBaseDataSet.BlockReadNext;
begin
  // Default block size = 1?
  MoveBy(1);
end;
{$ENDIF}

{ Editing }

{ As implemented by TDataSet, Translate copies a source string to a destination string, and ignores the value passed in ToOem.
  Translate provides a fallback method for derived dataset objects that do not reimplement Translate.
  In BDE-enabled datasets, Translate converts the source string from ANSI (the native character mapping for the VCL)
  to OEM (the native character mapping for the BDE) when ToOem is true, and reverses that translation when ToOem is false.
  Custom datasets can use Translate to convert strings betweenANSI and any character mapping used by the underlying data access mechanism.

  TBDEDataSet: When the ToOem parameter is true, Translate converts the source string from the ANSI character set to the OEM character set.
  If ToOem is false, Translate converts the source string from the OEM character set to the Microsoft ANSI character set.
  By default, BDE datasets work with strings in the OEM character set.   }
function TRtcBaseDataSet.Translate(Src, Dest: RtcPtrAnsiChar; ToOem: Boolean): Integer;
begin
  if (Src <> nil) then
  begin
    // Default for translate is just to copy
    if (Src <> Dest) then
      StrCopy(Dest, Src);
    Result := StrLen(Dest);
  end else
    Result := 0;
end;

{ Editing Stubs }

procedure TRtcBaseDataSet.InternalInitFieldDefs;
begin
  // ?? abstract
end;

procedure TRtcBaseDataSet.InternalInitRecord(Buffer: TRtcRecordBuffer);
begin
  // ??
end;

procedure TRtcBaseDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  // ??
end;

procedure TRtcBaseDataSet.InternalDelete;
begin
  // ??
end;

procedure TRtcBaseDataSet.InternalPost;
begin
  inherited; // CheckRequiredFields;
  // ??
end;

procedure TRtcBaseDataSet.InternalCancel;
begin
  // ??
end;

{ Streamed connection components use InternalClose to handle the details of closing a connection to the application server.
  This method is called by the DoDisconnect method (which implements the Connected property)
  and at other times when the connection component must terminate a connection.}
procedure TRtcBaseDataSet.InternalClose;
begin
  // ?? abstract
end;

procedure TRtcBaseDataSet.InternalEdit;
begin
  // ??
end;

{ The Insert method calls InternalInsert to add a node at the position specified by Index.
  Index specifies where to insert the node, where 0 is the first position, 1 is second position, and so on.
  If Index is –1, the node is added to the end of the list.
  Node is the node to add to the list.
  InternalInsert returns the index of the node after it is inserted.
  This is the same as the Index parameter when Index is not –1. }
procedure TRtcBaseDataSet.InternalInsert;
begin
  // ??
end;

procedure TRtcBaseDataSet.InternalRefresh;
begin
  // ??
end;

{ Bookmark Stubs }

procedure TRtcBaseDataSet.GetBookmarkData(Buffer: TRtcRecordBuffer; Data: Pointer);
begin
  // ??
end;

procedure TRtcBaseDataSet.SetBookmarkData(Buffer: TRtcRecordBuffer; Data: Pointer);
begin
  // ??
end;

function TRtcBaseDataSet.GetBookmarkFlag(Buffer: TRtcRecordBuffer): TBookmarkFlag;
begin
  Result:=bfCurrent;
  // ??
end;

procedure TRtcBaseDataSet.SetBookmarkFlag(Buffer: TRtcRecordBuffer; Value: TBookmarkFlag);
begin
  // ??
end;

procedure TRtcBaseDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  // ??
end;

procedure TRtcBaseDataSet.InternalHandleException;
begin
  // ?? abstract
end;

procedure TRtcBaseDataSet.InternalSetToRecord(Buffer: TRtcRecordBuffer);
begin
  // ??
end;

{ This function returns false in all cases, indicating that any bookmark is invalid because the
  TDataSet class does not implement support for bookmarks.
  Descendant classes that implement bookmark support override BookmarkValid to test bookmarks for
  validity and indicate when a bookmark is valid by returning true.

  TBDEDataSet: Call BookmarkValid to determine if a specified bookmark is currently assigned a value.
  Bookmark specifies the bookmark to test.
  BookmarkValid returns true if a bookmark is valid. Otherwise, it returns false. }
function TRtcBaseDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := False;
  // ??
end;

{ As implemented in TDataSet, CompareBookmarks always returns 0, indicating no difference between the bookmarks.
  This is because TDataSet does not implement support for bookmarks.
  Descendant classes that provide bookmark support override this method to return
  a value less than 0 if Bookmark1 is less than Bookmark2,
  0 if the bookmarks are identical, and
  a value greater than 0 if Bookmark1 is greater than Bookmark2.

  Call CompareBookmarks to determine if two bookmarks are identical.
  Bookmark1 and Bookmark2 are the bookmarks to compare.
  If the bookmarks differ, CompareBookmarks returns -1 if Bookmark1 is less than Bookmark2,
  1 if Bookmark1 is greater than Bookmark2, and
  0 if the bookmarks are identical, nil (Delphi), or NULL (C++) }
function TRtcBaseDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  Result := 0;
  // ??
end;

{ Filter / Locate / Find }

function TRtcBaseDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;
begin
  Result := False;
  // ??
end;

procedure TRtcBaseDataSet.SetFiltered(Value: Boolean);
begin
  inherited;
  // ??
end;

procedure TRtcBaseDataSet.SetFilterOptions(Value: TFilterOptions);
begin
  inherited;
  // ??
end;

procedure TRtcBaseDataSet.SetFilterText(const Value: string);
begin
  inherited;
  // ??
end;

procedure TRtcBaseDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  inherited;
  // ??
end;

{ This function:
  - Checks whether the dataset is unidirectional, and if so, raises an EDatabaseError exception.
	- Returns false, indicating that a matching record was not found and the active record was not changed.
  Descendant classes that are not unidirectional override this method so that it locates the record where the fields identified
  by the semicolon-separated list of fields in KeyFields have the values specified by the Variant or Variant array KeyValues.
  Options indicates whether the search is case insensitive and whether partial matches are supported. Locate returns true
  if a record is found that matches the specified criteria and that record is now active.

  TBDEDataSet:

  Call Locate to search a dataset for a specific record and position the cursor on it.
  KeyFields is a string containing a semicolon-delimited list of field names on which to search.
  KeyValues is a variant array containing the values to match in the key fields.
  If KeyFields lists a single field, KeyValues specifies the value for that field on the desired record.
  To specify multiple search values, pass a variant array as KeyValues, or construct a variant array on the
  fly using the VarArrayOf routine.

  For example:
  with CustTable do
    Locate('Company;Contact;Phone', VarArrayOf(['Sight Diver', 'P', '408-431-1000']), [loPartialKey]);

	TLocateOptions Opts;
  Opts.Clear();
  Opts << loPartialKey;
  Variant locvalues[2];
  locvalues[0] = Variant("Sight Diver");
  locvalues[1] = Variant("P");
	CustTable->Locate("Company;Contact", VarArrayOf(locvalues, 1), Opts);

  Options is a set that optionally specifies additional search latitude when searching on string fields.
  If Options contains the loCaseInsensitive setting, then Locate ignores case when matching fields.
  If Options contains the loPartialKey setting, then Locate allows partial-string matching on strings in KeyValues.
  If Options is an empty set, or if the KeyFields property does not include any string fields, Options is ignored.

  Locate returns true if it finds a matching record, and makes that record the current one. Otherwise Locate returns false.
  Locate uses the fastest possible method to locate matching records.
  If the search fields in KeyFields are indexed and the index is compatible with the specified search options,
  Locate uses the index. Otherwise Locate creates a filter for the search.}
function TRtcBaseDataSet.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  CheckBiDirectional;
  Result := False;
  // ??
end;

{ This function:
  - Checks whether the dataset is unidirectional, and if so, raises an EDatabaseError exception.
	- Returns a Variant with the value false, indicating that a matching record was not found.
  Descendant classes that are not unidirectional override this method so that it locates the record where the fields
  identified by the comma-delimited string KeyFields have the values specified by the Variant or Variant array KeyValues.
  In classes that implement Lookup, it returns a Variant or Variant array that contains the value or values of the fields
  specified by the comma-delimited string ResultFields on the specified record.

  TBDEDataSet:

  Call Lookup to search for a record in the dataset in which specified fields contain specified
  values and return other field values from the found record. Unlike other dataset search methods,
  Lookup performs its search without moving the record pointer of the dataset.
  This is useful when doing data validation on a record’s data while the record is still in edit or insert mode.

  KeyFields is a string containing the names of the field or fields on which the search is predicated.
  If the search is based on more than one field, KeyFields is a list of field names delimited with semicolons.

  KeyValues is a variant array containing the values to find in the fields specified in KeyFields.
  To specify multiple search values, pass KeyValues as a variant array as an argument, or construct
  a variant array on the fly using the VarArrayOf routine. These search values must be specified in the
  order of the fields in KeyFields to which each corresponds. For instance, the first value is used for
  the first field in KeyFields, the second value for the second field, and so on.

  ResultFields is a string containing the names of the field or fields from which to return values.
  If values are to be returned from more than one field, ResultFields is a list of field names delimited with semicolons.

  Lookup returns a variant array containing the values from the fields specified in ResultFields.

  Lookup uses the fastest possible method to locate matching records.
  If the search fields in KeyFields are indexed, Lookup uses the index.
  Otherwise Lookup creates a filter for the search.

  The example below uses the Lookup method to search the sample Paradox table Customer.
  The search is based on two fields: Company and State.
  The search is also based on the following two values in these two fields: "Blue Sports" and "OR".
  For the search to be successful, there must be a record where the Company field contains the
  value "Blue Sports" AND the field State contains "OR". If either criteria is not met, the search
  is unsuccessful (such a record does exist in this table, so the search will be successful).
  The values of the CustNo and Addr1 fields are returned by the execution of the Lookup method.
  If the search is unsuccessful, the VarType function applied to the returned variant array returns varNull.

  procedure TForm1.Button1Click(Sender: TObject);
    var
      V: Variant;
      C: Integer;
      A: String;
    begin
    V := Table1.Lookup('Company;State', VarArrayOf(['Blue Sports', 'OR']), 'CustNo;Addr1');
    if not (VarType(V) in [varNull]) then
      begin
      C := V[0];
      A := V[1];
      ShowMessage(IntToStr(C) + #10 + A);
      end
    else
      ShowMessage('Search unsuccessful!');
    end;  }
function TRtcBaseDataSet.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
begin
  CheckBiDirectional;
  Result := False;
  // ??
end;

{ Aggregates }

{$IFNDEF FPC}
function TRtcBaseDataSet.GetAggregateValue(Field: TField): Variant;
begin
  Result:=NULL;
  // ??
end;

function TRtcBaseDataSet.GetAggRecordCount(Grp: TGroupPosInd): Integer;
begin
  Result:=0;
  // ??
end;

procedure TRtcBaseDataSet.ResetAggField(Field: TField);
begin
  // ??
end;
{$ENDIF}

{ Informational }

function TRtcBaseDataSet.GetCanModify: Boolean;
begin
  Result:=True;
  // ??
end;

function TRtcBaseDataSet.GetRecord(Buffer: TRtcRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result:=grError;
  // ?? abstract
end;

function TRtcBaseDataSet.GetRecordCount: Longint;
begin
  Result:=-1;
  // ??
end;

function TRtcBaseDataSet.GetRecNo: Integer;
begin
  Result:=-1;
  // ??
end;

procedure TRtcBaseDataSet.SetRecNo(Value: Integer);
begin
  // ??
end;

function TRtcBaseDataSet.GetRecordSize: Word;
begin
  Result := 0;
  // ??
end;

function TRtcBaseDataSet.IsCursorOpen: Boolean;
begin
  Result:=False;
  // ?? abstract
end;

{ Use IsSequenced to determine whether the underlying database table supports sequence numbers,
  or whether these are computed by the dataset component. When IsSequenced returns true,
  applications can safely use the RecNo property to navigate to records in the dataset.
  As implemented in TDataSet, IsSequenced always returns true.
  Descendants of TDataSet reimplement this method to return a value that depends on the underlying table type.

  TBDEDataSet: Call IsSequenced to determine whether database records can be located by sequence numbers.
  When IsSequenced is true, the dataset can navigate directly to a specific record by setting the RecNo property.
  If IsSequenced is false, the only way to navigate to a specific record is to start at the beginning and count records. }
function TRtcBaseDataSet.IsSequenced: Boolean;
begin
  Result := True;
  // ??
end;

{ As implemented in TDataSet, UpdateStatus always returns usUnmodified.
  Descendant classes override this method to reflect whether the active record has been modified, and if so, how.

  TBDEDataSet:
  Call UpdateStatus to determine the update status for the current record in a dataset when cached updates are enabled.
  Update status can change frequently as records are edited, inserted, or deleted.
  UpdateStatus offers a convenient method for applications to assess the current status before undertaking or
  completing operations that depend on the update status of individual records in the dataset. }
function TRtcBaseDataSet.UpdateStatus: TUpdateStatus;
begin
  Result := usUnmodified;
  // ??
end;

{ Event Handler Helpers }

procedure TRtcBaseDataSet.DoAfterCancel;
begin
  inherited; // if Assigned(FAfterCancel) then FAfterCancel(Self);
end;

procedure TRtcBaseDataSet.DoAfterClose;
begin
  inherited; // if Assigned(FAfterClose) then FAfterClose(Self);
end;

procedure TRtcBaseDataSet.DoAfterDelete;
begin
  inherited; // if Assigned(FAfterDelete) then FAfterDelete(Self);
end;

procedure TRtcBaseDataSet.DoAfterEdit;
begin
  inherited; // if Assigned(FAfterEdit) then FAfterEdit(Self);
end;

procedure TRtcBaseDataSet.DoAfterInsert;
begin
  inherited; // if Assigned(FAfterInsert) then FAfterInsert(Self);
end;

procedure TRtcBaseDataSet.DoAfterOpen;
begin
  inherited; // if Assigned(FAfterOpen) then FAfterOpen(Self);  if not IsEmpty then DoAfterScroll;
end;

procedure TRtcBaseDataSet.DoAfterPost;
begin
  inherited; // if Assigned(FAfterPost) then FAfterPost(Self);
end;

procedure TRtcBaseDataSet.DoAfterRefresh;
begin
  inherited; // if Assigned(FAfterRefresh) then FAfterRefresh(Self);
end;

procedure TRtcBaseDataSet.DoAfterScroll;
begin
  inherited; // if Assigned(FAfterScroll) then FAfterScroll(Self);
end;

procedure TRtcBaseDataSet.DoBeforeCancel;
begin
  inherited; // if Assigned(FBeforeCancel) then FBeforeCancel(Self);
end;

procedure TRtcBaseDataSet.DoBeforeClose;
begin
  inherited; // if Assigned(FBeforeClose) then FBeforeClose(Self);
end;

procedure TRtcBaseDataSet.DoBeforeDelete;
begin
  inherited; // if Assigned(FBeforeDelete) then FBeforeDelete(Self);
end;

procedure TRtcBaseDataSet.DoBeforeEdit;
begin
  inherited; // if Assigned(FBeforeEdit) then FBeforeEdit(Self);
end;

procedure TRtcBaseDataSet.DoBeforeInsert;
begin
  inherited; // if Assigned(FBeforeInsert) then FBeforeInsert(Self);
end;

procedure TRtcBaseDataSet.DoBeforeOpen;
begin
  inherited; // if Assigned(FBeforeOpen) then FBeforeOpen(Self);
end;

procedure TRtcBaseDataSet.DoBeforePost;
begin
  inherited; // if Assigned(FBeforePost) then FBeforePost(Self);
end;

procedure TRtcBaseDataSet.DoBeforeRefresh;
begin
  inherited; // if Assigned(FBeforeRefresh) then FBeforeRefresh(Self);
end;

procedure TRtcBaseDataSet.DoBeforeScroll;
begin
  inherited; // if Assigned(FBeforeScroll) then FBeforeScroll(Self);
end;

procedure TRtcBaseDataSet.DoOnCalcFields;
begin
  inherited; // if Assigned(FOnCalcFields) then FOnCalcFields(Self);
end;

procedure TRtcBaseDataSet.DoOnNewRecord;
begin
  inherited; // if Assigned(FOnNewRecord) then FOnNewRecord(Self);
end;

{ IProviderSupport implementation }

{$IFNDEF RTC_DSNOPS}

{ The provider component applies updates within a transaction, if possible.
  To do so, it calls PSStartTransaction to start a transaction before applying updates, and calls
  PSEndTransaction when all updates are applied or too many errors are encountered.
  Note: TDataSetProvider only starts a transaction (Using PSStartTransaction) if the PSInTransaction method indicates
  that there is no transaction already established. That is, it does not assume nested transaction support is available. }
procedure TRtcBaseDataSet.PSStartTransaction;
begin
  // ??
end;

{ The provider component applies updates within a transaction, if possible.
  To do so, it first checks whether a transaction is already underway, and if not, generates one using PSStartTransaction.
  To determine whether a transaction is already underway, the provider calls PSInTransaction.
  PSInTransaction returns true if there is a transaction underway, and false otherwise. }
function TRtcBaseDataSet.PSInTransaction: Boolean;
begin
  Result := False;
  // ??
end;

{ The provider component applies updates within a transaction, if possible.
  To do so, it uses PSEndTransaction in conjunction with the PSStartTransaction method.
  The Commit parameter indicates whether the dataset should commit the current transaction
  (Commit is true), or roll it back (Commit is false).

  Note: TDataSetProvider only starts a transaction (Using PSStartTransaction)
  if the PSInTransaction method indicates that there is no transaction already established.
  That is, it does not assume nested transaction support is available.
  Thus, there is no need to ensure that PSEndTransaction does not terminate a transaction that was not started by the provider. }
procedure TRtcBaseDataSet.PSEndTransaction(Commit: Boolean);
begin
  // ??
end;

{ If the value its ResolveToDataSet property is false, a provider component calls PSExecuteStatement
  (indirectly) to execute SQL statements that it generates for applying updates.
  Usually, the dataset passes these statements on to its database server for execution.
  The ASQL parameter specifies the SQL command to execute.
  AParams contains any parameter values that should be applied to ASQL before execution.
  ResultSet allows PSExecuteStatement to return a dataset component that contains the results of a SELECT statement.
  If ResultSet is not nil (Delphi) or NULL (C++), PSExecuteStatement creates a new dataset component as the
  target of this pointer and fills it with the result set. The caller is responsible for freeing the dataset.
  PSExecuteStatement returns the number of rows affected by executing ASQL. }
function TRtcBaseDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer = nil): Integer;
begin
  Result := 0; // DatabaseError(SProviderSQLNotSupported, Self);
  // ??
end;

{ The provider component calls PSExecute to implement its Execute method. If the dataset represents a query or stored
  procedure that does not return a cursor, this method executes the query or stored procedure.
  If the provider Options includes poAllowCommandText and the dataset supports the PSSetCommandText method,
  the statement specified by PSSetCommandText may be executed instead.
  If there is not statement to execute, PSExecute raises an EDatabaseError exception. }
procedure TRtcBaseDataSet.PSExecute;
begin
  // DatabaseError(SProviderExecuteNotSupported, Self);
  // ??
end;

{ The provider component calls PSGetAttributes to add information about the dataset to the metadata stored with generated data packets.
  This information is stored as Name/Value pairs that are added to the list specified by the List parameter. }
procedure TRtcBaseDataSet.PSGetAttributes(List: TList);
begin
  // ??
end;

{$IFDEF IDE_2009up}
function TRtcBaseDataSet.PSGetCommandText: string;
begin
  Result := '';
  // ??
end;

function TRtcBaseDataSet.PSGetCommandType: TPSCommandType;
begin
  Result := ctUnknown;
  // ??
end;
{$ENDIF}

{ The provider component calls PSGetDefaultOrder to generate a default index, called DEFAULT_ORDER,
  for the records that appear in data packets. This index defines the default sort order for records
  when they appear in the client dataset on the client.
  PSGetDefaultOrder creates a TIndexDef object to describe the default index.
  The provider that calls PSGetDefaultOrder subsequently frees this TIndexDef object. }
function TRtcBaseDataSet.PSGetDefaultOrder: TIndexDef;
begin
  Result := nil;
  // ??
end;

{ The provider component calls PSGetKeyFields to determine what information is required to uniquely identify records in the data packet.
  This information allows it to locate the records that need to change when it applies updates.
  PSGetKeyFields returns a semicolon-delimited string that lists the names of the key fields of the dataset.
  By default (as implemented by TDataSet), these are the fields with pfInKey included in their ProviderFlags property. }
function TRtcBaseDataSet.PSGetKeyFields: string;
begin
  Result:=inherited PSGetKeyFields;
  if Result='' then
    begin
    // No fields declared with "pfInKey" in ProviderFlags? Use something else to return PKey fields separated with ";"
    end;
end;

{ The provider component calls PSGetParams to implement its GetParams method.
  PSGetParams returns a TParams object that describes the current values of all parameters of the dataset.
  If the dataset does not have any parameters, PSGetParams returns nil (Delphi) or NULL (C++).
  Note: The PSGetParams method should not create a new instance of TParams for the return value.
  The provider that calls PSGetParams does not free the returned value. }
function TRtcBaseDataSet.PSGetParams: TParams;
begin
  Result := nil;
  // ??
end;

{ The provider component calls PSGetQuoteChar (indirectly) when generating the dynamic SQL statements that apply updates.
  These statements are subsequently executed by a call to PSExecuteStatement.
  PSGetQuoteChar returns the character or characters that delimit quoted strings in the generated SQL.
  This string appears both before and after quoted strings. }
function TRtcBaseDataSet.PSGetQuoteChar: string;
begin
  Result := '';
  // ??
end;

{ PSGetTableName returns the name of the underlying database table against which updates are applied when
  the provider’s ResolveToDataSet property is false. This value is used for generating the SQL statements that apply updates.
  The generated SQL statements are subsequently executed by a call to PSExecuteStatement.
  If PSGetTableName does not return a table name, the provider must have an OnGetTableName event handler to supply this value. }
function TRtcBaseDataSet.PSGetTableName: string;
begin
  Result := '';
  // ??
end;

{ The provider component calls PSGetIndexDefs to locate an index that contains information it needs.
  For example, it uses PSGetIndexDefs to locate an index that contains the key fields returned by PSGetKeyFields.
  This information allows the provider to locate the records that need to change when it applies updates.
  The provider also calls this method to locate indexes that it adds to the metadata of data packets.

  IndexTypes indicates the options required of the returned indexes.
  PSGetIndexDefs returns all indexes whose Options property includes the specified flags.
  PSGetIndexDefs returns a TIndexDefs object that contains all the indexes of the dataset that match the IndexTypes parameter. }
function TRtcBaseDataSet.PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs;
begin
  Result := nil;
  // ??
end;

{ The provider calls PSIsSQLBased to determine whether the dataset supports some form of native SQL.
  This information is used when generating SQL commands that are executed by PSExecuteStatement.
  Even if the dataset is not SQL-based, it may still allow the provider to execute SQL statements for applying updates.
  For example, BDE-enabled local databases such as Paradox and DBASE provide SQL support via the BDE’s local SQL engine,
  but are not SQL-based. However, table and field names are not specified in the same way for the local SQL engine as
  they are for native SQL. The provider uses PSIsSQLBased to determine how these names should appear in generated SQL.

  Note: The PSIsSQLSupported method indicates whether the dataset can execute SQL commands at all,
  regardless of whether the dataset is SQL-based. }
function TRtcBaseDataSet.PSIsSQLBased: Boolean;
begin
  Result := False;
  // ??
end;

{ PSIsSQLSupported indicates whether the provider can call PSExecuteStatement to execute an SQL statement.
  When PSIsSQLSupported returns false, calling PSExecuteStatement generates an exception.
  The provider can still work with a dataset that returns false from PSIsSQLSupported, but it must set
  the ResolveToDataSet property to true, or provide data on a read-only basis. }
function TRtcBaseDataSet.PSIsSQLSupported: Boolean;
begin
  Result := False;
  // ??
end;

{ The provider calls PSReset to reposition the dataset on the first record before fetching records. }
procedure TRtcBaseDataSet.PSReset;
begin
  // ??
end;

{ If the provider’s Options property includes poAllowCommandText, clients can supply an SQL command
  along with a call to GetRecords or Execute. This command is passed to the dataset by a call to
  PSSetCommandText, and subsequently executed by a call to PSExecute.
  The value of the CommandText parameter replaces the dataset’s SQL statement (if it has one),
  or replaces the name of an underlying database table or stored procedure. }
procedure TRtcBaseDataSet.PSSetCommandText(const CommandText: string);
begin
  // ??
end;

{ PSSetParams assigns the parameter values specified by AParams to the parameters of the dataset. }
procedure TRtcBaseDataSet.PSSetParams(AParams: TParams);
begin
  // ??
end;

{ Before the provider generates SQL statements to apply an update, it calls PSUpdateRecord to let the dataset apply the update
  in some other fashion. For example, BDE-enabled datasets generate an OnUpdateRecord to allow the application to apply the update
  and, if the OnUpdateRecord event handler does not apply the update, uses any associated TUpdateSQL object to apply the update.

  UpdateKind indicates whether the update is an insertion (ukInsert), deletion (ukDelete), or modification (ukModify).
  Delta is the delta datapacket. The current record represents the record to be updated.
  PSUpdateRecord returns true if it updates the record, false otherwise.
  If PSUpdateRecord returns false, the provider generates dynamic SQL to perform the update and calls PSExecuteStatement.

  Note: PSUpdateRecord is only called if the provider’s ResolveToDataSet property is false. }
function TRtcBaseDataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
begin
  Result := False;
  // ??
end;

{$ENDIF}

{ TRtcMemDataSet }

{ This method is called by TDataSet.Open and also when FieldDefs need to
  be updated (usually by the DataSet designer).  Everything which is
  allocated or initialized in this method should also be freed or
  uninitialized in the InternalClose method. }
procedure TRtcMemDataSet.InternalOpen;
  begin
  if not assigned(FData) then
    if (FileName<>'') and File_Exists(RtcWideString(FileName)) then
      FData := TRtcDataSet.FromCode( RtcBytesToString(ZDecompress_Ex(Read_FileEx(RtcWideString(FileName)))) )
    else
      FData := TRtcDataSet.Create;

  FData.Row:=-1;

  { Tell TDataSet how big our Bookmarks are (REQUIRED) }
  BookmarkSize := SizeOf(Integer);

  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields(True);

  if FTrackChanges and not assigned(FCache) and not (csDesigning in ComponentState) then
    begin
    FCache:=TRtcDataSetRecorder.Create(self);
    FCache.OnChange:=DoCacheChange;
    end;
  end;

procedure TRtcMemDataSet.InternalClose;
  begin
  RtcFreeAndNil(FCache);

  { Write any edits to disk and free the managing String list }
  if (FileName<>'') and assigned(FData) then
    if FData.FieldCount>0 then
      Write_FileEx(RtcWideString(FileName), ZCompress_Ex( RtcStringToBytes(FData.toCode) ,zcDefault));

  RtcFreeAndNil(FData);

  if DefaultFields then
    DestroyFields;
  end;

{ This property is used while opening the dataset.
  It indicates if data is available even though the
  current state is still dsInActive. }
function TRtcMemDataSet.IsCursorOpen: Boolean;
  begin
  Result := Assigned(FData);
  end;

procedure TRtcMemDataSet.InternalInitFieldDefs;
  var
    a:integer;
    fname:RtcWideString;
    ftype:TFieldType;
    fsize:integer;
{  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd); }
  begin
  FieldDefs.Clear;
  if assigned(FData) then
    for a:=0 to FData.FieldCount-1 do
      begin
      fname:=FData.FieldName[a];
      fsize:=FData.FieldSize[fname];
      ftype:=RTC_FIELD2DB_TYPE(FData.FieldType[fname]);
      case ftype of
        ftString: if (fsize<1) or (fsize>8192) then fsize:=8192;
        ftBytes,ftVarBytes: if (fsize=0) then fsize:=8192;
        end;
      FieldDefs.Add(String(fname), ftype, fsize, FData.FieldRequired[fname]);
      end;
  end;

{ This is the exception handler which is called if an exception is raised
  while the component is being stream in or streamed out.  In most cases this
  should be implemented useing the application exception handler as follows. }
procedure TRtcMemDataSet.InternalHandleException;
  begin
   //Application.HandleException(Self);
  end;

{$IFDEF RTC_WIDECHAR}
procedure rtcStrLCopy(Dest: RtcPtrWideChar; const Source: RtcWideString; MaxLen:Cardinal);
  var
    Len,a:integer;
  begin
  Len:=length(Source);
  if Len>MaxLen then
    Len:=MaxLen;
  for a := 0 to Len-1 do
    Dest[a]:=Source[a+1];
  Dest[Len] := #0;
  end;

procedure rtcStrLCopy2(var Dest: RtcWideString; const Source: RtcPtrWideChar; MaxLen:Cardinal);
  var
    Len,a:integer;
  begin
  if Source=nil then
    Dest:=''
  else
    begin
    Len:=0;
    while Source[Len]<>#0 do
      begin
      Inc(Len);
      if Len>=MaxLen then Break;
      end;
    SetLength(Dest,Len);
    for a := 0 to Len-1 do
      Dest[a+1]:=Source[a];
    end;
  end;

procedure rtcStrLCopy3(Dest: RtcPtrWideChar; const Source: RtcPtrWideChar; MaxLen:Cardinal);
  var
    Len:integer;
  begin
  Len:=0;
  while Source[Len]<>#0 do
    begin
    Dest[Len]:=Source[Len];
    Inc(Len);
    if Len>=MaxLen then Break;
    end;
  Dest[Len] := #0;
  end;
{$ENDIF}

procedure TRtcMemDataSet.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);
  begin
  {$IFDEF RTC_WIDECHAR}
    if TRtcFieldTypes(Field.DataType) in [ft_WideString, ft_FixedWideChar, ft_WideMemo] then
      rtcStrLCopy3(RtcPtrWideChar(Dest),RtcPtrWideChar(Source),Field.Size)
    else
      inherited;
  {$ELSE}
    {$IFDEF RTC_ANSICHAR}
      if TRtcFieldTypes(Field.DataType) in [ft_WideString, ft_FixedWideChar, ft_WideMemo] then
        begin
        if ToNative then
          begin
          Word(Dest^) := Length(PWideString(Source)^)*2;
          Move(RtcPtrWideChar(Source^)^, (RtcPtrWideChar(Dest)+1)^, Word(Dest^));
          end
        else
          SetString(RtcWideString(Dest^), RtcPtrWideChar(RtcPtrAnsiChar(Source)+2), Word(Source^) div 2);
        end
      else
        inherited;
    {$ELSE}
      inherited;
    {$ENDIF}
  {$ENDIF}
  end;

{ Bookmarks }
{ ========= }

procedure TRtcMemDataSet.InternalGotoBookmark(Bookmark: Pointer);
  var
    Index: Integer;
  begin
  Index := PInteger(Bookmark)^;
  if Index <> -1 then
    FData.Row := Index
  else
    DatabaseError('Bookmark not found');
  end;

{ This function does the same thing as InternalGotoBookmark,
  but it takes a record buffer as a parameter instead }
procedure TRtcMemDataSet.InternalSetToRecord(Buffer: TRtcRecordBuffer);
  begin
  InternalGotoBookmark(@PRtcRecordInfo(Buffer)^.Bookmark);
  end;

{ Bookmark flags are used to indicate if a particular record is the first
  or last record in the dataset.  This is necessary for "crack" handling.
  If the bookmark flag is bfBOF or bfEOF then the bookmark is not actually
  used; InternalFirst, or InternalLast are called instead by TDataSet. }

function TRtcMemDataSet.GetBookmarkFlag(Buffer: TRtcRecordBuffer): TBookmarkFlag;
  begin
  Result := PRtcRecordInfo(Buffer)^.BookmarkFlag;
  end;

procedure TRtcMemDataSet.SetBookmarkFlag(Buffer: TRtcRecordBuffer; Value: TBookmarkFlag);
  begin
  PRtcRecordInfo(Buffer)^.BookmarkFlag := Value;
  end;

{ These methods provide a way to read and write bookmark data into the
  record buffer without actually repositioning the current record }

procedure TRtcMemDataSet.GetBookmarkData(Buffer: TRtcRecordBuffer; Data: Pointer);
  begin
  PInteger(Data)^ := PRtcRecordInfo(Buffer)^.Bookmark;
  end;

procedure TRtcMemDataSet.SetBookmarkData(Buffer: TRtcRecordBuffer; Data: Pointer);
  begin
  PRtcRecordInfo(Buffer)^.Bookmark := PInteger(Data)^;
  end;

{ Record / Field Access }
{ ===================== }

{ TDataSet calls this method to allocate the record buffer. }
function TRtcMemDataSet.AllocRecordBuffer: TRtcRecordBuffer;
  var
    obj:PRtcRecordInfo;
  begin
  New(obj);
  FillChar(obj^, SizeOf(obj^), 0);
  Result:=TRtcRecordBuffer(obj);
  end;

{ TDataSet calls this method to free the record buffer. }
procedure TRtcMemDataSet.FreeRecordBuffer(var Buffer: TRtcRecordBuffer);
  var
    obj:PRtcRecordInfo absolute Buffer;
  begin
  with obj^ do
    begin
    if assigned(Calc) then
      SetLength(Calc,0);
    if assigned(RowData) and not Linked then
      RowData.Free;
    end;
  Dispose(obj);
  Buffer:=nil;
  end;

{ This routine is called to initialize a record buffer.  In this sample,
  we fill the buffer with zero values, but we might have code to initialize
  default values or do other things as well. }
procedure TRtcMemDataSet.InternalInitRecord(Buffer: TRtcRecordBuffer);
  var
    obj:PRtcRecordInfo absolute Buffer;
  begin
  with obj^ do
    begin
    if CalcFieldsSize>0 then
      begin
      if not assigned(Calc) then
        SetLength(Calc, CalcFieldsSize);
      FillChar(Calc[0], CalcFieldsSize, #0);
      end;
    if assigned(RowData) and not Linked then
      RowData.Free;
    end;
  FillChar(obj^, SizeOf(obj^), 0);
  end;

{ This multi-purpose function does 3 jobs.  It retrieves data for either
  the current, the prior, or the next record.  It must return the status
  (TGetResult), and raise an exception if DoCheck is True. }
function TRtcMemDataSet.GetRecord(Buffer: TRtcRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
  var
    accepted:boolean;
    SaveState: TDataSetState;
  begin
  repeat
    accepted:=True;
    if FData.RowCount < 1 then
      Result := grEOF
    else
      begin
      Result := grOK;
      case GetMode of
        gmNext:
          begin
          FData.Next;
          if FData.Eof then
            Result := grEOF;
          end;
        gmPrior:
          begin
          FData.Prior;
          if FData.Bof then
            Result := grBOF;
          end;
        gmCurrent:
          if (FData.Row<0) or (FData.Row>=FData.RowCount) then
            Result := grError;
        end;
      if Result = grOK then
        begin
        with PRtcRecordInfo(Buffer)^ do
          begin
          if assigned(RowData) and not Linked then
            RowData.Free;
          RowData:=FData.RowData;
          Linked:=True;
          BookmarkFlag := bfCurrent;
          Bookmark := FData.Row;
          end;
        if Filtered and assigned(OnFilterRecord) then
          begin
          SaveState := SetTempState(dsFilter);
          try
            OnFilterRecord(Self, accepted);
          finally
            RestoreState(SaveState);
            end;
          end;
        if accepted then
          begin
          if CalcFieldsSize>0 then
            begin
            ClearCalcFields(Buffer);
            GetCalcFields(Buffer);
            end;
          end
        else if GetMode=gmCurrent then
          begin
          Result:=grError;
          Break;
          end;
        end
      else if (Result = grError) and DoCheck then
        DatabaseError('No Records');
      end;
    until accepted or (Result in [grEOF, grBOF]);
  end;

procedure TRtcMemDataSet.ClearCalcFields(Buffer: TRtcRecordBuffer);
  begin
  with PRtcRecordInfo(Buffer)^ do
    if CalcFieldsSize>0 then
      begin
      if not assigned(Calc) then
        SetLength(Calc, CalcFieldsSize);
      FillChar(Calc[0], CalcFieldsSize, #0);
      end;
  end;

function TRtcMemDataSet.GetActiveRecordBuffer: TRtcRecordBuffer;
  begin
  case State of
    dsBrowse: if isEmpty then Result := nil
              else Result := TRtcRecordBuffer(ActiveBuffer);

    dsEdit, dsInsert: Result := TRtcRecordBuffer(ActiveBuffer);

    dsCalcFields,dsInternalCalc: Result := TRtcRecordBuffer(CalcBuffer);

    dsFilter: Result:=TRtcRecordBuffer(TempBuffer); // Buffer used in GetRecord when Filtering

  { dsSetKey: RecBuf := RtcPtrAnsiChar(FKeyBuffer) + SizeOf(TKeyBuffer);
    dsFilter: RecBuf := FFilterBuffer;
    dsNewValue: RecBuf := FNewValueBuffer;
    dsOldValue: if FOldValueBuffer <> nil then
                  RecBuf := FOldValueBuffer else
                  RecBuf := TempBuffer;
    dsCurValue: RecBuf := FCurValueBuffer; }
    else Result := nil;
    end;
  end;

{ Here we copy the data from the record buffer into a field's buffer.
  This function, and SetFieldData, are more complex when supporting
  calculated fields, filters, and other more advanced features.
  See TBDEDataSet for a more complete example. }
{$IFDEF IDE_XE4up}
function TRtcMemDataSet.GetFieldData(Field: TField; var Buffer: TRtcValueBuffer): Boolean;
{$ELSE}
function TRtcMemDataSet.GetFieldData(Field: TField; Buffer: TRtcValueBuffer): Boolean;
{$ENDIF}
  var
    from_b:PRtcRecordInfo;
    idx:integer;
    myDateTime: TDateTime;
    Buff:Pointer;
  {$IFDEF RTC_NOUNICODE}
    MyWideStr:RtcWideString;
  {$ENDIF}
  begin
  Result:=False;

  {
  if GetActiveRecBuf(RecBuf) then
    with Field do
      if FieldKind in [fkData, fkInternalCalc] then
      begin
        Check(FDSCursor.GetField(RecBuf, FieldNo, Buffer, IsBlank));
        Result := not IsBlank;
      end else
        if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
        begin
          Inc(RecBuf, FRecordSize + Offset);
          Result := Boolean(RecBuf[0]);
          if Result and (Buffer <> nil) then
            Move(RecBuf[1], Buffer^, DataSize);
        end;
  }

  from_b:=PRtcRecordInfo(GetActiveRecordBuffer);
  if from_b = nil then
    Exit;

  if Field.FieldKind in [fkData,fkInternalCalc] then
    begin
    idx:=Field.FieldNo-1; // FData.FieldIndex[Field.FieldName];
    if assigned(from_b.RowData) and assigned(from_b.RowData.asObject[idx]) then
      begin
    {$IFDEF IDE_XE3up}
      if length(Buffer)>0 then
        Buff:=Addr(Buffer[0])
      else
        Buff:=nil;
    {$ELSE}
      Buff:=Buffer;
    {$ENDIF}

      if Buff<>nil then
        begin
        case TRtcFieldTypes(Field.DataType) of
          ft_Integer,
          ft_SmallInt,
          ft_Word,
          ft_AutoInc,
          ft_ShortInt,
          ft_Byte:
            LongInt(Buff^):=from_b.RowData.asInteger[idx];
          ft_Largeint:
            LargeInt(Buff^):=from_b.RowData.asLargeInt[idx];
          ft_LongWord:
            Cardinal(Buff^):=from_b.RowData.asCardinal[idx];
          ft_Boolean:
            WordBool(Buff^):=from_b.RowData.asBoolean[idx];
          ft_Currency,
          ft_Float:
            Double(Buff^):=from_b.RowData.asFloat[idx];
          ft_Extended:
            Extended(Buff^):=from_b.RowData.asFloat[idx];
          ft_Single:
            Single(Buff^):=from_b.RowData.asFloat[idx];
          ft_String,
          ft_FixedChar,
          ft_GUID,
          ft_Memo,
          ft_OraClob,
          ft_OraInterval:
            RtcStringToPBytesZero(from_b.RowData.asString[idx],Buff^,1,length(from_b.RowData.asString[idx]));
          ft_Date,
          ft_Time,
          ft_DateTime:
            begin
            MyDateTime:=from_b.RowData.asDateTime[idx];
            DataConvert(Field,@MyDateTime,Buff,True);
            end;
          ft_BCD:
            begin
            if not CurrToBCD(from_b.RowData.asCurrency[idx],TBcd(Buff^)) then
              raise ERtcDB.Create('Wrong BCD format.');
            end;
          ft_WideString,
          ft_FixedWideChar,
          ft_WideMemo:
            begin
            {$IFDEF RTC_UNICODE}
              StrPCopy(Buff, from_b.RowData.asText[idx]);
            {$ELSE}
              {$IFDEF RTC_WIDECHAR}
                MyWideStr:=from_b.RowData.asWideString[idx];
                rtcStrLCopy(RtcPtrWideChar(Buff), MyWideStr, Field.Size);
			        {$ELSE}
                {$IFDEF RTC_ANSICHAR}
                  MyWideStr:=from_b.RowData.asWideString[idx];
                  DataConvert(Field,@MyWideStr,Buff,True);
                {$ELSE}
                  StrPCopy(Buff, from_b.RowData.asWideString[idx]);
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
            end;
          ft_Variant:
            Variant(Buff^):=from_b.RowData.asValue[idx];
          {$IFNDEF FPC}
          ft_TimeStamp,
          ft_OraTimeStamp:
            TSQLTimeStamp(Buff^):=DateTimeToSQLTimeStamp(from_b.RowData.asDateTime[idx]);
          {$ENDIF}
          {$IFDEF IDE_XEup}
          ft_TimeStampOffset:
            TSQLTimeStampOffset(Buff^):=DateTimeToSQLTimeStampOffset(from_b.RowData.asDateTime[idx]);
          {$ENDIF}
          ft_FmtBCD:
            begin
            if not CurrToBCD(from_b.RowData.asCurrency[idx],TBcd(Buff^)) then
              raise ERtcDB.Create('Wrong BCD format.');
            end;
          else
            raise ERtcDB.Create('Unsupported Field Type');
          end;
        end;
      Result:=True;
      end;
    end
  else if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
    begin
    if assigned(from_b.Calc) then
      begin
      Result := from_b.Calc[Field.Offset]<>0;
    {$IFDEF IDE_XE3up}
      if Result and (length(Buffer)>=Field.DataSize) then
        Move(from_b.Calc[Field.Offset+1], Buffer[0], Field.DataSize);
    {$ELSE}
      if Result and (Buffer <> nil) then
        Move(from_b.Calc[Field.Offset+1], Buffer^, Field.DataSize);
    {$ENDIF}
      end;
    end;
  end;

procedure TRtcMemDataSet.SetFieldData(Field: TField; Buffer: TRtcValueBuffer);
  var
    to_b:PRtcRecordInfo;
    idx:integer;
  {$IFDEF RTC_NOUNICODE}
    myWideStr:RtcWideString;
  {$ENDIF}
    myCurr:Currency;
    myDateTime: TDateTime;
    Buff:Pointer;
  begin
  to_b:=PRtcRecordInfo(GetActiveRecordBuffer);
  if to_b=nil then
    Exit;

  if Field.FieldKind in [fkData,fkInternalCalc] then
    begin
    if (State=dsEdit) and not assigned(FOrigData) then
      if assigned(to_b.RowData) then
        FOrigData:=TRtcArray(to_b.RowData.copyOf)
      else
        FOrigData:=TRtcArray.Create;

    idx:=Field.FieldNo-1; // FData.FieldIndex[Field.FieldName];

  {$IFDEF IDE_XE3up}
    if length(Buffer)>0 then
      Buff:=Addr(Buffer[0])
    else
      Buff:=nil;
  {$ELSE}
    Buff:=Buffer;
  {$ENDIF}

    if Buff=nil then
      begin
      if assigned(to_b.RowData) then
        to_b.RowData.isNull[idx]:=True;
      end
    else
      begin
      if not assigned(to_b.RowData) then
        to_b.RowData:=TRtcArray.Create;

      case TRtcFieldTypes(Field.DataType) of

(*
    TBytesField,               { ftBytes }
    TVarBytesField,            { ftVarBytes }

    TBlobField,                { ftBlob }
    TGraphicField,             { ftGraphic }
    TBlobField,                { ftFmtMemo }
    TBlobField,                { ftParadoxOle }
    TBlobField,                { ftDBaseOle }
    TBlobField,                { ftTypedBinary }
    TBlobField,                { ftOraBlob }

    TADTField,                 { ftADT }
    TArrayField,               { ftArray }
    TReferenceField,           { ftReference }
    TDataSetField,             { ftDataSet }
    TInterfaceField,           { ftInterface }
    TIDispatchField,           { ftIDispatch }
*)

        ft_Integer,
        ft_SmallInt,
        ft_Word,
        ft_AutoInc,
        ft_ShortInt,
        ft_Byte:
          // TIntegerField
          to_b.RowData.asInteger[idx]:=Integer(Buff^);
        ft_Largeint:
          // TLargeIntField
          to_b.RowData.asLargeInt[idx]:=LargeInt(Buff^);
        ft_Boolean:
          // TBooleanField
          to_b.RowData.asBoolean[idx]:=WordBool(Buff^);
        ft_String,
        ft_FixedChar,
        ft_GUID,
        ft_Memo,
        ft_OraClob,
        ft_OraInterval:
          // TStringField
          to_b.RowData.asString[idx]:=RtcPBytesToString( Buff^, StrLen(RtcPtrAnsiChar(Buff)) );
        ft_Currency,
        ft_Float:
          // TFloatField
          to_b.RowData.asFloat[idx]:=Double(Buff^);
        ft_LongWord:
          // TLongWordField
          to_b.RowData.asCardinal[idx]:=Cardinal(Buff^);
        ft_Single:
          // TSingleField
          to_b.RowData.asFloat[idx]:=Single(Buff^);
        ft_Extended:
          // TExtendedField
          to_b.RowData.asFloat[idx]:=Extended(Buff^);
        ft_Date,
        ft_Time,
        ft_DateTime:
          // TDataTimeField
          begin
          DataConvert(Field,Buff,@MyDateTime,False);
          to_b.RowData.asDateTime[idx]:=MyDateTime;
          end;
        ft_BCD:
          // TBCDField
          begin
          if BCDToCurr(TBcd(Buff^),myCurr) then
            to_b.RowData.asCurrency[idx]:=MyCurr
          else
            raise ERtcDB.Create('Wrong BCD format.');
          end;
        ft_WideString,
        ft_FixedWideChar,
        ft_WideMemo:
          // TWideStringField
          begin
          {$IFDEF RTC_UNICODE}
            to_b.RowData.asText[idx]:=RtcWideString(RtcPtrWideChar(Buff));
          {$ELSE}
            {$IFDEF RTC_WIDECHAR}
              rtcStrLCopy2(MyWideStr,RtcPtrWideChar(Buff),Field.Size);
              to_b.RowData.asWideString[idx]:=MyWideStr;
            {$ELSE}
              {$IFDEF RTC_ANSICHAR}
                DataConvert(Field,Buff,@MyWideStr,False);
                to_b.RowData.asWideString[idx]:=MyWideStr;
              {$ELSE}
                to_b.RowData.asWideString[idx]:=RtcWideString(RtcPtrWideChar(Buff));
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
          end;
        ft_Variant:
          // TVariantField
          to_b.RowData.asValue[idx]:=Variant(Buff^);
        {$IFNDEF FPC}
        ft_TimeStamp,
        ft_OraTimeStamp:
          // TSQLTimeStampField
          to_b.RowData.asDateTime[idx]:=SQLTimeStampToDateTime(TSQLTimeStamp(Buff^));
        {$ENDIF}
        {$IFDEF IDE_XEup}
        ft_TimeStampOffset:
          // TSQLTimeStampOffsetField
          to_b.RowData.asDateTime[idx]:=SQLTimeStampOffsetToDateTime(TSQLTimeStampOffset(Buff^));
        {$ENDIF}
        ft_FMTBcd:
          // TFmtBCDField
          begin
          if BCDToCurr(TBcd(Buff^),myCurr) then
            to_b.RowData.asCurrency[idx]:=MyCurr
          else
            raise ERtcDB.Create('Wrong BCD format.');
          end;
        else
          raise ERtcDB.Create('Unsupported Data Type.');
        end;
      end;
    end
  else
    begin
    if State=dsInternalCalc then Exit;

    if CalcFieldsSize=0 then Exit;

    if not assigned(to_b.Calc) then
      begin
      SetLength(to_b.Calc, CalcFieldsSize);
      FillChar(to_b.Calc[0], CalcFieldsSize, #0);
      end;

  {$IFDEF IDE_XE3up}
    if length(Buffer)>0 then
      to_b.Calc[Field.Offset]:=1
    else
      to_b.Calc[Field.Offset]:=0;
    if (length(Buffer)>0) and (length(Buffer)>=Field.DataSize) then
      Move(Buffer[0], to_b.Calc[Field.Offset+1], Field.DataSize);
  {$ELSE}
    if assigned(Buffer) then
      to_b.Calc[Field.Offset]:=1
    else
      to_b.Calc[Field.Offset]:=0;
    if assigned(Buffer) then
      Move(Buffer^, to_b.Calc[Field.Offset+1], Field.DataSize);
  {$ENDIF}
    end;

  if not (State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, TRtcNativeInt(Field));
  end;

{ Record Navigation / Editing }
{ =========================== }

{ This method is called by TDataSet.First.  Crack behavior is required.
  That is we must position to a special place *before* the first record.
  Otherwise, we will actually end up on the second record after Resync
  is called. }

procedure TRtcMemDataSet.InternalFirst;
  begin
  if not assigned(FData) then Exit;

  FData.Row := -1;
  end;

{ Again, we position to the crack *after* the last record here. }

procedure TRtcMemDataSet.InternalLast;
  begin
  if not assigned(FData) then Exit;

  FData.Row := FData.RowCount;
  end;

{ This method is called by TDataSet.Post.  Most implmentations would write
  the changes directly to the associated datasource, but here we simply set
  a flag to write the changes when we close the dateset. }

procedure TRtcMemDataSet.InternalPost;
  begin
  if assigned(FOrigData) then
    begin
    FOrigData.Free;
    FOrigData:=nil;
    end;
  // When Editing existing record, we directly change the original data.
  // This means that we don't have to "copy" new values after Post.
  if State = dsInsert then
    begin
    FData.Insert;
    with PRtcRecordInfo(ActiveBuffer)^ do
      begin
      Linked := True;
      Bookmark := FData.Row;
      FData.RowData := RowData;
      end;
    end;
  end;

{ This method is similar to InternalPost above, but the operation is always
  an insert or append and takes a pointer to a record buffer as well. }

procedure TRtcMemDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
  begin
  if assigned(FOrigData) then
    begin
    FOrigData.Free;
    FOrigData:=nil;
    end;
  if Append then
    FData.Append
  else
    FData.Insert;
  with PRtcRecordInfo(Buffer)^ do
    begin
    Linked := True;
    Bookmark := FData.Row;
    FData.RowData := RowData;
    end;
  end;

procedure TRtcMemDataSet.DoBeforeCancel;
  begin
  inherited;
  if assigned(FOrigData) then
    begin
    // When Editing a record, we directly change the original data.
    // This means that we HAVE TO replace the record with Original Values if editing was Canceled.
    if State=dsEdit then
      begin
      FData.RowData:=FOrigData;
      with PRtcRecordInfo(ActiveBuffer)^ do
        begin
        if not Linked and assigned(RowData) then
          RowData.Free;
        RowData:=FData.RowData;
        Bookmark:=FData.Row;
        Linked:=True;
        end;
      end
    else
      FOrigData.Free;
    FOrigData:=nil;
    end;
  end;

{ This method is called by TDataSet.Delete to delete the current record }

procedure TRtcMemDataSet.InternalDelete;
  begin
  if assigned(FOrigData) then
    begin
    FOrigData.Free;
    FOrigData:=nil;
    end;
  FData.Delete;
  end;

{ Optional Methods }
{ ================ }

{ The following methods are optional.  When provided they will allow the
  DBGrid and other data aware controls to track the current cursor postion
  relative to the number of records in the dataset.  }

function TRtcMemDataSet.GetRecordCount: Longint;
  begin
  if not assigned(FData) then
    Result:=0
  else
    Result := FData.RowCount;
  end;

function TRtcMemDataSet.GetRecNo: Longint;
  begin
  if not assigned(FData) then
    Result:=0
  else
    begin
    UpdateCursorPos;
    if (FData.Row = -1) and (RecordCount > 0) then
      Result := 1
    else
      Result := FData.Row + 1;
    end;
  end;

procedure TRtcMemDataSet.SetRecNo(Value: Integer);
  begin
  if not assigned(FData) then Exit;
  if (Value >= 0) and (Value <= FData.RowCount) then
    begin
    CheckBrowseMode;
    DoBeforeScroll;
    FData.Row := Value - 1;
    Resync([]);
    DoAfterScroll;
    end;
  end;

function TRtcMemDataSet.GetDataSet: TRtcDataSet;
  begin
  Result:=FData;
  end;

procedure TRtcMemDataSet.SetDataSet(const Value: TRtcDataSet);
  begin
  if Value<>FData then
    begin
    if assigned(Value) then
      begin
      if assigned(FData) then
        begin
        if Active then
          Active:=False
        else
          RtcFreeAndNil(FData);
        end;
      FData:=Value;
      end
    else
      begin
      FData:=TRtcDataSet.Create;
      Active:=False;
      end;
    if TrackChanges then
      begin
      TrackChanges:=False;
      TrackChanges:=True;
      end;
    end;
  end;

procedure TRtcMemDataSet.SetDataSetCopy(const Value: TRtcDataSet);
  begin
  if Value<>FData then
    begin
    if assigned(FData) then
      begin
      if Active then
        Active:=False
      else
        RtcFreeAndNil(FData);
      end;
    if assigned(Value) then
      FData:=TRtcDataSet(Value.copyOf);
    if TrackChanges then
      begin
      TrackChanges:=False;
      TrackChanges:=True;
      end;
    end;
  end;

procedure TRtcMemDataSet.SetFileName(const Value: String);
  begin
  FFileName := Value;
  end;

destructor TRtcMemDataSet.Destroy;
  begin
  if not Active then
    RtcFreeAndNil(FData);
  inherited;
  end;

function TRtcMemDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
  begin
  if (Field<>nil) and (Field.IsBlob) then
    Result:=TRtcBlobStream.Create(Field as TBlobField,Mode)
  else
    Result:=nil;
  end;

procedure TRtcMemDataSet.DoBeforeDelete;
  begin
  if assigned(FCache) then FCache.BeforeChange;
  inherited;
  end;

procedure TRtcMemDataSet.DoBeforeEdit;
  begin
  if assigned(FCache) then FCache.BeforeChange;
  inherited;
  end;

procedure TRtcMemDataSet.DoAfterCancel;
  begin
  if assigned(FCache) then FCache.CancelChange;
  inherited;
  end;

procedure TRtcMemDataSet.DoBeforeInsert;
  begin
  if assigned(FCache) then FCache.CancelChange;
  inherited;
  end;

procedure TRtcMemDataSet.DoAfterDelete;
  begin
  if assigned(FCache) then FCache.AfterChange;
  inherited;
  end;

procedure TRtcMemDataSet.DoAfterPost;
  begin
  if assigned(FCache) then FCache.AfterChange;
  inherited;
  end;

function TRtcMemDataSet.ExtractChanges: TRtcValue;
  begin
  if assigned(FCache) then
    Result:=FCache.ExtractChanges
  else
    Result:=nil;
  end;

procedure TRtcMemDataSet.SetTrackChanges(const Value: boolean);
  begin
  if Value<>FTrackChanges then
    begin
    if not (csDesigning in ComponentState) then
      begin
      RtcFreeAndNil(FCache);
      if Value and Active then
        begin
        FCache:=TRtcDataSetRecorder.Create(self);
        FCache.OnChange:=DoCacheChange;
        end;
      end;
    FTrackChanges := Value;
    end;
  end;

procedure TRtcMemDataSet.DoCacheChange(Sender: TObject);
  begin
  if assigned(FOnDataChange) then
    FOnDataChange(self);
  end;

{ TRtcBlobStream }

constructor TRtcBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
  var
    from_b : PRtcRecordInfo;
    idx: Integer;
  begin
  inherited Create;
  FStream := nil;
  FDataset := Field.DataSet as TRtcMemDataSet;
  from_b:=PRtcRecordInfo(FDataSet.GetActiveRecordBuffer);
  if from_b <> nil then
    begin
    idx:=Field.FieldNo-1; // FData.FieldIndex[Field.FieldName];
    if assigned(from_b.RowData) then
      begin
      FStream:=from_b.RowData.asByteStream[idx];
      if not assigned(FStream) then
        begin
        if Mode<>bmRead then
          begin
          FStream:=from_b.RowData.newByteStream(idx);
          FDataSet.SetModified(True);
          end;
        end
      else if Mode=bmWrite then
        begin
        FStream.Size:=0;
        FDataSet.SetModified(True);
        end
      else
        FStream.Position:=0;
      end;
    end;
  end;

destructor TRtcBlobStream.Destroy;
  begin
  FStream:=nil;
  FDataSet:=nil;
  inherited;
  end;

{$IFDEF IDE_7up}
function TRtcBlobStream.GetSize: Int64;
  begin
  if assigned(FStream) then
    Result:=FStream.Size
  else
    Result:=0;
  end;
{$ENDIF}

function TRtcBlobStream.Read(var Buffer; Count: Integer): Longint;
  begin
  if assigned(FStream) then
    Result:=FStream.Read(Buffer,Count)
  else
    Result:=0;
  end;

function TRtcBlobStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
  begin
  if assigned(FStream) then
    Result:=FStream.Seek(Offset,Origin)
  else
    Result:=0;
  end;

function TRtcBlobStream.Seek(Offset: Integer; Origin: Word): Longint;
  begin
  if assigned(FStream) then
    Result:=FStream.Seek(Offset,Origin)
  else
    Result:=0;
  end;

procedure TRtcBlobStream.SetSize(NewSize: Integer);
  begin
  if assigned(FStream) then
    if NewSize<>FStream.Size then
      begin
      FStream.Size:=NewSize;
      FDataSet.SetModified(True);
      end;
  end;

procedure TRtcBlobStream.SetSize(const NewSize: Int64);
  begin
  if assigned(FStream) then
    if NewSize<>FStream.Size then
      begin
      FStream.Size:=NewSize;
      FDataSet.SetModified(True);
      end;
  end;

function TRtcBlobStream.Write(const Buffer; Count: Integer): Longint;
  begin
  if assigned(FStream) and (Count>0) then
    begin
    Result := FStream.Write(Buffer,Count);
    FDataSet.SetModified(True);
    end
  else
    Result:=0;
  end;

{$ENDIF}

initialization
{$IFDEF NEXTGEN}
  GetRTCFieldType:=@RTC_DB2FIELD_TYPE;
{$ELSE}
  @GetRTCFieldType:=@RTC_DB2FIELD_TYPE;
{$ENDIF}
finalization
end.
