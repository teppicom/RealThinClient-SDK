{
  @html(<b>)
  System utilities
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  Global System Parameters, Classes and Functions:
  Unicode<->ANSI support, File & Folder Access, Tick Timer, Critical Sections, Array & String manipulation.
}
unit rtcSystem;

{$INCLUDE rtcDefs.inc}

interface

{.$DEFINE RTC_WIN32_CS_DEBUG}

{ Declaring "RTC_TICKTIME_DEBUG" forces the internal "Tick Timer" to
  overflow once every 131 seconds instead of only once every 49 days.
  This is used for debugging RTC Timers, RTC Thread Synchronization
  and "GetTickTime/64" functions. Unless you are debugging these RTC
  functions, do NOT declare this compiler define for your Project! }
{.$DEFINE RTC_TICKTIME_DEBUG}

uses
  {$IFDEF WINDOWS}
    Windows, // GetTickCount() and file access API
  {$ELSE}
    {$IFDEF POSIX}
      {$IFDEF MACOSX}
        Macapi.CoreServices, // used for getTickTime
      {$ELSE}
        Posix.SysTime,
      {$ENDIF}
      Posix.Pthread,
      Posix.Unistd,
      Posix.Stdio,
    {$ELSE}
      {$IFDEF FPC}
        BaseUnix, // used for GetTickTime
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF RTC_FMI}
    FMX_Types, {$DEFINE RTC_FMXOBJECT}
  {$ELSE}
    {$IFDEF RTC_FMX}
      FMX.Types, {$DEFINE RTC_FMXOBJECT}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF IDE_1}
  FileCtrl,
  {$ENDIF}

  SysUtils,
  SyncObjs,
  Classes,

  rtcTypes,
  rtcSrcList;

var
  // Full Name of the application or extension in which RTC is running.
  AppFileName:RtcWideString='';

  // Default Packet Size used by Socket APIs
  SOCK_PACKET_SIZE:integer=1460;

  // Buffer used by Socket APIs to buffer outgoing data
  SOCK_SEND_BUFFER_SIZE:integer=1460*44;
  // Buffer used by Socket APIs to buffer incoming data
  SOCK_READ_BUFFER_SIZE:integer=1460*44;

  // Max packet size sent out at once
  SOCK_MAX_SEND_SIZE:integer=1460*44;
  // Max packet size read at once
  SOCK_MAX_READ_SIZE:integer=1460*44;

  // Maximum socket Receive Timeout in miliseconds
  SOCK_RECV_TIMEOUT:integer=1000*60*30; // 30 minutes timeout (silly number here, since we want time unlimited)
  // Maximum socket Receive Timeout in miliseconds
  SOCK_SEND_TIMEOUT:integer=1000*60*30; // 30 minutes timeout (ridicilous number, since we want time unlimited)

  // Listenin Socket "backlog" (max. connections waiting to be accepted)
  SOCK_LISTEN_BACKLOG:integer=200;

  { RTC_WAIT_BEFORE_RECONNECT sets the Sleep period (here in miliseconds) for Reconnect.
    @html(<br>)
    This parameter will be used to force a Sleep() before a new reconnect,
    independent of the "OnReconnect.Wait" parameter. }
  RTC_WAIT_BEFORE_RECONNECT:integer=20;

  { RTC_WAIT_BEFORE_RESTART sets the Sleep period (here in miliseconds) for Restart.
    @html(<br>)
    This parameter will only be used if RestartOn.Wait<=0. }
  RTC_WAIT_BEFORE_RESTART:integer=100;

  { RTC_WAITFOR_RELEASE_PROVIDER is the maximum time (in seconds) allowed for any
    connection component to wait for its connection provider to be released. }
  RTC_WAITFOR_RELEASE_PROVIDER:integer=15;

  { Time to wait (in milliseconds) after releasing a connection provider for background thread to finish. }
  RTC_WAITFOR_PROVIDER_FINISH:integer=50;

  { Default number of times to automatically retry
    "Write_File" and "Write_FileEx" operations if they fail. }
  RTC_WRITE_RETRIES:integer=0;

  { Default number of milliseconds to wait between each
    retry attempt in "Write_File" and "Write_FileEx" functions. }
  RTC_WRITE_RETRY_DELAY:integer=100;

  { Default Folder used when creating Temporary files ("GetTempFile" function).
    If empty (not specified), the '/TEMP' folder is acquired from the OS. }
  RTC_DEFAULT_TEMP_FOLDER:RtcWideString='';

const
  {$IFDEF IDE_XE2up}
    RTC_INVALID_FILE_HDL = INVALID_HANDLE_VALUE;
  {$ELSE}
    RTC_INVALID_FILE_HDL = -1;
  {$ENDIF}

type
  { All RTC components use at least one class declared in this unit.
    For Delphi to add this unit to uses clause, all RTC components inherit from this class.
    @exclude }
  {$IFDEF RTC_FMXOBJECT}
    T_Rtc_Component = class(TFmxObject);
  {$ELSE}
    T_Rtc_Component = class(TComponent);
  {$ENDIF}

  {$IFDEF IDE_XE2up}
    TRtcFileHdl=THandle;
  {$ELSE}
    TRtcFileHdl=integer;
  {$ENDIF}

  // RTC System exception
  ERtcSystem = class(Exception);

  // Silent socket error (no visible exception while debugging)
  ERtcSocketError      = class(EAbort);

  // Fatal socket error (visible exception will be raised)
  ERtcFatalSockException = class(Exception);

  // File Access Mode
  TRtcFileAccessMode= (// Allow other apps to read and write to the same file
                       rtc_ShareDenyNone,
                       // Allow other apps to read, but not to write to the file
                       rtc_ShareDenyWrite,
                       // Deny all access to the file - exclusive access mode!
                       rtc_ShareExclusive);

  { @abstract(RTC File Stream implementation) }
  TRtcFileStream = class(TObject)
    private
      f:TRtcFileHdl;
      l:int64;

    public
      destructor Destroy; override;

      procedure Open(const fname:RtcWideString);
      procedure Seek(loc:int64);

      function ReadEx(size:int64):RtcByteArray;
      function Read(size:int64):RtcString;

      procedure Close;
    end;

  { @abstract(RTC Byte Array Stream implementation) }
  TRtcByteArrayStream = class(TStream)
  private
    FData: RtcByteArray;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const AData: RtcByteArray);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function ReadBytes(Count: Longint): RtcByteArray;
    procedure WriteBytes(const AData: RtcByteArray);
    property GetBytes: RtcByteArray read FData;
  end;

  { @abstract(RTC "TPersistent" class) }
  TRtcPersistent = class(TPersistent);

  TRtcTimeoutsOfAPI = class(TRtcPersistent)
  private
    fResolveTimeout:Integer;
    fConnectTimeout:Integer;
    fSendTimeout:Integer;
    fReceiveTimeout:Integer;
    fResponseTimeout:Integer;

  public
    { @exclude }
    procedure AssignTo(Dest:TPersistent); override;

  published
    { A value of type integer that specifies the time-out value, in seconds, to use for name
      resolution. If resolution takes longer than this time-out value, the action is canceled. @html(<br>)
      Applies to WinHTTP
      Set to zero (0) to use the operating system default. }
    property ResolveTimeout:Integer read fResolveTimeout write fResolveTimeout default 0;
    { A value of type integer that specifies the time-out value, in seconds, to use for
      server connection requests. If a connection request takes longer than this time-out value,
      the request is canceled. @html(<br>)
      Applies to WinINET and WinHTTP @html(<br>)
      Set to zero (0) to use the operating system default. }
    property ConnectTimeout:Integer read fConnectTimeout write fConnectTimeout default 0;
    { A value of type integer that specifies the time-out value, in seconds, to use for sending
      requests. If sending a request takes longer than this time-out value, the send is canceled. @html(<br>)
      Applies to WinINET and WinHTTP and WinSock @html(<br>)
      Set to zero (0) to use the operating system default. }
    property SendTimeout:Integer read fSendTimeout write fSendTimeout default 0;
    { A value of type integer that specifies the time-out value, in seconds, to receive a
      response to a request. If a response takes longer than this time-out value, the request
      is canceled. @html(<br>)
      Applies to WinINET and WinHTTP and WinSock @html(<br>)
      Set to zero (0) to use the operating system default. }
    property ReceiveTimeout:Integer read fReceiveTimeout write fReceiveTimeout default 0;
    { A value of type integer that specifies the time-out value, in seconds, to wait to receive
      all response headers to a request. If a response takes longer than this time-out value,
      the request is canceled. @html(<br>)
      Applies to WinHTTP @html(<br>)
      Set to zero (0) to use the operating system default. }
    property ResponseTimeout:Integer read fResponseTimeout write fResponseTimeout default 0;
  end;

  // @exclude
  PRtcByte = ^Byte;

  // @exclude
  RtcIntPtr = rtcTypes.RtcIntPtr;

  // Unicode String
  RtcWideString = rtcTypes.RtcWideString;
  // Unicode character
  RtcWideChar = rtcTypes.RtcWideChar;

  // 8-bit Character String (NOT UNICODE!)
  RtcString = rtcTypes.RtcString;
  // 8-bit Character (NOT UNICODE!)
  RtcChar = rtcTypes.RtcChar;

  // @exclude
  RtcPtrAnsiChar = rtcTypes.RtcPtrAnsiChar;
  // @exclude
  RtcPtrWideChar = rtcTypes.RtcPtrWideChar;

  // Array of Bytes
  RtcByteArray = rtcTypes.RtcByteArray;

type
  // "Fix" Types for RTC_STRING_FIXMODE
  TRtcStrFixType = (
    //Do NOT modify RtcString data when converting to/from RtcByteArray
    rtcStr_NoFIX,
    // Replace Unicode characters above #255 with ANSI when converting RtcString to RtcByteArray
    rtcStr_FixDown,
    // rtcStr_FixDown option + Replace ANSI characters with Unicode when converting RtcByteArray to RtcString
    rtcStr_FixUpDown
    );

var
  // RtcString "fix" mode (rtcStr_NoFix, rtcStr_FixDown, rtcStr_FixUpDown)
  RTC_STRING_FIXMODE:TRtcStrFixType=rtcStr_FixDown;

  // Raise an exception if conversion from RtcString to RtcByteArray would result in data loss
  RTC_STRING_CHECK:boolean=False;

  // Character to be used as a replacement for all Unicode characters not in the current ANSI codepage
  RTC_INVALID_CHAR:byte=63;

const
  // @exclude
  RTC_STROBJ_SHIFT = 4; // = 16
  // @exclude
  RTC_STROBJ_PACK = 1 shl RTC_STROBJ_SHIFT;
  // @exclude
  RTC_STROBJ_AND = RTC_STROBJ_PACK-1;

const
  WAIT_INFINITE = Cardinal($FFFFFFFF);     { Infinite timeout }
  wr_Signaled=wrSignaled;
  wr_Timeout=wrTimeout;
  wr_Abandoned=wrAbandoned;
  wr_Error=wrError;

type
  TRtcWaitResult = TWaitResult;

  TRtcEvent=class(TEvent)
  public
    // If ManualReset=False, a successful call to WaitFor will automatically Reset the event
    // If InitialState=True, event will be Set at start. If InitialState=False, event will be Reset at start.
    constructor Create(ManualReset,InitialState:boolean);
    end;

  TRtcCritSec=class(TCriticalSection)
  {$IFDEF RTC_WIN32_CS_DEBUG}procedure Acquire; override; end{$ENDIF};

  TRtcRWSec=class
  private
    WriteSec:TRtcEvent;
    PassSec,ReadSec:TRtcCritSec;
    Cnt,Cnt3:longint;
  public
    constructor Create;
    destructor Destroy; override;

    procedure EnterRead;  // Normal reader, no hurry.
    procedure LeaveRead;

    procedure ForceWrite;  // Need to write as fast as possible, force readers to stop reading.
    procedure EnterWrite;  // Normal writer, no hurry.
    procedure LeaveWrite;

    procedure ForceRead;  // Need to read as fast as possible, ignore waiting writers.
    procedure DoneRead;  // Done reading.
  end;

type
  // @exclude
  tRtcStrRec=record
    str:RtcString;
    siz:integer;
    end;
  // @exclude
  tRtcStrArr=array[0..RTC_STROBJ_PACK-1] of tRtcStrRec;
  // @exclude
  PRtcStrArr=^tRtcStrArr;
  // @exclude
  tRtcStrArray=array of PRtcStrArr;

  // @abstract(Fast Huge Ansi String manipulation)
  TRtcHugeString=class(TRtcFastObject)
  private
    FSize:int64;

    FData:tRtcStrArray;
    FPack:PRtcStrArr;

    FDataCnt,
    FPackCnt,
    FPackFree,
    FPackLoc:integer;

    FCount:integer;

    procedure GrowHugeStringList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddEx(s:byte); overload;
    procedure AddEx(const s:RtcByteArray; len:Integer=-1); overload;
    procedure Add(const s:RtcString; len:Integer=-1);

    function GetEx:RtcByteArray;
    function Get:RtcString;

    property Size:int64 read FSize;
    end;

  // @exclude
  tRtcBytesRec=record
    str:RtcByteArray;
    siz:integer;
    end;
  // @exclude
  tRtcBytesArr=array[0..RTC_STROBJ_PACK-1] of tRtcBytesRec;
  // @exclude
  PRtcBytesArr=^tRtcBytesArr;
  // @exclude
  tRtcBytesArray=array of PRtcBytesArr;

  // @abstract(Fast Huge Byte Array manipulation)
  TRtcHugeByteArray=class(TRtcFastObject)
  private
    FSize:int64;
    
    FData:tRtcBytesArray;
    FPack:PRtcBytesArr;

    FDataCnt,
    FPackCnt,
    FPackFree,
    FPackLoc:integer;

    FCount:integer;

    procedure GrowHugeStringList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddEx(s:byte); overload;
    procedure AddEx(const s:RtcByteArray; len:Integer=-1; loc:integer=0); overload;
    procedure Add(const s:RtcString; len:Integer=-1; loc:integer=1);

    procedure AddPackEx(const s:RtcByteArray; packSize:integer; len:Integer=-1; loc:integer=0);
    procedure AddPack(const s:RtcString; packSize:integer; len:Integer=-1; loc:integer=1);

    function GetStartEx(len:integer):RtcByteArray;
    procedure DelStart(len:integer);

    function GetEx:RtcByteArray;
    function Get:RtcString;

    property Size:int64 read FSize;
    end;

  // @exclude
  tRtcStrObjRec=record
    str:RtcString;
    obj:TObject;
    end;
  // @exclude
  tRtcStrObjArr=array[0..RTC_STROBJ_PACK-1] of tRtcStrObjRec;
  // @exclude
  PRtcStrObjArr=^tRtcStrObjArr;

  // @exclude
  tRtcStrObjArray=array of PRtcStrObjArr;

  // @abstract(Fast Ansi String Object List)
  tRtcFastStrObjList=class(TRtcFastObject)
  private
    FData:tRtcStrObjArray; // array of PRtcStrObjArr;
    FPack:PRtcStrObjArr;
    Tree:TStrIntList;

    FDataCnt, 
    FPackCnt:integer;
    FCnt:integer;
    FOnChange: TNotifyEvent;

    function GetName(const index: integer): RtcString;
    function GetValue(const index: integer): TObject;
    procedure SetName(const index: integer; const _Value: RtcString);
    procedure SetValue(const index: integer; const _Value: TObject);
    function GetCount: integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure DestroyObjects;

    function Add(const Name:RtcString; _Value:TObject=nil):integer;
    function Find(const Name:RtcString):integer;
    function IndexOf(const Name:RtcString):integer;

    // Case-sensitive Add, Find and IndexOf
    function AddCS(const Name:RtcString; _Value:TObject=nil):integer;
    function FindCS(const Name:RtcString):integer;
    function IndexOfCS(const Name:RtcString):integer;

    property Objects[const index:integer]:TObject read GetValue write SetValue;
    property Strings[const index:integer]:RtcString read GetName write SetName;

    property Count:integer read GetCount;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    end;

  // @exclude
  tRtcStringObjRec=record
    str:RtcWideString;
    obj:TObject;
    end;
  // @exclude
  tRtcStringObjArr=array[0..RTC_STROBJ_PACK-1] of tRtcStringObjRec;
  // @exclude
  PRtcStringObjArr=^tRtcStringObjArr;

  // @exclude
  tRtcStringObjArray=array of PRtcStringObjArr;

  // @abstract(Fast Unicode / Wide String Object List)
  tRtcFastStringObjList=class(TRtcFastObject)
  private
    FData:tRtcStringObjArray; // array of PRtcStringObjArr;
    FPack:PRtcStringObjArr;
    Tree:TStringIntList;

    FDataCnt,
    FPackCnt:integer;
    FCnt:integer;
    FOnChange: TNotifyEvent;

    function GetName(const index: integer): RtcWideString;
    function GetValue(const index: integer): TObject;
    procedure SetName(const index: integer; const _Value: RtcWideString);
    procedure SetValue(const index: integer; const _Value: TObject);
    function GetCount: integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure DestroyObjects;

    function Add(const Name:RtcWideString; _Value:TObject=nil):integer;
    function Find(const Name:RtcWideString):integer;
    function IndexOf(const Name:RtcWideString):integer;

    // Case-sensitive Add, Find and IndexOf
    function AddCS(const Name:RtcWideString; _Value:TObject=nil):integer;
    function FindCS(const Name:RtcWideString):integer;
    function IndexOfCS(const Name:RtcWideString):integer;

    property Objects[const index:integer]:TObject read GetValue write SetValue;
    property Strings[const index:integer]:RtcWideString read GetName write SetName;

    property Count:integer read GetCount;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    end;

  // @exclude
  tRtcStrValRec=record
    str:RtcString;
    val:RtcString;
    end;
  // @exclude
  tRtcStrValArr=array[0..RTC_STROBJ_PACK-1] of tRtcStrValRec;
  // @exclude
  PRtcStrValArr=^tRtcStrValArr;

  // @exclude
  tRtcStrValArray=array of PRtcStrValArr;

  // @abstract(Fast Name = Value String List)
  tRtcFastStrValueList=class(TRtcFastObject)
  private
    FData:tRtcStrValArray; // array of PRtcStrValArr;
    FPack:PRtcStrValArr;
    Tree:TStrIntList;

    FDataCnt,
    FPackCnt:integer;
    FCnt:integer;
    FOnChange: TNotifyEvent;

    function GetName(const index: integer): RtcString;
    function GetValue(const index: integer): RtcString;
    procedure SetName(const index: integer; const _Value: RtcString);
    procedure SetValue(const index: integer; const _Value: RtcString);
    function GetCount: integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure DestroyObjects;

    function Add(const Name:RtcString; _Value:RtcString=''):integer;
    function Find(const Name:RtcString):integer;
    function IndexOf(const Name:RtcString):integer;

    // Case-sensitive Add, Find and IndexOf
    function AddCS(const Name:RtcString; _Value:RtcString=''):integer;
    function FindCS(const Name:RtcString):integer;
    function IndexOfCS(const Name:RtcString):integer;

    property Values[const index:integer]:RtcString read GetValue write SetValue;
    property Names[const index:integer]:RtcString read GetName write SetName;

    property Count:integer read GetCount;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    end;

  // @exclude
  PRtcHPACK_DynTableItem = ^TRtcHPACK_DynTableItem;
  // @exclude
  TRtcHPACK_DynTableItem = record
    Name:RtcString;
    Value:RtcString;
    Prior,Next:PRtcHPACK_DynTableItem;
    end;

  { @abstract(Dynamic Header Table)
    This class implements a Dynamic HTTP Header Table,
    as described in RFC 7541 (HPACK: Header Compression for HTTP/2) }
  tRtcDynamicHeaderTable=class(TRtcFastObject)
  private
    FCount,
    FSize,
    FMaxSize,
    FTableSize:Integer;

    First,Last:PRtcHPACK_DynTableItem;

    function GetCount: Integer;
    function GetMaxSize: Integer;
    function GetSize: Integer;

    procedure SetMaxSize(const Value: Integer);

    function GetTableSize: Integer;
    procedure SetTableSize(const Value: Integer);

    procedure MakeSpace(const Len: Integer);

  public
    { Create a Dynamic Header Table, using "StartMaxTableSize"
      to initialize both: "TableSize" and "MaxSize" properties. }
    constructor Create(StartMaxTableSize:Integer);

    destructor Destroy; override;

    { Insert "Name:Value" pair to the Table, return TRUE if success. @html(<br>)
      If adding "Name:Value" to the Table would exceed "MaxSize", this function will
      automatically make space in the Table by removing entries at the end of the Table,
      until there is enough space to insert "Name:Value", or until the Table is Empty. @html(<br>)
      If there is still NOT ENOUGH space to insert "Name:Value" AFTER removing all other entries,
      "Name:Value" pair will NOT be added to the Table and this function will return FALSE. }
    function Insert(const Name, Value:RtcString):boolean;

    { Check if "Name" and "Value" pair already exists in the Table, then ... @html(<br>)
      * If "Name" and "Value" pair exists, return their index in the Table (>0).  @html(<br>)
      * If "Name" exists, but "Value" does NOT match, return the negative index to "Name" (<0). @html(<br>)
      * If "Name" does NOT exist in the Table, return 0. }
    function Find(const Name, Value:RtcString):Integer;

    { Get "Name" and "Value" stored at index "idx" (starting from 1) in the Table, returns TRUE if success.
      If Table does NOT contain an entry at index "idx", clears "Name" and "Value" variables and returns FALSE. }
    function Get(idx:Integer; var Name, Value:RtcString):boolean;

    // Number of (current) Entries in the Table
    property Count:Integer read GetCount;
    // Total Size used by all Entries currently stored in this Table
    property Size:Integer read GetSize;

    { Maximum Size (in bytes) allowed to store Headers (Name:Value pairs).
      If a change to "MaxSize" would result in "Size" to become greater than "MaxSize",
      entries will be removed from the end of the Table until "Size" becomes
      smaller or equal to "MaxSize", or ... until the Table is Empty. @html(<br><br>)

      WARNING: "MaxSize" can NOT be set to a value greater than "TableSize"!
      If you do try to set "MaxSize" to a value greater than "TableSize",
      then "MaxSize" will be set to the current value of "TableSize". }
    property MaxSize:Integer read GetMaxSize write SetMaxSize;

    { Maximum alowed Table Size (in bytes).
      This is the "agreed upon" limit for the Table Size
      (sent from the decoder, acknowledged by the encoder). @html(<br><br>)

      If "TableSize" is reduced below the current value of "MaxSize",
      then "MaxSize" will be set to the new value of "TableSize".
      If reducing "MaxSize" would also result in "Size" to become greater than "MaxSize",
      entries will be removed from the end of the Table until "Size" becomes
      smaller  or equal to "MaxSize", or ... until the Table is Empty. }
    property TableSize:Integer read GetTableSize write SetTableSize;
    end;

type
  // @abstract(function type used for Unicode to ANSI code conversions)
  RtcUnicodeToAnsiFunc = function(Chr:Word):Byte;
  // @abstract(function type used for ANSI to Unicode code conversions)
  RtcAnsiToUnicodeFunc = function(Chr:Byte):Word;

{ By default, RTC will be using code-page cpWin1252 (Latin I) for implicit Unicode to ANSI conversions.
  To change the code-page used by RTC (use a different convertion), you have two options:  @html(<br><br>)

   A) If you need one of the code-pages listed above, simply use the function "RtcSetAnsiCodePage" 
      and set the code-page which you want to be used from this point on.   @html(<br><br>)

   B) If support for the code-page you need is NOT included, then you can implement your own functions similar
      to the "RtcUnicodeToAnsiCharWin1252" and "RtcAnsiToUnicodeCharWin1252" functions provided below (implementation section), 
      then assign their addresses to global "RtcUnicodeToAnsiChar" and "RtcAnsiToUnicodeChar" variables (function pointers).
      NOTE: Only conversions for single-byte ANSI code-pages can be implemented.  @html(<br><br>)

   To define how implicit Unicode<->ANSI conversions should work, use "RTC_STRING_FIXMODE" and "RTC_STRING_CHECK" variables.  @html(<br><br>)

   By default, RTC_STRING_MODE will be set to rtcStr_FixDown and RTC_STRING_CHECK will be FALSE, which means that
   implicit conversions will automatically be done from Unicode Strings to ANSI, but NOT in the other direction and
   there will be NO exceptions if a Unicode character is found which can NOT be mapped to the current ANSI code-page. }
  RtcAnsiCodePages=(// no conversion
                    cpNone,
                    // Central Europe (windows-1250)
                    cpWin1250,
                    // Cyrillic (windows-1251)
                    cpWin1251,
                    // Latin I (windows-1252)
                    cpWin1252,
                    // Greek (windows-1253)
                    cpWin1253,
                    // Turkish (windows-1254)
                    cpWin1254,
                    // Hebrew (windows-1255)
                    cpWin1255,
                    // Arabic (windows-1256)
                    cpWin1256,
                    // Baltic (windows-1257)
                    cpWin1257,
                    // Vietnam (windows-1258)
                    cpWin1258,
                    // Thai (windows-874)
                    cpWin874,
                    // Latin 1 (iso-8859-1)
                    cpISO8859_1,
                    // Latin 2 (iso-8859-2)
                    cpISO8859_2,
                    // Latin 3 (iso-8859-3)
                    cpISO8859_3,
                    // Baltic (iso-8859-4)
                    cpISO8859_4,
                    // Cyrillic (iso-8859-5)
                    cpISO8859_5,
                    // Arabic (iso-8859-6)
                    cpISO8859_6,
                    // Greek (iso-8859-7)
                    cpISO8859_7,
                    // Hebrew (iso-8859-8)
                    cpISO8859_8,
                    // Turkish (iso-8859-9)
                    cpISO8859_9,
                    // Latin 9 (iso-8859-15)
                    cpISO8859_15 );

var
  // @abstract(Pointer to the function used for Unicode to ANSI character conversions)
  RtcUnicodeToAnsiChar:RtcUnicodeToAnsiFunc=nil;
  // @abstract(Pointer to the function used for ANSI to Unicode character conversions)
  RtcAnsiToUnicodeChar:RtcAnsiToUnicodeFunc=nil;

{$IFDEF UNICODE}
  {$IFDEF RTC_BYTESTRING}
  //@exclude
  function UpperCase(const s:RtcString):RtcString; overload;
  //@exclude
  function Trim(const S: RtcString): RtcString; overload;
  {$ENDIF}
{$ENDIF}

// @exclude
function Lower_Case(const s:RtcString):RtcString;
// @exclude
function Lower_WCase(const s:RtcWideString):RtcWideString;

// @exclude
function Upper_Case(const s:RtcString):RtcString;
// @exclude
function Upper_WCase(const s:RtcWideString):RtcWideString;

// @exclude
function Same_Text(const s1,s2:RtcString):boolean;
// @exclude
function Same_WText(const s1,s2:RtcWideString):boolean;

// @exclude
function Up_Case(const c:RtcChar):RtcChar;
// @exclude
function Up_WCase(const c:RtcWideChar):RtcWideChar;

{ Set the code-page for implicit Unicode <-> ANSI conversions done by RTC.
  This will assign the "RtcUnicodeToAnsiChar" and "RtcAnsiToUnicodeChar" functions
  to use one of the built-in implementations for ANSI <-> Unicode character conversions.
  Supported built-in code-pages are cpWin874 and cpWin1250 through cpWin1258.
  Use "cpNone" to disable all implicit ANSI <-> Unicode conversions by RTC.
  For all other code-pages, you can write your own conversion functions and assign
  them manually to the "RtcUnicodeToAnsiChar" and "RtcAnsiToUnicodeChar" variables. }
procedure RtcSetAnsiCodePage(page:RtcAnsiCodePages);

// Convert Unicode String to ANSI String with the RtcUnicodeToAnsiChar function
function RtcUnicodeToAnsiString(const Source:RtcString):RtcString;

// Convert ANSI String to Unicode String with the RtcAnsiToUnicodeChar function
function RtcAnsiToUnicodeString(const Source:RtcString):RtcString;

// Cross-platform "GetTickCount" function
function Get_TickTime:Cardinal;

{ Returns the number of milliseconds passsed since the Application was started (32-bit version).
  WARNING: Because "Cardinal" is a 32-bit integer type and this function returns the number of
  milliseconds (1/1000th of a second) passed since the last time this APPLICATION was start,
  the "GetTickTime" function should NOT be used to measure Time in Applications running
  longer than 49.7 days, because "GetTickTime" will overflow once every 49.7 days. }
function GetTickTime:Cardinal;

{ Returns the number of milliseconds passed since the Application was started (64-bit version).
  If there are no TRtcTimer instances active, then this function should be called at least
  once every 49.7 days for the integrated "GetTickCount" overflow hanling to work correctly.
  "GetTickTime64" is used internally by RTC Timers and Timeouts to trigger timed events. }
function GetTickTime64:int64;

{ Return ID of the current Thread (GetCurrentThreadID) }
function GetMyThreadID:RtcThrID;

// Does file with name "fname" exist?
function File_Exists(const fname:RtcWideString):boolean;
// Size of the file with name "fname"
function File_Size(const fname:RtcWideString):int64;

// Read "Size" bytes of file "fname", starting at "Loc" (0=begining), using "AccessMode" (default = rtc_ShareDenyNone)
function Read_File(const fname:RtcWideString; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;
// Read complete file "fname", using "AccessMode" (default = rtc_ShareDenyNone)
function Read_File(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;

// Read "Size" bytes of file "fname", starting at "Loc" (0=begining), using "AccessMode" (default = rtc_ShareDenyNone)
function Read_FileEx(const fname:RtcWideString; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcByteArray; overload;
// Read complete file "fname", using "AccessMode" (default = rtc_ShareDenyNone)
function Read_FileEx(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcByteArray; overload;

{ Scan up to "Size" bytes of file "fname" for RtcByteArray "search_string",
  starting at "Loc" (0=beginning), using "AccessMode" (default = rtc_ShareDenyNone)
  and up to "BufferSize" memory for scanning the file.
  Larger buffer size will increase scanning speed, but use more memory.
  Recommended are "BufferSize" values of 16000 or more bytes. @html(<br><br>)
  If "search_string" is found, its location in file is returned (0=beginning).
  If "search_string" is not found, this function returns -1. }
function Scan_File(const fname:RtcWideString; const search_string:RtcByteArray; BufferSize:integer; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):int64; overload;

// Delete file "fname"
function Delete_File(const fname:RtcWideString):boolean;
// Rename file "old_name" to "new_name"
function Rename_File(const old_name,new_name:RtcWideString):boolean;

{ Write "Data" to file "fname" starting at "Loc" (0=begining, -1 = end), using "AccessMode" (default = rtc_ShareDenyNone).
  If write fails, retry up to "Retries" number of times (-1 = RTC_WRITE_RETRIES)
  with a delay of "RetryDelayMS" milliseconds (-1 = RTC_WRITE_RETRY_DELAY) between reach retry attempt. }
function Write_File(const fname:RtcWideString; const Data:RtcString; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; Retries:integer=-1; RetryDelayMS:integer=-1):boolean; overload;
{ Write "Data" to file "fname", overwriting old file, using "AccessMode" (default = rtc_ShareDenyNone)
  If write fails, retry up to "Retries" number of times (-1 = RTC_WRITE_RETRIES)
  with a delay of "RetryDelayMS" milliseconds (-1 = RTC_WRITE_RETRY_DELAY) between reach retry attempt. }
function Write_File(const fname:RtcWideString; const Data:RtcString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; Retries:integer=-1; RetryDelayMS:integer=-1):boolean; overload;

{ Write "Data" to file "fname" starting at "Loc" (0=begining, -1 = end), using "AccessMode" (default = rtc_ShareDenyNone)
  If write fails, retry up to "Retries" number of times (-1 = RTC_WRITE_RETRIES)
  with a delay of "RetryDelayMS" milliseconds (-1 = RTC_WRITE_RETRY_DELAY) between reach retry attempt. }
function Write_FileEx(const fname:RtcWideString; const Data:RtcByteArray; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; Retries:integer=-1; RetryDelayMS:integer=-1):boolean; overload;
{ Write "Data" to file "fname", overwriting old file, using "AccessMode" (default = rtc_ShareDenyNone)
  If write fails, retry up to "Retries" number of times (-1 = RTC_WRITE_RETRIES)
  with a delay of "RetryDelayMS" milliseconds (-1 = RTC_WRITE_RETRY_DELAY) between reach retry attempt. }
function Write_FileEx(const fname:RtcWideString; const Data:RtcByteArray; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; Retries:integer=-1; RetryDelayMS:integer=-1):boolean; overload;

{ Copy content from file "fromName" to file "toName", starting to read at "fromLoc" in "fromFile" and writing to "toLoc" in "toFile",
  copying "fromSize" bytes from the file "fromName" to file "toName" by using a memory buffer of "maxSize" bytes. }
function Copy_FileEx(const fromName,toName:RtcWideString; fromLoc,toLoc,fromSize,maxSize:int64; fromAccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; toAccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):boolean;

function Open_File(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):TRtcFileHdl;
function Read_File(const f:TRtcFileHdl; Loc,Size:int64):RtcString; overload;
function Scan_File(const f:TRtcFileHdl; const search_string:RtcByteArray; BufferSize:integer; Loc,Size:int64):int64; overload;
procedure Close_File(const f:TRtcFileHdl);

// File date and time
function File_Age(const fname:RtcWideString):TDateTime;
// Get Temporary Directory path
function GetTempDirectory:RtcWideString;
{ Get a Temporary File Name inside a Temporary Directory.
  (A) If "RTC_DEFAULT_TEMP_FOLDER" variable is empty (default),
  this function will be using the "GetTempDirectory" function
  to select a Temporary Folder, then use the "GetTempFileName" API 
  (OS-specific) function to get a Temporary File Name inside that folder.
  (B) If "RTC_DEFAULT_TEMP_FOLDER" variable is assigned (not empty),
  a GUID will be used to generate a unique file name inside the folder
  specified by the "RTC_DEFAULT_TEMP_FOLDER" global variable. }
function GetTempFile:RtcWideString;

// Append "Plus" byte to the "Dest" byte array
procedure AddBytes(var Dest:RtcByteArray; Plus:Byte); overload;
// Append "Plus" byte array to the "Dest" byte array
procedure AddBytes(var Dest:RtcByteArray; const Plus:RtcByteArray); overload;
{ Append "len" items (default=all) from the "Plus" byte array to the "Dest" byte array,
  starting from "loc" index in the "Plus" byte array (0-based; default=0) }
procedure AddBytes(var Dest:RtcByteArray; const Plus:RtcByteArray; loc:Integer; len:Integer=-1); overload;

// Delete "len" items from the beginning of the "Dest" byte array
procedure DelBytes(var Dest:RTcByteArray; Len:Integer);

// Returns TRUE if "Source" can safely be used with RtcStringTo* functions (no data loss?)
function isRtcStringOK(const Source:RtcString):boolean;

// If "Source" contains Unicode characters and "RTC_STRING_CHECK" = TRUE, raise an Exception
procedure RtcStringCheck(const Source:RtcString);

// Return a "byte" String containing a zero-terminated "Source" byte array
function RtcBytesZeroToString(const Source:RtcByteArray):RtcString; overload;
// Return a zero-terminated Byte array containing the "Source" string
function RtcStringToBytesZero(const Source:RtcString):RtcByteArray; overload;

// Return a "byte" String containing "len" bytes from a zero-terminated "Source" byte array, starting from "loc" index (0-based)
function RtcBytesZeroToString(const Source:RtcByteArray; loc:Integer; len:Integer=-1):RtcString; overload;
// Return a zero-terminated Byte array containing "len" characters from the "Source" string, starting from "loc" index (1-based)
function RtcStringToBytesZero(const Source:RtcString; loc:Integer; len:Integer=-1):RtcByteArray; overload;

// Return a String containing the "Source" byte array
function RtcBytesToString(const Source:RtcByteArray):RtcString; overload;
// Return a byte array containing the "Source" string
function RtcStringToBytes(const Source:RtcString):RtcByteArray; overload;
// Return a Hexadecimal String containing the "Source" byte array
function RtcBytesToHex(const Source:RtcByteArray):RtcString; overload;
{ Return a Hexadecimal String containing the "Source" byte array,
  separating every "Separate" bytes using the "Separator" String. }
function RtcBytesToHex(const Source:RtcByteArray; Separate:Byte; const Separator:RtcString):RtcString; overload;

// Return a String containing "len" bytes from the "Source" byte array, starting from "loc" index (0-based)
function RtcBytesToString(const Source:RtcByteArray; loc:Integer; len:Integer=-1):RtcString; overload;
// Return a byte array containing "len" characters from the "Source" string, starting from "loc" index (1-based)
function RtcStringToBytes(const Source:RtcString; loc:Integer; len:Integer=-1):RtcByteArray; overload;

// Return a "byte" String from a zero-terminated "Source"
function RtcPBytesZeroToString(var Source):RtcString;

// Return "Len" characters from "Source" byte array
function RtcPBytesToString(var Source; Len:Cardinal):RtcString; overload;
// Copy "Len" characters from "Source" to "Dest" byte array
procedure RtcStringToPBytes(Source:RtcString; var Dest; Len:Cardinal); overload;
// Copy "Len" characters starting from "Loc" index in "Source", to "Dest" byte array
procedure RtcStringToPBytes(Source:RtcString; var Dest; Loc, Len:Cardinal); overload;
// Copy "Len" characters starting from "Loc" index in "Source", to "Dest" byte array and add #0 at the end
procedure RtcStringToPBytesZero(Source:RtcString; var Dest; Loc, Len:Cardinal); overload;

// Return a WideString containing "len" characters from the "Source" byte array starting from "loc" index
function RtcBytesToWideString(const Source:RtcByteArray; loc:Integer=0; len:Integer=-1):RtcWideString;
// Return a byte array containing "len" bytes from the "Source" WideString starting from "loc" index
function RtcWideStringToBytes(const Source:RtcWideString; loc:Integer=1; len:Integer=-1):RtcByteArray;

// Encode String to be used as Http URL.
{ If Safe=TRUE, the following characters will NOT be encoded:
  Ampersand ("&")
  Semi-colon (";")
  Equals ("=")
  Question mark ("?")
  Slash ("/") }
function URL_Encode(const AStr:RtcString; Safe:boolean=False):RtcString;
// Decode Http URL String to be used in Delphi code
{ If Strict=TRUE, any errors in AStr (malformed string) will raise an exception.
  If Strict=FALSE, errors in AStr will be ignored, so that string part
  which can be properly decoded will be decoded, the rest will remain untouched. }
function URL_Decode(const AStr:RtcString; Strict:boolean=False):RtcString;

// Encode a binary String to Mime (Base-64).
// When called with "toJSON=True", line breaks are NOT inserted
function Mime_Encode(const s: RtcString; toJSON:boolean=False): RtcString;
// decode a Mime (Base-64) String to a binary String
function Mime_Decode(const s: RtcString): RtcString;

// Encode a binary String to Mime (Base-64).
// When called with "toJSON=True", line breaks are NOT inserted
function Mime_EncodeEx(const s: RtcByteArray; toJSON:boolean=False): RtcByteArray;
// decode a Mime (Base-64) String to a Byte Array
function Mime_DecodeEx(const s: RtcByteArray): RtcByteArray; overload;
// decode a Mime (Base-64) String to a Byte Array
function Mime_DecodeEx(const s: RtcString): RtcByteArray; overload;

// Convert RtcWideString to a normal UTF-8 encoded String
function Utf8Encode(const Source: RtcWideString; SourceOffset:Integer=1; SourceLength:Integer=-1): RtcString;
// Convert a normal UTF-8 encoded String to a RtcWideString
function Utf8Decode(const Source: RtcString; SourceOffset:Integer=1; SourceLength:Integer=-1):RtcWideString;

// Convert RtcWideString to an UTF-8 encoded byte array
function Utf8EncodeEx(const Source: RtcWideString; SourceOffset:Integer=1; SourceLength:Integer=-1): RtcByteArray;
// Convert an UTF-8 encoded byte array to a RtcWideString
function Utf8DecodeEx(const Source: RtcByteArray; SourceOffset:Integer=0; SourceLength:Integer=-1):RtcWideString;

{ Returns the expected Length (in bytes) of encoded "Source" (String of 8-bit characters),
  when used with Huffman Code from RFC 7541 (HPACK: Header Compression for HTTP/2). @html(<br>)
  Also check related functions "Huff_Encode", "Huff_Decode", "Huff_EncodeEx" and "Huff_DecodeEx". }
function Huff_CheckLength(const Source:RtcString):Integer;

{ Encode "Source" using Huffman Code from RFC 7541 (HPACK: Header Compression for HTTP/2).@html(<br>)
  "Huff_CheckLength" can be used to quickly check the expected length of Encoded Source.
  "Huff_DecodeEx" can be used to Decode the result produced by this function. @html(<br>)
  This function is optimized for performance with minimal memory usage and produces the best
  results when used on short literals (down to 67% of the original size when encoded), but
  is NOT suitable for encoding binary data (up to 400% of the original size after encoding)
  and should NOT be used with Unicode text, because only 8 bits of each character are encoded. }
function Huff_EncodeEx(const Source:RtcString):RtcByteArray;

// Alias for: RtcStringToBytes(Huff_EncodeEx(Source))
function Huff_Encode(const Source:RtcString):RtcString;

{ Decode "Data" encoded with Huffman Code from RFC 7541 (HPACK: Header Compression for HTTP/2). @html(<br>)
  "From" (optional) defines the index in the "Data" array (0-based) where decoding should start [default=0],
  "Len" (optional) defines the number of bytes from "Data" to decode [default=Length(Data)-From]. @html(<br>)
  Also check related functions: "Huff_CheckLength" and "Huff_EncodeEx". }
function Huff_DecodeEx(const Data:RtcByteArray; From:Integer=0; Len:Integer=-1):RtcString;

// Alias for: Huff_DecodeEx(RtcStringToBytes(Data))
function Huff_Decode(const Data:RtcString):RtcString;

{ Encode "Value" (unsigned 64-bit integer) into a dynamic-length array of bytes, using the least number of bytes.
  Use the Integer Representation as described in RFC 7541, Section 5.1 (HPACK: Header Compression for HTTP/2). @html(<br>)
  "PrefixBits" is the number of bits (default=8) usable in the Prefix (1st byte in the output array).
  To decode the array produced by this function, you can use the "LInt_EncodeEx" function. }
function LInt_EncodeEx(const Value:int64; PrefixBits:Byte=8):RtcByteArray;

{ Decode an unsigned 64-bit Integer Value from "Data" (dynamic-length array of bytes), previously encoded
  using the Integer Representation as described in RFC 7541, Section 5.1 (HPACK: Header Compression for HTTP/2). @html(<br>)
  "FromByte" is the starting index (0-based, default=0) in the "Data" byte array, where decoding should begin.
  "PrefixBits" is the number of bits (default=8) to be decoded from the "Prefix" (1st byte in "Data").
  If there was an error decoding the "Data" byte array, returns 0 and sets "Len" to 0. Otherwise,
  Returns the (decoded) Integer value and sets "Len" to the number bytes read from "Data". @html(<br>)
  See the "LInt_EncodeEx" function for info about encoding (reverse procedure). }
function LInt_DecodeEx(const Data:RtcByteArray; var Len:Byte; PrefixBits:Byte=8; FromByte:Integer=0):int64;

{ This function does the same as the "LInt_DecodeEx" function,
  but expects an unsigned 32-bit integer (Cardinal) as a Result. }
function LWord_DecodeEx(const Data:RtcByteArray; var Len:Byte; PrefixBits:Byte=8; FromByte:Integer=0):Cardinal;

{ This function does the same as the "LInt_EncodeEx" function,
  but only accepts an unsigned 32-bit integers (Cardinal) as input "Value". }
function LWord_EncodeEx(const Value:Cardinal; PrefixBits:Byte=8):RtcByteArray;

// 1-based "at" and Result (WideString)
function PosWEx(const c,s:RtcWideString):integer; overload;
function PosWEx(const c,s:RtcWideString; at:integer):integer; overload;

// 1-based "at" and Result (String)
function PosEx(const c,s:RtcString):integer; overload;
function PosEx(const c,s:RtcString; at:integer):integer; overload;

function PosEx(const c:RtcString; const s:RtcByteArray):integer; overload;
function PosEx(const c:RtcString; const s:RtcByteArray; at:integer):integer; overload;

{$IFNDEF FPC_WIDESTRING}
function PosEx(const c:RtcChar; const s:RtcString):integer; overload;
function PosEx(const c:RtcChar; const s:RtcString; at:integer):integer; overload;
{$ENDIF}

// 0-based "at" and Result
function PosEx(const c,s:RtcByteArray):integer; overload;
function PosEx(const c,s:RtcByteArray; at:integer):integer; overload;

function LWord2Str(i:Cardinal):RtcString;
{$IFNDEF FPC}
function Int2Str(i:integer):RtcString; overload;
{$ENDIF}
function Int2Str(i:int64):RtcString; overload;

function Int2Hex(i,l:integer):RtcString; overload;

function Int2WStr(i:int64):RtcWideString;

function Str2Int(const s:RtcString):integer; overload;
{$IFDEF RTC_BYTESTRING}
function Str2Int(const s:RtcWideString):integer; overload;
{$ENDIF}
function Str2IntDef(const s:RtcString; def:integer):integer; overload;
function Str2LWord(const s:RtcString):Cardinal; overload;
function Str2LWordDef(const s:RtcString; def:Cardinal):Cardinal; overload;
function Str2Int64(const s:RtcString):Int64; overload;
function Str2Int64Def(const s:RtcString; def:int64):int64; overload;

function LWordTo6bit(const i: Cardinal):RtcString;
function LWordFrom6bit(const r: RtcString):Cardinal;

implementation

function URL_Encode(const AStr: RtcString; Safe:boolean=False): RtcString;
  const
    ToHex:array[$0..$F] of RtcChar = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
  var
    sb: byte;
    a,b:integer;
  begin
  Result:='';
  SetLength(Result, Length(AStr) * 3);
  b:=0;
  for a:=1 to Length(AStr) do
    case AStr[a] of
      '0'..'9',
      'A'..'Z',
      'a'..'z',
      ',', '*', '@', '.', '_', '-', '(', ')', '$', ':':
        begin
        Inc(b);
        Result[b]:=AStr[a];
        end;
      '&', ';', '=', '?', '/':
        begin
        Inc(b);
        if Safe then // do not encode
          Result[b]:=AStr[a]
        else
          begin
          sb:=Ord(AStr[a]);
          Result[b]:='%';
          Result[b+1]:=ToHex[sb shr 4];
          Result[b+2]:=ToHex[sb and $F];
          Inc(b,2);
          end;
        end;
      ' ':
        begin
        Inc(b);
        Result[b]:='+';
        end;
      else
        begin
        sb:=Ord(AStr[a]);
        Inc(b);
        Result[b]:='%';
        Result[b+1]:=ToHex[sb shr 4];
        Result[b+2]:=ToHex[sb and $F];
        Inc(b,2);
        end;
      end;
  SetLength(Result, b);
  end;

function URL_Decode(const AStr: RtcString; Strict:boolean=False): RtcString;
  const
    fromHex:array[Ord('0')..Ord('9')] of byte = (0,1,2,3,4,5,6,7,8,9);
    fromHexA:array[Ord('A')..Ord('F')] of byte = ($A,$B,$C,$D,$E,$F);
    fromHexB:array[Ord('a')..Ord('f')] of byte = ($A,$B,$C,$D,$E,$F);
  var
    sb: byte;
    a,b:integer;
    ok: boolean;
  begin
  Result:='';
  SetLength(Result, Length(AStr));
  b:=0; a:=0;
  while a<Length(AStr) do
    begin
    Inc(a);
    case AStr[a] of
      '%':
        begin
        ok:=True;

        Inc(a);
        case AStr[a] of
          '0'..'9': sb:=fromHex[Ord(AStr[a])] shl 4;
          'A'..'F': sb:=fromHexA[Ord(AStr[a])] shl 4;
          'a'..'f': sb:=fromHexB[Ord(AStr[a])] shl 4;
          else
            begin
            sb:=0;
            ok:=False;
            if not Strict then
              begin
              Inc(b);
              Result[b]:='%';
              Inc(b);
              Result[b]:=AStr[a];
              end
            else
              raise EConvertError.Create('Error decoding URL: '+String(AStr));
            end;
          end;

        if ok then
          begin
          Inc(a);
          case AStr[a] of
            '0'..'9': Inc(sb,fromHex[Ord(AStr[a])]);
            'A'..'F': Inc(sb,fromHexA[Ord(AStr[a])]);
            'a'..'f': Inc(sb,fromHexB[Ord(AStr[a])]);
            else
              begin
              ok:=False;
              if not Strict then
                begin
                Inc(b);
                Result[b]:='%';
                Inc(b);
                Result[b]:=AStr[a-1];
                Inc(b);
                Result[b]:=AStr[a];
                end
              else
                raise EConvertError.Create('Error decoding URL: '+String(AStr));
              end;
            end;

          if ok then
            begin
            Inc(b);
            Result[b]:=RtcChar(sb);
            end;
          end;
        end;
      '+':
        begin
        Inc(b);
        Result[b]:=' ';
        end;
      else
        begin
        Inc(b);
        Result[b]:=AStr[a];
        end;
      end;
    end;
  SetLength(Result, b);
  end;

//=====================================================
//  Mime functions
//=====================================================

const
  MIME_ENCODED_LINE_BREAK = 76;
  MIME_DECODED_LINE_BREAK = MIME_ENCODED_LINE_BREAK div 4 * 3;

const
  MIME_ENCODE_TABLE: array[0..63] of Byte = (
    065, 066, 067, 068, 069, 070, 071, 072,
    073, 074, 075, 076, 077, 078, 079, 080,
    081, 082, 083, 084, 085, 086, 087, 088,
    089, 090, 097, 098, 099, 100, 101, 102,
    103, 104, 105, 106, 107, 108, 109, 110,
    111, 112, 113, 114, 115, 116, 117, 118,
    119, 120, 121, 122, 048, 049, 050, 051,
    052, 053, 054, 055, 056, 057, 043, 047);

  MIME_PAD_CHAR = Byte('=');

  MIME_DECODE_TABLE: array[Byte] of Cardinal = (
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 062, 255, 255, 255, 063,
    052, 053, 054, 055, 056, 057, 058, 059,
    060, 061, 255, 255, 255, 255, 255, 255,
    255, 000, 001, 002, 003, 004, 005, 006,
    007, 008, 009, 010, 011, 012, 013, 014,
    015, 016, 017, 018, 019, 020, 021, 022,
    023, 024, 025, 255, 255, 255, 255, 255,
    255, 026, 027, 028, 029, 030, 031, 032,
    033, 034, 035, 036, 037, 038, 039, 040,
    041, 042, 043, 044, 045, 046, 047, 048,
    049, 050, 051, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255);

type
  PByte4 = ^TByte4;
  TByte4 = packed record
    b1, b2, b3, b4: Byte;
  end;

  PByte3 = ^TByte3;
  TByte3 = packed record
    b1, b2, b3: Byte;
  end;

procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
  var
    b: Cardinal;
    InnerLimit, OuterLimit: RtcIntPtr;
    InPtr: PByte3;
    OutPtr: PByte4;
  begin
  if InputByteCount < MIME_DECODED_LINE_BREAK then Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  InnerLimit := RtcIntPtr(InPtr);
  Inc(InnerLimit, MIME_DECODED_LINE_BREAK);

  OuterLimit := RtcIntPtr(InPtr) + InputByteCount;

  repeat

    repeat
      b := InPtr^.b1;
      b := b shl 8;
      b := b or InPtr^.b2;
      b := b shl 8;
      b := b or InPtr^.b3;
      Inc(InPtr);

      OutPtr^.b4 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b1 := MIME_ENCODE_TABLE[b];
      Inc(OutPtr);
      until RtcIntPtr(InPtr) >= InnerLimit;

    if InnerLimit<OuterLimit then
      begin
      OutPtr^.b1 := 13;
      OutPtr^.b2 := 10;
      OutPtr := PByte4(RtcIntPtr(OutPtr)+2);
      Inc(InnerLimit, MIME_DECODED_LINE_BREAK);
      end
    else
      Break;

    until InnerLimit > OuterLimit;
  end;

procedure MimeEncodeNoCRLF(const InputBuffer;
                           const InputByteCount: Cardinal;
                           out OutputBuffer);
  var
    b: Cardinal;
    InnerLimit, OuterLimit: RtcIntPtr;
    InPtr: PByte3;
    OutPtr: PByte4;
  begin
  if InputByteCount = 0 then Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := RtcIntPtr(InPtr) + OuterLimit;

  while RtcIntPtr(InPtr) < InnerLimit do
    begin
    b := InPtr^.b1;
    b := b shl 8;
    b := b or InPtr^.b2;
    b := b shl 8;
    b := b or InPtr^.b3;
    Inc(InPtr);

    OutPtr^.b4 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b1 := MIME_ENCODE_TABLE[b];
    Inc(OutPtr);
    end;

  case InputByteCount - OuterLimit of
    1:begin
      b := InPtr^.b1;
      b := b shl 4;
      OutPtr.b2 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr.b1 := MIME_ENCODE_TABLE[b];
      OutPtr.b3 := MIME_PAD_CHAR;
      OutPtr.b4 := MIME_PAD_CHAR;
      end;
    2:begin
      b := InPtr^.b1;
      b := b shl 8;
      b := b or InPtr^.b2;
      b := b shl 2;
      OutPtr.b3 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr.b2 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr.b1 := MIME_ENCODE_TABLE[b];
      OutPtr.b4 := MIME_PAD_CHAR;
      end;
    end;
  end;

function MimeDecodePartial(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
  var
    lByteBuffer, lByteBufferSpace, c: Cardinal;
    InPtr, OuterLimit: ^Byte;
    OutPtr: PByte3;
  begin
  if InputBytesCount > 0 then
    begin
    InPtr := @InputBuffer;
    OuterLimit := Pointer( RtcIntPtr(InPtr) + InputBytesCount );
    OutPtr := @OutputBuffer;
    lByteBuffer := ByteBuffer;
    lByteBufferSpace := ByteBufferSpace;
    while InPtr <> OuterLimit do
      begin
      c := MIME_DECODE_TABLE[InPtr^];
      Inc(InPtr);
      if c = $FF then Continue;
      lByteBuffer := lByteBuffer shl 6;
      lByteBuffer := lByteBuffer or c;
      Dec(lByteBufferSpace);

      if lByteBufferSpace <> 0 then Continue;

      OutPtr^.b3 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      OutPtr^.b2 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      OutPtr^.b1 := Byte(lByteBuffer);
      lByteBuffer := 0;
      Inc(OutPtr);
      lByteBufferSpace := 4;
      end;
    ByteBuffer := lByteBuffer;
    ByteBufferSpace := lByteBufferSpace;
    Result := RtcIntPtr(OutPtr) - RtcIntPtr(@OutputBuffer);
    end
  else
    Result := 0;
  end;

function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal; const SpaceLeft: Cardinal): Cardinal;
  var
    lByteBuffer: Cardinal;
  begin
  case ByteBufferSpace of
    1:begin
      if SpaceLeft>=2 then
        begin
        lByteBuffer := ByteBuffer shr 2;
        PByte3(@OutputBuffer)^.b2 := Byte(lByteBuffer);
        lByteBuffer := lByteBuffer shr 8;
        PByte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
        Result := 2;
        end
      else
        Result:=0;
      end;
    2:begin
      if SpaceLeft>=1 then
        begin
        lByteBuffer := ByteBuffer shr 4;
        PByte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
        Result := 1;
        end
      else
        Result:=0;
      end;
    else
      Result := 0;
    end;
  end;

function Mime_EncodeEx(const s: RtcByteArray; toJSON:boolean=False): RtcByteArray;
  var
    l: Cardinal;
    aSize, iDelta, ODelta: Cardinal;
  begin
  if Length(s)>0 then
    begin
    l := Length(s);
    if toJSON then
      begin
      iDelta:=0;
      ODelta:=0;
      if l > 0 then
        aSize := (l + 2) div 3 * 4
      else
        aSize:=l;
      SetLength(Result, aSize);
      end
    else
      begin
      if l > 0 then
        aSize := (l + 2) div 3 * 4 + (l - 1) div MIME_DECODED_LINE_BREAK * 2
      else
        aSize:=l;
      SetLength(Result, aSize);
      MimeEncodeFullLines(s[0], l, Result[0]);
      iDelta := l div MIME_DECODED_LINE_BREAK;
      ODelta := iDelta * (MIME_ENCODED_LINE_BREAK + 2);
      iDelta := iDelta * MIME_DECODED_LINE_BREAK;
      end;
    MimeEncodeNoCRLF(s[iDelta],
                     l - iDelta,
                     Result[ODelta]);
    end
  else
    SetLength(Result,0);
  end;

function Mime_DecodeEx(const s: RtcByteArray): RtcByteArray;
  var
    ByteBuffer, ByteBufferSpace: Cardinal;
    aSize,l: Cardinal;
  begin
  if Pointer(s) <> nil then
    begin
    l := Length(s);
    aSize:= (l + 3) div 4 * 3;
    SetLength(Result, aSize);
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    l := MimeDecodePartial(s[0], l, Result[0], ByteBuffer, ByteBufferSpace);
    if l<aSize then
      l := l +  MimeDecodePartialEnd(Result[l], ByteBuffer, ByteBufferSpace, aSize-l);
    SetLength(Result, l);
    end
  else
    SetLength(Result,0);
  end;

function Mime_DecodeEx(const s: RtcString): RtcByteArray;
  begin
  Result:=Mime_DecodeEx(RtcStringToBytes(s));
  end;

{$IFDEF RTC_BYTESTRING}
function Mime_Encode(const s: RtcString; toJSON:boolean=False): RtcString;
  var
    l: Cardinal;
    aSize, iDelta, ODelta: Cardinal;
  begin
  if Length(s)>0 then
    begin
    l := Length(s);
    if toJSON then
      begin
      iDelta:=0;
      ODelta:=0;
      if l > 0 then
        aSize := (l + 2) div 3 * 4
      else
        aSize:=l;
      SetLength(Result, aSize);
      end
    else
      begin
      if l > 0 then
        aSize := (l + 2) div 3 * 4 + (l - 1) div MIME_DECODED_LINE_BREAK * 2
      else
        aSize:=l;
      SetLength(Result, aSize);
      MimeEncodeFullLines(s[1], l, Result[1]);
      iDelta := l div MIME_DECODED_LINE_BREAK;
      if toJSON then
        ODelta := iDelta * (MIME_ENCODED_LINE_BREAK + 4)
      else
        ODelta := iDelta * (MIME_ENCODED_LINE_BREAK + 2);
      iDelta := iDelta * MIME_DECODED_LINE_BREAK;
      end;
    MimeEncodeNoCRLF(s[iDelta+1],
                     l - iDelta,
                     Result[ODelta+1]);
    end
  else
    SetLength(Result,0);
  end;

function Mime_Decode(const s: RtcString): RtcString;
  var
    ByteBuffer, ByteBufferSpace: Cardinal;
    aSize,l: Cardinal;
  begin
  if Pointer(s) <> nil then
    begin
    l := Length(s);
    aSize:= (l + 3) div 4 * 3;
    SetLength(Result, aSize);
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    l := MimeDecodePartial(s[1], l, Result[1], ByteBuffer, ByteBufferSpace);
    if l<aSize then
      l := l +  MimeDecodePartialEnd(Result[l+1], ByteBuffer, ByteBufferSpace, aSize-l);
    SetLength(Result, l);
    end
  else
    SetLength(Result,0);
  end;
{$ELSE}
function Mime_Encode(const s: RtcString; toJSON:boolean=False): RtcString;
  begin
  Result:=RtcBytesToString(Mime_EncodeEx(RtcStringToBytes(s),toJSON));
  end;

function Mime_Decode(const s: RtcString): RtcString;
  begin
  Result:=RtcBytesToString(Mime_DecodeEx(RtcStringToBytes(s)));
  end;
{$ENDIF}

function Utf8ByteCount(const Source: RtcWideString; SourceOffset:Integer=1; SourceLength:Integer=-1): Integer;
  var
    c, i: Cardinal;
  begin
  Result := 0;
  if Source = '' then Exit;
  if SourceLength<0 then SourceLength:=Length(Source)-SourceOffset+1;
  for i:=SourceOffset to SourceOffset+SourceLength-1 do
    begin
    c := Cardinal(Source[i]);
    if c > $7F then
      begin
      if c > $7FF then
        Inc(Result);
      Inc(Result);
      end;
    Inc(Result);
    end;
  end;

procedure UnicodeToUtf8Bytes(var Dest: RtcString; DestOffset: Cardinal; const Source: RtcWideString; SourceOffset: Integer = 1; SourceLength: Integer = -1);
  var
    i, count: Cardinal;
    c: Cardinal;
  begin
  if SourceLength<0 then SourceLength:=Length(Source)-SourceOffset+1;
  if SourceLength<=0 then Exit;
  count := DestOffset;

  for i:=SourceOffset to SourceOffset+SourceLength-1 do
    begin
    c := Cardinal(Source[i]);
    if c <= $7F then
      begin
      Dest[count] := RtcChar(c);
      Inc(count);
      end
    else if c > $7FF then
      begin
      Dest[count] := RtcChar($E0 or (c shr 12));
      Dest[count+1] := RtcChar($80 or ((c shr 6) and $3F));
      Dest[count+2] := RtcChar($80 or (c and $3F));
      Inc(count,3);
      end
    else //  $7F < Source[i] <= $7FF
      begin
      Dest[count] := RtcChar($C0 or (c shr 6));
      Dest[count+1] := RtcChar($80 or (c and $3F));
      Inc(count,2);
      end;
    end;
  end;

function UnicodeCharCount(const Source: RtcString; SourceOffset:Integer=1; SourceLength:Integer=-1): Integer;
  var
    SourceBytes, i: Cardinal;
    c: Byte;
  begin
  Result := 0;
  if SourceLength<0 then SourceLength:=Length(Source)-SourceOffset+1;
  if SourceLength<=0 then Exit;
  SourceBytes:=SourceLength+SourceOffset-1;
  i := SourceOffset;

{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    while (i <= SourceBytes) do
      begin
      c := RtcUnicodeToAnsiChar( Word(Source[i]) );
      Inc(i);
      if (c and $80) <> 0 then
        begin
        c := c and $3F;
        if (c and $20) <> 0 then
          begin
          if i > SourceBytes then Exit;          // incomplete multibyte Char
          c := RtcUnicodeToAnsiChar( Word(Source[i]) );
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
          end;
        if i > SourceBytes then Exit;        // incomplete multibyte Char
        c := RtcUnicodeToAnsiChar( Word(Source[i]) );
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
        end;
      Inc(Result);
      end;
    end
  else
{$ENDIF}
    begin
    while (i <= SourceBytes) do
      begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
        begin
        c := c and $3F;
        if (c and $20) <> 0 then
          begin
          if i > SourceBytes then Exit;          // incomplete multibyte Char
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
          end;
        if i > SourceBytes then Exit;        // incomplete multibyte Char
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
        end;
      Inc(Result);
      end;
    end;
  end;

function Utf8Decode(const Source: RtcString; SourceOffset:Integer=1; SourceLength:Integer=-1):RtcWideString;
  var
    SourceBytes, MaxDestChars, i, count: Cardinal;
    c: Byte;
    wc: Cardinal;
  begin
  if SourceLength<0 then SourceLength:=Length(Source)-SourceOffset+1;
  if SourceLength<=0 then
    begin
    Result:='';
    Exit;
    end;
  MaxDestChars:=UnicodeCharCount(Source,SourceOffset,SourceLength);
  SetLength(Result,MaxDestChars);
  SourceBytes:=SourceLength+SourceOffset-1;
  count := 0;
  i := SourceOffset;

{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    while i <= SourceBytes do
      begin
      wc :=  RtcUnicodeToAnsiChar( Word(Source[i]) );
      Inc(i);
      if (wc and $80) <> 0 then
        begin
        wc := wc and $3F;
        if (wc and $20) <> 0 then
          begin
          if i > SourceBytes then Exit;          // incomplete multibyte Char
          c := RtcUnicodeToAnsiChar( Word(Source[i]) );
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
          wc := (wc shl 6) or (c and $3F);
          end;
        if i > SourceBytes then Exit;        // incomplete multibyte Char
        c :=  RtcUnicodeToAnsiChar( Word(Source[i]) );
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Inc(count);
        Result[count] := RtcWideChar((wc shl 6) or (c and $3F));
        end
      else
        begin
        Inc(count);
        Result[count] := RtcWideChar(wc);
        end;
      end;
    end
  else
{$ENDIF}
    begin
    while i <= SourceBytes do
      begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
        begin
        wc := wc and $3F;
        if (wc and $20) <> 0 then
          begin
          if i > SourceBytes then Exit;          // incomplete multibyte Char
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
          wc := (wc shl 6) or (c and $3F);
          end;
        if i > SourceBytes then Exit;        // incomplete multibyte Char
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Inc(count);
        Result[count] := RtcWideChar((wc shl 6) or (c and $3F));
        end
      else
        begin
        Inc(count);
        Result[count] := RtcWideChar(wc);
        end;
      end;
    end;
  end;

function Utf8Encode(const Source: RtcWideString; SourceOffset:Integer=1; SourceLength:Integer=-1): RtcString;
  var
    alen:Cardinal;
  begin
  alen:=Utf8ByteCount(Source,SourceOffset,SourceLength);
  SetLength(Result,alen);
  UnicodeToUtf8Bytes(Result,1,Source,SourceOffset,SourceLength);
  end;

procedure UnicodeToUtf8BytesEx(var Dest: RtcByteArray; DestOffset: Cardinal; const Source: RtcWideString; SourceOffset: Integer = 1; SourceLength: Integer = -1);
  var
    i, count: Cardinal;
    c: Cardinal;
  begin
  if SourceLength<0 then SourceLength:=Length(Source)-SourceOffset+1;
  if SourceLength<=0 then Exit;
  count := DestOffset;
  for i:=SourceOffset to SourceOffset+SourceLength-1 do
    begin
    c := Cardinal(Source[i]);
    if c <= $7F then
      begin
      Dest[count] := c;
      Inc(count);
      end
    else if c > $7FF then
      begin
      Dest[count] := $E0 or (c shr 12);
      Dest[count+1] := $80 or ((c shr 6) and $3F);
      Dest[count+2] := $80 or (c and $3F);
      Inc(count,3);
      end
    else //  $7F < Source[i] <= $7FF
      begin
      Dest[count] := $C0 or (c shr 6);
      Dest[count+1] := $80 or (c and $3F);
      Inc(count,2);
      end;
    end;
  end;

function UnicodeCharCountEx(const Source: RtcByteArray; SourceOffset:Integer=0; SourceLength:Integer=-1): Integer;
  var
    SourceBytes, i: Cardinal;
    c: Byte;
  begin
  Result := 0;
  if SourceLength<0 then SourceLength:=Length(Source)-SourceOffset;
  if SourceLength<=0 then Exit;
  SourceBytes:=SourceLength+SourceOffset;
  i := SourceOffset;
  while (i < SourceBytes) do
    begin
    c := Source[i];
    Inc(i);
    if (c and $80) <> 0 then
      begin
      c := c and $3F;
      if (c and $20) <> 0 then
        begin
        if i >= SourceBytes then Exit;          // incomplete multibyte Char
        c := Source[i];
        Inc(i);
        if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
        end;
      if i >= SourceBytes then Exit;        // incomplete multibyte Char
      c := Source[i];
      Inc(i);
      if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
    Inc(Result);
    end;
  end;

function Utf8DecodeEx(const Source: RtcByteArray; SourceOffset:Integer=0; SourceLength:Integer=-1):RtcWideString;
  var
    SourceBytes, MaxDestChars, i, count: Cardinal;
    c: Byte;
    wc: Cardinal;
  begin
  if SourceLength<0 then SourceLength:=Length(Source)-SourceOffset;
  if SourceLength<=0 then
    begin
    Result:='';
    Exit;
    end;
  MaxDestChars:=UnicodeCharCountEx(Source,SourceOffset,SourceLength);
  SetLength(Result,MaxDestChars);
  SourceBytes:=SourceLength+SourceOffset;
  count := 0;
  i := SourceOffset;
  while i < SourceBytes do
    begin
    wc := Cardinal(Source[i]);
    Inc(i);
    if (wc and $80) <> 0 then
      begin
      wc := wc and $3F;
      if (wc and $20) <> 0 then
        begin
        if i >= SourceBytes then Exit;          // incomplete multibyte Char
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range Char
        wc := (wc shl 6) or (c and $3F);
        end;
      if i >= SourceBytes then Exit;        // incomplete multibyte Char
      c := Byte(Source[i]);
      Inc(i);
      if (c and $C0) <> $80 then Exit;       // malformed trail byte

      Inc(count);
      Result[count] := RtcWideChar((wc shl 6) or (c and $3F));
      end
    else
      begin
      Inc(count);
      Result[count] := RtcWideChar(wc);
      end;
    end;
  end;

function Utf8EncodeEx(const Source: RtcWideString; SourceOffset:Integer=1; SourceLength:Integer=-1): RtcByteArray;
  var
    alen:Cardinal;
  begin
  alen:=Utf8ByteCount(Source,SourceOffset,SourceLength);
  SetLength(Result,alen);
  UnicodeToUtf8BytesEx(Result,0,Source,SourceOffset,SourceLength);
  end;

function AnsiToBytes(const Source: RtcWideString):RtcString;
  var
    len, i: Cardinal;
  begin
  len:=Length(Source);
  SetLength(Result,len);
  if len = 0 then Exit;

  for i:=1 to len do
    Result[i] := RtcChar(Source[i]);
  end;

function BytesToAnsi(const Source: RtcString):RtcWideString;
  var
    i, len: Cardinal;
  begin
  len:=Length(Source);
  SetLength(Result, len);
  if len = 0 then Exit;

  for i:=1 to len do
    Result[i] := RtcWideChar(Source[i]);
  end;

procedure RtcStringCheck(const Source:RtcString);
{$IFDEF RTC_BYTESTRING}
  begin
  // No need to check, there can be no characters above 255 in Source
  end;
{$ELSE}
  var
    len,i: Cardinal;
  begin
  if RTC_STRING_CHECK then
    begin
    len:=Length(Source);
    if len = 0 then Exit;
    for i:=1 to len do
      if Ord(Source[i])>255 then
        if RTC_STRING_FIXMODE=rtcStr_NoFIX then
          raise ERtcSystem.Create('RtcStringCheck: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]))
        else if RtcUnicodeToAnsiChar(Ord(Source[i]))=RTC_INVALID_CHAR then // invalid char
          raise ERtcSystem.Create('RtcStringCheck: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]));
    end;
  end;
{$ENDIF}

function isRtcStringOK(const Source:RtcString):boolean;
{$IFDEF RTC_BYTESTRING}
  begin
  Result:=True;
  // No need to check, there can be no characters above 255 in Source
  end;
{$ELSE}
  var
    len,i: Cardinal;
  begin
  Result:=True;
  len:=Length(Source);
  if len = 0 then Exit;
  for i:=1 to len do
    if Ord(Source[i])>255 then
      if RTC_STRING_FIXMODE=rtcStr_NoFIX then
        begin
        Result:=False;
        Break;
        end
      else if RtcUnicodeToAnsiChar(Ord(Source[i]))=RTC_INVALID_CHAR then
        begin
        Result:=False;
        Break;
        end;
  end;
{$ENDIF}

function RtcBytesToString(const Source:RtcByteArray):RtcString;
  var
    len: Cardinal;
{$IFNDEF RTC_BYTESTRING}
    i, k: Cardinal;
{$ENDIF}
  begin
  len:=Length(Source);
  SetLength(Result, len);
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[0],Result[1],len);
{$ELSE}
  k := 0;
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar( RtcAnsiToUnicodeChar(Source[k]) );
      Inc(k);
      end;
    end
  else
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar(Source[k]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

const
  hexdigs:array[0..15] of RtcChar =
         ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');

function RtcBytesToHex(const Source:RtcByteArray):RtcString; overload;
  var
    len: Cardinal;
    i, k: Cardinal;
  begin
  len:=Length(Source);
  SetLength(Result, len*2);
  if len = 0 then Exit;
  i := 1;
  for k:=0 to len-1 do
    begin
    Result[i] := hexdigs[Source[k] shr 4];  Inc(i);
    Result[i] := hexdigs[Source[k] and $F]; Inc(i);
    end;
  end;

function RtcBytesToHex(const Source:RtcByteArray; Separate:Byte; const Separator:RtcString):RtcString; overload;
  var
    len: Cardinal;
    i, j, k: Cardinal;
  begin
  len:=Length(Source);
  if Separate<1 then Separate:=1;
  if len = 0 then
    begin
    Result:='';
    Exit;
    end;
  SetLength(Result, len*2 + ((len-1) div Separate)*Cardinal(length(Separator)));
  i := 1;
  for k:=0 to len-1 do
    begin
    if (k>0) and (k mod Separate=0) then
      for j:=1 to length(Separator) do
        begin
        Result[i]:=Separator[j]; Inc(i);
        end;
    Result[i] := hexdigs[Source[k] shr 4];  Inc(i);
    Result[i] := hexdigs[Source[k] and $F]; Inc(i);
    end;
  end;

function RtcBytesToString(const Source:RtcByteArray; loc:Integer; len:Integer=-1):RtcString;
{$IFNDEF RTC_BYTESTRING}
  var
    i, k: Cardinal;
{$ENDIF}
  begin
  if len<0 then len:=Length(Source)-loc;
  SetLength(Result, len);
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[loc],Result[1],len);
{$ELSE}
  k := loc;
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar( RtcAnsiToUnicodeChar(Source[k]) );
      Inc(k);
      end;
    end
  else
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar(Source[k]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

function RtcPBytesZeroToString(var Source):RtcString;
  var
    len, i: Cardinal;
    Src: PRtcByte;
  begin
  len:=0;
  Src:=PRtcByte(Addr(Source));
  while Src^<>0 do
    begin
    Inc(len);
    Inc(Src);
    end;
  SetLength(Result, len);
  if len = 0 then Exit;

  Src:=PRtcByte(Addr(Source));
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar( RtcAnsiToUnicodeChar(Src^) );
      Inc(Src);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar(Src^);
      Inc(Src);
      end;
    end;
  end;

function RtcPBytesToString(var Source; Len:Cardinal):RtcString;
{$IFNDEF RTC_BYTESTRING}
  var
    i: Cardinal;
    Src: PRtcByte;
{$ENDIF}
  begin
  SetLength(Result, Len);
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source,Result[1],len);
{$ELSE}
  Src:=PRtcByte(Addr(Source));
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar( RtcAnsiToUnicodeChar(Src^) );
      Inc(Src);
      end;
    end
  else
    begin
    for i:=1 to len do
      begin
      Result[i] := RtcChar(Src^);
      Inc(Src);
      end;
    end;
{$ENDIF}
  end;
procedure RtcStringToPBytes(Source:RtcString; var Dest; Len:Cardinal);
  var
    i: Cardinal;
    Dst: PRtcByte;
  begin
  if len = 0 then Exit;
  Dst:=PRtcByte(Addr(Dest));
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        begin
        Dst^:=RtcUnicodeToAnsiChar(Ord(Source[i]));
        if RTC_STRING_CHECK and (Dst^=RTC_INVALID_CHAR) then
          raise ERtcSystem.Create('RtcStringToPBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]));
        end
      else
        Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        raise ERtcSystem.Create('RtcStringToPBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]));
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=1 to len do
      begin
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end;
  end;

procedure RtcStringToPBytes(Source:RtcString; var Dest; Loc, Len:Cardinal);
  var
    i: Cardinal;
    Dst: PRtcByte;
  begin
  if len = 0 then Exit;
  Dst:=PRtcByte(Addr(Dest));
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=loc to loc+len-1 do
      begin
      if Ord(Source[i])>255 then
        begin
        Dst^:=RtcUnicodeToAnsiChar(Ord(Source[i]));
        if RTC_STRING_CHECK and (Dst^=RTC_INVALID_CHAR) then
          raise ERtcSystem.Create('RtcStringToPBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]));
        end
      else
        Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=loc to loc+len-1 do
      begin
      if Ord(Source[i])>255 then
        raise ERtcSystem.Create('RtcStringToPBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]));
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=loc to loc+len-1 do
      begin
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  end;

procedure RtcStringToPBytesZero(Source:RtcString; var Dest; Loc, Len:Cardinal);
  var
    i: Cardinal;
    Dst: PRtcByte;
  begin
  Dst:=PRtcByte(Addr(Dest));
  if len = 0 then
    begin
    Dst^:=0;
    Exit;
    end;
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=loc to loc+len-1 do
      begin
      if Ord(Source[i])>255 then
        begin
        Dst^:=RtcUnicodeToAnsiChar(Ord(Source[i]));
        if RTC_STRING_CHECK and (Dst^=RTC_INVALID_CHAR) then
          raise ERtcSystem.Create('RtcStringToPBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]));
        end
      else
        Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=loc to loc+len-1 do
      begin
      if Ord(Source[i])>255 then
        begin
        Dst^:=0; // close dest string
        raise ERtcSystem.Create('RtcStringToPBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]));
        end;
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=loc to loc+len-1 do
      begin
      Dst^:=Ord(Source[i]);
      Inc(Dst);
      end;
    end;
  Dst^:=0;
  end;

function RtcBytesZeroToString(const Source:RtcByteArray):RtcString;
  var
    len: Integer;
    i, k: Cardinal;
  begin
  len:=Length(Source);
  SetLength(Result, len);
  if len = 0 then Exit;
  k := 0;
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      if Source[k]=0 then
        begin
        SetLength(Result,i-1);
        Break;
        end
      else
        Result[i] := RtcChar( RtcAnsiToUnicodeChar(Source[k]) );
      Inc(k);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=1 to len do
      begin
      if Source[k]=0 then
        begin
        SetLength(Result,i-1);
        Break;
        end
      else
        Result[i] := RtcChar(Source[k]);
      Inc(k);
      end;
    end;
  end;

function RtcBytesZeroToString(const Source:RtcByteArray; loc:Integer; len:Integer=-1):RtcString;
  var
    i, k: Cardinal;
  begin
  if len<0 then len:=Length(Source)-loc;
  SetLength(Result, len);
  if len = 0 then Exit;
  k := loc;
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=1 to len do
      begin
      if Source[k]=0 then
        begin
        SetLength(Result,i-1);
        Break;
        end
      else
        Result[i] := RtcChar( RtcAnsiToUnicodeChar(Source[k]) );
      Inc(k);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=1 to len do
      begin
      if Source[k]=0 then
        begin
        SetLength(Result,i-1);
        Break;
        end
      else
        Result[i] := RtcChar(Source[k]);
      Inc(k);
      end;
    end;
  end;

function RtcStringToBytes(const Source:RtcString):RtcByteArray;
  var
    len: Cardinal;
{$IFNDEF RTC_BYTESTRING}
    i, k: Cardinal;
{$ENDIF}
  begin
  len:=Length(Source);
  SetLength(Result, len);
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[1],Result[0],len);
{$ELSE}
  k := 0;
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        begin
        Result[k] := RtcUnicodeToAnsiChar(Ord(Source[i]));
        if RTC_STRING_CHECK and (Result[k]=RTC_INVALID_CHAR) then
          raise ERtcSystem.Create('RtcStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]));
        end
      else
        Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        raise ERtcSystem.Create('RtcStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]));
      Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end
  else
    begin
    for i:=1 to len do
      begin
      Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

function RtcStringToBytes(const Source:RtcString; loc:Integer; len:Integer=-1):RtcByteArray;
{$IFNDEF RTC_BYTESTRING}
  var
    i, k: Cardinal;
{$ENDIF}
  begin
  if len<0 then len:=Length(Source)-loc+1;
  SetLength(Result, len);
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[loc],Result[0],len);
{$ELSE}
  k := loc;
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        begin
        Result[i]:=RtcUnicodeToAnsiChar(Ord(Source[k]));
        if RTC_STRING_CHECK and (Result[i]=RTC_INVALID_CHAR) then
          raise ERtcSystem.Create('RtcStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Char(Source[k]));
        end
      else
        Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        raise ERtcSystem.Create('RtcStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Char(Source[k]));
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else
    begin
    for i:=0 to len-1 do
      begin
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

function RtcStringToBytesZero(const Source:RtcString):RtcByteArray;
  var
    len: Cardinal;
{$IFNDEF RTC_BYTESTRING}
    i, k: Cardinal;
{$ENDIF}
  begin
  len:=Length(Source);
  SetLength(Result, len+1);
  Result[len]:=0;
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[1],Result[0],len);
{$ELSE}
  k := 0;
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        begin
        Result[k]:=RtcUnicodeToAnsiChar(Ord(Source[i]));
        if RTC_STRING_CHECK and (Result[k]=RTC_INVALID_CHAR) then
          raise ERtcSystem.Create('RtcStringToBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]));
        end
      else
        Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=1 to len do
      begin
      if Ord(Source[i])>255 then
        raise ERtcSystem.Create('RtcStringToBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[i]))+' = '+Char(Source[i]));
      Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end
  else
    begin
    for i:=1 to len do
      begin
      Result[k] := Byte(Source[i]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

function RtcStringToBytesZero(const Source:RtcString; loc:Integer; len:Integer=-1):RtcByteArray;
{$IFNDEF RTC_BYTESTRING}
  var
    i, k: Cardinal;
{$ENDIF}
  begin
  if len<0 then len:=Length(Source)-loc+1;
  SetLength(Result, len+1);
  Result[len]:=0;
  if len = 0 then Exit;
{$IFDEF RTC_BYTESTRING}
  Move(Source[loc],Result[0],len);
{$ELSE}
  k := loc;
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        begin
        Result[i]:=RtcUnicodeToAnsiChar(Ord(Source[k]));
        if RTC_STRING_CHECK and (Result[i]=RTC_INVALID_CHAR) then
          raise ERtcSystem.Create('RtcStringToBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Char(Source[k]));
        end
      else
        Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        raise ERtcSystem.Create('RtcStringToBytesZero: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Char(Source[k]));
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else
    begin
    for i:=0 to len-1 do
      begin
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end;
{$ENDIF}
  end;

function RtcBytesToWideString(const Source:RtcByteArray; loc:Integer=0; len:Integer=-1):RtcWideString;
  var
    i, k: Cardinal;
  begin
  if len<0 then len:=Length(Source)-loc;
  SetLength(Result, len);
  if len = 0 then Exit;
  k := loc;
  for i:=1 to len do
    begin
    Result[i] := RtcWideChar(Source[k]);
    Inc(k);
    end;
  end;

function RtcWideStringToBytes(const Source:RtcWideString; loc:Integer=1; len:Integer=-1):RtcByteArray;
  var
    i, k: Cardinal;
  begin
  if len<0 then len:=Length(Source)-loc+1;
  SetLength(Result, len);
  if len = 0 then Exit;
  k := loc;
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        begin
        Result[i]:=RtcUnicodeToAnsiChar(Ord(Source[k]));
        if RTC_STRING_CHECK and (Result[i]=RTC_INVALID_CHAR) then
          raise ERtcSystem.Create('RtcWideStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Char(Source[k]));
        end
      else
        Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=0 to len-1 do
      begin
      if Ord(Source[k])>255 then
        raise ERtcSystem.Create('RtcWideStringToBytes: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Char(Source[k]));
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else
    begin
    for i:=0 to len-1 do
      begin
      Result[i] := Byte(Source[k]);
      Inc(k);
      end;
    end;
  end;

procedure AddBytes(var Dest:RtcByteArray; Plus:Byte);
  var
    olen:Integer;
  begin
  olen:=Length(Dest);
  SetLength(Dest,olen+1);
  Dest[olen]:=Plus;
  end;

procedure AddBytes(var Dest:RtcByteArray; const Plus:RtcByteArray; loc:Integer; len:Integer=-1);
  var
    olen:Integer;
  begin
  olen:=Length(Dest);
  if len<0 then len:=Length(Plus)-loc;
  if len<=0 then Exit;
  SetLength(Dest,olen+len);
  Move(Plus[loc],Dest[olen],len);
  end;

procedure AddBytes(var Dest:RtcByteArray; const Plus:RtcByteArray);
  var
    len, olen:Integer;
  begin
  len:=Length(Plus);
  if len<=0 then Exit;
  olen:=Length(Dest);
  SetLength(Dest,olen+len);
  Move(Plus[0],Dest[olen],len);
  end;

procedure DelBytes(var Dest:RTcByteArray; Len:Integer);
  begin
  if Len>0 then
    if Length(Dest)>Len then
      Dest:=Copy(Dest,Len,Length(Dest)-Len)
    else
      SetLength(Dest,0);
  end;

function GetMyThreadID:RtcThrID;
{$IFDEF Windows}
  begin
  Result:=RtcThrID(GetCurrentThreadID);
  end;
{$ELSE}{$IFDEF FPC}
  begin
  Result:=RtcThrID(GetCurrentThreadID);
  end;
{$ELSE}{$IFDEF POSIX}
  begin
  Result:=RtcThrID(pthread_self);
  end;
{$ELSE}{$IFDEF CLR}
  begin
  Result:=System.Threading.Thread.CurrentThreadId;
  end;
{$ELSE}
  begin
  Result:=0;
  {$MESSAGE WARN 'GetMyThreadID implementation missing.'}
  end;
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

function Get_TickTime:Cardinal;
{$IFDEF WINDOWS}
  begin
  Result:=GetTickCount;
  end;
{$ELSE}{$IFDEF POSIX}
  {$IFDEF MACOSX}
    begin
    Result:=AbsoluteToNanoseconds(UpTime) div 1000000;
    end;
  {$ELSE}{$IFDEF MACIOS}
    var
      tv: timeval;
    begin
    gettimeofday(tv, nil);
    Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
    end;
  {$ELSE}
    var
      tv: timeval;
    begin
    gettimeofday(tv, nil);
    Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
    end;
  {$ENDIF}{$ENDIF}
{$ELSE}{$IFDEF FPC}
  var
    b: tms;
  begin
  Result := Cardinal(fpTimes(b)*10);
  end;
{$ELSE}
  begin
  Result:=0;
  {$MESSAGE WARN 'Get_TickTime implementation missing.'}
  end;
{$ENDIF}{$ENDIF}{$ENDIF}

var
  LastTickTime:int64;
  TickTimeOverflow:int64;
  TickTimeCS:TRtcCritSec;

const
{$IFDEF RTC_TICKTIME_DEBUG}
  TickTimeOverflowMask=Cardinal($1FFFF); // 131 seconds
  TickTimeOverflowDiff=-int64($3FF); // -1 seconds
{$ELSE}
  TickTimeOverflowMask=Cardinal($FFFFFFFF); // 49.7 days
  TickTimeOverflowDiff=-int64($1FFFF); // -131 seconds
{$ENDIF}

function GetTickTime64:int64;
  var
    CurrentTick:int64;
  begin
  CurrentTick:=TickTimeOverflow+1;
  Inc(CurrentTick,Get_TickTime and TickTimeOverflowMask);
  if CurrentTick-LastTickTime<TickTimeOverflowDiff then
    begin
    LastTickTime:=CurrentTick+TickTimeOverflowMask;
    TickTimeCS.Acquire;
    try
      if CurrentTick>TickTimeOverflow then
        Inc(TickTimeOverflow,TickTimeOverflowMask);
    finally
      TickTimeCS.Release;
      end;
    end
  else
    LastTickTime:=CurrentTick;
  Result:=LastTickTime;
  end;

function GetTickTime:Cardinal;
  begin
  Result:=Cardinal(GetTickTime64);
  end;

function GetTempDirectory:RtcWideString;
{$IFDEF WINDOWS}
  var
    tempFolder: array[0..MAX_PATH] of Char;
  begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := RtcWideString(StrPas(tempFolder));
  end;
{$ELSE}{$IFDEF POSIX}
  begin
  Result := GetEnvironmentVariable('TMPDIR');
  end;
{$ELSE}{$IFDEF FPC}
  begin
  Result := SysUtils.GetTempDir(true);
  end;
{$ELSE}
  begin
  Result:='';
  {$MESSAGE WARN 'GetTempDirectory implementation missing.'}
  end;
{$ENDIF}{$ENDIF}{$ENDIF}

(* function GetUniqueFileName(const OriginalFileName: String): String;
  var
    aFilePath, aFileName, aFileExt, aCount, aNewFileName: String;
    i: integer;
  const
    aFormat = '(%d)';
  begin
  Result := OriginalFileName;
  i := 0;
  aFilePath := ExtractFilePath(OriginalFileName);
  aFileName := ChangeFileExt(ExtractFileName(OriginalFileName), '');
  aFileExt  := ExtractFileExt(OriginalFileName);
  aNewFileName := OriginalFileName;
  while FileExists(aNewFileName) do 
    begin
    Inc(i);
    aCount := Format(aFormat, [i]);
    aNewFileName := IncludeTrailingPathDelimiter(aFilePath) + aFileName + aCount + aFileExt;
    end;
  Result := aNewFileName;
  end; *)

function GetTempFile:RtcWideString;
{$IFDEF WINDOWS}
  procedure GetWinTempFile;
    var
      tempFile: array[0..MAX_PATH] of Char;
      tempFolder: array[0..MAX_PATH] of Char;
    begin
    GetTempPath(MAX_PATH, @tempFolder);
    if GetTempFileName(@tempFolder, 'RTC', 0, @tempFile)<>0 then
      Result := RtcWideString(StrPas(tempFile))
    else
      Result := '';
    end;
  procedure GetRtcTempFile;
    var
      aSID:TGUID;
      tempFolder:RtcWideString;
    begin
    if CreateGuid(aSID) = S_OK then
      begin
      tempFolder:=RTC_DEFAULT_TEMP_FOLDER;
      if not DirectoryExists(tempFolder) then
        CreateDir(tempFolder);
      if tempFolder[Length(tempFolder)]<>FOLDER_DELIMITER then
        tempFolder:=tempFolder+FOLDER_DELIMITER;
      Result := tempFolder + GuidToString(aSID) + '.tmp';
      end
    else
      GetWinTempFile;
    end;
  begin
  if RTC_DEFAULT_TEMP_FOLDER<>'' then
    GetRtcTempFile
  else
    GetWinTempFile;
  end;
{$ELSE}{$IFDEF POSIX}
  var
    tempFolder:String;
    a: integer;
    fn:RtcWideString;
  begin
  if RTC_DEFAULT_TEMP_FOLDER<>'' then
    tempFolder:=RTC_DEFAULT_TEMP_FOLDER
  else
    tempFolder:=GetTempDirectory;
  if tempFolder[Length(tempFolder)]<>FOLDER_DELIMITER then
    tempFolder:=tempFolder+FOLDER_DELIMITER;
  tempFolder:=tempFolder+'tmp_';
  repeat
    fn:='';
    for a := 1 to 16 do
      fn:=fn+RtcWideChar(Ord('a')+random(26));
    until not File_Exists(tempFolder+fn+'.rtc');
  end;
{$ELSE}{$IFDEF FPC}
  var
    tempFolder: String;
  begin
  if RTC_DEFAULT_TEMP_FOLDER<>'' then
    tempFolder:=RTC_DEFAULT_TEMP_FOLDER
  else
    tempFolder:=GetTempDirectory;
  Result:=GetTempFileName(tempFolder, 'RTC');
  end;
{$ELSE}
  begin
  {$MESSAGE WARN 'GetTempFile implementation missing.'}
  end;
{$ENDIF}{$ENDIF}{$ENDIF}

function File_Exists(const fname:RtcWideString):boolean;
  var
    f:TRtcFileHdl;
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  if f=RTC_INVALID_FILE_HDL then
    Result:=False
  else
    begin
    FileClose(f);
    Result:=True;
    end;
  end;

function File_Size(const fname:RtcWideString):int64;
  var
    f:TRtcFileHdl;
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  if f=RTC_INVALID_FILE_HDL then
    Result:=-1
  else
    begin
    Result:=FileSeek(f,int64(0),2);
    FileClose(f);
    end;
  end;

function File_Age(const fname:RtcWideString):TDateTime;
  var
    f:TRtcFileHdl;
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  if f=RTC_INVALID_FILE_HDL then
    Result:=-1
  else
    begin
    Result:=FileDateToDateTime(FileGetDate(f));
    FileClose(f);
    end;
  end;

function Read_FileEx(const f:TRtcFileHdl; Loc,Size:int64):RtcByteArray; overload;
  var
    sRead:int64;
  begin
  SetLength(Result,0);
  if f<>RTC_INVALID_FILE_HDL then
    begin
    if Loc<0 then
      Loc:=0;
    if Size<0 then
      Size:=FileSeek(f,int64(0),2)-Loc;
    if FileSeek(f,Loc,0)<>Loc then
      Exit;
    if Size>0 then
      begin
      SetLength(Result,Size);
      sRead:=FileRead(f,Result[0],Size);
      if sRead<Size then
        SetLength(Result,sRead);
      end;
    end;
  end;

function Read_FileEx(const fname:RtcWideString; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcByteArray; overload;
  var
    f:TRtcFileHdl;
    sRead:int64;
  begin
  SetLength(Result,0);
  case AccessMode of
    rtc_ShareDenyNone:  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
    rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenRead+fmShareDenyWrite);
    else                f:=FileOpen(fname,fmOpenRead+fmShareExclusive);
    end;
  if f=RTC_INVALID_FILE_HDL then
    Exit
  else
    begin
    try
      if Loc<0 then
        Loc:=0;
      if Size<0 then
        Size:=FileSeek(f,int64(0),2)-Loc;
      if FileSeek(f,Loc,0)<>Loc then
        Exit;
      if Size>0 then
        begin
        SetLength(Result,Size);
        sRead:=FileRead(f,Result[0],Size);
        if sRead<Size then
          SetLength(Result,sRead);
        end;
    finally
      FileClose(f);
      end;
    end;
  end;

function Read_FileEx(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcByteArray; overload;
  begin
  Result:=Read_FileEx(fname,0,-1,AccessMode);
  end;

{$IFDEF RTC_BYTESTRING}
function Read_File(const fname:RtcWideString; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;
  var
    f:TRtcFileHdl;
    sRead:int64;
  begin
  SetLength(Result,0);
  case AccessMode of
    rtc_ShareDenyNone:  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
    rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenRead+fmShareDenyWrite);
    else                f:=FileOpen(fname,fmOpenRead+fmShareExclusive);
    end;
  if f=RTC_INVALID_FILE_HDL then
    Exit
  else
    begin
    try
      if Loc<0 then
        Loc:=0;
      if Size<0 then
        Size:=FileSeek(f,int64(0),2)-Loc;
      if FileSeek(f,Loc,0)<>Loc then
        Exit;
      if Size>0 then
        begin
        SetLength(Result,Size);
        sRead:=FileRead(f,Result[1],Size);
        if sRead<Size then
          SetLength(Result,sRead);
        end;
    finally
      FileClose(f);
      end;
    end;
  end;

function Read_File(const f:TRtcFileHdl; Loc,Size:int64):RtcString; overload;
  var
    sRead:int64;
  begin
  SetLength(Result,0);
  if f<>RTC_INVALID_FILE_HDL then
    begin
    if Loc<0 then
      Loc:=0;
    if Size<0 then
      Size:=FileSeek(f,int64(0),2)-Loc;
    if FileSeek(f,Loc,0)<>Loc then
      Exit;
    if Size>0 then
      begin
      SetLength(Result,Size);
      sRead:=FileRead(f,Result[1],Size);
      if sRead<Size then
        SetLength(Result,sRead);
      end;
    end;
  end;

function Read_File(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;
  begin
  Result:=Read_File(fname,0,-1,AccessMode);
  end;
{$ELSE}
function Read_File(const f:TRtcFileHdl; Loc,Size:int64):RtcString; overload;
  begin
  Result:=RtcBytesToString(Read_FileEx(f,Loc,Size));
  end;

function Read_File(const fname:RtcWideString; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;
  begin
  Result:=RtcBytesToString(Read_FileEx(fname,Loc,Size,AccessMode));
  end;

function Read_File(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):RtcString; overload;
  begin
  Result:=RtcBytesToString(Read_FileEx(fname,0,-1,AccessMode));
  end;
{$ENDIF}

procedure Close_File(const f:TRtcFileHdl);
  begin
  FileClose(f);
  end;

function Scan_File(const f:TRtcFileHdl; const search_string:RtcByteArray; BufferSize:integer; Loc,Size:int64):int64; overload;
  var
    test:RtcByteArray;
    have_size,
    want_size,
    old_size, mypos, len:int64;
  begin
  Result:=-1;
  if (f<>RTC_INVALID_FILE_HDL) and (Size>0) and (Length(search_string)>0) then
    begin
    if Loc<0 then
      Loc:=0;
    if Size<0 then
      Size:=FileSeek(f,int64(0),2)-Loc;
    if FileSeek(f,Loc,0)<>Loc then
      Exit;
    old_size:=0;
    len:=Length(search_string);
    BufferSize:=BufferSize+len;
    test:=nil;
    if BufferSize<Size then
      SetLength(test,BufferSize)
    else
      SetLength(test,Size);
    repeat
      // Do not read more than requested
      if Size>BufferSize-old_size then
        want_size:=BufferSize-old_size // Do not oveflow our memory buffer
      else
        want_size:=Size;

      // Read next block behind last one
      if want_size>0 then
        have_size := FileRead(f, test[old_size], want_size)
      else
        have_size := 0;
        
      if have_size<=0 then // no more data to read!
        Break
      else if Length(test)>old_size+have_size then // less data read than memory reserved
        SetLength(test, old_size+have_size);

      mypos:=PosEx(search_string, test);
      if mypos>=0 then
        begin
        // Loc = last reading location
        // mypos = substring location
        // rd_size = bytes left-over from last read
        Result:=Loc+mypos-old_size;
        Break;
        end
      else if (have_size=want_size) and (Size>0) then // expecting more data
        begin
        // Copy last "len" bytes to the beginning of our test RtcByteArray
        Move(test[old_size+have_size-len],test[0],len);
        old_size:=len;

        Dec(Size,have_size);
        Inc(Loc,have_size);
        end
      else // this was last block read
        Break;
      until False;
    end;
  end;

function Open_File(const fname:RtcWideString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):TRtcFileHdl;
  begin
  case AccessMode of
    rtc_ShareDenyNone:  Result:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
    rtc_ShareDenyWrite: Result:=FileOpen(fname,fmOpenRead+fmShareDenyWrite);
    else                Result:=FileOpen(fname,fmOpenRead+fmShareExclusive);
    end;
  end;

function Scan_File(const fname:RtcWideString; const search_string:RtcByteArray; BufferSize:integer; Loc,Size:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):int64; overload;
  var
    f:TRtcFileHdl;
    test:RtcByteArray;
    have_size,
    want_size,
    old_size, mypos, len:int64;
  begin
  Result:=-1;
  case AccessMode of
    rtc_ShareDenyNone:  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
    rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenRead+fmShareDenyWrite);
    else                f:=FileOpen(fname,fmOpenRead+fmShareExclusive);
    end;
  if (f<>RTC_INVALID_FILE_HDL) and (Size>0) and (Length(search_string)>0) then
    begin
    try
      if Loc<0 then
        Loc:=0;
      if Size<0 then
        Size:=FileSeek(f,int64(0),2)-Loc;
      if FileSeek(f,Loc,0)<>Loc then
        Exit;
      old_size:=0;
      len:=Length(search_string);
      BufferSize:=BufferSize+len;
      test:=nil;
      if BufferSize<Size then
        SetLength(test,BufferSize)
      else
        SetLength(test,Size);
      repeat
        // Do not read more than requested
        if Size>BufferSize-old_size then
          want_size:=BufferSize-old_size // Do not oveflow our memory buffer
        else
          want_size:=Size;

        // Read next block behind last one
        if want_size>0 then
          have_size := FileRead(f, test[old_size], want_size)
        else
          have_size := 0;
          
        if have_size<=0 then // no more data to read!
          Break
        else if Length(test)>old_size+have_size then // less data read than memory reserved
          SetLength(test, old_size+have_size);

        mypos:=PosEx(search_string, test);
        if mypos>=0 then
          begin
          // Loc = last reading location
          // mypos = substring location
          // rd_size = bytes left-over from last read
          Result:=Loc+mypos-old_size;
          Break;
          end
        else if (have_size=want_size) and (Size>0) then // expecting more data
          begin
          // Copy last "len" bytes to the beginning of our test RtcByteArray
          Move(test[old_size+have_size-len],test[0],len);
          old_size:=len;

          Dec(Size,have_size);
          Inc(Loc,have_size);
          end
        else // this was last block read
          Break;
        until False;
    finally
      FileClose(f);
      end;
    end
  else if (f<>RTC_INVALID_FILE_HDL) then
    FileClose(f);
  end;

function Delete_File(const fname:RtcWideString):boolean;
  begin
  if File_Exists(fname) then
    Result:=DeleteFile(fname)
  else
    Result:=False;
  end;

function Rename_File(const old_name,new_name:RtcWideString):boolean;
  begin
  Result:=SysUtils.RenameFile(old_name,new_name);
  end;

function Write_FileEx(const fname:RtcWideString; const Data:RtcByteArray; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; Retries:integer=-1; RetryDelayMS:integer=-1):boolean; overload;
  var
    f:TRtcFileHdl;
  begin
  Result:=False;
  if Retries<0 then Retries:=RTC_WRITE_RETRIES;
  if RetryDelayMS<0 then RetryDelayMS:=RTC_WRITE_RETRY_DELAY;
  repeat
    case AccessMode of
      rtc_ShareDenyNone: f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyNone);
      rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyWrite);
      else f:=FileOpen(fname,fmOpenReadWrite+fmShareExclusive);
      end;
    if f=RTC_INVALID_FILE_HDL then
      f:=FileCreate(fname);
    if f<>RTC_INVALID_FILE_HDL then
      begin
      // if Loc<0 then Loc:=0;
      try
        if Length(Data)>0 then
          begin
          if Loc<0 then
            begin
            FileSeek(f,0,2);
            if FileWrite(f,data[0],Length(data))=Length(data) then
              Result:=True;
            end
          else
            begin
            if FileSeek(f,Loc,0)=Loc then
              if FileWrite(f,data[0],Length(data))=Length(data) then
                Result:=True;
            end;
          end
        else
          Result:=True;
      finally
        FileClose(f);
        end;
      end;
    if not Result then
      if Retries>0 then
        Sleep(RetryDelayMS);
    Dec(Retries);
    until Result or (Retries<0);
  end;

// Write "Data" to file "fname", overwriting old file.
function Write_FileEx(const fname:RtcWideString; const Data:RtcByteArray; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; Retries:integer=-1; RetryDelayMS:integer=-1):boolean; overload;
  begin
  DeleteFile(fname);
  Result:=Write_FileEx(fname, data, 0, AccessMode, Retries, RetryDelayMS);
  end;

{$IFDEF RTC_BYTESTRING}
function Write_File(const fname:RtcWideString; const Data:RtcString; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; Retries:integer=-1; RetryDelayMS:integer=-1):boolean; overload;
  var
    f:TRtcFileHdl;
  begin
  Result:=False;
  if Retries<0 then Retries:=RTC_WRITE_RETRIES;
  if RetryDelayMS<0 then RetryDelayMS:=RTC_WRITE_RETRY_DELAY;
  repeat
    case AccessMode of
      rtc_ShareDenyNone: f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyNone);
      rtc_ShareDenyWrite: f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyWrite);
      else f:=FileOpen(fname,fmOpenReadWrite+fmShareExclusive);
      end;
    if f=RTC_INVALID_FILE_HDL then
      f:=FileCreate(fname);
    if f<>RTC_INVALID_FILE_HDL then
      begin
      // if Loc<0 then Loc:=0;
      try
        if Length(Data)>0 then
          begin
          if Loc<0 then
            begin
            FileSeek(f,0,2);
            if FileWrite(f,data[1],Length(data))=Length(data) then
              Result:=True;
            end
          else
            begin
            if FileSeek(f,Loc,0)=Loc then
              if FileWrite(f,data[1],Length(data))=Length(data) then
                Result:=True;
            end;
          end
        else
          Result:=True;
      finally
        FileClose(f);
        end;
      end;
    if not Result then
      if Retries>0 then
        Sleep(RetryDelayMS);
    Dec(Retries);
    until Result or (Retries<0);
  end;

// Write "Data" to file "fname", overwriting old file.
function Write_File(const fname:RtcWideString; const Data:RtcString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; Retries:integer=-1; RetryDelayMS:integer=-1):boolean; overload;
  begin
  DeleteFile(fname);
  Result:=Write_File(fname, data, 0, AccessMode, Retries, RetryDelayMS);
  end;
{$ELSE}
function Write_File(const fname:RtcWideString; const Data:RtcString; Loc:int64; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; Retries:integer=-1; RetryDelayMS:integer=-1):boolean; overload;
  begin
  Result:=Write_FileEx(fname, RtcStringToBytes(Data), Loc, AccessMode, Retries, RetryDelayMS);
  end;

function Write_File(const fname:RtcWideString; const Data:RtcString; AccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; Retries:integer=-1; RetryDelayMS:integer=-1):boolean; overload;
  begin
  Result:=Write_FileEx(fname, RtcStringToBytes(Data), AccessMode, Retries, RetryDelayMS);
  end;
{$ENDIF}

function Copy_FileEx(const fromName,toName:RtcWideString; fromLoc,toLoc,fromSize,maxSize:int64; fromAccessMode:TRtcFileAccessMode=rtc_ShareDenyNone; toAccessMode:TRtcFileAccessMode=rtc_ShareDenyNone):boolean;
  var
    fs,fd:TRtcFileHdl;
    sRead:int64;
    data:RtcByteArray;
    LeftSize,NowSize:int64;
  begin
  Result:=False;
  case fromAccessMode of
    rtc_ShareDenyNone:  fs:=FileOpen(fromName,fmOpenRead+fmShareDenyNone);
    rtc_ShareDenyWrite: fs:=FileOpen(fromName,fmOpenRead+fmShareDenyWrite);
    else                fs:=FileOpen(fromName,fmOpenRead+fmShareExclusive);
    end;
  if fs=RTC_INVALID_FILE_HDL then
    Exit
  else
    begin
    try
      if fromLoc<0 then
        fromLoc:=0;
      if fromSize<0 then
        fromSize:=FileSeek(fs,int64(0),2)-fromLoc;
      if FileSeek(fs,fromLoc,0)<>fromLoc then
        Exit;
      case toAccessMode of
        rtc_ShareDenyNone:  fd:=FileOpen(toName,fmOpenReadWrite+fmShareDenyNone);
        rtc_ShareDenyWrite: fd:=FileOpen(toName,fmOpenReadWrite+fmShareDenyWrite);
        else                fd:=FileOpen(toName,fmOpenReadWrite+fmShareExclusive);
        end;
      if fd=RTC_INVALID_FILE_HDL then
        fd:=FileCreate(toName);
      if fd<>RTC_INVALID_FILE_HDL then
        begin
        try
          if toLoc<0 then
            FileSeek(fd,0,2)
          else if FileSeek(fd,toLoc,0)<>toLoc then
            Exit;

          Result:=True;

          LeftSize:=fromSize;
          while LeftSize>0 do
            begin
            if LeftSize>maxSize then
              NowSize:=maxSize
            else
              NowSize:=LeftSize;

            if NowSize>0 then
              begin
              SetLength(data,NowSize);
              sRead:=FileRead(fs,data[0],NowSize);
              if sRead<NowSize then
                SetLength(data,sRead);
              end
            else
              begin
              SetLength(data,0);
              sRead:=0;
              end;

            if sRead<=0 then
              begin
              Result:=False;
              Break;
              end
            else if FileWrite(fd,data[0],sRead)<>sRead then
              begin
              Result:=False;
              Break;
              end;

            Dec(LeftSize,sRead);
            end;
        finally
          SetLength(data,0);
          FileClose(fd);
          end;
        end;
    finally
      FileClose(fs);
      end;
    end;
  end;

function PosEx(const c,s:RtcByteArray):integer;
  var
    a,b,lc:integer;
  begin
  lc:=Length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=0 to Length(s)-1 do
        if s[a]=c[0] then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=0 to Length(s)-2 do
        if (s[a]=c[0]) and (s[a+1]=c[1]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=0 to Length(s)-lc do
        if (s[a]=c[0]) and (s[a+1]=c[1]) and (s[a+2]=c[2]) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if (s[a+b]<>c[b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>=0 then
            Break;
          end;
      end;
    end;
  end;

function PosEx(const c,s:RtcByteArray; at:integer):integer;
  var
    a,b,lc:integer;
  begin
  lc:=Length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=at to Length(s)-1 do
        if s[a]=c[0] then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=at to Length(s)-2 do
        if (s[a]=c[0]) and (s[a+1]=c[1]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=at to Length(s)-lc do
        if (s[a]=c[0]) and (s[a+1]=c[1]) and (s[a+2]=c[2]) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if (s[a+b]<>c[b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>=0 then
            Break;
          end;
      end;
    end;
  end;

{$IFNDEF FPC_WIDESTRING}

function PosEx(const c:RtcChar; const s:RtcString; at:integer):integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=at to Length(s) do
    if s[a]=c then
      begin
      Result:=a;
      Break;
      end;
  end;

function PosEx(const c:RtcChar; const s:RtcString):integer;
  var
    a:integer;
  begin
  Result:=-1;
  for a:=1 to Length(s) do
    if s[a]=c then
      begin
      Result:=a;
      Break;
      end;
  end;

{$ENDIF}

function PosWEx(const c,s:RtcWideString):integer;
  var
    a,b,lc:integer;
  begin
  lc:=Length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=1 to Length(s) do
        if s[a]=c[1] then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=1 to Length(s)-1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=1 to Length(s)-lc+1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) and (s[a+2]=c[3]) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if (s[a+b]<>c[1+b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>0 then
            Break;
          end;
      end;
    end;
  end;

function PosWEx(const c,s:RtcWideString; at:integer):integer;
  var
    a,b,lc:integer;
  begin
  lc:=Length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=at to Length(s) do
        if s[a]=c[1] then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=at to Length(s)-1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=at to Length(s)-lc+1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) and (s[a+2]=c[3]) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if (s[a+b]<>c[1+b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>0 then
            Break;
          end;
      end;
    end;
  end;

function PosEx(const c,s:RtcString):integer;
  var
    a,b,lc:integer;
  begin
  lc:=Length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=1 to Length(s) do
        if s[a]=c[1] then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=1 to Length(s)-1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=1 to Length(s)-lc+1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) and (s[a+2]=c[3]) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if (s[a+b]<>c[1+b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>0 then
            Break;
          end;
      end;
    end;
  end;

function PosEx(const c,s:RtcString; at:integer):integer;
  var
    a,b,lc:integer;
  begin
  lc:=Length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=at to Length(s) do
        if s[a]=c[1] then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=at to Length(s)-1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=at to Length(s)-lc+1 do
        if (s[a]=c[1]) and (s[a+1]=c[2]) and (s[a+2]=c[3]) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if (s[a+b]<>c[1+b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>0 then
            Break;
          end;
      end;
    end;
  end;

function PosEx(const c:RtcString; const s:RtcByteArray):integer;
  var
    a,b,lc:integer;
  begin
  lc:=Length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=1 to Length(s) do
        if s[a-1]=Byte(c[1]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=1 to Length(s)-1 do
        if (s[a-1]=Byte(c[1])) and (s[a]=Byte(c[2])) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=1 to Length(s)-lc+1 do
        if (s[a-1]=Byte(c[1])) and (s[a]=Byte(c[2])) and (s[a+1]=Byte(c[3])) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if s[a+b-1]<>Byte(c[1+b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>0 then
            Break;
          end;
      end;
    end;
  end;

function PosEx(const c:RtcString; const s:RtcByteArray; at:integer):integer;
  var
    a,b,lc:integer;
  begin
  lc:=Length(c);
  case lc of
    0:Result:=-1;
    1:begin
      Result:=-1;
      for a:=at to Length(s) do
        if s[a-1]=Byte(c[1]) then
          begin
          Result:=a;
          Break;
          end;
      end;
    2:begin
      Result:=-1;
      for a:=at to Length(s)-1 do
        if (s[a-1]=Byte(c[1])) and (s[a]=Byte(c[2])) then
          begin
          Result:=a;
          Break;
          end;
      end;
    else
      begin
      Result:=-1;
      for a:=at to Length(s)-lc+1 do
        if (s[a-1]=Byte(c[1])) and (s[a]=Byte(c[2])) and (s[a+1]=Byte(c[3])) then
          begin
          Result:=a;
          for b:=3 to lc-1 do
            if s[a+b-1]<>Byte(c[1+b]) then
              begin
              Result:=-1;
              Break;
              end;
          if Result>0 then
            Break;
          end;
      end;
    end;
  end;

function Str2Int(const s:RtcString):integer;
  begin
  Result:=StrToInt(String(s));
  end;

{$IFDEF RTC_BYTESTRING}
function Str2Int(const s:RtcWideString):integer;
  begin
  Result:=StrToInt(String(s));
  end;
{$ENDIF}

function Str2IntDef(const s:RtcString; def:integer):integer;
  begin
  Result:=StrToIntDef(String(s),def);
  end;

function Str2LWord(const s:RtcString):Cardinal;
  begin
  Result:=StrToInt64(String(s));
  end;

function Str2LWordDef(const s:RtcString; def:Cardinal):Cardinal;
  begin
  Result:=StrToInt64Def(String(s),def);
  end;

function Str2Int64(const s:RtcString):Int64;
  begin
  Result:=StrToInt64(String(s));
  end;

function Str2Int64Def(const s:RtcString; def:int64):int64;
  begin
  Result:=StrToInt64Def(String(s),def);
  end;

{$IFNDEF FPC}
function Int2Str(i:integer):RtcString;
  begin
  Result:=RtcString(IntToStr(i));
  end;
{$ENDIF}

function Int2Str(i:int64):RtcString;
  begin
  Result:=RtcString(IntToStr(i));
  end;

function Int2WStr(i:int64):RtcWideString;
  begin
  Result:=RtcWideString(IntToStr(i));
  end;

function Int2Hex(i,l:integer):RtcString;
  begin
  Result:=RtcString(IntToHex(i,l));
  end;

function LWord2Str(i:Cardinal):RtcString;
  begin
  Result:=RtcString(IntToStr(i));
  end;

function LWordTo6bit(const i: Cardinal):RtcString;
  var
    u:Cardinal;
    cnt:byte;
  const
	  Dig: array[0..63] of RtcChar =
		  ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
       'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
       'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
       'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D',
       'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
       'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
       'Y', 'Z', '@', '$');
  begin
  u:=i; cnt:=0;
  while u>0 do
    begin
    u:=u shr 6;
    Inc(cnt);
    end;
  SetLength(Result,cnt);
  u:=i;
  while u>0 do
    begin
    Result[cnt]:=Dig[u and $3F];
    Dec(cnt);
    u:=u shr 6;
    end;
  end;

function LWordFrom6bit(const r: RtcString):Cardinal;
  var
    i:integer;
    k:byte;
  begin
  Result:=0;
  for i:=1 to Length(r) do
    begin
    Result:=Result shl 6;
    case r[i] of
      '0'..'9':k:=Ord(r[i])-Ord('0');
      'a'..'z':k:=Ord(r[i])-Ord('a')+10;
      'A'..'Z':k:=Ord(r[i])-Ord('A')+36;
      '@': k:=62;
      '$': k:=63;
      else k:=0;
      end;
    Result:=Result or k;
    end;
  end;

{ TRtcByteArrayStream }

constructor TRtcByteArrayStream.Create(const AData: RtcByteArray);
  begin
  inherited Create;
  if assigned(AData) then
    FData:=Copy(AData,0,Length(AData))
  else
    SetLength(FData,0);
  FPosition:=0;
  end;

function TRtcByteArrayStream.Read(var Buffer; Count: Longint): Longint;
  begin
  Result := Length(FData) - FPosition;
  if Result > Count then Result := Count;
  Move(FData[FPosition], Buffer, Result);
  Inc(FPosition, Result);
  end;

function TRtcByteArrayStream.ReadBytes(Count: Longint): RtcByteArray;
  var
    Len: Integer;
  begin
  Len := Length(FData) - FPosition;
  if Len > Count then Len := Count;
  Result:=Copy(FData,FPosition,Len);
  Inc(FPosition, Len);
  end;

function TRtcByteArrayStream.Seek(Offset: Longint; Origin: Word): Longint;
  begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := Length(FData) - Offset;
  end;
  if FPosition > Length(FData) then
    FPosition := Length(FData)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
  end;

procedure TRtcByteArrayStream.SetSize(NewSize: Longint);
  begin
  SetLength(FData, NewSize);
  if FPosition > NewSize then FPosition := NewSize;
  end;

function TRtcByteArrayStream.Write(const Buffer; Count: Longint): Longint;
  begin
  Result := Count;
  SetLength(FData, (FPosition + Result));
  Move(Buffer, FData[FPosition], Result);
  Inc(FPosition, Result);
  end;

procedure TRtcByteArrayStream.WriteBytes(const AData: RtcByteArray);
  begin
  if assigned(AData) then
    Write(AData[0], Length(AData));
  end;

{ TRtcFileStream }

destructor TRtcFileStream.Destroy;
  begin
  Close;
  inherited;
  end;

procedure TRtcFileStream.Open(const fname:RtcWideString);
  begin
  f:=FileOpen(fname,fmOpenRead+fmShareDenyNone);
  l:=0;
  if f=RTC_INVALID_FILE_HDL then
    raise ERtcSystem.Create('Unable to open file for read access.');
  end;

procedure TRtcFileStream.Close;
  begin
  if f<>RTC_INVALID_FILE_HDL then
    begin
    FileClose(f);
    f:=RTC_INVALID_FILE_HDL;
    end;
  end;

function TRtcFileStream.ReadEx(Size: int64): RtcByteArray;
  var
    sRead:int64;
  begin
  if f=RTC_INVALID_FILE_HDL then
    raise ERtcSystem.Create('File not open.')
  else
    begin
    if Size=0 then
      begin
      SetLength(Result,0);
      Exit;
      end
    else if Size<0 then
      begin
      Size:=FileSeek(f,int64(0),2)-L;
      if (Size<=0) or (FileSeek(f,L,0)<>L) then
        begin
        SetLength(Result,0);
        Exit;
        end;
      end;
    SetLength(Result,Size);
    sRead:=FileRead(f,Result[0],Size);
    Inc(L,sRead);
    if sRead<Size then
      SetLength(Result,sRead);
    end;
  end;

{$IFDEF RTC_BYTESTRING}
function TRtcFileStream.Read(Size: int64): RtcString;
  var
    sRead:int64;
  begin
  if f=RTC_INVALID_FILE_HDL then
    raise ERtcSystem.Create('File not open.')
  else
    begin
    if Size=0 then
      begin
      SetLength(Result,0);
      Exit;
      end
    else if Size<0 then
      begin
      Size:=FileSeek(f,int64(0),2)-L;
      if (Size<=0) or (FileSeek(f,L,0)<>L) then
        begin
        SetLength(Result,0);
        Exit;
        end;
      end;
    SetLength(Result,Size);
    sRead:=FileRead(f,Result[1],Size);
    Inc(L,sRead);
    if sRead<Size then
      SetLength(Result,sRead);
    end;
  end;
{$ELSE}
function TRtcFileStream.Read(Size: int64): RtcString;
  begin
  Result:=RtcBytesToString(ReadEx(Size));
  end;
{$ENDIF}

procedure TRtcFileStream.Seek(Loc: int64);
  begin
  if f=RTC_INVALID_FILE_HDL then
    raise ERtcSystem.Create('File not open.')
  else
    begin
    if Loc<0 then Loc:=0;
    l:=FileSeek(f,Loc,0);
    if l<>LOC then raise ERtcSystem.Create('Error seeking through file.');
    end;
  end;

{ TRtcEvent }

constructor TRtcEvent.Create(ManualReset, InitialState: boolean);
  begin
  inherited Create(nil,ManualReset,InitialState,'');
  end;

{ TRtcRWSec }

constructor TRtcRWSec.Create;
  begin
  inherited;
  Cnt:=0;Cnt3:=0;
  PassSec:=TRtcCritSec.Create;
  ReadSec:=TRtcCritSec.Create;
  WriteSec:=TRtcEvent.Create(False,True);  // Auto-reset
  end;

destructor TRtcRWSec.Destroy;
  begin
  RtcFreeAndNil(PassSec);
  RtcFreeAndNil(ReadSec);
  RtcFreeAndNil(WriteSec);
  inherited;
  end;

procedure TRtcRWSec.EnterRead;
  begin
  PassSec.Acquire;
  PassSec.Release;

  ReadSec.Acquire;
  try
    if (Cnt=0) and (Cnt3=0) then // There are no readers inside
      WriteSec.WaitFor(WAIT_INFINITE);  // Block all writers, this is the first reader.
    Inc(Cnt);
  finally
    ReadSec.Release;
    end;
  end;

procedure TRtcRWSec.ForceRead;
  var
    OK:boolean;
  begin
  OK:=False;
  ReadSec.Acquire;
  try
    if Cnt>0 then // There are normal readers inside, writers are blocked.
      begin
      Inc(Cnt3);
      OK:=True;
      end;
  finally
    ReadSec.Release;
    end;

  if not OK then
    begin
    PassSec.Acquire;
    PassSec.Release;

    ReadSec.Acquire;
    try
      if (Cnt=0) and (Cnt3=0) then // There are no readers inside
        WriteSec.WaitFor(WAIT_INFINITE);  // Block all writers
      Inc(Cnt3);
    finally
      ReadSec.Release;
      end;
    end;
  end;

procedure TRtcRWSec.LeaveRead;
  begin
  ReadSec.Acquire;
  try
    Dec(Cnt);
    if (Cnt=0) and (Cnt3=0) then
      WriteSec.SetEvent;  // Un-block writers
  finally
    ReadSec.Release;
    end;
  end;

procedure TRtcRWSec.DoneRead;
  begin
  ReadSec.Acquire;
  try
    Dec(Cnt3);
    if (Cnt=0) and (Cnt3=0) then
      WriteSec.SetEvent;  // Un-block writers
  finally
    ReadSec.Release;
    end;
  end;

procedure TRtcRWSec.EnterWrite;
  begin
  PassSec.Acquire;

  WriteSec.WaitFor(WAIT_INFINITE);
  end;

procedure TRtcRWSec.ForceWrite;
  begin
  PassSec.Acquire;

  WriteSec.WaitFor(WAIT_INFINITE);
  end;

procedure TRtcRWSec.LeaveWrite;
  begin
  WriteSec.SetEvent;

  PassSec.Release;
  end;

{ TRtcCritSec }

{$IFDEF RTC_WIN32_CS_DEBUG}
procedure TRtcCritSec.Acquire;
  var
    waiting:integer;
  begin
  if FSection.OwningThread<>0 then
    begin
    if FSection.OwningThread<>GetCurrentThreadID then
      begin
      waiting:=0;
      while (FSection.OwningThread<>0) and (waiting<5000) do
        begin
        Sleep(1);
        Inc(waiting);
        end;
      if waiting>=5000 then
        Sleep(100); // deadlock!!!! set breakpoint here to debug.
      end;
    end;
  inherited;
  end;
{$ENDIF}

(****** ANSI to Unicode char conversion functions ******)

function RtcAnsiToUnicodeCharNone(Chr: Byte): Word;
  begin
  Result := Chr;
  end;

function RtcAnsiToUnicodeCharWin1250(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < 128) then
    Result := Chr
  else
    case Chr of
      $80: Result:= $20AC; // EURO SIGN
      $82: Result:= $201A; // SINGLE LOW-9 QUOTATION MARK
      $84: Result:= $201E; // DOUBLE LOW-9 QUOTATION MARK
      $85: Result:= $2026; // HORIZONTAL ELLIPSIS
      $86: Result:= $2020; // DAGGER
      $87: Result:= $2021; // DOUBLE DAGGER
      $89: Result:= $2030; // PER MILLE SIGN
      $8A: Result:= $0160; // LATIN CAPITAL LETTER S WITH CARON
      $8B: Result:= $2039; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $8C: Result:= $015A; // LATIN CAPITAL LETTER S WITH ACUTE
      $8D: Result:= $0164; // LATIN CAPITAL LETTER T WITH CARON
      $8E: Result:= $017D; // LATIN CAPITAL LETTER Z WITH CARON
      $8F: Result:= $0179; // LATIN CAPITAL LETTER Z WITH ACUTE
      $91: Result:= $2018; // LEFT SINGLE QUOTATION MARK
      $92: Result:= $2019; // RIGHT SINGLE QUOTATION MARK
      $93: Result:= $201C; // LEFT DOUBLE QUOTATION MARK
      $94: Result:= $201D; // RIGHT DOUBLE QUOTATION MARK
      $95: Result:= $2022; // BULLET
      $96: Result:= $2013; // EN DASH
      $97: Result:= $2014; // EM DASH
      $99: Result:= $2122; // TRADE MARK SIGN
      $9A: Result:= $0161; // LATIN SMALL LETTER S WITH CARON
      $9B: Result:= $203A; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $9C: Result:= $015B; // LATIN SMALL LETTER S WITH ACUTE
      $9D: Result:= $0165; // LATIN SMALL LETTER T WITH CARON
      $9E: Result:= $017E; // LATIN SMALL LETTER Z WITH CARON
      $9F: Result:= $017A; // LATIN SMALL LETTER Z WITH ACUTE
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $02C7; // CARON
      $A2: Result:= $02D8; // BREVE
      $A3: Result:= $0141; // LATIN CAPITAL LETTER L WITH STROKE
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $0104; // LATIN CAPITAL LETTER A WITH OGONEK
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $015E; // LATIN CAPITAL LETTER S WITH CEDILLA
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $017B; // LATIN CAPITAL LETTER Z WITH DOT ABOVE
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $02DB; // OGONEK
      $B3: Result:= $0142; // LATIN SMALL LETTER L WITH STROKE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $0105; // LATIN SMALL LETTER A WITH OGONEK
      $BA: Result:= $015F; // LATIN SMALL LETTER S WITH CEDILLA
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $013D; // LATIN CAPITAL LETTER L WITH CARON
      $BD: Result:= $02DD; // DOUBLE ACUTE ACCENT
      $BE: Result:= $013E; // LATIN SMALL LETTER L WITH CARON
      $BF: Result:= $017C; // LATIN SMALL LETTER Z WITH DOT ABOVE
      $C0: Result:= $0154; // LATIN CAPITAL LETTER R WITH ACUTE
      $C1: Result:= $00C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $C2: Result:= $00C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $C3: Result:= $0102; // LATIN CAPITAL LETTER A WITH BREVE
      $C4: Result:= $00C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $C5: Result:= $0139; // LATIN CAPITAL LETTER L WITH ACUTE
      $C6: Result:= $0106; // LATIN CAPITAL LETTER C WITH ACUTE
      $C7: Result:= $00C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $C8: Result:= $010C; // LATIN CAPITAL LETTER C WITH CARON
      $C9: Result:= $00C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $CA: Result:= $0118; // LATIN CAPITAL LETTER E WITH OGONEK
      $CB: Result:= $00CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $CC: Result:= $011A; // LATIN CAPITAL LETTER E WITH CARON
      $CD: Result:= $00CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $CE: Result:= $00CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $CF: Result:= $010E; // LATIN CAPITAL LETTER D WITH CARON
      $D0: Result:= $0110; // LATIN CAPITAL LETTER D WITH STROKE
      $D1: Result:= $0143; // LATIN CAPITAL LETTER N WITH ACUTE
      $D2: Result:= $0147; // LATIN CAPITAL LETTER N WITH CARON
      $D3: Result:= $00D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $D4: Result:= $00D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $D5: Result:= $0150; // LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
      $D6: Result:= $00D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $0158; // LATIN CAPITAL LETTER R WITH CARON
      $D9: Result:= $016E; // LATIN CAPITAL LETTER U WITH RING ABOVE
      $DA: Result:= $00DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $DB: Result:= $0170; // LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
      $DC: Result:= $00DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $DD: Result:= $00DD; // LATIN CAPITAL LETTER Y WITH ACUTE
      $DE: Result:= $0162; // LATIN CAPITAL LETTER T WITH CEDILLA
      $DF: Result:= $00DF; // LATIN SMALL LETTER SHARP S
      $E0: Result:= $0155; // LATIN SMALL LETTER R WITH ACUTE
      $E1: Result:= $00E1; // LATIN SMALL LETTER A WITH ACUTE
      $E2: Result:= $00E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $E3: Result:= $0103; // LATIN SMALL LETTER A WITH BREVE
      $E4: Result:= $00E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $E5: Result:= $013A; // LATIN SMALL LETTER L WITH ACUTE
      $E6: Result:= $0107; // LATIN SMALL LETTER C WITH ACUTE
      $E7: Result:= $00E7; // LATIN SMALL LETTER C WITH CEDILLA
      $E8: Result:= $010D; // LATIN SMALL LETTER C WITH CARON
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $0119; // LATIN SMALL LETTER E WITH OGONEK
      $EB: Result:= $00EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $EC: Result:= $011B; // LATIN SMALL LETTER E WITH CARON
      $ED: Result:= $00ED; // LATIN SMALL LETTER I WITH ACUTE
      $EE: Result:= $00EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $EF: Result:= $010F; // LATIN SMALL LETTER D WITH CARON
      $F0: Result:= $0111; // LATIN SMALL LETTER D WITH STROKE
      $F1: Result:= $0144; // LATIN SMALL LETTER N WITH ACUTE
      $F2: Result:= $0148; // LATIN SMALL LETTER N WITH CARON
      $F3: Result:= $00F3; // LATIN SMALL LETTER O WITH ACUTE
      $F4: Result:= $00F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $F5: Result:= $0151; // LATIN SMALL LETTER O WITH DOUBLE ACUTE
      $F6: Result:= $00F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $0159; // LATIN SMALL LETTER R WITH CARON
      $F9: Result:= $016F; // LATIN SMALL LETTER U WITH RING ABOVE
      $FA: Result:= $00FA; // LATIN SMALL LETTER U WITH ACUTE
      $FB: Result:= $0171; // LATIN SMALL LETTER U WITH DOUBLE ACUTE
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $00FD; // LATIN SMALL LETTER Y WITH ACUTE
      $FE: Result:= $0163; // LATIN SMALL LETTER T WITH CEDILLA
      $FF: Result:= $02D9; // DOT ABOVE
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharWin1251(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < 128) then
    Result := Chr
  else
    case Chr of
      $80: Result:= $0402; // CYRILLIC CAPITAL LETTER DJE
      $81: Result:= $0403; // CYRILLIC CAPITAL LETTER GJE
      $82: Result:= $201A; // SINGLE LOW-9 QUOTATION MARK
      $83: Result:= $0453; // CYRILLIC SMALL LETTER GJE
      $84: Result:= $201E; // DOUBLE LOW-9 QUOTATION MARK
      $85: Result:= $2026; // HORIZONTAL ELLIPSIS
      $86: Result:= $2020; // DAGGER
      $87: Result:= $2021; // DOUBLE DAGGER
      $88: Result:= $20AC; // EURO SIGN
      $89: Result:= $2030; // PER MILLE SIGN
      $8A: Result:= $0409; // CYRILLIC CAPITAL LETTER LJE
      $8B: Result:= $2039; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $8C: Result:= $040A; // CYRILLIC CAPITAL LETTER NJE
      $8D: Result:= $040C; // CYRILLIC CAPITAL LETTER KJE
      $8E: Result:= $040B; // CYRILLIC CAPITAL LETTER TSHE
      $8F: Result:= $040F; // CYRILLIC CAPITAL LETTER DZHE
      $90: Result:= $0452; // CYRILLIC SMALL LETTER DJE
      $91: Result:= $2018; // LEFT SINGLE QUOTATION MARK
      $92: Result:= $2019; // RIGHT SINGLE QUOTATION MARK
      $93: Result:= $201C; // LEFT DOUBLE QUOTATION MARK
      $94: Result:= $201D; // RIGHT DOUBLE QUOTATION MARK
      $95: Result:= $2022; // BULLET
      $96: Result:= $2013; // EN DASH
      $97: Result:= $2014; // EM DASH
      $99: Result:= $2122; // TRADE MARK SIGN
      $9A: Result:= $0459; // CYRILLIC SMALL LETTER LJE
      $9B: Result:= $203A; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $9C: Result:= $045A; // CYRILLIC SMALL LETTER NJE
      $9D: Result:= $045C; // CYRILLIC SMALL LETTER KJE
      $9E: Result:= $045B; // CYRILLIC SMALL LETTER TSHE
      $9F: Result:= $045F; // CYRILLIC SMALL LETTER DZHE
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $040E; // CYRILLIC CAPITAL LETTER SHORT U
      $A2: Result:= $045E; // CYRILLIC SMALL LETTER SHORT U
      $A3: Result:= $0408; // CYRILLIC CAPITAL LETTER JE
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $0490; // CYRILLIC CAPITAL LETTER GHE WITH UPTURN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $0401; // CYRILLIC CAPITAL LETTER IO
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $0404; // CYRILLIC CAPITAL LETTER UKRAINIAN IE
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $0407; // CYRILLIC CAPITAL LETTER YI
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $0406; // CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
      $B3: Result:= $0456; // CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
      $B4: Result:= $0491; // CYRILLIC SMALL LETTER GHE WITH UPTURN
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $0451; // CYRILLIC SMALL LETTER IO
      $B9: Result:= $2116; // NUMERO SIGN
      $BA: Result:= $0454; // CYRILLIC SMALL LETTER UKRAINIAN IE
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $0458; // CYRILLIC SMALL LETTER JE
      $BD: Result:= $0405; // CYRILLIC CAPITAL LETTER DZE
      $BE: Result:= $0455; // CYRILLIC SMALL LETTER DZE
      $BF: Result:= $0457; // CYRILLIC SMALL LETTER YI
      $C0: Result:= $0410; // CYRILLIC CAPITAL LETTER A
      $C1: Result:= $0411; // CYRILLIC CAPITAL LETTER BE
      $C2: Result:= $0412; // CYRILLIC CAPITAL LETTER VE
      $C3: Result:= $0413; // CYRILLIC CAPITAL LETTER GHE
      $C4: Result:= $0414; // CYRILLIC CAPITAL LETTER DE
      $C5: Result:= $0415; // CYRILLIC CAPITAL LETTER IE
      $C6: Result:= $0416; // CYRILLIC CAPITAL LETTER ZHE
      $C7: Result:= $0417; // CYRILLIC CAPITAL LETTER ZE
      $C8: Result:= $0418; // CYRILLIC CAPITAL LETTER I
      $C9: Result:= $0419; // CYRILLIC CAPITAL LETTER SHORT I
      $CA: Result:= $041A; // CYRILLIC CAPITAL LETTER KA
      $CB: Result:= $041B; // CYRILLIC CAPITAL LETTER EL
      $CC: Result:= $041C; // CYRILLIC CAPITAL LETTER EM
      $CD: Result:= $041D; // CYRILLIC CAPITAL LETTER EN
      $CE: Result:= $041E; // CYRILLIC CAPITAL LETTER O
      $CF: Result:= $041F; // CYRILLIC CAPITAL LETTER PE
      $D0: Result:= $0420; // CYRILLIC CAPITAL LETTER ER
      $D1: Result:= $0421; // CYRILLIC CAPITAL LETTER ES
      $D2: Result:= $0422; // CYRILLIC CAPITAL LETTER TE
      $D3: Result:= $0423; // CYRILLIC CAPITAL LETTER U
      $D4: Result:= $0424; // CYRILLIC CAPITAL LETTER EF
      $D5: Result:= $0425; // CYRILLIC CAPITAL LETTER HA
      $D6: Result:= $0426; // CYRILLIC CAPITAL LETTER TSE
      $D7: Result:= $0427; // CYRILLIC CAPITAL LETTER CHE
      $D8: Result:= $0428; // CYRILLIC CAPITAL LETTER SHA
      $D9: Result:= $0429; // CYRILLIC CAPITAL LETTER SHCHA
      $DA: Result:= $042A; // CYRILLIC CAPITAL LETTER HARD SIGN
      $DB: Result:= $042B; // CYRILLIC CAPITAL LETTER YERU
      $DC: Result:= $042C; // CYRILLIC CAPITAL LETTER SOFT SIGN
      $DD: Result:= $042D; // CYRILLIC CAPITAL LETTER E
      $DE: Result:= $042E; // CYRILLIC CAPITAL LETTER YU
      $DF: Result:= $042F; // CYRILLIC CAPITAL LETTER YA
      $E0: Result:= $0430; // CYRILLIC SMALL LETTER A
      $E1: Result:= $0431; // CYRILLIC SMALL LETTER BE
      $E2: Result:= $0432; // CYRILLIC SMALL LETTER VE
      $E3: Result:= $0433; // CYRILLIC SMALL LETTER GHE
      $E4: Result:= $0434; // CYRILLIC SMALL LETTER DE
      $E5: Result:= $0435; // CYRILLIC SMALL LETTER IE
      $E6: Result:= $0436; // CYRILLIC SMALL LETTER ZHE
      $E7: Result:= $0437; // CYRILLIC SMALL LETTER ZE
      $E8: Result:= $0438; // CYRILLIC SMALL LETTER I
      $E9: Result:= $0439; // CYRILLIC SMALL LETTER SHORT I
      $EA: Result:= $043A; // CYRILLIC SMALL LETTER KA
      $EB: Result:= $043B; // CYRILLIC SMALL LETTER EL
      $EC: Result:= $043C; // CYRILLIC SMALL LETTER EM
      $ED: Result:= $043D; // CYRILLIC SMALL LETTER EN
      $EE: Result:= $043E; // CYRILLIC SMALL LETTER O
      $EF: Result:= $043F; // CYRILLIC SMALL LETTER PE
      $F0: Result:= $0440; // CYRILLIC SMALL LETTER ER
      $F1: Result:= $0441; // CYRILLIC SMALL LETTER ES
      $F2: Result:= $0442; // CYRILLIC SMALL LETTER TE
      $F3: Result:= $0443; // CYRILLIC SMALL LETTER U
      $F4: Result:= $0444; // CYRILLIC SMALL LETTER EF
      $F5: Result:= $0445; // CYRILLIC SMALL LETTER HA
      $F6: Result:= $0446; // CYRILLIC SMALL LETTER TSE
      $F7: Result:= $0447; // CYRILLIC SMALL LETTER CHE
      $F8: Result:= $0448; // CYRILLIC SMALL LETTER SHA
      $F9: Result:= $0449; // CYRILLIC SMALL LETTER SHCHA
      $FA: Result:= $044A; // CYRILLIC SMALL LETTER HARD SIGN
      $FB: Result:= $044B; // CYRILLIC SMALL LETTER YERU
      $FC: Result:= $044C; // CYRILLIC SMALL LETTER SOFT SIGN
      $FD: Result:= $044D; // CYRILLIC SMALL LETTER E
      $FE: Result:= $044E; // CYRILLIC SMALL LETTER YU
      $FF: Result:= $044F; // CYRILLIC SMALL LETTER YA
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharWin1252(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < 128) then
    Result := Chr
  else
    case Chr of
      $80: Result:= $20AC; // EURO SIGN
      $82: Result:= $201A; // SINGLE LOW-9 QUOTATION MARK
      $83: Result:= $0192; // LATIN SMALL LETTER F WITH HOOK
      $84: Result:= $201E; // DOUBLE LOW-9 QUOTATION MARK
      $85: Result:= $2026; // HORIZONTAL ELLIPSIS
      $86: Result:= $2020; // DAGGER
      $87: Result:= $2021; // DOUBLE DAGGER
      $88: Result:= $02C6; // MODIFIER LETTER CIRCUMFLEX ACCENT
      $89: Result:= $2030; // PER MILLE SIGN
      $8A: Result:= $0160; // LATIN CAPITAL LETTER S WITH CARON
      $8B: Result:= $2039; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $8C: Result:= $0152; // LATIN CAPITAL LIGATURE OE
      $8E: Result:= $017D; // LATIN CAPITAL LETTER Z WITH CARON
      $91: Result:= $2018; // LEFT SINGLE QUOTATION MARK
      $92: Result:= $2019; // RIGHT SINGLE QUOTATION MARK
      $93: Result:= $201C; // LEFT DOUBLE QUOTATION MARK
      $94: Result:= $201D; // RIGHT DOUBLE QUOTATION MARK
      $95: Result:= $2022; // BULLET
      $96: Result:= $2013; // EN DASH
      $97: Result:= $2014; // EM DASH
      $98: Result:= $02DC; // SMALL TILDE
      $99: Result:= $2122; // TRADE MARK SIGN
      $9A: Result:= $0161; // LATIN SMALL LETTER S WITH CARON
      $9B: Result:= $203A; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $9C: Result:= $0153; // LATIN SMALL LIGATURE OE
      $9E: Result:= $017E; // LATIN SMALL LETTER Z WITH CARON
      $9F: Result:= $0178; // LATIN CAPITAL LETTER Y WITH DIAERESIS
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $00A1; // INVERTED EXCLAMATION MARK
      $A2: Result:= $00A2; // CENT SIGN
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $00A5; // YEN SIGN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $00AA; // FEMININE ORDINAL INDICATOR
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $00AF; // MACRON
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $00B9; // SUPERSCRIPT ONE
      $BA: Result:= $00BA; // MASCULINE ORDINAL INDICATOR
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $00BC; // VULGAR FRACTION ONE QUARTER
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BE: Result:= $00BE; // VULGAR FRACTION THREE QUARTERS
      $BF: Result:= $00BF; // INVERTED QUESTION MARK
      $C0: Result:= $00C0; // LATIN CAPITAL LETTER A WITH GRAVE
      $C1: Result:= $00C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $C2: Result:= $00C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $C3: Result:= $00C3; // LATIN CAPITAL LETTER A WITH TILDE
      $C4: Result:= $00C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $C5: Result:= $00C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $C6: Result:= $00C6; // LATIN CAPITAL LETTER AE
      $C7: Result:= $00C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $C8: Result:= $00C8; // LATIN CAPITAL LETTER E WITH GRAVE
      $C9: Result:= $00C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $CA: Result:= $00CA; // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $CB: Result:= $00CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $CC: Result:= $00CC; // LATIN CAPITAL LETTER I WITH GRAVE
      $CD: Result:= $00CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $CE: Result:= $00CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $CF: Result:= $00CF; // LATIN CAPITAL LETTER I WITH DIAERESIS
      $D0: Result:= $00D0; // LATIN CAPITAL LETTER ETH
      $D1: Result:= $00D1; // LATIN CAPITAL LETTER N WITH TILDE
      $D2: Result:= $00D2; // LATIN CAPITAL LETTER O WITH GRAVE
      $D3: Result:= $00D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $D4: Result:= $00D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $D5: Result:= $00D5; // LATIN CAPITAL LETTER O WITH TILDE
      $D6: Result:= $00D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $00D8; // LATIN CAPITAL LETTER O WITH STROKE
      $D9: Result:= $00D9; // LATIN CAPITAL LETTER U WITH GRAVE
      $DA: Result:= $00DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $DB: Result:= $00DB; // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $DC: Result:= $00DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $DD: Result:= $00DD; // LATIN CAPITAL LETTER Y WITH ACUTE
      $DE: Result:= $00DE; // LATIN CAPITAL LETTER THORN
      $DF: Result:= $00DF; // LATIN SMALL LETTER SHARP S
      $E0: Result:= $00E0; // LATIN SMALL LETTER A WITH GRAVE
      $E1: Result:= $00E1; // LATIN SMALL LETTER A WITH ACUTE
      $E2: Result:= $00E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $E3: Result:= $00E3; // LATIN SMALL LETTER A WITH TILDE
      $E4: Result:= $00E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $E5: Result:= $00E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $E6: Result:= $00E6; // LATIN SMALL LETTER AE
      $E7: Result:= $00E7; // LATIN SMALL LETTER C WITH CEDILLA
      $E8: Result:= $00E8; // LATIN SMALL LETTER E WITH GRAVE
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $00EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $EB: Result:= $00EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $EC: Result:= $00EC; // LATIN SMALL LETTER I WITH GRAVE
      $ED: Result:= $00ED; // LATIN SMALL LETTER I WITH ACUTE
      $EE: Result:= $00EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $EF: Result:= $00EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $F0: Result:= $00F0; // LATIN SMALL LETTER ETH
      $F1: Result:= $00F1; // LATIN SMALL LETTER N WITH TILDE
      $F2: Result:= $00F2; // LATIN SMALL LETTER O WITH GRAVE
      $F3: Result:= $00F3; // LATIN SMALL LETTER O WITH ACUTE
      $F4: Result:= $00F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $F5: Result:= $00F5; // LATIN SMALL LETTER O WITH TILDE
      $F6: Result:= $00F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $00F8; // LATIN SMALL LETTER O WITH STROKE
      $F9: Result:= $00F9; // LATIN SMALL LETTER U WITH GRAVE
      $FA: Result:= $00FA; // LATIN SMALL LETTER U WITH ACUTE
      $FB: Result:= $00FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $00FD; // LATIN SMALL LETTER Y WITH ACUTE
      $FE: Result:= $00FE; // LATIN SMALL LETTER THORN
      $FF: Result:= $00FF; // LATIN SMALL LETTER Y WITH DIAERESIS
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharWin1253(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < 128) then
    Result := Chr
  else
    case Chr of
      $80: Result:= $20AC; // EURO SIGN
      $82: Result:= $201A; // SINGLE LOW-9 QUOTATION MARK
      $83: Result:= $0192; // LATIN SMALL LETTER F WITH HOOK
      $84: Result:= $201E; // DOUBLE LOW-9 QUOTATION MARK
      $85: Result:= $2026; // HORIZONTAL ELLIPSIS
      $86: Result:= $2020; // DAGGER
      $87: Result:= $2021; // DOUBLE DAGGER
      $89: Result:= $2030; // PER MILLE SIGN
      $8B: Result:= $2039; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $91: Result:= $2018; // LEFT SINGLE QUOTATION MARK
      $92: Result:= $2019; // RIGHT SINGLE QUOTATION MARK
      $93: Result:= $201C; // LEFT DOUBLE QUOTATION MARK
      $94: Result:= $201D; // RIGHT DOUBLE QUOTATION MARK
      $95: Result:= $2022; // BULLET
      $96: Result:= $2013; // EN DASH
      $97: Result:= $2014; // EM DASH
      $99: Result:= $2122; // TRADE MARK SIGN
      $9B: Result:= $203A; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $0385; // GREEK DIALYTIKA TONOS
      $A2: Result:= $0386; // GREEK CAPITAL LETTER ALPHA WITH TONOS
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $00A5; // YEN SIGN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $2015; // HORIZONTAL BAR
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $0384; // GREEK TONOS
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $0388; // GREEK CAPITAL LETTER EPSILON WITH TONOS
      $B9: Result:= $0389; // GREEK CAPITAL LETTER ETA WITH TONOS
      $BA: Result:= $038A; // GREEK CAPITAL LETTER IOTA WITH TONOS
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $038C; // GREEK CAPITAL LETTER OMICRON WITH TONOS
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BE: Result:= $038E; // GREEK CAPITAL LETTER UPSILON WITH TONOS
      $BF: Result:= $038F; // GREEK CAPITAL LETTER OMEGA WITH TONOS
      $C0: Result:= $0390; // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
      $C1: Result:= $0391; // GREEK CAPITAL LETTER ALPHA
      $C2: Result:= $0392; // GREEK CAPITAL LETTER BETA
      $C3: Result:= $0393; // GREEK CAPITAL LETTER GAMMA
      $C4: Result:= $0394; // GREEK CAPITAL LETTER DELTA
      $C5: Result:= $0395; // GREEK CAPITAL LETTER EPSILON
      $C6: Result:= $0396; // GREEK CAPITAL LETTER ZETA
      $C7: Result:= $0397; // GREEK CAPITAL LETTER ETA
      $C8: Result:= $0398; // GREEK CAPITAL LETTER THETA
      $C9: Result:= $0399; // GREEK CAPITAL LETTER IOTA
      $CA: Result:= $039A; // GREEK CAPITAL LETTER KAPPA
      $CB: Result:= $039B; // GREEK CAPITAL LETTER LAMDA
      $CC: Result:= $039C; // GREEK CAPITAL LETTER MU
      $CD: Result:= $039D; // GREEK CAPITAL LETTER NU
      $CE: Result:= $039E; // GREEK CAPITAL LETTER XI
      $CF: Result:= $039F; // GREEK CAPITAL LETTER OMICRON
      $D0: Result:= $03A0; // GREEK CAPITAL LETTER PI
      $D1: Result:= $03A1; // GREEK CAPITAL LETTER RHO
      $D3: Result:= $03A3; // GREEK CAPITAL LETTER SIGMA
      $D4: Result:= $03A4; // GREEK CAPITAL LETTER TAU
      $D5: Result:= $03A5; // GREEK CAPITAL LETTER UPSILON
      $D6: Result:= $03A6; // GREEK CAPITAL LETTER PHI
      $D7: Result:= $03A7; // GREEK CAPITAL LETTER CHI
      $D8: Result:= $03A8; // GREEK CAPITAL LETTER PSI
      $D9: Result:= $03A9; // GREEK CAPITAL LETTER OMEGA
      $DA: Result:= $03AA; // GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
      $DB: Result:= $03AB; // GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
      $DC: Result:= $03AC; // GREEK SMALL LETTER ALPHA WITH TONOS
      $DD: Result:= $03AD; // GREEK SMALL LETTER EPSILON WITH TONOS
      $DE: Result:= $03AE; // GREEK SMALL LETTER ETA WITH TONOS
      $DF: Result:= $03AF; // GREEK SMALL LETTER IOTA WITH TONOS
      $E0: Result:= $03B0; // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
      $E1: Result:= $03B1; // GREEK SMALL LETTER ALPHA
      $E2: Result:= $03B2; // GREEK SMALL LETTER BETA
      $E3: Result:= $03B3; // GREEK SMALL LETTER GAMMA
      $E4: Result:= $03B4; // GREEK SMALL LETTER DELTA
      $E5: Result:= $03B5; // GREEK SMALL LETTER EPSILON
      $E6: Result:= $03B6; // GREEK SMALL LETTER ZETA
      $E7: Result:= $03B7; // GREEK SMALL LETTER ETA
      $E8: Result:= $03B8; // GREEK SMALL LETTER THETA
      $E9: Result:= $03B9; // GREEK SMALL LETTER IOTA
      $EA: Result:= $03BA; // GREEK SMALL LETTER KAPPA
      $EB: Result:= $03BB; // GREEK SMALL LETTER LAMDA
      $EC: Result:= $03BC; // GREEK SMALL LETTER MU
      $ED: Result:= $03BD; // GREEK SMALL LETTER NU
      $EE: Result:= $03BE; // GREEK SMALL LETTER XI
      $EF: Result:= $03BF; // GREEK SMALL LETTER OMICRON
      $F0: Result:= $03C0; // GREEK SMALL LETTER PI
      $F1: Result:= $03C1; // GREEK SMALL LETTER RHO
      $F2: Result:= $03C2; // GREEK SMALL LETTER FINAL SIGMA
      $F3: Result:= $03C3; // GREEK SMALL LETTER SIGMA
      $F4: Result:= $03C4; // GREEK SMALL LETTER TAU
      $F5: Result:= $03C5; // GREEK SMALL LETTER UPSILON
      $F6: Result:= $03C6; // GREEK SMALL LETTER PHI
      $F7: Result:= $03C7; // GREEK SMALL LETTER CHI
      $F8: Result:= $03C8; // GREEK SMALL LETTER PSI
      $F9: Result:= $03C9; // GREEK SMALL LETTER OMEGA
      $FA: Result:= $03CA; // GREEK SMALL LETTER IOTA WITH DIALYTIKA
      $FB: Result:= $03CB; // GREEK SMALL LETTER UPSILON WITH DIALYTIKA
      $FC: Result:= $03CC; // GREEK SMALL LETTER OMICRON WITH TONOS
      $FD: Result:= $03CD; // GREEK SMALL LETTER UPSILON WITH TONOS
      $FE: Result:= $03CE; // GREEK SMALL LETTER OMEGA WITH TONOS
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharWin1254(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < 128) then
    Result := Chr
  else
    case Chr of
      $80: Result:= $20AC; // EURO SIGN
      $82: Result:= $201A; // SINGLE LOW-9 QUOTATION MARK
      $83: Result:= $0192; // LATIN SMALL LETTER F WITH HOOK
      $84: Result:= $201E; // DOUBLE LOW-9 QUOTATION MARK
      $85: Result:= $2026; // HORIZONTAL ELLIPSIS
      $86: Result:= $2020; // DAGGER
      $87: Result:= $2021; // DOUBLE DAGGER
      $88: Result:= $02C6; // MODIFIER LETTER CIRCUMFLEX ACCENT
      $89: Result:= $2030; // PER MILLE SIGN
      $8A: Result:= $0160; // LATIN CAPITAL LETTER S WITH CARON
      $8B: Result:= $2039; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $8C: Result:= $0152; // LATIN CAPITAL LIGATURE OE
      $91: Result:= $2018; // LEFT SINGLE QUOTATION MARK
      $92: Result:= $2019; // RIGHT SINGLE QUOTATION MARK
      $93: Result:= $201C; // LEFT DOUBLE QUOTATION MARK
      $94: Result:= $201D; // RIGHT DOUBLE QUOTATION MARK
      $95: Result:= $2022; // BULLET
      $96: Result:= $2013; // EN DASH
      $97: Result:= $2014; // EM DASH
      $98: Result:= $02DC; // SMALL TILDE
      $99: Result:= $2122; // TRADE MARK SIGN
      $9A: Result:= $0161; // LATIN SMALL LETTER S WITH CARON
      $9B: Result:= $203A; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $9C: Result:= $0153; // LATIN SMALL LIGATURE OE
      $9F: Result:= $0178; // LATIN CAPITAL LETTER Y WITH DIAERESIS
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $00A1; // INVERTED EXCLAMATION MARK
      $A2: Result:= $00A2; // CENT SIGN
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $00A5; // YEN SIGN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $00AA; // FEMININE ORDINAL INDICATOR
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $00AF; // MACRON
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $00B9; // SUPERSCRIPT ONE
      $BA: Result:= $00BA; // MASCULINE ORDINAL INDICATOR
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $00BC; // VULGAR FRACTION ONE QUARTER
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BE: Result:= $00BE; // VULGAR FRACTION THREE QUARTERS
      $BF: Result:= $00BF; // INVERTED QUESTION MARK
      $C0: Result:= $00C0; // LATIN CAPITAL LETTER A WITH GRAVE
      $C1: Result:= $00C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $C2: Result:= $00C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $C3: Result:= $00C3; // LATIN CAPITAL LETTER A WITH TILDE
      $C4: Result:= $00C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $C5: Result:= $00C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $C6: Result:= $00C6; // LATIN CAPITAL LETTER AE
      $C7: Result:= $00C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $C8: Result:= $00C8; // LATIN CAPITAL LETTER E WITH GRAVE
      $C9: Result:= $00C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $CA: Result:= $00CA; // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $CB: Result:= $00CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $CC: Result:= $00CC; // LATIN CAPITAL LETTER I WITH GRAVE
      $CD: Result:= $00CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $CE: Result:= $00CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $CF: Result:= $00CF; // LATIN CAPITAL LETTER I WITH DIAERESIS
      $D0: Result:= $011E; // LATIN CAPITAL LETTER G WITH BREVE
      $D1: Result:= $00D1; // LATIN CAPITAL LETTER N WITH TILDE
      $D2: Result:= $00D2; // LATIN CAPITAL LETTER O WITH GRAVE
      $D3: Result:= $00D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $D4: Result:= $00D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $D5: Result:= $00D5; // LATIN CAPITAL LETTER O WITH TILDE
      $D6: Result:= $00D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $00D8; // LATIN CAPITAL LETTER O WITH STROKE
      $D9: Result:= $00D9; // LATIN CAPITAL LETTER U WITH GRAVE
      $DA: Result:= $00DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $DB: Result:= $00DB; // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $DC: Result:= $00DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $DD: Result:= $0130; // LATIN CAPITAL LETTER I WITH DOT ABOVE
      $DE: Result:= $015E; // LATIN CAPITAL LETTER S WITH CEDILLA
      $DF: Result:= $00DF; // LATIN SMALL LETTER SHARP S
      $E0: Result:= $00E0; // LATIN SMALL LETTER A WITH GRAVE
      $E1: Result:= $00E1; // LATIN SMALL LETTER A WITH ACUTE
      $E2: Result:= $00E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $E3: Result:= $00E3; // LATIN SMALL LETTER A WITH TILDE
      $E4: Result:= $00E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $E5: Result:= $00E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $E6: Result:= $00E6; // LATIN SMALL LETTER AE
      $E7: Result:= $00E7; // LATIN SMALL LETTER C WITH CEDILLA
      $E8: Result:= $00E8; // LATIN SMALL LETTER E WITH GRAVE
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $00EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $EB: Result:= $00EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $EC: Result:= $00EC; // LATIN SMALL LETTER I WITH GRAVE
      $ED: Result:= $00ED; // LATIN SMALL LETTER I WITH ACUTE
      $EE: Result:= $00EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $EF: Result:= $00EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $F0: Result:= $011F; // LATIN SMALL LETTER G WITH BREVE
      $F1: Result:= $00F1; // LATIN SMALL LETTER N WITH TILDE
      $F2: Result:= $00F2; // LATIN SMALL LETTER O WITH GRAVE
      $F3: Result:= $00F3; // LATIN SMALL LETTER O WITH ACUTE
      $F4: Result:= $00F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $F5: Result:= $00F5; // LATIN SMALL LETTER O WITH TILDE
      $F6: Result:= $00F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $00F8; // LATIN SMALL LETTER O WITH STROKE
      $F9: Result:= $00F9; // LATIN SMALL LETTER U WITH GRAVE
      $FA: Result:= $00FA; // LATIN SMALL LETTER U WITH ACUTE
      $FB: Result:= $00FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $0131; // LATIN SMALL LETTER DOTLESS I
      $FE: Result:= $015F; // LATIN SMALL LETTER S WITH CEDILLA
      $FF: Result:= $00FF; // LATIN SMALL LETTER Y WITH DIAERESIS
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharWin1255(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < 128) then
    Result := Chr
  else
    case Chr of
      $80: Result:= $20AC; // EURO SIGN
      $82: Result:= $201A; // SINGLE LOW-9 QUOTATION MARK
      $83: Result:= $0192; // LATIN SMALL LETTER F WITH HOOK
      $84: Result:= $201E; // DOUBLE LOW-9 QUOTATION MARK
      $85: Result:= $2026; // HORIZONTAL ELLIPSIS
      $86: Result:= $2020; // DAGGER
      $87: Result:= $2021; // DOUBLE DAGGER
      $88: Result:= $02C6; // MODIFIER LETTER CIRCUMFLEX ACCENT
      $89: Result:= $2030; // PER MILLE SIGN
      $8B: Result:= $2039; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $91: Result:= $2018; // LEFT SINGLE QUOTATION MARK
      $92: Result:= $2019; // RIGHT SINGLE QUOTATION MARK
      $93: Result:= $201C; // LEFT DOUBLE QUOTATION MARK
      $94: Result:= $201D; // RIGHT DOUBLE QUOTATION MARK
      $95: Result:= $2022; // BULLET
      $96: Result:= $2013; // EN DASH
      $97: Result:= $2014; // EM DASH
      $98: Result:= $02DC; // SMALL TILDE
      $99: Result:= $2122; // TRADE MARK SIGN
      $9B: Result:= $203A; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $00A1; // INVERTED EXCLAMATION MARK
      $A2: Result:= $00A2; // CENT SIGN
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $20AA; // NEW SHEQEL SIGN
      $A5: Result:= $00A5; // YEN SIGN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $00D7; // MULTIPLICATION SIGN
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $00AF; // MACRON
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $00B9; // SUPERSCRIPT ONE
      $BA: Result:= $00F7; // DIVISION SIGN
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $00BC; // VULGAR FRACTION ONE QUARTER
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BE: Result:= $00BE; // VULGAR FRACTION THREE QUARTERS
      $BF: Result:= $00BF; // INVERTED QUESTION MARK
      $C0: Result:= $05B0; // HEBREW POINT SHEVA
      $C1: Result:= $05B1; // HEBREW POINT HATAF SEGOL
      $C2: Result:= $05B2; // HEBREW POINT HATAF PATAH
      $C3: Result:= $05B3; // HEBREW POINT HATAF QAMATS
      $C4: Result:= $05B4; // HEBREW POINT HIRIQ
      $C5: Result:= $05B5; // HEBREW POINT TSERE
      $C6: Result:= $05B6; // HEBREW POINT SEGOL
      $C7: Result:= $05B7; // HEBREW POINT PATAH
      $C8: Result:= $05B8; // HEBREW POINT QAMATS
      $C9: Result:= $05B9; // HEBREW POINT HOLAM
      $CB: Result:= $05BB; // HEBREW POINT QUBUTS
      $CC: Result:= $05BC; // HEBREW POINT DAGESH OR MAPIQ
      $CD: Result:= $05BD; // HEBREW POINT METEG
      $CE: Result:= $05BE; // HEBREW PUNCTUATION MAQAF
      $CF: Result:= $05BF; // HEBREW POINT RAFE
      $D0: Result:= $05C0; // HEBREW PUNCTUATION PASEQ
      $D1: Result:= $05C1; // HEBREW POINT SHIN DOT
      $D2: Result:= $05C2; // HEBREW POINT SIN DOT
      $D3: Result:= $05C3; // HEBREW PUNCTUATION SOF PASUQ
      $D4: Result:= $05F0; // HEBREW LIGATURE YIDDISH DOUBLE VAV
      $D5: Result:= $05F1; // HEBREW LIGATURE YIDDISH VAV YOD
      $D6: Result:= $05F2; // HEBREW LIGATURE YIDDISH DOUBLE YOD
      $D7: Result:= $05F3; // HEBREW PUNCTUATION GERESH
      $D8: Result:= $05F4; // HEBREW PUNCTUATION GERSHAYIM
      $E0: Result:= $05D0; // HEBREW LETTER ALEF
      $E1: Result:= $05D1; // HEBREW LETTER BET
      $E2: Result:= $05D2; // HEBREW LETTER GIMEL
      $E3: Result:= $05D3; // HEBREW LETTER DALET
      $E4: Result:= $05D4; // HEBREW LETTER HE
      $E5: Result:= $05D5; // HEBREW LETTER VAV
      $E6: Result:= $05D6; // HEBREW LETTER ZAYIN
      $E7: Result:= $05D7; // HEBREW LETTER HET
      $E8: Result:= $05D8; // HEBREW LETTER TET
      $E9: Result:= $05D9; // HEBREW LETTER YOD
      $EA: Result:= $05DA; // HEBREW LETTER FINAL KAF
      $EB: Result:= $05DB; // HEBREW LETTER KAF
      $EC: Result:= $05DC; // HEBREW LETTER LAMED
      $ED: Result:= $05DD; // HEBREW LETTER FINAL MEM
      $EE: Result:= $05DE; // HEBREW LETTER MEM
      $EF: Result:= $05DF; // HEBREW LETTER FINAL NUN
      $F0: Result:= $05E0; // HEBREW LETTER NUN
      $F1: Result:= $05E1; // HEBREW LETTER SAMEKH
      $F2: Result:= $05E2; // HEBREW LETTER AYIN
      $F3: Result:= $05E3; // HEBREW LETTER FINAL PE
      $F4: Result:= $05E4; // HEBREW LETTER PE
      $F5: Result:= $05E5; // HEBREW LETTER FINAL TSADI
      $F6: Result:= $05E6; // HEBREW LETTER TSADI
      $F7: Result:= $05E7; // HEBREW LETTER QOF
      $F8: Result:= $05E8; // HEBREW LETTER RESH
      $F9: Result:= $05E9; // HEBREW LETTER SHIN
      $FA: Result:= $05EA; // HEBREW LETTER TAV
      $FD: Result:= $200E; // LEFT-TO-RIGHT MARK
      $FE: Result:= $200F; // RIGHT-TO-LEFT MARK
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharWin1256(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < 128) then
    Result := Chr
  else
    case Chr of
      $80: Result:= $20AC; // EURO SIGN
      $81: Result:= $067E; // ARABIC LETTER PEH
      $82: Result:= $201A; // SINGLE LOW-9 QUOTATION MARK
      $83: Result:= $0192; // LATIN SMALL LETTER F WITH HOOK
      $84: Result:= $201E; // DOUBLE LOW-9 QUOTATION MARK
      $85: Result:= $2026; // HORIZONTAL ELLIPSIS
      $86: Result:= $2020; // DAGGER
      $87: Result:= $2021; // DOUBLE DAGGER
      $88: Result:= $02C6; // MODIFIER LETTER CIRCUMFLEX ACCENT
      $89: Result:= $2030; // PER MILLE SIGN
      $8A: Result:= $0679; // ARABIC LETTER TTEH
      $8B: Result:= $2039; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $8C: Result:= $0152; // LATIN CAPITAL LIGATURE OE
      $8D: Result:= $0686; // ARABIC LETTER TCHEH
      $8E: Result:= $0698; // ARABIC LETTER JEH
      $8F: Result:= $0688; // ARABIC LETTER DDAL
      $90: Result:= $06AF; // ARABIC LETTER GAF
      $91: Result:= $2018; // LEFT SINGLE QUOTATION MARK
      $92: Result:= $2019; // RIGHT SINGLE QUOTATION MARK
      $93: Result:= $201C; // LEFT DOUBLE QUOTATION MARK
      $94: Result:= $201D; // RIGHT DOUBLE QUOTATION MARK
      $95: Result:= $2022; // BULLET
      $96: Result:= $2013; // EN DASH
      $97: Result:= $2014; // EM DASH
      $98: Result:= $06A9; // ARABIC LETTER KEHEH
      $99: Result:= $2122; // TRADE MARK SIGN
      $9A: Result:= $0691; // ARABIC LETTER RREH
      $9B: Result:= $203A; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $9C: Result:= $0153; // LATIN SMALL LIGATURE OE
      $9D: Result:= $200C; // ZERO WIDTH NON-JOINER
      $9E: Result:= $200D; // ZERO WIDTH JOINER
      $9F: Result:= $06BA; // ARABIC LETTER NOON GHUNNA
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $060C; // ARABIC COMMA
      $A2: Result:= $00A2; // CENT SIGN
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $00A5; // YEN SIGN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $06BE; // ARABIC LETTER HEH DOACHASHMEE
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $00AF; // MACRON
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $00B9; // SUPERSCRIPT ONE
      $BA: Result:= $061B; // ARABIC SEMICOLON
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $00BC; // VULGAR FRACTION ONE QUARTER
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BE: Result:= $00BE; // VULGAR FRACTION THREE QUARTERS
      $BF: Result:= $061F; // ARABIC QUESTION MARK
      $C0: Result:= $06C1; // ARABIC LETTER HEH GOAL
      $C1: Result:= $0621; // ARABIC LETTER HAMZA
      $C2: Result:= $0622; // ARABIC LETTER ALEF WITH MADDA ABOVE
      $C3: Result:= $0623; // ARABIC LETTER ALEF WITH HAMZA ABOVE
      $C4: Result:= $0624; // ARABIC LETTER WAW WITH HAMZA ABOVE
      $C5: Result:= $0625; // ARABIC LETTER ALEF WITH HAMZA BELOW
      $C6: Result:= $0626; // ARABIC LETTER YEH WITH HAMZA ABOVE
      $C7: Result:= $0627; // ARABIC LETTER ALEF
      $C8: Result:= $0628; // ARABIC LETTER BEH
      $C9: Result:= $0629; // ARABIC LETTER TEH MARBUTA
      $CA: Result:= $062A; // ARABIC LETTER TEH
      $CB: Result:= $062B; // ARABIC LETTER THEH
      $CC: Result:= $062C; // ARABIC LETTER JEEM
      $CD: Result:= $062D; // ARABIC LETTER HAH
      $CE: Result:= $062E; // ARABIC LETTER KHAH
      $CF: Result:= $062F; // ARABIC LETTER DAL
      $D0: Result:= $0630; // ARABIC LETTER THAL
      $D1: Result:= $0631; // ARABIC LETTER REH
      $D2: Result:= $0632; // ARABIC LETTER ZAIN
      $D3: Result:= $0633; // ARABIC LETTER SEEN
      $D4: Result:= $0634; // ARABIC LETTER SHEEN
      $D5: Result:= $0635; // ARABIC LETTER SAD
      $D6: Result:= $0636; // ARABIC LETTER DAD
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $0637; // ARABIC LETTER TAH
      $D9: Result:= $0638; // ARABIC LETTER ZAH
      $DA: Result:= $0639; // ARABIC LETTER AIN
      $DB: Result:= $063A; // ARABIC LETTER GHAIN
      $DC: Result:= $0640; // ARABIC TATWEEL
      $DD: Result:= $0641; // ARABIC LETTER FEH
      $DE: Result:= $0642; // ARABIC LETTER QAF
      $DF: Result:= $0643; // ARABIC LETTER KAF
      $E0: Result:= $00E0; // LATIN SMALL LETTER A WITH GRAVE
      $E1: Result:= $0644; // ARABIC LETTER LAM
      $E2: Result:= $00E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $E3: Result:= $0645; // ARABIC LETTER MEEM
      $E4: Result:= $0646; // ARABIC LETTER NOON
      $E5: Result:= $0647; // ARABIC LETTER HEH
      $E6: Result:= $0648; // ARABIC LETTER WAW
      $E7: Result:= $00E7; // LATIN SMALL LETTER C WITH CEDILLA
      $E8: Result:= $00E8; // LATIN SMALL LETTER E WITH GRAVE
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $00EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $EB: Result:= $00EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $EC: Result:= $0649; // ARABIC LETTER ALEF MAKSURA
      $ED: Result:= $064A; // ARABIC LETTER YEH
      $EE: Result:= $00EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $EF: Result:= $00EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $F0: Result:= $064B; // ARABIC FATHATAN
      $F1: Result:= $064C; // ARABIC DAMMATAN
      $F2: Result:= $064D; // ARABIC KASRATAN
      $F3: Result:= $064E; // ARABIC FATHA
      $F4: Result:= $00F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $F5: Result:= $064F; // ARABIC DAMMA
      $F6: Result:= $0650; // ARABIC KASRA
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $0651; // ARABIC SHADDA
      $F9: Result:= $00F9; // LATIN SMALL LETTER U WITH GRAVE
      $FA: Result:= $0652; // ARABIC SUKUN
      $FB: Result:= $00FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $200E; // LEFT-TO-RIGHT MARK
      $FE: Result:= $200F; // RIGHT-TO-LEFT MARK
      $FF: Result:= $06D2; // ARABIC LETTER YEH BARREE
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharWin1257(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < 128) then
    Result := Chr
  else
    case Chr of
      $80: Result:= $20AC; // EURO SIGN
      $82: Result:= $201A; // SINGLE LOW-9 QUOTATION MARK
      $84: Result:= $201E; // DOUBLE LOW-9 QUOTATION MARK
      $85: Result:= $2026; // HORIZONTAL ELLIPSIS
      $86: Result:= $2020; // DAGGER
      $87: Result:= $2021; // DOUBLE DAGGER
      $89: Result:= $2030; // PER MILLE SIGN
      $8B: Result:= $2039; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $8D: Result:= $00A8; // DIAERESIS
      $8E: Result:= $02C7; // CARON
      $8F: Result:= $00B8; // CEDILLA
      $91: Result:= $2018; // LEFT SINGLE QUOTATION MARK
      $92: Result:= $2019; // RIGHT SINGLE QUOTATION MARK
      $93: Result:= $201C; // LEFT DOUBLE QUOTATION MARK
      $94: Result:= $201D; // RIGHT DOUBLE QUOTATION MARK
      $95: Result:= $2022; // BULLET
      $96: Result:= $2013; // EN DASH
      $97: Result:= $2014; // EM DASH
      $99: Result:= $2122; // TRADE MARK SIGN
      $9B: Result:= $203A; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $9D: Result:= $00AF; // MACRON
      $9E: Result:= $02DB; // OGONEK
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A2: Result:= $00A2; // CENT SIGN
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00D8; // LATIN CAPITAL LETTER O WITH STROKE
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $0156; // LATIN CAPITAL LETTER R WITH CEDILLA
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $00C6; // LATIN CAPITAL LETTER AE
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $00F8; // LATIN SMALL LETTER O WITH STROKE
      $B9: Result:= $00B9; // SUPERSCRIPT ONE
      $BA: Result:= $0157; // LATIN SMALL LETTER R WITH CEDILLA
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $00BC; // VULGAR FRACTION ONE QUARTER
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BE: Result:= $00BE; // VULGAR FRACTION THREE QUARTERS
      $BF: Result:= $00E6; // LATIN SMALL LETTER AE
      $C0: Result:= $0104; // LATIN CAPITAL LETTER A WITH OGONEK
      $C1: Result:= $012E; // LATIN CAPITAL LETTER I WITH OGONEK
      $C2: Result:= $0100; // LATIN CAPITAL LETTER A WITH MACRON
      $C3: Result:= $0106; // LATIN CAPITAL LETTER C WITH ACUTE
      $C4: Result:= $00C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $C5: Result:= $00C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $C6: Result:= $0118; // LATIN CAPITAL LETTER E WITH OGONEK
      $C7: Result:= $0112; // LATIN CAPITAL LETTER E WITH MACRON
      $C8: Result:= $010C; // LATIN CAPITAL LETTER C WITH CARON
      $C9: Result:= $00C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $CA: Result:= $0179; // LATIN CAPITAL LETTER Z WITH ACUTE
      $CB: Result:= $0116; // LATIN CAPITAL LETTER E WITH DOT ABOVE
      $CC: Result:= $0122; // LATIN CAPITAL LETTER G WITH CEDILLA
      $CD: Result:= $0136; // LATIN CAPITAL LETTER K WITH CEDILLA
      $CE: Result:= $012A; // LATIN CAPITAL LETTER I WITH MACRON
      $CF: Result:= $013B; // LATIN CAPITAL LETTER L WITH CEDILLA
      $D0: Result:= $0160; // LATIN CAPITAL LETTER S WITH CARON
      $D1: Result:= $0143; // LATIN CAPITAL LETTER N WITH ACUTE
      $D2: Result:= $0145; // LATIN CAPITAL LETTER N WITH CEDILLA
      $D3: Result:= $00D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $D4: Result:= $014C; // LATIN CAPITAL LETTER O WITH MACRON
      $D5: Result:= $00D5; // LATIN CAPITAL LETTER O WITH TILDE
      $D6: Result:= $00D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $0172; // LATIN CAPITAL LETTER U WITH OGONEK
      $D9: Result:= $0141; // LATIN CAPITAL LETTER L WITH STROKE
      $DA: Result:= $015A; // LATIN CAPITAL LETTER S WITH ACUTE
      $DB: Result:= $016A; // LATIN CAPITAL LETTER U WITH MACRON
      $DC: Result:= $00DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $DD: Result:= $017B; // LATIN CAPITAL LETTER Z WITH DOT ABOVE
      $DE: Result:= $017D; // LATIN CAPITAL LETTER Z WITH CARON
      $DF: Result:= $00DF; // LATIN SMALL LETTER SHARP S
      $E0: Result:= $0105; // LATIN SMALL LETTER A WITH OGONEK
      $E1: Result:= $012F; // LATIN SMALL LETTER I WITH OGONEK
      $E2: Result:= $0101; // LATIN SMALL LETTER A WITH MACRON
      $E3: Result:= $0107; // LATIN SMALL LETTER C WITH ACUTE
      $E4: Result:= $00E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $E5: Result:= $00E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $E6: Result:= $0119; // LATIN SMALL LETTER E WITH OGONEK
      $E7: Result:= $0113; // LATIN SMALL LETTER E WITH MACRON
      $E8: Result:= $010D; // LATIN SMALL LETTER C WITH CARON
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $017A; // LATIN SMALL LETTER Z WITH ACUTE
      $EB: Result:= $0117; // LATIN SMALL LETTER E WITH DOT ABOVE
      $EC: Result:= $0123; // LATIN SMALL LETTER G WITH CEDILLA
      $ED: Result:= $0137; // LATIN SMALL LETTER K WITH CEDILLA
      $EE: Result:= $012B; // LATIN SMALL LETTER I WITH MACRON
      $EF: Result:= $013C; // LATIN SMALL LETTER L WITH CEDILLA
      $F0: Result:= $0161; // LATIN SMALL LETTER S WITH CARON
      $F1: Result:= $0144; // LATIN SMALL LETTER N WITH ACUTE
      $F2: Result:= $0146; // LATIN SMALL LETTER N WITH CEDILLA
      $F3: Result:= $00F3; // LATIN SMALL LETTER O WITH ACUTE
      $F4: Result:= $014D; // LATIN SMALL LETTER O WITH MACRON
      $F5: Result:= $00F5; // LATIN SMALL LETTER O WITH TILDE
      $F6: Result:= $00F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $0173; // LATIN SMALL LETTER U WITH OGONEK
      $F9: Result:= $0142; // LATIN SMALL LETTER L WITH STROKE
      $FA: Result:= $015B; // LATIN SMALL LETTER S WITH ACUTE
      $FB: Result:= $016B; // LATIN SMALL LETTER U WITH MACRON
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $017C; // LATIN SMALL LETTER Z WITH DOT ABOVE
      $FE: Result:= $017E; // LATIN SMALL LETTER Z WITH CARON
      $FF: Result:= $02D9; // DOT ABOVE
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharWin1258(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < 128) then
    Result := Chr
  else
    case Chr of
      $80: Result:= $20AC; // EURO SIGN
      $82: Result:= $201A; // SINGLE LOW-9 QUOTATION MARK
      $83: Result:= $0192; // LATIN SMALL LETTER F WITH HOOK
      $84: Result:= $201E; // DOUBLE LOW-9 QUOTATION MARK
      $85: Result:= $2026; // HORIZONTAL ELLIPSIS
      $86: Result:= $2020; // DAGGER
      $87: Result:= $2021; // DOUBLE DAGGER
      $88: Result:= $02C6; // MODIFIER LETTER CIRCUMFLEX ACCENT
      $89: Result:= $2030; // PER MILLE SIGN
      $8B: Result:= $2039; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $8C: Result:= $0152; // LATIN CAPITAL LIGATURE OE
      $91: Result:= $2018; // LEFT SINGLE QUOTATION MARK
      $92: Result:= $2019; // RIGHT SINGLE QUOTATION MARK
      $93: Result:= $201C; // LEFT DOUBLE QUOTATION MARK
      $94: Result:= $201D; // RIGHT DOUBLE QUOTATION MARK
      $95: Result:= $2022; // BULLET
      $96: Result:= $2013; // EN DASH
      $97: Result:= $2014; // EM DASH
      $98: Result:= $02DC; // SMALL TILDE
      $99: Result:= $2122; // TRADE MARK SIGN
      $9B: Result:= $203A; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $9C: Result:= $0153; // LATIN SMALL LIGATURE OE
      $9F: Result:= $0178; // LATIN CAPITAL LETTER Y WITH DIAERESIS
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $00A1; // INVERTED EXCLAMATION MARK
      $A2: Result:= $00A2; // CENT SIGN
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $00A5; // YEN SIGN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $00AA; // FEMININE ORDINAL INDICATOR
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $00AF; // MACRON
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $00B9; // SUPERSCRIPT ONE
      $BA: Result:= $00BA; // MASCULINE ORDINAL INDICATOR
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $00BC; // VULGAR FRACTION ONE QUARTER
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BE: Result:= $00BE; // VULGAR FRACTION THREE QUARTERS
      $BF: Result:= $00BF; // INVERTED QUESTION MARK
      $C0: Result:= $00C0; // LATIN CAPITAL LETTER A WITH GRAVE
      $C1: Result:= $00C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $C2: Result:= $00C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $C3: Result:= $0102; // LATIN CAPITAL LETTER A WITH BREVE
      $C4: Result:= $00C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $C5: Result:= $00C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $C6: Result:= $00C6; // LATIN CAPITAL LETTER AE
      $C7: Result:= $00C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $C8: Result:= $00C8; // LATIN CAPITAL LETTER E WITH GRAVE
      $C9: Result:= $00C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $CA: Result:= $00CA; // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $CB: Result:= $00CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $CC: Result:= $0300; // COMBINING GRAVE ACCENT
      $CD: Result:= $00CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $CE: Result:= $00CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $CF: Result:= $00CF; // LATIN CAPITAL LETTER I WITH DIAERESIS
      $D0: Result:= $0110; // LATIN CAPITAL LETTER D WITH STROKE
      $D1: Result:= $00D1; // LATIN CAPITAL LETTER N WITH TILDE
      $D2: Result:= $0309; // COMBINING HOOK ABOVE
      $D3: Result:= $00D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $D4: Result:= $00D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $D5: Result:= $01A0; // LATIN CAPITAL LETTER O WITH HORN
      $D6: Result:= $00D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $00D8; // LATIN CAPITAL LETTER O WITH STROKE
      $D9: Result:= $00D9; // LATIN CAPITAL LETTER U WITH GRAVE
      $DA: Result:= $00DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $DB: Result:= $00DB; // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $DC: Result:= $00DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $DD: Result:= $01AF; // LATIN CAPITAL LETTER U WITH HORN
      $DE: Result:= $0303; // COMBINING TILDE
      $DF: Result:= $00DF; // LATIN SMALL LETTER SHARP S
      $E0: Result:= $00E0; // LATIN SMALL LETTER A WITH GRAVE
      $E1: Result:= $00E1; // LATIN SMALL LETTER A WITH ACUTE
      $E2: Result:= $00E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $E3: Result:= $0103; // LATIN SMALL LETTER A WITH BREVE
      $E4: Result:= $00E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $E5: Result:= $00E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $E6: Result:= $00E6; // LATIN SMALL LETTER AE
      $E7: Result:= $00E7; // LATIN SMALL LETTER C WITH CEDILLA
      $E8: Result:= $00E8; // LATIN SMALL LETTER E WITH GRAVE
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $00EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $EB: Result:= $00EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $EC: Result:= $0301; // COMBINING ACUTE ACCENT
      $ED: Result:= $00ED; // LATIN SMALL LETTER I WITH ACUTE
      $EE: Result:= $00EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $EF: Result:= $00EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $F0: Result:= $0111; // LATIN SMALL LETTER D WITH STROKE
      $F1: Result:= $00F1; // LATIN SMALL LETTER N WITH TILDE
      $F2: Result:= $0323; // COMBINING DOT BELOW
      $F3: Result:= $00F3; // LATIN SMALL LETTER O WITH ACUTE
      $F4: Result:= $00F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $F5: Result:= $01A1; // LATIN SMALL LETTER O WITH HORN
      $F6: Result:= $00F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $00F8; // LATIN SMALL LETTER O WITH STROKE
      $F9: Result:= $00F9; // LATIN SMALL LETTER U WITH GRAVE
      $FA: Result:= $00FA; // LATIN SMALL LETTER U WITH ACUTE
      $FB: Result:= $00FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $01B0; // LATIN SMALL LETTER U WITH HORN
      $FE: Result:= $20AB; // DONG SIGN
      $FF: Result:= $00FF; // LATIN SMALL LETTER Y WITH DIAERESIS
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharWin874(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < 128) then
    Result := Chr
  else
    case Chr of
      $80: Result:= $20AC; // EURO SIGN
      $85: Result:= $2026; // HORIZONTAL ELLIPSIS
      $91: Result:= $2018; // LEFT SINGLE QUOTATION MARK
      $92: Result:= $2019; // RIGHT SINGLE QUOTATION MARK
      $93: Result:= $201C; // LEFT DOUBLE QUOTATION MARK
      $94: Result:= $201D; // RIGHT DOUBLE QUOTATION MARK
      $95: Result:= $2022; // BULLET
      $96: Result:= $2013; // EN DASH
      $97: Result:= $2014; // EM DASH
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $0E01; // THAI CHARACTER KO KAI
      $A2: Result:= $0E02; // THAI CHARACTER KHO KHAI
      $A3: Result:= $0E03; // THAI CHARACTER KHO KHUAT
      $A4: Result:= $0E04; // THAI CHARACTER KHO KHWAI
      $A5: Result:= $0E05; // THAI CHARACTER KHO KHON
      $A6: Result:= $0E06; // THAI CHARACTER KHO RAKHANG
      $A7: Result:= $0E07; // THAI CHARACTER NGO NGU
      $A8: Result:= $0E08; // THAI CHARACTER CHO CHAN
      $A9: Result:= $0E09; // THAI CHARACTER CHO CHING
      $AA: Result:= $0E0A; // THAI CHARACTER CHO CHANG
      $AB: Result:= $0E0B; // THAI CHARACTER SO SO
      $AC: Result:= $0E0C; // THAI CHARACTER CHO CHOE
      $AD: Result:= $0E0D; // THAI CHARACTER YO YING
      $AE: Result:= $0E0E; // THAI CHARACTER DO CHADA
      $AF: Result:= $0E0F; // THAI CHARACTER TO PATAK
      $B0: Result:= $0E10; // THAI CHARACTER THO THAN
      $B1: Result:= $0E11; // THAI CHARACTER THO NANGMONTHO
      $B2: Result:= $0E12; // THAI CHARACTER THO PHUTHAO
      $B3: Result:= $0E13; // THAI CHARACTER NO NEN
      $B4: Result:= $0E14; // THAI CHARACTER DO DEK
      $B5: Result:= $0E15; // THAI CHARACTER TO TAO
      $B6: Result:= $0E16; // THAI CHARACTER THO THUNG
      $B7: Result:= $0E17; // THAI CHARACTER THO THAHAN
      $B8: Result:= $0E18; // THAI CHARACTER THO THONG
      $B9: Result:= $0E19; // THAI CHARACTER NO NU
      $BA: Result:= $0E1A; // THAI CHARACTER BO BAIMAI
      $BB: Result:= $0E1B; // THAI CHARACTER PO PLA
      $BC: Result:= $0E1C; // THAI CHARACTER PHO PHUNG
      $BD: Result:= $0E1D; // THAI CHARACTER FO FA
      $BE: Result:= $0E1E; // THAI CHARACTER PHO PHAN
      $BF: Result:= $0E1F; // THAI CHARACTER FO FAN
      $C0: Result:= $0E20; // THAI CHARACTER PHO SAMPHAO
      $C1: Result:= $0E21; // THAI CHARACTER MO MA
      $C2: Result:= $0E22; // THAI CHARACTER YO YAK
      $C3: Result:= $0E23; // THAI CHARACTER RO RUA
      $C4: Result:= $0E24; // THAI CHARACTER RU
      $C5: Result:= $0E25; // THAI CHARACTER LO LING
      $C6: Result:= $0E26; // THAI CHARACTER LU
      $C7: Result:= $0E27; // THAI CHARACTER WO WAEN
      $C8: Result:= $0E28; // THAI CHARACTER SO SALA
      $C9: Result:= $0E29; // THAI CHARACTER SO RUSI
      $CA: Result:= $0E2A; // THAI CHARACTER SO SUA
      $CB: Result:= $0E2B; // THAI CHARACTER HO HIP
      $CC: Result:= $0E2C; // THAI CHARACTER LO CHULA
      $CD: Result:= $0E2D; // THAI CHARACTER O ANG
      $CE: Result:= $0E2E; // THAI CHARACTER HO NOKHUK
      $CF: Result:= $0E2F; // THAI CHARACTER PAIYANNOI
      $D0: Result:= $0E30; // THAI CHARACTER SARA A
      $D1: Result:= $0E31; // THAI CHARACTER MAI HAN-AKAT
      $D2: Result:= $0E32; // THAI CHARACTER SARA AA
      $D3: Result:= $0E33; // THAI CHARACTER SARA AM
      $D4: Result:= $0E34; // THAI CHARACTER SARA I
      $D5: Result:= $0E35; // THAI CHARACTER SARA II
      $D6: Result:= $0E36; // THAI CHARACTER SARA UE
      $D7: Result:= $0E37; // THAI CHARACTER SARA UEE
      $D8: Result:= $0E38; // THAI CHARACTER SARA U
      $D9: Result:= $0E39; // THAI CHARACTER SARA UU
      $DA: Result:= $0E3A; // THAI CHARACTER PHINTHU
      $DF: Result:= $0E3F; // THAI CURRENCY SYMBOL BAHT
      $E0: Result:= $0E40; // THAI CHARACTER SARA E
      $E1: Result:= $0E41; // THAI CHARACTER SARA AE
      $E2: Result:= $0E42; // THAI CHARACTER SARA O
      $E3: Result:= $0E43; // THAI CHARACTER SARA AI MAIMUAN
      $E4: Result:= $0E44; // THAI CHARACTER SARA AI MAIMALAI
      $E5: Result:= $0E45; // THAI CHARACTER LAKKHANGYAO
      $E6: Result:= $0E46; // THAI CHARACTER MAIYAMOK
      $E7: Result:= $0E47; // THAI CHARACTER MAITAIKHU
      $E8: Result:= $0E48; // THAI CHARACTER MAI EK
      $E9: Result:= $0E49; // THAI CHARACTER MAI THO
      $EA: Result:= $0E4A; // THAI CHARACTER MAI TRI
      $EB: Result:= $0E4B; // THAI CHARACTER MAI CHATTAWA
      $EC: Result:= $0E4C; // THAI CHARACTER THANTHAKHAT
      $ED: Result:= $0E4D; // THAI CHARACTER NIKHAHIT
      $EE: Result:= $0E4E; // THAI CHARACTER YAMAKKAN
      $EF: Result:= $0E4F; // THAI CHARACTER FONGMAN
      $F0: Result:= $0E50; // THAI DIGIT ZERO
      $F1: Result:= $0E51; // THAI DIGIT ONE
      $F2: Result:= $0E52; // THAI DIGIT TWO
      $F3: Result:= $0E53; // THAI DIGIT THREE
      $F4: Result:= $0E54; // THAI DIGIT FOUR
      $F5: Result:= $0E55; // THAI DIGIT FIVE
      $F6: Result:= $0E56; // THAI DIGIT SIX
      $F7: Result:= $0E57; // THAI DIGIT SEVEN
      $F8: Result:= $0E58; // THAI DIGIT EIGHT
      $F9: Result:= $0E59; // THAI DIGIT NINE
      $FA: Result:= $0E5A; // THAI CHARACTER ANGKHANKHU
      $FB: Result:= $0E5B; // THAI CHARACTER KHOMUT
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharISO8859_1(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < $A0) then
    Result := Chr
  else
    case Chr of
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $00A1; // INVERTED EXCLAMATION MARK
      $A2: Result:= $00A2; // CENT SIGN
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $00A5; // YEN SIGN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $00AA; // FEMININE ORDINAL INDICATOR
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $00AF; // MACRON
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $00B9; // SUPERSCRIPT ONE
      $BA: Result:= $00BA; // MASCULINE ORDINAL INDICATOR
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $00BC; // VULGAR FRACTION ONE QUARTER
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BE: Result:= $00BE; // VULGAR FRACTION THREE QUARTERS
      $BF: Result:= $00BF; // INVERTED QUESTION MARK
      $C0: Result:= $00C0; // LATIN CAPITAL LETTER A WITH GRAVE
      $C1: Result:= $00C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $C2: Result:= $00C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $C3: Result:= $00C3; // LATIN CAPITAL LETTER A WITH TILDE
      $C4: Result:= $00C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $C5: Result:= $00C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $C6: Result:= $00C6; // LATIN CAPITAL LETTER AE
      $C7: Result:= $00C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $C8: Result:= $00C8; // LATIN CAPITAL LETTER E WITH GRAVE
      $C9: Result:= $00C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $CA: Result:= $00CA; // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $CB: Result:= $00CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $CC: Result:= $00CC; // LATIN CAPITAL LETTER I WITH GRAVE
      $CD: Result:= $00CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $CE: Result:= $00CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $CF: Result:= $00CF; // LATIN CAPITAL LETTER I WITH DIAERESIS
      $D0: Result:= $00D0; // LATIN CAPITAL LETTER ETH
      $D1: Result:= $00D1; // LATIN CAPITAL LETTER N WITH TILDE
      $D2: Result:= $00D2; // LATIN CAPITAL LETTER O WITH GRAVE
      $D3: Result:= $00D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $D4: Result:= $00D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $D5: Result:= $00D5; // LATIN CAPITAL LETTER O WITH TILDE
      $D6: Result:= $00D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $00D8; // LATIN CAPITAL LETTER O WITH STROKE
      $D9: Result:= $00D9; // LATIN CAPITAL LETTER U WITH GRAVE
      $DA: Result:= $00DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $DB: Result:= $00DB; // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $DC: Result:= $00DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $DD: Result:= $00DD; // LATIN CAPITAL LETTER Y WITH ACUTE
      $DE: Result:= $00DE; // LATIN CAPITAL LETTER THORN
      $DF: Result:= $00DF; // LATIN SMALL LETTER SHARP S
      $E0: Result:= $00E0; // LATIN SMALL LETTER A WITH GRAVE
      $E1: Result:= $00E1; // LATIN SMALL LETTER A WITH ACUTE
      $E2: Result:= $00E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $E3: Result:= $00E3; // LATIN SMALL LETTER A WITH TILDE
      $E4: Result:= $00E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $E5: Result:= $00E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $E6: Result:= $00E6; // LATIN SMALL LETTER AE
      $E7: Result:= $00E7; // LATIN SMALL LETTER C WITH CEDILLA
      $E8: Result:= $00E8; // LATIN SMALL LETTER E WITH GRAVE
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $00EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $EB: Result:= $00EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $EC: Result:= $00EC; // LATIN SMALL LETTER I WITH GRAVE
      $ED: Result:= $00ED; // LATIN SMALL LETTER I WITH ACUTE
      $EE: Result:= $00EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $EF: Result:= $00EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $F0: Result:= $00F0; // LATIN SMALL LETTER ETH
      $F1: Result:= $00F1; // LATIN SMALL LETTER N WITH TILDE
      $F2: Result:= $00F2; // LATIN SMALL LETTER O WITH GRAVE
      $F3: Result:= $00F3; // LATIN SMALL LETTER O WITH ACUTE
      $F4: Result:= $00F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $F5: Result:= $00F5; // LATIN SMALL LETTER O WITH TILDE
      $F6: Result:= $00F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $00F8; // LATIN SMALL LETTER O WITH STROKE
      $F9: Result:= $00F9; // LATIN SMALL LETTER U WITH GRAVE
      $FA: Result:= $00FA; // LATIN SMALL LETTER U WITH ACUTE
      $FB: Result:= $00FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $00FD; // LATIN SMALL LETTER Y WITH ACUTE
      $FE: Result:= $00FE; // LATIN SMALL LETTER THORN
      $FF: Result:= $00FF; // LATIN SMALL LETTER Y WITH DIAERESIS
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharISO8859_2(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < $A0) then
    Result := Chr
  else
    case Chr of
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $0104; // LATIN CAPITAL LETTER A WITH OGONEK
      $A2: Result:= $02D8; // BREVE
      $A3: Result:= $0141; // LATIN CAPITAL LETTER L WITH STROKE
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $013D; // LATIN CAPITAL LETTER L WITH CARON
      $A6: Result:= $015A; // LATIN CAPITAL LETTER S WITH ACUTE
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $0160; // LATIN CAPITAL LETTER S WITH CARON
      $AA: Result:= $015E; // LATIN CAPITAL LETTER S WITH CEDILLA
      $AB: Result:= $0164; // LATIN CAPITAL LETTER T WITH CARON
      $AC: Result:= $0179; // LATIN CAPITAL LETTER Z WITH ACUTE
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $017D; // LATIN CAPITAL LETTER Z WITH CARON
      $AF: Result:= $017B; // LATIN CAPITAL LETTER Z WITH DOT ABOVE
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $0105; // LATIN SMALL LETTER A WITH OGONEK
      $B2: Result:= $02DB; // OGONEK
      $B3: Result:= $0142; // LATIN SMALL LETTER L WITH STROKE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $013E; // LATIN SMALL LETTER L WITH CARON
      $B6: Result:= $015B; // LATIN SMALL LETTER S WITH ACUTE
      $B7: Result:= $02C7; // CARON
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $0161; // LATIN SMALL LETTER S WITH CARON
      $BA: Result:= $015F; // LATIN SMALL LETTER S WITH CEDILLA
      $BB: Result:= $0165; // LATIN SMALL LETTER T WITH CARON
      $BC: Result:= $017A; // LATIN SMALL LETTER Z WITH ACUTE
      $BD: Result:= $02DD; // DOUBLE ACUTE ACCENT
      $BE: Result:= $017E; // LATIN SMALL LETTER Z WITH CARON
      $BF: Result:= $017C; // LATIN SMALL LETTER Z WITH DOT ABOVE
      $C0: Result:= $0154; // LATIN CAPITAL LETTER R WITH ACUTE
      $C1: Result:= $00C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $C2: Result:= $00C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $C3: Result:= $0102; // LATIN CAPITAL LETTER A WITH BREVE
      $C4: Result:= $00C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $C5: Result:= $0139; // LATIN CAPITAL LETTER L WITH ACUTE
      $C6: Result:= $0106; // LATIN CAPITAL LETTER C WITH ACUTE
      $C7: Result:= $00C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $C8: Result:= $010C; // LATIN CAPITAL LETTER C WITH CARON
      $C9: Result:= $00C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $CA: Result:= $0118; // LATIN CAPITAL LETTER E WITH OGONEK
      $CB: Result:= $00CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $CC: Result:= $011A; // LATIN CAPITAL LETTER E WITH CARON
      $CD: Result:= $00CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $CE: Result:= $00CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $CF: Result:= $010E; // LATIN CAPITAL LETTER D WITH CARON
      $D0: Result:= $0110; // LATIN CAPITAL LETTER D WITH STROKE
      $D1: Result:= $0143; // LATIN CAPITAL LETTER N WITH ACUTE
      $D2: Result:= $0147; // LATIN CAPITAL LETTER N WITH CARON
      $D3: Result:= $00D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $D4: Result:= $00D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $D5: Result:= $0150; // LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
      $D6: Result:= $00D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $0158; // LATIN CAPITAL LETTER R WITH CARON
      $D9: Result:= $016E; // LATIN CAPITAL LETTER U WITH RING ABOVE
      $DA: Result:= $00DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $DB: Result:= $0170; // LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
      $DC: Result:= $00DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $DD: Result:= $00DD; // LATIN CAPITAL LETTER Y WITH ACUTE
      $DE: Result:= $0162; // LATIN CAPITAL LETTER T WITH CEDILLA
      $DF: Result:= $00DF; // LATIN SMALL LETTER SHARP S
      $E0: Result:= $0155; // LATIN SMALL LETTER R WITH ACUTE
      $E1: Result:= $00E1; // LATIN SMALL LETTER A WITH ACUTE
      $E2: Result:= $00E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $E3: Result:= $0103; // LATIN SMALL LETTER A WITH BREVE
      $E4: Result:= $00E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $E5: Result:= $013A; // LATIN SMALL LETTER L WITH ACUTE
      $E6: Result:= $0107; // LATIN SMALL LETTER C WITH ACUTE
      $E7: Result:= $00E7; // LATIN SMALL LETTER C WITH CEDILLA
      $E8: Result:= $010D; // LATIN SMALL LETTER C WITH CARON
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $0119; // LATIN SMALL LETTER E WITH OGONEK
      $EB: Result:= $00EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $EC: Result:= $011B; // LATIN SMALL LETTER E WITH CARON
      $ED: Result:= $00ED; // LATIN SMALL LETTER I WITH ACUTE
      $EE: Result:= $00EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $EF: Result:= $010F; // LATIN SMALL LETTER D WITH CARON
      $F0: Result:= $0111; // LATIN SMALL LETTER D WITH STROKE
      $F1: Result:= $0144; // LATIN SMALL LETTER N WITH ACUTE
      $F2: Result:= $0148; // LATIN SMALL LETTER N WITH CARON
      $F3: Result:= $00F3; // LATIN SMALL LETTER O WITH ACUTE
      $F4: Result:= $00F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $F5: Result:= $0151; // LATIN SMALL LETTER O WITH DOUBLE ACUTE
      $F6: Result:= $00F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $0159; // LATIN SMALL LETTER R WITH CARON
      $F9: Result:= $016F; // LATIN SMALL LETTER U WITH RING ABOVE
      $FA: Result:= $00FA; // LATIN SMALL LETTER U WITH ACUTE
      $FB: Result:= $0171; // LATIN SMALL LETTER U WITH DOUBLE ACUTE
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $00FD; // LATIN SMALL LETTER Y WITH ACUTE
      $FE: Result:= $0163; // LATIN SMALL LETTER T WITH CEDILLA
      $FF: Result:= $02D9; // DOT ABOVE
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharISO8859_3(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < $A0) then
    Result := Chr
  else
    case Chr of
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $0126; // LATIN CAPITAL LETTER H WITH STROKE
      $A2: Result:= $02D8; // BREVE
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A6: Result:= $0124; // LATIN CAPITAL LETTER H WITH CIRCUMFLEX
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $0130; // LATIN CAPITAL LETTER I WITH DOT ABOVE
      $AA: Result:= $015E; // LATIN CAPITAL LETTER S WITH CEDILLA
      $AB: Result:= $011E; // LATIN CAPITAL LETTER G WITH BREVE
      $AC: Result:= $0134; // LATIN CAPITAL LETTER J WITH CIRCUMFLEX
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AF: Result:= $017B; // LATIN CAPITAL LETTER Z WITH DOT ABOVE
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $0127; // LATIN SMALL LETTER H WITH STROKE
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $0125; // LATIN SMALL LETTER H WITH CIRCUMFLEX
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $0131; // LATIN SMALL LETTER DOTLESS I
      $BA: Result:= $015F; // LATIN SMALL LETTER S WITH CEDILLA
      $BB: Result:= $011F; // LATIN SMALL LETTER G WITH BREVE
      $BC: Result:= $0135; // LATIN SMALL LETTER J WITH CIRCUMFLEX
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BF: Result:= $017C; // LATIN SMALL LETTER Z WITH DOT ABOVE
      $C0: Result:= $00C0; // LATIN CAPITAL LETTER A WITH GRAVE
      $C1: Result:= $00C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $C2: Result:= $00C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $C4: Result:= $00C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $C5: Result:= $010A; // LATIN CAPITAL LETTER C WITH DOT ABOVE
      $C6: Result:= $0108; // LATIN CAPITAL LETTER C WITH CIRCUMFLEX
      $C7: Result:= $00C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $C8: Result:= $00C8; // LATIN CAPITAL LETTER E WITH GRAVE
      $C9: Result:= $00C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $CA: Result:= $00CA; // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $CB: Result:= $00CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $CC: Result:= $00CC; // LATIN CAPITAL LETTER I WITH GRAVE
      $CD: Result:= $00CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $CE: Result:= $00CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $CF: Result:= $00CF; // LATIN CAPITAL LETTER I WITH DIAERESIS
      $D1: Result:= $00D1; // LATIN CAPITAL LETTER N WITH TILDE
      $D2: Result:= $00D2; // LATIN CAPITAL LETTER O WITH GRAVE
      $D3: Result:= $00D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $D4: Result:= $00D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $D5: Result:= $0120; // LATIN CAPITAL LETTER G WITH DOT ABOVE
      $D6: Result:= $00D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $011C; // LATIN CAPITAL LETTER G WITH CIRCUMFLEX
      $D9: Result:= $00D9; // LATIN CAPITAL LETTER U WITH GRAVE
      $DA: Result:= $00DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $DB: Result:= $00DB; // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $DC: Result:= $00DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $DD: Result:= $016C; // LATIN CAPITAL LETTER U WITH BREVE
      $DE: Result:= $015C; // LATIN CAPITAL LETTER S WITH CIRCUMFLEX
      $DF: Result:= $00DF; // LATIN SMALL LETTER SHARP S
      $E0: Result:= $00E0; // LATIN SMALL LETTER A WITH GRAVE
      $E1: Result:= $00E1; // LATIN SMALL LETTER A WITH ACUTE
      $E2: Result:= $00E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $E4: Result:= $00E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $E5: Result:= $010B; // LATIN SMALL LETTER C WITH DOT ABOVE
      $E6: Result:= $0109; // LATIN SMALL LETTER C WITH CIRCUMFLEX
      $E7: Result:= $00E7; // LATIN SMALL LETTER C WITH CEDILLA
      $E8: Result:= $00E8; // LATIN SMALL LETTER E WITH GRAVE
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $00EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $EB: Result:= $00EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $EC: Result:= $00EC; // LATIN SMALL LETTER I WITH GRAVE
      $ED: Result:= $00ED; // LATIN SMALL LETTER I WITH ACUTE
      $EE: Result:= $00EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $EF: Result:= $00EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $F1: Result:= $00F1; // LATIN SMALL LETTER N WITH TILDE
      $F2: Result:= $00F2; // LATIN SMALL LETTER O WITH GRAVE
      $F3: Result:= $00F3; // LATIN SMALL LETTER O WITH ACUTE
      $F4: Result:= $00F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $F5: Result:= $0121; // LATIN SMALL LETTER G WITH DOT ABOVE
      $F6: Result:= $00F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $011D; // LATIN SMALL LETTER G WITH CIRCUMFLEX
      $F9: Result:= $00F9; // LATIN SMALL LETTER U WITH GRAVE
      $FA: Result:= $00FA; // LATIN SMALL LETTER U WITH ACUTE
      $FB: Result:= $00FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $016D; // LATIN SMALL LETTER U WITH BREVE
      $FE: Result:= $015D; // LATIN SMALL LETTER S WITH CIRCUMFLEX
      $FF: Result:= $02D9; // DOT ABOVE
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharISO8859_4(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < $A0) then
    Result := Chr
  else
    case Chr of
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $0104; // LATIN CAPITAL LETTER A WITH OGONEK
      $A2: Result:= $0138; // LATIN SMALL LETTER KRA
      $A3: Result:= $0156; // LATIN CAPITAL LETTER R WITH CEDILLA
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $0128; // LATIN CAPITAL LETTER I WITH TILDE
      $A6: Result:= $013B; // LATIN CAPITAL LETTER L WITH CEDILLA
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $0160; // LATIN CAPITAL LETTER S WITH CARON
      $AA: Result:= $0112; // LATIN CAPITAL LETTER E WITH MACRON
      $AB: Result:= $0122; // LATIN CAPITAL LETTER G WITH CEDILLA
      $AC: Result:= $0166; // LATIN CAPITAL LETTER T WITH STROKE
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $017D; // LATIN CAPITAL LETTER Z WITH CARON
      $AF: Result:= $00AF; // MACRON
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $0105; // LATIN SMALL LETTER A WITH OGONEK
      $B2: Result:= $02DB; // OGONEK
      $B3: Result:= $0157; // LATIN SMALL LETTER R WITH CEDILLA
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $0129; // LATIN SMALL LETTER I WITH TILDE
      $B6: Result:= $013C; // LATIN SMALL LETTER L WITH CEDILLA
      $B7: Result:= $02C7; // CARON
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $0161; // LATIN SMALL LETTER S WITH CARON
      $BA: Result:= $0113; // LATIN SMALL LETTER E WITH MACRON
      $BB: Result:= $0123; // LATIN SMALL LETTER G WITH CEDILLA
      $BC: Result:= $0167; // LATIN SMALL LETTER T WITH STROKE
      $BD: Result:= $014A; // LATIN CAPITAL LETTER ENG
      $BE: Result:= $017E; // LATIN SMALL LETTER Z WITH CARON
      $BF: Result:= $014B; // LATIN SMALL LETTER ENG
      $C0: Result:= $0100; // LATIN CAPITAL LETTER A WITH MACRON
      $C1: Result:= $00C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $C2: Result:= $00C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $C3: Result:= $00C3; // LATIN CAPITAL LETTER A WITH TILDE
      $C4: Result:= $00C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $C5: Result:= $00C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $C6: Result:= $00C6; // LATIN CAPITAL LETTER AE
      $C7: Result:= $012E; // LATIN CAPITAL LETTER I WITH OGONEK
      $C8: Result:= $010C; // LATIN CAPITAL LETTER C WITH CARON
      $C9: Result:= $00C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $CA: Result:= $0118; // LATIN CAPITAL LETTER E WITH OGONEK
      $CB: Result:= $00CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $CC: Result:= $0116; // LATIN CAPITAL LETTER E WITH DOT ABOVE
      $CD: Result:= $00CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $CE: Result:= $00CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $CF: Result:= $012A; // LATIN CAPITAL LETTER I WITH MACRON
      $D0: Result:= $0110; // LATIN CAPITAL LETTER D WITH STROKE
      $D1: Result:= $0145; // LATIN CAPITAL LETTER N WITH CEDILLA
      $D2: Result:= $014C; // LATIN CAPITAL LETTER O WITH MACRON
      $D3: Result:= $0136; // LATIN CAPITAL LETTER K WITH CEDILLA
      $D4: Result:= $00D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $D5: Result:= $00D5; // LATIN CAPITAL LETTER O WITH TILDE
      $D6: Result:= $00D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $00D8; // LATIN CAPITAL LETTER O WITH STROKE
      $D9: Result:= $0172; // LATIN CAPITAL LETTER U WITH OGONEK
      $DA: Result:= $00DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $DB: Result:= $00DB; // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $DC: Result:= $00DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $DD: Result:= $0168; // LATIN CAPITAL LETTER U WITH TILDE
      $DE: Result:= $016A; // LATIN CAPITAL LETTER U WITH MACRON
      $DF: Result:= $00DF; // LATIN SMALL LETTER SHARP S
      $E0: Result:= $0101; // LATIN SMALL LETTER A WITH MACRON
      $E1: Result:= $00E1; // LATIN SMALL LETTER A WITH ACUTE
      $E2: Result:= $00E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $E3: Result:= $00E3; // LATIN SMALL LETTER A WITH TILDE
      $E4: Result:= $00E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $E5: Result:= $00E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $E6: Result:= $00E6; // LATIN SMALL LETTER AE
      $E7: Result:= $012F; // LATIN SMALL LETTER I WITH OGONEK
      $E8: Result:= $010D; // LATIN SMALL LETTER C WITH CARON
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $0119; // LATIN SMALL LETTER E WITH OGONEK
      $EB: Result:= $00EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $EC: Result:= $0117; // LATIN SMALL LETTER E WITH DOT ABOVE
      $ED: Result:= $00ED; // LATIN SMALL LETTER I WITH ACUTE
      $EE: Result:= $00EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $EF: Result:= $012B; // LATIN SMALL LETTER I WITH MACRON
      $F0: Result:= $0111; // LATIN SMALL LETTER D WITH STROKE
      $F1: Result:= $0146; // LATIN SMALL LETTER N WITH CEDILLA
      $F2: Result:= $014D; // LATIN SMALL LETTER O WITH MACRON
      $F3: Result:= $0137; // LATIN SMALL LETTER K WITH CEDILLA
      $F4: Result:= $00F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $F5: Result:= $00F5; // LATIN SMALL LETTER O WITH TILDE
      $F6: Result:= $00F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $00F8; // LATIN SMALL LETTER O WITH STROKE
      $F9: Result:= $0173; // LATIN SMALL LETTER U WITH OGONEK
      $FA: Result:= $00FA; // LATIN SMALL LETTER U WITH ACUTE
      $FB: Result:= $00FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $0169; // LATIN SMALL LETTER U WITH TILDE
      $FE: Result:= $016B; // LATIN SMALL LETTER U WITH MACRON
      $FF: Result:= $02D9; // DOT ABOVE
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharISO8859_5(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < $A0) then
    Result := Chr
  else
    case Chr of
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $0401; // CYRILLIC CAPITAL LETTER IO
      $A2: Result:= $0402; // CYRILLIC CAPITAL LETTER DJE
      $A3: Result:= $0403; // CYRILLIC CAPITAL LETTER GJE
      $A4: Result:= $0404; // CYRILLIC CAPITAL LETTER UKRAINIAN IE
      $A5: Result:= $0405; // CYRILLIC CAPITAL LETTER DZE
      $A6: Result:= $0406; // CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
      $A7: Result:= $0407; // CYRILLIC CAPITAL LETTER YI
      $A8: Result:= $0408; // CYRILLIC CAPITAL LETTER JE
      $A9: Result:= $0409; // CYRILLIC CAPITAL LETTER LJE
      $AA: Result:= $040A; // CYRILLIC CAPITAL LETTER NJE
      $AB: Result:= $040B; // CYRILLIC CAPITAL LETTER TSHE
      $AC: Result:= $040C; // CYRILLIC CAPITAL LETTER KJE
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $040E; // CYRILLIC CAPITAL LETTER SHORT U
      $AF: Result:= $040F; // CYRILLIC CAPITAL LETTER DZHE
      $B0: Result:= $0410; // CYRILLIC CAPITAL LETTER A
      $B1: Result:= $0411; // CYRILLIC CAPITAL LETTER BE
      $B2: Result:= $0412; // CYRILLIC CAPITAL LETTER VE
      $B3: Result:= $0413; // CYRILLIC CAPITAL LETTER GHE
      $B4: Result:= $0414; // CYRILLIC CAPITAL LETTER DE
      $B5: Result:= $0415; // CYRILLIC CAPITAL LETTER IE
      $B6: Result:= $0416; // CYRILLIC CAPITAL LETTER ZHE
      $B7: Result:= $0417; // CYRILLIC CAPITAL LETTER ZE
      $B8: Result:= $0418; // CYRILLIC CAPITAL LETTER I
      $B9: Result:= $0419; // CYRILLIC CAPITAL LETTER SHORT I
      $BA: Result:= $041A; // CYRILLIC CAPITAL LETTER KA
      $BB: Result:= $041B; // CYRILLIC CAPITAL LETTER EL
      $BC: Result:= $041C; // CYRILLIC CAPITAL LETTER EM
      $BD: Result:= $041D; // CYRILLIC CAPITAL LETTER EN
      $BE: Result:= $041E; // CYRILLIC CAPITAL LETTER O
      $BF: Result:= $041F; // CYRILLIC CAPITAL LETTER PE
      $C0: Result:= $0420; // CYRILLIC CAPITAL LETTER ER
      $C1: Result:= $0421; // CYRILLIC CAPITAL LETTER ES
      $C2: Result:= $0422; // CYRILLIC CAPITAL LETTER TE
      $C3: Result:= $0423; // CYRILLIC CAPITAL LETTER U
      $C4: Result:= $0424; // CYRILLIC CAPITAL LETTER EF
      $C5: Result:= $0425; // CYRILLIC CAPITAL LETTER HA
      $C6: Result:= $0426; // CYRILLIC CAPITAL LETTER TSE
      $C7: Result:= $0427; // CYRILLIC CAPITAL LETTER CHE
      $C8: Result:= $0428; // CYRILLIC CAPITAL LETTER SHA
      $C9: Result:= $0429; // CYRILLIC CAPITAL LETTER SHCHA
      $CA: Result:= $042A; // CYRILLIC CAPITAL LETTER HARD SIGN
      $CB: Result:= $042B; // CYRILLIC CAPITAL LETTER YERU
      $CC: Result:= $042C; // CYRILLIC CAPITAL LETTER SOFT SIGN
      $CD: Result:= $042D; // CYRILLIC CAPITAL LETTER E
      $CE: Result:= $042E; // CYRILLIC CAPITAL LETTER YU
      $CF: Result:= $042F; // CYRILLIC CAPITAL LETTER YA
      $D0: Result:= $0430; // CYRILLIC SMALL LETTER A
      $D1: Result:= $0431; // CYRILLIC SMALL LETTER BE
      $D2: Result:= $0432; // CYRILLIC SMALL LETTER VE
      $D3: Result:= $0433; // CYRILLIC SMALL LETTER GHE
      $D4: Result:= $0434; // CYRILLIC SMALL LETTER DE
      $D5: Result:= $0435; // CYRILLIC SMALL LETTER IE
      $D6: Result:= $0436; // CYRILLIC SMALL LETTER ZHE
      $D7: Result:= $0437; // CYRILLIC SMALL LETTER ZE
      $D8: Result:= $0438; // CYRILLIC SMALL LETTER I
      $D9: Result:= $0439; // CYRILLIC SMALL LETTER SHORT I
      $DA: Result:= $043A; // CYRILLIC SMALL LETTER KA
      $DB: Result:= $043B; // CYRILLIC SMALL LETTER EL
      $DC: Result:= $043C; // CYRILLIC SMALL LETTER EM
      $DD: Result:= $043D; // CYRILLIC SMALL LETTER EN
      $DE: Result:= $043E; // CYRILLIC SMALL LETTER O
      $DF: Result:= $043F; // CYRILLIC SMALL LETTER PE
      $E0: Result:= $0440; // CYRILLIC SMALL LETTER ER
      $E1: Result:= $0441; // CYRILLIC SMALL LETTER ES
      $E2: Result:= $0442; // CYRILLIC SMALL LETTER TE
      $E3: Result:= $0443; // CYRILLIC SMALL LETTER U
      $E4: Result:= $0444; // CYRILLIC SMALL LETTER EF
      $E5: Result:= $0445; // CYRILLIC SMALL LETTER HA
      $E6: Result:= $0446; // CYRILLIC SMALL LETTER TSE
      $E7: Result:= $0447; // CYRILLIC SMALL LETTER CHE
      $E8: Result:= $0448; // CYRILLIC SMALL LETTER SHA
      $E9: Result:= $0449; // CYRILLIC SMALL LETTER SHCHA
      $EA: Result:= $044A; // CYRILLIC SMALL LETTER HARD SIGN
      $EB: Result:= $044B; // CYRILLIC SMALL LETTER YERU
      $EC: Result:= $044C; // CYRILLIC SMALL LETTER SOFT SIGN
      $ED: Result:= $044D; // CYRILLIC SMALL LETTER E
      $EE: Result:= $044E; // CYRILLIC SMALL LETTER YU
      $EF: Result:= $044F; // CYRILLIC SMALL LETTER YA
      $F0: Result:= $2116; // NUMERO SIGN
      $F1: Result:= $0451; // CYRILLIC SMALL LETTER IO
      $F2: Result:= $0452; // CYRILLIC SMALL LETTER DJE
      $F3: Result:= $0453; // CYRILLIC SMALL LETTER GJE
      $F4: Result:= $0454; // CYRILLIC SMALL LETTER UKRAINIAN IE
      $F5: Result:= $0455; // CYRILLIC SMALL LETTER DZE
      $F6: Result:= $0456; // CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
      $F7: Result:= $0457; // CYRILLIC SMALL LETTER YI
      $F8: Result:= $0458; // CYRILLIC SMALL LETTER JE
      $F9: Result:= $0459; // CYRILLIC SMALL LETTER LJE
      $FA: Result:= $045A; // CYRILLIC SMALL LETTER NJE
      $FB: Result:= $045B; // CYRILLIC SMALL LETTER TSHE
      $FC: Result:= $045C; // CYRILLIC SMALL LETTER KJE
      $FD: Result:= $00A7; // SECTION SIGN
      $FE: Result:= $045E; // CYRILLIC SMALL LETTER SHORT U
      $FF: Result:= $045F; // CYRILLIC SMALL LETTER DZHE
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharISO8859_6(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < $A0) then
    Result := Chr
  else
    case Chr of
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A4: Result:= $00A4; // CURRENCY SIGN
      $AC: Result:= $060C; // ARABIC COMMA
      $AD: Result:= $00AD; // SOFT HYPHEN
      $BB: Result:= $061B; // ARABIC SEMICOLON
      $BF: Result:= $061F; // ARABIC QUESTION MARK
      $C1: Result:= $0621; // ARABIC LETTER HAMZA
      $C2: Result:= $0622; // ARABIC LETTER ALEF WITH MADDA ABOVE
      $C3: Result:= $0623; // ARABIC LETTER ALEF WITH HAMZA ABOVE
      $C4: Result:= $0624; // ARABIC LETTER WAW WITH HAMZA ABOVE
      $C5: Result:= $0625; // ARABIC LETTER ALEF WITH HAMZA BELOW
      $C6: Result:= $0626; // ARABIC LETTER YEH WITH HAMZA ABOVE
      $C7: Result:= $0627; // ARABIC LETTER ALEF
      $C8: Result:= $0628; // ARABIC LETTER BEH
      $C9: Result:= $0629; // ARABIC LETTER TEH MARBUTA
      $CA: Result:= $062A; // ARABIC LETTER TEH
      $CB: Result:= $062B; // ARABIC LETTER THEH
      $CC: Result:= $062C; // ARABIC LETTER JEEM
      $CD: Result:= $062D; // ARABIC LETTER HAH
      $CE: Result:= $062E; // ARABIC LETTER KHAH
      $CF: Result:= $062F; // ARABIC LETTER DAL
      $D0: Result:= $0630; // ARABIC LETTER THAL
      $D1: Result:= $0631; // ARABIC LETTER REH
      $D2: Result:= $0632; // ARABIC LETTER ZAIN
      $D3: Result:= $0633; // ARABIC LETTER SEEN
      $D4: Result:= $0634; // ARABIC LETTER SHEEN
      $D5: Result:= $0635; // ARABIC LETTER SAD
      $D6: Result:= $0636; // ARABIC LETTER DAD
      $D7: Result:= $0637; // ARABIC LETTER TAH
      $D8: Result:= $0638; // ARABIC LETTER ZAH
      $D9: Result:= $0639; // ARABIC LETTER AIN
      $DA: Result:= $063A; // ARABIC LETTER GHAIN
      $E0: Result:= $0640; // ARABIC TATWEEL
      $E1: Result:= $0641; // ARABIC LETTER FEH
      $E2: Result:= $0642; // ARABIC LETTER QAF
      $E3: Result:= $0643; // ARABIC LETTER KAF
      $E4: Result:= $0644; // ARABIC LETTER LAM
      $E5: Result:= $0645; // ARABIC LETTER MEEM
      $E6: Result:= $0646; // ARABIC LETTER NOON
      $E7: Result:= $0647; // ARABIC LETTER HEH
      $E8: Result:= $0648; // ARABIC LETTER WAW
      $E9: Result:= $0649; // ARABIC LETTER ALEF MAKSURA
      $EA: Result:= $064A; // ARABIC LETTER YEH
      $EB: Result:= $064B; // ARABIC FATHATAN
      $EC: Result:= $064C; // ARABIC DAMMATAN
      $ED: Result:= $064D; // ARABIC KASRATAN
      $EE: Result:= $064E; // ARABIC FATHA
      $EF: Result:= $064F; // ARABIC DAMMA
      $F0: Result:= $0650; // ARABIC KASRA
      $F1: Result:= $0651; // ARABIC SHADDA
      $F2: Result:= $0652; // ARABIC SUKUN
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharISO8859_7(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < $A0) then
    Result := Chr
  else
    case Chr of
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $02BD; // MODIFIER LETTER REVERSED COMMA
      $A2: Result:= $02BC; // MODIFIER LETTER APOSTROPHE
      $A3: Result:= $00A3; // POUND SIGN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AF: Result:= $2015; // HORIZONTAL BAR
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $0384; // GREEK TONOS
      $B5: Result:= $0385; // GREEK DIALYTIKA TONOS
      $B6: Result:= $0386; // GREEK CAPITAL LETTER ALPHA WITH TONOS
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $0388; // GREEK CAPITAL LETTER EPSILON WITH TONOS
      $B9: Result:= $0389; // GREEK CAPITAL LETTER ETA WITH TONOS
      $BA: Result:= $038A; // GREEK CAPITAL LETTER IOTA WITH TONOS
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $038C; // GREEK CAPITAL LETTER OMICRON WITH TONOS
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BE: Result:= $038E; // GREEK CAPITAL LETTER UPSILON WITH TONOS
      $BF: Result:= $038F; // GREEK CAPITAL LETTER OMEGA WITH TONOS
      $C0: Result:= $0390; // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
      $C1: Result:= $0391; // GREEK CAPITAL LETTER ALPHA
      $C2: Result:= $0392; // GREEK CAPITAL LETTER BETA
      $C3: Result:= $0393; // GREEK CAPITAL LETTER GAMMA
      $C4: Result:= $0394; // GREEK CAPITAL LETTER DELTA
      $C5: Result:= $0395; // GREEK CAPITAL LETTER EPSILON
      $C6: Result:= $0396; // GREEK CAPITAL LETTER ZETA
      $C7: Result:= $0397; // GREEK CAPITAL LETTER ETA
      $C8: Result:= $0398; // GREEK CAPITAL LETTER THETA
      $C9: Result:= $0399; // GREEK CAPITAL LETTER IOTA
      $CA: Result:= $039A; // GREEK CAPITAL LETTER KAPPA
      $CB: Result:= $039B; // GREEK CAPITAL LETTER LAMDA
      $CC: Result:= $039C; // GREEK CAPITAL LETTER MU
      $CD: Result:= $039D; // GREEK CAPITAL LETTER NU
      $CE: Result:= $039E; // GREEK CAPITAL LETTER XI
      $CF: Result:= $039F; // GREEK CAPITAL LETTER OMICRON
      $D0: Result:= $03A0; // GREEK CAPITAL LETTER PI
      $D1: Result:= $03A1; // GREEK CAPITAL LETTER RHO
      $D3: Result:= $03A3; // GREEK CAPITAL LETTER SIGMA
      $D4: Result:= $03A4; // GREEK CAPITAL LETTER TAU
      $D5: Result:= $03A5; // GREEK CAPITAL LETTER UPSILON
      $D6: Result:= $03A6; // GREEK CAPITAL LETTER PHI
      $D7: Result:= $03A7; // GREEK CAPITAL LETTER CHI
      $D8: Result:= $03A8; // GREEK CAPITAL LETTER PSI
      $D9: Result:= $03A9; // GREEK CAPITAL LETTER OMEGA
      $DA: Result:= $03AA; // GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
      $DB: Result:= $03AB; // GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
      $DC: Result:= $03AC; // GREEK SMALL LETTER ALPHA WITH TONOS
      $DD: Result:= $03AD; // GREEK SMALL LETTER EPSILON WITH TONOS
      $DE: Result:= $03AE; // GREEK SMALL LETTER ETA WITH TONOS
      $DF: Result:= $03AF; // GREEK SMALL LETTER IOTA WITH TONOS
      $E0: Result:= $03B0; // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
      $E1: Result:= $03B1; // GREEK SMALL LETTER ALPHA
      $E2: Result:= $03B2; // GREEK SMALL LETTER BETA
      $E3: Result:= $03B3; // GREEK SMALL LETTER GAMMA
      $E4: Result:= $03B4; // GREEK SMALL LETTER DELTA
      $E5: Result:= $03B5; // GREEK SMALL LETTER EPSILON
      $E6: Result:= $03B6; // GREEK SMALL LETTER ZETA
      $E7: Result:= $03B7; // GREEK SMALL LETTER ETA
      $E8: Result:= $03B8; // GREEK SMALL LETTER THETA
      $E9: Result:= $03B9; // GREEK SMALL LETTER IOTA
      $EA: Result:= $03BA; // GREEK SMALL LETTER KAPPA
      $EB: Result:= $03BB; // GREEK SMALL LETTER LAMDA
      $EC: Result:= $03BC; // GREEK SMALL LETTER MU
      $ED: Result:= $03BD; // GREEK SMALL LETTER NU
      $EE: Result:= $03BE; // GREEK SMALL LETTER XI
      $EF: Result:= $03BF; // GREEK SMALL LETTER OMICRON
      $F0: Result:= $03C0; // GREEK SMALL LETTER PI
      $F1: Result:= $03C1; // GREEK SMALL LETTER RHO
      $F2: Result:= $03C2; // GREEK SMALL LETTER FINAL SIGMA
      $F3: Result:= $03C3; // GREEK SMALL LETTER SIGMA
      $F4: Result:= $03C4; // GREEK SMALL LETTER TAU
      $F5: Result:= $03C5; // GREEK SMALL LETTER UPSILON
      $F6: Result:= $03C6; // GREEK SMALL LETTER PHI
      $F7: Result:= $03C7; // GREEK SMALL LETTER CHI
      $F8: Result:= $03C8; // GREEK SMALL LETTER PSI
      $F9: Result:= $03C9; // GREEK SMALL LETTER OMEGA
      $FA: Result:= $03CA; // GREEK SMALL LETTER IOTA WITH DIALYTIKA
      $FB: Result:= $03CB; // GREEK SMALL LETTER UPSILON WITH DIALYTIKA
      $FC: Result:= $03CC; // GREEK SMALL LETTER OMICRON WITH TONOS
      $FD: Result:= $03CD; // GREEK SMALL LETTER UPSILON WITH TONOS
      $FE: Result:= $03CE; // GREEK SMALL LETTER OMEGA WITH TONOS
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharISO8859_8(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < $A0) then
    Result := Chr
  else
    case Chr of
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A2: Result:= $00A2; // CENT SIGN
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $00A5; // YEN SIGN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $00D7; // MULTIPLICATION SIGN
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $203E; // OVERLINE
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $00B9; // SUPERSCRIPT ONE
      $BA: Result:= $00F7; // DIVISION SIGN
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $00BC; // VULGAR FRACTION ONE QUARTER
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BE: Result:= $00BE; // VULGAR FRACTION THREE QUARTERS
      $DF: Result:= $2017; // DOUBLE LOW LINE
      $E0: Result:= $05D0; // HEBREW LETTER ALEF
      $E1: Result:= $05D1; // HEBREW LETTER BET
      $E2: Result:= $05D2; // HEBREW LETTER GIMEL
      $E3: Result:= $05D3; // HEBREW LETTER DALET
      $E4: Result:= $05D4; // HEBREW LETTER HE
      $E5: Result:= $05D5; // HEBREW LETTER VAV
      $E6: Result:= $05D6; // HEBREW LETTER ZAYIN
      $E7: Result:= $05D7; // HEBREW LETTER HET
      $E8: Result:= $05D8; // HEBREW LETTER TET
      $E9: Result:= $05D9; // HEBREW LETTER YOD
      $EA: Result:= $05DA; // HEBREW LETTER FINAL KAF
      $EB: Result:= $05DB; // HEBREW LETTER KAF
      $EC: Result:= $05DC; // HEBREW LETTER LAMED
      $ED: Result:= $05DD; // HEBREW LETTER FINAL MEM
      $EE: Result:= $05DE; // HEBREW LETTER MEM
      $EF: Result:= $05DF; // HEBREW LETTER FINAL NUN
      $F0: Result:= $05E0; // HEBREW LETTER NUN
      $F1: Result:= $05E1; // HEBREW LETTER SAMEKH
      $F2: Result:= $05E2; // HEBREW LETTER AYIN
      $F3: Result:= $05E3; // HEBREW LETTER FINAL PE
      $F4: Result:= $05E4; // HEBREW LETTER PE
      $F5: Result:= $05E5; // HEBREW LETTER FINAL TSADI
      $F6: Result:= $05E6; // HEBREW LETTER TSADI
      $F7: Result:= $05E7; // HEBREW LETTER QOF
      $F8: Result:= $05E8; // HEBREW LETTER RESH
      $F9: Result:= $05E9; // HEBREW LETTER SHIN
      $FA: Result:= $05EA; // HEBREW LETTER TAV
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharISO8859_9(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < $A0) then
    Result := Chr
  else
    case Chr of
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $00A1; // INVERTED EXCLAMATION MARK
      $A2: Result:= $00A2; // CENT SIGN
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $00A4; // CURRENCY SIGN
      $A5: Result:= $00A5; // YEN SIGN
      $A6: Result:= $00A6; // BROKEN BAR
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $00A8; // DIAERESIS
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $00AA; // FEMININE ORDINAL INDICATOR
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $00AF; // MACRON
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $00B4; // ACUTE ACCENT
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $00B8; // CEDILLA
      $B9: Result:= $00B9; // SUPERSCRIPT ONE
      $BA: Result:= $00BA; // MASCULINE ORDINAL INDICATOR
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $00BC; // VULGAR FRACTION ONE QUARTER
      $BD: Result:= $00BD; // VULGAR FRACTION ONE HALF
      $BE: Result:= $00BE; // VULGAR FRACTION THREE QUARTERS
      $BF: Result:= $00BF; // INVERTED QUESTION MARK
      $C0: Result:= $00C0; // LATIN CAPITAL LETTER A WITH GRAVE
      $C1: Result:= $00C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $C2: Result:= $00C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $C3: Result:= $00C3; // LATIN CAPITAL LETTER A WITH TILDE
      $C4: Result:= $00C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $C5: Result:= $00C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $C6: Result:= $00C6; // LATIN CAPITAL LETTER AE
      $C7: Result:= $00C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $C8: Result:= $00C8; // LATIN CAPITAL LETTER E WITH GRAVE
      $C9: Result:= $00C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $CA: Result:= $00CA; // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $CB: Result:= $00CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $CC: Result:= $00CC; // LATIN CAPITAL LETTER I WITH GRAVE
      $CD: Result:= $00CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $CE: Result:= $00CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $CF: Result:= $00CF; // LATIN CAPITAL LETTER I WITH DIAERESIS
      $D0: Result:= $011E; // LATIN CAPITAL LETTER G WITH BREVE
      $D1: Result:= $00D1; // LATIN CAPITAL LETTER N WITH TILDE
      $D2: Result:= $00D2; // LATIN CAPITAL LETTER O WITH GRAVE
      $D3: Result:= $00D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $D4: Result:= $00D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $D5: Result:= $00D5; // LATIN CAPITAL LETTER O WITH TILDE
      $D6: Result:= $00D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $00D8; // LATIN CAPITAL LETTER O WITH STROKE
      $D9: Result:= $00D9; // LATIN CAPITAL LETTER U WITH GRAVE
      $DA: Result:= $00DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $DB: Result:= $00DB; // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $DC: Result:= $00DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $DD: Result:= $0130; // LATIN CAPITAL LETTER I WITH DOT ABOVE
      $DE: Result:= $015E; // LATIN CAPITAL LETTER S WITH CEDILLA
      $DF: Result:= $00DF; // LATIN SMALL LETTER SHARP S
      $E0: Result:= $00E0; // LATIN SMALL LETTER A WITH GRAVE
      $E1: Result:= $00E1; // LATIN SMALL LETTER A WITH ACUTE
      $E2: Result:= $00E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $E3: Result:= $00E3; // LATIN SMALL LETTER A WITH TILDE
      $E4: Result:= $00E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $E5: Result:= $00E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $E6: Result:= $00E6; // LATIN SMALL LETTER AE
      $E7: Result:= $00E7; // LATIN SMALL LETTER C WITH CEDILLA
      $E8: Result:= $00E8; // LATIN SMALL LETTER E WITH GRAVE
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $00EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $EB: Result:= $00EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $EC: Result:= $00EC; // LATIN SMALL LETTER I WITH GRAVE
      $ED: Result:= $00ED; // LATIN SMALL LETTER I WITH ACUTE
      $EE: Result:= $00EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $EF: Result:= $00EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $F0: Result:= $011F; // LATIN SMALL LETTER G WITH BREVE
      $F1: Result:= $00F1; // LATIN SMALL LETTER N WITH TILDE
      $F2: Result:= $00F2; // LATIN SMALL LETTER O WITH GRAVE
      $F3: Result:= $00F3; // LATIN SMALL LETTER O WITH ACUTE
      $F4: Result:= $00F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $F5: Result:= $00F5; // LATIN SMALL LETTER O WITH TILDE
      $F6: Result:= $00F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $00F8; // LATIN SMALL LETTER O WITH STROKE
      $F9: Result:= $00F9; // LATIN SMALL LETTER U WITH GRAVE
      $FA: Result:= $00FA; // LATIN SMALL LETTER U WITH ACUTE
      $FB: Result:= $00FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $0131; // LATIN SMALL LETTER DOTLESS I
      $FE: Result:= $015F; // LATIN SMALL LETTER S WITH CEDILLA
      $FF: Result:= $00FF; // LATIN SMALL LETTER Y WITH DIAERESIS
      else Result:=Chr;
    end;
{$ENDIF}
  end;

function RtcAnsiToUnicodeCharISO8859_15(Chr: Byte): Word;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Chr;
{$ELSE}
  if (Chr < $A0) then
    Result := Chr
  else
    case Chr of
      $A0: Result:= $00A0; // NO-BREAK SPACE
      $A1: Result:= $00A1; // INVERTED EXCLAMATION MARK
      $A2: Result:= $00A2; // CENT SIGN
      $A3: Result:= $00A3; // POUND SIGN
      $A4: Result:= $20AC; // EURO SIGN
      $A5: Result:= $00A5; // YEN SIGN
      $A6: Result:= $0160; // LATIN CAPITAL LETTER S WITH CARON
      $A7: Result:= $00A7; // SECTION SIGN
      $A8: Result:= $0161; // LATIN SMALL LETTER S WITH CARON
      $A9: Result:= $00A9; // COPYRIGHT SIGN
      $AA: Result:= $00AA; // FEMININE ORDINAL INDICATOR
      $AB: Result:= $00AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $AC: Result:= $00AC; // NOT SIGN
      $AD: Result:= $00AD; // SOFT HYPHEN
      $AE: Result:= $00AE; // REGISTERED SIGN
      $AF: Result:= $00AF; // MACRON
      $B0: Result:= $00B0; // DEGREE SIGN
      $B1: Result:= $00B1; // PLUS-MINUS SIGN
      $B2: Result:= $00B2; // SUPERSCRIPT TWO
      $B3: Result:= $00B3; // SUPERSCRIPT THREE
      $B4: Result:= $017D; // LATIN CAPITAL LETTER Z WITH CARON
      $B5: Result:= $00B5; // MICRO SIGN
      $B6: Result:= $00B6; // PILCROW SIGN
      $B7: Result:= $00B7; // MIDDLE DOT
      $B8: Result:= $017E; // LATIN SMALL LETTER Z WITH CARON
      $B9: Result:= $00B9; // SUPERSCRIPT ONE
      $BA: Result:= $00BA; // MASCULINE ORDINAL INDICATOR
      $BB: Result:= $00BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $BC: Result:= $0152; // LATIN CAPITAL LIGATURE OE
      $BD: Result:= $0153; // LATIN SMALL LIGATURE OE
      $BE: Result:= $0178; // LATIN CAPITAL LETTER Y WITH DIAERESIS
      $BF: Result:= $00BF; // INVERTED QUESTION MARK
      $C0: Result:= $00C0; // LATIN CAPITAL LETTER A WITH GRAVE
      $C1: Result:= $00C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $C2: Result:= $00C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $C3: Result:= $00C3; // LATIN CAPITAL LETTER A WITH TILDE
      $C4: Result:= $00C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $C5: Result:= $00C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $C6: Result:= $00C6; // LATIN CAPITAL LETTER AE
      $C7: Result:= $00C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $C8: Result:= $00C8; // LATIN CAPITAL LETTER E WITH GRAVE
      $C9: Result:= $00C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $CA: Result:= $00CA; // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $CB: Result:= $00CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $CC: Result:= $00CC; // LATIN CAPITAL LETTER I WITH GRAVE
      $CD: Result:= $00CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $CE: Result:= $00CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $CF: Result:= $00CF; // LATIN CAPITAL LETTER I WITH DIAERESIS
      $D0: Result:= $00D0; // LATIN CAPITAL LETTER ETH
      $D1: Result:= $00D1; // LATIN CAPITAL LETTER N WITH TILDE
      $D2: Result:= $00D2; // LATIN CAPITAL LETTER O WITH GRAVE
      $D3: Result:= $00D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $D4: Result:= $00D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $D5: Result:= $00D5; // LATIN CAPITAL LETTER O WITH TILDE
      $D6: Result:= $00D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $D7: Result:= $00D7; // MULTIPLICATION SIGN
      $D8: Result:= $00D8; // LATIN CAPITAL LETTER O WITH STROKE
      $D9: Result:= $00D9; // LATIN CAPITAL LETTER U WITH GRAVE
      $DA: Result:= $00DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $DB: Result:= $00DB; // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $DC: Result:= $00DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $DD: Result:= $00DD; // LATIN CAPITAL LETTER Y WITH ACUTE
      $DE: Result:= $00DE; // LATIN CAPITAL LETTER THORN
      $DF: Result:= $00DF; // LATIN SMALL LETTER SHARP S
      $E0: Result:= $00E0; // LATIN SMALL LETTER A WITH GRAVE
      $E1: Result:= $00E1; // LATIN SMALL LETTER A WITH ACUTE
      $E2: Result:= $00E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $E3: Result:= $00E3; // LATIN SMALL LETTER A WITH TILDE
      $E4: Result:= $00E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $E5: Result:= $00E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $E6: Result:= $00E6; // LATIN SMALL LETTER AE
      $E7: Result:= $00E7; // LATIN SMALL LETTER C WITH CEDILLA
      $E8: Result:= $00E8; // LATIN SMALL LETTER E WITH GRAVE
      $E9: Result:= $00E9; // LATIN SMALL LETTER E WITH ACUTE
      $EA: Result:= $00EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $EB: Result:= $00EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $EC: Result:= $00EC; // LATIN SMALL LETTER I WITH GRAVE
      $ED: Result:= $00ED; // LATIN SMALL LETTER I WITH ACUTE
      $EE: Result:= $00EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $EF: Result:= $00EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $F0: Result:= $00F0; // LATIN SMALL LETTER ETH
      $F1: Result:= $00F1; // LATIN SMALL LETTER N WITH TILDE
      $F2: Result:= $00F2; // LATIN SMALL LETTER O WITH GRAVE
      $F3: Result:= $00F3; // LATIN SMALL LETTER O WITH ACUTE
      $F4: Result:= $00F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $F5: Result:= $00F5; // LATIN SMALL LETTER O WITH TILDE
      $F6: Result:= $00F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $F7: Result:= $00F7; // DIVISION SIGN
      $F8: Result:= $00F8; // LATIN SMALL LETTER O WITH STROKE
      $F9: Result:= $00F9; // LATIN SMALL LETTER U WITH GRAVE
      $FA: Result:= $00FA; // LATIN SMALL LETTER U WITH ACUTE
      $FB: Result:= $00FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $FC: Result:= $00FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $FD: Result:= $00FD; // LATIN SMALL LETTER Y WITH ACUTE
      $FE: Result:= $00FE; // LATIN SMALL LETTER THORN
      $FF: Result:= $00FF; // LATIN SMALL LETTER Y WITH DIAERESIS
      else Result:=Chr;
    end;
{$ENDIF}
  end;

(****** Unicode to ANSI char conversion functions *****)

function RtcUnicodeToAnsiCharNone(Chr: Word): Byte;
  begin
  Result := Byte(Chr);
  end;

function RtcUnicodeToAnsiCharWin1250(Chr: Word): Byte;
  begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
       $20AC: Result:= $80; // EURO SIGN
       $201A: Result:= $82; // SINGLE LOW-9 QUOTATION MARK
       $201E: Result:= $84; // DOUBLE LOW-9 QUOTATION MARK
       $2026: Result:= $85; // HORIZONTAL ELLIPSIS
       $2020: Result:= $86; // DAGGER
       $2021: Result:= $87; // DOUBLE DAGGER
       $2030: Result:= $89; // PER MILLE SIGN
       $0160: Result:= $8A; // LATIN CAPITAL LETTER S WITH CARON
       $2039: Result:= $8B; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
       $015A: Result:= $8C; // LATIN CAPITAL LETTER S WITH ACUTE
       $0164: Result:= $8D; // LATIN CAPITAL LETTER T WITH CARON
       $017D: Result:= $8E; // LATIN CAPITAL LETTER Z WITH CARON
       $0179: Result:= $8F; // LATIN CAPITAL LETTER Z WITH ACUTE
       $2018: Result:= $91; // LEFT SINGLE QUOTATION MARK
       $2019: Result:= $92; // RIGHT SINGLE QUOTATION MARK
       $201C: Result:= $93; // LEFT DOUBLE QUOTATION MARK
       $201D: Result:= $94; // RIGHT DOUBLE QUOTATION MARK
       $2022: Result:= $95; // BULLET
       $2013: Result:= $96; // EN DASH
       $2014: Result:= $97; // EM DASH
       $2122: Result:= $99; // TRADE MARK SIGN
       $0161: Result:= $9A; // LATIN SMALL LETTER S WITH CARON
       $203A: Result:= $9B; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
       $015B: Result:= $9C; // LATIN SMALL LETTER S WITH ACUTE
       $0165: Result:= $9D; // LATIN SMALL LETTER T WITH CARON
       $017E: Result:= $9E; // LATIN SMALL LETTER Z WITH CARON
       $017A: Result:= $9F; // LATIN SMALL LETTER Z WITH ACUTE
       $00A0: Result:= $A0; // NO-BREAK SPACE
       $02C7: Result:= $A1; // CARON
       $02D8: Result:= $A2; // BREVE
       $0141: Result:= $A3; // LATIN CAPITAL LETTER L WITH STROKE
       $00A4: Result:= $A4; // CURRENCY SIGN
       $0104: Result:= $A5; // LATIN CAPITAL LETTER A WITH OGONEK
       $00A6: Result:= $A6; // BROKEN BAR
       $00A7: Result:= $A7; // SECTION SIGN
       $00A8: Result:= $A8; // DIAERESIS
       $00A9: Result:= $A9; // COPYRIGHT SIGN
       $015E: Result:= $AA; // LATIN CAPITAL LETTER S WITH CEDILLA
       $00AB: Result:= $AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
       $00AC: Result:= $AC; // NOT SIGN
       $00AD: Result:= $AD; // SOFT HYPHEN
       $00AE: Result:= $AE; // REGISTERED SIGN
       $017B: Result:= $AF; // LATIN CAPITAL LETTER Z WITH DOT ABOVE
       $00B0: Result:= $B0; // DEGREE SIGN
       $00B1: Result:= $B1; // PLUS-MINUS SIGN
       $02DB: Result:= $B2; // OGONEK
       $0142: Result:= $B3; // LATIN SMALL LETTER L WITH STROKE
       $00B4: Result:= $B4; // ACUTE ACCENT
       $00B5: Result:= $B5; // MICRO SIGN
       $00B6: Result:= $B6; // PILCROW SIGN
       $00B7: Result:= $B7; // MIDDLE DOT
       $00B8: Result:= $B8; // CEDILLA
       $0105: Result:= $B9; // LATIN SMALL LETTER A WITH OGONEK
       $015F: Result:= $BA; // LATIN SMALL LETTER S WITH CEDILLA
       $00BB: Result:= $BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
       $013D: Result:= $BC; // LATIN CAPITAL LETTER L WITH CARON
       $02DD: Result:= $BD; // DOUBLE ACUTE ACCENT
       $013E: Result:= $BE; // LATIN SMALL LETTER L WITH CARON
       $017C: Result:= $BF; // LATIN SMALL LETTER Z WITH DOT ABOVE
       $0154: Result:= $C0; // LATIN CAPITAL LETTER R WITH ACUTE
       $00C1: Result:= $C1; // LATIN CAPITAL LETTER A WITH ACUTE
       $00C2: Result:= $C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
       $0102: Result:= $C3; // LATIN CAPITAL LETTER A WITH BREVE
       $00C4: Result:= $C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
       $0139: Result:= $C5; // LATIN CAPITAL LETTER L WITH ACUTE
       $0106: Result:= $C6; // LATIN CAPITAL LETTER C WITH ACUTE
       $00C7: Result:= $C7; // LATIN CAPITAL LETTER C WITH CEDILLA
       $010C: Result:= $C8; // LATIN CAPITAL LETTER C WITH CARON
       $00C9: Result:= $C9; // LATIN CAPITAL LETTER E WITH ACUTE
       $0118: Result:= $CA; // LATIN CAPITAL LETTER E WITH OGONEK
       $00CB: Result:= $CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
       $011A: Result:= $CC; // LATIN CAPITAL LETTER E WITH CARON
       $00CD: Result:= $CD; // LATIN CAPITAL LETTER I WITH ACUTE
       $00CE: Result:= $CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
       $010E: Result:= $CF; // LATIN CAPITAL LETTER D WITH CARON
       $0110: Result:= $D0; // LATIN CAPITAL LETTER D WITH STROKE
       $0143: Result:= $D1; // LATIN CAPITAL LETTER N WITH ACUTE
       $0147: Result:= $D2; // LATIN CAPITAL LETTER N WITH CARON
       $00D3: Result:= $D3; // LATIN CAPITAL LETTER O WITH ACUTE
       $00D4: Result:= $D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
       $0150: Result:= $D5; // LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
       $00D6: Result:= $D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
       $00D7: Result:= $D7; // MULTIPLICATION SIGN
       $0158: Result:= $D8; // LATIN CAPITAL LETTER R WITH CARON
       $016E: Result:= $D9; // LATIN CAPITAL LETTER U WITH RING ABOVE
       $00DA: Result:= $DA; // LATIN CAPITAL LETTER U WITH ACUTE
       $0170: Result:= $DB; // LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
       $00DC: Result:= $DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
       $00DD: Result:= $DD; // LATIN CAPITAL LETTER Y WITH ACUTE
       $0162: Result:= $DE; // LATIN CAPITAL LETTER T WITH CEDILLA
       $00DF: Result:= $DF; // LATIN SMALL LETTER SHARP S
       $0155: Result:= $E0; // LATIN SMALL LETTER R WITH ACUTE
       $00E1: Result:= $E1; // LATIN SMALL LETTER A WITH ACUTE
       $00E2: Result:= $E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
       $0103: Result:= $E3; // LATIN SMALL LETTER A WITH BREVE
       $00E4: Result:= $E4; // LATIN SMALL LETTER A WITH DIAERESIS
       $013A: Result:= $E5; // LATIN SMALL LETTER L WITH ACUTE
       $0107: Result:= $E6; // LATIN SMALL LETTER C WITH ACUTE
       $00E7: Result:= $E7; // LATIN SMALL LETTER C WITH CEDILLA
       $010D: Result:= $E8; // LATIN SMALL LETTER C WITH CARON
       $00E9: Result:= $E9; // LATIN SMALL LETTER E WITH ACUTE
       $0119: Result:= $EA; // LATIN SMALL LETTER E WITH OGONEK
       $00EB: Result:= $EB; // LATIN SMALL LETTER E WITH DIAERESIS
       $011B: Result:= $EC; // LATIN SMALL LETTER E WITH CARON
       $00ED: Result:= $ED; // LATIN SMALL LETTER I WITH ACUTE
       $00EE: Result:= $EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
       $010F: Result:= $EF; // LATIN SMALL LETTER D WITH CARON
       $0111: Result:= $F0; // LATIN SMALL LETTER D WITH STROKE
       $0144: Result:= $F1; // LATIN SMALL LETTER N WITH ACUTE
       $0148: Result:= $F2; // LATIN SMALL LETTER N WITH CARON
       $00F3: Result:= $F3; // LATIN SMALL LETTER O WITH ACUTE
       $00F4: Result:= $F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
       $0151: Result:= $F5; // LATIN SMALL LETTER O WITH DOUBLE ACUTE
       $00F6: Result:= $F6; // LATIN SMALL LETTER O WITH DIAERESIS
       $00F7: Result:= $F7; // DIVISION SIGN
       $0159: Result:= $F8; // LATIN SMALL LETTER R WITH CARON
       $016F: Result:= $F9; // LATIN SMALL LETTER U WITH RING ABOVE
       $00FA: Result:= $FA; // LATIN SMALL LETTER U WITH ACUTE
       $0171: Result:= $FB; // LATIN SMALL LETTER U WITH DOUBLE ACUTE
       $00FC: Result:= $FC; // LATIN SMALL LETTER U WITH DIAERESIS
       $00FD: Result:= $FD; // LATIN SMALL LETTER Y WITH ACUTE
       $0163: Result:= $FE; // LATIN SMALL LETTER T WITH CEDILLA
       $02D9: Result:= $FF; // DOT ABOVE
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharWin1251(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $0402: Result:= $80; // CYRILLIC CAPITAL LETTER DJE
      $0403: Result:= $81; // CYRILLIC CAPITAL LETTER GJE
      $201A: Result:= $82; // SINGLE LOW-9 QUOTATION MARK
      $0453: Result:= $83; // CYRILLIC SMALL LETTER GJE
      $201E: Result:= $84; // DOUBLE LOW-9 QUOTATION MARK
      $2026: Result:= $85; // HORIZONTAL ELLIPSIS
      $2020: Result:= $86; // DAGGER
      $2021: Result:= $87; // DOUBLE DAGGER
      $20AC: Result:= $88; // EURO SIGN
      $2030: Result:= $89; // PER MILLE SIGN
      $0409: Result:= $8A; // CYRILLIC CAPITAL LETTER LJE
      $2039: Result:= $8B; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $040A: Result:= $8C; // CYRILLIC CAPITAL LETTER NJE
      $040C: Result:= $8D; // CYRILLIC CAPITAL LETTER KJE
      $040B: Result:= $8E; // CYRILLIC CAPITAL LETTER TSHE
      $040F: Result:= $8F; // CYRILLIC CAPITAL LETTER DZHE
      $0452: Result:= $90; // CYRILLIC SMALL LETTER DJE
      $2018: Result:= $91; // LEFT SINGLE QUOTATION MARK
      $2019: Result:= $92; // RIGHT SINGLE QUOTATION MARK
      $201C: Result:= $93; // LEFT DOUBLE QUOTATION MARK
      $201D: Result:= $94; // RIGHT DOUBLE QUOTATION MARK
      $2022: Result:= $95; // BULLET
      $2013: Result:= $96; // EN DASH
      $2014: Result:= $97; // EM DASH
      $2122: Result:= $99; // TRADE MARK SIGN
      $0459: Result:= $9A; // CYRILLIC SMALL LETTER LJE
      $203A: Result:= $9B; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $045A: Result:= $9C; // CYRILLIC SMALL LETTER NJE
      $045C: Result:= $9D; // CYRILLIC SMALL LETTER KJE
      $045B: Result:= $9E; // CYRILLIC SMALL LETTER TSHE
      $045F: Result:= $9F; // CYRILLIC SMALL LETTER DZHE
      $00A0: Result:= $A0; // NO-BREAK SPACE
      $040E: Result:= $A1; // CYRILLIC CAPITAL LETTER SHORT U
      $045E: Result:= $A2; // CYRILLIC SMALL LETTER SHORT U
      $0408: Result:= $A3; // CYRILLIC CAPITAL LETTER JE
      $00A4: Result:= $A4; // CURRENCY SIGN
      $0490: Result:= $A5; // CYRILLIC CAPITAL LETTER GHE WITH UPTURN
      $00A6: Result:= $A6; // BROKEN BAR
      $00A7: Result:= $A7; // SECTION SIGN
      $0401: Result:= $A8; // CYRILLIC CAPITAL LETTER IO
      $00A9: Result:= $A9; // COPYRIGHT SIGN
      $0404: Result:= $AA; // CYRILLIC CAPITAL LETTER UKRAINIAN IE
      $00AB: Result:= $AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; // NOT SIGN
      $00AD: Result:= $AD; // SOFT HYPHEN
      $00AE: Result:= $AE; // REGISTERED SIGN
      $0407: Result:= $AF; // CYRILLIC CAPITAL LETTER YI
      $00B0: Result:= $B0; // DEGREE SIGN
      $00B1: Result:= $B1; // PLUS-MINUS SIGN
      $0406: Result:= $B2; // CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
      $0456: Result:= $B3; // CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
      $0491: Result:= $B4; // CYRILLIC SMALL LETTER GHE WITH UPTURN
      $00B5: Result:= $B5; // MICRO SIGN
      $00B6: Result:= $B6; // PILCROW SIGN
      $00B7: Result:= $B7; // MIDDLE DOT
      $0451: Result:= $B8; // CYRILLIC SMALL LETTER IO
      $2116: Result:= $B9; // NUMERO SIGN
      $0454: Result:= $BA; // CYRILLIC SMALL LETTER UKRAINIAN IE
      $00BB: Result:= $BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $0458: Result:= $BC; // CYRILLIC SMALL LETTER JE
      $0405: Result:= $BD; // CYRILLIC CAPITAL LETTER DZE
      $0455: Result:= $BE; // CYRILLIC SMALL LETTER DZE
      $0457: Result:= $BF; // CYRILLIC SMALL LETTER YI
      $0410: Result:= $C0; // CYRILLIC CAPITAL LETTER A
      $0411: Result:= $C1; // CYRILLIC CAPITAL LETTER BE
      $0412: Result:= $C2; // CYRILLIC CAPITAL LETTER VE
      $0413: Result:= $C3; // CYRILLIC CAPITAL LETTER GHE
      $0414: Result:= $C4; // CYRILLIC CAPITAL LETTER DE
      $0415: Result:= $C5; // CYRILLIC CAPITAL LETTER IE
      $0416: Result:= $C6; // CYRILLIC CAPITAL LETTER ZHE
      $0417: Result:= $C7; // CYRILLIC CAPITAL LETTER ZE
      $0418: Result:= $C8; // CYRILLIC CAPITAL LETTER I
      $0419: Result:= $C9; // CYRILLIC CAPITAL LETTER SHORT I
      $041A: Result:= $CA; // CYRILLIC CAPITAL LETTER KA
      $041B: Result:= $CB; // CYRILLIC CAPITAL LETTER EL
      $041C: Result:= $CC; // CYRILLIC CAPITAL LETTER EM
      $041D: Result:= $CD; // CYRILLIC CAPITAL LETTER EN
      $041E: Result:= $CE; // CYRILLIC CAPITAL LETTER O
      $041F: Result:= $CF; // CYRILLIC CAPITAL LETTER PE
      $0420: Result:= $D0; // CYRILLIC CAPITAL LETTER ER
      $0421: Result:= $D1; // CYRILLIC CAPITAL LETTER ES
      $0422: Result:= $D2; // CYRILLIC CAPITAL LETTER TE
      $0423: Result:= $D3; // CYRILLIC CAPITAL LETTER U
      $0424: Result:= $D4; // CYRILLIC CAPITAL LETTER EF
      $0425: Result:= $D5; // CYRILLIC CAPITAL LETTER HA
      $0426: Result:= $D6; // CYRILLIC CAPITAL LETTER TSE
      $0427: Result:= $D7; // CYRILLIC CAPITAL LETTER CHE
      $0428: Result:= $D8; // CYRILLIC CAPITAL LETTER SHA
      $0429: Result:= $D9; // CYRILLIC CAPITAL LETTER SHCHA
      $042A: Result:= $DA; // CYRILLIC CAPITAL LETTER HARD SIGN
      $042B: Result:= $DB; // CYRILLIC CAPITAL LETTER YERU
      $042C: Result:= $DC; // CYRILLIC CAPITAL LETTER SOFT SIGN
      $042D: Result:= $DD; // CYRILLIC CAPITAL LETTER E
      $042E: Result:= $DE; // CYRILLIC CAPITAL LETTER YU
      $042F: Result:= $DF; // CYRILLIC CAPITAL LETTER YA
      $0430: Result:= $E0; // CYRILLIC SMALL LETTER A
      $0431: Result:= $E1; // CYRILLIC SMALL LETTER BE
      $0432: Result:= $E2; // CYRILLIC SMALL LETTER VE
      $0433: Result:= $E3; // CYRILLIC SMALL LETTER GHE
      $0434: Result:= $E4; // CYRILLIC SMALL LETTER DE
      $0435: Result:= $E5; // CYRILLIC SMALL LETTER IE
      $0436: Result:= $E6; // CYRILLIC SMALL LETTER ZHE
      $0437: Result:= $E7; // CYRILLIC SMALL LETTER ZE
      $0438: Result:= $E8; // CYRILLIC SMALL LETTER I
      $0439: Result:= $E9; // CYRILLIC SMALL LETTER SHORT I
      $043A: Result:= $EA; // CYRILLIC SMALL LETTER KA
      $043B: Result:= $EB; // CYRILLIC SMALL LETTER EL
      $043C: Result:= $EC; // CYRILLIC SMALL LETTER EM
      $043D: Result:= $ED; // CYRILLIC SMALL LETTER EN
      $043E: Result:= $EE; // CYRILLIC SMALL LETTER O
      $043F: Result:= $EF; // CYRILLIC SMALL LETTER PE
      $0440: Result:= $F0; // CYRILLIC SMALL LETTER ER
      $0441: Result:= $F1; // CYRILLIC SMALL LETTER ES
      $0442: Result:= $F2; // CYRILLIC SMALL LETTER TE
      $0443: Result:= $F3; // CYRILLIC SMALL LETTER U
      $0444: Result:= $F4; // CYRILLIC SMALL LETTER EF
      $0445: Result:= $F5; // CYRILLIC SMALL LETTER HA
      $0446: Result:= $F6; // CYRILLIC SMALL LETTER TSE
      $0447: Result:= $F7; // CYRILLIC SMALL LETTER CHE
      $0448: Result:= $F8; // CYRILLIC SMALL LETTER SHA
      $0449: Result:= $F9; // CYRILLIC SMALL LETTER SHCHA
      $044A: Result:= $FA; // CYRILLIC SMALL LETTER HARD SIGN
      $044B: Result:= $FB; // CYRILLIC SMALL LETTER YERU
      $044C: Result:= $FC; // CYRILLIC SMALL LETTER SOFT SIGN
      $044D: Result:= $FD; // CYRILLIC SMALL LETTER E
      $044E: Result:= $FE; // CYRILLIC SMALL LETTER YU
      $044F: Result:= $FF; // CYRILLIC SMALL LETTER YA
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharWin1252(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $20AC: Result:= $80; // EURO SIGN
      $201A: Result:= $82; // SINGLE LOW-9 QUOTATION MARK
      $0192: Result:= $83; // LATIN SMALL LETTER F WITH HOOK
      $201E: Result:= $84; // DOUBLE LOW-9 QUOTATION MARK
      $2026: Result:= $85; // HORIZONTAL ELLIPSIS
      $2020: Result:= $86; // DAGGER
      $2021: Result:= $87; // DOUBLE DAGGER
      $02C6: Result:= $88; // MODIFIER LETTER CIRCUMFLEX ACCENT
      $2030: Result:= $89; // PER MILLE SIGN
      $0160: Result:= $8A; // LATIN CAPITAL LETTER S WITH CARON
      $2039: Result:= $8B; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $0152: Result:= $8C; // LATIN CAPITAL LIGATURE OE
      $017D: Result:= $8E; // LATIN CAPITAL LETTER Z WITH CARON
      $2018: Result:= $91; // LEFT SINGLE QUOTATION MARK
      $2019: Result:= $92; // RIGHT SINGLE QUOTATION MARK
      $201C: Result:= $93; // LEFT DOUBLE QUOTATION MARK
      $201D: Result:= $94; // RIGHT DOUBLE QUOTATION MARK
      $2022: Result:= $95; // BULLET
      $2013: Result:= $96; // EN DASH
      $2014: Result:= $97; // EM DASH
      $02DC: Result:= $98; // SMALL TILDE
      $2122: Result:= $99; // TRADE MARK SIGN
      $0161: Result:= $9A; // LATIN SMALL LETTER S WITH CARON
      $203A: Result:= $9B; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $0153: Result:= $9C; // LATIN SMALL LIGATURE OE
      $017E: Result:= $9E; // LATIN SMALL LETTER Z WITH CARON
      $0178: Result:= $9F; // LATIN CAPITAL LETTER Y WITH DIAERESIS
      $00A0: Result:= $A0; // NO-BREAK SPACE
      $00A1: Result:= $A1; // INVERTED EXCLAMATION MARK
      $00A2: Result:= $A2; // CENT SIGN
      $00A3: Result:= $A3; // POUND SIGN
      $00A4: Result:= $A4; // CURRENCY SIGN
      $00A5: Result:= $A5; // YEN SIGN
      $00A6: Result:= $A6; // BROKEN BAR
      $00A7: Result:= $A7; // SECTION SIGN
      $00A8: Result:= $A8; // DIAERESIS
      $00A9: Result:= $A9; // COPYRIGHT SIGN
      $00AA: Result:= $AA; // FEMININE ORDINAL INDICATOR
      $00AB: Result:= $AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; // NOT SIGN
      $00AD: Result:= $AD; // SOFT HYPHEN
      $00AE: Result:= $AE; // REGISTERED SIGN
      $00AF: Result:= $AF; // MACRON
      $00B0: Result:= $B0; // DEGREE SIGN
      $00B1: Result:= $B1; // PLUS-MINUS SIGN
      $00B2: Result:= $B2; // SUPERSCRIPT TWO
      $00B3: Result:= $B3; // SUPERSCRIPT THREE
      $00B4: Result:= $B4; // ACUTE ACCENT
      $00B5: Result:= $B5; // MICRO SIGN
      $00B6: Result:= $B6; // PILCROW SIGN
      $00B7: Result:= $B7; // MIDDLE DOT
      $00B8: Result:= $B8; // CEDILLA
      $00B9: Result:= $B9; // SUPERSCRIPT ONE
      $00BA: Result:= $BA; // MASCULINE ORDINAL INDICATOR
      $00BB: Result:= $BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00BC: Result:= $BC; // VULGAR FRACTION ONE QUARTER
      $00BD: Result:= $BD; // VULGAR FRACTION ONE HALF
      $00BE: Result:= $BE; // VULGAR FRACTION THREE QUARTERS
      $00BF: Result:= $BF; // INVERTED QUESTION MARK
      $00C0: Result:= $C0; // LATIN CAPITAL LETTER A WITH GRAVE
      $00C1: Result:= $C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $00C2: Result:= $C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $00C3: Result:= $C3; // LATIN CAPITAL LETTER A WITH TILDE
      $00C4: Result:= $C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $00C5: Result:= $C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $00C6: Result:= $C6; // LATIN CAPITAL LETTER AE
      $00C7: Result:= $C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $00C8: Result:= $C8; // LATIN CAPITAL LETTER E WITH GRAVE
      $00C9: Result:= $C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $00CA: Result:= $CA; // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $00CB: Result:= $CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $00CC: Result:= $CC; // LATIN CAPITAL LETTER I WITH GRAVE
      $00CD: Result:= $CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $00CE: Result:= $CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $00CF: Result:= $CF; // LATIN CAPITAL LETTER I WITH DIAERESIS
      $00D0: Result:= $D0; // LATIN CAPITAL LETTER ETH
      $00D1: Result:= $D1; // LATIN CAPITAL LETTER N WITH TILDE
      $00D2: Result:= $D2; // LATIN CAPITAL LETTER O WITH GRAVE
      $00D3: Result:= $D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $00D4: Result:= $D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $00D5: Result:= $D5; // LATIN CAPITAL LETTER O WITH TILDE
      $00D6: Result:= $D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $00D7: Result:= $D7; // MULTIPLICATION SIGN
      $00D8: Result:= $D8; // LATIN CAPITAL LETTER O WITH STROKE
      $00D9: Result:= $D9; // LATIN CAPITAL LETTER U WITH GRAVE
      $00DA: Result:= $DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $00DB: Result:= $DB; // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $00DC: Result:= $DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $00DD: Result:= $DD; // LATIN CAPITAL LETTER Y WITH ACUTE
      $00DE: Result:= $DE; // LATIN CAPITAL LETTER THORN
      $00DF: Result:= $DF; // LATIN SMALL LETTER SHARP S
      $00E0: Result:= $E0; // LATIN SMALL LETTER A WITH GRAVE
      $00E1: Result:= $E1; // LATIN SMALL LETTER A WITH ACUTE
      $00E2: Result:= $E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $00E3: Result:= $E3; // LATIN SMALL LETTER A WITH TILDE
      $00E4: Result:= $E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $00E5: Result:= $E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $00E6: Result:= $E6; // LATIN SMALL LETTER AE
      $00E7: Result:= $E7; // LATIN SMALL LETTER C WITH CEDILLA
      $00E8: Result:= $E8; // LATIN SMALL LETTER E WITH GRAVE
      $00E9: Result:= $E9; // LATIN SMALL LETTER E WITH ACUTE
      $00EA: Result:= $EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $00EB: Result:= $EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $00EC: Result:= $EC; // LATIN SMALL LETTER I WITH GRAVE
      $00ED: Result:= $ED; // LATIN SMALL LETTER I WITH ACUTE
      $00EE: Result:= $EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $00EF: Result:= $EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $00F0: Result:= $F0; // LATIN SMALL LETTER ETH
      $00F1: Result:= $F1; // LATIN SMALL LETTER N WITH TILDE
      $00F2: Result:= $F2; // LATIN SMALL LETTER O WITH GRAVE
      $00F3: Result:= $F3; // LATIN SMALL LETTER O WITH ACUTE
      $00F4: Result:= $F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $00F5: Result:= $F5; // LATIN SMALL LETTER O WITH TILDE
      $00F6: Result:= $F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $00F7: Result:= $F7; // DIVISION SIGN
      $00F8: Result:= $F8; // LATIN SMALL LETTER O WITH STROKE
      $00F9: Result:= $F9; // LATIN SMALL LETTER U WITH GRAVE
      $00FA: Result:= $FA; // LATIN SMALL LETTER U WITH ACUTE
      $00FB: Result:= $FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $00FC: Result:= $FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $00FD: Result:= $FD; // LATIN SMALL LETTER Y WITH ACUTE
      $00FE: Result:= $FE; // LATIN SMALL LETTER THORN
      $00FF: Result:= $FF; // LATIN SMALL LETTER Y WITH DIAERESIS
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharWin1253(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $20AC: Result:= $80; // EURO SIGN
      $201A: Result:= $82; // SINGLE LOW-9 QUOTATION MARK
      $0192: Result:= $83; // LATIN SMALL LETTER F WITH HOOK
      $201E: Result:= $84; // DOUBLE LOW-9 QUOTATION MARK
      $2026: Result:= $85; // HORIZONTAL ELLIPSIS
      $2020: Result:= $86; // DAGGER
      $2021: Result:= $87; // DOUBLE DAGGER
      $2030: Result:= $89; // PER MILLE SIGN
      $2039: Result:= $8B; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $2018: Result:= $91; // LEFT SINGLE QUOTATION MARK
      $2019: Result:= $92; // RIGHT SINGLE QUOTATION MARK
      $201C: Result:= $93; // LEFT DOUBLE QUOTATION MARK
      $201D: Result:= $94; // RIGHT DOUBLE QUOTATION MARK
      $2022: Result:= $95; // BULLET
      $2013: Result:= $96; // EN DASH
      $2014: Result:= $97; // EM DASH
      $2122: Result:= $99; // TRADE MARK SIGN
      $203A: Result:= $9B; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $00A0: Result:= $A0; // NO-BREAK SPACE
      $0385: Result:= $A1; // GREEK DIALYTIKA TONOS
      $0386: Result:= $A2; // GREEK CAPITAL LETTER ALPHA WITH TONOS
      $00A3: Result:= $A3; // POUND SIGN
      $00A4: Result:= $A4; // CURRENCY SIGN
      $00A5: Result:= $A5; // YEN SIGN
      $00A6: Result:= $A6; // BROKEN BAR
      $00A7: Result:= $A7; // SECTION SIGN
      $00A8: Result:= $A8; // DIAERESIS
      $00A9: Result:= $A9; // COPYRIGHT SIGN
      $00AB: Result:= $AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; // NOT SIGN
      $00AD: Result:= $AD; // SOFT HYPHEN
      $00AE: Result:= $AE; // REGISTERED SIGN
      $2015: Result:= $AF; // HORIZONTAL BAR
      $00B0: Result:= $B0; // DEGREE SIGN
      $00B1: Result:= $B1; // PLUS-MINUS SIGN
      $00B2: Result:= $B2; // SUPERSCRIPT TWO
      $00B3: Result:= $B3; // SUPERSCRIPT THREE
      $0384: Result:= $B4; // GREEK TONOS
      $00B5: Result:= $B5; // MICRO SIGN
      $00B6: Result:= $B6; // PILCROW SIGN
      $00B7: Result:= $B7; // MIDDLE DOT
      $0388: Result:= $B8; // GREEK CAPITAL LETTER EPSILON WITH TONOS
      $0389: Result:= $B9; // GREEK CAPITAL LETTER ETA WITH TONOS
      $038A: Result:= $BA; // GREEK CAPITAL LETTER IOTA WITH TONOS
      $00BB: Result:= $BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $038C: Result:= $BC; // GREEK CAPITAL LETTER OMICRON WITH TONOS
      $00BD: Result:= $BD; // VULGAR FRACTION ONE HALF
      $038E: Result:= $BE; // GREEK CAPITAL LETTER UPSILON WITH TONOS
      $038F: Result:= $BF; // GREEK CAPITAL LETTER OMEGA WITH TONOS
      $0390: Result:= $C0; // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
      $0391: Result:= $C1; // GREEK CAPITAL LETTER ALPHA
      $0392: Result:= $C2; // GREEK CAPITAL LETTER BETA
      $0393: Result:= $C3; // GREEK CAPITAL LETTER GAMMA
      $0394: Result:= $C4; // GREEK CAPITAL LETTER DELTA
      $0395: Result:= $C5; // GREEK CAPITAL LETTER EPSILON
      $0396: Result:= $C6; // GREEK CAPITAL LETTER ZETA
      $0397: Result:= $C7; // GREEK CAPITAL LETTER ETA
      $0398: Result:= $C8; // GREEK CAPITAL LETTER THETA
      $0399: Result:= $C9; // GREEK CAPITAL LETTER IOTA
      $039A: Result:= $CA; // GREEK CAPITAL LETTER KAPPA
      $039B: Result:= $CB; // GREEK CAPITAL LETTER LAMDA
      $039C: Result:= $CC; // GREEK CAPITAL LETTER MU
      $039D: Result:= $CD; // GREEK CAPITAL LETTER NU
      $039E: Result:= $CE; // GREEK CAPITAL LETTER XI
      $039F: Result:= $CF; // GREEK CAPITAL LETTER OMICRON
      $03A0: Result:= $D0; // GREEK CAPITAL LETTER PI
      $03A1: Result:= $D1; // GREEK CAPITAL LETTER RHO
      $03A3: Result:= $D3; // GREEK CAPITAL LETTER SIGMA
      $03A4: Result:= $D4; // GREEK CAPITAL LETTER TAU
      $03A5: Result:= $D5; // GREEK CAPITAL LETTER UPSILON
      $03A6: Result:= $D6; // GREEK CAPITAL LETTER PHI
      $03A7: Result:= $D7; // GREEK CAPITAL LETTER CHI
      $03A8: Result:= $D8; // GREEK CAPITAL LETTER PSI
      $03A9: Result:= $D9; // GREEK CAPITAL LETTER OMEGA
      $03AA: Result:= $DA; // GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
      $03AB: Result:= $DB; // GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
      $03AC: Result:= $DC; // GREEK SMALL LETTER ALPHA WITH TONOS
      $03AD: Result:= $DD; // GREEK SMALL LETTER EPSILON WITH TONOS
      $03AE: Result:= $DE; // GREEK SMALL LETTER ETA WITH TONOS
      $03AF: Result:= $DF; // GREEK SMALL LETTER IOTA WITH TONOS
      $03B0: Result:= $E0; // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
      $03B1: Result:= $E1; // GREEK SMALL LETTER ALPHA
      $03B2: Result:= $E2; // GREEK SMALL LETTER BETA
      $03B3: Result:= $E3; // GREEK SMALL LETTER GAMMA
      $03B4: Result:= $E4; // GREEK SMALL LETTER DELTA
      $03B5: Result:= $E5; // GREEK SMALL LETTER EPSILON
      $03B6: Result:= $E6; // GREEK SMALL LETTER ZETA
      $03B7: Result:= $E7; // GREEK SMALL LETTER ETA
      $03B8: Result:= $E8; // GREEK SMALL LETTER THETA
      $03B9: Result:= $E9; // GREEK SMALL LETTER IOTA
      $03BA: Result:= $EA; // GREEK SMALL LETTER KAPPA
      $03BB: Result:= $EB; // GREEK SMALL LETTER LAMDA
      $03BC: Result:= $EC; // GREEK SMALL LETTER MU
      $03BD: Result:= $ED; // GREEK SMALL LETTER NU
      $03BE: Result:= $EE; // GREEK SMALL LETTER XI
      $03BF: Result:= $EF; // GREEK SMALL LETTER OMICRON
      $03C0: Result:= $F0; // GREEK SMALL LETTER PI
      $03C1: Result:= $F1; // GREEK SMALL LETTER RHO
      $03C2: Result:= $F2; // GREEK SMALL LETTER FINAL SIGMA
      $03C3: Result:= $F3; // GREEK SMALL LETTER SIGMA
      $03C4: Result:= $F4; // GREEK SMALL LETTER TAU
      $03C5: Result:= $F5; // GREEK SMALL LETTER UPSILON
      $03C6: Result:= $F6; // GREEK SMALL LETTER PHI
      $03C7: Result:= $F7; // GREEK SMALL LETTER CHI
      $03C8: Result:= $F8; // GREEK SMALL LETTER PSI
      $03C9: Result:= $F9; // GREEK SMALL LETTER OMEGA
      $03CA: Result:= $FA; // GREEK SMALL LETTER IOTA WITH DIALYTIKA
      $03CB: Result:= $FB; // GREEK SMALL LETTER UPSILON WITH DIALYTIKA
      $03CC: Result:= $FC; // GREEK SMALL LETTER OMICRON WITH TONOS
      $03CD: Result:= $FD; // GREEK SMALL LETTER UPSILON WITH TONOS
      $03CE: Result:= $FE; // GREEK SMALL LETTER OMEGA WITH TONOS
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharWin1254(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $20AC: Result:= $80; // EURO SIGN
      $201A: Result:= $82; // SINGLE LOW-9 QUOTATION MARK
      $0192: Result:= $83; // LATIN SMALL LETTER F WITH HOOK
      $201E: Result:= $84; // DOUBLE LOW-9 QUOTATION MARK
      $2026: Result:= $85; // HORIZONTAL ELLIPSIS
      $2020: Result:= $86; // DAGGER
      $2021: Result:= $87; // DOUBLE DAGGER
      $02C6: Result:= $88; // MODIFIER LETTER CIRCUMFLEX ACCENT
      $2030: Result:= $89; // PER MILLE SIGN
      $0160: Result:= $8A; // LATIN CAPITAL LETTER S WITH CARON
      $2039: Result:= $8B; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $0152: Result:= $8C; // LATIN CAPITAL LIGATURE OE
      $2018: Result:= $91; // LEFT SINGLE QUOTATION MARK
      $2019: Result:= $92; // RIGHT SINGLE QUOTATION MARK
      $201C: Result:= $93; // LEFT DOUBLE QUOTATION MARK
      $201D: Result:= $94; // RIGHT DOUBLE QUOTATION MARK
      $2022: Result:= $95; // BULLET
      $2013: Result:= $96; // EN DASH
      $2014: Result:= $97; // EM DASH
      $02DC: Result:= $98; // SMALL TILDE
      $2122: Result:= $99; // TRADE MARK SIGN
      $0161: Result:= $9A; // LATIN SMALL LETTER S WITH CARON
      $203A: Result:= $9B; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $0153: Result:= $9C; // LATIN SMALL LIGATURE OE
      $0178: Result:= $9F; // LATIN CAPITAL LETTER Y WITH DIAERESIS
      $00A0: Result:= $A0; // NO-BREAK SPACE
      $00A1: Result:= $A1; // INVERTED EXCLAMATION MARK
      $00A2: Result:= $A2; // CENT SIGN
      $00A3: Result:= $A3; // POUND SIGN
      $00A4: Result:= $A4; // CURRENCY SIGN
      $00A5: Result:= $A5; // YEN SIGN
      $00A6: Result:= $A6; // BROKEN BAR
      $00A7: Result:= $A7; // SECTION SIGN
      $00A8: Result:= $A8; // DIAERESIS
      $00A9: Result:= $A9; // COPYRIGHT SIGN
      $00AA: Result:= $AA; // FEMININE ORDINAL INDICATOR
      $00AB: Result:= $AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; // NOT SIGN
      $00AD: Result:= $AD; // SOFT HYPHEN
      $00AE: Result:= $AE; // REGISTERED SIGN
      $00AF: Result:= $AF; // MACRON
      $00B0: Result:= $B0; // DEGREE SIGN
      $00B1: Result:= $B1; // PLUS-MINUS SIGN
      $00B2: Result:= $B2; // SUPERSCRIPT TWO
      $00B3: Result:= $B3; // SUPERSCRIPT THREE
      $00B4: Result:= $B4; // ACUTE ACCENT
      $00B5: Result:= $B5; // MICRO SIGN
      $00B6: Result:= $B6; // PILCROW SIGN
      $00B7: Result:= $B7; // MIDDLE DOT
      $00B8: Result:= $B8; // CEDILLA
      $00B9: Result:= $B9; // SUPERSCRIPT ONE
      $00BA: Result:= $BA; // MASCULINE ORDINAL INDICATOR
      $00BB: Result:= $BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00BC: Result:= $BC; // VULGAR FRACTION ONE QUARTER
      $00BD: Result:= $BD; // VULGAR FRACTION ONE HALF
      $00BE: Result:= $BE; // VULGAR FRACTION THREE QUARTERS
      $00BF: Result:= $BF; // INVERTED QUESTION MARK
      $00C0: Result:= $C0; // LATIN CAPITAL LETTER A WITH GRAVE
      $00C1: Result:= $C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $00C2: Result:= $C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $00C3: Result:= $C3; // LATIN CAPITAL LETTER A WITH TILDE
      $00C4: Result:= $C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $00C5: Result:= $C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $00C6: Result:= $C6; // LATIN CAPITAL LETTER AE
      $00C7: Result:= $C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $00C8: Result:= $C8; // LATIN CAPITAL LETTER E WITH GRAVE
      $00C9: Result:= $C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $00CA: Result:= $CA; // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $00CB: Result:= $CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $00CC: Result:= $CC; // LATIN CAPITAL LETTER I WITH GRAVE
      $00CD: Result:= $CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $00CE: Result:= $CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $00CF: Result:= $CF; // LATIN CAPITAL LETTER I WITH DIAERESIS
      $011E: Result:= $D0; // LATIN CAPITAL LETTER G WITH BREVE
      $00D1: Result:= $D1; // LATIN CAPITAL LETTER N WITH TILDE
      $00D2: Result:= $D2; // LATIN CAPITAL LETTER O WITH GRAVE
      $00D3: Result:= $D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $00D4: Result:= $D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $00D5: Result:= $D5; // LATIN CAPITAL LETTER O WITH TILDE
      $00D6: Result:= $D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $00D7: Result:= $D7; // MULTIPLICATION SIGN
      $00D8: Result:= $D8; // LATIN CAPITAL LETTER O WITH STROKE
      $00D9: Result:= $D9; // LATIN CAPITAL LETTER U WITH GRAVE
      $00DA: Result:= $DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $00DB: Result:= $DB; // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $00DC: Result:= $DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $0130: Result:= $DD; // LATIN CAPITAL LETTER I WITH DOT ABOVE
      $015E: Result:= $DE; // LATIN CAPITAL LETTER S WITH CEDILLA
      $00DF: Result:= $DF; // LATIN SMALL LETTER SHARP S
      $00E0: Result:= $E0; // LATIN SMALL LETTER A WITH GRAVE
      $00E1: Result:= $E1; // LATIN SMALL LETTER A WITH ACUTE
      $00E2: Result:= $E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $00E3: Result:= $E3; // LATIN SMALL LETTER A WITH TILDE
      $00E4: Result:= $E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $00E5: Result:= $E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $00E6: Result:= $E6; // LATIN SMALL LETTER AE
      $00E7: Result:= $E7; // LATIN SMALL LETTER C WITH CEDILLA
      $00E8: Result:= $E8; // LATIN SMALL LETTER E WITH GRAVE
      $00E9: Result:= $E9; // LATIN SMALL LETTER E WITH ACUTE
      $00EA: Result:= $EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $00EB: Result:= $EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $00EC: Result:= $EC; // LATIN SMALL LETTER I WITH GRAVE
      $00ED: Result:= $ED; // LATIN SMALL LETTER I WITH ACUTE
      $00EE: Result:= $EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $00EF: Result:= $EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $011F: Result:= $F0; // LATIN SMALL LETTER G WITH BREVE
      $00F1: Result:= $F1; // LATIN SMALL LETTER N WITH TILDE
      $00F2: Result:= $F2; // LATIN SMALL LETTER O WITH GRAVE
      $00F3: Result:= $F3; // LATIN SMALL LETTER O WITH ACUTE
      $00F4: Result:= $F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $00F5: Result:= $F5; // LATIN SMALL LETTER O WITH TILDE
      $00F6: Result:= $F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $00F7: Result:= $F7; // DIVISION SIGN
      $00F8: Result:= $F8; // LATIN SMALL LETTER O WITH STROKE
      $00F9: Result:= $F9; // LATIN SMALL LETTER U WITH GRAVE
      $00FA: Result:= $FA; // LATIN SMALL LETTER U WITH ACUTE
      $00FB: Result:= $FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $00FC: Result:= $FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $0131: Result:= $FD; // LATIN SMALL LETTER DOTLESS I
      $015F: Result:= $FE; // LATIN SMALL LETTER S WITH CEDILLA
      $00FF: Result:= $FF; // LATIN SMALL LETTER Y WITH DIAERESIS
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharWin1255(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $20AC: Result:= $80; // EURO SIGN
      $201A: Result:= $82; // SINGLE LOW-9 QUOTATION MARK
      $0192: Result:= $83; // LATIN SMALL LETTER F WITH HOOK
      $201E: Result:= $84; // DOUBLE LOW-9 QUOTATION MARK
      $2026: Result:= $85; // HORIZONTAL ELLIPSIS
      $2020: Result:= $86; // DAGGER
      $2021: Result:= $87; // DOUBLE DAGGER
      $02C6: Result:= $88; // MODIFIER LETTER CIRCUMFLEX ACCENT
      $2030: Result:= $89; // PER MILLE SIGN
      $2039: Result:= $8B; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $2018: Result:= $91; // LEFT SINGLE QUOTATION MARK
      $2019: Result:= $92; // RIGHT SINGLE QUOTATION MARK
      $201C: Result:= $93; // LEFT DOUBLE QUOTATION MARK
      $201D: Result:= $94; // RIGHT DOUBLE QUOTATION MARK
      $2022: Result:= $95; // BULLET
      $2013: Result:= $96; // EN DASH
      $2014: Result:= $97; // EM DASH
      $02DC: Result:= $98; // SMALL TILDE
      $2122: Result:= $99; // TRADE MARK SIGN
      $203A: Result:= $9B; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $00A0: Result:= $A0; // NO-BREAK SPACE
      $00A1: Result:= $A1; // INVERTED EXCLAMATION MARK
      $00A2: Result:= $A2; // CENT SIGN
      $00A3: Result:= $A3; // POUND SIGN
      $20AA: Result:= $A4; // NEW SHEQEL SIGN
      $00A5: Result:= $A5; // YEN SIGN
      $00A6: Result:= $A6; // BROKEN BAR
      $00A7: Result:= $A7; // SECTION SIGN
      $00A8: Result:= $A8; // DIAERESIS
      $00A9: Result:= $A9; // COPYRIGHT SIGN
      $00D7: Result:= $AA; // MULTIPLICATION SIGN
      $00AB: Result:= $AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; // NOT SIGN
      $00AD: Result:= $AD; // SOFT HYPHEN
      $00AE: Result:= $AE; // REGISTERED SIGN
      $00AF: Result:= $AF; // MACRON
      $00B0: Result:= $B0; // DEGREE SIGN
      $00B1: Result:= $B1; // PLUS-MINUS SIGN
      $00B2: Result:= $B2; // SUPERSCRIPT TWO
      $00B3: Result:= $B3; // SUPERSCRIPT THREE
      $00B4: Result:= $B4; // ACUTE ACCENT
      $00B5: Result:= $B5; // MICRO SIGN
      $00B6: Result:= $B6; // PILCROW SIGN
      $00B7: Result:= $B7; // MIDDLE DOT
      $00B8: Result:= $B8; // CEDILLA
      $00B9: Result:= $B9; // SUPERSCRIPT ONE
      $00F7: Result:= $BA; // DIVISION SIGN
      $00BB: Result:= $BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00BC: Result:= $BC; // VULGAR FRACTION ONE QUARTER
      $00BD: Result:= $BD; // VULGAR FRACTION ONE HALF
      $00BE: Result:= $BE; // VULGAR FRACTION THREE QUARTERS
      $00BF: Result:= $BF; // INVERTED QUESTION MARK
      $05B0: Result:= $C0; // HEBREW POINT SHEVA
      $05B1: Result:= $C1; // HEBREW POINT HATAF SEGOL
      $05B2: Result:= $C2; // HEBREW POINT HATAF PATAH
      $05B3: Result:= $C3; // HEBREW POINT HATAF QAMATS
      $05B4: Result:= $C4; // HEBREW POINT HIRIQ
      $05B5: Result:= $C5; // HEBREW POINT TSERE
      $05B6: Result:= $C6; // HEBREW POINT SEGOL
      $05B7: Result:= $C7; // HEBREW POINT PATAH
      $05B8: Result:= $C8; // HEBREW POINT QAMATS
      $05B9: Result:= $C9; // HEBREW POINT HOLAM
      $05BB: Result:= $CB; // HEBREW POINT QUBUTS
      $05BC: Result:= $CC; // HEBREW POINT DAGESH OR MAPIQ
      $05BD: Result:= $CD; // HEBREW POINT METEG
      $05BE: Result:= $CE; // HEBREW PUNCTUATION MAQAF
      $05BF: Result:= $CF; // HEBREW POINT RAFE
      $05C0: Result:= $D0; // HEBREW PUNCTUATION PASEQ
      $05C1: Result:= $D1; // HEBREW POINT SHIN DOT
      $05C2: Result:= $D2; // HEBREW POINT SIN DOT
      $05C3: Result:= $D3; // HEBREW PUNCTUATION SOF PASUQ
      $05F0: Result:= $D4; // HEBREW LIGATURE YIDDISH DOUBLE VAV
      $05F1: Result:= $D5; // HEBREW LIGATURE YIDDISH VAV YOD
      $05F2: Result:= $D6; // HEBREW LIGATURE YIDDISH DOUBLE YOD
      $05F3: Result:= $D7; // HEBREW PUNCTUATION GERESH
      $05F4: Result:= $D8; // HEBREW PUNCTUATION GERSHAYIM
      $05D0: Result:= $E0; // HEBREW LETTER ALEF
      $05D1: Result:= $E1; // HEBREW LETTER BET
      $05D2: Result:= $E2; // HEBREW LETTER GIMEL
      $05D3: Result:= $E3; // HEBREW LETTER DALET
      $05D4: Result:= $E4; // HEBREW LETTER HE
      $05D5: Result:= $E5; // HEBREW LETTER VAV
      $05D6: Result:= $E6; // HEBREW LETTER ZAYIN
      $05D7: Result:= $E7; // HEBREW LETTER HET
      $05D8: Result:= $E8; // HEBREW LETTER TET
      $05D9: Result:= $E9; // HEBREW LETTER YOD
      $05DA: Result:= $EA; // HEBREW LETTER FINAL KAF
      $05DB: Result:= $EB; // HEBREW LETTER KAF
      $05DC: Result:= $EC; // HEBREW LETTER LAMED
      $05DD: Result:= $ED; // HEBREW LETTER FINAL MEM
      $05DE: Result:= $EE; // HEBREW LETTER MEM
      $05DF: Result:= $EF; // HEBREW LETTER FINAL NUN
      $05E0: Result:= $F0; // HEBREW LETTER NUN
      $05E1: Result:= $F1; // HEBREW LETTER SAMEKH
      $05E2: Result:= $F2; // HEBREW LETTER AYIN
      $05E3: Result:= $F3; // HEBREW LETTER FINAL PE
      $05E4: Result:= $F4; // HEBREW LETTER PE
      $05E5: Result:= $F5; // HEBREW LETTER FINAL TSADI
      $05E6: Result:= $F6; // HEBREW LETTER TSADI
      $05E7: Result:= $F7; // HEBREW LETTER QOF
      $05E8: Result:= $F8; // HEBREW LETTER RESH
      $05E9: Result:= $F9; // HEBREW LETTER SHIN
      $05EA: Result:= $FA; // HEBREW LETTER TAV
      $200E: Result:= $FD; // LEFT-TO-RIGHT MARK
      $200F: Result:= $FE; // RIGHT-TO-LEFT MARK
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharWin1256(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $20AC: Result:= $80; // EURO SIGN
      $067E: Result:= $81; // ARABIC LETTER PEH
      $201A: Result:= $82; // SINGLE LOW-9 QUOTATION MARK
      $0192: Result:= $83; // LATIN SMALL LETTER F WITH HOOK
      $201E: Result:= $84; // DOUBLE LOW-9 QUOTATION MARK
      $2026: Result:= $85; // HORIZONTAL ELLIPSIS
      $2020: Result:= $86; // DAGGER
      $2021: Result:= $87; // DOUBLE DAGGER
      $02C6: Result:= $88; // MODIFIER LETTER CIRCUMFLEX ACCENT
      $2030: Result:= $89; // PER MILLE SIGN
      $0679: Result:= $8A; // ARABIC LETTER TTEH
      $2039: Result:= $8B; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $0152: Result:= $8C; // LATIN CAPITAL LIGATURE OE
      $0686: Result:= $8D; // ARABIC LETTER TCHEH
      $0698: Result:= $8E; // ARABIC LETTER JEH
      $0688: Result:= $8F; // ARABIC LETTER DDAL
      $06AF: Result:= $90; // ARABIC LETTER GAF
      $2018: Result:= $91; // LEFT SINGLE QUOTATION MARK
      $2019: Result:= $92; // RIGHT SINGLE QUOTATION MARK
      $201C: Result:= $93; // LEFT DOUBLE QUOTATION MARK
      $201D: Result:= $94; // RIGHT DOUBLE QUOTATION MARK
      $2022: Result:= $95; // BULLET
      $2013: Result:= $96; // EN DASH
      $2014: Result:= $97; // EM DASH
      $06A9: Result:= $98; // ARABIC LETTER KEHEH
      $2122: Result:= $99; // TRADE MARK SIGN
      $0691: Result:= $9A; // ARABIC LETTER RREH
      $203A: Result:= $9B; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $0153: Result:= $9C; // LATIN SMALL LIGATURE OE
      $200C: Result:= $9D; // ZERO WIDTH NON-JOINER
      $200D: Result:= $9E; // ZERO WIDTH JOINER
      $06BA: Result:= $9F; // ARABIC LETTER NOON GHUNNA
      $00A0: Result:= $A0; // NO-BREAK SPACE
      $060C: Result:= $A1; // ARABIC COMMA
      $00A2: Result:= $A2; // CENT SIGN
      $00A3: Result:= $A3; // POUND SIGN
      $00A4: Result:= $A4; // CURRENCY SIGN
      $00A5: Result:= $A5; // YEN SIGN
      $00A6: Result:= $A6; // BROKEN BAR
      $00A7: Result:= $A7; // SECTION SIGN
      $00A8: Result:= $A8; // DIAERESIS
      $00A9: Result:= $A9; // COPYRIGHT SIGN
      $06BE: Result:= $AA; // ARABIC LETTER HEH DOACHASHMEE
      $00AB: Result:= $AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; // NOT SIGN
      $00AD: Result:= $AD; // SOFT HYPHEN
      $00AE: Result:= $AE; // REGISTERED SIGN
      $00AF: Result:= $AF; // MACRON
      $00B0: Result:= $B0; // DEGREE SIGN
      $00B1: Result:= $B1; // PLUS-MINUS SIGN
      $00B2: Result:= $B2; // SUPERSCRIPT TWO
      $00B3: Result:= $B3; // SUPERSCRIPT THREE
      $00B4: Result:= $B4; // ACUTE ACCENT
      $00B5: Result:= $B5; // MICRO SIGN
      $00B6: Result:= $B6; // PILCROW SIGN
      $00B7: Result:= $B7; // MIDDLE DOT
      $00B8: Result:= $B8; // CEDILLA
      $00B9: Result:= $B9; // SUPERSCRIPT ONE
      $061B: Result:= $BA; // ARABIC SEMICOLON
      $00BB: Result:= $BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00BC: Result:= $BC; // VULGAR FRACTION ONE QUARTER
      $00BD: Result:= $BD; // VULGAR FRACTION ONE HALF
      $00BE: Result:= $BE; // VULGAR FRACTION THREE QUARTERS
      $061F: Result:= $BF; // ARABIC QUESTION MARK
      $06C1: Result:= $C0; // ARABIC LETTER HEH GOAL
      $0621: Result:= $C1; // ARABIC LETTER HAMZA
      $0622: Result:= $C2; // ARABIC LETTER ALEF WITH MADDA ABOVE
      $0623: Result:= $C3; // ARABIC LETTER ALEF WITH HAMZA ABOVE
      $0624: Result:= $C4; // ARABIC LETTER WAW WITH HAMZA ABOVE
      $0625: Result:= $C5; // ARABIC LETTER ALEF WITH HAMZA BELOW
      $0626: Result:= $C6; // ARABIC LETTER YEH WITH HAMZA ABOVE
      $0627: Result:= $C7; // ARABIC LETTER ALEF
      $0628: Result:= $C8; // ARABIC LETTER BEH
      $0629: Result:= $C9; // ARABIC LETTER TEH MARBUTA
      $062A: Result:= $CA; // ARABIC LETTER TEH
      $062B: Result:= $CB; // ARABIC LETTER THEH
      $062C: Result:= $CC; // ARABIC LETTER JEEM
      $062D: Result:= $CD; // ARABIC LETTER HAH
      $062E: Result:= $CE; // ARABIC LETTER KHAH
      $062F: Result:= $CF; // ARABIC LETTER DAL
      $0630: Result:= $D0; // ARABIC LETTER THAL
      $0631: Result:= $D1; // ARABIC LETTER REH
      $0632: Result:= $D2; // ARABIC LETTER ZAIN
      $0633: Result:= $D3; // ARABIC LETTER SEEN
      $0634: Result:= $D4; // ARABIC LETTER SHEEN
      $0635: Result:= $D5; // ARABIC LETTER SAD
      $0636: Result:= $D6; // ARABIC LETTER DAD
      $00D7: Result:= $D7; // MULTIPLICATION SIGN
      $0637: Result:= $D8; // ARABIC LETTER TAH
      $0638: Result:= $D9; // ARABIC LETTER ZAH
      $0639: Result:= $DA; // ARABIC LETTER AIN
      $063A: Result:= $DB; // ARABIC LETTER GHAIN
      $0640: Result:= $DC; // ARABIC TATWEEL
      $0641: Result:= $DD; // ARABIC LETTER FEH
      $0642: Result:= $DE; // ARABIC LETTER QAF
      $0643: Result:= $DF; // ARABIC LETTER KAF
      $00E0: Result:= $E0; // LATIN SMALL LETTER A WITH GRAVE
      $0644: Result:= $E1; // ARABIC LETTER LAM
      $00E2: Result:= $E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $0645: Result:= $E3; // ARABIC LETTER MEEM
      $0646: Result:= $E4; // ARABIC LETTER NOON
      $0647: Result:= $E5; // ARABIC LETTER HEH
      $0648: Result:= $E6; // ARABIC LETTER WAW
      $00E7: Result:= $E7; // LATIN SMALL LETTER C WITH CEDILLA
      $00E8: Result:= $E8; // LATIN SMALL LETTER E WITH GRAVE
      $00E9: Result:= $E9; // LATIN SMALL LETTER E WITH ACUTE
      $00EA: Result:= $EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $00EB: Result:= $EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $0649: Result:= $EC; // ARABIC LETTER ALEF MAKSURA
      $064A: Result:= $ED; // ARABIC LETTER YEH
      $00EE: Result:= $EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $00EF: Result:= $EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $064B: Result:= $F0; // ARABIC FATHATAN
      $064C: Result:= $F1; // ARABIC DAMMATAN
      $064D: Result:= $F2; // ARABIC KASRATAN
      $064E: Result:= $F3; // ARABIC FATHA
      $00F4: Result:= $F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $064F: Result:= $F5; // ARABIC DAMMA
      $0650: Result:= $F6; // ARABIC KASRA
      $00F7: Result:= $F7; // DIVISION SIGN
      $0651: Result:= $F8; // ARABIC SHADDA
      $00F9: Result:= $F9; // LATIN SMALL LETTER U WITH GRAVE
      $0652: Result:= $FA; // ARABIC SUKUN
      $00FB: Result:= $FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $00FC: Result:= $FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $200E: Result:= $FD; // LEFT-TO-RIGHT MARK
      $200F: Result:= $FE; // RIGHT-TO-LEFT MARK
      $06D2: Result:= $FF; // ARABIC LETTER YEH BARREE
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharWin1257(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $20AC: Result:= $80; // EURO SIGN
      $201A: Result:= $82; // SINGLE LOW-9 QUOTATION MARK
      $201E: Result:= $84; // DOUBLE LOW-9 QUOTATION MARK
      $2026: Result:= $85; // HORIZONTAL ELLIPSIS
      $2020: Result:= $86; // DAGGER
      $2021: Result:= $87; // DOUBLE DAGGER
      $2030: Result:= $89; // PER MILLE SIGN
      $2039: Result:= $8B; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $00A8: Result:= $8D; // DIAERESIS
      $02C7: Result:= $8E; // CARON
      $00B8: Result:= $8F; // CEDILLA
      $2018: Result:= $91; // LEFT SINGLE QUOTATION MARK
      $2019: Result:= $92; // RIGHT SINGLE QUOTATION MARK
      $201C: Result:= $93; // LEFT DOUBLE QUOTATION MARK
      $201D: Result:= $94; // RIGHT DOUBLE QUOTATION MARK
      $2022: Result:= $95; // BULLET
      $2013: Result:= $96; // EN DASH
      $2014: Result:= $97; // EM DASH
      $2122: Result:= $99; // TRADE MARK SIGN
      $203A: Result:= $9B; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $00AF: Result:= $9D; // MACRON
      $02DB: Result:= $9E; // OGONEK
      $00A0: Result:= $A0; // NO-BREAK SPACE
      $00A2: Result:= $A2; // CENT SIGN
      $00A3: Result:= $A3; // POUND SIGN
      $00A4: Result:= $A4; // CURRENCY SIGN
      $00A6: Result:= $A6; // BROKEN BAR
      $00A7: Result:= $A7; // SECTION SIGN
      $00D8: Result:= $A8; // LATIN CAPITAL LETTER O WITH STROKE
      $00A9: Result:= $A9; // COPYRIGHT SIGN
      $0156: Result:= $AA; // LATIN CAPITAL LETTER R WITH CEDILLA
      $00AB: Result:= $AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; // NOT SIGN
      $00AD: Result:= $AD; // SOFT HYPHEN
      $00AE: Result:= $AE; // REGISTERED SIGN
      $00C6: Result:= $AF; // LATIN CAPITAL LETTER AE
      $00B0: Result:= $B0; // DEGREE SIGN
      $00B1: Result:= $B1; // PLUS-MINUS SIGN
      $00B2: Result:= $B2; // SUPERSCRIPT TWO
      $00B3: Result:= $B3; // SUPERSCRIPT THREE
      $00B4: Result:= $B4; // ACUTE ACCENT
      $00B5: Result:= $B5; // MICRO SIGN
      $00B6: Result:= $B6; // PILCROW SIGN
      $00B7: Result:= $B7; // MIDDLE DOT
      $00F8: Result:= $B8; // LATIN SMALL LETTER O WITH STROKE
      $00B9: Result:= $B9; // SUPERSCRIPT ONE
      $0157: Result:= $BA; // LATIN SMALL LETTER R WITH CEDILLA
      $00BB: Result:= $BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00BC: Result:= $BC; // VULGAR FRACTION ONE QUARTER
      $00BD: Result:= $BD; // VULGAR FRACTION ONE HALF
      $00BE: Result:= $BE; // VULGAR FRACTION THREE QUARTERS
      $00E6: Result:= $BF; // LATIN SMALL LETTER AE
      $0104: Result:= $C0; // LATIN CAPITAL LETTER A WITH OGONEK
      $012E: Result:= $C1; // LATIN CAPITAL LETTER I WITH OGONEK
      $0100: Result:= $C2; // LATIN CAPITAL LETTER A WITH MACRON
      $0106: Result:= $C3; // LATIN CAPITAL LETTER C WITH ACUTE
      $00C4: Result:= $C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $00C5: Result:= $C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $0118: Result:= $C6; // LATIN CAPITAL LETTER E WITH OGONEK
      $0112: Result:= $C7; // LATIN CAPITAL LETTER E WITH MACRON
      $010C: Result:= $C8; // LATIN CAPITAL LETTER C WITH CARON
      $00C9: Result:= $C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $0179: Result:= $CA; // LATIN CAPITAL LETTER Z WITH ACUTE
      $0116: Result:= $CB; // LATIN CAPITAL LETTER E WITH DOT ABOVE
      $0122: Result:= $CC; // LATIN CAPITAL LETTER G WITH CEDILLA
      $0136: Result:= $CD; // LATIN CAPITAL LETTER K WITH CEDILLA
      $012A: Result:= $CE; // LATIN CAPITAL LETTER I WITH MACRON
      $013B: Result:= $CF; // LATIN CAPITAL LETTER L WITH CEDILLA
      $0160: Result:= $D0; // LATIN CAPITAL LETTER S WITH CARON
      $0143: Result:= $D1; // LATIN CAPITAL LETTER N WITH ACUTE
      $0145: Result:= $D2; // LATIN CAPITAL LETTER N WITH CEDILLA
      $00D3: Result:= $D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $014C: Result:= $D4; // LATIN CAPITAL LETTER O WITH MACRON
      $00D5: Result:= $D5; // LATIN CAPITAL LETTER O WITH TILDE
      $00D6: Result:= $D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $00D7: Result:= $D7; // MULTIPLICATION SIGN
      $0172: Result:= $D8; // LATIN CAPITAL LETTER U WITH OGONEK
      $0141: Result:= $D9; // LATIN CAPITAL LETTER L WITH STROKE
      $015A: Result:= $DA; // LATIN CAPITAL LETTER S WITH ACUTE
      $016A: Result:= $DB; // LATIN CAPITAL LETTER U WITH MACRON
      $00DC: Result:= $DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $017B: Result:= $DD; // LATIN CAPITAL LETTER Z WITH DOT ABOVE
      $017D: Result:= $DE; // LATIN CAPITAL LETTER Z WITH CARON
      $00DF: Result:= $DF; // LATIN SMALL LETTER SHARP S
      $0105: Result:= $E0; // LATIN SMALL LETTER A WITH OGONEK
      $012F: Result:= $E1; // LATIN SMALL LETTER I WITH OGONEK
      $0101: Result:= $E2; // LATIN SMALL LETTER A WITH MACRON
      $0107: Result:= $E3; // LATIN SMALL LETTER C WITH ACUTE
      $00E4: Result:= $E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $00E5: Result:= $E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $0119: Result:= $E6; // LATIN SMALL LETTER E WITH OGONEK
      $0113: Result:= $E7; // LATIN SMALL LETTER E WITH MACRON
      $010D: Result:= $E8; // LATIN SMALL LETTER C WITH CARON
      $00E9: Result:= $E9; // LATIN SMALL LETTER E WITH ACUTE
      $017A: Result:= $EA; // LATIN SMALL LETTER Z WITH ACUTE
      $0117: Result:= $EB; // LATIN SMALL LETTER E WITH DOT ABOVE
      $0123: Result:= $EC; // LATIN SMALL LETTER G WITH CEDILLA
      $0137: Result:= $ED; // LATIN SMALL LETTER K WITH CEDILLA
      $012B: Result:= $EE; // LATIN SMALL LETTER I WITH MACRON
      $013C: Result:= $EF; // LATIN SMALL LETTER L WITH CEDILLA
      $0161: Result:= $F0; // LATIN SMALL LETTER S WITH CARON
      $0144: Result:= $F1; // LATIN SMALL LETTER N WITH ACUTE
      $0146: Result:= $F2; // LATIN SMALL LETTER N WITH CEDILLA
      $00F3: Result:= $F3; // LATIN SMALL LETTER O WITH ACUTE
      $014D: Result:= $F4; // LATIN SMALL LETTER O WITH MACRON
      $00F5: Result:= $F5; // LATIN SMALL LETTER O WITH TILDE
      $00F6: Result:= $F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $00F7: Result:= $F7; // DIVISION SIGN
      $0173: Result:= $F8; // LATIN SMALL LETTER U WITH OGONEK
      $0142: Result:= $F9; // LATIN SMALL LETTER L WITH STROKE
      $015B: Result:= $FA; // LATIN SMALL LETTER S WITH ACUTE
      $016B: Result:= $FB; // LATIN SMALL LETTER U WITH MACRON
      $00FC: Result:= $FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $017C: Result:= $FD; // LATIN SMALL LETTER Z WITH DOT ABOVE
      $017E: Result:= $FE; // LATIN SMALL LETTER Z WITH CARON
      $02D9: Result:= $FF; // DOT ABOVE
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharWin1258(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $20AC: Result:= $80; // EURO SIGN
      $201A: Result:= $82; // SINGLE LOW-9 QUOTATION MARK
      $0192: Result:= $83; // LATIN SMALL LETTER F WITH HOOK
      $201E: Result:= $84; // DOUBLE LOW-9 QUOTATION MARK
      $2026: Result:= $85; // HORIZONTAL ELLIPSIS
      $2020: Result:= $86; // DAGGER
      $2021: Result:= $87; // DOUBLE DAGGER
      $02C6: Result:= $88; // MODIFIER LETTER CIRCUMFLEX ACCENT
      $2030: Result:= $89; // PER MILLE SIGN
      $2039: Result:= $8B; // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
      $0152: Result:= $8C; // LATIN CAPITAL LIGATURE OE
      $2018: Result:= $91; // LEFT SINGLE QUOTATION MARK
      $2019: Result:= $92; // RIGHT SINGLE QUOTATION MARK
      $201C: Result:= $93; // LEFT DOUBLE QUOTATION MARK
      $201D: Result:= $94; // RIGHT DOUBLE QUOTATION MARK
      $2022: Result:= $95; // BULLET
      $2013: Result:= $96; // EN DASH
      $2014: Result:= $97; // EM DASH
      $02DC: Result:= $98; // SMALL TILDE
      $2122: Result:= $99; // TRADE MARK SIGN
      $203A: Result:= $9B; // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
      $0153: Result:= $9C; // LATIN SMALL LIGATURE OE
      $0178: Result:= $9F; // LATIN CAPITAL LETTER Y WITH DIAERESIS
      $00A0: Result:= $A0; // NO-BREAK SPACE
      $00A1: Result:= $A1; // INVERTED EXCLAMATION MARK
      $00A2: Result:= $A2; // CENT SIGN
      $00A3: Result:= $A3; // POUND SIGN
      $00A4: Result:= $A4; // CURRENCY SIGN
      $00A5: Result:= $A5; // YEN SIGN
      $00A6: Result:= $A6; // BROKEN BAR
      $00A7: Result:= $A7; // SECTION SIGN
      $00A8: Result:= $A8; // DIAERESIS
      $00A9: Result:= $A9; // COPYRIGHT SIGN
      $00AA: Result:= $AA; // FEMININE ORDINAL INDICATOR
      $00AB: Result:= $AB; // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; // NOT SIGN
      $00AD: Result:= $AD; // SOFT HYPHEN
      $00AE: Result:= $AE; // REGISTERED SIGN
      $00AF: Result:= $AF; // MACRON
      $00B0: Result:= $B0; // DEGREE SIGN
      $00B1: Result:= $B1; // PLUS-MINUS SIGN
      $00B2: Result:= $B2; // SUPERSCRIPT TWO
      $00B3: Result:= $B3; // SUPERSCRIPT THREE
      $00B4: Result:= $B4; // ACUTE ACCENT
      $00B5: Result:= $B5; // MICRO SIGN
      $00B6: Result:= $B6; // PILCROW SIGN
      $00B7: Result:= $B7; // MIDDLE DOT
      $00B8: Result:= $B8; // CEDILLA
      $00B9: Result:= $B9; // SUPERSCRIPT ONE
      $00BA: Result:= $BA; // MASCULINE ORDINAL INDICATOR
      $00BB: Result:= $BB; // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00BC: Result:= $BC; // VULGAR FRACTION ONE QUARTER
      $00BD: Result:= $BD; // VULGAR FRACTION ONE HALF
      $00BE: Result:= $BE; // VULGAR FRACTION THREE QUARTERS
      $00BF: Result:= $BF; // INVERTED QUESTION MARK
      $00C0: Result:= $C0; // LATIN CAPITAL LETTER A WITH GRAVE
      $00C1: Result:= $C1; // LATIN CAPITAL LETTER A WITH ACUTE
      $00C2: Result:= $C2; // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $0102: Result:= $C3; // LATIN CAPITAL LETTER A WITH BREVE
      $00C4: Result:= $C4; // LATIN CAPITAL LETTER A WITH DIAERESIS
      $00C5: Result:= $C5; // LATIN CAPITAL LETTER A WITH RING ABOVE
      $00C6: Result:= $C6; // LATIN CAPITAL LETTER AE
      $00C7: Result:= $C7; // LATIN CAPITAL LETTER C WITH CEDILLA
      $00C8: Result:= $C8; // LATIN CAPITAL LETTER E WITH GRAVE
      $00C9: Result:= $C9; // LATIN CAPITAL LETTER E WITH ACUTE
      $00CA: Result:= $CA; // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $00CB: Result:= $CB; // LATIN CAPITAL LETTER E WITH DIAERESIS
      $0300: Result:= $CC; // COMBINING GRAVE ACCENT
      $00CD: Result:= $CD; // LATIN CAPITAL LETTER I WITH ACUTE
      $00CE: Result:= $CE; // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $00CF: Result:= $CF; // LATIN CAPITAL LETTER I WITH DIAERESIS
      $0110: Result:= $D0; // LATIN CAPITAL LETTER D WITH STROKE
      $00D1: Result:= $D1; // LATIN CAPITAL LETTER N WITH TILDE
      $0309: Result:= $D2; // COMBINING HOOK ABOVE
      $00D3: Result:= $D3; // LATIN CAPITAL LETTER O WITH ACUTE
      $00D4: Result:= $D4; // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $01A0: Result:= $D5; // LATIN CAPITAL LETTER O WITH HORN
      $00D6: Result:= $D6; // LATIN CAPITAL LETTER O WITH DIAERESIS
      $00D7: Result:= $D7; // MULTIPLICATION SIGN
      $00D8: Result:= $D8; // LATIN CAPITAL LETTER O WITH STROKE
      $00D9: Result:= $D9; // LATIN CAPITAL LETTER U WITH GRAVE
      $00DA: Result:= $DA; // LATIN CAPITAL LETTER U WITH ACUTE
      $00DB: Result:= $DB; // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $00DC: Result:= $DC; // LATIN CAPITAL LETTER U WITH DIAERESIS
      $01AF: Result:= $DD; // LATIN CAPITAL LETTER U WITH HORN
      $0303: Result:= $DE; // COMBINING TILDE
      $00DF: Result:= $DF; // LATIN SMALL LETTER SHARP S
      $00E0: Result:= $E0; // LATIN SMALL LETTER A WITH GRAVE
      $00E1: Result:= $E1; // LATIN SMALL LETTER A WITH ACUTE
      $00E2: Result:= $E2; // LATIN SMALL LETTER A WITH CIRCUMFLEX
      $0103: Result:= $E3; // LATIN SMALL LETTER A WITH BREVE
      $00E4: Result:= $E4; // LATIN SMALL LETTER A WITH DIAERESIS
      $00E5: Result:= $E5; // LATIN SMALL LETTER A WITH RING ABOVE
      $00E6: Result:= $E6; // LATIN SMALL LETTER AE
      $00E7: Result:= $E7; // LATIN SMALL LETTER C WITH CEDILLA
      $00E8: Result:= $E8; // LATIN SMALL LETTER E WITH GRAVE
      $00E9: Result:= $E9; // LATIN SMALL LETTER E WITH ACUTE
      $00EA: Result:= $EA; // LATIN SMALL LETTER E WITH CIRCUMFLEX
      $00EB: Result:= $EB; // LATIN SMALL LETTER E WITH DIAERESIS
      $0301: Result:= $EC; // COMBINING ACUTE ACCENT
      $00ED: Result:= $ED; // LATIN SMALL LETTER I WITH ACUTE
      $00EE: Result:= $EE; // LATIN SMALL LETTER I WITH CIRCUMFLEX
      $00EF: Result:= $EF; // LATIN SMALL LETTER I WITH DIAERESIS
      $0111: Result:= $F0; // LATIN SMALL LETTER D WITH STROKE
      $00F1: Result:= $F1; // LATIN SMALL LETTER N WITH TILDE
      $0323: Result:= $F2; // COMBINING DOT BELOW
      $00F3: Result:= $F3; // LATIN SMALL LETTER O WITH ACUTE
      $00F4: Result:= $F4; // LATIN SMALL LETTER O WITH CIRCUMFLEX
      $01A1: Result:= $F5; // LATIN SMALL LETTER O WITH HORN
      $00F6: Result:= $F6; // LATIN SMALL LETTER O WITH DIAERESIS
      $00F7: Result:= $F7; // DIVISION SIGN
      $00F8: Result:= $F8; // LATIN SMALL LETTER O WITH STROKE
      $00F9: Result:= $F9; // LATIN SMALL LETTER U WITH GRAVE
      $00FA: Result:= $FA; // LATIN SMALL LETTER U WITH ACUTE
      $00FB: Result:= $FB; // LATIN SMALL LETTER U WITH CIRCUMFLEX
      $00FC: Result:= $FC; // LATIN SMALL LETTER U WITH DIAERESIS
      $01B0: Result:= $FD; // LATIN SMALL LETTER U WITH HORN
      $20AB: Result:= $FE; // DONG SIGN
      $00FF: Result:= $FF; // LATIN SMALL LETTER Y WITH DIAERESIS
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharWin874(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $20AC: Result:= $80; // EURO SIGN
      $2026: Result:= $85; // HORIZONTAL ELLIPSIS
      $2018: Result:= $91; // LEFT SINGLE QUOTATION MARK
      $2019: Result:= $92; // RIGHT SINGLE QUOTATION MARK
      $201C: Result:= $93; // LEFT DOUBLE QUOTATION MARK
      $201D: Result:= $94; // RIGHT DOUBLE QUOTATION MARK
      $2022: Result:= $95; // BULLET
      $2013: Result:= $96; // EN DASH
      $2014: Result:= $97; // EM DASH
      $00A0: Result:= $A0; // NO-BREAK SPACE
      $0E01: Result:= $A1; // THAI CHARACTER KO KAI
      $0E02: Result:= $A2; // THAI CHARACTER KHO KHAI
      $0E03: Result:= $A3; // THAI CHARACTER KHO KHUAT
      $0E04: Result:= $A4; // THAI CHARACTER KHO KHWAI
      $0E05: Result:= $A5; // THAI CHARACTER KHO KHON
      $0E06: Result:= $A6; // THAI CHARACTER KHO RAKHANG
      $0E07: Result:= $A7; // THAI CHARACTER NGO NGU
      $0E08: Result:= $A8; // THAI CHARACTER CHO CHAN
      $0E09: Result:= $A9; // THAI CHARACTER CHO CHING
      $0E0A: Result:= $AA; // THAI CHARACTER CHO CHANG
      $0E0B: Result:= $AB; // THAI CHARACTER SO SO
      $0E0C: Result:= $AC; // THAI CHARACTER CHO CHOE
      $0E0D: Result:= $AD; // THAI CHARACTER YO YING
      $0E0E: Result:= $AE; // THAI CHARACTER DO CHADA
      $0E0F: Result:= $AF; // THAI CHARACTER TO PATAK
      $0E10: Result:= $B0; // THAI CHARACTER THO THAN
      $0E11: Result:= $B1; // THAI CHARACTER THO NANGMONTHO
      $0E12: Result:= $B2; // THAI CHARACTER THO PHUTHAO
      $0E13: Result:= $B3; // THAI CHARACTER NO NEN
      $0E14: Result:= $B4; // THAI CHARACTER DO DEK
      $0E15: Result:= $B5; // THAI CHARACTER TO TAO
      $0E16: Result:= $B6; // THAI CHARACTER THO THUNG
      $0E17: Result:= $B7; // THAI CHARACTER THO THAHAN
      $0E18: Result:= $B8; // THAI CHARACTER THO THONG
      $0E19: Result:= $B9; // THAI CHARACTER NO NU
      $0E1A: Result:= $BA; // THAI CHARACTER BO BAIMAI
      $0E1B: Result:= $BB; // THAI CHARACTER PO PLA
      $0E1C: Result:= $BC; // THAI CHARACTER PHO PHUNG
      $0E1D: Result:= $BD; // THAI CHARACTER FO FA
      $0E1E: Result:= $BE; // THAI CHARACTER PHO PHAN
      $0E1F: Result:= $BF; // THAI CHARACTER FO FAN
      $0E20: Result:= $C0; // THAI CHARACTER PHO SAMPHAO
      $0E21: Result:= $C1; // THAI CHARACTER MO MA
      $0E22: Result:= $C2; // THAI CHARACTER YO YAK
      $0E23: Result:= $C3; // THAI CHARACTER RO RUA
      $0E24: Result:= $C4; // THAI CHARACTER RU
      $0E25: Result:= $C5; // THAI CHARACTER LO LING
      $0E26: Result:= $C6; // THAI CHARACTER LU
      $0E27: Result:= $C7; // THAI CHARACTER WO WAEN
      $0E28: Result:= $C8; // THAI CHARACTER SO SALA
      $0E29: Result:= $C9; // THAI CHARACTER SO RUSI
      $0E2A: Result:= $CA; // THAI CHARACTER SO SUA
      $0E2B: Result:= $CB; // THAI CHARACTER HO HIP
      $0E2C: Result:= $CC; // THAI CHARACTER LO CHULA
      $0E2D: Result:= $CD; // THAI CHARACTER O ANG
      $0E2E: Result:= $CE; // THAI CHARACTER HO NOKHUK
      $0E2F: Result:= $CF; // THAI CHARACTER PAIYANNOI
      $0E30: Result:= $D0; // THAI CHARACTER SARA A
      $0E31: Result:= $D1; // THAI CHARACTER MAI HAN-AKAT
      $0E32: Result:= $D2; // THAI CHARACTER SARA AA
      $0E33: Result:= $D3; // THAI CHARACTER SARA AM
      $0E34: Result:= $D4; // THAI CHARACTER SARA I
      $0E35: Result:= $D5; // THAI CHARACTER SARA II
      $0E36: Result:= $D6; // THAI CHARACTER SARA UE
      $0E37: Result:= $D7; // THAI CHARACTER SARA UEE
      $0E38: Result:= $D8; // THAI CHARACTER SARA U
      $0E39: Result:= $D9; // THAI CHARACTER SARA UU
      $0E3A: Result:= $DA; // THAI CHARACTER PHINTHU
      $0E3F: Result:= $DF; // THAI CURRENCY SYMBOL BAHT
      $0E40: Result:= $E0; // THAI CHARACTER SARA E
      $0E41: Result:= $E1; // THAI CHARACTER SARA AE
      $0E42: Result:= $E2; // THAI CHARACTER SARA O
      $0E43: Result:= $E3; // THAI CHARACTER SARA AI MAIMUAN
      $0E44: Result:= $E4; // THAI CHARACTER SARA AI MAIMALAI
      $0E45: Result:= $E5; // THAI CHARACTER LAKKHANGYAO
      $0E46: Result:= $E6; // THAI CHARACTER MAIYAMOK
      $0E47: Result:= $E7; // THAI CHARACTER MAITAIKHU
      $0E48: Result:= $E8; // THAI CHARACTER MAI EK
      $0E49: Result:= $E9; // THAI CHARACTER MAI THO
      $0E4A: Result:= $EA; // THAI CHARACTER MAI TRI
      $0E4B: Result:= $EB; // THAI CHARACTER MAI CHATTAWA
      $0E4C: Result:= $EC; // THAI CHARACTER THANTHAKHAT
      $0E4D: Result:= $ED; // THAI CHARACTER NIKHAHIT
      $0E4E: Result:= $EE; // THAI CHARACTER YAMAKKAN
      $0E4F: Result:= $EF; // THAI CHARACTER FONGMAN
      $0E50: Result:= $F0; // THAI DIGIT ZERO
      $0E51: Result:= $F1; // THAI DIGIT ONE
      $0E52: Result:= $F2; // THAI DIGIT TWO
      $0E53: Result:= $F3; // THAI DIGIT THREE
      $0E54: Result:= $F4; // THAI DIGIT FOUR
      $0E55: Result:= $F5; // THAI DIGIT FIVE
      $0E56: Result:= $F6; // THAI DIGIT SIX
      $0E57: Result:= $F7; // THAI DIGIT SEVEN
      $0E58: Result:= $F8; // THAI DIGIT EIGHT
      $0E59: Result:= $F9; // THAI DIGIT NINE
      $0E5A: Result:= $FA; // THAI CHARACTER ANGKHANKHU
      $0E5B: Result:= $FB; // THAI CHARACTER KHOMUT
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharISO8859_1(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $00A0: Result:= $A0; //  NO-BREAK SPACE
      $00A1: Result:= $A1; //  INVERTED EXCLAMATION MARK
      $00A2: Result:= $A2; //  CENT SIGN
      $00A3: Result:= $A3; //  POUND SIGN
      $00A4: Result:= $A4; //  CURRENCY SIGN
      $00A5: Result:= $A5; //  YEN SIGN
      $00A6: Result:= $A6; //  BROKEN BAR
      $00A7: Result:= $A7; //  SECTION SIGN
      $00A8: Result:= $A8; //  DIAERESIS
      $00A9: Result:= $A9; //  COPYRIGHT SIGN
      $00AA: Result:= $AA; //  FEMININE ORDINAL INDICATOR
      $00AB: Result:= $AB; //  LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; //  NOT SIGN
      $00AD: Result:= $AD; //  SOFT HYPHEN
      $00AE: Result:= $AE; //  REGISTERED SIGN
      $00AF: Result:= $AF; //  MACRON
      $00B0: Result:= $B0; //  DEGREE SIGN
      $00B1: Result:= $B1; //  PLUS-MINUS SIGN
      $00B2: Result:= $B2; //  SUPERSCRIPT TWO
      $00B3: Result:= $B3; //  SUPERSCRIPT THREE
      $00B4: Result:= $B4; //  ACUTE ACCENT
      $00B5: Result:= $B5; //  MICRO SIGN
      $00B6: Result:= $B6; //  PILCROW SIGN
      $00B7: Result:= $B7; //  MIDDLE DOT
      $00B8: Result:= $B8; //  CEDILLA
      $00B9: Result:= $B9; //  SUPERSCRIPT ONE
      $00BA: Result:= $BA; //  MASCULINE ORDINAL INDICATOR
      $00BB: Result:= $BB; //  RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00BC: Result:= $BC; //  VULGAR FRACTION ONE QUARTER
      $00BD: Result:= $BD; //  VULGAR FRACTION ONE HALF
      $00BE: Result:= $BE; //  VULGAR FRACTION THREE QUARTERS
      $00BF: Result:= $BF; //  INVERTED QUESTION MARK
      $00C0: Result:= $C0; //  LATIN CAPITAL LETTER A WITH GRAVE
      $00C1: Result:= $C1; //  LATIN CAPITAL LETTER A WITH ACUTE
      $00C2: Result:= $C2; //  LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $00C3: Result:= $C3; //  LATIN CAPITAL LETTER A WITH TILDE
      $00C4: Result:= $C4; //  LATIN CAPITAL LETTER A WITH DIAERESIS
      $00C5: Result:= $C5; //  LATIN CAPITAL LETTER A WITH RING ABOVE
      $00C6: Result:= $C6; //  LATIN CAPITAL LETTER AE
      $00C7: Result:= $C7; //  LATIN CAPITAL LETTER C WITH CEDILLA
      $00C8: Result:= $C8; //  LATIN CAPITAL LETTER E WITH GRAVE
      $00C9: Result:= $C9; //  LATIN CAPITAL LETTER E WITH ACUTE
      $00CA: Result:= $CA; //  LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $00CB: Result:= $CB; //  LATIN CAPITAL LETTER E WITH DIAERESIS
      $00CC: Result:= $CC; //  LATIN CAPITAL LETTER I WITH GRAVE
      $00CD: Result:= $CD; //  LATIN CAPITAL LETTER I WITH ACUTE
      $00CE: Result:= $CE; //  LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $00CF: Result:= $CF; //  LATIN CAPITAL LETTER I WITH DIAERESIS
      $00D0: Result:= $D0; //  LATIN CAPITAL LETTER ETH
      $00D1: Result:= $D1; //  LATIN CAPITAL LETTER N WITH TILDE
      $00D2: Result:= $D2; //  LATIN CAPITAL LETTER O WITH GRAVE
      $00D3: Result:= $D3; //  LATIN CAPITAL LETTER O WITH ACUTE
      $00D4: Result:= $D4; //  LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $00D5: Result:= $D5; //  LATIN CAPITAL LETTER O WITH TILDE
      $00D6: Result:= $D6; //  LATIN CAPITAL LETTER O WITH DIAERESIS
      $00D7: Result:= $D7; //  MULTIPLICATION SIGN
      $00D8: Result:= $D8; //  LATIN CAPITAL LETTER O WITH STROKE
      $00D9: Result:= $D9; //  LATIN CAPITAL LETTER U WITH GRAVE
      $00DA: Result:= $DA; //  LATIN CAPITAL LETTER U WITH ACUTE
      $00DB: Result:= $DB; //  LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $00DC: Result:= $DC; //  LATIN CAPITAL LETTER U WITH DIAERESIS
      $00DD: Result:= $DD; //  LATIN CAPITAL LETTER Y WITH ACUTE
      $00DE: Result:= $DE; //  LATIN CAPITAL LETTER THORN
      $00DF: Result:= $DF; //  LATIN SMALL LETTER SHARP S
      $00E0: Result:= $E0; //  LATIN SMALL LETTER A WITH GRAVE
      $00E1: Result:= $E1; //  LATIN SMALL LETTER A WITH ACUTE
      $00E2: Result:= $E2; //  LATIN SMALL LETTER A WITH CIRCUMFLEX
      $00E3: Result:= $E3; //  LATIN SMALL LETTER A WITH TILDE
      $00E4: Result:= $E4; //  LATIN SMALL LETTER A WITH DIAERESIS
      $00E5: Result:= $E5; //  LATIN SMALL LETTER A WITH RING ABOVE
      $00E6: Result:= $E6; //  LATIN SMALL LETTER AE
      $00E7: Result:= $E7; //  LATIN SMALL LETTER C WITH CEDILLA
      $00E8: Result:= $E8; //  LATIN SMALL LETTER E WITH GRAVE
      $00E9: Result:= $E9; //  LATIN SMALL LETTER E WITH ACUTE
      $00EA: Result:= $EA; //  LATIN SMALL LETTER E WITH CIRCUMFLEX
      $00EB: Result:= $EB; //  LATIN SMALL LETTER E WITH DIAERESIS
      $00EC: Result:= $EC; //  LATIN SMALL LETTER I WITH GRAVE
      $00ED: Result:= $ED; //  LATIN SMALL LETTER I WITH ACUTE
      $00EE: Result:= $EE; //  LATIN SMALL LETTER I WITH CIRCUMFLEX
      $00EF: Result:= $EF; //  LATIN SMALL LETTER I WITH DIAERESIS
      $00F0: Result:= $F0; //  LATIN SMALL LETTER ETH
      $00F1: Result:= $F1; //  LATIN SMALL LETTER N WITH TILDE
      $00F2: Result:= $F2; //  LATIN SMALL LETTER O WITH GRAVE
      $00F3: Result:= $F3; //  LATIN SMALL LETTER O WITH ACUTE
      $00F4: Result:= $F4; //  LATIN SMALL LETTER O WITH CIRCUMFLEX
      $00F5: Result:= $F5; //  LATIN SMALL LETTER O WITH TILDE
      $00F6: Result:= $F6; //  LATIN SMALL LETTER O WITH DIAERESIS
      $00F7: Result:= $F7; //  DIVISION SIGN
      $00F8: Result:= $F8; //  LATIN SMALL LETTER O WITH STROKE
      $00F9: Result:= $F9; //  LATIN SMALL LETTER U WITH GRAVE
      $00FA: Result:= $FA; //  LATIN SMALL LETTER U WITH ACUTE
      $00FB: Result:= $FB; //  LATIN SMALL LETTER U WITH CIRCUMFLEX
      $00FC: Result:= $FC; //  LATIN SMALL LETTER U WITH DIAERESIS
      $00FD: Result:= $FD; //  LATIN SMALL LETTER Y WITH ACUTE
      $00FE: Result:= $FE; //  LATIN SMALL LETTER THORN
      $00FF: Result:= $FF; //  LATIN SMALL LETTER Y WITH DIAERESIS
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharISO8859_2(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $00A0: Result:= $A0; //  NO-BREAK SPACE
      $0104: Result:= $A1; //  LATIN CAPITAL LETTER A WITH OGONEK
      $02D8: Result:= $A2; //  BREVE
      $0141: Result:= $A3; //  LATIN CAPITAL LETTER L WITH STROKE
      $00A4: Result:= $A4; //  CURRENCY SIGN
      $013D: Result:= $A5; //  LATIN CAPITAL LETTER L WITH CARON
      $015A: Result:= $A6; //  LATIN CAPITAL LETTER S WITH ACUTE
      $00A7: Result:= $A7; //  SECTION SIGN
      $00A8: Result:= $A8; //  DIAERESIS
      $0160: Result:= $A9; //  LATIN CAPITAL LETTER S WITH CARON
      $015E: Result:= $AA; //  LATIN CAPITAL LETTER S WITH CEDILLA
      $0164: Result:= $AB; //  LATIN CAPITAL LETTER T WITH CARON
      $0179: Result:= $AC; //  LATIN CAPITAL LETTER Z WITH ACUTE
      $00AD: Result:= $AD; //  SOFT HYPHEN
      $017D: Result:= $AE; //  LATIN CAPITAL LETTER Z WITH CARON
      $017B: Result:= $AF; //  LATIN CAPITAL LETTER Z WITH DOT ABOVE
      $00B0: Result:= $B0; //  DEGREE SIGN
      $0105: Result:= $B1; //  LATIN SMALL LETTER A WITH OGONEK
      $02DB: Result:= $B2; //  OGONEK
      $0142: Result:= $B3; //  LATIN SMALL LETTER L WITH STROKE
      $00B4: Result:= $B4; //  ACUTE ACCENT
      $013E: Result:= $B5; //  LATIN SMALL LETTER L WITH CARON
      $015B: Result:= $B6; //  LATIN SMALL LETTER S WITH ACUTE
      $02C7: Result:= $B7; //  CARON
      $00B8: Result:= $B8; //  CEDILLA
      $0161: Result:= $B9; //  LATIN SMALL LETTER S WITH CARON
      $015F: Result:= $BA; //  LATIN SMALL LETTER S WITH CEDILLA
      $0165: Result:= $BB; //  LATIN SMALL LETTER T WITH CARON
      $017A: Result:= $BC; //  LATIN SMALL LETTER Z WITH ACUTE
      $02DD: Result:= $BD; //  DOUBLE ACUTE ACCENT
      $017E: Result:= $BE; //  LATIN SMALL LETTER Z WITH CARON
      $017C: Result:= $BF; //  LATIN SMALL LETTER Z WITH DOT ABOVE
      $0154: Result:= $C0; //  LATIN CAPITAL LETTER R WITH ACUTE
      $00C1: Result:= $C1; //  LATIN CAPITAL LETTER A WITH ACUTE
      $00C2: Result:= $C2; //  LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $0102: Result:= $C3; //  LATIN CAPITAL LETTER A WITH BREVE
      $00C4: Result:= $C4; //  LATIN CAPITAL LETTER A WITH DIAERESIS
      $0139: Result:= $C5; //  LATIN CAPITAL LETTER L WITH ACUTE
      $0106: Result:= $C6; //  LATIN CAPITAL LETTER C WITH ACUTE
      $00C7: Result:= $C7; //  LATIN CAPITAL LETTER C WITH CEDILLA
      $010C: Result:= $C8; //  LATIN CAPITAL LETTER C WITH CARON
      $00C9: Result:= $C9; //  LATIN CAPITAL LETTER E WITH ACUTE
      $0118: Result:= $CA; //  LATIN CAPITAL LETTER E WITH OGONEK
      $00CB: Result:= $CB; //  LATIN CAPITAL LETTER E WITH DIAERESIS
      $011A: Result:= $CC; //  LATIN CAPITAL LETTER E WITH CARON
      $00CD: Result:= $CD; //  LATIN CAPITAL LETTER I WITH ACUTE
      $00CE: Result:= $CE; //  LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $010E: Result:= $CF; //  LATIN CAPITAL LETTER D WITH CARON
      $0110: Result:= $D0; //  LATIN CAPITAL LETTER D WITH STROKE
      $0143: Result:= $D1; //  LATIN CAPITAL LETTER N WITH ACUTE
      $0147: Result:= $D2; //  LATIN CAPITAL LETTER N WITH CARON
      $00D3: Result:= $D3; //  LATIN CAPITAL LETTER O WITH ACUTE
      $00D4: Result:= $D4; //  LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $0150: Result:= $D5; //  LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
      $00D6: Result:= $D6; //  LATIN CAPITAL LETTER O WITH DIAERESIS
      $00D7: Result:= $D7; //  MULTIPLICATION SIGN
      $0158: Result:= $D8; //  LATIN CAPITAL LETTER R WITH CARON
      $016E: Result:= $D9; //  LATIN CAPITAL LETTER U WITH RING ABOVE
      $00DA: Result:= $DA; //  LATIN CAPITAL LETTER U WITH ACUTE
      $0170: Result:= $DB; //  LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
      $00DC: Result:= $DC; //  LATIN CAPITAL LETTER U WITH DIAERESIS
      $00DD: Result:= $DD; //  LATIN CAPITAL LETTER Y WITH ACUTE
      $0162: Result:= $DE; //  LATIN CAPITAL LETTER T WITH CEDILLA
      $00DF: Result:= $DF; //  LATIN SMALL LETTER SHARP S
      $0155: Result:= $E0; //  LATIN SMALL LETTER R WITH ACUTE
      $00E1: Result:= $E1; //  LATIN SMALL LETTER A WITH ACUTE
      $00E2: Result:= $E2; //  LATIN SMALL LETTER A WITH CIRCUMFLEX
      $0103: Result:= $E3; //  LATIN SMALL LETTER A WITH BREVE
      $00E4: Result:= $E4; //  LATIN SMALL LETTER A WITH DIAERESIS
      $013A: Result:= $E5; //  LATIN SMALL LETTER L WITH ACUTE
      $0107: Result:= $E6; //  LATIN SMALL LETTER C WITH ACUTE
      $00E7: Result:= $E7; //  LATIN SMALL LETTER C WITH CEDILLA
      $010D: Result:= $E8; //  LATIN SMALL LETTER C WITH CARON
      $00E9: Result:= $E9; //  LATIN SMALL LETTER E WITH ACUTE
      $0119: Result:= $EA; //  LATIN SMALL LETTER E WITH OGONEK
      $00EB: Result:= $EB; //  LATIN SMALL LETTER E WITH DIAERESIS
      $011B: Result:= $EC; //  LATIN SMALL LETTER E WITH CARON
      $00ED: Result:= $ED; //  LATIN SMALL LETTER I WITH ACUTE
      $00EE: Result:= $EE; //  LATIN SMALL LETTER I WITH CIRCUMFLEX
      $010F: Result:= $EF; //  LATIN SMALL LETTER D WITH CARON
      $0111: Result:= $F0; //  LATIN SMALL LETTER D WITH STROKE
      $0144: Result:= $F1; //  LATIN SMALL LETTER N WITH ACUTE
      $0148: Result:= $F2; //  LATIN SMALL LETTER N WITH CARON
      $00F3: Result:= $F3; //  LATIN SMALL LETTER O WITH ACUTE
      $00F4: Result:= $F4; //  LATIN SMALL LETTER O WITH CIRCUMFLEX
      $0151: Result:= $F5; //  LATIN SMALL LETTER O WITH DOUBLE ACUTE
      $00F6: Result:= $F6; //  LATIN SMALL LETTER O WITH DIAERESIS
      $00F7: Result:= $F7; //  DIVISION SIGN
      $0159: Result:= $F8; //  LATIN SMALL LETTER R WITH CARON
      $016F: Result:= $F9; //  LATIN SMALL LETTER U WITH RING ABOVE
      $00FA: Result:= $FA; //  LATIN SMALL LETTER U WITH ACUTE
      $0171: Result:= $FB; //  LATIN SMALL LETTER U WITH DOUBLE ACUTE
      $00FC: Result:= $FC; //  LATIN SMALL LETTER U WITH DIAERESIS
      $00FD: Result:= $FD; //  LATIN SMALL LETTER Y WITH ACUTE
      $0163: Result:= $FE; //  LATIN SMALL LETTER T WITH CEDILLA
      $02D9: Result:= $FF; //  DOT ABOVE
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharISO8859_3(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $00A0: Result:= $A0; //  NO-BREAK SPACE
      $0126: Result:= $A1; //  LATIN CAPITAL LETTER H WITH STROKE
      $02D8: Result:= $A2; //  BREVE
      $00A3: Result:= $A3; //  POUND SIGN
      $00A4: Result:= $A4; //  CURRENCY SIGN
      $0124: Result:= $A6; //  LATIN CAPITAL LETTER H WITH CIRCUMFLEX
      $00A7: Result:= $A7; //  SECTION SIGN
      $00A8: Result:= $A8; //  DIAERESIS
      $0130: Result:= $A9; //  LATIN CAPITAL LETTER I WITH DOT ABOVE
      $015E: Result:= $AA; //  LATIN CAPITAL LETTER S WITH CEDILLA
      $011E: Result:= $AB; //  LATIN CAPITAL LETTER G WITH BREVE
      $0134: Result:= $AC; //  LATIN CAPITAL LETTER J WITH CIRCUMFLEX
      $00AD: Result:= $AD; //  SOFT HYPHEN
      $017B: Result:= $AF; //  LATIN CAPITAL LETTER Z WITH DOT ABOVE
      $00B0: Result:= $B0; //  DEGREE SIGN
      $0127: Result:= $B1; //  LATIN SMALL LETTER H WITH STROKE
      $00B2: Result:= $B2; //  SUPERSCRIPT TWO
      $00B3: Result:= $B3; //  SUPERSCRIPT THREE
      $00B4: Result:= $B4; //  ACUTE ACCENT
      $00B5: Result:= $B5; //  MICRO SIGN
      $0125: Result:= $B6; //  LATIN SMALL LETTER H WITH CIRCUMFLEX
      $00B7: Result:= $B7; //  MIDDLE DOT
      $00B8: Result:= $B8; //  CEDILLA
      $0131: Result:= $B9; //  LATIN SMALL LETTER DOTLESS I
      $015F: Result:= $BA; //  LATIN SMALL LETTER S WITH CEDILLA
      $011F: Result:= $BB; //  LATIN SMALL LETTER G WITH BREVE
      $0135: Result:= $BC; //  LATIN SMALL LETTER J WITH CIRCUMFLEX
      $00BD: Result:= $BD; //  VULGAR FRACTION ONE HALF
      $017C: Result:= $BF; //  LATIN SMALL LETTER Z WITH DOT ABOVE
      $00C0: Result:= $C0; //  LATIN CAPITAL LETTER A WITH GRAVE
      $00C1: Result:= $C1; //  LATIN CAPITAL LETTER A WITH ACUTE
      $00C2: Result:= $C2; //  LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $00C4: Result:= $C4; //  LATIN CAPITAL LETTER A WITH DIAERESIS
      $010A: Result:= $C5; //  LATIN CAPITAL LETTER C WITH DOT ABOVE
      $0108: Result:= $C6; //  LATIN CAPITAL LETTER C WITH CIRCUMFLEX
      $00C7: Result:= $C7; //  LATIN CAPITAL LETTER C WITH CEDILLA
      $00C8: Result:= $C8; //  LATIN CAPITAL LETTER E WITH GRAVE
      $00C9: Result:= $C9; //  LATIN CAPITAL LETTER E WITH ACUTE
      $00CA: Result:= $CA; //  LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $00CB: Result:= $CB; //  LATIN CAPITAL LETTER E WITH DIAERESIS
      $00CC: Result:= $CC; //  LATIN CAPITAL LETTER I WITH GRAVE
      $00CD: Result:= $CD; //  LATIN CAPITAL LETTER I WITH ACUTE
      $00CE: Result:= $CE; //  LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $00CF: Result:= $CF; //  LATIN CAPITAL LETTER I WITH DIAERESIS
      $00D1: Result:= $D1; //  LATIN CAPITAL LETTER N WITH TILDE
      $00D2: Result:= $D2; //  LATIN CAPITAL LETTER O WITH GRAVE
      $00D3: Result:= $D3; //  LATIN CAPITAL LETTER O WITH ACUTE
      $00D4: Result:= $D4; //  LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $0120: Result:= $D5; //  LATIN CAPITAL LETTER G WITH DOT ABOVE
      $00D6: Result:= $D6; //  LATIN CAPITAL LETTER O WITH DIAERESIS
      $00D7: Result:= $D7; //  MULTIPLICATION SIGN
      $011C: Result:= $D8; //  LATIN CAPITAL LETTER G WITH CIRCUMFLEX
      $00D9: Result:= $D9; //  LATIN CAPITAL LETTER U WITH GRAVE
      $00DA: Result:= $DA; //  LATIN CAPITAL LETTER U WITH ACUTE
      $00DB: Result:= $DB; //  LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $00DC: Result:= $DC; //  LATIN CAPITAL LETTER U WITH DIAERESIS
      $016C: Result:= $DD; //  LATIN CAPITAL LETTER U WITH BREVE
      $015C: Result:= $DE; //  LATIN CAPITAL LETTER S WITH CIRCUMFLEX
      $00DF: Result:= $DF; //  LATIN SMALL LETTER SHARP S
      $00E0: Result:= $E0; //  LATIN SMALL LETTER A WITH GRAVE
      $00E1: Result:= $E1; //  LATIN SMALL LETTER A WITH ACUTE
      $00E2: Result:= $E2; //  LATIN SMALL LETTER A WITH CIRCUMFLEX
      $00E4: Result:= $E4; //  LATIN SMALL LETTER A WITH DIAERESIS
      $010B: Result:= $E5; //  LATIN SMALL LETTER C WITH DOT ABOVE
      $0109: Result:= $E6; //  LATIN SMALL LETTER C WITH CIRCUMFLEX
      $00E7: Result:= $E7; //  LATIN SMALL LETTER C WITH CEDILLA
      $00E8: Result:= $E8; //  LATIN SMALL LETTER E WITH GRAVE
      $00E9: Result:= $E9; //  LATIN SMALL LETTER E WITH ACUTE
      $00EA: Result:= $EA; //  LATIN SMALL LETTER E WITH CIRCUMFLEX
      $00EB: Result:= $EB; //  LATIN SMALL LETTER E WITH DIAERESIS
      $00EC: Result:= $EC; //  LATIN SMALL LETTER I WITH GRAVE
      $00ED: Result:= $ED; //  LATIN SMALL LETTER I WITH ACUTE
      $00EE: Result:= $EE; //  LATIN SMALL LETTER I WITH CIRCUMFLEX
      $00EF: Result:= $EF; //  LATIN SMALL LETTER I WITH DIAERESIS
      $00F1: Result:= $F1; //  LATIN SMALL LETTER N WITH TILDE
      $00F2: Result:= $F2; //  LATIN SMALL LETTER O WITH GRAVE
      $00F3: Result:= $F3; //  LATIN SMALL LETTER O WITH ACUTE
      $00F4: Result:= $F4; //  LATIN SMALL LETTER O WITH CIRCUMFLEX
      $0121: Result:= $F5; //  LATIN SMALL LETTER G WITH DOT ABOVE
      $00F6: Result:= $F6; //  LATIN SMALL LETTER O WITH DIAERESIS
      $00F7: Result:= $F7; //  DIVISION SIGN
      $011D: Result:= $F8; //  LATIN SMALL LETTER G WITH CIRCUMFLEX
      $00F9: Result:= $F9; //  LATIN SMALL LETTER U WITH GRAVE
      $00FA: Result:= $FA; //  LATIN SMALL LETTER U WITH ACUTE
      $00FB: Result:= $FB; //  LATIN SMALL LETTER U WITH CIRCUMFLEX
      $00FC: Result:= $FC; //  LATIN SMALL LETTER U WITH DIAERESIS
      $016D: Result:= $FD; //  LATIN SMALL LETTER U WITH BREVE
      $015D: Result:= $FE; //  LATIN SMALL LETTER S WITH CIRCUMFLEX
      $02D9: Result:= $FF; //  DOT ABOVE
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharISO8859_4(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $00A0: Result:= $A0; //  NO-BREAK SPACE
      $0104: Result:= $A1; //  LATIN CAPITAL LETTER A WITH OGONEK
      $0138: Result:= $A2; //  LATIN SMALL LETTER KRA
      $0156: Result:= $A3; //  LATIN CAPITAL LETTER R WITH CEDILLA
      $00A4: Result:= $A4; //  CURRENCY SIGN
      $0128: Result:= $A5; //  LATIN CAPITAL LETTER I WITH TILDE
      $013B: Result:= $A6; //  LATIN CAPITAL LETTER L WITH CEDILLA
      $00A7: Result:= $A7; //  SECTION SIGN
      $00A8: Result:= $A8; //  DIAERESIS
      $0160: Result:= $A9; //  LATIN CAPITAL LETTER S WITH CARON
      $0112: Result:= $AA; //  LATIN CAPITAL LETTER E WITH MACRON
      $0122: Result:= $AB; //  LATIN CAPITAL LETTER G WITH CEDILLA
      $0166: Result:= $AC; //  LATIN CAPITAL LETTER T WITH STROKE
      $00AD: Result:= $AD; //  SOFT HYPHEN
      $017D: Result:= $AE; //  LATIN CAPITAL LETTER Z WITH CARON
      $00AF: Result:= $AF; //  MACRON
      $00B0: Result:= $B0; //  DEGREE SIGN
      $0105: Result:= $B1; //  LATIN SMALL LETTER A WITH OGONEK
      $02DB: Result:= $B2; //  OGONEK
      $0157: Result:= $B3; //  LATIN SMALL LETTER R WITH CEDILLA
      $00B4: Result:= $B4; //  ACUTE ACCENT
      $0129: Result:= $B5; //  LATIN SMALL LETTER I WITH TILDE
      $013C: Result:= $B6; //  LATIN SMALL LETTER L WITH CEDILLA
      $02C7: Result:= $B7; //  CARON
      $00B8: Result:= $B8; //  CEDILLA
      $0161: Result:= $B9; //  LATIN SMALL LETTER S WITH CARON
      $0113: Result:= $BA; //  LATIN SMALL LETTER E WITH MACRON
      $0123: Result:= $BB; //  LATIN SMALL LETTER G WITH CEDILLA
      $0167: Result:= $BC; //  LATIN SMALL LETTER T WITH STROKE
      $014A: Result:= $BD; //  LATIN CAPITAL LETTER ENG
      $017E: Result:= $BE; //  LATIN SMALL LETTER Z WITH CARON
      $014B: Result:= $BF; //  LATIN SMALL LETTER ENG
      $0100: Result:= $C0; //  LATIN CAPITAL LETTER A WITH MACRON
      $00C1: Result:= $C1; //  LATIN CAPITAL LETTER A WITH ACUTE
      $00C2: Result:= $C2; //  LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $00C3: Result:= $C3; //  LATIN CAPITAL LETTER A WITH TILDE
      $00C4: Result:= $C4; //  LATIN CAPITAL LETTER A WITH DIAERESIS
      $00C5: Result:= $C5; //  LATIN CAPITAL LETTER A WITH RING ABOVE
      $00C6: Result:= $C6; //  LATIN CAPITAL LETTER AE
      $012E: Result:= $C7; //  LATIN CAPITAL LETTER I WITH OGONEK
      $010C: Result:= $C8; //  LATIN CAPITAL LETTER C WITH CARON
      $00C9: Result:= $C9; //  LATIN CAPITAL LETTER E WITH ACUTE
      $0118: Result:= $CA; //  LATIN CAPITAL LETTER E WITH OGONEK
      $00CB: Result:= $CB; //  LATIN CAPITAL LETTER E WITH DIAERESIS
      $0116: Result:= $CC; //  LATIN CAPITAL LETTER E WITH DOT ABOVE
      $00CD: Result:= $CD; //  LATIN CAPITAL LETTER I WITH ACUTE
      $00CE: Result:= $CE; //  LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $012A: Result:= $CF; //  LATIN CAPITAL LETTER I WITH MACRON
      $0110: Result:= $D0; //  LATIN CAPITAL LETTER D WITH STROKE
      $0145: Result:= $D1; //  LATIN CAPITAL LETTER N WITH CEDILLA
      $014C: Result:= $D2; //  LATIN CAPITAL LETTER O WITH MACRON
      $0136: Result:= $D3; //  LATIN CAPITAL LETTER K WITH CEDILLA
      $00D4: Result:= $D4; //  LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $00D5: Result:= $D5; //  LATIN CAPITAL LETTER O WITH TILDE
      $00D6: Result:= $D6; //  LATIN CAPITAL LETTER O WITH DIAERESIS
      $00D7: Result:= $D7; //  MULTIPLICATION SIGN
      $00D8: Result:= $D8; //  LATIN CAPITAL LETTER O WITH STROKE
      $0172: Result:= $D9; //  LATIN CAPITAL LETTER U WITH OGONEK
      $00DA: Result:= $DA; //  LATIN CAPITAL LETTER U WITH ACUTE
      $00DB: Result:= $DB; //  LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $00DC: Result:= $DC; //  LATIN CAPITAL LETTER U WITH DIAERESIS
      $0168: Result:= $DD; //  LATIN CAPITAL LETTER U WITH TILDE
      $016A: Result:= $DE; //  LATIN CAPITAL LETTER U WITH MACRON
      $00DF: Result:= $DF; //  LATIN SMALL LETTER SHARP S
      $0101: Result:= $E0; //  LATIN SMALL LETTER A WITH MACRON
      $00E1: Result:= $E1; //  LATIN SMALL LETTER A WITH ACUTE
      $00E2: Result:= $E2; //  LATIN SMALL LETTER A WITH CIRCUMFLEX
      $00E3: Result:= $E3; //  LATIN SMALL LETTER A WITH TILDE
      $00E4: Result:= $E4; //  LATIN SMALL LETTER A WITH DIAERESIS
      $00E5: Result:= $E5; //  LATIN SMALL LETTER A WITH RING ABOVE
      $00E6: Result:= $E6; //  LATIN SMALL LETTER AE
      $012F: Result:= $E7; //  LATIN SMALL LETTER I WITH OGONEK
      $010D: Result:= $E8; //  LATIN SMALL LETTER C WITH CARON
      $00E9: Result:= $E9; //  LATIN SMALL LETTER E WITH ACUTE
      $0119: Result:= $EA; //  LATIN SMALL LETTER E WITH OGONEK
      $00EB: Result:= $EB; //  LATIN SMALL LETTER E WITH DIAERESIS
      $0117: Result:= $EC; //  LATIN SMALL LETTER E WITH DOT ABOVE
      $00ED: Result:= $ED; //  LATIN SMALL LETTER I WITH ACUTE
      $00EE: Result:= $EE; //  LATIN SMALL LETTER I WITH CIRCUMFLEX
      $012B: Result:= $EF; //  LATIN SMALL LETTER I WITH MACRON
      $0111: Result:= $F0; //  LATIN SMALL LETTER D WITH STROKE
      $0146: Result:= $F1; //  LATIN SMALL LETTER N WITH CEDILLA
      $014D: Result:= $F2; //  LATIN SMALL LETTER O WITH MACRON
      $0137: Result:= $F3; //  LATIN SMALL LETTER K WITH CEDILLA
      $00F4: Result:= $F4; //  LATIN SMALL LETTER O WITH CIRCUMFLEX
      $00F5: Result:= $F5; //  LATIN SMALL LETTER O WITH TILDE
      $00F6: Result:= $F6; //  LATIN SMALL LETTER O WITH DIAERESIS
      $00F7: Result:= $F7; //  DIVISION SIGN
      $00F8: Result:= $F8; //  LATIN SMALL LETTER O WITH STROKE
      $0173: Result:= $F9; //  LATIN SMALL LETTER U WITH OGONEK
      $00FA: Result:= $FA; //  LATIN SMALL LETTER U WITH ACUTE
      $00FB: Result:= $FB; //  LATIN SMALL LETTER U WITH CIRCUMFLEX
      $00FC: Result:= $FC; //  LATIN SMALL LETTER U WITH DIAERESIS
      $0169: Result:= $FD; //  LATIN SMALL LETTER U WITH TILDE
      $016B: Result:= $FE; //  LATIN SMALL LETTER U WITH MACRON
      $02D9: Result:= $FF; //  DOT ABOVE
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharISO8859_5(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $00A0: Result:= $A0; //  NO-BREAK SPACE
      $0401: Result:= $A1; //  CYRILLIC CAPITAL LETTER IO
      $0402: Result:= $A2; //  CYRILLIC CAPITAL LETTER DJE
      $0403: Result:= $A3; //  CYRILLIC CAPITAL LETTER GJE
      $0404: Result:= $A4; //  CYRILLIC CAPITAL LETTER UKRAINIAN IE
      $0405: Result:= $A5; //  CYRILLIC CAPITAL LETTER DZE
      $0406: Result:= $A6; //  CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
      $0407: Result:= $A7; //  CYRILLIC CAPITAL LETTER YI
      $0408: Result:= $A8; //  CYRILLIC CAPITAL LETTER JE
      $0409: Result:= $A9; //  CYRILLIC CAPITAL LETTER LJE
      $040A: Result:= $AA; //  CYRILLIC CAPITAL LETTER NJE
      $040B: Result:= $AB; //  CYRILLIC CAPITAL LETTER TSHE
      $040C: Result:= $AC; //  CYRILLIC CAPITAL LETTER KJE
      $00AD: Result:= $AD; //  SOFT HYPHEN
      $040E: Result:= $AE; //  CYRILLIC CAPITAL LETTER SHORT U
      $040F: Result:= $AF; //  CYRILLIC CAPITAL LETTER DZHE
      $0410: Result:= $B0; //  CYRILLIC CAPITAL LETTER A
      $0411: Result:= $B1; //  CYRILLIC CAPITAL LETTER BE
      $0412: Result:= $B2; //  CYRILLIC CAPITAL LETTER VE
      $0413: Result:= $B3; //  CYRILLIC CAPITAL LETTER GHE
      $0414: Result:= $B4; //  CYRILLIC CAPITAL LETTER DE
      $0415: Result:= $B5; //  CYRILLIC CAPITAL LETTER IE
      $0416: Result:= $B6; //  CYRILLIC CAPITAL LETTER ZHE
      $0417: Result:= $B7; //  CYRILLIC CAPITAL LETTER ZE
      $0418: Result:= $B8; //  CYRILLIC CAPITAL LETTER I
      $0419: Result:= $B9; //  CYRILLIC CAPITAL LETTER SHORT I
      $041A: Result:= $BA; //  CYRILLIC CAPITAL LETTER KA
      $041B: Result:= $BB; //  CYRILLIC CAPITAL LETTER EL
      $041C: Result:= $BC; //  CYRILLIC CAPITAL LETTER EM
      $041D: Result:= $BD; //  CYRILLIC CAPITAL LETTER EN
      $041E: Result:= $BE; //  CYRILLIC CAPITAL LETTER O
      $041F: Result:= $BF; //  CYRILLIC CAPITAL LETTER PE
      $0420: Result:= $C0; //  CYRILLIC CAPITAL LETTER ER
      $0421: Result:= $C1; //  CYRILLIC CAPITAL LETTER ES
      $0422: Result:= $C2; //  CYRILLIC CAPITAL LETTER TE
      $0423: Result:= $C3; //  CYRILLIC CAPITAL LETTER U
      $0424: Result:= $C4; //  CYRILLIC CAPITAL LETTER EF
      $0425: Result:= $C5; //  CYRILLIC CAPITAL LETTER HA
      $0426: Result:= $C6; //  CYRILLIC CAPITAL LETTER TSE
      $0427: Result:= $C7; //  CYRILLIC CAPITAL LETTER CHE
      $0428: Result:= $C8; //  CYRILLIC CAPITAL LETTER SHA
      $0429: Result:= $C9; //  CYRILLIC CAPITAL LETTER SHCHA
      $042A: Result:= $CA; //  CYRILLIC CAPITAL LETTER HARD SIGN
      $042B: Result:= $CB; //  CYRILLIC CAPITAL LETTER YERU
      $042C: Result:= $CC; //  CYRILLIC CAPITAL LETTER SOFT SIGN
      $042D: Result:= $CD; //  CYRILLIC CAPITAL LETTER E
      $042E: Result:= $CE; //  CYRILLIC CAPITAL LETTER YU
      $042F: Result:= $CF; //  CYRILLIC CAPITAL LETTER YA
      $0430: Result:= $D0; //  CYRILLIC SMALL LETTER A
      $0431: Result:= $D1; //  CYRILLIC SMALL LETTER BE
      $0432: Result:= $D2; //  CYRILLIC SMALL LETTER VE
      $0433: Result:= $D3; //  CYRILLIC SMALL LETTER GHE
      $0434: Result:= $D4; //  CYRILLIC SMALL LETTER DE
      $0435: Result:= $D5; //  CYRILLIC SMALL LETTER IE
      $0436: Result:= $D6; //  CYRILLIC SMALL LETTER ZHE
      $0437: Result:= $D7; //  CYRILLIC SMALL LETTER ZE
      $0438: Result:= $D8; //  CYRILLIC SMALL LETTER I
      $0439: Result:= $D9; //  CYRILLIC SMALL LETTER SHORT I
      $043A: Result:= $DA; //  CYRILLIC SMALL LETTER KA
      $043B: Result:= $DB; //  CYRILLIC SMALL LETTER EL
      $043C: Result:= $DC; //  CYRILLIC SMALL LETTER EM
      $043D: Result:= $DD; //  CYRILLIC SMALL LETTER EN
      $043E: Result:= $DE; //  CYRILLIC SMALL LETTER O
      $043F: Result:= $DF; //  CYRILLIC SMALL LETTER PE
      $0440: Result:= $E0; //  CYRILLIC SMALL LETTER ER
      $0441: Result:= $E1; //  CYRILLIC SMALL LETTER ES
      $0442: Result:= $E2; //  CYRILLIC SMALL LETTER TE
      $0443: Result:= $E3; //  CYRILLIC SMALL LETTER U
      $0444: Result:= $E4; //  CYRILLIC SMALL LETTER EF
      $0445: Result:= $E5; //  CYRILLIC SMALL LETTER HA
      $0446: Result:= $E6; //  CYRILLIC SMALL LETTER TSE
      $0447: Result:= $E7; //  CYRILLIC SMALL LETTER CHE
      $0448: Result:= $E8; //  CYRILLIC SMALL LETTER SHA
      $0449: Result:= $E9; //  CYRILLIC SMALL LETTER SHCHA
      $044A: Result:= $EA; //  CYRILLIC SMALL LETTER HARD SIGN
      $044B: Result:= $EB; //  CYRILLIC SMALL LETTER YERU
      $044C: Result:= $EC; //  CYRILLIC SMALL LETTER SOFT SIGN
      $044D: Result:= $ED; //  CYRILLIC SMALL LETTER E
      $044E: Result:= $EE; //  CYRILLIC SMALL LETTER YU
      $044F: Result:= $EF; //  CYRILLIC SMALL LETTER YA
      $2116: Result:= $F0; //  NUMERO SIGN
      $0451: Result:= $F1; //  CYRILLIC SMALL LETTER IO
      $0452: Result:= $F2; //  CYRILLIC SMALL LETTER DJE
      $0453: Result:= $F3; //  CYRILLIC SMALL LETTER GJE
      $0454: Result:= $F4; //  CYRILLIC SMALL LETTER UKRAINIAN IE
      $0455: Result:= $F5; //  CYRILLIC SMALL LETTER DZE
      $0456: Result:= $F6; //  CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
      $0457: Result:= $F7; //  CYRILLIC SMALL LETTER YI
      $0458: Result:= $F8; //  CYRILLIC SMALL LETTER JE
      $0459: Result:= $F9; //  CYRILLIC SMALL LETTER LJE
      $045A: Result:= $FA; //  CYRILLIC SMALL LETTER NJE
      $045B: Result:= $FB; //  CYRILLIC SMALL LETTER TSHE
      $045C: Result:= $FC; //  CYRILLIC SMALL LETTER KJE
      $00A7: Result:= $FD; //  SECTION SIGN
      $045E: Result:= $FE; //  CYRILLIC SMALL LETTER SHORT U
      $045F: Result:= $FF; //  CYRILLIC SMALL LETTER DZHE
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharISO8859_6(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $00A0: Result:= $A0; //  NO-BREAK SPACE
      $00A4: Result:= $A4; //  CURRENCY SIGN
      $060C: Result:= $AC; //  ARABIC COMMA
      $00AD: Result:= $AD; //  SOFT HYPHEN
      $061B: Result:= $BB; //  ARABIC SEMICOLON
      $061F: Result:= $BF; //  ARABIC QUESTION MARK
      $0621: Result:= $C1; //  ARABIC LETTER HAMZA
      $0622: Result:= $C2; //  ARABIC LETTER ALEF WITH MADDA ABOVE
      $0623: Result:= $C3; //  ARABIC LETTER ALEF WITH HAMZA ABOVE
      $0624: Result:= $C4; //  ARABIC LETTER WAW WITH HAMZA ABOVE
      $0625: Result:= $C5; //  ARABIC LETTER ALEF WITH HAMZA BELOW
      $0626: Result:= $C6; //  ARABIC LETTER YEH WITH HAMZA ABOVE
      $0627: Result:= $C7; //  ARABIC LETTER ALEF
      $0628: Result:= $C8; //  ARABIC LETTER BEH
      $0629: Result:= $C9; //  ARABIC LETTER TEH MARBUTA
      $062A: Result:= $CA; //  ARABIC LETTER TEH
      $062B: Result:= $CB; //  ARABIC LETTER THEH
      $062C: Result:= $CC; //  ARABIC LETTER JEEM
      $062D: Result:= $CD; //  ARABIC LETTER HAH
      $062E: Result:= $CE; //  ARABIC LETTER KHAH
      $062F: Result:= $CF; //  ARABIC LETTER DAL
      $0630: Result:= $D0; //  ARABIC LETTER THAL
      $0631: Result:= $D1; //  ARABIC LETTER REH
      $0632: Result:= $D2; //  ARABIC LETTER ZAIN
      $0633: Result:= $D3; //  ARABIC LETTER SEEN
      $0634: Result:= $D4; //  ARABIC LETTER SHEEN
      $0635: Result:= $D5; //  ARABIC LETTER SAD
      $0636: Result:= $D6; //  ARABIC LETTER DAD
      $0637: Result:= $D7; //  ARABIC LETTER TAH
      $0638: Result:= $D8; //  ARABIC LETTER ZAH
      $0639: Result:= $D9; //  ARABIC LETTER AIN
      $063A: Result:= $DA; //  ARABIC LETTER GHAIN
      $0640: Result:= $E0; //  ARABIC TATWEEL
      $0641: Result:= $E1; //  ARABIC LETTER FEH
      $0642: Result:= $E2; //  ARABIC LETTER QAF
      $0643: Result:= $E3; //  ARABIC LETTER KAF
      $0644: Result:= $E4; //  ARABIC LETTER LAM
      $0645: Result:= $E5; //  ARABIC LETTER MEEM
      $0646: Result:= $E6; //  ARABIC LETTER NOON
      $0647: Result:= $E7; //  ARABIC LETTER HEH
      $0648: Result:= $E8; //  ARABIC LETTER WAW
      $0649: Result:= $E9; //  ARABIC LETTER ALEF MAKSURA
      $064A: Result:= $EA; //  ARABIC LETTER YEH
      $064B: Result:= $EB; //  ARABIC FATHATAN
      $064C: Result:= $EC; //  ARABIC DAMMATAN
      $064D: Result:= $ED; //  ARABIC KASRATAN
      $064E: Result:= $EE; //  ARABIC FATHA
      $064F: Result:= $EF; //  ARABIC DAMMA
      $0650: Result:= $F0; //  ARABIC KASRA
      $0651: Result:= $F1; //  ARABIC SHADDA
      $0652: Result:= $F2; //  ARABIC SUKUN
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharISO8859_7(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $00A0: Result:= $A0; //  NO-BREAK SPACE
      $02BD: Result:= $A1; //  MODIFIER LETTER REVERSED COMMA
      $02BC: Result:= $A2; //  MODIFIER LETTER APOSTROPHE
      $00A3: Result:= $A3; //  POUND SIGN
      $00A6: Result:= $A6; //  BROKEN BAR
      $00A7: Result:= $A7; //  SECTION SIGN
      $00A8: Result:= $A8; //  DIAERESIS
      $00A9: Result:= $A9; //  COPYRIGHT SIGN
      $00AB: Result:= $AB; //  LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; //  NOT SIGN
      $00AD: Result:= $AD; //  SOFT HYPHEN
      $2015: Result:= $AF; //  HORIZONTAL BAR
      $00B0: Result:= $B0; //  DEGREE SIGN
      $00B1: Result:= $B1; //  PLUS-MINUS SIGN
      $00B2: Result:= $B2; //  SUPERSCRIPT TWO
      $00B3: Result:= $B3; //  SUPERSCRIPT THREE
      $0384: Result:= $B4; //  GREEK TONOS
      $0385: Result:= $B5; //  GREEK DIALYTIKA TONOS
      $0386: Result:= $B6; //  GREEK CAPITAL LETTER ALPHA WITH TONOS
      $00B7: Result:= $B7; //  MIDDLE DOT
      $0388: Result:= $B8; //  GREEK CAPITAL LETTER EPSILON WITH TONOS
      $0389: Result:= $B9; //  GREEK CAPITAL LETTER ETA WITH TONOS
      $038A: Result:= $BA; //  GREEK CAPITAL LETTER IOTA WITH TONOS
      $00BB: Result:= $BB; //  RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $038C: Result:= $BC; //  GREEK CAPITAL LETTER OMICRON WITH TONOS
      $00BD: Result:= $BD; //  VULGAR FRACTION ONE HALF
      $038E: Result:= $BE; //  GREEK CAPITAL LETTER UPSILON WITH TONOS
      $038F: Result:= $BF; //  GREEK CAPITAL LETTER OMEGA WITH TONOS
      $0390: Result:= $C0; //  GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
      $0391: Result:= $C1; //  GREEK CAPITAL LETTER ALPHA
      $0392: Result:= $C2; //  GREEK CAPITAL LETTER BETA
      $0393: Result:= $C3; //  GREEK CAPITAL LETTER GAMMA
      $0394: Result:= $C4; //  GREEK CAPITAL LETTER DELTA
      $0395: Result:= $C5; //  GREEK CAPITAL LETTER EPSILON
      $0396: Result:= $C6; //  GREEK CAPITAL LETTER ZETA
      $0397: Result:= $C7; //  GREEK CAPITAL LETTER ETA
      $0398: Result:= $C8; //  GREEK CAPITAL LETTER THETA
      $0399: Result:= $C9; //  GREEK CAPITAL LETTER IOTA
      $039A: Result:= $CA; //  GREEK CAPITAL LETTER KAPPA
      $039B: Result:= $CB; //  GREEK CAPITAL LETTER LAMDA
      $039C: Result:= $CC; //  GREEK CAPITAL LETTER MU
      $039D: Result:= $CD; //  GREEK CAPITAL LETTER NU
      $039E: Result:= $CE; //  GREEK CAPITAL LETTER XI
      $039F: Result:= $CF; //  GREEK CAPITAL LETTER OMICRON
      $03A0: Result:= $D0; //  GREEK CAPITAL LETTER PI
      $03A1: Result:= $D1; //  GREEK CAPITAL LETTER RHO
      $03A3: Result:= $D3; //  GREEK CAPITAL LETTER SIGMA
      $03A4: Result:= $D4; //  GREEK CAPITAL LETTER TAU
      $03A5: Result:= $D5; //  GREEK CAPITAL LETTER UPSILON
      $03A6: Result:= $D6; //  GREEK CAPITAL LETTER PHI
      $03A7: Result:= $D7; //  GREEK CAPITAL LETTER CHI
      $03A8: Result:= $D8; //  GREEK CAPITAL LETTER PSI
      $03A9: Result:= $D9; //  GREEK CAPITAL LETTER OMEGA
      $03AA: Result:= $DA; //  GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
      $03AB: Result:= $DB; //  GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
      $03AC: Result:= $DC; //  GREEK SMALL LETTER ALPHA WITH TONOS
      $03AD: Result:= $DD; //  GREEK SMALL LETTER EPSILON WITH TONOS
      $03AE: Result:= $DE; //  GREEK SMALL LETTER ETA WITH TONOS
      $03AF: Result:= $DF; //  GREEK SMALL LETTER IOTA WITH TONOS
      $03B0: Result:= $E0; //  GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
      $03B1: Result:= $E1; //  GREEK SMALL LETTER ALPHA
      $03B2: Result:= $E2; //  GREEK SMALL LETTER BETA
      $03B3: Result:= $E3; //  GREEK SMALL LETTER GAMMA
      $03B4: Result:= $E4; //  GREEK SMALL LETTER DELTA
      $03B5: Result:= $E5; //  GREEK SMALL LETTER EPSILON
      $03B6: Result:= $E6; //  GREEK SMALL LETTER ZETA
      $03B7: Result:= $E7; //  GREEK SMALL LETTER ETA
      $03B8: Result:= $E8; //  GREEK SMALL LETTER THETA
      $03B9: Result:= $E9; //  GREEK SMALL LETTER IOTA
      $03BA: Result:= $EA; //  GREEK SMALL LETTER KAPPA
      $03BB: Result:= $EB; //  GREEK SMALL LETTER LAMDA
      $03BC: Result:= $EC; //  GREEK SMALL LETTER MU
      $03BD: Result:= $ED; //  GREEK SMALL LETTER NU
      $03BE: Result:= $EE; //  GREEK SMALL LETTER XI
      $03BF: Result:= $EF; //  GREEK SMALL LETTER OMICRON
      $03C0: Result:= $F0; //  GREEK SMALL LETTER PI
      $03C1: Result:= $F1; //  GREEK SMALL LETTER RHO
      $03C2: Result:= $F2; //  GREEK SMALL LETTER FINAL SIGMA
      $03C3: Result:= $F3; //  GREEK SMALL LETTER SIGMA
      $03C4: Result:= $F4; //  GREEK SMALL LETTER TAU
      $03C5: Result:= $F5; //  GREEK SMALL LETTER UPSILON
      $03C6: Result:= $F6; //  GREEK SMALL LETTER PHI
      $03C7: Result:= $F7; //  GREEK SMALL LETTER CHI
      $03C8: Result:= $F8; //  GREEK SMALL LETTER PSI
      $03C9: Result:= $F9; //  GREEK SMALL LETTER OMEGA
      $03CA: Result:= $FA; //  GREEK SMALL LETTER IOTA WITH DIALYTIKA
      $03CB: Result:= $FB; //  GREEK SMALL LETTER UPSILON WITH DIALYTIKA
      $03CC: Result:= $FC; //  GREEK SMALL LETTER OMICRON WITH TONOS
      $03CD: Result:= $FD; //  GREEK SMALL LETTER UPSILON WITH TONOS
      $03CE: Result:= $FE; //  GREEK SMALL LETTER OMEGA WITH TONOS
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharISO8859_8(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $00A0: Result:= $A0; //  NO-BREAK SPACE
      $00A2: Result:= $A2; //  CENT SIGN
      $00A3: Result:= $A3; //  POUND SIGN
      $00A4: Result:= $A4; //  CURRENCY SIGN
      $00A5: Result:= $A5; //  YEN SIGN
      $00A6: Result:= $A6; //  BROKEN BAR
      $00A7: Result:= $A7; //  SECTION SIGN
      $00A8: Result:= $A8; //  DIAERESIS
      $00A9: Result:= $A9; //  COPYRIGHT SIGN
      $00D7: Result:= $AA; //  MULTIPLICATION SIGN
      $00AB: Result:= $AB; //  LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; //  NOT SIGN
      $00AD: Result:= $AD; //  SOFT HYPHEN
      $00AE: Result:= $AE; //  REGISTERED SIGN
      $203E: Result:= $AF; //  OVERLINE
      $00B0: Result:= $B0; //  DEGREE SIGN
      $00B1: Result:= $B1; //  PLUS-MINUS SIGN
      $00B2: Result:= $B2; //  SUPERSCRIPT TWO
      $00B3: Result:= $B3; //  SUPERSCRIPT THREE
      $00B4: Result:= $B4; //  ACUTE ACCENT
      $00B5: Result:= $B5; //  MICRO SIGN
      $00B6: Result:= $B6; //  PILCROW SIGN
      $00B7: Result:= $B7; //  MIDDLE DOT
      $00B8: Result:= $B8; //  CEDILLA
      $00B9: Result:= $B9; //  SUPERSCRIPT ONE
      $00F7: Result:= $BA; //  DIVISION SIGN
      $00BB: Result:= $BB; //  RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00BC: Result:= $BC; //  VULGAR FRACTION ONE QUARTER
      $00BD: Result:= $BD; //  VULGAR FRACTION ONE HALF
      $00BE: Result:= $BE; //  VULGAR FRACTION THREE QUARTERS
      $2017: Result:= $DF; //  DOUBLE LOW LINE
      $05D0: Result:= $E0; //  HEBREW LETTER ALEF
      $05D1: Result:= $E1; //  HEBREW LETTER BET
      $05D2: Result:= $E2; //  HEBREW LETTER GIMEL
      $05D3: Result:= $E3; //  HEBREW LETTER DALET
      $05D4: Result:= $E4; //  HEBREW LETTER HE
      $05D5: Result:= $E5; //  HEBREW LETTER VAV
      $05D6: Result:= $E6; //  HEBREW LETTER ZAYIN
      $05D7: Result:= $E7; //  HEBREW LETTER HET
      $05D8: Result:= $E8; //  HEBREW LETTER TET
      $05D9: Result:= $E9; //  HEBREW LETTER YOD
      $05DA: Result:= $EA; //  HEBREW LETTER FINAL KAF
      $05DB: Result:= $EB; //  HEBREW LETTER KAF
      $05DC: Result:= $EC; //  HEBREW LETTER LAMED
      $05DD: Result:= $ED; //  HEBREW LETTER FINAL MEM
      $05DE: Result:= $EE; //  HEBREW LETTER MEM
      $05DF: Result:= $EF; //  HEBREW LETTER FINAL NUN
      $05E0: Result:= $F0; //  HEBREW LETTER NUN
      $05E1: Result:= $F1; //  HEBREW LETTER SAMEKH
      $05E2: Result:= $F2; //  HEBREW LETTER AYIN
      $05E3: Result:= $F3; //  HEBREW LETTER FINAL PE
      $05E4: Result:= $F4; //  HEBREW LETTER PE
      $05E5: Result:= $F5; //  HEBREW LETTER FINAL TSADI
      $05E6: Result:= $F6; //  HEBREW LETTER TSADI
      $05E7: Result:= $F7; //  HEBREW LETTER QOF
      $05E8: Result:= $F8; //  HEBREW LETTER RESH
      $05E9: Result:= $F9; //  HEBREW LETTER SHIN
      $05EA: Result:= $FA; //  HEBREW LETTER TAV
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharISO8859_9(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $00A0: Result:= $A0; //  NO-BREAK SPACE
      $00A1: Result:= $A1; //  INVERTED EXCLAMATION MARK
      $00A2: Result:= $A2; //  CENT SIGN
      $00A3: Result:= $A3; //  POUND SIGN
      $00A4: Result:= $A4; //  CURRENCY SIGN
      $00A5: Result:= $A5; //  YEN SIGN
      $00A6: Result:= $A6; //  BROKEN BAR
      $00A7: Result:= $A7; //  SECTION SIGN
      $00A8: Result:= $A8; //  DIAERESIS
      $00A9: Result:= $A9; //  COPYRIGHT SIGN
      $00AA: Result:= $AA; //  FEMININE ORDINAL INDICATOR
      $00AB: Result:= $AB; //  LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; //  NOT SIGN
      $00AD: Result:= $AD; //  SOFT HYPHEN
      $00AE: Result:= $AE; //  REGISTERED SIGN
      $00AF: Result:= $AF; //  MACRON
      $00B0: Result:= $B0; //  DEGREE SIGN
      $00B1: Result:= $B1; //  PLUS-MINUS SIGN
      $00B2: Result:= $B2; //  SUPERSCRIPT TWO
      $00B3: Result:= $B3; //  SUPERSCRIPT THREE
      $00B4: Result:= $B4; //  ACUTE ACCENT
      $00B5: Result:= $B5; //  MICRO SIGN
      $00B6: Result:= $B6; //  PILCROW SIGN
      $00B7: Result:= $B7; //  MIDDLE DOT
      $00B8: Result:= $B8; //  CEDILLA
      $00B9: Result:= $B9; //  SUPERSCRIPT ONE
      $00BA: Result:= $BA; //  MASCULINE ORDINAL INDICATOR
      $00BB: Result:= $BB; //  RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00BC: Result:= $BC; //  VULGAR FRACTION ONE QUARTER
      $00BD: Result:= $BD; //  VULGAR FRACTION ONE HALF
      $00BE: Result:= $BE; //  VULGAR FRACTION THREE QUARTERS
      $00BF: Result:= $BF; //  INVERTED QUESTION MARK
      $00C0: Result:= $C0; //  LATIN CAPITAL LETTER A WITH GRAVE
      $00C1: Result:= $C1; //  LATIN CAPITAL LETTER A WITH ACUTE
      $00C2: Result:= $C2; //  LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $00C3: Result:= $C3; //  LATIN CAPITAL LETTER A WITH TILDE
      $00C4: Result:= $C4; //  LATIN CAPITAL LETTER A WITH DIAERESIS
      $00C5: Result:= $C5; //  LATIN CAPITAL LETTER A WITH RING ABOVE
      $00C6: Result:= $C6; //  LATIN CAPITAL LETTER AE
      $00C7: Result:= $C7; //  LATIN CAPITAL LETTER C WITH CEDILLA
      $00C8: Result:= $C8; //  LATIN CAPITAL LETTER E WITH GRAVE
      $00C9: Result:= $C9; //  LATIN CAPITAL LETTER E WITH ACUTE
      $00CA: Result:= $CA; //  LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $00CB: Result:= $CB; //  LATIN CAPITAL LETTER E WITH DIAERESIS
      $00CC: Result:= $CC; //  LATIN CAPITAL LETTER I WITH GRAVE
      $00CD: Result:= $CD; //  LATIN CAPITAL LETTER I WITH ACUTE
      $00CE: Result:= $CE; //  LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $00CF: Result:= $CF; //  LATIN CAPITAL LETTER I WITH DIAERESIS
      $011E: Result:= $D0; //  LATIN CAPITAL LETTER G WITH BREVE
      $00D1: Result:= $D1; //  LATIN CAPITAL LETTER N WITH TILDE
      $00D2: Result:= $D2; //  LATIN CAPITAL LETTER O WITH GRAVE
      $00D3: Result:= $D3; //  LATIN CAPITAL LETTER O WITH ACUTE
      $00D4: Result:= $D4; //  LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $00D5: Result:= $D5; //  LATIN CAPITAL LETTER O WITH TILDE
      $00D6: Result:= $D6; //  LATIN CAPITAL LETTER O WITH DIAERESIS
      $00D7: Result:= $D7; //  MULTIPLICATION SIGN
      $00D8: Result:= $D8; //  LATIN CAPITAL LETTER O WITH STROKE
      $00D9: Result:= $D9; //  LATIN CAPITAL LETTER U WITH GRAVE
      $00DA: Result:= $DA; //  LATIN CAPITAL LETTER U WITH ACUTE
      $00DB: Result:= $DB; //  LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $00DC: Result:= $DC; //  LATIN CAPITAL LETTER U WITH DIAERESIS
      $0130: Result:= $DD; //  LATIN CAPITAL LETTER I WITH DOT ABOVE
      $015E: Result:= $DE; //  LATIN CAPITAL LETTER S WITH CEDILLA
      $00DF: Result:= $DF; //  LATIN SMALL LETTER SHARP S
      $00E0: Result:= $E0; //  LATIN SMALL LETTER A WITH GRAVE
      $00E1: Result:= $E1; //  LATIN SMALL LETTER A WITH ACUTE
      $00E2: Result:= $E2; //  LATIN SMALL LETTER A WITH CIRCUMFLEX
      $00E3: Result:= $E3; //  LATIN SMALL LETTER A WITH TILDE
      $00E4: Result:= $E4; //  LATIN SMALL LETTER A WITH DIAERESIS
      $00E5: Result:= $E5; //  LATIN SMALL LETTER A WITH RING ABOVE
      $00E6: Result:= $E6; //  LATIN SMALL LETTER AE
      $00E7: Result:= $E7; //  LATIN SMALL LETTER C WITH CEDILLA
      $00E8: Result:= $E8; //  LATIN SMALL LETTER E WITH GRAVE
      $00E9: Result:= $E9; //  LATIN SMALL LETTER E WITH ACUTE
      $00EA: Result:= $EA; //  LATIN SMALL LETTER E WITH CIRCUMFLEX
      $00EB: Result:= $EB; //  LATIN SMALL LETTER E WITH DIAERESIS
      $00EC: Result:= $EC; //  LATIN SMALL LETTER I WITH GRAVE
      $00ED: Result:= $ED; //  LATIN SMALL LETTER I WITH ACUTE
      $00EE: Result:= $EE; //  LATIN SMALL LETTER I WITH CIRCUMFLEX
      $00EF: Result:= $EF; //  LATIN SMALL LETTER I WITH DIAERESIS
      $011F: Result:= $F0; //  LATIN SMALL LETTER G WITH BREVE
      $00F1: Result:= $F1; //  LATIN SMALL LETTER N WITH TILDE
      $00F2: Result:= $F2; //  LATIN SMALL LETTER O WITH GRAVE
      $00F3: Result:= $F3; //  LATIN SMALL LETTER O WITH ACUTE
      $00F4: Result:= $F4; //  LATIN SMALL LETTER O WITH CIRCUMFLEX
      $00F5: Result:= $F5; //  LATIN SMALL LETTER O WITH TILDE
      $00F6: Result:= $F6; //  LATIN SMALL LETTER O WITH DIAERESIS
      $00F7: Result:= $F7; //  DIVISION SIGN
      $00F8: Result:= $F8; //  LATIN SMALL LETTER O WITH STROKE
      $00F9: Result:= $F9; //  LATIN SMALL LETTER U WITH GRAVE
      $00FA: Result:= $FA; //  LATIN SMALL LETTER U WITH ACUTE
      $00FB: Result:= $FB; //  LATIN SMALL LETTER U WITH CIRCUMFLEX
      $00FC: Result:= $FC; //  LATIN SMALL LETTER U WITH DIAERESIS
      $0131: Result:= $FD; //  LATIN SMALL LETTER DOTLESS I
      $015F: Result:= $FE; //  LATIN SMALL LETTER S WITH CEDILLA
      $00FF: Result:= $FF; //  LATIN SMALL LETTER Y WITH DIAERESIS
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

function RtcUnicodeToAnsiCharISO8859_15(Chr: Word): Byte;
begin
{$IFDEF RTC_BYTESTRING}
  Result := Byte(Chr);
{$ELSE}
  if (Chr <= 255) then
    Result := Chr
  else
    case Chr of
      $00A0: Result:= $A0; //  NO-BREAK SPACE
      $00A1: Result:= $A1; //  INVERTED EXCLAMATION MARK
      $00A2: Result:= $A2; //  CENT SIGN
      $00A3: Result:= $A3; //  POUND SIGN
      $20AC: Result:= $A4; //  EURO SIGN
      $00A5: Result:= $A5; //  YEN SIGN
      $0160: Result:= $A6; //  LATIN CAPITAL LETTER S WITH CARON
      $00A7: Result:= $A7; //  SECTION SIGN
      $0161: Result:= $A8; //  LATIN SMALL LETTER S WITH CARON
      $00A9: Result:= $A9; //  COPYRIGHT SIGN
      $00AA: Result:= $AA; //  FEMININE ORDINAL INDICATOR
      $00AB: Result:= $AB; //  LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
      $00AC: Result:= $AC; //  NOT SIGN
      $00AD: Result:= $AD; //  SOFT HYPHEN
      $00AE: Result:= $AE; //  REGISTERED SIGN
      $00AF: Result:= $AF; //  MACRON
      $00B0: Result:= $B0; //  DEGREE SIGN
      $00B1: Result:= $B1; //  PLUS-MINUS SIGN
      $00B2: Result:= $B2; //  SUPERSCRIPT TWO
      $00B3: Result:= $B3; //  SUPERSCRIPT THREE
      $017D: Result:= $B4; //  LATIN CAPITAL LETTER Z WITH CARON
      $00B5: Result:= $B5; //  MICRO SIGN
      $00B6: Result:= $B6; //  PILCROW SIGN
      $00B7: Result:= $B7; //  MIDDLE DOT
      $017E: Result:= $B8; //  LATIN SMALL LETTER Z WITH CARON
      $00B9: Result:= $B9; //  SUPERSCRIPT ONE
      $00BA: Result:= $BA; //  MASCULINE ORDINAL INDICATOR
      $00BB: Result:= $BB; //  RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
      $0152: Result:= $BC; //  LATIN CAPITAL LIGATURE OE
      $0153: Result:= $BD; //  LATIN SMALL LIGATURE OE
      $0178: Result:= $BE; //  LATIN CAPITAL LETTER Y WITH DIAERESIS
      $00BF: Result:= $BF; //  INVERTED QUESTION MARK
      $00C0: Result:= $C0; //  LATIN CAPITAL LETTER A WITH GRAVE
      $00C1: Result:= $C1; //  LATIN CAPITAL LETTER A WITH ACUTE
      $00C2: Result:= $C2; //  LATIN CAPITAL LETTER A WITH CIRCUMFLEX
      $00C3: Result:= $C3; //  LATIN CAPITAL LETTER A WITH TILDE
      $00C4: Result:= $C4; //  LATIN CAPITAL LETTER A WITH DIAERESIS
      $00C5: Result:= $C5; //  LATIN CAPITAL LETTER A WITH RING ABOVE
      $00C6: Result:= $C6; //  LATIN CAPITAL LETTER AE
      $00C7: Result:= $C7; //  LATIN CAPITAL LETTER C WITH CEDILLA
      $00C8: Result:= $C8; //  LATIN CAPITAL LETTER E WITH GRAVE
      $00C9: Result:= $C9; //  LATIN CAPITAL LETTER E WITH ACUTE
      $00CA: Result:= $CA; //  LATIN CAPITAL LETTER E WITH CIRCUMFLEX
      $00CB: Result:= $CB; //  LATIN CAPITAL LETTER E WITH DIAERESIS
      $00CC: Result:= $CC; //  LATIN CAPITAL LETTER I WITH GRAVE
      $00CD: Result:= $CD; //  LATIN CAPITAL LETTER I WITH ACUTE
      $00CE: Result:= $CE; //  LATIN CAPITAL LETTER I WITH CIRCUMFLEX
      $00CF: Result:= $CF; //  LATIN CAPITAL LETTER I WITH DIAERESIS
      $00D0: Result:= $D0; //  LATIN CAPITAL LETTER ETH
      $00D1: Result:= $D1; //  LATIN CAPITAL LETTER N WITH TILDE
      $00D2: Result:= $D2; //  LATIN CAPITAL LETTER O WITH GRAVE
      $00D3: Result:= $D3; //  LATIN CAPITAL LETTER O WITH ACUTE
      $00D4: Result:= $D4; //  LATIN CAPITAL LETTER O WITH CIRCUMFLEX
      $00D5: Result:= $D5; //  LATIN CAPITAL LETTER O WITH TILDE
      $00D6: Result:= $D6; //  LATIN CAPITAL LETTER O WITH DIAERESIS
      $00D7: Result:= $D7; //  MULTIPLICATION SIGN
      $00D8: Result:= $D8; //  LATIN CAPITAL LETTER O WITH STROKE
      $00D9: Result:= $D9; //  LATIN CAPITAL LETTER U WITH GRAVE
      $00DA: Result:= $DA; //  LATIN CAPITAL LETTER U WITH ACUTE
      $00DB: Result:= $DB; //  LATIN CAPITAL LETTER U WITH CIRCUMFLEX
      $00DC: Result:= $DC; //  LATIN CAPITAL LETTER U WITH DIAERESIS
      $00DD: Result:= $DD; //  LATIN CAPITAL LETTER Y WITH ACUTE
      $00DE: Result:= $DE; //  LATIN CAPITAL LETTER THORN
      $00DF: Result:= $DF; //  LATIN SMALL LETTER SHARP S
      $00E0: Result:= $E0; //  LATIN SMALL LETTER A WITH GRAVE
      $00E1: Result:= $E1; //  LATIN SMALL LETTER A WITH ACUTE
      $00E2: Result:= $E2; //  LATIN SMALL LETTER A WITH CIRCUMFLEX
      $00E3: Result:= $E3; //  LATIN SMALL LETTER A WITH TILDE
      $00E4: Result:= $E4; //  LATIN SMALL LETTER A WITH DIAERESIS
      $00E5: Result:= $E5; //  LATIN SMALL LETTER A WITH RING ABOVE
      $00E6: Result:= $E6; //  LATIN SMALL LETTER AE
      $00E7: Result:= $E7; //  LATIN SMALL LETTER C WITH CEDILLA
      $00E8: Result:= $E8; //  LATIN SMALL LETTER E WITH GRAVE
      $00E9: Result:= $E9; //  LATIN SMALL LETTER E WITH ACUTE
      $00EA: Result:= $EA; //  LATIN SMALL LETTER E WITH CIRCUMFLEX
      $00EB: Result:= $EB; //  LATIN SMALL LETTER E WITH DIAERESIS
      $00EC: Result:= $EC; //  LATIN SMALL LETTER I WITH GRAVE
      $00ED: Result:= $ED; //  LATIN SMALL LETTER I WITH ACUTE
      $00EE: Result:= $EE; //  LATIN SMALL LETTER I WITH CIRCUMFLEX
      $00EF: Result:= $EF; //  LATIN SMALL LETTER I WITH DIAERESIS
      $00F0: Result:= $F0; //  LATIN SMALL LETTER ETH
      $00F1: Result:= $F1; //  LATIN SMALL LETTER N WITH TILDE
      $00F2: Result:= $F2; //  LATIN SMALL LETTER O WITH GRAVE
      $00F3: Result:= $F3; //  LATIN SMALL LETTER O WITH ACUTE
      $00F4: Result:= $F4; //  LATIN SMALL LETTER O WITH CIRCUMFLEX
      $00F5: Result:= $F5; //  LATIN SMALL LETTER O WITH TILDE
      $00F6: Result:= $F6; //  LATIN SMALL LETTER O WITH DIAERESIS
      $00F7: Result:= $F7; //  DIVISION SIGN
      $00F8: Result:= $F8; //  LATIN SMALL LETTER O WITH STROKE
      $00F9: Result:= $F9; //  LATIN SMALL LETTER U WITH GRAVE
      $00FA: Result:= $FA; //  LATIN SMALL LETTER U WITH ACUTE
      $00FB: Result:= $FB; //  LATIN SMALL LETTER U WITH CIRCUMFLEX
      $00FC: Result:= $FC; //  LATIN SMALL LETTER U WITH DIAERESIS
      $00FD: Result:= $FD; //  LATIN SMALL LETTER Y WITH ACUTE
      $00FE: Result:= $FE; //  LATIN SMALL LETTER THORN
      $00FF: Result:= $FF; //  LATIN SMALL LETTER Y WITH DIAERESIS
      else Result := RTC_INVALID_CHAR;
    end;
{$ENDIF}
  end;

procedure RtcSetAnsiCodePage(page:RtcAnsiCodePages);
  begin
  case page of
    cpNone:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharNone;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharNone;
      end;
    cpWin1250:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharWin1250;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharWin1250;
      end;
    cpWin1251:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharWin1251;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharWin1251;
      end;
    cpWin1252:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharWin1252;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharWin1252;
      end;
    cpWin1253:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharWin1253;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharWin1253;
      end;
    cpWin1254:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharWin1254;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharWin1254;
      end;
    cpWin1255:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharWin1255;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharWin1255;
      end;
    cpWin1256:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharWin1256;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharWin1256;
      end;
    cpWin1257:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharWin1257;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharWin1257;
      end;
    cpWin1258:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharWin1258;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharWin1258;
      end;
    cpWin874:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharWin874;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharWin874;
      end;
    cpISO8859_1:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharISO8859_1;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharISO8859_1;
      end;
    cpISO8859_2:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharISO8859_2;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharISO8859_2;
      end;
    cpISO8859_3:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharISO8859_3;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharISO8859_3;
      end;
    cpISO8859_4:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharISO8859_4;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharISO8859_4;
      end;
    cpISO8859_5:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharISO8859_5;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharISO8859_5;
      end;
    cpISO8859_6:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharISO8859_6;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharISO8859_6;
      end;
    cpISO8859_7:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharISO8859_7;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharISO8859_7;
      end;
    cpISO8859_8:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharISO8859_8;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharISO8859_8;
      end;
    cpISO8859_9:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharISO8859_9;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharISO8859_9;
      end;
    cpISO8859_15:
      begin
      @RtcUnicodeToAnsiChar := @RtcUnicodeToAnsiCharISO8859_15;
      @RtcAnsiToUnicodeChar := @RtcAnsiToUnicodeCharISO8859_15;
      end;
    end;
  end;

function RtcUnicodeToAnsiString(const Source:RtcString):RtcString;
{$IFDEF RTC_BYTESTRING}
  begin
  Result:=Source;
  end;
{$ELSE}
  var
    i: Integer;
  begin
  SetLength(Result,Length(Source));
  for i := 1 to Length(Source) do
    Result[i]:=RtcChar(RtcUnicodeToAnsiChar(Word(Source[i])));
  end;
{$ENDIF}

function RtcAnsiToUnicodeString(const Source:RtcString):RtcString;
{$IFDEF RTC_BYTESTRING}
  begin
  Result:=Source;
  end;
{$ELSE}
  var
    i: Integer;
  begin
  SetLength(Result,Length(Source));
  for i := 1 to Length(Source) do
    Result[i]:=RtcChar(RtcAnsiToUnicodeChar(Byte(Source[i])));
  end;
{$ENDIF}

const
  CHR_A = Ord('a');
  CHR_Z = Ord('z');
  CHR_uA = Ord('A');
  CHR_uZ = Ord('Z');

{$IFDEF UNICODE}
  {$IFDEF RTC_BYTESTRING}
function UpperCase(const s:RtcString):RtcString; overload;
  var
    i:integer;
    c,d:^RtcBinChar;
  begin
  i:=Length(s);
  SetLength(Result,i);
  if i>0 then
    begin
    c:=@(s[1]);
    d:=@(Result[1]);
    for i:=1 to i do
      begin
      if (c^>=CHR_A) and (c^<=CHR_Z) then
        d^:=c^ - 32
      else
        d^:=c^;
      Inc(d);
      Inc(c);
      end;
    end;
  end;

function Trim(const S: RtcString): RtcString; overload;
  var
    I, L: Integer;
  begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    SetLength(Result,0)
  else
    begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
    end;
  end;
  {$ENDIF}
{$ENDIF}

function Lower_Case(const s:RtcString):RtcString;
  var
    i:integer;
    c,d:^RtcBinChar;
  begin
  i:=Length(s);
  SetLength(Result,i);
  if i>0 then
    begin
    c:=@(s[1]);
    d:=@(Result[1]);
    for i:=1 to i do
      begin
      if (c^>=CHR_uA) and (c^<=CHR_uZ) then
        d^:=c^ + 32
      else
        d^:=c^;
      Inc(d);
      Inc(c);
      end;
    end;
  end;

function Upper_Case(const s:RtcString):RtcString;
  var
    i:integer;
    c,d:^RtcBinChar;
  begin
  i:=Length(s);
  SetLength(Result,i);
  if i>0 then
    begin
    c:=@(s[1]);
    d:=@(Result[1]);
    for i:=1 to i do
      begin
      if (c^>=CHR_A) and (c^<=CHR_Z) then
        d^:=c^ - 32
      else
        d^:=c^;
      Inc(d);
      Inc(c);
      end;
    end;
  end;

function Up_Case(const c:RtcChar):RtcChar;
  begin
  if (c>='a') and (c<='z') then
    Result:=RtcChar(Ord(c) - 32)
  else
    Result:=c;
  end;

function Up_WCase(const c:RtcWideChar):RtcWideChar;
  begin
  if (c>='a') and (c<='z') then
    Result:=RtcWideChar(Ord(c) - 32)
  else
    Result:=c;
  end;

function Same_Text(const s1,s2:RtcString):boolean;
  var
    i:integer;
    c,d:^RtcBinChar;
    e,f:RtcBinChar;
  begin
  i:=Length(s1);
  if i<>Length(s2) then
    Result:=False
  else if i>0 then
    begin
    Result:=True;
    c:=@(s1[1]);
    d:=@(s2[1]);
    for i:=1 to i do
      begin
      if (c^>=CHR_A) and (c^<=CHR_Z) then
        e:=c^ - 32
      else
        e:=c^;
      if (d^>=CHR_A) and (d^<=CHR_Z) then
        f:=d^ - 32
      else
        f:=d^;
      if e<>f then
        begin
        Result:=False;
        Break;
        end;
      Inc(d);
      Inc(c);
      end;
    end
  else
    Result:=True;
  end;

function Same_WText(const s1,s2:RtcWideString):boolean;
  var
    i:integer;
    c,d:^RtcBinWideChar;
    e,f:RtcBinWideChar;
  begin
  i:=Length(s1);
  if i<>Length(s2) then
    Result:=False
  else if i>0 then
    begin
    Result:=True;
    c:=@(s1[1]);
    d:=@(s2[1]);
    for i:=1 to i do
      begin
      if (c^>=CHR_A) and (c^<=CHR_Z) then
        e:=c^ - 32
      else
        e:=c^;
      if (d^>=CHR_A) and (d^<=CHR_Z) then
        f:=d^ - 32
      else
        f:=d^;
      if e<>f then
        begin
        Result:=False;
        Break;
        end;
      Inc(d);
      Inc(c);
      end;
    end
  else
    Result:=True;
  end;

function Lower_WCase(const s:RtcWideString):RtcWideString;
  var
    i:integer;
    c,d:^RtcBinWideChar;
  begin
  i:=Length(s);
  SetLength(Result,i);
  if i>0 then
    begin
    c:=@(s[1]);
    d:=@(Result[1]);
    for i:=1 to i do
      begin
      if (c^>=CHR_uA) and (c^<=CHR_uZ) then
        d^:=c^ + 32
      else
        d^:=c^;
      Inc(d);
      Inc(c);
      end;
    end;
  end;

function Upper_WCase(const s:RtcWideString):RtcWideString;
  var
    i:integer;
    c,d:^RtcBinWideChar;
  begin
  i:=Length(s);
  SetLength(Result,i);
  if i>0 then
    begin
    c:=@(s[1]);
    d:=@(Result[1]);
    for i:=1 to i do
      begin
      if (c^>=CHR_A) and (c^<=CHR_Z) then
        d^:=c^ - 32
      else
        d^:=c^;
      Inc(d);
      Inc(c);
      end;
    end;
  end;

procedure RtcStringToByteArray(const Source:RtcString; var Dest:RtcByteArray; SourceLoc:Integer=1; len:Integer=-1; DestLoc:Integer=0);
  var
    i, k: Integer;
  begin
  if len<0 then len:=Length(Source)-SourceLoc+1;
  if len = 0 then Exit;
  k := SourceLoc;
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixDown then
    begin
    for i:=DestLoc to DestLoc+len-1 do
      begin
      if Ord(Source[k])>255 then
        begin
        Dest[i] := RtcUnicodeToAnsiChar(Ord(Source[k]));
        if RTC_STRING_CHECK and (Dest[i]=RTC_INVALID_CHAR) then
          raise Exception.Create('RtcStringToByteArray: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Char(Source[k]));
        end
      else
        Dest[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else if RTC_STRING_CHECK then
    begin
    for i:=DestLoc to DestLoc+len-1 do
      begin
      if Ord(Source[k])>255 then
        raise Exception.Create('RtcStringToByteArray: String contains Unicode character #'+IntToStr(Ord(Source[k]))+' = '+Char(Source[k]))
      else
        Dest[i] := Byte(Source[k]);
      Inc(k);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=DestLoc to DestLoc+len-1 do
      begin
      Dest[i] := Byte(Source[k]);
      Inc(k);
      end;
    end;
  end;

procedure RtcByteArrayToString(const Source:RtcByteArray; var Dest:RtcString; SourceLoc:Integer=0; len:Integer=-1; DestLoc:Integer=1);
  var
    i, k: Integer;
  begin
  if len<0 then len:=Length(Source)-SourceLoc;
  if len = 0 then Exit;
  k := SourceLoc;
{$IFNDEF RTC_BYTESTRING}
  if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
    begin
    for i:=DestLoc to DestLoc+len-1 do
      begin
      if (Source[k]<128) or (Source[k]>159) then
        Dest[i] := RtcChar(Source[k])
      else
        Dest[i] := RtcChar(RtcAnsiToUnicodeChar(Source[k]));
      Inc(k);
      end;
    end
  else
{$ENDIF}
    begin
    for i:=DestLoc to DestLoc+len-1 do
      begin
      Dest[i] := RtcChar(Source[k]);
      Inc(k);
      end;
    end;
  end;

{ TRtcHugeString }

constructor TRtcHugeString.Create;
  begin
  inherited;

  FPack:=nil;
  SetLength(FData,0);
  FDataCnt:=0;

  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);

  FPackCnt:=0;
  FPackFree:=0;
  FPackLoc:=0;

  FCount:=0;
  FSize:=0;
  end;

destructor TRtcHugeString.Destroy;
  begin
  Clear;
  if FPack<>nil then Dispose(FPack);
  inherited;
  end;

procedure TRtcHugeString.Clear;
  var
    a,b:integer;
    FPack2:PRtcStrArr;
  begin
  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        SetLength(FPack2^[b].str,0);
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      SetLength(FPack^[b].str,0);
    FPackCnt:=0;
    FPackFree:=0;
    FPackLoc:=0;
    end;

  FSize:=0;
  FCount:=0;
  end;

procedure TRtcHugeString.GrowHugeStringList;
  begin
  if Length(FData)<=FDataCnt then
    SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
  FData[FDataCnt]:=FPack;
  Inc(FDataCnt);

  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);
  FPackCnt:=0;
  end;

procedure TRtcHugeString.Add(const s: RtcString; len:Integer=-1);
  begin
  if len<0 then len:=Length(s);
  if len>0 then
    begin
    FSize:=FSize + len;

    if FPackFree>=len then
      begin
      with FPack^[FPackCnt-1] do
        begin
        Move(s[1], str[FPackLoc], len * SizeOf(RtcChar));
        Inc(siz, len);
        end;
      Inc(FPackLoc,len);
      Dec(FPackFree,len);
      end
    else
      begin
      if FPackCnt>=RTC_STROBJ_PACK then
        GrowHugeStringList;

      if len>=255 then
        begin
        with FPack^[FPackCnt] do
          begin
          {$IFDEF RTC_WIDESTRING}
            SetLength(str, len);
            Move(s[1],str[1],len * SizeOf(RtcChar));
          {$ELSE}
            str:=s;
          {$ENDIF}
          siz:=len;
          end;
        FPackFree:=0;
        FPackLoc:=0;
        end
      else
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, 254);
          Move(s[1],str[1],len * SizeOf(RtcChar));
          siz:=len;
          end;
        FPackFree:=254-len;
        FPackLoc:=len+1;
        end;
      Inc(FPackCnt);
      Inc(FCount);
      end;
    end;
  end;

procedure TRtcHugeString.AddEx(s:byte);
  var
    b:RtcByteArray;
  begin
  SetLength(b,1);
  b[0]:=s;
  AddEx(b);
  SetLength(b,0);
  end;

procedure TRtcHugeString.AddEx(const s:RtcByteArray; len: Integer);
  begin
  if len<0 then len:=Length(s);
  if len>0 then
    begin
    FSize:=FSize + len;

    if FPackFree>=len then
      begin
      with FPack^[FPackCnt-1] do
        begin
      {$IFDEF RTC_BYTESTRING}
        Move(s[0], str[FPackLoc], len);
      {$ELSE}
        RtcByteArrayToString(s,str,0,len,FPackLoc);
      {$ENDIF}
        Inc(siz, len);
        end;
      Inc(FPackLoc,len);
      Dec(FPackFree,len);
      end
    else
      begin
      if FPackCnt>=RTC_STROBJ_PACK then
        GrowHugeStringList;

      if len>=255 then
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, len);
        {$IFDEF RTC_BYTESTRING}
          Move(s[0],str[1],len);
        {$ELSE}
          RtcByteArrayToString(s,str,0,len,1);
        {$ENDIF}
          siz:=len;
          end;
        FPackFree:=0;
        FPackLoc:=0;
        end
      else
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, 254);
        {$IFDEF RTC_BYTESTRING}
          Move(s[0],str[1],len);
        {$ELSE}
          RtcByteArrayToString(s,str,0,len,1);
        {$ENDIF}
          siz:=len;
          end;
        FPackFree:=254-len;
        FPackLoc:=len+1;
        end;
      Inc(FPackCnt);
      Inc(FCount);
      end;
    end;
  end;

function TRtcHugeString.Get: RtcString;
  var
    a,b,loc:integer;
    FPack2:PRtcStrArr;
  begin
  if FCount>1 then
    begin
    SetLength(Result, FSize);
    loc:=1;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          if siz>0 then
            begin
            Move(str[1], Result[loc], siz * SizeOf(RtcChar));
            Inc(loc, siz);
            end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        if siz>0 then
          begin
          Move(str[1], Result[loc], siz * SizeOf(RtcChar));
          Inc(loc, siz);
          end;

    if loc<>FSize+1 then
      raise Exception.Create('TRtcHugeString.Get: Internal error.');
    end
  else if FCount>0 then
    begin
    with FPack^[0] do
      if siz>=254 then
        Result:=str
      else
        begin
        SetLength(Result, siz);
        if siz>0 then
          Move(str[1], Result[1], siz * SizeOf(RtcChar));
        end;
    end
  else
    SetLength(Result,0);
  end;

function TRtcHugeString.GetEx: RtcByteArray;
  var
    a,b,loc:integer;
    FPack2:PRtcStrArr;
  begin
  if FCount>1 then
    begin
    SetLength(Result, FSize);
    loc:=0;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          if siz>0 then
            begin
          {$IFDEF RTC_BYTESTRING}
            Move(str[1], Result[loc], siz);
          {$ELSE}
            RtcStringToByteArray(str,Result,1,siz,loc);
          {$ENDIF}
            Inc(loc, siz);
            end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        if siz>0 then
          begin
        {$IFDEF RTC_BYTESTRING}
          Move(str[1], Result[loc], siz);
        {$ELSE}
          RtcStringToByteArray(str,Result,1,siz,loc);
        {$ENDIF}
          Inc(loc, siz);
          end;

    if loc<>FSize then
      raise Exception.Create('TRtcHugeString.GetEx: Internal error.');
    end
  else if FCount>0 then
    begin
    with FPack^[0] do
      begin
      SetLength(Result, siz);
      if siz>0 then
        begin
      {$IFDEF RTC_BYTESTRING}
        Move(str[1], Result[0], siz);
      {$ELSE}
        RtcStringToByteArray(str,Result,1,siz,0);
      {$ENDIF}
        end;
      end;
    end
  else
    SetLength(Result,0);
  end;

{ TRtcHugeByteArray }

constructor TRtcHugeByteArray.Create;
  begin
  inherited;

  FPack:=nil;
  SetLength(FData,0);
  FDataCnt:=0;

  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);

  FPackCnt:=0;
  FPackFree:=0;
  FPackLoc:=0;

  FCount:=0;
  FSize:=0;
  end;

destructor TRtcHugeByteArray.Destroy;
  begin
  Clear;
  if FPack<>nil then Dispose(FPack);
  inherited;
  end;

procedure TRtcHugeByteArray.Clear;
  var
    a,b:integer;
    FPack2:PRtcBytesArr;
  begin
  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        SetLength(FPack2^[b].str,0);
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      SetLength(FPack^[b].str,0);
    FPackCnt:=0;
    FPackFree:=0;
    FPackLoc:=0;
    end;

  FSize:=0;
  FCount:=0;
  end;

procedure TRtcHugeByteArray.GrowHugeStringList;
  begin
  if Length(FData)<=FDataCnt then
    SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
  FData[FDataCnt]:=FPack;
  Inc(FDataCnt);

  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);
  FPackCnt:=0;
  end;

procedure TRtcHugeByteArray.AddEx(s:byte);
  var
    b:RtcByteArray;
  begin
  SetLength(b,1);
  b[0]:=s;
  AddEx(b);
  SetLength(b,0);
  end;

procedure TRtcHugeByteArray.AddEx(const s:RtcByteArray; len:integer=-1; loc:integer=0);
  begin
  if len=-1 then len:=Length(s)-loc;
  if len>0 then
    begin
    FSize:=FSize + len;

    if FPackFree>=len then
      begin
      with FPack^[FPackCnt-1] do
        begin
        Move(s[loc], str[FPackLoc], len);
        Inc(siz, len);
        end;
      Inc(FPackLoc,len);
      Dec(FPackFree,len);
      end
    else
      begin
      if FPackCnt>=RTC_STROBJ_PACK then
        GrowHugeStringList;

      if len>=255 then
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, len);
          Move(s[loc],str[0],len);
          siz:=len;
          end;
        FPackFree:=0;
        FPackLoc:=0;
        end
      else
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, 254);
          Move(s[loc],str[0],len);
          siz:=len;
          end;
        FPackFree:=254-len;
        FPackLoc:=len;
        end;
      Inc(FPackCnt);
      Inc(FCount);
      end;
    end;
  end;

procedure TRtcHugeByteArray.Add(const s: RtcString; len: Integer=-1; loc: Integer=1);
  begin
  if len=-1 then len:=Length(s)-loc+1;
  if len>0 then
    begin
    FSize:=FSize + len;

    if FPackFree>=len then
      begin
      with FPack^[FPackCnt-1] do
        begin
        {$IFDEF RTC_BYTESTRING}
        Move(s[loc], str[FPackLoc], len);
        {$ELSE}
        RtcStringToByteArray(s,str,loc,len,FPackLoc);
        {$ENDIF}
        Inc(siz, len);
        end;
      Inc(FPackLoc,len);
      Dec(FPackFree,len);
      end
    else
      begin
      if FPackCnt>=RTC_STROBJ_PACK then
        GrowHugeStringList;

      if len>=255 then
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, len);
          {$IFDEF RTC_BYTESTRING}
          Move(s[loc],str[0],len);
          {$ELSE}
          RtcStringToByteArray(s,str,loc,len,0);
          {$ENDIF}
          siz:=len;
          end;
        FPackFree:=0;
        FPackLoc:=0;
        end
      else
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, 254);
          {$IFDEF RTC_BYTESTRING}
          Move(s[loc],str[0],len);
          {$ELSE}
          RtcStringToByteArray(s,str,loc,len,0);
          {$ENDIF}
          siz:=len;
          end;
        FPackFree:=254-len;
        FPackLoc:=len;
        end;
      Inc(FPackCnt);
      Inc(FCount);
      end;
    end;
  end;

function TRtcHugeByteArray.GetEx: RtcByteArray;
  var
    a,b,loc:integer;
    FPack2:PRtcBytesArr;
  begin
  if FCount>1 then
    begin
    SetLength(Result, FSize);
    loc:=0;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          if siz>0 then
            begin
            Move(str[0], Result[loc], siz);
            Inc(loc, siz);
            end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        if siz>0 then
          begin
          Move(str[0], Result[loc], siz);
          Inc(loc, siz);
          end;

    if loc<>FSize then
      raise Exception.Create('TRtcHugeByteArray.GetEx: Internal Error.');
    end
  else if FCount>0 then
    begin
    with FPack^[0] do
      if siz>=254 then
        Result:=str
      else
        begin
        SetLength(Result, siz);
        if siz>0 then
          Move(str[0], Result[0], siz);
        end;
    end
  else
    SetLength(Result,0);
  end;

function TRtcHugeByteArray.Get: RtcString;
  var
    a,b,loc:integer;
    FPack2:PRtcBytesArr;
  begin
  if FCount>1 then
    begin
    SetLength(Result, FSize);
    loc:=1;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          if siz>0 then
            begin
            {$IFDEF RTC_BYTESTRING}
            Move(str[0], Result[loc], siz);
            {$ELSE}
            RtcByteArrayToString(str,Result,0,siz,loc);
            {$ENDIF}
            Inc(loc, siz);
            end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        if siz>0 then
          begin
          {$IFDEF RTC_BYTESTRING}
          Move(str[0], Result[loc], siz);
          {$ELSE}
          RtcByteArrayToString(str,Result,0,siz,loc);
          {$ENDIF}
          Inc(loc, siz);
          end;

    if loc<>FSize+1 then
      raise Exception.Create('TRtcHugeByteArray.Get: Internal Error.');
    end
  else if FCount>0 then
    begin
    with FPack^[0] do
      begin
      SetLength(Result, siz);
      if siz>0 then
        begin
        {$IFDEF RTC_BYTESTRING}
        Move(str[0], Result[1], siz);
        {$ELSE}
        RtcByteArrayToString(str,Result,0,siz,1);
        {$ENDIF}
        end;
      end;
    end
  else
    SetLength(Result,0);
  end;

function TRtcHugeByteArray.GetStartEx(len: integer): RtcByteArray;
  var
    a,b,loc:integer;
    FPack2:PRtcBytesArr;
  begin
  if FCount>1 then
    begin
    if len>FSize then len:=FSize;

    SetLength(Result, len);

    if len=0 then Exit;

    loc:=0;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          if siz>=len then
            begin
            Move(str[0], Result[loc], len);
            Exit;
            end
          else if siz>0 then
            begin
            Move(str[0], Result[loc], siz);
            Inc(loc, siz);
            Dec(len, siz);
            end;
          end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        if siz>=len then
          begin
          Move(str[0], Result[loc], len);
          Exit;
          end
        else if siz>0 then
          begin
          Move(str[0], Result[loc], siz);
          Inc(loc, siz);
          Dec(len, siz);
          end;
        end;

    if loc<>FSize then
      raise Exception.Create('TRtcHugeByteArray.GetStartEx: Internal Error.');
    end
  else if FCount>0 then
    begin
    with FPack^[0] do
      begin
      if siz>=len then
        begin
        SetLength(Result, len);
        Move(str[0], Result[0], len);
        end
      else
        begin
        SetLength(Result, siz);
        if siz>0 then
          Move(str[0], Result[0], siz);
        end;
      end;
    end
  else
    SetLength(Result,0);
  end;


procedure TRtcHugeByteArray.DelStart(len: integer);
  var
    a,b,loc:integer;
    FPack2:PRtcBytesArr;
  begin
  if len=0 then
    Exit
  else if len>=Size then
    begin
    Clear;
    Exit;
    end;

  if FCount>1 then
    begin
    FSize:=FSize-len;

    loc:=0;

    FPackFree:=0;
    FPackLoc:=0;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          if siz>len then
            begin
            Move(str[len], str[0], siz-len);
            siz:=siz-len;
            Exit;
            end
          else
            begin
            SetLength(str,0);
            Inc(loc, siz);
            Dec(len, siz);
            siz:=0;
            end;
          end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        if siz>len then
          begin
          Move(str[len], str[0], siz-len);
          siz:=siz-len;
          Exit;
          end
        else
          begin
          SetLength(str,0);
          Inc(loc, siz);
          Dec(len, siz);
          siz:=0;
          end;
        end;

    if loc<>FSize then
      raise Exception.Create('TRtcHugeByteArray.DelStart: Internal Error.');
    end
  else if FCount>0 then
    begin
    FPackFree:=0;
    FPackLoc:=0;
    FSize:=FSize-len;

    with FPack^[0] do
      begin
      if siz>len then
        begin
        Move(str[len], str[0], siz-len);
        siz:=siz-len;
        end
      else
        begin
        SetLength(str,0);
        siz:=0;
        end;
      end;
    end;
  end;

procedure TRtcHugeByteArray.AddPackEx(const s: RtcByteArray;
                                      packSize:integer; len:Integer=-1; loc:integer=0);
  var
    pack:integer;
  begin
  if len=-1 then len:=Length(s)-loc;
  while len>0 do
    begin
    pack:=len;
    if pack>packSize then pack:=packSize;
    AddEx(s,pack,loc);
    len:=len-pack;
    loc:=loc+pack;
    end;
  end;

procedure TRtcHugeByteArray.AddPack(const s: RtcString;
                                    packSize:integer; len:Integer=-1; loc:integer=1);
  var
    pack:integer;
  begin
  if len=-1 then len:=Length(s)-loc+1;
  while len>0 do
    begin
    pack:=len;
    if pack>packSize then pack:=packSize;
    Add(s,pack,loc);
    len:=len-pack;
    loc:=loc+pack;
    end;
  end;

{ tRtcFastStrObjList }

constructor tRtcFastStrObjList.Create;
  begin
  inherited;
  FPack:=nil;
  Tree:=tStrIntList.Create(RTC_STROBJ_PACK);

  SetLength(FData,0);
  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);

  FCnt:=0;
  FDataCnt:=0;
  FPackCnt:=0;
  end;

destructor tRtcFastStrObjList.Destroy;
  begin
  Clear;
  if FPack<>nil then Dispose(FPack);
  RtcFreeAndNil(Tree);
  inherited;
  end;

procedure tRtcFastStrObjList.Clear;
  var
    a,b:integer;
    FPack2:PRtcStrObjArr;
  begin
  if self=nil then Exit;
  
  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        SetLength(str,0);
        obj:=nil;
        end;
    FPackCnt:=0;
    end;

  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          SetLength(str,0);
          obj:=nil;
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  if assigned(Tree) then
    Tree.removeall;
  FCnt:=0;

  if assigned(FOnChange) then FOnChange(self);
  end;

procedure tRtcFastStrObjList.DestroyObjects;
  var
    a,b,c:integer;
    FPack2:PRtcStrObjArr;
  begin
  if self=nil then Exit;

  if FPackCnt>0 then
    begin
    c:=FPackCnt;
    FPackCnt:=0;
    for b:=0 to c-1 do
      with FPack^[b] do
        begin
        SetLength(str,0);
        obj.Free;
        end;
    end;

  if FDataCnt>0 then
    begin
    c:=FDataCnt;
    FDataCnt:=0;
    for a:=0 to c-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          SetLength(str,0);
          obj.Free;
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    end;

  if assigned(Tree) then
    Tree.removeall;
  FCnt:=0;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrObjList.Add(const Name: RtcString; _Value:TObject=nil): integer;
  procedure GrowStrObjList;
    begin
    if Length(FData)<=FDataCnt then
      SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
    FData[FDataCnt]:=FPack;
    Inc(FDataCnt);

    New(FPack);
    FillChar(FPack^,SizeOf(FPack^),0);
    FPackCnt:=0;
    end;
  begin
  if FPackCnt>=RTC_STROBJ_PACK then
    GrowStrObjList;

  Tree.insert(Upper_Case(Name), FCnt);
  with FPack[FPackCnt] do
    begin
    str:=Name;
    obj:=_Value;
    end;
  Inc(FPackCnt);
  Inc(FCnt);

  Result:=FCnt-1;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrObjList.Find(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Upper_Case(Name));
  end;

function tRtcFastStrObjList.IndexOf(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Upper_Case(Name));
  end;

function tRtcFastStrObjList.AddCS(const Name: RtcString; _Value:TObject=nil): integer;
  begin
  if FPackCnt>=RTC_STROBJ_PACK then
    begin
    if Length(FData)<=FDataCnt then
      SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
    FData[FDataCnt]:=FPack;
    Inc(FDataCnt);

    New(FPack);
    FillChar(FPack^,SizeOf(FPack^),0);
    FPackCnt:=0;
    end;

  Tree.insert(Name, FCnt);
  with FPack[FPackCnt] do
    begin
    str:=Name;
    obj:=_Value;
    end;
  Inc(FPackCnt);
  Inc(FCnt);

  Result:=FCnt-1;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrObjList.FindCS(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Name);
  end;

function tRtcFastStrObjList.IndexOfCS(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Name);
  end;

function tRtcFastStrObjList.GetName(const index: integer): RtcString;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].str
  else
    Result:=FPack^[index and RTC_STROBJ_AND].str;
  end;

function tRtcFastStrObjList.GetValue(const index: integer): TObject;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].obj
  else
    Result:=FPack^[index and RTC_STROBJ_AND].obj;
  end;

procedure tRtcFastStrObjList.SetName(const index: integer; const _Value: RtcString);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    begin
    with FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(Upper_Case(str));
      str:=_Value;
      Tree.insert(Upper_Case(_Value), index);
      end;
    end
  else
    begin
    with FPack^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(Upper_Case(str));
      str:=_Value;
      Tree.insert(Upper_Case(_Value), index);
      end;
    end;
  if assigned(FOnChange) then FOnChange(self);
  end;

procedure tRtcFastStrObjList.SetValue(const index: integer; const _Value: TObject);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].obj:=_Value
  else
    FPack^[index and RTC_STROBJ_AND].obj:=_Value;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrObjList.GetCount: integer;
  begin
  Result:=FCnt;
  end;

{ tRtcFastStringObjList }

constructor tRtcFastStringObjList.Create;
  begin
  inherited;
  FPack:=nil;
  Tree:=tStringIntList.Create(RTC_STROBJ_PACK);

  SetLength(FData,0);
  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);

  FCnt:=0;
  FDataCnt:=0;
  FPackCnt:=0;
  end;

destructor tRtcFastStringObjList.Destroy;
  begin
  Clear;
  if FPack<>nil then Dispose(FPack);
  RtcFreeAndNil(Tree);
  inherited;
  end;

procedure tRtcFastStringObjList.Clear;
  var
    a,b:integer;
    FPack2:PRtcStringObjArr;
  begin
  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        SetLength(str,0);
        obj:=nil;
        end;
    FPackCnt:=0;
    end;

  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          SetLength(str,0);
          obj:=nil;
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  if assigned(Tree) then
    Tree.removeall;
  FCnt:=0;

  if assigned(FOnChange) then FOnChange(self);
  end;

procedure tRtcFastStringObjList.DestroyObjects;
  var
    a,b,c:integer;
    FPack2:PRtcStringObjArr;
  begin
  if FPackCnt>0 then
    begin
    c:=FPackCnt;
    FPackCnt:=0;
    for b:=0 to c-1 do
      with FPack^[b] do
        begin
        SetLength(str,0);
        obj.Free;
        end;
    end;

  if FDataCnt>0 then
    begin
    c:=FDataCnt;
    FDataCnt:=0;
    for a:=0 to c-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          SetLength(str,0);
          obj.Free;
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    end;

  if assigned(Tree) then
    Tree.removeall;
  FCnt:=0;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStringObjList.Add(const Name: RtcWideString; _Value:TObject=nil): integer;
  procedure FastStringListGrow;
    begin
    if Length(FData)<=FDataCnt then
      SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
    FData[FDataCnt]:=FPack;
    Inc(FDataCnt);

    New(FPack);
    FillChar(FPack^,SizeOf(FPack^),0);
    FPackCnt:=0;
    end;
  begin
  if FPackCnt>=RTC_STROBJ_PACK then
    FastStringListGrow;

  Tree.insert(Upper_WCase(Name), FCnt);
  with FPack[FPackCnt] do
    begin
    str:=Name;
    obj:=_Value;
    end;
  Inc(FPackCnt);
  Inc(FCnt);

  Result:=FCnt-1;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStringObjList.Find(const Name: RtcWideString): integer;
  begin
  Result:=Tree.search(Upper_WCase(Name));
  end;

function tRtcFastStringObjList.IndexOf(const Name: RtcWideString): integer;
  begin
  Result:=Tree.search(Upper_WCase(Name));
  end;

function tRtcFastStringObjList.AddCS(const Name: RtcWideString; _Value:TObject=nil): integer;
  procedure FastStringListGrow;
    begin
    if Length(FData)<=FDataCnt then
      SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
    FData[FDataCnt]:=FPack;
    Inc(FDataCnt);

    New(FPack);
    FillChar(FPack^,SizeOf(FPack^),0);
    FPackCnt:=0;
    end;
  begin
  if FPackCnt>=RTC_STROBJ_PACK then
    FastStringListGrow;

  Tree.insert(Name, FCnt);
  with FPack[FPackCnt] do
    begin
    str:=Name;
    obj:=_Value;
    end;
  Inc(FPackCnt);
  Inc(FCnt);

  Result:=FCnt-1;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStringObjList.FindCS(const Name: RtcWideString): integer;
  begin
  Result:=Tree.search(Name);
  end;

function tRtcFastStringObjList.IndexOfCS(const Name: RtcWideString): integer;
  begin
  Result:=Tree.search(Name);
  end;

function tRtcFastStringObjList.GetName(const index: integer): RtcWideString;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].str
  else
    Result:=FPack^[index and RTC_STROBJ_AND].str;
  end;

function tRtcFastStringObjList.GetValue(const index: integer): TObject;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].obj
  else
    Result:=FPack^[index and RTC_STROBJ_AND].obj;
  end;

procedure tRtcFastStringObjList.SetName(const index: integer; const _Value: RtcWideString);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    begin
    with FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(Upper_WCase(str));
      str:=_Value;
      Tree.insert(Upper_WCase(_Value), index);
      end;
    end
  else
    begin
    with FPack^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(Upper_WCase(str));
      str:=_Value;
      Tree.insert(Upper_WCase(_Value), index);
      end;
    end;
  if assigned(FOnChange) then FOnChange(self);
  end;

procedure tRtcFastStringObjList.SetValue(const index: integer; const _Value: TObject);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].obj:=_Value
  else
    FPack^[index and RTC_STROBJ_AND].obj:=_Value;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStringObjList.GetCount: integer;
  begin
  Result:=FCnt;
  end;

{ tRtcFastStrValueList }

constructor tRtcFastStrValueList.Create;
  begin
  inherited;
  FPack:=nil;
  Tree:=tStrIntList.Create(RTC_STROBJ_PACK);

  SetLength(FData,0);
  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);

  FCnt:=0;
  FDataCnt:=0;
  FPackCnt:=0;
  end;

destructor tRtcFastStrValueList.Destroy;
  begin
  Clear;
  if FPack<>nil then Dispose(FPack);
  RtcFreeAndNil(Tree);
  inherited;
  end;

procedure tRtcFastStrValueList.Clear;
  var
    a,b:integer;
    FPack2:PRtcStrValArr;
  begin
  if self=nil then Exit;
  
  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        SetLength(str,0);
        SetLength(val,0);
        end;
    FPackCnt:=0;
    end;

  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          SetLength(str,0);
          SetLength(val,0);
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  if assigned(Tree) then
    Tree.removeall;
  FCnt:=0;

  if assigned(FOnChange) then FOnChange(self);
  end;

procedure tRtcFastStrValueList.DestroyObjects;
  var
    a,b,c:integer;
    FPack2:PRtcStrValArr;
  begin
  if self=nil then Exit;

  if FPackCnt>0 then
    begin
    c:=FPackCnt;
    FPackCnt:=0;
    for b:=0 to c-1 do
      with FPack^[b] do
        begin
        SetLength(str,0);
        SetLength(val,0);
        end;
    end;

  if FDataCnt>0 then
    begin
    c:=FDataCnt;
    FDataCnt:=0;
    for a:=0 to c-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          SetLength(str,0);
          SetLength(val,0);
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    end;

  if assigned(Tree) then
    Tree.removeall;
  FCnt:=0;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrValueList.Add(const Name: RtcString; _Value:RtcString=''): integer;
  begin
  if FPackCnt>=RTC_STROBJ_PACK then
    begin
    if Length(FData)<=FDataCnt then
      SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
    FData[FDataCnt]:=FPack;
    Inc(FDataCnt);

    New(FPack);
    FillChar(FPack^,SizeOf(FPack^),0);
    FPackCnt:=0;
    end;

  Tree.insert(Upper_Case(Name), FCnt);
  with FPack[FPackCnt] do
    begin
    str:=Name;
    val:=_Value;
    end;
  Inc(FPackCnt);
  Inc(FCnt);

  Result:=FCnt-1;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrValueList.Find(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Upper_Case(Name));
  end;

function tRtcFastStrValueList.IndexOf(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Upper_Case(Name));
  end;

function tRtcFastStrValueList.AddCS(const Name: RtcString; _Value:RtcString=''): integer;
  begin
  if FPackCnt>=RTC_STROBJ_PACK then
    begin
    if Length(FData)<=FDataCnt then
      SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
    FData[FDataCnt]:=FPack;
    Inc(FDataCnt);

    New(FPack);
    FillChar(FPack^,SizeOf(FPack^),0);
    FPackCnt:=0;
    end;

  Tree.insert(Name, FCnt);
  with FPack[FPackCnt] do
    begin
    str:=Name;
    val:=_Value;
    end;
  Inc(FPackCnt);
  Inc(FCnt);

  Result:=FCnt-1;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrValueList.FindCS(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Name);
  end;

function tRtcFastStrValueList.IndexOfCS(const Name: RtcString): integer;
  begin
  Result:=Tree.search(Name);
  end;

function tRtcFastStrValueList.GetName(const index: integer): RtcString;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].str
  else
    Result:=FPack^[index and RTC_STROBJ_AND].str;
  end;

function tRtcFastStrValueList.GetValue(const index: integer): RtcString;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].val
  else
    Result:=FPack^[index and RTC_STROBJ_AND].val;
  end;

procedure tRtcFastStrValueList.SetName(const index: integer; const _Value: RtcString);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    begin
    with FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(Upper_Case(str));
      str:=_Value;
      Tree.insert(Upper_Case(_Value), index);
      end;
    end
  else
    begin
    with FPack^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(Upper_Case(str));
      str:=_Value;
      Tree.insert(Upper_Case(_Value), index);
      end;
    end;
  if assigned(FOnChange) then FOnChange(self);
  end;

procedure tRtcFastStrValueList.SetValue(const index: integer; const _Value: RtcString);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].val:=_Value
  else
    FPack^[index and RTC_STROBJ_AND].val:=_Value;

  if assigned(FOnChange) then FOnChange(self);
  end;

function tRtcFastStrValueList.GetCount: integer;
  begin
  Result:=FCnt;
  end;

{ TRtcTimeoutsOfAPI }

procedure TRtcTimeoutsOfAPI.AssignTo(Dest: TPersistent);
  begin
  if assigned(Dest) and (Dest<>self) and (Dest is TRtcTimeoutsOfAPI) then
    begin
    TRtcTimeoutsOfAPI(Dest).ResolveTimeout:=ResolveTimeout;
    TRtcTimeoutsOfAPI(Dest).ConnectTimeout:=ConnectTimeout;
    TRtcTimeoutsOfAPI(Dest).SendTimeout:=SendTimeout;
    TRtcTimeoutsOfAPI(Dest).ReceiveTimeout:=ReceiveTimeout;
    TRtcTimeoutsOfAPI(Dest).ResponseTimeout:=ResponseTimeout;
    end;
  end;

(* Huffman Code from "RFC 7541" - Appendix B.
                                                        code
                          code as bits                 as hex   len
        sym              aligned to MSB                aligned   in
                                                       to LSB   bits

       (  0)  |11111111|11000                             1ff8  [13]
       (  1)  |11111111|11111111|1011000                7fffd8  [23]
       (  2)  |11111111|11111111|11111110|0010         fffffe2  [28]
       (  3)  |11111111|11111111|11111110|0011         fffffe3  [28]
       (  4)  |11111111|11111111|11111110|0100         fffffe4  [28]
       (  5)  |11111111|11111111|11111110|0101         fffffe5  [28]
       (  6)  |11111111|11111111|11111110|0110         fffffe6  [28]
       (  7)  |11111111|11111111|11111110|0111         fffffe7  [28]
       (  8)  |11111111|11111111|11111110|1000         fffffe8  [28]
       (  9)  |11111111|11111111|11101010               ffffea  [24]
       ( 10)  |11111111|11111111|11111111|111100      3ffffffc  [30]
       ( 11)  |11111111|11111111|11111110|1001         fffffe9  [28]
       ( 12)  |11111111|11111111|11111110|1010         fffffea  [28]
       ( 13)  |11111111|11111111|11111111|111101      3ffffffd  [30]
       ( 14)  |11111111|11111111|11111110|1011         fffffeb  [28]
       ( 15)  |11111111|11111111|11111110|1100         fffffec  [28]
       ( 16)  |11111111|11111111|11111110|1101         fffffed  [28]
       ( 17)  |11111111|11111111|11111110|1110         fffffee  [28]
       ( 18)  |11111111|11111111|11111110|1111         fffffef  [28]
       ( 19)  |11111111|11111111|11111111|0000         ffffff0  [28]
       ( 20)  |11111111|11111111|11111111|0001         ffffff1  [28]
       ( 21)  |11111111|11111111|11111111|0010         ffffff2  [28]
       ( 22)  |11111111|11111111|11111111|111110      3ffffffe  [30]
       ( 23)  |11111111|11111111|11111111|0011         ffffff3  [28]
       ( 24)  |11111111|11111111|11111111|0100         ffffff4  [28]
       ( 25)  |11111111|11111111|11111111|0101         ffffff5  [28]
       ( 26)  |11111111|11111111|11111111|0110         ffffff6  [28]
       ( 27)  |11111111|11111111|11111111|0111         ffffff7  [28]
       ( 28)  |11111111|11111111|11111111|1000         ffffff8  [28]
       ( 29)  |11111111|11111111|11111111|1001         ffffff9  [28]
       ( 30)  |11111111|11111111|11111111|1010         ffffffa  [28]
       ( 31)  |11111111|11111111|11111111|1011         ffffffb  [28]
   ' ' ( 32)  |010100                                       14  [ 6]
   '!' ( 33)  |11111110|00                                 3f8  [10]
   '"' ( 34)  |11111110|01                                 3f9  [10]
   '#' ( 35)  |11111111|1010                               ffa  [12]
   '$' ( 36)  |11111111|11001                             1ff9  [13]
   '%' ( 37)  |010101                                       15  [ 6]
   '&' ( 38)  |11111000                                     f8  [ 8]
   ''' ( 39)  |11111111|010                                7fa  [11]
   '(' ( 40)  |11111110|10                                 3fa  [10]
   ')' ( 41)  |11111110|11                                 3fb  [10]
   '*' ( 42)  |11111001                                     f9  [ 8]
   '+' ( 43)  |11111111|011                                7fb  [11]
   ',' ( 44)  |11111010                                     fa  [ 8]
   '-' ( 45)  |010110                                       16  [ 6]
   '.' ( 46)  |010111                                       17  [ 6]
   '/' ( 47)  |011000                                       18  [ 6]
   '0' ( 48)  |00000                                         0  [ 5]
   '1' ( 49)  |00001                                         1  [ 5]
   '2' ( 50)  |00010                                         2  [ 5]
   '3' ( 51)  |011001                                       19  [ 6]
   '4' ( 52)  |011010                                       1a  [ 6]
   '5' ( 53)  |011011                                       1b  [ 6]
   '6' ( 54)  |011100                                       1c  [ 6]
   '7' ( 55)  |011101                                       1d  [ 6]
   '8' ( 56)  |011110                                       1e  [ 6]
   '9' ( 57)  |011111                                       1f  [ 6]
   ':' ( 58)  |1011100                                      5c  [ 7]
   ';' ( 59)  |11111011                                     fb  [ 8]
   '<' ( 60)  |11111111|1111100                           7ffc  [15]
   '=' ( 61)  |100000                                       20  [ 6]
   '>' ( 62)  |11111111|1011                               ffb  [12]
   '?' ( 63)  |11111111|00                                 3fc  [10]
   '@' ( 64)  |11111111|11010                             1ffa  [13]
   'A' ( 65)  |100001                                       21  [ 6]
   'B' ( 66)  |1011101                                      5d  [ 7]
   'C' ( 67)  |1011110                                      5e  [ 7]
   'D' ( 68)  |1011111                                      5f  [ 7]
   'E' ( 69)  |1100000                                      60  [ 7]
   'F' ( 70)  |1100001                                      61  [ 7]
   'G' ( 71)  |1100010                                      62  [ 7]
   'H' ( 72)  |1100011                                      63  [ 7]
   'I' ( 73)  |1100100                                      64  [ 7]
   'J' ( 74)  |1100101                                      65  [ 7]
   'K' ( 75)  |1100110                                      66  [ 7]
   'L' ( 76)  |1100111                                      67  [ 7]
   'M' ( 77)  |1101000                                      68  [ 7]
   'N' ( 78)  |1101001                                      69  [ 7]
   'O' ( 79)  |1101010                                      6a  [ 7]
   'P' ( 80)  |1101011                                      6b  [ 7]
   'Q' ( 81)  |1101100                                      6c  [ 7]
   'R' ( 82)  |1101101                                      6d  [ 7]
   'S' ( 83)  |1101110                                      6e  [ 7]
   'T' ( 84)  |1101111                                      6f  [ 7]
   'U' ( 85)  |1110000                                      70  [ 7]
   'V' ( 86)  |1110001                                      71  [ 7]
   'W' ( 87)  |1110010                                      72  [ 7]
   'X' ( 88)  |11111100                                     fc  [ 8]
   'Y' ( 89)  |1110011                                      73  [ 7]
   'Z' ( 90)  |11111101                                     fd  [ 8]
   '[' ( 91)  |11111111|11011                             1ffb  [13]
   '\' ( 92)  |11111111|11111110|000                     7fff0  [19]
   ']' ( 93)  |11111111|11100                             1ffc  [13]
   '^' ( 94)  |11111111|111100                            3ffc  [14]
   '_' ( 95)  |100010                                       22  [ 6]
   '`' ( 96)  |11111111|1111101                           7ffd  [15]
   'a' ( 97)  |00011                                         3  [ 5]
   'b' ( 98)  |100011                                       23  [ 6]
   'c' ( 99)  |00100                                         4  [ 5]
   'd' (100)  |100100                                       24  [ 6]
   'e' (101)  |00101                                         5  [ 5]
   'f' (102)  |100101                                       25  [ 6]
   'g' (103)  |100110                                       26  [ 6]
   'h' (104)  |100111                                       27  [ 6]
   'i' (105)  |00110                                         6  [ 5]
   'j' (106)  |1110100                                      74  [ 7]
   'k' (107)  |1110101                                      75  [ 7]
   'l' (108)  |101000                                       28  [ 6]
   'm' (109)  |101001                                       29  [ 6]
   'n' (110)  |101010                                       2a  [ 6]
   'o' (111)  |00111                                         7  [ 5]
   'p' (112)  |101011                                       2b  [ 6]
   'q' (113)  |1110110                                      76  [ 7]
   'r' (114)  |101100                                       2c  [ 6]
   's' (115)  |01000                                         8  [ 5]
   't' (116)  |01001                                         9  [ 5]
   'u' (117)  |101101                                       2d  [ 6]
   'v' (118)  |1110111                                      77  [ 7]
   'w' (119)  |1111000                                      78  [ 7]
   'x' (120)  |1111001                                      79  [ 7]
   'y' (121)  |1111010                                      7a  [ 7]
   'z' (122)  |1111011                                      7b  [ 7]
   '{' (123)  |11111111|1111110                           7ffe  [15]
   '|' (124)  |11111111|100                                7fc  [11]
   '}' (125)  |11111111|111101                            3ffd  [14]
   '~' (126)  |11111111|11101                             1ffd  [13]
       (127)  |11111111|11111111|11111111|1100         ffffffc  [28]
       (128)  |11111111|11111110|0110                    fffe6  [20]
       (129)  |11111111|11111111|010010                 3fffd2  [22]
       (130)  |11111111|11111110|0111                    fffe7  [20]
       (131)  |11111111|11111110|1000                    fffe8  [20]
       (132)  |11111111|11111111|010011                 3fffd3  [22]
       (133)  |11111111|11111111|010100                 3fffd4  [22]
       (134)  |11111111|11111111|010101                 3fffd5  [22]
       (135)  |11111111|11111111|1011001                7fffd9  [23]
       (136)  |11111111|11111111|010110                 3fffd6  [22]
       (137)  |11111111|11111111|1011010                7fffda  [23]
       (138)  |11111111|11111111|1011011                7fffdb  [23]
       (139)  |11111111|11111111|1011100                7fffdc  [23]
       (140)  |11111111|11111111|1011101                7fffdd  [23]
       (141)  |11111111|11111111|1011110                7fffde  [23]
       (142)  |11111111|11111111|11101011               ffffeb  [24]
       (143)  |11111111|11111111|1011111                7fffdf  [23]
       (144)  |11111111|11111111|11101100               ffffec  [24]
       (145)  |11111111|11111111|11101101               ffffed  [24]
       (146)  |11111111|11111111|010111                 3fffd7  [22]
       (147)  |11111111|11111111|1100000                7fffe0  [23]
       (148)  |11111111|11111111|11101110               ffffee  [24]
       (149)  |11111111|11111111|1100001                7fffe1  [23]
       (150)  |11111111|11111111|1100010                7fffe2  [23]
       (151)  |11111111|11111111|1100011                7fffe3  [23]
       (152)  |11111111|11111111|1100100                7fffe4  [23]
       (153)  |11111111|11111110|11100                  1fffdc  [21]
       (154)  |11111111|11111111|011000                 3fffd8  [22]
       (155)  |11111111|11111111|1100101                7fffe5  [23]
       (156)  |11111111|11111111|011001                 3fffd9  [22]
       (157)  |11111111|11111111|1100110                7fffe6  [23]
       (158)  |11111111|11111111|1100111                7fffe7  [23]
       (159)  |11111111|11111111|11101111               ffffef  [24]
       (160)  |11111111|11111111|011010                 3fffda  [22]
       (161)  |11111111|11111110|11101                  1fffdd  [21]
       (162)  |11111111|11111110|1001                    fffe9  [20]
       (163)  |11111111|11111111|011011                 3fffdb  [22]
       (164)  |11111111|11111111|011100                 3fffdc  [22]
       (165)  |11111111|11111111|1101000                7fffe8  [23]
       (166)  |11111111|11111111|1101001                7fffe9  [23]
       (167)  |11111111|11111110|11110                  1fffde  [21]
       (168)  |11111111|11111111|1101010                7fffea  [23]
       (169)  |11111111|11111111|011101                 3fffdd  [22]
       (170)  |11111111|11111111|011110                 3fffde  [22]
       (171)  |11111111|11111111|11110000               fffff0  [24]
       (172)  |11111111|11111110|11111                  1fffdf  [21]
       (173)  |11111111|11111111|011111                 3fffdf  [22]
       (174)  |11111111|11111111|1101011                7fffeb  [23]
       (175)  |11111111|11111111|1101100                7fffec  [23]
       (176)  |11111111|11111111|00000                  1fffe0  [21]
       (177)  |11111111|11111111|00001                  1fffe1  [21]
       (178)  |11111111|11111111|100000                 3fffe0  [22]
       (179)  |11111111|11111111|00010                  1fffe2  [21]
       (180)  |11111111|11111111|1101101                7fffed  [23]
       (181)  |11111111|11111111|100001                 3fffe1  [22]
       (182)  |11111111|11111111|1101110                7fffee  [23]
       (183)  |11111111|11111111|1101111                7fffef  [23]
       (184)  |11111111|11111110|1010                    fffea  [20]
       (185)  |11111111|11111111|100010                 3fffe2  [22]
       (186)  |11111111|11111111|100011                 3fffe3  [22]
       (187)  |11111111|11111111|100100                 3fffe4  [22]
       (188)  |11111111|11111111|1110000                7ffff0  [23]
       (189)  |11111111|11111111|100101                 3fffe5  [22]
       (190)  |11111111|11111111|100110                 3fffe6  [22]
       (191)  |11111111|11111111|1110001                7ffff1  [23]
       (192)  |11111111|11111111|11111000|00           3ffffe0  [26]
       (193)  |11111111|11111111|11111000|01           3ffffe1  [26]
       (194)  |11111111|11111110|1011                    fffeb  [20]
       (195)  |11111111|11111110|001                     7fff1  [19]
       (196)  |11111111|11111111|100111                 3fffe7  [22]
       (197)  |11111111|11111111|1110010                7ffff2  [23]
       (198)  |11111111|11111111|101000                 3fffe8  [22]
       (199)  |11111111|11111111|11110110|0            1ffffec  [25]
       (200)  |11111111|11111111|11111000|10           3ffffe2  [26]
       (201)  |11111111|11111111|11111000|11           3ffffe3  [26]
       (202)  |11111111|11111111|11111001|00           3ffffe4  [26]
       (203)  |11111111|11111111|11111011|110          7ffffde  [27]
       (204)  |11111111|11111111|11111011|111          7ffffdf  [27]
       (205)  |11111111|11111111|11111001|01           3ffffe5  [26]
       (206)  |11111111|11111111|11110001               fffff1  [24]
       (207)  |11111111|11111111|11110110|1            1ffffed  [25]
       (208)  |11111111|11111110|010                     7fff2  [19]
       (209)  |11111111|11111111|00011                  1fffe3  [21]
       (210)  |11111111|11111111|11111001|10           3ffffe6  [26]
       (211)  |11111111|11111111|11111100|000          7ffffe0  [27]
       (212)  |11111111|11111111|11111100|001          7ffffe1  [27]
       (213)  |11111111|11111111|11111001|11           3ffffe7  [26]
       (214)  |11111111|11111111|11111100|010          7ffffe2  [27]
       (215)  |11111111|11111111|11110010               fffff2  [24]
       (216)  |11111111|11111111|00100                  1fffe4  [21]
       (217)  |11111111|11111111|00101                  1fffe5  [21]
       (218)  |11111111|11111111|11111010|00           3ffffe8  [26]
       (219)  |11111111|11111111|11111010|01           3ffffe9  [26]
       (220)  |11111111|11111111|11111111|1101         ffffffd  [28]
       (221)  |11111111|11111111|11111100|011          7ffffe3  [27]
       (222)  |11111111|11111111|11111100|100          7ffffe4  [27]
       (223)  |11111111|11111111|11111100|101          7ffffe5  [27]
       (224)  |11111111|11111110|1100                    fffec  [20]
       (225)  |11111111|11111111|11110011               fffff3  [24]
       (226)  |11111111|11111110|1101                    fffed  [20]
       (227)  |11111111|11111111|00110                  1fffe6  [21]
       (228)  |11111111|11111111|101001                 3fffe9  [22]
       (229)  |11111111|11111111|00111                  1fffe7  [21]
       (230)  |11111111|11111111|01000                  1fffe8  [21]
       (231)  |11111111|11111111|1110011                7ffff3  [23]
       (232)  |11111111|11111111|101010                 3fffea  [22]
       (233)  |11111111|11111111|101011                 3fffeb  [22]
       (234)  |11111111|11111111|11110111|0            1ffffee  [25]
       (235)  |11111111|11111111|11110111|1            1ffffef  [25]
       (236)  |11111111|11111111|11110100               fffff4  [24]
       (237)  |11111111|11111111|11110101               fffff5  [24]
       (238)  |11111111|11111111|11111010|10           3ffffea  [26]
       (239)  |11111111|11111111|1110100                7ffff4  [23]
       (240)  |11111111|11111111|11111010|11           3ffffeb  [26]
       (241)  |11111111|11111111|11111100|110          7ffffe6  [27]
       (242)  |11111111|11111111|11111011|00           3ffffec  [26]
       (243)  |11111111|11111111|11111011|01           3ffffed  [26]
       (244)  |11111111|11111111|11111100|111          7ffffe7  [27]
       (245)  |11111111|11111111|11111101|000          7ffffe8  [27]
       (246)  |11111111|11111111|11111101|001          7ffffe9  [27]
       (247)  |11111111|11111111|11111101|010          7ffffea  [27]
       (248)  |11111111|11111111|11111101|011          7ffffeb  [27]
       (249)  |11111111|11111111|11111111|1110         ffffffe  [28]
       (250)  |11111111|11111111|11111101|100          7ffffec  [27]
       (251)  |11111111|11111111|11111101|101          7ffffed  [27]
       (252)  |11111111|11111111|11111101|110          7ffffee  [27]
       (253)  |11111111|11111111|11111101|111          7ffffef  [27]
       (254)  |11111111|11111111|11111110|000          7fffff0  [27]
       (255)  |11111111|11111111|11111011|10           3ffffee  [26]
   EOS (256)  |11111111|11111111|11111111|111111      3fffffff  [30]
*)

const
  Huff_EOS = 256;

  Huff_Code:array[0..Huff_EOS] of Cardinal =
   {0}    (    $1ff8,   $7fffd8,  $fffffe2,  $fffffe3,  $fffffe4,  $fffffe5,  $fffffe6,  $fffffe7,  $fffffe8,   $ffffea,
   {10}    $3ffffffc,  $fffffe9,  $fffffea, $3ffffffd,  $fffffeb,  $fffffec,  $fffffed,  $fffffee,  $fffffef,  $ffffff0,
   {20}     $ffffff1,  $ffffff2, $3ffffffe,  $ffffff3,  $ffffff4,  $ffffff5,  $ffffff6,  $ffffff7,  $ffffff8,  $ffffff9,
   {30}     $ffffffa,  $ffffffb,       $14,      $3f8,      $3f9,      $ffa,     $1ff9,       $15,       $f8,      $7fa,
   {40}         $3fa,      $3fb,       $f9,      $7fb,       $fa,       $16,       $17,       $18,        $0,        $1,
   {50}           $2,       $19,       $1a,       $1b,       $1c,       $1d,       $1e,       $1f,       $5c,       $fb,
   {60}        $7ffc,       $20,      $ffb,      $3fc,     $1ffa,       $21,       $5d,       $5e,       $5f,       $60,
   {70}          $61,       $62,       $63,       $64,       $65,       $66,       $67,       $68,       $69,       $6a,
   {80}          $6b,       $6c,       $6d,       $6e,       $6f,       $70,       $71,       $72,       $fc,       $73,
   {90}          $fd,     $1ffb,    $7fff0,     $1ffc,     $3ffc,       $22,     $7ffd,        $3,       $23,        $4,
   {100}         $24,        $5,       $25,       $26,       $27,        $6,       $74,       $75,       $28,       $29,
   {110}         $2a,        $7,       $2b,       $76,       $2c,        $8,        $9,       $2d,       $77,       $78,
   {120}         $79,       $7a,       $7b,     $7ffe,      $7fc,     $3ffd,     $1ffd,  $ffffffc,    $fffe6,   $3fffd2,
   {130}      $fffe7,    $fffe8,   $3fffd3,   $3fffd4,   $3fffd5,   $7fffd9,   $3fffd6,   $7fffda,   $7fffdb,   $7fffdc,
   {140}     $7fffdd,   $7fffde,   $ffffeb,   $7fffdf,   $ffffec,   $ffffed,   $3fffd7,   $7fffe0,   $ffffee,   $7fffe1,
   {150}     $7fffe2,   $7fffe3,   $7fffe4,   $1fffdc,   $3fffd8,   $7fffe5,   $3fffd9,   $7fffe6,   $7fffe7,   $ffffef,
   {160}     $3fffda,   $1fffdd,    $fffe9,   $3fffdb,   $3fffdc,   $7fffe8,   $7fffe9,   $1fffde,   $7fffea,   $3fffdd,
   {170}     $3fffde,   $fffff0,   $1fffdf,   $3fffdf,   $7fffeb,   $7fffec,   $1fffe0,   $1fffe1,   $3fffe0,   $1fffe2,
   {180}     $7fffed,   $3fffe1,   $7fffee,   $7fffef,    $fffea,   $3fffe2,   $3fffe3,   $3fffe4,   $7ffff0,   $3fffe5,
   {190}     $3fffe6,   $7ffff1,  $3ffffe0,  $3ffffe1,    $fffeb,    $7fff1,   $3fffe7,   $7ffff2,   $3fffe8,  $1ffffec,
   {200}    $3ffffe2,  $3ffffe3,  $3ffffe4,  $7ffffde,  $7ffffdf,  $3ffffe5,   $fffff1,  $1ffffed,    $7fff2,   $1fffe3,
   {210}    $3ffffe6,  $7ffffe0,  $7ffffe1,  $3ffffe7,  $7ffffe2,   $fffff2,   $1fffe4,   $1fffe5,  $3ffffe8,  $3ffffe9,
   {220}    $ffffffd,  $7ffffe3,  $7ffffe4,  $7ffffe5,    $fffec,   $fffff3,    $fffed,   $1fffe6,   $3fffe9,   $1fffe7,
   {230}     $1fffe8,   $7ffff3,   $3fffea,   $3fffeb,  $1ffffee,  $1ffffef,   $fffff4,   $fffff5,  $3ffffea,   $7ffff4,
   {240}    $3ffffeb,  $7ffffe6,  $3ffffec,  $3ffffed,  $7ffffe7,  $7ffffe8,  $7ffffe9,  $7ffffea,  $7ffffeb,  $ffffffe,
   {250}    $7ffffec,  $7ffffed,  $7ffffee,  $7ffffef,  $7fffff0,  $3ffffee, $3fffffff);
   
  Huff_Len:array[0..Huff_EOS] of Byte =
   {0}     ( 13,   23,   28,   28,   28,   28,   28,   28,   28,   24,
   {10}      30,   28,   28,   30,   28,   28,   28,   28,   28,   28,
   {20}      28,   28,   30,   28,   28,   28,   28,   28,   28,   28,
   {30}      28,   28,    6,   10,   10,   12,   13,    6,    8,   11,
   {40}      10,   10,    8,   11,    8,    6,    6,    6,    5,    5,
   {50}       5,    6,    6,    6,    6,    6,    6,    6,    7,    8,
   {60}      15,    6,   12,   10,   13,    6,    7,    7,    7,    7,
   {70}       7,    7,    7,    7,    7,    7,    7,    7,    7,    7,
   {80}       7,    7,    7,    7,    7,    7,    7,    7,    8,    7,
   {90}       8,   13,   19,   13,   14,    6,   15,    5,    6,    5,
   {100}      6,    5,    6,    6,    6,    5,    7,    7,    6,    6,
   {110}      6,    5,    6,    7,    6,    5,    5,    6,    7,    7,
   {120}      7,    7,    7,   15,   11,   14,   13,   28,   20,   22,
   {130}     20,   20,   22,   22,   22,   23,   22,   23,   23,   23,
   {140}     23,   23,   24,   23,   24,   24,   22,   23,   24,   23,
   {150}     23,   23,   23,   21,   22,   23,   22,   23,   23,   24,
   {160}     22,   21,   20,   22,   22,   23,   23,   21,   23,   22,
   {170}     22,   24,   21,   22,   23,   23,   21,   21,   22,   21,
   {180}     23,   22,   23,   23,   20,   22,   22,   22,   23,   22,
   {190}     22,   23,   26,   26,   20,   19,   22,   23,   22,   25,
   {200}     26,   26,   26,   27,   27,   26,   24,   25,   19,   21,
   {210}     26,   27,   27,   26,   27,   24,   21,   21,   26,   26,
   {220}     28,   27,   27,   27,   20,   24,   20,   21,   22,   21,
   {230}     21,   23,   22,   22,   25,   25,   24,   24,   26,   23,
   {240}     26,   27,   26,   26,   27,   27,   27,   27,   27,   28,
   {250}     27,   27,   27,   27,   27,   26,   30);

  Bit_Mask:array[0..48] of int64 =
     ( 0,
       $1, $3, $7, $f,
       $1f, $3f, $7f, $ff,
       $1ff, $3ff, $7ff, $fff,
       $1fff, $3fff, $7fff, $ffff,
       $1ffff, $3ffff, $7ffff, $fffff,
       $1fffff, $3fffff, $7fffff, $ffffff,
       $1ffffff, $3ffffff, $7ffffff, $fffffff,
       $1fffffff, $3fffffff, $7fffffff, $ffffffff,
       $1ffffffff, $3ffffffff, $7ffffffff, $fffffffff,
       $1fffffffff, $3fffffffff, $7fffffffff, $ffffffffff,
       $1ffffffffff, $3ffffffffff, $7ffffffffff, $fffffffffff,
       $1fffffffffff, $3fffffffffff, $7fffffffffff, $ffffffffffff);

function Huff_CheckLength(const Source:RtcString):Integer;
  var
    idx:integer;
    Chr:Byte;
  begin
  Result:=7;
  for idx:=1 to Length(Source) do
    begin
    Chr:=Ord(Source[idx]);
    Inc(Result,Huff_Len[Chr]);
    end;
  Result:=Result shr 3;
  end;

function Huff_EncodeEx(const Source:RtcString):RtcByteArray;
  var
    a:integer;
    hbits:byte;
    hdata:Cardinal;
    Chr:Byte;
    hshift:byte;
    outlen:integer;
  begin
  SetLength(Result,Huff_CheckLength(Source));
  if Length(Result)=0 then Exit;

  hbits:=0; hdata:=0; outlen:=0;
  for a:=1 to Length(Source) do
    begin
    Chr:=Ord(Source[a]);
    hshift:=Huff_Len[Chr];
    { Codes with more than 24 bits could "push out" the
      remaining bits from our 32-bit buffer. To avoid this,
      Codes with 25 and more bits are processed in 2 steps. }
    if hshift>24 then
      begin
      Dec(hshift,16);
      hdata:=hdata shl hshift;
      Inc(hdata,Huff_Code[Chr] shr 16);
      Inc(hbits,hshift);
      while hbits>=8 do
        begin
        Dec(hbits,8);
        Result[outlen]:=hdata shr hbits;
        hdata:=hdata and Bit_Mask[hbits];
        Inc(outlen);
        end;
      hshift:=16;
      hdata:=hdata shl hshift;
      Inc(hdata,Huff_Code[Chr] and $ffff);
      Inc(hbits,hshift);
      end
    else
      begin
      hdata:=hdata shl hshift;
      Inc(hdata,Huff_Code[Chr]);
      Inc(hbits,hshift);
      end;
    while hbits>=8 do
      begin
      Dec(hbits,8);
      Result[outlen]:=hdata shr hbits;
      hdata:=hdata and Bit_Mask[hbits];
      Inc(outlen);
      end;
    end;

  if hbits>0 then // last bits (if any) + EOS padding
    Result[outlen]:=hdata shl (8-hbits) or ($FF shr hbits);
  end;

function Huff_Encode(const Source:RtcString):RtcString;
  begin
  Result:=RtcBytesToString(Huff_EncodeEx(Source));
  end;

function Huff_DecodeEx(const Data:RtcByteArray; From:Integer=0; Len:Integer=-1):RtcString;
  var
    idx,lastidx,
    outlen:integer;

    hdata, h_data,
    hibit, hi_bit:Cardinal;

    Bit1, Bit2, Bit3, Bit4, Bit5, Bit6, Bit7, Bit8, Bit9, Bit10,
    Bit11, Bit12, Bit13, Bit14, Bit15, Bit16, Bit17, Bit18, Bit19, Bit20,
    Bit21, Bit22, Bit23, Bit24, Bit25, Bit26, Bit27, Bit28, Bit29, Bit30: Cardinal;

  procedure L(len,res:Byte);
    begin
    hibit:=hibit shr len;

    Inc(outlen);
    Result[outlen]:=RtcChar(res);
    end;

  procedure H(len,res:Byte);
    begin
    hibit:=hibit shr (len-15);

    Inc(outlen);
    Result[outlen]:=RtcChar(res);
    end;

  begin
  Result:='';

  if Len=0 then Exit;
  lastidx:=Length(Data);
  idx:=From;
  if (Len>0) and (Len<lastidx-idx) then
    lastidx:=Len+idx;
  if idx<0 then idx:=0;
  if lastidx<=idx then Exit;

  { The highest compression we can expect is 40%,
    because the smallest Huffman Code is 5 bits long }
  SetLength(Result,(lastidx-idx)*2);

  outlen:=0; hdata:=0; hibit:=0;

  while TRUE do // Loop ends with "BREAK" when End-Of-String is found
    begin
    hi_bit:=hibit;
    h_data:=hdata;

    // We need at least 15 bits
    if hi_bit<$4000 then
      begin
      if hi_bit=0 then
        begin
        hi_bit:=$80;
        if idx<lastidx then
          begin
          h_data:=Data[idx];
          Inc(idx);
          end
        else
          h_data:=$FF;
        end;

      // Fill up the buffer ...
      while hi_bit<=$800000 do
        begin
        hi_bit:=hi_bit shl 8;
        h_data:=h_data shl 8;
        if idx<lastidx then
          begin
          Inc(h_data,Data[idx]);
          Inc(idx);
          end
        else
          Inc(h_data,$FF);
        end;

      hibit:=hi_bit;
      hdata:=h_data;
      end;

    Bit1:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit2:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit3:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit4:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit5:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit6:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit1 = 0 then begin //                            |0
      if Bit2 = 0 then begin //                          |00
        if Bit3 = 0 then begin //                        |000
          if Bit4 = 0 then begin //                      |0000
            if Bit5 = 0 then L( 5,48)      // '0' ( 48)  |00000   $0  [ 5]
            else L( 5,49);                 // '1' ( 49)  |00001   $1  [ 5]
          end else //                                    |0001
          if Bit5 = 0 then L( 5,50)        // '2' ( 50)  |00010   $2  [ 5]
          else L( 5,97);                   // 'a' ( 97)  |00011   $3  [ 5]
        end else //                                      |001
        if Bit4 = 0 then begin //                        |0010
          if Bit5 = 0 then L( 5,99)        // 'c' ( 99)  |00100   $4  [ 5]
          else L( 5,101);                  // 'e' (101)  |00101   $5  [ 5]
        end else //                                      |0011
        if Bit5 = 0 then L( 5,105)         // 'i' (105)  |00110   $6  [ 5]
        else L( 5,111);                    // 'o' (111)  |00111   $7  [ 5]
      end else //                                        |01
      if Bit3 = 0 then begin //                          |010
        if Bit4 = 0 then begin //                        |0100
          if Bit5 = 0 then L( 5,115)       // 's' (115)  |01000   $8  [ 5]
          else L( 5,116);                  // 't' (116)  |01001   $9  [ 5]
        end else //                                      |0101
        if Bit5 = 0 then begin //                        |01010
          if Bit6 = 0 then L( 6,32)        // ' ' ( 32)  |010100   $14  [ 6]
          else L( 6,37);                   // '%' ( 37)  |010101   $15  [ 6]
        end else //                                      |01011
        if Bit6 = 0 then L( 6,45)          // '-' ( 45)  |010110   $16  [ 6]
        else L( 6,46);                     // '.' ( 46)  |010111   $17  [ 6]
      end else //                                        |011
      if Bit4 = 0 then begin //                          |0110
        if Bit5 = 0 then begin //                        |01100
          if Bit6 = 0 then L( 6,47)        // '/' ( 47)  |011000   $18  [ 6]
          else L( 6,51);                   // '3' ( 51)  |011001   $19  [ 6]
        end else //                                      |01101
        if Bit6 = 0 then L( 6,52)          // '4' ( 52)  |011010   $1a  [ 6]
        else L( 6,53);                     // '5' ( 53)  |011011   $1b  [ 6]
      end else //                                        |0111
      if Bit5 = 0 then begin //                          |01110
        if Bit6 = 0 then L( 6,54)          // '6' ( 54)  |011100   $1c  [ 6]
        else L( 6,55);                     // '7' ( 55)  |011101   $1d  [ 6]
      end else //                                        |01111
      if Bit6 = 0 then L( 6,56)            // '8' ( 56)  |011110   $1e  [ 6]
      else L( 6,57);                       // '9' ( 57)  |011111   $1f  [ 6]
    end else begin //                                    |1

    Bit7:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit2 = 0 then begin //                            |10
      if Bit3 = 0 then begin //                          |100
        if Bit4 = 0 then begin //                        |1000
          if Bit5 = 0 then begin //                      |10000
            if Bit6 = 0 then L( 6,61)      // '=' ( 61)  |100000   $20  [ 6]
            else L( 6,65);                 // 'A' ( 65)  |100001   $21  [ 6]
          end else //                                    |10001
          if Bit6 = 0 then L( 6,95)        // '_' ( 95)  |100010   $22  [ 6]
          else L( 6,98);                   // 'b' ( 98)  |100011   $23  [ 6]
        end else //                                      |1001
        if Bit5 = 0 then begin //                        |10010
          if Bit6 = 0 then L( 6,100)       // 'd' (100)  |100100   $24  [ 6]
          else L( 6,102)                   // 'f' (102)  |100101   $25  [ 6]
        end else //                                      |10011
        if Bit6 = 0 then L( 6,103)         // 'g' (103)  |100110   $26  [ 6]
        else L( 6,104);                    // 'h' (104)  |100111   $27  [ 6]
      end else //                                        |101
      if Bit4 = 0 then begin //                          |1010
        if Bit5 = 0 then begin //                        |10100
          if Bit6 = 0 then L( 6,108)       // 'l' (108)  |101000   $28  [ 6]
          else L( 6,109);                  // 'm' (109)  |101001   $29  [ 6]
        end else //                                      |10101
        if Bit6 = 0 then L( 6,110)         // 'n' (110)  |101010   $2a  [ 6]
        else L( 6,112);                    // 'p' (112)  |101011   $2b  [ 6]
      end else //                                        |1011
      if Bit5 = 0 then begin //                          |10110
        if Bit6 = 0 then L( 6,114)         // 'r' (114)  |101100   $2c  [ 6]
        else L( 6,117);                    // 'u' (117)  |101101   $2d  [ 6]
      end else //                                        |10111
      if Bit6 = 0 then begin //                          |101110
        if Bit7 = 0 then L( 7,58)          // ':' ( 58)  |1011100   $5c  [ 7]
        else L( 7,66);                     // 'B' ( 66)  |1011101   $5d  [ 7]
      end else //                                        |101111
      if Bit7 = 0 then L( 7,67)            // 'C' ( 67)  |1011110   $5e  [ 7]
      else L( 7,68);                       // 'D' ( 68)  |1011111   $5f  [ 7]
    end else begin //                                    |11

    if Bit3 = 0 then begin //                            |110
      if Bit4 = 0 then begin //                          |1100
        if Bit5 = 0 then begin //                        |11000
          if Bit6 = 0 then begin //                      |110000
            if Bit7 = 0 then L( 7,69)      // 'E' ( 69)  |1100000   $60  [ 7]
            else L( 7,70);                 // 'F' ( 70)  |1100001   $61  [ 7]
          end else //                                    |110001
          if Bit7 = 0 then L( 7,71)        // 'G' ( 71)  |1100010   $62  [ 7]
          else L( 7,72);                   // 'H' ( 72)  |1100011   $63  [ 7]
        end else //                                      |11001
        if Bit6 = 0 then begin //                        |110010
          if Bit7 = 0 then L( 7,73)        // 'I' ( 73)  |1100100   $64  [ 7]
          else L( 7,74);                   // 'J' ( 74)  |1100101   $65  [ 7]
        end else //                                      |110011
        if Bit7 = 0 then L( 7,75)          // 'K' ( 75)  |1100110   $66  [ 7]
        else L( 7,76);                     // 'L' ( 76)  |1100111   $67  [ 7]
      end else //                                        |1101
      if Bit5 = 0 then begin //                          |11010
        if Bit6 = 0 then begin //                        |110100
          if Bit7 = 0 then L( 7,77)        // 'M' ( 77)  |1101000   $68  [ 7]
          else L( 7,78);                   // 'N' ( 78)  |1101001   $69  [ 7]
        end else //                                      |110101
        if Bit7 = 0 then L( 7,79)          // 'O' ( 79)  |1101010   $6a  [ 7]
        else L( 7,80);                     // 'P' ( 80)  |1101011   $6b  [ 7]
      end else //                                        |11011
      if Bit6 = 0 then begin //                          |110110
        if Bit7 = 0 then L( 7,81)          // 'Q' ( 81)  |1101100   $6c  [ 7]
        else L( 7,82);                     // 'R' ( 82)  |1101101   $6d  [ 7]
      end else //                                        |110111
      if Bit7 = 0 then L( 7,83)            // 'S' ( 83)  |1101110   $6e  [ 7]
      else L( 7,84);                       // 'T' ( 84)  |1101111   $6f  [ 7]
    end else begin //                                    |111

    if Bit4 = 0 then begin //                            |1110
      if Bit5 = 0 then begin //                          |11100
        if Bit6 = 0 then begin //                        |111000
          if Bit7 = 0 then L( 7,85)        // 'U' ( 85)  |1110000   $70  [ 7]
          else L( 7,86);                   // 'V' ( 86)  |1110001   $71  [ 7]
        end else //                                      |111001
        if Bit7 = 0 then L( 7,87)          // 'W' ( 87)  |1110010   $72  [ 7]
        else L( 7,89);                     // 'Y' ( 89)  |1110011   $73  [ 7]
      end else //                                        |11101
      if Bit6 = 0 then begin //                          |111010
        if Bit7 = 0 then L( 7,106)         // 'j' (106)  |1110100   $74  [ 7]
        else L( 7,107);                    // 'k' (107)  |1110101   $75  [ 7]
      end else //                                        |111011
      if Bit7 = 0 then L( 7,113)           // 'q' (113)  |1110110   $76  [ 7]
      else L( 7,118);                      // 'v' (118)  |1110111   $77  [ 7]
    end else begin //                                    |1111

    if Bit5 = 0 then begin //                            |11110
      if Bit6 = 0 then begin //                          |111100
        if Bit7 = 0 then L( 7,119)         // 'w' (119)  |1111000   $78  [ 7]
        else L( 7,120);                    // 'x' (120)  |1111001   $79  [ 7]
      end else //                                        |111101
      if Bit7 = 0 then L( 7,121)           // 'y' (121)  |1111010   $7a  [ 7]
      else L( 7,122);                      // 'z' (122)  |1111011   $7b  [ 7]
    end else begin //                                    |11111

    Bit8:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit6 = 0 then begin //                            |111110
      if Bit7 = 0 then begin //                          |1111100
        if Bit8 = 0 then L( 8,38)          // '&' ( 38)  |11111000   $f8  [ 8]
        else L( 8,42);                     // '*' ( 42)  |11111001   $f9  [ 8]
      end else //                                        |1111101
      if Bit8 = 0 then L( 8,44)            // ',' ( 44)  |11111010   $fa  [ 8]
      else L( 8,59);                       // ';' ( 59)  |11111011   $fb  [ 8]
    end else begin //                                    |111111

    if Bit7 = 0 then begin //                            |1111110
      if Bit8 = 0 then L( 8,88)            // 'X' ( 88)  |11111100   $fc  [ 8]
      else L( 8,90);                       // 'Z' ( 90)  |11111101   $fd  [ 8]
    end else begin //                                    |1111111

    Bit9:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit10:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit8 = 0 then begin //                            |11111110|
      if Bit9 = 0 then begin //                          |11111110|0
        if Bit10 = 0 then L(10,33)         // '!' ( 33)  |11111110|00   $3f8  [10]
        else L(10,34);                     // '"' ( 34)  |11111110|01   $3f9  [10]
      end else //                                        |11111110|1
      if Bit10 = 0 then L(10,40)           // '(' ( 40)  |11111110|10   $3fa  [10]
      else L(10,41);                       // ')' ( 41)  |11111110|11   $3fb  [10]
    end else begin //                                    |11111111|

    Bit11:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit9 = 0 then begin //                            |11111111|0
      if Bit10 = 0 then L(10,63)           // '?' ( 63)  |11111111|00   $3fc  [10]
      else //                                            |11111111|01
      if Bit11 = 0 then L(11,39)           // ''' ( 39)  |11111111|010   $7fa  [11]
      else L(11,43);                       // '+' ( 43)  |11111111|011   $7fb  [11]
    end else begin //                                    |11111111|1

    Bit12:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit10 = 0 then begin //                           |11111111|10
      if Bit11 = 0 then L(11,124)          // '|' (124)  |11111111|100   $7fc  [11]
      else //                                            |11111111|101
      if Bit12 = 0 then L(12,35)           // '#' ( 35)  |11111111|1010   $ffa  [12]
      else L(12,62);                       // '>' ( 62)  |11111111|1011   $ffb  [12]
    end else begin //                                    |11111111|11

    Bit13:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit11 = 0 then begin //                           |11111111|110
      if Bit12 = 0 then begin //                         |11111111|1100
        if Bit13 = 0 then L(13,0)          //     (  0)  |11111111|11000   $1ff8  [13]
        else L(13,36);                     // '$' ( 36)  |11111111|11001   $1ff9  [13]
      end else //                                        |11111111|1101
      if Bit13 = 0 then L(13,64)           // '@' ( 64)  |11111111|11010   $1ffa  [13]
      else L(13,91);                       // '[' ( 91)  |11111111|11011   $1ffb  [13]
    end else begin //                                    |11111111|111

    if Bit12 = 0 then begin //                           |11111111|1110
      if Bit13 = 0 then L(13,93)           // ']' ( 93)  |11111111|11100   $1ffc  [13]
      else L(13,126);                      // '~' (126)  |11111111|11101   $1ffd  [13]
    end else begin //                                    |11111111|1111

    Bit14:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit13 = 0 then begin //                           |11111111|11110
      if Bit14 = 0 then L(14,94)           // '^' ( 94)  |11111111|111100   $3ffc  [14]
      else L(14,125);                      // '}' (125)  |11111111|111101   $3ffd  [14]
    end else begin //                                    |11111111|11111

    Bit15:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit14 = 0 then begin //                           |11111111|111110
      if Bit15 = 0 then L(15,60)           // '<' ( 60)  |11111111|1111100   $7ffc  [15]
      else L(15,96);                       // '`' ( 96)  |11111111|1111101   $7ffd  [15]
    end else begin //                                    |11111111|111111

    if Bit15 = 0 then L(15,123)            // '{' (123)  |11111111|1111110   $7ffe  [15]
    else begin //                                        |11111111|1111111

    // We need at least 15 more bits
    if hi_bit<$4000 then
      begin
      if hi_bit=0 then
        begin
        hi_bit:=$80;
        if idx<lastidx then
          begin
          h_data:=Data[idx];
          Inc(idx);
          end
        else
          h_data:=$FF;
        end;

      // Fill up the buffer ...
      while hi_bit<=$800000 do
        begin
        hi_bit:=hi_bit shl 8;
        h_data:=h_data shl 8;
        if idx<lastidx then
          begin
          Inc(h_data,Data[idx]);
          Inc(idx);
          end
        else
          Inc(h_data,$FF);
        end;

      hdata:=h_data;
      end;

    hibit:=hi_bit;

    Bit16:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit17:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit18:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit19:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit20:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit21:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit16 = 0 then begin //                           |11111111|11111110|
      if Bit17 = 0 then begin //                         |11111111|11111110|0
        if Bit18 = 0 then begin //                       |11111111|11111110|00
          if Bit19 = 0 then H(19,92)       // '\' ( 92)  |11111111|11111110|000   $7fff0  [19]
          else H(19,195);                  //     (195)  |11111111|11111110|001   $7fff1  [19]
        end else //                                      |11111111|11111110|01
        if Bit19 = 0 then H(19,208)        //     (208)  |11111111|11111110|010   $7fff2  [19]
        else //                                          |11111111|11111110|011
        if Bit20 = 0 then H(20,128)        //     (128)  |11111111|11111110|0110   $fffe6  [20]
        else H(20,130);                    //     (130)  |11111111|11111110|0111   $fffe7  [20]
      end else //                                        |11111111|11111110|1
      if Bit18 = 0 then begin //                         |11111111|11111110|10
        if Bit19 = 0 then begin //                       |11111111|11111110|100
          if Bit20 = 0 then H(20,131)      //     (131)  |11111111|11111110|1000   $fffe8  [20]
          else H(20,162);                  //     (162)  |11111111|11111110|1001   $fffe9  [20]
        end else //                                      |11111111|11111110|101
        if Bit20 = 0 then H(20,184)        //     (184)  |11111111|11111110|1010   $fffea  [20]
        else H(20,194);                    //     (194)  |11111111|11111110|1011   $fffeb  [20]
      end else //                                        |11111111|11111110|11
      if Bit19 = 0 then begin //                         |11111111|11111110|110
        if Bit20 = 0 then H(20,224)        //     (224)  |11111111|11111110|1100   $fffec  [20]
        else H(20,226);                    //     (226)  |11111111|11111110|1101   $fffed  [20]
      end else //                                        |11111111|11111110|111
      if Bit20 = 0 then begin //                         |11111111|11111110|1110
        if Bit21 = 0 then H(21,153)        //     (153)  |11111111|11111110|11100   $1fffdc  [21]
        else H(21,161);                    //     (161)  |11111111|11111110|11101   $1fffdd  [21]
      end else //                                        |11111111|11111110|1111
      if Bit21 = 0 then H(21,167)          //     (167)  |11111111|11111110|11110   $1fffde  [21]
      else H(21,172);                      //     (172)  |11111111|11111110|11111   $1fffdf  [21]
    end else begin //                                    |11111111|11111111|

    Bit22:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit17 = 0 then begin //                           |11111111|11111111|0
      if Bit18 = 0 then begin //                         |11111111|11111111|00
        if Bit19 = 0 then begin //                       |11111111|11111111|000
          if Bit20 = 0 then begin //                     |11111111|11111111|0000
            if Bit21 = 0 then H(21,176)    //     (176)  |11111111|11111111|00000   $1fffe0  [21]
            else H(21,177);                //     (177)  |11111111|11111111|00001   $1fffe1  [21]
          end else //                                    |11111111|11111111|0001
          if Bit21 = 0 then H(21,179)      //     (179)  |11111111|11111111|00010   $1fffe2  [21]
          else H(21,209);                  //     (209)  |11111111|11111111|00011   $1fffe3  [21]
        end else //                                      |11111111|11111111|001
        if Bit20 = 0 then begin //                       |11111111|11111111|0010
          if Bit21 = 0 then H(21,216)      //     (216)  |11111111|11111111|00100   $1fffe4  [21]
          else H(21,217);                  //     (217)  |11111111|11111111|00101   $1fffe5  [21]
        end else //                                      |11111111|11111111|0011
        if Bit21 = 0 then H(21,227)        //     (227)  |11111111|11111111|00110   $1fffe6  [21]
        else H(21,229);                    //     (229)  |11111111|11111111|00111   $1fffe7  [21]
      end else //                                        |11111111|11111111|01
      if Bit19 = 0 then begin //                         |11111111|11111111|010
        if Bit20 = 0 then begin //                       |11111111|11111111|0100
          if Bit21 = 0 then H(21,230)      //     (230)  |11111111|11111111|01000   $1fffe8  [21]
          else //                                        |11111111|11111111|01001
          if Bit22 = 0 then H(22,129)      //     (129)  |11111111|11111111|010010  $3fffd2  [22]
          else H(22,132);                  //     (132)  |11111111|11111111|010011  $3fffd3  [22]
        end else //                                      |11111111|11111111|0101
        if Bit21 = 0 then begin //                       |11111111|11111111|01010
          if Bit22 = 0 then H(22,133)      //     (133)  |11111111|11111111|010100   $3fffd4  [22]
          else H(22,134);                  //     (134)  |11111111|11111111|010101   $3fffd5  [22]
        end else //                                      |11111111|11111111|01011
        if Bit22 = 0 then H(22,136)        //     (136)  |11111111|11111111|010110   $3fffd6  [22]
        else H(22,146);                    //     (146)  |11111111|11111111|010111   $3fffd7  [22]
      end else //                                        |11111111|11111111|011
      if Bit20 = 0 then begin //                         |11111111|11111111|0110
        if Bit21 = 0 then begin //                       |11111111|11111111|01100
          if Bit22 = 0 then H(22,154)      //     (154)  |11111111|11111111|011000   $3fffd8  [22]
          else H(22,156);                  //     (156)  |11111111|11111111|011001   $3fffd9  [22]
        end else //                                      |11111111|11111111|01101
        if Bit22 = 0 then H(22,160)        //     (160)  |11111111|11111111|011010   $3fffda  [22]
        else H(22,163);                    //     (163)  |11111111|11111111|011011   $3fffdb  [22]
      end else //                                        |11111111|11111111|0111
      if Bit21 = 0 then begin //                         |11111111|11111111|01110
        if Bit22 = 0 then H(22,164)        //     (164)  |11111111|11111111|011100   $3fffdc  [22]
        else H(22,169);                    //     (169)  |11111111|11111111|011101   $3fffdd  [22]
      end else //                                        |11111111|11111111|01111
      if Bit22 = 0 then H(22,170)          //     (170)  |11111111|11111111|011110   $3fffde  [22]
      else H(22,173);                      //     (173)  |11111111|11111111|011111   $3fffdf  [22]
    end else begin //                                    |11111111|11111111|1

    Bit23:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit18 = 0 then begin //                           |11111111|11111111|10
      if Bit19 = 0 then begin //                         |11111111|11111111|100
        if Bit20 = 0 then begin //                       |11111111|11111111|1000
          if Bit21 = 0 then begin //                     |11111111|11111111|10000
            if Bit22 = 0 then H(22,178)    //     (178)  |11111111|11111111|100000   $3fffe0  [22]
            else H(22,181);                //     (181)  |11111111|11111111|100001   $3fffe1  [22]
          end else //                                    |11111111|11111111|10001
          if Bit22 = 0 then H(22,185)      //     (185)  |11111111|11111111|100010   $3fffe2  [22]
          else H(22,186);                  //     (186)  |11111111|11111111|100011   $3fffe3  [22]
        end else //                                      |11111111|11111111|1001
        if Bit21 = 0 then begin //                       |11111111|11111111|10010
          if Bit22 = 0 then H(22,187)      //     (187)  |11111111|11111111|100100   $3fffe4  [22]
          else H(22,189);                  //     (189)  |11111111|11111111|100101   $3fffe5  [22]
        end else //                                      |11111111|11111111|10011
        if Bit22 = 0 then H(22,190)        //     (190)  |11111111|11111111|100110   $3fffe6  [22]
        else H(22,196);                    //     (196)  |11111111|11111111|100111   $3fffe7  [22]
      end else //                                        |11111111|11111111|101
      if Bit20 = 0 then begin //                         |11111111|11111111|1010
        if Bit21 = 0 then begin //                       |11111111|11111111|10100
          if Bit22 = 0 then H(22,198)      //     (198)  |11111111|11111111|101000   $3fffe8  [22]
          else H(22,228);                  //     (228)  |11111111|11111111|101001   $3fffe9  [22]
        end else //                                      |11111111|11111111|10101
        if Bit22 = 0 then H(22,232)        //     (232)  |11111111|11111111|101010   $3fffea  [22]
        else H(22,233);                    //     (233)  |11111111|11111111|101011   $3fffeb  [22]
      end else //                                        |11111111|11111111|1011
      if Bit21 = 0 then begin //                         |11111111|11111111|10110
        if Bit22 = 0 then begin //                       |11111111|11111111|101100
          if Bit23 = 0 then H(23,1)        //     (  1)  |11111111|11111111|1011000   $7fffd8  [23]
          else H(23,135);                  //     (135)  |11111111|11111111|1011001   $7fffd9  [23]
        end else //                                      |11111111|11111111|101101
        if Bit23 = 0 then H(23,137)        //     (137)  |11111111|11111111|1011010   $7fffda  [23]
        else H(23,138);                    //     (138)  |11111111|11111111|1011011   $7fffdb  [23]
      end else //                                        |11111111|11111111|10111
      if Bit22 = 0 then begin //                         |11111111|11111111|101110
        if Bit23 = 0 then H(23,139)        //     (139)  |11111111|11111111|1011100   $7fffdc  [23]
        else H(23,140);                    //     (140)  |11111111|11111111|1011101   $7fffdd  [23]
      end else //                                        |11111111|11111111|101111
      if Bit23 = 0 then H(23,141)          //     (141)  |11111111|11111111|1011110   $7fffde  [23]
      else H(23,143);                      //     (143)  |11111111|11111111|1011111   $7fffdf  [23]
    end else begin //                                    |11111111|11111111|11

    if Bit19 = 0 then begin //                           |11111111|11111111|110
      if Bit20 = 0 then begin //                         |11111111|11111111|1100
        if Bit21 = 0 then begin //                       |11111111|11111111|11000
          if Bit22 = 0 then begin //                     |11111111|11111111|110000
            if Bit23 = 0 then H(23,147)    //     (147)  |11111111|11111111|1100000   $7fffe0  [23]
            else H(23,149);                //     (149)  |11111111|11111111|1100001   $7fffe1  [23]
          end else //                                    |11111111|11111111|110001
          if Bit23 = 0 then H(23,150)      //     (150)  |11111111|11111111|1100010   $7fffe2  [23]
          else H(23,151);                  //     (151)  |11111111|11111111|1100011   $7fffe3  [23]
        end else //                                      |11111111|11111111|11001
        if Bit22 = 0 then begin //                       |11111111|11111111|110010
          if Bit23 = 0 then H(23,152)      //     (152)  |11111111|11111111|1100100   $7fffe4  [23]
          else H(23,155);                  //     (155)  |11111111|11111111|1100101   $7fffe5  [23]
        end else //                                      |11111111|11111111|110011
        if Bit23 = 0 then H(23,157)        //     (157)  |11111111|11111111|1100110   $7fffe6  [23]
        else H(23,158);                    //     (158)  |11111111|11111111|1100111   $7fffe7  [23]
      end else //                                        |11111111|11111111|1101
      if Bit21 = 0 then begin //                         |11111111|11111111|11010
        if Bit22 = 0 then begin //                       |11111111|11111111|110100
          if Bit23 = 0 then H(23,165)      //     (165)  |11111111|11111111|1101000   $7fffe8  [23]
          else H(23,166);                  //     (166)  |11111111|11111111|1101001   $7fffe9  [23]
        end else //                                      |11111111|11111111|110101
        if Bit23 = 0 then H(23,168)        //     (168)  |11111111|11111111|1101010   $7fffea  [23]
        else H(23,174);                    //     (174)  |11111111|11111111|1101011   $7fffeb  [23]
      end else //                                        |11111111|11111111|11011
      if Bit22 = 0 then begin //                         |11111111|11111111|110110
        if Bit23 = 0 then H(23,175)        //     (175)  |11111111|11111111|1101100   $7fffec  [23]
        else H(23,180);                    //     (180)  |11111111|11111111|1101101   $7fffed  [23]
      end else //                                        |11111111|11111111|110111
      if Bit23 = 0 then H(23,182)          //     (182)  |11111111|11111111|1101110   $7fffee  [23]
      else H(23,183);                      //     (183)  |11111111|11111111|1101111   $7fffef  [23]
    end else begin //                                    |11111111|11111111|111

    Bit24:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit20 = 0 then begin //                           |11111111|11111111|1110
      if Bit21 = 0 then begin //                         |11111111|11111111|11100
        if Bit22 = 0 then begin //                       |11111111|11111111|111000
          if Bit23 = 0 then H(23,188)      //     (188)  |11111111|11111111|1110000   $7ffff0  [23]
          else H(23,191);                  //     (191)  |11111111|11111111|1110001   $7ffff1  [23]
        end else //                                      |11111111|11111111|111001
        if Bit23 = 0 then H(23,197)        //     (197)  |11111111|11111111|1110010   $7ffff2  [23]
        else H(23,231);                    //     (231)  |11111111|11111111|1110011   $7ffff3  [23]
      end else //                                        |11111111|11111111|11101
      if Bit22 = 0 then begin //                         |11111111|11111111|111010
        if Bit23 = 0 then H(23,239)        //     (239)  |11111111|11111111|1110100   $7ffff4  [23]
        else //                                          |11111111|11111111|1110101
        if Bit24 = 0 then H(24,9)          //     (  9)  |11111111|11111111|11101010  $ffffea  [24]
        else H(24,142);                    //     (142)  |11111111|11111111|11101011  $ffffeb  [24]
      end else //                                        |11111111|11111111|111011
      if Bit23 = 0 then begin //                         |11111111|11111111|1110110
        if Bit24 = 0 then H(24,144)        //     (144)  |11111111|11111111|11101100   $ffffec  [24]
        else H(24,145);                    //     (145)  |11111111|11111111|11101101   $ffffed  [24]
      end else //                                        |11111111|11111111|1110111
      if Bit24 = 0 then H(24,148)          //     (148)  |11111111|11111111|11101110   $ffffee  [24]
      else H(24,159);                      //     (159)  |11111111|11111111|11101111   $ffffef  [24]
    end else begin //                                    |11111111|11111111|1111

    Bit25:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit21 = 0 then begin //                           |11111111|11111111|11110
      if Bit22 = 0 then begin //                         |11111111|11111111|111100
        if Bit23 = 0 then begin //                       |11111111|11111111|1111000
          if Bit24 = 0 then H(24,171)      //     (171)  |11111111|11111111|11110000   $fffff0  [24]
          else H(24,206);                  //     (206)  |11111111|11111111|11110001   $fffff1  [24]
        end else //                                      |11111111|11111111|1111001
        if Bit24 = 0 then H(24,215)        //     (215)  |11111111|11111111|11110010   $fffff2  [24]
        else H(24,225);                    //     (225)  |11111111|11111111|11110011   $fffff3  [24]
      end else //                                        |11111111|11111111|111101
      if Bit23 = 0 then begin //                         |11111111|11111111|1111010
        if Bit24 = 0 then H(24,236)        //     (236)  |11111111|11111111|11110100   $fffff4  [24]
        else H(24,237);                    //     (237)  |11111111|11111111|11110101   $fffff5  [24]
      end else //                                        |11111111|11111111|1111011
      if Bit24 = 0 then begin //                         |11111111|11111111|11110110|
        if Bit25 = 0 then H(25,199)        //     (199)  |11111111|11111111|11110110|0   $1ffffec  [25]
        else H(25,207);                    //     (207)  |11111111|11111111|11110110|1   $1ffffed  [25]
      end else //                                        |11111111|11111111|11110111|
      if Bit25 = 0 then H(25,234)          //     (234)  |11111111|11111111|11110111|0   $1ffffee  [25]
      else H(25,235);                      //     (235)  |11111111|11111111|11110111|1   $1ffffef  [25]
    end else begin //                                    |11111111|11111111|11111

    Bit26:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit27:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit22 = 0 then begin //                           |11111111|11111111|111110
      if Bit23 = 0 then begin //                         |11111111|11111111|1111100
        if Bit24 = 0 then begin //                       |11111111|11111111|11111000|
          if Bit25 = 0 then begin //                     |11111111|11111111|11111000|0
            if Bit26 = 0 then H(26,192)    //     (192)  |11111111|11111111|11111000|00   $3ffffe0  [26]
            else H(26,193);                //     (193)  |11111111|11111111|11111000|01   $3ffffe1  [26]
          end else //                                    |11111111|11111111|11111000|1
          if Bit26 = 0 then H(26,200)      //     (200)  |11111111|11111111|11111000|10   $3ffffe2  [26]
          else H(26,201);                  //     (201)  |11111111|11111111|11111000|11   $3ffffe3  [26]
        end else //                                      |11111111|11111111|11111001|
        if Bit25 = 0 then begin //                       |11111111|11111111|11111001|0
          if Bit26 = 0 then H(26,202)      //     (202)  |11111111|11111111|11111001|00   $3ffffe4  [26]
          else H(26,205);                  //     (205)  |11111111|11111111|11111001|01   $3ffffe5  [26]
        end else //                                      |11111111|11111111|11111001|1
        if Bit26 = 0 then H(26,210)        //     (210)  |11111111|11111111|11111001|10   $3ffffe6  [26]
        else H(26,213);                    //     (213)  |11111111|11111111|11111001|11   $3ffffe7  [26]
      end else //                                        |11111111|11111111|1111101
      if Bit24 = 0 then begin //                         |11111111|11111111|11111010|
        if Bit25 = 0 then begin //                       |11111111|11111111|11111010|0
          if Bit26 = 0 then H(26,218)      //     (218)  |11111111|11111111|11111010|00   $3ffffe8  [26]
          else H(26,219);                  //     (219)  |11111111|11111111|11111010|01   $3ffffe9  [26]
        end else //                                      |11111111|11111111|11111010|1
        if Bit26 = 0 then H(26,238)        //     (238)  |11111111|11111111|11111010|10   $3ffffea  [26]
        else H(26,240);                    //     (240)  |11111111|11111111|11111010|11   $3ffffeb  [26]
      end else //                                        |11111111|11111111|11111011|
      if Bit25 = 0 then begin //                         |11111111|11111111|11111011|0
        if Bit26 = 0 then H(26,242)        //     (242)  |11111111|11111111|11111011|00   $3ffffec  [26]
        else H(26,243);                    //     (243)  |11111111|11111111|11111011|01   $3ffffed  [26]
      end else //                                        |11111111|11111111|11111011|1
      if Bit26 = 0 then H(26,255)          //     (255)  |11111111|11111111|11111011|10   $3ffffee  [26]
      else //                                            |11111111|11111111|11111011|11
      if Bit27 = 0 then H(27,203)          //     (203)  |11111111|11111111|11111011|110   $7ffffde  [27]
      else H(27,204);                      //     (204)  |11111111|11111111|11111011|111   $7ffffdf  [27]
    end else begin //                                    |11111111|11111111|111111

    if Bit23 = 0 then begin //                           |11111111|11111111|1111110
      if Bit24 = 0 then begin //                         |11111111|11111111|11111100|
        if Bit25 = 0 then begin //                       |11111111|11111111|11111100|0
          if Bit26 = 0 then begin //                     |11111111|11111111|11111100|00
            if Bit27 = 0 then H(27,211)    //     (211)  |11111111|11111111|11111100|000   $7ffffe0  [27]
            else H(27,212);                //     (212)  |11111111|11111111|11111100|001   $7ffffe1  [27]
          end else //                                    |11111111|11111111|11111100|01
          if Bit27 = 0 then H(27,214)      //     (214)  |11111111|11111111|11111100|010   $7ffffe2  [27]
          else H(27,221);                  //     (221)  |11111111|11111111|11111100|011   $7ffffe3  [27]
        end else //                                      |11111111|11111111|11111100|1
        if Bit26 = 0 then begin //                       |11111111|11111111|11111100|10
          if Bit27 = 0 then H(27,222)      //     (222)  |11111111|11111111|11111100|100   $7ffffe4  [27]
          else H(27,223);                  //     (223)  |11111111|11111111|11111100|101   $7ffffe5  [27]
        end else //                                      |11111111|11111111|11111100|11
        if Bit27 = 0 then H(27,241)        //     (241)  |11111111|11111111|11111100|110   $7ffffe6  [27]
        else H(27,244);                    //     (244)  |11111111|11111111|11111100|111   $7ffffe7  [27]
      end else //                                        |11111111|11111111|11111101|
      if Bit25 = 0 then begin //                         |11111111|11111111|11111101|0
        if Bit26 = 0 then begin //                       |11111111|11111111|11111101|00
          if Bit27 = 0 then H(27,245)      //     (245)  |11111111|11111111|11111101|000   $7ffffe8  [27]
          else H(27,246);                  //     (246)  |11111111|11111111|11111101|001   $7ffffe9  [27]
        end else //                                      |11111111|11111111|11111101|01
        if Bit27 = 0 then H(27,247)        //     (247)  |11111111|11111111|11111101|010   $7ffffea  [27]
        else H(27,248);                    //     (248)  |11111111|11111111|11111101|011   $7ffffeb  [27]
      end else //                                        |11111111|11111111|11111101|1
      if Bit26 = 0 then begin //                         |11111111|11111111|11111101|10
        if Bit27 = 0 then H(27,250)        //     (250)  |11111111|11111111|11111101|100   $7ffffec  [27]
        else H(27,251);                    //     (251)  |11111111|11111111|11111101|101   $7ffffed  [27]
      end else //                                        |11111111|11111111|11111101|11
      if Bit27 = 0 then H(27,252)          //     (252)  |11111111|11111111|11111101|110   $7ffffee  [27]
      else H(27,253);                      //     (253)  |11111111|11111111|11111101|111   $7ffffef  [27]
    end else begin //                                    |11111111|11111111|1111111

    Bit28:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    if Bit24 = 0 then begin //                           |11111111|11111111|11111110|
      if Bit25 = 0 then begin //                         |11111111|11111111|11111110|0
        if Bit26 = 0 then begin //                       |11111111|11111111|11111110|00
          if Bit27 = 0 then H(27,254)      //     (254)  |11111111|11111111|11111110|000   $7fffff0  [27]
          else //                                        |11111111|11111111|11111110|001
          if Bit28 = 0 then H(28,2)        //     (  2)  |11111111|11111111|11111110|0010   $fffffe2  [28]
          else H(28,3);                    //     (  3)  |11111111|11111111|11111110|0011   $fffffe3  [28]
        end else //                                      |11111111|11111111|11111110|01
        if Bit27 = 0 then begin //                       |11111111|11111111|11111110|010
          if Bit28 = 0 then H(28,4)        //     (  4)  |11111111|11111111|11111110|0100   $fffffe4  [28]
          else H(28,5);                    //     (  5)  |11111111|11111111|11111110|0101   $fffffe5  [28]
        end else //                                      |11111111|11111111|11111110|011
        if Bit28 = 0 then H(28,6)          //     (  6)  |11111111|11111111|11111110|0110   $fffffe6  [28]
        else H(28,7);                      //     (  7)  |11111111|11111111|11111110|0111   $fffffe7  [28]
      end else //                                        |11111111|11111111|11111110|1
      if Bit26 = 0 then begin //                         |11111111|11111111|11111110|10
        if Bit27 = 0 then begin //                       |11111111|11111111|11111110|100
          if Bit28 = 0 then H(28,8)        //     (  8)  |11111111|11111111|11111110|1000   $fffffe8  [28]
          else H(28,11);                   //     ( 11)  |11111111|11111111|11111110|1001   $fffffe9  [28]
        end else //                                      |11111111|11111111|11111110|101
        if Bit28 = 0 then H(28,12)         //     ( 12)  |11111111|11111111|11111110|1010   $fffffea  [28]
        else H(28,14);                     //     ( 14)  |11111111|11111111|11111110|1011   $fffffeb  [28]
      end else //                                        |11111111|11111111|11111110|11
      if Bit27 = 0 then begin //                         |11111111|11111111|11111110|110
        if Bit28 = 0 then H(28,15)         //     ( 15)  |11111111|11111111|11111110|1100   $fffffec  [28]
        else H(28,16);                     //     ( 16)  |11111111|11111111|11111110|1101   $fffffed  [28]
      end else //                                        |11111111|11111111|11111110|111
      if Bit28 = 0 then H(28,17)           //     ( 17)  |11111111|11111111|11111110|1110   $fffffee  [28]
      else H(28,18);                       //     ( 18)  |11111111|11111111|11111110|1111   $fffffef  [28]
    end else begin //                                    |11111111|11111111|11111111|

    if Bit25 = 0 then begin //                           |11111111|11111111|11111111|0
      if Bit26 = 0 then begin //                         |11111111|11111111|11111111|00
        if Bit27 = 0 then begin //                       |11111111|11111111|11111111|000
          if Bit28 = 0 then H(28,19)       //     ( 19)  |11111111|11111111|11111111|0000   $ffffff0  [28]
          else H(28,20);                   //     ( 20)  |11111111|11111111|11111111|0001   $ffffff1  [28]
        end else //                                      |11111111|11111111|11111111|001
        if Bit28 = 0 then H(28,21)         //     ( 21)  |11111111|11111111|11111111|0010   $ffffff2  [28]
        else H(28,23);                     //     ( 23)  |11111111|11111111|11111111|0011   $ffffff3  [28]
      end else //                                        |11111111|11111111|11111111|01
      if Bit27 = 0 then begin //                         |11111111|11111111|11111111|010
        if Bit28 = 0 then H(28,24)         //     ( 24)  |11111111|11111111|11111111|0100   $ffffff4  [28]
        else H(28,25);                     //     ( 25)  |11111111|11111111|11111111|0101   $ffffff5  [28]
      end else //                                        |11111111|11111111|11111111|011
      if Bit28 = 0 then H(28,26)           //     ( 26)  |11111111|11111111|11111111|0110   $ffffff6  [28]
      else H(28,27);                       //     ( 27)  |11111111|11111111|11111111|0111   $ffffff7  [28]
    end else begin //                                    |11111111|11111111|11111111|1

    if Bit26 = 0 then begin //                           |11111111|11111111|11111111|10
      if Bit27 = 0 then begin //                         |11111111|11111111|11111111|100
        if Bit28 = 0 then H(28,28)         //     ( 28)  |11111111|11111111|11111111|1000   $ffffff8  [28]
        else H(28,29);                     //     ( 29)  |11111111|11111111|11111111|1001   $ffffff9  [28]
      end else //                                        |11111111|11111111|11111111|101
      if Bit28 = 0 then H(28,30)           //     ( 30)  |11111111|11111111|11111111|1010   $ffffffa  [28]
      else H(28,31);                       //     ( 31)  |11111111|11111111|11111111|1011   $ffffffb  [28]
    end else begin //                                    |11111111|11111111|11111111|11

    if Bit27 = 0 then begin //                           |11111111|11111111|11111111|110
      if Bit28 = 0 then H(28,127)          //     (127)  |11111111|11111111|11111111|1100   $ffffffc  [28]
      else H(28,220);                      //     (220)  |11111111|11111111|11111111|1101   $ffffffd  [28]
    end else begin //                                    |11111111|11111111|11111111|111

    if Bit28 = 0 then H(28,249)            //     (249)  |11111111|11111111|11111111|1110   $ffffffe  [28]
    else begin //                                        |11111111|11111111|11111111|1111

    Bit29:=h_data and hi_bit;
    hi_bit:=hi_bit shr 1;

    Bit30:=h_data and hi_bit;

    if Bit29 = 0 then begin //                           |11111111|11111111|11111111|11110
      if Bit30 = 0 then H(30,10)           //     ( 10)  |11111111|11111111|11111111|111100   $3ffffffc  [30]
      else H(30,13);                       //     ( 13)  |11111111|11111111|11111111|111101   $3ffffffd  [30]
    end else begin //                                    |11111111|11111111|11111111|11111

    if Bit30 = 0 then H(30,22)             //     ( 22)  |11111111|11111111|11111111|111110   $3ffffffe  [30]
    else Break;                            // EOS (256)  |11111111|11111111|11111111|111111   $3fffffff  [30]

    end; // Bit29 <> 0
    end; // Bit28 <> 0
    end; // Bit27 <> 0
    end; // Bit26 <> 0
    end; // Bit25 <> 0
    end; // Bit24 <> 0
    end; // Bit23 <> 0
    end; // Bit22 <> 0
    end; // Bit21 <> 0
    end; // Bit20 <> 0
    end; // Bit19 <> 0
    end; // Bit18 <> 0
    end; // Bit17 <> 0
    end; // Bit16 <> 0
    end; // Bit15 <> 0
    end; // Bit14 <> 0
    end; // Bit13 <> 0
    end; // Bit12 <> 0
    end; // Bit11 <> 0
    end; // Bit10 <> 0
    end; // Bit9 <> 0
    end; // Bit8 <> 0
    end; // Bit7 <> 0
    end; // Bit6 <> 0
    end; // Bit5 <> 0
    end; // Bit4 <> 0
    end; // Bit3 <> 0
    end; // Bit2 <> 0
    end; // Bit1 <> 0

    end; // while TRUE do

  SetLength(Result,outlen);
  end;

function Huff_Decode(const Data:RtcString):RtcString;
  begin
  Result:=Huff_DecodeEx(RtcStringToBytes(Data));
  end;

function LInt_EncodeEx(const Value:int64; PrefixBits:Byte=8):RtcByteArray;
  var
    MaxValue, Len:Byte;
    Data: int64;
  begin
  MaxValue:=$FF shr (8-PrefixBits);
  if Value<MaxValue then
    begin
    SetLength(Result,1);
    Result[0]:=Value;
    end
  else
    begin
    // Calculate Length ...
    Len:=2;
    Data:=Value-MaxValue;
    while Data >= $80 do
      begin
      Inc(Len);
      Data:=Data shr 7;
      end;
    // Encode ...
    SetLength(Result,Len);
    Result[0]:=MaxValue;
    Len:=1;
    Data:=Value-MaxValue;
    while Data >= $80 do
      begin
      Result[Len]:=(Data and $7F)+$80;
      Inc(Len);
      Data:=Data shr 7;
      end;
    Result[Len]:=Data;
    end;
  end;

function LInt_DecodeEx(const Data:RtcByteArray; var Len:Byte; PrefixBits:Byte=8; FromByte:Integer=0):int64;
  var
    MyValue, Shift:Byte;
    Tmp: int64;
    idx, MaxIDX: Integer;
  begin
  MaxIDX:=length(Data)-1;
  if (FromByte<0) or (FromByte>MaxIDX) then
    begin
    Result:=0;
    Len:=0;
    end
  else
    begin
    MyValue:=$FF shr (8-PrefixBits);
    Len:=1;
    idx:=FromByte;
    Result:=Data[idx] and MyValue;
    if Result>=MyValue then
      begin
      shift:=0;
      MyValue:=$80;
      while idx<MaxIDX do
        begin
        Inc(idx); Inc(Len);
        MyValue:=Data[idx];
        Tmp:=MyValue and $7F;
        Inc(Result,Tmp shl shift);
        if MyValue and $80=0 then Break;
        Inc(shift,7);
        if shift>63 then Break; // overflow
        end;
      if MyValue and $80<>0 then
        begin
        // Decoding Error
        Result:=0;
        Len:=0;
        end;
      end;
    end;
  end;

function LWord_EncodeEx(const Value:Cardinal; PrefixBits:Byte=8):RtcByteArray;
  var
    MaxValue, Len:Byte;
    Data: Cardinal;
  begin
  MaxValue:=$FF shr (8-PrefixBits);
  if Value<MaxValue then
    begin
    SetLength(Result,1);
    Result[0]:=Value;
    end
  else
    begin
    // Calculate Length ...
    Len:=2;
    Data:=Value-MaxValue;
    while Data >= $80 do
      begin
      Inc(Len);
      Data:=Data shr 7;
      end;
    // Encode ...
    SetLength(Result,Len);
    Result[0]:=MaxValue;
    Len:=1;
    Data:=Value-MaxValue;
    while Data >= $80 do
      begin
      Result[Len]:=(Data and $7F)+$80;
      Inc(Len);
      Data:=Data shr 7;
      end;
    Result[Len]:=Data;
    end;
  end;

function LWord_DecodeEx(const Data:RtcByteArray; var Len:Byte; PrefixBits:Byte=8; FromByte:Integer=0):Cardinal;
  var
    MyValue, Shift:Byte;
    Tmp: Cardinal;
    idx, MaxIDX: Integer;
  begin
  MaxIDX:=length(Data)-1;
  if (FromByte<0) or (FromByte>MaxIDX) then
    begin
    Result:=0;
    Len:=0;
    end
  else
    begin
    MyValue:=$FF shr (8-PrefixBits);
    Len:=1;
    idx:=FromByte;
    Result:=Data[idx] and MyValue;
    if Result>=MyValue then
      begin
      shift:=0;
      MyValue:=$80;
      while idx<MaxIDX do
        begin
        Inc(idx); Inc(Len);
        MyValue:=Data[idx];
        Tmp:=MyValue and $7F;
        Inc(Result,Tmp shl shift);
        if MyValue and $80=0 then Break;
        Inc(shift,7);
        if shift>31 then Break; // overflow
        end;
      if MyValue and $80<>0 then
        begin
        // Decoding Error
        Result:=0;
        Len:=0;
        end;
      end;
    end;
  end;

{ tRtcDynamicHeaderTable }

constructor tRtcDynamicHeaderTable.Create(StartMaxTableSize:Integer);
  begin
  inherited Create;
  FTableSize:=StartMaxTableSize;
  FMaxSize:=FTableSize;
  FSize:=0;
  FCount:=0;
  First:=nil;
  Last:=nil;
  end;

destructor tRtcDynamicHeaderTable.Destroy;
  begin
  // This will remove all entries from the Table
  MakeSpace(FMaxSize);
  inherited;
  end;

function tRtcDynamicHeaderTable.Get(idx: Integer; var Name, Value: RtcString): boolean;
  var
    a:Integer;
    Item:PRtcHPACK_DynTableItem;
  begin
  if (idx<1) or (idx>FCount) then
    begin
    Name:='';
    Value:='';
    Result:=False;
    end
  else
    begin
    Item:=First;
    if idx>1 then
      for a:=2 to idx do
        Item:=Item^.Next;
    Name:=Item^.Name;
    Value:=Item^.Value;
    Result:=True;
    end;
  end;

function tRtcDynamicHeaderTable.Find(const Name, Value: RtcString): Integer;
  var
    a,Found:Integer;
    Item:PRtcHPACK_DynTableItem;
  begin
  Item:=First;
  Found:=0; a:=0;
  while Item<>nil do
    begin
    Inc(a);
    if Item^.Name=Name then
      if Item^.Value=Value then
        begin
        Found:=a;
        Break;
        end
      else if Found=0 then
        Found:=-a;
    Item:=Item^.Next;
    end;
  Result:=Found;
  end;

function tRtcDynamicHeaderTable.Insert(const Name, Value: RtcString): boolean;
  var
    WantLen:Integer;
    Item:PRtcHPACK_DynTableItem;
  begin
  WantLen:=Length(Name)+Length(Value)+32;
  MakeSpace(WantLen);
  if WantLen>FMaxSize then
    Result:=False
  else
    begin
    New(Item);
    FillChar(Item^,SizeOf(Item^),0);
    Item^.Name:=Name;
    Item^.Value:=Value;
    Item^.Prior:=nil;
    Item^.Next:=First;
    if First<>nil then
      First^.Prior:=Item
    else
      Last:=Item;
    First:=Item;
    Inc(FCount);
    Inc(FSize,WantLen);
    Result:=True;
    end;
  end;

procedure tRtcDynamicHeaderTable.MakeSpace(const Len: Integer);
  var
    WantLen:Integer;
    Item:PRtcHPACK_DynTableItem;
  begin
  if Len>=FMaxSize then
    WantLen:=0
  else
    WantLen:=FMaxSize-Len;

  Item:=Last;
  while FSize>WantLen do
    begin
    Dec(FCount);
    Dec(FSize,length(Item^.Name));
    Dec(FSize,length(Item^.Value));
    Dec(FSize,32);

    Last:=Item^.Prior;
    if Last<>nil then
      Last^.Next:=nil
    else
      First:=nil;

    Item^.Name:='';
    Item^.Value:='';
    Item^.Prior:=nil;
    Item^.Next:=nil;
    Dispose(Item);

    Item:=Last;
    end;
  end;

function tRtcDynamicHeaderTable.GetCount: Integer;
  begin
  Result:=FCount;
  end;

function tRtcDynamicHeaderTable.GetSize: Integer;
  begin
  Result:=FSize;
  end;

procedure tRtcDynamicHeaderTable.SetMaxSize(const Value: Integer);
  begin
  if FMaxSize<=FTableSize then
    begin
    FMaxSize:=Value;
    MakeSpace(0);
    end
  else
    FMaxSize:=FTableSize;
  end;

function tRtcDynamicHeaderTable.GetMaxSize: Integer;
  begin
  Result:=FMaxSize;
  end;

procedure tRtcDynamicHeaderTable.SetTableSize(const Value: Integer);
  begin
  FTableSize:=Value;
  if FTableSize<FMaxSize then
    begin
    FMaxSize:=FTableSize;
    MakeSpace(0);
    end;
  end;

function tRtcDynamicHeaderTable.GetTableSize: Integer;
  begin
  Result:=FTableSize;
  end;

initialization
AppFileName:=ExpandUNCFileName(RtcWideString(ParamStr(0)));
TickTimeCS:=TRtcCritSec.Create;
TickTimeOverflow:=-int64(Get_TickTime and TickTimeOverflowMask);
LastTickTime:=0;
RtcSetAnsiCodePage(cpWin1252);
finalization
RtcFreeAndNil(TickTimeCS);
AppFileName:='';
end.
