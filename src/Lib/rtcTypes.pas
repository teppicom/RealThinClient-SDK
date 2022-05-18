{
  @html(<b>)
  Base Types
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
}
unit rtcTypes;

{$INCLUDE rtcDefs.inc}

interface

{$INCLUDE rtc_Version.inc}

const
{$IFDEF WINDOWS}
  // System-specific Folder delimiter
  FOLDER_DELIMITER='\';
{$ELSE}
  // System-specific Folder delimiter
  FOLDER_DELIMITER='/';
{$ENDIF}

  // @exclude
  pidWin32        = $0001;
  // @exclude
  pidWin64        = $0002;
  // @exclude
  pidOSX32        = $0004;
  // @exclude
  pidiOSSimulator = $0008;
  // @exclude
  pidAndroid      = $0010;
  // @exclude
  pidLinux32      = $0020;
  // @exclude
  pidiOSDevice32  = $0040;
  // @exclude
  pidLinux64      = $0080;
  // @exclude
  pidWinNX32      = $0100;
  // @exclude
  pidWinIoT32     = $0200; // Embedded IoT (Internet of Things) Windows w/ Intel Galileo
  // @exclude
  pidiOSDevice64  = $0400;

  // @exclude
  pidLinux = pidLinux32 or pidLinux64;
  // @exclude
  pidWindows = pidWin32 or pidWin64 or pidWinIoT32 or pidWinNX32;
  // @exclude
  pidMacOSX  = pidOSX32;
  // @exclude
  pidDesktop = pidWindows or pidMacOSX or pidLinux;
  // @exclude
  pidMobile  = pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid;
  // @exclude
  pidAll     = pidDesktop or pidMobile;

type
  { "IP Version" preference
       IPv4 = IPv4 -> OS Default -> IPv6;
       IPvOS4 = OS Default -> IPv4 -> IPv6;
       IPv6 = IPv6 -> OS Default -> IPv4;
       IPvOS6 = OS Default -> IPv6 -> IPv4 }
  RtcIPV = (rtc_IPv4, rtc_IPvOS4, rtc_IPv6, rtc_IPvOS6);

{$IFDEF AUTOREFCOUNT}
  // Array of bytes
  RtcByteArray = array of byte; // TArray<Byte>;
{$ELSE}
  // Array of bytes
  RtcByteArray = array of byte;
{$ENDIF}

{$IFDEF UNICODE}
  // @exclude
  RtcBinWideChar = Word;
  // Unicode String
  RtcWideString = String;
  // Unicode character
  RtcWideChar = Char;
{$ELSE}
  // @exclude
  RtcBinWideChar = Word;
  {$IFDEF FPC_UNICODE}
    {$DEFINE UNICODE}
    // Unicode String
    RtcWideString = UnicodeString;
    // Unicode character
    RtcWideChar = UnicodeChar;
  {$ELSE}
    // Unicode String
    RtcWideString = WideString;
    // Unicode character
    RtcWideChar = WideChar;
  {$ENDIF}
{$ENDIF}

{$IFDEF NEXTGEN}
  // @exclude
  RtcBinChar = Word;
  // 8-bit character String (NOT UNICODE!)
  RtcString = String;
  // 8-bit character (NOT UNICODE!)
  RtcChar = Char;
  // @exclude
  RtcPtrAnsiChar = MarshaledAString;
  // @exclude
  RtcPtrWideChar = MarshaledString;
{$ELSE}
  // @exclude
  RtcPtrAnsiChar = PAnsiChar;
  // @exclude
  RtcPtrWideChar = PWideChar;
  {$IFDEF RTC_BYTESTRING}
    // @exclude
    RtcBinChar = Byte;
    {$IFDEF UNICODE}
      // 8-bit character String (NOT UNICODE!)
      RtcString = AnsiString;
      // 8-bit character (NOT UNICODE!)
      RtcChar = AnsiChar;
    {$ELSE}
      // 8-bit character String (NOT UNICODE!)
      RtcString = String;
      // 8-bit character (NOT UNICODE!)
      RtcChar = Char;
    {$ENDIF}
  {$ELSE}
    // @exclude
    RtcBinChar = Word;
    // 8-bit character String (NOT UNICODE!)
    RtcString = RtcWideString;
    // 8-bit character (NOT UNICODE!)
    RtcChar = RtcWideChar;
  {$ENDIF}
{$ENDIF}

{$IFDEF CPUX64}
  // @exclude
  RtcIntPtr = NativeUInt; // uint64;
  {$IFDEF FPC}
    // Thread ID
    RtcThrID = TThreadID;
  {$ELSE}
    // Thread ID
    RtcThrID = Cardinal;
  {$ENDIF}
{$ELSE}{$IFDEF CPUX32}
  // @exclude
  RtcIntPtr = Cardinal;
  {$IFDEF FPC}
    // Thread ID
    RtcThrID = TThreadID;
  {$ELSE}
    // Thread ID
    RtcThrID = Cardinal;
  {$ENDIF}
{$ELSE}{$IFDEF CPUARM}
  // @exclude
  RtcIntPtr = NativeUInt;
  // Thread ID
  RtcThrID = NativeUInt;
{$ELSE}{$IFDEF FPC}
  // @exclude
  RtcIntPtr = uint64;
  // Thread ID
  RtcThrID = TThreadID;
{$ELSE}
  {$message error 'rtcTypes: Unknown POINTER Type'}
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

  { @abstract(Fast RTC Object) }
  TRtcFastObject = class(TObject);

  { Pointer Pool (Pointer<>nil) }
  tRtcPtrPool = class(TRtcFastObject)
    private
      pObjs:array of Pointer;
      fCount,fSize:integer;
      procedure SetSize(x:integer);
    public
      { Create a Pool with starting capacity of "Size" Pointers.
        Use "Put" to add one Pointer to the Pool, then you can
        use "Get" to retrieve one Pointer from the Pool (last-in, first-out. }
      constructor Create(Size:integer=0);
      { Destroy the Pool }
      destructor Destroy; override;
      { Add Pointer "x" to the Pool.
        If Pool is full, returns FALSE and "x" is NOT added to the Pool. }
      function Put(const x:Pointer):boolean;
      { Get Pointer from Pool (returns last Pointer added).
        If Pool is Empty, returns NIL. }
      function Get:Pointer;
      { Get/Set Pool Size.
        Pool can ONLY hold up to "Size" Pointers.
        Reducing Pool size below "Count",
        removes last Pointers added to the Pool,
        without freeing the memory they point to
        (the same as using "Get" to retrieve the Pointers). }
      property Size:integer read fSize write SetSize;
      { Number of Pointers currently in the Pool }
      property Count:integer read fCount;
    end;

  { Object Pool (TObject<>nil) }
  tRtcObjPool = class(TRtcFastObject)
    private
      pObjs:array of TObject;
      fCount,fSize:integer;
      procedure SetSize(x:integer);
    public
      { Create a Pool with starting capacity of "Size" Objects.
        Use "Put" to add one Object to the Pool, then you can
        use "Get" to retrieve one Object from the Pool (last-in, first-out. }
      constructor Create(Size:integer=0);
      { Destroy the Pool }
      destructor Destroy; override;
      { Add Object "x" to the Pool.
        If Pool is full, returns FALSE and "x" is NOT added to the Pool. }
      function Put(const x:TObject):boolean;
      { Get one Object from the Pool (returns last Object added).
        If Pool is Empty, returns NIL. }
      function Get:TObject;
      { Get/Set Pool Size.
        Pool can ONLY hold up to "Size" Objects.
        Reducing Pool size below "Count",
        removes last Objects added to the Pool,
        without destroying the Objects
        (the same as using "Get" to retrieve the Objects). }
      property Size:integer read fSize write SetSize;
      { Number of Objects currently in the Pool }
      property Count:integer read fCount;
    end;

  // Alias for backwards compatibility
  tPtrPool = tRtcPtrPool;

const
  // @exclude
  rtc_IPVDefault = rtc_IPv4;
  // @exclude
  SizeOf_Char = SizeOf(RtcChar);
  // @exclude
  SizeOf_WideChar = SizeOf(RtcWideChar);
  // @exclude => Max 64-bit signed int - 1
  RtcMaxLargeInt = $7FFFFFFFFFFFFFFE;
  // @exclude => Max 32-bit signed int - 1
  RtcMaxLongInt = $7FFFFFFE;
  // @exclude => Max 32-bit unsigned int - 1
  RtcMaxLongWord = $FFFFFFFE;

var
  // @exclude
  RtcEmptyByteArray:RtcByteArray;

{ The purupose of "RtcFreeAndNil" is to make sure that the Object "Obj"
  is Disposed of immediately (destructor will be called now) and that
  the Object pointer is set to NIL, even on MOBILE platforms with ARC. }
procedure RtcFreeAndNil(var obj);

// RtcIPV matching "IPv6" and "OS_Default" parameters
function GetRtcIPV(IPv6:boolean=False;OS_Default:boolean=False):RtcIPV;

{ Returns TRUE if "s" is Empty or Undefined/Unspecified Network Address }
function NoIP(const s:RtcString):boolean;

implementation

function NoIP(const s:RtcString):boolean;
  begin
  Result:=(s='') or 
          (s='0.0.0.0') or 
          (s='::') or 
          (s='::0');
  end;
  
// RtcIPV matching "OS_Default" and "IPv6" parameters
function GetRtcIPV(IPv6:boolean=False;OS_Default:boolean=False):RtcIPV;
  begin
  if OS_Default then
    begin
    if IPv6 then
      Result:=rtc_IPvOS6
    else
      Result:=rtc_IPvOS4;
    end
  else if IPv6 then
    Result:=rtc_IPv6
  else
    Result:=rtc_IPv4;
  end;

procedure RtcFreeAndNil(var obj);
  var
    Temp:TObject absolute obj;
    i:RtcIntPtr absolute obj;

  { 
    // This would be required for propper "ARC"
    // if the "TRtcObject" class was implemented (see below) ...
    rtcTemp:TRtcObject absolute obj; 
  }

  begin
  if i=0 then Exit;
{$IFDEF AUTOREFCOUNT}
  Temp.DisposeOf; // We might have external references. Call the object destructor
  Temp:=nil;
{$ELSE}

{
  // This would ALSO be required for "TRtcObject" to work ...
  if Temp is TRtcObject then
    rtcTemp.DisposeOf
  else
}
    Temp.Free;
  Temp:=nil;
{$ENDIF}
  end;

(*
// Implementation from the SysUtils unit ...
procedure FreeAndNil(var obj);
{$IF not Defined(AUTOREFCOUNT)}
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;
{$ELSE}
begin
  TObject(Obj) := nil;
end;
{$ENDIF}
*)

(**********

{ *Experimental* implementation of a class with ARC (Automatic-Reference-Counting)
  which would kind-of work for ALL platforms by declaring an Interface
  and implementing ARC through that Interface on platforms without ARC.

  Problems?

  1. "TObject.Free" calls "Destroy" directly, ignoring all other references 
     of te same Object currently being used by Interfaces or other Objects.

  2. References to Objects are NOT auto-reference-counted, even if the 
     Class implements IInterface and all methods required for ARC.  }

type
  // forward
  TRtcObject = class;

{$IFDEF AUTOREFCOUNT}
  { @abstract(Auto-Reference-Counted RTC Interface) }
  ARtcObject = TRtcObject;
{$ELSE}
  { @abstract(Auto-Reference-Counted RTC Interface) }
  ARtcObject = interface(IInterface)
['{3CD9E634-D8FA-4D7B-B955-A0FEBC8746DE}']
    procedure Free;
    procedure DisposeOf;
    function GetDisposed: boolean;
    function RefCount: Integer;
    function AutoFree: ARtcObject;
    function Instance: TRtcObject;
    property Disposed: Boolean read GetDisposed;
    property Reference: ARtcObject read AutoFree;
    end;
{$ENDIF}

  { @abstract(Auto-Reference-Counted RTC Object) }
{$IFDEF AUTOREFCOUNT}
  TRtcObject = class(TObject)
{$ELSE}
  TRtcObject = class(TObject,ARtcObject)
  protected
    FRefCount: Integer;
    FRefValid: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    class function _MarkDestroying(const Obj): Boolean;
    class function _SetDisposed(const Obj): Boolean;

    function GetDisposed: Boolean; reintroduce;

  public
    { This call destroys the object immediately if
      called on an "TRtcObject" Object instance and
      there are no "ARtcObject" references left,
      but ... it does nothing if there are any
      "ARtcObject" refferences to this Object left,
      including any temporary Reference created by
      using the "AutoFree" method on this Object. }
    procedure Free; reintroduce;

    { If this is the 1st call to this method, it will
      disposes of this Object immediately by calling
      its destructor. This is true for calls made
      directly on any "TRtcObject" instance as well
      as calls made on any "ARtcObject" Reference.

      Then, the Object will be released from memory
      if there are no "ARtcObject" references left.

      If there is at least one "ARtcObject" Reference
      left, the Object will remain in memory until
      the last Reference has been set to NIL. }
    procedure DisposeOf; reintroduce;

    // @exclude
    procedure AfterConstruction; override;
    // @exclude
    procedure BeforeDestruction; override;
    // @exclude
    class function NewInstance: TObject; override;

    { Object Reference Counter. This returns the number of "ARtcObject"
      references curently pointing at this Object. If this returns 0,
      it means that there are no other references left and any call
      to "Free" or "DisposeOf" will destroy this Object immediately. }
    function RefCount: Integer;

    { Returns TRUE if "DisposeOf" has already been called for this Object. }
    property Disposed: Boolean read GetDisposed;
{$ENDIF}
    { Returns the "TRtcObject" instance (self) }
    function Instance: TRtcObject;

  { This is a convenience method, which makes it possible to
    use scope-based auto-Reference-counting to automatically
    free the Object regardless of target platform, without the
    need to use Interfaces on platforms which do NOT have support
    for automatic Reference counting on Objects (Windows, OSX).  @html(<br><br>)

    How does this work?  @html(<br><br>)

    The "AutoFree" method returns a "ARtcObject" (Interface to self)
    on all platforms which do NOT have support for Objects with automatic
    Reference counting (like Windows and Mac OSX). If you do NOT assign
    the result of this method to ANYTHING and (use this method as if
    it was a procedure which does NOT return a Result), a temporary
    "IRtcObject" Reference will be created for this Object.  @html(<br><br>)

    This temporary "ARtcObject" Reference will keep the Object alive
    inside the current code-block (procedure, function or method where
    the "AutoFree" method was called), while at the same time ensuring
    that the Object will be destroyed and released from memory when
    the current code-block finishes, if there are no other references
    to this Object stored in any other "ARtcArcObject" variable. @html(<br><br>)

    In other words, if you use the "AutoFree" method on this Object
    *without* assigning its Result to a "ARtcArcObject" variable,
    your Object will be auto-destroyed as soon as you exist the
    function, procedure or method where "AutoFree" was called. @html(<br><br>)

    Here is an Example: @codeblock(
    procedure Test_AutoFree;
      var
        MyRecord:TRtcRecord;
      begin
        MyRecord:=TRtcRecord.Create;
        MyRecord.AutoFree;
        // ... work with "MyRecord" as usual ...
        MyRecord.asDateTime['now']:=Now; // just an example
        // ...
      end; // "MyRecord" will be auto-freed here )

    @html(<br><br>)
    The code above produces the same results as this:
    procedure Test_AutoFree;
      var
        MyRecord:TRtcRecord;
      begin
        MyRecord:=TRtcRecord.Create;
        try
          // ... work with "MyRecord" as usual ...
          MyRecord.asDateTime['now']:=Now; // just an example
          // ...
        finally
          MyResult.Free; // "MyRecord" has to be freed manually
          end;
      end;  )

    }
    function AutoFree:ARtcObject;

    { Reference to "ARtcObject" with built-in validity checks }
    property Reference: ARtcObject read AutoFree;
    end;

{ TRtcObject }

{$IFDEF AUTOREFCOUNT}

function TRtcObject.AutoFree: ARtcObject;
  begin
  Result:=self;
  end;

function TRtcObject.Instance: TRtcObject;
  begin
  Result:=self;
  end;

{$ELSE}

{$IFDEF IDE_XE3UP}

// AtomicIncrement, AtomicDecrement and AtomicCmpExchange are available in the "System.pas" unit

{$ELSE}{$IFDEF CPUX64}

function AtomicDecrement(var Addend: LongInt): LongInt;
  asm
      .NOFRAME
      MOV   EAX,-1
  LOCK XADD  [RCX].Integer,EAX
      DEC   EAX
  end;

function AtomicIncrement(var Addend: LongInt): LongInt;
  asm
      MOV   EAX,1
  LOCK XADD  [RCX].Integer,EAX
      INC   EAX
  end;

function AtomicCmpExchange(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer;
  asm
      .NOFRAME
      MOV     EAX,R8d
  LOCK CMPXCHG [RCX].Integer,EDX
  end;

{$ELSE}{$IFDEF CPUX86}

function AtomicAdd(var Addend: Integer; Increment: Integer): Integer;
  asm
      MOV   ECX,EAX
      MOV   EAX,EDX
  LOCK XADD  [ECX],EAX
      ADD   EAX,EDX
  end;

function AtomicDecrement(var Addend: Integer): Integer;
  asm
      MOV   EDX,-1
      JMP   AtomicAdd
  end;

function AtomicIncrement(var Addend: Integer): Integer;
  asm
      MOV   EDX,1
      JMP   AtomicAdd
  end;

function AtomicCmpExchange(var Target: Integer; Exchange: Integer; Comparand: Integer): Integer;
  asm
      XCHG    EAX,ECX
  LOCK CMPXCHG [ECX],EDX
  end;

{$ELSE}

function AtomicDecrement(var Addend: LongInt): LongInt; stdcall;
  begin
  Dec(Addend);
  Result := Addend;
  end;

function AtomicIncrement(var Addend: LongInt): LongInt; stdcall;
  begin
  Inc(Addend);
  Result := Addend;
  end;

function AtomicCmpExchange(var Destination: LongInt; Exchange: LongInt; Comparand: LongInt): LongInt; stdcall;
  begin
  Result := Destination;
  if Result = Comparand then Destination := Exchange;
  end;

{$ENDIF}{$ENDIF}{$ENDIF}

const
  xDestroyFlag   = Integer($80000000);
  xDisposeFlag   = Integer($40000000);
  xGoneFlags     = Integer($C0000000);
  xValidFlag     = Integer($45C0B7EC);
  xInvalidFlag   = Integer($CE7B0C54);

class function TRtcObject.NewInstance: TObject;
  begin
  Result := inherited NewInstance;
  // Set an implicit refcount so that refcounting during construction won't destroy the object.
  TRtcObject(Result).FRefCount := 1;
  TRtcObject(Result).FRefValid := xValidFlag;
  end;

function TRtcObject.RefCount: Integer;
  begin
  if FRefValid = xValidFlag then
    Result := FRefCount and not xGoneFlags
  else
    Result := -1;
  end;

procedure TRtcObject.AfterConstruction;
  begin
  // Release the constructor's implicit refcount
  AtomicDecrement(FRefCount);
  end;

function TRtcObject.QueryInterface(const IID: TGUID; out Obj): HResult;
  begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
  end;

procedure TRtcObject.BeforeDestruction;
  begin
  if FRefValid<>xValidFlag then
    Error(reAccessViolation)  // Destructor called more than once
  else if not Disposed then // "TObject.Free" or ".Destroy" called directly
    begin
    if RefCount<>0 then
      Error(reInvalidPtr)  // Object has other references
    else if not _MarkDestroying(Self) then
      Error(reAccessViolation); // object already destroyed
    FRefValid:=xInvalidFlag;
    end
  else if (FRefCount and xDestroyFlag) <> 0 then
    FRefValid:=xInvalidFlag; // Destroying Object instance
  end;

function TRtcObject.GetDisposed: Boolean;
  begin
  if FRefValid = xValidFlag then
    Result := (FRefCount and xGoneFlags) <> 0
  else
    Result := True;
  end;

class function TRtcObject._SetDisposed(const Obj): Boolean;
  var
    LRef: Integer;
  begin
  repeat
    LRef := TRtcObject(Obj).FRefCount;
    until AtomicCmpExchange(TRtcObject(Obj).FRefCount, LRef or xDisposeFlag, LRef) = LRef;
  Result := LRef and xDisposeFlag = 0;
  end;

class function TRtcObject._MarkDestroying(const Obj) : Boolean;
  var
    LRef: Integer;
  begin
  repeat
    LRef := TRtcObject(Obj).FRefCount;
    until AtomicCmpExchange(TRtcObject(Obj).FRefCount, LRef or xDestroyFlag, LRef) = LRef;
  Result := LRef and xDestroyFlag = 0;
  end;

function TRtcObject._AddRef: Integer;
  begin
  if FRefValid <> xValidFlag then
    Result:=0
  else if (FRefCount and xDestroyFlag) <> 0 then
    Result:=0
  else
    Result := AtomicIncrement(FRefCount) and not xGoneFlags;
  end;

function TRtcObject._Release: Integer;
  begin
  if FRefValid <> xValidFlag then
    Result:=0
  else if (FRefCount and xDestroyFlag) <> 0 then
    Result:=0
  else
    begin
    Result := AtomicDecrement(FRefCount) and not xDisposeFlag;
    if Result = 0 then
      if _MarkDestroying(Self) then
        if _SetDisposed(Self) then
          Destroy
        else
          begin
          FRefValid:=xInvalidFlag;
          FreeInstance;
          end;
    end;
  end;

procedure TRtcObject.DisposeOf;
  type
    TDestructorProc = procedure (Instance: Pointer; OuterMost: ShortInt);
  var
    ip : RtcIntPtr;
    LRef : Integer;
  begin
  if Self<>nil then
    begin
    LRef := _AddRef;
    try
      if LRef>1 then
        if _SetDisposed(Self) then
          begin
          BeforeDestruction;
          ip:=RtcIntPtr(PByte(PPointer(Self)^));
          Inc(ip,vmtDestroy);
          TDestructorProc(PPointer(ip)^)(Self, 0);
          end;
    finally
      if LRef>0 then _Release;
      end;
    end;
  end;

procedure TRtcObject.Free;
  begin
  if Self<>nil then
    if _AddRef>0 then
      _Release;
  end;

function TRtcObject.AutoFree: ARtcObject;
  begin
  if self=nil then
    Result:=nil
  else if FRefValid <> xValidFlag then
    Result:=nil
  else if (FRefCount and xGoneFlags) <> 0 then
    Result:=nil
  else if _AddRef<=0 then
    Result:=nil
  else
    try
      Result:=self;
    finally
      _Release;
      end;
  end;

function TRtcObject.Instance: TRtcObject;
  begin
  if self=nil then
    Result:=nil
  else if FRefValid <> xValidFlag then
    Result:=nil
  else if (FRefCount and xGoneFlags) <> 0 then
    Result:=nil
  else
    Result:=self;
  end;

{$ENDIF}

********)

{ tRtcPrtPool }

constructor tRtcPtrPool.Create(Size: integer);
  begin
  inherited Create;
  fSize:=Size;
  if fSize>0 then
    SetLength(pObjs,fSize);
  fCount:=0;
  end;

destructor tRtcPtrPool.Destroy;
  var
    i:integer;
  begin
  if fCount>0 then
    begin
    for i:=0 to fCount-1 do
      pObjs[i]:=nil;
    fCount:=0;
    end;
  if fSize>0 then
    begin
    SetLength(pObjs,0);
    fSize:=0;
    end;
  inherited;
  end;

function tRtcPtrPool.Get:Pointer;
  begin
  if fCount>0 then
    begin
    Dec(fCount);
    Result:=pObjs[fCount];
    end
  else
    Result:=nil;
  end;

function tRtcPtrPool.Put(const x: Pointer): boolean;
  begin
  if fCount<fSize then
    begin
    pObjs[fCount]:=x;
    Inc(fCount);
    Result:=True;
    end
  else
    Result:=False;
  end;

procedure tRtcPtrPool.SetSize(x: integer);
  var
    i:integer;
  begin
  if x>fSize then
    begin
    fSize:=x;
    SetLength(pObjs,fSize);
    end
  else if x<fSize then
    begin
    for i:=x to fSize-1 do
      pObjs[i]:=nil;
    fSize:=x;
    SetLength(pObjs,fSize);
    end;
  end;

{ tRtcObjPool }

constructor tRtcObjPool.Create(Size: integer);
  begin
  inherited Create;
  fSize:=Size;
  if fSize>0 then
    SetLength(pObjs,fSize);
  fCount:=0;
  end;

destructor tRtcObjPool.Destroy;
  var
    i:integer;
  begin
  if fCount>0 then
    begin
    for i:=0 to fCount-1 do
      pObjs[i]:=nil;
    fCount:=0;
    end;
  if fSize>0 then
    begin
    SetLength(pObjs,0);
    fSize:=0;
    end;
  inherited;
  end;

function tRtcObjPool.Get:TObject;
  begin
  if fCount>0 then
    begin
    Dec(fCount);
    Result:=pObjs[fCount];
    end
  else
    Result:=nil;
  end;

function tRtcObjPool.Put(const x: TObject): boolean;
  begin
  if fCount<fSize then
    begin
    pObjs[fCount]:=x;
    Inc(fCount);
    Result:=True;
    end
  else
    Result:=False;
  end;

procedure tRtcObjPool.SetSize(x: integer);
  var
    i:integer;
  begin
  if x>fSize then
    begin
    fSize:=x;
    SetLength(pObjs,fSize);
    end
  else if x<fSize then
    begin
    for i:=x to fSize-1 do
      pObjs[i]:=nil;
    fSize:=x;
    SetLength(pObjs,fSize);
    end;
  end;

initialization
SetLength(RtcEmptyByteArray,0);
end.
