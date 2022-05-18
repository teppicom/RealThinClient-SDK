{
  @html(<b>)
  Memory Check Unit
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)
  
  @exclude
}
unit rtcMemory;

{$INCLUDE rtcDefs.inc}

interface

{ Get Complete Heap Status }
function Get_HeapStatus:THeapStatus;

{ Check the ammount of memoy in use (bytes) }
function Get_MemoryInUse:int64;

{ Check how much Address Space is used by the Application (KB) }
function Get_AddressSpaceUsed:int64;

implementation

{$IFNDEF FPC}
  {$O-}
{$ENDIF}

{$IFDEF WINDOWS}
uses 
  Windows;

function Get_AddressSpaceUsed: int64;
  var
    LMemoryStatus: TMemoryStatus;
  begin
  {Set the structure size}
  LMemoryStatus.dwLength := SizeOf(LMemoryStatus);
  {Get the memory status}
  GlobalMemoryStatus(LMemoryStatus);
  {The result is the total address space less the free address space}
  Result := (LMemoryStatus.dwTotalVirtual - LMemoryStatus.dwAvailVirtual) shr 10;
  end;
  
{$ELSE}

function Get_AddressSpaceUsed: int64;
  var
    hs :THeapStatus;
  begin
  // no funciton available?
  hs := GetHeapStatus;
  Result := hs.TotalCommitted
  end;
  
{$ENDIF}

function Get_HeapStatus:THeapStatus;
  begin
  Result:=GetHeapStatus;
  end;

function Get_MemoryInUse:int64;
  begin
  Result:=GetHeapStatus.TotalAllocated;
  end;

end.
