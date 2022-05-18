program MemTest;

{$APPTYPE CONSOLE}

{$include rtcDefs.inc}
{$include rtcDeploy.inc}

uses
{$IFDEF RtcDeploy}
  {$IFNDEF IDE_2006up}
    FastMM4,
    FastMove,
  {$ENDIF}
{$ENDIF}
  SysUtils,

  rtcTypes,
  rtcSystem,
  rtcInfo,
  TestmemObjList,
  TestmemBinList,
  TestmemStrList,
  TestmemStrIntList,
  TestmemStrObjList,
  TestmemStringIntList,
  TestmemStringObjList,
  TestmemBinTree
{$IFDEF IDE_2010up}
  ,TestRtcSortListStr
  ,TestRtcSortListInt
  ,TestRtcSortTreeInt
  ,TestRtcSortTreeStr
{$ENDIF}
  ;

var
  i,j:longword;
  st:int64;
  FullSearch:boolean;

  begin
{$IFDEF IDE_2006up}
  ReportMemoryLeaksOnShutdown:=True;
{$ENDIF}

  Writeln('RTC SDK List Testing v2.1 (c) Teppi Technology');
  Writeln('Use: '+ExtractFileName(ParamStr(0))+' [ItemCount]');
  Writeln('  ItemCount = number of items to use in test (default=1000)');
  Writeln;
  if ParamCount>0 then
    i:=StrToIntDef(ParamStr(1),1000)
  else
    i:=1000;

  if i<1 then i:=1;

  j:=i div 1000;
  if (j<64) then j:=64
  else if (j>10000) then j:=10000;

  Writeln('First Quick Test. Insert, then remove ',String(RtcString(IntToStr(i))),' items ...');
  Writeln;
  try
    FullSearch:=False;
    repeat
      st:=GetTickTime;
      TestBinList.Create(i,j,FullSearch,True).Free;
      Writeln;
      TestBinTree.Create(i,j,FullSearch,True).Free;
      Writeln;
      TestObjList.Create(i,j,FullSearch,True).Free;
      Writeln;
      TestStringIntList.Create(i,j,FullSearch,True).Free;
      Writeln;
      TestStringObjList.Create(i,j,FullSearch,True).Free;
      Writeln;

      TestStrList.Create(i,j,FullSearch,True).Free;
      Writeln;
      TestStrIntList.Create(i,j,FullSearch,True).Free;
      Writeln;
      TestStrObjList.Create(i,j,FullSearch,True).Free;
      Writeln;

    {$IFDEF IDE_2010up}
      TestSortListInt.Create(i,j,FullSearch,True).Free;
      Writeln;
      TestSortListStr.Create(i,j,FullSearch,True).Free;
      Writeln;
      TestSortTreeInt.Create(i,j,FullSearch,True).Free;
      Writeln;
      TestSortTreeStr.Create(i,j,FullSearch,True).Free;
      Writeln;
    {$ENDIF}

      Writeln('All Tests OK. Finished in '+IntToStr(GetTickTime-st)+' ms');
      if FullSearch then
        Break
      else
        begin
        FullSearch:=True;
        Writeln('Now, repeat all Tests with Full Check on every operation ...');
        Writeln;
        end;
      until False;
    Write('Hit <Enter> to close.');
  except
    on E:Exception do
      begin
      Writeln;
      Writeln('* '+E.ClassName+':');
      Writeln(E.Message);
      Writeln('Hit <Enter> key to terminate.');
      end;
    end;
  Readln;
end.
