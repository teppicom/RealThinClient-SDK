{
  "tBinList" Testing
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit TestRtcSortListInt;

interface

{$INCLUDE rtcDefs.inc}

uses
  SysUtils,
  rtcSrcList,
  TestRtcSortList;

type
  TestSortListInt = class(TestSortList<integer,integer>)
    constructor Create(TestSize,PoolSize:cardinal; full_search:boolean; run_now:boolean);
    procedure SetReturnValue(a: cardinal); override;
    function GetItemValue(a:cardinal):integer; override;
    end;

implementation

constructor TestSortListInt.Create(TestSize,PoolSize:cardinal; full_search:boolean; run_now:boolean);
  begin
  inherited Create(TestSize,PoolSize,full_search,run_now,0,0);
  end;

procedure TestSortListInt.SetReturnValue(a: cardinal);
  begin
  ReturnValue:=a;
  end;

// MyType = 1 (down), 2 (up), 3 (random)
function TestSortListInt.GetItemValue(a:cardinal):integer;
  begin
  case MyType of
    1:Result:=a; // top-down
    2:Result:=MySize-a+1; // bottom-up
    else
      begin // random
      repeat
        Result := Random(1234567890);
        until FMyList.search(Result)=infoNil;
      end;
    end;
  end;

end.
