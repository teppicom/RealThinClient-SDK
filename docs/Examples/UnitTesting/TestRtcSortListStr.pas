{
  "tBinList" Testing
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit TestRtcSortListStr;

interface

{$INCLUDE rtcDefs.inc}

uses
  SysUtils,
  rtcSrcList,
  TestRtcSortList;

type
  TestSortListStr = class(TestSortList<string,string>)
    constructor Create(TestSize,PoolSize:cardinal; full_search:boolean; run_now:boolean);
    procedure SetReturnValue(a: cardinal); override;
    function GetItemValue(a:cardinal):string; override;
    end;

implementation

constructor TestSortListStr.Create(TestSize,PoolSize:cardinal; full_search:boolean; run_now:boolean);
  begin
  inherited Create(TestSize,PoolSize,full_search,run_now,'','');
  end;

procedure TestSortListStr.SetReturnValue(a: cardinal);
  begin
  ReturnValue:=IntToStr(a);
  end;

// MyType = 1 (down), 2 (up), 3 (random)
function TestSortListStr.GetItemValue(a:cardinal):string;
  begin
  case MyType of
    1:Result:=IntToStr(a); // top-down
    2:Result:=IntToStr(MySize-a+1); // bottom-up
    else
      begin // random
      repeat
        Result := IntToStr(Random(1234567890));
        until FMyList.search(Result)=infoNil;
      end;
    end;
  end;

end.
