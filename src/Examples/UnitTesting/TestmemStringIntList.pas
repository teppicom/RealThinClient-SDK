{
  "tStringIntList" Testing
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit TestmemStringIntList;

interface

{$INCLUDE rtcDefs.inc}

uses
  SysUtils,

  rtcTestCase,
  rtcSrcList;

const
  itemMin='';
  infoNil=-1;

type
  itemType = tStringIntList_ItemType;
  infoType = tStringIntList_InfoType;

  TTestClass = tStringIntList;

  TestStringIntList = {$I mem\class.inc};

implementation

procedure TestStringIntList.SetReturnValue(a: cardinal);
  begin
  ReturnValue:=a;
  end;

// MyType = 1 (down), 2 (up), 3 (random)
function TestStringIntList.GetItemValue(a:cardinal):itemType;
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

constructor TestStringIntList.{$I mem\create.inc};

destructor TestStringIntList.{$I mem\destroy.inc};

procedure TestStringIntList.{$I mem\run.inc};

procedure TestStringIntList.{$I mem\insert_all.inc};

procedure TestStringIntList.{$I mem\check_empty.inc};

procedure TestStringIntList.{$I mem\search_all.inc};

procedure TestStringIntList.{$I mem\search_down.inc};

procedure TestStringIntList.{$I mem\search_up.inc};

procedure TestStringIntList.{$I mem\remove_all.inc};

procedure TestStringIntList.{$I mem\remove_all_down.inc};

procedure TestStringIntList.{$I mem\remove_search_up.inc};

procedure TestStringIntList.{$I mem\remove_search_down.inc};

procedure TestStringIntList.{$I mem\remove_all_up.inc};

end.
