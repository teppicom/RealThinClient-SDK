{
  "tStrIntList" Testing
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit TestmemStrIntList;

interface

{$INCLUDE rtcDefs.inc}

uses
  SysUtils,

  rtcTypes,
  rtcTestCase,
  rtcSrcList;

const
  itemMin='';
  infoNil=-1;

type
  itemType = tStrIntList_ItemType;
  infoType = tStrIntList_InfoType;

  TTestClass = tStrIntList;

  TestStrIntList = {$I mem\class.inc};

implementation

procedure TestStrIntList.SetReturnValue(a: cardinal);
  begin
  ReturnValue:=a;
  end;

// MyType = 1 (down), 2 (up), 3 (random)
function TestStrIntList.GetItemValue(a:cardinal):itemType;
  begin
  case MyType of
    1:Result:=RtcString(IntToStr(a)); // top-down
    2:Result:=RtcString(IntToStr(MySize-a+1)); // bottom-up
    else
      begin // random
      repeat
        Result := RtcString(IntToStr(Random(1234567890)));
        until FMyList.search(Result)=infoNil;
      end;
    end;
  end;

constructor TestStrIntList.{$I mem\create.inc};

destructor TestStrIntList.{$I mem\destroy.inc};

procedure TestStrIntList.{$I mem\run.inc};

procedure TestStrIntList.{$I mem\insert_all.inc};

procedure TestStrIntList.{$I mem\check_empty.inc};

procedure TestStrIntList.{$I mem\search_all.inc};

procedure TestStrIntList.{$I mem\search_down.inc};

procedure TestStrIntList.{$I mem\search_up.inc};

procedure TestStrIntList.{$I mem\remove_all.inc};

procedure TestStrIntList.{$I mem\remove_all_down.inc};

procedure TestStrIntList.{$I mem\remove_search_up.inc};

procedure TestStrIntList.{$I mem\remove_search_down.inc};

procedure TestStrIntList.{$I mem\remove_all_up.inc};

end.
