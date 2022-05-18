{
  "tStrObjList" Testing
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit TestmemStrObjList;

interface

{$INCLUDE rtcDefs.inc}

uses
  SysUtils,

  rtcTypes,
  rtcTestCase,
  rtcSrcList;

const
  itemMin='';
  infoNil=NIL;
  
type
  itemType = tStrObjList_ItemType;
  infoType = tStrObjList_InfoType;

  TTestClass = tStrObjList;

  TestStrObjList = {$I mem\class.inc};

implementation

procedure TestStrObjList.SetReturnValue(a: cardinal);
  begin
  ReturnValue:=self; // need a pointer to an object, so we simply use ours in this test
  end;

// MyType = 1 (down), 2 (up), 3 (random)
function TestStrObjList.GetItemValue(a:cardinal):itemType;
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

constructor TestStrObjList.{$I mem\create.inc};

destructor TestStrObjList.{$I mem\destroy.inc};

procedure TestStrObjList.{$I mem\run.inc};

procedure TestStrObjList.{$I mem\insert_all.inc};

procedure TestStrObjList.{$I mem\check_empty.inc};

procedure TestStrObjList.{$I mem\search_all.inc};

procedure TestStrObjList.{$I mem\search_down.inc};

procedure TestStrObjList.{$I mem\search_up.inc};

procedure TestStrObjList.{$I mem\remove_all.inc};

procedure TestStrObjList.{$I mem\remove_all_down.inc};

procedure TestStrObjList.{$I mem\remove_search_up.inc};

procedure TestStrObjList.{$I mem\remove_search_down.inc};

procedure TestStrObjList.{$I mem\remove_all_up.inc};

end.
