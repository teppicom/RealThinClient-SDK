{
  "tStringObjList" Testing
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit TestmemStringObjList;

interface

{$INCLUDE rtcDefs.inc}

uses
  SysUtils,

  rtcTestCase,
  rtcSrcList;

const
  itemMin='';
  infoNil=NIL;

type
  itemType = tStringObjList_ItemType;
  infoType = tStringObjList_InfoType;

  TTestClass = tStringObjList;

  TestStringObjList = {$I mem\class.inc};

implementation

procedure TestStringObjList.SetReturnValue(a: cardinal);
  begin
  ReturnValue:=self; // need a pointer to an object, so we simply use ours in this test
  end;

// MyType = 1 (down), 2 (up), 3 (random)
function TestStringObjList.GetItemValue(a:cardinal):itemType;
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

constructor TestStringObjList.{$I mem\create.inc};

destructor TestStringObjList.{$I mem\destroy.inc};

procedure TestStringObjList.{$I mem\run.inc};

procedure TestStringObjList.{$I mem\insert_all.inc};

procedure TestStringObjList.{$I mem\check_empty.inc};

procedure TestStringObjList.{$I mem\search_all.inc};

procedure TestStringObjList.{$I mem\search_down.inc};

procedure TestStringObjList.{$I mem\search_up.inc};

procedure TestStringObjList.{$I mem\remove_all.inc};

procedure TestStringObjList.{$I mem\remove_all_down.inc};

procedure TestStringObjList.{$I mem\remove_search_up.inc};

procedure TestStringObjList.{$I mem\remove_search_down.inc};

procedure TestStringObjList.{$I mem\remove_all_up.inc};

end.
