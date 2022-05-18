{
  "tObjList" Testing
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit TestmemObjList;

interface

{$INCLUDE rtcDefs.inc}

uses
  SysUtils,

  rtcTestCase,
  rtcSrcList;

const
  itemMin=0;
  infoNil=nil;

type
  itemType = tObjList_ItemType;
  infoType = tObjList_InfoType;

  TTestClass = tObjList;

  TestObjList = {$I mem\class.inc};

implementation

procedure TestObjList.SetReturnValue(a: cardinal);
  begin
  ReturnValue:=self; // we need an object, any object, so we will simply take the test case instanc
  end;

// MyType = 1 (down), 2 (up), 3 (random)
function TestObjList.GetItemValue(a:cardinal):itemType;
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

constructor TestObjList.{$I mem\create.inc};

destructor TestObjList.{$I mem\destroy.inc};

procedure TestObjList.{$I mem\run.inc};

procedure TestObjList.{$I mem\insert_all.inc};

procedure TestObjList.{$I mem\check_empty.inc};

procedure TestObjList.{$I mem\search_all.inc};

procedure TestObjList.{$I mem\search_down.inc};

procedure TestObjList.{$I mem\search_up.inc};

procedure TestObjList.{$I mem\remove_all.inc};

procedure TestObjList.{$I mem\remove_all_down.inc};

procedure TestObjList.{$I mem\remove_search_up.inc};

procedure TestObjList.{$I mem\remove_search_down.inc};

procedure TestObjList.{$I mem\remove_all_up.inc};

end.
