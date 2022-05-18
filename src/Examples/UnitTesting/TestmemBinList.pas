{
  "tBinList" Testing
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit TestmemBinList;

interface

{$INCLUDE rtcDefs.inc}

uses
  SysUtils,

  rtcTestCase,
  rtcSrcList;

const
  itemMin=0;
  infoNil=0;
  
type
  itemType = tBinList_ItemType;
  infoType = tBinList_InfoType;

  TTestClass = tBinList;

  TestBinList = {$I mem\class.inc};

implementation

procedure TestBinList.SetReturnValue(a: cardinal);
  begin
  ReturnValue:=a;
  end;

// MyType = 1 (down), 2 (up), 3 (random)
function TestBinList.GetItemValue(a:cardinal):itemType;
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

constructor TestBinList.{$I mem\create.inc};

destructor TestBinList.{$I mem\destroy.inc};

procedure TestBinList.{$I mem\run.inc};

procedure TestBinList.{$I mem\insert_all.inc};

procedure TestBinList.{$I mem\check_empty.inc};

procedure TestBinList.{$I mem\search_all.inc};

procedure TestBinList.{$I mem\search_down.inc};

procedure TestBinList.{$I mem\search_up.inc};

procedure TestBinList.{$I mem\remove_all.inc};

procedure TestBinList.{$I mem\remove_all_down.inc};

procedure TestBinList.{$I mem\remove_search_up.inc};

procedure TestBinList.{$I mem\remove_search_down.inc};

procedure TestBinList.{$I mem\remove_all_up.inc};

end.
