{
  "tBinTree" Testing
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit TestmemBinTree;

interface

{$INCLUDE rtcDefs.inc}

uses
  SysUtils,
  rtcTestCase,
  
  rtcSrcTree;

{$DEFINE RTC_TREE}

const
  itemMin=0;
  infoNil=0;

type
  itemType = tBinTree_ItemType;
  infoType = tBinTree_InfoType;

  TTestClass = tBinTree;

  TestBinTree = {$I mem\class.inc};

implementation

procedure TestBinTree.SetReturnValue(a: cardinal);
  begin
  ReturnValue:=a;
  end;

// MyType = 1 (down), 2 (up), 3 (random)
function TestBinTree.GetItemValue(a:cardinal):itemType;
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

constructor TestBinTree.{$I mem\create.inc};

destructor TestBinTree.{$I mem\destroy.inc};

procedure TestBinTree.{$I mem\run.inc};

procedure TestBinTree.{$I mem\insert_all.inc};

procedure TestBinTree.{$I mem\check_empty.inc};

procedure TestBinTree.{$I mem\search_all.inc};

procedure TestBinTree.{$I mem\search_down.inc};

procedure TestBinTree.{$I mem\search_up.inc};

procedure TestBinTree.{$I mem\isearch_down.inc};

procedure TestBinTree.{$I mem\isearch_up.inc};

procedure TestBinTree.{$I mem\remove_all.inc};

procedure TestBinTree.{$I mem\remove_all_down.inc};

procedure TestBinTree.{$I mem\remove_search_up.inc};

procedure TestBinTree.{$I mem\remove_search_down.inc};

procedure TestBinTree.{$I mem\remove_isearch_up.inc};

procedure TestBinTree.{$I mem\remove_isearch_down.inc};

procedure TestBinTree.{$I mem\remove_all_up.inc};

end.

