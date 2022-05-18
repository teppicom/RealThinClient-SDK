{
  "tBinList" Testing
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit TestRtcSortTree;

interface

{$INCLUDE rtcDefs.inc}

{$DEFINE RTC_GENERIC}
{$DEFINE RTC_TREE}

uses
  SysUtils,

  rtcTestCase,
  rtcSrcTree,
  Generics.Defaults;

type
  TestSortTree<itemType,infoType> = {$I mem\class.inc};

implementation

constructor TestSortTree<itemType,infoType>.{$I mem\create.inc};

destructor TestSortTree<itemType,infoType>.{$I mem\destroy.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\run.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\insert_all.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\check_empty.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\search_all.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\search_down.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\search_up.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\isearch_down.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\isearch_up.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\remove_all.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\remove_all_down.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\remove_search_up.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\remove_search_down.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\remove_isearch_up.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\remove_isearch_down.inc};

procedure TestSortTree<itemType,infoType>.{$I mem\remove_all_up.inc};

end.
