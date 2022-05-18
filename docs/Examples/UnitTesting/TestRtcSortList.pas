{
  "tBinList" Testing
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit TestRtcSortList;

interface

{$INCLUDE rtcDefs.inc}

{$DEFINE RTC_GENERIC}

uses
  SysUtils,

  rtcTestCase,
  rtcSrcList,
  Generics.Defaults;

type
  TestSortList<itemType,infoType> = {$I mem\class.inc};

implementation

constructor TestSortList<itemType,infoType>.{$I mem\create.inc};

destructor TestSortList<itemType,infoType>.{$I mem\destroy.inc};

procedure TestSortList<itemType,infoType>.{$I mem\run.inc};

procedure TestSortList<itemType,infoType>.{$I mem\insert_all.inc};

procedure TestSortList<itemType,infoType>.{$I mem\check_empty.inc};

procedure TestSortList<itemType,infoType>.{$I mem\search_all.inc};

procedure TestSortList<itemType,infoType>.{$I mem\search_down.inc};

procedure TestSortList<itemType,infoType>.{$I mem\search_up.inc};

procedure TestSortList<itemType,infoType>.{$I mem\remove_all.inc};

procedure TestSortList<itemType,infoType>.{$I mem\remove_all_down.inc};

procedure TestSortList<itemType,infoType>.{$I mem\remove_search_up.inc};

procedure TestSortList<itemType,infoType>.{$I mem\remove_search_down.inc};

procedure TestSortList<itemType,infoType>.{$I mem\remove_all_up.inc};

end.
