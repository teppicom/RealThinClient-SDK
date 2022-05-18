{
  @html(<b>)
  Fast Search Types
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  Type declarations required by Balanced Binary Search List and Tree classes
  (implemented in "rtcSrcList" and "rtcSrcTree" units).
}
unit rtcSrcTypes;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes;

type
  { Exception raised by all RTC "Search" classes }
  ERtcSearch = class(Exception);

type
  trtcItemType_CI32 = Longint;
  trtcInfoType_CI32 = Longint;
const
  vrtcItemMin_CI32 : trtcItemType_CI32 = 0;
  vrtcInfoNIL_CI32 : trtcInfoType_CI32 = 0;
type
  prtcNode_CI32=^trtcNode_CI32;
  trtcNode_CI32=record
    key:trtcItemType_CI32;
    info:trtcInfoType_CI32;
    l,r:prtcNode_CI32;
    b:boolean;
    end;
  prtcNodeArr_CI32=^trtcNodeArr_CI32;
  trtcNodeArr_CI32=array of trtcNode_CI32;
  prtcParentList_CI32=^trtcParentList_CI32;
  trtcParentList_CI32=record
    Nodes:array[0..100] of prtcNode_CI32;
    NodeCount:byte;
    end;

type
  trtcItemType_CI64 = int64;
  trtcInfoType_CI64 = int64;
const
  vrtcItemMin_CI64 : trtcItemType_CI64 = 0;
  vrtcInfoNIL_CI64 : trtcInfoType_CI64 = 0;
type
  prtcNode_CI64=^trtcNode_CI64;
  trtcNode_CI64=record
    key:trtcItemType_CI64;
    info:trtcInfoType_CI64;
    l,r:prtcNode_CI64;
    b:boolean;
    end;
  prtcNodeArr_CI64=^trtcNodeArr_CI64;
  trtcNodeArr_CI64=array of trtcNode_CI64;
  prtcParentList_CI64=^trtcParentList_CI64;
  trtcParentList_CI64=record
    Nodes:array[0..100] of prtcNode_CI64;
    NodeCount:byte;
    end;

type
  trtcInfoType_LI32 = Longint;
const
  vrtcInfoNIL_LI32:trtcInfoType_LI32 = 0;
type
  prtcNode_LI32=^trtcNode_LI32;
  trtcNode_LI32=record
    info:trtcInfoType_LI32;
    prior,
    next:prtcNode_LI32;
    end;
  prtcNodeArr_LI32=^trtcNodeArr_LI32;
  trtcNodeArr_LI32=array of trtcNode_LI32;

type
  trtcInfoType_LI64 = int64;
const
  vrtcInfoNIL_LI64:trtcInfoType_LI64 = 0;
type
  prtcNode_LI64=^trtcNode_LI64;
  trtcNode_LI64=record
    info:trtcInfoType_LI64;
    prior,
    next:prtcNode_LI64;
    end;
  prtcNodeArr_LI64=^trtcNodeArr_LI64;
  trtcNodeArr_LI64=array of trtcNode_LI64;

type
  trtcInfoType_LIPtr = RtcIntPtr;
const
  vrtcInfoNIL_LIPtr:trtcInfoType_LIPtr = 0;
type
  prtcNode_LIPtr=^trtcNode_LIPtr;
  trtcNode_LIPtr=record
    info:trtcInfoType_LIPtr;
    prior,
    next:prtcNode_LIPtr;
    end;
  prtcNodeArr_LIPtr=^trtcNodeArr_LIPtr;
  trtcNodeArr_LIPtr=array of trtcNode_LIPtr;

type
  trtcInfoType_LObj = TObject;
const
  vrtcInfoNIL_LObj:trtcInfoType_LObj = NIL;
type
  prtcNode_LObj=^trtcNode_LObj;
  trtcNode_LObj=record
    info:trtcInfoType_LObj;
    prior,
    next:prtcNode_LObj;
    end;
  prtcNodeArr_LObj=^trtcNodeArr_LObj;
  trtcNodeArr_LObj=array of trtcNode_LObj;

type
  trtcInfoType_LPtr = Pointer;
const
  vrtcInfoNIL_LPtr:trtcInfoType_LPtr = NIL;
type
  prtcNode_LPtr=^trtcNode_LPtr;
  trtcNode_LPtr=record
    info:trtcInfoType_LPtr;
    prior,
    next:prtcNode_LPtr;
    end;
  prtcNodeArr_LPtr=^trtcNodeArr_LPtr;
  trtcNodeArr_LPtr=array of trtcNode_LPtr;

type
  trtcInfoType_LStr = RtcString;
const
  vrtcInfoNIL_LStr:trtcInfoType_LStr = '';
type
  prtcNode_LStr=^trtcNode_LStr;
  trtcNode_LStr=record
    info:trtcInfoType_LStr;
    prior,
    next:prtcNode_LStr;
    end;
  prtcNodeArr_LStr=^trtcNodeArr_LStr;
  trtcNodeArr_LStr=array of trtcNode_LStr;

type
  trtcInfoType_LWStr = RtcWideString;
const
  vrtcInfoNIL_LWStr:trtcInfoType_LWStr = '';
type
  prtcNode_LWStr=^trtcNode_LWStr;
  trtcNode_LWStr=record
    info:trtcInfoType_LWStr;
    prior,
    next:prtcNode_LWStr;
    end;
  prtcNodeArr_LWStr=^trtcNodeArr_LWStr;
  trtcNodeArr_LWStr=array of trtcNode_LWStr;

type
  trtcItemType_I32I32 = Longint;
  trtcInfoType_I32I32 = Longint;
const
  vrtcItemMin_I32I32: trtcItemType_I32I32 = 0;
  vrtcInfoNIL_I32I32: trtcInfoType_I32I32 = 0;
type
  prtcNode_I32I32=^trtcNode_I32I32;
  prtcNode3_I32I32=^trtcNode3_I32I32;
  trtcNode_I32I32=record
    key:trtcItemType_I32I32;
    info:trtcInfoType_I32I32;
    l,r:prtcNode_I32I32;
    b:boolean;
    end;
  trtcNode3_I32I32=record
    key:trtcItemType_I32I32;
    info:trtcInfoType_I32I32;
    l,r:prtcNode3_I32I32;
    b:boolean;
    l2,r2:prtcNode3_I32I32;
    b2:boolean;
    end;
  prtcNodeArr_I32I32=^trtcNodeArr_I32I32;
  prtcNodeArr3_I32I32=^trtcNodeArr3_I32I32;
  trtcNodeArr_I32I32=array of trtcNode_I32I32;
  trtcNodeArr3_I32I32=array of trtcNode3_I32I32;
  prtcParentList_I32I32=^trtcParentList_I32I32;
  prtcParentList3_I32I32=^trtcParentList3_I32I32;
  trtcParentList_I32I32=record
    Nodes:array[0..100] of prtcNode_I32I32;
    NodeCount:byte;
    end;
  trtcParentList3_I32I32=record
    Nodes:array[0..100] of prtcNode3_I32I32;
    NodeCount:byte;
    end;

type
  trtcItemType_I32Obj = Longint;
  trtcInfoType_I32Obj = TObject;
  trtcInfoComp_I32Obj = RtcIntPtr;
const
  vrtcItemMin_I32Obj:trtcItemType_I32Obj = 0;
  vrtcInfoNIL_I32Obj:trtcInfoType_I32Obj = NIL;
type
  prtcNode_I32Obj=^trtcNode_I32Obj;
  prtcNode3_I32Obj=^trtcNode3_I32Obj;
  trtcNode_I32Obj=record
    key:trtcItemType_I32Obj;
    info:trtcInfoType_I32Obj;
    l,r:prtcNode_I32Obj;
    b:boolean;
    end;
  trtcNode3_I32Obj=record
    key:trtcItemType_I32Obj;
    info:trtcInfoType_I32Obj;
    l,r:prtcNode3_I32Obj;
    b:boolean;
    l2,r2:prtcNode3_I32Obj;
    b2:boolean;
    end;
  prtcNodeArr_I32Obj=^trtcNodeArr_I32Obj;
  prtcNodeArr3_I32Obj=^trtcNodeArr3_I32Obj;
  trtcNodeArr_I32Obj=array of trtcNode_I32Obj;
  trtcNodeArr3_I32Obj=array of trtcNode3_I32Obj;
  prtcParentList_I32Obj=^trtcParentList_I32Obj;
  prtcParentList3_I32Obj=^trtcParentList3_I32Obj;
  trtcParentList_I32Obj=record
    Nodes:array[0..100] of prtcNode_I32Obj;
    NodeCount:byte;
    end;
  trtcParentList3_I32Obj=record
    Nodes:array[0..100] of prtcNode3_I32Obj;
    NodeCount:byte;
    end;

type
  trtcItemType_I32Ptr = Longint;
  trtcInfoType_I32Ptr = Pointer;
  trtcInfoComp_I32Ptr = RtcIntPtr;
const
  vrtcItemMin_I32Ptr:trtcItemType_I32Ptr = 0;
  vrtcInfoNIL_I32Ptr:trtcInfoType_I32Ptr = NIL;
type
  prtcNode_I32Ptr=^trtcNode_I32Ptr;
  prtcNode3_I32Ptr=^trtcNode3_I32Ptr;
  trtcNode_I32Ptr=record
    key:trtcItemType_I32Ptr;
    info:trtcInfoType_I32Ptr;
    l,r:prtcNode_I32Ptr;
    b:boolean;
    end;
  trtcNode3_I32Ptr=record
    key:trtcItemType_I32Ptr;
    info:trtcInfoType_I32Ptr;
    l,r:prtcNode3_I32Ptr;
    b:boolean;
    l2,r2:prtcNode3_I32Ptr;
    b2:boolean;
    end;
  prtcNodeArr_I32Ptr=^trtcNodeArr_I32Ptr;
  prtcNodeArr3_I32Ptr=^trtcNodeArr3_I32Ptr;
  trtcNodeArr_I32Ptr=array of trtcNode_I32Ptr;
  trtcNodeArr3_I32Ptr=array of trtcNode3_I32Ptr;
  prtcParentList_I32Ptr=^trtcParentList_I32Ptr;
  prtcParentList3_I32Ptr=^trtcParentList3_I32Ptr;
  trtcParentList_I32Ptr=record
    Nodes:array[0..100] of prtcNode_I32Ptr;
    NodeCount:byte;
    end;
  trtcParentList3_I32Ptr=record
    Nodes:array[0..100] of prtcNode3_I32Ptr;
    NodeCount:byte;
    end;

type
  trtcItemType_I32Str = Longint;
  trtcInfoType_I32Str = RtcString;
const
  vrtcItemMin_I32Str: trtcItemType_I32Str = 0;
  vrtcInfoNIL_I32Str: trtcInfoType_I32Str = '';
type
  prtcNode_I32Str=^trtcNode_I32Str;
  prtcNode3_I32Str=^trtcNode3_I32Str;
  trtcNode_I32Str=record
    key:trtcItemType_I32Str;
    info:trtcInfoType_I32Str;
    l,r:prtcNode_I32Str;
    b:boolean;
    end;
  trtcNode3_I32Str=record
    key:trtcItemType_I32Str;
    info:trtcInfoType_I32Str;
    l,r:prtcNode3_I32Str;
    b:boolean;
    l2,r2:prtcNode3_I32Str;
    b2:boolean;
    end;
  prtcNodeArr_I32Str=^trtcNodeArr_I32Str;
  prtcNodeArr3_I32Str=^trtcNodeArr3_I32Str;
  trtcNodeArr_I32Str=array of trtcNode_I32Str;
  trtcNodeArr3_I32Str=array of trtcNode3_I32Str;
  prtcParentList_I32Str=^trtcParentList_I32Str;
  prtcParentList3_I32Str=^trtcParentList3_I32Str;
  trtcParentList_I32Str=record
    Nodes:array[0..100] of prtcNode_I32Str;
    NodeCount:byte;
    end;
  trtcParentList3_I32Str=record
    Nodes:array[0..100] of prtcNode3_I32Str;
    NodeCount:byte;
    end;

type
  trtcItemType_I32WStr = Longint;
  trtcInfoType_I32WStr = RtcWideString;
const
  vrtcItemMin_I32WStr: trtcItemType_I32WStr = 0;
  vrtcInfoNIL_I32WStr: trtcInfoType_I32WStr = '';
type
  prtcNode_I32WStr=^trtcNode_I32WStr;
  prtcNode3_I32WStr=^trtcNode3_I32WStr;
  trtcNode_I32WStr=record
    key:trtcItemType_I32WStr;
    info:trtcInfoType_I32WStr;
    l,r:prtcNode_I32WStr;
    b:boolean;
    end;
  trtcNode3_I32WStr=record
    key:trtcItemType_I32WStr;
    info:trtcInfoType_I32WStr;
    l,r:prtcNode3_I32WStr;
    b:boolean;
    l2,r2:prtcNode3_I32WStr;
    b2:boolean;
    end;
  prtcNodeArr_I32WStr=^trtcNodeArr_I32WStr;
  prtcNodeArr3_I32WStr=^trtcNodeArr3_I32WStr;
  trtcNodeArr_I32WStr=array of trtcNode_I32WStr;
  trtcNodeArr3_I32WStr=array of trtcNode3_I32WStr;
  prtcParentList_I32WStr=^trtcParentList_I32WStr;
  prtcParentList3_I32WStr=^trtcParentList3_I32WStr;
  trtcParentList_I32WStr=record
    Nodes:array[0..100] of prtcNode_I32WStr;
    NodeCount:byte;
    end;
  trtcParentList3_I32WStr=record
    Nodes:array[0..100] of prtcNode3_I32WStr;
    NodeCount:byte;
    end;

type
  trtcItemType_I64I64 = int64;
  trtcInfoType_I64I64 = int64;
const
  vrtcItemMin_I64I64: trtcItemType_I64I64 = 0;
  vrtcInfoNIL_I64I64: trtcInfoType_I64I64 = 0;
type
  prtcNode_I64I64=^trtcNode_I64I64;
  prtcNode3_I64I64=^trtcNode3_I64I64;
  trtcNode_I64I64=record
    key:trtcItemType_I64I64;
    info:trtcInfoType_I64I64;
    l,r:prtcNode_I64I64;
    b:boolean;
    end;
  trtcNode3_I64I64=record
    key:trtcItemType_I64I64;
    info:trtcInfoType_I64I64;
    l,r:prtcNode3_I64I64;
    b:boolean;
    l2,r2:prtcNode3_I64I64;
    b2:boolean;
    end;
  prtcNodeArr_I64I64=^trtcNodeArr_I64I64;
  prtcNodeArr3_I64I64=^trtcNodeArr3_I64I64;
  trtcNodeArr_I64I64=array of trtcNode_I64I64;
  trtcNodeArr3_I64I64=array of trtcNode3_I64I64;
  prtcParentList_I64I64=^trtcParentList_I64I64;
  prtcParentList3_I64I64=^trtcParentList3_I64I64;
  trtcParentList_I64I64=record
    Nodes:array[0..100] of prtcNode_I64I64;
    NodeCount:byte;
    end;
  trtcParentList3_I64I64=record
    Nodes:array[0..100] of prtcNode3_I64I64;
    NodeCount:byte;
    end;

type
  trtcItemType_I64Obj = int64;
  trtcInfoType_I64Obj = TObject;
  trtcInfoComp_I64Obj = RtcIntPtr;
const
  vrtcItemMin_I64Obj:trtcItemType_I64Obj = 0;
  vrtcInfoNIL_I64Obj:trtcInfoType_I64Obj = NIL;
type
  prtcNode_I64Obj=^trtcNode_I64Obj;
  prtcNode3_I64Obj=^trtcNode3_I64Obj;
  trtcNode_I64Obj=record
    key:trtcItemType_I64Obj;
    info:trtcInfoType_I64Obj;
    l,r:prtcNode_I64Obj;
    b:boolean;
    end;
  trtcNode3_I64Obj=record
    key:trtcItemType_I64Obj;
    info:trtcInfoType_I64Obj;
    l,r:prtcNode3_I64Obj;
    b:boolean;
    l2,r2:prtcNode3_I64Obj;
    b2:boolean;
    end;
  prtcNodeArr_I64Obj=^trtcNodeArr_I64Obj;
  prtcNodeArr3_I64Obj=^trtcNodeArr3_I64Obj;
  trtcNodeArr_I64Obj=array of trtcNode_I64Obj;
  trtcNodeArr3_I64Obj=array of trtcNode3_I64Obj;
  prtcParentList_I64Obj=^trtcParentList_I64Obj;
  prtcParentList3_I64Obj=^trtcParentList3_I64Obj;
  trtcParentList_I64Obj=record
    Nodes:array[0..100] of prtcNode_I64Obj;
    NodeCount:byte;
    end;
  trtcParentList3_I64Obj=record
    Nodes:array[0..100] of prtcNode3_I64Obj;
    NodeCount:byte;
    end;

type
  trtcItemType_I64Ptr = int64;
  trtcInfoType_I64Ptr = Pointer;
  trtcInfoComp_I64Ptr = RtcIntPtr;
const
  vrtcItemMin_I64Ptr:trtcItemType_I64Ptr = 0;
  vrtcInfoNIL_I64Ptr:trtcInfoType_I64Ptr = NIL;
type
  prtcNode_I64Ptr=^trtcNode_I64Ptr;
  prtcNode3_I64Ptr=^trtcNode3_I64Ptr;
  trtcNode_I64Ptr=record
    key:trtcItemType_I64Ptr;
    info:trtcInfoType_I64Ptr;
    l,r:prtcNode_I64Ptr;
    b:boolean;
    end;
  trtcNode3_I64Ptr=record
    key:trtcItemType_I64Ptr;
    info:trtcInfoType_I64Ptr;
    l,r:prtcNode3_I64Ptr;
    b:boolean;
    l2,r2:prtcNode3_I64Ptr;
    b2:boolean;
    end;
  prtcNodeArr_I64Ptr=^trtcNodeArr_I64Ptr;
  prtcNodeArr3_I64Ptr=^trtcNodeArr3_I64Ptr;
  trtcNodeArr_I64Ptr=array of trtcNode_I64Ptr;
  trtcNodeArr3_I64Ptr=array of trtcNode3_I64Ptr;
  prtcParentList_I64Ptr=^trtcParentList_I64Ptr;
  prtcParentList3_I64Ptr=^trtcParentList3_I64Ptr;
  trtcParentList_I64Ptr=record
    Nodes:array[0..100] of prtcNode_I64Ptr;
    NodeCount:byte;
    end;
  trtcParentList3_I64Ptr=record
    Nodes:array[0..100] of prtcNode3_I64Ptr;
    NodeCount:byte;
    end;

type
  trtcItemType_I64Str = int64;
  trtcInfoType_I64Str = RtcString; 
const
  vrtcItemMin_I64Str: trtcItemType_I64Str = 0;
  vrtcInfoNIL_I64Str: trtcInfoType_I64Str = '';
type
  prtcNode_I64Str=^trtcNode_I64Str;
  prtcNode3_I64Str=^trtcNode3_I64Str;
  trtcNode_I64Str=record
    key:trtcItemType_I64Str;
    info:trtcInfoType_I64Str;
    l,r:prtcNode_I64Str;
    b:boolean;
    end;
  trtcNode3_I64Str=record
    key:trtcItemType_I64Str;
    info:trtcInfoType_I64Str;
    l,r:prtcNode3_I64Str;
    b:boolean;
    l2,r2:prtcNode3_I64Str;
    b2:boolean;
    end;
  prtcNodeArr_I64Str=^trtcNodeArr_I64Str;
  prtcNodeArr3_I64Str=^trtcNodeArr3_I64Str;
  trtcNodeArr_I64Str=array of trtcNode_I64Str;
  trtcNodeArr3_I64Str=array of trtcNode3_I64Str;
  prtcParentList_I64Str=^trtcParentList_I64Str;
  prtcParentList3_I64Str=^trtcParentList3_I64Str;
  trtcParentList_I64Str=record
    Nodes:array[0..100] of prtcNode_I64Str;
    NodeCount:byte;
    end;
  trtcParentList3_I64Str=record
    Nodes:array[0..100] of prtcNode3_I64Str;
    NodeCount:byte;
    end;

type
  trtcItemType_I64WStr = int64;
  trtcInfoType_I64WStr = RtcWideString;
const
  vrtcItemMin_I64WStr: trtcItemType_I64WStr = 0;
  vrtcInfoNIL_I64WStr: trtcInfoType_I64WStr = '';
type
  prtcNode_I64WStr=^trtcNode_I64WStr;
  prtcNode3_I64WStr=^trtcNode3_I64WStr;
  trtcNode_I64WStr=record
    key:trtcItemType_I64WStr;
    info:trtcInfoType_I64WStr;
    l,r:prtcNode_I64WStr;
    b:boolean;
    end;
  trtcNode3_I64WStr=record
    key:trtcItemType_I64WStr;
    info:trtcInfoType_I64WStr;
    l,r:prtcNode3_I64WStr;
    b:boolean;
    l2,r2:prtcNode3_I64WStr;
    b2:boolean;
    end;
  prtcNodeArr_I64WStr=^trtcNodeArr_I64WStr;
  prtcNodeArr3_I64WStr=^trtcNodeArr3_I64WStr;
  trtcNodeArr_I64WStr=array of trtcNode_I64WStr;
  trtcNodeArr3_I64WStr=array of trtcNode3_I64WStr;
  prtcParentList_I64WStr=^trtcParentList_I64WStr;
  prtcParentList3_I64WStr=^trtcParentList3_I64WStr;
  trtcParentList_I64WStr=record
    Nodes:array[0..100] of prtcNode_I64WStr;
    NodeCount:byte;
    end;
  trtcParentList3_I64WStr=record
    Nodes:array[0..100] of prtcNode3_I64WStr;
    NodeCount:byte;
    end;

type
  trtcItemType_IPtrIPtr = RtcIntPtr;
  trtcInfoType_IPtrIPtr = RtcIntPtr;
const
  vrtcItemMin_IPtrIPtr: trtcItemType_IPtrIPtr = 0;
  vrtcInfoNIL_IPtrIPtr: trtcInfoType_IPtrIPtr = 0;
type
  prtcNode_IPtrIPtr=^trtcNode_IPtrIPtr;
  prtcNode3_IPtrIPtr=^trtcNode3_IPtrIPtr;
  trtcNode_IPtrIPtr=record
    key:trtcItemType_IPtrIPtr;
    info:trtcInfoType_IPtrIPtr;
    l,r:prtcNode_IPtrIPtr;
    b:boolean;
    end;
  trtcNode3_IPtrIPtr=record
    key:trtcItemType_IPtrIPtr;
    info:trtcInfoType_IPtrIPtr;
    l,r:prtcNode3_IPtrIPtr;
    b:boolean;
    l2,r2:prtcNode3_IPtrIPtr;
    b2:boolean;
    end;
  prtcNodeArr_IPtrIPtr=^trtcNodeArr_IPtrIPtr;
  prtcNodeArr3_IPtrIPtr=^trtcNodeArr3_IPtrIPtr;
  trtcNodeArr_IPtrIPtr=array of trtcNode_IPtrIPtr;
  trtcNodeArr3_IPtrIPtr=array of trtcNode3_IPtrIPtr;
  prtcParentList_IPtrIPtr=^trtcParentList_IPtrIPtr;
  prtcParentList3_IPtrIPtr=^trtcParentList3_IPtrIPtr;
  trtcParentList_IPtrIPtr=record
    Nodes:array[0..100] of prtcNode_IPtrIPtr;
    NodeCount:byte;
    end;
  trtcParentList3_IPtrIPtr=record
    Nodes:array[0..100] of prtcNode3_IPtrIPtr;
    NodeCount:byte;
    end;

type
  trtcItemType_IPtrObj = RtcIntPtr;
  trtcInfoType_IPtrObj = TObject;
  trtcInfoComp_IPtrObj = RtcIntPtr;
const
  vrtcItemMin_IPtrObj:trtcItemType_IPtrObj = 0;
  vrtcInfoNIL_IPtrObj:trtcInfoType_IPtrObj = NIL;
type
  prtcNode_IPtrObj=^trtcNode_IPtrObj;
  prtcNode3_IPtrObj=^trtcNode3_IPtrObj;
  trtcNode_IPtrObj=record
    key:trtcItemType_IPtrObj;
    info:trtcInfoType_IPtrObj;
    l,r:prtcNode_IPtrObj;
    b:boolean;
    end;
  trtcNode3_IPtrObj=record
    key:trtcItemType_IPtrObj;
    info:trtcInfoType_IPtrObj;
    l,r:prtcNode3_IPtrObj;
    b:boolean;
    l2,r2:prtcNode3_IPtrObj;
    b2:boolean;
    end;
  prtcNodeArr_IPtrObj=^trtcNodeArr_IPtrObj;
  prtcNodeArr3_IPtrObj=^trtcNodeArr3_IPtrObj;
  trtcNodeArr_IPtrObj=array of trtcNode_IPtrObj;
  trtcNodeArr3_IPtrObj=array of trtcNode3_IPtrObj;
  prtcParentList_IPtrObj=^trtcParentList_IPtrObj;
  prtcParentList3_IPtrObj=^trtcParentList3_IPtrObj;
  trtcParentList_IPtrObj=record
    Nodes:array[0..100] of prtcNode_IPtrObj;
    NodeCount:byte;
    end;
  trtcParentList3_IPtrObj=record
    Nodes:array[0..100] of prtcNode3_IPtrObj;
    NodeCount:byte;
    end;

type
  trtcItemType_IPtrPtr = RtcIntPtr;
  trtcInfoType_IPtrPtr = Pointer;
  trtcInfoComp_IPtrPtr = RtcIntPtr;
const
  vrtcItemMin_IPtrPtr:trtcItemType_IPtrPtr = 0;
  vrtcInfoNIL_IPtrPtr:trtcInfoType_IPtrPtr = NIL;
type
  prtcNode_IPtrPtr=^trtcNode_IPtrPtr;
  prtcNode3_IPtrPtr=^trtcNode3_IPtrPtr;
  trtcNode_IPtrPtr=record
    key:trtcItemType_IPtrPtr;
    info:trtcInfoType_IPtrPtr;
    l,r:prtcNode_IPtrPtr;
    b:boolean;
    end;
  trtcNode3_IPtrPtr=record
    key:trtcItemType_IPtrPtr;
    info:trtcInfoType_IPtrPtr;
    l,r:prtcNode3_IPtrPtr;
    b:boolean;
    l2,r2:prtcNode3_IPtrPtr;
    b2:boolean;
    end;
  prtcNodeArr_IPtrPtr=^trtcNodeArr_IPtrPtr;
  prtcNodeArr3_IPtrPtr=^trtcNodeArr3_IPtrPtr;
  trtcNodeArr_IPtrPtr=array of trtcNode_IPtrPtr;
  trtcNodeArr3_IPtrPtr=array of trtcNode3_IPtrPtr;
  prtcParentList_IPtrPtr=^trtcParentList_IPtrPtr;
  prtcParentList3_IPtrPtr=^trtcParentList3_IPtrPtr;
  trtcParentList_IPtrPtr=record
    Nodes:array[0..100] of prtcNode_IPtrPtr;
    NodeCount:byte;
    end;
  trtcParentList3_IPtrPtr=record
    Nodes:array[0..100] of prtcNode3_IPtrPtr;
    NodeCount:byte;
    end;

type
  trtcItemType_IPtrStr = RtcIntPtr;
  trtcInfoType_IPtrStr = RtcString;
const
  vrtcItemMin_IPtrStr: trtcItemType_IPtrStr = 0;
  vrtcInfoNIL_IPtrStr: trtcInfoType_IPtrStr = '';
type
  prtcNode_IPtrStr=^trtcNode_IPtrStr;
  prtcNode3_IPtrStr=^trtcNode3_IPtrStr;
  trtcNode_IPtrStr=record
    key:trtcItemType_IPtrStr;
    info:trtcInfoType_IPtrStr;
    l,r:prtcNode_IPtrStr;
    b:boolean;
    end;
  trtcNode3_IPtrStr=record
    key:trtcItemType_IPtrStr;
    info:trtcInfoType_IPtrStr;
    l,r:prtcNode3_IPtrStr;
    b:boolean;
    l2,r2:prtcNode3_IPtrStr;
    b2:boolean;
    end;
  prtcNodeArr_IPtrStr=^trtcNodeArr_IPtrStr;
  prtcNodeArr3_IPtrStr=^trtcNodeArr3_IPtrStr;
  trtcNodeArr_IPtrStr=array of trtcNode_IPtrStr;
  trtcNodeArr3_IPtrStr=array of trtcNode3_IPtrStr;
  prtcParentList_IPtrStr=^trtcParentList_IPtrStr;
  prtcParentList3_IPtrStr=^trtcParentList3_IPtrStr;
  trtcParentList_IPtrStr=record
    Nodes:array[0..100] of prtcNode_IPtrStr;
    NodeCount:byte;
    end;
  trtcParentList3_IPtrStr=record
    Nodes:array[0..100] of prtcNode3_IPtrStr;
    NodeCount:byte;
    end;

type
  trtcItemType_IPtrWStr = RtcIntPtr;
  trtcInfoType_IPtrWStr = RtcWideString;
const
  vrtcItemMin_IPtrWStr: trtcItemType_IPtrWStr = 0;
  vrtcInfoNIL_IPtrWStr: trtcInfoType_IPtrWStr = '';
type
  prtcNode_IPtrWStr=^trtcNode_IPtrWStr;
  prtcNode3_IPtrWStr=^trtcNode3_IPtrWStr;
  trtcNode_IPtrWStr=record
    key:trtcItemType_IPtrWStr;
    info:trtcInfoType_IPtrWStr;
    l,r:prtcNode_IPtrWStr;
    b:boolean;
    end;
  trtcNode3_IPtrWStr=record
    key:trtcItemType_IPtrWStr;
    info:trtcInfoType_IPtrWStr;
    l,r:prtcNode3_IPtrWStr;
    b:boolean;
    l2,r2:prtcNode3_IPtrWStr;
    b2:boolean;
    end;
  prtcNodeArr_IPtrWStr=^trtcNodeArr_IPtrWStr;
  prtcNodeArr3_IPtrWStr=^trtcNodeArr3_IPtrWStr;
  trtcNodeArr_IPtrWStr=array of trtcNode_IPtrWStr;
  trtcNodeArr3_IPtrWStr=array of trtcNode3_IPtrWStr;
  prtcParentList_IPtrWStr=^trtcParentList_IPtrWStr;
  prtcParentList3_IPtrWStr=^trtcParentList3_IPtrWStr;
  trtcParentList_IPtrWStr=record
    Nodes:array[0..100] of prtcNode_IPtrWStr;
    NodeCount:byte;
    end;
  trtcParentList3_IPtrWStr=record
    Nodes:array[0..100] of prtcNode3_IPtrWStr;
    NodeCount:byte;
    end;

type
  trtcItemType_StrI32 = RtcString;
  trtcInfoType_StrI32 = Longint;
const
  vrtcItemMin_StrI32: trtcItemType_StrI32 = '';
  vrtcInfoNIL_StrI32: trtcInfoType_StrI32 = -1;
type
  prtcNode_StrI32=^trtcNode_StrI32;
  prtcNode3_StrI32=^trtcNode3_StrI32;
  trtcNode_StrI32=record
    key:trtcItemType_StrI32;
    info:trtcInfoType_StrI32;
    l,r:prtcNode_StrI32;
    b:boolean;
    end;
  trtcNode3_StrI32=record
    key:trtcItemType_StrI32;
    info:trtcInfoType_StrI32;
    l,r:prtcNode3_StrI32;
    b:boolean;
    l2,r2:prtcNode3_StrI32;
    b2:boolean;
    end;
  prtcNodeArr_StrI32=^trtcNodeArr_StrI32;
  prtcNodeArr3_StrI32=^trtcNodeArr3_StrI32;
  trtcNodeArr_StrI32=array of trtcNode_StrI32;
  trtcNodeArr3_StrI32=array of trtcNode3_StrI32;
  prtcParentList_StrI32=^trtcParentList_StrI32;
  prtcParentList3_StrI32=^trtcParentList3_StrI32;
  trtcParentList_StrI32=record
    Nodes:array[0..100] of prtcNode_StrI32;
    NodeCount:byte;
    end;
  trtcParentList3_StrI32=record
    Nodes:array[0..100] of prtcNode3_StrI32;
    NodeCount:byte;
    end;

type
  trtcItemType_StrI64 = RtcString;
  trtcInfoType_StrI64 = int64;
const
  vrtcItemMin_StrI64: trtcItemType_StrI64 = '';
  vrtcInfoNIL_StrI64: trtcInfoType_StrI64 = -1;
type
  prtcNode_StrI64=^trtcNode_StrI64;
  prtcNode3_StrI64=^trtcNode3_StrI64;
  trtcNode_StrI64=record
    key:trtcItemType_StrI64;
    info:trtcInfoType_StrI64;
    l,r:prtcNode_StrI64;
    b:boolean;
    end;
  trtcNode3_StrI64=record
    key:trtcItemType_StrI64;
    info:trtcInfoType_StrI64;
    l,r:prtcNode3_StrI64;
    b:boolean;
    l2,r2:prtcNode3_StrI64;
    b2:boolean;
    end;
  prtcNodeArr_StrI64=^trtcNodeArr_StrI64;
  prtcNodeArr3_StrI64=^trtcNodeArr3_StrI64;
  trtcNodeArr_StrI64=array of trtcNode_StrI64;
  trtcNodeArr3_StrI64=array of trtcNode3_StrI64;
  prtcParentList_StrI64=^trtcParentList_StrI64;
  prtcParentList3_StrI64=^trtcParentList3_StrI64;
  trtcParentList_StrI64=record
    Nodes:array[0..100] of prtcNode_StrI64;
    NodeCount:byte;
    end;
  trtcParentList3_StrI64=record
    Nodes:array[0..100] of prtcNode3_StrI64;
    NodeCount:byte;
    end;

type
  trtcItemType_StrObj = RtcString;
  trtcInfoType_StrObj = TObject;
  trtcInfoComp_StrObj = RtcIntPtr;
const
  vrtcItemMin_StrObj:trtcItemType_StrObj = '';
  vrtcInfoNIL_StrObj:trtcInfoType_StrObj = NIL;
type
  prtcNode_StrObj=^trtcNode_StrObj;
  prtcNode3_StrObj=^trtcNode3_StrObj;
  trtcNode_StrObj=record
    key:trtcItemType_StrObj;
    info:trtcInfoType_StrObj;
    l,r:prtcNode_StrObj;
    b:boolean;
    end;
  trtcNode3_StrObj=record
    key:trtcItemType_StrObj;
    info:trtcInfoType_StrObj;
    l,r:prtcNode3_StrObj;
    b:boolean;
    l2,r2:prtcNode3_StrObj;
    b2:boolean;
    end;
  prtcNodeArr_StrObj=^trtcNodeArr_StrObj;
  prtcNodeArr3_StrObj=^trtcNodeArr3_StrObj;
  trtcNodeArr_StrObj=array of trtcNode_StrObj;
  trtcNodeArr3_StrObj=array of trtcNode3_StrObj;
  prtcParentList_StrObj=^trtcParentList_StrObj;
  prtcParentList3_StrObj=^trtcParentList3_StrObj;
  trtcParentList_StrObj=record
    Nodes:array[0..100] of prtcNode_StrObj;
    NodeCount:byte;
    end;
  trtcParentList3_StrObj=record
    Nodes:array[0..100] of prtcNode3_StrObj;
    NodeCount:byte;
    end;

type
  trtcItemType_StrPtr = RtcString;
  trtcInfoType_StrPtr = Pointer;
  trtcInfoComp_StrPtr = RtcIntPtr;
const
  vrtcItemMin_StrPtr:trtcItemType_StrPtr = '';
  vrtcInfoNIL_StrPtr:trtcInfoType_StrPtr = NIL;
type
  prtcNode_StrPtr=^trtcNode_StrPtr;
  prtcNode3_StrPtr=^trtcNode3_StrPtr;
  trtcNode_StrPtr=record
    key:trtcItemType_StrPtr;
    info:trtcInfoType_StrPtr;
    l,r:prtcNode_StrPtr;
    b:boolean;
    end;
  trtcNode3_StrPtr=record
    key:trtcItemType_StrPtr;
    info:trtcInfoType_StrPtr;
    l,r:prtcNode3_StrPtr;
    b:boolean;
    l2,r2:prtcNode3_StrPtr;
    b2:boolean;
    end;
  prtcNodeArr_StrPtr=^trtcNodeArr_StrPtr;
  prtcNodeArr3_StrPtr=^trtcNodeArr3_StrPtr;
  trtcNodeArr_StrPtr=array of trtcNode_StrPtr;
  trtcNodeArr3_StrPtr=array of trtcNode3_StrPtr;
  prtcParentList_StrPtr=^trtcParentList_StrPtr;
  prtcParentList3_StrPtr=^trtcParentList3_StrPtr;
  trtcParentList_StrPtr=record
    Nodes:array[0..100] of prtcNode_StrPtr;
    NodeCount:byte;
    end;
  trtcParentList3_StrPtr=record
    Nodes:array[0..100] of prtcNode3_StrPtr;
    NodeCount:byte;
    end;

type
  trtcItemType_StrStr = RtcString;
  trtcInfoType_StrStr = RtcString;
const
  vrtcItemMin_StrStr: trtcItemType_StrStr = '';
  vrtcInfoNIL_StrStr: trtcInfoType_StrStr = '';
type
  prtcNode_StrStr=^trtcNode_StrStr;
  prtcNode3_StrStr=^trtcNode3_StrStr;
  trtcNode_StrStr=record
    key:trtcItemType_StrStr;
    info:trtcInfoType_StrStr;
    l,r:prtcNode_StrStr;
    b:boolean;
    end;
  trtcNode3_StrStr=record
    key:trtcItemType_StrStr;
    info:trtcInfoType_StrStr;
    l,r:prtcNode3_StrStr;
    b:boolean;
    l2,r2:prtcNode3_StrStr;
    b2:boolean;
    end;
  prtcNodeArr_StrStr=^trtcNodeArr_StrStr;
  prtcNodeArr3_StrStr=^trtcNodeArr3_StrStr;
  trtcNodeArr_StrStr=array of trtcNode_StrStr;
  trtcNodeArr3_StrStr=array of trtcNode3_StrStr;
  prtcParentList_StrStr=^trtcParentList_StrStr;
  prtcParentList3_StrStr=^trtcParentList3_StrStr;
  trtcParentList_StrStr=record
    Nodes:array[0..100] of prtcNode_StrStr;
    NodeCount:byte;
    end;
  trtcParentList3_StrStr=record
    Nodes:array[0..100] of prtcNode3_StrStr;
    NodeCount:byte;
    end;

type
  trtcItemType_WStrI32 = RtcWideString;
  trtcInfoType_WStrI32 = Longint;
const
  vrtcItemMin_WStrI32: trtcItemType_WStrI32 = '';
  vrtcInfoNIL_WStrI32: trtcInfoType_WStrI32 = -1;
type
  prtcNode_WStrI32=^trtcNode_WStrI32;
  prtcNode3_WStrI32=^trtcNode3_WStrI32;
  trtcNode_WStrI32=record
    key:trtcItemType_WStrI32;
    info:trtcInfoType_WStrI32;
    l,r:prtcNode_WStrI32;
    b:boolean;
    end;
  trtcNode3_WStrI32=record
    key:trtcItemType_WStrI32;
    info:trtcInfoType_WStrI32;
    l,r:prtcNode3_WStrI32;
    b:boolean;
    l2,r2:prtcNode3_WStrI32;
    b2:boolean;
    end;
  prtcNodeArr_WStrI32=^trtcNodeArr_WStrI32;
  prtcNodeArr3_WStrI32=^trtcNodeArr3_WStrI32;
  trtcNodeArr_WStrI32=array of trtcNode_WStrI32;
  trtcNodeArr3_WStrI32=array of trtcNode3_WStrI32;
  prtcParentList_WStrI32=^trtcParentList_WStrI32;
  prtcParentList3_WStrI32=^trtcParentList3_WStrI32;
  trtcParentList_WStrI32=record
    Nodes:array[0..100] of prtcNode_WStrI32;
    NodeCount:byte;
    end;
  trtcParentList3_WStrI32=record
    Nodes:array[0..100] of prtcNode3_WStrI32;
    NodeCount:byte;
    end;

type
  trtcItemType_WStrI64 = RtcWideString;
  trtcInfoType_WStrI64 = int64;
const
  vrtcItemMin_WStrI64: trtcItemType_WStrI64 = '';
  vrtcInfoNIL_WStrI64: trtcInfoType_WStrI64 = -1;
type
  prtcNode_WStrI64=^trtcNode_WStrI64;
  prtcNode3_WStrI64=^trtcNode3_WStrI64;
  trtcNode_WStrI64=record
    key:trtcItemType_WStrI64;
    info:trtcInfoType_WStrI64;
    l,r:prtcNode_WStrI64;
    b:boolean;
    end;
  trtcNode3_WStrI64=record
    key:trtcItemType_WStrI64;
    info:trtcInfoType_WStrI64;
    l,r:prtcNode3_WStrI64;
    b:boolean;
    l2,r2:prtcNode3_WStrI64;
    b2:boolean;
    end;
  prtcNodeArr_WStrI64=^trtcNodeArr_WStrI64;
  prtcNodeArr3_WStrI64=^trtcNodeArr3_WStrI64;
  trtcNodeArr_WStrI64=array of trtcNode_WStrI64;
  trtcNodeArr3_WStrI64=array of trtcNode3_WStrI64;
  prtcParentList_WStrI64=^trtcParentList_WStrI64;
  prtcParentList3_WStrI64=^trtcParentList3_WStrI64;
  trtcParentList_WStrI64=record
    Nodes:array[0..100] of prtcNode_WStrI64;
    NodeCount:byte;
    end;
  trtcParentList3_WStrI64=record
    Nodes:array[0..100] of prtcNode3_WStrI64;
    NodeCount:byte;
    end;

type
  trtcItemType_WStrObj = RtcWideString;
  trtcInfoType_WStrObj = TObject;
  trtcInfoComp_WStrObj = RtcIntPtr;
const
  vrtcItemMin_WStrObj:trtcItemType_WStrObj = '';
  vrtcInfoNIL_WStrObj:trtcInfoType_WStrObj = NIL;
type
  prtcNode_WStrObj=^trtcNode_WStrObj;
  prtcNode3_WStrObj=^trtcNode3_WStrObj;
  trtcNode_WStrObj=record
    key:trtcItemType_WStrObj;
    info:trtcInfoType_WStrObj;
    l,r:prtcNode_WStrObj;
    b:boolean;
    end;
  trtcNode3_WStrObj=record
    key:trtcItemType_WStrObj;
    info:trtcInfoType_WStrObj;
    l,r:prtcNode3_WStrObj;
    b:boolean;
    l2,r2:prtcNode3_WStrObj;
    b2:boolean;
    end;
  prtcNodeArr_WStrObj=^trtcNodeArr_WStrObj;
  prtcNodeArr3_WStrObj=^trtcNodeArr3_WStrObj;
  trtcNodeArr_WStrObj=array of trtcNode_WStrObj;
  trtcNodeArr3_WStrObj=array of trtcNode3_WStrObj;
  prtcParentList_WStrObj=^trtcParentList_WStrObj;
  prtcParentList3_WStrObj=^trtcParentList3_WStrObj;
  trtcParentList_WStrObj=record
    Nodes:array[0..100] of prtcNode_WStrObj;
    NodeCount:byte;
    end;
  trtcParentList3_WStrObj=record
    Nodes:array[0..100] of prtcNode3_WStrObj;
    NodeCount:byte;
    end;

type
  trtcItemType_WStrPtr = RtcWideString;
  trtcInfoType_WStrPtr = Pointer;
  trtcInfoComp_WStrPtr = RtcIntPtr;
const
  vrtcItemMin_WStrPtr:trtcItemType_WStrPtr = '';
  vrtcInfoNIL_WStrPtr:trtcInfoType_WStrPtr = NIL;
type
  prtcNode_WStrPtr=^trtcNode_WStrPtr;
  prtcNode3_WStrPtr=^trtcNode3_WStrPtr;
  trtcNode_WStrPtr=record
    key:trtcItemType_WStrPtr;
    info:trtcInfoType_WStrPtr;
    l,r:prtcNode_WStrPtr;
    b:boolean;
    end;
  trtcNode3_WStrPtr=record
    key:trtcItemType_WStrPtr;
    info:trtcInfoType_WStrPtr;
    l,r:prtcNode3_WStrPtr;
    b:boolean;
    l2,r2:prtcNode3_WStrPtr;
    b2:boolean;
    end;
  prtcNodeArr_WStrPtr=^trtcNodeArr_WStrPtr;
  prtcNodeArr3_WStrPtr=^trtcNodeArr3_WStrPtr;
  trtcNodeArr_WStrPtr=array of trtcNode_WStrPtr;
  trtcNodeArr3_WStrPtr=array of trtcNode3_WStrPtr;
  prtcParentList_WStrPtr=^trtcParentList_WStrPtr;
  prtcParentList3_WStrPtr=^trtcParentList3_WStrPtr;
  trtcParentList_WStrPtr=record
    Nodes:array[0..100] of prtcNode_WStrPtr;
    NodeCount:byte;
    end;
  trtcParentList3_WStrPtr=record
    Nodes:array[0..100] of prtcNode3_WStrPtr;
    NodeCount:byte;
    end;

type
  trtcItemType_WStrWStr = RtcWideString;
  trtcInfoType_WStrWStr = RtcWideString;
const
  vrtcItemMin_WStrWStr: trtcItemType_WStrWStr = '';
  vrtcInfoNIL_WStrWStr: trtcInfoType_WStrWStr = '';
type
  prtcNode_WStrWStr=^trtcNode_WStrWStr;
  prtcNode3_WStrWStr=^trtcNode3_WStrWStr;
  trtcNode_WStrWStr=record
    key:trtcItemType_WStrWStr;
    info:trtcInfoType_WStrWStr;
    l,r:prtcNode_WStrWStr;
    b:boolean;
    end;
  trtcNode3_WStrWStr=record
    key:trtcItemType_WStrWStr;
    info:trtcInfoType_WStrWStr;
    l,r:prtcNode3_WStrWStr;
    b:boolean;
    l2,r2:prtcNode3_WStrWStr;
    b2:boolean;
    end;
  prtcNodeArr_WStrWStr=^trtcNodeArr_WStrWStr;
  prtcNodeArr3_WStrWStr=^trtcNodeArr3_WStrWStr;
  trtcNodeArr_WStrWStr=array of trtcNode_WStrWStr;
  trtcNodeArr3_WStrWStr=array of trtcNode3_WStrWStr;
  prtcParentList_WStrWStr=^trtcParentList_WStrWStr;
  prtcParentList3_WStrWStr=^trtcParentList3_WStrWStr;
  trtcParentList_WStrWStr=record
    Nodes:array[0..100] of prtcNode_WStrWStr;
    NodeCount:byte;
    end;
  trtcParentList3_WStrWStr=record
    Nodes:array[0..100] of prtcNode3_WStrWStr;
    NodeCount:byte;
    end;

implementation

end.
