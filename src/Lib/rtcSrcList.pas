{
  @html(<b>)
  Fast Search Lists
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
}
unit rtcSrcList;

{$INCLUDE rtcDefs.inc}

interface

uses
{$IFDEF RTC_WANT_GENERICS}
  Generics.Defaults,
{$ENDIF}
  rtcTypes,
  rtcSrcTypes;

{$IFDEF RTC_WANT_GENERICS}
type
{ Generic Balanced Binary search List (itemType, infoType) }
  tRtcSearchList<itemType,infoType>=class(TRtcFastObject)
    type
      pnode=^tnode;
      tnode=record
        key:itemType;
        info:infoType;
        l,r:pnode;
        b:boolean;
        end;
      pnodearr=^tnodearr;
      tnodearr=array of tnode;
      pParentList=^tParentList;
      tParentList=record
        Nodes:array[0..100] of pnode;
        NodeCount:byte;
        end;
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:pnode;
    xv:itemType;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:pnode;
    // Comparers
    itemC:IComparer<itemType>;
    // Min item and info values
    itemMin:itemType;
    infoNil:infoType;
    // data Pool
    myPoolSize:longint;
    myPools:array of pnodearr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:pnode;
    Parents:pParentList;
    procedure del_node(node:pnode);
    function new_node(const k:itemType; const i:infoType; const bi:boolean; const ll,rr:pnode):pnode;
    procedure RemoveThis(var t:pnode);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:pnode);
    function Remove_GetParentNode:pnode;
  public
    constructor Create(size:integer; const _min:itemType; const _nil:infoType); overload;
    constructor Create(size:integer; const _min:itemType; const _nil:infoType; const _itemComparer:IComparer<itemType>); overload;
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:itemType):infoType;      // Search for exact "v"
    function search_min(var i:infoType):itemType;
    function search_max(var i:infoType):itemType;
    function search_l(const v:itemType; var i:infoType):itemType;  // Search index lower than "v"
    function search_g(const v:itemType; var i:infoType):itemType;  // Search index higher than "v"
    function search_le(const v:itemType; var i:infoType):itemType;  // Search index for lower or equel to "v"
    function search_ge(const v:itemType; var i:infoType):itemType;  // Search index for higher or equal to "v"
    procedure change(const v:itemType;const info:infoType);
    procedure insert(const v:itemType;const info:infoType);
    procedure remove(const v:itemType);
    procedure removeall;
  public
    property RootNode:pnode read head;
    property NilNode:pnode read z;
    end;
{$ENDIF} // {$IFDEF RTC_WANT_GENERICS}

type
  TRtcComparer_CI32=function(left,right:trtcItemType_CI32):integer of object;
{ Balanced Binary Custom Item Search List (longint>0, longint>0) }
  tRtcList_CI32=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_CI32;
    xv:trtcItemType_CI32;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_CI32;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_CI32;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_CI32;
    Parents:prtcParentList_CI32;
    FUpdateCompare: TRtcComparer_CI32;
    FSearchCompare: TRtcComparer_CI32;
    procedure del_node(node:prtcNode_CI32);
    function new_node(const k:trtcItemType_CI32; const i:trtcInfoType_CI32; const bi:boolean; const ll,rr:prtcNode_CI32):prtcNode_CI32;
    procedure RemoveThis(var t:prtcNode_CI32);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_CI32);
    function Remove_GetParentNode:prtcNode_CI32;
  public
    constructor Create(size:integer); overload;
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_CI32):trtcInfoType_CI32;      // Search for exact "v"
    function search_near(const v:trtcItemType_CI32; var res:integer):trtcInfoType_CI32;      // Search for nearest "v"
    function search_min(var i:trtcInfoType_CI32):trtcItemType_CI32;
    function search_max(var i:trtcInfoType_CI32):trtcItemType_CI32;
    function search_l(const v:trtcItemType_CI32; var i:trtcInfoType_CI32):trtcItemType_CI32;  // Search index lower than "v"
    function search_g(const v:trtcItemType_CI32; var i:trtcInfoType_CI32):trtcItemType_CI32;  // Search index higher than "v"
    function search_le(const v:trtcItemType_CI32; var i:trtcInfoType_CI32):trtcItemType_CI32;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_CI32; var i:trtcInfoType_CI32):trtcItemType_CI32;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_CI32;const info:trtcInfoType_CI32);
    function insert(const v:trtcItemType_CI32;const info:trtcInfoType_CI32):trtcInfoType_CI32;
    procedure remove(const v:trtcItemType_CI32);
    procedure removeall;
    property UpdateComparer:TRtcComparer_CI32 read FUpdateCompare write FUpdateCompare;
    property SearchComparer:TRtcComparer_CI32 read FSearchCompare write FSearchCompare;
  public
    property RootNode:prtcNode_CI32 read head;
    property NilNode:prtcNode_CI32 read z;
    end;
type
  TRtcComparer_CI64=function(left,right:trtcItemType_CI64):integer of object;
{ Balanced Binary Custom Item Search List (longint>0, longint>0) }
  tRtcList_CI64=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_CI64;
    xv:trtcItemType_CI64;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_CI64;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_CI64;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_CI64;
    Parents:prtcParentList_CI64;
    FUpdateCompare: TRtcComparer_CI64;
    FSearchCompare: TRtcComparer_CI64;
    procedure del_node(node:prtcNode_CI64);
    function new_node(const k:trtcItemType_CI64; const i:trtcInfoType_CI64; const bi:boolean; const ll,rr:prtcNode_CI64):prtcNode_CI64;
    procedure RemoveThis(var t:prtcNode_CI64);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_CI64);
    function Remove_GetParentNode:prtcNode_CI64;
  public
    constructor Create(size:integer); overload;
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_CI64):trtcInfoType_CI64;      // Search for exact "v"
    function search_near(const v:trtcItemType_CI64; var res:integer):trtcInfoType_CI64;      // Search for nearest "v"
    function search_min(var i:trtcInfoType_CI64):trtcItemType_CI64;
    function search_max(var i:trtcInfoType_CI64):trtcItemType_CI64;
    function search_l(const v:trtcItemType_CI64; var i:trtcInfoType_CI64):trtcItemType_CI64;  // Search index lower than "v"
    function search_g(const v:trtcItemType_CI64; var i:trtcInfoType_CI64):trtcItemType_CI64;  // Search index higher than "v"
    function search_le(const v:trtcItemType_CI64; var i:trtcInfoType_CI64):trtcItemType_CI64;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_CI64; var i:trtcInfoType_CI64):trtcItemType_CI64;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_CI64;const info:trtcInfoType_CI64);
    function insert(const v:trtcItemType_CI64;const info:trtcInfoType_CI64):trtcInfoType_CI64;
    procedure remove(const v:trtcItemType_CI64);
    procedure removeall;
    property UpdateComparer:TRtcComparer_CI64 read FUpdateCompare write FUpdateCompare;
    property SearchComparer:TRtcComparer_CI64 read FSearchCompare write FSearchCompare;
  public
    property RootNode:prtcNode_CI64 read head;
    property NilNode:prtcNode_CI64 read z;
    end;
type
{ eXtended Pooled List (RtcIntPtr>0) }
  tRtcList_LI32=class(tRtcFastObject)
  private
    myPoolSize:longint;
    myPools:array of prtcNodeArr_LI32;
    pool:tRtcPtrPool;
    cnt:cardinal;
    Ffirst,
    Flast:prtcNode_LI32;
    procedure del_node(node:prtcNode_LI32);
    function new_node(const i:trtcInfoType_LI32; const pri,nex:prtcNode_LI32):prtcNode_LI32;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function First:trtcInfoType_LI32;
    function Last:trtcInfoType_LI32;
    procedure addFirst(const info:trtcInfoType_LI32);
    procedure addLast(const info:trtcInfoType_LI32);
    procedure removeFirst;
    procedure removeLast;
    procedure extractFirst(var exInfo:trtcInfoType_LI32);
    procedure extractLast(var exInfo:trtcInfoType_LI32);
    procedure removeThis(const info:trtcInfoType_LI32);
    procedure removeall;
    end;
type
{ eXtended Pooled List (RtcIntPtr>0) }
  tRtcList_LI64=class(tRtcFastObject)
  private
    myPoolSize:longint;
    myPools:array of prtcNodeArr_LI64;
    pool:tRtcPtrPool;
    cnt:cardinal;
    Ffirst,
    Flast:prtcNode_LI64;
    procedure del_node(node:prtcNode_LI64);
    function new_node(const i:trtcInfoType_LI64; const pri,nex:prtcNode_LI64):prtcNode_LI64;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function First:trtcInfoType_LI64;
    function Last:trtcInfoType_LI64;
    procedure addFirst(const info:trtcInfoType_LI64);
    procedure addLast(const info:trtcInfoType_LI64);
    procedure removeFirst;
    procedure removeLast;
    procedure extractFirst(var exInfo:trtcInfoType_LI64);
    procedure extractLast(var exInfo:trtcInfoType_LI64);
    procedure removeThis(const info:trtcInfoType_LI64);
    procedure removeall;
    end;
type
{ eXtended Pooled List (RtcIntPtr>0) }
  tRtcList_LIPtr=class(tRtcFastObject)
  private
    myPoolSize:longint;
    myPools:array of prtcNodeArr_LIPtr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    Ffirst,
    Flast:prtcNode_LIPtr;
    procedure del_node(node:prtcNode_LIPtr);
    function new_node(const i:trtcInfoType_LIPtr; const pri,nex:prtcNode_LIPtr):prtcNode_LIPtr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function First:trtcInfoType_LIPtr;
    function Last:trtcInfoType_LIPtr;
    procedure addFirst(const info:trtcInfoType_LIPtr);
    procedure addLast(const info:trtcInfoType_LIPtr);
    procedure removeFirst;
    procedure removeLast;
    procedure extractFirst(var exInfo:trtcInfoType_LIPtr);
    procedure extractLast(var exInfo:trtcInfoType_LIPtr);
    procedure removeThis(const info:trtcInfoType_LIPtr);
    procedure removeall;
    end;
type
{ eXtended Pooled List (RtcIntPtr>0) }
  tRtcList_LObj=class(tRtcFastObject)
  private
    myPoolSize:longint;
    myPools:array of prtcNodeArr_LObj;
    pool:tRtcPtrPool;
    cnt:cardinal;
    Ffirst,
    Flast:prtcNode_LObj;
    procedure del_node(node:prtcNode_LObj);
    function new_node(const i:trtcInfoType_LObj; const pri,nex:prtcNode_LObj):prtcNode_LObj;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function First:trtcInfoType_LObj;
    function Last:trtcInfoType_LObj;
    procedure addFirst(const info:trtcInfoType_LObj);
    procedure addLast(const info:trtcInfoType_LObj);
    procedure removeFirst;
    procedure removeLast;
    procedure extractFirst(var exInfo:trtcInfoType_LObj);
    procedure extractLast(var exInfo:trtcInfoType_LObj);
    procedure removeThis(const info:trtcInfoType_LObj);
    procedure removeall;
    end;
type  
{ eXtended Pooled List (RtcIntPtr>0) }
  tRtcList_LPtr=class(tRtcFastObject)
  private
    myPoolSize:longint;
    myPools:array of prtcNodeArr_LPtr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    Ffirst,
    Flast:prtcNode_LPtr;
    procedure del_node(node:prtcNode_LPtr);
    function new_node(const i:trtcInfoType_LPtr; const pri,nex:prtcNode_LPtr):prtcNode_LPtr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function First:trtcInfoType_LPtr;
    function Last:trtcInfoType_LPtr;
    procedure addFirst(const info:trtcInfoType_LPtr);
    procedure addLast(const info:trtcInfoType_LPtr);
    procedure removeFirst;
    procedure removeLast;
    procedure extractFirst(var exInfo:trtcInfoType_LPtr);
    procedure extractLast(var exInfo:trtcInfoType_LPtr);
    procedure removeThis(const info:trtcInfoType_LPtr);
    procedure removeall;
    end;
type
{ eXtended Pooled List (RtcIntPtr>0) }
  tRtcList_LStr=class(tRtcFastObject)
  private
    myPoolSize:longint;
    myPools:array of prtcNodeArr_LStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    Ffirst,
    Flast:prtcNode_LStr;
    procedure del_node(node:prtcNode_LStr);
    function new_node(const i:trtcInfoType_LStr; const pri,nex:prtcNode_LStr):prtcNode_LStr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function First:trtcInfoType_LStr;
    function Last:trtcInfoType_LStr;
    procedure addFirst(const info:trtcInfoType_LStr);
    procedure addLast(const info:trtcInfoType_LStr);
    procedure removeFirst;
    procedure removeLast;
    procedure extractFirst(var exInfo:trtcInfoType_LStr);
    procedure extractLast(var exInfo:trtcInfoType_LStr);
    procedure removeThis(const info:trtcInfoType_LStr);
    procedure removeall;
    end;
type
{ eXtended Pooled List (RtcIntPtr>0) }
  tRtcList_LWStr=class(tRtcFastObject)
  private
    myPoolSize:longint;
    myPools:array of prtcNodeArr_LWStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    Ffirst,
    Flast:prtcNode_LWStr;
    procedure del_node(node:prtcNode_LWStr);
    function new_node(const i:trtcInfoType_LWStr; const pri,nex:prtcNode_LWStr):prtcNode_LWStr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function First:trtcInfoType_LWStr;
    function Last:trtcInfoType_LWStr;
    procedure addFirst(const info:trtcInfoType_LWStr);
    procedure addLast(const info:trtcInfoType_LWStr);
    procedure removeFirst;
    procedure removeLast;
    procedure extractFirst(var exInfo:trtcInfoType_LWStr);
    procedure extractLast(var exInfo:trtcInfoType_LWStr);
    procedure removeThis(const info:trtcInfoType_LWStr);
    procedure removeall;
    end;
type
  tRtcList_I32I32=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_I32I32;
    xv:trtcItemType_I32I32;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_I32I32;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_I32I32;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_I32I32;
    Parents:prtcParentList_I32I32;
    procedure del_node(node:prtcNode_I32I32);
    function new_node(const k:trtcItemType_I32I32; const i:trtcInfoType_I32I32; const bi:boolean; const ll,rr:prtcNode_I32I32):prtcNode_I32I32;
    procedure RemoveThis(var t:prtcNode_I32I32);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_I32I32);
    function Remove_GetParentNode:prtcNode_I32I32;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_I32I32):trtcInfoType_I32I32;      // Search for exact "v"
    function search_min(var i:trtcInfoType_I32I32):trtcItemType_I32I32;
    function search_max(var i:trtcInfoType_I32I32):trtcItemType_I32I32;
    function search_l(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;  // Search index lower than "v"
    function search_g(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;  // Search index higher than "v"
    function search_le(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_I32I32;const info:trtcInfoType_I32I32);
    procedure insert(const v:trtcItemType_I32I32;const info:trtcInfoType_I32I32);
    procedure remove(const v:trtcItemType_I32I32);
    procedure removeall;
  public
    property RootNode:prtcNode_I32I32 read head;
    property NilNode:prtcNode_I32I32 read z;
    end;
type
  tRtcList_I32Obj=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_I32Obj;
    xv:trtcItemType_I32Obj;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_I32Obj;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_I32Obj;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_I32Obj;
    Parents:prtcParentList_I32Obj;
    procedure del_node(node:prtcNode_I32Obj);
    function new_node(const k:trtcItemType_I32Obj; const i:trtcInfoType_I32Obj; const bi:boolean; const ll,rr:prtcNode_I32Obj):prtcNode_I32Obj;
    procedure RemoveThis(var t:prtcNode_I32Obj);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_I32Obj);
    function Remove_GetParentNode:prtcNode_I32Obj;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_I32Obj):trtcInfoType_I32Obj;      // Search for exact "v"
    function search_min(var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
    function search_max(var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
    function search_l(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;  // Search index lower than "v"
    function search_g(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;  // Search index higher than "v"
    function search_le(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_I32Obj;const info:trtcInfoType_I32Obj);
    procedure insert(const v:trtcItemType_I32Obj;const info:trtcInfoType_I32Obj);
    procedure remove(const v:trtcItemType_I32Obj);
    procedure removeall;
  public
    property RootNode:prtcNode_I32Obj read head;
    property NilNode:prtcNode_I32Obj read z;
    end;
type
  tRtcList_I32Ptr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_I32Ptr;
    xv:trtcItemType_I32Ptr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_I32Ptr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_I32Ptr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_I32Ptr;
    Parents:prtcParentList_I32Ptr;
    procedure del_node(node:prtcNode_I32Ptr);
    function new_node(const k:trtcItemType_I32Ptr; const i:trtcInfoType_I32Ptr; const bi:boolean; const ll,rr:prtcNode_I32Ptr):prtcNode_I32Ptr;
    procedure RemoveThis(var t:prtcNode_I32Ptr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_I32Ptr);
    function Remove_GetParentNode:prtcNode_I32Ptr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
    function search_max(var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
    function search_l(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_I32Ptr;const info:trtcInfoType_I32Ptr);
    procedure insert(const v:trtcItemType_I32Ptr;const info:trtcInfoType_I32Ptr);
    procedure remove(const v:trtcItemType_I32Ptr);
    procedure removeall;
  public
    property RootNode:prtcNode_I32Ptr read head;
    property NilNode:prtcNode_I32Ptr read z;
    end;
type
  tRtcList_I32Str=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_I32Str;
    xv:trtcItemType_I32Str;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_I32Str;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_I32Str;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_I32Str;
    Parents:prtcParentList_I32Str;
    procedure del_node(node:prtcNode_I32Str);
    function new_node(const k:trtcItemType_I32Str; const i:trtcInfoType_I32Str; const bi:boolean; const ll,rr:prtcNode_I32Str):prtcNode_I32Str;
    procedure RemoveThis(var t:prtcNode_I32Str);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_I32Str);
    function Remove_GetParentNode:prtcNode_I32Str;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_I32Str):trtcInfoType_I32Str;      // Search for exact "v"
    function search_min(var i:trtcInfoType_I32Str):trtcItemType_I32Str;
    function search_max(var i:trtcInfoType_I32Str):trtcItemType_I32Str;
    function search_l(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;  // Search index lower than "v"
    function search_g(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;  // Search index higher than "v"
    function search_le(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_I32Str;const info:trtcInfoType_I32Str);
    procedure insert(const v:trtcItemType_I32Str;const info:trtcInfoType_I32Str);
    procedure remove(const v:trtcItemType_I32Str);
    procedure removeall;
  public
    property RootNode:prtcNode_I32Str read head;
    property NilNode:prtcNode_I32Str read z;
    end;
type
  tRtcList_I32WStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_I32WStr;
    xv:trtcItemType_I32WStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_I32WStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_I32WStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_I32WStr;
    Parents:prtcParentList_I32WStr;
    procedure del_node(node:prtcNode_I32WStr);
    function new_node(const k:trtcItemType_I32WStr; const i:trtcInfoType_I32WStr; const bi:boolean; const ll,rr:prtcNode_I32WStr):prtcNode_I32WStr;
    procedure RemoveThis(var t:prtcNode_I32WStr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_I32WStr);
    function Remove_GetParentNode:prtcNode_I32WStr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_I32WStr):trtcInfoType_I32WStr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
    function search_max(var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
    function search_l(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_I32WStr;const info:trtcInfoType_I32WStr);
    procedure insert(const v:trtcItemType_I32WStr;const info:trtcInfoType_I32WStr);
    procedure remove(const v:trtcItemType_I32WStr);
    procedure removeall;
  public
    property RootNode:prtcNode_I32WStr read head;
    property NilNode:prtcNode_I32WStr read z;
    end;
type
  tRtcList_I64I64=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_I64I64;
    xv:trtcItemType_I64I64;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_I64I64;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_I64I64;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_I64I64;
    Parents:prtcParentList_I64I64;
    procedure del_node(node:prtcNode_I64I64);
    function new_node(const k:trtcItemType_I64I64; const i:trtcInfoType_I64I64; const bi:boolean; const ll,rr:prtcNode_I64I64):prtcNode_I64I64;
    procedure RemoveThis(var t:prtcNode_I64I64);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_I64I64);
    function Remove_GetParentNode:prtcNode_I64I64;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_I64I64):trtcInfoType_I64I64;      // Search for exact "v"
    function search_min(var i:trtcInfoType_I64I64):trtcItemType_I64I64;
    function search_max(var i:trtcInfoType_I64I64):trtcItemType_I64I64;
    function search_l(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;  // Search index lower than "v"
    function search_g(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;  // Search index higher than "v"
    function search_le(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_I64I64;const info:trtcInfoType_I64I64);
    procedure insert(const v:trtcItemType_I64I64;const info:trtcInfoType_I64I64);
    procedure remove(const v:trtcItemType_I64I64);
    procedure removeall;
  public
    property RootNode:prtcNode_I64I64 read head;
    property NilNode:prtcNode_I64I64 read z;
    end;
type
  tRtcList_I64Obj=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_I64Obj;
    xv:trtcItemType_I64Obj;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_I64Obj;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_I64Obj;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_I64Obj;
    Parents:prtcParentList_I64Obj;
    procedure del_node(node:prtcNode_I64Obj);
    function new_node(const k:trtcItemType_I64Obj; const i:trtcInfoType_I64Obj; const bi:boolean; const ll,rr:prtcNode_I64Obj):prtcNode_I64Obj;
    procedure RemoveThis(var t:prtcNode_I64Obj);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_I64Obj);
    function Remove_GetParentNode:prtcNode_I64Obj;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_I64Obj):trtcInfoType_I64Obj;      // Search for exact "v"
    function search_min(var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
    function search_max(var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
    function search_l(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;  // Search index lower than "v"
    function search_g(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;  // Search index higher than "v"
    function search_le(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_I64Obj;const info:trtcInfoType_I64Obj);
    procedure insert(const v:trtcItemType_I64Obj;const info:trtcInfoType_I64Obj);
    procedure remove(const v:trtcItemType_I64Obj);
    procedure removeall;
  public
    property RootNode:prtcNode_I64Obj read head;
    property NilNode:prtcNode_I64Obj read z;
    end;
type
  tRtcList_I64Ptr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_I64Ptr;
    xv:trtcItemType_I64Ptr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_I64Ptr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_I64Ptr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_I64Ptr;
    Parents:prtcParentList_I64Ptr;
    procedure del_node(node:prtcNode_I64Ptr);
    function new_node(const k:trtcItemType_I64Ptr; const i:trtcInfoType_I64Ptr; const bi:boolean; const ll,rr:prtcNode_I64Ptr):prtcNode_I64Ptr;
    procedure RemoveThis(var t:prtcNode_I64Ptr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_I64Ptr);
    function Remove_GetParentNode:prtcNode_I64Ptr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
    function search_max(var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
    function search_l(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_I64Ptr;const info:trtcInfoType_I64Ptr);
    procedure insert(const v:trtcItemType_I64Ptr;const info:trtcInfoType_I64Ptr);
    procedure remove(const v:trtcItemType_I64Ptr);
    procedure removeall;
  public
    property RootNode:prtcNode_I64Ptr read head;
    property NilNode:prtcNode_I64Ptr read z;
    end;
type
  tRtcList_I64Str=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_I64Str;
    xv:trtcItemType_I64Str;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_I64Str;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_I64Str;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_I64Str;
    Parents:prtcParentList_I64Str;
    procedure del_node(node:prtcNode_I64Str);
    function new_node(const k:trtcItemType_I64Str; const i:trtcInfoType_I64Str; const bi:boolean; const ll,rr:prtcNode_I64Str):prtcNode_I64Str;
    procedure RemoveThis(var t:prtcNode_I64Str);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_I64Str);
    function Remove_GetParentNode:prtcNode_I64Str;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_I64Str):trtcInfoType_I64Str;      // Search for exact "v"
    function search_min(var i:trtcInfoType_I64Str):trtcItemType_I64Str;
    function search_max(var i:trtcInfoType_I64Str):trtcItemType_I64Str;
    function search_l(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;  // Search index lower than "v"
    function search_g(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;  // Search index higher than "v"
    function search_le(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_I64Str;const info:trtcInfoType_I64Str);
    procedure insert(const v:trtcItemType_I64Str;const info:trtcInfoType_I64Str);
    procedure remove(const v:trtcItemType_I64Str);
    procedure removeall;
  public
    property RootNode:prtcNode_I64Str read head;
    property NilNode:prtcNode_I64Str read z;
    end;
type
  tRtcList_I64WStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_I64WStr;
    xv:trtcItemType_I64WStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_I64WStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_I64WStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_I64WStr;
    Parents:prtcParentList_I64WStr;
    procedure del_node(node:prtcNode_I64WStr);
    function new_node(const k:trtcItemType_I64WStr; const i:trtcInfoType_I64WStr; const bi:boolean; const ll,rr:prtcNode_I64WStr):prtcNode_I64WStr;
    procedure RemoveThis(var t:prtcNode_I64WStr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_I64WStr);
    function Remove_GetParentNode:prtcNode_I64WStr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_I64WStr):trtcInfoType_I64WStr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
    function search_max(var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
    function search_l(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_I64WStr;const info:trtcInfoType_I64WStr);
    procedure insert(const v:trtcItemType_I64WStr;const info:trtcInfoType_I64WStr);
    procedure remove(const v:trtcItemType_I64WStr);
    procedure removeall;
  public
    property RootNode:prtcNode_I64WStr read head;
    property NilNode:prtcNode_I64WStr read z;
    end;
type
  tRtcList_IPtrIPtr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_IPtrIPtr;
    xv:trtcItemType_IPtrIPtr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_IPtrIPtr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_IPtrIPtr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_IPtrIPtr;
    Parents:prtcParentList_IPtrIPtr;
    procedure del_node(node:prtcNode_IPtrIPtr);
    function new_node(const k:trtcItemType_IPtrIPtr; const i:trtcInfoType_IPtrIPtr; const bi:boolean; const ll,rr:prtcNode_IPtrIPtr):prtcNode_IPtrIPtr;
    procedure RemoveThis(var t:prtcNode_IPtrIPtr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_IPtrIPtr);
    function Remove_GetParentNode:prtcNode_IPtrIPtr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
    function search_max(var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
    function search_l(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_IPtrIPtr;const info:trtcInfoType_IPtrIPtr);
    procedure insert(const v:trtcItemType_IPtrIPtr;const info:trtcInfoType_IPtrIPtr);
    procedure remove(const v:trtcItemType_IPtrIPtr);
    procedure removeall;
  public
    property RootNode:prtcNode_IPtrIPtr read head;
    property NilNode:prtcNode_IPtrIPtr read z;
    end;
type
  tRtcList_IPtrObj=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_IPtrObj;
    xv:trtcItemType_IPtrObj;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_IPtrObj;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_IPtrObj;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_IPtrObj;
    Parents:prtcParentList_IPtrObj;
    procedure del_node(node:prtcNode_IPtrObj);
    function new_node(const k:trtcItemType_IPtrObj; const i:trtcInfoType_IPtrObj; const bi:boolean; const ll,rr:prtcNode_IPtrObj):prtcNode_IPtrObj;
    procedure RemoveThis(var t:prtcNode_IPtrObj);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_IPtrObj);
    function Remove_GetParentNode:prtcNode_IPtrObj;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;      // Search for exact "v"
    function search_min(var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
    function search_max(var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
    function search_l(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;  // Search index lower than "v"
    function search_g(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;  // Search index higher than "v"
    function search_le(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_IPtrObj;const info:trtcInfoType_IPtrObj);
    procedure insert(const v:trtcItemType_IPtrObj;const info:trtcInfoType_IPtrObj);
    procedure remove(const v:trtcItemType_IPtrObj);
    procedure removeall;
  public
    property RootNode:prtcNode_IPtrObj read head;
    property NilNode:prtcNode_IPtrObj read z;
    end;
type
  tRtcList_IPtrPtr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_IPtrPtr;
    xv:trtcItemType_IPtrPtr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_IPtrPtr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_IPtrPtr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_IPtrPtr;
    Parents:prtcParentList_IPtrPtr;
    procedure del_node(node:prtcNode_IPtrPtr);
    function new_node(const k:trtcItemType_IPtrPtr; const i:trtcInfoType_IPtrPtr; const bi:boolean; const ll,rr:prtcNode_IPtrPtr):prtcNode_IPtrPtr;
    procedure RemoveThis(var t:prtcNode_IPtrPtr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_IPtrPtr);
    function Remove_GetParentNode:prtcNode_IPtrPtr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
    function search_max(var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
    function search_l(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_IPtrPtr;const info:trtcInfoType_IPtrPtr);
    procedure insert(const v:trtcItemType_IPtrPtr;const info:trtcInfoType_IPtrPtr);
    procedure remove(const v:trtcItemType_IPtrPtr);
    procedure removeall;
  public
    property RootNode:prtcNode_IPtrPtr read head;
    property NilNode:prtcNode_IPtrPtr read z;
    end;
type
  tRtcList_IPtrStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_IPtrStr;
    xv:trtcItemType_IPtrStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_IPtrStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_IPtrStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_IPtrStr;
    Parents:prtcParentList_IPtrStr;
    procedure del_node(node:prtcNode_IPtrStr);
    function new_node(const k:trtcItemType_IPtrStr; const i:trtcInfoType_IPtrStr; const bi:boolean; const ll,rr:prtcNode_IPtrStr):prtcNode_IPtrStr;
    procedure RemoveThis(var t:prtcNode_IPtrStr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_IPtrStr);
    function Remove_GetParentNode:prtcNode_IPtrStr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
    function search_max(var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
    function search_l(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_IPtrStr;const info:trtcInfoType_IPtrStr);
    procedure insert(const v:trtcItemType_IPtrStr;const info:trtcInfoType_IPtrStr);
    procedure remove(const v:trtcItemType_IPtrStr);
    procedure removeall;
  public
    property RootNode:prtcNode_IPtrStr read head;
    property NilNode:prtcNode_IPtrStr read z;
    end;
type
  tRtcList_IPtrWStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_IPtrWStr;
    xv:trtcItemType_IPtrWStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_IPtrWStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_IPtrWStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_IPtrWStr;
    Parents:prtcParentList_IPtrWStr;
    procedure del_node(node:prtcNode_IPtrWStr);
    function new_node(const k:trtcItemType_IPtrWStr; const i:trtcInfoType_IPtrWStr; const bi:boolean; const ll,rr:prtcNode_IPtrWStr):prtcNode_IPtrWStr;
    procedure RemoveThis(var t:prtcNode_IPtrWStr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_IPtrWStr);
    function Remove_GetParentNode:prtcNode_IPtrWStr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
    function search_max(var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
    function search_l(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_IPtrWStr;const info:trtcInfoType_IPtrWStr);
    procedure insert(const v:trtcItemType_IPtrWStr;const info:trtcInfoType_IPtrWStr);
    procedure remove(const v:trtcItemType_IPtrWStr);
    procedure removeall;
  public
    property RootNode:prtcNode_IPtrWStr read head;
    property NilNode:prtcNode_IPtrWStr read z;
    end;
type
  tRtcList_StrI32=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_StrI32;
    xv:trtcItemType_StrI32;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_StrI32;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_StrI32;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_StrI32;
    Parents:prtcParentList_StrI32;
    procedure del_node(node:prtcNode_StrI32);
    function new_node(const k:trtcItemType_StrI32; const i:trtcInfoType_StrI32; const bi:boolean; const ll,rr:prtcNode_StrI32):prtcNode_StrI32;
    procedure RemoveThis(var t:prtcNode_StrI32);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_StrI32);
    function Remove_GetParentNode:prtcNode_StrI32;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_StrI32):trtcInfoType_StrI32;      // Search for exact "v"
    function search_min(var i:trtcInfoType_StrI32):trtcItemType_StrI32;
    function search_max(var i:trtcInfoType_StrI32):trtcItemType_StrI32;
    function search_l(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;  // Search index lower than "v"
    function search_g(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;  // Search index higher than "v"
    function search_le(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_StrI32;const info:trtcInfoType_StrI32);
    procedure insert(const v:trtcItemType_StrI32;const info:trtcInfoType_StrI32);
    procedure remove(const v:trtcItemType_StrI32);
    procedure removeall;
  public
    property RootNode:prtcNode_StrI32 read head;
    property NilNode:prtcNode_StrI32 read z;
    end;
type
  tRtcList_StrI64=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_StrI64;
    xv:trtcItemType_StrI64;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_StrI64;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_StrI64;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_StrI64;
    Parents:prtcParentList_StrI64;
    procedure del_node(node:prtcNode_StrI64);
    function new_node(const k:trtcItemType_StrI64; const i:trtcInfoType_StrI64; const bi:boolean; const ll,rr:prtcNode_StrI64):prtcNode_StrI64;
    procedure RemoveThis(var t:prtcNode_StrI64);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_StrI64);
    function Remove_GetParentNode:prtcNode_StrI64;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_StrI64):trtcInfoType_StrI64;      // Search for exact "v"
    function search_min(var i:trtcInfoType_StrI64):trtcItemType_StrI64;
    function search_max(var i:trtcInfoType_StrI64):trtcItemType_StrI64;
    function search_l(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;  // Search index lower than "v"
    function search_g(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;  // Search index higher than "v"
    function search_le(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_StrI64;const info:trtcInfoType_StrI64);
    procedure insert(const v:trtcItemType_StrI64;const info:trtcInfoType_StrI64);
    procedure remove(const v:trtcItemType_StrI64);
    procedure removeall;
  public
    property RootNode:prtcNode_StrI64 read head;
    property NilNode:prtcNode_StrI64 read z;
    end;
type
  tRtcList_StrObj=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_StrObj;
    xv:trtcItemType_StrObj;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_StrObj;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_StrObj;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_StrObj;
    Parents:prtcParentList_StrObj;
    procedure del_node(node:prtcNode_StrObj);
    function new_node(const k:trtcItemType_StrObj; const i:trtcInfoType_StrObj; const bi:boolean; const ll,rr:prtcNode_StrObj):prtcNode_StrObj;
    procedure RemoveThis(var t:prtcNode_StrObj);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_StrObj);
    function Remove_GetParentNode:prtcNode_StrObj;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_StrObj):trtcInfoType_StrObj;      // Search for exact "v"
    function search_min(var i:trtcInfoType_StrObj):trtcItemType_StrObj;
    function search_max(var i:trtcInfoType_StrObj):trtcItemType_StrObj;
    function search_l(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;  // Search index lower than "v"
    function search_g(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;  // Search index higher than "v"
    function search_le(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_StrObj;const info:trtcInfoType_StrObj);
    procedure insert(const v:trtcItemType_StrObj;const info:trtcInfoType_StrObj);
    procedure remove(const v:trtcItemType_StrObj);
    procedure removeall;
  public
    property RootNode:prtcNode_StrObj read head;
    property NilNode:prtcNode_StrObj read z;
    end;
type
  tRtcList_StrPtr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_StrPtr;
    xv:trtcItemType_StrPtr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_StrPtr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_StrPtr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_StrPtr;
    Parents:prtcParentList_StrPtr;
    procedure del_node(node:prtcNode_StrPtr);
    function new_node(const k:trtcItemType_StrPtr; const i:trtcInfoType_StrPtr; const bi:boolean; const ll,rr:prtcNode_StrPtr):prtcNode_StrPtr;
    procedure RemoveThis(var t:prtcNode_StrPtr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_StrPtr);
    function Remove_GetParentNode:prtcNode_StrPtr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_StrPtr):trtcInfoType_StrPtr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
    function search_max(var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
    function search_l(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_StrPtr;const info:trtcInfoType_StrPtr);
    procedure insert(const v:trtcItemType_StrPtr;const info:trtcInfoType_StrPtr);
    procedure remove(const v:trtcItemType_StrPtr);
    procedure removeall;
  public
    property RootNode:prtcNode_StrPtr read head;
    property NilNode:prtcNode_StrPtr read z;
    end;
type
  tRtcList_StrStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_StrStr;
    xv:trtcItemType_StrStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_StrStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_StrStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_StrStr;
    Parents:prtcParentList_StrStr;
    procedure del_node(node:prtcNode_StrStr);
    function new_node(const k:trtcItemType_StrStr; const i:trtcInfoType_StrStr; const bi:boolean; const ll,rr:prtcNode_StrStr):prtcNode_StrStr;
    procedure RemoveThis(var t:prtcNode_StrStr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_StrStr);
    function Remove_GetParentNode:prtcNode_StrStr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_StrStr):trtcInfoType_StrStr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_StrStr):trtcItemType_StrStr;
    function search_max(var i:trtcInfoType_StrStr):trtcItemType_StrStr;
    function search_l(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_StrStr;const info:trtcInfoType_StrStr);
    procedure insert(const v:trtcItemType_StrStr;const info:trtcInfoType_StrStr);
    procedure remove(const v:trtcItemType_StrStr);
    procedure removeall;
  public
    property RootNode:prtcNode_StrStr read head;
    property NilNode:prtcNode_StrStr read z;
    end;
type
  tRtcList_WStrI32=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_WStrI32;
    xv:trtcItemType_WStrI32;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_WStrI32;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_WStrI32;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_WStrI32;
    Parents:prtcParentList_WStrI32;
    procedure del_node(node:prtcNode_WStrI32);
    function new_node(const k:trtcItemType_WStrI32; const i:trtcInfoType_WStrI32; const bi:boolean; const ll,rr:prtcNode_WStrI32):prtcNode_WStrI32;
    procedure RemoveThis(var t:prtcNode_WStrI32);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_WStrI32);
    function Remove_GetParentNode:prtcNode_WStrI32;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_WStrI32):trtcInfoType_WStrI32;      // Search for exact "v"
    function search_min(var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
    function search_max(var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
    function search_l(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;  // Search index lower than "v"
    function search_g(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;  // Search index higher than "v"
    function search_le(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_WStrI32;const info:trtcInfoType_WStrI32);
    procedure insert(const v:trtcItemType_WStrI32;const info:trtcInfoType_WStrI32);
    procedure remove(const v:trtcItemType_WStrI32);
    procedure removeall;
  public
    property RootNode:prtcNode_WStrI32 read head;
    property NilNode:prtcNode_WStrI32 read z;
    end;
type
  tRtcList_WStrI64=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_WStrI64;
    xv:trtcItemType_WStrI64;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_WStrI64;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_WStrI64;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_WStrI64;
    Parents:prtcParentList_WStrI64;
    procedure del_node(node:prtcNode_WStrI64);
    function new_node(const k:trtcItemType_WStrI64; const i:trtcInfoType_WStrI64; const bi:boolean; const ll,rr:prtcNode_WStrI64):prtcNode_WStrI64;
    procedure RemoveThis(var t:prtcNode_WStrI64);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_WStrI64);
    function Remove_GetParentNode:prtcNode_WStrI64;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_WStrI64):trtcInfoType_WStrI64;      // Search for exact "v"
    function search_min(var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
    function search_max(var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
    function search_l(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;  // Search index lower than "v"
    function search_g(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;  // Search index higher than "v"
    function search_le(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_WStrI64;const info:trtcInfoType_WStrI64);
    procedure insert(const v:trtcItemType_WStrI64;const info:trtcInfoType_WStrI64);
    procedure remove(const v:trtcItemType_WStrI64);
    procedure removeall;
  public
    property RootNode:prtcNode_WStrI64 read head;
    property NilNode:prtcNode_WStrI64 read z;
    end;
type
  tRtcList_WStrObj=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_WStrObj;
    xv:trtcItemType_WStrObj;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_WStrObj;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_WStrObj;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_WStrObj;
    Parents:prtcParentList_WStrObj;
    procedure del_node(node:prtcNode_WStrObj);
    function new_node(const k:trtcItemType_WStrObj; const i:trtcInfoType_WStrObj; const bi:boolean; const ll,rr:prtcNode_WStrObj):prtcNode_WStrObj;
    procedure RemoveThis(var t:prtcNode_WStrObj);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_WStrObj);
    function Remove_GetParentNode:prtcNode_WStrObj;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_WStrObj):trtcInfoType_WStrObj;      // Search for exact "v"
    function search_min(var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
    function search_max(var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
    function search_l(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;  // Search index lower than "v"
    function search_g(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;  // Search index higher than "v"
    function search_le(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_WStrObj;const info:trtcInfoType_WStrObj);
    procedure insert(const v:trtcItemType_WStrObj;const info:trtcInfoType_WStrObj);
    procedure remove(const v:trtcItemType_WStrObj);
    procedure removeall;
  public
    property RootNode:prtcNode_WStrObj read head;
    property NilNode:prtcNode_WStrObj read z;
    end;
type
  tRtcList_WStrPtr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_WStrPtr;
    xv:trtcItemType_WStrPtr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_WStrPtr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_WStrPtr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_WStrPtr;
    Parents:prtcParentList_WStrPtr;
    procedure del_node(node:prtcNode_WStrPtr);
    function new_node(const k:trtcItemType_WStrPtr; const i:trtcInfoType_WStrPtr; const bi:boolean; const ll,rr:prtcNode_WStrPtr):prtcNode_WStrPtr;
    procedure RemoveThis(var t:prtcNode_WStrPtr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_WStrPtr);
    function Remove_GetParentNode:prtcNode_WStrPtr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
    function search_max(var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
    function search_l(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_WStrPtr;const info:trtcInfoType_WStrPtr);
    procedure insert(const v:trtcItemType_WStrPtr;const info:trtcInfoType_WStrPtr);
    procedure remove(const v:trtcItemType_WStrPtr);
    procedure removeall;
  public
    property RootNode:prtcNode_WStrPtr read head;
    property NilNode:prtcNode_WStrPtr read z;
    end;
type
  tRtcList_WStrWStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode_WStrWStr;
    xv:trtcItemType_WStrWStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode_WStrWStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr_WStrWStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode_WStrWStr;
    Parents:prtcParentList_WStrWStr;
    procedure del_node(node:prtcNode_WStrWStr);
    function new_node(const k:trtcItemType_WStrWStr; const i:trtcInfoType_WStrWStr; const bi:boolean; const ll,rr:prtcNode_WStrWStr):prtcNode_WStrWStr;
    procedure RemoveThis(var t:prtcNode_WStrWStr);
    procedure Insert_split;
    procedure Remove_AddParentNode(node:prtcNode_WStrWStr);
    function Remove_GetParentNode:prtcNode_WStrWStr;
  public
    constructor Create(size:integer);
    destructor Destroy; override;
    function Empty:boolean;
    function Count:cardinal;
    procedure PoolSize(size:integer);
    function search(const v:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;      // Search for exact "v"
    function search_min(var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
    function search_max(var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
    function search_l(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;  // Search index lower than "v"
    function search_g(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;  // Search index higher than "v"
    function search_le(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;  // Search index for lower or equel to "v"
    function search_ge(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;  // Search index for higher or equal to "v"
    procedure change(const v:trtcItemType_WStrWStr;const info:trtcInfoType_WStrWStr);
    procedure insert(const v:trtcItemType_WStrWStr;const info:trtcInfoType_WStrWStr);
    procedure remove(const v:trtcItemType_WStrWStr);
    procedure removeall;
  public
    property RootNode:prtcNode_WStrWStr read head;
    property NilNode:prtcNode_WStrWStr read z;
    end;

(* Type aliases for backwards compatibility *)

type
  TCustomItemComparer = TRtcComparer_CI32;
  tItemSearchList = tRtcList_CI32;

  tBinList = tRtcList_IPtrIPtr;
  tBinList_ItemType = trtcItemType_IPtrIPtr;
  tBinList_InfoType = trtcInfoType_IPtrIPtr;

  tObjList = tRtcList_IPtrObj;
  tObjList_ItemType = trtcItemType_IPtrObj;
  tObjList_InfoType = trtcInfoType_IPtrObj;

  tObjList64 = tRtcList_I64Obj;
  tObjList64_ItemType = trtcItemType_I64Obj;
  tObjList64_InfoType = trtcInfoType_I64Obj;

  tStringIntList = tRtcList_WStrI32;
  tStringIntList_ItemType = trtcItemType_WStrI32;
  tStringIntList_InfoType = trtcInfoType_WStrI32;

  tStringObjList = tRtcList_WStrObj;
  tStringObjList_ItemType = trtcItemType_WStrObj;
  tStringObjList_InfoType = trtcInfoType_WStrObj;

  tStringPtrList = tRtcList_WStrPtr;
  tStringPtrList_ItemType = trtcItemType_WStrPtr;
  tStringPtrList_InfoType = trtcInfoType_WStrPtr;

  tStrIntList = tRtcList_StrI32;
  tStrIntList_ItemType = trtcItemType_StrI32;
  tStrIntList_InfoType = trtcInfoType_StrI32;

  tStrList = tRtcList_StrStr;
  tStrList_ItemType = trtcItemType_StrStr;
  tStrList_InfoType = trtcInfoType_StrStr;

  tStrObjList = tRtcList_StrObj;
  tStrObjList_ItemType = trtcItemType_StrObj;
  tStrObjList_InfoType = trtcInfoType_StrObj;

  tXList = tRtcList_LIPtr;
  tXList_InfoType = trtcInfoType_LIPtr;

  tXObjList = tRtcList_LObj;
  tXObjList_InfoType = trtcInfoType_LObj;

  tXSList = tRtcList_LStr;
  tXSList_InfoType = trtcInfoType_LStr;

implementation

{$IFDEF RTC_WANT_GENERICS}
constructor tRtcSearchList<itemType,infoType>.Create(size:integer; const _min:itemType; const _nil:infoType; const _itemComparer:IComparer<itemType>);
  begin
  inherited Create;
  itemMin:=_min;
  infoNil:=_nil;
  itemC:=_itemComparer;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(itemMin,infoNil,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(itemMin,infoNil,false,z,z);
  New(Parents);
  end;
constructor tRtcSearchList<itemType,infoType>.Create(size:integer; const _min:itemType; const _nil:infoType);
  begin
  Create(size,_min,_nil,TComparer<itemType>.Default);
  end;
destructor tRtcSearchList<itemType,infoType>.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=infoNil;
    head^.key:=itemMin;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=infoNil;
    z^.key:=itemMin;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;
function tRtcSearchList<itemType,infoType>.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;
function tRtcSearchList<itemType,infoType>.New_Node(const k:itemType; const i:infoType; const bi:boolean; const ll,rr:pnode):pnode;
  var
    p:pnodearr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(pnodearr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(tnode));
  FillChar(Result^,SizeOf(tnode),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;
procedure tRtcSearchList<itemType,infoType>.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;
procedure tRtcSearchList<itemType,infoType>.Del_Node(node:pnode);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;
procedure tRtcSearchList<itemType,infoType>.Change(const v:itemType;const info:infoType);
  var
    x:pnode;
  begin
  x:=head^.r;
  while (x<>z) and (itemC.Compare(v,x^.key)<>0) do
    if (itemC.Compare(v,x^.key)<0) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;
procedure tRtcSearchList<itemType,infoType>.RemoveThis(var t:pnode);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=infoNil;
  t^.key:=itemMin;
  del_node(t);
  t:=z;
  end;
procedure tRtcSearchList<itemType,infoType>.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=infoNil;
  head^.key:=itemMin;
  cnt:=0;
  end;
function tRtcSearchList<itemType,infoType>.Search(const v:itemType):infoType;
  var
    x:pnode;
  begin
  x:=head^.r;
  while (x<>z) and (itemC.Compare(v,x^.key)<>0) do
    if (itemC.Compare(v,x^.key)<0) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcSearchList<itemType,infoType>.Search_Min(var i:infoType):itemType;
  var
    x:pnode;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=infoNil;
    Result:=itemMin;
    end;
  end;
function tRtcSearchList<itemType,infoType>.Search_Max(var i:infoType):itemType;
  var
    x:pnode;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=infoNil;
    Result:=itemMin;
    end;
  end;
function tRtcSearchList<itemType,infoType>.Search_L(const v:itemType; var i:infoType):itemType;
  var
    x,y:pnode;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (itemC.Compare(x^.key,v)<0) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (itemC.Compare(x^.key,v)=0) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;
function tRtcSearchList<itemType,infoType>.Search_G(const v:itemType; var i:infoType):itemType;
  var
    x,y:pnode;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (itemC.Compare(x^.key,v)>0) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (itemC.Compare(x^.key,v)=0) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;
function tRtcSearchList<itemType,infoType>.Search_LE(const v:itemType; var i:infoType):itemType;
  var
    x,y:pnode;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (itemC.Compare(v,x^.key)<>0) do
    begin
    if (itemC.Compare(x^.key,v)<0) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;
function tRtcSearchList<itemType,infoType>.Search_GE(const v:itemType; var i:infoType):itemType;
  var
    x,y:pnode;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (itemC.Compare(v,x^.key)<>0) do
    begin
    if (itemC.Compare(x^.key,v)>0) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;
procedure tRtcSearchList<itemType,infoType>.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (itemC.Compare(xv,g^.key)<0)<>(itemC.Compare(xv,p^.key)<0) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (itemC.Compare(xv,c^.key)<0) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (itemC.Compare(xv,g^.key)<0) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (itemC.Compare(xv,c^.key)<0) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (itemC.Compare(xv,gg^.key)<0) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;
procedure tRtcSearchList<itemType,infoType>.Insert(const v:itemType;const info:infoType);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (itemC.Compare(v,x^.key)<0) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (itemC.Compare(v,p^.key)<0) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;
procedure tRtcSearchList<itemType,infoType>.Remove_AddParentNode(node:pnode);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;
function tRtcSearchList<itemType,infoType>.Remove_GetParentNode:pnode;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;
procedure tRtcSearchList<itemType,infoType>.Remove(const v:itemType);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (itemC.Compare(v,t^.key)<>0) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (itemC.Compare(v,t^.key)<0) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=infoNil;
  t^.key:=itemMin;
  del_node(t);
  Dec(cnt);
  end;
function tRtcSearchList<itemType,infoType>.Count: cardinal;
  begin
  Result:=cnt;
  end;
{$ENDIF} // {$IFDEF RTC_WANT_GENERICS}

function tRtcList_CI32.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_CI32.New_Node(const k:trtcItemType_CI32; const i:trtcInfoType_CI32; const bi:boolean; const ll,rr:prtcNode_CI32):prtcNode_CI32;
  var
    p:prtcNodeArr_CI32;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_CI32);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_CI32));
  FillChar(Result^,SizeOf(trtcNode_CI32),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_CI32.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_CI32.Del_Node(node:prtcNode_CI32);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tRtcList_CI32.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_CI32,vrtcInfoNIL_CI32,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_CI32,vrtcInfoNIL_CI32,false,z,z);
  New(Parents);
  end;

procedure tRtcList_CI32.Change(const v:trtcItemType_CI32;const info:trtcInfoType_CI32);
  var
    x:prtcNode_CI32;
    res:integer;
  begin
  x:=head^.r;
  while x<>z do
    begin
    res:=FUpdateCompare(v,x^.key);
    if res<0 then x:=x^.l
    else if res>0 then x:=x^.r
    else Break;
    end;
  x^.info:=info;
  end;

procedure tRtcList_CI32.RemoveThis(var t:prtcNode_CI32);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_CI32;
  t^.key:=vrtcItemMin_CI32;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_CI32.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_CI32;
  head^.key:=vrtcItemMin_CI32;
  cnt:=0;
  end;

destructor tRtcList_CI32.Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_CI32;
    head^.key:=vrtcItemMin_CI32;
    del_node(head);
    end;

  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_CI32;
    z^.key:=vrtcItemMin_CI32;
    del_node(z);
    end;

  if Parents<>nil then Dispose(Parents);

  for a:=0 to Length(myPools)-1 do
    begin
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    end;
  SetLength(myPools,0);
  
  if assigned(pool) then pool.Free;

  inherited;
  end;

function tRtcList_CI32.Search(const v:trtcItemType_CI32):trtcInfoType_CI32;
  var
    x:prtcNode_CI32;
    res:integer;
  begin
  x:=head^.r;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res<0 then x:=x^.l
    else if res>0 then x:=x^.r
    else Break;
    end;
  Result:=x^.info;
  end;

function tRtcList_CI32.Search_Near(const v:trtcItemType_CI32; var res:integer):trtcInfoType_CI32;
  var
    x:prtcNode_CI32;
  begin
  res:=1;
  x:=head^.r;
  y:=z;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res<0 then
      begin
      y:=x;
      x:=x^.l;
      end
    else if res>0 then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      y:=x;
      Break;
      end;
    end;
  Result:=y^.info;
  end;

function tRtcList_CI32.Search_Min(var i:trtcInfoType_CI32):trtcItemType_CI32;
  var
    x:prtcNode_CI32;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_CI32;
    Result:=vrtcItemMin_CI32;
    end;
  end;

function tRtcList_CI32.Search_Max(var i:trtcInfoType_CI32):trtcItemType_CI32;
  var
    x:prtcNode_CI32;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_CI32;
    Result:=vrtcItemMin_CI32;
    end;
  end;

function tRtcList_CI32.Search_L(const v:trtcItemType_CI32; var i:trtcInfoType_CI32):trtcItemType_CI32;
  var
    x,y:prtcNode_CI32;
    res:integer;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res>0 then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (res=0) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_CI32.Search_G(const v:trtcItemType_CI32; var i:trtcInfoType_CI32):trtcItemType_CI32;
  var
    x,y:prtcNode_CI32;
    res:integer;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res<0 then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (res=0) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_CI32.Search_LE(const v:trtcItemType_CI32; var i:trtcInfoType_CI32):trtcItemType_CI32;
  var
    x,y:prtcNode_CI32;
    res:integer;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res>0 then
      begin
      y:=x;
      x:=x^.r;
      end
    else if res<0 then
      x:=x^.l
    else
      Break;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_CI32.Search_GE(const v:trtcItemType_CI32; var i:trtcInfoType_CI32):trtcItemType_CI32;
  var
    x,y:prtcNode_CI32;
    res:integer;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res<0 then
      begin
      y:=x;
      x:=x^.l;
      end
    else if res>0 then
      x:=x^.r
    else
      Break;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_CI32.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (FUpdateCompare(xv,g^.key)<0)<>(FUpdateCompare(xv,p^.key)<0) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (FUpdateCompare(xv,c^.key)<0) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (FUpdateCompare(xv,g^.key)<0) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (FUpdateCompare(xv,c^.key)<0) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (FUpdateCompare(xv,gg^.key)<0) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

function tRtcList_CI32.Insert(const v:trtcItemType_CI32;const info:trtcInfoType_CI32):trtcInfoType_CI32;
  var
    res:integer;
  begin
  x:=head^.r;
  while x<>z do
    begin
    res:=FUpdateCompare(v,x^.key);
    if res<0 then x:=x^.l
    else if res>0 then x:=x^.r
    else Break;
    end;
  Result:=x^.info;

  if Result=vrtcInfoNIL_CI32 then
    begin
    xv:=v;
    // xinfo:=info;
    nx:=new_node(v,info,True,z,z);
    // Key Sort
    x:=head; p:=head; g:=head;
    while (x<>z) do
      begin
      gg:=g; g:=p; p:=x;
      if (FUpdateCompare(v,x^.key)<0) then x:=x^.l else x:=x^.r;
      if (x^.l^.b and x^.r^.b) then Insert_split;
      end;
    x:=nx;
    if (FUpdateCompare(v,p^.key)<0) then p^.l:=x else p^.r:=x;
    Insert_Split;

    Inc(cnt);
    end;
  end;

procedure tRtcList_CI32.Remove_AddParentNode(node:prtcNode_CI32);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_CI32.Remove_GetParentNode:prtcNode_CI32;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;
  
procedure tRtcList_CI32.Remove(const v:trtcItemType_CI32);
  var
    a:byte;

  begin
  Parents^.NodeCount:=0;

  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (FUpdateCompare(v,t^.key)<>0) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (FUpdateCompare(v,t^.key)<0) then t:=t^.l else t:=t^.r;
    end;

  if t=z then
    raise ERtcSearch.Create('Key not found !');

  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;

  t^.info:=vrtcInfoNIL_CI32;
  t^.key:=vrtcItemMin_CI32;
  del_node(t);

  Dec(cnt);
  end;

function tRtcList_CI32.Count: cardinal;
  begin
  Result:=cnt;
  end;

function tRtcList_CI64.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_CI64.New_Node(const k:trtcItemType_CI64; const i:trtcInfoType_CI64; const bi:boolean; const ll,rr:prtcNode_CI64):prtcNode_CI64;
  var
    p:prtcNodeArr_CI64;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_CI64);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_CI64));
  FillChar(Result^,SizeOf(trtcNode_CI64),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_CI64.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_CI64.Del_Node(node:prtcNode_CI64);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tRtcList_CI64.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_CI64,vrtcInfoNIL_CI64,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_CI64,vrtcInfoNIL_CI64,false,z,z);
  New(Parents);
  end;

procedure tRtcList_CI64.Change(const v:trtcItemType_CI64;const info:trtcInfoType_CI64);
  var
    x:prtcNode_CI64;
    res:integer;
  begin
  x:=head^.r;
  while x<>z do
    begin
    res:=FUpdateCompare(v,x^.key);
    if res<0 then x:=x^.l
    else if res>0 then x:=x^.r
    else Break;
    end;
  x^.info:=info;
  end;

procedure tRtcList_CI64.RemoveThis(var t:prtcNode_CI64);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_CI64;
  t^.key:=vrtcItemMin_CI64;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_CI64.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_CI64;
  head^.key:=vrtcItemMin_CI64;
  cnt:=0;
  end;

destructor tRtcList_CI64.Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_CI64;
    head^.key:=vrtcItemMin_CI64;
    del_node(head);
    end;

  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_CI64;
    z^.key:=vrtcItemMin_CI64;
    del_node(z);
    end;

  if Parents<>nil then Dispose(Parents);

  for a:=0 to Length(myPools)-1 do
    begin
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    end;
  SetLength(myPools,0);
  
  if assigned(pool) then pool.Free;

  inherited;
  end;

function tRtcList_CI64.Search(const v:trtcItemType_CI64):trtcInfoType_CI64;
  var
    x:prtcNode_CI64;
    res:integer;
  begin
  x:=head^.r;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res<0 then x:=x^.l
    else if res>0 then x:=x^.r
    else Break;
    end;
  Result:=x^.info;
  end;

function tRtcList_CI64.Search_Near(const v:trtcItemType_CI64; var res:integer):trtcInfoType_CI64;
  var
    x:prtcNode_CI64;
  begin
  res:=1;
  x:=head^.r;
  y:=z;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res<0 then
      begin
      y:=x;
      x:=x^.l;
      end
    else if res>0 then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      y:=x;
      Break;
      end;
    end;
  Result:=y^.info;
  end;

function tRtcList_CI64.Search_Min(var i:trtcInfoType_CI64):trtcItemType_CI64;
  var
    x:prtcNode_CI64;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_CI64;
    Result:=vrtcItemMin_CI64;
    end;
  end;

function tRtcList_CI64.Search_Max(var i:trtcInfoType_CI64):trtcItemType_CI64;
  var
    x:prtcNode_CI64;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_CI64;
    Result:=vrtcItemMin_CI64;
    end;
  end;

function tRtcList_CI64.Search_L(const v:trtcItemType_CI64; var i:trtcInfoType_CI64):trtcItemType_CI64;
  var
    x,y:prtcNode_CI64;
    res:integer;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res>0 then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (res=0) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_CI64.Search_G(const v:trtcItemType_CI64; var i:trtcInfoType_CI64):trtcItemType_CI64;
  var
    x,y:prtcNode_CI64;
    res:integer;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res<0 then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (res=0) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_CI64.Search_LE(const v:trtcItemType_CI64; var i:trtcInfoType_CI64):trtcItemType_CI64;
  var
    x,y:prtcNode_CI64;
    res:integer;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res>0 then
      begin
      y:=x;
      x:=x^.r;
      end
    else if res<0 then
      x:=x^.l
    else
      Break;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_CI64.Search_GE(const v:trtcItemType_CI64; var i:trtcInfoType_CI64):trtcItemType_CI64;
  var
    x,y:prtcNode_CI64;
    res:integer;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    res:=FSearchCompare(v,x^.key);
    if res<0 then
      begin
      y:=x;
      x:=x^.l;
      end
    else if res>0 then
      x:=x^.r
    else
      Break;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_CI64.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (FUpdateCompare(xv,g^.key)<0)<>(FUpdateCompare(xv,p^.key)<0) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (FUpdateCompare(xv,c^.key)<0) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (FUpdateCompare(xv,g^.key)<0) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (FUpdateCompare(xv,c^.key)<0) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (FUpdateCompare(xv,gg^.key)<0) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

function tRtcList_CI64.Insert(const v:trtcItemType_CI64;const info:trtcInfoType_CI64):trtcInfoType_CI64;
  var
    res:integer;
  begin
  x:=head^.r;
  while x<>z do
    begin
    res:=FUpdateCompare(v,x^.key);
    if res<0 then x:=x^.l
    else if res>0 then x:=x^.r
    else Break;
    end;
  Result:=x^.info;

  if Result=vrtcInfoNIL_CI64 then
    begin
    xv:=v;
    // xinfo:=info;
    nx:=new_node(v,info,True,z,z);
    // Key Sort
    x:=head; p:=head; g:=head;
    while (x<>z) do
      begin
      gg:=g; g:=p; p:=x;
      if (FUpdateCompare(v,x^.key)<0) then x:=x^.l else x:=x^.r;
      if (x^.l^.b and x^.r^.b) then Insert_split;
      end;
    x:=nx;
    if (FUpdateCompare(v,p^.key)<0) then p^.l:=x else p^.r:=x;
    Insert_Split;

    Inc(cnt);
    end;
  end;

procedure tRtcList_CI64.Remove_AddParentNode(node:prtcNode_CI64);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_CI64.Remove_GetParentNode:prtcNode_CI64;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;
  
procedure tRtcList_CI64.Remove(const v:trtcItemType_CI64);
  var
    a:byte;

  begin
  Parents^.NodeCount:=0;

  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (FUpdateCompare(v,t^.key)<>0) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (FUpdateCompare(v,t^.key)<0) then t:=t^.l else t:=t^.r;
    end;

  if t=z then
    raise ERtcSearch.Create('Key not found !');

  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;

  t^.info:=vrtcInfoNIL_CI64;
  t^.key:=vrtcItemMin_CI64;
  del_node(t);

  Dec(cnt);
  end;

function tRtcList_CI64.Count: cardinal;
  begin
  Result:=cnt;
  end;

function tRtcList_LI32.Empty:boolean;
  begin
  Result:= (cnt=0);
  end;

function tRtcList_LI32.New_Node(const i:trtcInfoType_LI32; const pri,nex:prtcNode_LI32):prtcNode_LI32;
  var
    a:longint;
    p:prtcNodeArr_LI32;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_LI32);
      GetMem(p,a); // Create new list
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_LI32));
  FillChar(Result^,SizeOf(trtcNode_LI32),0);
  with Result^ do
    begin
    info:=i;
    prior:=pri;
    next:=nex;
    end;
  end;

procedure tRtcList_LI32.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_LI32.Del_Node(node:prtcNode_LI32);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tRtcList_LI32.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  end;

procedure tRtcList_LI32.RemoveAll;
  var
    x:prtcNode_LI32;
  begin
  while fFirst<>nil do
    begin
    x:=fFirst; fFirst:=fFirst^.next;
    with x^ do
      begin
      info:=vrtcInfoNIL_LI32;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    end;
  fLast:=nil;
  cnt:=0;
  end;

destructor tRtcList_LI32.Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  for a:=0 to Length(myPools)-1 do
    begin
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    end;
  SetLength(myPools,0);
  RtcFreeAndNil(pool);

  inherited;
  end;

function tRtcList_LI32.Count: cardinal;
  begin
  Result:=cnt;
  end;

procedure tRtcList_LI32.addFirst(const info: trtcInfoType_LI32);
  var
    nn:prtcNode_LI32;
  begin
  nn:=new_node(info,nil,fFirst);
  if fFirst<>nil then
    fFirst^.prior:=nn;
  fFirst:=nn;
  if fLast=nil then
    fLast:=fFirst;
  Inc(cnt);
  end;

procedure tRtcList_LI32.addLast(const info: trtcInfoType_LI32);
  var
    nn:prtcNode_LI32;
  begin
  nn:=new_node(info,fLast,nil);
  if fLast<>nil then
    fLast^.next:=nn;
  fLast:=nn;
  if fFirst=nil then
    fFirst:=fLast;
  Inc(cnt);
  end;

function tRtcList_LI32.First: trtcInfoType_LI32;
  begin
  if fFirst=nil then
    Result:=vrtcInfoNIL_LI32
  else
    Result:=fFirst^.info;
  end;

function tRtcList_LI32.Last: trtcInfoType_LI32;
  begin
  if fLast=nil then
    Result:=vrtcInfoNIL_LI32
  else
    Result:=fLast^.info;
  end;

procedure tRtcList_LI32.removeFirst;
  var
    x:prtcNode_LI32;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      info:=vrtcInfoNIL_LI32;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end;
  end;

procedure tRtcList_LI32.removeLast;
  var
    x:prtcNode_LI32;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      info:=vrtcInfoNIL_LI32;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end;
  end;

procedure tRtcList_LI32.extractFirst(var exInfo:trtcInfoType_LI32);
  var
    x:prtcNode_LI32;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LI32;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LI32;
  end;

procedure tRtcList_LI32.extractLast(var exInfo:trtcInfoType_LI32);
  var
    x:prtcNode_LI32;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LI32;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LI32;
  end;

procedure tRtcList_LI32.removeThis(const info: trtcInfoType_LI32);
  var
    x:prtcNode_LI32;
  begin
  x:=fFirst;
  while (x<>nil) and (x^.info<>info) do
    x:=x^.next;

  if x<>nil then
    begin
    if x=fFirst then
      removeFirst
    else if x=fLast then
      removeLast
    else
      begin
      with x^ do
        begin
        prior^.next:=next;
        next^.prior:=prior;

        info:=vrtcInfoNIL_LI32;
        prior:=nil;
        next:=nil;
        end;
      del_node(x);
      Dec(cnt);
      end;
    end;
  end;

function tRtcList_LI64.Empty:boolean;
  begin
  Result:= (cnt=0);
  end;

function tRtcList_LI64.New_Node(const i:trtcInfoType_LI64; const pri,nex:prtcNode_LI64):prtcNode_LI64;
  var
    a:longint;
    p:prtcNodeArr_LI64;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_LI64);
      GetMem(p,a); // Create new list
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_LI64));
  FillChar(Result^,SizeOf(trtcNode_LI64),0);
  with Result^ do
    begin
    info:=i;
    prior:=pri;
    next:=nex;
    end;
  end;

procedure tRtcList_LI64.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_LI64.Del_Node(node:prtcNode_LI64);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tRtcList_LI64.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  end;

procedure tRtcList_LI64.RemoveAll;
  var
    x:prtcNode_LI64;
  begin
  while fFirst<>nil do
    begin
    x:=fFirst; fFirst:=fFirst^.next;
    with x^ do
      begin
      info:=vrtcInfoNIL_LI64;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    end;
  fLast:=nil;
  cnt:=0;
  end;

destructor tRtcList_LI64.Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  for a:=0 to Length(myPools)-1 do
    begin
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    end;
  SetLength(myPools,0);
  RtcFreeAndNil(pool);

  inherited;
  end;

function tRtcList_LI64.Count: cardinal;
  begin
  Result:=cnt;
  end;

procedure tRtcList_LI64.addFirst(const info: trtcInfoType_LI64);
  var
    nn:prtcNode_LI64;
  begin
  nn:=new_node(info,nil,fFirst);
  if fFirst<>nil then
    fFirst^.prior:=nn;
  fFirst:=nn;
  if fLast=nil then
    fLast:=fFirst;
  Inc(cnt);
  end;

procedure tRtcList_LI64.addLast(const info: trtcInfoType_LI64);
  var
    nn:prtcNode_LI64;
  begin
  nn:=new_node(info,fLast,nil);
  if fLast<>nil then
    fLast^.next:=nn;
  fLast:=nn;
  if fFirst=nil then
    fFirst:=fLast;
  Inc(cnt);
  end;

function tRtcList_LI64.First: trtcInfoType_LI64;
  begin
  if fFirst=nil then
    Result:=vrtcInfoNIL_LI64
  else
    Result:=fFirst^.info;
  end;

function tRtcList_LI64.Last: trtcInfoType_LI64;
  begin
  if fLast=nil then
    Result:=vrtcInfoNIL_LI64
  else
    Result:=fLast^.info;
  end;

procedure tRtcList_LI64.removeFirst;
  var
    x:prtcNode_LI64;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      info:=vrtcInfoNIL_LI64;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end;
  end;

procedure tRtcList_LI64.removeLast;
  var
    x:prtcNode_LI64;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      info:=vrtcInfoNIL_LI64;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end;
  end;

procedure tRtcList_LI64.extractFirst(var exInfo:trtcInfoType_LI64);
  var
    x:prtcNode_LI64;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LI64;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LI64;
  end;

procedure tRtcList_LI64.extractLast(var exInfo:trtcInfoType_LI64);
  var
    x:prtcNode_LI64;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LI64;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LI64;
  end;

procedure tRtcList_LI64.removeThis(const info: trtcInfoType_LI64);
  var
    x:prtcNode_LI64;
  begin
  x:=fFirst;
  while (x<>nil) and (x^.info<>info) do
    x:=x^.next;

  if x<>nil then
    begin
    if x=fFirst then
      removeFirst
    else if x=fLast then
      removeLast
    else
      begin
      with x^ do
        begin
        prior^.next:=next;
        next^.prior:=prior;

        info:=vrtcInfoNIL_LI64;
        prior:=nil;
        next:=nil;
        end;
      del_node(x);
      Dec(cnt);
      end;
    end;
  end;

function tRtcList_LIPtr.Empty:boolean;
  begin
  Result:= (cnt=0);
  end;

function tRtcList_LIPtr.New_Node(const i:trtcInfoType_LIPtr; const pri,nex:prtcNode_LIPtr):prtcNode_LIPtr;
  var
    a:longint;
    p:prtcNodeArr_LIPtr;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_LIPtr);
      GetMem(p,a); // Create new list
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_LIPtr));
  FillChar(Result^,SizeOf(trtcNode_LIPtr),0);
  with Result^ do
    begin
    info:=i;
    prior:=pri;
    next:=nex;
    end;
  end;

procedure tRtcList_LIPtr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_LIPtr.Del_Node(node:prtcNode_LIPtr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tRtcList_LIPtr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  end;

procedure tRtcList_LIPtr.RemoveAll;
  var
    x:prtcNode_LIPtr;
  begin
  while fFirst<>nil do
    begin
    x:=fFirst; fFirst:=fFirst^.next;
    with x^ do
      begin
      info:=vrtcInfoNIL_LIPtr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    end;
  fLast:=nil;
  cnt:=0;
  end;

destructor tRtcList_LIPtr.Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  for a:=0 to Length(myPools)-1 do
    begin
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    end;
  SetLength(myPools,0);
  RtcFreeAndNil(pool);

  inherited;
  end;

function tRtcList_LIPtr.Count: cardinal;
  begin
  Result:=cnt;
  end;

procedure tRtcList_LIPtr.addFirst(const info: trtcInfoType_LIPtr);
  var
    nn:prtcNode_LIPtr;
  begin
  nn:=new_node(info,nil,fFirst);
  if fFirst<>nil then
    fFirst^.prior:=nn;
  fFirst:=nn;
  if fLast=nil then
    fLast:=fFirst;
  Inc(cnt);
  end;

procedure tRtcList_LIPtr.addLast(const info: trtcInfoType_LIPtr);
  var
    nn:prtcNode_LIPtr;
  begin
  nn:=new_node(info,fLast,nil);
  if fLast<>nil then
    fLast^.next:=nn;
  fLast:=nn;
  if fFirst=nil then
    fFirst:=fLast;
  Inc(cnt);
  end;

function tRtcList_LIPtr.First: trtcInfoType_LIPtr;
  begin
  if fFirst=nil then
    Result:=vrtcInfoNIL_LIPtr
  else
    Result:=fFirst^.info;
  end;

function tRtcList_LIPtr.Last: trtcInfoType_LIPtr;
  begin
  if fLast=nil then
    Result:=vrtcInfoNIL_LIPtr
  else
    Result:=fLast^.info;
  end;

procedure tRtcList_LIPtr.removeFirst;
  var
    x:prtcNode_LIPtr;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      info:=vrtcInfoNIL_LIPtr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end;
  end;

procedure tRtcList_LIPtr.removeLast;
  var
    x:prtcNode_LIPtr;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      info:=vrtcInfoNIL_LIPtr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end;
  end;

procedure tRtcList_LIPtr.extractFirst(var exInfo:trtcInfoType_LIPtr);
  var
    x:prtcNode_LIPtr;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LIPtr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LIPtr;
  end;

procedure tRtcList_LIPtr.extractLast(var exInfo:trtcInfoType_LIPtr);
  var
    x:prtcNode_LIPtr;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LIPtr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LIPtr;
  end;

procedure tRtcList_LIPtr.removeThis(const info: trtcInfoType_LIPtr);
  var
    x:prtcNode_LIPtr;
  begin
  x:=fFirst;
  while (x<>nil) and (x^.info<>info) do
    x:=x^.next;

  if x<>nil then
    begin
    if x=fFirst then
      removeFirst
    else if x=fLast then
      removeLast
    else
      begin
      with x^ do
        begin
        prior^.next:=next;
        next^.prior:=prior;

        info:=vrtcInfoNIL_LIPtr;
        prior:=nil;
        next:=nil;
        end;
      del_node(x);
      Dec(cnt);
      end;
    end;
  end;

function tRtcList_LObj.Empty:boolean;
  begin
  Result:= (cnt=0);
  end;

function tRtcList_LObj.New_Node(const i:trtcInfoType_LObj; const pri,nex:prtcNode_LObj):prtcNode_LObj;
  var
    a:longint;
    p:prtcNodeArr_LObj;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_LObj);
      GetMem(p,a); // Create new list
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_LObj));
  FillChar(Result^,SizeOf(trtcNode_LObj),0);
  with Result^ do
    begin
    info:=i;
    prior:=pri;
    next:=nex;
    end;
  end;

procedure tRtcList_LObj.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_LObj.Del_Node(node:prtcNode_LObj);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tRtcList_LObj.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  end;

procedure tRtcList_LObj.RemoveAll;
  var
    x:prtcNode_LObj;
  begin
  while fFirst<>nil do
    begin
    x:=fFirst; fFirst:=fFirst^.next;
    with x^ do
      begin
      info:=vrtcInfoNIL_LObj;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    end;
  fLast:=nil;
  cnt:=0;
  end;

destructor tRtcList_LObj.Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  for a:=0 to Length(myPools)-1 do
    begin
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    end;
  SetLength(myPools,0);
  RtcFreeAndNil(pool);

  inherited;
  end;

function tRtcList_LObj.Count: cardinal;
  begin
  Result:=cnt;
  end;

procedure tRtcList_LObj.addFirst(const info: trtcInfoType_LObj);
  var
    nn:prtcNode_LObj;
  begin
  nn:=new_node(info,nil,fFirst);
  if fFirst<>nil then
    fFirst^.prior:=nn;
  fFirst:=nn;
  if fLast=nil then
    fLast:=fFirst;
  Inc(cnt);
  end;

procedure tRtcList_LObj.addLast(const info: trtcInfoType_LObj);
  var
    nn:prtcNode_LObj;
  begin
  nn:=new_node(info,fLast,nil);
  if fLast<>nil then
    fLast^.next:=nn;
  fLast:=nn;
  if fFirst=nil then
    fFirst:=fLast;
  Inc(cnt);
  end;

function tRtcList_LObj.First: trtcInfoType_LObj;
  begin
  if fFirst=nil then
    Result:=vrtcInfoNIL_LObj
  else
    Result:=fFirst^.info;
  end;

function tRtcList_LObj.Last: trtcInfoType_LObj;
  begin
  if fLast=nil then
    Result:=vrtcInfoNIL_LObj
  else
    Result:=fLast^.info;
  end;

procedure tRtcList_LObj.removeFirst;
  var
    x:prtcNode_LObj;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      info:=vrtcInfoNIL_LObj;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end;
  end;

procedure tRtcList_LObj.removeLast;
  var
    x:prtcNode_LObj;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      info:=vrtcInfoNIL_LObj;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end;
  end;

procedure tRtcList_LObj.extractFirst(var exInfo:trtcInfoType_LObj);
  var
    x:prtcNode_LObj;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LObj;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LObj;
  end;

procedure tRtcList_LObj.extractLast(var exInfo:trtcInfoType_LObj);
  var
    x:prtcNode_LObj;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LObj;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LObj;
  end;

procedure tRtcList_LObj.removeThis(const info: trtcInfoType_LObj);
  var
    x:prtcNode_LObj;
  begin
  x:=fFirst;
  while (x<>nil) and (x^.info<>info) do
    x:=x^.next;

  if x<>nil then
    begin
    if x=fFirst then
      removeFirst
    else if x=fLast then
      removeLast
    else
      begin
      with x^ do
        begin
        prior^.next:=next;
        next^.prior:=prior;

        info:=vrtcInfoNIL_LObj;
        prior:=nil;
        next:=nil;
        end;
      del_node(x);
      Dec(cnt);
      end;
    end;
  end;

function tRtcList_LPtr.Empty:boolean;
  begin
  Result:= (cnt=0);
  end;

function tRtcList_LPtr.New_Node(const i:trtcInfoType_LPtr; const pri,nex:prtcNode_LPtr):prtcNode_LPtr;
  var
    a:longint;
    p:prtcNodeArr_LPtr;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_LPtr);
      GetMem(p,a); // Create new list
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_LPtr));
  FillChar(Result^,SizeOf(trtcNode_LPtr),0);
  with Result^ do
    begin
    info:=i;
    prior:=pri;
    next:=nex;
    end;
  end;

procedure tRtcList_LPtr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_LPtr.Del_Node(node:prtcNode_LPtr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tRtcList_LPtr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  end;

procedure tRtcList_LPtr.RemoveAll;
  var
    x:prtcNode_LPtr;
  begin
  while fFirst<>nil do
    begin
    x:=fFirst; fFirst:=fFirst^.next;
    with x^ do
      begin
      info:=vrtcInfoNIL_LPtr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    end;
  fLast:=nil;
  cnt:=0;
  end;

destructor tRtcList_LPtr.Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  for a:=0 to Length(myPools)-1 do
    begin
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    end;
  SetLength(myPools,0);
  RtcFreeAndNil(pool);

  inherited;
  end;

function tRtcList_LPtr.Count: cardinal;
  begin
  Result:=cnt;
  end;

procedure tRtcList_LPtr.addFirst(const info: trtcInfoType_LPtr);
  var
    nn:prtcNode_LPtr;
  begin
  nn:=new_node(info,nil,fFirst);
  if fFirst<>nil then
    fFirst^.prior:=nn;
  fFirst:=nn;
  if fLast=nil then
    fLast:=fFirst;
  Inc(cnt);
  end;

procedure tRtcList_LPtr.addLast(const info: trtcInfoType_LPtr);
  var
    nn:prtcNode_LPtr;
  begin
  nn:=new_node(info,fLast,nil);
  if fLast<>nil then
    fLast^.next:=nn;
  fLast:=nn;
  if fFirst=nil then
    fFirst:=fLast;
  Inc(cnt);
  end;

function tRtcList_LPtr.First: trtcInfoType_LPtr;
  begin
  if fFirst=nil then
    Result:=vrtcInfoNIL_LPtr
  else
    Result:=fFirst^.info;
  end;

function tRtcList_LPtr.Last: trtcInfoType_LPtr;
  begin
  if fLast=nil then
    Result:=vrtcInfoNIL_LPtr
  else
    Result:=fLast^.info;
  end;

procedure tRtcList_LPtr.removeFirst;
  var
    x:prtcNode_LPtr;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      info:=vrtcInfoNIL_LPtr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end;
  end;

procedure tRtcList_LPtr.removeLast;
  var
    x:prtcNode_LPtr;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      info:=vrtcInfoNIL_LPtr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end;
  end;

procedure tRtcList_LPtr.extractFirst(var exInfo:trtcInfoType_LPtr);
  var
    x:prtcNode_LPtr;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LPtr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LPtr;
  end;

procedure tRtcList_LPtr.extractLast(var exInfo:trtcInfoType_LPtr);
  var
    x:prtcNode_LPtr;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LPtr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LPtr;
  end;

procedure tRtcList_LPtr.removeThis(const info: trtcInfoType_LPtr);
  var
    x:prtcNode_LPtr;
  begin
  x:=fFirst;
  while (x<>nil) and (x^.info<>info) do
    x:=x^.next;

  if x<>nil then
    begin
    if x=fFirst then
      removeFirst
    else if x=fLast then
      removeLast
    else
      begin
      with x^ do
        begin
        prior^.next:=next;
        next^.prior:=prior;

        info:=vrtcInfoNIL_LPtr;
        prior:=nil;
        next:=nil;
        end;
      del_node(x);
      Dec(cnt);
      end;
    end;
  end;

function tRtcList_LStr.Empty:boolean;
  begin
  Result:= (cnt=0);
  end;

function tRtcList_LStr.New_Node(const i:trtcInfoType_LStr; const pri,nex:prtcNode_LStr):prtcNode_LStr;
  var
    a:longint;
    p:prtcNodeArr_LStr;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_LStr);
      GetMem(p,a); // Create new list
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_LStr));
  FillChar(Result^,SizeOf(trtcNode_LStr),0);
  with Result^ do
    begin
    info:=i;
    prior:=pri;
    next:=nex;
    end;
  end;

procedure tRtcList_LStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_LStr.Del_Node(node:prtcNode_LStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tRtcList_LStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  end;

procedure tRtcList_LStr.RemoveAll;
  var
    x:prtcNode_LStr;
  begin
  while fFirst<>nil do
    begin
    x:=fFirst; fFirst:=fFirst^.next;
    with x^ do
      begin
      info:=vrtcInfoNIL_LStr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    end;
  fLast:=nil;
  cnt:=0;
  end;

destructor tRtcList_LStr.Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  for a:=0 to Length(myPools)-1 do
    begin
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    end;
  SetLength(myPools,0);
  RtcFreeAndNil(pool);

  inherited;
  end;

function tRtcList_LStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

procedure tRtcList_LStr.addFirst(const info: trtcInfoType_LStr);
  var
    nn:prtcNode_LStr;
  begin
  nn:=new_node(info,nil,fFirst);
  if fFirst<>nil then
    fFirst^.prior:=nn;
  fFirst:=nn;
  if fLast=nil then
    fLast:=fFirst;
  Inc(cnt);
  end;

procedure tRtcList_LStr.addLast(const info: trtcInfoType_LStr);
  var
    nn:prtcNode_LStr;
  begin
  nn:=new_node(info,fLast,nil);
  if fLast<>nil then
    fLast^.next:=nn;
  fLast:=nn;
  if fFirst=nil then
    fFirst:=fLast;
  Inc(cnt);
  end;

function tRtcList_LStr.First: trtcInfoType_LStr;
  begin
  if fFirst=nil then
    Result:=vrtcInfoNIL_LStr
  else
    Result:=fFirst^.info;
  end;

function tRtcList_LStr.Last: trtcInfoType_LStr;
  begin
  if fLast=nil then
    Result:=vrtcInfoNIL_LStr
  else
    Result:=fLast^.info;
  end;

procedure tRtcList_LStr.removeFirst;
  var
    x:prtcNode_LStr;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      info:=vrtcInfoNIL_LStr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end;
  end;

procedure tRtcList_LStr.removeLast;
  var
    x:prtcNode_LStr;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      info:=vrtcInfoNIL_LStr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end;
  end;

procedure tRtcList_LStr.extractFirst(var exInfo:trtcInfoType_LStr);
  var
    x:prtcNode_LStr;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LStr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LStr;
  end;

procedure tRtcList_LStr.extractLast(var exInfo:trtcInfoType_LStr);
  var
    x:prtcNode_LStr;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LStr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LStr;
  end;

procedure tRtcList_LStr.removeThis(const info: trtcInfoType_LStr);
  var
    x:prtcNode_LStr;
  begin
  x:=fFirst;
  while (x<>nil) and (x^.info<>info) do
    x:=x^.next;

  if x<>nil then
    begin
    if x=fFirst then
      removeFirst
    else if x=fLast then
      removeLast
    else
      begin
      with x^ do
        begin
        prior^.next:=next;
        next^.prior:=prior;

        info:=vrtcInfoNIL_LStr;
        prior:=nil;
        next:=nil;
        end;
      del_node(x);
      Dec(cnt);
      end;
    end;
  end;

function tRtcList_LWStr.Empty:boolean;
  begin
  Result:= (cnt=0);
  end;

function tRtcList_LWStr.New_Node(const i:trtcInfoType_LWStr; const pri,nex:prtcNode_LWStr):prtcNode_LWStr;
  var
    a:longint;
    p:prtcNodeArr_LWStr;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_LWStr);
      GetMem(p,a); // Create new list
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_LWStr));
  FillChar(Result^,SizeOf(trtcNode_LWStr),0);
  with Result^ do
    begin
    info:=i;
    prior:=pri;
    next:=nex;
    end;
  end;

procedure tRtcList_LWStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_LWStr.Del_Node(node:prtcNode_LWStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tRtcList_LWStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  end;

procedure tRtcList_LWStr.RemoveAll;
  var
    x:prtcNode_LWStr;
  begin
  while fFirst<>nil do
    begin
    x:=fFirst; fFirst:=fFirst^.next;
    with x^ do
      begin
      info:=vrtcInfoNIL_LWStr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    end;
  fLast:=nil;
  cnt:=0;
  end;

destructor tRtcList_LWStr.Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  for a:=0 to Length(myPools)-1 do
    begin
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    end;
  SetLength(myPools,0);
  RtcFreeAndNil(pool);

  inherited;
  end;

function tRtcList_LWStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

procedure tRtcList_LWStr.addFirst(const info: trtcInfoType_LWStr);
  var
    nn:prtcNode_LWStr;
  begin
  nn:=new_node(info,nil,fFirst);
  if fFirst<>nil then
    fFirst^.prior:=nn;
  fFirst:=nn;
  if fLast=nil then
    fLast:=fFirst;
  Inc(cnt);
  end;

procedure tRtcList_LWStr.addLast(const info: trtcInfoType_LWStr);
  var
    nn:prtcNode_LWStr;
  begin
  nn:=new_node(info,fLast,nil);
  if fLast<>nil then
    fLast^.next:=nn;
  fLast:=nn;
  if fFirst=nil then
    fFirst:=fLast;
  Inc(cnt);
  end;

function tRtcList_LWStr.First: trtcInfoType_LWStr;
  begin
  if fFirst=nil then
    Result:=vrtcInfoNIL_LWStr
  else
    Result:=fFirst^.info;
  end;

function tRtcList_LWStr.Last: trtcInfoType_LWStr;
  begin
  if fLast=nil then
    Result:=vrtcInfoNIL_LWStr
  else
    Result:=fLast^.info;
  end;

procedure tRtcList_LWStr.removeFirst;
  var
    x:prtcNode_LWStr;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      info:=vrtcInfoNIL_LWStr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end;
  end;

procedure tRtcList_LWStr.removeLast;
  var
    x:prtcNode_LWStr;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      info:=vrtcInfoNIL_LWStr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end;
  end;

procedure tRtcList_LWStr.extractFirst(var exInfo:trtcInfoType_LWStr);
  var
    x:prtcNode_LWStr;
  begin
  if fFirst<>nil then
    begin
    x:=fFirst;
    fFirst:=fFirst^.next;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LWStr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fFirst=nil then
      fLast:=nil
    else
      fFirst^.prior:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LWStr;
  end;

procedure tRtcList_LWStr.extractLast(var exInfo:trtcInfoType_LWStr);
  var
    x:prtcNode_LWStr;
  begin
  if fLast<>nil then
    begin
    x:=fLast;
    fLast:=fLast^.prior;

    with x^ do
      begin
      exInfo:=info;
      info:=vrtcInfoNIL_LWStr;
      prior:=nil;
      next:=nil;
      end;
    del_node(x);
    Dec(cnt);

    if fLast=nil then
      fFirst:=nil
    else
      fLast^.next:=nil;
    end
  else
    exInfo:=vrtcInfoNIL_LWStr;
  end;

procedure tRtcList_LWStr.removeThis(const info: trtcInfoType_LWStr);
  var
    x:prtcNode_LWStr;
  begin
  x:=fFirst;
  while (x<>nil) and (x^.info<>info) do
    x:=x^.next;

  if x<>nil then
    begin
    if x=fFirst then
      removeFirst
    else if x=fLast then
      removeLast
    else
      begin
      with x^ do
        begin
        prior^.next:=next;
        next^.prior:=prior;

        info:=vrtcInfoNIL_LWStr;
        prior:=nil;
        next:=nil;
        end;
      del_node(x);
      Dec(cnt);
      end;
    end;
  end;

constructor tRtcList_I32I32.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I32I32,vrtcInfoNIL_I32I32,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_I32I32,vrtcInfoNIL_I32I32,false,z,z);
  New(Parents);
  end;

destructor tRtcList_I32I32.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_I32I32;
    head^.key:=vrtcItemMin_I32I32;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_I32I32;
    z^.key:=vrtcItemMin_I32I32;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_I32I32.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_I32I32.New_Node(const k:trtcItemType_I32I32; const i:trtcInfoType_I32I32; const bi:boolean; const ll,rr:prtcNode_I32I32):prtcNode_I32I32;
  var
    p:prtcNodeArr_I32I32;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_I32I32);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_I32I32));
  FillChar(Result^,SizeOf(trtcNode_I32I32),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_I32I32.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_I32I32.Del_Node(node:prtcNode_I32I32);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_I32I32.Change(const v:trtcItemType_I32I32;const info:trtcInfoType_I32I32);
  var
    x:prtcNode_I32I32;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_I32I32.RemoveThis(var t:prtcNode_I32I32);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I32I32;
  t^.key:=vrtcItemMin_I32I32;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_I32I32.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I32I32;
  head^.key:=vrtcItemMin_I32I32;
  cnt:=0;
  end;

function tRtcList_I32I32.Search(const v:trtcItemType_I32I32):trtcInfoType_I32I32;
  var
    x:prtcNode_I32I32;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_I32I32.Search_Min(var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x:prtcNode_I32I32;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I32I32;
    Result:=vrtcItemMin_I32I32;
    end;
  end;

function tRtcList_I32I32.Search_Max(var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x:prtcNode_I32I32;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I32I32;
    Result:=vrtcItemMin_I32I32;
    end;
  end;

function tRtcList_I32I32.Search_L(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x,y:prtcNode_I32I32;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I32I32.Search_G(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x,y:prtcNode_I32I32;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I32I32.Search_LE(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x,y:prtcNode_I32I32;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_I32I32.Search_GE(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x,y:prtcNode_I32I32;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_I32I32.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_I32I32.Insert(const v:trtcItemType_I32I32;const info:trtcInfoType_I32I32);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_I32I32.Remove_AddParentNode(node:prtcNode_I32I32);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_I32I32.Remove_GetParentNode:prtcNode_I32I32;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_I32I32.Remove(const v:trtcItemType_I32I32);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I32I32;
  t^.key:=vrtcItemMin_I32I32;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_I32I32.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_I32Obj.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I32Obj,vrtcInfoNIL_I32Obj,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_I32Obj,vrtcInfoNIL_I32Obj,false,z,z);
  New(Parents);
  end;

destructor tRtcList_I32Obj.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_I32Obj;
    head^.key:=vrtcItemMin_I32Obj;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_I32Obj;
    z^.key:=vrtcItemMin_I32Obj;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_I32Obj.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_I32Obj.New_Node(const k:trtcItemType_I32Obj; const i:trtcInfoType_I32Obj; const bi:boolean; const ll,rr:prtcNode_I32Obj):prtcNode_I32Obj;
  var
    p:prtcNodeArr_I32Obj;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_I32Obj);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_I32Obj));
  FillChar(Result^,SizeOf(trtcNode_I32Obj),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_I32Obj.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_I32Obj.Del_Node(node:prtcNode_I32Obj);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_I32Obj.Change(const v:trtcItemType_I32Obj;const info:trtcInfoType_I32Obj);
  var
    x:prtcNode_I32Obj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_I32Obj.RemoveThis(var t:prtcNode_I32Obj);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I32Obj;
  t^.key:=vrtcItemMin_I32Obj;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_I32Obj.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I32Obj;
  head^.key:=vrtcItemMin_I32Obj;
  cnt:=0;
  end;

function tRtcList_I32Obj.Search(const v:trtcItemType_I32Obj):trtcInfoType_I32Obj;
  var
    x:prtcNode_I32Obj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_I32Obj.Search_Min(var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x:prtcNode_I32Obj;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I32Obj;
    Result:=vrtcItemMin_I32Obj;
    end;
  end;

function tRtcList_I32Obj.Search_Max(var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x:prtcNode_I32Obj;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I32Obj;
    Result:=vrtcItemMin_I32Obj;
    end;
  end;

function tRtcList_I32Obj.Search_L(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x,y:prtcNode_I32Obj;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I32Obj.Search_G(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x,y:prtcNode_I32Obj;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I32Obj.Search_LE(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x,y:prtcNode_I32Obj;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_I32Obj.Search_GE(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x,y:prtcNode_I32Obj;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_I32Obj.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_I32Obj.Insert(const v:trtcItemType_I32Obj;const info:trtcInfoType_I32Obj);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_I32Obj.Remove_AddParentNode(node:prtcNode_I32Obj);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_I32Obj.Remove_GetParentNode:prtcNode_I32Obj;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_I32Obj.Remove(const v:trtcItemType_I32Obj);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I32Obj;
  t^.key:=vrtcItemMin_I32Obj;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_I32Obj.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_I32Ptr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I32Ptr,vrtcInfoNIL_I32Ptr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_I32Ptr,vrtcInfoNIL_I32Ptr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_I32Ptr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_I32Ptr;
    head^.key:=vrtcItemMin_I32Ptr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_I32Ptr;
    z^.key:=vrtcItemMin_I32Ptr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_I32Ptr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_I32Ptr.New_Node(const k:trtcItemType_I32Ptr; const i:trtcInfoType_I32Ptr; const bi:boolean; const ll,rr:prtcNode_I32Ptr):prtcNode_I32Ptr;
  var
    p:prtcNodeArr_I32Ptr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_I32Ptr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_I32Ptr));
  FillChar(Result^,SizeOf(trtcNode_I32Ptr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_I32Ptr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_I32Ptr.Del_Node(node:prtcNode_I32Ptr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_I32Ptr.Change(const v:trtcItemType_I32Ptr;const info:trtcInfoType_I32Ptr);
  var
    x:prtcNode_I32Ptr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_I32Ptr.RemoveThis(var t:prtcNode_I32Ptr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I32Ptr;
  t^.key:=vrtcItemMin_I32Ptr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_I32Ptr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I32Ptr;
  head^.key:=vrtcItemMin_I32Ptr;
  cnt:=0;
  end;

function tRtcList_I32Ptr.Search(const v:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;
  var
    x:prtcNode_I32Ptr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_I32Ptr.Search_Min(var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x:prtcNode_I32Ptr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I32Ptr;
    Result:=vrtcItemMin_I32Ptr;
    end;
  end;

function tRtcList_I32Ptr.Search_Max(var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x:prtcNode_I32Ptr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I32Ptr;
    Result:=vrtcItemMin_I32Ptr;
    end;
  end;

function tRtcList_I32Ptr.Search_L(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x,y:prtcNode_I32Ptr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I32Ptr.Search_G(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x,y:prtcNode_I32Ptr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I32Ptr.Search_LE(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x,y:prtcNode_I32Ptr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_I32Ptr.Search_GE(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x,y:prtcNode_I32Ptr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_I32Ptr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_I32Ptr.Insert(const v:trtcItemType_I32Ptr;const info:trtcInfoType_I32Ptr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_I32Ptr.Remove_AddParentNode(node:prtcNode_I32Ptr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_I32Ptr.Remove_GetParentNode:prtcNode_I32Ptr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_I32Ptr.Remove(const v:trtcItemType_I32Ptr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I32Ptr;
  t^.key:=vrtcItemMin_I32Ptr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_I32Ptr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_I32Str.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I32Str,vrtcInfoNIL_I32Str,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_I32Str,vrtcInfoNIL_I32Str,false,z,z);
  New(Parents);
  end;

destructor tRtcList_I32Str.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_I32Str;
    head^.key:=vrtcItemMin_I32Str;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_I32Str;
    z^.key:=vrtcItemMin_I32Str;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_I32Str.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_I32Str.New_Node(const k:trtcItemType_I32Str; const i:trtcInfoType_I32Str; const bi:boolean; const ll,rr:prtcNode_I32Str):prtcNode_I32Str;
  var
    p:prtcNodeArr_I32Str;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_I32Str);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_I32Str));
  FillChar(Result^,SizeOf(trtcNode_I32Str),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_I32Str.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_I32Str.Del_Node(node:prtcNode_I32Str);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_I32Str.Change(const v:trtcItemType_I32Str;const info:trtcInfoType_I32Str);
  var
    x:prtcNode_I32Str;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_I32Str.RemoveThis(var t:prtcNode_I32Str);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I32Str;
  t^.key:=vrtcItemMin_I32Str;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_I32Str.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I32Str;
  head^.key:=vrtcItemMin_I32Str;
  cnt:=0;
  end;

function tRtcList_I32Str.Search(const v:trtcItemType_I32Str):trtcInfoType_I32Str;
  var
    x:prtcNode_I32Str;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_I32Str.Search_Min(var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x:prtcNode_I32Str;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I32Str;
    Result:=vrtcItemMin_I32Str;
    end;
  end;

function tRtcList_I32Str.Search_Max(var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x:prtcNode_I32Str;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I32Str;
    Result:=vrtcItemMin_I32Str;
    end;
  end;

function tRtcList_I32Str.Search_L(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x,y:prtcNode_I32Str;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I32Str.Search_G(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x,y:prtcNode_I32Str;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I32Str.Search_LE(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x,y:prtcNode_I32Str;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_I32Str.Search_GE(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x,y:prtcNode_I32Str;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_I32Str.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_I32Str.Insert(const v:trtcItemType_I32Str;const info:trtcInfoType_I32Str);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_I32Str.Remove_AddParentNode(node:prtcNode_I32Str);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_I32Str.Remove_GetParentNode:prtcNode_I32Str;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_I32Str.Remove(const v:trtcItemType_I32Str);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I32Str;
  t^.key:=vrtcItemMin_I32Str;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_I32Str.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_I32WStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I32WStr,vrtcInfoNIL_I32WStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_I32WStr,vrtcInfoNIL_I32WStr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_I32WStr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_I32WStr;
    head^.key:=vrtcItemMin_I32WStr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_I32WStr;
    z^.key:=vrtcItemMin_I32WStr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_I32WStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_I32WStr.New_Node(const k:trtcItemType_I32WStr; const i:trtcInfoType_I32WStr; const bi:boolean; const ll,rr:prtcNode_I32WStr):prtcNode_I32WStr;
  var
    p:prtcNodeArr_I32WStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_I32WStr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_I32WStr));
  FillChar(Result^,SizeOf(trtcNode_I32WStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_I32WStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_I32WStr.Del_Node(node:prtcNode_I32WStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_I32WStr.Change(const v:trtcItemType_I32WStr;const info:trtcInfoType_I32WStr);
  var
    x:prtcNode_I32WStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_I32WStr.RemoveThis(var t:prtcNode_I32WStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I32WStr;
  t^.key:=vrtcItemMin_I32WStr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_I32WStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I32WStr;
  head^.key:=vrtcItemMin_I32WStr;
  cnt:=0;
  end;

function tRtcList_I32WStr.Search(const v:trtcItemType_I32WStr):trtcInfoType_I32WStr;
  var
    x:prtcNode_I32WStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_I32WStr.Search_Min(var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x:prtcNode_I32WStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I32WStr;
    Result:=vrtcItemMin_I32WStr;
    end;
  end;

function tRtcList_I32WStr.Search_Max(var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x:prtcNode_I32WStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I32WStr;
    Result:=vrtcItemMin_I32WStr;
    end;
  end;

function tRtcList_I32WStr.Search_L(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x,y:prtcNode_I32WStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I32WStr.Search_G(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x,y:prtcNode_I32WStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I32WStr.Search_LE(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x,y:prtcNode_I32WStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_I32WStr.Search_GE(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x,y:prtcNode_I32WStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_I32WStr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_I32WStr.Insert(const v:trtcItemType_I32WStr;const info:trtcInfoType_I32WStr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_I32WStr.Remove_AddParentNode(node:prtcNode_I32WStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_I32WStr.Remove_GetParentNode:prtcNode_I32WStr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_I32WStr.Remove(const v:trtcItemType_I32WStr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I32WStr;
  t^.key:=vrtcItemMin_I32WStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_I32WStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_I64I64.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I64I64,vrtcInfoNIL_I64I64,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_I64I64,vrtcInfoNIL_I64I64,false,z,z);
  New(Parents);
  end;

destructor tRtcList_I64I64.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_I64I64;
    head^.key:=vrtcItemMin_I64I64;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_I64I64;
    z^.key:=vrtcItemMin_I64I64;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_I64I64.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_I64I64.New_Node(const k:trtcItemType_I64I64; const i:trtcInfoType_I64I64; const bi:boolean; const ll,rr:prtcNode_I64I64):prtcNode_I64I64;
  var
    p:prtcNodeArr_I64I64;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_I64I64);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_I64I64));
  FillChar(Result^,SizeOf(trtcNode_I64I64),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_I64I64.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_I64I64.Del_Node(node:prtcNode_I64I64);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_I64I64.Change(const v:trtcItemType_I64I64;const info:trtcInfoType_I64I64);
  var
    x:prtcNode_I64I64;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_I64I64.RemoveThis(var t:prtcNode_I64I64);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I64I64;
  t^.key:=vrtcItemMin_I64I64;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_I64I64.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I64I64;
  head^.key:=vrtcItemMin_I64I64;
  cnt:=0;
  end;

function tRtcList_I64I64.Search(const v:trtcItemType_I64I64):trtcInfoType_I64I64;
  var
    x:prtcNode_I64I64;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_I64I64.Search_Min(var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x:prtcNode_I64I64;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I64I64;
    Result:=vrtcItemMin_I64I64;
    end;
  end;

function tRtcList_I64I64.Search_Max(var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x:prtcNode_I64I64;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I64I64;
    Result:=vrtcItemMin_I64I64;
    end;
  end;

function tRtcList_I64I64.Search_L(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x,y:prtcNode_I64I64;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I64I64.Search_G(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x,y:prtcNode_I64I64;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I64I64.Search_LE(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x,y:prtcNode_I64I64;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_I64I64.Search_GE(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x,y:prtcNode_I64I64;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_I64I64.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_I64I64.Insert(const v:trtcItemType_I64I64;const info:trtcInfoType_I64I64);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_I64I64.Remove_AddParentNode(node:prtcNode_I64I64);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_I64I64.Remove_GetParentNode:prtcNode_I64I64;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_I64I64.Remove(const v:trtcItemType_I64I64);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I64I64;
  t^.key:=vrtcItemMin_I64I64;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_I64I64.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_I64Obj.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I64Obj,vrtcInfoNIL_I64Obj,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_I64Obj,vrtcInfoNIL_I64Obj,false,z,z);
  New(Parents);
  end;

destructor tRtcList_I64Obj.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_I64Obj;
    head^.key:=vrtcItemMin_I64Obj;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_I64Obj;
    z^.key:=vrtcItemMin_I64Obj;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_I64Obj.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_I64Obj.New_Node(const k:trtcItemType_I64Obj; const i:trtcInfoType_I64Obj; const bi:boolean; const ll,rr:prtcNode_I64Obj):prtcNode_I64Obj;
  var
    p:prtcNodeArr_I64Obj;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_I64Obj);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_I64Obj));
  FillChar(Result^,SizeOf(trtcNode_I64Obj),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_I64Obj.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_I64Obj.Del_Node(node:prtcNode_I64Obj);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_I64Obj.Change(const v:trtcItemType_I64Obj;const info:trtcInfoType_I64Obj);
  var
    x:prtcNode_I64Obj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_I64Obj.RemoveThis(var t:prtcNode_I64Obj);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I64Obj;
  t^.key:=vrtcItemMin_I64Obj;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_I64Obj.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I64Obj;
  head^.key:=vrtcItemMin_I64Obj;
  cnt:=0;
  end;

function tRtcList_I64Obj.Search(const v:trtcItemType_I64Obj):trtcInfoType_I64Obj;
  var
    x:prtcNode_I64Obj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_I64Obj.Search_Min(var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x:prtcNode_I64Obj;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I64Obj;
    Result:=vrtcItemMin_I64Obj;
    end;
  end;

function tRtcList_I64Obj.Search_Max(var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x:prtcNode_I64Obj;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I64Obj;
    Result:=vrtcItemMin_I64Obj;
    end;
  end;

function tRtcList_I64Obj.Search_L(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x,y:prtcNode_I64Obj;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I64Obj.Search_G(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x,y:prtcNode_I64Obj;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I64Obj.Search_LE(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x,y:prtcNode_I64Obj;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_I64Obj.Search_GE(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x,y:prtcNode_I64Obj;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_I64Obj.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_I64Obj.Insert(const v:trtcItemType_I64Obj;const info:trtcInfoType_I64Obj);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_I64Obj.Remove_AddParentNode(node:prtcNode_I64Obj);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_I64Obj.Remove_GetParentNode:prtcNode_I64Obj;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_I64Obj.Remove(const v:trtcItemType_I64Obj);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I64Obj;
  t^.key:=vrtcItemMin_I64Obj;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_I64Obj.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_I64Ptr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I64Ptr,vrtcInfoNIL_I64Ptr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_I64Ptr,vrtcInfoNIL_I64Ptr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_I64Ptr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_I64Ptr;
    head^.key:=vrtcItemMin_I64Ptr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_I64Ptr;
    z^.key:=vrtcItemMin_I64Ptr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_I64Ptr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_I64Ptr.New_Node(const k:trtcItemType_I64Ptr; const i:trtcInfoType_I64Ptr; const bi:boolean; const ll,rr:prtcNode_I64Ptr):prtcNode_I64Ptr;
  var
    p:prtcNodeArr_I64Ptr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_I64Ptr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_I64Ptr));
  FillChar(Result^,SizeOf(trtcNode_I64Ptr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_I64Ptr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_I64Ptr.Del_Node(node:prtcNode_I64Ptr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_I64Ptr.Change(const v:trtcItemType_I64Ptr;const info:trtcInfoType_I64Ptr);
  var
    x:prtcNode_I64Ptr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_I64Ptr.RemoveThis(var t:prtcNode_I64Ptr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I64Ptr;
  t^.key:=vrtcItemMin_I64Ptr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_I64Ptr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I64Ptr;
  head^.key:=vrtcItemMin_I64Ptr;
  cnt:=0;
  end;

function tRtcList_I64Ptr.Search(const v:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;
  var
    x:prtcNode_I64Ptr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_I64Ptr.Search_Min(var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x:prtcNode_I64Ptr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I64Ptr;
    Result:=vrtcItemMin_I64Ptr;
    end;
  end;

function tRtcList_I64Ptr.Search_Max(var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x:prtcNode_I64Ptr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I64Ptr;
    Result:=vrtcItemMin_I64Ptr;
    end;
  end;

function tRtcList_I64Ptr.Search_L(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x,y:prtcNode_I64Ptr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I64Ptr.Search_G(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x,y:prtcNode_I64Ptr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I64Ptr.Search_LE(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x,y:prtcNode_I64Ptr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_I64Ptr.Search_GE(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x,y:prtcNode_I64Ptr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_I64Ptr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_I64Ptr.Insert(const v:trtcItemType_I64Ptr;const info:trtcInfoType_I64Ptr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_I64Ptr.Remove_AddParentNode(node:prtcNode_I64Ptr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_I64Ptr.Remove_GetParentNode:prtcNode_I64Ptr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_I64Ptr.Remove(const v:trtcItemType_I64Ptr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I64Ptr;
  t^.key:=vrtcItemMin_I64Ptr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_I64Ptr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_I64Str.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I64Str,vrtcInfoNIL_I64Str,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_I64Str,vrtcInfoNIL_I64Str,false,z,z);
  New(Parents);
  end;

destructor tRtcList_I64Str.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_I64Str;
    head^.key:=vrtcItemMin_I64Str;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_I64Str;
    z^.key:=vrtcItemMin_I64Str;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_I64Str.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_I64Str.New_Node(const k:trtcItemType_I64Str; const i:trtcInfoType_I64Str; const bi:boolean; const ll,rr:prtcNode_I64Str):prtcNode_I64Str;
  var
    p:prtcNodeArr_I64Str;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_I64Str);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_I64Str));
  FillChar(Result^,SizeOf(trtcNode_I64Str),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_I64Str.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_I64Str.Del_Node(node:prtcNode_I64Str);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_I64Str.Change(const v:trtcItemType_I64Str;const info:trtcInfoType_I64Str);
  var
    x:prtcNode_I64Str;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_I64Str.RemoveThis(var t:prtcNode_I64Str);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I64Str;
  t^.key:=vrtcItemMin_I64Str;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_I64Str.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I64Str;
  head^.key:=vrtcItemMin_I64Str;
  cnt:=0;
  end;

function tRtcList_I64Str.Search(const v:trtcItemType_I64Str):trtcInfoType_I64Str;
  var
    x:prtcNode_I64Str;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_I64Str.Search_Min(var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x:prtcNode_I64Str;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I64Str;
    Result:=vrtcItemMin_I64Str;
    end;
  end;

function tRtcList_I64Str.Search_Max(var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x:prtcNode_I64Str;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I64Str;
    Result:=vrtcItemMin_I64Str;
    end;
  end;

function tRtcList_I64Str.Search_L(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x,y:prtcNode_I64Str;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I64Str.Search_G(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x,y:prtcNode_I64Str;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I64Str.Search_LE(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x,y:prtcNode_I64Str;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_I64Str.Search_GE(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x,y:prtcNode_I64Str;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_I64Str.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_I64Str.Insert(const v:trtcItemType_I64Str;const info:trtcInfoType_I64Str);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_I64Str.Remove_AddParentNode(node:prtcNode_I64Str);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_I64Str.Remove_GetParentNode:prtcNode_I64Str;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_I64Str.Remove(const v:trtcItemType_I64Str);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I64Str;
  t^.key:=vrtcItemMin_I64Str;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_I64Str.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_I64WStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I64WStr,vrtcInfoNIL_I64WStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_I64WStr,vrtcInfoNIL_I64WStr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_I64WStr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_I64WStr;
    head^.key:=vrtcItemMin_I64WStr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_I64WStr;
    z^.key:=vrtcItemMin_I64WStr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_I64WStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_I64WStr.New_Node(const k:trtcItemType_I64WStr; const i:trtcInfoType_I64WStr; const bi:boolean; const ll,rr:prtcNode_I64WStr):prtcNode_I64WStr;
  var
    p:prtcNodeArr_I64WStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_I64WStr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_I64WStr));
  FillChar(Result^,SizeOf(trtcNode_I64WStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_I64WStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_I64WStr.Del_Node(node:prtcNode_I64WStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_I64WStr.Change(const v:trtcItemType_I64WStr;const info:trtcInfoType_I64WStr);
  var
    x:prtcNode_I64WStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_I64WStr.RemoveThis(var t:prtcNode_I64WStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I64WStr;
  t^.key:=vrtcItemMin_I64WStr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_I64WStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I64WStr;
  head^.key:=vrtcItemMin_I64WStr;
  cnt:=0;
  end;

function tRtcList_I64WStr.Search(const v:trtcItemType_I64WStr):trtcInfoType_I64WStr;
  var
    x:prtcNode_I64WStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_I64WStr.Search_Min(var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x:prtcNode_I64WStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I64WStr;
    Result:=vrtcItemMin_I64WStr;
    end;
  end;

function tRtcList_I64WStr.Search_Max(var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x:prtcNode_I64WStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_I64WStr;
    Result:=vrtcItemMin_I64WStr;
    end;
  end;

function tRtcList_I64WStr.Search_L(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x,y:prtcNode_I64WStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I64WStr.Search_G(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x,y:prtcNode_I64WStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_I64WStr.Search_LE(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x,y:prtcNode_I64WStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_I64WStr.Search_GE(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x,y:prtcNode_I64WStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_I64WStr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_I64WStr.Insert(const v:trtcItemType_I64WStr;const info:trtcInfoType_I64WStr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_I64WStr.Remove_AddParentNode(node:prtcNode_I64WStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_I64WStr.Remove_GetParentNode:prtcNode_I64WStr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_I64WStr.Remove(const v:trtcItemType_I64WStr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I64WStr;
  t^.key:=vrtcItemMin_I64WStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_I64WStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_IPtrIPtr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_IPtrIPtr,vrtcInfoNIL_IPtrIPtr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_IPtrIPtr,vrtcInfoNIL_IPtrIPtr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_IPtrIPtr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_IPtrIPtr;
    head^.key:=vrtcItemMin_IPtrIPtr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_IPtrIPtr;
    z^.key:=vrtcItemMin_IPtrIPtr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_IPtrIPtr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_IPtrIPtr.New_Node(const k:trtcItemType_IPtrIPtr; const i:trtcInfoType_IPtrIPtr; const bi:boolean; const ll,rr:prtcNode_IPtrIPtr):prtcNode_IPtrIPtr;
  var
    p:prtcNodeArr_IPtrIPtr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_IPtrIPtr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_IPtrIPtr));
  FillChar(Result^,SizeOf(trtcNode_IPtrIPtr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_IPtrIPtr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_IPtrIPtr.Del_Node(node:prtcNode_IPtrIPtr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_IPtrIPtr.Change(const v:trtcItemType_IPtrIPtr;const info:trtcInfoType_IPtrIPtr);
  var
    x:prtcNode_IPtrIPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_IPtrIPtr.RemoveThis(var t:prtcNode_IPtrIPtr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_IPtrIPtr;
  t^.key:=vrtcItemMin_IPtrIPtr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_IPtrIPtr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_IPtrIPtr;
  head^.key:=vrtcItemMin_IPtrIPtr;
  cnt:=0;
  end;

function tRtcList_IPtrIPtr.Search(const v:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;
  var
    x:prtcNode_IPtrIPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_IPtrIPtr.Search_Min(var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x:prtcNode_IPtrIPtr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_IPtrIPtr;
    Result:=vrtcItemMin_IPtrIPtr;
    end;
  end;

function tRtcList_IPtrIPtr.Search_Max(var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x:prtcNode_IPtrIPtr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_IPtrIPtr;
    Result:=vrtcItemMin_IPtrIPtr;
    end;
  end;

function tRtcList_IPtrIPtr.Search_L(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x,y:prtcNode_IPtrIPtr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_IPtrIPtr.Search_G(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x,y:prtcNode_IPtrIPtr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_IPtrIPtr.Search_LE(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x,y:prtcNode_IPtrIPtr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_IPtrIPtr.Search_GE(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x,y:prtcNode_IPtrIPtr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_IPtrIPtr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_IPtrIPtr.Insert(const v:trtcItemType_IPtrIPtr;const info:trtcInfoType_IPtrIPtr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_IPtrIPtr.Remove_AddParentNode(node:prtcNode_IPtrIPtr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_IPtrIPtr.Remove_GetParentNode:prtcNode_IPtrIPtr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_IPtrIPtr.Remove(const v:trtcItemType_IPtrIPtr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_IPtrIPtr;
  t^.key:=vrtcItemMin_IPtrIPtr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_IPtrIPtr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_IPtrObj.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_IPtrObj,vrtcInfoNIL_IPtrObj,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_IPtrObj,vrtcInfoNIL_IPtrObj,false,z,z);
  New(Parents);
  end;

destructor tRtcList_IPtrObj.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_IPtrObj;
    head^.key:=vrtcItemMin_IPtrObj;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_IPtrObj;
    z^.key:=vrtcItemMin_IPtrObj;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_IPtrObj.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_IPtrObj.New_Node(const k:trtcItemType_IPtrObj; const i:trtcInfoType_IPtrObj; const bi:boolean; const ll,rr:prtcNode_IPtrObj):prtcNode_IPtrObj;
  var
    p:prtcNodeArr_IPtrObj;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_IPtrObj);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_IPtrObj));
  FillChar(Result^,SizeOf(trtcNode_IPtrObj),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_IPtrObj.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_IPtrObj.Del_Node(node:prtcNode_IPtrObj);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_IPtrObj.Change(const v:trtcItemType_IPtrObj;const info:trtcInfoType_IPtrObj);
  var
    x:prtcNode_IPtrObj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_IPtrObj.RemoveThis(var t:prtcNode_IPtrObj);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_IPtrObj;
  t^.key:=vrtcItemMin_IPtrObj;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_IPtrObj.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_IPtrObj;
  head^.key:=vrtcItemMin_IPtrObj;
  cnt:=0;
  end;

function tRtcList_IPtrObj.Search(const v:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;
  var
    x:prtcNode_IPtrObj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_IPtrObj.Search_Min(var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x:prtcNode_IPtrObj;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_IPtrObj;
    Result:=vrtcItemMin_IPtrObj;
    end;
  end;

function tRtcList_IPtrObj.Search_Max(var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x:prtcNode_IPtrObj;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_IPtrObj;
    Result:=vrtcItemMin_IPtrObj;
    end;
  end;

function tRtcList_IPtrObj.Search_L(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x,y:prtcNode_IPtrObj;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_IPtrObj.Search_G(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x,y:prtcNode_IPtrObj;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_IPtrObj.Search_LE(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x,y:prtcNode_IPtrObj;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_IPtrObj.Search_GE(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x,y:prtcNode_IPtrObj;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_IPtrObj.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_IPtrObj.Insert(const v:trtcItemType_IPtrObj;const info:trtcInfoType_IPtrObj);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_IPtrObj.Remove_AddParentNode(node:prtcNode_IPtrObj);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_IPtrObj.Remove_GetParentNode:prtcNode_IPtrObj;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_IPtrObj.Remove(const v:trtcItemType_IPtrObj);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_IPtrObj;
  t^.key:=vrtcItemMin_IPtrObj;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_IPtrObj.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_IPtrPtr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_IPtrPtr,vrtcInfoNIL_IPtrPtr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_IPtrPtr,vrtcInfoNIL_IPtrPtr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_IPtrPtr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_IPtrPtr;
    head^.key:=vrtcItemMin_IPtrPtr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_IPtrPtr;
    z^.key:=vrtcItemMin_IPtrPtr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_IPtrPtr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_IPtrPtr.New_Node(const k:trtcItemType_IPtrPtr; const i:trtcInfoType_IPtrPtr; const bi:boolean; const ll,rr:prtcNode_IPtrPtr):prtcNode_IPtrPtr;
  var
    p:prtcNodeArr_IPtrPtr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_IPtrPtr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_IPtrPtr));
  FillChar(Result^,SizeOf(trtcNode_IPtrPtr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_IPtrPtr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_IPtrPtr.Del_Node(node:prtcNode_IPtrPtr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_IPtrPtr.Change(const v:trtcItemType_IPtrPtr;const info:trtcInfoType_IPtrPtr);
  var
    x:prtcNode_IPtrPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_IPtrPtr.RemoveThis(var t:prtcNode_IPtrPtr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_IPtrPtr;
  t^.key:=vrtcItemMin_IPtrPtr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_IPtrPtr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_IPtrPtr;
  head^.key:=vrtcItemMin_IPtrPtr;
  cnt:=0;
  end;

function tRtcList_IPtrPtr.Search(const v:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;
  var
    x:prtcNode_IPtrPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_IPtrPtr.Search_Min(var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x:prtcNode_IPtrPtr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_IPtrPtr;
    Result:=vrtcItemMin_IPtrPtr;
    end;
  end;

function tRtcList_IPtrPtr.Search_Max(var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x:prtcNode_IPtrPtr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_IPtrPtr;
    Result:=vrtcItemMin_IPtrPtr;
    end;
  end;

function tRtcList_IPtrPtr.Search_L(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x,y:prtcNode_IPtrPtr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_IPtrPtr.Search_G(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x,y:prtcNode_IPtrPtr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_IPtrPtr.Search_LE(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x,y:prtcNode_IPtrPtr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_IPtrPtr.Search_GE(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x,y:prtcNode_IPtrPtr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_IPtrPtr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_IPtrPtr.Insert(const v:trtcItemType_IPtrPtr;const info:trtcInfoType_IPtrPtr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_IPtrPtr.Remove_AddParentNode(node:prtcNode_IPtrPtr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_IPtrPtr.Remove_GetParentNode:prtcNode_IPtrPtr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_IPtrPtr.Remove(const v:trtcItemType_IPtrPtr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_IPtrPtr;
  t^.key:=vrtcItemMin_IPtrPtr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_IPtrPtr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_IPtrStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_IPtrStr,vrtcInfoNIL_IPtrStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_IPtrStr,vrtcInfoNIL_IPtrStr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_IPtrStr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_IPtrStr;
    head^.key:=vrtcItemMin_IPtrStr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_IPtrStr;
    z^.key:=vrtcItemMin_IPtrStr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_IPtrStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_IPtrStr.New_Node(const k:trtcItemType_IPtrStr; const i:trtcInfoType_IPtrStr; const bi:boolean; const ll,rr:prtcNode_IPtrStr):prtcNode_IPtrStr;
  var
    p:prtcNodeArr_IPtrStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_IPtrStr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_IPtrStr));
  FillChar(Result^,SizeOf(trtcNode_IPtrStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_IPtrStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_IPtrStr.Del_Node(node:prtcNode_IPtrStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_IPtrStr.Change(const v:trtcItemType_IPtrStr;const info:trtcInfoType_IPtrStr);
  var
    x:prtcNode_IPtrStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_IPtrStr.RemoveThis(var t:prtcNode_IPtrStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_IPtrStr;
  t^.key:=vrtcItemMin_IPtrStr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_IPtrStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_IPtrStr;
  head^.key:=vrtcItemMin_IPtrStr;
  cnt:=0;
  end;

function tRtcList_IPtrStr.Search(const v:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;
  var
    x:prtcNode_IPtrStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_IPtrStr.Search_Min(var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x:prtcNode_IPtrStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_IPtrStr;
    Result:=vrtcItemMin_IPtrStr;
    end;
  end;

function tRtcList_IPtrStr.Search_Max(var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x:prtcNode_IPtrStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_IPtrStr;
    Result:=vrtcItemMin_IPtrStr;
    end;
  end;

function tRtcList_IPtrStr.Search_L(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x,y:prtcNode_IPtrStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_IPtrStr.Search_G(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x,y:prtcNode_IPtrStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_IPtrStr.Search_LE(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x,y:prtcNode_IPtrStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_IPtrStr.Search_GE(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x,y:prtcNode_IPtrStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_IPtrStr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_IPtrStr.Insert(const v:trtcItemType_IPtrStr;const info:trtcInfoType_IPtrStr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_IPtrStr.Remove_AddParentNode(node:prtcNode_IPtrStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_IPtrStr.Remove_GetParentNode:prtcNode_IPtrStr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_IPtrStr.Remove(const v:trtcItemType_IPtrStr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_IPtrStr;
  t^.key:=vrtcItemMin_IPtrStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_IPtrStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_IPtrWStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_IPtrWStr,vrtcInfoNIL_IPtrWStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_IPtrWStr,vrtcInfoNIL_IPtrWStr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_IPtrWStr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_IPtrWStr;
    head^.key:=vrtcItemMin_IPtrWStr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_IPtrWStr;
    z^.key:=vrtcItemMin_IPtrWStr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_IPtrWStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_IPtrWStr.New_Node(const k:trtcItemType_IPtrWStr; const i:trtcInfoType_IPtrWStr; const bi:boolean; const ll,rr:prtcNode_IPtrWStr):prtcNode_IPtrWStr;
  var
    p:prtcNodeArr_IPtrWStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_IPtrWStr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_IPtrWStr));
  FillChar(Result^,SizeOf(trtcNode_IPtrWStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_IPtrWStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_IPtrWStr.Del_Node(node:prtcNode_IPtrWStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_IPtrWStr.Change(const v:trtcItemType_IPtrWStr;const info:trtcInfoType_IPtrWStr);
  var
    x:prtcNode_IPtrWStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_IPtrWStr.RemoveThis(var t:prtcNode_IPtrWStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_IPtrWStr;
  t^.key:=vrtcItemMin_IPtrWStr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_IPtrWStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_IPtrWStr;
  head^.key:=vrtcItemMin_IPtrWStr;
  cnt:=0;
  end;

function tRtcList_IPtrWStr.Search(const v:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;
  var
    x:prtcNode_IPtrWStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_IPtrWStr.Search_Min(var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x:prtcNode_IPtrWStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_IPtrWStr;
    Result:=vrtcItemMin_IPtrWStr;
    end;
  end;

function tRtcList_IPtrWStr.Search_Max(var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x:prtcNode_IPtrWStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_IPtrWStr;
    Result:=vrtcItemMin_IPtrWStr;
    end;
  end;

function tRtcList_IPtrWStr.Search_L(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x,y:prtcNode_IPtrWStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_IPtrWStr.Search_G(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x,y:prtcNode_IPtrWStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_IPtrWStr.Search_LE(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x,y:prtcNode_IPtrWStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_IPtrWStr.Search_GE(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x,y:prtcNode_IPtrWStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_IPtrWStr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_IPtrWStr.Insert(const v:trtcItemType_IPtrWStr;const info:trtcInfoType_IPtrWStr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_IPtrWStr.Remove_AddParentNode(node:prtcNode_IPtrWStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_IPtrWStr.Remove_GetParentNode:prtcNode_IPtrWStr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_IPtrWStr.Remove(const v:trtcItemType_IPtrWStr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_IPtrWStr;
  t^.key:=vrtcItemMin_IPtrWStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_IPtrWStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_StrI32.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_StrI32,vrtcInfoNIL_StrI32,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_StrI32,vrtcInfoNIL_StrI32,false,z,z);
  New(Parents);
  end;

destructor tRtcList_StrI32.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_StrI32;
    head^.key:=vrtcItemMin_StrI32;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_StrI32;
    z^.key:=vrtcItemMin_StrI32;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_StrI32.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_StrI32.New_Node(const k:trtcItemType_StrI32; const i:trtcInfoType_StrI32; const bi:boolean; const ll,rr:prtcNode_StrI32):prtcNode_StrI32;
  var
    p:prtcNodeArr_StrI32;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_StrI32);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_StrI32));
  FillChar(Result^,SizeOf(trtcNode_StrI32),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_StrI32.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_StrI32.Del_Node(node:prtcNode_StrI32);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_StrI32.Change(const v:trtcItemType_StrI32;const info:trtcInfoType_StrI32);
  var
    x:prtcNode_StrI32;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_StrI32.RemoveThis(var t:prtcNode_StrI32);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_StrI32;
  t^.key:=vrtcItemMin_StrI32;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_StrI32.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_StrI32;
  head^.key:=vrtcItemMin_StrI32;
  cnt:=0;
  end;

function tRtcList_StrI32.Search(const v:trtcItemType_StrI32):trtcInfoType_StrI32;
  var
    x:prtcNode_StrI32;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_StrI32.Search_Min(var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x:prtcNode_StrI32;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_StrI32;
    Result:=vrtcItemMin_StrI32;
    end;
  end;

function tRtcList_StrI32.Search_Max(var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x:prtcNode_StrI32;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_StrI32;
    Result:=vrtcItemMin_StrI32;
    end;
  end;

function tRtcList_StrI32.Search_L(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x,y:prtcNode_StrI32;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_StrI32.Search_G(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x,y:prtcNode_StrI32;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_StrI32.Search_LE(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x,y:prtcNode_StrI32;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_StrI32.Search_GE(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x,y:prtcNode_StrI32;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_StrI32.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_StrI32.Insert(const v:trtcItemType_StrI32;const info:trtcInfoType_StrI32);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_StrI32.Remove_AddParentNode(node:prtcNode_StrI32);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_StrI32.Remove_GetParentNode:prtcNode_StrI32;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_StrI32.Remove(const v:trtcItemType_StrI32);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_StrI32;
  t^.key:=vrtcItemMin_StrI32;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_StrI32.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_StrI64.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_StrI64,vrtcInfoNIL_StrI64,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_StrI64,vrtcInfoNIL_StrI64,false,z,z);
  New(Parents);
  end;

destructor tRtcList_StrI64.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_StrI64;
    head^.key:=vrtcItemMin_StrI64;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_StrI64;
    z^.key:=vrtcItemMin_StrI64;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_StrI64.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_StrI64.New_Node(const k:trtcItemType_StrI64; const i:trtcInfoType_StrI64; const bi:boolean; const ll,rr:prtcNode_StrI64):prtcNode_StrI64;
  var
    p:prtcNodeArr_StrI64;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_StrI64);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_StrI64));
  FillChar(Result^,SizeOf(trtcNode_StrI64),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_StrI64.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_StrI64.Del_Node(node:prtcNode_StrI64);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_StrI64.Change(const v:trtcItemType_StrI64;const info:trtcInfoType_StrI64);
  var
    x:prtcNode_StrI64;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_StrI64.RemoveThis(var t:prtcNode_StrI64);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_StrI64;
  t^.key:=vrtcItemMin_StrI64;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_StrI64.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_StrI64;
  head^.key:=vrtcItemMin_StrI64;
  cnt:=0;
  end;

function tRtcList_StrI64.Search(const v:trtcItemType_StrI64):trtcInfoType_StrI64;
  var
    x:prtcNode_StrI64;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_StrI64.Search_Min(var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x:prtcNode_StrI64;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_StrI64;
    Result:=vrtcItemMin_StrI64;
    end;
  end;

function tRtcList_StrI64.Search_Max(var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x:prtcNode_StrI64;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_StrI64;
    Result:=vrtcItemMin_StrI64;
    end;
  end;

function tRtcList_StrI64.Search_L(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x,y:prtcNode_StrI64;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_StrI64.Search_G(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x,y:prtcNode_StrI64;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_StrI64.Search_LE(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x,y:prtcNode_StrI64;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_StrI64.Search_GE(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x,y:prtcNode_StrI64;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_StrI64.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_StrI64.Insert(const v:trtcItemType_StrI64;const info:trtcInfoType_StrI64);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_StrI64.Remove_AddParentNode(node:prtcNode_StrI64);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_StrI64.Remove_GetParentNode:prtcNode_StrI64;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_StrI64.Remove(const v:trtcItemType_StrI64);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_StrI64;
  t^.key:=vrtcItemMin_StrI64;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_StrI64.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_StrObj.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_StrObj,vrtcInfoNIL_StrObj,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_StrObj,vrtcInfoNIL_StrObj,false,z,z);
  New(Parents);
  end;

destructor tRtcList_StrObj.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_StrObj;
    head^.key:=vrtcItemMin_StrObj;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_StrObj;
    z^.key:=vrtcItemMin_StrObj;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_StrObj.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_StrObj.New_Node(const k:trtcItemType_StrObj; const i:trtcInfoType_StrObj; const bi:boolean; const ll,rr:prtcNode_StrObj):prtcNode_StrObj;
  var
    p:prtcNodeArr_StrObj;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_StrObj);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_StrObj));
  FillChar(Result^,SizeOf(trtcNode_StrObj),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_StrObj.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_StrObj.Del_Node(node:prtcNode_StrObj);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_StrObj.Change(const v:trtcItemType_StrObj;const info:trtcInfoType_StrObj);
  var
    x:prtcNode_StrObj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_StrObj.RemoveThis(var t:prtcNode_StrObj);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_StrObj;
  t^.key:=vrtcItemMin_StrObj;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_StrObj.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_StrObj;
  head^.key:=vrtcItemMin_StrObj;
  cnt:=0;
  end;

function tRtcList_StrObj.Search(const v:trtcItemType_StrObj):trtcInfoType_StrObj;
  var
    x:prtcNode_StrObj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_StrObj.Search_Min(var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x:prtcNode_StrObj;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_StrObj;
    Result:=vrtcItemMin_StrObj;
    end;
  end;

function tRtcList_StrObj.Search_Max(var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x:prtcNode_StrObj;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_StrObj;
    Result:=vrtcItemMin_StrObj;
    end;
  end;

function tRtcList_StrObj.Search_L(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x,y:prtcNode_StrObj;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_StrObj.Search_G(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x,y:prtcNode_StrObj;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_StrObj.Search_LE(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x,y:prtcNode_StrObj;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_StrObj.Search_GE(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x,y:prtcNode_StrObj;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_StrObj.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_StrObj.Insert(const v:trtcItemType_StrObj;const info:trtcInfoType_StrObj);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_StrObj.Remove_AddParentNode(node:prtcNode_StrObj);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_StrObj.Remove_GetParentNode:prtcNode_StrObj;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_StrObj.Remove(const v:trtcItemType_StrObj);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_StrObj;
  t^.key:=vrtcItemMin_StrObj;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_StrObj.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_StrPtr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_StrPtr,vrtcInfoNIL_StrPtr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_StrPtr,vrtcInfoNIL_StrPtr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_StrPtr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_StrPtr;
    head^.key:=vrtcItemMin_StrPtr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_StrPtr;
    z^.key:=vrtcItemMin_StrPtr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_StrPtr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_StrPtr.New_Node(const k:trtcItemType_StrPtr; const i:trtcInfoType_StrPtr; const bi:boolean; const ll,rr:prtcNode_StrPtr):prtcNode_StrPtr;
  var
    p:prtcNodeArr_StrPtr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_StrPtr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_StrPtr));
  FillChar(Result^,SizeOf(trtcNode_StrPtr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_StrPtr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_StrPtr.Del_Node(node:prtcNode_StrPtr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_StrPtr.Change(const v:trtcItemType_StrPtr;const info:trtcInfoType_StrPtr);
  var
    x:prtcNode_StrPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_StrPtr.RemoveThis(var t:prtcNode_StrPtr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_StrPtr;
  t^.key:=vrtcItemMin_StrPtr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_StrPtr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_StrPtr;
  head^.key:=vrtcItemMin_StrPtr;
  cnt:=0;
  end;

function tRtcList_StrPtr.Search(const v:trtcItemType_StrPtr):trtcInfoType_StrPtr;
  var
    x:prtcNode_StrPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_StrPtr.Search_Min(var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x:prtcNode_StrPtr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_StrPtr;
    Result:=vrtcItemMin_StrPtr;
    end;
  end;

function tRtcList_StrPtr.Search_Max(var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x:prtcNode_StrPtr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_StrPtr;
    Result:=vrtcItemMin_StrPtr;
    end;
  end;

function tRtcList_StrPtr.Search_L(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x,y:prtcNode_StrPtr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_StrPtr.Search_G(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x,y:prtcNode_StrPtr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_StrPtr.Search_LE(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x,y:prtcNode_StrPtr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_StrPtr.Search_GE(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x,y:prtcNode_StrPtr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_StrPtr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_StrPtr.Insert(const v:trtcItemType_StrPtr;const info:trtcInfoType_StrPtr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_StrPtr.Remove_AddParentNode(node:prtcNode_StrPtr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_StrPtr.Remove_GetParentNode:prtcNode_StrPtr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_StrPtr.Remove(const v:trtcItemType_StrPtr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_StrPtr;
  t^.key:=vrtcItemMin_StrPtr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_StrPtr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_StrStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_StrStr,vrtcInfoNIL_StrStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_StrStr,vrtcInfoNIL_StrStr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_StrStr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_StrStr;
    head^.key:=vrtcItemMin_StrStr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_StrStr;
    z^.key:=vrtcItemMin_StrStr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_StrStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_StrStr.New_Node(const k:trtcItemType_StrStr; const i:trtcInfoType_StrStr; const bi:boolean; const ll,rr:prtcNode_StrStr):prtcNode_StrStr;
  var
    p:prtcNodeArr_StrStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_StrStr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_StrStr));
  FillChar(Result^,SizeOf(trtcNode_StrStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_StrStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_StrStr.Del_Node(node:prtcNode_StrStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_StrStr.Change(const v:trtcItemType_StrStr;const info:trtcInfoType_StrStr);
  var
    x:prtcNode_StrStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_StrStr.RemoveThis(var t:prtcNode_StrStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_StrStr;
  t^.key:=vrtcItemMin_StrStr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_StrStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_StrStr;
  head^.key:=vrtcItemMin_StrStr;
  cnt:=0;
  end;

function tRtcList_StrStr.Search(const v:trtcItemType_StrStr):trtcInfoType_StrStr;
  var
    x:prtcNode_StrStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_StrStr.Search_Min(var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x:prtcNode_StrStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_StrStr;
    Result:=vrtcItemMin_StrStr;
    end;
  end;

function tRtcList_StrStr.Search_Max(var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x:prtcNode_StrStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_StrStr;
    Result:=vrtcItemMin_StrStr;
    end;
  end;

function tRtcList_StrStr.Search_L(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x,y:prtcNode_StrStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_StrStr.Search_G(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x,y:prtcNode_StrStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_StrStr.Search_LE(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x,y:prtcNode_StrStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_StrStr.Search_GE(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x,y:prtcNode_StrStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_StrStr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_StrStr.Insert(const v:trtcItemType_StrStr;const info:trtcInfoType_StrStr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_StrStr.Remove_AddParentNode(node:prtcNode_StrStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_StrStr.Remove_GetParentNode:prtcNode_StrStr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_StrStr.Remove(const v:trtcItemType_StrStr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_StrStr;
  t^.key:=vrtcItemMin_StrStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_StrStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_WStrI32.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_WStrI32,vrtcInfoNIL_WStrI32,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_WStrI32,vrtcInfoNIL_WStrI32,false,z,z);
  New(Parents);
  end;

destructor tRtcList_WStrI32.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_WStrI32;
    head^.key:=vrtcItemMin_WStrI32;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_WStrI32;
    z^.key:=vrtcItemMin_WStrI32;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_WStrI32.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_WStrI32.New_Node(const k:trtcItemType_WStrI32; const i:trtcInfoType_WStrI32; const bi:boolean; const ll,rr:prtcNode_WStrI32):prtcNode_WStrI32;
  var
    p:prtcNodeArr_WStrI32;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_WStrI32);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_WStrI32));
  FillChar(Result^,SizeOf(trtcNode_WStrI32),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_WStrI32.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_WStrI32.Del_Node(node:prtcNode_WStrI32);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_WStrI32.Change(const v:trtcItemType_WStrI32;const info:trtcInfoType_WStrI32);
  var
    x:prtcNode_WStrI32;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_WStrI32.RemoveThis(var t:prtcNode_WStrI32);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_WStrI32;
  t^.key:=vrtcItemMin_WStrI32;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_WStrI32.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_WStrI32;
  head^.key:=vrtcItemMin_WStrI32;
  cnt:=0;
  end;

function tRtcList_WStrI32.Search(const v:trtcItemType_WStrI32):trtcInfoType_WStrI32;
  var
    x:prtcNode_WStrI32;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_WStrI32.Search_Min(var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x:prtcNode_WStrI32;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_WStrI32;
    Result:=vrtcItemMin_WStrI32;
    end;
  end;

function tRtcList_WStrI32.Search_Max(var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x:prtcNode_WStrI32;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_WStrI32;
    Result:=vrtcItemMin_WStrI32;
    end;
  end;

function tRtcList_WStrI32.Search_L(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x,y:prtcNode_WStrI32;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_WStrI32.Search_G(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x,y:prtcNode_WStrI32;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_WStrI32.Search_LE(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x,y:prtcNode_WStrI32;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_WStrI32.Search_GE(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x,y:prtcNode_WStrI32;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_WStrI32.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_WStrI32.Insert(const v:trtcItemType_WStrI32;const info:trtcInfoType_WStrI32);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_WStrI32.Remove_AddParentNode(node:prtcNode_WStrI32);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_WStrI32.Remove_GetParentNode:prtcNode_WStrI32;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_WStrI32.Remove(const v:trtcItemType_WStrI32);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_WStrI32;
  t^.key:=vrtcItemMin_WStrI32;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_WStrI32.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_WStrI64.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_WStrI64,vrtcInfoNIL_WStrI64,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_WStrI64,vrtcInfoNIL_WStrI64,false,z,z);
  New(Parents);
  end;

destructor tRtcList_WStrI64.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_WStrI64;
    head^.key:=vrtcItemMin_WStrI64;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_WStrI64;
    z^.key:=vrtcItemMin_WStrI64;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_WStrI64.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_WStrI64.New_Node(const k:trtcItemType_WStrI64; const i:trtcInfoType_WStrI64; const bi:boolean; const ll,rr:prtcNode_WStrI64):prtcNode_WStrI64;
  var
    p:prtcNodeArr_WStrI64;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_WStrI64);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_WStrI64));
  FillChar(Result^,SizeOf(trtcNode_WStrI64),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_WStrI64.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_WStrI64.Del_Node(node:prtcNode_WStrI64);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_WStrI64.Change(const v:trtcItemType_WStrI64;const info:trtcInfoType_WStrI64);
  var
    x:prtcNode_WStrI64;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_WStrI64.RemoveThis(var t:prtcNode_WStrI64);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_WStrI64;
  t^.key:=vrtcItemMin_WStrI64;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_WStrI64.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_WStrI64;
  head^.key:=vrtcItemMin_WStrI64;
  cnt:=0;
  end;

function tRtcList_WStrI64.Search(const v:trtcItemType_WStrI64):trtcInfoType_WStrI64;
  var
    x:prtcNode_WStrI64;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_WStrI64.Search_Min(var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x:prtcNode_WStrI64;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_WStrI64;
    Result:=vrtcItemMin_WStrI64;
    end;
  end;

function tRtcList_WStrI64.Search_Max(var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x:prtcNode_WStrI64;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_WStrI64;
    Result:=vrtcItemMin_WStrI64;
    end;
  end;

function tRtcList_WStrI64.Search_L(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x,y:prtcNode_WStrI64;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_WStrI64.Search_G(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x,y:prtcNode_WStrI64;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_WStrI64.Search_LE(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x,y:prtcNode_WStrI64;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_WStrI64.Search_GE(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x,y:prtcNode_WStrI64;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_WStrI64.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_WStrI64.Insert(const v:trtcItemType_WStrI64;const info:trtcInfoType_WStrI64);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_WStrI64.Remove_AddParentNode(node:prtcNode_WStrI64);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_WStrI64.Remove_GetParentNode:prtcNode_WStrI64;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_WStrI64.Remove(const v:trtcItemType_WStrI64);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_WStrI64;
  t^.key:=vrtcItemMin_WStrI64;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_WStrI64.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_WStrObj.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_WStrObj,vrtcInfoNIL_WStrObj,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_WStrObj,vrtcInfoNIL_WStrObj,false,z,z);
  New(Parents);
  end;

destructor tRtcList_WStrObj.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_WStrObj;
    head^.key:=vrtcItemMin_WStrObj;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_WStrObj;
    z^.key:=vrtcItemMin_WStrObj;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_WStrObj.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_WStrObj.New_Node(const k:trtcItemType_WStrObj; const i:trtcInfoType_WStrObj; const bi:boolean; const ll,rr:prtcNode_WStrObj):prtcNode_WStrObj;
  var
    p:prtcNodeArr_WStrObj;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_WStrObj);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_WStrObj));
  FillChar(Result^,SizeOf(trtcNode_WStrObj),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_WStrObj.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_WStrObj.Del_Node(node:prtcNode_WStrObj);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_WStrObj.Change(const v:trtcItemType_WStrObj;const info:trtcInfoType_WStrObj);
  var
    x:prtcNode_WStrObj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_WStrObj.RemoveThis(var t:prtcNode_WStrObj);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_WStrObj;
  t^.key:=vrtcItemMin_WStrObj;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_WStrObj.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_WStrObj;
  head^.key:=vrtcItemMin_WStrObj;
  cnt:=0;
  end;

function tRtcList_WStrObj.Search(const v:trtcItemType_WStrObj):trtcInfoType_WStrObj;
  var
    x:prtcNode_WStrObj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_WStrObj.Search_Min(var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x:prtcNode_WStrObj;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_WStrObj;
    Result:=vrtcItemMin_WStrObj;
    end;
  end;

function tRtcList_WStrObj.Search_Max(var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x:prtcNode_WStrObj;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_WStrObj;
    Result:=vrtcItemMin_WStrObj;
    end;
  end;

function tRtcList_WStrObj.Search_L(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x,y:prtcNode_WStrObj;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_WStrObj.Search_G(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x,y:prtcNode_WStrObj;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_WStrObj.Search_LE(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x,y:prtcNode_WStrObj;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_WStrObj.Search_GE(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x,y:prtcNode_WStrObj;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_WStrObj.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_WStrObj.Insert(const v:trtcItemType_WStrObj;const info:trtcInfoType_WStrObj);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_WStrObj.Remove_AddParentNode(node:prtcNode_WStrObj);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_WStrObj.Remove_GetParentNode:prtcNode_WStrObj;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_WStrObj.Remove(const v:trtcItemType_WStrObj);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_WStrObj;
  t^.key:=vrtcItemMin_WStrObj;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_WStrObj.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_WStrPtr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_WStrPtr,vrtcInfoNIL_WStrPtr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_WStrPtr,vrtcInfoNIL_WStrPtr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_WStrPtr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_WStrPtr;
    head^.key:=vrtcItemMin_WStrPtr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_WStrPtr;
    z^.key:=vrtcItemMin_WStrPtr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_WStrPtr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_WStrPtr.New_Node(const k:trtcItemType_WStrPtr; const i:trtcInfoType_WStrPtr; const bi:boolean; const ll,rr:prtcNode_WStrPtr):prtcNode_WStrPtr;
  var
    p:prtcNodeArr_WStrPtr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_WStrPtr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_WStrPtr));
  FillChar(Result^,SizeOf(trtcNode_WStrPtr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_WStrPtr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_WStrPtr.Del_Node(node:prtcNode_WStrPtr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_WStrPtr.Change(const v:trtcItemType_WStrPtr;const info:trtcInfoType_WStrPtr);
  var
    x:prtcNode_WStrPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_WStrPtr.RemoveThis(var t:prtcNode_WStrPtr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_WStrPtr;
  t^.key:=vrtcItemMin_WStrPtr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_WStrPtr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_WStrPtr;
  head^.key:=vrtcItemMin_WStrPtr;
  cnt:=0;
  end;

function tRtcList_WStrPtr.Search(const v:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;
  var
    x:prtcNode_WStrPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_WStrPtr.Search_Min(var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x:prtcNode_WStrPtr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_WStrPtr;
    Result:=vrtcItemMin_WStrPtr;
    end;
  end;

function tRtcList_WStrPtr.Search_Max(var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x:prtcNode_WStrPtr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_WStrPtr;
    Result:=vrtcItemMin_WStrPtr;
    end;
  end;

function tRtcList_WStrPtr.Search_L(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x,y:prtcNode_WStrPtr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_WStrPtr.Search_G(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x,y:prtcNode_WStrPtr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_WStrPtr.Search_LE(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x,y:prtcNode_WStrPtr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_WStrPtr.Search_GE(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x,y:prtcNode_WStrPtr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_WStrPtr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_WStrPtr.Insert(const v:trtcItemType_WStrPtr;const info:trtcInfoType_WStrPtr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_WStrPtr.Remove_AddParentNode(node:prtcNode_WStrPtr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_WStrPtr.Remove_GetParentNode:prtcNode_WStrPtr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_WStrPtr.Remove(const v:trtcItemType_WStrPtr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_WStrPtr;
  t^.key:=vrtcItemMin_WStrPtr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_WStrPtr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcList_WStrWStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_WStrWStr,vrtcInfoNIL_WStrWStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(vrtcItemMin_WStrWStr,vrtcInfoNIL_WStrWStr,false,z,z);
  New(Parents);
  end;

destructor tRtcList_WStrWStr.Destroy;
  var
    a,i:longint;
  begin
  RemoveAll;
  if assigned(head) then
    begin
    head^.info:=vrtcInfoNIL_WStrWStr;
    head^.key:=vrtcItemMin_WStrWStr;
    del_node(head);
    end;
  if assigned(z) then
    begin
    z^.info:=vrtcInfoNIL_WStrWStr;
    z^.key:=vrtcItemMin_WStrWStr;
    del_node(z);
    end;
  if Parents<>nil then Dispose(Parents);
  for a:=0 to Length(myPools)-1 do
    begin
    for i:=0 to length(myPools[a]^)-1 do
      myPools[a]^:=nil;
    SetLength(myPools[a]^,0);
    FreeMem(myPools[a]);
    myPools[a]:=nil;
    end;
  SetLength(myPools,0);
  if assigned(pool) then pool.Free;
  inherited;
  end;

function tRtcList_WStrWStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcList_WStrWStr.New_Node(const k:trtcItemType_WStrWStr; const i:trtcInfoType_WStrWStr; const bi:boolean; const ll,rr:prtcNode_WStrWStr):prtcNode_WStrWStr;
  var
    p:prtcNodeArr_WStrWStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr_WStrWStr);
      GetMem(p,a);
      FillChar(p^,a,0);
      SetLength(p^,myPoolSize);
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@(p^[a]));
      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(trtcNode_WStrWStr));
  FillChar(Result^,SizeOf(trtcNode_WStrWStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tRtcList_WStrWStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcList_WStrWStr.Del_Node(node:prtcNode_WStrWStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcList_WStrWStr.Change(const v:trtcItemType_WStrWStr;const info:trtcInfoType_WStrWStr);
  var
    x:prtcNode_WStrWStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tRtcList_WStrWStr.RemoveThis(var t:prtcNode_WStrWStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_WStrWStr;
  t^.key:=vrtcItemMin_WStrWStr;
  del_node(t);
  t:=z;
  end;

procedure tRtcList_WStrWStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_WStrWStr;
  head^.key:=vrtcItemMin_WStrWStr;
  cnt:=0;
  end;

function tRtcList_WStrWStr.Search(const v:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;
  var
    x:prtcNode_WStrWStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tRtcList_WStrWStr.Search_Min(var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x:prtcNode_WStrWStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_WStrWStr;
    Result:=vrtcItemMin_WStrWStr;
    end;
  end;

function tRtcList_WStrWStr.Search_Max(var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x:prtcNode_WStrWStr;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=vrtcInfoNIL_WStrWStr;
    Result:=vrtcItemMin_WStrWStr;
    end;
  end;

function tRtcList_WStrWStr.Search_L(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x,y:prtcNode_WStrWStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_WStrWStr.Search_G(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x,y:prtcNode_WStrWStr;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tRtcList_WStrWStr.Search_LE(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x,y:prtcNode_WStrWStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tRtcList_WStrWStr.Search_GE(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x,y:prtcNode_WStrWStr;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tRtcList_WStrWStr.Insert_split;
  begin
  x^.b:=true;
  x^.l^.b:=false;
  x^.r^.b:=false;
  if (p^.b) then
    begin
    g^.b:=true;
    if (xv<g^.key)<>(xv<p^.key) then
      begin
      // procedure Insert_p_rotate_g; ->
      c:=p;
      if (xv<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (xv<g^.key) then
        g^.l:=p
      else
        g^.r:=p;
      // <-
      end;
    // Insert_x_rotate_gg; ->
    c:=g;
    if (xv<c^.key) then
      begin
      x:=c^.l;
      c^.l:=x^.r;
      x^.r:=c;
      end
    else
      begin
      x:=c^.r;
      c^.r:=x^.l;
      x^.l:=c;
      end;
    if (xv<gg^.key) then
      gg^.l:=x
    else
      gg^.r:=x;
    // <-
    x^.b:=false;
    end;
  head^.r^.b:=false;
  end;

procedure tRtcList_WStrWStr.Insert(const v:trtcItemType_WStrWStr;const info:trtcInfoType_WStrWStr);
  begin
  xv:=v;
  // xinfo:=info;
  nx:=new_node(v,info,True,z,z);
  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then Insert_split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  Insert_Split;
  Inc(cnt);
  end;

procedure tRtcList_WStrWStr.Remove_AddParentNode(node:prtcNode_WStrWStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcList_WStrWStr.Remove_GetParentNode:prtcNode_WStrWStr;
  begin
  with Parents^ do
    if NodeCount=0 then
      Result:=z
    else
      begin
      Dec(NodeCount);
      Result:=Nodes[NodeCount];
      end;
  end;

procedure tRtcList_WStrWStr.Remove(const v:trtcItemType_WStrWStr);
  var
    a:byte;
  begin
  Parents^.NodeCount:=0;
  p:=head; t:=head^.r;
  Remove_AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    Remove_AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;
  if t=z then
    raise ERtcSearch.Create('Key not found !');
  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            // y_rotateRight_p; ->
            c := y^.l;
            y^.l := c^.r;
            if (p^.r = y) then p^.r := c else p^.l := c;
            c^.r := y;
            y := p^.r;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r := y^.l;
          if (p = g^.r) then g^.r := y else g^.l := y;
          y^.l := p;
          g:=y; y:=p^.r;
          // <-
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then
            begin
            g^.l := y
            end
          else
            begin
            g^.r := y;
            end;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            // y_rotateLeft_p; ->
            c := y^.r;
            y^.r := c^.l;
            if (p^.l = y) then p^.l := c else p^.r := c;
            c^.l := y;
            y := p^.l;
            // <-
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l := y^.r;
          if (p = g^.l) then g^.l := y else g^.r := y;
          y^.r := p;
          g:=y; y:=p^.l;
          // <-
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_WStrWStr;
  t^.key:=vrtcItemMin_WStrWStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcList_WStrWStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

end.
