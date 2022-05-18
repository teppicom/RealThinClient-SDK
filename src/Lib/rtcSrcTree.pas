{
  @html(<b>)
  Fast Search Trees
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
}
unit rtcSrcTree;

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
  tRtcSearchTree<itemType,infoType>=class(TRtcFastObject)
    type
      pnode=^tnode;
      tnode=record
        key:itemType;
        info:infoType;
        l,r:pnode;
        b:boolean;
        l2,r2:pnode;
        b2:boolean;
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
    xv2:infoType;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:pnode;
    // Comparers
    itemC:IComparer<itemType>;
    infoC:IComparer<infoType>;
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
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:pnode);
    function Remove_GetParentNode:pnode;
  public
    constructor Create(size:integer; const _min:itemType; const _nil:infoType); overload;
    constructor Create(size:integer; const _min:itemType; const _nil:infoType; const _itemComparer:IComparer<itemType>); overload;
    constructor Create(size:integer; const _min:itemType; const _nil:infoType; const _itemComparer:IComparer<itemType>; const _infoComparer:IComparer<infoType>); overload;
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
    function isearch(const v:infoType):itemType;      // Search for info, exact "v"
    function isearch_min(var i:itemType):infoType;
    function isearch_max(var i:itemType):infoType;
    function isearch_l(const v:infoType; var i:itemType):infoType;  // Search for info lower than "v"
    function isearch_g(const v:infoType; var i:itemType):infoType;  // Search for info higher than "v"
    function isearch_le(const v:infoType; var i:itemType):infoType;  // Search for info lower or equel to "v"
    function isearch_ge(const v:infoType; var i:itemType):infoType;  // Search for info higher or equal to "v"
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
  tRtcTree_I32I32=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_I32I32;
    xv:trtcItemType_I32I32;
    xv2:trtcInfoType_I32I32;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_I32I32;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_I32I32;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_I32I32;
    Parents:prtcParentList3_I32I32;
    procedure del_node(node:prtcNode3_I32I32);
    function new_node(const k:trtcItemType_I32I32; const i:trtcInfoType_I32I32; const bi:boolean; const ll,rr:prtcNode3_I32I32):prtcNode3_I32I32;
    procedure RemoveThis(var t:prtcNode3_I32I32);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_I32I32);
    function Remove_GetParentNode:prtcNode3_I32I32;
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
    function isearch(const v:trtcInfoType_I32I32):trtcItemType_I32I32;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_I32I32):trtcInfoType_I32I32;
    function isearch_max(var i:trtcItemType_I32I32):trtcInfoType_I32I32;
    function isearch_l(const v:trtcInfoType_I32I32; var i:trtcItemType_I32I32):trtcInfoType_I32I32;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_I32I32; var i:trtcItemType_I32I32):trtcInfoType_I32I32;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_I32I32; var i:trtcItemType_I32I32):trtcInfoType_I32I32;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_I32I32; var i:trtcItemType_I32I32):trtcInfoType_I32I32;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_I32I32;const info:trtcInfoType_I32I32);
    procedure insert(const v:trtcItemType_I32I32;const info:trtcInfoType_I32I32);
    procedure remove(const v:trtcItemType_I32I32);
    procedure removeall;
  public
    property RootNode:prtcNode3_I32I32 read head;
    property NilNode:prtcNode3_I32I32 read z;
    end;
type
  tRtcTree_I32Obj=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_I32Obj;
    xv:trtcItemType_I32Obj;
    xv2:trtcInfoType_I32Obj;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_I32Obj;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_I32Obj;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_I32Obj;
    Parents:prtcParentList3_I32Obj;
    procedure del_node(node:prtcNode3_I32Obj);
    function new_node(const k:trtcItemType_I32Obj; const i:trtcInfoType_I32Obj; const bi:boolean; const ll,rr:prtcNode3_I32Obj):prtcNode3_I32Obj;
    procedure RemoveThis(var t:prtcNode3_I32Obj);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_I32Obj);
    function Remove_GetParentNode:prtcNode3_I32Obj;
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
    function isearch(const v:trtcInfoType_I32Obj):trtcItemType_I32Obj;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;
    function isearch_max(var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;
    function isearch_l(const v:trtcInfoType_I32Obj; var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_I32Obj; var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_I32Obj; var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_I32Obj; var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_I32Obj;const info:trtcInfoType_I32Obj);
    procedure insert(const v:trtcItemType_I32Obj;const info:trtcInfoType_I32Obj);
    procedure remove(const v:trtcItemType_I32Obj);
    procedure removeall;
  public
    property RootNode:prtcNode3_I32Obj read head;
    property NilNode:prtcNode3_I32Obj read z;
    end;
type
  tRtcTree_I32Ptr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_I32Ptr;
    xv:trtcItemType_I32Ptr;
    xv2:trtcInfoType_I32Ptr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_I32Ptr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_I32Ptr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_I32Ptr;
    Parents:prtcParentList3_I32Ptr;
    procedure del_node(node:prtcNode3_I32Ptr);
    function new_node(const k:trtcItemType_I32Ptr; const i:trtcInfoType_I32Ptr; const bi:boolean; const ll,rr:prtcNode3_I32Ptr):prtcNode3_I32Ptr;
    procedure RemoveThis(var t:prtcNode3_I32Ptr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_I32Ptr);
    function Remove_GetParentNode:prtcNode3_I32Ptr;
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
    function isearch(const v:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;
    function isearch_max(var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;
    function isearch_l(const v:trtcInfoType_I32Ptr; var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_I32Ptr; var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_I32Ptr; var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_I32Ptr; var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_I32Ptr;const info:trtcInfoType_I32Ptr);
    procedure insert(const v:trtcItemType_I32Ptr;const info:trtcInfoType_I32Ptr);
    procedure remove(const v:trtcItemType_I32Ptr);
    procedure removeall;
  public
    property RootNode:prtcNode3_I32Ptr read head;
    property NilNode:prtcNode3_I32Ptr read z;
    end;
type
  tRtcTree_I32Str=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_I32Str;
    xv:trtcItemType_I32Str;
    xv2:trtcInfoType_I32Str;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_I32Str;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_I32Str;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_I32Str;
    Parents:prtcParentList3_I32Str;
    procedure del_node(node:prtcNode3_I32Str);
    function new_node(const k:trtcItemType_I32Str; const i:trtcInfoType_I32Str; const bi:boolean; const ll,rr:prtcNode3_I32Str):prtcNode3_I32Str;
    procedure RemoveThis(var t:prtcNode3_I32Str);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_I32Str);
    function Remove_GetParentNode:prtcNode3_I32Str;
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
    function isearch(const v:trtcInfoType_I32Str):trtcItemType_I32Str;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_I32Str):trtcInfoType_I32Str;
    function isearch_max(var i:trtcItemType_I32Str):trtcInfoType_I32Str;
    function isearch_l(const v:trtcInfoType_I32Str; var i:trtcItemType_I32Str):trtcInfoType_I32Str;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_I32Str; var i:trtcItemType_I32Str):trtcInfoType_I32Str;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_I32Str; var i:trtcItemType_I32Str):trtcInfoType_I32Str;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_I32Str; var i:trtcItemType_I32Str):trtcInfoType_I32Str;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_I32Str;const info:trtcInfoType_I32Str);
    procedure insert(const v:trtcItemType_I32Str;const info:trtcInfoType_I32Str);
    procedure remove(const v:trtcItemType_I32Str);
    procedure removeall;
  public
    property RootNode:prtcNode3_I32Str read head;
    property NilNode:prtcNode3_I32Str read z;
    end;
type
  tRtcTree_I32WStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_I32WStr;
    xv:trtcItemType_I32WStr;
    xv2:trtcInfoType_I32WStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_I32WStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_I32WStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_I32WStr;
    Parents:prtcParentList3_I32WStr;
    procedure del_node(node:prtcNode3_I32WStr);
    function new_node(const k:trtcItemType_I32WStr; const i:trtcInfoType_I32WStr; const bi:boolean; const ll,rr:prtcNode3_I32WStr):prtcNode3_I32WStr;
    procedure RemoveThis(var t:prtcNode3_I32WStr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_I32WStr);
    function Remove_GetParentNode:prtcNode3_I32WStr;
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
    function isearch(const v:trtcInfoType_I32WStr):trtcItemType_I32WStr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;
    function isearch_max(var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;
    function isearch_l(const v:trtcInfoType_I32WStr; var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_I32WStr; var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_I32WStr; var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_I32WStr; var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_I32WStr;const info:trtcInfoType_I32WStr);
    procedure insert(const v:trtcItemType_I32WStr;const info:trtcInfoType_I32WStr);
    procedure remove(const v:trtcItemType_I32WStr);
    procedure removeall;
  public
    property RootNode:prtcNode3_I32WStr read head;
    property NilNode:prtcNode3_I32WStr read z;
    end;
type
  tRtcTree_I64I64=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_I64I64;
    xv:trtcItemType_I64I64;
    xv2:trtcInfoType_I64I64;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_I64I64;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_I64I64;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_I64I64;
    Parents:prtcParentList3_I64I64;
    procedure del_node(node:prtcNode3_I64I64);
    function new_node(const k:trtcItemType_I64I64; const i:trtcInfoType_I64I64; const bi:boolean; const ll,rr:prtcNode3_I64I64):prtcNode3_I64I64;
    procedure RemoveThis(var t:prtcNode3_I64I64);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_I64I64);
    function Remove_GetParentNode:prtcNode3_I64I64;
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
    function isearch(const v:trtcInfoType_I64I64):trtcItemType_I64I64;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_I64I64):trtcInfoType_I64I64;
    function isearch_max(var i:trtcItemType_I64I64):trtcInfoType_I64I64;
    function isearch_l(const v:trtcInfoType_I64I64; var i:trtcItemType_I64I64):trtcInfoType_I64I64;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_I64I64; var i:trtcItemType_I64I64):trtcInfoType_I64I64;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_I64I64; var i:trtcItemType_I64I64):trtcInfoType_I64I64;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_I64I64; var i:trtcItemType_I64I64):trtcInfoType_I64I64;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_I64I64;const info:trtcInfoType_I64I64);
    procedure insert(const v:trtcItemType_I64I64;const info:trtcInfoType_I64I64);
    procedure remove(const v:trtcItemType_I64I64);
    procedure removeall;
  public
    property RootNode:prtcNode3_I64I64 read head;
    property NilNode:prtcNode3_I64I64 read z;
    end;
type
  tRtcTree_I64Obj=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_I64Obj;
    xv:trtcItemType_I64Obj;
    xv2:trtcInfoType_I64Obj;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_I64Obj;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_I64Obj;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_I64Obj;
    Parents:prtcParentList3_I64Obj;
    procedure del_node(node:prtcNode3_I64Obj);
    function new_node(const k:trtcItemType_I64Obj; const i:trtcInfoType_I64Obj; const bi:boolean; const ll,rr:prtcNode3_I64Obj):prtcNode3_I64Obj;
    procedure RemoveThis(var t:prtcNode3_I64Obj);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_I64Obj);
    function Remove_GetParentNode:prtcNode3_I64Obj;
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
    function isearch(const v:trtcInfoType_I64Obj):trtcItemType_I64Obj;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;
    function isearch_max(var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;
    function isearch_l(const v:trtcInfoType_I64Obj; var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_I64Obj; var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_I64Obj; var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_I64Obj; var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_I64Obj;const info:trtcInfoType_I64Obj);
    procedure insert(const v:trtcItemType_I64Obj;const info:trtcInfoType_I64Obj);
    procedure remove(const v:trtcItemType_I64Obj);
    procedure removeall;
  public
    property RootNode:prtcNode3_I64Obj read head;
    property NilNode:prtcNode3_I64Obj read z;
    end;
type
  tRtcTree_I64Ptr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_I64Ptr;
    xv:trtcItemType_I64Ptr;
    xv2:trtcInfoType_I64Ptr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_I64Ptr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_I64Ptr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_I64Ptr;
    Parents:prtcParentList3_I64Ptr;
    procedure del_node(node:prtcNode3_I64Ptr);
    function new_node(const k:trtcItemType_I64Ptr; const i:trtcInfoType_I64Ptr; const bi:boolean; const ll,rr:prtcNode3_I64Ptr):prtcNode3_I64Ptr;
    procedure RemoveThis(var t:prtcNode3_I64Ptr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_I64Ptr);
    function Remove_GetParentNode:prtcNode3_I64Ptr;
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
    function isearch(const v:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;
    function isearch_max(var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;
    function isearch_l(const v:trtcInfoType_I64Ptr; var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_I64Ptr; var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_I64Ptr; var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_I64Ptr; var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_I64Ptr;const info:trtcInfoType_I64Ptr);
    procedure insert(const v:trtcItemType_I64Ptr;const info:trtcInfoType_I64Ptr);
    procedure remove(const v:trtcItemType_I64Ptr);
    procedure removeall;
  public
    property RootNode:prtcNode3_I64Ptr read head;
    property NilNode:prtcNode3_I64Ptr read z;
    end;
type
  tRtcTree_I64Str=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_I64Str;
    xv:trtcItemType_I64Str;
    xv2:trtcInfoType_I64Str;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_I64Str;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_I64Str;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_I64Str;
    Parents:prtcParentList3_I64Str;
    procedure del_node(node:prtcNode3_I64Str);
    function new_node(const k:trtcItemType_I64Str; const i:trtcInfoType_I64Str; const bi:boolean; const ll,rr:prtcNode3_I64Str):prtcNode3_I64Str;
    procedure RemoveThis(var t:prtcNode3_I64Str);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_I64Str);
    function Remove_GetParentNode:prtcNode3_I64Str;
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
    function isearch(const v:trtcInfoType_I64Str):trtcItemType_I64Str;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_I64Str):trtcInfoType_I64Str;
    function isearch_max(var i:trtcItemType_I64Str):trtcInfoType_I64Str;
    function isearch_l(const v:trtcInfoType_I64Str; var i:trtcItemType_I64Str):trtcInfoType_I64Str;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_I64Str; var i:trtcItemType_I64Str):trtcInfoType_I64Str;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_I64Str; var i:trtcItemType_I64Str):trtcInfoType_I64Str;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_I64Str; var i:trtcItemType_I64Str):trtcInfoType_I64Str;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_I64Str;const info:trtcInfoType_I64Str);
    procedure insert(const v:trtcItemType_I64Str;const info:trtcInfoType_I64Str);
    procedure remove(const v:trtcItemType_I64Str);
    procedure removeall;
  public
    property RootNode:prtcNode3_I64Str read head;
    property NilNode:prtcNode3_I64Str read z;
    end;
type
  tRtcTree_I64WStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_I64WStr;
    xv:trtcItemType_I64WStr;
    xv2:trtcInfoType_I64WStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_I64WStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_I64WStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_I64WStr;
    Parents:prtcParentList3_I64WStr;
    procedure del_node(node:prtcNode3_I64WStr);
    function new_node(const k:trtcItemType_I64WStr; const i:trtcInfoType_I64WStr; const bi:boolean; const ll,rr:prtcNode3_I64WStr):prtcNode3_I64WStr;
    procedure RemoveThis(var t:prtcNode3_I64WStr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_I64WStr);
    function Remove_GetParentNode:prtcNode3_I64WStr;
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
    function isearch(const v:trtcInfoType_I64WStr):trtcItemType_I64WStr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;
    function isearch_max(var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;
    function isearch_l(const v:trtcInfoType_I64WStr; var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_I64WStr; var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_I64WStr; var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_I64WStr; var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_I64WStr;const info:trtcInfoType_I64WStr);
    procedure insert(const v:trtcItemType_I64WStr;const info:trtcInfoType_I64WStr);
    procedure remove(const v:trtcItemType_I64WStr);
    procedure removeall;
  public
    property RootNode:prtcNode3_I64WStr read head;
    property NilNode:prtcNode3_I64WStr read z;
    end;
type
  tRtcTree_IPtrIPtr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_IPtrIPtr;
    xv:trtcItemType_IPtrIPtr;
    xv2:trtcInfoType_IPtrIPtr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_IPtrIPtr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_IPtrIPtr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_IPtrIPtr;
    Parents:prtcParentList3_IPtrIPtr;
    procedure del_node(node:prtcNode3_IPtrIPtr);
    function new_node(const k:trtcItemType_IPtrIPtr; const i:trtcInfoType_IPtrIPtr; const bi:boolean; const ll,rr:prtcNode3_IPtrIPtr):prtcNode3_IPtrIPtr;
    procedure RemoveThis(var t:prtcNode3_IPtrIPtr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_IPtrIPtr);
    function Remove_GetParentNode:prtcNode3_IPtrIPtr;
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
    function isearch(const v:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;
    function isearch_max(var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;
    function isearch_l(const v:trtcInfoType_IPtrIPtr; var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_IPtrIPtr; var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_IPtrIPtr; var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_IPtrIPtr; var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_IPtrIPtr;const info:trtcInfoType_IPtrIPtr);
    procedure insert(const v:trtcItemType_IPtrIPtr;const info:trtcInfoType_IPtrIPtr);
    procedure remove(const v:trtcItemType_IPtrIPtr);
    procedure removeall;
  public
    property RootNode:prtcNode3_IPtrIPtr read head;
    property NilNode:prtcNode3_IPtrIPtr read z;
    end;
type
  tRtcTree_IPtrObj=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_IPtrObj;
    xv:trtcItemType_IPtrObj;
    xv2:trtcInfoType_IPtrObj;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_IPtrObj;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_IPtrObj;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_IPtrObj;
    Parents:prtcParentList3_IPtrObj;
    procedure del_node(node:prtcNode3_IPtrObj);
    function new_node(const k:trtcItemType_IPtrObj; const i:trtcInfoType_IPtrObj; const bi:boolean; const ll,rr:prtcNode3_IPtrObj):prtcNode3_IPtrObj;
    procedure RemoveThis(var t:prtcNode3_IPtrObj);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_IPtrObj);
    function Remove_GetParentNode:prtcNode3_IPtrObj;
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
    function isearch(const v:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;
    function isearch_max(var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;
    function isearch_l(const v:trtcInfoType_IPtrObj; var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_IPtrObj; var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_IPtrObj; var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_IPtrObj; var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_IPtrObj;const info:trtcInfoType_IPtrObj);
    procedure insert(const v:trtcItemType_IPtrObj;const info:trtcInfoType_IPtrObj);
    procedure remove(const v:trtcItemType_IPtrObj);
    procedure removeall;
  public
    property RootNode:prtcNode3_IPtrObj read head;
    property NilNode:prtcNode3_IPtrObj read z;
    end;
type
  tRtcTree_IPtrPtr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_IPtrPtr;
    xv:trtcItemType_IPtrPtr;
    xv2:trtcInfoType_IPtrPtr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_IPtrPtr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_IPtrPtr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_IPtrPtr;
    Parents:prtcParentList3_IPtrPtr;
    procedure del_node(node:prtcNode3_IPtrPtr);
    function new_node(const k:trtcItemType_IPtrPtr; const i:trtcInfoType_IPtrPtr; const bi:boolean; const ll,rr:prtcNode3_IPtrPtr):prtcNode3_IPtrPtr;
    procedure RemoveThis(var t:prtcNode3_IPtrPtr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_IPtrPtr);
    function Remove_GetParentNode:prtcNode3_IPtrPtr;
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
    function isearch(const v:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;
    function isearch_max(var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;
    function isearch_l(const v:trtcInfoType_IPtrPtr; var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_IPtrPtr; var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_IPtrPtr; var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_IPtrPtr; var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_IPtrPtr;const info:trtcInfoType_IPtrPtr);
    procedure insert(const v:trtcItemType_IPtrPtr;const info:trtcInfoType_IPtrPtr);
    procedure remove(const v:trtcItemType_IPtrPtr);
    procedure removeall;
  public
    property RootNode:prtcNode3_IPtrPtr read head;
    property NilNode:prtcNode3_IPtrPtr read z;
    end;
type
  tRtcTree_IPtrStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_IPtrStr;
    xv:trtcItemType_IPtrStr;
    xv2:trtcInfoType_IPtrStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_IPtrStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_IPtrStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_IPtrStr;
    Parents:prtcParentList3_IPtrStr;
    procedure del_node(node:prtcNode3_IPtrStr);
    function new_node(const k:trtcItemType_IPtrStr; const i:trtcInfoType_IPtrStr; const bi:boolean; const ll,rr:prtcNode3_IPtrStr):prtcNode3_IPtrStr;
    procedure RemoveThis(var t:prtcNode3_IPtrStr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_IPtrStr);
    function Remove_GetParentNode:prtcNode3_IPtrStr;
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
    function isearch(const v:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;
    function isearch_max(var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;
    function isearch_l(const v:trtcInfoType_IPtrStr; var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_IPtrStr; var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_IPtrStr; var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_IPtrStr; var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_IPtrStr;const info:trtcInfoType_IPtrStr);
    procedure insert(const v:trtcItemType_IPtrStr;const info:trtcInfoType_IPtrStr);
    procedure remove(const v:trtcItemType_IPtrStr);
    procedure removeall;
  public
    property RootNode:prtcNode3_IPtrStr read head;
    property NilNode:prtcNode3_IPtrStr read z;
    end;
type
  tRtcTree_IPtrWStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_IPtrWStr;
    xv:trtcItemType_IPtrWStr;
    xv2:trtcInfoType_IPtrWStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_IPtrWStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_IPtrWStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_IPtrWStr;
    Parents:prtcParentList3_IPtrWStr;
    procedure del_node(node:prtcNode3_IPtrWStr);
    function new_node(const k:trtcItemType_IPtrWStr; const i:trtcInfoType_IPtrWStr; const bi:boolean; const ll,rr:prtcNode3_IPtrWStr):prtcNode3_IPtrWStr;
    procedure RemoveThis(var t:prtcNode3_IPtrWStr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_IPtrWStr);
    function Remove_GetParentNode:prtcNode3_IPtrWStr;
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
    function isearch(const v:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;
    function isearch_max(var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;
    function isearch_l(const v:trtcInfoType_IPtrWStr; var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_IPtrWStr; var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_IPtrWStr; var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_IPtrWStr; var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_IPtrWStr;const info:trtcInfoType_IPtrWStr);
    procedure insert(const v:trtcItemType_IPtrWStr;const info:trtcInfoType_IPtrWStr);
    procedure remove(const v:trtcItemType_IPtrWStr);
    procedure removeall;
  public
    property RootNode:prtcNode3_IPtrWStr read head;
    property NilNode:prtcNode3_IPtrWStr read z;
    end;
type
  tRtcTree_StrI32=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_StrI32;
    xv:trtcItemType_StrI32;
    xv2:trtcInfoType_StrI32;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_StrI32;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_StrI32;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_StrI32;
    Parents:prtcParentList3_StrI32;
    procedure del_node(node:prtcNode3_StrI32);
    function new_node(const k:trtcItemType_StrI32; const i:trtcInfoType_StrI32; const bi:boolean; const ll,rr:prtcNode3_StrI32):prtcNode3_StrI32;
    procedure RemoveThis(var t:prtcNode3_StrI32);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_StrI32);
    function Remove_GetParentNode:prtcNode3_StrI32;
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
    function isearch(const v:trtcInfoType_StrI32):trtcItemType_StrI32;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_StrI32):trtcInfoType_StrI32;
    function isearch_max(var i:trtcItemType_StrI32):trtcInfoType_StrI32;
    function isearch_l(const v:trtcInfoType_StrI32; var i:trtcItemType_StrI32):trtcInfoType_StrI32;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_StrI32; var i:trtcItemType_StrI32):trtcInfoType_StrI32;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_StrI32; var i:trtcItemType_StrI32):trtcInfoType_StrI32;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_StrI32; var i:trtcItemType_StrI32):trtcInfoType_StrI32;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_StrI32;const info:trtcInfoType_StrI32);
    procedure insert(const v:trtcItemType_StrI32;const info:trtcInfoType_StrI32);
    procedure remove(const v:trtcItemType_StrI32);
    procedure removeall;
  public
    property RootNode:prtcNode3_StrI32 read head;
    property NilNode:prtcNode3_StrI32 read z;
    end;
type
  tRtcTree_StrI64=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_StrI64;
    xv:trtcItemType_StrI64;
    xv2:trtcInfoType_StrI64;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_StrI64;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_StrI64;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_StrI64;
    Parents:prtcParentList3_StrI64;
    procedure del_node(node:prtcNode3_StrI64);
    function new_node(const k:trtcItemType_StrI64; const i:trtcInfoType_StrI64; const bi:boolean; const ll,rr:prtcNode3_StrI64):prtcNode3_StrI64;
    procedure RemoveThis(var t:prtcNode3_StrI64);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_StrI64);
    function Remove_GetParentNode:prtcNode3_StrI64;
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
    function isearch(const v:trtcInfoType_StrI64):trtcItemType_StrI64;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_StrI64):trtcInfoType_StrI64;
    function isearch_max(var i:trtcItemType_StrI64):trtcInfoType_StrI64;
    function isearch_l(const v:trtcInfoType_StrI64; var i:trtcItemType_StrI64):trtcInfoType_StrI64;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_StrI64; var i:trtcItemType_StrI64):trtcInfoType_StrI64;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_StrI64; var i:trtcItemType_StrI64):trtcInfoType_StrI64;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_StrI64; var i:trtcItemType_StrI64):trtcInfoType_StrI64;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_StrI64;const info:trtcInfoType_StrI64);
    procedure insert(const v:trtcItemType_StrI64;const info:trtcInfoType_StrI64);
    procedure remove(const v:trtcItemType_StrI64);
    procedure removeall;
  public
    property RootNode:prtcNode3_StrI64 read head;
    property NilNode:prtcNode3_StrI64 read z;
    end;
type
  tRtcTree_StrObj=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_StrObj;
    xv:trtcItemType_StrObj;
    xv2:trtcInfoType_StrObj;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_StrObj;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_StrObj;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_StrObj;
    Parents:prtcParentList3_StrObj;
    procedure del_node(node:prtcNode3_StrObj);
    function new_node(const k:trtcItemType_StrObj; const i:trtcInfoType_StrObj; const bi:boolean; const ll,rr:prtcNode3_StrObj):prtcNode3_StrObj;
    procedure RemoveThis(var t:prtcNode3_StrObj);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_StrObj);
    function Remove_GetParentNode:prtcNode3_StrObj;
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
    function isearch(const v:trtcInfoType_StrObj):trtcItemType_StrObj;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_StrObj):trtcInfoType_StrObj;
    function isearch_max(var i:trtcItemType_StrObj):trtcInfoType_StrObj;
    function isearch_l(const v:trtcInfoType_StrObj; var i:trtcItemType_StrObj):trtcInfoType_StrObj;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_StrObj; var i:trtcItemType_StrObj):trtcInfoType_StrObj;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_StrObj; var i:trtcItemType_StrObj):trtcInfoType_StrObj;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_StrObj; var i:trtcItemType_StrObj):trtcInfoType_StrObj;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_StrObj;const info:trtcInfoType_StrObj);
    procedure insert(const v:trtcItemType_StrObj;const info:trtcInfoType_StrObj);
    procedure remove(const v:trtcItemType_StrObj);
    procedure removeall;
  public
    property RootNode:prtcNode3_StrObj read head;
    property NilNode:prtcNode3_StrObj read z;
    end;
type
  tRtcTree_StrPtr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_StrPtr;
    xv:trtcItemType_StrPtr;
    xv2:trtcInfoType_StrPtr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_StrPtr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_StrPtr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_StrPtr;
    Parents:prtcParentList3_StrPtr;
    procedure del_node(node:prtcNode3_StrPtr);
    function new_node(const k:trtcItemType_StrPtr; const i:trtcInfoType_StrPtr; const bi:boolean; const ll,rr:prtcNode3_StrPtr):prtcNode3_StrPtr;
    procedure RemoveThis(var t:prtcNode3_StrPtr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_StrPtr);
    function Remove_GetParentNode:prtcNode3_StrPtr;
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
    function isearch(const v:trtcInfoType_StrPtr):trtcItemType_StrPtr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;
    function isearch_max(var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;
    function isearch_l(const v:trtcInfoType_StrPtr; var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_StrPtr; var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_StrPtr; var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_StrPtr; var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_StrPtr;const info:trtcInfoType_StrPtr);
    procedure insert(const v:trtcItemType_StrPtr;const info:trtcInfoType_StrPtr);
    procedure remove(const v:trtcItemType_StrPtr);
    procedure removeall;
  public
    property RootNode:prtcNode3_StrPtr read head;
    property NilNode:prtcNode3_StrPtr read z;
    end;
type
  tRtcTree_StrStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_StrStr;
    xv:trtcItemType_StrStr;
    xv2:trtcInfoType_StrStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_StrStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_StrStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_StrStr;
    Parents:prtcParentList3_StrStr;
    procedure del_node(node:prtcNode3_StrStr);
    function new_node(const k:trtcItemType_StrStr; const i:trtcInfoType_StrStr; const bi:boolean; const ll,rr:prtcNode3_StrStr):prtcNode3_StrStr;
    procedure RemoveThis(var t:prtcNode3_StrStr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_StrStr);
    function Remove_GetParentNode:prtcNode3_StrStr;
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
    function isearch(const v:trtcInfoType_StrStr):trtcItemType_StrStr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_StrStr):trtcInfoType_StrStr;
    function isearch_max(var i:trtcItemType_StrStr):trtcInfoType_StrStr;
    function isearch_l(const v:trtcInfoType_StrStr; var i:trtcItemType_StrStr):trtcInfoType_StrStr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_StrStr; var i:trtcItemType_StrStr):trtcInfoType_StrStr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_StrStr; var i:trtcItemType_StrStr):trtcInfoType_StrStr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_StrStr; var i:trtcItemType_StrStr):trtcInfoType_StrStr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_StrStr;const info:trtcInfoType_StrStr);
    procedure insert(const v:trtcItemType_StrStr;const info:trtcInfoType_StrStr);
    procedure remove(const v:trtcItemType_StrStr);
    procedure removeall;
  public
    property RootNode:prtcNode3_StrStr read head;
    property NilNode:prtcNode3_StrStr read z;
    end;
type
  tRtcTree_WStrI32=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_WStrI32;
    xv:trtcItemType_WStrI32;
    xv2:trtcInfoType_WStrI32;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_WStrI32;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_WStrI32;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_WStrI32;
    Parents:prtcParentList3_WStrI32;
    procedure del_node(node:prtcNode3_WStrI32);
    function new_node(const k:trtcItemType_WStrI32; const i:trtcInfoType_WStrI32; const bi:boolean; const ll,rr:prtcNode3_WStrI32):prtcNode3_WStrI32;
    procedure RemoveThis(var t:prtcNode3_WStrI32);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_WStrI32);
    function Remove_GetParentNode:prtcNode3_WStrI32;
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
    function isearch(const v:trtcInfoType_WStrI32):trtcItemType_WStrI32;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;
    function isearch_max(var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;
    function isearch_l(const v:trtcInfoType_WStrI32; var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_WStrI32; var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_WStrI32; var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_WStrI32; var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_WStrI32;const info:trtcInfoType_WStrI32);
    procedure insert(const v:trtcItemType_WStrI32;const info:trtcInfoType_WStrI32);
    procedure remove(const v:trtcItemType_WStrI32);
    procedure removeall;
  public
    property RootNode:prtcNode3_WStrI32 read head;
    property NilNode:prtcNode3_WStrI32 read z;
    end;
type
  tRtcTree_WStrI64=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_WStrI64;
    xv:trtcItemType_WStrI64;
    xv2:trtcInfoType_WStrI64;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_WStrI64;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_WStrI64;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_WStrI64;
    Parents:prtcParentList3_WStrI64;
    procedure del_node(node:prtcNode3_WStrI64);
    function new_node(const k:trtcItemType_WStrI64; const i:trtcInfoType_WStrI64; const bi:boolean; const ll,rr:prtcNode3_WStrI64):prtcNode3_WStrI64;
    procedure RemoveThis(var t:prtcNode3_WStrI64);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_WStrI64);
    function Remove_GetParentNode:prtcNode3_WStrI64;
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
    function isearch(const v:trtcInfoType_WStrI64):trtcItemType_WStrI64;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;
    function isearch_max(var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;
    function isearch_l(const v:trtcInfoType_WStrI64; var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_WStrI64; var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_WStrI64; var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_WStrI64; var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_WStrI64;const info:trtcInfoType_WStrI64);
    procedure insert(const v:trtcItemType_WStrI64;const info:trtcInfoType_WStrI64);
    procedure remove(const v:trtcItemType_WStrI64);
    procedure removeall;
  public
    property RootNode:prtcNode3_WStrI64 read head;
    property NilNode:prtcNode3_WStrI64 read z;
    end;
type
  tRtcTree_WStrObj=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_WStrObj;
    xv:trtcItemType_WStrObj;
    xv2:trtcInfoType_WStrObj;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_WStrObj;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_WStrObj;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_WStrObj;
    Parents:prtcParentList3_WStrObj;
    procedure del_node(node:prtcNode3_WStrObj);
    function new_node(const k:trtcItemType_WStrObj; const i:trtcInfoType_WStrObj; const bi:boolean; const ll,rr:prtcNode3_WStrObj):prtcNode3_WStrObj;
    procedure RemoveThis(var t:prtcNode3_WStrObj);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_WStrObj);
    function Remove_GetParentNode:prtcNode3_WStrObj;
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
    function isearch(const v:trtcInfoType_WStrObj):trtcItemType_WStrObj;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;
    function isearch_max(var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;
    function isearch_l(const v:trtcInfoType_WStrObj; var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_WStrObj; var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_WStrObj; var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_WStrObj; var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_WStrObj;const info:trtcInfoType_WStrObj);
    procedure insert(const v:trtcItemType_WStrObj;const info:trtcInfoType_WStrObj);
    procedure remove(const v:trtcItemType_WStrObj);
    procedure removeall;
  public
    property RootNode:prtcNode3_WStrObj read head;
    property NilNode:prtcNode3_WStrObj read z;
    end;
type
  tRtcTree_WStrPtr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_WStrPtr;
    xv:trtcItemType_WStrPtr;
    xv2:trtcInfoType_WStrPtr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_WStrPtr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_WStrPtr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_WStrPtr;
    Parents:prtcParentList3_WStrPtr;
    procedure del_node(node:prtcNode3_WStrPtr);
    function new_node(const k:trtcItemType_WStrPtr; const i:trtcInfoType_WStrPtr; const bi:boolean; const ll,rr:prtcNode3_WStrPtr):prtcNode3_WStrPtr;
    procedure RemoveThis(var t:prtcNode3_WStrPtr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_WStrPtr);
    function Remove_GetParentNode:prtcNode3_WStrPtr;
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
    function isearch(const v:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;
    function isearch_max(var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;
    function isearch_l(const v:trtcInfoType_WStrPtr; var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_WStrPtr; var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_WStrPtr; var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_WStrPtr; var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_WStrPtr;const info:trtcInfoType_WStrPtr);
    procedure insert(const v:trtcItemType_WStrPtr;const info:trtcInfoType_WStrPtr);
    procedure remove(const v:trtcItemType_WStrPtr);
    procedure removeall;
  public
    property RootNode:prtcNode3_WStrPtr read head;
    property NilNode:prtcNode3_WStrPtr read z;
    end;
type
  tRtcTree_WStrWStr=class(TRtcFastObject)
  private
    // Temp variables needed for "Insert"
    nx,x,p,g,gg,c:prtcNode3_WStrWStr;
    xv:trtcItemType_WStrWStr;
    xv2:trtcInfoType_WStrWStr;
    // Additional Temp variables needed for "remove"
    cb:boolean;
    y,p2,t:prtcNode3_WStrWStr;
    // data Pool
    myPoolSize:longint;
    myPools:array of prtcNodeArr3_WStrWStr;
    pool:tRtcPtrPool;
    cnt:cardinal;
    // search Tree
    head,z:prtcNode3_WStrWStr;
    Parents:prtcParentList3_WStrWStr;
    procedure del_node(node:prtcNode3_WStrWStr);
    function new_node(const k:trtcItemType_WStrWStr; const i:trtcInfoType_WStrWStr; const bi:boolean; const ll,rr:prtcNode3_WStrWStr):prtcNode3_WStrWStr;
    procedure RemoveThis(var t:prtcNode3_WStrWStr);
    procedure Insert_split;
    procedure Insert_split2;
    procedure Remove_AddParentNode(node:prtcNode3_WStrWStr);
    function Remove_GetParentNode:prtcNode3_WStrWStr;
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
    function isearch(const v:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;      // Search for info, exact "v"
    function isearch_min(var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;
    function isearch_max(var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;
    function isearch_l(const v:trtcInfoType_WStrWStr; var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;  // Search for info lower than "v"
    function isearch_g(const v:trtcInfoType_WStrWStr; var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;  // Search for info higher than "v"
    function isearch_le(const v:trtcInfoType_WStrWStr; var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;  // Search for info lower or equel to "v"
    function isearch_ge(const v:trtcInfoType_WStrWStr; var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;  // Search for info higher or equal to "v"
    procedure change(const v:trtcItemType_WStrWStr;const info:trtcInfoType_WStrWStr);
    procedure insert(const v:trtcItemType_WStrWStr;const info:trtcInfoType_WStrWStr);
    procedure remove(const v:trtcItemType_WStrWStr);
    procedure removeall;
  public
    property RootNode:prtcNode3_WStrWStr read head;
    property NilNode:prtcNode3_WStrWStr read z;
    end;

type
  tBinTree = tRtcTree_IPtrIPtr;
  tBinTree_ItemType = trtcItemType_IPtrIPtr;
  tBinTree_InfoType = trtcInfoType_IPtrIPtr;

  tStrIntTree = tRtcTree_StrI32;
  tStrIntTree_ItemType = trtcItemType_StrI32;
  tStrIntTree_InfoType = trtcInfoType_StrI32;

implementation

{$IFDEF RTC_WANT_GENERICS} {$DEFINE RTC_GENERIC}
constructor tRtcSearchTree<itemType,infoType>.Create(size:integer; const _min:itemType; const _nil:infoType; const _itemComparer:IComparer<itemType>; const _infoComparer:IComparer<infoType>);
  begin
  inherited Create;
  itemMin:=_min;
  infoNil:=_nil;
  itemC:=_itemComparer;
  infoC:=_infoComparer;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(itemMin,infoNil,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(itemMin,infoNil,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;
constructor tRtcSearchTree<itemType,infoType>.Create(size:integer; const _min:itemType; const _nil:infoType);
  begin
  Create(size,_min,_nil,TComparer<itemType>.Default);
  end;
constructor tRtcSearchTree<itemType,infoType>.Create(size:integer; const _min:itemType; const _nil:infoType; const _itemComparer:IComparer<itemType>);
  begin
  Create(size,_min,_nil,_itemComparer,TComparer<infoType>.Default);
  end;
function tRtcSearchTree<itemType,infoType>.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;
destructor tRtcSearchTree<itemType,infoType>.Destroy;
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
function tRtcSearchTree<itemType,infoType>.New_Node(const k:itemType; const i:infoType; const bi:boolean; const ll,rr:pnode):pnode;
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
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;
procedure tRtcSearchTree<itemType,infoType>.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;
procedure tRtcSearchTree<itemType,infoType>.Del_Node(node:pnode);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;
procedure tRtcSearchTree<itemType,infoType>.Change(const v:itemType;const info:infoType);
  begin
  remove(v);
  insert(v,info);
  end;
procedure tRtcSearchTree<itemType,infoType>.RemoveThis(var t:pnode);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=infoNil;
  t^.key:=itemMin;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;
procedure tRtcSearchTree<itemType,infoType>.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=infoNil;
  head^.key:=itemMin;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;
function tRtcSearchTree<itemType,infoType>.Search(const v:itemType):infoType;
  var
    x:pnode;
  begin
  x:=head^.r;
  while (x<>z) and (itemC.Compare(v,x^.key)<>0) do
    if (itemC.Compare(v,x^.key)<0) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcSearchTree<itemType,infoType>.iSearch(const v:infoType):itemType;
  var
    x:pnode;
  begin
  x:=head^.r2;
  while (x<>z) and (infoC.Compare(v,x^.info)<>0) do
    if (infoC.Compare(v,x^.info)<0) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;
function tRtcSearchTree<itemType,infoType>.Search_Min(var i:infoType):itemType;
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
function tRtcSearchTree<itemType,infoType>.iSearch_Min(var i:itemType):infoType;
  var
    x:pnode;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=itemMin;
    Result:=infoNil;
    end;
  end;
function tRtcSearchTree<itemType,infoType>.Search_Max(var i:infoType):itemType;
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
function tRtcSearchTree<itemType,infoType>.iSearch_Max(var i:itemType):infoType;
  var
    x:pnode;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=itemMin;
    Result:=infoNil;
    end;
  end;
function tRtcSearchTree<itemType,infoType>.Search_L(const v:itemType; var i:infoType):itemType;
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
function tRtcSearchTree<itemType,infoType>.iSearch_L(const v:infoType; var i:itemType):infoType;
  var
    x,y:pnode;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (infoC.Compare(x^.info,v)<0) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (infoC.Compare(x^.info,v)=0) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;
function tRtcSearchTree<itemType,infoType>.Search_G(const v:itemType; var i:infoType):itemType;
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
function tRtcSearchTree<itemType,infoType>.iSearch_G(const v:infoType; var i:itemType):infoType;
  var
    x,y:pnode;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (infoC.Compare(x^.info,v)>0) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (infoC.Compare(x^.info,v)=0) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;
function tRtcSearchTree<itemType,infoType>.Search_LE(const v:itemType; var i:infoType):itemType;
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
function tRtcSearchTree<itemType,infoType>.iSearch_LE(const v:infoType; var i:itemType):infoType;
  var
    x,y:pnode;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (infoC.Compare(v,x^.info)<>0) do
    begin
    if (infoC.Compare(x^.info,v)<0) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;
function tRtcSearchTree<itemType,infoType>.Search_GE(const v:itemType; var i:infoType):itemType;
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
function tRtcSearchTree<itemType,infoType>.iSearch_GE(const v:infoType; var i:itemType):infoType;
  var
    x,y:pnode;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (infoC.Compare(v,x^.info)<>0) do
    begin
    if (infoC.Compare(x^.info,v)>0) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;
procedure tRtcSearchTree<itemType,infoType>.Insert_split;
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
procedure tRtcSearchTree<itemType,infoType>.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
  
    if ( (infoC.Compare(xv2,g^.info)<0) or ((infoC.Compare(xv2,g^.info)=0) and (itemC.Compare(xv,g^.key)<0)) ) <>
       ( (infoC.Compare(xv2,p^.info)<0) or ((infoC.Compare(xv2,p^.info)=0) and (itemC.Compare(xv,p^.key)<0)) ) then
      begin
      // p_rotate_g; ->
      if (infoC.Compare(xv2,g^.info)<0) then c:=g^.l2
      else if (infoC.Compare(xv2,g^.info)>0) then c:=g^.r2
      else if (itemC.Compare(xv,g^.key)<0) then c:=g^.l2
      else c:=g^.r2;
      if (infoC.Compare(xv2,c^.info)<0) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (infoC.Compare(xv2,c^.info)>0) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (itemC.Compare(xv,c^.key)<0) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (infoC.Compare(xv2,g^.info)<0) then g^.l2:=p
      else if (infoC.Compare(xv2,g^.info)>0) then g^.r2:=p
      else if (itemC.Compare(xv,g^.key)<0) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (infoC.Compare(xv2,gg^.info)<0) then c:=gg^.l2
    else if (infoC.Compare(xv2,gg^.info)>0) then c:=gg^.r2
    else if (itemC.Compare(xv,gg^.key)<0) then c:=gg^.l2
    else c:=gg^.r2;
    if (infoC.Compare(xv2,c^.info)<0) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (infoC.Compare(xv2,c^.info)>0) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (itemC.Compare(xv,c^.key)<0) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (infoC.Compare(xv2,gg^.info)<0) then gg^.l2:=x
    else if (infoC.Compare(xv2,gg^.info)>0) then gg^.r2:=x
    else if (itemC.Compare(xv,gg^.key)<0) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;
procedure tRtcSearchTree<itemType,infoType>.Insert(const v:itemType;const info:infoType);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (infoC.Compare(xv2,x^.info)<0) then x:=x^.l2
    else if (infoC.Compare(xv2,x^.info)>0) then x:=x^.r2
    else if (itemC.Compare(v,x^.key)<0) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (infoC.Compare(xv2,p^.info)<0) then p^.l2:=x
  else if (infoC.Compare(xv2,p^.info)>0) then p^.r2:=x
  else if (itemC.Compare(v,p^.key)<0) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;
procedure tRtcSearchTree<itemType,infoType>.Remove_AddParentNode(node:pnode);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;
function tRtcSearchTree<itemType,infoType>.Remove_GetParentNode:pnode;
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
procedure tRtcSearchTree<itemType,infoType>.Remove(const v:itemType);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (itemC.Compare(v,x^.key)<>0) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (infoC.Compare(xv2,x^.info)<0) then x:=x^.l2
    else if (infoC.Compare(xv2,x^.info)>0) then x:=x^.r2
    else if (itemC.Compare(v,x^.key)<0) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=infoNil;
  t^.key:=itemMin;
  del_node(t);
  Dec(cnt);
  end;
function tRtcSearchTree<itemType,infoType>.Count: cardinal;
  begin
  Result:=cnt;
  end;
{$ENDIF} // {$IFDEF RTC_WANT_GENERICS}

constructor tRtcTree_I32I32.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I32I32,vrtcInfoNIL_I32I32,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_I32I32,vrtcInfoNIL_I32I32,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_I32I32.Destroy;
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

function tRtcTree_I32I32.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_I32I32.New_Node(const k:trtcItemType_I32I32; const i:trtcInfoType_I32I32; const bi:boolean; const ll,rr:prtcNode3_I32I32):prtcNode3_I32I32;
  var
    p:prtcNodeArr3_I32I32;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_I32I32);
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
    GetMem(Result,SizeOf(trtcNode3_I32I32));
  FillChar(Result^,SizeOf(trtcNode3_I32I32),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_I32I32.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_I32I32.Del_Node(node:prtcNode3_I32I32);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_I32I32.Change(const v:trtcItemType_I32I32;const info:trtcInfoType_I32I32);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_I32I32.RemoveThis(var t:prtcNode3_I32I32);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I32I32;
  t^.key:=vrtcItemMin_I32I32;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_I32I32.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I32I32;
  head^.key:=vrtcItemMin_I32I32;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_I32I32.Search(const v:trtcItemType_I32I32):trtcInfoType_I32I32;
  var
    x:prtcNode3_I32I32;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_I32I32.iSearch(const v:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x:prtcNode3_I32I32;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_I32I32.Search_Min(var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x:prtcNode3_I32I32;
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
function tRtcTree_I32I32.iSearch_Min(var i:trtcItemType_I32I32):trtcInfoType_I32I32;
  var
    x:prtcNode3_I32I32;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I32I32;
    Result:=vrtcInfoNIL_I32I32;
    end;
  end;

function tRtcTree_I32I32.Search_Max(var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x:prtcNode3_I32I32;
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
function tRtcTree_I32I32.iSearch_Max(var i:trtcItemType_I32I32):trtcInfoType_I32I32;
  var
    x:prtcNode3_I32I32;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I32I32;
    Result:=vrtcInfoNIL_I32I32;
    end;
  end;

function tRtcTree_I32I32.Search_L(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x,y:prtcNode3_I32I32;
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
function tRtcTree_I32I32.iSearch_L(const v:trtcInfoType_I32I32; var i:trtcItemType_I32I32):trtcInfoType_I32I32;
  var
    x,y:prtcNode3_I32I32;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I32I32.Search_G(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x,y:prtcNode3_I32I32;
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
function tRtcTree_I32I32.iSearch_G(const v:trtcInfoType_I32I32; var i:trtcItemType_I32I32):trtcInfoType_I32I32;
  var
    x,y:prtcNode3_I32I32;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I32I32.Search_LE(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x,y:prtcNode3_I32I32;
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
function tRtcTree_I32I32.iSearch_LE(const v:trtcInfoType_I32I32; var i:trtcItemType_I32I32):trtcInfoType_I32I32;
  var
    x,y:prtcNode3_I32I32;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_I32I32.Search_GE(const v:trtcItemType_I32I32; var i:trtcInfoType_I32I32):trtcItemType_I32I32;
  var
    x,y:prtcNode3_I32I32;
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
function tRtcTree_I32I32.iSearch_GE(const v:trtcInfoType_I32I32; var i:trtcItemType_I32I32):trtcInfoType_I32I32;
  var
    x,y:prtcNode3_I32I32;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_I32I32.Insert_split;
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
procedure tRtcTree_I32I32.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_I32I32.Insert(const v:trtcItemType_I32I32;const info:trtcInfoType_I32I32);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_I32I32.Remove_AddParentNode(node:prtcNode3_I32I32);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_I32I32.Remove_GetParentNode:prtcNode3_I32I32;
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

procedure tRtcTree_I32I32.Remove(const v:trtcItemType_I32I32);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I32I32;
  t^.key:=vrtcItemMin_I32I32;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_I32I32.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_I32Obj.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I32Obj,vrtcInfoNIL_I32Obj,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_I32Obj,vrtcInfoNIL_I32Obj,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_I32Obj.Destroy;
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

function tRtcTree_I32Obj.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_I32Obj.New_Node(const k:trtcItemType_I32Obj; const i:trtcInfoType_I32Obj; const bi:boolean; const ll,rr:prtcNode3_I32Obj):prtcNode3_I32Obj;
  var
    p:prtcNodeArr3_I32Obj;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_I32Obj);
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
    GetMem(Result,SizeOf(trtcNode3_I32Obj));
  FillChar(Result^,SizeOf(trtcNode3_I32Obj),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_I32Obj.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_I32Obj.Del_Node(node:prtcNode3_I32Obj);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_I32Obj.Change(const v:trtcItemType_I32Obj;const info:trtcInfoType_I32Obj);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_I32Obj.RemoveThis(var t:prtcNode3_I32Obj);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I32Obj;
  t^.key:=vrtcItemMin_I32Obj;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_I32Obj.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I32Obj;
  head^.key:=vrtcItemMin_I32Obj;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_I32Obj.Search(const v:trtcItemType_I32Obj):trtcInfoType_I32Obj;
  var
    x:prtcNode3_I32Obj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_I32Obj.iSearch(const v:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x:prtcNode3_I32Obj;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (trtcInfoComp_I32Obj(v)<trtcInfoComp_I32Obj(x^.info)) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_I32Obj.Search_Min(var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x:prtcNode3_I32Obj;
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
function tRtcTree_I32Obj.iSearch_Min(var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;
  var
    x:prtcNode3_I32Obj;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I32Obj;
    Result:=vrtcInfoNIL_I32Obj;
    end;
  end;

function tRtcTree_I32Obj.Search_Max(var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x:prtcNode3_I32Obj;
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
function tRtcTree_I32Obj.iSearch_Max(var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;
  var
    x:prtcNode3_I32Obj;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I32Obj;
    Result:=vrtcInfoNIL_I32Obj;
    end;
  end;

function tRtcTree_I32Obj.Search_L(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x,y:prtcNode3_I32Obj;
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
function tRtcTree_I32Obj.iSearch_L(const v:trtcInfoType_I32Obj; var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;
  var
    x,y:prtcNode3_I32Obj;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_I32Obj(x^.info)<trtcInfoComp_I32Obj(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I32Obj.Search_G(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x,y:prtcNode3_I32Obj;
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
function tRtcTree_I32Obj.iSearch_G(const v:trtcInfoType_I32Obj; var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;
  var
    x,y:prtcNode3_I32Obj;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_I32Obj(x^.info)>trtcInfoComp_I32Obj(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I32Obj.Search_LE(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x,y:prtcNode3_I32Obj;
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
function tRtcTree_I32Obj.iSearch_LE(const v:trtcInfoType_I32Obj; var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;
  var
    x,y:prtcNode3_I32Obj;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_I32Obj(x^.info)<trtcInfoComp_I32Obj(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_I32Obj.Search_GE(const v:trtcItemType_I32Obj; var i:trtcInfoType_I32Obj):trtcItemType_I32Obj;
  var
    x,y:prtcNode3_I32Obj;
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
function tRtcTree_I32Obj.iSearch_GE(const v:trtcInfoType_I32Obj; var i:trtcItemType_I32Obj):trtcInfoType_I32Obj;
  var
    x,y:prtcNode3_I32Obj;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_I32Obj(x^.info)>trtcInfoComp_I32Obj(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_I32Obj.Insert_split;
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
procedure tRtcTree_I32Obj.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (trtcInfoComp_I32Obj(xv2)<trtcInfoComp_I32Obj(g^.info)) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (trtcInfoComp_I32Obj(xv2)<trtcInfoComp_I32Obj(p^.info)) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (trtcInfoComp_I32Obj(xv2)<trtcInfoComp_I32Obj(g^.info)) then c:=g^.l2
      else if (trtcInfoComp_I32Obj(xv2)>trtcInfoComp_I32Obj(g^.info)) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (trtcInfoComp_I32Obj(xv2)<trtcInfoComp_I32Obj(c^.info)) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (trtcInfoComp_I32Obj(xv2)>trtcInfoComp_I32Obj(c^.info)) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (trtcInfoComp_I32Obj(xv2)<trtcInfoComp_I32Obj(g^.info)) then g^.l2:=p
      else if (trtcInfoComp_I32Obj(xv2)>trtcInfoComp_I32Obj(g^.info)) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (trtcInfoComp_I32Obj(xv2)<trtcInfoComp_I32Obj(gg^.info)) then c:=gg^.l2
    else if (trtcInfoComp_I32Obj(xv2)>trtcInfoComp_I32Obj(gg^.info)) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (trtcInfoComp_I32Obj(xv2)<trtcInfoComp_I32Obj(c^.info)) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (trtcInfoComp_I32Obj(xv2)>trtcInfoComp_I32Obj(c^.info)) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (trtcInfoComp_I32Obj(xv2)<trtcInfoComp_I32Obj(gg^.info)) then gg^.l2:=x
    else if (trtcInfoComp_I32Obj(xv2)>trtcInfoComp_I32Obj(gg^.info)) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_I32Obj.Insert(const v:trtcItemType_I32Obj;const info:trtcInfoType_I32Obj);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (trtcInfoComp_I32Obj(xv2)<trtcInfoComp_I32Obj(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_I32Obj(xv2)>trtcInfoComp_I32Obj(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (trtcInfoComp_I32Obj(xv2)<trtcInfoComp_I32Obj(p^.info)) then p^.l2:=x
  else if (trtcInfoComp_I32Obj(xv2)>trtcInfoComp_I32Obj(p^.info)) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_I32Obj.Remove_AddParentNode(node:prtcNode3_I32Obj);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_I32Obj.Remove_GetParentNode:prtcNode3_I32Obj;
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

procedure tRtcTree_I32Obj.Remove(const v:trtcItemType_I32Obj);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (trtcInfoComp_I32Obj(xv2)<trtcInfoComp_I32Obj(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_I32Obj(xv2)>trtcInfoComp_I32Obj(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I32Obj;
  t^.key:=vrtcItemMin_I32Obj;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_I32Obj.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_I32Ptr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I32Ptr,vrtcInfoNIL_I32Ptr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_I32Ptr,vrtcInfoNIL_I32Ptr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_I32Ptr.Destroy;
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

function tRtcTree_I32Ptr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_I32Ptr.New_Node(const k:trtcItemType_I32Ptr; const i:trtcInfoType_I32Ptr; const bi:boolean; const ll,rr:prtcNode3_I32Ptr):prtcNode3_I32Ptr;
  var
    p:prtcNodeArr3_I32Ptr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_I32Ptr);
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
    GetMem(Result,SizeOf(trtcNode3_I32Ptr));
  FillChar(Result^,SizeOf(trtcNode3_I32Ptr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_I32Ptr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_I32Ptr.Del_Node(node:prtcNode3_I32Ptr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_I32Ptr.Change(const v:trtcItemType_I32Ptr;const info:trtcInfoType_I32Ptr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_I32Ptr.RemoveThis(var t:prtcNode3_I32Ptr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I32Ptr;
  t^.key:=vrtcItemMin_I32Ptr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_I32Ptr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I32Ptr;
  head^.key:=vrtcItemMin_I32Ptr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_I32Ptr.Search(const v:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;
  var
    x:prtcNode3_I32Ptr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_I32Ptr.iSearch(const v:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x:prtcNode3_I32Ptr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (trtcInfoComp_I32Ptr(v)<trtcInfoComp_I32Ptr(x^.info)) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_I32Ptr.Search_Min(var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x:prtcNode3_I32Ptr;
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
function tRtcTree_I32Ptr.iSearch_Min(var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;
  var
    x:prtcNode3_I32Ptr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I32Ptr;
    Result:=vrtcInfoNIL_I32Ptr;
    end;
  end;

function tRtcTree_I32Ptr.Search_Max(var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x:prtcNode3_I32Ptr;
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
function tRtcTree_I32Ptr.iSearch_Max(var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;
  var
    x:prtcNode3_I32Ptr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I32Ptr;
    Result:=vrtcInfoNIL_I32Ptr;
    end;
  end;

function tRtcTree_I32Ptr.Search_L(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x,y:prtcNode3_I32Ptr;
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
function tRtcTree_I32Ptr.iSearch_L(const v:trtcInfoType_I32Ptr; var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;
  var
    x,y:prtcNode3_I32Ptr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_I32Ptr(x^.info)<trtcInfoComp_I32Ptr(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I32Ptr.Search_G(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x,y:prtcNode3_I32Ptr;
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
function tRtcTree_I32Ptr.iSearch_G(const v:trtcInfoType_I32Ptr; var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;
  var
    x,y:prtcNode3_I32Ptr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_I32Ptr(x^.info)>trtcInfoComp_I32Ptr(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I32Ptr.Search_LE(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x,y:prtcNode3_I32Ptr;
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
function tRtcTree_I32Ptr.iSearch_LE(const v:trtcInfoType_I32Ptr; var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;
  var
    x,y:prtcNode3_I32Ptr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_I32Ptr(x^.info)<trtcInfoComp_I32Ptr(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_I32Ptr.Search_GE(const v:trtcItemType_I32Ptr; var i:trtcInfoType_I32Ptr):trtcItemType_I32Ptr;
  var
    x,y:prtcNode3_I32Ptr;
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
function tRtcTree_I32Ptr.iSearch_GE(const v:trtcInfoType_I32Ptr; var i:trtcItemType_I32Ptr):trtcInfoType_I32Ptr;
  var
    x,y:prtcNode3_I32Ptr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_I32Ptr(x^.info)>trtcInfoComp_I32Ptr(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_I32Ptr.Insert_split;
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
procedure tRtcTree_I32Ptr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (trtcInfoComp_I32Ptr(xv2)<trtcInfoComp_I32Ptr(g^.info)) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (trtcInfoComp_I32Ptr(xv2)<trtcInfoComp_I32Ptr(p^.info)) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (trtcInfoComp_I32Ptr(xv2)<trtcInfoComp_I32Ptr(g^.info)) then c:=g^.l2
      else if (trtcInfoComp_I32Ptr(xv2)>trtcInfoComp_I32Ptr(g^.info)) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (trtcInfoComp_I32Ptr(xv2)<trtcInfoComp_I32Ptr(c^.info)) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (trtcInfoComp_I32Ptr(xv2)>trtcInfoComp_I32Ptr(c^.info)) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (trtcInfoComp_I32Ptr(xv2)<trtcInfoComp_I32Ptr(g^.info)) then g^.l2:=p
      else if (trtcInfoComp_I32Ptr(xv2)>trtcInfoComp_I32Ptr(g^.info)) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (trtcInfoComp_I32Ptr(xv2)<trtcInfoComp_I32Ptr(gg^.info)) then c:=gg^.l2
    else if (trtcInfoComp_I32Ptr(xv2)>trtcInfoComp_I32Ptr(gg^.info)) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (trtcInfoComp_I32Ptr(xv2)<trtcInfoComp_I32Ptr(c^.info)) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (trtcInfoComp_I32Ptr(xv2)>trtcInfoComp_I32Ptr(c^.info)) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (trtcInfoComp_I32Ptr(xv2)<trtcInfoComp_I32Ptr(gg^.info)) then gg^.l2:=x
    else if (trtcInfoComp_I32Ptr(xv2)>trtcInfoComp_I32Ptr(gg^.info)) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_I32Ptr.Insert(const v:trtcItemType_I32Ptr;const info:trtcInfoType_I32Ptr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (trtcInfoComp_I32Ptr(xv2)<trtcInfoComp_I32Ptr(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_I32Ptr(xv2)>trtcInfoComp_I32Ptr(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (trtcInfoComp_I32Ptr(xv2)<trtcInfoComp_I32Ptr(p^.info)) then p^.l2:=x
  else if (trtcInfoComp_I32Ptr(xv2)>trtcInfoComp_I32Ptr(p^.info)) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_I32Ptr.Remove_AddParentNode(node:prtcNode3_I32Ptr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_I32Ptr.Remove_GetParentNode:prtcNode3_I32Ptr;
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

procedure tRtcTree_I32Ptr.Remove(const v:trtcItemType_I32Ptr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (trtcInfoComp_I32Ptr(xv2)<trtcInfoComp_I32Ptr(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_I32Ptr(xv2)>trtcInfoComp_I32Ptr(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I32Ptr;
  t^.key:=vrtcItemMin_I32Ptr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_I32Ptr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_I32Str.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I32Str,vrtcInfoNIL_I32Str,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_I32Str,vrtcInfoNIL_I32Str,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_I32Str.Destroy;
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

function tRtcTree_I32Str.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_I32Str.New_Node(const k:trtcItemType_I32Str; const i:trtcInfoType_I32Str; const bi:boolean; const ll,rr:prtcNode3_I32Str):prtcNode3_I32Str;
  var
    p:prtcNodeArr3_I32Str;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_I32Str);
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
    GetMem(Result,SizeOf(trtcNode3_I32Str));
  FillChar(Result^,SizeOf(trtcNode3_I32Str),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_I32Str.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_I32Str.Del_Node(node:prtcNode3_I32Str);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_I32Str.Change(const v:trtcItemType_I32Str;const info:trtcInfoType_I32Str);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_I32Str.RemoveThis(var t:prtcNode3_I32Str);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I32Str;
  t^.key:=vrtcItemMin_I32Str;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_I32Str.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I32Str;
  head^.key:=vrtcItemMin_I32Str;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_I32Str.Search(const v:trtcItemType_I32Str):trtcInfoType_I32Str;
  var
    x:prtcNode3_I32Str;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_I32Str.iSearch(const v:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x:prtcNode3_I32Str;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_I32Str.Search_Min(var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x:prtcNode3_I32Str;
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
function tRtcTree_I32Str.iSearch_Min(var i:trtcItemType_I32Str):trtcInfoType_I32Str;
  var
    x:prtcNode3_I32Str;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I32Str;
    Result:=vrtcInfoNIL_I32Str;
    end;
  end;

function tRtcTree_I32Str.Search_Max(var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x:prtcNode3_I32Str;
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
function tRtcTree_I32Str.iSearch_Max(var i:trtcItemType_I32Str):trtcInfoType_I32Str;
  var
    x:prtcNode3_I32Str;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I32Str;
    Result:=vrtcInfoNIL_I32Str;
    end;
  end;

function tRtcTree_I32Str.Search_L(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x,y:prtcNode3_I32Str;
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
function tRtcTree_I32Str.iSearch_L(const v:trtcInfoType_I32Str; var i:trtcItemType_I32Str):trtcInfoType_I32Str;
  var
    x,y:prtcNode3_I32Str;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I32Str.Search_G(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x,y:prtcNode3_I32Str;
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
function tRtcTree_I32Str.iSearch_G(const v:trtcInfoType_I32Str; var i:trtcItemType_I32Str):trtcInfoType_I32Str;
  var
    x,y:prtcNode3_I32Str;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I32Str.Search_LE(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x,y:prtcNode3_I32Str;
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
function tRtcTree_I32Str.iSearch_LE(const v:trtcInfoType_I32Str; var i:trtcItemType_I32Str):trtcInfoType_I32Str;
  var
    x,y:prtcNode3_I32Str;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_I32Str.Search_GE(const v:trtcItemType_I32Str; var i:trtcInfoType_I32Str):trtcItemType_I32Str;
  var
    x,y:prtcNode3_I32Str;
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
function tRtcTree_I32Str.iSearch_GE(const v:trtcInfoType_I32Str; var i:trtcItemType_I32Str):trtcInfoType_I32Str;
  var
    x,y:prtcNode3_I32Str;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_I32Str.Insert_split;
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
procedure tRtcTree_I32Str.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_I32Str.Insert(const v:trtcItemType_I32Str;const info:trtcInfoType_I32Str);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_I32Str.Remove_AddParentNode(node:prtcNode3_I32Str);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_I32Str.Remove_GetParentNode:prtcNode3_I32Str;
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

procedure tRtcTree_I32Str.Remove(const v:trtcItemType_I32Str);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I32Str;
  t^.key:=vrtcItemMin_I32Str;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_I32Str.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_I32WStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I32WStr,vrtcInfoNIL_I32WStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_I32WStr,vrtcInfoNIL_I32WStr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_I32WStr.Destroy;
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

function tRtcTree_I32WStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_I32WStr.New_Node(const k:trtcItemType_I32WStr; const i:trtcInfoType_I32WStr; const bi:boolean; const ll,rr:prtcNode3_I32WStr):prtcNode3_I32WStr;
  var
    p:prtcNodeArr3_I32WStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_I32WStr);
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
    GetMem(Result,SizeOf(trtcNode3_I32WStr));
  FillChar(Result^,SizeOf(trtcNode3_I32WStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_I32WStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_I32WStr.Del_Node(node:prtcNode3_I32WStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_I32WStr.Change(const v:trtcItemType_I32WStr;const info:trtcInfoType_I32WStr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_I32WStr.RemoveThis(var t:prtcNode3_I32WStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I32WStr;
  t^.key:=vrtcItemMin_I32WStr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_I32WStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I32WStr;
  head^.key:=vrtcItemMin_I32WStr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_I32WStr.Search(const v:trtcItemType_I32WStr):trtcInfoType_I32WStr;
  var
    x:prtcNode3_I32WStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_I32WStr.iSearch(const v:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x:prtcNode3_I32WStr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_I32WStr.Search_Min(var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x:prtcNode3_I32WStr;
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
function tRtcTree_I32WStr.iSearch_Min(var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;
  var
    x:prtcNode3_I32WStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I32WStr;
    Result:=vrtcInfoNIL_I32WStr;
    end;
  end;

function tRtcTree_I32WStr.Search_Max(var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x:prtcNode3_I32WStr;
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
function tRtcTree_I32WStr.iSearch_Max(var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;
  var
    x:prtcNode3_I32WStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I32WStr;
    Result:=vrtcInfoNIL_I32WStr;
    end;
  end;

function tRtcTree_I32WStr.Search_L(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x,y:prtcNode3_I32WStr;
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
function tRtcTree_I32WStr.iSearch_L(const v:trtcInfoType_I32WStr; var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;
  var
    x,y:prtcNode3_I32WStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I32WStr.Search_G(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x,y:prtcNode3_I32WStr;
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
function tRtcTree_I32WStr.iSearch_G(const v:trtcInfoType_I32WStr; var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;
  var
    x,y:prtcNode3_I32WStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I32WStr.Search_LE(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x,y:prtcNode3_I32WStr;
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
function tRtcTree_I32WStr.iSearch_LE(const v:trtcInfoType_I32WStr; var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;
  var
    x,y:prtcNode3_I32WStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_I32WStr.Search_GE(const v:trtcItemType_I32WStr; var i:trtcInfoType_I32WStr):trtcItemType_I32WStr;
  var
    x,y:prtcNode3_I32WStr;
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
function tRtcTree_I32WStr.iSearch_GE(const v:trtcInfoType_I32WStr; var i:trtcItemType_I32WStr):trtcInfoType_I32WStr;
  var
    x,y:prtcNode3_I32WStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_I32WStr.Insert_split;
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
procedure tRtcTree_I32WStr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_I32WStr.Insert(const v:trtcItemType_I32WStr;const info:trtcInfoType_I32WStr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_I32WStr.Remove_AddParentNode(node:prtcNode3_I32WStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_I32WStr.Remove_GetParentNode:prtcNode3_I32WStr;
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

procedure tRtcTree_I32WStr.Remove(const v:trtcItemType_I32WStr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I32WStr;
  t^.key:=vrtcItemMin_I32WStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_I32WStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_I64I64.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I64I64,vrtcInfoNIL_I64I64,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_I64I64,vrtcInfoNIL_I64I64,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_I64I64.Destroy;
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

function tRtcTree_I64I64.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_I64I64.New_Node(const k:trtcItemType_I64I64; const i:trtcInfoType_I64I64; const bi:boolean; const ll,rr:prtcNode3_I64I64):prtcNode3_I64I64;
  var
    p:prtcNodeArr3_I64I64;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_I64I64);
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
    GetMem(Result,SizeOf(trtcNode3_I64I64));
  FillChar(Result^,SizeOf(trtcNode3_I64I64),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_I64I64.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_I64I64.Del_Node(node:prtcNode3_I64I64);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_I64I64.Change(const v:trtcItemType_I64I64;const info:trtcInfoType_I64I64);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_I64I64.RemoveThis(var t:prtcNode3_I64I64);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I64I64;
  t^.key:=vrtcItemMin_I64I64;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_I64I64.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I64I64;
  head^.key:=vrtcItemMin_I64I64;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_I64I64.Search(const v:trtcItemType_I64I64):trtcInfoType_I64I64;
  var
    x:prtcNode3_I64I64;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_I64I64.iSearch(const v:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x:prtcNode3_I64I64;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_I64I64.Search_Min(var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x:prtcNode3_I64I64;
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
function tRtcTree_I64I64.iSearch_Min(var i:trtcItemType_I64I64):trtcInfoType_I64I64;
  var
    x:prtcNode3_I64I64;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I64I64;
    Result:=vrtcInfoNIL_I64I64;
    end;
  end;

function tRtcTree_I64I64.Search_Max(var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x:prtcNode3_I64I64;
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
function tRtcTree_I64I64.iSearch_Max(var i:trtcItemType_I64I64):trtcInfoType_I64I64;
  var
    x:prtcNode3_I64I64;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I64I64;
    Result:=vrtcInfoNIL_I64I64;
    end;
  end;

function tRtcTree_I64I64.Search_L(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x,y:prtcNode3_I64I64;
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
function tRtcTree_I64I64.iSearch_L(const v:trtcInfoType_I64I64; var i:trtcItemType_I64I64):trtcInfoType_I64I64;
  var
    x,y:prtcNode3_I64I64;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I64I64.Search_G(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x,y:prtcNode3_I64I64;
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
function tRtcTree_I64I64.iSearch_G(const v:trtcInfoType_I64I64; var i:trtcItemType_I64I64):trtcInfoType_I64I64;
  var
    x,y:prtcNode3_I64I64;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I64I64.Search_LE(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x,y:prtcNode3_I64I64;
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
function tRtcTree_I64I64.iSearch_LE(const v:trtcInfoType_I64I64; var i:trtcItemType_I64I64):trtcInfoType_I64I64;
  var
    x,y:prtcNode3_I64I64;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_I64I64.Search_GE(const v:trtcItemType_I64I64; var i:trtcInfoType_I64I64):trtcItemType_I64I64;
  var
    x,y:prtcNode3_I64I64;
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
function tRtcTree_I64I64.iSearch_GE(const v:trtcInfoType_I64I64; var i:trtcItemType_I64I64):trtcInfoType_I64I64;
  var
    x,y:prtcNode3_I64I64;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_I64I64.Insert_split;
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
procedure tRtcTree_I64I64.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_I64I64.Insert(const v:trtcItemType_I64I64;const info:trtcInfoType_I64I64);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_I64I64.Remove_AddParentNode(node:prtcNode3_I64I64);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_I64I64.Remove_GetParentNode:prtcNode3_I64I64;
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

procedure tRtcTree_I64I64.Remove(const v:trtcItemType_I64I64);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I64I64;
  t^.key:=vrtcItemMin_I64I64;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_I64I64.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_I64Obj.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I64Obj,vrtcInfoNIL_I64Obj,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_I64Obj,vrtcInfoNIL_I64Obj,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_I64Obj.Destroy;
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

function tRtcTree_I64Obj.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_I64Obj.New_Node(const k:trtcItemType_I64Obj; const i:trtcInfoType_I64Obj; const bi:boolean; const ll,rr:prtcNode3_I64Obj):prtcNode3_I64Obj;
  var
    p:prtcNodeArr3_I64Obj;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_I64Obj);
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
    GetMem(Result,SizeOf(trtcNode3_I64Obj));
  FillChar(Result^,SizeOf(trtcNode3_I64Obj),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_I64Obj.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_I64Obj.Del_Node(node:prtcNode3_I64Obj);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_I64Obj.Change(const v:trtcItemType_I64Obj;const info:trtcInfoType_I64Obj);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_I64Obj.RemoveThis(var t:prtcNode3_I64Obj);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I64Obj;
  t^.key:=vrtcItemMin_I64Obj;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_I64Obj.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I64Obj;
  head^.key:=vrtcItemMin_I64Obj;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_I64Obj.Search(const v:trtcItemType_I64Obj):trtcInfoType_I64Obj;
  var
    x:prtcNode3_I64Obj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_I64Obj.iSearch(const v:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x:prtcNode3_I64Obj;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (trtcInfoComp_I64Obj(v)<trtcInfoComp_I64Obj(x^.info)) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_I64Obj.Search_Min(var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x:prtcNode3_I64Obj;
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
function tRtcTree_I64Obj.iSearch_Min(var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;
  var
    x:prtcNode3_I64Obj;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I64Obj;
    Result:=vrtcInfoNIL_I64Obj;
    end;
  end;

function tRtcTree_I64Obj.Search_Max(var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x:prtcNode3_I64Obj;
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
function tRtcTree_I64Obj.iSearch_Max(var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;
  var
    x:prtcNode3_I64Obj;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I64Obj;
    Result:=vrtcInfoNIL_I64Obj;
    end;
  end;

function tRtcTree_I64Obj.Search_L(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x,y:prtcNode3_I64Obj;
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
function tRtcTree_I64Obj.iSearch_L(const v:trtcInfoType_I64Obj; var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;
  var
    x,y:prtcNode3_I64Obj;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_I64Obj(x^.info)<trtcInfoComp_I64Obj(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I64Obj.Search_G(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x,y:prtcNode3_I64Obj;
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
function tRtcTree_I64Obj.iSearch_G(const v:trtcInfoType_I64Obj; var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;
  var
    x,y:prtcNode3_I64Obj;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_I64Obj(x^.info)>trtcInfoComp_I64Obj(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I64Obj.Search_LE(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x,y:prtcNode3_I64Obj;
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
function tRtcTree_I64Obj.iSearch_LE(const v:trtcInfoType_I64Obj; var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;
  var
    x,y:prtcNode3_I64Obj;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_I64Obj(x^.info)<trtcInfoComp_I64Obj(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_I64Obj.Search_GE(const v:trtcItemType_I64Obj; var i:trtcInfoType_I64Obj):trtcItemType_I64Obj;
  var
    x,y:prtcNode3_I64Obj;
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
function tRtcTree_I64Obj.iSearch_GE(const v:trtcInfoType_I64Obj; var i:trtcItemType_I64Obj):trtcInfoType_I64Obj;
  var
    x,y:prtcNode3_I64Obj;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_I64Obj(x^.info)>trtcInfoComp_I64Obj(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_I64Obj.Insert_split;
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
procedure tRtcTree_I64Obj.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (trtcInfoComp_I64Obj(xv2)<trtcInfoComp_I64Obj(g^.info)) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (trtcInfoComp_I64Obj(xv2)<trtcInfoComp_I64Obj(p^.info)) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (trtcInfoComp_I64Obj(xv2)<trtcInfoComp_I64Obj(g^.info)) then c:=g^.l2
      else if (trtcInfoComp_I64Obj(xv2)>trtcInfoComp_I64Obj(g^.info)) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (trtcInfoComp_I64Obj(xv2)<trtcInfoComp_I64Obj(c^.info)) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (trtcInfoComp_I64Obj(xv2)>trtcInfoComp_I64Obj(c^.info)) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (trtcInfoComp_I64Obj(xv2)<trtcInfoComp_I64Obj(g^.info)) then g^.l2:=p
      else if (trtcInfoComp_I64Obj(xv2)>trtcInfoComp_I64Obj(g^.info)) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (trtcInfoComp_I64Obj(xv2)<trtcInfoComp_I64Obj(gg^.info)) then c:=gg^.l2
    else if (trtcInfoComp_I64Obj(xv2)>trtcInfoComp_I64Obj(gg^.info)) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (trtcInfoComp_I64Obj(xv2)<trtcInfoComp_I64Obj(c^.info)) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (trtcInfoComp_I64Obj(xv2)>trtcInfoComp_I64Obj(c^.info)) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (trtcInfoComp_I64Obj(xv2)<trtcInfoComp_I64Obj(gg^.info)) then gg^.l2:=x
    else if (trtcInfoComp_I64Obj(xv2)>trtcInfoComp_I64Obj(gg^.info)) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_I64Obj.Insert(const v:trtcItemType_I64Obj;const info:trtcInfoType_I64Obj);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (trtcInfoComp_I64Obj(xv2)<trtcInfoComp_I64Obj(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_I64Obj(xv2)>trtcInfoComp_I64Obj(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (trtcInfoComp_I64Obj(xv2)<trtcInfoComp_I64Obj(p^.info)) then p^.l2:=x
  else if (trtcInfoComp_I64Obj(xv2)>trtcInfoComp_I64Obj(p^.info)) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_I64Obj.Remove_AddParentNode(node:prtcNode3_I64Obj);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_I64Obj.Remove_GetParentNode:prtcNode3_I64Obj;
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

procedure tRtcTree_I64Obj.Remove(const v:trtcItemType_I64Obj);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (trtcInfoComp_I64Obj(xv2)<trtcInfoComp_I64Obj(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_I64Obj(xv2)>trtcInfoComp_I64Obj(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I64Obj;
  t^.key:=vrtcItemMin_I64Obj;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_I64Obj.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_I64Ptr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I64Ptr,vrtcInfoNIL_I64Ptr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_I64Ptr,vrtcInfoNIL_I64Ptr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_I64Ptr.Destroy;
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

function tRtcTree_I64Ptr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_I64Ptr.New_Node(const k:trtcItemType_I64Ptr; const i:trtcInfoType_I64Ptr; const bi:boolean; const ll,rr:prtcNode3_I64Ptr):prtcNode3_I64Ptr;
  var
    p:prtcNodeArr3_I64Ptr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_I64Ptr);
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
    GetMem(Result,SizeOf(trtcNode3_I64Ptr));
  FillChar(Result^,SizeOf(trtcNode3_I64Ptr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_I64Ptr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_I64Ptr.Del_Node(node:prtcNode3_I64Ptr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_I64Ptr.Change(const v:trtcItemType_I64Ptr;const info:trtcInfoType_I64Ptr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_I64Ptr.RemoveThis(var t:prtcNode3_I64Ptr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I64Ptr;
  t^.key:=vrtcItemMin_I64Ptr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_I64Ptr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I64Ptr;
  head^.key:=vrtcItemMin_I64Ptr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_I64Ptr.Search(const v:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;
  var
    x:prtcNode3_I64Ptr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_I64Ptr.iSearch(const v:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x:prtcNode3_I64Ptr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (trtcInfoComp_I64Ptr(v)<trtcInfoComp_I64Ptr(x^.info)) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_I64Ptr.Search_Min(var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x:prtcNode3_I64Ptr;
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
function tRtcTree_I64Ptr.iSearch_Min(var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;
  var
    x:prtcNode3_I64Ptr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I64Ptr;
    Result:=vrtcInfoNIL_I64Ptr;
    end;
  end;

function tRtcTree_I64Ptr.Search_Max(var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x:prtcNode3_I64Ptr;
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
function tRtcTree_I64Ptr.iSearch_Max(var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;
  var
    x:prtcNode3_I64Ptr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I64Ptr;
    Result:=vrtcInfoNIL_I64Ptr;
    end;
  end;

function tRtcTree_I64Ptr.Search_L(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x,y:prtcNode3_I64Ptr;
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
function tRtcTree_I64Ptr.iSearch_L(const v:trtcInfoType_I64Ptr; var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;
  var
    x,y:prtcNode3_I64Ptr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_I64Ptr(x^.info)<trtcInfoComp_I64Ptr(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I64Ptr.Search_G(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x,y:prtcNode3_I64Ptr;
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
function tRtcTree_I64Ptr.iSearch_G(const v:trtcInfoType_I64Ptr; var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;
  var
    x,y:prtcNode3_I64Ptr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_I64Ptr(x^.info)>trtcInfoComp_I64Ptr(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I64Ptr.Search_LE(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x,y:prtcNode3_I64Ptr;
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
function tRtcTree_I64Ptr.iSearch_LE(const v:trtcInfoType_I64Ptr; var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;
  var
    x,y:prtcNode3_I64Ptr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_I64Ptr(x^.info)<trtcInfoComp_I64Ptr(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_I64Ptr.Search_GE(const v:trtcItemType_I64Ptr; var i:trtcInfoType_I64Ptr):trtcItemType_I64Ptr;
  var
    x,y:prtcNode3_I64Ptr;
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
function tRtcTree_I64Ptr.iSearch_GE(const v:trtcInfoType_I64Ptr; var i:trtcItemType_I64Ptr):trtcInfoType_I64Ptr;
  var
    x,y:prtcNode3_I64Ptr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_I64Ptr(x^.info)>trtcInfoComp_I64Ptr(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_I64Ptr.Insert_split;
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
procedure tRtcTree_I64Ptr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (trtcInfoComp_I64Ptr(xv2)<trtcInfoComp_I64Ptr(g^.info)) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (trtcInfoComp_I64Ptr(xv2)<trtcInfoComp_I64Ptr(p^.info)) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (trtcInfoComp_I64Ptr(xv2)<trtcInfoComp_I64Ptr(g^.info)) then c:=g^.l2
      else if (trtcInfoComp_I64Ptr(xv2)>trtcInfoComp_I64Ptr(g^.info)) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (trtcInfoComp_I64Ptr(xv2)<trtcInfoComp_I64Ptr(c^.info)) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (trtcInfoComp_I64Ptr(xv2)>trtcInfoComp_I64Ptr(c^.info)) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (trtcInfoComp_I64Ptr(xv2)<trtcInfoComp_I64Ptr(g^.info)) then g^.l2:=p
      else if (trtcInfoComp_I64Ptr(xv2)>trtcInfoComp_I64Ptr(g^.info)) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (trtcInfoComp_I64Ptr(xv2)<trtcInfoComp_I64Ptr(gg^.info)) then c:=gg^.l2
    else if (trtcInfoComp_I64Ptr(xv2)>trtcInfoComp_I64Ptr(gg^.info)) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (trtcInfoComp_I64Ptr(xv2)<trtcInfoComp_I64Ptr(c^.info)) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (trtcInfoComp_I64Ptr(xv2)>trtcInfoComp_I64Ptr(c^.info)) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (trtcInfoComp_I64Ptr(xv2)<trtcInfoComp_I64Ptr(gg^.info)) then gg^.l2:=x
    else if (trtcInfoComp_I64Ptr(xv2)>trtcInfoComp_I64Ptr(gg^.info)) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_I64Ptr.Insert(const v:trtcItemType_I64Ptr;const info:trtcInfoType_I64Ptr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (trtcInfoComp_I64Ptr(xv2)<trtcInfoComp_I64Ptr(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_I64Ptr(xv2)>trtcInfoComp_I64Ptr(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (trtcInfoComp_I64Ptr(xv2)<trtcInfoComp_I64Ptr(p^.info)) then p^.l2:=x
  else if (trtcInfoComp_I64Ptr(xv2)>trtcInfoComp_I64Ptr(p^.info)) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_I64Ptr.Remove_AddParentNode(node:prtcNode3_I64Ptr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_I64Ptr.Remove_GetParentNode:prtcNode3_I64Ptr;
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

procedure tRtcTree_I64Ptr.Remove(const v:trtcItemType_I64Ptr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (trtcInfoComp_I64Ptr(xv2)<trtcInfoComp_I64Ptr(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_I64Ptr(xv2)>trtcInfoComp_I64Ptr(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I64Ptr;
  t^.key:=vrtcItemMin_I64Ptr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_I64Ptr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_I64Str.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I64Str,vrtcInfoNIL_I64Str,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_I64Str,vrtcInfoNIL_I64Str,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_I64Str.Destroy;
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

function tRtcTree_I64Str.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_I64Str.New_Node(const k:trtcItemType_I64Str; const i:trtcInfoType_I64Str; const bi:boolean; const ll,rr:prtcNode3_I64Str):prtcNode3_I64Str;
  var
    p:prtcNodeArr3_I64Str;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_I64Str);
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
    GetMem(Result,SizeOf(trtcNode3_I64Str));
  FillChar(Result^,SizeOf(trtcNode3_I64Str),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_I64Str.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_I64Str.Del_Node(node:prtcNode3_I64Str);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_I64Str.Change(const v:trtcItemType_I64Str;const info:trtcInfoType_I64Str);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_I64Str.RemoveThis(var t:prtcNode3_I64Str);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I64Str;
  t^.key:=vrtcItemMin_I64Str;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_I64Str.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I64Str;
  head^.key:=vrtcItemMin_I64Str;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_I64Str.Search(const v:trtcItemType_I64Str):trtcInfoType_I64Str;
  var
    x:prtcNode3_I64Str;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_I64Str.iSearch(const v:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x:prtcNode3_I64Str;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_I64Str.Search_Min(var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x:prtcNode3_I64Str;
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
function tRtcTree_I64Str.iSearch_Min(var i:trtcItemType_I64Str):trtcInfoType_I64Str;
  var
    x:prtcNode3_I64Str;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I64Str;
    Result:=vrtcInfoNIL_I64Str;
    end;
  end;

function tRtcTree_I64Str.Search_Max(var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x:prtcNode3_I64Str;
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
function tRtcTree_I64Str.iSearch_Max(var i:trtcItemType_I64Str):trtcInfoType_I64Str;
  var
    x:prtcNode3_I64Str;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I64Str;
    Result:=vrtcInfoNIL_I64Str;
    end;
  end;

function tRtcTree_I64Str.Search_L(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x,y:prtcNode3_I64Str;
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
function tRtcTree_I64Str.iSearch_L(const v:trtcInfoType_I64Str; var i:trtcItemType_I64Str):trtcInfoType_I64Str;
  var
    x,y:prtcNode3_I64Str;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I64Str.Search_G(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x,y:prtcNode3_I64Str;
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
function tRtcTree_I64Str.iSearch_G(const v:trtcInfoType_I64Str; var i:trtcItemType_I64Str):trtcInfoType_I64Str;
  var
    x,y:prtcNode3_I64Str;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I64Str.Search_LE(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x,y:prtcNode3_I64Str;
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
function tRtcTree_I64Str.iSearch_LE(const v:trtcInfoType_I64Str; var i:trtcItemType_I64Str):trtcInfoType_I64Str;
  var
    x,y:prtcNode3_I64Str;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_I64Str.Search_GE(const v:trtcItemType_I64Str; var i:trtcInfoType_I64Str):trtcItemType_I64Str;
  var
    x,y:prtcNode3_I64Str;
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
function tRtcTree_I64Str.iSearch_GE(const v:trtcInfoType_I64Str; var i:trtcItemType_I64Str):trtcInfoType_I64Str;
  var
    x,y:prtcNode3_I64Str;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_I64Str.Insert_split;
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
procedure tRtcTree_I64Str.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_I64Str.Insert(const v:trtcItemType_I64Str;const info:trtcInfoType_I64Str);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_I64Str.Remove_AddParentNode(node:prtcNode3_I64Str);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_I64Str.Remove_GetParentNode:prtcNode3_I64Str;
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

procedure tRtcTree_I64Str.Remove(const v:trtcItemType_I64Str);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I64Str;
  t^.key:=vrtcItemMin_I64Str;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_I64Str.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_I64WStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_I64WStr,vrtcInfoNIL_I64WStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_I64WStr,vrtcInfoNIL_I64WStr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_I64WStr.Destroy;
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

function tRtcTree_I64WStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_I64WStr.New_Node(const k:trtcItemType_I64WStr; const i:trtcInfoType_I64WStr; const bi:boolean; const ll,rr:prtcNode3_I64WStr):prtcNode3_I64WStr;
  var
    p:prtcNodeArr3_I64WStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_I64WStr);
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
    GetMem(Result,SizeOf(trtcNode3_I64WStr));
  FillChar(Result^,SizeOf(trtcNode3_I64WStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_I64WStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_I64WStr.Del_Node(node:prtcNode3_I64WStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_I64WStr.Change(const v:trtcItemType_I64WStr;const info:trtcInfoType_I64WStr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_I64WStr.RemoveThis(var t:prtcNode3_I64WStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_I64WStr;
  t^.key:=vrtcItemMin_I64WStr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_I64WStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_I64WStr;
  head^.key:=vrtcItemMin_I64WStr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_I64WStr.Search(const v:trtcItemType_I64WStr):trtcInfoType_I64WStr;
  var
    x:prtcNode3_I64WStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_I64WStr.iSearch(const v:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x:prtcNode3_I64WStr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_I64WStr.Search_Min(var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x:prtcNode3_I64WStr;
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
function tRtcTree_I64WStr.iSearch_Min(var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;
  var
    x:prtcNode3_I64WStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I64WStr;
    Result:=vrtcInfoNIL_I64WStr;
    end;
  end;

function tRtcTree_I64WStr.Search_Max(var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x:prtcNode3_I64WStr;
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
function tRtcTree_I64WStr.iSearch_Max(var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;
  var
    x:prtcNode3_I64WStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_I64WStr;
    Result:=vrtcInfoNIL_I64WStr;
    end;
  end;

function tRtcTree_I64WStr.Search_L(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x,y:prtcNode3_I64WStr;
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
function tRtcTree_I64WStr.iSearch_L(const v:trtcInfoType_I64WStr; var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;
  var
    x,y:prtcNode3_I64WStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I64WStr.Search_G(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x,y:prtcNode3_I64WStr;
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
function tRtcTree_I64WStr.iSearch_G(const v:trtcInfoType_I64WStr; var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;
  var
    x,y:prtcNode3_I64WStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_I64WStr.Search_LE(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x,y:prtcNode3_I64WStr;
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
function tRtcTree_I64WStr.iSearch_LE(const v:trtcInfoType_I64WStr; var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;
  var
    x,y:prtcNode3_I64WStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_I64WStr.Search_GE(const v:trtcItemType_I64WStr; var i:trtcInfoType_I64WStr):trtcItemType_I64WStr;
  var
    x,y:prtcNode3_I64WStr;
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
function tRtcTree_I64WStr.iSearch_GE(const v:trtcInfoType_I64WStr; var i:trtcItemType_I64WStr):trtcInfoType_I64WStr;
  var
    x,y:prtcNode3_I64WStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_I64WStr.Insert_split;
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
procedure tRtcTree_I64WStr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_I64WStr.Insert(const v:trtcItemType_I64WStr;const info:trtcInfoType_I64WStr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_I64WStr.Remove_AddParentNode(node:prtcNode3_I64WStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_I64WStr.Remove_GetParentNode:prtcNode3_I64WStr;
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

procedure tRtcTree_I64WStr.Remove(const v:trtcItemType_I64WStr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_I64WStr;
  t^.key:=vrtcItemMin_I64WStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_I64WStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_IPtrIPtr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_IPtrIPtr,vrtcInfoNIL_IPtrIPtr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_IPtrIPtr,vrtcInfoNIL_IPtrIPtr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_IPtrIPtr.Destroy;
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

function tRtcTree_IPtrIPtr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_IPtrIPtr.New_Node(const k:trtcItemType_IPtrIPtr; const i:trtcInfoType_IPtrIPtr; const bi:boolean; const ll,rr:prtcNode3_IPtrIPtr):prtcNode3_IPtrIPtr;
  var
    p:prtcNodeArr3_IPtrIPtr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_IPtrIPtr);
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
    GetMem(Result,SizeOf(trtcNode3_IPtrIPtr));
  FillChar(Result^,SizeOf(trtcNode3_IPtrIPtr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_IPtrIPtr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_IPtrIPtr.Del_Node(node:prtcNode3_IPtrIPtr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_IPtrIPtr.Change(const v:trtcItemType_IPtrIPtr;const info:trtcInfoType_IPtrIPtr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_IPtrIPtr.RemoveThis(var t:prtcNode3_IPtrIPtr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_IPtrIPtr;
  t^.key:=vrtcItemMin_IPtrIPtr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_IPtrIPtr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_IPtrIPtr;
  head^.key:=vrtcItemMin_IPtrIPtr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_IPtrIPtr.Search(const v:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;
  var
    x:prtcNode3_IPtrIPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_IPtrIPtr.iSearch(const v:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x:prtcNode3_IPtrIPtr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_IPtrIPtr.Search_Min(var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x:prtcNode3_IPtrIPtr;
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
function tRtcTree_IPtrIPtr.iSearch_Min(var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;
  var
    x:prtcNode3_IPtrIPtr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_IPtrIPtr;
    Result:=vrtcInfoNIL_IPtrIPtr;
    end;
  end;

function tRtcTree_IPtrIPtr.Search_Max(var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x:prtcNode3_IPtrIPtr;
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
function tRtcTree_IPtrIPtr.iSearch_Max(var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;
  var
    x:prtcNode3_IPtrIPtr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_IPtrIPtr;
    Result:=vrtcInfoNIL_IPtrIPtr;
    end;
  end;

function tRtcTree_IPtrIPtr.Search_L(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x,y:prtcNode3_IPtrIPtr;
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
function tRtcTree_IPtrIPtr.iSearch_L(const v:trtcInfoType_IPtrIPtr; var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;
  var
    x,y:prtcNode3_IPtrIPtr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_IPtrIPtr.Search_G(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x,y:prtcNode3_IPtrIPtr;
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
function tRtcTree_IPtrIPtr.iSearch_G(const v:trtcInfoType_IPtrIPtr; var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;
  var
    x,y:prtcNode3_IPtrIPtr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_IPtrIPtr.Search_LE(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x,y:prtcNode3_IPtrIPtr;
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
function tRtcTree_IPtrIPtr.iSearch_LE(const v:trtcInfoType_IPtrIPtr; var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;
  var
    x,y:prtcNode3_IPtrIPtr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_IPtrIPtr.Search_GE(const v:trtcItemType_IPtrIPtr; var i:trtcInfoType_IPtrIPtr):trtcItemType_IPtrIPtr;
  var
    x,y:prtcNode3_IPtrIPtr;
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
function tRtcTree_IPtrIPtr.iSearch_GE(const v:trtcInfoType_IPtrIPtr; var i:trtcItemType_IPtrIPtr):trtcInfoType_IPtrIPtr;
  var
    x,y:prtcNode3_IPtrIPtr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_IPtrIPtr.Insert_split;
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
procedure tRtcTree_IPtrIPtr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_IPtrIPtr.Insert(const v:trtcItemType_IPtrIPtr;const info:trtcInfoType_IPtrIPtr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_IPtrIPtr.Remove_AddParentNode(node:prtcNode3_IPtrIPtr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_IPtrIPtr.Remove_GetParentNode:prtcNode3_IPtrIPtr;
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

procedure tRtcTree_IPtrIPtr.Remove(const v:trtcItemType_IPtrIPtr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_IPtrIPtr;
  t^.key:=vrtcItemMin_IPtrIPtr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_IPtrIPtr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_IPtrObj.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_IPtrObj,vrtcInfoNIL_IPtrObj,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_IPtrObj,vrtcInfoNIL_IPtrObj,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_IPtrObj.Destroy;
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

function tRtcTree_IPtrObj.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_IPtrObj.New_Node(const k:trtcItemType_IPtrObj; const i:trtcInfoType_IPtrObj; const bi:boolean; const ll,rr:prtcNode3_IPtrObj):prtcNode3_IPtrObj;
  var
    p:prtcNodeArr3_IPtrObj;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_IPtrObj);
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
    GetMem(Result,SizeOf(trtcNode3_IPtrObj));
  FillChar(Result^,SizeOf(trtcNode3_IPtrObj),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_IPtrObj.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_IPtrObj.Del_Node(node:prtcNode3_IPtrObj);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_IPtrObj.Change(const v:trtcItemType_IPtrObj;const info:trtcInfoType_IPtrObj);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_IPtrObj.RemoveThis(var t:prtcNode3_IPtrObj);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_IPtrObj;
  t^.key:=vrtcItemMin_IPtrObj;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_IPtrObj.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_IPtrObj;
  head^.key:=vrtcItemMin_IPtrObj;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_IPtrObj.Search(const v:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;
  var
    x:prtcNode3_IPtrObj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_IPtrObj.iSearch(const v:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x:prtcNode3_IPtrObj;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (trtcInfoComp_IPtrObj(v)<trtcInfoComp_IPtrObj(x^.info)) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_IPtrObj.Search_Min(var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x:prtcNode3_IPtrObj;
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
function tRtcTree_IPtrObj.iSearch_Min(var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;
  var
    x:prtcNode3_IPtrObj;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_IPtrObj;
    Result:=vrtcInfoNIL_IPtrObj;
    end;
  end;

function tRtcTree_IPtrObj.Search_Max(var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x:prtcNode3_IPtrObj;
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
function tRtcTree_IPtrObj.iSearch_Max(var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;
  var
    x:prtcNode3_IPtrObj;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_IPtrObj;
    Result:=vrtcInfoNIL_IPtrObj;
    end;
  end;

function tRtcTree_IPtrObj.Search_L(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x,y:prtcNode3_IPtrObj;
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
function tRtcTree_IPtrObj.iSearch_L(const v:trtcInfoType_IPtrObj; var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;
  var
    x,y:prtcNode3_IPtrObj;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_IPtrObj(x^.info)<trtcInfoComp_IPtrObj(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_IPtrObj.Search_G(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x,y:prtcNode3_IPtrObj;
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
function tRtcTree_IPtrObj.iSearch_G(const v:trtcInfoType_IPtrObj; var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;
  var
    x,y:prtcNode3_IPtrObj;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_IPtrObj(x^.info)>trtcInfoComp_IPtrObj(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_IPtrObj.Search_LE(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x,y:prtcNode3_IPtrObj;
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
function tRtcTree_IPtrObj.iSearch_LE(const v:trtcInfoType_IPtrObj; var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;
  var
    x,y:prtcNode3_IPtrObj;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_IPtrObj(x^.info)<trtcInfoComp_IPtrObj(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_IPtrObj.Search_GE(const v:trtcItemType_IPtrObj; var i:trtcInfoType_IPtrObj):trtcItemType_IPtrObj;
  var
    x,y:prtcNode3_IPtrObj;
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
function tRtcTree_IPtrObj.iSearch_GE(const v:trtcInfoType_IPtrObj; var i:trtcItemType_IPtrObj):trtcInfoType_IPtrObj;
  var
    x,y:prtcNode3_IPtrObj;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_IPtrObj(x^.info)>trtcInfoComp_IPtrObj(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_IPtrObj.Insert_split;
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
procedure tRtcTree_IPtrObj.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (trtcInfoComp_IPtrObj(xv2)<trtcInfoComp_IPtrObj(g^.info)) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (trtcInfoComp_IPtrObj(xv2)<trtcInfoComp_IPtrObj(p^.info)) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (trtcInfoComp_IPtrObj(xv2)<trtcInfoComp_IPtrObj(g^.info)) then c:=g^.l2
      else if (trtcInfoComp_IPtrObj(xv2)>trtcInfoComp_IPtrObj(g^.info)) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (trtcInfoComp_IPtrObj(xv2)<trtcInfoComp_IPtrObj(c^.info)) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (trtcInfoComp_IPtrObj(xv2)>trtcInfoComp_IPtrObj(c^.info)) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (trtcInfoComp_IPtrObj(xv2)<trtcInfoComp_IPtrObj(g^.info)) then g^.l2:=p
      else if (trtcInfoComp_IPtrObj(xv2)>trtcInfoComp_IPtrObj(g^.info)) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (trtcInfoComp_IPtrObj(xv2)<trtcInfoComp_IPtrObj(gg^.info)) then c:=gg^.l2
    else if (trtcInfoComp_IPtrObj(xv2)>trtcInfoComp_IPtrObj(gg^.info)) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (trtcInfoComp_IPtrObj(xv2)<trtcInfoComp_IPtrObj(c^.info)) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (trtcInfoComp_IPtrObj(xv2)>trtcInfoComp_IPtrObj(c^.info)) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (trtcInfoComp_IPtrObj(xv2)<trtcInfoComp_IPtrObj(gg^.info)) then gg^.l2:=x
    else if (trtcInfoComp_IPtrObj(xv2)>trtcInfoComp_IPtrObj(gg^.info)) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_IPtrObj.Insert(const v:trtcItemType_IPtrObj;const info:trtcInfoType_IPtrObj);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (trtcInfoComp_IPtrObj(xv2)<trtcInfoComp_IPtrObj(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_IPtrObj(xv2)>trtcInfoComp_IPtrObj(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (trtcInfoComp_IPtrObj(xv2)<trtcInfoComp_IPtrObj(p^.info)) then p^.l2:=x
  else if (trtcInfoComp_IPtrObj(xv2)>trtcInfoComp_IPtrObj(p^.info)) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_IPtrObj.Remove_AddParentNode(node:prtcNode3_IPtrObj);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_IPtrObj.Remove_GetParentNode:prtcNode3_IPtrObj;
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

procedure tRtcTree_IPtrObj.Remove(const v:trtcItemType_IPtrObj);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (trtcInfoComp_IPtrObj(xv2)<trtcInfoComp_IPtrObj(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_IPtrObj(xv2)>trtcInfoComp_IPtrObj(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_IPtrObj;
  t^.key:=vrtcItemMin_IPtrObj;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_IPtrObj.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_IPtrPtr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_IPtrPtr,vrtcInfoNIL_IPtrPtr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_IPtrPtr,vrtcInfoNIL_IPtrPtr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_IPtrPtr.Destroy;
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

function tRtcTree_IPtrPtr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_IPtrPtr.New_Node(const k:trtcItemType_IPtrPtr; const i:trtcInfoType_IPtrPtr; const bi:boolean; const ll,rr:prtcNode3_IPtrPtr):prtcNode3_IPtrPtr;
  var
    p:prtcNodeArr3_IPtrPtr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_IPtrPtr);
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
    GetMem(Result,SizeOf(trtcNode3_IPtrPtr));
  FillChar(Result^,SizeOf(trtcNode3_IPtrPtr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_IPtrPtr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_IPtrPtr.Del_Node(node:prtcNode3_IPtrPtr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_IPtrPtr.Change(const v:trtcItemType_IPtrPtr;const info:trtcInfoType_IPtrPtr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_IPtrPtr.RemoveThis(var t:prtcNode3_IPtrPtr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_IPtrPtr;
  t^.key:=vrtcItemMin_IPtrPtr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_IPtrPtr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_IPtrPtr;
  head^.key:=vrtcItemMin_IPtrPtr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_IPtrPtr.Search(const v:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;
  var
    x:prtcNode3_IPtrPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_IPtrPtr.iSearch(const v:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x:prtcNode3_IPtrPtr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (trtcInfoComp_IPtrPtr(v)<trtcInfoComp_IPtrPtr(x^.info)) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_IPtrPtr.Search_Min(var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x:prtcNode3_IPtrPtr;
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
function tRtcTree_IPtrPtr.iSearch_Min(var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;
  var
    x:prtcNode3_IPtrPtr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_IPtrPtr;
    Result:=vrtcInfoNIL_IPtrPtr;
    end;
  end;

function tRtcTree_IPtrPtr.Search_Max(var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x:prtcNode3_IPtrPtr;
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
function tRtcTree_IPtrPtr.iSearch_Max(var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;
  var
    x:prtcNode3_IPtrPtr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_IPtrPtr;
    Result:=vrtcInfoNIL_IPtrPtr;
    end;
  end;

function tRtcTree_IPtrPtr.Search_L(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x,y:prtcNode3_IPtrPtr;
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
function tRtcTree_IPtrPtr.iSearch_L(const v:trtcInfoType_IPtrPtr; var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;
  var
    x,y:prtcNode3_IPtrPtr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_IPtrPtr(x^.info)<trtcInfoComp_IPtrPtr(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_IPtrPtr.Search_G(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x,y:prtcNode3_IPtrPtr;
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
function tRtcTree_IPtrPtr.iSearch_G(const v:trtcInfoType_IPtrPtr; var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;
  var
    x,y:prtcNode3_IPtrPtr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_IPtrPtr(x^.info)>trtcInfoComp_IPtrPtr(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_IPtrPtr.Search_LE(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x,y:prtcNode3_IPtrPtr;
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
function tRtcTree_IPtrPtr.iSearch_LE(const v:trtcInfoType_IPtrPtr; var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;
  var
    x,y:prtcNode3_IPtrPtr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_IPtrPtr(x^.info)<trtcInfoComp_IPtrPtr(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_IPtrPtr.Search_GE(const v:trtcItemType_IPtrPtr; var i:trtcInfoType_IPtrPtr):trtcItemType_IPtrPtr;
  var
    x,y:prtcNode3_IPtrPtr;
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
function tRtcTree_IPtrPtr.iSearch_GE(const v:trtcInfoType_IPtrPtr; var i:trtcItemType_IPtrPtr):trtcInfoType_IPtrPtr;
  var
    x,y:prtcNode3_IPtrPtr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_IPtrPtr(x^.info)>trtcInfoComp_IPtrPtr(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_IPtrPtr.Insert_split;
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
procedure tRtcTree_IPtrPtr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (trtcInfoComp_IPtrPtr(xv2)<trtcInfoComp_IPtrPtr(g^.info)) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (trtcInfoComp_IPtrPtr(xv2)<trtcInfoComp_IPtrPtr(p^.info)) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (trtcInfoComp_IPtrPtr(xv2)<trtcInfoComp_IPtrPtr(g^.info)) then c:=g^.l2
      else if (trtcInfoComp_IPtrPtr(xv2)>trtcInfoComp_IPtrPtr(g^.info)) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (trtcInfoComp_IPtrPtr(xv2)<trtcInfoComp_IPtrPtr(c^.info)) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (trtcInfoComp_IPtrPtr(xv2)>trtcInfoComp_IPtrPtr(c^.info)) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (trtcInfoComp_IPtrPtr(xv2)<trtcInfoComp_IPtrPtr(g^.info)) then g^.l2:=p
      else if (trtcInfoComp_IPtrPtr(xv2)>trtcInfoComp_IPtrPtr(g^.info)) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (trtcInfoComp_IPtrPtr(xv2)<trtcInfoComp_IPtrPtr(gg^.info)) then c:=gg^.l2
    else if (trtcInfoComp_IPtrPtr(xv2)>trtcInfoComp_IPtrPtr(gg^.info)) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (trtcInfoComp_IPtrPtr(xv2)<trtcInfoComp_IPtrPtr(c^.info)) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (trtcInfoComp_IPtrPtr(xv2)>trtcInfoComp_IPtrPtr(c^.info)) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (trtcInfoComp_IPtrPtr(xv2)<trtcInfoComp_IPtrPtr(gg^.info)) then gg^.l2:=x
    else if (trtcInfoComp_IPtrPtr(xv2)>trtcInfoComp_IPtrPtr(gg^.info)) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_IPtrPtr.Insert(const v:trtcItemType_IPtrPtr;const info:trtcInfoType_IPtrPtr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (trtcInfoComp_IPtrPtr(xv2)<trtcInfoComp_IPtrPtr(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_IPtrPtr(xv2)>trtcInfoComp_IPtrPtr(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (trtcInfoComp_IPtrPtr(xv2)<trtcInfoComp_IPtrPtr(p^.info)) then p^.l2:=x
  else if (trtcInfoComp_IPtrPtr(xv2)>trtcInfoComp_IPtrPtr(p^.info)) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_IPtrPtr.Remove_AddParentNode(node:prtcNode3_IPtrPtr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_IPtrPtr.Remove_GetParentNode:prtcNode3_IPtrPtr;
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

procedure tRtcTree_IPtrPtr.Remove(const v:trtcItemType_IPtrPtr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (trtcInfoComp_IPtrPtr(xv2)<trtcInfoComp_IPtrPtr(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_IPtrPtr(xv2)>trtcInfoComp_IPtrPtr(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_IPtrPtr;
  t^.key:=vrtcItemMin_IPtrPtr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_IPtrPtr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_IPtrStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_IPtrStr,vrtcInfoNIL_IPtrStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_IPtrStr,vrtcInfoNIL_IPtrStr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_IPtrStr.Destroy;
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

function tRtcTree_IPtrStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_IPtrStr.New_Node(const k:trtcItemType_IPtrStr; const i:trtcInfoType_IPtrStr; const bi:boolean; const ll,rr:prtcNode3_IPtrStr):prtcNode3_IPtrStr;
  var
    p:prtcNodeArr3_IPtrStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_IPtrStr);
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
    GetMem(Result,SizeOf(trtcNode3_IPtrStr));
  FillChar(Result^,SizeOf(trtcNode3_IPtrStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_IPtrStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_IPtrStr.Del_Node(node:prtcNode3_IPtrStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_IPtrStr.Change(const v:trtcItemType_IPtrStr;const info:trtcInfoType_IPtrStr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_IPtrStr.RemoveThis(var t:prtcNode3_IPtrStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_IPtrStr;
  t^.key:=vrtcItemMin_IPtrStr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_IPtrStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_IPtrStr;
  head^.key:=vrtcItemMin_IPtrStr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_IPtrStr.Search(const v:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;
  var
    x:prtcNode3_IPtrStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_IPtrStr.iSearch(const v:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x:prtcNode3_IPtrStr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_IPtrStr.Search_Min(var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x:prtcNode3_IPtrStr;
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
function tRtcTree_IPtrStr.iSearch_Min(var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;
  var
    x:prtcNode3_IPtrStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_IPtrStr;
    Result:=vrtcInfoNIL_IPtrStr;
    end;
  end;

function tRtcTree_IPtrStr.Search_Max(var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x:prtcNode3_IPtrStr;
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
function tRtcTree_IPtrStr.iSearch_Max(var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;
  var
    x:prtcNode3_IPtrStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_IPtrStr;
    Result:=vrtcInfoNIL_IPtrStr;
    end;
  end;

function tRtcTree_IPtrStr.Search_L(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x,y:prtcNode3_IPtrStr;
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
function tRtcTree_IPtrStr.iSearch_L(const v:trtcInfoType_IPtrStr; var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;
  var
    x,y:prtcNode3_IPtrStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_IPtrStr.Search_G(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x,y:prtcNode3_IPtrStr;
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
function tRtcTree_IPtrStr.iSearch_G(const v:trtcInfoType_IPtrStr; var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;
  var
    x,y:prtcNode3_IPtrStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_IPtrStr.Search_LE(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x,y:prtcNode3_IPtrStr;
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
function tRtcTree_IPtrStr.iSearch_LE(const v:trtcInfoType_IPtrStr; var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;
  var
    x,y:prtcNode3_IPtrStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_IPtrStr.Search_GE(const v:trtcItemType_IPtrStr; var i:trtcInfoType_IPtrStr):trtcItemType_IPtrStr;
  var
    x,y:prtcNode3_IPtrStr;
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
function tRtcTree_IPtrStr.iSearch_GE(const v:trtcInfoType_IPtrStr; var i:trtcItemType_IPtrStr):trtcInfoType_IPtrStr;
  var
    x,y:prtcNode3_IPtrStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_IPtrStr.Insert_split;
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
procedure tRtcTree_IPtrStr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_IPtrStr.Insert(const v:trtcItemType_IPtrStr;const info:trtcInfoType_IPtrStr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_IPtrStr.Remove_AddParentNode(node:prtcNode3_IPtrStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_IPtrStr.Remove_GetParentNode:prtcNode3_IPtrStr;
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

procedure tRtcTree_IPtrStr.Remove(const v:trtcItemType_IPtrStr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_IPtrStr;
  t^.key:=vrtcItemMin_IPtrStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_IPtrStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_IPtrWStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_IPtrWStr,vrtcInfoNIL_IPtrWStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_IPtrWStr,vrtcInfoNIL_IPtrWStr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_IPtrWStr.Destroy;
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

function tRtcTree_IPtrWStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_IPtrWStr.New_Node(const k:trtcItemType_IPtrWStr; const i:trtcInfoType_IPtrWStr; const bi:boolean; const ll,rr:prtcNode3_IPtrWStr):prtcNode3_IPtrWStr;
  var
    p:prtcNodeArr3_IPtrWStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_IPtrWStr);
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
    GetMem(Result,SizeOf(trtcNode3_IPtrWStr));
  FillChar(Result^,SizeOf(trtcNode3_IPtrWStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_IPtrWStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_IPtrWStr.Del_Node(node:prtcNode3_IPtrWStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_IPtrWStr.Change(const v:trtcItemType_IPtrWStr;const info:trtcInfoType_IPtrWStr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_IPtrWStr.RemoveThis(var t:prtcNode3_IPtrWStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_IPtrWStr;
  t^.key:=vrtcItemMin_IPtrWStr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_IPtrWStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_IPtrWStr;
  head^.key:=vrtcItemMin_IPtrWStr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_IPtrWStr.Search(const v:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;
  var
    x:prtcNode3_IPtrWStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_IPtrWStr.iSearch(const v:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x:prtcNode3_IPtrWStr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_IPtrWStr.Search_Min(var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x:prtcNode3_IPtrWStr;
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
function tRtcTree_IPtrWStr.iSearch_Min(var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;
  var
    x:prtcNode3_IPtrWStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_IPtrWStr;
    Result:=vrtcInfoNIL_IPtrWStr;
    end;
  end;

function tRtcTree_IPtrWStr.Search_Max(var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x:prtcNode3_IPtrWStr;
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
function tRtcTree_IPtrWStr.iSearch_Max(var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;
  var
    x:prtcNode3_IPtrWStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_IPtrWStr;
    Result:=vrtcInfoNIL_IPtrWStr;
    end;
  end;

function tRtcTree_IPtrWStr.Search_L(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x,y:prtcNode3_IPtrWStr;
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
function tRtcTree_IPtrWStr.iSearch_L(const v:trtcInfoType_IPtrWStr; var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;
  var
    x,y:prtcNode3_IPtrWStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_IPtrWStr.Search_G(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x,y:prtcNode3_IPtrWStr;
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
function tRtcTree_IPtrWStr.iSearch_G(const v:trtcInfoType_IPtrWStr; var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;
  var
    x,y:prtcNode3_IPtrWStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_IPtrWStr.Search_LE(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x,y:prtcNode3_IPtrWStr;
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
function tRtcTree_IPtrWStr.iSearch_LE(const v:trtcInfoType_IPtrWStr; var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;
  var
    x,y:prtcNode3_IPtrWStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_IPtrWStr.Search_GE(const v:trtcItemType_IPtrWStr; var i:trtcInfoType_IPtrWStr):trtcItemType_IPtrWStr;
  var
    x,y:prtcNode3_IPtrWStr;
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
function tRtcTree_IPtrWStr.iSearch_GE(const v:trtcInfoType_IPtrWStr; var i:trtcItemType_IPtrWStr):trtcInfoType_IPtrWStr;
  var
    x,y:prtcNode3_IPtrWStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_IPtrWStr.Insert_split;
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
procedure tRtcTree_IPtrWStr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_IPtrWStr.Insert(const v:trtcItemType_IPtrWStr;const info:trtcInfoType_IPtrWStr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_IPtrWStr.Remove_AddParentNode(node:prtcNode3_IPtrWStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_IPtrWStr.Remove_GetParentNode:prtcNode3_IPtrWStr;
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

procedure tRtcTree_IPtrWStr.Remove(const v:trtcItemType_IPtrWStr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_IPtrWStr;
  t^.key:=vrtcItemMin_IPtrWStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_IPtrWStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_StrI32.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_StrI32,vrtcInfoNIL_StrI32,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_StrI32,vrtcInfoNIL_StrI32,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_StrI32.Destroy;
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

function tRtcTree_StrI32.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_StrI32.New_Node(const k:trtcItemType_StrI32; const i:trtcInfoType_StrI32; const bi:boolean; const ll,rr:prtcNode3_StrI32):prtcNode3_StrI32;
  var
    p:prtcNodeArr3_StrI32;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_StrI32);
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
    GetMem(Result,SizeOf(trtcNode3_StrI32));
  FillChar(Result^,SizeOf(trtcNode3_StrI32),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_StrI32.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_StrI32.Del_Node(node:prtcNode3_StrI32);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_StrI32.Change(const v:trtcItemType_StrI32;const info:trtcInfoType_StrI32);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_StrI32.RemoveThis(var t:prtcNode3_StrI32);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_StrI32;
  t^.key:=vrtcItemMin_StrI32;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_StrI32.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_StrI32;
  head^.key:=vrtcItemMin_StrI32;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_StrI32.Search(const v:trtcItemType_StrI32):trtcInfoType_StrI32;
  var
    x:prtcNode3_StrI32;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_StrI32.iSearch(const v:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x:prtcNode3_StrI32;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_StrI32.Search_Min(var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x:prtcNode3_StrI32;
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
function tRtcTree_StrI32.iSearch_Min(var i:trtcItemType_StrI32):trtcInfoType_StrI32;
  var
    x:prtcNode3_StrI32;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_StrI32;
    Result:=vrtcInfoNIL_StrI32;
    end;
  end;

function tRtcTree_StrI32.Search_Max(var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x:prtcNode3_StrI32;
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
function tRtcTree_StrI32.iSearch_Max(var i:trtcItemType_StrI32):trtcInfoType_StrI32;
  var
    x:prtcNode3_StrI32;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_StrI32;
    Result:=vrtcInfoNIL_StrI32;
    end;
  end;

function tRtcTree_StrI32.Search_L(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x,y:prtcNode3_StrI32;
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
function tRtcTree_StrI32.iSearch_L(const v:trtcInfoType_StrI32; var i:trtcItemType_StrI32):trtcInfoType_StrI32;
  var
    x,y:prtcNode3_StrI32;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_StrI32.Search_G(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x,y:prtcNode3_StrI32;
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
function tRtcTree_StrI32.iSearch_G(const v:trtcInfoType_StrI32; var i:trtcItemType_StrI32):trtcInfoType_StrI32;
  var
    x,y:prtcNode3_StrI32;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_StrI32.Search_LE(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x,y:prtcNode3_StrI32;
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
function tRtcTree_StrI32.iSearch_LE(const v:trtcInfoType_StrI32; var i:trtcItemType_StrI32):trtcInfoType_StrI32;
  var
    x,y:prtcNode3_StrI32;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_StrI32.Search_GE(const v:trtcItemType_StrI32; var i:trtcInfoType_StrI32):trtcItemType_StrI32;
  var
    x,y:prtcNode3_StrI32;
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
function tRtcTree_StrI32.iSearch_GE(const v:trtcInfoType_StrI32; var i:trtcItemType_StrI32):trtcInfoType_StrI32;
  var
    x,y:prtcNode3_StrI32;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_StrI32.Insert_split;
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
procedure tRtcTree_StrI32.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_StrI32.Insert(const v:trtcItemType_StrI32;const info:trtcInfoType_StrI32);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_StrI32.Remove_AddParentNode(node:prtcNode3_StrI32);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_StrI32.Remove_GetParentNode:prtcNode3_StrI32;
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

procedure tRtcTree_StrI32.Remove(const v:trtcItemType_StrI32);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_StrI32;
  t^.key:=vrtcItemMin_StrI32;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_StrI32.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_StrI64.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_StrI64,vrtcInfoNIL_StrI64,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_StrI64,vrtcInfoNIL_StrI64,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_StrI64.Destroy;
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

function tRtcTree_StrI64.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_StrI64.New_Node(const k:trtcItemType_StrI64; const i:trtcInfoType_StrI64; const bi:boolean; const ll,rr:prtcNode3_StrI64):prtcNode3_StrI64;
  var
    p:prtcNodeArr3_StrI64;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_StrI64);
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
    GetMem(Result,SizeOf(trtcNode3_StrI64));
  FillChar(Result^,SizeOf(trtcNode3_StrI64),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_StrI64.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_StrI64.Del_Node(node:prtcNode3_StrI64);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_StrI64.Change(const v:trtcItemType_StrI64;const info:trtcInfoType_StrI64);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_StrI64.RemoveThis(var t:prtcNode3_StrI64);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_StrI64;
  t^.key:=vrtcItemMin_StrI64;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_StrI64.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_StrI64;
  head^.key:=vrtcItemMin_StrI64;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_StrI64.Search(const v:trtcItemType_StrI64):trtcInfoType_StrI64;
  var
    x:prtcNode3_StrI64;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_StrI64.iSearch(const v:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x:prtcNode3_StrI64;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_StrI64.Search_Min(var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x:prtcNode3_StrI64;
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
function tRtcTree_StrI64.iSearch_Min(var i:trtcItemType_StrI64):trtcInfoType_StrI64;
  var
    x:prtcNode3_StrI64;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_StrI64;
    Result:=vrtcInfoNIL_StrI64;
    end;
  end;

function tRtcTree_StrI64.Search_Max(var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x:prtcNode3_StrI64;
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
function tRtcTree_StrI64.iSearch_Max(var i:trtcItemType_StrI64):trtcInfoType_StrI64;
  var
    x:prtcNode3_StrI64;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_StrI64;
    Result:=vrtcInfoNIL_StrI64;
    end;
  end;

function tRtcTree_StrI64.Search_L(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x,y:prtcNode3_StrI64;
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
function tRtcTree_StrI64.iSearch_L(const v:trtcInfoType_StrI64; var i:trtcItemType_StrI64):trtcInfoType_StrI64;
  var
    x,y:prtcNode3_StrI64;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_StrI64.Search_G(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x,y:prtcNode3_StrI64;
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
function tRtcTree_StrI64.iSearch_G(const v:trtcInfoType_StrI64; var i:trtcItemType_StrI64):trtcInfoType_StrI64;
  var
    x,y:prtcNode3_StrI64;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_StrI64.Search_LE(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x,y:prtcNode3_StrI64;
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
function tRtcTree_StrI64.iSearch_LE(const v:trtcInfoType_StrI64; var i:trtcItemType_StrI64):trtcInfoType_StrI64;
  var
    x,y:prtcNode3_StrI64;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_StrI64.Search_GE(const v:trtcItemType_StrI64; var i:trtcInfoType_StrI64):trtcItemType_StrI64;
  var
    x,y:prtcNode3_StrI64;
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
function tRtcTree_StrI64.iSearch_GE(const v:trtcInfoType_StrI64; var i:trtcItemType_StrI64):trtcInfoType_StrI64;
  var
    x,y:prtcNode3_StrI64;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_StrI64.Insert_split;
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
procedure tRtcTree_StrI64.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_StrI64.Insert(const v:trtcItemType_StrI64;const info:trtcInfoType_StrI64);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_StrI64.Remove_AddParentNode(node:prtcNode3_StrI64);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_StrI64.Remove_GetParentNode:prtcNode3_StrI64;
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

procedure tRtcTree_StrI64.Remove(const v:trtcItemType_StrI64);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_StrI64;
  t^.key:=vrtcItemMin_StrI64;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_StrI64.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_StrObj.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_StrObj,vrtcInfoNIL_StrObj,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_StrObj,vrtcInfoNIL_StrObj,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_StrObj.Destroy;
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

function tRtcTree_StrObj.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_StrObj.New_Node(const k:trtcItemType_StrObj; const i:trtcInfoType_StrObj; const bi:boolean; const ll,rr:prtcNode3_StrObj):prtcNode3_StrObj;
  var
    p:prtcNodeArr3_StrObj;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_StrObj);
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
    GetMem(Result,SizeOf(trtcNode3_StrObj));
  FillChar(Result^,SizeOf(trtcNode3_StrObj),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_StrObj.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_StrObj.Del_Node(node:prtcNode3_StrObj);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_StrObj.Change(const v:trtcItemType_StrObj;const info:trtcInfoType_StrObj);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_StrObj.RemoveThis(var t:prtcNode3_StrObj);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_StrObj;
  t^.key:=vrtcItemMin_StrObj;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_StrObj.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_StrObj;
  head^.key:=vrtcItemMin_StrObj;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_StrObj.Search(const v:trtcItemType_StrObj):trtcInfoType_StrObj;
  var
    x:prtcNode3_StrObj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_StrObj.iSearch(const v:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x:prtcNode3_StrObj;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (trtcInfoComp_StrObj(v)<trtcInfoComp_StrObj(x^.info)) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_StrObj.Search_Min(var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x:prtcNode3_StrObj;
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
function tRtcTree_StrObj.iSearch_Min(var i:trtcItemType_StrObj):trtcInfoType_StrObj;
  var
    x:prtcNode3_StrObj;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_StrObj;
    Result:=vrtcInfoNIL_StrObj;
    end;
  end;

function tRtcTree_StrObj.Search_Max(var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x:prtcNode3_StrObj;
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
function tRtcTree_StrObj.iSearch_Max(var i:trtcItemType_StrObj):trtcInfoType_StrObj;
  var
    x:prtcNode3_StrObj;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_StrObj;
    Result:=vrtcInfoNIL_StrObj;
    end;
  end;

function tRtcTree_StrObj.Search_L(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x,y:prtcNode3_StrObj;
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
function tRtcTree_StrObj.iSearch_L(const v:trtcInfoType_StrObj; var i:trtcItemType_StrObj):trtcInfoType_StrObj;
  var
    x,y:prtcNode3_StrObj;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_StrObj(x^.info)<trtcInfoComp_StrObj(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_StrObj.Search_G(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x,y:prtcNode3_StrObj;
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
function tRtcTree_StrObj.iSearch_G(const v:trtcInfoType_StrObj; var i:trtcItemType_StrObj):trtcInfoType_StrObj;
  var
    x,y:prtcNode3_StrObj;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_StrObj(x^.info)>trtcInfoComp_StrObj(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_StrObj.Search_LE(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x,y:prtcNode3_StrObj;
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
function tRtcTree_StrObj.iSearch_LE(const v:trtcInfoType_StrObj; var i:trtcItemType_StrObj):trtcInfoType_StrObj;
  var
    x,y:prtcNode3_StrObj;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_StrObj(x^.info)<trtcInfoComp_StrObj(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_StrObj.Search_GE(const v:trtcItemType_StrObj; var i:trtcInfoType_StrObj):trtcItemType_StrObj;
  var
    x,y:prtcNode3_StrObj;
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
function tRtcTree_StrObj.iSearch_GE(const v:trtcInfoType_StrObj; var i:trtcItemType_StrObj):trtcInfoType_StrObj;
  var
    x,y:prtcNode3_StrObj;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_StrObj(x^.info)>trtcInfoComp_StrObj(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_StrObj.Insert_split;
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
procedure tRtcTree_StrObj.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (trtcInfoComp_StrObj(xv2)<trtcInfoComp_StrObj(g^.info)) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (trtcInfoComp_StrObj(xv2)<trtcInfoComp_StrObj(p^.info)) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (trtcInfoComp_StrObj(xv2)<trtcInfoComp_StrObj(g^.info)) then c:=g^.l2
      else if (trtcInfoComp_StrObj(xv2)>trtcInfoComp_StrObj(g^.info)) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (trtcInfoComp_StrObj(xv2)<trtcInfoComp_StrObj(c^.info)) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (trtcInfoComp_StrObj(xv2)>trtcInfoComp_StrObj(c^.info)) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (trtcInfoComp_StrObj(xv2)<trtcInfoComp_StrObj(g^.info)) then g^.l2:=p
      else if (trtcInfoComp_StrObj(xv2)>trtcInfoComp_StrObj(g^.info)) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (trtcInfoComp_StrObj(xv2)<trtcInfoComp_StrObj(gg^.info)) then c:=gg^.l2
    else if (trtcInfoComp_StrObj(xv2)>trtcInfoComp_StrObj(gg^.info)) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (trtcInfoComp_StrObj(xv2)<trtcInfoComp_StrObj(c^.info)) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (trtcInfoComp_StrObj(xv2)>trtcInfoComp_StrObj(c^.info)) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (trtcInfoComp_StrObj(xv2)<trtcInfoComp_StrObj(gg^.info)) then gg^.l2:=x
    else if (trtcInfoComp_StrObj(xv2)>trtcInfoComp_StrObj(gg^.info)) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_StrObj.Insert(const v:trtcItemType_StrObj;const info:trtcInfoType_StrObj);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (trtcInfoComp_StrObj(xv2)<trtcInfoComp_StrObj(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_StrObj(xv2)>trtcInfoComp_StrObj(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (trtcInfoComp_StrObj(xv2)<trtcInfoComp_StrObj(p^.info)) then p^.l2:=x
  else if (trtcInfoComp_StrObj(xv2)>trtcInfoComp_StrObj(p^.info)) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_StrObj.Remove_AddParentNode(node:prtcNode3_StrObj);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_StrObj.Remove_GetParentNode:prtcNode3_StrObj;
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

procedure tRtcTree_StrObj.Remove(const v:trtcItemType_StrObj);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (trtcInfoComp_StrObj(xv2)<trtcInfoComp_StrObj(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_StrObj(xv2)>trtcInfoComp_StrObj(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_StrObj;
  t^.key:=vrtcItemMin_StrObj;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_StrObj.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_StrPtr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_StrPtr,vrtcInfoNIL_StrPtr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_StrPtr,vrtcInfoNIL_StrPtr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_StrPtr.Destroy;
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

function tRtcTree_StrPtr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_StrPtr.New_Node(const k:trtcItemType_StrPtr; const i:trtcInfoType_StrPtr; const bi:boolean; const ll,rr:prtcNode3_StrPtr):prtcNode3_StrPtr;
  var
    p:prtcNodeArr3_StrPtr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_StrPtr);
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
    GetMem(Result,SizeOf(trtcNode3_StrPtr));
  FillChar(Result^,SizeOf(trtcNode3_StrPtr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_StrPtr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_StrPtr.Del_Node(node:prtcNode3_StrPtr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_StrPtr.Change(const v:trtcItemType_StrPtr;const info:trtcInfoType_StrPtr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_StrPtr.RemoveThis(var t:prtcNode3_StrPtr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_StrPtr;
  t^.key:=vrtcItemMin_StrPtr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_StrPtr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_StrPtr;
  head^.key:=vrtcItemMin_StrPtr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_StrPtr.Search(const v:trtcItemType_StrPtr):trtcInfoType_StrPtr;
  var
    x:prtcNode3_StrPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_StrPtr.iSearch(const v:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x:prtcNode3_StrPtr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (trtcInfoComp_StrPtr(v)<trtcInfoComp_StrPtr(x^.info)) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_StrPtr.Search_Min(var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x:prtcNode3_StrPtr;
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
function tRtcTree_StrPtr.iSearch_Min(var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;
  var
    x:prtcNode3_StrPtr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_StrPtr;
    Result:=vrtcInfoNIL_StrPtr;
    end;
  end;

function tRtcTree_StrPtr.Search_Max(var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x:prtcNode3_StrPtr;
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
function tRtcTree_StrPtr.iSearch_Max(var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;
  var
    x:prtcNode3_StrPtr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_StrPtr;
    Result:=vrtcInfoNIL_StrPtr;
    end;
  end;

function tRtcTree_StrPtr.Search_L(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x,y:prtcNode3_StrPtr;
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
function tRtcTree_StrPtr.iSearch_L(const v:trtcInfoType_StrPtr; var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;
  var
    x,y:prtcNode3_StrPtr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_StrPtr(x^.info)<trtcInfoComp_StrPtr(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_StrPtr.Search_G(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x,y:prtcNode3_StrPtr;
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
function tRtcTree_StrPtr.iSearch_G(const v:trtcInfoType_StrPtr; var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;
  var
    x,y:prtcNode3_StrPtr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_StrPtr(x^.info)>trtcInfoComp_StrPtr(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_StrPtr.Search_LE(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x,y:prtcNode3_StrPtr;
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
function tRtcTree_StrPtr.iSearch_LE(const v:trtcInfoType_StrPtr; var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;
  var
    x,y:prtcNode3_StrPtr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_StrPtr(x^.info)<trtcInfoComp_StrPtr(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_StrPtr.Search_GE(const v:trtcItemType_StrPtr; var i:trtcInfoType_StrPtr):trtcItemType_StrPtr;
  var
    x,y:prtcNode3_StrPtr;
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
function tRtcTree_StrPtr.iSearch_GE(const v:trtcInfoType_StrPtr; var i:trtcItemType_StrPtr):trtcInfoType_StrPtr;
  var
    x,y:prtcNode3_StrPtr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_StrPtr(x^.info)>trtcInfoComp_StrPtr(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_StrPtr.Insert_split;
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
procedure tRtcTree_StrPtr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (trtcInfoComp_StrPtr(xv2)<trtcInfoComp_StrPtr(g^.info)) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (trtcInfoComp_StrPtr(xv2)<trtcInfoComp_StrPtr(p^.info)) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (trtcInfoComp_StrPtr(xv2)<trtcInfoComp_StrPtr(g^.info)) then c:=g^.l2
      else if (trtcInfoComp_StrPtr(xv2)>trtcInfoComp_StrPtr(g^.info)) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (trtcInfoComp_StrPtr(xv2)<trtcInfoComp_StrPtr(c^.info)) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (trtcInfoComp_StrPtr(xv2)>trtcInfoComp_StrPtr(c^.info)) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (trtcInfoComp_StrPtr(xv2)<trtcInfoComp_StrPtr(g^.info)) then g^.l2:=p
      else if (trtcInfoComp_StrPtr(xv2)>trtcInfoComp_StrPtr(g^.info)) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (trtcInfoComp_StrPtr(xv2)<trtcInfoComp_StrPtr(gg^.info)) then c:=gg^.l2
    else if (trtcInfoComp_StrPtr(xv2)>trtcInfoComp_StrPtr(gg^.info)) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (trtcInfoComp_StrPtr(xv2)<trtcInfoComp_StrPtr(c^.info)) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (trtcInfoComp_StrPtr(xv2)>trtcInfoComp_StrPtr(c^.info)) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (trtcInfoComp_StrPtr(xv2)<trtcInfoComp_StrPtr(gg^.info)) then gg^.l2:=x
    else if (trtcInfoComp_StrPtr(xv2)>trtcInfoComp_StrPtr(gg^.info)) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_StrPtr.Insert(const v:trtcItemType_StrPtr;const info:trtcInfoType_StrPtr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (trtcInfoComp_StrPtr(xv2)<trtcInfoComp_StrPtr(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_StrPtr(xv2)>trtcInfoComp_StrPtr(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (trtcInfoComp_StrPtr(xv2)<trtcInfoComp_StrPtr(p^.info)) then p^.l2:=x
  else if (trtcInfoComp_StrPtr(xv2)>trtcInfoComp_StrPtr(p^.info)) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_StrPtr.Remove_AddParentNode(node:prtcNode3_StrPtr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_StrPtr.Remove_GetParentNode:prtcNode3_StrPtr;
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

procedure tRtcTree_StrPtr.Remove(const v:trtcItemType_StrPtr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (trtcInfoComp_StrPtr(xv2)<trtcInfoComp_StrPtr(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_StrPtr(xv2)>trtcInfoComp_StrPtr(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_StrPtr;
  t^.key:=vrtcItemMin_StrPtr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_StrPtr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_StrStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_StrStr,vrtcInfoNIL_StrStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_StrStr,vrtcInfoNIL_StrStr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_StrStr.Destroy;
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

function tRtcTree_StrStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_StrStr.New_Node(const k:trtcItemType_StrStr; const i:trtcInfoType_StrStr; const bi:boolean; const ll,rr:prtcNode3_StrStr):prtcNode3_StrStr;
  var
    p:prtcNodeArr3_StrStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_StrStr);
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
    GetMem(Result,SizeOf(trtcNode3_StrStr));
  FillChar(Result^,SizeOf(trtcNode3_StrStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_StrStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_StrStr.Del_Node(node:prtcNode3_StrStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_StrStr.Change(const v:trtcItemType_StrStr;const info:trtcInfoType_StrStr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_StrStr.RemoveThis(var t:prtcNode3_StrStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_StrStr;
  t^.key:=vrtcItemMin_StrStr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_StrStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_StrStr;
  head^.key:=vrtcItemMin_StrStr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_StrStr.Search(const v:trtcItemType_StrStr):trtcInfoType_StrStr;
  var
    x:prtcNode3_StrStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_StrStr.iSearch(const v:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x:prtcNode3_StrStr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_StrStr.Search_Min(var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x:prtcNode3_StrStr;
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
function tRtcTree_StrStr.iSearch_Min(var i:trtcItemType_StrStr):trtcInfoType_StrStr;
  var
    x:prtcNode3_StrStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_StrStr;
    Result:=vrtcInfoNIL_StrStr;
    end;
  end;

function tRtcTree_StrStr.Search_Max(var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x:prtcNode3_StrStr;
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
function tRtcTree_StrStr.iSearch_Max(var i:trtcItemType_StrStr):trtcInfoType_StrStr;
  var
    x:prtcNode3_StrStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_StrStr;
    Result:=vrtcInfoNIL_StrStr;
    end;
  end;

function tRtcTree_StrStr.Search_L(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x,y:prtcNode3_StrStr;
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
function tRtcTree_StrStr.iSearch_L(const v:trtcInfoType_StrStr; var i:trtcItemType_StrStr):trtcInfoType_StrStr;
  var
    x,y:prtcNode3_StrStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_StrStr.Search_G(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x,y:prtcNode3_StrStr;
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
function tRtcTree_StrStr.iSearch_G(const v:trtcInfoType_StrStr; var i:trtcItemType_StrStr):trtcInfoType_StrStr;
  var
    x,y:prtcNode3_StrStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_StrStr.Search_LE(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x,y:prtcNode3_StrStr;
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
function tRtcTree_StrStr.iSearch_LE(const v:trtcInfoType_StrStr; var i:trtcItemType_StrStr):trtcInfoType_StrStr;
  var
    x,y:prtcNode3_StrStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_StrStr.Search_GE(const v:trtcItemType_StrStr; var i:trtcInfoType_StrStr):trtcItemType_StrStr;
  var
    x,y:prtcNode3_StrStr;
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
function tRtcTree_StrStr.iSearch_GE(const v:trtcInfoType_StrStr; var i:trtcItemType_StrStr):trtcInfoType_StrStr;
  var
    x,y:prtcNode3_StrStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_StrStr.Insert_split;
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
procedure tRtcTree_StrStr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_StrStr.Insert(const v:trtcItemType_StrStr;const info:trtcInfoType_StrStr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_StrStr.Remove_AddParentNode(node:prtcNode3_StrStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_StrStr.Remove_GetParentNode:prtcNode3_StrStr;
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

procedure tRtcTree_StrStr.Remove(const v:trtcItemType_StrStr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_StrStr;
  t^.key:=vrtcItemMin_StrStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_StrStr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_WStrI32.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_WStrI32,vrtcInfoNIL_WStrI32,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_WStrI32,vrtcInfoNIL_WStrI32,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_WStrI32.Destroy;
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

function tRtcTree_WStrI32.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_WStrI32.New_Node(const k:trtcItemType_WStrI32; const i:trtcInfoType_WStrI32; const bi:boolean; const ll,rr:prtcNode3_WStrI32):prtcNode3_WStrI32;
  var
    p:prtcNodeArr3_WStrI32;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_WStrI32);
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
    GetMem(Result,SizeOf(trtcNode3_WStrI32));
  FillChar(Result^,SizeOf(trtcNode3_WStrI32),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_WStrI32.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_WStrI32.Del_Node(node:prtcNode3_WStrI32);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_WStrI32.Change(const v:trtcItemType_WStrI32;const info:trtcInfoType_WStrI32);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_WStrI32.RemoveThis(var t:prtcNode3_WStrI32);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_WStrI32;
  t^.key:=vrtcItemMin_WStrI32;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_WStrI32.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_WStrI32;
  head^.key:=vrtcItemMin_WStrI32;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_WStrI32.Search(const v:trtcItemType_WStrI32):trtcInfoType_WStrI32;
  var
    x:prtcNode3_WStrI32;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_WStrI32.iSearch(const v:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x:prtcNode3_WStrI32;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_WStrI32.Search_Min(var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x:prtcNode3_WStrI32;
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
function tRtcTree_WStrI32.iSearch_Min(var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;
  var
    x:prtcNode3_WStrI32;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_WStrI32;
    Result:=vrtcInfoNIL_WStrI32;
    end;
  end;

function tRtcTree_WStrI32.Search_Max(var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x:prtcNode3_WStrI32;
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
function tRtcTree_WStrI32.iSearch_Max(var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;
  var
    x:prtcNode3_WStrI32;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_WStrI32;
    Result:=vrtcInfoNIL_WStrI32;
    end;
  end;

function tRtcTree_WStrI32.Search_L(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x,y:prtcNode3_WStrI32;
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
function tRtcTree_WStrI32.iSearch_L(const v:trtcInfoType_WStrI32; var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;
  var
    x,y:prtcNode3_WStrI32;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_WStrI32.Search_G(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x,y:prtcNode3_WStrI32;
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
function tRtcTree_WStrI32.iSearch_G(const v:trtcInfoType_WStrI32; var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;
  var
    x,y:prtcNode3_WStrI32;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_WStrI32.Search_LE(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x,y:prtcNode3_WStrI32;
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
function tRtcTree_WStrI32.iSearch_LE(const v:trtcInfoType_WStrI32; var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;
  var
    x,y:prtcNode3_WStrI32;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_WStrI32.Search_GE(const v:trtcItemType_WStrI32; var i:trtcInfoType_WStrI32):trtcItemType_WStrI32;
  var
    x,y:prtcNode3_WStrI32;
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
function tRtcTree_WStrI32.iSearch_GE(const v:trtcInfoType_WStrI32; var i:trtcItemType_WStrI32):trtcInfoType_WStrI32;
  var
    x,y:prtcNode3_WStrI32;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_WStrI32.Insert_split;
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
procedure tRtcTree_WStrI32.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_WStrI32.Insert(const v:trtcItemType_WStrI32;const info:trtcInfoType_WStrI32);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_WStrI32.Remove_AddParentNode(node:prtcNode3_WStrI32);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_WStrI32.Remove_GetParentNode:prtcNode3_WStrI32;
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

procedure tRtcTree_WStrI32.Remove(const v:trtcItemType_WStrI32);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_WStrI32;
  t^.key:=vrtcItemMin_WStrI32;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_WStrI32.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_WStrI64.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_WStrI64,vrtcInfoNIL_WStrI64,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_WStrI64,vrtcInfoNIL_WStrI64,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_WStrI64.Destroy;
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

function tRtcTree_WStrI64.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_WStrI64.New_Node(const k:trtcItemType_WStrI64; const i:trtcInfoType_WStrI64; const bi:boolean; const ll,rr:prtcNode3_WStrI64):prtcNode3_WStrI64;
  var
    p:prtcNodeArr3_WStrI64;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_WStrI64);
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
    GetMem(Result,SizeOf(trtcNode3_WStrI64));
  FillChar(Result^,SizeOf(trtcNode3_WStrI64),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_WStrI64.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_WStrI64.Del_Node(node:prtcNode3_WStrI64);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_WStrI64.Change(const v:trtcItemType_WStrI64;const info:trtcInfoType_WStrI64);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_WStrI64.RemoveThis(var t:prtcNode3_WStrI64);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_WStrI64;
  t^.key:=vrtcItemMin_WStrI64;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_WStrI64.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_WStrI64;
  head^.key:=vrtcItemMin_WStrI64;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_WStrI64.Search(const v:trtcItemType_WStrI64):trtcInfoType_WStrI64;
  var
    x:prtcNode3_WStrI64;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_WStrI64.iSearch(const v:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x:prtcNode3_WStrI64;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_WStrI64.Search_Min(var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x:prtcNode3_WStrI64;
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
function tRtcTree_WStrI64.iSearch_Min(var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;
  var
    x:prtcNode3_WStrI64;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_WStrI64;
    Result:=vrtcInfoNIL_WStrI64;
    end;
  end;

function tRtcTree_WStrI64.Search_Max(var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x:prtcNode3_WStrI64;
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
function tRtcTree_WStrI64.iSearch_Max(var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;
  var
    x:prtcNode3_WStrI64;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_WStrI64;
    Result:=vrtcInfoNIL_WStrI64;
    end;
  end;

function tRtcTree_WStrI64.Search_L(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x,y:prtcNode3_WStrI64;
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
function tRtcTree_WStrI64.iSearch_L(const v:trtcInfoType_WStrI64; var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;
  var
    x,y:prtcNode3_WStrI64;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_WStrI64.Search_G(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x,y:prtcNode3_WStrI64;
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
function tRtcTree_WStrI64.iSearch_G(const v:trtcInfoType_WStrI64; var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;
  var
    x,y:prtcNode3_WStrI64;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_WStrI64.Search_LE(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x,y:prtcNode3_WStrI64;
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
function tRtcTree_WStrI64.iSearch_LE(const v:trtcInfoType_WStrI64; var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;
  var
    x,y:prtcNode3_WStrI64;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_WStrI64.Search_GE(const v:trtcItemType_WStrI64; var i:trtcInfoType_WStrI64):trtcItemType_WStrI64;
  var
    x,y:prtcNode3_WStrI64;
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
function tRtcTree_WStrI64.iSearch_GE(const v:trtcInfoType_WStrI64; var i:trtcItemType_WStrI64):trtcInfoType_WStrI64;
  var
    x,y:prtcNode3_WStrI64;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_WStrI64.Insert_split;
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
procedure tRtcTree_WStrI64.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_WStrI64.Insert(const v:trtcItemType_WStrI64;const info:trtcInfoType_WStrI64);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_WStrI64.Remove_AddParentNode(node:prtcNode3_WStrI64);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_WStrI64.Remove_GetParentNode:prtcNode3_WStrI64;
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

procedure tRtcTree_WStrI64.Remove(const v:trtcItemType_WStrI64);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_WStrI64;
  t^.key:=vrtcItemMin_WStrI64;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_WStrI64.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_WStrObj.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_WStrObj,vrtcInfoNIL_WStrObj,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_WStrObj,vrtcInfoNIL_WStrObj,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_WStrObj.Destroy;
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

function tRtcTree_WStrObj.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_WStrObj.New_Node(const k:trtcItemType_WStrObj; const i:trtcInfoType_WStrObj; const bi:boolean; const ll,rr:prtcNode3_WStrObj):prtcNode3_WStrObj;
  var
    p:prtcNodeArr3_WStrObj;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_WStrObj);
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
    GetMem(Result,SizeOf(trtcNode3_WStrObj));
  FillChar(Result^,SizeOf(trtcNode3_WStrObj),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_WStrObj.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_WStrObj.Del_Node(node:prtcNode3_WStrObj);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_WStrObj.Change(const v:trtcItemType_WStrObj;const info:trtcInfoType_WStrObj);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_WStrObj.RemoveThis(var t:prtcNode3_WStrObj);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_WStrObj;
  t^.key:=vrtcItemMin_WStrObj;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_WStrObj.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_WStrObj;
  head^.key:=vrtcItemMin_WStrObj;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_WStrObj.Search(const v:trtcItemType_WStrObj):trtcInfoType_WStrObj;
  var
    x:prtcNode3_WStrObj;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_WStrObj.iSearch(const v:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x:prtcNode3_WStrObj;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (trtcInfoComp_WStrObj(v)<trtcInfoComp_WStrObj(x^.info)) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_WStrObj.Search_Min(var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x:prtcNode3_WStrObj;
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
function tRtcTree_WStrObj.iSearch_Min(var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;
  var
    x:prtcNode3_WStrObj;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_WStrObj;
    Result:=vrtcInfoNIL_WStrObj;
    end;
  end;

function tRtcTree_WStrObj.Search_Max(var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x:prtcNode3_WStrObj;
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
function tRtcTree_WStrObj.iSearch_Max(var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;
  var
    x:prtcNode3_WStrObj;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_WStrObj;
    Result:=vrtcInfoNIL_WStrObj;
    end;
  end;

function tRtcTree_WStrObj.Search_L(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x,y:prtcNode3_WStrObj;
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
function tRtcTree_WStrObj.iSearch_L(const v:trtcInfoType_WStrObj; var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;
  var
    x,y:prtcNode3_WStrObj;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_WStrObj(x^.info)<trtcInfoComp_WStrObj(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_WStrObj.Search_G(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x,y:prtcNode3_WStrObj;
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
function tRtcTree_WStrObj.iSearch_G(const v:trtcInfoType_WStrObj; var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;
  var
    x,y:prtcNode3_WStrObj;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_WStrObj(x^.info)>trtcInfoComp_WStrObj(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_WStrObj.Search_LE(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x,y:prtcNode3_WStrObj;
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
function tRtcTree_WStrObj.iSearch_LE(const v:trtcInfoType_WStrObj; var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;
  var
    x,y:prtcNode3_WStrObj;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_WStrObj(x^.info)<trtcInfoComp_WStrObj(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_WStrObj.Search_GE(const v:trtcItemType_WStrObj; var i:trtcInfoType_WStrObj):trtcItemType_WStrObj;
  var
    x,y:prtcNode3_WStrObj;
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
function tRtcTree_WStrObj.iSearch_GE(const v:trtcInfoType_WStrObj; var i:trtcItemType_WStrObj):trtcInfoType_WStrObj;
  var
    x,y:prtcNode3_WStrObj;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_WStrObj(x^.info)>trtcInfoComp_WStrObj(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_WStrObj.Insert_split;
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
procedure tRtcTree_WStrObj.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (trtcInfoComp_WStrObj(xv2)<trtcInfoComp_WStrObj(g^.info)) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (trtcInfoComp_WStrObj(xv2)<trtcInfoComp_WStrObj(p^.info)) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (trtcInfoComp_WStrObj(xv2)<trtcInfoComp_WStrObj(g^.info)) then c:=g^.l2
      else if (trtcInfoComp_WStrObj(xv2)>trtcInfoComp_WStrObj(g^.info)) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (trtcInfoComp_WStrObj(xv2)<trtcInfoComp_WStrObj(c^.info)) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (trtcInfoComp_WStrObj(xv2)>trtcInfoComp_WStrObj(c^.info)) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (trtcInfoComp_WStrObj(xv2)<trtcInfoComp_WStrObj(g^.info)) then g^.l2:=p
      else if (trtcInfoComp_WStrObj(xv2)>trtcInfoComp_WStrObj(g^.info)) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (trtcInfoComp_WStrObj(xv2)<trtcInfoComp_WStrObj(gg^.info)) then c:=gg^.l2
    else if (trtcInfoComp_WStrObj(xv2)>trtcInfoComp_WStrObj(gg^.info)) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (trtcInfoComp_WStrObj(xv2)<trtcInfoComp_WStrObj(c^.info)) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (trtcInfoComp_WStrObj(xv2)>trtcInfoComp_WStrObj(c^.info)) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (trtcInfoComp_WStrObj(xv2)<trtcInfoComp_WStrObj(gg^.info)) then gg^.l2:=x
    else if (trtcInfoComp_WStrObj(xv2)>trtcInfoComp_WStrObj(gg^.info)) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_WStrObj.Insert(const v:trtcItemType_WStrObj;const info:trtcInfoType_WStrObj);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (trtcInfoComp_WStrObj(xv2)<trtcInfoComp_WStrObj(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_WStrObj(xv2)>trtcInfoComp_WStrObj(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (trtcInfoComp_WStrObj(xv2)<trtcInfoComp_WStrObj(p^.info)) then p^.l2:=x
  else if (trtcInfoComp_WStrObj(xv2)>trtcInfoComp_WStrObj(p^.info)) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_WStrObj.Remove_AddParentNode(node:prtcNode3_WStrObj);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_WStrObj.Remove_GetParentNode:prtcNode3_WStrObj;
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

procedure tRtcTree_WStrObj.Remove(const v:trtcItemType_WStrObj);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (trtcInfoComp_WStrObj(xv2)<trtcInfoComp_WStrObj(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_WStrObj(xv2)>trtcInfoComp_WStrObj(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_WStrObj;
  t^.key:=vrtcItemMin_WStrObj;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_WStrObj.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_WStrPtr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_WStrPtr,vrtcInfoNIL_WStrPtr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_WStrPtr,vrtcInfoNIL_WStrPtr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_WStrPtr.Destroy;
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

function tRtcTree_WStrPtr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_WStrPtr.New_Node(const k:trtcItemType_WStrPtr; const i:trtcInfoType_WStrPtr; const bi:boolean; const ll,rr:prtcNode3_WStrPtr):prtcNode3_WStrPtr;
  var
    p:prtcNodeArr3_WStrPtr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_WStrPtr);
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
    GetMem(Result,SizeOf(trtcNode3_WStrPtr));
  FillChar(Result^,SizeOf(trtcNode3_WStrPtr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_WStrPtr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_WStrPtr.Del_Node(node:prtcNode3_WStrPtr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_WStrPtr.Change(const v:trtcItemType_WStrPtr;const info:trtcInfoType_WStrPtr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_WStrPtr.RemoveThis(var t:prtcNode3_WStrPtr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_WStrPtr;
  t^.key:=vrtcItemMin_WStrPtr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_WStrPtr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_WStrPtr;
  head^.key:=vrtcItemMin_WStrPtr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_WStrPtr.Search(const v:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;
  var
    x:prtcNode3_WStrPtr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_WStrPtr.iSearch(const v:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x:prtcNode3_WStrPtr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (trtcInfoComp_WStrPtr(v)<trtcInfoComp_WStrPtr(x^.info)) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_WStrPtr.Search_Min(var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x:prtcNode3_WStrPtr;
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
function tRtcTree_WStrPtr.iSearch_Min(var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;
  var
    x:prtcNode3_WStrPtr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_WStrPtr;
    Result:=vrtcInfoNIL_WStrPtr;
    end;
  end;

function tRtcTree_WStrPtr.Search_Max(var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x:prtcNode3_WStrPtr;
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
function tRtcTree_WStrPtr.iSearch_Max(var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;
  var
    x:prtcNode3_WStrPtr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_WStrPtr;
    Result:=vrtcInfoNIL_WStrPtr;
    end;
  end;

function tRtcTree_WStrPtr.Search_L(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x,y:prtcNode3_WStrPtr;
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
function tRtcTree_WStrPtr.iSearch_L(const v:trtcInfoType_WStrPtr; var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;
  var
    x,y:prtcNode3_WStrPtr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_WStrPtr(x^.info)<trtcInfoComp_WStrPtr(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_WStrPtr.Search_G(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x,y:prtcNode3_WStrPtr;
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
function tRtcTree_WStrPtr.iSearch_G(const v:trtcInfoType_WStrPtr; var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;
  var
    x,y:prtcNode3_WStrPtr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (trtcInfoComp_WStrPtr(x^.info)>trtcInfoComp_WStrPtr(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_WStrPtr.Search_LE(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x,y:prtcNode3_WStrPtr;
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
function tRtcTree_WStrPtr.iSearch_LE(const v:trtcInfoType_WStrPtr; var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;
  var
    x,y:prtcNode3_WStrPtr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_WStrPtr(x^.info)<trtcInfoComp_WStrPtr(v)) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_WStrPtr.Search_GE(const v:trtcItemType_WStrPtr; var i:trtcInfoType_WStrPtr):trtcItemType_WStrPtr;
  var
    x,y:prtcNode3_WStrPtr;
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
function tRtcTree_WStrPtr.iSearch_GE(const v:trtcInfoType_WStrPtr; var i:trtcItemType_WStrPtr):trtcInfoType_WStrPtr;
  var
    x,y:prtcNode3_WStrPtr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (trtcInfoComp_WStrPtr(x^.info)>trtcInfoComp_WStrPtr(v)) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_WStrPtr.Insert_split;
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
procedure tRtcTree_WStrPtr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (trtcInfoComp_WStrPtr(xv2)<trtcInfoComp_WStrPtr(g^.info)) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (trtcInfoComp_WStrPtr(xv2)<trtcInfoComp_WStrPtr(p^.info)) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (trtcInfoComp_WStrPtr(xv2)<trtcInfoComp_WStrPtr(g^.info)) then c:=g^.l2
      else if (trtcInfoComp_WStrPtr(xv2)>trtcInfoComp_WStrPtr(g^.info)) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (trtcInfoComp_WStrPtr(xv2)<trtcInfoComp_WStrPtr(c^.info)) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (trtcInfoComp_WStrPtr(xv2)>trtcInfoComp_WStrPtr(c^.info)) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (trtcInfoComp_WStrPtr(xv2)<trtcInfoComp_WStrPtr(g^.info)) then g^.l2:=p
      else if (trtcInfoComp_WStrPtr(xv2)>trtcInfoComp_WStrPtr(g^.info)) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (trtcInfoComp_WStrPtr(xv2)<trtcInfoComp_WStrPtr(gg^.info)) then c:=gg^.l2
    else if (trtcInfoComp_WStrPtr(xv2)>trtcInfoComp_WStrPtr(gg^.info)) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (trtcInfoComp_WStrPtr(xv2)<trtcInfoComp_WStrPtr(c^.info)) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (trtcInfoComp_WStrPtr(xv2)>trtcInfoComp_WStrPtr(c^.info)) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (trtcInfoComp_WStrPtr(xv2)<trtcInfoComp_WStrPtr(gg^.info)) then gg^.l2:=x
    else if (trtcInfoComp_WStrPtr(xv2)>trtcInfoComp_WStrPtr(gg^.info)) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_WStrPtr.Insert(const v:trtcItemType_WStrPtr;const info:trtcInfoType_WStrPtr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (trtcInfoComp_WStrPtr(xv2)<trtcInfoComp_WStrPtr(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_WStrPtr(xv2)>trtcInfoComp_WStrPtr(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (trtcInfoComp_WStrPtr(xv2)<trtcInfoComp_WStrPtr(p^.info)) then p^.l2:=x
  else if (trtcInfoComp_WStrPtr(xv2)>trtcInfoComp_WStrPtr(p^.info)) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_WStrPtr.Remove_AddParentNode(node:prtcNode3_WStrPtr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_WStrPtr.Remove_GetParentNode:prtcNode3_WStrPtr;
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

procedure tRtcTree_WStrPtr.Remove(const v:trtcItemType_WStrPtr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (trtcInfoComp_WStrPtr(xv2)<trtcInfoComp_WStrPtr(x^.info)) then x:=x^.l2
    else if (trtcInfoComp_WStrPtr(xv2)>trtcInfoComp_WStrPtr(x^.info)) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_WStrPtr;
  t^.key:=vrtcItemMin_WStrPtr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_WStrPtr.Count: cardinal;
  begin
  Result:=cnt;
  end;

constructor tRtcTree_WStrWStr.Create(size:integer);
  begin
  inherited Create;
  cnt:=0;
  head:=nil;
  z:=nil;
  myPoolSize:=size;
  pool:=tRtcPtrPool.Create;
  z:=new_node(vrtcItemMin_WStrWStr,vrtcInfoNIL_WStrWStr,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(vrtcItemMin_WStrWStr,vrtcInfoNIL_WStrWStr,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

destructor tRtcTree_WStrWStr.Destroy;
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

function tRtcTree_WStrWStr.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tRtcTree_WStrWStr.New_Node(const k:trtcItemType_WStrWStr; const i:trtcInfoType_WStrWStr; const bi:boolean; const ll,rr:prtcNode3_WStrWStr):prtcNode3_WStrWStr;
  var
    p:prtcNodeArr3_WStrWStr;
    a:integer;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      a:=SizeOf(prtcNodeArr3_WStrWStr);
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
    GetMem(Result,SizeOf(trtcNode3_WStrWStr));
  FillChar(Result^,SizeOf(trtcNode3_WStrWStr),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    l2:=l;
    r2:=r;
    b2:=b;
    end;
  end;

procedure tRtcTree_WStrWStr.PoolSize(size:integer);
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tRtcTree_WStrWStr.Del_Node(node:prtcNode3_WStrWStr);
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

procedure tRtcTree_WStrWStr.Change(const v:trtcItemType_WStrWStr;const info:trtcInfoType_WStrWStr);
  begin
  remove(v);
  insert(v,info);
  end;

procedure tRtcTree_WStrWStr.RemoveThis(var t:prtcNode3_WStrWStr);
  begin
  if t^.l<>z then RemoveThis(t^.l);
  if t^.r<>z then RemoveThis(t^.r);
  t^.info:=vrtcInfoNIL_WStrWStr;
  t^.key:=vrtcItemMin_WStrWStr;
  t^.r2:=z;
  t^.l2:=z;
  del_node(t);
  t:=z;
  end;

procedure tRtcTree_WStrWStr.RemoveAll;
  begin
  if head=nil then Exit;
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=vrtcInfoNIL_WStrWStr;
  head^.key:=vrtcItemMin_WStrWStr;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

function tRtcTree_WStrWStr.Search(const v:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;
  var
    x:prtcNode3_WStrWStr;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;
function tRtcTree_WStrWStr.iSearch(const v:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x:prtcNode3_WStrWStr;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tRtcTree_WStrWStr.Search_Min(var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x:prtcNode3_WStrWStr;
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
function tRtcTree_WStrWStr.iSearch_Min(var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;
  var
    x:prtcNode3_WStrWStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_WStrWStr;
    Result:=vrtcInfoNIL_WStrWStr;
    end;
  end;

function tRtcTree_WStrWStr.Search_Max(var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x:prtcNode3_WStrWStr;
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
function tRtcTree_WStrWStr.iSearch_Max(var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;
  var
    x:prtcNode3_WStrWStr;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=vrtcItemMin_WStrWStr;
    Result:=vrtcInfoNIL_WStrWStr;
    end;
  end;

function tRtcTree_WStrWStr.Search_L(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x,y:prtcNode3_WStrWStr;
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
function tRtcTree_WStrWStr.iSearch_L(const v:trtcInfoType_WStrWStr; var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;
  var
    x,y:prtcNode3_WStrWStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_WStrWStr.Search_G(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x,y:prtcNode3_WStrWStr;
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
function tRtcTree_WStrWStr.iSearch_G(const v:trtcInfoType_WStrWStr; var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;
  var
    x,y:prtcNode3_WStrWStr;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tRtcTree_WStrWStr.Search_LE(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x,y:prtcNode3_WStrWStr;
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
function tRtcTree_WStrWStr.iSearch_LE(const v:trtcInfoType_WStrWStr; var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;
  var
    x,y:prtcNode3_WStrWStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tRtcTree_WStrWStr.Search_GE(const v:trtcItemType_WStrWStr; var i:trtcInfoType_WStrWStr):trtcItemType_WStrWStr;
  var
    x,y:prtcNode3_WStrWStr;
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
function tRtcTree_WStrWStr.iSearch_GE(const v:trtcInfoType_WStrWStr; var i:trtcItemType_WStrWStr):trtcInfoType_WStrWStr;
  var
    x,y:prtcNode3_WStrWStr;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tRtcTree_WStrWStr.Insert_split;
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
procedure tRtcTree_WStrWStr.Insert_split2;
  begin
  x^.b2:=true;
  x^.l2^.b2:=false;
  x^.r2^.b2:=false;
  if (p^.b2) then
    begin
    g^.b2:=true;
    if ( (xv2<g^.info) or ((xv2=g^.info) and (xv<g^.key)) ) <>
       ( (xv2<p^.info) or ((xv2=p^.info) and (xv<p^.key)) ) then
      begin
      // p_rotate_g; ->
      if (xv2<g^.info) then c:=g^.l2
      else if (xv2>g^.info) then c:=g^.r2
      else if (xv<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (xv2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (xv2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (xv<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (xv2<g^.info) then g^.l2:=p
      else if (xv2>g^.info) then g^.r2:=p
      else if (xv<g^.key) then g^.l2:=p
      else g^.r2:=p;
      // <-
      end;
    // x_rotate_gg; ->
    if (xv2<gg^.info) then c:=gg^.l2
    else if (xv2>gg^.info) then c:=gg^.r2
    else if (xv<gg^.key) then c:=gg^.l2
    else c:=gg^.r2;
    if (xv2<c^.info) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else if (xv2>c^.info) then
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end
    else if (xv<c^.key) then
      begin
      x:=c^.l2;
      c^.l2:=x^.r2;
      x^.r2:=c;
      end
    else
      begin
      x:=c^.r2;
      c^.r2:=x^.l2;
      x^.l2:=c;
      end;
    if (xv2<gg^.info) then gg^.l2:=x
    else if (xv2>gg^.info) then gg^.r2:=x
    else if (xv<gg^.key) then gg^.l2:=x
    else gg^.r2:=x;
    // <-
    x^.b2:=false;
    end;
  head^.r2^.b2:=false;
  end;

procedure tRtcTree_WStrWStr.Insert(const v:trtcItemType_WStrWStr;const info:trtcInfoType_WStrWStr);
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
  // Info Sort
  xv2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then Insert_Split2;
    end;
  x:=nx;
  if (xv2<p^.info) then p^.l2:=x
  else if (xv2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  Insert_Split2;
  Inc(cnt);
  end;

procedure tRtcTree_WStrWStr.Remove_AddParentNode(node:prtcNode3_WStrWStr);
  begin
  if node<>nil then
    with Parents^ do
      begin
      Nodes[NodeCount]:=node;
      Inc(NodeCount);
      end;
  end;

function tRtcTree_WStrWStr.Remove_GetParentNode:prtcNode3_WStrWStr;
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

procedure tRtcTree_WStrWStr.Remove(const v:trtcItemType_WStrWStr);
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
  xv2:=t^.info;
  Parents^.NodeCount:=0;
  p:=head;x:=head^.r2;
  Remove_AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    Remove_AddParentNode(p);
    if (xv2<x^.info) then x:=x^.l2
    else if (xv2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise ERtcSearch.Create('BinTree -> structure corrupt !');
  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      Remove_AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      Remove_AddParentNode(t);
      repeat
        Remove_AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      // SwapParentNode; ->
      with Parents^ do
        for a:=0 to NodeCount-1 do
          if Nodes[a]=t then
            begin
            Nodes[a]:=c;
            Break;
            end;
      // <-
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then
    begin
    // deleteFixup2; ->
    p:=Remove_GetParentNode;
    g:=Remove_GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            // y_rotateRight_p; ->
            c := y^.l2;
            y^.l2 := c^.r2;
            if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
            c^.r2 := y;
            y := p^.r2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          // p_rotateLeft_g; ->
          Remove_AddParentNode(g);
          p^.r2 := y^.l2;
          if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
          y^.l2 := p;
          g:=y; y:=p^.r2;
          // <-
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := Remove_GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            // y_rotateLeft_p; ->
            c := y^.r2;
            y^.r2 := c^.l2;
            if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
            c^.l2 := y;
            y := p^.l2;
            // <-
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          // p_rotateRight_g; ->
          Remove_AddParentNode(g);
          p^.l2 := y^.r2;
          if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
          y^.r2 := p;
          g:=y; y:=p^.l2;
          // <-
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
    // <-
    end;
  t^.info:=vrtcInfoNIL_WStrWStr;
  t^.key:=vrtcItemMin_WStrWStr;
  del_node(t);
  Dec(cnt);
  end;

function tRtcTree_WStrWStr.Count: cardinal;
  begin
  Result:=cnt;
  end;
  
end.