class(TRtcTestCase)
  {$IFDEF RTC_GENERIC}
  type
    {$IFDEF RTC_TREE}
      TTestClass = tRtcSearchTree<itemType,infoType>;
    {$ELSE}
      TTestClass = tRtcSearchList<itemType,infoType>;
    {$ENDIF}
  {$ENDIF}
  protected
    {$IFDEF RTC_GENERIC}
    itemC: IComparer<itemType>;
    infoC: IComparer<infoType>;
    itemMin: itemType;
    infoNil: infoType;
    {$ENDIF}

    FMyList: TTestClass;
    MySize: cardinal;
    FullSearch: boolean;

    ReturnValue: infoType;
    v: itemType;
    s: array of itemType;

    MyType: byte; // used in GetItemValue

    procedure InsertAll(typ:byte);

    procedure SearchAll;
    procedure SearchUp;
    procedure SearchDown;
  {$IFDEF RTC_TREE}
    procedure iSearchUp;
    procedure iSearchDown;
  {$ENDIF}

    procedure RemoveAll;
    procedure RemoveAllDown;
    procedure RemoveAllUp;
    procedure RemoveSearchDown;
    procedure RemoveSearchUp;
  {$IFDEF RTC_TREE}
    procedure RemoveiSearchDown;
    procedure RemoveiSearchUp;
  {$ENDIF}

    procedure CheckEmpty;

    function GetItemValue(a:cardinal):itemType; {$IFDEF RTC_GENERIC}virtual; abstract;{$ENDIF}
    procedure SetReturnValue(a:cardinal); {$IFDEF RTC_GENERIC}virtual; abstract;{$ENDIF}

  public
    constructor Create(TestSize,PoolSize:cardinal; full_search:boolean; run_now:boolean
                        {$IFDEF RTC_GENERIC}; _min:itemType; _nil:infoType{$ENDIF});
    destructor Destroy; override;

    procedure Run;
  end
