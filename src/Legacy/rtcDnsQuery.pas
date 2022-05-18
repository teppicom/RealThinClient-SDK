{
  DNS Query component (Legacy)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br>)

  This is a LEGACY unit, which means that continued use of this unit is discouraged.
  If you are using this unit, please keep in mind that there will be NO MORE UPDATES 
  released for this unit and that THERE IS NO SUPPORT AVAILABLE for using this UNIT.
  
  @exclude
}
unit rtcDnsQuery;

{$INCLUDE rtcDefs.inc}

interface

{$IFDEF WINDOWS}

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$R-}           { Disable range checking              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}

uses
  Classes,

  rtcTypes,
  rtcWinSock,

  rtcConn,
  rtcUdpCli;

const
  DnsQueryVersion    = 001;

  { Maximum answers (responses) count }
  MAX_ANCOUNT     = 50;
  { Maximum number of MX records taken into account in responses }
  MAX_MX_RECORDS  = 50;
  MAX_A_RECORDS   = 50;

  { DNS Classes }
  DnsClassIN      = 1;   // The internet
  DnsClassCS      = 2;   // The CSNET class (obsolete, used only for examples)
  DnsClassCH      = 3;   // The CHAOS class
  DnsClassHS      = 4;   // Hesiod name service
  DnsClassALL     = 255; // Any class

  { Type of query/response a DNS can handle }
  DnsQueryA       = 1;  // A     HostAddress
  DnsQueryNS      = 2;  // NS    Authoritative name server
  DnsQueryMD      = 3;  // MD    MailDestination, obsolete, use Mail Exchange
  DnsQueryMF      = 4;  // MF    MailForwarder, obsolete, use Mail Exchange
  DnsQueryCNAME   = 5;  // CNAME CanonicalName
  DnsQuerySOA     = 6;  // SOA   Start of a Zone of Authority
  DnsQueryMB      = 7;  // MB    MailBox, experimental
  DnsQueryMG      = 8;  // MG    MailGroup, experimental
  DnsQueryMR      = 9;  // MR    MailRename, experimental
  DnsQueryNULL    = 10; // NULL  Experimental
  DnsQueryWKS     = 11; // WKS   Well Known Service Description
  DnsQueryPTR     = 12; // PTR   Domain Name Pointer
  DnsQueryHINFO   = 13; // HINFO Host Information
  DnsQueryMINFO   = 14; // MINFO Mailbox information
  DnsQueryMX      = 15; // MX    Mail Exchange
  DnsQueryTXT     = 16; // TXT   Text Strings

  { Some additional type only allowed in queries }
  DnsQueryAXFR    = 252; // Transfer for an entire zone
  DnsQueryMAILB   = 253; // Mailbox related records (MB, MG or MR)
  DnsQueryMAILA   = 254; // MailAgent, obsolete, use MX instead
  DnsQueryALL     = 255; // Request ALL records

  { Opcode field in query flags }
  DnsOpCodeQUERY  = 0;
  DnsOpCodeIQUERY = 1;
  DnsOpCodeSTATUS = 2;

type
  TDnsAnswerNameArray   = packed array [0..MAX_ANCOUNT - 1]    of AnsiString;
  TDnsAnswerTypeArray   = packed array [0..MAX_ANCOUNT - 1]    of Integer;
  TDnsAnswerClassArray  = packed array [0..MAX_ANCOUNT - 1]    of Integer;
  TDnsAnswerTTLArray    = packed array [0..MAX_ANCOUNT - 1]    of LongInt;
  TDnsAnswerTagArray    = packed array [0..MAX_ANCOUNT - 1]    of Integer;
  TDnsMXPreferenceArray = packed array [0..MAX_MX_RECORDS - 1] of Integer;
  TDnsMXExchangeArray   = packed array [0..MAX_MX_RECORDS - 1] of AnsiString;
  TDnsAddressArray      = packed array [0..MAX_A_RECORDS - 1]  of TInAddr;

  TDnsRequestDoneEvent = procedure (Sender : TObject; Error : WORD) of Object;
  TDnsRequestHeader = packed record
      ID      : WORD;
      Flags   : WORD;
      QDCount : WORD;
      ANCount : WORD;
      NSCount : WORD;
      ARCount : WORD;
  end;
  PDnsRequestHeader = ^TDnsRequestHeader;

  TRtcDnsQuery = class(TComponent)
  private
    { Déclarations privées }
  protected
    FWSocket                    : TRtcUdpClient;
    FPort                       : RtcString;
    FAddr                       : RtcString;
    FIDCount                    : WORD;
    FQueryBuf                   : array [0..4096] of AnsiChar;
    FQueryLen                   : Integer;
    FResponseBuf                : array [0..4096] of AnsiChar;
    FResponseLen                : Integer;
    FResponseID                 : Integer;
    FResponseCode               : Integer;
    FResponseOpCode             : Integer;
    FResponseAuthoritative      : Boolean;
    FResponseTruncation         : Boolean;
    FResponseRecursionAvailable : Boolean;
    FResponseQDCount            : Integer;
    FResponseANCount            : Integer;
    FResponseNSCount            : Integer;
    FResponseARCount            : Integer;
    FQuestionType               : Integer;
    FQuestionClass              : Integer;
    FQuestionName               : AnsiString;
    FAnswerNameArray            : TDnsAnswerNameArray;
    FAnswerTypeArray            : TDnsAnswerTypeArray;
    FAnswerClassArray           : TDnsAnswerClassArray;
    FAnswerTTLArray             : TDnsAnswerTTLArray;
    FAnswerTagArray             : TDnsAnswerTagArray;
    FMXRecordCount              : Integer;
    FMXPreferenceArray          : TDnsMXPreferenceArray;
    FMXExchangeArray            : TDnsMXExchangeArray;
    FARecordCount               : Integer;
    FAddressArray               : TDnsAddressArray;
    FOnRequestDone              : TDnsRequestDoneEvent;
    function GetMXPreference(nIndex : Integer) : Integer;
    function GetMXExchange(nIndex : Integer) : AnsiString;
    function GetAnswerName(nIndex : Integer) : AnsiString;
    function GetAnswerType(nIndex : Integer) : Integer;
    function GetAnswerClass(nIndex : Integer) : Integer;
    function GetAnswerTTL(nIndex : Integer) : LongInt;
    function GetAnswerTag(nIndex : Integer) : Integer;
    function GetAddress(nIndex : Integer) : TInAddr;
    procedure BuildRequestHeader(Dst       : PDnsRequestHeader;
                                 ID        : WORD;
                                 OPCode    : BYTE;
                                 Recursion : Boolean;
                                 QDCount   : WORD;
                                 ANCount   : WORD;
                                 NSCount   : WORD;
                                 ARCount   : WORD); virtual;
    function  BuildQuestionSection(Dst         : PAnsiChar;
                                   const QName : AnsiString;
                                   QType       : WORD;
                                   QClass      : WORD) : Integer; virtual;
    procedure WSocketDataAvailable(Sender: TRtcConnection); virtual;
    procedure TriggerRequestDone(Error: WORD); virtual;
    function  GetResponseBuf : PAnsiChar;
    procedure SendQuery;
    function  ExtractName(Base       : PAnsiChar;
                          From       : PAnsiChar;
                          var Name   : AnsiString) : PAnsiChar;
    function  DecodeQuestion(Base       : PAnsiChar;
                             From       : PAnsiChar;
                             var Name   : AnsiString;
                             var QType  : Integer;
                             var QClass : Integer) : PAnsiChar;
    function DecodeAnswer(Base         : PAnsiChar;
                          From         : PAnsiChar;
                          var Name     : AnsiString;
                          var QType    : Integer;
                          var QClass   : Integer;
                          var TTL      : LongInt;
                          var RDataPtr : Pointer;
                          var RDataLen : Integer) : PAnsiChar;
    function DecodeMXData(Base           : PAnsiChar;
                          From           : PAnsiChar;
                          var Preference : Integer;
                          var Exchange   : AnsiString) : PAnsiChar;
    function DecodeAData(Base        : PAnsiChar;
                         From        : PAnsiChar;
                         var Address : TInAddr) : PAnsiChar;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Notification(AComponent: TComponent; operation: TOperation); override;
    function    MXLookup(const Domain : AnsiString) : Integer;
    function    ALookup(const Host : AnsiString) : Integer;
    property ResponseID                 : Integer read FResponseID;
    property ResponseCode               : Integer read FResponseCode;
    property ResponseOpCode             : Integer read FResponseOpCode;
    property ResponseAuthoritative      : Boolean read FResponseAuthoritative;
    property ResponseTruncation         : Boolean read FResponseTruncation;
    property ResponseRecursionAvailable : Boolean read FResponseRecursionAvailable;
    property ResponseQDCount            : Integer read FResponseQDCount;
    property ResponseANCount            : Integer read FResponseANCount;
    property ResponseNSCount            : Integer read FResponseNSCount;
    property ResponseARCount            : Integer read FResponseARCount;
    property ResponseBuf                : PAnsiChar   read GetResponseBuf;
    property ResponseLen                : Integer read FResponseLen;
    property QuestionType               : Integer read FQuestionType;
    property QuestionClass              : Integer read FQuestionClass;
    property QuestionName               : AnsiString  read FQuestionName;
    property AnswerName[nIndex : Integer]   : AnsiString  read GetAnswerName;
    property AnswerType[nIndex : Integer]   : Integer read GetAnswerType;
    property AnswerClass[nIndex : Integer]  : Integer read GetAnswerClass;
    property AnswerTTL[nIndex : Integer]    : LongInt read GetAnswerTTL;
    property AnswerTag[nIndex : Integer]    : Integer read GetAnswerTag;
    property MXPreference[nIndex : Integer] : Integer read GetMXPreference;
    property MXExchange[nIndex : Integer]   : AnsiString  read GetMXExchange;
    property AddressCount:integer read FARecordCount;
    property Address[nIndex : Integer]      : TInAddr read GetAddress;
  published
    property Port    : RtcString read  FPort write FPort;
    property Addr    : RtcString read  FAddr write FAddr;
    property OnRequestDone : TDnsRequestDoneEvent read  FOnRequestDone
                                                  write FOnRequestDone;
  end;

{$ENDIF} // {$IFDEF WINDOWS}

implementation

{$IFDEF WINDOWS}

type
    PWORD  = ^WORD;
    PDWORD = ^Cardinal;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TRtcDnsQuery.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FWSocket := TRtcUdpClient.New;
    FPort    := '53';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TRtcDnsQuery.Destroy;
begin
    if Assigned(FWSocket) then begin
        RtcFreeAndNil(FWSocket);
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRtcDnsQuery.Notification(AComponent: TComponent; operation: TOperation);
begin
    inherited Notification(AComponent, operation);
    if operation = opRemove then begin
        if AComponent = FWSocket then
            FWSocket := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.GetMXPreference(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FMXPreferenceArray)) or
       (nIndex > High(FMXPreferenceArray)) then
        Result := 0
    else
        Result := FMXPreferenceArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.GetMXExchange(nIndex : Integer) : AnsiString;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FMXExchangeArray)) or
       (nIndex > High(FMXExchangeArray)) then
        Result := ''
    else
        Result := FMXExchangeArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.GetAnswerName(nIndex : Integer) : AnsiString;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerNameArray)) or
       (nIndex > High(FAnswerNameArray)) then
        Result := ''
    else
        Result := FAnswerNameArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.GetAnswerType(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTypeArray)) or
       (nIndex > High(FAnswerTypeArray)) then
        Result := 0
    else
        Result := FAnswerTypeArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.GetAnswerClass(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerClassArray)) or
       (nIndex > High(FAnswerClassArray)) then
        Result := 0
    else
        Result := FAnswerClassArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.GetAnswerTTL(nIndex : Integer) : LongInt;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTTLArray)) or
       (nIndex > High(FAnswerTTLArray)) then
        Result := 0
    else
        Result := FAnswerTTLArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.GetAnswerTag(nIndex : Integer) : Integer;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAnswerTagArray)) or
       (nIndex > High(FAnswerTagArray)) then
        Result := 0
    else
        Result := FAnswerTagArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.GetAddress(nIndex : Integer) : TInAddr;
begin
    { Silently ignore index out of bounds error }
    if (nIndex < Low(FAddressArray)) or
       (nIndex > High(FAddressArray)) then
        Result.S_addr := 0
    else
        Result := FAddressArray[nIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.GetResponseBuf : PAnsiChar;
begin
    Result := @FResponseBuf;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.MXLookup(const Domain : AnsiString) : Integer;
begin
   Inc(FIDCount);
   BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
   FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)], Domain, DnsQueryMX, DnsClassIN);
   FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
   Result    := FIDCount;
   SendQuery;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.ALookup(const Host : AnsiString) : Integer;
begin
   Inc(FIDCount);
   BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
   FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)], Host, DnsQueryA, DnsClassIN);
   FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
   Result    := FIDCount;
   SendQuery;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRtcDnsQuery.SendQuery;
var
  s:RtcByteArray;
begin
  FResponseLen             := -1;
    FWSocket.OnDataReceived := WSocketDataAvailable;
    FWSocket.ServerPort            := FPort;
    FWSocket.ServerAddr            := FAddr;
    FWSocket.Connect;

  FARecordCount:=0;
  FMXRecordCount:=0;

  SetLength(s,FQueryLen);
  Move(FQueryBuf[0],s[0],FQueryLen);
  FWSocket.WriteEx(s);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.BuildQuestionSection(
    Dst         : PAnsiChar;
    const QName : AnsiString;
    QType       : WORD;
    QClass      : WORD) : Integer;
var
    I   : Integer;
    p   : PAnsiChar;
    Ptr : PAnsiChar;
begin
    Ptr := Dst;
    if Ptr = nil then begin
        Result := 0;
        Exit;
    end;
    I := 1;
    while I <= Length(QName) do begin
        p := Ptr;
        Inc(Ptr);
        while (I <= Length(QName)) and (QName[I] <> '.') do begin
            Ptr^ := QName[I];
            Inc(Ptr);
            Inc(I);
        end;
        p^ := AnsiChar(Ptr - p - 1);
        Inc(I);
    end;
    Ptr^ := #0;
    Inc(Ptr);
    PWORD(Ptr)^ := WSocket_htons(QType);
    Inc(Ptr, 2);
    PWORD(Ptr)^ := WSocket_htons(QClass);
    Inc(Ptr, 2);
    Result := Ptr - Dst;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRtcDnsQuery.BuildRequestHeader(
    Dst       : PDnsRequestHeader;
    ID        : WORD;
    OPCode    : BYTE;
    Recursion : Boolean;
    QDCount   : WORD;
    ANCount   : WORD;
    NSCount   : WORD;
    ARCount   : WORD);
begin
    if Dst = nil then
        Exit;

    LoadWinSock;
    
    Dst^.ID      := WSocket_htons(ID);
    Dst^.Flags   := WSocket_htons((OpCode shl 11) + (Ord(Recursion) shl 8));
    Dst^.QDCount := WSocket_htons(QDCount);
    Dst^.ANCount := WSocket_htons(ANCount);
    Dst^.NSCount := WSocket_htons(NSCount);
    Dst^.ARCount := WSocket_htons(ARCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRtcDnsQuery.TriggerRequestDone(Error: WORD);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TRtcDnsQuery.WSocketDataAvailable(Sender: TRtcConnection);
var
    Len    : Integer;
    Ans    : PDnsRequestHeader;
    Flags  : Integer;
    P      : PAnsiChar;
    RDataPtr : Pointer;
    RDataLen : Integer;
    I        : Integer;
    s:RtcByteArray;
begin
      Ans := PDnsRequestHeader(@FResponseBuf);

      // Len := FWSocket.Receive(Ans, SizeOf(FResponseBuf));
      {if Error <> 0 then begin
          TriggerRequestDone(Error);
          Exit;
      end;}

      s:=FWSocket.ReadEx;
      Len:=length(s);
      if Len < SizeOf(TDnsRequestHeader) then
        begin
        TriggerRequestDone(1);
        Exit;
        end;
      Move(s[0],Ans^,length(s));

      { Check for minimum response length }
      Flags := WSocket_htons(Ans^.Flags);
      { Check if we got a response }
      if (Flags and $8000) = 0 then
        begin
        TriggerRequestDone(2);
        Exit;
        end;
      FResponseLen := Len;
      { Decode response header }
      FResponseID                 := WSocket_htons(Ans^.ID);
      FResponseCode               := Flags and $000F;
      FResponseOpCode             := (Flags shr 11) and $000F;
      FResponseAuthoritative      := (Flags and $0400) = $0400;
      FResponseTruncation         := (Flags and $0200) = $0200;
      FResponseRecursionAvailable := (Flags and $0080) = $0080;
      FResponseQDCount            := WSocket_ntohs(Ans^.QDCount);
      FResponseANCount            := WSocket_ntohs(Ans^.ANCount);
      FResponseNSCount            := WSocket_ntohs(Ans^.NSCount);
      FResponseARCount            := WSocket_ntohs(Ans^.ARCount);

      P := @ResponseBuf[SizeOf(TDnsRequestHeader)];
      if FResponseQDCount = 0 then begin
          { I don't think we could receive 0 questions }
          FQuestionName  := '';
          FQuestionType  := 0;
          FQuestionClass := 0;
      end
      else begin
          { Should never be greater than 1 because we sent only one question }
          P := DecodeQuestion(@FResponseBuf, P,
                              FQuestionName, FQuestionType, FQuestionClass);
      end;
      if FResponseANCount = 0 then begin
  //        FAnswerName    := '';
  //        FAnswerType    := 0;
  //        FAnswerClass   := 0;
  //        FAnswerTTL     := 0;
          RDataPtr       := nil;
          RDataLen       := 0;
          FMXRecordCount := 0;
      end
      else begin
          FMXRecordCount := 0;
          for I := 0 to FResponseANCount - 1 do begin
              P := DecodeAnswer(@FResponseBuf,        P,
                                FAnswerNameArray[I],  FAnswerTypeArray[I],
                                FAnswerClassArray[I], FAnswerTTLArray[I],
                                RDataPtr,             RDataLen);
              FAnswerTagArray[I] := -1;
              case FAnswerTypeArray[I] of
              DnsQueryMX:
                  begin
                      if FMXRecordCount <= High(FMXPreferenceArray) then begin
                          FAnswerTagArray[I] := FMXRecordCount;
                          DecodeMXData(@FResponseBuf, RDataPtr,
                                       FMXPreferenceArray[FMXRecordCount],
                                       FMXExchangeArray[FMXRecordCount]);
                          Inc(FMXRecordCount);
                      end;
                  end;
              DnsQueryA:
                  begin
                      if FARecordCount <= High(FAddressArray) then begin
                          FAnswerTagArray[I] := FARecordCount;
                          DecodeAData(@FResponseBuf, RDataPtr,
                                      FAddressArray[FARecordCount]);
                          Inc(FARecordCount);
                      end;
                  end;
              end;
          end;
      end;
    TriggerRequestDone(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.ExtractName(
    Base       : PAnsiChar;
    From       : PAnsiChar;
    var Name   : AnsiString) : PAnsiChar;
var
    N       : Integer;
    I       : Integer;
    P       : PAnsiChar;
    NameEnd : AnsiString;
begin
    P := From;
    if P^ = #0 then begin
        Name := '';
        Inc(P);
    end
    else begin
        Name := '';
        while TRUE do begin
            { Get name part length }
            N := Ord(P^);
            if (N and $C0) = $C0 then begin
                 { Message compression }
                 N := ((N and $3F) shl 8) + Ord(P[1]);
                 if Length(Name) = 0 then
                     ExtractName(Base, Base + N, Name)
                 else begin
                     ExtractName(Base, Base + N, NameEnd);
                     Name := Name + NameEnd;
                 end;
                 Inc(P, 2);
                 break;
            end;
            Inc(P);
            if N = 0 then
                break;
            { Copy name part }
            for I := 1 to N do begin
                Name := Name + P^;
                Inc(P);
            end;
            if P^ <> #0 then
                Name := Name + '.';
        end;
    end;
    Result := P;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.DecodeQuestion(
    Base       : PAnsiChar;
    From       : PAnsiChar;
    var Name   : AnsiString;
    var QType  : Integer;
    var QClass : Integer) : PAnsiChar;
var
    P : PAnsiChar;
begin
    P := ExtractName(Base, From, Name);
    QType  := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    QClass := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    Result := P;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.DecodeAnswer(
    Base         : PAnsiChar;
    From         : PAnsiChar;
    var Name     : AnsiString;
    var QType    : Integer;
    var QClass   : Integer;
    var TTL      : LongInt;
    var RDataPtr : Pointer;
    var RDataLen : Integer) : PAnsiChar;
var
    P : PAnsiChar;
begin
    P := ExtractName(Base, From, Name);
    QType  := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    QClass := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    TTL    := WSocket_ntohl(PDWORD(P)^);
    Inc(P, 4);
    RDataLen := WSocket_ntohs(PWORD(P)^);
    Inc(P, 2);
    RDataPtr := P;
    Result := P + RDataLen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.DecodeMXData(
    Base           : PAnsiChar;
    From           : PAnsiChar;
    var Preference : Integer;
    var Exchange   : AnsiString) : PAnsiChar;
begin
    Result := From;
    Preference := WSocket_ntohs(PWORD(Result)^);
    Inc(Result, 2);
    Result := ExtractName(Base, Result, Exchange);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TRtcDnsQuery.DecodeAData(
    Base        : PAnsiChar;
    From        : PAnsiChar;
    var Address : TInAddr) : PAnsiChar;
begin
    Result := From;
    Address.S_addr := PDWORD(Result)^;
    Inc(Result, 4);
end;

{$ENDIF} // {$IFDEF WINDOWS}
end.
