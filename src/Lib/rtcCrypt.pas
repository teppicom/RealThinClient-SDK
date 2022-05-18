{
  @html(<b>)
  Encryption Unit
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit provides simple-to-use classes for Random Number Generation,
  Encryption, Decryption, Asymmetric RSA Key Generation, (De-)Serialization,
  Digest Hashing, Signing and Verification using EMSA-PKCS#1-v1.5 encoding.

  Random Number Generator and RSA Encryption are based on multi-precision
  integer arithmetic and RSA routines written by W.Ehrhardt, available
  online at http://www.wolfgang-ehrhardt.de/mp_intro.html
}
unit rtcCrypt;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcSystem,
  rtcLog;

type
  { @abstract(RTC "Crypt" Object) }
  TRtcCryptObject = class(TObject);

  TRtcCrypt = class(TRtcCryptObject)
    private
      FCryptKey:RtcByteArray;
      FCryptCode:RtcByteArray;

      CErr:integer;
      CPos:integer;
      CLen:integer;
      CCode:byte;
      CValue:longint;
      CInit:boolean;

      procedure SetCryptCodeEx(const Value: RtcByteArray);

      procedure SetCryptCode(const Value: RtcString);
      function GetCryptCode:RtcString;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Init;

      procedure Crypt(var s:RtcString);
      procedure DeCrypt(var s:RtcString);

      procedure CryptEx(var s:RtcByteArray);
      procedure DeCryptEx(var s:RtcByteArray);

      property Key:RtcString read GetCryptCode write SetCryptCode;
      property KeyEx:RtcByteArray read FCryptCode write SetCryptCodeEx;
    end;

{$IFNDEF RTC_NORSA}

const
  // max number of mp_int digits @exclude
  MAXDigits = $1000000;

type
  // Exception class
  ERtcRSA=class(Exception);

  // type that holds a multi-precision digit @exclude
  mp_digit    = cardinal;

  // the digits of an mp_int @exclude
  TDigitArray = packed array[0..MaxDigits+63] of mp_digit;

  // pointer to digit array @exclude
  PDigitArray = ^TDigitArray;          

  // MP integer number @exclude
  mp_int      = record                 
                // pointer to digit array
                pdigits : PDigitArray; 
                // allocated digits in pdigits 
                alloc   : Longint;
                // used digits in pdigits     
                used    : Longint;
                // sign: MP_ZPOS or MP_NEG
                sign    : word;        
                // set to MP_MAGIC by mp_init
                magic   : word;
                end;
  // RSA private key record with CRT coefficients @exclude
  TPrivateKey = record
                p,q  : mp_int; {primes with n=p*q  }
                dp   : mp_int; {dp = e^-1 mod (p-1)}
                dq   : mp_int; {dq = e^-1 mod (q-1)}
                qinv : mp_int; {qinv = q^-1 mod p  }
                end;
  // context of random number generator @exclude
  isaac_ctx = record
                randmem: array[0..255] of Longint; {the internal state}
                randrsl: array[0..255] of Longint; {the results given to the user}
                randa  : Longint;                  {accumulator}
                randb  : Longint;                  {the last result}
                randc  : Longint;                  {counter, guarantees cycle >= 2^40 }
                nextres: Longint;                  {the next result }
                randidx: word;                     {the index in randrsl[] }
              end;

  {Valid hash algorithms for RSA signature schemes.  Only the hash
   digests and algorithm identifiers are used, not the actual hash
   functions, i.e. the digests have to be calculated externally.  }
  RtcHashType = (// 16-byte MD2 Hash
                 rtch_MD2,    
                 // 16-byte MD5 Hash
                 rtch_MD5,
                 // 20-byte RMD160 Hash
                 rtch_RMD160, 
                 // 20-byte SHA1 Hash
                 rtch_SHA1,
                 // 28-byte SHA224 Hash
                 rtch_SHA224, 
                 // 32-byte SHA256 Hash
                 rtch_SHA256,
                 // 48-Byte SHA384 Hash
                 rtch_SHA384,
                 // 64-Byte SHA512 Hash
                 rtch_SHA512,
                 // Custom Hash (NO Hash Type Header injected)
                 rtch_Custom);

type
  // @Abstract(ISAAC Random Numbers Generator)
  TRtcISAAC=class(TRtcCryptObject)
  private
    ctx:isaac_ctx;

  public
    // Create Random Numbers Generator ... and randomize it, if randomized=True. }
    constructor Create(randomized:boolean=False);
    destructor Destroy; override;

    { Generate a random Byte (8-bit positive integer) }
    function RandomByte:byte;
    { Generate a random Word (16-bit positive integer) }
    function RandomWord:word;
    { Generate a random Digit (32-bit positive integer) }
    function RandomDigit:mp_digit;
    { Generate a random number between 0 and "max - 1" }
    function Random(max:mp_digit):mp_digit;

    { Generate a "strong" Random positive 16-bit Integer (0-32766) using a
      combination of the ISAAC algorithm, current Time, Memory and loops. }
    function RandomInt:Longint;

    { Generate an Array of "size" Bytes filled with Random values. }
    function RND(size:Longint):RtcByteArray;

    { Prepare the Random Numbers Generator by using a
       combination of the ISAAC algorithm, current Time, Memory and loops. }
    procedure RandomizeMax;

    { Prepare the Random Numbers Generator by using the following parameters:
        if useSeed <> 0, the specified Seed number will be used, plus
        if useSelf = TRUE, this objects location in memory will be used, plus
        if useTime = TRUE, the current Time will be used, plus
        if useRND = TRUE, Random function from Delphi will be used.
      Random Numbers are used for Key Generation, Encryption and the RND method. }
    procedure Randomize(useSeed:Longint=0; useSelf:boolean=False; useTime:boolean=False; useRND:boolean=False);

    { Grow a new Array of Random Bytes using these parameters:
       FromData = Random array of bytes to use as Source (has to contain at least 1 byte);
       Seed = Seed to be used for Random Growth generation (any 32-bit Integer will do)
       Size = Size to which the array should grow (should be greater or equal to length of FromData)
       Growth = Growth control number, controls growth speed (0=no growth, 1 or more = controlled growth)

      Growth is reproducible, which means that it will always generate the same pseudo-random sequence
      from the same set of parameters. It works by using the random numbers generator with the specified
      Seed Number and the Growth parameter to control the growth rate of each supplied byte in the Source
      array (FromData). If a single growth pass does not generate the wanted Size, the Growth parameter is
      increased and the next pass is calculated using data from the last. Repeat until "Size" is reached. }
    function GrowFromSeed(const FromData:RtcByteArray; Seed, Size:Longint; Growth:byte):RtcByteArray;
    end;

  // @Abstract(Asymmetric "RSA" Encryption/Decryption)
  TRtcRSA=class(TRtcISAAC)
  private
    pKey:TPrivateKey;
    pE,pN:mp_int;
    PrivateArr,
    PublicArr:RtcByteArray;

    function GetPrivateKey: RtcByteArray;
    function GetPublicKey: RtcByteArray;

    procedure SetPrivateKey(const Value: RtcByteArray);
    procedure SetPublicKey(const Value: RtcByteArray);

  public
    constructor Create;
    destructor Destroy; override;

    { Generate a new Public and Private Key Pair for asymetric RSA Encryption and
      Decryption using Encryption Strength of "Octets" Bytes, where 1 Octet Strength
      is the equivalent of 8 Bits. For volatile or long term Keys, values between 64
      and 128 Octets (= 512 to 1024 bits) are recommended. Higher Octet values are
      supported, but expect the time required to Generate a new Key Pair to grow
      EXPONENTIALLY, especially if you go beyoned and above 256 Octets (2048 Bits).
       32 Octets =  256 bits;   64 Octets =  512 bits; 128 Octets = 1024 bits;
       256 Octets = 2048 bits; 384 Octets = 3072 bits; 512 Octets = 4096 bits.}
    procedure GenerateKeyPair(Octets:Longint);

    { Encrypt "Data" using the PUBLIC Key and PKCS#1-v1.5 padding.
      If more Data has to be Encrypted than fit in a single Crypto-Block,
      more blocks will be used (random numbers are used for padding).
      Encryption preserves all Bytes from Data in encrypted state, but
      consumes at least 11 more bytes per crypto-block than the original. }
    function Encrypt(const Data:RtcByteArray):RtcByteArray;

    { Decrypt "Data" using the PRIVATE Key and PKCS#1-v1.5 padding.
      If more than one crypto-block is provided in "Data", all decrypted
      blocks are concatenated to form a single continuous Byte Array,
      making Decrypt the exact opposite operation of Encrypt. }
    function Decrypt(const Data:RtcByteArray):RtcByteArray;

    { Sign "PlainHash" as "HashType" using the PRIVATE Key and EMSA-PKCS#1-v1.5 encoding.
      Before using this method, "PlainHash" has to be prepared by hashing Data with a Hash function.
      Use "HashType=rtch_Custom" to Sign a custom Hash with a Custom Header (no Hash type prefix). }
    function SignHash(HashType:RtcHashType; const PlainHash:RtcByteArray):RtcByteArray;

    { Use PUBLIC Key with EMSA-PKCS#1-v1.5 encoding to verify that "SignedHash"
      is really "PlainHash" signed as "AHash" using the correct Private Key.
      Return TRUE if Hash Type, Data and Signature match; FALSE if they don't match.
      The same Hash function has to be used on the same Data to generate the same PlainHash. }
    function VerifyHash(HashType:RtcHashType; const PlainHash, SignedHash:RtcByteArray):boolean;

    { Maximum number of Bytes which can be encrypted in a single crypto-block.
      When more bytes need encrypting, additional blocks of the same size are used.
      11 Bytes of each Block are used for random padding, to make the Code secure. }
    function BlockSize:Longint;

    { Encryption/Decryption Key Strength in Bits }
    function KeyBits:Longint;

    { Get or Set the complete Private Key (including the Public Key) as a Byte Array,
       encoded using the "Plain Memory" representation (no encoding or encryption) }
    property PrivateKey:RtcByteArray read GetPrivateKey write SetPrivateKey;

    { Get or Set the complete Public Key as a Byte Array,
       encoded using the "Plain Memory" representation (no encoding or encryption) }
    property PublicKey:RtcByteArray read GetPublicKey write SetPublicKey;
    end;

  // @Abstract(RTC Encryption with ISAAC random numbers)
  TRtcISAACrypt = class(TRtcCryptObject)
    private
      FCryptKey:RtcByteArray;
      FSeed:Longint;
      ctx:isaac_ctx;

      CErr:Longint;
      CPos:Longint;
      CLen:Longint;
      CCode:byte;
      CValue:Longint;
      CInit:boolean;

      procedure SetCryptKey(const Value: RtcByteArray);

    public
      constructor Create;
      destructor Destroy; override;

      // Initialize Encryption/Decryption
      procedure Init;

      // Crypt "s" in-place and update the Encryption/Decryption code
      procedure Crypt(var s:RtcByteArray);

      // Decrypt "s" in-place and update the Encryption/Decryption code
      procedure DeCrypt(var s:RtcByteArray);

      // Get/Set the Encryption Key
      property Key:RtcByteArray read FCryptKey write SetCryptKey;
    end;

{$ENDIF} // {$IFNDEF RTC_NORSA}

const
  MAX_RTCKEYLOCK_LENGTH = 26*3+1;
  LengthOfMD5Code4Bit = 32;
  LengthOfMD5Code5Bit = 26;
  LengthOfMD5Code6Bit = 22;
  LengthOfMD5Code8Bit = 16;
  LengthOfSHA1Digest = 20;

{ Create a SHA-1 hash digest (length=20) from "M" }
function SHA1_Digest(const M: RtcByteArray):RtcByteArray; overload;

{ Create a SHA-1 hash digest (length=20) from "M" }
function SHA1_Digest(const M: RtcString):RtcString; overload;

{ Create a standard MD5 hash digest (length=32) from "M",
  encoded using 4 bits per character ('0'-'9' and 'a'-'f') }
function MD5Code4Bit(const M: RtcString):RtcString;

{ Create a 5bit-coded MD5 hash digest (length=26) from "M",
  safe for use in HTML and URLs (only contains alpha-numerics),
  encoded using 5 bits per character ('0'-'9' and 'a'-'z') }
function MD5Code5bit(const M: RtcString):RtcString;

{ Create a 6bit-coded MD5 hash digest (length=22) from "M",
  safe for use in HTML and URLs (does not contain reserved HTML characters)
  but NOT safe for use in File names (case sensitive),
  encoded using 6 bits per character ('0'-'9', 'a'-'z', 'A'-'Z', '@' and '$') }
function MD5Code6bit(const M: RtcString):RtcString;

{ Create a Binary-coded MD5 hash digest (length=16) from "M",
  encoded using 8 bits per character (#0 - #255) }
function MD5Code8bit(const M: RtcString):RtcString;

{ Generate a New, Unique Key.
  RndLen = number of random characters to generate for "salt";
           if 0, "salt" will NOT be used and the Key will be smaller.
  Prefix = prefix to use as part of the main key and "salt".
  Big = if TRUE, returns a bigger Key using 2 GUIDs (79 or 53 characters);
        if FALSE, returns a smaller Key using 1 GUID (53 or 27 chars).
  Result = Key starting with "K" and containing the Key code (0..9, A..Z).

  NOTE: Except for the starting "K" character in the Key and "L" character in the "Lock" code,
           the Key and Lock codes do NOT contain any other instances of "K" or "L" characters.
           This makes it possible to concatenate multiple Keys and Locks into a single string
           ready for transfer and storage, without using additional "envelopes" or formats. }
function rtcGenerateKey(const Prefix:RtcString; RndLen:integer; Big:boolean):RtcString;

{ Calculate Lock from Key.
  Key = Key containing the code generated using "rtcGenerateKey".
  Result = Lock starting with "L" and containing the Lock code (0..9, A..Z) }
function rtcCalculateLock(const Key: RtcString): RtcString;

{ Calculate a Public Key from a Prefix (free text), which can be
  used by all Clients as a Public Channel (for example: "Lobby").
  Public Keys do NOT have a "Lock", they are exactly 26 characters 
  long and do NOT contain any instances of "K" or "L" characters.
  Public Keys do NOT contain random charactes, but are calculated
  to always generate the exact same "Key" from the same "Prefix".  }
function rtcMakePublicKey(const Prefix:RtcString):RtcString;

{ Is this a PUBLIC Key (not used with a Lock) ? }
function rtcKeyPublic(const Key: RtcString):boolean;

{ Is "Key" valid (could have been generated by rtcGenerateKey or rtcMakePublicKey)? }
function rtcKeyValid(const Key: RtcString):boolean;

{ Is "Lock" valid (could have been generated by rtcCalculateLock)? }
function rtcLockValid(const Lock: RtcString):boolean;

{ Does the "Key" match the "Lock"? }
function rtcKeyLockMatch(const Key,Lock:RtcString):boolean;

{ Does the "Key" match a MD5Code8Bit encoded "Lock"? }
function rtcKeyMD5Code8BitLockMatch(const Key,MD5Code8BitLock:RtcString):boolean;

procedure Crypt(var s:RtcString; const key:RtcString);
procedure DeCrypt(var s:RtcString; const key:RtcString);

procedure CryptEx(var s:RtcByteArray; const key:RtcByteArray);
procedure DeCryptEx(var s:RtcByteArray; const key:RtcByteArray);

implementation

procedure Crypt(var s:RtcString; const key:RtcString);
  var
    crypt:TRtcCrypt;
  begin
  crypt:=TRtcCrypt.Create;
  try
    crypt.Key:=key;
    crypt.Crypt(s);
  finally
    RtcFreeAndNil(crypt);
    end;
  end;

procedure DeCrypt(var s:RtcString; const key:RtcString);
  var
    crypt:TRtcCrypt;
  begin
  crypt:=TRtcCrypt.Create;
  try
    crypt.Key:=key;
    crypt.DeCrypt(s);
  finally
    RtcFreeAndNil(crypt);
    end;
  end;

procedure CryptEx(var s:RtcByteArray; const key:RtcByteArray);
  var
    crypt:TRtcCrypt;
  begin
  crypt:=TRtcCrypt.Create;
  try
    crypt.KeyEx:=key;
    crypt.CryptEx(s);
  finally
    RtcFreeAndNil(crypt);
    end;
  end;

procedure DeCryptEx(var s:RtcByteArray; const key:RtcByteArray);
  var
    crypt:TRtcCrypt;
  begin
  crypt:=TRtcCrypt.Create;
  try
    crypt.KeyEx:=key;
    crypt.DeCryptEx(s);
  finally
    RtcFreeAndNil(crypt);
    end;
  end;

{ TRtcCrypt }

constructor TRtcCrypt.Create;
  begin
  inherited;
  SetLength(FCryptKey,0);
  SetLength(FCryptCode,0);
  CInit:=False;
  Init;
  end;

destructor TRtcCrypt.Destroy;
  begin
  try
    SetLength(FCryptKey,0);
    SetLength(FCryptCode,0);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcCrypt.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcCrypt.Init;
  var
    a:integer;
  begin
  if CInit then Exit;

  FCryptCode := Copy(FCryptKey,0,length(FCryptKey)); // Initial encryption key
  CValue:=0;
  CLen:=length(FCryptCode);

  if CLen>0 then
    begin
    // First code = sum of all crypt bytes
    for a:=0 to CLen-1 do
      Inc(CValue,FCryptCode[a]);
      
    if CValue>$FFFF then
      CValue:=(CValue and $FFFF)+(CValue shr 16);
    CErr:=CValue+CLen;

    CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
    if CCode=0 then
      begin
      Inc(CValue,CErr);
      CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
      end;
    CPos:=0;

    CInit:=True;
    end;
  end;

procedure TRtcCrypt.CryptEx(var s: RtcByteArray);
  var
    a:integer;
    c,c2:byte;
  begin
  CInit:=False;
  if CLen>0 then
    begin
    for a:=0 to length(s)-1 do
      begin
      c2:=s[a];
      c:=c2 xor CCode; // Crypt this character

      CValue:=CValue * (1+(c2 and $F)) + (c2 and $F0); // include original character into the code

      if CPos>=CLen then
        CPos:=1
      else
        Inc(CPos);

      Inc(CValue, FCryptCode[CPos-1]);
      if CValue>$FFFF then
        CValue:=(CValue and $FFFF)+(CValue shr 16);

      CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
      if CCode=0 then
        begin
        Inc(CValue,CErr);
        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        end;
      s[a]:=c;
      end;
    end;
  end;

procedure TRtcCrypt.Crypt(var s: RtcString);
  var
    a:integer;
    c,c2:byte;
  begin
  CInit:=False;
  if CLen>0 then
    begin
  {$IFNDEF RTC_BYTESTRING}
    if RTC_STRING_FIXMODE>=rtcStr_FixDown then
      begin
      for a:=1 to length(s) do
        begin
        if Ord(s[a])<=255 then
          c2:=Byte(s[a])
        else
          begin
          c2:=RtcUnicodeToAnsiChar(Word(s[a]));
          if RTC_STRING_CHECK and (c2=RTC_INVALID_CHAR) then
            raise ERtcSystem.Create('TRtcCrypt.Crypt: Source contains Unicode character #'+IntToStr(Ord(s[a]))+' = '+Char(s[a]));
          end;
        c:=c2 xor CCode; // Crypt this character

        CValue:=CValue * (1+(c2 and $F)) + (c2 and $F0); // include original character into the code

        if CPos>=CLen then
          CPos:=1
        else
          Inc(CPos);

        Inc(CValue, FCryptCode[CPos-1]);
        if CValue>$FFFF then
          CValue:=(CValue and $FFFF)+(CValue shr 16);

        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        if CCode=0 then
          begin
          Inc(CValue,CErr);
          CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
          end;
        s[a]:=RtcChar(c);
        end;
      end
    else
  {$ENDIF}
      begin
      for a:=1 to length(s) do
        begin
        c2:=Byte(s[a]);
        c:=c2 xor CCode; // Crypt this character

        CValue:=CValue * (1+(c2 and $F)) + (c2 and $F0); // include original character into the code

        if CPos>=CLen then
          CPos:=1
        else
          Inc(CPos);

        Inc(CValue, FCryptCode[CPos-1]);
        if CValue>$FFFF then
          CValue:=(CValue and $FFFF)+(CValue shr 16);

        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        if CCode=0 then
          begin
          Inc(CValue,CErr);
          CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
          end;
        s[a]:=RtcChar(c);
        end;
      end;
    end;
  end;

procedure TRtcCrypt.DeCryptEx(var s: RtcByteArray);
  var
    a:integer;
    c:byte;
  begin
  CInit:=False;
  if CLen>0 then
    begin
    for a:=0 to length(s)-1 do
      begin
      c:=s[a] xor CCode; // Crypt this character

      CValue:=CValue * (1+(c and $F)) + (c and $F0); // include original character into the code

      if CPos>=CLen then
        CPos:=1
      else
        Inc(CPos);

      Inc(CValue, FCryptCode[CPos-1]);
      if CValue>$FFFF then
        CValue:=(CValue and $FFFF)+(CValue shr 16);

      CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
      if CCode=0 then
        begin
        Inc(CValue,CErr);
        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        end;
      s[a]:=c;
      end;
    end;
  end;

procedure TRtcCrypt.DeCrypt(var s: RtcString);
  var
    a:integer;
    c:byte;
  begin
  CInit:=False;
  if CLen>0 then
    begin
  {$IFNDEF RTC_BYTESTRING}
    if RTC_STRING_FIXMODE>=rtcStr_FixDown then
      begin
      for a:=1 to length(s) do
        begin
        if Ord(s[a])<=255 then
          c:=Byte(s[a])
        else
          begin
          c:=RtcUnicodeToAnsiChar(Word(s[a]));
          if RTC_STRING_CHECK and (c=RTC_INVALID_CHAR) then
            raise ERtcSystem.Create('TRtcCrypt.DeCrypt: Source contains Unicode character #'+IntToStr(Ord(s[a]))+' = '+Char(s[a]));
          end;
        
        c:=c xor CCode; // Crypt this character

        CValue:=CValue * (1+(c and $F)) + (c and $F0); // include original character into the code

        if CPos>=CLen then
          CPos:=1
        else
          Inc(CPos);

        Inc(CValue, FCryptCode[CPos-1]);
        if CValue>$FFFF then
          CValue:=(CValue and $FFFF)+(CValue shr 16);

        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        if CCode=0 then
          begin
          Inc(CValue,CErr);
          CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
          end;

        if RTC_STRING_FIXMODE>=rtcStr_FixUpDown then
          s[a]:=RtcChar( RtcAnsiToUnicodeChar(c) )
        else
          s[a]:=RtcChar(c);
        end;
      end
    else
  {$ENDIF}
      begin
      for a:=1 to length(s) do
        begin
        c:=byte(s[a]) xor CCode; // Crypt this character

        CValue:=CValue * (1+(c and $F)) + (c and $F0); // include original character into the code

        if CPos>=CLen then
          CPos:=1
        else
          Inc(CPos);

        Inc(CValue, FCryptCode[CPos-1]);
        if CValue>$FFFF then
          CValue:=(CValue and $FFFF)+(CValue shr 16);

        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        if CCode=0 then
          begin
          Inc(CValue,CErr);
          CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
          end;
        s[a]:=RtcChar(c);
        end;
      end;
    end;
  end;

procedure TRtcCrypt.SetCryptCodeEx(const Value: RtcByteArray);
  begin
  FCryptKey := Copy(Value,0,length(Value));
  CInit:=False;
  Init;
  end;

procedure TRtcCrypt.SetCryptCode(const Value: RtcString);
  begin
  FCryptKey := RtcStringToBytes(Value);
  CInit:=False;
  Init;
  end;

function TRtcCrypt.GetCryptCode: RtcString;
  begin
  Result:= RtcBytesToString(FCryptKey);
  end;

type
	MD5Count = array[0..1] of Cardinal;
	MD5State = array[0..3] of Cardinal;
	MD5Block = array[0..15] of Cardinal;
	MD5CBits = array[0..7] of byte;
	MD5Digest = array[0..15] of byte;
	MD5Buffer = array[0..63] of byte;
  MD5Array = array[0..RtcMaxLongint] of byte;
	MD5Context = record
		State: MD5State;
		Count: MD5Count;
		Buffer: MD5Buffer;
	end;

var
	PADDING: MD5Buffer = (
		$80, $00, $00, $00, $00, $00, $00, $00,
		$00, $00, $00, $00, $00, $00, $00, $00,
		$00, $00, $00, $00, $00, $00, $00, $00,
		$00, $00, $00, $00, $00, $00, $00, $00,
		$00, $00, $00, $00, $00, $00, $00, $00,
		$00, $00, $00, $00, $00, $00, $00, $00,
		$00, $00, $00, $00, $00, $00, $00, $00,
		$00, $00, $00, $00, $00, $00, $00, $00
	);

// Transform State according to first 64 bytes at Buffer
procedure Transform(Buffer: pointer; var State: MD5State);
  var
  	a, b, c, d: Cardinal;
  	Block: ^MD5Block absolute Buffer;
  begin
	a := State[0];
	b := State[1];
	c := State[2];
	d := State[3];

  inc(a, ((b and c) or ((not b) and d)) + Block[ 0] + $d76aa478); a := (a shl 7) or (a shr (32 - 7)) + b;
	inc(d, ((a and b) or ((not a) and c)) + Block[ 1] + $e8c7b756); d := (d shl 12) or (d shr (32 - 12)) + a;
	inc(c, ((d and a) or ((not d) and b)) + Block[ 2] + $242070db); c := (c shl 17) or (c shr (32 - 17)) + d;
	inc(b, ((c and d) or ((not c) and a)) + Block[ 3] + $c1bdceee); b := (b shl 22) or (b shr (32 - 22)) + c;
	inc(a, ((b and c) or ((not b) and d)) + Block[ 4] + $f57c0faf); a := (a shl 7) or (a shr (32 - 7)) + b;
	inc(d, ((a and b) or ((not a) and c)) + Block[ 5] + $4787c62a); d := (d shl 12) or (d shr (32 - 12)) + a;
	inc(c, ((d and a) or ((not d) and b)) + Block[ 6] + $a8304613); c := (c shl 17) or (c shr (32 - 17)) + d;
	inc(b, ((c and d) or ((not c) and a)) + Block[ 7] + $fd469501); b := (b shl 22) or (b shr (32 - 22)) + c;
	inc(a, ((b and c) or ((not b) and d)) + Block[ 8] + $698098d8); a := (a shl 7) or (a shr (32 - 7)) + b;
	inc(d, ((a and b) or ((not a) and c)) + Block[ 9] + $8b44f7af); d := (d shl 12) or (d shr (32 - 12)) + a;
	inc(c, ((d and a) or ((not d) and b)) + Block[10] + $ffff5bb1); c := (c shl 17) or (c shr (32 - 17)) + d;
	inc(b, ((c and d) or ((not c) and a)) + Block[11] + $895cd7be); b := (b shl 22) or (b shr (32 - 22)) + c;
	inc(a, ((b and c) or ((not b) and d)) + Block[12] + $6b901122); a := (a shl 7) or (a shr (32 - 7)) + b;
	inc(d, ((a and b) or ((not a) and c)) + Block[13] + $fd987193); d := (d shl 12) or (d shr (32 - 12)) + a;
	inc(c, ((d and a) or ((not d) and b)) + Block[14] + $a679438e); c := (c shl 17) or (c shr (32 - 17)) + d;
	inc(b, ((c and d) or ((not c) and a)) + Block[15] + $49b40821); b := (b shl 22) or (b shr (32 - 22)) + c;

	inc(a, ((b and d) or (c and (not d))) + Block[ 1] + $f61e2562); a := (a shl 5) or (a shr (32 - 5)) + b;
	inc(d, ((a and c) or (b and (not c))) + Block[ 6] + $c040b340); d := (d shl 9) or (d shr (32 - 9)) + a;
	inc(c, ((d and b) or (a and (not b))) + Block[11] + $265e5a51); c := (c shl 14) or (c shr (32 - 14)) + d;
	inc(b, ((c and a) or (d and (not a))) + Block[ 0] + $e9b6c7aa); b := (b shl 20) or (b shr (32 - 20)) + c;
	inc(a, ((b and d) or (c and (not d))) + Block[ 5] + $d62f105d); a := (a shl 5) or (a shr (32 - 5)) + b;
	inc(d, ((a and c) or (b and (not c))) + Block[10] + $02441453); d := (d shl 9) or (d shr (32 - 9)) + a;
	inc(c, ((d and b) or (a and (not b))) + Block[15] + $d8a1e681); c := (c shl 14) or (c shr (32 - 14)) + d;
	inc(b, ((c and a) or (d and (not a))) + Block[ 4] + $e7d3fbc8); b := (b shl 20) or (b shr (32 - 20)) + c;
	inc(a, ((b and d) or (c and (not d))) + Block[ 9] + $21e1cde6); a := (a shl 5) or (a shr (32 - 5)) + b;
	inc(d, ((a and c) or (b and (not c))) + Block[14] + $c33707d6); d := (d shl 9) or (d shr (32 - 9)) + a;
	inc(c, ((d and b) or (a and (not b))) + Block[ 3] + $f4d50d87); c := (c shl 14) or (c shr (32 - 14)) + d;
	inc(b, ((c and a) or (d and (not a))) + Block[ 8] + $455a14ed); b := (b shl 20) or (b shr (32 - 20)) + c;
	inc(a, ((b and d) or (c and (not d))) + Block[13] + $a9e3e905); a := (a shl 5) or (a shr (32 - 5)) + b;
	inc(d, ((a and c) or (b and (not c))) + Block[ 2] + $fcefa3f8); d := (d shl 9) or (d shr (32 - 9)) + a;
	inc(c, ((d and b) or (a and (not b))) + Block[ 7] + $676f02d9); c := (c shl 14) or (c shr (32 - 14)) + d;
	inc(b, ((c and a) or (d and (not a))) + Block[12] + $8d2a4c8a); b := (b shl 20) or (b shr (32 - 20)) + c;

	inc(a, (b xor c xor d) + Block[ 5] + $fffa3942);	a := (a shl 4) or (a shr (32 - 4)) + b;
	inc(d, (a xor b xor c) + Block[ 8] + $8771f681);	d := (d shl 11) or (d shr (32 - 11)) + a;
	inc(c, (d xor a xor b) + Block[11] + $6d9d6122);	c := (c shl 16) or (c shr (32 - 16)) + d;
	inc(b, (c xor d xor a) + Block[14] + $fde5380c);	b := (b shl 23) or (b shr (32 - 23)) + c;
	inc(a, (b xor c xor d) + Block[ 1] + $a4beea44);	a := (a shl 4) or (a shr (32 - 4)) + b;
	inc(d, (a xor b xor c) + Block[ 4] + $4bdecfa9);	d := (d shl 11) or (d shr (32 - 11)) + a;
	inc(c, (d xor a xor b) + Block[ 7] + $f6bb4b60);	c := (c shl 16) or (c shr (32 - 16)) + d;
	inc(b, (c xor d xor a) + Block[10] + $bebfbc70);	b := (b shl 23) or (b shr (32 - 23)) + c;
	inc(a, (b xor c xor d) + Block[13] + $289b7ec6);	a := (a shl 4) or (a shr (32 - 4)) + b;
	inc(d, (a xor b xor c) + Block[ 0] + $eaa127fa);	d := (d shl 11) or (d shr (32 - 11)) + a;
	inc(c, (d xor a xor b) + Block[ 3] + $d4ef3085);	c := (c shl 16) or (c shr (32 - 16)) + d;
	inc(b, (c xor d xor a) + Block[ 6] + $04881d05);	b := (b shl 23) or (b shr (32 - 23)) + c;
	inc(a, (b xor c xor d) + Block[ 9] + $d9d4d039);	a := (a shl 4) or (a shr (32 - 4)) + b;
	inc(d, (a xor b xor c) + Block[12] + $e6db99e5);	d := (d shl 11) or (d shr (32 - 11)) + a;
	inc(c, (d xor a xor b) + Block[15] + $1fa27cf8);	c := (c shl 16) or (c shr (32 - 16)) + d;
	inc(b, (c xor d xor a) + Block[ 2] + $c4ac5665);	b := (b shl 23) or (b shr (32 - 23)) + c;

	inc(a, (c xor (b or (not d))) + Block[ 0] + $f4292244); a := (a shl 6) or (a shr (32 - 6)) + b;
	inc(d, (b xor (a or (not c))) + Block[ 7] + $432aff97); d := (d shl 10) or (d shr (32 - 10)) + a;
	inc(c, (a xor (d or (not b))) + Block[14] + $ab9423a7); c := (c shl 15) or (c shr (32 - 15)) + d;
	inc(b, (d xor (c or (not a))) + Block[ 5] + $fc93a039); b := (b shl 21) or (b shr (32 - 21)) + c;
	inc(a, (c xor (b or (not d))) + Block[12] + $655b59c3); a := (a shl 6) or (a shr (32 - 6)) + b;
	inc(d, (b xor (a or (not c))) + Block[ 3] + $8f0ccc92); d := (d shl 10) or (d shr (32 - 10)) + a;
	inc(c, (a xor (d or (not b))) + Block[10] + $ffeff47d); c := (c shl 15) or (c shr (32 - 15)) + d;
	inc(b, (d xor (c or (not a))) + Block[ 1] + $85845dd1); b := (b shl 21) or (b shr (32 - 21)) + c;
	inc(a, (c xor (b or (not d))) + Block[ 8] + $6fa87e4f); a := (a shl 6) or (a shr (32 - 6)) + b;
	inc(d, (b xor (a or (not c))) + Block[15] + $fe2ce6e0); d := (d shl 10) or (d shr (32 - 10)) + a;
	inc(c, (a xor (d or (not b))) + Block[ 6] + $a3014314); c := (c shl 15) or (c shr (32 - 15)) + d;
	inc(b, (d xor (c or (not a))) + Block[13] + $4e0811a1); b := (b shl 21) or (b shr (32 - 21)) + c;
	inc(a, (c xor (b or (not d))) + Block[ 4] + $f7537e82); a := (a shl 6) or (a shr (32 - 6)) + b;
	inc(d, (b xor (a or (not c))) + Block[11] + $bd3af235); d := (d shl 10) or (d shr (32 - 10)) + a;
	inc(c, (a xor (d or (not b))) + Block[ 2] + $2ad7d2bb); c := (c shl 15) or (c shr (32 - 15)) + d;
	inc(b, (d xor (c or (not a))) + Block[ 9] + $eb86d391); b := (b shl 21) or (b shr (32 - 21)) + c;

	inc(State[0], a);
	inc(State[1], b);
	inc(State[2], c);
	inc(State[3], d);
  end;

// -----------------------------------------------------------------------------------------------

// Initialize given Context
procedure rtcMD5Init(var Context: MD5Context);
  begin
	with Context do
    begin
		State[0] := $67452301;
		State[1] := $efcdab89;
		State[2] := $98badcfe;
		State[3] := $10325476;
		Count[0] := 0;
		Count[1] := 0;
    FillChar(Buffer,SizeOf(MD5Buffer),#0);
		// ZeroMemory(@Buffer, SizeOf(MD5Buffer));
  	end;
  end;

// Update given Context to include Length bytes of Input
procedure rtcMD5Update(var Context: MD5Context; const Input:pointer; Length: Cardinal);
  var
    Index: Cardinal;
    PartLen: Cardinal;
    I: Cardinal;
    InData: ^MD5Array absolute Input;
  begin
	with Context do
    begin
		Index := (Count[0] shr 3) and $3f;
		inc(Count[0], Length shl 3);
		if Count[0] < (Length shl 3) then inc(Count[1]);
		inc(Count[1], Length shr 29);
	  end;
	PartLen := 64 - Index;
	if Length >= PartLen then
    begin
    Move(InData[0],Context.Buffer[Index],PartLen);
		// CopyMemory(@Context.Buffer[Index], Input, PartLen);
		Transform(@Context.Buffer, Context.State);
		I := PartLen;
		while I + 63 < Length do
      begin
			Transform(@InData[I], Context.State);
			inc(I, 64);
		  end;
		Index := 0;
	  end
  else
    I := 0;
  Move(InData[I],Context.Buffer[Index],Length-I);
	// CopyMemory(@Context.Buffer[Index], @Input[I], Length - I);
  end;

// Finalize given Context, create Digest and zeroize Context
procedure rtcMD5Final(var Context: MD5Context; var Digest: MD5Digest);
  var
  	Bits: MD5CBits;
  	Index: Cardinal;
  	PadLen: Cardinal;
  begin
  Move(Context.Count,Bits,2*4);
	//CopyMemory(@Bits, @Context.Count, 2*4);
	Index := (Context.Count[0] shr 3) and $3f;
	if Index < 56 then PadLen := 56 - Index else PadLen := 120 - Index;
	rtcMD5Update(Context, @PADDING, PadLen);
	rtcMD5Update(Context, @Bits, 8);
  Move(Context.State,Digest,4*4);
	//CopyMemory(@Digest, @Context.State, 4*4);
  end;

// -----------------------------------------------------------------------------------------------

// Create digest of given Message
function rtcMD5String(const M: RtcString): MD5Digest;
  var
  	Context: MD5Context;
    MB: RtcByteArray;
  begin
  MB := RtcStringToBytes(M);
	rtcMD5Init(Context);
	rtcMD5Update(Context, @MB[0], length(MB));
	rtcMD5Final(Context, Result);
  SetLength(MB,0);
  end;

// Create hex representation of given Digest
function rtcMD5Print(D: MD5Digest): RtcString;
  const
	  Digits: array[0..15] of RtcChar =
		  ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
  var
  	I, J: byte;
  begin
	SetLength(Result,32);
  J:=1;
	for I := 0 to 15 do
    begin
    Result[J]:=Digits[(D[I] shr 4) and $0f];
    Result[J+1]:=Digits[D[I] and $0f];
    Inc(J,2);
    end;
  end;

// -----------------------------------------------------------------------------------------------

function MD5Code4Bit(const M: RtcString): RtcString;
  begin
  Result:=rtcMD5Print(rtcMD5String(M));
  end;

function MD5Code5bit(const M: RtcString):RtcString;
  var
    Digest: MD5Digest;
    a: integer;
    i: uint64;
  const
	  Dig: array[0..31] of RtcChar =
		  ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
       'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
       'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'v', 'x', 'y', 'z');
  begin
	Digest:=rtcMD5String(M);
  SetLength(Result,26);

  i:=0;
  for a:=7 downto 0 do
    i:=(i shl 8) or Byte(Digest[a]);
  for a:=1 to 13 do
    begin
    Result[a]:=Dig[i and $1f];
    i:=i shr 5;
    end;
    
  i:=0;
  for a:=15 downto 8 do
    i:=(i shl 8) or Byte(Digest[a]);
  for a:=14 to 26 do
    begin
    Result[a]:=Dig[i and $1f];
    i:=i shr 5;
    end;
  end;

function MD5Code6bit(const M: RtcString):RtcString;
  var
    Digest: MD5Digest;
    a: integer;
    i: uint64;
  const
	  Dig: array[0..63] of RtcChar =
		  ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
       'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
       'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
       'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D',
       'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
       'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
       'Y', 'Z', '@', '$');
  begin
	Digest:=rtcMD5String(M);
  SetLength(Result,22);

  i:=0;
  for a:=7 downto 0 do
    i:=(i shl 8) or Byte(Digest[a]);
  for a:=1 to 11 do
    begin
    Result[a]:=Dig[i and $3f];
    i:=i shr 6;
    end;

  i:=0;
  for a:=15 downto 8 do
    i:=(i shl 8) or Byte(Digest[a]);
  for a:=12 to 22 do
    begin
    Result[a]:=Dig[i and $3f];
    i:=i shr 6;
    end;
  end;

function MD5Code8bit(const M: RtcString):RtcString;
  var
    Digest: MD5Digest;
    a: integer;
  begin
	Digest:=rtcMD5String(M);
  SetLength(Result,16);

  for a:=1 to 16 do
    Result[a]:=RtcChar(Digest[a-1]);
  end;

{ Generate a new, unique Key }
function rtcGenerateKey(const Prefix:RtcString; RndLen:integer; Big:boolean):RtcString;
  var
    GUID: TGUID;
    GUIDStr1, GUIDStr2: RtcString;
    RandStr: RtcString;
    i: integer;
  {$IFDEF RTC_RSA}
    rnd: TRtcISAAC;
  {$ENDIF}
  const
	  Dig: array[0..31] of RtcChar =
		  ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
       'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
       'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'v', 'x', 'y', 'z');
  begin
  Result:='';
  if CreateGuid(GUID) <> S_OK then Exit;
  GUIDStr1 := RtcString( IntToHex(GUID.D1,8)+
                       IntToHex(GUID.D2,4)+IntToHex(GUID.D3,4)+
                       IntToHex(GUID.D4[0],2)+IntToHex(GUID.D4[1],2)+
                       IntToHex(GUID.D4[2],2)+IntToHex(GUID.D4[3],2)+
                       IntToHex(GUID.D4[4],2)+IntToHex(GUID.D4[5],2)+
                       IntToHex(GUID.D4[6],2)+IntToHex(GUID.D4[7],2) );
  RandStr:='';
  if RndLen>0 then
    begin
  {$IFDEF RTC_RSA}
    rnd:=TRtcISAAC.Create(True);
    try
      for i:=1 to RndLen do
        RandStr:=RandStr+Dig[rnd.random(32)];
    finally
      rnd.Free;
      end;
  {$ELSE}
    for i:=1 to RndLen do
      RandStr:=RandStr+Dig[random(32)];
  {$ENDIF}
    end;

  if Big then
    begin
    if CreateGuid(GUID) <> S_OK then Exit;
    GUIDStr2 := RtcString( IntToHex(GUID.D1,8)+
                         IntToHex(GUID.D2,4)+IntToHex(GUID.D3,4)+
                         IntToHex(GUID.D4[0],2)+IntToHex(GUID.D4[1],2)+
                         IntToHex(GUID.D4[2],2)+IntToHex(GUID.D4[3],2)+
                         IntToHex(GUID.D4[4],2)+IntToHex(GUID.D4[5],2)+
                         IntToHex(GUID.D4[6],2)+IntToHex(GUID.D4[7],2) );
    if RndLen>0 then
      Result:='K'+
              Upper_Case( MD5Code5Bit(GUIDStr1+Prefix) +
                         MD5Code5Bit(GUIDStr2+Prefix) +
                         MD5Code5Bit(RandStr+GUIDStr1+GUIDStr2+RandStr) )
    else
      Result:='K'+
              Upper_Case( MD5Code5Bit(GUIDStr1+Prefix) +
                         MD5Code5Bit(GUIDStr2+Prefix) );
    end
  else
    begin
    if RndLen>0 then
      Result:='K'+
              UpperCase( MD5Code5Bit(GUIDStr1+Prefix) +
                         MD5Code5Bit(RandStr+GUIDStr1+RandStr) )
    else
      Result:='K'+
              UpperCase( MD5Code5Bit(GUIDStr1+Prefix) );
    end;
  end;

function rtcMakePublicKey(const Prefix:RtcString):RtcString;
  begin
  Result:=UpperCase( MD5Code5Bit(Prefix) );
  end;

{ Calculate a Lock from the Key }
function rtcCalculateLock(const Key: RtcString): RtcString;
  var
    i:integer;
    lKey:RtcString;
  begin
  Result:='';
  if (length(Key)<>26*3+1) and
     (length(Key)<>26*2+1) and
     (length(Key)<>26*1+1) then Exit;
  if Up_Case(Key[1])<>'K' then Exit;

  SetLength(lKey,length(Key)-1);
  for i:=2 to length(Key) do
    case Key[i] of
      '0'..'9': lKey[i-1]:=Key[i];
      'a'..'j', // skip K and L
      'm'..'p', // skip Q
      'r'..'v', // skip W
      'x'..'z': lKey[i-1]:=Key[i];
      'A'..'J', // skip K and L
      'M'..'P', // skip Q
      'R'..'V', // skip W
      'X'..'Z': lKey[i-1]:=RtcChar(Ord(Key[i])-Ord('A')+Ord('a'));
      else Exit;
    end;

  if length(lKey)=26*3 then
    Result:='L' + UpperCase (
                MD5Code5Bit( MD5Code4Bit(Copy(lKey, 1,       13*3)) +
                             MD5Code5Bit(Copy(lKey, 1+ 13*1, 13*3)) +
                             MD5Code6Bit(Copy(lKey, 1+ 13*2, 13*3)) +
                             MD5Code8Bit(Copy(lKey, 1+ 13*3, 13*3)) ) +
                MD5Code5Bit( lKey ) +
                Copy(lKey,1 + 13*4,13*2) )
  else if length(lKey)=26*2 then
    Result:='L' + UpperCase (
                MD5Code5Bit( MD5Code4Bit(Copy(lKey, 1,       13*2)) +
                             MD5Code6Bit(Copy(lKey, 1+ 13*1, 13*2)) +
                             MD5Code8Bit(Copy(lKey, 1+ 13*2, 13*2)) +
                             lKey ) +
                Copy(lKey,1 + 13*3,13) )
  else if length(lKey)=26*1 then
    Result:='L' + UpperCase (
                MD5Code5Bit( MD5Code4Bit(Copy(lKey, 1,    19)) +
                             MD5Code6Bit(Copy(lKey, 1+ 7, 19)) +
                             lKey ) );
  end;

function rtcKeyPublic(const Key: RtcString):boolean;
  begin
  Result:=(length(Key)=26) and rtcKeyValid(Key);
  end;

function rtcKeyValid(const Key: RtcString):boolean;
  var
    i:integer;
    ok:boolean;
  begin
  Result:=False;
  ok:=False;
  if (length(Key)=26) then
    begin
    case Key[1] of
      '0'..'9',
      'a'..'j', // skip K and L
      'm'..'p', // skip Q
      'r'..'v', // skip W
      'x'..'z',
      'A'..'J', // skip K and L
      'M'..'P', // skip Q
      'R'..'V', // skip W
      'X'..'Z': ok:=True;
      else Exit;
      end;
    end
  else
    begin
    if (length(Key)<>26*3+1) and
       (length(Key)<>26*2+1) and
       (length(Key)<>26*1+1) then Exit;
    if Up_Case(Key[1])<>'K' then Exit;
    end;
  for i:=2 to length(Key) do
    case Key[i] of
      '0'..'9',
      'a'..'j', // skip K and L
      'm'..'p', // skip Q
      'r'..'v', // skip W
      'x'..'z',
      'A'..'J', // skip K and L
      'M'..'P', // skip Q
      'R'..'V', // skip W
      'X'..'Z': ok:=True;
      else Exit;
    end;
  Result:=ok;
  end;

function rtcLockValid(const Lock: RtcString):boolean;
  var
    i:integer;
    ok:boolean;
  begin
  Result:=False;
  if (length(Lock)<>26*3+1) and
     (length(Lock)<>13*3+1) and
     (length(Lock)<>13*2+1) then Exit;
  if Up_Case(Lock[1])<>'L' then Exit;
  ok:=False;
  for i:=2 to length(Lock) do
    case Lock[i] of
      '0'..'9',
      'a'..'j', // skip K and L
      'm'..'p', // skip Q
      'r'..'v', // skip W
      'x'..'z',
      'A'..'J', // skip K and L
      'M'..'P', // skip Q
      'R'..'V', // skip W
      'X'..'Z': ok:=True;
      else Exit;
    end;
  Result:=ok;
  end;

function rtcKeyLockMatch(const Key,Lock:RtcString):boolean;
  begin
  if rtcKeyPublic(Key) then
    Result:=(Lock=Key)
  else if not (rtcKeyValid(Key) and rtcLockValid(Lock)) then
    Result:=False
  else
    Result := rtcCalculateLock(Key) = Lock;
  end;

function rtcKeyMD5Code8BitLockMatch(const Key,MD5Code8BitLock:RtcString):boolean;
  begin
  if length(MD5Code8BitLock)<>LengthOfMD5Code8Bit then
    Result:=False
  else
    Result := MD5Code8Bit(rtcCalculateLock(Key)) = MD5Code8BitLock;
  end;

type
  TSHA1Word = Cardinal;
  TSHA1Buf = array[0..4] of TSHA1Word;
  TSHA1In = packed record
    case Integer of
      0: (w: array[0..15] of TSHA1Word);
      1: (b: array[0..63] of Byte);
    end;

  TSHA1WArray = array[0..79] of TSHA1Word;

  TSHA1Context = record
    buf: TSHA1Buf;
    bytes: array[0..1] of TSHA1Word;
    in_: TSHA1In;
    W: TSHA1WArray;
  end;

  TSHA1Digest = array[0..19] of Byte;

procedure SHA1Transform(var buf: TSHA1Buf; const in_: TSHA1In; var W: TSHA1WArray); forward;

function ByteSwap(const X: TSHA1Word): TSHA1Word;
begin
  Result :=
    (X shl 24) or
    ((X and $FF00) shl 8) or
    ((X and $FF0000) shr 8) or
    (X shr 24);
end;

(*
* Start SHA-1 accumulation.  Set byte count to 0 and buffer to mysterious
* initialization constants.
*)
procedure SHA1Init(var ctx: TSHA1Context);
begin
  ctx.buf[0] := TSHA1Word($67452301);
  ctx.buf[1] := TSHA1Word($efcdab89);
  ctx.buf[2] := TSHA1Word($98badcfe);
  ctx.buf[3] := TSHA1Word($10325476);
  ctx.buf[4] := TSHA1Word($c3d2e1f0);

  ctx.bytes[0] := 0;
  ctx.bytes[1] := 0;
end;

(*
* Update context to reflect the concatenation of another buffer full
* of bytes.
*)
procedure SHA1Update(var ctx: TSHA1Context; const buffer; len: Cardinal);
var
  buf: ^Byte;
  t: TSHA1Word;
begin
  buf := @buffer;

  { Update byte count }
  t := ctx.bytes[0];
  Inc(ctx.bytes[0], len);
  if Cardinal(ctx.bytes[0]) < Cardinal(t) then
    Inc(ctx.bytes[1]); { Carry from low to high }

  t := 64 - (t and $3f); { Space available in ctx.in (at least 1) }
  if Cardinal(t) > Cardinal(len) then begin
    Move(buf^, ctx.in_.b[64 - t], len);
    Exit;
  end;
  { First chunk is an odd size }
  Move(buf^, ctx.in_.b[64 - t], t);
  SHA1Transform(ctx.buf, ctx.in_, ctx.W);
  Inc(buf, t);
  Dec(len, t);

  { Process data in 64-byte chunks }
  while Cardinal(len) >= Cardinal(64) do begin
    Move(buf^, ctx.in_, 64);
    SHA1Transform(ctx.buf, ctx.in_, ctx.W);
    Inc(buf, 64);
    Dec(len, 64);
  end;

  { Handle any remaining bytes of data. }
  Move(buf^, ctx.in_, len);
end;

(*
* Final wrapup - pad to 64-byte boundary with the bit pattern
* 1 0* (64-bit count of bits processed, MSB-first)
*)
function SHA1Final(var ctx: TSHA1Context): TSHA1Digest;
var
  count, i: Integer;
  p: ^Byte;
begin
  count := ctx.bytes[0] and $3f; { Number of bytes in ctx.in }
  p := @ctx.in_;
  Inc(p, count);

  { Set the first char of padding to 0x80.  There is always room. }
  p^ := $80;
  Inc(p);

  { Bytes of padding needed to make 56 bytes (-8..55) }
  count := 56 - 1 - count;

  if count < 0 then begin { Padding forces an extra block }
    FillChar(p^, count + 8, 0);
    SHA1Transform(ctx.buf, ctx.in_, ctx.W);
    p := @ctx.in_;
    count := 56;
  end;
  FillChar(p^, count, 0);

  { Append length in bits and transform }
  ctx.in_.w[15] := ByteSwap(ctx.bytes[0] shl 3);
  ctx.in_.w[14] := ByteSwap((ctx.bytes[1] shl 3) or (ctx.bytes[0] shr 29));
  SHA1Transform(ctx.buf, ctx.in_, ctx.W);

  for i := 0 to High(ctx.buf) do
    ctx.buf[i] := ByteSwap(ctx.buf[i]);
  Move(ctx.buf, Result, SizeOf(Result));
  FillChar(ctx, SizeOf(ctx), 0); { In case it's sensitive }
end;

(*
* The core of the SHA-1 algorithm, this alters an existing SHA-1 hash to
* reflect the addition of 16 longwords of new data.  SHA1Update blocks
* the data and converts bytes into longwords for this routine.
*)
procedure SHA1Transform(var buf: TSHA1Buf; const in_: TSHA1In; var W: TSHA1WArray);
const
  K1 = $5A827999;
  K2 = $6ED9EBA1;
  K3 = $8F1BBCDC;
  K4 = $CA62C1D6;
var
  t: Integer;
  temp, A, B, C, D, E: TSHA1Word;
begin
  for t := 0 to 15 do begin
    { ByteSwap inlined: }
    temp := in_.w[t];
    W[t] := (temp shl 24) or
            ((temp and $FF00) shl 8) or
            ((temp and $FF0000) shr 8) or
            (temp shr 24);
  end;

  for t := 16 to 79 do begin
    temp := W[t-3] xor W[t-8] xor W[t-14] xor W[t-16];
    W[t] := (temp shl 1) or (temp shr (32-1));
  end;

  A := buf[0];
  B := buf[1];
  C := buf[2];
  D := buf[3];
  E := buf[4];

  for t := 0 to 19 do begin
    temp := ((A shl 5) or (A shr (32-5))) +
            (D xor (B and (C xor D))) + E + W[t] + K1;
    E := D;
    D := C;
    C := (B shl 30) or (B shr (32-30));
    B := A;
    A := temp;
  end;

  for t := 20 to 39 do begin
    temp := ((A shl 5) or (A shr (32-5))) + (B xor C xor D) + E + W[t] + K2;
    E := D;
    D := C;
    C := (B shl 30) or (B shr (32-30));
    B := A;
    A := temp;
  end;

  for t := 40 to 59 do begin
    temp := ((A shl 5) or (A shr (32-5))) +
            ((B and C) or (B and D) or (C and D)) + E + W[t] + K3;
    E := D;
    D := C;
    C := (B shl 30) or (B shr (32-30));
    B := A;
    A := temp;
  end;

  for t := 60 to 79 do begin
    temp := ((A shl 5) or (A shr (32-5))) + (B xor C xor D) + E + W[t] + K4;
    E := D;
    D := C;
    C := (B shl 30) or (B shr (32-30));
    B := A;
    A := temp;
  end;

  Inc(buf[0], A);
  Inc(buf[1], B);
  Inc(buf[2], C);
  Inc(buf[3], D);
  Inc(buf[4], E);
end;

{ Create a SHA-1 hash digest (length=20) from "M" }
function SHA1_Digest(const M: RtcByteArray):RtcByteArray;
  var
    Context: TSHA1Context;
    Digest: TSHA1Digest;
  begin
  SHA1Init(Context);
  SHA1Update(Context, M[0], length(M));
  Digest := SHA1Final(Context);
  SetLength(Result,SizeOf(Digest));
  Move(Digest,Result[0],length(Result));
  end;

{ Create a SHA-1 hash digest (length=20) from "M" }
function SHA1_Digest(const M: RtcString):RtcString;
  begin
  Result:=RtcBytesToString(SHA1_Digest(RtcStringToBytes(M)));
  end;

{$IFNDEF RTC_NORSA}

(************ REFERENCES, COPYRIGHTS AND LICENSE AGREEMENTS ******************

  REFERENCES   :  [1] LibTomMath 0.30+ by Tom St Denis
                  [2] MPI by M.J. Fromberger
                   -  RFC 2313 - PKCS #1: RSA Encryption Version 1.5 and
                   -  RFC 3447 - PKCS #1: RSA Encryption Version 2.1 available
                      online from http://tools.ietf.org/html/rfc2313 and
                      http://tools.ietf.org/html/rfc3447
                  [3] Knuth, D.E.: The Art of computer programming. Vol 2
                      Seminumerical Algorithms, 3rd ed., 1998
                  [4] Forster, O.: Algorithmische Zahlentheorie, 1996
                  [5] (HAC) Menezes,A., von Oorschot,P., Vanstone, S: Handbook of
                      Applied Cryptography, 1996, www.cacr.math.uwaterloo.ca/hac
                  [6] R. P. Brent, Factor: an integer factorization program for
                      the IBM PC, Report TR-CS-89-23, October 1989, 7 pp.
                      http://maths.anu.edu.au/~brent/pub/pub117.html
                      http://maths.anu.edu.au/~brent/ftp/rpb117/rpb117.exe
                  [7] P. Ribenboim: The New Book of Prime Number Records, 3rd ed., 1995.
                  [8] Marcel Martin: NX - Numerics library of multiprecision
                      numbers for Delphi and Free Pascal, 2006-2009
                      www.ellipsa.eu/public/nx/index.html
                 [10] R.Crandall, C.Pomerance: Prime Numbers, A Computational
                      Perspective, 2nd ed., 2005
                 [12] PARI/GP at http://pari.math.u-bordeaux.fr/
                 [24] H. Cohen, A Course in Computational Algebraic Number Theory
                      4th printing, 2000
                 [25] IEEE P1363/Draft. Standard Specifications for Public Key Cryptography.
                      Annex A (informative). Number-Theoretic Background.
                      http://grouper.ieee.org/groups/1363/P1363/draft.html
                 [26] A. Adler, J.E. Coury: The Theory of Numbers, 1995
                 [30] J. v. zur Gathen, J. Gerhard, Modern computer algebra, 2nd ed., 2003
                      http://math-www.uni-paderborn.de/mca/
                 [32] S.C. Lindhurst, Computing Roots in Finite Fields and Groups, with a
                      Jaunt through Sums of Digits, University of Wisconsin, Madison 1997
                      http://scott.lindhurst.com/papers/thesis.ps.gz
                 [39] D.H. Lehmer: On the exact number of primes less than a given limit,
                      Illinois Journal of Mathematics, vol. 3, pp. 381-388, 1959;
                      available from http://projecteuclid.org/euclid.ijm/1255455259
                 [40] H. Riesel, Prime Numbers and Computer Methods for Factorization,
                      Vol. 126 of Progress in Mathematics, Boston, 2nd ed. 1994.
                      Paperback reprint 2012 in Modern Birkh„user Classics Series.
                 [41] http://burtleburtle.net/bob/rand/isaacafa.html
                      (ISAAC: a fast cryptographic random number generator)

---------------------------------------------------------------------------

 (C) Copyright 2004-2014 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.

---------------------------------------------------------------------------

LibTomMath 0.30+, multiple-precision integer library,
by Tom St Denis

  LibTomMath is a library that provides multiple-precision
  integer arithmetic as well as number theoretic functionality.

  The library was designed directly after the MPI library by
  Michael Fromberger but has been written from scratch with
  additional optimizations in place.

  The library is free for all purposes without any express
  guarantee it works.

---------------------------------------------------------------------------

MPI 1.8.6+,  Arbitrary precision integer arithmetic library
by Michael J. Fromberger 

  Copyright (C) 1998 Michael J. Fromberger, All Rights Reserved

  This software is in the public domain.  It is entirely free, and you
  may use it and/or redistribute it for whatever purpose you choose;
  however, as free software, it is provided without warranty of any
  kind, not even the implied warranty of merchantability or fitness for
  a particular purpose.

---------------------------------------------------------------------------

IMATH 1.1+, Arbitrary precision rational arithmetic routines.
by Michael J. Fromberger

  IMath is Copyright 2002-2007 Michael J. Fromberger
  You may use it subject to the following Licensing Terms:

  Permission is hereby granted, free of charge, to any person obtaining
  a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

----------------------------------------------------------------------------*)

const
  mp_allocprec = 16;
  mp_allocmask = mp_allocprec - 1;

type
  mp_word   = int64;              {type that holds two MP digits }
  pmp_digit = ^mp_digit;               {pointer to an MP digit      }
  pmp_int   = ^mp_int;                 {pointer to an MP integer    }

const
  RSA_MINSIZE = 11; {minimum octet size of modulus}

  DIGIT_BIT = 31;               {number of bits of an MP digit }
  MP_DIGIT_BIT  = DIGIT_BIT;    {Alias for DIGIT_BIT   }
  MP_DIG1       = mp_digit(1);  {Const 1 as mp_digit   }
            // => mp_digit(MP_DIG1) shl DIGIT_BIT - MP_DIG1;
  MP_MASK       = $7FFFFFFF;   {Mask for mp_digits in base type}
  MP_INV_MASK   = not MP_MASK;  {inverted digit mask}
  MP_DIGIT_MAX  = MP_MASK;      {largest mp_digit}
  MP_MAXBIT     = Longint(MAXDigits)*MP_DIGIT_BIT;   {Maximum possible bit number}

  MP_LT         = -1;  {constant for "less than"    }
  MP_EQ         =  0;  {constant for "equal to"     }
  MP_GT         =  1;  {constant for "greater than" }

  MP_ZPOS    = 0;  {constant for "positive"}
  MP_NEG     = 1;  {constant for "negative"}
  MP_MAGIC   = $BAF5;  {Magic number for mp_int magic field}

  mp_max_small = mp_digit(MP_DIGIT_MAX and $7FFF); {max. tested small factor}

  mp_mul_cutoff = 16;   {Karatsuba multiplication cutoff}
  mp_sqr_cutoff = 32;   {Karatsuba square cutoff        }
  mp_bz_cutoff  = 32;   {Burnikel-Ziegler cutoff        }
  mp_sqrt_cutoff = 4;       {Karatsuba square root cutoff   }
  mp_t3m_cutoff  = 2*mp_mul_cutoff;
  mp_t3s_cutoff  = 2*mp_sqr_cutoff;
  mpf_lna_cutoff = Longint(DIGIT_BIT) * mp_mul_cutoff;

const
  {Number of 16 bit primes}
  NumPrimes16 = 6542;
  { Bit mask array for 16 bit primes. Only odd integers are represented, ie
    IsPrime16(i) := (i=2) or (pbits16[i shr 4] and pmask16[i and $f] <> 0); }
  pbits16: array[0..4095] of byte = {#Z+}(
    $6e,$cb,$b4,$64,$9a,$12,$6d,$81,$32,$4c,$4a,$86,$0d,$82,$96,$21,
    $c9,$34,$04,$5a,$20,$61,$89,$a4,$44,$11,$86,$29,$d1,$82,$28,$4a,
    $30,$40,$42,$32,$21,$99,$34,$08,$4b,$06,$25,$42,$84,$48,$8a,$14,
    $05,$42,$30,$6c,$08,$b4,$40,$0b,$a0,$08,$51,$12,$28,$89,$04,$65,
    $98,$30,$4c,$80,$96,$44,$12,$80,$21,$42,$12,$41,$c9,$04,$21,$c0,
    $32,$2d,$98,$00,$00,$49,$04,$08,$81,$96,$68,$82,$b0,$25,$08,$22,
    $48,$89,$a2,$40,$59,$26,$04,$90,$06,$40,$43,$30,$44,$92,$00,$69,
    $10,$82,$08,$08,$a4,$0d,$41,$12,$60,$c0,$00,$24,$d2,$22,$61,$08,
    $84,$04,$1b,$82,$01,$d3,$10,$01,$02,$a0,$44,$c0,$22,$60,$91,$14,
    $0c,$40,$a6,$04,$d2,$94,$20,$09,$94,$20,$52,$00,$08,$10,$a2,$4c,
    $00,$82,$01,$51,$10,$08,$8b,$a4,$25,$9a,$30,$44,$81,$10,$4c,$03,
    $02,$25,$52,$80,$08,$49,$84,$20,$50,$32,$00,$18,$a2,$40,$11,$24,
    $28,$01,$84,$01,$01,$a0,$41,$0a,$12,$45,$00,$36,$08,$00,$26,$29,
    $83,$82,$61,$c0,$80,$04,$10,$10,$6d,$00,$22,$48,$58,$26,$0c,$c2,
    $10,$48,$89,$24,$20,$58,$20,$45,$88,$24,$00,$19,$02,$25,$c0,$10,
    $68,$08,$14,$01,$ca,$32,$28,$80,$00,$04,$4b,$26,$00,$13,$90,$60,
    $82,$80,$25,$d0,$00,$01,$10,$32,$0c,$43,$86,$21,$11,$00,$08,$43,
    $24,$04,$48,$10,$0c,$90,$92,$00,$43,$20,$2d,$00,$06,$09,$88,$24,
    $40,$c0,$32,$09,$09,$82,$00,$53,$80,$08,$80,$96,$41,$81,$00,$40,
    $48,$10,$48,$08,$96,$48,$58,$20,$29,$c3,$80,$20,$02,$94,$60,$92,
    $00,$20,$81,$22,$44,$10,$a0,$05,$40,$90,$01,$49,$20,$04,$0a,$00,
    $24,$89,$34,$48,$13,$80,$2c,$c0,$82,$29,$00,$24,$45,$08,$00,$08,
    $98,$36,$04,$52,$84,$04,$d0,$04,$00,$8a,$90,$44,$82,$32,$65,$18,
    $90,$00,$0a,$02,$01,$40,$02,$28,$40,$a4,$04,$92,$30,$04,$11,$86,
    $08,$42,$00,$2c,$52,$04,$08,$c9,$84,$60,$48,$12,$09,$99,$24,$44,
    $00,$24,$00,$03,$14,$21,$00,$10,$01,$1a,$32,$05,$88,$20,$40,$40,
    $06,$09,$c3,$84,$40,$01,$30,$60,$18,$02,$68,$11,$90,$0c,$02,$a2,
    $04,$00,$86,$29,$89,$14,$24,$82,$02,$41,$08,$80,$04,$19,$80,$08,
    $10,$12,$68,$42,$a4,$04,$00,$02,$61,$10,$06,$0c,$10,$00,$01,$12,
    $10,$20,$03,$94,$21,$42,$12,$65,$18,$94,$0c,$0a,$04,$28,$01,$14,
    $29,$0a,$a4,$40,$d0,$00,$40,$01,$90,$04,$41,$20,$2d,$40,$82,$48,
    $c1,$20,$00,$10,$30,$01,$08,$24,$04,$59,$84,$24,$00,$02,$29,$82,
    $00,$61,$58,$02,$48,$81,$16,$48,$10,$00,$21,$11,$06,$00,$ca,$a0,
    $40,$02,$00,$04,$91,$b0,$00,$42,$04,$0c,$81,$06,$09,$48,$14,$25,
    $92,$20,$25,$11,$a0,$00,$0a,$86,$0c,$c1,$02,$48,$00,$20,$45,$08,
    $32,$00,$98,$06,$04,$13,$22,$00,$82,$04,$48,$81,$14,$44,$82,$12,
    $24,$18,$10,$40,$43,$80,$28,$d0,$04,$20,$81,$24,$64,$d8,$00,$2c,
    $09,$12,$08,$41,$a2,$00,$00,$02,$41,$ca,$20,$41,$c0,$10,$01,$18,
    $a4,$04,$18,$a4,$20,$12,$94,$20,$83,$a0,$40,$02,$32,$44,$80,$04,
    $00,$18,$00,$0c,$40,$86,$60,$8a,$00,$64,$88,$12,$05,$01,$82,$00,
    $4a,$a2,$01,$c1,$10,$61,$09,$04,$01,$88,$00,$60,$01,$b4,$40,$08,
    $06,$01,$03,$80,$08,$40,$94,$04,$8a,$20,$29,$80,$02,$0c,$52,$02,
    $01,$42,$84,$00,$80,$84,$64,$02,$32,$48,$00,$30,$44,$40,$22,$21,
    $00,$02,$08,$c3,$a0,$04,$d0,$20,$40,$18,$16,$40,$40,$00,$28,$52,
    $90,$08,$82,$14,$01,$18,$10,$08,$09,$82,$40,$0a,$a0,$20,$93,$80,
    $08,$c0,$00,$20,$52,$00,$05,$01,$10,$40,$11,$06,$0c,$82,$00,$00,
    $4b,$90,$44,$9a,$00,$28,$80,$90,$04,$4a,$06,$09,$43,$02,$28,$00,
    $34,$01,$18,$00,$65,$09,$80,$44,$03,$00,$24,$02,$82,$61,$48,$14,
    $41,$00,$12,$28,$00,$34,$08,$51,$04,$05,$12,$90,$28,$89,$84,$60,
    $12,$10,$49,$10,$26,$40,$49,$82,$00,$91,$10,$01,$0a,$24,$40,$88,
    $10,$4c,$10,$04,$00,$50,$a2,$2c,$40,$90,$48,$0a,$b0,$01,$50,$12,
    $08,$00,$a4,$04,$09,$a0,$28,$92,$02,$00,$43,$10,$21,$02,$20,$41,
    $81,$32,$00,$08,$04,$0c,$52,$00,$21,$49,$84,$20,$10,$02,$01,$81,
    $10,$48,$40,$22,$01,$01,$84,$69,$c1,$30,$01,$c8,$02,$44,$88,$00,
    $0c,$01,$02,$2d,$c0,$12,$61,$00,$a0,$00,$c0,$30,$40,$01,$12,$08,
    $0b,$20,$00,$80,$94,$40,$01,$84,$40,$00,$32,$00,$10,$84,$00,$0b,
    $24,$00,$01,$06,$29,$8a,$84,$41,$80,$10,$08,$08,$94,$4c,$03,$80,
    $01,$40,$96,$40,$41,$20,$20,$50,$22,$25,$89,$a2,$40,$40,$a4,$20,
    $02,$86,$28,$01,$20,$21,$4a,$10,$08,$00,$14,$08,$40,$04,$25,$42,
    $02,$21,$43,$10,$04,$92,$00,$21,$11,$a0,$4c,$18,$22,$09,$03,$84,
    $41,$89,$10,$04,$82,$22,$24,$01,$14,$08,$08,$84,$08,$c1,$00,$09,
    $42,$b0,$41,$8a,$02,$00,$80,$36,$04,$49,$a0,$24,$91,$00,$00,$02,
    $94,$41,$92,$02,$01,$08,$06,$08,$09,$00,$01,$d0,$16,$28,$89,$80,
    $60,$00,$00,$68,$01,$90,$0c,$50,$20,$01,$40,$80,$40,$42,$30,$41,
    $00,$20,$25,$81,$06,$40,$49,$00,$08,$01,$12,$49,$00,$a0,$20,$18,
    $30,$05,$01,$a6,$00,$10,$24,$28,$00,$02,$20,$c8,$20,$00,$88,$12,
    $0c,$90,$92,$00,$02,$26,$01,$42,$16,$49,$00,$04,$24,$42,$02,$01,
    $88,$80,$0c,$1a,$80,$08,$10,$00,$60,$02,$94,$44,$88,$00,$69,$11,
    $30,$08,$12,$a0,$24,$13,$84,$00,$82,$00,$65,$c0,$10,$28,$00,$30,
    $04,$03,$20,$01,$11,$06,$01,$c8,$80,$00,$c2,$20,$08,$10,$82,$0c,
    $13,$02,$0c,$52,$06,$40,$00,$b0,$61,$40,$10,$01,$98,$86,$04,$10,
    $84,$08,$92,$14,$60,$41,$80,$41,$1a,$10,$04,$81,$22,$40,$41,$20,
    $29,$52,$00,$41,$08,$34,$60,$10,$00,$28,$01,$10,$40,$00,$84,$08,
    $42,$90,$20,$48,$04,$04,$52,$02,$00,$08,$20,$04,$00,$82,$0d,$00,
    $82,$40,$02,$10,$05,$48,$20,$40,$99,$00,$00,$01,$06,$24,$c0,$00,
    $68,$82,$04,$21,$12,$10,$44,$08,$04,$00,$40,$a6,$20,$d0,$16,$09,
    $c9,$24,$41,$02,$20,$0c,$09,$92,$40,$12,$00,$00,$40,$00,$09,$43,
    $84,$20,$98,$02,$01,$11,$24,$00,$43,$24,$00,$03,$90,$08,$41,$30,
    $24,$58,$20,$4c,$80,$82,$08,$10,$24,$25,$81,$06,$41,$09,$10,$20,
    $18,$10,$44,$80,$10,$00,$4a,$24,$0d,$01,$94,$28,$80,$30,$00,$c0,
    $02,$60,$10,$84,$0c,$02,$00,$09,$02,$82,$01,$08,$10,$04,$c2,$20,
    $68,$09,$06,$04,$18,$00,$00,$11,$90,$08,$0b,$10,$21,$82,$02,$0c,
    $10,$b6,$08,$00,$26,$00,$41,$02,$01,$4a,$24,$21,$1a,$20,$24,$80,
    $00,$44,$02,$00,$2d,$40,$02,$00,$8b,$94,$20,$10,$00,$20,$90,$a6,
    $40,$13,$00,$2c,$11,$86,$61,$01,$80,$41,$10,$02,$04,$81,$30,$48,
    $48,$20,$28,$50,$80,$21,$8a,$10,$04,$08,$10,$09,$10,$10,$48,$42,
    $a0,$0c,$82,$92,$60,$c0,$20,$05,$d2,$20,$40,$01,$00,$04,$08,$82,
    $2d,$82,$02,$00,$48,$80,$41,$48,$10,$00,$91,$04,$04,$03,$84,$00,
    $c2,$04,$68,$00,$00,$64,$c0,$22,$40,$08,$32,$44,$09,$86,$00,$91,
    $02,$28,$01,$00,$64,$48,$00,$24,$10,$90,$00,$43,$00,$21,$52,$86,
    $41,$8b,$90,$20,$40,$20,$08,$88,$04,$44,$13,$20,$00,$02,$84,$60,
    $81,$90,$24,$40,$30,$00,$08,$10,$08,$08,$02,$01,$10,$04,$20,$43,
    $b4,$40,$90,$12,$68,$01,$80,$4c,$18,$00,$08,$c0,$12,$49,$40,$10,
    $24,$1a,$00,$41,$89,$24,$4c,$10,$00,$04,$52,$10,$09,$4a,$20,$41,
    $48,$22,$69,$11,$14,$08,$10,$06,$24,$80,$84,$28,$00,$10,$00,$40,
    $10,$01,$08,$26,$08,$48,$06,$28,$00,$14,$01,$42,$84,$04,$0a,$20,
    $00,$01,$82,$08,$00,$82,$24,$12,$04,$40,$40,$a0,$40,$90,$10,$04,
    $90,$22,$40,$10,$20,$2c,$80,$10,$28,$43,$00,$04,$58,$00,$01,$81,
    $10,$48,$09,$20,$21,$83,$04,$00,$42,$a4,$44,$00,$00,$6c,$10,$a0,
    $44,$48,$80,$00,$83,$80,$48,$c9,$00,$00,$00,$02,$05,$10,$b0,$04,
    $13,$04,$29,$10,$92,$40,$08,$04,$44,$82,$22,$00,$19,$20,$00,$19,
    $20,$01,$81,$90,$60,$8a,$00,$41,$c0,$02,$45,$10,$04,$00,$02,$a2,
    $09,$40,$10,$21,$49,$20,$01,$42,$30,$2c,$00,$14,$44,$01,$22,$04,
    $02,$92,$08,$89,$04,$21,$80,$10,$05,$01,$20,$40,$41,$80,$04,$00,
    $12,$09,$40,$b0,$64,$58,$32,$01,$08,$90,$00,$41,$04,$09,$c1,$80,
    $61,$08,$90,$00,$9a,$00,$24,$01,$12,$08,$02,$26,$05,$82,$06,$08,
    $08,$00,$20,$48,$20,$00,$18,$24,$48,$03,$02,$00,$11,$00,$09,$00,
    $84,$01,$4a,$10,$01,$98,$00,$04,$18,$86,$00,$c0,$00,$20,$81,$80,
    $04,$10,$30,$05,$00,$b4,$0c,$4a,$82,$29,$91,$02,$28,$00,$20,$44,
    $c0,$00,$2c,$91,$80,$40,$01,$a2,$00,$12,$04,$09,$c3,$20,$00,$08,
    $02,$0c,$10,$22,$04,$00,$00,$2c,$11,$86,$00,$c0,$00,$00,$12,$32,
    $40,$89,$80,$40,$40,$02,$05,$50,$86,$60,$82,$a4,$60,$0a,$12,$4d,
    $80,$90,$08,$12,$80,$09,$02,$14,$48,$01,$24,$20,$8a,$00,$44,$90,
    $04,$04,$01,$02,$00,$d1,$12,$00,$0a,$04,$40,$00,$32,$21,$81,$24,
    $08,$19,$84,$20,$02,$04,$08,$89,$80,$24,$02,$02,$68,$18,$82,$44,
    $42,$00,$21,$40,$00,$28,$01,$80,$45,$82,$20,$40,$11,$80,$0c,$02,
    $00,$24,$40,$90,$01,$40,$20,$20,$50,$20,$28,$19,$00,$40,$09,$20,
    $08,$80,$04,$60,$40,$80,$20,$08,$30,$49,$09,$34,$00,$11,$24,$24,
    $82,$00,$41,$c2,$00,$04,$92,$02,$24,$80,$00,$0c,$02,$a0,$00,$01,
    $06,$60,$41,$04,$21,$d0,$00,$01,$01,$00,$48,$12,$84,$04,$91,$12,
    $08,$00,$24,$44,$00,$12,$41,$18,$26,$0c,$41,$80,$00,$52,$04,$20,
    $09,$00,$24,$90,$20,$48,$18,$02,$00,$03,$a2,$09,$d0,$14,$00,$8a,
    $84,$25,$4a,$00,$20,$98,$14,$40,$00,$a2,$05,$00,$00,$00,$40,$14,
    $01,$58,$20,$2c,$80,$84,$00,$09,$20,$20,$91,$02,$08,$02,$b0,$41,
    $08,$30,$00,$09,$10,$00,$18,$02,$21,$02,$02,$00,$00,$24,$44,$08,
    $12,$60,$00,$b2,$44,$12,$02,$0c,$c0,$80,$40,$c8,$20,$04,$50,$20,
    $05,$00,$b0,$04,$0b,$04,$29,$53,$00,$61,$48,$30,$00,$82,$20,$29,
    $00,$16,$00,$53,$22,$20,$43,$10,$48,$00,$80,$04,$d2,$00,$40,$00,
    $a2,$44,$03,$80,$29,$00,$04,$08,$c0,$04,$64,$40,$30,$28,$09,$84,
    $44,$50,$80,$21,$02,$92,$00,$c0,$10,$60,$88,$22,$08,$80,$00,$00,
    $18,$84,$04,$83,$96,$00,$81,$20,$05,$02,$00,$45,$88,$84,$00,$51,
    $20,$20,$51,$86,$41,$4b,$94,$00,$80,$00,$08,$11,$20,$4c,$58,$80,
    $04,$03,$06,$20,$89,$00,$05,$08,$22,$05,$90,$00,$40,$00,$82,$09,
    $50,$00,$00,$00,$a0,$41,$c2,$20,$08,$00,$16,$08,$40,$26,$21,$d0,
    $90,$08,$81,$90,$41,$00,$02,$44,$08,$10,$0c,$0a,$86,$09,$90,$04,
    $00,$c8,$a0,$04,$08,$30,$20,$89,$84,$00,$11,$22,$2c,$40,$00,$08,
    $02,$b0,$01,$48,$02,$01,$09,$20,$04,$03,$04,$00,$80,$02,$60,$42,
    $30,$21,$4a,$10,$44,$09,$02,$00,$01,$24,$00,$12,$82,$21,$80,$a4,
    $20,$10,$02,$04,$91,$a0,$40,$18,$04,$00,$02,$06,$69,$09,$00,$05,
    $58,$02,$01,$00,$00,$48,$00,$00,$00,$03,$92,$20,$00,$34,$01,$c8,
    $20,$48,$08,$30,$08,$42,$80,$20,$91,$90,$68,$01,$04,$40,$12,$02,
    $61,$00,$12,$08,$01,$a0,$00,$11,$04,$21,$48,$04,$24,$92,$00,$0c,
    $01,$84,$04,$00,$00,$01,$12,$96,$40,$01,$a0,$41,$88,$22,$28,$88,
    $00,$44,$42,$80,$24,$12,$14,$01,$42,$90,$60,$1a,$10,$04,$81,$10,
    $48,$08,$06,$29,$83,$02,$40,$02,$24,$64,$80,$10,$05,$80,$10,$40,
    $02,$02,$08,$42,$84,$01,$09,$20,$04,$50,$00,$60,$11,$30,$40,$13,
    $02,$04,$81,$00,$09,$08,$20,$45,$4a,$10,$61,$90,$26,$0c,$08,$02,
    $21,$91,$00,$60,$02,$04,$00,$02,$00,$0c,$08,$06,$08,$48,$84,$08,
    $11,$02,$00,$80,$a4,$00,$5a,$20,$00,$88,$04,$04,$02,$00,$09,$00,
    $14,$08,$49,$14,$20,$c8,$00,$04,$91,$a0,$40,$59,$80,$00,$12,$10,
    $00,$80,$80,$65,$00,$00,$04,$00,$80,$40,$19,$00,$21,$03,$84,$60,
    $c0,$04,$24,$1a,$12,$61,$80,$80,$08,$02,$04,$09,$42,$12,$20,$08,
    $34,$04,$90,$20,$01,$01,$a0,$00,$0b,$00,$08,$91,$92,$40,$02,$34,
    $40,$88,$10,$61,$19,$02,$00,$40,$04,$25,$c0,$80,$68,$08,$04,$21,
    $80,$22,$04,$00,$a0,$0c,$01,$84,$20,$41,$00,$08,$8a,$00,$20,$8a,
    $00,$48,$88,$04,$04,$11,$82,$08,$40,$86,$09,$49,$a4,$40,$00,$10,
    $01,$01,$a2,$04,$50,$80,$0c,$80,$00,$48,$82,$a0,$01,$18,$12,$41,
    $01,$04,$48,$41,$00,$24,$01,$00,$00,$88,$14,$00,$02,$00,$68,$01,
    $20,$08,$4a,$22,$08,$83,$80,$00,$89,$04,$01,$c2,$00,$00,$00,$34,
    $04,$00,$82,$28,$02,$02,$41,$4a,$90,$05,$82,$02,$09,$80,$24,$04,
    $41,$00,$01,$92,$80,$28,$01,$14,$00,$50,$20,$4c,$10,$b0,$04,$43,
    $a4,$21,$90,$04,$01,$02,$00,$44,$48,$00,$64,$08,$06,$00,$42,$20,
    $08,$02,$92,$01,$4a,$00,$20,$50,$32,$25,$90,$22,$04,$09,$00,$08,
    $11,$80,$21,$01,$10,$05,$00,$32,$08,$88,$94,$08,$08,$24,$0d,$c1,
    $80,$40,$0b,$20,$40,$18,$12,$04,$00,$22,$40,$10,$26,$05,$c1,$82,
    $00,$01,$30,$24,$02,$22,$41,$08,$24,$48,$1a,$00,$25,$d2,$12,$28,
    $42,$00,$04,$40,$30,$41,$00,$02,$00,$13,$20,$24,$d1,$84,$08,$89,
    $80,$04,$52,$00,$44,$18,$a4,$00,$00,$06,$20,$91,$10,$09,$42,$20,
    $24,$40,$30,$28,$00,$84,$40,$40,$80,$08,$10,$04,$09,$08,$04,$40,
    $08,$22,$00,$19,$02,$00,$00,$80,$2c,$02,$02,$21,$01,$90,$20,$40,
    $00,$0c,$00,$34,$48,$58,$20,$01,$43,$04,$20,$80,$14,$00,$90,$00,
    $6d,$11,$00,$00,$40,$20,$00,$03,$10,$40,$88,$30,$05,$4a,$00,$65,
    $10,$24,$08,$18,$84,$28,$03,$80,$20,$42,$b0,$40,$00,$10,$69,$19,
    $04,$00,$00,$80,$04,$c2,$04,$00,$01,$00,$05,$00,$22,$25,$08,$96,
    $04,$02,$22,$00,$d0,$10,$29,$01,$a0,$60,$08,$10,$04,$01,$16,$44,
    $10,$02,$28,$02,$82,$48,$40,$84,$20,$90,$22,$28,$80,$04,$00,$40,
    $04,$24,$00,$80,$29,$03,$10,$60,$48,$00,$00,$81,$a0,$00,$51,$20,
    $0c,$d1,$00,$01,$41,$20,$04,$92,$00,$00,$10,$92,$00,$42,$04,$05,
    $01,$86,$40,$80,$10,$20,$52,$20,$21,$00,$10,$48,$0a,$02,$00,$d0,
    $12,$41,$48,$80,$04,$00,$00,$48,$09,$22,$04,$00,$24,$00,$43,$10,
    $60,$0a,$00,$44,$12,$20,$2c,$08,$20,$44,$00,$84,$09,$40,$06,$08,
    $c1,$00,$40,$80,$20,$00,$98,$12,$48,$10,$a2,$20,$00,$84,$48,$c0,
    $10,$20,$90,$12,$08,$98,$82,$00,$0a,$a0,$04,$03,$00,$28,$c3,$00,
    $44,$42,$10,$04,$08,$04,$40,$00,$00,$05,$10,$00,$21,$03,$80,$04,
    $88,$12,$69,$10,$00,$04,$08,$04,$04,$02,$84,$48,$49,$04,$20,$18,
    $02,$64,$80,$30,$08,$01,$02,$00,$52,$12,$49,$08,$20,$41,$88,$10,
    $48,$08,$34,$00,$01,$86,$05,$d0,$00,$00,$83,$84,$21,$40,$02,$41,
    $10,$80,$48,$40,$a2,$20,$51,$00,$00,$49,$00,$01,$90,$20,$40,$18,
    $02,$40,$02,$22,$05,$40,$80,$08,$82,$10,$20,$18,$00,$05,$01,$82,
    $40,$58,$00,$04,$81,$90,$29,$01,$a0,$64,$00,$22,$40,$01,$a2,$00,
    $18,$04,$0d,$00,$00,$60,$80,$94,$60,$82,$10,$0d,$80,$30,$0c,$12,
    $20,$00,$00,$12,$40,$c0,$20,$21,$58,$02,$41,$10,$80,$44,$03,$02,
    $04,$13,$90,$29,$08,$00,$44,$c0,$00,$21,$00,$26,$00,$1a,$80,$01,
    $13,$14,$20,$0a,$14,$20,$00,$32,$61,$08,$00,$40,$42,$20,$09,$80,
    $06,$01,$81,$80,$60,$42,$00,$68,$90,$82,$08,$42,$80,$04,$02,$80,
    $09,$0b,$04,$00,$98,$00,$0c,$81,$06,$44,$48,$84,$28,$03,$92,$00,
    $01,$80,$40,$0a,$00,$0c,$81,$02,$08,$51,$04,$28,$90,$02,$20,$09,
    $10,$60,$00,$00,$09,$81,$a0,$0c,$00,$a4,$09,$00,$02,$28,$80,$20,
    $00,$02,$02,$04,$81,$14,$04,$00,$04,$09,$11,$12,$60,$40,$20,$01,
    $48,$30,$40,$11,$00,$08,$0a,$86,$00,$00,$04,$60,$81,$04,$01,$d0,
    $02,$41,$18,$90,$00,$0a,$20,$00,$c1,$06,$01,$08,$80,$64,$ca,$10,
    $04,$99,$80,$48,$01,$82,$20,$50,$90,$48,$80,$84,$20,$90,$22,$00,
    $19,$00,$04,$18,$20,$24,$10,$86,$40,$c2,$00,$24,$12,$10,$44,$00,
    $16,$08,$10,$24,$00,$12,$06,$01,$08,$90,$00,$12,$02,$4d,$10,$80,
    $40,$50,$22,$00,$43,$10,$01,$00,$30,$21,$0a,$00,$00,$01,$14,$00,
    $10,$84,$04,$c1,$10,$29,$0a,$00,$01,$8a,$00,$20,$01,$12,$0c,$49,
    $20,$04,$81,$00,$48,$01,$04,$60,$80,$12,$0c,$08,$10,$48,$4a,$04,
    $28,$10,$00,$28,$40,$84,$45,$50,$10,$60,$10,$06,$44,$01,$80,$09,
    $00,$86,$01,$42,$a0,$00,$90,$00,$05,$90,$22,$40,$41,$00,$08,$80,
    $02,$08,$c0,$00,$01,$58,$30,$49,$09,$14,$00,$41,$02,$0c,$02,$80,
    $40,$89,$00,$24,$08,$10,$05,$90,$32,$40,$0a,$82,$08,$00,$12,$61,
    $00,$04,$21,$00,$22,$04,$10,$24,$08,$0a,$04,$01,$10,$00,$20,$40,
    $84,$04,$88,$22,$20,$90,$12,$00,$53,$06,$24,$01,$04,$40,$0b,$14,
    $60,$82,$02,$0d,$10,$90,$0c,$08,$20,$09,$00,$14,$09,$80,$80,$24,
    $82,$00,$40,$01,$02,$44,$01,$20,$0c,$40,$84,$40,$0a,$10,$41,$00,
    $30,$05,$09,$80,$44,$08,$20,$20,$02,$00,$49,$43,$20,$21,$00,$20,
    $00,$01,$b6,$08,$40,$04,$08,$02,$80,$01,$41,$80,$40,$08,$10,$24,
    $00,$20,$04,$12,$86,$09,$c0,$12,$21,$81,$14,$04,$00,$02,$20,$89,
    $b4,$44,$12,$80,$00,$d1,$00,$69,$40,$80,$00,$42,$12,$00,$18,$04,
    $00,$49,$06,$21,$02,$04,$28,$02,$84,$01,$c0,$10,$68,$00,$20,$08,
    $40,$00,$08,$91,$10,$01,$81,$24,$04,$d2,$10,$4c,$88,$86,$00,$10,
    $80,$0c,$02,$14,$00,$8a,$90,$40,$18,$20,$21,$80,$a4,$00,$58,$24,
    $20,$10,$10,$60,$c1,$30,$41,$48,$02,$48,$09,$00,$40,$09,$02,$05,
    $11,$82,$20,$4a,$20,$24,$18,$02,$0c,$10,$22,$0c,$0a,$04,$00,$03,
    $06,$48,$48,$04,$04,$02,$00,$21,$80,$84,$00,$18,$00,$0c,$02,$12,
    $01,$00,$14,$05,$82,$10,$41,$89,$12,$08,$40,$a4,$21,$01,$84,$48,
    $02,$10,$60,$40,$02,$28,$00,$14,$08,$40,$a0,$20,$51,$12,$00,$c2,
    $00,$01,$1a,$30,$40,$89,$12,$4c,$02,$80,$00,$00,$14,$01,$01,$a0,
    $21,$18,$22,$21,$18,$06,$40,$01,$80,$00,$90,$04,$48,$02,$30,$04,
    $08,$00,$05,$88,$24,$08,$48,$04,$24,$02,$06,$00,$80,$00,$00,$00,
    $10,$65,$11,$90,$00,$0a,$82,$04,$c3,$04,$60,$48,$24,$04,$92,$02,
    $44,$88,$80,$40,$18,$06,$29,$80,$10,$01,$00,$00,$44,$c8,$10,$21,
    $89,$30,$00,$4b,$a0,$01,$10,$14,$00,$02,$94,$40,$00,$20,$65,$00,
    $a2,$0c,$40,$22,$20,$81,$12,$20,$82,$04,$01,$10,$00,$08,$88,$00,
    $00,$11,$80,$04,$42,$80,$40,$41,$14,$00,$40,$32,$2c,$80,$24,$04,
    $19,$00,$00,$91,$00,$20,$83,$00,$05,$40,$20,$09,$01,$84,$40,$40,
    $20,$20,$11,$00,$40,$41,$90,$20,$00,$00,$40,$90,$92,$48,$18,$06,
    $08,$81,$80,$48,$01,$34,$24,$10,$20,$04,$00,$20,$04,$18,$06,$2d,
    $90,$10,$01,$00,$90,$00,$0a,$22,$01,$00,$22,$00,$11,$84,$01,$01,
    $00,$20,$88,$00,$44,$00,$22,$01,$00,$a6,$40,$02,$06,$20,$11,$00,
    $01,$c8,$a0,$04,$8a,$00,$28,$19,$80,$00,$52,$a0,$24,$12,$12,$09,
    $08,$24,$01,$48,$00,$04,$00,$24,$40,$02,$84,$08,$00,$04,$48,$40,
    $90,$60,$0a,$22,$01,$88,$14,$08,$01,$02,$08,$d3,$00,$20,$c0,$90,
    $24,$10,$00,$00,$01,$b0,$08,$0a,$a0,$00,$80,$00,$01,$09,$00,$20,
    $52,$02,$25,$00,$24,$04,$02,$84,$24,$10,$92,$40,$02,$a0,$40,$00,
    $22,$08,$11,$04,$08,$01,$22,$00,$42,$14,$00,$09,$90,$21,$00,$30,
    $6c,$00,$00,$0c,$00,$22,$09,$90,$10,$28,$40,$00,$20,$c0,$20,$00,
    $90,$00,$40,$01,$82,$05,$12,$12,$09,$c1,$04,$61,$80,$02,$28,$81,
    $24,$00,$49,$04,$08,$10,$86,$29,$41,$80,$21,$0a,$30,$49,$88,$90,
    $00,$41,$04,$29,$81,$80,$41,$09,$00,$40,$12,$10,$40,$00,$10,$40,
    $48,$02,$05,$80,$02,$21,$40,$20,$00,$58,$20,$60,$00,$90,$48,$00,
    $80,$28,$c0,$80,$48,$00,$00,$44,$80,$02,$00,$09,$06,$00,$12,$02,
    $01,$00,$10,$08,$83,$10,$45,$12,$00,$2c,$08,$04,$44,$00,$20,$20,
    $c0,$10,$20,$01,$00,$05,$c8,$20,$04,$98,$10,$08,$10,$00,$24,$02,
    $16,$40,$88,$00,$61,$88,$12,$24,$80,$a6,$00,$42,$00,$08,$10,$06,
    $48,$40,$a0,$00,$50,$20,$04,$81,$a4,$40,$18,$00,$08,$10,$80,$01  ){#Z-};
  {Mask array: odd i is prime if pbits16[i shr 4] and pmask16[i and $f] <> 0)}
  pmask16: array[0..15] of byte = (0,1,0,2,0,4,0,8,0,16,0,32,0,64,0,128);

function mp_alloc(size: Longint): pointer;
  {-Allocate and zero heap}
  var
    p: pointer;
  begin
  GetMem(p,size);
  mp_alloc := p;
  FillChar(p^, size, 0);
  end;

procedure mp_init_prim(var a: mp_int; size: Longint);
  {-Initialize a to size digits, rounded up to multiple of mp_allocprec}
  begin
  if size=0 then
    size := mp_allocprec
  else
    size := (size+mp_allocmask) and (not mp_allocmask);
  with a do
    begin
    if size>MAXDigits then
      raise ERtcRSA.Create('mp_init_prim');
    {allocate memory required and clear it}
    pdigits := mp_alloc(sizeof(mp_digit)*size);
    if pdigits=nil then
      raise ERtcRSA.Create('mp_init_prim: alloc');
    {set the used to zero, allocated digits to the}
    {default precision and sign to positive       }
    used  := 0;
    alloc := size;
    sign  := MP_ZPOS;
    magic := MP_MAGIC;
    end;
  end;

procedure mp_init(var a: mp_int);
  {-Initialize an mp_int}
  begin
  mp_init_prim(a, 0);
  end;

procedure mp_rsa_init_private(var prk: TPrivateKey);
  {-Initialize fields of private RSA/CRT key}
  begin
  with prk do
    begin
    mp_init(p);
    mp_init(q);
    mp_init(dp);
    mp_init(dq);
    mp_init(qinv);
    end;
  end;

function mp_not_init(const a: mp_int): boolean;
  {-Sanity check if a is initialized, does not catch all cases!}
  begin
  mp_not_init := (a.magic<>MP_MAGIC) or (a.pdigits=nil) or (a.used > a.alloc);
  end;

procedure mp_freemem(var p: pointer; size: Longint);
  {-Deallocate heap if p<>nil, p will be set to nil}
  begin
  if p<>nil then
    begin
    freemem(p, size);
    p := nil;
    end;
  end;

procedure mp_clear(var a: mp_int);
  {-Free an mp_int}
  begin
  {mp_clear will be executed even if mp_error<>MP_OKAY!}
  if mp_not_init(a) then
    raise ERtcRSA.Create('mp_clear');
  with a do
    begin
    {do nothing if a hasn't been freed previously}
    if pdigits<>nil then
      begin
      {first zero the digits}
      fillchar(pdigits^, sizeof(mp_digit) * used, 0);
      {free ram}
      mp_freemem(pointer(pdigits), sizeof(mp_digit) * alloc);
      {reset members to make debugging easier}
      fillchar(a,sizeof(a),0);
      end;
    end;
  end;

procedure mp_rsa_clear_private(var prk: TPrivateKey);
  {-Clear fields of private RSA/CRT key}
  begin
  with prk do
    begin
    mp_clear(p);
    mp_clear(q);
    mp_clear(dp);
    mp_clear(dq);
    mp_clear(qinv);
    end;
  end;

procedure mp_zero(var a: mp_int);
  {-Set a to zero}
begin
  if mp_not_init(a) then
    raise ERtcRSA.Create('mp_zero');
  with a do
    begin
    sign := MP_ZPOS;
    used := 0;
    if (alloc>0) and (pdigits<>nil) then
      fillchar(pdigits^, sizeof(mp_digit) * alloc, 0);
    end;
  end;

function mp_realloc(p: pointer; oldsize, newsize: Longint): pointer;
  {-Reallocate heap to new size, if newsize>oldsize the new allocated space is zerofilled}
  var
    tmp: pointer;
  begin
  if oldsize=newsize then
    begin
    mp_realloc := p;
    exit;
    end;
  tmp := p;
  ReallocMem(tmp, newsize);
  mp_realloc := tmp;
  if tmp<>nil then
    begin
    if newsize>oldsize then
      begin
      {zero fill new part}
      inc(RtcIntPtr(tmp),oldsize);
      fillchar(tmp^,newsize-oldsize,0);
      end;
    end;
  end;

procedure mp_grow(var a: mp_int; size: Longint);
  {-Grow an mp_int to a given size (new part is zerofilled)}
  var
    np: PDigitArray;
    xs: Longint;
  begin
  if mp_not_init(a) then
    raise ERtcRSA.Create('mp_grow');

  {if the alloc size is smaller alloc more ram}
  if a.alloc<size then
    begin
    if size>MAXDigits then
      raise ERtcRSA.Create('mp_grow')
    else
      begin
      {ensure there are always at least mp_allocprec digits extra on top}
      xs := mp_allocprec + (mp_allocprec - (size and mp_allocmask));
      inc(size, xs);
      {Note: new part is zerofilled}
      np := mp_realloc(a.pdigits, sizeof(mp_digit)*a.alloc, sizeof(mp_digit)*size);
      if np=nil then
        raise ERtcRSA.Create('mp_grow: realloc')
      else
        begin
        a.pdigits := np;
        a.alloc := size;
        end;
      end;
    end;
  end;

{ We need to turn OFF Range and Overflow checking here
  for the Random Numbers Generator to avoid exceptions }
{$Q-,R-}

procedure isaac_generate(var ctx: isaac_ctx);
  {-generate next 256 result values, ie refill randrsl}
  var
    x,y: Longint;
    xi : Longint absolute x;  {better performance for BIT16}
    i  : Longint;
  begin
  with ctx do
    begin
    inc(randc);
    inc(randb, randc);
    for i:=0 to 255 do
      begin
      case i and 3 of
        0: randa := randa xor (randa shl 13);
        1: randa := randa xor (randa shr  6);
        2: randa := randa xor (randa shl  2);
        3: randa := randa xor (randa shr 16);
        end;
       x := randmem[i];
       inc(randa,randmem[(i+128) and 255]);
       y := randmem[(xi shr 2) and 255] + randa + randb;
       randmem[i] := y;
       randb := randmem[(y shr 10) and 255] + x;
       randrsl[i] := randb;
      end;
    {reset result index}
    randidx:=0;
    end;
  end;

procedure isaac_next(var ctx: isaac_ctx);
  {-Next step of PRNG}
  begin
  with ctx do
    begin
    if randidx>255 then isaac_generate(ctx);
    nextres := randrsl[randidx];
    inc(randidx);
    end;
  end;

procedure isaac_internal_init(var ctx: isaac_ctx; flag: boolean);
  {-Init state, use randrsl if flag=true}
  var
    i,j: Longint;
    m: array[0..7] of Longint;
  procedure Mix;
    {-mix the array}
    begin
    m[0] := m[0] xor (m[1] shl 11); inc(m[3], m[0]); inc(m[1], m[2]);
    m[1] := m[1] xor (m[2] shr  2); inc(m[4], m[1]); inc(m[2], m[3]);
    m[2] := m[2] xor (m[3] shl  8); inc(m[5], m[2]); inc(m[3], m[4]);
    m[3] := m[3] xor (m[4] shr 16); inc(m[6], m[3]); inc(m[4], m[5]);
    m[4] := m[4] xor (m[5] shl 10); inc(m[7], m[4]); inc(m[5], m[6]);
    m[5] := m[5] xor (m[6] shr  4); inc(m[0], m[5]); inc(m[6], m[7]);
    m[6] := m[6] xor (m[7] shl  8); inc(m[1], m[6]); inc(m[7], m[0]);
    m[7] := m[7] xor (m[0] shr  9); inc(m[2], m[7]); inc(m[0], m[1]);
    end;
  begin
  with ctx do
    begin
    randa := 0;
    randb := 0;
    randc := 0;

    for i:=0 to 7 do m[i] := Longint($9e3779b9); {the golden ratio}
    for i:=0 to 3 do Mix;

    i := 0;
    while i<256 do
      begin
      {fill in randmem[] with messy stuff}
      if flag then
        begin
        {use all the information in the seed}
        for j:=0 to 7 do inc(m[j], randrsl[i+j]);
        end;
      Mix;
      move(m, randmem[i], sizeof(m));
      inc(i,8);
      end;

    if flag then
      begin
      {do a second pass to make all of the seed affect all of randmem}
      i := 0;
      while i<256 do
        begin
        for j:=0 to 7 do inc(m[j], randmem[i+j]);
        Mix;
        move(m, randmem[i], sizeof(m));
        inc(i,8);
        end;
      end;

    {generate first set of results}
    isaac_generate(ctx);
    {prepare to use the first set of results }
    randidx := 0;
    end;
  end;

procedure isaac_init(var ctx: isaac_ctx; seed: Longint);
  {-Init context from randrsl[0]=seed, randrsl[i]=0 otherwise}
  begin
  with ctx do
    begin
    fillchar(randrsl, sizeof(randrsl),0);
    randrsl[0] := seed;
    end;
  isaac_internal_init(ctx, true);
  end;

procedure isaac_init0(var ctx: isaac_ctx; xseed:Longint);
  {-Init context from randseed and randrsl[i]:=random}
  var
    i,j: Longint;
    tl: Longint;
    ta: packed array[0..3] of byte absolute tl;
  begin
  with ctx do
    begin
    for i:=0 to 255 do
      begin
      for j:=0 to 3 do ta[j] := byte(random(256));
      randrsl[i] := tl xor xseed;
      end;
    end;
  isaac_internal_init(ctx, true);
  end;

{restore range/overflow check status}
{$ifdef RangeChecks_on}    {$R+} {$endif}
{$ifdef OverflowChecks_on} {$Q+} {$endif}

function mp_random_digit(var ctx:isaac_ctx): mp_digit;
  {-Returns a random mp_digit}
  begin
  isaac_next(ctx);
  mp_random_digit := mp_digit(ctx.nextres and MP_DIGIT_MAX);
  end;

function mp_random_word(var ctx:isaac_ctx): word;
  {-Returns a random Word}
  begin
  isaac_next(ctx);
  mp_random_word := word(ctx.nextres and $FFFF);
  end;

function mp_random_byte(var ctx:isaac_ctx): byte;
  {-Returns a random Byte}
  begin
  isaac_next(ctx);
  mp_random_byte := byte(ctx.nextres and $FF);
  end;

procedure mp_clamp(var a: mp_int);
  {-Trim unused digits}
  begin
  {Decrease used while the most significant digit is zero. This is    }
  {used to ensure that leading zero digits are trimmed and the leading}
  {"used" digit will be non-zero. Typically very fast. Also fixes the }
  {sign if there are no more leading digits.                          }

  {No arg check since mp_clamp will be called after working with a    }
  with a do
    begin
    if pdigits<>nil then
      while (used>0) and (pdigits^[pred(used)]=0) do dec(used);
    {reset the sign flag if used=0}
    if used=0 then sign := MP_ZPOS;
    end;
  end;

type
  TNInt     = Longint;            {'native' 32-bit Longint       }

procedure mp_rand_ex(var ctx:isaac_ctx; var a: mp_int; digits: Longint; sethi: boolean);
  {-Make a pseudo-random mp_int of a given digit size, if not sethi}
  { then a[digits-1] may be zero (and a.used will be decremented)}
  var
    d: mp_digit;
    i: TNInt;
  begin
  {InitCheck in mp_zero}
  if digits>=MAXDigits then
    raise ERtcRSA.Create('mp_rand_ex');

  mp_zero(a);
  if digits=0 then exit;

  if a.alloc<digits then
    mp_grow(a,digits);

  with a do
    begin
    used := digits;
    if sethi then
      begin
      {make highest digit non-zero}
      repeat
        d := mp_random_digit(ctx);
        until d<>0;
      end
    else
      d := mp_random_digit(ctx);
    pdigits^[digits-1] := d;
    if digits>1 then
      for i:=0 to digits-2 do
        pdigits^[i] := mp_random_digit(ctx);
    end;

  if not sethi then mp_clamp(a);
  end;

procedure mp_rand(var ctx:isaac_ctx; var a: mp_int; digits: Longint);
  {-Make a pseudo-random mp_int of a given digit size}
begin
  mp_rand_ex(ctx, a, digits, true);
end;

procedure mp_rand_bits_ex(var ctx:isaac_ctx; var a: mp_int; bits: Longint; sethi: boolean);
  {-Make pseudo-random a with bitsize <= bits, if sethi highest bit is set}
var
  digits: Longint;
  rem: Longint;
  m1,m2: mp_digit;
begin
  {InitCheck in mp_rand...}
  if bits<=0 then begin
    mp_zero(a);
    exit;
  end;
  {make random digits, here bits>0 and digits>0}
  digits := (bits+(DIGIT_BIT-1)) div DIGIT_BIT;
  rem := bits mod DIGIT_BIT;
  mp_rand(ctx, a, digits);
  {Mask highest digit and set highest bit}
  if rem=0 then begin
    m2 := mp_digit(1) shl (DIGIT_BIT-1);
    m1 := MP_MASK;
  end
  else begin
    m2 := mp_digit(1) shl (rem-1);
    m1 := m2 or mp_digit(m2-1);
  end;
  {highest bit is untouched if sethi=false}
  if not sethi then m2:=0;
  with a do if used>0 then pdigits^[used-1] := (pdigits^[used-1] and m1) or m2;
  if m2=0 then mp_clamp(a);
end;

procedure mp_rand_bits(var ctx:isaac_ctx; var a: mp_int; bits: Longint);
  {-Make a pseudo-random mp_int of a given bit size}
  { Note: bits are not completely random because highest bit is always set}
  begin
  mp_rand_bits_ex(ctx,a,bits,true);
  end;

function mp_isodd(const a: mp_int): boolean;
  {-Initialized and odd}
  begin
  with a do
    mp_isodd := (magic=MP_MAGIC) and (pdigits<>nil) and (used>0) and odd(pdigits^[0]);
  end;

procedure s_mp_add_d(const a: mp_int; b: mp_digit; var c: mp_int); forward;

procedure s_mp_sub_d(const a: mp_int; b: mp_digit; var c: mp_int);
  {-Single digit subtraction, no init check, b<>0}
  var
    pa, pc: pmp_digit;
    p: mp_int;
    ix: TNInt;
    oldused: Longint;
    t, mu: mp_digit;
  begin
  {grow c as required}
  if c.alloc < a.used + 1 then
    mp_grow(c, a.used + 1);

  {if a is negative just do an unsigned addition}
  if a.sign=MP_NEG then
    begin
    {make positive copy of a, pdigits is from a but unchanged!}
    p := a;
    p.sign := MP_ZPOS;
    s_mp_add_d(p, b, c);
    mp_clamp(c); {V0.6.06}
    c.sign := MP_NEG;
    exit;
    end;

  pa := pmp_digit(a.pdigits);
  pc := pmp_digit(c.pdigits);
  oldused := c.used;

  {if a <= b simply fix the single digit}
  if ((a.used=1) and (pa^ <= b)) or (a.used=0) then
    begin
    if a.used=1 then pc^ := b - pa^ else pc^ := b;
    inc(pc);
    ix := 1;
    {negative/1digit}
    c.sign := MP_NEG;
    c.used := 1;
    end
  else
    begin
    {positive/size}
    c.sign := MP_ZPOS;
    c.used := a.used;
    {t := pa^ - .. and t shr .. can produce range errors}
    {so turn off range/overflow check temporarily}
    {$Q-,R-}
    mu := b;
    ix := 0;
    {note: a.used>0, loop is executed at least once}
    while (mu<>0) and (ix<a.used) do
      begin
      t  := pa^ - mu;
      inc(pa);
      mu  := t shr (8*sizeof(mp_digit) - 1);
      pc^ := t and MP_MASK;
      inc(pc);
      inc(ix);
      end;
    {restore range/overflow check status}
    {$ifdef RangeChecks_on}    {$R+} {$endif}
    {$ifdef OverflowChecks_on} {$Q+} {$endif}
    while ix<a.used do
      begin
      pc^ := pa^;
      inc(pa);
      inc(pc);
      inc(ix);
      end;
    end;

  {zero excess digits}
  while ix<oldused do
    begin
    pc^ := 0;
    inc(pc);
    inc(ix);
    end;
  mp_clamp(c);
  end;

procedure s_mp_add_d(const a: mp_int; b: mp_digit; var c: mp_int);
  {-Single digit addition, no init check, b<>0}
var
  pa, pc: pmp_digit;
  p: mp_int;
  ix: TNInt;
  oldused: Longint;
  t, mu: mp_digit;
begin
  {grow c as required}
  if c.alloc < a.used + 1 then
    mp_grow(c, a.used + 1);
  pa := pmp_digit(a.pdigits);

  {if a is negative and |a| >= b, call c = |a| - b}
  if (a.sign=MP_NEG) and ((a.used>1) or (pa^ >= b)) then
    begin
    {make positive copy of a, pdigits are from a but unchanged!}
    p := a;
    p.sign := MP_ZPOS;
    {c = |a| - b}
    s_mp_sub_d(p, b, c);
    c.sign := MP_NEG;
    mp_clamp(c);
    exit;
    end;

  {old number of used digits in c}
  oldused := c.used;
  {sign always positive}
  c.sign := MP_ZPOS;
  pc := pmp_digit(c.pdigits);

  {if a is positive}
  if a.sign=MP_ZPOS then
    begin
    {add digit, after this we're propagating the carry}
    t := pa^ + b;
    inc(pa);
    mu  := t shr DIGIT_BIT;
    pc^ := t and MP_MASK;
    inc(pc);
    ix := 1;
    {now handle rest of the digits, first loop while carry is non-zero}
    while (mu<>0) and (ix<a.used) do
      begin
      t := pa^ + mu;
      inc(pa);
      mu  := t shr DIGIT_BIT;
      pc^ := t and MP_MASK;
      inc(pc);
      inc(ix);
      end;
    {copy remaining without carry}
    while ix<a.used do
      begin
      pc^ := pa^;
      inc(pa);
      inc(pc);
      inc(ix);
      end;
    {set final carry}
    inc(ix);
    pc^ := mu;
    {setup size}
    c.used := a.used + 1;
    end
  else
    begin
    {a was negative and |a| < b}
    c.used := 1;
    {the result is a single digit}
    if a.used=1 then pc^ := b-pa^ else pc^ := b;
    inc(pc);
    {setup count so the clearing of oldused can fall through correctly}
    ix := 1;
    end;

  {now zero up to oldused}
  while ix<oldused do
    begin
    pc^ := 0;
    inc(pc);
    inc(ix);
    end;

  mp_clamp(c);
  end;

procedure mp_inc(var a: mp_int);
  {-Increment an mp_int by 1}
  begin
  s_mp_add_d(a,1,a);
  end;

procedure mp_copy(const a: mp_int; var b: mp_int);
  {-Copy an mp_int, b = a}
  var
    i: TNInt;
  begin
  {if dst=src do nothing}
  if @a=@b then exit;
  {grow dest}
  if b.alloc < a.used then
    mp_grow(b, a.used);
  move(a.pdigits^, b.pdigits^, a.used*sizeof(mp_digit));
  if b.used>a.used then
    for i:=a.used to b.used-1 do
      b.pdigits^[i]:=0;
  b.used := a.used;
  b.sign := a.sign;
  end;

procedure mp_set(var a: mp_int; b: mp_digit);
  {-Set a to digit b}
  begin
  {mp_zero does ArgCheck!}
  mp_zero(a);
  b := b and MP_MASK;
  if b<>0 then
    with a do
      begin
      if (pdigits=nil) or (alloc=0) then
        raise ERtcRSA.Create('mp_set')
      else
        begin
        used := 1;
        pdigits^[0] := b;
        end;
      end;
  end;

procedure mp_lshd(var a: mp_int; b: Longint);
  {-Shift left a certain amount of digits}
  begin
  {if b is less than zero return}
  {if a=0, return} {*0.1.29}
  if (b<=0) or (a.used=0) then exit;

  with a do
    begin
    if Longint(b)+used > MaxDigits then
      raise ERtcRSA.Create('mp_lshd');
    {grow to fit the new digits}
    if alloc < used+b then
      mp_grow(a, used+b);
    {shift used digits, used>0!}
    move(pdigits^[0], pdigits^[b], used*sizeof(mp_digit));
    {clear low b digits, b>0!}
    fillchar(pdigits^, b*sizeof(mp_digit), 0);
    inc(used, b);
    end;
  end;

procedure mp_set_int(var a: mp_int; b: Longint);
  {-Set a to a Longint}
  var
    hd : mp_digit;
    neg: boolean;
  begin
  {mp_zero does ArgCheck!}
  if b=0 then
    begin
    mp_zero(a);
    exit;
    end;
  if b>0 then
    neg := false
  else
    begin
    neg := true;
    b := -b;
    end;
  {mp_digit has at least 16 bits, ie a Longint max two mp_digits}
  hd := b shr DIGIT_BIT;
  if hd=0 then
    mp_set(a, mp_digit(b))
  else
    begin
    mp_set(a, hd);
    mp_lshd(a,1);
    a.pdigits^[0] := mp_digit(b and MP_MASK);
    end;
  if neg then a.sign := MP_NEG;
  end;

procedure mp_init_set_int(var a: mp_int; b: Longint);
  {-Initialize and set a to a Longint}
  begin
  mp_init(a);
  mp_set_int(a,b);
  end;

procedure s_mp_add(const a,b: mp_int; var c: mp_int);
  {-Low level addition c=a+b, based on HAC pp.594, algorithm 14.7}
  var
    pa,pb,pc,px: pmp_digit;
    olduse, min, max: Longint;
    i: TNInt;
    u,t: mp_digit;
  begin
  {find sizes, we let |a| <= |b| which means we have to sort}
  {them. "px" will point to the input with the most digits  }

  if a.used > b.used then max := a.used else max := b.used;

  {initialize result}

  {must grow before assigning pointer otherwise realloc }
  {can change the digits memory, ie px points to the old}
  {digits, and (max+1)th digit is stored into nirvana   }

  if c.alloc < max+1 then
    mp_grow(c, max + 1);

  if a.used > b.used then
    begin
    min := b.used;
    px  := @a.pdigits^[min];
    end
  else
    begin
    min := a.used;
    px  := @b.pdigits^[min];
    end;

  pa  := pmp_digit(a.pdigits);
  pb  := pmp_digit(b.pdigits);
  pc  := pmp_digit(c.pdigits);

  {get old used digit count and set new one}
  olduse := c.used;
  c.used := max + 1;

  {zero the carry}
  u := 0;
  i := 0;
  while i<min do
    begin
    {Compute the sum at one digit, t[i] = a[i] + b[i] + u}
    t := pa^ + pb^ + u;
    {U = carry bit of t[i]}
    u := t shr mp_digit(DIGIT_BIT);
    {take away carry bit from t[i]}
    pc^ := t and MP_MASK;
    inc(pa);
    inc(pb);
    inc(pc);
    inc(i);
    end;
  {now copy higher words if any, that is in a+b}
  {if a or b has more digits add those in}
  if min<max then
    begin
    while i<max do
      begin
      {t[i] = x[i] + u}
      t := px^ + u;
      {u = carry bit of t[i]}
      u := t shr mp_digit(DIGIT_BIT);
      {take away carry bit from t[i]}
      pc^ := t and MP_MASK;
      inc(px);
      inc(pc);
      inc(i);
      end;
    end;

  {add carry}
  pc^ := u;
  inc(pc);
  inc(i);

  {clear digits above oldused}
  while i<olduse do
    begin
    pc^ := 0;
    inc(pc);
    inc(i);
    end;
  mp_clamp(c);
  end;

function mp_cmp_mag(const a,b: mp_int): Longint;
  {-Compare magnitude of two mp_ints (unsigned), return sign(|a|-|b|)}
  var
    pa,pb: pmp_digit;
    i: TNInt;
  begin
  {Value for last alternative, keep D6+ happy}
  mp_cmp_mag := 0;
  {compare based on # of non-zero digits}
  if a.used > b.used then
    begin
    mp_cmp_mag := 1;
    exit;
    end
  else if a.used < b.used then
    begin
    mp_cmp_mag := -1;
    exit;
    end
  else if a.used>0 then
    begin
    {a.used=b.used, if a.used=0 then both are 0}
    pa := @a.pdigits^[a.used-1];
    pb := @b.pdigits^[b.used-1];
    for i:=1 to a.used do
      begin
      {look for different highest digit}
      if pa^ <> pb^ then
        begin
        if pa^ < pb^ then mp_cmp_mag := -1
        else mp_cmp_mag := 1;
        exit;
        end;
      dec(pa);
      dec(pb);
      end;
    {all digits are equal, def. result 0 applies}
    end;
  end;

procedure s_mp_sub(const a,b: mp_int; var c: mp_int);
  {-Low level subtraction (assumes |a| >= |b|), HAC pp.595 algorithm 14.9}
  var
    pa,pb,pc: pmp_digit;
    olduse, min, max: Longint;
    i: TNInt;
    u,t: mp_digit;
  begin
  {find sizes}
  min := b.used;
  max := a.used;

  {initialize result}
  if c.alloc < max then
    mp_grow(c, max);

  {get old used digit count and set new one}
  olduse := c.used;
  c.used := max;

  pa  := pmp_digit(a.pdigits);
  pb  := pmp_digit(b.pdigits);
  pc  := pmp_digit(c.pdigits);

  {set carry to zero}
  u := 0;
  i := 0;

  {$Q-,R-} {Temporarily turn off range/overflow checking}
  while i<min do
    begin
    {t[i] = a[i] - b[i] - u}
    t := pa^ - pb^ - u;              {May cause overflow of mp_digit -> $Q-}
    {u = carry bit (sign bit) of t[i]}
    {Note this saves performing an AND operation since          }
    {if a carry does occur it will propagate all the way to the }
    {MSB.  As a result a single shift is enough to get the carry}

    {take away carry bit from t[i]}
    u := t shr mp_digit((8*sizeof(mp_digit) - 1));
    pc^ := t and MP_MASK;
    inc(pa);
    inc(pb);
    inc(pc);
    inc(i);
    end;

  {now copy higher words if any, e.g. if a has more digits than b}
  while i<max do
    begin
    {t[i] = a[i] - u}
    t := pa^ - u;                    {May cause overflow of mp_digit -> $Q-}
    {u = carry bit of t[i]}
    u := t shr mp_digit((8*sizeof(mp_digit)-1));
    {take away carry bit from t[i]}
    pc^ := t and MP_MASK;
    inc(pa);
    inc(pc);
    inc(i);
    end;

  {restore range/overflow check status}
  {$ifdef RangeChecks_on}    {$R+} {$endif}
  {$ifdef OverflowChecks_on} {$Q+} {$endif}

  {clear digits above oldused}
  while i<olduse do
    begin
    pc^ := 0;
    inc(pc);
    inc(i);
    end;
  mp_clamp(c);
  end;

procedure mp_add(const a,b: mp_int; var c: mp_int);
  {-High level addition (handles signs)}
  var
    cmp: Longint;
  begin
  {easy outs} {*0.1.29}
  if a.used=0 then
    begin
    mp_copy(b,c);
    exit;
    end;
  if b.used=0 then
    begin
    mp_copy(a,c);
    exit;
    end;

  {handle two cases, not four}
  if a.sign=b.sign then
    begin
    {both positive or both negative}
    {add their magnitudes, copy the sign}
    c.sign := a.sign;
    s_mp_add(a, b, c);
    end
  else
    begin
    {one positive, the other negative                 }
    {subtract the one with the greater magnitude from }
    {the one of the lesser magnitude.  The result gets}
    {the sign of the one with the greater magnitude.  }

    {Easy out, and make inputs to s_mp_sub unequal    } {*0.1.29}
    cmp := mp_cmp_mag(a, b);
    if cmp=MP_EQ then mp_zero(c)
    else if cmp=MP_LT then
      begin
      c.sign := b.sign;
      s_mp_sub(b, a, c);
      end
    else
      begin
      c.sign := a.sign;
      s_mp_sub(a, b, c);
      end;
    end;
  end;

procedure mp_add_int(const a: mp_int; b: Longint; var c: mp_int);
  begin
  if b>0 then s_mp_add_d(a,b,c)
  else if b<0 then s_mp_sub_d(a,-b,c)
  else mp_copy(a,c);
  end;

procedure mp_inc_int(var a: mp_int; b: Longint);
  {-Calculate a = a + b}
  begin
  mp_add_int(a,b,a);
  end;

function mp_get_int(const a: mp_int): Longint;
  {-Get the lower signed 31 bits of an mp_int}
  var
    i: Longint;
    res: Longint;
  begin
  mp_get_int:=0;

  {get number of digits of the lsb we have to read}
  i := (31+DIGIT_BIT-1) div DIGIT_BIT;
  if i>a.used then i:=a.used;
  dec(i);
  if i<0 then exit;

  {get most significant digit of result}
  res := a.pdigits^[i];

  while i>0 do
    begin
    dec(i);
    res := (res shl DIGIT_BIT) or Longint(a.pdigits^[i]);
    end;

  res := res and $7FFFFFFF;
  if a.sign=MP_NEG then res := -res;
  mp_get_int := res;
  end;

function bitsize32(a: mp_digit): Longint;
  {-Return the number of bits in a (index of highest bit), 0 if no bit is set}
  var
    x: word;
    r: Longint;
  type
    LH = packed record L,H: word; end;
  begin
  if LH(a).H<>0 then
    begin
    x := LH(a).H;
    r := 16;
    end
  else
    begin
    x := LH(a).L;
    r := 0;
    end;
  if x<>0 then
    begin
    if x and $FF00 <> 0 then begin x := x shr 8;  inc(r,8); end;
    if x and $00F0 <> 0 then begin x := x shr 4;  inc(r,4); end;
    if x and $000C <> 0 then begin x := x shr 2;  inc(r,2); end;
    if x and $0002 <> 0 then inc(r);
    bitsize32 := r+1;
    end
  else
    bitsize32 := r;
  end;

function mp_bitsize(const a: mp_int): Longint;
  {-Return the number of bits in a (index of highest bit), 0 if no bit is set}
  begin
  mp_bitsize := 0;
  {bits used = (used-1)*DIGIT_BIT + bits in highest digit}
  if a.used>0 then
    mp_bitsize := Longint(a.used-1)*DIGIT_BIT + bitsize32(a.pdigits^[a.used-1]);
  end;

function mp_is_longint(const a: mp_int; var b: Longint): boolean;
  {-Test if a fits into Longint, if true set b := a}
  const
    DC_MAXLONG = (30+DIGIT_BIT) div DIGIT_BIT;
  var
    i,u: TNInt;
  begin
  u := a.used;
  if u=0 then
    b := 0
  else
    begin
    if u=1 then
      b := Longint(a.pdigits^[0])
    else
      begin
      if (u>DC_MAXLONG) or (u=DC_MAXLONG) and (mp_bitsize(a)>31) then
        begin
        mp_is_longint := false;
        exit;
        end;
      b := 0;
      for i:=pred(u) downto 0 do
        b := (b shl DIGIT_BIT) or Longint(a.pdigits^[i]);
      end;
    if a.sign=MP_NEG then b := -b;
    end;
  mp_is_longint := true;
  end;

procedure s_mp_mod_int(const a: mp_int; b: Longint; var c: Longint);
  {-Calculate r = a mod b for a single Longint b, no init check}
  var
    w,q: Longint;
    ix: TNInt;
    d: mp_digit;
  begin
  q := abs(b);
  {cannot divide by zero}
  if q=0 then
    raise ERtcRSA.Create('s_mp_mod_int: b=0');
  {check for trivial cases}
  if (q=1) or (a.used=0) then
    begin
    c := 0;
    exit;
    end;

  if q and (q-1) = 0 then
    begin
    {b is power of 2}
    w := abs(mp_get_int(a)) and (q-1);
    end
  else
    begin
    if mp_is_longint(a,w) then
      w := abs(w) mod q
    else
      begin
      w := 0;
      {note: a.used>0}
      for ix:=a.used-1 downto 0 do
        begin
        d := a.pdigits^[ix];
        w := ((int64(w) shl DIGIT_BIT) or d) mod q;
        end;
      end;
    end;
  {adjust sign for non-zero result}
  if w<>0 then
    begin
    {w is positive. First if a<0 make remainder negative}
    if a.sign=MP_NEG then w := -w;
    {if remainder and modulus have different sign, add modulus}
    if w xor b < 0 then w := w+b;
    end;
  c := w;
  end;

procedure mp_mod_int(const a: mp_int; b: Longint; var c: Longint);
  {-Calculate r = a mod b for a single Longint b}
  begin
  s_mp_mod_int(a,b,c);
  end;

procedure mp_small_factor(const a: mp_int; f0,fmax: mp_digit; var f: mp_digit);
  {-Compute small digit prime factor or 0, f0..fmax, f will be <= min(fmax,$7FFF)}
  var
    i,imax,imin: word;
    r: Longint;
    q: mp_digit;
  const
    s1=3*5*7*11*13*17*19*23;
  begin
  {assume no error}
  if fmax<mp_max_small then
    imax := word(fmax)
  else
    imax := word(mp_max_small);

  if f0=0 then imin := 2
  else if f0>imax then imin:=imax else imin := word(f0);

  {easy outs}
  f := 0;
  i := a.used;
  if i<1 then exit;
  q := a.pdigits^[0];
  if (i<2) and (q<2) then exit; {a=0 or 1}
  if imin=2 then
    begin
    if q and 1 = 0 then
      begin
      f := 2;
      exit;
      end
    else
      imin := 3;
    end;

  if imin>3 then
    begin
    {Skip fast test; note that normal (internal) uses have imin 2 or 3}
    i := imin;
    if i and 1 = 0 then inc(i);
    end
  else
    begin
    mp_mod_int(a,s1,r); {3*5*7*11*13*17*19*23}
    if r mod 3 = 0 then
      begin
      f := 3;
      exit;
      end;
    if r mod 5 = 0 then
      begin
      f := 5;
      exit;
      end;
    if r mod 7 = 0 then
      begin
      f := 7;
      exit;
      end;
    if r mod 11 = 0 then
      begin
      f := 11;
      exit;
      end;
    if r mod 13 = 0 then
      begin
      f := 13;
      exit;
      end;
    if r mod 17 = 0 then
      begin
      f := 17;
      exit;
      end;
    if r mod 19 = 0 then
      begin
      f := 19;
      exit;
      end;
    if r mod 23 = 0 then
      begin
      f := 23;
      exit;
      end;
    i := 29;
    end;
  while i<=imax do
    begin
    if (pbits16[i shr 4] and pmask16[i and $0F] <> 0) then
      begin
      {Note: i<=imax<=MP_DIGIT_MAX}
      mp_mod_int(a, i, r);
      if r=0 then
        begin
        {return the factor}
        f := mp_digit(i);
        exit;
        end;
      end;
    inc(i,2);
    end;
  end;

procedure mp_sub_d(const a: mp_int; b: mp_digit; var c: mp_int);
  {-Single digit subtraction}
  begin
  if b>MP_DIGIT_MAX then
    raise ERtcRSA.Create('mp_sub_d: b>MP_DIGIT_MAX');
  if b=0 then mp_copy(a,c) else s_mp_sub_d(a,b,c);
  end;

function mp_iszero(const a: mp_int): boolean;
  {-Initialized and zero}
  begin
  with a do mp_iszero := (magic=MP_MAGIC) and (pdigits<>nil) and (used=0);
  end;

procedure mp_abs(const a: mp_int; var b: mp_int);
  {-Absolute value, b = |a|}
  begin
  {Arg check in mp_copy}
  mp_copy(a, b);
  {force the sign of b to positive}
  b.sign := MP_ZPOS;
  end;

procedure mp_exch(var a,b: mp_int);
  {-Exchange two mp_ints}
  var
    t: mp_int;
  begin
  t := a;
  a := b;
  b := t;
  end;

function mp_is_pow2_d(d: mp_digit; var n: Longint): boolean;
  {-Check if d is power of 2, if true, return n with d=2^n}
  var
    i: Longint;
  begin
  mp_is_pow2_d := false;
  {First do a quick check to get the result}
  if (d=0) or (d and pred(d) <> 0) then exit;
  {now get bit index in highest digit}
  for i:= 0 to DIGIT_BIT-1 do
    begin
    if d=(mp_digit(1) shl i) then
      begin
      mp_is_pow2_d := true;
      n := i;
      exit;
      end;
    end;
  end;

procedure mp_rshd(var a: mp_int; b: Longint);
  {-Shift right a certain amount of digits}
  begin
  {if b <= 0 then ignore}
  if b<=0 then exit;
  with a do
    begin
    {if b > used then simply zero it and return}
    if used<=b then
      begin
      mp_zero(a);
      exit;
      end;
    {shift digits}
    move(pdigits^[b], pdigits^[0], (used-b)*sizeof(mp_digit));
    {zero the top digits}
    fillchar(pdigits^[used-b],b*sizeof(mp_digit),0);
    dec(used, b);
    end;
  end;

procedure mp_shr(const a: mp_int; b: Longint; var c: mp_int);
  {-Shift right a, c = a/2^b; c=a if b<=0}
  var
    d, mask, shift: mp_digit;
    r, rr: mp_digit;
    pc: pmp_digit;
    x: TNInt;
  begin
  {if a=0 or b<=0 then we do no work}
  if (b<=0) or (a.used=0) then
    begin
    mp_copy(a, c);
    exit;
    end;
  if b>MP_MAXBIT then
    begin
    mp_zero(c);
    exit;
    end;

  {copy}
  mp_copy(a, c);

  {shift by as many digits in the bit count}
  if b>=DIGIT_BIT then mp_rshd(c, b div DIGIT_BIT);

  {shift any bit count < DIGIT_BIT}
  d := b mod DIGIT_BIT;
  if (d<>0) and (c.used>0) then
    begin  {0.2.06: c.used>0}
    mask := (1 shl d) - 1;
    {shift for lsb}
    shift := DIGIT_BIT - d;
    pc := @c.pdigits^[c.used-1];

    {carry}
    r := 0;
    for x:=c.used-1 downto 0 do
      begin
      {get the lower bits of this word in a temp}
      rr := pc^ and mask;
      {shift the current word and mix in the carry bits from the previous word}
      pc^ := (pc^ shr d) or (r shl shift);
      dec(pc);
      {set the carry to the carry bits of the current word found above}
      r := rr;
      end;
    end;
  mp_clamp(c);
  end;

procedure mp_init_size(var a: mp_int; size: Longint);
  {-Initialize a to size digits, rounded up to multiple of mp_allocprec}
  begin
  {pad size so there are always extra digits}
  if size>MAXDigits then
    raise ERtcRSA.Create('mp_init_size');
  mp_init_prim(a, size);
  end;

procedure s_mp_div_d(const a: mp_int; b: mp_digit; pc: pmp_int; var d: mp_digit);
  {-Single digit division, pc^=sign(a)(|a| div b), r = |a| mod b, no init check}
  var
    q: mp_int;
    pa,pq: pmp_digit;
    i2: Longint;
    ix: TNInt;
    w: mp_word;
    t: mp_digit;
  begin
  {cannot divide by zero}
  if b=0 then
    raise ERtcRSA.Create('s_mp_div_d: b=0');

  {*tbd: div 3?}

  {quick outs}
  if (b=1) or (a.used=0) then
    begin
    d := 0;
    if pc<>nil then mp_copy(a, pc^);
    exit;
    end;

  {power of two?}
  if mp_is_pow2_d(b, i2) then
    begin
    d := a.pdigits^[0] and pred(b);
    if pc<>nil then mp_shr(a, i2, pc^);
    exit;
    end;

  {no easy answer [c'est la vie]. Just division}
  mp_init_size(q, a.used);
  try
    q.used := a.used;
    q.sign := a.sign;
    {a<>0 -> a.used>0}

    ix := pred(a.used);

    pa := pmp_digit(@a.pdigits^[ix]);
    pq := pmp_digit(@q.pdigits^[ix]);

    w := 0;
    for ix:= pred(a.used) downto 0 do
      begin
      w := (w shl DIGIT_BIT) or pa^;
      if w >= b then
        begin
        t := w div b;
        w := w - mp_word(t) * mp_word(b); {w := w mod b}
        {The following is buggy for FPC242,244 with debug!!}
        {dec(w, mp_word(t) * mp_word(b));}
        end
      else
        t:=0;
      pq^ := t;
      dec(pa);
      dec(pq);
      end;
    d := mp_digit(w);
    if pc<>nil then
      begin
      {*0.9.00: clamp only if needed}
      mp_clamp(q);
      mp_exch(q, pc^);
      end;
  finally
    mp_clear(q);
    end;
  end;

procedure mp_init_copy(var a: mp_int; const b: mp_int);
  {-Create a, then copy b into it}
  begin
  mp_init_size(a,b.used);
  mp_copy(b, a);
  end;

procedure mp_shl(const a: mp_int; b: Longint; var c: mp_int);
  {-Shift left by a certain bit count}
  var
    shift, mask, r, d: mp_digit;
    rr: mp_digit;
    pc: pmp_digit;
    i: TNInt;
    nu,bd: Longint;
  begin
  {init check in mp_copy, (does nothing if @a=@c)}
  mp_copy(a, c);
  if (a.used=0) or (b<=0) then exit;

  if b>MP_MAXBIT then
    raise ERtcRSA.Create('mp_shl: b>MP_MAXBIT');

  bd := b div DIGIT_BIT;

  nu := c.used + bd + 1;
  if c.alloc < nu then
    mp_grow(c, nu);

  {shift by as many digits in the bit count}
  if bd>0 then
    mp_lshd(c, bd);

  {shift any bit count < DIGIT_BIT}
  d := b mod DIGIT_BIT;
  if (d<>0) and (bd<c.used) then
    begin
    {bitmask for carries}
    mask := (1 shl d) - 1;
    {shift for msbs}
    shift := DIGIT_BIT - d;
    pc := pmp_digit(@c.pdigits^[bd]);

    {carry}
    r := 0;
    {don't waste time on the zero digits we have just created}
    {with mp_lshd, ie don't touch digits 0..bd-1}
    for i:=bd to c.used-1 do
      begin
      {get the higher bits of the current word}
      rr := (pc^ shr shift) and mask;
      {shift the current word and OR in the carry}
      pc^ := ((pc^ shl d) or r) and MP_MASK;
      inc(pc);
      {set the carry to the carry bits of the current word}
      r := rr;
      end;
    {set final carry}
    if r<>0 then
      begin
      pc^ := r;
      inc(c.used);
      end;
    end;
  mp_clamp(c);
  end;

procedure mp_lshd2(const a: mp_int; var b: mp_int; cnt: Longint);
  {-Set b to a shifted left by cnt digits}
  var
    ns: Longint;
  begin
  {if a=0 or cnt is less than zero then b := a}
  if (cnt<=0) or (a.used=0) then
    begin
    mp_copy(a,b);
    exit;
    end;

  ns := Longint(cnt)+a.used;
  if ns > MaxDigits then
    raise ERtcRSA.Create('mp_lshd2');

  b.sign := a.sign;
  {grow b to fit the new digits}
  if b.alloc < ns then
    mp_grow(b, ns)
  else if b.used>ns then
    {clear high digits of b above new and old .used}
    fillchar(b.pdigits^[ns], (b.used-ns)*sizeof(mp_digit), 0);
  {shift used digits, used>0!}
  move(a.pdigits^[0], b.pdigits^[cnt], a.used*sizeof(mp_digit));
  {clear low cnt digits, cnt>0!}
  fillchar(b.pdigits^, cnt*sizeof(mp_digit), 0);
  b.used := ns;
  mp_clamp(b);
  end;

function mp_cmp(const a,b: mp_int): Longint;
  {-Compare two mp_ints (signed), return sign(a-b)}
  begin
  // mp_cmp := 0;
  {compare based on signs}
  if a.sign<>b.sign then
    begin
    if a.sign=MP_NEG then mp_cmp := -1 else mp_cmp := 1;
    exit;
    end;
  {compare digits}
  if a.sign=MP_NEG then
    {if negative, compare opposite direction}
    mp_cmp := mp_cmp_mag(b, a)
  else
    mp_cmp := mp_cmp_mag(a, b);
  end;

procedure mp_sub(const a,b: mp_int; var c: mp_int);
  {-High level subtraction (handles signs)}
  var
    cmp: Longint;
  begin
  if a.sign<>b.sign then
    begin
    {subtract a negative from a positive, OR}
    {subtract a positive from a negative.   }
    {In either case, ADD their magnitudes,  }
    {and use the sign of the first number.  }
    c.sign := a.sign;
    s_mp_add(a, b, c);
    end
  else
    begin
    {subtract a positive from a positive, OR }
    {subtract a negative from a negative.    }
    {First, take the difference between their}
    {magnitudes, then... }
    {make s_mp_sub arguments unequal in magnitude}
    {and easy out our equal} {*0.1.29}
    cmp := mp_cmp_mag(a, b);
    if cmp=MP_EQ then mp_zero(c)
    else if cmp=MP_GT then
      begin
      {Copy the sign from the first}
      c.sign := a.sign;
      {The first has a larger or equal magnitude}
      s_mp_sub(a, b, c);
      end
    else
      begin
      {The result has the *opposite* sign from the first number}
      c.sign := a.sign xor (MP_NEG xor MP_ZPOS);
      {The second has a larger magnitude}
      s_mp_sub(b, a, c);
      end;
    end;
  end;

function EstimateQDigit(x2,x1,x0,y1,y0: mp_digit): mp_digit;
  {-Calculate q as in Knuth's Algorithm D, step D3}
  var
    q,w: mp_word;
  begin
  w := (mp_word(x2) shl DIGIT_BIT) or x1;
  q := w div y1;
  w := w mod y1;
  while (q>MP_MASK) or (q*y0 > ((w shl DIGIT_BIT) or x0)) do
    begin
    dec(q);
    inc(w,y1);
    if w>=MP_MASK then break;
    end;
  if q>MP_MASK then
    q := MP_MASK;
  EstimateQDigit := q;
  end;

procedure mp_mul_d(const a: mp_int; b: mp_digit; var c: mp_int);
  {-Multiply by a digit}
  var
    pa,pc: pmp_digit;
    u,r,bw: mp_word;
    n: TNInt;
    olduse: Longint;
  begin
  if b>MP_DIGIT_MAX then
    raise ERtcRSA.Create('mp_mul_d: b>MP_DIGIT_MAX');

  {if a=0 or b=0, set c=0 and exit}
  if (a.used=0) or (b=0) then
    begin
    mp_zero(c);
    exit;
    end;

  {trivial case b=1, just copy a}
  if b=1 then
    begin
    mp_copy(a,c);
    exit;
    end;

  {make sure c is big enough to hold a*b}
  if c.alloc < a.used + 1 then
    mp_grow(c, a.used+1);

  {get the original destinations used count}
  olduse := c.used;
  {set the sign}
  c.sign := a.sign;
  {setup pointers}
  pa := pmp_digit(a.pdigits);
  pc := pmp_digit(c.pdigits);

  {zero carry}
  u  := 0;
  bw := b;

  for n:=0 to a.used-1 do
    begin
    {compute product and carry sum for this term}
    r := pa^ * bw + u;
    inc(pa);
    {mask off higher bits to get a single digit}
    pc^ := r and MP_MASK;
    inc(pc);
    {send carry into next iteration}
    u := r shr DIGIT_BIT;
    end;

  {store final carry [if any]}
  pc^ := mp_digit(u);

  {set used count}
  c.used := a.used + 1;

  {now zero digits above the top}
  if c.used<olduse then
    begin
    pc := @c.pdigits^[c.used];
    for n:=c.used to olduse-1 do
      begin
      pc^ := 0;
      inc(pc);
      end;
    end;

  mp_clamp(c);
  end;

procedure s_mp_divrem_basecase(const a,b: mp_int; pc,pd: pmp_int);
  {-Longint signed division using Knuth's basecase algorithm D;    }
  { pc^ = a div b, pd^ = a rem b; sign(pd^)=sign(a); no init check.}
  var
    q, x, y, z: mp_int;
    n, t, i, it: TNInt;
    norm: Longint;
    asign, bsign: word;
    y0,y1: mp_digit;
  begin
  { c*b + d == a [e.g. a/b, c=quotient, d=remainder]}
  { HAC[5], algorithm 14.20 and Knuth[3] Algorithm D}

  {if |a| < |b| then c=0, d=a}
  if mp_cmp_mag(a,b)=MP_LT then
    begin
    if pd<>nil then mp_copy(a,pd^);
    if pc<>nil then mp_zero(pc^);
    exit;
    end;

  {remember signs}
  asign := a.sign;
  bsign := b.sign;

  if b.used<2 then
    begin
    {single digit b, possibly signed}
    {although b should be <> 0, let mp_div_d handle b=0 correctly}
    if b.used=0 then y0:=0 else y0:=b.pdigits^[0];
    s_mp_div_d(a,y0,pc,y0);
    if pd<>nil then
      begin
      {sign of remainder = sign(pc^) = sign(a)}
      mp_set(pd^,y0);
      if y0<>0 then pd^.sign := asign;
      end;
    if pc<>nil then with pc^ do
      begin
      if used<>0 then
        if asign=bsign then sign := MP_ZPOS else sign := MP_NEG;
      end;
    exit;
    end;

  {Here b.used>1 and a.used>1}

  {step 1: initialize q}
  mp_init_size(q, a.used + 2);
  mp_init_copy(x, a);
  mp_init_copy(y, b);
  mp_init(z);
  try
    q.used := a.used + 2;

    {make local copies positive}
    x.sign := MP_ZPOS;
    y.sign := MP_ZPOS;

    {step 2: normalize both x and y, ensure that y[t] >= b/2, [b=2^DIGIT_BIT]}
    {Note: LTM norm value is off by one in many cases. Fixed in V1.0.03}

    norm := mp_bitsize(y) mod DIGIT_BIT;
    if norm > 0 then
      begin
      norm := DIGIT_BIT - norm;
      mp_shl(x, norm, x);
      mp_shl(y, norm, y);
      end;

    n := x.used - 1;
    t := y.used - 1;

    {while (x >= y*b^n-t) do [ q[n-t] += 1; x -= y*b^(n-t)] }
    mp_lshd2(y, z, n-t);  {z = y*b^(n-t)}
    while mp_cmp(x, z)<>MP_LT do
      begin
      inc(q.pdigits^[n-t]);
      mp_sub(x, z, x);
      end;

    {get upper two digits of y for quotient digit estimation, t>1!}
    y0 := y.pdigits^[t-1];
    y1 := y.pdigits^[t];

    {step 3: for i from n downto (t + 1)}
    for i:=n downto t+1 do
      begin
      if i>x.used then continue;
      it := i-t-1;
      {step 3.1 and 3.2: estimate and adjust next quotient digit}
      q.pdigits^[it] := EstimateQDigit(x.pdigits^[i],x.pdigits^[i-1],x.pdigits^[i-2],y1,y0);
      {step 3.3: x = x - q[i-t-1] * y * b^(i-t-1)}
      mp_mul_d(y, q.pdigits^[it], z);
      mp_lshd(z, it);
      mp_sub(x, z, x);
      {if x < 0 then [ x = x + y*b^(i-t-1); q[i-t-1] -= 1; ]}
      if x.sign=MP_NEG then
        begin
        mp_lshd2(y, z, it);
        mp_add(x, z, x);
        {q[i-t-1] -= 1}
        q.pdigits^[it] := (q.pdigits^[it]+MP_MASK) and MP_MASK;
        end;
      end; {for i}

    if pc<>nil then
      begin
      {q is the positive quotient, make it negative if non-zero and sign(a)<>sign(b)}
      mp_clamp(q);
      if (asign<>bsign) and (q.used>0) then q.sign := MP_NEG;
      mp_exch(q, pc^);
      end;

    if pd<>nil then
      begin
      {x is the remainder which we have to sign-adjust and normalize}
      if x.used>0 then x.sign := asign;
      if norm<>0 then mp_shr(x, norm, x);
      mp_exch(x, pd^);
      end;
  finally
    mp_clear(y);
    mp_clear(x);
    mp_clear(z);
    mp_clear(q);
    end;
  end;

procedure s_mp_mod_2k(const a: mp_int; b: Longint; var c: mp_int);
  {-Calculate c = a mod 2^b, -(|a| mod 2^b) if a < 0}
  var
    n,m,ndig,nbit: TNInt;
    dp: pmp_digit;
  begin

  {Arg check in mp_zero/mp_copy}

  {if b is <= 0 then zero the int}
  if b<=0 then
    begin
    mp_zero(c);
    exit;
    end;

  mp_copy(a, c);

  {if the modulus is larger than the value then return}
  if b >= Longint(a.used) * DIGIT_BIT then exit;  {0.7.01: >=}
  {The bug was fixed in LTM 0.33 but unfortunately Tom did not document it}

  ndig := b div DIGIT_BIT;
  nbit := b mod DIGIT_BIT;
  m    := succ(ndig);

  {Flush all the bits above 2^d in its digit}
  dp := @c.pdigits^[ndig];
  dp^ := dp^ and mp_digit((1 shl nbit) - 1);

  if m<c.used then
    begin
    {*0.9.00}
    {Flush all digits above the one with 2^b in it}
    for n:=m to c.used-1 do
      begin
      inc(dp);
      dp^ := 0;
      end;
    c.used := m;
    end;

  mp_clamp(c);
  end;

procedure mp_setbit(var a: mp_int; n: Longint);
  {-Set bit n of a, error if n<0 or n>MP_MAXBIT (1 = bit 0)}
  var
    d,k: TNInt;
  begin
  if (n<0) or (n>MP_MAXBIT) then
    raise ERtcRSA.Create('mp_setbit: (n<0) or (n>MP_MAXBIT)');

  k := n div MP_DIGIT_BIT;
  d := mp_digit(n mod MP_DIGIT_BIT);

  if k>=a.used then
    begin
    {greater than current maximum used mp_digit}
    {grow a to accommodate the single bit}
    mp_grow(a,k+1);
    {set the used count of where the bit will go}
    a.used := k+1;
    end;
  a.pdigits^[k] := a.pdigits^[k] or (mp_digit(1) shl d);
  end;

procedure mp_2expt(var a: mp_int; b: Longint);
  {-Compute a = 2^b, b>=0,  error if b<0 or b>MP_MAXBIT}
  begin
  {all checking is done in the used routines}
  {clear all bits}
  mp_zero(a);
  {set the specified bit}
  mp_setbit(a,b);
  end;

procedure mp_mod_2k(const a: mp_int; b: Longint; var c: mp_int);
  {-Calculate c = a mod 2^b, 0 <= c < 2^b}
  var
    t: mp_int;
  begin
  {checks done in s_mp_mod_2k}
  s_mp_mod_2k(a,b,c);
  if c.sign=MP_NEG then
    begin
    mp_init(t);
    try
      mp_2expt(t,b);
      mp_add(c,t,c);
    finally
      mp_clear(t);
      end;
    end
  end;

procedure mp_or(const a,b: mp_int; var c: mp_int);
  {-Calculate c = a or b}
  var
    pa,pb,pc,px: pmp_digit;
    olduse, min, max: Longint;
    i: TNInt;
  begin
  if a.used > b.used then max := a.used else max := b.used;

  {easy outs}
  if max=0 then
    begin
    mp_zero(c);
    exit;
    end;
  if a.used=0 then
    begin
    mp_copy(b,c);
    exit;
    end;
  if b.used=0 then
    begin
    mp_copy(a,c);
    exit;
    end;

  {must grow before assigning pointer otherwise realloc can}
  {change the digits memory, ie px points to the old digits}
  if c.alloc < max then
    mp_grow(c, max);

  if a.used > b.used then
    begin
    min := b.used;
    px  := @a.pdigits^[min];
    end
  else
    begin
    min := a.used;
    px  := @b.pdigits^[min];
    end;

  pa  := pmp_digit(a.pdigits);
  pb  := pmp_digit(b.pdigits);
  pc  := pmp_digit(c.pdigits);

  {get old used digit count and set new one}
  olduse := c.used;
  c.used := max;
  c.sign := a.sign or b.sign;

  {process digits 0 .. min-1}
  for i:=0 to pred(min) do
    begin
    pc^ := pa^ or pb^;
    inc(pa);
    inc(pb);
    inc(pc);
    end;

  {copy digits min .. max-1}
  if min<max then
    for i:=min to pred(max) do
      begin
      pc^ := px^;
      inc(px);
      inc(pc);
      end;

  {clear c above max if necessary}
  if olduse>max then
    for i:=max to pred(olduse) do
      begin
      pc^ := 0;
      inc(pc);
      end;
  mp_clamp(c);
  end;

procedure mp_div_2k(const a: mp_int; b: Longint; var c: mp_int; pd: pmp_int);
  {-Divide by 2^b; quotient in c, optional remainder in pd^, sign(pd^)=sign(a)}
  var
    t: mp_int;
  begin
  {if a=0 or b<=0 then we do no work}
  if (b<=0) or (a.used=0) then
    begin
    mp_copy(a, c);
    if pd<>nil then mp_zero(pd^);
    exit;
    end;

  mp_init(t);
  try
    {get the remainder}
    if pd<>nil then
      s_mp_mod_2k(a, b, t);
    {get the quotient via mp_shr}
    mp_shr(a,b,c);
    if pd<>nil then mp_exch(t, pd^);
  finally
    mp_clear(t);
    end;
  end;

procedure mp_dec(var a: mp_int);
  {-Decrement an mp_int by 1}
  begin
  s_mp_sub_d(a,1,a);
  end;

procedure s_mp_fakeinit(const a: mp_int; i0,i1: Longint; var b: mp_int);
  {-Make a positive fake mp_int b = a[i0..i1], b=0 if i0>=a.used, or i0>i1.}
  { Internal use only, no init check of a. DANGER: b uses the memory of a, }
  { therefore use b only as and on CONST parameter, DO NOT clear or GROW b!}
  begin
  with b do
    begin
    if (i0>=a.used) or (i0>i1) then
      begin
      pdigits := a.pdigits;
      used    := 0;
      end
    else
      begin
      {here a.used>0, and i0<a.used}
      if i1>=a.used then i1 := a.used-1;
      pdigits := PDigitArray(@a.pdigits^[i0]);
      used := 1+i1-i0;
      end;
    alloc  := used;
    sign   := MP_ZPOS;
    magic  := MP_MAGIC;
    mp_clamp(b);
    end;
  end;

procedure mp_shl1(var a: mp_int);
  {-Shift left a by 1}
  var
    shift, r: mp_digit;
    rr: mp_digit;
    pc: pmp_digit;
    i: TNInt;
  begin
  if a.used=0 then exit;

  if a.alloc<a.used+1 then
    if a.pdigits^[pred(a.used)] > (MP_DIGIT_MAX shr 1) then
      mp_grow(a, a.used + 1);

  {bitmask for carries}
  shift := DIGIT_BIT - 1;
  pc := pmp_digit(a.pdigits);

  {carry}
  r := 0;
  {don't waste time on the zero digits we have just created}
  {with mp_lshd, ie don't touch digits 0..bd-1}
  for i:=0 to a.used-1 do
    begin
    {get the higher bits of the current word}
    rr := (pc^ shr shift) and 1;
    {shift the current word and OR in the carry}
    pc^ := ((pc^ shl 1) or r) and MP_MASK;
    inc(pc);
    {set the carry to the carry bits of the current word}
    r := rr;
    end;
  {set final carry}
  if r<>0 then
    begin
    pc^ := r;
    inc(a.used);
    end;
  end;

procedure mp_shr1(var a: mp_int);
  {-Divide a by 2, a = a/2}
  var
    i: TNInt;
    r,rr: mp_digit;
    pa: pmp_digit;
  begin
  if a.used=0 then exit;

  pa := @a.pdigits^[pred(a.used)];
  {carry}
  r  := 0;
  for i:=pred(a.used) downto 0 do
    begin
    {get the carry for the next iteration}
    rr := pa^ and 1;
    {shift the current digit, add in carry and store}
    pa^ := (pa^ shr 1) or (r shl (DIGIT_BIT - 1));
    {forward carry to next iteration}
    r := rr;
    dec(pa);
    end;
  mp_clamp(a);
  end;

procedure mp_sqr(const a: mp_int; var b: mp_int); forward;

procedure s_mp_toom3_sqr(const a: mp_int; var b: mp_int);
  {-Compute b = a*a using Toom-3 squaring}
  var
    w0, w1, w2, w3, a0, a1, a2: mp_int;
    n: TNInt;
    d: mp_digit;
  begin
  {See s_mp_toom3_mul for algorithm and references}
  n := a.used;
  if n=0 then
    begin
    mp_zero(b);
    exit;
    end;

  {Note b is used as w4}
  mp_init(w0);
  mp_init(w1);
  mp_init(w2);
  mp_init(w3);
  try
    n  := (n+2) div 3;

    {B = 2^(n*DIGIT_BIT)}
    {|a| = a2 * B^2 + a1 * B + a0}
    s_mp_fakeinit(a,  0,     pred(n),a0);
    s_mp_fakeinit(a,  n,   pred(n+n),a1);
    s_mp_fakeinit(a,n+n,pred(a.used),a2);

    {w3 = a0+a2}
    s_mp_add(a0,a2,w3);
    {w1 = w3-a1 = a2-a1+a0}
    mp_sub(w3, a1, w1);
    {w3 = w3+a1 = a2+a1+a0}
    s_mp_add(w3, a1, w3);
    {w2 = w3^2}
    mp_sqr(w3, w2);
    {w3 = 2(w3+a2)-a0 = 4a2+2a1+a0}
    s_mp_add(w3, a2, w3);
    mp_shl1(w3);
    s_mp_sub(w3, a0, w3);
    {w1 = w1^2}
    mp_sqr(w1, w1);
    {w3 = w3^2}
    mp_sqr(w3, w3);
    {w0 = a0^2}
    mp_sqr(a0, w0);
    {w4 = a2^2}
    mp_sqr(a2, b);

    {As for multiplication solve the linear system

     | 0  0  0  0  1 |   |x0|    |w0|
     | 1 -1  1 -1  1 |   |x1|    |w1|
     | 1  1  1  1  1 | * |x2|  = |w2|
     | 16 8  4  2  1 |   |x3|    |w3|
     | 1  0  0  0  0 |   |x4|    |w4|

     for x[i] using 9 subs, 2 shifts, 1 small div.
     x[i] will overlay w[i], obviously x0=w0, x4=w4.
    }

    {w3 = (w3-w1)/3}
    mp_sub(w3, w1, w3);
    s_mp_div_d(w3, 3, @w3, d);
    {w1 = (w2-w1)/2}
    mp_sub(w2, w1, w1);
    mp_shr1(w1);
    {w2 = w2-w0}
    s_mp_sub(w2, w0, w2);
    {w3 = (w3-w2)/2 - 2*w4}
    s_mp_sub(w3, w2, w3);
    mp_shr1(w3);
    s_mp_sub(w3, b, w3);
    s_mp_sub(w3, b, w3);
    {w2 = w2-w1-w4}
    s_mp_sub(w2,  b, w2);
    s_mp_sub(w2, w1, w2);
    {w1 = w1-w3}
    s_mp_sub(w1, w3, w1);

    {now grow b if necessary and 'multiply' by powers of B}
    if b.alloc < (a.used shl 1) then mp_grow(b,a.used shl 1);
    {b = w4*B^4 + w3*B^2 + w2*B^2 + w1*B + w0}
    {  = (((w4*B + w3)*B + w2)*B + w1)*B + w0}
    mp_lshd(b,n);   s_mp_add(b,w3,b);
    mp_lshd(b,n);   s_mp_add(b,w2,b);
    mp_lshd(b,n);   s_mp_add(b,w1,b);
    mp_lshd(b,n);   s_mp_add(b,w0,b);
  finally
    mp_clear(w0);
    mp_clear(w1);
    mp_clear(w2);
    mp_clear(w3);
    end;
  end;

procedure s_mp_karatsuba_sqr(const a: mp_int; var b: mp_int);
  {-Karatsuba squaring, compute b = a*a using three half size squarings}
  var
    n,m: TNInt;
    a0,a1: mp_int;
    t0,t1: mp_int;
  begin
  {See comments of s_mp_karatsuba_mul for details. It is essentially }
  {the same algorithm but merely tuned to perform recursive squarings}

  if a.used=0 then
    begin
    mp_zero(b);
    exit;
    end;

  n := succ(a.used) div 2;
  {B = 2^(n*DIGIT_BIT)}

  mp_init_size(t0,2*n+4);
  mp_init_size(t1,2*n+4);
  try
    {Since @a=@b is allowed, remember size of product}
    m := a.used shl 1;

    {|a| = a1 * B + a0}
    s_mp_fakeinit(a,  0,     pred(n),a0);
    s_mp_fakeinit(a,  n,pred(a.used),a1);

    {b = a1^2*B^2 + ((a1+a0)^2 - a1^2 - a0^2)*B + a0^2}
    {  = b2*B^2 + b1*B + b0}

    s_mp_add(a1, a0, t1);      {t1 = a1 + a0}

    {now recursively compute the half size squares}
    mp_sqr(t1, t1);            {t1 = (a1 + a0)^2}
    mp_sqr(a0, t0);            {t2 = a0^2}
    mp_sqr(a1, b);             { b = a1^2}

    {compute middle term ('coefficient' of B)}
    s_mp_sub(t1, t0, t1);
    s_mp_sub(t1,  b, t1);      {t1 = (a1+a0)^2 - a0^2 - a1^2}

    {now grow b if necessary and 'multiply' by powers of B}
    if b.alloc < m then mp_grow(b,m);
    mp_lshd(b, n);
    s_mp_add(b,t1,b);          {b = b2*B + b1}
    mp_lshd(b, n);
    s_mp_add(b,t0,b);          {b = (b2*B + b1)*B + b0}
  finally
    mp_clear(t0);
    mp_clear(t1);
    end;
  end;

procedure s_mp_sqr(const a: mp_int; var b: mp_int);
  {-Low level squaring, b = a*a, HAC pp.596-597, algorithm 14.16}
  var
    t: mp_int;
    ix, iy: TNInt;
    pa: Longint;
    r: mp_word;
    tmpx,u: mp_digit;
    pt,py: pmp_digit;
  begin
  pa := a.used;
  if pa<1 then
    begin
    mp_zero(b);
    exit;
    end;

  mp_init_size(t, 2*pa+1);
  try
    {default used is maximum possible size}
    t.used := 2*pa + 1;

    for ix:=0 to pa-1 do
      begin
      {first calculate the digit at 2*ix}
      tmpx := a.pdigits^[ix];
      {calculate double precision result}
      {*we: sqr(int64) produces failures!?}
      r := mp_word(tmpx)*mp_word(tmpx) + t.pdigits^[2*ix];

      {store lower part in result}
      t.pdigits^[2*ix] := mp_digit(r and MP_MASK);
      {get the carry}
      u := mp_digit(r shr DIGIT_BIT);

      {left hand side of A[ix] * A[iy]}
      {alias for where to store the results}
      pt := pmp_digit(@t.pdigits^[2*ix+1]);
      py := pmp_digit(@a.pdigits^[ix+1]);

      for iy:=1 to pa-ix-1 do
        begin
        {first calculate the product}
        r := mp_word(tmpx)*mp_word(py^);
        {now calculate the double precision result, note we use}
        {addition instead of *2 since it's easier to optimize}
        r  :=  mp_word(pt^)+ r + r + mp_word(u);
        { store lower part}
        pt^ := mp_digit(r and MP_MASK);
        inc(py);
        inc(pt);
        u := mp_digit(r shr DIGIT_BIT);
        end;
      {propagate upwards}
      while u<>0 do
        begin
        r := mp_word(pt^) + mp_word(u);
        pt^ := mp_digit(r and MP_MASK);
        inc(pt);
        u := mp_digit(r shr DIGIT_BIT);
        end;
      end;

    mp_clamp(t);
    mp_exch(t, b);
  finally
    mp_clear(t);
    end;
  end;

procedure mp_sqr(const a: mp_int; var b: mp_int);
  {-Compute b = a*a}
  begin
  {Debug checking in called functions}
  if a.used >= mp_t3s_cutoff then s_mp_toom3_sqr(a, b)
  else if a.used >= mp_sqr_cutoff then s_mp_karatsuba_sqr(a, b)
  else s_mp_sqr(a, b);
  b.sign := MP_ZPOS;
  end;

procedure mp_mul(const a,b: mp_int; var c: mp_int); forward;

procedure s_mp_toom3_mul(const a,b: mp_int; var c: mp_int);
  {-Calculate c = |a| * |b| using Toom-3 multiplication}
  var
    w0,w1,w2,w3,w4,a0,a1,a2,b0,b1,b2: mp_int;
    n: TNInt;
    d: mp_digit;
  begin
  {Multiplication using the Toom-Cook 3-way algorithm, i.e. splitting  }
  {|a| and |b| in three parts, |a| = a2*B^2 + a1*B + a0, similar for b,}
  {with B ~ 2^(bitsize(max(|a|,|b|)/3). Toom-3 is more complicated than}
  {Karatsuba (~Toom-2) but has smaller asymptotic complexity O(n^1.465)}
  {More info: [1]/doc Ch. 5.3.5, [3] Ch. 4.3.3, and [35] Ch 1.3.3. This}
  {implementation uses M. Bodrato's [36] optimal Toom-3 code. See also }
  {http://bodrato.it/toom-cook/ and http://en.wikipedia.org/wiki/Toom3 }

  if (a.used=0) or (b.used=0) then
    begin
    mp_zero(c);
    exit;
    end;

  {Use splitting parameter B ~ 2^(bitsize(max(|a|,|b|)/3). Note that TSD [1]}
  {uses min(|a|,|b|), but this is inferior and often slower than Karatsuba.}

  if a.used > b.used then n := a.used else n := b.used;
  n  := (n+2) div 3;

  mp_init(w0);
  mp_init(w1);
  mp_init(w2);
  mp_init(w3);
  mp_init(w4);
  try
    {B = 2^(n*DIGIT_BIT)}

    {|a| = a2 * B^2 + a1 * B + a0}
    s_mp_fakeinit(a,  0,     pred(n),a0);
    s_mp_fakeinit(a,  n,   pred(n+n),a1);
    s_mp_fakeinit(a,n+n,pred(a.used),a2);
    {|b| = b2 * B^2 + b1 * B + b0}
    s_mp_fakeinit(b,  0,     pred(n),b0);
    s_mp_fakeinit(b,  n,   pred(n+n),b1);
    s_mp_fakeinit(b,n+n,pred(b.used),b2);

    {w0 = a0+a2}
    s_mp_add(a0, a2, w0);
    {w4 = b0+b2}
    s_mp_add(b0, b2, w4);
    {w3 = w0-a1 = a2-a1+a0}
    mp_sub(w0, a1, w3);
    {w0 = w0+a1 = a2+a1+a0}
    s_mp_add(w0, a1, w0);
    {w2 = w4-b1 = b2-b1+b0}
    mp_sub(w4, b1, w2);
    {w4 = w4+b1 = b2+b1+b0}
    s_mp_add(w4, b1, w4);
    {w1 = w3*w2}
    mp_mul(w3, w2, w1);
    {w2 = w0*w4}
    mp_mul(w0, w4, w2);
    {w0 = 2(w0+a2)-a0 = 4a2+2a1+a0}
    s_mp_add(w0, a2, w0);
    mp_shl1(w0);
    s_mp_sub(w0, a0, w0);
    {w4 = 2(w4+b2)-b0 = 4b2+2b1+b0}
    s_mp_add(w4, b2, w4);
    mp_shl1(w4);
    s_mp_sub(w4, b0, w4);
    {w3 = w0*w4}
    mp_mul(w0, w4, w3);
    {w0 = a0*b0}
    mp_mul(a0, b0, w0);
    {w4 = a2*b2}
    mp_mul(a2, b2, w4);

    {now solve the linear system

     | 0  0  0  0  1 |   |x0|    |w0|
     | 1 -1  1 -1  1 |   |x1|    |w1|
     | 1  1  1  1  1 | * |x2|  = |w2|
     | 16 8  4  2  1 |   |x3|    |w3|
     | 1  0  0  0  0 |   |x4|    |w4|

     for x[i] using 9 subs, 2 shifts, 1 small div.
     x[i] will overlay w[i], obviously x0=w0, x4=w4.
    }

    {w3 = (w3-w1)/3}
    mp_sub(w3, w1, w3);
    s_mp_div_d(w3, 3, @w3, d);
    {w1 = (w2-w1)/2}
    mp_sub(w2, w1, w1);
    mp_shr1(w1);
    {w2 = w2-w0}
    s_mp_sub(w2, w0, w2);
    {w3 = (w3-w2)/2 - 2*w4}
    s_mp_sub(w3, w2, w3);
    mp_shr1(w3);
    s_mp_sub(w3, w4, w3);
    s_mp_sub(w3, w4, w3);
    {w2 = w2-w1-w4}
    s_mp_sub(w2, w4, w2);
    s_mp_sub(w2, w1, w2);
    {w1 = w1-w3}
    s_mp_sub(w1, w3, w1);

    {now grow c if necessary and 'multiply' by powers of B}
    if c.alloc < a.used + b.used then mp_grow(c,a.used + b.used);
    {c = w4*B^4 + w3*B^2 + w2*B^2 + w1*B + w0}
    {  = (((w4*B + w3)*B + w2)*B + w1)*B + w0}
    mp_lshd2(w4,c,n);  s_mp_add(c,w3,c);
    mp_lshd(c,n);      s_mp_add(c,w2,c);
    mp_lshd(c,n);      s_mp_add(c,w1,c);
    mp_lshd(c,n);      s_mp_add(c,w0,c);
  finally
    mp_clear(w0);
    mp_clear(w1);
    mp_clear(w2);
    mp_clear(w3);
    mp_clear(w4);
    end;
  end;

procedure s_mp_karatsuba_mul(const a,b: mp_int; var c: mp_int);
  {-Calculate c = |a| * |b| using Karatsuba multiplication}
  var
    n,m: TNInt;
    a0,a1,b0,b1: mp_int;
    t1,t2: mp_int;
  begin
  {c = |a| * |b| with Karatsuba multiplication using 3 half size}
  {multiplications. With B ~ 2^(mp_bitsize(max(|a|,|b|)/2) write}
  { |a| = a1*B + a0 and |b| = b1*B + b0. Then |a * b| =         }
  { a1b1 * B^2 + ((a1 + a0)(b1 + b0) - (a0b0 + a1b1))*B + a0b0  }
  {This divide-and-conquer algorithm leads to the O(n^lg(3)) or }
  {O(n^1.584) asymptotics (for balanced arguments with n digits)}

  {Contrary to LTM use maximum of digits}
  if b.used>a.used then n:=b.used else n:=a.used;
  if n=0 then
    begin
    mp_zero(c);
    exit;
    end;

  n  := (n+1) div 2;
  {B = 2^(n*DIGIT_BIT)}

  mp_init_size(t1,2*n+4);
  mp_init_size(t2,2*n+4);
  try
    {Since @a=@c or @b=@c is allowed, remember size of product}
    m := a.used+b.used;

    {|a| = a1 * B + a0}
    s_mp_fakeinit(a,  0,     pred(n),a0);
    s_mp_fakeinit(a,  n,pred(a.used),a1);

    {|b| = b1 * B + b0}
    s_mp_fakeinit(b,  0,     pred(n),b0);
    s_mp_fakeinit(b,  n,pred(b.used),b1);

    {c = a1*b1*B^2 + ((a1+a0)*(b1+b0) - a1*b1 - a0*b0)*B + a0*b0}
    {  = c2*B^2 + c1*B + c0}

    s_mp_add(a1, a0, t1);      {t1 = a1 + a0}
    s_mp_add(b1, b0, t2);      {t2 = b1 + b0}
    mp_mul(t1, t2, t1);        {t1 = (a1 + a0) * (b1 + b0)}

    {now calc the products c0 = a0*b0 and c2 = a1*b1}
    mp_mul(a0, b0, t2);        {t2 = a0*b0}
    mp_mul(a1, b1, c);         { c = a1*b1}


    {compute c1. s_mp_sub can be used because (a1+a0)*(b1+b0) > a1*b1 + a0*b0}
    s_mp_sub(t1, t2, t1);
    s_mp_sub(t1,  c, t1);      {t1 = (a1+a0)*(b1+b0) - a1*b1 - a0*b0}

    {now grow c if necessary and 'multiply' by powers of B}
    if c.alloc < m then mp_grow(c,m);
    mp_lshd(c, n);
    s_mp_add(c,t1,c);          {c = c2*B + c1}
    mp_lshd(c, n);
    s_mp_add(c,t2,c);          {c = (c2*B + c1)*B + c0}
  finally
    mp_clear(t1);
    mp_clear(t2);
    end;
  end;

procedure s_mp_mul_digs(const a,b: mp_int; var c: mp_int; digs: Longint);
  {-Multiply |a|*|b| and only compute up to digs digits of result}
  { HAC pp. 595, algorithm 14.12  Modified so you can control how   }
  { many digits of output are created.                              }
  var
    r: mp_word;
    u, tmpx: mp_digit;
    pt, py: pmp_digit;
    my, ix, iy: TNInt;
    minu: Longint;
    t: mp_int;
  begin
  if a.used<b.used then minu:=a.used else minu:=b.used;

  {Product is zero?}
  if (minu=0) or (digs=0) then
    begin
    mp_zero(c);
    exit;
    end;

  {*Note: Comba canceled because MP_WARRAY too small for 8/16 bit}
  mp_init_size(t, digs);
  try
    t.used := digs;

    {compute the digits of the product directly}
    for ix:=0 to a.used-1 do
      begin
      {copy of the digit from a used within the nested loop}
      tmpx := a.pdigits^[ix];
      {skip loop if multiplier is zero, *V1.1.09. This is OK}
      {because init_size has already zero-filled  t.pdigits^}
      if (tmpx<>0) and (digs>ix) then
        begin
        {limit ourselves to making digs digits of output}
        my := digs-ix;
        if b.used<my then my:=b.used;
        {an alias for the destination shifted ix places}
        pt := @t.pdigits^[ix];
        {an alias for the digits of b }
        py := pmp_digit(b.pdigits);
        {set the carry to zero}
        u := 0;
        {compute the columns of the output and propagate the carry}
        for iy:=0 to my-1 do
          begin
          {compute the column as a mp_word}
          r := mp_word(pt^) + mp_word(tmpx)*mp_word(py^) + mp_word(u);
          inc(py);
          {the new column is the lower part of the result}
          pt^ := mp_digit(r and mp_word(MP_MASK));
          inc(pt);
          {get the carry word from the result}
          u := mp_digit(r shr mp_word(DIGIT_BIT));
          end;
        {set carry if it is placed below digs}
        {we: use my because iy may undefined}
        if ix+my < digs then pt^ := u;
        end;
      end;
    mp_clamp(t);
    mp_exch(t, c);
  finally
    mp_clear(t);
    end;
  end;

procedure mp_mul(const a,b: mp_int; var c: mp_int);
  {-High level multiplication, c = a*b}
  var
    s,t,ai,bb: mp_int;
    pa,pb: pmp_int;
    n,minu,maxu,i0,i1,i: Longint;
    sig: word;
  begin
  if @a=@b then
    begin
    mp_sqr(a,c);
    exit;
    end;

  {get result sign, calculated product sign is not used}
  if a.sign=b.sign then sig:=MP_ZPOS else sig:=MP_NEG;

  {get size/addr of arguments. pa points to larger, pb to smaller factor}
  if a.used >= b.used then
    begin
    pa := @a;
    pb := @b;
    maxu := a.used;
    minu := b.used;
    end
  else
    begin
    pb := @a;
    pa := @b;
    minu := a.used;
    maxu := b.used;
    end;

  if (minu < mp_mul_cutoff) or (maxu < 2*minu) then
    begin
    {small or approximately balanced factors}
    if minu=0 then
      begin
      mp_zero(c);
      exit;
      end;
    if minu >= mp_t3m_cutoff then s_mp_toom3_mul(a, b, c)
    else if minu >= mp_mul_cutoff then s_mp_karatsuba_mul(a, b, c)
    else
      begin
      {optimization for single digit factors, note: a,b <>0}
      if a.used=1 then
        begin
        {sign adjust is done below}
        mp_mul_d(b,a.pdigits^[0],c);
        end
      else if b.used=1 then
        begin
        {sign adjust is done below}
        mp_mul_d(a,b.pdigits^[0],c);
        end
      else
        s_mp_mul_digs(a, b, c, a.used + b.used + 1);
      end;
    end
  else
    begin
    {Large and unbalanced factors: partition larger factor 'a' in chunks of}
    {size equal to smaller factor a = a[n]*X^n .. a[1]*X + a[0] and compute}
    {a*b as (..((a[n]*b*X + a[n-1])*X .. )*X + a[0], X=2^(b.used*DIGIT_BIT)}
    mp_init(s);
    mp_init(t);
    try
      n := maxu div minu;
      i0 := n*minu;
      i1 := i0+pred(minu);
      {make local positive copy of b}
      bb := pb^;
      bb.sign := MP_ZPOS;
      {make fake chunk a[n]}
      s_mp_fakeinit(pa^,i0,i1,ai);
      mp_mul(ai,bb,s);
      for i:=pred(n) downto 0 do
        begin
        dec(i0,minu);
        dec(i1,minu);
        {ai represents a[i]}
        s_mp_fakeinit(pa^,i0,i1,ai);
        mp_mul(ai,bb,t);
        {multiply by X}
        mp_lshd(s,minu);
        mp_add(s,t,s);
        end;
      mp_exch(s,c);
    finally
      mp_clear(s);
      mp_clear(t);
      end;
    end;
  {adjust result sign}
  if c.used=0 then c.sign := MP_ZPOS else c.sign := sig;
  end;

procedure bz_d2n1n(const a,b: mp_int; n: Longint; var q,r: mp_int); forward;

procedure bz_d3n2n(const a12,a3,b,b1,b2: mp_int; n: Longint; sb: boolean; var q,r: mp_int);
  {-Divide 3n-bit (a12,a3) by 2n-bit b=(b1,b2), using a indirect recursion via}
  { bz_d2n1n; a,b positive, b<>0, a<2^n*b, internal, no init check; @r=@a12 is}
  { allowed. sb is true if (b1,b2)=2b, ie in the odd(n) case of bz_d2n1n, used}
  { for correction if a - (q*b+r) is negative.}
  var
    t: mp_int;
  begin
  mp_init(t);
  try
    mp_shr(a12,n,t);
    if mp_cmp_mag(t,b1) < 0 then
      begin
      {tricky: using a12 directly in bz_d2n1n does not work}
      mp_copy(a12,t);
      bz_d2n1n(t,b1,n,q,r);
      end
    else
      begin
      mp_shl(b1,n,t);
      mp_sub(a12,t,r);
      mp_add(r,b1,r);
      mp_2expt(q, n);
      mp_dec(q);
      end;
    mp_mul(q,b2,t);
    mp_shl(r,n,r);
    mp_or(r,a3,r);
    mp_sub(r,t,r);
    if r.sign=MP_NEG then
      begin
      {add back, use b or 2b depending on sb = (shift b flag)}
      if sb then mp_shl(b,1,t)
      else mp_copy(b,t);
      repeat
        mp_dec(q);
        mp_add(r,t,r);
        until r.sign=MP_ZPOS;
      end;
  finally
    mp_clear(t);
    end;
  end;

procedure bz_d2n1n(const a,b: mp_int; n: Longint; var q,r: mp_int);
  {-Divide 2n-bit a by n-bit b, using a indirect recursion via bz_d3n2n}
  { a,b positive, b<>0, a<2^n*b, internal, no init check}
  var
    b1,b2,q1,t: mp_int;
    nh: Longint;
  begin
  if b.used<=mp_bz_cutoff then
    begin
    {use Knuth's basecase algorithm D}
    s_mp_divrem_basecase(a,b,@q,@r);
    exit;
    end;
  mp_init(b1);
  mp_init(b2);
  mp_init(q1);
  mp_init(t);
  try
    if n and 1 = 0 then
      begin
      {n is even, divide (a1,a2,a3) by (b1,b2)}
      nh := n div 2;
      mp_div_2k(b,nh,b1,@b2);
      mp_shr(a,n,t);
      mp_shr(a,nh,q);
      mp_mod_2k(q,nh,q);
      bz_d3n2n(t,q,b,b1,b2,nh,false,q1,r);
      mp_mod_2k(a,nh,t);
      bz_d3n2n(r,t,b,b1,b2,nh,false,q,r);
      mp_shl(q1,nh,q1);
      mp_or(q1,q,q);
      end
    else
      begin
      {n is odd, the original Burnikel-Ziegler uses baseline division in this}
      {case. We follow the Python idea and divide 2*(a1,a2,a3) by 2*(b1,b2), }
      {but the shifts are NOT applied to a/b, they are virtual or with temps.}
      inc(n);
      nh := n div 2;
      mp_shl(b,1,t);
      mp_div_2k(t,nh,b1,@b2);
      mp_shl(a,1,t);
      mp_shr(t,n,q1);
      mp_shr(t,nh,q);
      mp_mod_2k(q,nh,q);
      bz_d3n2n(q1,q,b,b1,b2,nh,true,q1,r);
      mp_mod_2k(t,nh,t);
      bz_d3n2n(r,t,b,b1,b2,nh,true,q,r);
      mp_shl(q1,nh,q1);
      mp_or(q1,q,q);
      {q is OK, but remainder must be halved}
      mp_shr1(r);
      end;
  finally
    mp_clear(b1);
    mp_clear(b2);
    mp_clear(q1);
    mp_clear(t);
    end;
  end;

procedure bz_divrem_pos(const a,b: mp_int; calcq: boolean; var q,r: mp_int);
  {-Divide a by b using Burnikel-Ziegler algorithm, a,b>0 e; calcq=true if q}
  { shall be calculated. q,r: no overlap with a,b; internal use, no checks.}
  var
    i,m,n: Longint;
    s,t: mp_int;
  begin
  {Ref: C. Burnikel, J. Ziegler [33]. See also fast_div.py}
  {by M. Dickinson from http://bugs.python.org/issue3451  }

  n := mp_bitsize(b);
  m := mp_bitsize(a) div n;
  mp_init(s);
  mp_init(t);
  try
    if calcq then mp_zero(q);
    mp_shr(a,n*m,r);
    mp_mod_2k(r,n,r);
    for i:=m-1 downto 0 do
      begin
      mp_shr(a,n*i,t);
      mp_mod_2k(t,n,t);
      mp_shl(r,n,r);
      mp_or(r,t,t);
      bz_d2n1n(t,b,n,s,r);
      if calcq then
        begin
        {accumulate quotient only if requested}
        mp_shl(q,n,q);
        mp_or(q,s,q);
        end;
      end;
  finally
    mp_clear(s);
    mp_clear(t);
    end;
  end;

procedure s_mp_divrem(const a,b: mp_int; pc,pd: pmp_int);
  {-Longint signed division using recursive Burnikel-Ziegler algorithm:}
  { pc^ = a div b, pd^ = a rem b; sign(pd^)=sign(a); no init check.}
  { Knuth's algorithm D is used for bitsizes < mp_bz_cutoff.}
  var
    aa,bb,q,r: mp_int;
    asign, bsign: word;
  begin

  if (a.used<=b.used) or (b.used<=mp_bz_cutoff) or ((a.used-b.used)<=mp_bz_cutoff) then
    begin
    {use Knuth's basecase algorithm D}
    s_mp_divrem_basecase(a,b,pc,pd);
    exit;
    end;

  mp_init(q);
  mp_init(r);
  try
    {remember signs}
    asign := a.sign;
    bsign := b.sign;

    {make local positive copies of const parameters}
    aa := a; aa.sign := MP_ZPOS;
    bb := b; bb.sign := MP_ZPOS;

    {call Burnikel-Ziegler algorithm for positive integers}
    bz_divrem_pos(aa,bb,pc<>nil,q,r);

    if pc<>nil then
      begin
      {q is the positive quotient, make it negative if non-zero and sign(a)<>sign(b)}
      if (asign<>bsign) and (q.used>0) then q.sign := MP_NEG;
      mp_exch(q, pc^);
      end;

    if pd<>nil then
      begin
      {r is the remainder which we have to sign-adjust and normalize}
      if r.used>0 then r.sign := asign;
      mp_exch(r, pd^);
      end;
  finally
    mp_clear(q);
    mp_clear(r);
    end;
  end;

procedure mp_mod(const a,b: mp_int; var c: mp_int);
  {-Calculate c = a mod b, 0 <= c < b}
  var
    t: mp_int;
    s: Longint;
  begin
  if mp_is_longint(b,s) then
    begin
    {b fits into a Longint and we do not need a quotient here, so use mp_mod_int}
    mp_mod_int(a,s,s);
    mp_set_int(c,s);
    exit;
    end;

  mp_init(t);
  try
    {call Burnikel/Ziegler or Knuth's basecase algorithm D}
    s_mp_divrem(a, b, nil, @t);
    {don't adjust sign if result=0}
    if (t.used>0) and (t.sign<>b.sign) then mp_add(b, t, c)
    else mp_exch(t, c);
  finally
    mp_clear(t);
    end;
  end;

function __gcd32(A, B: Cardinal): Cardinal;
  {-Calculate GCD of unsigned (A,B)}
  var
    T: Cardinal;
  begin
  if A=0 then __gcd32 := B
  else
    begin
    while B<>0 do
      begin
      T := B;
      B := A mod T;
      A := T;
      end;
    __gcd32 := A;
    end;
  end;

function gcd32u(A, B: Longint): Longint;
  {-Calculate GCD of two longints (DWORD interpretation)}
  begin
  gcd32u := Longint(__gcd32(Cardinal(A), Cardinal(B)));
  end;

function gcd32(A, B: Longint): Longint;
  {-Calculate GCD of two longints}
  begin
  gcd32 := Longint(__gcd32(Cardinal(abs(A)), Cardinal(abs(B))));
  end;

function mp_cnt_lsb(const a: mp_int): Longint;
  {-Count the number of least significant bits which are zero}
  var
    x: Longint;
    q, qq: mp_digit;
  const
    lnz: array[0..15] of word = (4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0);
  begin
  mp_cnt_lsb := 0;
  {easy out}
  if mp_iszero(a) then exit;

  {scan lower digits until non-zero}
  x := 0;
  while (x<a.used) and (a.pdigits^[x]=0) do inc(x);

  q := a.pdigits^[x];
  x := x*DIGIT_BIT;

  {now scan this digit until a 1 is found}
  if q and 1 = 0 then
    begin
    repeat
      qq := q and 15;
      inc(x,lnz[qq]);
      q := q shr 4;
      until qq<>0;
    end;
  mp_cnt_lsb := x;
  end;

procedure mp_gcd(const a,b: mp_int; var c: mp_int);
  {-Calculate c = gcd(a,b) using the binary method}
  var
    u,v: mp_int;
    k,u_lsb,v_lsb: Longint;
    ui,vi: Longint;
  begin
  {if one arg is zero, gcd is the abs of the other}
  if mp_iszero(a) then
    begin
    mp_abs(b, c);
    exit;
    end
  else if mp_iszero(b) then
    begin
    mp_abs(a, c);
    exit;
    end;

  {get copies of a and b we can modify}
  mp_init(u);
  mp_init(v);
  try
    {u,v are positive for the remainder of the algorithm}
    mp_abs(a,u);
    mp_abs(b,v);

    {Do a single initial modular reduction if the sizes of u and v}
    {are very different, see H.Cohen's Alg. 1.3.5 in [24]}
    {u>0, v>0. Make u >= v}
    if (u.used < v.used) or (mp_cmp_mag(u,v)=MP_LT) then mp_exch(u,v);
    if u.used > v.used+1 then
      begin
      {do the initial reduction}
      mp_mod(u,v,u);
      {done in u mod v = 0}
      if u.used=0 then
        begin
        {store result and clear local vars}
        mp_exch(c,v);
        Exit;
        end;
      if mp_is_longint(u,ui) then
        begin
        {u <>0 but fits into Longint}
        mp_mod_int(v,ui,vi);
        mp_set_int(c,gcd32(ui,vi));
        Exit;
        end;
      end;

    {Here both u and v are > 0; find the common power of two for u and v}
    u_lsb := mp_cnt_lsb(u);
    v_lsb := mp_cnt_lsb(v);
    if u_lsb<v_lsb then k:=u_lsb else k:=v_lsb;

    {Make both u and v odd}
    if u_lsb>0 then mp_shr(u, u_lsb, u);
    if v_lsb>0 then mp_shr(v, v_lsb, v);

    while (v.used>0) do
      begin
      if mp_is_longint(v,vi) then
        begin
        mp_mod_int(u,vi,ui);
        mp_set_int(u,gcd32(ui,vi));
        break;
        end;
      {done if u=v, if u>v swap u and v}
      if u.used>=v.used then
        begin
        case mp_cmp_mag(u,v) of
          MP_GT: mp_exch(u, v);
          MP_EQ: break;
          end;
        end;
      {subtract smaller from larger, resulting v is > 0}
      s_mp_sub(v, u, v);
      if v.pdigits^[0] and 2 <>0 then
        begin
        {Only one factor two can be removed, so reduce overhead. This}
        {occurs about one-half of the time, see Knuth [3] 4.5.2, p348}
        mp_shr1(v);
        end
      else
        begin
        {Divide out all factors of two}
        mp_shr(v, mp_cnt_lsb(v), v);
        end;
      end;

    {multiply by 2^k which we divided out at the beginning}
    mp_shl(u, k, c);
  finally
    mp_clear(u);
    mp_clear(v);
    end;
  end;

function mp_is1(const a: mp_int): boolean;
  {-Initialized and a = 1}
  begin
  with a do
    mp_is1 := (magic=MP_MAGIC) and (sign=MP_ZPOS) and (pdigits<>nil) and (used=1) and (pdigits^[0]=1);
  end;

function mp_gcd1(const a,b: mp_int; var c: mp_int): boolean;
  {-Calculate c = gcd(a,b) using the binary method, return true if c=1 and no error}
  begin
  mp_gcd(a,b,c);
  mp_gcd1 := mp_is1(c);
  end;

procedure mp_init_set(var a: mp_int; b: mp_digit);
  {-Initialize and set a digit}
  begin
  mp_init(a);
  mp_set(a,b);
  end;

function mp_cmp_d(const a: mp_int; b: mp_digit): Longint;
  {-Compare a with an mp_digit, return sign(a-b)}
  var
    a0: mp_digit;
  begin
  mp_cmp_d := -1;   {default result, used if a<0}
  if b>MP_DIGIT_MAX then
    raise ERtcRSA.Create('mp_cmp_d: b>MP_DIGIT_MAX');
  if a.sign=MP_NEG then exit;
  if a.used>1 then
    mp_cmp_d := 1 {compare based on magnitude}
  else
    begin
    {compare the only digit of a to b}
    if a.used=1 then a0 := a.pdigits^[0] else a0:=0;
    if a0>b then mp_cmp_d := 1
    else if a0<b then mp_cmp_d := -1
    else mp_cmp_d := 0;
    end;
  end;

procedure mp_makeodd(const a: mp_int; var b: mp_int; var s: Longint);
  {-Return b,s with a = 2^s*b if a<>0, b=0,s=-1 otherwise}
  begin
  {count the number of least significant bits which are zero}
  s := mp_cnt_lsb(a);
  {now divide a by 2^s}
  if (s=0) and (a.used=0) then
    begin
    mp_zero(b);
    s := -1;
    end
  else
    mp_shr(a, s, b);
  end;

function s_mp_is_le0(const a: mp_int): boolean;
  {-Return true if a<=0, no init check}
  begin
  s_mp_is_le0 := (a.used=0) or (a.sign=MP_NEG);
  end;

function mp_iseven(const a: mp_int): boolean;
  {-Initialized and even}
  begin
  with a do mp_iseven := (magic=MP_MAGIC) and (pdigits<>nil) and ((used=0) or (not odd(pdigits^[0])));
  end;

function invmod32(a,b: Longint): Longint;
  {-Return a^-1 mod b, b>1. Result is 0 if gcd(a,b)<>1 or b<2}
  var
    u1,u3,v1,v3,t1,t3,q: Longint;
  begin
  invmod32 := 0;
  if (b>1) and (a<>0) then
    begin
    {Use extended GCD to calculate u1*a + u2*b = u3 = gcd(a.b)  }
    {If u3 = 1, then u1 = a^-1 mod b. u2 will not be calculated.}
    {Notation from Knuth [3] Algorithm X. u3 and v3 will be >=0 }
    {and |u1| <= b, |v1| <= b, see e.g. Shoup [29], Theorem 4.3.}
    u1 := 1;
    u3 := abs(a);
    v1 := 0;
    v3 := b;
    while v3<>0 do
      begin
      q  := u3 div v3;
      t1 := u1 - q*v1;
      t3 := u3 - q*v3;
      u1 := v1;
      u3 := v3;
      v1 := t1;
      v3 := t3;
      end;
    if u3=1 then
      begin
      {gcd(a,b)=1, so inverse exists: do some sign related adjustments.}
      if u1<0 then inc(u1,b);
      if (a<0) and (u1<>0) then invmod32 := b-u1 else invmod32 := u1;
      end
    end;
  end;

procedure mp_set1(var a: mp_int);
  {-Set a=1}
  begin
  {mp_zero does ArgCheck!}
  mp_zero(a);
  with a do
    begin
    if (pdigits=nil) or (alloc=0) then
      raise ERtcRSA.Create('mp_set1')
    else
      begin
      used := 1;
      pdigits^[0] := 1;
      end;
    end;
  end;

type
  tml_int = Longint; {FPC has 16 bit Longint/maxint with 32 bit code!!!!!}
  tml_dbl = int64;
const
  Lehmer_MaxSingle = MP_DIGIT_MAX;        {use with mul_d()}
  Lehmer_MaxK      = 2*DIGIT_BIT-1;

function Lehmer(const u,v: mp_int; k: Longint; var x0,x1,y0,y1: tml_int): boolean;
  {-Single precision Lehmer steps to reduce u,v based on highest k bits of}
  { u,v. Return true if successful and reduction using xi,yi should be done}
  var
    hu,hv,hb: mp_word;
    i: TNInt;
    uh,vh,q,x2,y2: tml_dbl;
  begin
  {This is an implementation of Sorenson's Modified Lehmer procedure [28]. }
  {It is based on results of Lehmer, Collins, Jebelean, and Sorenson. The  }
  {procedure performs single digit calculations that allow to combine some }
  {Euclidean GCD steps into a single multiprecision calculation. It returns}
  {Sorenson's x[i], y[i], x_[i-1], and y_[i-1] in x1, y1, x0, and y0. These}
  {values are used in both GCD procedures (standard and extended).         }

  Result := false;

  {Check the technical conditions and exit if they are not fulfilled}
  if (k>Lehmer_MaxK) or (u.sign=MP_NEG) or (v.sign=MP_NEG) or (u.used<2) or (v.used>u.used) or (u.used>v.used+1) then exit;

  {Get the leading two digits of u and v; here v[u.used-1] may be zero.}
  i  := pred(u.used);
  hu := (mp_word(u.pdigits^[i]) shl DIGIT_BIT) + u.pdigits^[pred(i)];
  if v.used<u.used then hv := v.pdigits^[pred(i)]
  else hv := (mp_word(v.pdigits^[i]) shl DIGIT_BIT) + v.pdigits^[pred(i)];

  {Get the highest k bits of u and the corresponding bits of v}
  hb := mp_word(1) shl k;
  i  :=0;
  while hu>=hb do
    begin
    hu := hu shr 1;
    inc(i);
    end;
  hv := hv shr i;
  if hv=0 then exit;

  i  := 0;
  uh := hu;
  vh := hv;
  x0 := 1;
  y0 := 0;
  x1 := 0;
  y1 := 1;
  repeat
    q  := uh div vh;
    x2 := vh;
    vh := uh - q*vh;
    uh := x2;
    x2 := x0 - q*x1;
    y2 := y0 - q*y1;
    {In addition to the standard break conditions, exit if the next  }
    {new x2/y2 values are greater than the maximum allowed values. If}
    {this happens during the first iteration, Lehmer is still false  }
    if (abs(x2) > Lehmer_MaxSingle) or (abs(y2) > Lehmer_MaxSingle) then break;
    inc(i);
    if odd(i) then
      begin
      if (vh < -y2) or (uh-vh < x2-x1) then break;
      end
    else
      begin
      if (vh < -x2) or (uh-vh < y2-y1) then break;
      end;
    x0 := x1;
    x1 := x2;
    y0 := y1;
    y1 := y2;
    Result := true;
    until {$IFDEF IDE_101up}Result ={$ENDIF} false; // avoid a Hint in Delphi 10.1
  end;

{ The code below results in a bad compiler hint in Delphi 10.1 ...
function my_foo:boolean;
var a:integer;
  begin
  a:=0;
  my_foo:=False;
  repeat
    Inc(a);
    if a>10 then break;
    my_foo:=True; // "H2077 Value assigned to 'my_foo' never used"
    until False;
  end;
}

procedure s_mp_chs(var a: mp_int);
  {-Change sign of an mp_int, no init check}
  begin
  if (a.magic=MP_MAGIC) then
    with a do
      begin
      if used>0 then sign := sign xor (MP_NEG xor MP_ZPOS)
      else sign := MP_ZPOS;
      end;
  end;

procedure mp_divrem(const a,b: mp_int; pc,pd: pmp_int);
  {-Longint signed division, pc^ = a div b, pd^ = a rem b; sign(pd^)=sign(a)}
  begin
  if pc=nil then
    begin
    if pd<>nil then mp_mod(a,b,pd^); {xx}
    exit;
    end;

  if mp_not_init(a) or mp_not_init(b) or
    ((pc<>nil) and mp_not_init(pc^)) or
    ((pd<>nil) and mp_not_init(pd^)) then
    raise ERtcRSA.Create('mp_divrem');
  if pc=pd then
    raise ERtcRSA.Create('mp_divrem: pc=pd');
  if (@a=pd) and (@b=pc) then
    raise ERtcRSA.Create('mp_divrem: (@a=pd) and (@b=pc)');

  {Check b=0}
  if b.used=0 then
    raise ERtcRSA.Create('mp_divrem: b=0');
  {call Burnikel/Ziegler or Knuth's basecase algorithm D}
  s_mp_divrem(a,b,pc,pd);
  end;

procedure mp_div(const a,b: mp_int; var c: mp_int);
  {-Longint signed division, c = a div b}
  begin
  mp_divrem(a,b,@c,nil);
  end;

procedure mp_xgcd(const a,b: mp_int; p1,p2,p3: pmp_int);
  {-Extended gcd algorithm, calculate  a*p1^ + b*p2^ = p3^ = gcd(a,b)}
  { p1,p2,p3 may be nil if the values are not required.}
  var
    u1,u3,v1,v3,t1,t3,q: mp_int;
    x0,x1,y0,y1: tml_int;
    TryLehmer: boolean;
  const
    k=Lehmer_MaxK; k2=(k+1) div 2;
  begin
  if mp_not_init(a) or mp_not_init(b) or ((p1<>nil) and mp_not_init(p1^)) or
     ((p2<>nil) and mp_not_init(p2^)) or ((p3<>nil) and mp_not_init(p3^)) then
    raise ERtcRSA.Create('mp_xgcd');

  if ((p1=p2) and (p1<>nil)) or ((p1=p3) and (p1<>nil)) or ((p2=p3) and (p2<>nil)) then
    raise ERtcRSA.Create('mp_xgcd: identical pointers');

  if (p1=nil) and (p2=nil) then
    begin
    {easy out for trivial cases}
    if p3<>nil then mp_gcd(a,b,p3^);
    exit;
    end;

  mp_init(u1);
  mp_init(u3);
  mp_init(v1);
  mp_init(v3);
  mp_init(t1);
  mp_init(t3);
  mp_init(q);
  try
    {Note: the following setup returns 1*0 + 0*0 = 0, for a=b=0! This}
    {is somewhat strange but OK. Knuth's algorithm X gives the same. }
    {Usage of u2,v2,t2 suppressed, p2^ = u2 will be = (u3 - a*u1)/b. }
    {We need positive working variables u3,v3. So init via abs, the  }
    {sign of u3 is corrected after loop and before calculation of u2.}

    {initialize, (u1,u3) = (1,a)}
    mp_set1(u1);
    mp_abs(a, u3);

    {initialize, (v1,v3) = (0,b), v1=0 after init}
    mp_abs(b, v3);

    {The Lehmer step needs u3 >= v3 > 0. This condition may no hold}
    {initially. For extended GCD we cannot simply swap u3 and v3.  }
    {Therefore we skip the step in the first iteration, after that }
    {the condition becomes true via the Euclidean mod operation.   }
    TryLehmer := mp_cmp_mag(u3,v3)<>MP_LT;

    {loop while v3 != 0}
    while not mp_iszero(v3) do
      begin
      if TryLehmer and (mp_bitsize(u3)-mp_bitsize(v3) <= k2) then
        begin
        if Lehmer(u3,v3,k,x0,x1,y0,y1) then
          begin
          {calculate new (ui, vi):  vi := x1*ui + y1*vi;  ui := x0*ui + y0*vi;}
          {v3 := x1*u3 + y1*v3}
          mp_mul_d(u3,abs(x1), q);  if x1<0 then s_mp_chs(q);
          mp_mul_d(v3,abs(y1),t1);  if y1<0 then s_mp_chs(t1);
          mp_add(q,t1,t1);
          {u3 := x0*u3 + y0*v3}
          mp_mul_d(u3,abs(x0), q);  if x0<0 then s_mp_chs(q);
          mp_mul_d(v3,abs(y0),t3);  if y0<0 then s_mp_chs(t3);
          mp_add(q,t3,t3);
          {the new u3,v3 should be >= 0, skip the Lehmer step if not}
          if (t1.sign=MP_ZPOS) and (t3.sign=MP_ZPOS) then
            begin
            mp_exch(t3,u3);
            mp_exch(t1,v3);
            {v1 := x1*u1 + y1*v1}
            mp_mul_d(u1,abs(x1), q);  if x1<0 then s_mp_chs(q);
            mp_mul_d(v1,abs(y1),t1);  if y1<0 then s_mp_chs(t1);
            mp_add(q,t1,t1);
            mp_exch(t1,v1);
            {u1 := x0*u1 + y0*v1}
            mp_mul_d(u1,abs(x0), q);  if x0<0 then s_mp_chs(q);
            mp_mul_d(t1,abs(y0),t1);  if y0<0 then s_mp_chs(t1);
            mp_add(q,t1,u1);
            end;
          end;
        end;

      {q = u3/v3, t3=u3-q*v3}
      mp_divrem(u3, v3, @q, @t3);

      {(t1,t3) = (u1,u3) - q*(v1,v3)}
      mp_mul(v1, q,  t1);
      mp_sub(u1, t1, t1);

      {(u1,u3) = (v1,v3)}
      mp_exch(v1, u1);
      mp_exch(v3, u3);

      {(v1,v3) = (t1,t3)}
      mp_exch(t1, v1);
      mp_exch(t3, v3);

      TryLehmer := true;
      end;

    {adjust sign if a<0}
    if a.sign=MP_NEG then s_mp_chs(u1);
    {copy results if requested}
    if p2<>nil then
      begin
      {here v3=0}
      if mp_iszero(b) then mp_exch(p2^, v3)
      else
        begin
        {use v3 to calculate p2^ = (u3 - a*u1)/b}
        mp_mul(a,u1,v1);
        mp_sub(u3,v1,v3);
        mp_div(v3,b,p2^);
        end;
      end;
    if p1<>nil then mp_exch(p1^, u1);
    if p3<>nil then mp_exch(p3^, u3);
  finally
    mp_clear(u1);
    mp_clear(u3);
    mp_clear(v1);
    mp_clear(v3);
    mp_clear(t1);
    mp_clear(t3);
    mp_clear(q);
    end;
  end;

function mp_invmodf(const a, b: mp_int; var c: mp_int): boolean;
  {-Compute c = a^-1 (mod b), b>0, via mp_xgcd, return true if inverse exists}
  var
    u,v: mp_int;
    la, lb, lc: Longint;
  begin

  {Note: this function is normally faster than the old fast_invmod}
  {for odd b due to the added Lehmer steps in mp_xgcd. It is much }
  {faster than the old mp_invmodf for even b.}

  mp_invmodf := false;

  {if a and b are even or b<=0 then return no success}
  if (mp_iseven(b) and mp_iseven(a)) or (b.sign=MP_NEG) or mp_iszero(b) then exit;

  {use invmod32 if b has less than 32 bits}
  if mp_is_longint(b,lb) then
    begin
    if not mp_is_longint(a,la) then mp_mod_int(a,lb,la);
    lc := invmod32(la,lb);
    if lc<>0 then
      begin
      mp_set_int(c,lc);
      mp_invmodf := true;
      end;
    exit;
    end;

  {initialize temps}
  mp_init(u);
  mp_init(v);
  try
    {calculate a*u + b*? = v = gcd(a,b)}
    mp_xgcd(a,b,@u,nil,@v);

    {if gcd(a,b><>1 then there is no inverse}
    if mp_is1(v) then
      begin
      {Make 0 <= a^-1 < b}
      while (u.sign=MP_NEG) do mp_add(u,b,u);
      while (mp_cmp_mag(u, b)<>MP_LT) do mp_sub(u,b,u);
      mp_exch(u, c);
      mp_invmodf := True;
      end;
  finally
    mp_clear(u);
    mp_clear(v);
    end;
  end;

procedure mp_invmod(const a, b: mp_int; var c: mp_int);
  {-Compute c = a^-1 (mod b), b>0, via mp_xgcd, MP_UNDEF error if there is no inverse}
  begin
  if not mp_invmodf(a,b,c) then
    raise ERtcRSA.Create('mp_invmod: no inverse');
  end;

function mp_reduce_is_2k(const a: mp_int): boolean;
  {-Determine if mp_reduce_2k can be used}
  var
    i: TNInt;
    da: mp_digit;
    pa: pmp_digit;
  begin
  mp_reduce_is_2k := false;
  if s_mp_is_le0(a) then exit;
  {if a.used=1 then skip loop}
  if a.used>1 then
    begin
    pa := @a.pdigits^[1];
    {check all digits except lowest and highest, note a.used>=2!}
    for i:=1 to a.used-2 do
      begin
      {if pa^ and MP_MASK <> MP_MASK then exit;}
      if pa^ <> MP_MASK then exit;
      inc(pa);
      end;
    {check highest digit: if we shift all the low}
    {bits away the remaining part should be zero }
    da := pa^;
    while odd(da) do da := da shr 1;
    if da<>0 then exit;
    end;
  mp_reduce_is_2k := True;
  end;

procedure mp_montgomery_reduce(var x: mp_int; const n: mp_int; rho: mp_digit);
  {-Calculate x = xR^-1 (mod n) via Montgomery reduction}
  var
    ix, iy: TNInt;
    tmpn, tmpx: pmp_digit;
    digs: Longint;
    mu,t,u: mp_digit;
    r: mp_word;
  begin
  if n.used<1 then exit;
  digs := n.used * 2 + 1;
  {grow the input as required}
  if x.alloc < digs then mp_grow(x, digs);
  x.used := digs;
  for ix:=0 to n.used-1 do
    begin
    {alias for digits of the modulus}
    tmpn := pmp_digit(n.pdigits);

    {alias for the digits of x [the input]}
    tmpx := @x.pdigits^[ix];

    {mu = ai * rho mod B}
    {The value of rho must be precalculated via montgomery_setup() such that
     it equals -1/n mod B this allows the following inner loop to reduce the
     input one digit at a time}
    mu := mp_word(x.pdigits^[ix]) * mp_word(rho) and MP_MASK;
    {set the carry to zero}
    u := 0;
    {Multiply and add in place}
    for iy:=0 to n.used-1 do
      begin
      {compute product and sum}
      r := mp_word(mu) * mp_word(tmpn^) + mp_word(u) + mp_word(tmpx^);
      {get carry}
      u := mp_digit(r shr DIGIT_BIT);
      {fix digit}
      tmpx^ := mp_digit(r and MP_MASK);
      inc(tmpn);
      inc(tmpx);
      end;
    {At this point the ix'th digit of x should be zero}
    {propagate carries upwards as required}
    while u<>0 do
      begin
      t := tmpx^ + u;
      u := t shr DIGIT_BIT;
      tmpx^ := t and MP_MASK;
      inc(tmpx);
      end;
    end;

  {at this point the n.used'th least significant digits of x are all zero
   which means we can shift x to the right by n.used digits and the
   residue is unchanged.}

  {x = x/B^n.used}
  mp_clamp(x);
  mp_rshd(x, n.used);

  {if x >= n then x = x - n}
  if mp_cmp_mag(x, n) <> MP_LT then s_mp_sub(x, n, x);
  end;

procedure s_mp_mul_high_digs(const a,b: mp_int; var c: mp_int; digs: Longint);
  {-Multiply |a| * |b| and does not compute the lower digs digits}
  { meant to get the higher part of the product}
  var
    pt, py: pmp_digit;
    u, tmpx: mp_digit;
    r: mp_word;
    pa, pb: Longint;
    ix, iy, id: TNInt;
    t: mp_int;
  begin
  mp_init_size(t, a.used + b.used + 1);
  {Since t is zerofilled we skip the inner loops for zero multiplier}
  try
    t.used := a.used + b.used + 1;
    pa := a.used;
    pb := b.used;

    for ix:=0 to pa-1 do
      begin
      {left hand side of A[ix] * B[iy]}
      tmpx := a.pdigits^[ix];
      {skip loop if multiplier is zero, *V1.1.09}
      if tmpx<>0 then
        begin
        {*V1.1.09: Test and adjust indices to allow digs downto zero}
        id := digs-ix; if id<0 then id := 0;
        if pb>id then
          begin
          {alias to the address of where the digits will be stored }
          pt := @t.pdigits^[id+ix];
          {alias for where to read the right hand side from}
          py := @b.pdigits^[id];
          {clear the carry}
          u := 0;
          for iy:=id to pb-1 do begin
            {calculate the double precision result}
            r := mp_word(pt^) + mp_word(tmpx)*mp_word(py^) + mp_word(u);
            inc(py);
            {get the lower part}
            pt^ := mp_digit(r and mp_word(MP_MASK));
            inc(pt);
            {carry the carry}
            u := mp_digit(r shr mp_word(DIGIT_BIT));
          end;
          pt^ := u;
          end;
        end;
      end;

    mp_clamp(t);
    mp_exch(t, c);
  finally
    mp_clear(t);
    end;
  end;

procedure mp_reduce(var x: mp_int; const m, mu: mp_int);
  {-Reduce x mod m via Barrett, assumes x<m^2, mu is from mp_reduce_setup}
  { x should be positive otherwise slow mp_mod is used}
  var
    um: TNInt;
    q : mp_int;
  const
    limit: Longint = Longint(MP_DIG1 shl pred(DIGIT_BIT));

  begin
  if mp_not_init(x) or mp_not_init(m) or mp_not_init(mu) then
    raise ERtcRSA.Create('mp_reduce');
  if s_mp_is_le0(m) then
    raise ERtcRSA.Create('mp_reduce: m <= 0');
  if s_mp_is_le0(mu) then
    raise ERtcRSA.Create('mp_reduce: mu <= 0');

  um := m.used;

  {x must be positive otherwise infinite loop, *V0.7.07}
  {allow x<0 instead of exception, *V1.1.03 }
  if x.sign=MP_NEG then
    begin
    {m is positive, use add if possible}
    if x.used<um then mp_add(x,m,x) else mp_mod(x,m,x);
    exit;
    end;

  {here x>=0; easy out if x<m, includes case x=0 since um>0}
  if x.used<um then exit;

  {here x.used >= um; use mp_mod if x.used > 2*um}
  if x.used-um>um then
    begin
    mp_mod(x,m,x);
    exit;
    end;

  {q = x}
  mp_init_copy(q, x);
  try
    {from HAC algorithm 14.42}
    {q1 = x / b^(k-1)}
    mp_rshd(q, um-1);

    {done if q1=0, *we:0.7.07}
    if q.used>0 then
      begin
      if (um>limit) or ((um >= mp_mul_cutoff) and (q.used >= mp_mul_cutoff)) then
        mp_mul(q, mu, q)
      else
        begin
        {*V1.1.09: use digs=um-1 to avoid large number of back subtracts below}
        s_mp_mul_high_digs(q, mu, q, um-1);
        end;

      {q3 = q2 / b^(k+1) */}
      mp_rshd(q, um + 1);

      {x = x mod b^(k+1), quick (no division)}
      s_mp_mod_2k(x, DIGIT_BIT * (um + 1), x);

      {q = q * m mod b^(k+1), quick (no division)}
      s_mp_mul_digs(q, m, q, um + 1);

      {x = x - q}
      mp_sub(x,q,x);

      {If x < 0, add b^(k+1) to it}
      if x.sign=MP_NEG then
        begin
        mp_set(q, 1);
        mp_lshd(q, um + 1);
        mp_add(x, q, x);
        end;

      {Back off if it's too big, use mp_cmp_mag because x and m are positive}
      while (mp_cmp_mag(x,m)<>MP_LT) do
        s_mp_sub(x, m, x);
      end;
  finally
    mp_clear(q);
    end;
  end;

procedure mp_reduce_2k(var a: mp_int; const n: mp_int; d: mp_digit);
  {-Reduce a mod n where n is of the form 2^p-d, @a<>@n}
  var
    q: mp_int;
    p: Longint;
  begin
  if mp_not_init(a) or mp_not_init(n) then
    raise ERtcRSA.Create('mp_reduce_2k');
  if @a=@n then
    raise ERtcRSA.Create('mp_reduce_2k: @a=@n');
  if s_mp_is_le0(n) then
    raise ERtcRSA.Create('mp_reduce_2k: n<=0');

  {if a<0 do a recursive call with -a}
  if a.sign=MP_NEG then
    begin
    s_mp_chs(a);
    mp_reduce_2k(a,n,d);
    if a.used>0 then
      begin
      {change sign mod n}
      mp_sub(n,a,a);
      end;
    exit;
    end;

  {done if a<n}
  if a.used<n.used then exit;

  p := mp_bitsize(n);

  mp_init(q);
  try
    while True do
      begin
      {q = a/2^p, a = a mod 2^p}
      mp_div_2k(a, p, q, @a);
      {Skip trivial case q=0} {*0.7.01}
      if q.used > 0 then
        begin
        if d<>1 then
          begin
          {q = q * d}
          mp_mul_d(q, d, q);
          end;
        {a = a + q}
        s_mp_add(a, q, a);
        end;
      if mp_cmp_mag(a, n)=MP_LT then break;
      s_mp_sub(a, n, a);
      end;
  finally
    mp_clear(q);
    end;
  end;

procedure mp_montgomery_setup(const n: mp_int; var rho: mp_digit);
  {-Calculate rho = -1/n mod B for Montgomery reduction, B=2^DIGIT_BIT}
  var
    x, b: mp_digit;
  begin
  {Fast inversion mod 2^k, based on the fact that
    XA = 1 (mod 2^n)  =>  (X(2-XA)) A = 1 (mod 2^2n)
                      =>  2*X*A - X*X*A*A = 1
                      =>  2*(1) - (1)     = 1 }
  b := n.pdigits^[0];
  if b and 1 = 0 then {n must be odd}
    raise ERtcRSA.Create('mp_montgomery_setup: n is even');

  {temporarily turn range checks off}
  {$R-}
  x := (((b + 2) and 4) shl 1) + b;  {here x*a==1 mod 2^4}
  x := x * (2 - b * x);              {here x*a==1 mod 2^8}
  x := x * (2 - b * x);              {here x*a==1 mod 2^16}
  x := x * (2 - b * x);              {here x*a==1 mod 2^32}
  {$ifdef RangeChecks_on} {$R+} {$endif}
  {rho = -1/n mod B }
  rho := ((mp_word(1) shl mp_word(MP_DIGIT_BIT)) - x) and MP_MASK;
  end;

procedure mp_mul_2(const a: mp_int; var b: mp_int);
  {-Multiply by 2, b = 2*a}
  var
    oldused: Longint;
    i: TNInt;
    r,rr: mp_digit;
    pa,pb: pmp_digit;
  begin
  {if a=0 then just copy}
  if a.used=0 then
    begin
    mp_copy(a, b);
    exit;
    end;
  {grow dest}
  if b.alloc < a.used+1 then
    mp_grow(b, a.used+1);

  oldused := b.used;
  b.used := a.used;
  pa := pmp_digit(a.pdigits);
  pb := pmp_digit(b.pdigits);
  {carry}
  r  := 0;
  for i:=0 to a.used-1 do
    begin
    {get what will be the *next* carry bit from the MSB of the current digit}
    rr := pa^ shr (DIGIT_BIT - 1);
    {now shift up this digit, add in the carry [from the previous]}
    pb^ := ((pa^ shl 1) or r) and MP_MASK;
    {copy carry that would be from the source digit into the next iteration}
    r := rr;
    inc(pa);
    inc(pb);
    end;

  {new leading digit?}
  if r<>0 then
    begin
    {add a MSB which is always 1 at this point}
    pb^ := 1;
    inc(b.used);
    inc(pb);
    end;

  {now zero any excess digits on the destination that we did not write to}
  for i:=b.used to oldused-1 do
    begin
    pb^ := 0;
    inc(pb);
    end;
  b.sign := a.sign;
  end;

procedure mp_montgomery_calcnorm(var R: mp_int; const m: mp_int);
  {-Calculate R = B^n mod m, n=number of digits in m, B=2^DIGIT_BIT}
  var
    x, bits: Longint;
  begin
  {The method is slightly modified to shift B unconditionally up to just under}
  {the leading bit of m.  This saves a lot of multiple precision shifting}

  {how many bits of last digit does m use}
  bits := mp_bitsize(m) mod DIGIT_BIT;
  if m.used>1 then mp_2expt(R, (m.used-1)*DIGIT_BIT + bits - 1)
  else
    begin
    mp_set(R, 1);
    bits := 1;
    end;
  {now compute R * B mod m}
  for x:= bits to DIGIT_BIT do
    begin
    mp_mul_2(R, R);
    if mp_cmp_mag(R, m) <> MP_LT then s_mp_sub(R, m, R);
    end;
  end;

procedure mp_mulmod(const a,b,c: mp_int; var d: mp_int);
  {-Calculate d = a * b (mod c)}
  var
    t: mp_int;
  begin
  mp_init(t);
  try
    mp_mul(a, b, t);
    mp_mod(t, c, d);
  finally
    mp_clear(t);
    end;
  end;

procedure mp_reduce_setup(var mu: mp_int; const a: mp_int);
  {-Pre-calculate the value required for Barrett reduction.}
  { For a given modulus a it calculates the value required in mu}
  var
    t: mp_int;
  begin
  {Setup local variable. This is paranoid: @a=@mu not useful, but...}
  mp_init_size(t, 2*(a.used+1));
  try
    mp_2expt(t, a.used * 2 * DIGIT_BIT);
    mp_div(t, a, mu);
  finally
    mp_clear(t);
    end;
  end;

procedure mp_reduce_2k_setup(const a: mp_int; var d: mp_digit);
  {-Determine setup value d for unrestricted diminished radix reduction, a>=0}
  var
    bs: Longint;
  begin
  if s_mp_is_le0(a) then
    raise ERtcRSA.Create('mp_reduce_2k_setup: a<=0');
  {a is of the form 2^k - digit}
  d := a.pdigits^[0];
  if a.used>1 then
    {if more than one digit, d = 2^DIGIT_BIT-a[0] = (MP_DIGIT_MAX-a[0])+1}
    d := (MP_DIGIT_MAX-d)+1
  else
    begin
    {d := 2^bitsize32(a[0]) - a[0]}
    bs := bitsize32(d);
    if bs<DIGIT_BIT then
      begin
      {$ifdef FPC}
        d := mp_digit(Longint(1 shl bs)-Longint(d));
      {$else}
        d := (1 shl bs)-d;
      {$endif}
      end
    else
      d := (MP_DIGIT_MAX-d)+1;
    end;
  end;

const
  WEXP_MAX     = 8;               {log2 of WEXP_TABSIZE}
  WEXP_TABSIZE = 1 shl WEXP_MAX;  {Sliding window table size in mp_exptmod}

type
  TRedType = (MR_Barret, MR_Montgomery ,MR_Reduce2k); {supported reduction types for exptmod}

procedure mp_exptmod_win(const g,e,p: mp_int; var b: mp_int; redmode: TRedType);
  {-Internal: Compute y=g^|e| mod p, p>0, internal sliding windows}
  var
    bitbuf, bitcpy, bitcnt, mode, x, y, winsize, wmax1,wmax2: Longint;
    digidx: TNint;
    ps2: Longint;
    buf: mp_digit;
    mp : mp_digit;
    d2k: mp_digit;
    bc: Longint;
    res, mu: mp_int;
    M: array[1..WEXP_TABSIZE] of mp_int;

  {---------------------------------------------}
  procedure Gen_Redux(var mpi: mp_int);
    {-General modular reduction of mpi driven by redmode}
  begin
    case redmode of
      MR_Montgomery: mp_montgomery_reduce(mpi, p, mp);
          MR_Barret: mp_reduce(mpi, p, mu);
        MR_Reduce2k: mp_reduce_2k(mpi, p, d2k);
      end;
  end;

begin
  {Uses a left-to-right k-ary sliding window to compute the modular exponentiation}
  {The value of k changes based on the size of the exponent.}

  {No checks}
  {find window size}
  bc := mp_bitsize(e);
  if bc<=7 then winsize := 2
  else if bc<=36 then winsize := 3
  else if bc<=140 then winsize := 4
  else if bc<=450 then winsize := 5
  else if bc<=1303 then winsize := 6
  else if bc<=3529 then winsize := 7
  else winsize := 8;

  if winsize>WEXP_MAX then winsize := WEXP_MAX;
  wmax1 := 1 shl (winsize-1);
  wmax2 := 1 shl winsize;

  ps2 := 2*(p.used+1);

  {initialize M array}
  {initialize first cell}
  mp_init_size(M[1],ps2);
  {now initialize the second half of the array}
  for x:=wmax1 to wmax2-1 do
    mp_init_size(M[x],ps2);
  {create mu, used for Barrett reduction}
  mp_init_size(mu,ps2);
  {setup result}
  mp_init(res);
  try
    {Do initial setup depending on reduction type}
    if Redmode=MR_Montgomery then
      begin
      mp_montgomery_setup(p, mp);
      mp_montgomery_calcnorm(res, p);
      {now set M[1] to G * R mod m}
      mp_mulmod(g, res, p, M[1]);
      end
    else
      begin
      {The M table contains powers of the base, }
      {e.g. M[x] = g^x mod p                   }
      mp_set(res, 1);
      mp_mod(g, p, M[1]);
      if Redmode=MR_Barret then mp_reduce_setup(mu, p)
      else if Redmode=MR_Reduce2k then mp_reduce_2k_setup(p, d2k);
      end;

    {Create M table                           }
    {The first half of the table is not       }
    {computed though accept for M[0] and M[1] }

    {compute the value at M[wmax1] by squaring M[1] (winsize-1) times}
    mp_copy(M[1], M[wmax1]);
    for x:=0 to winsize-2 do
      begin
      mp_sqr(M[wmax1], M[wmax1]);
      Gen_Redux(M[wmax1]);
      end;

    {create upper table, that is M[x] = M[x-1] * M[1] (mod p)}
    for x:=wmax1+1 to wmax2-1 do
      begin
      mp_mul(M[x-1], M[1], M[x]);
      Gen_Redux(M[x]);
      end;

    {set initial mode and bit cnt}
    mode   := 0;
    bitcnt := 1;
    buf    := 0;
    digidx := e.used - 1;
    bitcpy := 0;
    bitbuf := 0;

    repeat
      {grab next digit as required }
      dec(bitcnt);
      if bitcnt=0 then
        begin
        {if digidx = -1 we are out of digits}
        if digidx = -1 then break;
        {read next digit and reset the bitcnt}
        buf    := e.pdigits^[digidx]; dec(digidx);
        bitcnt := DIGIT_BIT;
        end;

      {grab the next msb from the exponent}
      y := (buf shr mp_digit(DIGIT_BIT - 1)) and 1;

      {temporarly turn range checks off}
      {$R-}
      buf := buf shl 1;
      {$ifdef RangeChecks_on} {$R+} {$endif}

      {if the bit is zero and mode = 0 then we ignore it             }
      {These represent the leading zero bits before the first 1 bit  }
      {in the exponent.  Technically this opt is not required but it }
      {does lower the # of trivial squaring/reductions used          }

      if (mode=0) and (y=0) then continue;

      {if the bit is zero and mode == 1 then we square}
      if (mode=1) and (y=0) then
        begin
        mp_sqr(res, res);
        Gen_Redux(res);
        continue;
        end;

      {else we add it to the window}
      inc(bitcpy);
      bitbuf := bitbuf or (y shl (winsize - bitcpy));
      mode   := 2;

      if bitcpy=winsize then
        begin
        {ok window is filled so square as required and multiply}
        {square first}
        for x:=0 to winsize-1 do
          begin
          mp_sqr(res, res);
          Gen_Redux(res);
          end;

        {then multiply}
        mp_mul(res, M[bitbuf], res);
        {and reduce}
        Gen_Redux(res);

        {empty window and reset}
        bitcpy := 0;
        bitbuf := 0;
        mode   := 1;
        end;
      until false;

    {if bits remain then square/multiply}
    if (mode=2) and (bitcpy > 0) then
      begin
      {square then multiply if the bit is set}
      for x:=0 to bitcpy-1 do
        begin
        mp_sqr(res, res);
        Gen_Redux(res);
        bitbuf := bitbuf shl 1;
        if (bitbuf and wmax2) <> 0 then
          begin
          {then multiply}
          mp_mul(res, M[1], res);
          {and reduce}
          Gen_Redux(res);
          end;
        end;
      end;

    if Redmode=MR_Montgomery then
      begin
      {fix result if Montgomery reduction is used recall that any value}
      {in a Montgomery system is actually multiplied by R mod n.  So we}
      {have to reduce one more time to cancel out the factor of R.     }
      mp_montgomery_reduce(res, P, mp);
      end;

    mp_exch(res, b);

  finally
    mp_clear(res);
    mp_clear(mu);
    mp_clear(M[1]);
    for x:=wmax1 to wmax2-1 do mp_clear(M[x]);
    end;
  end;

procedure mp_exptmod(const a,b,c: mp_int; var d: mp_int);
  {-Compute d = a^b mod c, c>0. If b<0, a must have an inverse mod c}
  var
    rt: TRedType;
    t0,t1: mp_int; {a:t[0], b:t[1]}
  begin

  {modulus c must be positive}
  if s_mp_is_le0(c) then
    raise ERtcRSA.Create('mp_exptmod: c<=0');

  {if exponent b is negative we have to recourse}
  if b.sign=MP_NEG then
    begin
    {first compute 1/a mod c}
    mp_init(t0);
    mp_init(t1);
    try
      mp_invmod(a, c, t0);
      {now get |b|}
      mp_abs(b, t1);
      {and now compute (1/a)^|b| instead of a^b [b < 0]}
      mp_exptmod(t0, t1, c, d);
    finally
      mp_clear(t0);
      mp_clear(t1);
      end;
    Exit;
    end;

  {easy outs}
  if mp_is1(c) then
    begin
    mp_zero(d);
    exit;
    end;
  if mp_is1(b) then
    begin
    mp_mod(a,c,d);
    exit;
    end;

  {Default: Barrett reduction}
  rt := MR_Barret;
  if mp_isodd(c) then rt := MR_Montgomery;

  if mp_reduce_is_2k(c) then rt := MR_Reduce2k;
  {*tbd: DR module variants}

  {Use sliding window routine to compute the modular exponentiation}
  mp_exptmod_win(a, b, c, d, rt)
  end;

function mp_is_ne(const a,b: mp_int): boolean;
  {-Return a <> b}
  begin
  mp_is_ne := (mp_cmp(a,b)<>MP_EQ);
  end;

procedure mp_sqrmod(const a,c: mp_int; var d: mp_int);
  {-Calculate d = a * a (mod c)}
  var
    t: mp_int;
  begin
  mp_init(t);
  try
    mp_sqr(a, t);
    mp_mod(t, c, d);
  finally
    mp_clear(t);
    end;
  end;

function mp_is_spsp(const n,a: mp_int): boolean;
  {-Strong probable prime test of n to base a > 1 from HAC p. 139 Alg.4.24  }
  { Sets result to false if definitely composite or true if probably prime.}
  { Randomly the chance of error is <= 1/4 and often very much lower.      }
  label
    leave;
  var
    n1, y, r: mp_int;
    s,j: Longint;
    useb: boolean;
  const
    SBMin = 5;
  begin
  {init default result}
  mp_is_spsp := false;

  if mp_cmp_d(n, 1)<>MP_GT then exit;

  {ensure a > 1}
  if mp_cmp_d(a, 1)<>MP_GT then
    raise ERtcRSA.Create('mp_is_spsp: a<=1');

  mp_init(n1);
  mp_init(y);
  mp_init(r);
  try
    {get n1 = n - 1}
    mp_sub_d(n,1,n1);

    {set 2^s * r = n1}
    mp_makeodd(n1,r,s);

    {compute y = a^r mod n}
    mp_exptmod(a, r, n, y);

    {if y<>1 and y<>n-1 do}
    if (not mp_is1(y)) and mp_is_ne(y, n1) then
      begin
      j := 1;
      {Use Barret if s is not very small}
      useb := s>=SBMIN;
      if useb then
        begin
        {Setup Barrett reduction for n}
        mp_reduce_setup(r, n);
        end;
      while (j <= s-1) and mp_is_ne(y, n1) do
        begin
        if useb then
          begin
          mp_sqr(y,y);
          mp_reduce(y,n,r);
          end
        else
          mp_sqrmod(y, n, y);
        {if y=1 then composite}
        if mp_is1(y) then goto leave;
        inc(j);
        end;
      {if y<>n1 then composite}
      if mp_is_ne(y, n1) then goto leave;
      end;

    {probably prime now}
    mp_is_spsp := true;
leave:
  finally
    mp_clear(n1);
    mp_clear(y);
    mp_clear(r);
    end;
  end;

function mp_is_spsp_d(const n: mp_int; a: mp_digit): boolean;
  {-Strong probable prime test of n to mp_digit base a > 1 from HAC p. 139 Alg.4.24}
  var
    t: mp_int;
  begin
  // mp_is_spsp_d := false;
  mp_init_set(t,a);
  try
    mp_is_spsp_d := mp_is_spsp(n,t);
  finally
    mp_clear(t);
    end;
  end;

function kronjac32(a,b: Longint; kron: boolean): Longint;
  {-Compute the Kronecker/Jacobi/Legendre symbol (a|b)}
  var
    res,m8: Longint;
    t: Longint;
  begin
  {Uses classic Jacobi method using only mod see e.g. [10] Alg. 2.3.5.}
  kronjac32 := 0;

  {initialize accumulated result}
  res := 1;

  if kron then
    begin
    {Compute Kronecker symbol, reduce to Jacobi with}
    if b=0 then
      begin
      if abs(a)=1 then kronjac32 := 1;
      exit;
      end;

    {here b<>0, make b positive}
    if b<0 then
      begin
      {(a|b) = (a|-1)*(a|-b)}
      if a<0 then res := -res;
      b := abs(b);
      end;

    {if b even, reduce to odd case}
    if b and 1 = 0 then
      begin
      {(a|2)=0 if a is even}
      if a and 1 = 0 then exit;
      {divide out powers of 4}
      while b and 3 = 0 do b := b shr 2;
      {if b is even, divide by 2 and adjust result}
      if b and 1 = 0 then
        begin
        b  := b shr 1;
        m8 := a and 7;
        if (m8=3) or (m8=5) then res := -res;
        end;
      end;
    end
  else
    begin
    {Compute Jacobi/Legendre symbol: Check range of b}
    if (b<3) or (b and 1 = 0) then
      raise ERtcRSA.Create('kronjac32: b<3 or even');
    end;

  {Here b is positive and odd, and we actually calculate a Jacobi symbol.}
  {if a<0 use property of Jacobi symbol to make it positive. Don't rely  }
  {on Pascal mod function to return positive result.}
  if a<0 then
    begin
    {(-a|b) = (a|b)*(-1|b); change sign if b=3 (mod 4)}
    if b and 3 = 3 then res := -res;
    a := -a;
    end;

  {do initial reduction to force a < b}
  if a>=b then a := a mod b;

  {loop invariant: a < b}
  while a<>0 do
    begin
    {divide out powers of 4}
    while a and 3 = 0 do a := a shr 2;
    {if a is even, divide by 2 and adjust result}
    if a and 1 = 0 then
      begin
      a  := a shr 1;
      m8 := b and 7;
      { (2|b) = -1  if b = +-3 (mod 8)}
      if (m8=3) or (m8=5) then res := -res;
      end;
    if a=1 then
      begin
      kronjac32 := res;
      exit;
      end;
    {swap variables, reduce and adjust result using quadratic reciprocity}
    if (a and b and 3) = 3 then res := -res;
    t := b;
    b := a;
    a := t mod a;
    end;
  if b=1 then kronjac32 := res;
  end;

function jacobi32(a,b: Longint): Longint;
  {-Compute the Jacobi/Legendre symbol (a|b), b: odd and > 2}
  begin
  jacobi32 := kronjac32(a,b,false);
  end;

function mp_jacobi_lm(a: Longint; const n: mp_int): Longint;
  {-Compute the Jacobi/Legendre symbol (a|n), n: odd and > 2}
  var
    j,k: Longint;
    t: Longint;
    m8: mp_digit;
  begin
  mp_jacobi_lm := 0;

  {Error if n<3 or even}
  if (n.sign=MP_NEG) or mp_is1(n) or mp_iseven(n) then
    raise ERtcRSA.Create('mp_jacobi_lm: n<3 or even');

  if a=0 then exit;

  {initialize accumulated result}
  j := 1;
  m8 := n.pdigits^[0] and 7;

  if a<0 then
    begin
    {(-a|b) = (a|b)*(-1|b); change sign if b=3 (mod 4)}
    if m8 and 3 = 3 then j := -j;
    a := -a;
    end;

  {if a is even divide out highest power of two: 2^k}
  if a and 1 = 0 then
    begin
    k:=0;
    repeat
      inc(k);
      a := a shr 1;
      until odd(a);
    {if odd exponent use (2|b) = -1 if b = +-3 (mod 8)}
    if odd(k) and ((m8=3) or (m8=5)) then j:=-j;
    end;

  {done if a=1}
  if a=1 then mp_jacobi_lm := j
  else
    begin
    {adjust result using quadratic reciprocity}
    if (a and 3 = 3) and (m8 and 3 = 3) then j := -j;
    {swap variables, reduce mp_int mod Longint, and use jacobi32}
    mp_mod_int(n,a,t);
    if t<>0 then mp_jacobi_lm := j*jacobi32(t,a);
    end;
  end;

function isqrt32(a: Longint): Longint;
  {-Return floor(sqrt(abs(a))}
  var
    n,r,b,t: Longint;
  begin
  {Based on Wilco Dijkstra's C code from comp.sys.arm}
  {http://groups.google.com/group/comp.sys.arm/msg/2f9fe6ab123f6a6d?dmode=source&output=gplain}
  {WE changes: local n, abs(arg), partial unroll}
  r := 0;
  n := abs(a);
  if n >= $10000 then b := $40000000
  else b := $4000;
  repeat
    t := r+b;
    if n >= t then
      begin
      dec(n, t);
      r := t + b;
      end;
    r := r shr 1;
    b := b shr 2;
    t := r+b;
    if n >= t then
      begin
      dec(n, t);
      r := t + b;
      end;
    r := r shr 1;
    b := b shr 2;
    t := r+b;
    if n >= t then
      begin
      dec(n, t);
      r := t + b;
      end;
    r := r shr 1;
    b := b shr 2;
    t := r+b;
    if n >= t then
      begin
      dec(n, t);
      r := t + b;
      end;
    r := r shr 1;
    b := b shr 2;
    t := r+b;
    if n >= t then
      begin
      dec(n, t);
      r := t + b;
      end;
    r := r shr 1;
    b := b shr 2;
    t := r+b;
    if n >= t then
      begin
      dec(n, t);
      r := t + b;
      end;
    r := r shr 1;
    b := b shr 2;
    t := r+b;
    if n >= t then
      begin
      dec(n, t);
      r := t + b;
      end;
    r := r shr 1;
    b := b shr 2;
    t := r+b;
    if n >= t then
      begin
      dec(n, t);
      r := t + b;
      end;
    r := r shr 1;
    b := b shr 2;
    until b=0;
  isqrt32 := r;
  end;

const {bit i is cleared if i mod 128 is a square}
  ba_128: array[0..15] of byte = ($ec,$fd,$fc,$fd,$ed,$fd,$fd,$fd,$ec,$fd,$fd,$fd,$ed,$fd,$fd,$fd);

const {set of byte size squares}
  sqrset = [0,1,4,9,16,25,36,49,64,81,100,121,144,169,196,225];

function is_square32ex(a: Longint; var b: Longint): boolean;
  {-Test if a is square, false if a<0. If yes, b = sqrt(a) else b is undefined}
  var
    i: Longint;
  begin
  is_square32ex := false;
  if a<256 then
    begin
    if a<0 then exit;
    if a in sqrset then
      begin
      is_square32ex := true;
      b := isqrt32(a);
      end;
    end
  else
    begin
    {First use the mp_is_square2 mod 128 test with a 82.03% rejection rate}
    i := a and 127;
    if ba_128[i shr 3] and (1 shl (i and 7)) <> 0 then exit;
    b := isqrt32(a);
    is_square32ex := sqr(b)=a;
    end;
  end;

procedure mp_div_2(const a: mp_int; var b: mp_int);
  {-Divide by 2, b = a/2}
  var
    oldused: Longint;
    i: TNInt;
    r,rr: mp_digit;
    pa,pb: pmp_digit;
  begin
  {if a=0 then just copy}
  if a.used=0 then
    begin
    mp_copy(a, b);
    exit;
    end;

  {grow dest}
  if b.alloc < a.used then
    mp_grow(b, a.used);

  oldused := b.used;
  b.used  := a.used;
  pa := @a.pdigits^[b.used-1];
  pb := @b.pdigits^[b.used-1];
  {carry}
  r  := 0;
  for i:=b.used-1 downto 0 do
    begin
    {get the carry for the next iteration}
    rr := pa^ and 1;
    {shift the current digit, add in carry and store}
    pb^ := (pa^ shr 1) or (r shl (DIGIT_BIT - 1));
    {forward carry to next iteration}
    r := rr;
    dec(pa);
    dec(pb);
    end;

  {zero excess digits}
  pb := @b.pdigits^[b.used];
  for i:=b.used to oldused-1 do
    begin
    pb^ := 0;
    inc(pb);
    end;
  b.sign := a.sign;
  mp_clamp(b);
  end;

procedure rec_sqrt(const a: mp_int; var b: mp_int);
  {-Compute b=floor(sqrt(a)), a>0, @a <> @b. Internal: no init. checks etc}
  var
    t: mp_int;
    s: Longint;
  begin
  s := mp_bitsize(a);
  if s<=31 then
    begin
    s := mp_get_int(a);
    mp_set_int(b, isqrt32(s));
    exit;
    end;
  mp_init(t);
  try
    {Init. approx. = (1+rec_sqrt(a div 2^(2s))*2^s, s = ceil(bitsize(a)/4)}
    s := (s + 3) div 4;
    mp_shr(a,2*s,b);
    rec_sqrt(b,t);
    mp_inc(t);
    mp_shl(t,s,t);
    {Newton iteration; init. approx t >= floor(sqrt(a)), see below}
    repeat
      mp_exch(t,b);
      {t := (t + (a div t)) div 2}
      mp_div(a,b,t);
      mp_add(b,t,t);
      mp_div_2(t,t);
      until mp_cmp(t,b)<>MP_LT;
  finally
    mp_clear(t);
    end;
  end;

procedure s_mp_sqrt(const a: mp_int; var b: mp_int);
  {-Compute b = floor(sqrt(a)), a >=0 using recursive Longint Newton square root, no init check}
  var
    t: mp_int;
  begin
  {must be positive}
  if a.sign=MP_NEG then
    raise ERtcRSA.Create('s_mp_sqrt: a < 0');

  {easy out for a=0/1}
  if mp_iszero(a) or mp_is1(a) then
    begin
    mp_copy(a,b);
    exit;
    end;

  if @a=@b then
    begin
    {Must use temporary local mp_int}
    mp_init(t);
    try
      rec_sqrt(a,t);
      {all the work is already done by rec_sqrt, store result}
      mp_exch(t,b);
    finally
      mp_clear(t);
      end;
    end
  else
    rec_sqrt(a,b);
  end;

procedure s_mp_sqrtrem(const n: mp_int; var s,r: mp_int);
  {-Compute Karatsuba square root s and remainder r of n >= 0, n = s^2 + r, no init check}
  var
    x,y: Longint;
    q,u: mp_int;
  begin
  {Primary reference: P. Zimmermann [34], Karatsuba Square Root.}
  {See also Modern Computer Arithmetic [35], Algorithm 1.12 SqrtRem}

  if n.used<mp_sqrt_cutoff then
    begin
    if mp_is_longint(n,y) then
      begin
      x := isqrt32(y);
      mp_set_int(s, x);
      mp_set_int(r, y-sqr(x));
      end
    else
      begin
      {use recursive Longint Newton square root algorithm}
      {and compute the remainder manually}
      s_mp_sqrt(n,s);
      mp_sqr(s,r);
      mp_sub(n,r,r);
      end;
    end
  else
    begin
    mp_init(q);
    mp_init(u);
    try
      { Algorithm SqrtRem(n = a_3*b^3 + a_2*b^2 + a_1*b + a_0)  }
      { Input: 0 <= a_i < b with a_3 >= b/4                     }
      { Output: (s, r) such that s^2 <= n = s^2 + r < (s + 1)^2 }
      {   (s, r) = SqrtRem(a_3*b + a_2)                         }
      {   (q, u) = DivRem(r*b + a_1, 2*s)                       }
      {        s = s*b + q                                      }
      {        r = u*b + a_0 - q^2                              }
      {        if r < 0 then                                    }
      {          r = r + 2*s - 1                                }
      {          s = s - 1                                      }
      {        end                                              }
      { return (s, r)                                           }

      {Here we set x = bitsize(n) div 4, b=2^x}
      x := mp_bitsize(n) shr 2;

      {a_3*b + a_2 = q = n div b^2 = n shr 2x}
      mp_shr(n,x+x,q);

      {(s, r) = SqrtRem(a_3*b + a_2)}
      s_mp_sqrtrem(q,s,r);

      {q = a_1 = (n shr x) mod 2^x}
      mp_shr(n,x,q);
      mp_mod_2k(q,x,q);

      {r*b + a_1}
      mp_shl(r,x,r);
      mp_add(r,q,r);
      mp_shl(s,1,q);

      {(q, u) = DivRem(r*b + a_1, 2*s)}
      mp_divrem(r,q,@q,@u);

      {s = s*b + q}
      mp_shl(s,x,s);
      mp_add(s,q,s);

      {r = u*b - q^2 + a_0,  a_0 = n mod 2^x}
      mp_shl(u,x,r);
      mp_sqr(q,q);
      mp_sub(r,q,r);
      mp_mod_2k(n,x,q);
      mp_add(r,q,r);

      if r.sign=MP_NEG then
        begin
        {r = r + 2*s - 1}
        mp_add(r,s,r);
        mp_add(r,s,r);
        mp_dec(r);
        {s = s - 1}
        mp_dec(s);
        end;
    finally
      mp_clear(q);
      mp_clear(u);
      end;
    end;
  end;

function mp_is_square2(const a: mp_int; psqrt: pmp_int): boolean;
  {-Test if a is square, return sqrt(a) if a is a square and psqrt<>nil}
  const {bit i is cleared if i mod n is a square, i=0..m-1}
    ba_37  : array[0..04] of byte = ($64,$e1,$de,$a1,$09);
    ba_41  : array[0..05] of byte = ($c8,$f8,$4a,$7d,$4c,$00);
    ba_43  : array[0..05] of byte = ($ac,$11,$5c,$7c,$a7,$04);
    ba_47  : array[0..05] of byte = ($20,$ac,$d8,$e4,$ca,$7b);
    ba_128 : array[0..15] of byte = ($ec,$fd,$fc,$fd,$ed,$fd,$fd,$fd,$ec,$fd,$fd,$fd,$ed,$fd,$fd,$fd);
  const
    DC_MAXLONG = (30+DIGIT_BIT) div DIGIT_BIT;
  var
    s,t: mp_int;
    k,r: Longint;
    i: Longint;
  begin
  {Default to Non-square}
  mp_is_square2 := false;

  if mp_not_init(a) or ((psqrt<>nil) and mp_not_init(psqrt^)) then
    raise ERtcRSA.Create('mp_is_square2');

  {Negative numbers are not square}
  if a.sign=MP_NEG then exit;

  {0 and 1 are square}
  if (a.used=0) or ((a.used=1) and (a.pdigits^[0]=1)) then
    begin
    mp_is_square2 := true;
    if psqrt<>nil then mp_copy(a, psqrt^);
    exit;
    end;

  {First check mod 128 (DIGIT_BIT is at least 8)}
  i := a.pdigits^[0] and 127;
  if ba_128[i shr 3] and (1 shl (i and 7)) <> 0 then exit;
  {82.03% rejection rate}

  {Brute force for remaining 32 bit arguments}
  if (a.used<=DC_MAXLONG) and mp_is_longint(a,k) then
    begin
    if is_square32ex(k,r) then
      begin
      mp_is_square2 := true;
      if psqrt<>nil then mp_set_int(psqrt^,r);
      end;
    exit;
    end;

  mp_mod_int(a,2029964475,r);   {31*29*25*23*21*17*11}
  if (1 shl (r mod 21)) and $1A7D6C   <> 0 then exit;  {0.6190}
  if (1 shl (r mod 25)) and $D6B5AC   <> 0 then exit;  {0.5600}
  if (1 shl (r mod 31)) and $6DE2B848 <> 0 then exit;  {0.4839}
  if (1 shl (r mod 29)) and $C2EDD0C  <> 0 then exit;  {0.4828}
  if (1 shl (r mod 23)) and $7ACCA0   <> 0 then exit;  {0.4783}
  if (1 shl (r mod 17)) and $5CE8     <> 0 then exit;  {0.4706}
  if (1 shl (r mod 11)) and $5C4      <> 0 then exit;  {0.4545}
  {99.88% cumulative rejection rate}

  mp_mod_int(a,757266679,r); {13*19*37*41*43*47}
  i := r mod 47; if ba_47[i shr 3] and (1 shl (i and 7)) <> 0 then exit; {0.4894}
  i := r mod 43; if ba_43[i shr 3] and (1 shl (i and 7)) <> 0 then exit; {0.4884}
  i := r mod 41; if ba_41[i shr 3] and (1 shl (i and 7)) <> 0 then exit; {0.4878}
  i := r mod 37; if ba_37[i shr 3] and (1 shl (i and 7)) <> 0 then exit; {0.4865}
  if (1 shl (r mod 19)) and $4F50C <> 0 then exit;  {0.4737}
  if (1 shl (r mod 13)) and $9E4   <> 0 then exit;  {0.4615}
  {99.9976% cumulative rejection rate}

  mp_init(s);
  mp_init(t);
  try
    {Final check: test if a-sqrt(a)^2=0}
    s_mp_sqrtrem(a,s,t);
    if mp_iszero(t) then
      begin
      mp_is_square2 := true;
      if psqrt<>nil then mp_exch(s, psqrt^);
      end;
  finally
    mp_clear(s);
    mp_clear(t);
    end;
  end;

function mp_is_square(const a: mp_int): boolean;
  {-Test if a is square}
  begin
  mp_is_square := mp_is_square2(a, nil);
  end;

procedure mp_add_d(const a: mp_int; b: mp_digit; var c: mp_int);
  {-Single digit addition}
  begin
  if b>MP_DIGIT_MAX then
    raise ERtcRSA.Create('mp_add_d: b>MP_DIGIT_MAX');
  if b=0 then mp_copy(a,c)
  else s_mp_add_d(a,b,c);
  end;

function mp_isbit(const a: mp_int; n: Longint): boolean;
  {-Test if bit n of a is set, (1 = bit 0)}
  var
    d,k: TNInt;
  begin
  mp_isbit := false;
  if (n<0) or (n>MP_MAXBIT) then exit;
  k := n div MP_DIGIT_BIT;
  d := mp_digit(n mod MP_DIGIT_BIT);
  with a do
    mp_isbit := (pdigits<>nil) and (used>0) and (k<used) and odd(pdigits^[k] shr d);
  end;

procedure s_mp_lucasvmod1(const p,n,k,mu: mp_int; var v: mp_int);
  {-Calculate v=v[k] mod n of Lucas V sequence for p,q=1. mu: Barrett parameter}
  { for n, k>0. Internal use: no init check, assumes p<n, return v=2 for k<=0.}
  var
    i,bc: Longint;
    v1: mp_int;
  begin
  {return v=2 if k<1}
  bc := mp_bitsize(k);
  if (bc=0) or (k.sign=MP_NEG) then
    begin
    mp_set(v,2);
    exit;
    end;

  {Initialize v=p, and v1=(p^2-2) mod n if k>1}
  mp_copy(p,v);
  if bc=1 then exit;

  mp_init(v1);
  try
    mp_sqr(p,v1);           mp_reduce(v1,n,mu);
    mp_sub_d(v1,2,v1);      if v1.sign=MP_NEG then mp_add(v1,n,v1);

    for i:=bc-2 downto 1 do
      begin
      if mp_isbit(k,i) then
        begin
        {v = v*v1 - p (mod n), v1 = v1^2 - 2 (mod n)}
        mp_mul(v,v1,v);     mp_reduce(v,n,mu);
        mp_sub(v,p,v);      if v.sign=MP_NEG then mp_add(v,n,v);
        mp_sqr(v1,v1);      mp_reduce(v1,n,mu);
        mp_sub_d(v1,2,v1);  if v1.sign=MP_NEG then mp_add(v1,n,v1);
        end
      else
        begin
        {v1 = v*v1 - p (mod n), v = v^2 - 2 (mod n)}
        mp_mul(v,v1,v1);    mp_reduce(v1,n,mu);
        mp_sub(v1,p,v1);    if v1.sign=MP_NEG then mp_add(v1,n,v1);
        mp_sqr(v,v);        mp_reduce(v,n,mu);
        mp_sub_d(v,2,v);    if v.sign=MP_NEG then mp_add(v,n,v);
        end;
      end;
    {Calculate final v (v1 not needed)}
    if mp_isodd(k) then
      begin
      mp_mul(v,v1,v);       mp_reduce(v,n,mu);
      mp_sub(v,p,v);        if v.sign=MP_NEG then mp_add(v,n,v);
      end
    else
      begin
      mp_sqr(v,v);          mp_reduce(v,n,mu);
      mp_sub_d(v,2,v);      if v.sign=MP_NEG then mp_add(v,n,v);
      end;
  finally
    mp_clear(v1);
    end;
  end;

function mp_is_slpsp(const a: mp_int): boolean;
  {-Strong Lucas probable prime test for a. Lucas test is }
  { done for the first p=2k+1 with mp_jacobi(p^2-4,a) = -1}
  const
    bmax = $7FFF; {max p for Jabobi test)}
    bsqr = 127;   {p index for perfect square test, must be odd}
  label
    leave;
  var
    b: Longint;
    d,i: Longint;
    mu,p,t,v: mp_int;
  begin
  {initialize function result with false}
  mp_is_slpsp := false;

  {easy outs: for a<3 return true only if a=2}
  b := mp_cmp_d(a,2);
  if (a.sign=MP_NEG) or (b=MP_LT) then exit;
  if mp_iseven(a) then
    begin
    mp_is_slpsp := (b=MP_EQ);
    exit;
    end;

  {create temporary mp_ints}
  mp_init(mu);
  mp_init(p);
  mp_init(t);
  mp_init(v);

  try
    {generate sequence without squaring: p(k)=2k+1, D(k)=p(k)^2 - 4}
    {p(1)=3, D(1)=5, D(k+1)=D(k)+i(k+1), with i(k+1)=4p(k+1)=i(k)+8}
    {Lucas test is done for the first p with mp_jacobi(p^2-4,a)=-1.}
    {This sequence is from Wei Dai / Marcel Martin, see references.}

    b := 3;
    d := 5;
    i := 8;
    repeat
      if mp_jacobi_lm(d,a)=-1 then break;
      if b=bsqr then
        begin
        {Avoid 'hopeless' loop and test whether a is a perfect square. This }
        {is delayed because we are searching for jacobi(d,a)=-1; if there is}
        {such a d then a is not square, and a square root will be computed  }
        if mp_is_square(a) then goto leave;
        end;
      inc(i,8);
      inc(d,i);
      inc(b,2);
      {assert(d+4=sqr(Longint(b)), 'd = b^2 - 4 in mp_is_slpsp');}
      until b>=bmax;

    {exit if no proper p=b found}
    if b>=bmax then goto leave;

    {p=b>0 should be MUCH less than a but ...}
    mp_set_int(p,b);
    if mp_cmp(a,p)=MP_LT then mp_mod(p,a,p);

    {calculate t,s with a+1 = 2^i*t}
    mp_add_d(a,1,t);
    mp_makeodd(t,t,i);

    {Setup Barrett reduction for a}
    mp_reduce_setup(mu, a);

    {Calculate Lucas sequence}
    s_mp_lucasvmod1(p,a,t,mu,v);

    {t=n-2 for the next compares}
    mp_sub_d(a,2,t);

    if (mp_cmp_d(v,2)=MP_EQ) or (mp_cmp(v,t)=MP_EQ) then
      begin
      {a is lpsp}
      mp_is_slpsp := true;
      goto leave;
      end;

    for d:=1 to i-1 do
      begin
      mp_sqr(v,v);      mp_reduce(v,a,mu);
      mp_sub_d(v,2,v);  if v.sign=MP_NEG then mp_add(v,a,v);
      if mp_cmp_d(v,2)=MP_EQ then goto leave;
      if mp_cmp(v,t)=MP_EQ then
        begin
        {a is lpsp}
        mp_is_slpsp := true;
        goto leave;
        end;
      end;
leave:
  finally
    mp_clear(mu);
    mp_clear(p);
    mp_clear(t);
    mp_clear(v);
    end;
  end;

function mp_is_le(const a,b: mp_int): boolean;
  {-Return a <= b}
  begin
  mp_is_le := (mp_cmp(a,b)<>MP_GT);
  end;

procedure mp_rsa_calc_npq(var ctx:isaac_ctx; const e: mp_int; osize: word; var n,p,q: mp_int);
  {-Generate RSA primes p,q; q<p, n=p*q, osize: octet size of n;}
  { e: public key encryption exponent, e odd and greater than 2}
  var
    t: mp_int;
    s3,s4,s8: word;
    f: mp_digit;
  const
    fmaxs = mp_digit(MP_DIGIT_MAX and $3FF);
  begin
  {check requested octet size of n}
  if osize<RSA_MINSIZE then
    raise ERtcRSA.Create('mp_rsa_calc_npq: osize < RSA_MINSIZE');

  mp_init(t);
  try
    {bit size of p and q = 1/2 bit size of n = 4*osize}
    s3 := 3*osize;
    s4 := 4*osize;
    s8 := s4+s4;
    {generate p with gcd(p-1,e)=1}
    repeat
      mp_rand_bits(ctx,p,s4);
      p.pdigits^[0] := p.pdigits^[0] or 1;
      {first test if p has small factor}
      mp_small_factor(p,3,fmaxs,f);
      if f<>0 then continue;
      {now check gcd(p-1,e)=1}
      mp_sub_d(p,1,t);
      if mp_gcd1(e,t,t) then
        begin
        {done if p is a BPSW probable prime}
        if mp_is_spsp_d(p,2) and mp_is_slpsp(p) then break;
        end;
      until false;
    {generate q with gcd(q-1,e)=1}
    repeat
      mp_rand_bits(ctx,q,s4);
      q.pdigits^[0] := q.pdigits^[0] or 1;
      {ensure |p-q| is not too small}
      mp_sub(p,q,t);
      if mp_bitsize(t) < s3 then
        continue;
      {test if q has a small factor}
      mp_small_factor(q,3,fmaxs,f);
      if f<>0 then continue;
      {check if n=p*q has required bit size}
      mp_mul(p,q,n);
      if mp_bitsize(n)<>s8 then continue;
      {now check gcd(q-1,e)=1}
      mp_sub_d(q,1,t);
      if mp_gcd1(e,t,t) then
        begin
        {done if q is a BPSW probable prime}
        if mp_is_spsp_d(q,2) and mp_is_slpsp(q) then break;
        end;
      until false;
    if mp_is_le(p,q) then mp_exch(p,q);
  finally
    mp_clear(t);
    end;
  end;

procedure mp_rsa_keygen2(var ctx:isaac_ctx; const e: mp_int; osize: word; var n: mp_int; var prk: TPrivateKey);
  {-Generate private RSA/CRT key prk and modulus n; osize: octet size of n;}
  { e: public key encryption exponent, e odd and greater than 2}
  begin
  with prk do
    begin
    {other args ar checked in mp_rsa_calc_npq}
    if mp_not_init(dp) or mp_not_init(dq) or mp_not_init(qinv) then
      raise ERtcRSA.Create('mp_rsa_keygen2');

    {first get RSA primes p,q}
    mp_rsa_calc_npq(ctx,e,osize,n,p,q);
    {dp := e^-1 mod (p-1)}
    mp_sub_d(p,1,qinv);
    mp_invmod(e,qinv,dp);
    {dq := e^-1 mod (q-1)}
    mp_sub_d(q,1,qinv);
    mp_invmod(e,qinv,dq);
    {qinv := q^-1 mod p}
    mp_invmod(q,p,qinv);
    end;
  end;

function mp_unsigned_bin_size(const a: mp_int): Longint;
  {-Get the size in bytes for an unsigned equivalent}
  var
    size: Longint;
  begin
  // mp_unsigned_bin_size := 0;
  {Arg check in mp_bitsize}
  size := (mp_bitsize(a)+7) div 8;
  {Size should always be < about 32000 = max allocated bytes}
  mp_unsigned_bin_size := size and $FFFF;
  end;

function mp_pkcs1v15_maxlen(const n: mp_int): word;
  {-Maximum message length for RSA modulus n using PKCS1-v1_5 encoding}
  var
    modlen: word;
  begin
  {MPC_ArgCheck in mp_unsigned_bin_size or deeper}
  modlen := mp_unsigned_bin_size(n);
  {at least RSA_MINSIZE bytes are required for EME-PKCS1-v1_5 padding}
  if modlen<RSA_MINSIZE then mp_pkcs1v15_maxlen := 0
  else mp_pkcs1v15_maxlen := modlen-RSA_MINSIZE;
  end;

type
  TArrayOfByte = packed array[0..RtcMaxLongInt] of byte; {helper type}

procedure mp_reverse(var s; len: Longint);
  {-Reverse an array of char, used for radix code}
  var
    sa: TArrayOfByte absolute s;
    i: Longint;
    t: byte;
  begin
  if len>1 then
    begin
    i := 0;
    dec(len);
    while i<len do
      begin
      t := sa[i];
      sa[i] := sa[len];
      sa[len] := t;
      inc(i);
      dec(len);
      end;
    end;
  end;

function mp_to_unsigned_bin_n(const a: mp_int; var b; n: Longint): Longint;
  {-Store in unsigned big-endian format, max n bytes; return no. of bytes stored}
  var
    ba: TArrayOfByte absolute b;
    i,x: Longint;
    bits: Longint;
    bbuf: mp_word;
  label
    maxn;
  begin
  mp_to_unsigned_bin_n := 0;
  if mp_iszero(a) or (n=0) then exit;

  {initialize bit buffer, bit counter, and byte counter}
  bbuf := 0;
  bits := 0;
  x    := 0;
  for i:=0 to a.used-1 do
    begin
    {accumulate next digit into bit buffer}
    bbuf := bbuf or mp_word(a.pdigits^[i]) shl bits;
    inc(bits, DIGIT_BIT);
    while bits>=8 do
      begin
      if x<n then
        begin
        ba[x] := byte(bbuf and $FF);
        inc(x);
        end
      else
        goto maxn;
      dec(bits,8);
      bbuf := bbuf shr 8;
      end;
    end;

  {here bbuf must be checked! bits may be non zero,}
  {even if all the remaining highest bits are zero!}
  while (bbuf<>0) and (x<n) do
    begin
    ba[x] := byte(bbuf and $FF);
    inc(x);
    bbuf := bbuf shr 8;
    end;

maxn:
  {Clamp high order zero bytes}
  while (x>0) and (ba[x-1]=0) do dec(x);
  if x>1 then mp_reverse(b, x);

  mp_to_unsigned_bin_n := x;
  end;

procedure mp_i2osp(const x: mp_int; outp: pointer; len: word);
  {-Convert a nonnegative mp_int to an octet string of a specified length}
  var
    lx,l0: word;
  begin
  {MPC_ArgCheck in mp_unsigned_bin_size or deeper}
  lx := mp_unsigned_bin_size(x);
  if lx>len then
    raise ERtcRSA.Create('mp_i2osp: x too large"');
  l0 := len-lx;
  {zero "leading digits"}
  fillchar(outp^,l0,0);
  Inc(RtcIntPtr(outp),l0);
  {$ifopt X-} l0 := {$endif} mp_to_unsigned_bin_n(x,outp^,lx);
  end;

procedure mp_read_unsigned_bin(var a: mp_int; const b; numbytes: Longint);
  {-Reads a unsigned mp_int, assumes the msb is stored first [big endian]}
  var
    ba: TArrayOfByte absolute b;
    bbuf: mp_word; {buffer for sequential bits}
    n: Longint;
    i: TNint;
    lim,bits: Longint;
    numbits: Longint;
  const
    mask: array[0..7] of word = ($0000,$0001,$0003,$0007,$000F,$001F,$003F,$007F);
  begin
  mp_zero(a);
  if numbytes=0 then exit;

  {Calculate number of trailing highest bits, these must go into a  }
  {separate digit, i.e. big endianness is digit-based not bit-based.}
  numbits := Longint(numbytes)*8;
  lim := numbits mod DIGIT_BIT;
  if lim=0 then lim := DIGIT_BIT;

  {index of highest digit}
  n := (numbits-1) div DIGIT_BIT;

  {single allocate enough digits}
  if a.alloc<n+2 then
    mp_grow(a,n+2);

  a.used := n+1;

  {initialize bit buffer and bit count}
  bits := 0;
  bbuf := 0;

  with a do
    begin
    for i:=0 to numbytes-1 do
      begin
      {accumulate next 8 bits}
      bbuf := (bbuf shl 8) or ba[i];
      inc(bits,8);
      if bits >= lim then
        begin
        dec(bits, lim);
        pdigits^[n] := mp_digit(bbuf shr bits);
        bbuf := bbuf and mask[bits];
        {$ifopt R+}
          {FPC produces RTE 201 for dec(0)}
          if n>0 then dec(n);
        {$else}
          dec(n);
        {$endif}
        {after first (partial) digit set limit bit count to DIGIT_BIT}
        lim := DIGIT_BIT;
        end;
      end;
    end;
  mp_clamp(a);
  end;

procedure mp_os2ip(inp: pointer; ilen: word; var x: mp_int);
  {-Convert an octet string of length ilen to a nonnegative mp_int}
  begin
  {MPC_ArgCheck in mp_read_unsigned_bin}
  mp_read_unsigned_bin(x, inp^, ilen);
  end;

procedure mp_submod(const a,b,c: mp_int; var d: mp_int);
  {-Calculate d = a - b (mod c)}
  var
    t: mp_int;
  begin
  mp_init(t);
  try
    mp_sub(a, b, t);
    mp_mod(t, c, d);
  finally
    mp_clear(t);
    end;
  end;

procedure mp_rsadp2(const c: mp_int; const prk: TPrivateKey; var m: mp_int);
  {-Basic RSA decryption operation for private key CRT record.}
  { c   : ciphertext representative, an Longint between 0 and n - 1. Note
          that this condition will be checked only approximately.
    prk : RSA private key CRT record.
    m   : message representative, an Longint between 0 and n - 1.}
  var
    t: mp_int;
  begin
  {check c,n here; d,m are checked in mp_exptmod}
  if mp_not_init(c) or mp_not_init(m) or mp_not_init(prk.p) or mp_not_init(prk.q) or
     mp_not_init(prk.dp) or mp_not_init(prk.dq) or mp_not_init(prk.qinv) then
    raise ERtcRSA.Create('mp_rsadp2');

  {Insure that ciphertext representative is in range of modulus}
  if (c.sign=MP_NEG) or (mp_bitsize(c) > (mp_bitsize(prk.p)+mp_bitsize(prk.q))) then
    raise ERtcRSA.Create('mp_rsadp2: ciphertext representative out of range');

  mp_init(t);
  try
    with prk do
      begin
      {no need to reduce c mod p/q, will be done implicitly in mp_exptmod}
      {m1 = c^dp mod p}
      mp_exptmod(c,dp,p,t);
      {m2 = c^dq mod q}
      mp_exptmod(c,dq,q,m);
      {h  = (m1-m2)*qinv mod p}
      mp_submod(t,m,p,t);
      mp_mulmod(t,qinv,p,t);
      {m = m2+q*h}
      mp_mul(q,t,t);
      mp_add(t,m,m);
      end;
  finally
    mp_clear(t);
    end;
  end;

type
  TByteArr = packed array[0..65519] of byte;  {Helper types}
  PByteArr = ^TByteArr;

function  mp_pkcs1v15_decode(em, msg: pointer; emlen: word; var mlen: word): boolean;
  {-EME-PKCS1-v1_5 decoding; true if decoding is successful, false otherwise}
  { em    - encoded message
    emlen - length of encoded message, in bytes
    msg   - decode message (may be same as em)
    mlen  - length of decoded msg}
  var
    ep: PByteArr absolute em;
    ip: word;
    res: boolean;
  begin
  {EME-PKCS1-v1_5 decoding: Separate the encoded message EM into an
   octet string PS consisting of nonzero octets and a message M as

      EM = 0x00 || 0x02 || PS || 0x00 || MSG.

   If the first octet of EM does not have hexadecimal value 0x00, if the
   second octet of EM does not have hexadecimal value 0x02, if there is
   no octet with hexadecimal value 0x00 to separate PS from M, or if the
   length of PS is less than 8 octets, output "decryption error".

   Note: Care shall be taken to ensure that an opponent cannot distinguish
   the different error conditions, whether by error message or timing.
   Otherwise an opponent may be able to obtain useful information about
   the decryption of the ciphertext C. WE note: This is partly achieved
   by running through the entire code without error exit.}

  res := true;

  {check min length and starting bytes}
  if emlen<RSA_MINSIZE then res := false;
  if ep^[0]<>$00 then res := false;
  if ep^[1]<>$02 then res := false;

  {look for zero separator}
  ip := 2;
  while ip<emlen do
    begin
    if ep^[ip]=0 then break;
    inc(ip);
    end;

  {position of zero separator must be >= 10 and < emlen}
  if (ip=emlen) or (ip<10) then res := false;

  {copy msg}
  mlen := 0;
  if emlen > (ip + 1) then
    begin
    mlen := emlen - (ip + 1);
    move(ep^[ip+1],msg^,mlen);
    end
  else
    res := false;

  mp_pkcs1v15_decode := res;
  end;

procedure mp_pkcs1v15_decrypt2(const prk: TPrivateKey; const n: mp_int; ctp,ptp: pointer; clen,pmax: word; var plen: word);
  {-Decrypt a message using RSA/CRT and EME-PKCS1-v1_5 padding}
  { prk,n - private key CRT record and modulus
    ctp   - input message (ciphertext)
    ptp   - pointer to buffer holding decrypted plaintext
    clen  - length of input message, in bytes
    pmax  - max. length of plaintext buffer
    plen  - length of plaintext, in bytes}
  var
    k: word;
    c: mp_int;
    buf: pointer;
  begin
  {Init check n in mp_unsigned_bin_size or below}
  {get size of modulus n in bytes}
  k := mp_unsigned_bin_size(n);
  if clen<>k then
    raise ERtcRSA.Create('mp_pkcs1v15_decrypt2: clen <> k');
  if pmax<k then
    raise ERtcRSA.Create('mp_pkcs1v15_decrypt2: pmax < k');

  {get local memory for encoded message}
  buf:=mp_alloc(clen);
  if buf=nil then
    raise ERtcRSA.Create('mp_pkcs1v15_decrypt2: buf = nil');
  {initialize ciphertext mp_int representative}
  mp_init(c);
  try
    {convert ciphertext to mp_int representative}
    mp_os2ip(ctp, clen, c);
    {apply the RSADP decryption primitive to the ciphertext representative c}
    mp_rsadp2(c, prk, c);
    {Convert the message representative to an encoded message}
    mp_i2osp(c, buf, k);
    {EME-PKCS1-v1_5 decoding buf -> ptp}
    if not mp_pkcs1v15_decode(buf, ptp, k, plen) then
      raise ERtcRSA.Create('mp_pkcs1v15_decrypt2: decode error');
  finally
    mp_clear(c);
    fillchar(buf^,clen,0);
    mp_freemem(buf, clen);
    end;
  end;

function mp_pkcs1v15_encode(var ctx:isaac_ctx; msg, em: pointer; mlen, emlen: word): boolean;
  {-EME-PKCS1-v1_5 encoding; true if encoding is successful, false otherwise}
  { msg   - plaintext message
    mlen  - length of plaintext message
    em    - encoded message
    emlen - length of encoded message, in bytes, will be padded
    rnd   - pad byte, if pad=0 use bytes generated with mp_random_byte}
  var
    i,ip: word;
    ep: PByteArr absolute em;
    rb: byte;
  begin
  if mlen>emlen-RSA_MINSIZE then
    mp_pkcs1v15_encode := false
  else
    begin
    {EME-PKCS1-v1_5 encoding:
     Generate an octet string PS of length emLen - mLen - 3 consisting
     of pseudo-randomly generated nonzero octets.  The length of PS
     will be at least eight octets.

     Concatenate PS, the message MSG, and other padding to form an
     encoded message EM of length k octets as

           EM = 0x00 || 0x02 || PS || 0x00 || MSG.
     WE-Note: First 0 byte makes mp_int representative less than modulus}
    {insert starting bytes}
    ep^[0] := $00;
    ep^[1] := $02;
    {calculate padding offset. Note: mlen<=emlen-RSA_MINSIZE -> ip>=RSA_MINSIZE}
    ip := emlen-mlen;
    {insert padding bytes}

    for i:=2 to ip-2 do
      begin
      {generate random nonzero padding bytes}
      repeat
        rb := mp_random_byte(ctx);
        until rb<>0;
      ep^[i] := rb;
      end;
    {insert zero separator}
    ep^[ip-1] := 0;
    move(msg^,ep^[ip],mlen);
    mp_pkcs1v15_encode := true;
    end;
  end;

procedure mp_rsaep(const m, e, n: mp_int; var c: mp_int);
  {-Basic RSA encryption operation, c=m^e mod n.}
  { m   : message representative, an Longint between 0 and n - 1.
    n,e : RSA public key pair (n, e) = (modulus, exponent).
    c   : ciphertext representative, an Longint between 0 and n - 1.}
  begin
  {Insure that message representative is in range of modulus}
  if (m.sign=MP_NEG) or (mp_cmp(m, n)<>MP_LT) then
    raise ERtcRSA.Create('mp_rsaep: message representative out of range');
  mp_exptmod(m, e, n, c);
  end;

procedure mp_pkcs1v15_encrypt(var ctx:isaac_ctx; const e, n: mp_int; ptp,ctp: pointer; plen,cmax: word; var clen: word);
  {-Encrypt a message using RSA and EME-PKCS1-v1_5 padding}
  { n,e   - recipient's RSA public key pair (n, e) = (modulus, exponent)
    rnd   - padding byte, if 0 mp_random_byte is used
    ptp   - input message (plaintext)
    ctp   - output message (ciphertext)
    plen  - length of buffer holding plaintext, in bytes
    cmax  - max length of ciphertext buffer
    clen  - length of output message, in bytes}
  var
    k: word;
    c: mp_int;
    buf: pointer;
  begin
  {get size of modulus n in bytes}
  k := mp_unsigned_bin_size(n);
  if cmax<k then
    raise ERtcRSA.Create('mp_pkcs1v15_encrypt: cmax < k');
  clen := k;

  {get local memory for encoded message}
  buf:=mp_alloc(clen);
  if buf=nil then
    raise ERtcRSA.Create('mp_pkcs1v15_encrypt: buf = nil');
  {initialize ciphertext mp_int representative}
  mp_init(c);
  try
    {-EME-PKCS1-v1_5 encoding ptp -> buf}
    if not mp_pkcs1v15_encode(ctx, ptp, buf, plen, k) then
      raise ERtcRSA.Create('mp_pkcs1v15_encrypt: encode error')
    else
      begin
      {Convert the encoded message EM to mp_int representative c}
      mp_os2ip(buf, k, c);
      {Apply the RSAEP encryption primitive to the message representative c}
      mp_rsaep(c, e, n, c);
      {Convert the ciphertext representative c to octet string}
      mp_i2osp(c, ctp, clen);
      end;
  finally
    mp_clear(c);
    fillchar(buf^,clen,0);
    mp_freemem(buf, clen);
    end;
  end;

function mp_pkcs1v15_emsa_encode(AHash: RtcHashType; hdp, smp: pointer; hlen, slen: word): boolean;
  {-EMSA-PKCS1-v1_5 encoding; true if encoding is successful, false otherwise}
  { AHash - hash algorithm for signature
    hdp   - hash digest
    hlen  - length of hash digest
    smp   - encoded signature message
    slen  - length of encoded signature, in bytes, will be padded}
  var
    i,ip,tlen: word;
    ep: PByteArr absolute smp;
    aip: pointer;
(* From RFC 3447, Section 9.2 EMSA-PKCS1-v1_5, Note 1
   MD2:     (0x)30 20 30 0c 06 08 2a 86 48 86 f7 0d 02 02 05 00 04 10 || H.
   MD5:     (0x)30 20 30 0c 06 08 2a 86 48 86 f7 0d 02 05 05 00 04 10 || H.
   SHA-1:   (0x)30 21 30 09 06 05 2b 0e 03 02 1a 05 00 04 14 || H.
   SHA-256: (0x)30 31 30 0d 06 09 60 86 48 01 65 03 04 02 01 05 00 04 20 || H.
   SHA-384: (0x)30 41 30 0d 06 09 60 86 48 01 65 03 04 02 02 05 00 04 30 || H.
   SHA-512: (0x)30 51 30 0d 06 09 60 86 48 01 65 03 04 02 03 05 00 04 40 || H.

 * From RFC 4880, Sec 5.2.2.  Version 3 Signature Packet Format
   (Second byte of SHA224 from Errata ID: 2270, wrong original reads $31)
   MD5:        0x30, 0x20, 0x30, 0x0C, 0x06, 0x08, 0x2A, 0x86,
               0x48, 0x86, 0xF7, 0x0D, 0x02, 0x05, 0x05, 0x00,
               0x04, 0x10
   RIPEMD-160: 0x30, 0x21, 0x30, 0x09, 0x06, 0x05, 0x2B, 0x24,
               0x03, 0x02, 0x01, 0x05, 0x00, 0x04, 0x14
   SHA-1:      0x30, 0x21, 0x30, 0x09, 0x06, 0x05, 0x2b, 0x0E,
               0x03, 0x02, 0x1A, 0x05, 0x00, 0x04, 0x14
   SHA224:     0x30, 0x2d, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86,
               0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x04, 0x05,
               0x00, 0x04, 0x1C
   SHA256:     0x30, 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86,
               0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x01, 0x05,
               0x00, 0x04, 0x20
   SHA384:     0x30, 0x41, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86,
               0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x02, 0x05,
               0x00, 0x04, 0x30
   SHA512:     0x30, 0x51, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86,
               0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x03, 0x05,
               0x00, 0x04, 0x40
*)
  const
    ai_md2:    array[0..17] of byte = ($30,$20,$30,$0c,$06,$08,$2a,$86,$48,$86,$f7,$0d,$02,$02,$05,$00,$04,$10);
    ai_md5:    array[0..17] of byte = ($30,$20,$30,$0c,$06,$08,$2a,$86,$48,$86,$f7,$0d,$02,$05,$05,$00,$04,$10);
    ai_rmd160: array[0..14] of byte = ($30,$21,$30,$09,$06,$05,$2B,$24,$03,$02,$01,$05,$00,$04,$14);
    ai_sha1:   array[0..14] of byte = ($30,$21,$30,$09,$06,$05,$2b,$0e,$03,$02,$1a,$05,$00,$04,$14);
    ai_sha224: array[0..18] of byte = ($30,$2d,$30,$0d,$06,$09,$60,$86,$48,$01,$65,$03,$04,$02,$04,$05,$00,$04,$1C);
    ai_sha256: array[0..18] of byte = ($30,$31,$30,$0d,$06,$09,$60,$86,$48,$01,$65,$03,$04,$02,$01,$05,$00,$04,$20);
    ai_sha384: array[0..18] of byte = ($30,$41,$30,$0d,$06,$09,$60,$86,$48,$01,$65,$03,$04,$02,$02,$05,$00,$04,$30);
    ai_sha512: array[0..18] of byte = ($30,$51,$30,$0d,$06,$09,$60,$86,$48,$01,$65,$03,$04,$02,$03,$05,$00,$04,$40);
  begin

  (* EMSA-PKCS1-v1_5 encoding:
     Generate an octet string PS of length slen-(tlen+hlen) - 3 consisting
     of $FF bytes. The length of PS will be at least eight octets.

     Concatenate PS, the DigestInfo (algorithm identifier || hash digest) and
     other padding to form an signature message SM of length slen octets:
     SM = 0x00 || 0x01 || PS || 0x00 || DigestInfo.

     WE-Note: First 0 byte makes mp_int representative less than modulus
  *)

  mp_pkcs1v15_emsa_encode := false;

  case AHash of
       rtch_MD2: begin tlen := sizeof(ai_md2);    aip := @ai_md2;    end;
       rtch_MD5: begin tlen := sizeof(ai_md5);    aip := @ai_md5;    end;
    rtch_RMD160: begin tlen := sizeof(ai_rmd160); aip := @ai_rmd160; end;
      rtch_SHA1: begin tlen := sizeof(ai_sha1);   aip := @ai_sha1;   end;
    rtch_SHA224: begin tlen := sizeof(ai_sha224); aip := @ai_sha224; end;
    rtch_SHA256: begin tlen := sizeof(ai_sha256); aip := @ai_sha256; end;
    rtch_SHA384: begin tlen := sizeof(ai_sha384); aip := @ai_sha384; end;
    rtch_SHA512: begin tlen := sizeof(ai_sha512); aip := @ai_sha512; end;
    rtch_Custom: begin tlen := 0; aip := nil; end;
         else  exit; {invalid AHash}
  end;

  {check hash length}
  if tlen>0 then
    if hlen<>PByteArr(aip)^[pred(tlen)] then exit;

  {check if slen is large enough}
  if tlen + hlen + RSA_MINSIZE > slen then exit;

  {Here AHash and lengths are valid; calculate padding offset.}
  {Note: hlen + tlen <= slen - RSA_MINSIZE -> ip >= RSA_MINSIZE}
  ip := slen - tlen - hlen;

  {insert starting bytes}
  ep^[0] := $00;
  ep^[1] := $01;
  {insert $FF padding bytes}
  for i:=2 to ip-2 do ep^[i] := $FF;
  {insert zero separator}
  ep^[ip-1] := 0;
  {insert algorithm identifier}
  if tlen>0 then
    move(aip^,ep^[ip],tlen);
  {insert hash digest}
  move(hdp^,ep^[ip+tlen],hlen);

  mp_pkcs1v15_emsa_encode := true;
  end;

function mp_is_eq(const a,b: mp_int): boolean;
  {-Return a = b}
  begin
  mp_is_eq := (mp_cmp(a,b)=MP_EQ);
  end;

function mp_pkcs1v15_verify(const e, n: mp_int; AHash: RtcHashType; hdp,smp: pointer; hlen,slen: word): boolean;
  {-Signature verification operation}
  { n,e   - signer's RSA public key pair (n, e) = (modulus, exponent)
    AHash - hash algorithm for signature
    hdp   - hash digest
    hlen  - length of hash digest
    smp   - signature message
    slen  - length of signature in bytes}
  var
    k: word;
    s,m: mp_int;
    buf: pointer;
  begin
  mp_pkcs1v15_verify := false;

  {Get size of modulus n in bytes}
  k := mp_unsigned_bin_size(n);
  if slen<>k then
    raise ERtcRSA.Create('mp_pkcs1v15_verify: slen <> k');

  {Get local memory for second encoded message}
  buf := mp_alloc(slen);
  if buf=nil then
    raise ERtcRSA.Create('mp_pkcs1v15_verify: buf = nil');
  try
    {Produce a second encoded message}
    if mp_pkcs1v15_emsa_encode(AHash, hdp, buf, hlen, slen) then
      begin
      mp_init(s);
      mp_init(m);
      try
        {Convert the signature to an Longint signature representative}
        mp_os2ip(smp, k, s);
        {Apply the RSA verification primitive}
        mp_rsaep(s,e,n,m);
        {Convert the second encode message to an Longint representative}
        mp_os2ip(buf, k, s);
        {Verify that both Longint representatives are the same}
        mp_pkcs1v15_verify := mp_is_eq(s,m);
      finally
        mp_clear(s);
        mp_clear(m);
        end;
      end;
  finally
    fillchar(buf^,slen,0);
    mp_freemem(buf, slen);
    end;
  end;

procedure mp_pkcs1v15_sign2(const prk: TPrivateKey; const n: mp_int; AHash: RtcHashType;
                            hdp,smp: pointer; hlen,smax: word; var slen: word);
  {-Sign a hash digest using RSA/CRT and EMSA-PKCS1-v1_5 encoding}
  { prk,n - signer's private key CRT record and modulus
    AHash - hash algorithm for signature
    hdp   - hash digest
    hlen  - length of hash digest
    smp   - signature message
    smax  - max length of signature buffer
    slen  - length of signature in bytes}
  var
    k: word;
    m: mp_int;
    buf: pointer;
  begin
  {Get size of modulus n in bytes}
  k := mp_unsigned_bin_size(n);
  if smax<k then
    raise ERtcRSA.Create('mp_pkcs1v15_sign2: smax < k');
  slen := k;

  {get local memory for encoded message}
  buf := mp_alloc(slen);
  if buf=nil then
    raise ERtcRSA.Create('mp_pkcs1v15_sign2: buf = nil');
  try
    {Apply EMSA-PKCS1-v1_5 encoding: hash digest -> encoded message EM in buf}
    if not mp_pkcs1v15_emsa_encode(AHash, hdp, buf, hlen, k) then
      raise ERtcRSA.Create('mp_pkcs1v15_sign2: encode error')
    else
      begin
      {initialize signature mp_int representative}
      mp_init(m);
      try
        {Convert the encoded message EM to mp_int representative m}
        mp_os2ip(buf, k, m);
        {Apply the RSA signature primitive to the message representative m}
        mp_rsadp2(m, prk, m);
        {Convert the signature representative to octet string}
        mp_i2osp(m, smp, slen);
      finally
        mp_clear(m);
        end;
      end;
  finally
    fillchar(buf^,slen,0);
    mp_freemem(buf, slen);
    end;
  end;

{ TRtcISAAC }

constructor TRtcISAAC.Create(randomized:boolean=False);
  begin
  inherited Create;
  if randomized then
    RandomizeMax
  else
    Randomize();
  end;

destructor TRtcISAAC.Destroy;
  begin
  inherited;
  end;

procedure TRtcISAAC.Randomize(useSeed:Longint=0; useSelf:boolean=False; useTime:boolean=False; useRND:boolean=False);
  begin
  if useTime then useSeed:=useSeed xor Longint(Get_TickTime);
  if useSelf then useSeed:=useSeed xor Longint(RtcIntPtr(self));
  if useRND then
    isaac_init0(ctx,useSeed)
  else
    isaac_init(ctx,useSeed);
  end;

procedure TRtcISAAC.RandomizeMax;
  begin
  Randomize(RandomInt*RandomInt,True,True,True);
  end;

function TRtcISAAC.RandomByte: Byte;
  begin
  Result:=mp_random_byte(ctx);
  end;

function TRtcISAAC.RandomWord: Word;
  begin
  Result:=mp_random_word(ctx);
  end;

function TRtcISAAC.RandomDigit: mp_digit;
  begin
  Result:=mp_random_digit(ctx);
  end;

function TRtcISAAC.Random(max: mp_digit): mp_digit;
  begin
  if max>0 then
    Result:=mp_random_digit(ctx) mod max
  else
    Result:=mp_random_digit(ctx);
  end;

function TRtcISAAC.RandomInt: Longint;
  var
    x:Cardinal;
    i:Longint;
    b,z:byte;
  begin
  z:=mp_random_byte(ctx) and 7;
  x:=mp_random_digit(ctx) mod (32767*7);

  Inc(x,(x xor Get_TickTime) mod (32767*5));
  z:=z xor ((x shr 7) and 7);

  for i := 0 to 11+(x mod 55) do
    begin
    b:=mp_random_byte(ctx);
    if b and 7 <> z then
      Inc(x,b)
    else
      x:=(x*mp_random_byte(ctx)) and $FFFF;
    end;

  Inc(x,(x xor Cardinal(RtcIntPtr(self))) mod 32767);
  z:=z xor ((x shr 3) and 7);

  for i := 0 to 17+(x mod 143) do
    begin
    b:=mp_random_byte(ctx);
    if b and 7 <> z then
      Inc(x,b)
    else
      x:=(x*mp_random_byte(ctx)) and $FFFF;
    end;

  Inc(x,(x xor mp_random_digit(ctx)) mod (32767*3));

  x:=x mod 32767;

  Result:=X;
  end;

function TRtcISAAC.RND(size: Longint): RtcByteArray;
  var
    i:Longint;
  begin
  SetLength(Result,size);
  for i:=0 to size-1 do
    Result[i]:=mp_random_byte(ctx);
  end;

{ Grow a new Array of Random Bytes using these parameters:
   Data = Random array of bytes to use as Source
   Seed = Seed to be used for Random Growth generation
   Size = Size to which the array should grow
   Growth = Growth control }
function TRtcISAAC.GrowFromSeed(const FromData:RtcByteArray; Seed, Size:Longint; Growth:byte):RtcByteArray;
  var
    ran:isaac_ctx;
    i,j,k:Longint;
    gr:RtcByteArray;
    b:word;
    total:Longint;
    Data:RtcByteArray;
  begin
  isaac_init(ran,Seed);

  if length(FromData)=0 then
    begin
    SetLength(Data,1);
    Data[0]:=0;
    end
  else
    Data:=FromData;

  repeat
    SetLength(gr,length(Data));
    FillChar(gr[0],length(gr),0);

    total:=length(Data);
    if Growth>0 then
      begin
      if Growth<255 then Inc(Growth);
      for i:=0 to length(gr)-1 do
        begin
        b:=mp_random_word(ran) mod Growth;
        if total<Size then
          begin
          if total+b>Size then b:=Size-total;
          gr[i]:=b;
          Inc(total,b);
          end;
        end;
      end;
    k:=0;

    SetLength(Result,total);
    for i:=0 to length(gr)-1 do
      begin
      b:=mp_random_byte(ran);
      Result[k]:=Data[i] xor b; Inc(k);
      for j:=1 to gr[i] do
        begin
        b:=mp_random_byte(ran);
        Result[k]:=Data[i] xor b; Inc(k);
        end;
      end;

    SetLength(gr,0);
    if (Growth>0) and (total<Size) then
      begin
      Data:=Result;
      SetLength(Result,0);
      end;
    until (Growth=0) or (total>=Size);
  SetLength(Data,0);
  end;

{ TRtcRSA }

constructor TRtcRSA.Create;
  begin
  inherited Create(True);
  mp_init(pE);
  mp_init(pN);
  mp_rsa_init_private(pKey);
  end;

destructor TRtcRSA.Destroy;
  begin
  mp_clear(pE);
  mp_clear(pN);
  mp_rsa_clear_private(pKey);
  inherited;
  end;

procedure TRtcRSA.GenerateKeyPair(Octets: Longint);
  begin
  if Octets<16 then Octets:=16
  else if Octets>2048 then Octets:=2048;
  SetLength(PrivateArr,0);
  SetLength(PublicArr,0);

  mp_set_int(pE,RandomInt);
  if mp_isodd(pE) then mp_inc(pE);
  mp_inc_int(pE,32767);

  mp_rsa_keygen2(ctx,pE,octets,pN,pKey);
  end;

function TRtcRSA.BlockSize: Longint;
  begin
  Result:=mp_pkcs1v15_maxlen(pN);
  end;

function TRtcRSA.KeyBits: Longint;
  begin
  Result:=mp_bitsize(pN);
  end;

function TRtcRSA.GetPrivateKey: RtcByteArray;
  var
    l,i,k:Longint;
  begin
  if length(PrivateArr)=0 then
    begin
    l:=0; k:=0;
    Inc(l,2+(mp_bitsize(pN)+7) div 8);
    Inc(l,2+(mp_bitsize(pKey.p)+7) div 8);
    Inc(l,2+(mp_bitsize(pKey.q)+7) div 8);
    Inc(l,2+(mp_bitsize(pKey.dp)+7) div 8);
    Inc(l,2+(mp_bitsize(pKey.dq)+7) div 8);
    Inc(l,2+(mp_bitsize(pKey.qinv)+7) div 8);
    Inc(l,2+(mp_bitsize(pE)+7) div 8);
    SetLength(Result,l);

    i:=(mp_bitsize(pN)+7) div 8;
    Result[k]:=i shr 8; Inc(k);
    Result[k]:=i and $FF; Inc(k);
    mp_i2osp(pN,@Result[k],i); Inc(k,i);

    i:=(mp_bitsize(pKey.p)+7) div 8;
    Result[k]:=i shr 8; Inc(k);
    Result[k]:=i and $FF; Inc(k);
    mp_i2osp(pKey.p,@Result[k],i); Inc(k,i);

    i:=(mp_bitsize(pKey.q)+7) div 8;
    Result[k]:=i shr 8; Inc(k);
    Result[k]:=i and $FF; Inc(k);
    mp_i2osp(pKey.q,@Result[k],i); Inc(k,i);

    i:=(mp_bitsize(pKey.dp)+7) div 8;
    Result[k]:=i shr 8; Inc(k);
    Result[k]:=i and $FF; Inc(k);
    mp_i2osp(pKey.dp,@Result[k],i); Inc(k,i);

    i:=(mp_bitsize(pKey.dq)+7) div 8;
    Result[k]:=i shr 8; Inc(k);
    Result[k]:=i and $FF; Inc(k);
    mp_i2osp(pKey.dq,@Result[k],i); Inc(k,i);

    i:=(mp_bitsize(pKey.qinv)+7) div 8;
    Result[k]:=i shr 8; Inc(k);
    Result[k]:=i and $FF; Inc(k);
    mp_i2osp(pKey.qinv,@Result[k],i); Inc(k,i);

    i:=(mp_bitsize(pE)+7) div 8;
    Result[k]:=i shr 8; Inc(k);
    Result[k]:=i and $FF; Inc(k);
    mp_i2osp(pE,@Result[k],i);

    PrivateArr:=Result;
    end
  else
    Result:=PrivateArr;
  end;

function TRtcRSA.GetPublicKey: RtcByteArray;
  var
    l,i,k:Longint;
  begin
  if length(PublicArr)=0 then
    begin
    l:=0; k:=0;
    Inc(l,2+(mp_bitsize(pN)+7) div 8);
    Inc(l,2+(mp_bitsize(pE)+7) div 8);
    SetLength(Result,l);

    i:=(mp_bitsize(pN)+7) div 8;
    Result[k]:=i shr 8; Inc(k);
    Result[k]:=i and $FF; Inc(k);
    mp_i2osp(pN,@Result[k],i); Inc(k,i);

    i:=(mp_bitsize(pE)+7) div 8;
    Result[k]:=i shr 8; Inc(k);
    Result[k]:=i and $FF; Inc(k);
    mp_i2osp(pE,@Result[k],i);

    PublicArr:=Result;
    end
  else
    Result:=PublicArr;
  end;

procedure TRtcRSA.SetPrivateKey(const Value: RtcByteArray);
  var
    l,i,k:Longint;
  begin
  l:=length(Value);
  if l<3*7 then
    raise ERtcRSA.Create('SetPrivateKey: Not a Private Key record');

  k:=0;
  SetLength(PublicArr,0);
  SetLength(PrivateArr,0);

  if k+2>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (N1)');
  i:=Value[k] shl 8 + Value[k+1]; Inc(k,2);
  if k+i>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (N2)');
  mp_os2ip(@Value[k],i,pN); Inc(k,i);

  if k+2>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (P1)');
  i:=Value[k] shl 8 + Value[k+1]; Inc(k,2);
  if k+i>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (P2)');
  mp_os2ip(@Value[k],i,pKey.p); Inc(k,i);

  if k+2>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (Q1)');
  i:=Value[k] shl 8 + Value[k+1]; Inc(k,2);
  if k+i>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (Q2)');
  mp_os2ip(@Value[k],i,pKey.q); Inc(k,i);

  if k+2>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (dP1)');
  i:=Value[k] shl 8 + Value[k+1]; Inc(k,2);
  if k+i>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (dP2)');
  mp_os2ip(@Value[k],i,pKey.dp); Inc(k,i);

  if k+2>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (dQ1)');
  i:=Value[k] shl 8 + Value[k+1]; Inc(k,2);
  if k+i>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (dQ2)');
  mp_os2ip(@Value[k],i,pKey.dq); Inc(k,i);

  if k+2>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (Qinv1)');
  i:=Value[k] shl 8 + Value[k+1]; Inc(k,2);
  if k+i>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (Qinv2)');
  mp_os2ip(@Value[k],i,pKey.qinv);  Inc(k,i);

  if k+2>=l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (E1)');
  i:=Value[k] shl 8 + Value[k+1]; Inc(k,2);
  if k+i>l then raise ERtcRSA.Create('SetPrivateKey: Insufficient data (E2)');
  mp_os2ip(@Value[k],i,pE);
  end;

procedure TRtcRSA.SetPublicKey(const Value: RtcByteArray);
  var
    l,i,k:Longint;
  begin
  l:=length(Value);
  if l<3*2 then
    raise ERtcRSA.Create('SetPublicKey: Not a Public Key record');

  k:=0;
  SetLength(PublicArr,0);
  SetLength(PrivateArr,0);

  if k+2>=l then raise ERtcRSA.Create('SetPublicKey: Insufficient data (N1)');
  i:=Value[k] shl 8 + Value[k+1]; Inc(k,2);
  if k+i>=l then raise ERtcRSA.Create('SetPublicKey: Insufficient data (N2)');
  mp_os2ip(@Value[k],i,pN); Inc(k,i);

  if k+2>=l then raise ERtcRSA.Create('SetPublicKey: Insufficient data (E1)');
  i:=Value[k] shl 8 + Value[k+1]; Inc(k,2);
  if k+i>l then raise ERtcRSA.Create('SetPublicKey: Insufficient data (E2)');
  mp_os2ip(@Value[k],i,pE);
  end;

function TRtcRSA.Encrypt(const Data: RtcByteArray): RtcByteArray;
  var
    clen:word;
    cnt,plen,klen,tolen,flen:Longint;
  begin
  if length(Data)=0 then
    begin
    SetLength(Result,0);
    Exit;
    end;
  plen:=BlockSize;
  klen:=mp_unsigned_bin_size(pN);
  cnt:=(length(Data)+plen-1) div plen;
  SetLength(Result, cnt * klen);
  flen:=0; tolen:=0;
  repeat
    if cnt=1 then
      plen:=(length(Data)-1) mod BlockSize + 1;
    mp_pkcs1v15_encrypt(ctx,pE,pN, @Data[flen],@Result[tolen], plen,klen, clen);
    Inc(flen,plen);
    Inc(tolen,clen);
    Dec(cnt);
    until cnt<=0;
  if tolen<>length(Result) then
    SetLength(Result,tolen);
  end;

function TRtcRSA.Decrypt(const Data: RtcByteArray): RtcByteArray;
  var
    plen:word;
    clen,cnt,flen,tolen:Longint;
  begin
  if length(Data)=0 then
    begin
    SetLength(Result,0);
    Exit;
    end;
  SetLength(Result,length(Data));
  clen:=mp_unsigned_bin_size(pN);
  cnt:=length(Data) div clen;
  tolen:=0;flen:=0;
  repeat
    mp_pkcs1v15_decrypt2(pKey,pN,@Data[flen],@Result[tolen],clen,clen,plen);
    Inc(tolen,plen);
    Inc(flen,clen);
    Dec(cnt);
    until cnt<=0;
  if tolen<>length(Result) then
    SetLength(Result,tolen);
  end;

function TRtcRSA.SignHash(HashType:RtcHashType; const PlainHash: RtcByteArray): RtcByteArray;
  var
    plen,klen:Longint;
    clen:word;
  begin
  if length(PlainHash)=0 then
    begin
    SetLength(Result,0);
    Exit;
    end;
  klen:=mp_unsigned_bin_size(pN);
  plen:=length(PlainHash);
  SetLength(Result, klen);
  mp_pkcs1v15_sign2(pKey,pN, HashType, @PlainHash[0],@Result[0], plen,klen, clen);
  if clen<>length(Result) then
    SetLength(Result,clen);
  end;

function TRtcRSA.VerifyHash(HashType:RtcHashType; const PlainHash,SignedHash: RtcByteArray): boolean;
  var
    plen,clen:word;
  begin
  clen:=length(PlainHash);
  plen:=length(SignedHash);
  if (clen>0) and (plen>0) then
    Result:=mp_pkcs1v15_verify(pE,pN,HashType,@PlainHash[0],@SignedHash[0],clen,plen)
  else
    Result:=False;
  end;

{ TRtcISAACrypt }

constructor TRtcISAACrypt.Create;
  begin
  inherited;
  SetLength(FCryptKey,0);
  FSeed:=0;
  CInit:=False;
  end;

destructor TRtcISAACrypt.Destroy;
  begin
  SetLength(FCryptKey,0);
  FSeed:=0;
  inherited;
  end;

procedure TRtcISAACrypt.Init;
  var
    a:Longint;
  begin
  if CInit then Exit;

  isaac_init(ctx,FSeed);

  CValue:=0;
  CLen:=length(FCryptKey);

  if CLen>0 then
    begin
    // First code = sum of all crypt bytes
    for a:=0 to CLen-1 do
      Inc(CValue,FCryptKey[a]);

    if CValue>$FFFF then
      CValue:=(CValue and $FFFF)+(CValue shr 16);
    CErr:=CValue+CLen;

    CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
    if CCode=0 then
      begin
      Inc(CValue,CErr);
      CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
      end;
    CPos:=0;

    CInit:=True;
    end;
  end;

procedure TRtcISAACrypt.Crypt(var s: RtcByteArray);
  var
    a:Longint;
    c,c2:byte;
  begin
  CInit:=False;
  if CLen>0 then
    begin
    for a:=0 to length(s)-1 do
      begin
      c2:=s[a];
      c:=c2 xor CCode xor mp_random_byte(ctx); // Crypt this character

      CValue:=CValue * (1+(c2 and $F)) + (c2 and $F0); // include original character into the code

      if CPos>=CLen then
        CPos:=1
      else
        Inc(CPos);

      Inc(CValue, FCryptKey[CPos-1]);
      if CValue>$FFFF then
        CValue:=(CValue and $FFFF)+(CValue shr 16);

      CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
      if CCode=0 then
        begin
        Inc(CValue,CErr);
        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        end;
      s[a]:=c;
      end;
    end;
  end;

procedure TRtcISAACrypt.DeCrypt(var s: RtcByteArray);
  var
    a:Longint;
    c:byte;
  begin
  CInit:=False;
  if CLen>0 then
    begin
    for a:=0 to length(s)-1 do
      begin
      c:=s[a] xor mp_random_byte(ctx) xor CCode; // De-Crypt this character

      CValue:=CValue * (1+(c and $F)) + (c and $F0); // include original character into the code

      if CPos>=CLen then
        CPos:=1
      else
        Inc(CPos);

      Inc(CValue, FCryptKey[CPos-1]);
      if CValue>$FFFF then
        CValue:=(CValue and $FFFF)+(CValue shr 16);

      CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
      if CCode=0 then
        begin
        Inc(CValue,CErr);
        CCode:=((CValue shr 8) and $FF) xor (CValue and $FF);
        end;
      s[a]:=c;
      end;
    end;
  end;

procedure TRtcISAACrypt.SetCryptKey(const Value: RtcByteArray);
  var
    i:Longint;
    b:Longint;
  begin
  FCryptKey := Copy(Value,0,length(Value));
  FSeed:=0;
  for i:=0 to length(Value)-1 do
    begin
    b:=FCryptKey[i];
    case (i and $3) of
      $1: b:=b shl 8;
      $2: b:=b shl 16;
      $3: b:=b shl 24;
      end;
    FSeed:=FSeed xor b;
    end;
  CInit:=False;
  Init;
  end;

{$ENDIF} // {$IFNDEF RTC_NORSA}

end.
