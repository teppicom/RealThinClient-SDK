{*****************************************************************************
*  This is a modified version of "ZLibEx.pas" from base2 technologies        *
*                                                                            *
*  Copyright (c) 2004-2019 Teppi Technology (https://rtc.teppi.net)          *
*  copyright (c) 2000-2005 base2 technologies                                *
*  copyright (c) 1997 Borland International                                  *
*                                                                            *
*  acknowledgements                                                          *
*    erik turner                                                             *
*      Z*Stream routines                                                     *
*                                                                            *
*    burak kalayci                                                           *
*      informing me about the zlib 1.1.4 update and the 1.2.1 update         *
*                                                                            *
*    vicente sánchez-alarcos                                                 *
*      informing me about the zlib 1.2.2 update                              *
*                                                                            *
*    luigi sandon                                                            *
*      pointing out the missing loop condition (Z_STREAM_END) in             *
*        ZInternalCompressStream and ZInternalDecompressStream               *
*                                                                            *
*    ferry van genderen                                                      *
*      assiting me fine tune and beta test ZInternalCompressStream and       *
*        ZInternalDecompressStream                                           *
*                                                                            *
*    mathijs van veluw                                                       *
*      informing me about the zlib 1.2.3 update                              *
*****************************************************************************
@exclude
}
unit rtcZLib;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,
  
  rtcTypes;

type
  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

{$IFDEF RTC_BYTESTRING}
function ZCompress_Str(const inBuffer: RtcString; level: TZCompressionLevel):RtcString; overload; deprecated;

function ZDecompress_Str(const inBuffer: RtcString; inSize:integer=0):RtcString; overload; deprecated;
{$ENDIF}

{ inBuffer = buffer to compress
  level = compression level
  inSize = number of bytes to compress from inBuffer (0=all) }
function ZCompress_Ex(const inBuffer: RtcByteArray; level: TZCompressionLevel; inSize:integer=0):RtcByteArray; overload;

{ inBuffer = buffer to decompress
  inSize = number of bytes to decompress from inBuffer (0=all) }
function ZDecompress_Ex(const inBuffer: RtcByteArray; inSize:integer=0):RtcByteArray; overload;

implementation

{$IFNDEF COMPRESS}
  {$MESSAGE ERROR 'COMPRESS compiler directive needs to be declared to use ZLib!'}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF MACOSX}
    // {$UNDEF RTC_USEZLIB}
  {$ELSE}{$IFDEF DARWIN}
    // {$UNDEF RTC_USEZLIB}
  {$ELSE}
    {$DEFINE RTC_USEZLIB}
  {$ENDIF}{$ENDIF}
{$ENDIF}

{$IFDEF NEXTGEN} 
  {$DEFINE RTC_USEZLIB} 
{$ELSE}
  {$IFDEF IDE_XE4up}
    {$IFDEF IOS}
      {$DEFINE DARWIN}
    {$ENDIF}
    {$IFDEF ANDROID}
      {$DEFINE RTC_USEZLIB}
    {$ENDIF}
    {$IFDEF LINUX}
      {$DEFINE RTC_USEZLIB}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF RTC_USEZLIB}
  {$IFDEF FPC}
    {$DEFINE FPC_USEZLIB}
  {$ELSE}
    {$DEFINE DCC_USEZLIB}
  {$ENDIF}
{$ENDIF}

{$IFDEF DCC_USEZLIB}
uses ZLib;

function ZCompress_Ex(const inBuffer: RtcByteArray; level: TZCompressionLevel; inSize:integer=0):RtcByteArray;
{$IFDEF IDE_XE4up}
  begin
  if (inSize<=0) or (inSize=length(inBuffer)) then
    ZCompress(TArray<Byte>(inBuffer), TArray<Byte>(Result), ZLib.TZCompressionLevel(level))
  else
    ZCompress(TArray<Byte>(Copy(inBuffer,0,inSize)), TArray<Byte>(Result), ZLib.TZCompressionLevel(level))
  end;
{$ELSE}
  var
    outsize:Integer;
    outBuffer:Pointer;
  begin
  outBuffer:=nil;
  if inSize<=0 then
  {$IFDEF IDE_2009up}
    ZCompress(Addr(inBuffer[0]), length(inBuffer),
              outBuffer, outSize, ZLib.TZCompressionLevel(level))
  {$ELSE}
    CompressBuf(Addr(inBuffer[0]), length(inBuffer),
                outBuffer, outSize)
  {$ENDIF}
  else
  {$IFDEF IDE_2009up}
    ZCompress(Addr(inBuffer[0]), inSize,
              outBuffer, outSize, ZLib.TZCompressionLevel(level));
  {$ELSE}
    CompressBuf(Addr(inBuffer[0]), inSize,
                outBuffer, outSize);
  {$ENDIF}
  SetLength(Result,outSize);
  Move(outBuffer^,Result[0],outSize);
  FreeMem(outBuffer);
  end;
{$ENDIF}

function ZDecompress_Ex(const inBuffer: RtcByteArray; inSize:integer=0):RtcByteArray;
{$IFDEF IDE_XE4up}
  begin
  if (inSize<=0) or (inSize=length(inBuffer)) then
    ZDecompress(TArray<Byte>(inBuffer), TArray<Byte>(Result))
  else
    ZDecompress(TArray<Byte>(Copy(inBuffer,0,inSize)), TArray<Byte>(Result));
  end;
{$ELSE}
  var
    outBuffer:Pointer;
    outSize:Integer;
  begin
  outBuffer:=nil;
  if inSize<=0 then
  {$IFDEF IDE_2009up}
    ZDecompress(Addr(inBuffer[0]), length(inBuffer),
                outBuffer, outSize)
  {$ELSE}
    DecompressBuf(Addr(inBuffer[0]), length(inBuffer), 0,
                outBuffer, outSize)
  {$ENDIF}
  else
  {$IFDEF IDE_2009up}
    ZDecompress(Addr(inBuffer[0]), inSize,
                outBuffer, outSize);
  {$ELSE}
    DecompressBuf(Addr(inBuffer[0]), inSize, 0,
                outBuffer, outSize);
  {$ENDIF}
  SetLength(Result,outSize);
  Move(outBuffer^,Result[0],outSize);
  FreeMem(outBuffer);
  end;
{$ENDIF}

{$IFDEF RTC_BYTESTRING}
function ZCompress_Str(const inBuffer: RtcString; level: TZCompressionLevel):RtcString;
  var
    outsize:Integer;
    outBuffer:Pointer;
  begin
  outBuffer:=nil;
{$IFDEF IDE_2009up}
  ZCompress(Addr(inBuffer[1]), length(inBuffer),
            outBuffer, outSize, ZLib.TZCompressionLevel(level));
{$ELSE}
  CompressBuf(Addr(inBuffer[1]), length(inBuffer),
              outBuffer, outSize);
{$ENDIF}
  SetLength(Result,outSize);
  Move(outBuffer^,Result[1],outSize);
  FreeMem(outBuffer);
  end;

function ZDecompress_Str(const inBuffer: RtcString; inSize:integer=0):RtcString;
  var
    outBuffer:Pointer;
    outSize:Integer;
  begin
  outBuffer:=nil;
  if inSize<=0 then
  {$IFDEF IDE_2009up}
    ZDecompress(Addr(inBuffer[1]), length(inBuffer),
                outBuffer, outSize)
  {$ELSE}
    DecompressBuf(Addr(inBuffer[1]), length(inBuffer), 0,
                outBuffer, outSize)
  {$ENDIF}
  else
  {$IFDEF IDE_2009up}
    ZDecompress(Addr(inBuffer[1]), inSize,
                outBuffer, outSize);
  {$ELSE}
    DecompressBuf(Addr(inBuffer[1]), inSize, 0,
                outBuffer, outSize);
  {$ENDIF}
  SetLength(Result,outSize);
  Move(outBuffer^,Result[1],outSize);
  FreeMem(outBuffer);
  end;
{$ENDIF}

{$ELSE} // !DCC_USEZLIB

{$IFDEF FPC_USEZLIB}
uses zbase, paszlib;

const
  _z_errmsg: array [0..9] of String = (
    'need dictionary',      // Z_NEED_DICT      (2)  //do not localize
    'stream end',           // Z_STREAM_END     (1)  //do not localize
    '',                     // Z_OK             (0)  //do not localize
    'file error',           // Z_ERRNO          (-1) //do not localize
    'stream error',         // Z_STREAM_ERROR   (-2) //do not localize
    'data error',           // Z_DATA_ERROR     (-3) //do not localize
    'insufficient memory',  // Z_MEM_ERROR      (-4) //do not localize
    'buffer error',         // Z_BUF_ERROR      (-5) //do not localize
    'incompatible version', // Z_VERSION_ERROR  (-6) //do not localize
    ''                                               //do not localize
    );
  ZLevels: array[TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_DEFAULT_COMPRESSION,
    Z_BEST_COMPRESSION
    );
    
{$ELSE} // !FPC_USEZLIB

{$IFDEF MACOSX}
  {$DEFINE RTC_EXTERNAL}
  {$IFDEF FPC} uses dynlibs; {$ENDIF}
  const
    libzlib = '/usr/lib/libz.dylib';
{$ELSE} // !MACOSX
  {$IFDEF DARWIN}
    {$DEFINE RTC_EXTERNAL}
    {$IFDEF FPC} uses dynlibs; {$ENDIF}
    const
      libzlib = '/usr/lib/libz.dylib';
  {$ELSE} // !DARWIN
    {$IFDEF CPUX32} // 32-bit version
      {$L zlib\deflate.obj}
      {$L zlib\inflate.obj}
      {$L zlib\infback.obj}
      {$L zlib\inffast.obj}
      {$L zlib\inftrees.obj}
      {$L zlib\trees.obj}
      {$L zlib\compress.obj}
      {$L zlib\adler32.obj}
      {$L zlib\crc32.obj}
    {$ELSE} // !CPUX32 
      {$IFDEF CPUX64} // 64-bit version
        {$L zlib\x64\deflate.obj}
        {$L zlib\x64\inflate.obj}
        {$L zlib\x64\infback.obj}
        {$L zlib\x64\inffast.obj}
        {$L zlib\x64\inftrees.obj}
        {$L zlib\x64\trees.obj}
        {$L zlib\x64\compress.obj}
        {$L zlib\x64\adler32.obj}
        {$L zlib\x64\crc32.obj}
      {$ELSE} // !CPUX64
        {$message error 'rtcZLib: Unsupported platform'}
      {$ENDIF} // CPUX64
    {$ENDIF} // CPUX32
  {$ENDIF} // DARWIN
{$ENDIF} // MACOSX

const
  ZLIB_VERSION:array[1..6] of Byte = (Byte('1'),Byte('.'),Byte('2'),Byte('.'),Byte('5'),0);
  ZLIB_VERNUM  = $1250;

type
  alloc_func = function(opaque: Pointer; Items, Size: Integer): Pointer;
  free_func = procedure(opaque, address: Pointer);
  PByte = ^Byte;

{$IFDEF CPUX32}
  z_stream = packed record
    next_in  : PByte;     // next input byte
    avail_in : Longint;   // number of bytes available at next_in
    total_in : Longint;   // total nb of input bytes read so far

    next_out : PByte;     // next output byte should be put here
    avail_out: Longint;   // remaining free space at next_out
    total_out: Longint;   // total nb of bytes output so far

    msg      : PByte;     // last error message, NULL if no error
    state    : Pointer;   // not visible by applications

    zalloc   : alloc_func;   // used to allocate the internal state
    zfree    : free_func;    // used to free the internal state
    opaque   : Pointer;   // private data object passed to zalloc and zfree

    data_type: Integer;   // best guess about the data type: ascii or binary
    adler    : Longint;   // adler32 value of the uncompressed data
    reserved : Longint;   // reserved for future use
  end;
{$ELSE}{$IFDEF RTC_EXTERNAL}
  z_stream = packed record
    next_in  : PByte;     // next input byte
    avail_in : Longint;   // number of bytes available at next_in
    total_in : Longint;   // total nb of input bytes read so far

    next_out : PByte;     // next output byte should be put here
    avail_out: Longint;   // remaining free space at next_out
    total_out: Longint;   // total nb of bytes output so far

    msg      : PByte;     // last error message, NULL if no error
    state    : Pointer;   // not visible by applications

    zalloc   : alloc_func;   // used to allocate the internal state
    zfree    : free_func;    // used to free the internal state
    opaque   : Pointer;   // private data object passed to zalloc and zfree

    data_type: Integer;   // best guess about the data type: ascii or binary
    adler    : Longint;   // adler32 value of the uncompressed data
    reserved : Longint;   // reserved for future use
  end;
{$ELSE}{$IFDEF CPUX64}
  internal_state = record end; {Dummy record}
  Pinternal_state = ^internal_state;

  z_stream = record
    next_in: PByte;      // next input byte
    avail_in: Cardinal;  // number of bytes available at next_in
    total_in: Cardinal;  // total nb of input bytes read so far
    next_out: PByte;     // next output byte should be put here
    avail_out: Cardinal; // remaining free space at next_out
    total_out: Cardinal; // total nb of bytes output so far
    msg: PByte;      // last error message, NULL if no error
    state: Pinternal_state; // not visible by applications
    zalloc: alloc_func;  // used to allocate the internal state
    zfree: free_func;    // used to free the internal state
    opaque: Pointer;     // private data object passed to zalloc and zfree
    data_type: Integer;  // best guess about the data type: ascii or binary
    adler: Cardinal;     // adler32 value of the uncompressed data
    reserved: Cardinal;  // reserved for future use
  end;
{$ELSE}
  {$message error 'rtcZLib: Unsupported platform'}
{$ENDIF}{$ENDIF}{$ENDIF}

  z_stream_s = z_stream;
  z_streamp = ^z_stream;

const
  Z_NO_FLUSH       = 0;
  Z_PARTIAL_FLUSH  = 1;
  Z_SYNC_FLUSH     = 2;
  Z_FULL_FLUSH     = 3;
  Z_FINISH         = 4;
  Z_BLOCK          = 5;
  Z_TREES          = 6;
  // Allowed flush values; see deflate() and inflate() below for details

  Z_OK             = 0;
  Z_STREAM_END     = 1;
  Z_NEED_DICT      = 2;
  Z_ERRNO          = (-1);
  Z_STREAM_ERROR   = (-2);
  Z_DATA_ERROR     = (-3);
  Z_MEM_ERROR      = (-4);
  Z_BUF_ERROR      = (-5);
  Z_VERSION_ERROR  = (-6);
  // Return codes for the compression/decompression functions. Negative values
  // are errors, positive values are used for special but normal events.

  Z_NO_COMPRESSION          = 0;
  Z_BEST_SPEED              = 1;
  Z_BEST_COMPRESSION        = 9;
  Z_DEFAULT_COMPRESSION     = (-1);
  // compression levels

  Z_FILTERED             = 1;
  Z_HUFFMAN_ONLY         = 2;
  Z_RLE                  = 3;
  Z_FIXED                = 4;
  Z_DEFAULT_STRATEGY     = 0;
  // compression strategy; see deflateInit2() below for details

  Z_BINARY    = 0;
  Z_TEXT      = 1;
  Z_ASCII     = Z_TEXT;  // for compatibility with 1.2.2 and earlier;
  Z_UNKNOWN   = 2;
  // Possible values of the data_type field (though see inflate())

  Z_DEFLATED    = 8;
  // The deflate compression method (the only one supported in this version)

  Z_NULL   = 0;  // for initializing zalloc, zfree, opaque;

{$IFDEF CPUX32}
  _z_errmsg: array [0..9] of String = (
{$ELSE}{$IFDEF RTC_EXTERNAL}
  _z_errmsg: array [0..9] of String = (
{$ELSE}{$IFDEF CPUX64}
  z_errmsg: array [0..9] of String = (
{$ELSE}
  {$message error 'rtcZLib: Unsupported platform'}
{$ENDIF}{$ENDIF}{$ENDIF}
    'need dictionary',      // Z_NEED_DICT      (2)  //do not localize
    'stream end',           // Z_STREAM_END     (1)  //do not localize
    '',                     // Z_OK             (0)  //do not localize
    'file error',           // Z_ERRNO          (-1) //do not localize
    'stream error',         // Z_STREAM_ERROR   (-2) //do not localize
    'data error',           // Z_DATA_ERROR     (-3) //do not localize
    'insufficient memory',  // Z_MEM_ERROR      (-4) //do not localize
    'buffer error',         // Z_BUF_ERROR      (-5) //do not localize
    'incompatible version', // Z_VERSION_ERROR  (-6) //do not localize
    ''                                               //do not localize
    );

  ZLevels: array[TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_DEFAULT_COMPRESSION,
    Z_BEST_COMPRESSION
    );

  SZInvalid = 'Invalid ZStream operation!';


{$IFDEF RTC_EXTERNAL}

type
  TdeflateInit_ = function (var strm: z_stream; level: Integer; version: PByte; stream_size: Integer): Integer; cdecl;
  Tdeflate = function (var strm: z_stream; flush: Integer): Integer; cdecl;
  TdeflateEnd = function (var strm: z_stream): Integer; cdecl;
  TinflateInit_ = function (var strm: z_stream; version: PByte; stream_size: Integer): Integer; cdecl;
  Tinflate = function(var strm: z_stream; flush: Integer): Integer; cdecl;
  TinflateEnd = function (var strm: z_stream): Integer; cdecl;
  // TinflateReset = function (var strm: z_stream): Integer; cdecl;

var
  deflateInit_: TdeflateInit_;
  deflate:      Tdeflate;
  deflateEnd:   TdeflateEnd;
  inflateInit_: TinflateInit_;
  inflate:      Tinflate;
  inflateEnd:   TinflateEnd;
  // inflateReset: TinflateReset;

  LibHandle: THandle = 0;

procedure LoadZLib;
  begin
  LibHandle := LoadLibrary(libzlib);
  if LibHandle <> 0 then
    begin
    deflateInit_ := GetProcAddress(LibHandle, PChar('deflateInit_'));
    deflate      := GetProcAddress(LibHandle, PChar('deflate'));
    deflateEnd   := GetProcAddress(LibHandle, PChar('deflateEnd'));
    inflateInit_ := GetProcAddress(LibHandle, PChar('inflateInit_'));
    inflate      := GetProcAddress(LibHandle, PChar('inflate'));
    inflateEnd   := GetProcAddress(LibHandle, PChar('inflateEnd'));
    // inflateReset := GetProcAddress(LibHandle, PChar('inflateReset'));
    end
  else
    raise Exception.Create('Error loading ZLib Compression Library');
  end;

procedure UnloadZLib;
  begin
  if LibHandle <> 0 then
    begin
    FreeLibrary(LibHandle);
    LibHandle := 0;
    end;
  end;

{$ELSE}{$IFDEF CPUX32}

function deflateInit_(var strm: z_stream; level: Integer;
  version: PByte; recsize: Integer): Integer;
  external;
function deflate(var strm: z_stream; flush: Integer): Integer;
  external;
function deflateEnd(var strm: z_stream): Integer;
  external;

function inflateInit_(var strm: z_stream;
  version: PByte; recsize: Integer): Integer;
  external;
function inflate(var strm: z_stream; flush: Integer): Integer;
  external;
function inflateEnd(var strm: z_stream): Integer;
  external;
// function inflateReset(var strm: z_stream): Integer; external;

{$ELSE}{$IFDEF CPUX64}

function deflateInit_(var strm: z_stream; level: Integer;
  version: PByte; stream_size: Integer): Integer; cdecl;
  external;
function deflate(var strm: z_stream; flush: Integer): Integer; cdecl;
  external;
function deflateEnd(var strm: z_stream): Integer; cdecl;
  external;

function inflateInit_(var strm: z_stream;
  version: PByte; stream_size: Integer): Integer; cdecl;
  external;
function inflate(var strm: z_stream; flush: Integer): Integer; cdecl;
  external;
function inflateEnd(var strm: z_stream): Integer; cdecl;
  external;
// function inflateReset(var strm: z_stream): Integer; cdecl; external;

{$ELSE}

  {$message error 'rtcZLib: Unsupported platform'}

{$ENDIF}{$ENDIF}{$ENDIF}

function deflateInit(var strm: z_stream; level: Integer): Integer;
begin
  Result := deflateInit_(strm, level,
    @ZLIB_VERSION, SizeOf(z_stream));
end;

function inflateInit(var strm: z_stream): Integer;
begin
  Result := inflateInit_(strm,
    @ZLIB_VERSION, SizeOf(z_stream));
end;

{** c function implementations **********************************************}

function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
begin
  GetMem(result,items * size);
end;

procedure zcfree(opaque, block: Pointer);
begin
  FreeMem(block);
end;

{$IFDEF RTC_EXTERNAL}
procedure _memset(p: Pointer; b: Byte; count: Integer); cdecl;
{$ELSE}{$IFDEF CPUX32}
procedure _memset(p: Pointer; b: Byte; count: Integer); cdecl;
{$ELSE}{$IFDEF CPUX64}
procedure memset(p: Pointer; b: Byte; count: Integer); cdecl;
{$ELSE}
  {$message error 'ZLib: Unsupported CPU'}
{$ENDIF}{$ENDIF}{$ENDIF}
begin
  FillChar(p^,count,b);
end;

{$IFDEF RTC_EXTERNAL}
procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
{$ELSE}{$IFDEF CPUX32}
procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
{$ELSE}{$IFDEF CPUX64}
procedure memcpy(dest, source: Pointer; count: Integer); cdecl;
{$ELSE}
  {$message error 'ZLib: Unsupported CPU'}
{$ENDIF}{$ENDIF}{$ENDIF}
begin
  Move(source^,dest^,count);
end;

{$ENDIF} // FPC + ZLIB

{****************************************************************************}

type
  EZLibError = class(Exception)
  public
    class function New(method, code: Integer):EZLibError;
  end;

const
  ZCHK_DEFLATE_INIT = 1;
  ZCHK_DEFLATE = 2;
  ZCHK_DEFLATE_END = 3;

  ZCHK_INFLATE_INIT = 4;
  ZCHK_INFLATE = 5;
  ZCHK_INFLATE_END = 6;

function ZCompressCheck(method: integer; code: Integer): Integer;
begin
  result := code;

  if code < 0 then
  begin
    raise EZLibError.New(method, code);
  end;
end;

function ZDecompressCheck(method:integer; code: Integer): Integer;
begin
  Result := code;

  if code < 0 then
  begin
    raise EZLibError.New(method, code);
  end;
end;

{** RtcByteArray routines *********************************************************}

const
  delta=1024;

function ZCompress_Ex(const inBuffer: RtcByteArray; level: TZCompressionLevel; inSize:integer=0):RtcByteArray;
  var
    SL:array of pointer;
    loc,a,slcount:integer;
    zstream: z_stream;
  begin
  FillChar(zstream, SizeOf(z_stream), 0);

  SetLength(Result,0);

  slcount:=0;
  SetLength(SL,32);
  try
    zstream.next_in := @inBuffer[0];
    if inSize<=0 then
      zstream.avail_in := length(inBuffer)
    else
      zstream.avail_in := inSize;

    Inc(slcount);
    GetMem(SL[slcount-1],delta);

    zstream.next_out := SL[slcount-1];
    zstream.avail_out := delta;

    ZCompressCheck(ZCHK_DEFLATE_INIT, DeflateInit(zstream, ZLevels[level]));

    try
      while ZCompressCheck(ZCHK_DEFLATE, deflate(zstream, Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(slcount);
        if slcount>length(SL) then
          SetLength(SL, length(SL)+32);
        GetMem(SL[slcount-1],delta);

        zstream.next_out := SL[slcount-1];
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(ZCHK_DEFLATE_END, deflateEnd(zstream));
    end;

    loc:=0;
    SetLength(Result, zstream.total_out);
    if slcount>0 then
      begin
      for a:=0 to slcount-2 do
        begin
        Move(SL[a]^,Result[loc],delta);
        Inc(loc,delta);
        end;
      Move(SL[slcount-1]^,Result[loc], length(Result)-loc);
      end;
  finally
    if slcount>0 then
      for a:=0 to slcount-1 do
        begin
        FreeMem(SL[a]);
        SL[a]:=nil;
        end;
    SetLength(SL,0);
    end;
  end;

function ZDecompress_Ex(const inBuffer: RtcByteArray; inSize:integer):RtcByteArray;
  var
    SL:array of pointer;
    loc,a,slcount:integer;
    zstream: z_stream;
  begin
  if inSize<=0 then
    inSize:=length(inBuffer)
  else if inSize>length(inBuffer) then
    raise Exception.Create('Error! Can not decompress more than received.');

  FillChar(zstream, SizeOf(z_stream), 0);

  SetLength(Result,0);

  slcount:=0;
  SetLength(SL, 32);
  try
    zstream.next_in := @inBuffer[0];
    zstream.avail_in := inSize;

    Inc(slcount);
    GetMem(SL[slcount-1],delta);

    zstream.next_out := SL[slcount-1];
    zstream.avail_out := delta;

    ZDecompressCheck(ZCHK_INFLATE_INIT, InflateInit(zstream));

    try
      while ZDecompressCheck(ZCHK_INFLATE, inflate(zstream, Z_NO_FLUSH)) <> Z_STREAM_END do
      begin
        Inc(slcount);
        if slcount>length(SL) then
          SetLength(SL, length(SL)+32);
        GetMem(SL[slcount-1],delta);

        zstream.next_out := SL[slcount-1];
        zstream.avail_out := delta;
      end;
    finally
      ZDecompressCheck(ZCHK_INFLATE_END, inflateEnd(zstream));
    end;

    loc:=0;
    SetLength(Result, zstream.total_out);
    if slcount>0 then
      begin
      for a:=0 to slcount-2 do
        begin
        Move(SL[a]^,Result[loc],delta);
        Inc(loc,delta);
        end;
      Move(SL[slcount-1]^,Result[loc], length(Result)-loc);
      end;
  finally
    if slcount>0 then
      for a:=0 to slcount-1 do
        begin
        FreeMem(SL[a]);
        SL[a]:=nil;
        end;
    SetLength(SL,0);
    end;
  end;

{$IFDEF RTC_BYTESTRING}

function ZCompress_Str(const inBuffer: RtcString; level: TZCompressionLevel):RtcString;
  var
    SL:array of pointer;
    loc,a,slcount:integer;
    zstream: z_stream;
  begin
  FillChar(zstream, SizeOf(z_stream), 0);

  SetLength(Result,0);

  slcount:=0;
  SetLength(SL,32);
  try
    zstream.next_in := @inBuffer[1];
    zstream.avail_in := length(inBuffer);

    Inc(slcount);
    GetMem(SL[slcount-1],delta);

    zstream.next_out := SL[slcount-1];
    zstream.avail_out := delta;

    ZCompressCheck(ZCHK_DEFLATE_INIT, DeflateInit(zstream, ZLevels[level]));

    try
      while ZCompressCheck(ZCHK_DEFLATE, deflate(zstream, Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(slcount);
        if slcount>length(SL) then
          SetLength(SL, length(SL)+32);
        GetMem(SL[slcount-1],delta);

        zstream.next_out := SL[slcount-1];
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(ZCHK_DEFLATE_END, deflateEnd(zstream));
    end;

    loc:=1;
    SetLength(Result, zstream.total_out);
    if slcount>0 then
      begin
      for a:=0 to slcount-2 do
        begin
        Move(SL[a]^,Result[loc],delta);
        Inc(loc,delta);
        end;
      Move(SL[slcount-1]^,Result[loc], length(Result)-loc+1);
      end;
  finally
    if slcount>0 then
      for a:=0 to slcount-1 do
        begin
        FreeMem(SL[a]);
        SL[a]:=nil;
        end;
    SetLength(SL,0);
    end;
  end;

function ZDecompress_Str(const inBuffer: RtcString; inSize:integer):RtcString;
  var
    SL:array of pointer;
    loc,a,slcount:integer;
    zstream: z_stream;
  begin
  if inSize<=0 then
    inSize:=length(inBuffer)
  else if inSize>length(inBuffer) then
    raise Exception.Create('Error! Can not decompress more than received.');

  FillChar(zstream, SizeOf(z_stream), 0);

  SetLength(Result,0);

  slcount:=0;
  SetLength(SL, 32);
  try
    zstream.next_in := @inBuffer[1];
    zstream.avail_in := inSize;

    Inc(slcount);
    GetMem(SL[slcount-1],delta);

    zstream.next_out := SL[slcount-1];
    zstream.avail_out := delta;

    ZDecompressCheck(ZCHK_INFLATE_INIT, InflateInit(zstream));

    try
      while ZDecompressCheck(ZCHK_INFLATE, inflate(zstream, Z_NO_FLUSH)) <> Z_STREAM_END do
      begin
        Inc(slcount);
        if slcount>length(SL) then
          SetLength(SL, length(SL)+32);
        GetMem(SL[slcount-1],delta);

        zstream.next_out := SL[slcount-1];
        zstream.avail_out := delta;
      end;
    finally
      ZDecompressCheck(ZCHK_INFLATE_END, inflateEnd(zstream));
    end;

    loc:=1;
    SetLength(Result, zstream.total_out);
    if slcount>0 then
      begin
      for a:=0 to slcount-2 do
        begin
        Move(SL[a]^,Result[loc],delta);
        Inc(loc,delta);
        end;
      Move(SL[slcount-1]^,Result[loc], length(Result)-loc+1);
      end;
  finally
    if slcount>0 then
      for a:=0 to slcount-1 do
        begin
        FreeMem(SL[a]);
        SL[a]:=nil;
        end;
    SetLength(SL,0);
    end;
  end;

{$ENDIF}

{** EZLibError **************************************************************}

class function EZLibError.New(method,code: Integer):EZLibError;
  var
    m:String;
  begin
  case method of
    ZCHK_DEFLATE_INIT: m:='ZCompress_str.DeflateInit';
    ZCHK_DEFLATE:      m:='ZCompress_str.Deflate';
    ZCHK_DEFLATE_END:  m:='ZCompress_str.DeflateEnd';
    ZCHK_INFLATE_INIT: m:='ZDecompress_str.InflateInit';
    ZCHK_INFLATE:      m:='ZDecompress_str.Inflate';
    ZCHK_INFLATE_END:  m:='ZDecompress_str.InflateEnd';
    else m:='Unknown';
    end;
{$IFDEF CPUX32}
  Result:=Create('ZLib exception in '+m+': '+_z_errmsg[2 - code]);
{$ELSE}{$IFDEF RTC_EXTERNAL}
  Result:=Create('ZLib exception in '+m+': '+_z_errmsg[2 - code]);
{$ELSE}{$IFDEF RTC_USEZLIB}
  Result:=Create('ZLib exception in '+m+': '+_z_errmsg[2 - code]);
{$ELSE}{$IFDEF CPUX64}
  Result:=Create('ZLib exception in '+m+': '+z_errmsg[2 - code]);
{$ELSE}
  {$message error 'rtcZLib: Unsupported platform'}
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}
  end;


{$IFDEF RTC_EXTERNAL}
initialization
LoadZLib;
finalization
UnloadZLib;
{$ENDIF}

{$ENDIF} // DCC_USEZLIB
end.
