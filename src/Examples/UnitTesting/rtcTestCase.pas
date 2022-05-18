{
  Basic Testing Case
    - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit rtcTestCase;

{$INCLUDE rtcDefs.inc}

interface


uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF MACOSX}
  Macapi.CoreServices,
{$ENDIF}

  SysUtils;

type
  TRtcTestCase = class
  protected
    st, tc, tc3: longword;
    r:byte;
    LastCall: String;
    LastMsg: String;
    FullPing: boolean;

    procedure Check(ok:boolean; const msg:String);
    procedure Log(const s:String);
    procedure Ping(val:longword=0);

  public
    MaxPing:longword;

    function LastLine: String;

    constructor Create;
    destructor Destroy; override;
  end;

{$IFDEF MACOSX}
function GetTickCount:longword; inline;
{$ENDIF}

implementation

{$IFDEF MACOSX}
function GetTickCount:longword; inline;
  begin
  Result:=AbsoluteToNanoseconds(UpTime) div 1000000;
  end;
{$ENDIF}

constructor TRtcTestCase.Create;
  begin
  MaxPing:=0;
  FullPing:=True;
  tc:=0;
  LastCall:='';
  randomize;
  st:=GetTickCount;
  end;

destructor TRtcTestCase.Destroy;
  begin
  if st>0 then
    Log('Done in '+IntToStr(GetTickCount-st)+' ms');
  Writeln;
  inherited;
  end;

function TRtcTestCase.LastLine: String;
  begin
  Result:=ClassName+':'+LastCall+' '+LastMsg;
  end;

procedure TRtcTestCase.Log(const s: String);
  var
    tc2:longword;
    a:integer;
  begin
  r:=0;
  tc2:=GetTickCount;
  if tc>0 then
    if tc2>tc then
      begin
      Write(#8#8#8#8#8#8#8#8);
      for a:=length(ClassName)+length(LastCall) to 40 do
        Write(' ');
      Writeln('['+IntToStr(tc2-tc)+' ms]');
      end
    else
      Writeln(#8#8#8#8#8#8#8#8'        ');
  LastMsg := '';
  LastCall:=s;
  tc:=tc2;
  tc3:=tc;
  Write(ClassName+': '+LastCall+'         ');
  end;

procedure TRtcTestCase.Check(ok: boolean; const msg: String);
  begin
  LastMsg := msg;
  if not ok then
    begin
    st:=0;
    Writeln;
    Writeln(ClassName+': '+LastCall+' -- Error -> '+msg);
    raise Exception.Create(ClassName+': '+msg);
    end;
  end;

procedure TRtcTestCase.Ping(val:longword=0);
  function Cnt:string;
    begin
    if val=0 then
      Result:='      '
    else
      begin
      if round(val/MaxPing*10000)>9998 then
        Result:=' 100% '
      else
        begin
        Result:=FormatFloat('00.00',val/MaxPing*100);
        while length(Result)<3 do Result:=' '+Result;
        Result:=Result+'%';
        end;
      end;
    end;
  begin
  if FullPing or (val mod 100=0) then
    if GetTickCount-tc3>=250 then
      begin
      Inc(r); if r>3 then r:=0;
      case r of
        0:Write(#8#8#8#8#8#8#8#8'- '+Cnt);
        1:Write(#8#8#8#8#8#8#8#8'\ '+Cnt);
        2:Write(#8#8#8#8#8#8#8#8'| '+Cnt);
        3:Write(#8#8#8#8#8#8#8#8'/ '+Cnt);
        end;
      tc3:=GetTickCount;
      end;
  end;

end.
