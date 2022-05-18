program MaxStrRefs;

{$APPTYPE CONSOLE}

{$IFDEF WIN64}
  {$SETPEFLAGS $27}
{$ENDIF}

uses
  Windows,
  Classes,
  SysUtils;

const
  OverflowStringSize = 256*1024*1024; // 256 million characters
  MyListMax = 65536;

type
  TMyStringType = String; // String type to use
  TMyCharType = Char; // Char type to use

  PMyList = ^TMyList;
  TMyList = array[0 .. MyListMax] of TMyStringType;
var
  s:TMyStringType;

function RandomString(size:integer):TMyStringType;
  var
    I:Integer;
  begin
  SetLength(Result,size);
  for I := 1 to size do
    Result[I]:=TMyCharType(random(255));
  end;

procedure ForceStringRefCntOverflow;
  var
    i,loop:integer;
    cnt:int64;
    MyList:PMyList;
    P,P2:pointer;
  begin
  loop:=1;
  System.Writeln('Creating String ',loop,' ...');
  s:=RandomString(OverflowStringSize);
  cnt:=1;
  try
    GetMem(MyList,MyListMax*SizeOf(TMyStringType));
    GetMem(P2,MyListMax*SizeOf(TMyStringType));
    p:=MyList;
    try
      repeat
        System.Writeln('References added:',cnt);
        ZeroMemory(p,MyListMax*SizeOf(TMyStringType));
        for i := 0 to MyListMax do
          begin
          MyList^[i] := s;
          Inc(cnt);
          if cnt>int64(MaxInt) then // ref counter overflow
            System.Writeln('Overflow count:',cnt-MaxInt);
          end;
        if cnt>int64(MaxInt) * 3 div 2 then // ref counter overflow
          begin
          System.Writeln('Removing references ...');
          Move(P^,P2^,MyListMax*SizeOf(TMyStringType));
          repeat
            Move(P2^,P^,MyListMax*SizeOf(TMyStringType));
            for i := 0 to MyListMax do
              begin
              SetLength(MyList^[i],0);
              Dec(cnt);
              end;
            System.Writeln('References left:',cnt);
            until cnt=1;
          // Remove the last reference
          SetLength(s,0);
          Dec(cnt);
          // Make a new String
          Inc(loop);
          System.Writeln('Creating String ',loop,' ...');
          s:=RandomString(OverflowStringSize);
          cnt:=1;
          end;
        until False;
    finally
      FreeMem(MyList,MyListMax*SizeOf(TMyStringType));
      FreeMem(P2,MyListMax*SizeOf(TMyStringType));
      end;
  except
    on E:Exception do
      if cnt>int64(MaxInt) then
        raise Exception.Create(
                E.ClassName+':'+E.Message+
                #13#10+'Overflow count: '+IntToStr(cnt-MaxInt))
      else
        raise Exception.Create(
                E.ClassName+':'+E.Message+
                #13#10+'Refrences left: '+IntToStr(cnt));
    end;
  end;

begin
try
    ForceStringRefCntOverflow;
except
    on E:Exception do
      Writeln(E.ClassName+':'+E.Message);
  end;
Readln;
end.
