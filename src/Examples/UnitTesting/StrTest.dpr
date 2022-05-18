program StrTest;

{$APPTYPE CONSOLE}

uses
  SysUtils, rtcTypes;

var
    s1,s2:RtcString;
    x1,x2:String;

begin
  try
    s1:='A';  x1:='A';
    s2:='AB'; x2:='AB';
    Writeln(s1,':',s2,'? =',s1=s2,' <',s1<s2,' >',s1>s2,' >=',s1>=s2,' <=',s1<=s2,' <>',s1<>s2);
    Writeln(x1,':',x2,'? =',x1=x2,' <',x1<x2,' >',x1>x2,' >=',x1>=x2,' <=',x1<=x2,' <>',x1<>x2);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
Readln;
end.
