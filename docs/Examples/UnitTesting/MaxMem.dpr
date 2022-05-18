program MaxMem;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  a:integer;
  p:array[1..100000] of pointer;

begin
  try
    for a := Low(p) to High(p) do
      p[a]:=nil;
    for a := Low(p) to High(p) do
      begin
      GetMem(p[a],1024*1024);
      Writeln(a,' MB Allocated');
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Writeln('Press any key to release memory ...');
  Readln;
  for a := Low(p) to High(p) do
    if p[a]<>nil then
      begin
      FreeMem(p[a]);
      p[a]:=nil;
      end;
end.
