program Project1;

{$mode delphi}{$H+}

{$minstacksize 2048}
{$maxstacksize 128000}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  // this includes the LCL widgetset
  Interfaces, Forms
  { you can add units after this }
  ,AppClient_Unit;

begin
  Application.Initialize;
  //Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

