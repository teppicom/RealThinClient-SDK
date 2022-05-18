program AppServer;

{$mode delphi}{$H+}
//{$minstacksize 2048}
//{$maxstacksize 128000}

uses
  {$ifdef unix}
  cthreads,
  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, AppServerModule, Server_Form;

begin
  Application.Initialize;
  Application.CreateForm(TAppSrv_Module, AppSrv_Module);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
