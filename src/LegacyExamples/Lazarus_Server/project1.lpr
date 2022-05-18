program project1;

{$mode objfpc}{$H+}

uses
//  {$ifdef unix}
//  chtreads,
//  {$endif}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Unit1, rtcSDK_FPC;

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

