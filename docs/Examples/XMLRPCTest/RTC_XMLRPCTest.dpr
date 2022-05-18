program RTC_XMLRPCTest;

uses
  Forms,
  xmlrpcTest_unit in 'xmlrpcTest_unit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
