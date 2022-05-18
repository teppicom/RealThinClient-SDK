program HelloWorldJSONServer;

{$include rtcDefs.inc}

uses
  rtcLog,
  Forms,
  Unit5 in 'Unit5.pas' {Form5};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
