program fmxAppClient;

uses
  FMX.Forms,
  fmxAppClient_Unit in 'fmxAppClient_Unit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
