program fmx3AppClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmx3AppClient_Unit in 'fmx3AppClient_Unit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
