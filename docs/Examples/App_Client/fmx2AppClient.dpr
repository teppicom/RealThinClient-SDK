program fmx2AppClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmx2AppClient_Unit in 'fmx2AppClient_Unit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
