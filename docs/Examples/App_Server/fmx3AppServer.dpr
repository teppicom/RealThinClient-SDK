program fmx3AppServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmx3Server_Form in 'fmx3Server_Form.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
