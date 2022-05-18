program fmx2AppServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmx2Server_Form in 'fmx2Server_Form.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
