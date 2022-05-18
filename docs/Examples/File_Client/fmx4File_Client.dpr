program fmx4File_Client;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmx4Client_Form in 'fmx4Client_Form.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
