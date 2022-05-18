program LinksClient;

uses
  Forms,
  CliUnit1 in 'CliUnit1.pas' {Form1},
  loFileCli in 'loFileCli.pas' {loFileClient};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
