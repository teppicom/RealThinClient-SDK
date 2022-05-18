program LinksLocal;

uses
  Forms,
  loLocalTest in 'loLocalTest.pas' {Form1},
  loFileCli in 'loFileCli.pas' {loFileClient},
  loFileSrv in 'loFileSrv.pas' {loFileServer: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
