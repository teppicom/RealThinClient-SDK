program iosAppServer;

uses
  FMX_Forms,
  iosServer_Form in 'iosServer_Form.pas' {Form1},
  AppServer_Module in 'AppServer_Module.pas' {AppSrv_Module: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
