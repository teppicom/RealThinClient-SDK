program fmxAppServer;

uses
  FMX.Forms,
  fmxServer_Form in 'fmxServer_Form.pas' {Form1},
  AppServer_Module in 'AppServer_Module.pas' {AppSrv_Module: TDataModule};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
