program RTCWebForumTLS;

uses
  Forms,
  uSrv_TLS in '..\uSrv_TLS.pas' {frmServer},
  HTTP_Module_TLS in '..\HTTP_Module_TLS.pas' {HTTPS_Server: TDataModule},
  rtcForumProvider in '..\rtcForumProvider.pas' {ForumProviderDM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmServer, frmServer);
  Application.OnException := frmServer.AppException;;
  Application.Run;
end.
