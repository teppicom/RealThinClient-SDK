program FileServer;

{$include rtcDefs.inc}
{$include rtcDeploy.inc}

uses
{$IFDEF RtcDeploy}
  {$IFNDEF IDE_2006up}
    FastMM4,
    FastMove,
  {$ENDIF}
{$ENDIF}
  Forms,
  rtcTypes,
  Server_Form in 'Server_Form.pas' {RtcFileServer: TRtcDataServer},
  rtcFileProvider in '..\DataProviders\rtcFileProvider.pas' {File_Provider: TDataModule},
  HTTP_Module in 'HTTP_Module.pas' {HTTP_Server: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'RTC WebServer Demo';
  Application.CreateForm(TRtcFileServer, RtcFileServer);
  Application.CreateForm(THTTP_Server, HTTP_Server);
  if ParamCount>0 then
    begin
    Http_Server.ServerHTTP.ServerPort:=RtcString(ParamStr(1));
    RtcFileServer.btnListen.Click;
    end;
  Application.Run;
end.
