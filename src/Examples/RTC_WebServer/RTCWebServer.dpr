program RTCWebServer;

{$include rtcDefs.inc}
{$include rtcDeploy.inc}

uses
{$IFDEF RtcDeploy}
  {$IFNDEF IDE_2006up}
    FastMM4,
    FastMove,
  {$ENDIF}
{$ENDIF}
  rtcLog,
  SysUtils,
  rtcTypes,
  rtcService,
  Windows,
  SvcMgr,
  WinSvc,
  Forms,
  Win_Service in 'Win_Service.pas' {Rtc_WebServer: TService},
  Server_Form in 'Server_Form.pas' {WebServerForm},
  Server_Module in 'Server_Module.pas' {Data_Server: TDataModule},
  rtcFileProvider in '..\DataProviders\rtcFileProvider.pas' {File_Provider: TDataModule},
  rtcMessengerProvider in '..\DataProviders\rtcMessengerProvider.pas' {Messenger_Provider: TDataModule},
  rtcISAPIProvider in '..\DataProviders\rtcISAPIProvider.pas' {ISAPI_Provider: TDataModule},
  rtcISAPI in '..\DataProviders\rtcISAPI.pas',
  rtcMessenger in '..\DataProviders\rtcMessenger.pas';

{$R *.res}

begin
StartLog;

if not IsDesktopMode(RTC_DATASERVICE_NAME) then
  begin
  SvcMgr.Application.Initialize;
  SvcMgr.Application.CreateForm(TRtc_WebServer, Rtc_WebServer);
  SvcMgr.Application.Run;
  end
else
  begin
  Forms.Application.Initialize;
  Forms.Application.Title := 'RTC WebServer';
  Forms.Application.CreateForm(TWebServerForm, WebServerForm);
  if ParamCount>0 then
    GetDataServer.ServerHTTP.ServerPort:=RtcString(ParamStr(1));
  Forms.Application.Run;
  end;
end.
