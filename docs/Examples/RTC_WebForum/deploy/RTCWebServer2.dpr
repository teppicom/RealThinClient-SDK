program RTCWebServer2;

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
  Windows,
  SvcMgr,
  WinSvc,
  Forms,
  rtcService,
  Win_Service in '..\Win_Service.pas' {Rtc_WebServer: TService},
  Server_Form in '..\Server_Form.pas' {WebServerForm},
  rtcFileProvider in '..\..\DataProviders\rtcFileProvider.pas' {File_Provider: TDataModule},
  rtcISAPIProvider in '..\..\DataProviders\rtcISAPIProvider.pas' {ISAPI_Provider: TDataModule},
  rtcMessengerProvider in '..\..\DataProviders\rtcMessengerProvider.pas' {Messenger_Provider: TDataModule},
  rtcForumProvider in '..\rtcForumProvider.pas' {Forum_Provider: TDataModule},
  Server_Module in '..\Server_Module.pas' {Data_Server: TDataModule};

{$R *.res}

begin
StartLog;

if not IsDesktopMode(RTC_DATASERVICE_NAME) then
  begin
  SvcMgr.Application.Initialize;
  SvcMgr.Application.Title := 'RTC WebServer 2';
  SvcMgr.Application.CreateForm(TData_Server, Data_Server);
  SvcMgr.Application.CreateForm(TRtc_WebServer, Rtc_WebServer);
  SvcMgr.Application.Run;
  end
else
  begin
  Forms.Application.Initialize;
  Forms.Application.Title := 'RTC WebServer';
  Forms.Application.CreateForm(TData_Server, Data_Server);
  Forms.Application.CreateForm(TWebServerForm, WebServerForm);
  if ParamCount>0 then
    begin
    Data_Server.Server.ServerPort:=ParamStr(1);
    WebServerForm.btnListen.Click;
    end;
  Forms.Application.Run;
  end;
end.


