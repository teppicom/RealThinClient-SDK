{ @html(<b>)
  ISAPI Project Template
  @html(</b>)
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  @exclude }
library AppISAPI;

{$include rtcDefs.inc}
{$include rtcDeploy.inc}

uses
{$IFDEF RtcDeploy}
  {$IFNDEF IDE_2006up}
    FastMM4,
    FastMove,
  {$ENDIF}
{$ENDIF}
  ActiveX,
  ComObj,
  rtcISAPIApp,
  Isapi_Module in 'Isapi_Module.pas' {MyISAPI_Module: TDataModule},
  AppServer_Module in 'AppServer_Module.pas' {AppSrv_Module: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMyISAPI_Module, MyISAPI_Module);
  Application.Run;
end.
