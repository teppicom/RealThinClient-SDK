{ @html(<b>)
  ISAPI Project Template
  @html(</b>)
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  @exclude }
library MSG_ISAPI;

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
  ISAPI_Module in 'ISAPI_Module.pas' {ISAPIModule: TDataModule},
  rtcMessengerProvider in '..\DataProviders\rtcMessengerProvider.pas' {Messenger_Provider: TDataModule},
  rtcMessenger in '..\DataProviders\rtcMessenger.pas';

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TISAPIModule, ISAPIModule);
  Application.Run;
end.
