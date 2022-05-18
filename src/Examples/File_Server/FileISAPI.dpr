{ @html(<b>)
  ISAPI Project Template
  @html(</b>)
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  @exclude }
library FileISAPI;

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
  ISAPI_Module in 'ISAPI_Module.pas' {ISAPI_Server: TDataModule},
  rtcFileProvider in '..\DataProviders\rtcFileProvider.pas' {File_Provider: TDataModule};

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TISAPI_Server, ISAPI_Server);
  Application.Run;
end.
