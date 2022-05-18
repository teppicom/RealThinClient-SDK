{ @html(<b>)
  Web Forum ISAPI Project
  @html(</b>)
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  @exclude }
library WebForum_ISAPI;

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
  Forms,
  ComObj,
  rtcISAPIApp,
  ISAPI_Module in '..\ISAPI_Module.pas' {ISAPI_Server: TDataModule},
  rtcForumProvider in '..\rtcForumProvider.pas' {Forum_Provider: TDataModule};

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
