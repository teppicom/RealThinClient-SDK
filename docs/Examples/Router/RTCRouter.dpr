program RTCRouter;

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
  MainUnit1 in 'MainUnit1.pas' {RtcRouterMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcRouterMainForm, RtcRouterMainForm);
  Application.Run;
end.
