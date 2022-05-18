program RTCRouter2;

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
  MainUnit2 in 'MainUnit2.pas' {RtcRouter2MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcRouter2MainForm, RtcRouter2MainForm);
  Application.Run;
end.
