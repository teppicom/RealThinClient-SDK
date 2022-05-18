program RTCLoadBalancer2;

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
  MainUnit2 in 'MainUnit2.pas' {RtcLoadBalancerMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcLoadBalancerMainForm, RtcLoadBalancerMainForm);
  Application.Run;
end.
