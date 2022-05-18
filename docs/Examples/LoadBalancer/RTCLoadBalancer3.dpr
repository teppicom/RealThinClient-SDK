program RTCLoadBalancer3;

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
  MainUnit3 in 'MainUnit3.pas' {RtcLoadBalancerMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcLoadBalancerMainForm, RtcLoadBalancerMainForm);
  Application.Run;
end.
