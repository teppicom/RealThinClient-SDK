program RTCGateNode;

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
  MainUnit in 'MainUnit.pas' {RTCGateNodeForm},
  MainModule in 'MainModule.pas' {RTCPModule: TDataModule},
  MakeAccUnit in 'MakeAccUnit.pas' {RTCPMakeAccount},
  SendAccUnit in 'SendAccUnit.pas' {RTCPSendAccount};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRTCGateNodeForm, RTCGateNodeForm);
  Application.Run;
end.
