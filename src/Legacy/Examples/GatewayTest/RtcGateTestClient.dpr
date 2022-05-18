program RTCGateTestClient;

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
  MainGateClientForm in 'MainGateClientForm.pas' {GateClientForm};

{$R *.res}

begin
  {$IFDEF IDE_2006up}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TGateClientForm, GateClientForm);
  Application.Run;
end.
