program RTCGateChatClient3;

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
  MainGateClientForm3 in 'MainGateClientForm3.pas' {GateClientForm},
  ChatHostForm in 'ChatHostForm.pas' {ChatHostFrm},
  ChatCIDs in 'ChatCIDs.pas';

{$R *.res}

begin
  {$IFDEF IDE_2006up}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TGateClientForm, GateClientForm);
  Application.Run;
end.
