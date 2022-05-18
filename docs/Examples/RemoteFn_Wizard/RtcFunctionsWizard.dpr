program RtcFunctionsWizard;

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
  MainForm in 'MainForm.pas' {formMain},
  SigFuncs in 'SigFuncs.pas',
  gmoOptions in 'gmoOptions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformMain, formMain);
  Application.Run;
end.
