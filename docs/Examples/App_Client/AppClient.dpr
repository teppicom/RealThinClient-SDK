program AppClient;

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
  AppClient_Unit in 'AppClient_Unit.pas' {Form1};

{$R *.res}

begin
  {$IFDEF IDE_2006up}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
