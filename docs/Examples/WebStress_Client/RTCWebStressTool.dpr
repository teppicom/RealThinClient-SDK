program RTCWebStressTool;

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
  WebStressTool_Unit in 'WebStressTool_Unit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
