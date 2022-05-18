program WSockServTest;

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
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

type TForcedMemLeak=class(TObject);

begin
  // TForcedMemLeak.Create;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
