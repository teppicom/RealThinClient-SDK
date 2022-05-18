program File_Client;

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
  Client_Form in 'Client_Form.pas' {RtcFileClient};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcFileClient, RtcFileClient);
  Application.Run;
end.
