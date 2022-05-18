program fmxFile_Client;

{$include rtcDefs.inc}

uses
  FMX.Forms,
  fmxClient_Form in 'fmxClient_Form.pas' {RtcFileClient};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcFileClient, RtcFileClient);
  Application.Run;
end.
