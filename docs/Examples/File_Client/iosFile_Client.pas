program iosFile_Client;

uses
  cwstring, cthreads, FMX_Forms,
  iosClient_Form in 'iosClient_Form.pas' {RtcFileClient};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcFileClient, RtcFileClient);
  Application.Run;
end.
