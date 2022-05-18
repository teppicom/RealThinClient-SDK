program fmx3File_Client;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmx3Client_Form in 'fmx3Client_Form.pas' {RtcFileClient};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcFileClient, RtcFileClient);
  Application.Run;
end.
