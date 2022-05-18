program fmx2File_Client;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmx2Client_Form in 'fmx2Client_Form.pas' {RtcFileClient};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcFileClient, RtcFileClient);
  Application.Run;
end.
