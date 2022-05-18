program templateForm;

uses
  Forms,
  uTemplate in 'uTemplate.pas' {frmRtcParseDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmRtcParseDemo, frmRtcParseDemo);
  Application.Run;
end.
