program fmxGateCli;

uses
  System.StartUpCopy,
  FMX.Forms,
  GCUnit1 in 'GCUnit1.pas' {GCMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGCMainForm, GCMainForm);
  Application.Run;
end.
