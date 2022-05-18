program iosAppClient;

uses
  FMX_Forms,
  iosAppClient_Unit in 'iosAppClient_Unit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
