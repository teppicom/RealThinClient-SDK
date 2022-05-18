program iosTestClient;

uses
  FMX_Forms,
  iosUnit1 in 'iosUnit1.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
