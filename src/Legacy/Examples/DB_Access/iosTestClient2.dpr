program iosTestClient2;

uses
  FMX_Forms,
  iosUnit2 in 'iosUnit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;

end.
