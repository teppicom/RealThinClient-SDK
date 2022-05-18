program FishFactClient3;

uses
  Forms,
  FishFactCli3 in 'FishFactCli3.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
