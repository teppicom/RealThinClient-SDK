program FishFactClient2;

uses
  Forms,
  FishFactCli2 in 'FishFactCli2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
