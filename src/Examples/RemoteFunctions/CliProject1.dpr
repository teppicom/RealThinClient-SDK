program CliProject1;

uses
  Forms,
  CliUnit1 in 'CliUnit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
