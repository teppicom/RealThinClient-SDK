program SrvProject1;

uses
  Forms,
  SrvUnit1 in 'SrvUnit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
