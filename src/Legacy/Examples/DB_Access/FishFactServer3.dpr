program FishFactServer3;

uses
  Forms,
  FishFactSrv3 in 'FishFactSrv3.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
