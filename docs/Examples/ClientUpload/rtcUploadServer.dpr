program rtcUploadServer;

uses
  Forms,
  ServerUnit1 in 'ServerUnit1.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
