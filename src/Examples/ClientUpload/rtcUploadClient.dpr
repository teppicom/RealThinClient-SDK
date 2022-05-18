program rtcUploadClient;

uses
  Forms,
  ClientUnit1 in 'ClientUnit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
