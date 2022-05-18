program UdpMessages;

uses
  Forms,
  UdpMessages_Unit in 'UdpMessages_Unit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
