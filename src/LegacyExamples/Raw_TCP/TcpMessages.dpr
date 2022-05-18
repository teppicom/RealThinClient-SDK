program TcpMessages;

uses
  Forms,
  ServerUnit1 in 'ServerUnit1.pas' {ServerForm1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServerForm1, ServerForm1);
  Application.Run;
end.
