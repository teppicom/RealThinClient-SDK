program MSG_Server;

{$include rtcDefs.inc}
{$include rtcDeploy.inc}

uses
{$IFDEF RtcDeploy}
  {$IFNDEF IDE_2006up}
    FastMM4,
    FastMove,
  {$ENDIF}
{$ENDIF}
  Forms,
  Server_MainForm in 'Server_MainForm.pas' {ServerMain},
  rtcMessengerProvider in '..\DataProviders\rtcMessengerProvider.pas' {Messenger_Provider: TDataModule},
  rtcMessenger in '..\DataProviders\rtcMessenger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServerMain, ServerMain);
  Application.Run;
end.
