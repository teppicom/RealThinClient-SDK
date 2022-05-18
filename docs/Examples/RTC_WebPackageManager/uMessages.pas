unit uMessages;

interface

function GetMsg(msg_id : string) : string;

implementation

uses
  IniFiles, SysUtils;

var
  Messages : TMemIniFile = nil;

function GetMsg(msg_id : string) : string;
begin
  if not Assigned(Messages) then
    Messages := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'messages.ini');

  Result := Trim(Messages.ReadString('Messages', msg_id, msg_id));
end;

initialization

finalization
  if Assigned(Messages) then
    begin
    Messages.Free;
    Messages:=nil;
    end;

end.
