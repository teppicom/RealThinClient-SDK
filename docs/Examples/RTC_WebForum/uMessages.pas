unit uMessages;

interface

uses
  rtcInfo, rtcSystem, IniFiles, SysUtils;

function GetMsg(msg_id : string) : string;

implementation

var
  Messages : TMemIniFile = nil;

function GetMsg(msg_id : string) : string;
  begin
  Result := Trim(Messages.ReadString('Messages', msg_id, msg_id));
  end;

initialization
Messages := TMemIniFile.Create(ExtractFilePath(AppFileName) + 'messages.ini');
finalization
FreeAndNil(Messages);
end.
