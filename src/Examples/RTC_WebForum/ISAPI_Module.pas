unit ISAPI_Module;

interface

uses
  SysUtils, Classes, iniFiles, Forms,
  rtcSystem, rtcLog, rtcInfo, rtcConn,
  rtcDataSrv, rtcISAPISrv,
  rtcForumProvider;

type
  TISAPI_Server = class(TDataModule)
    Server: TRtcISAPIServer;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ISAPI_Server: TISAPI_Server;

implementation

{$R *.dfm}

{ TDataModule1 }

procedure TISAPI_Server.DataModuleCreate(Sender: TObject);
var
  I : integer;

  IniName : string;
  Ini : TCustomIniFile;

  SL : TStringList;
begin
  IniName := ChangeFileExt(AppFileName, '.ini');

  // Read configuration file ...
  XLog(Format('Read configuration file: "%s"',[IniName]));

  Ini := TIniFile.Create(IniName);
  try
    with GetForumProvider do
      begin
      ClearContentTypes;
      Init(Ini.ReadString('Forum','Host',''),
           Ini.ReadString('Forum','URI','/'),
           Ini.ReadString('Forum','Path','RtcForumData'));
      end;
  finally
    Ini.Free;
    end;

  Ini := TMemIniFile.Create(IniName);
  try
    with GetForumProvider do
      begin
      SL := TStringList.Create;
      try
        Ini.ReadSectionValues('Content Types', SL);
        for I := 0 to SL.Count - 1 do
          AddContentType(SL[I]);
      finally
        SL.Free;
        end;
      end;
  finally
    Ini.Free;
    end;

  // Assign our Server to Data Provider
  GetForumProvider.ServerLink.Server := Server;
  end;

end.
