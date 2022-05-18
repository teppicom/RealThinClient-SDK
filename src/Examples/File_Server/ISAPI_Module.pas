unit ISAPI_Module;

interface

{$include rtcDefs.inc}

uses
  SysUtils, Classes,

  {$IFDEF VER120}
  Forms, // D4
  {$ENDIF}
  {$IFDEF VER130}
  Forms, // D5
  {$ENDIF}

  rtcTypes, rtcSystem,
  rtcLog, rtcInfo, rtcConn,
  rtcDataSrv, rtcISAPISrv,

  rtcFileProvider;

type
  TISAPI_Server = class(TDataModule)
    Server: TRtcISAPIServer;
    procedure ServerRequestNotAccepted(Sender: TRtcConnection);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
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

procedure TISAPI_Server.DataModuleCreate(Sender: TObject);
  var
    s:string;
    a,i:integer;
    f:TextFile;
  begin
  StartLog;
  // Read configuration file ...
  XLog('Read LOG "'+RtcString(AppFileName)+'.cfg"');

  AssignFile(f,AppFileName+'.cfg');
  {$I-}
  Reset(f);
  {$I+}
  if IOResult=0 then
    begin
    try
      with GetFileProvider do
        begin
        // first 4 lines are for PHP support.
        for a:=1 to 4 do
          ReadLn(f, s);

        ClearIndexPages;
        ReadLn(f, s); s:=Trim(s); i:=StrToIntDef(s,0);
        for a:=0 to i-1 do
          begin
          ReadLn(f, s);
          AddIndexPage(s);
          end;

        ClearHosts;
        ReadLn(f, s); s:=Trim(s); i:=StrToIntDef(s,0);
        for a:=0 to i-1 do
          begin
          ReadLn(f, s);
          AddHost(s);
          end;

        // next 3 lines are for ISAPI support
        for a:=1 to 3 do
          ReadLn(f, s);

        ClearContentTypes;
        ReadLn(f, s); s:=Trim(s); i:=StrToIntDef(s,0);
        for a:=0 to i-1 do
          begin
          ReadLn(f, s);
          AddContentType(s);
          end;
        end;
    except
      // ignore errors while reading the CFG file
      end;
    CloseFile(f);
    end
  else
    begin
    with GetFileProvider do
      begin
      ClearIndexPages;
      AddIndexPage('index.htm');
      AddIndexPage('index.html');

      ClearHosts;
      AddHost('* = .\');

      ClearContentTypes;
      end;
    end;

  // Assign our Server to Data Providers
  GetFileProvider.ServerLink.Server:=Server;
  end;

procedure TISAPI_Server.ServerListenStart(Sender: TRtcConnection);
  begin
  XLog('ISAPI LOADED ...');
  end;

procedure TISAPI_Server.ServerListenStop(Sender: TRtcConnection);
  begin
  XLog('ISAPI UNLOADED.');
  end;

procedure TISAPI_Server.ServerRequestNotAccepted(Sender: TRtcConnection);
  begin
  // Anything that comes this far is not acceptable by any DataProvider component.
  with TRtcDataServer(Sender) do
    begin
    XLog('BAD! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Method "'+Request.Method+'" not supported.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');

    Disconnect;
    end;
  end;

end.
