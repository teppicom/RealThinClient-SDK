unit AppServer_Module;

interface

uses
  SysUtils, Classes,

  rtcSystem, rtcInfo, rtcConn, rtcDataSrv,

  rtcFunction, rtcSrvModule;

type
  TAppSrv_Module = class(TDataModule)
    ServerModule: TRtcServerModule;
    FuncGroup: TRtcFunctionGroup;
    MulFunc: TRtcFunction;
    AddFunc: TRtcFunction;
    ServerLink: TRtcDataServerLink;
    LoopoFunc: TRtcFunction;
    procedure AddFuncExecute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MulFuncExecute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure LoopoFuncExecute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetAppSrvModule:TAppSrv_Module;

implementation

var
  AppSrv_Module: TAppSrv_Module;

{$R *.dfm}

function GetAppSrvModule:TAppSrv_Module;
  begin
  if not assigned(AppSrv_Module) then
    AppSrv_Module:=TAppSrv_Module.Create(nil);
  Result:=AppSrv_Module;
  end;

procedure TAppSrv_Module.AddFuncExecute(Sender: TRtcConnection;
    Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  Result.asFloat:=Param.asFloat['A']+Param.asFloat['B'];
  end;

procedure TAppSrv_Module.MulFuncExecute(Sender: TRtcConnection;
    Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  Result.asFloat:=Param.asFloat['A']*Param.asFloat['B'];
  end;

procedure TAppSrv_Module.LoopoFuncExecute(Sender: TRtcConnection;
    Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  Result.asObject:=Param.asObject['DATA'];
  Param.asObject['DATA']:=nil;
  end;

initialization
finalization
if assigned(AppSrv_Module) then
  FreeAndNil(AppSrv_Module);
end.
