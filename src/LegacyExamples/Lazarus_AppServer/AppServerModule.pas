unit AppServerModule;

{$ifdef fpc}{$mode delphi}{$h+}{$endif}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs,
  rtcSystem, rtcInfo, rtcConn, rtcDataSrv,
  rtcFunction, rtcSrvModule;

type

  { TAppSrv_Module }

  TAppSrv_Module = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    ServerModule: TRtcServerModule;
    FuncGroup: TRtcFunctionGroup;
    MulFunc: TRtcFunction;
    AddFunc: TRtcFunction;
    ServerLink: TRtcDataServerLink;
    LoopoFunc: TRtcFunction;
    procedure AddFuncExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure MulFuncExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure LoopoFuncExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  end; 

var
  AppSrv_Module: TAppSrv_Module;

implementation

{ TAppSrv_Module }

procedure TAppSrv_Module.DataModuleCreate(Sender: TObject);
begin
  FuncGroup:=TRtcFunctionGroup.Create(Self);
  
  ServerModule:=TRtcServerModule.Create(Self);
  with ServerModule do begin
    Link := ServerLink;
    DataFormats := [fmt_RTC, fmt_XMLRPC];
    SecureKey := 'This is a test.';
    AutoSessionsLive := 60;
    ModuleFileName := '/TEST';
    FunctionGroup := FuncGroup;
  end;

  
  ServerLink:=TRtcDataServerLink.Create(Self);
  MulFunc:=TRtcFunction.Create(Self);
  AddFunc:=TRtcFunction.Create(Self);
  LoopoFunc:=TRtcFunction.Create(Self);

  ServerModule.FunctionGroup := FuncGroup;
  ServerModule.Link:= ServerLink;

  MulFunc.Group := FuncGroup;
  MulFunc.FunctionName := 'Mul';
  MulFunc.OnExecute:=MulFuncExecute;
  AddFunc.Group := FuncGroup;
  AddFunc.FunctionName := 'Add';
  AddFunc.OnExecute:=AddFuncExecute;
  LoopoFunc.Group := FuncGroup;
  LoopoFunc.FunctionName := 'loopo';
  LoopoFunc.OnExecute:=LoopoFuncExecute;
end;

procedure TAppSrv_Module.DataModuleDestroy(Sender: TObject);
begin

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
  {$I AppServerModule.lrs}

end.

