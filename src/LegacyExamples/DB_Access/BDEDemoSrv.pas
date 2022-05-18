{
  @html(<b>)
  RTC BDE Demo Server Project
  @html(</b>)
  - Copyright 2004-2013 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This Project shows how to write a Server which can provide access to a Database
  as well as functions to modify the Database by using DB-aware components on the Client.

  This Server Project is compatible with "FishFactsClient" and "FishFactsClient2" Projects,
  but can also be used with any other Client using TRtcMemDataSet or TRtcDataSetMonitor components.

  This Project works with the BDE and implements a simple TSession object pool
  to allow the Server to run in Multi-Threaded mode and scale with higher load.
}

unit BDEDemoSrv;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBTables, Grids, DBGrids, StdCtrls, ExtCtrls,

  SyncObjs,

  rtcSystem, rtcConn, rtcInfo,
  rtcDataSrv, rtcHttpSrv,
  rtcSrvModule, rtcFunction,

  rtcDB;

type
  TForm1 = class(TForm)
    RtcHttpServer1: TRtcHttpServer;
    RtcServerModule1: TRtcServerModule;
    RtcFunctionGroup1: TRtcFunctionGroup;
    rtcSelectFn: TRtcFunction;
    rtcSubmitFn: TRtcFunction;
    Label1: TLabel;
    ePort: TEdit;
    btnListen: TButton;
    lblStatus: TLabel;
    procedure rtcSelectFnExecute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure rtcSubmitFnExecute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure btnListenClick(Sender: TObject);
    procedure RtcHttpServer1ListenStart(Sender: TRtcConnection);
    procedure RtcHttpServer1ListenStop(Sender: TRtcConnection);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MySessions:TList;
    MySessCount:integer;
    MySessCS:TCriticalSection;
    function Get_DB_Session:TSession;
    procedure Put_DB_Session(ses:TSession);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.rtcSelectFnExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  var
    ses:TSession;
    qry:TQuery;
    SqlWhere:String;
  begin
  { In this example Server project, the client will be sending us:

    asText['table'] = Table name
    asRecord['eq'] = EQUAL (=) fields for the "WHERE" clause
    asRecord['le'] = LOWER-OR-EQUAL (<=) fields for the "WHERE" clause
    asRecord['ge'] = GREATER-OR-EQUAL (>=) fields for the "WHERE" clause

    In a real Database Access Server, it would be wise to check request parameters
    (like the Table name and received field names) to avoid SQL injection attacks.
    The user would also need to log in (using a separate remote function) and
    we would need to check access rights to make sure the user is allowed to
    do what he wants, before we execute his request on the Database.

    But ... for simplicity reasons, this example assumes that all Clients can
    be trusted and are allowed to do what ever they want with the Database. }

  if not Param.CheckType('table',rtc_Text) then
    raise Exception.Create('Table parameter required');

  if not Param.isNull['eq'] then
    if not Param.CheckType('eq',rtc_Record) then
      raise Exception.Create('EQ parameter has to be a Record');

  if not Param.isNull['le'] then
    if not Param.CheckType('le',rtc_Record) then
      raise Exception.Create('LE parameter has to be a Record');

  if not Param.isNull['ge'] then
    if not Param.CheckType('ge',rtc_Record) then
      raise Exception.Create('GE parameter has to be a Record');

  ses:=Get_DB_Session;
  try
    qry:=TQuery.Create(nil);
    try
      qry.SessionName:=ses.SessionName;
      qry.DatabaseName:='DBDEMOS';

      { In this example, we will use functions from the "rtcDB" unit
        to prepare the WHERE clause in our SQL Select statement. }

      { First, we need to prepare the SQL Statement ... }
      SqlWhere:='';
      rtcPrepareSqlWhere(Param.asRecord['eq'],'eq', '=', Param.asText['table'], SqlWhere);
      rtcPrepareSqlWhere(Param.asRecord['le'],'le','<=', Param.asText['table'], SqlWhere);
      rtcPrepareSqlWhere(Param.asRecord['ge'],'ge','>=', Param.asText['table'], SqlWhere);

      qry.SQL.Text:='SELECT * from "'+Param.asText['table']+'"'+SqlWhere;
      qry.Prepare;

      { After that, we need to assign all SQL Where parameters }
      rtcSetSqlWhereParams(Param.asRecord['eq'],'eq', qry.Params);
      rtcSetSqlWhereParams(Param.asRecord['le'],'le', qry.Params);
      rtcSetSqlWhereParams(Param.asRecord['ge'],'ge', qry.Params);

      { Once the SQL Statement is prepared and all parameters assigned,
        we can open the Query to get the resulting dataset ... }
      qry.Open;
      try
        DelphiDataSetToRtc(qry, Result.NewDataSet);
      finally
        qry.Close;
        end;
    finally
      qry.Free;
      end;
  finally
    Put_DB_Session(ses);
    end;
  end;

procedure TForm1.rtcSubmitFnExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  var
    chg:TRtcDataSetChanges;
    ses:TSession;
    qry:TQuery;
  begin
  { In this example Server project, the client will be sending us:

    asText['table'] = Table name
    asObject['change_data'] = TRtcValue received from the "ExtractChanges" method (TRtcMemDataSet or TRtcDataSetMonitor component)

    In a real Database Access Server, it would be wise check request parameters
    (like the Table name and received field names) to avoid SQL injection attacks.
    The user would also need to log in (using a separate remote function) and
    we would need to check access rights to make sure the user is allowed to
    do what he wants, before we execute his request on the Database.

    But ... for simplicity reasons, this example assumes that all Clients can
    be trusted and are allowed to do what ever they want with the Database. }

  if not Param.CheckType('table',rtc_Text) then
    raise Exception.Create('Table parameter is required')
  else if Param.isNull['change_data'] then
    raise Exception.Create('Change_Data parameter is required');

  ses:=Get_DB_Session;
  try
    qry:=TQuery.Create(nil);
    try
      qry.SessionName:=ses.SessionName;
      qry.DatabaseName:='DBDEMOS';

      chg:=TRtcDataSetChanges.Create(Param.asObject['change_data']);
      try
        chg.First;
        while not chg.EOF do
          begin
          { First, we need to prepare the SQL statement ... }
          qry.SQL.Text := rtcPrepareSqlAction(chg, Param.asText['table']);
          qry.Prepare;

          { Then, we can set all SQL parameters ... }
          rtcSetSqlActionParams(chg, qry.Params);

          { And ... execute the SQL statement }
          qry.ExecSQL;

          if qry.RowsAffected<1 then
            raise Exception.Create('SQL operation did NOT succeed: NO ROWS affected.')
          else if qry.RowsAffected>1 then
            raise Exception.Create('SQL operation miss-fired: '+IntToStr(qry.RowsAffected)+' ROWS affected.');
          chg.Next;
          end;
      finally
        chg.Free;
        end;
    finally
      qry.Free;
      end;
  finally
    Put_DB_Session(ses);
    end;

  Result.asBoolean:=True;
  end;

procedure TForm1.btnListenClick(Sender: TObject);
  begin
  if RtcHttpServer1.isListening then
    RtcHttpServer1.StopListen
  else
    begin
    RtcHttpServer1.ServerPort:=ePort.Text;
    RtcHttpServer1.Listen();
    end;
  end;

procedure TForm1.RtcHttpServer1ListenStart(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcHttpServer1ListenStart);
  btnListen.Caption:='Stop';
  lblStatus.Caption:='Listening on Port '+Sender.LocalPort;
  end;

procedure TForm1.RtcHttpServer1ListenStop(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcHttpServer1ListenStop);
  btnListen.Caption:='Listen';
  lblStatus.Caption:='Server NOT listening.';
  end;

procedure TForm1.FormCreate(Sender: TObject);
  begin
  MySessCS:=TCriticalSection.Create;
  MySessions:=TList.Create;
  MySessCount:=0;
  end;

procedure TForm1.FormDestroy(Sender: TObject);
  var
    a:integer;
  begin
  for a:=0 to MySessions.Count-1 do
    TSession(MySessions.Items[a]).Free;
  MySessions.Free;
  MySessCS.Free;
  end;

function TForm1.Get_DB_Session: TSession;
  begin
  MySessCS.Acquire;
  try
    if MySessions.Count=0 then
      begin
      Inc(MySessCount);
      Result:=TSession.Create(nil);
      Result.SessionName:='BDEDemos'+IntToStr(MySessCount);
      end
    else
      begin
      Result:=TSession(MySessions.Items[MySessions.Count-1]);
      MySessions.Delete(MySessions.Count-1);
      end;
  finally
    MySessCS.Release;
    end;
  end;

procedure TForm1.Put_DB_Session(ses: TSession);
  begin
  MySessCS.Acquire;
  try
    MySessions.Add(ses);
  finally
    MySessCS.Release;
    end;
  end;

end.
