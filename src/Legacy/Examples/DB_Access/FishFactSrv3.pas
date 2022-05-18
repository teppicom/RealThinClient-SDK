{
  @html(<b>)
  RTC Fish Facts Server 2 Demo Project
  @html(</b>)
  - Copyright 2004-2013 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This Project shows how to write a Server to provide Client-side access to Server-side
  Data, as well as a way to modify that Data by using DB-aware components on the Client.

  This Server Project is written to work with "FishFactsClient" and "FishFactsClient2" Projects,
  but can be modified to work with any other Client using TRtcMemDataSet or TRtcDataSetMonitor.

  This Project uses the BDE and a single TQuery component as a Server-side Data source,
  demonstrating the use of data received from RTC Clients to generate SQL staments by
  using functions provided in the "rtcDB" unit.
}

unit FishFactSrv3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBTables, Grids, DBGrids, StdCtrls, ExtCtrls,

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
    Panel1: TPanel;
    mSQL: TMemo;
    Query1: TQuery;
    Panel2: TPanel;
    Label1: TLabel;
    xInsert: TCheckBox;
    xUpdate: TCheckBox;
    xDelete: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure rtcSelectFnExecute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure rtcSubmitFnExecute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormShow(Sender: TObject);
  begin
  { In this example project, we will be using a single TQuery object (BDE) to access the Database. }

  // For our Server to start waiting for incoming connections from Clients, we need to call "Listen" ...
  RtcHttpServer1.Listen();
  end;

procedure TForm1.rtcSelectFnExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  { We want our "select" calls to be parametrized, so the Client won't be sending SQL text (SELECT statements).
    And in this particular example, the only parameter that matters is the name of the Table ... }

  if not Param.CheckType('table',rtc_Text) then
    raise Exception.Create('Table parameter required')

  { In this particular example, we only have one table, which we called 'biolife' ... }

  else if Param.asText['table']='biolife' then
    begin
    { This Project is very simplified and uses only one TQuery object placed on the Main Form.
      Using this approach, the BDE will always see only one user (no concurrent Database access),
      but it will ONLY work in Single-Threaded mode, so it is NOT recommended for heavy-load Servers.

      In a real Database Application Server, you would implement a DB connection Pool, from which
      you would be taking out a DB connection before you need access to the Database, then use that
      DB connection to get a TDataSet containing selected records, copy that TDataSet contents into
      a TRtcDataSet (for example, using the "DelphiDataSetToRTC" function), then release the TDataSet
      received from the Database and return the DB connection back to the Database connection Pool,
      so it can be used again later. By doing that, you will be able to make the Server Multi-Threaded
      and scale under different loads, without losing too much time opening and closing DB connections.

      Here is a pseudo-code example for working with a Database connection Pool:

      MyDBConnection := Get_Connection_From_DB_Pool();
      try
        MyQuery := Execute_Query( MyDBConnection, MyQueryParameters );
        try
          DelphiDataSetToRTC( MyQuery, Result.newDataSet );
        finally
          Release_Query(MyQuery);
          end;
      finally
        Put_Connetion_To_DB_Pool(MyDBConnection);
        end; }

    // We will execute a SELECT statement on the Database to get the complete biolife table
    Query1.SQL.Text:='SELECT * FROM biolife';
    mSQL.Text:=Query1.SQL.Text; // Copy the SQL text into the Memo, as a simple monitoring option

    Query1.Open;
    try
      // And then, we will copy the received dataset into a new TRtcDataSet inside our "Result" object
      DelphiDataSetToRtc(Query1, Result.NewDataSet); // This function is in the "rtcDB" unit
    finally
      Query1.Close;
      end;
    end
  else
    raise Exception.Create('Table "'+Param.asText['table']+'" not found on this Server.');
  end;

procedure TForm1.rtcSubmitFnExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  var
    chg:TRtcDataSetChanges;
  begin
  { We want our "submit" calls to be parametrized as well ... }
  if not Param.CheckType('table',rtc_Text) then
    raise Exception.Create('Table parameter is required')
  else if Param.isNull['change_data'] then
    raise Exception.Create('Change_Data parameter is required')

  { In this particular example, we only have one table, which we called 'biolife' ... }
  else if Param.asText['table']='biolife' then
    begin
    { The Client has sent us the object generated by the "ExtractChanges" method,
      so we can use it here to create the "TRtcDataSetChanges" instance.

      Using the TRtcDataSetChanges instance will allow us to enumerate through
      all actions performed by the Client and decide what to do with them here.

      In a real Database Server Application, you should have a Database Connection
      Pool from which you would get a Database connection object before doing
      anything with the Database, create the Query component dinamically, work
      with that Query object in the loop (below) to execute requested actions
      on the Database, then release the Query object and return the Database
      Connection back to your Database Connection Pool.

      But since this is a very simple single-threaded BDE example, we will
      just be using the "TQuery" component from our Main Form.

      The part for preparing the SQL statement and execute requested actions
      (INSERT, UPDATE or DELETE) is identical in both situations, so this
      example could easily be extended to a real Database Server Application. }

    // Client sent us all Changes to our Table in the "change_data" parameter
    chg:=TRtcDataSetChanges.Create(Param.asObject['change_data']);
    try
      chg.First;
      // We want to enumerate through all received Actions,
      // allowing the client to send more than one operation at a time.
      while not chg.EOF do
        begin
        { Normally, you would first check if a user is allowed to execute
          a certain action before preparing the SQL statement. But ...
          in this example, we will be preparing the SQL statement even
          if the user is NOT allowed to execute it, so we can test the
          function "rtcPrepareSqlAction" without making changes to the Table. }

        // Prepare the SQL statement for executing received change Action
        Query1.SQL.Text := rtcPrepareSqlAction(chg, 'biolife'); // function from the "rtcDB" unit
        Query1.Prepare;

        // Copy the SQL statement into the MEMO, as a simple monitoring option
        mSQL.Text:=Query1.SQL.Text+        #13#10+
                  '----------------------'+#13#10+
                  chg.GetActionSQL('biolife');

        { Because this is a very simple single-threaded example,
          we are using Checkbox components placed on the Form to
          define global acccess rights for all Clients. In a real
          Database Server Application, you would want to implement
          more sophisticated methods of controlling user rights. }

        if (chg.Action=rds_Insert) and not xInsert.Checked then
          raise Exception.Create('Insert operations are NOT allowed');

        if (chg.Action=rds_Update) and not xUpdate.Checked then
          raise Exception.Create('Update operations are NOT allowed');

        if (chg.Action=rds_Delete) and not xDelete.Checked then
          raise Exception.Create('Delete operations are NOT allowed');

        { Set SQL parameters required to execute the SQL statement
          prepared by using the "rtcPrepareSqlAction" function above ... }
        rtcSetSqlActionParams(chg, Query1.Params); // function from the "rtcDB" unit

        { If everything went OK up until here, we will
          have our SQL statement prepared and ready for execution
          (see the "PrepareQueryAction" function - called above). }
        Query1.ExecSQL;

        { After executing the SQL statement (above), we should check how many rows have been
          affected after execution and notify the user if more or less than 1 row was affected.

          In a real Database Server Application, you should use Transactions and ROLL BACK
          any changes to the Database in case more than one ROW was affected, to make sure
          that errors in the WHERE clause won't damage a complete Table or the whole Database. }

        if Query1.RowsAffected<1 then
          raise Exception.Create('SQL operation did NOT succeed: NO ROWS affected.')
        else if Query1.RowsAffected>1 then
          raise Exception.Create('SQL operation miss-fired: '+IntToStr(Query1.RowsAffected)+' ROWS affected.');

        // Move to the next Action in the list ..
        chg.Next;
        end;
    finally
      // Release the "TRtcDataSetChanges" object here to avoid memory leaks
      chg.Free;
      end;

    { Our example Fishfact Client expects a Boolean "TRUE" as a Result
      if everything was OK, which is what we will do here ... }
    Result.asBoolean:=True;
    end
  else
    raise Exception.Create('Table "'+Param.asText['table']+'" not found on this Server.');
  end;

end.
