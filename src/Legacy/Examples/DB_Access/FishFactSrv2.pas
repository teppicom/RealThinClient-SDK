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
  demonstrating the use of data received from RTC Clients to generate SQL staments for
  selecting data from a real Database and updating data inside a real Database, and
  could be easily modified for use with any SQL-capable components for Database Access.
}

unit FishFactSrv2;

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
    Query1.Active:=True;
    try
      // And then, we will copy the received dataset into a new TRtcDataSet inside our "Result" object
      DelphiDataSetToRtc(Query1, Result.NewDataSet); // This function is in the "rtcDB" unit
    finally
      Query1.Active:=False; // Close the dataset
      end;
    end
  else
    raise Exception.Create('Table "'+Param.asText['table']+'" not found on this Server.');
  end;

{ This function works with the TQuery component (BDE) and can be used
  to prepare the SQL statement and populate all required field values
  for executing the current Action from the "chg:TRtcDataSetChagnes" object.

  This is the most complex part in this whole example project,
  becuse it has to execute 3 different operations: INSERT, UPDATE or DELETE
  by using the Action attribute and information about OLD and NEW ROW content.

  Even though most developers will NOT want to use the BDE from their Servers,
  this example can be used as a "template" for implementing a similar function
  which would work with any other set of components for direct Database Access.

  This function also demonstrates the level of control you have on the Server
  when executing Actions receied from the Client. While some Databases might
  have full support for executing some actions using SQL, others may not.
  And because the Server is the one generating the SQL, it can customize
  the SQL statements to suit the needs of a Database, making sure that
  the same Client can work with any Database in the back-end.

  After this function completes, provided there were no exceptions raised,
  we only need to call "ExecSQL" method to execute the INSERT/UPDATE/DELETE action. }

procedure PrepareQueryAction(chg:TRtcDataSetChanges; TableName:String; qry:TQuery);
  var
    fld:integer;
    fname,s1,s2:String;
    nrow,orow:TRtcDataRow;
  begin
  { For what ever reason, the BDE has a problem if field names
    after INSERT or SET are enclosed in double quotes, but when using '<tablename>.'
    as prefix, the problem with double quotes around field names disapears.

    BLOB and FLOAT fields inside WHERE clause also seem to cause problems for the BDE.
    BLOB fields don't seem to be supported at all for the WHERE clause,
    while FLOAT fields are supported but are unreliable because of precision loss.

    This is why we will NOT be using BLOB fields at all for the WHERE clause
    in this example, while FLOATS will ONLY be used if they have no fractional part.
    Other Databases or Database Access components might NOT have these problems. }

  case chg.Action of
    rds_Insert:
      begin
      s1:='';
      s2:='';
      nrow:=chg.NewRow;

      // Prepare the list of fields and values for "INSERT INTO" ...
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        if not nrow.isNull[fname] then
          begin
          if s1<>'' then s1:=s1+', ';
          if s2<>'' then s2:=s2+', ';
          s1:=s1+TableName+'."'+fname+'"';
          s2:=s2+':v'+IntToStr(fld);
          end;
        end;

      // Our SQL statement is ready
      qry.SQL.Text:='INSERT INTO '+TableName+' ('+s1+') VALUES ('+s2+')';
      qry.Prepare;

      // Now we just need to populate new field values
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        if not nrow.isNull[fname] then // NIL values have already been set in the SQL text
          with qry.ParamByName('v'+IntToStr(fld)) do
            begin
            DataType:=RTC_FIELD2DB_TYPE(nrow.FieldType[fname]);
            Value:=nrow.Value[fname];
            end;
        end;
      end;

    rds_Update:
      begin
      s1:='';
      s2:='';
      orow:=chg.OldRow;
      nrow:=chg.NewRow;

      // Prepare the list of fields and values for the "WHERE" clause ...
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        // BDE does NOT allow the use of BLOB fields inside a WHERE clause, while
        // FLOAT fields with a fractional part cause problems because of precision loss,
        // so we need to skip them if they were sent by the Client
        if (orow.isType[fname]<>rtc_ByteStream) and
           ((orow.isType[fname]<>rtc_Float) or (frac(orow.asFloat[fname])=0)) then
          begin
          if s1<>'' then s1:=s1+' AND ';
          if orow.isNull[fname] then
            s1:=s1+'('+TableName+'."'+fname+'" is NULL)' // NULL value coparisons require "is" and not "="
          else
            s1:=s1+'('+TableName+'."'+fname+'" = :w'+IntToStr(fld)+')';
          end;
        end;

      // Prepare the SQL part with fields and values for "UPDATE table SET" ...
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        // We only want to update modified fields
        if orow.asCode[fname]<>nrow.asCode[fname] then // Simple check if OLD and NEW fields have different values
          begin
          if s2<>'' then s2:=s2+', ';
          if nrow.isNull[fname] then
            s2:=s2+TableName+'."'+fname+'" = NULL'
          else
            s2:=s2+TableName+'."'+fname+'" = :v'+IntToStr(fld);
          end;
        end;

      // Our SQL statement is ready
      qry.SQL.Text:='UPDATE '+TableName+' SET '+s2+' WHERE '+s1;
      qry.Prepare;

      // Populate "WHERE" parameters ...
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        if (orow.isType[fname]<>rtc_ByteStream) and // not a BLOB field
           ((orow.isType[fname]<>rtc_Float) or (frac(orow.asFloat[fname])=0)) then // NOT a FLOAT with a fractional part
          with qry.ParamByName('w'+IntToStr(fld)) do
            begin
            DataType:=RTC_FIELD2DB_TYPE(orow.FieldType[fname]);
            Value:=orow.Value[fname];
            end;
        end;

      // Populate all "UPDATE table SET" parameters ...
      for fld:=0 to nrow.FieldCount-1 do
        begin
        fname:=nrow.FieldName[fld];
        if orow.asCode[fname]<>nrow.asCode[fname] then // Field was Updated
          if not nrow.isNull[fname] then // New value is NOT NULL
            with qry.ParamByName('v'+IntToStr(fld)) do
              begin
              DataType:=RTC_FIELD2DB_TYPE(nrow.FieldType[fname]);
              Value:=nrow.Value[fname];
              end;
        end;
      end;

    rds_Delete:
      begin
      s1:='';
      orow:=chg.OldRow;

      // Prepare the SQL text for the "WHERE" clause ...
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        // BDE does NOT allow the use of BLOB fields inside a WHERE clause, while
        // FLOAT fields with a fractional part cause problems because of precision loss,
        // so we need to skip them if they were sent by the Client
        if (orow.isType[fname]<>rtc_ByteStream) and
           ((orow.isType[fname]<>rtc_Float) or (frac(orow.asFloat[fname])=0)) then
          begin
          if s1<>'' then s1:=s1+' AND ';
          if orow.isNull[fname] then // NULL values will be set with "is NULL"
            s1:=s1+'('+TableName+'."'+fname+'" is NULL)'
          else
            s1:=s1+'('+TableName+'."'+fname+'" = :w'+IntToStr(fld)+')';
          end;
        end;

      // Our SQL statement is ready
      qry.SQL.Text:='DELETE FROM '+TableName+' WHERE '+s1;
      qry.Prepare;

      // Populate "WHERE" parameters
      for fld:=0 to orow.FieldCount-1 do
        begin
        fname:=orow.FieldName[fld];
        if (orow.isType[fname]<>rtc_ByteStream) and // not a BLOB field
           ((orow.isType[fname]<>rtc_Float) or (frac(orow.asFloat[fname])=0)) then // NOT a FLOAT with fractional part
          if not orow.isNull[fname] then // not NULL
            with qry.ParamByName('w'+IntToStr(fld)) do
              begin
              DataType:=RTC_FIELD2DB_TYPE(orow.FieldType[fname]);
              Value:=orow.Value[fname];
              end;
        end;
      end;
    end;
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
          function "PrepareQueryAction" without making changes to the Table. }

        PrepareQueryAction(chg, 'biolife', Query1);

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
