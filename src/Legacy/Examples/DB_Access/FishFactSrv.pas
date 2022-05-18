{
  @html(<b>)
  RTC Fish Facts Server Demo Project
  @html(</b>)
  - Copyright 2004-2013 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This Project shows how to write a Server to provide Client-side access to Server-side
  Data, as well as a way to modify that Data by using DB-aware components on the Client.

  This Server Project is written to work with "FishFactsClient" and "FishFactsClient2" Projects,
  but can be modified to work with any other Client using TRtcMemDataSet or TRtcDataSetMonitor.

  This Project uses a TRtcMemDataSet component as a Server-side Data source.
}

unit FishFactSrv;

interface

{$include rtcDefs.inc}

// Declare "RECREATE_TABLE" if you need to recreate the "FishFacts.data" file
{.$DEFINE RECREATE_TABLE}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, DBGrids, StdCtrls, ExtCtrls,

  {$IFDEF RECREATE_TABLE}
  DBTables,
{$ENDIF}

  rtcFunction,
  rtcDataSrv,
  rtcSrvModule,
  rtcInfo,
  rtcConn,
  rtcHttpSrv,
  rtcDB,
  rtcSystem;

type
  TForm1 = class(TForm)
    RtcHttpServer1: TRtcHttpServer;
    RtcServerModule1: TRtcServerModule;
    RtcFunctionGroup1: TRtcFunctionGroup;
    rtcSelectFn: TRtcFunction;
    rtcSubmitFn: TRtcFunction;
    RtcMemDataSet1: TRtcMemDataSet;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Panel1: TPanel;
    lblSQL: TLabel;
    Splitter1: TSplitter;
    Panel2: TPanel;
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
{$IFDEF RECREATE_TABLE}
  var ds:TRtcDataSet;
      Table1:TTable;
      a:integer;
      fn:String;
{$ENDIF}
  begin
  { In this example project, we will NOT be using an actual Database, but will instead
    be loading a file "FishFacts.data", which was previously created by using the code below.
    This is NOT because RTC components require such measures, but because I wanted to make
    this Project completely independent of any Database Access components (like the BDE),
    so even the Server can be copied to any PC along with the "Fishfacts.data" file and
    tested without having to install anything else there, to simplify testing ... }

{$IFDEF RECREATE_TABLE}
  Table1:=TTable.Create(nil); // Create a temporary TTable instance
  Table1.DatabaseName:='DBDEMOS'; // We want data from "DBDEMOS" database
  Table1.TableName:='biolife.db'; // "biolife" is the Table we need
  Table1.Active:=True;

  ds:=TRtcDataSet.Create; // This is our temporary storage
  DelphiDataSetToRtc(Table1,ds); // Copy table contents to our temporary storage

  // Change AnsiString fields to WideString, so we can test Unicode support ...
  for a := 0 to ds.FieldCount-1 do
    begin
    fn:=ds.FieldName[a];
    if ds.FieldType[fn]=ft_String then
      ds.FieldType[fn]:=ft_WideString;
    end;

  RtcMemDataSet1.asObject:=ds; // assign temporary storage to our in-memory TDataSet descendant

  Table1.Active:=False; // Close the Table
  Table1.Free; // Destroy the Table object, we won't need it anymore

  RtcMemDataSet1.Active:=True;
  RtcMemDataSet1.FileName:=ExtractFilePath(AppFileName)+'Fishfacts.data';
  RtcMemDataSet1.Active:=False; // Save DataSet to the file (see "FileName" property above)
{$ENDIF}

  { The use of a flat file instead of a Database is to avoid making the example
    Project dependant on the BDE or any other Database Access components,
    so it can be copied to any PC and tested together with the local "Fishfacts.data" file.

    We also don't want any changes to be written back to that file, so we can also test
    Delete and Insert operations without fear of losing our test data. }

  RtcMemDataSet1.FileName:=ExtractFilePath(AppFileName)+'Fishfacts.data'; // Load data from the file.
  RtcMemDataSet1.Active:=True; // Activate our in-memory DataSet on the Server for a LIVE view
  RtcMemDataSet1.FileName:=''; // We don't want changes to be written back to the File on Exit!

  { To make this simple Client/Server example a bit more interesting, we have also linked our
    in-Memory DataSet to a DBGrid through a DataSource, so we can see all the changes - LIVE.
    Please note that this is NOT something you should do in a real Database Application Server! }

  // For our Server to start waiting for incoming connections from Clients, we need to call "Listen" ...
  RtcHttpServer1.Listen();
  end;

procedure TForm1.rtcSelectFnExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  { We want our "select" calls to be parametrized, so the Client won't be sending SQL statements as text.
    And in this particular example, the only parameter that matters is the name of the Table ... }
  if not Param.CheckType('table',rtc_Text) then
    raise Exception.Create('Table parameter required')

  { In this particular example, we only have one table, which we called 'biolife' ... }
  else if Param.asText['table']='biolife' then
    begin
    { In a real Database Application Server, working with a real Database, you would implement a
      DB connection Pool, from which you would be taking out a DB connection before you need access
      to the Database, then use that DB connection to get a TDataSet containing selected records,
      copy that TDataSet contents into a TRtcDataSet (for example, using the "DelphiDataSetToRTC"
      function), then release the TDataSet received from the Database and return the DB connection
      back to the Database connection Pool, so it can be used again later. By doing that, you
      will be able to make the Server Multi-Threaded and scale under different loads, without
      losing too much time opening and closing DB connections for each access.

      Here is a pseudo-code example for working with a real Database:

      // Get a DB Connection from a DB connection pool
      MyDBConnection := Get_Connection_From_DB_Pool();
      try
        // Get a Query object from a Query object pool, or create a Query object on-demand
        // and execute a Query using "MyDBConnection" with parameters "MyQueryParameters".
        // This will return a TDataSet descendant allowing at least read access
        MyQuery := Execute_Query( MyDBConnection, MyQueryParameters );
        try
          // Copy the contents of our Query (TDataSet) to our Result object,
          // so it will be sent back to the Client ...
          DelphiDataSetToRTC( MyQuery, Result.newDataSet );

          // If we need more than a DataSet, we can place our DataSet inside a Record, like ...
          //   Result.newRecord; <- create a Record instead of a DataSet
          //   Result.asRecord.asBoolean['ok']:=True; <- example additional parameter
          //   DelphiDataSetToRTC( MyQuery, Result.asRecord.newDataSet('data') ); <- place a dataset inside the "data" field of our record
          // ... in which case the Client would have to access the DataSet through the record,
          // because Result.isType would then return rtc_Record and not rtc_DataSet, and
          // Result.asRecord.isType['data'] would return rtc_DataSet (the DataSet we sent).
        finally
          // Release the "MyQuery" object
          Release_Query(MyQuery);
          end;
      finally
        // Return the "MyDBConnection" back to our DB Connection pool
        Put_Connetion_To_DB_Pool(MyDBConnection);
        end; }

    { Since this Project is a very simplified example which does NOT use a Database,
      but instead uses a single TRtcMemDataSet instance containing all data from our single Table
      and because this example is designed to work ONLY in Single-Threaded mode, the following
      line is all we will need to copy the complete Table from RtcMemDataSet1 to the Result object ... }

    Result.asDataSet := RtcMemDataSet1.asDataSet;

    { The line above ONLY works with the TRtcMemDataSet component!

      When using any other TDataSet descendant, you will either call the "DelphiDataSetToRTC"
      function as shown in the pseudo-code example above, or write your own function to copy
      fields and records from DB components you are using - if they are not TDataSet descendants.

      Because TRtcMemDataSet is also a TDataSet descendant, this line would also work:
        DelphiDataSetToRtc( RtcMemDataSet1, Result.newDataSet );

      But since our TRtcMemDataSet is linked to a DBGrid, we would need to disable controls
      before copying and enable them afterwards to avoid the DBGrid scrolling up and down. }
    end
  else
    raise Exception.Create('Table "'+Param.asText['table']+'" not found on this Server.');
  end;

procedure TForm1.rtcSubmitFnExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  var
    chg:TRtcDataSetChanges;
    ds:TRtcDataSet;
    fname:String;
    i:integer;
    Found:boolean;
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
      all actions performed by the Client and decide what to do with them here. }

    { Since we are using a TRtcMemDataSet as our "Database", we want to use the fastest
      access method available, which is through its internal TRtcDataSet instance ... }
    ds:=RtcMemDataSet1.asDataSet; // Get direct access to TRtcDataSet inside our TRtcMemDataSet

    // Create a TRtcDataSetChanges object by using data received in the "change_data" parameter
    chg:=TRtcDataSetChanges.Create(Param.asObject['change_data']);
    try
      // Enumerate through all received change actions ...
      chg.First;
      while not chg.EOF do
        begin
        { This implementation is different from anything you will normally use,
          because it executes actions on a TRtcDataSet stored inside a TRtcMemDataset.

          Even though the code below looks rather complex and long, you will NOT need to
          do the same when working with a real Database, especially if it has SQL support!

          To see an example on how the SQL statement might look like for executing
          the current action (INSERT/UPDATE/DELETE) on a table "Biolife", check the
          implementation of the "GetActionSQL" method used below. This method is used here
          only to display a simplified SQL for each change received from the Client
          and should NOT be used 1:1 for executing SQL statements on a Database,
          because it uses RTC Format for data and NOT propper SQL formats.

          In a real Database Server Application, you will most likely be preparing an
          SQL statement and using parameters to fill in field values for "WHERE",
          "UPDATE" and "INSERT" clauses, to make sure all field types are set correctly. }

        lblSQL.Caption:='Latest change received from Client(s):'#13#10+
                        chg.GetActionSQL('biolife');

        { ---->>> Specific implementation, useful ONLY for this example project --->>> }

        if (chg.Action=rds_Update) or (chg.Action=rds_Delete) then
          begin
          { Because we do NOT have SQL capabilities in our simple in-Memory Dataset,
            nor do we have any functions for locating a record by using key fields,
            we will be going sequentially through the whole table while searching
            for a place to execute our Update or Delete action. Yes, this is a very
            "quick and dirty" way of doing things, but since you would NEVER EVER
            want to use the TRtcMemDatSet on the *Server* side to keep or update data
            from a Database, we can live with this implementation - HERE ... }

          Found:=False;
          ds.First;
          while not ds.EOF do
            begin
            Found:=True;
            // We will be enumerating through all Fields in "chg.OldRow" ...
            for i:=0 to chg.OldRow.FieldCount-1 do
              begin
              fname:=chg.OldRow.FieldName[i];
              if ds.asText[fname]<>chg.OldRow.asText[fname] then
                begin
                Found:=False;
                Break;
                end;
              end;
            // If we find a record in "DS" with all Field values
            // matching those from "OldRow", we are on the right record
            if Found then Break;
            // If any field value does NOT match, we need to continue ...
            ds.Next;
            end;

          // For Update and Delete operations to succeed,
          // the record being updated or deleted needs to exist
          // and it should NOT have been modified by another user.
          if not Found then
            raise Exception.Create('Record to be modified was NOT found');
          end;

        // We are now on the right record to execute the action ...
        if chg.Action=rds_Delete then
          ds.Delete
        else
          begin
          if chg.Action=rds_Insert then
            ds.Append;
          { Update all fields using data from "NewRow" ... }
          for i:=0 to chg.NewRow.FieldCount-1 do
            begin
            fname:=chg.NewRow.FieldName[i];
            // The easiest way to copy a complete RTC object is ...
            ds.asCode[fname]:=chg.NewRow.asCode[fname];
            end;
          end;

        { <<<--------------------
           The code above is equivalent to executing an SQL statement on a Database
           to perform the appropriate action (INSERT, UPDATE or DELETE) by using
           "OldRow" values for the "WHERE" clause and "NewRow" for "INSERT/SET". }

        // Move to the next "Action" row ...
        chg.Next;
        end;
    finally
      // Done, release the temporary "TRtcDataSetChanges" instance ...
      chg.Free;
      end;

    { Since we have linked out TRtcMemDataSet to a DBGrid, but changes made directly
      to the TRtcDataSet object contained inside the TRtcMemDataSet are not automatically
      reflected in DB-aware components, we need to use "Refresh" for the DBGrid to get updated.

      This is VERY SPECIFIC to this particular example and is NOT something
      you would normally do in a real Server working with a real Database! }
    RtcMemDataSet1.Refresh;

    { Our example Fishfact Client expects us to send a Boolean "TRUE" as a Result
      if everything was OK, which is what we will do here ... }
    Result.asBoolean:=True;
    end
  else
    raise Exception.Create('Table "'+Param.asText['table']+'" not found on this Server.');
  end;

end.
