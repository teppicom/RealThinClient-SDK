{
  @html(<b>)
  RTC Fish Facts Client 2 Demo Project
  @html(</b>)
  - Copyright 2004-2013 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This Project shows how to write a Client which can use any in-memory TDataSet
  descendant to work with DB-aware components by connecting it to a TRtcDataSetMonitor.
  It is designed to work with "FishFactServer" and "FishFactServer2" Projects.

  This Project uses a TClientDataSet as in-memory Dataset, which is linked to a TRtcDataSetMonitor
  for capturing user changes and sending them to a Server. Since TRtcDataSetMonitor is designed
  to works with any TDataSet descendant component, this example Project could easily be modified
  to wotk with any other 3rd-party in-memory TDataSet component for Client-side data storage.
}

unit FishFactCli2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons,

  DBCtrls, Grids, DBGrids, DB, DBClient,

  rtcConn, rtcInfo, rtcDataCli, rtcHttpCli,
  rtcCliModule, rtcFunction, rtcDB, ExtDlgs, rtcSystem;

type
  TForm2 = class(TForm)
    DataSource1: TDataSource;
    RtcHttpClient1: TRtcHttpClient;
    RtcClientModule1: TRtcClientModule;
    RtcResult1: TRtcResult;
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    Panel2: TPanel;
    DBImage1: TDBImage;
    DBMemo1: TDBMemo;
    Splitter2: TSplitter;
    Splitter1: TSplitter;
    Panel3: TPanel;
    DBNavigator1: TDBNavigator;
    Panel5: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    eAddr: TEdit;
    ePort: TEdit;
    btnRefresh: TSpeedButton;
    ClientDataSet1: TClientDataSet;
    RtcDataSetMonitor1: TRtcDataSetMonitor;
    Label3: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure RtcResult1Return(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure RtcDataSetMonitor1DataChange(Sender: TObject);
    procedure RtcResult1RequestAborted(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure FormShow(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure eAddrChange(Sender: TObject);
    procedure DBImage1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormShow(Sender: TObject);
  begin
  { We will be emulating a click on the "Reload DataSet" button to load
    the initial data from the Server, immediately when our Client starts.

    Server Address and Port will be copied from eAddr and ePort fields on the Form,
    which are currently set to "localhost" and "81", which means that the Client
    will expect the Server to run on the same PC and use Port 81.

    To test this Client with a Server running on a different PC,
    either change the values of eAddr and ePort fields on the Form
    before compiling, or manually change these entries at runtime
    and click the "Refresh" button after you get the error message
    about communication problems with the Server. }

  RtcHttpClient1.ServerAddr:=eAddr.Text;
  RtcHttpClient1.ServerPort:=ePort.Text;

  btnRefresh.Click;
  end;

procedure TForm2.btnRefreshClick(Sender: TObject);
  begin
  RtcClientModule1.Prepare('select'); // call the "select" function on the Server
  RtcClientModule1.Param.asText['table']:='biolife'; // with "biolife" as "table" parameter
  RtcClientModule1.Call(RtcResult1); // make the call non-blocking, result will be received in "RtcResult1" component events
  end;

procedure TForm2.RtcDataSetMonitor1DataChange(Sender: TObject);
  var
    data:TRtcValue;
  begin
  { Every time the user makes changes to the DataSet linked to our
    RtcDataSetMonitor and "submits" them (Post or Delete), this event will be called.

    And from here, we will have access to the "ExtractChanges" method
    which gives us all the data we need to execute the same operations
    on the Database at the Server side. This data is identical to the
    data we would get from the "ExtractChanges" method of TRtcMemDataSet,
    so the Server implementation is independent of the Client implementation
    and the Client can use any TDataSet descendant component it wants.

    Using a remote function call, we will be sending the "ExtractChanges" data
    to the Server. In this example, we are using a non-blocking call (asynchronous). }

  data:=RtcDataSetMonitor1.ExtractChanges;
  if assigned(data) then
    begin
    RtcClientModule1.Prepare('submit'); // We will be calling the "submit" function on the Server
    RtcClientModule1.Param.asText['table']:='biolife'; // "table" parameter is "biolife"
    RtcClientModule1.Param.asObject['change_data']:=data; // "change_data" parameter is the result of "ExtractChanges"
    RtcClientModule1.Call(RtcResult1); // Make the remote call, sending the result to the "RtcResult1Return" event
    { Because we are using "Call" (non-blocking) instead of "Execute" (blocking),
      we won't have access to the Result from the Server here, but in the "RtcResult1Return" event (below).
      Should something go wrong during communication, "RtcResultRequestAborted" event will be called. }
    end;
  end;

procedure TForm2.RtcResult1Return(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  { Because we have used non-blocking remote function calls with a single TRtcResult object
    for all calls related to our DataSet, we have to check which function was called on the Server
    to know what the Result means. This is what our check for "Data.asFunction.FunctionName" does.

    We could also have used blocking remote function calls ("Execute" method instead of "Call"),
    in which case the code for this event would need to be somewhere after "Execute".

    But with blocking calls the user would be forced to wait for a response from the Server before 
    being able to continue working. On slow connections, that could pose a dilemma: (A) we could 
    allow the user to submit his changes in a larger chunk, with potential loss of a larger set of
    records in case of an error, or (B) we could force the user to wait for each record to be 
    submitted before he can continue on a new record, in which case the users could get frustrated 
    in case the Server gets under high load for a longer time period.

    By using non-blocking event-driven communication instead of blocking communication, we have
    the option to send each change immediately, without forcing the user to wait for a result
    before he can continue making more changes (insert/delete/edit records). As long as there
    are no conflicts between users (two or more users trying to make changes to the same records),
    this method will work a lot better than using blocking communication.

    The down-side of this non-blocking method is that - especially when the Server is under heavy load,
    problems on the Server resulting from our SUBMIT request could arrive back to the Client too late,
    for example once the Client has already submitted multiple new records, in which case the user would
    be forced to "refresh" his complete dataset from the Server to see what went wrong.

    Naturally, this is only one way of using the RTC SDK and you can always opt for working with
    blocking remote functions if you preffer waiting for a result after each SUBMIT instead. }

  if Data.asFunction.FunctionName='select' then
    begin
    if Result.isType=rtc_DataSet then
      begin
      { We have received fresh data from the Server, so we will fill our
        in-memory DataSet with it, replacing anything that might have been in there.

        In this example, we are using the TClientDataSet component included in Delphi,
        but the code below should also work for any other in-memory TDataSet descendant ... }

      // Disable DataSet Monitoring and visual Controls before populating the DataSet
      RtcDataSetMonitor1.Active:=False;
      ClientDataSet1.DisableControls;
      try
        // Copy field definitions from RTC DataSet to our in-memory Client DataSet
        RtcDataSetFieldsToDelphi(Result.asDataSet, ClientDataSet1);
        ClientDataSet1.CreateDataSet;
        // Copy all data Rows from RTC DataSet to our in-memory Client DataSet
        RtcDataSetRowsToDelphi(Result.asDataSet, ClientDataSet1);
      finally
        // Enable DataSet Monitoring and visual Controls afterwards
        try
          ClientDataSet1.EnableControls;
        finally
          RtcDataSetMonitor1.Active:=True;
          // Move to the 1st row to update visual Controls
          ClientDataSet1.First;
          end;
        end;
      end
    else
      begin
      { Somethind is wrong,
        we did NOT receive a DataSet after Select. }

      if Sender<>nil then // We should NOT use modal dialogs (ShowMessage) from the context of connection objects,
        PostInteractive; // so we HAVE TO EXIT the context of the connection object by using "PostInteractive".

      { Because we have used "PostInteractive" (above), this method was now called outside of the context
        of the "Sender" connection object and we can now safely use ShowMessage, showing a modal dialog. }
      if Result.isType=rtc_Exception then
        ShowMessage('Server-side exception after Select:'+#13#10+Result.asException)
      else
        ShowMessage('Unexpected Result after Select');
      end;
    end
  else if Data.asFunction.FunctionName='submit' then
    begin
    if (Result.isType<>rtc_Boolean) or // We are expecting a Boolean "TRUE" as a Result of any SUBMIT.
       (Result.asBoolean<>TRUE) then // If we have received anything else, it means trouble.
      begin
      { There was an error after trying to submit our changes to the Database,
        which probably means that someone has modified records we wanted to change
        before our changes have made it to the Server. Because this is usually a
        fatal error, since only one user should be allowed to work on a single record
        at a time or we could end up with an inconsistent state inside our Database,
        we will simply notify the user about this problem and let him decide what to do next. }

      if Sender<>nil then // We should NOT use modal dialogs (ShowMessage) from the context of connection objects,
        PostInteractive; // so we HAVE TO EXIT the context of the connection object by using "PostInteractive".

      { Because we have used "PostInteractive" (above), this method was now called outside of the context
        of the "Sender" connection object and we can now safely use ShowMessage to show a modal dialog. }
      if Result.isType=rtc_Exception then
        ShowMessage('Server-side exception after Submit:'+#13#10+Result.asException)
      else
        ShowMessage('Unexpected Result after Submit');
      end;
    end;
  end;

procedure TForm2.RtcResult1RequestAborted(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  ShowMessage('Error communicating with the Server.');
  end;

procedure TForm2.eAddrChange(Sender: TObject);
  begin
  RtcHttpClient1.Disconnect;
  RtcHttpClient1.ServerAddr:=eAddr.Text;
  RtcHttpClient1.ServerPort:=ePort.Text;
  end;

procedure TForm2.DBImage1Click(Sender: TObject);
  var
    bs:TStream;
    f:TFileStream;
  begin
  if ClientDataSet1.Active then
    if OpenPictureDialog1.Execute then
      begin
      ClientDataSet1.Edit;
      bs:=ClientDataSet1.CreateBlobStream(ClientDataSet1.FieldByName('Graphic'),bmWrite);
      try
        f:=TFileStream.Create(OpenPictureDialog1.FileName,fmOpenRead);
        try
          bs.CopyFrom(f,f.Size);
        finally
          f.Free;
          end;
      finally
        bs.Free;
        end;
      ClientDataSet1.Post;
      end;
  end;

end.
