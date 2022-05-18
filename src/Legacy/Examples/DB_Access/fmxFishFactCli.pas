{
  @html(<b>)
  RTC Fish Facts FMX Client Demo Project
  @html(</b>)
  - Copyright 2004-2013 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This Project shows how to write a FMX Client which can use DB-aware components.
  It is designed to work with "FishFactServer" and "FishFactServer2" Projects.

  This is a cross-platform FMX Project using a TRtcMemDataSet as in-memory DataSet.
}
unit fmxFishFactCli;

interface

{$include rtcDefs.inc}

uses
  System.SysUtils, System.Types, System.UITypes,
  System.Classes, System.Variants,

  rtcSystem, rtcDB, rtcFunction, rtcDataCli,
  rtcCliModule, rtcInfo, rtcConn, rtcHttpCli,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Objects, Fmx.Bind.DBEngExt, Fmx.Bind.Editors,
  Fmx.Bind.DBLinks, FMX.Layouts, FMX.Grid,
  Fmx.Bind.Navigator, FMX.Edit, FMX.Memo,

{$IFDEF IDE_XE3up}
  FMX.StdCtrls, 
{$ENDIF}
{$IFDEF IDE_XE8up}
  System.Rtti, System.Bindings.Outputs, 
  FMX.Grid.Style, FMX.ScrollBox,
  FMX.Controls.Presentation, 
  Data.Bind.Controls,
{$ENDIF}

  Data.Bind.EngExt, Data.Bind.Components,
  Data.Bind.DBLinks, Data.DB,
  Data.Bind.DBScope;

type
  TForm2 = class(TForm)
    BindScopeDB1: TBindScopeDB;
    BindNavigator1: TBindNavigator;
    RtcMemDataSet1: TRtcMemDataSet;
    DataSource1: TDataSource;
    StringGrid1: TStringGrid;
    BindingsList1: TBindingsList;
    DBLinkStringGrid11: TBindDBGridLink;
    Memo1: TMemo;
    DBLinkMemo1Notes1: TBindDBMemoLink;
    ImageControl1: TImageControl;
    DBLinkImageControl1Graphic1: TBindDBImageLink;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Label1: TLabel;
    eAddr: TEdit;
    ePort: TEdit;
    Label2: TLabel;
    btnRefresh: TButton;
    RtcHttpClient1: TRtcHttpClient;
    RtcClientModule1: TRtcClientModule;
    RtcResult1: TRtcResult;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    OpenDialog1: TOpenDialog;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure RtcMemDataSet1DataChange(Sender: TObject);
    procedure RtcResult1Return(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure RtcResult1RequestAborted(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure eAddrChange(Sender: TObject);
    procedure ImageControl1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
  begin
  { Server Address and Port will be copied from eAddr and ePort fields on the Form,
    which are currently set to "localhost" and "81", which means that the Client
    will expect the Server to run on the same PC and use Port 81.

    To test this Client with a Server running on a different PC,
    either change the values of eAddr and ePort fields on the Form
    before compiling, or manually change these entries at runtime
    and click the "Refresh" button after you get the error message
    about communication problems with the Server. }

  RtcHttpClient1.ServerAddr:=RtcString(eAddr.Text);
  RtcHttpClient1.ServerPort:=RtcString(ePort.Text);

  // We want the user to enter the Server Address and Port, then click "Load DataSet"
  end;

procedure TForm2.ImageControl1Click(Sender: TObject);
  var
    bs:TStream;
    f:TFileStream;
  begin
  if RtcMemDataSet1.Active then
    if OpenDialog1.Execute then
      begin
      RtcMemDataSet1.Edit;
      bs:=RtcMemDataSet1.CreateBlobStream(RtcMemDataSet1.FieldByName('Graphic'),bmWrite);
      try
        f:=TFileStream.Create(OpenDialog1.FileName,fmOpenRead);
        try
          bs.CopyFrom(f,f.Size);
        finally
          f.Free;
          end;
      finally
        bs.Free;
        end;
      RtcMemDataSet1.Post;
      end;
  end;

procedure TForm2.btnRefreshClick(Sender: TObject);
  begin
  RtcClientModule1.Prepare('select'); // call the "select" function on the Server
  RtcClientModule1.Param.asText['table']:='biolife'; // with "biolife" as "table" parameter
  RtcClientModule1.Call(RtcResult1); // make the call non-blocking, result will be received in "RtcResult1" component events
  end;

procedure TForm2.eAddrChange(Sender: TObject);
  begin
  if not Visible then Exit;

  RtcHttpClient1.Disconnect;
  RtcHttpClient1.ServerAddr:=RtcString(eAddr.Text);
  RtcHttpClient1.ServerPort:=RtcString(ePort.Text);
  end;

procedure TForm2.RtcMemDataSet1DataChange(Sender: TObject);
  var
    data:TRtcValue;
  begin
  { With its "TrackChanges" property set to TRUE, every time the user makes
    changes and "submits" them (Post or Delete), this event will be called.
    And from here, we will have access to the "ExtractChanges" method
    which gives us all the data we need to execute the same operations
    on the Database at the Server side. Using a remote function call,
    we will be sending that information to the Server asynchronously ... }
  data:=RtcMemDataSet1.ExtractChanges;
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
      { We have received fresh data from the Server,
        so we will fill our in-memory DataSet with it,
        replacing anything that might have been in there. }

      { Because we are using "LiveBindings", we need to remove the DataSource
        before updating our in-memory DataSet, or our assignment will be registered
        as modifications to the underlying Data and result in "Change" events. }
      BindScopeDB1.DataSource:=nil;
      try
        RtcMemDataSet1.asObject:=Result.asDataSet;
        Result.Extract;
        RtcMemDataSet1.Active:=True;
      finally
        BindScopeDB1.DataSource:=DataSource1;
      end;
      end
    else
      begin
      { Somethind is wrong,
        we did NOT receive a DataSet after Select. }
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

end.
