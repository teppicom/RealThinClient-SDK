unit iosUnit2;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,

  FMX_Types, FMX_Controls, FMX_Forms,
  FMX_Dialogs, FMX_Edit,

  rtcSystem, rtcInfo, rtcConn,
  rtcDataCli, rtcHttpCli,
  rtcCliModule, rtcFunction,

  DB, rtcDB;

type
  TForm2 = class(TForm)
    btnCall: TButton;
    RtcHttpClient1: TRtcHttpClient;
    RtcClientModule1: TRtcClientModule;
    Label1: TLabel;
    RtcResult1: TRtcResult;
    xBlocking: TCheckBox;
    xMultiThreaded: TCheckBox;
    ePort: TEdit;
    eAddr: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    ImageControl1: TImageControl;
    btnPrior: TButton;
    btnNext: TButton;
    btnFirst: TButton;
    btnLast: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    btnUpdate: TButton;
    MyDataSet: TRtcMemDataSet;
    btnCancel: TButton;
    procedure btnCallClick(Sender: TObject);
    procedure RtcResult1RequestAborted(Sender: TRtcConnection;
      Data, Result: TRtcValue);
    procedure RtcResult1Return(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure eAddrChange(Sender: TObject);
    procedure btnPriorClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure MyDataSetDataChange(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateBitmap;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

procedure TForm2.btnCallClick(Sender: TObject);
  begin
  MyDataSet.Active:=False;

  RtcHttpClient1.ServerAddr := RtcString(eAddr.Text);
  RtcHttpClient1.ServerPort := RtcString(ePort.Text);
  RtcHttpClient1.Blocking := xBlocking.IsChecked;
  RtcHttpClient1.MultiThreaded := xMultiThreaded.IsChecked;

  Label1.Text := 'Calling ' + RtcHttpClient1.ServerAddr + ' on ' + RtcHttpClient1.ServerPort;
  RtcClientModule1.Prepare('select');
  RtcClientModule1.Param.asText['table'] := 'biolife';
  RtcClientModule1.Call(RtcResult1);
  end;

procedure TForm2.btnCancelClick(Sender: TObject);
  begin
  if MyDataSet.State in [dsEdit,dsInsert] then
    begin
    MyDataSet.Cancel;
    UpdateBitmap;
    end;
  end;

procedure TForm2.btnFirstClick(Sender: TObject);
  begin
  MyDataSet.First;
  UpdateBitmap;
  end;

procedure TForm2.btnNextClick(Sender: TObject);
  begin
  MyDataSet.Next;
  if MyDataSet.EOF then
    MyDataSet.Last;
  UpdateBitmap;
  end;

procedure TForm2.btnPriorClick(Sender: TObject);
  begin
  MyDataSet.Prior;
  if MyDataSet.BOF then
    MyDataSet.First;
  UpdateBitmap;
  end;

procedure TForm2.btnUpdateClick(Sender: TObject);
  begin
  if MyDataSet.State in [dsEdit,dsInsert] then
    begin
    MyDataSet.Post;
    UpdateBitmap;
    end;
  end;

procedure TForm2.btnLastClick(Sender: TObject);
  begin
  MyDataSet.Last;
  UpdateBitmap;
  end;

procedure TForm2.eAddrChange(Sender: TObject);
  begin
  RtcHttpClient1.DisconnectNow;
  end;

procedure TForm2.Edit1Change(Sender: TObject);
  begin
  MyDataSet.Edit;
  MyDataSet.FieldByName('Category').asWideString := Edit1.Text;
  end;

procedure TForm2.Edit2Change(Sender: TObject);
  begin
  MyDataSet.Edit;
  MyDataSet.FieldByName('Common_Name').asWideString := Edit2.Text;
  end;

procedure TForm2.MyDataSetDataChange(Sender: TObject);
  var
    Data: TRtcValue;
  begin
  { With its "TrackChanges" property set to TRUE, every time the user makes
    changes and "submits" them (Post or Delete), this event will be called.
    And from here, we will have access to the "ExtractChanges" method
    which gives us all the data we need to execute the same operations
    on the Database at the Server side. Using a remote function call,
    we will be sending that information to the Server asynchronously ... }
  Data := MyDataSet.ExtractChanges;
  if assigned(Data) then
    begin
    RtcClientModule1.Prepare('submit');
    // We will be calling the "submit" function on the Server
    RtcClientModule1.Param.asText['table'] := 'biolife';
    // "table" parameter is "biolife"
    RtcClientModule1.Param.asObject['change_data'] := Data;
    // "change_data" parameter is the result of "ExtractChanges"
    RtcClientModule1.Call(RtcResult1);
    // Make the remote call, sending the result to the "RtcResult1Return" event
    { Because we are using "Call" (non-blocking) instead of "Execute" (blocking),
      we won't have access to the Result from the Server here, but in the "RtcResult1Return" event (below).
      Should something go wrong during communication, "RtcResultRequestAborted" event will be called. }
    end;
  end;

procedure TForm2.RtcResult1RequestAborted(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  UpdateBitmap;

  Label1.Text := 'Request Aborted.';
  end;

procedure TForm2.RtcResult1Return(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if Result.isType=rtc_Exception then
    ShowMessage('Server-side Exception: '+#13#10+Result.asException)
  else if Data.asFunction.FunctionName='select' then
    begin
    if Result.isType = rtc_DataSet then
      begin
      { We have received fresh data from the Server,
        so we will fill our in-memory DataSet with it,
        replacing anything that might have been in there. }
      MyDataSet.asObject := Result.asDataSet;
      Result.Extract;

      MyDataSet.Active := True;
      MyDataSet.First;

      UpdateBitmap;
      end
    else
      begin
      UpdateBitmap;
      Label1.Text := 'No dataset received.';
      end;
    end
  else if Data.asFunction.FunctionName = 'submit' then
    if (Result.isType=rtc_Boolean) and Result.asBoolean then
      Label1.Text := 'Server updated.'
    else
      Label1.Text := 'Error updating Server.';
  end;

procedure TForm2.UpdateBitmap;
  var
    bs: TStream;
  begin
  if MyDataSet.Active then
    begin
    if MyDataSet.FieldByName('Graphic').IsBlob then
      begin
      Label1.Text := 'DataSet Received, showing Row ' +
        IntToStr(MyDataSet.RecNo) + ' of ' + IntToStr(MyDataSet.RecordCount);

      bs := MyDataSet.CreateBlobStream(MyDataSet.FieldByName('Graphic'), bmRead);
      try
        bs.Position := 0;
        ImageControl1.Bitmap.LoadFromStream(bs);
      finally
        bs.Free
        end;
      end
    else
      begin
      Label1.Text := 'DataSet Received, showing Row ' +
        IntToStr(MyDataSet.RecNo) + ' of ' + IntToStr(MyDataSet.RecordCount);
      if assigned(ImageControl1.Bitmap) then
        ImageControl1.Bitmap.Clear(TAlphaColorRec.White);
      end;
    Edit1.Text := MyDataSet.FieldByName('Category').asWideString;
    Edit2.Text := MyDataSet.FieldByName('Common_Name').asWideString;
    end
  else
    begin
    Label1.Text := 'No DataSet.';
    if assigned(ImageControl1.Bitmap) then
      ImageControl1.Bitmap.Clear(TAlphaColorRec.Black);
    Edit1.Text := '';
    Edit2.Text := '';
    end;
  end;

end.
