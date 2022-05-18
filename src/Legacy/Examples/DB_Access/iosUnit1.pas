unit iosUnit1;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,

  FMX_Types, FMX_Controls, FMX_Forms,
  FMX_Dialogs, FMX_Edit,

  rtcSystem, rtcInfo, rtcConn,
  rtcDataCli, rtcHttpCli,
  rtcCliModule, rtcFunction;

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
    lCategory: TLabel;
    lCommonName: TLabel;
    procedure btnCallClick(Sender: TObject);
    procedure RtcResult1RequestAborted(Sender: TRtcConnection; Data,
      Result: TRtcValue);
    procedure RtcResult1Return(Sender: TRtcConnection; Data, Result: TRtcValue);
    procedure eAddrChange(Sender: TObject);
    procedure btnPriorClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MyDataSet:TRtcDataSet;
    procedure UpdateBitmap;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

procedure TForm2.btnCallClick(Sender: TObject);
  begin
  RtcHttpClient1.ServerAddr:=RtcString(eAddr.Text);
  RtcHttpClient1.ServerPort:=RtcString(ePort.Text);
  RtcHttpClient1.Blocking:=xBlocking.IsChecked;
  RtcHttpClient1.MultiThreaded:=xMultiThreaded.IsChecked;

  Label1.Text:='Calling '+RtcHttpClient1.ServerAddr+' on '+RtcHttpClient1.ServerPort;
  RtcClientModule1.Prepare('select');
  RtcClientModule1.Param.asText['table']:='biolife';
  RtcClientModule1.Call(RtcResult1);
  end;

procedure TForm2.btnFirstClick(Sender: TObject);
  begin
  if assigned(MyDataSet) then
    begin
    MyDataSet.First;
    UpdateBitmap;
    end;
  end;

procedure TForm2.btnNextClick(Sender: TObject);
  begin
  if assigned(MyDataSet) then
    begin
    MyDataSet.Next;
    if MyDataSet.EOF then
      MyDataSet.Last;
    UpdateBitmap;
    end;
  end;

procedure TForm2.btnPriorClick(Sender: TObject);
  begin
  if assigned(MyDataSet) then
    begin
    MyDataSet.Prior;
    if MyDataSet.BOF then
      MyDataSet.First;
    UpdateBitmap;
    end;
  end;

procedure TForm2.btnLastClick(Sender: TObject);
  begin
  if assigned(MyDataSet) then
    begin
    MyDataSet.Last;
    UpdateBitmap;
    end;
  end;

procedure TForm2.eAddrChange(Sender: TObject);
  begin
  RtcHttpClient1.Disconnect;
  end;

procedure TForm2.RtcResult1RequestAborted(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  FreeAndNil(MyDataSet);
  UpdateBitmap;

  Label1.Text:='Request Aborted.';
  end;

procedure TForm2.FormDestroy(Sender: TObject);
  begin
  FreeAndNil(MyDataSet);
  end;

procedure TForm2.RtcResult1Return(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if Result.isType=rtc_DataSet then
    begin
    FreeAndNil(MyDataSet);
    { Because we want to use the DataSet outside of this event,
      we need to assign it to another variable (MyDataSet) and
      then extract it from the Result object ... }
    MyDataSet:=Result.asDataSet;
    Result.Extract;

    MyDataSet.First;
    UpdateBitmap;
    end
  else
    begin
    FreeAndNil(MyDataSet);
    UpdateBitmap;

    Label1.Text:='No dataset received.';
    end;
  end;

procedure TForm2.UpdateBitmap;
  begin
  if assigned(MyDataSet) then
    begin
    if MyDataSet.isType['Graphic']=rtc_ByteStream then
      begin
      Label1.Text:='DataSet Received, showing Row '+IntToStr(MyDataSet.Row+1)+' of '+IntToStr(MyDataSet.RowCount);
      ImageControl1.Bitmap.LoadFromStream(MyDataSet.asByteStream['Graphic']);
      end
    else
      begin
      Label1.Text:='DataSet Received, showing Row '+IntToStr(MyDataSet.Row+1)+' of '+IntToStr(MyDataSet.RowCount);
      if assigned(ImageControl1.Bitmap) then
        ImageControl1.Bitmap.Clear(TAlphaColorRec.White);
      end;
    lCategory.Text:=MyDataSet.asWideString['Category'];
    lCommonName.Text:=MyDataSet.asWideString['Common_Name'];
    end
  else
    begin
    Label1.Text:='No DataSet.';
    if assigned(ImageControl1.Bitmap) then
      ImageControl1.Bitmap.Clear(TAlphaColorRec.Black);
    lCategory.Text:='';
    lCommonName.Text:='';
    end;
  end;

end.
