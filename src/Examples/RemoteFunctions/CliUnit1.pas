unit CliUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, rtcFunction, rtcDataCli, rtcCliModule, rtcInfo,
  rtcConn, rtcHttpCli, rtcSystem;

{$include rtcDefs.inc}

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    RtcHttpClient1: TRtcHttpClient;
    RtcClientModule1: TRtcClientModule;
    Label1: TLabel;
    Label2: TLabel;
    xMultiThr: TCheckBox;
    xBlocking: TCheckBox;
    xProxy: TCheckBox;
    xWinHttp: TCheckBox;
    xFMT: TListBox;
    Label3: TLabel;
    xMode: TListBox;
    Label4: TLabel;
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RtcHttpClient1Connect(Sender: TRtcConnection);
    procedure RtcHttpClient1Disconnect(Sender: TRtcConnection);
    procedure RtcClientModule1BeginRequest(Sender: TRtcConnection);
    procedure RtcClientModule1ConnectLost(Sender: TRtcConnection);
    procedure RtcClientModule1EncryptNotSupported(Sender: TRtcConnection);
    procedure RtcClientModule1EncryptRequired(Sender: TRtcConnection);
    procedure RtcClientModule1EncryptWrongKey(Sender: TRtcConnection);
    procedure RtcClientModule1RepostCheck(Sender: TRtcConnection);
    procedure RtcClientModule1ResponseAbort(Sender: TRtcConnection);
    procedure RtcClientModule1ResponseDone(Sender: TRtcConnection);
    procedure RtcClientModule1ResponseError(Sender: TRtcConnection);
    procedure RtcClientModule1ResponseReject(Sender: TRtcConnection);
    procedure RtcClientModule1ResultError(Sender: TRtcConnection; Data,
      Result: TRtcValue; E: Exception);
    procedure xMultiThrClick(Sender: TObject);
    procedure xBlockingClick(Sender: TObject);
    procedure xProxyClick(Sender: TObject);
    procedure xWinHttpClick(Sender: TObject);
    procedure xFMTClick(Sender: TObject);
    procedure xModeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
  begin
  if Key=#13 then
    begin
    Edit1.SelectAll;
    Key:=#0;

    { This is the easiest way to use remote functions with RTC SDK:
      Using "Prepare" to prepare and using the "Execute" method to
      execute the remote function call, wait for the result and
      make sure the Result object is freed automatically the next
      time the "Execute" method is being used, so we do not need
      to free the result manually ... }
    with RtcClientModule1 do
      begin
      Prepare('hello');
      Param.asText['name']:=Edit1.Text;
      // Param.asInteger['test']:=100+Random(100); // test sending 2nd parameter (not used by the Server)
      try
        with Execute(True) do // execute remote function (TRUE = Free Result before next call).
          Memo1.Lines.Add(asText); // print out the result
      except
        on E:Exception do
          Memo1.Lines.Add('! '+E.Message);
        end;
      end;

    { Alternatively, we could make the "Execute" call with "FALSE" as
      the 1st parameter "Execute(False)" if we wanted to keep the Result
      object for ourself and free it manually when we do not need it ... }

    (*
      var myRes:TRtcValue; // we will need a variable to hold the result object

      // Prepare the remote call ...
      Prepare('hello');
      Param.asText['name']:=Edit1.Text;

      // Execute the remote call and get the result ...
      myRes:=Execute(False); // using FALSE as "AutoFreeResult" parameter

      // we have the result, let's use it ...
      try
        Memo1.Lines.Add(myRes.asText);
      finally
        // when finished, free our result object
        myRes.Free;
        end;
      end;
    *)

    end;
  end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  // Close the connection before closing the App
  RtcHttpClient1.DisconnectNow;
  end;

procedure TForm1.RtcHttpClient1Connect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcHttpClient1Connect)
  else
    Label1.Caption:='Connected';
  end;

procedure TForm1.RtcHttpClient1Disconnect(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcHttpClient1Disconnect)
  else
    Label1.Caption:='NOT Connected';
  end;

procedure TForm1.RtcClientModule1BeginRequest(Sender: TRtcConnection);
  begin
  Memo1.Lines.Add('Begin Request');
  end;

procedure TForm1.RtcClientModule1ConnectLost(Sender: TRtcConnection);
  begin
  Memo1.Lines.Add('Connect Lost');
  end;

procedure TForm1.RtcClientModule1EncryptNotSupported(
  Sender: TRtcConnection);
  begin
  Memo1.Lines.Add('Encryption not supported');
  end;

procedure TForm1.RtcClientModule1EncryptRequired(Sender: TRtcConnection);
  begin
  Memo1.Lines.Add('Encryption required');
  end;

procedure TForm1.RtcClientModule1EncryptWrongKey(Sender: TRtcConnection);
  begin
  Memo1.Lines.Add('Encryption wrong key');
  end;

procedure TForm1.RtcClientModule1RepostCheck(Sender: TRtcConnection);
  begin
  Memo1.Lines.Add('Repost Check');
  end;

procedure TForm1.RtcClientModule1ResponseAbort(Sender: TRtcConnection);
  begin
  Memo1.Lines.Add('Respone Abort');
  end;

procedure TForm1.RtcClientModule1ResponseDone(Sender: TRtcConnection);
  begin
  Memo1.Lines.Add('Respone Done');
  end;

procedure TForm1.RtcClientModule1ResponseError(Sender: TRtcConnection);
  begin
  Memo1.Lines.Add('Respone Error');
  end;

procedure TForm1.RtcClientModule1ResponseReject(Sender: TRtcConnection);
  begin
  Memo1.Lines.Add('Respone Reject');
  end;

procedure TForm1.RtcClientModule1ResultError(Sender: TRtcConnection; Data,
  Result: TRtcValue; E: Exception);
  begin
  Memo1.Lines.Add('Result Error');
  end;

procedure TForm1.xMultiThrClick(Sender: TObject);
  begin
  RtcHttpClient1.DisconnectNow(False);
  RtcHttpClient1.MultiThreaded:=xMultiThr.Checked;
  end;

procedure TForm1.xBlockingClick(Sender: TObject);
  begin
  RtcHttpClient1.DisconnectNow(False);
  RtcHttpClient1.Blocking:=xBlocking.Checked;
  end;

procedure TForm1.xProxyClick(Sender: TObject);
  begin
  RtcHttpClient1.DisconnectNow(False);
  RtcHttpClient1.UseProxy:=xProxy.Checked;
  end;

procedure TForm1.xWinHttpClick(Sender: TObject);
  begin
  RtcHttpClient1.DisconnectNow(False);
  RtcHttpClient1.UseWinHTTP:=xWinHttp.Checked;
  end;

procedure TForm1.xFMTClick(Sender: TObject);
  var
    ch:Char;
  begin
  RtcClientModule1.DataFormat:=TRtcDataFormat(xFMT.ItemIndex);
  if Edit1.Text<>'' then
    begin
    ch:=#13;
    Edit1KeyPress(Edit1,ch);
    end;
  end;

procedure TForm1.xModeClick(Sender: TObject);
  var
    ch:Char;
  begin
  RtcClientModule1.DataReqMode:=TRtcDataReqMode(xMode.ItemIndex);
  if Edit1.Text<>'' then
    begin
    ch:=#13;
    Edit1KeyPress(Edit1,ch);
    end;
  end;

end.
