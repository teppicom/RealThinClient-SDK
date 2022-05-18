unit loFileCli;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Gauges,

  rtcSystem, rtcInfo, rtcLink;

type
  TloFileClient = class(TForm)
    Link: TRtcLinkedModule;
    eFileName: TEdit;
    Label1: TLabel;
    btnOpenFile: TBitBtn;
    OpenDialog1: TOpenDialog;
    lStatus: TLabel;
    btnUpload: TButton;
    Gauge1: TGauge;
    btnCancel: TButton;
    ePath: TEdit;
    Label2: TLabel;
    lActive: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOpenFileClick(Sender: TObject);
    procedure LinkSetProp(Sender: TObject; Param: TRtcObjectCall);
    procedure btnUploadClick(Sender: TObject);
    procedure LinkCallMethod(Sender: TObject; Param: TRtcObjectCall);
    procedure btnCancelClick(Sender: TObject);
    procedure LinkSetEvent(Sender: TObject; Param: TRtcObjectCall);
  private
    { Private declarations }
  public
    { Public declarations }
    MyFileName,
    RemoteFileName:String;
    MyFileSize, MyFileOut:int64;
    MyActive,MyTotal:integer;
  end;

implementation

{$R *.dfm}

procedure MakeFileClient(Sender:TObject; Param:TRtcObjectCall);
  begin
  { This is our global "FileClient" constructor, which is called
    when the Server requests to have a File Client created.
    This procedure is registered in this units "initialization" section
    and unregistered in the "finalization" section. }
  if Param.xName='FileClient' then
    TloFileClient.Create(nil).Show;
  end;

procedure TloFileClient.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
  { Make sure this Form is destroyed on close, so there will be no memory leaks. }
  Action:=caFree;
  end;

procedure TloFileClient.btnOpenFileClick(Sender: TObject);
  begin
  if OpenDialog1.Execute then
    eFileName.Text:=OpenDialog1.FileName;
  end;

procedure TloFileClient.btnUploadClick(Sender: TObject);
  begin
  MyFileName := eFileName.Text;
  RemoteFileName := ePath.Text +'\'+ ExtractFileName(MyFileName);
  MyFileSize := File_Size(MyFileName);
  if MyFileSize>=0 then
    begin
    lStatus.Caption:='Checking';
    MyFileOut:=0;
    if MyFileSize>0 then
      Gauge1.MaxValue:=MyFileSize
    else
      Gauge1.MaxValue:=1;
    Gauge1.Progress:=0;

    { Call the "Upload" method on the "File Server" instance }
    with Link.Param.newRecord do
      begin
      asText['name']:=RemoteFileName;
      asLargeInt['size']:=MyFileSize;
      end;
    Link.CallMethod('Upload');
    end;
  end;

procedure TloFileClient.btnCancelClick(Sender: TObject);
  begin
  { Call Event "Cancel" }
  Link.CallEvent('Cancel');
  end;

procedure TloFileClient.LinkSetProp(Sender: TObject; Param: TRtcObjectCall);
  begin
  { "SetProp" used on the "File Server" instance to change our properties }
  if Param.xName='Path' then
    ePath.Text:=Param.asText
  else if Param.xName='Active' then
    begin
    MyActive:=Param.asInteger;
    lActive.Caption:=IntToStr(MyActive)+'/'+IntToStr(MyTotal);
    end
  else if Param.xNAme='Total' then
    begin
    MyTotal:=Param.asInteger;
    lActive.Caption:=IntToStr(MyActive)+'/'+IntToStr(MyTotal);
    end;
  end;

procedure TloFileClient.LinkCallMethod(Sender: TObject; Param: TRtcObjectCall);
  begin
  { "CallMethod" used on the "File Server" instance }
  if Param.xName='Get' then
    begin
    if MyFileOut=0 then
      lStatus.Caption:='Uploading '+IntToStr(MyFileSize)+' bytes ...';
    Gauge1.Progress:=MyFileOut;

    { Call Method "Put" on the "File Server" instance }
    Link.Param.asString := Read_File(MyFileName, MyFileOut, Param.asLargeInt);
    MyFileOut := MyFileOut+length(Link.Param.asString);
    Link.CallMethod('Put');
    end
  else if Param.xName='Done' then
    begin
    if MyFileOut=MyFileSize then
      begin
      Gauge1.Progress:=Gauge1.MaxValue;
      lStatus.Caption:='Done. '+IntToStr(MyFileSize)+' bytes sent.';
      end
    else
      begin
      Gauge1.Progress:=MyFileOut;
      lStatus.Caption:='Cancelled';
      end;
    end
  else if Param.xName='FileExists' then
    begin
    if MessageDlg('File already exists on the Server.'#13#10+
                  'Local File Size = '+IntToStr(MyFileSize)+' bytes'#13#10+
                  'Server File Size = '+IntToStr(Param.asLargeInt)+' bytes'#13#10+
                  'Delete the File on Server?',
                mtWarning,[mbYes,mbNo],0)=mrYes then
      begin
      { Use BeginUpdate/EndUpdate to send everything in a single request }
      Link.BeginUpdate;
      try
        Link.CallMethod('Del');
        btnUpload.Click;
      finally
        Link.EndUpdate;
        end;
      end;
    end;
  end;

procedure TloFileClient.LinkSetEvent(Sender: TObject; Param: TRtcObjectCall);
  begin
  { "SetEvent" used on the "File Server" instance }
  if Param.xName='Cancel' then
    { Enable/Disable the "Cancel" button
      depending on the state of the "Cancel" event. }
    btnCancel.Enabled:=Param.asBoolean;
  end;

initialization
{ We will register a global constructor for our File Client class,
  allowing remote servers to create a File Client instance on demand.

  This is ONLY required if we want to allow the Server to create "File Client" instances remotely.
  If "TloFileClient" instances will ONLY be created manually on the Client side,
  there is no need to register a global constructor for our "TloFileClient" class.

  An alternative to using RegisterRtcObjectConstructor and registering a global
  constructor is to implement the "OnObjectCreate" event on the TRtcClientModule
  component, but that only works if we are using the TRtcClientModule for communication
  and this particular demo is designed to work with TRtcLocalObjectManager as well. }
RegisterRtcObjectConstructor('FileClient',MakeFileClient);
finalization
UnregisterRtcObjectConstructor('FileClient');
end.
