unit Unit1;

interface

{$include rtcDeploy.inc}
{$include rtcDefs.inc}

{ To compile the project with StreamSec Tools 2.1+ components using demo SSL certificates,
  declare the StreamSecII compiler directive below, in your project or in the "rtcDeploy.inc" file.

  When StreamSecII compiler directive is declared,
  the "SSL" option for the Load Balancer will be enabled. }

{.$DEFINE StreamSecII}

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, 
  Dialogs, StdCtrls,

{$IFDEF StreamSecII}
  rtcSSecTest,
{$ENDIF}

  rtcSystem,
  rtcDataSrv,
  rtcInfo,
  rtcConn,
  rtcHttpSrv;

type
  TForm1 = class(TForm)
    RtcHttpServer1: TRtcHttpServer;
    RtcDataProvider1: TRtcDataProvider;
    Label1: TLabel;
    eSrvPort: TEdit;
    Label2: TLabel;
    btnListen: TButton;
    Label3: TLabel;
    eUploadFolder: TEdit;
    RtcDataProvider2: TRtcDataProvider;
    procedure FormCreate(Sender: TObject);
    procedure RtcDataProvider1CheckRequest(Sender: TRtcConnection);
    procedure RtcDataProvider1DataReceived(Sender: TRtcConnection);
    procedure btnListenClick(Sender: TObject);
    procedure RtcHttpServer1ListenStart(Sender: TRtcConnection);
    procedure RtcHttpServer1ListenStop(Sender: TRtcConnection);
    procedure RtcDataProvider2CheckRequest(Sender: TRtcConnection);
    procedure RtcDataProvider2DataReceived(Sender: TRtcConnection);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

(* General HTTP Server functionality *)

procedure TForm1.FormCreate(Sender: TObject);
  begin
  btnListenClick(nil);
  end;

procedure TForm1.btnListenClick(Sender: TObject);
  begin
  if RtcHttpServer1.isListening then
    RtcHttpServer1.StopListen
  else
    begin
  {$IFDEF StreamSecII}
    // Set to false to disable RC4 and enable AES-CBC.
    // ExpectOldBrowsers := false;

      //  Files will be IGNORED if they do not exist.
    AddServerRootCertFile(ExtractFilePath(AppFileName) + 'root.cer');
    AddServerPFXFile(ExtractFilePath(AppFileName) + 'server.pfx', 'abc');

    RtcHttpServer1.CryptPlugin := GetServerCryptPlugin;
    if Sender=nil then eSrvPort.Text:='443';
  {$ENDIF}
    RtcHttpServer1.ServerPort:=eSrvPort.Text;
    RtcHttpServer1.Listen;
    end;
  end;

procedure TForm1.RtcHttpServer1ListenStart(Sender: TRtcConnection);
  begin
  Caption:='Server listening ...';
  btnListen.Caption:='Stop Listen';
  end;

procedure TForm1.RtcHttpServer1ListenStop(Sender: TRtcConnection);
  begin
  Caption:='Idle (not listening)';
  btnListen.Caption:='Listen';
  end;

(* "/UPLOAD" request handler - showing how to accept file uploads from a browser *)

procedure TForm1.RtcDataProvider1CheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.FilePath.Equal(0,'UPLOAD') then
      Accept;
  end;

procedure TForm1.RtcDataProvider1DataReceived(Sender: TRtcConnection);
  var
    fname:String;
    cnt:integer;
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.Method='GET' then
      begin
      Write('<html><body><form enctype="multipart/form-data" method="post">');
      Write('Type some text, if you like:<br>');
      Write('<input type="text" name="textline" size="30"><br>');
      Write('Please specify a single file to upload:<br>');
      Write('<input type="file" name="onefile" size="40"><br>');
      Write('Please specify one or more files to upload:<br>');
      Write('<input type="file" name="morefiles" size="40" multiple><br>');
      Write('<input type="submit" value="Send">');
      Write('</form></body></html>');
      end
    else
      begin
      Request.Params.AddText(Read);
      if Request.Complete then
        begin
        Write('<html><body>');

        if Request.Params['textline']<>'' then
          Write('You typed this text: <br> '+Request.Params['textline']+'<br><br>');

        // Uploading a single file ...
        if Request.Params.IsFile('onefile') then
          begin
          if not DirectoryExists(eUploadFolder.Text) then
            CreateDir(eUploadFolder.Text);

          fname:=Request.Params['onefile'];
          if Request.Params.GetFile('onefile', eUploadFolder.Text+'\'+ExtractFileName(fname)) then
            Write('File "'+fname+'" was uploaded using the "single file" field.<br>')
          else
            Write('Error receiving File "'+fname+'".<br>');
          end
        else
          Write('Single file was NOT uploaded (OneFile parameter empty) <br>.');

        // Uploading one or more files ...
        if Request.Params.IsFile('morefiles') then
          begin
          if not DirectoryExists(eUploadFolder.Text) then
            CreateDir(eUploadFolder.Text);

          for cnt:=0 to Request.Params.ElementCount['morefiles']-1 do
            begin
            fname:=Request.Params.Element['morefiles',cnt];
            if Request.Params.GetFile('morefiles', eUploadFolder.Text+'\'+ExtractFileName(fname), cnt) then
              Write('File "'+fname+'" was uploaded using the "more files" field at ['+IntToStr(cnt)+'].<br>')
            else
              Write('Error receiving File "'+fname+'".<br>');
            end;
          end
        else
          Write('Multiple files were NOT uploaded (MoreFiles parameter empty).');
        
        Write('</body></html>');
        end;
      end;
    end;
  end;

(* "/GUESS" request handler - example using RTC Sessions, HTTP Cookies,
   HTML form generation and processing of user entry inside a Web Browser. *)

procedure TForm1.RtcDataProvider2CheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.FilePath.Equal(0,'GUESS') then
      Accept;
  end;

procedure TForm1.RtcDataProvider2DataReceived(Sender: TRtcConnection);
begin
with TRtcDataServer(Sender) do
  begin
  if Request.Method='GET' then
    begin
    if Request.Complete then
      begin
      Write('<html>');
      Write('<form name="frm" enctype="multipart/form-data" method="post">');
      Write('Guess a number between 0 and 9:<br>');
      Write('<input type="text" name="yourguess" size="30" selected><br>');
      Write('</form>');
      // we want our edit box to receive the focus ...
      Write('<script language="JavaScript">document.frm.yourguess.focus();</script>');
      Write('</html>');
      // Open a new Session for the user
      OpenSession;
      // we want to keep the session alive for at least 240 seconds (4 minutes)
      // to give the user 4 minutes time to think of a number
      Session.KeepAlive:=240;
      // generate a random number and store it in the session
      Session.asInteger['mynum']:=random(10);
      // send the Session ID to the browser
      Response.Cookie['sid']:=Session.ID;
      end;
    end
  else
    begin
    Request.Params.AddText(Read);
    if Request.Complete then
      begin
      if not FindSession(Request.Cookie['sid']) then
        begin
        Write('<html>Your session has expired.<br>');
        Write('Please click <a href="/guess">here</a> to try again.</html>');
        end
      else
        begin
        Session.asInteger['try']:=Session.asInteger['try']+1;
        if Session.asString['mynum']=Request.Params['yourguess'] then
          begin
          Write('<html>Congratulations! You have guessed it!<br>');
          Write('And you needed only '+Session.asString['try']+' tries.<br>');
          Write('Want to play again? <a href="/guess">Click here.</a></html>');
          end
        else
          begin
          Write('<html><form name="frm" enctype="multipart/form-data" method="post">');
          Write('Sorry, wrong. This was your guess nr. '+Session.asString['try']+'<br>');
          Write('What would be your next guess?<br>');
          Write('<input type="text" name="yourguess" size="30"><br>');
          Write('</form>');
          Write('<script language="JavaScript">document.frm.yourguess.focus();</script>');
          Write('</html>');
          end;
        end;
      end;
    end;
  end;
end;

end.
