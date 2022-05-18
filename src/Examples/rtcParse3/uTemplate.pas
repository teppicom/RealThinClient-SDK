unit uTemplate;
(*
  On the first pass, only the instructions are put into the form.
  On succeeding passes, a name from a short list is presented as
  part of the form. The named text fields of the form can be accessed
  via Request.Params, although this demo doesn't make much use of
  that fact.
*)
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  rtcDataSrv, rtcInfo, rtcConn, rtcHttpSrv, rtcSystem;

type
  TfrmRtcParseDemo = class(TForm)
    RtcHttpServer1: TRtcHttpServer;
    RtcDataProvider1: TRtcDataProvider;

    procedure FormCreate(Sender: TObject);
    procedure RtcDataProvider1CheckRequest(Sender: TRtcConnection);
    procedure RtcDataProvider1DataReceived(Sender: TRtcConnection);
  private
    templatePath: String;
  public
    { Public declarations }
  end;

var
  frmRtcParseDemo: TfrmRtcParseDemo;

implementation

uses
  rtcParse;

{$R *.DFM}

procedure TfrmRtcParseDemo.FormCreate(Sender: TObject);
begin
  // set template path and start server
  templatePath := ExtractFilePath(Application.ExeName) + 'templates/';
  RtcHttpServer1.Listen;
end;

procedure TfrmRtcParseDemo.RtcDataProvider1CheckRequest(Sender: TRtcConnection);
begin
  // we want to respond to all root requests
  with Sender as TRtcDataServer do
    if Request.FileName = '/' then Accept;
end;

procedure TfrmRtcParseDemo.RtcDataProvider1DataReceived(Sender: TRtcConnection);
const
  FirstNames: array[1..3] of String =
    ('Mickey',
     'Donald',
     'Goofy');
  LastNames: array[1..3] of String =
    ('Mouse',
     'Duck',
     'Dog');
var
  Srvr: TrtcDataServer;
  page: Integer;
  body: TRtcParse;
begin
  Srvr := TrtcDataServer(Sender);
  if Srvr.Request.Complete then
    begin
    // Load params from posted data
    Srvr.Request.Params.AddText(Srvr.Read);
    // get the page # to be displayed
    page := StrToIntDef(Srvr.Request.Params['NextName'], 0);
    // create an RtcParse object for the page body
    body := TRtcParse.Create(templatePath + 'body.htm');
    TRY
      // set the page title and header
      body['title'] := 'RtcParse Demo Home';
      body['instruct'] := '<p class="instruct">';
      body['instruct'] := body['instruct'] + 'This is the RtcParse form demo page.<br /><br />';
      body['instruct'] := body['instruct'] + 'Click the "submit" button to see dynamic content in a form.';
      body['instruct'] := body['instruct'] + '</p>';
      // No update unless page # is valid
      If page IN [ 1..3 ] then begin
        body['first'] := FirstNames[page];
        body['last'] := LastNames[page];
        end;
      // roll-over for page #
      If page < 3
        then body['nextname'] := intToStr(page+1)
        else body['nextname'] := '1';
      Srvr.Write(body.OutPut);
    FINALLY
      body.Free;
    END
    end;
end;

end.

