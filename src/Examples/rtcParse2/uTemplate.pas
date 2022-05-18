unit uTemplate;

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
  days: array[1..7] of String =
    ('Monday',
     'Tuesday',
     'Wednesday',
     'Thursday',
     'Friday',
     'Saturday',
     'Sunday');
var
  page: Integer;
  count: Integer;
  body: TRtcParse;
  table: TRtcParse;
  tableRow: TRtcParse;
begin
  with Sender as TRtcDataServer do
    if Request.Complete then
      begin
      // get the page to display
      page := StrToIntDef(Request.Query['p'], 0);

      // create an RtcParse object for the page body
      body := TRtcParse.Create(templatePath + 'body.htm');
      try
        // set the page title and header
        body['title'] := 'RtcParse Demo ';
        body['header'] := 'Welcome to the RtcParse Demo';

        case page of
          1, 2, 3:
            begin
            Randomize;

            // create an RtcParse object for the table
            table := TRtcParse.Create(templatePath + 'table.htm');
            try
              table['caption'] := 'Webstats for week: ' + Request.Query['p'];
              table['table_row'] := '';

              // create an RtcParse object for the table row
              tableRow := TRtcParse.Create(templatePath + 'table_row.htm');
              try
                for count := 1 to 7 do
                  begin
                    tableRow['day'] := days[count];
                    tableRow['total_pageviews'] := IntToStr(Random(1500)*3+10000);
                    tableRow['unique_pageviews'] := IntToStr(Random(1000)*3+5000);

                    // append row output
                    table['table_row'] := table['table_row'] + tableRow.Output;
                  end;
              finally
                tableRow.Free;
                end;
            finally
              body['table'] := table.Output;
              table.Free;
              end;
            end;

          else
            begin
            body['title'] := body['title'] + 'Home';
            body['instruct'] := '<p class="instruct">';
            body['instruct'] := body['instruct'] + 'This is the RtcParse demo home page.<br /><br />';
            body['instruct'] := body['instruct'] + 'Please select from the menu above to view ';
            body['instruct'] := body['instruct'] + 'each of the tables. They are dynamically ';
            body['instruct'] := body['instruct'] + 'generated with random data and should be ';
            body['instruct'] := body['instruct'] + 'different with each visit.';
            body['instruct'] := body['instruct'] + '</p>';
            end;
          end;
      finally
        // write final content to brower
        Write(body.Output);
        body.Free;
        end;
      end;
end;

end.
