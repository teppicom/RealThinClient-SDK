unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, rtcDataSrv, rtcConn, rtcHttpSrv, rtcInfo, rtcSystem;

type
  TForm1 = class(TForm)
    DualLink8080and1975: TRtcDualDataServerLink;
    QuadLinkToAll: TRtcDualDataServerLink;
    DualLink80and443: TRtcDualDataServerLink;
    HttpServer8080: TRtcHttpServer;
    HttpServer1975: TRtcHttpServer;
    HttpServer443: TRtcHttpServer;
    HttpServer80: TRtcHttpServer;
    DataProviderForAll: TRtcDataProvider;
    DataProviderFor80and443: TRtcDataProvider;
    DataProviderFor8080and1975: TRtcDataProvider;
    DataProviderFor80: TRtcDataProvider;
    procedure DataProviderForAllCheckRequest(Sender: TRtcConnection);
    procedure FormCreate(Sender: TObject);
    procedure DataProviderFor80and443CheckRequest(Sender: TRtcConnection);
    procedure DataProviderFor8080and1975CheckRequest(Sender: TRtcConnection);
    procedure DataProviderFor80CheckRequest(Sender: TRtcConnection);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
  begin
  HttpServer80.Listen;
  HttpServer443.Listen;
  HttpServer8080.Listen;
  HttpServer1975.Listen;
  end;

procedure TForm1.FormDestroy(Sender: TObject);
  begin
  HttpServer80.StopListenNow;
  HttpServer443.StopListenNow;
  HttpServer8080.StopListenNow;
  HttpServer1975.StopListenNow;
  end;

procedure TForm1.DataProviderForAllCheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.FileName='/' then
      begin
      Accept;
      Write('ALL Servers, you are on Server '+ServerPort);
      end;
  end;

procedure TForm1.DataProviderFor80and443CheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.FileName='/a' then
      begin
      Accept;
      Write('80 and 443 Servers, you are on Server '+ServerPort);
      end;
  end;

procedure TForm1.DataProviderFor8080and1975CheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.FileName='/b' then
      begin
      Accept;
      Write('8080 and 1975 Servers, you are on Server '+ServerPort);
      end;
  end;

procedure TForm1.DataProviderFor80CheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.FileName='/c' then
      begin
      Accept;
      Write('80 Server, you are on Server '+ServerPort);
      end;
  end;

end.
