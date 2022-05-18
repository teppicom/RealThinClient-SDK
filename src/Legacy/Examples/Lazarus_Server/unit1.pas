unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  rtcSystem, rtcInfo, rtcConn, rtcDataSrv, rtcHttpSrv;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    RtcDataProvider1: TRtcDataProvider;
    RtcHttpServer1: TRtcHttpServer;
    procedure Form1Create(Sender: TObject);
    procedure RtcDataProvider1CheckRequest(Sender: TRtcConnection);
    procedure RtcDataProvider1DataReceived(Sender: TRtcConnection);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Form1Create(Sender: TObject);
begin
  RtcHttpServer1.Listen;
end;

procedure TForm1.RtcDataProvider1CheckRequest(Sender: TRtcConnection);
begin
  with TRtcDataServer(Sender) do
    if Request.FileName='/time' then
      Accept;
end;

procedure TForm1.RtcDataProvider1DataReceived(Sender: TRtcConnection);
begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      Write('Current time is:'+TimeToStr(Now));
end;

initialization
  {$I unit1.lrs}

end.

