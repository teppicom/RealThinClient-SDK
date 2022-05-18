unit rtcGatewayProvider;

interface

{$include rtcDefs.inc}

uses
  SysUtils,
  Classes,

  rtcSystem,
  rtcInfo,
  rtcConn,
  rtcDataSrv,

  rtcGateSrv;

type
  TGateway_Provider = class(TDataModule)
    ServerLink: TRtcDataServerLink;
    RtcGateway: TRtcGateway;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetGatewayProvider:TGateway_Provider;

implementation

{$R *.dfm}

var
  Gateway_Provider: TGateway_Provider;

function GetGatewayProvider:TGateway_Provider;
  begin
  if not assigned(Gateway_Provider) then
    TGateway_Provider.Create(nil);
  Result:=Gateway_Provider;
  end;

procedure TGateway_Provider.DataModuleCreate(Sender: TObject);
  begin
  Gateway_Provider:=self;
  end;

procedure TGateway_Provider.DataModuleDestroy(Sender: TObject);
  begin
  Gateway_Provider:=nil;
  end;

initialization
finalization
if assigned(Gateway_Provider) then
  begin
  Gateway_Provider.Free;
  Gateway_Provider:=nil;
  end;
end.
