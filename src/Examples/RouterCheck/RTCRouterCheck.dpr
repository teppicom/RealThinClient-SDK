program RTCRouterCheck;

uses
  Forms,
  Unit1 in 'Unit1.pas' {RtcRouterCheckForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRtcRouterCheckForm, RtcRouterCheckForm);
  Application.Run;
end.
