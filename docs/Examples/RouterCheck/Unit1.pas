unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, rtcTypes, rtcInfo;

type
  TRtcRouterCheckForm = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    eEnd: TEdit;
    Memo1: TMemo;
    eStart: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RtcRouterCheckForm: TRtcRouterCheckForm;

implementation

{$R *.dfm}

procedure TRtcRouterCheckForm.Button1Click(Sender: TObject);
  var
    fname:String;
    s,data:RtcString;
    cnt,p1,p2:integer;
    f:TextFile;
    barr:RtcByteArray;
    i,min,max,count,maxfound:integer;
  begin
  min:=StrToInt(eStart.Text);
  max:=StrToInt(eEnd.Text);
  maxfound:=0;
  count:=max-min+1;
  if OpenDialog1.Execute then
    begin
    Screen.Cursor:=crHourGlass;

    fname:=OpenDialog1.FileName;
    SetLength(barr,count);
    for i:=0 to count-1 do
      barr[i]:=0;
    AssignFile(f,fname);
    Reset(f);
    while not EOF(f) do
      begin
      Readln(f,data);
      p1:=Pos('C. START',data);
      if p1>0 then
        begin
        p2:=Pos('] ',data);
        s:=Copy(data,p2+2,p1-p2-2);
        i:=StrToInt(s);
        if (i>=min) and (i<=max) then
          barr[i-min]:=barr[i-min] or 1;
        if i>maxfound then
          maxfound:=i;
        end
      else
        begin
        p1:=Pos('C. Destroy',data);
        if p1>0 then
          begin
          p2:=Pos('] ',data);
          s:=Copy(data,p2+2,p1-p2-2);
          i:=StrToInt(s);
          if (i>=min) and (i<=max) then
            barr[i-min]:=barr[i-min] or 2;
          if i>maxfound then
            maxfound:=i;
          end
        else
          begin
          p1:=Pos('S. Destroy',data);
          if p1>0 then
            begin
            p2:=Pos('] ',data);
            s:=Copy(data,p2+2,p1-p2-2);
            i:=StrToInt(s);
            if (i>=min) and (i<=max) then
              barr[i-min]:=barr[i-min] or 4;
            if i>maxfound then
              maxfound:=i;
            end;
          end;
        end;
      end;
    CloseFile(f);

    cnt:=0;
    Memo1.Lines.Clear;
    for i:=0 to count-1 do
      if (barr[i] and 1=1) then
        begin
        if (barr[i] and 6 = 0) then
          begin
          Inc(cnt);
          Memo1.Lines.Add(IntToStr(i+min)+'S + '+IntToStr(i+min)+'C');
          end
        else if (barr[i] and 2 = 0) then
          begin
          Inc(cnt);
          Memo1.Lines.Add(IntToStr(i+min)+'C');
          end
        else if (barr[i] and 4 = 0) then
          begin
          Inc(cnt);
          Memo1.Lines.Add(IntToStr(i+min)+'S');
          end;
        end;

    Memo1.Lines.Add('Found '+IntToStr(maxfound)+' items, '+IntToStr(cnt)+' waiting.');

    SetLength(barr,0);

    Screen.Cursor:=crDefault;
    end;
  end;

end.
