unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  rtcSystem, rtcInfo;

type
  TForm1 = class(TForm)
    OpenDlg: TOpenDialog;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    eVisitorCnt: TLabel;
    Label5: TLabel;
    eFileName: TEdit;
    btnOpen: TButton;
    eStart: TEdit;
    btnLoad: TButton;
    eRef: TEdit;
    btnNew: TButton;
    eVisitors: TListBox;
    Panel2: TPanel;
    Label4: TLabel;
    eInfo: TMemo;
    Splitter1: TSplitter;
    eVisIP: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    eTimeSite: TEdit;
    ePageVisit: TEdit;
    Label8: TLabel;
    lVisitRef: TLabel;
    eVisitRef: TEdit;
    Label9: TLabel;
    eMandatoryPage: TEdit;
    Label10: TLabel;
    eNegativePage: TEdit;
    eMinPages: TEdit;
    Label11: TLabel;
    eMinTime: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    eMaxPages: TEdit;
    eMaxTime: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    procedure btnOpenClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure eVisitorsClick(Sender: TObject);
    procedure eVisIPEnter(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Data:TRtcRecord;
  end;

var
  Form1: TForm1;

implementation

uses StrUtils;

{$R *.dfm}

procedure TForm1.btnOpenClick(Sender: TObject);
  begin
  if OpenDlg.Execute then
    eFileName.Text:=OpenDlg.FileName;
  end;

procedure TForm1.btnLoadClick(Sender: TObject);
  var
    tim1,tim2:TDateTime;
    negpage,mandpage,wantstart,wantref:string;
    fn, fpath, indata, line, tim, ip, url, ext, ref:string;
    a, loc, loc2, len, loc3:integer;
    minpages, maxpages: integer;
    totaltime, mintime, maxtime: TDateTime;
    havemand:boolean;
    Temp:TRtcRecord;
    f:TSearchRec;
  begin
  fn:=ExpandUNCFileName(eFileName.Text);
  if fn='' then Exit;

  eInfo.Lines.Clear;
  eVisitorCnt.Caption:='Loading ...';
  btnLoad.Enabled:=False;
  Update;

  try
    mandpage:=eMandatoryPage.Text;
    negpage:=eNegativePage.Text;
    wantstart:=eStart.Text;
    wantref:=eRef.Text;

    minpages:=Str2IntDef(eMinPages.Text,0);
    maxpages:=Str2IntDef(eMaxPages.Text,MaxInt);
    if eMinTime.Text<>'' then
      mintime:=Str2DateTime(eMinTime.Text)
    else
      mintime:=0;
    if eMaxTime.Text<>'' then
      maxtime:=Str2DateTime(eMaxTime.Text)
    else
      maxtime:=1000;

    fpath:=ExtractFilePath(fn);
    if fpath[length(fpath)]<>'\' then
      fpath:=fpath+'\';

    Temp:=TRtcRecord.Create;
    try
      if FindFirst(fn,faAnyFile,F)=0 then
        try
          repeat
            fn:=fpath+F.Name;

            indata:=Read_File(fn);
            len:=length(indata);
            loc:=1;
            while loc<len do
              begin
              loc2:=loc;
              while (loc2<=len) and (indata[loc2]<>#13) do Inc(loc2);
              line:=Copy(indata,loc,loc2-loc);
              loc:=loc2;
              while (loc<=len) and (indata[loc] in [#13,#10]) do Inc(loc);

              if length(line)>0 then
                begin
                loc3:=Pos(';',line);
                if loc3>0 then
                  begin
                  tim:=Copy(line,1,loc3-1);
                  Delete(line,1,loc3);
                  if (Copy(line,1,6)=' SEND ') or (Copy(line,1,6)=' EXEC ') then
                    begin
                    Delete(line,1,6);
                    loc3:=Pos(' ',line);
                    if loc3>0 then
                      begin
                      ip:=Copy(line,1,loc3-1);
                      Delete(line,1,loc3);
                      loc3:=Pos('"',line);
                      if loc3>0 then
                        begin
                        Delete(line,1,loc3);
                        if Copy(line,1,4)='GET ' then
                          begin
                          Delete(line,1,4);
                          loc3:=Pos('"',line);
                          if loc3>0 then
                            begin
                            url:=Copy(line,1,loc3-1);
                            Delete(line,1,loc3);

                            loc3:=Pos('?',url);
                            if loc3>0 then
                              ext:=Copy(url,loc3-5,5)
                            else
                              ext:=RightStr(url,5);
                            loc3:=Pos('.',ext);
                            if loc3>0 then
                              ext:=Copy(UpperCase(Copy(ext,loc3,length(ext)-loc3+1)),1,4)
                            else
                              ext:='';

                            if (ext='') or
                               ( (ext<>'.CSS') and (Copy(ext,1,3)<>'.JS') and
                                 (ext<>'.GIF') and (ext<>'.JPG') and (ext<>'.PNG') and
                                 (ext<>'.BMP') and (ext<>'.CUR') and (ext<>'.ICO') ) then
                              begin
                              // We have a valid non-image,non-css URL ...

                              // find first " after the URL string. This is the REF string.
                              loc3:=Pos('"',line);
                              if loc3>0 then
                                begin
                                Delete(line,1,loc3);
                                // find the end of the REF string.
                                loc3:=Pos('"',line);
                                if loc3>0 then
                                  ref:=Copy(line,1,loc3-1)
                                else
                                  ref:='';
                                end;

                              if Temp.isType[ip]<>rtc_DataSet then
                                if (wantstart='') or (Pos(wantstart,url)>0) then
                                  if (wantref='') or (Pos(wantref,ref)>0) then
                                    Temp.newDataSet(ip);

                              if Temp.isType[ip]=rtc_DataSet then
                                begin
                                with Temp.asDataSet[ip] do
                                  begin
                                  Append;
                                  asDateTime['time']:=Str2DateTime(tim);
                                  asString['url']:=url;
                                  asString['ref']:=ref;
                                  end;
                                end;
                              end;
                            end;
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            until FindNext(F)<>0;
        finally
          FindClose(F);
        end;

      for a:=0 to Temp.FieldCount-1 do
        begin
        ip:=Temp.FieldName[a];
        with Temp.asDataSet[ip] do
          begin
          if (minpages>0) or (maxpages<MaxInt) then
            havemand:=(RowCount>=minpages) and (RowCount<=maxpages)
          else
            havemand:=True;
          if havemand and ((mintime<>0) or (maxtime<>0)) then
            begin
            First;
            totaltime:=asDateTime['time'];
            Last;
            totaltime:=asDateTime['time']-totaltime;
            havemand:=(totaltime>=mintime) and (totaltime<=maxtime);
            end;
          if havemand and (mandpage<>'') then
            begin
            havemand:=False;
            First;
            if mandpage=wantstart then Next;
            while not EOF do
              begin
              if Pos(mandpage,asString['url'])>0 then
                begin
                havemand:=True;
                Break;
                end;
              Next;
              end;
            end;
          if havemand and (negpage<>'') then
            begin
            First;
            if negpage=wantstart then Next;
            while not EOF do
              begin
              if Pos(negpage,asString['url'])>0 then
                begin
                havemand:=False;
                Break;
                end;
              Next;
              end;
            end;
          end;

        if havemand then
          begin
          if Data.isType[ip]=rtc_Null then
            Data.newDataSet(ip);

          with Temp.asDataSet[ip] do
            begin
            First;
            Data.asDataSet[ip].First;
            if (Data.asDataSet[ip].RowCount=0) or
               (Data.asDataSet[ip].asDateTime['time']<asDateTime['time']) or
               (Data.asDataSet[ip].asString['url']<>asString['url']) then
              begin
              while not EOF do
                begin
                Data.asDataSet[ip].Append;
                Data.asDataSet[ip].asDateTime['time']:=asDateTime['time'];
                Data.asDataSet[ip].asString['url']:=asString['url'];
                Data.asDataSet[ip].asString['ref']:=asString['ref'];
                Next;
                end;
              end;
            end;
          end;
        end;
    finally
      Temp.Free;
      indata:='';
      end;
  eVisitors.Clear;
  for a:=0 to Data.FieldCount-1 do
    begin
    ip:=Data.FieldName[a];
    with Data.asDataSet[ip] do
      begin
      First;
      tim1:=asDateTime['time'];
      url:=asString['url'];
      ref:=asString['ref'];
      Last;
      tim2:=asDateTime['time'];
      if RowCount<10 then
        eVisitors.AddItem('0'+IntToStr(RowCount)+' '+TimeToStr(tim2-tim1)+' '+ip+' > '+url+' << '+ref,nil)
      else
        eVisitors.AddItem(IntToStr(RowCount)+' '+TimeToStr(tim2-tim1)+' '+ip+' > '+url+' << '+ref,nil);
      end;
    end;
  eVisitorCnt.Caption:=IntToStr(Data.FieldCount);
  finally
    btnLoad.Enabled:=True;
    end;
  end;

procedure TForm1.FormCreate(Sender: TObject);
  begin
  Data:=TRtcRecord.Create;
  end;

procedure TForm1.btnNewClick(Sender: TObject);
  begin
  Data.Clear;
  eVisitors.Clear;
  end;

procedure TForm1.eVisitorsClick(Sender: TObject);
  var
    ip:string;
    tim1,tim2:TDateTime;
  begin
  eInfo.Lines.Clear;
  eInfo.Lines.BeginUpdate;
  try
    if eVisitors.ItemIndex>=0 then
      begin
      ip:=Data.FieldName[eVisitors.ItemIndex];
      eVisIP.Text:=ip;
      with Data.asDataSet[ip] do
        begin
        First;
        tim2:=asDateTime['time'];
        eInfo.Lines.Add(DateTimeToStr(tim2));
        eVisitRef.Text:=asString['ref'];
        while not EOF do
          begin
          tim1:=tim2;
          tim2:=asDateTime['time'];
          if tim2-tim1>1/24 then // more than 1 hour pause
            eInfo.Lines.Add(TimeToStr(tim2-tim1)+' ('+DateTimeToStr(tim2)+') > '+ asString['url'])
          else
            eInfo.Lines.Add(TimeToStr(tim2-tim1)+' > '+ asString['url']);
          Next;
          end;
        First;
        tim1:=asDateTime['time'];
        Last;
        tim2:=asDateTime['time'];
        eTimeSite.Text:=TimeToStr(tim2-tim1);
        ePageVisit.Text:=IntToStr(RowCount);
        end;
      end;
  finally
    eInfo.Lines.EndUpdate;
    end;
  end;

procedure TForm1.eVisIPEnter(Sender: TObject);
  begin
  eVisIP.SelectAll;
  end;

end.
