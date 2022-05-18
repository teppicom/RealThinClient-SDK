unit xmlrpcTest_unit;

interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  rtcTypes, rtcSystem, rtcInfo,

  Buttons, Menus;

type
  TForm1 = class(TForm)
    Panel3: TPanel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Panel5: TPanel;
    eXMLSrc: TMemo;
    eXMLDest: TMemo;
    btnRTC: TSpeedButton;
    btnJSON: TSpeedButton;
    btnXML: TSpeedButton;
    Label1: TLabel;
    btnOTF: TCheckBox;
    btnClr: TSpeedButton;
    MainMenu1: TMainMenu;
    JSON1: TMenuItem;
    JSONGenerator1: TMenuItem;
    mParseDate: TMenuItem;
    mParseError: TMenuItem;
    mParseBase64: TMenuItem;
    mParseMethod: TMenuItem;
    mParseDataSet: TMenuItem;
    N1: TMenuItem;
    mGenSlash: TMenuItem;
    mGenDateTime: TMenuItem;
    mGenError: TMenuItem;
    mGenBase64: TMenuItem;
    mGenMethod: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    btnLoadOK: TButton;
    btnLoadFAIL: TButton;
    procedure btnXMLClick(Sender: TObject);
    procedure btnRTCClick(Sender: TObject);
    procedure btnJSONClick(Sender: TObject);
    procedure eXMLSrcChange(Sender: TObject);
    procedure btnOTFClick(Sender: TObject);
    procedure btnClrClick(Sender: TObject);
    procedure mParseDateClick(Sender: TObject);
    procedure mParseErrorClick(Sender: TObject);
    procedure mParseBase64Click(Sender: TObject);
    procedure mParseMethodClick(Sender: TObject);
    procedure mParseDataSetClick(Sender: TObject);
    procedure mGenSlashClick(Sender: TObject);
    procedure mGenDateTimeClick(Sender: TObject);
    procedure mGenErrorClick(Sender: TObject);
    procedure mGenBase64Click(Sender: TObject);
    procedure mGenMethodClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLoadOKClick(Sender: TObject);
    procedure btnLoadFAILClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure TestTheRest(obj:TRtcValue);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function Unicodize(s:RtcString):String;
  begin
  {$IFDEF Unicode}
  Result:=utf8decode(s);
  {$ELSE}
  Result:=s;
  {$ENDIF}
  end;

function DeUnicodize(s:String):RtcString;
  begin
  {$IFDEF Unicode}
  Result:=utf8encode(s);
  {$ELSE}
  Result:=s;
  {$ENDIF}
  end;

procedure TForm1.TestTheRest(obj:TRtcValue);
  var
    tim1,tim2,tim3:Cardinal;
    s1,s2:RtcString;
    s3:RtcWideString;
  begin
  tim1:=GetTickTime;
  s1:=obj.toCode;
  tim1:=GetTickTime-tim1;
  eXMLDest.Lines.Add(IntToStr(length(s1))+' RTC-CODE chars generated in '+IntToStr(tim1)+' ms');
  // Test parsing
  TRtcValue.FromCode(s1).Free;

  tim3:=GetTickTime;
  s3:='{'#13#10+'"JSON+REST": '+obj.toJSON;
  if obj is TRtcValueResult then
    begin
    TRtcValueResult(obj).reqVer:=1;
    s3:=s3+' ,'#13#10+'"JSON-RPC_v1.0": '+obj.toJSON+' ,';
    TRtcValueResult(obj).reqVer:=2;
    s3:=s3+#13#10+'"JSON-RPC_v2.0": '+obj.toJSON+' ,';
    TRtcValueResult(obj).reqVer:=3;
    s3:=s3+#13#10+'"CUSTOM": '+obj.toJSON;
    end;
  s3:=s3+#13#10+'}';
  tim3:=GetTickTime-tim3;
  eXMLDest.Lines.Add(IntToStr(length(s3))+' JSON chars generated in '+IntToStr(tim3)+' ms'#13#10);
  // Test parsing
  TRtcValue.FromJSON(s3).Free;

  tim2:=GetTickTime;
  s2:='<value><array><data>'#13#10#13#10+
      '<value>XML+REST REQUEST</value>'#13#10+
      obj.toXMLRESTRequest+#13#10#13#10+
      '<value>XML-RPC REQUEST</value>'#13#10+
      obj.toXMLrpcRequest+#13#10#13#10+
      '<value>XML-RPC RESPONSE</value>'#13#10+
      obj.toXMLrpcResponse+#13#10#13#10+
      '</data></array></value>';
  tim2:=GetTickTime-tim2;
  eXMLDest.Lines.Add(IntToStr(length(s2))+' XML-RPC chars generated in '+IntToStr(tim2)+' ms');
  // Test parsing
  TRtcValue.FromXMLrpc(s2).Free;

  eXMLDest.Lines.Add(#13#10#13#10+'----- in RTC-CODE format --------'#13#10);
  eXMLDest.Lines.Add(Unicodize(s1));
  eXMLDest.Lines.Add('----- in JSON formats --------'#13#10);
  eXMLDest.Lines.Add(String(s3));
  eXMLDest.Lines.Add(#13#10#13#10+'----- in XML formats --------'#13#10);
  eXMLDest.Lines.Add(Unicodize(s2));

  eXMLDest.SelStart:=0;
  eXMLDest.SelLength:=1;
  end;

procedure TForm1.btnJSONClick(Sender: TObject);
  var
    tim1:Cardinal;
    obj:TRtcValue;
    s1:RtcWideString;
  begin
  if eXMLSrc.Lines.Text='' then
    begin
    eXMLDest.Lines.Clear;
    Exit;
    end;
  s1:=eXMLSrc.Lines.Text;
  tim1:=GetTickTime;
  try
    obj:=TRtcValueResult.FromJSONrpc(1+2+4,s1);
  except
    on E:Exception do
      begin
      eXMLDest.Lines.Clear;
      eXMLDest.Lines.Add('From JSON Exception '+E.ClassName+':');
      eXMLDest.Lines.Add(E.Message);
      Exit;
      end;
    end;
  tim1:=GetTickTime-Tim1;
  eXMLDest.Lines.Clear;
  eXMLDest.Lines.Add(IntToStr(length(s1))+' JSON chars parsed in '+IntToStr(tim1)+' ms');
  try
    TestTheRest(obj);
  finally
    obj.Free;
    end;
  end;

procedure TForm1.btnXMLClick(Sender: TObject);
  var
    tim1:Cardinal;
    obj:TRtcValue;
    s1:RtcString;
  begin
  if eXMLSrc.Lines.Text='' then
    begin
    eXMLDest.Lines.Clear;
    Exit;
    end;
  s1:=DeUnicodize(eXMLSrc.Lines.Text);
  tim1:=GetTickTime;
  try
    obj:=TRtcValueResult.FromXMLrpc(s1);
  except
    on E:Exception do
      begin
      eXMLDest.Lines.Clear;
      eXMLDest.Lines.Add('From XML-RPC Exception '+E.ClassName+':');
      eXMLDest.Lines.Add(E.Message);
      Exit;
      end;
    end;
  tim1:=GetTickTime-Tim1;
  eXMLDest.Lines.Clear;
  eXMLDest.Lines.Add(IntToStr(length(s1))+' XML-RPC chars parsed in '+IntToStr(tim1)+' ms');
  try
    TestTheRest(obj);
  finally
    obj.Free;
    end;
  end;

procedure TForm1.btnRTCClick(Sender: TObject);
  var
    tim1:Cardinal;
    obj:TRtcValue;
    s1:RtcString;
  begin
  if eXMLSrc.Lines.Text='' then
    begin
    eXMLDest.Lines.Clear;
    Exit;
    end;
  s1:=DeUnicodize(eXMLSrc.Lines.Text);
  tim1:=GetTickTime;
  try
    obj:=TRtcValueResult.FromCode(s1);
  except
    on E:Exception do
      begin
      eXMLDest.Lines.Clear;
      eXMLDest.Lines.Add('From RTC-CODE Exception '+E.ClassName+':');
      eXMLDest.Lines.Add(E.Message);
      Exit;
      end;
    end;
  tim1:=GetTickTime-Tim1;
  eXMLDest.Lines.Clear;
  eXMLDest.Lines.Add(IntToStr(length(s1))+' RTC-CODE chars parsed in '+IntToStr(tim1)+' ms');
  try
    TestTheRest(obj);
  finally
    obj.Free;
    end;
  end;

procedure TForm1.btnLoadFAILClick(Sender: TObject);
  var
    sr:TSearchRec;
    tim1,tim2,tim3:int64;
    s1,s2,s3,s4:String;
    obj:TRtcValue;
    cntOK,cntFail:integer;
  begin
  if FindFirst('JSON_FAIL\*.*',0,sr)=0 then
    begin
    cntOK:=0;
    cntFail:=0;
    eXMLSrc.Lines.Clear;
    eXMLDest.Lines.Clear;
    repeat
      s1:=Read_File('JSON_FAIL\'+sr.Name);
      tim1:=GetTickTime64;
      try
        obj:=TRtcValue.FromJSON(s1);
        tim2:=GetTickTime64;
        s2:=obj.toJSON;
        s3:=obj.toCode; TRtcValue.FromCode(s3).Free; // test RTC Code parser
        s4:=obj.toXMLrpc; TRtcValue.FromXMLrpc(s4).Free; // test XML-RPC parser
        tim3:=GetTickTime64;
        eXMLSrc.Lines.Add('"'+sr.Name+'" PASS? ('+IntToStr(tim2-tim1)+'/'+IntToStr(tim3-tim2)+'ms)');
        eXMLSrc.Lines.Add(s4);
        eXMLSrc.Lines.Add(s3);
        eXMLSrc.Lines.Add(s1);
        eXMLSrc.Lines.Add(s2);
        eXMLSrc.Lines.Add('');
        obj.Free;
        Inc(cntOK);
      except
        on E:Exception do
          begin
          tim2:=GetTickTime64;
          eXMLDest.Lines.Add('"'+sr.Name+' FAIL! ('+IntToStr(tim2-tim1)+'ms) '+E.Message);
          eXMLDest.Lines.Add(s1);
          eXMLDest.Lines.Add('');
          Inc(cntFail);
          end;
        end;
      until FindNext(sr)<>0;
    FindClose(sr);
    eXMLDest.Lines.Add('"FAIL" TEST DONE: '+IntToStr(cntOK)+' PASS, '+IntToStr(cntFail)+' FAIL.');
    end;
  end;

procedure TForm1.btnLoadOKClick(Sender: TObject);
  var
    sr:TSearchRec;
    tim1,tim2,tim3:int64;
    s1,s2,s3,s4:String;
    obj:TRtcValue;
    cntOK,cntFail:integer;
  begin
  if FindFirst('JSON_PASS\*.*',0,sr)=0 then
    begin
    cntOK:=0;
    cntFail:=0;
    eXMLSrc.Lines.Clear;
    eXMLDest.Lines.Clear;
    repeat
      s1:=Read_File('JSON_PASS\'+sr.Name);
      tim1:=GetTickTime64;
      try
        obj:=TRtcValue.FromJSON(s1);
        tim2:=GetTickTime64;
        s2:=obj.toJSON;
        s3:=obj.toCode; TRtcValue.FromCode(s3).Free; // test RTC-Code parser
        s4:=obj.toXMLrpc; TRtcValue.FromXMLrpc(s4).Free; // test XML-RPC parser
        tim3:=GetTickTime64;
        eXMLDest.Lines.Add('"'+sr.Name+'" PASS! ('+IntToStr(tim2-tim1)+'/'+IntToStr(tim3-tim2)+'ms)');
        eXMLDest.Lines.Add(s4);
        eXMLDest.Lines.Add(s3);
        eXMLDest.Lines.Add(s1);
        eXMLDest.Lines.Add(s2);
        eXMLDest.Lines.Add('');
        obj.Free;
        Inc(cntOK);
      except
        on E:Exception do
          begin
          tim2:=GetTickTime64;
          eXMLSrc.Lines.Add('"'+sr.Name+'" FAIL? ('+IntToStr(tim2-tim1)+'ms) '+E.Message);
          eXMLSrc.Lines.Add(s1);
          eXMLSrc.Lines.Add('');
          Inc(cntFail);
          end;
        end;
      until FindNext(sr)<>0;
    FindClose(sr);
    eXMLDest.Lines.Add('"PASS" TEST DONE: '+IntToStr(cntOK)+' PASS, '+IntToStr(cntFail)+' FAIL.');
    end;
  end;

procedure TForm1.eXMLSrcChange(Sender: TObject);
  begin
  if (Sender<>nil) and not btnOTF.Checked then Exit;

  if eXMLSrc.Lines.Text='' then
    eXMLDest.Lines.Clear
  else if btnRTC.Down then
    btnRTCClick(nil)
  else if btnXML.Down then
    btnXMLClick(nil)
  else if btnJSON.Down then
    btnJSONClick(nil);
  end;

procedure TForm1.mGenBase64Click(Sender: TObject);
  begin
  RTC_JSON_GenTypedByteStream:=TMenuItem(Sender).Checked;
  eXMLSrcChange(nil);
  end;

procedure TForm1.mGenDateTimeClick(Sender: TObject);
  begin
  RTC_JSON_GenTypedDateTime:=TMenuItem(Sender).Checked;
  eXMLSrcChange(nil);
  end;

procedure TForm1.mGenErrorClick(Sender: TObject);
  begin
  RTC_JSON_GenTypedException:=TMenuItem(Sender).Checked;
  eXMLSrcChange(nil);
  end;

procedure TForm1.mGenMethodClick(Sender: TObject);
  begin
  RTC_JSON_GenTypedFunctions:=TMenuItem(Sender).Checked;
  eXMLSrcChange(nil);
  end;

procedure TForm1.mGenSlashClick(Sender: TObject);
  begin
  RTC_JSON_GenEscapeSlash:=TMenuItem(Sender).Checked;
  eXMLSrcChange(nil);
  end;

procedure TForm1.mParseBase64Click(Sender: TObject);
  begin
  RTC_JSON_ParseTypedByteStream:=TMenuItem(Sender).Checked;
  eXMLSrcChange(nil);
  end;

procedure TForm1.mParseDataSetClick(Sender: TObject);
  begin
  RTC_JSON_ParseTypedDataSet:=TMenuItem(Sender).Checked;
  eXMLSrcChange(nil);
  end;

procedure TForm1.mParseDateClick(Sender: TObject);
  begin
  RTC_JSON_ParseTypedDateTime:=TMenuItem(Sender).Checked;
  eXMLSrcChange(nil);
  end;

procedure TForm1.mParseErrorClick(Sender: TObject);
  begin
  RTC_JSON_ParseTypedException:=TMenuItem(Sender).Checked;
  eXMLSrcChange(nil);
  end;

procedure TForm1.mParseMethodClick(Sender: TObject);
  begin
  RTC_JSON_ParseTypedFunctions:=TMenuItem(Sender).Checked;
  eXMLSrcChange(nil);
  end;

procedure TForm1.btnOTFClick(Sender: TObject);
  begin
  if btnOTF.Checked then
    begin
    btnRTC.GroupIndex:=1;
    btnXML.GroupIndex:=1;
    btnJSON.GroupIndex:=1;
    end
  else
    begin
    btnRTC.Down:=False;
    btnXML.Down:=False;
    btnJSON.Down:=False;
    btnRTC.GroupIndex:=0;
    btnXML.GroupIndex:=0;
    btnJSON.GroupIndex:=0;
    end;
  end;

procedure TForm1.btnClrClick(Sender: TObject);
  begin
  eXMLSrc.Lines.Clear;
  end;

function GetJSONOne:String;
var
  rtcobj:TRtcRecord;
begin
rtcobj:=TRtcRecord.Create;
with rtcobj do
  begin
  asText['version']:='1.0';
  with newRecord('rmsResponse') do
    begin
    with newRecord('Results') do
      begin
      asText['URI']:='/rms/vol/GetRegEvents'; 
      asText['ErrCode']:='0';
      with newArray('event') do
        begin
        with newRecord(0) do
          begin
          newRecord('id').asText['=']:='LMCC';
          newRecord('web_date').asText['=']:='07 July';
          newRecord('web_name').asText['=']:='Laser Masters 2011';
          with newRecord('web_notes').newArray('Note') do
            begin
            newRecord(0).asText['Text']:='Bring booze';
            newRecord(1).asText['Text']:='Have Fun!!';
            end;
          end;
        with newRecord(1) do
          begin
          newRecord('id').asText['=']:='OFF11';
          newRecord('web_date').asText['=']:='August 25 - 28';
          newRecord('web_name').asText['=']:='Offshore 2011';
          newRecord('web_notes').newRecord('Note').asText['Text']:='Bring beer.';
          end;
        with newRecord(3) do
          begin
          newRecord('id').asText['=']:='OPT12';
          newRecord('web_date').asText['=']:='09 Aug - 12 Aug';
          newRecord('web_name').asText['=']:='Optimist CORK 2012';
          with newRecord('web_notes').newArray('Note') do
            begin
            newRecord(0).asText['Text']:='ISAF number required';
            newRecord(1).asText['Text']:='Valid CANSail Number which they can get from CYA.';
            newRecord(2).asText['Text']:='Age restricted 5 to 12 yrs.';
            end;
          end;
        with newRecord(4) do
          begin
          newRecord('id').asText['=']:='OFF12';
          newRecord('web_date').asText['=']:='23 Aug - 27 Aug';
          newRecord('web_name').asText['=']:='OFFSHORE 2012';
          with newRecord('web_notes').newArray('Note') do
            begin
            newRecord(0).asText['Text']:='Note line 1';
            newRecord(1).asText['Text']:='Note line 2';
            end;
          end;
        end;
      end;
    end;
  end;
Result:=rtcobj.toJSON;
rtcobj.Free;
end;

function GetJSONTwo:String;
var
  rtcobj:TRtcRecord;
begin
rtcobj:=TRtcRecord.Create;
with rtcobj do
  begin
  asText['&version']:='1.0';
  with newRecord('rmsResponse') do
    begin
    with newRecord('Results') do
      begin
      asText['&URI']:='/rms/vol/GetRegEvents'; 
      asText['&ErrCode']:='0';
      with newArray('event') do
        begin
        with newRecord(0) do
          begin
          asText['id']:='LMCC';
          asText['web_date']:='07 July';
          asText['web_name']:='Laser Masters 2011';
          with newRecord('web_notes').newArray('Note') do
            begin
            newRecord(0).asText['&Text']:='Bring booze';
            newRecord(1).asText['&Text']:='Have Fun!!';
            end;
          end;
        with newRecord(1) do
          begin
          asText['id']:='OFF11';
          asText['web_date']:='August 25 - 28';
          asText['web_name']:='Offshore 2011';
          newRecord('web_notes').newRecord('Note').asText['&Text']:='Bring beer.';
          end;
        with newRecord(2) do
          begin
          asText['id']:='OPT12';
          asText['web_date']:='09 Aug - 12 Aug';
          asText['web_name']:='Optimist CORK 2012';
          with newRecord('web_notes').newArray('Note') do
            begin
            newRecord(0).asText['&Text']:='ISAF number required';
            newRecord(1).asText['&Text']:='Valid CANSail Number which they can get from CYA.';
            newRecord(2).asText['&Text']:='Age restricted 5 to 12 yrs.';
            end;
          end;
        with newRecord(3) do
          begin
          asText['id']:='OFF12';
          asText['web_date']:='23 Aug - 27 Aug';
          asText['web_name']:='OFFSHORE 2012';
          with newRecord('web_notes').newArray('Note') do
            begin
            newRecord(0).asText['&Text']:='Note line 1';
            newRecord(1).asText['&Text']:='Note line 2';
            end;
          end;
        end;
      end;
    end;
  end;
Result:=rtcobj.toJSON;
rtcobj.Free;
end;

function GetJSONThree:String;
var
  rtcobj:TRtcRecord;
begin
rtcobj:=TRtcRecord.Create;
with rtcobj do
  begin
  asText['@version']:='1.0';
  with newDataSet('rmsResponse') do
    begin
    with newDataSet('Results') do
      begin
      asText['@URI']:='/rms/vol/GetRegEvents'; 
      asText['@ErrCode']:='0';
      with newDataSet('event') do
        begin
        asText['id']:='LMCC';
        asText['web_date']:='07 July';
        asText['web_name']:='Laser Masters 2011';
        with newDataSet('web_notes').newDataSet('Note') do
          begin
          asText['@Text']:='Bring booze';
          Append; 
          asText['@Text']:='Have Fun!!';
          end;

        Append;
        asText['id']:='OFF11';
        asText['web_date']:='August 25 - 28';
        asText['web_name']:='Offshore 2011';
        newDataSet('web_notes').newDataSet('Note').asText['@Text']:='Bring beer.';
        
        Append;
        asText['id']:='OPT12';
        asText['web_date']:='09 Aug - 12 Aug';
        asText['web_name']:='Optimist CORK 2012';
        with newDataSet('web_notes').newDataSet('Note') do
          begin
          asText['@Text']:='ISAF number required';
          Append;
          asText['@Text']:='Valid CANSail Number which they can get from CYA.';
          Append;
          asText['@Text']:='Age restricted 5 to 12 yrs.';
          end;

        Append;
        asText['id']:='OFF12';
        asText['web_date']:='23 Aug - 27 Aug';
        asText['web_name']:='OFFSHORE 2012';
        with newDataSet('web_notes').newDataSet('Note') do
          begin
          asText['@Text']:='Note line 1';
          Append;
          asText['@Text']:='Note line 2';
          end;
        end;
      end;
    end;
  end;
Result:=rtcobj.toJSON;
rtcobj.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
  begin
  eXMLSrc.Lines.Text:='['#13#10+GetJSONOne+#13#10+','+
                       #13#10+GetJSONTwo+#13#10+','+
                       #13#10+GetJSONThree+#13#10+']';
  end;

end.
