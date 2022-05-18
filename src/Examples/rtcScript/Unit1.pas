unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes,

  Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  rtcInfo, rtcConn, rtcFunction,
  rtcScript, rtcDataSrv, rtcHttpSrv, rtcSystem;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Label1: TLabel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    lblTime: TLabel;
    Button1: TButton;
    Memo2: TMemo;
    RtcScriptEngine1: TRtcScriptEngine;
    FunctionGroup: TRtcFunctionGroup;
    FnTime: TRtcFunction;
    FnDate: TRtcFunction;
    FnNow: TRtcFunction;
    FnRandom: TRtcFunction;
    FnPolyArea: TRtcFunction;
    DummyConnection: TRtcHttpServer;
    procedure Button1Click(Sender: TObject);
    procedure FnTimeExecute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure FnDateExecute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure FnNowExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo;
      Result: TRtcValue);
    procedure FnRandomExecute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
    procedure FormCreate(Sender: TObject);
    procedure FnPolyAreaExecute(Sender: TRtcConnection;
      Param: TRtcFunctionInfo; Result: TRtcValue);
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
  Randomize;
  end;

procedure TForm1.Button1Click(Sender: TObject);
  var
    fn,x:TRtcValue;

    s:string;

    tc:dword;
  begin
  Memo2.Clear;
  s := Memo1.Lines.Text;

  try
    x:=nil;
    try
      { I am using a dummy Server connection component here to use this
        components Session, Request, Response, Input and Query variables.
        In a real Server, you should send the Sender:TRtcConnection object,
        which you will get as a parameter in DataProvider's OnDataReceived event. }

      lblTime.Caption:='Compiling Script ...';
      lblTime.Update;
      // Compile Script and store it in "fn" ...
      fn:=RtcScriptEngine1.Compile(s);
      // s:=fn.toCode; -> can be used to check the result of a Script compilation

      lblTime.Caption:='Executing Script ...';
      lblTime.Update;
      tc:=GetTickCount;
      // Execute Script "fn" and return the Result.
      // "fn" will be destroyed by "Execute()" ...
      x:=RtcScriptEngine1.Execute(DummyConnection, fn);
      tc:=GetTickCount-tc;

      // Get execution result as Text
      lblTime.Caption:='Fetching Result ...';
      lblTime.Update;
      s:=x.asText;
      // Display raw execution time
      lblTime.Caption:='Time: '+IntToStr(tc)+' ms';
      Memo2.Lines.Text:=s;
    finally
      // We don't need the result object anymore
      x.Free;
      end;
  except
    on E:Exception do
      begin
      lblTime.Caption:='Error ...';
      Memo2.Lines.Text:=E.Message;
      end;
    end;
  end;

(* Here are implmentations for functions made available
   from Script files executed using "RtcScriptEngine1" *)

procedure TForm1.FnTimeExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  Result.asDateTime:=Time;
  end;

procedure TForm1.FnDateExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  Result.asDateTime:=Date;
  end;

procedure TForm1.FnNowExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  Result.asDateTime:=Now;
  end;

procedure TForm1.FnRandomExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  begin
  if Param.isType['PARAMS']=rtc_Array then
    Result.asInteger := random (Param.asArray['PARAMS'].asInteger[0])
  else
    Result.asInteger := random (Param.asInteger['X']);
  end;

procedure TForm1.FnPolyAreaExecute(Sender: TRtcConnection; Param: TRtcFunctionInfo; Result: TRtcValue);
  var
    Par:TRtcArray;
    I:integer;

  function GetN(n:integer; index:integer; const name:String):double;
    begin
    if n>=Par.Count then n:=0;
    if Par.isType[n]=rtc_Array then
      Result:=Par.asArray[n].asFloat[index]
    else if Par.isType[n]=rtc_Record then
      Result:=Par.asRecord[n].asFloat[name]
    else
      Result:=0;
    end;

  begin
  if Param.isType['params']=rtc_Array then
    begin
    // using temporary variable to make the code shorter
    Par:=Param.asArray['params'];

    Result.asFloat := 0.0;
    For I := 0 to Par.Count-1 do
      Result.asFloat := Result.asFloat
              + GetN(I,0,'x') * GetN(I+1,1,'y')
              - GetN(I+1,0,'x') * GetN(I,1,'y');
    if Result.asFloat < 0 then Result.asFloat := -Result.asFloat;
    Result.asFloat := Result.asFloat/2;
    end;
  end;

end.
