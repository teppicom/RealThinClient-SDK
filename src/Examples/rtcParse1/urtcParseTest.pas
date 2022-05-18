unit urtcParseTest;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,

  rtcParse;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    ListBox1: TListBox;
    btnGenerate: TButton;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Label1: TLabel;
    Memo2: TMemo;
    procedure FormShow(Sender: TObject);

    procedure btnGenerateClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormShow(Sender: TObject);
  begin
  //  load the source string from a file into Memo1
  Memo1.Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + 'template.htm');

  btnGenerate.Click;
  end;

procedure TForm1.btnGenerateClick(Sender: TObject);
  var
    parse:TRtcParse;
    count:integer;
  begin
  parse:=TRtcParse.Create;
  try
    // set new token delimiters
    parse.TokenOpen := '<%';
    parse.TokenClose := '%>';

    // Set source (Template)
    parse.Source:=Memo1.Lines.Text;

    // add the variable names to the list
    ListBox1.Items.Clear;
    for count := 0 to parse.Count-1 do
      ListBox1.Items.Add(parse.VariableName[count]);

    // set the values of the variables
    parse.Value['title'] := 'A Simple Parser';
    parse.Value['color'] := '#FFFFFF';
    parse.Value['bgcolor'] := '#000000';
    parse.Value['page'] := 'rtcParse';
    parse.Value['copyright'] := '(C) Copyright 2005';

    Memo2.Lines.Text := parse.Output;
  finally
    parse.Free;
    end;
  end;

end.
