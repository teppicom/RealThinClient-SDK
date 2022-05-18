object Form1: TForm1
  Left = 213
  Top = 206
  Width = 624
  Height = 408
  Caption = 'RTC Fish Fact Server (single-threaded RtcMemDataSet version)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 0
    Top = 248
    Width = 606
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 29
    Width = 606
    Height = 219
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -13
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 251
    Width = 606
    Height = 112
    Align = alBottom
    TabOrder = 1
    object lblSQL: TLabel
      Left = 1
      Top = 1
      Width = 604
      Height = 110
      Align = alClient
      Caption = 
        'This is where you will see SQL statements received from the Clie' +
        'nt ...'
      WordWrap = True
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 606
    Height = 29
    Align = alTop
    Caption = 
      'This is a Live View of Data stored on the Server, all changes re' +
      'ceived from Clients are applied.'
    TabOrder = 2
  end
  object RtcHttpServer1: TRtcHttpServer
    ServerPort = '81'
    Left = 24
    Top = 88
  end
  object RtcServerModule1: TRtcServerModule
    Server = RtcHttpServer1
    Compression = cDefault
    ModuleFileName = '/biolife'
    FunctionGroup = RtcFunctionGroup1
    Left = 56
    Top = 88
  end
  object RtcFunctionGroup1: TRtcFunctionGroup
    Left = 88
    Top = 88
  end
  object rtcSelectFn: TRtcFunction
    Group = RtcFunctionGroup1
    FunctionName = 'select'
    OnExecute = rtcSelectFnExecute
    Left = 120
    Top = 88
  end
  object rtcSubmitFn: TRtcFunction
    Group = RtcFunctionGroup1
    FunctionName = 'submit'
    OnExecute = rtcSubmitFnExecute
    Left = 152
    Top = 88
  end
  object RtcMemDataSet1: TRtcMemDataSet
    FileName = 'FishFacts.data'
    Left = 24
    Top = 136
  end
  object DataSource1: TDataSource
    DataSet = RtcMemDataSet1
    Left = 60
    Top = 136
  end
end
