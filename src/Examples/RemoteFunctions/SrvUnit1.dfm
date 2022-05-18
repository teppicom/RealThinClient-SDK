object Form1: TForm1
  Left = 502
  Top = 212
  Width = 410
  Height = 279
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Button1: TButton
    Left = 39
    Top = 162
    Width = 93
    Height = 31
    Caption = 'Listen'
    TabOrder = 0
    OnClick = Button1Click
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 4
    Width = 153
    Height = 129
    Caption = 'Data Formats'
    TabOrder = 1
  end
  object RadioGroup2: TRadioGroup
    Left = 168
    Top = 4
    Width = 213
    Height = 217
    Caption = 'Data Request Modes'
    TabOrder = 2
  end
  object xfmtRTC: TCheckBox
    Left = 16
    Top = 28
    Width = 97
    Height = 17
    Caption = 'fmt_RTC'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = xfmtClick
  end
  object xfmtXMLRPC: TCheckBox
    Tag = 1
    Left = 16
    Top = 48
    Width = 125
    Height = 17
    Caption = 'fmt_XMLRPC'
    TabOrder = 4
    OnClick = xfmtClick
  end
  object xfmtJSONrpc1: TCheckBox
    Tag = 2
    Left = 16
    Top = 68
    Width = 141
    Height = 17
    Caption = 'fmt_JSONrpc1'
    TabOrder = 5
    OnClick = xfmtClick
  end
  object xfmtJSON: TCheckBox
    Tag = 4
    Left = 16
    Top = 108
    Width = 105
    Height = 17
    Caption = 'fmt_JSON'
    TabOrder = 6
    OnClick = xfmtClick
  end
  object xfmtJSONrpc2: TCheckBox
    Tag = 3
    Left = 16
    Top = 88
    Width = 141
    Height = 17
    Caption = 'fmt_JSONrpc2'
    TabOrder = 7
    OnClick = xfmtClick
  end
  object xreqContentBodyALL: TCheckBox
    Left = 180
    Top = 28
    Width = 193
    Height = 17
    Caption = 'req_contentBodyALL'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = xreqClick
  end
  object xreqContentBodyParams: TCheckBox
    Tag = 2
    Left = 180
    Top = 68
    Width = 193
    Height = 17
    Caption = 'req_contentBodyParams'
    TabOrder = 9
    OnClick = xreqClick
  end
  object xreqDirectJSON: TCheckBox
    Tag = 3
    Left = 180
    Top = 88
    Width = 193
    Height = 17
    Caption = 'req_directJSON'
    TabOrder = 10
    OnClick = xreqClick
  end
  object xreqQueryJSON: TCheckBox
    Tag = 4
    Left = 180
    Top = 108
    Width = 193
    Height = 17
    Caption = 'req_queryJSON'
    TabOrder = 11
    OnClick = xreqClick
  end
  object xreqQueryNameJSON: TCheckBox
    Tag = 5
    Left = 180
    Top = 128
    Width = 193
    Height = 17
    Caption = 'req_queryNameJSON'
    TabOrder = 12
    OnClick = xreqClick
  end
  object xreqQueryNameText: TCheckBox
    Tag = 6
    Left = 180
    Top = 148
    Width = 193
    Height = 17
    Caption = 'req_queryNameText'
    TabOrder = 13
    OnClick = xreqClick
  end
  object xreqUriParamsJSON: TCheckBox
    Tag = 7
    Left = 180
    Top = 168
    Width = 193
    Height = 17
    Caption = 'req_uriParamsJSON'
    TabOrder = 14
    OnClick = xreqClick
  end
  object xreqUriParamsText: TCheckBox
    Tag = 8
    Left = 180
    Top = 188
    Width = 193
    Height = 17
    Caption = 'req_uriParamsText'
    TabOrder = 15
    OnClick = xreqClick
  end
  object CheckBox1: TCheckBox
    Tag = 1
    Left = 180
    Top = 48
    Width = 193
    Height = 17
    Caption = 'req_contentBodyOptional'
    TabOrder = 16
    OnClick = xreqClick
  end
  object xAutoSessionCheck: TCheckBox
    Left = 8
    Top = 140
    Width = 149
    Height = 17
    Caption = 'Auto Session Check'
    Checked = True
    State = cbChecked
    TabOrder = 17
    OnClick = xAutoSessionCheckClick
  end
  object RtcHttpServer1: TRtcHttpServer
    ServerPort = '81'
    OnListenStart = RtcHttpServer1ListenStart
    OnListenStop = RtcHttpServer1ListenStop
    Left = 12
    Top = 179
  end
  object RtcFunctionGroup1: TRtcFunctionGroup
    Left = 84
    Top = 179
  end
  object RtcServerModule1: TRtcServerModule
    Server = RtcHttpServer1
    AutoSessionCheck = True
    ModuleFileName = '/mytest'
    FunctionGroup = RtcFunctionGroup1
    Left = 48
    Top = 179
  end
  object RtcFunction1: TRtcFunction
    Group = RtcFunctionGroup1
    FunctionName = 'hello'
    OnExecute = RtcFunction1Execute
    Left = 120
    Top = 180
  end
end
