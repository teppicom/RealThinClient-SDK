object Form1: TForm1
  Left = 271
  Top = 109
  Width = 320
  Height = 152
  Caption = 'RTC BDE Demo Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 115
  TextHeight = 16
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 80
    Height = 16
    Caption = 'Listening Port'
  end
  object lblStatus: TLabel
    Left = 12
    Top = 48
    Width = 194
    Height = 16
    Caption = 'Press "Listen" to start the Server.'
  end
  object ePort: TEdit
    Left = 104
    Top = 12
    Width = 81
    Height = 24
    TabOrder = 0
    Text = '81'
  end
  object btnListen: TButton
    Left = 192
    Top = 8
    Width = 101
    Height = 33
    Caption = 'Listen'
    TabOrder = 1
    OnClick = btnListenClick
  end
  object RtcHttpServer1: TRtcHttpServer
    OnListenStart = RtcHttpServer1ListenStart
    OnListenStop = RtcHttpServer1ListenStop
    Left = 12
    Top = 72
  end
  object RtcServerModule1: TRtcServerModule
    Server = RtcHttpServer1
    Compression = cDefault
    ModuleFileName = '/biolife'
    FunctionGroup = RtcFunctionGroup1
    Left = 44
    Top = 72
  end
  object RtcFunctionGroup1: TRtcFunctionGroup
    Left = 76
    Top = 72
  end
  object rtcSelectFn: TRtcFunction
    Group = RtcFunctionGroup1
    FunctionName = 'select'
    OnExecute = rtcSelectFnExecute
    Left = 108
    Top = 72
  end
  object rtcSubmitFn: TRtcFunction
    Group = RtcFunctionGroup1
    FunctionName = 'submit'
    OnExecute = rtcSubmitFnExecute
    Left = 140
    Top = 72
  end
end
