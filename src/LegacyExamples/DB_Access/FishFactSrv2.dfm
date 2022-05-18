object Form1: TForm1
  Left = 221
  Top = 164
  Width = 603
  Height = 409
  Caption = 'RTC Fish Fact Server 2 (single-threaded BDE version)'
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 585
    Height = 29
    Align = alTop
    Caption = 
      'The last SQL statement received from any Client will be shown he' +
      're.'
    TabOrder = 0
  end
  object mSQL: TMemo
    Left = 0
    Top = 29
    Width = 585
    Height = 303
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 332
    Width = 585
    Height = 32
    Align = alBottom
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 177
      Height = 16
      Caption = 'Allowed operations by clients:'
    end
    object xInsert: TCheckBox
      Left = 196
      Top = 8
      Width = 89
      Height = 17
      Caption = 'INSERT'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object xUpdate: TCheckBox
      Left = 296
      Top = 8
      Width = 97
      Height = 17
      Caption = 'UPDATE'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object xDelete: TCheckBox
      Left = 400
      Top = 8
      Width = 97
      Height = 17
      Caption = 'DELETE'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object RtcHttpServer1: TRtcHttpServer
    ServerPort = '81'
    Left = 8
    Top = 40
  end
  object RtcServerModule1: TRtcServerModule
    Server = RtcHttpServer1
    Compression = cDefault
    ModuleFileName = '/biolife'
    FunctionGroup = RtcFunctionGroup1
    Left = 40
    Top = 40
  end
  object RtcFunctionGroup1: TRtcFunctionGroup
    Left = 72
    Top = 40
  end
  object rtcSelectFn: TRtcFunction
    Group = RtcFunctionGroup1
    FunctionName = 'select'
    OnExecute = rtcSelectFnExecute
    Left = 104
    Top = 40
  end
  object rtcSubmitFn: TRtcFunction
    Group = RtcFunctionGroup1
    FunctionName = 'submit'
    OnExecute = rtcSubmitFnExecute
    Left = 136
    Top = 40
  end
  object Query1: TQuery
    DatabaseName = 'DBDEMOS'
    SessionName = 'Default'
    Left = 24
    Top = 80
  end
end
