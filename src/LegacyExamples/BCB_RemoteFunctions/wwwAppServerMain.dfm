object frmAppServer: TfrmAppServer
  Left = 0
  Top = 0
  Caption = 'frmAppServer'
  ClientHeight = 304
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object HttpServer: TRtcHttpServer
    MultiThreaded = True
    ServerPort = '80'
    Left = 32
    Top = 20
  end
  object FunctionGroup: TRtcFunctionGroup
    Left = 112
    Top = 20
  end
  object ServerModule: TRtcServerModule
    Server = HttpServer
    ModuleFileName = '/TestRtc'
    FunctionGroup = FunctionGroup
    Left = 200
    Top = 20
  end
  object funcReceiveMessage: TRtcFunction
    Group = FunctionGroup
    FunctionName = 'ReceiveMessage'
    OnExecute = funcReceiveMessageExecute
    Left = 304
    Top = 20
  end
end
