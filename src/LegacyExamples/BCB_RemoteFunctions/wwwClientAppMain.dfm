object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 307
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 12
    Top = 8
    Width = 209
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
    OnKeyPress = Edit1KeyPress
  end
  object Memo1: TMemo
    Left = 12
    Top = 36
    Width = 513
    Height = 260
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object HttpClient: TRtcHttpClient
    MultiThreaded = True
    ServerAddr = 'localhost'
    ServerPort = '80'
    ReconnectOn.ConnectLost = True
    AutoConnect = True
    UseProxy = True
    Left = 232
    Top = 4
  end
  object ClientModule: TRtcClientModule
    AutoSyncEvents = True
    Client = HttpClient
    AutoRepost = 2
    ModuleFileName = '/TestRtc'
    Left = 300
    Top = 4
  end
  object Result: TRtcResult
    OnReturn = ResultReturn
    Left = 360
    Top = 4
  end
end
