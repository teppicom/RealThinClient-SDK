object Form1: TForm1
  Left = 205
  Top = 141
  Caption = 'Form1'
  ClientHeight = 363
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 115
  TextHeight = 16
  object Label1: TLabel
    Left = 20
    Top = 20
    Width = 59
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'FileName'
  end
  object Edit1: TEdit
    Left = 89
    Top = 20
    Width = 149
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 0
    Text = '/'
    OnKeyPress = Edit1KeyPress
  end
  object Memo1: TMemo
    Left = 20
    Top = 59
    Width = 661
    Height = 385
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object RtcHttpClient1: TRtcHttpClient
    MultiThreaded = True
    ServerAddr = 'www.realthinclient.com'
    ServerPort = '80'
    AutoConnect = True
    UseProxy = True
    Left = 272
    Top = 8
  end
  object DataReq: TRtcDataRequest
    AutoSyncEvents = True
    Client = RtcHttpClient1
    OnBeginRequest = DataReqBeginRequest
    OnDataReceived = DataReqDataReceived
    Left = 312
    Top = 8
  end
end
