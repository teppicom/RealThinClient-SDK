object Form1: TForm1
  Left = 215
  Top = 139
  Width = 360
  Height = 281
  Caption = 'RTC "FORM POST" Client'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 115
  TextHeight = 16
  object Label1: TLabel
    Left = 12
    Top = 56
    Width = 66
    Height = 16
    Caption = 'Response:'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 33
    Caption = 'POST'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 12
    Top = 80
    Width = 317
    Height = 145
    TabOrder = 1
  end
  object RtcHttpClient1: TRtcHttpClient
    ServerAddr = 'www.realthinclient.com'
    ServerPort = '80'
    AutoConnect = True
    Left = 104
    Top = 12
  end
  object RtcDataRequest1: TRtcDataRequest
    Client = RtcHttpClient1
    OnBeginRequest = RtcDataRequest1BeginRequest
    OnDataReceived = RtcDataRequest1DataReceived
    Left = 144
    Top = 12
  end
end
