object Form1: TForm1
  Left = 205
  Top = 125
  Width = 577
  Height = 383
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RtcHttpServer1: TRtcHttpServer
    ServerPort = '80'
    Left = 16
    Top = 16
  end
  object RtcDataProvider1: TRtcDataProvider
    Server = RtcHttpServer1
    OnCheckRequest = RtcDataProvider1CheckRequest
    Left = 112
    Top = 16
  end
end
