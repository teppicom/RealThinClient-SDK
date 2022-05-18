object RtcRouterCheckForm: TRtcRouterCheckForm
  Left = 31
  Top = 109
  Width = 567
  Height = 387
  Caption = 'RTC Router "Proxy" LOG Check'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    549
    342)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 15
    Top = 20
    Width = 26
    Height = 16
    Caption = 'from'
  end
  object Label2: TLabel
    Left = 128
    Top = 20
    Width = 11
    Height = 16
    Caption = 'to'
  end
  object Button1: TButton
    Left = 256
    Top = 10
    Width = 92
    Height = 31
    Caption = 'Load'
    TabOrder = 0
    OnClick = Button1Click
  end
  object eEnd: TEdit
    Left = 153
    Top = 15
    Width = 80
    Height = 24
    TabOrder = 1
    Text = '50000'
  end
  object Memo1: TMemo
    Left = 7
    Top = 52
    Width = 530
    Height = 281
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Click "Load" to open and check a ".proxy.log" file ...')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object eStart: TEdit
    Left = 54
    Top = 15
    Width = 65
    Height = 24
    TabOrder = 3
    Text = '1'
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.proxy.log'
    Filter = 'RTC "proxy" LOG|*.proxy.log'
    Left = 168
    Top = 40
  end
end
