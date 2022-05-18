object Form1: TForm1
  Left = 196
  Top = 154
  Width = 361
  Height = 148
  Caption = 'Local Object Manager test'
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
  PixelsPerInch = 120
  TextHeight = 16
  object btnNew: TButton
    Left = 24
    Top = 20
    Width = 121
    Height = 33
    Caption = 'New File Client'
    TabOrder = 0
    OnClick = btnNewClick
  end
  object Button1: TButton
    Left = 24
    Top = 60
    Width = 121
    Height = 33
    Caption = 'New File Server'
    TabOrder = 1
    OnClick = Button1Click
  end
end
