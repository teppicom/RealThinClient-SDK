object frmServer: TfrmServer
  Left = 223
  Top = 188
  Width = 364
  Height = 226
  Caption = 'RTC Web Forum HTTPS Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 149
    Width = 348
    Height = 41
    Align = alBottom
    TabOrder = 0
    object lblCliCon: TLabel
      Left = 86
      Top = 5
      Width = 93
      Height = 13
      Caption = 'Server not listening.'
    end
    object btStop: TButton
      Left = 40
      Top = 8
      Width = 33
      Height = 25
      Caption = 'Stop'
      TabOrder = 0
      OnClick = btStopClick
    end
    object btStart: TButton
      Left = 4
      Top = 8
      Width = 33
      Height = 25
      Caption = 'Start'
      TabOrder = 1
      OnClick = btStartClick
    end
  end
  object Memo1: TMemo
    Left = 8
    Top = 10
    Width = 337
    Height = 131
    Lines.Strings = (
      'After Server starts listening, open a Web Browser and go to ...'
      'https://localhost'
      ''
      'When you see a Forum mask, you can log in with '
      'username "admin" and password "admin" to enter the admin area.'
      ''
      'There, you can create new Users, set up the Forum Sections and '
      
        'Packages, then define access rights for each user to sections an' +
        'd '
      'packages.')
    ReadOnly = True
    TabOrder = 1
  end
end
