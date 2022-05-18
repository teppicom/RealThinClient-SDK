object frmServer: TfrmServer
  Left = 174
  Top = 161
  Width = 450
  Height = 275
  Caption = 'RTC Web Forum Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 179
    Width = 432
    Height = 51
    Align = alBottom
    TabOrder = 0
    object lblCliCon: TLabel
      Left = 106
      Top = 6
      Width = 116
      Height = 16
      Caption = 'Server not listening.'
    end
    object btStop: TButton
      Left = 49
      Top = 10
      Width = 41
      Height = 31
      Caption = 'Stop'
      TabOrder = 0
      OnClick = btStopClick
    end
    object btStart: TButton
      Left = 5
      Top = 10
      Width = 41
      Height = 31
      Caption = 'Start'
      TabOrder = 1
      OnClick = btStartClick
    end
  end
  object Memo1: TMemo
    Left = 10
    Top = 12
    Width = 415
    Height = 162
    Lines.Strings = (
      'After Server starts listening, open a Web Browser and go to ...'
      'http://localhost'
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
