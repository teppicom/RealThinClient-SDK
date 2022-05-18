object frmServer: TfrmServer
  Left = 125
  Top = 187
  Width = 456
  Height = 320
  AutoSize = True
  Caption = 'RTC Web Package Manager Demo'
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
    Top = 225
    Width = 438
    Height = 50
    Align = alBottom
    TabOrder = 0
    object lblCliCon: TLabel
      Left = 116
      Top = 6
      Width = 116
      Height = 16
      Caption = 'Server not listening.'
    end
    object btStop: TButton
      Left = 59
      Top = 10
      Width = 41
      Height = 31
      Caption = 'Stop'
      TabOrder = 0
      OnClick = btStopClick
    end
    object btStart: TButton
      Left = 10
      Top = 10
      Width = 40
      Height = 31
      Caption = 'Start'
      TabOrder = 1
      OnClick = btStartClick
    end
  end
  object Memo1: TMemo
    Left = 10
    Top = 0
    Width = 420
    Height = 211
    Lines.Strings = (
      
        'After this Server starts listening, open a Web Browser and go to' +
        ':'
      'http://localhost'
      ''
      'You will see a simple form with Username and Login entry fields.'
      
        'If you haven'#39't modified the "users.data" file (it'#39's a simple tex' +
        't file),'
      'you can log in with username "admin" and password "admin".'
      ''
      'After you have logged in, you can create new users and packages,'
      'then define access rights for each user to each package.'
      ''
      
        'You can then log in with any of the created users and you will h' +
        'ave'
      'access to packages granted to that user.')
    TabOrder = 1
  end
end
