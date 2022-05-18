object RTCPSendAccount: TRTCPSendAccount
  Left = 198
  Top = 181
  Width = 255
  Height = 194
  AutoSize = True
  BorderStyle = bsSizeToolWin
  Caption = 'Send Account'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PrintScale = poNone
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 17
    Top = 84
    Width = 76
    Height = 16
    Alignment = taRightJustify
    Caption = 'Auth.Code:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 37
    Top = 24
    Width = 56
    Height = 16
    Alignment = taRightJustify
    Caption = 'Gateway:'
  end
  object Label3: TLabel
    Left = 6
    Top = 52
    Width = 87
    Height = 16
    Alignment = taRightJustify
    Caption = 'Receiver ID:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lAccount: TLabel
    Left = 42
    Top = 0
    Width = 51
    Height = 16
    Alignment = taRightJustify
    Caption = 'Account:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object eAccount: TLabel
    Left = 100
    Top = 0
    Width = 65
    Height = 16
    Caption = 'eAccount'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object eGateway: TLabel
    Left = 100
    Top = 24
    Width = 61
    Height = 16
    Caption = 'eGateway'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object btnSend: TBitBtn
    Left = 96
    Top = 112
    Width = 141
    Height = 37
    Caption = 'Send'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 2
    OnClick = btnSendClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object btnCancel: TBitBtn
    Left = 0
    Top = 112
    Width = 93
    Height = 37
    TabOrder = 3
    OnClick = btnCancelClick
    Kind = bkCancel
  end
  object eCode: TEdit
    Left = 99
    Top = 80
    Width = 118
    Height = 24
    TabOrder = 1
    Text = '999-999-999'
  end
  object eUserID: TEdit
    Left = 99
    Top = 48
    Width = 82
    Height = 24
    TabOrder = 0
    Text = '999-999'
  end
end
