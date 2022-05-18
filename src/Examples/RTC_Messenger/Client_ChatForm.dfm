object ChatForm: TChatForm
  Left = 333
  Top = 145
  Width = 467
  Height = 396
  Caption = 'RTC Chat'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object mainPanel: TPanel
    Left = 0
    Top = 240
    Width = 449
    Height = 111
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 0
    object Panel3: TPanel
      Left = 357
      Top = 2
      Width = 90
      Height = 107
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object btnBuzz: TSpeedButton
        Left = 6
        Top = 5
        Width = 79
        Height = 31
        Hint = 'Buzz!'
        Caption = 'BUZZ!'
        Flat = True
        ParentShowHint = False
        ShowHint = True
        OnClick = btnBuzzClick
      end
      object btnSend: TBitBtn
        Left = 5
        Top = 44
        Width = 80
        Height = 51
        Caption = 'SEND'
        Enabled = False
        TabOrder = 0
        TabStop = False
        OnClick = btnSendClick
      end
    end
    object eEnter: TMemo
      Left = 2
      Top = 2
      Width = 355
      Height = 107
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = eEnterChange
      OnKeyDown = eEnterKeyDown
      OnKeyPress = eEnterKeyPress
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 449
    Height = 240
    Align = alClient
    BorderWidth = 2
    Color = 14408667
    TabOrder = 1
    object rtfChat: TRichEdit
      Left = 3
      Top = 3
      Width = 443
      Height = 234
      TabStop = False
      Align = alClient
      Color = 13565181
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
end
