object Form1: TForm1
  Left = 294
  Top = 157
  Width = 504
  Height = 548
  Caption = 'RTC Windows HTTP Server API Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    486
    503)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 12
    Top = 160
    Width = 30
    Height = 16
    Caption = 'Stat1'
  end
  object Label2: TLabel
    Left = 12
    Top = 180
    Width = 30
    Height = 16
    Caption = 'Stat2'
  end
  object Label3: TLabel
    Left = 16
    Top = 84
    Width = 30
    Height = 16
    Caption = 'URL:'
  end
  object Label4: TLabel
    Left = 16
    Top = 12
    Width = 104
    Height = 16
    Caption = 'www Root Folder:'
  end
  object btnSartServer: TButton
    Left = 8
    Top = 36
    Width = 157
    Height = 37
    Caption = 'Open Queue'
    TabOrder = 2
    OnClick = btnSartServerClick
  end
  object btnStopServer: TButton
    Left = 336
    Top = 36
    Width = 137
    Height = 37
    Anchors = [akTop, akRight]
    Caption = 'Close Queue'
    TabOrder = 3
    OnClick = btnStopServerClick
  end
  object Memo1: TMemo
    Left = 12
    Top = 204
    Width = 461
    Height = 285
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'First, make sure tha the "www Root Folder" (1st edit field) '
      'points to a Folder from which you want to serve static files.'
      ''
      'Then, enter the URL you want to listen to and click "Add URL"'
      'and repeat until you'#39've added all URLs you want to respond to.'
      ''
      'After that, you can open a Web Browser and enter any URL'
      'you'#39've added with the "Add URL" button to get a response.'
      ''
      
        'Hint: You can do this without restrictions if you start this App' +
        'lication'
      
        'as Administrator ( right-click EXE and use "Run as Administrator' +
        '").'
      ''
      'To allow Normal Users to "Add URL", start this process as '
      'Administrator ( right-click EXE and use "Run as Administrator")'
      'and use  "Set URL Access" to add all URLs you want to allow.'
      '')
    TabOrder = 4
  end
  object btnAddRequestHandler: TButton
    Left = 172
    Top = 36
    Width = 157
    Height = 37
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Add Request Handler'
    TabOrder = 5
    OnClick = btnAddRequestHandlerClick
  end
  object btnAddURL: TButton
    Left = 8
    Top = 112
    Width = 89
    Height = 37
    Caption = 'Add URL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnAddURLClick
  end
  object btnRemovePermissions: TButton
    Left = 372
    Top = 112
    Width = 101
    Height = 37
    Anchors = [akTop, akRight]
    Caption = 'Remove URL'
    TabOrder = 6
    OnClick = btnRemovePermissionsClick
  end
  object eURL: TEdit
    Left = 52
    Top = 80
    Width = 421
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    Text = 'http://*:80/'
  end
  object btnSetPermissions: TButton
    Left = 104
    Top = 112
    Width = 125
    Height = 37
    Caption = 'Set URL Access'
    TabOrder = 8
    OnClick = btnSetPermissionsClick
  end
  object btnClearPermissions: TButton
    Left = 232
    Top = 112
    Width = 133
    Height = 37
    Anchors = [akTop, akRight]
    Caption = 'Clear URL Access'
    TabOrder = 9
    OnClick = btnClearPermissionsClick
  end
  object eFolder: TEdit
    Left = 132
    Top = 8
    Width = 341
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Text = 'c:\www'
    OnChange = eFolderChange
  end
  object RtcQuickJob1: TRtcQuickJob
    OnExecute = RtcQuickJob1Execute
    Left = 384
    Top = 156
  end
  object Timer1: TTimer
    Interval = 40
    OnTimer = Timer1Timer
    Left = 260
    Top = 156
  end
end
