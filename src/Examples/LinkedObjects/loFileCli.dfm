object loFileClient: TloFileClient
  Left = 448
  Top = 185
  Width = 381
  Height = 255
  Caption = 'loFileClient'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  DesignSize = (
    363
    210)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 98
    Height = 16
    Caption = 'Local File Name'
  end
  object lStatus: TLabel
    Left = 8
    Top = 112
    Width = 21
    Height = 16
    Caption = '???'
  end
  object Gauge1: TGauge
    Left = 12
    Top = 172
    Width = 341
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Progress = 0
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 109
    Height = 16
    Caption = 'Destination Folder'
  end
  object lActive: TLabel
    Left = 180
    Top = 140
    Width = 40
    Height = 16
    Caption = 'lActive'
  end
  object eFileName: TEdit
    Left = 8
    Top = 36
    Width = 315
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnOpenFile: TBitBtn
    Left = 329
    Top = 36
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = btnOpenFileClick
  end
  object btnUpload: TButton
    Left = 12
    Top = 136
    Width = 75
    Height = 29
    Caption = 'Upload'
    TabOrder = 2
    OnClick = btnUploadClick
  end
  object btnCancel: TButton
    Left = 96
    Top = 136
    Width = 75
    Height = 29
    Caption = 'Cancel'
    Enabled = False
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object ePath: TEdit
    Left = 8
    Top = 84
    Width = 345
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 4
  end
  object Link: TRtcLinkedModule
    RemoteClass = 'FileServer'
    OnSetEvent = LinkSetEvent
    OnSetProp = LinkSetProp
    OnCallMethod = LinkCallMethod
    Left = 112
    Top = 4
  end
  object OpenDialog1: TOpenDialog
    Left = 148
    Top = 4
  end
end
