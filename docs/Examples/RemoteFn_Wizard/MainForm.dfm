object formMain: TformMain
  Left = 178
  Top = 167
  Width = 817
  Height = 600
  Caption = 'RTC Remote Functions Wizard'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 799
    Height = 555
    ActivePage = tabSetup
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = PagesChange
    object tabSetup: TTabSheet
      Caption = 'Setup'
      object Label1: TLabel
        Left = 16
        Top = 35
        Width = 102
        Height = 20
        Caption = 'Unit Filename'
      end
      object Label2: TLabel
        Left = 425
        Top = 31
        Width = 103
        Height = 20
        Caption = 'Unit Directory'
      end
      object Label3: TLabel
        Left = 30
        Top = 167
        Width = 91
        Height = 20
        Caption = 'Class Name'
      end
      object edtUnitFileName: TEdit
        Left = 128
        Top = 30
        Width = 257
        Height = 28
        Hint = 'Unit File Name'
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 0
      end
      object edtUnitPath: TEdit
        Left = 542
        Top = 30
        Width = 227
        Height = 28
        Hint = 'Path To Unit'
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        TabOrder = 1
      end
      object btnOpenFile: TButton
        Left = 252
        Top = 67
        Width = 129
        Height = 31
        Caption = 'Open File'
        TabOrder = 2
        OnClick = btnOpenFileClick
      end
      object edtClassName: TEdit
        Left = 128
        Top = 166
        Width = 257
        Height = 28
        Hint = 'Class Name Used by Unit'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
      object btnNewFile: TButton
        Left = 421
        Top = 67
        Width = 129
        Height = 31
        Caption = 'New File'
        TabOrder = 4
        OnClick = btnNewFileClick
      end
      object btnSetFont: TButton
        Left = 364
        Top = 295
        Width = 93
        Height = 31
        Caption = 'Set Font'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        OnClick = btnSetFontClick
      end
      object chkFuncLbx: TCheckBox
        Left = 384
        Top = 335
        Width = 188
        Height = 21
        Caption = 'Function List'
        Checked = True
        State = cbChecked
        TabOrder = 6
        OnClick = chkAutoSaveClick
      end
      object chkFuncVarMmo: TCheckBox
        Left = 384
        Top = 364
        Width = 218
        Height = 21
        Caption = 'Function Variables Memo'
        Checked = True
        State = cbChecked
        TabOrder = 7
        OnClick = chkAutoSaveClick
      end
      object chkFuncCodeMmo: TCheckBox
        Left = 384
        Top = 394
        Width = 267
        Height = 21
        Caption = 'Function Implementation Memo'
        Checked = True
        State = cbChecked
        TabOrder = 8
        OnClick = chkAutoSaveClick
      end
      object chkFuncSigEdt: TCheckBox
        Left = 384
        Top = 423
        Width = 238
        Height = 21
        Caption = 'Function Signature Edit box'
        Checked = True
        State = cbChecked
        TabOrder = 9
        OnClick = chkAutoSaveClick
      end
      object chkDiscardOptions: TCheckBox
        Left = 39
        Top = 463
        Width = 189
        Height = 21
        Caption = 'Discard all options'
        TabOrder = 10
      end
      object chkAsTmp: TCheckBox
        Left = 79
        Top = 374
        Width = 119
        Height = 21
        Caption = 'As TMP'
        Checked = True
        State = cbChecked
        TabOrder = 11
        OnClick = chkAsPasClick
      end
      object chkAsPas: TCheckBox
        Left = 79
        Top = 345
        Width = 119
        Height = 21
        Caption = 'As PAS'
        TabOrder = 12
        OnClick = chkAsPasClick
      end
      object chkAutoSave: TCheckBox
        Left = 39
        Top = 315
        Width = 189
        Height = 21
        Caption = 'AutoSave on Close'
        Checked = True
        State = cbChecked
        TabOrder = 13
        OnClick = chkAutoSaveClick
      end
      object chkPreserveReturnedValue: TCheckBox
        Left = 39
        Top = 276
        Width = 238
        Height = 21
        Caption = 'Preserve Returned Values'
        Checked = True
        State = cbChecked
        TabOrder = 14
      end
      object btnEditFuncs: TButton
        Left = 636
        Top = 67
        Width = 130
        Height = 31
        Caption = 'Edit Functions'
        TabOrder = 15
        OnClick = btnEditFuncsClick
      end
      object chkNewOnEnter: TCheckBox
        Left = 39
        Top = 423
        Width = 307
        Height = 21
        Caption = 'Insert New Function on Enter key'
        Checked = True
        State = cbChecked
        TabOrder = 16
      end
    end
    object tabFunctions: TTabSheet
      Caption = 'Functions'
      ImageIndex = 1
      object HSplit: TSplitter
        Left = 363
        Top = 70
        Width = 5
        Height = 450
        Align = alRight
        Color = clTeal
        ParentColor = False
      end
      object pnlSig: TPanel
        Left = 0
        Top = 0
        Width = 791
        Height = 70
        Align = alTop
        Color = clWhite
        TabOrder = 0
        DesignSize = (
          791
          70)
        object Label5: TLabel
          Left = 15
          Top = 20
          Width = 73
          Height = 20
          Caption = 'Function: '
        end
        object edtSig: TEdit
          Left = 89
          Top = 20
          Width = 684
          Height = 33
          Hint = 'Current Function Signature'
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          PopupMenu = popFuncList
          ShowHint = True
          TabOrder = 0
          OnChange = edtSigChange
          OnKeyDown = edtSigKeyDown
        end
      end
      object pnlRight: TPanel
        Left = 368
        Top = 70
        Width = 423
        Height = 450
        Align = alRight
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 1
        object VSplit: TSplitter
          Left = 1
          Top = 178
          Width = 421
          Height = 5
          Cursor = crVSplit
          Align = alTop
          Color = clTeal
          ParentColor = False
        end
        object mmoSigVars: TMemo
          Left = 1
          Top = 1
          Width = 421
          Height = 177
          Hint = 'Error/Variable Display Memo'
          TabStop = False
          Align = alTop
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 0
        end
        object mmoCode: TMemo
          Left = 1
          Top = 183
          Width = 421
          Height = 266
          Hint = 'Function Implementation Memo'
          TabStop = False
          Align = alClient
          ParentShowHint = False
          ReadOnly = True
          ScrollBars = ssBoth
          ShowHint = True
          TabOrder = 1
        end
      end
      object pnlLeft: TPanel
        Left = 0
        Top = 70
        Width = 363
        Height = 450
        Align = alClient
        TabOrder = 2
        DesignSize = (
          363
          450)
        object lbxSignatures: TListBox
          Left = 1
          Top = 1
          Width = 361
          Height = 360
          Hint = 'Signature List Box'
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -17
          Font.Name = 'Courier New'
          Font.Style = []
          ItemHeight = 20
          ParentFont = False
          ParentShowHint = False
          PopupMenu = popFuncList
          ShowHint = True
          TabOrder = 0
          OnClick = lbxSignaturesClick
        end
        object btnSaveToTmp: TButton
          Left = 14
          Top = 403
          Width = 135
          Height = 31
          Hint = 'Save all functions in .TMP file'
          Anchors = [akLeft, akBottom]
          Caption = 'Save To .TMP file'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = btnSaveToTmpClick
        end
        object btnSaveToPAS: TButton
          Left = 156
          Top = 403
          Width = 161
          Height = 31
          Hint = 'Overwrite original .PAS file'
          Anchors = [akLeft, akBottom]
          Caption = 'Save To .PAS file'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = btnSaveToPASClick
        end
        object btnAddFunction: TButton
          Left = 14
          Top = 368
          Width = 135
          Height = 30
          Hint = 'Save all functions in .TMP file'
          Anchors = [akLeft, akBottom]
          Caption = 'Add New Function'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = miNewSigClick
        end
        object btnDeleteFunction: TButton
          Left = 156
          Top = 368
          Width = 161
          Height = 30
          Hint = 'Save all functions in .TMP file'
          Anchors = [akLeft, akBottom]
          Caption = 'Delete Current Function'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnClick = miDeleteClick
        end
      end
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'pas'
    InitialDir = '.'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 172
    Top = 328
  end
  object popFuncList: TPopupMenu
    Left = 208
    Top = 328
    object miNewSig: TMenuItem
      Caption = 'New Function'
      ShortCut = 16462
      OnClick = miNewSigClick
    end
    object miDelete: TMenuItem
      Caption = 'Delete Function'
      ShortCut = 16452
      OnClick = miDeleteClick
    end
    object N1: TMenuItem
      Caption = '-'
      ShortCut = 189
    end
    object SaveTMPFile1: TMenuItem
      Caption = 'Save TMP File'
      ShortCut = 16468
      OnClick = btnSaveToTmpClick
    end
    object SavePASfile1: TMenuItem
      Caption = 'Save PAS file'
      ShortCut = 16464
      OnClick = btnSaveToPASClick
    end
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 248
    Top = 328
  end
end
