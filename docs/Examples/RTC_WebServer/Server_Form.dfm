object WebServerForm: TWebServerForm
  Left = 374
  Top = 182
  Width = 655
  Height = 408
  Caption = 'RealThinClient : Web + Gateway + Messenger Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PrintScale = poNone
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Panel8: TPanel
    Left = 0
    Top = 0
    Width = 637
    Height = 26
    Cursor = crHandPoint
    Align = alTop
    BevelOuter = bvNone
    Caption = 
      'Built using only RealThinClient SDK from http://www.realthinclie' +
      'nt.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = InfoPanelClick
  end
  object Panel9: TPanel
    Left = 0
    Top = 340
    Width = 637
    Height = 23
    Cursor = crHandPoint
    Align = alBottom
    BevelOuter = bvNone
    Caption = 
      'For more info and the latest version, go to: http://www.realthin' +
      'client.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = InfoPanelClick
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 26
    Width = 637
    Height = 314
    ActivePage = TabSheet4
    Align = alClient
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'File Locations (Virtual Hosts)'
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 629
        Height = 283
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel5'
        TabOrder = 0
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 629
          Height = 46
          Align = alTop
          BevelOuter = bvLowered
          TabOrder = 1
          object Label6: TLabel
            Left = 8
            Top = 10
            Width = 93
            Height = 16
            Caption = 'Virtual Hosts:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label1: TLabel
            Left = 8
            Top = 25
            Width = 213
            Height = 16
            Caption = 'HostName = Document Root Folder'
          end
          object Label7: TLabel
            Left = 96
            Top = 10
            Width = 60
            Height = 16
            Caption = '* = Default'
          end
        end
        object eVirtualHosts: TMemo
          Left = 0
          Top = 46
          Width = 629
          Height = 237
          Align = alClient
          Lines.Strings = (
            '* = .\')
          ScrollBars = ssBoth
          TabOrder = 0
          WordWrap = False
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'File Types'
      ImageIndex = 1
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 629
        Height = 283
        Align = alClient
        TabOrder = 0
        object Splitter1: TSplitter
          Left = 129
          Top = 1
          Height = 281
        end
        object Panel3: TPanel
          Left = 1
          Top = 1
          Width = 128
          Height = 281
          Align = alLeft
          BevelOuter = bvNone
          Caption = 'Panel3'
          TabOrder = 0
          object Panel6: TPanel
            Left = 0
            Top = 0
            Width = 128
            Height = 31
            Align = alTop
            BevelOuter = bvLowered
            TabOrder = 1
            object Label8: TLabel
              Left = 8
              Top = 10
              Width = 90
              Height = 16
              Caption = 'Index pages:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
          end
          object eIndexPages: TMemo
            Left = 0
            Top = 31
            Width = 128
            Height = 250
            Align = alClient
            Lines.Strings = (
              'default.html'
              'default.htm'
              'default.php'
              'default.phtm'
              ''
              'index.html'
              'index.htm'
              'index.php'
              'index.phtm')
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
        end
        object Panel10: TPanel
          Left = 132
          Top = 1
          Width = 496
          Height = 281
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object eContentTypes: TMemo
            Left = 0
            Top = 51
            Width = 496
            Height = 230
            Align = alClient
            Lines.Strings = (
              'c,pas,asc,txt,ini = text/plain'
              'html,htm = text/html'
              'ics,ifb = text/calendar'
              'css = text/css'
              'rtx,rtf = text/richtext'
              ''
              'bmp = image/bmp'
              'gif = image/gif'
              'jpeg,jpg,jpe = image/jpeg'
              'png = image/png'
              'svg = image/svg+xml'
              'tiff,tif = image/tiff'
              'qtif,qti = image/x-quicktime'
              'ppm = image/x-portable-pixmap'
              'xbm = image/x-xbitmap'
              'svg, svgz = image/svg+xml'
              ''
              'mpeg,mpg,mpe = video/mpeg'
              'mng = video/x-mng'
              'mov,qt = video/quicktime'
              'asf,asx = video/x-ms-asf'
              'wm = video/x-ms-wm'
              'wmv = video/x-ms-wmv'
              'wvx = video/x-ms-wvx'
              ''
              'mp3 = audio/mpeg'
              'wma = audio/x-ms-wma'
              'wax = audio/x-ms-wax'
              'au,snd = audio/basic'
              'rpm = audio/x-pn-realaudio-plugin'
              ''
              'rm = application/octet-stream'
              ''
              'swf = application/x-shockwave-flash'
              'spl = application/futuresplash'
              'dir,dxr,dcr = application/x-director'
              ''
              'js = application/x-javascript'
              'xml,xsl = application/xml'
              'xhtml,xht = application/xhtml+xml'
              'xslt = application/xslt+xml'
              ''
              'pdf = application/pdf'
              ''
              'zip = application/zip'
              'gz = application/gzip'
              'jar = application/java-archive'
              ''
              'bin,dms,lha,lzh,exe,class,so,dll = application/octet-stream'
              ''
              'hqx = application/mac-binhex40'
              'mathml = application/mathml+xml'
              'p7s = application/pkcs7-signature'
              'ai,eps,ps = application/postscript'
              'rdf = application/rdf+xml'
              'xul = application/vnd.mozilla.xul+xml'
              'xpi = application/x-xpinstall'
              'mtx = application/x-mtx'
              'rpj = application/vnd.rn-realplayer-javascript'
              ''
              '* =')
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
          object Panel11: TPanel
            Left = 0
            Top = 0
            Width = 496
            Height = 51
            Align = alTop
            BevelOuter = bvLowered
            TabOrder = 1
            object Label13: TLabel
              Left = 8
              Top = 10
              Width = 236
              Height = 16
              Caption = 'Content Types for File Extensions:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object Label14: TLabel
              Left = 8
              Top = 30
              Width = 156
              Height = 16
              Caption = 'extlist = Content-Type Text'
            end
            object Label15: TLabel
              Left = 212
              Top = 10
              Width = 60
              Height = 16
              Caption = '* = Default'
            end
          end
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Web Applications'
      ImageIndex = 2
      object Panel12: TPanel
        Left = 0
        Top = 37
        Width = 629
        Height = 37
        Align = alTop
        TabOrder = 1
        object Label3: TLabel
          Left = 12
          Top = 10
          Width = 101
          Height = 16
          Caption = 'ISAPI Extensions'
        end
        object eISAPIExtensions: TEdit
          Left = 124
          Top = 7
          Width = 233
          Height = 24
          Hint = 'Files with this extensions will be sent without HTTP Header.'
          TabOrder = 0
          Text = 'dll'
        end
        object btnUnload: TButton
          Left = 364
          Top = 5
          Width = 81
          Height = 25
          Caption = 'Unload ALL'
          Enabled = False
          TabOrder = 1
          OnClick = btnUnloadClick
        end
      end
      object Panel13: TPanel
        Left = 0
        Top = 0
        Width = 629
        Height = 37
        Align = alTop
        TabOrder = 0
        object xMsgServer: TCheckBox
          Left = 12
          Top = 9
          Width = 553
          Height = 17
          Caption = 
            'Enable RTC Messenger Server: "/$MSG" request will be reserved fo' +
            'r the Messenger.'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Install / Run'
      ImageIndex = 3
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 629
        Height = 281
        Align = alTop
        TabOrder = 0
        object lblCliCon: TLabel
          Left = 132
          Top = 34
          Width = 313
          Height = 28
          AutoSize = False
          Caption = 'Server not listening.'
          WordWrap = True
        end
        object Label11: TLabel
          Left = 8
          Top = 84
          Width = 114
          Height = 16
          Caption = 'Total bytes in + out:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblDataInOut: TLabel
          Left = 132
          Top = 84
          Width = 7
          Height = 16
          Caption = '0'
        end
        object Label12: TLabel
          Left = 8
          Top = 112
          Width = 371
          Height = 16
          Caption = 
            'Click "Install" to install this as a Service, "Uninsall" to remo' +
            've it.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label9: TLabel
          Left = 8
          Top = 10
          Width = 423
          Height = 16
          Caption = 
            'Click "Listen" to start listening for clients, "Stop" to close a' +
            'll connections.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label16: TLabel
          Left = 12
          Top = 205
          Width = 420
          Height = 64
          Caption = 
            'NOTE: If you should need to change settings while your Server wa' +
            's running as a Service, call the EXE with "-S" as parameter. Aft' +
            'er you'#39've changed all the settings you want, click "SAVE Setting' +
            's" and restart the Service from the Windows Services Control Pan' +
            'el.  '
          WordWrap = True
        end
        object btnListen: TButton
          Left = 8
          Top = 34
          Width = 57
          Height = 25
          Caption = 'Listen'
          Default = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = btnListenClick
        end
        object btnStop: TButton
          Left = 68
          Top = 34
          Width = 55
          Height = 25
          Caption = 'Stop'
          Enabled = False
          TabOrder = 1
          OnClick = btnStopClick
        end
        object btnInstall: TButton
          Left = 8
          Top = 132
          Width = 57
          Height = 25
          Caption = 'Install'
          TabOrder = 2
          OnClick = btnInstallClick
        end
        object btnDeinstall: TButton
          Left = 88
          Top = 132
          Width = 81
          Height = 25
          Caption = 'Uninstall'
          TabOrder = 3
          OnClick = btnDeinstallClick
        end
        object btnSave: TButton
          Left = 8
          Top = 170
          Width = 125
          Height = 25
          Caption = 'SAVE Settings'
          TabOrder = 4
          OnClick = btnSaveClick
        end
      end
    end
  end
end
