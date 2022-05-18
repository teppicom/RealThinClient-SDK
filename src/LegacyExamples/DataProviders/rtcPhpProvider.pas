unit rtcPhpProvider;

interface

uses
  SysUtils, Classes,

  {$IFDEF VER120}
  Forms, FileCtrl, // D4
  {$ENDIF}
  {$IFDEF VER130}
  Forms, FileCtrl, // D5
  {$ENDIF}

  rtcTypes,
  rtcSystem,
  rtcLog,
  rtcInfo, rtcConn,
  rtcDataSrv,
  rtcPHP,

  rtcFileProvider;

type
  TPHP_Provider = class(TDataModule)
    PhpProvider: TRtcDataProvider;
    ServerLink: TRtcDataServerLink;

    procedure PhpProviderCheckRequest(Sender: TRtcConnection);
    procedure PhpProviderDisconnect(Sender: TRtcConnection);
    procedure PhpProviderListenStart(Sender: TRtcConnection);
    procedure PhpProviderListenStop(Sender: TRtcConnection);
    procedure PhpProviderDataReceived(Sender: TRtcConnection);

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

  private
    { Private declarations }
    FReady:boolean;

    FDllFolder:string;
    FIniFolder:RtcString;

    ExtList:TStringList;

  public
    { Public declarations }
    procedure ClearExts;
    procedure AddExts(a:string);

    property DllFolder:string read FDllFolder write FDllFolder;
    property IniFolder:RtcString read FIniFolder write FIniFolder;
  end;

function GetPHPProvider:TPHP_Provider;

implementation

{$R *.dfm}

var
  PHP_Provider: TPHP_Provider;

function GetPHPProvider:TPHP_Provider;
  begin
  if not assigned(PHP_Provider) then
    PHP_Provider:=TPHP_Provider.Create(nil);
  Result:=PHP_Provider;
  end;

procedure TPHP_Provider.ClearExts;
  begin
  ExtList.Clear;
  end;

procedure TPHP_Provider.AddExts(a: string);
  var
    elist,ext:string;
  begin
  elist:=Trim(a);
  while elist<>'' do
    begin
    if Pos(',',elist)>0 then
      begin
      ext:=UpperCase(Trim(Copy(elist,1,Pos(',',elist)-1)));
      if Copy(ext,1,1)<>'.' then ext:='.'+ext;
      Delete(elist,1,Pos(',',elist));
      elist:=Trim(elist);
      end
    else
      begin
      ext:=UpperCase(elist);
      if Copy(ext,1,1)<>'.' then ext:='.'+ext;
      elist:='';
      end;
    ExtList.Add(ext);
    end;
  end;

procedure TPHP_Provider.PhpProviderListenStart(Sender: TRtcConnection);
  begin
  try
    StartupPHP(DllFolder, IniFolder);
    FReady:=True;
    XLog('PHP Ready.');
  except
    on E:Exception do
      XLog('Error starting PHP Library: '+RtcString(E.Message));
    end;
  end;

procedure TPHP_Provider.PhpProviderListenStop(Sender: TRtcConnection);
  begin
  if FReady then
    begin
    try
      ShutDownPHP;
    except
      on E:Exception do
        XLog('Error shuting down PHP Library: '+RtcString(E.Message));
      end;
    FReady:=False;
    end;
  end;

procedure TPHP_Provider.PhpProviderCheckRequest(Sender: TRtcConnection);

  procedure CheckPHPFile;
    var
      a:integer;
      MyFileName:string;
      DocRoot:string;
    begin
    with TRtcDataServer(Sender) do
      begin
      { Check the request header when it started and accept it immediatelly,
        even if we won't be responding before the request body is received. }
      DocRoot:=GetDocRoot(Request.Host);
      if DocRoot<>'' then
        begin
        MyFileName:=RepairWebFileName(String(Request.FileName),DocRoot);
        if MyFileName<>'' then
          begin
          for a:=0 to ExtList.Count-1 do
            begin
            if UpperCase(Copy(MyFileName,
                              length(MyFileName)-length(ExtList.Strings[a])+1,
                              length(ExtList.Strings[a]))) = ExtList.Strings[a] then
              begin
              Accept; // Accept the request (we will be responding)
              Request.FileName:=RtcString(MyFileName);
              Break;
              end;
            end;
          end;
        end;
      end;
    end;

  begin
  with TRtcDataServer(Sender).Request do
    if (Method='GET') or (Method='POST') or (Method='HEAD') then
      CheckPHPFile;
  end;

procedure TPHP_Provider.PhpProviderDataReceived(Sender: TRtcConnection);

  procedure SendPHP(SendContent:boolean=True);
    var
      s,s2:RtcString;
      TC:Cardinal;

    function Parse_PHP(Sender:TRtcDataServer; const filename:string):RtcString;
      var
        PHP:TRtcPHP;
        s:RtcString;
        a,TC:longword;
      begin
      PHP:=TRtcPHP.Create;
      try
        PHP.LocalAddr:=Sender.LocalAddr;
        PHP.LocalPort:=Sender.LocalPort;
        PHP.PeerAddr:=Sender.PeerAddr;
        PHP.PeerPort:=Sender.PeerPort;

        PHP.Request:=Sender.Request;
        PHP.RequestBody:=Sender.Read;

        PHP.DocumentRoot:=RtcString(GetDocRoot(PHP.Request.Host));

        PHP.FileName:=RtcString(filename);

        s2:='';
        try
          s:='';
          with Sender.Request.Query do if Text<>'' then
            begin
            s:='QUERY >> (Delimiter='+Delimiter+')'#13#10;
            for a:=0 to ItemCount-1 do
              s:=s+Int2Str(a+1)+'. "'+ItemName[a]+'" = <'+ItemValue[a]+'>'+#13#10;
            end;
        except
          on E:Exception do
            begin
            XLog('Request.Query error: '+RtcString(E.Message),'PHP');
            s:='';
            end;
          end;
        s2:=s2+s;

        try
          s:='';
          with Sender.Request.Cookie do if Text<>'' then
            begin
            s:='COOKIE >> (Delimiter='+Delimiter+')'#13#10;
            for a:=0 to ItemCount-1 do
              s:=s+Int2Str(a+1)+'. "'+ItemName[a]+'" = <'+ItemValue[a]+'>'+#13#10;
            end;
        except
          on E:Exception do
            begin
            XLog('Request.Cookie error: '+RtcString(E.Message),'PHP');
            s:='';
            end;
          end;
        s2:=s2+s;

        if PHP.RequestBody<>'' then
          begin
          try
            s:='';
            with Sender.Request.Params do
              begin
              Text:=PHP.RequestBody;
              s:=#13#10'PARAMS >> (Delimiter='+Delimiter+')'#13#10;
              for a:=0 to ItemCount-1 do
                s:=s+Int2Str(a+1)+'. "'+ItemName[a]+'" = <'+ItemValue[a]+'>'+#13#10;
              s:='<'+PHP.RequestBody+'>'+s;
              end;
          except
            on E:Exception do
              begin
              XLog('Request.Params error: '+RtcString(E.Message),'PHP');
              s:='<'+PHP.RequestBody+'>';
              end;
            end;
          s2:=s2+s;
          end;

        XLog('Exec '+Sender.Request.Host+' > '+Sender.Request.URI+#13#10+
             Sender.Request.HeaderText+
             s2,
             'PHP');

        TC:=GetTickTime;
        Result:=PHP.Execute;
        TC:=GetTickTime-TC;

        Sender.Response.HeaderText:=PHP.ResultHeader;

        s2:='';
        try
          s:='';
          with Sender.Response.Cookie do if Text<>'' then
            begin
            s:='COOKIE >> (Delimiter='+Delimiter+')'#13#10;
            for a:=0 to ItemCount-1 do
              s:=s+Int2Str(a+1)+'. "'+ItemName[a]+'" = <'+ItemValue[a]+'>'+#13#10;
            end;
        except
          on E:Exception do
            begin
            XLog('Response.Cookie error: '+RtcString(E.Message),'PHP');
            s:='';
            end;
          end;
        s2:=s2+s;

        XLog('Done '+Sender.Request.Host+' > '+Sender.Request.URI+' > '+Int2Str(TC)+' ms'#13#10+
             Sender.Response.HeaderText+
             s2,
             'PHP');

      finally
        PHP.Free;
        end;
      end;

    begin
    with TRtcDataServer(Sender) do
      begin
      TC:=GetTickTime;
      try
        if FReady then
          s:=Parse_PHP(TRtcDataServer(Sender),
                       StringReplace(GetDocRoot(Request.Host)+String(Request.FileName),'/','\',[rfReplaceAll]))
        else
          s:='ERROR! PHP engine not initialized.';
      except
        on E:Exception do
          s:=RtcString(E.Message);
        end;
      TC:=GetTickTime-TC;
      XLog('SEND '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' '+Int2Str(length(s))+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" > '+Int2Str(TC)+' ms');

      if Response.ContentLength=0 then
        Response.ContentLength:=length(s);

      Response.SendContent:=SendContent;
      WriteHeader;

      if SendContent and (length(s)>0) then
        Write(s);
      end;
    end;

  begin
  // start processing when complete request body was received
  with TRtcDataServer(Sender) do
    if Request.Complete then
      SendPHP(Request.Method<>'HEAD');
  end;

procedure TPHP_Provider.PhpProviderDisconnect(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.DataSize>Request.DataIn then
      begin
      // did not receive a complete request
      XLog('ERR! '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' 0'+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> DISCONNECTED while receiving a Request ('+Int2Str(Request.DataIn)+' of '+Int2Str(Request.DataSize)+' bytes received).');
      end
    else if Response.DataSize>Response.DataOut then
      begin
      // did not send a complete result
      XLog('ERR! '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' -'+Int2Str(Response.DataSize-Response.DataOut)+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> DISCONNECTED while sending a Result ('+Int2Str(Response.DataOut)+' of '+Int2Str(Response.DataSize)+' bytes sent).');
      end;
    end;
  end;

procedure TPHP_Provider.DataModuleCreate(Sender: TObject);
  begin
  ExtList:=TStringList.Create;
  end;

procedure TPHP_Provider.DataModuleDestroy(Sender: TObject);
  begin
  PHP_Provider:=nil;

  ClearExts;
  ExtList.Free;
  end;

initialization
finalization
if assigned(PHP_Provider) then
  begin
  PHP_Provider.Free;
  PHP_Provider:=nil;
  end;
end.
