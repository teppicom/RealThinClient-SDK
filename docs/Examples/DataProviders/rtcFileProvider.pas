unit rtcFileProvider;

interface

{$include rtcDefs.inc}

{ If you want to use a FileStream implementation, which leaves the files open
  while sending them to the client (instead of the default implementation which
  opens and closes each file as needed), define the RTC_FILESTREAM directive. }

{.$DEFINE RTC_FILESTREAM}

uses
  SysUtils, Classes,

  {$IFDEF VER120}
  Forms, FileCtrl, // D4
  {$ENDIF}
  {$IFDEF VER130}
  Forms, FileCtrl, // D5
  {$ENDIF}

  rtcTypes, rtcSystem, rtcLog,
  rtcInfo, rtcConn, rtcDataSrv;

var
  MAX_SEND_BLOCK_SIZE:int64=1460*44; // larger files will be sent in smaller blocks
  MAX_ACCEPT_BODY_SIZE:int64=128000;

type
  TStringObject=class
    public
      value:String;
    end;

  TFile_Provider = class(TDataModule)
    FileProvider: TRtcDataProvider;
    TimeProvider: TRtcDataProvider;
    ServerLink: TRtcDataServerLink;

    procedure FileProviderDisconnect(Sender: TRtcConnection);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure FileProviderSendBody(Sender: TRtcConnection);
    procedure TimeProviderDataReceived(Sender: TRtcConnection);
    procedure FileProviderCheckRequest(Sender: TRtcConnection);
    procedure TimeProviderCheckRequest(Sender: TRtcConnection);

  private
    { Private declarations }
    HostList:TStringList;
    PageList:TStringList;
    ExtList:TStringList;
    CTypesList:TList;

  protected
    function GetDocRoot(Host: RtcString): String;
    function GetContentType(FName: String): RtcString;

    function RepairWebFileName(FileName,DocRoot:String):String;

  public
    { Public declarations }
    procedure ClearHosts;
    procedure AddHost(a:String);

    procedure ClearIndexPages;
    procedure AddIndexPage(a:String);

    procedure ClearContentTypes;
    procedure AddContentType(a:String);
  end;

function GetDocRoot(Host: RtcString): String;
function RepairWebFileName(FileName,DocRoot:String):String;

function GetFileProvider:TFile_Provider;

implementation

{$R *.dfm}

var
  File_Provider: TFile_Provider;

function GetFileProvider:TFile_Provider;
  begin
  if not assigned(File_Provider) then
    File_Provider:=TFile_Provider.Create(nil);
  Result:=File_Provider;
  end;

function RepairWebFileName(FileName,DocRoot:String):String;
  begin
  Result:=GetFileProvider.RepairWebFileName(FileName,DocRoot);
  end;

function GetDocRoot(Host: RtcString): String;
  begin
  Result:=GetFileProvider.GetDocRoot(Host);
  end;

procedure TFile_Provider.DataModuleCreate(Sender: TObject);
  begin
  HostList:=TStringList.Create;
  HostList.Sorted:=True;
  PageList:=TStringList.Create;
  ExtList:=TStringList.Create;
  CTypesList:=TList.Create;
  end;

procedure TFile_Provider.DataModuleDestroy(Sender: TObject);
  begin
  File_Provider:=nil;

  ClearHosts;
  HostList.Free;

  ClearIndexPages;
  PageList.Free;

  ClearContentTypes;
  ExtList.Free;
  CTypesList.Free;
  end;

procedure TFile_Provider.ClearHosts;
  var
    a:integer;
  begin
  for a:=0 to HostList.Count-1 do
    with TStringObject(HostList.Objects[a]) do
      begin
      value:='';
      Free;
      end;
  HostList.Clear;
  end;

procedure TFile_Provider.ClearIndexPages;
  begin
  PageList.Clear;
  end;

procedure TFile_Provider.ClearContentTypes;
  var
    a:integer;
  begin
  for a:=0 to CTypesList.Count-1 do
    with TStringObject(CTypesList.Items[a]) do
      begin
      value:='';
      Free;
      end;
  CTypesList.Clear;
  ExtList.Clear;
  end;

procedure TFile_Provider.AddHost(a: String);
  var
    loc:integer;
    hname,hdir:String;
    odir:TStringObject;
  begin
  loc:=Pos('=',a);
  if loc>0 then
    begin
    hname:=UpperCase(Trim(Copy(a,1,loc-1)));
    hdir:=Trim(Copy(a,loc+1,MaxInt));
    hdir:=StringReplace(hdir,'/',FOLDER_DELIMITER,[rfReplaceAll]);

    // Resolve a problem with relative paths
    if (Pos(':',hdir)=0) and (Copy(hdir,1,1)<>FOLDER_DELIMITER) then
      hdir:=ExtractFilePath(AppFileName)+hdir;

    // Remove trailing \
    if Copy(hdir,length(hdir),1)=FOLDER_DELIMITER then
      Delete(hdir,length(hdir),1);

    hdir:=ExpandUNCFileName(hdir);

    odir:=TStringObject.Create;
    odir.value:=hdir;

    HostList.AddObject(hname,odir);
    end;
  HostList.Sort;
  HostList.Sorted:=True;
  end;

procedure TFile_Provider.AddIndexPage(a: String);
  begin
  if Trim(a)<>'' then
    PageList.Add(Trim(a));
  end;

procedure TFile_Provider.AddContentType(a: String);
  var
    elist,ext:String;
    loc:integer;
    htext:String;
    octype:TStringObject;
  begin
  loc:=Pos('=',a);
  if loc>0 then
    begin
    elist:=Trim(Copy(a,1,loc-1));
    htext:=Trim(Copy(a,loc+1,MaxInt));

    octype:=TStringObject.Create;
    octype.value:=htext;

    CTypesList.Add(octype);

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
      ExtList.AddObject(ext,octype);
      end;
    end;
  ExtList.Sort;
  ExtList.Sorted:=True;
  end;

function TFile_Provider.GetContentType(FName: String): RtcString;
  var
    loc:integer;
    ext:String;
  begin
  ext:=UpperCase(ExtractFileExt(FName));
  loc:=ExtList.IndexOf(ext);
  if loc>=0 then
    Result:=RtcString(TStringObject(ExtList.Objects[loc]).value)
  else
    begin
    loc:=ExtList.IndexOf('*');
    if loc>=0 then
      Result:=RtcString(TStringObject(ExtList.Objects[loc]).value)
    else
      Result:='';
    end;
  end;

function TFile_Provider.GetDocRoot(Host: RtcString): String;
  var
    loc:integer;
  begin
  Host:=UpperCase(Host);
  loc:=HostList.IndexOf(String(Host));
  if loc>=0 then
    Result:=TStringObject(HostList.Objects[loc]).value
  else
    begin
    loc:=HostList.IndexOf('*');
    if loc>=0 then
      Result:=TStringObject(HostList.Objects[loc]).value
    else
      Result:='';
    end;
  end;

function TFile_Provider.RepairWebFileName(FileName,DocRoot:String):String;
  var
    a:integer;
    FullName:String;
  begin
  FileName:=StringReplace(FileName,'/',FOLDER_DELIMITER,[rfreplaceall]);

  FullName:=ExpandUNCFileName(DocRoot+FileName);

  if Copy(FileName,length(FileName),1)=FOLDER_DELIMITER then
    if Copy(FullName,length(FullName),1)<>FOLDER_DELIMITER then
      FullName:=FullName+FOLDER_DELIMITER;

  // Check if our path is inside DocRoot
  if (Pos(FOLDER_DELIMITER+'..',FullName)>0) or
     (UpperCase(Copy(FullName,1,length(DocRoot)))<>UpperCase(DocRoot)) then
    begin
    Result:='';
    Exit;
    end;

  FileName:=FullName;
  Delete(FileName,1,length(DocRoot));
  // Check if FileName is a folder with missing '\' at the end.
  if Copy(FileName,length(FileName),1)<>FOLDER_DELIMITER then
    if DirectoryExists(DocRoot+FileName) then
      begin
      Result:='';
      Exit;
      end;

  // Check if FileName is a folder with existing index file
  if Copy(FileName,length(FileName),1)=FOLDER_DELIMITER then
    begin
    for a:=0 to PageList.Count-1 do
      begin
      if File_Exists(FullName+PageList.Strings[a]) then
        begin
        FileName:=FileName+PageList.Strings[a];
        Break;
        end;
      end;
    end;

  Result:=StringReplace(FileName,FOLDER_DELIMITER,'/',[rfreplaceall]);
  end;

{ *** FILE PROVIDER *** }

procedure TFile_Provider.FileProviderCheckRequest(Sender: TRtcConnection);

  procedure CheckDiskFile(Sender: TRtcDataServer);
    var
      fsize:int64;
      Content_Type:RtcString;
      rang_type, rang_from, rang_to:RtcString;
      rang_start, rang_end:int64;

      MyFileName,
      DocRoot:String;

    function RepairFileName(FileName:String):String;
      var
        a:integer;
        FullName, fname:String;
      begin
      FileName:=StringReplace(FileName,'/',FOLDER_DELIMITER,[rfreplaceall]);

      FullName:=ExpandUNCFileName(DocRoot+FileName);

      if Copy(FileName,length(FileName),1)=FOLDER_DELIMITER then
        if Copy(FullName,length(FullName),1)<>FOLDER_DELIMITER then
          FullName:=FullName+FOLDER_DELIMITER;

      if (Pos(FOLDER_DELIMITER+'..',FullName)>0) or
         (UpperCase(Copy(FullName,1,length(DocRoot)))<>UpperCase(DocRoot)) then
        begin
        Sender.Accept;

        XLog('DENY '+Sender.PeerAddr+' > '+Sender.Request.Host+
             ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
             ' 0'+
             ' REF "'+Sender.Request.Referer+'"'+
             ' AGENT "'+Sender.Request.Agent+'" > Invalid FileName: "'+RtcString(FullName)+'".');

        Sender.Response.Status(403,'Forbidden');
        Sender.Write('Status 403: Forbidden');

        Result:='';
        fsize:=-1;
        Exit;
        end
      else
        begin
        FileName:=FullName;
        Delete(FileName,1,length(DocRoot));
        end;

      if Copy(FileName,length(FileName),1)<>FOLDER_DELIMITER then
        if DirectoryExists(DocRoot+FileName) then
          begin
          Sender.Accept;

          Sender.Response.Status(301,'Moved Permanently');
          Sender.Response['LOCATION']:= Sender.Request.FileName+'/';
          Sender.Write('Status 301: Moved Permanently');

          Result:='';
          Exit;
          end;

      if Copy(FileName,length(FileName),1)=FOLDER_DELIMITER then
        begin
        for a:=0 to PageList.Count-1 do
          begin
          fname:=FullName+PageList.Strings[a];
          fsize:=File_Size(fname);
          if fsize>=0 then
            begin
            FileName:=FileName+PageList.Strings[a];
            Break;
            end;
          end;
        end
      else
        fsize:=File_Size(FullName);

      Result:=StringReplace(FileName,FOLDER_DELIMITER,'/',[rfreplaceall]);
      end;

    procedure ParseRange(s:RtcString; var r_type, r_from, r_to:RtcString);
      var
        a:integer;
      begin
      r_type:=''; r_from:=''; r_to:='';

      a:=pos(RtcChar('='),s);
      if a>0 then
        begin
        r_type:=UpperCase(Trim(Copy(s,1,a-1)));
        Delete(s,1,a);

        a:=Pos(RtcChar('-'),s);
        if a>0 then
          begin
          r_from:=Trim(Copy(s,1,a-1));
          Delete(s,1,a);

          a:=Pos(RtcChar(','),s);
          if a>0 then
            r_to:=Trim(Copy(s,1,a-1))
          else
            r_to:=Trim(s);
          end
        else
          begin
          a:=Pos(RtcChar(','),s);
          if a>0 then
            r_from:=Trim(Copy(s,1,a-1))
          else
            r_from:=Trim(s);
          end;
        end;
      end;

    begin
    with Sender do
      begin
      // Check HOST and find document root
      DocRoot:=GetDocRoot(Request.Host);

      if DocRoot='' then
        begin
        Sender.Accept;

        XLog('BAD! '+Sender.PeerAddr+' > '+Sender.Request.Host+
             ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
             ' 0'+
             ' REF "'+Sender.Request.Referer+'"'+
             ' AGENT "'+Sender.Request.Agent+'" > Invalid Host: "'+Request.Host+'".');

        Response.Status(400,'Bad Request');
        Write('Status 400: Bad Request');

        Exit;
        end;

      // Check File Name and send Result Header
      MyFileName := RepairFileName(String(Request.FileName));

      if MyFileName<>'' then
        begin
        if fsize>0 then
          begin
          Accept; // found the file, we will be responding to this request.

          // Check if we have some info about the content type for this file ...
          Content_Type:=GetContentType(MyFileName);

          Request.Info.asText['fname']:=StringReplace(MyFileName,'/',FOLDER_DELIMITER,[rfReplaceAll]);
          Request.Info.asText['root']:=DocRoot;
          Request.Info.asLargeInt['$time']:=GetTickTime;
          Request.Info.asLargeInt['from']:=0;

          // Response['Cache-Control']:='max-age=600'; // instruct the browser to load the file from cache, if file in cache is younger than 600 seconds (10 minutes)
          Response.ContentType:=Content_Type;
          Response.ContentLength:=fsize;

          if Request.Method='HEAD' then
            Response.SendContent:=False
          else if Request.Method='GET' then
            begin
            if Request['RANGE']<>'' then
              begin
              ParseRange(Trim(Request['RANGE']), rang_type, rang_from, rang_to);
              if rang_type='BYTES' then // we will support a single bytes range
                begin
                if rang_from<>'' then
                  begin
                  rang_end:=fsize-1;

                  rang_start:=Str2Int64Def(rang_from, 0);

                  if rang_to<>'' then
                    rang_end:=Str2Int64Def(rang_to, fsize-1);

                  if rang_start<=rang_end then
                    begin
                    if rang_end>=fsize then
                      rang_end:=fsize-1;

                    rang_from:=Int2Str(rang_start);
                    rang_to:=Int2Str(rang_end);

                    Request.Info.asLargeInt['from']:=rang_start;

                    Response.ContentLength:=rang_end-rang_start+1;
                    Response.Status(206,'Partial Content');
                    Response['Content-Range']:='bytes '+rang_from+'-'+rang_to+'/'+Int2Str(fsize);
                    end
                  else
                    begin
                    Response.Status(416,'Request range not satisfiable');
                    Request.ContentLength:=0;
                    end;
                  end
                else
                  begin
                  rang_end:=-1;

                  if rang_to<>'' then
                    rang_end:=Str2Int64Def(rang_to,-1);

                  if rang_end>0 then
                    begin
                    if rang_end>fsize then
                      rang_end:=fsize;

                    rang_start:=fsize-rang_end;
                    rang_end:=fsize-1;

                    if rang_to>=rang_from then
                      begin
                      rang_from:=Int2Str(rang_start);
                      rang_to:=Int2Str(rang_end);

                      Request.Info.asLargeInt['from']:=rang_start;

                      Response.ContentLength:=rang_end-rang_start+1;
                      Response.Status(206,'Partial Content');
                      Response['CONTENT-RANGE']:='bytes '+rang_from+'-'+rang_to+'/'+Int2Str(fsize);
                      end
                    else
                      begin
                      Response.Status(416,'Request range not satisfiable');
                      Response.ContentLength:=0;
                      end;
                    end;
                  end;
                end;
              end;
            end;

          if Sender.Request['RANGE']<>'' then
            XLog('PART '+PeerAddr+' > '+Request.Host+
                 ' "'+Request.Method+' '+Request.URI+'"'+
                 ' '+Int2Str(fsize)+
                 ' RANGE "'+Request['RANGE']+'"'+
                 ' ('+Response['CONTENT-RANGE']+')'+
                 ' REF "'+Request.Referer+'"'+
                 ' AGENT "'+Request.Agent+'"'+
                 ' TYPE "'+Content_Type+'"')
          else
            XLog('SEND '+PeerAddr+' > '+Request.Host+
                 ' "'+Request.Method+' '+Request.URI+'"'+
                 ' '+Int2Str(fsize)+
                 ' REF "'+Request.Referer+'"'+
                 ' AGENT "'+Request.Agent+'"'+
                 ' TYPE "'+Content_Type+'"');

          {$IFDEF RTC_FILESTREAM}
          if Response.ContentLength>0 then
            begin
            Request.Info.asObj['file']:=TRtcFileStream.Create;
            with TRtcFileStream(Request.Info.asObj['file']) do
              begin
              Open(DocRoot+Request.Info.asText['fname']);
              Seek(Request.Info.asLargeInt['from']);
              end;
            end;
          {$ENDIF}
          
          Write;
          end
        else if fsize=0 then
          begin
          // Found the file, but it is empty.
          Accept;

          XLog('SEND '+Sender.PeerAddr+' > '+Sender.Request.Host+
               ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
               ' 0'+
               ' REF "'+Sender.Request.Referer+'"'+
               ' AGENT "'+Sender.Request.Agent+'"');

          Write;
          end
        else
          begin
          // File not found.
          Accept;

          XLog('FAIL '+Sender.PeerAddr+' > '+Sender.Request.Host+
               ' "'+Sender.Request.Method+' '+Sender.Request.URI+'"'+
               ' 0'+
               ' REF "'+Sender.Request.Referer+'"'+
               ' AGENT "'+Sender.Request.Agent+'" > File not found: "'+RtcString(MyFileName)+'".');

          Response.Status(404,'File not found');
          Write('Status 404: File not found');
          end;
        end;
      end;
    end;

  begin
  with TRtcDataServer(Sender).Request do
    if (Method='GET') or (Method='HEAD') then
      CheckDiskFile(TRtcDataServer(Sender));
  end;

procedure TFile_Provider.FileProviderSendBody(Sender: TRtcConnection);
  var
    DocRoot:String;
    s:RtcString;
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.Complete then
      begin
      if Response.DataOut<Response.DataSize then // need to send more content
        begin
        {$IFDEF RTC_FILESTREAM}
        if Response.DataSize-Response.DataOut>MAX_SEND_BLOCK_SIZE then
          s:=TRtcFileStream(Request.Info.asObj['file']).Read(MAX_SEND_BLOCK_SIZE)
        else
          s:=TRtcFileStream(Request.Info.asObj['file']).Read(Response.DataSize-Response.DataOut);
        {$ELSE}
        DocRoot:=Request.Info['root'];

        if Response.DataSize-Response.DataOut>MAX_SEND_BLOCK_SIZE then
          s:=Read_File(DocRoot+Request.Info.asText['fname'],
                       Request.Info.asLargeInt['from']+Response.DataOut,
                       MAX_SEND_BLOCK_SIZE)
        else
          s:=Read_File(DocRoot+Request.Info.asText['fname'],
                       Request.Info.asLargeInt['from']+Response.DataOut,
                       Response.DataSize-Response.DataOut);
        {$ENDIF}

        if s='' then // Error reading file.
          begin
          XLog('CRC! '+PeerAddr+' > '+Request.Host+
               ' "'+Request.Method+' '+Request.URI+'"'+
               ' > Error reading File: "'+RtcString(DocRoot+Request.Info.asText['fname'])+'".');
          Disconnect;
          end
        else
          Write(s);
        end;
      end
    else if Request.DataSize>MAX_ACCEPT_BODY_SIZE then // Do not accept requests with body longer than 128K
      begin
      XLog('BAD! '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' 0'+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> Content size exceeds 128K limit (size='+Int2Str(Request.DataSize)+' bytes).');

      Response.Status(400,'Bad Request');
      Write('Status 400: Bad Request');

      Disconnect;
      end;
    end;
  end;

procedure TFile_Provider.FileProviderDisconnect(Sender: TRtcConnection);
  var
    tim:int64;
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.DataSize>Request.DataIn then
      begin
      // did not receive a complete request
      XLog('ERR! '+PeerAddr+' > '+Request['HOST'] {.rHost} +
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' 0'+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> DISCONNECTED while receiving a Request ('+Int2Str(Request.DataIn)+' of '+Int2Str(Request.DataSize)+' bytes received).');
      end
    else if Response.DataSize>Response.DataOut then
      begin
      tim:=GetTickTime-Request.Info.asLargeInt['$time'];
      if tim<=0 then tim:=1;

      if Response['CONTENT-RANGE']='' then // no need to show this for partial downloads
        // did not send a complete result
        XLog('ERR! '+PeerAddr+' > '+Request.Host+
             ' "'+Request.Method+' '+Request.URI+'"'+
             ' -'+Int2Str(Response.DataSize-Response.DataOut)+
             ' REF "'+Request.Referer+'"'+
             ' AGENT "'+Request.Agent+'" '+
             Int2Str(Response.DataOut)+'/'+Int2Str(Response.DataSize)+' bytes in '+
             Int2Str(GetTickTime-Request.Info.asLargeInt['$time'])+' ms ='+
             Int2Str(Response.DataOut div tim)+' kbits')
      else
        // did not send a complete result
        XLog('BRK! '+PeerAddr+' > '+Request.Host+
             ' "'+Request.Method+' '+Request.URI+'"'+
             ' -'+Int2Str(Response.DataSize-Response.DataOut)+
             ' REF "'+Request.Referer+'"'+
             ' AGENT "'+Request.Agent+'" '+
             Int2Str(Response.DataOut)+' bytes in '+
             Int2Str(GetTickTime-Request.Info.asLargeInt['$time'])+' ms ='+
             Int2Str(Response.DataOut div tim)+' kbits');
      end;
    end;
  end;

{ *** TIME PROVIDER *** }

procedure TFile_Provider.TimeProviderCheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if UpperCase(Request.FileName)='/$TIME' then
      Accept;
  end;

procedure TFile_Provider.TimeProviderDataReceived(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    if Request.Complete then
      Write('<html><body>Your IP: '+PeerAddr+'<br>'+
            'Server Time: '+RtcString(TimeToStr(Now))+'<br>'+
            'Connection count: '+Int2Str(TotalConnectionCount)+'<br>'+
            'Request Headers ... <br>'+StringReplace(Request.HeaderText,#13,'<br>',[rfReplaceAll])+'</body></html>');
  end;

initialization
finalization
if assigned(File_Provider) then
  begin
  File_Provider.Free;
  File_Provider:=nil;
  end;
end.
