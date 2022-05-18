{
  @html(<b>)
  Log File Creation
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit gives you thread-safe Log writing support.
}
unit rtcLog;

{$INCLUDE rtcDefs.inc}

interface

uses
{$IFDEF WINDOWS}
  Windows, // FileCreate + FileClose + GetCurrentThreadID
{$ENDIF}
{$IFDEF POSIX}
  Posix.Unistd, // FileClose
  Posix.PThread, // GetCurrentThreadID
{$ENDIF}

  SysUtils,

{$IFDEF IDE_1}
  FileCtrl,
{$ENDIF}

  rtcTypes,
  rtcSystem,
  rtcSrcList;

var
  { Write Logged exception into the Log file?
    Dafault=True. By changing this to False will remove any
    Connection component exceptions from the Log file. }
  LOG_EXCEPTIONS:boolean=True;

  { Write all "Log" calls to the CONSOLE (stdout)? }
  LOG_TO_CONSOLE:boolean=False;

  { Write all "xLog" calls to the CONSOLE (stdout)? }
  XLOG_TO_CONSOLE:boolean=False;

  { The RTC SDK can silently handle most exceptions which
    would otherwise cause the components to stop working.
    This is a safety-net which ensures that even bugs in
    the RTC SDK do not cause your apps to crash, but an
    exception getting that far down to the RTC SDK usually
    means something is wrong in the RTC SDK.
    When debugging the RTC SDK, LOG_AV_ERRORS should be
    TRUE in order for all abnormal exceptions to be logged. }
  LOG_AV_ERRORS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

  { If you want old log files to be deleted after several days,
    you can specify how long (in days) files should be kept.
    If this variable is 0 (default), log files will NOT be deleted. }
  RTC_LOGS_LIVE_DAYS:integer=0;

  { Sub-Folder inside AppFileName's directory where all LOG files will be stored.
    If you want LOG files to be created in the same folder as AppFile (EXE/DLL),
    set LOG_FOLDER to an empty String before calling "StartLog".
    For this value to have any effect, you need to set it before calling "StartLog". }
  LOG_FOLDER:RtcWideString='LOG';

  { Full path to the LOG folder. If you leave this variable empty (default),
    it will be initialized automatically by using the AppFileName and LOG_FOLDER
    variables, immediately before the first log entry needs to be written.
    If you want your LOG files written to a specific folder by using full path,
    you can do it by setting this variable before the first Log entry is written.
    RTC_LOG_FOLDER should ALWAYS end with '\' on Windows and '/' on other platforms. }
  RTC_LOG_FOLDER:RtcWideString='';

  { String used to format Date/Time output in RTC LOG. For more information on valid Data/Time
    format strings, please refer to Delphi help about the "FormatDataTime" function. }
  RTC_LOG_DATETIMEFORMAT:String='yyyy-mm-dd hh:nn:ss.zzz; ';

  { Include CurrentThreadID in every LOG entry? }
  RTC_LOG_THREADID:boolean=False;

{$IFDEF RTC_BYTESTRING}
{ Write exception with a short description into the Global App Log file.
  This procedure will have no effect if Log writer not started
  (by calling StartLog) or LOG_EXCEPTIONS is @false }
procedure Log(const s:RtcString; E:Exception; const name:String=''); overload;

{ Write message into the Global App Log file.
  This procedure will have no effect if Log writer not started. }
procedure Log(const s:RtcString; const name:String=''); overload;

{ Write message into the Log file for the current date.
  This procedure will have no effect if Log writer not started. }
procedure XLog(const s:RtcString; const name:String=''); overload;
{$ENDIF}

{ Copy LOG file "fromName" to LOG file "toName". }
procedure Copy_Log(const fromName,toName:String);

{ Delete LOG file "name" }
procedure Delete_Log(const name:String);

{ Write exception with a short description into the Global App Log file.
  This procedure will have no effect if Log writer not started
  (by calling StartLog) or LOG_EXCEPTIONS is @false }
procedure Log(const s:RtcWideString; E:Exception; const name:String=''); overload;

{ Write message into the Global App Log file.
  This procedure will have no effect if Log writer not started. }
procedure Log(const s:RtcWideString; const name:String=''); overload;

{ Write message into the Log file for the current date.
  This procedure will have no effect if Log writer not started. }
procedure XLog(const s:RtcWideString; const name:String=''); overload;

{ Before Log() procedures will have any effect,
  you have to call this procedure to start the Log writer.
  Without it, no Log file. }
procedure StartLog;

{ To stop Log file creation, simply call this procedure.
  To continue log writing, call StartLog. }
procedure StopLog;

{ Start using Buffers for Logging, which makes logging a lot faster.
  "MaxSize" is the maximum size (in bytes) the LOG may occupy
  in memory before it has to be dumped to files. @html(<br><br>)

  IMPORTANT!!! When using Buffers for logging, the "name" parameter is case-sensitive,
  which means that a separte Buffer will be created for 'XName' than for 'xname', but
  both buffers will at the end be dumbed into the same file, so you have to be careful
  when using the "name" parameter to always use the exact same value for all LOG entries
  which need to go to the same file, or the order of log entries could get mixed up. }
procedure StartLogBuffers(MaxSize:longint);

{ Stop using Buffers for Logging. }
procedure StopLogBuffers;

{ Dump current Log Buffers to files and release log buffer memory. }
procedure DumpLogBuffers;

implementation

var
  ThrCS:TRtcCritSec=nil;
  doLog:boolean=False;
  doBuffers:boolean=False;
  LogMaxBuff:longint;
  LogCurBuff:longint;
  LogBuff:TStringObjList;
  AppOnlyFileName,
  AppOnlyFilePath:RtcWideString;

procedure StartLog;
  begin
  if not doLog then
    if assigned(ThrCS) then
      begin
      doLog:=True;
      {$IFDEF RTC_DEBUG}Log('rtcLog START ...','DEBUG');{$ENDIF}
      end;
  end;

procedure StopLog;
  begin
  if doLog then
    begin
    {$IFDEF RTC_DEBUG} Log('rtcLog STOP.','DEBUG');{$ENDIF}
    doLog:=False;
    end;
  end;

procedure Delete_old_logs;
  var
    vdate      :TDatetime;
    sr         :TSearchRec;
    intFileAge :LongInt;
    myfileage  :TDatetime;
  begin
  try
    vdate:= Now - RTC_LOGS_LIVE_DAYS;
    if FindFirst(RTC_LOG_FOLDER + '*.log', faAnyFile - faDirectory, sr) = 0 then
      repeat
        intFileAge := FileAge(RTC_LOG_FOLDER + RtcWideString(sr.name));
        if intFileAge > -1 then
          begin
          myfileage:= FileDateToDateTime(intFileAge);
          if myfileage < vdate then
            Delete_File(RTC_LOG_FOLDER + RtcWideString(sr.name));
          end;
        until (FindNext(sr) <> 0);
  finally
    FindClose(sr);
    end;
  end;

procedure File_AppendEx(const fname:RtcWideString; const Data:RtcByteArray);
  var
    f:TRtcFileHdl;
  begin
  f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyNone);
  if f=RTC_INVALID_FILE_HDL then
    begin
    try
      if RTC_LOGS_LIVE_DAYS > 0 then
        Delete_old_logs;
    except
      // ignore problems with file deletion
      end;
    f:=FileCreate(fname);
    end;
  if f<>RTC_INVALID_FILE_HDL then
    try
      if FileSeek(f,0,2)>=0 then
        FileWrite(f,data[0],length(data));
    finally
      FileClose(f);
      end;
  end;

procedure File_Append(const fname:RtcWideString; const Data:RtcString);
  var
    f:TRtcFileHdl;
  begin
  f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyNone);
  if f=RTC_INVALID_FILE_HDL then
    begin
    try
      if RTC_LOGS_LIVE_DAYS > 0 then
        Delete_old_logs;
    except
      // ignore problems with file deletion
      end;
    f:=FileCreate(fname);
    end;
  if f<>RTC_INVALID_FILE_HDL then
    try
      if FileSeek(f,0,2)>=0 then
        {$IFDEF RTC_BYTESTRING}
        FileWrite(f,data[1],length(data));
        {$ELSE}
        FileWrite(f,RtcStringToBytes(data)[0],length(data));
        {$ENDIF}
    finally
      FileClose(f);
      end;
  end;

procedure PrepareLogFolder;
  begin
  if AppFileName='' then
    AppFileName:=ExpandUNCFileName(RtcWideString(ParamStr(0)));

  if AppOnlyFileName='' then
    begin
    AppOnlyFileName:=ExtractFileName(AppFileName);
    AppOnlyFilePath:=ExtractFilePath(AppFileName);
    if Copy(AppOnlyFilePath,length(AppOnlyFilePath),1)<>FOLDER_DELIMITER then
      AppOnlyFilePath:=AppOnlyFilePath+FOLDER_DELIMITER;
    end;

  if RTC_LOG_FOLDER='' then
    begin
    RTC_LOG_FOLDER:=AppOnlyFilePath;
    if LOG_FOLDER<>'' then
      begin
      RTC_LOG_FOLDER:=RTC_LOG_FOLDER+LOG_FOLDER;
      if Copy(RTC_LOG_FOLDER,length(RTC_LOG_FOLDER),1)<>FOLDER_DELIMITER then
        RTC_LOG_FOLDER:=RTC_LOG_FOLDER+FOLDER_DELIMITER;
      end;
    end;

  if not DirectoryExists(RTC_LOG_FOLDER) then
    if not CreateDir(RTC_LOG_FOLDER) then
      begin
      RTC_LOG_FOLDER:=GetTempDirectory;
      if Copy(RTC_LOG_FOLDER,length(RTC_LOG_FOLDER),1)<>FOLDER_DELIMITER then
        RTC_LOG_FOLDER:=RTC_LOG_FOLDER+FOLDER_DELIMITER;

      RTC_LOG_FOLDER:=RTC_LOG_FOLDER+LOG_FOLDER;
      if not DirectoryExists(RTC_LOG_FOLDER) then
        CreateDir(RTC_LOG_FOLDER);

      if Copy(RTC_LOG_FOLDER,length(RTC_LOG_FOLDER),1)<>FOLDER_DELIMITER then
        RTC_LOG_FOLDER:=RTC_LOG_FOLDER+FOLDER_DELIMITER;
      end;
  end;

procedure WriteToLogEx(const ext:RtcWideString; const text:RtcByteArray);
  begin
  PrepareLogFolder;
  File_AppendEx(RTC_LOG_FOLDER+AppOnlyFileName+'.'+ext, text);
  end;

procedure WriteToLog(const ext:RtcWideString; const text:RtcString);
  begin
  PrepareLogFolder;
  File_Append(RTC_LOG_FOLDER+AppOnlyFileName+'.'+ext, text);
  end;

procedure WriteToBuffEx(const ext:RtcWideString; const text:RtcByteArray);
  var
    obj:TObject;
    data:TRtcHugeByteArray;
  begin
  obj:=LogBuff.search(ext);
  if not assigned(obj) then
    begin
    data:=TRtcHugeByteArray.Create;
    LogBuff.insert(ext,data);
    end
  else
    data:=TRtcHugeByteArray(obj);
  data.AddEx(text);
  Inc(LogCurBuff,length(text));
  if LogCurBuff>LogMaxBuff then
    DumpLogBuffers;
  end;

procedure WriteToBuff(const ext:RtcWideString; const text:RtcString);
  var
    obj:TObject;
    data:TRtcHugeByteArray;
  begin
  obj:=LogBuff.search(ext);
  if not assigned(obj) then
    begin
    data:=TRtcHugeByteArray.Create;
    LogBuff.insert(ext,data);
    end
  else
    data:=TRtcHugeByteArray(obj);
  data.Add(text);
  Inc(LogCurBuff,length(text));
  if LogCurBuff>LogMaxBuff then
    DumpLogBuffers;
  end;

procedure StartLogBuffers(MaxSize:longint);
  begin
  ThrCS.Acquire;
  try
    doBuffers:=True;
    if assigned(LogBuff) then
      DumpLogBuffers
    else
      LogBuff:=tStringObjList.Create(128);
    LogMaxBuff:=MaxSize;
    LogCurBuff:=0;
  finally
    ThrCS.Release;
    end;
  end;

procedure DumpLogBuffers;
  var
    s:RtcWideString;
    obj:TObject;
    data:TRtcHugeByteArray;
  begin
  ThrCS.Acquire;
  try
    if assigned(LogBuff) then
      begin
      while not LogBuff.Empty do
        begin
        s:=LogBuff.search_min(obj);
        LogBuff.remove(s);
        if assigned(obj) then
          begin
          data:=TRtcHugeByteArray(obj);
          try
            WriteToLogEx(s,data.GetEx);
          except
            end;
          data.Free;
          end;
        end;
      LogCurBuff:=0;
      end;
  finally
    ThrCS.Release;
    end;
  end;

procedure StopLogBuffers;
  begin
  ThrCS.Acquire;
  try
    doBuffers:=False;
    DumpLogBuffers;
    RtcFreeAndNil(LogBuff);
  finally
    ThrCS.Release;
    end;
  end;

procedure XLog(const s:RtcWideString; const name:String='');
  var
    d:TDateTime;
    fname:RtcWideString;
    s2:RtcString;
  begin
  if not doLog then Exit; // Exit here !!!!

  d:=Now;
  if RTC_LOG_DATETIMEFORMAT<>'' then
    begin
    if RTC_LOG_THREADID then
      s2:= Utf8Encode(RtcWideString(IntToStr(Cardinal(GetCurrentThreadId))+'#'+FormatDateTime(RTC_LOG_DATETIMEFORMAT,d)))
    else
      s2:= Utf8Encode(RtcWideString(FormatDateTime(RTC_LOG_DATETIMEFORMAT,d)));
    end
  else if RTC_LOG_THREADID then
    s2:= Int2Str(Cardinal(GetCurrentThreadId))+'#'
  else
    s2:= '';

  if name<>'' then
    fname:=RtcWideString(FormatDateTime('yyyy_mm_dd',d)+'.'+name)+'.log'
  else
    fname:=RtcWideString(FormatDateTime('yyyy_mm_dd',d))+'.log';

  ThrCS.Acquire;
  try
    if XLOG_TO_CONSOLE then
      Writeln(name,':',s);
    if doBuffers then
      WriteToBuff(fname, s2+Utf8Encode(s)+#13#10 )
    else
      WriteToLog(fname, s2+Utf8Encode(s)+#13#10 );
  except
    end;
  ThrCS.Release;
  end;

procedure Log(const s:RtcWideString; const name:String='');
  var
    d:TDateTime;
    fname:RtcWideString;
    s2:RtcString;
  begin
  if not doLog then Exit; // Exit here !!!!

  d:=Now;
  if RTC_LOG_DATETIMEFORMAT<>'' then
    begin
    if RTC_LOG_THREADID then
      s2:=Utf8Encode(RtcWideString(IntToStr(Cardinal(GetCurrentThreadId))+'#'+FormatDateTime(RTC_LOG_DATETIMEFORMAT,d)))
    else
      s2:=Utf8Encode(RtcWideString(FormatDateTime(RTC_LOG_DATETIMEFORMAT,d)));
    end
  else if RTC_LOG_THREADID then
    s2:=Int2Str(Cardinal(GetCurrentThreadId))+'#'
  else
    s2:='';

  if name<>'' then
    fname:=RtcWideString(name)+'.log'
  else
    fname:='log';

  ThrCS.Acquire;
  try
    if LOG_TO_CONSOLE then
      Writeln(name,':',s);
    if doBuffers then
      WriteToBuff(fname, s2+Utf8Encode(s)+#13#10 )
    else
      WriteToLog(fname, s2+Utf8Encode(s)+#13#10 );
  except
    end;
  ThrCS.Release;
  end;

procedure Log(const s:RtcWideString; E:Exception; const name:String='');
  begin
  if LOG_EXCEPTIONS then
    Log(s+' Exception! '+RtcWideString(E.ClassName)+': '+RtcWideString(E.Message), name);
  end;

{$IFDEF RTC_BYTESTRING}

procedure XLog(const s:RtcString; const name:String='');
  var
    d:TDateTime;
    fname:RtcWideString;
    s2:RtcString;
  begin
  if not doLog then Exit; // Exit here !!!!

  d:=Now;
  if RTC_LOG_DATETIMEFORMAT<>'' then
    begin
    if RTC_LOG_THREADID then
      s2:= Utf8Encode(RtcWideString(IntToStr(Cardinal(GetCurrentThreadId))+'#'+FormatDateTime(RTC_LOG_DATETIMEFORMAT,d)))
    else
      s2:= Utf8Encode(RtcWideString(FormatDateTime(RTC_LOG_DATETIMEFORMAT,d)));
    end
  else if RTC_LOG_THREADID then
    s2:= Int2Str(Cardinal(GetCurrentThreadId))+'#'
  else
    s2:= '';

  if name<>'' then
    fname:=RtcWideString(FormatDateTime('yyyy_mm_dd',d)+'.'+name)+'.log'
  else
    fname:=RtcWideString(FormatDateTime('yyyy_mm_dd',d))+'.log';

  ThrCS.Acquire;
  try
    if XLOG_TO_CONSOLE then
      Writeln(name,':',s);
    if doBuffers then
      WriteToBuff(fname, s2+s+#13#10 )
    else
      WriteToLog(fname, s2+s+#13#10 );
  except
    end;
  ThrCS.Release;
  end;

procedure Log(const s:RtcString; const name:String='');
  var
    d:TDateTime;
    fname:RtcWideString;
    s2:RtcString;
  begin
  if not doLog then Exit; // Exit here !!!!

  d:=Now;
  if RTC_LOG_DATETIMEFORMAT<>'' then
    begin
    if RTC_LOG_THREADID then
      s2:=Utf8Encode(RtcWideString(IntToStr(Cardinal(GetCurrentThreadId))+'#'+FormatDateTime(RTC_LOG_DATETIMEFORMAT,d)))
    else
      s2:=Utf8Encode(RtcWideString(FormatDateTime(RTC_LOG_DATETIMEFORMAT,d)));
    end
  else if RTC_LOG_THREADID then
    s2:=Int2Str(Cardinal(GetCurrentThreadId))+'#'
  else
    s2:='';

  if name<>'' then
    fname:=RtcWideString(name)+'.log'
  else
    fname:='log';

  ThrCS.Acquire;
  try
    if LOG_TO_CONSOLE then
      Writeln(name,':',s);
    if doBuffers then
      WriteToBuff(fname, s2+s+#13#10 )
    else
      WriteToLog(fname, s2+s+#13#10 );
  except
    end;
  ThrCS.Release;
  end;

procedure Log(const s:RtcString; E:Exception; const name:String='');
  begin
  if LOG_EXCEPTIONS then
    Log(s+' Exception! '+RtcString(E.ClassName)+': '+RtcString(E.Message), name);
  end;
{$ENDIF}

{ Copy LOG file "fromName" to LOG file "toName". }
procedure Copy_Log(const fromName,toName:String);
  var
    xname,fname,tname:RtcWideString;
    cnt:integer;
  begin
  if not doLog then Exit; // Exit here !!!!
  if fromName=toName then Exit; // and here !!!!

  PrepareLogFolder;
  if fromName='' then fname:='log' else fname:=RtcWideString(fromName)+'.log';
  if toName=''   then tname:='log' else tname:=RtcWideString(toName)+'.log';

  DumpLogBuffers;

  cnt:=1;
  xname:=RTC_LOG_FOLDER+AppOnlyFileName+'.';
  while not Copy_FileEx(xname+fname,xname+tname,0,0,-1,128000) do
    begin
    Sleep(100);
    Inc(cnt);
    if cnt>10 then Break;
    end;
  end;

{ Delete LOG file "name" }
procedure Delete_Log(const name:String);
  var
    xname,fname:RtcWideString;
    cnt:integer;
  begin
  if not doLog then Exit; // Exit here !!!!

  PrepareLogFolder;
  if name='' then fname:='log' else fname:=RtcWideString(name)+'.log';

  DumpLogBuffers;

  cnt:=1;
  xname:=RTC_LOG_FOLDER+AppOnlyFileName+'.';
  while not Delete_File(xname+fname) do
    begin
    Sleep(100);
    Inc(cnt);
    if cnt>10 then Break;
    end;
  end;

initialization
AppOnlyFileName:='';
AppOnlyFilePath:='';
ThrCS:=TRtcCritSec.Create;
finalization
{$IFDEF RTC_DEBUG} Log('rtcLog Finalizing ...','DEBUG');{$ENDIF}
StopLogBuffers;
StopLog;
AppOnlyFileName:='';
AppOnlyFilePath:='';
RtcFreeAndNil(ThrCS);
end.
