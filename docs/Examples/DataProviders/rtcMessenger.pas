unit rtcMessenger;

interface

uses
  SysUtils,

{$IFDEF VER120}
  FileCtrl,
{$ENDIF}
{$IFDEF VER130}
  FileCtrl,
{$ENDIF}

  rtcTypes, rtcSystem,
  rtcInfo, rtcCrypt,
  rtcSrvModule;

const
  MSG_LIST_CRYPT_KEY:RtcString='$RtcMessenger_UserList';
  MSG_DATA_CRYPT_KEY:RtcString='$RtcMessenger_UserData';
  MSG_INFO_CRYPT_KEY:RtcString='$RtcMessenger_UserInfo';

  // Sub-Folder relative to AppFileName, inside which Messenger Data files will be stored
  MSG_DATA_FOLDER:String='RtcMessengerData';

type
  TRtcMessengerUsers=class
  private
    UserListFileName:string;
    UserDataFileName:string;
    UserList:tRtcRecord;

    UserInfo:tRtcInfo;

    userCS:TRtcCritSec;
    msgCS:TRtcCritSec;

    procedure doLogIn(uname:string; sessid:RtcString);
    procedure doLogIn2(uname:string; sessid:RtcString); // 2nd connnection
    procedure doLogOut(uname:string; sessid:RtcString);

    function isValidUserName(const uname:string):boolean;

    procedure SendData(const to_name:string; data:TRtcValueObject);

    function GetCallback(uname:string):TRtcDelayedCall;

    procedure TriggerCallback(uname:string);

  public
    constructor Create;
    destructor Destroy; override;

    procedure SetCallback(uname:string; cb:TRtcDelayedCall);

    procedure LoadUserList;
    procedure SaveUserList;

    function UserDataFile(const uname:String):String;

    function isLoggedIn(uname:string; sessid:RtcString):boolean; overload;
    function isLoggedIn(uname:string):boolean; overload;

    procedure Login(uname,upass:string; sessid:RtcString);
    procedure Login2(uname,upass:string; sessid:RtcString); // 2nd connection
    procedure Logout(uname:string; sessid:RtcString);

    procedure RegUser(uname,upass:string; sessid:RtcString);
    procedure ChangePass(uname,oldpass,newpass:string);
    procedure DelUser(uname:string);

    procedure AddFriend(uname,friend_name:string);
    procedure DelFriend(uname,friend_name:string);
    procedure AddIgnore(uname,ignore_name:string);
    procedure DelIgnore(uname,ignore_name:string);

    function LoadInfo(uname:string):TRtcRecord;
    procedure SaveInfo(uname:string; info:TRtcRecord);

    function Exists(uname:string):boolean;

    procedure SendText(const from_name,to_name,text:string);

    function GetData(const uname:string; lastcheck:TDateTime; var thischeck:TDateTime): TRtcArray;
    end;

implementation

{ TRtcMessengerUsers }

constructor TRtcMessengerUsers.Create;
  begin
  inherited;

  userCS:=TRtcCritSec.Create;
  msgCS:=TRtcCritSec.Create;

  UserDataFileName:=ExtractFilePath(AppFileName);
  if Copy(UserDataFileName,length(UserDataFileName),1)<>FOLDER_DELIMITER then
    UserDataFileName:=UserDataFileName+FOLDER_DELIMITER;

  if MSG_DATA_FOLDER<>'' then
    begin
    UserDataFileName:=UserDataFileName+MSG_DATA_FOLDER;
    if not DirectoryExists(UserDataFileName) then
      if not CreateDir(UserDataFileName) then
        begin
        UserDataFileName:=GetTempDirectory;
        if Copy(UserDataFileName,length(UserDataFileName),1)<>FOLDER_DELIMITER then
          UserDataFileName:=UserDataFileName+FOLDER_DELIMITER;

        UserDataFileName:=UserDataFileName+MSG_DATA_FOLDER;
        if not DirectoryExists(UserDataFileName) then
          CreateDir(UserDataFileName);
        end;
    end;

  if Copy(UserDataFileName,length(UserDataFileName),1)<>FOLDER_DELIMITER then
    UserDataFileName:=UserDataFileName+FOLDER_DELIMITER;

  UserListFileName:=UserDataFileName+'Users.list';

  UserInfo:=TRtcInfo.Create;
  LoadUserList;
  end;

destructor TRtcMessengerUsers.Destroy;
  begin
  UserInfo.Free;
  UserList.Free;
  userCS.Free;
  msgCS.Free;
  inherited;
  end;

procedure TRtcMessengerUsers.LoadUserList;
  var
    s:RtcString;
  begin
  userCS.Acquire;
  try
    if File_Exists(UserListFileName) then
      begin
      s:=Read_File(UserListFileName);
      DeCrypt(s,MSG_LIST_CRYPT_KEY);
      end
    else
      s:='';
    if assigned(UserList) then
      UserList.Free;
    if s<>'' then
      UserList:=TRtcRecord.FromCode(s)
    else
      UserList:=TRtcRecord.Create;
  finally
    userCS.Release;
    end;
  end;

procedure TRtcMessengerUsers.SaveUserList;
  var
    s:RtcString;
  begin
  userCS.Acquire;
  try
    s:=UserList.toCode;
    Crypt(s,MSG_LIST_CRYPT_KEY);
    if not Write_File(UserListFileName,s) then
      raise Exception.Create('Error writing UserList to "'+UserListFileName+'".');
  finally
    userCS.Release;
    end;
  end;

function TRtcMessengerUsers.UserDataFile(const uname:String):String;
  begin
  Result:=UserDataFileName+'User.'+LowerCase(uname);
  end;

function TRtcMessengerUsers.LoadInfo(uname:string):TRtcRecord;
  var
    s:RtcString;
  begin
  userCS.Acquire;
  try
    if File_Exists(UserDataFile(uname)+'.info') then
      begin
      s:=Read_File(UserDataFile(uname)+'.info');
      DeCrypt(s, MSG_INFO_CRYPT_KEY);
      Result:=TRtcRecord.FromCode(s);
      end
    else
      Result:=TRtcRecord.Create;
    Result.AutoCreate:=True;
  finally
    userCS.Release;
    end;
  end;

procedure TRtcMessengerUsers.SaveInfo(uname:string; info:TRtcRecord);
  var
    s:RtcString;
  begin
  userCS.Acquire;
  try
    s:=info.toCode;
    Crypt(s, MSG_INFO_CRYPT_KEY);
    Write_File(UserDataFile(uname)+'.info',s);
  finally
    userCS.Release;
    end;
  end;

procedure TRtcMessengerUsers.doLogIn2(uname:string; sessid:RtcString);
  begin
  userCS.Acquire;
  try
    // Remember new session ID
    if UserInfo.Child[uname]=nil then
      raise Exception.Create('Not logged in.')
    else with UserInfo.Child[uname] do
      asString['Session2']:=RtcString(sessid);
  finally
    userCS.Release;
    end;
  end;

procedure TRtcMessengerUsers.doLogIn(uname:string;sessid:RtcString);
  var
    info,info2,rec,rec2:TRtcRecord;
    cb:TRtcDelayedCall;
    fname:string;
    i:integer;
    log_in:boolean;
  begin
  userCS.Acquire;
  try
    // Get callback info, if exists
    cb:=GetCallback(uname);
    // Remove existing info.
    UserInfo.SetNil(uname);
    // Remember new session ID
    with UserInfo.NewChild(uname) do
      asString['Session']:=sessid;
  finally
    userCS.Release;
    end;

  // Trigger calback, if it was set (second connectin was open and is now waiting).
  // This will signal the waiting connection to execute,
  // which will end in an exception "not logged in" and close the connection.
  if assigned(cb) then
    cb.WakeUp;

  info:=LoadInfo(uname);
  try
    if info.isType['friends']=rtc_Record then
      with info.asRecord['friends'] do
        begin
        rec:=TRtcRecord.Create;
        rec2:=TRtcRecord.Create;
        try
          for i:=0 to Count-1 do // Send "login" message to all friends.
            begin
            fname:=FieldName[i];
            info2:=LoadInfo(fname);
            try
              if info2.isType['friends']=rtc_Record then
                if not info2.asRecord['friends'].isNull[uname] then // we are in our friend's list
                  begin
                  userCS.Acquire;
                  try
                    log_in:=UserInfo.Child[fname]<>nil;
                  finally
                    userCS.Release;
                    end;
                  if log_in then // friend logged in
                    begin
                    // Send ourself info that friend is logged in.
                    rec.asText['login']:=fname;
                    SendData(uname, rec);

                    // Send friend info that we're logging in.
                    rec.asText['login']:=uname;
                    SendData(fname, rec);
                    end
                  else
                    begin
                    // Send ourself info that friend is NOT logged in.
                    rec2.asText['logout']:=fname;
                    SendData(uname, rec2);
                    end;
                  end;
            finally
              info2.Free;
              end;
            end;
        finally
          rec.Free;
          rec2.Free;
          end;
        end;
  finally
    info.Free;
    end;
  end;

procedure TRtcMessengerUsers.doLogOut(uname:String;sessid:RtcString);
  var
    info,rec:TRtcRecord;
    log_out,log_in:boolean;
    cb:TRtcDelayedCall;
    i:integer;
    fname:string;
  begin
  cb:=nil;
  log_out:=False;
  userCS.Acquire;
  try
    // If logged in under this session ID, remove info
    if UserInfo.Child[uname]<>nil then
      if UserInfo.Child[uname]['session']=sessid then
        begin
        cb:=GetCallback(uname);
        UserInfo.SetNil(uname);
        log_out:=True;
        end;
  finally
    userCS.Release;
    end;

  if log_out then
    begin
    // Triger Callback after Logout, to signal the second connection to check user status.
    // This will end in "user not logged in" exception and close the connection.
    if assigned(cb) then
      cb.WakeUp;

    info:=LoadInfo(uname);
    try
      if info.isType['friends']=rtc_Record then
        with info.asRecord['friends'] do
          begin
          rec:=TRtcRecord.Create;
          try
            for i:=0 to Count-1 do // for all Friends in out list ...
              begin
              fname:=FieldName[i];
              userCS.Acquire;
              try
                log_in:=UserInfo.Child[fname]<>nil;
              finally
                userCS.Release;
                end;
              if log_in then // friend logged in
                begin
                // Send friend info that we're logging out.
                rec.asText['logout']:=uname;
                SendData(fname, rec);
                end;
              end;
          finally
            rec.Free;
            end;
          end;
    finally
      info.Free;
      end;
    end;
  end;

function TRtcMessengerUsers.isLoggedIn(uname:string;sessid:RtcString):boolean;
  begin
  userCS.Acquire;
  try
    Result:=False;
    // If logged in under this session ID, return True
    if UserInfo.Child[uname]<>nil then
      if (UserInfo.Child[uname]['session']=sessid) or
         (UserInfo.Child[uname]['session2']=sessid) then
        Result:=True;
  finally
    userCS.Release;
    end;
  end;

function TRtcMessengerUsers.isLoggedIn(uname:string):boolean;
  begin
  userCS.Acquire;
  try
    Result:=UserInfo.Child[uname]<>nil;
  finally
    userCS.Release;
    end;
  end;

procedure TRtcMessengerUsers.SetCallback(uname: string; cb: TRtcDelayedCall);
  begin
  userCS.Acquire;
  try
    if UserInfo.Child[uname]<>nil then
      UserInfo.Child[uname].asPtr['callback']:=cb;
  finally
    userCS.Release;
    end;
  end;

function TRtcMessengerUsers.GetCallback(uname: string): TRtcDelayedCall;
  begin
  Result:=nil;
  userCS.Acquire;
  try
    if UserInfo.Child[uname]<>nil then
      begin
      Result:=TRtcDelayedCall(UserInfo.Child[uname].asPtr['callback']);
      if Assigned(Result) then
        // We can not call the calback function more than once,
        // so we will clear its assignment here.
        UserInfo.Child[uname].asPtr['callback']:=nil;
      end;
  finally
    userCS.Release;
    end;
  end;

procedure TRtcMessengerUsers.TriggerCallback(uname: string);
  var
    cb:TRtcDelayedCall;
  begin
  cb:=GetCallback(uname);
  if assigned(cb) then
    cb.WakeUp;
  end;

procedure TRtcMessengerUsers.Login(uname,upass:string;sessid:RtcString);
  begin
  userCS.Acquire;
  try
    if uname='' then
      raise Exception.Create('Username required for Login.')
    else if upass='' then
      raise Exception.Create('Password required for Login.')
    else
      begin
      if UserList.isType[uname]<>rtc_Record then // user doesn't exist
        raise Exception.Create('User "'+uname+'" not registered.')
      else
        begin
        with UserList.asRecord[uname] do
          if asText['pass']<>upass then
            raise Exception.Create('Wrong password for user "'+uname+'".')
          else
            doLogIn(uname,sessid);
        end;
      end;
  finally
    userCS.Release;
    end;
  end;

procedure TRtcMessengerUsers.Login2(uname,upass:string;sessid:RtcString);
  begin
  userCS.Acquire;
  try
    if uname='' then
      raise Exception.Create('Username required for Login.')
    else if upass='' then
      raise Exception.Create('Password required for Login.')
    else
      begin
      if UserList.isType[uname]<>rtc_Record then // user doesn't exist
        raise Exception.Create('User "'+uname+'" not registered.')
      else
        begin
        with UserList.asRecord[uname] do
          if asText['pass']<>upass then
            raise Exception.Create('Wrong password for user "'+uname+'".')
          else
            doLogIn2(uname,sessid);
        end;
      end;
  finally
    userCS.Release;
    end;
  end;

function TRtcMessengerUsers.Exists(uname:string):boolean;
  begin
  userCS.Acquire;
  try
    Result:=False;
    if uname<>'' then
      if UserList.isType[uname]=rtc_Record then // user exists
        Result:=True;
  finally
    userCS.Release;
    end;
  end;

procedure TRtcMessengerUsers.RegUser(uname,upass:string;sessid:RtcString);
  begin
  userCS.Acquire;
  try
    if uname='' then
      raise Exception.Create('Username required to Register.')
    else if upass='' then
      raise Exception.Create('Password required to Register.')
    else if not isValidUserName(uname) then
      raise Exception.Create('"'+uname+'" is not a valid username.')
    else
      begin
      if UserList.isType[uname]<>rtc_Null then
        raise Exception.Create('Username "'+uname+'" already taken.'#13#10+
                               'Can not register a new user with the same name.')
      else // user doesn't exists
        begin
        with UserList.NewRecord(uname) do
          begin
          asText['pass']:=upass;
          SaveUserList;
          doLogIn(uname,sessid);
          end;
        end;
      end;
  finally
    userCS.Release;
    end;
  end;

procedure TRtcMessengerUsers.DelUser(uname:string);
  begin
  userCS.Acquire;
  try
    UserList.isNull[uname]:=True; // das entfernt den Record aus UserList
    SaveUserList; // Das speichert die neue UserList
  finally
    userCS.Release;
    end;
  end;

procedure TRtcMessengerUsers.ChangePass(uname,oldpass,newpass:string);
  begin
  userCS.Acquire;
  try
    if uname='' then
      raise Exception.Create('Username required to change Password.')
    else if oldpass='' then
      raise Exception.Create('Old Password required to change Password.')
    else if newpass='' then
      raise Exception.Create('New Password required to change Password.')
    else
      begin
      if UserList.isType[uname]<>rtc_Record then
        raise Exception.Create('User "'+uname+'" not registered.')
      else // user exists
        begin
        with UserList.asRecord[uname] do
          if asText['pass']<>oldpass then
            raise Exception.Create('Wrong password for user "'+uname+'".')
          else
            begin
            asText['pass']:=newpass;
            SaveUserList;
            end;
        end;
      end;
  finally
    userCS.Release;
    end;
  end;

procedure TRtcMessengerUsers.Logout(uname:string; sessid: RtcString);
  begin
  doLogOut(uname, sessid);
  end;

procedure TRtcMessengerUsers.SendData(const to_name:string; data:TRtcValueObject);
  var
    mydata:TRtcValue;
    s:RtcString;
    fname:string;
  begin
  s:=data.toCode;
  Crypt(s, MSG_DATA_CRYPT_KEY);

  mydata:=TRtcValue.Create;
  try
    mydata.asString:=s;
    s:=mydata.toCode;
  finally
    mydata.Free;
    end;
  fname:=UserDataFile(to_name)+'.msg.data';

  msgCS.Acquire;
  try
    Write_File(fname,s,File_Size(fname));
  finally
    msgCS.Release;
    end;

  TriggerCallback(to_name);
  end;

function TRtcMessengerUsers.GetData(const uname:string; lastcheck:TDateTime; var thischeck:TDateTime): TRtcArray;
  var
    fname:String;
    s,code:RtcString;
    old:boolean;
    at,i:integer;
    rec:TRtcValue;
  begin
  thischeck:=0;
  Result:=nil;

  old:=True;

  { On first call, before we start reading newly received data, we will rename the
    original ".data" file to ".old", so that we can read the file without having to
    worry about concurrent file access, in case someone sends us a new message while
    we're getting the "old" data. This function will return he content of the old file
    together with the "old" file age, so we can delete the old file on next call,
    only if file time is less or equal to "lastcheck" time. If last send operation failed,
    we will be able to resend the "old" file, without loosing anything. }

  // Delete Old file if file is older or equal to "lastcheck" time
  fname:=UserDataFile(uname)+'.msg.old';
  if not File_Exists(fname) then
    begin
    fname:=UserDataFile(uname)+'.msg.data';
    old:=False;
    end
  else if File_Age(fname)<=lastcheck then
    begin
    Delete_File(fname);
    fname:=UserDataFile(uname)+'.msg.data';
    old:=False;
    end;

  msgCS.Acquire;
  try
    if not File_Exists(fname) then // nothing to get
      Exit
    else
      begin
      if old then
        // resending old file, update time
        thischeck:=File_Age(fname)
      else
        begin
        // sending new file, rename file to ".old" and update time
        Rename_File(fname, UserDataFile(uname)+'.msg.old');
        fname:=UserDataFile(uname)+'.msg.old';
        thischeck:=File_Age(fname);
        end;
      end;
  finally
    msgCS.Release;
    end;

  // Read file content and create an array of mesages
  s:=Read_File(fname);
  at:=0; i:=0;

  Result:=TRtcArray.Create;
  while at<length(s) do
    begin
    rec:=TRtcValue.FromCode(s,at);
    try
      code:=rec.asString;
    finally
      rec.Free;
      end;
    DeCrypt(code, MSG_DATA_CRYPT_KEY);
    Result.asCode[i]:=code;
    Inc(i);
    end;
  end;

procedure TRtcMessengerUsers.AddFriend(uname, friend_name: string);
  var
    info,rec:TRtcRecord;
  begin
  if not Exists(friend_name) then
    raise Exception.Create('User "'+friend_name+'" not registered.');

  info:=LoadInfo(uname);
  try
    info.asRecord['friends'].asBoolean[friend_name]:=True;
    SaveInfo(uname,info);
  finally
    info.Free;
    end;

  info:=LoadInfo(friend_name);
  try
    if info.isType['friends']=rtc_Record then
      begin
      if info.asRecord['friends'].asBoolean[uname] then
        begin
        rec:=TRtcRecord.Create;
        try
          // Send me info about friend's status.
          if isLoggedIn(friend_name) then
            rec.asText['login']:=friend_name
          else
            rec.asText['logout']:=friend_name;
          SendData(uname, rec);
          // Send friend info about my status.
          rec.asText['login']:=uname;
          SendData(friend_name,rec);
        finally
          rec.Free;
          end;
        end;
      end;
  finally
    info.Free;
    end;

  rec:=TRtcRecord.Create;
  try
    rec.asText['addfriend']:=uname;
    SendData(friend_name, rec);
  finally
    rec.Free;
    end;
  end;

procedure TRtcMessengerUsers.AddIgnore(uname, ignore_name: string);
  var
    info,rec:TRtcRecord;
  begin
  if not Exists(ignore_name) then
    raise Exception.Create('User "'+ignore_name+'" not registered.');

  info:=LoadInfo(uname);
  try
    info.asRecord['ignore'].asBoolean[ignore_name]:=True;
    SaveInfo(uname,info);
  finally
    info.Free;
    end;

  rec:=TRtcRecord.Create;
  try
    rec.asText['addignore']:=uname;
    SendData(ignore_name, rec);
  finally
    rec.Free;
    end;
  end;

procedure TRtcMessengerUsers.DelFriend(uname, friend_name: string);
  var
    info,rec:TRtcRecord;
  begin
  if not Exists(friend_name) then
    raise Exception.Create('User "'+friend_name+'" not registered.');

  info:=LoadInfo(uname);
  try
    info.asRecord['friends'].isNull[friend_name]:=True;
    SaveInfo(uname,info);
  finally
    info.Free;
    end;

  rec:=TRtcRecord.Create;
  try
    rec.asText['delfriend']:=uname;
    SendData(friend_name, rec);
  finally
    rec.Free;
    end;
  end;

procedure TRtcMessengerUsers.DelIgnore(uname, ignore_name: string);
  var
    info,rec:TRtcRecord;
  begin
  if not Exists(ignore_name) then
    raise Exception.Create('User "'+ignore_name+'" not registered.');

  info:=LoadInfo(uname);
  try
    info.asRecord['ignore'].isNull[ignore_name]:=True;
    SaveInfo(uname,info);
  finally
    info.Free;
    end;

  rec:=TRtcRecord.Create;
  try
    rec.asText['delignore']:=uname;
    SendData(ignore_name, rec);
  finally
    rec.Free;
    end;
  end;

procedure TRtcMessengerUsers.SendText(const from_name, to_name, text:string);
  var
    rec:TRtcRecord;
  begin
  rec:=TRtcRecord.Create;
  try
    rec.asText['from']:=from_name;
    rec.asText['text']:=text;
    SendData(to_name,rec);
  finally
    rec.Free;
    end;
  end;

function TRtcMessengerUsers.isValidUserName(const uname: string): boolean;
  var
    a:integer;
  begin
  Result:=True;
  for a:=1 to length(uname) do
    case uname[a] of
      'a'..'z','A'..'Z','0'..'9','_',' ','.': Result:=True;
      else if Ord(uname[a])<128 then
        begin
        Result:=False;
        Break;
        end;
    end;
  end;

end.
