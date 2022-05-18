{ @html(<b>)
  Console Web Server Project
  @html(</b>)
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  When deploying this Web Server, make sure the "RTConWebServer.ini" file
  is in the same folder as the "RTConWebServer.exe",
  because it is used to configure the Server.

  @exclude }
program RTConWebServer;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  rtcConn,
  rtcLog,
  Server_Module in 'Server_Module.pas' {Data_Server: TDataModule},
  rtcFileProvider in '..\DataProviders\rtcFileProvider.pas' {File_Provider: TDataModule},
  rtcMessengerProvider in '..\DataProviders\rtcMessengerProvider.pas' {Messenger_Provider: TDataModule},
  rtcISAPIProvider in '..\DataProviders\rtcISAPIProvider.pas' {ISAPI_Provider: TDataModule},
  rtcISAPI in '..\DataProviders\rtcISAPI.pas',
  rtcMessenger in '..\DataProviders\rtcMessenger.pas';

var
  cmd:String;
  SrvPort:String;
begin
  try
    if ParamStr(1)<>'' then
      begin
      SrvPort:=ParamStr(1);
      GetDataServer.ServerHTTP.ServerPort:=SrvPort;
      GetDataServer.ServerHTTP6.ServerPort:=SrvPort;
      end;
    Writeln('HTTP Server Port = '+GetDataServer.ServerHTTP.ServerPort);

    {$IFDEF StreamSecII}
    if ParamStr(2)<>'' then
      begin
      SrvPort:=ParamStr(2);
      GetDataServer.ServerHTTPS.ServerPort:=SrvPort;
      GetDataServer.ServerHTTPS6.ServerPort:=SrvPort;
      end;
    Writeln('HTTPS (SSL) Server Port = '+GetDataServer.ServerHTTPS.ServerPort);
    {$ENDIF}

    Writeln('Folder '+ExtractFilePath(ParamStr(0)));

	  StartLog;

    Writeln('+-------------------------------+');
    Writeln('| Type ? for available commands |');
    Writeln('+-------------------------------+');

    // Function from "Server_Module" unit (create, setup and Start the Server)
    Writeln('Starting Server ...');
    GetDataServer.Start;

    repeat
      Readln(cmd);
      cmd:=LowerCase(cmd);
      if cmd='c' then
        Writeln('Connections: ',rtcTotalConnectionCount)
      else if cmd='s' then
        begin
        Writeln('IPv4 HTTP Server: ',GetDataServer.ServerHTTP.isListening,' @ '+GetDataServer.ServerHTTP.ServerPort);
        Writeln('IPv6 HTTP Server: ',GetDataServer.ServerHTTP6.isListening,' @ '+GetDataServer.ServerHTTP6.ServerPort);
        {$IFDEF StreamSecII}
        Writeln('IPv4 HTTPS Server: ',GetDataServer.ServerHTTPS.isListening,' @ '+GetDataServer.ServerHTTPS.ServerPort);
        Writeln('IPv6 HTTPS Server: ',GetDataServer.ServerHTTPS6.isListening,' @ '+GetDataServer.ServerHTTPS6.ServerPort);
        {$ENDIF}
        end
      else if cmd='x' then
        begin
        Writeln('Stopping Servers ...');
        GetDataServer.Stop;
        end
      else if cmd='r' then
        begin
        Writeln('Starting IPv6 and IPv4 Servers ...');
        GetDataServer.Start;
        end
      else if cmd='4' then
        begin
        Writeln('Starting IPv4 Server ...');
        GetDataServer.Start(True,False);
        end
      else if cmd='6' then
        begin
        Writeln('Starting IPv6 Server ...');
        GetDataServer.Start(False,True);
        end
      else if cmd='f' then
        begin
        LOG_TO_CONSOLE:=FALSE;
        XLOG_TO_CONSOLE:=FALSE;
        Writeln('Console LOGs turned OFF.');
        end
      else if cmd='o' then
        begin
        LOG_TO_CONSOLE:=TRUE;
        XLOG_TO_CONSOLE:=TRUE;
        Writeln('Console LOGs turned ON.');
        end
      else if cmd<>'q' then
        begin
        if cmd<>'?' then
          Writeln('Unrecognized command "'+cmd+'".');
        Writeln('+---------------------------+');
        Writeln('| Available commands:       |');
        Writeln('+===========================+');
        Writeln('| c - Connection count      |');
        Writeln('| s - Server status         |');
        Writeln('| x - Stop Servers          |');
        Writeln('| 4 - Run IPv4 Server       |');
        Writeln('| 6 - Run IPv6 Server       |');
        Writeln('| r - Run both Servers      |');
        Writeln('| o - Console LOGs "ON"     |');
        Writeln('| f - Console LOGs "OFF"    |');
        Writeln('| q - Stop Servers and Quit |');
        Writeln('+---------------------------+');
        end;
      until (cmd='q');

    // Function from "Server_Module" unit (stop the Server)
    GetDataServer.Stop;
  except
    on E:Exception do
      begin
      Writeln('Error! '+E.ClassName+': '+E.Message);
      Readln;
      end;
    end;
// Also check "Server_Module" units finalization section,
// where the Data_Module with Server components is destroyed.
end.
