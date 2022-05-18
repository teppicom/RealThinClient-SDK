program ConsolAppCli;

{$APPTYPE CONSOLE}

{$include rtcDefs.inc}
{$include rtcDeploy.inc}

uses
{$IFDEF RtcDeploy}
  {$IFNDEF IDE_2006up}
    FastMM4,
    FastMove,
  {$ENDIF}
{$ENDIF}
  SysUtils,
  SyncObjs,

  rtcTypes,
  rtcSystem,
  rtcInfo,
  rtcConn,
  rtcLog,
  rtcSynSocket,
  rtcSynAPI,
  rtcThrPool,
  rtcDataCli,
  rtcDataSrv,
  rtcHttpSrv,
  rtcHttpCli,
  rtcCliModule,
  rtcSrvModule,
  rtcFunction,
  rtcScript;

var
  cli:TRtcHttpClient;
  cm:TRtcClientModule;
  s_addr,s_port:RtcString;
  a,b:String;

procedure MakeClient;
  begin
  if assigned(cli) then
    begin
    cli.Disconnect;
    cli.Free;
    end;

  cli:=TRtcHttpClient.Create(nil);
  cli.ServerAddr:=s_addr;
  cli.ServerPort:=s_port;
  cli.AutoConnect:=True;
  cli.ReconnectOn.ConnectError:=True;
  cli.ReconnectOn.ConnectLost:=True;
  cli.ReconnectOn.ConnectFail:=True;
  //cli.MultiThreaded:=True;
  cli.Blocking:=True;
  System.Write('!Connect;');
  cli.Connect();
  System.Write('!Disconnect;');
  cli.Disconnect;
  System.Write('!Connect;');
  cli.Connect();
  end;

procedure MakeModule;
  begin
  if assigned(cm) then
    begin
    cm.Client:=nil;
    cm.Free;
    end;

  cm:=TRtcClientModule.Create(nil);
  cm.Client:=cli;
  cm.SecureKey:='This is a test.';
  cm.ModuleFileName:='/TEST';
  end;

procedure PrepareBigFunction(Func:TRtcFunctionInfo);
  var
    a:integer;
  begin
  with Func do
    begin
    with newRecord('data') do
      begin
      (*with NewFunction('func','add') do
        begin
        with NewFunction('A','add') do
          begin
          asInteger['A']:=random(1000000);
          asInteger['B']:=random(1000000);
          end;
        with NewFunction('B','mul') do
          begin
          asCardinal['A']:=random(1000000);
          asCardinal['B']:=random(1000000);
          end;
        end;*)
      with newArray('arr') do
        begin
        asBoolean[0]:=true;
        asBoolean[1]:=false;
        asInteger[2]:=random(123456789);
        asLargeInt[3]:=random(1234567890);
        asFloat[4]:=random(123456789)/1000;
        asCurrency[5]:=random(123456789)/100;
        asDateTime[6]:=Now;
        asException[7]:='Test Exception message';
        asVarName[8]:='Test Variable name';
        asWideString[9]:='Test Wide String';
        asText[10]:='Test Text';
        asString[11]:='Test String';
        asCardinal[12]:=random(1234567890);
        asOID[13]:=random(1234567890);
        with NewRecord(14) do
          begin
          asBoolean['abool1']:=true;
          asBoolean['abool2']:=false;
          asInteger['int']:=random(123456789);
          asLargeInt['lint']:=random(123456789);
          asFloat['float']:=random(123456789)/1000;
          asCurrency['curr']:=random(123456789)/100;
          asDateTime['dat']:=Now;
          asException['exc']:='Test Exception message';
          asVarName['var']:='Test Variable name';
          asWideString['wstr']:='Test Wide String';
          asText['txt']:='Test Text';
          asString['str']:='Test String';
          asCardinal['card']:=random(1234567890);
          asOID['oid']:=random(1234567890);
          end;
        with newArray(15) do
          begin
          asBoolean[0]:=true;
          asBoolean[1]:=false;
          asInteger[2]:=random(123456789);
          asLargeInt[3]:=random(1234567890);
          asFloat[4]:=random(123456789)/1000;
          asCurrency[5]:=random(123456789)/100;
          asDateTime[6]:=Now;
          asException[7]:='Test Exception message';
          asVarName[8]:='Test Variable name';
          asWideString[9]:='Test Wide String';
          asText[10]:='Test Text';
          asString[11]:='Test String';
          asCardinal[12]:=random(1234567890);
          asOID[13]:=random(1234567890);
          end;
        end;
      with newRecord('rec') do
        begin
        asBoolean['bbool1']:=true;
        asBoolean['bbool2']:=false;
        asInteger['int']:=random(123456789);
        asLargeInt['lint']:=random(1234567890);
        asFloat['float']:=random(123456789)/1000;
        asCurrency['curr']:=random(123456789)/100;
        asDateTime['dat']:=Now;
        asException['exc']:='Test Exception message';
        asVarName['var']:='Test Variable name';
        asWideString['wstr']:='Test Wide String';
        asText['txt']:='Test Text';
        asString['str']:='Test String';
        asCardinal['card']:=random(1234567890);
        asOID['oid']:=random(1234567890);
        with NewRecord('rec') do
          begin
          asBoolean['cbool1']:=true;
          asBoolean['cbool2']:=false;
          asInteger['int']:=random(123456789);
          asLargeInt['lint']:=random(1234567890);
          asFloat['float']:=random(123456789)/1000;
          asCurrency['curr']:=random(123456789)/100;
          asDateTime['dat']:=Now;
          asException['exc']:='Test Exception message';
          asVarName['var']:='Test Variable name';
          asWideString['wstr']:='Test Wide String';
          asText['txt']:='Test Text';
          asString['str']:='Test String';
          asCardinal['card']:=random(1234567890);
          asOID['oid']:=random(1234567890);
          end;
        with newArray('arr') do
          begin
          asBoolean[0]:=true;
          asBoolean[1]:=false;
          asInteger[2]:=random(123456789);
          asLargeInt[3]:=random(1234567890);
          asFloat[4]:=random(123456789)/1000;
          asCurrency[5]:=random(123456789)/100;
          asDateTime[6]:=Now;
          asException[7]:='Test Exception message';
          asVarName[8]:='Test Variable name';
          asWideString[9]:='Test Wide String';
          asText[10]:='Test Text';
          asString[11]:='Test String';
          asCardinal[12]:=random(1234567890);
          asOID[13]:=random(1234567890);
          end;
        end;
      with NewDataSet('dset') do
        begin
        for a:=1 to random(100)+10 do // 10-110 complex records
          begin
          Append;
          asBoolean['dbool1']:=true;
          asBoolean['dbool2']:=false;
          asInteger['int']:=random(123456789);
          asLargeInt['lint']:=int64(random(1234567890))*random(100000);
          asFloat['float']:=random(123456789)/1000;
          asCurrency['curr']:=random(123456789)/100;
          asDateTime['dat']:=Now;
          asException['exc']:='Test Exception message';
          asVarName['var']:='Test Variable name';
          asWideString['wstr']:='Test Wide String';
          asText['txt']:='Test Text';
          asString['str']:='Test String';
          asCardinal['card']:=random(1234567890);
          asOID['oid']:=random(1234567890);
          with newArray('arr') do
            begin
            asBoolean[0]:=true;
            asBoolean[1]:=false;
            asInteger[2]:=random(123456789);
            asLargeInt[3]:=int64(random(1234567890))*random(100000);
            asFloat[4]:=random(123456789)/1000;
            asCurrency[5]:=random(123456789)/100;
            asDateTime[6]:=Now;
            asException[7]:='Test Exception message';
            asVarName[8]:='Test Variable name';
            asWideString[9]:='Test Wide String';
            asText[10]:='Test Text';
            asString[11]:='Test String';
            asCardinal[12]:=random(1234567890);
            asOID[13]:=random(1234567890);
            with NewRecord(14) do
              begin
              asBoolean['ebool1']:=true;
              asBoolean['ebool2']:=false;
              asInteger['int']:=random(123456789);
              asLargeInt['lint']:=int64(random(1234567890))*random(100000);
              asFloat['float']:=random(1234567890)/10000;
              asCurrency['curr']:=random(1234567890)/100;
              asDateTime['dat']:=Now;
              asException['exc']:='Test Exception message';
              asVarName['var']:='Test Variable name';
              asWideString['wstr']:='Test Wide String';
              asText['txt']:='Test Text';
              asString['str']:='Test String';
              asCardinal['car']:=random(1234567890);
              asOID['oid']:=random(1234567890);
              end;
            with newArray(15) do
              begin
              asBoolean[0]:=true;
              asBoolean[1]:=false;
              asInteger[2]:=random(123456789);
              asLargeInt[3]:=random(1234567890);
              asFloat[4]:=random(123456789)/1000;
              asCurrency[5]:=random(123456789)/100;
              asDateTime[6]:=Now;
              asException[7]:='Test Exception message';
              asVarName[8]:='Test Variable name';
              asWideString[9]:='Test Wide String';
              asText[10]:='Test Text';
              asString[11]:='Test String';
              asCardinal[12]:=random(1234567890);
              asOID[13]:=random(1234567890);
              end;
            end;
          with newRecord('rec') do
            begin
            asBoolean['fbool1']:=true;
            asBoolean['fbool2']:=false;
            asInteger['int']:=random(1234567890);
            asLargeInt['lint']:=int64(random(1234567890))*random(100000);
            asFloat['float']:=random(1234567890)/(random(10000)+1);
            asCurrency['curr']:=random(1234567890)/(random(100)+1);
            asDateTime['dat']:=Now;
            asException['exc']:='Test Exception message';
            asVarName['var']:='Test Variable name';
            asWideString['wstr']:='Test Wide String';
            asText['txt']:='Test Text';
            asString['str']:='Test String';
            asCardinal['card']:=random(1234567890);
            asOID['oid']:=random(1234567890);
            with NewRecord('rec') do
              begin
              asBoolean['gbool1']:=true;
              asBoolean['gbool2']:=false;
              asInteger['int']:=random(123456789);
              asLargeInt['lint']:=random(1234567890);
              asFloat['float']:=random(123456789)/(random(1000)+1);
              asCurrency['curr']:=random(123456789)/(random(100)+1);
              asDateTime['dat']:=Now;
              asException['exc']:='Test Exception message';
              asVarName['var']:='Test Variable name';
              asWideString['wstr']:='Test Wide String';
              asText['txt']:='Test Text';
              asString['str']:='Test String';
              asCardinal['int']:=random(1234567890);
              asOID['oid']:=random(1234567890);
              end;
            with newArray('arr') do
              begin
              asBoolean[0]:=true;
              asBoolean[1]:=false;
              asInteger[2]:=random(123456789);
              asLargeInt[3]:=random(1234567890);
              asFloat[4]:=random(123456789)/1000;
              asCurrency[5]:=random(123456789)/100;
              asDateTime[6]:=Now;
              asException[7]:='Test Exception message';
              asVarName[8]:='Test Variable name';
              asWideString[9]:='Test Wide String';
              asText[10]:='Test Text';
              asString[11]:='Test String';
              asCardinal[12]:=random(1234567890);
              asOID[13]:=random(1234567890);
              end;
            end;
          end;
        end;
      end;
    end;
  end;

var
  Counter:int64;
  x:RtcString;
  z:longint;

begin
  try
    {$ifdef rtcTest}
    s_addr:='server';
    s_port:='81';
    StartLog;
    {$else}
    Write('Server adddress:');
    Readln(s_addr);
    if s_addr='' then s_addr:='localhost';
    Write('Server port:');
    Readln(s_port);
    if s_port='' then s_port:='81';
    {$endif}

    Writeln('Connecting to '+s_addr+' at '+s_port);
    MakeClient;
    MakeModule;
    Counter:=0;
    repeat
      a:=IntToStr(1+random(100000));
      b:=IntToStr(1+random(100000));

      Writeln;
      Write(' PrepareA ');
      cm.Prepare('add');
      cm.Param.asInteger['a']:=StrToInt(a);
      cm.Param.asInteger['b']:=StrToInt(b);
      z:=StrToInt(a)+StrToInt(b);
      try
        Inc(Counter);
        Write(Counter,'. Call: ');
        cm.Execute;
        if (cm.LastResult.asInteger=z) then
          Write('Correct Result received. ')
        else
          begin
          Write('Wrong Result received. ');
          Break;
          end;
        Write('A + B = ' + cm.LastResult.asString + ';');
      except
        on E:Exception do
          begin
          Writeln('Error :'+E.Message);

          //MakeClient;
          //MakeModule;
          end;
        end;

      Writeln;
      Write(' PrepareB ');
      PrepareBigFunction(cm.Data.NewFunction('loopo'));
      x:=cm.Data.asFunction.asString['data'];
      try
        Inc(Counter);
        Write(Counter,'. Call: ');
        cm.Execute;
        if (x=cm.LastResult.asString) then
          Write('Correct Result received. ')
        else
          begin
          Write('Wrong Result received. ');
          Break;
          end;

        Write('Len = ' + Int2Str(length(cm.LastResult.asString)),'/' + Int2Str(length(x))+';');
      except
        on E:Exception do
          begin
          Writeln('Error :'+E.Message);

          //MakeClient;
          //MakeModule;
          end;
        end;

      until false;

    cm.Client:=nil;
    cm.Free;

    cli.Disconnect;
    cli.Free;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  Readln;

end.
