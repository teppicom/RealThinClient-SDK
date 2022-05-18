{
  @html(<b>)
  Scripting Engine
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit implements the RTC Scripting Engine.
}
unit rtcScript;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSystem,
  rtcLog,

  rtcInfo,
  rtcConn,

  rtcFunction;

type
  // @exclude
  ERtcScript=class(Exception);

  // @exclude
  ERtcForward=class(EAbort);

  { RTC Script compiler class }
  TRtcScriptCompiler=class(TObject)
    private
      FGroup: TRtcFunctionGroup;
      FDenyRTCFunctionCalls: boolean;
      FDenyScriptFunctionCalls: boolean;

      FFilePath:RtcWideString;
      FFileName:RtcWideString;

      FScriptOpen1,FScriptOpen2,
      FScriptClose1,FScriptClose2:RtcWideChar;

      FScript:RtcWideString;
      Size,Loc:integer;
      row,col:integer;

      procedure skipWhitespace;

      function findScriptBegin:RtcWideString;

      function isParam: boolean;
      function isName: boolean;
      function isNumber: boolean;
      function isString(long_strings:boolean): boolean;
      function isOperator: boolean;
      function isFrontOperator: boolean;
      function isMidOperator: boolean;
      function isHighOperator: boolean;
      function isAssignment: boolean;
      function isComparison: boolean;
      function isClosing(const s:RtcWideString): boolean;
      function isOpening: boolean;
      function isSeparator: boolean;
      function isDot: boolean;

      function peekName:RtcWideString;

      function getName:RtcWideString;
      function getProperty:RtcWideString;
      function getNumber:RtcWideString;
      function getOperator:RtcWideString;
      function getOpening:RtcWideString;

      procedure skipClosing(const s:RtcWideString);
      procedure skipSeparator;
      procedure skipDot;

      function getString:RtcWideString;

      function Eof:boolean;

      function CompileValue(long_strings:boolean; MidOperators:boolean=True):TRtcValueObject;

      function CompileScript:TRtcValueObject;

      procedure SetScript(const Value: RtcWideString);

      function GetScriptClose: RtcWideString;
      function GetScriptOpen: RtcWideString;
      procedure SetScriptClose(const Value: RtcWideString);
      procedure SetScriptOpen(const Value: RtcWideString);

    public
      constructor Create;
      destructor Destroy; override;

      function Compile:TRtcValue;

      property Script:RtcWideString read FScript write SetScript;
      property FileName:RtcWideString read FFileName write FFileName;
      property FilePath:RtcWideString read FFilePath write FFilePath;

      { Characters used to open a script block.
        There always need to be exectly 2 characters for this,
        and only a subset of character combinations is allowed. }
      property ScriptOpen:RtcWideString read GetScriptOpen write SetScriptOpen;
      { Characters used to close a script block.
        There always need to be exectly 2 characters for this,
        and only a subset of character combinations is allowed. }
      property ScriptClose:RtcWideString read GetScriptClose write SetScriptClose;

      { If a FunctionGroup is assigned before compilation,
        each function name used in script will be checked (does the function exist?) }
      property FunctionGroup:TRtcFunctionGroup read FGroup write FGroup;

      { If you set DenyRTCFunctionCalls to TRUE, any attempts to call a RTC function
        (implemented using TRtcFunction and TRtcFunctionGroup components) from within
        the Script will fail and an exception will be raised. }
      property DenyRTCFunctionCalls:boolean read FDenyRTCFunctionCalls write FDenyRTCFunctionCalls;

      { If you set DenyScriptFunctionCalls to TRUE, any attempts to call a Script function
        (functions implemented inside the script) will fail (exception) during script execution. }
      property DenyScriptFunctionCalls:boolean read FDenyScriptFunctionCalls write FDenyScriptFunctionCalls;
      end;

  // @exclude
  TRtcScriptCommandInfo=class(TRtcCommandInfo)
    public
      Globals,Locals: TRtcRecord;
      MaxDepth, MaxRecurse, MaxLoop:cardinal;
      StopTime: int64;
      CodeDepth, RecurseCount: cardinal;
    end;

  { @abstract(RTC Script Engine) }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcScriptEngine=class(TRtcAbsCommand)
    private
      FGroup: TRtcFunctionGroup;
      FDenyRTCFunctionCalls: boolean;
      FDenyScriptFunctionCalls: boolean;
      FAutoCollapseArrays: boolean;

      FMaxLoopCount,
      FMaxExecutionTime,
      FMaxRecursion,
      FMaxCodeDepth: cardinal;

      FScriptOpen: RtcWideString;
      FScriptClose: RtcWideString;

      procedure SetScriptClose(const Value: RtcWideString);
      procedure SetScriptOpen(const Value: RtcWideString);

    protected
      // @exclude
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

      // @exclude
      function Call_Execute(const CmdInfo:TRtcCommandInfo; const Param:TRtcFunctionInfo; const Res:TRtcValue):boolean; override;

      // @exclude
      function Function_Exists(const Function_Name:RtcWideString):boolean; override;

    public
      // @exclude
      constructor Create(AOwner:TComponent); override;

      // @exclude
      destructor Destroy; override;

      { Compile a Script RtcWideString and return a compiled script object.

        Unless you pass the object to Execute(), in which case the
        object will be destroyed by the Execute() method, you have to
        take care of the compiled object (destroy it when not needed).

        If you are running complex scripts which take time to compile,
        you can compile the script(s) once (store in your variables).
        You can then send a copy of your script object to Execute() by
        using the ".copyOf" method on your script object ...
        Execute(Sender, Script.CopyOf);
        ... this way, you are keeping the (original) compiled script. }
      function Compile(const Script:RtcWideString; const FileName:RtcWideString=''):TRtcValue;

      { Execute Compiled Script object and return plain data object.

        You can call "asString" on the result object to get the RtcWideString,
        or traverse through the resulting object to get to every element.

        NOTE: Script passed as a parameter will always be destroyed,
        even if an exception was raised during script execution.
        This means that you can safely pass the result from Compile() to Execute().

        If you want to keep the compiled script which you are using for execution,
        you HAVE TO make a copy before using it in Execute().

        If you are running complex scripts which take a long time to compile,
        you can compile the script(s) once (store in your variables),
        then send a copy of your script object to Execute() by
        using the ".copyOf" method on your script object ...
        Execute(Sender, Script.CopyOf);
        ... this way, you are keeping the original script object. }
      function Execute(const Sender:TRtcConnection; const CompiledScript:TRtcValueObject; recursive:boolean=False):TRtcValue;

    published

      { To make your RTC functions accessible from your Scripts, write your functions
        using TRtcFunction components, assign them to a TRtcFunctiongroup component,
        then assign this TRtcFunctionGroup component here (FunctionGroup).

        If a FunctionGroup is assigned before compilation, each function name
        used in script will be checked (does the function exist?), and an exception
        will be raised if the Function does NOT exist in the assigned FunctionGroup.

        If you do NOT assign a FunctionGroup and try to execute a compiled script
        which is calling a RTC Function, an exception will be raised during Script execution. }
      property FunctionGroup:TRtcFunctionGroup read FGroup write FGroup;

      { If you set DenyRTCFunctionCalls to TRUE, any attempts to call a RTC function
        (implemented using TRtcFunction and TRtcFunctionGroup components) from within
        the Script will fail and an exception will be raised. }
      property DenyRTCFunctionCalls:boolean read FDenyRTCFunctionCalls write FDenyRTCFunctionCalls default False;

      { If you set DenyScriptFunctionCalls to TRUE, any attempts to define a Script function
        (functions implemented inside the script) will fail (exception) during script compilation. }
      property DenyScriptFunctionCalls:boolean read FDenyScriptFunctionCalls write FDenyScriptFunctionCalls default False;

      { Characters used to open a script block (default = <? ).
        There always need to be exectly 2 characters for this,
        and only a subset of character combinations is allowed. }
      property ScriptOpen:RtcWideString read FScriptOpen write SetScriptOpen;

      { Characters used to close a script block (default = ?> ).
        There always need to be exectly 2 characters for this,
        and only a subset of character combinations is allowed. }
      property ScriptClose:RtcWideString read FScriptClose write SetScriptClose;

      { Safety feature "Max code depth":
        Maximum allowed code depth inside Script (example: [(1 + 2) + 3] => 2 calls deep).
        Default value = 0 (no limit) }
      property MaxCodeDepth:cardinal read FMaxCodeDepth write FMaxCodeDepth default 0;

      { Safety feature "Max Recursion":
        Maximum allowed recursive calls from inside Script.
        If a script should try to enter more than this number of recursions,
        an exception will be raised and execution aborted.
        Default value = 0 (no limit) }
      property MaxRecursion:cardinal read FMaxRecursion write FMaxRecursion default 0;

      { Safety feature: "Max Loop Count":
        Maximum allowed number of loops inside a single repeat/until, for or while statement.
        If a script should try to run a single loop for more than specified count,
        an exception will be raised and execution aborted.
        Default value = 0 (no limit) }
      property MaxLoopCount:cardinal read FMaxLoopCount write FMaxLoopCount default 0;

      { Safety feature: "Max Execution Time":
        Maximum alowed single script execution time (seconds).
        If a script should try to run for longer than specified time,
        an exception will be raised and exection aborted.
        This is a safety feature to prevent you from having to restart the
        Server should there be a problem in one of your scripts (endless loops?)
        Default value = 0 (no limit). }
      property MaxExecutionTime:cardinal read FMaxExecutionTime write FMaxExecutionTime default 0;

      { When set to TRUE (default), assignments and other operations inside the
        RTC Script automatically collapse all Arrays containing a single element
        to return the actual element, instead of returning the entire Array.
        When set to FALSE, this feature will be disabled, making it possible for
        variables accepting results from functions returning Arrays with a single
        element to store the actual Array, instead of the element from the array. }
      property AutoCollapseArrays:boolean read FAutoCollapseArrays write FAutoCollapseArrays default True;
      end;

implementation

  //RTC_NAMESTART_CHARSET:set of RtcWideChar=['a'..'z','A'..'Z','_','$','@'];
  //RTC_NAME_CHARSET:set of RtcWideChar=['a'..'z','A'..'Z','0'..'9','_','$','@'];
  //RTC_NAME_CHARSET2:set of RtcWideChar=['a'..'z','A'..'Z','0'..'9','_','$','@','.'];

  //RTC_NUMSTART_CHARSET:set of RtcWideChar=['0'..'9'];
  //RTC_NUM_CHARSET:set of RtcWideChar=['0'..'9','.'];

  //RTC_OPERATOR_CHARSET:set of RtcWideChar=['+','-','*','/','%','>','<','=','&','|','!',':','^','#'];
  //RTC_ASSIGN_CHARSET:set of RtcWideChar=['+','-','*','/','%','&','|',':','^'];

  //RTC_LIST_SEPARATORS:set of RtcWideChar=[',',';'];

  //RTC_WHITESPACE:set of RtcWideChar=[#9,#32,#10,#13];

{ TRtcScriptCompiler }

constructor TRtcScriptCompiler.Create;
  begin
  inherited;
  ScriptOpen:='<?';
  ScriptClose:='?>';
  FFilePath:='';
  FFileName:='';
  FScript:='';
  Loc:=1;
  Row:=1;Col:=1;
  Size:=0;
  FGroup:=nil;
  FDenyRTCFunctionCalls:=False;
  FDenyScriptFunctionCalls:=False;
  end;

destructor TRtcScriptCompiler.Destroy;
  begin
  try
    FFilePath:='';
    FFileName:='';
    FFilePath:='';
    FScript:='';
    FGroup:=nil;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcScriptCompiler.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcScriptCompiler.SetScript(const Value: RtcWideString);
  begin
  FScript:=Value;
  Loc:=1;
  Row:=1;Col:=1;
  Size:=length(FScript);
  end;

function TRtcScriptCompiler.Eof: boolean;
  begin
  Result:=loc>size;
  end;

function TRtcScriptCompiler.findScriptBegin: RtcWideString;
  var
    i:integer;
    found:boolean;
  begin
  found:=false;
  for i:=loc to size-1 do
    begin
    if (Script[i]=FScriptOpen1) and (Script[i+1]=FScriptOpen2) then
      begin
      found:=True;
      // Get text before <?
      Result:=Copy(Script,loc,i-loc);
      loc:=i;
      Break;
      end
    else if Script[i]=#10 then
      begin
      Inc(row);
      col:=1;
      end
    else
      Inc(col);
    end;

  if not found then
    begin
    // Get all remaining text
    Result:=Copy(Script,loc,size+1-loc);
    loc:=size+1;
    end;
  end;

procedure TRtcScriptCompiler.skipWhitespace;
  begin
  while loc<size do
    begin
    case Script[loc] of
      #32,#13,#9:
        begin
        Inc(col);
        Inc(loc);
        end;
      #10:
        begin
        Inc(loc);
        Inc(row);
        col:=1;
        end;
      '/':
        begin
        if Script[loc+1]='/' then
          begin
          // Skip //
          Inc(loc,2);Inc(col,2);
          while (loc<size) and (Script[loc]<>#10) do
            begin
            Inc(loc);Inc(col);
            end;
          // skip #10
          Inc(loc); Inc(row); col:=1;
          end
        else
          Break;
        end;
      '(':
        begin
        if Script[loc+1]='*' then
          begin
          // Skip (*
          Inc(loc,2);Inc(col,2);
          while (loc<size) do
            begin
            case Script[loc] of
              '*':if Script[loc+1]=')' then
                    Break
                  else
                    begin
                    Inc(loc);
                    Inc(col);
                    end;
              #10:begin
                  Inc(loc);
                  Inc(row);
                  col:=1;
                  end;
              else
                begin
                Inc(loc);Inc(col);
                end;
              end;
            end;
          if loc<size then
            begin
            // skip *)
            Inc(loc,2);Inc(col,2);
            end
          else
            raise ERtcScript.Create('Comment started with "(*", but not closed with "*)"');
          end
        else
          Break;
        end;
      else
        Break;
      end;
    end;
  end;

function TRtcScriptCompiler.isString(long_strings:boolean): boolean;
  begin
  case Script[loc] of
    '"',#39: Result:=True;
    else
      if long_strings and (Script[loc]=FScriptClose1) and (Script[loc+1]=FScriptClose2) then
        Result:=True
      else
        Result:=False;
    end;
  end;

function TRtcScriptCompiler.getString:RtcWideString;
  var
    i,c,r:integer;
    found:boolean;
    terminator:RtcWideChar;
  begin
  if (loc<size) and (Script[loc]=FScriptClose1) and (Script[loc+1]=FScriptClose2) then
    begin
    // skip ?>
    Inc(loc,2); Inc(col,2);

    found:=false;
    for i:=loc to size-1 do
      begin
      if (Script[i]=FScriptOpen1) and (Script[i+1]=FScriptOpen2) then
        begin
        found:=True;
        // Get text before <?
        Result:=Copy(Script,loc,i-loc);
        loc:=i+2; Inc(col,2);
        Break;
        end
      else if Script[i]=#10 then
        begin
        Inc(row);
        col:=1;
        end
      else
        Inc(col);
      end;

    if not found then
      begin
      // Get all remaining text
      Result:=Copy(Script,loc,size+1-loc);
      loc:=size+1;
      end;
    end
  else
    begin
    terminator:=Script[loc];

    // skip " '
    Inc(loc);Inc(col);

    c:=col; r:=row;
    found:=false;

    if terminator='"' then
      begin
      for i:=loc to size do
        case Script[i] of
          '"':
            begin
            col:=c; row:=r;
            found:=True;
            if i=loc then
              Result:=''
            else
              Result:=Copy(Script,loc,i-loc);
            loc:=i+1;
            Break;
            end;
          #10:
            begin
            Break;
            {Inc(r); c:=1;}
            end;
          else
            Inc(c);
          end;
      end
    else
      begin
      for i:=loc to size do
        case Script[i] of
          #39:
            begin
            col:=c; row:=r;
            found:=True;
            if i=loc then
              Result:=''
            else
              Result:=Copy(Script,loc,i-loc);
            loc:=i+1;
            Break;
            end;
          #10:
            begin
            Break;
            { Inc(r); c:=1; }
            end;
          else
            Inc(c);
          end;
      end;
    if not found then
      raise ERtcScript.Create('Unterminated short RtcWideString (starts with '+String(terminator)+').'#13#10+
                              'Short strings have to be closed before the end of line.');
    end;
  end;

function TRtcScriptCompiler.isName: boolean;
  begin
  case Script[loc] of
    'a'..'z','A'..'Z','_','$','@': Result:=True;
    else Result:=Ord(Script[loc])>=128;
    end;
  end;

function TRtcScriptCompiler.isDot: boolean;
  begin
  Result:=Script[loc]='.';
  end;

function TRtcScriptCompiler.isParam: boolean;
  var
    i:integer;
    ws:boolean;
  begin
  ws:=False;
  Result:=false;
  for i:=loc to size-1 do
    case Script[i] of
      'a'..'z','A'..'Z','0'..'9','_','$','@':
        if ws then Break;
      #9,#32,#10,#13:
        ws:=True;
      ':':
        begin
        case Script[i+1] of
          '<','>','=':begin end;
          else Result:=True;
          end;
        Break;
        end
      else if (Ord(Script[i])<128) or ws then
        Break;
    end;
  end;

function TRtcScriptCompiler.getName: RtcWideString;
  var
    i:integer;
    found:boolean;
    r,c:integer;
  begin
  r:=row; c:=col;
  found:=false;
  for i:=loc to size-1 do
    case Script[i] of
      'a'..'z','A'..'Z','0'..'9','_','$','@':
        Inc(c);
      else
        if Ord(Script[i])>=128 then
          Inc(c)
        else
          begin
          row:=r; col:=c;
          found:=True;
          // Get name
          Result:=Copy(Script,loc,i-loc);
          loc:=i;
          Break;
          end;
      end;

  if not found then
    raise ERtcScript.Create('Malformed Name, ending with '+Char(Script[i]));
  end;

function TRtcScriptCompiler.getProperty: RtcWideString;
  var
    i:integer;
    found:boolean;
    r,c:integer;
  begin
  r:=row; c:=col;
  found:=false;
  for i:=loc to size-1 do
    case Script[i] of
      'a'..'z','A'..'Z','0'..'9','_','$','@','.':
        Inc(c);
      else
        if Ord(Script[i])>=128 then
          Inc(c)
        else
          begin
          row:=r; col:=c;
          found:=True;
          // Get name
          Result:=Copy(Script,loc,i-loc);
          loc:=i;
          Break;
          end;
      end;

  if not found then
    raise ERtcScript.Create('Malformed Name, ending with '+Char(Script[i]));
  end;

function TRtcScriptCompiler.peekName: RtcWideString;
  var
    i:integer;
    found:boolean;
  begin
  found:=false;
  for i:=loc to size-1 do
    case Script[i] of
      'a'..'z','A'..'Z','0'..'9','_','$','@':
        found:=True;
      else
        if Ord(Script[i])>=128 then
          found:=True
        else
          begin
          if found then
            Result:=Copy(Script,loc,i-loc);
          Break;
          end;
      end;

  if not found then
    Result:='';
  end;

function TRtcScriptCompiler.isNumber: boolean;
  begin
  case Script[loc] of
    '0'..'9': Result:=True;
    else Result:=False;
    end;
  end;

function TRtcScriptCompiler.getNumber: RtcWideString;
  var
    i:integer;
    found:boolean;
    r,c:integer;
  begin
  r:=row; c:=col;
  found:=false;
  for i:=loc to size-1 do
    case Script[i] of
      '0'..'9','.':
        Inc(c);
      else
        begin
        row:=r; col:=c;
        found:=True;
        // Get number
        Result:=Copy(Script,loc,i-loc);
        loc:=i;
        Break;
        end;
      end;

  if not found then
    raise ERtcScript.Create('Malformed Number, ending with '+Char(Script[i]));
  end;

function TRtcScriptCompiler.isOperator: boolean;
  begin
  case Script[loc] of
    '+','-','*','/','\','%','>','<','=','&','|','!',':','^','#': Result:=True;
    else Result:=False;
    end;
  end;

function TRtcScriptCompiler.isAssignment: boolean;
  begin
  if (Script[loc+1]<>'=') then
    Result:=False
  else
    case Script[loc] of
      '+','-','*','/','\','%','&','|',':','^':Result:=True;
      else Result:=False;
      end;
  end;

function TRtcScriptCompiler.isComparison: boolean;
  begin
  case Script[loc] of
    '<','>','=': Result:=True;
    else
      if Script[loc+1]='=' then
        Result:= Script[loc]='!'
      else
        Result:=False;
    end;
  end;

function TRtcScriptCompiler.isFrontOperator: boolean;
  begin
  case Script[loc] of
    '-','!','#','^':
      Result:= (Script[loc+1]<>'=');
    else
      Result:=False;
    end;
  end;

function TRtcScriptCompiler.isMidOperator: boolean;
  begin
  if Script[loc]='!' then
    begin
    if Script[loc+1]='=' then
      Result:=True
    else
      Result:=False;
    end
  else
    Result:=isOperator;
  end;

function TRtcScriptCompiler.isHighOperator: boolean;
  var
    n:RtcWideString;
  begin
  case Script[loc] of
    '*','/','%','^':
      Result:=True;
    else
      if isName then
        begin
        n:=UpperCase(peekName);
        Result:=(n='DIV') or (n='MOD');
        end
      else
        Result:=False;
    end;
  end;

function TRtcScriptCompiler.getOperator: RtcWideString;
  begin
  case Script[loc] of
    '<','>','=':
      begin
      case Script[loc+1] of
        '<','>','=':
          begin
          Result:=RtcWideString('')+Script[loc]+Script[loc+1];
         Inc(loc,2);Inc(col,2);
          end
        else
          begin
          Result:=RtcWideChar(Script[loc]);
          Inc(loc);Inc(col);
          end;
        end;
      end;
    '+','-','*','/','\','%','&','|','!',':','^','#':
      begin
      if Script[loc+1]='=' then
        begin
        Result:=RtcWideString('')+Script[loc]+Script[loc+1];
        Inc(loc,2);Inc(col,2);
        end
      else if (Script[loc]=':') and ( (Script[loc+1]='>') or (Script[loc+1]='<') ) then
        begin
        Result:=RtcWideString('')+Script[loc]+Script[loc+1];
        Inc(loc,2);Inc(col,2);
        end
      else
        begin
        Result:=Script[loc];
        Inc(loc);Inc(col);
        end;
      end
    else
      raise ERtcScript.Create('Malformed operator, starting with '+Char(Script[loc]));
    end;
  end;

function TRtcScriptCompiler.isOpening: boolean;
  begin
  case Script[loc] of
    '(','[','{':
      Result:=True;
    else
      if isName and (UpperCase(peekName)='BEGIN') then
        Result:=True
      else
        Result:=False;
    end;
  end;

function TRtcScriptCompiler.getOpening: RtcWideString;
  begin
  case Script[loc] of
    '(','[','{':
      begin
      Result:=Script[loc];
      Inc(loc);Inc(col);
      end
    else
      if isName and (UpperCase(peekName)='BEGIN') then
        Result:=getName
      else
        Result:='';
    end;
  end;

function TRtcScriptCompiler.isClosing(const s:RtcWideString): boolean;
  begin
  if (s='(') and (Script[loc]=')') then
    begin
    Result:=True;
    end
  else if (s='[') and (Script[loc]=']') then
    begin
    Result:=True;
    end
  else if (s='{') and (Script[loc]='}') then
    begin
    Result:=True;
    end
  else if (UpperCase(s)='BEGIN') and isName and (UpperCase(peekName)='END') then
    begin
    Result:=True;
    end
  else
    Result:=False;
  end;

procedure TRtcScriptCompiler.skipClosing(const s:RtcWideString);
  begin
  if (s='(') and (Script[loc]=')') then
    begin
    Inc(loc);Inc(col);
    end
  else if (s='[') and (Script[loc]=']') then
    begin
    Inc(loc);Inc(col);
    end
  else if (s='{') and (Script[loc]='}') then
    begin
    Inc(loc);Inc(col);
    end
  else if (UpperCase(s)='BEGIN') and isName and (UpperCase(peekName)='END') then
    begin
    getName;
    end
  else
    raise ERtcScript.Create('Opened with "'+String(s)+'", closing with "'+Char(Script[loc])+'"?');
  end;

function TRtcScriptCompiler.isSeparator: boolean;
  begin
  case Script[loc] of
    ',',';': Result:=True;
    else Result:=False;
    end;
  end;

procedure TRtcScriptCompiler.skipSeparator;
  begin
  Inc(loc);Inc(col);
  end;

procedure TRtcScriptCompiler.skipDot;
  begin
  Inc(loc);Inc(col);
  skipWhiteSpace;
  end;

function TRtcScriptCompiler.CompileValue(long_strings:boolean; MidOperators:boolean=True): TRtcValueObject;
  var
    idx:integer;
    op, fname, parname:RtcWideString;
    obj: TRtcValueObject;
    func: TRtcFunctionInfo;
    arr: TRtcArray;
    rec: TRtcRecord;
  begin
  Result:=nil;
  try
    if isSeparator then
      begin
      Result:=nil;
      skipSeparator;
      Exit; // No more processing!
      end
    else if isFrontOperator then
      begin
      fname:=getOperator;

      func:=TRtcFunctionInfo.Create;
      func.FunctionName:='?';
      func.asInteger['$R']:=row;
      func.asInteger['$C']:=col;
      func.asString['C']:=RtcString(fname);
      Result:=func;

      skipWhitespace;
      func.asObject['Y']:=CompileValue(True, False);
      end
    else if isString(long_strings) then
      Result:=TRtcTextValue.Create(getString)
    else if isNumber then
      begin
      fname:=getNumber;
      if Pos('.',fname)=0 then
        Result:=TRtcLargeIntValue.Create(StrToInt64(String(fname)))
      else
        Result:=TRtcFloatValue.Create(Str2Float(RtcString(fname)));
      end
    else if isOpening then
      begin
      op:=getOpening;

      skipWhitespace;
      if isClosing(op) then
        Result:=nil
      else if isParam then // record (fields with names)
        begin
        obj:=nil;
        rec:=nil;
        Result:=nil;
        idx:=0;

        repeat
          if isParam then
            begin
            parname:=getProperty;
            skipWhitespace;
            // Skip ":"
            Inc(loc);Inc(col);
            skipWhitespace;
            end
          else
            parname:=Int2WStr(idx);

          if not (isSeparator or isClosing(op)) then
            begin
            obj:=CompileValue(true);
            if assigned(obj) then
              begin
              if not assigned(rec) then
                begin
                rec:=TRtcRecord.Create;
                Result:=rec;
                end;
              rec.asObject[parname]:=obj;
              end;
            SkipWhitespace;
            end;
          Inc(idx);

          if isSeparator then
            begin
            skipSeparator;
            skipWhitespace;
            end
          else if not isClosing(op) then
            begin
            if not (obj is TRtcTextValue) then
              if not isString(long_strings) then
                raise ERtcScript.Create('List separator or closing expected');
            end;

          until isClosing(op);
        end
      else // array (enumeration, no element names)
        begin
        obj:=nil;
        arr:=nil;
        Result:=nil;
        idx:=0;

        repeat
          if not (isSeparator or isClosing(op)) then
            begin
            obj:=CompileValue(true);
            if assigned(obj) then
              begin
              if not assigned(Result) then
                Result:=obj
              else if not assigned(arr) then
                begin
                arr:=TRtcArray.Create;
                arr.asObject[idx-1]:=Result;
                arr.asObject[idx]:=obj;
                Result:=arr;
                end
              else
                arr.asObject[idx]:=obj;
              end;
            skipWhitespace;
            end;
          Inc(idx);

          if isSeparator then
            begin
            skipSeparator;
            skipWhitespace;
            end
          else if not isClosing(op) then
            begin
            if not (obj is TRtcTextValue) then
              if not isString(long_strings) then
                raise ERtcScript.Create('List separator or closing expected');
            end;

          until isClosing(op);
        end;

      skipClosing(op);
      end
    else if isName then
      begin
      fname:=GetName;
      parname:=UpperCase(fname);
      skipWhitespace;

      if (parname='CODE') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='@';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        Result:=func;

        func.asObject['Y']:=CompileValue(True, True);
        end
      else if (parname='NEW') then
        begin
        if not isName then
          raise ERtcScript.Create('Type name expected after NEW');

        fname:=UpperCase(GetName);
        skipWhiteSpace;

        if (fname<>'ARRAY') and
           (fname<>'RECORD') and
           (fname<>'DATASET') and
           (fname<>'STREAM') and (fname<>'BYTESTREAM') and
           (fname<>'INTEGER') and (fname<>'INT') and (fname<>'INT32') and
           (fname<>'LARGEINT') and (fname<>'INT64') and
           (fname<>'DATETIME') and (fname<>'DATE') and (fname<>'TIME') and
           (fname<>'FLOAT') and (fname<>'DOUBLE') and
           (fname<>'CURRENCY') and
           (fname<>'BOOLEAN') and (fname<>'BOOL') and
           (fname<>'WIDESTRING') and
           (fname<>'TEXT') then
          raise ERtcScript.Create('Type "'+String(fname)+'" not supported in NEW');

        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='?';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='NEW';
        func.asString['X']:=RtcString(fname);
        Result:=func;
        end
      else if (parname='NULL') or (parname='NIL') then
        Result:=nil
      else if (parname='NOT') or (parname='RAISE') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='?';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:=RtcString(parname);
        Result:=func;

        skipWhitespace;
        func.asObject['Y']:=CompileValue(True);
        end
      else if (parname='IF') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='I';
        Result:=func;

        func.asObject['I']:=CompileValue(TRUE);
        skipWhitespace;
        if not isName or (UpperCase(getName)<>'THEN') then
          raise ERtcScript.Create('"IF" statement: "THEN" expected');
        skipWhitespace;
        func.asObject['X']:=CompileValue(TRUE);
        skipWhitespace;
        if isName then
          begin
          if UpperCase(getName)<>'ELSE' then
            raise ERtcScript.Create('"IF" statement: "ELSE" or ";" expected');
          skipWhitespace;
          func.asObject['Y']:=CompileValue(TRUE);
          end;
        end
      else if (parname='REPEAT') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='R';
        Result:=func;

        obj:=nil;
        arr:=nil;
        idx:=0;
        repeat
          if not isSeparator and not (isName and (UpperCase(peekName)='UNTIL')) then
            begin
            obj:=CompileValue(true);
            if assigned(obj) then
              begin
              if not assigned(arr) then
                arr:=func.newArray('X');
              arr.asObject[idx]:=obj;
              end;
            skipWhitespace;
            end;
          Inc(idx);

          if isSeparator then
            begin
            repeat
              skipSeparator;
              skipWhitespace;
              until not isSeparator;
            if isName and (UpperCase(peekName)='UNTIL') then
              Break;
            end
          else if isName and (UpperCase(peekName)='UNTIL') then
            Break
          else
            begin
            if not (obj is TRtcTextValue) then
              if not isString(long_strings) then
                raise ERtcScript.Create('"REPEAT" statement: Command separator or "UNTIL" expected');
            end;
          until false;

        // skip "UNTIL"
        getName;
        skipWhitespace;

        func.asObject['I']:=CompileValue(TRUE);
        end
      else if (parname='WHILE') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='W';
        Result:=func;

        func.asObject['I']:=CompileValue(TRUE);
        skipWhitespace;
        if not isName or (UpperCase(getName)<>'DO') then
          raise ERtcScript.Create('"WHILE" statement: "DO" expected');
        skipWhitespace;
        func.asObject['X']:=CompileValue(TRUE);
        end
      else if (parname='FOREACH') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='E';
        Result:=func;

        func.asObject['I']:=CompileValue(TRUE);
        skipWhitespace;

        if not isName or (UpperCase(getName)<>'DO') then
          raise ERtcScript.Create('"FOREACH" statement: "DO" expected');
        skipWhitespace;

        func.asObject['X']:=CompileValue(TRUE);
        end
      else if (parname='FOR') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='F';
        Result:=func;

        if not isName then
          raise ERtcScript.Create('"FOR" statement: Variable name expected');
        parname:=getName;

        if Copy(parname,1,1)<>'$' then
          raise ERtcScript.Create('"FOR" statement: Variable name has to start with "$"');

        func.asVarName['V']:=Copy(parname,2,length(parname)-1);
        skipWhitespace;

        if not isOperator or (getOperator<>':=') then
          raise ERtcScript.Create('"FOR" statement: ":=" expected');
        skipWhitespace;

        func.asObject['A']:=CompileValue(TRUE);
        skipWhitespace;

        if not isName then
          raise ERtcScript.Create('"FOR" statement: "TO" or "DOWNTO" expected');
        parname:=UpperCase(getName);
        if parname='TO' then
          func.asBoolean['D']:=True
        else if parname='DOWNTO' then
          func.asBoolean['D']:=False
        else
          raise ERtcScript.Create('"FOR" statement: "TO" or "DOWNTO" expected');
        skipWhitespace;

        func.asObject['B']:=CompileValue(TRUE);
        skipWhitespace;

        if not isName or (UpperCase(getName)<>'DO') then
          raise ERtcScript.Create('"FOR" statement: "DO" expected');
        skipWhitespace;

        func.asObject['X']:=CompileValue(TRUE);
        end
      else if (parname='FUNCTION') then
        begin
        if DenyScriptFunctionCalls then
          raise ERtcScript.Create('Local script Functions denied (see Script Compiler properties).');

        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='X';
        Result:=func;

        if not isName then
          raise ERtcScript.Create('"FUNCTION" declaration: Function name expected');
        parname:=getName;

        if Copy(parname,1,1)<>'$' then
          raise ERtcScript.Create('"FUNCTION" declaration: Script Function names have to start with "$"');

        func.asVarName['V']:=Copy(parname,2,length(parname)-1);
        skipWhitespace;

        func.asObject['X']:=CompileValue(TRUE);
        end
      else if (parname='TRUE') then
        begin
        Result:=TRtcBooleanValue.Create(true);
        end
      else if (parname='FALSE') then
        begin
        Result:=TRtcBooleanValue.Create(false);
        end
      else if (parname='AND') or (parname='OR') or (parname='XOR') or
         (parname='IN') or (parname='IS') then
        raise ERtcScript.Create('"'+String(parname)+'" without Left-side parameter')
      else if (parname='ELSE') then
        raise ERtcScript.Create('"ELSE" without "IF"')
      else if (parname='UNTIL') then
        raise ERtcScript.Create('"UNTIL" without "REPEAT"')
      else if (parname='DO') then
        raise ERtcScript.Create('"DO" without "WHILE", "FOR" or "FOREACH"')
      else if (parname='TO') or (parname='DOWNTO') then
        raise ERtcScript.Create('"'+String(parname)+'" without "FOR"')
      else if (parname='END') then
        raise ERtcScript.Create('"END" without "BEGIN"')
      else if (parname='THEN') then
        raise ERtcScript.Create('"THEN" without "IF"')
      else
        begin
        if not isOpening then
          begin
          if Copy(fname,1,1)='$' then
            begin
            if fname='$' then
              raise ERtcScript.Create('Parameter expected after $');
            // Variable names always begin with $
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:=Copy(fname,2,length(fname)-1);
            end
          else if Copy(fname,1,1)='@' then
            begin
            if fname='@' then
              raise ERtcScript.Create('Parameter expected after @');
            // Variable names always begin with $
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:=Copy(fname,2,length(fname)-1);
            func.asBoolean['$A']:=True;
            end
          else if (parname='REQUEST') or
             (parname='RESPONSE') or
             (parname='SESSION') or
             (parname='QUERY') or
             (parname='INPUT') or
             (parname='CLIENT') or
             (parname='SERVER') then
            begin
            if (parname<>'CLIENT') and (parname<>'SERVER') and
               (parname<>'INPUT') and (parname<>'QUERY') and not isDot then
              raise ERtcScript.Create('Parameter expected after '+String(parname));
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:='-';
            func.asVarName['$P']:=parname;
            end
          else
            begin
            if assigned(FGroup) then
              begin
              if not FGroup.FunctionExists(fname) then
                raise ERtcScript.Create('RTC Function "'+String(fname)+'" not found in FunctionGroup.')
              else if DenyRTCFunctionCalls then
                raise ERtcScript.Create('Can not call RTC Function "'+String(fname)+'" (RTC Function calls denied).');
              end
            else if DenyRTCFunctionCalls then
              raise ERtcScript.Create('Can not call RTC Function "'+String(fname)+'" (no FunctionGroup and RTC Function calls denied).');

            func:=TRtcFunctionInfo.Create;
            func.FunctionName:=fname;
            end;
          Result:=func;
          end
        else
          begin
          op:=getOpening;

          if Copy(fname,1,1)='$' then
            begin
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:=Copy(fname,2,length(fname)-1);
            end
          else if Copy(fname,1,1)='@' then
            begin
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:=Copy(fname,2,length(fname)-1);
            func.asBoolean['$A']:=True;
            end
          else if (parname='REQUEST') or
             (parname='RESPONSE') or
             (parname='SESSION') or
             (parname='QUERY') or
             (parname='INPUT') or
             (parname='CLIENT') or
             (parname='SERVER') then
            begin
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:='-';
            func.asVarName['$P']:=parname;
            end
          else
            begin
            if assigned(FGroup) then
              begin
              if not FGroup.FunctionExists(fname) then
                raise ERtcScript.Create('RTC Function "'+String(fname)+'" not found in FunctionGroup.')
              else if DenyRTCFunctionCalls then
                raise ERtcScript.Create('Can not call RTC Function "'+String(fname)+'" (RTC Function calls denied).');
              end
            else if DenyRTCFunctionCalls then
              raise ERtcScript.Create('Can not call RTC Function "'+String(fname)+'" (no FunctionGroup and RTC Function calls denied).');

            func:=TRtcFunctionInfo.Create;
            func.FunctionName:=fname;
            end;
          Result:=func;

          skipWhitespace;
          if isClosing(op) then
            begin
            // No parameters
            end
          else if isParam then // parameters with names
            begin
            obj:=nil;
            idx:=0;
            repeat
              if isParam then
                begin
                parname:=getProperty;
                skipWhitespace;
                // Skip ":"
                Inc(loc);Inc(col);
                skipWhitespace;
                end
              else
                parname:=Int2WStr(idx);

              if not (isSeparator or isClosing(op)) then
                begin
                obj:=CompileValue(true);
                if assigned(obj) then
                  func.asObject[parname]:=obj;
                skipWhitespace;
                end;
              Inc(idx);

              if isSeparator then
                begin
                skipSeparator;
                skipWhitespace;
                end
              else if not isClosing(op) then
                begin
                if not (obj is TRtcTextValue) then
                  if not isString(long_strings) then
                    raise ERtcScript.Create('List separator or closing expected');
                end;

              until isClosing(op);
            end
          else // parameters without names - will be passed as "PARAMS" array
            begin
            obj:=nil;
            arr:=nil;
            idx:=0;
            repeat
              if not (isSeparator or isClosing(op)) then
                begin
                obj:=CompileValue(true);
                if assigned(obj) then
                  begin
                  if not assigned(arr) then
                    arr:=func.newArray('PARAMS');
                  arr.asObject[idx]:=obj;
                  end;
                skipWhitespace;
                end;
              Inc(idx);

              if isSeparator then
                begin
                skipSeparator;
                skipWhitespace;
                end
              else if not isClosing(op) then
                begin
                if not (obj is TRtcTextValue) then
                  if not isString(long_strings) then
                    raise ERtcScript.Create('List separator or closing expected');
                end;

              until isClosing(op);
            end;

          skipClosing(op);
          end;
        end;
      end
    else
      raise ERtcScript.Create('Script Error! Terminated with "'+Char(Script[loc])+'"');

    skipWhitespace;
    while isDot do
      begin
      skipDot;

      fname:=GetProperty;
      skipWhitespace;

      if not isOpening then
        begin
        func:=TRtcFunctionInfo.Create;

        func.FunctionName:='?';
        func.asString['C']:='.';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asObject['X']:=Result;
        func.asString['Y']:=RtcString(fname);
        Result:=func;
        end
      else
        begin
        op:=getOpening;

        func:=TRtcFunctionInfo.Create;

        func.FunctionName:='?';
        func.asString['C']:='.';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asObject['X']:=Result;
        Result:=func;
      
        with func.newFunction('Y','$.') do
          begin
          asInteger['$R']:=row;
          asInteger['$C']:=col;
          asVarName['$V']:=fname;

          skipWhitespace;
          if not isClosing(op) then
            begin
            asObject['X']:=CompileValue(True);
            skipWhitespace;
            if not isClosing(op) then
              raise ERtcScript.Create('Closing expected');
            end;
          end;
        skipClosing(op);
        end;
      skipWhitespace;
      end;

    if MidOperators then
      begin
      repeat
        if isAssignment then
          begin
          fname:=getOperator;
          skipWhitespace;

          if Result is TRtcFunctionInfo then
            begin
            func:=TRtcFunctionInfo(Result);
            if (func.FunctionName<>'$') and
               ( (func.FunctionName<>'?') or (func.asString['C']<>'.') ) then
              raise ERtcScript.Create('Left side can not be assigned to');
            end
          else
            raise ERtcScript.Create('Left side can not be assigned to');

          func:=TRtcFunctionInfo.Create;
          func.FunctionName:='$!';
          func.asString['C']:=RtcString(fname);
          func.asInteger['$R']:=row;
          func.asInteger['$C']:=col;
          func.asObject['X']:=Result;
          Result:=func;

          func.asObject['Y']:=CompileValue(True, True);
          end
        else if isComparison then
          begin
          fname:=getOperator;
          skipWhitespace;

          func:=TRtcFunctionInfo.Create;
          func.FunctionName:='?';
          func.asString['C']:=RtcString(fname);
          func.asInteger['$R']:=row;
          func.asInteger['$C']:=col;
          func.asObject['X']:=Result;
          Result:=func;

          func.asObject['Y']:=CompileValue(True, True);
          end
        else if isMidOperator then
          begin
          fname:=getOperator;
          skipWhitespace;

          func:=TRtcFunctionInfo.Create;
          func.FunctionName:='?';
          func.asString['C']:=RtcString(fname);
          func.asInteger['$R']:=row;
          func.asInteger['$C']:=col;
          func.asObject['X']:=Result;
          Result:=func;

          func.asObject['Y']:=CompileValue(True, False);
          end
        else if isName then
          begin
          parname:=UpperCase(peekName);

          if (parname='AND') or (parname='OR') or (parname='XOR') or
             (parname='IN') or (parname='IS') or
             (parname='DIV') or (parname='MOD') or
             (parname='SHL') or (parname='SHR') then
            begin
            fname:=getName;
            skipWhitespace;

            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='?';
            func.asString['C']:=RtcString(fname);
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asObject['X']:=Result;
            Result:=func;

            func.asObject['Y']:=CompileValue(True, False);
            end
          else
            Break;
          end
        else
          Break;
        skipWhitespace;
        until not (isMidOperator or isName);
      end
    else if isHighOperator then
      begin
      if isName then
        fname:=getName
      else
        fname:=getOperator;
      skipWhitespace;

      func:=TRtcFunctionInfo.Create;
      func.FunctionName:='?';
      func.asString['C']:=RtcString(fname);
      func.asInteger['$R']:=row;
      func.asInteger['$C']:=col;
      func.asObject['X']:=Result;
      Result:=func;

      func.asObject['Y']:=CompileValue(True, False);
      end;
  except
    on E:Exception do
      begin
      RtcFreeAndNil(Result);
      raise;
      end;
    end;
  end;

function TRtcScriptCompiler.CompileScript: TRtcValueObject;
  var
    arr:TRtcArray;
    obj:TRtcValueObject;
  begin
  // Skip '<?'
  Inc(loc,2); Inc(col,2);

  Result:=nil; arr:=nil;

  try
    repeat
      skipWhitespace;
      obj:=CompileValue(false);
      if assigned(obj) then
        begin
        if not assigned(Result) then
          Result:=obj
        else if not assigned(arr) then
          begin
          arr:=TRtcArray.Create;
          arr.asObject[0]:=Result;
          arr.asObject[1]:=obj;
          Result:=arr;
          end
        else
          arr.asObject[arr.Count]:=obj;
        end;

      skipWhitespace;
      if isSeparator then
        begin
        skipSeparator;
        skipWhitespace;
        if (Script[loc]=FScriptClose1) and (Script[loc+1]=FScriptClose2) then
          Break;
        end
      else if (Script[loc]=FScriptClose1) and (Script[loc+1]=FScriptClose2) then
        Break
      else if not isString(false) then
        if not (assigned(obj) and (obj is TRtcTextValue)) then
          raise ERtcScript.Create('Command separator or script closing expected')
      until false;
  except
    on E:Exception do
      begin
      RtcFreeAndNil(Result);
      raise;
      end;
    end;

  // Skip '?>'
  Inc(loc,2); Inc(col,2);
  end;

function TRtcScriptCompiler.Compile: TRtcValue;
  var
    arr:TRtcArray;
    obj:TRtcValueObject;
    str:RtcWideString;
  begin
  Result:=TRtcValue.Create;
  arr:=Result.newArray;
  try
    while not Eof do
      begin
      str:=findScriptBegin;
      if str<>'' then
        arr.asText[arr.Count]:=str;

      if Eof then Break;
      obj:=CompileScript;
      if assigned(obj) then
        arr.asObject[arr.Count]:=obj;
      end;
  except
    on E:ERtcForward do
      begin
      RtcFreeAndNil(Result);
      raise ERtcScript.Create('['+IntToStr(row)+':'+IntToStr(col)+'] '+
                              'Compiling '+String(FFileName)+':'+#13#10+String(E.Message));
      end;
    on E:Exception do
      begin
      RtcFreeAndNil(Result);
      raise ERtcScript.Create('['+IntToStr(row)+':'+IntToStr(col)+'] '+
                              String(E.ClassName)+' compiling '+String(FFileName)+':'+#13#10+String(E.Message));
      end;
    end;
  end;

function TRtcScriptCompiler.GetScriptClose: RtcWideString;
  begin
  Result:=RtcWideString('')+FScriptClose1+FScriptClose2;
  end;

function TRtcScriptCompiler.GetScriptOpen: RtcWideString;
  begin
  Result:=RtcWideString('')+FScriptOpen1+FScriptOpen2;
  end;

procedure TRtcScriptCompiler.SetScriptClose(const Value: RtcWideString);
  begin
  if length(Value)<>2 then
    raise Exception.Create('Need 2 characters for Script closing')
  else
    begin
    FScriptClose1:=value[1];
    FScriptClose2:=value[2];
    end;
  end;

procedure TRtcScriptCompiler.SetScriptOpen(const Value: RtcWideString);
  begin
  if length(Value)<>2 then
    raise Exception.Create('Need 2 characters for Script opening')
  else
    begin
    FScriptOpen1:=value[1];
    FScriptOpen2:=value[2];
    end;
  end;

{ TRtcScriptEngine }

constructor TRtcScriptEngine.Create(AOwner: TComponent);
  begin
  inherited;
  FScriptOpen:='<?';
  FScriptClose:='?>';
  FGroup:=nil;
  FDenyRTCFunctionCalls:=False;
  FDenyScriptFunctionCalls:=False;
  FAutoCollapseArrays:=True;
  end;

destructor TRtcScriptEngine.Destroy;
  begin
  try
    FGroup:=nil;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcScriptEngine.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcScriptEngine.Function_Exists(const Function_Name: RtcWideString): boolean;
  begin
  Result:=(Function_Name='!') or // commands
          (Function_Name='@') or // code block
          (Function_Name='?') or // functions
          (Function_Name='$') or // read variable
          (Function_Name='$.') or // property
          (Function_Name='$!'); // assign to variable
  end;

function TRtcScriptEngine.Call_Execute(const CmdInfo: TRtcCommandInfo;
                                       const Param: TRtcFunctionInfo;
                                       const Res: TRtcValue): boolean;
  var
    Comm:TRtcScriptCommandInfo;

    func:RtcWideString;
    obj,objB:TRtcValue;
    ctype:TRtcValueTypes;

  function GetAsString(const obj:TRtcValueObject):RtcString;
    begin
    if obj is TRtcSimpleValue then
      Result:=TRtcSimpleValue(obj).GetString
    else if obj is TRtcValue then
      Result:=TRtcValue(obj).asString
    else if obj is TRtcArray then
      Result:=TRtcArray(obj).GetAsString
    else if obj is TRtcRecord then
      Result:=TRtcRecord(obj).GetAsString
    else if obj is TRtcDataSet then
      Result:=TRtcDataSet(obj).GetAsString
    else
      raise EConvertError.Create('Can not convert '+obj.ClassName+' to String.');
    end;

  function GetAsText(const obj:TRtcValueObject):RtcWideString;
    begin
    if obj is TRtcSimpleValue then
      Result:=TRtcSimpleValue(obj).GetText
    else if obj is TRtcValue then
      Result:=TRtcValue(obj).asText
    else if obj is TRtcArray then
      Result:=TRtcArray(obj).GetAsText
    else if obj is TRtcRecord then
      Result:=TRtcRecord(obj).GetAsText
    else if obj is TRtcDataSet then
      Result:=TRtcDataSet(obj).GetAsText
    else
      raise EConvertError.Create('Can not convert '+obj.ClassName+' to RtcWideString.');
    end;

  function GetAsBoolean(const obj:TRtcValueObject):boolean;
    begin
    if obj is TRtcSimpleValue then
      Result:=TRtcSimpleValue(obj).GetBoolean
    else if obj is TRtcValue then
      Result:=TRtcValue(obj).asBoolean
    else
      raise EConvertError.Create('Can not convert '+obj.ClassName+' to Boolean.');
    end;

  function GetAsInteger(const obj:TRtcValueObject):Longint;
    begin
    if obj is TRtcSimpleValue then
      Result:=TRtcSimpleValue(obj).GetInteger
    else if obj is TRtcValue then
      Result:=TRtcValue(obj).asInteger
    else
      raise EConvertError.Create('Can not convert '+obj.ClassName+' to Integer.');
    end;

  function GetAsInt64(const obj:TRtcValueObject):int64;
    begin
    if obj is TRtcSimpleValue then
      Result:=TRtcSimpleValue(obj).GetLargeInt
    else if obj is TRtcValue then
      Result:=TRtcValue(obj).asLargeInt
    else
      raise EConvertError.Create('Can not convert '+obj.ClassName+' to LargeInt.');
    end;

  function GetAsDateTime(const obj:TRtcValueObject):TDateTime;
    begin
    if obj is TRtcSimpleValue then
      Result:=TRtcSimpleValue(obj).GetDateTime
    else if obj is TRtcValue then
      Result:=TRtcValue(obj).asDateTime
    else
      raise EConvertError.Create('Can not convert '+obj.ClassName+' to DateTime.');
    end;

  procedure Expose(var obj:TRtcValueObject);
    var
      obj2:TRtcValueObject;
    begin
    while obj is TRtcValue do
      begin
      obj2:=TRtcValue(obj).asObject;
      TRtcValue(obj).asObject:=nil;
      RtcFreeAndNil(obj);
      obj:=obj2;
      end;
    end;

  { Check param executes "paramname" and stores the result back into
    the variable, then returns the pointer to that variable.
    You should NOT destroy the object returned. }
  function CheckParam(const parname:RtcWideString):TRtcValue;
    var
      obj1,obj2:TRtcValueObject;
    begin
    // remove old pointer
    obj.asObject:=nil;
    obj1:=Param.asObject[parname];

    obj2:=Comm.Group.ExecuteData(Comm, obj1, false);
    if obj2<>obj1 then
      begin
      Expose(obj2);
      // destroy old object, store new in place
      Param.isNull[parname]:=true;
      Param.asObject[parname]:=obj2;
      obj.asObject:=obj2;
      end
    else
      obj.asObject:=obj1;
    Result:=obj;
    end;

  { Check param executes "paramname" and stores the result back into
    the variable, then returns the pointer to that variable.
    You should NOT destroy the object returned. }
  function CheckParamB(const parname:RtcWideString):TRtcValue;
    var
      obj1,obj2:TRtcValueObject;
    begin
    // remove old pointer
    objB.asObject:=nil;
    obj1:=Param.asObject[parname];

    obj2:=Comm.Group.ExecuteData(Comm, obj1, false);
    if obj2<>obj1 then
      begin
      Expose(obj2);
      // destroy old object, store new in place
      Param.isNull[parname]:=true;
      Param.asObject[parname]:=obj2;
      objB.asObject:=obj2;
      end
    else
      objB.asObject:=obj1;
    Result:=objB;
    end;

  { Execute Param returns the result, destroying the original.
    You should take care of destroying the object returned. }
  function ExecuteParam(const parname:RtcWideString):TRtcValueObject;
    var
      obj1,obj2:TRtcValueObject;
    begin
    obj.asObject:=nil;
    obj1:=Param.asObject[parname];

    obj2:=Comm.Group.ExecuteData(Comm, obj1, false);
    if obj2<>obj1 then
      begin
      Expose(obj2);
      // destroy old object
      Param.isNull[parname]:=true;
      obj.asObject:=obj2;
      Result:=obj2;
      end
    else
      begin
      // extract object pointer
      Param.asObject[parname]:=nil;
      obj.asObject:=obj1;
      Result:=obj1;
      end;
    end;

  { Execute Param returns the result, destroying the original.
    You should take care of destroying the object returned. }
  function ExecuteParam_X:TRtcValueObject;
    var
      obj1,obj2:TRtcValueObject;
    begin
    obj.asObject:=nil;
    obj1:=Param.asObject['X'];

    obj2:=Comm.Group.ExecuteData(Comm, obj1, false);
    if obj2<>obj1 then
      begin
      Expose(obj2);
      // destroy old object
      Param.isNull['X']:=true;
      obj.asObject:=obj2;
      Result:=obj2;
      end
    else
      begin
      // extract object pointer
      Param.asObject['X']:=nil;
      obj.asObject:=obj1;
      Result:=obj1;
      end;
    end;

  { Execute Param returns the result, destroying the original.
    You should take care of destroying the object returned. }
  function ExecuteParam_Y:TRtcValueObject;
    var
      obj1,obj2:TRtcValueObject;
    begin
    obj.asObject:=nil;
    obj1:=Param.asObject['Y'];

    obj2:=Comm.Group.ExecuteData(Comm, obj1, false);
    if obj2<>obj1 then
      begin
      Expose(obj2);
      // destroy old object
      Param.isNull['Y']:=true;
      obj.asObject:=obj2;
      Result:=obj2;
      end
    else
      begin
      // extract object pointer
      Param.asObject['Y']:=nil;
      obj.asObject:=obj1;
      Result:=obj1;
      end;
    end;

  { Execute Param returns the result, destroying the original.
    You should take care of destroying the object returned. }
  function ExecuteParamB(const parname:RtcWideString):TRtcValueObject;
    var
      obj1,obj2:TRtcValueObject;
    begin
    objB.asObject:=nil;
    obj1:=Param.asObject[parname];

    obj2:=Comm.Group.ExecuteData(Comm, obj1, false);
    if obj2<>obj1 then
      begin
      Expose(obj2);
      // destroy old object
      Param.isNull[parname]:=true;
      objB.asObject:=obj2;
      Result:=obj2;
      end
    else
      begin
      // extract object pointer
      Param.asObject[parname]:=nil;
      objB.asObject:=obj1;
      Result:=obj1;
      end;
    end;

  { Execute Param returns the result, destroying the original.
    You should take care of destroying the object returned. }
  function ExecuteParamB_Y:TRtcValueObject;
    var
      obj1,obj2:TRtcValueObject;
    begin
    objB.asObject:=nil;
    obj1:=Param.asObject['Y'];

    obj2:=Comm.Group.ExecuteData(Comm, obj1, false);
    if obj2<>obj1 then
      begin
      Expose(obj2);
      // destroy old object
      Param.isNull['Y']:=true;
      objB.asObject:=obj2;
      Result:=obj2;
      end
    else
      begin
      // extract object pointer
      Param.asObject['Y']:=nil;
      objB.asObject:=obj1;
      Result:=obj1;
      end;
    end;

  { Execute Command returns the result, preserving the original.
    You should take care of destroying the object returned. }
  function ExecuteCommand(const parname:RtcWideString):TRtcValueObject;
    var
      orig:TRtcValueObject;
    begin
    orig := Param.asObject[parname];
    if not assigned(orig) then
      Result:=nil
    else
      begin
      orig:=orig.copyOf;
      Result:=ExecuteParam(parname);
      Param.asObject[parname] := orig;
      end;
    end;

  procedure Execute_If;
    begin
    if CheckParam('I').asBoolean then
      Res.asObject:=ExecuteParam_X
    else
      Res.asObject:=ExecuteParam_Y;
    end;

  procedure Execute_Repeat;
    var
      new_cmds:TRtcValueObject;
      new_cond:TRtcValueObject;
      arr:TRtcArray;
      cnt:cardinal;
    begin
    arr:=nil;
    new_cond:=nil;
    new_cmds:=nil;
    cnt:=0;
    try
      repeat
        if Comm.StopTime>0 then
          if GetTickTime64>Comm.StopTime then
            raise ERtcScript.Create('Script Execution Time Limit exceeded.');

        if Comm.MaxLoop>0 then
          if cnt>Comm.MaxLoop then
            raise ERtcScript.Create('Maximum Loop count exceeded')
          else
            Inc(cnt);

        RtcFreeAndNil(new_cond);

        new_cmds:=ExecuteCommand('X');
        if assigned(new_cmds) then
          begin
          if not assigned(arr) then
            arr:=Res.NewArray;
          arr.asObject[arr.Count]:=new_cmds;
          new_cmds:=nil;
          end;

        new_cond := ExecuteCommand('I');
        until obj.asBoolean;
    finally
      RtcFreeAndNil(new_cond);
      RtcFreeAndNil(new_cmds);
      end;
    end;

  procedure Execute_While;
    var
      new_cmds:TRtcValueObject;
      new_cond:TRtcValueObject;
      arr:TRtcArray;
      cnt:cardinal;
    begin
    arr:=nil;
    new_cmds:=nil;
    new_cond:=nil;
    cnt:=0;
    try
      new_cond := ExecuteCommand('I');
      while obj.asBoolean do
        begin
        if Comm.StopTime>0 then
          if GetTickTime64>Comm.StopTime then
            raise ERtcScript.Create('Script Execution Time Limit exceeded.');

        if Comm.MaxLoop>0 then
          if cnt>Comm.MaxLoop then
            raise ERtcScript.Create('Maximum Loop count exceeded')
          else
            Inc(cnt);

        RtcFreeAndNil(new_cond);

        new_cmds:=ExecuteCommand('X');
        if assigned(new_cmds) then
          begin
          if not assigned(arr) then
            arr:=Res.NewArray;
          arr.asObject[arr.Count]:=new_cmds;
          new_cmds:=nil;
          end;

        new_cond := ExecuteCommand('I');
        end;
    finally
      RtcFreeAndNil(new_cond);
      RtcFreeAndNil(new_cmds);
      end;
    end;

  procedure Execute_For;
    var
      a,b,i:longint;
      vname:RtcWideString;
      arr:TRtcArray;
      new_cmds:TRtcValueObject;
      cnt:cardinal;
    begin
    vname:=CheckParam('V').asVarName;
    a:=CheckParam('A').asInteger;
    b:=CheckParam('B').asInteger;

    // remove anything else that might have been stored in our loop variable
    Comm.Locals.isNull[vname]:=true;
    arr:=nil; new_cmds:=nil; cnt:=0;
    try
      if CheckParam('D').asBoolean then
        begin
        for i:=a to b do
          begin
          if Comm.StopTime>0 then
            if GetTickTime64>Comm.StopTime then
              raise ERtcScript.Create('Script Execution Time Limit exceeded.');

          if Comm.MaxLoop>0 then
            if cnt>Comm.MaxLoop then
              raise ERtcScript.Create('Maximum Loop count exceeded')
            else
              Inc(cnt);

          // set loop variable value
          Comm.Locals.asInteger[vname]:=i;

          new_cmds:=ExecuteCommand('X');
          if assigned(new_cmds) then
            begin
            if not assigned(arr) then
              arr:=Res.NewArray;
            arr.asObject[arr.Count]:=new_cmds;
            new_cmds:=nil;
            end;
          end;
        end
      else
        begin
        for i:=a downto b do
          begin
          if Comm.StopTime>0 then
            if GetTickTime64>Comm.StopTime then
              raise ERtcScript.Create('Script Execution Time Limit exceeded.');

          if Comm.MaxLoop>0 then
            if cnt>Comm.MaxLoop then
              raise ERtcScript.Create('Maximum Loop count exceeded')
            else
              Inc(cnt);

          // set loop variable value
          Comm.Locals.asInteger[vname]:=i;

          new_cmds:=ExecuteCommand('X');
          if assigned(new_cmds) then
            begin
            if not assigned(arr) then
              arr:=Res.NewArray;
            arr.asObject[arr.Count]:=new_cmds;
            new_cmds:=nil;
            end;
          end;
        end;
    finally
      RtcFreeAndNil(new_cmds);
      end;
    end;

  procedure Execute_ForEach;
    begin
    // TODO: Foreach
    raise ERtcScript.Create('"FOREACH" loops are not yet implemented.');
    end;

  function Collapse(const obj:TRtcValueObject):TRtcValueObject;
    var
      i,cnt,x:integer;
      arr:TRtcArray;
    begin
    if obj is TRtcArray then
      begin
      arr:=TRtcArray(obj);
      cnt:=0; x:=0;
      for i:=0 to arr.Count-1 do
        if arr.isType[i]<>rtc_Null then
          begin
          x:=i;
          Inc(cnt);
          if cnt>1 then Break;
          end;
      if cnt=0 then
        Result:=nil
      else if cnt=1 then
        begin
        Result:=arr.asObject[x];
        arr.asObject[x]:=nil;
        end
      else
        Result:=obj;
      end
    else
      Result:=obj;
    end;

  function _Execute(const data:TRtcValueObject):TRtcValueObject;
    var
      temp:TRtcValueObject;
    begin
    if not assigned(data) then
      Result:=nil
    else
      begin
      temp:=data.copyOf;
      Result:=temp;
      try
        Result:=Comm.Group.ExecuteData(Comm, temp);
      finally
        if Result<>temp then RtcFreeAndNil(temp);
        end;
      temp:=Collapse(Result);
      if temp<>Result then
        begin
        RtcFreeAndNil(Result);
        Result:=temp;
        end;
      end;
    end;

  function Get_Variable(vname:RtcWideString; var isCopy:boolean; asAddr:boolean):TRtcValueObject; forward;

  function Get_Property(obj:TRtcValueObject; s:RtcWideString; var isCopy:boolean; asAddr:boolean):TRtcValueObject;
    var
      dot:integer;
      vname:RtcWideString;

    procedure CheckCodeBlocks;
      var
        done:boolean;
        myvar:TRtcFunctionInfo;
        objX:TRtcValueObject;
        asAddr2:boolean;
        vname2,vname3:RtcWideString;

        temp:TRtcValueObject;
        tempLocals:TRtcRecord;
        par:TRtcRecord;
        fname:RtcWideString;
        i:integer;

      begin
      if Result is TRtcFunctionInfo then
        begin
        done:=False;

        myvar:=TRtcFunctionInfo(Result);
        while (myvar.FunctionName='@') and (myvar.isType['Y']=rtc_Function) do
          myvar:=myvar.asFunction['Y'];

        if (myvar.FunctionName='$') then
          begin
          vname2:=myvar.asVarName['$V'];
          if vname2='-' then
            vname2:=vname2+myvar.asVarName['$P'];
          asAddr2:=myvar.asBoolean['$A'];
          objX:=_Execute(myvar.asObject['PARAMS']);
          try
            if assigned(objX) then
              begin
              vname3:=GetAsText(objX);
              if vname3<>'' then
                begin
                if vname2<>'' then vname2:=vname2+'.';
                vname2:=vname2+vname3;
                end;
              end;
          finally
            RtcFreeAndNil(ObjX);
            end;

          if Result<>obj then
            RtcFreeAndNil(obj);

          if isCopy then
            RtcFreeAndNil(Result);

          isCopy:=False;
          done:=True;

          Result:=Get_Variable(vname2, isCopy, asAddr2);
          if isCopy then obj:=Result;
          end
        else if (myvar.FunctionName='?') and (myvar.asString['C']='.') then
          begin
          done:=True;
          vname2:='';
          repeat
            objX:=_Execute(myvar.asObject['Y']);
            try
              vname3:=GetAsText(objX);
              if vname3<>'' then
                begin
                if vname2<>'' then vname2:='.'+vname2;
                vname2:=vname3+vname2;
                end;
            finally
              RtcFreeAndNil(objX);
              end;
            if myvar.isType['X']<>rtc_Function then
              begin
              done:=False;
              Break;
              end;
            myvar:=TRtcFunctionInfo(myvar.asObject['X']);
            until (myvar.FunctionName<>'?') or (myvar.asString['C']<>'.');

          if done and (myvar.FunctionName<>'$') then
            done:=False;

          if done then
            begin
            objX:=_Execute(myvar.asObject['PARAMS']);
            try
              if assigned(objX) then
                begin
                vname3:=GetAsText(objX);
                if vname3<>'' then
                  begin
                  if vname2<>'' then vname2:='.'+vname2;
                  vname2:=vname3+vname2;
                  end;
                end;
            finally
              RtcFreeAndNil(ObjX);
              end;

            vname3:=myvar.asVarName['$V'];
            if vname3='-' then
              vname3:=vname3+myvar.asVarName['$P'];
            if vname3<>'' then
              begin
              if vname2<>'' then vname2:='.'+vname2;
              vname2:=vname3+vname2;
              end;
            asAddr2:=myvar.asBoolean['$A'];

            if Result<>obj then
              RtcFreeAndNil(obj);

            if isCopy then
              RtcFreeAndNil(Result);

            isCopy:=False;

            Result:=Get_Variable(vname2, isCopy, asAddr2);
            if isCopy then obj:=Result;
            end;
          end
        else if myvar.FunctionName='!!' then
          begin
          if Comm.MaxRecurse>0 then
            if Comm.RecurseCount>=Comm.MaxRecurse then
              raise ERtcScript.Create('Maximum allowed Recursion Depth exceeded.')
            else
              Inc(Comm.RecurseCount);

          try
            Par:=TRtcRecord.Create;
            try
              for i:=0 to Param.FieldCount-1 do
                begin
                fname:=Param.FieldName[i];
                if (fname<>'$C') and (fname<>'$R') and (fname<>'$V') then
                  begin
                  Par.asObject[fname]:=Comm.Group.ExecuteData(Comm, Param.asObject[fname]);
                  if Par.asObject[fname]<>Param.asObject[fname] then
                    Param.isNull[fname]:=True
                  else
                    Param.asObject[fname]:=nil;
                  end;
                end;

              temp:=myvar.asObject['X'];
              if assigned(temp) then
                temp:=temp.copyOf;
              try
                // changing Local variables to Global variables
                // and input parameters to Local variables
                tempLocals:=Comm.Locals;
                Comm.Locals:=Par;
                objX:=nil;
                try
                  objX:=Comm.Group.ExecuteData(Comm, temp);
                finally
                  Comm.Locals:=tempLocals;
                  if assigned(objX) and (objX<>temp) then RtcFreeAndNil(objX);
                  end;
              finally
                RtcFreeAndNil(temp);
                end;

              // Get the result
              if Result<>obj then
                RtcFreeAndNil(obj);

              if isCopy then
                RtcFreeAndNil(Result);

              Result:=Par.asObject['RESULT'];
              Par.asObject['RESULT']:=nil;

              isCopy:=True;
              obj:=Result;
              done:=True;
            finally
              RtcFreeAndNil(Par);
              end;
          finally
            if Comm.MaxRecurse>0 then
              Dec(Comm.RecurseCount);
            end;
          end;

        if not done then
          begin
          if Result<>obj then
            RtcFreeAndNil(obj);

          if isCopy then
            obj:=Result // so we can Free it afterwards
          else
            obj:=nil;

          Result:=_Execute(Result);

          RtcFreeAndNil(obj); // free old Result object if it was a copy

          isCopy:=True;
          obj:=Result;
          end;
        end;
      end;

    begin
    s:=UpperCase(s);
    Result:=obj;
    obj:=nil;

    CheckCodeBlocks;
    try
      while s<>'' do
        begin
        dot:=Pos('.',s);
        if dot>0 then
          begin
          vname:=Copy(s,1,dot-1);
          Delete(s,1,dot);
          end
        else
          begin
          vname:=s;
          s:='';
          end;

        while assigned(Result) and (Result is TRtcValue) do
          Result:=TRtcValue(Result).asObject;

        if Result=nil then
          raise ERtcScript.Create('Can not get ".'+String(vname)+'" from NULL');

        if Result is TRtcArray then
          begin
          if (vname='COUNT') or (vname='FIELDCOUNT') then
            begin
            isCopy:=True;
            Result:=TRtcIntegerValue.Create(TRtcArray(Result).Count);
            Break;
            end
          else
            Result:=TRtcArray(Result).asObject[Str2Int(vname)];
          end
        else if Result is TRtcRecord then
          begin
          if (vname='COUNT') or (vname='FIELDCOUNT') then
            begin
            isCopy:=True;
            Result:=TRtcIntegerValue.Create(TRtcRecord(Result).Count);
            Break;
            end
          else
            Result:=TRtcRecord(Result).asObject[vname];
          end
        else if Result is TRtcDataSet then
          begin
          if vname='FIRST' then
            begin
            TRtcDataSet(Result).First;
            Result:=nil;
            end
          else if vname='LAST' then
            begin
            TRtcDataSet(Result).Last;
            Result:=nil;
            end
          else if vname='NEXT' then
            begin
            TRtcDataSet(Result).Next;
            Result:=nil;
            end
          else if vname='PRIOR' then
            begin
            TRtcDataSet(Result).Prior;
            Result:=nil;
            end
          else if vname='INSERT' then
            begin
            TRtcDataSet(Result).Insert;
            Result:=nil;
            end
          else if vname='APPEND' then
            begin
            TRtcDataSet(Result).Append;
            Result:=nil;
            end
          else if vname='DELETE' then
            begin
            TRtcDataSet(Result).Delete;
            Result:=nil;
            end
          else
            begin
            if (vname='COUNT') or (vname='ROWCOUNT') then
              begin
              Result:=TRtcIntegerValue.Create(TRtcDataSet(Result).RowCount);
              isCopy:=True;
              Break;
              end
            else if vname='ROW' then
              begin
              Result:=TRtcIntegerValue.Create(TRtcDataSet(Result).Row);
              isCopy:=True;
              Break;
              end
            else if vname='FIELDCOUNT' then
              begin
              Result:=TRtcIntegerValue.Create(TRtcDataSet(Result).FieldCount);
              isCopy:=True;
              Break;
              end
            else if vname='EOF' then
              begin
              Result:=TRtcBooleanValue.Create(TRtcDataSet(Result).Eof);
              isCopy:=True;
              Break;
              end
            else if vname='EMPTY' then
              begin
              Result:=TRtcBooleanValue.Create(TRtcDataSet(Result).Empty);
              isCopy:=True;
              Break;
              end
            else if vname='BOF' then
              begin
              Result:=TRtcBooleanValue.Create(TRtcDataSet(Result).Bof);
              isCopy:=True;
              Break;
              end
            else
              case vname[1] of
                '0'..'9': Result:=TRtcDataSet(Result).asObject[TRtcDataSet(Result).FieldName[Str2Int(vname)]];
                else      Result:=TRtcDataSet(Result).asObject[vname];
              end;
            end;
          end
        else
          raise ERtcScript.Create('Can not get ".'+String(vname)+'" from a simple data type');
        CheckCodeBlocks;
        end;
    finally
      if assigned(obj) and (obj<>Result) then
        begin
        if assigned(Result) then
          Result:=Result.copyOf;
        RtcFreeAndNil(obj);
        end;
      end;
    end;

  procedure Set_Variable(s:RtcWideString; var value:TRtcValueObject; asAddr:boolean);
    var
      obj:TRtcValueObject;
      vname:RtcWideString;

    procedure GetNext;
      var
        dot:integer;
      begin
      // Get first RtcWideString before dot
      dot:=Pos('.',s);
      if dot>0 then
        begin
        vname:=Copy(s,1,dot-1);
        Delete(s,1,dot);
        end
      else
        begin
        vname:=s;
        s:='';
        end;
      end;

    procedure SetVariable; forward;

    procedure Set_Property(obj:TRtcValueObject);
      var
        dot:integer;

      procedure CheckCodeBlocks;
        var
          myvar:TRtcFunctionInfo;
          objX:TRtcValueObject;
          vname2,vname3:RtcWideString;
        begin
        if obj is TRtcFunctionInfo then
          begin
          myvar:=TRtcFunctionInfo(obj);
          while (myvar.FunctionName='@') and (myvar.isType['Y']=rtc_Function) do
            myvar:=myvar.asFunction['Y'];

          if (myvar.FunctionName='$') then
            begin
            vname2:=myvar.asVarName['$V'];
            if vname2='-' then
              vname2:=vname2+myvar.asVarName['$P'];
            asAddr:=myvar.asBoolean['$A'];

            objX:=_Execute(myvar.asObject['PARAMS']);
            try
              if assigned(objX) then
                begin
                vname3:=GetAsText(objX);
                if vname3<>'' then
                  begin
                  if vname2<>'' then vname2:=vname2+'.';
                  vname2:=vname2+vname3;
                  end;
                end;
            finally
              RtcFreeAndNil(ObjX);
              end;

            if s<>'' then s:='.'+s;
            s:=vname2+s;
            SetVariable;
            end
          else if (myvar.FunctionName='?') and (myvar.asString['C']='.') then
            begin
            vname2:='';
            repeat
              objX:=_Execute(myvar.asObject['Y']);
              try
                vname3:=GetAsText(objX);
                if vname3<>'' then
                  begin
                  if vname2<>'' then vname2:='.'+vname2;
                  vname2:=vname3+vname2;
                  end;
              finally
                RtcFreeAndNil(objX);
                end;
              if myvar.isType['X']<>rtc_Function then
                raise ERtcScript.Create('Left side can not be assigned to');
              myvar:=TRtcFunctionInfo(myvar.asObject['X']);
              until (myvar.FunctionName<>'?') or (myvar.asString['C']<>'.');

            if myvar.FunctionName<>'$' then
              raise ERtcScript.Create('Left side can not be assigned to');

            objX:=_Execute(myvar.asObject['PARAMS']);
            try
              if assigned(objX) then
                begin
                vname3:=GetAsText(objX);
                if vname3<>'' then
                  begin
                  if vname2<>'' then vname2:='.'+vname2;
                  vname2:=vname3+vname2;
                  end;
                end;
            finally
              RtcFreeAndNil(ObjX);
              end;

            vname3:=myvar.asVarName['$V'];
            if vname3='-' then
              vname3:=vname3+myvar.asVarName['$P'];
            if vname3<>'' then
              begin
              if vname2<>'' then vname2:='.'+vname2;
              vname2:=vname3+vname2;
              end;
            asAddr:=myvar.asBoolean['$A'];

            if s<>'' then s:='.'+s;
            s:=vname2+s;
            SetVariable;
            end
          else
            raise ERtcScript.Create('Left side can not be assigned to');
          end;
        end;

      begin
      CheckCodeBlocks;

      while s<>'' do
        begin
        dot:=Pos('.',s);
        if dot>0 then
          begin
          vname:=Copy(s,1,dot-1);
          Delete(s,1,dot);
          end
        else
          begin
          vname:=s;
          s:='';
          end;

        while assigned(obj) and (obj is TRtcValue) do
          if TRtcValue(obj).asObject=nil then Break
          else obj:=TRtcValue(obj).asObject;

        if obj=nil then
          raise ERtcScript.Create('Can not get ".'+String(vname)+'" from NULL');

        if obj is TRtcArray then
          begin
          if (vname='COUNT') or (vname='FIELDCOUNT') then
            raise Exception.Create('Can not assign to ".'+String(vname)+'"')
          else if (s='') and not asAddr then
            begin
            TRtcArray(obj).isNull[Str2Int(vname)]:=True;
            TRtcArray(obj).asObject[Str2Int(vname)]:=value;
            value:=nil;
            Break;
            end
          else
            obj:=TRtcArray(obj).asObject[Str2Int(vname)];
          end
        else if obj is TRtcRecord then
          begin
          if (vname='COUNT') or (vname='FIELDCOUNT') then
            raise Exception.Create('Can not assign to ".'+String(vname)+'"')
          else if (s='') and not asAddr then
            begin
            TRtcRecord(obj).isNull[vname]:=True;
            TRtcRecord(obj).asObject[vname]:=value;
            value:=nil;
            Break;
            end
          else
            obj:=TRtcRecord(obj).asObject[vname];
          end
        else if obj is TRtcDataSet then
          begin
          if vname='ROW' then
            begin
            if (s='') and not asAddr then
              begin
              TRtcDataSet(obj).Row:=GetAsInteger(value);
              RtcFreeAndNil(value);
              Break;
              end
            else
              raise ERtcScript.Create('Property "ROW" has no sub-properties');
            end
          else if (vname='FIRST') or (vname='LAST') or (vname='NEXT') or (vname='PRIOR') or
             (vname='INSERT') or (vname='APPEND') or (vname='DELETE') or
             (vname='COUNT') or (vname='ROWCOUNT') or (vname='FIELDCOUNT') or
             (vname='EOF') or (vname='EMPTY') or (vname='BOF') then
            raise ERtcScript.Create('Can not assign to ".'+String(vname)+'"')
          else
            case vname[1] of
              '0'..'9':
                  begin
                  if (s='') and not asAddr then
                    begin
                    TRtcDataSet(obj).isNull[TRtcDataSet(obj).FieldName[Str2Int(vname)]]:=True;
                    TRtcDataSet(obj).asObject[TRtcDataSet(obj).FieldName[Str2Int(vname)]]:=value;
                    value:=nil;
                    Break;
                    end
                  else
                    obj:=TRtcDataSet(obj).asObject[TRtcDataSet(obj).FieldName[Str2Int(vname)]];
                  end;
              else
                  begin
                  if (s='') and not asAddr then
                    begin
                    TRtcDataSet(obj).isNull[vname]:=True;
                    TRtcDataSet(obj).asObject[vname]:=value;
                    value:=nil;
                    Break;
                    end
                  else
                    obj:=TRtcDataSet(obj).asObject[vname];
                  end;
              end;
          end
        else
          raise ERtcScript.Create('Can not get ".'+String(vname)+'" from a simple data type');

        CheckCodeBlocks;
        end;
      end;

    procedure Set_HTTPValues(const Vals:TRtcHttpValues);
      var
        idx:integer;
      begin
      if (vname='') or (vname='TEXT') then
        begin
        Vals.Text:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='DELIMITER' then
        begin
        Vals.Delimiter:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='NAME' then
        begin
        GetNext;
        if vname='' then
          raise ERtcScript.Create('Parameter missing after "Item.Name"');
        idx:=Str2Int(vname);
        Vals.ItemName[idx]:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='VALUE' then
        begin
        GetNext;
        if vname='' then
          raise ERtcScript.Create('Parameter missing after "Item.Value"');
        idx:=Str2Int(vname);
        Vals.ItemValue[idx]:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='COUNT' then
        raise ERtcScript.Create('Can not assign to ".Count"')
      else
        begin
        Vals.Value[RtcString(vname)]:=GetAsString(value);
        RtcFreeAndNil(value);
        end;
      end;

    procedure Set_HTTPHeaderVar(const Vars:TRtcHttpHeader);
      var
        idx:integer;
      begin
      if vname='COOKIE' then
        begin
        GetNext;
        if vname='' then
          begin
          Vars.Cookie.Text:=GetAsString(value);
          RtcFreeAndNil(value);
          end
        else
          Set_HTTPValues(Vars.Cookie);
        end
      else if vname='HEADER' then
        begin
        GetNext;
        if vname='NAME' then
          begin
          GetNext;
          if vname='' then raise ERtcScript.Create('Missing item index for "HEADER.NAME"');
          idx:=Str2Int(vname);
          Vars.ItemName[idx]:=GetAsString(value);
          RtcFreeAndNil(value);
          end
        else if vname='VALUE' then
          begin
          GetNext;
          if vname='' then raise ERtcScript.Create('Missing item index for "HEADER.VALUE"');
          idx:=Str2Int(vname);
          Vars.ItemValue[idx]:=GetAsString(value);
          RtcFreeAndNil(value);
          end
        else if (vname='TEXT') or (vname='') then
          begin
          Vars.HeaderText:=GetAsString(value);
          RtcFreeAndNil(value);
          end
        else if vname='COUNT' then
          raise ERtcScript.Create('Can not assign to "Header.Count"')
        else
          begin
          Vars.Value[RtcString(vname)]:=GetAsString(value);
          RtcFreeAndNil(value);
          end;
        end
      else if vname='CONTENTTYPE' then
        begin
        Vars.ContentType:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='CONTENTLENGTH' then
        begin
        Vars.ContentLength:=GetAsInt64(value);
        RtcFreeAndNil(value);
        end
      else if vname<>'' then
        begin
        Vars.Value[RtcString(vname)]:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else
        raise ERtcScript.Create('Missing parameter');
      end;

    procedure Set_SessionVar;
      var
        Srv:TRtcConnection;
      begin
      Srv:=Comm.Sender;
      GetNext;

      if vname='ID' then
        begin
        Srv.FindSession(GetAsString(value));
        RtcFreeAndNil(value);
        end
      else if (vname='OPEN') or (vname='FIND') or
              (vname='HAVE') or (vname='CLOSE') or
              (vname='EXPIRETIME') then
        raise ERtcScript.Create('Can not assign to "SESSION.'+String(vname)+'"')
      else
        begin
        if not assigned(Srv.Session) then
          raise ERtcScript.Create('No session locked, can not set "SESSION.'+String(vname)+'"');
        if vname='KEEPALIVE' then
          begin
          Srv.Session.KeepAlive:=GetAsInteger(value);
          RtcFreeAndNil(value);
          end
        else if vname='FINALEXPIRE' then
          begin
          Srv.Session.FinalExpire:=GetAsDateTime(value);
          RtcFreeAndNil(value);
          end
        else
          begin
          if s<>'' then
            s:=vname+'.'+s
          else
            s:=vname;
          vname:='';
          Set_Property(Srv.Session);
          end;
        end;
      end;

    procedure Set_QueryVar;
      begin
      GetNext;

      Set_HTTPValues( Comm.Sender.Request.Query );
      end;

    procedure Set_InputVar;
      begin
      GetNext;

      Set_HTTPValues( Comm.Sender.Request.Params );
      end;

    procedure Set_RequestVar;
      var
        Req:TRtcRequest;
      begin
      GetNext;

      Req:=Comm.Sender.Request;
      if vname='METHOD' then
        begin
        Req.Method:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='FILENAME' then
        begin
        Req.FileName:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='QUERY' then
        Set_HTTPValues(Req.Query)
      else if vname='PARAMS' then
        Set_HTTPValues(Req.Params)
      else if vname='CLOSE' then
        begin
        Req.Close:=GetAsBoolean(value);
        RtcFreeAndNil(value);
        end
      else if vname='HOST' then
        begin
        Req.Host:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='AGENT' then
        begin
        Req.Agent:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='REFERER' then
        begin
        Req.Referer:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='FORWARDEDFOR' then
        begin
        Req.ForwardedFor:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='URI' then
        begin
        Req.URI:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else if vname='INFO' then
        Set_Property(Req.Info)
      else if vname='URL' then
        raise ERtcScript.Create('Can not assign to "REQUEST.URL"')
      else
        Set_HttpHeaderVar(Req);
      end;

    procedure Set_ResponseVar;
      var
        Resp:TRtcResponse;

      function ExtractNum:integer;
        var
          idx:integer;
        begin
        idx:=0;
        while idx<length(s) do
          case s[idx+1] of
            '0'..'9': Inc(idx);
            else Break;
          end;
        if idx>0 then
          begin
          Result:=Str2Int(Copy(s,1,idx));
          Delete(s,1,idx);
          s:=Trim(s);
          end
        else
          raise ERtcScript.Create('Status code required before Status Text');
        end;

      begin
      GetNext;

      Resp:=Comm.Sender.Response;
      if vname='STATUS' then
        begin
        GetNext;
        if vname='CODE' then
          begin
          Resp.StatusCode:=GetAsInteger(value);
          RtcFreeAndNil(value);
          end
        else if vname='TEXT' then
          begin
          Resp.StatusText:=GetAsString(value);
          RtcFreeAndNil(value);
          end
        else if vname='' then
          begin
          s:=GetAsText(value);
          RtcFreeAndNil(value);
          Resp.StatusCode:=ExtractNum;
          Resp.StatusText:=RtcString(s);
          end;
        end
      else if vname='STATUSCODE' then
        begin
        Resp.StatusCode:=GetAsInteger(value);
        RtcFreeAndNil(value);
        end
      else if vname='STATUSTEXT' then
        begin
        Resp.StatusText:=GetAsString(value);
        RtcFreeAndNil(value);
        end
      else
        Set_HttpHeaderVar(Resp);
      end;

    procedure SetVariable;
      begin
      s:=UpperCase(s);
      GetNext;

      if vname[1]='-' then
        begin
        if not assigned(Comm.Sender) then
          raise ERtcScript.Create('Connection unassigned, can not access "'+String(vname)+'"');

        if vname='-REQUEST' then
          Set_RequestVar
        else if vname='-RESPONSE' then
          Set_ResponseVar
        else if vname='-SESSION' then
          Set_SessionVar
        else if vname='-QUERY' then
          Set_QueryVar
        else if vname='-INPUT' then
          Set_InputVar
        else
          raise ERtcScript.Create('Unknown variabe "'+String(vname)+'"');
        end
      else if s='' then
        begin
        if not asAddr then
          begin
          if vname='RESULT' then
            begin
            Comm.Locals.isNull[vname]:=True;
            Comm.Locals.asObject[vname]:=value;
            value:=nil;
            end
          else if not Comm.Locals.isNull[vname] then
            begin
            Comm.Locals.isNull[vname]:=True;
            Comm.Locals.asObject[vname]:=value;
            value:=nil;
            end
          else if not Comm.Globals.isNull[vname] then
            begin
            Comm.Globals.isNull[vname]:=True;
            Comm.Globals.asObject[vname]:=value;
            value:=nil;
            end
          else
            begin
            Comm.Locals.asObject[vname]:=value;
            value:=nil;
            end;
          end
        else
          begin
          if vname='RESULT' then
            obj:=Comm.Locals.asObject[vname]
          else if not Comm.Locals.isNull[vname] then
            obj:=Comm.Locals.asObject[vname]
          else if not Comm.Globals.isNull[vname] then
            obj:=Comm.Globals.asObject[vname]
          else
            obj:=nil;

          if obj=nil then
            ERtcScript.Create('Left side is NULL. Can not assign to NULL');

          Set_Property(obj);
          end;
        end
      else
        begin
        if vname='RESULT' then
          obj:=Comm.Locals.asObject[vname]
        else if not Comm.Locals.isNull[vname] then
          obj:=Comm.Locals.asObject[vname]
        else if not Comm.Globals.isNull[vname] then
          obj:=Comm.Globals.asObject[vname]
        else
          obj:=nil;

        Set_Property(obj);
        end;
      end;

    begin
    try
      try
        SetVariable;
      except
        on E:Exception do
          begin
          RtcFreeAndNil(value);
          raise;
          end;
        end;
    finally
      if assigned(value) then
        begin
        RtcFreeAndNil(value);
        raise ERtcScript.Create('Error assigning data to variable');
        end;
      end;
    end;

  function Get_Variable(vname:RtcWideString; var isCopy:boolean; asAddr:boolean):TRtcValueObject;
    var
      s:RtcWideString;

    procedure GetNext;
      var
        dot:integer;
      begin
      // Get first RtcWideString before dot
      dot:=Pos('.',s);
      if dot>0 then
        begin
        vname:=Copy(s,1,dot-1);
        Delete(s,1,dot);
        end
      else
        begin
        vname:=s;
        s:='';
        end;
      end;

    function Get_HTTPValues(const Vals:TRtcHttpValues):TRtcValueObject;
      var
        idx:integer;
      begin
      if (vname='') or (vname='TEXT') then
        Result:=TRtcStringValue.Create(Vals.Text)
      else if vname='DELIMITER' then
        Result:=TRtcStringValue.Create(Vals.Delimiter)
      else if vname='COUNT' then
        Result:=TRtcIntegerValue.Create(Vals.ItemCount)
      else if vname='NAME' then
        begin
        GetNext;
        if vname='' then
          raise ERtcScript.Create('Parameter missing after Item.Name');
        idx:=Str2Int(vname);
        Result:=TRtcStringValue.Create(Vals.ItemName[idx]);
        end
      else if vname='VALUE' then
        begin
        GetNext;
        if vname='' then
          raise ERtcScript.Create('Parameter missing after Item.Value');
        idx:=Str2Int(vname);
        Result:=TRtcStringValue.Create(Vals.ItemValue[idx]);
        end
      else
        Result:=TRtcStringValue.Create(Vals.Value[RtcString(vname)]);
      end;

    function Get_HTTPHeaderVar(const Vars:TRtcHttpHeader):TRtcValueObject;
      var
        idx:integer;
      begin
      if vname='COOKIE' then
        begin
        GetNext;
        if vname='' then
          Result:=TRtcStringValue.Create(Vars.Cookie.Text)
        else
          Result:=Get_HTTPValues(Vars.Cookie);
        end
      else if vname='HEADER' then
        begin
        GetNext;
        if vname='COUNT' then
          Result:=TRtcIntegerValue.Create(Vars.ItemCount)
        else if vname='NAME' then
          begin
          GetNext;
          if vname='' then raise ERtcScript.Create('Missing item index for "HEADER.NAME"');
          idx:=Str2Int(vname);
          Result:=TRtcStringValue.Create(Vars.ItemName[idx]);
          end
        else if vname='VALUE' then
          begin
          GetNext;
          if vname='' then raise ERtcScript.Create('Missing item index for "HEADER.VALUE"');
          idx:=Str2Int(vname);
          Result:=TRtcStringValue.Create(Vars.ItemValue[idx]);
          end
        else if (vname='TEXT') or (vname='') then
          Result:=TRtcStringValue.Create(Vars.HeaderText)
        else
          begin
          if Vars.Value[RtcString(vname)]<>'' then
            Result:=TRtcStringValue.Create(Vars.Value[RtcString(vname)])
          else
            Result:=nil;
          end;
        end
      else if vname='CONTENTTYPE' then
        begin
        if Vars.ContentType<>'' then
          Result:=TRtcStringValue.Create(Vars.ContentType)
        else
          Result:=nil;
        end
      else if vname='CONTENTLENGTH' then
        begin
        if Vars.ContentLength<>0 then
          Result:=TRtcIntegerValue.Create(Vars.ContentLength)
        else
          Result:=nil;
        end
      else if vname<>'' then
        begin
        if Vars.Value[RtcString(vname)]<>'' then
          Result:=TRtcStringValue.Create(Vars.Value[RtcString(vname)])
        else
          Result:=nil;
        end
      else
        raise ERtcScript.Create('Missing parameter');
      end;

    function Get_SessionVar:TRtcValueObject;
      var
        Srv:TRtcConnection;
      begin
      Srv:=Comm.Sender;
      GetNext;

      if vname='ID' then
        begin
        if assigned(Srv.Session) then
          Result:=TRtcStringValue.Create(Srv.Session.ID)
        else
          Result:=nil;
        end
      else if vname='COUNT' then
        begin
        GetNext;
        if (vname='') or (vname='TOTAL') then
          Result:=TRtcIntegerValue(Srv.TotalSessionsCount)
        else if vname='LOCKED' then
          Result:=TRtcIntegerValue(Srv.TotalSessionsLocked)
        else if vname='UNLOCKED' then
          Result:=TRtcIntegerValue(Srv.TotalSessionsUnlocked)
        else
          raise ERtcScript.Create('Invalid parameter for SESSION.COUNT ... "'+String(vname)+'"');
        end
      else if vname='OPEN' then
        begin
        GetNext;
        Result:=nil;
        if (vname='') or (vname='PRIVATE') or (vname='FWDLOCK') then
          Srv.OpenSession(sesFwdLock)
        else if (vname='PUBLIC') or (vname='NOLOCK') then
          Srv.OpenSession(sesNoLock)
        else if (vname='IPLOCK') then
          Srv.OpenSession(sesIPLock)
        else if (vname='STRONG') or (vname='SECURE') or (vname='IPWNDLOCK') then
          Srv.OpenSession(sesIPFwdLock)
        else
          raise ERtcScript.Create('Can not Open a Session with parameter "'+String(vname)+'"')
        end
      else if vname='UNLOCK' then
        begin
        Result:=nil;
        Srv.UnLockSession;
        end
      else if vname='FIND' then
        begin
        GetNext;
        if vname='' then
          Result:=TRtcBooleanValue.Create(Srv.Session<>nil)
        else
          Result:=TRtcBooleanValue.Create(Srv.FindSession(RtcString(vname)));
        end
      else if vname='LOCK' then
        begin
        GetNext;
        if vname='' then
          Result:=TRtcBooleanValue.Create(Srv.Session<>nil)
        else
          begin
          if Srv.FindSession(RtcString(vname)) then
            Result:=TRtcBooleanValue.Create(true)
          else
            begin
            repeat
              if Comm.StopTime>0 then
                if GetTickTime64>Comm.StopTime then
                  raise ERtcScript.Create('Script Execution Time Limit exceeded.');

              if not Srv.HaveSession(RtcString(vname)) then Break;
              Sleep(10);
              until Srv.FindSession(RtcString(vname));
            Result:=TRtcBooleanValue.Create(assigned(Srv.Session));
            end;
          end;
        end
      else if vname='HAVE' then
        begin
        GetNext;
        if vname='' then
          Result:=TRtcBooleanValue.Create(Srv.Session<>nil)
        else
          Result:=TRtcBooleanValue.Create(Srv.HaveSession(RtcString(vname)));
        end
      else if vname='CLOSE' then
        begin
        GetNext;
        Result:=nil;
        if vname='' then
          begin
          if assigned(Srv.Session) then
            Srv.Session.Close;
          end
        else
          Srv.CloseSession(RtcString(vname));
        end
      else if vname='KEEPALIVE' then
        begin
        if assigned(Srv.Session) then
          Result:=TRtcIntegerValue.Create(Srv.Session.KeepAlive)
        else
          Result:=nil;
        end
      else if vname='EXPIRETIME' then
        begin
        if assigned(Srv.Session) then
          Result:=TRtcDateTimeValue.Create(Srv.Session.ExpireTime)
        else
          Result:=nil;
        end
      else if vname='FINALEXPIRE' then
        begin
        if assigned(Srv.Session) then
          begin
          if Srv.Session.FinalExpire<>0 then
            Result:=TRtcDateTimeValue.Create(Srv.Session.FinalExpire)
          else
            Result:=nil;
          end
        else
          Result:=nil;
        end
      else if assigned(Srv.Session) then
        begin
        if s<>'' then
          s:=vname+'.'+s
        else
          s:=vname;
        isCopy:=False;
        Result:=Get_Property(Srv.Session,s,isCopy,asAddr);
        s:='';
        end
      else
        Result:=nil;
      end;

    function Get_QueryVar:TRtcValueObject;
      begin
      GetNext;

      Result:= Get_HTTPValues( Comm.Sender.Request.Query );
      end;

    function Get_InputVar:TRtcValueObject;
      begin
      GetNext;

      Result:= Get_HTTPValues( Comm.Sender.Request.Params );
      end;

    function Get_RequestVar:TRtcValueObject;
      var
        Req:TRtcRequest;
      begin
      GetNext;

      Req:=Comm.Sender.Request;
      if vname='METHOD' then
        Result:=TRtcStringValue.Create(Req.Method)
      else if vname='FILENAME' then
        Result:=TRtcStringValue.Create(Req.FileName)
      else if vname='QUERY' then
        begin
        GetNext;
        Result:=Get_HTTPValues(Req.Query);
        end
      else if vname='PARAMS' then
        begin
        GetNext;
        Result:=Get_HTTPValues(Req.Params);
        end
      else if vname='CLOSE' then
        Result:=TRtcBooleanValue.Create(Req.Close)
      else if vname='HOST' then
        begin
        if Req.Host<>'' then
          Result:=TRtcStringValue.Create(Req.Host)
        else
          Result:=nil;
        end
      else if vname='AGENT' then
        begin
        if Req.Agent<>'' then
          Result:=TRtcStringValue.Create(Req.Agent)
        else
          Result:=nil;
        end
      else if vname='REFERER' then
        begin
        if Req.Referer<>'' then
          Result:=TRtcStringValue.Create(Req.Referer)
        else
          Result:=nil;
        end
      else if vname='FORWARDEDFOR' then
        begin
        if Req.ForwardedFor<>'' then
          Result:=TRtcStringValue.Create(Req.ForwardedFor)
        else
          Result:=nil;
        end
      else if vname='URI' then
        Result:=TRtcStringValue.Create(Req.URI)
      else if vname='URL' then
        Result:=TRtcStringValue.Create(Req.URL)
      else if vname='INFO' then
        begin
        isCopy:=False;
        Result:=Get_Property(Req.Info,s,isCopy,asAddr);
        end
      else
        Result:=Get_HttpHeaderVar(Req);
      end;

    function Get_ResponseVar:TRtcValueObject;
      var
        Resp:TRtcResponse;
      begin
      GetNext;

      Resp:=Comm.Sender.Response;
      if vname='STATUS' then
        begin
        GetNext;
        if vname='CODE' then
          Result:=TRtcIntegerValue.Create(Resp.StatusCode)
        else if vname='TEXT' then
          Result:=TRtcStringValue.Create(Resp.StatusText)
        else if vname='' then
          Result:=TRtcStringValue.Create(Int2Str(Resp.StatusCode)+' '+Resp.StatusText)
        else
          raise ERtcScript.Create('Can not read "RESPONSE.STATUS.'+String(vname)+'"');
        end
      else if vname='STATUSCODE' then
        Result:=TRtcIntegerValue.Create(Resp.StatusCode)
      else if vname='STATUSTEXT' then
        Result:=TRtcStringValue.Create(Resp.StatusText)
      else
        Result:=Get_HttpHeaderVar(Resp);
      end;

    function Get_ClientVar:TRtcValueObject;
      var
        Srv:TRtcConnection;
      begin
      GetNext;

      Srv:=Comm.Sender;
      if vname='COUNT' then
        Result:=TRtcIntegerValue.Create(Srv.TotalServerConnectionCount)
      else if (vname='IP') or (vname='ADDR') or (vname='ADDRESS') then
        Result:=TRtcStringValue.Create(Srv.PeerAddr)
      else if vname='PORT' then
        Result:=TRtcStringValue.Create(Srv.PeerPort)
      else if vname='' then
        Result:=TRtcStringValue.Create(Srv.PeerAddr+':'+Srv.PeerPort)
      else
        raise ERtcScript.Create('Unsupported function: "CLIENT.'+String(vname)+'"');
      end;

    function Get_ServerVar:TRtcValueObject;
      var
        Srv:TRtcConnection;
      begin
      GetNext;

      Srv:=Comm.Sender;
      if (vname='IP') or (vname='ADDR') or (vname='ADDRESS') then
        Result:=TRtcStringValue.Create(Srv.LocalAddr)
      else if vname='PORT' then
        Result:=TRtcStringValue.Create(Srv.LocalPort)
      else if vname='' then
        Result:=TRtcStringValue.Create(Srv.LocalAddr+':'+Srv.LocalPort)
      else
        raise ERtcScript.Create('Unsupported function: "SERVER.'+String(vname)+'"');
      end;

    begin
    isCopy:=False;

    s:=UpperCase(vname);
    GetNext;

    if vname[1]='-' then
      begin
      if not assigned(Comm.Sender) then
        raise ERtcScript.Create('No connection object, can not access "'+String(vname)+'"');

      isCopy:=True;
      if vname='-REQUEST' then
        Result:=Get_RequestVar
      else if vname='-RESPONSE' then
        Result:=Get_ResponseVar
      else if vname='-SESSION' then
        Result:=Get_SessionVar
      else if vname='-QUERY' then
        Result:=Get_QueryVar
      else if vname='-INPUT' then
        Result:=Get_InputVar
      else if vname='-CLIENT' then
        Result:=Get_ClientVar
      else if vname='-SERVER' then
        Result:=Get_ServerVar
      else
        raise ERtcScript.Create('Unable to read variable "'+String(vname)+'"');
      end
    else
      begin
      if vname='RESULT' then
        Result:=Comm.Locals.asObject[vname]
      else if not Comm.Locals.isNull[vname] then
        Result:=Comm.Locals.asObject[vname]
      else if not Comm.Globals.isNull[vname] then
        Result:=Comm.Globals.asObject[vname]
      else
        Result:=nil;

      Result:=Get_Property(Result,s,isCopy,asAddr);
      end;
    end;

  procedure Execute_Variable;
    var
      vname:RtcWideString;
      obj:TRtcValueObject;
      isCopy:boolean;
      what:byte;
      asAddr:boolean;

    procedure ExtractPropertyValue;
      var
        obj2,obj3:TRtcValueObject;
        isCopy:boolean;
      begin
      what:=2; // Reading variable property
      isCopy:=False;
      if Param.isType['PARAMS']<>rtc_Null then
        begin
        obj2:=Comm.Group.ExecuteData(Comm, Param.asObject['PARAMS']);
        try
          if obj2<>Param.asObject['PARAMS'] then
            Param.isNull['PARAMS']:=True
          else
            Param.asObject['PARAMS']:=nil;

          obj3:=Get_Property(obj,GetAsText(obj2),isCopy,asAddr);
          if isCopy then
            Res.asObject:=obj3
          else if assigned(obj3) then
            Res.asObject:=obj3.copyOf;
        finally
          RtcFreeAndNil(obj2);
          end;
        end
      else
        Res.asObject:=_Execute(obj);
      end;

    procedure ExtractNamedValue;
      var
        obj2,obj3:TRtcValueObject;
        isCopy:boolean;
      begin
      what:=2; // Reading variable property
      isCopy:=False;
      if Param.isType['PARAMS']<>rtc_Null then
        begin
        obj2:=Comm.Group.ExecuteData(Comm, Param.asObject['PARAMS']);
        try
          if obj2<>Param.asObject['PARAMS'] then
            Param.isNull['PARAMS']:=True
          else
            Param.asObject['PARAMS']:=nil;

          if vname<>'' then
            vname:=vname+'.'+GetAsText(obj2)
          else
            vname:=GetAsText(obj2);
        finally
          RtcFreeAndNil(obj2);
          end;
        end;

      if vname<>'' then
        begin
        obj3:=Get_Variable(vname,isCopy,asAddr);
        if isCopy then
          Res.asObject:=obj3
        else if assigned(obj3) then
          Res.asObject:=obj3.copyOf;
        end;
      end;

    procedure ExecuteRecursiveCall;
      var
        temp:TRtcValueObject;
        tempLocals:TRtcRecord;
        par:TRtcRecord;
        fname:RtcWideString;
        i:integer;
      begin
      what:=3; // Calling script function "vname"

      if Comm.MaxRecurse>0 then
        if Comm.RecurseCount>=Comm.MaxRecurse then
          raise ERtcScript.Create('Maximum allowed Recursion Depth exceeded.')
        else
          Inc(Comm.RecurseCount);

      try
        Par:=TRtcRecord.Create;
        try
          for i:=0 to Param.FieldCount-1 do
            begin
            fname:=Param.FieldName[i];
            if (fname<>'$C') and (fname<>'$R') and (fname<>'$V') then
              begin
              Par.asObject[fname]:=Comm.Group.ExecuteData(Comm, Param.asObject[fname]);
              if Par.asObject[fname]<>Param.asObject[fname] then
                Param.isNull[fname]:=True
              else
                Param.asObject[fname]:=nil;
              end;
            end;

          temp:=TRtcFunctionInfo(obj).asObject['X'];
          if assigned(temp) then
            temp:=temp.copyOf;
          try
            // changing Local variables to Global variables
            // and input parameters to Local variables
            tempLocals:=Comm.Locals;
            Comm.Locals:=Par;
            obj:=nil;
            try
              obj:=Comm.Group.ExecuteData(Comm, temp);
            finally
              Comm.Locals:=tempLocals;
              if assigned(obj) and (obj<>temp) then RtcFreeAndNil(obj);
              end;
          finally
            RtcFreeAndNil(temp);
            end;

          // Get the result
          Res.asObject:=Par.asObject['RESULT'];
          Par.asObject['RESULT']:=nil;
        finally
          RtcFreeAndNil(Par);
          end;
      finally
        if Comm.MaxRecurse>0 then
          Dec(Comm.RecurseCount);
        end;
      end;

    begin
    what:=0; // 'Checking variable';
    try
      vname:=Param.asVarName['$V'];
      asAddr:=Param.asBoolean['$A'];
      if vname='' then
        ExtractNamedValue
      else if vname='-' then // Sender variables
        begin
        vname:=CheckParam('$P').asVarName;
        vname:='-'+vname;
        ExtractNamedValue;
        end
      else
        begin
        what:=1; // 'Reading variable "'+RtcWideString(vname)+'"';
        obj:=Get_Variable(vname,isCopy,asAddr);
        if assigned(obj) and not isCopy then
          begin
          if obj is TRtcFunctionInfo then
            begin
            if TRtcFunctionInfo(obj).FunctionName='!!' then
              begin
              if assigned(TRtcFunctionInfo(obj).asObject['X']) then
                ExecuteRecursiveCall;
              end
            else
              Res.asObject:=_Execute(obj);
            end
          else if not isSimpleValue(obj) then
            ExtractPropertyValue
          else
            Res.asObject:=obj.copyOf;
          end
        else
          Res.asObject:=obj;
        end;
    except
      on E:Exception do
        begin
        if E is ERtcForward then
          func:='['+Param.asText['$R']+':'+Param.asText['$C']+'] '
        else
          func:='['+Param.asText['$R']+':'+Param.asText['$C']+'] '+RtcWideString(E.ClassName)+' ';

        case what of
          0:func:=func+'Variable Access';
          1:func:=func+'Reading Variable "$'+vname+'"';
          2:func:=func+'Reading Variable "$'+vname+'()"';
          3:func:=func+'Calling Function "$'+vname+'"';
          end;

        if Pos(RtcWideString(func),E.Message)>0 then
          begin
          if Copy(E.Message,1,1)='>' then
            raise ERtcForward.Create(String('>['+Param.asText['$R']+':'+Param.asText['$C']+'] '+RtcWideString(E.Message)))
          else
            raise ERtcForward.Create(String('>['+Param.asText['$R']+':'+Param.asText['$C']+']'+#13#10+RtcWideString(E.Message)));
          end
        else
          raise ERtcForward.Create(String(func)+#13#10+E.Message);
        end;
      end;
    end;

  procedure Execute_Property;
    var
      vname,vname2:RtcWideString;
      what:byte;
    begin
    what:=0; // 'Checking property';
    try
      vname:=Param.asVarName['$V'];
      what:=1; // 'Reading property "'+RtcWideString(vname)+'"';
      vname2:=CheckParam('X').asText;
      if vname2<>'' then
        begin
        if vname<>'' then vname:=vname+'.';
        vname:=vname+vname2;
        end;
      Res.asText:=vname;
    except
      on E:Exception do
        begin
        if E is ERtcForward then
          func:='['+Param.asText['$R']+':'+Param.asText['$C']+'] '
        else
          func:='['+Param.asText['$R']+':'+Param.asText['$C']+'] '+RtcWideString(E.ClassName)+' ';

        case what of
          0:func:=func+'Property Access';
          1:func:=func+'Reading Property ".'+vname+'"';
          end;

        if Pos(RtcWideString(func),E.Message)>0 then
          begin
          if Copy(E.Message,1,1)='>' then
            raise ERtcForward.Create(String('>['+Param.asText['$R']+':'+Param.asText['$C']+'] '+RtcWideString(E.Message)))
          else
            raise ERtcForward.Create(String('>['+Param.asText['$R']+':'+Param.asText['$C']+']'+#13#10+RtcWideString(E.Message)));
          end
        else
          raise ERtcForward.Create(String(func)+#13#10+E.Message);
        end;
      end;
    end;

  procedure CollapseArray(const obj:TRtcValue; makeNull:boolean=True);
    var
      i,cnt,x:integer;
      arr:TRtcArray;
    begin
    if obj.isType=rtc_Array then
      begin
      arr:=obj.asArray;
      cnt:=0; x:=0;
      for i:=0 to arr.Count-1 do
        if arr.isType[i]<>rtc_Null then
          begin
          x:=i;
          Inc(cnt);
          if cnt>1 then Break;
          end;
      if makeNull and (cnt=0) then
        obj.isNull:=True
      else if cnt=1 then
        begin
        obj.asObject:=nil;
        obj.asObject:=arr.asObject[x];
        arr.asObject[x]:=nil;
        RtcFreeAndNil(arr);
        end;
      end;
    end;

  procedure Extract_X;
    begin
    ExecuteParam_X;
    if FAutoCollapseArrays then
      CollapseArray(obj);
    end;
  procedure Extract_Y;
    begin
    ExecuteParamB_Y;
    if FAutoCollapseArrays then
      CollapseArray(objB);
    end;
  procedure Extract_Both;
    begin
    ExecuteParam_X;
    if FAutoCollapseArrays then
      CollapseArray(obj);
    ExecuteParamB_Y;
    if FAutoCollapseArrays then
      CollapseArray(objB);
    end;
    
  procedure ClearBoth;
    begin
    obj.isNull:=True;
    objB.isNull:=True;
    end;
  function SetOneNull:boolean;
    begin
    if obj.isType=rtc_Null then
      begin
      Res.asObject := objB.asObject;
      objB.asObject := nil;
      Result:=True;
      end
    else if objB.isType=rtc_Null then
      begin
      Res.asObject := obj.asObject;
      obj.asObject := nil;
      Result:=True;
      end
    else
      Result:=False;
    end;

  procedure ResSetB;
    begin
    Res.asObject:=objB.asObject;
    objB.asObject:=nil;
    end;

  procedure ResSet;
    begin
    Res.asObject:=obj.asObject;
    obj.asObject:=nil;
    end;

  procedure Do_Set;
    begin
    ExecuteParamB_Y;
    if FAutoCollapseArrays then
      CollapseArray(objB,False);
    ResSetB;
    end;

  procedure Do_Property;
    var
      isCopy:boolean;
      ObjX:TRtcValueObject;
      myvar3:TRtcValueObject;
      myvar2:TRtcFunctionInfo;
      vname,vname2:RtcWideString;
      asAddr:boolean;
    begin
    myvar3:=nil;
    myvar2:=Param;
    isCopy:=False;
    vname:='';
    repeat
      objX:=_Execute(myvar2.asObject['Y']);
      try
        vname2:=GetAsText(objX);
        if vname2<>'' then
          if vname<>'' then
            vname:=vname2+'.'+vname
          else
            vname:=vname2;
      finally
        RtcFreeAndNil(objX);
        end;
      if myvar2.isType['X']=rtc_Function then
        begin
        if (myvar2.asFunction['X'].FunctionName='$') then
          begin
          myvar2:=TRtcFunctionInfo(myvar2.asObject['X']);
          Break;
          end
        else if (myvar2.asFunction['X'].FunctionName='?') and (myvar2.asFunction['X'].asString['C']='.') then
          myvar2:=TRtcFunctionInfo(myvar2.asObject['X'])
        else
          begin
          myvar3:=TRtcFunctionInfo(myvar2.asObject['X']);
          myvar2:=nil;
          Break;
          end;
        end
      else
        begin
        myvar3:=TRtcFunctionInfo(myvar2.asObject['X']);
        myvar2:=nil;
        Break;
        end;
      until not assigned(myvar2);

    if assigned(myvar3) then
      begin // static data (not a variable)
      objX:=_Execute(myvar3);
      try
        if assigned(objX) then
          begin
          isCopy:=False;
          Res.asObject:=Get_Property(objX,vname,isCopy,False);
          if not isCopy then objX:=nil;
          end;
      finally
        RtcFreeAndNil(objX);
        end;
      end
    else
      begin
      objX:=_Execute(myvar2.asObject['PARAMS']);
      try
        if assigned(objX) then
          begin
          vname2:=GetAsText(objX);
          if vname2<>'' then
            begin
            if vname<>'' then vname:='.'+vname;
            vname:=vname2+vname;
            end;
          end;
      finally
        RtcFreeAndNil(ObjX);
        end;

      vname2:=myvar2.asVarName['$V'];
      if vname2='-' then
        vname2:=vname2+myvar2.asVarName['$P'];
      if vname2<>'' then
        begin
        if vname<>'' then vname:='.'+vname;
        vname:=vname2+vname;
        end;
      asAddr:=myvar2.asBoolean['$A'];

      isCopy:=False;
      ObjX:=Get_Variable(vname,isCopy,asAddr);
      if isCopy then
        Res.asObject:=ObjX
      else if assigned(ObjX) then
        Res.asObject:=ObjX.copyOf;
      end;
    end;

  procedure Do_Plus;
    begin
    Extract_Both;
    if not SetOneNull then
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asText := obj.asText + objB.asText;
        rtc_String: Res.asString := obj.asString + objB.asString;
        rtc_WideString: Res.asWideString := obj.asWideString + objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean or objB.asBoolean;
        rtc_Integer: Res.asInteger := obj.asInteger + objB.asInteger;
        rtc_LargeInt: Res.asLargeInt := obj.asLargeInt + objB.asLargeInt;
        rtc_Float: Res.asFloat := obj.asFloat + objB.asFloat;
        rtc_Currency: Res.asCurrency := obj.asCurrency + objB.asCurrency;
        rtc_DateTime: Res.asDateTime := obj.asDateTime + objB.asDateTime;
        rtc_Variant: Res.asValue := obj.asValue + objB.asValue;
        {rtc_Array:
        rtc_ByteStream:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not add '+String(rtcTypeName(obj.isType))+' to '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_Minus;
    begin
    Extract_Both;
    if obj.isNull then
      begin
      SetOneNull;
      ctype:=Res.isType;
      if ctype<>rtc_Null then
        begin
        case ctype of
          rtc_Text: Res.asText := '-'+Res.asText;
          rtc_String: Res.asString := '-'+Res.asString;
          rtc_WideString: Res.asWideString := '-'+Res.asWideString;
          rtc_Boolean: Res.asBoolean := not Res.asBoolean;
          rtc_Integer: Res.asInteger := - Res.asInteger;
          rtc_LargeInt: Res.asLargeInt := - Res.asLargeInt;
          rtc_Float: Res.asFloat := - Res.asFloat;
          rtc_Currency: Res.asCurrency := - Res.asCurrency;
          rtc_DateTime: Res.asDateTime := - Res.asDateTime;
          rtc_Variant: Res.asValue := - Res.asValue;
          {rtc_ByteStream:
          rtc_Array:
          rtc_Record:
          rtc_DataSet:}
          else
            raise ERtcScript.Create('Can not negate '+String(rtcTypeName(Res.isType)));
          end;
        end;
      end
    else if not SetOneNull then
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asText := obj.asText+'-'+objB.asText;
        rtc_String: Res.asString := obj.asString+'-'+objB.asString;
        rtc_WideString: Res.asWideString := obj.asWideString+'-'+objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean xor objB.asBoolean;
        rtc_Integer: Res.asInteger := obj.asInteger - objB.asInteger;
        rtc_LargeInt: Res.asLargeInt := obj.asLargeInt - objB.asLargeInt;
        rtc_Float: Res.asFloat := obj.asFloat - objB.asFloat;
        rtc_Currency: Res.asCurrency := obj.asCurrency - objB.asCurrency;
        rtc_DateTime: Res.asDateTime := obj.asDateTime - objB.asDateTime;
        rtc_Variant: Res.asValue := obj.asValue - objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not subtract '+String(rtcTypeName(objB.isType))+' from '+String(rtcTypeName(obj.isType)));
        end;
      end;
    end;

  procedure Do_Shl;
    begin
    Extract_Both;
    if obj.isNull then
      // NULL << X = NULL
      ClearBoth
    else if objB.isNull then
      // X << NULL = X
      SetOneNull
    else if not SetOneNull then
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asText := obj.asText+'<<'+objB.asText;
        rtc_String: Res.asString := obj.asString+'<<'+objB.asString;
        rtc_WideString: Res.asWideString := obj.asWideString+'<<'+objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean xor objB.asBoolean;
        rtc_Integer: Res.asInteger := obj.asInteger shl objB.asInteger;
        rtc_LargeInt: Res.asLargeInt := obj.asLargeInt shl objB.asLargeInt;
        rtc_Float: Res.asFloat := round(obj.asFloat) shl round(objB.asFloat);
        rtc_Currency: Res.asCurrency := round(obj.asCurrency) shl round(objB.asCurrency);
        rtc_DateTime: Res.asDateTime := round(obj.asDateTime) shl round(objB.asDateTime);
        rtc_Variant: Res.asValue := obj.asValue shl objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not execute '+String(rtcTypeName(objB.isType))+' SHL '+String(rtcTypeName(obj.isType)));
        end;
      end;
    end;

  procedure Do_Shr;
    begin
    Extract_Both;
    if obj.isNull then
      // NULL >> X = NULL
      ClearBoth
    else if objB.isNull then
      // X >> NULL = X
      SetOneNull
    else if not SetOneNull then
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asText := obj.asText+'>>'+objB.asText;
        rtc_String: Res.asString := obj.asString+'>>'+objB.asString;
        rtc_WideString: Res.asWideString := obj.asWideString+'>>'+objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean xor objB.asBoolean;
        rtc_Integer: Res.asInteger := obj.asInteger shr objB.asInteger;
        rtc_LargeInt: Res.asLargeInt := obj.asLargeInt shr objB.asLargeInt;
        rtc_Float: Res.asFloat := round(obj.asFloat) shr round(objB.asFloat);
        rtc_Currency: Res.asCurrency := round(obj.asCurrency) shr round(objB.asCurrency);
        rtc_DateTime: Res.asDateTime := round(obj.asDateTime) shr round(objB.asDateTime);
        rtc_Variant: Res.asValue := obj.asValue shr objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not execute '+String(rtcTypeName(objB.isType))+' SHR '+String(rtcTypeName(obj.isType)));
        end;
      end;
    end;

  procedure Do_Multiply;
    begin
    Extract_Both;
    if SetOneNull then
      begin
      // A * NULL = NULL
      // NULL * A = NULL
      Res.isNull:=True;
      end
    else
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asText := obj.asText+'*'+objB.asText;
        rtc_String: Res.asString := obj.asString+'*'+objB.asString;
        rtc_WideString: Res.asWideString := obj.asWideString+'*'+objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean and objB.asBoolean;
        rtc_Integer: Res.asInteger := obj.asInteger * objB.asInteger;
        rtc_LargeInt: Res.asLargeInt := obj.asLargeInt * objB.asLargeInt;
        rtc_Float: Res.asFloat := obj.asFloat * objB.asFloat;
        rtc_Currency: Res.asCurrency := obj.asCurrency * objB.asCurrency;
        rtc_DateTime: Res.asDateTime := obj.asDateTime * objB.asDateTime;
        rtc_Variant: Res.asValue := obj.asValue * objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not multiply '+String(rtcTypeName(obj.isType))+' and '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_Divide;
    begin
    Extract_Both;
    if not SetOneNull then
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asText := obj.asText+'/'+objB.asText;
        rtc_String: Res.asString := obj.asString+'/'+objB.asString;
        rtc_WideString: Res.asWideString := obj.asWideString+'/'+objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean xor objB.asBoolean;
        rtc_Integer: Res.asInteger := obj.asInteger div objB.asInteger;
        rtc_LargeInt: Res.asLargeInt := obj.asLargeInt div objB.asLargeInt;
        rtc_Float: Res.asFloat := obj.asFloat / objB.asFloat;
        rtc_Currency: Res.asCurrency := obj.asCurrency / objB.asCurrency;
        rtc_DateTime: Res.asDateTime := obj.asDateTime / objB.asDateTime;
        rtc_Variant: Res.asValue := obj.asValue / objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not divide '+String(rtcTypeName(obj.isType))+' by '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_Modulo;
    begin
    Extract_Both;
    if not SetOneNull then
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asText := obj.asText+'%'+objB.asText;
        rtc_String: Res.asString := obj.asString+'%'+objB.asString;
        rtc_WideString: Res.asWideString := obj.asWideString+'%'+objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean xor objB.asBoolean;
        rtc_Integer: Res.asInteger := obj.asInteger mod objB.asInteger;
        rtc_LargeInt: Res.asLargeInt := obj.asLargeInt mod objB.asLargeInt;
        rtc_Float: Res.asFloat := frac(obj.asFloat / objB.asFloat)*objB.asFloat;
        rtc_Currency: Res.asCurrency := frac(obj.asCurrency / objB.asCurrency)*objB.asCurrency;
        rtc_DateTime: Res.asDateTime := frac(obj.asDateTime / objB.asDateTime)*objB.asDateTime;
        rtc_Variant: Res.asValue := frac(obj.asValue / objB.asValue)*objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not execute '+String(rtcTypeName(obj.isType))+' MOD '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_And;
    begin
    Extract_Both;
    if SetOneNull then
      begin
      // A & NULL = NULL
      // NULL & A = NULL
      Res.isNull:=True;
      end
    else
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asText := obj.asText+objB.asText;
        rtc_String: Res.asString := obj.asString+objB.asString;
        rtc_WideString: Res.asWideString := obj.asWideString+objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean and objB.asBoolean;
        rtc_Integer: Res.asInteger := obj.asInteger and objB.asInteger;
        rtc_LargeInt: Res.asLargeInt := obj.asLargeInt and objB.asLargeInt;
        rtc_Float: Res.asFloat := round(obj.asFloat) and round(objB.asFloat);
        rtc_Currency: Res.asCurrency := round(obj.asCurrency) and round(objB.asCurrency);
        rtc_DateTime: Res.asDateTime := round(obj.asDateTime) and round(objB.asDateTime);
        rtc_Variant: Res.asValue := obj.asValue and objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not execute '+String(rtcTypeName(obj.isType))+' AND '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_Or;
    begin
    Extract_Both;
    if not SetOneNull then
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asText := obj.asText+objB.asText;
        rtc_String: Res.asString := obj.asString+objB.asString;
        rtc_WideString: Res.asWideString := obj.asWideString+objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean or objB.asBoolean;
        rtc_Integer: Res.asInteger := obj.asInteger or objB.asInteger;
        rtc_LargeInt: Res.asLargeInt := obj.asLargeInt or objB.asLargeInt;
        rtc_Float: Res.asFloat := round(obj.asFloat) or round(objB.asFloat);
        rtc_Currency: Res.asCurrency := round(obj.asCurrency) or round(objB.asCurrency);
        rtc_DateTime: Res.asDateTime := round(obj.asDateTime) or round(objB.asDateTime);
        rtc_Variant: Res.asValue := obj.asValue or objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not execute '+String(rtcTypeName(obj.isType))+' OR '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_Max;
    begin
    Extract_Both;
    if not (obj.isNull or objB.isNull) then
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: if (obj.asText<=objB.asText) then ResSet;
        rtc_String: if (obj.asString<=objB.asString) then ResSet;
        rtc_WideString: if (obj.asWideString<=objB.asWideString) then ResSet;
        rtc_Boolean: if (obj.asBoolean<=objB.asBoolean) then ResSet;
        rtc_Integer: if (obj.asInteger<=objB.asInteger) then ResSet;
        rtc_LargeInt: if (obj.asLargeInt<=objB.asLargeInt) then ResSet;
        rtc_Float: if (obj.asFloat<=objB.asFloat) then ResSet;
        rtc_Currency: if (obj.asCurrency<=objB.asCurrency) then ResSet;
        rtc_DateTime: if (obj.asDateTime<=objB.asDateTime) then ResSet;
        rtc_Variant: if (obj.asValue<=objB.asValue) then ResSet;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not compare '+String(rtcTypeName(obj.isType))+' and '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_Min;
    begin
    Extract_Both;
    if not (obj.isNull or objB.isNull) then
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: if (obj.asText>=objB.asText) then ResSet;
        rtc_String: if (obj.asString>=objB.asString) then ResSet;
        rtc_WideString: if (obj.asWideString>=objB.asWideString) then ResSet;
        rtc_Boolean: if (obj.asBoolean>=objB.asBoolean) then ResSet;
        rtc_Integer: if (obj.asInteger>=objB.asInteger) then ResSet;
        rtc_LargeInt: if (obj.asLargeInt>=objB.asLargeInt) then ResSet;
        rtc_Float: if (obj.asFloat>=objB.asFloat) then ResSet;
        rtc_Currency: if (obj.asCurrency>=objB.asCurrency) then ResSet;
        rtc_DateTime: if (obj.asDateTime>=objB.asDateTime) then ResSet;
        rtc_Variant: if (obj.asValue>=objB.asValue) then ResSet;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not compare '+String(rtcTypeName(obj.isType))+' and '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_Xor;
    begin
    Extract_Both;
    if not SetOneNull then
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Boolean: Res.asBoolean := obj.asBoolean xor objB.asBoolean;
        rtc_Integer: Res.asInteger := obj.asInteger xor objB.asInteger;
        rtc_LargeInt: Res.asLargeInt := obj.asLargeInt xor objB.asLargeInt;
        rtc_Float: Res.asFloat := round(obj.asFloat) xor round(objB.asFloat);
        rtc_Currency: Res.asCurrency := round(obj.asCurrency) xor round(objB.asCurrency);
        rtc_DateTime: Res.asDateTime := round(obj.asDateTime) xor round(objB.asDateTime);
        rtc_Variant: Res.asValue := obj.asValue xor objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not execute '+String(rtcTypeName(obj.isType))+' XOR '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_Not;
    begin
    Extract_Y;
    SetOneNull;
    case Res.isType of
      rtc_Text: Res.asText := '!'+Res.asText;
      rtc_String: Res.asString := '!'+Res.asString;
      rtc_WideString: Res.asWideString := '!'+Res.asWideString;
      rtc_Boolean: Res.asBoolean := not Res.asBoolean;
      rtc_Integer: Res.asInteger := not Res.asInteger;
      rtc_LargeInt: Res.asLargeInt := not Res.asLargeInt;
      rtc_Float: Res.asFloat := not round(Res.asFloat);
      rtc_Currency: Res.asCurrency := not round(Res.asCurrency);
      rtc_DateTime: Res.asDateTime := not round(Res.asDateTime);
      rtc_Variant: Res.asValue := not Res.asValue;
      {rtc_ByteStream:
      rtc_Array:
      rtc_Record:
      rtc_DataSet:}
      else
        raise ERtcScript.Create('Can not negate '+String(rtcTypeName(Res.isType)));
      end;
    end;

  procedure Do_Chr;
    var
      ch:integer;
    begin
    Extract_Y;
    SetOneNull;
    case Res.isType of
      rtc_Text: ch := Str2Int(Res.asText);
      rtc_String: ch := Str2Int(Res.asString);
      rtc_WideString: ch := Str2Int(Res.asWideString);
      rtc_Boolean: if Res.asBoolean then ch := 0 else ch := 1;
      rtc_Integer: ch := Res.asInteger;
      rtc_LargeInt: ch := Res.asLargeInt;
      rtc_Float: ch := round(Res.asFloat);
      rtc_Currency: ch := round(Res.asCurrency);
      rtc_DateTime: ch := round(Res.asDateTime);
      rtc_Variant: ch := {$IFDEF FPC}round(Res.asInteger){$ELSE}round(Res.asValue){$ENDIF};
      rtc_Array: ch := Str2Int(Res.asString);
      {rtc_ByteStream:
      rtc_Record:
      rtc_DataSet:}
      else
        raise ERtcScript.Create('Can not get Integer from '+String(rtcTypeName(Res.isType)));
      end;
    Extract_X;
    Res.isNull:=True;
    if assigned(obj) then
      Res.asText:=obj.asText+RtcWideChar(ch)
    else
      Res.asText:=RtcWideChar(ch);
    end;

  procedure Do_Greater;
    begin
    Extract_Both;
    if objB.isNull then
      begin
      // NULL > NULL = FALSE
      // X > NULL = TRUE
      Res.asBoolean:= not obj.isNull;
      ClearBoth;
      end
    else if obj.isNull then
      begin
      // NULL > X = FALSE
      Res.asBoolean:=False;
      ClearBoth;
      end
    else
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asBoolean := obj.asText > objB.asText;
        rtc_String: Res.asBoolean := obj.asString > objB.asString;
        rtc_WideString: Res.asBoolean := obj.asWideString > objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean > objB.asBoolean;
        rtc_Integer: Res.asBoolean := obj.asInteger > objB.asInteger;
        rtc_LargeInt: Res.asBoolean := obj.asLargeInt > objB.asLargeInt;
        rtc_Float: Res.asBoolean := obj.asFloat > objB.asFloat;
        rtc_Currency: Res.asBoolean := obj.asCurrency > objB.asCurrency;
        rtc_DateTime: Res.asBoolean := obj.asDateTime > objB.asDateTime;
        rtc_Variant: Res.asBoolean := obj.asValue > objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not compare '+String(rtcTypeName(obj.isType))+' and '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_GreaterEqual;
    begin
    Extract_Both;
    if objB.isNull then
      begin
      // NULL >= NULL = TRUE
      // X >= NULL = TRUE
      Res.asBoolean:= True;
      ClearBoth;
      end
    else if obj.isNull then
      begin
      // NULL >= X = FALSE
      Res.asBoolean:=False;
      ClearBoth;
      end
    else
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asBoolean := obj.asText >= objB.asText;
        rtc_String: Res.asBoolean := obj.asString >= objB.asString;
        rtc_WideString: Res.asBoolean := obj.asWideString >= objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean >= objB.asBoolean;
        rtc_Integer: Res.asBoolean := obj.asInteger >= objB.asInteger;
        rtc_LargeInt: Res.asBoolean := obj.asLargeInt >= objB.asLargeInt;
        rtc_Float: Res.asBoolean := obj.asFloat >= objB.asFloat;
        rtc_Currency: Res.asBoolean := obj.asCurrency >= objB.asCurrency;
        rtc_DateTime: Res.asBoolean := obj.asDateTime >= objB.asDateTime;
        rtc_Variant: Res.asBoolean := obj.asValue >= objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not compare '+String(rtcTypeName(obj.isType))+' and '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_Lower;
    begin
    Extract_Both;
    if obj.isNull then
      begin
      // NULL < NULL = FALSE
      // NULL < X = TRUE
      Res.asBoolean:= not objB.isNull;
      ClearBoth;
      end
    else if objB.isNull then
      begin
      // X < NULL = FALSE
      Res.asBoolean:=False;
      ClearBoth;
      end
    else
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asBoolean := obj.asText < objB.asText;
        rtc_String: Res.asBoolean := obj.asString < objB.asString;
        rtc_WideString: Res.asBoolean := obj.asWideString < objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean < objB.asBoolean;
        rtc_Integer: Res.asBoolean := obj.asInteger < objB.asInteger;
        rtc_LargeInt: Res.asBoolean := obj.asLargeInt < objB.asLargeInt;
        rtc_Float: Res.asBoolean := obj.asFloat < objB.asFloat;
        rtc_Currency: Res.asBoolean := obj.asCurrency < objB.asCurrency;
        rtc_DateTime: Res.asBoolean := obj.asDateTime < objB.asDateTime;
        rtc_Variant: Res.asBoolean := obj.asValue < objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not compare '+String(rtcTypeName(obj.isType))+' and '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_LowerEqual;
    begin
    Extract_Both;
    if obj.isNull then
      begin
      // NULL <= NULL = TRUE
      // NULL <= X = TRUE
      Res.asBoolean:= True;
      ClearBoth;
      end
    else if objB.isNull then
      begin
      // X <= NULL = FALSE
      Res.asBoolean:=False;
      ClearBoth;
      end
    else
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asBoolean := obj.asText <= objB.asText;
        rtc_String: Res.asBoolean := obj.asString <= objB.asString;
        rtc_WideString: Res.asBoolean := obj.asWideString <= objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean <= objB.asBoolean;
        rtc_Integer: Res.asBoolean := obj.asInteger <= objB.asInteger;
        rtc_LargeInt: Res.asBoolean := obj.asLargeInt <= objB.asLargeInt;
        rtc_Float: Res.asBoolean := obj.asFloat <= objB.asFloat;
        rtc_Currency: Res.asBoolean := obj.asCurrency <= objB.asCurrency;
        rtc_DateTime: Res.asBoolean := obj.asDateTime <= objB.asDateTime;
        rtc_Variant: Res.asBoolean := obj.asValue <= objB.asValue;
        {rtc_ByteStream:
        rtc_Array:
        rtc_Record:
        rtc_DataSet:}
        else
          raise ERtcScript.Create('Can not compare '+String(rtcTypeName(obj.isType))+' and '+String(rtcTypeName(objB.isType)));
        end;
      end;
    end;

  procedure Do_Equal;
    begin
    Extract_Both;
    if obj.isNull then
      begin
      // NULL = NULL = TRUE
      // NULL = X = FALSE
      Res.asBoolean:= objB.isNull;
      ClearBoth;
      end
    else if objB.isNull then
      begin
      // X = NULL = FALSE
      Res.asBoolean:=False;
      ClearBoth;
      end
    else
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asBoolean := obj.asText = objB.asText;
        rtc_String: Res.asBoolean := obj.asString = objB.asString;
        rtc_WideString: Res.asBoolean := obj.asWideString = objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean = objB.asBoolean;
        rtc_Integer: Res.asBoolean := obj.asInteger = objB.asInteger;
        rtc_LargeInt: Res.asBoolean := obj.asLargeInt = objB.asLargeInt;
        rtc_Float: Res.asBoolean := obj.asFloat = objB.asFloat;
        rtc_Currency: Res.asBoolean := obj.asCurrency = objB.asCurrency;
        rtc_DateTime: Res.asBoolean := obj.asDateTime = objB.asDateTime;
        rtc_Variant: Res.asBoolean := obj.asValue = objB.asValue;
        else Res.asBoolean := obj.toCode = objB.toCode;
        end;
      end;
    end;

  procedure Do_NotEqual;
    begin
    Extract_Both;
    if obj.isNull then
      begin
      // NULL != NULL = FALSE
      // NULL != X = TRUE
      Res.asBoolean:= not objB.isNull;
      ClearBoth;
      end
    else if objB.isNull then
      begin
      // X != NULL = TRUE
      Res.asBoolean:=True;
      ClearBoth;
      end
    else
      begin
      ctype:=rtcCombineTypes(obj.isType, objB.isType);
      case ctype of
        rtc_Text: Res.asBoolean := obj.asText <> objB.asText;
        rtc_String: Res.asBoolean := obj.asString <> objB.asString;
        rtc_WideString: Res.asBoolean := obj.asWideString <> objB.asWideString;
        rtc_Boolean: Res.asBoolean := obj.asBoolean <> objB.asBoolean;
        rtc_Integer: Res.asBoolean := obj.asInteger <> objB.asInteger;
        rtc_LargeInt: Res.asBoolean := obj.asLargeInt <> objB.asLargeInt;
        rtc_Float: Res.asBoolean := obj.asFloat <> objB.asFloat;
        rtc_Currency: Res.asBoolean := obj.asCurrency <> objB.asCurrency;
        rtc_DateTime: Res.asBoolean := obj.asDateTime <> objB.asDateTime;
        rtc_Variant: Res.asBoolean := obj.asValue <> objB.asValue;
        else Res.asBoolean := obj.toCode <> objB.toCode;
        end;
      end;
    end;

  procedure Do_New;
    var
      typ:RtcString;
    begin
    typ:=UpperCase(Param.asString['X']);
    if typ='ARRAY' then
      Res.NewArray
    else if typ='RECORD' then
      Res.NewRecord
    else if typ='DATASET' then
      Res.NewDataSet
    else if (typ='STREAM') or (typ='BYTESTREAM') then
      Res.NewByteStream
    else if (typ='INTEGER') or (typ='INT') or (typ='INT32') then
      Res.NewInteger
    else if (typ='LARGEINT') or (typ='INT64') then
      Res.NewLargeInt
    else if (typ='DATETIME') or (typ='DATE') or (typ='TIME') then
      Res.NewDateTime
    else if (typ='FLOAT') or (typ='DOUBLE') then
      Res.NewFloat
    else if (typ='CURRENCY') then
      Res.NewCurrency
    else if (typ='BOOLEAN') or (typ='BOOL') then
      Res.NewBoolean
    else if (typ='WIDESTRING') then
      Res.NewWideString
    else if (typ='TEXT') then
      Res.NewText
    else
      raise ERtcScript.Create('Type not supported for NEW');
    end;

  procedure Execute_Function;
    begin
    try
      case length(func) of
        1:case func[1] of
            '#':Do_Chr;
            '+':Do_Plus;
            '-':Do_Minus;
            '*':Do_Multiply;
            '/','\':Do_Divide;
            '%':Do_Modulo;
            '&':Do_And;
            '|':Do_Or;
            '!':Do_Not;
            '>':Do_Greater;
            '<':Do_Lower;
            '=':Do_Equal;
            '.':Do_Property;
            '^':Do_Xor;
            else
              raise ERtcScript.Create('Function "'+String(func)+'" not found');
            end;
        2:case func[2] of
            '=':case func[1] of
                  '=':Do_Equal;
                  '!':Do_NotEqual;
                  '>':Do_GreaterEqual;
                  '<':Do_LowerEqual;
                  else raise ERtcScript.Create('Function "'+String(func)+'" not found');
                  end;
            '>':case func[1] of
                  ':':Do_Min;
                  '>':Do_Shr;
                  '<':Do_NotEqual;
                  else raise ERtcScript.Create('Function "'+String(func)+'" not found');
                  end;
            '<':case func[1] of
                  ':':Do_Max;
                  '<':Do_Shl;
                  else raise ERtcScript.Create('Function "'+String(func)+'" not found');
                  end;
            else if func='&&' then Do_And
            else if (func='OR') or (func='||') then Do_Or
            else raise ERtcScript.Create('Function "'+String(func)+'" not found');
            end;
        3:case func[1] of
            'A': if func='AND' then Do_And
                 else raise ERtcScript.Create('Function "'+String(func)+'" not found');
            'X': if func='XOR' then Do_Xor
                 else raise ERtcScript.Create('Function "'+String(func)+'" not found');
            'S': if func='SHL' then Do_Shl
                 else if func='SHR' then Do_Shr
                 else raise ERtcScript.Create('Function "'+String(func)+'" not found');
            'M': if func='MOD' then Do_Modulo
                 else raise ERtcScript.Create('Function "'+String(func)+'" not found');
            'D': if func='DIV' then Do_Divide
                 else raise ERtcScript.Create('Function "'+String(func)+'" not found');
            'N': if func='NOT' then Do_Not
                 else if func='NEW' then Do_New
                 else raise ERtcScript.Create('Function "'+String(func)+'" not found');
            else raise ERtcScript.Create('Function "'+String(func)+'" not found');
            end;
        else raise ERtcScript.Create('Function "'+String(func)+'" not found');
        end;
    finally
      objB.isNull:=True;
      obj.isNull:=True;
      end;
    end;

  procedure Execute_Assignment;
    var
      myvar,myvar2: TRtcFunctionInfo;
      vname,vname2:RtcWideString;
      what:byte;
      asAddr:boolean;
      objX:TRtcValueObject;

    begin
    what:=0;
    try
      try
        if Param.isType['X']<>rtc_Function then
          raise ERtcScript.Create('Left side can not be assigned to');

        if func[1]=':' then
          begin
          myvar:=TRtcFunctionInfo(Param.asObject['X']);
          Param.asObject['X']:=nil;
          end
        else
          myvar:=TRtcFunctionInfo(Param.asObject['X'].copyOf);

        try
          if (func[1]=':') and
             (Param.isType['Y']=rtc_Function) and
             (Param.asFunction['Y'].FunctionName='@') then
            begin
            Res.asObject:=Param.asFunction['Y'].asObject['Y'];
            Param.asFunction['Y'].asObject['Y']:=nil;
            end
          else
            begin
            case func[1] of
              '+':Do_Plus;
              '-':Do_Minus;
              '*':Do_Multiply;
              '/','\':Do_Divide;
              '%':Do_Modulo;
              '&':Do_And;
              '|':Do_Or;
              '!':Do_Not;
              ':':Do_Set;
              '^':Do_Xor;
              else
                raise ERtcScript.Create('Function "'+String(func)+'" not found');
              end;
            end;

          if (myvar.FunctionName='$') then
            begin
            vname:=myvar.asVarName['$V'];
            if vname='-' then
              vname:=vname+myvar.asVarName['$P'];
            asAddr:=myvar.asBoolean['$A'];
            objX:=_Execute(myvar.asObject['PARAMS']);
            try
              if assigned(objX) then
                begin
                vname2:=GetAsText(objX);
                if vname2<>'' then
                  begin
                  if vname<>'' then vname:=vname+'.';
                  vname:=vname+vname2;
                  end;
                end;
            finally
              RtcFreeAndNil(ObjX);
              end;
            if asAddr then what:=2 else what:=1;

            objX:=Res.asObject;
            Res.asObject:=nil;

            Set_Variable(vname, objX, asAddr);
            end
          else if (myvar.FunctionName='?') and (myvar.asString['C']='.') then
            begin
            vname:='';
            myvar2:=myvar;
            repeat
              objX:=_Execute(myvar2.asObject['Y']);
              try
                vname2:=GetAsText(objX);
                if vname2<>'' then
                  if vname<>'' then
                    vname:=vname2+'.'+vname
                  else
                    vname:=vname2;
              finally
                RtcFreeAndNil(objX);
                end;
              if myvar2.isType['X']<>rtc_Function then
                raise ERtcScript.Create('Left side can not be assigned to');
              myvar2:=TRtcFunctionInfo(myvar2.asObject['X']);
              until (myvar2.FunctionName<>'?') or (myvar2.asString['C']<>'.');

            if myvar2.FunctionName<>'$' then
              raise ERtcScript.Create('Left side can not be assigned to');

            objX:=_Execute(myvar2.asObject['PARAMS']);
            try
              if assigned(objX) then
                begin
                vname2:=GetAsText(objX);
                if vname2<>'' then
                  begin
                  if vname<>'' then vname:='.'+vname;
                  vname:=vname2+vname;
                  end;
                end;
            finally
              RtcFreeAndNil(ObjX);
              end;

            vname2:=myvar2.asVarName['$V'];
            if vname2='-' then
              vname2:=vname2+myvar2.asVarName['$P'];
            if vname2<>'' then
              begin
              if vname<>'' then vname:='.'+vname;
              vname:=vname2+vname;
              end;
            asAddr:=myvar2.asBoolean['$A'];

            if asAddr then what:=2 else what:=1;

            objX:=Res.asObject;
            Res.asObject:=nil;

            Set_Variable(vname, objX, asAddr);
            end
          else
            raise ERtcScript.Create('Left side can not be assigned to');

        finally
          RtcFreeAndNil(myvar);
          end;
      except
        on E:Exception do
          begin
          if E is ERtcForward then
            func:='['+Param.asText['$R']+':'+Param.asText['$C']+'] '
          else
            func:='['+Param.asText['$R']+':'+Param.asText['$C']+'] '+RtcWideString(E.ClassName)+' ';

          case what of
            0:func:=func+'Assignment';
            1:func:=func+'Setting variable "$'+vname+'"';
            2:func:=func+'Setting variable "@'+vname+'"';
            end;

          if Pos(RtcWideString(func),E.Message)>0 then
            begin
            if Copy(E.Message,1,1)='>' then
              raise ERtcForward.Create(String('>['+Param.asText['$R']+':'+Param.asText['$C']+'] '+RtcWideString(E.Message)))
            else
              raise ERtcForward.Create(String('>['+Param.asText['$R']+':'+Param.asText['$C']+']'+#13#10+RtcWideString(E.Message)));
            end
          else
            raise ERtcForward.Create(String(func)+#13#10+E.Message);
          end;
        end;
    finally
      objB.isNull:=True;
      obj.isNull:=True;
      end;
    end;

  procedure Execute_NewFunction;
    var
      vname:RtcWideString;
    begin
    vname:=Param.asVarName['V'];
    Comm.Locals.isNull[vname]:=True;
    with Comm.Locals.newFunction(vname,'!!') do
      asObject['X']:=Param.asObject['X'];
    Param.asObject['X']:=nil;
    end;

  begin
  Comm:=TRtcScriptCommandInfo(CmdInfo);

  if Comm.StopTime>0 then
    if GetTickTime64>Comm.StopTime then
      raise ERtcScript.Create('Script Execution Time Limit exceeded.');

  if Comm.MaxDepth>0 then
    if Comm.CodeDepth>=Comm.MaxDepth then
      raise ERtcScript.Create('Maximum Script code depth exceeded.')
    else
      Inc(Comm.CodeDepth);

  try
    if Param.FunctionName='!' then // Commands
      begin
      Result:=True;
      obj:=TRtcValue.Create;
      try
        try
          func:=UpperCase(Param.asText['C']);
          case func[1] of
            'I':Execute_If;
            'R':Execute_Repeat;
            'W':Execute_While;
            'F':Execute_For;
            'E':Execute_ForEach;
            'X':Execute_NewFunction;
            else
                raise ERtcScript.Create('Unknown command: "'+String(func)+'"');
            end;
        except
          on E:Exception do
            begin
            case func[1] of
              'I':func:='"IF" statement';
              'R':func:='"REPEAT" loop';
              'W':func:='"WHILE" loop';
              'F':func:='"FOR" loop';
              'E':func:='"FOREACH" loop';
              'X':func:='Script "FUNCTION"';
              else raise;
              end;
            if E is ERtcForward then
              begin
              func:='['+Param.asText['$R']+':'+Param.asText['$C']+'] '+func;
              if Pos(RtcWideString(func),E.Message)>0 then
                begin
                if Copy(E.Message,1,1)='>' then
                  raise ERtcForward.Create(String('>['+Param.asText['$R']+':'+Param.asText['$C']+'] '+RtcWideString(E.Message)))
                else
                  raise ERtcForward.Create(String('>['+Param.asText['$R']+':'+Param.asText['$C']+']'+#13#10+RtcWideString(E.Message)));
                end
              else
                raise ERtcForward.Create(String(func)+#13#10+E.Message);
              end
            else
              raise ERtcForward.Create(String('['+Param.asText['$R']+':'+Param.asText['$C']+'] '+
                                       RtcWideString(E.ClassName)+' in '+func+':'+#13#10+RtcWideString(E.Message)));
            end;
          end;
      finally
        obj.asObject:=nil;
        RtcFreeAndNil(obj);
        end;
      end
    else if Param.FunctionName='?' then // Functions
      begin
      Result:=True;
      obj:=TRtcValue.Create;
      objB:=TRtcValue.Create;
      try
        try
          func:=UpperCase(Param.asText['C']);
          Execute_Function;
        except
          on E:ERtcForward do
            begin
            func:='['+Param.asText['$R']+':'+Param.asText['$C']+'] Function "'+func+'"';
            if Pos(RtcWideString(func),E.Message)>0 then
              begin
              if Copy(E.Message,1,1)='>' then
                raise ERtcForward.Create(String('>['+Param.asText['$R']+':'+Param.asText['$C']+'] '+RtcWideString(E.Message)))
              else
                raise ERtcForward.Create(String('>['+Param.asText['$R']+':'+Param.asText['$C']+']'+#13#10+RtcWideString(E.Message)));
              end
            else
              raise ERtcForward.Create(String(func)+#13#10+E.Message);
            end;
          on E:Exception do
            raise ERtcForward.Create(String('['+Param.asText['$R']+':'+Param.asText['$C']+'] '+
                                     RtcWideString(E.ClassName)+' in Function "'+func+'":'+#13#10+RtcWideString(E.Message)));
          end;
      finally
        obj.asObject:=nil;
        RtcFreeAndNil(obj);
        objB.asObject:=nil;
        RtcFreeAndNil(objB);
        end;
      end
    else if Param.FunctionName='$!' then // variable assignment
      begin
      Result:=True;
      obj:=TRtcValue.Create;
      objB:=TRtcValue.Create;
      try
        func:=UpperCase(Param.asText['C']);
        Execute_Assignment;
      finally
        obj.asObject:=nil;
        RtcFreeAndNil(obj);
        objB.asObject:=nil;
        RtcFreeAndNil(objB);
        end;
      end
    else if Param.FunctionName='$.' then // property RtcWideString
      begin
      Result:=True;
      obj:=TRtcValue.Create;
      try
        Execute_Property;
      finally
        obj.asObject:=nil;
        RtcFreeAndNil(obj);
        end;
      end
    else if Param.FunctionName='$' then // read variable
      begin
      Result:=True;
      obj:=TRtcValue.Create;
      try
        Execute_Variable;
      finally
        obj.asObject:=nil;
        RtcFreeAndNil(obj);
        end;
      end
    else if Param.FunctionName='@' then // extract data
      begin
      Result:=True;
      Res.asObject:=Param.asObject['Y'];
      Param.asObject['Y']:=nil;
      end
    else
      Result:=False;
  finally
    if Comm.MaxDepth>0 then
      Dec(Comm.CodeDepth);
    end;
  end;

function TRtcScriptEngine.Execute(const Sender: TRtcConnection;
                                  const CompiledScript: TRtcValueObject;
                                  recursive:boolean=False): TRtcValue;
  var
    Comm:TRtcScriptCommandInfo;
    obj:TRtcValueObject;

  begin
  {$IFNDEF IDE_102up} Result:=nil; {$ENDIF}
  obj:=nil;
  try
    if not assigned(FGroup) then
      raise Exception.Create('Need a FunctionGroup to execute a Script.');

    // create temporary storage
    Comm:=TRtcScriptCommandInfo.Create;
    try
      Comm.Sender:=Sender;
      Comm.Locals:=TRtcRecord.Create;
      Comm.Globals:=Comm.Locals;
      Comm.Command:=self;
      Comm.Group:=FGroup;

      Comm.MaxDepth:=FMaxCodeDepth;
      Comm.MaxRecurse:=FMaxRecursion;
      Comm.MaxLoop:=FMaxLoopCount;

      if FMaxExecutionTime>0 then
        Comm.StopTime:=GetTickTime64+FMaxExecutionTime*1000
      else
        Comm.StopTime:=0;

      Comm.RecurseCount:=0;

      try
        // Execute script
        obj:=FGroup.ExecuteData(Comm, CompiledScript, recursive);

        if obj is TRtcValue then
          Result:=TRtcValue(obj)
        else
          begin
          Result:=TRtcValue.Create;
          Result.asObject:=obj;
          end;
      finally
        RtcFreeAndNil(Comm.Globals);
        RtcFreeAndNil(Comm);
        end;
    except
      on E:ERtcForward do
        raise ERtcScript.Create(E.Message);
      end;
  finally
    {$IFNDEF NEXTGEN} if obj<>CompiledScript then CompiledScript.Free; {$ENDIF}
    end;
  end;

function TRtcScriptEngine.Compile(const Script: RtcWideString; const FileName:RtcWideString=''): TRtcValue;
  var
    Compiler:TRtcScriptCompiler;
  begin
  Compiler:=TRtcScriptCompiler.Create;
  try
    Compiler.FunctionGroup:= FGroup;
    Compiler.DenyRTCFunctionCalls:= DenyRTCFunctionCalls;
    Compiler.DenyScriptFunctionCalls:= DenyScriptFunctionCalls;
    if FileName<>'' then
      begin
      Compiler.FilePath:= ExtractFilePath(FileName);
      Compiler.FileName:= ExtractFileName(FileName);
      end;
    Compiler.ScriptOpen:=FScriptOpen;
    Compiler.ScriptClose:=FScriptClose;
    Compiler.Script:= Script;
    Result:= Compiler.Compile;
  finally
    RtcFreeAndNil(Compiler);
    end;
  end;

procedure TRtcScriptEngine.SetScriptClose(const Value: RtcWideString);
  begin
  if length(Value)<>2 then
    raise Exception.Create('Need 2 characters for Script closing')
  else
    FScriptClose:=Value;
  end;

procedure TRtcScriptEngine.SetScriptOpen(const Value: RtcWideString);
  begin
  if length(Value)<>2 then
    raise Exception.Create('Need 2 characters for Script opening')
  else
    FScriptOpen:=Value;
  end;

procedure TRtcScriptEngine.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FGroup then
      FGroup:=nil;
  end;

end.
