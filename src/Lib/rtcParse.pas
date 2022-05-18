{
  @html(<b>)
  Simple Parser
  @html(</b>)
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @html(<br><br>)

  This unit defines a simple parser which is designed to be used either
  stand-alone or hand-in-hand with any of the RTC components. RTC parser uses
  a source RtcString as Template and replaces tokens inside TokenOpen and TokenClose.
  If you are writing a Web Server, you can use this parser to generate dynamic content
  by using HTML templates designed with any Web Page Editor.
}
unit rtcParse;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  Classes,

  rtcTypes,
  rtcSystem,
  rtcLog;

type
  { @exclude
    ERtcParse is an exception handler for TRtcParse objects }
  ERtcParse = class(Exception);

  { @abstract(Simple Template parser)
    Simple parser which is designed to be used either stand-alone or
    hand-in-hand with any of the RTC components. RTC parser uses
    a source RtcString as Template and replaces tokens inside TokenOpen and TokenClose.
    If you are writing a Web Server, you can use this parser to generate dynamic content
    by using HTML templates designed with any Web Page Editor. }
  TRtcParse = class(TObject)
  private
    FSource: RtcString;
    FSilent: Boolean;
    FTokenClose: RtcString;
    FTokenOpen: RtcString;
    FVariables: TStringList;
    FConditions: TStringList;
    FIncludePath : String;

    // @exclude
    function FindPos(const Substr, Str: RtcString; StartPos: Integer = 1): Integer;
     // Uppercase case-insensitive FindPos
    function FindPosUp(const Substr, Str: RtcString; StartPos: Integer = 1): Integer;

    // @exclude
    function GetCount: Integer;
    // @exclude
    function GetVariableName(Index: Integer): String;
    // @exclude
    procedure SetVariableName(Index: Integer; const AValue: String);
    // @exclude
    function GetVariableValue(Index: Integer): RtcString;
    // @exclude
    procedure SetVariableValue(Index: Integer; const AValue: RtcString);
    // @exclude
    function CheckValue(const Index: String): boolean;
    // @exclude
    function GetValue(const Index: String): RtcString;
    // @exclude
    procedure SetValue(const Index: String; const AValue: RtcString);

    // @exclude
    procedure SetSource(const AValue: RtcString);

    // @exclude
    procedure SetTokenOpen(const AValue: RtcString);
    // @exclude
    procedure SetTokenClose(const AValue: RtcString);

    // @exclude
    function GetCondition(const Index: String): boolean;
    // @exclude
    procedure SetCondition(const Index: String; const Value: boolean);

  protected
    { @exclude
      Parses the source RtcString and builds a list of variable names }
    procedure GetVariables;

  public
    { Constructor: use to create a parser object.
      Pass FileName as parameter to load local file as Source template. }
    constructor Create(const AFilename: String = '');

    { Destructor: when you are done using the parser,
      you should destroy it by calling Free. }
    destructor Destroy; override;

    { Clears values for all variables parsed from the source RtcString.
      Using Clear, you can re-use your Source Template to generate more
      outputs with different content, since only values for variables will
      be removed, while Source and known variable names remain. }
    procedure Clear;

    { Loads the source RtcString from a file }
    procedure LoadFromFile(const aFilename: String);

    { Loads the source RtcString from a stream }
    procedure LoadFromStream(aStream:TStream);

    { Generates the output, replacing all variables with their associated values }
    function Output: RtcString;

    { Gets count of variables parsed from the source RtcString }
    property Count: Integer read GetCount default 0;

    { Name of the 'index'-th variable parsed from the source RtcString (starting from 0) }
    property VariableName[Index: Integer]: String read GetVariableName write SetVariableName;
    { Value of the 'index'-th variable parsed from the source RtcString (starting from 0) }
    property VariableValue[Index: Integer]: RtcString read GetVariableValue write SetVariableValue;

    { Variable with the name 'Index' (parsed from the source RtcString) has a Value assigned. }
    property HasValue[const Index: String]: boolean read CheckValue;

    { Value of the variable with the name 'Index' parsed from the source RtcString }
    property Value[const Index: String]: RtcString read GetValue write SetValue; default;

    property Condition[const Index: String]: boolean read GetCondition write SetCondition;
    property Conditions : TStringList read FConditions;

    { Source RtcString (Template) to use when generating the output }
    property Source: RtcString read FSource write SetSource;

    { Prevents an exception from being raised when trying to set the value
      of a non-existent variable }
    property Silent: Boolean read FSilent write FSilent default False;
    { RtcString to use for opening token. Default is <% }
    property TokenOpen: RtcString read FTokenOpen write SetTokenOpen;
    { RtcString to use for closing token. Default is %> }
    property TokenClose: RtcString read FTokenClose write SetTokenClose;
  end;

function RtcIncludeTrailingPathDelimiter(const S: String): String;

implementation

const
  BEGIN_CONDITION  = 'IF:';
  ELSE_CONDITION   = 'ELSEIF:';
  END_CONDITION    = 'ENDIF:';
  BEGIN_INCLUDE    = 'INCLUDE:';

type
  // @exclude
  TString = class(TObject)
  public
    Value: RtcString;
    constructor Create(const AValue: RtcString = '');
  end;

  // @exclude
  TCondition = class(TObject)
  public
    Value: boolean;
    constructor Create(AValue: boolean = False);
  end;

function RtcGetPathRelativeTo (Root : String; FilePath : String): String;
  begin
  Result := FilePath;
  if Copy(Result,1,1)=FOLDER_DELIMITER then
    Delete(Result,1,1);
  if Copy(Root,length(Root),1)=FOLDER_DELIMITER then
    Result:=Root+FilePath
  else
    Result:=Root+FOLDER_DELIMITER+FilePath;
  end;

function RtcIncludeTrailingPathDelimiter(const S: String): String;
  begin
  if Copy(S, Length(S), 1)=FOLDER_DELIMITER then
    Result := S
  else
    Result := S + FOLDER_DELIMITER;
  end;

{ TString }

constructor TString.Create(const AValue: RtcString = '');
  begin
  inherited Create;
  Value := AValue;
  end;

{ TRtcParse }

function TRtcParse.FindPos(const Substr, Str: RtcString; StartPos: Integer = 1): Integer;
  var
    lenStr: Integer;
    lenSubstr: Integer;
    x, y: Integer;
  begin
  lenStr := Length(Str);
  lenSubstr := Length(Substr);

  case lenSubstr of
    0: Result := 0;

    1: begin
      Result := 0;
      for x:= StartPos to lenStr do
        if (Substr[1] = Str[x]) then
          begin
          Result := x;
          Break;
          end;
      end;
    2: begin
      Result := 0;
      for x := StartPos to lenStr-1 do
        if ((Substr[1] = Str[x]) and (SubStr[2] = Str[x+1])) then
          begin
          Result := x;
          Break;
          end;
      end;
    else
      begin
      Result := 0;
      for x := StartPos to lenStr-lenSubstr+1 do
        if ((Substr[1] = Str[x]) and (Substr[2] = Str[x+1]) and (Substr[3] = Str[x+2])) then
          begin
          Result := x;
          for y := 3 to lenSubstr-1 do
            if (Substr[1+y] <> Str[x+y]) then
              begin
              Result := 0;
              Break;
              end;
          if Result > 0 then
            Break;
          end;
      end;
    end;
  end;

function TRtcParse.FindPosUp(const Substr,Str: RtcString; StartPos: Integer = 1): Integer;
  var
    lenStr: Integer;
    lenSubstr: Integer;
    x, y: Integer;
  begin
  lenStr := Length(Str);
  lenSubstr := Length(Substr);

  case lenSubstr of
    0: Result := 0;

    1: begin
      Result := 0;
      for x:= StartPos to lenStr do
        if Substr[1] = Up_Case(Str[x]) then
          begin
          Result := x;
          Break;
          end;
      end;
    2: begin
      Result := 0;
      for x := StartPos to lenStr-1 do
        if ((Substr[1] = Up_Case(Str[x])) and (SubStr[2] = Up_Case(Str[x+1]))) then
          begin
          Result := x;
          Break;
          end;
      end;
    else
      begin
      Result := 0;
      for x := StartPos to lenStr-lenSubstr+1 do
        if ((Substr[1] = Up_Case(Str[x])) and (Substr[2] = Up_Case(Str[x+1])) and (Substr[3] = Up_Case(Str[x+2]))) then
          begin
          Result := x;
          for y := 3 to lenSubstr-1 do
            if (Substr[1+y] <> Up_Case(Str[x+y])) then
              begin
              Result := 0;
              Break;
              end;
          if Result > 0 then
            Break;
          end;
      end;
    end;
  end;

function TRtcParse.GetCount: Integer;
  begin
  if Assigned(FVariables) then
    Result := FVariables.Count
  else
    Result := 0;
  end;

function TRtcParse.GetVariableName(Index: Integer): String;
  begin
  // return the selected variable's name
  if Assigned(FVariables) and (Index >= 0) and (Index < FVariables.Count) then
    Result := FVariables.Strings[Index]
  else
    Result := '';
  end;

procedure TRtcParse.SetVariableName(Index: Integer; const AValue: String);
  begin
  // set the selected variable's name
  if Assigned(FVariables) and (Index >= 0) and (Index < FVariables.Count) then
    FVariables.Strings[Index] := AValue;
  end;

function TRtcParse.GetVariableValue(Index: Integer): RtcString;
  begin
  // return the selected variable's value
  if Assigned(FVariables) and (Index >= 0) and (Index < FVariables.Count) and
     Assigned(FVariables.Objects[Index]) then
    Result := TString(FVariables.Objects[Index]).Value
  else
    Result := '';
  end;

procedure TRtcParse.SetVariableValue(Index: Integer; const AValue: RtcString);
  begin
  // set the selected variable's value
  if Assigned(FVariables) and (Index >= 0) and (Index < FVariables.Count) then
    if Assigned(Fvariables.Objects[Index]) then
      TString(FVariables.Objects[Index]).Value := AValue
    else
      FVariables.Objects[Index]:=TString.Create(AValue);
  end;

function TRtcParse.GetValue(const Index: String): RtcString;
  var
    idx: Integer;
  begin
  // return the value of variable named 'Index'
  if Assigned(FVariables) then
    begin
    idx := FVariables.IndexOf(UpperCase(Trim(Index)));
    if (idx >= 0) and Assigned(FVariables.Objects[idx]) then
      Result := TString(FVariables.Objects[idx]).Value
    else
      Result := '';
    end
  else
    Result := '';
  end;

function TRtcParse.CheckValue(const Index: String): boolean;
  var
    idx: Integer;
  begin
  // return TRUE if a value is assigned to variable named 'Index'
  if Assigned(FVariables) then
    begin
    idx := FVariables.IndexOf(UpperCase(Trim(Index)));
    if (idx >= 0) and Assigned(FVariables.Objects[idx]) then
      Result := True
    else
      Result := False;
    end
  else
    Result := False;
  end;

function TRtcParse.GetCondition(const Index: String): boolean;
var
  idx: Integer;
begin
  // return the value of condition named 'Index'
  if Assigned(FConditions) then
    begin
      idx := FConditions.IndexOf(UpperCase(Trim(Index)));
      if (idx >= 0) and Assigned(FConditions.Objects[idx]) then
        Result := TCondition(FConditions.Objects[idx]).Value
      else
        Result := False;
    end
  else
    Result := False;
end;

procedure TRtcParse.SetValue(const Index: String; const AValue: RtcString);
  var
    idx: Integer;
  begin
  if Assigned(FVariables) then
    begin
    // set the value of variable named 'Index'
    idx := FVariables.IndexOf(UpperCase(Trim(Index)));
    if idx >= 0 then
      begin
      if Assigned(Fvariables.Objects[idx]) then
        TString(FVariables.Objects[idx]).Value := AValue
      else
        FVariables.Objects[idx]:=TString.Create(AValue);
      end
    else
      if not Silent then
        raise ERtcParse.Create('Unknown Variable: ' + Index);
    end;
  end;

procedure TRtcParse.SetCondition(const Index: String; const Value: boolean);
var
  idx: Integer;
begin
  if Assigned(FConditions) then
    begin
    // set the value of condition named 'Index'
    idx := FConditions.IndexOf(UpperCase(Trim(Index)));
    if idx >= 0 then
      begin
        if Assigned(FConditions.Objects[idx]) then
          TCondition(FConditions.Objects[idx]).Value := Value
        else
          FConditions.Objects[idx] := TCondition.Create(Value);
      end
    else
      if not Silent then
        raise ERtcParse.Create('Unknown Condition: ' + Index);
  end;
end;

procedure TRtcParse.SetSource(const AValue: RtcString);
  begin
  // set the new source RtcString and (re)build the variables list
  if FSource <> AValue then
    begin
    FSource := AValue;
    GetVariables;
    end;
  end;

procedure TRtcParse.SetTokenOpen(const AValue: RtcString);
  begin
  // set the new open token delimiter and (re)build the variables list
  if (AValue <> '') and (FTokenOpen <> AValue) then
    begin
    FTokenOpen := Trim(AValue);
    GetVariables;
    end;
  end;

procedure TRtcParse.SetTokenClose(const AValue: RtcString);
  begin
  // set the new close token delimiter and (re)build the variables list
  if (AValue <> '') and (FTokenClose <> AValue) then
    begin
    FTokenClose := Trim(AValue);
    GetVariables;
    end;
  end;

procedure TRtcParse.GetVariables;
  var
    lTokenOpen,
    lTokenClose: Integer;
    posStart: Integer;
    posEnd: Integer;
    variable: String;
    idx: Integer;
  begin
  if (FSource <> '') then
    begin
    // clear/create the existing variable list
    if Assigned(FVariables) then
      begin
      Clear;
      FVariables.Clear;
      end
    else
      FVariables := TStringList.Create;

    // clear/create the existing conditions list
    if Assigned(FConditions) then
      begin
        Clear;
        FConditions.Clear;
      end
    else
      FConditions := TStringList.Create;

    lTokenOpen := Length(FTokenOpen);
    lTokenClose := Length(FTokenClose);

    // look for the tokens in the source RtcString and extract the variables
    posStart := FindPos(FTokenOpen, FSource, 1);
    while posStart > 0 do
      begin
      posEnd := FindPos(FTokenClose, FSource, posStart+lTokenOpen);
      if (posEnd <= 0) then Break;

      // extract the variable name from the source RtcString
      variable := String(Copy(FSource, posStart+lTokenOpen, posEnd-(posStart+lTokenOpen)));
      if variable <> '' then
        begin
        variable := UpperCase(Trim(variable));

        // we don't want duplicated variable names
        idx := FVariables.IndexOf(variable);
        if (idx < 0) then
          begin
          if Copy(variable, 1, 8) = BEGIN_INCLUDE then
            begin
              variable := Copy(variable, 9, MAXINT);
              Delete(FSource, posStart, lTokenOpen + 8 + Length(variable) + lTokenClose);
              variable := RtcIncludeTrailingPathDelimiter(RtcGetPathRelativeTo(FIncludePath, ExtractFilePath(variable))) +
                ExtractFileName(variable);
              if FileExists(variable) then
                Insert(Read_File(RtcWideString(variable)), FSource, posStart);
              posEnd := posStart - 1;
            end
          else if Copy(variable, 1, 3) = BEGIN_CONDITION then
            begin
              variable := Copy(variable, 4, MAXINT);
              idx := FConditions.IndexOf(variable);
              if (idx < 0) then
                FConditions.AddObject(variable, TCondition.Create);
            end
          else if Copy(variable, 1, 7) = ELSE_CONDITION then
            begin
              // nothing to do
            end
          else if Copy(variable, 1, 6) = END_CONDITION then
            begin
              // nothing to do
            end
          else
            FVariables.AddObject(variable, TString.Create);
          end;
        end;
      posStart := FindPos(FTokenOpen, FSource, posEnd+1);
      end;
    end;
  end;

constructor TRtcParse.Create(const AFilename: String = '');
  begin
  inherited Create;

  // set the default values for the parser
  FSource := '';
  FSilent := False;
  FTokenOpen := '<%';
  FTokenClose := '%>';
  FVariables := nil;
  FConditions := nil;

  FIncludePath := '';

  // load the source RtcString from a file
  if AFilename <> '' then
    try
      LoadFromFile(AFilename);
    except
    end;
  end;

destructor TRtcParse.Destroy;
  begin
  try
    // clear the variable list and clean up any allocated memory
    Clear;
    RtcFreeAndNil(FVariables);
    RtcFreeAndNil(FConditions);

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcParse.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcParse.Clear;
  var
    count: Integer;
  begin
  // clear all variables parsed from source RtcString
  if Assigned(FVariables) then
    begin
    for count := 0 to FVariables.Count-1 do
      if Assigned(FVariables.Objects[count]) then
        begin
        {$IFNDEF NEXTGEN} FVariables.Objects[count].Free; {$ENDIF}
        FVariables.Objects[count] := nil;
        end;
    end;
  // clear all conditions parsed from source RtcString
  if Assigned(FConditions) then
    begin
    for count := 0 to FConditions.Count-1 do
      if Assigned(FConditions.Objects[count]) then
        begin
        {$IFNDEF NEXTGEN} FConditions.Objects[count].Free; {$ENDIF}
        FConditions.Objects[count] := nil;
        end;
    end;
  end;

procedure TRtcParse.LoadFromFile(const aFilename: String);
  begin
  if FileExists(aFileName) then
    begin
    FIncludePath := RtcIncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(AFilename)));
    FSource:= Read_File(RtcWideString(aFileName));
    // build the variable list
    GetVariables;
    end
  else
    raise ERtcParse.Create('File not found: ' + aFilename);
  end;

procedure TRtcParse.LoadFromStream(aStream: TStream);
  var
    Len:int64;
    {$IFNDEF RTC_BYTESTRING}
    data:RtcByteArray;
    {$ENDIF}
  begin
  if assigned(aStream) then
    begin
    FIncludePath := '';
    Len:=aStream.Size-aStream.Position;
    if Len>0 then
      begin
    {$IFDEF RTC_BYTESTRING}
      SetLength(FSource, Len);
      aStream.Read(FSource[1], Len);
    {$ELSE}
      SetLength(data, Len);
      aStream.Read(data[0],Len);
      FSource:= RtcBytesToString(data);
      SetLength(data,0);
    {$ENDIF}
      end
    else
      FSource:='';
    GetVariables;
    end
  else
    raise ERtcParse.Create('aStream parameter is NIL');
  end;

function TRtcParse.Output: RtcString;

  function GetOutput(const FSource:RtcString):RtcString;
    var
      lSource: Integer;
      lTokenOpen: Integer;
      lTokenClose: Integer;

      copyStart: Integer;
      posStart: Integer;
      posEnd: Integer;
      variable: String;
      idx: Integer;
      S : RtcString;
      posStart_End_Condition : Integer;
    begin
    Result:='';
    if FSource <> '' then
      begin
      lSource := Length(FSource);
      lTokenOpen := Length(FTokenOpen);
      lTokenClose := Length(FTokenClose);
      copyStart := 1;

      // look for the tokens and replace matching variables with their values
      posStart := FindPos(FTokenOpen, FSource, 1);
      while posStart > 0 do
        begin
        Result := Result + Copy(FSource, copyStart, posStart-copyStart);

        posEnd := FindPos(FTokenClose, FSource, posStart+1);
        if posEnd <= 0 then Break;

        // extract the variable name from the source RtcString
        variable := String(Copy(FSource, posStart+lTokenOpen, posEnd-(posStart+lTokenOpen)));
        if variable <> '' then
          begin
          variable := UpperCase(Trim(variable));

          // only replace the variable if it is present in list
          idx := FVariables.IndexOf(variable);
          if idx > -1 then
            Result := Result + GetVariableValue(idx)
          else
            begin
            if Copy(variable, 1, 3) = BEGIN_CONDITION then
              begin
              variable := Copy(variable, 4, MAXINT);
              idx := FConditions.IndexOf(variable);
              if (idx >= 0) then
                begin
                copyStart := posEnd + lTokenClose;
                posEnd := FindPos(FTokenClose, FSource, posStart+1);

                S := Upper_Case(FTokenOpen + END_CONDITION + RtcString(variable));
                posStart_End_Condition := FindPosUp(S, FSource, posEnd+1);

                posStart := FindPosUp(Upper_Case(FTokenOpen + ELSE_CONDITION + RtcString(variable)), FSource, posEnd+1);

                if posStart > posStart_End_Condition then
                  posStart := -1;

                if posStart > 0 then
                  begin
                  if Condition[variable] then
                    Result := Result + GetOutput(Copy(FSource, copyStart, posStart-copyStart));

                  posEnd := FindPos(FTokenClose, FSource, posStart+1);
                  copyStart := posEnd + lTokenClose;

                  posStart := FindPosUp(S, FSource, posEnd+1); // FTokenOpen + END_CONDITION + variable

                  if not Condition[variable] then
                    Result := Result + GetOutput(Copy(FSource, copyStart, posStart-copyStart));

                  posEnd := FindPos(FTokenClose, FSource, posStart+1);
                  end
                else
                  begin
                  posStart := FindPosUp(S, FSource, posEnd+1); // FTokenOpen + END_CONDITION + variable

                  posEnd := FindPos(FTokenClose, FSource, posStart+1);

                  if Condition[variable] then
                    Result := Result + GetOutput(Copy(FSource, copyStart, posStart-copyStart));
                  end;
                end;
              end;
            end;

          copyStart := posEnd + lTokenClose;
          posStart := FindPos(FTokenOpen, FSource, posEnd+1);
          end;
        end;
      // make sure that remaining part of FSource is returned
      if copyStart < lSource then
        Result := Result + Copy(FSource, copyStart, lSource-copyStart+1);
      end;
    end;
  begin
  Result:=GetOutput(FSource);
  end;

{ TCondition }

constructor TCondition.Create(AValue: boolean);
begin
  inherited Create;
  Value := AValue;
end;

end.
