unit SigFuncs; // Signature functions for Rtc Function Wizard
(*
  Programmer: Glynn Owen
  Copyright 2007 by Glynn Owen
  Created: 1/27/07
  Note: Output file is re-built each time before it is saved

  Decomposition results in a string list that has this format -

    FunctionName=NameOfFunction
    PasReturnType=TypeOfOutPut
    InputParam1=TypeOfParam
    ...
    InputParamN=TypeOfParam

  So that the name of the function is always the first NAME=VALUE pair,
  the function's type is always the second NAME=VALUE pair, and the
  NAME=VALUE pairs past the second each determine an input parameter
  name and data type.

  Every datatype must cross-reference to fit within one of these conversions -
    property asBoolean:boolean read GetBoolean write SetBoolean;
    property asInteger:rtcInteger read GetInteger write SetInteger;
    property asLargeInt:rtcLargeInt read GetLargeInt write SetLargeInt;
    property asFloat:rtcFloat read GetFloat write SetFloat;
    property asCurrency:Currency read GetCurrency write SetCurrency;
    property asDateTime:TDateTime read GetDateTime write SetDateTime;
    property asString:RtcString read GetString write SetString;
    property asWideString:RtcWideString read GetWideString write SetWideString;
    property asByteStream:TStream read GetByteStream write SetByteStream;
    property asArray:TRtcArray read GetArray write SetArray;
    property asRecord:TRtcRecord read GetRecord write SetRecord;
    property asDataSet:TRtcDataSet read GetDataSet write SetDataSet;

*)
interface

uses rtcTypes, rtcParse, StrUtils, Classes, SysUtils;

type

  TRemFuncSrcDoc = class(TStringList)
  protected
    FSigs, FSigErrors: TStringList;
    FOuterParser, FInnerParser: TrtcParse;
    FActiveSig: integer;
    function GetSigErrors: TStringList;
    function getSigs: TStrings;
    procedure SetSigs(const Value: TStrings);
    function GetSigVar(IX: integer; VarName: string): string;
    procedure SetSigVar(IX: integer; VarName: string; const Value: string);
    function GetSigVars(IX: integer): TstringList;
    function GetFileName: string;
    function GetTemplateFileName: string;
    procedure SetTemplateFileName(const Value: string);
    function GetTagUnitName: string;
    function GetTagClassName: string;
    function GetTagFunctionSignatureList: string;
    function GetTagImplement: string;
    function GetTagFunctionSignature: string;
    function GetTagFunctionName: string;
    function GetTagFunctionParams: string;
    function GetTagPreserveReturn: string;
    function GetPasReturnType: string;
    function GetTagRtcReturnEnum: string;
    function GetTagRtcReturnAs: string;
    function GetTagNILResult: string;
    function RtcConversionType(const Typ: string): string;
    function RtcEnumType(const Typ: string): string;
    function XRefType(const Typ: string): string;
    procedure SetOuterTagValues;
    procedure SetInnerTagValues;
    procedure ClearSigs;
    function ImplementActiveSig: string;
    function ImplementAllSigs: string;
    function SimpleType(Typ: string): boolean;
    function ClassNameFromSource(Src: TStrings): string;
    procedure ParseParams(OParen, CParen: integer; const Txt: TStringList; const
      Sig: string);
    function ValidName(const Name: string): boolean;
    function CleanParm(const Parm: string): string;
    function IsValidParamType(Typ: string): boolean;
    procedure AddError(Err: string);
    property ActiveSig: integer read FActiveSig;
    // Named value of one signature
    property SigVar[IX: integer; VarName: string]: string
    read GetSigVar
      write SetSigVar;
    property OutputFileName: string read GetFileName;
    property TemplateFileName: string
      read GetTemplateFileName
      write SetTemplateFileName;
    property OuterParser: TrtcParse read FOuterParser;
    property InnerParser: TrtcParse read FInnerParser;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetFunctionListFromSource(const Lst: TStrings);
    function ForceTerminator(var Sig: string): boolean;
    function FQNSource: string;
    procedure WriteSigImplementation(const SigImpTxt: TStrings; Sig: string);
    // Return NAME=VALUE pairs separated by CRLF
    function DecomposeSig(Sig: string): string;
    function Compile: Boolean;
    // All variables for one signature
    property SigVars[IX: integer]: TstringList read GetSigVars;
    property Signatures: TStrings read GetSigs write SetSigs;
    property SigErrors: TStringList read GetSigErrors;
  end;

  //Var SigErrors:TStringList;

implementation

uses Forms, Dialogs, MainForm;

const
  CRLF = #13#10;

  cWizardFile = '// RTC Remote Functions Wizard output file';
  cFuncDefnBegin = '// BEGIN User-defined remote functions';
  cFuncDefnEnd = '// END User-defined remote functions';
  cFuncImpBegin = '// BEGIN function implementation';
  cFuncImpEnd = '// END function implementation';

  cDefaultTemplateFileName = 'RemoteFunctionTemplate.txt';
  tagTemplateFileName = 'TemplateFileName';

  cTabSize = 2;

  // Positions of key items in a SigVar list
  cFuncNameIX = 0;
  cFuncTypeIX = 1;

  cPasReturnType = 'PasReturnType';

  // Outer template tags
  tagUnitName = 'UnitName';
  tagClassName = 'ClassName';
  tagFunctionName = 'FunctionName';
  tagFunctionSignatureList = 'FunctionSignatureList';
  tagFunctionImplementation = 'FunctionImplementation';
  // Inner template tags
  tagNilResult = 'NilResult';
  tagFunctionSignature = 'FunctionSignature';
  tagFunctionParameters = 'FunctionParameters';
  tagRtcReturnEnum = 'RtcReturnEnum';
  tagRtcReturnAs = 'RtcReturnAs';
  tagPreserveReturn = 'PreserveReturn';

var
  OKTypes: TStringList;
  TypeXRef: TStringList;
  AlphaChars: set of char = ['a'..'z', '_', 'A'..'Z'];
  NameChars: set of char = ['a'..'z', '_', 'A'..'Z', '0'..'9'];

  { TRemFuncSrcDoc }

constructor TRemFuncSrcDoc.create;
var
  Start, Stop: integer;
begin
  inherited;
  if not FileExists(TemplateFileName) then
  begin
    ShowMessage('"' + TemplateFileName + '" - Template file is missing');
    Application.Terminate;
  end;
  FSigs := TStringList.Create;
  FSigErrors := TStringList.Create;
  FOuterParser := TrtcParse.Create(TemplateFileName);
  FInnerParser := TrtcParse.Create;
  Start := pos(cFuncImpBegin, FOuterParser.Source);
  if Start = 0 then
    raise Exception.Create('Missing BEGIN bracket for implementing functions');
  Stop := pos(cFuncImpEnd, FOuterParser.Source);
  if Stop = 0 then
    raise Exception.Create('Missing END bracket for implementing functions');
  // Extract inner source
  FInnerParser.Source := trim(copy(FOuterParser.Source, Start +
    length(cFuncImpBegin), Stop - Start - length(cFuncImpEnd) - 2));
  // Delete inner source from outer source
  FOuterParser.Source := trim(copy(FOuterParser.Source, 1, Start));
end;

destructor TRemFuncSrcDoc.Destroy;
begin
  ClearSigs;
  FSigs.Free;
  FSigErrors.Free;
  FInnerParser.Free;
  FOuterParser.Free;
  inherited;
end;

procedure TRemFuncSrcDoc.AddError(Err: string);
begin
  SigErrors.Add(Err)
end;

function TRemFuncSrcDoc.IsValidParamType(Typ: string): boolean;
begin
  Result := OKTypes.IndexOf(Typ) >= 0
end;

function TRemFuncSrcDoc.CleanParm(const Parm: string): string;
var
  Uvar: string;
begin
  Result := trim(Parm);
  UVar := uppercase(Result);
  if (Uvar = 'CONST') then
    Result := ''
  else if (Uvar = 'VAR') then
  begin
    AddError('Keyword "VAR" may not be used in a remote function signature');
    Abort
  end;
end;

function TRemFuncSrcDoc.FQNSource: string;
  begin
  Result := trim(formMain.edtUnitPath.Text) + trim(formMain.edtUnitFileName.Text);
  if length(Result)>0 then
    if Result[length(Result)]<>FOLDER_DELIMITER then
      Result:=Result+FOLDER_DELIMITER;
  end;

function TRemFuncSrcDoc.ForceTerminator(var Sig: string): boolean;
begin
  // Return TRUE <=> terminator was added
  Result := (length(Sig) > 0) and not (Sig[length(Sig)] = ';');
  if Result then
    Sig := Sig + ';';
end;

function TRemFuncSrcDoc.ValidName(const Name: string): boolean;
var
  I: integer;
begin
  Result := length(Name) <> 0;
  if not Result then
  begin // 0-length name
    AddError('Zero-length name encountered');
    Exit; // 0-length name
  end;
  Result := Name[1] in AlphaChars;
  if not Result then
  begin // Names must begin with a letter
    AddError('"' + Name + '" does not begin with a letter');
    Exit
  end;
  Result := FALSE;
  for I := 2 to length(Name) do
    if not (Name[I] in NameChars) then
    begin
      AddError('"' + Name + '" contains an invalid character');
      Exit;
    end;
  Result := TRUE
end;

function TRemFuncSrcDoc.ClassNameFromSource(Src: TStrings): string;
var
  I, J: integer;
begin
  for I := 0 to pred(Src.Count) do
  begin
    J := pos(' = ', src[I]);
    if J > 0 then
    begin
      Result := trim(copy(Src[I], 1, J));
      Exit
    end;
  end;
  Result := '';
end;

procedure TRemFuncSrcDoc.ParseParams(OParen, CParen: integer; const Txt:
  TStringList; const Sig: string);
// OParen is opening parenthesis and CParen is closing parenthesis within Sig
var
  I, IX: Integer;
  Maybe, Typ: string;
  CommaParms: TStringList;
  TypeAt: Integer;
begin
  IX := OParen;
  CommaParms := TStringList.Create;
  try
    repeat
      // Skip up to type separator
      while (IX < CParen) and not (Sig[IX] = ':') do
        inc(IX);
      TypeAt := IX; // mark point where type separator occurs
      // Move index to end of this set of parameters
      while (IX < CParen) and not (Sig[IX] = ';') do
        inc(IX);
      // Extract type for this list of parameter names
      Typ := trim(copy(Sig, TypeAt + 1, IX - TypeAt - 1));
      if not IsValidParamType(Typ) then
      begin
        AddError('"' + Typ + '" is not a valid datatype');
        Exit
      end;
      // Allow for default parameter values
      I := pos('=', typ);
      if I > 0 then
        Typ := trim(copy(Typ, 1, I - 1));
      // Load parameters into a stringlist
      CommaParms.CommaText := copy(Sig, OParen + 1, TypeAt - OParen - 1);
      // Use Oparen to track start of next list of parameter names
      OParen := IX;
      for I := 0 to CommaParms.Count - 1 do
      begin
        Maybe := CleanParm(CommaParms[I]);
        if length(Maybe) > 0 then
          if ValidName(Maybe) then
            Txt.Add(Maybe + '=' + Typ)
          else
          begin
            AddError('Parameter "' + Maybe + '" is not a valid name');
            Exit;
          end
      end
    until IX >= CParen;
  finally
    CommaParms.Free
  end
end;

procedure TRemFuncSrcDoc.GetFunctionListFromSource(const Lst: TStrings);
var
  Src: TStringlist;
  FName, UCase, FuncSig: string;
  IXBegin, IXEnd: Integer;
begin
  Lst.Clear;
  Src := TStringList.Create;
  try
    FName := FQNSource;
    if length(FName) = 0 then
      Exit;
    if fileExists(FName) then
      Src.LoadFromFile(FName)
    else
      Exit; // New file - list is empty
    formMain.UnitClassname := ClassNameFromSource(Src);
    if pos(cWizardFile, Src[0]) = 0 then
      raise
        Exception.Create('Source file is not an RTC Remote Functions Wizard file');
    UCase := uppercase(Src[0]);
    if pos('UNIT ' + upperCase(formMain.UnitBasename), UCase) = 0 then
      raise Exception.Create('Unit name mismatch');
    IXBegin := Src.IndexOf(cFuncDefnBegin);
    IXEnd := Src.IndexOf(cFuncDefnEnd);
    if IXBegin < 0 then
      raise Exception.Create('Beginning of function interface list not found');
    if IXEnd < 0 then
      raise Exception.Create('End of function interface list not found');
    if IXEnd <= IXBegin then
      raise Exception.Create('Invalid function list markers');
    Inc(IXBegin);
    repeat
      FuncSig := trim(Src[IXBegin]);
      // Add function signatures to the list
      if pos('function', FuncSig) = 1 then
        Lst.Add(FuncSig);
      // Add remmed out function signatures to the list
      if pos('// function', FuncSig) = 1 then
        Lst.Add(copy(FuncSig, 4, length(FuncSig)));
      Inc(IXBegin)
    until IXBegin = IXEnd;
  finally
    Src.Free
  end
end;

function TRemFuncSrcDoc.DecomposeSig(Sig: string): string;
var
  Txt: TStringList;
  Cparen, Oparen, Colon: Integer;
  Tmp: string;
begin
  Result := '';
  Txt := TStringList.Create;
  SigErrors.Clear;
  try
    Tmp := uppercase(copy(Sig, 1, 9));
    if not (Tmp = 'FUNCTION ') then
    begin
      AddError('Signature does not begin with "FUNCTION" keyword');
      Exit;
    end;
    if Sig[length(Sig)] <> ';' then
      AddError('No ";" terminator');
    Oparen := pos('(', Sig);
    Cparen := pos(')', Sig);
    if Cparen > 0 then
      if Oparen < 1 then
        AddError('Opening parenthesis not found');
    if Oparen > 0 then
      if Cparen < 1 then
        AddError('Closing parenthesis not found');

    if SigErrors.Count > 1 then
      Exit;

    // Locate return type separator
    if Oparen = 0 {// there are no parameters} then
      Colon := pos(':', Sig)
    else
      Colon := PosEx(':', Sig, CParen);
    if Colon = 0 then
    begin
      AddError('Missing ":" before return datatype');
      Exit
    end;

    // Need Oparen to find the end of the name
    if Oparen = 0 then
      OParen := Colon;

    // Pull out the function name
    Tmp := trim(copy(Sig, 10, Oparen - 10));
    if (OParen < 11) or (length(Tmp) = 0) then
    begin
      AddError('Missing function name');
      Exit
    end;

    Txt.Add('FunctionName=' + Tmp);

    if CParen > 0 then
      ParseParams(OParen, CParen, Txt, Sig);
    if SigErrors.Count > 0 then
      Exit; // Errors during parse of parameter list

    Tmp := trim(copy(Sig, Colon + 1, 32));
    // Note implied limit to length of type name
    system.Delete(Tmp, length(Tmp), 1); // Remove trailing semi-colon
    if not IsValidParamType(Tmp) then
    begin
      AddError('"' + Tmp + '" is not a valid return type');
      Exit;
    end;

    Txt.Insert(1, cPasReturnType + '=' + Tmp);
    Result := Txt.Text;
  finally
    Txt.Free
  end
end;

procedure TRemFuncSrcDoc.ClearSigs;
var
  I: Integer;
begin
  for I := 0 to pred(FSigs.Count) do
  begin
    FSigs.Objects[I].Free;
    FSigs.Objects[I] := nil
  end;
  FSigs.Clear;
end;

function TRemFuncSrcDoc.getSigs: TStrings;
begin
  Result := FSigs;
end;

procedure TRemFuncSrcDoc.SetSigs(const Value: TStrings);
var
  I: integer;
  SigVars: TStringList;
begin
  ClearSigs;
  for I := 0 to pred(Value.Count) do
    if length(Value[I]) > 0 then
    begin
      SigVars := TStringList.Create;
      SigVars.Text := DecomposeSig(Value[I]);
      if SigErrors.Count <> 0 then
      begin
        SigVars.Clear;
        SigVars.AddStrings(SigErrors);
        Signatures.AddObject('// ' + Value[I], SigVars)
      end
      else
        Signatures.AddObject('    ' + Value[I], SigVars)
    end;
end;

function TRemFuncSrcDoc.GetSigVar(IX: integer; VarName: string): string;
begin
  Result := SigVars[IX].Values[VarName]
end;

procedure TRemFuncSrcDoc.SetSigVar(IX: integer; VarName: string; const Value:
  string);
begin
  SigVars[IX].Values[VarName] := Value
end;

function TRemFuncSrcDoc.GetSigVars(IX: integer): TstringList;
begin
  Result := TStringList(Signatures.Objects[IX]);
end;

function TRemFuncSrcDoc.GetFileName: string;
begin
  Result := FQNSource
end;

function TRemFuncSrcDoc.GetTemplateFileName: string;
begin
  Result := formMain.Option[tagTemplateFileName];
  if length(Result) = 0 then
    Result := cDefaultTemplateFileName;
end;

procedure TRemFuncSrcDoc.SetTemplateFileName(const Value: string);
begin
  formMain.Option[tagTemplateFileName] := Value;
end;

function TRemFuncSrcDoc.RtcEnumType(const Typ: string): string;
begin
  Result := 'rtc_' + XRefType(Typ);
end;

function TRemFuncSrcDoc.XRefType(const Typ: string): string;
begin
  Result := typeXRef.Values[Typ];
  if length(Result) = 0 then
    Result := Typ;
  Result[1] := UpCase(Result[1]);
end;

function TRemFuncSrcDoc.RtcConversionType(const Typ: string): string;
begin
  Result := 'as' + XRefType(Typ)
end;

function TRemFuncSrcDoc.ImplementAllSigs: string;
var
  I, Old: integer;
begin
  Old := FActiveSig;
  try
    Result := '';
    for I := 0 to pred(FSigs.Count) do
    begin
      FActiveSig := I;
      // Allow for remmed out signatures
      if FSigs[FActiveSig][1] = '/' then
        Continue;
      Result := Result + ImplementActiveSig + CRLF + CRLF;
    end;
  finally
    FActiveSig := Old
  end
end;

function TRemFuncSrcDoc.ImplementActiveSig: string;
begin
  // Skip remmed out signatures
  if (length(Signatures[ActiveSig]) > 0) and (Signatures[ActiveSig][1] = '/')
    then
    Exit;
  // Return implementation of active sig
  SetInnerTagValues;
  Result := InnerParser.Output;
end;

function TRemFuncSrcDoc.GetTagClassName: string;
begin
  Result := formMain.UnitClassname;
end;

function TRemFuncSrcDoc.GetTagFunctionName: string;
begin
  Result := SigVars[ActiveSig].Values[tagFunctionName]
end;

function TRemFuncSrcDoc.GetTagFunctionParams: string;
var
  Vars: TStrings;
  Nm, Typ, AsTyp: string;
  I: integer;
begin
  Result := '';
  Vars := SigVars[ActiveSig];
  for I := 2 to pred(Vars.Count) do
  begin
    Nm := Vars.Names[I];
    Typ := Vars.Values[Nm];
    AsTyp := RtcConversionType(Typ);
    if I > 2 then
      Result := Result + CRLF + '      ';
    Result := Result + AsTyp + '[''' + Nm + '''] := ' + Nm + ';';
  end
end;

function TRemFuncSrcDoc.GetTagFunctionSignature: string;
begin
  // Leading spaces must be trimmed
  Result := copy(trim(Signatures[ActiveSig]), 10, 1000);
end;

function TRemFuncSrcDoc.GetTagImplement: string;
begin
  //  SetInnerTagValues;
  Result := ImplementAllSigs;
end;

function TRemFuncSrcDoc.GetTagFunctionSignatureList: string;
begin
  Result := Signatures.Text;
end;

function TRemFuncSrcDoc.GetTagUnitName: string;
begin
  Result := formMain.UnitBaseName;
end;

function TRemFuncSrcDoc.GetTagPreserveReturn: string;
begin
  if formMain.chkPreserveReturnedValue.Checked then
    Result := 'Extract;  // Returned object is not destroyed on next call'
  else
    Result := '// NOTE: Returned object will be destroyed on the next call to any function';
end;

function TRemFuncSrcDoc.GetPasReturnType: string;
begin
  Result := SigVars[ActiveSig].Values[cPasReturnType];
end;

function TRemFuncSrcDoc.GetTagNilResult: string;
begin
  if SimpleType(SigVars[ActiveSig].Values[cPasReturnType]) then
    Result := '// Result := NIL'
  else
    Result := 'Result := NIL'
end;

function TRemFuncSrcDoc.GetTagRtcReturnEnum: string;
begin
  Result := RtcEnumType(GetPasReturnType);
end;

function TRemFuncSrcDoc.GetTagRtcReturnAs: string;
begin
  Result := RtcConversionType(GetPasReturnType);
end;

procedure TRemFuncSrcDoc.SetOuterTagValues;
begin
  with OuterParser do
  begin
    Value[tagUnitName] := GetTagUnitName;
    Value[tagClassName] := GetTagClassName;
    Value[tagFunctionSignatureList] := GetTagFunctionSignatureList;
    Value[tagFunctionImplementation] := GetTagImplement;
  end;
end;

procedure TRemFuncSrcDoc.SetInnerTagValues;
begin // These change for different functions
  with InnerParser do
  begin
    Value[tagClassName] := GetTagClassName;
    Value[tagFunctionSignature] := GetTagFunctionSignature;
    Value[tagFunctionName] := GetTagFunctionName;
    Value[tagFunctionParameters] := GetTagFunctionParams;
    Value[tagRtcReturnAs] := GetTagRtcReturnAs;
    Value[tagRtcReturnEnum] := GetTagRtcReturnEnum;
    Value[tagPreserveReturn] := GetTagPreserveReturn;
    Value[tagNilResult] := GetTagNilResult;
  end;
end;

function TRemFuncSrcDoc.Compile: Boolean;
begin
  SetOuterTagValues;
  Result := TRUE;
  try
    Text := OuterParser.Output;
    while not (trim(Strings[Count - 1]) = 'end;') do
      delete(Count - 1);
    Add('');
    Add('end.');
  except on E: Exception do
    begin
      E.Message := 'Compile Failed: ' + E.Message;
      raise
    end
  end
end;

procedure TRemFuncSrcDoc.WriteSigImplementation(const SigImpTxt: TStrings; Sig:string);
begin
  SigImpTxt.Text := Sig;
  Signatures := SigImpTxt;
  FActiveSig := 0;
  SigImpTxt.Text := ImplementActiveSig;
end;

function TRemFuncSrcDoc.SimpleType(Typ: string): boolean;
begin
  Result := pos(uppercase(Typ),'TRTCRECORD TRTCARRAY TRTCDATASET TSTREAM')=0;
end;

function TRemFuncSrcDoc.GetSigErrors: TStringList;
begin
  Result := FSigErrors;
end;

initialization
  // Cross-reference for RTC "as" types
  // Types w/o XRef are taken as-is
  TypeXRef := TStringList.Create;
  TypeXRef.CaseSensitive := FALSE;
  with typeXRef do
  begin
    Add('double=float');
    Add('extended=float');
    Add('real=float');
    Add('Tdatetime=datetime');
    Add('byte=integer');
    Add('word=integer');
    Add('shortint=integer');
    Add('longint=integer');
    Add('smallint=integer');
    Add('int64=largeint');
    Add('string=text');
    Add('UnicodeString=text');
    Add('rtcstring=string');
    Add('rtcwidestring=text');
    Add('ansistring=string');
    Add('shortstring=string');
    Add('TrtcRecord=record');
    Add('TrtcArray=array');
    Add('TrtcDataSet=dataset');
    Add('TStream=bytestream');
  end;
  TypeXRef.Sort;
  TypeXref.Sorted := TRUE;

  // These types are OK for parameters or return types
  OKTypes := TStringList.Create;
  OKTypes.CaseSensitive := FALSE;
  with OKTypes do
  begin
    Add('integer');
    Add('shortint');
    Add('longint');
    Add('smallint');
    Add('byte');
    Add('word');
    Add('int64');
    Add('double');
    Add('extended');
    Add('currency');
    Add('Tdatetime');
    Add('boolean');
    Add('string');
    Add('rtcstring');
    Add('ansistring');
    Add('unicodestring');
    Add('shortstring');
    Add('WideString');
    Add('RtcWideString');
    Add('TrtcRecord');
    Add('TrtcArray');
    Add('TrtcDataSet');
    Add('TStream');
  end;
  OKTypes.Sort;
  OKTypes.Sorted := TRUE;

finalization
  OKtypes.Free;
  TypeXRef.Free;
end.

