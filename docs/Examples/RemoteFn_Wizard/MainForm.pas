unit MainForm;
(*
  Programmer: Glynn Owen
  Created: 1/24/07
  Copyright 2007 by Glynn Owen

CHANGE LOG
  02/23/07 - fixed issue reported by Ed Blanchard. Closing after initial
  start without opening a file caused a situation that required zapping
  the OPT file as a workaround.
*)
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, SigFuncs;

const
  App_StartNewFunction = WM_USER + 300;

type

  TSourceFileType = (sfNone, sfNew, sfExisting);

  TformMain = class(TForm)
    dlgOpen: TOpenDialog;
    btnNewFile: TButton;
    pnlSig: TPanel;
    edtSig: TEdit;
    Label5: TLabel;
    pnlRight: TPanel;
    mmoSigVars: TMemo;
    VSplit: TSplitter;
    mmoCode: TMemo;
    pnlLeft: TPanel;
    lbxSignatures: TListBox;
    HSplit: TSplitter;
    popFuncList: TPopupMenu;
    miNewSig: TMenuItem;
    dlgFont: TFontDialog;
    miDelete: TMenuItem;
    btnSaveToTmp: TButton;
    btnSaveToPAS: TButton;
    SaveTMPFile1: TMenuItem;
    SavePASfile1: TMenuItem;
    N1: TMenuItem;
    btnSetFont: TButton;
    chkFuncLbx: TCheckBox;
    chkFuncVarMmo: TCheckBox;
    chkFuncCodeMmo: TCheckBox;
    chkFuncSigEdt: TCheckBox;
    chkDiscardOptions: TCheckBox;
    chkAsTmp: TCheckBox;
    chkAsPas: TCheckBox;
    chkAutoSave: TCheckBox;
    chkPreserveReturnedValue: TCheckBox;
    btnAddFunction: TButton;
    btnDeleteFunction: TButton;
    btnEditFuncs: TButton;
    chkNewOnEnter: TCheckBox;
    procedure lbxSignaturesClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNewFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtSigChange(Sender: TObject);
    procedure miNewSigClick(Sender: TObject);
    procedure btnSetFontClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure chkAsPasClick(Sender: TObject);
    procedure chkAutoSaveClick(Sender: TObject);
    procedure btnSaveToTmpClick(Sender: TObject);
    procedure btnSaveToPASClick(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure btnEditFuncsClick(Sender: TObject);
    procedure edtSigKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FSrcDoc: TRemFuncSrcDoc;
    FileStatus: TSourceFileType;
    procedure AddSig;
    procedure UpdateFonts;
    procedure UpdateWindowDisplay;
    procedure SaveWindowSettings;
    procedure DoChangeFont(Fnt: TFont);
    function GetUnitBasename: string;
    function GetUnitFilename: string;
    function GetUnitPath: string;
    function GetUnitClassname: string;
    function DoOpenFile(Typ: TSourceFileType; NameChosen: boolean = FALSE):
      boolean;
    function GetOption(Name: string): string;
    procedure SetOption(Name: string; const Value: string);
    procedure SetUnitClassName(const Value: string);
    procedure SetUpIsIncomplete;
    procedure AppStartNewFunction(var Msg: TMessage); message
      App_StartNewFunction;
    procedure ClearAllControls;
  published
    Pages: TPageControl;
    tabSetup: TTabSheet;
    tabFunctions: TTabSheet;
    edtUnitFileName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtUnitPath: TEdit;
    btnOpenFile: TButton;
    Label3: TLabel;
    edtClassName: TEdit;
    procedure btnOpenFileClick(Sender: TObject);
  private
    { Private declarations }
//    FUnitFile,FUnitDir,FUnitClass:string;
    function GetCurrentSig: string;
    procedure SetCurrentSig(const Value: string);
    procedure SetDefaultOptions;
    procedure UpdateOptions;
    procedure UpdateOptionControls;
    function DstFileName(TempFile: Boolean = TRUE): string;
    procedure SaveFile(const FileName: string);
  public
    { Public declarations }
    function SetupValid: boolean;
    // Class Name for unit
    property Option[Name: string]: string read GetOption write SetOption;
    property UnitClassname: string read GetUnitClassname write SetUnitClassName;
    // Path to unit file
    property UnitPath: string read GetUnitPath;
    // Filename without path
    property UnitFilename: string read GetUnitFilename;
    // Filename without extent
    property UnitBaseName: string read GetUnitBasename;
    property CurrentSig: string read GetCurrentSig write SetCurrentSig;
  end;

var
  formMain: TformMain;

implementation

{$R *.dfm}

uses gmoOptions;

const
  cNoDupes = 'Duplicates are not allowed';

  cFontName = 'FontName';
  cFontSize = 'FontSize';
  cFontColor = 'FontColor';

  cWndoLeft = 'WndoLeft';
  cWndoTop = 'WndoTop';
  cWndoHeight = 'WndoHeight';
  cWndoWidth = 'WndoWidth';
  cWndoState = 'WndoState';
  cHSplit = 'HSplit';
  cVSplit = 'VSplit';
  cWorkFile = 'WorkFile';

var
  Options: TStringList;
  OptionsFileName: string;

  // Comma-delimited list of default options
var
  DefaultOptions: string =
  'chkPreserveReturnedValue=TRUE,chkAutoSave=TRUE,chkClassIsUnitFileName';

procedure TformMain.SetDefaultOptions;
begin
  OptionsFromString(DefaultOptions, tabSetup);
end;

procedure TformMain.UpdateOptions;
begin
  OptionsToStringList(Options, tabSetup);
end;

procedure TformMain.UpdateOptionControls;
begin
  OptionsFromStringList(Options, tabSetup);
end;

function TformMain.GetCurrentSig: string;
begin
  Result := edtSig.Text;
end;

procedure TformMain.SetCurrentSig(const Value: string);
begin
  edtSig.Text := Value
end;

procedure TformMain.lbxSignaturesClick(Sender: TObject);
var
  Sig: string;
begin
  mmoSigVars.Text := '';

  with lbxSignatures do
    if ItemIndex < 0 then
    begin
      CurrentSig := '';
    end
    else
    begin
      Sig := Items[ItemIndex];
      if FSrcDoc.ForceTerminator(Sig) then
        Items[ItemIndex] := Sig;
      CurrentSig := Sig;
      try
        mmoSigVars.Text := FSrcDoc.DecomposeSig(Sig);
      finally
        if FSrcDoc.SigErrors.Count > 0 then
          mmoSigVars.Text := FSrcDoc.SigErrors.Text
      end;
    end;
end;

procedure TformMain.btnReplaceClick(Sender: TObject);
var
  IX: integer;
begin
  with lbxSignatures do
  begin
    IX := Items.IndexOf(CurrentSig);
    if IX >= 0 then
      if IX = ItemIndex then
        Exit // Replacing same text at same position
      else
        ShowMessage(cNoDupes)
    else
      Items[ItemIndex] := CurrentSig;
    OnClick(self)
  end;
end;

procedure TformMain.btnNewClick(Sender: TObject);
var
  IX: integer;
  NewSig: string;
begin
  NewSig := CurrentSig;
  with lbxSignatures do
  begin
    IX := Items.IndexOf(NewSig);
    if IX < 0 then
    begin
      Items.Add(CurrentSig);
      ItemIndex := Items.Count - 1;
    end
    else
      ShowMessage(cNoDupes);
    OnClick(self);
  end
end;

procedure TformMain.FormCreate(Sender: TObject);
begin
  Pages.ActivePage := tabSetup;
  Options := TStringList.Create;
  OptionsFileName := changeFileExt(application.ExeName, '.opt');
  if FileExists(OptionsFileName) then
    Options.LoadFromFile(OptionsFileName);
  if Options.Count = 0 then
    SetDefaultOptions;
  UpdateOptionControls;
  UpdateFonts;
  UpdateWindowDisplay;
end;

procedure TformMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if SetupValid and chkAutoSave.Checked then
  begin
    FSrcDoc.Signatures := lbxSignatures.Items;
    FSrcDoc.Compile;
    if chkAsPas.Checked then
      btnSaveToPas.Click
    else if chkAsTmp.Checked then
      btnSaveToTmp.Click;
  end;
  FSrcDoc.Free;
  if chkDiscardOptions.Checked then
  begin
    DeleteFile(OptionsFileName);
    Options.Free;
    Exit;
  end;
  SaveWindowSettings;
  UpdateOptions;
  Options.SaveToFile(OptionsFileName);
  Options.Free;
  OptionsFileName := '';
end;

function TformMain.SetupValid: boolean;
begin
  Result := (length(UnitFilename) > 0)
    and (length(UnitPath) > 0)
    and (length(UnitClassname) > 0);
end;

function TformMain.GetUnitBasename: string;
begin
  Result := ChangeFileExt(UnitFileName, '');
end;

function TformMain.GetUnitFilename: string;
begin
  Result := edtUnitFileName.Text;
end;

function TformMain.GetUnitPath: string;
begin
  Result := edtUnitPath.Text
end;

function TformMain.GetUnitClassname: string;
begin
  Result := trim(edtClassName.Text);
  if length(Result) = 0 then
  begin
    Result := 'T' + UnitBaseName;
    edtClassName.Text := Result;
  end
end;

function TformMain.DoOpenFile(Typ: TSourceFileType; NameChosen: boolean):
  boolean;
begin
  Result := TRUE;
  if NameChosen or dlgOpen.Execute then
  begin
    ClearAllControls;
    FileStatus := Typ;
    edtUnitFileName.Text := extractFileName(dlgOpen.FileName);
    edtUnitPath.Text := extractFilePath(dlgOpen.FileName);
    // if a "new file" exists, it will be treated as an existing file
    if fileExists(dlgOpen.FileName) then
    begin
      try
        FileStatus := sfExisting;
        // Existing files must have been created by this app
        // Others will be rejected
        FSrcDoc.GetFunctionListFromSource(lbxSignatures.Items);
      except on Exception do
        begin
          FileStatus := sfNone;
          edtUnitFileName.Text := '';
          edtUnitPath.Text := '';
          raise
        end
      end;
    end;
  end;
  // Else nothing changes
end;

procedure TformMain.btnNewFileClick(Sender: TObject);
begin
  dlgOpen.Options := dlgOpen.Options - [ofPathMustExist, ofFileMustExist];
  edtClassName.Text := '';
  DoOpenFile(sfNew);
  if FileStatus = sfNew then
    GetUnitClassName;
end;

procedure TformMain.btnOpenFileClick(Sender: TObject);
begin
  dlgOpen.Options := dlgOpen.Options + [ofPathMustExist, ofFileMustExist];
  DoOpenFile(sfExisting);
end;

function TformMain.GetOption(Name: string): string;
begin
  Result := Options.Values[Name];
end;

procedure TformMain.SetOption(Name: string; const Value: string);
begin
  Options.Values[Name] := Value;
end;

procedure TformMain.FormShow(Sender: TObject);
begin
  FSrcDoc := TRemFuncSrcDoc.Create;
end;

procedure TformMain.SetUnitClassName(const Value: string);
begin
  edtClassName.Text := Value
end;

procedure TformMain.edtSigChange(Sender: TObject);
begin
  with lbxSignatures do
    Items[ItemIndex] := edtSig.Text;
  if length(edtSig.Text) = 0 then
    Exit;
  FSrcDoc.WriteSigImplementation(mmoCode.Lines, edtSig.Text);
  if FSrcDoc.SigErrors.Count > 0 then
  begin
    mmoCode.Clear;
    mmoSigVars.Text := FSrcDoc.SigErrors.Text;
  end
  else
    mmoSigVars.Text := FSrcDoc.SigVars[0].Text;
end;

procedure TformMain.AddSig;
begin
  with lbxSignatures do
  begin
    AddItem('function ', nil);
    ItemIndex := pred(Count);
    lbxSignaturesClick(lbxSignatures);
  end
end;

procedure TformMain.miNewSigClick(Sender: TObject);
begin
  AddSig;
  with edtSig do
  begin
    SetFocus;
    Perform(WM_KEYDOWN, VK_END, 0);
    Perform(WM_KEYDOWN, VK_LEFT, 0)
  end;
end;

procedure TformMain.DoChangeFont(Fnt: TFont);
begin
  with Options do
  begin
    Values[cFontName] := Fnt.Name;
    Values[cFontSize] := intToStr(Fnt.Size);
    Values[cFontColor] := intToStr(Fnt.Color);
  end;
  if chkFuncVarMmo.Checked then
    mmoSigVars.Font := dlgFont.Font;
  if chkFuncCodeMmo.Checked then
    mmoCode.Font := dlgFont.Font;
  if chkFuncSigEdt.Checked then
    edtSig.Font := dlgFont.Font;
  if chkFuncLbx.Checked then
    lbxSignatures.Font := dlgFont.Font;
end;

procedure TformMain.btnSetFontClick(Sender: TObject);
begin
  dlgFont.Font := mmoSigVars.Font;
  if dlgFont.Execute then
    DoChangeFont(dlgFont.Font)
end;

procedure TformMain.UpdateFonts;
var
  Nm, Sz, Clr: string;
begin
  Nm := Options.Values[cFontName];
  Sz := Options.Values[cFontSize];
  Clr := Options.Values[cFontColor];
  if (length(Nm) > 0) and (length(Sz) > 0) and (length(Clr) > 0) then
  begin
    with dlgFont.Font do
    begin
      Name := Nm;
      Size := strToint(Sz);
      Color := strToInt(Clr);
    end;
    DoChangeFont(dlgFont.Font);
  end;
end;

procedure TformMain.SaveWindowSettings;
begin
  with Options do
  begin
    Values[cWndoState] := intToStr(ord(WindowState));
    Values[cHSplit] := inttoStr(pnlRight.Width);
    Values[cVSplit] := inttoStr(mmoSigVars.Height);
    Values[cWorkFile] := FSrcDoc.FQNSource;
    Values[cWndoLeft] := intToStr(Left);
    Values[cWndoTop] := intToStr(Top);
    Values[cWndoHeight] := intToStr(Height);
    Values[cWndoWidth] := intToStr(Width);
  end;
end;

procedure TformMain.UpdateWindowDisplay;
var
  L, T, H, W, S, SH, SV, FN: string;
begin
  with Options do
  begin
    L := Values[cWndoLeft];
    T := Values[cWndoTop];
    H := Values[cWndoHeight];
    W := Values[cWndoWidth];
    S := Values[cWndoState];
    SH := Values[cHSplit];
    SV := Values[cVSplit];
    FN := Values[cWorkFile];
  end;
  if length(L) > 0 then
  begin
    Left := strToInt(L);
    Top := strToInt(T);
    Height := strToInt(H);
    Width := strToInt(W);
    WindowState := TWindowState(strToInt(S));
    pnlRight.Width := strToInt(SH);
    mmoSigVars.Height := strToInt(SV);
    with dlgOpen do
    begin
      FileName := FN;
      InitialDir := extractFilePath(FN);
      DoOpenFile(sfExisting, TRUE);
    end;
  end
end;

procedure TformMain.miDeleteClick(Sender: TObject);
begin
  with lbxSignatures do
  begin
    if ItemIndex >= 0 then
    begin
      CurrentSig := '';
      Items.Delete(ItemIndex);
      ItemIndex := -1;
      mmoCode.Clear;
      mmoSigVars.Clear;
    end;
    OnClick(self);
  end
end;

procedure TformMain.chkAsPasClick(Sender: TObject);
var
  Other: TCheckBox;
begin
  if Sender = chkAsPas then
    Other := chkAsTmp
  else
    Other := chkAsPas;
  // Avoid recursive calls
  Other.OnClick := nil;
  Other.Checked := not (Sender as TCheckBox).Checked;
  Other.OnClick := chkAsPasClick;
end;

procedure TformMain.chkAutoSaveClick(Sender: TObject);
begin
  chkAsPas.Enabled := chkAutoSave.Checked;
  chkAsTmp.Enabled := chkAutoSave.Checked;
end;

function TformMain.DstFileName(TempFile: Boolean): string;
begin
  Result := FSrcDoc.FQNSource;
  if TempFile then
    Result := changeFileExt(Result, '.tmp')
  else
    Result := changeFileExt(Result, '.pas');
  // extent may have changed from PAS -> TMP or vice-versa
  edtUnitFileName.Text := extractFileName(Result);
end;

procedure TformMain.SaveFile(const FileName: string);
begin
  if SetupValid then
    with FSrcDoc do
    begin
      Signatures := lbxSignatures.Items;
      Compile;
      SaveToFile(FileName);
    end
  else
    SetUpIsIncomplete;
end;

procedure TformMain.btnSaveToTmpClick(Sender: TObject);
begin
  SaveFile(DstFileName(TRUE));
end;

procedure TformMain.btnSaveToPASClick(Sender: TObject);
begin
  SaveFile(DstFileName(FALSE));
end;

procedure TformMain.SetUpIsIncomplete;
begin
  ShowMessage('Setup is not complete')
end;

procedure TformMain.PagesChange(Sender: TObject);
begin
  if Pages.ActivePage=tabFunctions then
    if SetupValid then
      if lbxSignatures.ItemIndex >= 0
        then lbxSignatures.OnClick(self)
        else if lbxSignatures.Count = 0
          then btnAddFunction.Click
          else begin
            lbxSignatures.ItemIndex := 0;
            lbxSignatures.OnClick(self)
            End
    else begin
      SetupIsIncomplete;
      Pages.ActivePage := tabSetup;
      end;
end;

procedure TformMain.btnEditFuncsClick(Sender: TObject);
begin
  if SetupValid then
    Pages.ActivePage := tabFunctions
  else
    SetupIsIncomplete;
end;

procedure TformMain.edtSigKeyDown(Sender: TObject;
  var Key: Word;
  Shift: TShiftState);
begin
  if (Key = vk_Return) and (Shift = []) then
    if chkNewOnEnter.Checked then
      PostMessage(Handle, App_StartNewFunction, 0, 0);
end;

procedure TformMain.AppStartNewFunction(var Msg: TMessage);
begin
  miNewSig.Click;
end;

procedure TformMain.ClearAllControls;
begin
  edtUnitFilename.Text := '';
  edtUnitPath.Text := '';
  edtClassname.Text := '';
  edtSig.Text := '';
  lbxSignatures.Clear;
  mmoSigVars.Clear;
  mmoCode.Clear;
end;

end.

