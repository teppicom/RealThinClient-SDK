unit gmoOptions;
(*
  Save and restore the CHECKED, TEXT and CAPTION properties of controls in a
  container such as a TForm or TTabSheet. Only one property of any control
  is saved/restored, with priority as in the list above.
*)
interface

Uses Classes, SysUtils, Controls, TypInfo;

Procedure OptionsFromString(Data:String; const Parent:TWinControl);
Procedure OptionsFromStringList(Data:TStrings; const Parent:TWinControl);
Procedure OptionsToStringList(const StrLst:TStrings; const Parent:TWinControl);

implementation

const
  cChecked = 'CHECKED';
  cText = 'TEXT';
  cCaption = 'CAPTION';

Procedure OptionsFromString(Data:String; const Parent:TWinControl);
Var Lst:TStringList;
Begin
  Lst := TStringList.Create;
TRY
  Lst.CommaText := Data;
  OptionsFromStringList(Lst,Parent);
FINALLY
  Lst.Free;
END;
End;

Procedure OptionsFromStringList(Data:TStrings; const Parent:TWinControl);
Var I:integer;
    Ctrl:TControl;
    Nm,Dat:String;
Begin
  For I := 0 to pred(Parent.ControlCount) do begin
    Ctrl := Parent.Controls[I];
    Nm := Ctrl.Name;
    Dat := Data.Values[Nm];
    If length(Dat) > 0 then begin
      If IsPublishedProp(Ctrl, cChecked) then
        SetEnumProp(Ctrl,cChecked,Dat)
      Else if IsPublishedProp(Ctrl, cText) then
        SetStrProp(Ctrl,cText,Dat)
      End;
    End
End;

Procedure OptionsToStringList(const StrLst:TStrings; const Parent:TWinControl);
Var I:integer;
    Ctrl:TControl;
Begin // NOTE: contents of StrLst preserved
  For I := 0 to pred(Parent.ControlCount) do begin
    Ctrl := Parent.Controls[I];
    If IsPublishedProp(Ctrl, cChecked) then
      StrLst.Values[Ctrl.Name] := GetEnumProp(Ctrl, cChecked)
    Else if IsPublishedProp(Ctrl, cText) then
      StrLst.Values[Ctrl.Name] := GetStrProp(Ctrl, cText)
    End
End;

end.
