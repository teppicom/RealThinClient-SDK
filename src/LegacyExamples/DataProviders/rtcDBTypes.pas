{
  This unit is NOT used by the RealThinClient SDK nor by any RTC SDK examples!

  It is provided ONLY as an example on how data *could* be copied from TRtcDataSet to a TDataSet
  and could be used as a starting point if you want to write your own functions for this purpose.
}
unit rtcDBTypes;

{$include rtcDefs.inc}

interface

uses
  SysUtils, DB, DBClient,
  
  rtcDB, rtcInfo;

// Can be used to copy data (this code only supports native types, no blobs) from a RTC Dataset to a TDataSet
procedure RtcDataSetToDelphi(rtcDS:TRtcDataSet; DelphiDS:TDataSet; ClearFieldDefs:boolean=True);


{ NOTE: "DelphiDataSetToRtc" function was moved to the "rtcDB.pas" unit in the "Lib" folder. }

implementation

procedure RtcDataSetToDelphi(rtcDS:TRtcDataSet; DelphiDS:TDataSet; ClearFieldDefs:boolean=True);
  var
    flds:integer;
    fldname:string;
    field:TField;
  begin
  if ClearFieldDefs then
    begin
    DelphiDS.Active:=False;
    DelphiDS.FieldDefs.Clear;
    for flds:=0 to rtcDS.FieldCount-1 do
      begin
      fldname:=rtcDS.FieldName[flds];
      DelphiDS.FieldDefs.Add(fldname,
                             RTC_FIELD2DB_TYPE(rtcDS.FieldType[fldname]),
                             rtcDS.FieldSize[fldname],
                             rtcDS.FieldRequired[fldname]);
      end;
    if DelphiDS is TClientDataSet then
      TClientDataSet(DelphiDS).CreateDataSet;
    end;

  if not DelphiDS.Active then
    DelphiDS.Active:=True;

  rtcDS.First;
  while not rtcDS.EOF do
    begin
    DelphiDS.Append;
    for flds:=0 to rtcDS.FieldCount-1 do
      begin
      fldname:=rtcDS.FieldName[flds];
      field:=delphiDS.FindField(fldname);
      if assigned(field) then
        field.Value:=rtcDS.Value[fldname];
      end;
    DelphiDS.Post;
    rtcDS.Next;
    end;
  end;

end.
