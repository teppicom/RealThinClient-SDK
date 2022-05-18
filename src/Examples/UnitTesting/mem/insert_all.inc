InsertAll(typ:byte);
  var
    a: Cardinal;
  begin
  MyType:=typ;
  case typ of
    1:Log('InsertAllDown');
    2:Log('InsertAllUp');
    else Log('InsertAllRandom');
    end;

  Check(FMyList.Count=0, 'Count mismatch before Test!');
  SearchAll;

  // adding items in random order ...
  for a:=1 to MySize do
    begin
    SetReturnValue(a);
    v:=GetItemValue(a);
    s[a]:=v;

    FMyList.insert(v, ReturnValue);
    Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,FMyList.search(v))=0{$ELSE}ReturnValue=FMyList.search(v){$ENDIF},
          'search: Inserted value not found!');
  {$IFDEF RTC_TREE}
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,FMyList.isearch(ReturnValue))=0{$ELSE}v = FMyList.isearch(ReturnValue){$ENDIF},
          'isearch: Inserted value not found!');
  {$ENDIF}

    Check(FMyList.Count=a, 'Count mismatch after insert!');

    if FullSearch then
      SearchAll;
    Ping(a);
    end;
  if not FullSearch then
    begin
    Log('CheckInserted');
    SearchAll;
    end;
  end
