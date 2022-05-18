RemoveiSearchDown;
  var
    a: Cardinal;
  begin
  Log('RemoveiSearchDown');
  // removing items one by one ...
  for a:=1 to MySize do
    begin
    ReturnValue := FMyList.isearch_min(v);

    Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,infoNil)<>0{$ELSE}ReturnValue<>infoNil{$ENDIF}, 'No more items?');
    Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,FMyList.search(v))=0{$ELSE}ReturnValue = FMyList.search(v){$ENDIF}, 'Search value not found before delete!');
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,FMyList.isearch(ReturnValue))=0{$ELSE}v = FMyList.isearch(ReturnValue){$ENDIF}, 'iSearch value not found before delete!');

    FMyList.remove(v);
    Check({$IFDEF RTC_GENERIC}infoC.Compare(FMyList.search(v),infoNil)=0{$ELSE}FMyList.search(v)=infoNil{$ENDIF},'search: Value found after delete!');
    Check({$IFDEF RTC_GENERIC}itemC.Compare(FMyList.isearch(ReturnValue),itemMin)=0{$ELSE}FMyList.isearch(ReturnValue)=itemMin{$ENDIF},'isearch: Value found after delete!');
    Check(FMyList.Count=Cardinal(MySize)-a,'Count mismatch after delete!');

    if FullSearch then
      SearchAll;
    Ping(a);
    end;

  if not FullSearch then
    SearchAll;
  CheckEmpty;
  end