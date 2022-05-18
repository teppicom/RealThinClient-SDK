RemoveAllDown;
  var
    a, b: Cardinal;
  begin
  Log('RemoveAllDown');
  // removing items one by one ...
  for a:=1 to MySize do
    begin
    v := s[a];
    SetReturnValue(a);

    Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,FMyList.search(v))=0{$ELSE}ReturnValue=FMyList.search(v){$ENDIF},
          'Search value not found before delete!');
  {$IFDEF RTC_TREE}
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,FMyList.isearch(ReturnValue))=0{$ELSE}v=FMyList.isearch(ReturnValue){$ENDIF},
          'iSearch value not found before delete!');
  {$ENDIF}

    FMyList.remove(v);
    Check({$IFDEF RTC_GENERIC}infoC.Compare(FMyList.search(v),infoNil)=0{$ELSE}FMyList.search(v)=infoNil{$ENDIF},
          'search: Value found after delete!');
  {$IFDEF RTC_TREE}
    Check({$IFDEF RTC_GENERIC}itemC.Compare(FMyList.isearch(ReturnValue),itemMin)=0{$ELSE}FMyList.isearch(ReturnValue)=itemMin{$ENDIF},
          'isearch: Value found after delete!');
  {$ENDIF}

    Check(FMyList.Count=Cardinal(MySize)-a,'Count mismatch after delete!');

    if FullSearch then
      begin
      for b:=1 to a do
        begin
        v := s[b];
        SetReturnValue(b);
        Check({$IFDEF RTC_GENERIC}infoC.Compare(infoNil,FMyList.search(v))=0{$ELSE}infoNil=FMyList.search(v){$ENDIF},
              'Search value found after delete!');
      {$IFDEF RTC_TREE}
        Check({$IFDEF RTC_GENERIC}itemC.Compare(itemMin,FMyList.isearch(ReturnValue))=0{$ELSE}itemMin = FMyList.isearch(ReturnValue){$ENDIF},
              'Search value found after delete!');
      {$ENDIF}
        end;
      for b:=a+1 to MySize do
        begin
        v := s[b];
        SetReturnValue(b);
        Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,FMyList.search(v))=0{$ELSE}ReturnValue=FMyList.search(v){$ENDIF},
              'Search value not found after delete!');
      {$IFDEF RTC_TREE}
        Check({$IFDEF RTC_GENERIC}itemC.Compare(v,FMyList.isearch(ReturnValue))=0{$ELSE}v = FMyList.isearch(ReturnValue){$ENDIF},
              'iSearch value not found after delete!');
      {$ENDIF}
        end;
      SearchAll;
      end;
    Ping(a);
    end;

  if not FullSearch then
    begin
    Log('CheckRemoved');
    a:=MySize;
    for b:=1 to a do
      begin
      v := s[b];
      SetReturnValue(b);
      Check({$IFDEF RTC_GENERIC}infoC.Compare(infoNil,FMyList.search(v))=0{$ELSE}infoNil=FMyList.search(v){$ENDIF},
            'Search value found after delete!');
    {$IFDEF RTC_TREE}
      Check({$IFDEF RTC_GENERIC}itemC.Compare(itemMin,FMyList.isearch(ReturnValue))=0{$ELSE}itemMin = FMyList.isearch(ReturnValue){$ENDIF},
            'Search value found after delete!');
    {$ENDIF}
      end;
    for b:=a+1 to MySize do
      begin
      v := s[b];
      SetReturnValue(b);
      Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,FMyList.search(v))=0{$ELSE}ReturnValue=FMyList.search(v){$ENDIF},
            'Search value not found after delete!');
    {$IFDEF RTC_TREE}
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v,FMyList.isearch(ReturnValue))=0{$ELSE}v = FMyList.isearch(ReturnValue){$ENDIF},
            'iSearch value not found after delete!');
    {$ENDIF}
      end;
    SearchAll;
    end;
  CheckEmpty;
  end