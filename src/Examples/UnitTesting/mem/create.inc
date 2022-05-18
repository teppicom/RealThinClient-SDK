Create(TestSize,PoolSize:Cardinal; full_search:boolean; run_now:boolean
      {$IFDEF RTC_GENERIC}; _min:itemType; _nil:infoType{$ENDIF});
  begin
  inherited Create;
  {$IFDEF RTC_GENERIC}
  itemC := TComparer<itemType>.Default;
  infoC := TComparer<infoType>.Default;
  itemMin:=_min;
  infoNil:=_nil;
  {$ENDIF}
  if full_search then
    Log('CreateFS('+IntToStr(TestSize)+')')
  else
    Log('Create('+IntToStr(TestSize)+')');
  randomize;
  FullSearch:=full_search;
  FullPing:=FullSearch;
  MySize:=TestSize;
  MaxPing:=MySize;
  FMyList := TTestClass.Create(PoolSize{$IFDEF RTC_GENERIC},itemMin,infoNil{$ENDIF});
  SetLength(s,MySize+1); // need 1 extra element, so we can start from 1 instead of 0.

  if run_now then Run;
  end