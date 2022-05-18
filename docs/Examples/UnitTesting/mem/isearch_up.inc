iSearchUp;
  var
    a: Cardinal;
    i, i2: infoType;
  begin
  i := FMyList.isearch_max(v);
  if FMyList.Count=0 then
    begin
    Check({$IFDEF RTC_GENERIC}infoC.Compare(i,infoNil)=0{$ELSE}i=infoNil{$ENDIF}, 'iSearchUp: isearch_max -> Item found in empty list?');
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,itemMin)=0{$ELSE}v=itemMin{$ENDIF}, 'iSearchUp: isearch_max -> Item found in empty list?');
    end
  else
    begin
    Check({$IFDEF RTC_GENERIC}infoC.Compare(i,infoNil)>0{$ELSE}i>infoNil{$ENDIF}, 'iSearchUp: isearch_max -> No max item!');
    for a:=1 to FMyList.Count-1 do
      begin
      i2 := FMyList.isearch_le(i,v);
      Check({$IFDEF RTC_GENERIC}infoC.Compare(i2,i)=0{$ELSE}i2=i{$ENDIF}, 'iSearchUp: isearch_le -> Item not found!');
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v,itemMin)>0{$ELSE}v>itemMin{$ENDIF}, 'iSearchUp: isearch_le -> No lower item!');

      i2 := FMyList.isearch_l(i,v);
      Check({$IFDEF RTC_GENERIC}infoC.Compare(i2,i)<0{$ELSE}i2<i{$ENDIF}, 'iSearchUp: isearch_l -> Not a lower item!');
      Check({$IFDEF RTC_GENERIC}infoC.Compare(i2,infoNil)>0{$ELSE}i2>infoNil{$ENDIF}, 'iSearchUp: isearch_l -> Zero item?');
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v,itemMin)>0{$ELSE}v>itemMin{$ENDIF}, 'iSearchUp: isearch_l -> No lower item!');

      i := i2;
      end;
    i2 := FMyList.isearch_min(v);
    Check({$IFDEF RTC_GENERIC}infoC.Compare(i,i2)=0{$ELSE}i=i2{$ENDIF}, 'iSearchUp: isearch_min <> first item?');
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,itemMin)>0{$ELSE}v>itemMin{$ENDIF}, 'iSearchUp: search_min -> No min item!');
    end;
  end