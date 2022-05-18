iSearchDown;
  var
    a: Cardinal;
    i, i2: infoType;
  begin
  i := FMyList.isearch_min(v);
  if FMyList.Count=0 then
    begin
    Check({$IFDEF RTC_GENERIC}infoC.Compare(i,infoNil)=0{$ELSE}i=infoNil{$ENDIF}, 'iSearchDown: isearch_min -> Item found in empty list?');
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,itemMin)=0{$ELSE}v=itemMin{$ENDIF}, 'iSearchDown: isearch_min -> Item found in empty list?');
    end
  else
    begin
    Check({$IFDEF RTC_GENERIC}infoC.Compare(i,infoNil)>0{$ELSE}i>infoNil{$ENDIF}, 'iSearchDown: isearch_min -> No min item!');
    for a:=1 to FMyList.Count-1 do
      begin
      Check({$IFDEF RTC_GENERIC}infoC.Compare(i,FMyList.search(v))=0{$ELSE}i = FMyList.search(v){$ENDIF},'iSearchDown: Search value not found!');
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v,FMyList.isearch(i))=0{$ELSE}v = FMyList.isearch(i){$ENDIF},'iSearchDown: iSearch value not found!');

      i2 := FMyList.isearch_ge(i,v);
      Check({$IFDEF RTC_GENERIC}infoC.Compare(i2,i)=0{$ELSE}i2=i{$ENDIF}, 'iSearchDown: isearch_ge -> Item not found!');
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v,itemMin)<>0{$ELSE}v<>itemMin{$ENDIF}, 'iSearchDown: isearch_ge -> No greater item!');

      i2 := FMyList.isearch_g(i,v);
      Check({$IFDEF RTC_GENERIC}infoC.Compare(i2,i)>0{$ELSE}i2>i{$ENDIF}, 'iSearchDown: isearch_g -> Not a greater item!');
      Check({$IFDEF RTC_GENERIC}infoC.Compare(i2,infoNil)<>0{$ELSE}i2<>infoNil{$ENDIF}, 'iSearchDown: isearch_l -> Zero item?');
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v,itemMin)<>0{$ELSE}v<>itemMin{$ENDIF}, 'iSearchDown: isearch_g -> No greater item!');

      i := i2;
      end;
    Check({$IFDEF RTC_GENERIC}infoC.Compare(i,FMyList.search(v))=0{$ELSE}i = FMyList.search(v){$ENDIF},'iSearchDown: Search value not found!');
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,FMyList.isearch(i))=0{$ELSE}v = FMyList.isearch(i){$ENDIF},'iSearchDown: iSearch value not found!');

    i2 := FMyList.isearch_max(v);
    Check({$IFDEF RTC_GENERIC}infoC.Compare(i,i2)=0{$ELSE}i=i2{$ENDIF}, 'iSearchDown: isearch_max <> last item?');
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,itemMin)<>0{$ELSE}v<>itemMin{$ENDIF}, 'iSearchDown: isearch_max -> No max item!');
    end;
  end