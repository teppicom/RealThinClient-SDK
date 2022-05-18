SearchDown;
  var
    a: Cardinal;
    v2: itemType;
  begin
  v := FMyList.search_min(ReturnValue);
  if FMyList.Count=0 then
    begin
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,itemMin)=0{$ELSE}v=itemMin{$ENDIF},
          'SearchDown: search_min -> Item found in empty list?');
    Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,infoNil)=0{$ELSE}ReturnValue=infoNil{$ENDIF},
          'SearchDown: search_min -> Item found in empty list?');
    end
  else
    begin
    Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,infoNil)<>0{$ELSE}ReturnValue<>infoNil{$ENDIF},
          'SearchDown: search_min -> No min item!');
    for a:=1 to FMyList.Count-1 do
      begin
      Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,FMyList.search(v))=0{$ELSE}ReturnValue=FMyList.search(v){$ENDIF},
            'SearchDown: Search value not found!');
    {$IFDEF RTC_TREE}
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v,FMyList.isearch(ReturnValue))=0{$ELSE}v = FMyList.isearch(ReturnValue){$ENDIF},
            'SearchDown: iSearch value not found!');
    {$ENDIF}

      v2 := FMyList.search_ge(v,ReturnValue);
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v2,v)=0{$ELSE}v2=v{$ENDIF},
            'SearchDown: search_ge -> Item not found!');
      Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,infoNil)<>0{$ELSE}ReturnValue<>infoNil{$ENDIF},
            'SearchDown: search_ge -> No greater item!');

      v2 := FMyList.search_g(v,ReturnValue);
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v2,v)>0{$ELSE}v2>v{$ENDIF},
            'SearchDown: search_g -> Not a greater item!');
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v2,itemMin)<>0{$ELSE}v2<>itemMin{$ENDIF},
            'SearchDown: search_l -> Zero item?');
      Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,infoNil)<>0{$ELSE}ReturnValue<>infoNil{$ENDIF},
            'SearchDown: search_g -> No greater item!');

      v := v2;
      end;
    Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,FMyList.search(v))=0{$ELSE}ReturnValue=FMyList.search(v){$ENDIF},
          'SearchDown: Search value not found!');
  {$IFDEF RTC_TREE}
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,FMyList.isearch(ReturnValue))=0{$ELSE}v = FMyList.isearch(ReturnValue){$ENDIF},
          'SearchDown: iSearch value not found!');
  {$ENDIF}

    v2 := FMyList.search_max(ReturnValue);
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,v2)=0{$ELSE}v=v2{$ENDIF},
          'SearchDown: search_max <> last item?');
    Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,infoNil)<>0{$ELSE}ReturnValue<>infoNil{$ENDIF},
          'SearchDown: search_max -> No max item!');
    end;
  end