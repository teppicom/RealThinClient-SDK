SearchUp;
  var
    a: Cardinal;
    v2: itemType;
  begin
  v := FMyList.search_max(ReturnValue);
  if FMyList.Count=0 then
    begin
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,itemMin)=0{$ELSE}v=itemMin{$ENDIF},
          'SearchUp: search_min -> Item found in empty list?');
    Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,infoNil)=0{$ELSE}ReturnValue=infoNil{$ENDIF},
          'SearchUp: search_min -> Item found in empty list?');
    end
  else
    begin
    Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,infoNil)<>0{$ELSE}ReturnValue<>infoNil{$ENDIF},
          'SearchUp: search_max -> No max item!');
    for a:=1 to FMyList.Count-1 do
      begin
      v2 := FMyList.search_le(v,ReturnValue);
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v2,v)=0{$ELSE}v2=v{$ENDIF},
            'SearchUp: search_le -> Item not found!');
      Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,infoNil)<>0{$ELSE}ReturnValue<>infoNil{$ENDIF},
            'SearchUp: search_le -> No lower item!');

      v2 := FMyList.search_l(v,ReturnValue);
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v2,v)<0{$ELSE}v2<v{$ENDIF},
            'SearchUp: search_l -> Not a lower item!');
      Check({$IFDEF RTC_GENERIC}itemC.Compare(v2,itemMin)<>0{$ELSE}v2<>itemMin{$ENDIF},
            'SearchUp: search_l -> Zero item?');
      Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,infoNil)<>0{$ELSE}ReturnValue<>infoNil{$ENDIF},
            'SearchUp: search_l -> No lower item!');

      v:=v2;
      end;
    v2 := FMyList.search_min(ReturnValue);
    Check({$IFDEF RTC_GENERIC}itemC.Compare(v,v2)=0{$ELSE}v=v2{$ENDIF},
          'SearchUp: search_min <> first item?');
    Check({$IFDEF RTC_GENERIC}infoC.Compare(ReturnValue,infoNil)<>0{$ELSE}ReturnValue<>infoNil{$ENDIF},
          'SearchUp: search_min -> No min item!');
    end;
  end