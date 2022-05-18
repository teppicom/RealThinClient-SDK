Destroy;
  begin
  Log('Destroy');
  FMyList.Free;
  FMyList := nil;
  SetLength(s,0);
  inherited;
  end