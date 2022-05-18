Run;
  var
    a:byte;
  begin
  try
    for a:=1 to 3 do
      begin
      InsertAll(a); RemoveAll;
      InsertAll(a); RemoveAllUp;
      InsertAll(a); RemoveAllDown;
      InsertAll(a); RemoveSearchUp;
      InsertAll(a); RemoveSearchDown;
    {$IFDEF RTC_TREE}
      InsertAll(a); RemoveiSearchUp;
      InsertAll(a); RemoveiSearchDown;
    {$ENDIF}
      end;
  except
    on E:Exception do
      raise Exception.Create(E.ClassName+':'+E.Message+#13#10+LastLine+#13#10);
  end;
  end
