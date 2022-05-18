{
  IDE integration support
  - Copyright 2004-2019 (c) Teppi Technology (https://rtc.teppi.net)
  @exclude
}
{ @exclude }
unit rtcEditors;

{$INCLUDE rtcDefs.inc}

interface

{$IFDEF WIN32}

uses
  {$IFDEF IDE_6up}
  DesignEditors, TypInfo,
  {$ELSE}
  DsgnIntf, TypInfo,
  {$ENDIF}

  Classes, rtcTypes;

type
  TRtcSimpleAddStringList = class(TStringList)
    public 
      procedure SimpleAdd(const s: string); 
    end; 

  TRtcInterfacedComponentProperty = class(TComponentProperty) 
    public 
      function GetIID: TGUID; virtual; abstract; 
      procedure GetValues(Proc: TGetStrProc); override; 
      procedure SetValue(const Value: string); override; 
    end;

{$ENDIF}

implementation

{$IFDEF WIN32}

procedure TRtcSimpleAddStringList.SimpleAdd(const s: string);
  begin
  Add(s);
  end;

procedure TRtcInterfacedComponentProperty.GetValues(Proc: TGetStrProc);
  var
    sl        : TRtcSimpleAddStringList;
    i         : Integer; 
    Component : TComponent; 
    Unknown   : IUnknown; 
  begin 
  sl := TRtcSimpleAddStringList.Create; 
  try 
    inherited GetValues(sl.SimpleAdd); 
    for i := 0 to Pred(sl.Count) do 
      begin 
      Component := Designer.GetComponent(sl[i]); 
      if Assigned(Component) and Component.GetInterface(GetIID, Unknown) then 
        Proc(sl[i]); 
      Unknown := nil; 
      end; 
  finally 
    sl.Free; 
    end; 
  end; 

procedure TRtcInterfacedComponentProperty.SetValue(const Value: string); 
  var 
    Component : TComponent; 
    Unknown   : IUnknown; 
  begin 
  inherited SetValue(Value); 

  {$IFDEF DCC6OrLater} 
  Component := GetComponentReference; 
  {$ELSE} 
  Component := TComponent(GetOrdValue); 
  {$ENDIF} 

  if Assigned(Component) then 
    begin 
    if Component.GetInterface(GetIID, Unknown) then 
      begin 
      Unknown := nil; 
      Exit; 
      end; 

    inherited SetValue(''); 

    raise EPropertyError.Create('Invalid property value');
    end; 
  end;

{$ENDIF}

end.
