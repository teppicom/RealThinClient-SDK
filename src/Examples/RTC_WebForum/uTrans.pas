{-----------------------------------------------------------------------------
 Unit Name: uTrans
 Author:    Alexander V. Aleksishin
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
-----------------------------------------------------------------------------}

unit uTrans;

{$include rtcDefs.inc}

interface

function HTMLEncode(const S: string): string;
function BBCode2HTML(const FSource : string) : string;

implementation

uses
  SysUtils, uMessages;

{-- HTML encode ----------------------------------------------------------------}
function HTMLEncode(const S: string): string;
  const
    l_NumStr = 2+1;

    NumStrPrefix = '&#';
    NumStrPostfix = ';';
    AmpStr = '&amp;';
    QuotStr = '&quot;';
    LtStr = '&lt;';
    GtStr = '&gt;';
    SpaceStr = '&nbsp;';
    NewLine = '<br>';

    l_AmpStr = 5-1;
    l_QuotStr = 6-1;
    l_LtStr = 4-1;
    l_GtStr = 4-1;
    l_SpaceStr = 6-1;
    l_NewLine = 4-1;
    l_TabStr = 6*8-1;

  var
    i, j, n: Integer;
    len : integer;
    leading_space : boolean;
    ch:char;

  begin
  // Calc size for result buffers
  leading_space := true;

  Len := Length(S);
  n := Len;
  for I := 1 to Len do
    begin
    ch:=S[i];
    case ch of
      #13: Dec(n);
      #10: begin
           leading_space := True;
           Inc(n,l_NewLine);
           end;
      #9:  if leading_space then Inc(n,l_TabStr);
      #32: if leading_space then Inc(n,l_SpaceStr)
           else leading_space:=true;
      '&': begin
           Inc(n, l_AmpStr);
           leading_space:=False;
           end;
      '"': begin
           Inc(n, l_QuotStr);
           leading_space:=False;
           end;
      '<': begin
           Inc(n, l_LtStr);
           leading_space:=False;
           end;
      '>': begin
           Inc(n, l_GtStr);
           leading_space:=False;
           end;
      #0..#8:
        begin
        Inc(n, l_NumStr);
        leading_space:=False;
        end;
      #11,#12,#14..#31:
        begin
        Inc(n, l_NumStr+1);
        leading_space:=False;
        end;
      else
        leading_space:=False;
      end;
    end;

  SetLength(Result, n);

  // Converting
  leading_space := true;
  n := 1;
  for I := 1 to Len do
    begin
    ch:=S[i];
    case ch of
      #13: continue;
      #10:
        begin
        leading_space := True;
        Result[n]:='<';
        Result[n+1]:='b';
        Result[n+2]:='r';
        Result[n+3]:='>';Inc(n,4);
        end;
      #9:
        if leading_space then
          begin
          for j:=1 to 8 do
            begin
            Result[n]:='&';
            Result[n+1]:='n';
            Result[n+2]:='b';
            Result[n+3]:='s';
            Result[n+4]:='p';
            Result[n+5]:=';';Inc(n,6);
            end;
          end
        else
          begin
          Result[n]:=#9;
          Inc(n);
          end;
      #32:
        if leading_space then
          begin
          Result[n]:='&';
          Result[n+1]:='n';
          Result[n+2]:='b';
          Result[n+3]:='s';
          Result[n+4]:='p';
          Result[n+5]:=';';Inc(n,6);
          end
        else
          begin
          Result[n]:=#32;
          Inc(n);
          leading_space:=True;
          end;
      #0..#8:
        begin
        Result[n]:='&';
        Result[n+1]:='#';
        Result[n+2]:= Chr(Ord(ch)+48);
        Result[n+3]:=';';
        Inc(n,4);
        leading_space:=False;
        end;
      #11,#12,#14..#31:
        begin
        Result[n]:='&';
        Result[n+1]:='#';
        Result[n+2]:= Chr((Ord(ch) div 10)+48);
        Result[n+3]:= Chr((Ord(ch) mod 10)+48);
        Result[n+4]:=';';
        Inc(n,5);
        leading_space:=False;
        end;
      '&':
        begin
        Result[n]:='&';
        Result[n+1]:='a';
        Result[n+2]:='m';
        Result[n+3]:='p';
        Result[n+4]:=';';
        Inc(n,5);
        leading_space:=False;
        end;
      '"':
        begin
        Result[n]:='&';
        Result[n+1]:='q';
        Result[n+2]:='u';
        Result[n+3]:='o';
        Result[n+4]:='t';
        Result[n+5]:=';';
        Inc(n,6);
        leading_space:=False;
        end;
      '<':
        begin
        Result[n]:='&';
        Result[n+1]:='l';
        Result[n+2]:='t';
        Result[n+3]:=';';
        Inc(n,4);
        leading_space:=False;
        end;
      '>':
        begin
        Result[n]:='&';
        Result[n+1]:='g';
        Result[n+2]:='t';
        Result[n+3]:=';';
        Inc(n,4);
        leading_space:=False;
        end;
      else
        begin
        Result[n]:=ch;
        Inc(n);
        leading_space:=False;
        end;
      end;
    end;
  end;

function FindPos(const Substr, Str: String; StartPos: Integer = 1): Integer;
  var
    lenStr: Integer;
    lenSubstr: Integer;
    x, y: Integer;
  begin
  lenStr := Length(Str);
  lenSubstr := Length(Substr);

  case lenSubstr of
    0: Result := 0;

    1: begin
      Result := 0;
      for x:= StartPos to lenStr do
        if (Substr[1] = Str[x]) then
          begin
          Result := x;
          Break;
          end;
      end;
    2: begin
      Result := 0;
      for x := StartPos to lenStr-1 do
        if ((Substr[1] = Str[x]) and (SubStr[2] = Str[x+1])) then
          begin
          Result := x;
          Break;
          end;
      end;
    else
      begin
      Result := 0;
      for x := StartPos to lenStr-lenSubstr+1 do
        if ((Substr[1] = Str[x]) and (Substr[2] = Str[x+1]) and (Substr[3] = Str[x+2])) then
          begin
          Result := x;
          for y := 3 to lenSubstr-1 do
            if (Substr[1+y] <> Str[x+y]) then
              begin
              Result := 0;
              Break;
              end;
          if Result > 0 then
            Break;
          end;
      end;
    end;
  end;

function FindPosUp(const Substr, Str: String; StartPos: Integer = 1): Integer;
  var
    lenStr: Integer;
    lenSubstr: Integer;
    x, y: Integer;
  begin
  lenStr := Length(Str);
  lenSubstr := Length(Substr);

  case lenSubstr of
    0: Result := 0;

    1: begin
      Result := 0;
      for x:= StartPos to lenStr do
        if (Substr[1] = UpCase(Str[x])) then
          begin
          Result := x;
          Break;
          end;
      end;
    2: begin
      Result := 0;
      for x := StartPos to lenStr-1 do
        if ((Substr[1] = UpCase(Str[x])) and (SubStr[2] = UpCase(Str[x+1]))) then
          begin
          Result := x;
          Break;
          end;
      end;
    else
      begin
      Result := 0;
      for x := StartPos to lenStr-lenSubstr+1 do
        if ((Substr[1] = UpCase(Str[x])) and (Substr[2] = UpCase(Str[x+1])) and (Substr[3] = UpCase(Str[x+2]))) then
          begin
          Result := x;
          for y := 3 to lenSubstr-1 do
            if (Substr[1+y] <> UpCase(Str[x+y])) then
              begin
              Result := 0;
              Break;
              end;
          if Result > 0 then
            Break;
          end;
      end;
    end;
  end;

const
  BEGIN_URL     = 'URL=';
  END_URL       = '/URL';
  BEGIN_URL2    = 'URL ';
  BEGIN_IMG     = 'IMG=';
  END_IMG       = '/IMG';
  BEGIN_IMG2    = 'IMG ';

  BEGIN_IMG3    = 'GIF ';

  BEGIN_CODE    = 'CODE';
  END_CODE      = '/CODE';

  LEN_MAX_CODE = 10;
  LEN_MAX = 255;

function BBCode2HTML(const FSource : string) : string;
  var
    FTokenClose: String;
    FTokenOpen: String;
    lSource: Integer;
    lTokenOpen: Integer;
    lTokenClose: Integer;
    copyStart: Integer;
    posStart: Integer;
    posEnd: Integer;
    variable: String;
    u_variable: String;
    url_tag : integer;
    img_tag : integer;
  begin
  FTokenOpen := '[';
  FTokenClose := ']';

  if FSource <> '' then
    begin
    lSource := Length(FSource);
    lTokenOpen := Length(FTokenOpen);
    lTokenClose := Length(FTokenClose);

    copyStart := 1;
    Result := '';

    url_tag := 0;
    img_tag := 0;

    // look for the tokens and replace matching variables with their values
    posStart := FindPos(FTokenOpen, FSource, 1);
    while posStart > 0 do
      begin
      Result := Result + Copy(FSource, copyStart, posStart - copyStart);

      posEnd := FindPos(FTokenClose, FSource, posStart + 1);
      if posEnd <= 0 then Break;

      // extract the variable name from the source string
      if (posEnd-posStart>0) and (posEnd-posStart<=LEN_MAX) then
        begin
        variable := Copy(FSource, posStart + lTokenOpen, posEnd - (posStart + lTokenOpen));

        if length(variable)>=4 then
          begin
          u_variable := UpperCase(Copy(variable, 1, 4));

          if u_variable = BEGIN_URL then
            begin
            Inc(url_tag);
            variable := Copy(variable, 4 + 1, MAXINT);
            Result := Result+'<a target="_blank" href="'+variable+'">';
            end
          else if u_variable = BEGIN_URL2 then
            begin
            variable := Copy(variable, 4 + 1, MAXINT);
            Result := Result+'<a target="_blank" href="'+variable+'">'+variable+'</a>';
            end
          else if u_variable = BEGIN_IMG then
            begin
            Inc(img_tag);
            variable := Copy(variable, 4 + 1, MAXINT);
            Result := Result+'<img src="'+variable+'" alt="';
            end
          else if u_variable = BEGIN_IMG2 then
            begin
            variable := Copy(variable, 4 + 1, MAXINT);
            Result := Result+'<img src="'+variable+'">';
            end
          else if u_variable = BEGIN_IMG3 then
            begin
            u_variable := Copy(variable, 1, 3);
            variable := Copy(variable, 4 + 1, MAXINT);
            Result := Result+'<img src="'+variable+'.'+u_variable+'">';
            end
          else if (u_variable = BEGIN_CODE) and (length(variable)=4) then
            begin
            copyStart := posEnd + lTokenClose;
            posEnd := FindPos(FTokenClose, FSource, posStart+1);

            posStart := FindPosUp(FTokenOpen + END_CODE, FSource, posEnd+1);
            if posStart>0 then
              begin
              posEnd := FindPos(FTokenClose, FSource, posStart+1);

              Result := Result + GetMsg('source_code_begin') +
                Copy(FSource, copyStart, posStart-copyStart) +
                GetMsg('source_code_end');
              end
            else
              begin
              Result:=Result+FTokenOpen+variable+FTokenClose;
              end;
            end
          else if (url_tag > 0) and (u_variable = END_URL) then
            begin
            Dec(url_tag);
            Result := Result+'</a>';
            end
          else if (img_tag > 0) and (u_variable = END_IMG) then
            begin
            Dec(url_tag);
            Result := Result+'">';
            end
          else
            begin
            Result:=Result+FTokenOpen+variable+FTokenClose;
            end;
          end
        else
          Result:=Result+FTokenOpen+variable+FTokenClose;
        end
      else
        begin
        Result:=Result+FTokenOpen+variable+FTokenClose;
        end;

      copyStart := posEnd + lTokenClose;
      posStart := FindPos(FTokenOpen, FSource, posEnd + 1);
      end;

    // make sure that remaining part of FSource is returned
    if copyStart < lSource then
      Result := Result + Copy(FSource, copyStart, lSource - copyStart + 1);
    end;
  end;

end.
