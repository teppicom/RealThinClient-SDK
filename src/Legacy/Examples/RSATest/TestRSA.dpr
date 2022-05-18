program TestRSA;

{$apptype console}

uses
  rtcSystem,
  rtcCrypt,
  rtcInfo;

var
  rsa:TRtcRSA;
  cry:TRtcISAACrypt;
  a,b,c,d,e:RtcByteArray;
  en,de,pr,ha,si:RtcByteArray;
  seed,i:integer;

begin
  SetLength(a,0);
  SetLength(b,0);
  SetLength(c,0);
  SetLength(d,0);
  SetLength(e,0);
  SetLength(en,0);
  SetLength(de,0);
  SetLength(pr,0);
  SetLength(ha,0);
  SetLength(si,0);

  rsa:=TRtcRSA.Create;
  try
    rsa.GenerateKeyPair(256);
    writeln('Strength = ',rsa.KeyBits);
    writeln('Private = ',length(rsa.PrivateKey),'---');
    Writeln(RtcBytesToHex(rsa.PrivateKey));
    Writeln('---');
    writeln('Public = ',length(rsa.PublicKey),'---');
    Writeln(RtcBytesToHex(rsa.PublicKey));
    Writeln('---');

    // Test get/set Private and Public Keys
    rsa.PrivateKey:=rsa.PrivateKey;
    rsa.PublicKey:=rsa.PublicKey;

    pr:=rsa.PrivateKey;
    rsa.PrivateKey:=pr;
    en:=rsa.Encrypt(pr);
    writeln('Encrypt Private = ',length(en),'---');
    Writeln(RtcBytesToHex(en));
    Writeln('---');
    de:=rsa.Decrypt(en);
    writeln('Decrypt Private = ',length(de),'---');
    Writeln(RtcBytesToHex(de));
    Writeln('---');
    if length(de)<>length(pr) then
      begin
      Writeln('Private Size missmatch');
      Exit;
      end;
    for i:=0 to length(de)-1 do
      if de[i]<>pr[i] then
        begin
        Writeln('Private Char ',i,' error');
        Exit;
        end;

    pr:=rsa.PublicKey;
    en:=rsa.Encrypt(pr);
    writeln('Encrypt Public = ',length(en),'---');
    Writeln(RtcBytesToHex(en));
    Writeln('---');
    de:=rsa.Decrypt(en);
    writeln('Decrypt Public = ',length(de),'---');
    Writeln(RtcBytesToHex(de));
    Writeln('---');
    if length(de)<>length(pr) then
      begin
      Writeln('Public Size missmatch');
      Exit;
      end;
    for i:=0 to length(de)-1 do
      if de[i]<>pr[i] then
        begin
        Writeln('Public Char ',i,' error');
        Exit;
        end;

    pr:=rsa.RND(1024+random(1024));
    en:=rsa.Encrypt(pr);
    writeln('Random = ',length(pr),'---');
    Writeln(RtcBytesToHex(pr));
    Writeln('---');
    writeln('Encrypt Random = ',length(en),'---');
    Writeln(RtcBytesToHex(en));
    Writeln('---');
    de:=rsa.Decrypt(en);
    writeln('Decrypt Random = ',length(de),'---');
    Writeln(RtcBytesToHex(de));
    Writeln('---');
    if length(de)<>length(pr) then
      begin
      Writeln('Random Size missmatch');
      Exit;
      end;
    for i:=0 to length(de)-1 do
      if de[i]<>pr[i] then
        begin
        Writeln('Random Char ',i,' error');
        Exit;
        end;

    ha:=rsa.RND(16);
    si:=rsa.SignHash(rtch_MD2,ha);
    writeln('MD2 = ',length(ha),'---');
    Writeln(RtcBytesToHex(ha));
    Writeln('---');
    writeln('MD2 Signed = ',length(si),'---');
    Writeln(RtcBytesToHex(si));
    Writeln('---');
    if not rsa.VerifyHash(rtch_MD2,ha,si) then
      begin
      Writeln('MD2 Sign Error');
      Exit;
      end;

    ha:=rsa.RND(16);
    si:=rsa.SignHash(rtch_MD5,ha);
    writeln('MD5 = ',length(ha),'---');
    Writeln(RtcBytesToHex(ha));
    Writeln('---');
    writeln('MD5 Signed = ',length(si),'---');
    Writeln(RtcBytesToHex(si));
    Writeln('---');
    if not rsa.VerifyHash(rtch_MD5,ha,si) then
      begin
      Writeln('MD5 Sign Error');
      Exit;
      end;

    ha:=rsa.RND(20);
    si:=rsa.SignHash(rtch_RMD160,ha);
    writeln('RMD160 = ',length(ha),'---');
    Writeln(RtcBytesToHex(ha));
    Writeln('---');
    writeln('RMD160 Signed = ',length(si),'---');
    Writeln(RtcBytesToHex(si));
    Writeln('---');
    if not rsa.VerifyHash(rtch_RMD160,ha,si) then
      begin
      Writeln('RMD160 Sign Error');
      Exit;
      end;

    ha:=rsa.RND(20);
    si:=rsa.SignHash(rtch_SHA1,ha);
    writeln('SHA1 = ',length(ha),'---');
    Writeln(RtcBytesToHex(ha));
    Writeln('---');
    writeln('SHA1 Signed = ',length(si),'---');
    Writeln(RtcBytesToHex(si));
    Writeln('---');
    if not rsa.VerifyHash(rtch_SHA1,ha,si) then
      begin
      Writeln('SHA1 Sign Error');
      Exit;
      end;

    ha:=rsa.RND(28);
    si:=rsa.SignHash(rtch_SHA224,ha);
    writeln('SHA224 = ',length(ha),'---');
    Writeln(RtcBytesToHex(ha));
    Writeln('---');
    writeln('SHA224 Signed = ',length(si),'---');
    Writeln(RtcBytesToHex(si));
    Writeln('---');
    if not rsa.VerifyHash(rtch_SHA224,ha,si) then
      begin
      Writeln('SHA224 Sign Error');
      Exit;
      end;

    ha:=rsa.RND(32);
    si:=rsa.SignHash(rtch_SHA256,ha);
    writeln('SHA256 = ',length(ha),'---');
    Writeln(RtcBytesToHex(ha));
    Writeln('---');
    writeln('SHA256 Signed = ',length(si),'---');
    Writeln(RtcBytesToHex(si));
    Writeln('---');
    if not rsa.VerifyHash(rtch_SHA256,ha,si) then
      begin
      Writeln('SHA256 Sign Error');
      Exit;
      end;

    ha:=rsa.RND(48);
    si:=rsa.SignHash(rtch_SHA384,ha);
    writeln('SHA384 = ',length(ha),'---');
    Writeln(RtcBytesToHex(ha));
    Writeln('---');
    writeln('SHA384 Signed = ',length(si),'---');
    Writeln(RtcBytesToHex(si));
    Writeln('---');
    if not rsa.VerifyHash(rtch_SHA384,ha,si) then
      begin
      Writeln('SHA384 Sign Error');
      Exit;
      end;

    ha:=rsa.RND(64);
    si:=rsa.SignHash(rtch_SHA512,ha);
    writeln('SHA512 = ',length(ha),'---');
    Writeln(RtcBytesToHex(ha));
    Writeln('---');
    writeln('SHA512 Signed = ',length(si),'---');
    Writeln(RtcBytesToHex(si));
    Writeln('---');
    if not rsa.VerifyHash(rtch_SHA512,ha,si) then
      begin
      Writeln('SHA512 Sign Error');
      Exit;
      end;

    ha:=rsa.RND(9+random(90));
    si:=rsa.SignHash(rtch_Custom,ha);
    writeln('Custom = ',length(ha),'---');
    Writeln(RtcBytesToHex(ha));
    Writeln('---');
    writeln('Custom Signed = ',length(si),'---');
    Writeln(RtcBytesToHex(si));
    Writeln('---');
    if not rsa.VerifyHash(rtch_Custom,ha,si) then
      begin
      Writeln('Custom Sign Error');
      Exit;
      end;

    a:=rsa.RND(64);
    seed:=rsa.RandomInt*rsa.RandomInt;

    Writeln('Orig='+RtcBytesToHex(a));
    b:=rsa.GrowFromSeed(a,seed,1024,7);
    Writeln('Grown1='+RtcBytesToHex(b));
    c:=rsa.GrowFromSeed(a,seed,1024,7);
    Writeln('Grown2='+RtcBytesToHex(c));
    d:=rsa.GrowFromSeed(a,seed,1024,7);
    Writeln('Grown3='+RtcBytesToHex(d));
    e:=rsa.GrowFromSeed(a,seed,1024,7);
    Writeln('Grown4='+RtcBytesToHex(e));
    if (length(b)<>length(c)) or (length(c)<>length(d)) or (length(d)<>length(e)) then
      begin
      Writeln('Growth Size Error!');
      Exit;
      end;
    for i:=0 to length(b)-1 do
      if (b[i]<>c[i]) or (c[i]<>d[i]) or (d[i]<>e[i]) then
        begin
        Writeln('Growth Data Error !',i);
        Exit;
        end;

    // TRtcISAACrypt test ...
    cry:=TRtcISAACrypt.Create;
    try
      cry.Key:=a;

      // Encrypt B, then C, then D (in-place) with the Key "A"
      cry.Crypt(b);
      cry.Crypt(c);
      cry.Crypt(d);

      Writeln('CryptB='+RtcBytesToHex(b));
      Writeln('CryptC='+RtcBytesToHex(c));
      Writeln('CryptD='+RtcBytesToHex(d));

      cry.Init;
      // Decrypt B, then C, then D (in-place) with the Key "A"
      cry.DeCrypt(b);
      cry.Decrypt(c);
      cry.Decrypt(d);

      Writeln('DeCryptB='+RtcBytesToHex(b));
      Writeln('DeCryptC='+RtcBytesToHex(c));
      Writeln('DeCryptD='+RtcBytesToHex(d));

      for i:=0 to length(b)-1 do
        if (b[i]<>c[i]) or (c[i]<>d[i]) or (d[i]<>e[i]) then
          begin
          Writeln('ISAACrypt/Decrypt Error !',i);
          Exit;
          end;

      SetLength(a,1024);
      for i := 0 to 1023 do
        a[i]:=i mod 255;
      cry.Key:=a;
      Writeln('Orig='+RtcBytesToHex(a));
      cry.Crypt(a);
      Writeln('Crypt='+RtcBytesToHex(a));
      cry.Init;
      cry.DeCrypt(a);
      writeln('DeCr='+RtcBytesToHex(a));

      for i := 0 to length(a)-1 do
        if a[i]<>i mod 255 then
          begin
          Writeln('Error at ',i);
          Exit;
          end;

    finally
      cry.Free;
      end;

    writeln('Done.');

  finally
    rsa.Free;

    readln;
    end;
end.
