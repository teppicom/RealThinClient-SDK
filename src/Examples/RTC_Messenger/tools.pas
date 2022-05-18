unit tools;

interface
uses mmsystem;

procedure klick;
procedure knock;
procedure door;
procedure eingang;
procedure doorbell;
procedure chimeup;
procedure morsen;

implementation



procedure klick;
begin
  try
    playsound('click', Hinstance, SND_ASYNC or SND_RESOURCE);
  except
  end;
end;

procedure morsen;
begin
  try
    playsound('morse', Hinstance, SND_ASYNC or SND_RESOURCE);
  except
  end;
end;



procedure door;
begin
  try
    playsound('door', Hinstance, SND_ASYNC or SND_RESOURCE);
  except
  end;
end;

procedure chimeup;
begin
  try
    playsound('chimeup', Hinstance, SND_ASYNC or SND_RESOURCE);
  except
  end;
end;

procedure knock;
begin
  try
    playsound('knock', Hinstance, SND_ASYNC or SND_RESOURCE);
  except
  end;
end;

procedure doorbell;
begin
  try
    playsound('doorbell', Hinstance, SND_ASYNC or SND_RESOURCE);
  except
  end;
end;

procedure eingang;
begin
  try
    playsound('eingang', Hinstance, SND_ASYNC or SND_RESOURCE);
  except
  end;
end;


end.  
