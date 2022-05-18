unit uForumDB;

{$include rtcDefs.inc}

interface

uses
  Windows, SysUtils, Classes, rtcInfo, rtcSystem, 
  uSections, uRights, uTopics, uReplies,
  {$IFDEF IDE_1}
  FileCtrl,
  {$ENDIF}
  uTypes, rtcTypes, rtcCrypt;

const
  SECTIONS_DATA_FILE_NAME = 'sections.data';
  RIGHTS_DATA_FILE_NAME = 'rights.data';
  SECTIONS_FOLDER_NAME = 'section';
  TOPICS_DATA_FILE_NAME = 'topicinfo.data';
  REPLIES_DATA_FILE_NAME = 'text.data';

type
  TRtcForumData=class
  private
    forum_path: RtcString;
    CS:TRtcCritSec;

    Topics_section,
    Replies_section,
    Replies_topic:integer;

    function GetSectionDirName(SectionID: integer): RtcString;
    function GetRepliesFileName(SectionID, TopicID: integer): RtcString;
    function GetTopicsFileName(SectionID: integer): RtcString;

  public

    Sections : TSectionsTable;
    Rights : TRightsTable;
    Topics : TTopicsTable;
    Replies: TRepliesTable;

    constructor Create(folder:RtcString);
    destructor Destroy; override;

    procedure Lock;

    procedure LoadRights;
    procedure LoadSections;
    procedure LoadTopics(section_id:integer);
    procedure LoadReplies(section_id:integer; topic_id:integer);

    procedure DeleteSection(section_id:integer);
    procedure ClearSection(section_id: integer);

    procedure DeleteTopic(section_id,topic_id:integer);

    procedure Unlock;
    end;

implementation

{-- ClearDir -------------------------------------------------------------------}

function NormalDir(const DirName: RtcString): RtcString;
begin
  Result := DirName;
  if (Result <> '') and
{$IFDEF RX_D3}
    not (AnsiLastChar(Result)^ in [':', '\']) then
{$ELSE}
    not (Result[Length(Result)] in [':', '\']) then
{$ENDIF}
  begin
    if (Length(Result) = 1) and (UpCase(Result[1]) in ['A'..'Z']) then
      Result := Result + ':\'
    else Result := Result + '\';
  end;
end;

function ClearDir(const Path: RtcString; Delete: Boolean): Boolean;
const
{$IFDEF WIN32}
  FileNotFound = 18;
{$ELSE}
  FileNotFound = -18;
{$ENDIF}
var
  FileInfo: TSearchRec;
  DosCode: Integer;
begin
  Result := DirectoryExists(Path);
  if not Result then Exit;
  DosCode := FindFirst(NormalDir(Path) + '*.*', faAnyFile, FileInfo);
  try
    while DosCode = 0 do begin
      if (FileInfo.Name[1] <> '.') and (FileInfo.Attr <> faVolumeID) then
      begin
        if (FileInfo.Attr and faDirectory = faDirectory) then
          Result := ClearDir(NormalDir(Path) + FileInfo.Name, Delete) and Result
        else if (FileInfo.Attr and faVolumeID <> faVolumeID) then begin
          if (FileInfo.Attr and faReadOnly = faReadOnly) then
            FileSetAttr(NormalDir(Path) + FileInfo.Name, faArchive);
          Result := DeleteFile(NormalDir(Path) + FileInfo.Name) and Result;
        end;
      end;
      DosCode := FindNext(FileInfo);
    end;
  finally
    FindClose(FileInfo);
  end;
  if Delete and Result and (DosCode = FileNotFound) and
    not ((Length(Path) = 2) and (Path[2] = ':')) then
  begin
    RmDir(Path);
    Result := (IOResult = 0) and Result;
  end;
end;

function _ExcludeTrailingPathDelimiter(const S: RtcString): RtcString;
begin
  Result := S;
  if IsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;


function TRtcForumData.GetSectionDirName(SectionID: integer): RtcString;
begin
  Result := Format('%s\%s%.8x',
    [
      _ExcludeTrailingPathDelimiter(forum_path),
      SECTIONS_FOLDER_NAME,
      SectionID
    ]
  );
end;

function TRtcForumData.GetRepliesFileName(SectionID, TopicID: integer): RtcString;
begin
  Result := Format('%s\Topic%.8x_%s',
    [
      GetSectionDirName(SectionID),
      TopicID,
      REPLIES_DATA_FILE_NAME
    ]
  );
end;

function TRtcForumData.GetTopicsFileName(SectionID: integer): RtcString;
begin
  Result := Format('%s\%s',
    [
      GetSectionDirName(SectionID),
      TOPICS_DATA_FILE_NAME
    ]
  );
end;

procedure TRtcForumData.Lock;
  begin
  CS.Acquire;
  end;

procedure TRtcForumData.LoadRights;
  begin
  if not assigned(Rights) then
    Rights := TRightsTable.Create(_ExcludeTrailingPathDelimiter(forum_path)+'\'+RIGHTS_DATA_FILE_NAME);
  end;

procedure TRtcForumData.LoadSections;
  begin
  if not assigned(Sections) then
    Sections := TSectionsTable.Create(_ExcludeTrailingPathDelimiter(forum_path)+'\'+SECTIONS_DATA_FILE_NAME);
  end;

procedure TRtcForumData.LoadTopics(section_id:integer);
  begin
  if assigned(Topics) then
    if (section_id<>Topics_section) then
      begin
      Topics.Free;
      Topics:=nil;
      end;
  if not assigned(Topics) then
    begin
    Topics := TTopicsTable.Create(GetTopicsFileName(section_id));
    Topics_section:=section_id;
    end;
  end;

procedure TRtcForumData.LoadReplies(section_id:integer; topic_id:integer);
  begin
  if assigned(Replies) then
    if (section_id<>Replies_section) or (topic_id<>Replies_topic) then
      begin
      Replies.Free;
      Replies:=nil;
      end;
  if not assigned(Replies) then
    begin
    Replies := TRepliesTable.Create(GetRepliesFileName(section_id, topic_id));
    Replies_section:=section_id;
    Replies_topic:=topic_id;
    end;
  end;

procedure TRtcForumData.DeleteSection(section_id:integer);
  begin
  // Make sure all Topic and Reply files are closed
  if assigned(Topics) then
     begin
     Topics.Free;
     Topics:=nil;
     end;
  if assigned(Replies) then
     begin
     Replies.Free;
     Replies:=nil;
     end;
  // Delete Section Directory
  ClearDir(GetSectionDirName(section_id), True);
  end;

procedure TRtcForumData.ClearSection(section_id:integer);
  begin
  // Make sure all Topic and Reply files are closed
  if assigned(Topics) then
    begin
    Topics.Free;
    Topics:=nil;
    end;
  if assigned(Replies) then
    begin
    Replies.Free;
    Replies:=nil;
    end;
  // Clear Section Directory
  ClearDir(GetSectionDirName(section_id), False);
  end;

procedure TRtcForumData.DeleteTopic(section_id,topic_id:integer);
  begin
  // Make sure all Topic and Reply files are closed
  if assigned(Topics) then
    begin
    Topics.Free;
    Topics:=nil;
    end;
  if assigned(Replies) then
    begin
    Replies.Free;
    Replies:=nil;
    end;
  // Delete Topic
  DeleteFile(GetRepliesFileName(section_id, topic_id));
  end;

procedure TRtcForumData.Unlock;
  begin
  if assigned(Sections) then Sections.Flush;
  if assigned(Rights) then Rights.Flush;
  if assigned(Topics) then Topics.Flush; // FreeAndNil(Topics);
  if assigned(Replies) then Replies.Flush; // FreeAndNil(Replies);
  CS.Release;
  end;

constructor TRtcForumData.Create(folder: RtcString);
  begin
  CS:=TRtcCritSec.Create;
  forum_path:=folder;
  Topics_section:=-1;
  Replies_section:=-1;
  Replies_topic:=-1;
  Sections:=nil;
  Rights:=nil;
  Topics:=nil;
  Replies:=nil;
  end;

destructor TRtcForumData.Destroy;
  begin
  if assigned(Sections) then
    begin
    Sections.Free;
    Sections:=nil;
    end;
  if assigned(Rights) then
    begin
    Rights.Free;
    Rights:=nil;
    end;
  if assigned(Topics) then
    begin
    Topics.Free;
    Topics:=nil;
    end;
  if assigned(Replies) then
    begin
    Replies.Free;
    Replies:=nil;
    end;
  CS.Free;
  inherited;
  end;

end.
