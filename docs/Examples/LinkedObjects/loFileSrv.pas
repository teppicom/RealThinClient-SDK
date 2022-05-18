{
  Example implementation of a Server-side Linked Object inside a Data Module.
  - Copyright 2004-2018 (c) Teppi Technology (https://rtc.teppi.net)
}
unit loFileSrv;

interface

uses
  SysUtils, Classes,

  rtcInfo, rtcLink, rtcSystem;

type
  TloFileServer = class(TDataModule)
    Link: TRtcLinkedModule;
    procedure LinkCallMethod(Sender: TObject; Param: TRtcObjectCall);
    procedure DataModuleCreate(Sender: TObject);
    procedure LinkCallEvent(Sender: TObject; Param: TRtcObjectCall);
    procedure LinkBroadcast(Sender: TObject; Param: TRtcObjectCall);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    amActive:boolean;
    MyFileName:String;
    MyFileSize,MyFileIn:int64;
    MyActive,MyTotal:integer;
    ID:String;
  end;

implementation

{$R *.dfm}

procedure MakeFileServer(Sender:TObject; Param:TRtcObjectCall);
  begin
  { This is our global "FileServer" constructor, which is called
    when the Client requests to have a File Server created.
    This procedure is registered in this units "initialization" section
    and unregistered in the "finalization" section. }
  if Param.xName='FileServer' then
    TloFileServer.Create(nil);
  end;

procedure TloFileServer.DataModuleCreate(Sender: TObject);
  var
    G:TGUID;
  begin
  MyActive:=0;
  MyTotal:=0;

  { Subscribe to receive "FileSrv" broadcasts, which are used here
    to update the number of Total and Active (uploading) File Servers. }
  Link.Subscribe('FileSrv');

  { Subscribe to a unique broadcasting channel,
    reserved for receiving personal broadcasts. }
  CreateGUID(G);
  ID:=GUIDToString(G);
  Link.Subscribe('FileSrv.'+ID);

  { Broadcast to everyone subscribed to "FileSrv" (including myself)
    that we have a new Inactive File Server. }
  Link.Broadcast('FileSrv','Inactive');

  { Broadcast a "Check" request to everyone, asking them to return their status.
    "ID" parameter is sent so the receiver can decide if they want to respond or not. }
  Link.Param.asText:=ID;
  Link.Broadcast('FileSrv','Check');

  { Send our local path to our Client's "Path" property }
  Link.Param.asText:=ExtractFilePath(ExpandUNCFileName(AppFileName));
  Link.SetProp('Path');
  end;

procedure TloFileServer.DataModuleDestroy(Sender: TObject);
  begin
  { Linked Objects are destroyed automatically when a remote Linked Object instance is destroyed
    or when the Object Manager holding this instance is destroyed (a Session has timed out?). }
  if amActive then
    begin
    amActive:=False;
    { We were actively uploading.
      Broadcast to all other File Servers that our upload has stopped. }
    Link.Broadcast('FileSrv','Stop');
    end;
  { Broadcast to all other "FileSrv" subcribers that our File Server was closed. }
  Link.Broadcast('FileSrv','Closed');
  end;

procedure TloFileServer.LinkCallMethod(Sender: TObject; Param: TRtcObjectCall);
  procedure DoUpload;
    begin
    { Call Methods "Get" or "Done" on the Client to ask for more data
      or signal the Client that we have received the whole file. }
    if MyFileSize-MyFileIn>10000 then // limit chunks to 10K at a time to reduce memory usage
      Link.Param.asLargeInt:=10000
    else
      Link.Param.asLargeInt:=MyFileSize-MyFileIn;
    if Link.Param.asLargeInt>0 then
      begin
      Link.CallMethod('Get');

      { Enable the "Cancel" event (used on the Client to enable the "Cancel" button). }
      Link.Param.asBoolean:=True;
      Link.SetEvent('Cancel');

      if not amActive then
        begin
        amActive:=True;
        { Broadcast to all "FileSrv" subscribers that our upload has started. }
        Link.Broadcast('FileSrv','Start');
        end;
      end
    else
      begin
      Link.CallMethod('Done');

      { Disable "Cancel" event (used on the Client to disable the "Cancel" button). }
      Link.Param.asBoolean:=False;
      Link.SetEvent('Cancel');

      if amActive then
        begin
        amActive:=False;
        { Broadcast to all "FileSrv" subscribers that our upload has stopped. }
        Link.Broadcast('FileSrv','Stop');
        end;
      end;
    end;
  begin
  { When the Client instance uses "CallMethod", this event is called.
      Param.xName = xMethodName parameter used in "CallMethod"
      Param = parameters prepared in the "Param" property }
  if Param.xName='Upload' then
    begin
    { "Upload" method call received from the Client ...
      The Client wants to start sending us a file. }
    if Param.CheckType(rtc_Record) then
      begin
      MyFileName := Param.asRecord.asText['name'];
      if File_Exists(MyFileName) then
        begin
        { Let the Client know that the File already Exists }
        Link.Param.asLargeInt:=File_Size(MyFileName);
        Link.CallMethod('FileExists');
        end
      else
        begin
        { Prepare local "size" and "in" variables for receiving
          and request the first file chunk from the Client }
        MyFileSize := Param.asRecord.asLargeInt['size'];
        MyFileIn := 0;
        DoUpload;
        end;
      end;
    end
  else if Param.xName='Put' then
    begin
    { We have received a part of the file, write it down }
    Write_File(MyFileName,Param.asString,MyFileIn);
    MyFileIn:=MyFileIn+length(Param.asString);
    { Request next file chunk from the Client }
    DoUpload;
    end
  else if Param.xName='Del' then
    begin
    { Client has requested to delete the file }
    Delete_File(MyFileName);
    end;
  end;

procedure TloFileServer.LinkCallEvent(Sender: TObject; Param: TRtcObjectCall);
  begin
  { When the Client instance uses "CallEvent", this event is called.
      Param.xName = xEventName parameter used in "CallEvent"
      Param = Parameters prepared before "CallEvent" }
  if Param.xName='Cancel' then
    begin
    { Client has pressed the "Cancel" button }
    MyFileSize:=0;
    MyFileIn:=0;
    if amActive then
      begin
      amActive:=False;
      { Upload was active, Broadcast "Stop" method to all "FileSrv" subscribers (including self) }
      Link.Broadcast('FileSrv','Stop');
      end;
    end;
  end;

procedure TloFileServer.LinkBroadcast(Sender: TObject; Param: TRtcObjectCall);
  begin
  { When a Linked Object on this Server belonging to the same "BroadcastGroup" uses
    the "Broadcast" method to broadcast a call to any channel we are subscribed to
    (we subscribed to "FileSrv" and "FileSrv."+ID in our constructor), this event is called. }
  if Param.xName='Check' then
    begin
    { Someone wants to check our status.
      Sender will broadcast its ID in the "Param.asText" parameter. }
    if Param.asText<>ID then // We do NOT want to respond our own "check" broadcast
      if MyFileIn<MyFileSize then
        // Broadcast "Active" back to the sender
        Link.Broadcast('FileSrv.'+Param.asText,'Active')
      else
        // Broadcast "Inactive" back to the sender
        Link.Broadcast('FileSrv.'+Param.asText,'Inactive');
    end
  else if Param.xName='Active' then
    begin
    { Someone has broadcasted the "Active" call. }
    Inc(MyActive);
    Inc(MyTotal);

    { Update our Client's "Active" and "Total" properties }
    Link.Param.asInteger:=MyActive;
    Link.SetProp('Active');
    Link.Param.asInteger:=MyTotal;
    Link.SetProp('Total');
    end
  else if Param.xName='Inactive' then
    begin
    { Someone has broadcasted the "Inactive" call }
    Inc(MyTotal);

    {Update our Client's "Total" property }
    Link.Param.asInteger:=MyTotal;
    Link.SetProp('Total');
    end
  else if Param.xName='Start' then
    begin
    {Someone has broadcasted the "Start" call }
    Inc(MyActive);

    { Update our Client's "Active" property }
    Link.Param.asInteger:=MyActive;
    Link.SetProp('Active');
    end
  else if Param.xName='Stop' then
    begin
    { "Stop" broadcast received }
    Dec(MyActive);

    { Update our Client's "Active" property }
    Link.Param.asInteger:=MyActive;
    Link.SetProp('Active');
    end
  else if Param.xName='Closed' then
    begin
    { "Closed" broadcast received }
    Dec(MyTotal);

    { Update our Client's "Total" property }
    Link.Param.asInteger:=MyTotal;
    Link.SetProp('Total');
    end
  end;

initialization
{ We will register a global constructor for our File Server class,
  allowing remote clients to create a File Server instance on demand.

  This is ONLY required if we want to allow the Client to create "File Server" instances remotely.
  If "TloFileServer" instances will ONLY be created manually on the Server side,
  there is no need to register a global constructor for our "TloFileServer" class.

  An alternative to using RegisterRtcObjectConstructor and registering a global
  constructor is to implement the "OnObjectCreate" event on the TRtcServerModule
  component, but that only works if we are using a TRtcServerModule for communication.
  And this particular demo is designed to work with TRtcLocalObjectManager as well. }
RegisterRtcObjectConstructor('FileServer',MakeFileServer);
finalization
UnregisterRtcObjectConstructor('FileServer');
end.
