//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "rtcConn"
#pragma link "rtcDataCli"
#pragma link "rtcHttpCli"
#pragma link "rtcInfo"
#pragma link "rtcSystem"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::DataReqBeginRequest(TRtcConnection *Sender)
{
  TRtcDataClient* sender = (TRtcDataClient*) Sender;
  TRtcClientRequest* req = (TRtcClientRequest*) sender->Request;

  // make sure our request starts with "/"
  if (req->FileName.SubString(1,1) != "/")
    req->FileName = "/" + req->FileName;

  // define the "HOST" header
  if (req->Host == "")
    req->Host = sender->ServerAddr;

  Memo1->Text = "Requesting '" + req->FileName +
                "' from '" + sender->ServerAddr + "'.";

  // send request header out
  sender->WriteHeader();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::DataReqDataReceived(TRtcConnection *Sender)
{
  TRtcDataClient* sender = (TRtcDataClient*) Sender;
  TRtcClientResponse* res = (TRtcClientResponse*) sender->Response;

  if (res->Started)
  {
    /* Executed only once per request,
       when we start receiving the response. */

    // Clear the info we wrote here in our "OnBeginRequest"
    Memo1->Clear();
    Memo1->Lines->Add("Status code: "+
                      IntToStr(res->StatusCode));
    Memo1->Lines->Add("Status text:"+
                      res->StatusText);
    Memo1->Lines->Add("ALL Headers:");
    Memo1->Lines->Add(res->HeaderText);

    Memo1->Lines->Add("Content Length:");
    Memo1->Lines->Add(IntToStr(res->ContentLength));
    Memo1->Lines->Add("Content body:");
    Memo1->Lines->Add("START >");
  }

  /* Could be executed more than once,
     depending on the content size */

  // add content received now.
  Memo1->Text = Memo1->Text + sender->Read();
  if (res->Done)
  {
    /* Executed only once per request,
        when we have just received it all. */
    Memo1->Lines->Add("< END");
  }
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Edit1KeyPress(TObject *Sender, char &Key)
{
  if (Key == 13)
  {
    Key = 0;
    Edit1->SelectAll();

    DataReq->Request->Method = "GET";
    // request the file defined in the Edit field
    DataReq->Request->FileName = Edit1->Text;
    DataReq->Post(); // Post the request
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  DataReq->Request->Method = "GET";
  // request the file defined in the Edit field
  DataReq->Request->FileName = Edit1->Text;
  DataReq->Post(); // Post the request
}
//---------------------------------------------------------------------------
