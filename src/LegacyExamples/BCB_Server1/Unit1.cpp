//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "rtcConn"
#pragma link "rtcDataSrv"
#pragma link "rtcHttpSrv"
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

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  RtcHttpServer1->Listen();        
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RtcDataProvider1CheckRequest(
      TRtcConnection *Sender)
{
  TRtcDataServer *sender = (TRtcDataServer*) Sender;
  if (sender->Request->FileName.UpperCase() == "/TIME")
  {
        sender->Accept();
        sender->Write("Current time is" + TimeToStr(Now()));
  }
}
//---------------------------------------------------------------------------

