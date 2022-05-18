//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "wwwClientAppMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "rtcConn"
#pragma link "rtcDataCli"
#pragma link "rtcHttpCli"
#pragma link "rtcInfo"
#pragma link "rtcSystem"
#pragma link "rtcCliModule"
#pragma link "rtcFunction"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ResultReturn(TRtcConnection *Sender, TRtcValue *Data,
          TRtcValue *Result)
{
	/*** If you need information from a Data class, use the cast below for portability ***/
  // TRtcDataClient *Client = static_cast<TRtcDataClient *>(Sender);

  Memo1->Lines->Add(Result->asString);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Edit1KeyPress(TObject *Sender, wchar_t &Key)
{
	if (Key == 13)
  {
  	ClientModule->Data->NewFunction("ReceiveMessage")->asString["Name"] = Edit1->Text;
		ClientModule->Call(Result);
  }
}
//---------------------------------------------------------------------------
