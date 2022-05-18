//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
#include "wwwAppServerMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "rtcConn"
#pragma link "rtcDataSrv"
#pragma link "rtcHttpSrv"
#pragma link "rtcInfo"
#pragma link "rtcSystem"
#pragma link "rtcFunction"
#pragma link "rtcSrvModule"
#pragma resource "*.dfm"
TfrmAppServer *frmAppServer;
//---------------------------------------------------------------------------
__fastcall TfrmAppServer::TfrmAppServer(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmAppServer::FormCreate(TObject *Sender)
{
	HttpServer->Listen();
}
//---------------------------------------------------------------------------
void __fastcall TfrmAppServer::funcReceiveMessageExecute(TRtcConnection *Sender, TRtcFunctionInfo *Param,
          TRtcValue *Result)
{
	/*** If you need information from a Data class, use the cast below for portability ***/
	// TRtcDataServer *Server = static_cast<TRtcDataServer *>(Sender);

  Result->asString = "Hello World " + Param->asString["Name"];
}
//---------------------------------------------------------------------------
