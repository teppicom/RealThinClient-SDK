//---------------------------------------------------------------------------
#ifndef wwwAppServerMainH
#define wwwAppServerMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "rtcConn.hpp"
#include "rtcDataSrv.hpp"
#include "rtcHttpSrv.hpp"
#include "rtcInfo.hpp"
#include "rtcFunction.hpp"
#include "rtcSrvModule.hpp"
//---------------------------------------------------------------------------
class TfrmAppServer : public TForm
{
__published:	// IDE-managed Components
	TRtcHttpServer *HttpServer;
	TRtcFunctionGroup *FunctionGroup;
	TRtcServerModule *ServerModule;
	TRtcFunction *funcReceiveMessage;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall funcReceiveMessageExecute(TRtcConnection *Sender, TRtcFunctionInfo *Param,
          TRtcValue *Result);
private:	// User declarations
public:		// User declarations
	__fastcall TfrmAppServer(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmAppServer *frmAppServer;
//---------------------------------------------------------------------------
#endif
