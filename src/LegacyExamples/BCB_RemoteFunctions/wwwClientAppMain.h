//---------------------------------------------------------------------------

#ifndef wwwClientAppMainH
#define wwwClientAppMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "rtcConn.hpp"
#include "rtcDataCli.hpp"
#include "rtcHttpCli.hpp"
#include "rtcInfo.hpp"
#include "rtcCliModule.hpp"
#include "rtcFunction.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TEdit *Edit1;
	TMemo *Memo1;
	TRtcHttpClient *HttpClient;
	TRtcClientModule *ClientModule;
	TRtcResult *Result;
	void __fastcall ResultReturn(TRtcConnection *Sender, TRtcValue *Data, TRtcValue *Result);
	void __fastcall Edit1KeyPress(TObject *Sender, wchar_t &Key);

private:	// User declarations
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
