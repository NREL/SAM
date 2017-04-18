#ifndef __LK_h
#define __LK_h

#include <wx/wx.h>
#include <wx/scrolbar.h>
#include <wx/print.h>
#include <wx/listctrl.h>
#include <wx/printdlg.h>
#include <wx/aui/auibook.h>
#include <wx/snglinst.h>

class LKFrame;
class LKDocWin;
class CodeEdit;

extern LKFrame *app_frame;
extern wxArrayString app_args;
extern wxConfig *app_config;

void applog(const wxString &s);
void applog(const char *fmt, ...);

extern int LK_major_ver;
extern int LK_minor_ver;
extern int LK_micro_ver;

/***********************************************************/

class LKApp : public wxApp
{
public:
	LKApp();
	bool OnInit();
	int OnExit();	

	wxString GetInstanceName();
	void OnFatalException();
private:
	wxString m_inst_name;
	wxSingleInstanceChecker *m_inst_checker;
};

DECLARE_APP(LKApp)

#define MAX_RECENT 9

class LKFrame : public wxFrame
{
public:
	LKFrame();
	void SaveCode();
	void LoadCode();
	void Post(const wxString &text) { m_txtOutput->AppendText(text); }
private:	

	void OnDocumentCommand(wxCommandEvent &evt);
	void OnCloseFrame(wxCloseEvent &evt);
	void OnCharHook(wxKeyEvent &evt);

	CodeEdit *m_codeEdit;
	wxTextCtrl *m_txtOutput;
	
	DECLARE_EVENT_TABLE()
};
#endif
