#ifndef __tpdlg_h
#define __tpdlg_h

#include <vector>
#include <wx/dialog.h>

class wxGauge;
class wxTextCtrl;
class wxStaticText;
class wxMetroButton;

class wxThreadProgressDialog : public wxDialog
{
public:
	wxThreadProgressDialog(wxWindow *parent, int nthreads, bool border=false);
	void Cancel() { m_canceled = true; }
	bool IsCanceled() { return m_canceled; }
	void Log( const wxArrayString &list );
	void Log( const wxString &text );
	void Update(int ThreadNum, float percent, const wxString &label = wxEmptyString );
	void Status( const wxString & );
	void Reset();
	void ShowBars( int n=-1 );
	void SetButtonText( const wxString &txt );
	bool HasMessages();
	wxString GetMessages();
	void ShowSaveLogButton();

	void Finalize( const wxString &title = wxEmptyString ); // if messages shown, switch to modal view

	void OnCancel(wxCommandEvent &evt);
	void OnClose( wxCommandEvent &evt );
	void OnDialogClose(wxCloseEvent &evt);
	void OnSaveLog( wxCommandEvent & );
protected:
	bool m_canceled;
	std::vector<wxStaticText*> m_labels;
	std::vector<wxGauge*> m_progbars;
	std::vector<wxTextCtrl*> m_percents;
	wxStaticText *m_status;
	wxTextCtrl *m_log;
	wxMetroButton *m_button, *m_saveLog;
	wxFrame *m_transp;

	
	DECLARE_EVENT_TABLE()
};

#endif
