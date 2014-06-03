#ifndef __registration_h
#define __registration_h

#include <wx/dialog.h>

class wxTextCtrl;
class wxMetroButton;

class SamRegistration : public wxDialog
{
public:
	SamRegistration( wxWindow *parent );
	
	static wxString GetEmail();
	static wxString GetKey();
	static int GetCount();
	static wxString GetVersionAndPlatform();
	static int LastLoggedCount();
	static bool IncrementUsage();
	static bool ConfirmWithServer();
	static bool CanStart();
	static int AllowedStartsRemaining();
	static void ShowDialog( const wxString &msg = wxEmptyString, const wxString &btn = wxEmptyString );

private:

	wxTextCtrl *m_email;
	wxTextCtrl *m_key;
	wxTextCtrl *m_output;
	wxMetroButton *m_close;

	void OnRegister( wxCommandEvent & );
	void OnConfirm( wxCommandEvent & );
	void OnHelp( wxCommandEvent & );

	DECLARE_EVENT_TABLE();
};


#endif
