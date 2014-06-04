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
	static wxString GetVersionAndPlatform();
	static bool IncrementUsage();
	static bool CheckInWithServer( int *total_usage_count = 0 );
	static int CountSinceLastVerify();
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
