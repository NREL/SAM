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
	static void DecrementUsage();
	static bool CheckInWithServer( int *total_usage_count = 0 );
	static int CountSinceLastVerify();
	static bool CanStart();
	static int AllowedStartsRemaining();
	static bool ShowDialog( const wxString &msg = wxEmptyString, const wxString &btn = wxEmptyString ); // returns false on cancel

private:

	wxTextCtrl *m_email;
	wxTextCtrl *m_key;
	wxTextCtrl *m_output;
	wxMetroButton *m_close, *m_register;

	void OnRegister( wxCommandEvent & );
	void OnConfirm( wxCommandEvent & );
	void OnHelp( wxCommandEvent & );
	void OnEmail( wxCommandEvent & );

	DECLARE_EVENT_TABLE();
};


#endif
