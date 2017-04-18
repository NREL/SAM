#ifndef __wex_registration_h
#define __wex_registration_h

#include <wx/dialog.h>

class wxTextCtrl;
class wxMetroButton;

class wxOnlineRegistrationData
{
public:
	wxOnlineRegistrationData();
	virtual ~wxOnlineRegistrationData();

	virtual wxString GetAppName() = 0;
	virtual wxString GetOrganization() = 0;

	virtual bool IsAnOverrideKey( const char * ) = 0;
	virtual wxString GetLocalRegistrationFile() = 0;
	virtual wxString ReadSetting( const wxString & ) = 0;
	virtual void WriteSetting( const wxString &, const wxString & ) = 0;
	
	virtual wxString GetVersionAndPlatform( ) = 0;
	virtual wxString GetNoticeText() = 0;
	
	enum Endpoint { CHECK_IN, RESEND_KEY, REGISTER_NEW };

	virtual bool GetApi( Endpoint ept, wxString *url, wxString *post ) = 0;

	virtual void ShowHelp() = 0;
	virtual wxString ReadProxy() = 0;
	virtual bool WriteProxy( const wxString & ) = 0;
};

class wxOnlineRegistration : public wxDialog
{
public:
	wxOnlineRegistration( wxWindow *parent );

	static void Init( wxOnlineRegistrationData * );	

	static bool CheckRegistration();
	static void EnableDebugMessages(bool b );

	static wxString GetEmail();
	static wxString GetKey();
	static wxString GetVersionAndPlatform();
	static bool IncrementUsage();
	static void DecrementUsage();
	static bool CheckInWithServer( int *total_usage_count = 0 );
	static int CountSinceLastVerify();
	static bool CanStart();
	static int AllowedStartsRemaining();
	static bool ShowDialog( 
		const wxString &msg = wxEmptyString, 
		const wxString &btn = wxEmptyString ); // returns false on cancel
	static bool ShowNotice();


private:

	wxTextCtrl *m_email;
	wxTextCtrl *m_key;
	wxTextCtrl *m_output;
	wxMetroButton *m_close, *m_register;

	void OnRegister( wxCommandEvent & );
	void OnConfirm( wxCommandEvent & );
	void OnHelp( wxCommandEvent & );
	void OnEmail( wxCommandEvent & );
	void OnProxySetup( wxCommandEvent & );

	DECLARE_EVENT_TABLE();
};


#endif
