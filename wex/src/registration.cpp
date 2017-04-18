#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/sizer.h>
#include <wx/platinfo.h>
#include <wx/config.h>
#include <wx/app.h>
#include <wx/msgdlg.h>

#include <wex/metro.h>
#include <wex/utils.h>
#include <wex/jsonreader.h>
#include <wex/easycurl.h>

#include <wex/registration.h>

static bool gs_enDebug = false;

void wxOnlineRegistration::EnableDebugMessages( bool b )
{
	gs_enDebug = b;
}

static wxOnlineRegistrationData *gs_regData = 0;
wxOnlineRegistrationData::wxOnlineRegistrationData() {
	// nothing to do
}
wxOnlineRegistrationData::~wxOnlineRegistrationData() {
	// nothing to do
}

void wxOnlineRegistration::Init( wxOnlineRegistrationData *dd )
{
	gs_regData = dd;
}

static bool GetFileRegistration( wxString *email, wxString *key )
{
static bool s_filereg = false;
static wxString s_email, s_key;
static bool first_run = true;

	if ( first_run && gs_regData )
	{
		first_run = false;

		wxString regfile( gs_regData->GetLocalRegistrationFile() );
		if ( FILE *f = fopen( regfile.c_str(), "r" ) )
		{
			char buf[256];
			fgets( buf, 255, f );
			s_email = wxString( buf ).Trim().Trim(true);
			fgets( buf, 255, f );
			s_key = wxString( buf ).Trim().Trim(true);
			fclose( f );

			s_filereg = s_email.Find("@")!=wxNOT_FOUND && s_key.Find("-")!=wxNOT_FOUND;
		}
	}

	if ( s_filereg )
	{
		if ( email ) *email = s_email;
		if ( key ) *key = s_key;
	}

	return s_filereg;
}

bool wxOnlineRegistration::CheckRegistration()
{	
	if ( !gs_regData ) return false;

	wxString app = gs_regData->GetAppName();
	wxString org = gs_regData->GetOrganization();

	wxString email = wxOnlineRegistration::GetEmail();
	wxString key = wxOnlineRegistration::GetKey();
	
	if ( email.IsEmpty() || key.IsEmpty() )
	{
		wxOnlineRegistration::ShowDialog();
	}
		
	if ( !wxOnlineRegistration::IncrementUsage() )
	{
		if ( !wxOnlineRegistration::CanStart() )
		{
			wxOnlineRegistration::ShowDialog( "You have reached the limit for the number of times you can run " +
				app + " without verifying your registration key.\n\n"
				"Please check your email address, verification code, and internet connection." );

			if ( !wxOnlineRegistration::CanStart() )
				return false;
		}
		else 
		{
			wxString text = (app + " could not connect to " + org + " servers to verify your registration key.\n"
				"This might be caused by a problem with your internet connection. "
				"Click Confirm to try again or Skip for now to continue without registering.\n\n" );
			int nstarts = wxOnlineRegistration::AllowedStartsRemaining();
			if ( nstarts == 1 )	text += "This is the last time you may run " + app + " without verifying your registration.\n\n";
			else text += wxString::Format( "You may run " + app + " %d more times without your verifying your registration.\n\n", nstarts );

			if ( !wxOnlineRegistration::ShowDialog( text, wxString::Format("Skip for now (%d left)", nstarts) ) )
			{
				// since app is not going to start, decrement usage count (it was incremented already for this start above)
				wxOnlineRegistration::DecrementUsage();
				return false;
			}
		}
	}

	return true;
}

wxString wxOnlineRegistration::GetEmail()
{
	if ( !gs_regData ) return wxEmptyString;

	wxString email;
	if ( !GetFileRegistration( &email, 0 ) )
		email = gs_regData->ReadSetting( "user-email-" + GetVersionAndPlatform() );

	return email;
}

wxString wxOnlineRegistration::GetKey()
{
	if ( !gs_regData ) return wxEmptyString;

	wxString key;
	if ( !GetFileRegistration( 0, &key ) )
		key = gs_regData->ReadSetting( "user-key-" + GetVersionAndPlatform() );
	return key;
}

#define MAX_ATTEMPTS 15

bool wxOnlineRegistration::CanStart()
{
	if ( !gs_regData ) return false;
	return gs_regData->IsAnOverrideKey( GetKey().c_str() ) || CountSinceLastVerify() < MAX_ATTEMPTS;
}

int wxOnlineRegistration::AllowedStartsRemaining()
{
	return MAX_ATTEMPTS - CountSinceLastVerify();
}

int wxOnlineRegistration::CountSinceLastVerify()
{
	int count = 0;
	if ( gs_regData )
		count = wxAtoi( gs_regData->ReadSetting( "count-since-last-verify-" + GetVersionAndPlatform() ) );
	return count;
}

bool wxOnlineRegistration::ShowDialog( const wxString &msg, const wxString &btn )
{
	wxOnlineRegistration regdlg( 0 );
	regdlg.CenterOnScreen();
	if ( !msg.IsEmpty() )
	{
		regdlg.m_output->SetForegroundColour( *wxRED );
		regdlg.m_output->SetValue( msg );
	}

	if ( !btn.IsEmpty() ) regdlg.m_close->SetLabel( btn );
	regdlg.Layout();
	return regdlg.ShowModal() == wxID_OK;
}

bool wxOnlineRegistration::IncrementUsage()
{
	if ( !gs_regData ) return false;

	int count = CountSinceLastVerify();
	// don't increment local count if SAM won't start
	// but do increment if using an override key
	if ( count < MAX_ATTEMPTS || gs_regData->IsAnOverrideKey(GetKey().c_str()))
		gs_regData->WriteSetting("count-since-last-verify-" + GetVersionAndPlatform(), wxString::Format("%d", count+1) );

	return CheckInWithServer();
}

void wxOnlineRegistration::DecrementUsage()
{
	if ( !gs_regData ) return;

	gs_regData->WriteSetting("count-since-last-verify-" + GetVersionAndPlatform(),
		wxString::Format("%d", CountSinceLastVerify()-1) );
}
	

bool wxOnlineRegistration::CheckInWithServer( int *usage_count )
{
	if ( !gs_regData ) return false;

	// now try to sync with the server using the current email and key
	wxString email = GetEmail();
	wxString key = GetKey();
	
	if ( gs_regData->IsAnOverrideKey( GetKey().c_str() ) )
		return true;

	if ( email.IsEmpty() || key.IsEmpty() )
		return false;
		
	wxBusyCursor curs;
	wxEasyCurl curl;
	
	wxString url, post; 
	gs_regData->GetApi( wxOnlineRegistrationData::CHECK_IN, &url, &post );

	curl.SetPostData( post );
	curl.Get( url );
	
	if ( usage_count ) *usage_count = -999;
	
	wxJSONValue root;
	wxJSONReader reader;
	wxString raw( curl.GetDataAsString() );

	if ( gs_enDebug )
	{
		wxLogStatus("wxOnlineRegistration::CheckInWithServer");
		wxLogStatus("\turl: " + url );
		wxLogStatus("\tpost: " + post );
		wxLogStatus("\tresponse: " + raw );
}

	if ( reader.Parse( raw, &root ) == 0 )
	{
		int code = root.Item("status").AsInt();
		if ( gs_enDebug ) wxLogStatus("\tcode: %d", code);

		if (code == 200)
		{
			gs_regData->WriteSetting("count-since-last-verify-" + GetVersionAndPlatform(), wxString::Format("%d", 0) );
			if ( usage_count ) *usage_count = root.Item("result").Item("usage_count").AsInt();
			return true;
		}
	}

	return false;
}


wxString wxOnlineRegistration::GetVersionAndPlatform()
{
	if ( !gs_regData ) return wxEmptyString;
	else return gs_regData->GetVersionAndPlatform();
}

enum { ID_REGISTER = wxID_HIGHEST+134, ID_CONFIRM, ID_EMAIL, ID_PROXYSETUP };

#define SPACE 15

BEGIN_EVENT_TABLE( wxOnlineRegistration, wxDialog )
	EVT_BUTTON( ID_REGISTER, wxOnlineRegistration::OnRegister )
	EVT_BUTTON( ID_CONFIRM, wxOnlineRegistration::OnConfirm )
	EVT_BUTTON( wxID_HELP, wxOnlineRegistration::OnHelp )
	EVT_BUTTON( ID_PROXYSETUP, wxOnlineRegistration::OnProxySetup )
	EVT_TEXT( ID_EMAIL, wxOnlineRegistration::OnEmail )
END_EVENT_TABLE()


wxOnlineRegistration::wxOnlineRegistration( wxWindow *parent )
	: wxDialog( parent, wxID_ANY, 
		gs_regData ? gs_regData->GetAppName() + " Registration" : "Software Registration", 
		wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE )
{
	wxFont font( wxMetroTheme::Font( wxMT_LIGHT, 12 ) );
	SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );

	wxPanel *panel = new wxPanel( this );
	panel->SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

	wxFlexGridSizer *grid = new wxFlexGridSizer( 3 );
	grid->AddGrowableCol(1);
	wxStaticText *label = 0;
	
	label = new wxStaticText( panel, wxID_ANY, "Email:" );
	label->SetFont( font );
	label->SetForegroundColour( *wxWHITE );
	grid->Add( label, 0, wxLEFT|wxTOP|wxBOTTOM|wxALIGN_CENTER_VERTICAL|wxALIGN_RIGHT, SPACE );

	m_email = new wxTextCtrl( panel, ID_EMAIL, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );
#ifdef __WXMSW__
	m_email->SetFont( font );
#endif
	grid->Add( m_email, 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, SPACE );
	
	m_register = new wxMetroButton( panel, ID_REGISTER, "Register" );
	grid->Add( m_register, 0, wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER|wxRIGHT, SPACE );
	
	label = new wxStaticText( panel, wxID_ANY, "Key:" );
	label->SetFont( font );
	label->SetForegroundColour( *wxWHITE );
	grid->Add( label, 0, wxLEFT|wxTOP|wxBOTTOM|wxALIGN_CENTER_VERTICAL|wxALIGN_RIGHT, SPACE );

	m_key = new wxTextCtrl( panel, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );
#ifdef __WXMSW__
	m_key->SetFont( font );
#endif
	grid->Add( m_key, 1, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, SPACE );
	grid->Add( new wxMetroButton( panel, ID_CONFIRM, "Confirm" ), 0, wxALIGN_CENTER_VERTICAL|wxALIGN_CENTER|wxRIGHT, SPACE );

	panel->SetSizer(grid);
	
	m_output = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE|wxTE_READONLY|wxTE_MULTILINE );
	m_output->SetBackgroundColour( *wxWHITE );
	m_output->SetForegroundColour( wxMetroTheme::Colour( wxMT_TEXT ) );
	m_output->SetInitialSize( wxScaleSize(550, 150 ) );
	m_output->SetFont( font );
	wxString appname;
	if ( gs_regData ) appname = gs_regData->GetAppName();
	m_output->SetValue("To register " + appname + ":\n"
		"1. Type your email address and click Register.\n"
		"    You should receive an email with a registration key.\n"
		"2. Copy and paste the key from the email and click Confirm.\n\n"
        "If you are already registered but cannot find your key, type your email address and click Register. To continue without registering, click Close.");
	

	sizer->Add( panel, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( m_output, 1, wxALL|wxEXPAND, 0 );

	
	m_close = new wxMetroButton( this, wxID_OK, "Close" );

	wxSizer *tools = new wxBoxSizer( wxHORIZONTAL );
	tools->Add( new wxMetroButton( this, wxID_HELP, "Help" ), 0 );
	tools->Add( new wxMetroButton( this, ID_PROXYSETUP, "Proxies..." ), 0 );
	tools->AddStretchSpacer();
	tools->Add( m_close, 0 );

	sizer->Add( tools, 0, wxALL|wxEXPAND, 0  );


	SetSizerAndFit( sizer );
	wxTheApp->Yield( true );

	m_email->SetValue( GetEmail() );
	m_email->SelectNone();
	
	m_key->SetValue( GetKey() );
	m_email->SelectNone();

	if ( GetFileRegistration( 0, 0 ) )
	{
		m_email->SetForegroundColour( *wxLIGHT_GREY );
		m_key->SetForegroundColour( *wxLIGHT_GREY );
		m_email->Enable( false );
		m_key->Enable( false );
		m_output->SetValue( "Email and key loaded from registration.txt\n\n"
			"Edit this file manually to change the email address and key associated with this installation, "
			"or delete the file to enable this registration page." );
	}

	m_close->SetFocus();
}

bool wxOnlineRegistration::ShowNotice()
{
	if ( !gs_regData ) return true;
	wxString text( gs_regData->GetNoticeText( ) );

	wxDialog dlg(NULL, wxID_ANY, 		
		"Registration Information", wxDefaultPosition, wxDefaultSize, 
		wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER);
	dlg.SetBackgroundColour( *wxWHITE );
	wxStaticText *tt = new wxStaticText( &dlg, wxID_ANY, text );
	tt->SetFont( wxMetroTheme::Font( wxMT_NORMAL, 10 ) );
	tt->SetForegroundColour( wxColour(120,120,120) );
	tt->Wrap( (int)(450*wxGetScreenHDScale()) );
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( tt, 0, wxALL|wxEXPAND, 15 );
	sizer->Add( dlg.CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxCENTER, 15 );
	dlg.SetSizerAndFit(sizer);
	dlg.CenterOnScreen();
	return dlg.ShowModal() == wxID_OK;		
}

void wxOnlineRegistration::OnRegister( wxCommandEvent & )
{
	if( !gs_regData )
		return;

	wxString email = m_email->GetValue();
	if ( email.IsEmpty() || email.Find("@")==wxNOT_FOUND || email.Find(".")==wxNOT_FOUND )
	{
		wxMessageBox("Please enter a valid email address to register.", "Notice");
		return;
	}
	
	// save the email address in settings.
	gs_regData->WriteSetting("user-email-" + GetVersionAndPlatform(), email );	
	wxEasyCurl::SetUrlEscape( "<USEREMAIL>", email );

	wxBusyCursor curs;
	wxEasyCurl curl( this, wxID_ANY );
	int code = -1;
	wxJSONValue root;
	wxJSONReader reader;
	
	m_output->SetForegroundColour( wxMetroTheme::Colour( wxMT_TEXT ) );

	if ( m_register->GetLabel() == "Resend key" )
	{
	//	https://developer.nrel.gov/api/sam/v1/tracker/resend_key?api_key=SAMAPIKEY&email=someusersemail@somedomain.com
		wxString url, post;
		gs_regData->GetApi( wxOnlineRegistrationData::RESEND_KEY, &url, &post );
		curl.SetPostData(post);
		curl.Get( url );
		
		wxString raw( curl.GetDataAsString() );
		if ( reader.Parse( raw, &root ) == 0 )
			code = root.Item("status").AsInt();
		
		if ( gs_enDebug )
		{
			wxLogStatus("wxOnlineRegistration::OnRegister (Resend key)");
			wxLogStatus("\turl: " + url);
			wxLogStatus("\tpost: " + post );
			wxLogStatus("\tresponse: " + raw );
			wxLogStatus("\tcode: %d", code);
		}
		
		if ( code == 404 ) m_output->SetValue("No user exists with that email address." );
		else if ( code == 200 ) m_output->SetValue("An email with your registration key has been sent to " + email + ". Paste the key from the email into the box above and click 'Confirm' to register.");
		else m_output->SetValue( "An unknown error occurred.  Please check your internet connection." );

		return;
	}
	
	wxString url, post;
	gs_regData->GetApi( wxOnlineRegistrationData::REGISTER_NEW, &url, &post );
		
	curl.SetPostData( post );
	curl.Get( url );		

	wxString raw( curl.GetDataAsString() );
	if ( reader.Parse( raw, &root ) == 0 )
		code = root.Item("status").AsInt();
		
	if ( gs_enDebug )
	{
		wxLogStatus("wxOnlineRegistration::OnRegister (Resend key)");
		wxLogStatus("\turl: " + url);
		wxLogStatus("\tpost: " + post );
		wxLogStatus("\tresponse: " + raw );
		wxLogStatus("\tcode: %d", code);
	}

	if ( code == 200 ) m_output->SetValue( "Registration successful!  You have been sent an email with a registration key.");
	else if ( code == 409 ) 
	{
		m_output->SetValue("You are already registered.  Please enter the registration key sent to you by email when you first registered and click 'Confirm' above.\n\nYou can click 'Resend key' to have your key emailed to you again." );
		m_register->SetLabel( "Resend key");
		m_register->Refresh();
		Layout();
	}
	else if ( code == 404 ) m_output->SetValue("Your registration information was not correct.  No user exists with that registration key." );
	else m_output->SetValue(wxString::Format("Registration failed with error code %d.  Please check your internet connection.", code));

	/*
	if ( code != 200 )
		m_output->AppendText( "\n\n" + raw );
		*/	
}

void wxOnlineRegistration::OnConfirm( wxCommandEvent & )
{
	if ( !ShowNotice() || !gs_regData ) return;

	m_output->SetForegroundColour( wxMetroTheme::Colour( wxMT_TEXT ) );
	wxBusyCursor curs;
	wxString email = m_email->GetValue().Trim().Trim(false);
	wxString key = m_key->GetValue().Trim().Trim(false);
	
	gs_regData->WriteSetting("user-email-" + GetVersionAndPlatform(), email );
	gs_regData->WriteSetting("user-key-" + GetVersionAndPlatform(), key );

	int total_usage = 0;
	if ( !CheckInWithServer( &total_usage ) )
		m_output->SetValue("The registration key could not be verified. Please check the key and internet connection.");
	else
	{
		m_output->SetValue(wxString::Format(
			"Registration successful!\n\n"
			"You have used %s %d times.", 
			(const char*)gs_regData->GetAppName().c_str(), total_usage ));
		wxYield();
		wxMilliSleep( 1000 );
		EndModal( wxID_OK );
	}
}


void wxOnlineRegistration::OnHelp( wxCommandEvent & )
{
	if ( gs_regData ) gs_regData->ShowHelp();
}

void wxOnlineRegistration::OnEmail( wxCommandEvent & )
{
	if ( m_register->GetLabel() != "Register" )
	{
		m_register->SetLabel( "Register" );
		m_register->Refresh();
		Layout();
	}
}

void wxOnlineRegistration::OnProxySetup( wxCommandEvent & )
{
	if ( !gs_regData ) return;

	wxString proxy( gs_regData->ReadProxy() );

	wxDialog dlg( this, wxID_ANY, "Configure Proxy", wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER );
	wxTextCtrl *text = new wxTextCtrl( &dlg, wxID_ANY, proxy );
	
	wxString label;
#ifdef __WXMSW__
	label += "By default, the software will try to automatically detect proxies on Windows\n"
		     "from Internet Explorer settings.  If you wish to use a specific proxy,\n"
			 "enter the information below.  Otherwise, leave it blank.\n\n";
#else
	
	label += "Enter proxy address (leave empty for direct internet connection):\n\n";
#endif
	label += 
		"  Example:               proxy-server.myorganization.org\n"
		"  With a custom port:    proxy-server.myorganization.org:9142\n";


	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( new wxStaticText( &dlg, wxID_ANY, label), 0, wxALL|wxALIGN_CENTER_VERTICAL, 10 );
	
	sizer->Add( text, 0, wxALL|wxEXPAND, 10 );
	sizer->Add( dlg.CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );
	dlg.SetSizerAndFit( sizer );
	dlg.CenterOnParent();

	if ( dlg.ShowModal() == wxID_OK )
	{
		proxy = text->GetValue();
		wxEasyCurl::SetProxyAddress( proxy );
		if ( ! gs_regData->WriteProxy( proxy ) )
			wxMessageBox("Could not save proxy configuration to installation folder.\n\n"
				"Please ensure that you have write permissions to the installation folder.");
	}
}
