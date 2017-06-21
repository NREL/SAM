#include <cstdio>
#include <cstdarg>
#include <cstring>
#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <limits>
#include <climits>
#include <math.h>
#include <float.h>
#include <string.h>

#include <lk/stdlib.h>

#include "sqlite3.h"

#ifndef M_PI
#define M_PI 3.141592653589793238462643
#endif


#ifdef LK_USE_WXWIDGETS
#include <wx/wx.h>
#include <wx/toplevel.h>
#include <wx/html/htmlwin.h>
#include <wx/choicdlg.h>
#include <wx/filename.h>
#include <wx/progdlg.h>

static wxWindow *GetCurrentTopLevelWindow()
{
	wxWindowList &wl = ::wxTopLevelWindows;
	for( wxWindowList::iterator it = wl.begin(); it != wl.end(); ++it )
		if ( wxTopLevelWindow *tlw = dynamic_cast<wxTopLevelWindow*>( *it ) )
			if ( tlw->IsActive() )
				return tlw;

	return 0;
}

class MyMessageDialog : public wxDialog
{
public:
	MyMessageDialog( wxWindow *parent,
		const wxString &message,
		const wxString &title,
		long buttons,
		const wxPoint &pos = wxDefaultPosition,
		const wxSize &size = wxDefaultSize )
		: wxDialog( parent, wxID_ANY, title, pos, size, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		SetEscapeId( wxID_NONE );

		wxPanel *panel = new wxPanel( this );
		panel->SetBackgroundColour( *wxWHITE );

		wxBoxSizer *szpnl = new wxBoxSizer( wxVERTICAL );
		

		int wrap = 600;
		if ( size != wxDefaultSize && size.x > 100 )
			wrap = size.x - 40;


		int nlpos  =  message.Find( '\n' );
		if ( nlpos > 0 )
		{
			wxStaticText *label1 = new wxStaticText( panel, wxID_ANY, message.Left(nlpos), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
			wxFont font( label1->GetFont() );
			font.SetPointSize( font.GetPointSize() + 2 );
			label1->SetFont( font );
			label1->SetForegroundColour( wxColour(0,0,120) );
			label1->Wrap( wrap );

			wxStaticText *label2 = new wxStaticText( panel, wxID_ANY, message.Mid(nlpos+1), wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
			label2->Wrap( wrap );
			
			szpnl->Add( label1, 0, wxTOP|wxLEFT|wxRIGHT|wxEXPAND, 20 );
			szpnl->Add( label2, 1,  wxALL|wxEXPAND, 20 );

		}
		else
		{
			wxStaticText *label = new wxStaticText( panel, wxID_ANY, message, wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
			label->Wrap( wrap );

			szpnl->Add( label, 1, wxALL|wxEXPAND, 20 );
		}

		panel->SetSizer( szpnl );
				
		wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
		sizer->Add( panel, 1, wxALL|wxEXPAND, 0 );
		sizer->Add( CreateButtonSizer( buttons ), 0, wxALL|wxEXPAND, 11 );
		SetSizerAndFit( sizer );
		
		if ( size != wxDefaultSize )
			SetClientSize( size );
		else
		{
			wxSize sz = GetClientSize();
			if ( sz.x < 340 ) sz.x = 340;
			if ( sz.y < 120 ) sz.y = 120;
			SetClientSize( sz );
		}
		
		if ( pos == wxDefaultPosition )
		{
			if ( parent )
				CenterOnParent();
			else 
				CenterOnScreen();
		}
	}

	void OnClose( wxCloseEvent &evt )
	{
		EndModal( wxID_CANCEL );
	}

	void OnCharHook( wxKeyEvent &evt )
	{
		if ( evt.GetKeyCode() == WXK_ESCAPE )
			EndModal( wxID_CANCEL );
	}

	void OnCommand( wxCommandEvent &evt )
	{
		EndModal( evt.GetId() );
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( MyMessageDialog, wxDialog )
	EVT_BUTTON( wxID_YES, MyMessageDialog::OnCommand )
	EVT_BUTTON( wxID_NO, MyMessageDialog::OnCommand )
	EVT_BUTTON( wxID_CANCEL, MyMessageDialog::OnCommand )
	EVT_BUTTON( wxID_OK, MyMessageDialog::OnCommand )
	EVT_CLOSE( MyMessageDialog::OnClose )
	EVT_CHAR_HOOK( MyMessageDialog::OnCharHook )
END_EVENT_TABLE()

static void run_message_box( lk::invoke_t &cxt, long style, const wxString &title )
{
	wxPoint pt( wxDefaultPosition );	
	wxSize sz( wxDefaultSize );

	if ( cxt.arg_count() > 1 )
	{
		lk::vardata_t &pos = cxt.arg(1).deref();
		pt.x = pos.index(0)->as_integer();
		pt.y = pos.index(1)->as_integer();

		if ( pos.length() > 2 )
		{
			sz.x = pos.index(2)->as_integer();
			sz.y = pos.index(3)->as_integer();
		}
	}
	else
		style |= wxCENTER;
	
	MyMessageDialog dlg( GetCurrentTopLevelWindow(), cxt.arg(0).as_string(), title, style, pt, sz );
	int ret = dlg.ShowModal();	
    cxt.result().assign( (ret==wxID_OK||ret==wxID_YES) ? 1.0 : 0.0 ) ;
}

static void _wx_msgbox( lk::invoke_t &cxt )
{
	LK_DOC("msgbox", "Shows a message dialog box.", "(string:message, [array:window position [x,y] or geometry [x,y,w,h]):boolean");
	run_message_box( cxt, wxOK, "Notice" );

}

static void _wx_yesno( lk::invoke_t &cxt )
{
	LK_DOC("yesno", "Shows a message box with yes and no buttons.  The function returns true when yes is clicked, false otherwise.", "(string:message, [array:window position [x,y] or geometry [x,y,w,h]]):boolean");
	run_message_box( cxt, wxYES_NO, "Query" );
}

class MyProgressDialog : public wxProgressDialog
{
	lk::objref_t *m_ref;
public:
	MyProgressDialog( lk::objref_t *ref, const wxString &title, const wxString &message, int max, wxWindow *parent, long style )
		: wxProgressDialog( title, message, max, parent, style )
	{
		m_ref = ref;
	}

	void ClearRef() { m_ref = 0; }

	virtual ~MyProgressDialog() {
		// destroy and de-register the object from the environment
		// if the dialog box is destroyed because the parent window is closed
		// or for some other reason, and the reference is no longer valid
		if ( m_ref ) m_ref->get_env()->destroy_object( m_ref );
	}
	
};

class prgdlg_t : public lk::objref_t
{
public:
	MyProgressDialog *m_dlg;
	prgdlg_t() { m_dlg = 0; }
	virtual ~prgdlg_t() { if ( m_dlg ){ m_dlg->ClearRef(); m_dlg->Destroy(); } }
	virtual lk_string type_name() { return "progressbar"; }
};

static void _wx_progressbar( lk::invoke_t &cxt )
{
	LK_DOC( "progressbar", "Shows a progress bar dialog.  "
		"Call this function with a table of options (message,title,cancelbutton:t/f,time:t/f,max) to create a new dialog, and the reference is returned.  "
		"Call this function with a reference and a table of options (message,value) to update the progress.  "
		"Call this function with a reference only to destroy an existing dialog.  "
		"Returns true if cancel button was pressed for an update call, or obj-ref when creating a new dialog.", "([obj-ref], table:options):[boolean or obj-ref]" );

	if ( cxt.arg_count() == 1 && cxt.arg(0).type() == lk::vardata_t::HASH )
	{
		int max = 100;
		wxString title("Progress"), message("Please wait...");
		bool cancelbutton = false;
		bool showtime = false;

		lk::vardata_t &opt = cxt.arg(0);
		if ( lk::vardata_t *x = opt.lookup( "message" ) )
			message = x->as_string();
		if ( lk::vardata_t *x = opt.lookup( "title" ) )
			title = x->as_string();
		if ( lk::vardata_t *x = opt.lookup( "cancelbutton" ) )
			cancelbutton = x->as_boolean();
		if ( lk::vardata_t *x = opt.lookup( "time" ) )
			showtime = x->as_boolean();
		if ( lk::vardata_t *x = opt.lookup( "max" ) )
			max = x->as_integer();

		long style = wxPD_SMOOTH|wxPD_APP_MODAL;
		if ( cancelbutton ) style |= wxPD_CAN_ABORT;
		if ( showtime ) style |= wxPD_ELAPSED_TIME;

		prgdlg_t *ref = new prgdlg_t;
		ref->m_dlg = new MyProgressDialog( ref, title, message, max, GetCurrentTopLevelWindow(), style );		
		ref->m_dlg->Show();
		size_t iref = cxt.env()->insert_object( ref );
		cxt.result().assign( (double)iref );
	}
	else if ( cxt.arg_count() == 1 && cxt.arg(0).type() == lk::vardata_t::NUMBER )
	{
		if ( prgdlg_t *ref = dynamic_cast<prgdlg_t*>( cxt.env()->query_object( cxt.arg(0).as_unsigned() ) ) )
		{
			ref->m_dlg->ClearRef();
			ref->m_dlg->Destroy();
			cxt.env()->destroy_object( ref );
		}
	}
	else if ( cxt.arg_count() == 2 )
	{
		if ( prgdlg_t *ref = dynamic_cast<prgdlg_t*>( cxt.env()->query_object( cxt.arg(0).as_unsigned() ) ) )
		{
			wxString message;
			int value = -9999;
			lk::vardata_t &opt = cxt.arg(1);
			if ( lk::vardata_t *x = opt.lookup( "message" ) )
				message = x->as_string();
			if ( lk::vardata_t *x = opt.lookup( "value" ) )
				value = x->as_integer();

			ref->m_dlg->Update( value, message );	
			wxSafeYield( ref->m_dlg );
			cxt.result().assign( ref->m_dlg->WasCancelled() ? 1.0 : 0.0 );
		}
	}
}

static void _wx_in(  lk::invoke_t &cxt )
{
	LK_DOC("in", "Input text from the user using a popup dialog box.", "([string:message], [string:default value], [string:caption], [array:window position [x,y]]):string");
	wxString message("Standard input:"), capt( wxGetTextFromUserPromptStr ), defval;
	int x = wxDefaultCoord, y = wxDefaultCoord;
	bool cntr = true;

	if ( cxt.arg_count() > 0 )
		message = cxt.arg(0).as_string();
	if ( cxt.arg_count() > 1 )
		defval = cxt.arg(1).as_string();
	if ( cxt.arg_count() > 2 )
		capt = cxt.arg(2).as_string();
	if ( cxt.arg_count() > 3 )
	{
		lk::vardata_t &pos = cxt.arg(3).deref();
		x = pos.index(0)->as_number();
		y = pos.index(1)->as_number();
		cntr = false;
	}

	cxt.result().assign( wxGetTextFromUser(message, capt, defval, GetCurrentTopLevelWindow(), x, y, cntr ) );	
}

static void _wx_choose_from_list( lk::invoke_t &cxt )
{
	LK_DOC("choose_from_list", "Show a dialog for the user to select one item from a list", "(array:options, [string:message], [string:caption], [integer:initial selection], [array:window position [x,y] or geometry [x,y,w,h]]):string" );
	
	int x = wxDefaultCoord, y = wxDefaultCoord, width = wxCHOICE_WIDTH, height = wxCHOICE_HEIGHT;
	bool cntr = true;


	wxArrayString list;
	for( size_t i=0;i<cxt.arg(0).length();i++ )
		list.Add( cxt.arg(0).index(i)->as_string() );

	wxString msg("Make a selection:");
	if (cxt.arg_count() > 1 ) msg = cxt.arg(1).as_string();

	wxString capt("Query");
	if (cxt.arg_count() > 2 ) capt = cxt.arg(2).as_string();

	int isel = -1;
	if ( cxt.arg_count() > 3 ) isel = cxt.arg(3).as_integer();

	if( cxt.arg_count() > 4 && cxt.arg(4).deref().type() == lk::vardata_t::VECTOR )
	{
		cntr = false;
		if ( cxt.arg(4).length() == 2 )
		{
			x = (int)cxt.arg(4).index(0)->as_number();
			y = (int)cxt.arg(4).index(1)->as_number();
		}
		else if ( cxt.arg(4).length() == 4 )
		{
			x = (int)cxt.arg(4).index(0)->as_number();
			y = (int)cxt.arg(4).index(1)->as_number();
			width = (int)cxt.arg(4).index(2)->as_number();
			height = (int)cxt.arg(4).index(3)->as_number();
		}
	}
	
	wxSingleChoiceDialog sdlg( GetCurrentTopLevelWindow(), msg, capt, list );
	if ( isel > 0 ) sdlg.SetSelection( isel );

	if ( cntr )
	{
		sdlg.CenterOnParent();
	}
	else
	{
		sdlg.SetPosition( wxPoint(x,y) );
		sdlg.SetClientSize( wxSize(width,height) );
	}

	wxString result;
	if ( sdlg.ShowModal() == wxID_OK )
		result = sdlg.GetStringSelection();

	cxt.result().assign( result );
}

static void _wx_transp( lk::invoke_t &cxt )
{
static wxFrame *s_trans = 0;

	LK_DOC( "transp", "Create or hide a transparent overlay window on the current toplevel window.", "(array:x,y,width.height, [variant:color], [number:transparency level 0-255]):none" );

	if ( s_trans )
	{
		s_trans->Destroy();
		s_trans = 0;
	}

	if ( cxt.arg_count() > 0 )
	{
		wxPoint pos(0, 0);
		wxSize size( 100, 100 );

		lk::vardata_t &geom = cxt.arg(0).deref();
		pos.x = geom.index(0)->as_integer();
		pos.y = geom.index(1)->as_integer();
		size.x = geom.index(2)->as_integer();
		size.y = geom.index(3)->as_integer();

		wxColour color( *wxLIGHT_GREY );
		int itran = 230;

		if ( cxt.arg_count() > 1 )
		{
			lk::vardata_t &acol = cxt.arg(1).deref();
			if ( acol.type() == lk::vardata_t::VECTOR )
				color.Set( acol.index(0)->as_integer(), acol.index(1)->as_integer(), acol.index(2)->as_integer() );
			else
				color = wxColour( acol.as_string() );
		}

		if ( cxt.arg_count() > 2 )
			itran = cxt.arg(2).as_integer();
		
		s_trans = new wxFrame( GetCurrentTopLevelWindow(), wxID_ANY, wxEmptyString,  pos, size, 
			wxBORDER_NONE | wxFRAME_FLOAT_ON_PARENT | wxFRAME_NO_TASKBAR );
		s_trans->SetBackgroundColour( color );
		s_trans->SetTransparent( itran );
		s_trans->Show();
	}
}

static void _wx_scrnres( lk::invoke_t &cxt )
{
	LK_DOC("scrnres", "Obtain screen resolution in pixels.", "(none):array");
	wxSize pix = wxGetDisplaySize();
	cxt.result().empty_vector();
	cxt.result().vec_append( (double)pix.x );
	cxt.result().vec_append( (double)pix.y );
}

static void _wx_html_dialog( lk::invoke_t &cxt )
{
	LK_DOC("html_dialog", "Show a dialog with an HTML viewer.", "(string:html source, [string:title], [array:window size [w,h] or geometry [x,y,w,h]]):none");
	wxString title("HTML Viewer");
	if( cxt.arg_count() > 1 )
		title = cxt.arg(1).as_string();

	wxPoint pos( wxDefaultPosition );
	wxSize size( 600, 500 );
	if( cxt.arg_count() > 2 && cxt.arg(2).deref().type() == lk::vardata_t::VECTOR )
	{
		if ( cxt.arg(2).length() == 2 )
		{
			size.x = (int)cxt.arg(2).index(0)->as_number();
			size.y = (int)cxt.arg(2).index(1)->as_number();
		}
		else if ( cxt.arg(2).length() == 4 )
		{
			pos.x = (int)cxt.arg(2).index(0)->as_number();
			pos.y = (int)cxt.arg(2).index(1)->as_number();
			size.x = (int)cxt.arg(2).index(2)->as_number();
			size.y = (int)cxt.arg(2).index(3)->as_number();
		}
	}

	wxWindow *parent = GetCurrentTopLevelWindow();
	wxFrame *frm = new wxFrame( parent, wxID_ANY, title, pos, size, 
		(wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN | wxRESIZE_BORDER | wxFRAME_TOOL_WINDOW) 
		| ( parent != 0 ? wxFRAME_FLOAT_ON_PARENT : 0 ) );
	wxHtmlWindow *html = new wxHtmlWindow( frm, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxHW_DEFAULT_STYLE|wxBORDER_NONE );
	html->SetPage( cxt.arg(0).as_string() );
	frm->Show();
}

static void _wx_choose_file( lk::invoke_t &cxt )
{

	LK_DOC( "choose_file", "Show a file chooser dialog to select a file name for opening or saving. All of the arguments are options.  The filter should be a string like 'Text Files (*.txt)|*.txt'", "([string:initial path], [string:caption], [string:filter], [boolean:save dialog], [boolean:multiple files]):string");

	wxString caption = "Select a file";
	wxString path = ::wxGetHomeDir();
	wxString filter = "All Files (*.*)";

	bool savedlg = false;
	bool multiple = false;

	if (cxt.arg_count() > 0)
		path = cxt.arg(0).as_string();
	if (cxt.arg_count() > 1)
		caption = cxt.arg(1).as_string();
	if (cxt.arg_count() > 2)
		filter = cxt.arg(2).as_string();
	if (cxt.arg_count() > 3)
		savedlg = cxt.arg(3).as_boolean();
	if (cxt.arg_count() > 4)
		multiple = cxt.arg(4).as_boolean();

	wxString file;
	long style =  savedlg ? wxFD_SAVE|wxFD_OVERWRITE_PROMPT : wxFD_OPEN;
	wxFileDialog fdlg( GetCurrentTopLevelWindow(), caption,
		path, wxEmptyString, filter, style );

	if (fdlg.ShowModal())
	{
		if (multiple)
		{
			wxArrayString files;
			fdlg.GetPaths(files);
			cxt.result().empty_vector();
			for (size_t i=0;i<files.Count();i++)
				cxt.result().vec_append( files[i] );
		}
		else
			cxt.result().assign( fdlg.GetPath() );
	}
}

static void _wx_choose_dir( lk::invoke_t &cxt )
{
	LK_DOC( "choose_dir", "Show a directory chooser dialog to select a folder.", "([string:initial folder], [string:caption]):string" );
	wxString caption("Choose a folder");
	wxString initial;
	if ( cxt.arg_count() > 0 ) initial = cxt.arg(0).as_string();
	if ( cxt.arg_count() > 1 ) caption = cxt.arg(1).as_string();

	cxt.result().assign( wxDirSelector( caption, initial, wxDD_NEW_DIR_BUTTON|wxDD_DEFAULT_STYLE, wxDefaultPosition, GetCurrentTopLevelWindow() ) );
}

static void _wx_date_time( lk::invoke_t &cxt )
{
	LK_DOC( "date_time", "Returns a string containing the current date and time.", "(none):string");
	cxt.result().assign( wxNow() );
}

static wxStopWatch *_stopwatch = NULL;

static void _wx_start_timer( lk::invoke_t &cxt )
{
	LK_DOC( "start_timer", "Starts or restarts a timer.", "(none):none" );
	if ( _stopwatch == 0 ) _stopwatch = new wxStopWatch;

	_stopwatch->Start();
}

static void _wx_elapsed_time( lk::invoke_t &cxt )
{
	LK_DOC( "elapsed_time", "Returns the number of milliseconds elapsed since the last call to start_timer.", "(none):integer");
	cxt.result().assign( _stopwatch != 0 ? (double)_stopwatch->Time() : std::numeric_limits<double>::quiet_NaN() );
}

static void _wx_millisleep( lk::invoke_t &cxt )
{
	LK_DOC( "millisleep", "Puts the application to sleep fro the specified number of milliseconds.", "(integer:milliseconds):none");
	wxMilliSleep( (unsigned long) cxt.arg(0).as_number() );
}

static void _wx_homedir( lk::invoke_t &cxt )
{
	LK_DOC("homedir", "Returns the current user's home directory.", "(none):string");
	cxt.result().assign( ::wxGetHomeDir() );
}

static void _wx_tempfile( lk::invoke_t &cxt )
{
	LK_DOC( "tempfile", "Returns a new temporary file name path.", "([string:extension], [string:prefix]):string");
	wxString tf;
	wxString pfx("pfx"), ext("tmp");
	if ( cxt.arg_count() > 0 ) ext = cxt.arg(0).as_string();
	if ( cxt.arg_count() > 1 ) pfx = cxt.arg(1).as_string();
	cxt.result().assign( wxFileName::CreateTempFileName( pfx ) + "." + ext );
}

static void _wx_uiyield( lk::invoke_t &cxt )
{
	LK_DOC("uiyield", "Yield to the user interface to process any pending events.", "(none):none");
	wxYield();
}

static void _wx_username( lk::invoke_t &cxt )
{
	LK_DOC("username", "Returns the name of the current user.", "(none):string");
	cxt.result().assign( wxGetUserName() );
}

static void _wx_env( lk::invoke_t &cxt )
{
	LK_DOC("env", "Returns the value of an environment variable.", "(string:variable name):string");
	wxString val;
	wxGetEnv( cxt.arg(0).as_string(), &val );
	cxt.result().assign( val );
}

lk::fcall_t* lk::stdlib_wxui()
{
	static const lk::fcall_t vec[] = {
		_wx_msgbox,
		_wx_in,
		_wx_progressbar,
		_wx_choose_from_list,
		_wx_html_dialog,
		_wx_yesno,
		_wx_choose_file,
		_wx_choose_dir,
		_wx_date_time,
		_wx_start_timer,
		_wx_elapsed_time,
		_wx_millisleep,
		_wx_homedir,
		_wx_username,
		_wx_env,
		_wx_uiyield,
		_wx_tempfile,
		_wx_scrnres,
		_wx_transp,
		0 };
	return (fcall_t*) vec;
}

#endif

#ifdef _WIN32
#include <direct.h>
#include <windows.h>

// DIR,dirent for win32 taken May 2011
// Copyright Kevlin Henney, 1997, 2003. All rights reserved.
// http://www.two-sdg.demon.co.uk/curbralan/code/dirent/dirent.h
typedef struct DIR DIR;
struct dirent
{ char *d_name; };
DIR           *opendir(const char *);
int           closedir(DIR *);
struct dirent *readdir(DIR *);
void          rewinddir(DIR *);


#else
#include <unistd.h>
#include <sys/stat.h>
#include <dirent.h>


#endif

#include <sys/types.h>


#ifdef _WIN32
/* taken from wxMSW-2.9.1/include/wx/defs.h - appropriate for Win32/Win64 */
//#define va_copy(d, s) ((d)=(s))
#define snprintf _snprintf
#define strcasecmp _stricmp
//#define strdup _strdup
#endif



class stdfile_t : public lk::objref_t
{
private:
	FILE *m_fp;
	lk_string m_fileName;
public:
	enum {READ, WRITE, APPEND};

	stdfile_t() : m_fp(0) { }
	stdfile_t(FILE *f, const lk_string &n)
		: m_fp(f), m_fileName(n) {  }

	virtual lk_string type_name() { return "file"; }

	virtual ~stdfile_t() {
		if (m_fp)
			fclose(m_fp);
	}

	operator FILE*() {
		return m_fp;
	}

	lk_string name() {
		return m_fileName;
	}

	bool open(const lk_string &name, int mode = READ)
	{
		close();

		char chmode[2];
		chmode[0] = 'r';
		if (mode == WRITE) chmode[0] = 'w';
		if (mode == APPEND) chmode[0] = 'a';
		chmode[1] = 0;

		m_fp = fopen( (const char*) name.c_str(), chmode );
		if (m_fp != 0)
		{
			m_fileName = name;
			return true;
		}
		else
			return false;
	}

	bool close()
	{
		if (m_fp != 0)
		{
			fclose(m_fp);
			m_fp = 0;
			m_fileName = "";
			return true;
		}
		else
			return false;
	}

	bool ok() { return m_fp != 0; }
};

static void _open( lk::invoke_t &cxt )
{
	LK_DOC("open", "Opens a file for 'r'eading, 'w'riting, or 'a'ppending.", "(string:file, string:rwa):integer");

	lk_string name = cxt.arg(0).as_string();
	lk_string mode = lk::lower_case(cxt.arg(1).as_string());

	int m = stdfile_t::READ;
	if (mode == "w" || mode == "write") m = stdfile_t::WRITE;
	if (mode == "a" || mode == "append") m = stdfile_t::APPEND;

	stdfile_t *f = new stdfile_t;
	if (f->open( name, m ))
	{
		cxt.result().assign( (double)(int)cxt.env()->insert_object( f ) );
		return;
	}
	else
	{
		delete f;
		cxt.result().assign( (double)0 );
	}
}

static void _close( lk::invoke_t &cxt )
{
	LK_DOC("close", "Closes an open file.", "(integer:filenum):void");

	stdfile_t *f = dynamic_cast<stdfile_t*>(
			cxt.env()->query_object(
					cxt.arg(0).as_unsigned() ) );

	if (f) cxt.env()->destroy_object( f );
}

static void _seek( lk::invoke_t &cxt )
{
	LK_DOC("seek", "Seeks to a position in an open file.", "(integer:filenum, integer:offset, integer:origin):integer");
	size_t filno = cxt.arg(0).as_unsigned();
	int offset = cxt.arg(1).as_integer();
	int origin = cxt.arg(2).as_integer();

	stdfile_t *f = dynamic_cast<stdfile_t*>( cxt.env()->query_object( filno ));

	if (f && f->ok())
		cxt.result().assign( (int) fseek( *f, offset, origin ) );
	else
		cxt.result().assign( -1 );
}

static void _tell( lk::invoke_t &cxt )
{
	LK_DOC("tell", "Returns the current value of the file position indicator.", "(integer:filenum):integer");
	stdfile_t *f = dynamic_cast<stdfile_t*>(
			cxt.env()->query_object(
					cxt.arg(0).as_unsigned() ) );
	if (f && f->ok())
		cxt.result().assign( (int)::ftell( *f ) );
	else
		cxt.result().assign( -1 );
}

static void _eof( lk::invoke_t &cxt )
{
	LK_DOC("eof", "Determines whether the end of a file has been reached.", "(integer:filenum):boolean");
	stdfile_t *f = dynamic_cast<stdfile_t*>(
			cxt.env()->query_object( cxt.arg(0).as_unsigned()));
	if (f && f->ok())
		cxt.result().assign( feof(*f)!=0 ? 1 : 0 );
	else
		cxt.result().assign( 1 );
}

static void _flush( lk::invoke_t &cxt)
{
	LK_DOC("flush", "Flushes the file output stream to disk.", "(integer:filenum):void");

	stdfile_t *f = dynamic_cast<stdfile_t*>(
			cxt.env()->query_object( cxt.arg(0).as_unsigned()));

	if (f && f->ok())
		fflush( *f );
}


static void _read_line( lk::invoke_t &cxt )
{
	LK_DOC("read_line", "Reads one line of text from a file, returning whether there are more lines to read.", "(integer:filenum, @string:buffer):boolean");

	stdfile_t *f = dynamic_cast<stdfile_t*>(
			cxt.env()->query_object( cxt.arg(0).as_unsigned()));
	if (f && f->ok())
	{
		lk_string buf;
		cxt.result().assign( lk::read_line( *f, buf ) ? 1 : 0 );
		cxt.arg(1).assign( buf );
	}
	else
		cxt.result().assign( 0.0 );
}

static void _read( lk::invoke_t &cxt )
{
	LK_DOC("read", "Reads data from a file, returning false if there is no more data to read.", "(integer:filenum, @string:buffer, integer:bytes):boolean");

	stdfile_t *f = dynamic_cast<stdfile_t*>(
			cxt.env()->query_object( cxt.arg(0).as_unsigned()));

	int nchars = cxt.arg(2).as_integer();

	if (f && f->ok() && nchars >= 0)
	{
		char *buf = new char[nchars+1];

		int i=0;
		char c;
		while (i<nchars && (c=fgetc( *f )) != EOF)
			buf[i++] = c;

		buf[i] = 0;

		cxt.arg(1).assign( buf );
		delete [] buf;

		cxt.result().assign( feof( *f) != 0 ? 1.0 : 0.0 );
	}
	else
		cxt.result().assign(0.0);
}

static void _write_line( lk::invoke_t &cxt )
{
	LK_DOC("write_line", "Writes one line of text to a file.", "(integer:filenum, string:dataline):boolean");

	stdfile_t *f = dynamic_cast<stdfile_t*>(
			cxt.env()->query_object( cxt.arg(0).as_unsigned()));
	if (f && f->ok())
	{
		lk_string buf = cxt.arg(1).as_string();
		fputs( (const char*)buf.c_str(), *f );
		fputs( "\n", *f );
		cxt.result().assign( 1.0 );
	}
	else
		cxt.result().assign( 0.0 );

}

static void _write( lk::invoke_t &cxt )
{
	LK_DOC("write", "Writes data to a file, optionally specifying the number of characters to write.", "(integer:filenum, string:buffer, [integer:nchars]):boolean");

	stdfile_t *f = dynamic_cast<stdfile_t*>(
			cxt.env()->query_object( cxt.arg(0).as_unsigned()));

	if ( f && f->ok() )
	{
		lk_string buf = cxt.arg(1).as_string();
		
		if ( cxt.arg_count() > 2 )
		{
			int nch = cxt.arg(2).as_integer();
			if ( nch > 0 )
				buf.resize( cxt.arg(2).as_integer(), ' ' );
		}

		fputs( (const char*)buf.c_str(), *f );
		cxt.result().assign( 1.0 );
	}
	else
		cxt.result().assign( 0.0 );

}

static void _to_int( lk::invoke_t &cxt )
{
	LK_DOC("to_int", "Converts the argument to an integer value.", "(any):integer");
	cxt.result().assign( (double) ((int)cxt.arg(0).as_number()) );
}

static void _to_real( lk::invoke_t &cxt )
{
	LK_DOC("to_real", "Converts the argument to a real number (double precision).", "(any):real");
	cxt.result().assign( cxt.arg(0).as_number() );
}

static void _to_bool( lk::invoke_t &cxt )
{
	LK_DOC("to_bool", "Converts the argument to a boolean value (1=true, 0=false).", "(any):boolean");
	cxt.result().assign( cxt.arg(0).as_boolean() ? 1.0 : 0.0 );
}

static void _to_string( lk::invoke_t &cxt )
{
	LK_DOC("to_string", "Converts the argument[s] to a text string.", "([any]):string");
	lk_string buf;
	for (size_t i=0;i<cxt.arg_count();i++)	buf += cxt.arg(i).as_string();
	cxt.result().assign(buf);
}

static void _alloc( lk::invoke_t & cxt )
{
	LK_DOC("alloc", "Allocates an array of one or two dimensions.", "(integer, {integer}):array");
	cxt.result().empty_vector();

	int dim1 = (int)cxt.arg(0).as_number();
	int dim2 = (cxt.arg_count() == 2) ? (int)cxt.arg(1).as_number() : -1;

	if (dim1 < 1)
		return;

	cxt.result().resize( dim1 );

	if (dim2 > 0)
	{
		for (int i=0;i<dim1;i++)
		{
			lk::vardata_t *item = cxt.result().index(i);
			item->empty_vector();
			item->resize(dim2);
		}
	}
}

static void _dir_list( lk::invoke_t &cxt )
{
	LK_DOC("dir_list", "List all the files in a directory. The extensions parameter is a comma separated list of extensions, or * to retrieve every item.",
		   "(string:path, string:extensions, [boolean:list dirs also]):array");

	lk_string path = cxt.arg(0).as_string();
	lk_string ext = cxt.arg(1).as_string();
	bool dirs_also =  (cxt.arg_count() > 2)? cxt.arg(2).as_boolean() : false;

	std::vector<lk_string> list = lk::dir_list( path, ext, dirs_also );
	cxt.result().empty_vector();
	for (size_t i=0;i<list.size();i++)
		cxt.result().vec_append( list[i] );
}

static void _file_exists( lk::invoke_t &cxt )
{
	LK_DOC("file_exists", "Determines whether a file exists or not.", "(string):boolean");
	cxt.result().assign( lk::file_exists( (const char*)cxt.arg(0).as_string().c_str() ) ? 1.0 : 0.0 );
}

static void _rename_file( lk::invoke_t &cxt )
{
	LK_DOC("rename_file", "Renames an existing file.", "(string:old name, string:new name):boolean");
	cxt.result().assign( lk::rename_file(cxt.arg(0).as_string(),
		cxt.arg(1).as_string()  ));
}

static void _dir_exists( lk::invoke_t &cxt )
{
	LK_DOC("dir_exists", "Determines whether the specified directory exists.", "(string):boolean");
	cxt.result().assign( lk::dir_exists( (const char*)cxt.arg(0).as_string().c_str() ) ? 1.0 : 0.0 );
}

static void _remove_file( lk::invoke_t &cxt )
{
	LK_DOC("remove_file", "Deletes the specified file from the filesystem.", "(string):boolean");
	cxt.result().assign( lk::remove_file( (const char*)cxt.arg(0).as_string().c_str() ) ? 1.0 : 0.0 );
}

static void _mkdir( lk::invoke_t &cxt )
{
	LK_DOC("mkdir", "Creates the specified directory, optionally creating directories as need for the full path.", "(string:path, {boolean:make_full=true}):boolean");
	lk_string s = cxt.arg(0).as_string();
	bool f = true;
	if (cxt.arg_count() > 0) f = cxt.arg(1).as_boolean();
	cxt.result().assign( lk::mkdir( (const char*)s.c_str(), f ) ? 1.0 : 0.0 );
}

static void _path_only( lk::invoke_t &cxt )
{
	LK_DOC("path_only", "Returns the path portion of a complete file path.", "(string):string");
	cxt.result().assign( lk::path_only( cxt.arg(0).as_string() ) );
}

static void _file_only( lk::invoke_t &cxt )
{
	LK_DOC("file_only", "Returns the file name portion of a complete file path, including extension.", "(string):string");
	cxt.result().assign( lk::name_only( cxt.arg(0).as_string() ) );
}

static void _ext_only( lk::invoke_t &cxt )
{
	LK_DOC("ext_only", "Returns the extension of a file name.", "(string):string");
	cxt.result().assign( lk::ext_only( cxt.arg(0).as_string() ) );
}

static void _cwd( lk::invoke_t &cxt )
{
	LK_DOC2("cwd", "Two modes of operation: set or retrieve the current working directory.",
		   "Returns the current working directory.", "(void):string",
		   "Sets the current working directory.", "(string):boolean" );

	if (cxt.arg_count() == 1)
	{
		lk_string path = cxt.arg(0).as_string();
		if (lk::dir_exists((const char*)path.c_str()))
		{
			lk::set_cwd( path );
			cxt.result().assign( 1.0 );
		}
		else
			cxt.result().assign( 0.0 );
	}
	else
		cxt.result().assign( lk::get_cwd() );
}

static void _system( lk::invoke_t &cxt )
{
	LK_DOC("system", "Executes the system command specified, and returns the exit code.", "(string):integer");
	cxt.result().assign( (double) ::system( (const char*)cxt.arg(0).as_string().c_str() ) );
}

static void _read_text_file( lk::invoke_t &cxt )
{
	LK_DOC("read_text_file", "Reads the entire contents of the specified text file and returns the result.", "(string:file):string");
	cxt.result().assign( lk::read_file( cxt.arg(0).as_string() ));
}

static void _write_text_file( lk::invoke_t &cxt )
{
	LK_DOC("write_text_file", "Writes data to a text file.", "(string:file, string:data):boolean");

	lk_string file = cxt.arg(0).as_string();
	lk_string data = cxt.arg(1).as_string();

	FILE *fp = fopen( (const char*) file.c_str(), "w");
	if (!fp)
	{
		cxt.result().assign( 0.0 );
		return;
	}

	fputs( (const char*)data.c_str(), fp );
	fclose(fp);
	cxt.result().assign( 1.0 );
}

static void _load_extension( lk::invoke_t &cxt )
{
	LK_DOC("load_extension", "Loads an LK extension library.", "(string:path):boolean");
	cxt.result().assign( cxt.env()->global()->load_library( cxt.arg(0).as_string() ) ? 1.0 : 0.0 );
}

static void _extensions( lk::invoke_t &cxt )
{
	LK_DOC("extensions", "Returns information about currently loaded extensions", "(none):table");
	cxt.result().empty_hash();
	std::vector< lk::env_t::dynlib_t* > libs = cxt.env()->libraries();
	for ( std::vector< lk::env_t::dynlib_t* >::const_iterator it = libs.begin();
		it != libs.end();
		++it )
	{
		lk::vardata_t &item = cxt.result().hash_item( (*it)->path );
		item.empty_hash();

		lk_invokable *funcs = (*it)->functions;
		size_t idx = 0;
		while ( funcs[idx] != 0 )
		{
			lk::doc_t d;
			if ( lk::doc_t::info(funcs[idx], d) && !d.func_name.empty())
			{
				lk::vardata_t &record = item.hash_item( d.func_name );
				record.empty_hash();
				record.hash_item( "description", d.desc1 );
				record.hash_item( "signature", d.sig1 );
				if ( d.has_2 )
				{
					record.hash_item( "description2", d.desc2 );
					record.hash_item( "signature2", d.sig2 );
				}
				if ( d.has_3 )
				{
					record.hash_item( "description3", d.desc3 );
					record.hash_item( "signature3", d.sig3 );
				}
			}
			idx++;
		}
	}
}

static void _ostype( lk::invoke_t &cxt )
{
	LK_DOC("ostype", "Returns identifying information about the operating system type. ('osx', 'win32', 'linux', etc).", "(none):string");

	lk_string os;
#if defined(_MSC_VER) || defined(WIN32) || defined(__MINGW32__)
	
#ifdef _WIN64
	os = "win64";
#else
	os = "win32";
#endif

#elif defined(__APPLE__)||defined(__APPLE_CC__)

#ifdef __LP64__
	os = "osx64";
#else
	os = "osx32";
#endif

#elif defined(__linux__)||defined(__linux)

#ifdef __LP64__
	os = "linux64";
#else
	os = "linux32";
#endif

#else
	os = "unknown";
#endif


	cxt.result().assign(os);
}

class vardata_compare
{
public:
    size_t sort_column;

    vardata_compare()
    {
        sort_column = 0;     //initialize
    };

	bool operator() ( const lk::vardata_t &lhs, const lk::vardata_t &rhs )
	{
		if (lhs.type() == lk::vardata_t::NUMBER && rhs.type() == lk::vardata_t::NUMBER)
			return lhs.num() < rhs.num();
		else if (lhs.type() == lk::vardata_t::VECTOR && rhs.type() == lk::vardata_t::VECTOR)
		{
			if(lhs.vec()->size() > sort_column && rhs.vec()->size() > sort_column)  //both vectors must have sufficient entries, otherwise use default string comparison below
			{
				if(lhs.vec()->at(sort_column).type() == lk::vardata_t::NUMBER && rhs.vec()->at(sort_column).type() == lk::vardata_t::NUMBER)
					return lhs.vec()->at(sort_column).num() < rhs.vec()->at(sort_column).num();  //numeric comparison if both numeric
				else
					return lhs.vec()->at(sort_column).as_string() < rhs.vec()->at(sort_column).as_string();     //otherwise string comparison
			}
		}
				
        //default is string comparison
        return lhs.as_string() < rhs.as_string();
	}
};
static void _stable_sort( lk::invoke_t &cxt )
{
	LK_DOC("stable_sort", "Sort an array of numbers or strings in place while preserving relative ordering of elements.", "(array, [integer:sort column index]):none");
	
	lk::vardata_t &x = cxt.arg(0);
	if (x.type() == lk::vardata_t::VECTOR )
	{
		vardata_compare cc;
        if( cxt.arg_count() > 1 )
            cc.sort_column = cxt.arg(1).as_integer();

		std::stable_sort( x.vec()->begin(), x.vec()->end(), cc );
	}
}




class lkJSONwriterBase {
	int level;
public:
	lkJSONwriterBase()
		: level(0)
	{
	}
	
	virtual void out( const lk_string &s ) = 0;

	void indent()
	{
		for( int i=0;i<level;i++ )
			out( "  " );
	}

	bool write( const lk::vardata_t &x )
	{
		switch( x.type() )
		{
		case lk::vardata_t::REFERENCE:
			write( x.deref() );
			break;

		case lk::vardata_t::NUMBER:
		{
			out( lk::format( "%lg", x.num() ) );
			break;
		}

		case lk::vardata_t::STRING:
			out( "\"" + x.str() + "\"" );
			break;

		case lk::vardata_t::VECTOR:
			out( "[ " );
			level++;
			for( int i=0;i<(int)x.length();i++ )
			{
				write( *x.index(i) );
				if ( i < (int)x.length()-1 )
					out( ", " );
			}
			level--;
			out( " ]" );
			break;

		case lk::vardata_t::HASH:
		{
			indent();
			out( "{\n" );
			level++;

			size_t i=0, n=x.hash()->size();
			for( lk::varhash_t::const_iterator it = x.hash()->begin();
				it != x.hash()->end();
				++it )
			{
				
				indent();
				out( "\"" + it->first + "\" : " );
				write( *it->second );
				if ( i++ < n-1 )
					out( ",\n" );
				else
					out( "\n" );
			}

			level--;
			indent();
			out( "}" );
		}
			break;

		case lk::vardata_t::FUNCTION:
		case lk::vardata_t::EXTFUNC:
		case lk::vardata_t::INTFUNC:
			out( "\"<function>\"" );
			break;
		case lk::vardata_t::NULLVAL:
		default:
			out( "null" );
		}

		return true;
	}
};

class lkJSONwriterString : public lkJSONwriterBase
{
	lk_string json;
public:
	lkJSONwriterString( ) : lkJSONwriterBase( )	{  }
	virtual void out( const lk_string &s ) { json += s; }
	lk_string &jsonstr() { return json; }
};

class lkJSONwriterFile : public lkJSONwriterBase
{
	FILE *fp;
public:
	lkJSONwriterFile( FILE *f )	: lkJSONwriterBase( ), fp(f)	{  }
	virtual void out( const lk_string &s ) { fputs( s.c_str(), fp ); }
};


#include <lk/lex.h>

class lkJSONreader
{
	lk::input_string is;
	lk::lexer lex;
	lk_string error;
	int tok;
public:
	lkJSONreader( const lk_string &json )
		: is(json), lex(is)
	{
		tok = lex.next();
	}
	
	void skip() {
		tok = lex.next();
	}

	lk_string err() { return error; }

	bool match( int t ) {
		if ( t == tok )
		{
			tok = lex.next();
			return true;
		}
		else
		{
			if ( error.size() == 0 )
				error = "expected '" + lk_string(lex.tokstr(t)) + "' but found '" + lex.text() + "' (" + lk_string(lex.tokstr(tok)) + ")";
			return false;
		}
	}

	bool parse_value( lk::vardata_t &x ) {
		if ( tok == lk::lexer::NUMBER )
		{
			x.assign( lex.value() );
			skip();
			return true;
		}
		else if ( tok == lk::lexer::OP_MINUS || tok == lk::lexer::OP_PLUS )
		{
			double pm = (tok==lk::lexer::OP_MINUS) ? -1.0 : 1.0;
			skip();
			if ( tok != lk::lexer::NUMBER )
			{
				if ( error.size() == 0 )
					error = "expected number after + or - prefix, but found: '" + lex.text() + "' (" + lk_string(lex.tokstr(tok)) + ")";

				return false;
			}

			x.assign( lex.value() * pm );
			skip();
			return true;
		}
		else if ( tok == lk::lexer::LITERAL )
		{
			x.assign( lex.text() );
			skip();
			return true;
		}
		else if ( tok == lk::lexer::IDENTIFIER
			&& lk::lower_case(lex.text()) == "null" )
		{
			x.nullify();
			skip();
			return true;
		}
		else if ( tok == lk::lexer::IDENTIFIER )
		{
			x.assign( lex.text() );
			skip();
			return true;
		}
		else
		{
			if (error.size() == 0 )
				error = "expected number, literal, or null, but found: '" + lex.text() + "' (" + lk_string(lex.tokstr(tok)) + ")";
			return false;
		}
	}
	bool parse_array( lk::vardata_t &x ) {
		match( lk::lexer::SEP_LBRACK );

		x.empty_vector();
		while( tok != lk::lexer::INVALID
			&& tok != lk::lexer::END
			&& tok != lk::lexer::SEP_RBRACK )
		{
			lk::vardata_t item;
			if ( !parse(item) )
				return false;

			x.vec()->push_back( item );

			if ( tok == lk::lexer::SEP_COMMA )
				skip();
			else
				break;
		}

		return match( lk::lexer::SEP_RBRACK );
	}
	bool parse_table( lk::vardata_t &x ) {
		match( lk::lexer::SEP_LCURLY );

		x.empty_hash();
		while( tok != lk::lexer::INVALID
			&& tok != lk::lexer::END
			&& tok != lk::lexer::SEP_RCURLY )
		{

			if ( tok != lk::lexer::LITERAL )
				return false;

			lk_string key( lex.text() );
			skip();

			match( lk::lexer::SEP_COLON );

			if ( !parse( x.hash_item(key) ) )
				return false;
			
			if ( tok == lk::lexer::SEP_COMMA )
				skip();
			else
				break;
		}
		return match( lk::lexer::SEP_RCURLY );
	}

	bool parse( lk::vardata_t &x )
	{
		if ( tok == lk::lexer::SEP_LBRACK )
			return parse_array( x );
		else if ( tok == lk::lexer::SEP_LCURLY )
			return parse_table( x );
		else
			return parse_value( x );
	}
};

lk_string lk::json_write( const lk::vardata_t &x )
{
	lkJSONwriterString wr;
	wr.write( x );
	return wr.jsonstr();

}


static void _json_file( lk::invoke_t &cxt )
{
	LK_DOC( "json_file", "Read or write data stored in a JSON file.", "(string:file name, variant):boolean or (string:file name):variant" );

	lk_string file( cxt.arg(0).as_string() );
	if ( cxt.arg_count() == 1 )
	{
		lk_string err;
		if ( !lk::json_read( lk::read_file( file ), cxt.result(), &err ) )
			cxt.result().assign( "<json-error> " + err );
	}
	else
	{
		if ( FILE *fp = fopen( file.c_str(), "w" ) )
		{
			lkJSONwriterFile wr( fp );
			cxt.result().assign( wr.write( cxt.arg(1) ) ? 1.0 : 0.0 );
			fclose( fp );
		}
		else
			cxt.result().assign( 0.0 );
	}
}



static void _json_write( lk::invoke_t &cxt )
{
	LK_DOC( "json_write", "Convert a variable to a JSON string representation.", "(variant):string" );
	cxt.result().assign( lk::json_write( cxt.arg(0) ) );
}

static void _json_read( lk::invoke_t &cxt )
{
	LK_DOC( "json_read", "Convert a JSON string to a variable.", "(string):variant" );
	
	lk_string err;
	if ( !lk::json_read( cxt.arg(0).as_string(), cxt.result(), &err ) )
		cxt.result().assign( "<json-error> " + err );
}

static void _sprintf( lk::invoke_t &cxt )
{
	LK_DOC("sprintf", "Returns a formatted string using standard C printf conventions, but adding the %m and %, specifiers for monetary and comma separated real numbers.", "(string:format, ...):string");
	lk_string fmt = cxt.arg(0).as_string();
	std::vector< lk::vardata_t * > args;
	for (size_t i=1;i<cxt.arg_count();i++)
		args.push_back( & cxt.arg(i) );

	cxt.result().assign( lk::format_vl( fmt, args ) );
}

static void _strpos( lk::invoke_t &cxt )
{
	LK_DOC("strpos", "Locates the first instance of a character or substring in a string.", "(string, string):integer");
	lk_string s = cxt.arg(0).as_string();
	lk_string f = cxt.arg(1).as_string();
	lk_string::size_type idx = s.find(f);
	cxt.result().assign( (idx!=lk_string::npos) ? (int)idx : -1 );
}

static void _first_of( lk::invoke_t &cxt )
{
	LK_DOC("first_of", "Searches the string s1 for any of the characters in s2, and returns the position of the first occurence", "(string:s1, string:s2):integer");
	cxt.result().assign( (double) cxt.arg(0).as_string().find_first_of( cxt.arg(1).as_string() ));
}

static void _last_of( lk::invoke_t &cxt )
{
	LK_DOC("last_of", "Searches the string s1 for any of the characters in s2, and returns the position of the last occurence", "(string:s1, string:s2):integer");
	cxt.result().assign( (double) cxt.arg(0).as_string().find_last_of( cxt.arg(1).as_string() ));
}

static void _left( lk::invoke_t & cxt )
{
	LK_DOC("left", "Returns the leftmost n characters of a string.", "(string, integer:n):string");
	size_t n = (size_t)cxt.arg(1).as_number();
	lk_string s = cxt.arg(0).as_string(), buf;
	if (n >= s.length())
	{
		cxt.result().assign(s);
		return;
	}
	buf.reserve(n+1);
	for (size_t i=0;i<n && i<s.length();i++)
		buf += s[i];
	cxt.result().assign(buf);
}

static void _right( lk::invoke_t &cxt )
{
	LK_DOC("right", "Returns the rightmost n characters of a string.", "(string, integer:n):string");
	size_t n = (size_t)cxt.arg(1).as_number();
	lk_string s = cxt.arg(0).as_string(), buf;
	if (n >= s.length())
	{
		cxt.result().assign(s);
		return;
	}
	buf.reserve(n+1);
	size_t start = s.length()-n;
	for (size_t i=0;i<n && start+i<s.length();i++)
		buf += s[start+i];
	cxt.result().assign(buf);
}

static void _mid( lk::invoke_t &cxt )
{
	LK_DOC("mid", "Returns a subsection of a string.", "(string, integer:start, {integer:length}):string");
	lk_string s = cxt.arg(0).as_string();
	size_t start = (size_t)cxt.arg(1).as_number();
	if (start  > s.length()) start = s.length();
	size_t len = cxt.arg_count() > 2 ? (size_t)cxt.arg(2).as_number() : 0;
	cxt.result().assign( s.substr( start, (len==0) ? lk_string::npos : len ) );
}

static void _strlen( lk::invoke_t &cxt )
{
	LK_DOC("strlen", "Returns the length of a string.", "(string):integer");
	cxt.result().assign( (double)(int) cxt.arg(0).as_string().length() );
}

static void _ascii( lk::invoke_t &cxt )
{
	LK_DOC("ascii", "Returns the ascii code of a character.", "(character):integer");
	lk_string s = cxt.arg(0).as_string();
	int ch = 0;
	if (s.length() > 0)
		ch = (int) s[0];
	cxt.result().assign( (double)ch );
}

static void _isalpha( lk::invoke_t &cxt )
{
	LK_DOC("isalpha", "Returns true if the argument is an alphabetic character A-Z,a-z.", "(character):boolean");
	lk_string s = cxt.arg(0).as_string();
	cxt.result().assign( ::isalpha(s.length() > 0 ? (int)s[0] : 0) ? 1.0 : 0.0 );
}

static void _isdigit( lk::invoke_t &cxt )
{
	LK_DOC("isdigit", "Returns true if the argument is a numeric digit 0-9.", "(character):boolean");
	lk_string s = cxt.arg(0).as_string();
	cxt.result().assign( ::isdigit(s.length() > 0 ? (int)s[0] : 0) ? 1.0 : 0.0 );
}

static void _isalnum( lk::invoke_t &cxt )
{
	LK_DOC("isalnum", "Returns true if the argument is an alphanumeric A-Z,a-z,0-9.", "(character):boolean");
	lk_string s = cxt.arg(0).as_string();
	cxt.result().assign( ::isalnum(s.length() > 0 ? (int)s[0] : 0) ? 1.0 : 0.0 );
}

static void _char( lk::invoke_t &cxt )
{
	LK_DOC("char", "Returns a string (one character long) from an ascii code.", "(integer):string");
	lk_char ascii = (lk_char) (int) cxt.arg(0).as_number();
	lk_string b("0");
	b[0] = (lk_char)ascii;
	cxt.result().assign( b );
}

static void _ch( lk::invoke_t &cxt )
{
	LK_DOC2("ch", "Two modes of operation: for retrieving and setting individual characters in a string.",
			"Sets the character at the specified index in a string.", "(string, integer, character):void",
			"Gets the character at the specified index in a string.", "(string, integer):character" );

	lk_string s = cxt.arg(0).as_string();
	size_t pos = (size_t) cxt.arg(1).as_number();
	if (cxt.arg_count() == 3)
	{
		lk_string v = cxt.arg(2).as_string();
		if (pos < s.length() && v.length() > 0)
			s[pos] = v[0];

		cxt.arg(0).assign( s );
	}
	else if (cxt.arg_count() == 2)
	{
		if (pos < s.length())
		{
			char b[2];
			b[0] = s[pos];
			b[1] = 0;
			cxt.result().assign( lk_string( b ) );
		}
		else
			cxt.result().assign( lk_string("") );
	}
	else
		cxt.error("invalid number of arguments to 'ch' function");
}

static void _upper( lk::invoke_t &cxt )
{
	LK_DOC("upper", "Returns an upper case version of the supplied string.", "(string):string");
	lk_string s = cxt.arg(0).as_string();
	lk_string ret(s);
	for (lk_string::size_type i=0;i<ret.length();i++)
		ret[i] = (lk_char) toupper( ret[i] );
	cxt.result().assign(ret);
}

static void _lower( lk::invoke_t &cxt )
{
	LK_DOC("lower", "Returns a lower case version of the supplied string.", "(string):string");
	lk_string s = cxt.arg(0).as_string();
	lk_string ret(s);
	for (lk_string::size_type i=0;i<ret.length();i++)
		ret[i] = (lk_char) tolower( ret[i] );
	cxt.result().assign(ret);
}

static void _strcmp( lk::invoke_t &cxt )
{
	LK_DOC("strcmp", "Case sensitive string comparison.  Returns 0 if strings are identical, negative number if s1 comes before s2, and positive number if s2 comes before s1.", "(string:s1, string:s2):integer" );
#ifdef LK_USE_WXWIDGETS
	int result = cxt.arg(0).as_string().Cmp( cxt.arg(1).as_string() );
#else
	int result = strcmp( cxt.arg(0).as_string().c_str(), cxt.arg(1).as_string().c_str() );
#endif
	cxt.result().assign( (double)result );
}

static void _stricmp( lk::invoke_t &cxt )
{
	LK_DOC("stricmp", "Case insensitive string comparison.  Returns 0 if strings are identical, negative number if s1 comes before s2, and positive number if s2 comes before s1.", "(string:s1, string:s2):integer" );
#ifdef LK_USE_WXWIDGETS
	int result = cxt.arg(0).as_string().CmpNoCase( cxt.arg(1).as_string() );
#else
#ifdef _MSC_VER
	int result = stricmp( cxt.arg(0).as_string().c_str(), cxt.arg(1).as_string().c_str() );
#else
	int result = strcasecmp( cxt.arg(0).as_string().c_str(), cxt.arg(1).as_string().c_str() );
#endif
#endif
	cxt.result().assign( (double)result );
}


static void _replace( lk::invoke_t &cxt )
{
	LK_DOC("replace", "Replaces all instances of s1 with s2 in the supplied string.", "(string, string:s1, string:s2):string");
	lk_string s = cxt.arg(0).as_string();
	lk_string s_old = cxt.arg(1).as_string();
	lk_string s_new = cxt.arg(2).as_string();

	lk::replace( s, s_old, s_new );

	cxt.result().assign( s );
}

static void _split( lk::invoke_t &cxt )
{
	LK_DOC("split", "Splits a string into parts at the specified delimiter characters.", "(string:s, string:delims, {boolean:ret_empty=false}, {boolean:ret_delim=false}):array");
	bool ret_empty = false;
	bool ret_delim = false;
	lk_string str = cxt.arg(0).as_string();
	lk_string delim = cxt.arg(1).as_string();
	if (cxt.arg_count() >= 3) ret_empty = cxt.arg(2).as_boolean();
	if (cxt.arg_count() >= 4) ret_delim = cxt.arg(3).as_boolean();

	std::vector<lk_string> list = lk::split( str, delim, ret_empty, ret_delim );
	cxt.result().empty_vector();
	cxt.result().resize( list.size() );
	for (size_t i=0;i<list.size();i++)
		cxt.result().index(i)->assign( list[i] );
}

static void _join( lk::invoke_t &cxt )
{
	LK_DOC("join", "Joins an array of strings into a single one using the given delimiter.", "(array, string:delim):string");
	lk_string buf;
	lk_string delim = cxt.arg(1).as_string();
	lk::vardata_t &a = cxt.arg(0);
	for (size_t i=0;i<a.length();i++)
	{
		buf += a.index(i)->as_string();
		if (i<a.length()-1)
			buf += delim;
	}
	cxt.result().assign(buf);
}

static void _real_array( lk::invoke_t &cxt )
{
	LK_DOC("real_array", "Splits a whitespace delimited string into an array of real numbers.", "(string):array");

	std::vector<lk_string> list = lk::split( cxt.arg(0).as_string(), " \t\n\r,;:", false, false );
	cxt.result().empty_vector();
	for (size_t i=0;i<list.size();i++)
		cxt.result().vec_append( atof( (const char*)list[i].c_str() ) );
}


static void _ff_sum( lk::vardata_t &x, double mean, double *sum, double *sumsqr, int *nvalues )
{
	switch (x.type())
	{
	case lk::vardata_t::VECTOR:
		{
			for (size_t i=0;i<x.length();i++)
				_ff_sum( *(x.index(i)), mean, sum, sumsqr, nvalues );
		}
		break;
	case lk::vardata_t::NUMBER:
		{
			double val = x.as_number();
			(*nvalues)++;
			(*sum) += val;
			(*sumsqr) += (val-mean)*(val-mean);
		}
		break;
	}
}

static void _msum( lk::invoke_t &cxt )
{
	LK_DOC("sum", "Returns the numeric sum of all values passed to the function. Arguments can be arrays or numbers.", "(...):real");
	
	double sum = 0, sumsqr = 0;
	int nvalues = 0;
	for (size_t i=0;i<cxt.arg_count();i++)
		_ff_sum( cxt.arg(i), 0, &sum, &sumsqr, &nvalues );

	cxt.result().assign( sum );
}

static void _mmean( lk::invoke_t &cxt )
{
	LK_DOC("mean", "Returns the mean (average) value all values passed to the function. Arguments can be arrays or numbers.", "(...):real");
	
	double sum = 0, sumsqr = 0;
	int nvalues = 0;
	for (size_t i=0;i<cxt.arg_count();i++)
		_ff_sum( cxt.arg(i), 0, &sum, &sumsqr, &nvalues );

	cxt.result().assign( sum / ((double)nvalues) );
}

static void _mmedian(lk::invoke_t &cxt)
{
	LK_DOC2("median", "Returns the median numeric value.",
		"Returns the median of the numeric arguments.", "(...):real",
		"Returns the median value in an array of numbers", "(array):real");

	//get the values into an array
	std::vector<double> values;
	if (cxt.arg_count() >=2)
		for (size_t i = 0; i < cxt.arg_count(); i++)
			values.push_back(cxt.arg(i).as_number());
	else if (cxt.arg_count() == 1 && cxt.arg(0).type() == lk::vardata_t::VECTOR	&& cxt.arg(0).length() > 0)
	{
		lk::vardata_t &arr = cxt.arg(0);
		for (size_t i = 0; i < arr.length(); i++)
			values.push_back(arr.index(i)->as_number());
	}
	else
	{
		cxt.error("invalid arguments to the min() function");
		return;
	}

	//sort the array
	for (int i = 0; i < values.size(); i++)
	{
		//start at the current spot in the output
		int smallest = i;
		for (int j = i + 1; j < values.size(); j++) //find the index of the smallest number in the remaining range
			if (values[j] < values[smallest])
				smallest = j;
		//swap the spots of the current i and the smallest number
		if (smallest != i)
		{
			double value_temp = values[i];
			values[i] = values[smallest];
			values[smallest] = value_temp;
		}
	}
	
	//determine if the array is an odd or even number of values
	bool odd = false;
	if (values.size() % 2 != 0) odd = true;

	//if the array has an odd number of values, the median is the middle value
	double median = std::numeric_limits<double>::quiet_NaN();

	if (odd)
	{
		int index = floor(values.size() / 2);
		median = values[index];
	}
	else
	{
		int high_index = values.size() / 2;
		median = (values[high_index] + values[high_index - 1]) / 2;
	}

	cxt.result().assign(median);
}

static void _mstddev( lk::invoke_t &cxt )
{
	LK_DOC("stddev", "Returns the sample standard deviation of all values passed to the function. Uses Bessel's correction (N-1). Arguments can be arrays or numbers.", "(...):real");
	
	// two pass implementation to avoid round off.
	// could be improved according to: http://www.cs.berkeley.edu/~mhoemmen/cs194/Tutorials/variance.pdf
	double sum = 0, sumsqr = 0;
	int nvalues = 0;
	for (size_t i=0;i<cxt.arg_count();i++)
		_ff_sum( cxt.arg(i), 0, &sum, &sumsqr, &nvalues );

	double mean = sum / ((double)nvalues);
	nvalues = 0;
	sum = sumsqr = 0;
	for (size_t i=0;i<cxt.arg_count();i++)
		_ff_sum( cxt.arg(i), mean, &sum, &sumsqr, &nvalues );

	cxt.result().assign( sqrt(sumsqr/(nvalues-1)) );
}

static void _mmin( lk::invoke_t &cxt )
{
	LK_DOC2("min", "Returns the minimum numeric value.", 
		"Returns the minimum of the numeric arguments.", "(...):real",
		"Returns the minimum value in an array of numbers", "(array):real");
	if (cxt.arg_count() >= 2)
	{
		double m = cxt.arg(0).as_number();
		for (size_t i=1;i<cxt.arg_count();i++)
			if (cxt.arg(i).as_number() < m)
				m = cxt.arg(i).as_number();
		cxt.result().assign( m );
	}
	else if (cxt.arg_count() == 1 
		&& cxt.arg(0).type() == lk::vardata_t::VECTOR
		&& cxt.arg(0).length() > 0)
	{
		lk::vardata_t &arr = cxt.arg(0);
		double m = arr.index(0)->as_number();
		for (size_t i=1;i<arr.length();i++)
		{
			double t = arr.index(i)->as_number();
			if (t < m) m = t;
		}
		cxt.result().assign( m );
	}
	else
		cxt.error("invalid arguments to the min() function");
}

static void _mmax( lk::invoke_t &cxt )
{
	LK_DOC2("max", "Returns the maximum numeric value.", 
		"Returns the maximum of the passed numeric arguments.", "(...):real",
		"Returns the maximum value in an array of numbers", "(array):real");
	if (cxt.arg_count() >= 2)
	{
		double m = cxt.arg(0).as_number();
		for (size_t i=1;i<cxt.arg_count();i++)
			if (cxt.arg(i).as_number() > m)
				m = cxt.arg(i).as_number();
		cxt.result().assign( m );
	}
	else if (cxt.arg_count() == 1 
		&& cxt.arg(0).type() == lk::vardata_t::VECTOR
		&& cxt.arg(0).length() > 0)
	{
		lk::vardata_t &arr = cxt.arg(0);
		double m = arr.index(0)->as_number();
		for (size_t i=1;i<arr.length();i++)
		{
			double t = arr.index(i)->as_number();
			if (t > m) m = t;
		}
		cxt.result().assign( m );
	}
	else
		cxt.error("invalid arguments to the max() function");
}

static void _mceil( lk::invoke_t &cxt )
{
	LK_DOC("ceil", "Round to the smallest integral value not less than x.", "(real:x):real");
	cxt.result().assign( ::ceil( cxt.arg(0).as_number() ));
}
static void _mround(lk::invoke_t &cxt)
{
	LK_DOC("round", "Round to the nearest integral value.", "(real:x):real");
	cxt.result().assign(::round(cxt.arg(0).as_number()));
}
static void _mfloor( lk::invoke_t &cxt )
{
	LK_DOC("floor", "Round to the largest integral value not greater than x.", "(real:x):real");
	cxt.result().assign( ::floor(cxt.arg(0).as_number() ));
}

static void _msqrt( lk::invoke_t &cxt )
{
	LK_DOC("sqrt", "Returns the square root of a number.", "(real:x):real");
	cxt.result().assign( ::sqrt(cxt.arg(0).as_number() ));
}

static void _mpow( lk::invoke_t &cxt )
{
	LK_DOC("pow", "Returns a number x raised to the power y.", "(real:x, real:y):real");
	cxt.result().assign( ::pow( cxt.arg(0).as_number(), cxt.arg(1).as_number() ) );
}

static void _mexp( lk::invoke_t &cxt )
{
	LK_DOC("exp", "Returns the base-e exponential of x.", "(real:x):real");
	cxt.result().assign( ::exp( cxt.arg(0).as_number() ));
}

static void _mlog( lk::invoke_t &cxt )
{
	LK_DOC("log", "Returns the base-e logarithm of x.", "(real:x):real");
	cxt.result().assign( ::log( cxt.arg(0).as_number() ));
}

static void _mlog10( lk::invoke_t &cxt )
{
	LK_DOC("log10", "Returns the base-10 logarithm of x.", "(real:x):real");
	cxt.result().assign( ::log10( cxt.arg(0).as_number() ));
}

static void _mpi( lk::invoke_t &cxt )
{
	LK_DOC("pi", "Returns the value of PI.", "(void):real");
	cxt.result().assign( 3.14156295358979323846264338327950 );
}

static void _mabs( lk::invoke_t &cxt )
{
	LK_DOC("abs", "Returns the absolute value of a number.", "(real:x):real");
	cxt.result().assign( ::fabs( cxt.arg(0).as_number() ) );
}

static void _msgn( lk::invoke_t &cxt )
{
	LK_DOC("sgn", "Returns 1 if the argument is greater than zero, 0 if argument is 0, otherwise -1.", "(real:x):real");
	double x = cxt.arg(0).as_number();
	cxt.result().assign( x > 0 ? 1.0 : ( x == 0 ? 0.0 : -1.0 ) );
}

static void _msin( lk::invoke_t &cxt )
{
	LK_DOC("sin", "Computes the sine of x (radians)", "(real:x):real");
	cxt.result().assign( ::sin( cxt.arg(0).as_number() ));
}

static void _msind( lk::invoke_t &cxt )
{
	LK_DOC("sind", "Computes the sine of x (degrees)", "(real:x):real");
	cxt.result().assign( ::sin( cxt.arg(0).as_number() * M_PI / 180 ) );
}

static void _mcos( lk::invoke_t &cxt )
{
	LK_DOC("cos", "Computes the cosine of x (radians)", "(real:x):real");
	cxt.result().assign( ::cos( cxt.arg(0).as_number() ));
}

static void _mcosd( lk::invoke_t &cxt )
{
	LK_DOC("cosd", "Computes the cosine of x (degrees)", "(real:x):real");
	cxt.result().assign( ::cos( cxt.arg(0).as_number() * M_PI / 180 ));
}

static void _mtan( lk::invoke_t &cxt )
{
	LK_DOC("tan", "Computes the tangent of x (radians)", "(real:x):real");
	cxt.result().assign( ::tan( cxt.arg(0).as_number() ));
}

static void _mtand( lk::invoke_t &cxt )
{
	LK_DOC("tand", "Computes the tangent of x (degrees)", "(real:x):real");
	cxt.result().assign( ::tan( cxt.arg(0).as_number() * M_PI / 180 ));
}

static void _masin( lk::invoke_t &cxt )
{
	LK_DOC("asin", "Computes the arc sine of x, result is in radians, -pi/2 to pi/2.", "(real:x):real");
	cxt.result().assign( ::asin( cxt.arg(0).as_number() ));
}

static void _masind( lk::invoke_t &cxt )
{
	LK_DOC("asind", "Computes the arc sine of x, result is in degrees, -90 to 90.", "(real:x):real");
	cxt.result().assign( ::asin( cxt.arg(0).as_number() ) * 180 / M_PI );
}


static void _macos( lk::invoke_t &cxt )
{
	LK_DOC("acos", "Computes the arc cosine of x, result is in radians, 0 to pi.", "(real:x):real");
	cxt.result().assign( ::acos( cxt.arg(0).as_number() ));
}

static void _macosd( lk::invoke_t &cxt )
{
	LK_DOC("acosd", "Computes the arc cosine of x, result is in degrees, 0 to 180.", "(real:x):real");
	cxt.result().assign( ::acos( cxt.arg(0).as_number() ) * 180 / M_PI );
}


static void _matan( lk::invoke_t &cxt )
{
	LK_DOC("atan", "Computes the arc tangent of x, result is in radians, -pi/2 to pi/2.", "(real:x):real");
	cxt.result().assign( ::atan( cxt.arg(0).as_number() ));
}

static void _matand( lk::invoke_t &cxt )
{
	LK_DOC("atand", "Computes the arc tangent of x, result is in degrees, -90 to 90.", "(real:x):real");
	cxt.result().assign( ::atan( cxt.arg(0).as_number() ) * 180 / M_PI );
}

static void _matan2( lk::invoke_t &cxt )
{
	LK_DOC("atan2", "Computes the arc tangent using both x and y to determine the quadrant of the result, result is in radians.", "(real:x, real:y):real");
	cxt.result().assign( ::atan2( cxt.arg(0).as_number(), cxt.arg(1).as_number() ));
}

static void _matan2d( lk::invoke_t &cxt )
{
	LK_DOC("atan2d", "Computes the arc tangent using both x and y to determine the quadrant of the result, result is in degrees.", "(real:x, real:y):real");
	cxt.result().assign( ::atan2( cxt.arg(0).as_number(), cxt.arg(1).as_number() ) * 180 / M_PI);
}

static void _mnan( lk::invoke_t &cxt )
{
	LK_DOC("nan", "Returns the non-a-number (NAN) value.", "(void):real");
	cxt.result().assign( std::numeric_limits<double>::quiet_NaN() );
}

static void _misnan( lk::invoke_t &cxt )
{
	LK_DOC("isnan", "Returns true if the argument is NaN.", "(number):boolean");
	cxt.result().assign( std::isnan( cxt.arg(0).as_number() ) );
}

static void _mmod( lk::invoke_t &cxt )
{
	LK_DOC("mod", "Returns the remainder after integer division of x by y.", "(integer:x, integer:y):integer");
	cxt.result().assign( (double) (((int)cxt.arg(0).as_number()) % ((int)cxt.arg(1).as_number())));
}

static void _mbesj0( lk::invoke_t &cxt )
{
	LK_DOC("besj0", "Computes the value of the Bessel function of the first kind, order 0, J0(x)", "(real:x):real");
	cxt.result().assign( lk::besj0( cxt.arg(0).as_number() ));
}

static void _mbesj1( lk::invoke_t &cxt )
{
	LK_DOC("besj1", "Computes the value of the Bessel function of the first kind, order 1, J1(x)", "(real:x):real");
	cxt.result().assign( lk::besj1( cxt.arg(0).as_number() ));
}

static void _mbesy0( lk::invoke_t &cxt )
{
	LK_DOC("besy0", "Computes the value of the Bessel function of the second kind, order 0, Y0(x)", "(real:x):real");
	cxt.result().assign( lk::besy0( cxt.arg(0).as_number() ));
}

static void _mbesy1( lk::invoke_t &cxt )
{
	LK_DOC("besy1", "Computes the value of the Bessel function of the second kind, order 1, Y1(x)", "(real:x):real");
	cxt.result().assign( lk::besy1( cxt.arg(0).as_number() ));
}

static void _mbesi0( lk::invoke_t &cxt )
{
	LK_DOC("besi0", "Computes the value of the modified Bessel function of the first kind, order 0, I0(x)", "(real:x):real");
	cxt.result().assign( lk::besi0( cxt.arg(0).as_number() ));
}

static void _mbesi1( lk::invoke_t &cxt )
{
	LK_DOC("besi1", "Computes the value of the modified Bessel function of the first kind, order 1, I1(x)", "(real:x):real");
	cxt.result().assign( lk::besi1( cxt.arg(0).as_number() ));
}

static void _mbesk0( lk::invoke_t &cxt )
{
	LK_DOC("besk0", "Computes the value of the modified Bessel function of the second kind, order 0, K0(x)", "(real:x):real");
	cxt.result().assign( lk::besk0( cxt.arg(0).as_number() ));
}

static void _mbesk1( lk::invoke_t &cxt )
{
	LK_DOC("besk1", "Computes the value of the modified Bessel function of the second kind, order 1, K1(x)", "(real:x):real");
	cxt.result().assign( lk::besk1( cxt.arg(0).as_number() ));
}

static void _gammaln( lk::invoke_t &cxt )
{
	LK_DOC("gammaln", "Computes the logarithm of the Gamma function.", "(real):real");
	cxt.result().assign( lk::gammln( cxt.arg(0).as_number() ) );
}

void _pearson( lk::invoke_t &cxt )
{	
	LK_DOC("pearson", "Calculate the Pearson linear rank correlation coefficient of two arrays.", "(array:x, array:y):real");
		
	if ( cxt.arg_count() != 2
		|| cxt.arg(0).type() != lk::vardata_t::VECTOR
		|| cxt.arg(1).type() != lk::vardata_t::VECTOR
		|| cxt.arg(0).length() < 2
		|| cxt.arg(1).length() != cxt.arg(0).length() )
	{
		cxt.error( "pearson must be supplied with 2 arrays of the same length" );
		return;
	}
	
	int len = cxt.arg(0).length();

	double *x = new double[len];
	double *y = new double[len];

	for (int i=0;i<len;i++)
	{
		x[i] = cxt.arg(0).index(i)->as_number();
		y[i] = cxt.arg(1).index(i)->as_number();
	}

	cxt.result().assign( lk::pearson( x, y, len ) );

	delete [] x;
	delete [] y;
}


void _erf( lk::invoke_t &cxt )
{
	LK_DOC("erf", "Calculates the value of the error function", "(real):real");
	cxt.result().assign( lk::erf( cxt.arg(0).as_number() ) );
}


void _erfc( lk::invoke_t &cxt )
{
	LK_DOC("erfc", "Calculates the value of the complementary error function", "(real):real");
	cxt.result().assign( lk::erfc( cxt.arg(0).as_number() ) );
}


lk::fcall_t* lk::stdlib_basic()
{
	static const lk::fcall_t vec[] = {
		_to_int,
		_to_real,
		_to_bool,
		_to_string,
		_alloc,
		_path_only,
		_file_only,
		_ext_only,
		_extensions,
		_ostype,
		_stable_sort,
		_json_write,
		_json_read,
		0 };


	return (fcall_t*)vec;
}

static bool sql_initialized = false;
#define SQLITE3_VERIFY_INIT if (!sql_initialized ) { sqlite3_initialize(); sql_initialized = true; }


class sqlite3_obj_ref : public lk::objref_t
{
public:
	sqlite3 *db;
	sqlite3_obj_ref() { db = 0; }
	sqlite3_obj_ref( sqlite3 *_db ) : db(_db) { }
	virtual ~sqlite3_obj_ref() { if ( db ){ sqlite3_close(db); db=0; } }
	virtual lk_string type_name() { return "sqlite3"; }
};

#define GET_SQLITE3(obj, iarg) sqlite3_obj_ref *obj = dynamic_cast<sqlite3_obj_ref*>( cxt.env()->query_object( cxt.arg(iarg).as_unsigned() ) )

void _sql_open( lk::invoke_t &cxt )
{
	LK_DOC( "sql_open", "Open a SQLITE3 database file.", "(string:file):sql-obj-ref" );
	SQLITE3_VERIFY_INIT;
	sqlite3 *db = 0;
	int result = sqlite3_open( (const char*)cxt.arg(0).as_string().c_str(), &db );

	if ( SQLITE_OK != result )
	{
		sqlite3_close( db );
		cxt.result().nullify();
	}

	size_t objref = cxt.env()->insert_object( new sqlite3_obj_ref(db) );
	cxt.result().assign( (double)objref );
}

void _sql_close( lk::invoke_t &cxt )
{
	LK_DOC( "sql_close", "Close a SQLITE3 database file.", "(sql-obj-ref:db):none" );
	SQLITE3_VERIFY_INIT;
	if ( GET_SQLITE3(obj, 0) )
	{
		if ( obj->db )
		{
			sqlite3_close( obj->db );
			obj->db = 0;
		}

		cxt.env()->destroy_object( obj );
	}
}

void _sql_exec( lk::invoke_t &cxt )
{
	LK_DOC( "sql_exec", "Execute a SQL command on a SQLITE3 database and possibly return any data.", "(sql-obj-ref:db, string:command):variant" );
	SQLITE3_VERIFY_INIT;

	if ( GET_SQLITE3( obj, 0 ) )
	{
		if ( !obj->db ) return;

		sqlite3_stmt *stmt = 0;
		if ( SQLITE_OK != sqlite3_prepare( obj->db, (const char*)cxt.arg(1).as_string().c_str(), -1, &stmt, NULL ) )
		{
			sqlite3_finalize( stmt );
			return; // return null to indicate statement error
		}

		lk::vardata_t &R = cxt.result(); // return value;
		R.empty_vector();
		
		while( sqlite3_step( stmt ) == SQLITE_ROW )
		{
			int ncol = sqlite3_column_count( stmt );

			R.vec()->push_back( lk::vardata_t() );
			lk::vardata_t &row = R.vec()->back();
			row.empty_vector();
			row.vec()->reserve( ncol );

			for( int i=0;i<ncol;i++ )
			{
				int type = sqlite3_column_type( stmt, i );
				switch( type )
				{
				case SQLITE_INTEGER:
					row.vec_append( (double)sqlite3_column_int( stmt, i ) );
					break;

				case SQLITE_FLOAT:
					row.vec_append( (double)sqlite3_column_double( stmt, i ) );
					break;

				case SQLITE_TEXT:
					row.vec_append( lk_string( sqlite3_column_text( stmt, i ) ) );
					break;

				case SQLITE_BLOB:
					{
						const unsigned char *blob = (const unsigned char*)sqlite3_column_blob( stmt, i );
						int len = sqlite3_column_bytes( stmt, i );

						lk_string hexstr;
						// convert to hex chars
						for( int i=0;i<len;i++ )
						{
							char buf[16];
							sprintf( buf, "%02X", blob[i] );
							hexstr += buf;
						}

						row.vec_append( hexstr );
					}
					break;

				case SQLITE_NULL:
					{
						lk::vardata_t nv;
						nv.nullify();
						row.vec()->push_back( nv );
					}
					break;
				};
			} // value column loop

		} // sqlite3_step()

		sqlite3_finalize( stmt );
	}

}

void _sql_error( lk::invoke_t &cxt ) 
{
	LK_DOC( "sql_error", "Returns any error messages from the SQLITE3 engine", "(sql-obj-ref:db):string" );
		SQLITE3_VERIFY_INIT;

	if ( GET_SQLITE3( obj, 0 ) )
	{
		if ( !obj->db ) return;
		cxt.result().assign( lk_string(sqlite3_errmsg( obj->db )) );
	}
}



lk::fcall_t* lk::stdlib_sysio()
{
	static const lk::fcall_t vec[] = {
		_dir_list,
		_file_exists,
		_dir_exists,
		_rename_file,
		_remove_file,
		_mkdir,
		_cwd,
		_system,
		_write_text_file,
		_read_text_file,
		_open,
		_close,
		_seek,
		_tell,
		_eof,
		_flush,
		_read_line,
		_write_line,
		_read,
		_write,
		_load_extension,
		_json_file,
		_sql_open,
		_sql_close,
		_sql_exec,
		_sql_error,
		0 };


	return (fcall_t*)vec;
}

lk::fcall_t* lk::stdlib_string()
{
	static const lk::fcall_t vec[] = {
		_sprintf,
		_strlen,
		_strpos,
		_strcmp,
		_stricmp,
		_first_of,
		_last_of,
		_left,
		_right,
		_mid,
		_ascii,
		_char,
		_ch,
		_isdigit,
		_isalpha,
		_isalnum,
		_upper,
		_lower,
		_replace,
		_split,
		_join,
		_real_array,
		0 };

	return (fcall_t*)vec;
}

lk::fcall_t* lk::stdlib_math()
{
	static const lk::fcall_t vec[] = {
		_mceil,
		_mround,
		_mfloor,
		_msqrt,
		_mpow,
		_mexp,
		_mlog,
		_mlog10,
		_mpi,
		_msgn,
		_mabs,
		_msin,
		_mcos,
		_mtan,
		_masin,
		_macos,
		_matan,
		_matan2,
		_msind,
		_mcosd,
		_mtand,
		_masind,
		_macosd,
		_matand,
		_matan2d,
		_mnan,
		_misnan,
		_mmod,
		_msum,
		_mmin,
		_mmax,
		_mmean,
		_mmedian,
		_mstddev,
		_gammaln,
		_pearson,
		_mbesj0,
		_mbesj1,
		_mbesy0,
		_mbesy1,
		_mbesi0,
		_mbesi1,
		_mbesk0,
		_mbesk1,
		_erf,
		_erfc,
		0 };
		
	return (fcall_t*)vec;
}


std::vector< lk_string > lk::dir_list( const lk_string &path, const lk_string &extlist, bool ret_dirs )
{
	std::vector< lk_string > list;
	std::vector< lk_string > extensions = split(lower_case(extlist), ",");
	DIR *dir;
	struct dirent *ent;

	dir = ::opendir( (const char*)path.c_str() );
	if (!dir) return list;

	while( (ent=readdir(dir)) )
	{
		lk_string item(ent->d_name);
		if (item == "." || item == "..")
			continue;

		if ( extlist.empty()
			|| extlist=="*"
			||  ( ret_dirs && dir_exists( (const char*)lk_string(path + "/" + item ).c_str() )) )
		{
			list.push_back( item );
		}
		else
		{
			bool found = false;
			lk_string ext = lower_case(ext_only( item ));
			for (size_t i=0;!found && i<extensions.size();i++)
			{
				if (ext == extensions[i])
				{
					list.push_back(item);
					found = true;
				}
			}
		}
	}

	closedir(dir);

	return list;
}

#ifdef _MSC_VER
/* taken from wxMSW-2.9.1/include/wx/defs.h - appropriate for Win32/Win64 */
//#define va_copy(d, s) ((d)=(s))
#endif

std::vector< lk_string > lk::split( const lk_string &str, const lk_string &delim, bool ret_empty, bool ret_delim )
{
	std::vector< lk_string > list;

	lk_char cur_delim;
	lk_string::size_type m_pos = 0;
	lk_string token;

	while (m_pos < str.length())
	{
		lk_string::size_type pos = str.find_first_of(delim, m_pos);
		if (pos == lk_string::npos)
		{
			cur_delim = 0;
			token.assign(str, m_pos, lk_string::npos);
			m_pos = str.length();
		}
		else
		{
			cur_delim = str[pos];
			lk_string::size_type len = pos - m_pos;
			token.assign(str, m_pos, len);
			m_pos = pos + 1;
		}

		if (token.empty() && !ret_empty)
			continue;

		list.push_back( token );

		if ( ret_delim && cur_delim != 0 && m_pos < str.length() )
			list.push_back( to_string( cur_delim ) );
	}

	return list;
}

lk_string lk::join( const std::vector< lk_string > &list, const lk_string &delim )
{
	lk_string str;
	for (std::vector<lk_string>::size_type i=0;i<list.size();i++)
	{
		str += list[i];
		if (i < list.size()-1)
			str += delim;
	}
	return str;
}

size_t lk::replace( lk_string &s, const lk_string &old_text, const lk_string &new_text)
{
	const size_t uiOldLen = old_text.length();
	const size_t uiNewLen = new_text.length();

	lk_string::size_type pos = 0;
	size_t uiCount = 0;
	while(1)
	{
		pos = s.find(old_text, pos);
		if ( pos == lk_string::npos )
			break;

		// replace this occurrence of the old string with the new one
#ifdef LK_USE_WXWIDGETS
		s.replace(pos, uiOldLen, new_text, uiNewLen);
#else
		s.replace(pos, uiOldLen, new_text.c_str(), uiNewLen);
#endif

		// move past the string that was replaced
		pos += uiNewLen;

		// increase replace count
		uiCount++;
	}

	return uiCount;
}

lk_string lk::lower_case( const lk_string &in )
{
	lk_string ret(in);
	for (lk_string::size_type i=0;i<ret.length();i++)
		ret[i] = lk::lower_char(ret[i]);
	return ret;
}

lk_string lk::upper_case( const lk_string &in )
{
	lk_string ret(in);
	for (lk_string::size_type i=0;i<ret.length();i++)
		ret[i] = lk::upper_char(ret[i]);
	return ret;
}

bool lk::file_exists( const char *file )
{
#ifdef _WIN32
	// from wxWidgets: must use GetFileAttributes instead of ansi C
	// b/c can cope with network (unc) paths
	DWORD ret = ::GetFileAttributesA( file );
	return (ret != (DWORD)-1) && !(ret & FILE_ATTRIBUTE_DIRECTORY);
#else
	struct stat st;
	return stat(file, &st) == 0 && S_ISREG(st.st_mode);
#endif
}

bool lk::rename_file( const lk_string &f0, const lk_string &f1 )
{
#ifdef LK_USE_WXWIDGETS
	return wxRenameFile( f0, f1 );
#else
	return ::rename( (const char*)f0.c_str(), (const char*)f1.c_str() ) == 0;
#endif
}

bool lk::dir_exists( const char *path )
{
#ifdef _WIN32
	// Windows fails to find directory named "c:\dir\" even if "c:\dir" exists,
	// so remove all trailing backslashes from the path - but don't do this for
	// the paths "d:\" (which are different from "d:") nor for just "\"
	char *wpath = _strdup( path );
	if (!wpath) return false;

	int pos = strlen(wpath)-1;
	while (pos > 1 && (wpath[pos] == '/' || wpath[pos] == '\\'))
	{
		if (pos == 3 && wpath[pos-1] == ':') break;

		wpath[pos] = 0;
		pos--;
	}

	DWORD ret = ::GetFileAttributesA(wpath);
	bool exists =  (ret != (DWORD)-1) && (ret & FILE_ATTRIBUTE_DIRECTORY);

	free( wpath );

	return exists;
#else
	struct stat st;
	return ::stat(path, &st) == 0 && S_ISDIR(st.st_mode);
#endif
}

bool lk::remove_file( const char *path )
{
	return 0 == ::remove( path );
}

#ifdef _WIN32
#define make_dir(x) ::_mkdir(x)
#else
#define make_dir(x) ::mkdir(x, 0777)
#endif

bool lk::mkdir( const char *path, bool make_full )
{
	if (make_full)
	{
		std::vector<lk_string> parts = split( path, "/\\" );

		if (parts.size() < 1) return false;

		lk_string cur_path = parts[0] + path_separator();

		for (size_t i=1;i<parts.size();i++)
		{
			cur_path += parts[i];

			if ( !dir_exists((const char*)cur_path.c_str()) )
				if (0 != make_dir( (const char*)cur_path.c_str() ) ) return false;

			cur_path += path_separator();
		}

		return true;
	}
	else
		return 0 == make_dir( path );
}

lk_string lk::path_only( const lk_string &path )
{
	lk_string::size_type pos = path.find_last_of("/\\");
	if (pos==lk_string::npos) return path;
	else return path.substr(0, pos);
}

lk_string lk::name_only( const lk_string &path )
{
	lk_string::size_type pos = path.find_last_of("/\\");
	if (pos==lk_string::npos) return path;
	else return path.substr(pos+1);
}

lk_string lk::ext_only( const lk_string &path )
{
	lk_string::size_type pos = path.find_last_of('.');
	if (pos==lk_string::npos) return path;
	else return path.substr(pos+1);
}

char lk::path_separator()
{
#ifdef _WIN32
	return '\\';
#else
	return '/';
#endif
}

lk_string lk::get_cwd()
{
	char buf[2048];
#ifdef _WIN32
	::GetCurrentDirectoryA( 2047, buf );
#else
	::getcwd(buf, 2047);
#endif
	buf[2047] = 0;
	return lk_string(buf);
}

bool lk::set_cwd( const lk_string &path )
{
#ifdef _WIN32
	return ::SetCurrentDirectoryA( (const char*)path.c_str() ) != 0;
#else
	return ::chdir( (const char*)path.c_str() ) == 0;
#endif
}

lk_string lk::read_file( const lk_string &file )
{
	lk_string buf;
	char c;
	FILE *fp = fopen( (const char*)file.c_str(), "r");
	if (fp)
	{
		while ( (c=fgetc(fp))!=EOF )
			buf += c;
		fclose(fp);
	}
	return buf;
}

bool lk::read_line( FILE *fp, lk_string &buf, int prealloc )
{
	int c;

	buf = "";
	if (prealloc > 10)
		buf.reserve( (size_t)prealloc );

	// read the whole line, 1 character at a time, no concern about buffer length
	while ( (c=fgetc(fp)) != EOF && c != '\n' && c != '\r')
		buf += (char)c;

	// handle windows <CR><LF>
	if (c == '\r')
	{
		if ( (c=fgetc(fp)) != '\n')
			ungetc(c,fp);
	}

	// handle a stray <CR>
	if (c == '\n')
	{
		if ( (c=fgetc(fp)) != '\r')
			ungetc(c,fp);
	}

	return !(buf.length() == 0 && c == EOF);
}


#ifdef _WIN32

int lk::sync_piped_process::spawn(const lk_string &command, const lk_string &workdir)
{
	int result = 0;

	lk_string lastwd;
	if ( !workdir.empty() )
	{
		lastwd = lk::get_cwd();
		lk::set_cwd( workdir );
	}

	SECURITY_ATTRIBUTES sa;
	sa.nLength= sizeof(SECURITY_ATTRIBUTES);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = TRUE;


	HANDLE hStdoutReadEnd, hStdoutWriteEnd;

	hStdoutReadEnd = hStdoutWriteEnd = INVALID_HANDLE_VALUE;


	if (!CreatePipe( &hStdoutReadEnd, &hStdoutWriteEnd, &sa, 0 ))
		return -90;

	// prep and launch redirected child here
	PROCESS_INFORMATION pi;
	STARTUPINFOA si;

	ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));
	pi.hProcess = INVALID_HANDLE_VALUE;
	pi.hThread = INVALID_HANDLE_VALUE;

	// Set up the start up info struct.
	ZeroMemory(&si,sizeof(STARTUPINFO));
	si.cb = sizeof(STARTUPINFO);
	si.dwFlags = STARTF_USESTDHANDLES;
	si.hStdOutput = hStdoutWriteEnd;

	// Use this if you want to hide the child:
	//     si.wShowWindow = SW_HIDE;
	// Note that dwFlags must include STARTF_USESHOWWINDOW if you want to
	// use the wShowWindow flags.


	// Launch the process that you want to redirect (in this case,
	// Child.exe). Make sure Child.exe is in the same directory as
	// redirect.c launch redirect from a command line to prevent location
	// confusion.
	if (result == 0 && !CreateProcessA(NULL,(char*)(const char*)command.c_str(),NULL,NULL,TRUE,
					 //CREATE_NEW_CONSOLE|CREATE_NO_WINDOW|NORMAL_PRIORITY_CLASS
					 //CREATE_NEW_CONSOLE
					 CREATE_NO_WINDOW,
					 NULL,
					 NULL, /*workdir.IsEmpty()?NULL:(char*)workdir.c_str(),*/
					 &si,&pi))
	{
		result = -99;
	}

	// read childs output

	CHAR lpBuffer[256];
	DWORD nBytesRead;
//	DWORD nCharsWritten;

	lk_string line;
	while (  WaitForSingleObject( pi.hProcess, 1 ) == WAIT_TIMEOUT
		&& hStdoutReadEnd != INVALID_HANDLE_VALUE )
	{
		line = "";
		// read a text line from the output
		while( result == 0 && hStdoutReadEnd != INVALID_HANDLE_VALUE)
		{
			// wait for something to appear
			DWORD navail = 0, rc;
			int npeek = 0;
			while( hStdoutReadEnd != INVALID_HANDLE_VALUE )
			{
				rc = PeekNamedPipe( hStdoutReadEnd, NULL, 0, NULL, &navail, NULL );
				if (!rc)
				{
					CloseHandle( hStdoutReadEnd );
					hStdoutReadEnd = INVALID_HANDLE_VALUE;
					result = -97;
					break;
				}

				if (navail > 0)
					break;

				// make sure somehow the process didn't end a while ago
				// and we're still in this loop for some reason
				if (WaitForSingleObject( pi.hProcess, 1 ) != WAIT_TIMEOUT
					|| npeek++ > 500 )
				{
					CloseHandle( hStdoutReadEnd );
					hStdoutReadEnd = INVALID_HANDLE_VALUE;
					break;
				}

				::Sleep( 5 );
			}

			if ( hStdoutReadEnd == INVALID_HANDLE_VALUE
				|| !ReadFile(hStdoutReadEnd,lpBuffer, 1, &nBytesRead,NULL)
				|| nBytesRead == 0)
			{
					CloseHandle( hStdoutReadEnd );
					hStdoutReadEnd = INVALID_HANDLE_VALUE;
					break; // pipe done
			}

			if (lpBuffer[0] != 0 && lpBuffer[0] != '\r' && lpBuffer[0] != '\n')
				line += lpBuffer[0];

			if (lpBuffer[0] == '\n' || lpBuffer[0] == 0)
				break; // line finished
		}

		on_stdout( line );

	}

	// make sure process ended
	if (pi.hProcess!=INVALID_HANDLE_VALUE)
		WaitForSingleObject( pi.hProcess, INFINITE );

	DWORD exitcode = 0;
	GetExitCodeProcess(pi.hProcess, &exitcode);
	if (result >= 0)
		result = exitcode;

	if (pi.hProcess!=INVALID_HANDLE_VALUE) CloseHandle( pi.hProcess );
	if (pi.hThread!=INVALID_HANDLE_VALUE) CloseHandle( pi.hThread );

	if (hStdoutReadEnd!=INVALID_HANDLE_VALUE) CloseHandle( hStdoutReadEnd );

	if ( !lastwd.empty() )
		lk::set_cwd( lastwd );

	return result;
}
#else
int lk::sync_piped_process::spawn(const lk_string &command, const lk_string &workdir)
{
	lk_string line;

	lk_string lastwd;
	if ( !workdir.empty() )
	{
		lastwd = lk::get_cwd();
		lk::set_cwd( workdir );
	}

	FILE *fp = popen( (const char*)command.c_str(), "r" );
	if (!fp)
		return -99;

	while ( lk::read_line(fp, line) )
		on_stdout(line);

	if ( !lastwd.empty() )
		lk::set_cwd( lastwd );

	return pclose( fp );
}

#endif

lk_string lk::format(const char *fmt, ...)
{
	if (!fmt || *fmt == 0) return "";

	va_list arglist;
	va_start( arglist, fmt );

	size_t ret = 0;

	int size = 512;
	char *buffer = new char[size];
	if (!buffer)
		return "";

	do
	{
		va_list argptr_copy;
		va_copy( argptr_copy, arglist );
		ret = lk::format_vn(buffer,size-1,fmt,argptr_copy);
		va_end( argptr_copy );

		if (ret == 0)
		{
			delete [] buffer;
			size *= 2;
			buffer = new char[size];
			if (!buffer)
				return "";
		}

	}
	while (ret == 0);

	va_end(arglist);

	lk_string s(buffer);
	if (buffer)
		delete [] buffer;

	return s;
}


#define TEMPLEN 128

lk_string lk::format_vl( const lk_string &fmt, const std::vector< vardata_t* > &args )
{
	size_t argidx = 0;
	char *pfmt = new char[fmt.length()+1];
	char *p = pfmt;
	strcpy(p,  (const char*) fmt.c_str());
	lk_string s;

	char temp[TEMPLEN];
	char tempfmt[TEMPLEN];
	char *decpt;
	int ndigit;
	int with_precision;
	char *with_comma;
	char prev;
	int i;
	char *tp;

	while ( *p )
	{
		if (*p != '%') s += *p++;
		else
		{
			p++;
			switch (*p)
			{
			case '%': s += *p++; break;
			case 's':
			case 'S':
				if (argidx < args.size())
					s += args[argidx++]->as_string();
				p++;
				break;
			case 'c':
			case 'C':
				if (argidx < args.size())
					s += (char)args[argidx++]->as_integer();
				p++;
				break;
			case 'x':
			case 'X':
				if (argidx < args.size())
					s += lk::format("%x", (unsigned int)args[argidx++]->as_integer());
				p++;
				break;
			case 'u':
			case 'U':
				if (argidx < args.size())
					s += lk::format("%u", (unsigned int)args[argidx++]->as_integer());
				p++;
				break;
			case 'd':
			case 'D':
				if (argidx < args.size())
					s += lk::format("%d", args[argidx++]->as_integer());
				p++;
				break;

			case 'l':
			case 'L':
			case 'f':
			case 'F':
			case 'g':
			case 'G':
			case '.':
				if (argidx < args.size())
				{
					double arg_double = args[argidx++]->as_number();

					with_precision = 0;
					with_comma = 0;
					tp = tempfmt;
					*tp++ = '%';
					if (*p == '.')
					{ /* accumulate the precision */
						with_precision = 1;
						*tp++ = *p++;
						while ( *p && isdigit(*p) )
							*tp++ = *p++;
					}
					*tp++ = 'l';
					if (*p == 'l' || *p == 'L')	p++;// skip lL
					if (*p == ',') // comma separated
					{
						*tp++ = 'f'; p++;
						with_comma = (char*)1;
					}
					else // fFgG
						*tp++ = *p++;

					*tp = '\0'; // end format string

					snprintf(temp, TEMPLEN, tempfmt, arg_double);

					i=0;
					if (with_comma)
					{
						decpt = strchr(temp, '.');
						if (!decpt) ndigit = strlen(temp);
						else ndigit = (int)(decpt-temp);
						i=0-ndigit%3;
					}

					if ((!with_precision || with_comma!=NULL) &&
						!strchr(tempfmt,'g') &&
						!strchr(tempfmt,'G'))
					{
						tp = temp+strlen(temp)-1;
						while (tp > temp && *tp == '0')
							*tp-- = 0;
						if (*tp == '.')
							*tp-- = 0;
					}

					tp = temp; decpt = 0; prev = 0;
					while (*tp )
					{
						if (*tp == '.') decpt = (char*)1;
						if (with_comma != NULL && isdigit(prev) && i%3==0 && !decpt) s += ',';
						prev = *tp;
						s += *tp++;
						i++;
					}
				}
				break;

			/* handle comma or money format (double precision) */
			case 'm':
			case 'M':
			case ',':
				if (argidx < args.size())
				{
					double arg_double = args[argidx++]->as_number();
					if (*p == ',')
					{
						snprintf(temp, TEMPLEN, "%lf", arg_double);
						if (strchr(temp,'e')!=NULL) snprintf(temp, TEMPLEN, "%d", (int)arg_double);
					}
					else snprintf(temp, TEMPLEN, "%.2lf",  arg_double);
					decpt = strchr(temp, '.');
					if (!decpt) ndigit = strlen(temp);
					else ndigit = (int)(decpt-temp);
					if (*p == ',')
					{
						tp = temp+strlen(temp)-1;
						while (tp > temp && *tp == '0')
							*tp-- = 0;
						if (*tp == '.')
							*tp-- = 0;
					}
					i=0-(ndigit%3); tp = temp; decpt = 0; prev = 0;
					while (*tp)
					{
						if (*tp == '.')	decpt = (char*)1;
						if ( isdigit(prev) && i%3==0 && !decpt) s += ',';
						prev = *tp;
						s += *tp++;
						i++;
					}
					p++;
				}
				break;
			}

		}

	}

	delete [] pfmt;

	return s;
}




size_t lk::format_vn(char *buffer, int maxlen, const char *fmt, va_list arglist)
{
	char *p = (char*)fmt, *bp = buffer, *tp;
	char *bpmax = buffer+maxlen-1;
	int i;

	char arg_char;
	char *arg_str;
	int arg_int;
	unsigned int arg_uint;
	double arg_double;

	char temp[TEMPLEN];
	char tempfmt[TEMPLEN];
	char *decpt;
	size_t ndigit;
	int with_precision;
	char *with_comma;
	char prev;

	if (!p)
	{
		*bp = 0;
		return 0;
	}

	while( *p && bp<bpmax )
	{
		if (*p != '%')	*bp++ = *p++;
		else
		{
			p++;
			switch (*p)
			{
			case 'd':
			case 'D':
			/* handle simple signed integer format */
				p++;
				arg_int = va_arg(arglist, int);
				sprintf(temp, "%d", arg_int);
				tp = temp;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;
				break;

			case 'u':
			case 'U':
			/* handle simple unsigned integer format */
				p++;
				arg_uint = va_arg(arglist, unsigned int);
				sprintf(temp, "%u", arg_uint);
				tp = temp;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;
				break;

			case 'x':
			case 'X':
			/* handle hexadecimal unsigned integer format */
				p++;
				arg_uint = va_arg(arglist, unsigned int);
				sprintf(temp, "%x", arg_uint);
				tp = temp;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;
				break;

			case 'c':
			case 'C':
			/* handle simple char format */
				arg_char = (char)va_arg(arglist, int);
				if ( bp+1<bpmax ) *bp++ = arg_char;
				p++;
				break;

			case 's':
			case 'S':
			/* handle simple string format */
				p++;
				arg_str = va_arg(arglist, char*);
				tp = arg_str;
				while (*tp && bp<bpmax)
					*bp++ = *tp++;
				break;

			case '%':
				if (bp+1<bpmax)	*bp++ = *p++;
				break;


			case 'l':
			case 'L':
			case 'f':
			case 'F':
			case 'g':
			case 'G':
			case '.':
				with_precision = 0;
				with_comma = 0;
				tp = tempfmt;
				*tp++ = '%';
				if (*p == '.')
				{ /* accumulate the precision */
					with_precision = 1;
					*tp++ = *p++;
					if (*p == '0') with_precision = 2;
					while ( *p && isdigit(*p) )
						*tp++ = *p++;
				}
				*tp++ = 'l';
				if (*p == 'l' || *p == 'L')	p++;// skip lL
				if (*p == ',') // comma separated
				{
					*tp++ = 'f'; p++;
					with_comma = (char*)1;
				}
				else // fFgG
					*tp++ = *p++;

				*tp = '\0'; // end format string
				arg_double = va_arg(arglist, double);

				sprintf(temp, tempfmt, (double)arg_double);

				i=0;
				if (with_comma)
				{
					decpt = strchr(temp, '.');
					if (!decpt) ndigit = strlen(temp);
					else ndigit = (int)(decpt-temp);
					i=0-ndigit%3;
				}

				if ((!with_precision || with_comma!=NULL) &&
					!strchr(tempfmt,'g') &&
					!strchr(tempfmt,'G') &&
					(!(with_precision == 2)) )
				{
					tp = temp+strlen(temp)-1;
					while (tp > temp && *tp == '0')
						*tp-- = 0;
					if (*tp == '.')
						*tp-- = 0;
				}

				tp = temp; decpt = 0; prev = 0;
				while (*tp && bp<bpmax)
				{
					if (*tp == '.') decpt = (char*)1;
					if (with_comma != NULL && isdigit(prev) && i%3==0 && !decpt && bp<bpmax) *bp++ = ',';
					prev = *tp;
					if (bp<bpmax) *bp++ = *tp++;
					i++;
				}

				break;

			/* handle comma or money format (double precision) */
			case 'm':
			case 'M':
			case ',':
				arg_double = va_arg(arglist, double);
				if (*p == ',')
				{
					sprintf(temp, "%lf", arg_double);
					if (strchr(temp,'e')!=NULL) sprintf(temp, "%d", (int)arg_double);
				}
				else sprintf(temp, "%.2lf",  arg_double);

				decpt = strchr(temp, '.');
				if (!decpt) ndigit = strlen(temp);
				else ndigit = (int)(decpt-temp);

				if (*p == ',')
				{
					tp = temp+strlen(temp)-1;
					while (tp > temp && *tp == '0') *tp-- = 0;

					if (*tp == '.') *tp-- = 0;
				}

				i=0-(ndigit%3); tp = temp; decpt = 0; prev = 0;
				while (*tp)
				{
					if (*tp == '.')	decpt = (char*)1;
					if ( isdigit(prev) && i%3==0 && !decpt && bp<bpmax) *bp++ = ',';
					prev = *tp;
					if (bp<bpmax) *bp++ = *tp;
					tp++; i++;
				}
				p++;
				break;
			}

		}

	}

	*bp = 0;

#undef TEMPLEN

	if (bp==bpmax) return 0;
	else return (bp-buffer);
}

lk_string lk::trim_to_columns(const lk_string &str, int numcols)
{
	lk_string buf;
	int len = (int)str.length();
	int col=0;
	for (int i=0;i<len;i++)
	{
		if (col == numcols)
		{
			while (i < len && str[i] != ' ' && str[i] != '\t' && str[i] != '\n')
			{
				buf += str[i];
				i++;
			}

			while (i < len && (str[i] == ' ' || str[i] == '\t'))
				i++;

			if (i<len)
				buf += '\n';
			col = 0;
			i--;
		}
		else
		{
			buf += str[i];

			if (str[i] == '\n')
				col = 0;
			else
				col++;
		}
	}

	return buf;
}

static lk_string _latexify_text( lk_string s )
{
	lk::replace(s, "\\", "\\textbackslash ");
	lk::replace(s, "$", "\\$");
	lk::replace(s, "<", "$\\lt$");
	lk::replace(s, ">", "$\\gt$");
	lk::replace(s, "{", "\\{");
	lk::replace(s, "}", "\\}");
	lk::replace(s, "%", "\\%");
	lk::replace(s, "&", "\\&");
	lk::replace(s, "_", "\\_");
	lk::replace(s, "#", "\\#");
	return s;
}

static lk_string format_sig(const lk_string &s)
{
	lk_string fmt = s;
	lk::replace( fmt, "(", "<b>(</b>" );
	lk::replace( fmt, ")", "<b>)</b>" );
	lk::replace( fmt, "integer", "<font color=#aa0099>integer</font>" );
	lk::replace( fmt, "real", "<font color=#aa0099>real</font>" );
	lk::replace( fmt, "string", "<font color=#aa0099>string</font>" );
	lk::replace( fmt, "number", "<font color=#aa0099>number</font>" );
	lk::replace( fmt, "array", "<font color=#aa0099>array</font>" );
	lk::replace( fmt, "table", "<font color=#aa0099>table</font>" );
	lk::replace( fmt, "character", "<font color=#aa0099>character</font>" );
	lk::replace( fmt, "boolean", "<font color=#aa0099>boolean</font>" );
	lk::replace( fmt, "null", "<font color=#aa0099>null</font>" );
	lk::replace( fmt, "any", "<font color=#aa0099>any</font>" );
	lk::replace( fmt, "void", "<font color=#777777>void</font>" );
	lk::replace( fmt, "none", "<font color=#777777>none</font>" );
	return fmt;
}

bool lk::json_read( const lk_string &json, lk::vardata_t &x, lk_string *err )
{
	lkJSONreader rd( json );
	bool ok = rd.parse( x );
	if ( err ) *err = rd.err();
	return ok;
}

lk_string lk::html_doc( const lk_string &title, fcall_t *lib )
{
	lk_string data = "<h3>" + title + "</h3>\n";
	size_t i=0;
	while ( fcall_t f = lib[i++] )
		data += html_doc( f );	

	return data;
}

lk_string lk::html_doc( fcall_t f )
{
	lk_string data;
	lk::doc_t d;
	if (lk::doc_t::info( f, d ))
	{
		data += "<b><font size=+1 color=#0066aa>" + d.func_name + "</font></b>&nbsp<font size=+1>" + format_sig(d.sig1) + "</font><br><i>" + d.desc1 + "</i>\n";
		if (d.has_2) data += "<br><br><b><font size=+1 color=#0066aa>" + d.func_name + "</font></b>&nbsp<font size=+1>" + format_sig(d.sig2)  + "</font><br><i>" + d.desc2 + "</i>\n";
		if (d.has_3) data += "<br><br><b><font size=+1 color=#0066aa>" + d.func_name + "</font></b>&nbsp<font size=+1>" + format_sig(d.sig3)  + "</font><br><i>" + d.desc3 + "</i>\n";
		data += "<hr>\n";
	}
	return data;
}

bool lk::tex_doc( const lk_string &file,
			  const lk_string &title,
			  fcall_t *lib )
{
	FILE *fp = fopen( (const char*)file.c_str(),  "w" );
	if (!fp)
		return false;

	fprintf(fp, "\\subsection{%s}\n", (const char*)title.c_str());
	size_t i=0;
	while ( fcall_t f = lib[i++] )
	{
		lk::doc_t d;
		if ( lk::doc_t::info(f, d))
		{
			/*

			fprintf(fp, "{\\large \\texttt{\\textbf{%s}}}\\\\\n",
					(const char*)_latexify_text(d.func_name).c_str() );

			if (!d.notes.empty())
			{
				fprintf(fp, "%s\\\\\\\\\n", (const char*)_latexify_text(d.notes).c_str());
			}

			fprintf(fp, "\\textsf{ %s }\\\\\n%s\\\\\n",
					(const char*)_latexify_text(d.sig1).c_str(),
					(const char*)_latexify_text(d.desc1).c_str() );

			if (d.has_2)
			{
				fprintf(fp, "\\\\\\textsf{ %s }\\\\\n%s\\\\\n",
						(const char*)_latexify_text(d.sig2).c_str(),
						(const char*)_latexify_text(d.desc2).c_str() );
			}

			if (d.has_3)
			{
				fprintf(fp, "\\\\\\textsf{ %s }\\\\\n%s\\\\\n",
						(const char*)_latexify_text(d.sig3).c_str(),
						(const char*)_latexify_text(d.desc3).c_str() );
			}
			*/
			

			fprintf(fp, "\\texttt{{\\large\\textbf{%s}}}\\textsf{%s}\\\\\n %s\n",
					(const char*)_latexify_text(d.func_name).c_str(),
					(const char*)_latexify_text(d.sig1).c_str(),
					(const char*)_latexify_text(d.desc1).c_str() );
			
			if (d.has_2)
			{
				fprintf(fp, "\n\\texttt{{\\large\\textbf{%s}}}\\textsf{%s}\\\\\n %s\n",
					(const char*)_latexify_text(d.func_name).c_str(),
					(const char*)_latexify_text(d.sig2).c_str(),
					(const char*)_latexify_text(d.desc2).c_str() );
			}

			if (d.has_3)
			{
				fprintf(fp, "\n\\texttt{{\\large\\textbf{%s}}}\\textsf{%s}\\\\\n %s\n",
					(const char*)_latexify_text(d.func_name).c_str(),
					(const char*)_latexify_text(d.sig3).c_str(),
					(const char*)_latexify_text(d.desc3).c_str() );
			}
			
			if (!d.notes.empty())
			{
				fprintf(fp, "\n\\emph{Notes:} %s\n", (const char*)_latexify_text(d.notes).c_str());
			}

			if (lib[i] != 0) fprintf(fp, "\n\\hrulefill\n\n");
		}
	}

	fclose(fp);
	return true;
}

double lk::besj0(double x)
{
	double ax,z;
	double xx,y,ans,ans1,ans2;

	if ((ax=fabs(x)) < 8.0) {
		y=x*x;
		ans1=57568490574.0+y*(-13362590354.0+y*(651619640.7
			+y*(-11214424.18+y*(77392.33017+y*(-184.9052456)))));
		ans2=57568490411.0+y*(1029532985.0+y*(9494680.718
			+y*(59272.64853+y*(267.8532712+y*1.0))));
		ans=ans1/ans2;
	} else {
		z=8.0/ax;
		y=z*z;
		xx=ax-0.785398164;
		ans1=1.0+y*(-0.1098628627e-2+y*(0.2734510407e-4
			+y*(-0.2073370639e-5+y*0.2093887211e-6)));
		ans2 = -0.1562499995e-1+y*(0.1430488765e-3
			+y*(-0.6911147651e-5+y*(0.7621095161e-6
			-y*0.934945152e-7)));
		ans=sqrt(0.636619772/ax)*(cos(xx)*ans1-z*sin(xx)*ans2);
	}
	return ans;
}

double lk::besj1(double x)
{
	double ax,z;
	double xx,y,ans,ans1,ans2;

	if ((ax=fabs(x)) < 8.0) {
		y=x*x;
		ans1=x*(72362614232.0+y*(-7895059235.0+y*(242396853.1
			+y*(-2972611.439+y*(15704.48260+y*(-30.16036606))))));
		ans2=144725228442.0+y*(2300535178.0+y*(18583304.74
			+y*(99447.43394+y*(376.9991397+y*1.0))));
		ans=ans1/ans2;
	} else {
		z=8.0/ax;
		y=z*z;
		xx=ax-2.356194491;
		ans1=1.0+y*(0.183105e-2+y*(-0.3516396496e-4
			+y*(0.2457520174e-5+y*(-0.240337019e-6))));
		ans2=0.04687499995+y*(-0.2002690873e-3
			+y*(0.8449199096e-5+y*(-0.88228987e-6
			+y*0.105787412e-6)));
		ans=sqrt(0.636619772/ax)*(cos(xx)*ans1-z*sin(xx)*ans2);
		if (x < 0.0) ans = -ans;
	}
	return ans;
}

double lk::besy0(double x)
{
//	double lk::besj0(double x);
	double z;
	double xx,y,ans,ans1,ans2;

	if (x < 8.0) {
		y=x*x;
		ans1 = -2957821389.0+y*(7062834065.0+y*(-512359803.6
			+y*(10879881.29+y*(-86327.92757+y*228.4622733))));
		ans2=40076544269.0+y*(745249964.8+y*(7189466.438
			+y*(47447.26470+y*(226.1030244+y*1.0))));
		ans=(ans1/ans2)+0.636619772*lk::besj0(x)*log(x);
	} else {
		z=8.0/x;
		y=z*z;
		xx=x-0.785398164;
		ans1=1.0+y*(-0.1098628627e-2+y*(0.2734510407e-4
			+y*(-0.2073370639e-5+y*0.2093887211e-6)));
		ans2 = -0.1562499995e-1+y*(0.1430488765e-3
			+y*(-0.6911147651e-5+y*(0.7621095161e-6
			+y*(-0.934945152e-7))));
		ans=sqrt(0.636619772/x)*(sin(xx)*ans1+z*cos(xx)*ans2);
	}
	return ans;
}

double lk::besy1(double x)
{
//	double lk::besj1(double x);
	double z;
	double xx,y,ans,ans1,ans2;

	if (x < 8.0) {
		y=x*x;
		ans1=x*(-0.4900604943e13+y*(0.1275274390e13
			+y*(-0.5153438139e11+y*(0.7349264551e9
			+y*(-0.4237922726e7+y*0.8511937935e4)))));
		ans2=0.2499580570e14+y*(0.4244419664e12
			+y*(0.3733650367e10+y*(0.2245904002e8
			+y*(0.1020426050e6+y*(0.3549632885e3+y)))));
		ans=(ans1/ans2)+0.636619772*(lk::besj1(x)*log(x)-1.0/x);
	} else {
		z=8.0/x;
		y=z*z;
		xx=x-2.356194491;
		ans1=1.0+y*(0.183105e-2+y*(-0.3516396496e-4
			+y*(0.2457520174e-5+y*(-0.240337019e-6))));
		ans2=0.04687499995+y*(-0.2002690873e-3
			+y*(0.8449199096e-5+y*(-0.88228987e-6
			+y*0.105787412e-6)));
		ans=sqrt(0.636619772/x)*(sin(xx)*ans1+z*cos(xx)*ans2);
	}
	return ans;
}


double lk::besi0(double x)
{
	double ax,ans;
	double y;

	if ((ax=fabs(x)) < 3.75) {
		y=x/3.75;
		y*=y;
		ans=1.0+y*(3.5156229+y*(3.0899424+y*(1.2067492
			+y*(0.2659732+y*(0.360768e-1+y*0.45813e-2)))));
	} else {
		y=3.75/ax;
		ans=(exp(ax)/sqrt(ax))*(0.39894228+y*(0.1328592e-1
			+y*(0.225319e-2+y*(-0.157565e-2+y*(0.916281e-2
			+y*(-0.2057706e-1+y*(0.2635537e-1+y*(-0.1647633e-1
			+y*0.392377e-2))))))));
	}
	return ans;
}

double lk::besk0(double x)
{
	//double lk::besi0(double x);
	double y,ans;

	if (x <= 2.0) {
		y=x*x/4.0;
		ans=(-log(x/2.0)*lk::besi0(x))+(-0.57721566+y*(0.42278420
			+y*(0.23069756+y*(0.3488590e-1+y*(0.262698e-2
			+y*(0.10750e-3+y*0.74e-5))))));
	} else {
		y=2.0/x;
		ans=(exp(-x)/sqrt(x))*(1.25331414+y*(-0.7832358e-1
			+y*(0.2189568e-1+y*(-0.1062446e-1+y*(0.587872e-2
			+y*(-0.251540e-2+y*0.53208e-3))))));
	}
	return ans;
}

double lk::besi1(double x)
{
	double ax,ans;
	double y;

	if ((ax=fabs(x)) < 3.75) {
		y=x/3.75;
		y*=y;
		ans=ax*(0.5+y*(0.87890594+y*(0.51498869+y*(0.15084934
			+y*(0.2658733e-1+y*(0.301532e-2+y*0.32411e-3))))));
	} else {
		y=3.75/ax;
		ans=0.2282967e-1+y*(-0.2895312e-1+y*(0.1787654e-1
			-y*0.420059e-2));
		ans=0.39894228+y*(-0.3988024e-1+y*(-0.362018e-2
			+y*(0.163801e-2+y*(-0.1031555e-1+y*ans))));
		ans *= (exp(ax)/sqrt(ax));
	}
	return x < 0.0 ? -ans : ans;
}

double lk::besk1(double x)
{
//	double lk::besi1(double x);
	double y,ans;

	if (x <= 2.0) {
		y=x*x/4.0;
		ans=(log(x/2.0)*lk::besi1(x))+(1.0/x)*(1.0+y*(0.15443144
			+y*(-0.67278579+y*(-0.18156897+y*(-0.1919402e-1
			+y*(-0.110404e-2+y*(-0.4686e-4)))))));
	} else {
		y=2.0/x;
		ans=(exp(-x)/sqrt(x))*(1.25331414+y*(0.23498619
			+y*(-0.3655620e-1+y*(0.1504268e-1+y*(-0.780353e-2
			+y*(0.325614e-2+y*(-0.68245e-3)))))));
	}
	return ans;
}

double lk::gammln(double xx)
{
	double x,y,tmp,ser;
	static double cof[6]={76.18009172947146,-86.50532032941677,
		24.01409824083091,-1.231739572450155,
		0.1208650973866179e-2,-0.5395239384953e-5};
	int j;

	y=x=xx;
	tmp=x+5.5;
	tmp -= (x+0.5)*log(tmp);
	ser=1.000000000190015;
	for (j=0;j<=5;j++) ser += cof[j]/++y;
	return -tmp+log(2.5066282746310005*ser/x);
}


double lk::betacf(double a, double b, double x) throw( lk::error_t )
{
#define MAXIT 100
#define EPS 3.0e-7
#define FPMIN 1.0e-30

	int m,m2;
	double aa,c,d,del,h,qab,qam,qap;

	qab=a+b;
	qap=a+1.0;
	qam=a-1.0;
	c=1.0;
	d=1.0-qab*x/qap;
	if (fabs(d) < FPMIN) d=FPMIN;
	d=1.0/d;
	h=d;
	for (m=1;m<=MAXIT;m++) {
		m2=2*m;
		aa=m*(b-m)*x/((qam+m2)*(a+m2));
		d=1.0+aa*d;
		if (fabs(d) < FPMIN) d=FPMIN;
		c=1.0+aa/c;
		if (fabs(c) < FPMIN) c=FPMIN;
		d=1.0/d;
		h *= d*c;
		aa = -(a+m)*(qab+m)*x/((a+m2)*(qap+m2));
		d=1.0+aa*d;
		if (fabs(d) < FPMIN) d=FPMIN;
		c=1.0+aa/c;
		if (fabs(c) < FPMIN) c=FPMIN;
		d=1.0/d;
		del=d*c;
		h *= del;
		if (fabs(del-1.0) < EPS) break;
	}
	
	if (m > MAXIT) throw lk::error_t("a or b too big, or MAXIT too small in betacf");
	return h;
	
#undef MAXIT
#undef EPS
#undef FPMIN
}

double lk::betai(double a, double b, double x) throw( lk::error_t )
{
	void nrerror(char error_text[]);
	double bt;

	if (x < 0.0 || x > 1.0) throw lk::error_t("Bad x in routine betai");
	if (x == 0.0 || x == 1.0) bt=0.0;
	else
		bt=exp(gammln(a+b)-gammln(a)-gammln(b)+a*log(x)+b*log(1.0-x));
	if (x < (a+1.0)/(a+b+2.0))
		return bt*betacf(a,b,x)/a;
	else
		return 1.0-bt*betacf(b,a,1.0-x)/b;
}

double lk::pearson( double *x, double *y, size_t len )
{
	// apd: implemented june 2012, from  http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient
	// matches output of excel's CORREL function.

	double xm = 0, ym = 0;
	for (size_t i=0;i<len;i++)
	{
		xm += x[i];
		ym += y[i];
	}
	xm /= len;
	ym /= len;

	double num = 0;
	double xden = 0;
	double yden = 0;

	for (size_t i=0;i<len;i++)
	{
		num += (x[i]-xm)*(y[i]-ym);
		xden += (x[i]-xm)*(x[i]-xm);
		yden += (y[i]-ym)*(y[i]-ym);
	}

	return num / sqrt(xden*yden);
}

void lk::gser(double *gamser, double a, double x, double *gln)
{
#define ITMAX 100
#define EPS 3.0e-7
	int n;
	double sum,del,ap;

	*gln=gammln(a);
	if (x <= 0.0) {
		if (x < 0.0) throw lk::error_t("x less than 0 in routine gser");
		*gamser=0.0;
		return;
	} else {
		ap=a;
		del=sum=1.0/a;
		for (n=1;n<=ITMAX;n++) {
			++ap;
			del *= x/ap;
			sum += del;
			if (fabs(del) < fabs(sum)*EPS) {
				*gamser=sum*exp(-x+a*log(x)-(*gln));
				return;
			}
		}
		throw lk::error_t("a too large, ITMAX too small in routine gser");
		return;
	}
#undef ITMAX
#undef EPS
}


void lk::gcf(double *gammcf, double a, double x, double *gln)
{
#define ITMAX 100
#define EPS 3.0e-7
#define FPMIN 1.0e-30
//	double gammln(double xx);
	void nrerror(char error_text[]);
	int i;
	double an,b,c,d,del,h;

	*gln=lk::gammln(a);
	b=x+1.0-a;
	c=1.0/FPMIN;
	d=1.0/b;
	h=d;
	for (i=1;i<=ITMAX;i++) {
		an = -i*(i-a);
		b += 2.0;
		d=an*d+b;
		if (fabs(d) < FPMIN) d=FPMIN;
		c=b+an/c;
		if (fabs(c) < FPMIN) c=FPMIN;
		d=1.0/d;
		del=d*c;
		h *= del;
		if (fabs(del-1.0) < EPS) break;
	}
	if (i > ITMAX) throw lk::error_t("a too large, ITMAX too small in gcf");
	*gammcf=exp(-x+a*log(x)-(*gln))*h;
#undef ITMAX
#undef EPS
#undef FPMIN
}

double lk::gammp(double a, double x)
{
	double gamser,gammcf,gln;

	if (x < 0.0 || a <= 0.0) throw lk::error_t("Invalid arguments in routine gammp");
	if (x < (a+1.0)) {
		gser(&gamser,a,x,&gln);
		return gamser;
	} else {
		gcf(&gammcf,a,x,&gln);
		return 1.0-gammcf;
	}
}

double lk::gammq(double a, double x)
{
	double gamser,gammcf,gln;

	if (x < 0.0 || a <= 0.0) throw lk::error_t("Invalid arguments in routine gammq");
	if (x < (a+1.0)) {
		gser(&gamser,a,x,&gln);
		return 1.0-gamser;
	} else {
		gcf(&gammcf,a,x,&gln);
		return gammcf;
	}
}

double lk::erf(double x)
{
	return x < 0.0 ? -gammp(0.5,x*x) : gammp(0.5,x*x);
}
double lk::erfc(double x)
{
	return x < 0.0 ? 1.0+gammp(0.5,x*x) : gammq(0.5,x*x);
}


#ifdef WIN32

/*

    Implementation of POSIX directory browsing functions and types for Win32.

    Author:  Kevlin Henney (kevlin@acm.org, kevlin@curbralan.com)
    History: Created March 1997. Updated June 2003.
    Rights:  See end of file.

*/
#include <errno.h>
#include <io.h> /* _findfirst and _findnext set errno iff they return -1 */
#include <stdlib.h>
#include <string.h>

struct DIR
{
	intptr_t                handle; /* -1 for failed rewind */
	struct _finddata_t  info;
    struct dirent       result; /* d_name null iff first time */
    char                *name;  /* null-terminated char string */
};

DIR *opendir(const char *name)
{
    DIR *dir = 0;

    if(name && name[0])
    {
        size_t base_length = strlen(name);
        const char *all = /* search pattern must end with suitable wildcard */
            strchr("/\\", name[base_length - 1]) ? "*" : "/*";

        if((dir = (DIR *) malloc(sizeof *dir)) != 0 &&
           (dir->name = (char *) malloc(base_length + strlen(all) + 1)) != 0)
        {
            strcat(strcpy(dir->name, name), all);

			if ((dir->handle = _findfirst(dir->name, &dir->info)) != -1)
				{
                dir->result.d_name = 0;
            }
            else /* rollback */
            {
                free(dir->name);
                free(dir);
                dir = 0;
            }
        }
        else /* rollback */
        {
            free(dir);
            dir   = 0;
            errno = ENOMEM;
        }
    }
    else
    {
        errno = EINVAL;
    }

    return dir;
}

int closedir(DIR *dir)
{
    int result = -1;

    if(dir)
    {
        if(dir->handle != -1)
        {
            result = _findclose(dir->handle);
        }

        free(dir->name);
        free(dir);
    }

    if(result == -1) /* map all errors to EBADF */
    {
        errno = EBADF;
    }

    return result;
}

struct dirent *readdir(DIR *dir)
{
    struct dirent *result = 0;

    if(dir && dir->handle != -1)
    {
        if(!dir->result.d_name || _findnext(dir->handle, &dir->info) != -1)
        {
            result         = &dir->result;
            result->d_name = dir->info.name;
        }
    }
    else
    {
        errno = EBADF;
    }

    return result;
}

void rewinddir(DIR *dir)
{
    if(dir && dir->handle != -1)
    {
        _findclose(dir->handle);
		dir->handle = _findfirst(dir->name, &dir->info);
		dir->result.d_name = 0;
    }
    else
    {
        errno = EBADF;
    }
}

#endif // WIN32 (for DIR,dirent)

