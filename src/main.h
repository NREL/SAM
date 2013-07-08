#ifndef __sammain_h
#define __sammain_h

#include <wx/app.h>
#include <wx/frame.h>
#include <wx/config.h>
#include <wx/filehistory.h>

#include <exception>

class SamException : public std::exception
{
	wxString m_err;
public:
	SamException( const wxString &err ) : m_err(err) { }
	virtual ~SamException() throw() { } 
	const char *what() const throw() { return (const char*)m_err.c_str(); };
};

class WelcomeScreen;
class MainWindow : public wxFrame
{
public:
	MainWindow( );

	void OnClose( wxCloseEvent & );

	DECLARE_EVENT_TABLE();

private:
	WelcomeScreen *m_welcomeScreen;
};


class SamApp : public wxApp
{
public:
	virtual bool OnInit();
	virtual int OnExit();

	static wxConfig &Config();
	static MainWindow *Window();
	static wxFileHistory &FileHistory();	
	static wxArrayString RecentFiles();
	static void ShowHelp( const wxString &id );
	static wxString VersionStr();
	static int VersionMajor();
	static int VersionMinor();

};

DECLARE_APP( SamApp );


#endif

