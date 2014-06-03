#ifndef __welcome_h
#define __welcome_h

#include <wx/wx.h>
#include <wx/socket.h>
#include <wx/sstream.h>
#include <wx/html/htmlwin.h>
#include <wx/hyperlink.h>

#include "simplecurl.h"


class MainWindow;
class wxMetroButton;
class wxMetroListBox;

class WelcomeScreen : public wxPanel
{
public:
	WelcomeScreen( wxWindow *parent);
	virtual ~WelcomeScreen();
	void UpdateRecentList();
		
	void AbortDownloadThreads();
private:
	void LayoutWidgets();
	
	void OnCommand( wxCommandEvent & );
	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);
	void OnMessagesLinkClicked(wxHtmlLinkEvent &);
	void OnDownloadTimeout( wxTimerEvent & );

	wxBitmap m_nrelLogo;

	wxHtmlWindow *m_htmlWin;
	int m_messageStatus;

	wxMetroButton *m_createCase;
	wxMetroButton *m_openExisting;
	wxMetroButton *m_openScript, *m_newScript, *m_btnAbout, *m_btnHelp, *m_btnQuit, *m_btnRegistration;
	wxMetroButton *m_onlineForum;
	wxMetroButton *m_helpSystem;
	wxMetroListBox *m_recent;
		
	wxTimer m_downloadTimer;
	
	wxSimpleCurlDownloadThread m_ssCurlMessage;
	void OnMessageDownloadThread( wxSimpleCurlEvent & );
	void UpdateMessagesHtml(const wxString &html);
	
	DECLARE_EVENT_TABLE()
};

#endif
