#ifndef __welcome_h
#define __welcome_h

#include <wx/wx.h>
#include <wx/socket.h>
#include <wx/sstream.h>
#include <wx/html/htmlwin.h>
#include <wx/hyperlink.h>

#include "wxcurlthread.h"
#include "simplecurl.h"


class MainWindow;
class wxMetroButton;

class WelcomeScreen : public wxPanel
{
public:
	WelcomeScreen( MainWindow *parent);
	virtual ~WelcomeScreen();
	void UpdateRecentList();
		
	void AbortDownloadThreads();
private:
	void LayoutWidgets();
	
	void OnCreateProject(wxCommandEvent &evt);
	void OnOpenRecent(wxCommandEvent &evt);

	void OnHyperlink(wxCommandEvent &);
	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);

	void OnLinkClicked(wxHtmlLinkEvent &);

	void OnDownloadTimeout( wxTimerEvent & );

	wxBitmap m_nrelLogo;


	wxHtmlWindow *m_htmlWin;
	MainWindow *m_pAppFrame;
	int m_messageStatus;

	wxTextCtrl *m_caseName;
	wxMetroButton *m_createCase;
	wxMetroButton *m_onlineForum;
	wxMetroButton *m_helpSystem;
	wxListBox *m_recent;
		
	wxTimer m_downloadTimer;
	
	wxSimpleCurlDownloadThread m_ssCurlMessage;
	void OnMessageDownloadThread( wxSimpleCurlEvent & );
	void UpdateMessagesHtml(const wxString &html);

	wxSimpleCurlDownloadThread m_ssCurlUpdate;
	void OnUpdateDownloadThread( wxSimpleCurlEvent & );

	wxString GetLocalMessagesFile();
	DECLARE_EVENT_TABLE()
};

#endif
