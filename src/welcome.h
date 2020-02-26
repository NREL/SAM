#ifndef __welcome_h
#define __welcome_h

#include <wx/wx.h>
#include <wx/socket.h>
#include <wx/sstream.h>
#include <wx/html/htmlwin.h>
#include <wx/hyperlink.h>

#include <wex/easycurl.h>

class wxStaticText;

class MainWindow;

class wxMetroButton;

class wxMetroListBox;

class WelcomeScreen : public wxPanel {
public:
    WelcomeScreen(wxWindow *parent);

    virtual ~WelcomeScreen();

    void UpdateRecentList();

    void AbortDownloadThreads();

private:
    void LayoutWidgets();


    wxArrayString m_qstartScripts;

    void OnQStartScript(wxCommandEvent &);

    void OnCommand(wxCommandEvent &);

    void OnPaint(wxPaintEvent &evt);

    void OnResize(wxSizeEvent &evt);

    void OnMessagesLinkClicked(wxHtmlLinkEvent &);

    void OnDownloadTimeout(wxTimerEvent &);

    wxHtmlWindow *m_htmlWin;
    int m_messageStatus;

    //wxStaticText *m_versionLabel;
    wxMetroButton *m_createCase;
    wxMetroButton *m_openExisting;
    wxMetroButton *m_openScript, *m_newScript, *m_btnAbout, *m_btnHelp, *m_btnGetStarted, *m_btnQuit;
    wxMetroListBox *m_recent;

    wxTimer m_downloadTimer;

    wxEasyCurl m_ssCurlMessage;

    void OnMessageDownloadThread(wxEasyCurlEvent &);

    void UpdateMessagesHtml(const wxString &html);

    void RunWelcomeScript(const wxString &script);

DECLARE_EVENT_TABLE()
};

#endif
