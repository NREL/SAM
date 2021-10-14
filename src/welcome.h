/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

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

class WelcomeScreen : public wxPanel
{
public:
	WelcomeScreen( wxWindow *parent);
	virtual ~WelcomeScreen();
	void UpdateRecentList();
		
	void AbortDownloadThreads();
private:
	void LayoutWidgets();
	

	wxArrayString m_qstartScripts;
	void OnQStartScript( wxCommandEvent & );

	void OnCommand( wxCommandEvent & );
	void OnPaint(wxPaintEvent &evt);
	void OnResize(wxSizeEvent &evt);
	void OnMessagesLinkClicked(wxHtmlLinkEvent &);
	void OnDownloadTimeout( wxTimerEvent & );

	wxHtmlWindow *m_htmlWin;
	int m_messageStatus;

	//wxStaticText *m_versionLabel;
	wxMetroButton *m_createCase;
	wxMetroButton *m_openExisting;
	wxMetroButton *m_openScript, *m_newScript, *m_btnAbout, *m_btnHelp, *m_btnGetStarted, *m_btnQuit;
	wxMetroListBox *m_recent;
		
	wxTimer m_downloadTimer;
	
	wxEasyCurl m_ssCurlMessage;
	void OnMessageDownloadThread( wxEasyCurlEvent & );
	void UpdateMessagesHtml(const wxString &html);

	void RunWelcomeScript( const wxString &script );
	
	DECLARE_EVENT_TABLE()
};

#endif
