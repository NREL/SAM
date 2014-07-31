#ifndef __macro_h
#define __macro_h

#include <wx/splitter.h>
#include <wx/html/htmlwin.h>

class wxMetroButton;
class wxMetroListBox;
class wxTextCtrl;
class wxHtmlWindow;
class wxPanel;
class Case;


class MacroEngine
{

public:
	MacroEngine();
	virtual ~MacroEngine();

	bool Run( const wxString &script );
	void Stop();
	virtual void Output( const wxString &text );
	virtual void ClearOutput();
	virtual bool IsStopFlagSet();

private:
	bool m_stopFlag;
};

class MacroPanel : public wxSplitterWindow, public MacroEngine
{
public:
	MacroPanel( wxWindow *parent, Case *cc );

	void ConfigurationChanged();
	
	virtual void Output( const wxString &text );
	virtual void ClearOutput();

private:
	void OnCommand( wxCommandEvent & );
	void OnHtmlLink( wxHtmlLinkEvent & );
	void UpdateHtml();
	void ListScripts( const wxString &path, wxArrayString &list );

	Case *m_case;
	wxMetroListBox *m_listbox;
	wxHtmlWindow *m_html;
	wxTextCtrl *m_output;
	wxArrayString m_macroList;
	wxMetroButton *m_run, *m_stop;
	wxPanel *m_rightPanel;

	DECLARE_EVENT_TABLE();

};

#endif
