#ifndef __macro_h
#define __macro_h

#include <wx/splitter.h>
#include <wx/html/htmlwin.h>

#include <wex/lkscript.h>
#include <wex/uiform.h>

class wxMetroButton;
class wxMetroListBox;
class wxTextCtrl;
class wxHtmlWindow;
class wxPanel;
class Case;
class wxStaticText;


class MacroEngine
{

public:
	MacroEngine();
	virtual ~MacroEngine();

	// note: the args variable should be heap allocated with 'new'
	// and will be owned and deleted by this Run method
	bool Run( const wxString &script, lk::vardata_t *args = 0 );
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
	
	lk::vardata_t *GetUIArgs();
	void GetUIArgs( lk::vardata_t &table );
	int SetUIArgs( lk::vardata_t &table );

	int ReadUIData( const wxString &file );
	bool WriteUIData( const wxString &file );
private:
	void OnCommand( wxCommandEvent & );
	void OnHtmlLink( wxHtmlLinkEvent & );
	void UpdateHtml();
	void ListScripts( const wxString &path, wxArrayString &list );
	
	Case *m_case;
	wxMetroListBox *m_listbox;
	wxHtmlWindow *m_html;
	wxString m_curMacroPath;
	wxTextCtrl *m_output;
	wxArrayString m_macroList;
	wxMetroButton *m_run, *m_stop, *m_code;
	wxPanel *m_leftPanel;
	wxPanel *m_rightPanel;

	struct ui_item {
		wxString name;
		wxStaticText *label;
		wxWindow *window;
	};

	std::vector<ui_item> m_ui;
	wxPanel *m_macroUI;
	wxFlexGridSizer *m_macroUISizer;
	void ClearUI();
	void CreateUI( const wxString &buf );
	ui_item *FindItem( const wxString &name );

	DECLARE_EVENT_TABLE();

};

#endif
