#ifndef __script_h
#define __script_h

#include <vector>
#include <wx/frame.h>
#include <wx/stc/stc.h>

class SamScriptCtrl;
class wxTextCtrl;
class wxMetroButton;

class ScriptWindow  : public wxFrame
{
public:
	ScriptWindow( wxWindow *parent, int id = wxID_ANY, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize );

	static ScriptWindow *CreateNewWindow( bool show = true );
	static void OpenFiles( ScriptWindow *current = 0);
	static std::vector<ScriptWindow*> GetWindows();
	static ScriptWindow *FindOpenFile( const wxString &file );
	static bool CloseAll();	

	 // cancel any SAM simulations started from a script in fcall_simulate()
	static void CancelRunningSimulations();

	void AddOutput( const wxString &out );
	void ClearOutput();

	bool Save();
	bool SaveAs();
	bool Load( const wxString &file );
	bool Write( const wxString &file );
	wxString GetFileName();

	bool Find( const wxString &text, bool match_case, bool whole_word, bool at_beginning,
		int *pos, int *line, wxString *line_text );

private:
	SamScriptCtrl *m_script;
	wxTextCtrl *m_output;
	wxMetroButton *m_runBtn, *m_stopBtn;
	wxString m_fileName;
	wxString m_lastTitle;

	int m_lastFindPos;

	void UpdateWindowTitle();
	void OnCommand( wxCommandEvent & );
	void OnModified( wxStyledTextEvent & );
	void OnClose( wxCloseEvent & );

	DECLARE_EVENT_TABLE();
};


#endif

