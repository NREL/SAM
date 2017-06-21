#ifndef __lkscript_h
#define __lkscript_h

#include <wx/wx.h>
#include <wx/thread.h>

#define LK_USE_WXWIDGETS 1

#include <lk/absyn.h>
#include <lk/env.h>
#include <lk/vm.h>

#include "wex/codeedit.h"
#include "wex/utils.h"

#define wxLK_STDLIB_BASIC  0x0001
#define wxLK_STDLIB_SYSIO  0x0002
#define wxLK_STDLIB_STRING 0x0004
#define wxLK_STDLIB_MATH   0x0008
#define wxLK_STDLIB_WXUI   0x0010
#define wxLK_STDLIB_PLOT   0x0020
#define wxLK_STDLIB_MISC   0x0040
#define wxLK_STDLIB_FILE   0x0080
#define wxLK_STDLIB_SOUT   0x0100 // out,outln via wxLKScriptCtrl::OnOutput() virtual method

#define wxLK_STDLIB_ALL \
	(wxLK_STDLIB_BASIC|wxLK_STDLIB_SYSIO|wxLK_STDLIB_STRING \
	|wxLK_STDLIB_MATH|wxLK_STDLIB_WXUI|wxLK_STDLIB_PLOT \
	|wxLK_STDLIB_MISC|wxLK_STDLIB_FILE) // by default don't include stdout functions

lk::fcall_t* wxLKPlotFunctions(); // newplot, plot, plotopt, plotout, gifanim
lk::fcall_t* wxLKMiscFunctions(); // geocode, curl, apikeys, rand
lk::fcall_t* wxLKFileFunctions(); // csvread, csvwrite, decompress
lk::fcall_t* wxLKStdOutFunctions(); // out, outln:  must use an extended wxLKScriptCtrl that implements ::OnOutput()

class wxPLPlotCtrl;

// if parent=NULL and no_parent=true, then the plot will have no parent.  
// otherwise, even if parent=NULL, LK will use the currently active 
// toplevel window as the parent.
void wxLKSetToplevelParentForPlots( wxWindow *parent ); 
void wxLKSetPlotTarget( wxPLPlotCtrl *plot );
wxPLPlotCtrl *wxLKGetPlotTarget();

class wxLKDebugger;
class wxLKScriptCtrl : 
	public wxCodeEditCtrl, 
	public wxThreadHelper
{
public:

	wxLKScriptCtrl( wxWindow *parent, int id = wxID_ANY,
		const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize,
		unsigned long libs = wxLK_STDLIB_ALL );

	virtual ~wxLKScriptCtrl();
	
	void SetSyntaxCheck( bool on );

	virtual bool OnEval( int line );
	virtual void OnOutput( const wxString & );
	virtual void OnSyntaxCheck( int line = -1, const wxString &error = wxEmptyString );
	
	void RegisterLibrary( lk::fcall_t *funcs, const wxString &group = "Miscellaneous", void *user_data = 0);

	wxString GetHtmlDocs();
	void ShowHelpDialog( wxWindow *custom_parent = 0 );

	bool IsScriptRunning();
	bool IsStopFlagSet();
	void Stop();

	void SetWorkDir( const wxString &path );
	wxString GetWorkDir();
	bool Execute();
	bool CompileAndLoad();
	void UpdateInfo();

	enum { DEBUG_RUN, DEBUG_STEP, DEBUG_SINGLE };
	bool Debug(int mode);

	lk::env_t *GetEnvironment() { return m_env; }
	
	
	struct libdata
	{
		lk::fcall_t *library;
		wxString name;
	};
private:

	wxString m_workDir;
	bool m_syntaxCheck;

	unsigned int m_syntaxCheckRequestId, m_syntaxCheckThreadId;
	wxString m_codeToSyntaxCheck;
	wxString m_syntaxCheckWorkDir;
	std::vector<int> m_syntaxErrorLines;
	wxArrayString m_syntaxErrorMessages;
	wxCriticalSection m_syntaxCheckCS;


	virtual wxThread::ExitCode Entry();
	void OnSyntaxCheckThreadFinished(wxThreadEvent& evt);
	void StartSyntaxCheckThread();

	void OnScriptTextChanged( wxStyledTextEvent & );
	void OnMarginClick( wxStyledTextEvent & );
	void OnTimer( wxTimerEvent & );

	wxTimer m_timer;

	std::vector<libdata> m_libs;
	lk::env_t *m_env;

	class my_vm : public lk::vm
	{
		wxLKScriptCtrl *m_lcs;
		size_t m_counter;
	public:
		my_vm( wxLKScriptCtrl *lcs );
		virtual bool on_run( const lk::srcpos_t &sp );
	};
	lk::bytecode m_bc;
	my_vm m_vm;

	wxString m_assemblyText;
	
	bool m_scriptRunning;
	bool m_stopScriptFlag;
	wxWindow *m_topLevelWindow;
	wxLKDebugger *m_debugger;
	bool m_debuggerFirstShow;

	DECLARE_EVENT_TABLE();
};

class wxBoxSizer;
class wxTextCtrl;
class wxMetroButton;
class wxLKScriptWindow;

class wxLKScriptWindowFactory
{
public:
	wxLKScriptWindowFactory();
	virtual ~wxLKScriptWindowFactory();
	virtual wxLKScriptWindow *Create() = 0;
};

class wxLKScriptWindow  : public wxFrame
{
public:
	wxLKScriptWindow( wxWindow *parent, int id = wxID_ANY, 
		const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxScaleSize( 760, 800 ) );
	
	static wxLKScriptWindowFactory &GetFactory();
	static void SetFactory( wxLKScriptWindowFactory *f );

	static wxLKScriptWindow *CreateNewWindow( bool show = true );
	static void OpenFiles();
	static std::vector<wxLKScriptWindow*> GetWindows();
	static wxLKScriptWindow *FindOpenFile( const wxString &file );
	static bool CloseAll();

	void AddOutput( const wxString &out );
	void ClearOutput();


	bool Save();
	bool SaveAs();
	bool Load( const wxString &file );
	bool Write( const wxString &file );
	wxString GetFileName();

	bool Find( const wxString &text, bool match_case, bool whole_word, bool at_beginning,
		int *pos, int *line, wxString *line_text );

	bool IsModified();
	wxLKScriptCtrl *GetEditor();
	
	virtual void OnHelp();
	virtual bool RunScript();
	virtual void StopScript();

protected:
	bool QueryAndCanClose();
	static void OpenFilesInternal( wxLKScriptWindow *current = 0);

	wxBoxSizer *m_toolbar;

	class MyScriptCtrl;
	MyScriptCtrl *m_script;
	wxTextCtrl *m_output;
	wxMetroButton *m_runBtn, *m_stopBtn;
	wxString m_fileName;
	wxString m_lastTitle;

	int m_lastFindPos;

	void UpdateWindowTitle();

private:
	void OnCommand( wxCommandEvent & );
	void OnModified( wxStyledTextEvent & );
	void OnClose( wxCloseEvent & );

	DECLARE_EVENT_TABLE();
};

#endif

