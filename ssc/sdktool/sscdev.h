#ifndef __SC_h
#define __SC_h

#include <wx/wx.h>
#include <wx/wfstream.h>
#include <wx/datstrm.h>

#include "dllinvoke.h"

class DataView;
class SCFrame;
class SCDocWin;
class wxNotebook;
class EditorWindow;
class wxConfig;

extern SCFrame *app_frame;
extern wxConfig *app_config;

void applog(const wxString &s);
void applog(const char *fmt, ...);

extern int SC_major_ver;
extern int SC_minor_ver;
extern int SC_micro_ver;

class SCApp : public wxApp
{
public:
	virtual bool OnInit();
	virtual int OnExit();
};

DECLARE_APP(SCApp)

class wxMetroNotebook;
class wxExtGridCtrl;

class SCFrame : public wxFrame
{
public:
	SCFrame();
	virtual ~SCFrame();
		
	bool CloseDocument();
	bool LoadBdat( wxString fn = wxEmptyString );

	bool LoadScript(wxString fn = wxEmptyString);


	void SaveBdat();
	bool WriteBdatToDisk(const wxString &fn);
	
	void ChooseDynamicLibrary();
	void LoadUnloadLibrary();

	std::vector<bool> Start();
	void ClearLog();
	void Log(const wxString &, bool wnl=true);
	
	static void Copy( ssc_data_t p_data, var_table *vt, bool clear_first );
	static void Copy( var_table *vt,  ssc_data_t p_data, bool clear_first );

	void Progress(const wxString &text, float percent);

	wxString LastFileName() { return m_lastFile; }

	DataView *GetDataView() { return m_dataView; }
	var_table *GetVarTable() { return m_varTable; }


	void SetProgress( int percent, const wxString &msg = wxEmptyString );
	
	wxArrayString GetAvailableCMs();
	void LoadCMs();
	void SetCurrentCM( const wxString & cm ) { m_currentCM->SetStringSelection( cm ); }
	wxString GetCurrentCM() { return m_currentCM->GetStringSelection(); }
	void UpdateCMForm();
	void OnRun( wxCommandEvent & );
	void OnCMListSelect(wxCommandEvent &evt);
	void OnCopyToClipboard(wxCommandEvent &);

	bool UpdateIsStopFlagSet();

private:	
	void WriteVarTable( wxDataOutputStream &o, var_table &vt );
	bool ReadVarTable( wxDataInputStream &o, var_table &vt, bool clear_first );

	void UpdateUI();

	void OnCommand(wxCommandEvent &evt);
	void OnCloseFrame(wxCloseEvent &evt);
	
	wxExtGridCtrl *m_gridCM;
	wxChoice *m_currentCM;
	wxListBox *m_listCM;

	wxStaticText *m_statusLabel;
	wxGauge *m_progressBar;

	wxString m_currentAppDir;
	wxString m_lastFile;
	wxString m_dllPath;

	wxTextCtrl *m_txtOutput;

	wxMetroNotebook *m_notebook;
	DataView *m_dataView;
	EditorWindow *m_scriptWindow;

	var_table *m_varTable;
		
	DECLARE_EVENT_TABLE()
};

#endif
