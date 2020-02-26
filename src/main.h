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

#ifndef __sammain_h
#define __sammain_h

// to load binary ui forms and defaults uncomment following
//#define UI_BINARY 1

#include <exception>
#include <memory>

#include <wx/app.h>
#include <wx/frame.h>
#include <wx/config.h>
#include <wx/filehistory.h>
#include <wx/dialog.h>
#include <wx/dataview.h>

#include "inputpage.h"
#include "project.h"


/* Macros for C++11 support */
template <typename T>
struct smart_ptr
{
#if __cplusplus <= 201103L
	typedef std::unique_ptr<T> ptr;
#else
	typedef std::auto_ptr<T> ptr;
#endif
};

// SAM API key to use with developer.nrel.gov services
extern const char *sam_api_key;

class wxSimplebook;
class wxPanel;
class wxMetroButton;
class wxMetroTabList;

class SamException : public std::exception
{
	wxString m_err;
public:
	SamException( const wxString &err ) : m_err(err) { }
	virtual ~SamException() throw() { } 
	const char *what() const throw() { return (const char*)m_err.c_str(); };
};

class WelcomeScreen;
class CaseWindow;

class MainWindow : public wxFrame
{
public:
	MainWindow( );

	ProjectFile &Project() { return m_project; }

	bool CreateProject();
	bool CloseProject();
	void ImportCases();

	wxString GetUniqueCaseName( wxString base = wxEmptyString );
	bool CreateNewCase( const wxString &name = wxEmptyString, 
		wxString tech = wxEmptyString, 
		wxString fin = wxEmptyString );

	CaseWindow *GetCaseWindow( Case *c );
	CaseWindow *CreateCaseWindow( Case *c );
	void DeleteCaseWindow( Case *c );
	bool SwitchToCaseWindow( const wxString &name );

	bool LoadProject( const wxString &file );
	bool SaveProject( const wxString &file );

	void Save();
	void SaveAs();

	wxString GetProjectDisplayName();
	wxString GetProjectFileName();

	Case *GetCurrentCase();
	CaseWindow *GetCurrentCaseWindow();
	void CaseVarGrid(std::vector<Case*> &cases);
	
protected:
	void OnClose( wxCloseEvent & );
	void OnCommand( wxCommandEvent & );
	void OnCaseTabChange( wxCommandEvent & );
	void OnCaseTabButton( wxCommandEvent & );
	void OnCaseMenu( wxCommandEvent & );
	void OnInternalCommand( wxCommandEvent & );

private:
	wxSimplebook *m_topBook;
	WelcomeScreen *m_welcomeScreen;
	wxPanel *m_caseTabPanel;
	wxMetroButton *m_mainMenuButton;
	wxMetroTabList *m_caseTabList;
	wxSimplebook *m_caseNotebook;

	ProjectFile m_project;
	wxString m_projectFileName;

	bool CheckVersionBeforeSaving( const wxString &file );
	void UpdateFrameTitle();
	
	DECLARE_EVENT_TABLE();
};

class ScriptDatabase
{
public:
	ScriptDatabase();
	virtual ~ScriptDatabase();

	bool LoadFile( const wxString &file );
	bool LoadScript( const wxString &source );
	void ClearAll();

	lk::node_t *Lookup( const wxString &method_name, const wxString &obj_name );	
	lk::env_t *GetEnv() { return &m_cbenv; }

protected:
	struct cb_data{ lk::node_t *tree; wxString source; };
	std::vector<cb_data*> m_cblist;
	lk::env_t m_cbenv;
};


class InputPageData
{
	wxUIFormData m_form;
	VarDatabase m_vars;
	
	wxString m_eqnScript;
	wxString m_cbScript;

	EqnDatabase m_eqns;
	ScriptDatabase m_cbs;

public:
	InputPageData();

	void Clear();
	void Write(wxOutputStream &os);
	bool Read(wxInputStream &is);

	void Write_text(wxOutputStream &os, wxString &);
	bool Read_text(wxInputStream &is, wxString &);

	wxUIFormData &Form() { return m_form; }
	VarDatabase &Variables() { return m_vars; }
	wxString &EqnScript() { return m_eqnScript; }
	wxString &CbScript() { return m_cbScript; }
	
	bool BuildDatabases();

	EqnDatabase &Equations() { return m_eqns; }
	ScriptDatabase &Callbacks() { return m_cbs; }

	
};


typedef unordered_map<wxString, InputPageData*, wxStringHash, wxStringEqual> InputPageDataHash;

class InputPageDatabase
{
public:
	InputPageDatabase();
	~InputPageDatabase();

	void Add( const wxString &name, InputPageData *data );
	InputPageData *Lookup( const wxString &name );
	void Clear();

	bool LoadFile(const wxString &file);
	bool LoadFileText(const wxString &file);
private:
	InputPageDataHash m_hash;
};

class PageInfo
{
public:
	PageInfo() {
		Collapsible = CollapsedByDefault = false;
	}
	PageInfo( const wxString &_name ) {
		Name = _name;
		Collapsible = CollapsedByDefault = false;
	}
	wxString Name;
	wxString Caption;
	bool Collapsible;
	wxString CollapsiblePageVar;
	bool CollapsedByDefault;
	wxString ShowHideLabel;
};

struct InputPageGroup
{
	std::vector< std::vector<PageInfo> > Pages;
	wxString SideBarLabel;
	wxString HelpContext;
	bool OrganizeAsExclusivePages;
	wxString ExclusivePageVar;
	std::vector< PageInfo > ExclusiveHeaderPages;
	bool ExclusiveTabs;
};

	
class ConfigInfo
{ 
public:
	ConfigInfo();
	~ConfigInfo();

	wxString Technology;
	wxString Financing;
	wxArrayString Simulations;
	std::vector<InputPageGroup*> InputPageGroups;
	InputPageDataHash InputPages;
	VarInfoLookup Variables;		
	EqnFastLookup Equations;
	
	StringHash Settings;

	// storage for variables specific to this configuration
	// this variables are automatically added when the configuration
	// cache is generated, and serve purposes like exclusive pages
	// and collapsible panes
	VarDatabase AutoVariables;
};

struct ConfigOptions
{
	wxString LongName;
	wxString ShortName;
	wxString TreeParent;
	wxString Description;
};

class ConfigDatabase
{
public:
	ConfigDatabase();
	~ConfigDatabase();

	void Clear();
	void Add( const wxString &tech, const wxArrayString &fin );
	void SetConfig( const wxString &t, const wxString &f );
	ConfigInfo *CurrentConfig() { return m_curConfig; }
	
	void SetModules( const wxArrayString &list );
	void AddInputPageGroup( const std::vector< std::vector<PageInfo> > &pages, 
		const wxString &sidebar,
		const wxString &hlpcxt,
		const wxString &exclvar,
		const std::vector<PageInfo> &exclhdr_pages,
		bool excl_tabs );

	wxArrayString GetTechnologies();
	wxArrayString GetFinancingForTech(const wxString &tech);
	
/*
	std::vector<InputPageGroup*> &GetInputPageGroups(const wxString &tech, const wxString &financing );
	VarInfoLookup &GetVariables( const wxString &tech, const wxString &financing );
	EqnFastLookup &GetEquations( const wxString &tech, const wxString &financing );
	InputPageDataHash &GetInputPages( const wxString &tech, const wxString &financing );
*/
	void CachePagesInConfiguration( std::vector<PageInfo> &Pages, ConfigInfo *ci );
	void RebuildCaches();

	ConfigInfo *Find( const wxString &t, const wxString &f );

	static ConfigOptions &Options( const wxString &name );


private:
	struct TechInfo { wxString Name; wxArrayString FinancingOptions; };
	std::vector<TechInfo> m_techList;
	std::vector<ConfigInfo*> m_configList;
	ConfigInfo *m_curConfig;
};

class SamApp : public wxApp
{
public:

	struct ver { int major, minor, micro; };

	SamApp();
	virtual bool OnInit();
	virtual int OnExit();
	virtual void OnFatalException();

	static void Restart();
	static wxString ReadProxyFile();
	static bool WriteProxyFile( const wxString & );
	static wxString WebApi( const wxString &name );
	static wxString GetAppPath();
	static wxString GetRuntimePath();
	static wxString GetUserLocalDataDir();
	static wxConfig &Settings();
	static MainWindow *Window();
	static ProjectFile &Project();
	static wxFileHistory &FileHistory();	
	static wxArrayString RecentFiles();
	static void ShowHelp( const wxString &context = wxEmptyString );
	static wxString VersionStr( bool with_patches = false, bool short_style = false );
	static int VersionMajor();
	static int VersionMinor();
	static int VersionMicro();
	static size_t Version( int *maj=0, int *min=0, int *mic=0, int nrelease=0 );
	static int RevisionNumber();
	static int NumReleases();

	static wxWindow *CurrentActiveWindow();	
	static void CheckForUpdates( bool quiet );

	static ConfigDatabase &Config();
	static InputPageDatabase &InputPages();
	static ScriptDatabase &GlobalCallbacks();

	static bool LoadAndRunScriptFile( const wxString &script_file, wxArrayString *errors = 0 );
};

// provide global function version for use in mswfatal.cpp
size_t sam_version( int *maj=0, int *min=0, int *mic=0 );

DECLARE_APP( SamApp );


class wxCheckBox;
class wxMetroButton;
class wxMetroListBox;
class wxMetroDataViewTreeCtrl;

class ConfigDialog : public wxDialog
{
public:
	ConfigDialog( wxWindow *parent, const wxSize &size = wxScaleSize(700,570) );

	void SetConfiguration(const wxString &t, const wxString &f);
	void GetConfiguration(wxString &t, wxString &f);

	void ShowResetCheckbox(bool b);
	bool ResetToDefaults();

	virtual int ShowModal();
	virtual void EndModal( int retCode );

private:
	void PopulateTech();
	bool ValidateSelections();
	void OnTreeActivated(wxDataViewEvent &evt);
	void OnTechTree(wxDataViewEvent &);
	void OnFinTree(wxDataViewEvent &);
	void OnFinTreeDoubleClick(wxDataViewEvent &);

	void UpdateFinTree();

	wxMetroDataViewTreeCtrl *m_pTech, *m_pFin;
	wxArrayString m_tnames, m_fnames;
	wxString m_techname, m_finname;

	void OnOk( wxCommandEvent & );
	void OnCancel( wxCommandEvent & );
	void OnHelp( wxCommandEvent & );
	void OnCharHook( wxKeyEvent & );

	wxCheckBox *m_pChkUseDefaults;
	DECLARE_EVENT_TABLE();
};

bool ShowConfigurationDialog( wxWindow *parent, wxString *tech, wxString *fin, bool *reset );



#endif

