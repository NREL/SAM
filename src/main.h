#ifndef __sammain_h
#define __sammain_h

#include <exception>

#include <wx/app.h>
#include <wx/frame.h>
#include <wx/config.h>
#include <wx/filehistory.h>
#include <wx/dialog.h>

#include "inputpage.h"
#include "project.h"

class wxSimplebook;
class wxPanel;
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

	wxString GetUniqueCaseName( wxString base = wxEmptyString );
	void CreateNewCase( const wxString &name = wxEmptyString );
	CaseWindow *GetCaseWindow( Case *c );
	CaseWindow *CreateCaseWindow( Case *c );
	void DeleteCaseWindow( Case *c );
	void SwitchToCaseWindow( const wxString &name );

	bool LoadProject( const wxString &file );
	bool SaveProject( const wxString &file );

	void Save();
	void SaveAs();

	wxString GetProjectDisplayName();


protected:
	void OnClose( wxCloseEvent & );
	void OnCommand( wxCommandEvent & );
	void OnCaseTabChange( wxCommandEvent & );
	void OnCaseTabButton( wxCommandEvent & );
	void OnCaseMenu( wxCommandEvent & );
	void OnInternalCommand( wxCommandEvent & );

private:
#ifdef __WXOSX__
	wxMenuBar *m_menuBar;
	wxMenu *m_fileMenu, *m_caseMenu, *m_toolsMenu, *m_helpMenu;
#endif
	wxSimplebook *m_topBook;
	WelcomeScreen *m_welcomeScreen;
	wxPanel *m_caseTabPanel;
	wxMetroTabList *m_caseTabList;
	wxSimplebook *m_caseNotebook;

	ProjectFile m_project;
	wxString m_projectFileName;

	void UpdateFrameTitle();
	
	DECLARE_EVENT_TABLE();
};




class ConfigDatabase
{
public:
	ConfigDatabase();
	~ConfigDatabase();

	void Clear();
	void Add( const wxString &tech, const wxArrayString &fin );
	void SetConfig( const wxString &t, const wxString &f );
	void AddInputPageGroup( const wxArrayString &pages, const wxString &caption, const wxString &hlpcxt,
		bool exclusive = false, const wxString &exclvar = wxEmptyString );


	struct InputPageGroup
	{
		wxArrayString Pages;
		wxString Caption;
		wxString HelpContext;
		bool OrganizeAsExclusivePages;
		wxString ExclusivePageVar;
	};

	wxArrayString GetTechnologies();
	wxArrayString GetFinancingForTech(const wxString &tech);
	
	std::vector<InputPageGroup*> &GetInputPages(const wxString &tech, const wxString &financing );
	VarInfoLookup &GetVariables( const wxString &tech, const wxString &financing );
	EqnFastLookup &GetEquations( const wxString &tech, const wxString &financing );
	
private:
	struct TechInfo { wxString Name; wxArrayString FinancingOptions; };
	std::vector<TechInfo> m_techList;

	struct ConfigInfo { 
		ConfigInfo();
		~ConfigInfo();

		wxString Technology;
		wxString Financing;
		std::vector<InputPageGroup*> InputPages;
		VarInfoLookup Variables;
		EqnFastLookup Equations;
	};

	ConfigInfo *Find( const wxString &t, const wxString &f );

	std::vector<ConfigInfo*> m_configList;

	ConfigInfo *m_curConfig;	
	VarInfoLookup m_emptyVarList;
	EqnFastLookup m_emptyEqnList;
};

class SamApp : public wxApp
{
public:
	virtual bool OnInit();
	virtual int OnExit();

	static void Restart();
	static wxString GetAppPath();
	static wxString GetRuntimePath();
	static wxConfig &Settings();
	static MainWindow *Window();
	static wxFileHistory &FileHistory();	
	static wxArrayString RecentFiles();
	static void ShowHelp( const wxString &id );
	static wxString VersionStr();
	static int VersionMajor();
	static int VersionMinor();
	static int VersionMicro();
	
	static ConfigDatabase &Config();
	static VarDatabase &Variables();
	static EqnDatabase &Equations();
	static CallbackDatabase &Callbacks();
	static FormDatabase &Forms();

	static bool LoadAndRunScriptFile( const wxString &script_file, wxArrayString *errors = 0 );
};

DECLARE_APP( SamApp );


class wxCheckBox;
class wxMetroListBox;

class ConfigDialog : public wxDialog
{
public:
	ConfigDialog( wxWindow *parent, const wxSize &size = wxSize(700,570) );

	void SetConfiguration(const wxString &t, const wxString &f);
	bool GetConfiguration(wxString &t, wxString &f);

	void ShowResetCheckbox(bool b);
	bool ResetToDefaults();

private:
	void PopulateTech();
	bool ValidateSelections();
	void OnTechTree(wxCommandEvent &evt);
	void OnDoubleClick(wxCommandEvent &evt);

	wxMetroListBox *m_pTech;
	wxMetroListBox *m_pFin;

	wxString m_t, m_f;

	void OnHelp( wxCommandEvent &evt );

	wxCheckBox *m_pChkUseDefaults;
	DECLARE_EVENT_TABLE();
};

bool ShowConfigurationDialog( wxWindow *parent, wxString *tech, wxString *fin, bool *reset );



#endif

