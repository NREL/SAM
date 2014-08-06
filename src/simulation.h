#ifndef __simulation_h
#define __simulation_h

// to enable lk script generation, uncomment following line
//#define __LK_DEBUG_FILE__

#include <wx/string.h>
#include <wx/stream.h>
#include <wx/dialog.h>

#include <lk_env.h>

#include <ssc/sscapi.h>

#include "variables.h"

class Case;
class ConfigInfo;

class ISimulationHandler
{
private:
	wxArrayString m_savedErrors, m_savedWarnings;
public:
	ISimulationHandler() { }
	virtual ~ISimulationHandler() { }

	virtual void Update( float percent, const wxString & ) = 0;
	virtual bool IsCancelled() = 0;
	
	// override Error and Warn to enable real-time
	// update of simulation messages elsewhere
	virtual void Error( const wxString &s ) { m_savedErrors.Add( s ); }
	virtual void Warn( const wxString &s ) { m_savedWarnings.Add( s ); }

	// handle saved messages
	virtual wxArrayString GetErrors() { return m_savedErrors; }
	virtual wxArrayString GetWarnings() { return m_savedWarnings; }
	virtual void ClearSavedMessages() { m_savedErrors.clear(); m_savedWarnings.clear(); }
	
	// optionally output a debug file before the SSC module is run
	virtual bool WriteDebugFile( 
		const wxString &sim, 
		ssc_module_t, 
		ssc_data_t ) { return false; }
};

class ThreadProgressDialog;
class SimulationDialog;

class Simulation
{
public:
	Simulation( Case *cc, const wxString &name );
	
	void Write( wxOutputStream & );
	bool Read( wxInputStream & );
	void Copy( const Simulation & );

	void Clear();

	// setting up the simulation
	void Override( const wxString &name, const VarValue &val );
	wxString GetOverridesLabel( bool with_labels = true );
	void SetName( const wxString &s ) { m_name = s; }
	wxString GetName() { return m_name; }
	VarValue *GetInput( const wxString &name );

	Case *GetCase() { return m_case; }
	
	virtual wxArrayString ListOutputs();
	virtual VarValue *GetOutput( const wxString &var );
	virtual wxString GetLabel( const wxString &var );
	virtual wxString GetUnits( const wxString &var );
	virtual VarTable &Outputs();

	// returns an output or input, outputs have precedence
	VarValue *GetValue( const wxString &name );

	VarTable *GetInputVarTable() { return &m_inputs; }
	
	void ListByCount( size_t n, wxArrayString &list );
	void GetVariableLengths( std::vector<size_t> &varlengths );
	
	// prepares and runs simulation
	bool Invoke(bool silent=false, bool prepare=true);
	
	bool Prepare(); // not threadable, but must be called before below
	bool InvokeWithHandler( ISimulationHandler *ih );

	// results and messages if it succeeded
	bool Ok();
	wxArrayString &GetErrors();
	wxArrayString &GetWarnings();
	wxArrayString GetAllMessages();
		
	static bool ListAllOutputs( ConfigInfo *cfg, 
		wxArrayString *names, 
		wxArrayString *labels, 
		wxArrayString *units, 
		wxArrayString *groups, 
		bool single_values = false );

	static int DispatchThreads( ThreadProgressDialog &tpd, 
		std::vector<Simulation*> &sims, 
		int nthread );
	static int DispatchThreads( SimulationDialog &tpd, 
		std::vector<Simulation*> &sims, 
		int nthread );

protected:
	Case *m_case;
	wxArrayString m_simlist;
	wxString m_name;
	wxArrayString m_overrides;
	VarTable m_inputs;
	wxArrayString m_outputList;
	VarTable m_outputs;
	wxArrayString m_errors, m_warnings;
	StringHash m_outputLabels, m_outputUnits;
};

class wxGauge;
class wxTextCtrl;
class wxStaticText;
class wxMetroButton;

class ThreadProgressDialog : public wxDialog
{
public:
	ThreadProgressDialog(wxWindow *parent, int nthreads);
		bool IsCanceled() { return m_canceled; }
	void Log( const wxArrayString &list );
	void Log( const wxString &text );
	void Update(int ThreadNum, float percent, const wxString &label = wxEmptyString );
	void Status( const wxString & );
	void Reset();
	void ShowBars( int n=-1 );
	void OnCancel(wxCommandEvent &evt);
	void OnClose( wxCommandEvent &evt );
	void OnDialogClose(wxCloseEvent &evt);
	void SetButtonText( const wxString &txt );
	bool HasMessages();
	wxString GetMessages();

protected:
	bool m_canceled;
	std::vector<wxStaticText*> m_labels;
	std::vector<wxGauge*> m_progbars;
	std::vector<wxTextCtrl*> m_percents;
	wxStaticText *m_status;
	wxTextCtrl *m_log;
	wxMetroButton *m_button;
	wxFrame *m_transp;
	
	DECLARE_EVENT_TABLE()
};

class SimulationDialog
{
public:
	SimulationDialog( const wxString &message=wxEmptyString, int nthreads = 0 );
	~SimulationDialog();
	
	// update the Status title and visiblity of bars.  Calls yield.
	void NewStage( const wxString &title, int nbars_to_show=-1 );
	
	// if messages appeared during simulation,
	// show the dialog as modal
	void Finalize( const wxString &custom_title = wxEmptyString );
	
	// update progress, calls yield
	void Update(int ThreadNum, float percent, const wxString &label = wxEmptyString );

	// these don't call yield
	void Log( const wxArrayString &list ) { m_tpd->Log(list); }
	void Log( const wxString &s ) { m_tpd->Log(s); }

	bool Canceled() { return m_tpd->IsCanceled(); }
		
	ThreadProgressDialog &Dialog() { return *m_tpd; }
	
private:
	ThreadProgressDialog *m_tpd;
	wxFrame *m_transp;
};


#endif
