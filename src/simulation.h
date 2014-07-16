#ifndef __simulation_h
#define __simulation_h

#include <wx/string.h>
#include <wx/stream.h>

#include <lk_env.h>

#include <ssc/sscapi.h>

#include "variables.h"

class Case;
class ConfigInfo;

class ISimulationHandler
{
public:
	ISimulationHandler() { }
	virtual ~ISimulationHandler() { }
	virtual void Error( const wxString & ) = 0;
	virtual void Warn( const wxString & ) = 0;
	virtual void Update( float percent, const wxString & ) = 0;
	virtual bool IsCancelled() = 0;
	virtual bool WriteDebugFile( const wxString &, ssc_module_t, ssc_data_t ) = 0;
};

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
	
	static bool ListAllOutputs( ConfigInfo *cfg, 
		wxArrayString *names, wxArrayString *labels, wxArrayString *units, bool single_values = false );

	static int DispatchThreads( std::vector<Simulation*> &sims, int nthread = 0 );

protected:


	Case *m_case;
	wxString m_name;
	wxArrayString m_overrides;
	VarTable m_inputs;
	wxArrayString m_outputList;
	VarTable m_outputs;
	wxArrayString m_errors, m_warnings;
	StringHash m_outputLabels, m_outputUnits;
};


#endif
