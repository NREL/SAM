#ifndef __simulation_h
#define __simulation_h

#include <wx/string.h>
#include <wx/stream.h>

#include <lk_env.h>

#include "variables.h"

class Case;
class ConfigInfo;

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
	
	void ListByCount( size_t n, wxArrayString &list );
	void GetVariableLengths( std::vector<size_t> &varlengths );
	
	bool Invoke(bool silent=false);

	// results and messages if it succeeded
	bool Ok();
	wxArrayString &GetErrors();
	wxArrayString &GetWarnings();
	
	static bool ListAllOutputs( ConfigInfo *cfg, 
		wxArrayString *names, wxArrayString *labels, wxArrayString *units, bool single_values = false );

private:
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
