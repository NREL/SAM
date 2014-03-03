#ifndef __simulation_h
#define __simulation_h

#include <wx/string.h>
#include <lk_env.h>
#include "variables.h"
#include "basecase.h"

class SimulationContext;

/* 

General simulation process:

1. create a new Simulation based on the case inputs and the configuration
2. override any inputs you may want
3. call prepare inputs to setup internal memory with updated calculations
4. create a new SimulationEngine
5. add all the Simulation objects to its collection, invoke RunAll().
6. the simulation engine will first prepare the inputs for all the configurations,
   which means making copies of the inputs and re-evaluate  all the equations.  it will
   then efficiently dispatch the simulations to the available
   processor cores and round-robin run until all are completed (or canceled).  it will
   show a progress dialog with the ability to cancel the threads and show progress updates
   and individual simulation messages from each core
7. upon completion, the dialog will go away, and the results will be available
   for each simulation if it was successful

  
*/

class Case;
class SimulationEngine;

class Simulation : public DataProvider
{
	friend class SimulationEngine;
public:
	Simulation( Case *cc, const wxString &name );

	// setting up the simulation
	void Override( const wxString &name, const VarValue &val );
	wxString GetOverridesLabel( bool with_labels = true );
	void SetName( const wxString &s ) { m_name = s; }
	wxString GetName() { return m_name; }
	VarValue *GetInput( const wxString &name );
	
	// results and messages if it succeeded
	bool Ok();
	wxArrayString &GetErrors();
	wxArrayString &GetWarnings();
	VarTable &Results();

	virtual wxArrayString GetVariables();
	virtual VarValue *GetValue( const wxString &var );
	virtual wxString GetLabel( const wxString &var );
	virtual wxString GetUnits( const wxString &var );
	
	bool Invoke();

private:
	Case *m_case;
	wxString m_name;
	wxArrayString m_overrides;
	VarTable m_inputs;
	VarTable m_results;
	wxArrayString m_errors, m_warnings;
	StringHash m_outputLabels, m_outputUnits;
};

class SimulationEngine
{
public:
	SimulationEngine();
	~SimulationEngine();

	void Add( Simulation *sim );
	bool RunAll( int ncpus = 0 );
};

#endif
