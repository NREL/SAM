#ifndef __simulation_h
#define __simulation_h

#include <wx/string.h>
#include <lk_env.h>
#include "variables.h"

class SimulationContext;

/* 

General simulation process:

1. create a new Simulation based on the case inputs and the configuration
2. override any inputs you may want
3. call prepare inputs to setup internal memory with updated calculations
4. create a new SimulationEngine
5. add all the Simulation objects to its collection, invoke RunAll().
6. the simulation engine will first prepare the inputs for all the configurations,
   which means making copies of the inputs and re-evaluating all the equations.  it will
   then efficiently dispatch the simulations to the available
   processor cores and round-robin run until all are completed (or canceled).  it will
   show a progress dialog with the ability to cancel the threads and show progress updates
   and individual simulation messages from each core
7. upon completion, the dialog will go away, and the results will be available
   for each simulation if it was successful

Individual thread-safe simulation process (Invoke):

1. For the technology specified, the appropriate LK script is loaded
2. It is up to the LK script to translate the SAM variables to SSC inputs,
   run the simulation by calling the appropriate SSC modules, and assigning
   the relevant SAM outputs.  Internally, when an ssc_module_exec call is made
   as invoked from a script, the progress information and notices/errors/warnings
   will be saved into the Simulation object.  If the SimulationContext object indicates
   that the simulation should be canceled, the flag is passed back to SSC which should
   terminate calculations and return.
  



class SimulationEngine;
class Case;

lk::fcall_t *invoke_simulation_stubs();

class Simulation
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

private:
	Case *m_case;
	wxArrayString m_overrides;
	VarTable m_inputs;
	wxString m_name;
	wxString m_tech, m_fin;
	VarTable m_results;
	wxArrayString m_errors, m_warnings;

	
	bool PrepareInputs(); // transfer all the values and recalculate equations
	bool Invoke( const wxString &name, SimulationContext *context );
	bool Invoke( SimulationContext *context = 0 );
};

class SimulationEngine
{
public:
	SimulationEngine();
	~SimulationEngine();

	void Add( Simulation *sim );
	bool RunAll( int ncpus = 0 );
};

*/

#endif
