/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __simulation_h
#define __simulation_h

#include <wx/string.h>
#include <wx/stream.h>
#include <wx/dialog.h>

#include <wex/tpdlg.h>

#include <lk/env.h>

#include <ssc/sscapi.h>

#include "variables.h"

class Case;
class ConfigInfo;

class ISimulationHandler
{
private:
	wxArrayString m_savedErrors, m_savedWarnings, m_savedNotices;
public:
	ISimulationHandler() { }
	virtual ~ISimulationHandler() { }

	virtual void Update( float percent, const wxString & ) = 0;
	virtual bool IsCancelled() = 0;
	
	// override Error and Warn to enable real-time
	// update of simulation messages elsewhere
	virtual void Error( const wxString &s ) { m_savedErrors.Add( s ); }
	virtual void Warn( const wxString &s ) { m_savedWarnings.Add( s ); }
	virtual void Notice( const wxString &s ) { m_savedNotices.Add( s ); }

	// handle saved messages
	virtual wxArrayString GetErrors() { return m_savedErrors; }
	virtual wxArrayString GetWarnings() { return m_savedWarnings; }
	virtual wxArrayString GetNotices() { return m_savedNotices; }
	virtual void ClearSavedMessages() { m_savedErrors.clear(); m_savedWarnings.clear(); }
	
	// optionally output a debug file before the SSC module is run
	virtual bool WriteDebugFile( 
		const wxString &, 
		ssc_module_t, 
		ssc_data_t ) { return false; }
};

class wxThreadProgressDialog;
class SimulationDialog;

class Simulation
{
public:
	Simulation( Case *cc, const wxString &name );
	
	void Write( wxOutputStream & );
	bool Read( wxInputStream & );
	void Copy( const Simulation & );

	void Clear();

	static bool WriteDebugFile( const wxString &file, ssc_module_t p_mod, ssc_data_t p_data );
	static bool WriteDebugFile( const wxString &file, ssc_data_t p_data );

	// setting up the simulation
	void Override( const wxString &name, const VarValue &val );
	wxString GetOverridesLabel( bool with_labels = true );
	void SetName( const wxString &s ) { m_name = s; }
	wxString GetName() { return m_name; }
	VarValue *GetInput( const wxString &name );

	// generate code
	bool Generate_lk(FILE *fp);


	Case *GetCase() { return m_case; }
	
	virtual wxArrayString ListOutputs();
	virtual VarValue *GetOutput( const wxString &var );
	virtual wxString GetLabel( const wxString &var );
	virtual wxString GetUnits( const wxString &var );
	virtual StringHash GetUIHints(const wxString &var);
	virtual VarTable &Outputs();

	// returns an output or input, outputs have precedence
	VarValue *GetValue( const wxString &name );

	VarTable *GetInputVarTable() { return &m_inputs; }
	
	void ListByCount( size_t nr, size_t nc, wxArrayString &list );
	void GetVariableLengths( std::vector<ArraySize> & sizes );

	// prepares and runs simulation
	bool Invoke(bool silent=false, bool prepare=true, wxString folder=wxEmptyString);
	
	bool Prepare(); // not threadable, but must be called before below
	bool InvokeWithHandler(ISimulationHandler *ih, wxString folder = wxEmptyString); // updates elapsed time

	// results and messages if it succeeded
	bool Ok();
	wxArrayString &GetErrors();
	void SetErrors(wxArrayString &_errors);
	wxArrayString &GetWarnings();
	wxArrayString &GetNotices();
	wxArrayString GetAllMessages();
		
	static bool ListAllOutputs( ConfigInfo *cfg, 
		wxArrayString *names, 
		wxArrayString *labels, 
		wxArrayString *units, 
		wxArrayString *groups, 
		bool single_values = false );

	static int DispatchThreads( wxThreadProgressDialog &tpd, 
		std::vector<Simulation*> &sims, 
		int nthread );
	static int DispatchThreads( SimulationDialog &tpd, 
		std::vector<Simulation*> &sims, 
		int nthread );

	// total time for creating data container, model, setting inputs, running simulation
	int GetTotalElapsedTime() { return m_totalElapsedMsec; }
	// SSC compute module execution time only
	int GetSSCElapsedTime() { return m_sscElapsedMsec; }

	wxArrayString GetModels() { return m_simlist; }

protected:
	Case *m_case;
	wxArrayString m_simlist;
	wxString m_name;
	wxArrayString m_overrides;
	VarTable m_inputs;
	wxArrayString m_outputList;
	VarTable m_outputs;
	wxArrayString m_errors, m_warnings, m_notices;
	StringHash m_outputLabels, m_outputUnits, m_uiHints;
	int m_sscElapsedMsec;
	int m_totalElapsedMsec;
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
		
	wxThreadProgressDialog &Dialog() { return *m_tpd; }
	
private:
	wxThreadProgressDialog *m_tpd;
	wxFrame *m_transp;
};
#endif
