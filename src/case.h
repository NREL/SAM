/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#ifndef __case_h
#define __case_h

#include <vector>

#include "object.h"
#include "variables.h"
#include "equations.h"
#include "simulation.h"
#include "excelexch.h"
#include "codegenerator.h"
#include "parametric.h"
#include "stochastic.h"
#include "graph.h"
#include "uncertainties.h"
#include "pvuncertainty.h"

// case events allow the user interface to be updated
// when something internal in the case changes that needs to be reflected
// to the user, i.e. variables are recalculated...

class Case;
class ConfigInfo;

class CaseCallbackContext
{
	Case *m_case;
	wxString m_name;
public:
	CaseCallbackContext( Case *cc, const wxString &name );

	void SetCase( Case *cc, const wxString &name );

	VarTable &GetValues(size_t i);
	Case &GetCase();
	wxString GetName();

	bool Invoke( lk::node_t *root, lk::env_t *parent, size_t i );

protected:
	virtual void SetupLibraries( lk::env_t *env );
};

class CaseEvent
{
private:
	int m_type;
	wxArrayString m_vars;
	wxString m_str, m_str2;
    size_t m_ndxHybrid;
public:
	enum { VARS_CHANGED, CONFIG_CHANGED, VALUE_USER_INPUT, SAVE_NOTIFICATION };

    CaseEvent( int type ) : m_type(type) { }
    CaseEvent( int type, const size_t& ndxHybrid ) : m_type(type), m_ndxHybrid(ndxHybrid) { }
	CaseEvent(int type, const wxString& str) : m_type(type), m_str(str) { }
	CaseEvent(int type, const wxString& str, const size_t& ndxHybrid) : m_type(type), m_str(str), m_ndxHybrid(ndxHybrid) { }
	CaseEvent( int type, const wxString &str1, const wxString &str2 ) : m_type(type), m_str(str1), m_str2(str2) { }
	CaseEvent(int type, const wxArrayString& vars) : m_type(type), m_vars(vars) { }
	CaseEvent(int type, const wxArrayString& vars, const size_t& ndxHybrid) : m_type(type), m_vars(vars), m_ndxHybrid(ndxHybrid) { }

	int GetType() { return m_type; }
	wxString GetString() { return m_str; }
	wxString GetString2() { return m_str2; }
	wxArrayString &GetVars() { return m_vars; }
    size_t GetndxHybrid() { return m_ndxHybrid; }
};

class CaseEventListener
{
public:
	virtual void OnCaseEvent( Case *, CaseEvent & ) = 0;
};

class Case : public Object
{
public:
	Case();
	virtual ~Case();

	virtual Object *Duplicate();
	virtual bool Copy( Object *obj );
	virtual wxString GetTypeName();
	virtual void Write( wxOutputStream & );
	virtual bool Read( wxInputStream & );

	struct LoadStatus
	{
		LoadStatus() { nread = 0; }
		wxArrayString wrong_type;
		wxArrayString not_found;// _in_sam_configuration;
		wxArrayString not_found_in_external_source;
		size_t nread;
		wxString error;
	};

	bool LoadValuesFromExternalSource(const VarTable& vt, size_t ndxHybrid, LoadStatus* di = 0, VarTable* invalids = 0);
	bool LoadValuesFromExternalSource( wxInputStream &in, size_t ndxHybrid, LoadStatus *di = 0, VarTable *invalids = 0, bool binary = true );
	bool VarTableFromInputStream(VarTable* vt, wxInputStream& in, bool binary);
	bool VarTableFromJSONFile(VarTable* vt, const std::string& file);
//	bool VarTablesFromJSONFile(std::vector<VarTable>& vt, const std::string& file);

	bool LoadDefaults( wxString *error_msg = 0 );
	bool SaveDefaults(bool quiet = false);
	bool SaveAsJSON(bool quiet, wxString fn, wxString case_name);
	//bool LoadFromJSON(wxString fn, wxString* error_msg = 0); // Loads JSON file from SaveAsJSON (defaults)
	bool SaveAsSSCJSON(wxString fn); // like code generator but uses RapidJSON for compatibility with tables
	//bool LoadFromSSCJSON(wxString fn, wxString* error_msg = 0); // Loads JSON file generated from Ctrl+F7
	bool PreRunSSCJSON(const wxString& tech, const wxString& fin, const wxString& fn, wxString* error_msg = 0); // Loads JSON file generated from Ctrl+F7

	bool SetConfiguration( const wxString &tech, const wxString &fin, bool silent=false, wxString *message = 0 );
	void GetConfiguration( wxString *tech, wxString *fin );	
	ConfigInfo *GetConfiguration() { return m_config; }
	VarTable &Values(size_t i) { return m_vals[i]; } // check index
	VarTable &OldValues(size_t i) { return m_oldVals[i]; } // check index
	VarInfoLookup &Variables(size_t i);
	EqnFastLookup &Equations(size_t i);
	wxString GetTechnology() const;
	wxString GetFinancing() const;
	lk::env_t &CallbackEnvironment(size_t i_vt);
	lk::node_t *QueryCallback( const wxString &method, const wxString &object, size_t i_vt );
	
	// call this method when a variable is programmatically changed,
	// i.e. through a script callback, SamUL script, or other
	// indirect way of changing a variable.  causes any affected
	// variables to be recalculated, and updates any views
	void VariableChanged( const wxString &name, size_t ndxHybrid);
	void VariablesChanged( const wxArrayString &list, size_t ndxHybrid);

	// recalculate any variables that are impacted by a changed value of 'trigger'
	// any views are updated with the variables that are consequently updated with 
	// new values as a result of the calculations
	// Note: if the 'trigger' variable is a library item (has the VF_LIBRARY flag)
	// this will also apply all the library values and cause any affected variables
	// to be subsequently updated
	int Recalculate( const wxString &trigger, size_t ndxHybrid ); 
	int Recalculate( const wxArrayString &triggers, size_t ndxHybrid);

	// recalculate all equations in this case
	// CaseEvent is issued for all updated variables
	// returns negative on error, or positive number indicating
	// success of number of variables update.  shows an error
	// message box unless 'quietly' = true
	int RecalculateAll(size_t ndxHybrid, bool quietly );

	// evaluates all equations in a case
	int EvaluateEquations();
	
	StringHash &Properties() { return m_properties; }
	wxString GetProperty( const wxString &id );
	void SetProperty( const wxString &id, const wxString &value );
	StringHash &Notes() { return m_notes; }
	wxString RetrieveNote( const wxString &id );
	void SaveNote( const wxString &id, const wxString &text );

	void AddListener( CaseEventListener *cel );
	void RemoveListener( CaseEventListener *cel );
	void ClearListeners();
	void SendEvent( CaseEvent e );

	Simulation &BaseCase();

	ExcelExchange &ExcelExch() { return m_excelExch; }
	ParametricData &Parametric() { return m_parametric; }
	StochasticData& Stochastic() { return m_stochastic; }
	PVUncertaintyData& PVUncertainty() { return m_pvuncertainty; }
	void SetGraphs(std::vector<Graph> &gl) { m_graphs = gl; }
	void GetGraphs(std::vector<Graph> &gl) { gl = m_graphs; }
	void SetUncertainties(std::vector<Uncertainties> &ul) { m_uncertainties = ul; }
	void GetUncertainties(std::vector<Uncertainties> &ul) { ul = m_uncertainties; }
	StringHash &Perspective() { return m_perspective; }

	wxString GetLastError() { return m_lastError; };

	// handle analysis period changes
	size_t m_analysis_period;
	size_t m_analysis_period_old;

private:
	wxString m_lastError;
	std::vector<CaseEventListener*> m_listeners;

	ConfigInfo *m_config;
	
	// current variable values
	std::vector<VarTable> m_vals;
	// variables that were read in from an old project file made
	// with a previous version of SAM that either don't exist in
	// in the current version configuration, or are of the wrong data
	// type
	std::vector<VarTable> m_oldVals;
	
	std::vector<lk::env_t> m_cbEnv;
	Simulation m_baseCase;
	StringHash m_properties;
	StringHash m_notes;
	ExcelExchange m_excelExch;
	ParametricData m_parametric;
	StochasticData m_stochastic;
	PVUncertaintyData m_pvuncertainty;
	std::vector<Graph> m_graphs;
	std::vector<Uncertainties> m_uncertainties;
	StringHash m_perspective;

};

class CaseEvaluator : public EqnEvaluator
{
	Case *m_case;
	VarTable *m_vt;
public:	
	CaseEvaluator( Case *cc, VarTable &vars, EqnFastLookup &efl );
	virtual void SetupEnvironment( lk::env_t &env );	
	virtual int CalculateAll(size_t ndxHybrid);
	virtual int Changed( const wxArrayString &vars, size_t ndxHybrid );
	virtual int Changed( const wxString &trigger, size_t ndxHybrid );

	bool UpdateLibrary( const wxString &trigger, wxArrayString &changed, size_t ndxHybrid );
};



#endif
