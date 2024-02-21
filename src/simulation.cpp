
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

#include <algorithm>
#include <iostream>
#include <fstream>


#include <wx/datstrm.h>
#include <wx/gauge.h>
#include <wx/progdlg.h>
#include <wx/thread.h>
#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/file.h>
#include <wx/ffile.h>

#include <wx/filedlg.h>
#include <wx/filefn.h>

#include <wex/metro.h>
#include <wex/utils.h>

#include <lk/absyn.h>
#include <lk/stdlib.h>
#include <lk/eval.h>

#include <ssc/sscapi.h>

#include "simulation.h"
#include "main.h"
#include "equations.h"
#include "case.h"


#include "codegenerator.h" // write out ssc inputs for generating tests from SAM simulations

bool VarValueToSSC( VarValue *vv, ssc_data_t pdata, const wxString &sscname, bool match_case )
{
    auto var = ssc_var_create();
    if (!vv->AsSSCVar(var)){
        ssc_var_free(var);
        return false;
    }
	if (match_case)
		ssc_data_set_var_match_case(pdata, sscname.c_str(), var);
	else
		ssc_data_set_var(pdata, sscname.c_str(), var);
    ssc_var_free(var);
    return true;
}

Simulation::Simulation( Case *cc, const wxString &name )
	: m_case( cc ), m_name( name )
{
	m_totalElapsedMsec = 0;
	m_sscElapsedMsec = 0;
    m_bSscTestsGeneration = false;
}


static void write_array_string( wxDataOutputStream &out, wxArrayString &list )
{
	out.Write32( list.size() );
	for( size_t i=0;i<list.size();i++ )
		out.WriteString( list[i] );
}

static void read_array_string( wxDataInputStream &in, wxArrayString &list )
{
	list.Clear();
	size_t n = in.Read32();
	for( size_t i=0;i<n;i++ )
		list.Add( in.ReadString() );
}

void Simulation::Write( wxOutputStream &os )
{
	size_t i;
	wxDataOutputStream out( os );
	out.Write8( 0x9c );
	out.Write8( 4 ); // version - update to 4 for hybrids m_inputs and m_overrides

	out.WriteString( m_name );

	out.Write8(m_overrides.size());
	for (i = 0; i < m_overrides.size(); i++)
		write_array_string( out, m_overrides[i]);

	out.Write8(m_inputs.size());
	for (i=0; i < m_inputs.size(); i++)
		m_inputs[i].Write(os);

	m_outputs.Write( os, SamApp::Project().GetSaveHourlyData() ? 0 : 1024 );
	
	write_array_string( out, m_errors );
	write_array_string( out, m_warnings );
	write_array_string( out, m_notices );

	m_outputLabels.Write( os );
	m_outputUnits.Write( os );
	m_uiHints.Write(os);

	out.Write8( 0x9c );
}

bool Simulation::Read(wxInputStream& is)
{
	size_t i;
	Clear();
	wxDataInputStream in(is);

	wxUint8 code = in.Read8(); // code
	wxUint8 ver = in.Read8(); // ver

	m_name = in.ReadString();

	if (ver > 3) {
		size_t n = in.Read8();
		m_overrides.resize(n);
		for (i = 0; i < m_overrides.size(); i++)
			read_array_string(in, m_overrides[i]);
		n = in.Read8();
		m_inputs.resize(n);
		for (i = 0; i < m_inputs.size(); i++)
			m_inputs[i].Read(is);
	}
	else {
		m_overrides.resize(1);
		read_array_string(in, m_overrides[0]);
		m_inputs.resize(1);
		m_inputs[0].Read(is);
	}
	m_outputs.Read(is);

	read_array_string( in, m_errors );
	read_array_string( in, m_warnings );
	if ( ver > 1 ) read_array_string( in, m_notices );
		
	m_outputLabels.Read( is );
	m_outputUnits.Read( is );
	
	if (ver > 2)
		m_uiHints.Read( is );
	
	return ( code == in.Read8() );	
}

void Simulation::Copy( const Simulation &rh )
{
	Clear();
	m_name = rh.m_name;
	m_overrides = rh.m_overrides;
	m_inputs = rh.m_inputs;
	m_outputs = rh.m_outputs;
	m_errors = rh.m_errors;
	m_warnings = rh.m_warnings;
	m_notices = rh.m_notices;
	m_outputLabels = rh.m_outputLabels;
	m_outputUnits = rh.m_outputUnits;
	m_uiHints = rh.m_uiHints;
}

void Simulation::Clear()
{
	size_t i;
	for (i = 0; i < m_overrides.size(); i++)
		m_overrides[i].clear();
	m_overrides.clear();
	for (i = 0; i < m_inputs.size(); i++)
		m_inputs[i].clear();
	m_inputs.clear();
	m_outputList.clear();
	m_outputs.clear();
	m_errors.clear();
	m_warnings.clear();
	m_notices.clear();
	m_outputLabels.clear();
	m_outputUnits.clear();
	m_uiHints.clear();
	
	Setup(); // resize after clearing
}

void Simulation::Override( const wxString &name, const VarValue &val, size_t ndxHybrid)
{
	if ( VarValue *vv = m_inputs[ndxHybrid].Create(name, val.Type()))
	{
		m_overrides[ndxHybrid].Add(name);
		vv->Copy( val );
	}
}


wxString Simulation::GetOverridesLabel(size_t ndxHybrid, bool with_labels )
{
	wxString tag;
	for( size_t i=0;i<m_overrides[ndxHybrid].size(); i++)
	{
		if ( VarValue *vv = m_inputs[ndxHybrid].Get(m_overrides[ndxHybrid][i]))
		{
			wxString label = m_overrides[ndxHybrid][i];
			
			if ( with_labels )
				if ( VarInfo *vi = m_case->Variables(ndxHybrid).Lookup( m_overrides[ndxHybrid][i] ) )
					if ( !vi->Label.IsEmpty() )
						label = vi->Label;
			
			tag += label + "=" + vv->AsString();
			if ( i < m_overrides.size()-1 )
				tag += ";";
		}
	}

	return tag;
}

VarValue *Simulation::GetInput( const wxString &name, size_t ndxHybrid)
{
	if ( VarValue *val = m_inputs[ndxHybrid].Get(name))
		return val;
	if (VarValue* val = m_case->Values(ndxHybrid).Get(name))
		return val;

	return NULL;
}

void Simulation::SetInput(const wxString & , lk::vardata_t) {
	//if (VarValue *vv = m_inputs.Get(name)) {
	//	if (vv->Type == VV_NUMBER && val.type == 3) {

	//	}
	//	else if (vv->Type == VV_STRING && val.type == 4) {

	//	}
	//	else if (vv->Type == VV_ARRAY && val.type == 5) {

	//	}
	//}
}

bool Simulation::Ok()
{
	return m_errors.size() == 0;
}

wxArrayString &Simulation::GetErrors()
{
	return m_errors;
}

void Simulation::SetErrors(wxArrayString &_errors)
{
	m_errors = _errors;
}


wxArrayString &Simulation::GetWarnings()
{
	return m_warnings;
}

wxArrayString &Simulation::GetNotices()
{
	return m_notices;
}

VarTable &Simulation::Outputs()
{
	return m_outputs;
}

wxArrayString Simulation::ListOutputs()
{
	return m_outputList;
}

VarValue *Simulation::GetOutput( const wxString &var )
{
	return Outputs().Get( var );
}

VarValue *Simulation::GetValue( const wxString &name )
{
	if ( VarValue *vv = Outputs().Get( name ) ) 
		return vv;
	else {
		// search from last to first vartable for hybrids i=ndxHybrid and name is variable name in vorrect vartable - may report incorrect value for same names in different vartables
		bool found = false;
		for (int i = m_inputs.size() - 1; i>=0 && !found; i--) {
			if (vv = GetInput(name, i))
				found = true;
		}
		if (!found) {
			for (int i = m_case->GetConfiguration()->Technology.size() - 1; i >= 0 && !found; i--) {
				if (vv = m_case->Values(i).Get(name))
					found = true;
			}
		}
		return vv;
	}
}

wxString Simulation::GetLabel( const wxString &var )
{
	if ( m_outputLabels.find( var ) != m_outputLabels.end() )
		return m_outputLabels[ var ];
	else {
		bool found = false;
		wxString label = wxEmptyString;
		for (int i = m_inputs.size() - 1; i >= 0 && !found; i--) {
			label = m_case->Variables(i).Label(var);
			found = (label.Left(11) != "<not found:");
		}
		return label;
	}
}

wxString Simulation::GetUnits( const wxString &var )
{
	if ( m_outputUnits.find( var ) != m_outputUnits.end() )
		return m_outputUnits[ var ];
	else {
		bool found = false;
		wxString units = wxEmptyString;
		for (int i = m_inputs.size() - 1; i >= 0 && !found; i--) {
			units = m_case->Variables(i).Units(var);
			found = (units != wxEmptyString);
		}
		return units;
	}

}
StringHash Simulation::GetUIHints(const wxString &var)
{
	StringHash tmp;
	if (m_uiHints.find(var) != m_uiHints.end())
	{
		wxString value = m_uiHints[var];
		value.UpperCase();
		tmp.Split(value, ',', '=');
	}
	return tmp;
	
}


class SingleThreadHandler : public ISimulationHandler
{
	wxProgressDialog *progdlg;
	wxString save_folder;
public:
	SingleThreadHandler() {
		progdlg = 0;
		save_folder = wxGetHomeDir();
	};

	void SetProgressDialog( wxProgressDialog *d ) { progdlg = d; }

	virtual void Update( float percent, const wxString &s ) {
		if( progdlg) progdlg->Update( (int)percent, s );
	}
	virtual bool IsCancelled() {
		if ( progdlg) return progdlg->WasCancelled();
		else return false;
	}

	virtual bool WriteDebugFile( const wxString &sim, ssc_module_t p_mod, ssc_data_t p_data )
	{
		return Simulation::WriteDebugFile(sim, p_mod, p_data);
	}

};


class SingleThreadHandlerWithDebugOutput : public ISimulationHandler
{
	wxProgressDialog* progdlg;
	wxString save_folder;
public:
	SingleThreadHandlerWithDebugOutput() {
		progdlg = 0;
		save_folder = wxGetHomeDir();
	};

	void SetProgressDialog(wxProgressDialog* d) { progdlg = d; }

	virtual void Update(float percent, const wxString& s) {
		if (progdlg) progdlg->Update((int)percent, s);
	}
	virtual bool IsCancelled() {
		if (progdlg) return progdlg->WasCancelled();
		else return false;
	}

	virtual bool WriteDebugFile(const wxString& sim, ssc_module_t p_mod, ssc_data_t p_data)
	{
		// folder prompting
		wxString fn = "ssc-" + sim + ".lk";
		wxFileDialog dlg(SamApp::Window(), "Save inputs as...",
			save_folder,
			fn,
			"SAM Script Files (*.lk)|*.lk", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
		if (dlg.ShowModal() == wxID_OK)
		{
			save_folder = wxPathOnly(dlg.GetPath());
			return Simulation::WriteDebugFile(dlg.GetPath(), p_mod, p_data);
		}
		else
			return false;
	}

};


static ssc_bool_t ssc_invoke_handler( ssc_module_t , ssc_handler_t ,
	int action_type, float f0, float , 
	const char *s0, const char *,
	void *user_data )
{
	ISimulationHandler *hh = (ISimulationHandler*) user_data;
	if (!hh) return 0;

	if (action_type == SSC_LOG)
	{		
		switch( (int)f0 )
		{
		case SSC_NOTICE:
			hh->Notice( s0 );
			break;
		case SSC_WARNING:
			hh->Warn( s0 );
			break;
		case SSC_ERROR:
			hh->Error( s0 );
			break;
		}
		
		return hh->IsCancelled() ? 0 : 1;		
	}
	else if (action_type == SSC_UPDATE)
	{
		hh->Update( f0, s0 );
		return hh->IsCancelled() ? 0 : 1;
	}
	else
		return 0;
}

bool Simulation::InvokeSSC(bool silent, const wxString& fn)
{
	SingleThreadHandler sc;

	wxProgressDialog* prog = 0;

	ConfigInfo* cfg = m_case->GetConfiguration();
	if (!cfg)
	{
		m_errors.Add("no valid configuration for this case");
		return false;
	}

	m_simlist = cfg->Simulations;

	if (!silent)
	{
		prog = new wxProgressDialog("Simulation", "in progress", 100,
			SamApp::GetMainTopWindow(),
			  wxPD_SMOOTH | wxPD_AUTO_HIDE );
		prog->Show();

		sc.SetProgressDialog(prog);
	}

	// Warning - be careful here if threading!
	std::ifstream test(fn.ToStdString().c_str());
	std::string json_str((std::istreambuf_iterator<char>(test)), std::istreambuf_iterator<char>());
	auto p_data = json_to_ssc_data(json_str.c_str());

	bool ok = InvokeSSCWithHandler(&sc, p_data);
	if (!ok) {
		wxMessageBox("ssc simulation failed " + wxJoin(m_errors, '\n'));
	}

	ssc_data_free(p_data);

	if (prog) prog->Destroy();

	return ok;
}




bool Simulation::Invoke( bool silent, bool prepare, wxString folder )
{
	SingleThreadHandler sc;
//	SingleThreadHandlerWithDebugOutput sc;
	wxProgressDialog *prog = 0;
	if (!folder.IsEmpty())
	{
		// set folder before progress dialog to prevent hiding
	}
	if ( !silent )
	{
		prog = new wxProgressDialog("SAM Simulation", "Simulation running...", 100,
			SamApp::CurrentActiveWindow(),  // progress dialog parent is current active window - works better when invoked scripting
			wxPD_APP_MODAL | wxPD_SMOOTH | wxPD_CAN_ABORT | wxPD_AUTO_HIDE );
		prog->Show();

		sc.SetProgressDialog( prog );
	}

	if ( prepare && !Prepare() )
		return false;
	
	bool ok =  InvokeWithHandler( &sc, folder );

	if ( prog ) prog->Destroy();
	
	return ok;
}

bool Simulation::Setup()
{
	ConfigInfo* cfg = m_case->GetConfiguration();
	if (!cfg)
	{
		m_errors.Add("no valid configuration for this case");
		return false;
	}

	// resize vectors
	size_t nHybrids = cfg->Technology.size();
	m_inputs.resize(nHybrids);
	m_overrides.resize(nHybrids);

	return true;
}

bool Simulation::Prepare()
{	
	ConfigInfo *cfg = m_case->GetConfiguration();
	if ( !cfg )
	{
		m_errors.Add("no valid configuration for this case");
		return false;
	}

	m_simlist = cfg->Simulations;

	m_outputList.clear();
	m_outputLabels.clear();
	m_outputUnits.clear();
	m_uiHints.clear();

	size_t nHybrids = cfg->Technology.size();

	for (size_t ndx_hybrid = 0; ndx_hybrid < nHybrids; ndx_hybrid++) {

		// transfer all the values except for ones that have been 'overriden'
		for (VarTableBase::const_iterator it = m_case->Values(ndx_hybrid).begin();
			it != m_case->Values(ndx_hybrid).end();
			++it)
			if (0 == m_inputs[ndx_hybrid].Get(it->first))
				m_inputs[ndx_hybrid].Set(it->first, *(it->second));

		// recalculate all the equations
		CaseEvaluator eval(m_case, m_inputs[ndx_hybrid], m_case->Equations(ndx_hybrid));// update m_inputs for hybrids
		int n = eval.CalculateAll(ndx_hybrid);

		if (n < 0)
		{
			wxArrayString& errs = eval.GetErrors();
			for (size_t i = 0; i < errs.size(); i++)
				m_errors.Add(errs[i]);

			return false;
		}

		//wxLogStatus("Simulation preparation time: %d copy, %d eval", (int)time_copy, (int)time_eval);
	}
	return true;
}

static void dump_variable( FILE *fp, ssc_data_t p_data, const char *name )
{ // .17g to .17g for full double precision.
	ssc_number_t value;
	ssc_number_t *p;
	int len, nr, nc;
	wxString str_value;
	double dbl_value;
	int type = ::ssc_data_query( p_data, name );
	switch( type )
	{
	case SSC_STRING:
		str_value = wxString::FromUTF8(::ssc_data_get_string( p_data, name ));
		str_value.Replace("\\", "/" );
		fprintf(fp, "var( '%s', '%s' );\n", name, (const char*)str_value.c_str() );
		break;
	case SSC_NUMBER:
		::ssc_data_get_number( p_data, name, &value );
		dbl_value = (double)value;
		if ( dbl_value > 1e38 ) dbl_value = 1e38;
		fprintf(fp, "var( '%s', %.17g );\n", name, dbl_value );
		break;
	case SSC_ARRAY:
		p = ::ssc_data_get_array( p_data, name, &len );
		fprintf(fp, "var( '%s', [", name);
		for ( int i=0;i<(len-1);i++ )
		{
			dbl_value = (double)p[i];
			if ( dbl_value > 1e38 ) dbl_value = 1e38;
			fprintf(fp, " %.17g,", dbl_value );
		}
		dbl_value = (double)p[len-1];
		if ( dbl_value > 1e38 ) dbl_value = 1e38;
		fprintf(fp, " %.17g ] );\n", dbl_value );
		break;
	case SSC_MATRIX:
		p = ::ssc_data_get_matrix( p_data, name, &nr, &nc );
		len = nr*nc;
		fprintf( fp, "var( '%s', \n[ [", name );					
		for (int k=0;k<(len-1);k++)
		{
			dbl_value = (double)p[k];
			if ( dbl_value > 1e38 ) dbl_value = 1e38;
			if ( (k+1)%nc == 0 ) 
				fprintf(fp, " %.17g ], \n[", dbl_value);
			else
				fprintf(fp, " %.17g,", dbl_value);
		}
		dbl_value = (double)p[len-1];
		if ( dbl_value > 1e38 ) dbl_value = 1e38;
		fprintf(fp, " %.17g ] ] );\n", dbl_value);
	}
}

bool Simulation::WriteDebugFile( const wxString &file, ssc_module_t p_mod, ssc_data_t p_data )
{
	if( FILE *fp = fopen( file.c_str(), "w" ) )
	{
		fprintf(fp, "clear();\n");
		int dbgidx = 0;
		while( const ssc_info_t p_inf = ssc_module_var_info( p_mod, dbgidx++ ) )
		{
			const char *name = ::ssc_info_name( p_inf );
			dump_variable( fp, p_data, name );
		}
		wxString name = wxFileName(file).GetName();
		name = name.Right(name.length() - 4); // skip "ssc-"
		fprintf(fp, "run('%s');\n", (const char *)name.c_str());
		fclose(fp);
		return true;
	}
	else
		return false;
}

bool Simulation::WriteDebugFile( const wxString &file, ssc_data_t p_data )
{
	if( FILE *fp = fopen( file.c_str(), "w" ) )
	{
		const char *name = ssc_data_first( p_data );
		while( name )
		{
			dump_variable( fp, p_data, name );
			name = ssc_data_next( p_data );
		}
		fclose( fp );
		return true;
	}
	else
		return false;
}


bool Simulation::Generate_lk(FILE *fp)
{
	SingleThreadHandler ih;

	if (!Prepare())
		return false;

	ssc_data_t p_data = ssc_data_create();

	if (m_simlist.size() == 0)
		ih.Error("No simulation compute modules defined for this configuration.");

	for (size_t kk = 0; kk < m_simlist.size(); kk++)
	{
		ssc_module_t p_mod = ssc_module_create(m_simlist[kk].c_str());
		if (!p_mod)
		{
			ih.Error("could not create ssc module: " + m_simlist[kk]);
			continue;
		}

		int pidx = 0;
		while (const ssc_info_t p_inf = ssc_module_var_info(p_mod, pidx++))
		{
			int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
			int data_type = ssc_info_data_type(p_inf); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
			wxString name(ssc_info_name(p_inf)); // assumed to be non-null
			wxString reqd(ssc_info_required(p_inf));

			if (var_type == SSC_INPUT || var_type == SSC_INOUT)
			{
				// handle ssc variable names
				// that are explicit field accesses"shading:mxh"
				wxString field;
				int pos = name.Find(':');
				if (pos != wxNOT_FOUND)
				{
					field = name.Mid(pos + 1);
					name = name.Left(pos);
				}

				int existing_type = ssc_data_query(p_data, ssc_info_name(p_inf));
				if (existing_type != data_type)
				{
					if (VarValue *vv = GetInput(name, 0)) // TODO: hybrid update
					{
						if (!field.IsEmpty())
						{
							if (vv->Type() != VV_TABLE)
								ih.Error("SSC variable has table:field specification, but '" + name + "' is not a table in SAM");
							
							bool do_copy_var = false;
							if (reqd.Left(1) == "?")
							{
								// if the SSC variable is optional, check for the 'en_<field>' element in the table
								if (VarValue *en_flag = vv->Table().Get("en_" + field))
									if (en_flag->Boolean())
										do_copy_var = true;
							}
							else do_copy_var = true;

							if (do_copy_var)
							{
								if (VarValue *vv_field = vv->Table().Get(field))
								{
									if (!VarValueToSSC(vv_field, p_data, name + ":" + field))
										ih.Error("Error translating table:field variable from SAM UI to SSC for '" + name + "':" + field);
								}
							}
							
						}

						if (!VarValueToSSC(vv, p_data, name))
							ih.Error("Error translating data from SAM UI to SSC for " + name);

					}
					else if (reqd == "*")
						ih.Error("SSC requires input '" + name + "', but was not found in the SAM UI or from previous simulations");
				}
			}
		}

		const char *name = ssc_data_first(p_data);
		while (name)
		{
			dump_variable(fp, p_data, name);
			name = ssc_data_next(p_data);
		}
		fprintf(fp, "run('%s');\n", (const char*)m_simlist[kk].c_str());

	}
	return true;
}

bool Simulation::WriteSSCTestInputs(wxString& cmod_name, ssc_module_t p_mod, ssc_data_t p_data) {
	// can filter on compute module name
//    if (cmod_name != "cashloan") return false;
    if (std::find(std::begin(m_asSscTestsComputeModules),std::end(m_asSscTestsComputeModules), cmod_name) == std::end(m_asSscTestsComputeModules))
        return false;
    
	auto cfg = m_case->GetConfiguration();
    wxString casename = SamApp::Project().GetCaseName( m_case );

    
    wxString fn = m_sSscTestsJSONFolder; //SamApp::GetUserLocalDataDir();
	wxString tech = cfg->TechnologyFullName;
	tech.Replace(" ", "_");
	wxString fin = cfg->Financing;
	fin.Replace(" ", "_");
	fn += "/" +  casename + "_" + tech  + "_" + fin + "_" + "cmod_" + cmod_name + ".json";

	auto cg = std::make_shared<CodeGen_json>(m_case, fn);
	cg->Header();

	int pidx = 0;
	while (const ssc_info_t p_inf = ssc_module_var_info(p_mod, pidx++)) {
		int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
		wxString name(ssc_info_name(p_inf)); // assumed to be non-null
//		wxString reqd(ssc_info_required(p_inf)); // optional if want required inputs only

        if (var_type == SSC_INPUT || var_type == SSC_INOUT) { // all SSC_INPUT and SSC_INOUT without checking required
            if (!cg->Input(p_data, name.c_str(), "", 0)) {
                wxString err = "SSC requires input '" + name +
                    "', but was not found in the SAM UI or from previous simulations";
                ssc_data_set_string(p_data, "error", err.c_str());
                return false;
            }
		}
	}
	cg->Footer();
	return true;
}


bool Simulation::WriteSSCTestOutputs(wxString& cmod_name, ssc_module_t p_mod, ssc_data_t p_data) {
    // can filter on compute module name
//    if (cmod_name != "cashloan") return false;
    if (std::find(std::begin(m_asSscTestsComputeModules),std::end(m_asSscTestsComputeModules), cmod_name) == std::end(m_asSscTestsComputeModules))
        return false;

    auto cfg = m_case->GetConfiguration();
    wxString casename = SamApp::Project().GetCaseName( m_case );

  	wxString fn = m_sSscTestsJSONFolder; //SamApp::GetUserLocalDataDir();
	wxString tech = cfg->TechnologyFullName;
	tech.Replace(" ", "_");
	wxString fin = cfg->Financing;
	fin.Replace(" ", "_");
	fn += "/" + casename + "_" + tech + "_" + fin + "_" + "cmod_" + cmod_name + "_outputs.json";

    auto cg = std::make_shared<CodeGen_json>(m_case, fn);
    cg->Header();

    int pidx = 0;
    while (const ssc_info_t p_inf = ssc_module_var_info(p_mod, pidx++)) {
        int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
        wxString name(ssc_info_name(p_inf)); // assumed to be non-null
//        wxString reqd(ssc_info_required(p_inf)); // optional if want required inputs only

        if (var_type == SSC_OUTPUT || var_type == SSC_INOUT) { // all SSC_OUTPUT and SSC_INOUT without checking required
            if (!cg->Input(p_data, name.c_str(), "", 0)) {
 //            if (!cg->Output(p_data)) {
                wxString err = "SSC requires output '" + name +
                    "', but was not found in the SAM UI or from previous simulations";
                ssc_data_set_string(p_data, "error", err.c_str());
                return false;
            }
        }
    }
    cg->Footer();
    return true;
}

bool Simulation::JSONInputsToSSCData(wxString& fn, ssc_data_t p_data) {
	bool ret = false;
	std::ifstream test(fn.ToStdString().c_str());
	std::string json_str((std::istreambuf_iterator<char>(test)), std::istreambuf_iterator<char>());
	p_data = json_to_ssc_data(json_str.c_str());
	ret = true;
	return ret;
}



bool Simulation::CmodInputsToSSCData(ssc_module_t p_mod, ssc_data_t p_data) {
    int pidx = 0;
    while (const ssc_info_t p_inf = ssc_module_var_info(p_mod, pidx++)) {
        int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
        int data_type = ssc_info_data_type(p_inf); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX
        wxString name(ssc_info_name(p_inf)); // assumed to be non-null
        wxString reqd(ssc_info_required(p_inf));

        if (var_type == SSC_INPUT || var_type == SSC_INOUT) {
            // handle ssc variable names
            // that are explicit field accesses"shading:mxh"
            wxString field;
            int pos = name.Find(':');
            if (pos != wxNOT_FOUND) {
                field = name.Mid(pos + 1);
                name = name.Left(pos);
				wxLogStatus("Table value, table %s, field %s", name.c_str(), field.c_str());
            }

            int existing_type = ssc_data_query(p_data, ssc_info_name(p_inf));
            if (existing_type != data_type) {
                if (VarValue *vv = GetInput(name, 0)) { // TODO:hybrids
                    if (!field.IsEmpty()) {
                        if (vv->Type() != VV_TABLE) {
                            wxString err = "SSC variable has table:field specification, but '" + name +
                                           "' is not a table in SAM";
                            ssc_data_set_string(p_data, "error", err.c_str());
                            return false;
                        }

                        bool do_copy_var = false;
                        if (reqd.Left(1) == "?") {
                            // if the SSC variable is optional, check for the 'en_<field>' element in the table
                            if (VarValue *en_flag = vv->Table().Get("en_" + field))
                                if (en_flag->Boolean())
                                    do_copy_var = true;
                        } else do_copy_var = true;

                        if (do_copy_var) {
                            if (VarValue *vv_field = vv->Table().Get(field)) {
                                if (!VarValueToSSC(vv_field, p_data, name + ":" + field)) {
                                    wxString err =
                                            "Error translating table:field variable from SAM UI to SSC for '" +
                                            name + "':" + field;
                                    ssc_data_set_string(p_data, "error", err.c_str());
                                    return false;
                                }
                            }
                        }

                    }

                    if (!VarValueToSSC(vv, p_data, name)) {
                        wxString err = "Error translating data from SAM UI to SSC for " + name;
                        ssc_data_set_string(p_data, "error", err.c_str());
                        return false;
                    }
                } else if (reqd == "*") {
                    wxString err = "SSC requires input '" + name +
                                   "', but was not found in the SAM UI or from previous simulations";
                    ssc_data_set_string(p_data, "error", err.c_str());
                    return false;
                }
            }
        }
    }
    return true;
}

bool Simulation::SetModels()
{
	ConfigInfo* cfg = m_case->GetConfiguration();
	if (!cfg)
	{
		m_errors.Add("no valid configuration for this case");
		return false;
	}

	m_simlist = cfg->Simulations;
	return true;
}

bool Simulation::InvokeSSCWithHandler(ISimulationHandler* ih, ssc_data_t p_data)
{
	assert(0 != ih);

	m_totalElapsedMsec = 0;
	m_sscElapsedMsec = 0;
	wxStopWatch sw;


	if (m_simlist.size() == 0)
		ih->Error("No simulation compute modules defined for this configuration.");

//	// Warning - be careful here if threading!
//	std::ifstream test(fn.ToStdString().c_str());
//	std::string json_str((std::istreambuf_iterator<char>(test)), std::istreambuf_iterator<char>());
//	auto p_data = json_to_ssc_data(json_str.c_str());

//	if (!JSONInputsToSSCData(fn, p_data)) {
//		ih->Error(ssc_data_get_string(p_data, "error"));
//	}

	for (size_t kk = 0; kk < m_simlist.size(); kk++)
	{
		ssc_module_t p_mod = ssc_module_create(m_simlist[kk].c_str());
		if (!p_mod) {
			wxString err = "could not create ssc module: " + m_simlist[kk];
			ssc_data_set_string(p_data, "error", err.c_str());
			return false;
		}

//		if (m_bSscTestsGeneration)
//			WriteSSCTestInputs(m_simlist[kk], p_mod, p_data);

		// optionally write a debug input file if the ISimulationHandler defines it
//		wxString fn = folder + "/ssc-" + m_simlist[kk] + ".lk";
//		ih->WriteDebugFile(fn, p_mod, p_data);
		//ih->WriteDebugFile( m_simlist[kk], p_mod, p_data );

		wxStopWatch ssctime;
		ssc_bool_t ok = ssc_module_exec_with_handler(p_mod, p_data, ssc_invoke_handler, ih);
		m_sscElapsedMsec += (int)ssctime.Time();

		if (!ok)
		{
			ih->Error("Simulation " + m_simlist[kk] + " failed :" + ssc_module_log(p_mod, 0, nullptr, nullptr));
		}
		else
		{
			int pidx = 0;
			while (const ssc_info_t p_inf = ssc_module_var_info(p_mod, pidx++))
			{
				int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
				int data_type = ssc_info_data_type(p_inf); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
				const char* name = ssc_info_name(p_inf); // assumed to be non-null
				wxString label(ssc_info_label(p_inf));
				wxString units(ssc_info_units(p_inf));
				wxString ui_hint(ssc_info_uihint(p_inf));


				if ((var_type == SSC_OUTPUT || var_type == SSC_INOUT) && data_type == SSC_NUMBER)
				{
					ssc_number_t vval;
					if (ssc_data_get_number(p_data, name, &vval))
					{
						if (m_outputList.Index(name) != wxNOT_FOUND) {
							m_outputList.Remove(name);
						}

						m_outputList.Add(name);
						VarValue* vv = m_outputs.Create(name, VV_NUMBER);
						vv->Set((float)vval);

						m_outputLabels[name] = label;
						m_outputUnits[name] = units;
						if (!ui_hint.IsEmpty()) m_uiHints[name] = ui_hint;
					}
				}
				else if ((var_type == SSC_OUTPUT || var_type == SSC_INOUT) && data_type == SSC_ARRAY)
				{
					int len;
					if (ssc_number_t* varr = ssc_data_get_array(p_data, name, &len))
					{
						if (m_outputList.Index(name) != wxNOT_FOUND) {
							m_outputList.Remove(name);
						}
						m_outputList.Add(name);
						VarValue* vv = m_outputs.Create(name, VV_ARRAY);
						double* ff = new double[len];
						for (int i = 0; i < len; i++)
							ff[i] = (double)(varr[i]);

						vv->Set(ff, (size_t)len);
						delete[] ff;

						m_outputLabels[name] = label;
						m_outputUnits[name] = units;
						if (!ui_hint.IsEmpty()) m_uiHints[name] = ui_hint;

					}
				}
				else if ((var_type == SSC_OUTPUT || var_type == SSC_INOUT) && data_type == SSC_MATRIX)
				{
					int nr, nc;
					if (ssc_number_t* varr = ssc_data_get_matrix(p_data, name, &nr, &nc))
					{
						if (m_outputList.Index(name) != wxNOT_FOUND) {
							m_outputList.Remove(name);
						}
						m_outputList.Add(name);
						VarValue* vv = m_outputs.Create(name, VV_MATRIX);
						matrix_t<double> ff(nr, nc);

						int count = 0;
						for (int i = 0; i < nr; i++)
						{
							for (int j = 0; j < nc; j++)
							{
								ff(i, j) = (double)(varr[count]);
								count++;
							}
						}
						vv->Set(ff);
						m_outputLabels[name] = label;
						m_outputUnits[name] = units;
						if (!ui_hint.IsEmpty()) m_uiHints[name] = ui_hint;
					}
				}
			}
		}

//		if (m_bSscTestsGeneration)
//			WriteSSCTestOutputs(m_simlist[kk], p_mod, p_data);


		ssc_module_free(p_mod);
	}

	// calling functions responsibility
//	ssc_data_free(p_data);

	// copy over warnings and errors from the simulation
	// for to enable retrieval after the simulation handler has gone away
	m_errors = ih->GetErrors();
	m_warnings = ih->GetWarnings();
	m_notices = ih->GetNotices();

	m_totalElapsedMsec = (int)sw.Time();

	return m_errors.size() == 0;

}



bool Simulation::InvokeWithHandler(ISimulationHandler *ih, wxString folder)
{
	assert( 0 != ih );

	m_totalElapsedMsec = 0;
	m_sscElapsedMsec = 0;
	wxStopWatch sw;

	ssc_data_t p_data = ssc_data_create();

	if ( m_simlist.size() == 0 )
		ih->Error("No simulation compute modules defined for this configuration.");

	// hybrids (already gone through equations in Prepare)
	if (m_case->GetConfiguration()->Technology.size() > 1) {
		// set "input" table
		ssc_data_t p_input = ssc_data_create();
		// set "compute_modules"
		ssc_var_t p_compute_modules[10];  // only constant allowed here - assumes m_simlist.size() < 10
		for (size_t i = 0; i < m_simlist.size() && i<10; i++) {
			p_compute_modules[i] = ssc_var_create();
			ssc_var_set_string(p_compute_modules[i], m_simlist[i].c_str());
		}
		ssc_data_set_data_array(p_input, "compute_modules", &p_compute_modules[0], (int)m_simlist.size());
		for (size_t i = 0; i < m_simlist.size() && i < 10; i++)
			ssc_var_free(p_compute_modules[i]);

		for (size_t i = 0; i < m_case->GetConfiguration()->Technology.size() && i < m_simlist.size(); i++) { // each vartable
			ssc_data_t p_val = ssc_data_create();
//			m_case->Values(i).AsSSCData(p_val); // skips overrides
			m_inputs[i].AsSSCData(p_val); // includes overrides
			if (i == m_case->GetConfiguration()->Technology.size() - 1) // "Hybrid" captures all non-generators and non-battery and non-fuel cell compute modules, e.g. "grid", "utility rate5","singleowner", etc.
				ssc_data_set_table(p_input, m_case->GetConfiguration()->Technology[i].c_str(), p_val);
			else
				ssc_data_set_table(p_input, m_simlist[i].c_str(), p_val);
			ssc_data_free(p_val);
		}
		// set "input" SSC_TABLE
		ssc_data_set_table(p_data, "input", p_input);
		ssc_data_free(p_input); // after deep copy above
		// invoke "hybrid" compute module
		ssc_module_t p_modhybrid = ssc_module_create("hybrid");
		if (!p_modhybrid) {
			wxString err = "could not create ssc module: hybrid";
			ssc_data_set_string(p_data, "error", err.c_str());
			return false;
		}
		wxStopWatch ssctime;
		ssc_bool_t ok = ssc_module_exec_with_handler(p_modhybrid, p_data, ssc_invoke_handler, ih);
		m_sscElapsedMsec += (int)ssctime.Time();
		// process "output" table
		if (!ok) {
			ih->Error("Simulation " + m_case->GetConfiguration()->Technology[m_case->GetConfiguration()->Technology.size() - 1] + " failed :" + ssc_module_log(p_modhybrid, 0, nullptr, nullptr));
		}
		else {
			int poutidx = 0;
			while (const ssc_info_t p_outinf = ssc_module_var_info(p_modhybrid, poutidx++))
			{
				int out_var_type = ssc_info_var_type(p_outinf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
				int out_data_type = ssc_info_data_type(p_outinf); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
				const char* out_name = ssc_info_name(p_outinf); // assumed to be non-null

				// finish setting up outputs
				if ((out_var_type == SSC_OUTPUT || out_var_type == SSC_INOUT) && out_data_type == SSC_TABLE) {
					// parse and flatten "output" table from ssc
					ssc_data_t p_outputs = ssc_data_get_table(p_data, out_name); // every variable in "output" is an output - do not need to check type below
					for (size_t i = 0; i < m_simlist.size(); i++) { // each vartable
						const char* vartable_name;
						wxString prepend_name, prepend_label;
						if (i >= m_case->GetConfiguration()->Technology.size() - 1) {
							vartable_name = m_case->GetConfiguration()->Technology[m_case->GetConfiguration()->Technology.size() - 1].c_str();
//							prepend_name = m_case->GetConfiguration()->Technology[m_case->GetConfiguration()->Technology.size() - 1];
							prepend_name = ""; // change "Hybrid" to nothing for normal metric processing
							prepend_label = ""; // change "Hybrid" to nothing for normal metric processing
						}
						else {
							vartable_name = m_simlist[i].c_str();
							prepend_name = m_case->GetConfiguration()->Technology[i].Lower() + "_";
							prepend_label = m_case->GetConfiguration()->Technology[i] + " ";
						}
						ssc_data_t p_vartable = ssc_data_get_table(p_outputs, vartable_name);
						// prepend outputs with "Technology" names (i.e. bin_names)

						ssc_module_t p_mod = ssc_module_create(m_simlist[i].c_str());
						int pidx = 0;
						while (const ssc_info_t p_inf = ssc_module_var_info(p_mod, pidx++))
						{
							//int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
							int data_type = ssc_info_data_type(p_inf); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
							const char* name = ssc_info_name(p_inf); // assumed to be non-null
							wxString label(ssc_info_label(p_inf));
							wxString units(ssc_info_units(p_inf));
							wxString ui_hint(ssc_info_uihint(p_inf));
							wxString sam_output_name = prepend_name + name; // hybrid processing
							wxString label_no_space = label;
							label_no_space.Replace(" ", "");
							if (((prepend_name.Len() > 0))
								&& (prepend_name.Left(prepend_name.length() - 1).Lower() != label.Left(prepend_name.length() - 1).Lower())  // check for "Battery Battery..."
								&& (prepend_name.Left(prepend_name.length() - 1).Lower() != label_no_space.Left(prepend_name.length() - 1).Lower()))  {// check for "FuelCell fuel cell..."
								if (label.Left(2) == "AC" || label.Left(2) == "DC") // e.g. PVWatts AC..."
									label = prepend_label + label;
								else
									label = prepend_label + label.Left(1) + label.Right(label.length()-1);
							}

							if (/*(var_type == SSC_OUTPUT || var_type == SSC_INOUT) &&*/ data_type == SSC_NUMBER)
							{
								ssc_number_t vval;
								if (ssc_data_get_number(p_vartable, name, &vval))
								{
									if (m_outputList.Index(sam_output_name) != wxNOT_FOUND) {
										m_outputList.Remove(sam_output_name);
									}

									m_outputList.Add(sam_output_name);
									VarValue* vv = m_outputs.Create(sam_output_name, VV_NUMBER);
									vv->Set((float)vval);

									m_outputLabels[sam_output_name] = label;
									m_outputUnits[sam_output_name] = units;
									if (!ui_hint.IsEmpty()) m_uiHints[sam_output_name] = ui_hint;
								}
							}
							else if (/*(var_type == SSC_OUTPUT || var_type == SSC_INOUT) &&*/ data_type == SSC_ARRAY)
							{
								int len;
								if (ssc_number_t* varr = ssc_data_get_array(p_vartable, name, &len))
								{
									if (m_outputList.Index(sam_output_name) != wxNOT_FOUND) {
										m_outputList.Remove(sam_output_name);
									}
									m_outputList.Add(sam_output_name);
									VarValue* vv = m_outputs.Create(sam_output_name, VV_ARRAY);
									double* ff = new double[len];
									for (int i = 0; i < len; i++)
										ff[i] = (double)(varr[i]);

									vv->Set(ff, (size_t)len);
									delete[] ff;

									m_outputLabels[sam_output_name] = label;
									m_outputUnits[sam_output_name] = units;
									if (!ui_hint.IsEmpty()) m_uiHints[sam_output_name] = ui_hint;

								}
							}
							else if (/*(var_type == SSC_OUTPUT || var_type == SSC_INOUT) && */data_type == SSC_MATRIX)
							{
								int nr, nc;
								if (ssc_number_t* varr = ssc_data_get_matrix(p_vartable, name, &nr, &nc))
								{
									if (m_outputList.Index(sam_output_name) != wxNOT_FOUND) {
										m_outputList.Remove(sam_output_name);
									}
									m_outputList.Add(sam_output_name);
									VarValue* vv = m_outputs.Create(sam_output_name, VV_MATRIX);
									matrix_t<double> ff(nr, nc);

									int count = 0;
									for (int i = 0; i < nr; i++)
									{
										for (int j = 0; j < nc; j++)
										{
											ff(i, j) = (double)(varr[count]);
											count++;
										}
									}
									vv->Set(ff);
									m_outputLabels[sam_output_name] = label;
									m_outputUnits[sam_output_name] = units;
									if (!ui_hint.IsEmpty()) m_uiHints[sam_output_name] = ui_hint;
								}
							}
						}
						ssc_module_free(p_mod);
					}
				}
			}
		}
		ssc_module_free(p_modhybrid);
	}
	else { // previous Tech/Fin processing
		for (size_t kk = 0; kk < m_simlist.size(); kk++)
		{
			ssc_module_t p_mod = ssc_module_create(m_simlist[kk].c_str());
			if (!p_mod) {
				wxString err = "could not create ssc module: " + m_simlist[kk];
				ssc_data_set_string(p_data, "error", err.c_str());
				return false;
			}

			if (!CmodInputsToSSCData(p_mod, p_data)) {
				ih->Error(ssc_data_get_string(p_data, "error"));
			}

			if (m_bSscTestsGeneration)
				WriteSSCTestInputs(m_simlist[kk], p_mod, p_data);

			// optionally write a debug input file if the ISimulationHandler defines it
			wxString fn = folder + "/ssc-" + m_simlist[kk] + ".lk";
			ih->WriteDebugFile(fn, p_mod, p_data);
			//ih->WriteDebugFile( m_simlist[kk], p_mod, p_data );

			wxStopWatch ssctime;
			ssc_bool_t ok = ssc_module_exec_with_handler(p_mod, p_data, ssc_invoke_handler, ih);
			m_sscElapsedMsec += (int)ssctime.Time();

			if (!ok)
			{
				ih->Error("Simulation " + m_simlist[kk] + " failed :" + ssc_module_log(p_mod, 0, nullptr, nullptr));
			}
			else
			{
				int pidx = 0;
				while (const ssc_info_t p_inf = ssc_module_var_info(p_mod, pidx++))
				{
					int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
					int data_type = ssc_info_data_type(p_inf); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
					const char* name = ssc_info_name(p_inf); // assumed to be non-null
					wxString label(ssc_info_label(p_inf));
					wxString units(ssc_info_units(p_inf));
					wxString ui_hint(ssc_info_uihint(p_inf));


					if ((var_type == SSC_OUTPUT || var_type == SSC_INOUT) && data_type == SSC_NUMBER)
					{
						ssc_number_t vval;
						if (ssc_data_get_number(p_data, name, &vval))
						{
							if (m_outputList.Index(name) != wxNOT_FOUND) {
								m_outputList.Remove(name);
							}

							m_outputList.Add(name);
							VarValue* vv = m_outputs.Create(name, VV_NUMBER);
							vv->Set((float)vval);

							m_outputLabels[name] = label;
							m_outputUnits[name] = units;
							if (!ui_hint.IsEmpty()) m_uiHints[name] = ui_hint;
						}
					}
					else if ((var_type == SSC_OUTPUT || var_type == SSC_INOUT) && data_type == SSC_ARRAY)
					{
						int len;
						if (ssc_number_t* varr = ssc_data_get_array(p_data, name, &len))
						{
							if (m_outputList.Index(name) != wxNOT_FOUND) {
								m_outputList.Remove(name);
							}
							m_outputList.Add(name);
							VarValue* vv = m_outputs.Create(name, VV_ARRAY);
							double* ff = new double[len];
							for (int i = 0; i < len; i++)
								ff[i] = (double)(varr[i]);

							vv->Set(ff, (size_t)len);
							delete[] ff;

							m_outputLabels[name] = label;
							m_outputUnits[name] = units;
							if (!ui_hint.IsEmpty()) m_uiHints[name] = ui_hint;

						}
					}
					else if ((var_type == SSC_OUTPUT || var_type == SSC_INOUT) && data_type == SSC_MATRIX)
					{
						int nr, nc;
						if (ssc_number_t* varr = ssc_data_get_matrix(p_data, name, &nr, &nc))
						{
							if (m_outputList.Index(name) != wxNOT_FOUND) {
								m_outputList.Remove(name);
							}
							m_outputList.Add(name);
							VarValue* vv = m_outputs.Create(name, VV_MATRIX);
							matrix_t<double> ff(nr, nc);

							int count = 0;
							for (int i = 0; i < nr; i++)
							{
								for (int j = 0; j < nc; j++)
								{
									ff(i, j) = (double)(varr[count]);
									count++;
								}
							}
							vv->Set(ff);
							m_outputLabels[name] = label;
							m_outputUnits[name] = units;
							if (!ui_hint.IsEmpty()) m_uiHints[name] = ui_hint;
						}
					}
				}
			}

			if (m_bSscTestsGeneration)
				WriteSSCTestOutputs(m_simlist[kk], p_mod, p_data);


			ssc_module_free(p_mod);
		}
	}

	ssc_data_free( p_data );
	
	// copy over warnings and errors from the simulation
	// for to enable retrieval after the simulation handler has gone away
	m_errors = ih->GetErrors();
	m_warnings = ih->GetWarnings();
	m_notices = ih->GetNotices();
	
	m_totalElapsedMsec = (int) sw.Time();

	return m_errors.size() == 0;

}

bool Simulation::GetInputsSSCData(ssc_data_t p_data) {
    if (m_simlist.empty())
        m_simlist = m_case->GetConfiguration()->Simulations;
    ssc_module_exec_set_print( 0 );
    for( size_t kk=0;kk<m_simlist.size();kk++ )
    {
        ssc_module_t p_mod = ssc_module_create(m_simlist[kk].c_str());
        if (!p_mod) {
            wxString err = "could not create ssc module: " + m_simlist[kk];
            ssc_data_set_string(p_data, "error", err.c_str());
            return false;
        }

        if (!CmodInputsToSSCData(p_mod, p_data)){
            return false;
        }
        ssc_bool_t result = ssc_module_exec( p_mod, p_data );

        // copy over first error if there was one to internal buffer
        if (!result)
        {
            int type, i=0;
			const char *text = ssc_module_log(p_mod, i, &type, 0);
            while(text)
            {
                if (type == SSC_ERROR)
                {
                    ssc_data_set_string(p_data, "error", text);
                    break;
                }
                i++;
				text = ssc_module_log(p_mod, i, &type, 0);
            }
        }
        ssc_module_free(p_mod);
    }
    ssc_module_exec_set_print( 1 );
    return true;
}

void Simulation::ListByCount( size_t nr, size_t nc, wxArrayString &list )
{
	VarTable &vt = Outputs();
	for ( VarTable::iterator it = vt.begin(); it != vt.end(); ++it )
	{
		size_t nrows, ncols;
		nrows = 1; ncols = 1;
		if ( it->second->Type() == VV_NUMBER )
			nrows = 1;
		else if ( it->second->Type() == VV_ARRAY )
			nrows = it->second->Length();
		else if (it->second->Type() == VV_MATRIX)
		{
			nrows = it->second->Rows();
			ncols = it->second->Columns();
		}
        else if (it->second->Type() == VV_TABLE)
        {
            nrows = it->second->Rows();
            ncols = it->second->Columns();
        }

		if ( nr == nrows && nc == ncols )
			list.Add( it->first );
	}
}


wxArrayString Simulation::GetAllMessages()
{
	wxArrayString list;
	for( size_t i=0;i<m_errors.size();i++ )
		list.Add( "Error: " + m_errors[i] );
	for( size_t i=0;i<m_warnings.size();i++ )
		list.Add( "Warning: " + m_warnings[i] );
	for( size_t i=0;i<m_notices.size();i++ )
		list.Add( "Notice: " + m_notices[i] );
	return list;
}

void Simulation::GetVariableLengths( std::vector<ArraySize> &sizes )
{	
	sizes.clear();
	

	VarTable &vt = Outputs();
	if( vt.size() == 0 ) return;
	
	bool has_single_value = false;
	ArraySize tmp;

	for (VarTable::iterator it = vt.begin(); it != vt.end(); ++it)
	{
		if (it->second->Type() == VV_ARRAY)
		{
			size_t n = 0;
			it->second->Array(&n);
			tmp.n_rows = n; tmp.n_cols = 1;
			if (n > 1 && std::find(sizes.begin(), sizes.end(), tmp) == sizes.end())
				sizes.push_back(tmp);	
		}
		else if (it->second->Type() == VV_NUMBER)
			has_single_value = true;
		else if (it->second->Type() == VV_MATRIX)
		{
			it->second->Matrix(&tmp.n_rows, &tmp.n_cols);
			if (tmp.n_rows > 1 && tmp.n_cols > 0 && std::find(sizes.begin(), sizes.end(), tmp) == sizes.end())
				sizes.push_back(tmp);
		}
	}

	if (has_single_value)
	{
		tmp.n_rows = tmp.n_cols = 1;
		sizes.push_back(tmp);
	}
	
	// sort variable lengths
	std::stable_sort(sizes.begin(),sizes.end(), SortByRow());
}

bool Simulation::ListAllOutputs( ConfigInfo *cfg, 
	wxArrayString *names, 
	wxArrayString *labels, 
	wxArrayString *units, 
	wxArrayString *groups,
	wxArrayString* types,
	bool single_values )  // TODO: hybrids
{
	if ( !cfg ) return false;

	for( size_t kk=0;kk<cfg->Simulations.size();kk++ )
	{
		wxString prepend_name, prepend_label;
		if (kk >= cfg->Technology.size() - 1) {
			prepend_name = ""; // change "Hybrid" to nothing for normal metric processing
			prepend_label = ""; // change "Hybrid" to nothing for normal metric processing
		}
		else {
			prepend_name = cfg->Technology[kk].Lower() + "_";
			prepend_label = cfg->Technology[kk] + " ";
		}


		ssc_module_t p_mod = ssc_module_create( cfg->Simulations[kk].c_str() );
		if ( !p_mod )
			return false;

		int pidx=0;
		while( const ssc_info_t p_inf = ssc_module_var_info( p_mod, pidx++ ) )
		{
			int var_type = ssc_info_var_type( p_inf );   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
			int data_type = ssc_info_data_type( p_inf ); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
			
			if ( var_type == SSC_OUTPUT || var_type == SSC_INOUT )
			{
				if ( !single_values || (single_values && data_type == SSC_NUMBER ) )
				{
					wxString strName = wxString(ssc_info_name(p_inf));
                    // inconsistent with list of variables which includes last label; e.g. "Annual Energy AC (year 1)" (cmod_grid) instead of "Annual AC energy" (cmod_pvsamv1) for pv-batt / commercial configuration
					//if (names && (names->Index(strName) == wxNOT_FOUND)) {
						// incorrect - multiple listings for SSC_INOUT and SSC_OUTPUT for multiple compute modules - see SAM issue #393
                    if (names) {
                        auto ndx = names->Index(strName);
                        if (ndx == wxNOT_FOUND) {
                            if (names) names->Add(prepend_name + wxString(ssc_info_name(p_inf)));
                            if (labels) labels->Add(prepend_label + wxString(ssc_info_label(p_inf)));
                            if (units) units->Add(wxString(ssc_info_units(p_inf)));
                            if (groups) groups->Add(wxString(ssc_info_group(p_inf)));
                            if (types) types->Add(wxString::Format(wxT("%i"), data_type));
                        }
                        else {
                            if (labels) labels->Item(ndx) = prepend_label + wxString(ssc_info_label(p_inf));
                            if (units) units->Item(ndx) = wxString(ssc_info_units(p_inf));
                            if (groups) groups->Item(ndx) = wxString(ssc_info_group(p_inf));
                            if (types) types->Item(ndx) = wxString::Format(wxT("%i"), data_type);
                        }
					}
				}
			}
		}
	}

	return true;
}


class SimulationThread : public wxThread, ISimulationHandler
{
	std::vector<Simulation*> m_list;
	wxMutex m_currentLock, m_cancelLock, m_nokLock, m_logLock, m_percentLock;
	size_t m_current;
	bool m_canceled;
	size_t m_nok;
	wxArrayString m_messages;
	wxString m_update;
	wxString m_curName;
	float m_percent;
	int m_threadId;
public:

	SimulationThread( int id )
		: wxThread( wxTHREAD_JOINABLE ) {
		m_canceled = false;
		m_threadId = id;
		m_nok = 0;
		m_percent = 0;
		m_current = 0;
	}
	
	void Add( Simulation *s ) {
		m_list.push_back( s );
	}

	size_t Size() { return m_list.size(); }
	size_t Current() { 
		wxMutexLocker _lock( m_currentLock );
		return m_current;
	}
	float GetPercent( wxString *update = 0) {
		size_t ns = Size();
		size_t cur = Current();
		wxMutexLocker _lock(m_percentLock);
		float curper = m_percent;

		if ( update != 0 )
			*update = m_update;

		if ( ns == 0 ) return 0.0f;

		if ( cur < ns )
		{
			float each = 100/ns;
			float overall = cur*each + 0.01*curper*each;
			return overall;
		}
		else
			return 100.0f;
	}

	void Cancel()
	{
		wxMutexLocker _lock(m_cancelLock);
		m_canceled = true;
	}

	size_t NOk() {
		wxMutexLocker _lock(m_nokLock);
		return m_nok;
	}
	
	void Message( const wxString &text )
	{
		wxMutexLocker _lock(m_logLock);
		wxString L( m_curName );
		if ( !L.IsEmpty() ) L += ": ";
		m_messages.Add( L + text );
	}
	
	virtual void Warn( const wxString &text )
	{
		ISimulationHandler::Warn( text );
		Message( text );
	}

	virtual void Error( const wxString &text )
	{
		ISimulationHandler::Error( text );
		Message( text );
	}

	virtual void Update( float percent, const wxString &text )
	{
		wxMutexLocker _lock(m_percentLock);
		m_percent = percent;
		m_update = text;
	}


	virtual bool IsCancelled() {
		wxMutexLocker _lock(m_cancelLock);
		return m_canceled;
	}
	
	wxArrayString GetNewMessages()
	{
		wxMutexLocker _lock(m_logLock);
		wxArrayString list = m_messages;
		m_messages.Clear();
		return list;
	}

	virtual void *Entry()
	{
		m_canceled = false;
		for( size_t i=0;i<m_list.size();i++ )
		{
			m_curName = m_list[i]->GetName();

			// clear any saved messages from the previous simulation
			ClearSavedMessages();
			if ( m_list[i]->InvokeWithHandler( this ) )
			{
				wxMutexLocker _lock(m_nokLock);
				m_nok++;
			}

			m_currentLock.Lock();
			m_current++;
			m_currentLock.Unlock();

			wxMutexLocker _lock(m_cancelLock);
			if (m_canceled) break;
		}

		return 0;
	}
    
    virtual bool WriteDebugFile( const wxString &,     ssc_module_t, ssc_data_t ) {return false; }; 

    
};



class SimulationSSCThread : public wxThread, ISimulationHandler
{
	std::vector<Simulation*> m_list;
	wxMutex m_currentLock, m_cancelLock, m_nokLock, m_logLock, m_percentLock;
	size_t m_current;
	bool m_canceled;
	size_t m_nok;
	wxArrayString m_messages;
	wxString m_update;
	wxString m_curName;
	float m_percent;
	int m_threadId;
	ssc_data_t m_data;
public:

	SimulationSSCThread(int id, ssc_data_t data)
		: wxThread(wxTHREAD_JOINABLE) {
		m_canceled = false;
		m_threadId = id;
		m_data = data;
		m_nok = 0;
		m_percent = 0;
		m_current = 0;
	}

	void Add(Simulation* s) {
		m_list.push_back(s);
	}

	size_t Size() { return m_list.size(); }
	size_t Current() {
		wxMutexLocker _lock(m_currentLock);
		return m_current;
	}
	float GetPercent(wxString* update = 0) {
		size_t ns = Size();
		size_t cur = Current();
		wxMutexLocker _lock(m_percentLock);
		float curper = m_percent;

		if (update != 0)
			*update = m_update;

		if (ns == 0) return 0.0f;

		if (cur < ns)
		{
			float each = 100 / ns;
			float overall = cur * each + 0.01 * curper * each;
			return overall;
		}
		else
			return 100.0f;
	}

	void Cancel()
	{
		wxMutexLocker _lock(m_cancelLock);
		m_canceled = true;
	}

	size_t NOk() {
		wxMutexLocker _lock(m_nokLock);
		return m_nok;
	}

	void Message(const wxString& text)
	{
		wxMutexLocker _lock(m_logLock);
		wxString L(m_curName);
		if (!L.IsEmpty()) L += ": ";
		m_messages.Add(L + text);
	}

	virtual void Warn(const wxString& text)
	{
		ISimulationHandler::Warn(text);
		Message(text);
	}

	virtual void Error(const wxString& text)
	{
		ISimulationHandler::Error(text);
		Message(text);
	}

	virtual void Update(float percent, const wxString& text)
	{
		wxMutexLocker _lock(m_percentLock);
		m_percent = percent;
		m_update = text;
	}


	virtual bool IsCancelled() {
		wxMutexLocker _lock(m_cancelLock);
		return m_canceled;
	}

	wxArrayString GetNewMessages()
	{
		wxMutexLocker _lock(m_logLock);
		wxArrayString list = m_messages;
		m_messages.Clear();
		return list;
	}

	virtual void* Entry()
	{
		m_canceled = false;
		for (size_t i = 0; i < m_list.size(); i++)
		{
			m_curName = m_list[i]->GetName();

			// clear any saved messages from the previous simulation
			ClearSavedMessages();
			if (m_list[i]->InvokeSSCWithHandler(this, m_data))
			{
				wxMutexLocker _lock(m_nokLock);
				m_nok++;
			}

			m_currentLock.Lock();
			m_current++;
			m_currentLock.Unlock();

			wxMutexLocker _lock(m_cancelLock);
			if (m_canceled) break;
		}

		return 0;
	}

	virtual bool WriteDebugFile(const wxString&, ssc_module_t, ssc_data_t) { return false; };


};


int Simulation::DispatchThreads( SimulationDialog &tpd, 
	std::vector<Simulation*> &sims, 
	int nthread )
{
	return DispatchThreads( tpd.Dialog(), sims, nthread );
} 

int Simulation::DispatchThreads( wxThreadProgressDialog &tpd, 
	std::vector<Simulation*> &sims, 
	int nthread )
{	
	wxStopWatch sw;

	// no need to create extra unnecessary threads 
	if (nthread > (int)sims.size()) nthread = sims.size();

	std::vector<SimulationThread*> threads;
	for( int i=0;i<nthread;i++)
	{
		SimulationThread *t = new SimulationThread( i );
		threads.push_back( t );
		t->Create();
	}

	// round robin assign each simulation to a thread
	size_t ithr = 0;
	for( size_t i=0;i<sims.size();i++ )
	{
		threads[ithr++]->Add( sims[i] );
		if ( ithr == threads.size() )
			ithr = 0;
	}

	sw.Start();
	
	// start the threads
	for ( int i=0;i<nthread;i++ )
		threads[i]->Run();

 	while (1)
	{
		size_t i, num_finished = 0;
		for (i=0;i<threads.size();i++)
			if (!threads[i]->IsRunning())
				num_finished++;

		if (num_finished == threads.size())
			break;

		// threads still running so update interface
		for (i=0;i<threads.size();i++)
		{
			wxString update;
			float per = threads[i]->GetPercent(&update);
			tpd.Update(i, per, update);
			wxArrayString msgs = threads[i]->GetNewMessages();
			tpd.Log( msgs );
		}

         wxGetApp().Yield();

		// if dialog's cancel button was pressed, send cancel signal to all threads
		if (tpd.IsCanceled())
		{
			for (i=0;i<threads.size();i++)
				threads[i]->Cancel();
		}

		::wxMilliSleep( 100 );
	}

 	size_t nok = 0;
	// wait on the joinable threads
	for (size_t i=0;i<threads.size();i++)
	{
		threads[i]->Wait();
		nok += threads[i]->NOk();

		// update final progress
		float per = threads[i]->GetPercent();
		tpd.Update(i, per);

		// get any final simulation messages
		wxArrayString msgs = threads[i]->GetNewMessages();
		tpd.Log( msgs );
	}
	
	// delete all the thread objects
	for (size_t i=0;i<threads.size();i++)
		delete threads[i];

	threads.clear();
	
	return nok;
}


int Simulation::DispatchSSCThreads(SimulationDialog& tpd,
	std::vector<Simulation*>& sims,
	int nthread, wxString& fn)
{
	int ret = 0;

	std::ifstream test(fn.ToStdString().c_str());
	std::string json_str((std::istreambuf_iterator<char>(test)), std::istreambuf_iterator<char>());
	auto p_data = json_to_ssc_data(json_str.c_str());

	ret =  DispatchSSCThreads(tpd.Dialog(), sims, nthread, p_data);

	ssc_data_free(p_data);

	return ret;
}

int Simulation::DispatchSSCThreads(wxThreadProgressDialog& tpd,
	std::vector<Simulation*>& sims,
	int nthread, ssc_data_t data)
{
	wxStopWatch sw;

	// no need to create extra unnecessary threads 
	if (nthread > (int)sims.size()) nthread = sims.size();

	std::vector<SimulationSSCThread*> threads;
	for (int i = 0; i < nthread; i++)
	{
		SimulationSSCThread* t = new SimulationSSCThread(i, data);
		threads.push_back(t);
		t->Create();
	}

	// round robin assign each simulation to a thread
	size_t ithr = 0;
	for (size_t i = 0; i < sims.size(); i++)
	{
		threads[ithr++]->Add(sims[i]);
		if (ithr == threads.size())
			ithr = 0;
	}

	sw.Start();

	// start the threads
	for (int i = 0; i < nthread; i++)
		threads[i]->Run();

	while (1)
	{
		size_t i, num_finished = 0;
		for (i = 0; i < threads.size(); i++)
			if (!threads[i]->IsRunning())
				num_finished++;

		if (num_finished == threads.size())
			break;

		// threads still running so update interface
		for (i = 0; i < threads.size(); i++)
		{
			wxString update;
			float per = threads[i]->GetPercent(&update);
			tpd.Update(i, per, update);
			wxArrayString msgs = threads[i]->GetNewMessages();
			tpd.Log(msgs);
		}

		wxGetApp().Yield();

		// if dialog's cancel button was pressed, send cancel signal to all threads
		if (tpd.IsCanceled())
		{
			for (i = 0; i < threads.size(); i++)
				threads[i]->Cancel();
		}

		::wxMilliSleep(100);
	}

	size_t nok = 0;
	// wait on the joinable threads
	for (size_t i = 0; i < threads.size(); i++)
	{
		threads[i]->Wait();
		nok += threads[i]->NOk();

		// update final progress
		float per = threads[i]->GetPercent();
		tpd.Update(i, per);

		// get any final simulation messages
		wxArrayString msgs = threads[i]->GetNewMessages();
		tpd.Log(msgs);
	}

	// delete all the thread objects
	for (size_t i = 0; i < threads.size(); i++)
		delete threads[i];

	threads.clear();

	return nok;
}



SimulationDialog::SimulationDialog( const wxString &message, int nthread )
{
	if ( nthread < 1 )
		nthread = wxThread::GetCPUCount();

	m_transp = wxCreateTransparentOverlay( SamApp::Window() );
	m_tpd = new wxThreadProgressDialog( m_transp, nthread );
	m_tpd->Show();

	if ( message.IsEmpty() )
		m_tpd->Status( "Simulating...");
	else
		m_tpd->Status( message );
	m_tpd->ShowBars( 1 );
    wxYield();
}

SimulationDialog::~SimulationDialog()
{
	m_tpd->Destroy();
	m_transp->Destroy();
}

void SimulationDialog::Finalize( const wxString & )
{			
	wxYield(); // allow status bars to show full update
	m_tpd->Finalize();
}

void SimulationDialog::Update(int ThreadNum, float percent, const wxString &label )
{
	m_tpd->Update(ThreadNum, percent, label);
	wxYield();
}

void SimulationDialog::NewStage( const wxString &title, int nbars_to_show )
{
	m_tpd->Reset();
	m_tpd->Status( title );
	m_tpd->ShowBars( nbars_to_show );
	wxYield();
}

