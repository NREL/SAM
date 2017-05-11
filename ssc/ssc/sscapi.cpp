#include <stdio.h>
#include <cstring>

#include "core.h"
#include "sscapi.h"

SSCEXPORT int ssc_version()
{
	return 175;
}

SSCEXPORT const char *ssc_build_info()
{
	static const char *_bi = __PLATFORM__ " " __ARCH__ " " __COMPILER__ " " __DATE__ " " __TIME__;
	return _bi;
}

/* to add new computation modules, 
	specify an extern module entry,
	and add it to 'module_table'
*/

extern module_entry_info 
/* extern declarations of modules for linking */
	cm_entry_singlediode,
	cm_entry_singlediodeparams,
	cm_entry_iec61853par,
	cm_entry_iec61853interp,
	cm_entry_6parsolve,
	cm_entry_pvsamv1,
	cm_entry_pvwattsv0,
	cm_entry_pvwattsv1,
	cm_entry_pvwattsv1_1ts,
	cm_entry_pvwattsv1_poa,
	cm_entry_pvwattsv5,
	cm_entry_pvwattsv5_1ts,
	cm_entry_pv6parmod,
	cm_entry_pvsandiainv,
	cm_entry_wfreader,
	cm_entry_irradproc,
	cm_entry_utilityrate,
	cm_entry_utilityrate2,
	cm_entry_utilityrate3,
	cm_entry_utilityrate4,
	cm_entry_utilityrate5,
	cm_entry_annualoutput,
	cm_entry_cashloan,
	cm_entry_thirdpartyownership,
	cm_entry_ippppa,
	cm_entry_timeseq,
	cm_entry_levpartflip,
	cm_entry_equpartflip,
	cm_entry_saleleaseback,
	cm_entry_singleowner,
	cm_entry_swh,
	cm_entry_geothermal,
	cm_entry_windpower,
	cm_entry_poacalib,
	cm_entry_snowmodel,
	cm_entry_generic_system,
	cm_entry_wfcsvconv,
	cm_entry_tcstrough_empirical,
	cm_entry_tcstrough_physical,
	cm_entry_trough_physical_csp_solver,
	cm_entry_trough_physical_process_heat,
	cm_entry_iph_to_lcoefcr,
	cm_entry_tcsgeneric_solar,
	cm_entry_tcsmolten_salt,
	cm_entry_tcsdirect_steam,
	cm_entry_tcslinear_fresnel,
	cm_entry_linear_fresnel_dsg_iph,
	cm_entry_tcsdish,
	cm_entry_tcsiscc,
	cm_entry_tcsmslf,
	cm_entry_hcpv,
	cm_entry_wind_file_reader,
	cm_entry_wfcheck,
	cm_entry_windbos,
	cm_entry_wind_obos,
	cm_entry_biomass,
	cm_entry_solarpilot,
	cm_entry_belpe,
	cm_entry_dsg_flux_preprocess,
	cm_entry_layoutarea,
	cm_entry_sco2_design_point,
	cm_entry_sco2_design_cycle,
	cm_entry_sco2_csp_system,
	cm_entry_sco2_csp_ud_pc_tables,
	cm_entry_user_htf_comparison,
	cm_entry_ui_tes_calcs,
	cm_entry_cb_mspt_system_costs,
	cm_entry_cb_construction_financing,
	cm_entry_cb_empirical_hce_heat_loss,
	cm_entry_iscc_design_point,
	cm_entry_battery,
	cm_entry_battwatts,
   	cm_entry_lcoefcr,
	cm_entry_pv_get_shade_loss_mpp,
	cm_entry_inv_cec_cg;

/* official module table */
static module_entry_info *module_table[] = {
	&cm_entry_singlediode,
	&cm_entry_singlediodeparams,
	&cm_entry_iec61853par,
	&cm_entry_iec61853interp,
	&cm_entry_6parsolve,
	&cm_entry_pv6parmod,
	&cm_entry_pvsamv1,
	//&cm_entry_pvwattsv0,
	&cm_entry_pvwattsv1,
	&cm_entry_pvwattsv1_1ts,
	&cm_entry_pvwattsv1_poa,
	&cm_entry_pvwattsv5,
	&cm_entry_pvwattsv5_1ts,
	&cm_entry_pvsandiainv,
	&cm_entry_wfreader,
	&cm_entry_irradproc,
	&cm_entry_utilityrate,
	&cm_entry_utilityrate2,
	&cm_entry_utilityrate3,
	&cm_entry_utilityrate4,
	&cm_entry_utilityrate5,
	&cm_entry_annualoutput,
	&cm_entry_cashloan,
	&cm_entry_thirdpartyownership,
	&cm_entry_ippppa,
	&cm_entry_timeseq,
	&cm_entry_levpartflip,
	&cm_entry_equpartflip,
	&cm_entry_saleleaseback,
	&cm_entry_singleowner,
	&cm_entry_swh,
	&cm_entry_geothermal,
	&cm_entry_windpower,
	&cm_entry_poacalib,
	&cm_entry_snowmodel,
	&cm_entry_generic_system,
	&cm_entry_wfcsvconv,
	&cm_entry_tcstrough_empirical,
	&cm_entry_tcstrough_physical,
	&cm_entry_trough_physical_csp_solver,
	&cm_entry_trough_physical_process_heat,
	&cm_entry_iph_to_lcoefcr,
	&cm_entry_tcsgeneric_solar,
	&cm_entry_tcsmolten_salt,
	&cm_entry_tcsdirect_steam,
	&cm_entry_tcslinear_fresnel,
	&cm_entry_linear_fresnel_dsg_iph,
	&cm_entry_tcsdish,
	&cm_entry_tcsiscc,
	&cm_entry_tcsmslf,
	&cm_entry_hcpv,
	&cm_entry_wind_file_reader,
	&cm_entry_wfcheck,
	&cm_entry_windbos,
	&cm_entry_wind_obos,
	&cm_entry_biomass,
	&cm_entry_solarpilot,
	&cm_entry_belpe,
	&cm_entry_dsg_flux_preprocess,
	&cm_entry_layoutarea,
	&cm_entry_sco2_design_point,
	&cm_entry_sco2_design_cycle,
	&cm_entry_sco2_csp_system,
	&cm_entry_sco2_csp_ud_pc_tables,
	&cm_entry_user_htf_comparison,
	&cm_entry_ui_tes_calcs,
	&cm_entry_cb_mspt_system_costs,
	&cm_entry_cb_construction_financing,
	&cm_entry_cb_empirical_hce_heat_loss,
	&cm_entry_iscc_design_point,
	&cm_entry_battery,
	&cm_entry_battwatts,
	&cm_entry_lcoefcr,
	&cm_entry_pv_get_shade_loss_mpp,
	&cm_entry_inv_cec_cg,
	0 };

SSCEXPORT ssc_module_t ssc_module_create( const char *name )
{
	std::string lname = util::lower_case( name );

	int i=0;
	while ( module_table[i] != 0
		 && module_table[i]->f_create != 0 )
	{
		if ( lname == util::lower_case( module_table[i]->name ) )
			return (*(module_table[i]->f_create))();
		i++;
	}

	return 0;
}

SSCEXPORT void ssc_module_free( ssc_module_t p_mod )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (cm) delete cm; // calls destructors for compute_module and tcskernel if a ssc-tcs technology
}

/*************************** data object manipulation ***************************/

SSCEXPORT ssc_data_t ssc_data_create()
{
	return static_cast<ssc_data_t>( new var_table );
}

SSCEXPORT void ssc_data_free( ssc_data_t p_data )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (vt) delete vt;
}

SSCEXPORT void ssc_data_clear( ssc_data_t p_data )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (vt) vt->clear();
}

SSCEXPORT void ssc_data_unassign( ssc_data_t p_data, const char *name )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->unassign( name );
}


SSCEXPORT int ssc_data_rename( ssc_data_t p_data, const char *oldname, const char *newname )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;

	return vt->rename( oldname, newname ) ? 1 : 0;
}

SSCEXPORT int ssc_data_query( ssc_data_t p_data, const char *name )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return SSC_INVALID;
	var_data *dat = vt->lookup(name);
	if (!dat) return SSC_INVALID;
	else return dat->type;
}

SSCEXPORT const char *ssc_data_first( ssc_data_t p_data ) // returns the name of the first data item, 0 if empty
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	return vt->first();
}

SSCEXPORT const char *ssc_data_next( ssc_data_t p_data ) // returns the next name in the data set object, 0, if none left.
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	return vt->next();
}

SSCEXPORT void ssc_data_set_string( ssc_data_t p_data, const char *name, const char *value )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data( std::string(value) ) );
}

SSCEXPORT void ssc_data_set_number( ssc_data_t p_data, const char *name, ssc_number_t value )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data( value ) );
}

SSCEXPORT void ssc_data_set_array( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int length )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data( pvalues, length ) );
}

SSCEXPORT void ssc_data_set_matrix( ssc_data_t p_data, const char *name, ssc_number_t *pvalues, int nrows, int ncols )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return;
	vt->assign( name, var_data(pvalues, nrows, ncols) );
}

SSCEXPORT void ssc_data_set_table( ssc_data_t p_data, const char *name, ssc_data_t table )
{
	var_table *vt = static_cast<var_table*>(p_data);
	var_table *value = static_cast<var_table*>(table);
	if (!vt || !value) return;
	var_data *dat = vt->assign( name, var_data() );
	dat->type = SSC_TABLE;
	dat->table = *value;  // invokes operator= for deep copy
}

SSCEXPORT const char *ssc_data_get_string( ssc_data_t p_data, const char *name )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != SSC_STRING) return 0;
	return dat->str.c_str();	
}

SSCEXPORT ssc_bool_t ssc_data_get_number( ssc_data_t p_data, const char *name, ssc_number_t *value )
{
	if (!value) return 0;
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != SSC_NUMBER) return 0;
	*value = dat->num;
	return 1;	
}

SSCEXPORT ssc_number_t *ssc_data_get_array(ssc_data_t p_data,  const char *name, int *length )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != SSC_ARRAY) return 0;
	if (length) *length = (int) dat->num.length();
	return dat->num.data();
}

SSCEXPORT ssc_number_t *ssc_data_get_matrix( ssc_data_t p_data, const char *name, int *nrows, int *ncols )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != SSC_MATRIX) return 0;
	if (nrows) *nrows = (int) dat->num.nrows();
	if (ncols) *ncols = (int) dat->num.ncols();
	return dat->num.data();
}

SSCEXPORT ssc_data_t ssc_data_get_table( ssc_data_t p_data, const char *name )
{
	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt) return 0;
	var_data *dat = vt->lookup(name);
	if (!dat || dat->type != SSC_TABLE) return 0;
	return static_cast<ssc_data_t>( &(dat->table) );
}

SSCEXPORT ssc_entry_t ssc_module_entry( int index )
{
	int max=0;
	while( module_table[max++] != 0 );

	if (index >= 0 && index < max) return static_cast<ssc_entry_t>(module_table[index]);
	else return 0;
}

SSCEXPORT const char *ssc_entry_name( ssc_entry_t p_entry )
{
	module_entry_info *p = static_cast<module_entry_info*>(p_entry);
	return p ? p->name : 0;
}

SSCEXPORT const char *ssc_entry_description( ssc_entry_t p_entry )
{
	module_entry_info *p = static_cast<module_entry_info*>(p_entry);
	return p ? p->description : 0;
}

SSCEXPORT int ssc_entry_version( ssc_entry_t p_entry )
{
	module_entry_info *p = static_cast<module_entry_info*>(p_entry);
	return p ? p->version : 0;
}


SSCEXPORT const ssc_info_t ssc_module_var_info( ssc_module_t p_mod, int index )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!cm) return 0;
	return static_cast<ssc_info_t>( cm->info( index ) );
}

SSCEXPORT int ssc_info_var_type( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->var_type : SSC_INVALID;
}

SSCEXPORT int ssc_info_data_type( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->data_type : SSC_INVALID;
}

SSCEXPORT const char *ssc_info_name( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->name : 0;
}

SSCEXPORT const char *ssc_info_label( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->label : 0;
}

SSCEXPORT const char *ssc_info_units( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->units : 0;
}

SSCEXPORT const char *ssc_info_meta( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->meta : 0;
}

SSCEXPORT const char *ssc_info_required( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi? vi->required_if : 0;
}

SSCEXPORT const char *ssc_info_group( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->group : 0;
}

SSCEXPORT const char *ssc_info_constraints( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->constraints : 0;
}

SSCEXPORT const char *ssc_info_uihint( ssc_info_t p_inf )
{
	var_info *vi = static_cast<var_info*>(p_inf);
	return vi ? vi->ui_hint : 0;
}

/*
class default_sync_proc : public util::sync_piped_process
{
private:
	ssc_handler_t m_handler;
public:
	default_sync_proc( ssc_handler_t ph ) : m_handler(ph) {  }

	virtual void on_stdout(const std::string &line_text)
	{
		ssc_module_extproc_output( m_handler, line_text.c_str() );
	}
};
*/

static ssc_bool_t default_internal_handler_no_print( ssc_module_t p_mod, ssc_handler_t p_handler,
	int action_type, float f0, float f1, 
	const char *s0, const char *s1,
	void *p_data )
{
	// ignore all warnings and errors
	// don't print progress updates
	return 1;
}

static ssc_bool_t default_internal_handler( ssc_module_t p_mod, ssc_handler_t p_handler,
	int action_type, float f0, float f1, 
	const char *s0, const char *s1,
	void *p_data )
{
	if (action_type == SSC_LOG)
	{
		// print log message to console
		std::cout << "Log ";
		switch( (int)f0 )
		{
		case SSC_NOTICE: std::cout << "Notice: " << s0 << " time " << f1 << std::endl; break;
		case SSC_WARNING: std::cout << "Warning: " << s0 << " time " << f1 << std::endl; break;
		case SSC_ERROR: std::cout << "Error: " << s0 << " time " << f1 << std::endl; break;
		default: std::cout << "Log notice uninterpretable: " << f0 << " time " << f1 << std::endl; break;
		}
		return 1;
	}
	else if (action_type == SSC_UPDATE)
	{
		// print status update to console
		printf( "%5.2f %% %s @ %g\n", f0, s0, f1 );
		return 1; // return 0 to abort simulation as needed.
	}
	else
		return 0;
}

SSCEXPORT ssc_bool_t ssc_module_exec_simple( const char *name, ssc_data_t p_data )
{
	ssc_module_t p_mod = ssc_module_create( name );
	if ( !p_mod ) return 0;
	
	ssc_bool_t result = ssc_module_exec( p_mod, p_data );

	ssc_module_free( p_mod );
	return result;
}

SSCEXPORT const char *ssc_module_exec_simple_nothread( const char *name, ssc_data_t p_data )
{
static char p_internal_buf[256];

	ssc_module_t p_mod = ssc_module_create( name );
	if (!p_mod) return 0;

	ssc_bool_t result = ssc_module_exec( p_mod, p_data );

	// copy over first error if there was one to internal buffer
	if (!result)
	{
		strcpy(p_internal_buf, "general error detected");

		const char *text;
		int type;
		int i=0;
		while( (text = ssc_module_log( p_mod, i, &type, 0 )) )
		{
			if (type == SSC_ERROR)
			{
				strncpy( p_internal_buf, text, 255 );
				break;
			}
			i++;
		}
	}

	ssc_module_free( p_mod );
	return result ? 0 : p_internal_buf;
}

static int sg_defaultPrint = 1;

SSCEXPORT void ssc_module_exec_set_print( int print )
{
	sg_defaultPrint = print;
}

SSCEXPORT ssc_bool_t ssc_module_exec( ssc_module_t p_mod, ssc_data_t p_data )
{
	return ssc_module_exec_with_handler( p_mod, p_data, sg_defaultPrint ? default_internal_handler : default_internal_handler_no_print, 0 );
}

class default_exec_handler : public handler_interface
{
private:
	ssc_bool_t (*m_hfunc)( ssc_module_t, ssc_handler_t, int, float, float, const char *, const char *, void * );
	void *m_hdata;

public:
	default_exec_handler(
		compute_module *cm,
		ssc_bool_t (*f)( ssc_module_t, ssc_handler_t, int, float, float, const char *, const char *, void * ),
		void *d )
		: handler_interface(cm)
	{
		m_hfunc = f;
		m_hdata = d;
	}


	virtual void on_log( const std::string &text, int type, float time )
	{
		if (!m_hfunc) return;
		(*m_hfunc)( static_cast<ssc_module_t>( module() ), 
					static_cast<ssc_handler_t>( static_cast<handler_interface*>(this) ), 
					SSC_LOG, (float)type, time, text.c_str(), 0, m_hdata );
	}

	virtual bool on_update( const std::string &text, float percent, float time )
	{
		if (!m_hfunc) return true;
		
		return (*m_hfunc)( static_cast<ssc_module_t>( module() ),
					static_cast<ssc_handler_t>( static_cast<handler_interface*>(this) ), 
					SSC_UPDATE, percent, time, text.c_str(), 0, m_hdata ) ? 1 : 0;
	}
};

SSCEXPORT ssc_bool_t ssc_module_exec_with_handler( 
	ssc_module_t p_mod, 
	ssc_data_t p_data, 
	ssc_bool_t (*pf_handler)( ssc_module_t, ssc_handler_t, int, float, float, const char*, const char *, void * ),
	void *pf_user_data )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!cm) return 0;

	var_table *vt = static_cast<var_table*>(p_data);
	if (!vt)
	{
		cm->log("invalid data object provided", SSC_ERROR);
		return 0;
	}
	
	default_exec_handler h( cm, pf_handler, pf_user_data );
	return cm->compute( &h, vt ) ? 1 : 0;
}


SSCEXPORT void ssc_module_extproc_output( ssc_handler_t p_handler, const char *output_line )
{
	handler_interface *hi = static_cast<handler_interface*>( p_handler );
	if (hi)	hi->on_stdout( output_line );
}

SSCEXPORT const char *ssc_module_log( ssc_module_t p_mod, int index, int *item_type, float *time )
{
	compute_module *cm = static_cast<compute_module*>(p_mod);
	if (!p_mod) return 0;

	compute_module::log_item *l = cm->log(index);
	if (!l) return 0;

	if (item_type) *item_type = l->type;
	if (time) *time = l->time;

	return l->text.c_str();
}

SSCEXPORT void __ssc_segfault()
{
	std::string *pstr = 0;
	std::string mystr = *pstr;
}
