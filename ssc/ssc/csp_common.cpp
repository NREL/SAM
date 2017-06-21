#include "csp_common.h"
#include "core.h"
#include "lib_weatherfile.h"
#include "lib_util.h"
#include <sstream>

// solarpilot header files
#include "AutoPilot_API.h"
#include "SolarField.h"
#include "IOUtil.h"

#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

//static bool solarpilot_callback( simulation_info *siminfo, void *data );
static bool optimize_callback( simulation_info *siminfo, void *data );

solarpilot_invoke::solarpilot_invoke(compute_module *cm)
{
    m_cmod = cm;
    //anything else?
    m_sapi = 0;

}

solarpilot_invoke::~solarpilot_invoke()
{
    if(m_sapi != 0)
        delete m_sapi;
}

AutoPilot_S *solarpilot_invoke::GetSAPI()
{
    return m_sapi;
}

bool solarpilot_invoke::run()
{
    /* 
    
    */
    if(m_sapi != 0)
        delete m_sapi;

    m_sapi = new AutoPilot_S();

	// read inputs from SSC module
		
    //fin.is_pmt_factors.val = true;
    //testing <<<


	bool isopt = m_cmod->as_boolean( "is_optimize" );
    if(isopt)
    {
		//opt.flux_max = m_cmod->as_double("flux_max");
        recs.front().peak_flux.val = m_cmod->as_double("flux_max");
        opt.max_step.val = m_cmod->as_double("opt_init_step");
        opt.max_iter.val = m_cmod->as_integer("opt_max_iter");
        opt.converge_tol.val = m_cmod->as_double("opt_conv_tol");
        opt.algorithm.combo_select_by_mapval( m_cmod->as_integer("opt_algorithm") ); //map correctly?
        opt.flux_penalty.val = m_cmod->as_double("opt_flux_penalty");
    }

    var_heliostat *hf = &hels.front();
    //need to set up the template combo
    sf.temp_which.combo_clear();
    std::string name = "Template 1", val = "0";
    sf.temp_which.combo_add_choice(name, val);
    sf.temp_which.combo_select_by_choice_index( 0 ); //use the first heliostat template

	hf->width.val = m_cmod->as_double("helio_width");
	hf->height.val = m_cmod->as_double("helio_height");
    hf->err_azimuth.val = hf->err_elevation.val = hf->err_reflect_x.val = hf->err_reflect_y.val = 0.;   //all other error =0
    hf->err_surface_x.val = hf->err_surface_y.val = m_cmod->as_double("helio_optical_error");       //slope error
    hf->soiling.val = 1.;   //reflectivity is the only consideration for this model

	hf->reflect_ratio.val = m_cmod->as_double("helio_active_fraction") * m_cmod->as_double("dens_mirror");
	hf->reflectivity.val = m_cmod->as_double("helio_reflectance");
	hf->n_cant_x.val = m_cmod->as_integer("n_facet_x");
	hf->n_cant_y.val = m_cmod->as_integer("n_facet_y");

    std:string cant_choices[] = {"No canting","On-axis at slant","On-axis, user-defined","Off-axis, day and hour","User-defined vector"};

	int cmap[5];
    cmap[0] = var_heliostat::CANT_METHOD::NO_CANTING;
    cmap[1] = var_heliostat::CANT_METHOD::ONAXIS_AT_SLANT;
    cmap[2] = cmap[3] = cmap[4] = var_heliostat::CANT_METHOD::OFFAXIS_DAY_AND_HOUR;

	int cant_type = m_cmod->as_integer("cant_type");

	//hf->cant_method.val = cmap[ cant_type ];       //Convert to the Heliostat::CANT_METHOD list
    //hf->cant_method.combo_select( cant_choices[cmap[cant_type]] );
    hf->cant_method.combo_select( cant_choices[cant_type] );
    switch (cant_type)
{
    case AutoPilot::API_CANT_TYPE::NONE:
    case AutoPilot::API_CANT_TYPE::ON_AXIS:
        //do nothing
        break;
    case AutoPilot::API_CANT_TYPE::EQUINOX:
        hf->cant_day.val = 81;  //spring equinox
	    hf->cant_hour.val = 12;
        break;
    case AutoPilot::API_CANT_TYPE::SOLSTICE_SUMMER:
        hf->cant_day.val = 172;  //Summer solstice
	    hf->cant_hour.val = 12;
        break;
    case AutoPilot::API_CANT_TYPE::SOLSTICE_WINTER:
        hf->cant_day.val = 355;  //Winter solstice
	    hf->cant_hour.val = 12;
        break;
    default:
    {
        stringstream msg;
        msg << "Invalid Cant Type specified in AutoPILOT API. Method must be one of: \n" <<
               "NONE(0), ON_AXIS(1), EQUINOX(2), SOLSTICE_SUMMER(3), SOLSTICE_WINTER(4).\n" <<
               "Method specified is: " << cant_type << ".";
        throw spexception(msg.str());
    }
        break;
    }


    hf->focus_method.combo_select_by_choice_index( m_cmod->as_integer("focus_type") );

    var_receiver *rf = &recs.front();

	rf->absorptance.val = m_cmod->as_double("rec_absorptance");
	rf->rec_height.val = m_cmod->as_double("rec_height");
	rf->rec_width.val = rf->rec_diameter.val = rf->rec_height.val/m_cmod->as_double("rec_aspect"); 
	rf->therm_loss_base.val = m_cmod->as_double("rec_hl_perm2");
		
    sf.q_des.val = m_cmod->as_double("q_design");
	sf.dni_des.val = m_cmod->as_double("dni_des");
    land.is_bounds_scaled.val = true;
    land.is_bounds_fixed.val = false;
    land.is_bounds_array.val = false;
	land.max_scaled_rad.val = m_cmod->as_double("land_max");
	land.min_scaled_rad.val = m_cmod->as_double("land_min");
	sf.tht.val = m_cmod->as_double("h_tower");
		
	fin.tower_fixed_cost.val = m_cmod->as_double("tower_fixed_cost");
	fin.tower_exp.val = m_cmod->as_double("tower_exp");
	fin.rec_ref_cost.val = m_cmod->as_double("rec_ref_cost");
	fin.rec_ref_area.val = m_cmod->as_double("rec_ref_area");
	fin.rec_cost_exp.val = m_cmod->as_double("rec_cost_exp");
	fin.site_spec_cost.val = m_cmod->as_double("site_spec_cost");
	fin.heliostat_spec_cost.val = m_cmod->as_double("heliostat_spec_cost");
	fin.plant_spec_cost.val = m_cmod->as_double("plant_spec_cost") + m_cmod->as_double("bop_spec_cost");
	fin.tes_spec_cost.val = m_cmod->as_double("tes_spec_cost");
	fin.land_spec_cost.val = m_cmod->as_double("land_spec_cost");
	fin.contingency_rate.val = m_cmod->as_double("contingency_rate");
	fin.sales_tax_rate.val = m_cmod->as_double("sales_tax_rate");
	fin.sales_tax_frac.val = m_cmod->as_double("sales_tax_frac");
	fin.fixed_cost.val = m_cmod->as_double("cost_sf_fixed");
    ////update financial tables
    //fin.weekday_sched.val = m_cmod->value("dispatch_sched_weekday").str;
    //fin.weekend_sched.val = m_cmod->value("dispatch_sched_weekend").str;
    //std::string ps;
    //for(int i=0; i<9; i++)
    //    ps.append( m_cmod->as_double("dispatch_factor" + my_to_string(i+1)) + i < 8 ? "," : "" );
    //fin.pricing_array.Val().clear();
    //fin.pricing_array.set_from_string( ps.c_str() );
    
	
	//set up the weather data for simulation
	const char *wffile = m_cmod->as_string("solar_resource_file" );
	if ( !wffile ) throw compute_module::exec_error( "solarpilot", "no weather file specified" );
	weatherfile wFile( wffile );
	if ( !wFile.ok() || wFile.type() == weatherfile::INVALID ) throw compute_module::exec_error("solarpilot", wFile.message());

	weather_header hdr;
	wFile.header( &hdr );
		
    amb.latitude.val = hdr.lat;
	amb.longitude.val = hdr.lon;
	amb.time_zone.val = hdr.tz;
	amb.atm_model.combo_select_by_choice_index(2); //USER_DEFINED
	
	amb.atm_coefs.val.at(2,0) = m_cmod->as_double("c_atm_0");
    amb.atm_coefs.val.at(2,1) = m_cmod->as_double("c_atm_1");
    amb.atm_coefs.val.at(2,2) = m_cmod->as_double("c_atm_2");
    amb.atm_coefs.val.at(2,3) = m_cmod->as_double("c_atm_3");

    if(! m_cmod->is_assigned("helio_positions_in") ) 
    {

	    weather_record wf;

	    vector<string> wfdata;
	    wfdata.reserve( 8760 );
	    char buf[1024];
	    for( int i=0;i<8760;i++ )
	    {
		    if( !wFile.read( &wf ) )
			    throw compute_module::exec_error("solarpilot", "could not read data line " + util::to_string(i+1) + " of 8760 in weather file");

		    mysnprintf(buf, 1023, "%d,%d,%d,%.2lf,%.1lf,%.1lf,%.1lf", wf.day, wf.hour, wf.month, wf.dn, wf.tdry, wf.pres/1000., wf.wspd);
		    wfdata.push_back( std::string(buf) );
	    }

	    m_sapi->SetDetailCallback( ssc_cmod_solarpilot_callback, m_cmod);
	    m_sapi->SetSummaryCallbackStatus(false);

	    m_sapi->GenerateDesignPointSimulations( *this, wfdata );
	
        if(isopt){
            m_cmod->log("Optimizing...", SSC_WARNING, 0.);
            m_sapi->SetSummaryCallback( optimize_callback, m_cmod);
		    m_sapi->Setup(*this, true);
            
            //set up optimization variables
            {
                int nv = 3;
                vector<double*> optvars(nv);
                vector<double> upper(nv, HUGE_VAL);
                vector<double> lower(nv, -HUGE_VAL);
                vector<double> stepsize(nv);
                vector<string> names(nv);

                //pointers
                optvars.at(0) = &sf.tht.val;
                optvars.at(1) = &recs.front().rec_height.val;
                optvars.at(2) = &recs.front().rec_diameter.val;
                //names
                names.at(0) = (split(sf.tht.name, ".")).back();
                names.at(1) = (split(recs.front().rec_height.name, ".")).back();
                names.at(2) = (split(recs.front().rec_diameter.name, ".")).back();
                //step size
                stepsize.at(0) = sf.tht.val*opt.max_step.val;
                stepsize.at(1) = recs.front().rec_height.val*opt.max_step.val;
                stepsize.at(2) = recs.front().rec_diameter.val*opt.max_step.val;

                if(! m_sapi->Optimize(opt.algorithm.mapval(), optvars, upper, lower, stepsize, &names) )
                    return false;
            }

            m_sapi->SetSummaryCallbackStatus(false);
            m_sapi->PreSimCallbackUpdate();
            
        }
        else{
		    m_sapi->Setup(*this);
        }
        if(! m_sapi->CreateLayout(layout) )
            return false;

    }
    else
    {
        /* 
		Load in the heliostat field positions that are provided by the user.
		*/
		//layout.heliostat_positions.clear();
		//layout.heliostat_positions.resize(m_N_hel);
		string format = "0,%f,%f,%f,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL;";
        sf.layout_data.val.clear();

        util::matrix_t<double> hpos = m_cmod->as_matrix("helio_positions_in");

        char row[200];
		for( int i=0; i<hpos.nrows(); i++)
		{
            sprintf(row, format.c_str(), hpos.at(i,0), hpos.at(i,1),  0. );

            sf.layout_data.val.append( row );
		}

		m_sapi->Setup(*this);
    }
    
    //check if flux map calculations are desired
	if( m_cmod->as_boolean("calc_fluxmaps") ){      // <<--- was set "false" for some reason

		m_sapi->SetDetailCallbackStatus(false);
		m_sapi->SetSummaryCallbackStatus(true);
		m_sapi->SetSummaryCallback( ssc_cmod_solarpilot_callback, m_cmod );
		
	
		//sp_optical_table opttab;
		fluxtab.is_user_spacing = true;
		fluxtab.n_flux_days = m_cmod->as_integer("n_flux_days");
		fluxtab.delta_flux_hrs = m_cmod->as_integer("delta_flux_hrs");
		
        string aim_method_save = flux.aim_method.val;
        flux.aim_method.combo_select( "Simple aim points" );

		int nflux_x = 12, nflux_y = 1;
		if(! m_sapi->CalculateFluxMaps(fluxtab, nflux_x, nflux_y, true) )
        {
            flux.aim_method.combo_select( aim_method_save );
            return false;  //simulation failed or was cancelled.
        }
        flux.aim_method.combo_select( aim_method_save );


		//collect the optical efficiency data and sun positions
		if ( fluxtab.zeniths.size() == 0 || fluxtab.azimuths.size() == 0
			|| fluxtab.efficiency.size() == 0 )
			throw compute_module::exec_error("solarpilot", "failed to calculate a correct optical efficiency table");
		
		//collect the flux map data
		block_t<double> *flux_data = &fluxtab.flux_surfaces.front().flux_data;  //there should be only one flux stack for SAM
		if( flux_data->ncols() == 0 || flux_data->nlayers() == 0 )
			throw compute_module::exec_error("solarpilot", "failed to calculate a correct flux map table");
	}

    //check if max flux check is desired
    if( m_cmod->as_boolean("check_max_flux") )
    {
        m_sapi->SetDetailCallbackStatus(false);
		m_sapi->SetSummaryCallbackStatus(true);
		m_sapi->SetSummaryCallback( ssc_cmod_solarpilot_callback, m_cmod );
		
	    sp_flux_table flux_temp;

		//sp_optical_table opttab;
		flux_temp.is_user_spacing = false;
        flux_temp.azimuths.clear();
        flux_temp.zeniths.clear();

        flux_temp.azimuths.push_back( (flux.flux_solar_az.Val())*D2R );
        flux_temp.zeniths.push_back( (90.-flux.flux_solar_el.Val())*D2R );

		
		if(! m_sapi->CalculateFluxMaps(flux_temp, 20, 15, false) )
            return false;  //simulation failed or was cancelled.
            
        
		block_t<double> *flux_data = &flux_temp.flux_surfaces.front().flux_data;  //there should be only one flux stack for SAM
        
        double flux_max_observed = 0.;

        for(int i=0; i<flux_data->nrows(); i++)
        {
            for(int j=0; j<flux_data->ncols(); j++)
            {
                if( flux_data->at(i, j, 0) > flux_max_observed ) 
                    flux_max_observed = flux_data->at(i, j, 0);
            }
        }

        m_cmod->assign("flux_max_observed", flux_max_observed);
    }
        
    return true;
}

bool solarpilot_invoke::postsim_calcs(compute_module *cm)
{
    /* 
    Update calculated values and cost model number to be used in subsequent simulation and analysis.

    The variable values used in this are consistent with the solarpilot compute module. These same variables are used in all 
    tower modules that use solarpilot API.

    */


    //receiver calculations
    double H_rec = recs.front().rec_height.val;
    double rec_aspect = recs.front().rec_aspect.Val();
    double THT = sf.tht.val;
    //update heliostat position table
    int nr = (int)heliotab.positions.size();
    ssc_number_t *ssc_hl = cm->allocate( "helio_positions", nr, 2 );
    for(int i=0; i<nr; i++){
        ssc_hl[i*2] = (ssc_number_t)layout.heliostat_positions.at(i).location.x;
        ssc_hl[i*2+1] = (ssc_number_t)layout.heliostat_positions.at(i).location.y;
    }

    double A_sf = cm->as_double("helio_height") * cm->as_double("helio_width") * cm->as_double("dens_mirror") * (double)nr;

    //update piping length for parasitic calculation
    double piping_length = THT * cm->as_double("csp.pt.par.piping_length_mult") + cm->as_double("csp.pt.par.piping_length_const");
            
    //update assignments for cost model
	cm->assign("H_rec", var_data((ssc_number_t)H_rec));
    cm->assign("rec_height", var_data((ssc_number_t)H_rec));
	cm->assign("rec_aspect", var_data((ssc_number_t)rec_aspect));
    cm->assign("D_rec", var_data((ssc_number_t)(H_rec/rec_aspect)));
	cm->assign("THT", var_data((ssc_number_t)THT));
    cm->assign("h_tower", var_data((ssc_number_t)THT));
	cm->assign("A_sf", var_data((ssc_number_t)A_sf));
    cm->assign("Piping_length", var_data((ssc_number_t)piping_length) );

    //Update the total installed cost
    double total_direct_cost = 0.;
    double A_rec;
    switch (recs.front().rec_type.mapval())
    {
    case var_receiver::REC_TYPE::EXTERNAL_CYLINDRICAL:
    {
        double h = recs.front().rec_height.val;
        double d = h/recs.front().rec_aspect.Val();
        A_rec =  h*d*3.1415926;
        break;
    }
    //case Receiver::REC_TYPE::CAVITY:
    case var_receiver::REC_TYPE::FLAT_PLATE:
        double h = recs.front().rec_height.val;
        double w = h/recs.front().rec_aspect.Val();
        A_rec = h*w;
        break;
    }
    double receiver = cm->as_double("rec_ref_cost")*pow(A_rec/cm->as_double("rec_ref_area"), cm->as_double("rec_cost_exp"));     //receiver cost

    //storage cost
    double storage = cm->as_double("q_pb_design")*cm->as_double("tshours")*cm->as_double("tes_spec_cost")*1000.;

    //power block + BOP
    double P_ref = cm->as_double("P_ref") * 1000.;  //kWe
    double power_block = P_ref * (cm->as_double("plant_spec_cost") + cm->as_double("bop_spec_cost") ); //$/kWe --> $

    //site improvements
    double site_improvements = A_sf * cm->as_double("site_spec_cost");
            
    //heliostats
    double heliostats = A_sf * cm->as_double("heliostat_spec_cost");
            
    //fixed cost
    double cost_fixed = cm->as_double("cost_sf_fixed");

    //fossil
    double fossil = P_ref * cm->as_double("fossil_spec_cost");

    //tower cost
    double tower = cm->as_double("tower_fixed_cost") * exp( cm->as_double("tower_exp") * (THT + 0.5*(-H_rec + cm->as_double("helio_height")) ) );

    //---- total direct cost -----
    total_direct_cost = (1. + cm->as_double("contingency_rate")/100.) * (
        site_improvements + heliostats + power_block + 
        cost_fixed + storage + fossil + tower + receiver);
    //-----

    //land area
    double land_area = land.land_area.Val() * cm->as_double("csp.pt.sf.land_overhead_factor") + cm->as_double("csp.pt.sf.fixed_land_area");

    //EPC
    double cost_epc = 
        cm->as_double("csp.pt.cost.epc.per_acre") * land_area
        + cm->as_double("csp.pt.cost.epc.percent") * total_direct_cost / 100.
        + P_ref * 1000. * cm->as_double("csp.pt.cost.epc.per_watt") 
        + cm->as_double("csp.pt.cost.epc.fixed");

    //PLM
    double cost_plm = 
        cm->as_double("csp.pt.cost.plm.per_acre") * land_area
        + cm->as_double("csp.pt.cost.plm.percent") * total_direct_cost / 100.
        + P_ref * 1000. * cm->as_double("csp.pt.cost.plm.per_watt") 
        + cm->as_double("csp.pt.cost.plm.fixed");

    //sales tax
    //return ${csp.pt.cost.sales_tax.value}/100*${total_direct_cost}*${csp.pt.cost.sales_tax.percent}/100; };
    double cost_sales_tax = cm->as_double("sales_tax_rate")/100. * total_direct_cost * cm->as_double("sales_tax_frac")/100.;

    //----- indirect cost
    double total_indirect_cost = cost_epc + cost_plm + cost_sales_tax;
            
    //----- total installed cost!
    double total_installed_cost = total_direct_cost + total_indirect_cost;
    cm->assign("total_installed_cost", var_data((ssc_number_t)total_installed_cost ));

    return true;

}


void solarpilot_invoke::getOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values)
{
	/* 
	Return the addresses of the optimization simulation history data, if applicable.
	*/
	sim_points = _optimization_sim_points;
	obj_values = _optimization_objectives;
	flux_values = _optimization_fluxes;
}

void solarpilot_invoke::setOptimizationSimulationHistory(vector<vector<double> > &sim_points, vector<double> &obj_values, vector<double> &flux_values)
{
	//Create local copies
	_optimization_sim_points = sim_points;
	_optimization_objectives = obj_values;
	_optimization_fluxes = flux_values;
}


bool ssc_cmod_solarpilot_callback( simulation_info *siminfo, void *data )
{
	compute_module *cm = static_cast<compute_module*>( data );
	if ( !cm ) return false;
	float simprogress = (float)siminfo->getCurrentSimulation()/(float)(max(siminfo->getTotalSimulationCount(),1));

	return cm->update( *siminfo->getSimulationNotices(),
		simprogress*100.0f );

}

static bool optimize_callback( simulation_info *siminfo, void *data )
{
    compute_module *cm = static_cast<compute_module*>( data );
    if(! cm) return false;
    
    std::string notices = *siminfo->getSimulationNotices();
    cm->log( notices, SSC_WARNING, 0. );
    
    return true;
}