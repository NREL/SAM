#include "core.h"
#include "htf_props.h"
#include "sam_csp_util.h"
#include "csp_solver_two_tank_tes.h"

static var_info _cm_vtab_ui_tes_calcs[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                            UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,   SSC_NUMBER,   "W_dot_pb_des",       "Power cycle output at design",                 "MWe",   "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "eta_pb_des",         "Power cycle thermal efficiency",               "",      "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "tes_hrs",            "Hours of TES relative to q_dot_pb_des",        "hr",    "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "T_HTF_hot",          "Hot HTF temp (into TES HX, if applicable)",    "C",     "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "T_HTF_cold",         "Cold HTF temp (out of TES HX, if applicable)", "C",     "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "TES_HTF_code",       "TES storage fluid code",                       "",      "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_MATRIX,   "TES_HTF_props",      "User defined tes storage fluid prop data",     "",      "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "h_tank_min",         "Min. allowable HTF height in storage tank",    "m",     "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "h_tank",             "Total height of tank (HTF when tank is full",  "m",     "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "tank_pairs",         "Number of equivalent tank pairs",              "",      "", "",  "*",  "", "" },
	{ SSC_INPUT,   SSC_NUMBER,   "u_tank",             "Loss coefficient from the tank",               "W/m2-K","", "",  "*",  "", "" },

	{ SSC_OUTPUT,  SSC_NUMBER,   "q_tes_des",          "TES thermal capacity at design",               "MWt-hr","", "",  "*",  "", "" },
	{ SSC_OUTPUT,  SSC_NUMBER,   "vol_one_temp_avail", "Available single temp storage volume",         "m^3",   "", "",  "*",  "", "" },
	{ SSC_OUTPUT,  SSC_NUMBER,   "vol_one_temp_total", "Total single temp storage volume",             "m^3",   "", "",  "*",  "", "" },
	{ SSC_OUTPUT,  SSC_NUMBER,   "d_tank",             "Single tank diameter",                         "m",     "", "",  "*",  "", "" },
	{ SSC_OUTPUT,  SSC_NUMBER,   "q_dot_loss",         "Estimated tank heat loss to env.",             "MWt",   "", "",  "*",  "", "" },
	{ SSC_OUTPUT,  SSC_NUMBER,   "HTF_dens",           "HTF dens",                                     "kg/m^3","", "",  "*",  "", "" },

	var_info_invalid};

class cm_ui_tes_calcs : public compute_module
{
public:

	cm_ui_tes_calcs()
	{
		add_var_info(_cm_vtab_ui_tes_calcs);
	}

	void exec() throw(general_error)
	{
		double W_dot_pb_des = as_double("W_dot_pb_des");		//[MWe] Power cycle output at design
		double eta_pb_des = as_double("eta_pb_des");            //[-] Power cycle efficiency at design 
		double q_dot_pb_des = W_dot_pb_des / eta_pb_des;		//[MWt] Power cycle thermal power at design

		double tes_hrs = as_double("tes_hrs");                  //[hrs] Hours of TES relative to q_dot_pb_des
		double Q_tes_des = q_dot_pb_des*tes_hrs;                //[MWt-hr] TES thermal capacity at design
		assign("q_tes_des", Q_tes_des);

		// Initialize HTF class
		HTFProperties tes_htf_props;			// Instance of HTFProperties class for TES HTF
		int tes_fl = (int) as_double("TES_HTF_code");
		util::matrix_t<double> tes_fl_props = as_matrix("TES_HTF_props");

		double T_HTF_hot = as_double("T_HTF_hot");			//[C] Hot HTF temp
		double T_HTF_cold = as_double("T_HTF_cold");		//[C] Cold HTF temp
		double T_HTF_ave = 0.5*(T_HTF_hot + T_HTF_cold);	//[C] Ave HTF temp at design

		// Set fluid number and copy over fluid matrix if it makes sense.
		if( tes_fl != HTFProperties::User_defined && tes_fl < HTFProperties::End_Library_Fluids )
		{
			if( !tes_htf_props.SetFluid(tes_fl) )
			{
				throw exec_error("ui_tes_calcs", util::format("The user-defined HTF did not read correctly"));
			}
		}
		else if( tes_fl == HTFProperties::User_defined )
		{
			int n_rows = tes_fl_props.nrows();
			int n_cols = tes_fl_props.ncols();
			if( n_rows > 2 && n_cols == 7 )
			{
				if( !tes_htf_props.SetUserDefinedFluid(tes_fl_props) )
				{
					std::string error_msg = util::format(tes_htf_props.UserFluidErrMessage(), n_rows, n_cols);
					throw exec_error("ui_tes_calcs", error_msg);
				}
			}
			else
			{
				std::string error_msg = util::format("The user defined storage HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
				throw exec_error("ui_tes_calcs", error_msg);
			}
		}
		else
		{
			throw exec_error("ui_tes_calcs", "Storage HTF code is not recognized");
		}

		double h_min = as_double("h_tank_min");			//[m]
		double h_tank = as_double("h_tank");			//[m]
		double tank_pairs = as_double("tank_pairs");	//[-]
		double u_tank = as_double("u_tank");			//[W/m^2-K]

		double vol_one_temp_avail, vol_one_temp_total, d_tank, q_dot_loss_des;
		vol_one_temp_avail = vol_one_temp_total = d_tank = q_dot_loss_des = std::numeric_limits<double>::quiet_NaN();
		two_tank_tes_sizing(tes_htf_props, Q_tes_des, T_HTF_hot+273.15, T_HTF_cold+273.15, 
			h_min, h_tank, tank_pairs, u_tank,
			vol_one_temp_avail, vol_one_temp_total, d_tank, q_dot_loss_des);

		assign("vol_one_temp_avail", vol_one_temp_avail);
		assign("vol_one_temp_total", vol_one_temp_total);
		assign("d_tank", d_tank);
		assign("q_dot_loss", q_dot_loss_des);
		assign("HTF_dens", tes_htf_props.dens(T_HTF_ave+273.15,1.0));

		//double rho_ave = tes_htf_props.dens(T_HTF_ave+273.15, 1.0);		//[kg/m^3] Density at average temperature
		//double cp_ave = tes_htf_props.Cp(T_HTF_ave+273.15);				//[kJ/kg-K] Specific heat at average temperature
		//
		//	//[m^3] = [MJ/s-hr] * [sec]/[hr] = [MJ] / (kg/m^3 * MJ/kg-K * K 
		//double vol_one_temp_avail = Q_tes_des*3600.0 / (rho_ave * cp_ave/1000.0 * (T_HTF_hot - T_HTF_cold));
		//assign("vol_one_temp_avail", vol_one_temp_avail);
		//
		//double h_min = as_double("h_tank_min");
		//double h_tank = as_double("h_tank");
		//
		//double vol_one_temp_total = vol_one_temp_avail / (1.0 - h_min/h_tank);	//[m^3]
		//assign("vol_one_temp_total", vol_one_temp_total);
		//
		//double tank_pairs = as_double("tank_pairs");
		//
		//double A_cs = vol_one_temp_total / (h_tank*tank_pairs);		//[m^2] Cross-sectional area of a single tank
		//
		//double diameter = pow(A_cs / CSP::pi, 0.5)*2.0;			//[m] Diameter of a single tank
		////assign("d_tank", diameter);
		//assign("d_tank", 1234.5);
		//
		//double u_tank = as_double("u_tank");	//[W/m^2-K]
		//
		//double UA = u_tank*(A_cs + CSP::pi*diameter*h_tank)*tank_pairs;		//[W/K]
		//double q_dot_loss = UA*(T_HTF_ave - 15.0)*1.E-6;	//[MWt]
		//
		//assign("q_dot_loss",q_dot_loss);
		
		return;
	}
};

DEFINE_MODULE_ENTRY(ui_tes_calcs, "Calculates values for all calculated values on UI TES page(s)", 0)