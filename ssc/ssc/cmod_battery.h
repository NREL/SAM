#ifndef _CMOD_BATTERY_COMMON_
#define _CMOD_BATTERY_COMMON_ 1

#include "core.h"
#include "lib_power_electronics.h"
#include "lib_sandia.h"
#include "lib_pvinv.h"


extern var_info vtab_battery_inputs[];
extern var_info vtab_battery_outputs[];

struct batt_variables
{
	batt_variables()
	{
		pcharge = pdischarge = pdischarge = pgridcharge = pdischarge_percent = pgridcharge_percent = psched = psched_weekend = 0;
	};

	bool system_use_lifetime_output;
	bool en_batt;
	int analysis_period;
	int batt_chem;
	int batt_dispatch;
	int batt_voltage_choice;
	int batt_meter_position;
	int batt_pv_choice;
	int batt_target_choice;
	int batt_loss_choice;

	size_t ncharge;
	size_t ndischarge;
	size_t ndischarge_percent;
	size_t ngridcharge_percent;
	size_t ngridcharge;
	size_t nsched;
	size_t msched;

	ssc_number_t *pcharge;
	ssc_number_t *pdischarge;
	ssc_number_t *pdischarge_percent;
	ssc_number_t *pgridcharge_percent;
	ssc_number_t *pgridcharge;
	ssc_number_t *psched;
	ssc_number_t *psched_weekend;

	util::matrix_t<float> schedule;
	util::matrix_t<double>  batt_lifetime_matrix;
	util::matrix_t<double> batt_voltage_matrix;

	std::vector<double> target_power_monthly;
	std::vector<double> target_power;

	std::vector<double> batt_losses_monthly;
	std::vector<double> batt_losses;

	int batt_computed_series;
	int batt_computed_strings;
	double batt_kw;
	double batt_kwh;

	double batt_Vnom_default;
	double batt_Vfull;
	double batt_Vexp;
	double batt_Vnom;
	double batt_Qfull;
	double batt_Qexp;
	double batt_Qnom;
	double batt_C_rate;
	double batt_resistance;

	double batt_replacement_capacity;
	util::matrix_t<double> cap_vs_temp;
	double batt_mass;
	double batt_length;
	double batt_width;
	double batt_height;
	double batt_Cp;
	double batt_h_to_ambient;
	double T_room;

	double LeadAcid_q20_computed;
	double LeadAcid_tn;
	double LeadAcid_qn_computed;
	double LeadAcid_q10_computed;

	double batt_maximum_SOC;
	double batt_minimum_SOC;
	double batt_current_charge_max;
	double batt_current_discharge_max;
	double batt_minimum_modetime;

	int batt_topology;
	double batt_ac_dc_efficiency;
	double batt_dc_ac_efficiency;
	double batt_dc_dc_bms_efficiency;
	double pv_dc_dc_mppt_efficiency;

	int inverter_model;
	double inv_snl_eff_cec;
	double inv_cec_cg_eff_cec;
	double inv_ds_eff;
	double inv_pd_eff;
	double inverter_efficiency;
};


struct battstor
{

	battstor( compute_module &cm, bool setup_model, int replacement_option, size_t nrec, double dt_hr, batt_variables *batt_vars=0);
	void initialize_automated_dispatch(ssc_number_t *pv=0, ssc_number_t *load=0);
	~battstor();

	void advance(compute_module &cm, size_t year, size_t hour_of_year, size_t step, double P_pv, double P_load);
	void outputs_fixed(compute_module &cm, size_t year, size_t hour_of_year, size_t step);
	void outputs_topology_dependent(compute_module &cm, size_t year, size_t hour_of_year, size_t step);
	void metrics(compute_module &cm, size_t year, size_t hour_of_year, size_t step);
	void update_post_inverted(compute_module &cm, size_t year, size_t hour_of_year, size_t step, double P_gen_ac);
	bool check_iterate(size_t count);
	void process_messages(compute_module &cm);

	// for user schedule
	void force_replacement();
	void check_replacement_schedule(int batt_replacement_option, size_t count_batt_replacement, ssc_number_t *batt_replacement, int iyear, int hour, int step);
	void calculate_monthly_and_annual_outputs( compute_module &cm );


	// time quantities
	int year;
	size_t step_per_hour;
	size_t nyears;
	size_t total_steps;
	double _dt_hour;

	// member data
	voltage_t *voltage_model;
	lifetime_t *lifetime_model;
	thermal_t *thermal_model;
	capacity_t *capacity_model;
	battery_t *battery_model;
	battery_metrics_t *battery_metrics;
	dispatch_manual_t *dispatch_model;
	losses_t *losses_model;
	charge_controller *charge_control;

	bool en;
	int chem;

	batt_variables * batt_vars;
	bool make_vars;
	
	bool dm_charge[6], dm_discharge[6], dm_gridcharge[6]; // manual dispatch
	std::map<int, double> dm_percent_discharge; // <profile, discharge_percent>
	std::map<int, double> dm_percent_gridcharge; // <profile, gridcharge_percent>
	util::matrix_t<float> dm_dynamic_sched;
	util::matrix_t<float> dm_dynamic_sched_weekend;
	std::vector<double> target_power;
	std::vector<double> target_power_monthly;
	
	int topology;
	double dc_dc, ac_dc, dc_ac;

	double e_charge;
	double e_discharge;

	std::vector<double> pv_prediction;
	std::vector<double> load_prediction;
	int prediction_index;

	// outputs
	ssc_number_t
		*outTotalCharge,
		*outAvailableCharge,
		*outBoundCharge,
		*outMaxChargeAtCurrent,
		*outMaxCharge,
		*outSOC,
		*outDOD,
		*outCurrent,
		*outCellVoltage,
		*outBatteryVoltage,
		*outCapacityPercent,
		*outCycles,
		*outBatteryBankReplacement,
		*outBatteryTemperature,
		*outCapacityThermalPercent,
		*outDispatchMode,
		*outBatteryPower,
		*outGenPower,
		*outGridPower,
		*outPVToLoad,
		*outBatteryToLoad,
		*outGridToLoad,
		*outGridPowerTarget,
		*outPVToBatt,
		*outGridToBatt,
		*outPVToGrid,
		*outBatteryToGrid,
		*outBatteryConversionPowerLoss,
		*outBatterySystemLoss,
		*outAnnualPVChargeEnergy,
		*outAnnualGridChargeEnergy,
		*outAnnualChargeEnergy,
		*outAnnualDischargeEnergy,
		*outAnnualGridImportEnergy,
		*outAnnualGridExportEnergy,
		*outAnnualEnergyLoss;

	double outAverageCycleEfficiency;
	double outPVChargePercent;
};


#endif
