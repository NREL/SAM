#include "lib_battery.h"

#ifndef __LIB_BATTERY_DISPATCH_H__
#define __LIB_BATTERY_DISPATCH_H__

/*
Dispatch Base Class - can envision many potential modifications. Goal is to define standard API
*/
class dispatch_t
{
public:
	dispatch_t(battery_t * Battery,
		double dt,
		double SOC_min,
		double SOC_max,
		double Ic_max,
		double Id_max,
		double t_min,
		int mode,
		int pv_dispatch);

	// deep copy constructor (new memory), from dispatch to this
	dispatch_t(const dispatch_t& dispatch);

	// copy members from dispatch to this
	virtual void copy(const dispatch_t & dispatch);
	void delete_clone();

	virtual ~dispatch_t();

	// Public APIs
	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging) = 0;

	virtual void compute_grid_net();

	enum MODES{ LOOK_AHEAD, LOOK_BEHIND, MAINTAIN_TARGET, MANUAL };
	enum METERING{ BEHIND, FRONT };
	enum PV_PRIORITY{ MEET_LOAD, CHARGE_BATTERY };

	// Outputs
	double cycle_efficiency();

	// dc powers
	double power_tofrom_battery();
	double power_tofrom_grid();
	double power_gen();
	double power_pv_to_load();
	double power_battery_to_load();
	double power_grid_to_load();
	double power_pv_to_batt();
	double power_grid_to_batt();
	double power_pv_to_grid();
	double power_battery_to_grid();
	virtual double power_grid_target(){
		return 0;
	};

	// control settings
	int pv_dispatch_priority(){ return _pv_dispatch_to_battery_first; }
	double battery_power_to_fill(){ return _Battery->battery_power_to_fill(); }

	message get_messages();

protected:

	// Initialization help
	void init(battery_t * Battery,
		double dt_hour,
		double SOC_min,
		double SOC_max,
		double Ic_max,
		double Id_max,
		double t_min,
		int mode,
		int pv_dispatch);

	// Controllers
	void SOC_controller();
	void energy_controller();
	void switch_controller();
	double current_controller(double battery_voltage);
	void restrict_current(double &I);

	// compute totals
	void compute_battery_state();
	void compute_to_batt();
	void compute_to_load();
	void compute_to_grid();
	void compute_generation();

	battery_t * _Battery;
	battery_t * _Battery_initial;

	double _dt_hour;

	// configuration
	int _mode; // 0 = look ahead, 1 = look behind, 2 = maintain target power, 3 = manual dispatch
	int _pv_dispatch_to_battery_first; // 0 = meet load first, 1 = meet battery first

	// dc power quantities
	double _P_gen;				 // DC
	double _P_tofrom_batt;		 // DC
	double _P_grid;              // DC
	double _P_pv_to_load;		 // DC
	double _P_battery_to_load;   // DC
	double _P_grid_to_load;      // DC
	double _P_pv_to_batt;	     // DC
	double _P_grid_to_batt;      // DC
	double _P_pv_to_grid;		 // DC
	double _P_battery_to_grid;   // DC

	// the actual power inputs chosen based on charging/discharging
	double _P_pv;
	double _P_load;

	// the options of what the PV and load power is that the battery sees depending on scenario
	double _P_pv_charging;
	double _P_pv_discharging;
	double _P_load_charging;
	double _P_load_discharging;

	// Charge & current limits controllers
	double _SOC_min;
	double _SOC_max;
	double _Ic_max;
	double _Id_max;
	double _t_min;
	double _e_max;
	double _percent_discharge;
	double _percent_charge;
	double _P_target;

	// rapid charge change controller
	int _t_at_mode; // [minutes]
	bool _charging;
	bool _prev_charging;
	bool _grid_recharge;

	// messages
	message _message;
};

/*
Manual dispatch class
*/
class dispatch_manual_t : public dispatch_t
{
public:
	dispatch_manual_t(battery_t * Battery,
		double dt_hour,
		double SOC_min,
		double SOC_max,
		double Ic_max,
		double Id_max,
		double t_min,
		int mode,
		bool pv_dispatch,
		util::matrix_t<float> dm_dynamic_sched,
		util::matrix_t<float> dm_dynamic_sched_weekend,
		bool * dm_charge,
		bool *dm_discharge,
		bool * dm_gridcharge,
		std::map<int, double> dm_percent_discharge,
		std::map<int, double> dm_percent_gridcharge);

	// deep copy constructor (new memory), from dispatch to this
	dispatch_manual_t(const dispatch_t& dispatch);

	// copy members from dispatch to this
	virtual void copy(const dispatch_t & dispatch);

	virtual ~dispatch_manual_t(){};
	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging);

protected:

	// Initialization help
	void init(util::matrix_t<float> dm_dynamic_sched,
		util::matrix_t<float> dm_dynamic_sched_weekend,
		bool * dm_charge,
		bool *dm_discharge,
		bool * dm_gridcharge,
		std::map<int, double> dm_percent_discharge,
		std::map<int, double> dm_percent_gridcharge);

	void init_with_vects(
		util::matrix_t<float> dm_dynamic_sched,
		util::matrix_t<float> dm_dynamic_sched_weekend,
		std::vector<bool>,
		std::vector<bool>,
		std::vector<bool>,
		std::map<int, double> dm_percent_discharge,
		std::map<int, double> dm_percent_gridcharge);

	void initialize_dispatch(size_t hour_of_year, size_t step, double P_pv_dc_charging, double P_pv_dc_discharging, double P_load_dc_charging, double P_load_dc_discharging);
	void reset();
	void compute_energy_load_priority(double energy_needed);
	void compute_energy_battery_priority(double energy_needed);
	bool compute_energy_battery_priority_charging(double energy_needed);
	bool check_constraints(double &I, int count);


	util::matrix_t < float > _sched;
	util::matrix_t < float > _sched_weekend;
	std::vector<bool> _charge_array;
	std::vector<bool> _discharge_array;
	std::vector<bool> _gridcharge_array;
	std::map<int, double>  _percent_discharge_array;
	std::map<int, double> _percent_charge_array;
	bool  _can_charge;
	bool  _can_discharge;
	bool  _can_grid_charge;
};
/* Manual dispatch for utility scale (front of meter)*/
class dispatch_manual_front_of_meter_t : public dispatch_manual_t
{
public:
	dispatch_manual_front_of_meter_t(battery_t * Battery,
		double dt_hour,
		double SOC_min,
		double SOC_max,
		double Ic_max,
		double Id_max,
		double t_min,
		int mode,
		bool pv_dispatch,
		util::matrix_t<float> dm_dynamic_sched,
		util::matrix_t<float> dm_dynamic_sched_weekend,
		bool * dm_charge,
		bool *dm_discharge,
		bool * dm_gridcharge,
		std::map<int, double> dm_percent_discharge,
		std::map<int, double> dm_percent_gridcharge);
	~dispatch_manual_front_of_meter_t(){};

	virtual void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging = 0,
		double P_load_dc_discharging = 0);
	void compute_grid_net();

protected:
	void compute_energy_no_load(double energy_needed);
	void compute_to_grid();
};


/*
Automated dispatch classes
*/
class grid_point
{
	// Class to encapsulate the required grid power, hour, and step, i.e
	// grid_point = [grid_power, hour, step]
public:
	grid_point(double grid = 0., int hour = 0, int step = 0) :
		_grid(grid), _hour(hour), _step(step){}
	double Grid() const { return _grid; }
	int Hour() const { return _hour; }
	int Step() const { return _step; }

private:
	double _grid;
	int _hour;
	int _step;
};

struct byGrid
{
	bool operator()(grid_point const  &a, grid_point const &b)
	{
		return a.Grid() > b.Grid();
	}
};
typedef std::vector<grid_point> grid_vec;

/* Automated dispatch*/

class automate_dispatch_t : public dispatch_manual_t
{
public:
	automate_dispatch_t(
		battery_t * Battery,
		double dt_hour,
		double SOC_min,
		double SOC_max,
		double Ic_max,
		double Id_max,
		double t_min,
		int mode,   // 0/1/2
		bool pv_dispatch,
		util::matrix_t<float> dm_dynamic_sched,
		util::matrix_t<float> dm_dynamic_sched_weekend,
		bool * dm_charge,
		bool *dm_discharge,
		bool * dm_gridcharge,
		std::map<int, double> dm_percent_discharge,
		std::map<int, double> dm_percent_gridcharge,
		int nyears
		);

	void dispatch(size_t year,
		size_t hour_of_year,
		size_t step,
		double P_pv_dc_charging,
		double P_pv_dc_discharging,
		double P_load_dc_charging,
		double P_load_dc_discharging);

	void update_pv_load_data(std::vector<double> P_pv_dc, std::vector<double> P_load_dc);
	void set_target_power(std::vector<double> P_target);
	double power_grid_target(){ return _P_target_current; };

protected:
	void update_dispatch(int hour_of_year, int step, int idx);
	int get_mode();
	void initialize(int hour_of_year);
	void check_debug(FILE *&p, bool & debug, int hour_of_year, int idx);
	void sort_grid(FILE *p, bool debug, int idx);
	void compute_energy(FILE *p, bool debug, double & E_max);
	void target_power(FILE*p, bool debug, double E_max, int idx);
	void set_charge(int profile);
	int set_discharge(FILE *p, bool debug, int hour_of_year, double E_max);
	void set_gridcharge(FILE *p, bool debug, int hour_of_year, int profile, double E_max);
	void check_new_month(int hour_of_year, int step);

	double_vec _P_pv_dc;		// Full time-series of pv [kW]
	double_vec _P_load_dc;      // Full time-series of loads [kW]
	double_vec _P_target_input; // Full time-series of target power [kW]

	double_vec _P_target_use;   // 24 hours * steps_per_hour of target powers [kW]
	double _P_target_month;	    // The target grid power for the month [kW]
	double _P_target_current;	// The current grid power target [kW]

	int _day_index;				// The index of the current day (hour * steps_per_hour + step)
	int _month;					// [0-11]
	int _hour_last_updated;
	double _dt_hour;
	int _steps_per_hour;
	int _num_steps;
	int _nyears;
	int _mode;
	double _safety_factor;
	
	grid_vec grid; // [P_grid, hour, step]
};

// battery_metrics (report as AC or DC energy quanitities)
class battery_metrics_t
{
public:
	battery_metrics_t(battery_t * Battery, double dt_hour);
	~battery_metrics_t(){};

	void compute_metrics_ac(double P_tofrom_batt, double P_pv_to_batt, double P_grid_to_batt, double P_tofrom_grid);
	void compute_metrics_dc(dispatch_t * dispatch);
	void compute_annual_loss();

	void accumulate_energy_charge(double P_tofrom_batt);
	void accumulate_energy_discharge(double P_tofrom_batt);
	void accumulate_battery_charge_components(double P_tofrom_batt, double P_pv_to_batt, double P_grid_to_batt);
	void accumulate_grid_annual(double P_tofrom_grid);
	void new_year();


	// outputs
	double energy_pv_charge_annual();
	double energy_grid_charge_annual();
	double energy_charge_annual();
	double energy_discharge_annual();
	double energy_grid_import_annual();
	double energy_grid_export_annual();
	double energy_loss_annual();
	double average_efficiency();
	double pv_charge_percent();

protected:

	// single value metrics
	double _e_charge_accumulated;	 // [Kwh]
	double _e_discharge_accumulated; // [Kwh]
	double _e_charge_from_pv;		 // [Kwh]
	double _e_charge_from_grid;		 // [Kwh]
	double _average_efficiency;		 // [%]
	double _pv_charge_percent;		 // [%]

	// annual metrics
	double _e_charge_from_pv_annual;   // [Kwh]
	double _e_charge_from_grid_annual; // [Kwh]
	double _e_charge_annual;		   // [Kwh]
	double _e_discharge_annual;		   // [Kwh]
	double _e_grid_import_annual;	   // [Kwh]
	double _e_grid_export_annual;	   // [Kwh]
	double _e_loss_annual;			   // [kWh]

	battery_t * _Battery;
	double _dt_hour;
};

#endif
