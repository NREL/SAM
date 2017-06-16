#ifndef battery_h
#define battery_h

#include "lib_util.h"
#include "lsqfit.h"

#include <vector>
#include <map>
#include <string>
#include <stdio.h>
#include <algorithm>

const double tolerance = 0.001;

typedef std::vector<double> double_vec;
typedef std::vector<int> int_vec;

// Messages
class message
{
public:
	message(){};
	virtual ~message(){};


	void add(std::string message);
	int total_message_count();
	size_t message_count(int index);
	std::string get_message(int index);
	std::string construct_log_count_string(int index);

protected:
	std::vector<std::string> messages;
	std::vector<int> count;
};

/*
Base class from which capacity models derive
Note, all capacity models are based on the capacity of one battery
*/
class voltage_t;
class capacity_t
{
public:
	capacity_t(double q, double SOC_max);
	virtual capacity_t * clone() = 0;
	virtual void copy(capacity_t *&);
	virtual ~capacity_t(){};
	
	// pure virtual functions (abstract) which need to be defined in derived classes
	virtual void updateCapacity(double I, double dt) = 0;
	virtual void updateCapacityForThermal(double capacity_percent)=0;
	virtual void updateCapacityForLifetime(double capacity_percent)=0;
	virtual void replace_battery()=0;

	virtual double q1() = 0; // available charge
	virtual double q10() = 0; // capacity at 10 hour discharge rate

	void check_charge_change(); 
	void update_SOC();

	// common outputs
	double SOC();
	double DOD();
	double prev_DOD();
	double q0();
	double qmax(); 
	double I();
	bool chargeChanged();
	double I_loss();

protected:
	double _q0;  // [Ah] - Total capacity at timestep 
	double _qmax; // [Ah] - maximum possible capacity
	double _qmax0; // [Ah] - original maximum capacity
	double _I;   // [A]  - Current draw during last step
	double _I_loss; // [A] - Lifetime and thermal losses
	double _SOC; // [%] - State of Charge
	double _SOC_max; // [%] - Maximum SOC
	double _DOD; // [%] - Depth of Discharge
	double _DOD_prev; // [%] - Depth of Discharge of previous step
	double _dt_hour; // [hr] - Timestep in hours
	bool _chargeChange; // [true/false] - indicates if charging state has changed since last step
	int _prev_charge; // {CHARGE, NO_CHARGE, DISCHARGE}

	enum {CHARGE, NO_CHARGE, DISCHARGE};
};

/*
KiBaM specific capacity model
*/
class capacity_kibam_t : public capacity_t
{
public:

	// Public APIs 
	capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_max);
	~capacity_kibam_t(){}
	capacity_kibam_t * clone();
	void copy(capacity_t *&);

	void updateCapacity(double I, double dt);
	void updateCapacityForThermal(double capacity_percent);
	void updateCapacityForLifetime(double capacity_percent);
	void replace_battery();
	double q1(); // Available charge
	double q2(); // Bound charge
	double q10(); // Capacity at 10 hour discharge rate
	double q20(); // Capacity at 20 hour discharge rate

protected:
	// unique to kibam
	double c_compute(double F, double t1, double t2, double k_guess);
	double q1_compute(double q10, double q0, double dt, double I); // may remove some inputs, use class variables
	double q2_compute(double q20, double q0, double dt, double I); // may remove some inputs, use class variables
	double Icmax_compute(double q10, double q0, double dt);
	double Idmax_compute(double q10, double q0, double dt);
	double qmax_compute();
	double qmax_of_i_compute(double T);
	void parameter_compute();
	
	// parameters for finding c, k, qmax
	double _t1;  // [h] - discharge rate for capacity at _q1
	double _t2;  // [h] - discharge rate for capacity at _q2
	double _q1;  // [Ah]- capacity at discharge rate t1
	double _q2;  // [Ah] - capacity at discharge rate t2
	double _F1;  // [unitless] - internal ratio computation
	double _F2;  // [unitless] - internal ratio computation

	// model parameters
	double _c;  // [0-1] - capacity fraction
	double _k;  // [1/hour] - rate constant

	// charge which changes with time
	double _q1_0; // [Ah] - charge available
	double _q2_0; // [Ah] - charge bound
	double _q10; //  [Ah] - Capacity at 10 hour discharge rate
	double _q20; // [Ah] - Capacity at 20 hour discharge rate
	double _I20; // [A]  - Current at 20 hour discharge rate
};

/*
Lithium Ion specific capacity model
*/
class capacity_lithium_ion_t : public capacity_t
{
public:
	capacity_lithium_ion_t(double q, double SOC_max);
	~capacity_lithium_ion_t(){};
	capacity_lithium_ion_t * clone();
	void copy(capacity_t *&);

	// override public api
	void updateCapacity(double I, double dt);
	void updateCapacityForThermal(double capacity_percent);
	void updateCapacityForLifetime(double capacity_percent);
	void replace_battery();

	double q1(); // Available charge
	double q10(); // Capacity at 10 hour discharge rate

protected:
};


/*
Voltage Base class.  
All voltage models are based on one-cell, but return the voltage for one battery
*/
class thermal_t;
class voltage_t
{
public:
	voltage_t(int mode, int num_cells_series, int num_strings, double voltage, util::matrix_t<double> &voltage_table);
	virtual voltage_t * clone()=0;
	virtual void copy(voltage_t *&);
	virtual ~voltage_t(){};

	virtual void updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt)=0;
	virtual double battery_voltage(); // voltage of one battery

	double battery_voltage_nominal(); // nominal voltage of battery
	double cell_voltage(); // voltage of one cell
	double R(); // computed resistance

	enum VOLTAGE_CHOICE{VOLTAGE_MODEL, VOLTAGE_TABLE};

protected:
	int _mode;					  // voltage model (0), voltage table (1)
	int _num_cells_series;        // number of cells in series
	int _num_strings;             // addition number in parallel
	double _cell_voltage;         // closed circuit voltage per cell [V]
	double _cell_voltage_nominal; // nominal cell voltage [V]
	double _R;                    // internal resistance (Ohm)
	util::matrix_t<double> _batt_voltage_matrix;  // voltage vs depth-of-discharge
};

// A row in the table
class table_point
{
public:
	table_point(double DOD = 0., double V = 0.) :
		_DOD(DOD), _V(V){}
	double DOD() const{ return _DOD; }
	double V() const{ return _V; }

private:
	double _DOD;
	double _V;
};

struct byDOD
{
	bool operator()(table_point const &a, table_point const &b){ return a.DOD() < b.DOD(); }
};


class voltage_table_t : public voltage_t
{
public:
	voltage_table_t(int num_cells_series, int num_strings, double voltage, util::matrix_t<double> &voltage_table);
	voltage_table_t * clone();
	void copy(voltage_t *&);
	void updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt);

protected:

	bool exactVoltageFound(double DOD, double &V);
	void prepareInterpolation(double & DOD_lo, double & V_lo, double & DOD_hi, double & V_hi, double DOD);

private:
	std::vector<table_point> _voltage_table;
};

// Shepard + Tremblay Model
class voltage_dynamic_t : public voltage_t
{
public:
	voltage_dynamic_t(int num_cells_series, int num_strings, double voltage, double Vfull, double Vexp, double Vnom, double Qfull, double Qexp, double Qnom, double C_rate, double R);
	voltage_dynamic_t * clone();
	void copy(voltage_t *&);

	void parameter_compute();
	void updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt);

protected:
	double voltage_model_tremblay_hybrid(double capacity, double current, double q0);

private:
	double _Vfull;
	double _Vexp;
	double _Vnom;
	double _Qfull;
	double _Qexp;
	double _Qnom;
	double _C_rate;
	double _A;
	double _B0;
	double _E0;
	double _K;

};

// D'Agostino Vanadium Redox Flow Model
class voltage_vanadium_redox_t : public voltage_t
{
public:
	voltage_vanadium_redox_t(int num_cells_series, int num_strings, double V_ref_50, double R);
	voltage_vanadium_redox_t * clone();
	void copy(voltage_t *&);

	void updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt);

protected:
	double voltage_model(double q0, double qmax, double T);

private:
	double _V_ref_50;				// Reference voltage at 50% SOC
	double _R;						// Internal resistance [Ohm]
	double _I;						// Current level [A]
	double _R_molar;
	double _F;
	double _C0;
};

/*
Lifetime class.  Currently only one lifetime cycling model anticipated
*/

class lifetime_cycle_t
{

public:
	lifetime_cycle_t(const util::matrix_t<double> &cyles_vs_DOD, const int replacement_option, const double replacement_capacity);
	~lifetime_cycle_t();
	lifetime_cycle_t * clone();
	void copy(lifetime_cycle_t *&);

	void rainflow(double DOD);
	bool check_replaced();
	void reset_replacements();

	int replacements();
	int cycles_elapsed();
	double capacity_percent();
	int forty_percent_cycles();
	int hundred_percent_cycles();
	double cycle_range();

	// for user replacement schedule
	void force_replacement();

protected:
	void rainflow_ranges();
	void rainflow_ranges_circular(int index);
	int rainflow_compareRanges();
	double bilinear(double DOD, int cycle_number);

	util::matrix_t<double> _cycles_vs_DOD;
	util::matrix_t<double> _batt_lifetime_matrix;
	std::vector<double> _DOD_vect;
	std::vector<double> _cycles_vect;
	std::vector<double> _capacities_vect;


	int _nCycles;
	double _Dlt;			// % damage according to rainflow
	double _Clt;			// % capacity 
	double _jlt;			// last index in Peaks, i.e, if Peaks = [0,1], then _jlt = 1
	double _Xlt;
	double _Ylt;
	std::vector<double> _Peaks;
	double _Range;
	double _average_range;

	// battery replacement
	int _replacement_option;
	double _replacement_capacity;
	int _replacements;
	bool _replacement_scheduled;

	enum RETURN_CODES
	{
		LT_SUCCESS,
		LT_GET_DATA,
		LT_RERANGE
	};
};
/*
Lifetime calendar model
*/
class lifetime_calendar_t
{
public:
	lifetime_calendar_t();
	virtual ~lifetime_calendar_t();
};
/*
Thermal classes
*/
class thermal_t
{
public:
	thermal_t(double mass, double length, double width, double height,
		double Cp, double h, double T_room,
		const util::matrix_t<double> &cap_vs_temp);
	thermal_t * clone();
	void copy(thermal_t *&);

	void updateTemperature(double I, double R, double dt);
	void replace_battery();

	// outputs
	double T_battery();
	double capacity_percent();
	message get_messages(){ return _message; }

protected:
	double f(double T_battery, double I);
	double rk4(double I, double dt);
	double trapezoidal(double I, double dt);
	double implicit_euler(double I, double dt);

protected:

	util::matrix_t<double> _cap_vs_temp;

	double _mass;		// [kg]
	double _length;		// [m]
	double _width;		// [m]
	double _height;		// [m]
	double _Cp;			// [J/KgK] - battery specific heat capacity
	double _h;			// [Wm2K] - general heat transfer coefficient
	double _T_room;		// [K] - storage room temperature
	double _R;			// [Ohm] - internal resistance
	double _A;			// [m2] - exposed surface area
	double _T_battery;   // [K]
	double _capacity_percent; //[%]
	message _message;
	double _T_max;    //(mw) can't initialize here - breaks on gcc 
};
/*
Losses Base class
*/
class losses_t
{
public:
	losses_t(lifetime_cycle_t *, thermal_t *, capacity_t*, double_vec batt_system_losses);
	losses_t * clone();
	void copy(losses_t *&);

	void run_losses(double dt_hour);
	void replace_battery();
	double battery_system_loss(int i){ return _system_losses[i]; }

	enum { MONTHLY, TIMESERIES};

protected:
	lifetime_cycle_t * _lifetime_cycle;
	thermal_t * _thermal;
	capacity_t * _capacity;
	double_vec _system_losses;
	int _nCycle;
};

/*
Class which encapsulates a battery and all its models
*/

class battery_t
{
public:
	battery_t();
	battery_t(double dt, int battery_chemistry);

	// deep copy constructor (new memory), from battery to this
	battery_t(const battery_t& battery);

	// copy members from battery to this
	void copy(const battery_t& battery);
	~battery_t(){};
	void delete_clone();

	void initialize(capacity_t *, voltage_t *, lifetime_cycle_t *, thermal_t *, losses_t *);

	// Run all
	void run(double P);

	// Run a component level model
	void runCapacityModel(double I);
	void runVoltageModel();
	void runThermalModel(double I);
	void runLifetimeModel(double DOD);
	void runLossesModel();

	capacity_t * capacity_model() const;
	voltage_t * voltage_model() const;
	lifetime_cycle_t * lifetime_cycle_model() const;
	thermal_t * thermal_model() const;
	losses_t * losses_model() const;

	// Get capacity quantities
	double battery_charge_needed();
	double battery_charge_total();
	double battery_charge_maximum();
	double battery_energy_to_fill();
	double battery_power_to_fill();
	double battery_soc();

	// Get Voltage
	double cell_voltage();
	double battery_voltage(); // the actual battery voltage
	double battery_voltage_nominal(); // the nominal battery voltage

	double timestep_hour();

	enum CHEMS{ LEAD_ACID, LITHIUM_ION, VANADIUM_REDOX, IRON_FLOW};


private:
	capacity_t * _capacity;
	lifetime_cycle_t * _lifetime_cycle;
	voltage_t * _voltage;
	thermal_t * _thermal;
	losses_t * _losses;
	int _battery_chemistry;
	double _dt_hour;			// [hr] - timestep
	double _dt_min;				// [min] - timestep
	bool _firstStep;
};

#endif
