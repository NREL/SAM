#include "lib_battery_dispatch.h"
#include <math.h>

/*
Dispatch base class
*/
dispatch_t::dispatch_t(battery_t * Battery, double dt_hour, double SOC_min, double SOC_max, double Ic_max, double Id_max,
	double t_min, int mode, int pv_dispatch)
{
	_Battery = Battery;
	_Battery_initial = new battery_t(*_Battery);
	init(_Battery, dt_hour, SOC_min, SOC_max, Ic_max, Id_max, t_min, mode, pv_dispatch);
}

void dispatch_t::init(battery_t * Battery, double dt_hour, double SOC_min, double SOC_max, double Ic_max, double Id_max, double t_min, int mode, int pv_dispatch)
{
	_dt_hour = dt_hour;
	_SOC_min = SOC_min;
	_SOC_max = SOC_max;
	_Ic_max = Ic_max;
	_Id_max = Id_max;
	_t_min = t_min;
	_mode = mode;
	_pv_dispatch_to_battery_first = pv_dispatch;

	// positive quantities describing how much went to load
	_P_pv_to_load = 0.;
	_P_battery_to_load = 0.;
	_P_grid_to_load = 0.;

	// positive or negative quantities describing net power flows
	// note, do not include pv, since we don't modify from pv module
	_P_tofrom_batt = 0.;
	_P_grid = 0.;
	_P_gen = 0.;

	// positive quanitites describing how much went to battery
	_P_pv_to_batt = 0.;
	_P_grid_to_batt = 0.;

	// positive quantities describing how much went to grid
	_P_pv_to_grid = 0.;
	_P_battery_to_grid = 0.;

	// limit the switch from charging to discharge so that doesn't flip-flop subhourly
	_t_at_mode = 1000;
	_prev_charging = false;
	_charging = false;
	_e_max = Battery->battery_voltage()*Battery->battery_charge_maximum()*util::watt_to_kilowatt*0.01*(SOC_max - SOC_min);
	_grid_recharge = false;
}

// deep copy
dispatch_t::dispatch_t(const dispatch_t& dispatch)
{
	_Battery = new battery_t(*dispatch._Battery);
	_Battery_initial = new battery_t(*dispatch._Battery_initial);
	init(_Battery, dispatch._dt_hour, dispatch._SOC_min, dispatch._SOC_max, dispatch._Ic_max, dispatch._Id_max, dispatch._t_min, dispatch._mode, dispatch._pv_dispatch_to_battery_first);
}

// shallow copy from dispatch to this
void dispatch_t::copy(const dispatch_t & dispatch)
{
	_Battery->copy(*dispatch._Battery);
	_Battery_initial->copy(*dispatch._Battery_initial);
	init(_Battery, dispatch._dt_hour, dispatch._SOC_min, dispatch._SOC_max, dispatch._Ic_max, dispatch._Id_max, dispatch._t_min, dispatch._mode, dispatch._pv_dispatch_to_battery_first);
}
void dispatch_t::delete_clone()
{
	// need to delete both, since allocated memory for both in deep copy 
	if (_Battery) delete _Battery;
	if (_Battery_initial) delete _Battery_initial;
}
dispatch_t::~dispatch_t()
{
	// original _Battery doesn't need deleted, since was a pointer passed in
	_Battery_initial->delete_clone();
	delete _Battery_initial;
}
double dispatch_t::power_tofrom_battery(){ return _P_tofrom_batt; };
double dispatch_t::power_tofrom_grid(){ return _P_grid; };
double dispatch_t::power_pv_to_load(){ return _P_pv_to_load; };
double dispatch_t::power_battery_to_load(){ return _P_battery_to_load; };
double dispatch_t::power_grid_to_load(){ return _P_grid_to_load; };
double dispatch_t::power_pv_to_batt(){ return _P_pv_to_batt; };
double dispatch_t::power_grid_to_batt(){ return _P_grid_to_batt; };
double dispatch_t::power_gen(){ return _P_gen; }
double dispatch_t::power_pv_to_grid(){ return _P_pv_to_grid; }
double dispatch_t::power_battery_to_grid(){ return _P_battery_to_grid; }

message dispatch_t::get_messages(){ return _message; };

void dispatch_t::SOC_controller()
{
	// Implement minimum SOC cut-off
	if (_P_tofrom_batt > 0)
	{
		_charging = false;

		if (_P_tofrom_batt*_dt_hour > _e_max)
			_P_tofrom_batt = _e_max / _dt_hour;

		//  discharge percent
		double e_percent = _e_max*_percent_discharge*0.01;

		if (_P_tofrom_batt*_dt_hour > e_percent)
			_P_tofrom_batt = e_percent / _dt_hour;
	}
	// Maximum SOC cut-off
	else if (_P_tofrom_batt < 0)
	{
		_charging = true;

		if (_P_tofrom_batt*_dt_hour < -_e_max)
			_P_tofrom_batt = -_e_max / _dt_hour;

		//  charge percent for automated grid charging
		double e_percent = _e_max*_percent_charge*0.01;

		if (fabs(_P_tofrom_batt) > fabs(e_percent) / _dt_hour)
			_P_tofrom_batt = -e_percent / _dt_hour;
	}
	else
		_charging = _prev_charging;
}
void dispatch_t::switch_controller()
{
	// Implement rapid switching check
	if (_charging != _prev_charging)
	{
		if (_t_at_mode <= _t_min)
		{
			_P_tofrom_batt = 0.;
			_charging = _prev_charging;
			_t_at_mode += round(_dt_hour * util::hour_to_min);
		}
		else
			_t_at_mode = 0.;
	}
	_t_at_mode += round(_dt_hour * util::hour_to_min);

}
double dispatch_t::current_controller(double battery_voltage)
{
	double P, I = 0.; // [W],[V]
	P = util::kilowatt_to_watt*_P_tofrom_batt;
	I = P / battery_voltage;
	restrict_current(I);
	return I;
}
void dispatch_t::restrict_current(double &I)
{
	if (I < 0)
	{
		if (fabs(I) > _Ic_max)
			I = -_Ic_max;
	}
	else
	{
		if (I > _Id_max)
			I = _Id_max;
	}
}
void dispatch_t::compute_to_batt()
{
	// Compute how much power went to battery from each component
	if (_P_tofrom_batt < 0)
	{
		if (_P_pv_to_batt > 0)
		{
			// in event less energy dispatched than requested
			if (_P_pv_to_batt > fabs(_P_tofrom_batt))
				_P_pv_to_batt = fabs(_P_tofrom_batt);

			// in event more energy dispatched than requested
			if (_pv_dispatch_to_battery_first)
			{
				if (_P_pv_to_batt < fabs(_P_tofrom_batt))
				{
					// Compare DC to DC
					if (_P_pv_to_batt < _P_pv_charging)
					{
						_P_pv_to_batt = _P_pv_charging;

						if (_P_pv_to_batt > fabs(_P_tofrom_batt))
							_P_pv_to_batt = fabs(_P_tofrom_batt);
					}
				}
			}
		}
		if (_P_pv_to_batt < 0)
			_P_pv_to_batt = 0;

		_P_grid_to_batt = fabs(_P_tofrom_batt) - _P_pv_to_batt;
	}
	else
	{
		_P_pv_to_batt = 0;
		_P_grid_to_batt = 0;
	}
}
void dispatch_t::compute_to_load()
{
	// Compute how much of each component will meet the load.  
	if (!_pv_dispatch_to_battery_first)
	{
		// PV meets load before battery
		if (_P_pv > _P_load)
			_P_pv_to_load = _P_load;
		else
			_P_pv_to_load = _P_pv;
	}
	else
	{
		// derate the PV energy available
		_P_pv_to_load = _P_pv - _P_pv_to_batt;
	}

	if (_P_pv_to_load > _P_load)
		_P_pv_to_load = _P_load;

	if (_P_pv_to_load < 0)
		_P_pv_to_load = 0;

	if (_P_tofrom_batt > 0)
		_P_battery_to_load = _P_tofrom_batt;

	// could have slightly more dispatched than needed
	if (_P_battery_to_load > _P_load || (_P_battery_to_load + _P_pv_to_load > _P_load))
		_P_battery_to_load = _P_load - _P_pv_to_load;

	_P_grid_to_load = _P_load - (_P_pv_to_load + _P_battery_to_load);
}
void dispatch_t::compute_to_grid()
{
	_P_pv_to_grid = _P_pv - _P_pv_to_load - _P_pv_to_batt;
}
void dispatch_t::compute_generation()
{
	_P_gen = _P_pv + _P_tofrom_batt;
}
void dispatch_t::compute_grid_net()
{
	_P_grid = _P_pv + _P_tofrom_batt - _P_load;

	compute_to_batt();
	compute_to_load();
	compute_to_grid();
}
void dispatch_t::compute_battery_state()
{
	if (_P_tofrom_batt < 0)
	{
		_P_pv = _P_pv_charging;
		_P_load = _P_load_charging;
	}
	else
	{
		_P_pv = _P_pv_discharging;
		_P_load = _P_load_discharging;
	}
}
/*
Manual Dispatch
*/
dispatch_manual_t::dispatch_manual_t(battery_t * Battery, double dt, double SOC_min, double SOC_max, double Ic_max, double Id_max,
	double t_min, int mode, bool pv_dispatch,
	util::matrix_t<float> dm_dynamic_sched, util::matrix_t<float> dm_dynamic_sched_weekend,
	bool * dm_charge, bool *dm_discharge, bool * dm_gridcharge, std::map<int, double>  dm_percent_discharge, std::map<int, double>  dm_percent_gridcharge)
	: dispatch_t(Battery, dt, SOC_min, SOC_max, Ic_max, Id_max,
	t_min, mode, pv_dispatch)
{
	init(dm_dynamic_sched, dm_dynamic_sched_weekend, dm_charge, dm_discharge, dm_gridcharge, dm_percent_discharge, dm_percent_gridcharge);
}

void dispatch_manual_t::init(
	util::matrix_t<float> dm_dynamic_sched,
	util::matrix_t<float> dm_dynamic_sched_weekend,
	bool * dm_charge,
	bool *dm_discharge,
	bool * dm_gridcharge,
	std::map<int, double> dm_percent_discharge,
	std::map<int, double> dm_percent_gridcharge)
{
	_sched = dm_dynamic_sched;
	_sched_weekend = dm_dynamic_sched_weekend;
	for (int i = 0; i != 6; i++)
	{
		_charge_array.push_back(dm_charge[i]);
		_discharge_array.push_back(dm_discharge[i]);
		_gridcharge_array.push_back(dm_gridcharge[i]);
	}
	_percent_discharge_array = dm_percent_discharge;
	_percent_charge_array = dm_percent_gridcharge;
}

void dispatch_manual_t::init_with_vects(
	util::matrix_t<float> dm_dynamic_sched,
	util::matrix_t<float> dm_dynamic_sched_weekend,
	std::vector<bool> dm_charge,
	std::vector<bool> dm_discharge,
	std::vector<bool> dm_gridcharge,
	std::map<int, double> dm_percent_discharge,
	std::map<int, double> dm_percent_gridcharge)
{
	_sched = dm_dynamic_sched;
	_sched_weekend = dm_dynamic_sched_weekend;
	_charge_array = dm_charge;
	_discharge_array = dm_discharge;
	_gridcharge_array = dm_gridcharge;
	_percent_discharge_array = dm_percent_discharge;
	_percent_charge_array = dm_percent_gridcharge;
}

// deep copy from dispatch to this
dispatch_manual_t::dispatch_manual_t(const dispatch_t & dispatch) : 
dispatch_t(dispatch)
{
	const dispatch_manual_t * tmp = dynamic_cast<const dispatch_manual_t *>(&dispatch);
	init_with_vects(tmp->_sched, tmp->_sched_weekend, tmp->_charge_array, tmp->_discharge_array, tmp->_gridcharge_array, tmp->_percent_discharge_array, tmp->_percent_charge_array);
}

// shallow copy from dispatch to this
void dispatch_manual_t::copy(const dispatch_t & dispatch)
{
	dispatch_t::copy(dispatch);
	const dispatch_manual_t * tmp = dynamic_cast<const dispatch_manual_t *>(&dispatch);
	init_with_vects(tmp->_sched, tmp->_sched_weekend, tmp->_charge_array, tmp->_discharge_array, tmp->_gridcharge_array, tmp->_percent_discharge_array, tmp->_percent_charge_array);
}

void dispatch_manual_t::initialize_dispatch(size_t hour_of_year, size_t step, double P_pv_dc_charging, double P_pv_dc_discharging, double P_load_dc_charging, double P_load_dc_discharging)
{
	int m, h, column;
	int iprofile = -1;
	util::month_hour(hour_of_year, m, h);
	bool is_weekday = util::weekday(hour_of_year);
	_mode == MANUAL ? column = h - 1 : column = (h - 1) / _dt_hour + step;

	if (!is_weekday && _mode == MANUAL)
		iprofile = _sched_weekend(m - 1, column);
	else
		iprofile = _sched(m - 1, column);  // 1-based

	_can_charge = _charge_array[iprofile - 1];
	_can_discharge = _discharge_array[iprofile - 1];
	_can_grid_charge = _gridcharge_array[iprofile - 1];
	_percent_discharge = 0.;
	_percent_charge = 0.;

	if (_can_discharge){ _percent_discharge = _percent_discharge_array[iprofile]; }
	if (_can_charge){ _percent_charge = 100.; }
	if (_can_grid_charge){ _percent_charge = _percent_charge_array[iprofile]; }

	_P_grid = 0.;		      // [KW] power needed from grid to charge battery.  Positive indicates sending to grid.  Negative pulling from grid.
	_P_tofrom_batt = 0.;      // [KW] power transferred to/from the battery.     Positive indicates discharging, Negative indicates charging
	_P_pv_to_load = 0.;
	_P_battery_to_load = 0.;
	_P_grid_to_load = 0.;
	_P_pv_to_batt = 0.;
	_P_grid_to_batt = 0.;
	_P_pv_to_grid = 0.;
	_P_battery_to_grid = 0.;
	_charging = true;

	_P_pv_charging = P_pv_dc_charging;
	_P_pv_discharging = P_pv_dc_discharging;
	_P_load_charging = P_load_dc_charging;
	_P_load_discharging = P_load_dc_discharging;
}
void dispatch_manual_t::dispatch(size_t year,
	size_t hour_of_year,
	size_t step,
	double P_pv_dc_charging,
	double P_pv_dc_discharging,
	double P_load_dc_charging,
	double P_load_dc_discharging)
{
	initialize_dispatch(hour_of_year, step, P_pv_dc_charging, P_pv_dc_discharging, P_load_dc_charging, P_load_dc_discharging);

	// current charge state of battery from last time step.  
	double battery_voltage_nominal = _Battery->battery_voltage_nominal();						 // [V] 
	double battery_voltage = _Battery->battery_voltage();										 // [V] 
	double charge_needed_to_fill = _Battery->battery_charge_needed();						     // [Ah] - qmax - q0
	double energy_needed_to_fill = (charge_needed_to_fill * battery_voltage_nominal)*util::watt_to_kilowatt;   // [kWh]
	double charge_total = _Battery->battery_charge_total();								         // [Ah]
	double charge_max = _Battery->battery_charge_maximum();								         // [Ah]
	double I = 0.;															                     // [A] - The  current input/draw from battery after losses

	// Options for how to use PV
	if (!_pv_dispatch_to_battery_first)
		compute_energy_load_priority(energy_needed_to_fill);
	else
		compute_energy_battery_priority(energy_needed_to_fill);

	// Controllers
	SOC_controller();
	switch_controller();
	I = current_controller(battery_voltage_nominal);

	// Iteration variables
	_Battery_initial->copy(*_Battery);
	bool iterate = true;
	int count = 0;

	do {

		// Recompute
		if (!_pv_dispatch_to_battery_first)
			compute_energy_load_priority(energy_needed_to_fill);
		else
			compute_energy_battery_priority(energy_needed_to_fill);

		// Run Battery Model to update charge based on charge/discharge
		_Battery->run(I);

		// Update how much power was actually used to/from battery
		I = _Battery->capacity_model()->I();
		double battery_voltage_new = _Battery->battery_voltage();
		_P_tofrom_batt = I * 0.5*(battery_voltage + battery_voltage_new) * util::watt_to_kilowatt;// [kW]

		compute_battery_state();
		compute_generation();
		compute_grid_net();

		iterate = check_constraints(I, count);
		count++;

	} while (iterate);

	// update for next step
	_prev_charging = _charging;

}

bool dispatch_manual_t::check_constraints(double &I, int count)
{
	bool iterate = true;

	double I_initial = I;

	// stop iterating after 5 tries
	if (count > 5)
		iterate = false;
	// decrease the current draw if took too much
	else if (_Battery->battery_soc() < _SOC_min - tolerance)
	{
		double dQ = 0.01 * (_SOC_min - _Battery->battery_soc()) * _Battery->battery_charge_maximum();
		I -= dQ / _dt_hour;
	}
	// decrease the current charging if charged too much
	else if (_Battery->battery_soc() > _SOC_max + tolerance)
	{
		double dQ = 0.01 * (_Battery->battery_soc() - _SOC_max) * _Battery->battery_charge_maximum();
		I += dQ / _dt_hour;
	}
	// Don't allow grid charging unless explicitly allowed (reduce charging) 
	else if (_P_grid_to_batt > tolerance && !_can_grid_charge)
	{
		if (fabs(_P_tofrom_batt) < tolerance)
			I += (_P_grid_to_batt * util::kilowatt_to_watt / _Battery->battery_voltage());
		else
			I += (_P_grid_to_batt / fabs(_P_tofrom_batt)) *fabs(I);
	}
	// Don't let PV export to grid if can still charge battery (increase charging)
	else if (_P_pv_to_grid > tolerance && _can_charge && _Battery->battery_soc() < _SOC_max - tolerance && fabs(I) < fabs(_Ic_max))
	{
		if (fabs(_P_tofrom_batt) < tolerance)
			I -= (_P_pv_to_grid * util::kilowatt_to_watt / _Battery->battery_voltage());
		else
			I -= (_P_pv_to_grid / fabs(_P_tofrom_batt)) *fabs(I);
	}
	else
		iterate = false;

	// don't allow changes to violate current limits
	restrict_current(I);

	// don't allow battery to flip from charging to discharging or vice versa
	if ((I_initial / I) < 0)
		I = 0;
	
	if (iterate)
		_Battery->copy(*_Battery_initial);


	return iterate;
}

void dispatch_manual_t::compute_energy_load_priority(double energy_needed)
{
	double diff = 0.; // [%]

	// Is there extra power from array
	if (_P_pv_charging > _P_load_charging)
	{
		if (_can_charge)
		{
			// use all power available, it will only use what it can handle
			_P_pv_to_batt = _P_pv_charging - _P_load_charging;
			_P_tofrom_batt = -_P_pv_to_batt;

			if (((_P_pv_charging - _P_load_charging)*_dt_hour < energy_needed) && _can_grid_charge)
				_P_tofrom_batt = -energy_needed / _dt_hour;
		}
		// if we want to charge from grid without charging from array
		else if (_can_grid_charge)
			_P_tofrom_batt = -energy_needed / _dt_hour;
	}
	// Or, is the demand greater than or equal to what the array provides
	else if (_P_load_discharging >= _P_pv_discharging)
	{
		// try to discharge full amount.  Will only use what battery can provide
		if (_can_discharge)
		{
			_P_tofrom_batt = (_P_load_discharging - _P_pv_discharging) * 1.1;
			diff = fabs(_Battery->capacity_model()->SOC() - _SOC_min);
			if ((diff < tolerance) || _grid_recharge)
			{
				if (_can_grid_charge)
				{
					_grid_recharge = true;
					_P_tofrom_batt = -energy_needed / _dt_hour;
					diff = fabs(_Battery->capacity_model()->SOC() - _SOC_max);
					if (diff < tolerance)
						_grid_recharge = false;
				}
			}

		}
		// if we want to charge from grid
		else if (_can_grid_charge)
			_P_tofrom_batt = -energy_needed / _dt_hour;
		else if (!_can_grid_charge)
			_grid_recharge = false;
	}
}

void dispatch_manual_t::compute_energy_battery_priority(double energy_needed)
{
	double SOC = _Battery->capacity_model()->SOC();
	bool charged = (round(SOC) == _SOC_max);

	bool charging = compute_energy_battery_priority_charging(energy_needed);

	if (!charging && _can_discharge && _P_load_discharging > 0)
		_P_tofrom_batt = _P_load_discharging - _P_pv_discharging;
}
bool dispatch_manual_t::compute_energy_battery_priority_charging(double energy_needed)
{
	double SOC = _Battery->capacity_model()->SOC();
	bool charged = (round(SOC) == _SOC_max);
	bool charging = false;

	if (_can_charge && !charged > 0 && _P_pv_charging > 0)
	{
		if (_P_pv_charging > energy_needed / _dt_hour)
			_P_pv_to_batt = energy_needed / _dt_hour;
		else
			_P_pv_to_batt = _P_pv_charging;

		_P_tofrom_batt = -_P_pv_to_batt;

		if (_can_grid_charge)
			_P_tofrom_batt = -energy_needed / _dt_hour;
		charging = true;
	}
	else if (_can_grid_charge && !charged > 0)
	{
		_P_tofrom_batt = -energy_needed / _dt_hour;
		charging = true;
	}
	return charging;
}

dispatch_manual_front_of_meter_t::dispatch_manual_front_of_meter_t(battery_t * Battery, double dt, double SOC_min, double SOC_max, double Ic_max, double Id_max,
	double t_min, int mode, bool pv_dispatch,
	util::matrix_t<float> dm_dynamic_sched, util::matrix_t<float> dm_dynamic_sched_weekend,
	bool * dm_charge, bool *dm_discharge, bool * dm_gridcharge, std::map<int, double>  dm_percent_discharge, std::map<int, double>  dm_percent_gridcharge)
	: dispatch_manual_t(Battery, dt, SOC_min, SOC_max, Ic_max, Id_max,
	t_min, mode, pv_dispatch,
	dm_dynamic_sched, dm_dynamic_sched_weekend,
	dm_charge, dm_discharge, dm_gridcharge, dm_percent_discharge, dm_percent_gridcharge){};

void dispatch_manual_front_of_meter_t::dispatch(size_t year,
	size_t hour_of_year,
	size_t step,
	double P_pv_dc_charging,
	double P_pv_dc_discharging,
	double P_load_dc_charging,
	double P_load_dc_discharging)
{
	initialize_dispatch(hour_of_year, step, P_pv_dc_charging, P_pv_dc_discharging, P_load_dc_charging, P_load_dc_discharging);

	// current charge state of battery from last time step.  
	double battery_voltage_nominal = _Battery->battery_voltage_nominal();						  // [V] 
	double battery_voltage = _Battery->battery_voltage();
	double charge_needed_to_fill = _Battery->battery_charge_needed();						     // [Ah] - qmax - q0
	double energy_needed_to_fill = (charge_needed_to_fill * battery_voltage_nominal)*util::watt_to_kilowatt;   // [kWh]
	double charge_total = _Battery->battery_charge_total();								         // [Ah]
	double charge_max = _Battery->battery_charge_maximum();								         // [Ah]
	double I = 0.;															                     // [A] - The  current input/draw from battery after losses

	// Options for how to use PV
	compute_energy_no_load(energy_needed_to_fill);

	// Controllers
	SOC_controller();
	switch_controller();
	I = current_controller(battery_voltage_nominal);

	// Iteration variables
	_Battery_initial->copy(*_Battery);
	bool iterate = true;
	int count = 0;

	do {

		// Recompute every iteration to reset 
		compute_energy_no_load(energy_needed_to_fill);

		// Run Battery Model to update charge based on charge/discharge
		_Battery->run(I);

		// Update how much power was actually used to/from battery
		I = _Battery->capacity_model()->I();
		double battery_voltage_new = _Battery->battery_voltage();
		_P_tofrom_batt = I * 0.5*(battery_voltage + battery_voltage_new) * util::watt_to_kilowatt;// [kW]

		compute_battery_state();
		compute_generation();
		compute_grid_net();

		iterate = check_constraints(I, count);
		count++;

	} while (iterate);

	// update for next step
	_prev_charging = _charging;
}

void dispatch_manual_front_of_meter_t::compute_energy_no_load(double energy_needed)
{
	double SOC = _Battery->capacity_model()->SOC();
	bool charged = (round(SOC) == _SOC_max);

	bool charging = compute_energy_battery_priority_charging(energy_needed);

	// set to maximum discharge possible, will be constrained by controllers
	if (!charging && _can_discharge)
		_P_tofrom_batt = 1e38;
}
void dispatch_manual_front_of_meter_t::compute_grid_net()
{
	_P_grid = _P_pv + _P_tofrom_batt;

	compute_to_batt();
	compute_to_grid();
}
void dispatch_manual_front_of_meter_t::compute_to_grid()
{
	_P_pv_to_grid = _P_pv - _P_pv_to_batt;

	if (_P_tofrom_batt > 0)
		_P_battery_to_grid = fabs(_P_tofrom_batt);
}

automate_dispatch_t::automate_dispatch_t(
	battery_t * Battery,
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
	std::map<int, double> dm_percent_gridcharge,
	int nyears
	) : dispatch_manual_t(Battery, dt_hour, SOC_min, SOC_max, Ic_max, Id_max, t_min, mode, pv_dispatch,
	dm_dynamic_sched, dm_dynamic_sched_weekend, dm_charge, dm_discharge, dm_gridcharge, dm_percent_discharge, dm_percent_gridcharge)
{
	_day_index = 0;
	_hour_last_updated = -999;
	_dt_hour = dt_hour;
	_steps_per_hour = 1. / dt_hour;
	_nyears = nyears;
	_mode = mode;
	_num_steps = 24 * _steps_per_hour; // change if do look ahead of more than 24 hours
	_P_target_month = -1e16;
	_P_target_current = -1e16;
	_P_target_use.reserve(_num_steps);
	_month = 1;
	_safety_factor = 0.03;
	grid.reserve(_num_steps);
	for (int ii = 0; ii != _num_steps; ii++)
		grid.push_back(grid_point(0., 0, 0));
}
void automate_dispatch_t::dispatch(size_t year,
	size_t hour_of_year,
	size_t step,
	double P_pv_dc_charging,
	double P_pv_dc_discharging,
	double P_load_dc_charging,
	double P_load_dc_discharging)
{
	int step_per_hour = 1 / _dt_hour;
	int idx = 0;

	if (_mode == LOOK_AHEAD ||_mode == LOOK_BEHIND || _mode == MAINTAIN_TARGET)
		idx = (year * 8760 + hour_of_year)*step_per_hour + step;

	update_dispatch(hour_of_year, step, idx);
	dispatch_manual_t::dispatch(year, hour_of_year, step, P_pv_dc_charging, P_pv_dc_discharging, P_load_dc_charging, P_load_dc_discharging);
}
void automate_dispatch_t::update_pv_load_data(std::vector<double> P_pv_dc, std::vector<double> P_load_dc)
{
	_P_pv_dc = P_pv_dc;
	_P_load_dc = P_load_dc;
}
int automate_dispatch_t::get_mode(){ return _mode; }
void automate_dispatch_t::set_target_power(std::vector<double> P_target){ _P_target_input = P_target; }
void automate_dispatch_t::update_dispatch(int hour_of_year, int step, int idx)
{
	bool debug = false;
	FILE *p;
	check_debug(p, debug, hour_of_year, idx);
	int hour_of_day = util::hour_of_day(hour_of_year);
	_day_index = (hour_of_day * _steps_per_hour + step);

	if (hour_of_day == 0 && hour_of_year != _hour_last_updated)
	{
		double E_useful;  // [kWh] - the cyclable energy available in the battery
		double E_max;     // [kWh] - the maximum energy that can be cycled

		check_new_month(hour_of_year, step);

		// setup vectors
		initialize(hour_of_year);

		// compute grid power, sort highest to lowest
		sort_grid(p, debug, idx);

		// set period 1 as only PV charging
		int profile = 1;
		set_charge(profile);

		// Peak shaving scheme
		compute_energy(p, debug, E_max);
		target_power(p, debug, E_max, idx);

		// Set discharge, gridcharge profiles
		profile = set_discharge(p, debug, hour_of_year, E_max);
		set_gridcharge(p, debug, hour_of_year, profile, E_max);
	}
	// save for extraction
	_P_target_current = _P_target_use[_day_index];

	if (debug)
		fclose(p);
}
void automate_dispatch_t::initialize(int hour_of_year)
{
	_hour_last_updated = hour_of_year;
	_charge_array.clear();
	_discharge_array.clear();
	_gridcharge_array.clear();
	_P_target_use.clear();

	// clean up vectors
	for (int ii = 0; ii != _num_steps; ii++)
	{
		grid[ii] = grid_point(0., 0, 0);
		_P_target_use.push_back(0.);
	}
}
void automate_dispatch_t::check_new_month(int hour_of_year, int step)
{
	int hours = 0;
	for (int month = 1; month <= _month; month++)
		hours += util::hours_in_month(month);

	if (hours == 8760)
		hours = 0;

	if ((hour_of_year == hours) && step == 0)
	{
		_P_target_month = -1e16;
		_month < 12 ? _month++ : _month = 1;
	}
}
void automate_dispatch_t::check_debug(FILE *&p, bool & debug, int hour_of_year, int idx)
{
	// for now, don't enable
	// debug = true;

	if (hour_of_year == 0 && hour_of_year != _hour_last_updated)
	{
		// debug = true;
		if (debug)
		{
			p = fopen("dispatch.txt", "w");
			fprintf(p, "Hour of Year: %d\t Hour Last Updated: %d \t Steps per Hour: %d\n", hour_of_year, _hour_last_updated, _steps_per_hour);
		}
		// failed for some reason
		if (p == NULL)
			debug = false;
	}
}

void automate_dispatch_t::sort_grid(FILE *p, bool debug, int idx)
{

	if (debug)
		fprintf(p, "Index\t P_load (kW)\t P_pv (kW)\t P_grid (kW)\n");

	// compute grid net from pv and load (no battery)
	int count = 0;
	for (int hour = 0; hour != 24; hour++)
	{
		for (int step = 0; step != _steps_per_hour; step++)
		{
			grid[count] = grid_point(_P_load_dc[idx] - _P_pv_dc[idx], hour, step);

			if (debug)
				fprintf(p, "%d\t %.1f\t %.1f\t %.1f\n", count, _P_load_dc[idx], _P_pv_dc[idx], _P_load_dc[idx] - _P_pv_dc[idx]);

			idx++;
			count++;
		}
	}
	std::sort(grid.begin(), grid.end(), byGrid());
}

void automate_dispatch_t::compute_energy(FILE *p, bool debug, double & E_max)
{
	if (capacity_kibam_t * capacity = dynamic_cast<capacity_kibam_t *>(_Battery->capacity_model()))
	{
		E_max = _Battery->battery_voltage() *_Battery->capacity_model()->q1()*util::watt_to_kilowatt;
		if (E_max < 0)
			E_max = 0;
	}
	else
		E_max = _Battery->battery_voltage() *_Battery->battery_charge_maximum()*(_SOC_max - _SOC_min) *0.01 *util::watt_to_kilowatt;

	if (debug)
	{
		fprintf(p, "Energy Max: %.3f\t", E_max);
		fprintf(p, "Battery Voltage: %.3f\n", _Battery->battery_voltage());
	}
}

void automate_dispatch_t::target_power(FILE*p, bool debug, double E_useful, int idx)
{
	// if target power set, use that
	if (_P_target_input.size() > idx && _P_target_input[idx] >= 0)
	{
		double_vec::const_iterator first = _P_target_input.begin() + idx;
		double_vec::const_iterator last = _P_target_input.begin() + idx + _num_steps;
		double_vec tmp(first, last);
		_P_target_use = tmp;
		return;
	}
	// don't calculate if peak grid demand is less than a previous target in the month
	else if (grid[0].Grid() < _P_target_month)
	{
		for (int i = 0; i != _num_steps; i++)
			_P_target_use[i] = _P_target_month;
		return;
	}
	// otherwise, compute one target for the next 24 hours.
	else
	{
		// First compute target power which will allow battery to charge up to E_useful over 24 hour period
		if (debug)
			fprintf(p, "Index\tRecharge_target\t charge_energy\n");

		double P_target = grid[0].Grid();
		double P_target_min = 1e16;
		double E_charge = 0.;
		int index = _num_steps - 1;
		std::vector<double> E_charge_vec;
		for (int jj = _num_steps - 1; jj >= 0; jj--)
		{
			E_charge = 0.;
			P_target_min = grid[index].Grid();
			for (int ii = _num_steps - 1; ii >= 0; ii--)
			{
				if (grid[ii].Grid() > P_target_min)
					break;

				E_charge += (P_target_min - grid[ii].Grid())*_dt_hour;
			}
			E_charge_vec.push_back(E_charge);
			if (debug)
				fprintf(p, "%d: index\t%.3f\t %.3f\n", index, P_target_min, E_charge);
			index--;

			if (index < 0)
				break;
		}
		std::reverse(E_charge_vec.begin(), E_charge_vec.end());

		// Calculate target power 
		std::vector<double> sorted_grid_diff;
		sorted_grid_diff.reserve(_num_steps - 1);

		for (int ii = 0; ii != _num_steps - 1; ii++)
			sorted_grid_diff.push_back(grid[ii].Grid() - grid[ii + 1].Grid());

		P_target = grid[0].Grid(); // target power to shave to [kW]
		double sum = 0;			   // energy [kWh];
		if (debug)
			fprintf(p, "Step\tTarget_Power\tEnergy_Sum\tEnergy_charged\n");

		for (int ii = 0; ii != _num_steps - 1; ii++)
		{
			// don't look at negative grid power
			if (grid[ii + 1].Grid() < 0)
				break;
			// Update power target
			else
				P_target = grid[ii + 1].Grid();

			if (debug)
				fprintf(p, "%d\t %.3f\t", ii, P_target);

			// implies a repeated power
			if (sorted_grid_diff[ii] == 0)
			{
				if (debug)
					fprintf(p, "\n");
				continue;
			}
			// add to energy we are trimming
			else
				sum += sorted_grid_diff[ii] * (ii + 1)*_dt_hour;

			if (debug)
				fprintf(p, "%.3f\t%.3f\n", sum, E_charge_vec[ii + 1]);

			if (sum < E_charge_vec[ii + 1] && sum < E_useful)
				continue;
			// we have limited power, we'll shave what more we can
			else if (sum > E_charge_vec[ii + 1])
			{
				P_target += (sum - E_charge_vec[ii]) / ((ii + 1)*_dt_hour);
				sum = E_charge_vec[ii];
				if (debug)
					fprintf(p, "%d\t %.3f\t%.3f\t%.3f\n", ii, P_target, sum, E_charge_vec[ii]);
				break;
			}
			// only allow one cycle per day
			else if (sum > E_useful)
			{
				P_target += (sum - E_useful) / ((ii + 1)*_dt_hour);
				sum = E_useful;
				if (debug)
					fprintf(p, "%d\t %.3f\t%.3f\t%.3f\n", ii, P_target, sum, E_charge_vec[ii]);
				break;
			}
		}
		// set safety factor in case voltage differences make it impossible to achieve target without violated minimum SOC
		P_target *= (1 + _safety_factor);

		// don't set target lower than previous high in month
		if (P_target < _P_target_month)
		{
			P_target = _P_target_month;
			if (debug)
				fprintf(p, "P_target exceeds monthly target, move to  %.3f\n", P_target);
		}
		else
			_P_target_month = P_target;

		// write vector of targets
		for (int i = 0; i != _num_steps; i++)
			_P_target_use[i] = P_target;
	}
}
void automate_dispatch_t::set_charge(int profile)
{
	// set period 1 as only PV charging
	_charge_array.push_back(true);
	_discharge_array.push_back(false);
	_gridcharge_array.push_back(false);
	_sched.fill(profile);
}
int automate_dispatch_t::set_discharge(FILE *p, bool debug, int hour_of_year, double E_max)
{
	// Assign profiles within dispatch controller
	int profile = 1;
	int m, h;
	double discharge_energy = 0;
	if (debug)
		fprintf(p, "Profile\t Hour\t Step\t Discharge_Percent\t Discharge_Energy\t E_required\t Grid\n");

	for (int ii = 0; ii != _num_steps; ii++)
	{
		double discharge_percent = 0;
		size_t ind = (grid[ii].Hour() * _steps_per_hour) + grid[ii].Step();
		double energy_required = (grid[ii].Grid() - _P_target_use[ind])*_dt_hour;

		// Only discharge if current grid power is above target
		if (energy_required > 0)
		{
			discharge_percent = 100 * (energy_required / E_max);
			discharge_energy += energy_required;
			profile++;
			if (debug)
				fprintf(p, "%d\t %d\t %d\t %.3f\t %.3f\t %.3f\t %.3f\n", profile, grid[ii].Hour(), grid[ii].Step(), discharge_percent, discharge_energy, energy_required, grid[ii].Grid());

			if (discharge_percent > 100 && _mode == MAINTAIN_TARGET)
				_message.add("Unable to discharge enough to meet power target.  Increase power target.");
		

			util::month_hour(hour_of_year + grid[ii].Hour(), m, h);
			int min = grid[ii].Step();
			int column = (h - 1)*_steps_per_hour + min;

			// have set profile 0 as charge from solar only as default, start from 1
			_sched.set_value(profile, m - 1, column); // in hourly case, column is hour-1, sched is 1-based
			_charge_array.push_back(false);
			_discharge_array.push_back(true);
			_gridcharge_array.push_back(false);
			_percent_discharge_array[profile] = discharge_percent;
			_percent_charge_array[profile] = 100.;
		}
	}
	return profile;
}
void automate_dispatch_t::set_gridcharge(FILE *p, bool debug, int hour_of_year, int profile, double E_max)
{
	// grid charging scheme
	profile++;
	int m, h;
	std::vector<int> grid_profiles;
	int peak_hour = grid[0].Hour();
	double charge_energy = 0;
	int steps_grid_charged = 0;
	double charge_percent = 0;

	// Count charge all day
	for (int ii = 0; ii != _num_steps; ii++)
	{
		if (grid[ii].Grid() < 0)
			charge_energy += (-grid[ii].Grid()) * _dt_hour;
	}
	// Only grid charge if battery can't charge all the way from PV
	if (charge_energy < E_max)
	{
		if (debug)
			fprintf(p, "hour\t step\t grid\t charge_percent\t charge_energy\n");

		for (int ii = _num_steps - 1; ii >= 0; ii--)
		{
			size_t ind = (grid[ii].Hour() * _steps_per_hour) + grid[ii].Step();
			double P_target = _P_target_use[ind];

			// Grid power above target, don't charge battery (need to discharge)
			if (grid[ii].Grid() > P_target)
				continue;

			int hour = grid[ii].Hour();
			int step = grid[ii].Step();
			charge_percent = 100 * ((P_target - grid[ii].Grid())*_dt_hour) / E_max;
			charge_energy += (P_target - grid[ii].Grid())*_dt_hour;

			if (debug)
				fprintf(p, "%d\t %d\t %.3f\t %.3f\t %.3f\n", hour, step, grid[ii].Grid(), charge_percent, charge_energy);

			// Computed that we need to discharge, not charge
			if (charge_percent < 0)
				continue;

			util::month_hour(hour_of_year + hour, m, h);
			int column = (h - 1)*_steps_per_hour + step;
			_sched.set_value(profile, m - 1, column); // hourly, column is h-1
			_charge_array.push_back(true);
			_discharge_array.push_back(false);
			_gridcharge_array.push_back(true);
			_percent_charge_array[profile] = charge_percent;
			profile++;
		}
	}
	if (charge_energy == 0 && _mode == MAINTAIN_TARGET)
		_message.add("Power target too low, unable to charge battery.  Increase target power.");
}

battery_metrics_t::battery_metrics_t(battery_t * Battery, double dt_hour)
{
	_Battery = Battery;
	_dt_hour = dt_hour;

	// single value metrics
	_e_charge_accumulated = 0; // _Battery->battery_charge_total()*_Battery->battery_voltage()*watt_to_kilowatt;
	_e_charge_from_pv = 0.;
	_e_charge_from_grid = _e_charge_accumulated; // assumes initial charge from grid
	_e_discharge_accumulated = 0.;
	_average_efficiency = 100.;
	_pv_charge_percent = 0.;

	// annual metrics
	_e_charge_from_pv_annual = 0.;
	_e_charge_from_grid_annual = _e_charge_from_grid;
	_e_charge_annual = _e_charge_accumulated;
	_e_discharge_annual = 0.;
	_e_grid_import_annual = 0.;
	_e_grid_export_annual = 0.;
	_e_loss_annual = 0.;
}
double battery_metrics_t::average_efficiency(){ return _average_efficiency; }
double battery_metrics_t::pv_charge_percent(){ return _pv_charge_percent; }
double battery_metrics_t::energy_pv_charge_annual(){ return _e_charge_from_pv_annual; }
double battery_metrics_t::energy_grid_charge_annual(){ return _e_charge_from_grid_annual; }
double battery_metrics_t::energy_charge_annual(){ return _e_charge_annual; }
double battery_metrics_t::energy_discharge_annual(){ return _e_discharge_annual; }
double battery_metrics_t::energy_grid_import_annual(){ return _e_grid_import_annual; }
double battery_metrics_t::energy_grid_export_annual(){ return _e_grid_export_annual; }
double battery_metrics_t::energy_loss_annual(){ return _e_loss_annual; }

void battery_metrics_t::compute_metrics_ac(double P_tofrom_batt, double P_pv_to_batt, double P_grid_to_batt, double P_tofrom_grid)
{
	accumulate_grid_annual(P_tofrom_grid);
	accumulate_battery_charge_components(P_tofrom_batt, P_pv_to_batt, P_grid_to_batt);
	accumulate_energy_charge(P_tofrom_batt);
	accumulate_energy_discharge(P_tofrom_batt);
	compute_annual_loss();
}

void battery_metrics_t::compute_metrics_dc(dispatch_t * dispatch)
{
	// dc quantities
	double P_tofrom_grid = dispatch->power_tofrom_grid();
	double P_tofrom_batt = dispatch->power_tofrom_battery();
	double P_pv_to_batt = dispatch->power_pv_to_batt();
	double P_grid_to_batt = dispatch->power_grid_to_batt();

	accumulate_grid_annual(P_tofrom_grid);
	accumulate_energy_charge(P_tofrom_batt);
	accumulate_energy_discharge(P_tofrom_batt);
	accumulate_battery_charge_components(P_tofrom_batt, P_pv_to_batt, P_grid_to_batt);
	compute_annual_loss();
}
void battery_metrics_t::compute_annual_loss()
{
	_e_loss_annual = _e_charge_annual - _e_discharge_annual;
}
void battery_metrics_t::accumulate_energy_charge(double P_tofrom_batt)
{
	if (P_tofrom_batt < 0.)
	{
		_e_charge_accumulated += (-P_tofrom_batt)*_dt_hour;
		_e_charge_annual += (-P_tofrom_batt)*_dt_hour;
	}
}
void battery_metrics_t::accumulate_energy_discharge(double P_tofrom_batt)
{
	if (P_tofrom_batt > 0.)
	{
		_e_discharge_accumulated += P_tofrom_batt*_dt_hour;
		_e_discharge_annual += P_tofrom_batt*_dt_hour;
	}
}
void battery_metrics_t::accumulate_battery_charge_components(double P_tofrom_batt, double P_pv_to_batt, double P_grid_to_batt)
{
	if (P_tofrom_batt < 0.)
	{
		_e_charge_from_pv += P_pv_to_batt * _dt_hour;
		_e_charge_from_pv_annual += P_pv_to_batt * _dt_hour;
		_e_charge_from_grid += P_grid_to_batt * _dt_hour;
		_e_charge_from_grid_annual += P_grid_to_batt * _dt_hour;
	}
	_average_efficiency = 100.*(_e_discharge_accumulated / _e_charge_accumulated);
	_pv_charge_percent = 100.*(_e_charge_from_pv / _e_charge_accumulated);
}
void battery_metrics_t::accumulate_grid_annual(double P_tofrom_grid)
{
	// e_grid > 0 (export to grid) 
	// e_grid < 0 (import from grid)

	if (P_tofrom_grid > 0)
		_e_grid_export_annual += P_tofrom_grid*_dt_hour;
	else
		_e_grid_import_annual += (-P_tofrom_grid)*_dt_hour;
}

void battery_metrics_t::new_year()
{
	_e_charge_from_pv_annual = 0.;
	_e_charge_from_grid_annual = 0;
	_e_charge_annual = 0.;
	_e_discharge_annual = 0.;
	_e_grid_import_annual = 0.;
	_e_grid_export_annual = 0.;
}
