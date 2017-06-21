#include "lib_power_electronics.h"
#include "lib_sandia.h"
#include <cmath>

double bidirectional_inverter::convert_to_dc(double P_ac, double * P_dc)
{
	double P_loss = P_ac * (1 - _ac_dc_efficiency);
	*P_dc = P_ac * _ac_dc_efficiency;
	return P_loss;
}
double bidirectional_inverter::convert_to_ac(double P_dc, double * P_ac)
{
	double P_loss = P_dc * (1 - _dc_ac_efficiency);
	*P_ac = P_dc * _dc_ac_efficiency;
	return P_loss;
}
double bidirectional_inverter::compute_dc_from_ac(double P_ac)
{
	return P_ac / _dc_ac_efficiency;
}

double rectifier::convert_to_dc(double P_ac, double * P_dc)
{
	double P_loss = P_ac * (1 - _ac_dc_efficiency);
	*P_dc = P_ac * _ac_dc_efficiency;
	return P_loss;
}

charge_controller::charge_controller(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double efficiency_1, double efficiency_2)
{	
	_dispatch = dispatch;

	if (dispatch_manual_front_of_meter_t * dispatch_man_fom = dynamic_cast<dispatch_manual_front_of_meter_t*>(_dispatch))
			_dispatch_initial = new dispatch_manual_front_of_meter_t(*dispatch_man_fom);
	else if (dispatch_manual_t * dispatch_man = dynamic_cast<dispatch_manual_t*>(_dispatch))
		_dispatch_initial = new dispatch_manual_t(*dispatch_man);



	_battery_metrics = battery_metrics;
	_iterate = false;
	initialize(0,0,0);
}
charge_controller::~charge_controller()
{
	_dispatch_initial->delete_clone();
}
void charge_controller::initialize(double P_pv, double P_load_ac, size_t index)
{
	_P_load = P_load_ac;
	_P_grid = 0;
	_P_grid_to_load = 0;
	_P_grid_to_batt = 0;
	_P_gen = P_pv;
	_P_battery_to_load = 0;
	_P_pv_to_load = 0;
	_P_pv_to_battery = 0;
	_P_pv_to_grid = 0;
	_P_battery_to_grid = 0;
	_P_battery = 0;
	_P_inverter_draw = 0;
	_P_system_loss = _dispatch->battery_model()->losses_model()->battery_system_loss(index);

	if (P_pv < 0)
	{
		_P_inverter_draw = P_pv;
		P_pv = 0;
	}
	
	// ac for ac-connected, dc for dc-connected
	_P_pv = P_pv;

	// if this is an iteration loop, reset the dispatch
	if (_iterate)
		_dispatch->copy(*_dispatch_initial);
}
bool charge_controller::check_iterate(){ return _iterate; }
void charge_controller::finalize()
{
	_battery_metrics->compute_metrics_ac(_P_battery, _P_pv_to_battery, _P_grid_to_batt, _P_grid);
	_dispatch_initial->copy(*_dispatch);
}

dc_connected_battery_controller::dc_connected_battery_controller(dispatch_t * dispatch, 
																 battery_metrics_t * battery_metrics, 
																 double batt_dc_dc_bms_efficiency, 
																 double pv_dc_dc_mppt_efficiency,
																 double inverter_efficiency) :
charge_controller(dispatch, battery_metrics, 100, batt_dc_dc_bms_efficiency)
{
	_dc_dc_charge_controller = new dc_dc_charge_controller(batt_dc_dc_bms_efficiency, pv_dc_dc_mppt_efficiency);
	_inverter_efficiency = inverter_efficiency * 0.01;
}
dc_connected_battery_controller::~dc_connected_battery_controller()
{
	if (_dc_dc_charge_controller)
		delete _dc_dc_charge_controller;
}
void dc_connected_battery_controller::preprocess_pv_load()
{

	int pv_batt_choice = _dispatch->pv_dispatch_priority();

	// assume inverter isn't operating at peak efficiency, and that system losses are an additional load
	double P_load_dc = (_P_load / _inverter_efficiency) + _P_system_loss;
	double P_grid_dc = _P_pv - P_load_dc;


	// compute effective PV and Load if battery is discharging
	_P_pv_dc_discharge_input = _P_pv;
	_P_load_dc_discharge_input = P_load_dc / _dc_dc_charge_controller->batt_dc_dc_bms_efficiency();

	// compute effective PV and Load if battery is charging
	_P_pv_dc_charge_input = _P_pv;
	_P_load_dc_charge_input = P_load_dc;

	double P_max_to_batt = 0.;
	if (pv_batt_choice == dispatch_t::MEET_LOAD)
	{
		if (P_grid_dc > 0)
		{
			P_max_to_batt = P_grid_dc * _dc_dc_charge_controller->batt_dc_dc_bms_efficiency();
			_P_pv_dc_charge_input = _P_pv - (P_grid_dc - P_max_to_batt);
		}
	}
	else if (pv_batt_choice == dispatch_t::CHARGE_BATTERY)
	{
		double P_to_fill_dc = _dispatch->battery_power_to_fill();
		double P_to_fill_before_bms = P_to_fill_dc / _dc_dc_charge_controller->batt_dc_dc_bms_efficiency();
		double P_loss_bms = P_to_fill_before_bms - P_to_fill_dc;

		if (_P_pv > P_to_fill_before_bms)
			_P_pv_dc_charge_input = _P_pv - P_loss_bms;
		else
		{
			double P_pv_to_batt = _P_pv * _dc_dc_charge_controller->batt_dc_dc_bms_efficiency();
			P_loss_bms = _P_pv - P_pv_to_batt;
			_P_pv_dc_charge_input = _P_pv - P_loss_bms;
		}
	}

}

void dc_connected_battery_controller::run(size_t year, size_t hour_of_year, size_t step_of_hour, size_t index, double P_pv_dc, double P_load_ac)
{
	
	initialize(P_pv_dc, P_load_ac, index);

	preprocess_pv_load();

	// dispatch battery, load must be in dc.  This computes dc battery power to load, dc pv power to load, dc grid power to dc load, dc grid power to battery
	_dispatch->dispatch(year, hour_of_year, step_of_hour, _P_pv_dc_charge_input, _P_pv_dc_discharge_input, _P_load_dc_charge_input, _P_load_dc_discharge_input);

	// loss is due to conversion in dc-dc bms
	process_dispatch();

}
void dc_connected_battery_controller::process_dispatch()
{
	double P_battery_dc = _dispatch->power_tofrom_battery();
	double P_battery_dc_post_bms = 0;
	double P_battery_ac = 0;

	// post DC/DC w/BMS
	if (P_battery_dc > 0)
		P_battery_dc_post_bms = P_battery_dc * _dc_dc_charge_controller->batt_dc_dc_bms_efficiency();
	else if (P_battery_dc < 0)
		P_battery_dc_post_bms = P_battery_dc / _dc_dc_charge_controller->batt_dc_dc_bms_efficiency();
	
	// compute generation
	double P_gen_dc = _P_pv + P_battery_dc_post_bms - _P_system_loss;

	// dc output quantities	
	_P_battery = P_battery_dc_post_bms;
	_P_gen = P_gen_dc;

	// dc-dc bms conversion loss
	_P_loss = fabs(P_battery_dc - P_battery_dc_post_bms);
}

double dc_connected_battery_controller::update_gen_ac(double P_gen_ac)
{
	double P_battery_dc = _P_battery;
	double P_gen_dc = _P_gen;

	// capture inverter night-time losses
	if (P_gen_dc == 0 && P_gen_ac < 0)
		_P_inverter_draw = P_gen_ac;
	
	// if there is no "gen", then either no dispatch or all PV went to battery
	double inverter_efficiency = 1.00;
	if (fabs(P_gen_dc) > tolerance)
		inverter_efficiency = P_gen_ac / P_gen_dc;
	// if all of the PV went to the battery, then no need to worry about inverter
	else if (fabs(P_gen_dc) < tolerance)
		inverter_efficiency = _inverter_efficiency;

	// An edge case with large implications on efficiency, investigate further 
	if (inverter_efficiency < 0)
		inverter_efficiency = 1.00;

	compute_to_batt_load_grid(P_battery_dc, _P_pv, _P_load, inverter_efficiency);
	
	// add battery power inversion loss to total loss
	double P_battery_ac = _P_battery;
	_P_loss += fabs(P_battery_ac - P_battery_dc);

	// check assumption on inverter efficiency
	_iterate = false;
	if (fabs(inverter_efficiency - _inverter_efficiency) > tolerance && _P_load > tolerance)
	{
		_inverter_efficiency = 0.5 *(inverter_efficiency + _inverter_efficiency);
		_iterate = true;
	}

	return _P_loss;
}

void dc_connected_battery_controller::compute_to_batt_load_grid(double P_battery_dc, double P_pv_dc, double P_load_ac, double inverter_efficiency)
{
	double P_pv_to_batt_dc = 0;
	double P_pv_to_load_dc = 0;
	double P_pv_to_load_ac = 0;
	double P_pv_to_grid_ac = 0;
	double P_grid_to_batt_ac = 0;
	double P_batt_to_load_ac = 0;
	double P_batt_to_grid_ac = 0;
	double P_grid_to_load_ac = 0;
	double P_gen_ac = 0;
	double P_grid_ac = 0;
	double P_pv_ac = 0;

	double P_load_dc = P_load_ac / inverter_efficiency;
	double P_battery_ac = P_battery_dc;

	if (P_battery_dc > 0)
		P_battery_ac = P_battery_dc * inverter_efficiency;

	// compute to battery, to load, to grid
	if (P_battery_ac <= 0)
	{
		// allowed to charge with PV, grid?  Check in some way

		// pv priority to charging battery
		if (_dispatch->pv_dispatch_priority() == dispatch_t::CHARGE_BATTERY)
		{
			P_pv_to_batt_dc = fabs(P_battery_dc);

			if (P_pv_to_batt_dc > P_pv_dc)
			{
				P_pv_to_batt_dc = P_pv_dc;
				P_grid_to_batt_ac = (fabs(P_battery_dc) - P_pv_to_batt_dc) / inverter_efficiency;
			}
			
			P_pv_to_load_dc = P_pv_dc - P_pv_to_batt_dc;
			P_pv_to_load_ac = P_pv_to_load_dc * inverter_efficiency;

			if (P_pv_to_load_ac > P_load_ac)
			{
				P_pv_to_load_ac = P_load_ac;
				P_pv_to_grid_ac = (P_pv_dc - P_pv_to_batt_dc) * inverter_efficiency - P_pv_to_load_ac;
			}
		}
		// pv priority to meeting load
		else
		{
			P_pv_to_load_dc = P_load_dc;

			if (P_pv_to_load_dc > P_pv_dc)
				P_pv_to_load_dc = P_pv_dc;

			P_pv_to_batt_dc = P_pv_dc - P_pv_to_load_dc;

			if (P_pv_to_batt_dc > fabs(P_battery_dc))
			{
				P_pv_to_batt_dc = fabs(P_battery_dc);
				P_pv_to_grid_ac = (P_pv_dc - P_pv_to_load_dc - P_pv_to_batt_dc) * inverter_efficiency;
			}
			else
			{
				P_grid_to_batt_ac = (fabs(P_battery_dc) - P_pv_to_batt_dc) / inverter_efficiency;
				P_battery_ac = -(P_pv_to_batt_dc + P_grid_to_batt_ac);
			}
				
			P_pv_to_load_ac = P_pv_to_load_dc * inverter_efficiency;
		}
		P_pv_ac = P_pv_to_load_ac + P_pv_to_grid_ac;
		P_gen_ac = P_pv_ac - P_grid_to_batt_ac + _P_inverter_draw;
	}
	// discharge
	else
	{
		P_pv_ac = P_pv_dc * inverter_efficiency;
		P_pv_to_load_ac = P_pv_ac;
		if (P_pv_ac > P_load_ac)
		{
			P_pv_to_load_ac = P_load_ac;
			P_batt_to_load_ac = 0;
			P_pv_to_grid_ac = P_pv_ac - P_pv_to_load_ac;
			P_batt_to_grid_ac = P_battery_ac - P_batt_to_load_ac;
		}
		else if (P_battery_ac < P_load_ac - P_pv_to_load_ac)
			P_batt_to_load_ac = P_battery_ac;
		else
			// probably shouldn't happen, need to iterate and reduce I from battery
			P_batt_to_load_ac = P_load_ac - P_pv_to_load_ac;
		
		P_gen_ac = P_pv_ac + P_battery_ac + _P_inverter_draw;
	}

	P_grid_to_load_ac = P_load_ac - P_pv_to_load_ac - P_batt_to_load_ac;
	P_grid_ac = P_gen_ac - _P_load;

	// assign outputs
	_P_battery = P_battery_ac;
	_P_pv = P_pv_ac;
	_P_gen = P_gen_ac;
	_P_grid = P_grid_ac;
	_P_battery_to_load = P_batt_to_load_ac;
	_P_battery_to_grid = P_batt_to_grid_ac;
	_P_pv_to_battery = P_pv_to_batt_dc;
	_P_pv_to_load = P_pv_to_load_ac;
	_P_pv_to_grid = P_pv_to_grid_ac;
	_P_grid_to_load = P_grid_to_load_ac;
	_P_grid_to_batt = P_grid_to_batt_ac;

	if (fabs(_P_battery) < tolerance)
		_P_battery = 0.;
}



ac_connected_battery_controller::ac_connected_battery_controller(dispatch_t * dispatch, battery_metrics_t * battery_metrics, double ac_dc_efficiency, double dc_ac_efficiency) :
charge_controller(dispatch, battery_metrics, ac_dc_efficiency, dc_ac_efficiency)
{
	_bidirectional_inverter = new bidirectional_inverter(ac_dc_efficiency, dc_ac_efficiency);
}
ac_connected_battery_controller::~ac_connected_battery_controller()
{
	if (_bidirectional_inverter)
		delete _bidirectional_inverter;
}
void ac_connected_battery_controller::preprocess_pv_load()
{
	int pv_batt_choice = _dispatch->pv_dispatch_priority();
	double P_load_system = _P_load + _P_system_loss;
	double P_grid_ac = _P_pv - P_load_system;
	double P_grid_dc = 0.;

	// compute effective PV and Load if battery is discharging
	_P_pv_dc_discharge_input = _P_pv / _bidirectional_inverter->dc_ac_efficiency();
	_P_load_dc_discharge_input = _P_load / _bidirectional_inverter->dc_ac_efficiency();

	// compute effective PV and Load if battery is charging
	_P_pv_dc_charge_input = _P_pv;
	_P_load_dc_charge_input = _P_load;

	double P_max_to_batt_dc = 0.;
	if (pv_batt_choice == dispatch_t::MEET_LOAD)
	{
		if (P_grid_ac > 0)
		{
			P_max_to_batt_dc = P_grid_ac * _bidirectional_inverter->ac_dc_efficiency();
			_P_pv_dc_charge_input = _P_pv -(P_grid_ac - P_max_to_batt_dc);
			_P_load_dc_charge_input = P_load_system;
		}
	}
	else if (pv_batt_choice == dispatch_t::CHARGE_BATTERY)
	{
		double P_to_fill_dc = _dispatch->battery_power_to_fill();
		double P_to_fill_ac = P_to_fill_dc / _bidirectional_inverter->ac_dc_efficiency();
		double P_loss_inverter = P_to_fill_ac - P_to_fill_dc;

		if (_P_pv > P_to_fill_ac)
			_P_pv_dc_charge_input = _P_pv - P_to_fill_ac;
		else
		{
			double P_pv_to_batt = _P_pv * _bidirectional_inverter->ac_dc_efficiency();
			double P_loss_ac = _P_pv - P_pv_to_batt;
			_P_pv_dc_charge_input = _P_pv - P_loss_ac;
		} 
	}
}
void ac_connected_battery_controller::run( size_t year, size_t hour_of_year, size_t step_of_hour, size_t index, double P_pv_ac, double P_load_ac)
{
	
	initialize(P_pv_ac, P_load_ac, index);

	preprocess_pv_load();

	// dispatch battery
	_dispatch->dispatch(year, hour_of_year, step_of_hour, _P_pv_dc_charge_input, _P_pv_dc_discharge_input, _P_load_dc_charge_input, _P_load_dc_discharge_input);

	// Take resulting battery power and compute components
	process_dispatch();

	// AC charging metrics
	_battery_metrics->compute_metrics_ac(_P_battery, _P_pv_to_battery, _P_grid_to_batt, _P_grid);
}
void ac_connected_battery_controller::process_dispatch()
{
	double P_pv_ac = _P_pv;
	double P_load_ac = _P_load;
	double P_battery_dc = _dispatch->power_tofrom_battery();
	double P_battery_ac = 0;


	if (P_battery_dc > 0)
		P_battery_ac = P_battery_dc * _bidirectional_inverter->dc_ac_efficiency();
	else if (P_battery_dc < 0)
		P_battery_ac = P_battery_dc / _bidirectional_inverter->ac_dc_efficiency();

	compute_to_batt_load_grid(P_battery_ac, P_pv_ac, P_load_ac);
}

void ac_connected_battery_controller::compute_to_batt_load_grid(double P_battery_ac, double P_pv_ac, double P_load_ac, double inverter_efficiency)
{
	double P_battery_dc = _dispatch->power_tofrom_battery();

	// pre loss
	double P_pv_to_batt_ac = 0;
	double P_grid_to_batt_ac = 0;

	// post loss
	double P_batt_to_load_ac = 0;
	double P_grid_to_load_ac = 0;
	double P_pv_to_load_ac = 0;
	double P_pv_to_grid_ac = 0;
	double P_batt_to_grid_ac = 0;

	// losses
	double P_pv_to_batt_loss = 0;
	double P_grid_to_batt_loss = 0;
	double P_batt_to_load_loss = 0;

	// Gen should include losses except for grid_to_batt
	double P_gen_ac = 0;

	// grid should include losses for grid_to_batt
	double P_grid_ac = 0;

	// charging 
	if (P_battery_ac <= 0)
	{
		// allowed to charge with PV, grid?  Check in some way

		// pv priority to charging battery
		if (_dispatch->pv_dispatch_priority() == dispatch_t::CHARGE_BATTERY)
		{
			P_pv_to_batt_ac = fabs(P_battery_ac);

			// don't include any conversion efficiencies, want to compare AC to AC
			if (P_pv_to_batt_ac > P_pv_ac)
			{
				P_pv_to_batt_ac = P_pv_ac;
				P_grid_to_batt_ac = fabs(P_battery_ac) - P_pv_to_batt_ac;
			}

			P_pv_to_load_ac = P_pv_ac - P_pv_to_batt_ac;
			if (P_pv_to_load_ac > P_load_ac)
			{
				P_pv_to_load_ac = P_load_ac;
				P_pv_to_grid_ac = P_pv_ac - P_pv_to_batt_ac - P_pv_to_load_ac;
			}
		}
		// pv priority to meeting load
		else
		{
			P_pv_to_load_ac = P_load_ac;

			if (P_pv_to_load_ac > P_pv_ac)
				P_pv_to_load_ac = P_pv_ac;

			P_pv_to_batt_ac = (P_pv_ac - P_pv_to_load_ac);

			if (P_pv_to_batt_ac > fabs(P_battery_ac))
			{
				P_pv_to_batt_ac = fabs(P_battery_ac);
				P_pv_to_grid_ac = P_pv_ac - P_pv_to_load_ac - P_pv_to_batt_ac;
			}
			else
				P_grid_to_batt_ac = fabs(P_battery_ac) - P_pv_to_batt_ac;
		}
	}
	// discharging
	else
	{
		P_pv_to_load_ac = P_pv_ac;
		if (P_pv_ac > P_load_ac)
		{
			P_pv_to_load_ac = P_load_ac;
			P_batt_to_load_ac = 0;

			// discharging to grid
			P_pv_to_grid_ac = P_pv_ac - P_pv_to_load_ac;
			P_batt_to_grid_ac = P_battery_ac - P_batt_to_load_ac;
		}
		else if (P_battery_ac < P_load_ac - P_pv_to_load_ac)
			P_batt_to_load_ac = P_battery_ac;
		else
			// probably shouldn't happen, need to iterate and reduce I from battery
			P_batt_to_load_ac = P_load_ac - P_pv_to_load_ac;
	}

	// compute losses
	P_pv_to_batt_loss = P_pv_to_batt_ac * (1 - _bidirectional_inverter->ac_dc_efficiency());
	P_grid_to_batt_loss = P_grid_to_batt_ac *( 1- _bidirectional_inverter->ac_dc_efficiency());
	
	if (P_batt_to_load_ac > 0)
		P_batt_to_load_loss = P_battery_dc - P_battery_ac;

	P_grid_to_load_ac = P_load_ac - P_pv_to_load_ac - P_batt_to_load_ac;
	P_gen_ac = P_pv_ac + P_battery_ac + _P_inverter_draw - _P_system_loss;

	// Grid charging loss accounted for in P_battery_ac 
	P_grid_ac = P_gen_ac - _P_load;

	// check tolerances
	if (fabs(P_grid_to_load_ac) < tolerance)
		P_grid_to_load_ac = 0;
	if (fabs(P_grid_to_batt_ac) < tolerance)
		P_grid_to_batt_ac = 0;
	if (fabs(P_grid_ac) < tolerance)
		P_grid_ac = 0;

	// assign outputs
	_P_battery = P_battery_ac;
	_P_pv = P_pv_ac;
	_P_gen = P_gen_ac;
	_P_grid = P_grid_ac;
	_P_battery_to_load = P_batt_to_load_ac;
	_P_battery_to_grid = P_batt_to_grid_ac;
	_P_pv_to_battery = P_pv_to_batt_ac;
	_P_pv_to_load = P_pv_to_load_ac;
	_P_pv_to_grid = P_pv_to_grid_ac;
	_P_grid_to_load = P_grid_to_load_ac;
	_P_grid_to_batt = P_grid_to_batt_ac;

	// report only the loss due to pv charging and batt discharging
	_P_loss = P_pv_to_batt_loss + P_batt_to_load_loss;

}

double ac_connected_battery_controller::gen_ac()
{
	// dc quantities
	double P_gen_dc = _dispatch->power_gen();
	double P_battery_dc = _dispatch->power_tofrom_battery();
	double P_battery_to_load_dc = _dispatch->power_battery_to_load();
	double P_pv_to_load_dc = _dispatch->power_pv_to_load();
	double P_pv_to_battery_dc = _dispatch->power_pv_to_batt();
	double P_pv_to_grid_dc = _dispatch->power_pv_to_grid();
	double P_battery_to_grid_dc = _dispatch->power_battery_to_grid();

	// ac quantities
	_P_gen = P_gen_dc * _bidirectional_inverter->dc_ac_efficiency();
	_P_battery_to_load = P_battery_to_load_dc * _bidirectional_inverter->dc_ac_efficiency();
	_P_pv_to_load = P_pv_to_load_dc * _bidirectional_inverter->dc_ac_efficiency();
	_P_pv_to_grid = P_pv_to_grid_dc * _bidirectional_inverter->dc_ac_efficiency();

	if (P_battery_dc > 0)
	{
		_P_battery = P_battery_dc * _bidirectional_inverter->dc_ac_efficiency();
		_P_pv_to_battery = P_pv_to_battery_dc * _bidirectional_inverter->dc_ac_efficiency();
		_P_battery_to_grid = P_battery_to_grid_dc * _bidirectional_inverter->dc_ac_efficiency();
	}
	else if (P_battery_dc < 0)
	{
		_P_battery = P_battery_dc / _bidirectional_inverter->ac_dc_efficiency();
		_P_pv_to_battery = P_pv_to_battery_dc / _bidirectional_inverter->ac_dc_efficiency();

		if (_P_pv_to_battery > _P_pv)
		{
			double dPv = _P_pv_to_battery - _P_pv;
			_P_pv_to_battery -= dPv;
			_P_battery -= dPv;
		}
		else if (_P_pv_to_battery + _P_pv_to_load > _P_pv)
		{
			double dPv = _P_pv_to_battery + _P_pv_to_load - _P_pv;
			_P_pv_to_battery -= dPv;
			_P_battery -= dPv;
		}
	}
	double P_loss_gen = P_gen_dc - _P_gen;

	// extra metrics if desired
	double P_loss_battery_to_load = P_battery_to_load_dc - _P_battery_to_load;
	double P_loss_pv_to_load = P_pv_to_load_dc - _P_pv_to_load;
	double P_loss_pv_to_battery = P_pv_to_battery_dc - _P_pv_to_battery;
	double P_loss_battery = P_battery_dc - _P_battery;


	return P_loss_gen;
}
