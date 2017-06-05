#include <math.h>
#include <cmath>
#include <cfloat>
#include <sstream>

#include "lib_battery.h"



/*
Message class
*/
void message::add(std::string message)
{
	std::vector<std::string>::iterator it;
	it = std::find(messages.begin(), messages.end(), message);
	if (it == messages.end())
	{
		messages.push_back(message);
		count.push_back(1);
	}
	else
		count[it - messages.begin()]++;

}
int message::total_message_count(){ return messages.size(); }
size_t message::message_count(int index)
{
	if (index < messages.size())
		return count[index];
	else
		return 0;
}
std::string message::get_message(int index)
{
	if (index < messages.size())
		return messages[index];
	else
		return NULL;
}
std::string message::construct_log_count_string(int index)
{
	//    std::string message_count = static_cast<std::ostringstream*>(&(std::ostringstream() << count[index]))->str();
	//    std::string message_count = static_cast<std::ostringstream>((std::ostringstream() << count[index])).str();
	std::ostringstream oss;
	oss << count[index];

	std::string message_count = oss.str();
	std::string log = messages[index] + " - warning occurred: " + message_count + " times";
	return log;
}

/* 
Define Capacity Model 
*/

capacity_t::capacity_t(double q, double SOC_max)
{
	_q0 = 0.01*SOC_max*q;
	_qmax = q;
	_qmax0 = q;
	_I = 0.;
	_I_loss = 0.;
	_dt_hour = 0.;

	// Initialize SOC, DOD
	_SOC = SOC_max;
	_SOC_max = SOC_max;
	_DOD = 0;
	_DOD_prev = 0;

	// Initialize charging states
	_prev_charge = DISCHARGE;
	_chargeChange = false;
}
void capacity_t::copy(capacity_t *& capacity)
{
	capacity->_q0 = _q0;
	capacity->_qmax = _qmax;
	capacity->_qmax0 = _qmax0;
	capacity->_I = _I;
	capacity->_I_loss = _I_loss;
	capacity->_dt_hour = _dt_hour;
	capacity->_SOC = _SOC;
	capacity->_SOC_max = _SOC_max;
	capacity->_DOD = _DOD;
	capacity->_DOD_prev = _DOD_prev;
	capacity->_prev_charge = _prev_charge;
	capacity->_chargeChange = _chargeChange;
}
void capacity_t::check_charge_change()
{
	int charging = NO_CHARGE;

	// charge state 
	if (_I < 0)
		charging = CHARGE;
	else if (_I > 0)
		charging = DISCHARGE;

	// Check if charge changed 
	_chargeChange = false;
	if ((charging != _prev_charge) && (charging != NO_CHARGE) && (_prev_charge != NO_CHARGE)  )
	{
		_chargeChange = true;
		_prev_charge = charging;
	}
}
void capacity_t::update_SOC()
{ 
	if (_qmax > 0)
		_SOC = 100.*(_q0 / _qmax);
	else
		_SOC = 0.;

	// due to dynamics, it's possible SOC could be slightly above 1 or below 0
	if (_SOC > 100.0)
		_SOC = 100.0;
	else if (_SOC < 0.)
		_SOC = 0.;

	_DOD = 100. - _SOC;
}
bool capacity_t::chargeChanged(){return _chargeChange;}
double capacity_t::SOC(){ return _SOC; }
double capacity_t::DOD(){ return _DOD; }
double capacity_t::prev_DOD(){ return _DOD_prev; }
double capacity_t::q0(){ return _q0;}
double capacity_t::qmax(){ return _qmax; }
double capacity_t::I(){ return _I; }
double capacity_t::I_loss() { return _I_loss; }

/*
Define KiBam Capacity Model
*/
capacity_kibam_t::capacity_kibam_t(double q20, double t1, double q1, double q10, double SOC_max) :
capacity_t(q20, SOC_max)
{
	_q10 = q10;
	_q20 = q20;
	_I20 = q20/20.;

	// parameters for c, k calculation
	_q1 = q1;
	_q2 = q10;
	_t1 = t1;
	_t2 = 10.;
	_F1 = q1 / q20; // use t1, 20
	_F2 = q1 / q10;  // use t1, 10

	// compute the parameters
	parameter_compute();
	_qmax0 = _qmax;

	// initializes to full battery
	replace_battery();
}
capacity_kibam_t * capacity_kibam_t::clone(){ return new capacity_kibam_t(*this); }
void capacity_kibam_t::copy(capacity_t *& capacity)
{
	capacity_t::copy(capacity);
	capacity_kibam_t * tmp = dynamic_cast<capacity_kibam_t*>(capacity);
	
	tmp->_t1 = _t1;
	tmp->_t2 = _t2;
	tmp->_q1 = _q1;
	tmp->_q2 = _q2;
	tmp->_F1 = _F1;
	tmp->_c = _c;
	tmp->_k = _k;
	tmp->_q1_0 = _q1_0;
	tmp->_q2_0 = _q2_0;
	tmp->_q10 = _q10;
	tmp->_q20 = _q20;
	tmp->_I20 = _I20;

	capacity = dynamic_cast<capacity_t*>(tmp);
}

void capacity_kibam_t::replace_battery()
{
	// Assume initial charge is max capacity
	_q0 = _qmax*_SOC_max*0.01;
	_q1_0 = _q0*_c;
	_q2_0 = _q0 - _q1_0;
	_qmax = _qmax0;
}

double capacity_kibam_t::c_compute(double F, double t1, double t2, double k_guess)
{
	double num = F*(1 - exp(-k_guess*t1))*t2 - (1 - exp(-k_guess*t2))*t1;
	double denom = F*(1 - exp(-k_guess*t1))*t2 - (1 - exp(-k_guess*t2))*t1 - k_guess*F*t1*t2 + k_guess*t1*t2;
	return (num / denom);
}

double capacity_kibam_t::q1_compute(double q10, double q0, double dt, double I)
{
	double A = q10*exp(-_k*dt);
	double B = (q0*_k*_c - I)*(1 - exp(-_k*dt)) / _k;
	double C = I*_c*(_k*dt - 1 + exp(-_k*dt)) / _k;
	return (A + B - C);
}

double capacity_kibam_t::q2_compute(double q20, double q0, double dt, double I)
{
	double A = q20*exp(-_k*dt);
	double B = q0*(1 - _c)*(1 - exp(-_k*dt));
	double C = I*(1 - _c)*(_k*dt - 1 + exp(-_k*dt)) / _k;
	return (A + B - C);
}

double capacity_kibam_t::Icmax_compute(double q10, double q0, double dt)
{
	double num = -_k*_c*_qmax + _k*q10*exp(-_k*dt) + q0*_k*_c*(1 - exp(-_k*dt));
	double denom = 1 - exp(-_k*dt) + _c*(_k*dt - 1 + exp(-_k*dt));
	return (num / denom);
}

double capacity_kibam_t::Idmax_compute(double q10, double q0, double dt)
{
	double num = _k*q10*exp(-_k*dt) + q0*_k*_c*(1 - exp(-_k*dt));
	double denom = 1 - exp(-_k*dt) + _c*(_k*dt - 1 + exp(-_k*dt));
	return (num / denom);
}

double capacity_kibam_t::qmax_compute()
{
	double num = _q20*((1 - exp(-_k * 20)) * (1 - _c) + _k*_c * 20);
	double denom = _k*_c * 20;
	return (num / denom);
}

double capacity_kibam_t::qmax_of_i_compute(double T)
{
	return ((_qmax*_k*_c*T) / (1 -exp(-_k*T) + _c*(_k*T - 1 + exp(-_k*T))));
}
void capacity_kibam_t::parameter_compute()
{
	double k_guess = 0.;
	double c1 = 0.;
	double c2 = 0.;
	double minRes = 10000.;

	for (int i = 0; i < 5000; i++)
	{
		k_guess = i*0.001;
		c1 = c_compute(_F1, _t1, 20, k_guess);
		c2 = c_compute(_F2, _t1, _t2, k_guess);

		if (fabs(c1 - c2) < minRes)
		{
			minRes = fabs(c1 - c2);
			_k = k_guess;
			_c = 0.5*(c1 + c2);
		}
	}
	_qmax = qmax_compute();
}

void capacity_kibam_t::updateCapacity(double I, double dt_hour)
{
	_DOD_prev = _DOD;							 
	_I_loss = 0.;
	_I = I;
	_dt_hour = dt_hour;

	double Idmax = 0.;
	double Icmax = 0.;
	double Id = 0.;
	double Ic = 0.;
	double q1 = 0.;
	double q2 = 0.;

	if (_I > 0)
	{
		Idmax = Idmax_compute(_q1_0, _q0, dt_hour);
		Id = fmin(_I, Idmax);
		_I = Id;
	}
	else if (_I < 0 )
	{
		Icmax = Icmax_compute(_q1_0, _q0, dt_hour);
		Ic = -fmin(fabs(_I), fabs(Icmax));
		_I = Ic;
	}

	// new charge levels
	q1 = q1_compute(_q1_0, _q0, dt_hour, _I);
	q2 = q2_compute(_q2_0, _q0, dt_hour, _I);

	// potentially a bug that needs to be fixed, for now hack
	if (q1 + q2 > _qmax)
	{
		double q0 = q1 + q2;
		double p1 = q1 / q0;
		double p2 = q2 / q0;
		_q0 = _qmax;
		q1 = _q0*p1;
		q2 = _q0*p2;
	}

	// update internal variables 
	_q1_0 = q1;
	_q2_0 = q2;
	_q0 = q1 + q2;

	update_SOC();
	check_charge_change(); 
}
void capacity_kibam_t::updateCapacityForThermal(double capacity_percent)
{
	double qmax_tmp = _qmax*capacity_percent*0.01;
	if (_q0 > qmax_tmp)
	{
		double q0_orig = _q0;
		double p = qmax_tmp / _q0;
		_q0 *= p;
		_q1 *= p;
		_q2 *= p;
		_I_loss += (q0_orig - _q0) / _dt_hour;
		_I += (_q0 - qmax_tmp) / _dt_hour;
	}
	update_SOC();
}
void capacity_kibam_t::updateCapacityForLifetime(double capacity_percent)
{

	if (_qmax0* capacity_percent*0.01 <= _qmax)
		_qmax = _qmax0* capacity_percent*0.01;

	// scale to q0 = qmax if q0 > qmax
	if (_q0 > _qmax)
	{
		double q0_orig = _q0;
		double p = _qmax / _q0;
		_q0 *= p;
		_q1 *= p;
		_q2 *= p;
		_I_loss += (q0_orig - _q0) / _dt_hour;
	}
	update_SOC();
}

double capacity_kibam_t::q1(){ return _q1_0; }
double capacity_kibam_t::q2(){ return _q2_0; }
double capacity_kibam_t::q10(){ return _q10; }
double capacity_kibam_t::q20(){return _q20;}


/*
Define Lithium Ion capacity model
*/
capacity_lithium_ion_t::capacity_lithium_ion_t(double q, double SOC_max) :capacity_t(q, SOC_max){};
capacity_lithium_ion_t * capacity_lithium_ion_t::clone(){ return new capacity_lithium_ion_t(*this); }
void capacity_lithium_ion_t::copy(capacity_t *& capacity)
{
	capacity_t::copy(capacity);
}

void capacity_lithium_ion_t::replace_battery()
{
	_q0 = _qmax0;
	_qmax = _qmax0;
}
void capacity_lithium_ion_t::updateCapacity(double I, double dt)
{
	_DOD_prev = _DOD;
	_I_loss = 0.;
	_dt_hour = dt;
	double q0_old = _q0;
	_I = I;

	// update charge ( I > 0 discharging, I < 0 charging)
	_q0 -= _I*dt;

	// check if overcharged
	if (_q0 > _qmax)
	{
		_I = -(_qmax - q0_old) / dt;
		_q0 = _qmax;
	}

	// check if undercharged 
	if (_q0 < 0)
	{
		_I = (q0_old) / dt;
		_q0 = 0;
	}

	// update SOC, DOD
	update_SOC();
	check_charge_change();
}
void capacity_lithium_ion_t::updateCapacityForThermal(double capacity_percent)
{
	double qmax_tmp = _qmax*capacity_percent*0.01;
	if (_q0 > qmax_tmp)
	{
		// investigate more, do we actually need to adjust current?
		if (fabs(_I) > 0)
		{
			_I_loss += (_q0 - qmax_tmp) / _dt_hour;
			_I += (_q0 - qmax_tmp) / _dt_hour;
		}
		_q0 = qmax_tmp;
	}
	update_SOC();

}
void capacity_lithium_ion_t::updateCapacityForLifetime(double capacity_percent)
{

	if (_qmax0* capacity_percent*0.01 <= _qmax)
		_qmax = _qmax0* capacity_percent*0.01;
	
	if (_q0 > _qmax)
	{
		_I_loss += (_q0 - _qmax) / _dt_hour;
		_q0 = _qmax;
	}

	update_SOC();
}
double capacity_lithium_ion_t::q1(){return _q0;}
double capacity_lithium_ion_t::q10(){return _qmax;}


/*
Define Voltage Model
*/
voltage_t::voltage_t(int num_cells_series, int num_strings, double voltage)
{
	_num_cells_series = num_cells_series;
	_num_strings = num_strings;
	_cell_voltage = voltage;
	_cell_voltage_nominal = voltage;
	_R = 0.004; // just a default, will get recalculated upon construction
}
void voltage_t::copy(voltage_t *& voltage)
{
	voltage->_num_cells_series = _num_cells_series;
	voltage->_num_strings = _num_strings;
	voltage->_cell_voltage = _cell_voltage;
	voltage->_R = _R;
}
double voltage_t::battery_voltage(){ return _num_cells_series*_cell_voltage; }
double voltage_t::battery_voltage_nominal(){ return _num_cells_series * _cell_voltage_nominal; }
double voltage_t::cell_voltage(){ return _cell_voltage; }
double voltage_t::R(){ return _R; }


// Dynamic voltage model
voltage_dynamic_t::voltage_dynamic_t(int num_cells_series, int num_strings, double voltage, double Vfull, double Vexp, double Vnom, double Qfull, double Qexp, double Qnom, double C_rate, double R):
voltage_t(num_cells_series, num_strings, voltage)
{
	_Vfull = Vfull;
	_Vexp = Vexp;
	_Vnom = Vnom;
	_Qfull = Qfull;
	_Qexp = Qexp;
	_Qnom = Qnom;
	_C_rate = C_rate;
	_R = R;

	// assume fully charged, not the nominal value
	_cell_voltage = _Vfull;

	parameter_compute();
};
voltage_dynamic_t * voltage_dynamic_t::clone(){ return new voltage_dynamic_t(*this); }
void voltage_dynamic_t::copy(voltage_t *& voltage)
{
	
	voltage_t::copy(voltage);
	voltage_dynamic_t * tmp = dynamic_cast<voltage_dynamic_t*>(voltage);

	tmp->_Vfull = _Vfull;
	tmp->_Vexp = _Vexp;
	tmp->_Qfull = _Qfull;
	tmp->_Qexp = _Qexp;
	tmp->_Qnom = _Qnom;
	tmp->_C_rate = _C_rate;
	tmp->_A = _A;
	tmp->_B0 = _B0;
	tmp->_E0 = _E0;
	tmp->_K = _K;

	voltage = dynamic_cast<voltage_t*>(tmp);

}
void voltage_dynamic_t::parameter_compute()
{
	// Determines parameters according to page 2 of:
	// Tremblay 2009 "A Generic Bettery Model for the Dynamic Simulation of Hybrid Electric Vehicles"
	double eta = 0.995;
	double I = _Qfull*_C_rate; // [A]
	//_R = _Vnom*(1. - eta) / (_C_rate*_Qnom); // [Ohm]
	_A = _Vfull - _Vexp; // [V]
	_B0 = 3. / _Qexp;     // [1/Ah]
	_K = ((_Vfull - _Vnom + _A*(std::exp(-_B0*_Qnom) - 1))*(_Qfull - _Qnom)) / (_Qnom); // [V] - polarization voltage
	_E0 = _Vfull + _K + _R*I - _A;
}

void voltage_dynamic_t::updateVoltage(capacity_t * capacity, thermal_t * themal, double dt)
{

	double Q = capacity->qmax();
	double I = capacity->I();
	double q0 = capacity->q0();
	
	// is on a per-cell basis.
	// I, Q, q0 are on a per-string basis since adding cells in series does not change current or charge
	double cell_voltage = voltage_model_tremblay_hybrid(Q / _num_strings, I/_num_strings , q0 / _num_strings);

	// the cell voltage should not increase when the battery is discharging
	if (I <= 0 || (I > 0 && cell_voltage <= _cell_voltage) )
		_cell_voltage = cell_voltage;
}

double voltage_dynamic_t::voltage_model_tremblay_hybrid(double Q, double I, double q0)
{
	// everything in here is on a per-cell basis
	// Tremblay Dynamic Model
	double it = Q - q0;
	double E = _E0 - _K*(Q / (Q - it)) + _A*exp(-_B0*it);
	double V = E - _R*I;

	// Discharged lower than model can handle ( < 1% SOC)
	if (V < 0 || !std::isfinite(V))
		V = 0.5*_Vnom; 
	else if (V > _Vfull*1.25)
		V = _Vfull;
	return V;
}

// Vanadium redox flow model
voltage_vanadium_redox_t::voltage_vanadium_redox_t(int num_cells_series, int num_strings, double V_ref_50,  double R):
voltage_t(num_cells_series, num_strings, V_ref_50)
{
	_I = 0;
	_V_ref_50 = V_ref_50;
	_R = R;
    _R_molar = 8.314;  // Molar gas constant [J/mol/K]^M
    _F = 26.801 * 3600;// Faraday constant [As/mol]^M
    _C0 = 1.38;                 // model correction factor^M	
}
voltage_vanadium_redox_t * voltage_vanadium_redox_t::clone(){ return new voltage_vanadium_redox_t(*this); }
void voltage_vanadium_redox_t::copy(voltage_t *& voltage)
{
	voltage_t::copy(voltage);
	voltage_vanadium_redox_t * tmp = dynamic_cast<voltage_vanadium_redox_t*>(voltage);

	tmp->_V_ref_50 = _V_ref_50;
	tmp->_R = _R;

	voltage = dynamic_cast<voltage_t*>(tmp);
}
void voltage_vanadium_redox_t::updateVoltage(capacity_t * capacity, thermal_t * thermal, double dt)
{

	double Q = capacity->qmax();
	_I = capacity->I();
	double q0 = capacity->q0();

	// Kelvin
	double T = thermal->T_battery(); 

	// is on a per-cell basis.
	// I, Q, q0 are on a per-string basis since adding cells in series does not change current or charge
	double cell_voltage = voltage_model(Q / _num_strings, q0 / _num_strings, T);

	// the cell voltage should not increase when the battery is discharging
	if (_I <= 0 || (_I > 0 && cell_voltage <= _cell_voltage))
		_cell_voltage = cell_voltage;
}
double voltage_vanadium_redox_t::voltage_model(double qmax, double q0, double T)
{
	double SOC = q0 / qmax;
	double SOC_use = SOC;
	if (SOC > 1 - tolerance)
		SOC_use = 1 - tolerance;

	double A = std::log(std::pow(SOC_use, 2) / std::pow(1 - SOC_use, 2));

	double V_stack_cell = 0.;
	if (std::isfinite(A))
		V_stack_cell = _V_ref_50 + (_R_molar * T / _F) * A *_C0;

	return V_stack_cell;
}
/*
// The I*R term makes the voltage increase when battery is discharging?
double voltage_vanadium_redox_t::battery_voltage()
{
	double V_batt = (_num_cells_series * _cell_voltage) + (_I * _R);
	return V_batt;
}
*/

/*
Define Lifetime Model
*/

lifetime_t::lifetime_t(const util::matrix_t<double> &batt_lifetime_matrix, const int replacement_option, const double replacement_capacity)
{
	_batt_lifetime_matrix = batt_lifetime_matrix;
	_replacement_option = replacement_option;
	_replacement_capacity = replacement_capacity; 
	// issues as capacity approaches 0%
	if (replacement_capacity == 0.) { _replacement_capacity = 2.; }
	_replacements = 0;
	_replacement_scheduled = false;

	for (int i = 0; i < _batt_lifetime_matrix.nrows(); i++)
	{
		_DOD_vect.push_back(batt_lifetime_matrix.at(i,0));
		_cycles_vect.push_back(batt_lifetime_matrix.at(i,1));
		_capacities_vect.push_back(batt_lifetime_matrix.at(i, 2));
	}
	// initialize other member variables
	_nCycles = 0;
	_Dlt = 0;
	_Clt = bilinear(0.,0);
	_jlt = 0;
	_Xlt = 0;
	_Ylt = 0;
	_Range = 0;
	_average_range = 0;
}

lifetime_t::~lifetime_t(){}
lifetime_t * lifetime_t::clone(){ return new lifetime_t(*this); }
void lifetime_t::copy(lifetime_t *& lifetime)
{
	lifetime->_nCycles = _nCycles;
	lifetime->_Dlt = _Dlt;
	lifetime->_Clt = _Clt;
	lifetime->_jlt = _jlt;
	lifetime->_Xlt = _Xlt;
	lifetime->_Ylt = _Ylt;
	lifetime->_Peaks = _Peaks;
	lifetime->_Range = _Range;
	lifetime->_average_range = _average_range;
	lifetime->_replacement_option = _replacement_option;
	lifetime->_replacement_capacity = _replacement_capacity;
	lifetime->_replacements = _replacements;
	lifetime->_replacement_scheduled = _replacement_scheduled;
}


void lifetime_t::rainflow(double DOD)
{
	// initialize return code
	int retCode = LT_GET_DATA;

	// Begin algorithm
	_Peaks.push_back(DOD);
	bool atStepTwo = true;

	// Loop until break
	while (atStepTwo)
	{
		// Rainflow: Step 2: Form ranges X,Y
		if (_jlt >= 2)
			rainflow_ranges();
		else
		{
			// Get more data (Step 1)
			retCode = LT_GET_DATA;
			break;
		}

		// Rainflow: Step 3: Compare ranges
		retCode = rainflow_compareRanges();

		// We break to get more data, or if we are done with step 5
		if (retCode == LT_GET_DATA)
			break;
	}

	if (retCode == LT_GET_DATA)
		_jlt++;
}

void lifetime_t::rainflow_ranges()
{
	_Ylt = fabs(_Peaks[_jlt - 1] - _Peaks[_jlt - 2]);
	_Xlt = fabs(_Peaks[_jlt] - _Peaks[_jlt - 1]);
}
void lifetime_t::rainflow_ranges_circular(int index)
{
	int end = _Peaks.size() - 1;
	if (index == 0)
	{
		_Xlt = fabs(_Peaks[0] - _Peaks[end]);
		_Ylt = fabs(_Peaks[end] - _Peaks[end - 1]);
	}
	else if (index == 1)
	{
		_Xlt = fabs(_Peaks[1] - _Peaks[0]);
		_Ylt = fabs(_Peaks[0] - _Peaks[end]);
	}
	else
		rainflow_ranges();
}

int lifetime_t::rainflow_compareRanges()
{
	int retCode = LT_SUCCESS;
	bool contained = true;

	// modified to disregard some of algorithm which doesn't work well
	if (_Xlt < _Ylt)
		retCode = LT_GET_DATA;
	else if (_Xlt >= _Ylt)
		contained = false;

	// Step 5: Count range Y, discard peak & valley of Y, go to Step 2
	if (!contained)
	{
		_Range = _Ylt;
		_average_range = (_average_range*_nCycles + _Range) / (_nCycles + 1);		
		_nCycles++;

		// the capacity percent cannot increase
		if (bilinear(_average_range, _nCycles) <= _Clt)
			_Clt = bilinear(_average_range, _nCycles);

		if (_Clt < 0)
			_Clt = 0.;
		
		// discard peak & valley of Y
		double save = _Peaks[_jlt]; 
		_Peaks.pop_back(); 
		_Peaks.pop_back();
		_Peaks.pop_back();
		_Peaks.push_back(save);
		_jlt -= 2;
		// stay in while loop
		retCode = LT_RERANGE;
	}

	return retCode;
}
bool lifetime_t::check_replaced()
{
	bool replaced = false;
	if ( (_replacement_option == 1 && (_Clt - tolerance) <= _replacement_capacity) || _replacement_scheduled)
	{
		_replacements++;
		_Clt = bilinear(0.,0);
		_Dlt = 0.;
		_nCycles = 0;
		_jlt = 0;
		_Xlt = 0;
		_Ylt = 0;
		_Range = 0;
		_Peaks.clear();
		replaced = true;
		_replacement_scheduled = false;
	}
	return replaced;
}
void lifetime_t::force_replacement()
{
	_replacement_scheduled = true;
}

void lifetime_t::reset_replacements(){ _replacements = 0; }
int lifetime_t::replacements(){ return _replacements; }
int lifetime_t::cycles_elapsed(){return _nCycles;}
double lifetime_t::capacity_percent(){ return _Clt; }
double lifetime_t::cycle_range(){ return _Range; }


double lifetime_t::bilinear(double DOD, int cycle_number)
{
	/*
	Work could be done to make this simpler
	Current idea is to interpolate first along the C = f(n) curves for each DOD to get C_DOD_, C_DOD_+ 
	Then interpolate C_, C+ to get C at the DOD of interest
	*/

	std::vector<double> D_unique_vect;
	std::vector<double> C_n_low_vect;
	std::vector<double> D_high_vect;
	std::vector<double> C_n_high_vect;
	std::vector<int> low_indices;
	std::vector<int> high_indices;
	double D = 0.;
	int n = 0;
	double C = 100;

	// get unique values of D
	D_unique_vect.push_back(_DOD_vect[0]);
	for (int i = 0; i < _DOD_vect.size(); i++){
		bool contained = false;
		for (int j = 0; j < D_unique_vect.size(); j++){
			if (_DOD_vect[i] == D_unique_vect[j]){
				contained = true;
				break;
			}
		}
		if (!contained){
			D_unique_vect.push_back(_DOD_vect[i]);
		}
	}
	n = D_unique_vect.size();

	if (n > 1)
	{
		// get where DOD is bracketed [D_lo, DOD, D_hi]
		double D_lo = 0;
		double D_hi = 100;

		for (int i = 0; i < _DOD_vect.size(); i++)
		{
			D = _DOD_vect[i];
			if (D < DOD && D > D_lo)
				D_lo = D;
			else if (D > DOD && D < D_hi)
				D_hi = D;
		}

		// Seperate table into bins
		double D_min = 100.;
		double D_max = 0.;

		for (int i = 0; i < _DOD_vect.size(); i++)
		{
			D = _DOD_vect[i];
			if (D == D_lo)
				low_indices.push_back(i);
			else if (D == D_hi)
				high_indices.push_back(i);

			if (D < D_min){ D_min = D; }
			else if (D > D_max){ D_max = D; }
		}
		size_t n_rows_lo = low_indices.size();
		size_t n_rows_hi = high_indices.size();
		size_t n_cols = 2;

		// If we aren't bounded, fill in values
		if (n_rows_lo == 0)
		{
			// Assumes 0% DOD
			for (int i = 0; i < n_rows_hi; i++)
			{
				C_n_low_vect.push_back(0. + i * 500); // cycles
				C_n_low_vect.push_back(100.); // 100 % capacity
			}
		}
		else if (n_rows_hi == 0)
		{
			// Assume 100% DOD
			for (int i = 0; i < n_rows_lo; i++)
			{
				C_n_high_vect.push_back(100. + i * 500); // cycles
				C_n_high_vect.push_back(80 - i*10); // % capacity
			}
		}

		if (n_rows_lo != 0)
		{
			for (int i = 0; i < n_rows_lo; i++)
			{
				C_n_low_vect.push_back(_cycles_vect[low_indices[i]]);
				C_n_low_vect.push_back(_capacities_vect[low_indices[i]]);
			}
		}
		if (n_rows_hi != 0)
		{
			for (int i = 0; i < n_rows_hi; i++)
			{
				C_n_high_vect.push_back(_cycles_vect[high_indices[i]]);
				C_n_high_vect.push_back(_capacities_vect[high_indices[i]]);
			}
		}
		n_rows_lo = C_n_low_vect.size() / n_cols;
		n_rows_hi = C_n_high_vect.size() / n_cols;

		if (n_rows_lo == 0 || n_rows_hi == 0)
		{
			// need a safeguard here
		}

		util::matrix_t<double> C_n_low(n_rows_lo, n_cols, &C_n_low_vect);
		util::matrix_t<double> C_n_high(n_rows_lo, n_cols, &C_n_high_vect);

		// Compute C(D_lo, n), C(D_hi, n)
		double C_Dlo = util::linterp_col(C_n_low, 0, cycle_number, 1);
		double C_Dhi = util::linterp_col(C_n_high, 0, cycle_number, 1);

		if (C_Dlo < 0.)
			C_Dlo = 0.;
		if (C_Dhi > 100.)
			C_Dhi = 100.;

		// Interpolate to get C(D, n)
		C = util::interpolate(D_lo, C_Dlo, D_hi, C_Dhi, DOD);
	}
	// just have one row, single level interpolation
	else
	{
		C = util::linterp_col(_batt_lifetime_matrix, 1, cycle_number, 2);
	}

	return C;
}


/*
Define Thermal Model
*/
thermal_t::thermal_t(double mass, double length, double width, double height, 
	double Cp,  double h, double T_room, 
	const util::matrix_t<double> &c_vs_t )
{
	_cap_vs_temp = c_vs_t;
	_mass = mass;
	_length = length;
	_width = width;
	_height = height;
	_Cp = Cp;
	_h = h;
	_T_room = T_room;
	_R = 0.004;
	_capacity_percent = 100;

	// assume all surfaces are exposed
	_A = 2 * (length*width + length*height + width*height);

	// initialize to room temperature
	_T_battery = T_room;

	//initialize maximum temperature
	_T_max = 400.;

	// curve fit
	int n = _cap_vs_temp.nrows();
	for (int i = 0; i < n; i++)
	{
		_cap_vs_temp(i,0) += 273.15; // convert C to K
	}
}
thermal_t * thermal_t::clone(){ return new thermal_t(*this); }
void thermal_t::copy(thermal_t *& thermal)
{
	thermal->_mass = _mass;
	thermal->_length = _length;
	thermal->_width = _width;
	thermal->_height = _height;
	thermal->_Cp = _Cp;
	thermal->_h = _h;
	thermal->_T_room = _T_room;
	thermal->_R = _R;
	thermal->_A = _A;
	thermal->_T_battery = _T_battery;
	thermal->_capacity_percent = _capacity_percent;
	thermal->_T_max = _T_max;

}
void thermal_t::replace_battery()
{ 
	_T_battery = _T_room; 
	_capacity_percent = 100.;
}

#define HR2SEC 3600.0


void thermal_t::updateTemperature(double I, double R, double dt)
{
	_R = R;
	if (trapezoidal(I, dt*HR2SEC) < _T_max && trapezoidal(I, dt*HR2SEC) > 0)
		_T_battery = trapezoidal(I, dt*HR2SEC);
	else if (rk4(I, dt*HR2SEC) < _T_max && rk4(I, dt*HR2SEC) > 0)
		_T_battery = rk4(I, dt*HR2SEC);
	else if (implicit_euler(I, dt*HR2SEC) < _T_max && implicit_euler(I, dt*HR2SEC) > 0)
		_T_battery = implicit_euler(I, dt*HR2SEC);
	else
		_message.add("Computed battery temperature below zero or greater than max allowed, consider reducing C-rate");
}

double thermal_t::f(double T_battery, double I)
{
	return (1 / (_mass*_Cp)) * ((_h*(_T_room - T_battery)*_A) + pow(I, 2)*_R);
}
double thermal_t::rk4( double I, double dt)
{
	double k1 = dt*f(_T_battery, I);
	double k2 = dt*f(_T_battery + k1 / 2, I);
	double k3 = dt*f(_T_battery + k2 / 2, I);
	double k4 = dt*f(_T_battery + k3, I);
	return (_T_battery + (1. / 6)*(k1 + k4) + (1. / 3.)*(k2 + k3));
}
double thermal_t::trapezoidal(double I, double dt)
{
	double B = 1 / (_mass*_Cp); // [K/J]
	double C = _h*_A;			// [W/K]
	double D = pow(I, 2)*_R;	// [Ohm A*A]
	double T_prime = f(_T_battery, I);	// [K]

	return (_T_battery + 0.5*dt*(T_prime + B*(C*_T_room + D))) / (1 + 0.5*dt*B*C);
} 
double thermal_t::implicit_euler(double I, double dt)
{
	double B = 1 / (_mass*_Cp); // [K/J]
	double C = _h*_A;			// [W/K]
	double D = pow(I, 2)*_R;	// [Ohm A*A]
	double T_prime = f(_T_battery, I);	// [K]

	return (_T_battery + dt*(B*C*_T_room + D)) / (1 + dt*B*C);
}
double thermal_t::T_battery(){ return _T_battery; }
double thermal_t::capacity_percent()
{ 
	double percent = util::linterp_col(_cap_vs_temp, 0, _T_battery, 1); 

	if (percent < 0 || percent > 100)
	{
		percent = 100;
		_message.add("Unable to determine capacity adjustment for temperature, ignoring");
	}

	return percent;
}
/*
Define Losses
*/
losses_t::losses_t(lifetime_t * lifetime, thermal_t * thermal, capacity_t* capacity, double_vec batt_system_losses)
{
	_lifetime = lifetime;
	_thermal = thermal;
	_capacity = capacity;
	_system_losses = batt_system_losses;
	_nCycle = 0;
}
losses_t * losses_t::clone(){ return new losses_t(*this); }
void losses_t::copy(losses_t *& losses)
{
	losses->_lifetime = _lifetime;
	losses->_thermal = _thermal;
	losses->_capacity = _capacity;
	losses->_nCycle = _nCycle;
	losses->_system_losses = _system_losses;
}

void losses_t::replace_battery(){ _nCycle = 0; }
void losses_t::run_losses(double dt_hour)
{
	bool update_max_capacity = false;
	
	// if cycle number has changed, update max capacity
	if (_lifetime->cycles_elapsed() > _nCycle)
	{
		_nCycle++;
		_capacity->updateCapacityForLifetime(_lifetime->capacity_percent());
	}
	
	// modify max capacity based on temperature
	_capacity->updateCapacityForThermal(_thermal->capacity_percent());
}
/* 
Define Battery 
*/
battery_t::battery_t(){};
battery_t::battery_t(double dt_hour, int battery_chemistry)
{
	_dt_hour = dt_hour;
	_dt_min = dt_hour * 60;
	_battery_chemistry = battery_chemistry;
}

battery_t::battery_t(const battery_t& battery)
{
	_capacity = battery.capacity_model()->clone();
	_voltage = battery.voltage_model()->clone();
	_thermal = battery.thermal_model()->clone();
	_lifetime = battery.lifetime_model()->clone();
	_losses = battery.losses_model()->clone();
	_battery_chemistry = battery._battery_chemistry;
	_dt_hour = battery._dt_hour;
	_dt_min = battery._dt_min;
	_firstStep = battery._firstStep;
}
void battery_t::copy(const battery_t& battery)
{
	battery.capacity_model()->copy(_capacity);
	battery.voltage_model()->copy(_voltage);
	battery.thermal_model()->copy(_thermal);
	battery.lifetime_model()->copy(_lifetime);
	battery.losses_model()->copy(_losses);
	_battery_chemistry = battery._battery_chemistry;
	_dt_hour = battery._dt_hour;
	_dt_min = battery._dt_min;
	_firstStep = battery._firstStep;
}

//void battery_t::operator=(const battery_t& battery){}
void battery_t::delete_clone()
{
	if (_capacity) delete _capacity;
	if (_voltage) delete _voltage;
	if (_thermal) delete _thermal;
	if (_lifetime) delete _lifetime;
	if (_losses) delete _losses;
}
void battery_t::initialize(capacity_t *capacity, voltage_t * voltage, lifetime_t * lifetime, thermal_t * thermal, losses_t * losses)
{
	_capacity = capacity;
	_lifetime = lifetime;
	_voltage = voltage;
	_thermal = thermal;
	_losses = losses;
	_firstStep = true;
}

void battery_t::run(double I)
{	
	// Compute temperature at end of timestep
	runThermalModel(I);
	runCapacityModel(I);
	runVoltageModel();

	if (_capacity->chargeChanged())
		runLifetimeModel(_capacity->prev_DOD());
	else if (_firstStep)
	{
		runLifetimeModel(_capacity->DOD());
		_firstStep = false;
	}

	runLossesModel();
}
void battery_t::runThermalModel(double I)
{
	_thermal->updateTemperature(I, _voltage->R(), _dt_hour);
}

void battery_t::runCapacityModel(double I)
{
	_capacity->updateCapacity(I, _dt_hour );
}

void battery_t::runVoltageModel()
{
	_voltage->updateVoltage(_capacity, _thermal, _dt_hour);
}

void battery_t::runLifetimeModel(double DOD)
{
	_lifetime->rainflow(DOD);
	if (_lifetime->check_replaced())
	{
		_capacity->replace_battery();
		_thermal->replace_battery();
		_losses->replace_battery();
	}
}
void battery_t::runLossesModel()
{
	_losses->run_losses(_dt_hour);
}
capacity_t * battery_t::capacity_model() const { return _capacity; }
voltage_t * battery_t::voltage_model() const { return _voltage; }
lifetime_t * battery_t::lifetime_model() const { return _lifetime; }
thermal_t * battery_t::thermal_model() const { return _thermal; }
losses_t * battery_t::losses_model() const { return _losses; }

double battery_t::battery_charge_needed()
{
	double charge_needed = _capacity->qmax() - _capacity->q0();
	if (charge_needed > 0)
		return charge_needed;
	else
		return 0.;
}
double battery_t::battery_energy_to_fill()
{
	double battery_voltage = this->battery_voltage(); // [V] 
	double charge_needed_to_fill = this->battery_charge_needed(); // [Ah] - qmax - q0
	double energy_needed_to_fill = (charge_needed_to_fill * battery_voltage)*util::watt_to_kilowatt;  // [kWh]
	return energy_needed_to_fill;
}
double battery_t::battery_power_to_fill()
{
	// in one time step
	return (this->battery_energy_to_fill() / _dt_hour);
}

double battery_t::battery_charge_total(){return _capacity->q0();}
double battery_t::battery_charge_maximum(){ return _capacity->qmax(); }
double battery_t::cell_voltage(){ return _voltage->cell_voltage();}
double battery_t::battery_voltage(){ return _voltage->battery_voltage();}
double battery_t::battery_voltage_nominal(){ return _voltage->battery_voltage_nominal(); }
double battery_t::battery_soc(){ return _capacity->SOC(); }
