#ifndef __common_h
#define __common_h

#include <memory>
#include <vector>
#include "core.h"

#include "lib_weatherfile.h"
#include "lib_pv_shade_loss_mpp.h"

extern var_info vtab_standard_financial[];
extern var_info vtab_standard_loan[];
extern var_info vtab_oandm[];
extern var_info vtab_depreciation[];
extern var_info vtab_tax_credits[];
extern var_info vtab_payment_incentives[];

extern var_info vtab_adjustment_factors[];
extern var_info vtab_dc_adjustment_factors[];
extern var_info vtab_sf_adjustment_factors[];
extern var_info vtab_technology_outputs[];

class adjustment_factors
{
	compute_module *m_cm;
	std::vector<float> m_factors;
	std::string m_error;
	std::string m_prefix;
public:
	adjustment_factors(compute_module *cm, const std::string &prefix);
	bool setup(int nsteps=8760);
	float operator()(size_t time);
	std::string error() { return m_error; }
};

class sf_adjustment_factors
{
	compute_module *m_cm;
	std::vector<float> m_factors;
	std::string m_error;
public:
	sf_adjustment_factors(compute_module *cm);
	bool setup(int nsteps=8760);
    int size();
	float operator()(size_t time);
	std::string error() { return m_error; }
};


class shading_factor_calculator
{
	std::vector<std::string> m_errors;
	util::matrix_t<double> m_azaltvals;
	bool m_enAzAlt;
	double m_diffFactor;

	// shading database mods
	int m_string_option;// 0=shading db, 1=average, 2=max, 3=min
	//ShadeDB8_mpp *m_db8;
	//std::auto_ptr<ShadeDB8_mpp> m_db8;
	double m_beam_shade_factor;
	double m_dc_shade_factor;

	// subhourly modifications
	int m_steps_per_hour;
	bool m_enTimestep;
	util::matrix_t<double> m_beamFactors;
	bool m_enMxH;
	util::matrix_t<double> m_mxhFactors;

public:
	shading_factor_calculator();
	bool setup(compute_module *cm, const std::string &prefix = "");
	std::string get_error(size_t i = 0);

	size_t get_row_index_for_input(size_t hour, size_t hour_step, size_t steps_per_hour);
	bool use_shade_db();

	// beam and diffuse loss factors (0: full loss, 1: no loss )
	bool fbeam(size_t hour, double solalt, double solazi, size_t hour_step = 0, size_t steps_per_hour = 1);
	// shading database instantiated once outside of shading factor calculator
	bool fbeam_shade_db(std::auto_ptr<ShadeDB8_mpp> & p_shadedb, size_t hour, double solalt, double solazi, size_t hour_step = 0, size_t steps_per_hour = 1, double gpoa = 0.0, double dpoa = 0.0, double pv_cell_temp = 0.0, int mods_per_str = 0, double str_vmp_stc = 0.0, double mppt_lo = 0.0, double mppt_hi = 0.0);

	double fdiff();

	double beam_shade_factor();
	double dc_shade_factor();
};


	
class weatherdata : public weather_data_provider
{
	std::string m_error;
	size_t m_startSec, m_stepSec, m_nRecords;
	weather_header m_hdr;
	std::vector< weather_record* > m_data;
	size_t m_index;
	std::vector<size_t> m_columns;

	struct vec {
		ssc_number_t *p;
		int len;
	};

	vec get_vector( var_data *v, const char *name, int *maxlen = 0 );	
	ssc_number_t get_number( var_data *v, const char *name );
	
	int name_to_id( const char *name );
public:
	weatherdata( var_data *data_table );
	virtual ~weatherdata();

	virtual bool header( weather_header *h );		
	virtual bool read( weather_record *r ); // reads one more record
	virtual void rewind();	
	virtual size_t start_sec() { return m_startSec; } // start time in seconds, 0 = jan 1st midnight
	virtual size_t step_sec() { return m_stepSec; } // step time in seconds
	virtual size_t nrecords() { return m_nRecords; } // number of data records in file		
	virtual const char *error( size_t idx = 0 );
	virtual bool has_data_column( size_t id );
};

#endif

