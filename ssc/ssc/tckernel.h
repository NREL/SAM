#ifndef __tckernel_h
#define __tckernel_h

#include "tcskernel.h"
#include "core.h"

class tcKernel : public tcskernel, public compute_module
{
public:
	tcKernel(tcstypeprovider *prov);
	virtual ~tcKernel();
	
	virtual void message( const std::string & text, int msgtype );
	virtual bool progress( float percent, const std::string &status );
	virtual bool converged( double time );
	void set_store_array_matrix_data( bool b ) { m_storeArrMatData = b; }
	void set_store_all_parameters( bool b ) { m_storeAllParameters = b; }
	virtual int simulate( double start, double end, double step, int max_iter = 100 );
	void set_unit_value_ssc_string( int id, const char *name );
	void set_unit_value_ssc_double( int id, const char *name );
	void set_unit_value_ssc_double( int id, const char *name, double x );
	void set_unit_value_ssc_array( int id, const char *name );
	void set_unit_value_ssc_matrix(int id, const char *name);
	void set_unit_value_ssc_matrix_transpose(int id, const char *name);

	// change ssc name to tcs name
	void set_unit_value_ssc_string(int id, const char *tcs_name, const char *ssc_name);
	void set_unit_value_ssc_double(int id, const char *tcs_name, const char *ssc_name);
	void set_unit_value_ssc_array(int id, const char *tcs_name, const char *ssc_name);
	void set_unit_value_ssc_matrix(int id, const char *tcs_name, const char *ssc_name);
	void set_unit_value_ssc_matrix_transpose(int id, const char *tcs_name, const char *ssc_name);

	bool set_output_array(const char *output_name, size_t len, double scaling = 1);
	bool set_output_array(const char *ssc_output_name, const char *tcs_output_name, size_t len, double scaling = 1);
	bool set_all_output_arrays();

	struct dataitem {
		dataitem( const char *s ) : sval(s) { }
		dataitem( const std::string &s ) : sval(s) { }
		dataitem( double d ) : dval(d) { }
		std::string sval;
		double dval;
	};

	struct dataset {
		unit *u;
		int uidx;
		int idx;
		std::string name;
		std::string units;
		std::string group;
		int type;
		std::vector<dataitem> values;
	};

	dataset *get_results(int idx);

private:
	bool m_storeArrMatData;
	bool m_storeAllParameters; // true = all inputs/outputs for all units will be saved for every time step; false = only store values that match SSC parameters defined as SSC_OUTPUT or SSC_INOUT
	std::vector< dataset > m_results;
	double m_start, m_end, m_step;
	size_t m_dataIndex;
};

#endif