#ifndef __lib_windfile_h
#define __lib_windfile_h

#include <string>
#include "lib_util.h"

class winddata_provider
{
public:
	enum { INVAL, 
		TEMP,  /* degrees Celsius */
		PRES,  /* atmospheres */
		SPEED, /* m/s */
		DIR  /* degrees */
	};

	winddata_provider();
	virtual ~winddata_provider();
	
	std::string city;
	std::string state;
	std::string locid;
	std::string country;
	std::string desc;
	int year;
	double lat;
	double lon;
	double elev;

	std::vector<int> types() { return m_dataid; }
	std::vector<double> heights() { return m_heights; }

	bool read( double requested_height,
		double *speed,
		double *direction,
		double *temperature,
		double *pressure,
		double *speed_meas_height,
		double *dir_meas_height,
		bool bInterpolate = false);
	
	virtual bool read_line( std::vector<double> &values ) = 0;
	virtual size_t nrecords() = 0;

	
	std::string error() { return m_errorMsg; }

protected:
	std::vector<int> m_dataid;
	std::vector<double> m_heights;
	std::string m_errorMsg;
	
	bool find_closest( int& closest_index, int id, int ncols, double requested_height, int index_to_exclude = -1 );
	bool can_interpolate( int index1, int index2, int ncols, double requested_height );


};

class windfile : public winddata_provider
{
private:
	FILE *m_fp;
	char *m_buf;
	std::string m_file;
	size_t m_nrec;

public:
	windfile();
	windfile( const std::string &file );
	virtual ~windfile();

	bool ok();
	std::string filename();
	void close();
	bool open( const std::string &file );
	
	virtual bool read_line( std::vector<double> &values );
	virtual size_t nrecords();
	
};

#endif

