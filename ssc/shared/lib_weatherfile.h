#ifndef __lib_weatherfile_h
#define __lib_weatherfile_h

#include <string>
#include <vector>  // needed to compile in typelib_vc2012



/***************************************************************************\

   Function humidity()
	This function calculates the relative humidity(%) based on the drybulb
	temperature(C) and the dewpoint temperature.  It uses equations and
	procedures presented in the 1993 ASHRAE Fundamentals Handbook, p6.7-10.
	If humidity cannot be calculated an error value of 999 is returned.
														  1/4/00
	List of Parameters Passed to Function:
	db     = dry bulb temperature in degrees C
	dpt    = dew point temperature in degrees C

	Variable Returned
	rh    = relative humidity in %, or error value of 999  

\***************************************************************************/
int calc_humidity(float db,float dpt);

/* This function calculates the dewpoint temperature(C) based on the drybulb
	temperature(C) and the relative humidity(%).  It uses equations and
	procedures presented in the 1993 ASHRAE Fundamentals Handbook, p6.7-10.
	If dewpoint cannot be calculated an error value of 99.9 is returned.

	List of Parameters Passed to Function:
	db     = dry bulb temperature in degrees C
	rh     = relative humidity in %

	Variable Returned
	dpt    = dew point temperature in degrees C, or error value of 99.9   */
float calc_dewpt(float db,float rh);

// Calculate wet bulb temperature from T (dry bulb, 'C), RH (%), Pressure (mbar)
// see http://www.ejournal.unam.mx/atm/Vol07-3/ATM07304.pdf for eqns.
double calc_twet( double T, double RH, double P );

struct weather_header {	
	weather_header() { reset(); }
	void reset();

	std::string location;
	std::string city;
	std::string state;
	std::string country;
	std::string source;
	std::string description;
	std::string url;
	bool interpmet;
	bool hasunits;
	double tz;
	double lat;
	double lon;
	double elev;
};

struct weather_record {		
	weather_record() { reset(); }
	void reset();
	
	int year;
	int month;
	int day;
	int hour;
	double minute;
	double gh;     // global (W/m2)
	double dn;     // direct (W/m2)
	double df;     // diffuse (W/m2)
	double poa;    // plane of array (W/m2)
	double wspd;   // wind speed (m/s)
	double wdir;   // wind direction (deg: N = 0 or 360, E = 90, S = 180,W = 270 )
	double tdry;   // dry bulb temp (C)
	double twet;   // wet bulb temp (C)
	double tdew;   // dew point temp (C)
	double rhum;   // relative humidity (%)
	double pres;   // pressure (mbar)
	double snow;   // snow depth (cm) 0-150
	double alb; // ground reflectance 0-1.  values outside this range mean it is not included
	double aod;    // aerosol optical depth
};

class weather_data_provider
{
	bool m_hdrInitialized;
	weather_header m_hdr;
public:
	
	enum { YEAR, MONTH, DAY, HOUR, MINUTE,
		GHI, DNI, DHI, POA, 
		TDRY, TWET, TDEW, 
		WSPD, WDIR, 
		RH, PRES, SNOW, ALB, AOD,
	_MAXCOL_ };

	weather_data_provider() : m_hdrInitialized( false ) { }
	virtual ~weather_data_provider() { }
	
	// pure virtuals
	virtual bool header( weather_header *hdr ) = 0;		
	virtual size_t start_sec() = 0; // start time in seconds, 0 = jan 1st midnight
	virtual size_t step_sec() = 0; // step time in seconds
	virtual size_t nrecords() = 0; // number of data records in file		
	virtual bool read( weather_record *r ) = 0; // reads one more record
	virtual void rewind() = 0;
	virtual const char *error( size_t idx = 0 ) = 0;
	virtual bool has_data_column( size_t id ) = 0;

	
	// some helper methods for ease of use of htis class
	virtual weather_header &header()  {
		if ( !m_hdrInitialized )
			m_hdrInitialized = header( &m_hdr );
			
		return m_hdr;
	}

	double lat() { return header().lat; }
	double lon() { return header().lon; }
	double tz() { return header().tz; }
	double elev() { return header().elev; }

};

class weatherfile : public weather_data_provider
{
private:
	bool m_ok;
	bool m_msg;
	int m_type;
	std::string m_file;
	int m_startYear;
	double m_time;
	std::string m_message;


	struct column
	{
		int index; // used for wfcsv to get column index in CSV file from which to read
		std::vector<float> data;
	};
	
	size_t m_startSec;
	size_t m_stepSec;
	size_t m_nRecords;

	column m_columns[_MAXCOL_];
	size_t m_index;


	weather_header m_hdr;

public:
	weatherfile();
	weatherfile( const std::string &file, bool header_only = false, bool interp = false );
	virtual ~weatherfile();

	void reset();

    void set_counter_to(int cur_index); 
    int get_counter_value();
	enum { INVALID, TMY2, TMY3, EPW, SMW, WFCSV };

	bool ok();
	int type();
	std::string filename();
	bool open( const std::string &file, bool header_only = false, bool interp = false );

	bool has_message() { return m_message.size() > 0; }
	std::string message() { return m_message; }


	static std::string normalize_city( const std::string &in );
	static bool convert_to_wfcsv( const std::string &input, const std::string &output );

	
	virtual bool header( weather_header *hdr );		
	virtual bool read( weather_record *r ); // reads one more record
	virtual void rewind();	
	virtual size_t start_sec(); // start time in seconds, 0 = jan 1st midnight
	virtual size_t step_sec(); // step time in seconds
	virtual size_t nrecords(); // number of data records in file		
	virtual const char *error( size_t idx = 0 );
	virtual bool has_data_column( size_t id );
	
};


#endif

