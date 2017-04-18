#include <stdio.h>
#include <cmath>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <limits>
#include <numeric>

#if defined(__WINDOWS__)||defined(WIN32)||defined(_WIN32)
#define CASECMP(a,b) _stricmp(a,b)
#define CASENCMP(a,b,n) _strnicmp(a,b,n)
#else
#define CASECMP(a,b) strcasecmp(a,b)
#define CASENCMP(a,b,n) strncasecmp(a,b,n)
#endif

#include "lib_util.h"
#include "lib_windfile.h"

#ifdef _MSC_VER
#define my_isnan(x) ::_isnan( x )
#else
#define my_isnan(x) std::isnan( x )
#endif

static void trim( char *buf )
{
	if (!buf) return;

	size_t len = strlen(buf);
	if (len > 0 && buf[len-1] == '\n') // strip newline
		buf[len-1] = 0;

	if (len > 1 && buf[len-2] == '\r') // strip carriage return
		buf[len-2] = 0;
}

static int locate2(char *buf, char **colidx, int colmax, char delim)
{
	trim(buf);

	char *p = buf;
	int i = 1;
	int ncols = 0;
	
	colidx[0] = p;
	while (p && *p && i < colmax)
	{
		p = strchr(p, delim);
		if (p)
		{
			*p = 0;
			colidx[i++] = ++p;
		}
	}

	ncols = i;

	while (i<colmax) colidx[i++] = 0;

	return ncols;
}

/* 
   version of strtok_r from (2010/9/24)
   http://www.koders.com/c/fid9E7961E1E818E911DA7C34DD56DD8782726B3FCD.aspx
   */
static char *gettoken2 (char *s, const char *delim, char **save_ptr)
{
	char *token;

	if (s == NULL)
		s = *save_ptr;

	/* Scan leading delimiters.  */
	s += strspn (s, delim);
	if (*s == '\0')
	{
		*save_ptr = s;
		return NULL;
	}

	/* Find the end of the token.  */
	token = s;
	s = strpbrk (token, delim);
	
	if (s == NULL)
		/* This token finishes the string.  */
		*save_ptr = strchr (token, '\0');
	else
	{
		/* Terminate the token and make *SAVE_PTR point past it.  */
		*s = '\0';
		*save_ptr = s + 1;
	}
	return token;
}

static int cmp_ext(const char *file, const char *ext)
{
	size_t len_ext, len_file;
	const char *extp;
	
	if (!file || !ext) return 0;
	len_ext = strlen(ext);
	len_file = strlen(file);
	extp = file+len_file-len_ext;
	
	if (extp < file) return 0;
	return CASENCMP(extp, ext, len_ext)==0 ? 1 : 0;
}

#define MBUFLEN 4096


winddata_provider::winddata_provider()
{
	year = 1900;
	lat = lon = elev = 0;
}
winddata_provider::~winddata_provider()
{
	// nothing to do
}

bool winddata_provider::find_closest( int& closest_index, int id, int ncols, double requested_height, int index_to_exclude /* = -1 */ )
{
	closest_index = -1;
	double height_diff = 1e99;
	for ( size_t i=0;i<m_dataid.size();i++ )
	{
		if ( (m_dataid[i] == id) && (i != index_to_exclude) )
		{
			if ( fabs(m_heights[i] - requested_height) < height_diff )
			{
				if ( index_to_exclude>=0 ) // we're looking for the next closest column for interpolation
				{	// the next closest measurement height can't be on the same side of requested_height as index_to_exclude
					if ( (m_heights[i] > requested_height) && (m_heights[index_to_exclude] > requested_height) ) continue;
					if ( (m_heights[i] < requested_height) && (m_heights[index_to_exclude] < requested_height) ) continue;
				}
				closest_index = i;
				height_diff = fabs(m_heights[i] - requested_height);
			}
		}
	}

	return (closest_index >= 0 && closest_index < ncols);
}

bool winddata_provider::can_interpolate( int index1, int index2, int ncols, double requested_height )
{
	if ( index1<0 || index2<0 ) return false;
	if ( index1>=ncols || index2>=ncols ) return false;
	if ( m_heights[index1]<requested_height && requested_height<m_heights[index2] ) return true; // height 1 < height 2
	if ( m_heights[index1]>requested_height && requested_height>m_heights[index2] ) return true; // height 1 > height 2

	return false;
}

bool winddata_provider::read( double requested_height,
	double *speed,
	double *direction,
	double *temperature,
	double *pressure,
	double *closest_speed_meas_height_in_file,
	double *closest_dir_meas_height_in_file,
	bool bInterpolate /*= false*/)
{	
	std::vector<double> values;
	if ( !read_line( values ) )
		return false;
	
	if (values.size() < m_heights.size() || values.size() < m_dataid.size())
		return false;

	size_t ncols = values.size();

	*speed = *direction = *temperature = *pressure = *closest_speed_meas_height_in_file = *closest_dir_meas_height_in_file = std::numeric_limits<double>::quiet_NaN();

	int index = -1, index2 = -1;
	if ( find_closest(index, SPEED, ncols, requested_height) )
	{
		if ( (bInterpolate) && (m_heights[index] != requested_height) && find_closest(index2, SPEED, ncols, requested_height, index) && can_interpolate(index, index2, ncols, requested_height)  )
		{
			*speed = util::interpolate(m_heights[index], values[index], m_heights[index2], values[index2], requested_height);
			*closest_speed_meas_height_in_file = requested_height;
		}
		else
		{
			*speed = values[index];
			*closest_speed_meas_height_in_file = m_heights[index];
		}
	}

	if (find_closest(index, DIR, ncols, requested_height) )
	{
		// interpolating direction is a little more complicated
		double dir1, dir2, angle;
		double ht1, ht2;
		bool interp_direction = ( (bInterpolate) && (m_heights[index] != requested_height) && find_closest(index2, DIR, ncols, requested_height, index) && can_interpolate(index, index2, ncols, requested_height)  );
		if ( interp_direction )
		{
			dir1 = values[index];
			dir2 = values[index2];
			if (my_isnan(dir1) || my_isnan(dir2))
				return false;
			while (dir1 < 0) dir1 += 360; //add 360 to negative values until it is positive
			while (dir1 >= 360) dir1 -= 360; //360 is set to zero, anything above 360 has 360 subtracted until it's below 360
			//dir1 = (values[index]<360) ? values[index] : 0; // set any 360 deg values to zero //error checking added 11/28/16 jmf
			while (dir2 < 0) dir2 += 360;
			while (dir2 >= 360) dir2 -= 360; //same error checking as above, added 11/28/16 jmf
			//dir2 = (values[index2]<360) ? values[index2] : 0;
			ht1 = m_heights[index];
			ht2 = m_heights[index2];
			if (dir1>dir2)
			{	// swap
				double temp = dir2;
				dir2=dir1;
				dir1 = temp;
				temp = ht2;
				ht2 = ht1;
				ht1 = temp;
			}
			angle = ( (dir2-dir1) < 180 ) ? (dir2-dir1) : 360.0 - (dir2-dir1);
			interp_direction &= (angle <= 180 ); // not sure if it makes sense to 'interpolate' between directions that are 180 deg apart?
		}
		
		if (interp_direction)
		{
			// special case when interpolating across straight north (0 degrees)
			if (dir1<90 && dir2>270) 
			{
				*direction = util::interpolate(ht1, dir1+90.0, ht2, dir2-270.0, requested_height)-90.0;
				if (*direction<0) *direction += 360.0;
			}
			else
				*direction = util::interpolate(ht1, dir1, ht2, dir2, requested_height);

			*closest_dir_meas_height_in_file = requested_height;
		}
		else
		{
			*direction = values[index];
			*closest_dir_meas_height_in_file = m_heights[index];
		}
	}

	if ( find_closest(index, TEMP, ncols, requested_height) )
	{
		if ( (bInterpolate) && (m_heights[index] != requested_height) && find_closest(index2, TEMP, ncols, requested_height, index) && can_interpolate(index, index2, ncols, requested_height)  )
			*temperature = util::interpolate(m_heights[index], values[index], m_heights[index2], values[index2], requested_height);
		else
			*temperature = values[index];
	}

	if ( find_closest(index, PRES, ncols, requested_height) )
	{
		if ( (bInterpolate) && (m_heights[index] != requested_height) && find_closest(index2, PRES, ncols, requested_height, index) && can_interpolate(index, index2, ncols, requested_height)  )
			*pressure = util::interpolate(m_heights[index], values[index], m_heights[index2], values[index2], requested_height);
		else
			*pressure = values[index];
	}

	bool found_all 
		= !my_isnan( *speed )
		&& !my_isnan( *direction )
		&& !my_isnan( *temperature )
		&& !my_isnan( *pressure );

	//add error checking. direction error checking performed in the averaging function.
	if (*speed < 0 || *speed > 120) //units are m/s, wind speed cannot be negative and highest recorded wind speed ever was 113 m/s (https://en.wikipedia.org/wiki/Wind_speed)
	{
		found_all = false;
		m_errorMsg = util::format("Error: wind speed of %d m/s found in weather file, this speed is outside the possible range of 0 to 120 m/s", *speed);
	}
	if (*temperature < -200 || *temperature > 100) //units are Celsius
	{
		found_all = false;
		m_errorMsg = util::format("Error: temperature of %d degrees Celsius found in weather file, this temperature is outside the possible range of -200 to 100 degrees C", *pressure);
	}
	if (*pressure < 0.5 || *pressure > 1.1) //units are atm, highest recorded pressure was 1085.7 Hectopascals (1.07 atm)  (https://en.wikipedia.org/wiki/Atmospheric_pressure#Records)
	{
		found_all = false;
		m_errorMsg = util::format("Error: atmospheric pressure of %d atm found in weather file, this pressure is outside the possible range of 0.5 to 1.1 atm", *pressure);
	}

	return found_all;

}




windfile::windfile()
	: winddata_provider()
{
	m_nrec = 0;
	m_buf = new char[MBUFLEN];
	m_fp = 0;
	close();
}

windfile::windfile( const std::string &file )
	: winddata_provider()
{
	m_nrec = 0;
	m_buf = new char[MBUFLEN];
	m_fp = 0;
	close();
	open( file );
}

windfile::~windfile()
{
	delete [] m_buf;
	if (m_fp != 0) fclose( m_fp );
}

bool windfile::ok()
{
	return m_fp != 0;
}


std::string windfile::filename()
{
	return m_file;
}

bool windfile::open( const std::string &file )
{
	close();
	if (file.empty()) return false;
	
	/*  // don't be strict about requiring .srw extensions for now
	if ( !cmp_ext(file.c_str(), "srw") )
		return false;
		*/
		
	m_fp = fopen(file.c_str(), "r");
	if (!m_fp)
	{
		m_errorMsg = "could not open file for reading: " + file;
		return false;
	}
		
	/* read header information */
	
	// read line 1 (header info
	fgets( m_buf, MBUFLEN-1, m_fp );
	char *cols[128];
	int ncols = locate2(m_buf, cols, 128, ',');

	if (ncols < 8)
	{
		m_errorMsg = util::format("error reading header (line 1).  At least 8 columns required, %d found.", ncols);
		fclose( m_fp );
		m_fp = 0;
		return false;
	}

	locid = std::string( cols[0] );
	city = std::string( cols[1] );
	state = std::string( cols[2] );
	country = std::string( cols[3] );

	year = atoi( cols[4] );
	lat = atof( cols[5] );
	lon = atof( cols[6] );
	elev = atof( cols[7] );

	
	// read line 2, description
	fgets( m_buf, MBUFLEN-1, m_fp );
	trim( m_buf );
	desc = std::string(m_buf);
	
	// read line 3, column names (must be pressure, temperature, speed, direction)
	fgets( m_buf, MBUFLEN-1, m_fp );
	ncols = locate2( m_buf, cols, 128, ',' );
	if (ncols < 3)
	{
		m_errorMsg = util::format("too few data column types found: %d.  at least 3 required.", ncols);
		fclose( m_fp );
		m_fp = 0;
		return false;
	}
	
	for (int i=0;i<ncols;i++)
	{
		std::string ctype = util::lower_case( cols[i] );
		if ( ctype == "temperature" || ctype == "temp" )
			m_dataid.push_back(TEMP);
		else if ( ctype == "pressure" || ctype == "pres" )
			m_dataid.push_back(PRES);
		else if ( ctype == "speed" || ctype == "velocity" )
			m_dataid.push_back(SPEED);
		else if ( ctype == "direction" || ctype == "dir" )
			m_dataid.push_back(DIR);
		else if ( ctype.length() > 0 )
		{
			m_errorMsg = util::format( "error reading data column type specifier in col %d of %d: '%s' len: %d", i+1, ncols, ctype.c_str(), ctype.length() );
			fclose( m_fp );
			m_fp = 0;
			return false;
		}
	}

	m_heights.resize( m_dataid.size(), -1  );


	// read line 4, units for each column (ignore this for now)
	fgets( m_buf, MBUFLEN-1, m_fp );

	// read line 5, height in meters for each data column
	fgets( m_buf, MBUFLEN-1, m_fp );
	ncols = locate2( m_buf, cols, 128, ',' );
	if ( ncols < (int)m_heights.size() )
	{
		m_errorMsg = util::format("too few columns in the height row.  %d required but only %d found", (int)m_heights.size(), ncols);
		fclose( m_fp );
		m_fp = 0;
		return false;
	}

	for (size_t i=0;i<m_heights.size();i++)
		m_heights[i] = atof( cols[i] );
	

	// read all the lines to determine the nubmer of records in the file
	m_nrec = 0;
	while( fgets( m_buf, MBUFLEN-1, m_fp ) )
		m_nrec++;

	// rewind the file and reposition right after the header information
	rewind( m_fp );
	for( size_t i=0;i<5;i++ )
		fgets( m_buf, MBUFLEN-1, m_fp );

	
	// ready to read line-by-line.  subsequent columns of data correspond to the
	// data types in m_dataid and measurement heights in m_heights
	m_file = file;
	return true;
}

void windfile::close()
{
	if ( m_fp != 0 ) fclose( m_fp );
	
	m_fp = 0;
	m_file.clear();
	city.clear();
	state.clear();
	locid.clear();
	country.clear();
	desc.clear();
	year = 1900;
	lat = lon = elev = 0.0;
	m_nrec = 0;
}

size_t windfile::nrecords()
{
	return m_nrec;
}

bool windfile::read_line( std::vector<double> &values )
{
	if ( !ok() ) return false;

	char *cols[128];
	fgets( m_buf, MBUFLEN-1, m_fp );
	int ncols = locate2( m_buf, cols, 128, ',' );	
	if (ncols >= m_heights.size() 
		&& ncols >= m_dataid.size())
	{
		values.resize( m_heights.size(), 0.0 );
		for (size_t i=0;i<m_heights.size();i++)
			values[i] = atof( cols[i] );

		return true;
	}
	else
		return false;
}
