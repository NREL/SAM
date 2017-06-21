#ifndef _STRING_UTIL_
#define _STRING_UTIL_ 1

#include <string>
#include <vector>

using namespace std;


//string and data handling methods
extern vector< string > split( const string &str, const string &delim, bool ret_empty=false, bool ret_delim=false );
extern string join( const vector< string > &list, const string &delim );
		
extern bool to_integer(const string &str, int *x);
extern bool to_float(const string &str, float *x);
extern bool to_double(const string &str, double *x);
extern bool to_bool(const string &str, bool &x);
		
extern string to_string( int x, const char *fmt="%d" );
extern string to_string( double x, const char *fmt="%lg" );


extern string lower_case( const string &in );
extern string upper_case( const string &in );

extern string ReplaceString(string subject, const string &search, const string &replace);
extern void ReplaceStringInPlace(string &subject, const string &search, const string &replace);


//------



#endif