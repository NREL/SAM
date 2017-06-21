#ifndef __dllinvoke_h
#define __dllinvoke_h

#include <string>
#include <exception>

/* note:
 local implementations of ssc_* functions
 forward dynamically to the .dll

 however, if the .dll is not loaded or if the 
 symbol cannot be addressed:
      EXCEPTIONS ARE THROWN of type sscdll_error
*/


class sscdll_error : public std::exception
{
public:
	sscdll_error(const std::string &s,
			const std::string &f) : text(s), func(f) { }
	virtual ~sscdll_error() throw() { }
	virtual const char *what() { return std::string( text + " " + func ).c_str(); }
	std::string text;
	std::string func;
};

// __SSCLINKAGECPP__ defined so that API functions are not declared extern "C".
// This allows C++ dynamic library re-implementation wrapper to throw exceptions
// for DLL not loaded and symbol address lookup errors
#define __SSCLINKAGECPP__ 1 
#include <sscapi.h>
#undef __SSCLINKAGECPP__


/* these functions do NOT throw exceptions */
bool sscdll_load( const char *path );
void sscdll_unload();
bool sscdll_isloaded();


/* include shared ssc code here */
#include <lib_util.h>
#include <vartab.h>

#endif
