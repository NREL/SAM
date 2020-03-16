#ifndef __sam_api_h
#define __sam_api_h

#include <ssc/sscapi.h>

#include "invoke.h"
#include "equations.h"
#include "variables.h"

#if defined(__WINDOWS__)&&defined(__DLL__)
#define SAMEXPORT __declspec(dllexport)
#else
#define SAMEXPORT
#endif

#ifndef __SSCLINKAGECPP__
#ifdef __cplusplus
extern "C" {
#endif
#endif // __SSCLINKAGECPP__


/** Returns the library version number as an integer.  Version numbers start at 1. */
SAMEXPORT int sam_version();

SAMEXPORT bool init_eqn_evaluator(std::string cmod_name);

SAMEXPORT double eval_eqn_double();

#ifndef __SSCLINKAGECPP__
#ifdef __cplusplus
} /* extern "C" */
#endif
#endif // __SSCLINKAGECPP__

#endif

