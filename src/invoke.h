#ifndef __invoke_h
#define __invoke_h

#include <wx/string.h>
#include <lk_env.h>

// general functions like logging
lk::fcall_t* invoke_general_funcs();

// functions for invoking SSC
lk::fcall_t* invoke_ssc_funcs();

// functions for the startup script
lk::fcall_t* invoke_config_funcs();

// these functions expect a UICallbackContext* as user_data
lk::fcall_t* invoke_uicallback_funcs();

// these functions expect a CaseCallbackContext* as user_data
lk::fcall_t* invoke_casecallback_funcs();

// these functions expect a ResultsCallbackContext* as user_data
lk::fcall_t* invoke_resultscallback_funcs();

// these functions expect a LossDiagCallbackContext* as user_data
lk::fcall_t* invoke_lossdiag_funcs();

// functions that can be called in equations
lk::fcall_t* invoke_equation_funcs(); 



// helper function
class Case;
void invoke_get_var_info( Case *c, const wxString &name, lk::vardata_t &result );

#endif
