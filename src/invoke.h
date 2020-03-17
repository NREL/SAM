/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __invoke_h
#define __invoke_h

#include <wx/string.h>
#include <lk/env.h>

// version function
void fcall_samver( lk::invoke_t &cxt );


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

// these functions expect a CodeGenCallbackContext* as user_data
lk::fcall_t* invoke_codegencallback_funcs();

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
