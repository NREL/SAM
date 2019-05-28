/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __invoke_h
#define __invoke_h

#include <wx/string.h>
#include <lk/env.h>

#include <ssc/sscapi.h>
#include <ssc/cmod_windpower_eqns.h>

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

typedef void (*ssc_equation_ptr)(ssc_data_t data);

struct ssc_equation_entry{
    const char* name;
    ssc_equation_ptr func;
};

static ssc_equation_entry ssc_equation_table [] = {
        {"windpower_turbine_powercurve", windpower_turbine_powercurve},
        {nullptr, nullptr}
};


// helper function
class Case;
void invoke_get_var_info( Case *c, const wxString &name, lk::vardata_t &result );

#endif
