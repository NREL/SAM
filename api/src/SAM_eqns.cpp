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

#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <iostream>

#include <ssc/sscapi.h>
#include <ssc/cmod_windpower_eqns.h>
#include <ssc/cmod_pvsamv1_eqns.h>
#include <ssc/cmod_merchantplant_eqns.h>
#include <ssc/cmod_analysisperiodchange_eqns.h>

#include "SAM_api.h"
#include "SAM_eqns.h"
#include "ErrorHandler.h"


SAM_EXPORT void SAM_windpower_turbine_powercurve_eqn(ssc_data_t data, SAM_error* err){
    translateExceptions(err, [&]{
        Turbine_calculate_powercurve(data);
    });
}

SAM_EXPORT void SAM_Reopt_size_battery_post_eqn(ssc_data_t data, SAM_error* err){
    translateExceptions(err, [&]{
        Reopt_size_battery_params(data);
    });
}

SAM_EXPORT void SAM_mp_ancillary_services_eqn(ssc_data_t data, SAM_error* err){
    translateExceptions(err, [&]{
        mp_ancillary_services(data);
    });
}

SAM_EXPORT void SAM_analysisperiodchange_eqn(ssc_data_t data, SAM_error* err) {
    translateExceptions(err, [&] {
        analysisperiodchange(data);
        });
}