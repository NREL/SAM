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

#ifndef SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H
#define SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H

#include <string>
#include <iostream>
#ifdef __WINDOWS__
	#include "dlfcn_win.h"
#else
	#include <dlfcn.h>
#endif

#include "include/visibility.h"
#include "ErrorHandler.h"
#include "include/SAM_api.h"

#ifdef __WINDOWS__
static std::string path = std::string(getenv("SAMNTDIR")) + "/api/SAM_apid.dll";
#else
static std::string path = std::string(getenv("SAMNTDIR")) + "/api/libSAM_apid.so";
#endif

static int PySAM_has_error_msg(SAM_error error, const char *msg){
    printf("step1b, %s\n", msg);

    const char* cc = error_message(error);
    printf("step2b\n");

    if ((cc != NULL) && (cc[0] != '\0')) {
        printf("step3b\n");
        char err_msg[256];
        strncat(err_msg, cc, strlen(err_msg) - 1);
        printf("step4b\n");
        strncat(err_msg, ". ", 2);
        strncat(err_msg, msg, strlen(msg));
        printf("step5b\n");
        printf("step6b\n");
        error_destruct(error);
        printf("step7b\n");
        return 1;
    }
    printf("step8b\n");
    error_destruct(error);
    printf("step9b\n");
    return 0;
}

class SystemLoader {
private:
    void* m_handle;
    void* m_system;

    static std::string path;


public:

    SystemLoader(void* system, std::string filepath){
        m_handle = SAM_load_library(filepath.c_str(), nullptr);
        m_system = system;
    }

    void loadString(const std::string& cmod_symbol, const std::string& group,
                    const std::string& var_name, std::string value){
        SAM_set_string_t Func = SAM_set_string_func(m_handle, cmod_symbol.c_str(), group.c_str(), var_name.c_str(),
                                                       nullptr);

        Func(m_system, value.c_str(), nullptr);

        SAM_get_string_t gFunc = SAM_get_string_func(m_handle, cmod_symbol.c_str(), group.c_str(), var_name.c_str(),
                                                    nullptr);

        std::cout << "getting from loadstring: :" << gFunc(m_system, nullptr) << "\n";
    }

    void loadArray(const std::string& cmod_symbol, const std::string& group,
                    const std::string& var_name, float* value, int length){
        SAM_set_array_t Func = SAM_set_array_func(m_handle, cmod_symbol.c_str(), group.c_str(), var_name.c_str(),
                                                    nullptr);

        Func(m_system, value, length, ThrowOnError());

        SAM_get_array_t gFunc = SAM_get_array_func(m_handle, cmod_symbol.c_str(), group.c_str(), var_name.c_str(),
                                                  nullptr);

        std::cout << "getting from loadarray: :" << gFunc(m_system, &length, nullptr) << "\n";

    }

    void loadFloat(const std::string& cmod_symbol, const std::string& group,
            const std::string& var_name, const float& value){

        SAM_error error = new_error();
        SAM_set_float_t floatFunc = SAM_set_float_func(m_handle, cmod_symbol.c_str(), group.c_str(), var_name.c_str(),
                                                       &error);
        if (PySAM_has_error_msg(error, "Either parameter does not exist or is not numeric type.")) return;


        floatFunc(m_system, value, nullptr);
    }


};


#endif //SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H
