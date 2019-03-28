#ifndef SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H
#define SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H

#include <string>
#include <iostream>
#ifdef __WINDOWS__
	#include "dlfcn_win.h"
#else
	#include <dlfcn.h>
#endif

#include "visibility.h"
#include "ErrorHandler.h"
#include "SAM_api.h"

#ifdef __WINDOWS__
static std::string path = std::string(getenv("SAMNTDIR")) + "/api/SAM_apid.dll";
#else
static std::string path = std::string(getenv("SAMNTDIR")) + "/api/libSAM_apid.so";
#endif

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

        std::cout << "getting from loadarray: :" << gFunc(m_system, &length, nullptr)[0] << "\n";

    }

    void loadFloat(const std::string& cmod_symbol, const std::string& group,
            const std::string& var_name, const float& value){

        SAM_set_float_t floatFunc = SAM_set_float_func(m_handle, cmod_symbol.c_str(), group.c_str(), var_name.c_str(),
                                                       nullptr);

        floatFunc(m_system, value, nullptr);
    }


};


#endif //SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H
