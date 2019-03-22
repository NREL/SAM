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
#elif __APPLE__
static std::string path = std::string(getenv("SAMNTDIR")) + "/api/SAM_apid.dylib";
#else
static std::string path = std::string(getenv("SAMNTDIR")) + "/api/SAM_apid.so";
#endif

class SystemLoader {
private:
    void* m_handle;
    void* m_system;

    static std::string path;


public:

    SystemLoader(void* system, std::string filepath){
        m_handle = SAM_load_library(filepath.c_str());
        m_system = system;
    }

    void loadString(std::string cmod, std::string var_name, std::string value){

    }

    void loadFloat(const std::string& cmod_symbol, const std::string& group,
            const std::string& var_name, const float& value){

        SAM_set_float_t floatFunc = SAM_load_float(m_handle, cmod_symbol, group, var_name);

        float conv_eff = floatFunc(m_system, value, nullptr);
    }


};


#endif //SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H
