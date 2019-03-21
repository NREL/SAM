#ifndef SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H
#define SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H

#include <string>
#include <iostream>
#include <dlfcn.h>

#include <ssc/vartab.h>
#include "ErrorHandler.h"
#include "sam_api.h"

typedef float (*SAM_set_float_t)(void*, float, SAM_error*);



/**
 * exploratory loading from file by name could upgrade to json libraries such as RapidJSON for better performance
 */

class SystemLoader {
private:
    void* m_handle;
    void* m_system;

public:

    SystemLoader(void* system){
        auto* vt = static_cast<var_table*>(system);

        if (!vt){
            throw std::runtime_error("Error creating SystemLoader");
        }
        m_system = system;

        char* pPath;
        pPath = getenv ("SAMNTDIR");
        if (pPath!=NULL)
            printf ("The current path is: %s \n",pPath);
        std::string lib_path = std::string(pPath) + "/api/SAM_apid.dylib";

        m_handle = dlopen(lib_path.c_str(), RTLD_LAZY);

        if (!m_handle) {
            std::string msg(dlerror());
            throw std::runtime_error( "Cannot open library: " + msg );
        }

        // reset errors
        dlerror();
    }

    void loadString(std::string cmod, std::string var_name, std::string value){

    }

    void loadFloat(const std::string& cmod_symbol, const std::string& group,
            const std::string& var_name, const float& value){
        std::string funcName = "SAM_" + cmod_symbol + "_" + group + "_" + var_name + "_set";

        std::cout << "Loading " << funcName << ": ";

        auto floatFunc = (SAM_set_float_t) dlsym(m_handle, funcName.c_str());

        float conv_eff = floatFunc(m_system, value, nullptr);
        std::cout << conv_eff;
    }


};


#endif //SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H
