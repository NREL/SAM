#ifndef SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H
#define SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H

#include <string>
#include <iostream>
#include <dlfcn.h>


#include "ErrorHandler.h"
#include "sam_api.h"

typedef float (*SAM_set_float_t)();



/**
 * exploratory loading from file by name could upgrade to json libraries such as RapidJSON for better performance
 */

class SystemLoader {
private:
    void* handle;

public:

    SystemLoader(){

        char* pPath;
        pPath = getenv ("SAMNTDIR");
        if (pPath!=NULL)
            printf ("The current path is: %s \n",pPath);
        std::string lib_path = std::string(pPath) + "/api/SAM_apid.dylib";

        handle = dlopen(lib_path.c_str(), RTLD_LAZY);

        if (!handle) {
            std::string msg(dlerror());
            throw std::runtime_error( "Cannot open library: " + msg );
        }

        // reset errors
        dlerror();
    }

    void loadString(void* system, std::string cmod, std::string var_name, std::string value){

    }

    void loadFloat(void* system, std::string cmod, std::string var_name, float value){
        SAM_set_float_t hello = (SAM_set_float_t) dlsym(handle, yourfunc.c_str());

    }


};


#endif //SYSTEM_ADVISOR_MODEL_SYSTEMLOADER_H
