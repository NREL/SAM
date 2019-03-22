#include <string>
#include <iostream>

#ifdef __WINDOWS__
#include "dlfcn_win.h"
#else
#include <dlfcn.h>
#endif

#include "SAM_api.h"

#define CHECK_DLL_LOADED() \
	if (!handle) {std::string msg(dlerror()); throw std::runtime_error( "Cannot open library: " + msg );}

#define CHECK_FUNC_LOADED() \
    if (!func) {std::string msg(dlerror()); throw std::runtime_error( "Cannot load function " + funcName + ": " + msg);}

SAM_EXPORT void* SAM_load_library(const char* filepath){

    void* handle = dlopen(filepath, RTLD_LAZY);

    CHECK_DLL_LOADED()

    // reset errors
    dlerror();

    return handle;
}

SAM_EXPORT SAM_set_float_t
SAM_load_float(void *handle, const std::string &cmod_symbol, const std::string &group, const std::string &var_name) {
    CHECK_DLL_LOADED()

    std::string funcName = "SAM_" + cmod_symbol + "_" + group + "_" + var_name + "_set";

    std::cout << funcName << "\n";

    auto func = (SAM_set_float_t) dlsym(handle, funcName.c_str());

    CHECK_FUNC_LOADED()

    return func;
}