#include <string>
#include <iostream>
#include <stdexcept>

#if defined(__WINDOWS__)||defined(WIN32)||defined(_WIN32)||defined(__MINGW___)||defined(_MSC_VER)
#include <Windows.h>
#define RTLD_LAZY   0x000 /* accept unresolved externs */

void *dll_open(const char *name) { return (void*) ::LoadLibraryA(name); }
void dll_close(void *handle) { ::FreeLibrary((HMODULE)handle); }
void *dll_sym(void *handle, const char *name) { return (void*) ::GetProcAddress((HMODULE)handle, name); }
#else
#include <dlfcn.h>
void *dll_open(const char *name) { return dlopen(name, RTLD_LAZY); }
void dll_close(void *handle) { dlclose(handle); }
void *dll_sym(void *handle, const char *name) { return dlsym(handle, name); }
#endif

#include "SAM_api.h"

#define CHECK_DLL_LOADED() \
	if (!handle) {std::string msg("Cannot open SAM library"); \
		 throw std::runtime_error( msg );}

#define CHECK_FUNC_LOADED() \
    if (!func) {throw std::runtime_error( "Cannot load function " + std::string(funcName) );}

SAM_EXPORT void* SAM_load_library(const char* filepath){

    void* handle = dll_open(filepath);
	
    CHECK_DLL_LOADED()


    return handle;
}

SAM_EXPORT SAM_set_float_t
SAM_load_float(void *handle, const char *cmod_symbol, const char *group, const char *var_name) {
    CHECK_DLL_LOADED()

    std::string funcName = "SAM_" + std::string(cmod_symbol) + "_" + std::string(group) + "_" + std::string(var_name) + "_set";

    std::cout << funcName << "\n";

    auto func = (SAM_set_float_t) dll_sym(handle, funcName.c_str());

    CHECK_FUNC_LOADED()

    return func;
}