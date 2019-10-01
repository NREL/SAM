#include <string>
#include <iostream>
#include <stdexcept>

#define _LIB_UTIL_CHECK_
#include <shared/vartab.h>
#include "sscapi.h"
#include "ErrorHandler.h"
#include "SAM_api.h"

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

void check_dll_loaded(void *handle){
	if (!handle) {
        std::string msg("Cannot open SAM library");
        throw std::runtime_error( msg );
	}
}

#define CHECK_FUNC_LOADED() \
    if (!func) {throw std::runtime_error( "Cannot load function " + funcName );}

SAM_EXPORT void *SAM_load_library(const char *filepath, SAM_error *err) {
    void *handle;
    translateExceptions(err, [&] {
        handle = dll_open(filepath);
        check_dll_loaded(handle);
    });

    return handle;
}

#define FUNC_NAME_STR(type) \
std::string funcName = "SAM_" + std::string(cmod_symbol) + "_" + std::string(group) + "_" + std::string(var_name) + "_" + std::string(type);

SAM_EXPORT SAM_set_double_t
SAM_set_double_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err) {
    SAM_set_double_t func = nullptr;
    translateExceptions(err, [&] {
        FUNC_NAME_STR("nset")
        check_dll_loaded(handle);
        func = (SAM_set_double_t) dll_sym(handle, funcName.c_str());
        CHECK_FUNC_LOADED()
    });
    return func;
}

SAM_EXPORT SAM_set_array_t
SAM_set_array_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err) {
    SAM_set_array_t func = nullptr;
    translateExceptions(err, [&] {
        FUNC_NAME_STR("aset");
        check_dll_loaded(handle);
        func = (SAM_set_array_t) dll_sym(handle, funcName.c_str());
        CHECK_FUNC_LOADED()
    });
    return func;
}

SAM_EXPORT SAM_set_matrix_t
SAM_set_matrix_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err) {
    SAM_set_matrix_t func = nullptr;
    translateExceptions(err, [&] {
        FUNC_NAME_STR("mset");
        check_dll_loaded(handle);
        func = (SAM_set_matrix_t) dll_sym(handle, funcName.c_str());
        CHECK_FUNC_LOADED()
    });
    return func;
}

SAM_EXPORT SAM_set_string_t
SAM_set_string_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err) {
    SAM_set_string_t func = nullptr;
    translateExceptions(err, [&] {
        FUNC_NAME_STR("sset");
        check_dll_loaded(handle);
        func = (SAM_set_string_t) dll_sym(handle, funcName.c_str());
        CHECK_FUNC_LOADED()
    });
    return func;
}

SAM_EXPORT SAM_set_table_t
SAM_set_table_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err) {
    SAM_set_table_t func = nullptr;
    translateExceptions(err, [&] {
        FUNC_NAME_STR("tset");
        check_dll_loaded(handle);
        func = (SAM_set_table_t) dll_sym(handle, funcName.c_str());
        CHECK_FUNC_LOADED()
    });
    return func;
}


SAM_EXPORT SAM_get_double_t
SAM_get_double_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err) {
    SAM_get_double_t func = nullptr;
    translateExceptions(err, [&] {
        FUNC_NAME_STR("nget");
        check_dll_loaded(handle);
        func = (SAM_get_double_t) dll_sym(handle, funcName.c_str());
        CHECK_FUNC_LOADED()
    });
    return func;
}

SAM_EXPORT SAM_get_array_t
SAM_get_array_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err) {
    SAM_get_array_t func = nullptr;
    translateExceptions(err, [&] {
        FUNC_NAME_STR("aget");
        check_dll_loaded(handle);
        func = (SAM_get_array_t) dll_sym(handle, funcName.c_str());
        CHECK_FUNC_LOADED()
    });
    return func;
}

SAM_EXPORT SAM_get_matrix_t
SAM_get_matrix_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err) {
    SAM_get_matrix_t func = nullptr;
    translateExceptions(err, [&] {
        FUNC_NAME_STR("mget");
        check_dll_loaded(handle);
        func = (SAM_get_matrix_t) dll_sym(handle, funcName.c_str());
        CHECK_FUNC_LOADED()
    });
    return func;
}

SAM_EXPORT SAM_get_string_t
SAM_get_string_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err) {
    SAM_get_string_t func = nullptr;
    translateExceptions(err, [&] {
        FUNC_NAME_STR("sget");
        check_dll_loaded(handle);
        func = (SAM_get_string_t) dll_sym(handle, funcName.c_str());
        CHECK_FUNC_LOADED()
    });
    return func;
}

SAM_EXPORT SAM_get_table_t
SAM_get_table_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err) {
    SAM_get_table_t func = nullptr;
    translateExceptions(err, [&] {
        FUNC_NAME_STR("tget");
        check_dll_loaded(handle);
        func = (SAM_get_table_t) dll_sym(handle, funcName.c_str());
        CHECK_FUNC_LOADED()
    });
    return func;
}

//
// Wrappers around ssc_data_t for SAM_table
//

#define GET_VARTABLE() \
auto *vt = static_cast<var_table*>(t); if (!vt) throw std::runtime_error("SAM_table is NULL.");

SAM_EXPORT SAM_table SAM_table_construct(SAM_error *err){
    SAM_table result = nullptr;
    translateExceptions(err, [&]{

        result = ssc_data_create();
    });
    return result;
}

SAM_EXPORT void SAM_table_set_num(SAM_table t, const char* key, double num, SAM_error *err){
    translateExceptions(err, [&]{
        GET_VARTABLE()
        vt->assign( key, var_data( num ) );
    });
}

SAM_EXPORT void SAM_table_set_array(SAM_table t, const char* key, double* arr, int n, SAM_error *err){
    translateExceptions(err, [&]{
        GET_VARTABLE()
        vt->assign( key, var_data( arr, n ) );
    });
}

SAM_EXPORT void SAM_table_set_matrix(SAM_table t, const char* key, double* arr, int nrows, int ncols, SAM_error *err){
    translateExceptions(err, [&]{
        GET_VARTABLE()
        vt->assign( key, var_data( arr, nrows, ncols ) );
    });
}

SAM_EXPORT void SAM_table_set_string(SAM_table t, const char* key, const char* str, SAM_error *err){
    translateExceptions(err, [&]{
        GET_VARTABLE()
        vt->assign( key, std::string( str ) );
    });
}

SAM_EXPORT void SAM_table_set_table(SAM_table t, const char *key, SAM_table tab, SAM_error *err){
    translateExceptions(err, [&]{
        GET_VARTABLE()
        auto *vt2 = static_cast<var_table*>(tab);
        if (!vt2) throw std::runtime_error("Entry value is not a table.");
        var_data *dat = vt->assign( key, var_data() );
        dat->type = SSC_TABLE;
        dat->table = *vt2;
    });
}

#define GET_VARDATA(key, ssc_type) \
var_data *dat = vt->lookup(key); \
if (!dat) {throw std::runtime_error(std::string(key) + " is not assigned");}\
if ( dat->type != ssc_type ) throw std::runtime_error(std::string(__func__) \
    + ": " + std::string(key) + " is type " + std::string(dat->type_name()));



SAM_EXPORT double* SAM_table_get_num(SAM_table t, const char *key, SAM_error *err){
    double* result = nullptr;
    translateExceptions(err, [&]{
        GET_VARTABLE()
        GET_VARDATA(key, SSC_NUMBER)
        result = &dat->num.at(0);
    });
    return result;
}

SAM_EXPORT double * SAM_table_get_array(SAM_table t, const char *key, int *n, SAM_error *err) {
    double* result = nullptr;
    translateExceptions(err, [&]{
        GET_VARTABLE()
        GET_VARDATA(key, SSC_ARRAY)
        if (n) *n = (int) dat->num.length();
        result = dat->num.data();
    });
    return result;
}

SAM_EXPORT double *
SAM_table_get_matrix(SAM_table t, const char *key, int *nrows, int *ncols, SAM_error *err) {
    double* result = nullptr;
    translateExceptions(err, [&]{
        GET_VARTABLE()
        GET_VARDATA(key, SSC_MATRIX)
        if (nrows) *nrows = (int) dat->num.nrows();
        if (ncols) *ncols = (int) dat->num.ncols();
        result = dat->num.data();
    });
    return result;
}

SAM_EXPORT SAM_table SAM_table_get_table(SAM_table t, const char *key, SAM_error *err){
    SAM_table result = nullptr;
    translateExceptions(err, [&]{
        GET_VARTABLE()
        GET_VARDATA(key, SSC_TABLE)
        result = &dat->table;
    });
    return result;
}

SAM_EXPORT const char* SAM_table_get_string(SAM_table t, const char* key, SAM_error *err){
    const char* result;
    translateExceptions(err, [&]{
        GET_VARTABLE()
        GET_VARDATA(key, SSC_STRING)
        result = dat->str.c_str();
    });
    return result;
}


SAM_EXPORT void SAM_table_unassign_entry(SAM_table t, const char *key, SAM_error *err) {
    translateExceptions(err, [&]{
        GET_VARTABLE()
        vt->unassign( key );
    });
}

SAM_EXPORT void SAM_table_destruct(SAM_table t, SAM_error *err) {
    translateExceptions(err, [&]{
        auto *vt = static_cast<var_table*>(t);
        delete vt;
    });
}


SAM_EXPORT int SAM_table_size(SAM_table t, SAM_error *err){
    int result = 0;
    translateExceptions(err, [&]{
        GET_VARTABLE()
        result = (int)vt->size();
    });
    return result;
}

SAM_EXPORT const char* SAM_table_key(SAM_table t, int pos, int *type, SAM_error *err){
    const char* result = nullptr;
    translateExceptions(err, [&]{
        GET_VARTABLE()
        result = vt->key(pos);
        *type = (int)vt->lookup(result)->type;
    });
    return result;
}


SAM_EXPORT int SAM_module_exec(const char* cmod, void* data, int verbosity, SAM_error *err){
    translateExceptions(err, [&]{
        ssc_module_t cm = ssc_module_create(cmod);
        if (!cm) throw std::runtime_error("Unable to create compute_module " + std::string(cmod));

        if(verbosity == 0){
            ssc_module_exec_set_print(0);
        }

        if (!ssc_module_exec( cm, data )){
            std::string str = std::string(cmod) + " execution error. " + ssc_module_log(cm, 0, nullptr, nullptr);
            ssc_module_free(cm);
            throw std::runtime_error(str);
        }
        ssc_module_free(cm);
    });
    return 1;
}