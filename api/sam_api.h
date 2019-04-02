#ifndef SYSTEM_ADVISOR_MODEL_SAM_API_H
#define SYSTEM_ADVISOR_MODEL_SAM_API_H

#include "visibility.h"

#ifdef __cplusplus
extern "C" {
#endif



//
// Error handling
//

typedef struct error* SAM_error;

SAM_EXPORT
SAM_error new_error();
SAM_EXPORT
const char* error_message(SAM_error error);
SAM_EXPORT
void error_destruct(SAM_error error);



//
// SAM table/dictionary methods
//

SAM_EXPORT typedef void* SAM_table;

SAM_EXPORT SAM_table SAM_table_construct(SAM_error *err);

SAM_EXPORT void SAM_table_destruct(SAM_table t, SAM_error *err);

SAM_EXPORT void SAM_table_unassign_entry(SAM_table t, const char *key, SAM_error *err);

/// Assignments

SAM_EXPORT void SAM_table_set_num(SAM_table t, const char* key, float num, SAM_error *err);

SAM_EXPORT void SAM_table_set_array(SAM_table t, const char* key, float* arr, int n, SAM_error *err);

SAM_EXPORT void SAM_table_set_matrix(SAM_table t, const char* key, float* arr, int nrows, int ncols, SAM_error *err);

SAM_EXPORT void SAM_table_set_string(SAM_table t, const char* key, const char* str, SAM_error *err);

/// Get references to table entries

SAM_EXPORT float * SAM_table_get_num(SAM_table t, const char *key, SAM_error *err);

SAM_EXPORT float * SAM_table_get_array(SAM_table t, const char *key, int *n, SAM_error *err);

SAM_EXPORT float * SAM_table_get_matrix(SAM_table t, const char *key, int *nrows, int *ncols, SAM_error *err);

/// Read table entries

SAM_EXPORT const float SAM_table_read_num(SAM_table t, const char *key, SAM_error *err);

SAM_EXPORT const float * SAM_table_read_array(SAM_table t, const char *key, int *n, SAM_error *err);

SAM_EXPORT const float * SAM_table_read_matrix(SAM_table t, const char *key, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT const char * SAM_table_read_string(SAM_table t, const char* key, SAM_error *err);



//
// Loading setter and getter functions by Technology, group, and variable names
//

SAM_EXPORT void *SAM_load_library(const char *filepath, SAM_error *err);

SAM_EXPORT typedef void (*SAM_set_float_t)(void*, float, SAM_error*);

SAM_EXPORT typedef void (*SAM_set_array_t)(void*, float*, int, SAM_error*);

SAM_EXPORT typedef void (*SAM_set_matrix_t)(void*, float*, int, int, SAM_error*);

SAM_EXPORT typedef void (*SAM_set_string_t)(void*, const char*, SAM_error*);

SAM_EXPORT typedef void (*SAM_set_table_t)(void*, SAM_table, SAM_error*);


SAM_EXPORT typedef float (*SAM_get_float_t)(void*, SAM_error*);

SAM_EXPORT typedef float* (*SAM_get_array_t)(void*, int*, SAM_error*);

SAM_EXPORT typedef float* (*SAM_get_matrix_t)(void*, int*, int*, SAM_error*);

SAM_EXPORT typedef const char* (*SAM_get_string_t)(void*, SAM_error*);

SAM_EXPORT typedef SAM_table (*SAM_get_table_t)(void*, SAM_error*);

/// Functions that set Model parameters

SAM_EXPORT SAM_set_float_t
SAM_set_float_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_set_array_t
SAM_set_array_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_set_matrix_t
SAM_set_matrix_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_set_string_t
SAM_set_string_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_set_table_t
SAM_set_table_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

/// Functions that get Model parameters

SAM_EXPORT SAM_get_float_t
SAM_get_float_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_get_array_t
SAM_get_array_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_get_matrix_t
SAM_get_matrix_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_get_string_t
SAM_get_string_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_get_table_t
SAM_get_table_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

//
// Running single simulations
//

SAM_EXPORT int SAM_module_exec(const char* cmod, void* data, int verbosity, SAM_error *err);



#ifdef __cplusplus
} // extern "C"
#endif


#endif //SYSTEM_ADVISOR_MODEL_SAM_API_H
