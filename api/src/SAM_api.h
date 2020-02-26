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


SAM_EXPORT typedef void* SAM_table;

//
// SAM_var is a variant type that form the entries of SAM_table
//

SAM_EXPORT typedef void* SAM_var;

SAM_EXPORT SAM_var SAM_var_construct(SAM_error *err);

SAM_EXPORT void SAM_var_destruct(SAM_var d, SAM_error *err);

SAM_EXPORT int SAM_var_query(SAM_var d, SAM_error *err);

SAM_EXPORT void SAM_var_size(SAM_var d, int* rows, int* cols, SAM_error *err);

SAM_EXPORT const char * SAM_var_get_string(SAM_var d, SAM_error *err);

SAM_EXPORT double SAM_var_get_number(SAM_var d, SAM_error *err);

SAM_EXPORT double* SAM_var_get_arr(SAM_var d, int* length, SAM_error *err);

SAM_EXPORT double* SAM_var_get_mat(SAM_var d, int* nrows, int* ncols, SAM_error *err);

SAM_EXPORT SAM_table SAM_var_get_table(SAM_var d, SAM_error *err);

SAM_EXPORT SAM_var SAM_var_get_datarr(SAM_var d, int r, SAM_error *err);

SAM_EXPORT SAM_var SAM_var_get_datmat(SAM_var d, int r, int c, SAM_error *err);

//
// SAM table/dictionary methods
//


SAM_EXPORT SAM_table SAM_table_construct(SAM_error *err);

SAM_EXPORT void SAM_table_destruct(SAM_table t, SAM_error *err);

SAM_EXPORT void SAM_table_unassign_entry(SAM_table t, const char *key, SAM_error *err);

/// Assignment copies of values to table

SAM_EXPORT void SAM_table_set_num(SAM_table t, const char* key, double num, SAM_error *err);

SAM_EXPORT void SAM_table_set_array(SAM_table t, const char* key, double* arr, int n, SAM_error *err);

SAM_EXPORT void SAM_table_set_matrix(SAM_table t, const char* key, double* arr, int nrows, int ncols, SAM_error *err);

SAM_EXPORT void SAM_table_set_string(SAM_table t, const char* key, const char* str, SAM_error *err);

SAM_EXPORT void SAM_table_set_table(SAM_table t, const char *key, SAM_table tab, SAM_error *err);

/// Get references to data stored in table

SAM_EXPORT double SAM_table_get_num(SAM_table t, const char *key, SAM_error *err);

SAM_EXPORT double * SAM_table_get_array(SAM_table t, const char *key, int *n, SAM_error *err);

SAM_EXPORT double * SAM_table_get_matrix(SAM_table t, const char *key, int *nrows, int *ncols, SAM_error *err);

SAM_EXPORT SAM_table SAM_table_get_table(SAM_table t, const char *key, SAM_error *err);

SAM_EXPORT const char * SAM_table_get_string(SAM_table t, const char* key, SAM_error *err);

SAM_EXPORT SAM_var SAM_table_get_datarr(SAM_table t, const char *key, int *len, SAM_error *err);

SAM_EXPORT SAM_var SAM_table_get_datmat(SAM_table t, const char *key, int *nrows, int *ncols, SAM_error *err);


/// Iterator functions

#define SAM_INVALID 0
#define SAM_STRING 1
#define SAM_NUMBER 2
#define SAM_ARRAY 3
#define SAM_MATRIX 4
#define SAM_TABLE 5
#define SAM_DATARR 6    // entries may be any type
#define SAM_DATMAT 7    // entries may be any type

SAM_EXPORT int SAM_table_size(SAM_table t, SAM_error *err);

// populates type and returns key name of entry at pos
SAM_EXPORT const char* SAM_table_key(SAM_table t, int pos, int *type, SAM_error *err);



//
// Loading setter and getter functions by Technology, group, and variable names
//

SAM_EXPORT void *SAM_load_library(const char *filepath, SAM_error *err);

SAM_EXPORT typedef void* (*SAM_construct_t)(const char*, SAM_error*);

SAM_EXPORT typedef void (*SAM_set_double_t)(void*, double, SAM_error*);

SAM_EXPORT typedef void (*SAM_set_array_t)(void*, double*, int, SAM_error*);

SAM_EXPORT typedef void (*SAM_set_matrix_t)(void*, double*, int, int, SAM_error*);

SAM_EXPORT typedef void (*SAM_set_string_t)(void*, const char*, SAM_error*);

SAM_EXPORT typedef void (*SAM_set_table_t)(void*, SAM_table, SAM_error*);


SAM_EXPORT typedef double (*SAM_get_double_t)(void*, SAM_error*);

SAM_EXPORT typedef double* (*SAM_get_array_t)(void*, int*, SAM_error*);

SAM_EXPORT typedef double* (*SAM_get_matrix_t)(void*, int*, int*, SAM_error*);

SAM_EXPORT typedef const char* (*SAM_get_string_t)(void*, SAM_error*);

SAM_EXPORT typedef SAM_table (*SAM_get_table_t)(void*, SAM_error*);

/// Functions that set Model parameters

SAM_EXPORT SAM_set_double_t
SAM_set_double_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_set_array_t
SAM_set_array_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_set_matrix_t
SAM_set_matrix_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_set_string_t
SAM_set_string_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

SAM_EXPORT SAM_set_table_t
SAM_set_table_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

/// Functions that get Model parameters

SAM_EXPORT SAM_get_double_t
SAM_get_double_func(void *handle, const char *cmod_symbol, const char *group, const char *var_name, SAM_error *err);

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
