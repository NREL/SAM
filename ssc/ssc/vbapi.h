/**
\file vbapi.h

\brief SSC: SAM Simulation Core _stdcall wrapper for VBA

Be sure to use the correct library for your operating platform: ssc32
or ssc64. Opaque pointer types will be 4-byte pointer on 32-bit architectures,
and 8-byte pointer on 64-bit architectures.

Shared libraries have the .dll file extension on Windows,

\copyright 2012 National Renewable Energy Laboratory
\authors Aron Dobos, Steven Janzou

   C DLL Interface for Visual Basic (_stdcall)
*/

#ifndef __vbapi_h
#define __vbapi_h

#include "sscapi.h"

#ifdef __cplusplus
extern "C" {
#endif

#define VBCALL_CONVENTION _stdcall

SSCEXPORT long VBCALL_CONVENTION sscvb_version();
SSCEXPORT long VBCALL_CONVENTION sscvb_build_info(char *build_info, long len);

SSCEXPORT void *VBCALL_CONVENTION sscvb_data_create();
SSCEXPORT long VBCALL_CONVENTION sscvb_data_free(void * p_data);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_clear(void *p_data);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_unassign(void *p_data, const char *name);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_query(void *p_data, const char *name);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_first(void *p_data, const char *data_first);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_next(void *p_data, const char *data_next);

SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_string(void *p_data, const char *name, const char *value);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_number(void *p_data, const char *name, double value);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_array(void *p_data, const char *name, double *pvalues, long length);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_matrix(void *p_data, const char *name, double *pvalues, long nrows, long ncols);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_set_table(void *p_data, const char *name, void *table);

SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_string(void *p_data, const char *name, char *value, long len);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_number(void *p_data, const char *name, double *value);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_array(void *p_data, const char *name, double *pvalue, long length);
SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_matrix(void *p_data, const char *name, double *pvalue, long nrows, long ncols);
// TODO test this
SSCEXPORT long VBCALL_CONVENTION sscvb_data_get_table(void *p_data, const char *name, void *table);

SSCEXPORT void *VBCALL_CONVENTION sscvb_module_entry( long index);
SSCEXPORT long VBCALL_CONVENTION sscvb_entry_name(void *p_entry, const char *name);
SSCEXPORT long VBCALL_CONVENTION sscvb_entry_description(void *p_entry, const char *description);
SSCEXPORT long VBCALL_CONVENTION sscvb_entry_version(void *p_entry);

SSCEXPORT void *VBCALL_CONVENTION sscvb_module_create(const char *name);
SSCEXPORT long VBCALL_CONVENTION sscvb_module_free(void *p_mod);

SSCEXPORT void *VBCALL_CONVENTION sscvb_module_var_info(void *p_mod, long index);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_var_type(void *p_inf);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_data_type(void *p_inf);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_name(void *p_inf, const char *name);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_label(void *p_inf, const char *label);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_units(void *p_inf, const char *units);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_meta(void *p_inf, const char *meta);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_group(void *p_inf, const char *group);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_required(void *p_inf, const char *required);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_constraints(void *p_inf, const char *constraints);
SSCEXPORT long VBCALL_CONVENTION sscvb_info_uihint(void *p_inf, const char *uihint);

SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_set_print(long print);

SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_simple(const char *name, void *p_data);
SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_simple_nothread(const char *name, void *p_data, const char *msg);
SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec(void *p_mod, void *p_data);

// TODO test this
SSCEXPORT long VBCALL_CONVENTION sscvb_module_exec_with_handler(void *p_mod, void *p_data, long pf_handler, void *pf_user_data);

SSCEXPORT long VBCALL_CONVENTION sscvb_module_log(void *p_mod, long index, long *item_type, double *time, char *msg, long msg_len);
SSCEXPORT long VBCALL_CONVENTION __sscvb_segfault();


#ifdef __cplusplus
} /* extern "C" */
#endif


#endif
