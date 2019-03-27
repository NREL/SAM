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
// Loading by name
//

SAM_EXPORT typedef float (*SAM_set_float_t)(void*, float, SAM_error*);

SAM_EXPORT void* SAM_load_library(const char* filepath);


SAM_EXPORT SAM_set_float_t
SAM_load_float(void *handle, const char *cmod_symbol, const char *group, const char *var_name);


#ifdef __cplusplus
} // extern "C"
#endif


#endif //SYSTEM_ADVISOR_MODEL_SAM_API_H
