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
const char* error_message(SAM_error error);
SAM_EXPORT
void error_destruct(SAM_error error);


#ifdef __cplusplus
} // extern "C"
#endif


#endif //SYSTEM_ADVISOR_MODEL_SAM_API_H
