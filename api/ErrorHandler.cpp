#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "ErrorHandler.h"
#include "SAM_api.h"



const char* error_message(SAM_error error)
{
    return error->message.c_str();
}

void error_destruct(SAM_error error)
{
    delete error;
}


