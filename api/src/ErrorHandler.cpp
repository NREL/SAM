#include <stdexcept>
#include <string>

#include "ErrorHandler.h"
#include "SAM_api.h"

SAM_error new_error(){
    return new error;
}

const char* error_message(SAM_error error)
{
    return error->message.c_str();
}

void error_destruct(SAM_error error)
{
    delete error;
}

void make_access_error(const std::string &obj_name, const std::string &var){
    std::string s = "Runtime error: get_" + var + " called for ";
    s += obj_name + " but " + "\"" + var + "\" not assigned\n";
    throw std::runtime_error(s);
}
