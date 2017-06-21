#ifndef _EXCEPTIONS_
#define _EXCEPTIONS_ 1

#include <exception>
#include <string>
#include <stdexcept>

class spexception : public std::runtime_error
{
public:
	spexception(const std::string& message) 
		: std::runtime_error(message.c_str()){};
	spexception(const char *msg)
		: std::runtime_error(msg){};

};

#endif
