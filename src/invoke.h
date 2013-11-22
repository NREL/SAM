#ifndef __invoke_h
#define __invoke_h

#include <lk_env.h>

lk::fcall_t* invoke_general_funcs();
lk::fcall_t* invoke_config_funcs();
lk::fcall_t* invoke_uicallback_funcs();

#endif
