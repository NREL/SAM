#ifndef SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H
#define SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H

#include <vector>
#include <string>

#include <lk/env.h>

class config_extractor{
private:
    std::string config_name;

    lk::env_t m_env;
public:
    config_extractor(std::string name){
        config_name = name;
    }

    void register_callback_functions();
};

#endif //SYSTEM_ADVISOR_MODEL_CONFIG_EXTRACTOR_H
