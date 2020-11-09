
#ifndef SYSTEM_ADVISOR_MODEL_BUILDER_PYSAM_H
#define SYSTEM_ADVISOR_MODEL_BUILDER_PYSAM_H

#include <vector>
#include <string>
#include "builder_generator.h"

class builder_PySAM {
private:
    std::string config_name;
    std::string config_symbol;
    builder_generator* root;

public:
    explicit builder_PySAM(builder_generator* ptr){
        root = ptr;
        config_name = root->config_name;
        config_symbol = root->config_symbol;
    }


    void create_PySAM_files(const std::string &cmod, const std::string &file_dir, bool stateful=false);
};


#endif //SYSTEM_ADVISOR_MODEL_BUILDER_PYSAM_H
