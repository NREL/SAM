#ifndef SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H
#define SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H

#include <string>
#include <vector>

#include "equations.h"

/**
 *  One per config?
 */

class equation_extractor : public EqnDatabase{
private:
    std::string config_name;
    std::vector<std::string> input_variables;
    std::vector<std::string> output_variables;

public:
    equation_extractor() = default;

    bool parse_script(std::string eqn_script);

    std::vector<std::string> get_output_variables();
};

#endif //SYSTEM_ADVISOR_MODEL_EQUATION_EXTRACTOR_H
