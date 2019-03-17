#ifndef SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_INFO_H
#define SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_INFO_H

#include <map>
#include <vector>
#include <string>
#include <algorithm>

static std::map<std::string, std::vector<std::string>> cmod_to_extra_modules = {
        {"generic", {"a"}}
};

static std::map<std::string, std::vector<std::string>> extra_modules_to_members = {
        {"generic", {"a"}}
};

static std::string find_module_of_var(std::string var, std::string cmod){
    std::vector<std::string> extra_groups = cmod_to_extra_modules[cmod];
    if (extra_groups.size() == 0){
        return "";
    }
    for (size_t g = 0; g < extra_groups.size(); g++){
        std::vector<std::string> group_members = extra_modules_to_members[extra_groups[g]];
        if (std::find(group_members.begin(), group_members.end(), var) != group_members.end())
            return extra_groups[g];
    }
    return "";
}

#endif //SYSTEM_ADVISOR_MODEL_BUILDER_GENERATOR_INFO_H
