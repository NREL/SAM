#ifndef SYSTEM_ADVISOR_MODEL_TEST_API_H
#define SYSTEM_ADVISOR_MODEL_TEST_API_H

#include <iostream>
#include <string>
#include <fstream>
#include <unordered_map>

#include "ssc/sscapi.h"

#include "include/visibility.h"


#include "SAM_GenericSystem-man.h"
#include "ErrorHandler.h"
#include "SystemLoader.h"


class GenericSystem_PowerPlant {
private:
    SAM_GenericSystem system;

public:
    GenericSystem_PowerPlant() {
        system = nullptr;
    }

    void attach(SAM_GenericSystem enclosing_system) {
        system = enclosing_system;
    }

    void set_derate(float n) {
        SAM_GenericSystem_PowerPlant_derate_fset(system, n, ThrowOnError());
    }

    void set_heat_rate(float n) {
        SAM_GenericSystem_PowerPlant_heat_rate_set(system, n, ThrowOnError());
    }

    float get_derate() {
        return SAM_GenericSystem_PowerPlant_derate_fget(system, ThrowOnError());
    }

    float get_conv_eff() {
        return SAM_GenericSystem_PowerPlant_conv_eff_eval(system, ThrowOnError());
    }
};

class Common {

};

class GenericSystem {
private:
    SAM_GenericSystem cmod;

public:
    SAM_GenericSystem system;

    GenericSystem_PowerPlant PowerPlant;
    Common common;

    GenericSystem(const char *def = 0)
            : system(SAM_GenericSystem_construct(def, ThrowOnError())) {
        std::cout << "Generic constructor system" << system << "\n";
        PowerPlant.attach(system);

        cmod = ssc_module_create("generic_system");
        if (!cmod)
            throw std::runtime_error("Could not create cmod");

        if (def != 0) {
            loadFromFile(def);
        }
    }

    ~GenericSystem() {
        SAM_GenericSystem_destruct(system);
        ssc_module_free(cmod);
    }

    bool loadFromFile(std::string def) {


        if (def == "None") {
            // load some data structure containing defaults until we have:
            for (size_t i = 0; i < 1000; i++) {
                std::string group = "PowerPlant";
                std::string var_name = "derate";
                std::string type = "float";
                float value = 1.f;

                SystemLoader loader(system, path);

                loader.loadFloat("GenericSystem", group, var_name, value);
                std::cout << PowerPlant.get_derate();

                float f[2] = {1, 2};
                loader.loadArray("GenericSystem", group, "energy_output_array", f, 2);

            }

        }
        return true;
    }

    int execute() {

        return ssc_module_exec(cmod, system);

    }
};


#endif //SYSTEM_ADVISOR_MODEL_TEST_API_H
