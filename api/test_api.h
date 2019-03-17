#ifndef SYSTEM_ADVISOR_MODEL_TEST_API_H
#define SYSTEM_ADVISOR_MODEL_TEST_API_H

#include "SAM_GenericSystem.h"
#include <iostream>


//
// Error handling
//

struct Error {
    Error() : opaque(nullptr) {}

    ~Error()
    {
        if (opaque) {
            error_destruct(opaque);
        }
    }

    SAM_error opaque;
};

class ThrowOnError {
public:
    ~ThrowOnError() noexcept(false)
    {
        if (_error.opaque) {
            throw std::runtime_error(error_message(_error.opaque));
        }
    }

    operator SAM_error*() { return &_error.opaque; }

private:
    Error _error;
};

class GenericSystem_PowerPlant {
private:
    SAM_GenericSystem system;

public:
    GenericSystem_PowerPlant(){}

    void attach(SAM_GenericSystem enclosing_system){
        system = enclosing_system;
    }

    void set_derate(float n){
        SAM_GenericSystem_PowerPlant_derate_set(system, n, ThrowOnError());
    }

    void set_heat_rate(float n){
        SAM_GenericSystem_PowerPlant_heat_rate_set(system, n, ThrowOnError());
    }

    float get_derate(){
        return SAM_GenericSystem_PowerPlant_derate_get(system, ThrowOnError());
    }

    float get_conv_eff(){
        return SAM_GenericSystem_PowerPlant_conv_eff_eval(system, ThrowOnError());
    }
};

class Common{

};

class GenericSystem {
public:
    GenericSystem(const char* def = 0)
    :system(SAM_GenericSystem_construct(def, ThrowOnError()))
    {
        std::cout << "Generic constructor system" << system <<"\n" ;
        PowerPlant.attach(system);
    }

    ~GenericSystem(){
        SAM_GenericSystem_destruct(system);
    }

    GenericSystem_PowerPlant PowerPlant;
    Common Common;

    SAM_GenericSystem system;
private:
};



#endif //SYSTEM_ADVISOR_MODEL_TEST_API_H
