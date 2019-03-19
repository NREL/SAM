#include "ErrorHandler.h"
#include "test_api.h"

#include <iostream>
#include <string>
#include <vector>

#include <ssc/sscapi.h>

void create(){
    GenericSystem system = GenericSystem();

//    system.PowerPlant.set_derate(10.f);

    std::cout << system.PowerPlant.get_derate();

    system.PowerPlant.set_heat_rate(1.f);

    std::cout << system.PowerPlant.get_conv_eff();
}

int main(int argc, char *argv[]){

    try {
//        create();
    } catch (const std::exception& e) {
        std::cerr << "Error during poll: " << e.what() << std::endl;
        return 1;
    }
    var_table* vt;

    float spe_reference = 0.f;
    float spe_eff0 = 1.f;
    float spe_rad0 = 2.f;
    float spe_eff1 = 3.f;
    float spe_rad1 = 4.f;

    // outputs
    float spe_power;

    auto switch_fx = [&]{
        float switch_var = 0.f;
        switch( (int)spe_reference ){
            case 0:
                switch_var = spe_eff0 / 100.000000f * spe_rad0;
                break;
            case 1:
                switch_var = spe_eff1 / 100.000000f * spe_rad1;
                break;
            default:
                throw;

        }
        return switch_var;
    };

    spe_power = switch_fx() * 2;
    std::cout << spe_power;

    return 1;
}