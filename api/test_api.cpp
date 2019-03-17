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
        create();
    } catch (const std::exception& e) {
        std::cerr << "Error during poll: " << e.what() << std::endl;
        return 1;
    }




    return 1;
}