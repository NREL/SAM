#include <iostream>
#include <string>
#include <vector>

#include "GenericSystem-External.h"

#include <ssc/sscapi.h>


void create(){
    GenericSystem system = GenericSystem();

    system.PowerPlant.set_derate(10.f);

    std::cout << system.PowerPlant.get_derate();

    system.PowerPlant.set_heat_rate(1.f);

    std::cout << system.PowerPlant.get_conv_eff();

    system.execute();
}

void loadFromFile(){


    
    GenericSystem system = GenericSystem("None");
}

int main(int argc, char *argv[]){


    
    try {
        create();

        loadFromFile();

    } catch (const std::exception& e) {
        std::cerr << "SAM error: " << e.what() << std::endl;
        return 1;
    }


    return 1;
}