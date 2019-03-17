#include "ErrorHandler.h"
#include "test_api.h"

#include <iostream>
#include <string>
#include <vector>

#include <ssc/sscapi.h>

void create(){
    GenericSystem system = GenericSystem();

    system.PowerPlant.set_derate(10.f);

    std::cout << system.PowerPlant.get_derate();

    system.PowerPlant.set_heat_rate(1.f);

    std::cout << system.PowerPlant.get_conv_eff();
}

void ssc(){
    ssc_data_t data = ssc_data_create();
    ssc_data_set_number(data, "derate", 1.f);
    float f;
    ssc_data_get_number(data, "derate", &f);
    ssc_data_free(data);
}

int main(int argc, char *argv[]){

    create();
//    ssc();







    return 1;
}