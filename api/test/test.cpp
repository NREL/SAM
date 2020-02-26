#include <iostream>
#include <string>
#include <vector>

#include "GenericSystem-External.h"

#include <ssc/sscapi.h>
#include <shared/lib_util.h>


void create(){
    GenericSystem system = GenericSystem();

    system.PowerPlant.set_derate(10.f);

    std::cout << system.PowerPlant.get_derate();

    system.PowerPlant.set_heat_rate(1.f);

    std::cout << system.PowerPlant.get_conv_eff();

    system.execute();
}

void loadFromFile(){

    SAM_table table = SAM_table_construct(ThrowOnError());

    SAM_table_set_num(table, "num", 5, ThrowOnError());

    float f[2] = {1,2};

    SAM_table_set_array(table, "arr", f, 2, ThrowOnError());


    float* n = SAM_table_get_num(table, "num", ThrowOnError());

    std::cout << "num is " << *n << "\n";

    int l;
    float* rf = SAM_table_get_array(table, "arr", &l, ThrowOnError());

    std::cout << rf[0] << ", " << rf[1] << "\n";

    rf[1] = 5;

    rf = SAM_table_get_array(table, "arr", &l, ThrowOnError());

    std::cout << rf[0] << ", " << rf[1] << "\n";

    SAM_table_set_string(table, "str", "Iamstring", ThrowOnError());

    const char* s = SAM_table_read_string(table, "str", ThrowOnError());

    std::cout << "str: " << s << "\n";

    s = "newstr";

    std::cout << "str: " << SAM_table_read_string(table, "str", ThrowOnError()) << "\n";

    std::vector<float> vec = {1,2};
    util::matrix_t<float> mat(2, 1, &vec);
    std::cout << "mat: " << mat.at(0, 0) << ", " << mat.at(1,0) << "\n";

    char assignment_err_str[128] = "error assigning ";
    strcat(assignment_err_str, s);

    std::cout << "strcat: " << assignment_err_str << "\n";



    GenericSystem system = GenericSystem("None");



    SAM_table_destruct(table, nullptr);

}

int main(int argc, char *argv[]){


    
    try {
//        create();

        loadFromFile();

    } catch (const std::exception& e) {
        std::cerr << "SAM error: " << e.what() << std::endl;
        return 1;
    }


    return 1;
}