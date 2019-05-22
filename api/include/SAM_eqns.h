#ifndef SYSTEM_ADVISOR_MODEL_SAM_EQNS_H
#define SYSTEM_ADVISOR_MODEL_SAM_EQNS_H

typedef void* ssc_data_t;

typedef void (*ssc_equation_ptr)(ssc_data_t data);

struct ssc_equation_entry{
    const char* name;
    ssc_equation_ptr func;
};

extern ssc_equation_ptr windpower_turbine_powercurve;

static ssc_equation_entry ssc_equation_table [] = {
        {"windpower_turbine_powercurve", windpower_turbine_powercurve},
        {nullptr, nullptr}
};

#endif //SYSTEM_ADVISOR_MODEL_SAM_EQNS_H
