#ifndef SAM_GENERICSYSTEM_FUNCTIONS_H_
#define SAM_GENERICSYSTEM_FUNCTIONS_H_

#include "visibility.h"
#include "SAM_api.h"
//#include "ErrorHandler.h"
//#include <ssc/sscapi.h>


//#include <ssc/cmod_generic_system-builder.h>
//#include "cmod_generic_system-builder.h"

#include <stdint.h>
#ifdef __cplusplus
extern "C"
{
#endif

    //
    // Generic System Technology Model
    //
    
	/**
	 * Create a PowerPlant variable table for a GenericSystemNone system
	 * @param def_ the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err_ a pointer to an error object
	 */
	SAM_EXPORT typedef void * SAM_GenericSystem;

	SAM_EXPORT SAM_GenericSystem SAM_GenericSystem_construct(const char* def, SAM_error* err);

    SAM_EXPORT void SAM_GenericSystem_destruct(SAM_GenericSystem gs_system);

    //
    // Modules and associated parameters
    //

    /** Power Plant */
    
    /**
     * Set derate_ Derate
     * type_ numeric
     * units_ %
     * options_ None
     * constraints_ None
     * required if_ None
     */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_derate_fset(SAM_GenericSystem ptr, float number, SAM_error *err);

	/**
	 * Set energy_output_array_ Array of Energy Output Profile
	 * type_ array
	 * units_ kW
	 * options_ None
	 * constraints_ None
	 * required if_ None
	 */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_energy_output_array_aset(SAM_GenericSystem ptr, float *array,
                                                                          int length, SAM_error *err);

	/**
	 * Set heat_rate_ Heat Rate
	 * type_ numeric
	 * units_ MMBTUs/MWhe
	 * options_ None
	 * constraints_ None
	 * required if_ None
	 */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_heat_rate_set(SAM_GenericSystem ptr, float number, SAM_error* err);

	/**
	 * Set spec_mode_ Spec mode_ 0=constant CF,1=profile
	 * type_ numeric
	 * units_ None
	 * options_ None
	 * constraints_ None
	 * required if_ None
	 */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_spec_mode_set(SAM_GenericSystem ptr, float number, SAM_error* err);

	/**
	 * Set system_capacity_ Nameplace Capcity
	 * type_ numeric
	 * units_ kW
	 * options_ None
	 * constraints_ None
	 * required if_ None
	 */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_system_capacity_set(SAM_GenericSystem ptr, float number, SAM_error* err);

	/**
	 * Set user_capacity_factor_ Capacity Factor
	 * type_ numeric
	 * units_ %
	 * options_ None
	 * constraints_ None
	 * required if_ None
	 */
	SAM_EXPORT void SAM_GenericSystem_PowerPlant_user_capacity_factor_set(SAM_GenericSystem ptr, float number, SAM_error* err);


	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_GenericSystem_PowerPlant_derate_fget(SAM_GenericSystem ptr, SAM_error *err);

	SAM_EXPORT float* SAM_GenericSystem_PowerPlant_energy_output_array_aget(SAM_GenericSystem ptr, int *length,
                                                                            SAM_error *err);

	SAM_EXPORT float SAM_GenericSystem_PowerPlant_heat_rate_get(SAM_GenericSystem ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_PowerPlant_spec_mode_get(SAM_GenericSystem ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_PowerPlant_system_capacity_get(SAM_GenericSystem ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_PowerPlant_user_capacity_factor_get(SAM_GenericSystem ptr, SAM_error* err);



	 /** Common */
	typedef void * SAM_GenericSystem_Common;

	/** 
	 * Create a Common variable table for a GenericSystemNone system
	 * @param def_ the set of financial model-dependent defaults to use (None, Residential, ...)
	 * @param[in,out] err_ a pointer to an error object
	 */
	SAM_EXPORT SAM_GenericSystem SAM_GenericSystem_Common_create(const char* def, SAM_error* err);


	/**
	 * Set adjust_constant_ Constant loss adjustment
	 * type_ numeric
	 * units_ %
	 * options_ None
	 * constraints_ MAX=100
	 * required if_ None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_adjust_constant_set(SAM_GenericSystem ptr, float number, SAM_error* err);

	/**
	 * Set adjust_hourly_ Hourly loss adjustments
	 * type_ array
	 * units_ %
	 * options_ None
	 * constraints_ LENGTH=8760
	 * required if_ None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_adjust_hourly_set(SAM_GenericSystem ptr, float* array, int length, SAM_error* err);

	/**
	 * Set adjust_periods_ Period-based loss adjustments
	 * type_ matrix
	 * units_ %
	 * options_ n x 3 matrix [ start, end, loss ]
	 * constraints_ COLS=3
	 * required if_ None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_adjust_periods_set(SAM_GenericSystem ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set analysis_period_ Lifetime analysis period
	 * type_ numeric
	 * units_ years
	 * options_ None
	 * constraints_ None
	 * required if_ system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_analysis_period_set(SAM_GenericSystem ptr, float number, SAM_error* err);

	/**
	 * Set dc_adjust_constant_ DC Constant loss adjustment
	 * type_ numeric
	 * units_ %
	 * options_ None
	 * constraints_ MAX=100
	 * required if_ None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_dc_adjust_constant_set(SAM_GenericSystem ptr, float number, SAM_error* err);

	/**
	 * Set dc_adjust_hourly_ DC Hourly loss adjustments
	 * type_ array
	 * units_ %
	 * options_ None
	 * constraints_ LENGTH=8760
	 * required if_ None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_dc_adjust_hourly_set(SAM_GenericSystem ptr, float* array, int length, SAM_error* err);

	/**
	 * Set dc_adjust_periods_ DC Period-based loss adjustments
	 * type_ matrix
	 * units_ %
	 * options_ n x 3 matrix [ start, end, loss ]
	 * constraints_ COLS=3
	 * required if_ None
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_dc_adjust_periods_set(SAM_GenericSystem ptr, float* matrix, int nr, int nc, SAM_error* err);

	/**
	 * Set generic_degradation_ Annual module degradation
	 * type_ array
	 * units_ %/year
	 * options_ None
	 * constraints_ None
	 * required if_ system_use_lifetime_output=1
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_generic_degradation_set(SAM_GenericSystem ptr, float* array, int length, SAM_error* err);

	/**
	 * Set system_use_lifetime_output_ Generic lifetime simulation
	 * type_ numeric
	 * units_ 0/1
	 * options_ None
	 * constraints_ INTEGER,MIN=0,MAX=1
	 * required if_ ?=0
	 */
	SAM_EXPORT void SAM_GenericSystem_Common_system_use_lifetime_output_set(SAM_GenericSystem ptr, float number, SAM_error* err);


	/**
	 * Evaluators
	 */

	SAM_EXPORT float SAM_GenericSystem_PowerPlant_conv_eff_eval(SAM_GenericSystem ptr, SAM_error* err);

	/**
	 * Getters
	 */

	SAM_EXPORT float SAM_GenericSystem_Common_adjust_constant_get(SAM_GenericSystem ptr, SAM_error* err);

	SAM_EXPORT float* SAM_GenericSystem_Common_adjust_hourly_get(SAM_GenericSystem ptr, SAM_error* err);

	SAM_EXPORT float* SAM_GenericSystem_Common_adjust_periods_get(SAM_GenericSystem ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_Common_analysis_period_get(SAM_GenericSystem ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_Common_dc_adjust_constant_get(SAM_GenericSystem ptr, SAM_error* err);

	SAM_EXPORT float* SAM_GenericSystem_Common_dc_adjust_hourly_get(SAM_GenericSystem ptr, SAM_error* err);

	SAM_EXPORT float* SAM_GenericSystem_Common_dc_adjust_periods_get(SAM_GenericSystem ptr, SAM_error* err);

	SAM_EXPORT float* SAM_GenericSystem_Common_generic_degradation_get(SAM_GenericSystem ptr, SAM_error* err);

	SAM_EXPORT float SAM_GenericSystem_Common_system_use_lifetime_output_get(SAM_GenericSystem ptr, SAM_error* err);



#ifdef __cplusplus
} /* end of extern "C" { */
#endif

#endif