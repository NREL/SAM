#include <string>
#include <vector>

#include "vartab.h"

#include "cmod_trough_physical_process_heat-builder.h"

var_table TroughPhysicalProcessHeat_W_aperture_MIMO_eval(var_table* vt)
{
	// inputs
	float csp_dtr_sca_w_profile_1 = vt->lookup("csp_dtr_sca_w_profile_1")->num;
	float csp_dtr_sca_w_profile_2 = vt->lookup("csp_dtr_sca_w_profile_2")->num;
	float csp_dtr_sca_w_profile_3 = vt->lookup("csp_dtr_sca_w_profile_3")->num;
	float csp_dtr_sca_w_profile_4 = vt->lookup("csp_dtr_sca_w_profile_4")->num;
	util::matrix_t<ssc_number_t> arr_collectors_in_loop = vt->lookup("arr_collectors_in_loop")->num;
	float csp_dtr_sca_aperture_1 = vt->lookup("csp_dtr_sca_aperture_1")->num;
	float csp_dtr_sca_aperture_2 = vt->lookup("csp_dtr_sca_aperture_2")->num;
	float csp_dtr_sca_aperture_3 = vt->lookup("csp_dtr_sca_aperture_3")->num;
	float csp_dtr_sca_aperture_4 = vt->lookup("csp_dtr_sca_aperture_4")->num;
	float csp_dtr_sca_tracking_error_1 = vt->lookup("csp_dtr_sca_tracking_error_1")->num;
	float csp_dtr_sca_tracking_error_2 = vt->lookup("csp_dtr_sca_tracking_error_2")->num;
	float csp_dtr_sca_tracking_error_3 = vt->lookup("csp_dtr_sca_tracking_error_3")->num;
	float csp_dtr_sca_tracking_error_4 = vt->lookup("csp_dtr_sca_tracking_error_4")->num;
	float csp_dtr_sca_geometry_effects_1 = vt->lookup("csp_dtr_sca_geometry_effects_1")->num;
	float csp_dtr_sca_geometry_effects_2 = vt->lookup("csp_dtr_sca_geometry_effects_2")->num;
	float csp_dtr_sca_geometry_effects_3 = vt->lookup("csp_dtr_sca_geometry_effects_3")->num;
	float csp_dtr_sca_geometry_effects_4 = vt->lookup("csp_dtr_sca_geometry_effects_4")->num;
	float csp_dtr_sca_clean_reflectivity_1 = vt->lookup("csp_dtr_sca_clean_reflectivity_1")->num;
	float csp_dtr_sca_clean_reflectivity_2 = vt->lookup("csp_dtr_sca_clean_reflectivity_2")->num;
	float csp_dtr_sca_clean_reflectivity_3 = vt->lookup("csp_dtr_sca_clean_reflectivity_3")->num;
	float csp_dtr_sca_clean_reflectivity_4 = vt->lookup("csp_dtr_sca_clean_reflectivity_4")->num;
	float csp_dtr_sca_mirror_dirt_1 = vt->lookup("csp_dtr_sca_mirror_dirt_1")->num;
	float csp_dtr_sca_mirror_dirt_2 = vt->lookup("csp_dtr_sca_mirror_dirt_2")->num;
	float csp_dtr_sca_mirror_dirt_3 = vt->lookup("csp_dtr_sca_mirror_dirt_3")->num;
	float csp_dtr_sca_mirror_dirt_4 = vt->lookup("csp_dtr_sca_mirror_dirt_4")->num;
	float csp_dtr_sca_general_error_1 = vt->lookup("csp_dtr_sca_general_error_1")->num;
	float csp_dtr_sca_general_error_2 = vt->lookup("csp_dtr_sca_general_error_2")->num;
	float csp_dtr_sca_general_error_3 = vt->lookup("csp_dtr_sca_general_error_3")->num;
	float csp_dtr_sca_general_error_4 = vt->lookup("csp_dtr_sca_general_error_4")->num;
	float csp_dtr_sca_ave_focal_len_1 = vt->lookup("csp_dtr_sca_ave_focal_len_1")->num;
	float csp_dtr_sca_ave_focal_len_2 = vt->lookup("csp_dtr_sca_ave_focal_len_2")->num;
	float csp_dtr_sca_ave_focal_len_3 = vt->lookup("csp_dtr_sca_ave_focal_len_3")->num;
	float csp_dtr_sca_ave_focal_len_4 = vt->lookup("csp_dtr_sca_ave_focal_len_4")->num;
	float csp_dtr_sca_length_1 = vt->lookup("csp_dtr_sca_length_1")->num;
	float csp_dtr_sca_length_2 = vt->lookup("csp_dtr_sca_length_2")->num;
	float csp_dtr_sca_length_3 = vt->lookup("csp_dtr_sca_length_3")->num;
	float csp_dtr_sca_length_4 = vt->lookup("csp_dtr_sca_length_4")->num;
	float csp_dtr_sca_ap_length_1 = vt->lookup("csp_dtr_sca_ap_length_1")->num;
	float csp_dtr_sca_ap_length_2 = vt->lookup("csp_dtr_sca_ap_length_2")->num;
	float csp_dtr_sca_ap_length_3 = vt->lookup("csp_dtr_sca_ap_length_3")->num;
	float csp_dtr_sca_ap_length_4 = vt->lookup("csp_dtr_sca_ap_length_4")->num;
	float csp_dtr_sca_ncol_per_sca_1 = vt->lookup("csp_dtr_sca_ncol_per_sca_1")->num;
	float csp_dtr_sca_ncol_per_sca_2 = vt->lookup("csp_dtr_sca_ncol_per_sca_2")->num;
	float csp_dtr_sca_ncol_per_sca_3 = vt->lookup("csp_dtr_sca_ncol_per_sca_3")->num;
	float csp_dtr_sca_ncol_per_sca_4 = vt->lookup("csp_dtr_sca_ncol_per_sca_4")->num;
	float csp_dtr_sca_piping_dist_1 = vt->lookup("csp_dtr_sca_piping_dist_1")->num;
	float csp_dtr_sca_piping_dist_2 = vt->lookup("csp_dtr_sca_piping_dist_2")->num;
	float csp_dtr_sca_piping_dist_3 = vt->lookup("csp_dtr_sca_piping_dist_3")->num;
	float csp_dtr_sca_piping_dist_4 = vt->lookup("csp_dtr_sca_piping_dist_4")->num;

	// outputs
	util::matrix_t<ssc_number_t> W_aperture;
	float max_collector_width;
	util::matrix_t<ssc_number_t> A_aperture;
	util::matrix_t<ssc_number_t> TrackingError;
	util::matrix_t<ssc_number_t> GeomEffects;
	util::matrix_t<ssc_number_t> Rho_mirror_clean;
	util::matrix_t<ssc_number_t> Dirt_mirror;
	util::matrix_t<ssc_number_t> Error;
	util::matrix_t<ssc_number_t> Ave_Focal_Length;
	util::matrix_t<ssc_number_t> L_SCA;
	util::matrix_t<ssc_number_t> L_aperture;
	util::matrix_t<ssc_number_t> ColperSCA;
	util::matrix_t<ssc_number_t> Distance_SCA;

	std::vector<float> acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_w_profile_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_w_profile_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_w_profile_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_w_profile_4);
	W_aperture = acollector;
	util::matrix_t<float> arr = arr_collectors_in_loop;
	util::matrix_t<float> n = arr.size();
	for ( float i = 0.000000; i < n; i += 1 ){
		std::vector<util::matrix_t<float>> widths;
		widths.insert(widths.begin()+i, acollector[arr[i]]);
	}
	max_collector_width = max( widths );
	acollector.insert(acollector.begin()+0, csp_dtr_sca_aperture_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_aperture_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_aperture_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_aperture_4);
	A_aperture = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_tracking_error_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_tracking_error_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_tracking_error_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_tracking_error_4);
	TrackingError = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_geometry_effects_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_geometry_effects_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_geometry_effects_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_geometry_effects_4);
	GeomEffects = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_clean_reflectivity_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_clean_reflectivity_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_clean_reflectivity_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_clean_reflectivity_4);
	Rho_mirror_clean = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_mirror_dirt_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_mirror_dirt_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_mirror_dirt_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_mirror_dirt_4);
	Dirt_mirror = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_general_error_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_general_error_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_general_error_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_general_error_4);
	Error = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_ave_focal_len_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_ave_focal_len_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_ave_focal_len_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_ave_focal_len_4);
	Ave_Focal_Length = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_length_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_length_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_length_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_length_4);
	L_SCA = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_ap_length_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_ap_length_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_ap_length_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_ap_length_4);
	L_aperture = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_ncol_per_sca_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_ncol_per_sca_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_ncol_per_sca_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_ncol_per_sca_4);
	ColperSCA = acollector;
	acollector.insert(acollector.begin()+0, csp_dtr_sca_piping_dist_1);
	acollector.insert(acollector.begin()+1, csp_dtr_sca_piping_dist_2);
	acollector.insert(acollector.begin()+2, csp_dtr_sca_piping_dist_3);
	acollector.insert(acollector.begin()+3, csp_dtr_sca_piping_dist_4);
	Distance_SCA = acollector;


	var_table vt;
	vt.assign( "W_aperture", W_aperture );
	vt.assign( "max_collector_width", max_collector_width );
	vt.assign( "A_aperture", A_aperture );
	vt.assign( "TrackingError", TrackingError );
	vt.assign( "GeomEffects", GeomEffects );
	vt.assign( "Rho_mirror_clean", Rho_mirror_clean );
	vt.assign( "Dirt_mirror", Dirt_mirror );
	vt.assign( "Error", Error );
	vt.assign( "Ave_Focal_Length", Ave_Focal_Length );
	vt.assign( "L_SCA", L_SCA );
	vt.assign( "L_aperture", L_aperture );
	vt.assign( "ColperSCA", ColperSCA );
	vt.assign( "Distance_SCA", Distance_SCA );

}



var_table TroughPhysicalProcessHeat_collectors_in_field_MIMO_eval(var_table* vt)
{
	// inputs
	util::matrix_t<ssc_number_t> SCAInfoArray = vt->lookup("SCAInfoArray")->num;
	float nColt = vt->lookup("nColt")->num;

	// outputs
	const char* collectors_in_field;
	util::matrix_t<ssc_number_t> arr_collectors_in_loop;

	util::matrix_t<float> arr = SCAInfoArray;
	float n = nColt;
	std::string str = Cold - ;
	for ( float i = 0.000000; i < arr.size(); i += 1 ){
		for ( float j = 0.000000; j < n; j += 1 ){
			if ( arr[i][0.000000] == j + 1.000000 ) {
				std::vector<float> c;
				c.insert(c.begin()+i, j);
				std::vector<float> s;
				s.insert(s.begin()+j, to_string( j + 1.000000 ));
				std::string str = str + s[j] +  - ;
			
			}
		}
	}
	collectors_in_field = str + Hot;
	arr_collectors_in_loop = c;


	var_table vt;
	vt.assign( "collectors_in_field", collectors_in_field );
	vt.assign( "arr_collectors_in_loop", arr_collectors_in_loop );

}



util::matrix_t<ssc_number_t> TroughPhysicalProcessHeat_D_cpnt_eval(var_table* vt)
{
	// inputs
	float nSCA = vt->lookup("nSCA")->num;

	// outputs
	util::matrix_t<ssc_number_t> D_cpnt;

	float D_cpnt_0 = { , , , , , , , , , ,  }
	float D_cpnt_1 = { , , , , , , , , , ,  }
	float D_cpnt_i = { , , , , , , , , , ,  }
	float D_cpnt_x_2 = { , , , , , , , , , ,  }
	float D_cpnt_x_1 = { , , , , , , , , , ,  }
	D = {  }
	std::vector<float> D;
	D.insert(D.begin()+0, D_cpnt_0);
	D.insert(D.begin()+1, D_cpnt_1);
	for ( float i = 0.000000; i < nSCA - 1.000000; i += 1 ){
		D.insert(D.begin()+[i + 2.000000, D_cpnt_i);
	}
	D.insert(D.begin()+[nSCA + 1.000000, D_cpnt_x_2);
	D.insert(D.begin()+[nSCA + 2.000000, D_cpnt_x_1);
	float = D;


	return float;

}



