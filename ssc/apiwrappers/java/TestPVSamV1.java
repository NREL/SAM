
import java.lang.reflect.Field;
import java.util.Arrays;

public class TestPVSamV1 {

/**
* Adds the specified path to the java library path
*
* @param pathToAdd the path to add
* @throws Exception
*/
    public static void addLibraryPath(String pathToAdd) throws Exception{
        final Field usrPathsField = ClassLoader.class.getDeclaredField("usr_paths");
        usrPathsField.setAccessible(true);

        //get array of paths
        final String[] paths = (String[])usrPathsField.get(null);

        //check if the path to add is already present
        for(String path : paths) {
            if(path.equals(pathToAdd)) {
                return;
            }
        }

        //add the new path
        final String[] newPaths = Arrays.copyOf(paths, paths.length + 1);
        newPaths[newPaths.length-1] = pathToAdd;
        usrPathsField.set(null, newPaths);
    }

    public static void PVSamV1()
    {
        System.out.println("\npvsamv1 example using PV Residential defaults from 2014.11.24 release");
        SSC.Module mod = new SSC.Module("pvsamv1");
		SSC.Data data = new SSC.Data();

		data.setNumber("system_capacity", 3.8745f );
		data.setString("solar_resource_file", "USA AZ Phoenix (TMY2).csv" );
		data.setNumber("use_wf_albedo", 0f );
		data.setArray("albedo", new float[]{ 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f, 0.2f } );
		data.setNumber("irrad_mode", 0f );
		data.setNumber("sky_model", 2f );
		data.setNumber("ac_loss", 1f );
		data.setNumber("modules_per_string", 9f );
		data.setNumber("strings_in_parallel", 2f );
		data.setNumber("inverter_count", 1f );
		data.setNumber("enable_mismatch_vmax_calc", 0f );
		data.setNumber("subarray1_tilt", 20f );
		data.setNumber("subarray1_tilt_eq_lat", 0f );
		data.setNumber("subarray1_azimuth", 180f );
		data.setNumber("subarray1_track_mode", 0f );
		data.setNumber("subarray1_rotlim", 45f );
		data.setNumber("subarray1_shade_mode", 1f );
		data.setNumber("subarray1_gcr", 0.3f );
		data.setArray("subarray1_soiling", new float[]{ 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f } );
		data.setNumber("subarray1_dcloss", 4.4402f );
		data.setNumber("subarray1_mismatch_loss", 2f );
		data.setNumber("subarray1_diodeconn_loss", 0.5f );
		data.setNumber("subarray1_dcwiring_loss", 2f );
		data.setNumber("subarray1_tracking_loss", 0f );
		data.setNumber("subarray1_nameplate_loss", 0f );
		data.setNumber("subarray2_mismatch_loss", 2f );
		data.setNumber("subarray2_diodeconn_loss", 0.5f );
		data.setNumber("subarray2_dcwiring_loss", 2f );
		data.setNumber("subarray2_tracking_loss", 0f );
		data.setNumber("subarray2_nameplate_loss", 0f );
		data.setNumber("subarray3_mismatch_loss", 2f );
		data.setNumber("subarray3_diodeconn_loss", 0.5f );
		data.setNumber("subarray3_dcwiring_loss", 2f );
		data.setNumber("subarray3_tracking_loss", 0f );
		data.setNumber("subarray3_nameplate_loss", 0f );
		data.setNumber("subarray4_mismatch_loss", 2f );
		data.setNumber("subarray4_diodeconn_loss", 0.5f );
		data.setNumber("subarray4_dcwiring_loss", 2f );
		data.setNumber("subarray4_tracking_loss", 0f );
		data.setNumber("subarray4_nameplate_loss", 0f );
		data.setNumber("acwiring_loss", 1f );
		data.setNumber("transformer_loss", 0f );
		data.setNumber("subarray1_mod_orient", 0f );
		data.setNumber("subarray1_nmodx", 9f );
		data.setNumber("subarray1_nmody", 2f );
		data.setNumber("subarray1_backtrack", 0f );
		data.setNumber("subarray2_enable", 0f );
		data.setNumber("subarray2_nstrings", 0f );
		data.setNumber("subarray2_tilt", 20f );
		data.setNumber("subarray2_tilt_eq_lat", 0f );
		data.setNumber("subarray2_azimuth", 180f );
		data.setNumber("subarray2_track_mode", 0f );
		data.setNumber("subarray2_rotlim", 45f );
		data.setNumber("subarray2_shade_mode", 1f );
		data.setNumber("subarray2_gcr", 0.3f );
		data.setArray("subarray2_soiling", new float[]{ 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f } );
		data.setNumber("subarray2_dcloss", 4.4402f );
		data.setNumber("subarray2_mod_orient", 0f );
		data.setNumber("subarray2_nmodx", 9f );
		data.setNumber("subarray2_nmody", 2f );
		data.setNumber("subarray2_backtrack", 0f );
		data.setNumber("subarray3_enable", 0f );
		data.setNumber("subarray3_nstrings", 0f );
		data.setNumber("subarray3_tilt", 20f );
		data.setNumber("subarray3_tilt_eq_lat", 0f );
		data.setNumber("subarray3_azimuth", 180f );
		data.setNumber("subarray3_track_mode", 0f );
		data.setNumber("subarray3_rotlim", 45f );
		data.setNumber("subarray3_shade_mode", 1f );
		data.setNumber("subarray3_gcr", 0.3f );
		data.setArray("subarray3_soiling", new float[]{ 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f } );
		data.setNumber("subarray3_dcloss", 4.4402f );
		data.setNumber("subarray3_mod_orient", 0f );
		data.setNumber("subarray3_nmodx", 9f );
		data.setNumber("subarray3_nmody", 2f );
		data.setNumber("subarray3_backtrack", 0f );
		data.setNumber("subarray4_enable", 0f );
		data.setNumber("subarray4_nstrings", 0f );
		data.setNumber("subarray4_tilt", 20f );
		data.setNumber("subarray4_tilt_eq_lat", 0f );
		data.setNumber("subarray4_azimuth", 180f );
		data.setNumber("subarray4_track_mode", 0f );
		data.setNumber("subarray4_rotlim", 45f );
		data.setNumber("subarray4_shade_mode", 1f );
		data.setNumber("subarray4_gcr", 0.3f );
		data.setArray("subarray4_soiling", new float[]{ 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f, 5f } );
		data.setNumber("subarray4_dcloss", 4.4402f );
		data.setNumber("subarray4_mod_orient", 0f );
		data.setNumber("subarray4_nmodx", 9f );
		data.setNumber("subarray4_nmody", 2f );
		data.setNumber("subarray4_backtrack", 0f );
		data.setNumber("module_model", 1f );
		data.setNumber("spe_area", 0.74074f );
		data.setNumber("spe_rad0", 200f );
		data.setNumber("spe_rad1", 400f );
		data.setNumber("spe_rad2", 600f );
		data.setNumber("spe_rad3", 800f );
		data.setNumber("spe_rad4", 1000f );
		data.setNumber("spe_eff0", 13.5f );
		data.setNumber("spe_eff1", 13.5f );
		data.setNumber("spe_eff2", 13.5f );
		data.setNumber("spe_eff3", 13.5f );
		data.setNumber("spe_eff4", 13.5f );
		data.setNumber("spe_reference", 4f );
		data.setNumber("spe_module_structure", 0f );
		data.setNumber("spe_a", -3.56f );
		data.setNumber("spe_b", -0.075f );
		data.setNumber("spe_dT", 3f );
		data.setNumber("spe_temp_coeff", -0.5f );
		data.setNumber("spe_fd", 1f );
		data.setNumber("cec_area", 1.244f );
		data.setNumber("cec_a_ref", 1.9816f );
		data.setNumber("cec_adjust", 20.8f );
		data.setNumber("cec_alpha_sc", 0.002651f );
		data.setNumber("cec_beta_oc", -0.14234f );
		data.setNumber("cec_gamma_r", -0.407f );
		data.setNumber("cec_i_l_ref", 5.754f );
		data.setNumber("cec_i_mp_ref", 5.25f );
		data.setNumber("cec_i_o_ref", 1.919e-010f );
		data.setNumber("cec_i_sc_ref", 5.75f );
		data.setNumber("cec_n_s", 72f );
		data.setNumber("cec_r_s", 0.105f );
		data.setNumber("cec_r_sh_ref", 160.48f );
		data.setNumber("cec_t_noct", 49.2f );
		data.setNumber("cec_v_mp_ref", 41f );
		data.setNumber("cec_v_oc_ref", 47.7f );
		data.setNumber("cec_temp_corr_mode", 0f );
		data.setNumber("cec_standoff", 6f );
		data.setNumber("cec_height", 0f );
		data.setNumber("cec_mounting_config", 0f );
		data.setNumber("cec_heat_transfer", 0f );
		data.setNumber("cec_mounting_orientation", 0f );
		data.setNumber("cec_gap_spacing", 0.05f );
		data.setNumber("cec_module_width", 1f );
		data.setNumber("cec_module_length", 1.244f );
		data.setNumber("cec_array_rows", 1f );
		data.setNumber("cec_array_cols", 10f );
		data.setNumber("cec_backside_temp", 20f );
		data.setNumber("6par_celltech", 1f );
		data.setNumber("6par_vmp", 30f );
		data.setNumber("6par_imp", 6f );
		data.setNumber("6par_voc", 37f );
		data.setNumber("6par_isc", 7f );
		data.setNumber("6par_bvoc", -0.11f );
		data.setNumber("6par_aisc", 0.004f );
		data.setNumber("6par_gpmp", -0.41f );
		data.setNumber("6par_nser", 60f );
		data.setNumber("6par_area", 1.3f );
		data.setNumber("6par_tnoct", 46f );
		data.setNumber("6par_standoff", 6f );
		data.setNumber("6par_mounting", 0f );
		data.setNumber("snl_module_structure", 0f );
		data.setNumber("snl_a", -3.62f );
		data.setNumber("snl_b", -0.075f );
		data.setNumber("snl_dtc", 3f );
		data.setNumber("snl_ref_a", -3.62f );
		data.setNumber("snl_ref_b", -0.075f );
		data.setNumber("snl_ref_dT", 3f );
		data.setNumber("snl_fd", 1f );
		data.setNumber("snl_a0", 0.94045f );
		data.setNumber("snl_a1", 0.052641f );
		data.setNumber("snl_a2", -0.0093897f );
		data.setNumber("snl_a3", 0.00072623f );
		data.setNumber("snl_a4", -1.9938e-005f );
		data.setNumber("snl_aimp", -0.00038f );
		data.setNumber("snl_aisc", 0.00061f );
		data.setNumber("snl_area", 1.244f );
		data.setNumber("snl_b0", 1f );
		data.setNumber("snl_b1", -0.002438f );
		data.setNumber("snl_b2", 0.0003103f );
		data.setNumber("snl_b3", -1.246e-005f );
		data.setNumber("snl_b4", 2.11e-007f );
		data.setNumber("snl_b5", -1.36e-009f );
		data.setNumber("snl_bvmpo", -0.139f );
		data.setNumber("snl_bvoco", -0.136f );
		data.setNumber("snl_c0", 1.0039f );
		data.setNumber("snl_c1", -0.0039f );
		data.setNumber("snl_c2", 0.291066f );
		data.setNumber("snl_c3", -4.73546f );
		data.setNumber("snl_c4", 0.9942f );
		data.setNumber("snl_c5", 0.0058f );
		data.setNumber("snl_c6", 1.0723f );
		data.setNumber("snl_c7", -0.0723f );
		data.setNumber("snl_impo", 5.25f );
		data.setNumber("snl_isco", 5.75f );
		data.setNumber("snl_ixo", 5.65f );
		data.setNumber("snl_ixxo", 3.85f );
		data.setNumber("snl_mbvmp", 0f );
		data.setNumber("snl_mbvoc", 0f );
		data.setNumber("snl_n", 1.221f );
		data.setNumber("snl_series_cells", 72f );
		data.setNumber("snl_vmpo", 40f );
		data.setNumber("snl_voco", 47.7f );
		data.setNumber("inverter_model", 0f );
		data.setNumber("inv_snl_c0", -6.57929e-006f );
		data.setNumber("inv_snl_c1", 4.72925e-005f );
		data.setNumber("inv_snl_c2", 0.00202195f );
		data.setNumber("inv_snl_c3", 0.000285321f );
		data.setNumber("inv_snl_paco", 4000f );
		data.setNumber("inv_snl_pdco", 4186f );
		data.setNumber("inv_snl_pnt", 0.17f );
		data.setNumber("inv_snl_pso", 19.7391f );
		data.setNumber("inv_snl_vdco", 310.67f );
		data.setNumber("inv_snl_vdcmax", 600f );
		data.setNumber("inv_ds_paco", 4000f );
		data.setNumber("inv_ds_eff", 96f );
		data.setNumber("inv_ds_pnt", 1f );
		data.setNumber("inv_ds_pso", 0f );
		data.setNumber("inv_ds_vdco", 310f );
		data.setNumber("inv_ds_vdcmax", 600f );
		data.setNumber("inv_pd_paco", 4000f );
		data.setNumber("inv_pd_pdco", 4210.53f );
		data.setArray("inv_pd_partload", new float[]{ 0f, 0.404f, 0.808f, 1.212f, 1.616f, 2.02f, 2.424f, 2.828f, 3.232f, 3.636f, 4.04f, 4.444f, 4.848f, 5.252f, 5.656f, 6.06f, 6.464f, 6.868f, 7.272f, 7.676f, 8.08f, 8.484f, 8.888f, 9.292f, 9.696f, 10.1f, 10.504f, 10.908f, 11.312f, 11.716f, 12.12f, 12.524f, 12.928f, 13.332f, 13.736f, 14.14f, 14.544f, 14.948f, 15.352f, 15.756f, 16.16f, 16.564f, 16.968f, 17.372f, 17.776f, 18.18f, 18.584f, 18.988f, 19.392f, 19.796f, 20.2f, 20.604f, 21.008f, 21.412f, 21.816f, 22.22f, 22.624f, 23.028f, 23.432f, 23.836f, 24.24f, 24.644f, 25.048f, 25.452f, 25.856f, 26.26f, 26.664f, 27.068f, 27.472f, 27.876f, 28.28f, 28.684f, 29.088f, 29.492f, 29.896f, 30.3f, 30.704f, 31.108f, 31.512f, 31.916f, 32.32f, 32.724f, 33.128f, 33.532f, 33.936f, 34.34f, 34.744f, 35.148f, 35.552f, 35.956f, 36.36f, 36.764f, 37.168f, 37.572f, 37.976f, 38.38f, 38.784f, 39.188f, 39.592f, 39.996f, 40.4f, 40.804f, 41.208f, 41.612f, 42.016f, 42.42f, 42.824f, 43.228f, 43.632f, 44.036f, 44.44f, 44.844f, 45.248f, 45.652f, 46.056f, 46.46f, 46.864f, 47.268f, 47.672f, 48.076f, 48.48f, 48.884f, 49.288f, 49.692f, 50.096f, 50.5f, 50.904f, 51.308f, 51.712f, 52.116f, 52.52f, 52.924f, 53.328f, 53.732f, 54.136f, 54.54f, 54.944f, 55.348f, 55.752f, 56.156f, 56.56f, 56.964f, 57.368f, 57.772f, 58.176f, 58.58f, 58.984f, 59.388f, 59.792f, 60.196f, 60.6f, 61.004f, 61.408f, 61.812f, 62.216f, 62.62f, 63.024f, 63.428f, 63.832f, 64.236f, 64.64f, 65.044f, 65.448f, 65.852f, 66.256f, 66.66f, 67.064f, 67.468f, 67.872f, 68.276f, 68.68f, 69.084f, 69.488f, 69.892f, 70.296f, 70.7f, 71.104f, 71.508f, 71.912f, 72.316f, 72.72f, 73.124f, 73.528f, 73.932f, 74.336f, 74.74f, 75.144f, 75.548f, 75.952f, 76.356f, 76.76f, 77.164f, 77.568f, 77.972f, 78.376f, 78.78f, 79.184f, 79.588f, 79.992f, 80.396f, 80.8f, 81.204f, 81.608f, 82.012f, 82.416f, 82.82f, 83.224f, 83.628f, 84.032f, 84.436f, 84.84f, 85.244f, 85.648f, 86.052f, 86.456f, 86.86f, 87.264f, 87.668f, 88.072f, 88.476f, 88.88f, 89.284f, 89.688f, 90.092f, 90.496f, 90.9f, 91.304f, 91.708f, 92.112f, 92.516f, 92.92f, 93.324f, 93.728f, 94.132f, 94.536f, 94.94f, 95.344f, 95.748f, 96.152f, 96.556f, 96.96f, 97.364f, 97.768f, 98.172f, 98.576f, 98.98f, 99.384f, 99.788f, 100.192f, 100.596f, 101f } );
		data.setArray("inv_pd_efficiency", new float[]{ 0f, 0f, 34.42f, 55.2f, 65.59f, 71.82f, 75.97f, 78.94f, 81.17f, 82.9f, 84.28f, 85.42f, 86.36f, 87.16f, 87.84f, 88.44f, 88.95f, 89.41f, 89.82f, 90.18f, 90.51f, 90.81f, 91.08f, 91.32f, 91.55f, 91.75f, 91.95f, 92.12f, 92.29f, 92.44f, 92.58f, 92.72f, 92.84f, 92.96f, 93.07f, 93.17f, 93.27f, 93.37f, 93.45f, 93.54f, 93.62f, 93.69f, 93.76f, 93.83f, 93.9f, 93.96f, 94.02f, 94.08f, 94.13f, 94.18f, 94.23f, 94.28f, 94.33f, 94.37f, 94.42f, 94.46f, 94.5f, 94.54f, 94.57f, 94.61f, 94.64f, 94.68f, 94.71f, 94.74f, 94.77f, 94.8f, 94.83f, 94.86f, 94.89f, 94.91f, 94.94f, 94.96f, 94.98f, 95.01f, 95.03f, 95.05f, 95.07f, 95.09f, 95.11f, 95.13f, 95.15f, 95.17f, 95.19f, 95.21f, 95.23f, 95.24f, 95.26f, 95.28f, 95.29f, 95.31f, 95.32f, 95.34f, 95.35f, 95.36f, 95.38f, 95.39f, 95.4f, 95.42f, 95.43f, 95.44f, 95.45f, 95.47f, 95.48f, 95.49f, 95.5f, 95.51f, 95.52f, 95.53f, 95.54f, 95.55f, 95.56f, 95.57f, 95.58f, 95.59f, 95.6f, 95.61f, 95.62f, 95.63f, 95.64f, 95.64f, 95.65f, 95.66f, 95.67f, 95.68f, 95.68f, 95.69f, 95.7f, 95.71f, 95.71f, 95.72f, 95.73f, 95.73f, 95.74f, 95.75f, 95.75f, 95.76f, 95.77f, 95.77f, 95.78f, 95.78f, 95.79f, 95.8f, 95.8f, 95.81f, 95.81f, 95.82f, 95.82f, 95.83f, 95.83f, 95.84f, 95.84f, 95.85f, 95.85f, 95.86f, 95.86f, 95.87f, 95.87f, 95.88f, 95.88f, 95.89f, 95.89f, 95.89f, 95.9f, 95.9f, 95.91f, 95.91f, 95.91f, 95.92f, 95.92f, 95.93f, 95.93f, 95.93f, 95.94f, 95.94f, 95.94f, 95.95f, 95.95f, 95.96f, 95.96f, 95.96f, 95.97f, 95.97f, 95.97f, 95.98f, 95.98f, 95.98f, 95.98f, 95.99f, 95.99f, 95.99f, 96f, 96f, 96f, 96.01f, 96.01f, 96.01f, 96.01f, 96.02f, 96.02f, 96.02f, 96.02f, 96.03f, 96.03f, 96.03f, 96.03f, 96.04f, 96.04f, 96.04f, 96.04f, 96.05f, 96.05f, 96.05f, 96.05f, 96.06f, 96.06f, 96.06f, 96.06f, 96.06f, 96.07f, 96.07f, 96.07f, 96.07f, 96.07f, 96.08f, 96.08f, 96.08f, 96.08f, 96.08f, 96.09f, 96.09f, 96.09f, 96.09f, 96.09f, 96.09f, 96.1f, 96.1f, 96.1f, 96.1f, 96.1f, 96.1f, 96.11f, 96.11f, 96.11f, 96.11f, 96.11f, 96.11f, 96.12f, 96.12f, 96.12f, 96.12f, 96.12f } );
		data.setNumber("inv_pd_pnt", 0f );
		data.setNumber("inv_pd_vdco", 310f );
		data.setNumber("inv_pd_vdcmax", 600f );
		data.setNumber("adjust:factor", 1f );


        if (mod.exec(data))
        {
	     float enet = data.getNumber("annual_energy");
	     float cf = data.getNumber("capacity_factor");
	     float kWhperkW = data.getNumber("kwh_per_kw");
	     System.out.println("Annual energy : " + enet + " kWh" );
	     System.out.println("Capacity factor : " + cf + "%");
	     System.out.println("First year kWhAC/kWDC : " + kWhperkW);
	     System.out.println("pvsamv1 example OK");
        }
        else
        {
            int idx = 0;
            String msg="";
            int[] type={0};
            float[] time={0};
            while (mod.log(idx, msg, type, time))
            {
                String stype = "NOTICE";
                if (type[0] == SSC.API.WARNING) stype = "WARNING";
                else if (type[0] == SSC.API.ERROR) stype = "ERROR";
                System.out.println("[ " + stype + " at time:" + time[0] + " ]: " + msg);
                idx++;
            }
            System.out.println("pvsamv1 example failed");
        }
    }


    public static void main(String[] args) throws Exception
    {
        // address dll path issues (if necessary)
        //e.g. addLibraryPath( "C:\\Projects\\SAM\\VS2012\\ssc\\java");
        System.loadLibrary("SSCAPIJNI");
 		PVSamV1();
    }
}
