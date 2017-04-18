#include "csp_solver_pc_Rankine_indirect_224.h"
#include "csp_solver_util.h"

#include "lib_physics.h"
#include "water_properties.h"
#include "lib_util.h"
#include "sam_csp_util.h"

static C_csp_reported_outputs::S_output_info S_output_info[] =
{
	{C_pc_Rankine_indirect_224::E_ETA_THERMAL, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_Rankine_indirect_224::E_Q_DOT_HTF, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_Rankine_indirect_224::E_M_DOT_HTF, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_Rankine_indirect_224::E_Q_DOT_STARTUP, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_Rankine_indirect_224::E_W_DOT, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_Rankine_indirect_224::E_T_HTF_IN, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_Rankine_indirect_224::E_T_HTF_OUT, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_Rankine_indirect_224::E_M_DOT_WATER, C_csp_reported_outputs::TS_WEIGHTED_AVE},
	{C_pc_Rankine_indirect_224::E_M_DOT_HTF_REF, C_csp_reported_outputs::TS_WEIGHTED_AVE},

	csp_info_invalid
};

C_pc_Rankine_indirect_224::C_pc_Rankine_indirect_224()
{
	m_is_initialized = false;

	m_standby_control_prev = m_standby_control_calc = -1;	

	m_F_wcMax = m_F_wcMin = m_delta_h_steam = m_startup_energy_required = m_eta_adj =
		m_m_dot_design = m_q_dot_design = m_cp_htf_design =
		m_startup_time_remain_prev = m_startup_time_remain_calc =
		m_startup_energy_remain_prev = m_startup_energy_remain_calc = std::numeric_limits<double>::quiet_NaN();

	m_ncall = -1;

	mc_reported_outputs.construct(S_output_info);
}

void C_pc_Rankine_indirect_224::init(C_csp_power_cycle::S_solved_params &solved_params)
{
	// Declare instance of fluid class for FIELD fluid
	if( ms_params.m_pc_fl != HTFProperties::User_defined && ms_params.m_pc_fl < HTFProperties::End_Library_Fluids )
	{
		if( !mc_pc_htfProps.SetFluid(ms_params.m_pc_fl) )
		{
			throw(C_csp_exception("Power cycle HTF code is not recognized", "Rankine Indirect Power Cycle Initialization"));
		}
	}
	else if( ms_params.m_pc_fl == HTFProperties::User_defined )
	{
		// Check that 'm_field_fl_props' is allocated and correct dimensions
		int n_rows = ms_params.m_pc_fl_props.nrows();
		int n_cols = ms_params.m_pc_fl_props.ncols();
		if( n_rows > 2 && n_cols == 7 )
		{
			if( !mc_pc_htfProps.SetUserDefinedFluid(ms_params.m_pc_fl_props) )
			{
				m_error_msg = util::format(mc_pc_htfProps.UserFluidErrMessage(), n_rows, n_cols);
				throw(C_csp_exception(m_error_msg, "Rankine Indirect Power Cycle Initialization"));
			}
		}
		else
		{
			m_error_msg = util::format("The user defined field HTF table must contain at least 3 rows and exactly 7 columns. The current table contains %d row(s) and %d column(s)", n_rows, n_cols);
			throw(C_csp_exception(m_error_msg, "Rankine Indirect Power Cycle Initialization"));
		}
	}
	else
	{
		throw(C_csp_exception("Power cycle HTF code is not recognized", "Rankine Indirect Power Cycle Initialization"));
	}


	// Calculations for hardcoded Rankine power cycle model
	if( !ms_params.m_is_user_defined_pc )
	{
		if(ms_params.m_tech_type == 1)
		{	//	Power tower applications
			double dTemp[18][20] =
			{
				{ 0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316, 0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895, 0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474, 1.14737, 1.20000 },
				{ 0.16759, 0.21750, 0.26932, 0.32275, 0.37743, 0.43300, 0.48910, 0.54545, 0.60181, 0.65815, 0.71431, 0.77018, 0.82541, 0.88019, 0.93444, 0.98886, 1.04378, 1.09890, 1.15425, 1.20982 },
				{ 0.19656, 0.24969, 0.30325, 0.35710, 0.41106, 0.46497, 0.51869, 0.57215, 0.62529, 0.67822, 0.73091, 0.78333, 0.83526, 0.88694, 0.93838, 0.98960, 1.04065, 1.09154, 1.14230, 1.19294 },
				{ 3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00 },
				{ 1.07401, 1.04917, 1.03025, 1.01488, 1.00201, 0.99072, 0.98072, 0.97174, 0.96357, 0.95607, 0.94914, 0.94269, 0.93666, 0.93098, 0.92563, 0.92056, 0.91573, 0.91114, 0.90675, 0.90255 },
				{ 1.00880, 1.00583, 1.00355, 1.00168, 1.00010, 0.99870, 0.99746, 0.99635, 0.99532, 0.99438, 0.99351, 0.99269, 0.99193, 0.99121, 0.99052, 0.98988, 0.98926, 0.98867, 0.98810, 0.98756 },
				{ 0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000 },
				{ 0.09403, 0.16542, 0.23861, 0.31328, 0.38901, 0.46540, 0.54203, 0.61849, 0.69437, 0.76928, 0.84282, 0.91458, 0.98470, 1.05517, 1.12536, 1.19531, 1.26502, 1.33450, 1.40376, 1.47282 },
				{ 0.10659, 0.18303, 0.25848, 0.33316, 0.40722, 0.48075, 0.55381, 0.62646, 0.69873, 0.77066, 0.84228, 0.91360, 0.98464, 1.05542, 1.12596, 1.19627, 1.26637, 1.33625, 1.40593, 1.47542 },
				{ 0.20000, 0.25263, 0.30526, 0.35789, 0.41053, 0.46316, 0.51579, 0.56842, 0.62105, 0.67368, 0.72632, 0.77895, 0.83158, 0.88421, 0.93684, 0.98947, 1.04211, 1.09474, 1.14737, 1.20000 },
				{ 1.03323, 1.04058, 1.04456, 1.04544, 1.04357, 1.03926, 1.03282, 1.02446, 1.01554, 1.00944, 1.00487, 1.00169, 0.99986, 0.99926, 0.99980, 1.00027, 1.00021, 1.00015, 1.00006, 0.99995 },
				{ 0.98344, 0.98630, 0.98876, 0.99081, 0.99247, 0.99379, 0.99486, 0.99574, 0.99649, 0.99716, 0.99774, 0.99826, 0.99877, 0.99926, 0.99972, 1.00017, 1.00060, 1.00103, 1.00143, 1.00182 },
				{ 3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00 },
				{ 0.99269, 0.99520, 0.99718, 0.99882, 1.00024, 1.00150, 1.00264, 1.00368, 1.00464, 1.00554, 1.00637, 1.00716, 1.00790, 1.00840, 1.00905, 1.00965, 1.01022, 1.01075, 1.01126, 1.01173 },
				{ 0.99768, 0.99861, 0.99933, 0.99992, 1.00043, 1.00087, 1.00127, 1.00164, 1.00197, 1.00227, 1.00255, 1.00282, 1.00307, 1.00331, 1.00353, 1.00375, 1.00395, 1.00415, 1.00433, 1.00451 },
				{ 0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000 },
				{ 1.00812, 1.00513, 1.00294, 1.00128, 0.99980, 0.99901, 0.99855, 0.99836, 0.99846, 0.99883, 0.99944, 1.00033, 1.00042, 1.00056, 1.00069, 1.00081, 1.00093, 1.00104, 1.00115, 1.00125 },
				{ 1.09816, 1.07859, 1.06487, 1.05438, 1.04550, 1.03816, 1.03159, 1.02579, 1.02061, 1.01587, 1.01157, 1.00751, 1.00380, 1.00033, 0.99705, 0.99400, 0.99104, 0.98832, 0.98565, 0.98316 }
			};
			m_db.assign(dTemp[0], 18, 20);
		}
	
		else if(ms_params.m_tech_type == 2)
		{	//  Low temperature parabolic trough applications
			double dTemp[18][20] =
			{
				{ 0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211, 0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263, 0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316, 1.33158, 1.40000 },
				{ 0.08547, 0.14823, 0.21378, 0.28166, 0.35143, 0.42264, 0.49482, 0.56747, 0.64012, 0.71236, 0.78378, 0.85406, 0.92284, 0.98989, 1.05685, 1.12369, 1.19018, 1.25624, 1.32197, 1.38744 },
				{ 0.10051, 0.16934, 0.23822, 0.30718, 0.37623, 0.44534, 0.51443, 0.58338, 0.65209, 0.72048, 0.78848, 0.85606, 0.92317, 0.98983, 1.05604, 1.12182, 1.18718, 1.25200, 1.31641, 1.38047 },
				{ 3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00 },
				{ 1.08827, 1.06020, 1.03882, 1.02145, 1.00692, 0.99416, 0.98288, 0.97273, 0.96350, 0.95504, 0.94721, 0.93996, 0.93314, 0.92673, 0.92069, 0.91496, 0.90952, 0.90433, 0.89938, 0.89464 },
				{ 1.01276, 1.00877, 1.00570, 1.00318, 1.00106, 0.99918, 0.99751, 0.99601, 0.99463, 0.99335, 0.99218, 0.99107, 0.99004, 0.98907, 0.98814, 0.98727, 0.98643, 0.98563, 0.98487, 0.98413 },
				{ 0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000 },
				{ 0.09307, 0.16421, 0.23730, 0.31194, 0.38772, 0.46420, 0.54098, 0.61763, 0.69374, 0.76896, 0.84287, 0.91511, 0.98530, 1.05512, 1.12494, 1.19447, 1.26373, 1.33273, 1.40148, 1.46999 },
				{ 0.10741, 0.18443, 0.26031, 0.33528, 0.40950, 0.48308, 0.55610, 0.62861, 0.70066, 0.77229, 0.84354, 0.91443, 0.98497, 1.05520, 1.12514, 1.19478, 1.26416, 1.33329, 1.40217, 1.47081 },
				{ 0.10000, 0.16842, 0.23684, 0.30526, 0.37368, 0.44211, 0.51053, 0.57895, 0.64737, 0.71579, 0.78421, 0.85263, 0.92105, 0.98947, 1.05789, 1.12632, 1.19474, 1.26316, 1.33158, 1.40000 },
				{ 1.01749, 1.03327, 1.04339, 1.04900, 1.05051, 1.04825, 1.04249, 1.03343, 1.02126, 1.01162, 1.00500, 1.00084, 0.99912, 0.99966, 0.99972, 0.99942, 0.99920, 0.99911, 0.99885, 0.99861 },
				{ 0.99137, 0.99297, 0.99431, 0.99564, 0.99681, 0.99778, 0.99855, 0.99910, 0.99948, 0.99971, 0.99984, 0.99989, 0.99993, 0.99993, 0.99992, 0.99992, 0.99992, 1.00009, 1.00010, 1.00012 },
				{ 3000.00, 4263.16, 5526.32, 6789.47, 8052.63, 9315.79, 10578.95, 11842.11, 13105.26, 14368.42, 15631.58, 16894.74, 18157.89, 19421.05, 20684.21, 21947.37, 23210.53, 24473.68, 25736.84, 27000.00 },
				{ 0.99653, 0.99756, 0.99839, 0.99906, 0.99965, 1.00017, 1.00063, 1.00106, 1.00146, 1.00183, 1.00218, 1.00246, 1.00277, 1.00306, 1.00334, 1.00361, 1.00387, 1.00411, 1.00435, 1.00458 },
				{ 0.99760, 0.99831, 0.99888, 0.99934, 0.99973, 1.00008, 1.00039, 1.00067, 1.00093, 1.00118, 1.00140, 1.00161, 1.00180, 1.00199, 1.00217, 1.00234, 1.00250, 1.00265, 1.00280, 1.00294 },
				{ 0.10000, 0.17368, 0.24737, 0.32105, 0.39474, 0.46842, 0.54211, 0.61579, 0.68947, 0.76316, 0.83684, 0.91053, 0.98421, 1.05789, 1.13158, 1.20526, 1.27895, 1.35263, 1.42632, 1.50000 },
				{ 1.01994, 1.01645, 1.01350, 1.01073, 1.00801, 1.00553, 1.00354, 1.00192, 1.00077, 0.99995, 0.99956, 0.99957, 1.00000, 0.99964, 0.99955, 0.99945, 0.99937, 0.99928, 0.99919, 0.99918 },
				{ 1.02055, 1.01864, 1.01869, 1.01783, 1.01508, 1.01265, 1.01031, 1.00832, 1.00637, 1.00454, 1.00301, 1.00141, 1.00008, 0.99851, 0.99715, 0.99586, 0.99464, 0.99347, 0.99227, 0.99177 }
			};
			m_db.assign(dTemp[0], 18, 20);
		}
	
		else if(ms_params.m_tech_type == 3)
		{	//  Sliding pressure power cycle formulation
			double dTemp[18][10] =
			{
				{ 0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556, 0.76667, 0.87778, 0.98889, 1.10000 },
				{ 0.89280, 0.90760, 0.92160, 0.93510, 0.94820, 0.96110, 0.97370, 0.98620, 0.99860, 1.01100 },
				{ 0.93030, 0.94020, 0.94950, 0.95830, 0.96690, 0.97520, 0.98330, 0.99130, 0.99910, 1.00700 },
				{ 4000.00, 6556.00, 9111.00, 11677.0, 14222.0, 16778.0, 19333.0, 21889.0, 24444.0, 27000.0 },
				{ 1.04800, 1.01400, 0.99020, 0.97140, 0.95580, 0.94240, 0.93070, 0.92020, 0.91060, 0.90190 },
				{ 0.99880, 0.99960, 1.00000, 1.00100, 1.00100, 1.00100, 1.00100, 1.00200, 1.00200, 1.00200 },
				{ 0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333, 0.90000, 1.01667, 1.13333, 1.25000 },
				{ 0.16030, 0.27430, 0.39630, 0.52310, 0.65140, 0.77820, 0.90060, 1.01600, 1.12100, 1.21400 },
				{ 0.22410, 0.34700, 0.46640, 0.58270, 0.69570, 0.80550, 0.91180, 1.01400, 1.11300, 1.20700 },
				{ 0.10000, 0.21111, 0.32222, 0.43333, 0.54444, 0.65556, 0.76667, 0.87778, 0.98889, 1.10000 },
				{ 1.05802, 1.05127, 1.04709, 1.03940, 1.03297, 1.02480, 1.01758, 1.00833, 1.00180, 0.99307 },
				{ 1.03671, 1.03314, 1.02894, 1.02370, 1.01912, 1.01549, 1.01002, 1.00486, 1.00034, 0.99554 },
				{ 4000.00, 6556.00, 9111.00, 11677.0, 14222.0, 16778.0, 19333.0, 21889.0, 24444.0, 27000.0 },
				{ 1.00825, 0.98849, 0.99742, 1.02080, 1.02831, 1.03415, 1.03926, 1.04808, 1.05554, 1.05862 },
				{ 1.01838, 1.02970, 0.99785, 0.99663, 0.99542, 0.99183, 0.98897, 0.99299, 0.99013, 0.98798 }, // tweaked entry #4 to be the average of 3 and 5. it was an outlier in the simulation. mjw 3.31.11
				{ 0.20000, 0.31667, 0.43333, 0.55000, 0.66667, 0.78333, 0.90000, 1.01667, 1.13333, 1.25000 },
				{ 1.43311, 1.27347, 1.19090, 1.13367, 1.09073, 1.05602, 1.02693, 1.00103, 0.97899, 0.95912 },
				{ 0.48342, 0.64841, 0.64322, 0.74366, 0.76661, 0.82764, 0.97792, 1.15056, 1.23117, 1.31179 }  // tweaked entry #9 to be the average of 8 and 10. it was an outlier in the simulation mjw 3.31.11
			};
			m_db.assign(dTemp[0], 18, 10);
		}

		else if(ms_params.m_tech_type == 4)
		{	//	Geothermal applications - Isopentane Rankine cycle
			double dTemp[18][20] =
			{
				{ 0.50000, 0.53158, 0.56316, 0.59474, 0.62632, 0.65789, 0.68947, 0.72105, 0.75263, 0.78421, 0.81579, 0.84737, 0.87895, 0.91053, 0.94211, 0.97368, 1.00526, 1.03684, 1.06842, 1.10000 },
				{ 0.55720, 0.58320, 0.60960, 0.63630, 0.66330, 0.69070, 0.71840, 0.74630, 0.77440, 0.80270, 0.83130, 0.85990, 0.88870, 0.91760, 0.94670, 0.97570, 1.00500, 1.03400, 1.06300, 1.09200 },
				{ 0.67620, 0.69590, 0.71570, 0.73570, 0.75580, 0.77600, 0.79630, 0.81670, 0.83720, 0.85780, 0.87840, 0.89910, 0.91990, 0.94070, 0.96150, 0.98230, 1.00300, 1.02400, 1.04500, 1.06600 },
				{ 35000.00, 46315.79, 57631.58, 68947.37, 80263.16, 91578.95, 102894.74, 114210.53, 125526.32, 136842.11, 148157.89, 159473.68, 170789.47, 182105.26, 193421.05, 204736.84, 216052.63, 227368.42, 238684.21, 250000.00 },
				{ 1.94000, 1.77900, 1.65200, 1.54600, 1.45600, 1.37800, 1.30800, 1.24600, 1.18900, 1.13700, 1.08800, 1.04400, 1.00200, 0.96290, 0.92620, 0.89150, 0.85860, 0.82740, 0.79770, 0.76940 },
				{ 1.22400, 1.19100, 1.16400, 1.14000, 1.11900, 1.10000, 1.08300, 1.06700, 1.05200, 1.03800, 1.02500, 1.01200, 1.00000, 0.98880, 0.97780, 0.96720, 0.95710, 0.94720, 0.93770, 0.92850 },
				{ 0.80000, 0.81316, 0.82632, 0.83947, 0.85263, 0.86579, 0.87895, 0.89211, 0.90526, 0.91842, 0.93158, 0.94474, 0.95789, 0.97105, 0.98421, 0.99737, 1.01053, 1.02368, 1.03684, 1.05000 },
				{ 0.84760, 0.85880, 0.86970, 0.88050, 0.89120, 0.90160, 0.91200, 0.92210, 0.93220, 0.94200, 0.95180, 0.96130, 0.97080, 0.98010, 0.98920, 0.99820, 1.00700, 1.01600, 1.02400, 1.03300 },
				{ 0.89590, 0.90350, 0.91100, 0.91840, 0.92570, 0.93290, 0.93990, 0.94680, 0.95370, 0.96040, 0.96700, 0.97350, 0.97990, 0.98620, 0.99240, 0.99850, 1.00400, 1.01000, 1.01500, 1.02100 },
				{ 0.50000, 0.53158, 0.56316, 0.59474, 0.62632, 0.65789, 0.68947, 0.72105, 0.75263, 0.78421, 0.81579, 0.84737, 0.87895, 0.91053, 0.94211, 0.97368, 1.00526, 1.03684, 1.06842, 1.10000 },
				{ 0.79042, 0.80556, 0.82439, 0.84177, 0.85786, 0.87485, 0.88898, 0.90182, 0.91783, 0.93019, 0.93955, 0.95105, 0.96233, 0.97150, 0.98059, 0.98237, 0.99829, 1.00271, 1.02084, 1.02413 },
				{ 0.67400, 0.69477, 0.71830, 0.73778, 0.75991, 0.78079, 0.80052, 0.82622, 0.88152, 0.92737, 0.93608, 0.94800, 0.95774, 0.96653, 0.97792, 0.99852, 0.99701, 1.01295, 1.02825, 1.04294 },
				{ 35000.00, 46315.79, 57631.58, 68947.37, 80263.16, 91578.95, 102894.74, 114210.53, 125526.32, 136842.11, 148157.89, 159473.68, 170789.47, 182105.26, 193421.05, 204736.84, 216052.63, 227368.42, 238684.21, 250000.00 },
				{ 0.80313, 0.82344, 0.83980, 0.86140, 0.87652, 0.89274, 0.91079, 0.92325, 0.93832, 0.95229, 0.97004, 0.98211, 1.00399, 1.01514, 1.03494, 1.04962, 1.06646, 1.08374, 1.10088, 1.11789 },
				{ 0.93426, 0.94458, 0.94618, 0.95878, 0.96352, 0.96738, 0.97058, 0.98007, 0.98185, 0.99048, 0.99144, 0.99914, 1.00696, 1.00849, 1.01573, 1.01973, 1.01982, 1.02577, 1.02850, 1.03585 },
				{ 0.80000, 0.81316, 0.82632, 0.83947, 0.85263, 0.86579, 0.87895, 0.89211, 0.90526, 0.91842, 0.93158, 0.94474, 0.95789, 0.97105, 0.98421, 0.99737, 1.01053, 1.02368, 1.03684, 1.05000 },
				{ 1.06790, 1.06247, 1.05688, 1.05185, 1.04687, 1.04230, 1.03748, 1.03281, 1.02871, 1.02473, 1.02050, 1.01639, 1.01204, 1.00863, 1.00461, 1.00051, 0.99710, 0.99352, 0.98974, 0.98692 },
				{ 1.02335, 1.02130, 1.02041, 1.01912, 1.01655, 1.01601, 1.01379, 1.01431, 1.01321, 1.01207, 1.01129, 1.00784, 1.00548, 1.00348, 1.00183, 0.99982, 0.99698, 0.99457, 0.99124, 0.99016 }
			};
			m_db.assign(dTemp[0], 18, 20);
		}
		else
		{
			m_error_msg = util::format("The power cycle technology type identifier, %d, must be [1..4]", ms_params.m_tech_type);
			throw(C_csp_exception(m_error_msg, "Power cycle initialization"));
		}

	
		ms_params.m_P_cond_min = physics::InHgToPa(ms_params.m_P_cond_min);

		// find min and max hybrid cooling dispatch fractions
		for( int i = 0; i<9; i++ )
		{
			double F_wc_current = ms_params.m_F_wc[i];
			if(F_wc_current < 0.0)
			{
				m_error_msg = util::format("The hybrid dispatch value at TOU period %d was entered as %lg. It was reset to 0", i + 1, F_wc_current);
				mc_csp_messages.add_message(C_csp_messages::WARNING, m_error_msg);
				F_wc_current = 0.0;
			}
			if(F_wc_current > 1.0)
			{
				m_error_msg = util::format("The hybrid dispatch value at TOU period %d was entered as %lg. It was reset to 1.0", i + 1, F_wc_current);
				mc_csp_messages.add_message(C_csp_messages::WARNING, m_error_msg);
				F_wc_current = 1.0;
			}

			m_F_wcMax = fmax(m_F_wcMax, F_wc_current);
			m_F_wcMin = fmin(m_F_wcMin, F_wc_current);

			ms_params.m_F_wc[i] = F_wc_current;
		}

		// Calculate the power block side steam enthalpy rise for blowdown calculations
		// Steam properties are as follows:
		// =======================================================================
		//  | T(C) | P(MPa) | h(kJ/kg) | s(kJ/kg-K) | x(-) | v(m3/kg) | U(kJ/kg) |
		// =======================================================================
		// Limit the boiler pressure to below the supercritical point.  If a supercritical pressure is used,
		// notify the user that the pressure value is being switched.
		if( ms_params.m_P_boil > 220.0 )
		{
			m_error_msg = util::format("The boiler pressure provided by the user, %lg, requires a supercritical system. The pressure has been reset to 220 bar", ms_params.m_P_boil);
			mc_csp_messages.add_message(C_csp_messages::WARNING, m_error_msg);
			ms_params.m_P_boil = 220.0; // Set to 220 bar, 22 MPa
		}

		double h_st_hot, h_st_cold;

		// 1.3.13 twn: Use FIT water props to calculate enthalpy rise over economizer/boiler/superheater
		water_state wp;
		water_TP(ms_params.m_T_htf_hot_ref - GetFieldToTurbineTemperatureDropC() + 273.15, ms_params.m_P_boil*100.0, &wp);	// Get hot side enthalpy [kJ/kg] using Steam Props
		h_st_hot = wp.enth;
		water_PQ(ms_params.m_P_boil*100.0, 0.0, &wp);
		h_st_cold = wp.enth;
		m_delta_h_steam = h_st_hot - h_st_cold + 4.91*100.0;

		m_is_initialized = true;

		// Add the initial call from the (if(m_bFirstCall)) from RankineCycle here:
		// The user provides a reference efficiency, ambient temperature, and cooling system parameters. Using
		// this information, we have to adjust the provided reference efficiency to match the normalized efficiency
		// that is part of the power block regression coefficients. I.e. if the user provides a ref. ambient temperature
		// of 25degC, but the power block coefficients indicate that the normalized efficiency equals 1.0 at an ambient
		// temp of 20degC, we have to adjust the user's efficiency value back to the coefficient set.
		double Psat_ref = 0;
		switch( ms_params.m_CT )
		{
		case 1:		// Wet cooled case
			if( ms_params.m_tech_type != 4 )
			{
				water_TQ(ms_params.m_dT_cw_ref + 3.0 + ms_params.m_T_approach + ms_params.m_T_amb_des + 273.15, 1.0, &wp);
				Psat_ref = wp.pres*1000.0;
			}
			else
			{
				Psat_ref = CSP::P_sat4(ms_params.m_dT_cw_ref + 3.0 + ms_params.m_T_approach + ms_params.m_T_amb_des);	// Isopentane
			}

			break;
		case 2:
		case 3:		// Dry cooled and hyrbid cases
			if( ms_params.m_tech_type != 4 )
			{
				water_TQ(ms_params.m_T_ITD_des + ms_params.m_T_amb_des + 273.15, 1.0, &wp);
				Psat_ref = wp.pres * 1000.0;
			}
			else
			{
				Psat_ref = CSP::P_sat4(ms_params.m_T_ITD_des + ms_params.m_T_amb_des);	// Isopentane
			}

			break;
		}	// end cooling technology switch()

		m_eta_adj = ms_params.m_eta_ref / (Interpolate(12, 2, Psat_ref) / Interpolate(22, 2, Psat_ref));
	}
	else
	{	// Initialization calculations for User Defined power cycle model

		// Load tables into user defined power cycle member class
			// .init method will throw an error if initialization fails, so catch upstream
		mc_user_defined_pc.init( ms_params.mc_T_htf_ind, ms_params.m_T_htf_hot_ref, ms_params.m_T_htf_low, ms_params.m_T_htf_high, 
								ms_params.mc_T_amb_ind, ms_params.m_T_amb_des, ms_params.m_T_amb_low, ms_params.m_T_amb_high,
								ms_params.mc_m_dot_htf_ind, 1.0, ms_params.m_m_dot_htf_low, ms_params.m_m_dot_htf_high );

		if(ms_params.m_W_dot_cooling_des != ms_params.m_W_dot_cooling_des || ms_params.m_W_dot_cooling_des < 0.0 )
		{
			m_error_msg = util::format("The cycle cooling electric parasitic at design input for the user-defined power cycle was either not defined or negative."
									" It was reset to 0.0 for the timeseries simulation");
			mc_csp_messages.add_message(C_csp_messages::WARNING, m_error_msg);
		}

		if(ms_params.m_m_dot_water_des != ms_params.m_m_dot_water_des || ms_params.m_m_dot_water_des < 0.0 )
		{
			m_error_msg = util::format("The cycle water use at design input for the user-defined power cycle was either not defined or negative."
				" It was reset to 0.0 for the timeseries simulation");
			mc_csp_messages.add_message(C_csp_messages::WARNING, m_error_msg);
		}

	}

	// Calculate design point HTF mass flow rate
	m_cp_htf_design = mc_pc_htfProps.Cp(physics::CelciusToKelvin((ms_params.m_T_htf_hot_ref + ms_params.m_T_htf_cold_ref) / 2.0));		//[kJ/kg-K]

	ms_params.m_P_ref *= 1000.0;		//[kW] convert from MW
	m_q_dot_design = ms_params.m_P_ref / 1000.0 / ms_params.m_eta_ref;	//[MWt]
	m_m_dot_design = m_q_dot_design*1000.0 / (m_cp_htf_design*((ms_params.m_T_htf_hot_ref - ms_params.m_T_htf_cold_ref)))*3600.0;		//[kg/hr]
	m_m_dot_min = ms_params.m_cycle_cutoff_frac*m_m_dot_design;		//[kg/hr]
	m_m_dot_max = ms_params.m_cycle_max_frac*m_m_dot_design;		//[kg/hr]

	// 8.30.2010 :: Calculate the startup energy needed
	m_startup_energy_required = ms_params.m_startup_frac * ms_params.m_P_ref / ms_params.m_eta_ref; // [kWt-hr]
	
	// Finally, set member model-timestep-tracking variables
	m_standby_control_prev = OFF;			// Assume power cycle is off when simulation begins
	m_startup_energy_remain_prev = m_startup_energy_required;	//[kW-hr]
	m_startup_time_remain_prev = ms_params.m_startup_time;		//[hr]

	m_ncall = -1;

	solved_params.m_W_dot_des = ms_params.m_P_ref / 1000.0;		//[MW], convert from kW
	solved_params.m_eta_des = ms_params.m_eta_ref;				//[-]
	//solved_params.m_q_dot_des = solved_params.m_W_dot_des / solved_params.m_eta_des;	//[MW]
	solved_params.m_q_dot_des = m_q_dot_design;					//[MWt]
	solved_params.m_q_startup = m_startup_energy_required/1.E3;	//[MWt-hr]
	solved_params.m_max_frac = ms_params.m_cycle_max_frac;		//[-]
	solved_params.m_cutoff_frac = ms_params.m_cycle_cutoff_frac;	//[-]
	solved_params.m_sb_frac = ms_params.m_q_sby_frac;				//[-]
	solved_params.m_T_htf_hot_ref = ms_params.m_T_htf_hot_ref;			//[C]

	// Calculate design point HTF mass flow rate
	// double c_htf = mc_pc_htfProps.Cp(physics::CelciusToKelvin((ms_params.m_T_htf_hot_ref + ms_params.m_T_htf_cold_ref) / 2.0));		//[kJ/kg-K]
	// m_m_dot_design = solved_params.m_q_dot_des*1000.0/(c_htf*((ms_params.m_T_htf_hot_ref - ms_params.m_T_htf_cold_ref)))*3600.0;	//[kg/hr]
	solved_params.m_m_dot_design = m_m_dot_design;		//[kg/hr]
	solved_params.m_m_dot_min = m_m_dot_min;			//[kg/hr]
	solved_params.m_m_dot_max = m_m_dot_max;			//[kg/hr]
}

double C_pc_Rankine_indirect_224::get_cold_startup_time()
{
    //startup time from cold state
    return ms_params.m_startup_time;
}

double C_pc_Rankine_indirect_224::get_warm_startup_time()
{
    //startup time from warm state. No differentiation between cold/hot yet.
    return ms_params.m_startup_time;
}

double C_pc_Rankine_indirect_224::get_hot_startup_time()
{
    //startup time from warm state. No differentiation between cold/warm yet.
    return ms_params.m_startup_time;
}

double C_pc_Rankine_indirect_224::get_standby_energy_requirement()
{
    return ms_params.m_q_sby_frac * ms_params.m_P_ref / ms_params.m_eta_ref *1.e-3;   //MW
}

double C_pc_Rankine_indirect_224::get_cold_startup_energy()
{
    //cold startup energy requirement. No differentiation between warm/hot yet.
    return ms_params.m_startup_frac* ms_params.m_P_ref / ms_params.m_eta_ref*1.e-3; //MWt-hr
}

double C_pc_Rankine_indirect_224::get_warm_startup_energy()
{
    //warm startup energy requirement. No differentiation between cold/hot yet.
    return ms_params.m_startup_frac * ms_params.m_P_ref / ms_params.m_eta_ref*1.e-3; //MWh
}

double C_pc_Rankine_indirect_224::get_hot_startup_energy()
{
    //hot startup energy requirement. No differentiation between cold/hot yet.
    return ms_params.m_startup_frac * ms_params.m_P_ref / ms_params.m_eta_ref*1.e-3; //MWh
}


double C_pc_Rankine_indirect_224::get_max_thermal_power()     //MW
{
    return ms_params.m_cycle_max_frac * ms_params.m_P_ref / ms_params.m_eta_ref*1.e-3;    //MWh
}

double C_pc_Rankine_indirect_224::get_min_thermal_power()     //MW
{
    return ms_params.m_cycle_cutoff_frac * ms_params.m_P_ref / ms_params.m_eta_ref*1.e-3;    //MWh
}

double C_pc_Rankine_indirect_224::get_max_q_pc_startup()
{
	if( m_startup_time_remain_prev > 0.0 )
		return fmin(ms_params.m_cycle_max_frac * ms_params.m_P_ref / ms_params.m_eta_ref*1.e-3, 
			m_startup_energy_remain_prev / 1.E3 / m_startup_time_remain_prev);		//[MWt]
	else if( m_startup_energy_remain_prev > 0.0 )
	{
		return ms_params.m_cycle_max_frac * ms_params.m_P_ref / ms_params.m_eta_ref*1.e-3;    //[MWt]
	}
	else
	{
		return 0.0;
	}
}

double C_pc_Rankine_indirect_224::get_efficiency_at_TPH(double T_degC, double P_atm, double relhum_pct, double *w_dot_condenser)
{
    /* 
    Get cycle efficiency, assuming full load operation. 

    If w_dot_condenser var reference is not null, return the condenser power as well.
    */
    double eta = std::numeric_limits<double>::quiet_NaN();

	if( !ms_params.m_is_user_defined_pc )
	{
		double P_cycle, T_htf_cold, m_dot_demand, m_dot_htf_ref, m_dot_makeup, W_cool_par, f_hrsys, P_cond;

		water_state wprop;

		double Twet = calc_twet(T_degC, relhum_pct, P_atm*1.01325e6);

		RankineCycle(
				//inputs
				T_degC+273.15, Twet+273.15, P_atm*101325., ms_params.m_T_htf_hot_ref, m_m_dot_design, 
				2, 0., ms_params.m_P_boil, 1., m_F_wcMin, m_F_wcMax,
				//outputs
				P_cycle, eta, T_htf_cold, m_dot_demand, m_dot_htf_ref, m_dot_makeup, W_cool_par, f_hrsys, P_cond);

        if( w_dot_condenser != 0 )
            *w_dot_condenser = W_cool_par;
    }
	else
	{
		// User-defined power cycle model

		// Calculate non-dimensional mass flow rate relative to design point
		double m_dot_htf_ND = 1.0;		//[-] Use design point mass flow rate

		// Get ND performance at off-design ambient temperature
		double P_cycle = ms_params.m_P_ref*mc_user_defined_pc.get_W_dot_gross_ND
			(ms_params.m_T_htf_hot_ref,
			T_degC,
			m_dot_htf_ND);	//[kWe]

		double q_dot_htf = m_q_dot_design*mc_user_defined_pc.get_Q_dot_HTF_ND
			(ms_params.m_T_htf_hot_ref,
			T_degC,
			m_dot_htf_ND);	//[MWt]

		eta = P_cycle / 1.E3 / q_dot_htf;

        if( w_dot_condenser != 0 )
            *w_dot_condenser = mc_user_defined_pc.get_W_dot_cooling_ND(
                    ms_params.m_T_htf_hot_ref, 
                    T_degC, 
                    m_dot_htf_ND )
                    *ms_params.m_W_dot_cooling_des;
	}

    return eta;
}

double C_pc_Rankine_indirect_224::get_efficiency_at_load(double load_frac, double *w_dot_condenser)
{
    /* 
    Get cycle efficiency, assuming design point temperature operation
    */
	double eta = std::numeric_limits<double>::quiet_NaN();

	if( !ms_params.m_is_user_defined_pc )
	{
		double cp = mc_pc_htfProps.Cp( (ms_params.m_T_htf_cold_ref + ms_params.m_T_htf_hot_ref)/2. +273.15);  //kJ/kg-K

		//calculate mass flow    [kg/hr]
		double mdot = ms_params.m_P_ref /* kW */ / ( /*ms_params.m_eta_ref*/m_eta_adj * cp * (ms_params.m_T_htf_hot_ref - ms_params.m_T_htf_cold_ref) ) *3600.;
		mdot *= load_frac;

		//ambient calculations
		water_state wprop;
		double Twet = calc_twet(ms_params.m_T_amb_des, 45, 1.01325e6);

		//Call
		double P_cycle, T_htf_cold, m_dot_demand, m_dot_htf_ref, m_dot_makeup, W_cool_par, f_hrsys, P_cond;

        RankineCycle(
			    //inputs
			    ms_params.m_T_amb_des+273.15, Twet+273.15, 101325., ms_params.m_T_htf_hot_ref, mdot, 2, 
                0., ms_params.m_P_boil, 1., m_F_wcMin, m_F_wcMax, 
			    //outputs
			    P_cycle, eta, T_htf_cold, m_dot_demand, m_dot_htf_ref, m_dot_makeup, W_cool_par, f_hrsys, P_cond);
        
        if( w_dot_condenser != 0 )
            *w_dot_condenser = W_cool_par;
	}
	else
	{
		// User-defined power cycle model

		// Calculate non-dimensional mass flow rate relative to design point
		double m_dot_htf_ND = load_frac;		//[-] Use design point mass flow rate

		// Get ND performance at off-design ambient temperature
		double P_cycle = ms_params.m_P_ref*mc_user_defined_pc.get_W_dot_gross_ND
			(ms_params.m_T_htf_hot_ref,
			ms_params.m_T_amb_des,
			m_dot_htf_ND);	//[kWe]

		double q_dot_htf = m_q_dot_design*mc_user_defined_pc.get_Q_dot_HTF_ND
			(ms_params.m_T_htf_hot_ref,
			ms_params.m_T_amb_des,
			m_dot_htf_ND);	//[MWt]

		eta = P_cycle / 1.E3 / q_dot_htf;

        if( w_dot_condenser != 0 )
            *w_dot_condenser = mc_user_defined_pc.get_W_dot_cooling_ND(
                    ms_params.m_T_htf_hot_ref, 
                    ms_params.m_T_amb_des, 
                    m_dot_htf_ND )
                    *ms_params.m_W_dot_cooling_des;
	}
    
    return eta;
}

double C_pc_Rankine_indirect_224::get_htf_pumping_parasitic_coef()
{
	return ms_params.m_htf_pump_coef* (m_m_dot_design / 3600.) / (m_q_dot_design*1000.);	// kWe/kWt
}


void C_pc_Rankine_indirect_224::call(const C_csp_weatherreader::S_outputs &weather, 
	C_csp_solver_htf_1state &htf_state_in,
	const C_csp_power_cycle::S_control_inputs & inputs,
	C_csp_power_cycle::S_csp_pc_out_solver &out_solver,
	//C_csp_power_cycle::S_csp_pc_out_report &out_report,
	const C_csp_solver_sim_info &sim_info)
{
	// Increase call-per-timestep counter
	// Converge() sets it to -1, so on first call this line will adjust it = 0
	m_ncall++;

	// Get sim info
	double time = sim_info.ms_ts.m_time;			//[s]
	double step_sec = sim_info.ms_ts.m_step;		//[s]
	//int ncall = p_sim_info->m_ncall;

	// Check and convert inputs
	double T_htf_hot = htf_state_in.m_temp;		//[C] 
	double m_dot_htf = inputs.m_m_dot;			//[kg/hr]
	double T_wb = weather.m_twet + 273.15;		//[K], converted from C
	int standby_control = inputs.m_standby_control;	//[-] 1: On, 2: Standby, 3: Off
	double T_db = weather.m_tdry + 273.15;		//[K], converted from C
	double P_amb = weather.m_pres*100.0;		//[Pa], converted from mbar
	int tou = sim_info.m_tou - 1;				//[-], convert from 1-based index
	//double rh = weather.m_rhum/100.0;			//[-], convert from %

	double m_dot_st_bd = 0.0;

	double P_cycle, eta, T_htf_cold, m_dot_demand, m_dot_htf_ref, m_dot_water_cooling, W_cool_par, f_hrsys, P_cond;
	P_cycle = eta = T_htf_cold = m_dot_demand = m_dot_htf_ref = m_dot_water_cooling = W_cool_par = f_hrsys = P_cond = std::numeric_limits<double>::quiet_NaN();

	// 4.15.15 twn: hardcode these so they don't have to be passed into call(). Mode is always = 2 for CSP simulations
	int mode = 2;
	double demand_var = 0.0;

	double q_dot_htf = std::numeric_limits<double>::quiet_NaN();	//[MWt]

	double time_required_su = 0.0;

	m_standby_control_calc = standby_control;

	double q_startup = 0.0;

	bool was_method_successful = true;

	switch(standby_control)
	{
	case STARTUP:
		{
			double c_htf = mc_pc_htfProps.Cp(physics::CelciusToKelvin((T_htf_hot + ms_params.m_T_htf_cold_ref) / 2.0));		//[kJ/kg-K]

			double time_required_su_energy = m_startup_energy_remain_prev / (m_dot_htf*c_htf*(T_htf_hot - ms_params.m_T_htf_cold_ref)/3600);	//[hr]
			double time_required_su_ramping = m_startup_time_remain_prev;	//[hr]

			double time_required_max = fmax(time_required_su_energy, time_required_su_ramping);

			double time_step_hrs = step_sec / 3600.0;	//[hr]


			if( time_required_max > time_step_hrs )
			{
				time_required_su = time_step_hrs;		//[hr]
				m_standby_control_calc = STARTUP;	//[-] Power cycle requires additional startup next timestep
				q_startup = m_dot_htf*c_htf*(T_htf_hot - ms_params.m_T_htf_cold_ref)*time_step_hrs/3600.0;	//[kW-hr]
			}
			else
			{
				time_required_su = time_required_max;	//[hr]
				m_standby_control_calc = ON;	//[-] Power cycle has started up, next time step it will be ON
				
				double q_startup_energy_req = m_startup_energy_remain_prev;	//[kWt-hr]
				double q_startup_ramping_req = m_dot_htf*c_htf*(T_htf_hot - ms_params.m_T_htf_cold_ref)*m_startup_time_remain_prev/3600.0;	//[kWt-hr]
				q_startup = fmax(q_startup_energy_req, q_startup_ramping_req);	//[kWt-hr]

				// ******************

			}

			m_startup_time_remain_calc = fmax(m_startup_time_remain_prev - time_required_su, 0.0);	//[hr]
			m_startup_energy_remain_calc = fmax(m_startup_energy_remain_prev - q_startup, 0.0);		//[kWt-hr]
		}

		q_dot_htf = q_startup/1000.0 / (time_required_su);	//[kWt-hr] * [MW/kW] * [1/hr] = [MWt]

		// *****
		P_cycle = 0.0;		
		eta = 0.0;									
		T_htf_cold = ms_params.m_T_htf_cold_ref;
		// *****
		m_dot_demand = 0.0;
		m_dot_water_cooling = 0.0;
		W_cool_par = 0.0;
		f_hrsys = 0.0;
		P_cond = 0.0;
		m_dot_st_bd = 0.0;

		was_method_successful = true;

		break;

	case ON:
		
		if( !ms_params.m_is_user_defined_pc )
		{

            RankineCycle(T_db, T_wb, P_amb, T_htf_hot, m_dot_htf, mode, demand_var, ms_params.m_P_boil, 
                ms_params.m_F_wc[tou], m_F_wcMin, m_F_wcMax, 
			P_cycle, eta, T_htf_cold, m_dot_demand, m_dot_htf_ref, m_dot_water_cooling, W_cool_par, f_hrsys, P_cond);

			// Check the output to make sure it's reasonable. If not, return zeros.
			if( ((eta > 1.0) || (eta < 0.0)) || ((T_htf_cold > T_htf_hot) || (T_htf_cold < ms_params.m_T_htf_cold_ref - 50.0)) )
			{
				P_cycle = 0.0;
				eta = 0.0;
				T_htf_cold = ms_params.m_T_htf_cold_ref;
				// 7.10.13 twn: set ALL outputs to 0
				m_dot_demand = 0.0;
				m_dot_water_cooling = 0.0;
				W_cool_par = 0.0;
				f_hrsys = 0.0;
				P_cond = 0.0;
				q_dot_htf = 0.0;

				// 7.17.16 twn: don't want the controller/solver to key off P_cyle == 0, so add boolean
				was_method_successful = false;
			}
			else
			{
				q_dot_htf = P_cycle/1000.0/eta;		//[MWt]
				was_method_successful = true;
			}

			// -----Calculate the blowdown fraction-----
			if( ms_params.m_tech_type != 4 )
				m_dot_st_bd = P_cycle / fmax((eta * m_delta_h_steam), 1.e-6) * ms_params.m_pb_bd_frac;	//[kg/s]
			else
				m_dot_st_bd = 0; // Added Aug 3, 2011 for Isopentane Rankine cycle

		}
		else
		{
			// User-defined power cycle model

			// Calculate non-dimensional mass flow rate relative to design point
			double m_dot_htf_ND = m_dot_htf / m_m_dot_design;         //[-]

			// Get ND performance at off-design / part-load conditions
			P_cycle = ms_params.m_P_ref*mc_user_defined_pc.get_W_dot_gross_ND(T_htf_hot, T_db - 273.15, m_dot_htf_ND);	//[kW]

			q_dot_htf = m_q_dot_design*mc_user_defined_pc.get_Q_dot_HTF_ND(T_htf_hot, T_db - 273.15, m_dot_htf_ND);		//[MWt]

			W_cool_par = ms_params.m_W_dot_cooling_des*mc_user_defined_pc.get_W_dot_cooling_ND(T_htf_hot, T_db - 273.15, m_dot_htf_ND);	//[MW]

			m_dot_water_cooling = ms_params.m_m_dot_water_des*mc_user_defined_pc.get_m_dot_water_ND(T_htf_hot, T_db - 273.15, m_dot_htf_ND);	//[kg/hr]

			// Check power cycle outputs to be sure that they are reasonable. If not, return zeros
			if( ((eta > 1.0) || (eta < 0.0)) || ((T_htf_cold > T_htf_hot) || (T_htf_cold < ms_params.m_T_htf_cold_ref - 100.0)) )
			{
				P_cycle = 0.0;
				eta = 0.0;
				T_htf_cold = ms_params.m_T_htf_cold_ref;
				W_cool_par = 0.0;
				m_dot_water_cooling = 0.0;
				q_dot_htf = 0.0;

				// 7.17.16 twn: don't want the controller/solver to key off P_cyle == 0, so add boolean
				was_method_successful = false;
			}
			else
			{
				// Calculate other important metrics
				eta = P_cycle / 1.E3 / q_dot_htf;		//[-]

				// Want to iterate to fine more accurate cp_htf?
				T_htf_cold = T_htf_hot - q_dot_htf / (m_dot_htf / 3600.0*m_cp_htf_design / 1.E3);		//[MJ/s * hr/kg * s/hr * kg-K/kJ * MJ/kJ] = C/K

				was_method_successful = true;
			}

			if( W_cool_par < 0.0 )
			{
				W_cool_par = 0.0;
			}

			if( m_dot_water_cooling < 0.0 )
			{
				m_dot_water_cooling = 0.0;
			}
			
			m_dot_st_bd = 0.0;					//[kg/hr] 
			m_dot_htf_ref = m_m_dot_design;		//[kg/hr]
			
			f_hrsys = 0.0;		//[-] Not captured in User-defined power cycle model
			P_cond = 0.0;		//[Pa] Not captured in User-defined power cycle model
			m_dot_demand = 0.0;	//[kg/hr] Not captured in User-defined power cycle model
		}

		break;

	case STANDBY:
		{
			double c_htf = mc_pc_htfProps.Cp(physics::CelciusToKelvin((T_htf_hot + ms_params.m_T_htf_cold_ref) / 2.0));	//[kJ/kg-K]
			// double c_htf = specheat(m_pbp.HTF, physics::CelciusToKelvin((m_pbi.T_htf_hot + m_pbp.T_htf_cold_ref)/2.0), 1.0);
			double q_tot = ms_params.m_P_ref / ms_params.m_eta_ref;

			// Calculate the actual q_sby_needed from the reference flows
			double q_sby_needed = q_tot * ms_params.m_q_sby_frac;

			// now calculate the mass flow rate knowing the inlet temperature of the salt,
			// ..and holding the outlet temperature at the reference outlet temperature
			double m_dot_sby = q_sby_needed / (c_htf * (T_htf_hot - ms_params.m_T_htf_cold_ref))*3600.0;

			// Set other output values
			P_cycle = 0.0;
			eta = 0.0;
			T_htf_cold = ms_params.m_T_htf_cold_ref;
			m_dot_demand = m_dot_sby;
			m_dot_st_bd = 0.0;
			m_dot_water_cooling = 0.0;
			W_cool_par = 0.0;
			f_hrsys = 0.0;
			P_cond = 0.0;

			q_dot_htf = m_dot_htf/3600.0*c_htf*(T_htf_hot - T_htf_cold)/1000.0;		//[MWt]

			was_method_successful = true;
		}

		break;

	case OFF:

		// Set other output values
		P_cycle = 0.0;
		eta = 0.0;
		T_htf_cold = ms_params.m_T_htf_cold_ref;
		m_dot_demand = 0.0;
		m_dot_water_cooling = 0.0;
		m_dot_st_bd = 0.0;
		W_cool_par = 0.0;
		f_hrsys = 0.0;
		P_cond = 0.0;
		
		q_dot_htf = 0.0;

		// Cycle is off, so reset startup parameters!
		m_startup_time_remain_calc = ms_params.m_startup_time;			//[hr]
		m_startup_energy_remain_calc = m_startup_energy_required;		//[kWt-hr]

		was_method_successful = true;

		break;

	case STARTUP_CONTROLLED:
		// Thermal input can be controlled (e.g. TES mass flow rate is adjustable, rather than direct connection
		//     to the receiver), so find the mass flow rate that results in the required energy input can be achieved
		//     simultaneously with the required startup time. If the timestep is less than the required startup time
		//     scale the mass flow rate appropriately

		double c_htf = mc_pc_htfProps.Cp(physics::CelciusToKelvin((T_htf_hot + ms_params.m_T_htf_cold_ref) / 2.0));		//[kJ/kg-K]
		
			// Maximum thermal power to power cycle based on heat input constraint parameters:
		double q_dot_to_pc_max_q_constraint = ms_params.m_cycle_max_frac * ms_params.m_P_ref / ms_params.m_eta_ref;	//[kWt]
			// Maximum thermal power to power cycle based on mass flow rate constraint parameters:
		double q_dot_to_pc_max_m_constraint = m_m_dot_max / 3600.0 * c_htf * (T_htf_hot - ms_params.m_T_htf_cold_ref);	//[kWt]
			// Choose smaller of two values
		double q_dot_to_pc_max = fmin(q_dot_to_pc_max_q_constraint, q_dot_to_pc_max_m_constraint);	//[kWt]

		double time_required_su_energy = m_startup_energy_remain_prev / q_dot_to_pc_max;		//[hr]
		double time_required_su_ramping = m_startup_time_remain_prev;		//[hr]

		double q_dot_to_pc = std::numeric_limits<double>::quiet_NaN();

		if( time_required_su_energy > time_required_su_ramping )	// Meeting energy requirements (at design thermal input) will require more time than time requirements
		{
			// Can the power cycle startup within the timestep?
			if( time_required_su_energy > step_sec / 3600.0 )	// No: the power cycle startup will require another timestep
			{
				time_required_su = step_sec / 3600.0;	//[hr]
				m_standby_control_calc = STARTUP;		//[-] Power cycle requires additional startup next timestep

			}	
			else	// Yes: the power cycle will complete startup within this timestep
			{
				time_required_su = time_required_su_energy;	//[hr]
				m_standby_control_calc = ON;				//[-] Power cycle has started up, next time step it will be ON
			}		
			// If the thermal energy requirement is the limiting factor, then send max q_dot to power cycle
			q_dot_to_pc = q_dot_to_pc_max;		//[kWt]
		}
		else		// Meeting time requirements will require more time than energy requirements (at design thermal input)
		{
			// Can the power cycle startup within the timestep?
			if( time_required_su_ramping > step_sec / 3600.0 )	// No: the power cycle startup will require another timestep
			{
				time_required_su = step_sec / 3600.0;			//[hr]
				m_standby_control_calc = STARTUP;		//[-] Power cycle requires additional startup next timestep
			}
			else	// Yes: the power cycle will complete startup within this timestep
			{
				time_required_su = time_required_su_ramping;	//[hr]
				m_standby_control_calc = ON;					//[-] Power cycle has started up, next time step it will be ON
			}

			// 2.27.17 twn: now need to recalculate q_dot_to_pc based on having more time to deliver energy requirement
			// 3.28.17 twn: use 'time_required_su_ramping' insteand of 'time_required_su'
			q_dot_to_pc = m_startup_energy_remain_prev / time_required_su_ramping;		//[kWt]
		}
		q_startup = q_dot_to_pc*time_required_su;	//[kWt-hr]

		double m_dot_htf_required = (q_startup/time_required_su) / (c_htf*(T_htf_hot - ms_params.m_T_htf_cold_ref));	//[kg/s]
		double m_dot_htf_req_kg_s = m_dot_htf_required*3600.0;	//[kg/hr], convert from kg/s

		m_startup_time_remain_calc = fmax(m_startup_time_remain_prev - time_required_su, 0.0);	//[hr]
		m_startup_energy_remain_calc = fmax(m_startup_energy_remain_prev - q_startup, 0.0);		//[kWt-hr]	


		// Set other output values
		P_cycle = 0.0;
		eta = 0.0;
		T_htf_cold = ms_params.m_T_htf_cold_ref;
		m_dot_htf = m_dot_htf_req_kg_s;		//[kg/hr]
		//m_dot_demand = m_dot_htf_required*3600.0;		//[kg/hr], convert from kg/s
		m_dot_water_cooling = 0.0;
		m_dot_st_bd = 0.0;
		W_cool_par = 0.0;
		f_hrsys = 0.0;
		P_cond = 0.0;

		q_dot_htf = m_dot_htf_required*c_htf*(T_htf_hot - ms_params.m_T_htf_cold_ref)/1000.0;	//[MWt]

		was_method_successful = true;

		break;
	
	}	// end switch() on standby control


	// If the cycle is starting up beginning in this time period, or it is continuing to start
	// up from the last time period, then subtract the appropriate fraction of electric power
	// from the output.  Note that during the startup time, not only is the cycle not producing power,
	// but it is also consuming thermal energy
	if( P_cycle > 0.0 )
	{
		if( (m_startup_time_remain_prev + m_startup_energy_remain_prev) > 1.E-6 )
		{

			// Adjust the power cycle output. Both the energy and time requirement must be met before power is produced,
			// so subtract the maximum of these two values
			double Q_cycle = P_cycle / eta;		//[kW]

			/*
			// Alternative method for calculations (f_st & P_cycle) below
			double startup_e_used;
			if( m_dStartupERemain < Q_cycle*m_dHoursSinceLastStep )
			{
			startup_e_used = m_dStartupERemain;
			if( dmin1(1.0, m_dStartupRemain/m_dHoursSinceLastStep) > startup_e_used/(Q_cycle*m_dHoursSinceLastStep) )
			{
			double f_st = 1.0 - dmin1(1.0, m_dStartupRemain/m_dHoursSinceLastStep);
			m_pbo.P_cycle *= f_st;
			}
			else
			m_pbo.P_cycle -= (startup_e_used * m_pbo.eta);
			}
			else
			{
			startup_e_used = Q_cycle * m_dHoursSinceLastStep;
			m_pbo.P_cycle = 0.0;
			}
			*/

			// ******
			double step_hrs = step_sec / 3600.0;

			double startup_e_used = fmin(Q_cycle * step_hrs, m_startup_energy_remain_prev);       // The used startup energy is the less of the energy to the power block and the remaining startup requirement

			double f_st = 1.0 - fmax(fmin(1.0, m_startup_time_remain_prev / step_hrs), startup_e_used / (Q_cycle*step_hrs));
			P_cycle = P_cycle*f_st;
			// *****

			// Fraction of the timestep running at full capacity
			// The power cycle still requires mass flow to satisfy the energy demand requirement, so only subtract demand mass flow
			// for the case when startup time exceeds startup energy.
			m_dot_demand = m_dot_demand * (1.0 - fmax(fmin(1.0, m_startup_time_remain_prev / step_hrs) - startup_e_used / (Q_cycle*step_hrs), 0.0));

			eta = ms_params.m_eta_ref;		// Using reference efficiency because starting up during this timestep
			T_htf_cold = ms_params.m_T_htf_cold_ref;
			// m_dot_htf_ref = m_dot_htf_ref // TFF - huh? Is this a typo?

			m_startup_time_remain_calc = fmax(m_startup_time_remain_prev - step_hrs, 0.0);
			m_startup_energy_remain_calc = fmax(m_startup_energy_remain_prev - startup_e_used, 0.0);

			double c_htf = mc_pc_htfProps.Cp(physics::CelciusToKelvin((T_htf_hot + ms_params.m_T_htf_cold_ref) / 2.0));		//[kJ/kg-K]
			// If still starting up, then all of energy input going to startup
			if(m_startup_time_remain_calc + m_startup_energy_remain_calc > 0.0)
			{
				q_startup = m_dot_htf*c_htf*(T_htf_hot - ms_params.m_T_htf_cold_ref)*step_hrs / 3600.0;	//[kW-hr]
				
				time_required_su = step_hrs;						//[hr]
			}
			else
			{
				double q_startup_energy_req = m_startup_energy_remain_prev;	//[kWt-hr]
				double q_startup_ramping_req = m_dot_htf*c_htf*(T_htf_hot - ms_params.m_T_htf_cold_ref)*m_startup_time_remain_prev/3600.0;	//[kWt-hr]
				q_startup = fmax(q_startup_energy_req, q_startup_ramping_req);	//[kWt-hr]

				time_required_su = m_startup_time_remain_prev;		//[hr]
			}
		}
	}	

	// Set outputs
	out_solver.m_P_cycle = P_cycle/1000.0;				//[MWe] Cycle power output, convert from kWe
	mc_reported_outputs.value(E_W_DOT, P_cycle/1000.0);	//[MWe] Cycle power output, convert from kWe

	//out_report.m_eta = eta;							//[-] Cycle thermal efficiency
	mc_reported_outputs.value(E_ETA_THERMAL, eta);	//[-] Cycle thermal efficiency

	out_solver.m_T_htf_cold = T_htf_cold;				//[C] HTF outlet temperature
	mc_reported_outputs.value(E_T_HTF_OUT, T_htf_cold);	//[C] HTF outlet temperature
	mc_reported_outputs.value(E_T_HTF_IN, T_htf_hot);	//[C] HTF inlet temperature

	//out_report.m_m_dot_makeup = (m_dot_water_cooling + m_dot_st_bd)*3600.0;		//[kg/hr] Cooling water makeup flow rate, convert from kg/s
	mc_reported_outputs.value(E_M_DOT_WATER, (m_dot_water_cooling + m_dot_st_bd)*3600.0);		//[kg/hr] Cooling water makeup flow rate, convert from kg/s

	//out_report.m_m_dot_demand = m_dot_demand;			//[kg/hr] HTF required flow rate to meet power load
	
	out_solver.m_m_dot_htf = m_dot_htf;					//[kg/hr] Actual HTF flow rate passing through the power cycle
	mc_reported_outputs.value(E_M_DOT_HTF,m_dot_htf);	//[kg/hr] Actual HTF flow rate passing through the power cycle
	
	//out_report.m_m_dot_htf_ref = m_dot_htf_ref;		//[kg/hr] Calculated reference HTF flow rate at design
	mc_reported_outputs.value(E_M_DOT_HTF_REF, m_dot_htf_ref);	//[kg/hr]
	out_solver.m_W_cool_par = W_cool_par;				//[MWe] Cooling system parasitic load
	//out_report.m_P_ref = ms_params.m_P_ref / 1000.0;		//[MWe] Reference power level output at design, convert from kWe
	//out_report.m_f_hrsys = f_hrsys;					//[-] Fraction of operating heat rejection system
	//out_report.m_P_cond = P_cond;						//[Pa] Condenser pressure

	//outputs.m_q_startup = q_startup / 1.E3;					//[MWt-hr] Startup energy
	double q_dot_startup = 0.0;
	if( q_startup > 0.0 )
		q_dot_startup = q_startup / 1.E3 / time_required_su;	//[MWt] Startup thermal power
	else
		q_dot_startup = 0.0;
	//out_report.m_q_startup = q_dot_startup;						//[MWt] Startup thermal power
	mc_reported_outputs.value(E_Q_DOT_STARTUP, q_dot_startup);	//[MWt] Startup thermal power

	out_solver.m_time_required_su = time_required_su*3600.0;	//[s]
	
	out_solver.m_q_dot_htf = q_dot_htf;					//[MWt] Thermal power from HTF (= thermal power into cycle)
	mc_reported_outputs.value(E_Q_DOT_HTF, q_dot_htf);	//[MWt] Thermal power from HTF (= thermal power into cycle)
	
	out_solver.m_W_dot_htf_pump = ms_params.m_htf_pump_coef*(m_dot_htf / 3.6E6);	//[MW] HTF pumping power, convert from [kW/kg/s]*[kg/hr]    

	out_solver.m_was_method_successful = was_method_successful;	//[-]
}

void C_pc_Rankine_indirect_224::converged()
{
	m_standby_control_prev = m_standby_control_calc;
	m_startup_time_remain_prev = m_startup_time_remain_calc;
	m_startup_energy_remain_prev = m_startup_energy_remain_calc;

	m_ncall = -1;

	mc_reported_outputs.set_timestep_outputs();
}

void C_pc_Rankine_indirect_224::write_output_intervals(double report_time_start,
	const std::vector<double> & v_temp_ts_time_end, double report_time_end)
{
	mc_reported_outputs.send_to_reporting_ts_array(report_time_start,
		v_temp_ts_time_end, report_time_end);
}

void C_pc_Rankine_indirect_224::assign(int index, float *p_reporting_ts_array, int n_reporting_ts_array)
{
	mc_reported_outputs.assign(index, p_reporting_ts_array, n_reporting_ts_array);
}

int C_pc_Rankine_indirect_224::get_operating_state()
{
	if(ms_params.m_startup_frac == 0.0 && ms_params.m_startup_time == 0.0)
	{
		return C_csp_power_cycle::ON;
	}

	return m_standby_control_prev;
}

void C_pc_Rankine_indirect_224::RankineCycle(double T_db, double T_wb,
		double P_amb, double T_htf_hot, double m_dot_htf, int mode,
		double demand_var, double P_boil, double F_wc, double F_wcmin, double F_wcmax, 
        //outputs
        double& P_cycle, double& eta, double& T_htf_cold, double& m_dot_demand, double& m_dot_htf_ref,
		double& m_dot_makeup, double& W_cool_par, double& f_hrsys, double& P_cond)
{
	
    //local names for parameters
    double P_ref = ms_params.m_P_ref;
    double T_htf_hot_ref = ms_params.m_T_htf_hot_ref;
    double T_htf_cold_ref = ms_params.m_T_htf_cold_ref;
    double dT_cw_ref = ms_params.m_dT_cw_ref;
    double T_amb_des = ms_params.m_T_amb_des;
    double T_approach = ms_params.m_T_approach;
    double T_ITD_des = ms_params.m_T_ITD_des;
    double P_cond_ratio = ms_params.m_P_cond_ratio;
    double P_cond_min = ms_params.m_P_cond_min;
    
    
    water_state wp;

	// Calculate the specific heat before converting to Kelvin
	double c_htf_ref = mc_pc_htfProps.Cp(physics::CelciusToKelvin((T_htf_hot_ref + T_htf_cold_ref) / 2.0));
	double c_htf = mc_pc_htfProps.Cp(physics::CelciusToKelvin((T_htf_hot + T_htf_cold_ref) / 2.0));

	// Convert units
	// **Temperatures from Celcius to Kelvin
	T_htf_hot = physics::CelciusToKelvin(T_htf_hot);			//[K]
	T_htf_hot_ref = physics::CelciusToKelvin(T_htf_hot_ref);	//[K]
	T_htf_cold_ref = physics::CelciusToKelvin(T_htf_cold_ref);	//[K]
	// Mass flow rates from kg/hr to kg/s
	m_dot_htf = m_dot_htf / 3600.0; // [kg/s]

	// ****Calculate the reference values
	double q_dot_ref = P_ref / m_eta_adj;   // The reference heat flow
	m_dot_htf_ref = q_dot_ref / (c_htf_ref*(T_htf_hot_ref - T_htf_cold_ref));  // The HTF mass flow rate [kg/s]

	double T_ref = 0; // The saturation temp at the boiler
	if( ms_params.m_tech_type == 4 )
		T_ref = T_sat4(P_boil); // Sat temp for isopentane
	else
	{
		water_PQ(P_boil * 100, 1.0, &wp);
		T_ref = wp.temp;	//[K]
	}

	// Calculate the htf hot temperature, in non-dimensional form
	if( T_ref >= T_htf_hot )
	{	// boiler pressure is unrealistic -> it could not be achieved with this resource temp
		mc_csp_messages.add_message(C_csp_messages::WARNING,"The input boiler pressure could not be achieved with the resource temperature entered.");
		//P_cycle = 0.0;
	}
	
	double T_htf_hot_ND = (T_htf_hot - T_ref) / (T_htf_hot_ref - T_ref);

	// Calculate the htf mass flow rate in non-dimensional form
	double m_dot_htf_ND = m_dot_htf / m_dot_htf_ref;

	// Do an initial cooling tower call to estimate the turbine back pressure.
	double q_reject_est = q_dot_ref*1000.0*(1.0 - m_eta_adj)*m_dot_htf_ND*T_htf_hot_ND;

	double T_cond = 0, m_dot_air = 0, W_cool_parhac = 0, W_cool_parhwc = 0;
	switch( ms_params.m_CT )  // Cooling technology type {1=evaporative cooling, 2=air cooling, 3=hybrid cooling}
	{
	case 1:
		// For a wet-cooled system
		CSP::evap_tower(ms_params.m_tech_type, P_cond_min, ms_params.m_n_pl_inc, dT_cw_ref, T_approach, (P_ref*1000.), m_eta_adj, T_db, T_wb, P_amb, q_reject_est, m_dot_makeup, W_cool_par, P_cond, T_cond, f_hrsys);
		break;
	case 2:
		// For a dry-cooled system
		CSP::ACC(ms_params.m_tech_type, P_cond_min, ms_params.m_n_pl_inc, T_ITD_des, P_cond_ratio, (P_ref*1000.), m_eta_adj, T_db, P_amb, q_reject_est, m_dot_air, W_cool_par, P_cond, T_cond, f_hrsys);
		m_dot_makeup = 0.0;
		break;
	case 3:
		// for a hybrid cooled system
		CSP::HybridHR(/*fcall,*/ms_params.m_tech_type, P_cond_min, ms_params.m_n_pl_inc, F_wc, F_wcmax, F_wcmin, T_ITD_des, T_approach, dT_cw_ref, P_cond_ratio, (P_ref*1000.), m_eta_adj, T_db, T_wb,
			P_amb, q_reject_est, m_dot_makeup, W_cool_parhac, W_cool_parhwc, W_cool_par, P_cond, T_cond, f_hrsys);
		break;
	}

	//   Set initial values
	double ADJ = 1.0, err = 1.0; /*qq=0;*/

	// Do a quick check to see if there is actually a mass flow being supplied
	//   to the cycle. If not, go to the end.
	if( fabs(m_dot_htf_ND) < 1.0E-3 )
	{
		P_cycle = 0.0;
		eta = 0.0;
		T_htf_cold = T_htf_hot_ref;
		m_dot_demand = m_dot_htf_ref;
		W_cool_par = 0.0;
		m_dot_makeup = 0.0;
		// Set the error to zero, since we don't want to iterate
		err = 0.0;
	}

	double P_dem_ND, P_AB, P_CA, P_BC, Q_AB, Q_CA, Q_BC, P_ND_tot, Q_ND_tot, q_reject;
	double P_ND[3], Q_ND[3];
	double P_cond_guess = 0.0;
	double P_cond_low = -1.0;
	double P_cond_high = -1.0;
	// Begin iterations
	//do while ((err.gt.1.e-6).and.(qq.lt.100))
	for( int qq = 1; qq<100; qq++ )
	{
		if( err <= 1.0E-6 ) break;
		/*qq=qq+1*/

		// Now use the constrained variable to calculate the demand mass flow rate
		if( mode == 1 )
		{
			P_dem_ND = demand_var / P_ref;
			if( qq == 1 ) m_dot_htf_ND = P_dem_ND;   // An initial guess (function of power)
			// if(qq.gt.1) m_dot_htf_ND = m_dot_htf_ND*ADJ
		}
		/*
		elseif(mode == 2.) then
		continue     //  do nothing
		endif*/

		// ++++++++++++++Correlations++++++++++++++++++
		// Calculate the correlations
		// ++++++++++++++++++++++++++++++++++++++++++++
		// POWER
		// Main effects
		P_ND[0] = Interpolate(11, 1, T_htf_hot_ND) - 1.0;
		P_ND[1] = Interpolate(12, 2, P_cond) - 1.0;
		P_ND[2] = Interpolate(13, 3, m_dot_htf_ND) - 1.0;

		// Interactions
		P_CA = Interpolate(113, 13, T_htf_hot_ND);
		P_AB = Interpolate(112, 12, P_cond);
		P_BC = Interpolate(123, 23, m_dot_htf_ND);

		P_ND[0] = P_ND[0] * P_AB;
		P_ND[1] = P_ND[1] * P_BC;
		P_ND[2] = P_ND[2] * P_CA;

		// HEAT
		// Main effects
		Q_ND[0] = Interpolate(21, 1, T_htf_hot_ND) - 1.0;
		Q_ND[1] = Interpolate(22, 2, P_cond) - 1.0;
		Q_ND[2] = Interpolate(23, 3, m_dot_htf_ND) - 1.0;

		// Interactions
		Q_CA = Interpolate(213, 13, T_htf_hot_ND);
		Q_AB = Interpolate(212, 12, P_cond);
		Q_BC = Interpolate(223, 23, m_dot_htf_ND);

		Q_ND[0] = Q_ND[0] * Q_AB;
		Q_ND[1] = Q_ND[1] * Q_BC;
		Q_ND[2] = Q_ND[2] * Q_CA;

		// Calculate the cumulative values
		P_ND_tot = 1.0;
		Q_ND_tot = 1.0;

		// Increment main effects. MJW 8.11.2010 :: For this system, the effects are multiplicative.
		for( int i = 0; i<3; i++ )
		{
			P_ND_tot = P_ND_tot * (1.0 + P_ND[i]);
			Q_ND_tot = Q_ND_tot * (1.0 + Q_ND[i]);
		}

		// Calculate the output values:
		P_cycle = P_ND_tot*P_ref;
		T_htf_cold = T_htf_hot - Q_ND_tot*q_dot_ref / (m_dot_htf*c_htf);
		eta = P_cycle / (Q_ND_tot*q_dot_ref);
		m_dot_demand = fmax(m_dot_htf_ND*m_dot_htf_ref, 0.00001);   // [kg/s]

		// Call the cooling tower model to update the condenser pressure
		q_reject = (1.0 - eta)*q_dot_ref*Q_ND_tot*1000.0;
		if( qq < 10 ) // MJW 10.31.2010
		{
			switch( ms_params.m_CT )  // Cooling technology type {1=evaporative cooling, 2=air cooling, 3=hybrid cooling}
			{
			case 1:
				CSP::evap_tower(ms_params.m_tech_type, P_cond_min, ms_params.m_n_pl_inc, dT_cw_ref, T_approach, (P_ref*1000.), m_eta_adj, T_db, T_wb, P_amb, q_reject, m_dot_makeup, W_cool_par, P_cond_guess, T_cond, f_hrsys);
				break;
			case 2:
				CSP::ACC(ms_params.m_tech_type, P_cond_min, ms_params.m_n_pl_inc, T_ITD_des, P_cond_ratio, (P_ref*1000.), m_eta_adj, T_db, P_amb, q_reject, m_dot_air, W_cool_par, P_cond_guess, T_cond, f_hrsys);
				break;
			case 3:
				CSP::HybridHR(/*fcall, */ms_params.m_tech_type, P_cond_min, ms_params.m_n_pl_inc, F_wc, F_wcmax, F_wcmin, T_ITD_des, T_approach, dT_cw_ref, P_cond_ratio, (P_ref*1000.), m_eta_adj, T_db, T_wb,
					P_amb, q_reject, m_dot_makeup, W_cool_parhac, W_cool_parhwc, W_cool_par, P_cond_guess, T_cond, f_hrsys);
				break;
			}
		}

		// Check to see if the calculated and demand values match
		// If they don't match, calculate the "ADJ" factor
		if( mode == 1 )
		{
			// err = (P_cycle - demand_var)/demand_var
			// ADJ = 1.+(demand_var-P_cycle)/(3.*demand_var)
			ADJ = (demand_var - P_cycle) / demand_var;		// MJW 10.31.2010: Adjustment factor
			err = fabs(ADJ);								// MJW 10.31.2010: Take the absolute value of the error..
			m_dot_htf_ND = m_dot_htf_ND + ADJ*0.75;		// MJW 10.31.2010: Iterate the mass flow rate. Take a step smaller than the calculated adjustment

		}
		else if( mode == 2 )
			err = 0.0;

		err = (P_cond_guess - P_cond) / P_cond;

		if( err > 0 )
			P_cond_low = P_cond;
		else
			P_cond_high = P_cond;

		if( P_cond_low > 0.0 && P_cond_high > 0.0 )
		{
			P_cond_guess = 0.5*P_cond_low + 0.5*P_cond_high;
			if( (P_cond_high - P_cond_low) / P_cond_high < 1.E-6 )
				err = 0.0;
		}

		P_cond = P_cond_guess;

		err = fabs(err);

		if( qq == 99 )
		{
			mc_csp_messages.add_message(C_csp_messages::WARNING, "Power cycle model did not converge after 100 iterations");
			P_cycle = 0.0;
			eta = -999.9;			// 4.15.15 twn: set this such that it hits feasibility checks up stream
			T_htf_cold = T_htf_hot_ref;
			m_dot_demand = m_dot_htf_ref;
			// TFF - should this be here too? m_bFirstCall = false;
			/*if(errorfound())*/ return;
		}
		// If this is not true, the cycle has not yet converged, and we should return
		//  to continue in the iterations
	}

	// Finally, convert the values back to their original units
	T_htf_cold = T_htf_cold - 273.15;			// [K]-->[C]
	T_htf_cold_ref = T_htf_cold_ref - 273.15;	// [K]->[C]
	T_htf_hot_ref = T_htf_hot_ref - 273.15;		// [K]->[C]
	m_dot_demand = m_dot_demand*3600.0;			// [kg/s]->[kg/hr]
	m_dot_htf = m_dot_htf*3600.0;				// [kg/s]->[kg/hr]
	m_dot_htf_ref = m_dot_htf_ref*3600.0;		// [kg/s]->[kg/hr]

}

double C_pc_Rankine_indirect_224::Interpolate(int YT, int XT, double X)
{
	double ind;//, temp(200);
	int XI = 0, YI = 0, lbi = 0, ubi = 0;
	//This function interpolates the data of one data list based on the value provided to a corresponding list of the same length.
	//YT: The name of the dependent Y variable being interpolated
	//XT: The name of the independent X variable which the Y variable is a function of
	//X: The value of the X variable
	//Y: is returned

	switch( XT )
	{
	case 1: XI = 1; break;    // A
	case 2: XI = 4; break;    // B
	case 3: XI = 7; break;    // C
	case 12: XI = 13; break;  // AB
	case 13: XI = 10; break;  // AC
	case 23: XI = 16; break;  // BC
	}

	switch( YT )
	{
	case 11:  YI = 2; break;      // PA
	case 12:  YI = 5; break;      // PB
	case 13:  YI = 8; break;      // PC
	case 112: YI = 14; break;    // PAB
	case 113: YI = 11; break;    // PAC
	case 123: YI = 17; break;    // PBC
	case 21:  YI = 3; break;      // QA
	case 22:  YI = 6; break;      // QB
	case 23:  YI = 9; break;      // QC
	case 212: YI = 15; break;    // QAB
	case 213: YI = 12; break;    // QAC
	case 223: YI = 18; break;    // QBC
	}

	//Set the data to be interpolated
	//datx(1:n)=db(XI,1:n)
	//daty(1:n)=db(YI,1:n)

	if( (XI == 0) || (YI == 0) ) return 0.0;

	XI--; YI--; // C++ arrays start index at 0 instead of 1, like Fortran arrays

	//Use brute force interpolation.. it is faster in this case than bisection or hunting methods used in the user-specified HTF case

	int iLastIndex = m_db.ncols() - 1;
	for( size_t i = 0; i < m_db.ncols(); i++ )
	{
		// if we got to the last one, then set bounds and end loop
		if( i == iLastIndex )
		{
			lbi = iLastIndex;
			ubi = iLastIndex;
			break;
		}

		// if the x variable is outside the table range, set the bounds and get out
		if( i == 0 ) {
			if( m_db.at(XI, 1) > m_db.at(XI, 0) )
			{ // The table is in ascending order
				if( X <= m_db.at(XI, 0) )
				{
					lbi = 0; ubi = 0; break;
				}
				if( X >= m_db.at(XI, iLastIndex) )
				{
					lbi = iLastIndex; ubi = iLastIndex; break;
				}
			}
			else
			{// the table is in descending order
				if( X >= m_db.at(XI, 0) )
				{
					lbi = 0; ubi = 0; break;
				}
				if( X <= m_db.at(XI, iLastIndex) )
				{
					lbi = iLastIndex; ubi = iLastIndex; break;
				}
			}
		}

		// if i = iLastIndex, the code above will catch it and break out of the loop before getting here.
		// so the reference [i+1], where i = iLastIndex, will never happen
		if( ((X >= m_db.at(XI, i)) && (X < m_db.at(XI, i + 1))) || ((X <= m_db.at(XI, i)) && (X > m_db.at(XI, i + 1))) )
		{
			lbi = i;
			ubi = i + 1;
			break;
		}
	}

	if( m_db.at(XI, ubi) == m_db.at(XI, lbi) )
		ind = 0.0;
	else
		ind = (X - m_db.at(XI, lbi)) / (m_db.at(XI, ubi) - m_db.at(XI, lbi));

	return m_db.at(YI, lbi) + ind * (m_db.at(YI, ubi) - m_db.at(YI, lbi));
} // Interpolate
