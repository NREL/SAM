#include "core.h"
#include "htf_props.h"



static var_info _cm_vtab_user_htf_comparison[] = {

	/*   VARTYPE   DATATYPE         NAME               LABEL                                          UNITS     META  GROUP REQUIRED_IF CONSTRAINTS         UI_HINTS*/
	{ SSC_INPUT,  SSC_NUMBER,  "HTF_code1",     "HTF fluid code: Fluid 1",                                 "-",       "",    "",      "*",     "",                ""  },
	{ SSC_INPUT,  SSC_MATRIX,  "fl_props1",     "User defined field fluid property data, Fluid 1",         "-",       "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "*", "", "" },
	
	{ SSC_INPUT,  SSC_NUMBER,  "HTF_code2",     "HTF fluid code: Fluid 2",                                 "-",       "",    "",      "*",     "",                ""  },
	{ SSC_INPUT,  SSC_MATRIX,  "fl_props2",     "User defined field fluid property data, Fluid 2",         "-",       "7 columns (T,Cp,dens,visc,kvisc,cond,h), at least 3 rows", "", "*", "", "" },

	{ SSC_OUTPUT, SSC_NUMBER,  "are_equal",     "1: Input tables are equal, 0: not equal",                 "-",       "",                                                         "", "*", "", "" },

	var_info_invalid };

class cm_user_htf_comparison : public compute_module
{
public:

	cm_user_htf_comparison()
	{
		add_var_info(_cm_vtab_user_htf_comparison);
	}

	void exec() throw(general_error)
	{
		// Get fluid codes
		int htf_code1 = (int)as_double("HTF_code1");
		int htf_code2 = (int)as_double("HTF_code2");

		int htf_user_defined_code = HTFProperties::User_defined;

		if( htf_code1 != htf_code2 )
		{
			assign("are_equal", 0.0);
			return;
		}

		// **********************************************
		// Following code assumes HTF code 1 = HTF code 2
		// **********************************************

		// HTF codes are equal, so are they user defined HTFs?
		if( htf_code1 != htf_user_defined_code)
		{
			assign("are_equal", 1.0);
			return;
		}

		HTFProperties htfProps1;			// Instance of HTFProperties class for receiver/HX htf

		size_t nrows = 0, ncols = 0;
		ssc_number_t *fl_props1 = as_matrix("fl_props1", &nrows, &ncols);
		if( fl_props1 != 0 && nrows > 2 && ncols == 7 )
		{
			util::matrix_t<ssc_number_t> mat;
			mat.assign(fl_props1, nrows, ncols);

			util::matrix_t<double> mat_double(nrows, ncols);
			for( int i = 0; i < nrows; i++ )
			{
				for( int j = 0; j < ncols; j++ )
				{
					mat_double(i, j) = (double)mat(i, j);
				}
			}
			if( !htfProps1.SetUserDefinedFluid(mat_double) )
			{
				// If table doesn't read, assumes HTFs are not equal (as they will fail in performance code)
				assign("are_equal", 0.0);
				return;
			}
		}
		else
		{
			// If tables isn't in correct format, assumes HTFs are not equal (as they will fail in performance code)
			assign("are_equal", 0.0);
			return;
		}

		HTFProperties htfProps2;			// Instance of HTFProperties class for receiver/HX htf

		nrows = 0; 
		ncols = 0;
		ssc_number_t *fl_props2 = as_matrix("fl_props2", &nrows, &ncols);
		if( fl_props2 != 0 && nrows > 2 && ncols == 7 )
		{
			util::matrix_t<ssc_number_t> mat;
			mat.assign(fl_props2, nrows, ncols);

			util::matrix_t<double> mat_double(nrows, ncols);
			for( int i = 0; i < nrows; i++ )
			{
				for( int j = 0; j < ncols; j++ )
				{
					mat_double(i, j) = (double)mat(i, j);
				}
			}
			if( !htfProps2.SetUserDefinedFluid(mat_double) )
			{
				// If table doesn't read, assumes HTFs are not equal (as they will fail in performance code)
				assign("are_equal", 0.0);
				return;
			}
		}
		else
		{
			// If tables isn't in correct format, assumes HTFs are not equal (as they will fail in performance code)
			assign("are_equal", 0.0);
			return;
		}

		if( !htfProps1.equals(&htfProps2) )
		{
			assign("are_equal", 0.0);
			return;
		}
		else
			assign("are_equal", 1.0);	
	}

};

DEFINE_MODULE_ENTRY(user_htf_comparison, "Evaluates equivalence of two user-defined HTF tables", 0)