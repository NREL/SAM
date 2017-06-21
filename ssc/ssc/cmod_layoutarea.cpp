#include "core.h"
//#include "lib_weatherfile.h"
#include "lib_util.h"
#include "Toolbox.h"


static var_info _cm_vtab_layoutarea[] = {
/*   VARTYPE           DATATYPE         NAME                         LABEL                                          UNITS     META        GROUP          REQUIRED_IF         CONSTRAINTS         UI_HINTS*/

	
	
	{ SSC_INPUT,        SSC_MATRIX,      "positions",                 "Positions within calculataed area",          "",       "",         "layoutarea",   "*",                "",                "" },        
	
	/* outputs */
	{ SSC_OUTPUT,       SSC_MATRIX,      "convex_hull",               "Convex hull bounding the region",            "",       "",         "layoutarea",   "*",                "",                "" },
	{ SSC_OUTPUT,       SSC_NUMBER,      "area",                      "Area inside the convex hull",                "",       "",         "layoutarea",   "*",                "",                "" },

	var_info_invalid };


#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

class cm_layoutarea : public compute_module
{
	/*struct Point
	{
		double x, y;
	};*/

public:
	
	cm_layoutarea()
	{
		add_var_info( _cm_vtab_layoutarea );
	}

	void exec( ) throw( general_error )
	{
		
		util::matrix_t<float> positions;
		//get the matrix of points
		get_matrix("positions", positions);
		//put into an array of points
		std::vector<Point> pos_pts;
		pos_pts.reserve( positions.nrows() );

		for(int i=0; i<(int)positions.nrows(); i++){
			pos_pts.push_back( Point () );
			pos_pts.back().x = positions.at(i, 0);
			pos_pts.back().y = positions.at(i, 1);
		}


		//Calculate the convex hull surrounding the heliostat positions
		std::vector<Point> hull;
		Toolbox::convex_hull(pos_pts, hull);

		//Calculate the area of the convex hull
		double area = Toolbox::area_polygon(hull);


		//return the results
		assign("area", area*0.000247105 );	//acres
		ssc_number_t *hull_t = allocate( "convex_hull", hull.size(), 2);
		for(int i=0; i<(int)hull.size(); i++){
			hull_t[i*2] = hull.at(i).x;
			hull_t[i*2 + 1] = hull.at(i).y;
		}
		

	}
};


DEFINE_MODULE_ENTRY( layoutarea, "Layout Area Calculation", 0 )
