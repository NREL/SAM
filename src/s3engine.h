/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef __s3dengine_h
#define __s3dengine_h

#include <vector>
#include <string>

#include <unordered_map>
using std::unordered_map;
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'



namespace s3d {

class point3d
{
public:
	point3d();
	point3d( double x_, double y_, double z_ );
	double x, y, z; // original points
	
	double magnitude( void ) const;
	void normalize( void );
	double dot( const point3d& Pnt ) const;
	point3d cross( const point3d& Pnt ) const;
	bool operator==( const point3d& Pnt ) const;
	point3d operator-( const point3d& Pnt ) const;

	double _x, _y, _z; // translated points
};

class rgba
{
public:
	rgba();
	rgba( unsigned char r, unsigned char g, unsigned char b, unsigned char a = 255 );
	unsigned char r, g, b, a;
};

class text3d
{
public:
	text3d();
	text3d( double x, double y, double z, const std::string &_text );
	text3d( double x, double y, double z, const std::string &_text, 
		rgba _color, int _size, const std::string &_face );

	point3d pos;
	std::string text;
	rgba color;
	int size;
	std::string face;

};

class polygon3d
{
public:
	polygon3d( int _id = 0);
	polygon3d( int _id, int _type, rgba _fill, rgba _border, int _thick, bool _line );
	polygon3d( int _id, int _type, rgba _fill, rgba _border, int _thick, bool _line, const std::vector<point3d> &_points, bool _ncul=false );
	polygon3d( const polygon3d &rhs );
		
	int id;
	int type;
	rgba fill;
	rgba border;
	int thick;
	bool as_line;
	std::vector<point3d> points;
	bool no_cull;
};


class transform
{
public:
	transform();
	virtual ~transform();
	void reset();
	
	void operator() ( point3d & );
	
	
	double get_scale();
	void get_rotation( double *rx, double *yy );
	void get_azal( double *azi, double *alt );
	void set_azal( double &azi, double &alt);
	void get_offset( double *xoff, double *yoff, double *zoff );
	void get_view_normal( double *vx, double *vy, double *vz );

	void get_xy( double *x, double *y );
	void get_xyz( double *x, double *y, double *z );

	void set_scale( double s );
	void set_zorient( int zor );
	
	void aspect( double xa, double ya, double za );
	
	// angles in degrees
	void rotate_x( double xa );
	void rotate_z( double za );
	void rotate_xz( double xa, double za );
	void change_xz( double dx, double dz );
	void rotate_azal( double azimuth, double altitude );
	void offset( double xoff, double yoff, double zoff );
	
	
protected:
	
	void init();
	void compute();	
	void matprod4(double z[4][4], double u[4][4], double v[4][4]);
	
	double angleX, angleY, angleZ;
	double cosZ, sinZ, cosX, sinX, cosY, sinY;
	double scale;
	double xaspect, yaspect, zaspect;
	int zorientation;
	
	double m[4][4];
	double mX[4][4];
	double mY[4][4];
	double mZ[4][4];
	
	double m_Z[4][4];
	double m_YZ[4][4];
	double m_XYZ[4][4];
	
	double xO, yO, zO;
	
	double a00, a01, a02, a03;  // coefficients of the transformation
	double a10, a11, a12, a13;
	double a20, a21, a22, a23;	
	
	double x, y, z;
	double X, Y, Z;
	
};

class BSPNode : public polygon3d
{
#ifdef _DEBUG
public:
#endif

	size_t Index;
	BSPNode *FrontNode, *BackNode;
	
	point3d Center;
	point3d Normal;
	double D;

	bool m_rendered;


	unsigned long _SplitPoly( BSPNode *Plane, std::vector<point3d> &SplitPnts, bool savepoints=true );
	void _ComputeCenter( void );
	void _ComputeNormal( void );
	void _ComputeD( void );

public:
	BSPNode( const polygon3d &rhs );
	~BSPNode();

	bool GetRendered() { return m_rendered;}
		
	point3d GetCenter( void )				{ return Center; }
	point3d GetNormal( void )				{ return Normal; }

	bool Intersects( BSPNode *Plane );
	BSPNode *Split( BSPNode *Plane );

	BSPNode *GetFront( void )			{ return FrontNode; }
	BSPNode *GetBack( void )			{ return BackNode; }

	void SetFront( BSPNode *Node )		{ FrontNode = Node; }
	void SetBack( BSPNode *Node)		{ BackNode = Node; }

	void Traverse( const point3d& CameraLoc, std::vector<s3d::polygon3d*>& polys );

	double GetMinZ();
};



class BSPTree
{
private:
	unordered_map<int, float> m_id_minz;
	std::vector<BSPNode*> m_nodes;
	std::vector<BSPNode*> m_listnodes; // for deletion
	BSPNode *m_root;

	BSPNode *_FindRoot( std::vector<BSPNode*>& List );
	BSPNode *_BuildBSPTree( std::vector<BSPNode*>& List );

public:
	BSPTree() {};
	BSPTree( std::vector<s3d::polygon3d*>& polys, double x_viewport, double y_viewport, double z_viewport);
	~BSPTree();

	void Traverse( point3d& CameraLoc, std::vector<s3d::polygon3d*>& polys );

	size_t NNodes() { return m_nodes.size(); }
	void Reset();
	void ReadPolyList(const std::vector<s3d::polygon3d*>& polys );

	void ReadPolyList( std::ifstream& Input );
	void ReadTree( std::ifstream& Input );
	void WriteTree( std::ofstream& Output );

	void BuildTree( void );
};

static const unsigned int RIGHT = 0x0001;
static const unsigned int TOP = 0x0002;
static const unsigned int BOTTOM = 0x0004;
static const unsigned int BACK = 0x0008;
static const unsigned int FRONT = 0x0010;
static const unsigned int LEFT = 0x0020;
static const unsigned int ALL_FACES = RIGHT|TOP|BOTTOM|BACK|FRONT|LEFT;
static const unsigned int SIDES = RIGHT|BACK|FRONT|LEFT;


bool is_backface( const s3d::polygon3d &p );
void polynormal( const s3d::polygon3d &p, double N[3] );
void polynormaltr( const s3d::polygon3d &p, double *x, double *y, double *z );
double polyareatr( const s3d::polygon3d &p );
bool zeroarea( const s3d::polygon3d &p );
double angle_between( double A[3], double B[3] );


class shade_result
{
public:
	shade_result() : id(-1), active_area(0.0), shade_area(0.0), shade_fraction(0.0), aoi(0.0), backmost(0) {  }
	int id;
	double active_area;
	double shade_area;
	double shade_fraction;
	double aoi; // angle of incidence with view normal, degrees
	std::vector<polygon3d> shadings;
	std::vector<polygon3d*> polygons;
	int backmost;
};

class scene
{
private:
	std::vector<polygon3d*> m_polygons;
	std::vector<text3d*> m_labels;
	
	BSPTree m_bsp;
	double m_viewNormal[3];
	std::vector<polygon3d*> m_sortedCulled, m_rendered;

	int m_polyType;
	bool m_noCull;
	rgba m_fillColor, m_lineColor;
	std::vector<point3d> m_curPoints;

	void cull_backfaces( );
	void sort_polys();
public:
	bool m_bspValid;
	scene();
	scene( const scene & rhs );
	~scene();

	void copy( const scene &rhs );
	scene &operator=( const scene &rhs );
		
	void basic_axes_with_ground( int axes_len = 100 );

	void label( double x, double y, double z, const std::string &text, 
		rgba col = rgba(0,0,0), int size=-1, const std::string &face = "" );

	// state based drawing routines
	enum { OBSTRUCTION=1, ACTIVE }; // polygon types

	void reset();
	void type( int m );
	void fill( rgba c );
	void outline( rgba o );
	void colors( rgba fill, rgba line );
	void point( double x, double y, double z );
	void nocull( bool b );
	void line( int id=0, int thick=1 );
	void poly( int id=0 );
	void conical( int id, double x, double y, double zstart, double height, double r1, double r2, 
				   int npoly=10, bool face_bottom = true, bool face_top = true );
	void cylinder( int id, double x, double y, double zstart, double height, double r,
				   double angle_start=0.0, double angle_end = 360.0, double angle_xy = 0.0, int npoly=18 );
		
	void box( int id, double x, double y, double z, double rot, double xdim, double ydim, double zdim, 
		unsigned int faces = ALL_FACES );
	void roof( int id, double x, double y, double z, double width, double length, 
				 double height, double pitch1, double pitch2, double angle_xy );

	// add a new polygon all custom properties
	void poly( int _id, int _type, rgba _fill, rgba _border, int thick, bool line, const std::vector<point3d> &pts );
	
	// clear everything
	void clear();

	// erase all polygons of a specific ID
	void clear( int id );
	
	// transform, cull, sort	
	void build( transform &tr );

	// compute shade after scene is built
	double shade( std::vector<shade_result> &results, 
		double *total_active = 0, double *total_shade = 0 );

	// get polygons and labels for rendering
	const std::vector<text3d*> &get_labels() const;
	const std::vector<polygon3d*> &get_polygons() const;
	const std::vector<polygon3d*> &get_rendered() const;
};


bool intri(double x1, double y1,
				 double x2, double y2,
				 double x3, double y3,
				 double xt, double yt);

bool inquad(double x1, double y1,
				 double x2, double y2,
				 double x3, double y3,
				 double x4, double y4,
				 double xt, double yt);

bool inpoly( double *x, double *y, size_t n,
			double xt, double yt );

bool incirc( double xc, double yc, double r, 
			 double xt, double yt );
			 


void rotate2dxz( double xc, double yc, double x[], double y[],  double angle_xy /*deg*/, int n);
void rotate2dy( double xc, double zc, double x[], double z[],  double angle_xz /*deg*/, int n);


void get_rotated_box_points( double x, double y,
							 double width, double height, 
							 double angle_xy,  /*deg*/
							 double xr[4], double yr[4]);


/* angles in degrees for these functions */
void sun_unit( double sazi, double szen, double sun[3] );
void sun_pos( int year,int month,int day,int hour,double minute,double lat,double lng,double tz, 
		double *sazi, double *szen );


}; // namespace s3d


#endif
