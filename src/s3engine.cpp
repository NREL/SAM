#include <math.h>
#include <algorithm>

#include <wex/utils.h>


#include "s3engine.h"
#include "s3clipper.h"
#include "s3bsp.h"



#ifndef DTOR
#define DTOR 0.0174532925199433
#endif

namespace s3d {

point3d::point3d() { x = y = z = _x = _y = _z = 0.0f; }
point3d::point3d( double x_, double y_, double z_ )  : x(x_), y(y_), z(z_), _x(0), _y(0), _z(0) { }
rgba::rgba() : r(0), g(0), b(0), a(255)  { }
rgba::rgba( unsigned char _r, unsigned char _g, unsigned char _b, unsigned char _a ) : r(_r), g(_g), b(_b), a(_a) {  }
text3d::text3d() : size(-1) { }
text3d::text3d( double x, double y, double z, const std::string &t ) : pos(x,y,z), text(t), size(-1) { }
text3d::text3d( double x, double y, double z, const std::string &t, 
		rgba col, int sz, const std::string &ff )
		: pos(x,y,z), text(t), color(col), size(sz), face(ff) { }

polygon3d::polygon3d( int _id ) : id(_id), type(0), thick(1), as_line(false) { }
polygon3d::polygon3d( int _id, int _type, rgba _fill, rgba _border, int th, bool line ) : id(_id), type(_type), fill(_fill), border(_border), thick(th), as_line(line) {  }
polygon3d::polygon3d( int _id, int _type, rgba _fill, rgba _border, int th, bool line, const std::vector<point3d> &pts ) : id(_id), type(_type), fill(_fill), border(_border), thick(th), as_line(line), points(pts) {  }
polygon3d::polygon3d( const polygon3d &rhs )
{
	points = rhs.points;
	id = rhs.id;
	type = rhs.type;
	fill = rhs.fill;
	border = rhs.border;
	thick = rhs.thick;
	as_line = rhs.as_line;
}

transform::transform()
{
	reset();
}

transform::~transform()
{
	// nothing to do
}

void transform::reset()
{
	/* initialize variables */
	zorientation = -1; // -1 is z pointing out of the screen
					   // 1 is z pointing into the screen
	
	angleX = angleY = angleZ = 0.0;
	
	scale = 1;
	xaspect = 1;
	yaspect = 1;
	zaspect = 1;
	xO = yO = zO = 0.0;
	
	init();
}


void transform::operator() ( point3d &pt )
{
	x = pt.x;
	y = pt.y;
	z = pt.z;
	compute();
	pt._x = (double)X;
	pt._y = (double)Y;
	pt._z = (double)Z;
}


void transform::set_scale(double s)
{
	// do not allow for zero zoom
	if ( s < 1.0) s = 1.0;
	scale = s;
	init();
}

double transform::get_scale()
{
	return scale;
}

void transform::get_rotation( double *rx, double *rz )
{
	*rx = angleX/DTOR;
	*rz = angleZ/DTOR;
}

void transform::get_azal( double *azi, double *alt )
{
	*azi = angleZ/DTOR - 180;
	*alt = 90 - angleX/DTOR;
}


void transform::set_azal( double &azi, double &alt)
{
	angleZ = (azi+180) * DTOR;
	angleX = (90-alt) * DTOR;
	init();
}


void transform::get_offset( double *xoff, double *yoff, double *zoff )
{
	*xoff = -xO;
	*yoff = -yO;
	*zoff = -zO;
}


#define cosd( x ) cos( DTOR*(x) )
#define sind( x ) sin( DTOR*(x) )
#define tand( x ) tan( DTOR*(x) )
#define acosd( x ) (acos(x)/DTOR)


void transform::get_xy( double *x, double *y )
{	
	double azimuth, altitude;

	get_azal( &azimuth, &altitude );

	if ( azimuth >= 0 && azimuth <= 90 )
	{
		*x = cosd(altitude)*sind(azimuth);
		*y = cosd(altitude)*cosd(azimuth);
	}
	else if ( azimuth > 90 && azimuth <= 180 )
	{
		*x = cosd(altitude)*sind(180-azimuth);
		*y = -cosd(altitude)*cosd(180-azimuth);
	}
	else if ( azimuth > 180 && azimuth <= 270 )
	{
		*x = -cosd(altitude)*sind(azimuth-180);
		*y = -cosd(altitude)*cosd(azimuth-180);
	}
	else
	{
		*x = -cosd(altitude)*sind(360-azimuth);
		*y = cosd(altitude)*cosd(360-azimuth);
	}
}

void transform::get_xyz( double *x, double *y, double *z )
{	
	double azimuth, altitude;

	get_azal( &azimuth, &altitude );

	if ( azimuth >= 0 && azimuth <= 90 )
	{
		*x = cosd(altitude)*sind(azimuth);
		*y = cosd(altitude)*cosd(azimuth);
	}
	else if ( azimuth > 90 && azimuth <= 180 )
	{
		*x = cosd(altitude)*sind(180-azimuth);
		*y = -cosd(altitude)*cosd(180-azimuth);
	}
	else if ( azimuth > 180 && azimuth <= 270 )
	{
		*x = -cosd(altitude)*sind(azimuth-180);
		*y = -cosd(altitude)*cosd(azimuth-180);
	}
	else
	{
		*x = -cosd(altitude)*sind(360-azimuth);
		*y = cosd(altitude)*cosd(360-azimuth);
	}
	*z = sind(altitude);

}

void transform::get_view_normal( double vec[3] )
{	
	double azimuth, altitude;

	get_azal( &azimuth, &altitude );

	if ( azimuth >= 0 && azimuth <= 90 )
	{
		vec[0] = cosd(altitude)*sind(azimuth);
		vec[1] = cosd(altitude)*cosd(azimuth);
	}
	else if ( azimuth > 90 && azimuth <= 180 )
	{
		vec[0] = cosd(altitude)*sind(180-azimuth);
		vec[1] = -cosd(altitude)*cosd(180-azimuth);
	}
	else if ( azimuth > 180 && azimuth <= 270 )
	{
		vec[0] = -cosd(altitude)*sind(azimuth-180);
		vec[1] = -cosd(altitude)*cosd(azimuth-180);
	}
	else
	{
		vec[0] = -cosd(altitude)*sind(360-azimuth);
		vec[1] = cosd(altitude)*cosd(360-azimuth);
	}
	
	vec[2] = sind(altitude);
		
	//normalize
	double magnitude = sqrt(vec[0]*vec[0] + vec[1]*vec[1] + vec[2]*vec[2]);
	vec[0] = vec[0] / magnitude;
	vec[1] = vec[1] / magnitude;
	vec[2] = vec[2] / magnitude;
}


void transform::set_zorient(int zorient)
{
	if (zorient <= 0)
		zorientation = -1;
	else
		zorientation = 1;
	
	init();	
}

void transform::aspect(double xasp, double yasp, double zasp)
{
	xaspect = xasp;
	yaspect = yasp;
	zaspect = zasp;
	init();
}

void transform::rotate_x(double ax)
{
	angleX = ax*DTOR;
	init();
}

void transform::rotate_z(double az)
{
	angleZ = az*DTOR;
	init();
}

void transform::rotate_xz(double ax, double az)
{
	angleX = ax*DTOR;
	angleZ = az*DTOR;
	init();
}
void transform::change_xz(double dx, double dz)
{
	angleX += dx*DTOR;
	angleZ += dz*DTOR;
	init();
}

void transform::rotate_azal( double azi, double alt )
{
	rotate_xz( 90-alt, azi+180 );
}

void transform::offset(double xoff, double yoff, double zoff)
{
	xO = -xoff;
	yO = -yoff;
	zO = -zoff;
	init();
}

	
//	// translation
//	matrix[0][0] = 1; 	matrix[0][1] = 0; 		matrix[0][2] = 0;		matrix[0][3] = xO;
//	matrix[1][0] = 0;	matrix[1][1] = 1;		matrix[1][2] = 0;		matrix[1][3] = yO; 
//	matrix[2][0] = 0;	matrix[2][1] = 0;		matrix[2][2] = zOrientation; 	matrix[2][3] = zOrientation*zO;
//	matrix[3][0] = 0;	matrix[3][1] = 0;		matrix[3][2] = 0; 		matrix[3][3] = 1;
//	
//	// scaling		
//	mS[0][0] =  s*xasp	mS[0][1] =  0;			mS[0][2] =  0;				mS[0][3] = 0;
//	mS[1][0] =  0;		mS[1][1] =  s*yaspect	mS[1][2] =  0;				mS[1][3] = 0;
//	mS[2][0] =  0;		mS[2][1] =  0;			mS[2][2] =  s*zAspectRatio;	mS[2][3] = 0;
//	mS[3][0] =  0;		mS[3][1] =  0;			mS[3][2] =  0;				mS[3][3] = 1;
//	matProd(matrix, mS, matrix);
//
//	// z rotation
//	mZ[0][0] =  cosZ;	mZ[0][1] =  sinZ;	mZ[0][2] =  0;			mZ[0][3] =  0;
//	mZ[1][0] = -sinZ;	mZ[1][1] =  cosZ;	mZ[1][2] =  0;			mZ[1][3] =  0; 
//	mZ[2][0] =  0;	    mZ[2][1] =  0;		mZ[2][2] =  1;			mZ[2][3] =  0;
//	mZ[3][0] =  0;		mZ[3][1] =  0;		mZ[3][2] =  0;			mZ[3][3] =  1;
//	
//	// y rotation
//	mY[0][0] =  cosY;   mY[0][1] =  0; 		mY[0][2] =  -sinY;		mY[0][3] =  0;
//	mY[1][0] =  0;		mY[1][1] =  1;		mY[1][2] =  0;			mY[1][3] =  0; 
//	mY[2][0] =  sinY;	mY[2][1] =  0;		mY[2][2] =  cosY;		mY[2][3] =  0;
//	mY[3][0] =  0;		mY[3][1] =  0;		mY[3][2] =  0;			mY[3][3] =  1;
//	
//	// x rotation
//	mX[0][0] =  1;   	mX[0][1] =  0;		mX[0][2] =  0;			mX[0][3] =  0;
//	mX[1][0] =  0;		mX[1][1] =  cosX;	mX[1][2] =  sinX;		mX[1][3] =  0; 
//	mX[2][0] =  0;		mX[2][1] =  -sinX;	mX[2][2] =  cosX;		mX[2][3] =  0;
//	mX[3][0] =  0;		mX[3][1] =  0;		mX[3][2] =  0;			mX[3][3] =  1;
//
//	m_XYZ[0][3] +=  xs;  // Screen offsets
//	m_XYZ[1][3] +=  ys;

void transform::init()
{
	// calculate new cos and sins 
	cosX = (double)cos(angleX); 
	sinX = (double)sin(angleX);
	cosY = (double)cos(angleY); 
	sinY = (double)sin(angleY);
	cosZ = (double)cos(angleZ);
	sinZ = (double)sin(angleZ);

	double s = scale;

	// translation & scale
	m[0][0] = s*xaspect; 	m[0][1] = 0; 	m[0][2] = 0;				m[0][3] = s*xaspect*xO;
	m[1][0] = 0;	m[1][1] = s*yaspect;	m[1][2] = 0;				m[1][3] = s*yaspect*yO; 
	m[2][0] = 0;	m[2][1] = 0;	m[2][2] = s*zorientation*zaspect; 	m[2][3] = s*zaspect*zorientation*zO;
	m[3][0] = 0;	m[3][1] = 0;	m[3][2] = 0; 				m[3][3] = 1;
	
	// z rotation
	mZ[0][0] =  cosZ;	mZ[0][1] =  -sinZ;	mZ[0][2] =  0;			mZ[0][3] =  0;
	mZ[1][0] = sinZ;	mZ[1][1] =  cosZ;	mZ[1][2] =  0;			mZ[1][3] =  0; 
	mZ[2][0] =  0;	    mZ[2][1] =  0;		mZ[2][2] =  1;			mZ[2][3] =  0;
	mZ[3][0] =  0;		mZ[3][1] =  0;		mZ[3][2] =  0;			mZ[3][3] =  1;
	
	// y rotation
	mY[0][0] =  cosY;   mY[0][1] =  0; 		mY[0][2] =  sinY;		mY[0][3] =  0;
	mY[1][0] =  0;		mY[1][1] =  1;		mY[1][2] =  0;			mY[1][3] =  0; 
	mY[2][0] =  -sinY;	mY[2][1] =  0;		mY[2][2] =  cosY;		mY[2][3] =  0;
	mY[3][0] =  0;		mY[3][1] =  0;		mY[3][2] =  0;			mY[3][3] =  1;
	
	// x rotation
	mX[0][0] =  1;   	mX[0][1] =  0;		mX[0][2] =  0;			mX[0][3] =  0;
	mX[1][0] =  0;		mX[1][1] =  cosX;	mX[1][2] =  -sinX;		mX[1][3] =  0; 
	mX[2][0] =  0;		mX[2][1] =  sinX;	mX[2][2] =  cosX;		mX[2][3] =  0;
	mX[3][0] =  0;		mX[3][1] =  0;		mX[3][2] =  0;			mX[3][3] =  1;
	
	matprod4(m_Z,   mZ, m);
	matprod4(m_YZ,  mY, m_Z);
	matprod4(m_XYZ, mX, m_YZ);
	
	a00 =  m_XYZ[0][0];
	a01 =  m_XYZ[0][1];
	a02 =  m_XYZ[0][2];
	a03 =  m_XYZ[0][3];
	
	a10 =  m_XYZ[1][0];
	a11 =  m_XYZ[1][1];
	a12 =  m_XYZ[1][2];
	a13 =  m_XYZ[1][3]; 
	
	a20 =  m_XYZ[2][0];
	a21 =  m_XYZ[2][1];
	a22 =  m_XYZ[2][2];
	a23 =  m_XYZ[2][3];
		
}

void transform::compute()
{
	X = a00*x + a01*y + a02*z + a03;
	Y = a10*x + a11*y + a12*z + a13;
	Z = a20*x + a21*y + a22*z + a23;
	
//	X = scale*X;
//	Y = scale*Y;
}

void transform::matprod4(double z[4][4], double u[4][4], double v[4][4])
{
	int i, j, k;
	for (i=0; i<4; i++) 
		for (j=0; j<4; j++)
		{
			z[i][j]=0.0f;
			for (k=0; k<4; k++) 
				z[i][j]+=u[i][k]*v[k][j];
		}
}

scene::scene()
{
	m_fillColor = rgba(  18, 92, 14, 80 );
	m_lineColor = rgba(  0, 0, 0, 255 );
	m_polyType = OBSTRUCTION;
	m_view_x = 0;
	m_view_y = 0;
	m_view_z = -1e99;
}

scene::scene( const scene & rhs )
{
	copy( rhs );
}

scene::~scene()
{
	clear();
}

void scene::copy( const scene &rhs )
{
	clear();
	for( size_t i=0;i<rhs.m_polygons.size();i++ )
	{
		polygon3d *p = new polygon3d;
		*p = *rhs.m_polygons[i];
		m_polygons.push_back( p );
	}

	for( size_t i=0;i<rhs.m_labels.size();i++ )
	{
		text3d *t = new text3d;
		*t = *rhs.m_labels[i];
		m_labels.push_back( t );
	}

	// additional attributes to be copied
	m_fillColor = rhs.m_fillColor;
	m_lineColor = rhs.m_lineColor;
	m_polyType = rhs.m_polyType;
	m_view_x = rhs.m_view_x;
	m_view_y = rhs.m_view_y;
	m_view_z = rhs.m_view_z;
}

scene &scene::operator=( const scene &rhs )
{
	copy( rhs );
	return *this;
}


void scene::label( double x, double y, double z, const std::string &text, 
		rgba col, int size, const std::string &face )
{
	m_labels.push_back( new text3d( x, y, z, text, col, size, face ) );
}

void scene::fill( rgba c )
{
	m_fillColor = c;
}

void scene::outline( rgba o )
{
	m_lineColor = o;
}

void scene::reset()
{
	m_fillColor = rgba(  18, 92, 14, 80 );
	m_lineColor = rgba(  0, 0, 0, 255 );
	m_polyType = OBSTRUCTION;
	m_curPoints.clear();
}

void scene::type( int t )
{
	m_polyType = t;
}

void scene::colors( rgba fill, rgba line )
{
	m_fillColor = fill;
	m_lineColor = line;
}

void scene::point( double x, double y, double z )
{
	m_curPoints.push_back( point3d(x,y,z) );
}

void scene::line( int id, int thick )
{
	if ( m_curPoints.size() < 2 ) return;
	m_polygons.push_back( new polygon3d( id, 0, m_fillColor, m_lineColor, thick, true, m_curPoints ) );
	m_curPoints.clear();
}

void scene::poly( int id )
{
	if ( m_curPoints.size() < 3 ) return;
	m_polygons.push_back( new polygon3d( id, m_polyType, m_fillColor, m_lineColor, 1, false, m_curPoints ) );
	m_curPoints.clear();
}


void scene::poly( int _id, int _type, rgba _fill, rgba _border, int thick, bool line, const std::vector<point3d> &pts )
{
	m_polygons.push_back( new polygon3d( _id, _type, _fill, _border, thick, line, pts ) );
}

//void scene::box( int id, double x, double y, double angle_xy, double xdim, double ydim, double zstart, double height, 
//				unsigned int faces, int rotateCenter )
				
void scene::box( int id, double x, double y, double z, double rot, double xdim, double ydim, double zdim, 
		unsigned int faces )
{
	// NO ends: {0,1,1,1,1,0};
	// rotate
	double xr[4], yr[4];
	double zmin = z;
	double zmax = z + zdim;
	
	s3d::get_rotated_box_points( x, y, xdim, ydim, rot, xr, yr);
	
	// right face
	if ( faces & RIGHT )
	{
		point(xr[0],yr[0],zmax);
		point(xr[1],yr[1],zmax);
		point(xr[1],yr[1],zmin);
		point(xr[0],yr[0],zmin);
		poly( id );
	}
	// top face
	if ( faces & TOP )
	{
		point(xr[2],yr[2],zmin);
		point(xr[1],yr[1],zmin);
		point(xr[1],yr[1],zmax);
		point(xr[2],yr[2],zmax);
		poly ( id ); 
	}
	// bottom face
	if ( faces & BOTTOM )
	{
		point(xr[0],yr[0],zmax);
		point(xr[0],yr[0],zmin);
		point(xr[3],yr[3],zmin);
		point(xr[3],yr[3],zmax);
		poly ( id );
	}
	// back face
	if ( faces & BACK )
	{
		point(xr[0],yr[0],zmin);
		point(xr[1],yr[1],zmin);
		point(xr[2],yr[2],zmin);
		point(xr[3],yr[3],zmin);
		poly (id );
	}
	// front face
	if ( faces & FRONT )
	{
		point(xr[0],yr[0],zmax);
		point(xr[3],yr[3],zmax);
		point(xr[2],yr[2],zmax);
		point(xr[1],yr[1],zmax);
		poly (id );
	}
	// left face
	if ( faces & LEFT )
	{
		point(xr[2],yr[2],zmax);
		point(xr[3],yr[3],zmax);
		point(xr[3],yr[3],zmin);
		point(xr[2],yr[2],zmin);
		poly( id );
	}
}

void scene::plane( int id, double x[4], double y[4], double z[4] )
{
	
	// extract and cast from double to double
	double xr[4], yr[4], zr[4];
	xr[0] = static_cast<double>(x[0]); yr[0] = static_cast<double>(y[0]); zr[0] = static_cast<double>(z[0]);
	xr[1] = static_cast<double>(x[1]); yr[1] = static_cast<double>(y[1]); zr[1] = static_cast<double>(z[1]);
	xr[2] = static_cast<double>(x[2]); yr[2] = static_cast<double>(y[2]); zr[2] = static_cast<double>(z[2]);
	xr[3] = static_cast<double>(x[3]); yr[3] = static_cast<double>(y[3]); zr[3] = static_cast<double>(z[3]);

	// Plane
	point(xr[3],yr[3],zr[3]);
	point(xr[2],yr[2],zr[2]);
	point(xr[1],yr[1],zr[1]);
	point(xr[0],yr[0],zr[0]);
	poly( id );
}

void scene::conical( int id, double x, double y, double zstart, double height, double r1, double r2,
					 double angle_start, double angle_end, int npoly )
{
	if ( (r1 == 0.0 && r2 == 0.0) 
		|| r1 < 0.0
		|| r2 < 0.0
		|| npoly == 0
		|| height == 0.0 ) return;
		
	std::vector<point3d> end1;
	std::vector<point3d> end2;

	double range = angle_end - angle_start;
	if (range <= 0) range += 360.0;
	double angle = angle_start, step = range/npoly;
	if ( step < 1 ) step = 10;
	if ( step > 45 ) step = 45;
	
	double x_last1 = x + r1;
	double y_last1 = y ;
	double x_last2 = x + r2;
	double y_last2 = y ;
	
	
	end1.push_back( s3d::point3d( x_last1, y_last1, zstart+0 ) );
	end2.push_back( s3d::point3d( x_last2, y_last2, zstart+height ) );

	while( angle <= angle_end )
	{
		double cosA = (double)cos( angle*3.14159/180 );
		double sinA = (double)sin( angle*3.14159/180 );

		double x1 = x + r1*cosA;
		double y1 = y + r1*sinA;
		double x2 = x + r2*cosA;
		double y2 = y + r2*sinA;
		

		std::vector<point3d> pplist;

		if ( r2 > 0.0 )
			pplist.push_back( s3d::point3d( x_last2, y_last2, zstart + height ) );
				
		pplist.push_back( s3d::point3d( x2, y2, zstart + height ) );
		
		if ( r1 > 0.0 )
			pplist.push_back( s3d::point3d( x1, y1, zstart ) );
		
		pplist.push_back( s3d::point3d( x_last1, y_last1, zstart ) );

		poly( id, m_polyType, m_fillColor, m_lineColor, 1, false, pplist );
		
		end1.push_back( s3d::point3d( x1, y1, zstart+0 ) );
		end2.push_back( s3d::point3d( x + r2*cosA, y - r2*sinA, zstart+height ) );
				
		x_last1 = x1;
		y_last1 = y1;
		x_last2 = x2;
		y_last2 = y2;

		angle += step;
	}
	
	if ( r1 > 0.0 ) poly( id, m_polyType, m_fillColor, m_lineColor, 1, false, end1 );
	if ( r2 > 0.0 ) poly( id, m_polyType, m_fillColor, m_lineColor, 1, false, end2 );
}

void scene::cylinder( int id, double x, double y, double zstart, double height, double r,
					 double angle_start, double angle_end, double angle_xy, int npoly )
{
// function developed in support of VHedgeObject	
	if ( (r == 0.0) 
		|| r < 0.0
		|| npoly == 0
		|| height == 0.0 ) return;
	
	std::vector<point3d> end1;
	std::vector<point3d> end2;

	double range = angle_end - angle_start;
	double angle = angle_start, step = range/npoly;
	double cosAxy = cos(angle_xy*3.14159/180);
	double sinAxy = sin(angle_xy*3.14159/180);

	double x_last1 = x + r*sinAxy;
	double y_last1 = y - r*cosAxy;
	double x_last2 = x + r*sinAxy;
	double y_last2 = y - r*cosAxy;

	end1.push_back( s3d::point3d( x_last1, y_last1, zstart+0 ) );
	end2.push_back( s3d::point3d( x_last2, y_last2, zstart+height ) );

	std::vector<double> x_store(npoly+1), y_store(npoly+1);

	for (int i = 0; i < npoly+1; i++)
	{

		double cosA = (double)cos( angle*3.14159/180 );
		double sinA = (double)sin( angle*3.14159/180 );

		double x1 = x + r*sinA;
		double y1 = y + r*cosA;
		double x2 = x + r*sinA;
		double y2 = y + r*cosA;

		int j = npoly - i;
		x_store[j] = x2;
		y_store[j] = y2;

		if (step > 0)
		{
			point( x_last1, y_last1, zstart );
			point( x1, y1, zstart );
			point( x2, y2, zstart+height );
			point( x_last2, y_last2, zstart+height );
			poly( id );
			end2.push_back( s3d::point3d( x2, y2, zstart+height ) );
		}
		else
		{
			point( x_last2, y_last2, zstart+height );
			point( x2, y2, zstart+height );
			point( x1, y1, zstart );
			point( x_last1, y_last1, zstart );
			poly( id );
			end1.push_back( s3d::point3d( x2, y2, zstart ) );
		}
		
		x_last1 = x1;
		y_last1 = y1;
		x_last2 = x2;
		y_last2 = y2;

		angle += step;
	}

	// flip order for correct face shading
	if (step > 0) 
	{
		for (int i = 0; i < npoly+1; i++)
		{
			end1.push_back( s3d::point3d( x_store[i], y_store[i], zstart) );
		}
	}
	else 
	{
		for (int i = 0; i < npoly+1; i++)
		{
			end2.push_back( s3d::point3d( x_store[i], y_store[i], zstart+height) );
		}
	}

	// draw top and bottom faces
	poly( id, m_polyType, m_fillColor, m_lineColor, 1, false, end1 );
	poly( id, m_polyType, m_fillColor, m_lineColor, 1, false, end2 );
}

void scene::roof( int id, double xc, double yc, double zmin, double width, double length, 
				 double height, double pitch1, double pitch2, double angle_xy )
{
	double zmax = zmin + height;
	double xr[4], yr[4], xu[4], yu[4];
	get_rotated_box_points(xc,yc,width,length,0.0,xu,yu);
	get_rotated_box_points(xc,yc,width,length,angle_xy,xr,yr);

	// compute xy pitch points
	double xp[2], yp[2];
	xp[0] = 0.5*(xu[0] + xu[2]);
	xp[1] = xp[0];
	yp[0] = yu[0] + height/(tan(pitch1*DTOR) );
	yp[1] = yu[1] - height/(tan(pitch2*DTOR) );
	rotate2dxz(xc,yc,xp,yp,angle_xy,2);

	// bottom face
	point(xr[0],yr[0],zmin);
	point(xr[1],yr[1],zmin);
	point(xr[2],yr[2],zmin);
	point(xr[3],yr[3],zmin);
	poly (id );

	// Triangle Face 1
	point(xr[3], yr[3],zmin);
	point(xp[0], yp[0],zmax);
	point(xr[0], yr[0],zmin);
	poly (id );

	// Triangle Face 2
	point(xr[1], yr[1], zmin);
	point(xp[1], yp[1], zmax);
	point(xr[2], yr[2], zmin);
	poly (id );

	// Side Face 1
	point(xr[0], yr[0], zmin);
	point(xp[0], yp[0], zmax);
	point(xp[1], yp[1], zmax);
	point(xr[1], yr[1], zmin);
	poly (id);

	// Side Face 2
	point(xr[2], yr[2], zmin);
	point(xp[1], yp[1], zmax);
	point(xp[0], yp[0], zmax);
	point(xr[3], yr[3], zmin);
	poly(id);

}

void scene::clear()
{
	for ( std::vector<polygon3d*>::iterator it = m_culled_sorted.begin(); it != m_culled_sorted.end(); ++it )
		if ( std::find( m_polygons.begin(), m_polygons.end(), *it) == m_polygons.end() )
			delete *it;
	m_culled_sorted.clear();


	for ( std::vector<polygon3d*>::iterator it = m_polygons.begin();
		it != m_polygons.end();
		++it )
		delete *it;

	m_polygons.clear();

	
	for ( std::vector<text3d*>::iterator it = m_labels.begin();
		it != m_labels.end();
		++it )
		delete *it;

	m_labels.clear();
}

void scene::clear( int id )
{
	size_t i=0;
	while( i < m_polygons.size() )
	{
		if (m_polygons[i]->id == id)
		{
			delete m_polygons[i];
			m_polygons.erase( m_polygons.begin() + i );
		}
		else i++;
	}
	i=0;
	while( i < m_culled_sorted.size() )
	{
		if (m_culled_sorted[i]->id == id)
		{
			delete m_culled_sorted[i];
			m_culled_sorted.erase( m_culled_sorted.begin() + i );
		}
		else i++;
	}
}

void scene::basic_axes_with_ground( int axes_len )
{
	int size = 2*axes_len;
	reset();
	colors( rgba( 18, 92, 14, 80 ), rgba(255, 255, 255) );
	point(-size, -size, 0 );
	point(-size, size, 0 );
	point(size, size, 0 );
	point(size, -size, 0 );
	poly(-2);

	colors( rgba( 200, 0, 0 ), rgba(200, 0, 0 ) );
	point( 0, 0, 0 );
	point( axes_len, 0, 0 );
	line(-1, 2);

	colors( rgba( 0, 130, 0 ), rgba(0, 130, 0 ) );
	point( 0, 0, 0 );
	point( 0, axes_len, 0 );
	line(-1, 2);
	
	colors( rgba(0, 0, 200 ), rgba(0, 0, 200 ) );
	point( 0, 0, 0 );
	point( 0, 0, axes_len );
	line(-1, 2);
	
	label( axes_len+4, 0, 0, "X (east)" );
	label( 0, axes_len+4, 0, "Y (north)" );
	label( 0, 0, axes_len+6, "Z (sky)" );
}



static double average_z( const s3d::polygon3d *p ) 
{
	const std::vector<point3d> &points = p->points;
	double sum = 0;
	for ( size_t i=0;i<points.size(); i++ )
		sum += points[i]._z;

	return sum/points.size();
}

static bool polybefore( const s3d::polygon3d *p1, const s3d::polygon3d *p2 )
{
	// if IDs are negative, they always come first and are ordered based on smalled ID
	// (useful for ground and axes
	if (p1->id < 0 && p2->id < 0)
		return p1->id < p2->id;
	else if ( p1->id < 0 )
		return true;
	else if ( p2->id < 0 )
		return false;
	else
		return average_z(p1) > average_z(p2);
}

static bool is_backface( const s3d::polygon3d *p )
{	
	const std::vector<point3d> &points = p->points;

	double C = 0;
	size_t i, j;
	for ( i=0;i< points.size();i++ )
	{
		if ( i == points.size() - 1 ) j = 0;
		else j = i+1;

		C += ( points[i]._x - points[j]._x ) * ( points[i]._y + points[j]._y );
	}

	return (C > 0);
}

void scene::set_viewxyz( double &x, double &y, double &z)
{
	m_view_x = x;
	m_view_y = y;
	m_view_z = z;
}

void scene::get_viewxyz( double *x, double *y, double *z)
{
	*x = m_view_x;
	*y = m_view_y;
	*z = m_view_z;
}

void scene::build( transform &tr )
{
	size_t i;

	// transform all points
	for ( size_t i=0;i<m_polygons.size();i++ )
		for ( size_t j=0;j<m_polygons[i]->points.size();j++ )
			tr( m_polygons[i]->points[j] );

	for ( size_t i=0;i<m_labels.size();i++ )
		tr( m_labels[i]->pos );

	// cull backfaces into secondary list
	m_culled.clear();
	m_culled.reserve( m_polygons.size() );
	for ( i=0; i<m_polygons.size();i++ )
	{
		if ( m_polygons[i]->as_line || ! is_backface( m_polygons[i] ) )
			m_culled.push_back( m_polygons[i] );
	}
//double azi, alt;
//tr.get_azal(&azi, &alt);
//DBOUT("\n\nscene::build azimuth = " << azi << ", altitude = " << alt << "\n" );
	// sort by average z order
//	std::sort( m_culled.begin(), m_culled.end(), polybefore );

	std::vector<s3d::polygon3d*> background;


	// use BSP
	 // make sure all newly created "split" polygons are deleted
	for ( std::vector<polygon3d*>::iterator it = m_culled_sorted.begin(); it != m_culled_sorted.end(); ++it )
		if ( std::find( m_polygons.begin(), m_polygons.end(), *it) == m_polygons.end() )
			delete *it;
	m_culled_sorted.clear();

	for (i=0;i<m_culled.size();i++)
	{
		if ( m_culled[i]->as_line || ( m_culled[i]->id < 0 ))
			background.push_back(m_culled[i]);
		else
			m_culled_sorted.push_back(m_culled[i]);
	}

	if (m_culled_sorted.size() > 0)
	{

		// test presort and use middle node as root for more balanced tree
		//std::sort( m_culled_sorted.begin(), m_culled_sorted.end(), polybefore );


		//BSPTree bsp( m_culled_sorted , vec[0], vec[1], -1.0e6);
		BSPTree bsp;
		bsp.ReadPolyList( m_culled_sorted );
		m_culled_sorted.clear();
		
		bsp.BuildTree();



/*
		// From Sun's viewpoint
		double azi, alt;
		double scale = tr.get_scale();
		tr.get_azal(&azi, &alt);
		s3d::point3d sun( scale * sind(azi), scale* cosd(azi), scale * sind(alt));
		//s3d::point3d sun( 0,0,1e6);
		//s3d::point3d sun( x,y,z);
		tr(sun);
		CPoint viewpoint;
		viewpoint.x = sun._x;
		viewpoint.y = sun._y;
		viewpoint.z = 1e99*sun._z;
*/

// suggestion from Aron coordinate.pptx
//		double x,y,z;
//		tr.get_xy(&x, &y);
//		tr.get_xyz(&x, &y, &z); // unscaled normal

//		double scale = tr.get_scale();
//		double sf = 1e6 * scale;

		CPoint viewpoint;
//		viewpoint.x = sf*x;
//		viewpoint.y = sf*y;
//		viewpoint.z = -sf*z;

		viewpoint.x = m_view_x;
		viewpoint.y = m_view_y;
		viewpoint.z = m_view_z;




//		double r=1e6;
//		CPoint viewpoint;
//		viewpoint.x = -r*x;
//		viewpoint.y = -r*y;
//		viewpoint.z = -r*z;
//		viewpoint.x = 1.0e6;
//		viewpoint.y = 1.0e6;
//		viewpoint.z = 1.0e6;
//		viewpoint.x = 0;
//		viewpoint.y = 0;
//		viewpoint.z = 1.0e9;
//		viewport.z = 0;


		bsp.Traverse( viewpoint, m_culled_sorted);
	}

	std::sort( background.begin(), background.end(), polybefore );

	m_culled.clear();
	for (i=0;i<background.size();i++)
		m_culled.push_back(background[i]);

	for (i=0;i<m_culled_sorted.size();i++)
		m_culled.push_back(m_culled_sorted[i]);

	tr.get_view_normal( m_vn );
}

void scene::get_viewnormal( double *x, double *y, double *z )
{
	*x = m_vn[0];
	*y = m_vn[1];
	*z = m_vn[2];
}

const std::vector<text3d*> &scene::get_labels() const
{
	return m_labels;
}

const std::vector<polygon3d*> &scene::get_polygons() const
{
	return m_culled;
}


static void copy_poly(ClipperLib::Path &lhs, polygon3d &rhs)
{
	if ( rhs.points.size() == 0 ) return;
	lhs.reserve( rhs.points.size() );
	for ( size_t i=0;i<rhs.points.size(); i++ )
		lhs.push_back( ClipperLib::IntPoint( rhs.points[i]._x, rhs.points[i]._y ) );
}

static void copy_poly(polygon3d &lhs, ClipperLib::Path &rhs)
{
	if ( rhs.size() == 0 ) return;
	lhs.points.reserve( rhs.size() );
	for ( size_t i=0;i<rhs.size();i++ )
	{
		point3d pi( rhs[i].X, rhs[i].Y, 0 );
		pi._x = pi.x;
		pi._y = pi.y;
		lhs.points.push_back( pi );
	}
}

void scene::shade( shade_result &result )
{
//	result.shade_fraction = 0;
	result.shade_fraction = 1; // initialize to fully shaded when active area = 0
	result.total_active_area = 0.0;
	result.total_shade_area = 0.0;
	result.shadings.clear();

	for ( size_t i = 0; i < m_culled.size(); i++ )
	{
		polygon3d *p = m_culled[i];
		if ( !p->as_line && p->type == ACTIVE && p->points.size() > 2 )
		{
			ClipperLib::Path active;
			copy_poly( active, *p );
			
			double active_area = fabs(ClipperLib::Area( active ));
			if ( active_area == 0.0 ) continue;

			result.total_active_area += active_area;

			ClipperLib::Clipper cc;
/*
			DBOUT("\nClipperLib Preserve Collinear=" << cc.PreserveCollinear() << "\n");
			DBOUT("\nClipperLib Strictly Simple=" << cc.StrictlySimple() << "\n");
*/
			cc.AddPath(active, ClipperLib::ptSubject,true);

			size_t nobstruct=0;
			for ( size_t j=i+1; j<m_culled.size();j++ )				
			{
				polygon3d *obs = m_culled[j];
//				if ( !obs->as_line && obs->type == OBSTRUCTION && obs->points.size() > 2 )
				if ( !obs->as_line && obs->id != p->id && obs->points.size() > 2 )
				{
					ClipperLib::Path obstruct;
					copy_poly( obstruct, *obs );
					cc.AddPath(obstruct, ClipperLib::ptClip, true);
					nobstruct++;
				}
			}

			if ( nobstruct==0 ) continue;

			ClipperLib::Paths soln;
			cc.Execute(ClipperLib::ctIntersection, soln, ClipperLib::pftNonZero, ClipperLib::pftNonZero);

			for ( size_t k=0;k<soln.size();k++ )
			{
				double shade_area = fabs(ClipperLib::Area( soln[k] ));				
				if ( shade_area == 0.0 ) continue;


				if (shade_area > active_area)
				{
//					DBOUT("\nshade_area=" << shade_area << " > " << active_area << "=active_area  for k=" << k << "\n");
					shade_area = active_area;
				}


				shade_polygon sr;
				sr.shade_area = shade_area;
				sr.active_area = active_area;
				sr.shade_fraction = sr.shade_area/sr.active_area;
				sr.intersect.points.reserve( soln[k].size() );
				copy_poly( sr.intersect, soln[k] );

				result.total_shade_area += shade_area;
				result.shadings.push_back( sr );
			}
			
		}

	}

	if (result.total_active_area != 0.0)
	{
		if (result.total_shade_area > result.total_active_area) 
			result.total_shade_area = result.total_active_area;
		result.shade_fraction = result.total_shade_area / result.total_active_area;
	}
}



#define sign(x) ((x)>=0)

bool intri(double x1, double y1,
				 double x2, double y2,
				 double x3, double y3,
				 double xt, double yt)
{
	double a = (x1 - xt)*(y2 - yt) - (x2 - xt)*(y1 - yt);
    double b = (x2 - xt)*(y3 - yt) - (x3 - xt)*(y2 - yt);
    double c = (x3 - xt)*(y1 - yt) - (x1 - xt)*(y3 - yt);
    return (sign(a) == sign(b) && sign(b) == sign(c));
}

bool inquad(double x1, double y1,
				 double x2, double y2,
				 double x3, double y3,
				 double x4, double y4,
				 double xt, double yt)
{
	return intri(x1,y1,x2,y2,x3,y3,xt,yt)
		|| intri(x1,y1,x3,y3,x4,y4,xt,yt);
}


bool inpoly( double *x, double *y, size_t n,
			double xt, double yt )
{
	if ( n < 3 ) return false;

	for ( size_t i=2;i<n;i++ )
	{
		if ( intri( x[0], y[0],
			x[i-1], y[i-1],
			x[i], y[i],
			xt, yt ) )
			return true;
	}

	return false;
}
bool incirc( double xc, double yc, double r,
			 double xt, double yt )
{
	if (sqrt( pow(xt-xc,2) + pow(yt-yc,2) ) <= r) return true;
	
	return false;
}

void rotate2dxz( double xc, double yc, double x[], double y[], double angle_xy /*deg*/, int n)
{
	// this function will correctly rotate about either the x or z axis
	// for x rotation enter y and z coords, for z rotation enter x and y coords

	for (int i = 0; i < n; i++)
	{
		// compute relative to center
		x[i] -= xc;
		y[i] -= yc; 

		// necessary angles
		double cosA = cos(angle_xy*DTOR);
		double sinA = sin(angle_xy*DTOR);

		// rotate using 2D rotation matrix
		double x_new = x[i]*cosA - y[i]*sinA;
		double y_new = x[i]*sinA + y[i]*cosA;

		// Back to absolute
		x[i] = x_new + xc;
		y[i] = y_new + yc;
	}
}

void rotate2dy( double xc, double zc, double x[], double z[], double angle_xz /*deg*/, int n)
{
	// this function will correctly rotate about the y axis
	// enter the x and z coords

	for (int i = 0; i < n; i++)
	{
		// compute relative to center
		x[i] -= xc;
		z[i] -= zc; 

		// necessary angles
		double cosA = cos(angle_xz*DTOR);
		double sinA = sin(angle_xz*DTOR);

		// rotate using 2D rotation matrix
		double x_new = x[i]*cosA + z[i]*sinA;
		double z_new = -x[i]*sinA + z[i]*cosA;

		// Back to absolute
		x[i] = x_new + xc;
		z[i] = z_new + zc;
	}
}


void get_rotated_box_points( double x, double y,
							 double width, double height, 
							 double angle_xy,  /*deg*/
							 double xr[4], double yr[4])
{
	// currently set to work only with rotations about x,z axes.
	// rotates correctly only about the center, not ends

	// Right Bottom point
	xr[0] = x + width;
	yr[0] = y;

	// Right Top
	xr[1] = x + width;
	yr[1] = y + height;

	// Left Top
	xr[2] = x;
	yr[2] = y + height;

	// Left Bottom
	xr[3] = x;
	yr[3] = y;

	// rotate
	rotate2dxz( x, y, xr,yr, angle_xy /*deg*/,4);
}



#ifndef M_PI
#define M_PI 3.14159265358979323
#endif

#ifndef DTOR
#define DTOR 0.0174532925
#endif


static int julian(int yr,int month,int day)    /* Calculates julian day of year */
{
	int i=1,jday=0,k;

	if( yr%4 == 0 )                      /* For leap years */
		k = 1;
	else
		k = 0;

	while( i < month )
		{
		jday = jday + wxNDay[i-1];
		i++;
		}
	if( month > 2 )
		jday = jday + k + day;
	else
		jday = jday + day;
	return(jday);
}

void sun_pos(int year,int month,int day,int hour,double minute,double lat,double lng,double tz,
		double *sazi, double *szen )
{
/* This function is based on a paper by Michalsky published in Solar Energy
	Vol. 40, No. 3, pp. 227-235, 1988. It calculates solar position for the
	time and location passed to the function based on the Astronomical
	Almanac's Algorithm for the period 1950-2050. For data averaged over an
	interval, the appropriate time passed is the midpoint of the interval.
	(Example: For hourly data averaged from 10 to 11, the time passed to the
	function should be 10 hours and 30 minutes). The exception is when the time
	interval includes a sunrise or sunset. For these intervals, the appropriate
	time should be the midpoint of the portion of the interval when the sun is
	above the horizon. (Example: For hourly data averaged from 7 to 8 with a
	sunrise time of 7:30, the time passed to the function should be 7 hours and
	and 45 minutes).

	Revised 5/15/98. Replaced algorithm for solar azimuth with one by Iqbal
	so latitudes below the equator are correctly handled. Also put in checks
	to allow an elevation of 90 degrees without crashing the program and prevented
	elevation from exceeding 90 degrees after refraction correction.

	This function calls the function julian to get the julian day of year.

	List of Parameters Passed to Function:
	year   = year (e.g. 1986)
	month  = month of year (e.g. 1=Jan)
	day    = day of month
	hour   = hour of day, local standard time, (1-24, or 0-23)
	minute = minutes past the hour, local standard time
	lat    = latitude in degrees, north positive
	lng    = longitude in degrees, east positive
	tz     = time zone, west longitudes negative

	sunn[]  = array of elements to return sun parameters to calling function
	sunn[0] = azm = sun azimuth in radians, measured east from north, 0 to 2*pi
	sunn[1] = 0.5*pi - elv = sun zenith in radians, 0 to pi
	sunn[2] = elv = sun elevation in radians, -pi/2 to pi/2
	sunn[3] = dec = sun declination in radians
	sunn[4] = sunrise in local standard time (hrs), not corrected for refraction
	sunn[5] = sunset in local standard time (hrs), not corrected for refraction
	sunn[6] = Eo = eccentricity correction factor
	sunn[7] = tst = true solar time (hrs)               
	sunn[8] = hextra = extraterrestrial solar irradiance on horizontal at particular time (W/m2)  */

	int jday,delta,leap;                           /* Local variables */
	double zulu,jd,time,mnlong,mnanom,
			eclong,oblqec,num,den,ra,dec,gmst,lmst,ha,elv,azm,refrac,
			E,ws,sunrise,sunset,Eo,tst;
	double arg,zen;

	jday = julian(year,month,day);       /* Get julian day of year */
	zulu = hour + minute/60.0 - tz;      /* Convert local time to zulu time */
	if( zulu < 0.0 )                     /* Force time between 0-24 hrs */
		{                                 /* Adjust julian day if needed */
		zulu = zulu + 24.0;
		jday = jday - 1;
		}
	else if( zulu > 24.0 )
		{
		zulu = zulu - 24.0;
		jday = jday + 1;
		}
	delta = year - 1949;
	leap = delta/4;
	jd = 32916.5 + delta*365 + leap + jday + zulu/24.0;
	time = jd - 51545.0;     /* Time in days referenced from noon 1 Jan 2000 */

	mnlong = 280.46 + 0.9856474*time;
	mnlong = fmod((double)mnlong,360.0);         /* Finds doubleing point remainder */
	if( mnlong < 0.0 )
		mnlong = mnlong + 360.0;          /* Mean longitude between 0-360 deg */

	mnanom = 357.528 + 0.9856003*time;
	mnanom = fmod((double)mnanom,360.0);
	if( mnanom < 0.0 )
		mnanom = mnanom + 360.0;
	mnanom = mnanom*DTOR;             /* Mean anomaly between 0-2pi radians */

	eclong = mnlong + 1.915*sin(mnanom) + 0.020*sin(2.0*mnanom);
	eclong = fmod((double)eclong,360.0);
	if( eclong < 0.0 )
		eclong = eclong + 360.0;
	eclong = eclong*DTOR;       /* Ecliptic longitude between 0-2pi radians */

	oblqec = ( 23.439 - 0.0000004*time )*DTOR;   /* Obliquity of ecliptic in radians */
	num = cos(oblqec)*sin(eclong);
	den = cos(eclong);
	ra  = atan(num/den);                         /* Right ascension in radians */
	if( den < 0.0 )
		ra = ra + M_PI;
	else if( num < 0.0 )
		ra = ra + 2.0*M_PI;

	dec = asin( sin(oblqec)*sin(eclong) );       /* Declination in radians */

	gmst = 6.697375 + 0.0657098242*time + zulu;
	gmst = fmod((double)gmst,24.0);
	if( gmst < 0.0 )
		gmst = gmst + 24.0;         /* Greenwich mean sidereal time in hours */

	lmst = gmst + lng/15.0;
	lmst = fmod((double)lmst,24.0);
	if( lmst < 0.0 )
		lmst = lmst + 24.0;
	lmst = lmst*15.0*DTOR;         /* Local mean sidereal time in radians */

	ha = lmst - ra;
	if( ha < -M_PI )
		ha = ha + 2*M_PI;
	else if( ha > M_PI )
		ha = ha - 2*M_PI;             /* Hour angle in radians between -pi and pi */

	lat = lat*DTOR;                /* Change latitude to radians */

	arg = sin(dec)*sin(lat) + cos(dec)*cos(lat)*cos(ha);  /* For elevation in radians */
	if( arg > 1.0 )
		elv = M_PI/2.0;
	else if( arg < -1.0 )
		elv = -M_PI/2.0;
	else
		elv = asin(arg);

	if( cos(elv) == 0.0 )
		{
		azm = M_PI;         /* Assign azimuth = 180 deg if elv = 90 or -90 */
		}
	else
		{                 /* For solar azimuth in radians per Iqbal */
		arg = ((sin(elv)*sin(lat)-sin(dec))/(cos(elv)*cos(lat))); /* for azimuth */
		if( arg > 1.0 )
			azm = 0.0;              /* Azimuth(radians)*/
		else if( arg < -1.0 )
			azm = M_PI;
		else
			azm = acos(arg);

		if( ( ha <= 0.0 && ha >= -M_PI) || ha >= M_PI )
			azm = M_PI - azm;
		else
			azm = M_PI + azm;
		}

	elv = elv/DTOR;          /* Change to degrees for atmospheric correction */
	if( elv > -0.56 )
		refrac = 3.51561*( 0.1594 + 0.0196*elv + 0.00002*elv*elv )/( 1.0 + 0.505*elv + 0.0845*elv*elv );
	else
		refrac = 0.56;
	if( elv + refrac > 90.0 )
		elv = 90.0*DTOR;
	else
		elv = ( elv + refrac )*DTOR ; /* Atmospheric corrected elevation(radians) */

	E = ( mnlong - ra/DTOR )/15.0;       /* Equation of time in hours */
	if( E < - 0.33 )   /* Adjust for error occuring if mnlong and ra are in quadrants I and IV */
		E = E + 24.0;
	else if( E > 0.33 )
		E = E - 24.0;

	arg = -tan(lat)*tan(dec);
	if( arg >= 1.0 )
		ws = 0.0;                         /* No sunrise, continuous nights */
	else if( arg <= -1.0 )
		ws = M_PI;                          /* No sunset, continuous days */
	else
		ws = acos(arg);                   /* Sunrise hour angle in radians */

					/* Sunrise and sunset in local standard time */
	sunrise = 12.0 - (ws/DTOR)/15.0 - (lng/15.0 - tz) - E;
	sunset  = 12.0 + (ws/DTOR)/15.0 - (lng/15.0 - tz) - E;

	Eo = 1.00014 - 0.01671*cos(mnanom) - 0.00014*cos(2.0*mnanom);  /* Earth-sun distance (AU) */
	Eo = 1.0/(Eo*Eo);                    /* Eccentricity correction factor */

	tst = hour + minute/60.0 + (lng/15.0 - tz) + E;  /* True solar time (hr) */
	
	zen = 0.5*M_PI - elv;

	/*
	sunn[0] = azm;
	sunn[1] = zen;
	sunn[2] = elv;
	sunn[3] = dec;
	sunn[4] = sunrise;
	sunn[5] = sunset;
	sunn[6] = Eo;
	sunn[7] = tst;
	*/

	*sazi = azm/DTOR;
	*szen = zen/DTOR;
}


void sun_unit( double sazm, double szen, double sun[3] )
{	
	//Get unit vector in direction of sun
	double solalt = 90 - szen;
		
	if ( sazm >= 0 && sazm <= 90 )
	{
		sun[0] = cosd(solalt)*sind(sazm);
		sun[1] = cosd(solalt)*cosd(sazm);
	}
	else if ( sazm > 90 && sazm <= 180 )
	{
		sun[0] = cosd(solalt)*sind(180-sazm);
		sun[1] = -cosd(solalt)*cosd(180-sazm);
	}
	else if ( sazm > 180 && sazm <= 270 )
	{
		sun[0] = -cosd(solalt)*sind(sazm-180);
		sun[1] = -cosd(solalt)*cosd(sazm-180);
	}
	else
	{
		sun[0] = -cosd(solalt)*sind(360-sazm);
		sun[1] = cosd(solalt)*cosd(360-sazm);
	}
	
	sun[2] = sind(solalt);
		
	//normalize
	double magnitude = sqrt(sun[0]*sun[0] + sun[1]*sun[1] + sun[2]*sun[2]);
	sun[0] = sun[0] / magnitude;
	sun[1] = sun[1] / magnitude;
	sun[2] = sun[2] / magnitude;
}

}; // namespace s3d