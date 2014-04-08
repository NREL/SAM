#ifndef __view2d_h
#define __view2d_h

#include <vector>
#include <wx/window.h>
#include <wx/stream.h>

#include "s3engine.h"

class VObject;

enum { NONDIM, LENGTH, TIME, MASS, TEMP, ANGLE };

class VProperty
{
public:
	explicit VProperty();
	explicit VProperty( VProperty *ref );
	explicit VProperty( double d, int dim = NONDIM );
	explicit VProperty( int i, int dim = NONDIM );
	explicit VProperty( bool b );
	explicit VProperty( const wxString &s );
	explicit VProperty( const wxColour &c );

	enum { INVALID, DOUBLE, BOOLEAN, INTEGER, COLOUR, STRING };

	int GetType();
	int GetDimension();

	void Set( double d );
	void Set( bool b );
	void Set( int i );
	void Set( const wxColour &c );
	void Set( const wxString &s );

	int GetInteger();
	bool GetBoolean();
	double GetDouble();
	wxColour GetColour();
	wxString GetString();

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

private:
	void Init();
	wxUint8 m_type;
	VProperty *m_pReference;

	double m_doubleVal;
	bool m_boolVal;
	int m_intVal;
	wxColour m_colour;
	wxString m_string;
	int m_dimension;
};

class VHandle
{
public:
	VHandle( VObject *o, int id, double x, double yz, 
		const wxCursor &curs = wxCURSOR_SIZING, const wxString &name = wxEmptyString );

	VObject *GetObject();
	void GetOriginalPos( double *x, double *yz );
	void GetCurrentPos( double *x, double *yz );
	void SetOriginalPos( double x, double yz );
	double GetX();
	double GetYZ();
	double GetDeltaX();
	double GetDeltaYZ();
	double GetDistance();

	wxString GetName();
	wxCursor GetCursor();
	int GetId();

	void MoveTo( double x, double yz );

private:
	double m_origX, m_origYZ;
	double m_x, m_yz;
	wxString m_name;
	int m_id;
	wxCursor m_cursor;
	VObject *m_object;
};


enum VPlaneType { PLANE_XY, PLANE_XZ };

class VRenderer2D
{
public:
	virtual void Poly( double *X, double *YZ, size_t n ) = 0;
	virtual void Rect( double x, double yz, double width, double height ) = 0;
	virtual void Circ( double x, double yz, double radius ) = 0;
};

class VObject
{
public:
	VObject( );
	virtual ~VObject();

	int GetId();

	virtual wxString GetTypeName() = 0;
	virtual VObject *Duplicate() = 0;
	virtual bool Copy( VObject *rhs );
	virtual void BuildModel( s3d::scene & ) = 0;
	virtual void SetupHandles( VPlaneType plane );
	virtual bool OnHandleMoved( VHandle *, VPlaneType );
	virtual void DrawOnPlane( VRenderer2D &dc, VPlaneType plane);
	virtual bool IsWithin( double x, double y, VPlaneType plane );

	VProperty &Property( const wxString &name );
	wxArrayString Properties();

	void DeleteHandles();
	std::vector<VHandle*> GetHandles();
		
	virtual void Write( wxOutputStream & );
	virtual bool Read( wxInputStream & );
	
	void Show( bool b ) { m_visible = b; }
	bool IsVisible() { return m_visible; }

protected:
	void AddProperty( const wxString &name, VProperty *prop );
	VHandle *AddHandle( int id, double x, double y, 
		const wxCursor &curs = wxCURSOR_SIZING, const wxString &name = wxEmptyString );
private:
	void DeleteProperties();
	int m_id;
	bool m_visible;
	struct propdata { wxString name, lowered; VProperty *prop; };
	std::vector<propdata> m_properties;
	std::vector<VHandle*> m_handles;
};


class VConicalTreeObject : public VObject
{
public:
	VConicalTreeObject();
	virtual wxString GetTypeName();
	virtual VObject *Duplicate();
	virtual void BuildModel( s3d::scene & );
	virtual void SetupHandles( VPlaneType plane );
	virtual bool OnHandleMoved( VHandle *, VPlaneType );	
	virtual void DrawOnPlane( VRenderer2D &dc, VPlaneType plane );	
	virtual bool IsWithin( double x, double y, VPlaneType plane );

	enum { HH_MOVE, HH_DIAM, HH_TOPDIAM, HH_TRUNK, HH_HEIGHT };

private:
	static const int m_nPoints = 9; // number of points to draw with
	void GetXZPoints( double x[m_nPoints], double z[m_nPoints] );
};


class VBoxObject : public VObject
{
public:
	VBoxObject();
	virtual wxString GetTypeName();
	virtual VObject *Duplicate();
	virtual void SetupHandles( VPlaneType plane );
	virtual bool OnHandleMoved( VHandle *, VPlaneType );
	virtual void BuildModel( s3d::scene & );
	virtual void DrawOnPlane( VRenderer2D &dc, VPlaneType plane );	
	virtual bool IsWithin( double x, double y, VPlaneType plane );

	enum { HH_MOVE, HH_TOP, HH_RIGHT, HH_ROTATE_XY, HH_LEFT, HH_BOTTOM };

private:

};


class VActiveSurfaceObject : public VObject
{
public:
	VActiveSurfaceObject();
	virtual wxString GetTypeName();
	virtual VObject *Duplicate();
	virtual void SetupHandles( VPlaneType plane );
	virtual bool OnHandleMoved( VHandle *, VPlaneType );
	virtual void BuildModel( s3d::scene & );
	virtual void DrawOnPlane( VRenderer2D &dc, VPlaneType plane );	
	virtual bool IsWithin( double x, double y, VPlaneType plane );

	// Specific to PVArray since can rotate about two-axes
	void TiltAndAzimuth(double n_points, double tilt, double azimuth, double xc, double yc, double zc, double length, double x[4], double y[4], double z[4]);
	void SetVertices(double x[4],double y[4],double z[4]);
	void GetVertices(double x[4],double y[4], double z[4]);
	void GetDistances(int face, double distances[4]);
	void UnitVectorNormal(int face, double unit_vector[3]);
	void UpdateOnMove( VHandle *h, VPlaneType plane );
	double UpdateOnStretch(int face, double length, VHandle *h );

	enum { HH_MOVE, HH_LEFT, HH_BOTTOM, HH_AZIMUTH, HH_RIGHT, HH_TOP };

private:
	double m_x[4], m_y[4], m_z[4];

	// As looking down on x-y plane
	// 0 - Right Bottom point
	// 1 - Right Top
	// 2 - Left Top
	// 3 - Left Bottom

/*
 2|----<------|1
  |           |
  |           |
 3|---->------|0
 */
};

class VCylinderObject : public VObject
{
public:
	VCylinderObject();
	virtual wxString GetTypeName();
	virtual VObject *Duplicate();
	virtual void BuildModel( s3d::scene & );
	
	virtual void SetupHandles( VPlaneType plane );
	virtual bool OnHandleMoved( VHandle *, VPlaneType );
	virtual void DrawOnPlane( VRenderer2D &dc, VPlaneType plane );	
	virtual bool IsWithin( double x, double y, VPlaneType plane );
	
	enum{ HH_MOVE, HH_DIAM, HH_BOTTOM, HH_TOP};
};

class VRoofObject : public VObject
{
public:
	VRoofObject();
	virtual wxString GetTypeName();
	virtual VObject *Duplicate();
	virtual void BuildModel( s3d::scene & );
	virtual void SetupHandles( VPlaneType plane );
	virtual bool OnHandleMoved( VHandle *, VPlaneType );
	virtual void DrawOnPlane( VRenderer2D &dc, VPlaneType plane );	
	virtual bool IsWithin( double x, double yz, VPlaneType plane );

	void GetXZPoints(double x[4], double z[4]);

	enum { HH_MOVE, HH_TOP, HH_RIGHT, HH_ROTATE_XY, HH_LEFT, HH_BOTTOM };

};





#endif

