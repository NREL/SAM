/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

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
	explicit VProperty( int i, const wxArrayString &choices );
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

	wxArrayString &GetChoices();

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
	wxArrayString m_choices;
};

class VHandle
{
public:
	VHandle( VObject *o, int id, double x, double yz, 
		const wxCursor &curs = wxCURSOR_SIZING, const wxString &name = wxEmptyString );

	VObject *GetObject();
	void GetCurrentPos( double *x, double *yz );
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


class VTreeObject : public VObject
{
public:
	VTreeObject();
	virtual wxString GetTypeName();
	virtual VObject *Duplicate();
	virtual void BuildModel( s3d::scene & );
	virtual void SetupHandles( VPlaneType plane );
	virtual bool OnHandleMoved( VHandle *, VPlaneType );	
	virtual void DrawOnPlane( VRenderer2D &dc, VPlaneType plane );	
	virtual bool IsWithin( double x, double y, VPlaneType plane );

	enum { HH_MOVE, HH_DIAM, HH_TOPDIAM, HH_TRUNK, HH_HEIGHT };

	enum { ROUNDED, CONICAL };

private:
	size_t GetXZPoints( double x[10], double z[10] );
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

	void TiltAndAzimuth(double n_points, double tilt, double azimuth, 
		double x0, double y0, double z0, 
		double x[], double y[], double z[]);

	void GetPoints( double xx[4], double yy[4], double zz[4] );
	
	enum { HH_MOVE, HH_LEFT, HH_BOTTOM, HH_AZIMUTH, HH_RIGHT, HH_TOP };

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

