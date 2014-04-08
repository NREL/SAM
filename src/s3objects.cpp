#include <wx/datstrm.h>
#include <algorithm>

#include "s3objects.h"


#ifndef DTOR
#define DTOR 0.0174532925199433
#endif

VProperty::VProperty()
{
	Init();
}

VProperty::VProperty( VProperty *ref )
{
	Init();
	m_type = INVALID;
	m_pReference = ref;
}

VProperty::VProperty( double dp, int dim )
{
	Init();
	m_type = DOUBLE;
	m_doubleVal = dp;
	m_dimension = dim;
}

VProperty::VProperty( bool bp )
{
	Init();
	m_type = BOOLEAN;
	m_boolVal = bp;
}

VProperty::VProperty( int ip, int dim )
{
	Init();
	m_type = INTEGER;
	m_intVal = ip;
	m_dimension = dim;
}

VProperty::VProperty( const wxColour &cp )
{
	Init();
	m_type = COLOUR;
	m_colour = cp;
}

VProperty::VProperty( const wxString &sp )
{
	Init();
	m_type = STRING;
	m_string = sp;
}

int VProperty::GetType()
{
	if (m_pReference != 0) return m_pReference->GetType();
	else return m_type;
}

void VProperty::Set( double d )
{
	if ( m_pReference ) m_pReference->Set( d );	
	else m_doubleVal = d;
}

void VProperty::Set( bool b )
{
	if (m_pReference ) m_pReference->Set( b );
	else m_boolVal = b;
}

void VProperty::Set( int i )
{
	if ( m_pReference ) m_pReference->Set( i );
	else m_intVal = i;
}

void VProperty::Set( const wxColour &c )
{
	if ( m_pReference ) m_pReference->Set( c );
	else m_colour = c;
}

void VProperty::Set( const wxString &s )
{
	if ( m_pReference ) m_pReference->Set( s );
	else m_string = s;
}


int VProperty::GetInteger()
{
	if ( m_pReference ) return m_pReference->GetInteger();
	else return m_intVal;
}

bool VProperty::GetBoolean()
{
	if ( m_pReference ) return m_pReference->GetBoolean();
	else return m_boolVal;
}

double VProperty::GetDouble()
{
	if ( m_pReference ) return m_pReference->GetDouble();
	else return m_doubleVal;
}

wxColour VProperty::GetColour()
{
	if ( m_pReference ) return m_pReference->GetColour();
	else return m_colour;
}

wxString VProperty::GetString()
{
	if ( m_pReference ) return m_pReference->GetString();
	else return m_string;
}

void VProperty::Write( wxOutputStream &_o )
{
	wxDataOutputStream out( _o );
	int type = GetType();
	out.Write8( 0x1d );
	out.Write16( (wxUint16)type );
	switch( type )
	{
	case DOUBLE: out.WriteDouble( GetDouble() ); break;
	case BOOLEAN: out.Write8( GetBoolean() ? 1 : 0 ); break;
	case INTEGER: out.Write32( GetInteger() ); break;
	case STRING: out.WriteString( GetString() ); break;
	case COLOUR:
		{
			wxColour c = GetColour();
			out.Write8( c.Red() );
			out.Write8( c.Green() );
			out.Write8( c.Blue() );
			out.Write8( c.Alpha() );
		}
		break;
	}
	out.Write8(0x1d);
}

bool VProperty::Read( wxInputStream &_i )
{
	wxDataInputStream in(_i);

	wxUint8 code = in.Read8();
	wxUint16 type = in.Read16();

	if ( m_pReference )
		m_pReference->m_type = type;
	else
		m_type = type;

	wxUint8 r,g,b,a;
	switch( type )
	{
	case DOUBLE: Set(in.ReadDouble()); break;
	case BOOLEAN: Set( in.Read8() != 0 ? true : false ); break;
	case INTEGER: Set( (int)in.Read32()); break;
	case STRING: Set(in.ReadString()); break;
	case COLOUR: 
		r = in.Read8();
		g = in.Read8();
		b = in.Read8();
		a = in.Read8();
		Set( wxColour(r,g,b,a) );
		break;
	}

	return ( code == in.Read8() );
}

void VProperty::Init()
{
	m_dimension = NONDIM;
	m_type = INVALID;
	m_pReference = 0;
	m_doubleVal = 0.0;
	m_intVal = 0;
	m_boolVal = false;
}

int VProperty::GetDimension()
{
	return m_dimension;
}

static int gs_idCounter = 0;

VObject::VObject( )
{
	++gs_idCounter;
	m_id = gs_idCounter;
	m_visible = true;
	AddProperty( "Name", new VProperty( wxString::Format("untitled %d", m_id ) ) );
}

VObject::~VObject()
{
	DeleteProperties();
	DeleteHandles();
}

void VObject::DeleteHandles()
{
	for( size_t i=0;i<m_handles.size();i++ )
		delete m_handles[i];
	m_handles.clear();
}

void VObject::DeleteProperties()
{
	for( size_t i=0;i<m_properties.size();i++ )
		delete m_properties[i].prop;
	m_properties.clear();
}

void VObject::SetupHandles( VPlaneType plane )
{
	// nothing to do
}

bool VObject::OnHandleMoved( VHandle *, VPlaneType plane )
{
	return false;
}

bool VObject::IsWithin( double x, double y, VPlaneType plane )
{
	return false;
}

void VObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane )
{
	// nothing to do 
}

bool VObject::Copy( VObject *rhs )
{
	DeleteProperties();
	for( size_t i=0;i<rhs->m_properties.size();i++ )
	{
		propdata x;
		x.name = rhs->m_properties[i].name;
		x.lowered = rhs->m_properties[i].lowered;
		x.prop = new VProperty( *rhs->m_properties[i].prop );
		m_properties.push_back( x );
	}

	return true;
}

int VObject::GetId()
{
	return m_id;
}

VProperty &VObject::Property( const wxString &name )
{
	static VProperty s_nullProp;

	wxString lowered = name.Lower();
	for( size_t i=0;i<m_properties.size();i++ )
		if ( lowered == m_properties[i].lowered )
			return *m_properties[i].prop;

	return s_nullProp;
}

wxArrayString VObject::Properties()
{
	wxArrayString list;
	for( size_t i=0;i<m_properties.size();i++ )
		list.Add( m_properties[i].name );

	return list;
}

std::vector<VHandle*> VObject::GetHandles()
{
	return m_handles;
}

void VObject::Write( wxOutputStream &_o )
{
	wxDataOutputStream out(_o);
	out.Write8( 0xaf ); // start code
	out.Write8( 1 ); // version

	out.Write8( m_visible ? 1 : 0 );

	out.Write32( m_properties.size() );
	for( size_t i=0;i<m_properties.size(); i++ )
	{
		out.WriteString( m_properties[i].name );
		m_properties[i].prop->Write( _o );
	}

	out.Write8( 0xaf );
}

bool VObject::Read( wxInputStream &_i )
{
	wxDataInputStream in(_i);
	wxUint8 code = in.Read8();
	in.Read8(); // version

	m_visible = in.Read8() != 0;

	size_t n = in.Read32();
	for( size_t i=0;i<n;i++)
	{
		wxString name = in.ReadString();
		Property(name).Read( _i );
	}

	return in.Read8() == code;
}


void VObject::AddProperty( const wxString &name, VProperty *prop )
{
	propdata x;
	x.name = name;
	x.lowered = name.Lower();
	x.prop = prop;
	m_properties.push_back( x );
}

VHandle *VObject::AddHandle( int  id, double x, double yz, const wxCursor &curs, const wxString &name )
{
	VHandle *h = new VHandle( this, id, x, yz, curs, name );
	m_handles.push_back( h );
	return h;
}
VHandle::VHandle( VObject *o, int id, double x, double yz, const wxCursor &curs, const wxString &name )
{
	m_object = o;
	m_id = id;
	m_x = m_origX = x;
	m_yz = m_origYZ = yz;
	m_name = name;
	m_cursor = curs;
}

VObject *VHandle::GetObject() { return m_object; }
void VHandle::GetOriginalPos( double *x, double *yz )
{
	*x = m_origX;
	*yz = m_origYZ;
}

void VHandle::GetCurrentPos( double *x, double *yz )
{
	*x = m_x;
	*yz = m_yz;
}

void VHandle::SetOriginalPos(double x, double yz)
{
	m_origX = x;
	m_origYZ = yz;
}

double VHandle::GetX()
{
	return m_x;
}
double VHandle::GetYZ()
{
	return m_yz;
}

double VHandle::GetDeltaX()
{
	return m_x - m_origX;
}

double VHandle::GetDeltaYZ()
{
	return m_yz - m_origYZ;
}

double VHandle::GetDistance()
{
	double dx = GetDeltaX();
	double dyz = GetDeltaYZ();
	return sqrt( dx*dx + dyz*dyz );
}
wxString VHandle::GetName()
{
	return m_name;
}

int VHandle::GetId()
{
	return m_id;
}

wxCursor VHandle::GetCursor()
{
	return m_cursor;
}
void VHandle::MoveTo( double x, double yz )
{
	m_x = x;
	m_yz = yz;
}


VConicalTreeObject::VConicalTreeObject()
{
	AddProperty( "X", new VProperty( 0.0, LENGTH ) );
	AddProperty( "Y", new VProperty( 0.0, LENGTH ) );
	AddProperty( "Diameter", new VProperty( 20.0, LENGTH ) );
	AddProperty( "Top Diameter", new VProperty( 5.0, LENGTH ) );
	AddProperty( "Overall Height", new VProperty( 50.0, LENGTH ) );
	AddProperty( "Trunk Height", new VProperty( 10.0, LENGTH ) );
}

wxString VConicalTreeObject::GetTypeName()
{
	return "Tree (Conical)";
}

VObject *VConicalTreeObject::Duplicate()
{
	VConicalTreeObject *tree = new VConicalTreeObject;
	tree->Copy( this );
	return tree;
}

void VConicalTreeObject::BuildModel( s3d::scene &sc )
{
	double x = Property("X").GetDouble();
	double y = Property("Y").GetDouble();
	double diam = Property("Diameter").GetDouble();
	double topDiam = Property("Top Diameter").GetDouble();
	double height = Property("Overall Height").GetDouble();
	double trunk = Property("Trunk Height").GetDouble();

	sc.reset();
	sc.colors(s3d::rgba(128, 64, 0, 155), s3d::rgba(128, 64, 0, 155));
	sc.conical(GetId(), x, y, 0, trunk, diam / 6, diam / 6);
	sc.colors( s3d::rgba(0, 186, 107, 155 ), s3d::rgba(0, 186, 107, 155 ) );
	sc.conical( GetId(), x, y, trunk, height-trunk, diam/2, topDiam/2 );
}

void VConicalTreeObject::SetupHandles( VPlaneType plane )
{	
	double xc = Property("X").GetDouble();
	double diam = Property("Diameter").GetDouble();
	double topDiam = Property("Top Diameter").GetDouble();

	// top view
	if ( plane == PLANE_XY )
	{
		double yc = Property("Y").GetDouble();
		AddHandle( HH_MOVE, xc, yc );
		AddHandle( HH_DIAM, xc + diam/2, yc );
		AddHandle( HH_TOPDIAM, xc + topDiam/2, yc );
	}
	// side view
	else if ( plane == PLANE_XZ )
	{
		double trunk = Property("Trunk Height").GetDouble();
		double height = Property("Overall Height").GetDouble();
		AddHandle( HH_MOVE, xc, trunk/2, wxCURSOR_SIZEWE );
		AddHandle( HH_DIAM, xc+diam/2, trunk, wxCURSOR_SIZEWE );
		AddHandle( HH_TOPDIAM, xc+topDiam/2, height, wxCURSOR_SIZEWE ) ;
		AddHandle( HH_TRUNK, xc, trunk, wxCURSOR_SIZENS );
		AddHandle( HH_HEIGHT, xc, height, wxCURSOR_SIZENS );
	}
}

bool VConicalTreeObject::OnHandleMoved( VHandle *h, VPlaneType plane )
{
	int id = h->GetId();
	if ( id == HH_MOVE )
	{
		Property("X").Set( h->GetX() );
		if ( plane == PLANE_XY ) Property("Y").Set( h->GetYZ() );
		return true;
	}
	// Deal with both the bottom and top diameters
	else if ( id == HH_DIAM || id == HH_TOPDIAM)
	{
		if ( plane == PLANE_XY )
		{
			double xc = Property("X").GetDouble();
			double yc = Property("Y").GetDouble();
			double x, y;
			h->GetCurrentPos( &x, &y );
			double radius = sqrt( (x-xc)*(x-xc) + (y-yc)*(y-yc) );
			(id == HH_DIAM) ? Property("Diameter").Set( radius * 2.0 ) : Property("Top Diameter").Set( radius * 2.0 );
		}
		else if ( plane == PLANE_XZ )
		{
			double xc = Property("X").GetDouble();
			(id == HH_DIAM) ? Property("Diameter").Set( fabs(h->GetX() - xc)*2.0 ) : Property("Top Diameter").Set( fabs(h->GetX() - xc)*2.0 );
		}
		return true;
	}
	else if ( id == HH_TRUNK )
	{
		Property("Trunk Height").Set( h->GetYZ() );
		return true;
	}
	else if ( id == HH_HEIGHT )
	{
		Property("Overall Height").Set( h->GetYZ() );
		return true;
	}

	return false;
}

void VConicalTreeObject::GetXZPoints( double x[m_nPoints], double z[m_nPoints] )
{
	double xc = Property("X").GetDouble();
	double diam = Property("Diameter").GetDouble();
	double trunk = Property("Trunk Height").GetDouble();
	double height = Property("Overall Height").GetDouble();
	double top_diam = Property("Top Diameter").GetDouble();


	double skirt_radius = diam/2;
	double trunk_radius = diam/6;
	double top_radius = top_diam/2;

	// top point
	x[0] = xc;
	z[0] = height;

	// top left point
	x[1] = xc-top_radius;
	z[1] = height;

	// left point
	x[2] = xc-skirt_radius;
	z[2] = trunk;

	// inner left point
	x[3] = xc-trunk_radius;
	z[3] = trunk;

	// bottom left point
	x[4] = xc-trunk_radius;
	z[4] = 0.0;

	// bottom right point
	x[5] = xc+trunk_radius;
	z[5] = 0.0;

	// inner right point
	x[6] = xc+trunk_radius;
	z[6] = trunk;

	// right point
	x[7] = xc+skirt_radius;
	z[7] = trunk;

	// top right point
	x[8] = xc+top_radius;
	z[8] = height;
}

void VConicalTreeObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane )
{

	if ( plane == PLANE_XY )
	{
		double xc = Property("X").GetDouble();
		double yc = Property("Y").GetDouble();
		double diam = Property("Diameter").GetDouble();
		double top_diam = Property("Top Diameter").GetDouble();
		dc.Circ( xc, yc, diam/2 );
		dc.Circ( xc, yc, top_diam/2);

	}
	else if ( plane == PLANE_XZ )
	{		
		double x[m_nPoints], z[m_nPoints];
		GetXZPoints( x, z );
		dc.Poly( x, z, m_nPoints );
	}
}

bool VConicalTreeObject::IsWithin( double x, double y, VPlaneType plane )
{
	if ( plane == PLANE_XY )
	{
		double xc = Property("X").GetDouble();
		double yc = Property("Y").GetDouble();
		double diam = Property("Diameter").GetDouble();

		return ( sqrt( (x-xc)*(x-xc) + (y-yc)*(y-yc) ) < diam/2 );
	}
	else
	{
		double XX[m_nPoints], ZZ[m_nPoints];
		GetXZPoints( XX, ZZ );
		return s3d::inpoly( XX, ZZ, m_nPoints, x, y );
	}
}


VBoxObject::VBoxObject()
{
	AddProperty( "X", new VProperty(0.0, LENGTH) );
	AddProperty( "Y", new VProperty(0.0, LENGTH) );
	AddProperty( "Z", new VProperty(0.0, LENGTH) );
	AddProperty( "Width", new VProperty(15.0, LENGTH) );
	AddProperty( "Length", new VProperty(15.0, LENGTH) );
	AddProperty( "Height", new VProperty(15.0, LENGTH) );
	AddProperty( "Rotation", new VProperty(0.0 ) );
	AddProperty( "Color", new VProperty( wxColour(155,0,0,144) ) );
	AddProperty( "Capped", new VProperty( true ) );
}

wxString VBoxObject::GetTypeName()
{
	return "Box";
}

VObject *VBoxObject::Duplicate()
{
	VBoxObject *box = new VBoxObject;
	box->Copy( this );
	return box;
}

void VBoxObject::BuildModel( s3d::scene &sc )
{
	double x = Property("X").GetDouble();
	double y = Property("Y").GetDouble();
	double z = Property("Z").GetDouble();
	double w = Property("Width").GetDouble();
	double l = Property("Length").GetDouble();
	double h = Property("Height").GetDouble();
	double r = Property("Rotation").GetDouble();
	bool cap = Property("Capped").GetBoolean();
	
	wxColour cc = Property("Color").GetColour();
	s3d::rgba scc1( cc.Red(), cc.Green(), cc.Blue(), cc.Alpha() );
	s3d::rgba scc2( cc.Red(), cc.Green(), cc.Blue(), 255 );
	sc.reset();
	sc.colors( scc1, scc2 );
	sc.type( s3d::scene::OBSTRUCTION );

	sc.box( GetId(), x, y, z, r, w, l, h, cap ? s3d::ALL_FACES : s3d::SIDES );

}

void VBoxObject::SetupHandles( VPlaneType plane )
{
	
	double x = Property("X").GetDouble();
	double y = Property("Y").GetDouble();
	double z = Property("Z").GetDouble();
	double w = Property("Width").GetDouble();
	double l = Property("Length").GetDouble();
	double h = Property("Height").GetDouble();
	double r = Property("Rotation").GetDouble();

	double xx[5], yy[5];
	xx[0] = x + w;        yy[0] = y+l/2;    //HH_RIGHT
	xx[1] = x + w/2;      yy[1] = y+l;      // HH_TOP
	xx[2] = x + 3*w/4;    yy[2] = y;      // HH_ROTATE_XY
	xx[3] = x ;           yy[3] = y+l/2;    //HH_LEFT
	xx[4] = x + w/2;      yy[4] = y;   // HH_BOTTOM
	s3d::rotate2dxz(x,y,xx,yy,r,5);

	if ( plane == PLANE_XY )
	{
		AddHandle( HH_MOVE, x, y );
		AddHandle( HH_RIGHT,xx[0], yy[0], wxCURSOR_SIZEWE );
		AddHandle( HH_TOP, xx[1], yy[1], wxCURSOR_SIZENS );
		AddHandle( HH_ROTATE_XY, xx[2], yy[2], wxCURSOR_BULLSEYE );
		AddHandle( HH_LEFT, xx[3], yy[3], wxCURSOR_SIZEWE );
		AddHandle( HH_BOTTOM, xx[4], yy[4], wxCURSOR_SIZENS );

	}
	else if ( plane == PLANE_XZ )
	{
		AddHandle( HH_MOVE, x, z );
		AddHandle( HH_TOP, x + w/2, z + h, wxCURSOR_SIZENS );
		AddHandle( HH_BOTTOM, x + w/2, z, wxCURSOR_SIZENS);
	}
}

double GetLength( double bx, double by, double bw, double bh, double br, 
	double x, double y, int mode, double *xshifted = 0, double *yshifted = 0 )
{
	double xx00 = bw;
	double yy00 = 0;
	if ( mode == 2 )
	{
		xx00 = 0;
		yy00 = bh;
	}

	double r = br*M_PI/180.0;
	double xx0 = xx00*cos(r)-yy00*sin(r);
	double yy0 = xx00*sin(r)+yy00*cos(r);

	double xx1 = x - bx;
	double yy1 = y - by;

	double scale = (xx1*xx0 + yy1*yy0)/(xx0*xx0 + yy0*yy0);
	double projx = scale*xx0;
	double projy = scale*yy0;
	double len = sqrt(projx*projx + projy*projy);
	if ( len < 0.1 ) len = 0.1;

	if ( xshifted ) *xshifted = x-len*cos(r);
	if ( yshifted ) *yshifted = y-len*sin(r);

	return len;
}

bool VBoxObject::OnHandleMoved( VHandle *hh, VPlaneType plane )
{
	// extract previous properties
	double bx = Property("X").GetDouble();
	double by = Property("Y").GetDouble();
	double bz = Property("Z").GetDouble();
	double bw = Property("Width").GetDouble();
	double bl = Property("Length").GetDouble();
	double bh = Property("Height").GetDouble();
	double br = Property("Rotation").GetDouble();

	int id = hh->GetId();
	double x = hh->GetX();
	double y = hh->GetYZ();
	double z = hh->GetYZ();


	if ( id == HH_MOVE )
	{
		Property("X").Set( x );
		if ( plane == PLANE_XY ) Property("Y").Set( y );
		else Property("Z").Set( z );
		return true;
	}
	else if ( id == HH_RIGHT) 
	{
		Property("Width").Set( GetLength( bx, by, bw, bh, br, x, y, 1 ) );
		return true;
	}
	else if ( id == HH_TOP )
	{
		// compute new center and height/depth
		if ( plane == PLANE_XY )
			Property("Length").Set( GetLength( bx, by, bw, bh, br, x, y, 2 ) );
		else
			Property("Height").Set( z - bz );

		return true;
	}
	/*
	else if ( id == HH_LEFT) 
	{
		double xsh = x, ysh = y;
		double len = GetLength( bx, by, bw, bh, br, x, y, 1, &xsh, &ysh );		
		Property("X").Set(xsh);
		Property("Y").Set(ysh);
		Property("Width").Set(len);

		return true;
	}*/
	/*
	else if ( id == HH_BOTTOM )
	{
		// compute new center and height/depth
		if ( plane == PLANE_XY ) 
		{
			double d = dxy;
			double dh = d - l/2;
			double hP = l + dh;

			Property("Y").Set(y-dh);
			Property("Length").Set(hP);
		}
		else
		{
			double dd = fabs(dz) - h/2;
			double dP = h + dd;
			Property("Z").Set(z-dd);
			Property("Height").Set(dP);
		}

		return true;
	}
	*/
	else if ( id == HH_ROTATE_XY ) 
	{
		double r = (180.0/M_PI)*atan2( hh->GetYZ()-by, hh->GetX()-bx );
		Property("Rotation").Set( r /*deg*/);
	}
	
	return false;
}

void VBoxObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane )
{
	double x = Property("X").GetDouble();
	double y = Property("Y").GetDouble();
	double z = Property("Z").GetDouble();
	double w = Property("Width").GetDouble();
	double l = Property("Length").GetDouble();
	double h = Property("Height").GetDouble();
	double r = Property("Rotation").GetDouble();

	if ( plane == PLANE_XY )
	{
		double xx[4], yy[4];
		s3d::get_rotated_box_points( x, y, w, l, r, xx, yy);
		dc.Poly(xx,yy,4);
	}
	else if ( plane == PLANE_XZ )
	{
		double minDim = std::min(w,l);
		double maxDim = std::max(w,l);
		double dDim = maxDim-minDim;
		double xDim = minDim + fabs(cos(r*M_PI/180))*dDim;

		dc.Rect(x, z,xDim, h);
	}
}

bool VBoxObject::IsWithin( double px, double pyz, VPlaneType plane )
{
	double x = Property("X").GetDouble();
	double y = Property("Y").GetDouble();
	double z = Property("Z").GetDouble();
	double w = Property("Width").GetDouble();
	double l = Property("Length").GetDouble();
	double h = Property("Height").GetDouble();
	double r = Property("Rotation").GetDouble();

	if ( plane == PLANE_XY )
	{
		double xx[4], yy[4];
		s3d::get_rotated_box_points( x, y, w, l, r, xx, yy);
		return s3d::inpoly( xx, yy, 4, px, pyz );

	}
	else if ( plane == PLANE_XZ )
	{
		return ( px >= x && px <= x+w
			&& pyz >= z && pyz <= z+h );
	}

	return false;
}



VActiveSurfaceObject::VActiveSurfaceObject()
{
	AddProperty( "X", new VProperty(0.0, LENGTH ) );
	AddProperty( "Y", new VProperty(0.0, LENGTH ) );
	AddProperty( "Z", new VProperty(0.0, LENGTH ) );
	AddProperty( "Width", new VProperty( 5.0, LENGTH ) );
	AddProperty( "Height", new VProperty( 10.0, LENGTH ) );
	AddProperty( "Azimuth", new VProperty(180.0 ) );
	AddProperty( "Tilt", new VProperty(30.0) );
}

wxString VActiveSurfaceObject::GetTypeName()
{
	return "PVArray";
}

VObject *VActiveSurfaceObject::Duplicate()
{
	VActiveSurfaceObject *p = new VActiveSurfaceObject;
	p->Copy( this );
	return p;
}

void VActiveSurfaceObject::SetupHandles( VPlaneType plane )
{
	double xc = Property("X").GetDouble();
	double yc = Property("Y").GetDouble();
	double zc = Property("Z").GetDouble();
	double w = Property("Width").GetDouble();
	double h = Property("Height").GetDouble();
	double azimuth = Property("Azimuth").GetDouble();
	double tilt = Property("Tilt").GetDouble();

	double x[6], y[6], z[6];
	x[0] = xc; y[0] = yc;		   // HH_MOVE
	x[1] = xc + w/2; y[1] = yc;    // HH_LEFT
	x[2] = xc; y[2] = yc+h/2;      // HH_BOTTOM
	x[3] = xc; y[3] = yc+h/4;      // HH_AZIMUTH
	x[4] = xc - w/2; y[4] = yc;    // HH_RIGHT
	x[5] = xc; y[5] = yc - h/2;    // HH_TOP

	// Rotate the default handle positions by tilt and azimuth
	TiltAndAzimuth(6,tilt,azimuth,xc,yc,zc,h,x,y,z);
	
	if ( plane == PLANE_XY )
	{
		AddHandle( HH_MOVE, x[0], y[0] );
		AddHandle( HH_LEFT, x[1], y[1], wxCURSOR_SIZEWE );
		AddHandle( HH_BOTTOM, x[2], y[2], wxCURSOR_SIZENS );
		AddHandle( HH_AZIMUTH, x[3], y[3], wxCURSOR_BULLSEYE );
		AddHandle( HH_RIGHT, x[4], y[4], wxCURSOR_SIZEWE );
		AddHandle( HH_TOP, x[5], y[5] , wxCURSOR_SIZENS );
		
	}
	else if (plane == PLANE_XZ )
	{
		AddHandle( HH_MOVE, x[0], z[0] ); 
		//AddHandle( HH_BOTTOM, x[2],z[2] );
		//AddHandle( HH_TOP, x[5],z[5] );
	}
}

bool VActiveSurfaceObject::OnHandleMoved( VHandle *h, VPlaneType plane )
{
	// extract previous properties
	double width = Property("Width").GetDouble();
	double height = Property("Height").GetDouble();

	// get current properties
	int id = h->GetId();

	// Move is relevent to both views
	if (id == HH_MOVE )
		{
			UpdateOnMove(h,plane);
			return true;
		}

	// Other properties occur only in XY plane
	if ( plane == PLANE_XY )
	{
		if (id == HH_LEFT )
		{
			// face = 0; length = width
			double length = UpdateOnStretch(0,width,h);
			Property("Width").Set(length);
		}
		else if (id == HH_BOTTOM )
		{
			// face = 1; length = height
			double length = UpdateOnStretch(1,height,h);
			Property("Height").Set(length);
		}

		else if (id == HH_RIGHT)
		{
			// face = 2; length = width
			double length = UpdateOnStretch(2,width,h);
			Property("Width").Set(length);
		}	
		else if (id == HH_TOP)
		{
			// face = 3; length = height
			double length = UpdateOnStretch(3,height,h);
			Property("Height").Set(length);
		}
		else if (id == HH_AZIMUTH)
		{
			double dx = h->GetX() - Property("X").GetDouble();
			double dy = h->GetYZ() - Property("Y").GetDouble();
			double azimuth = (180/M_PI)*atan2(dx,dy);
			if (azimuth < 0) azimuth += 360;
			Property("Azimuth").Set(azimuth);
		}
	}
	
	return false;
}

void VActiveSurfaceObject::BuildModel( s3d::scene &sc )
{
	double x = Property("X").GetDouble();
	double y = Property("Y").GetDouble();
	double z = Property("Z").GetDouble();
	double w = Property("Width").GetDouble();
	double h = Property("Height").GetDouble();
	double azimuth = Property("Azimuth").GetDouble();
	double tilt = Property("Tilt").GetDouble();
	
	// compute vertices of plane
	double xx[4], yy[4], zz[4];
	s3d::get_rotated_box_points(x, y, w, h,0.0, xx, yy );
	TiltAndAzimuth(4,tilt,azimuth,x,y,z,h,xx,yy,zz);
	SetVertices(xx,yy,zz);

	// set up scene
	sc.reset();
	sc.colors( s3d::rgba( 0, 107, 186, 200 ), s3d::rgba( 0, 88, 153 ) );
	sc.type( s3d::scene::ACTIVE );
	sc.plane(GetId(), xx,yy,zz);
	sc.poly( GetId() );
}

void VActiveSurfaceObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane )
{

	double w = Property("Width").GetDouble();
	double h = Property("Height").GetDouble();
	double x = Property("X").GetDouble();
	double y = Property("Y").GetDouble();
	double z = Property("Z").GetDouble();
	double azimuth = Property("Azimuth").GetDouble();
	double tilt = Property("Tilt").GetDouble();

	double xx[4], yy[4], zz[4];
	s3d::get_rotated_box_points(x, y, w, h,0.0, xx, yy );
	TiltAndAzimuth(4,tilt,azimuth,x,y,z,h,xx,yy,zz);	

	if (plane == PLANE_XY ) dc.Poly(xx,yy,4);
	else dc.Poly(xx,zz,4);

}

bool VActiveSurfaceObject::IsWithin( double xt, double yt, VPlaneType plane )
{
	double w = Property("Width").GetDouble();
	double h = Property("Height").GetDouble();
	double x = Property("X").GetDouble();
	double y = Property("Y").GetDouble();
	double z = Property("Z").GetDouble();
	double azimuth = Property("Azimuth").GetDouble();
	double tilt = Property("Tilt").GetDouble();

	double xx[4], yy[4], zz[4];
	s3d::get_rotated_box_points(x, y, w, h,0.0, xx, yy );
	TiltAndAzimuth(4,tilt,azimuth,x,y,z,h,xx,yy,zz);	
	
	if (plane == PLANE_XY) return s3d::inpoly(xx,yy,4,xt,yt);
	else return s3d::inpoly(xx,zz,4,xt,yt);

}

void VActiveSurfaceObject::TiltAndAzimuth(double n_points, double tilt,double azimuth,
							  double xc, double yc, double zc, double length, 
							  double x[], double y[],double z[])
{
	// set initial z points
	for (int i = 0; i < n_points; i++) z[i] = zc;

	// y offset to move before and after rotation
	double y_offset = yc + length/2;

	// offset and then tilt about x-axis 
	s3d::rotate2dxz(y_offset, zc, y, z, -tilt, n_points);

	// center and rotate about z-axis
	s3d::rotate2dxz(xc,yc,x,y,-azimuth,n_points);
	
}

void VActiveSurfaceObject::SetVertices(double x[4], double y[4], double z[4])
{
	for (int i = 0; i < 4; i++) 
	{
		m_x[i] = x[i];
		m_y[i] = y[i];
		m_z[i] = z[i];
	}
}

void VActiveSurfaceObject::GetVertices(double x[4], double y[4], double z[4])
{
	for (int i = 0; i < 4; i++) 
	{
		x[i] = m_x[i];
		y[i] = m_y[i];
		z[i] = m_z[i];
	}
}

void VActiveSurfaceObject::GetDistances(int face, double distances[4])
{
	int high = face+1; int low = face;
	if (high > 3) high = 0;

	distances[0] = m_x[high] - m_x[low];
	distances[1] = m_y[high] - m_y[low];
	distances[2] = m_z[high] - m_z[low];
	distances[3] = sqrt(distances[0]*distances[0] + distances[1]*distances[1] + distances[2]*distances[2]);
}

void VActiveSurfaceObject::UnitVectorNormal(int face, double unit_vector[3])
{
	double distances[4];

	// the normal vector is the tangent vector of the previous face
	face -= 1; 
	if (face < 0) face = 3;
	GetDistances(face,distances);

	// compute unit vector
	for (int i = 0; i < 3; i++) unit_vector[i] = distances[i]/distances[3];
}
void VActiveSurfaceObject::UpdateOnMove( VHandle *h, VPlaneType plane )
{
	double xc = Property("X").GetDouble();
	double yc = Property("Y").GetDouble();
	double zc = Property("Z").GetDouble();
	double x = h->GetX();
	double yz = h->GetYZ();

	// extract absolute distance moved by handle
	double dl = h->GetDistance();

	// get vertices
	double xx[4], yy[4], zz[4];
	GetVertices(xx,yy,zz);

	double distance_vector[2];
	if (fabs(dl) > 1e-2)
	{
		distance_vector[0] = h->GetDeltaX()/dl;
		distance_vector[1] = h->GetDeltaYZ()/dl;
	}
	else
	{
		distance_vector[0] = 0;
		distance_vector[1] = 0;
	}

	if (plane == PLANE_XY)
	{
		for (int i = 0; i < 4; i++)
		{
			xx[i] += dl*distance_vector[0];
			yy[i] += dl*distance_vector[1];
		}

		Property("X").Set(xc + dl*distance_vector[0]);
		Property("Y").Set(yc + dl*distance_vector[1]);
	}
	else
	{
		for (int i = 0; i < 4; i++)
		{
			xx[i] += dl*distance_vector[0];
			zz[i] += dl*distance_vector[1];
		}

		Property("X").Set(xc + dl*distance_vector[0]);
		Property("Z").Set(zc + dl*distance_vector[1]);
	}

	// set vertices
	SetVertices(xx,yy,zz);

	// update handle properties
	h->SetOriginalPos(x,yz);

}
double VActiveSurfaceObject::UpdateOnStretch(int face, double length, VHandle *h )
{
	double xc = Property("X").GetDouble();
	double yc = Property("Y").GetDouble();
	double zc = Property("Z").GetDouble();
	double x = h->GetX();
	double yz = h->GetYZ();

	// extract absolute distance moved by handle
	double dl = h->GetDistance();

	// get vertices
	double xx[4], yy[4], zz[4];
	GetVertices(xx,yy,zz);

	// compute outward pointing vector
	double unit_vector[3];
	UnitVectorNormal(face,unit_vector);

	// determine whether handle moved along normal or opposite normal
	if (fabs(dl) > 1e-2)
	{
		double distance_vector[2];
		distance_vector[0] = h->GetDeltaX()/dl;
		distance_vector[1] = h->GetDeltaYZ()/dl;
		double dot_product = (unit_vector[0]*distance_vector[0] + unit_vector[1]*distance_vector[1]);
		if (dot_product < 0) dl *= -1; 
	}

	// get vertices to update
	int low = face; int high = face+1;
	if (high > 3) high = 0;

	// update face vertices by projecting along unit vector by dw
	xx[low] += dl*unit_vector[0]; yy[low] += dl*unit_vector[1]; zz[low] += dl*unit_vector[2];
	xx[high] += dl*unit_vector[0]; yy[high] += dl*unit_vector[1]; zz[high] += dl*unit_vector[2];

	// update centers
	Property("X").Set(xc + 0.5*dl*unit_vector[0]);
	Property("Y").Set(yc + 0.5*dl*unit_vector[1]);
	Property("Z").Set(zc + 0.5*dl*unit_vector[2]);

	// set vertices
	SetVertices(xx,yy,zz);

	// update handle properties
	h->SetOriginalPos(x,yz);	
	length+= dl;
	return length;

}

VCylinderObject::VCylinderObject()
{
	AddProperty( "X", new VProperty(0.0, LENGTH ) );
	AddProperty( "Y", new VProperty(0.0 , LENGTH) );
	AddProperty( "Z", new VProperty(5.0, LENGTH ) );
	AddProperty( "Diameter", new VProperty(2.5, LENGTH ) );
	AddProperty( "Height", new VProperty(10.0, LENGTH ) );
}

wxString VCylinderObject::GetTypeName()
{
	return "Cylinder";
}

VObject *VCylinderObject::Duplicate()
{
	VCylinderObject *p = new VCylinderObject;
	p->Copy( this );
	return p;
}

void VCylinderObject::BuildModel( s3d::scene &sc )
{
	double x = Property("X").GetDouble();
	double y = Property("Y").GetDouble();
	double z = Property("Z").GetDouble();
	double diam = Property("Diameter").GetDouble();
	double height = Property("Height").GetDouble();

	sc.reset();
	sc.colors( s3d::rgba(64, 64, 64, 155), s3d::rgba(64, 64, 64, 155) );
	sc.conical( GetId(), x, y, z-height/2, height, diam/2, diam/2 );
}

void VCylinderObject::SetupHandles( VPlaneType plane )
{
	double x = Property("X").GetDouble();
	double diam = Property("Diameter").GetDouble();

	if (plane == PLANE_XY)
	{
		double y = Property("Y").GetDouble();

		AddHandle( HH_MOVE, x,y);
		AddHandle( HH_DIAM, x+diam/2,y,wxCURSOR_SIZEWE );
	}
	else if (plane == PLANE_XZ)
	{
		double z = Property("Z").GetDouble();
		double height = Property("Height").GetDouble();

		AddHandle( HH_MOVE, x, z);
		AddHandle( HH_DIAM, x+diam/2,z,wxCURSOR_SIZEWE );
		AddHandle( HH_BOTTOM, x, z-height/2, wxCURSOR_SIZENS );
		AddHandle( HH_TOP, x, z+height/2, wxCURSOR_SIZENS );
	}
}

bool VCylinderObject::OnHandleMoved(VHandle *h, VPlaneType plane )
{
	int id = h->GetId();
	double xc = Property("X").GetDouble();
	double zc = Property("Z").GetDouble();
	double x = h->GetX();
	double yz = h->GetYZ();
	
	if (id == HH_DIAM)
		{
			Property("Diameter").Set(2*(x-xc) );
		}
	else if (plane == PLANE_XY)
	{
		if (id == HH_MOVE)
		{
			Property("X").Set(x);
			Property("Y").Set(yz);
		}
	}
	else if (plane == PLANE_XZ )
	{
		if (id == HH_MOVE)
		{
			Property("X").Set(x);
			Property("Z").Set(yz);
		}
		else if (id == HH_BOTTOM )
		{
			double height = Property("Height").GetDouble();
			double dz = h->GetDeltaYZ();
			Property("Height").Set(height - dz);
			Property("Z").Set(zc + 0.5*dz);
		}
		else if ( id == HH_TOP )
		{
			double height = Property("Height").GetDouble();
			double dz = h->GetDeltaYZ();
			Property("Height").Set(height + dz);
			Property("Z").Set(zc + 0.5*dz);
		}
	}

	h->SetOriginalPos(x,yz);
	return true;
}

void VCylinderObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane )
{
	double xc = Property("X").GetDouble();
	double diam = Property("Diameter").GetDouble();

	if ( plane == PLANE_XY )
	{
		double yc = Property("Y").GetDouble();
		dc.Circ( xc, yc, diam/2 );
	}
	else if ( plane == PLANE_XZ )
	{
		double height = Property("Height").GetDouble();
		double zc = Property("Z").GetDouble();
		dc.Rect(xc-diam/2,zc-height/2,diam,height);
	}
}

bool VCylinderObject::IsWithin( double x, double yz, VPlaneType plane )
{
	double xc = Property("X").GetDouble();
	double diam = Property("Diameter").GetDouble();

	if ( plane == PLANE_XY )
	{
		double yc = Property("Y").GetDouble();
		return ( sqrt( (x-xc)*(x-xc) + (yz-yc)*(yz-yc) ) < diam/2 );
	}
	else if ( plane == PLANE_XZ)
	{
		double zc = Property("Z").GetDouble();
		double height = Property("Height").GetDouble();
		return ( (x >= xc - diam/2) && (x <= xc + diam/2) && 
				 (yz >= zc - height/2) && (yz <= zc + height/2) );
	}
	return false;
}

/* *******************************************************************************************************
VRoofObject
******************************************************************************************************** */
VRoofObject::VRoofObject()
{
	AddProperty( "Center X", new VProperty(0.0, LENGTH ) );
	AddProperty( "Center Y", new VProperty(0.0, LENGTH ) );
	AddProperty( "Bottom Z", new VProperty(0.0, LENGTH ) );
	AddProperty( "Width", new VProperty( 10.0, LENGTH ) );
	AddProperty( "Length", new VProperty( 20.0, LENGTH ) );
	AddProperty( "Height", new VProperty(5.0, LENGTH ) );
	AddProperty( "Angle XY", new VProperty( 0.0 ) );
	AddProperty( "Pitch Angle 1", new VProperty( 45.0 ) );
	AddProperty( "Pitch Angle 2", new VProperty( 45.0 ) );
}

wxString VRoofObject::GetTypeName()
{
	return "Roof";
}

VObject* VRoofObject::Duplicate()
{
	VRoofObject *p = new VRoofObject;
	p->Copy( this );
	return p;
}

void VRoofObject::BuildModel( s3d::scene & sc)
{
	double width = Property("Width").GetDouble();
	double length = Property("Length").GetDouble();
	double height = Property("Height").GetDouble();
	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double zb = Property("Bottom Z").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();
	double pitch1 = Property("Pitch Angle 1").GetDouble();
	double pitch2 = Property("Pitch Angle 2").GetDouble();

	sc.colors( s3d::rgba(102,51, 0, 155), s3d::rgba(102, 51, 0, 155) );
	sc.roof( GetId(), xc, yc, zb, width,length, height, pitch1, pitch2, angle_xy );
}

void VRoofObject::SetupHandles( VPlaneType plane)
{
	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double width = Property("Width").GetDouble();
	double length = Property("Length").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();

	double x[5], y[5];
	x[0] = xc + width/2; y[0] = yc;    //HH_RIGHT
	x[1] = xc; y[1] = yc+length/2;      // HH_TOP
	x[2] = xc + width/4; y[2] = yc;      // HH_ROTATE_XY
	x[3] = xc - width/2; y[3] = yc;    //HH_LEFT
	x[4] = xc ; y[4] = yc - length/2;   // HH_BOTTOM
	s3d::rotate2dxz(xc,yc,x,y,angle_xy,5);

	if ( plane == PLANE_XY )
	{
		AddHandle( HH_MOVE, xc, yc );
		AddHandle( HH_RIGHT,x[0], y[0], wxCURSOR_SIZEWE );
		AddHandle( HH_TOP, x[1], y[1], wxCURSOR_SIZENS );
		AddHandle( HH_ROTATE_XY, x[2], y[2], wxCURSOR_BULLSEYE );
		AddHandle( HH_LEFT, x[3], y[3], wxCURSOR_SIZEWE );
		AddHandle( HH_BOTTOM, x[4], y[4], wxCURSOR_SIZENS );

	}
	else if ( plane == PLANE_XZ )
	{
		double zb = Property("Bottom Z").GetDouble();
		double height = Property("Height").GetDouble();

		AddHandle( HH_MOVE, xc, zb );
		AddHandle( HH_TOP, xc, zb + height, wxCURSOR_SIZENS );
	}

}

bool VRoofObject::OnHandleMoved( VHandle *h, VPlaneType plane)
{
	double width = Property("Width").GetDouble();
	double length = Property("Length").GetDouble();
	double height = Property("Height").GetDouble();
	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double zb = Property("Bottom Z").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();

	double xh[5], yh[5];
	xh[0] = xc + width/2; yh[0] = yc;    //HH_RIGHT
	xh[1] = xc; yh[1] = yc+length/2;      // HH_TOP
	xh[2] = xc+width/4; yh[2] = yc;      // HH_ROTATE_XY
	xh[3] = xc - width/2; yh[3] = yc;    //HH_LEFT
	xh[4] = xc ; yh[4] = yc - length/2;   // HH_BOTTOM
	s3d::rotate2dxz(xc,yc,xh,yh,angle_xy,5);

	double x = h->GetX();
	double yz = h->GetYZ();

	double id = h->GetId();
	double dx = x - xc;
	double dy = yz - yc;
	double dz = yz - zb;
	double d = sqrt(dx*dx + dy*dy);


	if (id == HH_MOVE)
	{
		Property("Center X").Set(x);
		if (plane == PLANE_XY) Property("Center Y").Set(yz);
		else Property("Bottom Z").Set(yz);
	}
	else if (id == HH_TOP)
	{
		// compute new center and height/depth
		if ( plane == PLANE_XY ) 
		{
			double d0 = sqrt( pow(xh[1]-xc,2) + pow(yh[1]-yc,2) );
		    double e_x = (xh[1] - xc)/d0;
	        double e_y = (yh[1] - yc)/d0;

			double dl = d - length/2;
			double lP = length+ dl;
			double xcP = xc + 0.5*dl*e_x;
			double ycP = yc + 0.5*dl*e_y;

			Property("Length").Set(lP);
			Property("Center X").Set(xcP);
			Property("Center Y").Set(ycP);
		}
		else
		{
			double dh = fabs(dz) - height;
			double dP = height + dh;
			Property("Height").Set(dP);
		}
	}
	else if ( id == HH_RIGHT) 
	{
		// compute new center and width
		double d = sqrt(dx*dx + dy*dy);
		double dw = d - width/2;
		double wP = width + dw;
		double xcP = xc + (dw*cos(angle_xy*M_PI/180)/2);
		double ycP = yc + (dw*sin(angle_xy*M_PI/180)/2);

		// update properties
		Property("Width").Set(wP);
		Property("Center X").Set(xcP);
		Property("Center Y").Set(ycP);

		return true;
	
	}
	else if (id == HH_BOTTOM)
	{
			double d0 = sqrt( pow(xh[4]-xc,2) + pow(yh[4]-yc,2) );
		    double e_x = (xh[4] - xc)/d0;
	        double e_y = (yh[4] - yc)/d0;

			double dl = d - length/2;
			double lP = length+ dl;
			double xcP = xc + 0.5*dl*e_x;
			double ycP = yc + 0.5*dl*e_y;

			Property("Length").Set(lP);
			Property("Center X").Set(xcP);
			Property("Center Y").Set(ycP);
	}
	else if (id == HH_LEFT)
	{
		double d0 = sqrt( pow(xh[3]-xc,2) + pow(yh[3]-yc,2) );
		double e_x = (xh[3] - xc)/d0;
		double e_y = (yh[3] - yc)/d0;

		double dw = d - width/2;
		double wP = width + dw;
		double xcP = xc + 0.5*dw*e_x;
		double ycP = yc + 0.5*dw*e_y;

		Property("Width").Set(wP);
		Property("Center X").Set(xcP);
		Property("Center Y").Set(ycP);
	}
	else if ( id == HH_ROTATE_XY ) 
	{
		double angle = atan2(yz-yc, x - xc)*(180/M_PI);
		Property("Angle XY").Set(angle /*deg*/);
	}
	h->SetOriginalPos(x,yz);
	return false;
}

void VRoofObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane)
{
	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double width = Property("Width").GetDouble();
	double length = Property("Length").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();

	double xr[4], yr[4];

	if (plane == PLANE_XY)
	{	
		s3d::get_rotated_box_points(xc,yc,width,length,angle_xy,xr,yr);
		dc.Poly(xr,yr,4);
	}
	else if (plane == PLANE_XZ)
	{
		double xd[4], zd[4];
		GetXZPoints(xd,zd);
		dc.Poly(xd,zd,4);
	}
}

bool VRoofObject::IsWithin( double x, double yz, VPlaneType plane)
{
	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double width = Property("Width").GetDouble();
	double length = Property("Length").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();

	if (plane == PLANE_XY)
	{
		double xr[4], yr[4];
		s3d::get_rotated_box_points(xc,yc,width,length,angle_xy,xr,yr);
		return s3d::inpoly(xr,yr,4,x,yz);
	}
	else if (plane == PLANE_XZ)
	{
		double xd[4], zd[4];
		GetXZPoints(xd,zd);
		return s3d::inpoly(xd,zd,4,x,yz);
	
	}
	return false;
}
void VRoofObject::GetXZPoints(double xd[4], double zd[4])
{
	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double width = Property("Width").GetDouble();
	double length = Property("Length").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();
	double zb = Property("Bottom Z").GetDouble();
	double height = Property("Height").GetDouble();
	double pitch1 = Property("Pitch Angle 1").GetDouble();
	double pitch2 = Property("Pitch Angle 2").GetDouble();

	double xr[4], yr[4];
	s3d::get_rotated_box_points(xc,yc,width,length,0.0,xr,yr);

	// compute xy pitch points
	double xp[2], yp[2];
	xp[0] = 0.5*(xr[0] + xr[2]);
	xp[1] = xp[0];
	yp[0] = yr[0] + height/(tan(pitch1*DTOR) );
	yp[1] = yr[1] - height/(tan(pitch2*DTOR) );
	s3d::rotate2dxz(xc,yc,xp,yp,angle_xy,2);

	// draw points
	s3d::rotate2dxz(xc, yc, xr, yr, angle_xy, 4);

	if (sin(angle_xy*DTOR) >= 0. && cos(angle_xy*DTOR) >= 0.0 )
	{
		xd[0] = xr[0]; zd[0] = zb;
		xd[1] = xp[0]; zd[1] = zb + height;
		xd[2] = xp[1]; zd[2] = zb +height;
		xd[3] = xr[2]; zd[0] = zb;
	}
	else if (sin(angle_xy*DTOR) >=0.0 && cos(angle_xy*DTOR <0. ) )
	{
		xd[0] = xr[3]; zd[0] = zb;
		xd[1] = xp[0]; zd[1] = zb + height;
		xd[2] = xp[1]; zd[2] = zb +height;
		xd[3] = xr[1]; zd[0] = zb;
	}
	else if (sin(angle_xy*DTOR) < 0.0 && cos(angle_xy*DTOR) < 0. )
	{
		xd[0] = xr[2]; zd[0] = zb;
		xd[1] = xp[1]; zd[1] = zb + height;
		xd[2] = xp[0]; zd[2] = zb +height;
		xd[3] = xr[0]; zd[0] = zb;
	}
	else
	{
		xd[0] = xr[1]; zd[0] = zb;
		xd[1] = xp[1]; zd[1] = zb + height;
		xd[2] = xp[0]; zd[2] = zb +height;
		xd[3] = xr[3]; zd[0] = zb;
	}
}
