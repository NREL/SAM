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

VProperty::VProperty( double dp )
{
	Init();
	m_type = DOUBLE;
	m_doubleVal = dp;
}

VProperty::VProperty( bool bp )
{
	Init();
	m_type = BOOLEAN;
	m_boolVal = bp;
}

VProperty::VProperty( int ip )
{
	Init();
	m_type = INTEGER;
	m_intVal = ip;
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
	m_type = INVALID;
	m_pReference = 0;
	m_doubleVal = 0.0;
	m_intVal = 0;
	m_boolVal = false;
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


VTreeObject::VTreeObject()
{
	AddProperty( "X", new VProperty( 0.0 ) );
	AddProperty( "Y", new VProperty( 0.0 ) );
	AddProperty( "Diameter", new VProperty( 20.0 ) );
	AddProperty( "Top Diameter", new VProperty( 5.0) );
	AddProperty( "Overall Height", new VProperty( 50.0 ) );
	AddProperty( "Trunk Height", new VProperty( 10.0 ) );
}

wxString VTreeObject::GetTypeName()
{
	return "Tree";
}

VObject *VTreeObject::Duplicate()
{
	VTreeObject *tree = new VTreeObject;
	tree->Copy( this );
	return tree;
}

void VTreeObject::BuildModel( s3d::scene &sc )
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

void VTreeObject::SetupHandles( VPlaneType plane )
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

bool VTreeObject::OnHandleMoved( VHandle *h, VPlaneType plane )
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

void VTreeObject::GetXZPoints( double x[m_nPoints], double z[m_nPoints] )
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

void VTreeObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane )
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

bool VTreeObject::IsWithin( double x, double y, VPlaneType plane )
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
	AddProperty( "Center X", new VProperty(0.0) );
	AddProperty( "Center Y", new VProperty(0.0) );
	AddProperty( "Center Z", new VProperty(2.5) );
	AddProperty( "Width", new VProperty(10.0) );
	AddProperty( "Height", new VProperty(5.0) );
	AddProperty( "Depth", new VProperty(5.0) );
	AddProperty( "Angle XY", new VProperty(0.0 ) );
	AddProperty( "Color", new VProperty( wxColour(155,0,0,144) ) );
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
	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double zc = Property("Center Z").GetDouble();
	double w = Property("Width").GetDouble();
	double h = Property("Height").GetDouble();
	double d = Property("Depth").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();
	
	wxColour cc = Property("Color").GetColour();
	s3d::rgba scc1( cc.Red(), cc.Green(), cc.Blue(), cc.Alpha() );
	s3d::rgba scc2( cc.Red(), cc.Green(), cc.Blue(), 255 );
	sc.reset();
	sc.colors( scc1, scc2 );
	sc.type( s3d::scene::OBSTRUCTION );
	sc.box( GetId(), xc, yc, angle_xy, w, h, zc-d/2, d );

}

void VBoxObject::SetupHandles( VPlaneType plane )
{

	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double zc = Property("Center Z").GetDouble();
	double w = Property("Width").GetDouble();
	double h = Property("Height").GetDouble();
	double d = Property("Depth").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();

	double x[5], y[5];
	x[0] = xc + w/2; y[0] = yc;    //HH_RIGHT
	x[1] = xc; y[1] = yc+h/2;      // HH_TOP
	x[2] = xc+w/4; y[2] = yc;      // HH_ROTATE_XY
	x[3] = xc - w/2; y[3] = yc;    //HH_LEFT
	x[4] = xc ; y[4] = yc - h/2;   // HH_BOTTOM
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
		AddHandle( HH_MOVE, xc, zc );
		AddHandle( HH_TOP, xc, zc + d/2, wxCURSOR_SIZENS );
		AddHandle( HH_BOTTOM, xc, zc - d/2, wxCURSOR_SIZENS);
	}
}

bool VBoxObject::OnHandleMoved( VHandle *h, VPlaneType plane )
{
	// extract previous properties
	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double zc = Property("Center Z").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();
	double width = Property("Width").GetDouble();
	double height = Property("Height").GetDouble();
	double depth = Property("Depth").GetDouble();

	// get current properties
	int id = h->GetId();
	double x = h->GetX();
	double yz = h->GetYZ();
	double dx = x - xc;
	double dy = yz - yc;
	double dz = yz - zc;

	if ( id == HH_MOVE )
	{
		Property("Center X").Set( x );
		if ( plane == PLANE_XY ) Property("Center Y").Set( yz);
		else Property("Center Z").Set(yz );
		return true;
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
	else if ( id == HH_TOP )
	{
		// compute new center and height/depth
		if ( plane == PLANE_XY ) 
		{
			double d = sqrt(dx*dx + dy*dy);
			double dh = d - height/2;
			double hP = height + dh;
			double alpha = (angle_xy+90)*(M_PI/180);
			double xcP = xc + 0.5*dh*cos(alpha);
			double ycP = yc + 0.5*dh*sin(alpha);

			Property("Height").Set(hP);
			Property("Center X").Set(xcP);
			Property("Center Y").Set(ycP);
		}
		else
		{
			double dd = fabs(dz) - depth/2;
			double dP = depth + dd;
			double zcP = zc + 0.5*dd;

			Property("Depth").Set(dP);
			Property("Center Z").Set(zcP);
		}

		return true;
	}
	else if ( id == HH_ROTATE_XY ) 
	{
		double angle_xy = (180/M_PI)*atan2(yz-yc,x-xc);
		Property("Angle XY").Set(angle_xy /*deg*/);
	}

	else if ( id == HH_LEFT) 
	{
		// compute new center and width
		double d = sqrt(dx*dx + dy*dy);
		double dw = d - width/2;
		double wP = width + dw;
		double xcP = xc - (dw*cos(angle_xy*M_PI/180)/2);
		double ycP = yc - (dw*sin(angle_xy*M_PI/180)/2);

		// update properties
		Property("Width").Set(wP);
		Property("Center X").Set(xcP);
		Property("Center Y").Set(ycP);

		return true;
	}
	else if ( id == HH_BOTTOM )
	{
		// compute new center and height/depth
		if ( plane == PLANE_XY ) 
		{
			double d = sqrt(dx*dx + dy*dy);
			double dh = d - height/2;
			double hP = height + dh;
			double alpha = (angle_xy+90)*(M_PI/180);
			double xcP = xc - 0.5*dh*cos(alpha);
			double ycP = yc - 0.5*dh*sin(alpha);

			Property("Height").Set(hP);
			Property("Center X").Set(xcP);
			Property("Center Y").Set(ycP);
		}
		else
		{
			double dd = fabs(dz) - depth/2;
			double dP = depth + dd;
			double zcP = zc - 0.5*dd;

			Property("Depth").Set(dP);
			Property("Center Z").Set(zcP);
		}

		return true;
	}
	return false;
}

void VBoxObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane )
{
	double xc = Property("Center X").GetDouble();
	double width = Property("Width").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double height = Property("Height").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();

	if ( plane == PLANE_XY )
	{
		double x[4], y[4];
		s3d::get_rotated_box_points( xc, yc, width, height, angle_xy,x, y);
		dc.Poly(x,y,4);
	}
	else if ( plane == PLANE_XZ )
	{

		double zc = Property("Center Z").GetDouble();
		double depth = Property("Depth").GetDouble();
		double minDim = std::min(width,height);
		double maxDim = std::max(width,height);
		double dDim = maxDim-minDim;
		double xDim = minDim + fabs(cos(angle_xy*M_PI/180))*dDim;

		dc.Rect(xc-xDim/2, zc-depth/2,xDim,depth);
	}
}

bool VBoxObject::IsWithin( double x, double yz, VPlaneType plane )
{
	double xc = Property("Center X").GetDouble();
	double w = Property("Width").GetDouble();

	if ( plane == PLANE_XY )
	{
		double xx[4]; double yy[4];
		double yc = Property("Center Y").GetDouble();
		double h = Property("Height").GetDouble();
		double angle_xy = Property("Angle XY").GetDouble();

		s3d::get_rotated_box_points( xc, yc,w, h,angle_xy, xx,yy );
		return s3d::inpoly( xx, yy, 4, x, yz );

	}
	else if ( plane == PLANE_XZ )
	{
		double zc = Property("Center Z").GetDouble();
		double d = Property("Depth").GetDouble();

		double xx = xc-w/2;
		double zz = zc-d/2;

		return ( x >= xx && x <= xx+w
			&& yz >= zz && yz <= zz+d );
	}

	return false;
}



VPVArray::VPVArray()
{
	AddProperty( "X", new VProperty(0.0 ) );
	AddProperty( "Y", new VProperty(0.0 ) );
	AddProperty( "Z", new VProperty(0.0 ) );
	AddProperty( "Width", new VProperty( 5.0 ) );
	AddProperty( "Height", new VProperty( 10.0 ) );
	AddProperty( "Azimuth", new VProperty(180.0 ) );
	AddProperty( "Tilt", new VProperty(30.0) );
}

wxString VPVArray::GetTypeName()
{
	return "PVArray";
}

VObject *VPVArray::Duplicate()
{
	VPVArray *p = new VPVArray;
	p->Copy( this );
	return p;
}

void VPVArray::SetupHandles( VPlaneType plane )
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

bool VPVArray::OnHandleMoved( VHandle *h, VPlaneType plane )
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

void VPVArray::BuildModel( s3d::scene &sc )
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

void VPVArray::DrawOnPlane( VRenderer2D &dc, VPlaneType plane )
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

bool VPVArray::IsWithin( double xt, double yt, VPlaneType plane )
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

void VPVArray::TiltAndAzimuth(double n_points, double tilt,double azimuth,
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

void VPVArray::SetVertices(double x[4], double y[4], double z[4])
{
	for (int i = 0; i < 4; i++) 
	{
		m_x[i] = x[i];
		m_y[i] = y[i];
		m_z[i] = z[i];
	}
}

void VPVArray::GetVertices(double x[4], double y[4], double z[4])
{
	for (int i = 0; i < 4; i++) 
	{
		x[i] = m_x[i];
		y[i] = m_y[i];
		z[i] = m_z[i];
	}
}

void VPVArray::GetDistances(int face, double distances[4])
{
	int high = face+1; int low = face;
	if (high > 3) high = 0;

	distances[0] = m_x[high] - m_x[low];
	distances[1] = m_y[high] - m_y[low];
	distances[2] = m_z[high] - m_z[low];
	distances[3] = sqrt(distances[0]*distances[0] + distances[1]*distances[1] + distances[2]*distances[2]);
}

void VPVArray::UnitVectorNormal(int face, double unit_vector[3])
{
	double distances[4];

	// the normal vector is the tangent vector of the previous face
	face -= 1; 
	if (face < 0) face = 3;
	GetDistances(face,distances);

	// compute unit vector
	for (int i = 0; i < 3; i++) unit_vector[i] = distances[i]/distances[3];
}
void VPVArray::UpdateOnMove( VHandle *h, VPlaneType plane )
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
double VPVArray::UpdateOnStretch(int face, double length, VHandle *h )
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

VPoleObject::VPoleObject()
{
	AddProperty( "X", new VProperty(0.0 ) );
	AddProperty( "Y", new VProperty(0.0 ) );
	AddProperty( "Z", new VProperty(5.0 ) );
	AddProperty( "Diameter", new VProperty(2.5 ) );
	AddProperty( "Height", new VProperty(10.0 ) );
}

wxString VPoleObject::GetTypeName()
{
	return "Pole";
}

VObject *VPoleObject::Duplicate()
{
	VPoleObject *p = new VPoleObject;
	p->Copy( this );
	return p;
}

void VPoleObject::BuildModel( s3d::scene &sc )
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

void VPoleObject::SetupHandles( VPlaneType plane )
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

bool VPoleObject::OnHandleMoved(VHandle *h, VPlaneType plane )
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

void VPoleObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane )
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

bool VPoleObject::IsWithin( double x, double yz, VPlaneType plane )
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
VHedgeObject
******************************************************************************************************** */
VHedgeObject::VHedgeObject()
{

	double xc = 0.0;
	double yc = 0.0;
	double width = 10.0;
	double length = 5.0;
	double angle_xy = 45.;

	// initalize left, right handles
	double xx[2], yy[2];
	xx[0] = xc - width/2; yy[0] = yc; // left
	xx[1] = xc + width/2; yy[1] = yc; // right
	s3d::rotate2dxz(xc, yc, xx, yy, angle_xy, 2);

	RotateCenter(1); // initially rotate about center

	AddProperty( "Bottom Z", new VProperty(0.0 ) );
	AddProperty( "Width", new VProperty( width ) );
	AddProperty( "Length", new VProperty( length ) );
	AddProperty( "Depth", new VProperty(5.0 ) );
	AddProperty("Left X", new VProperty(xx[0]) );
	AddProperty("Left Y", new VProperty(yy[0]) );
	AddProperty("Right X", new VProperty(xx[1]) );
	AddProperty("Right Y", new VProperty(yy[1]) );
	AddProperty( "Angle XY", new VProperty( angle_xy ) );
	
	// Store
	StoreHandles();

}

wxString VHedgeObject::GetTypeName()
{
	return "Hedge";
}

VObject* VHedgeObject::Duplicate()
{
	VHedgeObject *p = new VHedgeObject;
	p->Copy( this );
	return p;
}

void VHedgeObject::BuildModel( s3d::scene & sc)
{
	double zb = Property("Bottom Z").GetDouble();
	double length = Property("Length").GetDouble();
	double width = Property("Width").GetDouble();
	double depth = Property("Depth").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();
	double xleft = Property("Left X").GetDouble();
	double yleft = Property("Left Y").GetDouble();
	double xright = Property("Right X").GetDouble();
	double yright = Property("Right Y").GetDouble();

	// catch for if user manually enters width
	double distance = sqrt( pow(xright-xleft, 2) + pow(yright - yleft, 2) );
	double dw = width - distance;
	double angle_check = atan2( yright - yleft, xright - xleft)*180/M_PI;
	if ( (fabs(dw) > 0.01) && (fabs(angle_check - angle_xy) < 0.01) )
	{
		RotateCenter(0);

		double e_x = (xright - xleft)/distance;
		double e_y = (yright - yleft)/distance;

		xright += e_x*dw;
		yright += e_y*dw;

		Property("Right X").Set(xright);
		Property("Right Y").Set(yright);
	}
	// catch for if user manually inputs positions or angle
	else if (fabs(angle_check-angle_xy) > 0.01)
	{	
		double x_old[2], y_old[2];
		GetHandles(x_old, y_old);

		// changed Left X or Left Y
		if ( (fabs(x_old[0] - xleft) > 0.01) || (fabs(y_old[0] - yleft) > 0.01) )
		{
			// set rotate center as HH_RIGHT_MOVE
			RotateCenter(2);

			// update width
			width = sqrt( pow(xleft-xright,2) + pow(yleft-yright,2) );
			Property("Width").Set(width);

			// update angle
			Property("Angle XY").Set(angle_check);
			angle_xy = angle_check;
		}
		// changed Right X or Right Y
		else if ( (fabs(x_old[1] - xright) > 0.01) || (fabs(y_old[1] - yright) > 0.01) )
		{

			// set rotate center as HH_LEFT_MOVE
			RotateCenter(0);

			// update properties
			width = sqrt( pow(xright-xleft,2) + pow(yright-yleft,2) );
			Property("Width").Set(width);

			// update angle
			Property("Angle XY").Set(angle_check);
			angle_xy = angle_check;
		}
		// changed angle_xy
		else
		{
			RotateCenter(0);

			xright = xleft + width*cos(angle_xy*DTOR);
			yright = yleft + width*sin(angle_xy*DTOR);

			Property("Right X").Set(xright);
			Property("Right Y").Set(yright);

		}
	}

	StoreHandles();

	
	// extract unrotated end positions
	double xr,yr;
	int rotateCenter = ReturnRotateCenter( &xr, &yr);

	// set initial unrotated angles
	float start1 = 180.0 - angle_xy; float start2 = 180.0-angle_xy;
	float end1 = 360.0 -angle_xy; float end2 = 0.0 - angle_xy;
	
	// draw
	sc.reset();
	sc.colors( s3d::rgba(25,51, 0, 155), s3d::rgba(25, 51, 0, 155) );
	sc.cylinder( GetId(), xleft, yleft,zb, depth, length/2,
	 			start1, end1, angle_xy, 18);
	sc.colors( s3d::rgba(25, 51, 0, 155), s3d::rgba(25, 51, 0, 255) );
	sc.box( GetId(),xr , yr, angle_xy, width, length,zb,depth, 
		s3d::FRONT|s3d::BACK|s3d::TOP|s3d::BOTTOM, rotateCenter);
	sc.colors( s3d::rgba(25, 51, 0, 155), s3d::rgba(25, 51, 0, 155) );
	sc.cylinder( GetId(), xright, yright,zb, depth, length/2,
	   			start2, end2, angle_xy, 18);

}

void VHedgeObject::SetupHandles( VPlaneType plane )
{
	double width = Property("Width").GetDouble();
	double length = Property("Length").GetDouble();

	double xx[3], yy[3];
	xx[0] = Property("Left X").GetDouble();
	yy[0] = Property("Left Y").GetDouble();
	xx[1] = Property("Right X").GetDouble();
	yy[1] = Property("Right Y").GetDouble();

	// compute location of HH_DIAM
	double dx = (xx[1] - xx[0])/width;
	double dy = (yy[1] - yy[0])/width;
	xx[2] = xx[1] + dy*length/2;
	yy[2] = yy[1] - dx*length/2;

	if (plane == PLANE_XY)
	{
		AddHandle( HH_LEFT_MOVE, xx[0],yy[0]);
		AddHandle( HH_RIGHT_MOVE, xx[1],yy[1]);
		AddHandle( HH_DIAM, xx[2],yy[2],  wxCURSOR_SIZENS);
	}
	else if (plane == PLANE_XZ)
	{
		double zb = Property("Bottom Z").GetDouble();
		double depth = Property("Depth").GetDouble();

		AddHandle( HH_Z_MOVE, 0.5*(xx[0]+xx[1]), zb + depth/2);
		AddHandle( HH_TOP, 0.5*(xx[0]+ xx[1]), zb + depth, wxCURSOR_SIZENS);
		AddHandle( HH_BOTTOM, 0.5*(xx[0] + xx[1]), zb, wxCURSOR_SIZENS);
		
	}
	
}
bool VHedgeObject::OnHandleMoved( VHandle *h, VPlaneType plane )
{
	
	int id = h->GetId();
	double x = h->GetX();
	double y = h->GetYZ();

	if (plane == PLANE_XY)
	{
		if (id == HH_LEFT_MOVE)
		{
			double xr = Property("Right X").GetDouble();
			double yr = Property("Right Y").GetDouble();

			// set rotate center as HH_RIGHT_MOVE
			RotateCenter(2);

			// update properties
			Property("Left X").Set(x);
			Property("Left Y").Set(y);
			double width = sqrt( pow(x-xr,2) + pow(y-yr,2) );
			Property("Width").Set(width);

			// update angle
			double angle_xy = atan2( yr - y, xr - x ) * 180/M_PI;
			Property("Angle XY").Set(angle_xy);
		}
		
		else if (id == HH_RIGHT_MOVE)
		{
			double xl = Property("Left X").GetDouble();
			double yl = Property("Left Y").GetDouble();

			// set rotate center as HH_LEFT_MOVE
			RotateCenter(0);

			// update properties
			Property("Right X").Set(x);
			Property("Right Y").Set(y);
			double width = sqrt( pow(x-xl,2) + pow(y-yl,2) );
			Property("Width").Set(width);

			// update angle
			double angle_xy = atan2( y - yl, x - xl ) * 180/M_PI;
			Property("Angle XY").Set(angle_xy);
		}
		else if (id == HH_DIAM)
		{
			double xr = Property("Right X").GetDouble();
			double yr = Property("Right Y").GetDouble();
			double length = 2*sqrt( pow(x-xr,2) + pow(y-yr,2) );
			Property("Length").Set(length);
		}
		
	}
	
	else if (plane == PLANE_XZ)
	{
		double depth = Property("Depth").GetDouble();
		double zb = Property("Bottom Z").GetDouble();


		if (id == HH_Z_MOVE)
		{	
			double dx = h->GetDeltaX();
			double dz = h->GetDeltaYZ();

			double xl = Property("Left X").GetDouble();
			double xr = Property("Right X").GetDouble();

			Property("Left X").Set( xl + dx );
			Property("Right X").Set( xr + dx );
			Property("Bottom Z").Set(zb + dz );
		}
		else if (id == HH_TOP ) Property("Depth").Set(y - zb);
		else if (id == HH_BOTTOM )
		{
			double dz = zb - y;
			Property("Bottom Z").Set(y);
			Property("Depth").Set(depth += dz);
		}
			
	}
	h->SetOriginalPos(x,y);
	StoreHandles();
	
	return true;
}

void VHedgeObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane)
{
	
	double xp[38], yp[38];
	ReturnXYPoints(xp, yp);

	if (plane == PLANE_XY)
	{
		dc.Poly(xp,yp,38);
	}
	else if (plane == PLANE_XZ)
	{
		double zb = Property("Bottom Z").GetDouble();
		double depth = Property("Depth").GetDouble();
		double xmin = xp[0];
		double xmax = xp[0];

		for (int i = 1; i < 38; i++)
		{
			if (xp[i] < xmin) xmin = xp[i];
			if (xp[i] > xmax) xmax = xp[i];
		}
		dc.Rect(xmin, zb,xmax-xmin,depth);
	}
	
}

bool VHedgeObject::IsWithin( double xt, double yt, VPlaneType plane)
{
	double xp[38], yp[38];
	ReturnXYPoints(xp,yp);

	if (plane == PLANE_XY)
	{
		return s3d::inpoly(xp,yp,38,xt,yt);
	}
	else if (plane == PLANE_XZ)
	{
		double zb = Property("Bottom Z").GetDouble();
		double depth = Property("Depth").GetDouble();
		double xmin = xp[0];
		double xmax = xp[0];

		for (int i = 1; i < 38; i++)
		{
			if (xp[i] < xmin) xmin = xp[i];
			if (xp[i] > xmax) xmax = xp[i];
		}
		return ( (xt >= xmin) && (xt <= xmax) && 
			     (yt >=zb) && (yt <= zb + depth) );
	}
	
	return false;
}

void VHedgeObject::RotateCenter( int center)
{
	m_rotate = center;
	// 0 - left; 1 - center; 2 - right 
}
int VHedgeObject::ReturnRotateCenter(double *xc, double *yc)
{

	double xl = Property("Left X").GetDouble();
	double yl = Property("Left Y").GetDouble();
	double xr = Property("Right X").GetDouble();
	double yr = Property("Right Y").GetDouble();

	if (m_rotate == 0)
	{
		*xc = xl;
		*yc = yl;
	}
	else if (m_rotate == 1) 
	{
		*xc = 0.5*(xl+xr);
		*yc = 0.5*(yl+yr);
	}
	else if (m_rotate == 2)
	{
		*xc = xr;
		*yc = yr;
	}

	return m_rotate;
}

void VHedgeObject::ReturnXYPoints( double xp[38], double yp[38] )
{
		double length = Property("Length").GetDouble();
		double width = Property("Width").GetDouble();
		double angle_xy = Property("Angle XY").GetDouble();
		double r = length/2;

		// retrieve circle centers
		double xx[2], yy[2];
		xx[0] = Property("Left X").GetDouble();
		yy[0] = Property("Left Y").GetDouble();
		xx[1] = Property("Right X").GetDouble();
		yy[1] = Property("Right Y").GetDouble();		

		double xr, yr; 
		int rotateCenter = ReturnRotateCenter(&xr,&yr);

		// compute start and end angles: rotation from bottom right to top right
		// and then rotation from top left to bottom left
		float start1 = 180.0 - angle_xy; float end1 = 0.0 - angle_xy;
		float start2 = 360.0 - angle_xy; float end2 = 180.0 - angle_xy;

		// rotate box
		double xb[4], yb[4];
		s3d::get_rotated_box_points(xr, yr, width,length,angle_xy,xb,yb,rotateCenter);
		
		// begin polygon
		double step1 = (end1 - start1)/18;
		for (int i = 0; i < 19; i++)
		{
			double angle = start1 + step1*i;
			double sinA = sin(angle*M_PI/180);
			double cosA = cos(angle*M_PI/180);

			xp[i] = xx[1] + r*sinA;
			yp[i] = yy[1] + r*cosA;
		}
		double step2 = (end2 - start2)/18;
		for (int i = 0; i < 19; i++)
		{
			double angle = start2 + step2*i;
			double sinA = sin(angle*M_PI/180);
			double cosA = cos(angle*M_PI/180);

			xp[i+19] = xx[0] + r*sinA;
			yp[i+19] = yy[0] + r*cosA;
		}


}
void VHedgeObject::StoreHandles()
{
	m_x[0] = Property("Left X").GetDouble();
	m_x[1] = Property("Right X").GetDouble();
	m_y[0] = Property("Left Y").GetDouble();
	m_y[1] = Property("Right Y").GetDouble();
}
void VHedgeObject::GetHandles( double x[2], double y[2] )
{
	x[0] = m_x[0];
	x[1] = m_x[1];
	y[0] = m_y[0];
	y[1] = m_y[1];
}

/* *******************************************************************************************************
VRoofObject
******************************************************************************************************** */
VRoofObject::VRoofObject()
{
	AddProperty( "Center X", new VProperty(0.0 ) );
	AddProperty( "Center Y", new VProperty(0.0 ) );
	AddProperty( "Bottom Z", new VProperty(0.0 ) );
	AddProperty( "Width", new VProperty( 10.0 ) );
	AddProperty( "Length", new VProperty( 20.0 ) );
	AddProperty( "Height", new VProperty(5.0 ) );
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




VTriangleObject::VTriangleObject()
{
	AddProperty( "Center X", new VProperty(0.0) );
	AddProperty( "Center Y", new VProperty(0.0) );
	AddProperty( "Center Z", new VProperty(2.5) );
	AddProperty( "Width", new VProperty(10.0) );
	AddProperty( "Height", new VProperty(5.0) );
	AddProperty( "Depth", new VProperty(5.0) );
	AddProperty( "Angle XY", new VProperty(0.0 ) );
	AddProperty( "Color", new VProperty( wxColour(155,0,0,144) ) );
}

wxString VTriangleObject::GetTypeName()
{
	return "Triangle";
}

VObject *VTriangleObject::Duplicate()
{
	VTriangleObject *Triangle = new VTriangleObject;
	Triangle->Copy( this );
	return Triangle;
}

void VTriangleObject::BuildModel( s3d::scene &sc )
{
	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double zc = Property("Center Z").GetDouble();
	double w = Property("Width").GetDouble();
	double h = Property("Height").GetDouble();
	double d = Property("Depth").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();
	
	wxColour cc = Property("Color").GetColour();
	s3d::rgba scc1( cc.Red(), cc.Green(), cc.Blue(), cc.Alpha() );
	s3d::rgba scc2( cc.Red(), cc.Green(), cc.Blue(), 255 );
	sc.reset();
	sc.colors( scc1, scc2 );
	sc.type( s3d::scene::OBSTRUCTION );
	sc.box( GetId(), xc, yc, angle_xy, w, h, zc-d/2, d );

}

void VTriangleObject::SetupHandles( VPlaneType plane )
{

	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double zc = Property("Center Z").GetDouble();
	double w = Property("Width").GetDouble();
	double h = Property("Height").GetDouble();
	double d = Property("Depth").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();

	double x[5], y[5];
	x[0] = xc + w/2; y[0] = yc;    //HH_RIGHT
	x[1] = xc; y[1] = yc+h/2;      // HH_TOP
	x[2] = xc+w/4; y[2] = yc;      // HH_ROTATE_XY
	x[3] = xc - w/2; y[3] = yc;    //HH_LEFT
	x[4] = xc ; y[4] = yc - h/2;   // HH_BOTTOM
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
		AddHandle( HH_MOVE, xc, zc );
		AddHandle( HH_TOP, xc, zc + d/2, wxCURSOR_SIZENS );
		AddHandle( HH_BOTTOM, xc, zc - d/2, wxCURSOR_SIZENS);
	}
}

bool VTriangleObject::OnHandleMoved( VHandle *h, VPlaneType plane )
{
	// extract previous properties
	double xc = Property("Center X").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double zc = Property("Center Z").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();
	double width = Property("Width").GetDouble();
	double height = Property("Height").GetDouble();
	double depth = Property("Depth").GetDouble();

	// get current properties
	int id = h->GetId();
	double x = h->GetX();
	double yz = h->GetYZ();
	double dx = x - xc;
	double dy = yz - yc;
	double dz = yz - zc;

	if ( id == HH_MOVE )
	{
		Property("Center X").Set( x );
		if ( plane == PLANE_XY ) Property("Center Y").Set( yz);
		else Property("Center Z").Set(yz );
		return true;
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
	else if ( id == HH_TOP )
	{
		// compute new center and height/depth
		if ( plane == PLANE_XY ) 
		{
			double d = sqrt(dx*dx + dy*dy);
			double dh = d - height/2;
			double hP = height + dh;
			double alpha = (angle_xy+90)*(M_PI/180);
			double xcP = xc + 0.5*dh*cos(alpha);
			double ycP = yc + 0.5*dh*sin(alpha);

			Property("Height").Set(hP);
			Property("Center X").Set(xcP);
			Property("Center Y").Set(ycP);
		}
		else
		{
			double dd = fabs(dz) - depth/2;
			double dP = depth + dd;
			double zcP = zc + 0.5*dd;

			Property("Depth").Set(dP);
			Property("Center Z").Set(zcP);
		}

		return true;
	}
	else if ( id == HH_ROTATE_XY ) 
	{
		double angle_xy = (180/M_PI)*atan2(yz-yc,x-xc);
		Property("Angle XY").Set(angle_xy /*deg*/);
	}

	else if ( id == HH_LEFT) 
	{
		// compute new center and width
		double d = sqrt(dx*dx + dy*dy);
		double dw = d - width/2;
		double wP = width + dw;
		double xcP = xc - (dw*cos(angle_xy*M_PI/180)/2);
		double ycP = yc - (dw*sin(angle_xy*M_PI/180)/2);

		// update properties
		Property("Width").Set(wP);
		Property("Center X").Set(xcP);
		Property("Center Y").Set(ycP);

		return true;
	}
	else if ( id == HH_BOTTOM )
	{
		// compute new center and height/depth
		if ( plane == PLANE_XY ) 
		{
			double d = sqrt(dx*dx + dy*dy);
			double dh = d - height/2;
			double hP = height + dh;
			double alpha = (angle_xy+90)*(M_PI/180);
			double xcP = xc - 0.5*dh*cos(alpha);
			double ycP = yc - 0.5*dh*sin(alpha);

			Property("Height").Set(hP);
			Property("Center X").Set(xcP);
			Property("Center Y").Set(ycP);
		}
		else
		{
			double dd = fabs(dz) - depth/2;
			double dP = depth + dd;
			double zcP = zc - 0.5*dd;

			Property("Depth").Set(dP);
			Property("Center Z").Set(zcP);
		}

		return true;
	}
	return false;
}

void VTriangleObject::DrawOnPlane( VRenderer2D &dc, VPlaneType plane )
{
	double xc = Property("Center X").GetDouble();
	double width = Property("Width").GetDouble();
	double yc = Property("Center Y").GetDouble();
	double height = Property("Height").GetDouble();
	double angle_xy = Property("Angle XY").GetDouble();

	if ( plane == PLANE_XY )
	{
		double x[4], y[4];
		s3d::get_rotated_box_points( xc, yc, width, height, angle_xy,x, y);
		dc.Poly(x,y,4);
	}
	else if ( plane == PLANE_XZ )
	{

		double zc = Property("Center Z").GetDouble();
		double depth = Property("Depth").GetDouble();
		double minDim = std::min(width,height);
		double maxDim = std::max(width,height);
		double dDim = maxDim-minDim;
		double xDim = minDim + fabs(cos(angle_xy*M_PI/180))*dDim;

		dc.Rect(xc-xDim/2, zc-depth/2,xDim,depth);
	}
}

bool VTriangleObject::IsWithin( double x, double yz, VPlaneType plane )
{
	double xc = Property("Center X").GetDouble();
	double w = Property("Width").GetDouble();

	if ( plane == PLANE_XY )
	{
		double xx[4]; double yy[4];
		double yc = Property("Center Y").GetDouble();
		double h = Property("Height").GetDouble();
		double angle_xy = Property("Angle XY").GetDouble();

		s3d::get_rotated_box_points( xc, yc,w, h,angle_xy, xx,yy );
		return s3d::inpoly( xx, yy, 4, x, yz );

	}
	else if ( plane == PLANE_XZ )
	{
		double zc = Property("Center Z").GetDouble();
		double d = Property("Depth").GetDouble();

		double xx = xc-w/2;
		double zz = zc-d/2;

		return ( x >= xx && x <= xx+w
			&& yz >= zz && yz <= zz+d );
	}

	return false;
}


