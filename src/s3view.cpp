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

#include <algorithm>
#include <numeric>

#include <wx/spinctrl.h>
#include <wx/datstrm.h>
#include <wx/fontenum.h>
#include <wx/dcbuffer.h>
#include <wx/dcgraph.h>
#include <wx/slider.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/statline.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/clrpicker.h>
#include <wx/checklst.h>
#include <wx/statbmp.h>
#include <wx/generic/statbmpg.h>
#include <wx/xml/xml.h>
#include <wx/ffile.h>
#include <wx/wfstream.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>
#include <wx/tglbtn.h>
#include <wx/busyinfo.h>

#include <wx/propgrid/propgrid.h>
#include <wx/propgrid/advprops.h>

#include <wex/numeric.h>
#include <wex/plot/plplotctrl.h>
#include <wex/plot/pllineplot.h>
#include <wex/dview/dvplotctrl.h>
#include <wex/utils.h>
#include <wex/jsonreader.h>
#include <wex/easycurl.h>

//#include <curl/curl.h>

#include "s3view.h"


DEFINE_EVENT_TYPE( wxEVT_VIEW3D_UPDATE_VIEW )
DEFINE_EVENT_TYPE( wxEVT_VIEW3D_UPDATE_OBJECTS )
DEFINE_EVENT_TYPE( wxEVT_VIEW3D_UPDATE_PROPERTIES )
DEFINE_EVENT_TYPE( wxEVT_VIEW3D_UPDATE_SELECTION )

class MovingObjectDC : public VRenderer2D
{
#ifdef VIEW_USE_OVERLAY
	wxDCOverlay m_overlaydc;
#endif
	wxClientDC &m_dc;
	View3D *m_view;
public:
	MovingObjectDC( wxClientDC &dc, View3D *view )
#ifdef VIEW_USE_OVERLAY
		: m_dc(dc), m_overlaydc( view->m_overlay, &dc ), m_view(view)
#else
		: m_dc(dc), m_view(view)
#endif
	{
#ifdef VIEW_USE_OVERLAY
		m_overlaydc.Clear();
		m_dc.SetPen( wxColour( 100, 100, 100 ) );
		m_dc.SetBrush( wxColour( 150, 150, 150, 150 ) );
#else
		m_dc.SetBrush( *wxBLACK_BRUSH );
		m_dc.SetPen( *wxBLACK_PEN );
		m_dc.SetLogicalFunction( wxINVERT );
#endif
	}

	virtual void Poly( double *X, double *YZ, size_t n )
	{
		wxPoint *points = new wxPoint[n];

		for( size_t i=0;i<n;i++ )
		{
			int xs, ys;
			m_view->WorldToScreen( X[i], YZ[i], &xs, &ys );
			points[i].x = xs;
			points[i].y = ys;
		}

		m_dc.DrawPolygon( n, points );
		delete [] points;
	}

	virtual void Rect( double x, double yz, double width, double height )
	{
		int x0, y0, x1, y1;
		m_view->WorldToScreen( x, yz, &x0, &y0 );
		m_view->WorldToScreen( x+width, yz+height, &x1, &y1 );
		m_dc.DrawRectangle( x0, y0, x1-x0, y1-y0 );
	}

	virtual void Circ( double x, double yz, double radius )
	{
		int xc, yc;
		m_view->WorldToScreen( x, yz, &xc, &yc );
		m_dc.DrawCircle( xc, yc, radius*m_view->GetScale() );
	}

};



#define HANDLE_RADIUS 5

BEGIN_EVENT_TABLE( View3D, wxWindow )
	EVT_PAINT( View3D::OnPaint )
	EVT_SIZE( View3D::OnSize )
	EVT_LEFT_DOWN( View3D::OnLeftDown )
	EVT_LEFT_DCLICK( View3D::OnLeftDown )
	EVT_LEFT_UP( View3D::OnLeftUp )
	EVT_MOUSEWHEEL( View3D::OnWheel )
	EVT_MOTION( View3D::OnMotion )
	EVT_KEY_DOWN( View3D::OnKey )
END_EVENT_TABLE()

View3D::View3D( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxWindow( parent, id, pos, size, wxWANTS_CHARS )
{	
	// '' default values for window size ''
	m_winWidth = 400;
	m_winHeight = 300;

	m_sf = 0.0;

	m_nextSelectionIndex = 0;

	m_mpp = 1;

	m_mode = SPIN_VIEW;
	m_origX = m_origY = 0;
	m_origScale = 1;
	m_origXOff = m_origYOff = m_origZOff = 0;
	m_zoomMode = false;
	m_panMode = false;
	m_pressed = false;
	m_eraseNeeded = false;
	m_movingHandle = 0;
	m_gridSpacing = 5;
	m_snapSpacing = 0.25;
	SetBackgroundStyle( wxBG_STYLE_PAINT );
	
	RegisterType( new VActiveSurfaceObject );
	RegisterType( new VBoxObject );
	RegisterType( new VCylinderObject );
	RegisterType( new VRoofObject );
	RegisterType( new VTreeObject );
	//RegisterType( new VConicalTreeObject );

	m_scene.basic_axes_with_ground();
		
	m_lastView[0].azimuth = 143;
	m_lastView[0].altitude = 28;
	m_transform.rotate_azal( 143, 28 );
	m_transform.set_scale( 4.0 );

	Render();
}

View3D::~View3D()
{
	DeleteAll();

	for( size_t i=0;i<m_registeredTypes.size(); i++ )
		delete m_registeredTypes[i];
}

void View3D::CreateStaticDemoScene()
{
	// pv panels (active area)
	m_scene.type( s3d::scene::ACTIVE );
	m_scene.colors( s3d::rgba( 0, 107, 186, 200 ), s3d::rgba( 0, 88, 153 ) );

	m_scene.point( 0, 0, 20 ) ;
	m_scene.point( 0, 15, 20 ) ;
	m_scene.point( 20, 15, 0 ) ;
	m_scene.point( 20, 0, 0 ) ;
	m_scene.poly( 1 );

	m_scene.point( 0, 17, 20 );
	m_scene.point( 0, 32, 20 );
	m_scene.point( 20, 32, 0 );
	m_scene.point( 20, 17, 0 );
	m_scene.poly( 1 );
	
	m_scene.point( 0, 34, 20 );
	m_scene.point( 0, 49, 20 );
	m_scene.point( 20, 49, 0 );
	m_scene.point( 20, 34, 0 );
	m_scene.poly( 1 );

	// build obstructions
	m_scene.type( s3d::scene::OBSTRUCTION );
	

	// pole between panels to test sorting
	wxUint8 alpha = 140;
	m_scene.colors( s3d::rgba(254,0,0, alpha), s3d::rgba(254,0,0, alpha) );
	m_scene.conical( 2, 10, 33, 0, 25, 1, 1 );


	// magenta walls
	m_scene.colors( s3d::rgba(202, 11, 183, alpha), s3d::rgba(202, 11, 183, alpha) );
	m_scene.box( 2, 15, -20, 0, 10, 20, 0, 10 );

	// tree 1, chopped off top.
	m_scene.colors( s3d::rgba(128, 64, 0, alpha), s3d::rgba(128, 64, 0, alpha) );
	m_scene.conical( 2, 50, 50, 0, 15, 3, 3 );	
	m_scene.colors( s3d::rgba(0, 186, 107, alpha ), s3d::rgba(0, 186, 107, alpha ) );
	m_scene.conical( 2, 50, 50, 15, 45, 14, 4 );

	
	// tree 2, pointy top
	m_scene.colors( s3d::rgba(128, 64, 0, alpha), s3d::rgba(128, 64, 0, alpha) );
	m_scene.conical( 2, 50, 0, 0, 25, 4, 4 );
	m_scene.colors( s3d::rgba(0, 186, 107, alpha ), s3d::rgba(0, 186, 107, alpha ) );
	m_scene.conical( 2, 50, 0, 25, 55, 16, 0 );
	
	// upside down blue/red thing
	m_scene.colors( s3d::rgba(0,0,255, alpha), s3d::rgba(255,0,0, alpha) );
	m_scene.conical( 2, 50, 25, 0, 10, 0, 4 );

	Render();
	Refresh();
}

static void linspace( double *v, size_t N, double start, double end )
{
	double step = (end-start)/N;
	for ( size_t i=0;i<N;i++ )
		v[i] = start + i*step;

	v[N-1] = end; // fixup for any numerical rounding in stepping
}


void View3D::Animate( double az, double al, double scale, double xo, double yo, double zo )
{
#define ANIM_STEPS 10
	double az_steps[ANIM_STEPS], al_steps[ANIM_STEPS];
	double sc_steps[ANIM_STEPS], xo_steps[ANIM_STEPS], yo_steps[ANIM_STEPS], zo_steps[ANIM_STEPS];

	double cur_az, cur_al, cur_scale, cur_xo, cur_yo, cur_zo;
	m_transform.get_azal( &cur_az, &cur_al );
	cur_scale = m_transform.get_scale();
	m_transform.get_offset( &cur_xo, &cur_yo, &cur_zo );

	linspace( az_steps, ANIM_STEPS, cur_az, az );
	linspace( al_steps, ANIM_STEPS, cur_al, al );
	linspace( sc_steps, ANIM_STEPS, cur_scale, scale );
	linspace( xo_steps, ANIM_STEPS, cur_xo, xo );
	linspace( yo_steps, ANIM_STEPS, cur_yo, yo );
	linspace( zo_steps, ANIM_STEPS, cur_zo, zo );

	for ( size_t i=0;i<ANIM_STEPS;i++ )
	{
		m_transform.rotate_azal( az_steps[i], al_steps[i] );
		m_transform.set_scale( sc_steps[i] );
		m_transform.offset( xo_steps[i], yo_steps[i], zo_steps[i] );
		Render();
		Refresh();
		SendEvent( wxEVT_VIEW3D_UPDATE_VIEW );
		wxYield();
		wxMilliSleep( 6 );
	}
}

void View3D::SetMode( int mode )
{
	if ( mode == m_mode ) return;

	// save the current view mode
	m_transform.get_offset( &m_lastView[m_mode].xoff,
		&m_lastView[m_mode].yoff,
		&m_lastView[m_mode].zoff );
	m_transform.get_azal( &m_lastView[m_mode].azimuth,
			&m_lastView[m_mode].altitude );
	m_lastView[m_mode].scale = m_transform.get_scale();

	// switch to the new view
	double azi_new, alt_new;

	if ( mode == TOP_VIEW )
	{
		azi_new = 180;
		alt_new = 90;
	}
	else if ( mode == Z_VIEW )
	{
		azi_new = 180;
		alt_new = 0;
	}
	else
	{
		azi_new = m_lastView[mode].azimuth;
		alt_new = m_lastView[mode].altitude;
		SetCursor( wxCURSOR_DEFAULT );
	}
	
	Animate( azi_new, alt_new, m_lastView[mode].scale, 
		m_lastView[mode].xoff,
		m_lastView[mode].yoff,
		m_lastView[mode].zoff );
	
	// switch view
	m_mode = mode;
	
	UpdateAllHandles();
	Render();
	Refresh();
	
	SendEvent( wxEVT_VIEW3D_UPDATE_VIEW );
}

int View3D::GetMode()
{
	return m_mode;
}

void View3D::RegisterType( VObject *obj )
{
	wxString type = obj->GetTypeName().Lower();
	for( size_t i=0;i<m_registeredTypes.size(); i++ )
		if ( m_registeredTypes[i]->GetTypeName().Lower() == type )
			return;

	m_registeredTypes.push_back( obj );
}

wxArrayString View3D::GetRegisteredTypes()
{
	wxArrayString list;
	for( size_t i=0;i<m_registeredTypes.size(); i++ )
		list.Add( m_registeredTypes[i]->GetTypeName() );
	return list;
}

VObject *View3D::FindRegisteredType( const wxString &type )
{
	VObject *klass = 0;
	for( size_t i=0;i<m_registeredTypes.size(); i++ )
		if ( type.Lower() == m_registeredTypes[i]->GetTypeName().Lower() )
			klass = m_registeredTypes[i];

	return klass;
}

VObject *View3D::CreateObject( const wxString &type )
{
	VObject *klass = FindRegisteredType( type );
	if ( !klass ) return 0;

	VObject *instance = klass->Duplicate();
	AddObject( instance );
	return instance;
}

void View3D::AddObject( VObject *obj )
{
	if ( std::find(m_objects.begin(), m_objects.end(), obj ) != m_objects.end() ) return;

	m_objects.push_back( obj );
	obj->BuildModel( m_scene );
	UpdateHandles( obj );
	Render();
	Refresh();
	SendEvent( wxEVT_VIEW3D_UPDATE_OBJECTS );
}

void View3D::RemoveHandlesForObject( VObject *obj )
{
	// clean up handles associated with this object
	size_t i=0;
	while( i<m_handles.size() )
	{
		if ( m_handles[i]->GetObject() == obj )
			m_handles.erase( m_handles.begin() + i );
		else
			i++;
	}
}

void View3D::DeleteObject( VObject *obj )
{
	m_objects.erase( std::find( m_objects.begin(), m_objects.end(), obj ) );

	// clear from selections list
	std::vector<VObject*>::iterator it = std::find( m_selections.begin(), m_selections.end(), obj );
	if ( it != m_selections.end() ) m_selections.erase( it );
	
	m_scene.clear( obj->GetId() );
	
	RemoveHandlesForObject( obj );

	delete obj;

	Render();
	Refresh();
	
	SendEvent( wxEVT_VIEW3D_UPDATE_OBJECTS );
}

void View3D::DeleteAll()
{
	m_selections.clear();
	for( size_t i=0;i<m_objects.size();i++ )
		delete m_objects[i];
	m_objects.clear();
	m_scene.clear();
	m_handles.clear();
	
	m_scene.basic_axes_with_ground();
	Render();
	Refresh();
	
	SendEvent( wxEVT_VIEW3D_UPDATE_OBJECTS );
}

std::vector<VObject*> View3D::GetObjects()
{
	return m_objects;
}

void View3D::Select( VObject *obj )
{
	m_selections.clear();
	if ( obj != 0 )
		m_selections.push_back( obj );

	SendEvent( wxEVT_VIEW3D_UPDATE_SELECTION );

	Refresh();
}

void View3D::ClearSelections()
{
	m_selections.clear();
	SendEvent( wxEVT_VIEW3D_UPDATE_OBJECTS );
	SendEvent( wxEVT_VIEW3D_UPDATE_SELECTION );
	Refresh();
}


bool View3D::IsSelected( VObject *obj )
{
	return std::find( m_selections.begin(), m_selections.end(), obj ) != m_selections.end();
}

std::vector<VObject*> View3D::GetSelectedObjects()
{
	return m_selections;
}

VObject *View3D::GetFirstSelectedObject()
{
	if ( m_selections.size() > 0 )
		return m_selections[0];
	else
		return 0;
}


VObject *View3D::FindObjectByName( const wxString &name )
{
	for( size_t i=0;i<m_objects.size(); i++ )
		if ( m_objects[i]->Property("Name").GetString().Lower() == name.Lower() )
			return m_objects[i];

	return 0;
}

VObject *View3D::FindObjectById( int id )
{
	for( size_t i=0;i<m_objects.size(); i++ )
		if ( id == m_objects[i]->GetId() ) return m_objects[i];

	return 0;
}

VObject *View3D::GetObject( size_t index )
{
	if ( index < m_objects.size() ) return m_objects[index];
	else return 0;
}

int View3D::GetObjectIndex( VObject *o )
{
	for( size_t i=0;i<m_objects.size(); i++ )
		if ( m_objects[i] == o ) return (int)i;

	return -1;
}

void View3D::DeleteSelected()
{
	std::vector<VObject*> list = m_selections;
	for( size_t i=0;i<list.size();i++)
		DeleteObject(list[i]);
}

void View3D::DuplicateSelected()
{
	for( size_t i=0;i<m_selections.size();i++)
		AddObject( m_selections[i]->Duplicate() );
}

void View3D::ShowAll()
{
	for (size_t i=0;i<m_objects.size();i++ )
		m_objects[i]->Show( true );

	
	SendEvent( wxEVT_VIEW3D_UPDATE_OBJECTS );

	Refresh();
}

void View3D::ChangeMap( const wxBitmap &map, double mpp )
{
	m_mpp = mpp;
	m_staticMapXY = map;
	m_staticMapXYScaled = wxNullBitmap;			
}


void View3D::Write( wxOutputStream &ostrm )
{
	wxDataOutputStream out(ostrm);

	out.Write8( 0xf2 ); // start code
	out.Write8( 3 ); // version

	// view settings
	out.Write8( (wxUint8)m_mode );
	out.Write32( __N_MODES );
	for( size_t i=0;i<__N_MODES;i++ )
		m_lastView[i].Write( ostrm );

	// objects
	out.Write32( m_objects.size() );
	for( size_t i=0;i<m_objects.size();i++ )
	{
		out.WriteString( m_objects[i]->GetTypeName() );
		m_objects[i]->Write( ostrm );
	}

	// background map if it exists
	out.WriteDouble( m_mpp );
	out.Write8( m_staticMapXY.IsNull() ? 0 : 1 ); // has map or not
	if ( !m_staticMapXY.IsNull() )
	{
		wxImage img = m_staticMapXY.ConvertToImage();
		wxPNGHandler().SaveFile( &img, ostrm, false );
	}

	// finish code
	out.Write8( 0xf2 );
}

bool View3D::Read( wxInputStream &istrm )
{
	DeleteAll(); // clear the scene

	wxDataInputStream in(istrm);
	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8();

	// read back view settings
	wxUint32 mode = in.Read8();
	wxUint32 nmodes = in.Read32();
	for( size_t i=0;i<nmodes;i++ )
	{
		if ( i < __N_MODES ) m_lastView[i].Read( istrm );
		else ViewParams().Read( istrm ); // just read it, but don't keep the data
	}

	// read in objects
	wxUint32 nobj = in.Read32();
	for( size_t i=0;i<nobj;i++ )
	{
		wxString tyname = in.ReadString();
		// if object type names change, update them 
		// here manually for upgrading
		if ( VObject *klass = FindRegisteredType( tyname ) )
		{
			VObject *obj = klass->Duplicate();
			if ( obj ) obj->Read( istrm );
			m_objects.push_back( obj );
		}
		else
			VBoxObject().Read( istrm );
	}

	if ( ver == 1 )
	{
		in.ReadDouble(); // lat
		in.ReadDouble(); // lon
		in.ReadString(); // addr
		in.Read32(); // zoom
	}
	else if ( ver >= 2 )
	{
		m_mpp = in.ReadDouble();
	}

	if ( ver < 3 )
		in.Read8(); // formerly showmap flag
	
	m_staticMapXY = wxNullBitmap;
	m_staticMapXYScaled = wxNullBitmap;
	bool has_map = in.Read8() != 0;
	if ( has_map )
	{
		wxImage img;
		wxPNGHandler().LoadFile( &img, istrm, false );
		m_staticMapXY = wxBitmap(img);
	}

	// switch to view mode as it was saved
	if ( mode < __N_MODES )
		SetMode( mode );

	UpdateAllModels();
	UpdateAllHandles();	
	Refresh();
	
	SendEvent( wxEVT_VIEW3D_UPDATE_OBJECTS );

	return in.Read8() == code;
}

const s3d::scene &View3D::GetScene()
{
	return m_scene;
}

double View3D::GetShadeFraction()
{
	return m_sf;
}

void View3D::SetRotation( double azimuth, double altitude )
{
	if ( m_mode == SPIN_VIEW )
	{
		m_transform.rotate_azal( azimuth, altitude );
		Render();
		Refresh();
	}
}

void View3D::GetRotation( double *azi, double *alt )
{
	m_transform.get_azal( azi, alt );
}

void View3D::SetOffset( double xoff, double yoff, double zoff )
{
	m_transform.offset( xoff, yoff, zoff );
	Render();
	Refresh();
}

void View3D::GetOffset( double *xoff, double *yoff, double *zoff )
{
	m_transform.get_offset( xoff, yoff, zoff );
}


void View3D::SetScale( double scale )
{
	m_transform.set_scale( scale );
	Render();
	Refresh();
}

double View3D::GetScale()
{
	return m_transform.get_scale();
}

void View3D::UpdateAllHandles()
{
	m_handles.clear();
	
	if ( m_mode == TOP_VIEW || m_mode == Z_VIEW )
		for( size_t i=0;i<m_objects.size(); i++ )
			UpdateHandles( m_objects[i] );
}

void View3D::UpdateHandles( VObject *obj )
{
	if ( m_mode == SPIN_VIEW )
	{
		m_handles.clear();
		return;
	}

	RemoveHandlesForObject( obj );
	
	obj->DeleteHandles();
	if ( !obj->IsVisible() ) return;

	obj->SetupHandles( m_mode == TOP_VIEW ? PLANE_XY : PLANE_XZ );
	std::vector<VHandle*> list = obj->GetHandles();
	for( size_t j=0;j<list.size();j++ )
		m_handles.push_back( list[j] );
}

void View3D::UpdateModel( VObject *obj )
{
	// erase all polygons with this object's ID
	m_scene.clear( obj->GetId() );
	obj->BuildModel( m_scene );
	UpdateHandles( obj );
	Render();
}

void View3D::UpdateAllModels()
{
	m_scene.clear();
	m_scene.basic_axes_with_ground();
	for (size_t i=0;i<m_objects.size();i++ )
		m_objects[i]->BuildModel( m_scene );

	Render();
}

void View3D::RebuildBSPTree()
{
	m_scene.m_bspValid = false;
	Render();
}


void View3D::Render()
{
	m_scene.build( m_transform );

	if (m_mode == SPIN_VIEW)
		m_sf = m_scene.shade(m_shade);
	else
		m_sf = 0.0;
}

wxColour View3D::FromRGBA( s3d::rgba &c )
{
	return wxColour( c.r, c.g, c.b, c.a );
}

void View3D::Draw( wxDC &dc, const s3d::polygon3d &poly, int xoff, int yoff )
{	
	if ( !poly.as_line && s3d::zeroarea( poly ) ) 
		return;

	size_t n = poly.points.size();
	if ( n < 2 ) return;
	wxPoint *pp = new wxPoint[ n ];

	for (size_t i=0;i<n;i++)
	{
		pp[i].x = (int) poly.points[i]._x;
		pp[i].y = (int) -poly.points[i]._y;
	}

	if ( poly.as_line ) dc.DrawLines( n, pp, xoff, yoff );
	else dc.DrawPolygon( n, pp, xoff, yoff );

	delete [] pp;
}




void View3D::ScreenToWorld( int xs, int yzs, double *xw, double *yzw )
{
	double xoff, yoff, zoff, scale;
	m_transform.get_offset( &xoff, &yoff, &zoff );
	scale = m_transform.get_scale();
	double yzoff = m_mode == TOP_VIEW ? yoff : zoff;
		
	xs = xs-m_winWidth/2;
	yzs = -1*(yzs-m_winHeight/2);
	
	*xw = xs / scale + xoff;
	*yzw = yzs / scale + yzoff;
}

void View3D::WorldToScreen( double xw, double yzw, int *xs, int *yzs )
{
	double xoff, yoff, zoff, scale;
	m_transform.get_offset( &xoff, &yoff, &zoff );
	scale = m_transform.get_scale();
	double yzoff = m_mode == TOP_VIEW ? yoff : zoff;
	
	*xs = (int)((xw - xoff)*scale) + m_winWidth/2;
	*yzs = (int)(-1.0*( yzw - yzoff)*scale) + m_winHeight/2;
}

void View3D::Snap(double *x, double *y, double spacing)
{
	if ( spacing < 0 ) spacing = m_snapSpacing;

	*x = Snap(*x, spacing);
	*y = Snap(*y, spacing);
}

double View3D::Snap( double v, double spacing )
{
	int incr = (v<0) ? -1 : 1;

	int multiples = (int)(v / spacing);
	double dist1 = fabs(spacing*multiples - v);
	double dist2 = fabs(spacing*(multiples+incr) - v);
	
	if ( dist1 < dist2 ) return spacing*multiples;
	else return spacing*(multiples+incr);
}

void View3D::DrawGrid( wxDC &dc )
{
	int xs, yzs;

	double minX, minYZ, maxX, maxYZ;	
	ScreenToWorld( 0, 0, &minX, &maxYZ );
	ScreenToWorld( m_winWidth, m_winHeight, &maxX, &minYZ );

	Snap( &minX, &minYZ, m_gridSpacing );
	Snap( &maxX, &maxYZ, m_gridSpacing );

	dc.SetPen( *wxBLACK_PEN );

	for ( double X = minX; X < maxX; X += m_gridSpacing )
	{
		for( double Y = minYZ; Y < maxYZ; Y+= m_gridSpacing )
		{
			WorldToScreen( X, Y, &xs, &yzs );
			dc.DrawPoint( xs, yzs );
		}
	}
}

template <typename T> T CLAMP(T value, T low, T high)
{
    return (value < low) ? low : ((value > high) ? high : value);
}

void View3D::OnPaint( wxPaintEvent & )
{
	wxAutoBufferedPaintDC pdc(this);
	wxGCDC dc( pdc );
	
	GetClientSize( &m_winWidth, &m_winHeight );
	int xoff = m_winWidth/2;
	int yoff = m_winHeight/2;

	dc.SetBackground( wxBrush( *wxWHITE ) );
	dc.Clear();
		
	bool has_statmap = false;
	if ( m_mode == TOP_VIEW && !m_staticMapXY.IsNull() )
	{
		has_statmap = true;
		
		double scale = m_transform.get_scale()*m_mpp;
		int scaled_width = (int)(m_staticMapXY.GetWidth() * scale);
		int scaled_height = (int)(m_staticMapXY.GetHeight() * scale);

		if ( m_staticMapXYScaled.GetWidth() != scaled_width
			|| m_staticMapXYScaled.GetHeight() != scaled_height )
		{
			m_staticMapXYScaled = wxBitmap(
				m_staticMapXY.ConvertToImage().Scale(
					scaled_width, scaled_height ) );
		}

		int xp, yp;
		WorldToScreen( 0, 0, &xp, &yp );
		dc.DrawBitmap( m_staticMapXYScaled, 
			xp-scaled_width/2, yp-scaled_height/2 );
	}

	double vx, vy, vz, nx, ny, nz;
	const std::vector<s3d::polygon3d*> &polygons = m_scene.get_rendered();
	for ( size_t i=0;i<polygons.size(); i++ )
	{
		s3d::polygon3d &p = *polygons[i];
		
		// skip the green background if we have a static bitmap
		if ( has_statmap && p.id == -2 ) continue;

		if ( VObject *obj = FindObjectById( p.id ) )
			if ( !obj->IsVisible() )
				continue;
		
		// compute "shaded" color of polygon	
		s3d::rgba cc = p.fill;
		if ( !p.as_line && p.id >= 0 )
		{
			// get polygon normal (cross prod of p._x,p._y,p._z [0] --> [1]
			double xo, yo, zo;
			m_transform.get_offset( &xo, &yo, &zo );
			m_transform.get_view_normal( &vx, &vy, &vz );
			s3d::point3d vn( xo+vx, yo+vy, zo+vz );
			m_transform( vn );
			vx = vn._x;
			vy = vn._y;
			vz = vn._z;

			s3d::polynormaltr( p, &nx, &ny, &nz );
			nx = 0-nx;
			ny = 0-ny;
			nz = 0-nz;
			
			double dot = nx*vx + ny*vy + nz*vz;
			double costh = dot/( sqrt(nx*nx+ny*ny+nz*nz)*sqrt(vx*vx+vy*vy+vz*vz) );
			double theta = acos( costh ) * 180/3.1415926;

			double factor = theta/180.0+0.5;

			double R = cc.r*factor;
			double G = cc.g*factor;
			double B = cc.b*factor;
			
			cc.r = (unsigned char)CLAMP<double>( R, 1, 254 );
			cc.g = (unsigned char)CLAMP<double>( G, 1, 254 );
			cc.b = (unsigned char)CLAMP<double>( B, 1, 254 );			
			cc.a = (m_mode==SPIN_VIEW||p.as_line) ? 255 : 180;
		}
				
		wxColour wcol( FromRGBA( cc ) );

		dc.SetBrush( wxBrush( wcol ) );

#ifdef _DEBUG
		dc.SetPen( wxPen( FromRGBA( p.border ), p.thick ) );		
#else 
		dc.SetPen( (m_mode==SPIN_VIEW||p.as_line) ? wxPen(wcol,2) : *wxTRANSPARENT_PEN );
#endif
		Draw( dc, p, xoff, yoff );
	}



	if ( m_mode == SPIN_VIEW )
	{
		
		wxColour cshade( 70, 70, 70 );
		dc.SetPen( wxPen(cshade,1) );
		dc.SetBrush( wxBrush( cshade, wxBRUSHSTYLE_SOLID ) );
		for( size_t k=0;k<m_shade.size();k++ )
		{
			for ( size_t i=0;i<m_shade[k].shadings.size();i++ )
			{
				s3d::polygon3d &shade = m_shade[k].shadings[i];
				Draw( dc, shade, xoff, yoff );
			}
		}

		std::vector<VObject*> sel = GetSelectedObjects();
		dc.SetPen( *wxTRANSPARENT_PEN );
		dc.SetBrush( wxBrush( wxColour(255,0,255, 200), wxBRUSHSTYLE_SOLID ) );
		std::vector<int> sel_ids;
		for (size_t i=0; i<sel.size(); i++)
			sel_ids.push_back(sel[i]->GetId());
		for (size_t i=0; i<polygons.size(); i++)
		{
			s3d::polygon3d &p = *polygons[i];
			if (std::find(sel_ids.begin(), sel_ids.end(), p.id) != sel_ids.end())
			Draw( dc, p, xoff, yoff );
		}
	}
	
	const std::vector<s3d::text3d*> &labels = m_scene.get_labels();

	for ( size_t i=0;i<labels.size(); i++ )
	{
		s3d::text3d &t = *labels[i];
		
		wxFont font( *wxNORMAL_FONT );
		if ( t.size > 2 ) font.SetPointSize( t.size );
		if ( !t.face.empty() && wxFontEnumerator::IsValidFacename( t.face ) )
			font.SetFaceName( t.face );
		dc.SetTextForeground( FromRGBA( t.color ) );
		dc.SetFont( font );
		dc.DrawText( t.text, (int)t.pos._x+xoff, (int)-t.pos._y+yoff );

	}

	if ( m_mode == TOP_VIEW || m_mode == Z_VIEW )
		DrawGrid( pdc );

	if ( m_mode == TOP_VIEW || m_mode == Z_VIEW  )
	{
		std::vector<VObject*> sel = GetSelectedObjects();
		dc.SetBrush( wxBrush( wxColour(255,0,128, 200) ) );
		dc.SetPen( wxPen( wxColour(255,0,128, 255), 1) );
		for (size_t i=0;i<m_handles.size();i++)
		{
			if ( std::find( sel.begin(), sel.end(), m_handles[i]->GetObject() ) != sel.end() )
			{
				double xw, yw;
 				m_handles[i]->GetCurrentPos(&xw, &yw);
				int xs, ys;
				WorldToScreen( xw, yw, &xs, &ys );
				dc.DrawCircle( xs, ys, HANDLE_RADIUS );
			}
		}
	}


	wxFont font = *wxNORMAL_FONT;
	font.SetWeight( wxFONTWEIGHT_BOLD );
	dc.SetFont( font );
	dc.SetTextForeground( *wxWHITE );

	if ( m_mode == SPIN_VIEW )
	{
		wxString extra;
	/*	for( size_t i=0;i<m_shade.size();i++ )
		{
			extra += wxString::Format(" %d{aoi=%.1lf sf=%.3lf} ", m_shade[i].id, m_shade[i].aoi, m_shade[i].shade_fraction );
		}
		 */

		double azi, alt;
		m_transform.get_azal( &azi, &alt );
		dc.SetFont( *wxNORMAL_FONT );
		dc.SetTextForeground( *wxBLACK );
		dc.DrawText(wxString::Format("Azimuth: %.1lf   Altitude: %.1lf   Shade fraction: %0.3lf  ",
			azi, alt, m_sf ) + extra,
			4, m_winHeight - dc.GetCharHeight() - 3);
	}
}

void View3D::OnSize( wxSizeEvent & )
{
	Refresh();
}

VHandle *View3D::FindHandle( int mx, int my, VObject *obj )
{
	VHandle *hover_handle = 0;
	for( size_t i=0;i<m_handles.size();i++ )
	{
		double x, y;
		int xs, ys;
		m_handles[i]->GetCurrentPos(&x, &y);
		WorldToScreen( x, y, &xs, &ys );
		if ( (obj == 0 || m_handles[i]->GetObject() == obj )
			&& sqrt( (mx-xs)*(mx-xs) + (my-ys)*(my-ys) ) <= HANDLE_RADIUS )
		{
			hover_handle = m_handles[i];
			break;
		}
	}
	return hover_handle;
}

VObject *View3D::FindFirstObject( int mx, int my )
{
	double xw, yzw;
	ScreenToWorld( mx, my, &xw, &yzw );
	for( size_t i=0;i<m_objects.size();i++ )
		if ( m_objects[i]->IsWithin( xw, yzw, m_mode == Z_VIEW ? PLANE_XZ : PLANE_XY )
			&& m_objects[i]->IsVisible() )
			return m_objects[i];

	return 0;
}

std::vector<VObject*> View3D::FindObjects( int mx, int my )
{
	std::vector<VObject*> list;
	double xw, yzw;
	ScreenToWorld( mx, my, &xw, &yzw );
	for( size_t i=0;i<m_objects.size();i++ )
		if ( m_objects[i]->IsWithin( xw, yzw, m_mode == Z_VIEW ? PLANE_XZ : PLANE_XY )
			&& m_objects[i]->IsVisible() )
			list.push_back( m_objects[i] );
	
	return list;
}

void View3D::OnMotion( wxMouseEvent &evt )
{
	double xw, yzw;
//	int xs, yzs;
	ScreenToWorld( evt.GetX(), evt.GetY(), &xw, &yzw );
		
			
	if ( m_zoomMode )
	{
		double dy = evt.GetY() - m_origY;
		double scale = m_origScale * ( 1 - dy/500 );
		if ( scale < 0.1 ) scale = 0.1;
		if ( scale > 1000 ) scale = 1000;
		m_transform.set_scale( scale );
		Render();
		Refresh();
		SendEvent( wxEVT_VIEW3D_UPDATE_VIEW );
		return;
	}
	else if ( m_panMode )
	{
		double dx = evt.GetX() - m_origX;
		double dy = evt.GetY() - m_origY;
		double scale = m_transform.get_scale();
		double xoff, yoff, zoff;
		m_transform.get_offset( &xoff, &yoff, &zoff );

		xoff = m_origXOff-dx/scale;
		
		if ( m_mode == SPIN_VIEW || m_mode == Z_VIEW )
			zoff = m_origZOff+dy/scale;
		else
			yoff = m_origYOff+dy/scale;

		m_transform.offset( xoff, yoff, zoff );
		Render();
		Refresh();
		return;
	}

	if ( m_mode == TOP_VIEW || m_mode == Z_VIEW )
	{

		if ( m_movingHandle )
		{
			wxClientDC cdc( this );
			MovingObjectDC movedc( cdc, this );
			VPlaneType plane( m_mode==TOP_VIEW ? PLANE_XY : PLANE_XZ );

#ifndef VIEW_USE_OVERLAY
			if ( m_eraseNeeded )
				m_movingHandle->GetObject()->DrawOnPlane( movedc, plane );
#endif

			if ( !evt.AltDown() )
				Snap( &xw, &yzw );

			m_movingHandle->MoveTo( xw, yzw );		
			m_movingHandle->GetObject()->OnHandleMoved( m_movingHandle, plane );
			m_movingHandle->GetObject()->DrawOnPlane( movedc, plane );
			m_eraseNeeded = true;
			SendEvent( wxEVT_VIEW3D_UPDATE_PROPERTIES );
		}


		VHandle *hover_handle = FindHandle( evt.GetX(), evt.GetY(), GetFirstSelectedObject() );
		VObject *hover_object = FindFirstObject( evt.GetX(), evt.GetY() );
		if ( hover_handle ) SetCursor( hover_handle->GetCursor() );
		else if ( m_movingHandle ) SetCursor( m_movingHandle->GetCursor() );
		else if ( hover_object != 0 ) SetCursor( wxCURSOR_HAND );
		else SetCursor( wxCURSOR_DEFAULT );
	}
	else
	{
		// 3D spining view
		if ( m_pressed )
		{
			int dx = evt.GetX() - m_origX;
			int dy = evt.GetY() - m_origY;
		
			if ( m_mode == SPIN_VIEW )
			{
				m_transform.change_xz( -(float)dy/4.0, (double)dx/4.0 );
				Render();
				Refresh();
			}
		
			m_origX = evt.GetX();
			m_origY = evt.GetY();

			SendEvent( wxEVT_VIEW3D_UPDATE_VIEW );
		}
	}
}

void View3D::OnLeftDown( wxMouseEvent &evt )
{
	SetFocus();
	m_pressed = true;
	m_zoomMode = evt.ControlDown();
	m_panMode = evt.ShiftDown();
	m_origX = evt.GetX();
	m_origY = evt.GetY();
	m_origScale = m_transform.get_scale();
	m_transform.get_offset( &m_origXOff, &m_origYOff, &m_origZOff );

	if ( m_mode == TOP_VIEW || m_mode == Z_VIEW )
	{
		VObject *sel_obj = GetFirstSelectedObject();
		if ( (m_movingHandle = FindHandle( evt.GetX(), evt.GetY(), sel_obj ) )  != 0)
		{
			wxClientDC cdc( this );
#ifdef VIEW_USE_OVERLAY
			wxDCOverlay overlaydc( m_overlay, &cdc );
			overlaydc.Clear();
//			m_overlay.Reset();
#else
			MovingObjectDC movedc( cdc, this );
			m_movingHandle->GetObject()->DrawOnPlane( movedc, m_mode == TOP_VIEW ? PLANE_XY : PLANE_XZ );
			m_eraseNeeded = false;
#endif		
		}
		else
		{
			std::vector<VObject*> selectable = FindObjects( evt.GetX(), evt.GetY() );
		
			if ( m_nextSelectionIndex >= selectable.size() )
				m_nextSelectionIndex = 0;

			VObject *sel_objj = 0;
			if ( m_nextSelectionIndex < selectable.size() )
				sel_objj = selectable[m_nextSelectionIndex];

			Select( sel_objj );

			m_nextSelectionIndex++; // increment to cycle through possible selectable objects
		}

	}
	else if ( m_mode == SPIN_VIEW )
	{
		// intersect mouse x, y with transformed polygons currently displayed
		int mx = evt.GetX();
		int my = evt.GetY();
		
		GetClientSize( &m_winWidth, &m_winHeight );
		int xoff = m_winWidth/2;
		int yoff = m_winHeight/2;

		// get list of all object ids that are selectable
		std::vector<int> selids;
		const std::vector<s3d::polygon3d*> &polys = m_scene.get_rendered();
		for( int i=polys.size()-1;i>=0;i-- )
		{
			s3d::polygon3d &poly = *polys[i];
			size_t nn = poly.points.size();
			if ( nn >= 3 
				&& !poly.as_line 
				&& poly.id >= 0 
				&& !s3d::zeroarea( poly ) )
			{
				double *xx = new double[nn];
				double *yy = new double[nn];
				for( size_t j=0;j<nn;j++ )
				{
					xx[j] = xoff + poly.points[j]._x;
					yy[j] = yoff - poly.points[j]._y;
				}

				if( s3d::inpoly( xx, yy, nn, mx, my )
					&& std::find( selids.begin(), selids.end(), poly.id ) == selids.end() )
					selids.push_back( poly.id );

				delete [] xx;
				delete [] yy;
			}
		}

		// now obtain list of objects that are selectable
		std::vector<VObject*> selectable;
		for( size_t i=0;i<m_objects.size();i++ )
		{
			if ( std::find( selids.begin(), selids.end(), m_objects[i]->GetId() ) != selids.end() )
				selectable.push_back( m_objects[i] );
		}			

		// iterate through possible selectiosn
		if ( m_nextSelectionIndex >= selectable.size() )
			m_nextSelectionIndex = 0;

		VObject *sel_obj = 0;
		if ( m_nextSelectionIndex < selectable.size() )
			sel_obj = selectable[m_nextSelectionIndex];

		if ( sel_obj ) Select( sel_obj );
		else ClearSelections();

		m_nextSelectionIndex++; // increment to cycle through possible selectable objects

		Refresh();
	}
}


void View3D::OnLeftUp( wxMouseEvent & )
{
	if ( m_movingHandle != 0 )
	{
		VObject *obj = m_movingHandle->GetObject();
		obj->OnHandleMoved( m_movingHandle, m_mode == TOP_VIEW ? PLANE_XY : PLANE_XZ );
		UpdateModel( obj );
		UpdateHandles( obj );
		Refresh();
		m_movingHandle = 0;
		m_eraseNeeded = false;

#ifdef VIEW_USE_OVERLAY		
		wxClientDC dc( this );
		wxDCOverlay overlaydc( m_overlay, &dc );
		overlaydc.Clear();      
//        m_overlay.Reset();
#endif

		SendEvent( wxEVT_VIEW3D_UPDATE_PROPERTIES );
	}

	m_pressed = false;
	m_zoomMode = false;
	m_panMode = false;
}

void View3D::OnWheel( wxMouseEvent &evt )
{
	double s = m_transform.get_scale() + ((double)evt.GetWheelRotation())/100.0;
	if (s > 0)
	{
		m_transform.set_scale( (double)(int)s );
		Render();
		Refresh();
	}
}


void View3D::OnKey( wxKeyEvent &evt )
{
	int key = evt.GetKeyCode();
	wxUniChar ukey = evt.GetUnicodeKey();
	bool needs_render = false;
	
	if ( key == WXK_LEFT || key == WXK_RIGHT || key == WXK_UP || key == WXK_DOWN )
	{
		double xoff, yoff, zoff;
		m_transform.get_offset( &xoff, &yoff, &zoff );
	
		if (key == WXK_LEFT)
			xoff -= 3.0;
		else if (key== WXK_RIGHT)
			xoff += 3.0;
		else if (key== WXK_UP)
		{
			if ( m_mode == TOP_VIEW ) yoff += 3.0;
			else zoff += 3.0;
		}
		else if (key == WXK_DOWN)
		{
			if ( m_mode == TOP_VIEW ) yoff -= 3.0;
			else zoff -= 3.0;
		}
	
		m_transform.offset( xoff, yoff, zoff );
		needs_render = true;
	}
	else if (ukey == 'I' || ukey == 'O')
	{
		double scale = m_transform.get_scale();
		if (ukey == 'I')
			scale += 0.2;
		else if (ukey == 'O')
		{
			scale -= 0.2;
			if ( scale < 0.2 ) scale = 0.2;
		}
		m_transform.set_scale( scale );
		needs_render = true;
	}
	else if (ukey == 'C')
	{
		m_transform.offset( 0, 0, 0 );
		needs_render = true;
	}
	else if (ukey == 'Z')
	{
		SetMode( Z_VIEW );
	}
	else if (ukey == 'T')
	{
		SetMode( TOP_VIEW );
	}
	else if (ukey == 'S')
	{
		SetMode( SPIN_VIEW );
	}
	else if (ukey == 'A')
	{
		ShowAll();
	}

	if ( needs_render )
	{
		SendEvent( wxEVT_VIEW3D_UPDATE_VIEW );
		Render();
		Refresh();
	}
}

void View3D::SetAzAl( double &az, double &al)
{
	m_transform.set_azal(az,al);
	Render();
	Refresh();
	SendEvent( wxEVT_VIEW3D_UPDATE_VIEW );
}

void View3D::SendEvent( int type )
{
	wxCommandEvent e( type, GetId() );
	e.SetEventObject( this );
	GetEventHandler()->ProcessEvent( e );
}

View3D::ViewParams::ViewParams()
{
	azimuth = 180;
	altitude = 0;
	scale = 4;
	xoff = yoff = zoff = 0;
}

void View3D::ViewParams::Write( wxOutputStream &os )
{
	wxDataOutputStream out(os);
	out.Write8( 0xf1 );
	out.Write8( 1 );
	out.WriteDouble( azimuth );
	out.WriteDouble( altitude );
	out.WriteDouble( scale );
	out.WriteDouble( xoff );
	out.WriteDouble( yoff );
	out.WriteDouble( zoff );
	out.Write8( 0xf1 );
}

bool View3D::ViewParams::Read( wxInputStream &is )
{
	wxDataInputStream in(is);
	wxUint8 code = in.Read8();
	in.Read8(); // version

	azimuth = in.ReadDouble();
	altitude = in.ReadDouble();
	scale = in.ReadDouble();
	xoff = in.ReadDouble();
	yoff = in.ReadDouble();
	zoff = in.ReadDouble();

	return in.Read8() == code;
}
