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

#include <curl/curl.h>

#include "simplecurl.h"
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

enum { _idMenuFirst = wxID_HIGHEST+982,
	ID_CENTER_VIEW, ID_DELETE, ID_DUPLICATE, ID_SHOW_ALL, ID_DELETE_ALL, 
	ID_SAVE_SCENE, ID_LOAD_SCENE, ID_SHOW_MAP, ID_CLEAR_MAP,
	ID_CREATE_OBJECT, ID_CREATE_OBJECT_MAX=ID_CREATE_OBJECT+100,	
	ID_SELECT_OBJECT, ID_SELECT_OBJECT_MAX=ID_SELECT_OBJECT+500,
	
	_idMenuLast };

BEGIN_EVENT_TABLE( View3D, wxWindow )
	EVT_PAINT( View3D::OnPaint )
	EVT_SIZE( View3D::OnSize )
	EVT_LEFT_DOWN( View3D::OnLeftDown )
	EVT_LEFT_DCLICK( View3D::OnLeftDown )
	EVT_LEFT_UP( View3D::OnLeftUp )
	EVT_RIGHT_DOWN( View3D::OnRightDown )
	EVT_MOUSEWHEEL( View3D::OnWheel )
	EVT_MOTION( View3D::OnMotion )
	EVT_MENU_RANGE( _idMenuFirst, _idMenuLast, View3D::OnMenu )
	EVT_KEY_DOWN( View3D::OnKey )
END_EVENT_TABLE()

View3D::View3D( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxWindow( parent, id, pos, size, wxWANTS_CHARS )
{	
	// '' default values for window size ''
	m_winWidth = 400;
	m_winHeight = 300;

	m_modeToolHeight = 30;
	for ( int i=0;i<__N_MODES;i++ )
		m_modeToolWidths[i] = 0;

	m_nextSelectionIndex = 0;

	m_mpp = 1;
	m_showMap = true;

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
	m_snapSpacing = 1;
	SetBackgroundStyle( wxBG_STYLE_PAINT );
	
	RegisterType( new VBoxObject );
	RegisterType( new VCylinderObject );
	RegisterType( new VRoofObject );
	RegisterType( new VConicalTreeObject );
//	RegisterType( new VRoundTreeObject );
	RegisterType( new VActiveSurfaceObject );

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

void linspace( double *v, size_t N, double start, double end )
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


void View3D::SaveScene()
{
	wxFileDialog dlg( this, "Save scene to file", wxEmptyString, 
		wxEmptyString, "Scene file (*.s3d)|*.s3d", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if ( dlg.ShowModal() == wxID_OK )
	{
		if (!WriteToFile( dlg.GetPath() ))
			wxMessageBox("Could not open file for writing:\n\n" + dlg.GetPath() );
	}
}

void View3D::LoadScene()
{
	wxFileDialog dlg( this, "Load scene from file", wxEmptyString,
		wxEmptyString, "Scene file (*.s3d)|*.s3d", wxFD_OPEN );
	if ( dlg.ShowModal() == wxID_OK )
	{
		if (!LoadFromFile( dlg.GetPath() ))
			wxMessageBox("Could not open file for reading:\n\n" + dlg.GetPath() );
	}
}

bool View3D::WriteToFile( const wxString &file )
{
	wxFFileOutputStream fos( file );
	if ( !fos.IsOk() ) return false;
	Write( fos );
	return true;
}

bool View3D::LoadFromFile( const wxString &file )
{
	wxFFileInputStream fis( file );
	if ( !fis.IsOk() || !Read( fis ) ) return false;
	else return true;
}

void View3D::ChangeMap( const wxBitmap &map, double mpp )
{
	m_mpp = mpp;
	m_staticMapXY = map;
	m_staticMapXYScaled = wxNullBitmap;			
}

void View3D::ShowMap( bool b )
{
	m_showMap = b;
	Refresh();
}


void View3D::Write( wxOutputStream &ostrm )
{
	wxDataOutputStream out(ostrm);

	out.Write8( 0xf2 ); // start code
	out.Write8( 2 ); // version

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
	out.Write8( m_showMap ? 1 : 0 );
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
		if ( VObject *klass = FindRegisteredType( in.ReadString() ) )
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
	else if ( ver == 2 )
	{
		m_mpp = in.ReadDouble();
	}
	m_showMap = in.Read8() != 0;
	
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
	return m_shade.shade_fraction;
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

void View3D::Render()
{
	m_scene.build( m_transform );

	if (m_mode == SPIN_VIEW)
		m_scene.shade(m_shade);
	else
		m_shade.shade_fraction = 0.0;
}

wxColour View3D::FromRGBA( s3d::rgba &c )
{
	return wxColour( c.r, c.g, c.b, c.a );
}

void View3D::Draw( wxDC &dc, const s3d::polygon3d &poly, bool as_line, int xoff, int yoff )
{
	size_t n = poly.points.size();
	if ( n < 2 ) return;
	wxPoint *pp = new wxPoint[ n ];

	for (size_t i=0;i<n;i++)
	{
		pp[i].x = (int) poly.points[i]._x;
		pp[i].y = (int) -poly.points[i]._y;
	}

	if ( as_line ) dc.DrawLines( n, pp, xoff, yoff );
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
	if ( m_mode == TOP_VIEW && !m_staticMapXY.IsNull() && m_showMap )
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
	const std::vector<s3d::polygon3d*> &polygons = m_scene.get_polygons();
	for ( size_t i=0;i<polygons.size(); i++ )
	{
		s3d::polygon3d &p = *polygons[i];
		
		// skip the green background if we have a static bitmap
		if ( has_statmap && p.id == -2 ) continue;

		if ( VObject *obj = FindObjectById( p.id ) )
			if ( !obj->IsVisible() )
				continue;
			
		s3d::rgba cc = p.fill;

		if ( !p.as_line && p.id >= 0 )
		{
			// get polygon normal (cross prod of p._x,p._y,p._z [0] --> [1]
			m_scene.get_viewnormal( &vx, &vy, &vz );

			double Ax = p.points[0].x;
			double Ay = p.points[0].y;
			double Az = p.points[0].z;
			double Bx = p.points[1].x;
			double By = p.points[1].y;
			double Bz = p.points[1].z;

			nx = (Ay*Bz)-(By*Az);
			ny = -(Ax*Bz)+(Bx*Az);
			nz = (Ax*By)-(Ay*Bx);

			double dot = nx*vx + ny*vy + nz*vz;
			double costh = dot/( sqrt(nx*nx+ny*ny+nz*nz)*sqrt(vx*vx+vy*vy+vz*vz) );
			double theta = acos( costh ) * 180/3.1415926;

			double factor = theta/180.0 + 0.5;

			cc.r = (unsigned char)(((double)cc.r)*factor);
			cc.g = (unsigned char)(((double)cc.g)*factor);
			cc.b = (unsigned char)(((double)cc.b)*factor);
			cc.a = 255;
		}
				
		dc.SetBrush( wxBrush( FromRGBA( cc ) ) );

#ifdef _DEBUG
		dc.SetPen( wxPen( FromRGBA( p.border ), p.thick ) );		
#else // no borders in release mode
		if (p.as_line)
			dc.SetPen( wxPen( FromRGBA( p.border ), p.thick ) );		
		else
			dc.SetPen( *wxTRANSPARENT_PEN );
#endif
		Draw( dc, p, p.as_line, xoff, yoff );
	}



	if ( m_mode == SPIN_VIEW )
	{
		dc.SetPen( *wxTRANSPARENT_PEN );
		dc.SetBrush( wxColour( 90, 90, 90, 220 ) );
		for ( size_t i=0;i<m_shade.shadings.size();i++ )
		{
			s3d::polygon3d &shade = m_shade.shadings[i].intersect;
			Draw( dc, shade, false, xoff, yoff );
		}
		std::vector<VObject*> sel = GetSelectedObjects();
		// magenta
		dc.SetBrush( wxBrush( wxColour(255,0,255, 200) ) );
		dc.SetPen( *wxTRANSPARENT_PEN );
		std::vector<int> sel_ids;
		for (size_t i=0; i<sel.size(); i++)
			sel_ids.push_back(sel[i]->GetId());
		for (size_t i=0; i<polygons.size(); i++)
		{
			s3d::polygon3d &p = *polygons[i];
			if (std::find(sel_ids.begin(), sel_ids.end(), p.id) != sel_ids.end())
			Draw( dc, p, p.as_line, xoff, yoff );
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

/* hide embedded buttons
	char *modes[__N_MODES] = { "3D:s (view)", "Top:t (edit)", "Side:z (edit)" };
	int total_width = 0;
	for( int i=0;i<__N_MODES;i++ )
	{
		m_modeToolWidths[i] = dc.GetTextExtent( modes[i] ).GetWidth()+6;
		total_width += m_modeToolWidths[i];
	}	
	m_modeToolHeight = dc.GetCharHeight()+6;
	dc.SetPen( wxPen( "navy", 1 ) );
	dc.SetBrush( wxBrush( "navy" ) );
	dc.DrawRectangle( 0, 0, total_width, m_modeToolHeight );
	int cur_x = 0;
	for ( int i=0;i<__N_MODES;i++ )
	{
		if ( m_mode == i )
		{
			dc.SetPen( wxPen( "magenta", 1 ) );
			dc.SetBrush( wxBrush("magenta") );
			dc.DrawRectangle( cur_x, 0, m_modeToolWidths[i], m_modeToolHeight );
		}
		dc.DrawText( modes[i], cur_x+3, 3 );
		cur_x += m_modeToolWidths[i];
	}

	*/
	double azi, alt;
	m_transform.get_azal( &azi, &alt );
	double s = m_transform.get_scale();
	dc.SetFont( *wxNORMAL_FONT );
	dc.SetTextForeground( *wxBLUE );
	// shade fraction calculated at same scale for scene, day and year
//	dc.DrawText(wxString::Format("Azimuth: %.1lf   Altitude: %.1lf   Scale: %.2lf   Shade fraction: %.3lf", azi, alt, s, m_shade.shade_fraction),4, m_winHeight - dc.GetCharHeight() - 3);
	
//	dc.DrawText(wxString::Format("Azimuth: %.1lf   Altitude: %.1lf   Scale: %.2lf   Shade fraction: %.3lf Viewpoint(%lg,%lg,%lg)", azi, alt, s, m_shade.shade_fraction, x,y,z),	4, m_winHeight - dc.GetCharHeight() - 3);
#ifdef _DEBUG
	double x,y,z;
	//m_scene.get_viewxyz(&x,&y,&z);
	m_transform.get_xyz( &x,&y,&z);
//	dc.DrawText(wxString::Format("Azimuth: %.1lf   Altitude: %.1lf   Scale: %.2lf   Shade fraction: %.3lf View_normal(%lg,%lg,%lg)", azi, alt, s, m_shade.shade_fraction, x,y,z),	4, m_winHeight - dc.GetCharHeight() - 3);
	dc.DrawText(wxString::Format("Azimuth: %.1lf   Altitude: %.1lf   Scale: %.2lf   Shade fraction: %.3lf Mouse_pos(%lg,%lg,%lg)", azi, alt, s, m_shade.shade_fraction, m_xw,m_yw,m_zw),	4, m_winHeight - dc.GetCharHeight() - 3);
#else
	dc.DrawText(wxString::Format("Azimuth: %.1lf   Altitude: %.1lf   Scale: %.2lf", azi, alt, s),4, m_winHeight - dc.GetCharHeight() - 3);
#endif
/*
	dc.DrawText(wxString::Format("Azimuth: %.1lf   Altitude: %.1lf   Scale: %.2lf   Shade fraction: %.3lf", azi, alt, s, m_sf_shade.shade_fraction),
		4, m_winHeight - dc.GetCharHeight() - 3);
*/
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
	int xs, yzs;
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
	if ( evt.GetY() < m_modeToolHeight )
	{
		int x_max = 0;
		for ( int i=0;i<__N_MODES;i++ )
		{
			x_max += m_modeToolWidths[i];
			if ( evt.GetX() < x_max )
			{
				SetMode( i );
				return;
			}
		}
	}

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
		if ( m_movingHandle = FindHandle( evt.GetX(), evt.GetY(), sel_obj ) )
		{
			wxClientDC cdc( this );
#ifdef VIEW_USE_OVERLAY
			wxDCOverlay overlaydc( m_overlay, &cdc );
			overlaydc.Clear();
			m_overlay.Reset();
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

			sel_obj = 0;
			if ( m_nextSelectionIndex < selectable.size() )
				sel_obj = selectable[m_nextSelectionIndex];

			Select( sel_obj );

			m_nextSelectionIndex++; // increment to cycle through possible selectable objects
		}

	}
	else if ( m_mode == SPIN_VIEW )
	{
		// selection code goes here - for now deselect all selected (clear object list selected)
		m_selections.clear();
		SendEvent( wxEVT_VIEW3D_UPDATE_OBJECTS );
		SendEvent( wxEVT_VIEW3D_UPDATE_SELECTION );
		Render();
		Refresh();
	}
}

void View3D::OnRightDown( wxMouseEvent & )
{
	wxMenu menu;

	wxArrayString types = GetRegisteredTypes();
	for( size_t i=0;i<types.size();i++ )
		menu.Append( ID_CREATE_OBJECT+i, "Create " + types[i] + "" );
	
	bool has_sel = (GetFirstSelectedObject() != 0);
	menu.AppendSeparator();
	menu.Append( ID_DUPLICATE, "Duplicate");
	menu.Enable( ID_DUPLICATE, has_sel );
	menu.Append( ID_DELETE, "Delete" );
	menu.Enable( ID_DELETE, has_sel );
	menu.Append( ID_DELETE_ALL, "Delete all" );
	menu.AppendSeparator();
	menu.Append( ID_SHOW_ALL, "Show all" );	
	menu.Append( ID_CENTER_VIEW, "Move to origin" );
	menu.AppendSeparator();
	menu.AppendCheckItem( ID_SHOW_MAP, "Show map" );
	menu.Check( ID_SHOW_MAP, m_showMap );
	menu.Append( ID_CLEAR_MAP, "Clear map" );
	menu.AppendSeparator();
	menu.Append( ID_SAVE_SCENE, "Save scene to file..." );
	menu.Append( ID_LOAD_SCENE, "Load scene from file..." );
	PopupMenu( &menu );
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
        m_overlay.Reset();
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

	if ( needs_render )
	{
		SendEvent( wxEVT_VIEW3D_UPDATE_VIEW );
		Render();
		Refresh();
	}
}

void View3D::OnMenu( wxCommandEvent &evt )
{
	switch(evt.GetId())
	{
	case ID_SAVE_SCENE:
		SaveScene();
		break;
	case ID_LOAD_SCENE:
		LoadScene();
		break;

	case ID_DUPLICATE:
		DuplicateSelected();
		break;

	case ID_DELETE:
		DeleteSelected();
		break;

	case ID_DELETE_ALL:
		DeleteAll();
		Refresh();
		break;
		
	case ID_SHOW_ALL:
		ShowAll();
		break;

	case ID_SHOW_MAP:
		m_showMap = !m_showMap;
		Refresh();
		break;

	case ID_CLEAR_MAP:
		m_staticMapXY = wxNullBitmap;
		m_staticMapXYScaled = wxNullBitmap;
		Refresh();
		break;

	case ID_CENTER_VIEW: SetOffset( 0, 0, 0 ); break;
	default:
		if ( evt.GetId() >= ID_CREATE_OBJECT
			&& evt.GetId() < ID_CREATE_OBJECT_MAX )
		{
			size_t idx = evt.GetId() - ID_CREATE_OBJECT;
			wxArrayString types = GetRegisteredTypes();
			if ( idx < types.Count() )
			{
				CreateObject( types[idx] );
				Render();
				Refresh();
			}
		}		
		break;
	}
}


void View3D::SetAzAl( double &az, double &al)
{
	m_transform.set_azal(az,al);
	Render();
	Refresh();
	SendEvent( wxEVT_VIEW3D_UPDATE_VIEW );
}

void View3D::SetViewXYZ( double &x, double &y, double &z)
{
	m_scene.set_viewxyz(x,y,z);
	Render();
	Refresh();
	SendEvent( wxEVT_VIEW3D_UPDATE_VIEW );
}

void View3D::GetViewXYZ( double *x, double *y, double *z)
{
	m_scene.get_viewxyz(x,y,z);
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
