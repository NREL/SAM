#include <vector>
#include <stdio.h>
#include <math.h>

#include <wx/simplebook.h>
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
#include <wx/tglbtn.h>
#include <wx/busyinfo.h>
#include <wx/statbmp.h>
#include <wx/clipbrd.h>
#include <wx/generic/statbmpg.h>
#include <wx/mstream.h>

#include <wx/propgrid/propgrid.h>
#include <wx/propgrid/advprops.h>
#include <wx/splitter.h>

#include <wex/numeric.h>
#include <wex/plot/plplotctrl.h>
#include <wex/plot/pllineplot.h>
#include <wex/dview/dvplotctrl.h>
#include <wex/utils.h>
#include <wex/jsonreader.h>
#include <wex/metro.h>

#include <wex/icons/cirplus_12.cpng>
#include <wex/icons/cirminus_12.cpng>
#include <wex/icons/left_arrow_13.cpng>
#include <wex/icons/right_arrow_13.cpng>
#include <wex/icons/up_arrow_13.cpng>
#include <wex/icons/down_arrow_13.cpng>

#include "widgets.h"

#include "s3tool.h"
#include "s3view.h"
#include "simplecurl.h"

#ifndef S3D_STANDALONE
#include "main.h"
#endif

enum { ID_ADDRESS = wxID_HIGHEST+239, ID_CURL, ID_LOOKUP_ADDRESS, ID_LATITUDE, ID_LONGITUDE, ID_TIMEZONE,
	ID_GET_MAP, ID_GO_UP, ID_GO_DOWN, ID_GO_LEFT, ID_GO_RIGHT, ID_ZOOM_IN, ID_ZOOM_OUT, ID_UNDERLAY_MAP,
	ID_REMOVE_UNDERLAY,	ID_LOAD_MAP_IMAGE, ID_PASTE_MAP_IMAGE, ID_MANUAL_SCALE };


BEGIN_EVENT_TABLE( LocationSetup, wxPanel )
	EVT_BUTTON( ID_LOOKUP_ADDRESS, LocationSetup::OnAddressChange )
	EVT_TEXT_ENTER( ID_ADDRESS, LocationSetup::OnAddressChange )

	EVT_BUTTON( ID_GET_MAP, LocationSetup::OnGetMap )
	EVT_BUTTON( ID_GO_LEFT, LocationSetup::OnMapChange )
	EVT_BUTTON( ID_GO_RIGHT, LocationSetup::OnMapChange )
	EVT_BUTTON( ID_GO_UP, LocationSetup::OnMapChange )
	EVT_BUTTON( ID_GO_DOWN, LocationSetup::OnMapChange )
	EVT_BUTTON( ID_ZOOM_IN, LocationSetup::OnMapChange )
	EVT_BUTTON( ID_ZOOM_OUT, LocationSetup::OnMapChange )
	EVT_BUTTON( ID_LOAD_MAP_IMAGE, LocationSetup::OnImportMapImage )
	EVT_BUTTON( ID_PASTE_MAP_IMAGE, LocationSetup::OnImportMapImage )
	EVT_BUTTON( ID_MANUAL_SCALE, LocationSetup::OnManualScale )
	EVT_BUTTON( ID_UNDERLAY_MAP, LocationSetup::OnUnderlayMap )
	EVT_BUTTON( ID_REMOVE_UNDERLAY, LocationSetup::OnRemoveUnderlay )
	EVT_SIMPLECURL( ID_CURL, LocationSetup::OnCurl )
END_EVENT_TABLE()


LocationSetup::LocationSetup( wxWindow *parent, ShadeTool *st )
	: wxPanel( parent ), 
	  m_curl( this, ID_CURL ),
	  m_shadeTool( st )
{
	SetBackgroundColour( *wxWHITE );
	
	
	m_scrollWin = new wxScrolledWindow( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxScrolledWindowStyle|wxBORDER_NONE );
	m_bitmapCtrl = new wxGenericStaticBitmap( m_scrollWin, wxID_ANY, wxNullBitmap );
	m_address = new wxTextCtrl( this, ID_ADDRESS, "Denver, CO", wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );

	m_zoomLevel = 19;
	m_mpp = 0.01;
	
	wxMetroButton *btn;
	
	m_lat = new wxNumericCtrl( this, ID_LATITUDE, 39.7375670 );
	m_lat->SetFormat( 7 );
	m_lon = new wxNumericCtrl( this, ID_LONGITUDE, -104.9847179 );
	m_lon->SetFormat( 7 );
	m_tz = new wxNumericCtrl( this, ID_TIMEZONE, -7 );

	wxBoxSizer *tools1 = new wxBoxSizer( wxHORIZONTAL );
	tools1->Add( new wxStaticText( this, wxID_ANY, "Address:"), 0, wxLEFT|wxTOP|wxBOTTOM|wxALIGN_CENTER_VERTICAL, 10 );
	tools1->Add( m_address, 1, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	tools1->Add( new wxStaticText( this, wxID_ANY, "Latitude:"), 0, wxLEFT|wxTOP|wxBOTTOM|wxALIGN_CENTER_VERTICAL, 10 );
	tools1->Add( m_lat, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	tools1->Add( new wxStaticText( this, wxID_ANY, "Longitude:"), 0, wxLEFT|wxTOP|wxBOTTOM|wxALIGN_CENTER_VERTICAL, 10 );
	tools1->Add( m_lon, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	tools1->Add( new wxStaticText( this, wxID_ANY, "Time zone:"), 0, wxLEFT|wxTOP|wxBOTTOM|wxALIGN_CENTER_VERTICAL, 10 );
	tools1->Add( m_tz, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	
	wxPanel *panel_map_tools = new wxPanel( this );
	panel_map_tools->SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

	wxBoxSizer *tools3 = new wxBoxSizer( wxHORIZONTAL );
	
	tools3->Add( new wxMetroButton( panel_map_tools, ID_LOOKUP_ADDRESS, "Lookup address", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT ) );
	tools3->Add( new wxMetroButton( panel_map_tools, ID_GET_MAP, "Update map from coordinates", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT  ) );	
	tools3->Add( new wxMetroButton( panel_map_tools, ID_LOAD_MAP_IMAGE, "Load image", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT ) );
	tools3->Add( new wxMetroButton( panel_map_tools, ID_PASTE_MAP_IMAGE, "Paste image", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT ) );
	tools3->Add( new wxMetroButton( panel_map_tools, ID_MANUAL_SCALE, "Manual scale", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT ) );

	tools3->Add( new wxMetroButton( panel_map_tools, ID_ZOOM_IN, wxEmptyString, wxBITMAP_PNG_FROM_DATA( cirplus_12 )), 0, wxALL|wxEXPAND, 0 );
	tools3->Add( new wxMetroButton( panel_map_tools, ID_ZOOM_OUT, wxEmptyString, wxBITMAP_PNG_FROM_DATA( cirminus_12 )), 0, wxALL|wxEXPAND, 0 );
	tools3->Add( new wxMetroButton( panel_map_tools, ID_GO_LEFT, wxEmptyString, wxBITMAP_PNG_FROM_DATA( left_arrow_13 ) ), 0, wxALL|wxEXPAND, 0 );
	tools3->Add( new wxMetroButton( panel_map_tools, ID_GO_RIGHT, wxEmptyString, wxBITMAP_PNG_FROM_DATA( right_arrow_13 ) ), 0, wxALL|wxEXPAND, 0 );
	tools3->Add( new wxMetroButton( panel_map_tools, ID_GO_UP, wxEmptyString, wxBITMAP_PNG_FROM_DATA( up_arrow_13 ) ), 0, wxALL|wxEXPAND, 0 );
	tools3->Add( new wxMetroButton( panel_map_tools, ID_GO_DOWN, wxEmptyString, wxBITMAP_PNG_FROM_DATA( down_arrow_13 ) ), 0, wxALL|wxEXPAND, 0 );

	tools3->Add( new wxMetroButton( panel_map_tools, ID_UNDERLAY_MAP, "Underlay this map in the scene", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT  ) );
	tools3->Add( new wxMetroButton( panel_map_tools, ID_REMOVE_UNDERLAY, "Remove underlay in scene", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT  ) );

	tools3->AddStretchSpacer();

	panel_map_tools->SetSizer( tools3 );

	wxBoxSizer *main = new wxBoxSizer( wxVERTICAL );
	main->Add( tools1, 0, wxALL|wxEXPAND, 2 );
	main->Add( panel_map_tools, 0, wxALL|wxEXPAND, 0 );
	main->Add( m_scrollWin, 1, wxALL|wxEXPAND, 0 );
	SetSizer(main);
}



void LocationSetup::OnCurl( wxSimpleCurlEvent &evt )
{
	// could do a progress thing...?
}

void LocationSetup::OnAddressChange( wxCommandEvent &e )
{
	m_lat->SetValue( std::numeric_limits<double>::quiet_NaN() );
	m_lon->SetValue( std::numeric_limits<double>::quiet_NaN() );
	m_tz->SetValue( std::numeric_limits<double>::quiet_NaN() );

	wxYield();

	double lat, lon, tz;
	if ( !wxSimpleCurl::GeoCode( m_address->GetValue(), &lat, &lon, &tz ) )
	{
		wxMessageBox("failed to geocode address");
		return;
	}
	
	m_lat->SetValue( lat );
	m_lon->SetValue( lon );
	m_tz->SetValue( tz );	

	DownloadMap();
}

void LocationSetup::OnGetMap( wxCommandEvent & )
{
	DownloadMap();
}

void LocationSetup::DownloadMap(  )
{
	wxBusyInfo info("Obtaining aerial imagery...");
	m_bitmap = wxSimpleCurl::StaticMap( m_lat->Value(), m_lon->Value(), m_zoomLevel, wxSimpleCurl::BING_MAPS );
	if (!m_bitmap.IsOk())
	{
		wxMessageBox("Invalid image data file");
		return;
	}

	m_unannotatedBitmap = m_bitmap;
	
	// Map resolution = 156543.04 meters/pixel * cos(latitude) / (2 ^ zoomlevel)
	// http://msdn.microsoft.com/en-us/library/aa940990.aspx
	m_mpp = 156543.04 * cos(m_lat->Value()*3.15926/180) / pow(2,m_zoomLevel);
	
	UpdateMap();
}


void LocationSetup::OnMapChange( wxCommandEvent &evt )
{
	double lat = m_lat->Value();
	double lon = m_lon->Value();

	if ( lat == -999 || lon == -999 )
	{
		wxMessageBox( "please obtain a map first");
		return;
	}

	double incr = 0.00005*(22-m_zoomLevel);

	switch(evt.GetId())
	{
	case ID_GO_UP: lat+=incr; break;
	case ID_GO_DOWN: lat-=incr; break;
	case ID_GO_LEFT: lon-=incr; break;
	case ID_GO_RIGHT: lon+=incr; break;
	case ID_ZOOM_IN: if ( m_zoomLevel < 21 ) m_zoomLevel++; break;
	case ID_ZOOM_OUT: if ( m_zoomLevel > 1 ) m_zoomLevel--; break;
	}

	m_lat->SetValue( lat );
	m_lon->SetValue( lon );

	DownloadMap( );
}

int LocationSetup::GetZoomLevel() { return m_zoomLevel; }
wxString LocationSetup::GetAddress() { return m_address->GetValue(); }


void LocationSetup::SetLocation( const wxString &address, double lat, double lon, double tz )
{
	m_address->ChangeValue( address );
	m_lat->SetValue( lat );
	m_lon->SetValue( lon );
	m_tz->SetValue( tz );
}

void LocationSetup::GetLocation(double *lat, double *lon, double *tz)
{
	if ( lat ) *lat = m_lat->Value();
	if ( lon ) *lon = m_lon->Value();
	if ( tz ) *tz = m_tz->Value();
}

wxBitmap LocationSetup::GetMap(double *lat, double *lon, double *tz, double *mpp)
{
	GetLocation( lat, lon, tz );
	if ( mpp ) *mpp = m_mpp;
	return m_bitmap;
}

void LocationSetup::SetMap( const wxBitmap &bit, const wxString &addrstr, double lat, double lon, double tz, double mpp )
{
	m_lat->SetValue( lat );
	m_lon->SetValue( lon );
	m_tz->SetValue( tz );
	m_address->SetValue( addrstr );

	m_mpp = mpp;
	m_unannotatedBitmap = bit;

	UpdateMap();
}

void LocationSetup::OnUnderlayMap( wxCommandEvent & )
{
	if ( m_bitmap.IsOk() )
	{
		m_shadeTool->SwitchTo( PG_SCENE );
		m_shadeTool->GetView()->SetMode( View3D::TOP_VIEW );
		wxYield();
		wxMilliSleep( 50 );
		m_shadeTool->GetView()->ChangeMap( m_bitmap, m_mpp );
		m_shadeTool->GetView()->Refresh();
	}
	else
		wxMessageBox("Please setup a map image first.");
}

void LocationSetup::OnRemoveUnderlay( wxCommandEvent & )
{
	m_shadeTool->SwitchTo( PG_SCENE );
	m_shadeTool->GetView()->SetMode( View3D::TOP_VIEW );
	wxYield();
	wxMilliSleep( 50 );
	m_shadeTool->GetView()->ChangeMap( wxNullBitmap, 1 );
	m_shadeTool->GetView()->Refresh();
}

void LocationSetup::OnImportMapImage( wxCommandEvent &evt )
{
	wxBitmap map( wxNullBitmap );
	if ( evt.GetId() == ID_LOAD_MAP_IMAGE )
	{
		wxFileDialog dlg( this, "Select a map image file", wxEmptyString, wxEmptyString,
			"Image Files (*.bmp;*.jpg;*.png)|*.bmp;*.jpg;*.png", wxFD_OPEN );

		if ( dlg.ShowModal() != wxID_OK )
			return;

		if ( ! map.LoadFile( dlg.GetPath(), wxBITMAP_TYPE_ANY ) )
		{
			wxMessageBox("Error loading selected image file:\n\n" + dlg.GetPath() );
			return;
		}
	}
	else
	{
		// paste clipboard
		if (wxTheClipboard->Open())
		{
			wxBitmapDataObject bitobj;
			if (wxTheClipboard->GetData( bitobj ))
			{
				map = bitobj.GetBitmap();
				wxTheClipboard->Close();
			}
		}

		if ( map.IsNull() )
		{
			wxMessageBox("No image data is in the clipboard");
			return;
		}
	}

	m_unannotatedBitmap = map;	
	UpdateMap();
	UpdateScale();
}

void LocationSetup::UpdateMap()
{
	m_bitmap = m_unannotatedBitmap;

	wxMemoryDC dc( m_bitmap );
	wxFont font( *wxNORMAL_FONT );
	font.SetWeight( wxFONTWEIGHT_BOLD );
	dc.SetFont( font );
	dc.SetTextForeground( *wxWHITE );
	dc.SetPen( wxPen( *wxWHITE, 2 ) );
	dc.DrawLine( 2, 2, 102, 2 );
	dc.DrawLine( 2, 2, 2, 6 );
	dc.DrawLine( 102, 2, 102, 6 );
	dc.DrawText( wxString::Format("%0.2lf m", m_mpp*100), 5, 3 );

	m_bitmapCtrl->SetBitmap( m_bitmap );
	m_bitmapCtrl->Refresh();

	m_scrollWin->SetScrollbars(1, 1, m_bitmap.GetWidth(), m_bitmap.GetHeight());
	m_scrollWin->SetScrollRate( 20, 20 );
}

void LocationSetup::UpdateScale()
{
	wxString text = wxGetTextFromUser( "Please enter image scale in meters per 100 pixels", "Scale", 
		wxString::Format("%lg", m_mpp*100 ) );
	if ( text.IsEmpty() )
		return;

	m_mpp = wxAtof( text )/100.0;
	if ( m_mpp < 0.0001 ) m_mpp = 0.0001;
	if ( m_mpp > 10 ) m_mpp = 10;
	
	UpdateMap();
}

void LocationSetup::OnManualScale( wxCommandEvent & )
{
	UpdateScale();
}


void LocationSetup::Write( wxOutputStream &os )
{
	wxDataOutputStream out( os );
	out.Write8( 0x94 );
	out.Write8( 1 );
	
	out.WriteString( m_address->GetValue() );
	out.WriteDouble( m_lat->Value() );
	out.WriteDouble( m_lon->Value() );
	out.WriteDouble( m_tz->Value() );
	out.WriteDouble( m_mpp );
	out.Write32( m_zoomLevel );

	out.Write8( m_unannotatedBitmap.IsNull() ? 0 : 1 ); // has map or not
	if ( !m_unannotatedBitmap.IsNull() )
	{
		wxImage img = m_unannotatedBitmap.ConvertToImage();
		wxPNGHandler().SaveFile( &img, os, false );
	}

	out.Write8( 0x94 );
}

bool LocationSetup::Read( wxInputStream &is )
{
	wxDataInputStream in( is );

	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8();

	m_address->ChangeValue( in.ReadString() );
	m_lat->SetValue( in.ReadDouble() );
	m_lon->SetValue( in.ReadDouble() );
	m_tz->SetValue( in.ReadDouble() );
	m_mpp = in.ReadDouble();
	m_zoomLevel = in.Read32();

	wxUint8 has_map = in.Read8();
	if ( has_map != 0 )
	{
		wxImage img;
		wxPNGHandler().LoadFile( &img, is, false );
		m_unannotatedBitmap = wxBitmap(img);
	}

	UpdateMap();

	return code == in.Read8();
}

enum { ID_PROPGRID = wxID_HIGHEST+959, ID_OBJLIST, ID_DUPLICATE, ID_DELETE };

BEGIN_EVENT_TABLE( ObjectEditor, wxPanel )
    EVT_PG_CHANGED( ID_PROPGRID, ObjectEditor::OnPropertyGridChange )
    EVT_PG_CHANGING( ID_PROPGRID, ObjectEditor::OnPropertyGridChanging )
	EVT_LISTBOX( ID_OBJLIST, ObjectEditor::OnObjectList )
	EVT_CHECKLISTBOX( ID_OBJLIST, ObjectEditor::OnObjectCheckList )	
	EVT_BUTTON( ID_DUPLICATE, ObjectEditor::OnCommand )
	EVT_BUTTON( ID_DELETE, ObjectEditor::OnCommand )
END_EVENT_TABLE()



#define SPACING 2

ObjectEditor::ObjectEditor( wxWindow *parent, int id, View3D *view, 
	const wxPoint &pos, const wxSize &size  )
	: wxPanel( parent, id, pos, size, wxTAB_TRAVERSAL ),
	m_view( view ),
	m_curObject( 0 )
{
	SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

	m_objList = new wxCheckListBox(this, ID_OBJLIST, wxDefaultPosition, wxDefaultSize, 0, 0, wxBORDER_NONE );
	m_propGrid = new wxPropertyGrid(this, ID_PROPGRID, wxDefaultPosition, wxDefaultSize, wxPG_SPLITTER_AUTO_CENTER|wxBORDER_NONE );
	
	m_duplicate = new wxMetroButton( this, ID_DUPLICATE, "Duplicate", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT );
	m_delete = new wxMetroButton( this, ID_DELETE, "Delete", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_SMALLFONT );

	wxBoxSizer *objtools = new wxBoxSizer( wxHORIZONTAL );
	objtools->Add( m_duplicate, 0, wxALL, 0 );
	objtools->Add( m_delete, 0, wxALL, 0 );
	objtools->AddStretchSpacer();

	m_duplicate->Hide();
	m_delete->Hide();

	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( m_objList, 1, wxALL|wxEXPAND, 0 );
	sizer->Add( objtools, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( m_propGrid, 1, wxALL|wxEXPAND, 0 );
	SetSizer( sizer );
}

ObjectEditor::~ObjectEditor()
{
	SetObject( 0 );
}


void ObjectEditor::UpdateObjectList( )
{
	m_objList->Clear();

	if ( !m_view ) return;

	wxString sel = m_objList->GetStringSelection();
	std::vector<VObject*> list = m_view->GetObjects();
	for( size_t i=0; i<list.size(); i++ )
	{
		m_objList->Append( list[i]->Property("Name").GetString() + " (" + list[i]->GetTypeName() + ")" );
		m_objList->Check( m_objList->GetCount()-1, list[i]->IsVisible() );
	}

	if ( !sel.IsEmpty() ) m_objList->SetStringSelection( sel );
}


void ObjectEditor::UpdatePropertyValues()
{
	for ( size_t i=0;i<m_curProps.size(); i++ )
		ValueToPropGrid( m_curProps[i] );
}

void ObjectEditor::SetObject( VObject *obj )
{	
	m_curObject = 0;

	m_duplicate->Hide();
	m_delete->Hide();

	// clear the property grid
	m_curProps.clear();
	m_propGrid->Clear();
	m_objList->SetSelection( m_objList->GetSelection(), false );

	if ( obj == 0 ) 
	{
		Layout();
		return;
	}

	m_curObject = obj;

	if ( m_view != 0 )
	{
		int index = m_view->GetObjectIndex( obj );
		if ( m_objList->GetSelection() != index )
			m_objList->SetSelection( index );
	}

	wxArrayString list = obj->Properties();
	for ( size_t i=0; i<list.Count();i++ )
	{
		VProperty &p = obj->Property( list[i] );
		
		wxPGProperty *pg = 0;
		switch( p.GetType() )
		{
		case VProperty::DOUBLE:
			pg = new wxFloatProperty( list[i], wxPG_LABEL );
			break;
		case VProperty::INTEGER:
			if ( p.GetChoices().size() == 0 )
				pg = new wxIntProperty( list[i], wxPG_LABEL );
			else
				pg = new wxEnumProperty( list[i], wxPG_LABEL, p.GetChoices() );
			break;
		case VProperty::STRING:
			pg =  new wxStringProperty( list[i], wxPG_LABEL);
			break;
		case VProperty::BOOLEAN:
			pg =  new wxBoolProperty( list[i], wxPG_LABEL );
			break;
		case VProperty::COLOUR:
			pg = new wxColourProperty( list[i], wxPG_LABEL );
			pg->SetAttribute( "HasAlpha", true );
			break;
		}

		if ( pg != 0 )
		{
			m_propGrid->Append( pg );
			pgpinfo x;
			x.name = list[i];
			x.type = p.GetType();
			x.pgp = pg;
			m_curProps.push_back( x );
			ValueToPropGrid( x );
		}
	}

	m_duplicate->Show();
	m_delete->Show();
	Layout();
}

void ObjectEditor::ValueToPropGrid( pgpinfo &p )
{
	if ( !m_curObject ) return;

	VProperty &vp = m_curObject->Property(p.name);

	switch( vp.GetType() )
	{
	case VProperty::BOOLEAN:
		m_propGrid->SetPropertyValue( p.name,  wxVariant( vp.GetBoolean() ) );
		break;
	case VProperty::DOUBLE:
		m_propGrid->SetPropertyValue( p.name,  wxVariant( vp.GetDouble() ) );
		break;
	case VProperty::INTEGER:
		m_propGrid->SetPropertyValue( p.name,  wxVariant( vp.GetInteger() ) );
		break;
	case VProperty::STRING:
		m_propGrid->SetPropertyValue( p.name,  wxVariant( vp.GetString() ) );
		break;
	case VProperty::COLOUR:
		m_propGrid->SetPropertyValue( p.name,  wxVariant( vp.GetColour() ) );
		break;
	}
}

void ObjectEditor::PropGridToValue( pgpinfo &p )
{
	if ( !m_curObject ) return;
		
	VProperty &vp = m_curObject->Property(p.name);
	
	wxAny value = p.pgp->GetValue();

	if ( vp.GetType() == VProperty::INVALID ) return;

	switch( vp.GetType() )
	{
	case VProperty::BOOLEAN:
		vp.Set( wxANY_AS(value, bool) );
		break;
	case VProperty::DOUBLE:
		vp.Set( wxANY_AS(value, double) );
		break;
	case VProperty::INTEGER:
		vp.Set( wxANY_AS(value, int) );
		break;
	case VProperty::STRING:
		vp.Set( wxANY_AS(value, wxString) );
		break;
	case VProperty::COLOUR:
		vp.Set( wxANY_AS(value, wxColour) );
		break;
	}

	if ( m_view )
	{
		m_view->UpdateModel( m_curObject );
		m_view->Refresh();
		
		if ( p.name.Lower() == "name" )
		{
			int idx = m_view->GetObjectIndex( m_curObject );
			if ( idx >= 0 )
				m_objList->SetString(idx, vp.GetString() + " (" + m_curObject->GetTypeName() + ")" );
		}		
	}
}


void ObjectEditor::OnPropertyGridChange(wxPropertyGridEvent &evt)
{
    wxPGProperty* p = evt.GetProperty();
	for ( size_t i=0;i<m_curProps.size();i++ )
		if ( m_curProps[i].pgp == p )
			PropGridToValue( m_curProps[i] );
}

void ObjectEditor::OnPropertyGridChanging(wxPropertyGridEvent &evt)
{
}

void ObjectEditor::OnCommand( wxCommandEvent &evt )
{
	if ( !m_view ) return;

	switch( evt.GetId() )
	{
	case ID_DELETE:
		m_view->DeleteSelected();
		SetObject( 0 );
		break;
	case ID_DUPLICATE:
		m_view->DuplicateSelected();
		break;
	}
}

void ObjectEditor::OnObjectList( wxCommandEvent &evt )
{
	if ( !m_view ) return;

	if ( VObject *obj = m_view->GetObject( (size_t) m_objList->GetSelection() ))
		m_view->Select( obj );
}

void ObjectEditor::OnObjectCheckList( wxCommandEvent &evt )
{
	if ( !m_view ) return;
	
	size_t sel = (size_t) evt.GetSelection();
	if ( VObject *obj = m_view->GetObject( sel ))
	{
		obj->Show( m_objList->IsChecked( sel ) );
		m_view->UpdateHandles( obj );
		m_view->Refresh();
	}
}


enum {  
		
	ID_GENERATE_DIURNAL = wxID_HIGHEST+321, ID_GENERATE_TIMESERIES, ID_GENERATE_DIFFUSE 
};


BEGIN_EVENT_TABLE( ShadeAnalysis, wxPanel )	
	EVT_BUTTON( ID_GENERATE_DIURNAL, ShadeAnalysis::OnGenerateDiurnal )
	EVT_BUTTON(ID_GENERATE_TIMESERIES, ShadeAnalysis::OnGenerateTimeSeries)
	EVT_BUTTON(ID_GENERATE_DIFFUSE, ShadeAnalysis::OnGenerateDiffuse)
	END_EVENT_TABLE()

ShadeAnalysis::ShadeAnalysis( wxWindow *parent, ShadeTool *st )
	: wxPanel( parent, wxID_ANY ),
	m_shadeTool( st )
{
	SetBackgroundColour( *wxWHITE );

	wxBoxSizer *tools = new wxBoxSizer( wxHORIZONTAL );

	tools->Add( new wxButton(this, ID_GENERATE_DIURNAL, "Diurnal analysis" ), 0, wxALL, 2 );
	tools->Add(new wxButton(this, ID_GENERATE_TIMESERIES, "Time series analysis"), 0, wxALL, 2);
	tools->Add(new wxButton(this, ID_GENERATE_DIFFUSE, "Diffuse analysis"), 0, wxALL, 2);

	tools->AddStretchSpacer();

	m_scroll_diffuse = new wxScrolledWindow(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxScrolledWindowStyle | wxBORDER_NONE);
	m_scroll = new wxScrolledWindow(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxScrolledWindowStyle | wxBORDER_NONE);

	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );	
	sizer->Add( tools, 0, wxALL|wxEXPAND, 2 );
	sizer->Add(m_scroll_diffuse, 1, wxALL | wxEXPAND, 0);
	sizer->Add(m_scroll, 4, wxALL | wxEXPAND, 0);

	SetSizer( sizer );
	
}


void ShadeAnalysis::OnGenerateDiffuse(wxCommandEvent &)
{
	SimulateDiffuse(true);
}

bool ShadeAnalysis::SimulateDiffuse(bool save)
	{
		bool success = false;

		wxProgressDialog pdlg("Shade calculation", "Computing...", 100, m_shadeTool,
		wxPD_SMOOTH | wxPD_CAN_ABORT | wxPD_APP_MODAL | wxPD_AUTO_HIDE);
#ifdef __WXMSW__
	pdlg.SetIcon(wxICON(appicon));
#endif

	pdlg.Show();

	double lat, lon, tz;
	m_shadeTool->GetLocationSetup()->GetLocation(&lat, &lon, &tz);

	double azi, alt;
	const int *ndays = ::wxNDay;
	size_t i_azi, i_alt, c = 0;


	s3d::transform tr;
	tr.set_scale(SF_ANALYSIS_SCALE);

	s3d::scene sc(m_shadeTool->GetView()->GetScene());
	std::vector<s3d::shade_result> shresult;

	wxStopWatch sw;
	bool stopped = false;

	size_t azi_min, azi_max, azi_step;
	size_t alt_min, alt_max, alt_step;
	azi_min = 0;
	azi_max = 359;
	azi_step = 1;
	
	// alt= 1 to keep flat plates from having shading percent
	alt_min = 1;
	alt_max = 81;
	alt_step = 10;
	
	// best compromise speed and accuracy
	// closest to PVSyst
//	azi_step = 20;
//	alt_step = 10;


	size_t num_alt = 1 + (alt_max - alt_min)/alt_step;
	size_t num_azi = 1 + (azi_max - azi_min)/azi_step;
	size_t num_scenes =  num_alt * num_azi;

	wxMessageBox(wxString::Format("Diffuse scense: %d\n", (int)num_scenes));
	
	std::vector<surfshade> shade;
	InitializeSections( num_scenes, shade );

	for (i_azi = azi_min; i_azi<=azi_max && !stopped; i_azi += azi_step)
	{
		for (i_alt = alt_min; i_alt <= alt_max && !stopped; i_alt += alt_step)
		{
			azi = i_azi;
			alt = i_alt;
			if (c % 400 == 0)
			{
				int percent = (int)(100.0*c / num_scenes);
				if (!pdlg.Update(percent))
				{
					stopped = true;
					break;
				}
				else
					wxYieldIfNeeded();
			}


			// for nighttime full shading (fraction=1 and factor=0)
			// consistent with SAM shading factor of zero for night time
			//	double sf = 0;
			double scene_sf = 1;
			tr.rotate_azal(azi, alt);
			sc.build(tr);

			std::vector<s3d::shade_result> shresult;
			scene_sf = sc.shade(shresult);

			for (size_t k = 0; k<shresult.size(); k++)
			{
				int id = shresult[k].id;
				// find the correct shade group for this 'id'
				// and accumulate the total shaded and active areas
				for (size_t n = 1; n<shade.size(); n++)
				{
					std::vector<int> &ids = shade[n].ids;
					if (std::find(ids.begin(), ids.end(), id) != ids.end())
					{
						shade[n].shaded[c] += shresult[k].shade_area;
						shade[n].active[c] += shresult[k].active_area;
					}
				}
			}

			// store overall array shading factor
			shade[0].sfac[c] = 100.0f * scene_sf;

			// compute each group's shading factor from the overall areas
			for (size_t n = 1; n<shade.size(); n++)
			{
				double sf = 1;
				if (shade[n].active[c] != 0.0)
					sf = shade[n].shaded[c] / shade[n].active[c];

				shade[n].sfac[c] = 100.0f * sf;
			}
			c++;
		}
	}

	wxMessageBox( wxString::Format("c=%ld\n", (int)c ) );

	num_scenes = c;

	
//	wxMessageBox(wxString::Format("Diffuse shading (%d (%d) scenes in %d ms)", c, num_scenes,sw.Time()));
	if (save)
	{
		wxFileDialog dlg(this, "Diffuse Shading File Export", wxEmptyString, "diffuse_shade.csv", "*.*", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
		if (wxID_OK == dlg.ShowModal())
		{
			if (FILE *fp = fopen((const char*)dlg.GetPath().c_str(), "w"))
			{
				fprintf(fp, "azimuth,altitude,");
				for (size_t i = 0; i < shade.size(); i++)
					fprintf(fp, "%s %c", (const char*)shade[i].group.c_str(), i + 1 < shade.size() ? ',' : '\n');

				c = 0;
				for (i_azi = azi_min; i_azi<azi_max; i_azi += azi_step)
					for (i_alt = alt_min; i_alt < alt_max; i_alt += alt_step)
					{
					fprintf(fp, "%d,%d,", i_azi, i_alt);
					for (size_t j = 0; j < shade.size(); j++)
							fprintf(fp, "%lg%c", shade[j].sfac[c], j + 1 < shade.size() ? ',' : '\n');
						c++;
					}
				fclose(fp);
			}
			else
				wxMessageBox("Could not write to file:\n\n" + dlg.GetPath());
		}
	}
	
	int y = 0;
	// average shading factor over skydome
	std::vector<double> data(shade.size());
	m_diffuse_shade_percent.clear();
	m_diffuse_name.Clear();
	for (size_t j = 0; j < shade.size(); j++)
	{
		data[j] = 0;
		for (size_t i = 0; i < num_scenes; i++)
			data[j] += shade[j].sfac[i];
		data[j] /= num_scenes;
		m_diffuse_shade_percent.push_back(data[j]);
		m_diffuse_name.push_back(shade[j].group);
		if (j > 0) // j=0 taken to be minimum value as shown below
		{
			wxStaticText *st = new wxStaticText(m_scroll_diffuse, wxID_ANY, wxString::Format("%s: diffuse shade fraction = %lg %%", m_diffuse_name[j].c_str(), m_diffuse_shade_percent[j]));
			st->SetSize(10, y, 1300, 30);
			y += 30;
		}
	}

	/* Note that Chris Deline email 2/19/15 suggest to take minimum string value as 
	diffuse view factor for array. So, for single value will use minimum of the active 
	surfaces as suggested. If the active surfaces have group names, then the respective
	diffuse view factor will be applied to each subarray which is expected value 
	since, in SAM, the subarrays are assumed to be connected in parallel. So, the assumption 
	is that if not named, the active surfaces are referring to string connected arrays 
	and the minimum view factor or maximum shading percentage value will be used as follows: */
	if (shade.size() > 1)
	{
		for (size_t j = 1; j < shade.size(); j++)
		{
			if (m_diffuse_shade_percent[j] > m_diffuse_shade_percent[0])
				m_diffuse_shade_percent[0] = m_diffuse_shade_percent[j];
		}
	}
	wxStaticText *st = new wxStaticText(m_scroll_diffuse, wxID_ANY, wxString::Format("%s: diffuse shade fraction = %lg %%", m_diffuse_name[0].c_str(), m_diffuse_shade_percent[0]));
	st->SetSize(10, y, 1300, 30);
	y += 30;

	m_scroll_diffuse->SetScrollbars(1, 1, 1100, y);

	success = true;

	return success;
}







bool ShadeAnalysis::SimulateTimeseries( int minute_step, std::vector<surfshade> &shade )
{
	int allowed_steps[] = { 1, 5, 10, 15, 20, 30, 60, 0 };
	int ii=0;
	bool tsok = false;
	while( allowed_steps[ii] != 0 )
	{
		if ( minute_step == allowed_steps[ii++] )
		{
			tsok = true;
			break;
		}
	}

	if ( !tsok ) return false;


	wxProgressDialog pdlg( "Time series shade calculation", "Computing...", 100, m_shadeTool,
		wxPD_SMOOTH|wxPD_CAN_ABORT|wxPD_APP_MODAL|wxPD_AUTO_HIDE );
#ifdef __WXMSW__
	pdlg.SetIcon(  wxICON( appicon) );
#endif
	pdlg.Show();

	double lat, lon, tz;
	m_shadeTool->GetLocationSetup()->GetLocation( &lat, &lon, &tz );

	double azi, zen, alt;
	const int *ndays = ::wxNDay;
	size_t m, d, h, jj, c = 0;

	int step_per_hour = 60/minute_step;
	int npoints = surfshade::nvalues( minute_step );
	InitializeSections( npoints, shade );

	s3d::transform tr;
	tr.set_scale( SF_ANALYSIS_SCALE );

	s3d::scene sc( m_shadeTool->GetView()->GetScene() );	
	std::vector<s3d::shade_result> shresult;

	int last_percent = 0.0;
	bool stopped = false;

	for ( m=0;m<12 && !stopped;m++ )
	{
		for ( d=0;d<ndays[m] && !stopped;d++ )
		{
			for ( h=0;h<24 && !stopped;h++ )
			{
				for( jj=0;jj<step_per_hour;jj++ )
				{
					int percent = (int)( 100.0*c/npoints );
					if ( last_percent != percent )
					{
						if ( !pdlg.Update( percent ) ) 
						{
							stopped = true;
							break;
						}
						else
							wxYieldIfNeeded();

						last_percent = percent;
					}

					s3d::sun_pos( 1970, m+1, d+1, h, jj*minute_step + 0.5*minute_step, lat, lon, tz, &azi, &zen );				
					alt = 90-zen;

					// for nighttime full shading (fraction=1 and factor=0)
					// consistent with SAM shading factor of zero for night time
					//	double sf = 0;
					double scene_sf = 1; 
					if (alt > 0)
					{
						tr.rotate_azal( azi, alt );
						sc.build( tr );
					
						std::vector<s3d::shade_result> shresult;
						scene_sf = sc.shade( shresult );

						for ( size_t k=0;k<shresult.size();k++ )
						{
							int id = shresult[k].id;
							// find the correct shade group for this 'id'
							// and accumulate the total shaded and active areas
							for( size_t n=1;n<shade.size();n++ )
							{
								std::vector<int> &ids = shade[n].ids;
								if ( std::find( ids.begin(), ids.end(), id ) != ids.end() )
								{
									shade[n].shaded[c] += shresult[k].shade_area;
									shade[n].active[c] += shresult[k].active_area;
								}
							}
						}
					}
			
					// store overall array shading factor
					shade[0].sfac[c] =  100.0f * scene_sf;

					// compute each group's shading factor from the overall areas
					for( size_t n=1;n<shade.size();n++ )
					{
						double sf = 1;
						if ( shade[n].active[c] != 0.0 )
							sf = shade[n].shaded[c] / shade[n].active[c];

						shade[n].sfac[c] =  100.0f * sf;
					}

					c++;
				}
			}
		}
	}

	return true;
}



void ShadeAnalysis::OnGenerateTimeSeries( wxCommandEvent & )
{
	std::vector<surfshade> shade;

	wxStopWatch sw;
	wxString sNewValue = wxGetTextFromUser("Enter time step in minutes:\n\nAllowed values: 1, 5, 10, 15, 20, 30, 60", "Time series calculation", "60");
	double * dMin;
	sNewValue.ToDouble(dMin);
	int min = (int)(*dMin);
	
	if ( !SimulateTimeseries( min, shade ) )
	{
		wxMessageBox("Error or cancellation in time series shade calculation");
		return;
	}

	long time = sw.Time();
	
	int nstep = surfshade::nvalues( min );

	wxMessageBox(wxString::Format("Time series shading (%d scenes in %d ms)", nstep, time));
	

	wxString outputfile( wxFileName(m_shadeTool->GetFileName()).GetName() );
	if (outputfile.IsEmpty() ) outputfile = "shade";
	outputfile += wxString::Format("_%dmin.csv", min );
	wxFileDialog dlg( this, "Time series shading file output", wxEmptyString, outputfile, "*.*", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if ( wxID_OK == dlg.ShowModal() )
	{
		if ( FILE *fp = fopen( (const char*)dlg.GetPath().c_str(), "w" ) )
		{
			
#ifdef S3D_STANDALONE
			extern wxString g_appTitle;
#else
			wxString g_appTitle("Integrated SAM Shade Calculator");
#endif
			fprintf(fp, "Shading results generated by %s on %s in %.3lf seconds\n", (const char*) g_appTitle.c_str(), (const char*)wxNow().c_str(), 0.001*time );
			fprintf(fp, "Geometry file: %s\n", (const char*)m_shadeTool->GetFileName().c_str() );
			wxString addr(m_shadeTool->GetLocationSetup()->GetAddress() );
			addr.Replace(",", " ");
			fprintf(fp, "Site address: %s\n", (const char*)addr.c_str() );
			double lat, lon, tz;
			m_shadeTool->GetLocationSetup()->GetLocation( &lat, &lon, &tz );
			fprintf(fp, "Latitude,%lg,Longitude,%lg,Time zone,%lg\n", lat, lon, tz );
			fputs( "Month,Day,Hour,Minute,", fp );
			for( size_t i=0;i<shade.size();i++ )
				fprintf( fp, "%s %%%c", (const char*)shade[i].group.c_str(), i+1 < shade.size() ? ',' : '\n' );

			int c=0;
			int step_per_hour = 60/min;
			for( int m=1;m<=12;m++ )
			{
				for( int d=1;d<=wxNDay[m-1];d++ )
				{
					for( int h=0;h<24;h++ )
					{
						for( int jj=0;jj<step_per_hour;jj++ )
						{
							double fminval = jj*min + 0.5*min;
							fprintf(fp, "%d,%d,%d,%lg,", m, d, h, fminval );
							for( size_t j=0;j<shade.size();j++ )
								fprintf( fp, "%.3lf%c", shade[j].sfac[c], j+1 < shade.size() ? ',' : '\n' );

							c++;
						}
					}
				}
			}

			fclose(fp);
		}
		else
			wxMessageBox( "Could not write to file:\n\n" + dlg.GetPath() );
	}

	if (wxYES == wxMessageBox("View time series shading factor results?", "Query", wxYES_NO, this ))
	{	
		wxFrame *frame = new wxFrame( 0, wxID_ANY, 
			wxString::Format("Shade fractions (computation in %d ms)", time), 
			wxDefaultPosition, wxSize(900,700) );

		wxDVPlotCtrl *dview = new wxDVPlotCtrl( frame );	
		std::vector<double> data(nstep);
		for( size_t j=0;j<shade.size();j++ )
		{
			for( size_t i=0;i<nstep;i++ )
				data[i] = shade[j].sfac[i];

			wxDVArrayDataSet *dset = new wxDVArrayDataSet(shade[j].group, "% Shaded", min/60.0, data);
			dset->SetOffset( 0.5*min/60.0 );
			dview->AddDataSet( dset );
		}
		dview->SelectDataOnBlankTabs();
		frame->Show(); 
	}
	
}




void ShadeAnalysis::OnGenerateDiurnal( wxCommandEvent & )
{
	SimulateDiurnal();
}

void ShadeAnalysis::InitializeSections( int mode, std::vector<surfshade> &shade )
{
	shade.clear();
	shade.push_back( surfshade(mode, "Entire Array") ); // overall system shade

	// setup shading result storage for each group
	std::vector<VObject*> objs = m_shadeTool->GetView()->GetObjects();
	for( size_t i=0;i<objs.size();i++ )
	{
		if ( VActiveSurfaceObject *surf = dynamic_cast<VActiveSurfaceObject*>( objs[i] ) )
		{
			wxString grp = surf->Property("Group").GetString();
			if ( grp.IsEmpty() ) continue;

			int index = -1;
			for( int k=0;k<shade.size();k++ )
				if ( shade[k].group == grp )
					index = k;

			if ( index < 0 )
			{
				shade.push_back( surfshade( mode, grp ) );
				index = shade.size()-1;
			}

			surfshade &ss = shade[index];
			ss.surfaces.push_back( surf );
			if ( std::find( ss.ids.begin(), ss.ids.end(), surf->GetId() ) == ss.ids.end() )
				ss.ids.push_back( surf->GetId() );
		}
	}
}

bool ShadeAnalysis::SimulateDiurnal()
{	
	bool success = false;
	wxProgressDialog pdlg( "Shade calculation", "Computing...", 288, m_shadeTool,
		wxPD_SMOOTH|wxPD_CAN_ABORT|wxPD_APP_MODAL|wxPD_AUTO_HIDE );
#ifdef __WXMSW__
	pdlg.SetIcon( wxICON( appicon) );
#endif
	pdlg.Show();
	
	wxYield();

	double lat, lon, tz;
	m_shadeTool->GetLocationSetup()->GetLocation( &lat, &lon, &tz );

	double azi, zen, alt;
	size_t m, d, h;

	std::vector<surfshade> shade;	
	InitializeSections( surfshade::DIURNAL, shade );
	
	s3d::transform tr;
	tr.set_scale( SF_ANALYSIS_SCALE );

	s3d::scene sc( m_shadeTool->GetView()->GetScene() );

	wxStopWatch sw;
	bool stopped = false;
	int i=0;
	for ( m=0;m<12 && !stopped;m++ )
	{
		d=14;
		for ( h=0;h<24 && !stopped;h++ )
		{
			pdlg.Update( i++ );
			wxYieldIfNeeded();

			s3d::sun_pos( 1970, m+1, d+1, h, 30.0, lat, lon, tz, &azi, &zen );							
			alt = 90-zen;

			// for nighttime full shading (fraction=1 and factor=0)
			// consistent with SAM shading factor of zero for night time
			//	double sf = 0;
			double scene_sf = 1; 
			if (alt > 0)
			{
				tr.rotate_azal( azi, alt );
				sc.build( tr );
					
				std::vector<s3d::shade_result> shresult;
				scene_sf = sc.shade( shresult );

				for ( size_t k=0;k<shresult.size();k++ )
				{
					int id = shresult[k].id;

					// find the correct shade group for this 'id'
					// and accumulate the total shaded and active areas
					for( size_t n=1;n<shade.size();n++ )
					{
						std::vector<int> &ids = shade[n].ids;
						if ( std::find( ids.begin(), ids.end(), id ) != ids.end() )
						{
							shade[n].shaded(m,h) += shresult[k].shade_area;
							shade[n].active(m,h) += shresult[k].active_area;
						}
					}
				}
			}
			
			// store overall array shading factor
			shade[0].sfac( m, h ) =  100.0f * scene_sf;

			// compute each group's shading factor from the overall areas
			for( size_t n=1;n<shade.size();n++ )
			{
				double sf = 1;
				if ( shade[n].active(m,h) != 0.0 )
					sf = shade[n].shaded(m,h) / shade[n].active(m,h);

				shade[n].sfac(m,h) =  100.0f * sf;
			}


			if (pdlg.WasCancelled())
				stopped = true;
		}
	}

	int y = 0;
	for( size_t i=0;i<shade.size();i++ )
	{
		if ( i >= m_mxhList.size() )
			m_mxhList.push_back( new AFMonthByHourFactorCtrl( m_scroll, wxID_ANY ) );

		AFMonthByHourFactorCtrl *mxh = m_mxhList[i];
		mxh->SetSize( 0, y, 1300, 360 );
		mxh->SetTitle( shade[i].group );
		mxh->SetData( shade[i].sfac );
		mxh->SetLegend( "Shade Loss (%): 0=no shade, 100=fully shaded" );
		y += 360;
	}

	while ( m_mxhList.size() > shade.size() )
	{
		m_mxhList[ m_mxhList.size()-1 ]->Destroy();
		m_mxhList.erase( m_mxhList.end()-1 );
	}

	m_scroll->SetScrollbars( 1, 1, 1100, y );
	success = true;
	return success;
}

size_t ShadeAnalysis::GetDiurnalCount()
{
	return m_mxhList.size();
}

size_t ShadeAnalysis::GetDiffuseCount()
{
	return m_diffuse_shade_percent.size();
}

void ShadeAnalysis::GetDiurnal(size_t i, matrix_t<float> *mxh, wxString *name)
{
	if (i < m_mxhList.size())
	{
		AFMonthByHourFactorCtrl *c = m_mxhList[i];
		(*mxh) = c->GetData();
		(*name) = c->GetTitle();
	}
}

void ShadeAnalysis::GetDiffuse(size_t i, double *shade_percent, wxString *name)
{
	if ((i < m_diffuse_shade_percent.size()) && (i<m_diffuse_name.Count()))
	{
		(*shade_percent) = m_diffuse_shade_percent[i];
		(*name) = m_diffuse_name[i];
	}
}


enum { ID_SLIDER = wxID_HIGHEST+441, ID_LAST_SLIDER = ID_SLIDER+100, 
	ID_LOCATION, ID_CREATE, ID_GRAPHICS, ID_ANALYSIS, ID_VIEW_XYZ, ID_VIEW_XY, ID_VIEW_XZ, ID_FEEDBACK,
	ID_OBJECT_ID, ID_OBJECT_IDMAX=ID_OBJECT_ID+100,

	// debugging
	ID_AZIMUTH, ID_ALTITUDE, ID_SCALE, ID_TRI_TEST, 
	ID_EXPORT_HOURLY, ID_EXPORT_DIURNAL
};

BEGIN_EVENT_TABLE( ShadeTool, wxPanel )
	EVT_VIEW3D_UPDATE_OBJECTS( ID_GRAPHICS, ShadeTool::OnUpdateObjectList )
	EVT_VIEW3D_UPDATE_PROPERTIES( ID_GRAPHICS, ShadeTool::OnUpdateProperties )
	EVT_VIEW3D_UPDATE_SELECTION( ID_GRAPHICS, ShadeTool::OnUpdateSelection )

	EVT_BUTTON( wxID_OPEN, ShadeTool::OnCommand )
	EVT_BUTTON( wxID_SAVE, ShadeTool::OnCommand )
	EVT_BUTTON( ID_ANALYSIS, ShadeTool::OnCommand )
	EVT_BUTTON( ID_LOCATION, ShadeTool::OnCommand )
	EVT_BUTTON( ID_CREATE, ShadeTool::OnCommand )
	EVT_BUTTON( ID_VIEW_XYZ, ShadeTool::OnCommand )
	EVT_BUTTON( ID_VIEW_XY, ShadeTool::OnCommand )
	EVT_BUTTON( ID_VIEW_XZ, ShadeTool::OnCommand )
	EVT_BUTTON( ID_FEEDBACK, ShadeTool::OnCommand )
	EVT_BUTTON( wxID_HELP, ShadeTool::OnCommand )
	

	EVT_MENU_RANGE( ID_OBJECT_ID, ID_OBJECT_IDMAX, ShadeTool::OnCreateObject )
	
	// handlers for debugging tools
	EVT_TEXT_ENTER(ID_AZIMUTH, ShadeTool::OnDebugCommand)
	EVT_TEXT_ENTER(ID_ALTITUDE, ShadeTool::OnDebugCommand)
	EVT_TEXT_ENTER(ID_SCALE, ShadeTool::OnDebugCommand)
	EVT_BUTTON(ID_TRI_TEST, ShadeTool::OnDebugCommand)
	EVT_BUTTON(ID_EXPORT_HOURLY, ShadeTool::OnDebugCommand)
	EVT_BUTTON(ID_EXPORT_DIURNAL, ShadeTool::OnDebugCommand)
END_EVENT_TABLE()


ShadeTool::ShadeTool( wxWindow *parent, int id, const wxString &data_path )
	: wxPanel( parent, id )
{
	SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

	wxBoxSizer *sizer_tool = new wxBoxSizer( wxHORIZONTAL );
#ifdef S3D_STANDALONE
	sizer_tool->Add( new wxMetroButton( this, wxID_OPEN, "Open" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, wxID_SAVE, "Save" ), 0, wxALL|wxEXPAND, 0 );
#endif
	sizer_tool->Add( new wxMetroButton( this, ID_LOCATION, "Location" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_CREATE, "Create", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_DOWNARROW ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_VIEW_XYZ, "3D scene" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_VIEW_XY, "Bird's eye" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_VIEW_XZ, "Elevations" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_ANALYSIS, "Analyze" ), 0, wxALL|wxEXPAND, 0 );
	
#ifndef S3D_STANDALONE
	sizer_tool->Add( new wxMetroButton( this, wxID_OPEN, "Import" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, wxID_SAVE, "Export" ), 0, wxALL|wxEXPAND, 0 );
#endif

	sizer_tool->AddStretchSpacer();
	

#ifdef S3D_STANDALONE
	sizer_tool->Add( new wxMetroButton( this, ID_FEEDBACK, "Feedback" ), 0, wxALL|wxEXPAND, 0 );
#endif

	sizer_tool->Add( new wxMetroButton( this, wxID_HELP, "Help" ), 0, wxALL|wxEXPAND, 0 );

#ifndef S3D_STANDALONE
	sizer_tool->Add( new wxMetroButton( this, wxID_OK, "Save and close" ), 0, wxALL|wxEXPAND, 0 );
#endif
	
	m_book = new wxSimplebook( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE );
	
	m_location = new LocationSetup( m_book, this );

	m_split = new wxSplitterWindow( m_book );
	m_view = new View3D( m_split, ID_GRAPHICS );
	m_sceneParams = new ObjectEditor( m_split, wxID_ANY, m_view );	
	m_split->SetMinimumPaneSize( 20 );
	m_split->SplitVertically( m_sceneParams, m_view, 200 );	
	

	// debugging tool panel
	m_debugPanel = new wxPanel( this );
	m_debugPanel->SetBackgroundColour( *wxWHITE );
	wxBoxSizer *debug_sizer = new wxBoxSizer( wxHORIZONTAL );
	
	debug_sizer->Add( new wxStaticText( m_debugPanel, wxID_ANY, "Azi:"),0,wxALIGN_CENTER_VERTICAL|wxALL,4);
	debug_sizer->Add( m_txtAzi = new wxTextCtrl( m_debugPanel, ID_AZIMUTH, "143", wxDefaultPosition, wxSize(70,24),  wxTE_PROCESS_ENTER), 0, wxALL|wxEXPAND, 2);
	
	debug_sizer->Add(new wxStaticText(m_debugPanel, wxID_ANY, "Alt:"), 0, wxALIGN_CENTER_VERTICAL|wxALL, 4);
	debug_sizer->Add(m_txtAlt = new wxTextCtrl(m_debugPanel, ID_ALTITUDE, "28", wxDefaultPosition, wxSize(70, 24), wxTE_PROCESS_ENTER), 0, wxALL | wxEXPAND, 2);
	
	debug_sizer->Add(new wxStaticText(m_debugPanel, wxID_ANY, "scale:"), 0, wxALIGN_CENTER_VERTICAL|wxALL, 4);
	debug_sizer->Add(m_txtScale = new wxTextCtrl(m_debugPanel, ID_SCALE, "4", wxDefaultPosition, wxSize(70, 24), wxTE_PROCESS_ENTER), 0, wxALL | wxEXPAND, 2);
	
	debug_sizer->Add(new wxButton(m_debugPanel, ID_EXPORT_HOURLY, "Export Hourly"), 0, wxALL | wxEXPAND, 0);
	debug_sizer->Add(new wxButton(m_debugPanel, ID_EXPORT_DIURNAL, "Export Diurnal"), 0, wxALL | wxEXPAND, 0);
	debug_sizer->Add(new wxButton(m_debugPanel, ID_TRI_TEST, "Triangle Test"), 0, wxALL | wxEXPAND, 0);
	
	m_debugPanel->SetSizer( debug_sizer );
	m_debugPanel->Show( false );

	m_analysis = new ShadeAnalysis( m_book, this );

#ifdef S3D_STANDALONE
	wxString help_index( "file:///" + data_path + "/help/index.html" );
	//wxShowTextMessageDialog( help_index );
	m_helpViewer = wxWebView::New( m_book, wxID_ANY, help_index,
		wxDefaultPosition, wxDefaultSize, wxWebViewBackendDefault, wxBORDER_NONE );
#endif

	wxBoxSizer *sizer_main = new wxBoxSizer( wxVERTICAL );
	sizer_main->Add( sizer_tool, 0, wxALL|wxEXPAND, 0 );
	sizer_main->Add( m_book, 1, wxALL|wxEXPAND, 0 );
	sizer_main->Add( m_debugPanel, 0, wxALL|wxEXPAND, 0 );
	SetSizer( sizer_main );
	
	m_book->AddPage( m_location, "Location" );
	m_book->AddPage( m_split, "Scene Editor" );
	m_book->AddPage( m_analysis, "Analysis page" );

#ifdef S3D_STANDALONE
	m_book->AddPage( m_helpViewer, "Help" );
#else
	m_book->SetSelection( PG_SCENE );
#endif
}
	
View3D *ShadeTool::GetView()
{
	return m_view;
}

LocationSetup *ShadeTool::GetLocationSetup()
{
	return m_location;
}

void ShadeTool::SwitchTo( int page )
{
	m_book->SetSelection( page );
}


void ShadeTool::Save()
{
	wxFileDialog dlg( this, "Save scene to file", wxPathOnly(m_fileName), 
		m_fileName, "Scene file (*.s3d)|*.s3d", wxFD_SAVE );
	if ( dlg.ShowModal() == wxID_OK )
	{
		if ( m_fileName != dlg.GetPath()
			&& wxFileExists( dlg.GetPath() )
			&& wxNO == wxMessageBox("Overwrite existing file?\n\n" + dlg.GetPath(), "Query", wxYES_NO) )
			return;

		if ( WriteToFile( dlg.GetPath() ) )
			m_fileName = dlg.GetPath();
		else
			wxMessageBox("Could not open file for writing:\n\n" + dlg.GetPath() );
	}
}

bool ShadeTool::Load()
{
	wxFileDialog dlg( this, "Load scene from file", wxPathOnly(m_fileName),
		wxEmptyString, "Scene file (*.s3d)|*.s3d", wxFD_OPEN );
	if ( dlg.ShowModal() == wxID_OK )
	{
		if (LoadFromFile( dlg.GetPath() ))
		{
			m_fileName = dlg.GetPath();
			return true;
		}
		else
			wxMessageBox("Could not open file for reading:\n\n" + dlg.GetPath() );
	}

	return false;
}

bool ShadeTool::WriteToFile( const wxString &file )
{
	wxBusyInfo busy("Writing shading data to file: " + wxFileNameFromPath( file ), this );
	wxYield();
	wxMilliSleep( 50 );
	wxFFileOutputStream fos( file );
	if ( !fos.IsOk() ) return false;
	Write( fos );
	return true;
}

bool ShadeTool::LoadFromFile( const wxString &file )
{
	wxFFileInputStream fis( file );
	if ( !fis.IsOk() || !Read( fis ) ) return false;
	else return true;
}

void ShadeTool::Write( wxOutputStream &os )
{
	wxDataOutputStream out(os);
	out.Write8( 0x84 );
	out.Write8( 1 );
	m_location->Write( os );
	m_view->Write( os );
	out.Write8( 0x84 );
}

bool ShadeTool::Read( wxInputStream &is )
{
	wxDataInputStream in(is);
	wxUint8 code = in.Read8();
	in.Read8();

	bool ok1 = m_location->Read( is );
	bool ok2 = m_view->Read( is );

	return in.Read8() == code && ok1 && ok2;
}

bool ShadeTool::SimulateDiurnal(std::vector<diurnal> &result)
{
	result.clear();
	if (m_analysis->SimulateDiurnal())
	{
		size_t n = m_analysis->GetDiurnalCount();
		for (size_t i = 0; i<n; i++)
		{
			result.push_back(diurnal());
			diurnal &d = result[result.size() - 1];
			m_analysis->GetDiurnal(i, &d.mxh, &d.name);
		}
		return true;
	}
	else
		return false;
}

bool ShadeTool::SimulateDiffuse(std::vector<diffuse> &result)
{
	result.clear();
	if (m_analysis->SimulateDiffuse())
	{
		size_t n = m_analysis->GetDiffuseCount();
		for (size_t i = 0; i<n; i++)
		{
			result.push_back(diffuse());
			diffuse &d = result[result.size() - 1];
			m_analysis->GetDiffuse(i, &d.shade_percent, &d.name);
		}
		return true;
	}
	else
		return false;
}

void ShadeTool::OnUpdateObjectList( wxCommandEvent & )
{
	m_sceneParams->UpdateObjectList();
}

void ShadeTool::OnUpdateProperties( wxCommandEvent & )
{
	m_sceneParams->UpdatePropertyValues();
}

void ShadeTool::OnUpdateSelection( wxCommandEvent & )
{
	m_sceneParams->SetObject( m_view->GetFirstSelectedObject() );
}

void ShadeTool::OnCommand( wxCommandEvent &evt)
{
	if (! m_view ) return;

	switch (evt.GetId())
	{
	case wxID_SAVE: Save(); break;
	case wxID_OPEN: 		
		if( Load() ) m_book->SetSelection( PG_SCENE );
		break;
	case ID_LOCATION:
		m_book->SetSelection( PG_LOCATION );
		break;
	case ID_ANALYSIS:
		m_book->SetSelection( PG_ANALYSIS );
		break;
	case ID_CREATE:
	{
#ifdef __WXGTK__
		wxMenu menu;
#else
		wxMetroPopupMenu menu;
#endif
		wxArrayString types = m_view->GetRegisteredTypes();
		for (size_t i = 0; i < types.size(); i++)
			menu.Append( ID_OBJECT_ID + i, types[i] );

#ifdef __WXGTK__
		this->PopupMenu( &menu );
#else
		
		wxPoint pos(wxDefaultPosition);
		if ( wxWindow *win = dynamic_cast<wxWindow*>(evt.GetEventObject()) )
		{
			pos = win->GetScreenPosition();
			pos.y += win->GetClientSize().y;
		}
		menu.Popup( this, pos/*, wxBOTTOM|wxRIGHT*/ );
#endif
		break;
	}

	case ID_VIEW_XYZ: m_book->SetSelection( PG_SCENE ); m_view->SetMode( m_view->SPIN_VIEW ); break;
	case ID_VIEW_XY: m_book->SetSelection( PG_SCENE ); m_view->SetMode( m_view->TOP_VIEW ); break;
	case ID_VIEW_XZ: m_book->SetSelection( PG_SCENE ); m_view->SetMode( m_view->Z_VIEW ); break;
	case ID_FEEDBACK:
		wxLaunchDefaultBrowser( "mailto://sam.support@nrel.gov?subject=Shade Calculator - Beta Feedback" );
		break;
	case wxID_HELP:
#ifdef S3D_STANDALONE
		m_book->SetSelection( PG_HELP );
#else
		SamApp::ShowHelp( "shade_calculator" );
#endif
		break;
	}
}

void ShadeTool::OnCreateObject( wxCommandEvent &evt )
{
	size_t idx = evt.GetId() - ID_OBJECT_ID;
	wxArrayString types = m_view->GetRegisteredTypes();
	if ( idx < types.Count() )
	{
		if ( m_book->GetSelection() != PG_SCENE )
			m_book->SetSelection( PG_SCENE );

		m_view->CreateObject( types[idx] );
		m_view->Render();
		m_view->Refresh();		
	}
}

void ShadeTool::OnDebugCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_AZIMUTH:
	case ID_ALTITUDE:
		{
			double az, al;
			m_txtAzi->GetValue().ToDouble(&az);
			m_txtAlt->GetValue().ToDouble(&al);
			m_view->SetAzAl(az, al);
		}
		break;
	case ID_SCALE:
		{
				double scale;
				m_txtScale->GetValue().ToDouble(&scale);
				m_view->SetScale(scale);
		}
		break;
	case ID_EXPORT_HOURLY:
		wxMessageBox("do analysis and save hourly factors");
		break;
	case ID_EXPORT_DIURNAL:
		wxMessageBox("do analysis and copy to clipboard 12x24 factors table");
		break;
	}
}
