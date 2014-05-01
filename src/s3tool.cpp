
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

static wxString GOOGLE_API_KEY("AIzaSyCyH4nHkZ7FhBK5xYg4db3K7WN-vhpDxas");
static wxString BING_API_KEY("Av0Op8DvYGR2w07w_771JLum7-fdry0kBtu3ZA4uu_9jBJOUZgPY7mdbWhVjiORY");

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

void LocationSetup::DoCurl( const wxString &url )
{
	wxBusyCursor curs;
	m_curl.Start( url );
	while( 1 )
	{
		if ( m_curl.IsStarted() && !m_curl.Finished() )
		{
			wxMilliSleep( 50 );
			//wxYield();
		}
		else break;
	}
}


bool LocationSetup::GeoCode( const wxString &address, double *lat, double *lon, double *tz)
{
	wxBusyCursor _curs;
	bool latlonok = false;

	wxString plusaddr = address;
	plusaddr.Replace("   ", " ");
	plusaddr.Replace("  ", " ");
	plusaddr.Replace(" ", "+");
	
	wxString query = "https://maps.googleapis.com/maps/api/geocode/json?address=" + plusaddr + "&sensor=false&key=" + GOOGLE_API_KEY;
	DoCurl( query );

	wxJSONReader reader;
	wxJSONValue root;
	if (reader.Parse( m_curl.GetDataAsString(), &root )==0)
	{
		wxJSONValue loc = root.Item("results").Item(0).Item("geometry").Item("location");
		if (!loc.IsValid()) return false;
		*lat = loc.Item("lat").AsDouble();
		*lon = loc.Item("lng").AsDouble();
		
		if ( root.Item("status").AsString() != "OK" )
		{
			wxMessageBox("Status error from geocoding web service");
			return false;
		}
	}
	else
	{
		wxMessageBox("Error parsing json output of geocoder");
		return false;
	}

	// get timezone from another service
	query = wxString::Format("https://maps.googleapis.com/maps/api/timezone/json?location=%.14lf,%.14lf&timestamp=1&sensor=false&key=",
		*lat, *lon) + GOOGLE_API_KEY;
	DoCurl( query );
	if (reader.Parse( m_curl.GetDataAsString(), &root )==0)
	{
		wxJSONValue val = root.Item("rawOffset");
		if ( val.IsDouble() ) *tz = val.AsDouble() / 3600.0;
		else *tz = val.AsInt() / 3600.0;

		return root.Item("status").AsString() == "OK";
	}
	else
	{
		wxMessageBox("Error parsing timezone output of service api");
		return false;
	}

}

void LocationSetup::OnAddressChange( wxCommandEvent &e )
{
	m_lat->SetValue( std::numeric_limits<double>::quiet_NaN() );
	m_lon->SetValue( std::numeric_limits<double>::quiet_NaN() );
	m_tz->SetValue( std::numeric_limits<double>::quiet_NaN() );

	wxYield();

	double lat, lon, tz;
	if ( !GeoCode( m_address->GetValue(), &lat, &lon, &tz ) )
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
	double lat = m_lat->Value();
	double lon = m_lon->Value();
	if ( m_zoomLevel > 21 ) m_zoomLevel = 21;
	if ( m_zoomLevel < 1 ) m_zoomLevel = 1;
	wxString zoomStr = wxString::Format("%d", m_zoomLevel );
		
	/*
	wxString url = "https://maps.googleapis.com/maps/api/staticmap?center=" 
		+ wxString::Format("%.9lf,%.9lf", lat, lon) + "&zoom=" + zoomStr 
		+ "&size=800x800&maptype=hybrid&sensor=false&format=jpg-baseline&key=" + GOOGLE_API_KEY;
	*/

	wxString url = "http://dev.virtualearth.net/REST/v1/Imagery/Map/Aerial/"
		+ wxString::Format("%.15lf,%.15lf/%d", lat, lon, m_zoomLevel)
		+ "?mapSize=800,800&format=jpeg&key=" + BING_API_KEY;
		
	DoCurl( url );		
	m_bitmap = wxBitmap( m_curl.GetDataAsImage(wxBITMAP_TYPE_JPEG) );
	if (!m_bitmap.IsOk())
	{
		wxMessageBox("Invalid image data file");
		return;
	}

	m_unannotatedBitmap = m_bitmap;
	
	// Map resolution = 156543.04 meters/pixel * cos(latitude) / (2 ^ zoomlevel)
	// http://msdn.microsoft.com/en-us/library/aa940990.aspx
	m_mpp = 156543.04 * cos(lat*3.15926/180) / pow(2,m_zoomLevel);
	
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
		
	ID_GENERATE_DIURNAL = wxID_HIGHEST+321, ID_GENERATE_HOURLY 
};


BEGIN_EVENT_TABLE( ShadeAnalysis, wxPanel )	
	EVT_BUTTON( ID_GENERATE_DIURNAL, ShadeAnalysis::OnGenerateDiurnal )
	EVT_BUTTON( ID_GENERATE_HOURLY, ShadeAnalysis::OnGenerateHourly )
END_EVENT_TABLE()

ShadeAnalysis::ShadeAnalysis( wxWindow *parent, ShadeTool *st )
	: wxPanel( parent, wxID_ANY ),
	m_shadeTool( st )
{
	SetBackgroundColour( *wxWHITE );

	wxBoxSizer *tools = new wxBoxSizer( wxHORIZONTAL );

	tools->Add( new wxButton(this, ID_GENERATE_DIURNAL, "Diurnal analysis" ), 0, wxALL, 2 );
	tools->Add( new wxButton(this, ID_GENERATE_HOURLY, "Hourly analysis (entire array)" ), 0, wxALL, 2 );

	wxString modes[] = { "Shade fraction", "Beam shading factor" };
	m_sfMode = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, 2, modes );
	m_sfMode->SetSelection( 0 );
	tools->Add( m_sfMode, 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );

	tools->AddStretchSpacer();

	m_scroll = new wxScrolledWindow( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxScrolledWindowStyle|wxBORDER_NONE );
	
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );	
	sizer->Add( tools, 0, wxALL|wxEXPAND, 2 );
	sizer->Add( m_scroll, 1, wxALL|wxEXPAND, 0 );	

	SetSizer( sizer );
	
}

void ShadeAnalysis::OnGenerateHourly( wxCommandEvent & )
{
	wxProgressDialog pdlg( "Shade calculation", "Computing...", 100, m_shadeTool,
		wxPD_SMOOTH|wxPD_CAN_ABORT|wxPD_APP_MODAL|wxPD_AUTO_HIDE );
#ifdef __WXMSW__
	pdlg.SetIcon(  wxICON( appicon) );
#endif

	pdlg.Show();

	double lat, lon, tz;
	m_shadeTool->GetLocationSetup()->GetLocation( &lat, &lon, &tz );

	double azi, zen, alt;
	const int *ndays = ::wxNDay;
	size_t m, d, h, c = 0;

	std::vector<double> shade_fraction;
	shade_fraction.reserve(8760);

	s3d::transform tr;
	tr.set_scale( SF_ANALYSIS_SCALE );

	s3d::scene sc( m_shadeTool->GetView()->GetScene() );	
	std::vector<s3d::shade_result> shresult;

	wxStopWatch sw;
	bool stopped = false;
	for ( m=0;m<12 && !stopped;m++ )
	{
		for ( d=0;d<ndays[m] && !stopped;d++ )
		{
			for ( h=0;h<24 && !stopped;h++ )
			{
				if ( c % 400 == 0 )
				{
					int percent = (int)( 100.0*c/8760.0 );
					if ( !pdlg.Update( percent ) ) 
					{
						stopped = true;
						break;
					}
					else
						wxYieldIfNeeded();
				}

				s3d::sun_pos( 1970, m+1, d+1, h, 30.0, lat, lon, tz, &azi, &zen );				
				alt = 90-zen;

				// for nighttime full shading (fraction=1 and factor=0)
				// consistent with SAM shading factor of zero for night time
				//	double sf = 0;
				double sf = 1; 
				if (alt > 0)
				{
					tr.rotate_azal( azi, alt );
					sc.build( tr );
					sf = sc.shade( shresult );
				}

				shade_fraction.push_back(sf);
				c++;
			}
		}
	}

	wxFileDialog dlg( this, "Hourly Shading File Export", wxEmptyString, "shade.csv", "*.*", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if ( wxID_OK == dlg.ShowModal() )
	{
		if ( FILE *fp = fopen( (const char*)dlg.GetPath().c_str(), "w" ) )
		{
			fprintf( fp, "Shade Fraction,Shading Derate\n");
			for( size_t i=0;i<shade_fraction.size();i++ )
				fprintf( fp, "%.3lf,%.3lf\n", shade_fraction[i], 1.0-shade_fraction[i] );

			fclose(fp);
		}
		else
			wxMessageBox( "Could not write to file:\n\n" + dlg.GetPath() );
	}
	
	/*
	wxFrame *frame = new wxFrame( 0, wxID_ANY, 
		wxString::Format("Shade fractions (computation in %d ms)", sw.Time()), 
		wxDefaultPosition, wxSize(900,700) );

	wxDVPlotCtrl *dview = new wxDVPlotCtrl( frame );
	dview->AddDataSet(new wxDVArrayDataSet("Shade fraction", shade_fraction));
	dview->AddDataSet(new wxDVArrayDataSet("Shade factor", shade_factor));
	dview->SelectDataOnBlankTabs();
	frame->Show(); 
	*/
}


struct surfshade
{
	surfshade()
		: group("Entire Array"), sfac( 12, 24, 1 ), shaded(12,24,0), active(12,24,0)
	{
	}
	
	wxString group;
	matrix_t<float> sfac;
	matrix_t<double> shaded, active;
	std::vector<VActiveSurfaceObject*> surfaces;
	std::vector<int> ids;
};


void ShadeAnalysis::OnGenerateDiurnal( wxCommandEvent & )
{	
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
	shade.push_back( surfshade() ); // overall system shade

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
				shade.push_back( surfshade() );
				index = shade.size()-1;
			}

			surfshade &ss = shade[index];
			ss.surfaces.push_back( surf );
			ss.group = grp;
			if ( std::find( ss.ids.begin(), ss.ids.end(), surf->GetId() ) == ss.ids.end() )
				ss.ids.push_back( surf->GetId() );
		}
	}

	int mode = m_sfMode->GetSelection();

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
					double sf = shresult[k].shade_fraction;

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
			shade[0].sfac( m, h ) = (float)( (mode==0) ? scene_sf : 1.0-scene_sf );

			// compute each group's shading factor from the overall areas
			for( size_t n=1;n<shade.size();n++ )
			{
				double sf = 1;
				if ( shade[n].active(m,h) != 0.0 )
					sf = shade[n].shaded(m,h) / shade[n].active(m,h);

				shade[n].sfac(m,h) =  (float)( (mode==0) ? sf : 1.0-sf );
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
		mxh->Colour1 = ( mode == 0 ) ? *wxWHITE : *wxRED;
		mxh->Colour2 = ( mode == 0 ) ? *wxRED : *wxWHITE;
		mxh->SetTitle( shade[i].group );
		mxh->SetData( shade[i].sfac );
		mxh->SetLegend( mode==0 ? "(shade fraction) 0=no shade, 1=fully shaded" : "(derate)  0=fully shaded, 1=no shade" );
		y += 360;
	}

	while ( m_mxhList.size() > shade.size() )
	{
		m_mxhList[ m_mxhList.size()-1 ]->Destroy();
		m_mxhList.erase( m_mxhList.end()-1 );
	}

	m_scroll->SetScrollbars( 1, 1, 1100, y );
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
	sizer_tool->Add( new wxMetroButton( this, wxID_OPEN, "Open" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, wxID_SAVE, "Save" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_LOCATION, "Location" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_CREATE, "Create", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_DOWNARROW ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_VIEW_XYZ, "3D scene" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_VIEW_XY, "Bird's eye" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_VIEW_XZ, "Elevations" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, ID_ANALYSIS, "Analyze" ), 0, wxALL|wxEXPAND, 0 );
	
	sizer_tool->AddStretchSpacer();
	sizer_tool->Add( new wxMetroButton( this, ID_FEEDBACK, "Feedback" ), 0, wxALL|wxEXPAND, 0 );
	sizer_tool->Add( new wxMetroButton( this, wxID_HELP, "Help" ), 0, wxALL|wxEXPAND, 0 );
	
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

	wxString help_index( "file:///" + data_path + "/help/index.html" );
	//wxShowTextMessageDialog( help_index );
	m_helpViewer = wxWebView::New( m_book, wxID_ANY, help_index,
		wxDefaultPosition, wxDefaultSize, wxWebViewBackendDefault, wxBORDER_NONE );

	wxBoxSizer *sizer_main = new wxBoxSizer( wxVERTICAL );
	sizer_main->Add( sizer_tool, 0, wxALL|wxEXPAND, 0 );
	sizer_main->Add( m_book, 1, wxALL|wxEXPAND, 0 );
	sizer_main->Add( m_debugPanel, 0, wxALL|wxEXPAND, 0 );
	SetSizer( sizer_main );
	
	m_book->AddPage( m_location, "Location" );
	m_book->AddPage( m_split, "Scene Editor" );
	m_book->AddPage( m_analysis, "Analysis page" );
	m_book->AddPage( m_helpViewer, "Help" );
	//m_book->SetSelection( PG_SCENE );
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
		m_book->SetSelection( PG_SCENE );
		wxMetroPopupMenu menu;
		wxArrayString types = m_view->GetRegisteredTypes();
		for (size_t i = 0; i < types.size(); i++)
			menu.Append( ID_OBJECT_ID + i, types[i] );
		
		wxPoint pos(wxDefaultPosition);
		if ( wxWindow *win = dynamic_cast<wxWindow*>(evt.GetEventObject()) )
		{
			pos = win->GetScreenPosition();
			pos.y += win->GetClientSize().y;
		}
		menu.Popup( this, pos/*, wxBOTTOM|wxRIGHT*/ );
		break;
	}

	case ID_VIEW_XYZ: m_book->SetSelection( PG_SCENE ); m_view->SetMode( m_view->SPIN_VIEW ); break;
	case ID_VIEW_XY: m_book->SetSelection( PG_SCENE ); m_view->SetMode( m_view->TOP_VIEW ); break;
	case ID_VIEW_XZ: m_book->SetSelection( PG_SCENE ); m_view->SetMode( m_view->Z_VIEW ); break;
	case ID_FEEDBACK:
		wxLaunchDefaultBrowser( "mailto://sam.support@nrel.gov?subject=Shade Calculator - Beta Feedback" );
		break;
	case wxID_HELP:
		m_book->SetSelection( PG_HELP );
		break;
	}
}

void ShadeTool::OnCreateObject( wxCommandEvent &evt )
{
	size_t idx = evt.GetId() - ID_OBJECT_ID;
	wxArrayString types = m_view->GetRegisteredTypes();
	if ( idx < types.Count() )
	{
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
