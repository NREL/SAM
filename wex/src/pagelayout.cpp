
#include <wx/valtext.h>
#include <wx/dcbuffer.h>
#include <wx/grid.h>
#include <wx/imaglist.h>
#include <wx/gdicmn.h>
#include <wx/paper.h>
#include <wx/tokenzr.h>
#include <wx/datstrm.h>
#include <wx/busyinfo.h>
#include <wx/wfstream.h>

#include "wex/utils.h"
#include "wex/pdf/pdfdocument.h"

#include "wex/pagelayout.h"

#include <algorithm>

wxScreenOutputDevice::wxScreenOutputDevice( wxPageScaleInterface *lc, wxDC &dc ) : m_lc(lc), m_dc(dc)
{
	Color( *wxBLUE );
	Font( SERIF, 12, false, false );
}
	
void wxScreenOutputDevice::Clip( float x, float y, float width, float height )
{
	wxPoint tl;
	m_lc->PageToScreen( x, y, &tl.x, &tl.y );
	m_dc.SetClippingRegion( tl.x, tl.y, width*m_lc->GetPPI(), height*m_lc->GetPPI() );
}

void wxScreenOutputDevice::Unclip()
{
	m_dc.DestroyClippingRegion();
}

void wxScreenOutputDevice::Color( const wxColour &c )
{
	m_pen.SetColour( c );
	m_brush.SetColour( c );
	m_dc.SetPen( m_pen );
	m_dc.SetBrush( m_brush );
	m_dc.SetTextForeground( c );

}

void wxScreenOutputDevice::LineStyle( float thick, int sty )
{
	int pixels = thick*m_lc->GetPPI();
	if (pixels < 1) pixels = 1;
	m_pen.SetStyle( sty==DOTTED ? wxDOT : wxSOLID );
	m_pen.SetWidth( pixels );
	m_dc.SetPen( m_pen );
}

void wxScreenOutputDevice::Line( float x1, float y1, float x2, float y2 )
{
	wxPoint start, end;
	m_lc->PageToScreen( x1, y1, &start.x, &start.y );
	m_lc->PageToScreen( x2, y2, &end.x, &end.y );
	m_dc.DrawLine( start, end );
}

void wxScreenOutputDevice::Rect( float x, float y, float width, float height, bool fill, float radius )
{
	wxPoint topleft;
	m_lc->PageToScreen( x, y, &topleft.x, &topleft.y );
	m_brush.SetStyle( fill ? wxSOLID : wxTRANSPARENT );
	m_pen.SetWidth(1);
	m_dc.SetPen( m_pen );
	m_dc.SetBrush( m_brush );
	if (radius > 0)
	{
		m_dc.DrawRoundedRectangle( topleft.x, topleft.y,
			width * m_lc->GetPPI(),
			height * m_lc->GetPPI(),
			radius * m_lc->GetPPI() );
	}
	else
		m_dc.DrawRectangle( topleft.x, topleft.y, width * m_lc->GetPPI(), height * m_lc->GetPPI() );
}

void wxScreenOutputDevice::Circle( float x, float y, float radius, bool fill )
{
	wxPoint center;
	m_lc->PageToScreen( x, y, &center.x, &center.y );

	m_pen.SetWidth(1);
	m_brush.SetStyle( fill ? wxSOLID : wxTRANSPARENT );
	m_dc.SetPen( m_pen );
	m_dc.SetBrush( m_brush );

	m_dc.DrawCircle( center.x, center.y, radius * m_lc->GetPPI() );
}

void wxScreenOutputDevice::Arc( float x, float y, float width, float height, float angle1, float angle2, bool fill )
{
	wxPoint topleft;
	m_lc->PageToScreen( x, y, &topleft.x, &topleft.y );

	m_pen.SetWidth(1);
	m_brush.SetStyle( fill ? wxSOLID : wxTRANSPARENT );
	m_dc.SetPen( m_pen );
	m_dc.SetBrush( m_brush );

	wxPoint size( width*m_lc->GetPPI(), height*m_lc->GetPPI() );
	m_dc.DrawEllipticArc( topleft.x, topleft.y,
		size.x, size.y, 
		angle1, angle2 );
}

void wxScreenOutputDevice::Font( int face, int points, bool bold, bool italic )
{
	switch( face )
	{
	case wxPageOutputDevice::FIXED: 
		m_font.SetFamily( wxFONTFAMILY_MODERN ); 
		if (!m_font.SetFaceName("Courier New") )
			if (!m_font.SetFaceName("Courier"))
				m_font.SetFaceName("Consolas");
		break;
	case wxPageOutputDevice::SERIF:
		m_font.SetFamily( wxFONTFAMILY_ROMAN );
		if (!m_font.SetFaceName("Times New Roman"))
			if (!m_font.SetFaceName("Times"))
				m_font.SetFaceName("Century Schoolbook");
		break;
	case wxPageOutputDevice::SANSERIF:
		m_font.SetFamily( wxFONTFAMILY_SWISS );
		if (!m_font.SetFaceName("Helvetica"))
			if (!m_font.SetFaceName("Arial"))
				m_font.SetFaceName("Verdana");
		break;
	default: m_font = *wxNORMAL_FONT; break;
	}

	m_font.SetWeight( bold ? wxFONTWEIGHT_BOLD : wxFONTWEIGHT_NORMAL );
	m_font.SetStyle( italic ? wxFONTSTYLE_ITALIC : wxFONTSTYLE_NORMAL );
	if (points < 1) points = 1;
	m_font.SetPointSize( points );
	m_dc.SetFont( m_font );
}

void wxScreenOutputDevice::Text( float x, float y, const wxString &text, float angle )
{
	wxPoint topleft;
	m_lc->PageToScreen( x, y, &topleft.x, &topleft.y );
	wxFont f = m_font;
	f.SetPointSize( ScalePointSizeToScreen( m_font.GetPointSize() ) );
	m_dc.SetFont( f );
	if ( angle == 0.0f )
		m_dc.DrawText( text, topleft.x, topleft.y );
	else
		m_dc.DrawRotatedText( text, topleft.x, topleft.y, angle );
}

void wxScreenOutputDevice::Measure( const wxString &text, float *width, float *height )
{
	m_dc.SetFont( m_font );
	wxSize size( m_dc.GetTextExtent( text ) );
	float devppi = DevicePPI();
	if (width) *width = size.x/devppi;
	if (height) *height = m_dc.GetCharHeight()/devppi;
}

void wxScreenOutputDevice::Image( const wxImage &img, float top, float left, float width, float height )
{
	wxPoint topleft;
	m_lc->PageToScreen( top, left, &topleft.x, &topleft.y );

	float img_width_inches = width < 1 ? img.GetWidth() / 72.0f : width;
	float img_height_inches = height < 1 ? img.GetHeight() / 72.0f : height;
				
	float scale = m_lc->GetPPI() / DevicePPI();

	int pix_width = (int)( scale * img_width_inches * DevicePPI() );
	int pix_height = (int)( scale * img_height_inches * DevicePPI() );

	m_dc.DrawBitmap( img.Scale( pix_width, pix_height, ::wxIMAGE_QUALITY_NORMAL ),
		topleft.x, topleft.y );
}

float wxScreenOutputDevice::DevicePPI()
{
	wxSize sz = m_dc.GetPPI();
	if (sz.x != sz.y) return (float) (sz.x>sz.y)?sz.x:sz.y;
	else return (float)sz.x;
}

int wxScreenOutputDevice::ScalePointSizeToScreen( int requested )
{
	int points = (int)( ((float)requested) * m_lc->GetPPI() / DevicePPI());
	if (points < 1) points = 1;
	return points;
}


wxPdfOutputDevice::wxPdfOutputDevice( wxPdfDocument &pdf )
	: m_pdf(pdf) { }

void wxPdfOutputDevice::Clip( float x, float y, float width, float height )
{
	m_pdf.ClippingRect( x, y, width, height );
}

void wxPdfOutputDevice::Unclip()
{
	m_pdf.UnsetClipping();
}

void wxPdfOutputDevice::Color( const wxColour &c )
{
	m_pdf.SetDrawColour( c );
	m_pdf.SetFillColour( c );
	m_pdf.SetTextColour( c );
}

void wxPdfOutputDevice::LineStyle( float thick, int style )
{
	wxPdfArrayDouble dash;
	if (style == DOTTED ) dash.Add( 0.05 );
	m_pdf.SetLineStyle( wxPdfLineStyle( thick, wxPDF_LINECAP_NONE, wxPDF_LINEJOIN_NONE, dash, -1, m_pdf.GetDrawColour() ) );
}

void wxPdfOutputDevice::Line( float x1, float y1, float x2, float y2 )
{
	m_pdf.Line( x1, y1, x2, y2 );
}

void wxPdfOutputDevice::Rect( float x, float y, float width, float height, bool fill, float radius  ) 
{
	int style = fill ? wxPDF_STYLE_FILLDRAW : wxPDF_STYLE_DRAW;
	if (radius > 0.0f)
		m_pdf.RoundedRect( x, y, width, height, radius, wxPDF_CORNER_ALL, style );
	else
		m_pdf.Rect( x, y, width, height,  style );
}

void wxPdfOutputDevice::Circle( float x, float y, float radius, bool fill )
{
	m_pdf.Circle( x, y, radius, 0, 360, fill ? wxPDF_STYLE_FILLDRAW : wxPDF_STYLE_DRAW );
}

void wxPdfOutputDevice::Arc( float x, float y, float width, float height, float angle1, float angle2, bool fill )
{
	m_pdf.Ellipse( x+width/2, y+height/2, width/2, height/2, 0, angle1, angle2, fill ? wxPDF_STYLE_FILLDRAW : wxPDF_STYLE_DRAW );
}

void wxPdfOutputDevice::Font( int face, int points, bool bold, bool italic )
{
	wxString name = "Courier";
	if ( face == SERIF ) name = "Times";
	if ( face == SANSERIF ) name = "Helvetica";

	int style = 0;
	if (bold) style |= wxPDF_FONTSTYLE_BOLD;
	if (italic) style |= wxPDF_FONTSTYLE_ITALIC;

	m_pdf.SetFont( name, style, points );
}

void wxPdfOutputDevice::Text( float x, float y, const wxString &text, float angle )
{
	if (angle == 0.0f)
	{
		 // the .95 factor pushes the text up a little off baseline, looks better
		m_pdf.Text( x, y + m_pdf.GetFontSize()/72.0f*0.95f, text );
	}
	else
	{
		double fh = m_pdf.GetFontSize()/72.0f;
		double xx = x + fh*sin( angle*M_PI/180 );
		double yy = y + fh*cos( angle*M_PI/180 );
		m_pdf.RotatedText( xx, yy, text, angle );
	}
}

void wxPdfOutputDevice::Measure( const wxString &text, float *width, float *height )
{
	if (width) *width = (float) m_pdf.GetStringWidth( text );

	// note: 1.15 factor is kludge to make text height appear more accurately on win32
	if (height) *height = (float)( m_pdf.GetFontSize() / 72.0f )*1.15f;
}

void wxPdfOutputDevice::Image( const wxImage &img, float top, float left, float width, float height )
{
	m_pdf.Image( wxString::Format("img_%d", ++m_imageIndex), img, top, left, width, height );
}

int wxPdfOutputDevice::m_imageIndex = 0;


void wxPagePdfRenderer::AddPage( wxPageLayout *page,
	const wxString &header,
	const wxString &footer)
{
	page_data x;
	x.page = page;
	x.header = header;
	x.footer = footer;
	m_pageList.push_back( x );
}

bool wxPagePdfRenderer::Render( const wxString &pdf_file )
{
	if ( m_pageList.size() == 0 ) return false;

	wxPdfDocument pdf( m_pageList[0].page->GetOrientation(),
		wxT("in"),
		m_pageList[0].page->GetPaperType() );

	wxPdfOutputDevice dv( pdf );

	for (size_t i=0;i<m_pageList.size();i++)
	{
		wxPageLayout *pl = m_pageList[i].page;

		pdf.AddPage( pl->GetOrientation(), pl->GetPaperType() );		

		float width, height, top, bottom, left, right;
		pl->GetDimensions( &width, &height );
		pl->GetMargins( &top, &bottom, &left, &right );

		wxString h = m_pageList[i].header;
		if ( !h.IsEmpty() )
		{
			h.Replace("@PAGENUM@", wxString::Format("%d", i+1));
			h.Replace("@PAGECOUNT@", wxString::Format("%d", m_pageList.size()));
			h.Replace("@DATETIME@", wxNow());

			dv.Color( *wxLIGHT_GREY );
			dv.Font( wxPageOutputDevice::SANSERIF, 9, false, false );
			dv.Text( left, top - 9.0f/72.0f, h, 0.0f );
		}

		wxString f = m_pageList[i].footer;
		if ( !f.IsEmpty() )
		{		
			f.Replace("@PAGENUM@", wxString::Format("%d", i+1));
			f.Replace("@PAGECOUNT@", wxString::Format("%d", m_pageList.size()));
			f.Replace("@DATETIME@", wxNow());
			
			dv.Color( *wxLIGHT_GREY );
			dv.Font( wxPageOutputDevice::SANSERIF, 9, false, false );
			dv.Text( left, height - bottom , f, 0.0f );
		}

		pl->Render( dv );
	}

	const wxMemoryOutputStream &data = pdf.CloseAndGetBuffer();

	wxFileOutputStream fp( pdf_file );
	if (!fp.IsOk()) return false;

	wxMemoryInputStream tmpis( data );
	fp.Write( tmpis );
	return fp.Close();
}


bool wxPageObject::Inside( float x, float y )
{
	return ( x >= m_x && x <= m_x+m_width
		&& y >= m_y && y <= m_y+m_height );
}

void wxPageObject::SetGeometry( float x, float y, float width, float height )
{
	if (x >= 0) m_x = x;
	if (y >= 0) m_y = y;
	if (width >= 0) m_width = width;
	if (height >= 0) m_height = height;
}

void wxPageObject::GetGeometry( float *x, float *y, float *width, float *height )
{
	if (x) *x = m_x;
	if (y) *y = m_y;
	if (width) *width = m_width;
	if (height) *height = m_height;
}



wxPageLayout::wxPageLayout( int orient, wxPaperSize paper )
{
	m_paperXDim = m_paperYDim = 0;
	m_viewCtrl = 0;
	layout_cache.x = layout_cache.y = layout_cache.width = layout_cache.height = 0;

	m_marginTop = m_marginBottom = m_marginLeft = m_marginRight = 0.5; // standard margin of .5 inch

	m_orientation = orient;
	SetPaperType( paper );
}

wxPageLayout::~wxPageLayout()
{
	if ( m_viewCtrl != 0 )
		m_viewCtrl->SetPage( 0 );

	for (size_t i=0;i<m_objectList.size();i++)
		delete m_objectList[i];

	m_objectList.clear();
}

wxPageLayout *wxPageLayout::Duplicate()
{
	wxPageLayout *copy = new wxPageLayout;
	copy->m_orientation = m_orientation;
	copy->m_paperType = m_paperType;
	copy->m_marginTop = m_marginTop;
	copy->m_marginBottom = m_marginBottom;
	copy->m_marginLeft = m_marginLeft;
	copy->m_marginRight = m_marginRight;

	copy->m_paperXDim = m_paperXDim;
	copy->m_paperYDim = m_paperYDim;
	copy->m_viewCtrl = 0;

	for (size_t i=0;i<m_objectList.size();i++)
		copy->m_objectList.push_back( m_objectList[i]->Duplicate() );

	return copy;
}

void wxPageLayout::Add( wxPageObject *obj )
{
	if ( std::find( m_objectList.begin(), 
		m_objectList.end(), obj ) == m_objectList.end() )
	{
		m_objectList.push_back(obj);
		if (m_viewCtrl != 0)
			m_viewCtrl->Invalidate();
	}
}
void wxPageLayout::Delete( wxPageObject *obj )
{
	std::vector<wxPageObject*>::iterator it = std::find(m_objectList.begin(),
		m_objectList.end(), obj);
	if ( it != m_objectList.end())
	{
		m_objectList.erase(it);
		delete obj;
		if (m_viewCtrl != 0)
			m_viewCtrl->Invalidate();
	}

}

void wxPageLayout::Raise( wxPageObject *obj )
{
	std::vector<wxPageObject*>::iterator it = std::find(m_objectList.begin(),
		m_objectList.end(), obj);
	if ( it != m_objectList.end() )
	{
		m_objectList.erase( it );
		m_objectList.insert(m_objectList.begin(), obj);
	}
}

void wxPageLayout::Clear()
{
	for (size_t i=0;i<m_objectList.size();i++)
		delete m_objectList[i];

	m_objectList.clear();
	if (m_viewCtrl != 0)
		m_viewCtrl->Invalidate();
}

wxPageObject **wxPageLayout::GetObjects( int *count )
{
	if (count != 0) *count = m_objectList.size();
	return m_objectList.data();
}
	
wxPageObject *wxPageLayout::Under( float x, float y )
{
	for (size_t i=0;i<m_objectList.size();i++)
		if ( m_objectList[i]->Inside(x,y) )
			return m_objectList[i];
	
	return 0;
}

void wxPageLayout::SetOrientation( int orient )
{
	m_orientation = orient;
	if ( m_viewCtrl != 0 )
		m_viewCtrl->Invalidate();
}

void wxPageLayout::SetPaperType( wxPaperSize paper )
{
	m_paperType = paper;

	wxPrintPaperType *p = wxThePrintPaperDatabase->FindPaperType( paper );
	if (!p)
	{
		m_paperType = wxPAPER_LETTER;
		p = wxThePrintPaperDatabase->FindPaperType( wxPAPER_LETTER );
	}

	if (p)
	{
		wxSize sz = p->GetSizeDeviceUnits();
		m_paperXDim = sz.x / 72.0;
		m_paperYDim = sz.y / 72.0;
	}
	else
	{
		m_paperXDim = 8.5;
		m_paperYDim = 11;
	}
	
	if ( m_viewCtrl != 0 )
		m_viewCtrl->Invalidate();
}

void wxPageLayout::SetMargins( float top, float bottom, float left, float right )
{
	if (top < 0.3f) top = 0.3f;
	if (bottom < 0.3f) bottom = 0.3f;
	if (left < 0.3f) left = 0.3f;
	if (right < 0.3f) right = 0.3f;
	
	if (top > 2) top = 2;
	if (bottom > 2) bottom = 2;
	if (left > 2) left = 2;
	if (right > 2) right = 2;

	m_marginTop = top;
	m_marginBottom = bottom;
	m_marginLeft = left;
	m_marginRight = right;
}

void wxPageLayout::GetMargins( float *top, float *bottom, float *left, float *right )
{
	if (top) *top = m_marginTop;
	if (bottom) *bottom = m_marginBottom;
	if (left) *left = m_marginLeft;
	if (right) *right = m_marginRight;
}


void wxPageLayout::GetDimensions( float *horiz, float *vert )
{
	if ( m_orientation  == wxLANDSCAPE )
	{
		*horiz = m_paperYDim;
		*vert = m_paperXDim;
	}
	else
	{
		*horiz = m_paperXDim;
		*vert = m_paperYDim;
	}
}

bool wxPageLayout::Read( wxInputStream &is )
{
	wxDataInputStream in(is);

	unsigned short page_code = in.Read16();
	unsigned char page_ver = in.Read8();
	if (page_ver > 0)
	{
		m_orientation = in.Read8();
		SetPaperType( (wxPaperSize) in.Read32() );
		m_marginTop = (float)in.ReadDouble();
		m_marginBottom = (float)in.ReadDouble();
		m_marginLeft = (float)in.ReadDouble();
		m_marginRight = (float)in.ReadDouble();
		unsigned int count = in.Read32();

		for (unsigned int i=0;i<count;i++)
		{
			unsigned short obj_code = in.Read16();
			wxString type = in.ReadString();
			float x = (float)in.ReadDouble();
			float y = (float)in.ReadDouble();
			float width = (float)in.ReadDouble();
			float height = (float)in.ReadDouble();
			wxPageObject *obj = wxPageObjectTypes::Create( type );
			if ( obj )
			{
				obj->SetGeometry( x, y, width, height );
				obj->ReadData( is );
				Add(obj);
			}

			if ( in.Read16() != obj_code ) return false;

		}
	}

	return in.Read16() == page_code;
}

bool wxPageLayout::Write( wxOutputStream &os )
{
	if ( !os.IsOk() ) return false;

	wxDataOutputStream out(os);
	out.Write16( 0xfa ); // code
	out.Write8( 1 ); // version

	out.Write8( m_orientation );
	out.Write32( (unsigned int) m_paperType );
	out.WriteDouble( m_marginTop );
	out.WriteDouble( m_marginBottom );
	out.WriteDouble( m_marginLeft );
	out.WriteDouble( m_marginRight );
	out.Write32( (unsigned int) m_objectList.size() );


	for (size_t i=0;i<m_objectList.size();i++)
	{
		wxPageObject *obj = m_objectList[i];
		out.Write16( 0xfb ); // code
		
		out.WriteString( obj->TypeName() );
		out.WriteDouble( obj->m_x );
		out.WriteDouble( obj->m_y );
		out.WriteDouble( obj->m_width );
		out.WriteDouble( obj->m_height );

		obj->WriteData( os );

		out.Write16( 0xfb ); // code
	}
	
	out.Write16( 0xfa ); // code

	return true;
}


void wxPageLayout::Render( wxPageOutputDevice &dv )
{
	dv.Color( *wxBLACK );
	dv.LineStyle( 0.013f, wxPageOutputDevice::SOLID );
	dv.Font( wxPageOutputDevice::SANSERIF, 12, false, false );

	for (size_t i=0;i<m_objectList.size();i++)
	{
		float x, y, width, height;
		m_objectList[i]->GetGeometry( &x, &y, &width, &height );
		dv.Clip( x, y, width, height );
		m_objectList[i]->Render( dv );
		dv.Unclip();
	}
}

class ObjectTypeArray : public std::vector<wxPageObject*>
{
public:
	virtual ~ObjectTypeArray()
	{
		for (size_t i=0;i<size();i++)
			delete (*this)[i];
		clear();
	}
};

static ObjectTypeArray g_objectTypes;

void wxPageObjectTypes::Register( wxPageObject *info )
{
	for (size_t i=0;i<g_objectTypes.size();i++)
		if (g_objectTypes[i]->TypeName() == info->TypeName())
			return;

	g_objectTypes.push_back( info );
}

wxPageObject *wxPageObjectTypes::Create( const wxString &type )
{
	for (size_t i=0;i<g_objectTypes.size();i++)
		if ( type.CmpNoCase( g_objectTypes[i]->TypeName() ) == 0)
			return g_objectTypes[i]->Duplicate();

	return 0;
}

wxArrayString wxPageObjectTypes::AllTypes()
{
	wxArrayString list;
	for (size_t i=0;i<g_objectTypes.size();i++)
		list.Add( g_objectTypes[i]->TypeName() );
	return list;
}

wxString wxPageObjectTypes::DescriptionOf( const wxString &type )
{
	for (size_t i=0;i<g_objectTypes.size();i++)
		if (type.CmpNoCase( g_objectTypes[i]->TypeName() ) == 0)
			return g_objectTypes[i]->Description();

	return wxEmptyString;
}

DEFINE_EVENT_TYPE( wxEVT_PAGELAYOUT_SELECT )
DEFINE_EVENT_TYPE( wxEVT_PAGELAYOUT_MODIFY )
DEFINE_EVENT_TYPE( wxEVT_PAGELAYOUT_CREATE )

wxPageLayoutEvent::wxPageLayoutEvent( wxEventType cmdType, wxPageLayoutCtrl *sender, wxPageObject *obj)
	: wxCommandEvent( cmdType, sender->GetId() )
{
	SetEventObject( sender );
	m_pageLayout = sender->GetPage();
	m_pageObject = obj;	
}

enum { ID_popup_first = 1598,
		ID_CREATE, ID_CREATE_LAST=ID_CREATE+50,
		ID_ZOOM50, ID_ZOOM75, ID_ZOOM100, ID_ZOOM125, ID_ZOOM150, ID_FITHORIZ, ID_FITVERT, ID_FITBOTH,
		ID_ALIGNTOP, ID_ALIGNLEFT, ID_ALIGNRIGHT, ID_ALIGNBOTTOM,
		ID_DUPLICATE, ID_DELETE,
		ID_SAVEFILE, ID_LOADFILE, ID_EXPORTPDF,
		ID_EDITMARGINS,
		ID_SETGRIDSPACING, ID_SHOWGRID, ID_SHOWOUTLINES,
		ID_SNAP, ID_SETSNAPSPACING,
		ID_LANDSCAPE, ID_PORTRAIT,
		ID_PAPER_FIRST, ID_PAPER_LAST = ID_PAPER_FIRST+20,
	ID_popup_last };

BEGIN_EVENT_TABLE(wxPageLayoutCtrl, wxScrolledWindow)

	EVT_MENU_RANGE( ID_popup_first, ID_popup_last, wxPageLayoutCtrl::OnPopup )

	EVT_SIZE(wxPageLayoutCtrl::OnResize)
	EVT_ERASE_BACKGROUND(wxPageLayoutCtrl::OnErase)
	EVT_PAINT(wxPageLayoutCtrl::OnPaint)
	EVT_LEFT_DOWN( wxPageLayoutCtrl::OnLeftDown )
	EVT_LEFT_DCLICK( wxPageLayoutCtrl::OnLeftDouble )
	EVT_LEFT_UP( wxPageLayoutCtrl::OnLeftUp )
	EVT_RIGHT_DOWN( wxPageLayoutCtrl::OnRightDown )
	EVT_MOTION( wxPageLayoutCtrl::OnMouseMove )
	EVT_LEAVE_WINDOW( wxPageLayoutCtrl::OnLeave )
	EVT_MOUSEWHEEL( wxPageLayoutCtrl::OnMouseWheel )

END_EVENT_TABLE()

#define RSZBOXW 6
#define BOX_NONE 0
#define BOX_TOPLEFT 1
#define BOX_TOPRIGHT 2
#define BOX_BOTTOMLEFT 3
#define BOX_BOTTOMRIGHT 4
#define BOX_TOP 5
#define BOX_LEFT 6
#define BOX_RIGHT 7
#define BOX_BOTTOM 8

wxPageLayoutCtrl::wxPageLayoutCtrl( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size)
	: wxScrolledWindow( parent,  id, pos, size )
{	

	SetBackgroundStyle( wxBG_STYLE_CUSTOM );

	wxSize mm = wxGetDisplaySizeMM();
	wxSize sz = wxGetDisplaySize();

	float ppix = sz.x / (mm.x/25.4) ;
	float ppiy = sz.y / (mm.y/25.4);

	if ( ppix == ppiy ) m_screenPPI = ppix;
	else m_screenPPI = (ppix<ppiy)?ppix:ppiy;

	m_popupMenu = 0;
	m_page = 0;
	m_ppi = 72;
	m_xMargin = 10;
	m_yMargin = 10;
	m_gridSpacing = 1.0;
	m_showGrid = false;
	m_showOutlines = false;
	m_snapCoordinates = true;
	m_snapSpacing = 0.125f;

	m_moveMode = m_moveModeErase = false;
	m_multiSelMode = m_multiSelModeErase = false;
	m_origX = m_origY = m_diffX = m_diffW = m_diffH = 0;
	m_resizeMode = m_resizeModeErase = false;
	m_resizeBox = BOX_NONE;

	m_selectColour = *wxBLUE;
	
	m_standardCursor = wxCursor( wxCURSOR_ARROW ); 
	m_moveResizeCursor = wxCursor( wxCURSOR_SIZING );
	m_nwseCursor = wxCursor( wxCURSOR_SIZENWSE );
	m_nsCursor = wxCursor( wxCURSOR_SIZENS );
	m_weCursor = wxCursor( wxCURSOR_SIZEWE );
	m_neswCursor = wxCursor( wxCURSOR_SIZENESW );

	m_popupX = m_popupY = 0;

	SetBackgroundColour( wxColour( 100, 100, 100 ) );
	SetCursor( m_standardCursor );

	CreatePopupMenu();
}

wxPageLayoutCtrl::~wxPageLayoutCtrl()
{
	m_page = 0;
	delete m_popupMenu;
}

void wxPageLayoutCtrl::CreatePopupMenu()
{
	if (m_popupMenu != 0) delete m_popupMenu;

	m_popupMenu = new wxMenu;

	wxArrayString types = wxPageObjectTypes::AllTypes();
	for (size_t i=0;i<(int)types.size();i++)
		m_popupMenu->Append( ID_CREATE+i, "Create " + wxPageObjectTypes::DescriptionOf( types[i] ) );

	if (types.size() > 1)
		m_popupMenu->AppendSeparator();


	m_popupMenu->Append( ID_DUPLICATE, "Duplicate");
	m_popupMenu->Append( ID_DELETE, "Delete");
	m_popupMenu->AppendSeparator();

	m_popupMenu->Append( ID_EXPORTPDF, "Export page as PDF..." );
	m_popupMenu->Append( ID_SAVEFILE, "Save page data file..." );
	m_popupMenu->Append( ID_LOADFILE, "Load page data file..." );
	m_popupMenu->AppendSeparator();

	m_popupMenu->Append( ID_ZOOM50, "Zoom 50%" );
	m_popupMenu->Append( ID_ZOOM75, "Zoom 75%" );
	m_popupMenu->Append( ID_ZOOM100, "Zoom 100%" );
	m_popupMenu->Append( ID_ZOOM125, "Zoom 125%" );
	m_popupMenu->Append( ID_ZOOM150, "Zoom 150%" );
	m_popupMenu->Append( ID_FITHORIZ, "Fit horizontally" );
	m_popupMenu->Append( ID_FITVERT, "Fit vertically" );
	m_popupMenu->Append( ID_FITBOTH, "Fit both dimensions" );
	m_popupMenu->AppendSeparator();
	
	m_popupMenu->Append( ID_ALIGNTOP, "Align top edges");
	m_popupMenu->Append( ID_ALIGNLEFT, "Align left edges");
	m_popupMenu->Append( ID_ALIGNRIGHT, "Align right edges");
	m_popupMenu->Append( ID_ALIGNBOTTOM, "Align bottom edges");
	m_popupMenu->AppendSeparator();

	//m_popupMenu->Append( ID_EDITMARGINS, "Edit margins...");
	//m_popupMenu->AppendSeparator();

	m_popupMenu->AppendCheckItem( ID_SHOWOUTLINES, "Show outlines");
	m_popupMenu->AppendCheckItem( ID_SHOWGRID, "Show grid" );
	m_popupMenu->Append( ID_SETGRIDSPACING, "Set grid spacing...");
	m_popupMenu->Append( ID_SETSNAPSPACING, "Set snap spacing...");
	m_popupMenu->AppendCheckItem( ID_SNAP, "Snap coordinates" );
	m_popupMenu->AppendSeparator();
	
	m_popupMenu->AppendRadioItem( ID_PORTRAIT, "Portrait");
	m_popupMenu->AppendRadioItem( ID_LANDSCAPE, "Landscape");
	m_popupMenu->AppendSeparator();

	m_paperMenuInfo.push_back( paper_menu_item("Letter", wxPAPER_LETTER) );
	m_paperMenuInfo.push_back( paper_menu_item("Legal", wxPAPER_LEGAL) );
	m_paperMenuInfo.push_back( paper_menu_item("A3", wxPAPER_A3) );
	m_paperMenuInfo.push_back( paper_menu_item("A4", wxPAPER_A4) );
	m_paperMenuInfo.push_back( paper_menu_item("A5", wxPAPER_A5) );
	
	for (size_t i=0;i<m_paperMenuInfo.size();i++)
	{
		m_paperMenuInfo[i].menuId = ID_PAPER_FIRST + i;
		m_popupMenu->AppendRadioItem( 
			m_paperMenuInfo[i].menuId, m_paperMenuInfo[i].caption );
	}
}

void wxPageLayoutCtrl::SetPage( wxPageLayout *page )
{
	if ( m_page == page ) return;

	// first detach current page
	if (m_page)
	{
		m_page->m_viewCtrl = 0;
		m_page = 0;
	}

	m_page = page;

	if (m_page)
		m_page->m_viewCtrl = this;

	Invalidate();
}

wxPageLayout *wxPageLayoutCtrl::GetPage()
{
	return m_page;
}

void wxPageLayoutCtrl::SetPPI( float ppi )
{
	m_ppi = ppi;
	if (m_ppi < 20) m_ppi = 20;
	if (m_ppi > 200) m_ppi = 200;
	Invalidate();
}

wxPageObject **wxPageLayoutCtrl::GetSelections(int *count)
{
	if (count) *count = m_selectedItems.size();
	return m_selectedItems.data();
}

void wxPageLayoutCtrl::Invalidate()
{
	int hpos, vpos;
	GetViewStart( &hpos, &vpos );

	float horiz = 0, vert = 0;
	if (m_page)
	{
		m_page->GetDimensions(&horiz, &vert);
		m_page->layout_cache.x = m_xMargin;
		m_page->layout_cache.y = m_yMargin;
		m_page->layout_cache.width = horiz*m_ppi;
		m_page->layout_cache.height = vert*m_ppi;
	}

	SetScrollbars(1, 1, m_xMargin*2 + horiz*m_ppi, m_yMargin*2 + vert*m_ppi, hpos, vpos);
	Refresh();
}

void wxPageLayoutCtrl::Invalidate( wxPageObject * )
{
	// only force redraw of the selected object (todo: later)
	Invalidate();
}

void wxPageLayoutCtrl::PageToScreen( float x, float y, int *px, int *py )
{
	*px = (int)(x*m_ppi) + m_xMargin;
	*py = (int)(y*m_ppi) + m_yMargin;
}

void wxPageLayoutCtrl::ScreenToPage( int px, int py, float *x, float *y )
{
	*x = (px - m_xMargin)/m_ppi;
	*y = (py - m_yMargin)/m_ppi;
}

void wxPageLayoutCtrl::OnResize( wxSizeEvent & )
{
	Invalidate();
}

void wxPageLayoutCtrl::DrawBackground( wxDC &dc )
{
	// draw the control background
	wxColour bg = GetBackgroundColour();
	dc.SetBrush(wxBrush(bg));
	dc.SetPen(wxPen(bg,1));
	wxRect windowRect( wxPoint(0,0), GetClientSize() );
	CalcUnscrolledPosition(windowRect.x, windowRect.y,
		&windowRect.x, &windowRect.y);
	dc.DrawRectangle(windowRect);
}

void wxPageLayoutCtrl::DrawPageOutline( wxDC &dc )
{
	if (!m_page) return;

	dc.SetPen( *wxTRANSPARENT_PEN );
	
	dc.SetBrush( wxBrush(*wxWHITE) );
	dc.DrawRectangle( m_page->layout_cache.x,
		m_page->layout_cache.y,
		m_page->layout_cache.width,
		m_page->layout_cache.height );

	// draw grid
	wxPen grid_pen( wxColour(220,220,220), 1, wxDOT );
	dc.SetPen(grid_pen);
	dc.SetBrush( *wxTRANSPARENT_BRUSH );

	float xmax, ymax;
	m_page->GetDimensions(&xmax, &ymax);

	float top, bottom, left, right;
	m_page->GetMargins( &top, &bottom, &left, &right );

	if ( m_showGrid && m_gridSpacing > 0 )
	{
		for (float x = left+m_gridSpacing; x < xmax-right; x += m_gridSpacing )
		{
			wxPoint start, end;
			PageToScreen( x, top, &start.x, &start.y );
			PageToScreen( x, ymax-bottom, &end.x, &end.y );

			dc.DrawLine( start, end );
		}

		for (float y = top+m_gridSpacing; y < ymax-bottom; y += m_gridSpacing )
		{
			wxPoint start, end;
			PageToScreen( left, y, &start.x, &start.y );
			PageToScreen( xmax-right, y, &end.x, &end.y );

			dc.DrawLine( start, end );
		}

		// draw margin box
		grid_pen.SetStyle( wxSOLID );
		dc.SetPen( grid_pen );
		wxPoint topleft, bottomright;
		PageToScreen( left, top, &topleft.x, &topleft.y );
		PageToScreen( xmax-right, ymax-bottom, &bottomright.x, &bottomright.y );

		dc.DrawRectangle( topleft.x, topleft.y, bottomright.x-topleft.x, bottomright.y-topleft.y );

	}

	// draw thin black page border outline
	dc.SetPen( wxPen(*wxBLACK, 1) );
	dc.SetBrush( *wxTRANSPARENT_BRUSH );

	dc.DrawRectangle( m_page->layout_cache.x,
		m_page->layout_cache.y,
		m_page->layout_cache.width,
		m_page->layout_cache.height );
}

void wxPageLayoutCtrl::OnPaint( wxPaintEvent & )
{
	wxAutoBufferedPaintDC dc(this);
	DoPrepareDC( dc );
	
	DrawBackground( dc );
	DrawPageOutline( dc );

	if (m_page)
	{
		float top, bottom, left, right, width, height;
		m_page->GetMargins(&top, &bottom, &left, &right);
		m_page->GetDimensions(&width, &height);
		wxPoint tl, br;
		PageToScreen( left, top, &tl.x, &tl.y );
		PageToScreen( width-right, height-bottom, &br.x, &br.y );		
		
		wxScreenOutputDevice dv( this, dc );
		int count = 0;
		
		wxPageObject **objects = m_page->GetObjects(&count);
		for (int i=count-1;i>=0;i--)
		{
			float x, y;
			objects[i]->GetGeometry( &x, &y, &width, &height );
			PageToScreen( x, y, &tl.x, &tl.y );
			
			dc.SetClippingRegion( tl.x, tl.y, width*m_ppi, height*m_ppi );
			objects[i]->Render( dv );
			dc.DestroyClippingRegion();

			if (m_showOutlines)
			{
				dc.SetPen( wxPen( *wxBLUE, 1, wxDOT ) );
				dc.SetBrush( *wxTRANSPARENT_BRUSH );
				dc.DrawRectangle( tl.x, tl.y, width*m_ppi, height*m_ppi );
			}

			if ( std::find( m_selectedItems.begin(),
				m_selectedItems.end(),
				objects[i] ) != m_selectedItems.end() )
			{
				wxRect rct( tl.x, tl.y, width*m_ppi, height*m_ppi );

				dc.SetPen( wxPen( m_selectColour ) );
				dc.SetBrush( wxBrush( m_selectColour, wxTRANSPARENT ) );
				dc.DrawRectangle( rct );
				
				dc.SetBrush( wxBrush( m_selectColour, wxSOLID ) );

				// left side
				dc.DrawRectangle(rct.x - RSZBOXW, rct.y - RSZBOXW, RSZBOXW, RSZBOXW);
				dc.DrawRectangle(rct.x - RSZBOXW, rct.y + rct.height/2 - RSZBOXW/2, RSZBOXW, RSZBOXW);
				dc.DrawRectangle(rct.x - RSZBOXW, rct.y + rct.height, RSZBOXW, RSZBOXW);

				// right side
				dc.DrawRectangle(rct.x + rct.width, rct.y - RSZBOXW, RSZBOXW, RSZBOXW);
				dc.DrawRectangle(rct.x + rct.width, rct.y + rct.height/2 - RSZBOXW/2, RSZBOXW, RSZBOXW);
				dc.DrawRectangle(rct.x + rct.width, rct.y + rct.height, RSZBOXW, RSZBOXW);
				
				// bottom
				dc.DrawRectangle(rct.x + rct.width/2 - RSZBOXW/2, rct.y + rct.height, RSZBOXW, RSZBOXW);

				// top
				dc.DrawRectangle(rct.x + rct.width/2 - RSZBOXW/2, rct.y - RSZBOXW, RSZBOXW, RSZBOXW);
			}
		}

		dc.DestroyClippingRegion();
	}
}

void wxPageLayoutCtrl::OnErase( wxEraseEvent & )
{
	/* nothing to do */
}

void wxPageLayoutCtrl::OnLeftDown( wxMouseEvent &evt )
{
	if (!m_page) return;

	SetFocus(); // required to obtain keyboard events, also mouse wheel operation on some systems

	CaptureMouse();

	float xpage, ypage;
	MouseToPage( evt.GetPosition(), &xpage, &ypage );
	
	int mx = m_origX = evt.GetX();
	int my = m_origY = evt.GetY();

	int vsx, vsy;
	GetViewStart(&vsx, &vsy);

	ClientToScreen( &m_origX, &m_origY );

	int box_type = 0;

	if (m_selectedItems.size() == 1 && 
		(box_type = IsOverResizeBox(mx+vsx, my+vsy, m_selectedItems[0])) > 0 )
	{
		m_resizeBox = box_type;
		// start a resize
		m_diffX = 0;
		m_diffY = 0;
		m_diffW = 0;
		m_diffH = 0;
		m_resizeMode = true;
		m_resizeModeErase = false;
	}
	else
	{
		wxPageObject *select_obj = m_page->Under( xpage, ypage );
		bool redraw = false;

		std::vector<wxPageObject*>::iterator it = std::find(m_selectedItems.begin(),
			m_selectedItems.end(), select_obj );
		
		if ( select_obj && it == m_selectedItems.end() )
		{
			m_page->Raise( select_obj );

			if (!evt.ShiftDown())
				m_selectedItems.clear();

			m_selectedItems.push_back( select_obj );
			redraw = true;
		}
		else if ( evt.ShiftDown() 
			&& select_obj 
			&& it != m_selectedItems.end() )
		{
			m_selectedItems.erase( it );
			redraw = true;
		}
		else if ( !select_obj )
		{
			if (m_selectedItems.size() > 0)
				redraw = true;
			m_selectedItems.clear();
		}

		if ( select_obj )
		{
			m_moveMode = true;
			m_moveModeErase = false;
			m_diffX = m_diffY = m_diffW = m_diffH = 0;
			
			wxPageLayoutEvent e( wxEVT_PAGELAYOUT_SELECT, this, select_obj );
			ProcessEvent( e );
		}
		else
		{			
			m_diffX = 0;
			m_diffY = 0;
			m_multiSelMode = true;
			m_multiSelModeErase = true;
		}

		if (redraw) Invalidate();
	}
}

void wxPageLayoutCtrl::OnLeftUp( wxMouseEvent & )
{
	if ( !m_page ) return;

	if (HasCapture())
		ReleaseMouse();

	int vsx, vsy;
	GetViewStart(&vsx, &vsy);

	if ( m_moveMode )
	{
		for (size_t i=0;i<m_selectedItems.size();i++)
		{
			float diffx_inches = m_diffX/m_ppi;
			float diffy_inches = m_diffY/m_ppi;

			wxPageObject *obj = m_selectedItems[i];
			float x, y;
			obj->GetGeometry( &x, &y, 0, 0 );
			x += diffx_inches;
			y += diffy_inches;
			Snap( &x, &y );
			obj->SetGeometry( x, y );
		}

		if (m_selectedItems.size() > 0 && 
			(m_diffX != 0 || m_diffY != 0))
		{
			wxPageLayoutEvent e( wxEVT_PAGELAYOUT_MODIFY, this, 0 );
			ProcessEvent( e );
		}

		m_moveMode = false;
		m_moveModeErase = false;

		Invalidate();
	}
	else if ( m_multiSelMode )
	{
		wxRect selbox;	
		selbox.x = m_diffX<0 ? m_origX + m_diffX : m_origX;
		selbox.width = m_diffX<0 ? -m_diffX : m_diffX;
		selbox.y = m_diffY<0 ? m_origY + m_diffY : m_origY;
		selbox.height = m_diffY<0 ? -m_diffY : m_diffY;
	
		ScreenToClient(&selbox.x, &selbox.y);

		selbox.x += vsx;
		selbox.y += vsy;

		float sb_x, sb_y, sb_width, sb_height;
		ScreenToPage( selbox.x, selbox.y, &sb_x, &sb_y );
		sb_width = selbox.width / m_ppi;
		sb_height = selbox.height / m_ppi;

		int count = 0;
		wxPageObject **objects = m_page->GetObjects( &count );
		for (int i=0;i<count;i++)
		{
			float x, y, width, height, right, bottom;
			objects[i]->GetGeometry( &x, &y, &width, &height );
			right = x + width;
			bottom = y + height;

			// selbox contains top left corner
			if ( x >= sb_x && y >= sb_y 
				&& (y-sb_y) < sb_height
				&& (x-sb_x) < sb_width

				// as well as bottom right corner
				&& right >= sb_x && bottom >= sb_y 
				&& (bottom-sb_y) < sb_height
				&& (right-sb_x) < sb_width )
			{
				std::vector<wxPageObject*>::iterator it = std::find( m_selectedItems.begin(), m_selectedItems.end(), objects[i] );
				if ( it == m_selectedItems.end() )
					m_selectedItems.push_back( objects[i] );
				else
					m_selectedItems.erase( it );
			}
		}
		
		wxPageLayoutEvent e( wxEVT_PAGELAYOUT_SELECT, this, 0 );
		ProcessEvent( e );

		m_multiSelMode = false;
		m_multiSelModeErase = false;

		Invalidate();
	}
	else if ( m_resizeMode && m_selectedItems.size() == 1 )
	{
		wxPageObject *obj = m_selectedItems[0];
		float x, y, width, height;
		obj->GetGeometry( &x, &y, &width, &height );
		x += m_diffX / m_ppi;
		y += m_diffY / m_ppi;
		width += m_diffW / m_ppi;
		height += m_diffH / m_ppi;

		if (width < 0.01) width = 0.01f;
		if (height < 0.01) height = 0.01f;

		obj->SetGeometry( x, y, width, height );
	
		if ( m_diffX != 0 || m_diffY != 0 || m_diffW != 0 || m_diffH != 0)
		{
			wxPageLayoutEvent e( wxEVT_PAGELAYOUT_MODIFY, this, obj );
			ProcessEvent( e );
		}

		m_resizeMode = false;
		m_resizeModeErase = false;
		Invalidate();
	}

	SetCursor( m_standardCursor );
}

void wxPageLayoutCtrl::OnLeftDouble( wxMouseEvent &evt )
{
	if (!m_page) return;

	float x, y;
	MouseToPage( evt.GetPosition(), &x, &y );
	wxPageObject *obj = m_page->Under(x,y);
	if (obj)
	{
		if ( obj->EditObject(this) )
		{
			wxPageLayoutEvent e( wxEVT_PAGELAYOUT_MODIFY, this, 0 );
			ProcessEvent( e );
		}
		Invalidate();
	}
}

void wxPageLayoutCtrl::OnRightDown( wxMouseEvent &evt )
{
	if ( 0 == m_page ) return; // no popup menu without an active page to edit
	if (m_page->GetOrientation() == wxLANDSCAPE)
		m_popupMenu->Check( ID_LANDSCAPE, true );
	else
		m_popupMenu->Check( ID_PORTRAIT, true );

	m_popupMenu->Check( ID_SNAP, m_snapCoordinates );
	m_popupMenu->Check( ID_SHOWGRID, m_showGrid );
	m_popupMenu->Check( ID_SHOWOUTLINES, m_showOutlines );

	wxPaperSize sz = m_page->GetPaperType();
	for ( size_t i=0;i<m_paperMenuInfo.size();i++ )
	{
		if ( sz == m_paperMenuInfo[i].paperSize )
		{
			m_popupMenu->Check( ID_PAPER_FIRST+i, true );				
			break;
		}
	}

	int vsx, vsy;
	GetViewStart( &vsx, &vsy );
	m_popupX = evt.GetX() + vsx;
	m_popupY = evt.GetY() + vsy;
	PopupMenu( m_popupMenu, m_popupX, m_popupY );
}

void wxPageLayoutCtrl::OnMouseMove( wxMouseEvent &evt )
{
	if (!m_page) return;

	float xpage, ypage;
	MouseToPage( evt.GetPosition(), &xpage, &ypage );

	int mx = evt.GetX();
	int my = evt.GetY();
	int xroot = mx;
	int yroot = my;

	int vsx, vsy;
	GetViewStart(&vsx, &vsy);

	ClientToScreen( &xroot, &yroot );

	if ( m_moveMode )
	{
		if (m_moveModeErase )
			DrawMoveResizeOutlines();

		m_diffX = xroot - m_origX;
		m_diffY = yroot - m_origY;

		float x = m_diffX / m_ppi;
		float y = m_diffY / m_ppi;

		Snap( &x, &y );

		m_diffX = (int)(x * m_ppi);
		m_diffY = (int)(y * m_ppi);

		DrawMoveResizeOutlines();
		m_moveModeErase = true;
	}
	else if ( m_resizeMode )
	{
		SetResizeCursor();

		if ( m_resizeModeErase )
			DrawMoveResizeOutlines();

		int diffx = xroot - m_origX;
		int diffy = yroot - m_origY;
		

		float x = diffx / m_ppi;
		float y = diffy / m_ppi;
		Snap( &x, &y );
		diffx = (int)(x * m_ppi);
		diffy = (int)(y * m_ppi);

		switch(m_resizeBox)
		{
		case BOX_TOPLEFT:
			m_diffX = diffx;
			m_diffY = diffy;
			m_diffW = -diffx;
			m_diffH = -diffy;
			break;
		case BOX_TOPRIGHT:
			m_diffX = 0;
			m_diffY = diffy;
			m_diffW = diffx;
			m_diffH = -diffy;
			break;
		case BOX_BOTTOMLEFT:
			m_diffX = diffx;
			m_diffY = 0;
			m_diffW = -diffx;
			m_diffH = diffy;
			break;
		case BOX_BOTTOMRIGHT:
			m_diffX = 0;
			m_diffY = 0;
			m_diffW = diffx;
			m_diffH = diffy;
			break;
		case BOX_TOP:
			m_diffX = 0;
			m_diffY = diffy;
			m_diffW = 0;
			m_diffH = -diffy;				
			break;
		case BOX_LEFT:
			m_diffX = diffx;
			m_diffY = 0;
			m_diffW = -diffx;
			m_diffH = 0;
			break;
		case BOX_RIGHT:
			m_diffX = 0;
			m_diffY = 0;
			m_diffW = diffx;
			m_diffH = 0;
			break;
		case BOX_BOTTOM:
			m_diffX = 0;
			m_diffY = 0;
			m_diffW = 0;
			m_diffH = diffy;
			break;
		default:
			break;
		}

		DrawMoveResizeOutlines();
		m_resizeModeErase = true;
	}
	else if ( m_multiSelMode)
	{
		if ( m_multiSelModeErase )
			DrawMultiSelBox();

		m_diffX = xroot - m_origX;
		m_diffY = yroot - m_origY;

		DrawMultiSelBox();
		m_multiSelModeErase = true;
	}
	else if ( m_selectedItems.size() == 1)
	{
		int box = IsOverResizeBox(mx+vsx, my+vsy, m_selectedItems[0]);
		if (box)
			SetResizeCursor(box);
		else
			SetCursor( m_standardCursor );
	}
}

void wxPageLayoutCtrl::OnLeave( wxMouseEvent & )
{
}

void wxPageLayoutCtrl::OnMouseWheel( wxMouseEvent &evt )
{
	SetPPI( GetPPI() + evt.GetWheelRotation()/20.0 );
}

void wxPageLayoutCtrl::Zoom( float percent )
{
	m_ppi = m_screenPPI * percent / 100.0f;

	if ( m_ppi < 0 )
	{
		wxSize client = GetClientSize();
		client.x -= 2*m_xMargin;
		client.y -= 2*m_yMargin;
		float width, height;
		m_page->GetDimensions(&width, &height);
		float ppix = client.x / width;
		float ppiy = client.y / height;
		m_ppi = (ppix<ppiy) ? ppix : ppiy;
	}

	if ( m_ppi < 10 ) m_ppi = 10;
	if ( m_ppi > 300 ) m_ppi = 300;

	Invalidate();
}

void wxPageLayoutCtrl::FitHorizontal()
{
	if ( !m_page ) return;

	wxSize client = GetClientSize();
	client.x -= 2*m_xMargin;
	float h, v;
	m_page->GetDimensions(&h, &v);
	m_ppi = client.x / h;
	Invalidate();
}

void wxPageLayoutCtrl::FitVertical()
{
	if ( !m_page ) return;

	wxSize client = GetClientSize();
	client.y -= 2*m_yMargin;
	float h, v;
	m_page->GetDimensions(&h, &v);
	m_ppi = client.y / v;
	Invalidate();
}

void wxPageLayoutCtrl::AlignEdges( int edge )
{
	if (m_page && m_selectedItems.size() > 1)
	{
		float x0, y0, width0, height0;
		m_selectedItems[0]->GetGeometry( &x0, &y0, &width0, &height0 );

		for (size_t i=1;i<m_selectedItems.size();i++)
		{
			float x, y, width, height;
			m_selectedItems[i]->GetGeometry( &x, &y, &width, &height );

			if (edge == wxTOP)
				y = y0;
			else if (edge == wxLEFT)
				x = x0;
			else if (edge == wxRIGHT)
				x = (x0+width0)-width;
			else if (edge == wxBOTTOM)
				y = (y0+height0)-height;

			m_selectedItems[i]->SetGeometry( x, y, width, height );
		}

		if (m_selectedItems.size() > 1)
		{
			wxPageLayoutEvent e( wxEVT_PAGELAYOUT_MODIFY, this, 0 );
			ProcessEvent( e );
		}
		Invalidate();
	}
}

void wxPageLayoutCtrl::OnPopup( wxCommandEvent &evt )
{
	if (!m_page) return;

	int cmd = evt.GetId();
	if (cmd >= ID_CREATE && cmd < ID_CREATE_LAST)
	{
		wxArrayString types = wxPageObjectTypes::AllTypes();
		int idx = cmd-ID_CREATE;
		if (idx >= 0 && idx < (int)types.size() )
		{
			wxPageObject *obj = wxPageObjectTypes::Create( types[idx] );
			if (obj)
			{
				float popx, popy;
				ScreenToPage( m_popupX, m_popupY, &popx, &popy );
				Snap(&popx, &popy);
				obj->SetGeometry(popx, popy, 4, 3);
				m_page->Add(obj);

				wxPageLayoutEvent evt_cre( wxEVT_PAGELAYOUT_CREATE, this, obj );
				ProcessEvent( evt_cre );

				wxPageLayoutEvent evt_mod( wxEVT_PAGELAYOUT_MODIFY, this, 0 );
				ProcessEvent( evt_mod );

				Invalidate();
			}
		}
	}
	else if (cmd == ID_DUPLICATE)
	{
		if ( m_selectedItems.size() > 0 )
		{
			float popx, popy;
			ScreenToPage( m_popupX, m_popupY, &popx, &popy );

			std::vector< wxPageObject* > added;
			float x0 = 0.0f, y0 = 0.0f;
			m_page->GetDimensions(&x0, &y0);

			for (size_t i=0; i < m_selectedItems.size(); i++)
			{
				float x, y;
				m_selectedItems[i]->GetGeometry( &x, &y, 0, 0 );
				if ( x < x0 ) x0 = x;
				if ( y < y0 ) y0 = y;
			}

			for (size_t i=0; i < m_selectedItems.size(); i++)
			{
				wxPageObject *source = m_selectedItems[i];
				float x, y, width, height;
				source->GetGeometry( &x, &y, &width, &height );

				float dx = x - x0;
				float dy = y - y0;						

				wxPageObject *target = source->Duplicate();
				target->SetGeometry( popx+dx, popy+dy, width, height );				
				m_page->Add(target);
				added.push_back( target );
				
				wxPageLayoutEvent e( wxEVT_PAGELAYOUT_CREATE, this, target );
				ProcessEvent( e );
			}

			m_selectedItems = added;
			Invalidate();

			wxPageLayoutEvent e( wxEVT_PAGELAYOUT_MODIFY, this, 0 );
			ProcessEvent( e );
		}
	}
	else if (cmd == ID_DELETE )
	{
		for (size_t i=0;i<m_selectedItems.size();i++)
			m_page->Delete( m_selectedItems[i] );

		m_selectedItems.clear();
	
		wxPageLayoutEvent e( wxEVT_PAGELAYOUT_MODIFY, this, 0 );
		ProcessEvent( e );
		Invalidate();
	}
	else if (cmd >= ID_PAPER_FIRST && cmd < ID_PAPER_LAST )
	{
		int idx = cmd-ID_PAPER_FIRST;
		if (idx >= 0 && idx < (int)m_paperMenuInfo.size())
		{
			m_page->SetPaperType( m_paperMenuInfo[idx].paperSize );
			
			wxPageLayoutEvent e( wxEVT_PAGELAYOUT_MODIFY, this, 0 );
			ProcessEvent( e );
		}
	}
	else if ( cmd == ID_PORTRAIT )
	{
		m_page->SetOrientation( wxPORTRAIT );
		wxPageLayoutEvent e( wxEVT_PAGELAYOUT_MODIFY, this, 0 );
		ProcessEvent( e );
	}
	else if ( cmd == ID_LANDSCAPE )
	{
		m_page->SetOrientation( wxLANDSCAPE );
		wxPageLayoutEvent e( wxEVT_PAGELAYOUT_MODIFY, this, 0 );
		ProcessEvent( e );
	}
	else if ( cmd >= ID_ZOOM50 && cmd <= ID_ZOOM150 )
	{
		float factor = ((cmd-ID_ZOOM50)*25 + 50)/100.0f;
		m_ppi = m_screenPPI*factor;
		Invalidate();
	}
	else if ( cmd == ID_FITBOTH )
	{
		Zoom();
	}
	else if ( cmd == ID_FITHORIZ )
	{
		FitHorizontal();
	}
	else if ( cmd == ID_FITVERT )
	{
		FitVertical();
	}
	else if ( cmd == ID_SHOWGRID )
	{
		m_showGrid = m_popupMenu->IsChecked(ID_SHOWGRID);
		Invalidate();
	}
	else if ( cmd == ID_SHOWOUTLINES )
	{
		m_showOutlines = m_popupMenu->IsChecked(ID_SHOWOUTLINES);
		Invalidate();
	}
	else if ( cmd == ID_SETGRIDSPACING)
	{
		wxString text = wxGetTextFromUser("Enter grid spacing in inches:", "Grid", wxString::Format("%f", m_gridSpacing) );
		double spacing;
		if (!text.IsEmpty() && text.ToDouble(&spacing))
		{
			if (spacing < 0.05) spacing = 0.05;
			if (spacing > 5) spacing = 5;

			m_gridSpacing = (float)spacing;
			if (m_showGrid) Invalidate();
		}
	}
	else if ( cmd == ID_SETSNAPSPACING )
	{
		wxString text = wxGetTextFromUser("Enter snap spacing in inches:", "Snapping", wxString::Format("%f", m_snapSpacing) );
		double spacing;
		if (!text.IsEmpty() && text.ToDouble(&spacing))
		{
			if (spacing < 0.01) spacing = 0.01;
			if (spacing > 2) spacing = 2;

			m_snapSpacing = (float)spacing;
		}
	}
	else if ( cmd == ID_SNAP )
	{
		m_snapCoordinates = m_popupMenu->IsChecked( ID_SNAP );
	}
	else if ( cmd == ID_ALIGNTOP )
	{
		AlignEdges( wxTOP );
	}
	else if ( cmd == ID_ALIGNBOTTOM )
	{
		AlignEdges( wxBOTTOM );
	}
	else if ( cmd == ID_ALIGNLEFT )
	{
		AlignEdges( wxLEFT );
	}
	else if ( cmd == ID_ALIGNRIGHT )
	{
		AlignEdges( wxRIGHT );	
	}
	else if ( cmd == ID_SAVEFILE )
	{
		wxFileDialog dlg( this, "Save Page Layout File", wxEmptyString, wxEmptyString,
			"Page Layout Files (*.pld)|*.pld", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
		if (dlg.ShowModal() == wxID_OK)
		{
			wxFFileOutputStream fos( dlg.GetPath() );
			if (!fos.IsOk())
			{
				wxMessageBox("Could not open file for writing:\n\n" + dlg.GetPath());
				return;
			}

			if (! m_page->Write( fos ))
				wxMessageBox("Error writing data to file.");
		}
	}
	else if ( cmd == ID_EXPORTPDF )
	{
		wxFileDialog dlg( this, "Export current page as PDF", wxEmptyString, wxEmptyString,
			"Portable Document Format Files (*.pdf)|*.pdf", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
		if (dlg.ShowModal() == wxID_OK)
		{
			wxPagePdfRenderer pdf;
			pdf.AddPage( m_page );
			if ( !pdf.Render( dlg.GetPath() ) )
				wxMessageBox("Failed to render PDF document:\n\n" + dlg.GetPath());
		}
	}
	else if ( cmd == ID_LOADFILE )
	{
		wxFileDialog dlg( this, "Load Page Layout File", wxEmptyString, wxEmptyString,
			"Page Layout Files (*.pld)|*.pld", wxFD_OPEN );
		if (dlg.ShowModal() == wxID_OK)
		{
			wxFFileInputStream fis( dlg.GetPath() );
			if ( !fis.IsOk() )
			{
				wxMessageBox("Could not open file for reading:\n\n" + dlg.GetPath());
				return;
			}

			wxBusyInfo info("Loading page layout data...");
			wxYield();

			m_page->Clear();
			if ( !m_page->Read( fis ) )
				wxMessageBox("Error reading data from file.");

			Invalidate();				
		}
	}
}

void wxPageLayoutCtrl::MouseToPage( const wxPoint &pt, float *x, float *y )
{
	int vsx, vsy;
	GetViewStart(&vsx,&vsy);
	ScreenToPage( vsx+pt.x, vsy+pt.y, x, y );
}

void wxPageLayoutCtrl::DrawMultiSelBox()
{
	if ( !m_multiSelMode ) return;

	wxClientDC dc(this);
	PrepareDC( dc );

	dc.SetLogicalFunction( wxINVERT );
	wxBrush brush( *wxWHITE, wxTRANSPARENT );
	wxPen pen(*wxBLACK, 2, wxSOLID);
	pen.SetCap(wxCAP_BUTT);
	pen.SetJoin(wxJOIN_MITER);
	dc.SetBrush(brush);
	dc.SetPen(pen);

	wxRect selbox;	
	selbox.x = m_diffX<0 ? m_origX + m_diffX : m_origX;
	selbox.width = m_diffX<0 ? -m_diffX : m_diffX;

	selbox.y = m_diffY<0 ? m_origY + m_diffY : m_origY;
	selbox.height = m_diffY<0 ? -m_diffY : m_diffY;

	ScreenToClient(&selbox.x, &selbox.y);

	int vsx, vsy;
	GetViewStart(&vsx, &vsy);	
	selbox.x += vsx;
	selbox.y += vsy;

	dc.DrawRectangle( selbox );
}

void wxPageLayoutCtrl::DrawMoveResizeOutlines()
{
	wxClientDC dc(this);
	PrepareDC( dc );
	dc.SetLogicalFunction( wxINVERT );
	wxBrush brush( *wxWHITE, wxTRANSPARENT );
	wxPen pen(*wxBLACK, 2, wxSOLID);
	pen.SetCap(wxCAP_BUTT);
	pen.SetJoin(wxJOIN_MITER);
	dc.SetBrush(brush);
	dc.SetPen(pen);

	for (size_t i=0;i<m_selectedItems.size();i++)
	{
		wxPageObject *obj = m_selectedItems[i];

		float x, y, width, height;
		obj->GetGeometry( &x, &y, &width, &height );

		x += m_diffX / m_ppi;
		y += m_diffY / m_ppi;
		width += m_diffW / m_ppi;
		height += m_diffH / m_ppi;

		Snap( &x, &y );

		wxRect rct;
		PageToScreen( x, y, &rct.x, &rct.y );
		rct.width = width * m_ppi;
		rct.height = height * m_ppi;

		dc.DrawRectangle(rct);
	}

}

void wxPageLayoutCtrl::SetResizeCursor(int pos)
{
	if (pos < 0)
		pos = m_resizeBox;

	switch(pos)
	{
	case BOX_TOPLEFT:
	case BOX_BOTTOMRIGHT:
		SetCursor( m_nwseCursor );
		break;
	case BOX_TOPRIGHT:
	case BOX_BOTTOMLEFT:
		SetCursor( m_neswCursor );
		break;
	case BOX_TOP:
	case BOX_BOTTOM:
		SetCursor( m_nsCursor );
		break;
	case BOX_LEFT:
	case BOX_RIGHT:
		SetCursor( m_weCursor );
		break;
	default:
		SetCursor( m_moveResizeCursor );
	}
}

int wxPageLayoutCtrl::IsOverResizeBox(int x, int y, wxPageObject *obj)
{
	float fx, fy, fw, fh;
	obj->GetGeometry( &fx, &fy, &fw, &fh );

	wxRect rct;
	PageToScreen( fx, fy, &rct.x, &rct.y );
	rct.width = fw * m_ppi;
	rct.height = fh * m_ppi;
	
	if (x >= rct.x-RSZBOXW && x <= rct.x &&
			y >= rct.y-RSZBOXW && y <= rct.y)
		return BOX_TOPLEFT;	
	
	if (x >= rct.x-RSZBOXW && x <= rct.x &&
			y >= rct.y + rct.height/2 - RSZBOXW/2 && y <= rct.y + rct.height/2 + RSZBOXW/2)
		return BOX_LEFT;

	if (x >= rct.x-RSZBOXW && x <= rct.x &&
			y >= rct.y + rct.height && y <= rct.y + rct.height + RSZBOXW)
		return BOX_BOTTOMLEFT;
	
	if (x >= rct.x+rct.width/2-RSZBOXW/2 && x <= rct.x+rct.width/2+RSZBOXW/2 &&
			y >= rct.y-RSZBOXW && y <= rct.y)
		return BOX_TOP;
	
	if (x >= rct.x+rct.width/2-RSZBOXW/2 && x <= rct.x+rct.width/2+RSZBOXW/2 &&
			y >= rct.y + rct.height && y <= rct.y + rct.height + RSZBOXW)
		return BOX_BOTTOM;

	if (x >= rct.x+rct.width && x <= rct.x+rct.width+RSZBOXW &&
			y >= rct.y-RSZBOXW && y <= rct.y)
		return BOX_TOPRIGHT;	
	
	if (x >= rct.x+rct.width && x <= rct.x+rct.width+RSZBOXW &&
			y >= rct.y + rct.height/2 - RSZBOXW/2 && y <= rct.y + rct.height/2 + RSZBOXW/2)
		return BOX_RIGHT;

	if (x >= rct.x+rct.width && x <= rct.x+rct.width+RSZBOXW &&
			y >= rct.y + rct.height && y <= rct.y + rct.height + RSZBOXW)
		return BOX_BOTTOMRIGHT;

	return BOX_NONE;
}

void wxPageLayoutCtrl::Snap(float *x, float *y)
{
	if (!m_snapCoordinates) return;
	*x = Snap(*x);
	*y = Snap(*y);
}

float wxPageLayoutCtrl::Snap( float v )
{
	int multiples = (int)(v / m_snapSpacing);
	float dist1 = (float)fabs(m_snapSpacing*multiples - v);
	float dist2 = (float)fabs(m_snapSpacing*(multiples+1) - v);
	if (dist1 < dist2) return m_snapSpacing*multiples;
	else return m_snapSpacing*(multiples+1);
}
