#include <wx/dcbuffer.h>
#include <wx/numformatter.h>
#include <wx/clipbrd.h>
#include <wx/busyinfo.h>

#include <wex/numeric.h>

#include "invoke.h"
#include "lossdiag.h"
#include "main.h"



LossDiagramObject::LossDiagramObject()
{
	m_createFromCase = false;
}

wxPageObject *LossDiagramObject::Duplicate()
{
	LossDiagramObject *obj = new LossDiagramObject;
	obj->Copy( this );
	return obj;
}

bool LossDiagramObject::Copy( wxPageObject *obj )
{
	if ( LossDiagramObject *src = dynamic_cast<LossDiagramObject*>(obj) )
	{
		m_createFromCase = src->m_createFromCase;
		m_list = src->m_list;
		return true;
	}
	else
		return false;
}

bool LossDiagramObject::EditObject( wxPageLayoutCtrl *layout_window )
{
	wxBusyInfo info("Updating loss diagram from current case." );
	wxMilliSleep(200);
	return SetupFromCase();
}

wxRealPoint LossDiagramObject::EstimateSize( double height_char ) const
{
	int nbaselines = 0;
	int nlosses = 0;
	double textwidth = 0;
	for( size_t i=0;i<m_list.size();i++ )
	{
		double tw = m_list[i].text.Len() * height_char/2; // assume average character is half as wide as tall
		if ( textwidth < tw ) textwidth = tw;

		if ( m_list[i].baseline ) nbaselines++;
		else nlosses++;
	}
	
	return wxRealPoint( 300+textwidth, height_char*4*nbaselines + height_char*3*nlosses + height_char );
}

void LossDiagramObject::SetCaseName( const wxString &c )
{
	SamReportObject::SetCaseName( c );
	SetupFromCase();
}

#define LD_BORDER_INCH 0.1 // inset border on loss diagram, inches (roughly 7 pixels)

void LossDiagramObject::Render( wxPageOutputDevice &dv )
{
	float x, y, width, height;
	GetGeometry( &x, &y, &width, &height );
	
	int face = wxPageOutputDevice::SANSERIF;
	int points = 10;

	dv.Font( face, points, true, false );

	if ( m_list.size() == 0 )
	{
		float tw, th;
		dv.Measure( "hy", &tw, &th );
		dv.Text( x, y+th, "No loss diagram items specified." );
		dv.Text( x, y+th+th, wxString("Current case: ") + ( GetCase() ? GetCaseName() : wxString("none") ) );
		return;
	}

	x += LD_BORDER_INCH;
	y += LD_BORDER_INCH;
	width -= 2*LD_BORDER_INCH;
	height -= 2*LD_BORDER_INCH;

	// find out longest text string
	float tw = 0, th = 0, twmax = 0;
	for( size_t i=0;i<m_list.size();i++ )
	{
		dv.Measure( m_list[i].text, &tw, &th );
		if ( tw > twmax ) twmax = tw;
	}
	
	float cursize = width - twmax;
	float textx = x+cursize+0.1;
	
	float sec_height = th*4; 
	dv.Color( *wxBLACK );
	float linewidth = 0.015;
	dv.LineStyle( linewidth, wxPageOutputDevice::SOLID );
	for( size_t i=0;i<m_list.size();i++ )
	{
		ld_item &li = m_list[i];

		if ( li.baseline )
		{
			if ( i > 0 ) // close up previous section and move down
			{
				dv.Line( x, y, x+cursize, y ); // section top line
				y += 0.3f; // spacing between sections
			}

			dv.Line( x, y, x+cursize, y ); // section top line
			dv.Line( x, y, x, y + sec_height ); // left vertical line
			dv.Line( x+cursize, y, x+cursize, y+sec_height ); // right vertical line
			
			textx = x + cursize + 0.2; // realign text
			
			dv.Font( face, points+2, true, false );
			dv.Text( x+0.1f, y+0.05f, li.text );
			dv.Text( x+0.1f, y+0.05f+th*1.2f, 
				wxNumberFormatter::ToString( li.value, 0, wxNumberFormatter::Style_WithThousandsSep ) );
			dv.Font( face, points, false, false );
			y += sec_height;
		}
		else
		{
			float lw = li.value/100.0f*cursize; // width of section that is lost

			dv.Line( x, y, x, y+sec_height ); // left vertical line

			// text point
			float tpx = textx-0.05;
			float tpy = y+sec_height/2;

			//dv.Line( x+cursize, y, x+cursize-lw/2, y+sec_height/2 );  // triangle line 1
			//dv.Line( x+cursize-lw/2, y+sec_height/2, x+cursize-lw, y ); // triangle line 2

			dv.Arc( x + cursize, y-sec_height/2, 2*(tpx-(x+cursize)), sec_height, 180, 270 );
			dv.Arc( x + cursize-lw, y-sec_height/2, 2*(tpx-(x+cursize-lw)), sec_height, 180, 270);

			//dv.Line( x+cursize, y, x+cursize, y+sec_height/2); // vertical line down on right
			dv.Line( x+cursize-lw, y, x+cursize-lw, y+sec_height ); // vertical line down on left

			//dv.LineStyle( linewidth, wxPageOutputDevice::DOTTED );
			//dv.Line( x+cursize-lw/2, y+sec_height/2, textx-0.05, y+sec_height/2 ); // triangle point to text line
			//dv.LineStyle( linewidth, wxPageOutputDevice::SOLID );

			dv.Text( textx, tpy-th, li.text );
			dv.Text( textx, tpy+0.1f*th, wxString::Format("-%lg %%", li.value ) );

			cursize -= lw;
			y += sec_height;

		}
	}

	// close up with bottom point
	dv.Line( x, y, x+cursize/2, y+th );
	dv.Line( x+cursize/2, y+th, x+cursize, y );
}

bool LossDiagramObject::ReadData( wxInputStream &is )
{
	return true;
}

bool LossDiagramObject::WriteData( wxOutputStream &os )
{
	return true;
}

void LossDiagramObject::Configure( bool from_case )
{
	m_createFromCase = from_case;
}


LossDiagCallbackContext::LossDiagCallbackContext( Case *c, Simulation *sim, LossDiagramObject *ld, const wxString &desc )
	: CaseCallbackContext( c, desc ), m_sim( sim ), m_lossDiag( ld ) { }

LossDiagramObject &LossDiagCallbackContext::GetDiagram() { return *m_lossDiag; }
Simulation &LossDiagCallbackContext::GetSimulation() { return *m_sim; }
	
void LossDiagCallbackContext::SetupLibraries( lk::env_t *env )
{
	env->register_funcs( invoke_lossdiag_funcs(), this );
}

bool LossDiagramObject::SetupFromCase()
{
	Clear();

	// todo: automatically generate based on stored case data
	Case *c = GetCase();
	if ( !c ) return false;
	
	ConfigInfo *cfg = c->GetConfiguration();
	if ( !cfg ) return false;


	LossDiagCallbackContext context( c, &c->BaseCase(), this, "LossDiagramObject::SetupFromCase");
	if ( lk::node_t *cb = SamApp::GlobalCallbacks().Lookup( "loss_diagram", cfg->Technology ))
		return context.Invoke( cb, SamApp::GlobalCallbacks().GetEnv() );
	else
		return false;
}

void LossDiagramObject::Clear()
{
	m_list.clear();
}

size_t LossDiagramObject::Size() const
{
	return m_list.size();
}

void LossDiagramObject::NewBaseline( double value, const wxString &text )
{
	ld_item x;
	x.baseline = true;
	x.value = value;
	x.text = text;
	m_list.push_back( x );
}

void LossDiagramObject::AddLossTerm( double percent, const wxString &text )
{
	ld_item x;
	x.baseline = false;
	x.value = percent;
	x.text = text;
	m_list.push_back( x );
}

enum{ ID_COPY_IMAGE = wxID_HIGHEST+482 };

BEGIN_EVENT_TABLE( LossDiagramCtrl, wxWindow )
	EVT_SIZE( LossDiagramCtrl::OnSize )
	EVT_PAINT( LossDiagramCtrl::OnPaint )
	EVT_RIGHT_DOWN( LossDiagramCtrl::OnRightDown )
	EVT_MENU( ID_COPY_IMAGE, LossDiagramCtrl::OnContextMenu )
END_EVENT_TABLE()

LossDiagramCtrl::LossDiagramCtrl( wxWindow *parent )
	: wxWindow( parent, wxID_ANY )
{	
	SetBackgroundStyle( wxBG_STYLE_PAINT );

	wxSize mm = wxGetDisplaySizeMM();
	wxSize sz = wxGetDisplaySize();

	float ppix = sz.x / (mm.x/25.4) ;
	float ppiy = sz.y / (mm.y/25.4);

	if ( ppix == ppiy ) m_ppi = ppix;
	else m_ppi = (ppix<ppiy)?ppix:ppiy;
}

void LossDiagramCtrl::OnSize( wxSizeEvent & )
{
	Refresh();
}

void LossDiagramCtrl::OnPaint( wxPaintEvent & )
{
	wxAutoBufferedPaintDC pdc( this );

	pdc.SetBackground( *wxWHITE_BRUSH );
	pdc.Clear();

	int width, height;
	GetClientSize( &width, &height );
	
	float ppi = 72.0f;
	wxSize sz = pdc.GetPPI();
	if (sz.x != sz.y) ppi = (float) (sz.x>sz.y)?sz.x:sz.y;
	else ppi = (float)sz.x;

	m_lossDiagram.SetGeometry( 0, 0, width/ppi, height/ppi );

	wxScreenOutputDevice scrn( this, pdc );
	m_lossDiagram.Render( scrn );
}

void LossDiagramCtrl::PageToScreen( float x, float y, int *px, int *py )
{
	*px = (int)(x*m_ppi);
	*py = (int)(y*m_ppi);
}

void LossDiagramCtrl::ScreenToPage( int px, int py, float *x, float *y )
{
	*x = (px)/m_ppi;
	*y = (py)/m_ppi;
}

wxSize LossDiagramCtrl::DoGetBestSize() const
{
	wxClientDC dc( const_cast<LossDiagramCtrl*>( this ) );
	dc.SetFont( *wxNORMAL_FONT );
	wxRealPoint pt = m_lossDiagram.EstimateSize( (float) dc.GetCharHeight() );
	return wxSize( (int)pt.x, (int)pt.y );
}


wxBitmap LossDiagramCtrl::GetBitmap()
{
	wxSize size( GetClientSize() );
	wxBitmap bit( size.x, size.y );
	wxMemoryDC dc( bit );	
	dc.SetBackground( *wxWHITE_BRUSH );
	dc.Clear();	
	wxScreenOutputDevice scrn( this, dc );
	m_lossDiagram.Render( scrn );
	return bit;
}

void LossDiagramCtrl::OnRightDown( wxMouseEvent & )
{
	wxMenu menu;
	menu.Append( ID_COPY_IMAGE, "Copy image" );
	PopupMenu( &menu );
}

void LossDiagramCtrl::OnContextMenu( wxCommandEvent &evt )
{
	if ( evt.GetId() == ID_COPY_IMAGE )
	{
		if ( wxTheClipboard->Open() )
		{
			wxTheClipboard->SetData(new wxBitmapDataObject( GetBitmap() ));
			wxTheClipboard->Close();
		}
	}
}

void loss_diagram_test()
{
	wxFrame *frame = new wxFrame( 0, wxID_ANY, "Loss Diagram Test", wxDefaultPosition, wxSize(500,750) );
	LossDiagramCtrl *ldc = new LossDiagramCtrl( frame );

	LossDiagramObject &ld = ldc->GetDiagram();
	ld.NewBaseline( 52595, "Nominal POA" );
	
	ld.AddLossTerm( 1.5, "Shading" );
	ld.AddLossTerm( 4.9, "Soiling" );
	
	ld.NewBaseline( 8142, "DC kWh @ STC" );
	ld.AddLossTerm( 13.64, "Module modeled loss" );
	ld.AddLossTerm( 2.2, "Mismatch" );
	ld.AddLossTerm( 0.5, "Connections" );
	ld.AddLossTerm( 1, "Nameplate" );
	
	ld.NewBaseline( 7135, "Net DC output" );
	ld.AddLossTerm( 1.8, "Clipping" );
	ld.AddLossTerm( 2.9, "Inverter efficiency" );
	ld.AddLossTerm( 0.0, "No loss" );
	ld.AddLossTerm( 1.7, "Wiring" );
	ld.AddLossTerm( 4.2, "Performance adjustment" );
	ld.NewBaseline( 6777, "Energy to grid" );
	

	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( ldc, 1, wxALL|wxEXPAND, 5 );
	frame->SetSizerAndFit( sizer );

	frame->Show();
}