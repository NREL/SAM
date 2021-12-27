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

#include <wx/dcbuffer.h>
#include <wx/clipbrd.h>
#include <wx/busyinfo.h>
#include <wx/ffile.h>
#include <wx/wfstream.h>

#include <wex/pdf/pdfdoc.h>
#include <wex/numeric.h>

#include "invoke.h"
#include "lossdiag.h"
#include "main.h"



LossDiagramObject::LossDiagramObject()
{
	m_createFromCase = false;
	m_scaleToGeometry = false;
}

LossDiagramObject::LossDiagramObject( bool fromcase, bool scale )
{
	m_createFromCase = fromcase;
	m_scaleToGeometry = scale;
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
		m_scaleToGeometry = src->m_scaleToGeometry;
		m_list = src->m_list;
		return true;
	}
	else
		return false;
}

bool LossDiagramObject::EditObject( wxPageLayoutCtrl * )
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

	float sec_height = 3*height_char;
	
	// assume a 10 pix char height x 20 --> 200 pix minimum width roughly
	return wxRealPoint( height_char*20 + textwidth, 
		sec_height*m_list.size() // section heights
		+ nbaselines*height_char // space btw sections
		+ height_char); //vertical border
}

void LossDiagramObject::SetCaseName( const wxString &c )
{
	SamReportObject::SetCaseName( c );
	SetupFromCase();
}


void LossDiagramObject::Render( wxPageOutputDevice &dv )
{
	float x, y, width, height;
	GetGeometry(&x, &y, &width, &height);
    
    float tw, th;
	int face = wxPageOutputDevice::SANSERIF;
	int points = 10;
	dv.Font( face, points, false, false );
	dv.Measure( "hy", &th, &th );
	
	if ( m_scaleToGeometry ) // not a true scaling, but squish down vertically to fit into available height
	{
		while( points > 5 && EstimateSize(th).y > height )
		{
			points--;
			dv.Font( face, points, false, false );
			dv.Measure( "hy", &th, &th );
		}
	}

    if ( m_list.size() == 0 )
	{
		dv.Text( x, y+th, "This performance model does not specify any loss diagram items." );
		dv.Text( x, y+th+th, wxString("Current case name is ") + ( GetCase() ? GetCaseName() : wxString("none") ) );
		return;
	}

	// borders
	float border = th/2;
	x += border;
	y += border;
	width -= 2*border;
	height -= 2*border;
	

	// find out longest text string
	float twmax = 0;
	for( size_t i=0;i<m_list.size();i++ )
	{
		dv.Measure( m_list[i].text, &tw, 0 );
		if ( tw > twmax ) twmax = tw;
	}
	
	float cursize = width - twmax;
	float textx = x+cursize+0.1;
	
	float sec_height = th*3; 
	dv.Color( *wxBLACK );
	float linewidth = (float)0.015;
	dv.LineStyle( linewidth, wxPageOutputDevice::SOLID );
	for( size_t i=0;i<m_list.size();i++ )
	{
		ld_item &li = m_list[i];

		if ( li.baseline )
		{
			if ( i > 0 ) // close up previous section and move down
			{
				dv.Line( x, y, x+cursize, y ); // section top line
				y += th; // spacing between sections
			}

			dv.Line( x, y, x+cursize, y ); // section top line
			dv.Line( x, y, x, y + sec_height ); // left vertical line
			dv.Line( x+cursize, y, x+cursize, y+sec_height ); // right vertical line
			
			textx = x + cursize + 0.2; // realign text
			
			dv.Font( face, points+1, true, false );
			dv.Text( x+0.1f, y+0.05f, li.text );
			dv.Text( x+0.1f, y+0.05f+th*1.2f, wxNumericFormat( li.value, wxNUMERIC_REAL, 0, true, wxEmptyString, wxEmptyString ) );
			dv.Font( face, points, false, false );
		}
		else
		{
			float lw = 0.0;
			dv.Line( x, y, x, y+sec_height ); // left vertical line

			// text point
			float tpx = textx-0.05;
			float tpy = y+sec_height/2;

			if( li.value >= 0 ) // loss
			{
				lw = li.value / 100.0f*cursize; // width of section that is lost
				dv.Arc( x + cursize, y-sec_height/2, 2*(tpx-(x+cursize)), sec_height, 180, 270 );
				dv.Arc( x + cursize-lw, y-sec_height/2, 2*(tpx-(x+cursize-lw)), sec_height, 180, 270);
				dv.Line( x+cursize-lw, y, x+cursize-lw, y+sec_height ); // vertical line down on right
			}
			else // gain
			{
				lw = -li.value / 100.0f*cursize; // width of section that is lost
				dv.Arc( x + cursize, y+sec_height/2, 2*(tpx-(x+cursize-lw)), sec_height/2, 90, 180 );
				dv.Arc( x + cursize+lw, y+sec_height/2, 2*(tpx-(x+cursize)), sec_height/2, 90, 180);
				dv.Line( x+cursize, y, x+cursize,y+0.75f*sec_height ); 
				dv.Line( x+cursize+lw, y+0.75f*sec_height, x+cursize+lw, y+sec_height); // vertical line down on right
				textx += lw;
			}
			
			dv.Text( textx, tpy-th, li.text );
			int ilv = li.value * 1000;
			double lv = ilv / 1000.0;
//			dv.Text(textx, tpy + 0.1f*th, wxString::Format("-%g %%", li.value));
			if (li.value >= 0)
			{
				dv.Text(textx, tpy + 0.1f*th, wxString::Format("-%g %%", lv));
				cursize -= lw;
			}
			else
			{
				dv.Text(textx, tpy + 0.1f*th, wxString::Format("%g %%", 0.0 - lv));
				cursize += lw;
			}
		}
		
		y += sec_height;
	}

	// close up with bottom point
	dv.Line( x, y, x+cursize/2, y+th );
	dv.Line( x+cursize/2, y+th, x+cursize, y );
}

bool LossDiagramObject::ReadData( wxInputStream & )
{
	return true;
}

bool LossDiagramObject::WriteData( wxOutputStream & )
{
	return true;
}

void LossDiagramObject::Configure( bool from_case, bool scale )
{
	m_createFromCase = from_case;
	m_scaleToGeometry = scale;
}


LossDiagCallbackContext::LossDiagCallbackContext( Case *c, Simulation *sim, LossDiagramObject *ld, const wxString &desc )
	: CaseCallbackContext( c, desc ), m_lossDiag( ld ), m_sim (sim) { }

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

	float ppix = sz.x / (mm.x/25.4);
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
    
    m_lossDiagram.SetGeometry( 0, 0, width/m_ppi, height/m_ppi );
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
	wxScreenOutputDevice dv( const_cast<LossDiagramCtrl*>( this ), dc );
	dv.Font( wxScreenOutputDevice::SANSERIF, 10, false, false );
	float tw, th;
	dv.Measure( "hy", &tw, &th );
	wxRealPoint pt = m_lossDiagram.EstimateSize( th );
	//wxLogStatus("DoGetBestSize:  th=%f inches estsize(inches)=(%lg,%lg)  m_ppi=%f", th, pt.x, pt.y, m_ppi);
    wxSize sz = dc.GetPPI(); // handles scaling
    return wxSize( (int)(pt.x*sz.x), (int)(pt.y*sz.y) );
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
	wxFrame *frame = new wxFrame( 0, wxID_ANY, "Loss Diagram Test", wxDefaultPosition, wxScaleSize(100,300) );
	LossDiagramCtrl *ldc = new LossDiagramCtrl( frame );

	LossDiagramObject &ld = ldc->GetDiagram();
	ld.Configure( false, false );

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
	ld.AddLossTerm( -4.2, "Performance adjustment" );
	ld.NewBaseline( 6777, "Energy to grid" );

	
	wxPdfDocument pdf( wxPORTRAIT, wxT("in"), wxPAPER_LETTER );	
	pdf.AddPage( wxPORTRAIT, wxPAPER_LETTER );
	wxPdfOutputDevice dv( pdf );
	ld.SetGeometry( 0.5, 0.5, 7.5, 10 );
	ld.Render( dv );	

	wxString pdf_file( "C:/Users/adobos/desktop/loss_diagram.pdf" );
	const wxMemoryOutputStream &data = pdf.CloseAndGetBuffer();
	wxFileOutputStream fp( pdf_file );
	if (fp.IsOk()) 
	{	
		wxMemoryInputStream tmpis( data );
		fp.Write( tmpis );
		if ( fp.Close() )
			wxLaunchDefaultBrowser( pdf_file );
	}
	else
		wxMessageBox("Failed to write PDF output version.");
	
	
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( ldc, 1, wxALL|wxEXPAND, 0 );
	frame->SetSizerAndFit( sizer );
	frame->Show();
}
