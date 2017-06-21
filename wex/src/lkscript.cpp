#include <wx/app.h>
#include <wx/thread.h>
#include <wx/html/htmlwin.h>
#include <wx/htmllbox.h>
#include <wx/fontenum.h>
#include <wx/progdlg.h>
#include <wx/splitter.h>
#include <wx/busyinfo.h>
#include <wx/app.h>
#include <wx/tokenzr.h>
#include <wx/simplebook.h>

#include <memory>

#include "wex/lkscript.h" // defines LK_USE_WXWIDGETS
#include "wex/mtrand.h"
#include "wex/utils.h"
#include "wex/csv.h"
#include "wex/metro.h"
#include "wex/easycurl.h"

#include <lk/lex.h>
#include <lk/absyn.h>
#include <lk/eval.h>
#include <lk/stdlib.h>
#include <lk/invoke.h>
#include <lk/vm.h>
#include <lk/codegen.h>
#include <lk/env.h>
#include <lk/parse.h>


#include "wex/plot/plplotctrl.h"
#include "wex/plot/plbarplot.h"
#include "wex/plot/pllineplot.h"
#include "wex/plot/plscatterplot.h"
#include "wex/plot/plwindrose.h"
#include "wex/plot/plcontourplot.h"
#include "wex/plot/plcolourmap.h"
#include "wex/plot/plsectorplot.h"

enum { BAR, HBAR, LINE, SCATTER, WINDROSE };
static void CreatePlot( wxPLPlotCtrl *plot, double *x, double *y, int len, double thick, wxColour &col, int type,
	const wxString &xlab, const wxString &ylab, const wxString &series,
	int xap, int yap, double baseline, const wxString &stackon, const wxString &lnsty, const wxString &marker )
{
	if (len <= 0 ) return;
		
	std::vector<wxRealPoint> data;
	data.reserve( len );
	for (int i=0;i<len;i++)
		data.push_back( wxRealPoint( x[i], y[i] ) );

	wxPLPlottable *p = 0;
	wxPLLinePlot::Style sty = wxPLLinePlot::SOLID;
	wxPLLinePlot::Marker mkr = wxPLLinePlot::NO_MARKER;

	switch (type )
	{
	case BAR:
	{
		wxPLBarPlot *bar = new wxPLBarPlot( data, baseline, series, col );

		if ( !stackon.IsEmpty() )
			if ( wxPLBarPlot *pp = dynamic_cast<wxPLBarPlot*>( plot->GetPlotByLabel( stackon ) ) )
				bar->SetStackedOn( pp );
		
		bar->SetThickness( thick );
		p = bar;
	}
		break;
	case HBAR:
	{
		wxPLHBarPlot *hbar = new wxPLHBarPlot( data, baseline, series, col );
		
		if ( !stackon.IsEmpty() )
			if ( wxPLHBarPlot *pp = dynamic_cast<wxPLHBarPlot*>( plot->GetPlotByLabel( stackon ) ) )
				hbar->SetStackedOn( pp );
		
		hbar->SetThickness( thick );
		p = hbar;
	}
		break;
	case LINE:
		if ( lnsty == "dotted" || lnsty == "dot" ) sty = wxPLLinePlot::DOTTED;
		else if ( lnsty == "dashed" || lnsty == "dash" ) sty = wxPLLinePlot::DASHED;
		else if ( lnsty == "none" || lnsty == "blank" ) sty = wxPLLinePlot::NO_LINE;
		
		if ( marker == "circle" ) mkr = wxPLLinePlot::CIRCLE;
		else if ( marker == "square" ) mkr = wxPLLinePlot::SQUARE;
		else if ( marker == "diamond" ) mkr = wxPLLinePlot::DIAMOND;
		else if ( marker == "hourglass" ) mkr = wxPLLinePlot::HOURGLASS;

		p = new wxPLLinePlot( data, series, col, sty, thick, mkr );
		break;
	case SCATTER:
		p = new wxPLScatterPlot( data, series, col, thick, false );
		break;
	case WINDROSE:
		p = new wxPLWindRose(data, series, col);
		break;
	}

	if (!p)
		return;

	p->SetXDataLabel( xlab );
	p->SetYDataLabel( ylab );
	plot->AddPlot( p, (wxPLPlotCtrl::AxisPos)xap, (wxPLPlotCtrl::AxisPos) yap );
	plot->Invalidate();
	plot->Refresh();
}


class PlotWin;

static wxWindow *s_curToplevelParent = 0;
static int _iplot = 1;
static PlotWin *s_curPlotWin = 0;
static wxPLPlotCtrl *s_curPlot = 0;

static wxWindow *GetCurrentTopLevelWindow()
{
	wxWindowList &wl = ::wxTopLevelWindows;
	for( wxWindowList::iterator it = wl.begin(); it != wl.end(); ++it )
		if ( wxTopLevelWindow *tlw = dynamic_cast<wxTopLevelWindow*>( *it ) )
			if ( tlw->IsShown() && tlw->IsActive() )
				return tlw;

	return 0;
}

class PlotWin : public wxFrame
{
public:
	wxPLPlotCtrl *m_plot;

	PlotWin( wxWindow *parent )
		: wxFrame( parent, wxID_ANY, 
			wxString::Format("plot %d", _iplot++),
			wxDefaultPosition, wxScaleSize(wxSize(500,400)),
			wxCLOSE_BOX|wxCLIP_CHILDREN|wxCAPTION|wxRESIZE_BORDER )
	{
		m_plot = new wxPLPlotCtrl( this, wxID_ANY );
		m_plot->SetHighlightMode( wxPLPlotCtrl::HIGHLIGHT_ZOOM );
		m_plot->SetBackgroundColour(*wxWHITE);
		Show();
	}

	virtual ~PlotWin()
	{
		if ( s_curPlot == m_plot )
			s_curPlot = 0;

		if ( s_curPlotWin == this )
			s_curPlotWin = 0;

	}
};

static void ClearPlotSurface()
{
	s_curPlot = 0;
	s_curPlotWin = 0;
}

static wxPLPlotCtrl *GetPlotSurface( wxWindow *parent )
{
	// somebody externally defined the plot target?
	if ( s_curPlot != 0 ) 
		return s_curPlot;

	// there is current window
	if ( s_curPlotWin != 0 ) {
		s_curPlot = s_curPlotWin->m_plot;
		return s_curPlot;
	}

	// create a new window
	s_curPlotWin = new PlotWin( parent );
	s_curPlot = s_curPlotWin->m_plot;
	return s_curPlot;
}

void wxLKSetToplevelParentForPlots( wxWindow *parent )
{
	s_curToplevelParent = parent;
}

void wxLKSetPlotTarget( wxPLPlotCtrl *plot )
{
	s_curPlot = plot;
}

wxPLPlotCtrl *wxLKGetPlotTarget()
{
	return s_curPlot;
}

void fcall_newplot( lk::invoke_t &cxt )
{
	LK_DOC("newplot", "Switches to a new plotting window on the next call to plot.", "([boolean:remove all]):none");
	ClearPlotSurface();

	if ( cxt.arg_count() == 1 && cxt.arg(0).as_boolean() )
	{
		wxWindowList wl = ::wxTopLevelWindows;
		for (size_t i=0;i<wl.size();i++)
			if ( PlotWin *p = dynamic_cast<PlotWin*>( wl[i] ))
				p->Destroy();
	}
}

static wxColour lk_to_colour( lk::vardata_t *arg )
{
	if ( arg->type() == lk::vardata_t::VECTOR
		&& arg->length() == 3 )
	{
		int r = arg->index(0)->as_integer();
		int g = arg->index(1)->as_integer();
		int b = arg->index(2)->as_integer();

		return wxColour(r,g,b);
	}
	else
		return wxColour( wxString(arg->as_string().c_str()));
}

void fcall_plot( lk::invoke_t &cxt )
{
	LK_DOC("plot", "Creates an XY line, bar, horizontal bar, or scatter plot. Options include thick/size, type, color, xap, yap, xlabel, ylabel, series, baseline, stackon, style.", "(array:x, array:y, table:options):void");
	
	wxPLPlotCtrl *plot = GetPlotSurface( 
		(s_curToplevelParent!=0)
			? s_curToplevelParent 
			: GetCurrentTopLevelWindow() );

	lk::vardata_t &a0 = cxt.arg(0).deref();
	lk::vardata_t &a1 = cxt.arg(1).deref();

	if ( a0.type() == lk::vardata_t::VECTOR
		&& a1.type() == lk::vardata_t::VECTOR
		&& a0.length() == a1.length()
		&& a0.length() > 0 )
	{
		double thick = 1;
		double baseline = 0.0; // used for bar plots
		int type = LINE;
		wxColour col = *wxBLUE;
		wxString xlab, ylab;
		wxString lnsty("solid");
		wxString marker("none");
		wxString series = wxEmptyString;
		int xap = wxPLPlotCtrl::X_BOTTOM;
		int yap = wxPLPlotCtrl::Y_LEFT;
		wxString stackon;

		if (cxt.arg_count() > 2 && cxt.arg(2).deref().type() == lk::vardata_t::HASH )
		{
			lk::vardata_t &t = cxt.arg(2).deref();
			if ( lk::vardata_t *arg = t.lookup("thick") )
				thick = arg->as_number();
			if ( lk::vardata_t *arg = t.lookup("size") )
				thick = arg->as_number();

			if ( lk::vardata_t *arg = t.lookup("type") )
			{
				wxString stype = arg->as_string().c_str();
				stype.Lower();
				if (stype == "bar") type = BAR;
				if (stype == "hbar") type = HBAR;
				else if (stype == "scatter") type = SCATTER;
				else if (stype == "windrose") type = WINDROSE;
			}

			if ( lk::vardata_t *arg = t.lookup("baseline") )
				baseline = arg->as_number();

			if ( lk::vardata_t *arg = t.lookup("stackon") )
				stackon = arg->as_string();
			
			if (lk::vardata_t *arg = t.lookup("color") )
			{
				col = lk_to_colour( arg );
			}

			if (lk::vardata_t *arg = t.lookup("xap"))
			{
				if (arg->as_string() == "top") xap = wxPLPlotCtrl::X_TOP;
			}

			if ( lk::vardata_t *arg = t.lookup("yap"))
			{
				if (arg->as_string() == "right") yap = wxPLPlotCtrl::Y_RIGHT;
			}

			if ( lk::vardata_t *arg = t.lookup("series"))
				series = arg->as_string();

			if ( lk::vardata_t *arg = t.lookup("xlabel"))
				xlab = arg->as_string();

			if ( lk::vardata_t *arg = t.lookup("ylabel"))
				ylab = arg->as_string();

			if ( lk::vardata_t *arg = t.lookup("style"))
				lnsty = arg->as_string().Lower();

			if ( lk::vardata_t *arg = t.lookup("marker"))
				marker = arg->as_string().Lower();
		}
		
		int len = cxt.arg(0).length();
		double *x = new double[len];
		double *y = new double[len];

		for (int i=0;i<len;i++)
		{
			x[i] = a0.index(i)->as_number();
			y[i] = a1.index(i)->as_number();
		}

		CreatePlot( plot, x, y, len, thick, col, type, xlab, ylab, series, xap, yap, baseline, stackon, lnsty, marker );

		delete [] x;
		delete [] y;
	}
}

void fcall_annotate( lk::invoke_t &cxt )
{
	LK_DOC( "annotate", "Adds an annotation (text, line, brace, circle, rectangle) on a plot surface. Options: type{line,brace,rect,circle}, color, size, align{left,center,right}, angle, style{solid,dot,dash,dotdash}, arrow{outline,filled}, position{axis,fractional,points}, zorder{front,back}, dxdy=[dx,dy], filled=t/f.", "(string or [x0 y0], [x y] or radius, table:options):none" );

	wxPLPlotCtrl *plot = GetPlotSurface( 
		(s_curToplevelParent!=0)
			? s_curToplevelParent 
			: GetCurrentTopLevelWindow() );

	wxRealPoint pos;
	double radius = 1;

	if ( cxt.arg(1).type() == lk::vardata_t::NUMBER )
		radius = cxt.arg(1).as_number(); // for circle shapes
	else
		pos = wxRealPoint(cxt.arg(1).index(0)->as_number(),
			cxt.arg(1).index(1)->as_number() );

	double size = 0;
	double angle = 0;
	wxColour color( *wxBLACK );
	wxPLTextLayout::TextAlignment align = wxPLTextLayout::LEFT;
	wxPLOutputDevice::Style style = wxPLOutputDevice::SOLID;
	wxPLAnnotation::PositionMode posm = wxPLAnnotation::AXIS;
	wxPLAnnotation::ZOrder zorder = wxPLAnnotation::FRONT;
	wxPLLineAnnotation::ArrowType arrow = wxPLLineAnnotation::NO_ARROW;
	wxPLPlot::AxisPos xap( wxPLPlot::X_BOTTOM );
	wxPLPlot::AxisPos yap( wxPLPlot::Y_LEFT );
	wxPLPlot::PlotPos ppos( wxPLPlot::PLOT_TOP );

	wxRealPoint dxdy(0,0);

	bool filled = true;
	bool brace = false;
	bool rect = false;
	bool circle = false;

	if ( cxt.arg_count() > 2 )
	{
		lk::vardata_t &opts = cxt.arg(2).deref();
		if ( lk::vardata_t *o = opts.lookup( "color" ) )
		{
			color = lk_to_colour( o );
		}
		
		if ( lk::vardata_t *o = opts.lookup( "filled" ) )
		{
			filled = o->as_boolean();
		}
		
		if ( lk::vardata_t *o = opts.lookup( "type" ) )
		{
			if( o->as_string().Lower() == "brace" )
				brace = true;
			if( o->as_string().Lower() == "rect" )
				rect = true;
			if( o->as_string().Lower() == "circle" )
				circle = true;
		}

		if ( lk::vardata_t *o = opts.lookup( "size" ) )
		{
			size = o->as_number();
		}

		if ( lk::vardata_t *o = opts.lookup( "arrow" ) )
		{
			if ( o->type() == lk::vardata_t::STRING )
			{
				wxString sarr( o->as_string().Lower() );
				if ( sarr == "filled" ) arrow = wxPLLineAnnotation::FILLED_ARROW;
				else if ( sarr == "outline" ) arrow = wxPLLineAnnotation::OUTLINE_ARROW;
			}
			else
				arrow = o->as_boolean() ? wxPLLineAnnotation::FILLED_ARROW : wxPLLineAnnotation::NO_ARROW;
		}

		if ( lk::vardata_t *o = opts.lookup( "align" ) )
		{
			wxString A( o->as_string().Lower() );
			if ( A == "center" ) align = wxPLTextLayout::CENTER;
			else if ( A == "right" ) align = wxPLTextLayout::RIGHT;
		}

		if ( lk::vardata_t *o = opts.lookup( "angle" ) )
		{
			angle = o->as_number();
		}

		if ( lk::vardata_t *o = opts.lookup( "style" ) )
		{
			wxString A( o->as_string().Lower() );
			if ( A == "dot" || A == "dotted" ) style = wxPLOutputDevice::DOT;
			else if ( A == "dash" || A == "dashed" ) style = wxPLOutputDevice::DASH;
			else if ( A == "dotdash" ) style = wxPLOutputDevice::DOTDASH;
		}

		if ( lk::vardata_t *o = opts.lookup( "position" ) )
		{
			wxString A( o->as_string().Lower() );
			if ( A == "fractional" ) posm = wxPLAnnotation::FRACTIONAL;
			if ( A == "points" ) posm = wxPLAnnotation::POINTS;
		}

		if ( lk::vardata_t *o = opts.lookup( "zorder" ) )
		{
			if ( o->as_string().Lower() == "back" )
				zorder = wxPLAnnotation::BACK;
		}

		if ( lk::vardata_t *o = opts.lookup( "dxdy" ) )
		{
			dxdy.x = o->index(0)->as_number();
			dxdy.y = o->index(1)->as_number();
		}

	}
	

	if ( cxt.arg(0).deref().type() == lk::vardata_t::STRING )
	{
		plot->AddAnnotation( new wxPLTextAnnotation( cxt.arg(0).as_string(),
			pos, size, angle, color, align, dxdy ), posm, xap, yap, ppos, zorder );
	}
	else if ( cxt.arg(0).deref().type() == lk::vardata_t::VECTOR )
	{
		wxRealPoint p0( cxt.arg(0).index(0)->as_number(),
			cxt.arg(0).index(1)->as_number() );

		std::vector<wxRealPoint> pts;
		pts.push_back( p0 );
		pts.push_back( pos );
		if ( size <= 0 ) size = 1;
		if ( brace ) 
		{
			plot->AddAnnotation( new wxPLBraceAnnotation( pos, p0, 1.0, size, color, style ), posm, xap, yap, ppos, zorder );
		}
		else if ( rect )
		{
			wxPLRealRect rr(  p0.x, p0.y, pos.x, pos.y );
			plot->AddAnnotation( new wxPLShapeAnnotation( wxPLShapeAnnotation::RECTANGLE, rr, color, filled, size ), posm, xap, yap, ppos, zorder );
		}
		else if ( circle )
		{
			wxPLRealRect rr( p0.x, p0.y, radius, radius );
			plot->AddAnnotation( new wxPLShapeAnnotation( wxPLShapeAnnotation::CIRCLE, rr, color, filled, size ), posm, xap, yap, ppos, zorder );
		}
		else
		{
			plot->AddAnnotation( new wxPLLineAnnotation( pts, size, color, style, arrow ), posm, xap, yap, ppos, zorder );
		}
	}
	
	plot->Refresh();

}

void fcall_plotopt( lk::invoke_t &cxt )
{
	LK_DOC("plotopt", 
		"Modifies the current plot properties like title, coarse, fine, legend, legendpos, legendborder, scale, font, window, border, space, showaxes, pdffontface, pdffontsize, pdffontdir", 
		"(table:options):boolean");
	
	cxt.result().assign( 1.0 );
	
	if ( lk::vardata_t *arg = cxt.arg(0).lookup("pdffontface") )
		if ( !wxPLPlot::SetPdfDefaultFont( arg->as_string() ) )
			cxt.result().assign( 0.0 );
	
	if ( lk::vardata_t *arg = cxt.arg(0).lookup("pdffontdir") )
		if ( !wxPLPlot::AddPdfFontDir( arg->as_string() ) )
			cxt.result().assign( 0.0 );

	wxPLPlotCtrl *plot = s_curPlot;
	if (!plot) {
		return;
	}

	cxt.result().assign( 1.0 ); // by default return true

	bool mod = false;
	if ( lk::vardata_t *arg = cxt.arg(0).lookup("title") )
	{
		plot->SetTitle( arg->as_string() );
		mod = true;
	}
	
	if ( lk::vardata_t *arg = cxt.arg(0).lookup("coarse") )
	{
		plot->ShowCoarseGrid( arg->as_boolean() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("fine") )
	{
		plot->ShowFineGrid( arg->as_boolean() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("legend") )
	{
		plot->ShowLegend( arg->as_boolean() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("border") )
	{
		plot->SetBorderWidth( arg->as_number() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("showaxes") )
	{
		plot->ShowAxes( arg->as_boolean() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("legendpos") )
	{
		double xper = 90, yper = 10;
		if (arg->type() == lk::vardata_t::VECTOR && arg->length() == 2 )
		{
			xper = arg->index(0)->as_number();
			yper = arg->index(1)->as_number();
			plot->SetLegendLocation( wxPLPlotCtrl::FLOATING, xper, yper);
			mod = true;
		}
		else if ( arg->type() == lk::vardata_t::STRING )
		{
			mod = plot->SetLegendLocation( arg->as_string() );
		}
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("legendborder") )
	{
		plot->ShowLegendBorder( arg->as_boolean() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("window") )
	{
		if ( s_curPlotWin
			&& arg->type() == lk::vardata_t::VECTOR )
		{
			int x, y, w, h;
			x=y=w=h=-1;
			
			if ( arg->length() == 4 )
			{
				x = arg->index(0)->as_integer();
				y = arg->index(1)->as_integer();
				w = arg->index(2)->as_integer();
				h = arg->index(3)->as_integer();
			}
			else if ( arg->length() == 2 )
			{
				w = arg->index(0)->as_integer();
				h = arg->index(1)->as_integer();
			}

			
			if ( x >= 0 && y >= 0 )
				s_curPlotWin->SetPosition( wxPoint(x, y) );

			// coordinates for plot window size always in DIPs
			if ( w > 0 && h > 0 )
				s_curPlotWin->SetClientSize( wxScaleSize(w,h) );
		}
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("scale") )
	{
		double scale = arg->as_number();
		if ( scale > 0.2 && scale < 5 )
		{
			wxFont font( *wxNORMAL_FONT );
			double point = font.GetPointSize();
			point *= scale;
			font.SetPointSize( (int)point );
			plot->SetFont( font );
			mod = true;
		}
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("font") )
	{
		wxString face( arg->as_string() );
		if ( wxFontEnumerator::IsValidFacename(face) )
		{
			wxFont font( plot->GetFont() );
			font.SetFaceName( face );
			plot->SetFont( font );
			mod = true;
		}
	}

	if ( lk::vardata_t *arg = cxt.arg(0).lookup("space") )
	{
		wxRealPoint tl(0,0), br(0,0);
		tl.x = arg->index(0)->as_number();
		br.x = arg->index(1)->as_number();
		if ( arg->length() > 2 )
		{
			tl.y = arg->index(2)->as_number();
			br.y = arg->index(3)->as_number();
		}
		plot->SetBorderSpace( tl.x, br.x, tl.y, br.y );
		mod = true;
	}
	
	if (mod)
	{
		plot->Invalidate();
		plot->Refresh();
	}
}

void fcall_plotout( lk::invoke_t &cxt )
{
	LK_DOC( "plotout", "Output the current plot to a file. Valid formats are png, bmp, jpg, pdf.", "(string:file name):boolean" );
	wxPLPlotCtrl *plot = s_curPlot;
	if (!plot) return;
	
	wxString file( cxt.arg(0).as_string() );
	wxFileName fn(file);
	wxString format( fn.GetExt().Lower() );
	
	bool ok = false;
	if ( format == "pdf" ) 
		ok = plot->ExportPdf( file );
	else if ( format == "bmp" )
		ok = plot->Export( file, wxBITMAP_TYPE_BMP );
	else if ( format == "jpg" )
		ok = plot->Export( file, wxBITMAP_TYPE_JPEG );
	else if ( format == "png" )
		ok = plot->Export( file, wxBITMAP_TYPE_PNG );

	cxt.result().assign( ok );
}

void fcall_axis( lk::invoke_t &cxt )
{
	LK_DOC("axis", "Modifies axis properties (type, label, labels[2D array for 'label' axis type], show, showlabel, min, max, ticklabels, ticksizes=[large,small]) on the current plot.", "(string:axis name 'x1' 'y1' 'x2' 'y2', table:options):void");
	lk_string axname = cxt.arg(0).as_string();
	wxPLPlotCtrl *plot = s_curPlot;
	if (!plot) return;
	wxPLAxis *axis = 0;
	if (axname == "x1") axis = plot->GetXAxis1();
	if (axname == "x2") axis = plot->GetXAxis2();
	if (axname == "y1") axis = plot->GetYAxis1();
	if (axname == "y2") axis = plot->GetYAxis2();


	if ( axis == NULL )
	{
		axis = new wxPLLinearAxis( 0, 1 );
		if (axname == "x1") plot->SetXAxis1( axis );
		if (axname == "x2") plot->SetXAxis2( axis );
		if (axname == "y1") plot->SetYAxis1( axis );
		if (axname == "y2") plot->SetYAxis2( axis );
	}

	if (cxt.arg_count() < 2 || cxt.arg(1).type() != lk::vardata_t::HASH ) return;
	bool mod = false;

	
	if ( lk::vardata_t *arg = cxt.arg(1).lookup("type") )
	{
		double min, max;
		axis->GetWorld(&min, &max);

		wxPLAxis *axis_new = 0;

		if ( arg->as_string() == "log" )
		{
			if ( min <= 0 ) min = 0.00001;
			if ( max < min ) max = min+10;

			axis_new = new wxPLLogAxis( min, max, axis->GetLabel() );
		}
		else if ( arg->as_string() == "linear" )
		{
			axis_new = new wxPLLinearAxis( min, max, axis->GetLabel() );
		}
		else if ( arg->as_string() == "time" )
		{
			axis_new = new wxPLTimeAxis( min, max, axis->GetLabel() );
		}
		else if ( arg->as_string() == "label" )
		{
			if (lk::vardata_t *_tx = cxt.arg(1).lookup("labels"))
			{
				lk::vardata_t &tx = _tx->deref();
				if ( tx.type() == lk::vardata_t::VECTOR )
				{
					wxPLLabelAxis *axl = new wxPLLabelAxis( min, max, axis->GetLabel() );					
					for( size_t i=0;i<tx.length();i++ )
					{
						lk::vardata_t &item = tx.index(i)->deref();
						if ( item.type() == lk::vardata_t::VECTOR && item.length() == 2 )
							axl->Add( item.index(0)->as_number(), item.index(1)->as_string() );
					}

					axis_new = axl;
				}
			}

		}

		if ( axis_new != 0 )
		{
			axis_new->ShowTickText( axis->IsTickTextVisible() );
			axis_new->ShowLabel( axis->IsLabelVisible() );
			mod = true;
			axis = axis_new;
			if (axname == "x1") plot->SetXAxis1( axis_new );
			if (axname == "x2") plot->SetXAxis2( axis_new );
			if (axname == "y1") plot->SetYAxis1( axis_new );
			if (axname == "y2") plot->SetYAxis2( axis_new );
		}
	}

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("label") )
	{
		axis->SetLabel( arg->as_string() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("show") )
	{
		axis->Show( arg->as_boolean() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("min") )
	{
		double min, max;
		axis->GetWorld(&min, &max);
		min = arg->as_number();
		axis->SetWorld(min, max);
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("max") )
	{
		double min, max;
		axis->GetWorld(&min,&max);
		max = arg->as_number();
		axis->SetWorld(min,max);
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("ticklabels") )
	{
		axis->ShowTickText( arg->as_boolean() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("showlabel") )
	{
		axis->ShowLabel( arg->as_boolean() );
		mod = true;
	}

	if ( lk::vardata_t *arg = cxt.arg(1).lookup("ticksizes") )
	{
		axis->SetTickSizes( arg->index(0)->as_number(), arg->index(1)->as_number() );
		mod = true;
	}
	
	if (mod) {
		plot->Invalidate();
		plot->Refresh();
	}
}

static void to_stdvec( lk::vardata_t &v, std::vector<double> &vec )
{
	if ( v.type() != lk::vardata_t::VECTOR ) return;
	size_t len = v.length();
	if ( len == 0 ) { vec.clear(); return; }
	vec.resize( len );
	for( size_t i=0;i<len;i++ )
		vec[i] = v.index(i)->as_number();
}

static void to_matrix( lk::vardata_t &v, wxMatrix<double> &mat )
{
	mat.Clear();
	if ( v.type() != lk::vardata_t::VECTOR ) return;

	size_t rows = v.length();
	size_t cols = 0;
	for( size_t i=0;i<rows;i++ )
	{
		if ( v.index(i)->deref().type() == lk::vardata_t::VECTOR )
		{
			size_t len = v.index(i)->deref().length();
			if ( len > cols ) cols = len;
		}
	}

	if ( rows == 0 || cols == 0 ) return;

	mat.ResizeFill( rows, cols, std::numeric_limits<double>::quiet_NaN() );
	for( size_t r=0;r<rows;r++ )
	{
		for( size_t c=0;c<cols;c++ )
		{
			if ( v.index(r)->deref().type() == lk::vardata_t::VECTOR 
				&& c < v.index(r)->deref().length() )
			{
				mat(r,c) = v.index(r)->deref().index(c)->as_number();
			}
		}
	}
}

static void to_lk_matrix( wxMatrix<double> &mat, lk::vardata_t &v )
{
	v.empty_vector();
	if ( mat.Cells() == 0 ) return;

	v.resize( mat.Rows() );
	for( size_t r=0;r<mat.Rows();r++ )
	{
		v.index(r)->empty_vector();
		v.index(r)->resize( mat.Cols() );
		for( size_t c=0;c<mat.Cols();c++ )
			v.index(r)->index(c)->assign( mat(r,c) );
	}
}


void fcall_peaks( lk::invoke_t &cxt )
{
	LK_DOC( "peaks", "Sample data for contour plotting.  Returns a table with x, y, z matrices for use with contour(...).", "( number: #points ):table" );
	
	wxMatrix<double> x, y, z;
	double min, max;
	wxPLContourPlot::Peaks( cxt.arg(0).as_unsigned(), x, y, z, &min, &max );
	cxt.result().empty_hash();
	to_lk_matrix( x, cxt.result().hash_item( "x" ) );
	to_lk_matrix( y, cxt.result().hash_item( "y" ) );
	to_lk_matrix( z, cxt.result().hash_item( "z" ) );
	cxt.result().hash_item( "min" ).assign( min );
	cxt.result().hash_item( "max" ).assign( max );
}

void fcall_meshgrid( lk::invoke_t &cxt )
{
	//	G = meshgrid( xmin, xmax, np, ymin+y_offset, ymax+y_offset, np );
	LK_DOC( "meshgrid", "Creates a regularly spaced grid of x, y coordinates. Returns a table with two matrices, 'xq' and 'yq', for use with griddata(...).", "( number:xmin, number:xmax, number: #points, number:ymin, number:ymax, number: #points ):table" );
	wxMatrix<double> xq, yq;
	wxPLContourPlot::MeshGrid( 

		cxt.arg(0).as_number(),
		cxt.arg(1).as_number(),
		cxt.arg(2).as_unsigned(),

		cxt.arg(3).as_number(),
		cxt.arg(4).as_number(),
		cxt.arg(5).as_unsigned(),

		xq, yq );

	cxt.result().empty_hash();
	to_lk_matrix( xq, cxt.result().hash_item( "xq" ) );
	to_lk_matrix( yq, cxt.result().hash_item( "yq" ) );
}

void fcall_griddata( lk::invoke_t &cxt )
{
	LK_DOC( "griddata", "Interpolates X,Y,Z data onto a regular xq, yq mesh.  Note: use meshgrid(...) to generate the xq and yq matrices.", "( array:x, array:y, array:z, matrix:xq, matrix:yq ):matrix" );

	std::vector<double> x, y, z;
	to_stdvec( cxt.arg(0).deref(), x );
	to_stdvec( cxt.arg(1).deref(), y );
	to_stdvec( cxt.arg(2).deref(), z );

	wxMatrix<double> xq, yq, zq;
	to_matrix( cxt.arg(3).deref(), xq );
	to_matrix( cxt.arg(4).deref(), yq );

	wxPLContourPlot::GridData( x, y, z, xq, yq, zq );

	to_lk_matrix( zq, cxt.result() );
}

void fcall_sector( lk::invoke_t &cxt )
{
	LK_DOC( "sector", "Creates a sector plot from labeled data, with an optional inner section.  Options are: colors{array}, showvalues, calloutsize, holesize, textspace, border.", "(array:[[ value, label ]...], {array:[[ value, label ]...]}, table:options):none" );
	
	wxPLPlotCtrl *plot = GetPlotSurface( 
		(s_curToplevelParent!=0)
			? s_curToplevelParent 
			: GetCurrentTopLevelWindow() );

	wxPLSectorPlot *sector = new wxPLSectorPlot;
	for( size_t i=0;i<cxt.arg(0).length();i++ )
	{
		if ( cxt.arg(0).index(i)->length() < 2 ) break;
		sector->AddSector( cxt.arg(0).index(i)->index(0)->as_number(),
				cxt.arg(0).index(i)->index(1)->as_string() );
	}

	size_t optidx = 1;
	if ( cxt.arg_count() > 1 && cxt.arg(1).deref().type() == lk::vardata_t::VECTOR )
	{
		optidx = 2;
		for( size_t i=0;i<cxt.arg(1).length();i++ )
		{
			if ( cxt.arg(1).index(i)->length() < 2 ) break;
			sector->AddInnerSector( cxt.arg(1).index(i)->index(0)->as_number(),
					cxt.arg(1).index(i)->index(1)->as_string() );
		}
	}
	
	if ( optidx < cxt.arg_count() )
	{
		lk::vardata_t &opts = cxt.arg(optidx);
		if ( lk::vardata_t *o = opts.lookup( "showvalues" ) )
			sector->ShowSegmentValues( o->as_boolean() );
		if ( lk::vardata_t *o = opts.lookup( "holesize" ) )
			sector->SetCenterHoleSize( o->as_number() );
		if ( lk::vardata_t *o = opts.lookup( "calloutsize" ) )
			sector->SetCalloutSize( o->as_number() );
		if ( lk::vardata_t *o = opts.lookup( "textspace") ) 
			sector->SetTextSpace( o->as_number() );
		if ( lk::vardata_t *o = opts.lookup( "border" ) )
			sector->SetBorder( o->as_number() );
		if ( lk::vardata_t *o = opts.lookup( "colors" ) )
		{
			std::vector<wxColour> list;
			for( size_t i=0;i<o->length();i++ )
				list.push_back( lk_to_colour( o->index(i) ) );

			sector->SetColours( list );
		}

		wxString prefix, suffix;
		if ( lk::vardata_t *o = opts.lookup( "suffix" ) )
			suffix = o->as_string();
		if ( lk::vardata_t *o = opts.lookup( "prefix" ) )
			prefix = o->as_string();

		if ( !prefix.IsEmpty() || !suffix.IsEmpty() )
			sector->SetFormat( wxNUMERIC_REAL, wxNUMERIC_GENERIC, false, prefix, suffix );
	}

	plot->AddPlot( sector );
}


void fcall_contour( lk::invoke_t &cxt )
{
	LK_DOC( "contour", "Creates a contour plot from gridded x,y,z data. Options are filled, colormap=jet/parula/grayscale/rainbow, reversecolors=t/f, scalelabels=['',''...], levels, min, max, decimals", "( matrix:x, matrix:y, matrix:z, { table:options } ):none" );
	
	wxPLPlotCtrl *plot = GetPlotSurface( 
		(s_curToplevelParent!=0)
			? s_curToplevelParent 
			: GetCurrentTopLevelWindow() );
	
	wxString cmap_name;
	bool filled = false;
	int decimals = -1;
	size_t levels = 10;
	bool reversed = false;
	double min, max;
	min=max=std::numeric_limits<double>::quiet_NaN();
	wxString label;
	wxArrayString scalelabels;
	if ( cxt.arg_count() > 3 )
	{
		lk::vardata_t &opt = cxt.arg(3);
		if ( lk::vardata_t *o = opt.lookup( "colormap" ) )
			cmap_name = o->as_string().Lower();
		if ( lk::vardata_t *o = opt.lookup( "reversecolors"  ) )
			reversed = o->as_boolean();
		if ( lk::vardata_t *o = opt.lookup( "filled" ) )
			filled = o->as_boolean();
		if ( lk::vardata_t *o = opt.lookup( "levels" ) )
			levels = o->as_unsigned();
		if ( lk::vardata_t *o = opt.lookup( "min" ) )
			min = o->as_number();
		if ( lk::vardata_t *o = opt.lookup( "max" ) )
			max = o->as_number();
		if ( lk::vardata_t *o = opt.lookup( "label" ) )
			label = o->as_string();
		if ( lk::vardata_t *o = opt.lookup( "decimals" ) )
			decimals = o->as_integer();
		if ( lk::vardata_t *o = opt.lookup( "scalelabels" ) )
		{
			lk::vardata_t &oref = o->deref();
			if ( oref.type() == lk::vardata_t::VECTOR )
			{
				for( size_t i=0;i<oref.length();i++ )
					scalelabels.Add( o->index(i)->as_string() );
			}
		}
	}

	wxMatrix<double> x, y, z;
	to_matrix( cxt.arg(0), x );
	to_matrix( cxt.arg(1), y );
	to_matrix( cxt.arg(2), z );

	if ( !std::isfinite( min ) || !std::isfinite( max ) )
		wxPLContourPlot::MinMax( z, &min, &max );

	wxPLColourMap *cmap = dynamic_cast<wxPLColourMap*>( plot->GetSideWidget( wxPLPlot::Y_RIGHT ) );
	if ( 0 == cmap || !cmap_name.IsEmpty() )
	{
		if ( cmap_name=="grayscale" || cmap_name == "greyscale" 
			|| cmap_name == "grey" || cmap_name == "gray" ) 
			cmap = new wxPLGrayscaleColourMap( min, max );
		else if ( cmap_name == "parula" )
			cmap = new wxPLParulaColourMap( min, max );
		else if ( cmap_name == "rainbow" )
			cmap = new wxPLCoarseRainbowColourMap( min, max );
		else 
			cmap = new wxPLJetColourMap( min, max );

		
		plot->SetSideWidget( cmap, wxPLPlot::Y_RIGHT );

		for( size_t i=0;i<plot->GetPlotCount();i++ )
			if ( wxPLContourPlot *cp = dynamic_cast<wxPLContourPlot*>( plot->GetPlot(i) ) )
				cp->SetColourMap( cmap );
	}
	
	cmap->SetReversed( reversed );
	if (scalelabels.size() > 0 ) cmap->SetLabels( scalelabels );

	if ( min < cmap->GetScaleMin() ) cmap->SetScaleMin( min );
	if ( max > cmap->GetScaleMax() ) cmap->SetScaleMax( max );

	if ( decimals > 0 && decimals < 20 ) cmap->SetFormat( wxString::Format( "%%.%dlf", decimals ) );
	else cmap->SetFormat( "%lg" );



	wxPLContourPlot *contour = new wxPLContourPlot( x, y, z, filled, label, (int)levels, cmap );
	plot->AddPlot( contour );
}

#include <wx/anidecod.h>
#include <wx/imaggif.h>
#include <wx/file.h>
#include <wx/ffile.h>
#include <wx/wfstream.h>
#include <wx/quantize.h>

void fcall_gifanim( lk::invoke_t &cxt )
{
	LK_DOC( "gifanim", "Create a GIF animation.  Call with no arguments to save the current plot as a frame.  Call with a file name and delay time (ms) to write to a file.", "(string:file name[, number:delay] OR none):boolean" );

static wxImageArray s_frames;

	if ( cxt.arg_count() == 0 )
	{
		
		wxPLPlotCtrl *plot = GetPlotSurface( 
			(s_curToplevelParent!=0)
				? s_curToplevelParent 
				: GetCurrentTopLevelWindow() );
		if ( !plot ) return;

		wxImage img;
		cxt.result().assign( wxQuantize::Quantize( plot->GetBitmap().ConvertToImage(), img ) ? 1.0 : 0.0 );
		s_frames.push_back( img );
	}
	else
	{		
		cxt.result().assign( 0.0 );

		wxString file( cxt.arg(0).as_string() );
		int delay = 75;
		if ( cxt.arg_count() > 1 )
			delay = cxt.arg(1).as_integer();
		
		wxBusyInfo info("Writing gif animation: " + file );
		wxGIFHandler gif;
		wxFileOutputStream fs(file);
		if ( fs.IsOk() && gif.SaveAnimation( s_frames, &fs, true, delay ) )
			cxt.result().assign( 1.0 );

		s_frames.Clear();
	}
}


void fcall_csvconv( lk::invoke_t &cxt )
{
	LK_DOC( "csvconv", "Convert arrays to CSV text and vice-versa."
		" For string->array conversion, options are 'numeric'=t/F, '1darray'=t/F (for one line csv data)."
		" For array->string conversion, options are 'trim'=t/F' (remove last newline).",
		"( array, [table:options] ):string or ( string, [table:options] ):array" );

	bool as_numbers = false;
	bool as_1D = false;
	bool trim_nl = false;
	
	if ( cxt.arg_count() > 1 && cxt.arg(1).type() == lk::vardata_t::HASH )
	{
		lk::vardata_t &opt = cxt.arg(1);
		if ( lk::vardata_t *x = opt.lookup( "numeric" ) )
			as_numbers = x->as_boolean();

		if ( lk::vardata_t *x = opt.lookup( "1darray" ) )
			as_1D = x->as_boolean();
		
		if ( lk::vardata_t *x = opt.lookup( "trim" ) )
			trim_nl = x->as_boolean();
	}

	lk::vardata_t &data = cxt.arg(0);
	lk::vardata_t &R = cxt.result();
	R.nullify();
	
	if ( data.type() == lk::vardata_t::STRING )
	{
		wxCSVData csv;
		csv.ReadString( data.as_string() );
		
		R.empty_vector();
		if ( as_1D && csv.NumRows() == 1 && csv.NumCols() > 0 )
		{
			R.vec()->resize( csv.NumCols() );
			for( size_t i=0;i<csv.NumCols();i++ )
			{
				if ( as_numbers ) R.index(i)->assign( wxAtof( csv(0,i) ) );
				else R.index(i)->assign( csv(0,i) );
			}
		}
		else if ( csv.NumRows() > 0 && csv.NumCols() > 0 )
		{
			R.vec()->resize( csv.NumRows() );
			for( size_t i=0;i<csv.NumRows();i++ )
			{
				R.index(i)->empty_vector();
				R.index(i)->resize( csv.NumCols() );
				for( size_t j=0;j<csv.NumCols();j++ )
				{
					if ( as_numbers ) R.index(i)->index(j)->assign( wxAtof( csv(i,j) ) );
					else R.index(i)->index(j)->assign( csv(i,j) );
				}
			}
		}
	}
	else if ( data.type() == lk::vardata_t::VECTOR )
	{
		wxCSVData csv;
		for( size_t i=0;i<data.length();i++ )
		{
			lk::vardata_t &row = *data.index(i);
			if ( row.type() == lk::vardata_t::VECTOR )
			{
				for( size_t j=0;j<row.length();j++ )
					csv(i,j) = row.index(j)->as_string();
			}
			else
				csv(0,i) = row.as_string();
		}

		wxString s( csv.WriteString() );
		if ( trim_nl && s.Last() == '\n' ) s.Truncate( s.Len() - 1 );
		R.assign( s );
	}
}

void fcall_csvread( lk::invoke_t &cxt )
{
	LK_DOC( "csvread", "Read a CSV file into a 2D array (default) or a table. Options: "
		"'skip' (header lines to skip), "
		"'numeric' (t/f to return numbers), "
		"'delim' (character(s) to use as delimiters, default is comma), "
		"'table' (t/f to return a table assuming 1 header line with names)",
		"(string:file[, table:options]):array or table" );
	
	lk::vardata_t &out = cxt.result();
	
	size_t nskip = 0;
	bool tonum = false;
	bool astable = false;
	wxUniChar sep(',');

	if ( cxt.arg_count() > 1 && cxt.arg(1).deref().type() == lk::vardata_t::HASH )
	{
		lk::vardata_t &opts = cxt.arg(1).deref();

		if (lk::vardata_t *item = opts.lookup("skip"))
			nskip = item->as_unsigned();

		if ( lk::vardata_t *item = opts.lookup("numeric"))
			tonum = item->as_boolean();

		if ( lk::vardata_t *item = opts.lookup("table"))
			astable = item->as_boolean();

		if ( lk::vardata_t *item = opts.lookup("delim"))
		{
			wxString s( item->as_string() );
			if ( s.Len() > 0 )
				sep = s.at(0);
		}
	}
	
	wxCSVData csv;
	csv.SetSeparator( sep );
	if ( !csv.ReadFile( cxt.arg(0).as_string() ) 
		|| csv.NumRows() == 0
		|| csv.NumCols() == 0 )
	{
		out.nullify();
		return;
	}

	size_t nr = csv.NumRows();
	size_t nc = csv.NumCols();


	if ( nskip >= nr ) nskip = 0;

	if ( astable )
	{
		if ( nskip >= nr-1 ) nskip = 0;

		out.empty_hash();
		for( size_t c=0;c<nc;c++ )
		{
			wxString name( csv(nskip,c) );
			if ( name.IsEmpty() ) continue;

			lk::vardata_t &it = out.hash_item(name);
			it.empty_vector();
			it.resize( nr-1-nskip );
			for( size_t i=1+nskip;i<nr;i++ )
			{
				if ( tonum ) it.index(i-1-nskip)->assign( wxAtof( csv(i,c) ) );
				else it.index(i-1-nskip)->assign( csv(i,c) );
			}
		}
	}
	else
	{
		out.empty_vector();
		out.vec()->resize( nr-nskip );
		for( size_t i=0;i<nr-nskip;i++ )
		{
			lk::vardata_t *row = out.index(i);
			row->empty_vector();
			row->vec()->resize( nc );

			for( size_t j=0;j<nc;j++ )
			{
				if ( tonum ) row->index(j)->assign( wxAtof( csv(i+nskip,j) ) );
				else row->index(j)->assign( csv(i+nskip,j) );
			}
		}
	}
}

void fcall_csvwrite( lk::invoke_t &cxt )
{
	LK_DOC( "csvwrite", "Write a CSV file from a 2D array or table of arrays. Possible options: 'cols'=[column name list]", "(string:file, array or table:data, [table:options]):boolean" );

	wxCSVData csv;

	lk::vardata_t &data = cxt.arg(1).deref();
	if ( data.type() == lk::vardata_t::HASH )
	{
		std::vector<lk_string> colnames;
		if ( cxt.arg_count() > 2 && cxt.arg(2).type() == lk::vardata_t::HASH )
		{
			lk::vardata_t &opt = cxt.arg(2);

			if ( lk::vardata_t *cl = opt.lookup( "cols" ) )
			{
				if ( cl->type() == lk::vardata_t::VECTOR )
				{

					for( size_t i=0;i<cl->length();i++ )
					{
						lk_string s = cl->index(i)->as_string();
						if ( data.hash()->find( s ) != data.hash()->end() )
							colnames.push_back( s );
					}
				}
			}
		}
		
		
		if ( colnames.size() == 0 )
		{
			for( lk::varhash_t::iterator it = data.hash()->begin();
				it != data.hash()->end();
				++it )
				colnames.push_back( it->first );
		}

		size_t icol = 0;
		for( std::vector<lk_string>::iterator it = colnames.begin();
			it != colnames.end();
			++it )
		{
			lk::varhash_t::iterator itdd = data.hash()->find( *it );
			if ( itdd != data.hash()->end() )
			{
				csv(0,icol) = itdd->first;
				lk::vardata_t &dd = *itdd->second;
				if ( dd.type() == lk::vardata_t::VECTOR )
				{
					for( size_t row=0;row<dd.length();row++ )
						csv(1+row,icol) = dd.index(row)->as_string();
				}
				else
					csv(1,icol) = dd.as_string();

				icol++;			
			}
		}
	}
	else if (data.type() == lk::vardata_t::VECTOR )
	{
		for( size_t r=0;r<data.length();r++ )
		{
			lk::vardata_t &row = data.index(r)->deref();
			if( row.type() == lk::vardata_t::VECTOR )
			{
				for( size_t c=0;c<row.length();c++ )
					csv(r,c) = row.index(c)->as_string();
			}
			else
				csv(r,0) = row.as_string();
		}
	}
	else
		csv(0,0) = data.as_string();

	cxt.result().assign( csv.WriteFile( cxt.arg(0).as_string() ) ? 1.0 : 0.0 );
}

void fcall_rand( lk::invoke_t &cxt )
{
	LK_DOC("rand", "Generate a random number between 0 and 1.", "(none):number");
static wxMTRand rng;
	cxt.result().assign( rng.rand() );
}

static void fcall_apikeys( lk::invoke_t &cxt )
{
	LK_DOC( "apikeys", "Set API keys for Google and Bing web services. Table can have 'google' and 'bing' keys.", "(table:keys):none" );
	
	wxString google, bing;
	if ( lk::vardata_t *x = cxt.arg(0).lookup( "google" ) )
		google = x->as_string();

	if ( lk::vardata_t *x = cxt.arg(0).lookup( "bing" ) )
		bing = x->as_string();

	wxEasyCurl::SetApiKeys( google, bing );
}

static void fcall_geocode( lk::invoke_t &cxt )
{
	LK_DOC( "geocode", "Returns the latitude and longitude of an address using Google's geocoding web API.  Returned table fields are 'lat', 'lon', 'ok'.", "(string:address):table");
		
	double lat = 0, lon = 0;
	bool ok = wxEasyCurl::GeoCode( cxt.arg(0).as_string(), &lat, &lon );
	cxt.result().empty_hash();
	cxt.result().hash_item("lat").assign(lat);
	cxt.result().hash_item("lon").assign(lon);
	cxt.result().hash_item("ok").assign( ok ? 1.0 : 0.0 );
}

static void fcall_browse( lk::invoke_t &cxt )
{
	LK_DOC("browse", "Open a URL, local file, or folder using the default browser.", "(string:url):none");
	::wxLaunchDefaultBrowser( cxt.arg(0).as_string() );
}

static void fcall_curl( lk::invoke_t &cxt )
{
	LK_DOC( "curl", "Issue a synchronous HTTP/HTTPS request.  Options are: 'post', 'message', 'file'.  If 'file' is specified, data is downloaded to that file and true/false is returned. Otherwise, the retrieved data is returned as a string.", 
		"(string:url, [table:options]):variant" );
	wxEasyCurl curl;
	
	wxString url(cxt.arg(0).as_string()), msg, file;

	if ( cxt.arg_count() > 1 && cxt.arg(1).type() == lk::vardata_t::HASH )
	{
		lk::vardata_t &opt = cxt.arg(1);

		if ( lk::vardata_t *x = opt.lookup( "post" ) )
			curl.SetPostData( x->as_string() );

		if ( lk::vardata_t *x = opt.lookup( "message" ) )
			msg = x->as_string();

		if ( lk::vardata_t *x = opt.lookup( "file" ) )
			file = x->as_string();

	}

	if ( !curl.Get( url, msg ) )
	{
		cxt.result().assign( 0.0 );
		return;
	}
	
	if ( !file.IsEmpty() )
		cxt.result().assign( curl.WriteDataToFile( file ) ? 1.0 : 0.0 );
	else
		cxt.result().assign( curl.GetDataAsString() );
}


void fcall_decompress( lk::invoke_t &cxt )
{
	LK_DOC("decompress", "Decompress an archive file to a local folder.  Supports .zip, .tar, .tar.gz, and .gz archives.", "(string:archive file, string:output folder):boolean");
	cxt.result().assign( wxDecompressFile( cxt.arg(0).as_string(), cxt.arg(1).as_string() ) );
}

void fcall_out( lk::invoke_t &cxt )
{
	LK_DOC("out", "Output data to the console.", "(...):none");

	wxLKScriptCtrl *lksc = (wxLKScriptCtrl*)cxt.user_data();
	wxString output;
	for (size_t i=0;i<cxt.arg_count();i++)
		output += cxt.arg(i).as_string();
	lksc->OnOutput( output );
}

void fcall_outln( lk::invoke_t &cxt )
{
	LK_DOC("outln", "Output data to the console with a newline.", "(...):none");
	
	wxLKScriptCtrl *lksc = (wxLKScriptCtrl*)cxt.user_data();
	wxString output;
	for (size_t i=0;i<cxt.arg_count();i++)
		output += cxt.arg(i).as_string(); 
	output += '\n';
	lksc->OnOutput( output );
}

lk::fcall_t* wxLKPlotFunctions()
{
	static const lk::fcall_t vec[] = {
		fcall_newplot,
		fcall_plot,
		fcall_plotopt,
		fcall_annotate,
		fcall_plotout,
		fcall_axis, 
		fcall_meshgrid,
		fcall_peaks,
		fcall_griddata,
		fcall_contour,
		fcall_sector,
		fcall_gifanim,
		0 };
		
	return (lk::fcall_t*)vec;
}

lk::fcall_t* wxLKMiscFunctions()
{
	static const lk::fcall_t vec[] = {
		fcall_browse,
		fcall_curl,
		fcall_geocode,
		fcall_apikeys,
		fcall_rand,
		0 };
		
	return (lk::fcall_t*)vec;
}

lk::fcall_t* wxLKFileFunctions()
{
	static const lk::fcall_t vec[] = {
		fcall_csvread,
		fcall_csvwrite,
		fcall_csvconv,
		fcall_decompress, 
		0 };
		
	return (lk::fcall_t*)vec;
}

lk::fcall_t* wxLKStdOutFunctions()
{
	static const lk::fcall_t vec[] = {
		fcall_out, 
		fcall_outln,
		0 };
		
	return (lk::fcall_t*)vec;
}
class wxLKDebugger : public wxFrame
{
	wxTextCtrl *m_vars;
	wxLKScriptCtrl *m_lcs;
	wxListBox *m_asm;
	wxTextCtrl *m_stack;
	wxMetroButton *m_step1;
	wxPanel *m_panel;
	lk::vm *m_vm;

public:
	enum { ID_RESUME = wxID_HIGHEST+391, ID_STEP, ID_STEP_ASM, ID_BREAK, ID_SWITCH_ADVANCED };

	wxLKDebugger( wxLKScriptCtrl *lcs, lk::vm *vm )
		: wxFrame( lcs, wxID_ANY, "Debugger", wxDefaultPosition, wxScaleSize( 450, 310 ), 
			wxRESIZE_BORDER|wxCAPTION|wxCLOSE_BOX|wxFRAME_TOOL_WINDOW|wxFRAME_FLOAT_ON_PARENT ), 
			m_lcs(lcs), m_vm(vm)
	{
		m_panel = new wxPanel(this);
		m_panel->SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

		wxBoxSizer *button_sizer = new wxBoxSizer( wxHORIZONTAL );
		button_sizer->Add( new wxMetroButton( m_panel, ID_STEP, "Step" ), 0, wxALL|wxEXPAND, 0 );

		m_step1 = new wxMetroButton( m_panel, ID_STEP_ASM, "Step 1" );
		m_step1->Hide();
		button_sizer->Add( m_step1, 0, wxALL|wxEXPAND, 0 );

		button_sizer->Add( new wxMetroButton( m_panel, ID_RESUME, "Resume" ), 0, wxALL|wxEXPAND, 0 );
		button_sizer->Add( new wxMetroButton( m_panel, ID_BREAK, "Break" ), 0, wxALL|wxEXPAND, 0 );
		button_sizer->AddStretchSpacer();
		button_sizer->Add( new wxMetroButton( m_panel, wxID_CLOSE, "Close" ), 0, wxALL|wxEXPAND, 0 );
				
		m_vars = new wxTextCtrl( m_panel, wxID_ANY, "ready", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY|wxTE_DONTWRAP|wxBORDER_NONE );
	
		m_asm = new wxListBox( m_panel, wxID_ANY, wxDefaultPosition, 
			wxDefaultSize, 0, 0, wxLB_HSCROLL|wxBORDER_NONE );
		m_asm->SetFont( wxFont( 9, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "Consolas" ) );
		m_asm->SetForegroundColour( "Forest green" );
		m_asm->Hide();

		m_stack = new wxTextCtrl( m_panel, wxID_ANY, "stack", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY|wxTE_DONTWRAP|wxBORDER_NONE );
		m_stack->SetForegroundColour( *wxRED );
		m_stack->Hide();

		wxBoxSizer *vsplit = new wxBoxSizer( wxVERTICAL );
		vsplit->Add( m_asm, 3, wxALL|wxEXPAND, 0 );
		vsplit->Add( m_stack, 1, wxALL|wxEXPAND, 0 );
	
		wxBoxSizer *hsplit = new wxBoxSizer( wxHORIZONTAL );
		hsplit->Add( m_vars, 2, wxALL|wxEXPAND, 0 );
		hsplit->Add( vsplit, 1, wxALL|wxEXPAND, 0 );

		wxBoxSizer *main_sizer = new wxBoxSizer( wxVERTICAL );
		main_sizer->Add( button_sizer, 0, wxALL|wxEXPAND, 0 );
		main_sizer->Add( hsplit, 1, wxALL|wxEXPAND, 0 );
		m_panel->SetSizer( main_sizer );	
	}

	void PrependMessage( const wxString &msg )
	{
		m_vars->ChangeValue( msg + "\n\n" + m_vars->GetValue() );
	}

	void SetMessage( const wxString &msg )
	{
		m_vars->ChangeValue( msg );
	}

	void UpdateAssembly( const wxArrayString &asmlines )
	{		
		m_asm->Freeze();
		m_asm->Clear();
		m_asm->Append( asmlines );
		m_asm->Thaw();
	}

	void UpdateView()
	{		
		size_t ip = m_vm->get_ip();
		if ( ip  <  m_asm->GetCount() )
			m_asm->SetSelection( ip );

		wxString sout;

		wxString err( m_vm->error() );
		if ( !err.IsEmpty() )
			sout += err + "\n\n";

		size_t nfrm = 0;
		lk::vm::frame **frames = m_vm->get_frames( &nfrm );
		for( size_t i=0;i<nfrm;i++ )
		{
			lk::vm::frame &F = *frames[nfrm-i-1];
			if ( i > 0 ) sout += "\n";

			if ( i==nfrm-1 ) sout += "main:\n";
			else sout += F.id + "():\n";

			wxArrayString keys;
			lk_string key;
			lk::vardata_t *val;
			bool has_more = F.env.first( key, val );
			while( has_more )
			{
				keys.Add( key );
				has_more = F.env.next( key, val );
			}

			keys.Sort();

			for( size_t k=0;k<keys.size();k++ )
				if ( val = F.env.lookup( keys[k], false ) )
					sout += "    " + keys[k] + "  =  " + val->as_string() + "\n";
		}

		m_vars->ChangeValue( sout );

		
		size_t sp = 0;
		lk::vardata_t *stack = m_vm->get_stack( &sp );
		sout = "breakpoints: ";
		std::vector<lk::srcpos_t> bk( m_vm->getbrk() );
		if ( bk.size() > 0 )
		{
			for( size_t i=0;i<bk.size();i++ )
				sout += bk[i].file + wxString::Format(":%d ", bk[i].line );
		}
		else
			sout += "none";

		sout += wxString::Format("\nstack [%d]:\n", (int)sp);
		for( size_t i=0;i<sp;i++ )
		{
			lk::vardata_t &sval = stack[sp-i-1];
			sout += "\t" + sval.as_string() + "\t\t(" + sval.typestr() + ")\n";
		}
		m_stack->ChangeValue( sout );
	}

	void OnCommand( wxCommandEvent &evt )
	{
		switch( evt.GetId() )
		{
		case ID_RESUME:
			m_lcs->Debug( wxLKScriptCtrl::DEBUG_RUN );
			break;
		case ID_STEP:
			m_lcs->Debug( wxLKScriptCtrl::DEBUG_STEP );
			break;
		case ID_STEP_ASM:
			m_lcs->Debug( wxLKScriptCtrl::DEBUG_SINGLE );
			break;
		case ID_BREAK:
			m_lcs->Stop();
			break;
		case wxID_CLOSE:
			Close();
			break;
		}
	}

	void OnClose( wxCloseEvent &evt )
	{
		// turn off current line indicator
		m_lcs->HideLineArrow();

		// simply hide the debugger window, owned by the LK script control widget
		Hide();
		evt.Veto();
	}
	
	void OnCharHook( wxKeyEvent &evt )
	{
		int key = evt.GetKeyCode();
		if ( key == WXK_F2
			|| key == 'A' )
		{
			bool show = !m_asm->IsShown();
			m_asm->Show( show );
			m_stack->Show( show );
			m_step1->Show( show );

			m_panel->Layout();
			Refresh();
		}
		
		evt.Skip();
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( wxLKDebugger, wxFrame )
	EVT_BUTTON( wxLKDebugger::ID_RESUME, wxLKDebugger::OnCommand )
	EVT_BUTTON( wxLKDebugger::ID_STEP, wxLKDebugger::OnCommand )
	EVT_BUTTON( wxLKDebugger::ID_BREAK, wxLKDebugger::OnCommand )
	EVT_BUTTON( wxLKDebugger::ID_STEP_ASM, wxLKDebugger::OnCommand )
	EVT_BUTTON( wxID_CLOSE, wxLKDebugger::OnCommand )
	EVT_CHAR_HOOK( wxLKDebugger::OnCharHook )
	EVT_CLOSE( wxLKDebugger::OnClose )
END_EVENT_TABLE()


enum { IDT_TIMER = wxID_HIGHEST+213 };

BEGIN_EVENT_TABLE( wxLKScriptCtrl, wxCodeEditCtrl )	
	EVT_STC_MODIFIED( wxID_ANY, wxLKScriptCtrl::OnScriptTextChanged )
	EVT_STC_MARGINCLICK( wxID_ANY, wxLKScriptCtrl::OnMarginClick )
	EVT_TIMER( IDT_TIMER, wxLKScriptCtrl::OnTimer )
END_EVENT_TABLE()

wxLKScriptCtrl::wxLKScriptCtrl( wxWindow *parent, int id,
	const wxPoint &pos, const wxSize &size, unsigned long libs )
	: wxCodeEditCtrl( parent, id, pos, size ), m_vm(this),
		m_timer( this, IDT_TIMER )
{
	m_syntaxCheckRequestId = m_syntaxCheckThreadId = 0;
	Bind( wxEVT_THREAD, &wxLKScriptCtrl::OnSyntaxCheckThreadFinished, this );

	m_debugger = new wxLKDebugger( this, &m_vm );
	m_debugger->Hide();
	m_debuggerFirstShow = true;

	m_syntaxCheck = true;
	m_env = new lk::env_t;	
	m_scriptRunning = false;
	m_stopScriptFlag = false;

	SetLanguage( LK );
	EnableCallTips( true );
		
	if( libs & wxLK_STDLIB_BASIC ) 
		RegisterLibrary( lk::stdlib_basic(), "Basic Operations" );
	if( libs & wxLK_STDLIB_SYSIO ) 
		RegisterLibrary( lk::stdlib_sysio(), "System Input/Output" );
	if( libs & wxLK_STDLIB_STRING ) 
		RegisterLibrary( lk::stdlib_string(), "String Functions" );
	if( libs & wxLK_STDLIB_MATH ) 
		RegisterLibrary( lk::stdlib_math(), "Math Functions" );
	if( libs & wxLK_STDLIB_WXUI ) 
		RegisterLibrary( lk::stdlib_wxui(), "User interface Functions" );
	if( libs & wxLK_STDLIB_PLOT ) 
		RegisterLibrary( wxLKPlotFunctions(), "Plotting Functions", this );
	if( libs & wxLK_STDLIB_MISC ) 
		RegisterLibrary( wxLKMiscFunctions(), "Misc Functions", this );
	if( libs & wxLK_STDLIB_FILE ) 
		RegisterLibrary( wxLKFileFunctions(), "Data File Functions", this );
	if( libs & wxLK_STDLIB_SOUT ) 
		RegisterLibrary( wxLKStdOutFunctions(), "BIOS Functions", this );
	
	wxFont font( *wxNORMAL_FONT );
	AnnotationSetStyleOffset( 512 );
	StyleSetFont( 512, font );
	StyleSetForeground( 512, *wxBLACK );
	StyleSetBackground( 512, wxColour(255,187,187) );

	MarkerSetBackground( m_markLeftBox, *wxRED );
	SetMarginWidth( m_syntaxCheckMarginId, (int)(5.0 * wxGetScreenHDScale()) );
	SetMarginSensitive( m_syntaxCheckMarginId, true );
	SetMarginType( m_syntaxCheckMarginId, wxSTC_MARGIN_SYMBOL );

	ShowBreakpoints( true );
}

wxLKScriptCtrl::~wxLKScriptCtrl()
{
	// wait for the current parsing thread to finish
	// if it is in progress
	if (GetThread() && GetThread()->IsRunning())        
		GetThread()->Wait();

	delete m_env;
}

bool wxLKScriptCtrl::OnEval( int /*line*/ )
{
	return !IsStopFlagSet();
}

void wxLKScriptCtrl::OnOutput( const wxString &output )
{
	wxLogStatus( output ); // default behavior
}

void wxLKScriptCtrl::RegisterLibrary( lk::fcall_t *funcs, const wxString &group, void *user_data )
{
	m_env->register_funcs( funcs, user_data );
	libdata x;
	x.library = funcs;
	x.name = group;
	m_libs.push_back( x );
	UpdateInfo();
}

void wxLKScriptCtrl::UpdateInfo()
{
	ClearCallTips();
	std::vector<lk_string> list = m_env->list_funcs();
	wxString funclist;
	for (size_t i=0;i<list.size();i++)
	{
		lk::doc_t d;
		if (lk::doc_t::info( m_env->lookup_func( list[i] ), d ))
		{
			wxString data = d.func_name + d.sig1 + "\n\n" + d.desc1;
			if (d.has_2) data += "\n\n" + d.func_name + d.sig2 + "\n\n" + d.desc2;
			if (d.has_3) data += "\n\n" + d.func_name + d.sig3 + "\n\n" + d.desc3;

			AddCallTip( d.func_name, data );	
			funclist += d.func_name + " ";
		}
	}

	SetKnownIdentifiers( funclist );
}


wxString wxLKScriptCtrl::GetHtmlDocs()
{
	wxString data;
	for ( size_t i=0;i<m_libs.size();i++)
		data += lk::html_doc( m_libs[i].name, m_libs[i].library );
	return data;
}

class LKDocListBox : public wxHtmlListBox
{
	wxArrayString m_htmlData;
	std::vector<size_t> m_indices;
public:
	LKDocListBox( wxWindow *parent, int id, std::vector< wxLKScriptCtrl::libdata > &ll )
		: wxHtmlListBox( parent, id, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE )
	{
		for( size_t i=0;i<ll.size();i++ )
		{
			int j=0;
			while( ll[i].library[j] )
			{
				m_htmlData.Add( lk::html_doc( ll[i].library[j] ) );
				j++;
			}
		}

		Filter( wxEmptyString );
	}

	void Filter( const wxString &filter )
	{
		if ( m_htmlData.size() == 0 ) return;

		if (filter.IsEmpty())
		{
			m_indices.resize( m_htmlData.size(), 0 );
			for( size_t i=0;i<m_indices.size();i++ )
				m_indices[i] = i;		
		}
		else
		{
			m_indices.clear();
			m_indices.reserve( m_htmlData.size() );
			for (size_t i=0;i<m_htmlData.size();i++)
			{
				wxString &html = m_htmlData[i];
				if (filter.Len() <= 2 && html.Left( filter.Len() ).Lower() == filter)
				{
					m_indices.push_back( i );
				}
				else if (html.Lower().Find( filter ) >= 0)
				{
					m_indices.push_back( i );
				}
				else if (html.Lower().Find( filter ) == 0)
				{
					m_indices.push_back( i );
				}
			}
		}

		SetItemCount( m_indices.size() );
		Refresh();
	}

	virtual wxString OnGetItem( size_t n ) const
	{
		return m_htmlData[ m_indices[n] ];
	}

	void OnDClick( wxMouseEvent & )
	{
		/* nothing to do */
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( LKDocListBox, wxHtmlListBox )
	EVT_LEFT_DCLICK( LKDocListBox::OnDClick )
	EVT_MIDDLE_DCLICK( LKDocListBox::OnDClick )
	EVT_RIGHT_DCLICK( LKDocListBox::OnDClick )
END_EVENT_TABLE()

enum{ ID_FILTER = wxID_HIGHEST + 857 };

class LKDocWindow : public wxFrame
{
	LKDocListBox *m_list;
	wxTextCtrl *m_filter;
public:
	LKDocWindow( wxWindow *parent, 
		std::vector<wxLKScriptCtrl::libdata> &ll,
		const wxString &title = "Function Reference" )
		: wxFrame( parent, wxID_ANY, title,
			wxDefaultPosition, wxScaleSize(700, 600),
			wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN | wxRESIZE_BORDER | wxFRAME_TOOL_WINDOW | wxFRAME_FLOAT_ON_PARENT)
	{
		SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

		m_filter = new wxTextCtrl( this, ID_FILTER );
		m_list = new LKDocListBox( this, wxID_ANY, ll );

		wxStaticText *label = new wxStaticText( this, wxID_ANY, "Search:");
		label->SetForegroundColour( *wxWHITE );
		wxBoxSizer *tools = new wxBoxSizer( wxHORIZONTAL );
		tools->Add( label, 0, wxLEFT|wxALIGN_CENTER_VERTICAL, 4 );
		tools->Add( m_filter, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
		tools->AddStretchSpacer();
		tools->Add( new wxMetroButton( this, wxID_CLOSE, "Close" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );

		wxBoxSizer *main = new wxBoxSizer( wxVERTICAL );
		main->Add( tools, 0, wxALL|wxEXPAND, 0 );
		main->Add( m_list, 1, wxALL|wxEXPAND, 0 );
		SetSizer( main );
	}

	void OnFilter( wxCommandEvent & )
	{
		m_list->Filter( m_filter->GetValue() );
	}

	void OnCommand( wxCommandEvent &evt )
	{
		if ( evt.GetId() == wxID_CLOSE )
			Close();
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( LKDocWindow, wxFrame )
	EVT_TEXT( ID_FILTER, LKDocWindow::OnFilter )
	EVT_BUTTON( wxID_CLOSE, LKDocWindow::OnCommand )
END_EVENT_TABLE()


void wxLKScriptCtrl::ShowHelpDialog( wxWindow *custom_parent )
{
	if (custom_parent == 0)
		custom_parent = this;

	(new LKDocWindow( custom_parent, m_libs ))->Show();

	/*
	wxFrame *frm = new wxFrame( custom_parent, wxID_ANY, 
		"Scripting Reference", wxDefaultPosition, wxSize(900, 800),
		wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN | wxRESIZE_BORDER | wxFRAME_TOOL_WINDOW | wxFRAME_FLOAT_ON_PARENT);
	wxHtmlWindow *html = new wxHtmlWindow( frm, wxID_ANY, 
		wxDefaultPosition, wxDefaultSize, 
		wxHW_DEFAULT_STYLE | wxBORDER_NONE );
	html->SetPage( GetHtmlDocs() );
	
	frm->Show();
	*/
}

void wxLKScriptCtrl::SetSyntaxCheck( bool on )
{
	m_syntaxCheck = on;
	
	SetMouseDwellTime( on ? 500 : wxSTC_TIME_FOREVER );
	SetMarginWidth( m_syntaxCheckMarginId, on ? (int)(wxGetScreenHDScale() * 5.0) : 0 );
	SetMarginSensitive( m_syntaxCheckMarginId, on );
	
	if ( !on )
		m_timer.Stop();
}

void wxLKScriptCtrl::OnSyntaxCheck( int line, const wxString &err )
{	
	if ( line <= 0 ) 
	{
		AnnotationClearAll();
		return;
	}
	

	if ( !err.IsEmpty() )
	{
		line--;

		AnnotationSetText( line, err );
		AnnotationSetStyle( line, 0 );
	
		int curline = GetCurrentLine();
		if ( curline < 0 )
			curline = GetFirstVisibleLine();

		if ( line != curline
			&& ( line < GetFirstVisibleLine() 
				|| line > GetFirstVisibleLine()+LinesOnScreen()) )
		{
			AnnotationSetText( curline, err );
			AnnotationSetStyle( curline, 0 );
		}

		AnnotationSetVisible( wxSTC_ANNOTATION_STANDARD );
		
		wxCriticalSectionLocker lock( m_syntaxCheckCS );
		if ( m_syntaxErrorLines.size() > 0 )
		{
			// mark this as shown by default.
			m_syntaxErrorLines[0] = -m_syntaxErrorLines[0];
		}

	}
}

void wxLKScriptCtrl::OnScriptTextChanged( wxStyledTextEvent &evt )
{
	if ( evt.GetModificationType() & wxSTC_MOD_INSERTTEXT 
		|| evt.GetModificationType() & wxSTC_MOD_DELETETEXT )
	{
		MarkerDeleteAll( m_markLeftBox );
		OnSyntaxCheck();

		if ( m_syntaxCheck )
		{
			m_timer.Stop();
			m_timer.Start( 500, true );
		}
	}
	
	evt.Skip();
}

void wxLKScriptCtrl::OnMarginClick( wxStyledTextEvent &evt )
{
	if (evt.GetMargin() == m_syntaxCheckMarginId )
	{
		int line = LineFromPosition(evt.GetPosition());
		size_t i=0;

		wxCriticalSectionLocker lock( m_syntaxCheckCS );

		while( i < m_syntaxErrorLines.size() )
		{
			if ( line == abs(m_syntaxErrorLines[i])-1 )
			{
				int ifirst = i;

				wxString text;
				while( i < m_syntaxErrorLines.size()
					&& line == abs(m_syntaxErrorLines[i])-1 )
					text += m_syntaxErrorMessages[i++] + "\n";

				text.Trim();

				if ( m_syntaxErrorLines[ifirst] > 0 )
					AnnotationClearLine( line );
				else
				{
					AnnotationSetText( line, text );
					AnnotationSetStyle( line, 0 );
					AnnotationSetVisible( wxSTC_ANNOTATION_STANDARD );
				}

				m_syntaxErrorLines[ifirst] = -m_syntaxErrorLines[ifirst];
				break;
			}

			i++;
		}
	}
	
	evt.Skip();
}

void wxLKScriptCtrl::OnSyntaxCheckThreadFinished( wxThreadEvent & )
{
	if ( GetThread() )
		GetThread()->Wait(); // clean up resources with thread (joinable)

	wxCriticalSectionLocker lock( m_syntaxCheckCS );

	if ( m_syntaxCheckRequestId != m_syntaxCheckThreadId )
	{
		// if a new check request from the editor/timer
		// was issued before this thread was finished,
		// the parse errors are not correct, so don't show them
		// rather, start a new parse thread

		// clear annotations
		AnnotationClearAll();

		// start a new parse thread
		StartSyntaxCheckThread();
		return;
	}

	if ( m_syntaxErrorLines.size() == 0 )
	{
		AnnotationClearAll();
		return;
	}

	OnSyntaxCheck( m_syntaxErrorLines[0], wxJoin(m_syntaxErrorMessages, '\n') );

	for( size_t i=0;i<m_syntaxErrorLines.size();i++ )
	{
		int line = m_syntaxErrorLines[i];			
		MarkerAdd( line-1, m_markLeftBox );

		// negate the error lines, to indicate
		// that the annotations are hidden (by default)
		// they can be toggled on/off by clicking the margin
		m_syntaxErrorLines[i] = -m_syntaxErrorLines[i];
	}	
}




wxThread::ExitCode wxLKScriptCtrl::Entry()
{
	m_syntaxCheckCS.Enter();
	lk::input_string p( m_codeToSyntaxCheck );
	m_syntaxCheckCS.Leave();

	lk::parser parse( p );
	parse.add_search_path( m_syntaxCheckWorkDir );
	
	lk::node_t *tree = parse.script();	
	
	wxCriticalSectionLocker lock( m_syntaxCheckCS );
	
	if ( parse.error_count() == 0 
		&& parse.token() == lk::lexer::END)
	{
		m_syntaxErrorMessages.Clear();
		m_syntaxErrorLines.clear();	
	}
	else
	{
		int i=0;
		while ( i < parse.error_count() )
		{
			int line;
			wxString msg = parse.error(i, &line);

			m_syntaxErrorMessages.Add( msg );
			m_syntaxErrorLines.push_back( line );
			i++;
		}
		
		if ( parse.token() != lk::lexer::END )
		{
			m_syntaxErrorLines.push_back( 
				m_syntaxErrorLines.size() > 0
					? m_syntaxErrorLines.back()
					: -1 );

			m_syntaxErrorMessages.push_back( "parsing did not reach end of input" );
		}
	}

	if ( tree )
		delete tree;

	wxQueueEvent(GetEventHandler(), new wxThreadEvent());
	return (wxThread::ExitCode)0;
}

void wxLKScriptCtrl::OnTimer( wxTimerEvent & )
{
	m_syntaxCheckRequestId++;
	StartSyntaxCheckThread();
}

void wxLKScriptCtrl::StartSyntaxCheckThread()
{
	if ( GetThread() 
		&& GetThread()->IsRunning() )
	{
		// the last syntax check is still in progress
		// so don't start anew.  if the thread finish 
		// handler detects that the request codes don't
		// match, it will issue a new thread start itself
		return;
	}

	MarkerDeleteAll( m_markLeftBox );
	AnnotationClearAll();

	wxCriticalSectionLocker lock(m_syntaxCheckCS);
	m_codeToSyntaxCheck = GetText();
	m_syntaxCheckWorkDir = m_workDir;
	m_syntaxErrorLines.clear();
	m_syntaxErrorMessages.clear();
	m_syntaxCheckThreadId = m_syntaxCheckRequestId;

	if ( wxTHREAD_NO_ERROR != CreateThread( wxTHREAD_JOINABLE ) )
	{
		wxLogStatus("wxLKScriptCtrl: could not create syntax checking worker thread" );
		return;
	}

	if ( wxTHREAD_NO_ERROR != GetThread()->Run() )
	{
		wxLogStatus("wxLKScriptCtrl: could not start the syntax check work thread" );
		return;
	}

	// now wait for thread to do its work
	// when it is done, an event will be issued
}

bool wxLKScriptCtrl::IsScriptRunning()
{
	return m_scriptRunning;
}

bool wxLKScriptCtrl::IsStopFlagSet()
{
	return m_stopScriptFlag;
}

void wxLKScriptCtrl::Stop()
{
	m_stopScriptFlag = true;
}

wxLKScriptCtrl::my_vm::my_vm( wxLKScriptCtrl *lcs ) 
	: lk::vm(), m_lcs(lcs)
{
	m_counter = 0;
}

bool wxLKScriptCtrl::my_vm::on_run( const lk::srcpos_t &sp )
{
	// expression & (constant-1) is equivalent to expression % constant where 
	// constant is a power of two: so use bitwise operator for better performance
	// see https://en.wikipedia.org/wiki/Modulo_operation#Performance_issues 
	if ( 0 == (m_counter++ & 1023) ) {
		wxYield();
		return m_lcs->OnEval( sp.line );
	}
	else return true;
}

bool wxLKScriptCtrl::CompileAndLoad( )
{
	wxBusyInfo info( "Compiling script...", this );
	wxYield();

	lk::input_string p( GetText() );
	lk::parser parse( p );
	if ( !m_workDir.IsEmpty() && wxDirExists( m_workDir ) )
		parse.add_search_path( m_workDir );

	std::auto_ptr<lk::node_t> tree( parse.script() );
			
	int i=0;
	while ( i < parse.error_count() )
		OnOutput( wxString(parse.error(i++)) + "\n" );
	
	if ( parse.token() != lk::lexer::END)
		OnOutput("parsing did not reach end of input\n");

	if ( parse.error_count() > 0 || parse.token() != lk::lexer::END )
		return false;	

	m_env->clear_vars();
	m_env->clear_objs();

	lk::codegen cg;
	if ( cg.generate( tree.get() ) )
	{
		m_assemblyText.Clear();
		wxString bcText;
		cg.textout( m_assemblyText, bcText );
		
		cg.get( m_bc );
		m_vm.load( &m_bc );
		m_vm.initialize( m_env );
		return true;
	}
	else
	{
		OnOutput("error in code generation: " + cg.error() + "\n");
		return false;
	}
}

bool wxLKScriptCtrl::Debug( int mode )
{
	if ( m_debuggerFirstShow )
	{
		wxPoint pt( ClientToScreen( GetPosition() ) );
		wxSize sz( GetClientSize() );
		m_debugger->SetPosition( wxPoint(pt.x + sz.x/2, pt.y + (int)(30.0*wxGetScreenHDScale())) );
		m_debuggerFirstShow = false;
	}

	m_debugger->Show();
	
	m_debugger->UpdateAssembly( wxStringTokenize( m_assemblyText, "\n" ) );

	m_vm.clrbrk();
	if ( mode == DEBUG_RUN )
	{
		// update all breakpoint markers in vm
		std::vector<int> brk( GetBreakpoints() );
		for( size_t i=0;i<brk.size();i++ )
		{
			int ibrk = m_vm.setbrk( brk[i] + 1, wxEmptyString );
			if ( ibrk-1 != brk[i] )
			{
				// update breakpoint position indicators
				// to the actual line where something will break
				RemoveBreakpoint( brk[i] );
				AddBreakpoint( ibrk-1 );
			}
		}
	}

	HideLineArrow();

	m_debugger->SetMessage( "running...\n" );

	m_stopScriptFlag = false;

	lk::vm::ExecMode em( lk::vm::SINGLE );
	if ( mode == DEBUG_RUN ) em = lk::vm::DEBUG;
	else if ( mode == DEBUG_STEP ) em = lk::vm::STEP;

	bool ok = m_vm.run( em );

	if ( !ok )
	{
		OnOutput("*** stopped ***\n" );
		wxString err( m_vm.error() );
		if ( !err.IsEmpty() )
			OnOutput( err + "\n" );
	}
	
	size_t ip = m_vm.get_ip();
	const std::vector<lk::srcpos_t> &dbg = m_bc.debuginfo;
	
	// always update the view.
	m_debugger->UpdateView();
	
	if ( ip < (int)dbg.size() )
	{
		ShowLineArrow( dbg[ip].stmt-1 );
		
		int line = dbg[ip].stmt;
		if ( line > 0 && line <= GetNumberOfLines() )
		{
			int nnl = LinesOnScreen();
			int ln_to_scroll = line - nnl/2 - 1;

			ScrollToLine( ln_to_scroll );
		}
	}
	else
		m_debugger->PrependMessage( "*** finished ***" );

	return ok;
}

void wxLKScriptCtrl::SetWorkDir( const wxString &path )
{
	if ( wxDirExists(path) ) 
		m_workDir = path;
}

wxString wxLKScriptCtrl::GetWorkDir()
{
	return m_workDir;
}

bool wxLKScriptCtrl::Execute( )
{
	if (m_scriptRunning)
	{
		wxMessageBox("A script is already running.");
		return false;
	}
	
	m_scriptRunning = true;
	m_stopScriptFlag = false;

	wxString cwd_old( wxGetCwd() );

	wxString backupfile;
	wxString script = GetText();
	if ( !m_workDir.IsEmpty() && wxDirExists(m_workDir) )
	{
		backupfile = m_workDir + "/~script";
		FILE *fp = fopen( (const char*)backupfile.c_str(), "w" );
		if (fp)
		{
			fprintf( fp, "// backup script written by SAM at %s\n\n", (const char*)wxNow().c_str() );
			fputs( (const char*)script.c_str(), fp );
			fclose(fp);
		}

		// change working directory by default to where script is located
		wxSetWorkingDirectory( m_workDir );
	}
		
	bool success = true;
	wxYield();
	if ( !CompileAndLoad( ) )
		success = false;

	m_vm.clrbrk();
	wxStopWatch sw;
	if ( success ) 
	{
		if ( GetBreakpoints().size() > 0 ) success = Debug( DEBUG_RUN );
		else success = m_vm.run( lk::vm::NORMAL );
	}

	if ( success ) OnOutput(wxString::Format("Elapsed time: %.1lf seconds.\n", 0.001*sw.Time()));
	else
	{
		if (wxYES == wxMessageBox("An error occurred in the script:\n\n" + m_vm.error() + "\n\nBreak into the debugger?", "Query", wxYES_NO ) )
			success = Debug( DEBUG_RUN );
		else
			OnOutput("Error: " + m_vm.error() );
	}

			
	m_env->clear_objs();

	m_scriptRunning = false;
	m_stopScriptFlag = false;

	if ( !backupfile.IsEmpty() && wxFileExists( backupfile ) )
		wxRemoveFile( backupfile );

	// restore previous working directory if it was changed...
	if ( wxGetCwd() != cwd_old )
		wxSetWorkingDirectory( cwd_old );

	return success;
}



class wxLKScriptWindow::MyScriptCtrl : public wxLKScriptCtrl
{
	wxLKScriptWindow *m_scriptwin;
public:
	MyScriptCtrl( wxWindow *parent, int id, wxLKScriptWindow *scriptwin )
		: wxLKScriptCtrl( parent, id, wxDefaultPosition, wxDefaultSize,
			(wxLK_STDLIB_ALL|wxLK_STDLIB_SOUT) ),
		 m_scriptwin( scriptwin )
	{
		ShowFindInFilesButton( true );
	}

	virtual bool OnFindInFiles( const wxString &text, bool match_case, bool whole_word )
	{;
		std::vector<wxLKScriptWindow*> windows = m_scriptwin->GetWindows();
		
		wxProgressDialog dialog( "Find in files", "Searching for " + text, (int)windows.size(), m_scriptwin,
			wxPD_SMOOTH|wxPD_CAN_ABORT );
		dialog.SetClientSize( wxScaleSize(350,100) );
		dialog.CenterOnParent();
		dialog.Show();

		m_scriptwin->ClearOutput();
		int noccur = 0;
		for( size_t i=0;i<windows.size();i++ )
		{
			wxLKScriptWindow *sw = windows[i];

			int iter = 0;
			int pos, line_num;
			wxString line_text;
			while( sw->Find( text, match_case, whole_word,
				iter == 0, &pos, &line_num, &line_text ) )
			{
				m_scriptwin->AddOutput( sw->GetTitle() + " (" + wxString::Format("%d):  ", line_num+1) + line_text );
				noccur++;
				iter++;
			}

			if ( !dialog.Update( i ) )
				break;
		}

		m_scriptwin->AddOutput( wxString::Format("\n%d files searched, %d occurences found.", (int)windows.size(), noccur) );

		return true;
	}
	
	virtual void OnOutput( const wxString &out )
	{
		m_scriptwin->AddOutput( out );
	}

	virtual void OnSyntaxCheck( int, const wxString &errstr )
	{
		// disable auto showing of error annotations in text
		// and post the error in the script output window
		m_scriptwin->ClearOutput();
		m_scriptwin->AddOutput( errstr );
	}
};

BEGIN_EVENT_TABLE( wxLKScriptWindow, wxFrame )
	EVT_BUTTON( wxID_NEW, wxLKScriptWindow::OnCommand )
	EVT_BUTTON( wxID_OPEN, wxLKScriptWindow::OnCommand )
	EVT_BUTTON( wxID_SAVE, wxLKScriptWindow::OnCommand )
	EVT_BUTTON( wxID_SAVEAS, wxLKScriptWindow::OnCommand )
	EVT_BUTTON( wxID_FIND, wxLKScriptWindow::OnCommand )
	EVT_BUTTON( wxID_EXECUTE, wxLKScriptWindow::OnCommand )
	EVT_BUTTON( wxID_STOP, wxLKScriptWindow::OnCommand )
	EVT_BUTTON( wxID_HELP, wxLKScriptWindow::OnCommand )
	EVT_BUTTON( wxID_CLOSE, wxLKScriptWindow::OnCommand )
	EVT_BUTTON( wxID_ABOUT, wxLKScriptWindow::OnCommand )
		
	EVT_MENU( wxID_NEW, wxLKScriptWindow::OnCommand )
	EVT_MENU( wxID_OPEN, wxLKScriptWindow::OnCommand )
	EVT_MENU( wxID_SAVE, wxLKScriptWindow::OnCommand )
	EVT_MENU( wxID_FIND, wxLKScriptWindow::OnCommand )
	EVT_MENU( wxID_EXECUTE, wxLKScriptWindow::OnCommand )
	EVT_MENU( wxID_HELP, wxLKScriptWindow::OnCommand )
	EVT_MENU( wxID_CLOSE, wxLKScriptWindow::OnCommand )

	EVT_STC_MODIFIED( wxID_EDIT, wxLKScriptWindow::OnModified )
	EVT_CLOSE( wxLKScriptWindow::OnClose )
END_EVENT_TABLE()

wxLKScriptWindow::wxLKScriptWindow( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxFrame( parent, id, wxT("untitled"), pos, size ) 
{
	m_lastFindPos = 0;

	SetBackgroundColour( wxMetroTheme::Colour(wxMT_FOREGROUND) );
	
#ifdef __WXOSX__
	wxMenu *file = new wxMenu;
	file->Append( wxID_NEW );
	file->AppendSeparator();
	file->Append( wxID_OPEN );
	file->Append( wxID_SAVE );
	file->Append( wxID_SAVEAS );
	file->AppendSeparator();
	file->Append( wxID_EXECUTE );
	file->AppendSeparator();
	file->Append( wxID_CLOSE );

	wxMenuBar *menuBar = new wxMenuBar;
	menuBar->Append( file, wxT("&File") );
	SetMenuBar( menuBar );
#endif	

	m_toolbar = new wxBoxSizer( wxHORIZONTAL );
	m_toolbar->Add( new wxMetroButton( this, wxID_NEW, "New" ), 0, wxALL|wxEXPAND, 0 );
	m_toolbar->Add( new wxMetroButton( this, wxID_OPEN, "Open" ), 0, wxALL|wxEXPAND, 0 );
	m_toolbar->Add( new wxMetroButton( this, wxID_SAVE, "Save" ), 0, wxALL|wxEXPAND, 0 );
	m_toolbar->Add( new wxMetroButton( this, wxID_SAVEAS, "Save as" ), 0, wxALL|wxEXPAND, 0 );
	m_toolbar->Add( new wxMetroButton( this, wxID_FIND, "Find" ), 0, wxALL|wxEXPAND, 0 );
	m_toolbar->Add( m_runBtn=new wxMetroButton( this, wxID_EXECUTE, "Run", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW ), 0, wxALL|wxEXPAND, 0 );
	m_toolbar->Add( m_stopBtn=new wxMetroButton( this, wxID_STOP, "Stop" ), 0, wxALL|wxEXPAND, 0 );
	m_toolbar->AddStretchSpacer();
	
	m_toolbar->Add( new wxMetroButton( this, wxID_ABOUT, "Functions" ), 0, wxALL|wxEXPAND, 0 );
	m_toolbar->Add( new wxMetroButton( this, wxID_HELP, "Help" ), 0, wxALL|wxEXPAND, 0 );
	m_toolbar->Add( new wxMetroButton( this, wxID_CLOSE, "Close" ), 0, wxALL|wxEXPAND, 0 );

	m_stopBtn->Hide();

	wxSplitterWindow *split = new wxSplitterWindow( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE|wxSP_3DSASH|wxBORDER_NONE );

	m_script = new MyScriptCtrl( split, wxID_EDIT, this );

	m_output = new wxTextCtrl( split, wxID_ANY, wxT("Ready."), wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY|wxBORDER_NONE );
	
	wxBoxSizer *sizer = new wxBoxSizer( wxVERTICAL );
	sizer->Add( m_toolbar, 0, wxALL|wxEXPAND, 0 );
	sizer->Add( split, 1, wxALL|wxEXPAND, 0 );
	SetSizer( sizer );
	
	split->SetMinimumPaneSize( 100 );
	split->SplitHorizontally( m_script, m_output, (int)(-150*wxGetScreenHDScale()) );
	split->SetSashGravity( 1.0 );
		
	std::vector<wxAcceleratorEntry> entries;
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, 'n', wxID_NEW ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, 'o', wxID_OPEN ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, 's', wxID_SAVE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, 'f', wxID_FIND ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_CTRL, 'w', wxID_CLOSE ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F1, wxID_HELP ) );
	entries.push_back( wxAcceleratorEntry( wxACCEL_NORMAL, WXK_F5, wxID_EXECUTE ) );
	SetAcceleratorTable( wxAcceleratorTable( entries.size(), &entries[0] ) );

	UpdateWindowTitle();

}

wxLKScriptCtrl *wxLKScriptWindow::GetEditor()
{
	return m_script;
}


wxLKScriptWindowFactory::wxLKScriptWindowFactory()
{
	// nothing to do
}

wxLKScriptWindowFactory::~wxLKScriptWindowFactory()
{
	// nothing to do
}

class DefaultScriptWindowFactory : public wxLKScriptWindowFactory
{
public:
	DefaultScriptWindowFactory() {
	}
	virtual ~DefaultScriptWindowFactory() {
	}
	
	virtual wxLKScriptWindow *Create()
	{
		return new wxLKScriptWindow( 0, wxID_ANY );
	}
	virtual wxWindow *GetParentWindowForScriptExec()
	{
		return 0;
	}
};

static wxLKScriptWindowFactory *g_scriptWindowFactory = 0;
wxLKScriptWindowFactory &wxLKScriptWindow::GetFactory()
{
	if ( !g_scriptWindowFactory )
		g_scriptWindowFactory = new DefaultScriptWindowFactory();

	return *g_scriptWindowFactory;
}

void wxLKScriptWindow::SetFactory( wxLKScriptWindowFactory *f )
{
	if ( g_scriptWindowFactory )
		delete g_scriptWindowFactory;

	g_scriptWindowFactory = f;
}

wxLKScriptWindow *wxLKScriptWindow::CreateNewWindow( bool show )
{
	wxLKScriptWindow *sw = wxLKScriptWindow::GetFactory().Create();
	sw->Show( show );
	return sw;
}
void wxLKScriptWindow::OpenFiles()
{
	OpenFilesInternal( NULL );
}

void wxLKScriptWindow::OpenFilesInternal( wxLKScriptWindow *current )
{
	wxFileDialog dlg( GetCurrentTopLevelWindow(), "Open script", 
		wxEmptyString, wxEmptyString, "Script Files (*.lk)|*.lk", wxFD_OPEN|wxFD_MULTIPLE );
	if ( wxID_OK == dlg.ShowModal() )
	{
		wxArrayString files;
		dlg.GetPaths( files );
		for( size_t i=0;i<files.size();i++ )
		{
			if ( wxLKScriptWindow *sw = FindOpenFile( files[i] ) )
			{
				sw->Raise();
				sw->SetFocus();
				continue;
			}

			if ( wxFileExists( files[i] ) )
			{
				wxLKScriptWindow *sw = 
					( current != 0 
						&& current->GetFileName().IsEmpty()
						&& !current->IsModified() ) 
					? current 
					: CreateNewWindow( false );

				if ( !sw->Load( files[i] ) )
				{
					wxMessageBox( "Failed to load script.\n\n" + files[i] );
					if ( sw != current ) sw->Destroy();
				}
				else
					sw->Show();
			}
		}
	}
}



std::vector<wxLKScriptWindow*> wxLKScriptWindow::GetWindows()
{
	std::vector<wxLKScriptWindow*> list;
	for( wxWindowList::iterator it = wxTopLevelWindows.begin();
		it != wxTopLevelWindows.end();
		++it )
		if ( wxLKScriptWindow *scrip = dynamic_cast<wxLKScriptWindow*>( *it ) )
			list.push_back( scrip );

	return list;
}

bool wxLKScriptWindow::CloseAll()
{
	bool closed = true;
	std::vector<wxLKScriptWindow*> list = GetWindows();
	for( size_t i=0;i<list.size();i++ )
	{
		if ( !list[i]->Close() )
			closed = false;

		wxYield();
	}

	return closed;
}

wxLKScriptWindow *wxLKScriptWindow::FindOpenFile( const wxString &file )
{
	std::vector<wxLKScriptWindow*> list = GetWindows();
	for( size_t i=0;i<list.size();i++ )
		if ( list[i]->GetFileName() == file )
			return list[i];

	return 0;
}

void wxLKScriptWindow::AddOutput( const wxString &out )
{
	m_output->AppendText( out );
}

void wxLKScriptWindow::ClearOutput()
{
	m_output->Clear();
}

bool wxLKScriptWindow::Save()
{
	if ( m_fileName.IsEmpty() )
		return SaveAs();
	else
		return Write( m_fileName );
}

bool wxLKScriptWindow::SaveAs()
{
	wxFileDialog dlg( this, "Save as...",
		wxPathOnly(m_fileName),
		wxFileNameFromPath(m_fileName),
		"Script Files (*.lk)|*.lk", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if (dlg.ShowModal() == wxID_OK)
		return Write( dlg.GetPath() );
	else
		return false;
}

bool wxLKScriptWindow::Load( const wxString &file )
{
	if( m_script->ReadAscii( file ) )
	{
		m_fileName = file;
		m_script->SetWorkDir( wxPathOnly(file) );
		UpdateWindowTitle();
		return true;
	}
	else
		return false;
}

bool wxLKScriptWindow::Write( const wxString &file )
{
	wxBusyInfo info( "Saving: " + file, this );
	wxMilliSleep( 90 );

	if ( m_script->WriteAscii( file ) )
	{
		m_fileName = file;
		m_script->SetWorkDir( wxPathOnly(file) );
		UpdateWindowTitle();
		return true;
	}
	else return false;
}

void wxLKScriptWindow::UpdateWindowTitle()
{
	wxString title( m_fileName );
	if ( title.IsEmpty() ) title = "untitled";
	if ( m_script->IsModified() ) title += " *";
	if ( m_lastTitle != title )
	{
		SetTitle( title );
		m_lastTitle = title;
	}
}

wxString wxLKScriptWindow::GetFileName()
{
	return m_fileName;
}

void wxLKScriptWindow::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case wxID_NEW:
		CreateNewWindow();
		break;

	case wxID_OPEN:
		OpenFilesInternal( this );
		break;

	case wxID_SAVE:
		Save();
		break;

	case wxID_SAVEAS:
		SaveAs();
		break;

	case wxID_FIND:
		m_script->ShowFindReplaceDialog();
		break;

	case wxID_EXECUTE:
		RunScript();
		break;
				
	case wxID_STOP:
		StopScript();
		break;

	case wxID_CLOSE:
		Close();
		break;

	case wxID_HELP:
		OnHelp();
		break;
		
	case wxID_ABOUT:
		m_script->ShowHelpDialog();
		break;
	};
}

void wxLKScriptWindow::OnHelp()
{
	wxMessageBox( "No help available for scripting." );
}

bool wxLKScriptWindow::RunScript()
{
	m_output->Clear();
	m_runBtn->Hide();
	m_stopBtn->Show();
	Layout();
	wxYield();

	wxString work_dir;
	if( !m_fileName.IsEmpty() )
		work_dir = wxPathOnly(m_fileName);

	m_script->SetWorkDir( work_dir );
	bool ok = m_script->Execute();

	m_stopBtn->Hide();
	m_runBtn->Show();
	Layout();
	return ok;
}
void wxLKScriptWindow::StopScript()
{
	m_script->Stop();
}

bool wxLKScriptWindow::Find( const wxString &text, 
	bool match_case, bool whole_word, bool at_beginning,
	int *pos, int *line, wxString *line_text )
{
	if ( text.Len() == 0 ) return false;

	int flags = 0;	
	if ( whole_word ) flags |= wxSTC_FIND_WHOLEWORD;	
	if ( match_case ) flags |= wxSTC_FIND_MATCHCASE;

	if ( at_beginning )
		m_lastFindPos = 0;

	m_lastFindPos = m_script->FindText( m_lastFindPos, 
		m_script->GetLength(), text, flags );

	if ( m_lastFindPos >= 0 )
	{
		*pos = m_lastFindPos;
		*line = m_script->LineFromPosition( m_lastFindPos );
		*line_text = m_script->GetLine( *line );
		m_lastFindPos += text.Len();
		return true;
	}
	else
		return false;
}

void wxLKScriptWindow::OnModified( wxStyledTextEvent & )
{
	UpdateWindowTitle();
}

bool wxLKScriptWindow::IsModified()
{
	return m_script->IsModified();
}

bool wxLKScriptWindow::QueryAndCanClose()
{
	if ( IsModified() )
	{
		Raise();
		wxString dispname( m_fileName );
		if ( dispname.IsEmpty() ) dispname = "untitled";
		int ret = wxMessageBox("The script '" + dispname + "' has been modified.  Save changes?", "Query", 
			wxICON_EXCLAMATION|wxYES_NO|wxCANCEL, this );
		if (ret == wxYES)
		{
			Save( );
			if ( IsModified() ) // if failed to save, cancel
				return false;
		}
		else if (ret == wxCANCEL)
			return false;

	}
	return true;
}

void wxLKScriptWindow::OnClose( wxCloseEvent &evt )
{	
	if ( m_script->IsScriptRunning() )
	{
		if ( wxYES == wxMessageBox("The script is running.  Stop it?", "Query", wxYES_NO, this ) )
			m_script->Stop();

		evt.Veto();
		return;
	}

	if ( !QueryAndCanClose() )
	{
		evt.Veto();
		return;
	}
	
	Destroy();
}
