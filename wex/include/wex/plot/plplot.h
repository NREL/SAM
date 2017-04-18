#ifndef __pl_plot_h
#define __pl_plot_h

#include <vector>

#include <wx/string.h>
#include <wx/stream.h>

#include "wex/plot/plaxis.h"
#include "wex/plot/ploutdev.h"
#include "wex/plot/pltext.h"
#include "wex/plot/plannotation.h"

class wxPLPlot;
class wxPLAnnotation;

class wxPLDeviceMapping
{
public:
	wxPLDeviceMapping() { }
	virtual ~wxPLDeviceMapping() { }
	virtual wxRealPoint ToDevice( double x, double y ) const = 0;
	virtual void GetDeviceExtents( wxRealPoint *pos, wxRealPoint *size ) const = 0;
	virtual wxRealPoint GetWorldMinimum() const = 0;
	virtual wxRealPoint GetWorldMaximum() const = 0;
	virtual wxPLAxis* GetXAxis() const = 0;
	virtual wxPLAxis* GetYAxis() const = 0;
	virtual bool IsPrimaryXAxis() const = 0; // true if X_BOTTOM
	virtual bool IsPrimaryYAxis() const = 0; // true if Y_LEFT
	
	inline wxRealPoint ToDevice( const wxRealPoint &p ) const { return ToDevice(p.x, p.y); }
};

class wxPLPlottable
{
protected:
	wxString m_label;
	wxString m_xLabel;
	wxString m_yLabel;
	bool m_showInLegend;
	bool m_antiAliasing;

public:
	wxPLPlottable() : m_label(wxEmptyString), m_showInLegend(true), m_antiAliasing(true) {  }
	wxPLPlottable( const wxString &label ) : m_label(label), m_showInLegend(true), m_antiAliasing(true) {  }
	virtual ~wxPLPlottable() {  }

	// pure virtuals

	virtual wxRealPoint At( size_t i ) const = 0;
	virtual size_t Len() const = 0;
	virtual void Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map ) = 0;
	virtual void DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct ) = 0;

	// properties

	virtual wxString GetLabel() const { return m_label; }
	virtual wxString GetXDataLabel( wxPLPlot *plot ) const;
	virtual wxString GetYDataLabel( wxPLPlot *plot ) const;

	virtual void SetLabel( const wxString &label ) { m_label = label; }
	virtual void SetXDataLabel( const wxString &label ) { m_xLabel = label; }
	virtual void SetYDataLabel( const wxString &label ) { m_yLabel = label; }
	virtual void ShowInLegend( bool b ) { m_showInLegend = b; }
	virtual bool IsShownInLegend() { return m_showInLegend; }

	// display

	void SetAntiAliasing( bool aa ) { m_antiAliasing = aa; }
	bool GetAntiAliasing() { return m_antiAliasing; }


	// helpers

	virtual wxPLAxis *SuggestXAxis() const;
	virtual wxPLAxis *SuggestYAxis() const;

	virtual bool GetMinMax(double *pxmin, double *pxmax, double *pymin, double *pymax) const;
	virtual bool ExtendMinMax(double *pxmin, double *pxmax, double *pymin, double *pymax, bool extendToNice = false) const;
	virtual std::vector<wxString> GetExportableDatasetHeaders( wxUniChar sep, wxPLPlot *plot ) const;
	virtual std::vector<wxRealPoint> GetExportableDataset(double Xmin, double Xmax, bool visible_only) const;
};

class wxPLSideWidgetBase
{
public:
	wxPLSideWidgetBase();
	virtual ~wxPLSideWidgetBase();

	virtual void Render( wxPLOutputDevice &, const wxPLRealRect & ) = 0;
	virtual wxRealPoint CalculateBestSize( wxPLOutputDevice & ) = 0;

	void InvalidateBestSize();
	wxRealPoint GetBestSize( wxPLOutputDevice & );

private:
	wxRealPoint m_bestSize;
};

class wxPLPlot
{
public:
	wxPLPlot();
	virtual ~wxPLPlot();
	
	enum AxisPos { X_BOTTOM, X_TOP, Y_LEFT, Y_RIGHT };
	enum PlotPos { PLOT_TOP, PLOT_BOTTOM, NPLOTPOS };
	enum LegendPos { FLOATING, NORTHWEST, SOUTHWEST, NORTHEAST, SOUTHEAST, NORTH, SOUTH, EAST, WEST, BOTTOM, RIGHT  };

	void AddPlot( wxPLPlottable *p, AxisPos xap = X_BOTTOM, AxisPos yap = Y_LEFT, PlotPos ppos = PLOT_TOP, bool update_axes = true );
	wxPLPlottable *RemovePlot(wxPLPlottable *p, PlotPos plotPosition = NPLOTPOS);
	bool ContainsPlot(wxPLPlottable *p, PlotPos plotPosition = NPLOTPOS);
	void DeleteAllPlots();
	size_t GetPlotCount();
	wxPLPlottable *GetPlot( size_t i );
	wxPLPlottable *GetPlotByLabel( const wxString &series );
	bool GetPlotPosition( const wxPLPlottable *p, 
		AxisPos *xap, AxisPos *yap, PlotPos *ppos );

	void AddAnnotation( wxPLAnnotation *an, 
		wxPLAnnotation::PositionMode pm = wxPLAnnotation::AXIS,
		AxisPos xap = X_BOTTOM,
		AxisPos yap = Y_LEFT,
		PlotPos ppos = PLOT_TOP,
		wxPLAnnotation::ZOrder zo = wxPLAnnotation::FRONT );

	void DeleteAllAnnotations();

	wxPLAxis *GetXAxis1() { return m_x1.axis; }
	wxPLAxis &X1() { return Axis(X_BOTTOM); }
	void SetXAxis1( wxPLAxis *a ) { m_x1.set( a ); }
	
	wxPLAxis *GetXAxis2() { return m_x2.axis; }
	wxPLAxis &X2() { return Axis(X_TOP); }
	void SetXAxis2( wxPLAxis *a ) { return m_x2.set( a ); }
	
	wxPLAxis *GetYAxis1( PlotPos ppos = PLOT_TOP ) { return m_y1[ppos].axis; }
	wxPLAxis &Y1( PlotPos ppos = PLOT_TOP ) { return Axis(Y_LEFT,ppos); }
	void SetYAxis1( wxPLAxis *a, PlotPos ppos = PLOT_TOP ) { m_y1[ppos].set( a ); }
	
	wxPLAxis *GetYAxis2( PlotPos ppos = PLOT_TOP ) { return m_y2[ppos].axis; }
	wxPLAxis &Y2( PlotPos ppos = PLOT_TOP ) { return Axis(Y_RIGHT,ppos); }
	void SetYAxis2( wxPLAxis *a, PlotPos ppos = PLOT_TOP ) { m_y2[ppos].set( a ); }

	wxPLAxis *GetAxis( AxisPos axispos, PlotPos ppos = PLOT_TOP );
	wxPLAxis &Axis( AxisPos axispos, PlotPos ppos = PLOT_TOP );
	void SetAxis( wxPLAxis *a, AxisPos axispos, PlotPos ppos = PLOT_TOP );
	
	void SetTextSize( double points ) { m_textSizePoints = points; }
	double GetTextSize() { return m_textSizePoints; }
	
	void SetBorderWidth( double b=0.5 ) { m_borderWidth = b; } // zero is OK to hide plot borders
	void SetBorderSpace( double left=0, double right=0, double top=0, double bottom=0 ) { 
		m_spaceLeftTop.x = left; m_spaceLeftTop.y = top;
		m_spaceRightBottom.x = right; m_spaceRightBottom.y = bottom; }
	void ShowAxes( bool b );
	void ShowGrid( bool coarse, bool fine ) { m_showCoarseGrid = coarse; m_showFineGrid = fine; }
	void ShowCoarseGrid( bool coarse ) { m_showCoarseGrid = coarse; }
	void ShowFineGrid( bool fine ) { m_showFineGrid = fine; }
	void ShowTitle( bool show ) { m_showTitle = show; }
	void SetTitle( const wxString &title );
	wxString GetTitle() { return m_title; }

	void SetGridColour( const wxColour &col ) { m_gridColour = col; }
	void SetPlotAreaColour( const wxColour &col ) { m_plotAreaColour = col; }
	void SetAxisColour( const wxColour &col ) { m_axisColour = col; }
	void SetTickTextColour( const wxColour &col ) { m_tickTextColour = col; }
	
	void ShowLegend( bool show ) { m_showLegend = show; }
	bool IsLegendShown() const { return m_showLegend; }
	void ShowLegendBorder( bool show ) { m_showLegendBorder = show; }
	bool IsLegendBorderShown() { return m_showLegendBorder; }
	void SetLegendReversed( bool reverse ) { m_reverseLegend = reverse; }
	bool IsLegendReversed() const { return m_reverseLegend; }
	void SetLegendPosition( LegendPos pos ) { m_legendPos = pos; }
	void SetLegendLocation( LegendPos pos, double xpercent = -999.0, double ypercent = -999.0 );
	bool SetLegendLocation( const wxString &spos );	
	LegendPos GetLegendPosition() const { return m_legendPos; }
	wxRealPoint GetLegendLocation() { return m_legendPosPercent; }
		
	void SetSideWidget( wxPLSideWidgetBase *sw, AxisPos pos = Y_RIGHT );
	wxPLSideWidgetBase *GetSideWidget( AxisPos pos );
	wxPLSideWidgetBase *ReleaseSideWidget( AxisPos pos );

	void WriteDataAsText( wxUniChar sep, wxOutputStream &os, 
		bool visible_only = true, bool include_x = true );
		
	void UpdateAxes( bool recalculate_all = false );
	void RescaleAxes();
	void DeleteAxes();

	void Invalidate(); // erases all cached positions and layouts, but does not issue refresh
	void Render( wxPLOutputDevice &dc, wxPLRealRect geom ); // note: does not draw the background.  DC should be cleared with desired bg color already


	static bool AddPdfFontDir( const wxString &path );
	static wxString LocatePdfFontDataFile( const wxString &face );
	static wxArrayString ListAvailablePdfFonts();
	static bool SetPdfDefaultFont( const wxString &face );
	bool RenderPdf( const wxString &file, double width, double height, double fontpoints=-1 );
	
	class axis_layout;
protected:

	void InvalidateLegend() { m_legendInvalidated = true; }
	void CalculateLegendLayout( wxPLOutputDevice &dc );
	void DrawLegend(wxPLOutputDevice &gdc, const wxPLRealRect &geom);
	wxPLRealRect GetLegendRect() { return m_legendRect; }
	const std::vector<wxPLRealRect> &GetPlotRects() const { return m_plotRects; }

	void UpdateHighlightRegion();
	void DrawLegendOutline();
private:

	void DrawAnnotations( wxPLOutputDevice &dc, const wxPLRealRect &plotarea, wxPLAnnotation::ZOrder zo );
	void DrawGrid( wxPLOutputDevice &dc, wxPLAxis::TickData::TickSize size );
	void DrawPolarGrid(wxPLOutputDevice &dc, wxPLAxis::TickData::TickSize size);


	wxRealPoint m_spaceLeftTop, m_spaceRightBottom;
	double m_borderWidth;
	bool m_showLegend;
	bool m_showLegendBorder;
	bool m_showCoarseGrid;
	bool m_showFineGrid;
	bool m_showTitle;
	wxString m_title;
	wxColour m_gridColour;
	wxColour m_axisColour;
	wxColour m_tickTextColour;
	wxColour m_plotAreaColour;
	wxPLRealRect m_legendRect;
	LegendPos m_legendPos;
	wxRealPoint m_legendPosPercent;
	bool m_reverseLegend;
	bool m_moveLegendMode;
	bool m_moveLegendErase;
	wxPoint m_anchorPoint;
	wxPoint m_currentPoint;
	double m_textSizePoints;

	std::vector< wxPLRealRect > m_plotRects;
	wxPLSideWidgetBase *m_sideWidgets[4];

	struct plot_data
	{
		plot_data() : plot(0), ppos(PLOT_TOP), xap(X_BOTTOM), yap(Y_LEFT) {  }
		wxPLPlottable *plot;
		PlotPos ppos;
		AxisPos xap;
		AxisPos yap;
	};

	std::vector<plot_data> m_plots;

	struct annot_data
	{
		annot_data() : ann(0), posm( wxPLAnnotation::AXIS ), ppos(PLOT_TOP), xap(X_BOTTOM), yap(Y_LEFT), zorder( wxPLAnnotation::FRONT ) { }
		wxPLAnnotation *ann;
		wxPLAnnotation::PositionMode posm;
		PlotPos ppos;
		AxisPos xap, yap;
		wxPLAnnotation::ZOrder zorder;
	};

	std::vector<annot_data> m_annotations;

	struct legend_item
	{
		legend_item( wxPLOutputDevice &dc, wxPLPlottable *p );
		~legend_item();
		wxPLPlottable *plot;
		wxPLTextLayout *text;
		double width, height;
	};

	bool m_legendInvalidated;
	std::vector< legend_item* > m_legendItems;

	struct axis_data
	{
		axis_data();
		~axis_data();
		void set( wxPLAxis *a );
		void invalidate();

		wxPLAxis *axis;
		axis_layout *layout;
		wxPLTextLayout *label;
	};
	
	wxPLTextLayout *m_titleLayout;
	axis_data m_x1, m_x2, m_y1[NPLOTPOS], m_y2[NPLOTPOS]; // m_x1 used for angular axis on polar plots, m_y1[0] used for radial axis
};

#endif

