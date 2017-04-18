#ifndef __pl_barplot_h
#define __pl_barplot_h

#include <vector>
#include "wex/plot/plplot.h"

#define wxPL_BAR_AUTOSIZE -1

class wxPLBarPlotBase : public wxPLPlottable
{
public:
	wxPLBarPlotBase();
	wxPLBarPlotBase( const std::vector<wxRealPoint> &data, double baseline,
		const wxString &label = wxEmptyString, 
		const wxColour &col = *wxLIGHT_GREY );
	virtual ~wxPLBarPlotBase();
	
	virtual wxRealPoint At( size_t i ) const;
	virtual size_t Len() const;
	
	void SetColour( const wxColour &col ) { m_colour = col; }
	void SetThickness( double thick = wxPL_BAR_AUTOSIZE ) { m_thickness = thick; }
	void SetData( const std::vector<wxRealPoint> &data ) { m_data = data; }

	virtual void DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct);

protected:
	
	void Init();
	double m_baseline;
	wxColour m_colour;
	double m_thickness;
	std::vector< wxRealPoint > m_data;

};

class wxPLBarPlot : public wxPLBarPlotBase
{
public:
	wxPLBarPlot();
	wxPLBarPlot( const std::vector<wxRealPoint> &data, double baseline_y = 0.0,
		const wxString &label = wxEmptyString, 
		const wxColour &col = *wxLIGHT_GREY );
	virtual ~wxPLBarPlot();
	
	virtual void Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map );
	
	void SetStackedOn( wxPLBarPlot *bp ) { m_stackedOn = bp; }
	void SetGroup( const std::vector<wxPLBarPlot*> &grp ) { m_group = grp; }
	
	virtual wxPLAxis *SuggestYAxis() const;

protected:

	double CalcYPos(double x) const;
	double CalcXPos(double x, const wxPLDeviceMapping &map, double dispwidth);
	double CalcDispBarWidth( const wxPLDeviceMapping &map );
	
	wxPLBarPlot *m_stackedOn;
	std::vector<wxPLBarPlot*> m_group;
};

class wxPLHBarPlot : public wxPLBarPlotBase
{
public:
	wxPLHBarPlot();
	wxPLHBarPlot( const std::vector<wxRealPoint> &data, double baseline_x = 0.0,
		const wxString &label = wxEmptyString,
		const wxColour &col = *wxLIGHT_GREY );
	virtual ~wxPLHBarPlot();
		
	void SetStackedOn( wxPLHBarPlot *bp ) { m_stackedOn = bp; }
	virtual void Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map );
	
	virtual wxPLAxis *SuggestXAxis() const;
protected:
	double CalcXPos(double y) const;
	double CalcDispBarWidth( const wxPLDeviceMapping &map );

	wxPLHBarPlot *m_stackedOn;
};

#endif

