#ifndef __pl_lineplot_h
#define __pl_lineplot_h

#include "wex/plot/plplot.h"

class wxPLLinePlot : public wxPLPlottable
{
public:
	enum Style { NO_LINE, SOLID, DOTTED, DASHED };
	enum Marker { NO_MARKER, CIRCLE, SQUARE, DIAMOND, HOURGLASS };

	wxPLLinePlot();
	wxPLLinePlot( const std::vector<wxRealPoint> &data, 
		const wxString &label = wxEmptyString, 
		const wxColour &col = *wxBLUE,
		Style sty = SOLID,
		double thick = 2.0,
		Marker mkr = NO_MARKER );
	virtual ~wxPLLinePlot();


	virtual wxRealPoint At( size_t i ) const;
	virtual size_t Len() const;
	virtual void Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map );
	virtual void DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct);

	bool GetIgnoreZeros();
	void SetIgnoreZeros(bool value = true);

	void SetColour( const wxColour &col ) { m_colour = col; }
	void SetThickness( double thick ) { m_thickness = thick; }
	void SetStyle( Style ss ) { m_style = ss; }
	void SetMarker( Marker mm ) { m_marker = mm; }
	void SetData( const std::vector<wxRealPoint> &data ) { m_data = data; }

protected:
	void Init();
	wxColour m_colour;
	double m_thickness;
	Style m_style;
	Marker m_marker;
	std::vector< wxRealPoint > m_data;
	
	void DrawMarkers( wxPLOutputDevice &dc, std::vector<wxRealPoint> &points, double size );

private:	
	bool m_ignoreZeros;
};
#endif

