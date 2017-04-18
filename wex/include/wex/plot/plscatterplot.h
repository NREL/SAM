#ifndef __pl_scatterplot_h
#define __pl_scatterplot_h

#include "wex/plot/plplot.h"

class wxPLColourMap;

class wxPLScatterPlot : public wxPLPlottable
{
public:
	wxPLScatterPlot();
	wxPLScatterPlot( const std::vector<wxRealPoint> &data,
		const wxString &label = wxEmptyString,
		const wxColour &col = *wxBLUE,
		double size = 1,
		bool scale = false );

	virtual ~wxPLScatterPlot();

	void SetColourMap( wxPLColourMap *cmap ); // does not take ownership of colour map
	void SetColours( const std::vector<double> &zv );
	void ClearColours();
	void SetSizes( const std::vector<double> &sv );
	void ClearSizes();
	
	virtual wxRealPoint At( size_t i ) const;
	virtual size_t Len() const;
	virtual void Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map );
	virtual void DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct);

	void SetColour( const wxColour &col ) { m_colour = col; }
	void SetSize( double radius ) { m_radius = radius; }
	void SetLineOfPerfectAgreementFlag(bool flagValue);

protected:
	wxColour m_colour;
	double m_radius;
	bool m_scale;
	bool m_drawLineOfPerfectAgreement;
	bool m_isLineOfPerfectAgreementDrawn;
	std::vector<wxRealPoint> m_data;
	std::vector<double> m_colours, m_sizes;
	wxPLColourMap *m_cmap;
};

#endif
