#ifndef __pl_polarplot_h
#define __pl_polarplot_h

#include "wex/plot/plplot.h"

class wxPLWindRose : public wxPLPlottable
{
public:
	wxPLWindRose();
	wxPLWindRose( const std::vector<wxRealPoint> &data,
		const wxString &label = wxEmptyString,
		const wxColour &col = *wxBLUE );

	virtual ~wxPLWindRose();
	
	virtual wxRealPoint At( size_t i ) const;
	virtual size_t Len() const;
	virtual void Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map );
	virtual void DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct);

	void SetColour( const wxColour &col ) { m_colour = col; }
	void SetIgnoreAngle(bool ignore=true) { m_ignoreAngles = ignore; }

	virtual wxPLAxis *SuggestXAxis() const;
	virtual wxPLAxis *SuggestYAxis() const;

protected:
	wxColour m_colour;
	bool m_ignoreAngles;
	std::vector<wxRealPoint> m_data;

};

#endif
