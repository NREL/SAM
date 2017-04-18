#ifndef __pl_annotation_h
#define __pl_annotation_h

#include <wx/string.h>

#include "wex/plot/pltext.h"
#include "wex/plot/ploutdev.h"

class wxPLAnnotationMapping
{
public:
	wxPLAnnotationMapping();
	virtual ~wxPLAnnotationMapping();
	virtual wxRealPoint ToDevice( const wxRealPoint &pos ) const = 0;
};

class wxPLAnnotation
{
public:
	enum PositionMode { FRACTIONAL, POINTS, AXIS };
	enum ZOrder { FRONT, BACK };

	wxPLAnnotation();
	virtual ~wxPLAnnotation();
	virtual void Draw( wxPLOutputDevice &dc, const wxPLAnnotationMapping &map ) = 0;
};

class wxPLTextAnnotation : public wxPLAnnotation
{
	wxString m_text;
	wxRealPoint m_pos;
	double m_size;
	double m_angle;
	wxColour m_colour;
	wxPLTextLayout::TextAlignment m_align;
	wxPLTextLayout *m_layout;
	wxRealPoint m_delta;
public:
	wxPLTextAnnotation( const wxString &text,
		const wxRealPoint &pos,
		double size = 0,
		double angle = 0,
		const wxColour &c = *wxBLACK,
		wxPLTextLayout::TextAlignment align = wxPLTextLayout::LEFT,
		const wxRealPoint &delta = wxRealPoint(0,0) );

	virtual ~wxPLTextAnnotation();
	
	virtual void Draw( wxPLOutputDevice &dc, const wxPLAnnotationMapping &map );
};

class wxPLLineAnnotation : public wxPLAnnotation
{
public:
	enum ArrowType { NO_ARROW, FILLED_ARROW, OUTLINE_ARROW };
	wxPLLineAnnotation( const std::vector<wxRealPoint> &pts,
		double size = 1,
		const wxColour &c = *wxBLACK,
		wxPLOutputDevice::Style style = wxPLOutputDevice::SOLID,
		ArrowType arrow = NO_ARROW );

	virtual ~wxPLLineAnnotation();
	
	virtual void Draw( wxPLOutputDevice &dc, const wxPLAnnotationMapping &map );

private:
	std::vector<wxRealPoint> m_points;
	double m_size;
	wxColour m_colour;
	wxPLOutputDevice::Style m_style;
	ArrowType m_arrow;
};

class wxPLBraceAnnotation : public wxPLAnnotation
{	
public:
	wxPLBraceAnnotation( 
		const wxRealPoint &p1,
		const wxRealPoint &p2,
		double scale,
		double size = 1,
		const wxColour &c = *wxBLACK,
		wxPLOutputDevice::Style style = wxPLOutputDevice::SOLID );
	
	virtual ~wxPLBraceAnnotation();
	
	virtual void Draw( wxPLOutputDevice &dc, const wxPLAnnotationMapping &map );

private:
	wxRealPoint m_p1, m_p2;
	double m_scale;
	double m_size;
	wxColour m_colour;
	wxPLOutputDevice::Style m_style;
};

class wxPLShapeAnnotation : public wxPLAnnotation
{
public:
	enum ShapeType { RECTANGLE, CIRCLE };
	wxPLShapeAnnotation( 
		ShapeType type,
		const wxPLRealRect &rect,
		const wxColour &c = *wxLIGHT_GREY,
		bool filled = true,
		double size = 1 );

	virtual ~wxPLShapeAnnotation();
	
	virtual void Draw( wxPLOutputDevice &dc, const wxPLAnnotationMapping &map );
private:
	ShapeType m_type;
	wxPLRealRect m_rect;
	wxColour m_colour;
	bool m_filled;
	double m_size;
};

#endif
