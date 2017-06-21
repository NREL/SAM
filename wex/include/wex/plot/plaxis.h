#ifndef __pl_axis_h
#define __pl_axis_h

#include <vector>

#include <wx/string.h>
#include <wx/gdicmn.h>
#include <wx/colour.h>

class wxPLAxis
{
public:
	wxPLAxis();
	wxPLAxis( double min, double max, const wxString &label = wxEmptyString );
	wxPLAxis( const wxPLAxis &rhs );
	virtual ~wxPLAxis();
	
	// pure virtuals
	struct TickData {
		enum TickSize { NONE, SMALL, LARGE };
		TickData( ) : world(0.0), size(SMALL), label( wxEmptyString ) { }
		TickData( double w, const wxString &l, TickSize s ) : world(w), size(s), label(l) { }

		double world;
		TickSize size;
		wxString label;
	};

	virtual wxPLAxis *Duplicate() = 0;
	virtual void GetAxisTicks( double phys_min, double phys_max, std::vector<TickData> &list ) = 0;

	// by default assumes a linear axis.  can be overridden to provide logarithmic or other scaling

	virtual double WorldToPhysical( double coord, double phys_min, double phys_max );
	virtual double PhysicalToWorld( double p, double phys_min, double phys_max );

	// get & set axis properties

	virtual void SetWorld( double min, double max );
	virtual void SetWorldMin( double min );
	virtual void SetWorldMax( double max );
	void GetWorld( double *min, double *max ) { if (min) *min = m_min; if (max) *max = m_max; }
	double GetWorldMin() { return m_min; }
	double GetWorldMax() { return m_max; }
	double GetWorldLength() { return m_max - m_min; }
	
	virtual void SetLabel( const wxString &s ) { m_label = s; }
	virtual wxString GetLabel() { return m_label; }
	virtual void SetUnits(const wxString &s) { m_units = s; }
	virtual wxString GetUnits() { return m_units; }
	virtual void SetColour( const wxColour &col ) { m_colour = col; }
	virtual wxColour GetColour() { return m_colour; }
	
	void SetTickSizes( double smallsz, double largesz ) { m_smallTickSize = smallsz; m_largeTickSize = largesz; }
	void GetTickSizes( double *smallsz, double *largesz ) { if (smallsz) *smallsz = m_smallTickSize; if (largesz) *largesz = m_largeTickSize; }

	void Show( bool b ) { m_shown = b; }
	bool IsShown() { return m_shown; }
	void ShowLabel( bool label ) { m_showLabel = label; }
	void ShowTickText( bool ticktext ) { m_showTickText = ticktext; }
	bool IsLabelVisible() { return m_shown&&m_showLabel; }
	bool IsTickTextVisible() { return m_shown&&m_showTickText; }
	
	void SetReversed( bool rev ) { m_reversed = rev; }
	bool IsReversed() const { return m_reversed; }

	virtual void ExtendBound( wxPLAxis *a );


	// useful helper function: ExtendBound will always move a number farther from 0.
	static void ExtendBoundsToNiceNumber( double *upper, double *lower );

protected:
	void Init();
	
	wxString m_label;
	wxString m_units;
	wxColour m_colour;
	double m_min;
	double m_max;
	bool m_showLabel;
	bool m_showTickText;
	double m_smallTickSize;
	double m_largeTickSize;
	bool m_reversed;
	bool m_shown;
};


class wxPLLinearAxis : public wxPLAxis
{
public:
	wxPLLinearAxis( double min, double max, const wxString &label = wxEmptyString );
	wxPLLinearAxis( const wxPLLinearAxis &rhs );

	virtual wxPLAxis *Duplicate();
	virtual void GetAxisTicks( double phys_min, double phys_max, std::vector<TickData> &list );
	
protected:
	virtual void CalcTicksFirstPass( double phys_min, double phys_max, 
			std::vector<double> &largeticks, std::vector<double> &smallticks) ;
	virtual void CalcTicksSecondPass( double phys_min, double phys_max, 
			std::vector<double> &largeticks, std::vector<double> &smallticks) ;
	
	double AdjustedWorldValue( double world );
	double DetermineLargeTickStep( double physical_len, bool &should_cull_middle);
	size_t DetermineNumberSmallTicks( double big_tick_dist );

	double m_scale, m_offset;
	double m_approxNumberLargeTicks;
	std::vector<double> m_mantissas;
	std::vector<size_t> m_smallTickCounts;
};

class wxPLLabelAxis : public wxPLAxis
{
public:
	wxPLLabelAxis( double min, double max, const wxString &label = wxEmptyString );
	wxPLLabelAxis( const wxPLLabelAxis &rhs );
	
	virtual wxPLAxis *Duplicate();
	virtual void GetAxisTicks( double phys_min, double phys_max, std::vector<TickData> &list );

	void Add( double world, const wxString &text );
	void Clear();

protected:	
	std::vector<TickData> m_tickLabels;
};


class wxPLLogAxis : public wxPLAxis
{
public:
	wxPLLogAxis( double min, double max, const wxString &label = wxEmptyString );
	wxPLLogAxis( const wxPLLogAxis &rhs );

	virtual wxPLAxis *Duplicate();
	virtual void GetAxisTicks( double phys_min, double phys_max, std::vector<TickData> &list );
	virtual double WorldToPhysical( double coord, double phys_min, double phys_max );
	virtual double PhysicalToWorld( double p, double phys_min, double phys_max );

	virtual void SetWorld( double min, double max );
	virtual void SetWorldMin( double min );
	virtual void ExtendBound( wxPLAxis *a );

protected:
	virtual void CalcTicksFirstPass( std::vector<double> &largeticks, std::vector<double> &smallticks) ;
	virtual void CalcTicksSecondPass( std::vector<double> &largeticks, std::vector<double> &smallticks) ;
	
	double DetermineTickSpacing();
	size_t DetermineNumberSmallTicks( double big_tick_dist );
};

class wxPLTimeAxis : public wxPLAxis
{
public:
	wxPLTimeAxis( double min, double max, const wxString &label = wxEmptyString );
	wxPLTimeAxis( const wxPLTimeAxis &rhs );

	virtual wxString GetLabel();
	virtual wxPLAxis *Duplicate();
	virtual void GetAxisTicks( double phys_min, double phys_max, std::vector<TickData> &list );

private:
	std::vector<TickData> m_tickList;
	wxString m_timeLabel;
	double m_lastMin, m_lastMax;
	void RecalculateTicksAndLabel();
};

class wxPLPolarAngularAxis : public wxPLLinearAxis
{
public:
	enum PolarAngularZero { UP, RIGHT, DOWN, LEFT };
	enum PolarAxisLabels { NUMBERS, DIRECTIONS };
	enum PolarAngularUnits { NON_POLAR, DEGREES, RADIANS, GRADIANS };

	wxPLPolarAngularAxis(const wxString &label = wxEmptyString, PolarAngularUnits units = DEGREES, PolarAngularZero zero = UP, PolarAxisLabels pal = NUMBERS);
	wxPLPolarAngularAxis( const wxPLPolarAngularAxis &rhs );
	
	virtual void GetAxisTicks(double phys_min, double phys_max, std::vector<TickData> &list);
	virtual PolarAngularUnits GetAxisUnits() { return m_pau; }
	virtual double AngleInRadians(double world) { return AdjustedWorldValue(world); }
	virtual void ExtendBound(wxPLAxis *a);

private:
	PolarAngularUnits m_pau;
	PolarAxisLabels m_pal;
	int AngleInDegrees(double world);
};

/*
class wxPLPolarRadialAxis : public wxPLLinearAxis
{
public:
	wxPLPolarRadialAxis(const wxString &label = wxEmptyString);
	wxPLPolarRadialAxis(const wxPLPolarRadialAxis &rhs);

protected:

private:

};

*/


#endif

