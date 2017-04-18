#ifndef __pl_histplot_h
#define __pl_histplot_h

#include "wex/plot/plplot.h"

class wxPLHistogramPlot : public wxPLPlottable
{
public:
	wxPLHistogramPlot();
	wxPLHistogramPlot( const std::vector<wxRealPoint> &data,
		const wxString &label );

	void Init();

	enum NormalizeType {NO_NORMALIZE = 0, NORMALIZE, NORMALIZE_PDF};
	
	void SetData( const std::vector<wxRealPoint> &data );

	//Getters and Setters
	void SetLineStyle( const wxColour &c, double width );
	void SetFillColour( const wxColour &c );
	void SetNumberOfBins( size_t n );
	void SetNormalize( NormalizeType n );
	NormalizeType GetNormalize() const;
	int GetNumberOfBins() const;
	double GetNiceYMax();
	double HistAt( size_t i ) const;
	wxRealPoint HistBinAt( size_t i ) const;

	bool GetIgnoreZeros();
	void SetIgnoreZeros(bool value = true);

	static int GetSturgesBinsFor(int nDataPoints);
	static int GetSqrtBinsFor(int nDataPoints);

	
	virtual wxRealPoint At( size_t i ) const;
	virtual size_t Len() const;
	virtual void Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map );
	virtual void DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct);
	virtual wxPLAxis *SuggestXAxis();
	virtual wxPLAxis *SuggestYAxis();
	virtual bool GetMinMax(double *pxmin, double *pxmax, double *pymin, double *pymax) const;
	virtual std::vector<wxString> GetExportableDatasetHeaders( wxUniChar sep, wxPLPlot *plot ) const;
	virtual std::vector<wxRealPoint> GetExportableDataset(double Xmin, double Xmax, bool visible_only) const;

private:	
	bool m_normalize;
	bool m_normalizeToPdf;
	bool m_ignoreZeros;

	wxColour m_lineColour;
	double m_lineThickness;
	wxColour m_fillColour;

	size_t m_numberOfBins;

	std::vector<double> m_histData;
	std::vector<wxRealPoint> m_histDataBinRanges;

	void RecalculateHistogram();

	double m_niceMax;
	double m_dataMin, m_dataMax;

	std::vector<wxRealPoint> m_data;
};

#endif
