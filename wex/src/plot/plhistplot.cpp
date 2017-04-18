#include <wx/dc.h>
#include "wex/plot/plhistplot.h"

wxPLHistogramPlot::wxPLHistogramPlot()
{
	Init();
}

wxPLHistogramPlot::wxPLHistogramPlot( const std::vector<wxRealPoint> &data,
		const wxString &label )
	: wxPLPlottable( label )
{
	Init();
	m_data = data;
	RecalculateHistogram();
}

void wxPLHistogramPlot::Init()
{
	m_lineColour = *wxBLACK;
	m_lineThickness = 1.0;
	m_fillColour = wxColour("dark olive green");
	m_normalize = true;
	m_normalizeToPdf = false;
	m_ignoreZeros = false;
	m_numberOfBins = 20;
	m_niceMax = m_dataMin = m_dataMax = 0.0;
}

void wxPLHistogramPlot::SetData( const std::vector<wxRealPoint> &data )
{
	m_data = data;
	RecalculateHistogram();
}

wxRealPoint wxPLHistogramPlot::At( size_t i ) const
{
	return m_data[i];
}

double wxPLHistogramPlot::HistAt( size_t i ) const
{
	return m_histData[i];
}

wxRealPoint wxPLHistogramPlot::HistBinAt( size_t i ) const
{
	return m_histDataBinRanges[i];
}

size_t wxPLHistogramPlot::Len() const
{
	return m_data.size();
}

void wxPLHistogramPlot::SetLineStyle( const wxColour &c, double width )
{
	m_lineColour = c;
	m_lineThickness = width;
}

void wxPLHistogramPlot::SetFillColour( const wxColour &c )
{
	m_fillColour = c;
}

void wxPLHistogramPlot::SetNumberOfBins( size_t n)
{
	m_numberOfBins = n;
	RecalculateHistogram();
}

void wxPLHistogramPlot::SetNormalize( NormalizeType n )
{
	switch(n)
	{
	case NO_NORMALIZE:
		m_normalize = false;
		m_normalizeToPdf = false;
		break;
	case NORMALIZE:
		m_normalize = true;
		m_normalizeToPdf = false;
		break;
	case NORMALIZE_PDF:
		m_normalize = true;
		m_normalizeToPdf = true;
	}

	RecalculateHistogram();
}

wxPLHistogramPlot::NormalizeType wxPLHistogramPlot::GetNormalize() const
{
	if (m_normalize)
	{
		if (m_normalizeToPdf)
			return NORMALIZE_PDF;

		return NORMALIZE;
	}
	return NO_NORMALIZE;
}

int wxPLHistogramPlot::GetNumberOfBins() const
{
	return m_numberOfBins;
}

double wxPLHistogramPlot::GetNiceYMax()
{
	return m_niceMax;
}

bool wxPLHistogramPlot::GetIgnoreZeros()
{
	return m_ignoreZeros;
}

void wxPLHistogramPlot::SetIgnoreZeros(bool value)
{
	m_ignoreZeros = value;
	RecalculateHistogram();
}

void wxPLHistogramPlot::Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map )
{
	if ( m_histData.size() > 0 )
	{
		double xMin = map.ToDevice( wxRealPoint(m_dataMin, 0) ).x;
		double xMax = map.ToDevice( wxRealPoint(m_dataMax, 0) ).x;

		double usableLength = xMax - xMin;
		
		// try alpha blending for nicer plots
		wxColour col( m_fillColour.Red(), m_fillColour.Green(), m_fillColour.Blue(), 128 );
		dc.Pen( m_lineColour, 0.5 );
		dc.Brush( col );

		double xLoc = 0;
		wxRealPoint zero = map.ToDevice(wxRealPoint(0.0,0.0));
		for (size_t i=0; i<m_histData.size(); i++)
		{
			wxPLRealRect barRect;
			double nextXLoc = int((double)(i+1) * usableLength / (double)m_numberOfBins + 0.5); // .5 rounds.
			barRect.x = xMin + xLoc; 

			barRect.width = nextXLoc - xLoc + 1;
			xLoc = nextXLoc;

			barRect.y = map.ToDevice(wxPoint(0,m_histData[i])).y;
			barRect.height = zero.y - barRect.y + 1;

			dc.Rect( barRect.x, barRect.y, barRect.width, barRect.height );
		}
	}
}

void wxPLHistogramPlot::DrawInLegend(wxPLOutputDevice &dc, const wxPLRealRect &rct)
{
	wxColour col( m_fillColour.Red(), m_fillColour.Green(), m_fillColour.Blue(), 128 );
	dc.Pen( col );
	dc.Brush( col );
	dc.Rect(rct.x, rct.y, rct.width, rct.height);
}

wxPLAxis* wxPLHistogramPlot::SuggestXAxis()
{
	double xmin = 0, xmax = 0;
	GetMinMax(&xmin, &xmax, NULL, NULL);
	return new wxPLLinearAxis(xmin, xmax);
}

wxPLAxis* wxPLHistogramPlot::SuggestYAxis()
{
	double ymin = 0, ymax = 0;
	GetMinMax(NULL, NULL, &ymin, &ymax);
	return new wxPLLinearAxis(ymin, ymax);
}

bool wxPLHistogramPlot::GetMinMax(double *pxmin, double *pxmax, double *pymin, double *pymax) const
{
	if (pxmin)
		*pxmin = m_dataMin;
	if (pxmax)
		*pxmax = m_dataMax;
	if (pymin)
		*pymin = 0;
	if (pymax)
		*pymax = m_niceMax;

	return true;
}

std::vector<wxString> wxPLHistogramPlot::GetExportableDatasetHeaders( wxUniChar sep, wxPLPlot *plot ) const
{
	std::vector<wxString> tt;
	wxString xLabel = GetXDataLabel( plot );
	wxString yLabel = (GetNormalize() == 0 ? "Point Count" : "% of Points");

	if(xLabel.size() == 0) { xLabel = "Avg Bin Value"; }
			
	//Remove sep chars that we don't want
	while (xLabel.Find(sep) != wxNOT_FOUND)
	{
		xLabel = xLabel.BeforeFirst(sep) + xLabel.AfterFirst(sep);
	}

	while (yLabel.Find(sep) != wxNOT_FOUND)
	{
		yLabel = yLabel.BeforeFirst(sep) + yLabel.AfterFirst(sep);
	}

	tt.push_back(xLabel);
	tt.push_back(yLabel);

	return tt;
}

std::vector<wxRealPoint> wxPLHistogramPlot::GetExportableDataset(double Xmin, double Xmax, bool visible_only) const
{
	std::vector<wxRealPoint> data;

	for(int i = 0; i < m_numberOfBins; i++)
	{
		data.push_back(wxRealPoint((HistBinAt(i).x + HistBinAt(i).y) / 2.0, HistAt(i)));
	}

	return data;
}

void wxPLHistogramPlot::RecalculateHistogram()
{
	//This method builds histogram data. (basically, just groups and counts WPPlotData)
	//Histogram data is stored in a local array m_histData.
	//Uses WPPlotData to build this data.

	size_t index;
	size_t DataCount = 0;

	m_histData.clear();
	m_histDataBinRanges.clear();

	if ( m_data.size() < 2 || m_numberOfBins < 1) return;

	m_dataMin = m_data[0].y;
	m_dataMax = m_data[0].y;
	for ( size_t i=1;i<m_data.size();i++ )
	{
		if ( m_data[i].y < m_dataMin ) m_dataMin = m_data[i].y;
		if ( m_data[i].y > m_dataMax ) m_dataMax = m_data[i].y;
	}

	if ( m_dataMin == m_dataMax ) return;
	
	m_histData.resize( m_numberOfBins, 0.0 );
	m_histDataBinRanges.resize( m_numberOfBins, wxRealPoint(m_dataMax, m_dataMin) );

	for (size_t i=0; i<m_data.size(); i++)
	{
		if (!m_ignoreZeros || m_data[i].y != 0 ) DataCount++;
		if ( m_ignoreZeros && m_data[i].y == 0 ) continue;

		index = m_numberOfBins * (m_data[i].y - m_dataMin) / (m_dataMax - m_dataMin);
		if (index >= m_numberOfBins)
			index--;

		if ( index >= 0 && index < m_histData.size() )
		{
			m_histData[index]++;
			if(m_histDataBinRanges[index].x > m_data[i].y) { m_histDataBinRanges[index].x = m_data[i].y; }
			if(m_histDataBinRanges[index].y < m_data[i].y) { m_histDataBinRanges[index].y = m_data[i].y; }
		}
	}
	//We now have a histogram...

	m_niceMax = 0;
	if (m_normalize)
	{
		//Here we normalize it so that we show percent on the y axis instead of the count.
		for (size_t i=0; i<m_numberOfBins; i++)
		{
			m_histData[i] *= 100.0f / (double)DataCount;
			if (m_normalizeToPdf)
			{
				//This scales so total area is equal to 1 (like a pdf).
				double binWidth = (m_dataMax - m_dataMin) / ((double)m_numberOfBins);
				m_histData[i] /= binWidth;
			}
			if (m_histData[i] > m_niceMax)
				m_niceMax = m_histData[i];
		}
	}
	else
	{
		for (size_t i=0; i<m_histData.size(); i++)
		{
			if (m_histData[i] > m_niceMax)
				m_niceMax = m_histData[i];
		}
	}

	double order = floor(log10(m_niceMax));
	double interval = pow(10.0, order) / 2;
	m_niceMax += interval - fmod(m_niceMax, interval);
}

int wxPLHistogramPlot::GetSturgesBinsFor(int nDataPoints)
{
	return (int) ceil(log10(double(nDataPoints)) / log10(2.0) + 1); //Sturges formula.
}

int wxPLHistogramPlot::GetSqrtBinsFor(int nDataPoints)
{
	return (int) sqrt(double(nDataPoints));
}
