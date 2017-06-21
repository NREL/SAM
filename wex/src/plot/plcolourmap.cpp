#include <wx/dcmemory.h>
#include "wex/plot/plaxis.h"
#include "wex/plot/plcolourmap.h"

wxPLColourMap::wxPLColourMap(double min, double max)
{
	m_min = min;
	m_max = max;
	m_format = "%lg";
	m_reversed = false;
}

wxPLColourMap::wxPLColourMap( const wxPLColourMap &cpy )
{
	Copy( cpy );
}

wxPLColourMap::~wxPLColourMap()
{
	// nothing to do
}

wxPLColourMap &wxPLColourMap::operator=( const wxPLColourMap &cpy )
{
	if ( this != &cpy )
		Copy( cpy );

	return *this;
}

void wxPLColourMap::Copy( const wxPLColourMap &cpy )
{
	m_min = cpy.m_min;
	m_max = cpy.m_max;
	m_format = cpy.m_format;
	m_colourList = cpy.m_colourList;
}


void wxPLColourMap::SetScaleMinMax(double min, double max)
{
	m_min = min;
	m_max = max;
	InvalidateBestSize();
}

void wxPLColourMap::SetScaleMin(double min)
{
	m_min = min;
	InvalidateBestSize();
}

void wxPLColourMap::SetScaleMax(double max)
{
	m_max = max;
	InvalidateBestSize();
}

void wxPLColourMap::SetFormat( const wxString &fmt )
{
	m_format = fmt;
	InvalidateBestSize();
}

void wxPLColourMap::SetLabels( const wxArrayString &l )
{
	m_labels = l;
	InvalidateBestSize();
}

double wxPLColourMap::GetScaleMin()
{
	return m_min;
}

double wxPLColourMap::GetScaleMax()
{
	return m_max;
}

void wxPLColourMap::ExtendScaleToNiceNumbers()
{
	wxPLAxis::ExtendBoundsToNiceNumber(&m_max, &m_min);
}

wxRealPoint wxPLColourMap::CalculateBestSize( wxPLOutputDevice &dc )
{
	double range = m_max - m_min;
	double step = range / 10;
	double maxWidth = 0, width, height;
	if ( m_labels.size() > 0 )
	{
		for( size_t i=0;i<m_labels.size();i++ )
		{
			dc.Measure( m_labels[i], &width, &height );
			if ( width > maxWidth )
				maxWidth = width;
		}
	}
	else
	{
		for (int i=0; i<11; i++)
		{
			dc.Measure( wxString::Format( wxFormatString(m_format), m_min + i*step), &width, &height );
			if (width > maxWidth)
				maxWidth = width;
		}
	}

	return wxRealPoint( 17.0+maxWidth, 300.0 );
}

void wxPLColourMap::Render( wxPLOutputDevice &dc, const wxPLRealRect &geom)
{
	double colourBarHeight = 240;
	if (geom.height < 240)
		colourBarHeight = 120; //Probably not ideal.  Fix this.
		
	dc.TextPoints( -1 );

	double charHeight;
	dc.Measure( "0", NULL, &charHeight );
	
	double colourBarX = geom.x+1;
	double colourBarStep = colourBarHeight / ((double)m_colourList.size());
	for (size_t i=0; i<m_colourList.size(); i++)
	{
		size_t idx =  m_reversed ? m_colourList.size() - i - 1 : i;
		dc.Pen( m_colourList[idx] );
		dc.Brush( m_colourList[idx] );
		dc.Rect(colourBarX+1, geom.y+charHeight/2 + 1 + (m_colourList.size()-1-i)*colourBarStep, 10, colourBarStep+1);
	}
		
	double xTextPos = colourBarX + 14;

	if ( m_labels.size() < 2 )
	{
		double yTextStep = colourBarHeight / 10;
		double step = (m_max - m_min) / 10;
		for (size_t i=0; i<11; i++)
			dc.Text( wxString::Format( wxFormatString(m_format), m_min + i*step), 
				xTextPos, geom.y+wxCoord((10-i)*yTextStep) );
	}
	else
	{
		double yTextStep = colourBarHeight / (m_labels.size()-1.0);
		double step = (m_max - m_min) / ( m_labels.size() - 1.0 );
		for( size_t i=0;i<m_labels.size();i++ )
			dc.Text( m_labels[i], xTextPos, geom.y + wxCoord( m_labels.size()-1-i)*yTextStep );
	}
}

wxColour wxPLColourMap::ColourForValue(double val)
{
	if ( m_colourList.size() == 0 || !(wxFinite(val)) ) return *wxBLACK;

	int position = 
		(int)( ((double)m_colourList.size()) * (
			m_reversed 
				? 1.0 - (val - m_min) / (m_max - m_min)
				: (val - m_min) / (m_max - m_min) 
				) );
	
	if ( position >= 0 && position < m_colourList.size() )
		return m_colourList[position];	
	else if ( position < 0 )
		return m_colourList.front();
	else
		return m_colourList.back();

}



wxPLCoarseRainbowColourMap::wxPLCoarseRainbowColourMap( double min, double max )
	: wxPLColourMap( min, max )
{
	m_colourList.push_back(wxColour(0, 0, 0));
	m_colourList.push_back(wxColour(46, 44, 213));
	m_colourList.push_back(wxColour(0, 108, 255));
	m_colourList.push_back(wxColour(0, 245, 245));
	m_colourList.push_back(wxColour(23, 210, 135));
	m_colourList.push_back(wxColour(104, 186, 44));
	m_colourList.push_back(wxColour(226, 241, 6));
	m_colourList.push_back(wxColour(255, 213, 0));
	m_colourList.push_back(wxColour(255, 159, 0));
	m_colourList.push_back(wxColour(255, 0, 0));
}

wxString wxPLCoarseRainbowColourMap::GetName()
{
	return _("Coarse Rainbow");
}

wxPLFineRainbowColourMap::wxPLFineRainbowColourMap( double min, double max )
	: wxPLColourMap( min, max )
{
	//Color Transition resolution: number of colors to use per transition.
	//In most cases we use this number.
	//In some cases we gorw or shrink the transition lenght to get a better selection of colours.
	//For instance, orange is between red and yellow, and the green transition is too long (loss of contrast).
	int res = 20;

	// Black to Violet
	for (int i=0; i<res; i++)
		m_colourList.push_back(wxColour(i * 255 / res, 0, i * 255 / res));
	
	// Violet to Dark Blue
	for (int i=res-1; i>res/2; i--)
		m_colourList.push_back(wxColour(i * 255 / (res/2), 0, 255));
	
	// Dark Blue to Light Blue
	for (int i=0; i<=res/2-1; i++)
		m_colourList.push_back(wxColour(0, 255 * i / (res/2), 255));
	
	//Light Blue to Green
	for (int i=res/2-1; i>0; i--)
		m_colourList.push_back(wxColour(0, 255, i * 255 / (res/2)));
	
	//Green to Yellow
	for (int i=0; i<res/2; i++)
		m_colourList.push_back(wxColour(i * 255 / (res/2), 255, 0));

	//Yellow to Orange to Red
	for (int i=2*res; i>=0; i--)
		m_colourList.push_back(wxColour(255, i * 255 / (2*res), 0));
}

wxString wxPLFineRainbowColourMap::GetName()
{
	return _("Fine Rainbow");
}

static double parula [] = {
   53.0655,   42.4065,  134.9460,
   53.6930,   46.2160,  142.7768,
   54.0971,   50.0550,  150.7135,
   54.1504,   53.9487,  158.7236,
   53.7378,   57.9306,  166.8357,
   52.6714,   62.0253,  175.0026,
   50.7125,   66.2258,  183.2500,
   47.5583,   70.5345,  191.5266,
   42.7195,   75.1292,  199.9092,
   35.7765,   80.2601,  208.2724,
   26.1383,   86.0610,  215.9549,
   15.0790,   91.7575,  221.4250,
    5.7004,   96.5384,  224.2261,
    1.8715,  100.3981,  225.1627,
    1.3971,  103.7556,  225.1812,
    2.7076,  106.7685,  224.6295,
    4.9030,  109.5658,  223.7780,
    7.5758,  112.2386,  222.6869,
   10.4457,  114.7964,  221.4605,
   13.0498,  117.2923,  220.1469,
   15.2351,  119.7495,  218.7436,
   17.0363,  122.1798,  217.3365,
   18.4280,  124.6100,  215.9085,
   19.4797,  127.0719,  214.5354,
   20.0870,  129.5933,  213.2426,
   20.2524,  132.2214,  212.1013,
   19.8467,  134.9885,  211.1500,
   18.7170,  137.9457,  210.5442,
   16.9351,  141.0930,  210.1841,
   14.6246,  144.3771,  209.9902,
   12.0708,  147.6543,  209.7792,
    9.6251,  150.8255,  209.3604,
    7.8563,  153.7712,  208.5807,
    6.7575,  156.4935,  207.4425,
    6.2506,  158.9855,  205.9535,
    6.0358,  161.2620,  204.1391,
    5.9044,  163.3669,  202.0805,
    5.8240,  165.3111,  199.7948,
    5.9005,  167.1455,  197.3600,
    6.4700,  168.8819,  194.7528,
    7.8277,  170.5625,  192.0335,
   10.2070,  172.1644,  189.1930,
   13.4269,  173.7106,  186.2682,
   17.2565,  175.2213,  183.2082,
   21.4965,  176.6725,  180.0725,
   26.1166,  178.0943,  176.8054,
   31.0544,  179.4736,  173.4456,
   36.2842,  180.8004,  169.9776,
   41.8130,  182.0978,  166.4292,
   47.6309,  183.3458,  162.7642,
   53.7501,  184.5481,  159.0002,
   60.1800,  185.6701,  155.1814,
   66.9298,  186.7373,  151.2807,
   73.9662,  187.7109,  147.3413,
   81.2731,  188.5748,  143.4259,
   88.7825,  189.3205,  139.5530,
   96.4186,  189.9225,  135.7983,
  104.0840,  190.3807,  132.2129,
  111.6521,  190.7137,  128.8237,
  119.0502,  190.9170,  125.5991,
  126.2482,  191.0205,  122.5646,
  133.2244,  191.0429,  119.6661,
  139.9540,  190.9772,  116.8850,
  146.4905,  190.8466,  114.2238,
  152.8586,  190.6496,  111.6838,
  159.0296,  190.4162,  109.1987,
  165.0615,  190.1280,  106.7940,
  170.9582,  189.7996,  104.4441,
  176.7413,  189.4418,  102.1453,
  182.3969,  189.0477,   99.8905,
  187.9721,  188.6204,   97.6573,
  193.4415,  188.1606,   95.4496,
  198.8560,  187.7009,   93.2674,
  204.1893,  187.2411,   91.0852,
  209.4694,  186.7813,   88.8775,
  214.6938,  186.3215,   86.6444,
  219.8826,  185.8811,   84.3725,
  225.0205,  185.4955,   82.0335,
  230.1352,  185.1903,   79.5739,
  235.2205,  185.0280,   76.9497,
  240.2510,  185.1122,   74.0543,
  245.1269,  185.6562,   70.7277,
  249.5252,  186.9699,   66.8208,
  252.7946,  189.2687,   62.5059,
  254.4320,  192.3164,   58.3579,
  254.7465,  195.6337,   54.6790,
  254.2304,  199.0090,   51.4536,
  253.2475,  202.3564,   48.4801,
  251.9400,  205.6830,   45.7385,
  250.4718,  209.0073,   43.1089,
  248.9611,  212.3570,   40.5473,
  247.4705,  215.7779,   37.9757,
  246.1909,  219.3340,   35.3484,
  245.2196,  223.0647,   32.5774,
  244.6060,  227.0172,   29.7214,
  244.4608,  231.2340,   26.7735,
  244.8510,  235.7313,   23.7451,
  245.7798,  240.5067,   20.6365,
  247.2380,  245.5163,   17.3640,
  248.9565,  250.6905,   13.7190
  };

  
 static double jet [] = {
	     0,         0,  132.6000,
         0,         0,  142.8000,
         0,         0,  153.0000,
         0,         0,  163.2000,
         0,         0,  173.4000,
         0,         0,  183.6000,
         0,         0,  193.8000,
         0,         0,  204.0000,
         0,         0,  214.2000,
         0,         0,  224.4000,
         0,         0,  234.6000,
         0,         0,  244.8000,
         0,         0,  255.0000,
         0,   10.2000,  255.0000,
         0,   20.4000,  255.0000,
         0,   30.6000,  255.0000,
         0,   40.8000,  255.0000,
         0,   51.0000,  255.0000,
         0,   61.2000,  255.0000,
         0,   71.4000,  255.0000,
         0,   81.6000,  255.0000,
         0,   91.8000,  255.0000,
         0,  102.0000,  255.0000,
         0,  112.2000,  255.0000,
         0,  122.4000,  255.0000,
         0,  132.6000,  255.0000,
         0,  142.8000,  255.0000,
         0,  153.0000,  255.0000,
         0,  163.2000,  255.0000,
         0,  173.4000,  255.0000,
         0,  183.6000,  255.0000,
         0,  193.8000,  255.0000,
         0,  204.0000,  255.0000,
         0,  214.2000,  255.0000,
         0,  224.4000,  255.0000,
         0,  234.6000,  255.0000,
         0,  244.8000,  255.0000,
         0,  255.0000,  255.0000,
   10.2000,  255.0000,  244.8000,
   20.4000,  255.0000,  234.6000,
   30.6000,  255.0000,  224.4000,
   40.8000,  255.0000,  214.2000,
   51.0000,  255.0000,  204.0000,
   61.2000,  255.0000,  193.8000,
   71.4000,  255.0000,  183.6000,
   81.6000,  255.0000,  173.4000,
   91.8000,  255.0000,  163.2000,
  102.0000,  255.0000,  153.0000,
  112.2000,  255.0000,  142.8000,
  122.4000,  255.0000,  132.6000,
  132.6000,  255.0000,  122.4000,
  142.8000,  255.0000,  112.2000,
  153.0000,  255.0000,  102.0000,
  163.2000,  255.0000,   91.8000,
  173.4000,  255.0000,   81.6000,
  183.6000,  255.0000,   71.4000,
  193.8000,  255.0000,   61.2000,
  204.0000,  255.0000,   51.0000,
  214.2000,  255.0000,   40.8000,
  224.4000,  255.0000,   30.6000,
  234.6000,  255.0000,   20.4000,
  244.8000,  255.0000,   10.2000,
  255.0000,  255.0000,         0,
  255.0000,  244.8000,         0,
  255.0000,  234.6000,         0,
  255.0000,  224.4000,         0,
  255.0000,  214.2000,         0,
  255.0000,  204.0000,         0,
  255.0000,  193.8000,         0,
  255.0000,  183.6000,         0,
  255.0000,  173.4000,         0,
  255.0000,  163.2000,         0,
  255.0000,  153.0000,         0,
  255.0000,  142.8000,         0,
  255.0000,  132.6000,         0,
  255.0000,  122.4000,         0,
  255.0000,  112.2000,         0,
  255.0000,  102.0000,         0,
  255.0000,   91.8000,         0,
  255.0000,   81.6000,         0,
  255.0000,   71.4000,         0,
  255.0000,   61.2000,         0,
  255.0000,   51.0000,         0,
  255.0000,   40.8000,         0,
  255.0000,   30.6000,         0,
  255.0000,   20.4000,         0,
  255.0000,   10.2000,         0,
  255.0000,         0,         0,
  244.8000,         0,         0,
  234.6000,         0,         0,
  224.4000,         0,         0,
  214.2000,         0,         0,
  204.0000,         0,         0,
  193.8000,         0,         0,
  183.6000,         0,         0,
  173.4000,         0,         0,
  163.2000,         0,         0,
  153.0000,         0,         0,
  142.8000,         0,         0,
  132.6000,         0,         0 };

wxPLJetColourMap::wxPLJetColourMap( double min, double max )
	: wxPLColourMap( min, max )
{
	size_t nc = sizeof(jet)/sizeof(double)/3;
	for( size_t i=0;i<nc;i++ )
	{
		int r = (int)jet[3*i];
		int g = (int)jet[3*i+1];
		int b = (int)jet[3*i+2];
		m_colourList.push_back( wxColour(r,g,b) );
	}
}
wxString wxPLJetColourMap::GetName()
{
	return _("Jet");
}

wxPLParulaColourMap::wxPLParulaColourMap( double min, double max )
	: wxPLColourMap( min, max )
{
	size_t nc = sizeof(parula)/sizeof(double)/3;
	for( size_t i=0;i<nc;i++ )
	{
		int r = (int)parula[3*i];
		int g = (int)parula[3*i+1];
		int b = (int)parula[3*i+2];
		m_colourList.push_back( wxColour(r,g,b) );
	}
}

wxString wxPLParulaColourMap::GetName()
{
	return _("Parula");
}


wxPLGrayscaleColourMap::wxPLGrayscaleColourMap( double min, double max )
	: wxPLColourMap( min, max )
{
	const size_t ncolours = 50;
	for ( size_t i=0;i<ncolours;i++ )
	{
		int grey = (int)(((double)i)/ncolours*255.0);
		m_colourList.push_back( wxColour( grey, grey, grey ) );
	}
}

wxString wxPLGrayscaleColourMap::GetName()
{
	return _("Grayscale");
}
