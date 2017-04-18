#ifndef __pl_contourplot_h
#define __pl_contourplot_h

#include "wex/matrix.h"
#include "wex/plot/plplot.h"

class wxPLColourMap;

class wxPLContourPlot : public wxPLPlottable
{
public:
	wxPLContourPlot();
	wxPLContourPlot( 
		const wxMatrix<double> &x,
		const wxMatrix<double> &y,
		const wxMatrix<double> &z,
		bool filled,
		const wxString &label = wxEmptyString,
		int levels = 10,
		wxPLColourMap *cmap = 0 );

	virtual ~wxPLContourPlot();

	static void MinMax(	const std::vector<double> &v,
			double *minval, double *maxval );
	static void MinMax(	const wxMatrix<double> &v,
			double *minval, double *maxval );

	static bool MeshGrid( 
			double xmin, double xmax, size_t nx,
			double ymin, double ymax, size_t ny,
		wxMatrix<double> &xmesh,
		wxMatrix<double> &ymesh );

	static bool GridData( 
			const std::vector<double> &x, 
			const std::vector<double> &y,
			const std::vector<double> &z,
			const wxMatrix<double> &xq,
			const wxMatrix<double> &yq,
		wxMatrix<double> &zinterp  );

	
	static void Peaks( size_t n,
		wxMatrix<double> &xx, wxMatrix<double> &yy, wxMatrix<double> &zz,
		double *min, double *max );


	void SetLevels( int levels, double min = 0, double max = 0 );
	void SetLevels( const std::vector<double> &lev );
	void SetColourMap( wxPLColourMap *cmap ); // does not take ownership of colour map

	virtual wxRealPoint At( size_t i ) const;
	virtual size_t Len() const;
	
	virtual void Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map );
	virtual void DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct);

protected:
	wxMatrix<double> m_x, m_y, m_z;
	wxMatrix<unsigned int> m_mask;
	double m_zMin, m_zMax;
	wxPLColourMap *m_cmap;
	bool m_filled;
	std::vector<double> m_levels;

	void RebuildMask();
	void RebuildLevels( int n, double min=0, double max=0 );
	void RebuildContours();
	
	struct C_poly {
		std::vector< wxRealPoint > pts;
		std::vector< unsigned char > act;
		double z, zmax;
	};

	std::vector<C_poly> m_cPolys;
};


#endif
