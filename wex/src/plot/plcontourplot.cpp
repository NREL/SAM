#include <wx/msgdlg.h>

#include "wex/plot/plcontourplot.h"
#include "wex/plot/plcolourmap.h"
#include "wex/clipper/clipper.h"

#include <wex/pdf/pdfdoc.h>
#include <wex/pdf/pdfshape.h>

#include <algorithm>
#include <limits>
#include <cmath>


wxPLContourPlot::wxPLContourPlot()
	: wxPLPlottable()
{
	m_cmap = 0;
}

wxPLContourPlot::wxPLContourPlot( 
	const wxMatrix<double> &x,
	const wxMatrix<double> &y,
	const wxMatrix<double> &z,
	bool filled,
	const wxString &label, int levels, wxPLColourMap *cmap )
	: wxPLPlottable( label ), m_x(x), m_y(y), m_z(z), m_filled(filled), m_cmap(cmap)
{
	m_zMin = m_zMax = std::numeric_limits<double>::quiet_NaN();

	if ( z.Cells() > 0 )
	{
		RebuildMask();
		RebuildLevels( levels );
		RebuildContours();
	}
}

wxPLContourPlot::~wxPLContourPlot()
{
	// nothing to do
}

#include "mplcontour.h"

void wxPLContourPlot::RebuildMask()
{	
	if ( m_z.Cells() == 0 ) return;

	if ( m_x.Rows() != m_y.Rows() 
		|| m_y.Rows() != m_z.Rows() ) return;
	
	if ( m_x.Cols() != m_y.Cols() 
		|| m_y.Cols() != m_z.Cols() ) return;

	m_cPolys.clear();

	size_t ny = m_z.Rows();
	size_t nx = m_z.Cols();
	m_zMin=1e99;
	m_zMax=-1e99;
	
	m_mask.Clear();
	for( size_t i=0;i<ny;i++ )
	{
		for( size_t j=0;j<nx;j++ )
		{
			double here = m_z(i,j);
			
			// create the mask if needed (i.e. if NaNs or Infs in the z data)
			if ( !std::isfinite( here ) )
			{
				if ( m_mask.Empty() )
					m_mask.ResizeFill( ny, nx, 0 );

				// mask out this cell if not finite
				m_mask(i,j) = 1;
			}
			else
			{				
				if ( here < m_zMin ) m_zMin = here;
				if ( here > m_zMax ) m_zMax = here;
			}
		}
	}
}

void wxPLContourPlot::RebuildLevels( int n, double min, double max )
{
	if ( !std::isfinite(m_zMin) || m_zMax <= m_zMin ) return;

	if ( min == max )
	{
		min = m_zMin;
		max = m_zMax;
	}

	m_levels.clear();
	for( int i=0;i<n;i++ )
		m_levels.push_back( min + ((double)i)/((double)n-1) * (max-min) );
}

void wxPLContourPlot::RebuildContours()
{
	if ( !std::isfinite(m_zMin) || m_zMax <= m_zMin || m_levels.size() < 2 ) return;
	
	QuadContourGenerator qcg( m_x, m_y, m_z, m_mask, true, 0 );	
	if ( !m_filled )
	{
		for( int k=0;k<m_levels.size();k++ )
		{
			double zval =  m_levels[k];
			std::vector<ContourLine*> list;
			qcg.create_contour( zval, list );
					
			for( size_t i=0;i<list.size();i++ )
			{
				m_cPolys.push_back( C_poly() );
				C_poly &CC = m_cPolys.back();
				CC.z = zval;
				std::vector<XY> &xy = *list[i];
				for( size_t j=0;j<xy.size();j++ )
					CC.pts.push_back( wxRealPoint( xy[j].x, xy[j].y ) );

				delete list[i]; // free the contour data
			}
		}
	}
	else
	{
		for( int k=0;k<m_levels.size()-1;k++ )
		{
			double zlow =  m_levels[k];
			double zhigh =  m_levels[k+1];

			std::vector<QuadContourGenerator::VertexCodes*> list;
			qcg.create_filled_contour( zlow, zhigh, list );
								
			for( size_t i=0;i<list.size();i++ )
			{
				QuadContourGenerator::VertexCodes &vc = *list[i];
				if ( vc.vertices.Rows() != vc.codes.Rows() ) continue;
				
				m_cPolys.push_back( C_poly() );
				C_poly &CC = m_cPolys.back();
				CC.z = zlow;
				CC.zmax = zhigh;
				size_t len = vc.vertices.Rows();
				CC.pts.reserve( len );
				CC.act.reserve( len );
				for( size_t j=0;j<len;j++ )
				{
					CC.pts.push_back( wxRealPoint( vc.vertices(j,0), vc.vertices(j,1) ) );
					CC.act.push_back( vc.codes(j,0) );
				}

				delete list[i]; // free the contour data

			}
		}
	}
}

void wxPLContourPlot::SetColourMap( wxPLColourMap *cmap )
{
	m_cmap = cmap;
}

void wxPLContourPlot::SetLevels( int levels, double min, double max )
{
	RebuildLevels( levels, min, max );
	RebuildContours();
}

void wxPLContourPlot::SetLevels( const std::vector<double> &lev )
{
	m_levels = lev;
	RebuildContours();
}


wxRealPoint wxPLContourPlot::At( size_t i ) const
{
	if ( i < m_x.Cells() && i < m_y.Cells() ) 
		return wxRealPoint( m_x.RawIndex(i), m_y.RawIndex(i) );
	else 
		return wxRealPoint( std::numeric_limits<double>::quiet_NaN(),
			std::numeric_limits<double>::quiet_NaN() );
}

size_t wxPLContourPlot::Len() const
{
	return m_x.Cells();
}

//#define DEBUG_DELAUNAY 1

#ifdef DEBUG_DELAUNAY
static std::vector<double> dbg_data_x;
static std::vector<double> dbg_data_y;
static wxMatrix<int> dbg_triangles;

static void draw_tri_mesh( wxPLOutputDevice &dc, const wxPLDeviceMapping &map )
{
	dc.Pen( *wxBLACK, 0.75 );
	dc.NoBrush();
	
	for( size_t i=0;i<dbg_triangles.Rows();i++ )
	{
		wxRealPoint pt[3];
		double solval = 0;
		for (int k=0;k<3;k++ )
		{
			int idx = dbg_triangles(i,k);
			pt[k] = map.ToDevice( dbg_data_x[idx], dbg_data_y[idx] );
		}
		dc.Polygon( 3, pt );
	}
}
#endif

	
void wxPLContourPlot::Draw( wxPLOutputDevice &dc, const wxPLDeviceMapping &map )
{
	if ( !m_cmap ) return;
	
	if ( !m_filled )
	{
		dc.NoBrush();
		for( size_t i=0;i<m_cPolys.size();i++ )
		{
			dc.Pen( m_cmap->ColourForValue( m_cPolys[i].z ), 2 );

			size_t n = m_cPolys[i].pts.size();
			std::vector<wxRealPoint> mapped( n );
			for( size_t j=0;j<n;j++ )
				mapped[j] = map.ToDevice( m_cPolys[i].pts[j].x, m_cPolys[i].pts[j].y );

			dc.Lines( m_cPolys[i].pts.size(), &mapped[0] ); 
		}
	}
	else
	{
		dc.NoPen();

		int ipoly = 0;
		for( size_t i=0;i<m_cPolys.size();i++ )
		{
			double zmid = 0.5*(m_cPolys[i].z + m_cPolys[i].zmax );
			wxColour color(m_cmap->ColourForValue( zmid ));
			dc.Pen( color, 2 );
			dc.Brush( color );

			size_t n = m_cPolys[i].pts.size();
			for( size_t j=0;j<n;j++ )
			{
				wxRealPoint mapped = map.ToDevice( m_cPolys[i].pts[j] );
				switch( m_cPolys[i].act[j] )
				{
				case MOVETO: 
					dc.MoveTo( mapped.x, mapped.y );
					break;
				case LINETO:
					dc.LineTo( mapped.x, mapped.y );
					break;
				case CLOSEPOLY:
					dc.LineTo( mapped.x, mapped.y );
					dc.CloseSubPath();
					break;
				}
			}

			dc.Path( wxPLOutputDevice::WINDING_RULE );
		}
	}

#ifdef DEBUG_DELAUNAY
	draw_tri_mesh( dc, map );
#endif
}

void wxPLContourPlot::DrawInLegend( wxPLOutputDevice &dc, const wxPLRealRect &rct)
{
	// currently nothing to show in legend...? 
}


void wxPLContourPlot::MinMax(	const std::vector<double> &v,
		double *minval, double *maxval )
{
	double min = 1e99, max = -1e99;
	for( size_t i=0;i<v.size();i++ )
	{
		if ( std::isfinite( v[i] ) )
		{
			if ( v[i] < min ) min = v[i];
			if ( v[i] > max ) max = v[i];
		}
	}

	if ( minval ) *minval = min;
	if ( maxval ) *maxval = max;
}

void wxPLContourPlot::MinMax(	const wxMatrix<double> &v,
		double *minval, double *maxval )
{
	double min = 1e99, max = -1e99;
	for( size_t i=0;i<v.Rows();i++ )
	{
		for( size_t j=0;j<v.Cols();j++ )
		{
			double here = v(i,j);
			if ( std::isfinite( here ) )
			{
				if ( here < min ) min = here;
				if ( here > max ) max = here;
			}
		}
	}

	if ( minval ) *minval = min;
	if ( maxval ) *maxval = max;
}

bool wxPLContourPlot::MeshGrid( 
		double xmin, double xmax, size_t nx,
		double ymin, double ymax, size_t ny,
	wxMatrix<double> &xmesh,
	wxMatrix<double> &ymesh )
{
	if ( xmax <= xmin || ymax <= ymin || nx < 2  || ny < 2 ) return false;

	double xstep = (xmax-xmin)/(nx-1);
	double ystep = (ymax-ymin)/(ny-1);

	xmesh.Resize( ny, nx );
	ymesh.Resize( ny, nx );

	for( size_t i=0;i<ny;i++ )
	{
		for( size_t j=0;j<nx;j++ )
		{
			xmesh(i,j) = xmin + j*xstep;
			ymesh(i,j) = ymin + i*ystep;
		}
	}

	return true;
}


void wxPLContourPlot::Peaks( size_t n,
	wxMatrix<double> &xx, wxMatrix<double> &yy, wxMatrix<double> &zz, 
	double *min, double *max )
{
	if ( min ) *min = 1e99;
	if ( max ) *max = -1e99;

	xx.Resize( n, n );
	yy.Resize( n, n );
	zz.Resize( n, n );

	for( size_t i=0;i<n;i++ )
	{
		for( size_t j=0;j<n;j++ )
		{
			double y = -3.0 + ((double)i)/((double)n-1)*6.0;
			double x = -3.0 + ((double)j)/((double)n-1)*6.0;

			double z =  3*(1-x)*(1-x)*exp(-(x*x) - (y+1)*(y+1))
			   - 10*(x/5 - x*x*x - y*y*y*y*y)*exp(-x*x-y*y)
			   - 1/3*exp(-(x+1)*(x+1) - y*y);

			if ( min && z < *min ) *min = z;
			if ( max && z > *max ) *max = z;
			
			xx(j,i) = x;
			yy(j,i) = y;
			zz(j,i) = z;
		}
	}
}


//#include "delaunay.h"
extern "C" {
	#include "qhull/qhull_a.h"
}

static const char* qhull_error_msg[6] = {
    "no error",             /* 0 = qh_ERRnone */
    "input inconsistency",  /* 1 = qh_ERRinput */
    "singular input data",  /* 2 = qh_ERRsingular */
    "precision error",      /* 3 = qh_ERRprec */
    "insufficient memory",  /* 4 = qh_ERRmem */
    "internal error"};      /* 5 = qh_ERRqhull */


/* Return the indices of the 3 vertices that comprise the specified facet (i.e.
 * triangle). */
static void get_facet_vertices(const facetT* facet, int indices[3])
{
    vertexT *vertex, **vertexp;
    FOREACHvertex_(facet->vertices)
        *indices++ = qh_pointid(vertex->point);
}

/* Return the indices of the 3 triangles that are neighbors of the specified
 * facet (triangle). */
static void get_facet_neighbours(const facetT* facet, const int* tri_indices,
                     int indices[3])
{
    facetT *neighbor, **neighborp;
    FOREACHneighbor_(facet)
        *indices++ = (neighbor->upperdelaunay ? -1 : tri_indices[neighbor->id]);
}

/* Delaunay implementation methyod.  If hide_qhull_errors is 1 then qhull error
 * messages are discarded; if it is 0 then they are written to stderr. */
static bool qhull_delaunay(int npoints, const double* x, const double* y,
			  wxMatrix<int> &triangles,
			  wxMatrix<int> &neighbors,
			  wxString *errstr )
{
    coordT* points = NULL;
    facetT* facet;
    int i, ntri, max_facet_id;
    int exitcode;               /* Value returned from qh_new_qhull(). */
    int* tri_indices = NULL;    /* Maps qhull facet id to triangle index. */
    int indices[3];
    int curlong, totlong;       /* Memory remaining after qh_memfreeshort. */
    const int ndim = 2;
    int* triangles_ptr;
    int* neighbors_ptr;

    /* Allocate points. */
    points = (coordT*)malloc(npoints*ndim*sizeof(coordT));
    if (points == NULL) {
        fprintf( stderr, "Could not allocate points array in qhull.delaunay" );
        goto error_before_qhull;
    }

    /* Prepare points array to pass to qhull. */
    for (i = 0; i < npoints; ++i) {
        points[2*i  ] = x[i];
        points[2*i+1] = y[i];
    }
    
    /* Perform Delaunay triangulation. */
	/* qhull expects a FILE* to write errors to, use stderr */
    exitcode = qh_new_qhull(ndim, npoints, points, False,
                            "qhull d Qt Qbb Qc Qz", NULL, stderr);
    if (exitcode != qh_ERRnone) {
        fprintf( stderr,
                     "Error in qhull Delaunay triangulation calculation: %s (exitcode=%d)",
                     qhull_error_msg[exitcode], exitcode );

		if ( errstr ) *errstr = wxString::Format("code %d: %s", exitcode, qhull_error_msg[exitcode]);
        goto error;
    }

    /* Split facets so that they only have 3 points each. */
    qh_triangulate();

    /* Determine ntri and max_facet_id.
       Note that libqhull uses macros to iterate through collections. */
    ntri = 0;
    FORALLfacets {
        if (!facet->upperdelaunay)
            ++ntri;
    }

    max_facet_id = qh facet_id - 1;

    /* Create array to map facet id to triangle index. */
    tri_indices = (int*)malloc((max_facet_id+1)*sizeof(int));
    if (tri_indices == NULL) {
		fprintf( stderr,"Could not allocate triangle map in qhull.delaunay");
		if ( errstr ) *errstr = "Could not allocate triangle map in qhull.delaunay";
        goto error;
    }

    /* Allocate python arrays to return. */
	triangles.Resize( ntri, 3 );
	neighbors.Resize( ntri, 3 );

    triangles_ptr = triangles.Data();
    neighbors_ptr = neighbors.Data();

    /* Determine triangles array and set tri_indices array. */
    i = 0;
    FORALLfacets {
        if (!facet->upperdelaunay) {
            tri_indices[facet->id] = i++;
            get_facet_vertices(facet, indices);
            *triangles_ptr++ = (facet->toporient ? indices[0] : indices[2]);
            *triangles_ptr++ = indices[1];
            *triangles_ptr++ = (facet->toporient ? indices[2] : indices[0]);
        }
        else
            tri_indices[facet->id] = -1;
    }

    /* Determine neighbors array. */
    FORALLfacets {
        if (!facet->upperdelaunay) {
            get_facet_neighbours(facet, tri_indices, indices);
            *neighbors_ptr++ = (facet->toporient ? indices[2] : indices[0]);
            *neighbors_ptr++ = (facet->toporient ? indices[0] : indices[2]);
            *neighbors_ptr++ = indices[1];
        }
    }

#ifdef DEBUG_DELAUNAY
	dbg_triangles = triangles;
	dbg_data_x.resize( npoints );
	dbg_data_y.resize( npoints );
	for( int i=0;i<npoints;i++ )
	{
		dbg_data_x[i] = x[i];
		dbg_data_y[i] = y[i];
	}
#endif

    /* Clean up. */
    qh_freeqhull(!qh_ALL);
    qh_memfreeshort(&curlong, &totlong);
    if (curlong || totlong)
        fprintf( stderr, "Qhull could not free all allocated memory", 1);

    free(tri_indices);
    free(points);

	return true;

error:
    qh_freeqhull(!qh_ALL);
    qh_memfreeshort(&curlong, &totlong);
    free(tri_indices);

error_before_qhull:
    free(points);

    return false;
}

static int search( const wxMatrix<int> &tri, const std::vector<double> &x, const std::vector<double> &y, double xq, double yq )
{
	// TODO: http://www.geom.uiuc.edu/~bradb/qhull3.1/html/qh-faq.htm#vclosest
	// this is a naive search - faster methods available for delaunay triangulation using the neighbors information
	for( size_t i=0;i<tri.Rows();i++ )
	{
		/*
		Get the vertices of triangle TRIANGLE.
		*/
		int a = tri(i,0);
		int b = tri(i,1);
		int c = tri(i,2);
		/*
		Using vertex C as a base, compute the distances to vertices A and B,
		and the point (X,Y).
		*/
		double dxa = x[a] - x[c];
		double dya = y[a] - y[c];

		double dxb = x[b] - x[c];
		double dyb = y[b] - y[c];

		double dxp = xq - x[c];
		double dyp = yq - y[c];

		double det = dxa * dyb - dya * dxb;
		/*
		Compute the barycentric coordinates of the point (X,Y) with respect
		to this triangle.
		*/
		double alpha = ( dxp * dyb - dyp * dxb ) / det;
		double beta =  ( dxa * dyp - dya * dxp ) / det;
		double gamma = 1.0 - alpha - beta;
		/*
		If the barycentric coordinates are all positive, then the point
		is inside the triangle and we're done.
		*/
		if ( 0.0 <= alpha &&
			0.0 <= beta  &&
			0.0 <= gamma )
		{
			return i;
		}
	}

	return -1;
}

#include <wx/msgdlg.h>

bool wxPLContourPlot::GridData( 
			const std::vector<double> &x, 
			const std::vector<double> &y,
			const std::vector<double> &z,
			const wxMatrix<double> &xq,
			const wxMatrix<double> &yq,
		wxMatrix<double> &zinterp )
{
	if ( x.size() != y.size() || y.size() != z.size() ) return false;
	if ( xq.Rows() != yq.Rows() || xq.Cols() != yq.Cols() ) return false;

	size_t len = x.size();

	wxMatrix<int> triangles;
	wxMatrix<int> neighbors;
	wxString errstr;
	if ( !qhull_delaunay( len, &x[0], &y[0], triangles, neighbors, &errstr ) )
	{
		wxString dbgfile( wxGetHomeDir() + "/qhull_debug.csv" );
		if ( FILE *fp = fopen( dbgfile.c_str(), "w" ) )
		{
			fprintf(fp, "x,y\n" );
			for( size_t i=0;i<len;i++ )
				fprintf(fp, "%lg,%lg\n", x[i], y[i]);
			fclose(fp);
		}
		
		wxMessageBox( "Error in qhull delaunay: " + errstr + "\n\n" + wxString::Format( "xyz data vector length=%d", (int)len)+ "\nwrote debug: " + dbgfile );
		return false;
	}

	zinterp.Resize( xq.Rows(), xq.Cols() );

	for( size_t i=0;i<xq.Rows();i++ )
	{
		for( size_t j=0;j<xq.Cols();j++ )
		{
			double xqq = xq(i,j);
			double yqq = yq(i,j);
			double zqq = std::numeric_limits<double>::quiet_NaN();

			int index = search( triangles, x, y, xqq, yqq );
			if ( index >= 0 )
			{
				int a = triangles(index,0);
				int b = triangles(index,1);
				int c = triangles(index,2);

				double d1 = sqrt( pow( xqq - x[a], 2 ) + pow( yqq - y[a], 2 ) );
				if ( d1 == 0.0 ) zqq = z[a];

				double d2 = sqrt( pow( xqq - x[b], 2 ) + pow( yqq - y[b], 2 ) );
				if ( d2 == 0.0 ) zqq = z[b];

				double d3 = sqrt( pow( xqq - x[c], 2 ) + pow( yqq - y[c], 2 ) );
				if ( d3 == 0.0 ) zqq = z[c];
				
				// calculate interpolated Z value
				if ( !std::isfinite(zqq) )
				{
					d1 = 1.0/d1;
					d2 = 1.0/d2;
					d3 = 1.0/d3;
					zqq = ( d1*z[a] + d2*z[b] + d3*z[c] ) / ( d1 + d2 + d3 );
				}				
			}

			zinterp(i,j) = zqq;
		}
	}

	return true;
}
