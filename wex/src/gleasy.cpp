#include <math.h>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <limits>

#include <wx/window.h>
#include <wx/filedlg.h>
#include <wx/menu.h>
#include <wx/dc.h>
#include <wx/dcbuffer.h>
#include <wx/dcgraph.h>
#include <wx/msgdlg.h>
#include <wx/stopwatch.h>
#include <wx/clipbrd.h>
#include <wx/busyinfo.h>

#include <wex/plot/plaxis.h>
#include <wex/plot/plplot.h>
#include <wex/gleasy.h>

#ifdef __WXMSW__

static void *GetGLFuncAddress( const char *name )
{
  void *p = (void *)wglGetProcAddress(name);

  if(p == 0 ||
    (p == (void*)0x1) || (p == (void*)0x2) || (p == (void*)0x3) ||
    (p == (void*)-1) )
  {
    HMODULE module = LoadLibraryA("opengl32.dll");
    p = (void *)GetProcAddress(module, name);
  }

  return p;
}

#ifndef APIENTRY
#define APIENTRY
#endif
#ifndef APIENTRYP
#define APIENTRYP APIENTRY *
#endif
#ifndef GLAPI
#define GLAPI extern
#endif

typedef void (APIENTRYP PFNGLWINDOWPOS2FPROC) (GLfloat x, GLfloat y);
static PFNGLWINDOWPOS2FPROC glWindowPos2f = NULL;

#elif defined(__WXGTK__)

static PFNGLWINDOWPOS2FPROC glWindowPos2f = NULL;

#endif

/*
GLubyte space[] =
    {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
GLubyte letters[][13] = {
    {0x00, 0x00, 0xc3, 0xc3, 0xc3, 0xc3, 0xff, 0xc3, 0xc3, 0xc3, 0x66, 0x3c, 0x18},
    {0x00, 0x00, 0xfe, 0xc7, 0xc3, 0xc3, 0xc7, 0xfe, 0xc7, 0xc3, 0xc3, 0xc7, 0xfe},
    {0x00, 0x00, 0x7e, 0xe7, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xe7, 0x7e},
    {0x00, 0x00, 0xfc, 0xce, 0xc7, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc7, 0xce, 0xfc},
    {0x00, 0x00, 0xff, 0xc0, 0xc0, 0xc0, 0xc0, 0xfc, 0xc0, 0xc0, 0xc0, 0xc0, 0xff},
    {0x00, 0x00, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xfc, 0xc0, 0xc0, 0xc0, 0xff},
    {0x00, 0x00, 0x7e, 0xe7, 0xc3, 0xc3, 0xcf, 0xc0, 0xc0, 0xc0, 0xc0, 0xe7, 0x7e},
    {0x00, 0x00, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xff, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3},
    {0x00, 0x00, 0x7e, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x7e},
    {0x00, 0x00, 0x7c, 0xee, 0xc6, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06},
    {0x00, 0x00, 0xc3, 0xc6, 0xcc, 0xd8, 0xf0, 0xe0, 0xf0, 0xd8, 0xcc, 0xc6, 0xc3},
    {0x00, 0x00, 0xff, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0},
    {0x00, 0x00, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xdb, 0xff, 0xff, 0xe7, 0xc3},
    {0x00, 0x00, 0xc7, 0xc7, 0xcf, 0xcf, 0xdf, 0xdb, 0xfb, 0xf3, 0xf3, 0xe3, 0xe3},
    {0x00, 0x00, 0x7e, 0xe7, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xe7, 0x7e},
    {0x00, 0x00, 0xc0, 0xc0, 0xc0, 0xc0, 0xc0, 0xfe, 0xc7, 0xc3, 0xc3, 0xc7, 0xfe},
    {0x00, 0x00, 0x3f, 0x6e, 0xdf, 0xdb, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0x66, 0x3c},
    {0x00, 0x00, 0xc3, 0xc6, 0xcc, 0xd8, 0xf0, 0xfe, 0xc7, 0xc3, 0xc3, 0xc7, 0xfe},
    {0x00, 0x00, 0x7e, 0xe7, 0x03, 0x03, 0x07, 0x7e, 0xe0, 0xc0, 0xc0, 0xe7, 0x7e},
    {0x00, 0x00, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0xff},
    {0x00, 0x00, 0x7e, 0xe7, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3},
    {0x00, 0x00, 0x18, 0x3c, 0x3c, 0x66, 0x66, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3},
    {0x00, 0x00, 0xc3, 0xe7, 0xff, 0xff, 0xdb, 0xdb, 0xc3, 0xc3, 0xc3, 0xc3, 0xc3},
    {0x00, 0x00, 0xc3, 0x66, 0x66, 0x3c, 0x3c, 0x18, 0x3c, 0x3c, 0x66, 0x66, 0xc3},
    {0x00, 0x00, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x3c, 0x3c, 0x66, 0x66, 0xc3},
    {0x00, 0x00, 0xff, 0xc0, 0xc0, 0x60, 0x30, 0x7e, 0x0c, 0x06, 0x03, 0x03, 0xff}
};


void wxGLEasyCanvas::makeRasterFont(void)
{
   GLuint i, j;
   glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

   m_fontOffset = glGenLists (128);
   for (i = 0,j = 'A'; i < 26; i++,j++) {
      glNewList(m_fontOffset + j, GL_COMPILE);
      glBitmap(8, 13, 0.0, 2.0, 10.0, 0.0, letters[i]);
      glEndList();
   }
   glNewList(m_fontOffset + ' ', GL_COMPILE);
   glBitmap(8, 13, 0.0, 2.0, 10.0, 0.0, space);
   glEndList();
}

void wxGLEasyCanvas::printString(const char *s)
{
	if (!s) return;
   glPushAttrib (GL_LIST_BIT);
   glListBase(m_fontOffset);
   glCallLists(strlen(s), GL_UNSIGNED_BYTE, (GLubyte *) s);
   glPopAttrib ();
}*/

template <typename T> T CLAMP(T value, T low, T high)
{
    return (value < low) ? low : ((value > high) ? high : value);
}


BEGIN_EVENT_TABLE( wxGLEasyCanvas, wxGLCanvas )
    EVT_SIZE(wxGLEasyCanvas::OnSize)
    EVT_PAINT(wxGLEasyCanvas::OnPaint)
    EVT_CHAR(wxGLEasyCanvas::OnChar)
    EVT_MOUSE_EVENTS(wxGLEasyCanvas::OnMouse)
	EVT_MENU( wxID_COPY, wxGLEasyCanvas::OnMenu )
	EVT_MENU( wxID_SAVE, wxGLEasyCanvas::OnMenu )
END_EVENT_TABLE()

wxGLEasyCanvas::wxGLEasyCanvas( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxGLCanvas( parent, id, NULL, pos, size, wxBORDER_NONE ), m_glContext( this )
{
	SetBackgroundColour( *wxWHITE );

	m_showStatus = false;
	m_antiAliasing = true;
	m_pointListMode = false;
	m_lastX = m_lastY = 0;
	m_scale.x = m_scale.y = m_scale.z = 1.0f;
	m_zoom = 1.0f;
	m_zoomMin = 0.001f;
	m_zoomMax = 1000.0f;
	m_zoomRate = 0.05f;
	
	m_orth.top = 100.0f;
	m_orth.left = -m_orth.top;
	m_orth.right = m_orth.top;
	m_orth.bottom = -m_orth.top;
	m_orth.znear = -1e10;
	m_orth.zfar = 1e10;
}

wxGLEasyCanvas::~wxGLEasyCanvas()
{
	// nothing to do (yet)
}


wxBitmap wxGLEasyCanvas::GetBitmap()
{
	SetCurrent( m_glContext );

	// Read the OpenGL image into a pixel array 
	GLint view[4]; 
	glGetIntegerv(GL_VIEWPORT, view); 
	void* pixels = malloc(3 * view[2] * view[3]); // must use malloc 
	
	glPixelStorei(GL_PACK_ALIGNMENT, 1); 
	glReadBuffer( GL_BACK_LEFT ); 	
	glReadPixels(0, 0, view[2], view[3], GL_RGB, GL_UNSIGNED_BYTE, pixels); 
  
	// Put the image into a wxImage 
	wxImage image((int) view[2], (int) view[3]); 
	image.SetData((unsigned char*) pixels); 

	image = image.Mirror(false); 
	return wxBitmap(image);
}


void wxGLEasyCanvas::OnRender()
{
	// nothing to do here
}



void wxGLEasyCanvas::Color( const wxColour &c )
{
	glColor3f( float(c.Red())/255.0f, 
		float(c.Green())/255.0f, 
		float(c.Blue())/255.0f );
}

void wxGLEasyCanvas::PointSize( float p )
{
	glPointSize( p );
}

void wxGLEasyCanvas::Point( float x, float y, float z )
{
	if ( m_pointListMode ) glVertex3f( x, y, z );
	else
	{
		glBegin( GL_POINT );
		glVertex3f( x, y, z );
		glEnd( );
	}
}

void wxGLEasyCanvas::Point( const wxGLPoint3D &p )
{
	glVertex3f( p.x, p.y, p.z );
}

void wxGLEasyCanvas::BeginPoints()
{
	if ( !m_pointListMode )
	{
		m_pointListMode = true;
		glBegin( GL_POINTS );
	}
}

void wxGLEasyCanvas::EndPoints()
{
	if ( m_pointListMode )
	{
		m_pointListMode = false;
		glEnd();
	}
}

void wxGLEasyCanvas::Points( const std::vector<wxGLPoint3D> &list )
{
	BeginPoints();
	for( std::vector<wxGLPoint3D>::const_iterator it = list.begin();
		it != list.end();
		++it )
		glVertex3f( it->x, it->y, it->z );

	EndPoints();
}

void wxGLEasyCanvas::LineWidth( float f )
{
	glLineWidth( f );
}

void wxGLEasyCanvas::Line( const wxGLPoint3D &p1, const wxGLPoint3D &p2 )
{
	glBegin( GL_LINES );
	glVertex3f( p1.x, p1.y, p1.z );
	glVertex3f( p2.x, p2.y, p2.z );
	glEnd();
}

void wxGLEasyCanvas::Lines( const std::vector<wxGLPoint3D> &list )
{
	glBegin( GL_LINE_STRIP );
	for( std::vector<wxGLPoint3D>::const_iterator it = list.begin();
		it != list.end();
		++it )
		glVertex3f( it->x, it->y, it->z );
	glEnd();
}

void wxGLEasyCanvas::Text( int x, int y, const wxString &text, 
		const wxColour &textcolor, const wxBrush &back, const wxFont *font )
{
	wxGLPoint3D p( x, y, std::numeric_limits<float>::quiet_NaN() );
	Text( p, text, textcolor, back, font );
}


void wxGLEasyCanvas::Text( const wxGLPoint3D &p, const wxString &text, 
		const wxColour &textcolor, const wxBrush &back, const wxFont *font )
{
	if ( text.IsEmpty() ) return;
	
#ifdef _WIN32
	if ( !std::isfinite( p.z ) && glWindowPos2f == NULL )
	{
		glWindowPos2f = (PFNGLWINDOWPOS2FPROC) GetGLFuncAddress( "glWindowPos2f" );
		if ( !glWindowPos2f )
			return; // unsupported glWindowPos2f
	}
#elif defined(__WXGTK__)
	if ( !std::isfinite( p.z ) && glWindowPos2f == NULL )
	{
		glWindowPos2f = (PFNGLWINDOWPOS2FPROC) glXGetProcAddress( (const GLubyte*)"glWindowPos2f" );
		if ( !glWindowPos2f )
			return; // unsupported glWindowPos2f
	}
#endif
	
	if ( std::isfinite( p.z ) )
		glRasterPos3f( p.x, p.y, p.z );
	else
		glWindowPos2f( p.x, p.y );

	GLboolean ok = false;
	glGetBooleanv( GL_CURRENT_RASTER_POSITION_VALID, &ok );
	if ( !ok )
		return; // don't bother rendering the text if raster position is invalid

	wxClientDC cdc( this );
	cdc.SetFont( font ? *font : GetFont() );
	wxSize sz = cdc.GetTextExtent( text );
	
	wxBitmap bmp( sz );
	wxMemoryDC dc( bmp );
	dc.SetFont( font ? *font : GetFont() );
	bool transp = back.IsTransparent();
	
	if ( transp )
	{
		dc.SetBackground( *wxWHITE_BRUSH );
		dc.SetTextForeground( *wxBLACK );
	}
	else
	{
		dc.SetBackground( back );
		dc.SetTextForeground( textcolor );
	}

	dc.Clear();
	dc.DrawText( text, 0, 0 );

	wxImage img( bmp.ConvertToImage() );
    GLubyte *bitmapData = img.GetData();   
    const int BPP = 4;
    GLubyte *imageData=(GLubyte *)malloc(sz.x * sz.y * BPP);
	if ( !imageData ) return;


    for(int y=0; y<sz.y; y++)
    {
        for(int x=0; x<sz.x; x++)
        {	
			GLubyte red = bitmapData[( x+(sz.y-y-1)*sz.x)*3+0];
			GLubyte green = bitmapData[( x+(sz.y-y-1)*sz.x)*3+1];
			GLubyte blue = bitmapData[( x+(sz.y-y-1)*sz.x)*3+2];
			GLubyte alpha = 255;
			
			if ( transp )
			{
				float scale = 1.0f - ( (((float)red)+((float)green)+((float)blue) )/3.0f ) / 255.0f;

				// pass in desired text color scaled by intensity
				red = textcolor.Red();
				green = textcolor.Green();
				blue = textcolor.Blue();
				alpha = (GLubyte)(255.0f*scale);
			}

			imageData[(x+y*sz.x)*BPP+0] = red;
			imageData[(x+y*sz.x)*BPP+1] = green;
			imageData[(x+y*sz.x)*BPP+2] = blue;
			imageData[(x+y*sz.x)*BPP+3] = alpha;

        }
    }

	glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );
	glPixelZoom( 1.0f, 1.0f );
	glDrawPixels( sz.x, sz.y, GL_RGBA, GL_UNSIGNED_BYTE, imageData );
	
    free(imageData);
}

void wxGLEasyCanvas::Axes( const wxGLPoint3D &min, const wxGLPoint3D &max, 
	float ticksizepx, bool extend,
	const wxString &xlabel, const wxString &ylabel, const wxString &zlabel, wxFont *font )
{

static wxColour xcolor( *wxRED );
static wxColour ycolor( "forest green" );
static wxColour zcolor( *wxBLUE );
	
	wxSize client( GetClientSize() );
	float world_size = m_orth.right - m_orth.left;
	float pixel_to_world = (world_size/((float)client.x))/m_zoom;

	float fontsize = font ? font->GetPointSize() : GetFont().GetPointSize();

	float ticksize = ticksizepx*pixel_to_world;
	float axis_label_offset = 7*pixel_to_world;


	// ------- x axis -------
	Color( xcolor );
	std::vector<wxPLAxis::TickData> list;
	double dmin = min.x, dmax = max.x;
	if( extend ) wxPLAxis::ExtendBoundsToNiceNumber( &dmax, &dmin );
	if ( ticksize > 0.0f )
	{
		float physx = 0.1f/fontsize * world_size * m_zoom * m_scale.x * (max.x - min.x);
		wxPLLinearAxis xax( dmin, dmax );
		xax.GetAxisTicks( 0, physx, list );
		for( size_t i=0;i<list.size();i++ )
		{
			if ( list[i].size != wxPLAxis::TickData::NONE )
			{
				float sz = list[i].size == wxPLAxis::TickData::LARGE ? ticksize : 0.5f*ticksize;
				Line( wxGLPoint3D( list[i].world, 0, 0 ), 
					wxGLPoint3D( list[i].world, sz, 0 ) );
			}

			if ( !list[i].label.IsEmpty() && fabs(list[i].world) > 1.0e-14 )
				Text( wxGLPoint3D( list[i].world, ticksize, 0 ), list[i].label, xcolor, *wxTRANSPARENT_BRUSH, font );
		}
	}
	Line( wxGLPoint3D( dmin, 0, 0 ), wxGLPoint3D( dmax, 0, 0 ) );
	Text( wxGLPoint3D( dmax+axis_label_offset, 0, 0 ), xlabel, xcolor, *wxTRANSPARENT_BRUSH, font );

	// ------- y axis -------
	Color( ycolor );
	dmin = min.y;
	dmax = max.y;
	if( extend ) wxPLAxis::ExtendBoundsToNiceNumber( &dmax, &dmin );
	if ( ticksize > 0.0f )
	{
		float physy = 0.1f/fontsize * world_size * m_zoom * m_scale.y * (max.y - min.y);
		wxPLLinearAxis yax( dmin, dmax );
		list.clear();
		yax.GetAxisTicks( 0, physy, list );
		for( size_t i=0;i<list.size();i++ )
		{
			if ( list[i].size != wxPLAxis::TickData::NONE )
			{
				float sz = list[i].size == wxPLAxis::TickData::LARGE ? ticksize : 0.5f*ticksize;
				Line( wxGLPoint3D( 0, list[i].world, 0 ), 
					wxGLPoint3D( sz, list[i].world, 0 ) );
			}

			if ( !list[i].label.IsEmpty() && fabs(list[i].world) > 1.0e-14 )
				Text( wxGLPoint3D( ticksize, list[i].world, 0 ), list[i].label, ycolor, *wxTRANSPARENT_BRUSH, font );
		}
	}
	Line( wxGLPoint3D( 0, dmin, 0 ), wxGLPoint3D( 0, dmax, 0 ) );
	Text( wxGLPoint3D( 0, dmax+axis_label_offset, 0 ), ylabel, ycolor, *wxTRANSPARENT_BRUSH, font );
		
	// ------- z axis -------
	Color( zcolor );
	dmin = min.z;
	dmax = max.z;
	if( extend ) wxPLAxis::ExtendBoundsToNiceNumber( &dmax, &dmin );
	if ( ticksize > 0.0f )
	{
		float physz = 0.1f/fontsize * world_size * m_zoom * m_scale.z * (max.z - min.z);
		wxPLLinearAxis zax( dmin, dmax );
		list.clear();
		zax.GetAxisTicks( 0, physz, list );
		for( size_t i=0;i<list.size();i++ )
		{
			if ( list[i].size != wxPLAxis::TickData::NONE )
			{
				float sz = list[i].size == wxPLAxis::TickData::LARGE ? ticksize : 0.5f*ticksize;
				Line( wxGLPoint3D( 0, 0, list[i].world ), 
					wxGLPoint3D( 0, sz, list[i].world ) );
			}

			if ( !list[i].label.IsEmpty() && fabs(list[i].world) > 1.0e-14 )
				Text( wxGLPoint3D( 0, ticksize, list[i].world ), list[i].label, zcolor, *wxTRANSPARENT_BRUSH, font );
		}
	}

	Line( wxGLPoint3D( 0, 0, dmin ), wxGLPoint3D( 0, 0 , dmax) );
	Text( wxGLPoint3D( 0, 0, dmax+axis_label_offset ), zlabel, zcolor, *wxTRANSPARENT_BRUSH, font );
}

void wxGLEasyCanvas::OnChar( wxKeyEvent &evt )
{
	// don't do anything yet.
	evt.Skip();
}

void wxGLEasyCanvas::OnMenu( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case wxID_COPY:		
		if ( wxTheClipboard->Open() )
		{
			wxBusyInfo info( "Copying image to clipboard...", this );
			wxTheClipboard->SetData( new wxBitmapDataObject( GetBitmap() ) );
			wxTheClipboard->Close();
			wxMilliSleep( 50 );
		}
		break;
	case wxID_SAVE:
	{
		wxFileDialog dlg( this, "Save image to file", wxEmptyString, "image.png", "PNG Files (*.png)|*.png", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
		if ( dlg.ShowModal() == wxID_OK )
		{
			wxBitmap bit( GetBitmap() );
			if ( !bit.SaveFile( dlg.GetPath(), wxBITMAP_TYPE_PNG ) )
				wxMessageBox("Error saving to file:\n\n" + dlg.GetPath() );
		}
	}
		break;
	}
}

void wxGLEasyCanvas::OnMouse( wxMouseEvent &evt )
{
    // Allow default processing to happen, or else the canvas cannot gain focus
    // (for key events).
    evt.Skip();

	wxPoint mpos( evt.GetPosition() );
	
	GLint viewport[4];
	glGetIntegerv( GL_VIEWPORT, viewport );		
		
	double px = (double)(mpos.x-viewport[0])/(double)(viewport[2]);
	double py = (double)(mpos.y-viewport[1])/(double)(viewport[3]);
	px = m_orth.left + px*(m_orth.right-m_orth.left);
	py = m_orth.top  + py*(m_orth.bottom-m_orth.top);
	double pz = m_orth.znear;
	
	SetCurrent( m_glContext );
		
	if ( evt.Dragging() && evt.ControlDown() )
	{
		// zoom in/out
		int dy = m_lastY - evt.GetY();
		float ds = ((float)dy) * m_zoomRate;
		m_zoom = CLAMP( m_zoom+ds, m_zoomMin, m_zoomMax );

		Refresh(false);
	}
	else if ( evt.Dragging() && evt.ShiftDown() )
	{
		m_offset.x += px-m_last3D.x;
		m_offset.y += py-m_last3D.y;
		m_offset.z += pz-m_last3D.z;
		
		Refresh(false);
	}
    else if ( evt.Dragging() )
	{
		wxSize client( GetClientSize() );
		m_trackball.Spin( evt.GetX(), evt.GetY(), client.x, client.y );
        Refresh(false);
    }

    m_lastX = evt.GetX();
    m_lastY = evt.GetY();
	
	m_last3D.x = px;
	m_last3D.y = py;
	m_last3D.z = pz;

	m_trackball.Mouse( m_lastX, m_lastY );

	if ( evt.GetEventType() == wxEVT_RIGHT_DOWN )
	{
		wxMenu menu;
		menu.Append( wxID_COPY, "Copy to clipboard" );
		menu.Append( wxID_SAVE, "Save to image file..." );
		PopupMenu( &menu );

	}
	else
	{
		// allow descendents to handle mouse events too
		evt.Skip();
	}
}
static void get_gl_rgba( const wxColour &c, float glc[4] )
{
	glc[0] = float(c.Red())/255.0f;
	glc[1] = float(c.Green())/255.0f; 
	glc[2] = float(c.Blue())/255.0f;
	glc[3] = float(c.Alpha())/255.0f;
}

void wxGLEasyCanvas::OnPaint( wxPaintEvent & )
{
    // This is a dummy, to avoid an endless succession of paint messages.
    // OnPaint handlers must always create a wxPaintDC.
    wxPaintDC dc(this);

	wxStopWatch sw;

    int w, h;
    GetClientSize(&w, &h);

	// reasonable limits on width/height
	if ( w < 10 ) w = 10;
	if ( h < 10 ) h = 10;

    // This is normally only necessary if there is more than one wxGLCanvas
    // or more than one wxGLContext in the application.
    SetCurrent( m_glContext );
	
	float color[4];
	get_gl_rgba( GetBackgroundColour(), color );
    glClearColor( color[0], color[1], color[2], color[3] );
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
	
	glEnable(GL_COLOR_MATERIAL);
    glDisable(GL_CULL_FACE);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
     
	if ( m_antiAliasing )
	{
		glEnable( GL_LINE_SMOOTH );
		glHint( GL_LINE_SMOOTH_HINT, GL_FASTEST );
	}
	
    // It's up to the application code to update the OpenGL viewport settings.
    glViewport(0, 0, (GLint) w, (GLint) h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
	
	// m_orth.top doesn't change unless set by user in SetViewSize()
	// znear and zfar are set at constructor time or by SetViewZ();
    m_orth.bottom = -m_orth.top;
    m_orth.left   = -(double)w/(double)h*m_orth.top;
    m_orth.right  = -m_orth.left;
	

	glOrtho( m_orth.left, m_orth.right, m_orth.bottom, m_orth.top, m_orth.znear, m_orth.zfar );
	
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
	
	
	glTranslatef( m_offset.x, m_offset.y, m_offset.z );
	GLfloat M[4][4];
	m_trackball.GetRotationMatrix( M );
	glMultMatrixf( &M[0][0] );
	glScalef( m_scale.x*m_zoom, m_scale.y*m_zoom, m_scale.z*m_zoom );	
		
	OnRender();

	if ( m_showStatus )
	{
		long ms = sw.Time();
		Text( 0, 0, 
			wxString::Format("rendering(%d ms) offset(%g %g %g) zoom(%g) scale(%g %g %g)", 
				ms, m_offset.x, m_offset.y, m_offset.z, m_zoom, m_scale.x, m_scale.y, m_scale.z ), 
			*wxWHITE, *wxBLUE_BRUSH, wxSMALL_FONT );
	}

	glFlush();

    SwapBuffers();
}

void wxGLEasyCanvas::OnSize( wxSizeEvent &evt )
{
	Refresh( false );
}



BEGIN_EVENT_TABLE( wxGLEasyCanvasTest, wxGLEasyCanvas )
//	EVT_RIGHT_DOWN( wxGLEasyCanvasTest::OnRightDown )
	EVT_MENU( wxID_OPEN, wxGLEasyCanvasTest::OnMenu )
END_EVENT_TABLE()

wxGLEasyCanvasTest::wxGLEasyCanvasTest( wxWindow *parent )
	: wxGLEasyCanvas( parent, wxID_ANY )
{
	m_showStatus = true;
	//wxFont font(*wxNORMAL_FONT);
	//font.SetPointSize( 14 );
	//SetFont(font);
}

void wxGLEasyCanvasTest::OnMenu( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case wxID_OPEN:
	{
		wxFileDialog dlg( this, "Open x,y,z data file" );
		if ( dlg.ShowModal() == wxID_OK )
		{
			if ( FILE *fp = fopen( dlg.GetPath().c_str(), "r" ) )
			{
				wxGLPoint3D min(1e38,1e38,1e38), max(-1e38,-1e38,-1e38);
				char buf[512];
				m_data.clear();
				m_data.reserve( 20000 );
				while( fgets( buf, 512, fp ) )
				{
					wxGLPoint3D p;
					sscanf( buf, "%g,%g,%g", &p.x, &p.y, &p.z );
					m_data.push_back( p );

					if ( p.x < min.x ) min.x = p.x;
					if ( p.y < min.y ) min.y = p.y;
					if ( p.z < min.z ) min.z = p.z;
					if ( p.x > max.x ) max.x = p.x;
					if ( p.y > max.y ) max.y = p.y;
					if ( p.z > max.z ) max.z = p.z;
				}
				fclose(fp);

				// NOTE: for some reason under wxGTK the following is required to avoid that
				//       the surface gets rendered in a small rectangle in the top-left corner of the frame
				PostSizeEventToParent();

				Refresh();
			}
		}
	}
		break;
	}
}

void wxGLEasyCanvasTest::OnRender()
{
	float len = 50.0f;
	Text( wxGLPoint3D( len, 0, 0 ), "X axis (50)", *wxWHITE, *wxRED_BRUSH, wxSMALL_FONT );
	Text( wxGLPoint3D( 0, len, 0 ), "Y axis (50)", *wxGREEN );
	Text( wxGLPoint3D( 0, 0, len ), "Z axis (50)", *wxBLUE );

	if ( m_data.size() > 0 )
	{
		// data
		Color( *wxBLACK );
		Points( m_data );
	}

	Axes(wxGLPoint3D(-3,-10,0), wxGLPoint3D(4,7,1) );
}

void wxGLEasyCanvasTest::OnRightDown( wxMouseEvent &evt )
{
	wxMenu menu;
	menu.Append( wxID_OPEN, "Open..." );
	PopupMenu( &menu );
}


/*
 * (c) Copyright 1993, 1994, Silicon Graphics, Inc.
 * ALL RIGHTS RESERVED
 * Permission to use, copy, modify, and distribute this software for
 * any purpose and without fee is hereby granted, provided that the above
 * copyright notice appear in all copies and that both the copyright notice
 * and this permission notice appear in supporting documentation, and that
 * the name of Silicon Graphics, Inc. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.
 *
 * THE MATERIAL EMBODIED ON THIS SOFTWARE IS PROVIDED TO YOU "AS-IS"
 * AND WITHOUT WARRANTY OF ANY KIND, EXPRESS, IMPLIED OR OTHERWISE,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY OR
 * FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL SILICON
 * GRAPHICS, INC.  BE LIABLE TO YOU OR ANYONE ELSE FOR ANY DIRECT,
 * SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY
 * KIND, OR ANY DAMAGES WHATSOEVER, INCLUDING WITHOUT LIMITATION,
 * LOSS OF PROFIT, LOSS OF USE, SAVINGS OR REVENUE, OR THE CLAIMS OF
 * THIRD PARTIES, WHETHER OR NOT SILICON GRAPHICS, INC.  HAS BEEN
 * ADVISED OF THE POSSIBILITY OF SUCH LOSS, HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE
 * POSSESSION, USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * US Government Users Restricted Rights
 * Use, duplication, or disclosure by the Government is subject to
 * restrictions set forth in FAR 52.227.19(c)(2) or subparagraph
 * (c)(1)(ii) of the Rights in Technical Data and Computer Software
 * clause at DFARS 252.227-7013 and/or in similar or successor
 * clauses in the FAR or the DOD or NASA FAR Supplement.
 * Unpublished-- rights reserved under the copyright laws of the
 * United States.  Contractor/manufacturer is Silicon Graphics,
 * Inc., 2011 N.  Shoreline Blvd., Mountain View, CA 94039-7311.
 *
 * OpenGL(TM) is a trademark of Silicon Graphics, Inc.
 */
/*
 * Trackball code:
 *
 * Implementation of a virtual trackball.
 * Implemented by Gavin Bell, lots of ideas from Thant Tessman and
 *   the August '88 issue of Siggraph's "Computer Graphics," pp. 121-129.
 *
 * Vector manip code:
 *
 * Original code from:
 * David M. Ciemiewicz, Mark Grossman, Henry Moreton, and Paul Haeberli
 *
 * Much mucking with by:
 * Gavin Bell
 */

/*
 * This size should really be based on the distance from the center of
 * rotation to the point on the object underneath the mouse.  That
 * point would then track the mouse as closely as possible.  This is a
 * simple example, though, so that is left as an Exercise for the
 * Programmer.
 */
#define TRACKBALLSIZE  (0.8f)

/*
 * Local function prototypes (not defined in trackball.h)
 */
static float tb_project_to_sphere(float, float, float);
static void normalize_quat(float [4]);

static void vzero(float *v)
{
    v[0] = 0.0;
    v[1] = 0.0;
    v[2] = 0.0;
}

static void vset(float *v, float x, float y, float z)
{
    v[0] = x;
    v[1] = y;
    v[2] = z;
}

static void vsub(const float *src1, const float *src2, float *dst)
{
    dst[0] = src1[0] - src2[0];
    dst[1] = src1[1] - src2[1];
    dst[2] = src1[2] - src2[2];
}

static void vcopy(const float *v1, float *v2)
{
    register int i;
    for (i = 0 ; i < 3 ; i++)
        v2[i] = v1[i];
}

static void vcross(const float *v1, const float *v2, float *cross)
{
    float temp[3];

    temp[0] = (v1[1] * v2[2]) - (v1[2] * v2[1]);
    temp[1] = (v1[2] * v2[0]) - (v1[0] * v2[2]);
    temp[2] = (v1[0] * v2[1]) - (v1[1] * v2[0]);
    vcopy(temp, cross);
}

static float vlength(const float *v)
{
    return (float) sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
}

static void vscale(float *v, float div)
{
    v[0] *= div;
    v[1] *= div;
    v[2] *= div;
}

static void vnormal(float *v)
{
    vscale(v, 1.0f/vlength(v));
}

static float vdot(const float *v1, const float *v2)
{
    return v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2];
}

static void vadd(const float *src1, const float *src2, float *dst)
{
    dst[0] = src1[0] + src2[0];
    dst[1] = src1[1] + src2[1];
    dst[2] = src1[2] + src2[2];
}


/*
 *  Given an axis and angle, compute quaternion.
 */
static void axis_to_quat(float a[3], float phi, float q[4])
{
    vnormal(a);
    vcopy(a, q);
    vscale(q, (float) sin(phi/2.0));
    q[3] = (float) cos(phi/2.0);
}

/*
 * Ok, simulate a track-ball.  Project the points onto the virtual
 * trackball, then figure out the axis of rotation, which is the cross
 * product of P1 P2 and O P1 (O is the center of the ball, 0,0,0)
 * Note:  This is a deformed trackball-- is a trackball in the center,
 * but is deformed into a hyperbolic sheet of rotation away from the
 * center.  This particular function was chosen after trying out
 * several variations.
 *
 * It is assumed that the arguments to this routine are in the range
 * (-1.0 ... 1.0)
 */
static void trackball(float q[4], float p1x, float p1y, float p2x, float p2y)
{
    float a[3]; /* Axis of rotation */
    float phi;  /* how much to rotate about axis */
    float p1[3], p2[3], d[3];
    float t;

    if (p1x == p2x && p1y == p2y) {
        /* Zero rotation */
        vzero(q);
        q[3] = 1.0;
        return;
    }

    /*
     * First, figure out z-coordinates for projection of P1 and P2 to
     * deformed sphere
     */
    vset(p1, p1x, p1y, tb_project_to_sphere(TRACKBALLSIZE, p1x, p1y));
    vset(p2, p2x, p2y, tb_project_to_sphere(TRACKBALLSIZE, p2x, p2y));

    /*
     *  Now, we want the cross product of P1 and P2
     */
    vcross(p2,p1,a);

    /*
     *  Figure out how much to rotate around that axis.
     */
    vsub(p1, p2, d);
    t = vlength(d) / (2.0f*TRACKBALLSIZE);

    /*
     * Avoid problems with out-of-control values...
     */
    if (t > 1.0) t = 1.0;
    if (t < -1.0) t = -1.0;
    phi = 2.0f * (float) asin(t);

    axis_to_quat(a,phi,q);
}

/*
 * Project an x,y pair onto a sphere of radius r OR a hyperbolic sheet
 * if we are away from the center of the sphere.
 */
static float tb_project_to_sphere(float r, float x, float y)
{
    float d, t, z;

    d = (float) sqrt(x*x + y*y);
    if (d < r * 0.70710678118654752440) {    /* Inside sphere */
        z = (float) sqrt(r*r - d*d);
    } else {           /* On hyperbola */
        t = r / 1.41421356237309504880f;
        z = t*t / d;
    }
    return z;
}

/*
 * Given two rotations, e1 and e2, expressed as quaternion rotations,
 * figure out the equivalent single rotation and stuff it into dest.
 *
 * This routine also normalizes the result every RENORMCOUNT times it is
 * called, to keep error from creeping in.
 *
 * NOTE: This routine is written so that q1 or q2 may be the same
 * as dest (or each other).
 */

#define RENORMCOUNT 97

static void add_quats(float q1[4], float q2[4], float dest[4])
{
    static int count=0;
    float t1[4], t2[4], t3[4];
    float tf[4];

    vcopy(q1,t1);
    vscale(t1,q2[3]);

    vcopy(q2,t2);
    vscale(t2,q1[3]);

    vcross(q2,q1,t3);
    vadd(t1,t2,tf);
    vadd(t3,tf,tf);
    tf[3] = q1[3] * q2[3] - vdot(q1,q2);

    dest[0] = tf[0];
    dest[1] = tf[1];
    dest[2] = tf[2];
    dest[3] = tf[3];

    if (++count > RENORMCOUNT) {
        count = 0;
        normalize_quat(dest);
    }
}

/*
 * Quaternions always obey:  a^2 + b^2 + c^2 + d^2 = 1.0
 * If they don't add up to 1.0, dividing by their magnitued will
 * renormalize them.
 *
 * Note: See the following for more information on quaternions:
 *
 * - Shoemake, K., Animating rotation with quaternion curves, Computer
 *   Graphics 19, No 3 (Proc. SIGGRAPH'85), 245-254, 1985.
 * - Pletinckx, D., Quaternion calculus as a basic tool in computer
 *   graphics, The Visual Computer 5, 2-13, 1989.
 */
static void normalize_quat(float q[4])
{
    int i;
    float mag;

    mag = (q[0]*q[0] + q[1]*q[1] + q[2]*q[2] + q[3]*q[3]);
    for (i = 0; i < 4; i++) q[i] /= mag;
}

/*
 * Build a rotation matrix, given a quaternion rotation.
 *
 */
static void build_rotmatrix(float m[4][4], float q[4])
{
    m[0][0] = 1.0f - 2.0f * (q[1] * q[1] + q[2] * q[2]);
    m[0][1] = 2.0f * (q[0] * q[1] - q[2] * q[3]);
    m[0][2] = 2.0f * (q[2] * q[0] + q[1] * q[3]);
    m[0][3] = 0.0f;

    m[1][0] = 2.0f * (q[0] * q[1] + q[2] * q[3]);
    m[1][1]= 1.0f - 2.0f * (q[2] * q[2] + q[0] * q[0]);
    m[1][2] = 2.0f * (q[1] * q[2] - q[0] * q[3]);
    m[1][3] = 0.0f;

    m[2][0] = 2.0f * (q[2] * q[0] - q[1] * q[3]);
    m[2][1] = 2.0f * (q[1] * q[2] + q[0] * q[3]);
    m[2][2] = 1.0f - 2.0f * (q[1] * q[1] + q[0] * q[0]);
    m[2][3] = 0.0f;

    m[3][0] = 0.0f;
    m[3][1] = 0.0f;
    m[3][2] = 0.0f;
    m[3][3] = 1.0f;
}



wxGLTrackball::wxGLTrackball()
{
	m_lastX = m_lastY = 0;
	trackball( m_quat, 0, 0, 0, 0 );
}

// call this when mouse moves
void wxGLTrackball::Mouse( float mx, float my )
{
	m_lastX = mx;
	m_lastY = my;
}

// call this when mouse moves and you want to spin
void wxGLTrackball::Spin( float mx, float my, float win_width, float win_height )
{	
    float spin_quat[4];
    trackball(spin_quat,
        (2.0*m_lastX - win_width) / win_width,
        (win_height - 2.0*m_lastY) / win_height,
        (2.0*mx - win_width) / win_width,
        (win_height - 2.0*my) / win_height);

    add_quats(spin_quat, m_quat, m_quat);

	m_lastX = mx;
	m_lastY = my;
}

// call this for a rotation matrix to use with glMultMatrixf()
void wxGLTrackball::GetRotationMatrix( GLfloat M[4][4] )
{
	build_rotmatrix( M, m_quat );
}
