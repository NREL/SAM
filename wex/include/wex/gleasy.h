#ifndef __gleasy_h
#define __gleasy_h

#include <vector>
#include <wx/window.h>
#include <wx/glcanvas.h>
#include <wx/gdicmn.h>

struct wxGLPoint3D
{
	wxGLPoint3D() : x(0),y(0), z(0) { }
	wxGLPoint3D( const wxGLPoint3D &p ) :x(p.x), y(p.y), z(p.z) { }
	wxGLPoint3D( float _x, float _y, float _z ) : x(_x), y(_y), z(_z) { }
	float x, y, z;
};


class wxGLTrackball
{
protected:
	float m_quat[4];
	float m_lastX, m_lastY;
public:
	wxGLTrackball();

	// call this every time when mouse moves
	void Mouse( float mx, float my );

	// call this when mouse moves and you want to rotate the scene,
	void Spin( float mx, float my, float win_width, float win_height );

	// call this for a rotation matrix to use with glMultMatrixf()
	void GetRotationMatrix( GLfloat m[4][4] );
};

class wxGLEasyCanvas : public wxGLCanvas
{
public:
	wxGLEasyCanvas( wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize );
	virtual ~wxGLEasyCanvas();

	void ShowStatus( bool b ) { m_showStatus = b; }
	void SetScale( float x, float y, float z ) { m_scale.x = x; m_scale.y = y; m_scale.z = z; }
	void SetScaleX( float x ) { m_scale.x = x; }
	void SetScaleY( float y ) { m_scale.y = y; }
	void SetScaleZ( float z ) { m_scale.z = z; }
	void SetZoomRate( float zr ) { m_zoomRate = 1.0f/zr; }
	void SetZoomRange( float min, float max ) { m_zoomMin = min; m_zoomMax = max; }
	void SetAntiAliasing( bool aa ) { m_antiAliasing = aa; }
	void SetZoom( float z ) { m_zoom = z; }
	float GetZoom() const { return m_zoom; }
	void SetViewSize( float size ) { m_orth.top = size; }
	float GetViewSize() const { return m_orth.top; }

	void SetViewZ( float znear, float zfar ) { m_orth.znear = znear; m_orth.zfar = zfar; }
	
	wxBitmap GetBitmap();

protected:
	virtual void OnRender();

	void Color( const wxColour &c );
	void PointSize( float p );
	void Point( float x, float y, float z );
	void Point( const wxGLPoint3D &p );
	void BeginPoints(); // optional optimization for rendering lots of points
	void EndPoints();
	void Points( const std::vector<wxGLPoint3D> &list );
	void LineWidth( float w );
	void Line( const wxGLPoint3D &p1, const wxGLPoint3D &p2 );
	void Lines( const std::vector<wxGLPoint3D> &list );
	void Text( const wxGLPoint3D &p, const wxString &text, 
		const wxColour &c = *wxBLACK, 
		const wxBrush &back=*wxTRANSPARENT_BRUSH, const wxFont *font = 0 );
	void Text( int x, int y, const wxString &text, 
		const wxColour &c = *wxBLACK, 
		const wxBrush &back=*wxTRANSPARENT_BRUSH, const wxFont *font = 0 );
	void Axes(	const wxGLPoint3D &min=wxGLPoint3D(0,0,0), 
				const wxGLPoint3D &max=wxGLPoint3D(1,1,1),
				float ticksizepx=9.0f, // in screen pixels
				bool extend=false,
				const wxString &xlabel="X", 
				const wxString &ylabel="Y", 
				const wxString &zlabel="Z",
				wxFont *font = 0 );


	wxGLContext m_glContext;
	bool m_pointListMode, m_antiAliasing, m_showStatus;
	float m_lastX, m_lastY, m_zoom, m_zoomMin, m_zoomMax, m_zoomRate;
	wxGLTrackball m_trackball;
	wxGLPoint3D m_offset, m_scale;
	unsigned int m_fontOffset;
	struct {
		float left, right, top, bottom, znear, zfar;
	} m_orth;
	wxGLPoint3D m_last3D;

	void makeRasterFont();
	void printString( const char *s );
	
	void OnMenu( wxCommandEvent & );
	void OnChar( wxKeyEvent & );
	void OnMouse( wxMouseEvent & );
	void OnPaint( wxPaintEvent & );
	void OnSize( wxSizeEvent & );
	DECLARE_EVENT_TABLE()
};


class wxGLEasyCanvasTest : public wxGLEasyCanvas
{
public:
	wxGLEasyCanvasTest( wxWindow *parent );

	void OnMenu( wxCommandEvent & );
	void OnRightDown( wxMouseEvent &evt );


protected:
	virtual void OnRender();
	std::vector<wxGLPoint3D> m_data;
	DECLARE_EVENT_TABLE();
};


#endif
