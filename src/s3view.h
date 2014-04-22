#ifndef __view3d_h
#define __view3d_h

#include <wx/window.h>
#include <wx/frame.h>
#include <wx/spinctrl.h>
#include <wx/clrpicker.h>
#include <wx/propgrid/propgrid.h>

#ifdef __WXOSX__
#define VIEW_USE_OVERLAY 1
#include <wx/overlay.h>
#endif

#include "s3objects.h"
#include "s3engine.h"

#define SF_ANALYSIS_SCALE 100

BEGIN_DECLARE_EVENT_TYPES()
	DECLARE_EVENT_TYPE( wxEVT_VIEW3D_UPDATE_VIEW, 0)
	DECLARE_EVENT_TYPE( wxEVT_VIEW3D_UPDATE_OBJECTS, 0)
	DECLARE_EVENT_TYPE( wxEVT_VIEW3D_UPDATE_PROPERTIES, 0 )
	DECLARE_EVENT_TYPE( wxEVT_VIEW3D_UPDATE_SELECTION, 0 )
END_DECLARE_EVENT_TYPES()
	
#define EVT_VIEW3D_UPDATE_VIEW(id, func) EVT_COMMAND(id, wxEVT_VIEW3D_UPDATE_VIEW, func)
#define EVT_VIEW3D_UPDATE_OBJECTS(id, func) EVT_COMMAND(id, wxEVT_VIEW3D_UPDATE_OBJECTS, func)
#define EVT_VIEW3D_UPDATE_PROPERTIES(id, func) EVT_COMMAND(id, wxEVT_VIEW3D_UPDATE_PROPERTIES, func)
#define EVT_VIEW3D_UPDATE_SELECTION(id, func) EVT_COMMAND(id, wxEVT_VIEW3D_UPDATE_SELECTION, func)

class View3D : public wxWindow
{
public:
	View3D(wxWindow *parent, int id, 
		const wxPoint &pos = wxDefaultPosition, 
		const wxSize &size = wxDefaultSize );
	virtual ~View3D();

	enum { SPIN_VIEW, TOP_VIEW, Z_VIEW, __N_MODES };
	void SetMode( int mode );
	int GetMode();
	
	VObject *CreateObject( const wxString &type );
	void AddObject( VObject *obj );
	void DeleteObject( VObject *obj );
	std::vector<VObject*> GetObjects();
	void DeleteAll();
	void Select( VObject *obj );
	bool IsSelected( VObject *obj );
	std::vector<VObject*> GetSelectedObjects();
	VObject *GetFirstSelectedObject();
	VObject *FindObjectByName( const wxString &name );
	VObject *FindObjectById( int id );
	VObject *GetObject( size_t index );
	int GetObjectIndex( VObject *o );

	// actions
	void DuplicateSelected();
	void DeleteSelected();
	void ShowAll();
	void ChangeMap( const wxBitmap &map, double mpp );
	void ShowMap( bool b );

	void SetAzAl( double &az, double &al);

	void UpdateModel( VObject *obj );
	void UpdateAllModels();

	wxArrayString GetRegisteredTypes();
	void RegisterType( VObject *obj );
	VObject *FindRegisteredType( const wxString &name );

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

	const s3d::scene &GetScene();

	double GetShadeFraction();
	void SetRotation( double azimuth, double altitude );
	void GetRotation( double *azi, double *alt );
	void SetOffset( double xoff, double yoff, double zoff );
	void GetOffset( double *xoff, double *yoff, double *zoff );
	void SetScale( double scale );
	double GetScale();

	void Render();
	
	void ScreenToWorld( int xs, int yzs, double *xw, double *yzw );
	void WorldToScreen( double xw, double yzw, int *xs, int *yzs );
	
	VHandle *FindHandle( int mx, int my, VObject *obj = 0 );
	VObject *FindFirstObject( int mx, int my );
	std::vector<VObject*> FindObjects( int mx, int my );

	void UpdateHandles( VObject *obj );
	void RemoveHandlesForObject( VObject *obj );
	void UpdateAllHandles();

	void Animate( double az, double al, double scale, double xo, double yo, double zo );

	void CreateStaticDemoScene();

#ifdef VIEW_USE_OVERLAY
	// required for rubberbanding rendering on OSX. 
	// public member so that VRenderer2D and derivative classes
	// can access it directly without denoting them as friend classes
	wxOverlay m_overlay;
#endif
protected:
	int m_winWidth, m_winHeight;

	bool m_showMap;
	wxBitmap m_staticMapXY;
	wxBitmap m_staticMapXYScaled;
	double m_mpp;

	// scene rendering
	s3d::scene m_scene;
	s3d::transform m_transform;

	std::vector<s3d::shade_result> m_shade;
	double m_sf; // scene shading fraction (computed for spin view)
	
	int m_mode;
	std::vector<VObject*> m_objects;
	std::vector<VObject*> m_selections;
	std::vector<VObject*> m_registeredTypes;

	int m_modeToolWidths[__N_MODES];
	int m_modeToolHeight;

	struct ViewParams
	{
		ViewParams();

		double azimuth;
		double altitude;
		double scale;
		double xoff;
		double yoff;
		double zoff;

		void Write( wxOutputStream & );
		bool Read( wxInputStream & );
	};

	double m_snapSpacing;
	double m_gridSpacing;
	void Snap( double *x, double *y, double spacing = -1 );
	double Snap( double v, double spacing );


	VHandle *m_movingHandle;
	std::vector<VHandle*> m_handles;

	ViewParams m_lastView[__N_MODES];

	size_t m_nextSelectionIndex;


	bool m_eraseNeeded;
	bool m_pressed;
	int m_origX, m_origY;
	double m_origScale;
	double m_origXOff, m_origYOff, m_origZOff;
	bool m_zoomMode;
	bool m_panMode;

	// screen 2d to world 3d
	double m_xw, m_yw, m_zw;

	wxColour FromRGBA( s3d::rgba &c );
	void Draw( wxDC &dc, const s3d::polygon3d &pts, 
		bool as_line, int xoff, int yoff );
	void DrawGrid( wxDC &dc );


	void OnPaint( wxPaintEvent & );
	void OnSize( wxSizeEvent & );
	void OnMotion( wxMouseEvent & );
	void OnLeftDown( wxMouseEvent & );
	void OnLeftUp( wxMouseEvent & );
	void OnRightDown( wxMouseEvent & );
	void OnWheel( wxMouseEvent & );
	void OnKey( wxKeyEvent & );
	void OnMenu( wxCommandEvent & );

	void SendEvent( int type );

	DECLARE_EVENT_TABLE();
};



#endif
