#ifndef __shade3d_h
#define __shade3d_h

#include <wx/frame.h>
#include <wx/propgrid/propgrid.h>
#include <wx/webview.h>
#include <wx/buffer.h>

#include <wex/lkscript.h>

#include "object.h"
#include "simplecurl.h"

class View3D;

class wxListBox;
class wxCheckListBox;
class wxSlider;
class wxNumericCtrl;
class wxStaticText;
class wxPLPlotCtrl;
class wxCheckBox;
class wxSplitterWindow;
class VObject;
class wxSimplebook;
class wxNumericCtrl;
class wxGenericStaticBitmap;
class ShadeTool;
class AFMonthByHourFactorCtrl;
class wxMetroButton;
class VActiveSurfaceObject;

class LocationSetup : public wxPanel
{
public:
	LocationSetup( wxWindow *parent, ShadeTool *st );
		
	void DownloadMap(  );
	int GetZoomLevel();
	wxString GetAddress();
	void SetLocation( const wxString &address, double lat, double lon, double tz );
	void GetLocation( double *lat, double *lon, double *tz );
	wxBitmap GetMap(double *lat = 0, double *lon = 0, double *tz = 0, double *mpp = 0 );
	void SetMap( const wxBitmap &bit, const wxString &addrstr, double lat, double lon, double tz, double mpp );
	void UpdateScale();
	void UpdateMap();

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

private:	
	ShadeTool *m_shadeTool;
	int m_zoomLevel;
	double m_mpp;
	wxTextCtrl *m_address;
	wxGenericStaticBitmap *m_bitmapCtrl;
	wxScrolledWindow *m_scrollWin;
	wxBitmap m_bitmap, m_unannotatedBitmap;
	wxTextCtrl *m_locationInfo;
	wxSimpleCurl m_curl;
	wxNumericCtrl *m_lat, *m_lon, *m_tz;

	void OnCurl( wxSimpleCurlEvent &evt );
	void DoCurlDirect( const wxString &url );	
	void OnGetMap( wxCommandEvent &evt );
	void OnMapChange( wxCommandEvent &evt );
	void OnAddressChange( wxCommandEvent & );
	void OnUnderlayMap( wxCommandEvent & );
	void OnRemoveUnderlay( wxCommandEvent & );
	void OnImportMapImage( wxCommandEvent & );
	void OnManualScale( wxCommandEvent & );
	DECLARE_EVENT_TABLE();
};

class ObjectEditor : public wxPanel
{
public:
	ObjectEditor( wxWindow *parent, int id, View3D *view, 
		const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize );
	virtual ~ObjectEditor();
	
	void SetObject( VObject *obj ); // for properties
	void UpdateObjectList( );
	void UpdatePropertyValues();

private:

	struct pgpinfo {
		wxString name;
		int type;
		wxPGProperty *pgp;
	};

	void ValueToPropGrid( pgpinfo &p );
	void PropGridToValue( pgpinfo &p );
		
	View3D *m_view;

	std::vector<pgpinfo> m_curProps;
	VObject *m_curObject;

	wxPropertyGrid *m_propGrid;
	wxCheckListBox *m_objList;
	
	wxMetroButton *m_duplicate, *m_delete;

    void OnPropertyGridChange(wxPropertyGridEvent &evt);
    void OnPropertyGridChanging(wxPropertyGridEvent &evt);

	void OnObjectList( wxCommandEvent & );
	void OnObjectCheckList( wxCommandEvent & );
	void OnCommand( wxCommandEvent & );
		
	DECLARE_EVENT_TABLE();
};

class ShadeAnalysis : public wxPanel
{
public:
	ShadeAnalysis( wxWindow *parent, ShadeTool *st );

	bool SimulateDiurnal();
	size_t GetDiurnalCount();
	void GetDiurnal( size_t i, matrix_t<float> *mxh, wxString *name );

private:
	
	struct surfshade
	{
		enum { DIURNAL, HOURLY };
		surfshade( int mode, const wxString &grpname )
		{
			group = grpname;

			if ( mode == DIURNAL )
			{
				sfac.resize_fill( 12, 24, 1 );
				shaded.resize_fill( 12, 24, 0 );
				active.resize_fill( 12, 24, 0 );
			}
			else
			{
				sfac.resize_fill( 8760, 1 );
				shaded.resize_fill( 8760, 0 );
				active.resize_fill( 8760, 0 );
			}
		}
	
		wxString group;
		matrix_t<float> sfac;
		matrix_t<double> shaded, active;
		std::vector<VActiveSurfaceObject*> surfaces;
		std::vector<int> ids;
	};

	void InitializeSections( int mode, std::vector<surfshade> &shade );

	ShadeTool *m_shadeTool;
	
	wxScrolledWindow *m_scroll;
	std::vector<AFMonthByHourFactorCtrl*> m_mxhList;
	
	void OnGenerateHourly( wxCommandEvent & );
	void OnGenerateDiurnal(wxCommandEvent &);
	void OnGenerateDiffuse(wxCommandEvent &);

	DECLARE_EVENT_TABLE();
};

#define PG_LOCATION 0
#define PG_SCENE 1
#define PG_ANALYSIS 2
#define PG_HELP 3

class ShadeTool : public wxPanel
{
public:
	ShadeTool( wxWindow *parent, int id, const wxString &data_path = wxEmptyString);
	
	LocationSetup *GetLocationSetup();
	View3D *GetView();
	void SwitchTo( int page );

	bool Load();
	void Save();

	bool WriteToFile( const wxString &file );
	bool LoadFromFile( const wxString &file );

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

	struct diurnal {
		wxString name;
		matrix_t<float> mxh;
	};

	bool SimulateDiurnal( std::vector<diurnal> &result );

private:
	wxString m_fileName;

	wxSimplebook *m_book;
	wxSplitterWindow *m_split;

	LocationSetup *m_location;
	View3D *m_view;
	ObjectEditor *m_sceneParams;
	ShadeAnalysis *m_analysis;

#ifdef S3D_STANDALONE
	wxWebView *m_helpViewer;
#endif


	void OnCommand( wxCommandEvent &evt );
	void OnUpdateObjectList( wxCommandEvent & );
	void OnUpdateProperties( wxCommandEvent & );
	void OnUpdateSelection( wxCommandEvent & );

	void OnCreateObject( wxCommandEvent & );


	// debugging tools
	wxPanel *m_debugPanel;
	wxTextCtrl *m_txtAzi;
	wxTextCtrl *m_txtAlt;
	wxTextCtrl *m_txtScale;
	wxTextCtrl *m_txtViewX;
	wxTextCtrl *m_txtViewY;
	wxTextCtrl *m_txtViewZ;
	void OnDebugCommand( wxCommandEvent & );

	DECLARE_EVENT_TABLE();
};


#endif

