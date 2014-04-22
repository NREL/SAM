#ifndef __shade3d_h
#define __shade3d_h

#include <wx/frame.h>
#include <wx/propgrid/propgrid.h>
#include <wex/lkscript.h>

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

class LocationSetup : public wxPanel
{
public:
	LocationSetup( wxWindow *parent, ShadeTool *st );
		
	bool GeoCode( const wxString &address, double *lat, double *lon, double *tz);
	void DownloadMap(  );
	int GetZoomLevel();
	wxString GetAddress();
	void GetLocation( double *lat, double *lon, double *tz );
	wxBitmap GetMap(double *lat = 0, double *lon = 0, double *tz = 0, double *mpp = 0 );
	void SetMap( const wxBitmap &bit, const wxString &addrstr, double lat, double lon, double tz, double mpp );
	void AnnotateMap();
	void UpdateScale();
	void UpdateMapCtrl();

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
	wxSimpleCurlDownloadThread m_curl;
	wxNumericCtrl *m_lat, *m_lon, *m_tz;

	void OnCurl( wxSimpleCurlEvent &evt );
	void DoCurlDirect( const wxString &url );
	void DoCurl( const wxString &url );
	void OnGetMap( wxCommandEvent &evt );
	void OnMapChange( wxCommandEvent &evt );
	void OnAddressChange( wxCommandEvent & );
	void OnUnderlayMap( wxCommandEvent & );
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

    void OnPropertyGridChange(wxPropertyGridEvent &evt);
    void OnPropertyGridChanging(wxPropertyGridEvent &evt);

	void OnObjectList( wxCommandEvent & );
	void OnObjectCheckList( wxCommandEvent & );
		
	DECLARE_EVENT_TABLE();
};

class ShadeAnalysis : public wxPanel
{
public:
	ShadeAnalysis( wxWindow *parent, ShadeTool *st );


private:
	ShadeTool *m_shadeTool;
	
	wxChoice *m_sfMode;

	wxScrolledWindow *m_scroll;
	std::vector<AFMonthByHourFactorCtrl*> m_mxhList;
	
	void OnGenerateHourly( wxCommandEvent & );
	void OnGenerateDiurnal( wxCommandEvent & );

	DECLARE_EVENT_TABLE();
};

#define PG_LOCATION 0
#define PG_SCENE 1
#define PG_ANALYSIS 2

class ShadeTool : public wxPanel
{

public:
	ShadeTool( wxWindow *parent, int id );
	
	LocationSetup *GetLocationSetup();
	View3D *GetView();
	void SwitchTo( int page );

	bool Load();
	void Save();

	bool WriteToFile( const wxString &file );
	bool LoadFromFile( const wxString &file );

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

private:
	wxSimplebook *m_book;
	wxSplitterWindow *m_split;

	LocationSetup *m_location;
	View3D *m_view;
	ObjectEditor *m_sceneParams;
	ShadeAnalysis *m_analysis;
	
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

