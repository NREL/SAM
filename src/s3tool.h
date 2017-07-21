/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __shade3d_h
#define __shade3d_h

#include <wx/frame.h>
#include <wx/propgrid/propgrid.h>


#include <wx/buffer.h>

//#include <wex/lkscript.h>
#include <wex/easycurl.h>

#include "object.h"

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
class wxWebView;

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
	wxEasyCurl m_curl;
	wxNumericCtrl *m_lat, *m_lon, *m_tz;

	void OnCurl( wxEasyCurlEvent &evt );
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

	struct surfshade
	{
		static const int DIURNAL = -1;

		surfshade( int size, const wxString &grpname )
		{
			group = grpname;

			if ( size == DIURNAL )
			{
				sfac.resize_fill( 12, 24, 1 );
				shaded.resize_fill( 12, 24, 0 );
				active.resize_fill( 12, 24, 0 );
				nsurf.resize_fill( 12, 24, 0 );
				aoisum.resize_fill( 12, 24, 0 );
			}
			else if (size > 0)
			{
				sfac.resize_fill( size, 1 );
				shaded.resize_fill( size, 0 );
				active.resize_fill( size, 0 );
				nsurf.resize_fill( size, 0 );
				aoisum.resize_fill( size, 0 );
			}
		}

		static int nvalues( int minute_step )
		{
			return 8760*(60/minute_step);
		}
	
		// configuration
		wxString group;
		std::vector<VActiveSurfaceObject*> surfaces;
		std::vector<int> ids;

		// calculated
		matrix_t<float> sfac;
		matrix_t<double> shaded, active;
		matrix_t<size_t> nsurf;
		matrix_t<double> aoisum;

	};


	ShadeAnalysis( wxWindow *parent, ShadeTool *st );

	bool SimulateDiurnal();
	size_t GetDiurnalCount();
	void GetDiurnal( size_t i, matrix_t<float> *mxh, wxString *name );
	bool SimulateDiffuse(std::vector<surfshade> &shade, bool save = false);
	size_t GetDiffuseCount();
	void GetDiffuse(size_t i, double *shade_percent, double *shade_factor, double *shade_count, wxString *name);
	bool SimulateTimeseries( int minute_step, std::vector<surfshade> &shade );
	size_t GetTimeseriesCount();
	void GetTimeseries(size_t i, std::vector<float> *ts, wxString *name);

	void InitializeSections( int mode, std::vector<surfshade> &shade );
	
	wxString GetGroupDisplayName(const wxString &group);
private:
	ShadeTool *m_shadeTool;
	
	wxTextCtrl *m_diffuseResults;
	wxScrolledWindow *m_scroll;
	std::vector<AFMonthByHourFactorCtrl*> m_mxhList;
	std::vector<double> m_diffuseShadePercent;
	std::vector<double> m_diffuseShadeFactor;
	std::vector<double> m_diffuseShadeCount;
	wxArrayString m_diffuseName;

	void OnGenerateTimeSeries( wxCommandEvent & );
	void OnGenerateDiurnal(wxCommandEvent &);
	void OnGenerateDiffuse(wxCommandEvent &);

	DECLARE_EVENT_TABLE();
};

#define PG_LOCATION 0
#define PG_SCENE 1
#define PG_ANALYSIS 2
#define PG_SCRIPTING 3
#define PG_HELP 4

class ShadeScripting : public wxPanel
{
public:
	ShadeScripting( wxWindow *parent, ShadeTool *st );
	virtual ~ShadeScripting();
	
	
	void Open();
	bool Save();
	bool SaveAs();
	bool CloseDoc();
	void Exec();

	bool Load( const wxString & );
	bool Write( const wxString & );

protected:
	void OnCommand( wxCommandEvent & );
private:
	ShadeTool *m_shadeTool;
	wxString m_fileName;
	class MyScriptCtrl;
	MyScriptCtrl *m_script;
	wxTextCtrl *m_output;
	wxStaticText *m_statusLabel;
	wxButton *m_stopButton;

	DECLARE_EVENT_TABLE();
};

class ShadeTool : public wxPanel
{
public:
	ShadeTool( wxWindow *parent, int id, const wxString &data_path = wxEmptyString);
	
	LocationSetup *GetLocationSetup();
	View3D *GetView();
	ShadeAnalysis *GetAnalysis() { return m_analysis; }
	void SwitchTo( int page );
	wxString GetFileName() { return m_fileName; }
	ObjectEditor *GetObjectEditor() { return m_sceneParams; }

	bool Load();
	void Save();

	bool WriteToFile( const wxString &file );
	bool LoadFromFile( const wxString &file );

	void Write( wxOutputStream & );
	bool Read( wxInputStream & );

	struct shadets {
		wxString name;
		std::vector<float> ts;
	};
	struct diurnal {
		wxString name;
		matrix_t<float> mxh;
	};
	// for each group shade_percent = shade_factor / shade_count
	// overall = sum (shade_factor) / sum (shade_count)
	struct diffuse {
		wxString name;
		double shade_percent;
		double shade_factor; 
		double shade_count;
	};

	bool SimulateTimeseries(int &minute_timestep, std::vector<shadets> &result, bool use_groups=false);
	bool SimulateDiurnal(std::vector<diurnal> &result);
	bool SimulateDiffuse(std::vector<diffuse> &result, bool use_groups = false);

private:
	wxString m_fileName, m_dataPath;

	wxSimplebook *m_book;
	wxSplitterWindow *m_split;

	LocationSetup *m_location;
	View3D *m_view;
	ObjectEditor *m_sceneParams;
	ShadeAnalysis *m_analysis;
	ShadeScripting *m_scripting;
#ifdef S3D_STANDALONE
#if defined(__WXMSW__)||defined(__WXOSX__)
	wxWebView *m_helpViewer;
#endif
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

