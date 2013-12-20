#ifndef __MonthByHourFactorCtrl_h
#define __MonthByHourFactorCtrl_h

#include <wx/wx.h>
#include <wx/treectrl.h>
#include <wx/grid.h>
#include <wx/listctrl.h>
#include <wx/splitter.h>
#include <wx/aui/auibook.h>
#include <wx/imaglist.h>

#include <wex/numeric.h>

#include "object.h"

class VarValue;
class AFMonthByHourFactorCtrl;

/* utility class to save/load shading data */
struct ShadingInputData
{
	ShadingInputData();

	bool en_hourly;
	std::vector<float> hourly;

	bool en_mxh;
	matrix_t<float> mxh;

	bool en_azal;
	matrix_t<float> azal;

	bool en_diff;
	double diff;

	void clear();
	void save( std::vector<float> &data );
	bool load( const std::vector<float> &data );

	void write( VarValue *vv );
	bool read( VarValue *vv );

	
};

#define EVT_SHADINGBUTTON(i,f) EVT_BUTTON(i,f)
class ShadingButtonCtrl : public wxButton
{
public:
	ShadingButtonCtrl( wxWindow *parent, int id, 
		const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize);

	void Write( VarValue * );
	bool Read( VarValue * );
	void SetDescription( const wxString &s ) { m_descText = s; }

private:
	ShadingInputData m_shad;
	wxString m_descText;
	void OnPressed(wxCommandEvent &evt);
	DECLARE_EVENT_TABLE();
};


bool ImportPVsystNearShading( ShadingInputData &dat, wxWindow *parent = 0 );
bool ImportSunEyeHourly( ShadingInputData &dat, wxWindow *parent = 0 );
bool ImportSunEyeObstructions( ShadingInputData &dat, wxWindow *parent = 0 );
bool ImportSolPathMonthByHour( ShadingInputData &dat, wxWindow *parent = 0 );
bool ImportSolPathObstructions( ShadingInputData &dat, wxWindow *parent = 0 );

#endif

