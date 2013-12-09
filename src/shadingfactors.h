#ifndef __shadingctrl_h
#define __shadingctrl_h

#include <wx/wx.h>
#include <wx/treectrl.h>
#include <wx/grid.h>
#include <wx/listctrl.h>
#include <wx/splitter.h>
#include <wx/aui/auibook.h>
#include <wx/imaglist.h>

#include <wex/numeric.h>

#include "object.h"

BEGIN_DECLARE_EVENT_TYPES()
DECLARE_EVENT_TYPE( wxEVT_SHADINGCTRL_CHANGE, 0)
END_DECLARE_EVENT_TYPES()


class VarValue;

#define EVT_SHADINGCTRL(id, func) EVT_COMMAND(id, wxEVT_SHADINGCTRL_CHANGE, func)

class ShadingCtrl : public wxPanel
{
public:
	ShadingCtrl(wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &sz = wxDefaultSize);
	virtual ~ShadingCtrl();

	void SetData(const matrix_t<float> &data);
	matrix_t<float> GetData();
	
	void SetTitle(  wxString &title);
	wxString GetTitle( );
	void SetLegend(  wxString &legend);
	wxString GetLegend( );

	wxColour Colour1;
	wxColour Colour2;

private:
	void UpdateCell(int r, int c);
	void UpdateGrid();

	void OnGridCellChange(wxGridEvent &evt);
	void OnGridCellSelect(wxGridEvent &evt);
	void OnGridEditorHidden(wxGridEvent &evt);
	void OnGridEditorShown(wxGridEvent &evt);
	void OnGridRangeSelect(wxGridRangeSelectEvent &evt);

	void OnImport(wxCommandEvent &evt);
	void OnExport(wxCommandEvent &evt);
	void OnApply(wxCommandEvent &evt);

	void ApplyVal(int r, int c, double sf);
	void DispatchEvent();

	matrix_t<float> mData;
	wxNumericCtrl *mShadingVal;
	wxButton *mBtnApply;
	wxGrid *mGrid;
	int mSelTopRow, mSelBottomRow;
	int mSelLeftCol, mSelRightCol;
	bool bSkipSelect;

	wxStaticText *m_title;
	wxStaticText *m_legend;

	DECLARE_EVENT_TABLE();
};



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

