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
	/* version 2 */
//	bool en_hourly;
//	std::vector<float> hourly;

	/* version 3 */
	bool en_shading_db;
	bool en_timestep;
	matrix_t<float> timestep;

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
	ShadingButtonCtrl( wxWindow *parent, int id, bool show_db_options = false,
		const wxPoint &pos=wxDefaultPosition, const wxSize &size=wxDefaultSize);

	void Write( VarValue * );
	bool Read( VarValue * );
	void SetDescription( const wxString &s ) { m_descText = s; }
	void OnPressed(wxCommandEvent &evt);

private:
	bool m_show_db_options;
	ShadingInputData m_shad;
	wxString m_descText;
	DECLARE_EVENT_TABLE();
};


/* custom control for shading fractions for each parallel string (up to 8) in each subarray (up to 4)  can move to widgets or wex if necessary */
BEGIN_DECLARE_EVENT_TYPES()
DECLARE_EVENT_TYPE(wxEVT_wxShadingFactorsCtrl_CHANGE, 0)
END_DECLARE_EVENT_TYPES()



class wxShadingFactorsCtrl : public wxPanel
{
public:
	wxShadingFactorsCtrl(wxWindow *parent, int id,
		const wxPoint &pos = wxDefaultPosition,
		const wxSize &sz = wxDefaultSize,
		bool show_db_options = false,
		bool sidebuttons = false);

	void SetData(const matrix_t<float> &mat);
	void GetData(matrix_t<float> &mat);
	matrix_t<float> GetData() const { return m_data; }


	void SetColCaption(const wxString &cap);
	wxString GetColCaption();

	void SetMinuteCaption(const wxString &cap);
	wxString GetMinuteCaption();


	void ShowCols(bool b);
	bool ShowCols();

	void ShowColLabels(bool b);
	bool ShowColLabels();

	void SetColLabels(const wxArrayString &colLabels);
	wxArrayString GetColLabels();

	void SetColLabelFormatString(const wxString &col_format_str);
	wxString GetColLabelFormatString() { return m_col_format_str; }

	size_t GetNumCols() { return m_num_cols; }
	void SetNumCols(size_t &cols);

	size_t GetNumRows() { return m_num_rows; }
	void SetNumRows(size_t &rows);

	size_t GetNumMinutes() { return m_num_minutes; }
	void SetNumMinutes(size_t &minutes);

	void SetDefaultValue(float &default_val) { m_default_val=default_val; }
	float GetDefaultValue() { return m_default_val; }

	void SetEnableShadingDB(bool &en_shading_db);
	bool GetEnableShadingDB();

private:
	void UpdateNumberColumns(size_t &new_cols);
	void UpdateNumberRows(size_t &new_rows);
	void UpdateNumberMinutes(size_t &new_timesteps);
	void UpdateColumnHeaders();
	void UpdateRowLabels();

	bool m_col_header_use_format;
	wxArrayString m_col_ary_str;
	wxString m_col_format_str;
	float m_default_val;
	matrix_t<float> m_data;
	wxExtGridCtrl *m_grid;
	wxStaticText *m_caption_col;
	wxChoice *m_choice_col;
	wxStaticText *m_caption_timestep;
	wxChoice *m_choice_timestep;
	wxCheckBox *m_en_shading_db;

	wxArrayString m_minute_arystrvals;
	wxArrayString m_col_arystrvals;

	size_t m_num_cols;
	size_t m_num_rows;
	size_t m_num_minutes;

	void OnCellChange(wxGridEvent &evt);
	void OnCommand(wxCommandEvent &evt);
	void OnChoiceCol(wxCommandEvent  &evt);
	void OnChoiceMinute(wxCommandEvent  &evt);


	void MatrixToGrid();

	DECLARE_EVENT_TABLE();
};





bool ImportPVsystNearShading( ShadingInputData &dat, wxWindow *parent = 0 );
bool ImportSunEyeHourly( ShadingInputData &dat, wxWindow *parent = 0 );
bool ImportSunEyeObstructions( ShadingInputData &dat, wxWindow *parent = 0 );
bool ImportSolPathMonthByHour( ShadingInputData &dat, wxWindow *parent = 0 );







#endif

