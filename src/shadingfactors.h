/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

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
	// for more than one string and PV only
//	int string_option; // 0=shading db,1=average,2=max,3=min
//	int string_option; // 0=shading db with Tc,1=shading db,2=average,3=max,4=min
//	int string_option; // 0=shading db,1=average,2=max,3=min
	int string_option; // 0=shading db,1=average
	bool en_timestep;
	matrix_t<double> timestep;

	bool en_mxh;
	matrix_t<double> mxh;

	bool en_azal;
	matrix_t<double> azal;

	bool en_diff;
	double diff;

	void clear();
	void save( std::vector<double> &data );
	bool load( const std::vector<double> &data );

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





// based on AFFloatTable
class wxShadingFactorsTable : public wxGridTableBase
{
	matrix_t<double> *d_mat;
	wxString label;
	float def_val;

public:
	wxShadingFactorsTable(matrix_t<double> *da, float _def_val = 0, const wxString &_label = "");
	void SetMatrix(matrix_t<double> *da);
	virtual int GetNumberRows();
	virtual int GetNumberCols();
	virtual bool IsEmptyCell(int row, int col);
	virtual wxString GetValue(int row, int col);
	virtual void SetValue(int row, int col, const wxString& value);
	virtual wxString GetRowLabelValue(int row);
	virtual wxString GetColLabelValue(int col);
	virtual wxString GetTypeName(int row, int col);
	virtual bool CanGetValueAs(int row, int col, const wxString& typeName);
	virtual bool CanSetValueAs(int row, int col, const wxString& typeName);
	virtual bool AppendRows(size_t nrows);
	virtual bool InsertRows(size_t pos, size_t nrows);
	virtual bool DeleteRows(size_t pos, size_t nrows);
	virtual bool AppendCols(size_t ncols);
	virtual bool InsertCols(size_t pos, size_t ncols);
	virtual bool DeleteCols(size_t pos, size_t ncols);

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

	void SetData(const matrix_t<double> &mat);
	void GetData(matrix_t<double> &mat);
	matrix_t<double> GetData() const { return m_data; }


//	void SetStringCaption(const wxString &cap);
//	wxString GetStringCaption();
	void SetDBCaption(const wxString &cap);
	wxString GetDBCaption();

	void SetColCaption(const wxString &cap);
	wxString GetColCaption();

	void SetMinuteCaption(const wxString &cap);
	wxString GetMinuteCaption();


	size_t GetNumCols() { return m_data.ncols(); }
	void SetNumCols(size_t &cols);


	size_t GetNumMinutes() { return m_num_minutes; }
	void SetNumMinutes(size_t &minutes);

	void SetDefaultValue(float &default_val) { m_default_val=default_val; }
	float GetDefaultValue() { return m_default_val; }

//	void SetStringOption(int &string_option);
//	int GetStringOption();
	void SetDBOption(int &db_option);
	int GetDBOption();

private:
	void UpdateNumberColumns(size_t &new_cols);
	void UpdateNumberRows(size_t &new_rows);
	void UpdateNumberMinutes(size_t &new_timesteps);
	void AverageCols();

	bool Export(const wxString &file);
	bool Import(const wxString &file);
	bool IsValidMinutes(int &minutes);

	double m_default_val;
	matrix_t<double> m_data;
	wxExtGridCtrl *m_grid;
	wxShadingFactorsTable *m_grid_data;
	wxStaticText *m_caption_col;
//	wxStaticText *m_caption_string;
	wxChoice *m_choice_col;
	wxStaticText *m_caption_timestep;
	wxChoice *m_choice_timestep;
//	wxChoice *m_choice_string_option;
	wxButton *m_btn_copy;
	wxButton *m_btn_paste;
	wxButton *m_btn_import;
	wxButton *m_btn_export;
	bool m_show_db_options;

	wxArrayString m_minute_arystrvals;
	wxArrayString m_col_arystrvals;
//	wxArrayString m_string_arystrvals;
	wxCheckBox *m_chk_shading_db;
	wxStaticText *m_caption_shading_db;

	size_t m_num_minutes;

	void OnCellChange(wxGridEvent &evt);
	void OnCommand(wxCommandEvent &evt);
	void OnChoiceCol(wxCommandEvent  &evt);
	void OnChoiceMinute(wxCommandEvent  &evt);


	DECLARE_EVENT_TABLE();
};



bool ImportPVsystNearShading( ShadingInputData &dat, wxWindow *parent = 0 );
bool ImportSunEyeHourly( ShadingInputData &dat, wxWindow *parent = 0 );
bool ImportSunEyeObstructions( ShadingInputData &dat, wxWindow *parent = 0 );
bool ImportSolPathMonthByHour( ShadingInputData &dat, wxWindow *parent = 0 );







#endif

