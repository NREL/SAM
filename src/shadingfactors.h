/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
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
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
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





// based on AFFloatTable
class wxShadingFactorsTable : public wxGridTableBase
{
	matrix_t<float> *d_mat;
	wxString label;
	float def_val;

public:
	wxShadingFactorsTable(matrix_t<float> *da, float _def_val = 0, const wxString &_label = "");
	void SetMatrix(matrix_t<float> *da);
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

	void SetData(const matrix_t<float> &mat);
	void GetData(matrix_t<float> &mat);
	matrix_t<float> GetData() const { return m_data; }


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

	float m_default_val;
	matrix_t<float> m_data;
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

