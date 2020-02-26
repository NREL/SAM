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

#include <algorithm>
#include <set>

#include <wx/sizer.h>
#include <wx/bitmap.h>
#include <wx/msgdlg.h>
#include <wx/tokenzr.h>
#include <wx/clipbrd.h>
#include <wx/filename.h>
#include <wx/busyinfo.h>

#include <wex/metro.h>

#include "variablegrid.h"
#include "widgets.h"
#include "inputpage.h"
#include "object.h"
#include "main.h"

#define COMPARE_SHOW_ALL 0
#define COMPARE_SHOW_DIFFERENT 1
#define COMPARE_SHOW_SAME 2


VariableGridData::VariableGridData(ProjectFile *pf, Case *c, VarTable *vt) : m_pf(pf), m_vt(vt)
{
	m_attr_for_calculated = new wxGridCellAttr;
	m_attr_for_calculated->SetBackgroundColour(UIColorCalculatedBack);
	m_attr_for_calculated->SetTextColour(UIColorCalculatedFore);
	m_sorted = false;
	if (c)
		m_cases.push_back(c);
	else
		m_cases = m_pf->GetCases();
	Init();
}

VariableGridData::~VariableGridData()
{
	m_attr_for_calculated->DecRef();
}



void VariableGridData::Init()
{
	m_col_hdrs.Clear();
	m_var_names.Clear();
	m_var_labels.Clear();
	m_var_info_lookup_vec.clear();
	m_var_table_vec.clear();
	VarInfoLookup vi_not_calculated;
	VarTable vt_not_calculated;


	if (m_cases.size() > 0)
	{
		m_col_hdrs.push_back("Variable");
		m_col_hdrs.push_back("Label");
		if (m_cases.size() == 1)
		{
			m_col_hdrs.push_back(m_pf->GetCaseName(m_cases[0]));
			if (m_vt)
				m_var_table_vec.push_back(m_vt); // for parametric simulation
			else
				m_var_table_vec.push_back(&m_cases[0]->Values());
			m_var_info_lookup_vec.push_back(&m_cases[0]->Variables());
		}
		else
		{
			for (std::vector<Case*>::iterator it = m_cases.begin(); it != m_cases.end(); ++it)
			{
				m_col_hdrs.push_back(m_pf->GetCaseName(*it));
				m_var_table_vec.push_back(&(*it)->Values());
				m_var_info_lookup_vec.push_back(&(*it)->Variables());
			}
		}


		m_cols = m_col_hdrs.Count();

		std::set<wxString> var_names;
		// variable names
		//		skip calculated
		for (std::vector<VarInfoLookup*>::iterator it = m_var_info_lookup_vec.begin(); it != m_var_info_lookup_vec.end(); ++it)
		{
			wxArrayString as = (*it)->ListAll();
			for (size_t i = 0; i < as.Count(); i++)
//				if ((!((*it)->Lookup(as[i])->Flags  & VF_CALCULATED)) &&
//					(!((*it)->Lookup(as[i])->Flags  & VF_INDICATOR)))
				if (!((*it)->Lookup(as[i])->Flags  & VF_INDICATOR))
						var_names.insert(as[i]);
		}
/*		all
		for (std::vector<VarTable*>::iterator it = m_var_table_vec.begin(); it != m_var_table_vec.end(); ++it)
		{
			wxArrayString as = (*it)->ListAll();
			for (size_t i = 0; i < as.Count(); i++)
				var_names.insert(as[i]);
		}
*/
		m_rows = var_names.size();
		for (int row = 0; row < m_rows; row++)
			m_sorted_index.Add(row);

		// variable labels
		for (std::set<wxString>::iterator idx = var_names.begin(); idx != var_names.end(); ++idx)
		{
			wxString str_label = " ";
			for (std::vector<VarInfoLookup*>::iterator it = m_var_info_lookup_vec.begin(); it != m_var_info_lookup_vec.end(); ++it)
			{
				if ((*it)->Lookup(*idx))
					str_label = (*it)->Label(*idx);
			}
			m_var_labels.push_back(str_label);
			m_var_names.push_back(*idx);
		}

	}
}

int VariableGridData::GetNumberRows()
{
	return m_rows;
}

int VariableGridData::GetNumberCols()
{
	return m_cols;
}

bool VariableGridData::IsEmptyCell(int row, int col)
{
	if (!GetView()->GetParent()->IsShown()) return true;
	if (col == 0) // variable name
		return (m_var_names[row].IsEmpty());
	else if (col == 1) // variable label
		return (m_var_labels[row].IsEmpty());
	else // get var table and value
	{
		int lookup_row = row;
		if (m_sorted) lookup_row = m_sorted_index[row];
		if (m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]))
		{
			if ((col - 2) >= (int)m_var_table_vec.size())
				return true;
			else
				return (m_var_table_vec[col - 2]->Get(m_var_names[lookup_row])->AsString() == wxEmptyString);
		}
		else
			return true;
	}
}


wxString VariableGridData::GetColLabelValue(int col)
{
	if (col <= (int)m_col_hdrs.size())
		return m_col_hdrs[col];
	else
		return wxEmptyString;
}

wxString VariableGridData::GetVarName(int row, int col)
{
	wxString  ret_val = wxEmptyString;
	if ((col > 0) && (col < (int)m_var_names.Count()) && (row > -1) && (row < m_rows))
	{
		int lookup_row = row;
		if (m_sorted) lookup_row = m_sorted_index[row];
		ret_val = m_var_names[lookup_row];
	}
	return ret_val;
}



VarInfo* VariableGridData::GetVarInfo(int row, int col)
{
	VarInfo* vi = NULL;
	int lookup_row = row;
	if (m_sorted) lookup_row = m_sorted_index[row];
	if ((col > 1) && ((col - 2) <  (int)m_var_info_lookup_vec.size()))
	{
		vi = m_var_info_lookup_vec[col - 2]->Lookup(m_var_names[lookup_row]);
	}
	return vi;
}

void VariableGridData::SetVarInfo(int row, int col, VarInfo *vi)
{
	int lookup_row = row;
	if (m_sorted) lookup_row = m_sorted_index[row];
	if ((col > 1) && ((col - 2) < (int)m_var_info_lookup_vec.size()))
	{
		if (VarInfo *var_info = m_var_info_lookup_vec[col - 2]->Lookup(m_var_names[lookup_row]))
			var_info = vi;
	}
}

VarValue* VariableGridData::GetVarValue(int row, int col)
{
	VarValue* vv = NULL;
	int lookup_row = row;
	if (m_sorted) lookup_row = m_sorted_index[row];
	if ((col > 1) && ((col - 2) <  (int)m_var_table_vec.size()))
	{
		vv = m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]);
	}
	return vv;

}

void VariableGridData::SetVarValue(int row, int col, VarValue *vv)
{
	int lookup_row = row;
	if (m_sorted) lookup_row = m_sorted_index[row];
	if ((col > 1) && ((col - 2) <  (int)m_var_table_vec.size()))
	{
		if (VarValue *var_value = m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]))
			var_value = vv;
	}
}

/*
bool VariableGridData::CanGetValueAs(int row, int col, const wxString &typeName)
{
	if (GetTypeName(row, col) == wxGRID_VALUE_CHOICE)
		return (typeName == wxGRID_VALUE_NUMBER);
}

bool VariableGridData::CanSetValueAs(int row, int col, const wxString &typeName)
{
	if (GetTypeName(row, col) == wxGRID_VALUE_CHOICE)
		return (typeName == wxGRID_VALUE_NUMBER);
}
*/

wxString VariableGridData::GetChoice(int row, int col)
{
	wxString ret_str = wxEmptyString;
	if ((col>-1) && (col < m_cols))
	{
		if (VarInfo *vi = GetVarInfo(row, col))
		{
			wxArrayString as = vi->IndexLabels;
			int ndx = -1;
			double val;
			if (GetValue(row, col).ToDouble(&val)) ndx = int(val);
			if ((as.Count() > 0) && (ndx >= 0) && (ndx < (int)as.Count()))
				ret_str = as[ndx];
		}
	}
	return ret_str;
}


wxString VariableGridData::GetChoices(int row, int col)
{
	wxString ret_str = wxEmptyString;
	int lookup_row = row;
	if (m_sorted) lookup_row = m_sorted_index[row];
	if ( col >= 2 ) // get var table and value
	{
		if ((col - 2) < (int)m_var_info_lookup_vec.size())
		{
			if (m_var_info_lookup_vec[col - 2]->Lookup(m_var_names[lookup_row]))
			{
				wxArrayString as = m_var_info_lookup_vec[col - 2]->Lookup(m_var_names[lookup_row])->IndexLabels;
				if (as.Count() > 0)
				{
					for (int i = 0; i < (int)as.Count() - 1; i++)
						ret_str += as[i] + ",";
					ret_str += as[as.Count() - 1];
				}
			}
		}
	}
	return ret_str;
}

wxString VariableGridData::GetValue(int row, int col)
{
	int lookup_row = row;
	if (m_sorted) lookup_row = m_sorted_index[row];
	if (col == 0) // variable name
		return m_var_names[lookup_row];
	else if (col == 1) // variable label
		return m_var_labels[lookup_row];
	else // get var table and value
	{
		if ((col - 2) >= (int)m_var_table_vec.size())
			return wxEmptyString;
		else
		{
			if (m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]))
				return m_var_table_vec[col - 2]->Get(m_var_names[lookup_row])->AsString();
			else
				return wxEmptyString;
		}
	}
}

void VariableGridData::SetValue(int row, int col, const wxString& value)
{
	if (col == 0)
		return; // no updating of variable name allowed here!
	else if (col == 1)
		return; // no updating of variable label allowed here!
	else if ((col > 1) && (col < (int)(m_cases.size() + 2)))
	{
		int lookup_row = row;
		if (m_sorted) lookup_row = m_sorted_index[row];
		if (m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]))
		{
			VarValue *vv = m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]);
			if (vv) 
			{
				VarValue::Parse(vv->Type(), value, *vv);
				// updates ui from grid
				m_cases[col - 2]->VariableChanged(m_var_names[lookup_row]);
			}
		}
	}
}



void VariableGridData::Sort(int col, bool ascending)
{
	// get all values for column
	m_sorted = false;
	wxArrayString col_values;
	for (int row = 0; row < m_rows; row++)
		col_values.Add(GetValue(row, col));
	wxArrayString sorted_col_value(col_values);
	// sort 
	sorted_col_value.Sort(!ascending);
	// update index to get value
	// TODO - handle same values

	for (int row = 0; row < m_rows; row++)
		m_sorted_index[row] = -1;
	for (int row = 0; row < m_rows; row++)
		{
		int ndx = col_values.Index(sorted_col_value[row]);
		if (m_sorted_index.Index(ndx) != wxNOT_FOUND)
			col_values[ndx] = "|||||||||"; // hopefully not in original list
		ndx = col_values.Index(sorted_col_value[row]);
		m_sorted_index[row] = ndx;
	}
	m_sorted = true;
}


wxGridCellAttr *VariableGridData::GetAttr(int row, int col, wxGridCellAttr::wxAttrKind kind)
{

	wxGridCellAttr *attr = NULL;
	if (GetAttrProvider())
	{
		attr = GetAttrProvider()->GetAttr(row, col, kind);

		if (GetTypeName(row, col) == "GridCellCalculated")
		{
			if (!attr)
			{
				attr = m_attr_for_calculated;
				attr->IncRef();
			}
			else if (!attr->HasBackgroundColour())
			{
					wxGridCellAttr *attrNew = attr->Clone();
					attr->DecRef();
					attr = attrNew;
					attr->SetTextColour(UIColorCalculatedFore);
					attr->SetBackgroundColour(UIColorCalculatedBack);
			}
		}
	}
	return attr;
}


wxString VariableGridData::GetTypeName(int row, int col)
{
	if ((col > -1) && (col < 2))
		return wxGRID_VALUE_STRING;
	else if (col < m_cols)
	{
		int lookup_row = row;
		if (m_sorted) lookup_row = m_sorted_index[row];
		if (VarInfo *var_info = m_var_info_lookup_vec[col - 2]->Lookup(m_var_names[lookup_row]))
		{ // TODO - better control list maintenance here and in UIEditorPanel
			wxString type = var_info->UIObject;
			bool calculated = (var_info->Flags   & VF_CALCULATED) > 0;
			if (calculated)
				return "GridCellCalculated";
			else if (type == "Numeric")
				return wxGRID_VALUE_STRING;
			else if (type == "Choice")
				return "GridCellChoice";
			else if (type == "ListBox")
				return "GridCellVarValue";
			else if (type == "RadioChoice")
				return "GridCellVarValue";
			else if (type == "TextEntry")
				return wxGRID_VALUE_STRING;
			else if (type == "MultilineText")
				return wxGRID_VALUE_STRING;
			else if (type == "Slider")
				return "GridCellVarValue";
			else if (type == "CheckBox")
				return "GridCellCheckBox";
			else if (type == "SchedNumeric")
				return "GridCellVarValue";
			else if (type == "TOUSchedule")
				return "GridCellVarValue";
			else if (type == "PTLayout")
				return "GridCellVarValue";
			else if (type == "MaterialProperties")
				return "GridCellVarValue";
			else if (type == "TroughLoop")
				return "GridCellVarValue";
			else if (type == "MonthlyFactor")
				return "GridCellVarValue";
			else if (type == "SearchListBox")
				return "GridCellVarValue";
			else if (type == "DataArray")
				return "GridCellVarValue";
			else if (type == "DataLifetimeArray")
				return "GridCellVarValue";
			else if (type == "StringArray")
				return "GridCellVarValue";
			else if (type == "DataMatrix")
				return "GridCellVarValue";
			else if (type == "DataLifetimeMatrix")
				return "GridCellVarValue";
			else if (type == "ShadingFactors")
				return "GridCellVarValue";
			else if (type == "ValueMatrix")
				return "GridCellVarValue";
			else if (type == "MonthByHourFactors")
				return "GridCellVarValue";
			else if (type == "Library")
				return "GridCellVarValue";
			else if (type == "LossAdjustment")
				return "GridCellVarValue";
			else if (type == "DiurnalPeriod")
				return "GridCellVarValue";
			else if (var_info->UIObject == VUIOBJ_NONE)
				return wxGRID_VALUE_STRING;
			else
				return wxGRID_VALUE_STRING;
		}
		else
			return wxGRID_VALUE_STRING;
	}
	else
		return wxGRID_VALUE_STRING;
}

bool VariableGridData::ShowRow(int row, int comparison_type, bool show_calculated)
{
	bool show = true;
	if (m_cases.size() > 1) // comparison
	{
		switch (comparison_type)
		{
		case COMPARE_SHOW_DIFFERENT:
		case COMPARE_SHOW_SAME:
		{
				int lookup_row = row;
				if (m_sorted) lookup_row = m_sorted_index[row];
				if (m_var_table_vec[0]->Get(m_var_names[lookup_row]))
				{
					VarValue *vv = m_var_table_vec[0]->Get(m_var_names[lookup_row]);
					bool row_varvalues_same = true;
					for (int col = 1; col < (int)m_cases.size(); col++)
					{
						if (m_var_table_vec[col]->Get(m_var_names[lookup_row]))
						{
							VarValue *vv_new = m_var_table_vec[col]->Get(m_var_names[lookup_row]);
							if (vv->Type() == vv_new->Type())
								row_varvalues_same = (row_varvalues_same && (vv->ValueEqual(*vv_new)));
							else
							{
								show = false; // different variable type in row - should not happen
								continue;
							}
						}
						else
						{
							show = false; // no variable value for (row, col)
							continue;
						}
					}
					if (comparison_type == COMPARE_SHOW_DIFFERENT)
						show = (show && !row_varvalues_same);
					else
						show = (show && row_varvalues_same);
				}
				else
					show = false; // no variable value for (row, col=0)
			}
			break;
		case COMPARE_SHOW_ALL:
		default:
			break;
		}
	}
	// check for calculated inputs
	if (show)
	{
		bool calculated = false;
		int lookup_row = row;
		if ((row < (int)m_sorted_index.Count()) && (m_sorted)) lookup_row = m_sorted_index[row];
		if ((lookup_row < (int)m_var_names.Count()) && (m_var_info_lookup_vec[0]->Lookup(m_var_names[lookup_row])))
		{
			VarInfo *vi = m_var_info_lookup_vec[0]->Lookup(m_var_names[lookup_row]);
			if (vi)
				calculated = (vi->Flags & VF_CALCULATED) > 0 ;
			if (calculated) show = show && show_calculated;
		}
	}
	return show;
}

bool VariableGridData::DeleteCase(Case *c)
{
	std::vector<Case*>::iterator it = std::find(m_cases.begin(), m_cases.end(), c);
	if (it != m_cases.end())
	{
		m_cases.erase(it);
		Init();
		DeleteCols();
		return true;
	}
	return false;
}


bool VariableGridData::DeleteCols(size_t pos, size_t numCols)
{
	if (GetView())
	{
		wxGridTableMessage msg(this,
			wxGRIDTABLE_NOTIFY_COLS_DELETED,
			pos,
			numCols);

		GetView()->ProcessTableMessage(msg);
	}

	return true;
}

bool VariableGridData::AddCase(Case *c)
{
	std::vector<Case*>::iterator it = std::find(m_cases.begin(), m_cases.end(), c);
	if (it == m_cases.end())
	{
		m_cases.push_back(c);
		Init();
		AppendCols();
		return true;
	}
	return false;
}


bool VariableGridData::AppendCols(size_t numCols)
{
	if (GetView())
	{
		wxGridTableMessage msg(this,
			wxGRIDTABLE_NOTIFY_COLS_APPENDED,
			numCols);

		GetView()->ProcessTableMessage(msg);
	}

	return true;
}


bool VariableGridData::RenameCase(const wxString &old_name, const wxString &new_name)
{
	int ndx = m_col_hdrs.Index(old_name);
	if (ndx == wxNOT_FOUND)
		return false;
	else
	{
		SetColLabelValue(ndx, new_name);
		return true;
	}
}

void VariableGridData::SetColLabelValue(int col, const wxString &label)
{
	if ((col > 0) && (col < (int)m_col_hdrs.Count()))
	{
		m_col_hdrs[col] = label;
	}
}




////////////////////////////////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(VariableGrid, wxExtGridCtrl)
EVT_GRID_CELL_LEFT_CLICK(VariableGrid::OnLeftClick)
END_EVENT_TABLE()

VariableGrid::VariableGrid(wxWindow *parent, wxWindowID id, const wxPoint &pos, const wxSize &size, long , const wxString &) : wxExtGridCtrl(parent, id, pos, size)
{
}

VariableGrid::~VariableGrid()
{
}

void VariableGrid::OnLeftClick(wxGridEvent &evt)
{
	SetGridCursor(evt.GetRow(), evt.GetCol());
	evt.Skip();
}

////////////////////////////////////////////////////////////////////////////////////////

enum {
	__idFirst = wxID_HIGHEST + 992,
	ID_SHOW_DIFFERENT, ID_SHOW_SAME, ID_SHOW_ALL, ID_EXP_CLIPBOARD, ID_EXP_CSV, ID_EXP_EXCEL, ID_EXP_BTN, ID_VIEW_BTN, ID_FILTER, ID_SHOW_CALCULATED
};

BEGIN_EVENT_TABLE(VariableGridFrame, wxFrame)
	EVT_MENU(ID_SHOW_DIFFERENT, VariableGridFrame::OnCommand)
	EVT_MENU(ID_SHOW_SAME, VariableGridFrame::OnCommand)
	EVT_MENU(ID_SHOW_ALL, VariableGridFrame::OnCommand)
	EVT_MENU(ID_SHOW_CALCULATED, VariableGridFrame::OnCommand)
	EVT_MENU(ID_EXP_CLIPBOARD, VariableGridFrame::OnCommand)
	EVT_MENU(ID_EXP_CSV, VariableGridFrame::OnCommand)
	EVT_MENU(ID_EXP_EXCEL, VariableGridFrame::OnCommand)
	EVT_BUTTON(ID_EXP_BTN, VariableGridFrame::OnCommand)
	EVT_BUTTON(ID_VIEW_BTN, VariableGridFrame::OnCommand)
	EVT_BUTTON(wxID_HELP, VariableGridFrame::OnCommand)
	EVT_TEXT( ID_FILTER, VariableGridFrame::OnCommand)	
	EVT_GRID_COL_SORT(VariableGridFrame::OnGridColSort)
//	EVT_SHOW(VariableGridFrame::OnShow)
END_EVENT_TABLE()

VariableGridFrame::VariableGridFrame(wxWindow *parent, ProjectFile *pf, Case *c, VarTable *vt, wxString frame_title) 
	: wxFrame(parent, wxID_ANY, "Variable Grid", wxDefaultPosition, wxScaleSize(800, 700)), m_pf(pf)
{
	m_show_calculated = false;
	SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

	if (!m_pf) return;

	if (!vt) m_pf->AddListener(this); // no listeners when using parametric var tables

	if (c)
	{
		m_cases.push_back(c);
		m_input_list = true; // input list - do not allow for cases to be added
	}
	else
	{
		m_cases = m_pf->GetCases();
		m_input_list = false;
	}

	if (m_cases.size() > 0)
	{
		if (!vt)
		{
			for (size_t i = 0; i < m_cases.size(); i++)
				m_cases[i]->AddListener(this);
		}

		wxString title=frame_title;
		if (title.IsEmpty())
		{
			if (m_cases.size() == 1)
				title = "Inputs Browser"; //Current Case Values: " + m_pf->GetCaseName(m_cases[0]);
			else
				title = "Inputs Browser"; //Case comparison";
		}

		SetTitle(title);

		m_griddata = new VariableGridData(m_pf, c, vt);

		m_grid = new VariableGrid(this, wxID_ANY);

		m_grid->RegisterDataType("GridCellCheckBox", new GridCellCheckBoxRenderer, new GridCellCheckBoxEditor);
		m_grid->RegisterDataType("GridCellChoice", new GridCellChoiceRenderer, new GridCellChoiceEditor);
		m_grid->RegisterDataType("GridCellVarValue", new GridCellVarValueRenderer, new GridCellVarValueEditor);

		m_grid->RegisterDataType("GridCellCalculated", new GridCellCalculatedRenderer, new GridCellCalculatedEditor);

		m_grid->HideRowLabels();

		m_grid->SetTable(m_griddata, true, wxGrid::wxGridSelectRows);


		
//		wxBoxSizer *comparetools = new wxBoxSizer(wxHORIZONTAL);

		wxBoxSizer *tools = new wxBoxSizer(wxHORIZONTAL);
		m_btn_export = new wxMetroButton(this, ID_EXP_BTN, "Export", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_DOWNARROW);
		tools->Add(m_btn_export, 0, wxALL | wxEXPAND, 0);
		
		m_btn_view = new wxMetroButton(this, ID_VIEW_BTN, "View", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_DOWNARROW);
		tools->Add( m_btn_view, 0, wxALL|wxEXPAND, 0 );
		
		tools->AddSpacer(5);
		m_filter = new wxTextCtrl( this, ID_FILTER );
		wxStaticText *lblfilter = new wxStaticText( this, wxID_ANY, "Search:" );
		lblfilter->SetForegroundColour( *wxWHITE );
		lblfilter->SetFont( wxMetroTheme::Font( wxMT_NORMAL ) );
		tools->Add( lblfilter, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
		tools->Add( m_filter, 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
		tools->AddStretchSpacer();
		tools->Add(new wxMetroButton(this, wxID_HELP, "Help"), 0, wxALL | wxEXPAND, 0);

		m_sizer = new wxBoxSizer(wxVERTICAL);
		m_sizer->Add(tools, 0, wxALL | wxEXPAND, 0);
		m_sizer->Add(m_grid, 1, wxALL | wxEXPAND, 0);
		SetSizer(m_sizer);

		SizeColumns();

		m_compare_show_type = m_cases.size() > 1 ? COMPARE_SHOW_DIFFERENT : COMPARE_SHOW_ALL;

		UpdateGrid();

	}
#ifdef __WXMSW__
	SetIcon(wxICON(appicon));
#endif	
	CenterOnParent();
	Show();

}

void VariableGridFrame::OnShow(wxShowEvent& )
{
	m_griddata->Sort(0, !(m_grid->IsSortingBy(0) &&
		m_grid->IsSortOrderAscending()));
	UpdateGrid();
}


VariableGridFrame::~VariableGridFrame()
{
	/*Strange issue with VS2013 in release mode only*/
	/*The following code caused AV when exiting with input list open*/
	/*No issue running in debug mode or outside of VS2013*/
	
	std::vector<Case*> pfcases;
	
	if ( m_pf != 0 )
	{
		pfcases = m_pf->GetCases();
		m_pf->RemoveListener(this);
	}

	for (size_t i = 0; i < m_cases.size(); i++)
		if ( std::find( pfcases.begin(), pfcases.end(), m_cases[i] ) != pfcases.end() )
			m_cases[i]->RemoveListener(this);
}

void VariableGridFrame::GetTextData(wxString &dat, char sep)
{
	dat = wxEmptyString;
	if (!m_grid)
		return;

	size_t approxbytes = m_griddata->GetNumberRows() * 15 * m_griddata->GetNumberCols();
	dat.Alloc(approxbytes);

	int c;

	for (c = 0; c<m_griddata->GetNumberCols(); c++)
	{
		wxString label = m_griddata->GetColLabelValue(c);
		label.Replace('\n', " | ");

		if (sep == ',')
			dat += '"' + label + '"';
		else
			dat += label;

		if (c < m_griddata->GetNumberCols() - 1)
			dat += sep;
		else
			dat += '\n';
	}

	for (int r = 0; r<m_griddata->GetNumberRows(); r++)
	{
		if (m_grid->IsRowShown(r))
		{
			for (c = 0; c < m_griddata->GetNumberCols(); c++)
			{
				// choice values - can handle hourly and monthly similarly
				if (m_griddata->GetTypeName(r, c) == "GridCellChoice")

					dat += m_griddata->GetChoice(r, c);
				else
					dat += m_griddata->GetValue(r, c);

				if (c < m_griddata->GetNumberCols() - 1)
					dat += sep;
				else
					dat += '\n';
			}
		}
	}
}


void VariableGridFrame::CopyToClipboard()
{
	wxBusyInfo busy("Processing data table... please wait");
	wxString dat;
	GetTextData(dat, '\t');

	// strip commas per request from Paul 5/23/12 meeting
	dat.Replace(",", "");

	if (wxTheClipboard->Open())
	{
		wxTheClipboard->SetData(new wxTextDataObject(dat));
		wxTheClipboard->Close();
	}
}

void VariableGridFrame::SaveToCSV()
{
	wxFileDialog fdlg(this, "Save as CSV", wxEmptyString, "inputs.csv", "Comma-separated values (*.csv)|*.csv", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
	if (fdlg.ShowModal() != wxID_OK) return;

	FILE *fp = fopen(fdlg.GetPath().c_str(), "w");
	if (!fp)
	{
		wxMessageBox("Could not open file for write:\n\n" + fdlg.GetPath());
		return;
	}

	wxBusyInfo busy("Writing CSV file... please wait");

	wxString dat;
	GetTextData(dat, ',');
	fputs(dat.c_str(), fp);
	fclose(fp);

}

void VariableGridFrame::SendToExcel()
{
	wxBusyInfo busy("Processing data table... please wait");
	wxString dat;
	GetTextData(dat, '\t');

	// strip commas per request from Paul 5/23/12 meeting
	dat.Replace(",", "");

#ifdef __WXMSW__
	wxExcelAutomation xl;
	if (!xl.StartExcel())
	{
		wxMessageBox("Could not start Excel.");
		return;
	}

	xl.Show(true);

	if (!xl.NewWorkbook())
	{
		wxMessageBox("Could not create a new Excel worksheet.");
		return;
	}
	if (wxTheClipboard->Open())
	{
		wxTheClipboard->SetData(new wxTextDataObject(dat));
		wxTheClipboard->Close();
		xl.PasteClipboard();
	}
#endif
}




void VariableGridFrame::OnGridColSort(wxGridEvent& event)
{
	const int col = event.GetCol();
	m_griddata->Sort(col, !(m_grid->IsSortingBy(col) &&
		m_grid->IsSortOrderAscending()));
	UpdateGrid();
}



void VariableGridFrame::SizeColumns()
{
	// column widths
	int col = 0, row = 0;
	int width, height;
	std::vector<int> col_width(m_grid->GetNumberCols(), 60);
	for (row = 0; row< m_grid->GetNumberRows(); row++)
	{
		if (m_griddata->ShowRow(row, m_compare_show_type, m_show_calculated))
		{
			for (col = 0; col < m_grid->GetNumberCols(); col++)
			{
				GetTextExtent(m_grid->GetCellValue(row, col), &width, &height);
				if ((width + 10) > col_width[col]) col_width[col] = width + 10;
			}
		}
	}

	for (col = 0; col < m_grid->GetNumberCols(); col++)
	{
		GetTextExtent(m_grid->GetColLabelValue(col), &width, &height);
		if ((width + 10) > col_width[col]) col_width[col] = width + 10;
		if (col_width[col] > 250) col_width[col] = 250;
		m_grid->SetColSize(col, col_width[col]);
	}
}

void VariableGridFrame::UpdateGrid()
{
	wxString filter(m_filter->GetValue().Lower());
	m_grid->Freeze();
	for (int row = 0; row < m_grid->GetNumberRows(); row++)
	{
		bool show = true;
		if ( !filter.IsEmpty() )
		{
			wxString target( m_griddata->GetValue( row, 0 ).Lower() + " " + m_griddata->GetValue( row, 1 ).Lower() );
			show = (filter.Len() <= 2 && target.Left( filter.Len() ).Lower() == filter)
				|| (target.Lower().Find( filter ) >= 0);
		}

		if (show && m_griddata->ShowRow(row, m_compare_show_type, m_show_calculated))
		{
			m_grid->ShowRow(row);
		}
		else
			m_grid->HideRow(row);
	}
	m_grid->Thaw();
	m_grid->ForceRefresh();
}

void VariableGridFrame::OnCommand(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_VIEW_BTN:
		{
			wxMetroPopupMenu menu;
			if ( m_cases.size() >= 2 )
			{
				menu.AppendCheckItem(ID_SHOW_DIFFERENT, "Show different values", m_compare_show_type == COMPARE_SHOW_DIFFERENT);
				menu.AppendCheckItem(ID_SHOW_SAME, "Show equal values", m_compare_show_type == COMPARE_SHOW_SAME);
				menu.AppendCheckItem(ID_SHOW_ALL, "Show all values", m_compare_show_type == COMPARE_SHOW_ALL);		
				menu.AppendSeparator();
			}
			menu.AppendCheckItem(ID_SHOW_CALCULATED, "Show calculated values", m_show_calculated );
			wxPoint p = m_btn_view->ClientToScreen( wxPoint( 0, m_btn_view->GetClientSize().y ) );
			menu.Popup( m_btn_view, p, wxTOP|wxLEFT );
		}
		break;
	case ID_EXP_BTN:
		{				   
			wxMetroPopupMenu menu;
			menu.Append(ID_EXP_CLIPBOARD, "Copy to clipboard");
			menu.Append(ID_EXP_CSV, "Save as CSV");
		#ifdef __WXMSW__
			menu.Append(ID_EXP_EXCEL, "Send to Excel");
		#endif

			wxPoint p = m_btn_export->ClientToScreen( wxPoint( 0, m_btn_export->GetClientSize().y ) );
			menu.Popup( m_btn_export, p, wxTOP|wxLEFT );
		}
		break;
	case ID_SHOW_DIFFERENT:
		m_compare_show_type = COMPARE_SHOW_DIFFERENT;
		UpdateGrid();
		break;
	case ID_SHOW_SAME:
		m_compare_show_type = COMPARE_SHOW_SAME;
		UpdateGrid();
		break;
	case ID_SHOW_ALL:
		m_compare_show_type = COMPARE_SHOW_ALL;
		UpdateGrid();
		break;
	case ID_EXP_CLIPBOARD:
		CopyToClipboard();
		break;
	case ID_EXP_CSV:
		SaveToCSV();
		break;
	case ID_EXP_EXCEL:
		SendToExcel();
		break;
	case wxID_HELP:
		SamApp::ShowHelp( "inputs_browser" );
		break;
	case ID_FILTER:
		UpdateGrid();
		break;
	case ID_SHOW_CALCULATED:
		m_show_calculated = !m_show_calculated;
		UpdateGrid();
		break;
	}

}

void VariableGridFrame::OnProjectFileEvent(ProjectFile* WXUNUSED(p), ProjectFileEvent &evt)
{
	if (evt.GetType() == ProjectFileEvent::CASE_DELETED)
	{
		Case *c = m_pf->GetCase(evt.GetString());
		std::vector<Case*>::iterator it = std::find(m_cases.begin(), m_cases.end(), c);
		if (it != m_cases.end())
		{
			m_cases.erase(it);
			if (m_cases.size() == 0) 
			{
				Close(); // AV when closeing main window protection
				return;
			}
			m_griddata->DeleteCase(c);
			m_grid->Refresh();
			UpdateGrid();
		}
	}
	else if (evt.GetType() == ProjectFileEvent::CASE_ADDED)
	{
		Case *c = m_pf->GetCase(evt.GetString());
		std::vector<Case*>::iterator it = std::find(m_cases.begin(), m_cases.end(), c);
		if ((!m_input_list) && (it == m_cases.end()))
		{
			m_cases.push_back(c);
			m_griddata->AddCase(c);
			m_grid->Refresh();
			UpdateGrid();
		}
	}
	else if (evt.GetType() == ProjectFileEvent::CASE_RENAMED)
	{
		Case *c = m_pf->GetCase(evt.GetString());
		std::vector<Case*>::iterator it = std::find(m_cases.begin(), m_cases.end(), c);
		if (it == m_cases.end())
		{
			m_griddata->RenameCase(evt.GetString(), evt.GetString2());
			m_grid->Refresh();
			UpdateGrid();
		}
	}
	else if (evt.GetType() == ProjectFileEvent::PROJECTFILE_DELETED)
	{
		Close(); // to prevent AV when closing main window
	}

}

void VariableGridFrame::OnCaseEvent(Case* WXUNUSED(c), CaseEvent &evt)
{
	if ( evt.GetType() == CaseEvent::VALUE_USER_INPUT )
	{
		// refresh when any case values change
		UpdateGrid(); // for comparison views
		m_grid->ForceRefresh();
	}
}



