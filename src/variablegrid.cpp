#include <algorithm>
#include <set>

#include <wx/sizer.h>
#include <wx/bitmap.h>

#include <wex/metro.h>

#include "variablegrid.h"

#define COMPARE_SHOW_ALL 0
#define COMPARE_SHOW_DIFFERENT 1
#define COMPARE_SHOW_SAME 2

VariableGridData::VariableGridData(std::vector<Case *> &cases, wxArrayString &case_names)
{
	wxArrayString to_remove;
	m_sorted = false;
	m_cases = cases;
	if (m_cases.size() > 0)
	{
		m_col_hdrs.push_back("Variable");
		m_col_hdrs.push_back("Label");
		if (m_cases.size() == 1)
		{
			m_col_hdrs.push_back(case_names[0]);
			m_var_table_vec.push_back(&m_cases[0]->Values());
			m_var_info_lookup_vec.push_back(&m_cases[0]->Variables());
			// TODO: skip calculated value
		}
		else
		{
			int i = 0;
			for (std::vector<Case*>::iterator it = m_cases.begin(); it != m_cases.end(); ++it)
			{
				m_col_hdrs.push_back(case_names[i++]);
				m_var_table_vec.push_back(&(*it)->Values());
				m_var_info_lookup_vec.push_back(&(*it)->Variables());
			}
		}


		m_cols = m_col_hdrs.Count();

		std::set<wxString> var_names;
		// variable names
		for (std::vector<VarTable*>::iterator it = m_var_table_vec.begin(); it != m_var_table_vec.end(); ++it)
		{
			wxArrayString as = (*it)->ListAll();
			for (size_t i = 0; i < as.Count(); i++)
				var_names.insert(as[i]);
		}

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

/*
bool VariableGridData::IsEmptyCell(int row, int col)
{
	if (col == 0) // variable name
		return (m_var_names[row]==wxEmptyString);
	else if (col == 1) // variable label
		return (m_var_labels[row] == wxEmptyString);
	else // get var table and value
		return (m_var_table_vec[col].Get(m_var_names[row])->AsString() == wxEmptyString);
}
*/

wxString VariableGridData::GetColLabelValue(int col)
{
	return m_col_hdrs[col];
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
		if (m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]))
			return m_var_table_vec[col - 2]->Get(m_var_names[lookup_row])->AsString();
		else
			return wxEmptyString;
	}
}

void VariableGridData::SetValue(int row, int col, const wxString& value)
{ // must update labels and values as necessary
	// TODO - update label 
	if (col == 0)
		return; // no updating of variable name allowed here!
	else if (col == 1)
		// TODO - update label and update m_var_labels
		return;
	else if ((col > 1) && (col < (m_cases.size()+2)))
	{
		int lookup_row = row;
		if (m_sorted) lookup_row = m_sorted_index[row];
		if (m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]))
		{
			// fails to update case value
			VarValue *vv = m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]);
			//VarValue *vv = m_cases[col - 2]->Values().Get(m_var_names[lookup_row]);
			if (vv) // TODO - check for table updating
			{
				VarValue::Parse(vv->Type(), value, *vv);
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


wxString VariableGridData::GetTypeName(int row, int col)
{
	if ((col > -1) && (col < 2))
		return wxGRID_VALUE_STRING;
	else if (col < m_cols)
	{
		int lookup_row = row;
		if (m_sorted) lookup_row = m_sorted_index[row];
		if (m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]))
		{
			VarValue *vv = m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]);
			switch (vv->Type())
			{
			case VV_ARRAY:
			case VV_MATRIX:
			case VV_TABLE:
				return "autowrapstring";
				break;
			case VV_NUMBER:
			case VV_STRING:
			default:
				return wxGRID_VALUE_STRING;
				break;
			}
			return m_var_table_vec[col - 2]->Get(m_var_names[lookup_row])->AsString();
		}
		else
			return wxGRID_VALUE_STRING;
	}
	else
		return wxGRID_VALUE_STRING;
}

bool VariableGridData::ShowRow(int row, int comparison_type)
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
					int row_var_type = vv->Type();
					bool row_varvalues_same = true;
					for (int col = 1; col < m_cases.size(); col++)
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
	return show;
}



enum {
	__idFirst = wxID_HIGHEST + 992,
	ID_SHOW_DIFFERENT, ID_SHOW_SAME, ID_SHOW_ALL
};

BEGIN_EVENT_TABLE(VariableGridFrame, wxFrame)
	EVT_BUTTON(ID_SHOW_DIFFERENT, VariableGridFrame::OnCommand)
	EVT_BUTTON(ID_SHOW_SAME, VariableGridFrame::OnCommand)
	EVT_BUTTON(ID_SHOW_ALL, VariableGridFrame::OnCommand)

	EVT_GRID_COL_SORT(VariableGridFrame::OnGridColSort)
END_EVENT_TABLE()

VariableGridFrame::VariableGridFrame(wxWindow *parent, std::vector<Case *> &cases, wxArrayString &case_names) : wxFrame(parent, wxID_ANY, "Variable Grid", wxDefaultPosition, wxSize(400, 700)), m_cases(cases)
{

	if (m_cases.size() > 0)
	{
		for (size_t i = 0; i < m_cases.size(); i++)
			m_cases[i]->AddListener(this);

		wxString title;
		if (m_cases.size() == 1)
			title = "Current Case Values: " + case_names[0];
		else
			title = "Case comparison";
		
		SetTitle(title);

		m_griddata = new VariableGridData(m_cases, case_names);

		m_grid = new wxGrid(this, wxID_ANY);




//		m_grid->UseNativeColHeader(); // does not load correctly
		m_grid->RegisterDataType("autowrapstring", new wxGridCellAutoWrapStringRenderer, new wxGridCellAutoWrapStringEditor);
		m_grid->HideRowLabels();




		m_grid->Freeze();
		m_grid->SetTable(m_griddata, true, wxGrid::wxGridSelectRows);
//		m_grid->AutoSize(); // fails with autowrap string renderer


		// column widths
		int col = 0, row = 0;
		int width, height;
		std::vector<int> col_width(m_grid->GetNumberCols(), 60);
		for (row = 0; row< m_grid->GetNumberRows(); row++)
		{
			if (m_griddata->GetTypeName(row, 2) != "autowrapstring")
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
			m_grid->SetColumnWidth(col, col_width[col]);
		}

		/*
		// TODO - radio button group for show all, same, different
		// go through all rows for case comparison and only show unequal values
		// skip same values or all empty strings
		if (m_cases.size() > 1)
		{
			for (int row = 0; row < m_grid->GetNumberRows(); row++)
			{
				wxString str_val = m_grid->GetCellValue(row, 2);
				bool same_val = true;
				bool empty_val = (m_grid->GetCellValue(row, 2) == wxEmptyString);
				for (int col = 3; col < m_grid->GetNumberCols(); col++)
				{
					same_val = same_val && (str_val == m_grid->GetCellValue(row, col));
					empty_val = empty_val || (m_grid->GetCellValue(row, col) == wxEmptyString);
				}
				if (same_val || empty_val) m_grid->HideRow(row);
			}
		}
		*/
		UpdateGrid();
		m_grid->Thaw();


		wxBoxSizer *tools = new wxBoxSizer(wxHORIZONTAL);
		tools->Add(new wxMetroButton(this, ID_SHOW_DIFFERENT, "Show differences"), wxALL | wxEXPAND, 0);
		tools->Add(new wxMetroButton(this, ID_SHOW_SAME, "Show equal values"), wxALL | wxEXPAND, 0);
		tools->Add(new wxMetroButton(this, ID_SHOW_ALL, "Show all"), wxALL | wxEXPAND, 0);


		wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
		sizer->Add(tools, 0, wxALL | wxEXPAND, 0);
		sizer->Add(m_grid, 1, wxALL | wxEXPAND, 0);
		SetSizer(sizer);

		m_compare_show_type = COMPARE_SHOW_ALL;

		if (m_cases.size() < 2)
			tools->Show(false);
	}
}

void VariableGridFrame::OnGridColSort(wxGridEvent& event)
{
	m_grid->Freeze();
	const int col = event.GetCol();
	m_griddata->Sort(col, !(m_grid->IsSortingBy(col) &&
		m_grid->IsSortOrderAscending()));
	UpdateGrid();
	m_grid->Thaw();
}


void VariableGridFrame::UpdateGrid()
{
	for (int row = 0; row < m_grid->GetNumberRows(); row++)
	{
		if (m_griddata->ShowRow(row, m_compare_show_type))
		{
			m_grid->ShowRow(row);
			bool big_height = false;
			for (int col = 2; col < m_grid->GetNumberCols(); col++)
				big_height = (big_height || (m_griddata->GetTypeName(row, 2) == "autowrapstring"));

			if (big_height)
				m_grid->SetRowHeight(row, 10 * m_grid->GetDefaultRowSize());
			else
				m_grid->SetRowHeight(row, m_grid->GetDefaultRowSize());
		}
		else
			m_grid->HideRow(row);
	}
}

void VariableGridFrame::OnCommand(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_SHOW_DIFFERENT:
		m_compare_show_type = COMPARE_SHOW_DIFFERENT;
		m_grid->Freeze();
		UpdateGrid();
		m_grid->Thaw();
		break;
	case ID_SHOW_SAME:
		m_compare_show_type = COMPARE_SHOW_SAME;
		m_grid->Freeze();
		UpdateGrid();
		m_grid->Thaw();
		break;
	case ID_SHOW_ALL:
		m_compare_show_type = COMPARE_SHOW_ALL;
		m_grid->Freeze();
		UpdateGrid();
		m_grid->Thaw();
		break;
	}

}

void VariableGridFrame::OnCaseEvent(Case *, CaseEvent &evt)
{
	if (evt.GetType() == CaseEvent::VALUE_CHANGED)
	{
		// refresh when any case values change
		UpdateGrid(); // for comparison views
		m_grid->Refresh();
	}
}