#include <algorithm>
#include <set>


#include "variablegrid.h"

VariableGridData::VariableGridData(ProjectFile &project)
{

	m_cases = project.GetCases();
	if (m_cases.size() > 0)
	{
		if (m_cases.size() == 1)
		{
			m_col_hdrs.push_back("Variable");
			m_col_hdrs.push_back("Label");
			wxString case_name = project.GetCaseName(m_cases[0]);
			m_col_hdrs.push_back(case_name);
			m_var_table_vec.push_back(m_cases[0]->Values());
			m_var_info_lookup_vec.push_back(m_cases[0]->Variables());
		}
		else
		{
			m_col_hdrs = project.GetCaseNames();
			m_col_hdrs.Insert("Label", 0);
			m_col_hdrs.Insert("Variable", 0);
			for (std::vector<Case*>::iterator it = m_cases.begin(); it != m_cases.end(); ++it)
			{
				m_var_table_vec.push_back((*it)->Values());
				m_var_info_lookup_vec.push_back((*it)->Variables());
			}
		}


		m_cols = m_col_hdrs.Count();

		std::set<wxString> var_names;
		// variable names
		for (std::vector<VarTable>::iterator it = m_var_table_vec.begin(); it != m_var_table_vec.end(); ++it)
		{
			wxArrayString as = it->ListAll();
			for (size_t i = 0; i < as.Count(); i++)
				var_names.insert(as[i]);
		}

		m_rows = var_names.size();

		// variable labels
		for (std::set<wxString>::iterator idx = var_names.begin(); idx != var_names.end(); ++idx)
		{
			wxString str_label = " ";
			for (std::vector<VarInfoLookup>::iterator it = m_var_info_lookup_vec.begin(); it != m_var_info_lookup_vec.end(); ++it)
			{
				if ((*it).Lookup(*idx))
					str_label = (*it).Label(*idx);
			}
			m_var_labels.push_back(str_label);
			m_var_names.push_back(*idx);
		}

		/*
		// wxGrid only support 6500 characters per cell (empirically determined) - use 1024 for display
		size_t col = 0, row = 0;
		int width, height;
		std::vector<int> col_width(num_cols, 60);
		for (std::set<wxString>::iterator idx = var_names.begin(); idx != var_names.end(); ++idx)
		{
			GetTextExtent(*idx, &width, &height);
			if ((width + 10) > col_width[col]) col_width[col] = width + 10;
			grid->SetCellValue(row, col++, *idx); //name
			if (row < var_labels.Count())
			{
				GetTextExtent(var_labels[row], &width, &height);
				if ((width + 10) > col_width[col]) col_width[col] = width + 10;
				grid->SetCellValue(row, col++, var_labels[row]); //label
			}
			for (std::vector<VarTable>::iterator it = var_table_vec.begin(); it != var_table_vec.end(); ++it)
			{
				wxString str_val = "";
				if (it->Get(*idx)) str_val = it->Get(*idx)->AsString();
				if (str_val.Length() > 1024) str_val = str_val.Left(1024) + "...";
				grid->SetCellValue(row, col++, str_val);
			}
			row++;
			col = 0;
		}

		// go through all rows for case comparison and only show unequal values
		if (m_cases.size() > 1)
		{
			for (row = 0; row < num_rows; row++)
			{
				wxString str_val = grid->GetCellValue(row, 2);
				bool same_val = true;
				for (col = 3; col < num_cols; col++)
					same_val = same_val && (str_val == grid->GetCellValue(row, col));
				if (same_val) grid->HideRow(row);
			}
		}
		//grid->AutoSizeColumns();
		// column headers
		for (col = 0; col < col_hdrs.Count(); col++)
		{
			grid->SetColLabelValue(col, col_hdrs[col]);
			GetTextExtent(col_hdrs[col], &width, &height);
			if ((width + 10) > col_width[col]) col_width[col] = width + 10;
			grid->SetColumnWidth(col, col_width[col]);
		}
		grid->Thaw();

		frame->Show();
		*/
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
	if (col == 0) // variable name
		return m_var_names[row];
	else if (col == 1) // variable label
		return m_var_labels[row];
	else // get var table and value
		return m_var_table_vec[col-2].Get(m_var_names[row])->AsString();
}

void VariableGridData::SetValue(int row, int col, const wxString& value)
{ // must update labels and values as necessary
	// TODO
}


VariableGridFrame::VariableGridFrame(wxWindow *parent, ProjectFile &project) : wxFrame(parent, wxID_ANY, "Variable Grid", wxDefaultPosition, wxSize(400, 700))
{

	if (project.GetCases().size() > 0)
	{
		wxString title;
		if (project.GetCases().size() == 1)
		{
			wxString case_name = project.GetCaseName(project.GetCases()[0]);
			title = "Current Case Values: " + case_name;
		}
		else
		{
			title = "Case comparison";
		}
		
		SetTitle(title);
		m_griddata = new VariableGridData(project);

		m_grid = new wxGrid(this, wxID_ANY);
		m_grid->Freeze();
		m_grid->SetTable(m_griddata, true, wxGrid::wxGridSelectRows);
		/*
		// wxGrid only support 6500 characters per cell (empirically determined) - use 1024 for display
		size_t col = 0, row = 0;
		int width, height;
		std::vector<int> col_width(num_cols, 60);
		for (std::set<wxString>::iterator idx = var_names.begin(); idx != var_names.end(); ++idx)
		{
			GetTextExtent(*idx, &width, &height);
			if ((width + 10) > col_width[col]) col_width[col] = width + 10;
			grid->SetCellValue(row, col++, *idx); //name
			if (row < var_labels.Count())
			{
				GetTextExtent(var_labels[row], &width, &height);
				if ((width + 10) > col_width[col]) col_width[col] = width + 10;
				grid->SetCellValue(row, col++, var_labels[row]); //label
			}
			for (std::vector<VarTable>::iterator it = var_table_vec.begin(); it != var_table_vec.end(); ++it)
			{
				wxString str_val = "";
				if (it->Get(*idx)) str_val = it->Get(*idx)->AsString();
				if (str_val.Length() > 1024) str_val = str_val.Left(1024) + "...";
				grid->SetCellValue(row, col++, str_val);
			}
			row++;
			col = 0;
		}

		// go through all rows for case comparison and only show unequal values
		if (m_cases.size() > 1)
		{
			for (row = 0; row < num_rows; row++)
			{
				wxString str_val = grid->GetCellValue(row, 2);
				bool same_val = true;
				for (col = 3; col < num_cols; col++)
					same_val = same_val && (str_val == grid->GetCellValue(row, col));
				if (same_val) grid->HideRow(row);
			}
		}
		//grid->AutoSizeColumns();
		// column headers
		for (col = 0; col < col_hdrs.Count(); col++)
		{
			grid->SetColLabelValue(col, col_hdrs[col]);
			GetTextExtent(col_hdrs[col], &width, &height);
			if ((width + 10) > col_width[col]) col_width[col] = width + 10;
			grid->SetColumnWidth(col, col_width[col]);
		}
		*/
		m_grid->Thaw();

		Show();
	}
}