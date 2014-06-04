#include <algorithm>
#include <set>

#include <wx/sizer.h>
#include <wx/bitmap.h>
#include <wx/msgdlg.h>
#include <wx/tokenzr.h>

#include <wex/metro.h>

#include "variablegrid.h"
#include "widgets.h"
#include "inputpage.h"
#include "object.h"

#define COMPARE_SHOW_ALL 0
#define COMPARE_SHOW_DIFFERENT 1
#define COMPARE_SHOW_SAME 2

VariableGridData::VariableGridData(ProjectFile *pf, Case *c)
{
	m_pf = pf;
	m_sorted = false;
	if (c)
		m_cases.push_back(c);
	else
		m_cases = m_pf->GetCases();
	Init();
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
				if ((!((*it)->Lookup(as[i])->Flags  & VF_CALCULATED)) && 
					(!((*it)->Lookup(as[i])->Flags  & VF_INDICATOR)) )
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
			if (type == "Numeric")
				return wxGRID_VALUE_STRING;
			else if (type == "Choice")
				return "GridCellChoice";
			else if (type == "ListBox")
				return "GridCellVarValue";
			else if (type == "RadioChoice")
				return "GridCellVarValue";
			else if (type == "TextEntry")
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
			else if (type == "DataMatrix")
				return "GridCellVarValue";
			else if (type == "ShadingFactors")
				return "GridCellVarValue";
			else if (type == "ValueMatrix")
				return "GridCellVarValue";
			else if (type == "MonthByHourFactors")
				return "GridCellVarValue";
			else if (type == "Library")
				return "GridCellVarValue";
			else if (type == "HourlyFactor")
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

BEGIN_EVENT_TABLE(VariableGrid, wxGrid)
EVT_GRID_CELL_LEFT_CLICK(VariableGrid::OnLeftClick)
END_EVENT_TABLE()

VariableGrid::
VariableGrid(wxWindow *parent, wxWindowID id, const wxPoint &pos, const wxSize &size, long style, const wxString &name)
: wxGrid(parent, id, pos, size, style, name)
{
}

VariableGrid::
~VariableGrid()
{
}

void VariableGrid::
OnLeftClick(wxGridEvent &evt)
{
	SetGridCursor(evt.GetRow(), evt.GetCol());
	evt.Skip();
}
////////////////////////////////////////////////////////////////////////////////////////


VariablePopupDialog::VariablePopupDialog(wxWindow *parent, wxUIObject *obj, wxString &name, VarValue *vv, VarInfo *vi)
: wxDialog(parent, wxID_ANY, "Variable Editor", wxDefaultPosition, wxDefaultSize), m_obj(obj), m_vv(vv), m_vi(vi)
{
	if ((m_vv == 0) || (m_vi == 0) || (m_obj == 0)) return;
	
	wxWindow *ctrl = m_obj->CreateNative(this);

	wxString type = m_obj->GetTypeName();

	// similar to main.cpp and activeinput page initilaize
	if (type == "Library")
	{
		if (LibraryCtrl *ll = m_obj->GetNative<LibraryCtrl>())
		{
			wxArrayString lib = m_vi->IndexLabels;
			if (lib.Count() > 0)
				ll->SetLibrary(lib[0], "*"); // no field list kept with VarInfo - only with form object
		}
	}
	else if (vi->Type == VV_NUMBER && vi->IndexLabels.size() > 0
		&& (type == "Choice" || type == "ListBox" || type == "CheckListBox" || type == "RadioChoice"))
	{
		m_obj->Property("Items").SetNamedOptions(vi->IndexLabels, 0);
		// RadioChoice not a wxItemContainer descendant
		if (wxItemContainer *ic = m_obj->GetNative<wxItemContainer>())
		{
			ic->Clear();
			ic->Append(vi->IndexLabels);
		}
		else if (wxRadioChoice *rc = m_obj->GetNative<wxRadioChoice>())
		{
			rc->Clear();
			rc->Add(vi->IndexLabels);
		}
	}
	else if (vi->Type == VV_STRING && vi->Flags & VF_LIBRARY
		&& type == "SearchListBox" && vi->IndexLabels.size() == 2)
	{
		if (Library *lib = Library::Find(vi->IndexLabels[0]))
		{
			if (AFSearchListBox *slb = m_obj->GetNative<AFSearchListBox>())
			{
				slb->Clear();
				slb->Append(lib->ListEntries());
			}
		}
	}


	ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);

	
	
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	sizer->Add(ctrl,
		1,            // make vertically stretchable
		wxEXPAND |    // make horizontally stretchable
		wxALL,        //   and make border all around
		0);         // set border width to 10


	wxBoxSizer *button_sizer = new wxBoxSizer(wxHORIZONTAL);
	button_sizer->Add(
		new wxButton(this, wxID_OK, "OK"),
		0,           // make horizontally unstretchable
		wxALL,       // make border all around (implicit top alignment)
		0);        // set border width to 10
	button_sizer->Add(
		new wxButton(this, wxID_CANCEL, "Cancel"),
		0,           // make horizontally unstretchable
		wxALL,       // make border all around (implicit top alignment)
		0);        // set border width to 10

	sizer->Add(
		button_sizer,
		wxSizerFlags(0).Right());


	SetSizerAndFit(sizer); // use the sizer for layout and set size and hints

	SetTitle(name);
#ifdef __WXMSW__
	SetIcon(wxICON(appicon));
#endif	
	CenterOnParent();
}

VariablePopupDialog::~VariablePopupDialog()
{
}

wxUIObject *VariablePopupDialog::GetUIObject()
{
	return m_obj;
}


////////////////////////////////////////////////////////////////////////////////////////

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

VariableGridFrame::VariableGridFrame(wxWindow *parent, ProjectFile *pf, Case *c) : wxFrame(parent, wxID_ANY, "Variable Grid", wxDefaultPosition, wxSize(800, 700)), m_pf(pf)
{
	
	if (!m_pf) return;

	m_pf->AddListener(this);

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
		for (size_t i = 0; i < m_cases.size(); i++)
			m_cases[i]->AddListener(this);

		wxString title;
		if (m_cases.size() == 1)
			title = "Current Case Values: " + m_pf->GetCaseName(m_cases[0]);
		else
			title = "Case comparison";
		
		SetTitle(title);

		m_griddata = new VariableGridData(m_pf, c);

		m_grid = new VariableGrid(this, wxID_ANY);

		m_grid->RegisterDataType("GridCellCheckBox", new GridCellCheckBoxRenderer, new GridCellCheckBoxEditor);
		m_grid->RegisterDataType("GridCellChoice", new GridCellChoiceRenderer, new GridCellChoiceEditor);
		m_grid->RegisterDataType("GridCellVarValue", new GridCellVarValueRenderer, new GridCellVarValueEditor);

		m_grid->HideRowLabels();

		m_grid->SetTable(m_griddata, true, wxGrid::wxGridSelectRows);


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
		SizeColumns();
		UpdateGrid();


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
#ifdef __WXMSW__
	SetIcon(wxICON(appicon));
#endif	
	CenterOnParent();
	Show();
}

VariableGridFrame::~VariableGridFrame()
{
	/*Strange issue with VS2013 in release mode only*/
	/*The following code caused AV when exiting with input list open*/
	/*No issue running in debug mode or outside of VS2013*/
	/*
	if (m_cases.size() > 0)
		for (size_t i = 0; i < m_cases.size(); i++)
			if (m_cases[i]) m_cases[i]->RemoveListener(this);
			*/
	if (m_pf) m_pf->RemoveListener(this);
	
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
		if (m_griddata->ShowRow(row, m_compare_show_type))
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
}

void VariableGridFrame::UpdateGrid()
{
	m_grid->Freeze();
	for (int row = 0; row < m_grid->GetNumberRows(); row++)
	{
		if (m_griddata->ShowRow(row, m_compare_show_type))
			m_grid->ShowRow(row);
		else
			m_grid->HideRow(row);
	}
	m_grid->Thaw();
}

void VariableGridFrame::OnCommand(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
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
			if (m_cases.size() == 0) Close(); // AV when closeing main window protection
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
	if (evt.GetType() == CaseEvent::VALUE_CHANGED)
	{
		// refresh when any case values change
		UpdateGrid(); // for comparison views
		m_grid->ForceRefresh();
	}
}



