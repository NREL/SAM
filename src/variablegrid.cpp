#include <algorithm>
#include <set>

#include <wx/sizer.h>
#include <wx/bitmap.h>
#include <wx/msgdlg.h>
#include <wex/metro.h>

#include "variablegrid.h"
#include "widgets.h"
#include "inputpage.h"

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

	if (m_cases.size() > 0)
	{
		m_col_hdrs.push_back("Variable");
		m_col_hdrs.push_back("Label");
		if (m_cases.size() == 1)
		{
			m_col_hdrs.push_back(m_pf->GetCaseName(m_cases[0]));
			m_var_table_vec.push_back(&m_cases[0]->Values());
			m_var_info_lookup_vec.push_back(&m_cases[0]->Variables());
			// TODO: skip calculated value
		}
		else
		{
			int i = 0;
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

bool VariableGridData::IsEmptyCell(int row, int col)
{
	if (!GetView()->GetParent()->IsShown()) return wxEmptyString;
	if (col == 0) // variable name
		return (m_var_names[row]==wxEmptyString);
	else if (col == 1) // variable label
		return (m_var_labels[row] == wxEmptyString);
	else // get var table and value
	{
		int lookup_row = row;
		if (m_sorted) lookup_row = m_sorted_index[row];
		if (m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]))
		{
			if ((col - 2) >= m_var_table_vec.size())
				return wxEmptyString;
			else
				return (m_var_table_vec[col - 2]->Get(m_var_names[lookup_row])->AsString() == wxEmptyString);
		}
		else
			return wxEmptyString;
	}
}


wxString VariableGridData::GetColLabelValue(int col)
{
	if (col >= m_col_hdrs.size());
	return m_col_hdrs[col];
}


VarInfo* VariableGridData::GetVarInfo(int row, int col)
{
	VarInfo* vi = NULL;
	int lookup_row = row;
	if (m_sorted) lookup_row = m_sorted_index[row];
	if ((col > 1) && ((col - 2) <  m_var_info_lookup_vec.size()))
	{
		vi = m_var_info_lookup_vec[col - 2]->Lookup(m_var_names[lookup_row]);
	}
	return vi;
}

void VariableGridData::SetVarInfo(int row, int col, VarInfo *vi)
{
	int lookup_row = row;
	if (m_sorted) lookup_row = m_sorted_index[row];
	if ((col > 1) && ((col - 2) <  m_var_info_lookup_vec.size()))
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
	if ((col > 1) && ((col - 2) <  m_var_table_vec.size()))
	{
		vv = m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]);
	}
	return vv;

}

void VariableGridData::SetVarValue(int row, int col, VarValue *vv)
{
	int lookup_row = row;
	if (m_sorted) lookup_row = m_sorted_index[row];
	if ((col > 1) && ((col - 2) <  m_var_info_lookup_vec.size()))
	{
		if (VarValue *var_value = m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]))
			var_value = vv;
	}
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
		if ((col - 2) >= m_var_table_vec.size())
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
		if (m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]))
		{
			VarValue *vv = m_var_table_vec[col - 2]->Get(m_var_names[lookup_row]);
			switch (vv->Type())
			{
			case VV_ARRAY:
			case VV_MATRIX:
			case VV_TABLE:
				return "gridcellbutton";
				break;
			case VV_NUMBER:
			case VV_STRING:
			default:
				return wxGRID_VALUE_STRING;
				break;
			}
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
	if ((col > 0) && (col < m_col_hdrs.Count()))
	{
		m_col_hdrs[col] = label;
	}
}


//////////////////////////////////////////////////////////////////////////////////////

#define BUTTON_WIDTH 10;

GridCellButtonRenderer::GridCellButtonRenderer(wxString label)
{
	m_strLabel = label;
	m_button_width = BUTTON_WIDTH;
}

GridCellButtonRenderer::~GridCellButtonRenderer(void)
{
}

void GridCellButtonRenderer::Draw(wxGrid &grid, wxGridCellAttr &attr, wxDC &dc, const wxRect &rectCell, int row, int col, bool isSelected)
{
	wxRect rect = rectCell;
	rect.Inflate(-1);

	// draw button at right end
	int x = rect.x + rect.width - m_button_width;
	int y = rect.y;
	wxRect button_rect(x, y, m_button_width, rect.height);
	wxRendererNative::Get().DrawPushButton(&grid, dc, button_rect, wxCONTROL_CURRENT);
	dc.DrawText(m_strLabel, x + 1, y + 1);


	//  draw the text
	// resize to fit starting at 100
	// text extent to resize by 
	int dec_width = 5;
	int str_width = 100;
	wxString value = grid.GetCellValue(row, col).Left(str_width);
	rect.width -= button_rect.width;

	wxSize sz = dc.GetTextExtent(value);
	while ((sz.GetWidth() > rect.GetWidth()) && (str_width > dec_width))
	{
		str_width -= dec_width;
		value = grid.GetCellValue(row, col).Left(str_width);
		sz = dc.GetTextExtent(value);
	}

	// erase only this cells background
	wxGridCellRenderer::Draw(grid, attr, dc, rect, row, col, isSelected);
	int hAlign, vAlign;
	attr.GetAlignment(&hAlign, &vAlign);

	SetTextColoursAndFont(grid, attr, dc, isSelected);
	grid.DrawTextRectangle(dc, value,	rect, hAlign, vAlign);

}

wxSize GridCellButtonRenderer::GetBestSize(wxGrid &grid, wxGridCellAttr &attr, wxDC &dc, int row, int col)
{
	wxString text = grid.GetCellValue(row, col);
	dc.SetFont(attr.GetFont());
	return dc.GetTextExtent(text);
}

wxGridCellRenderer *GridCellButtonRenderer::Clone() const
{
	return new GridCellButtonRenderer(m_strLabel);
}

void GridCellButtonRenderer::SetTextColoursAndFont(const wxGrid& grid, const wxGridCellAttr& attr, wxDC& dc, bool isSelected)
{
	dc.SetBackgroundMode(wxBRUSHSTYLE_TRANSPARENT);

	// TODO some special colours for attr.IsReadOnly() case?

	// different coloured text when the grid is disabled
	if (grid.IsThisEnabled())
	{
		if (isSelected)
		{
			wxColour clr;
			if (grid.HasFocus())
				clr = grid.GetSelectionBackground();
			else
				clr = wxSystemSettings::GetColour(wxSYS_COLOUR_BTNSHADOW);
			dc.SetTextBackground(clr);
			dc.SetTextForeground(grid.GetSelectionForeground());
		}
		else
		{
			dc.SetTextBackground(attr.GetBackgroundColour());
			dc.SetTextForeground(attr.GetTextColour());
		}
	}
	else
	{
		dc.SetTextBackground(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
		dc.SetTextForeground(wxSystemSettings::GetColour(wxSYS_COLOUR_GRAYTEXT));
	}

	dc.SetFont(attr.GetFont());
}



//////////////////////////////////////////////////////////////////////////////////////

GridCellButtonEditor::GridCellButtonEditor(wxString label)
{
	m_strLabel = label;
	m_button_width = BUTTON_WIDTH
}

GridCellButtonEditor::~GridCellButtonEditor(void)
{
}

void GridCellButtonEditor::Create(wxWindow *parent, wxWindowID id, wxEvtHandler* pEvtHandler)
{
	m_parent = parent;
	m_pButton = new wxButton(parent, id, m_strLabel);
	SetControl(m_pButton);
	m_pButton->Connect(wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler(GridCellButtonEditor::OnButton));
}

void GridCellButtonEditor::OnButton(wxCommandEvent &evt)
{
//	wxMessageBox("Button pushed");
//	VariableGridData *vgd = static_cast<VariableGridData *>(m_grid->GetTable());
//	VariablePopupEditor *vpe = new VariablePopupEditor(m_parent, vgd->GetVarInfo(m_row, m_col), vgd->GetVarValue(m_row, m_col), vgd->GetValue(m_row, 0));
	evt.Skip();
}

void GridCellButtonEditor::SetSize(const wxRect &rect)
{
	
	m_pButton->SetSize(rect.x - m_button_width, rect.y, m_button_width + 2, rect.height + 2, wxSIZE_ALLOW_MINUS_ONE);
}


void GridCellButtonEditor::BeginEdit(int row, int col, wxGrid *pGrid)
{
	/* event values are not preserved*/
	m_row = row;
	m_col = col;
	m_grid = pGrid;
	VariableGridData *vgd = static_cast<VariableGridData *>(pGrid->GetTable());
	VariablePopupEditor vpe(m_parent, vgd->GetVarInfo(row, col), vgd->GetVarValue(row, col), vgd->GetValue(row, 0));
	if (vpe.ShowModal() == wxID_OK) 
		// update variable value
		m_grid = pGrid;
	else
		// cancel;
		m_grid = pGrid;
	wxGridEvent evt(wxID_ANY, wxEVT_GRID_CELL_LEFT_CLICK, pGrid, row, col);
//	wxPostEvent(m_pButton, wxCommandEvent(wxEVT_COMMAND_BUTTON_CLICKED));
}

bool GridCellButtonEditor::EndEdit(int row, int col, const wxGrid *grid, const wxString &oldval, wxString *newval)
{
	return true;
}

void GridCellButtonEditor::ApplyEdit(int row, int col, wxGrid *grid)
{

}

void GridCellButtonEditor::Reset()
{
}

wxString GridCellButtonEditor::GetValue() const
{
	return wxEmptyString;
}

wxGridCellEditor *GridCellButtonEditor::Clone() const
{
	return new GridCellButtonEditor(m_strLabel);
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

VariablePopupEditor::VariablePopupEditor(wxWindow *parent, VarInfo *vi, VarValue *vv, wxString &var_name)
: wxDialog(parent, wxID_ANY, "Variable Editor", wxDefaultPosition, wxSize(400, 700)), m_vi(vi), m_vv(vv), m_var_name(var_name)
{
	if (!m_vi || !m_vv) return;
	m_form_data = new wxUIFormData;

	m_form_data->Attach(this);
	SetClientSize(m_form_data->GetSize());

	Init();
	SetTitle("Variable editor for: " + var_name);
#ifdef __WXMSW__
	SetIcon(wxICON(appicon));
#endif	
	CenterOnParent();

}

VariablePopupEditor::~VariablePopupEditor()
{
	m_form_data->Detach();
	delete m_form_data;
}


void VariablePopupEditor::Init()
{
	// TODO get editor name from VarInfo and create appropriate editor
	// 	e.g.	wxUIObject *obj = m_form_data->Create(vi->Editor, GetClientSize(), m_var_name);
	if (m_vv->Type() == VV_MATRIX)
	{
		wxUIObject *obj = m_form_data->Create("DataMatrix", GetClientSize(), m_var_name);
		ActiveInputPage::DataExchange(obj, *m_vv, ActiveInputPage::VAR_TO_OBJ);
	}

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
		m_cases.push_back(c);
	else
		m_cases = m_pf->GetCases();
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

		//		m_grid->UseNativeColHeader(); // does not load correctly
		m_grid->RegisterDataType("autowrapstring", new wxGridCellAutoWrapStringRenderer, new wxGridCellAutoWrapStringEditor);
		m_grid->RegisterDataType("gridcellbutton", new GridCellButtonRenderer("..."), new GridCellButtonEditor("..."));
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
	for (std::vector<Case*>::iterator it = m_cases.begin(); it != m_cases.end(); ++it)
		if (*it) (*it)->RemoveListener(this);
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
			if (m_griddata->GetTypeName(row, 2) != "autowrapstring")
			{
				for (col = 0; col < m_grid->GetNumberCols(); col++)
				{
					GetTextExtent(m_grid->GetCellValue(row, col), &width, &height);
					if ((width + 10) > col_width[col]) col_width[col] = width + 10;
				}
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

void VariableGridFrame::OnProjectFileEvent(ProjectFile *p, ProjectFileEvent &evt)
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
		if (it == m_cases.end())
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

void VariableGridFrame::OnCaseEvent(Case *c, CaseEvent &evt)
{
	if (evt.GetType() == CaseEvent::VALUE_CHANGED)
	{
		// refresh when any case values change
		UpdateGrid(); // for comparison views
		m_grid->Refresh();
	}
}



