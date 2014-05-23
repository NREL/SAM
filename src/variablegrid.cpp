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
#include <unordered_map>

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


//////////////////////////////////////////////////////////////////////////////////////


GridCellVarValueRenderer::GridCellVarValueRenderer()
{
	m_ellipsis = "...";
}

GridCellVarValueRenderer::~GridCellVarValueRenderer(void)
{
}

void GridCellVarValueRenderer::Draw(wxGrid &grid, wxGridCellAttr &attr, wxDC &dc, const wxRect &rectCell, int row, int col, bool isSelected)
{
	wxGridCellRenderer::Draw(grid, attr, dc, rectCell, row, col, isSelected);

	SetTextColoursAndFont(grid, attr, dc, isSelected);

	wxRect rect = rectCell;
	rect.Inflate(-1);

	// draw ellipsis at right end
	wxSize sz_ellipsis = dc.GetTextExtent(m_ellipsis);
	int x = rect.x + rect.width - sz_ellipsis.GetWidth();
	int y = rect.y;
	wxRect ellipsis_rect(x, y, sz_ellipsis.GetWidth(), rect.height);


	//  draw the text
	// resize to fit starting at 100
	// text extent to resize by 
	int dec_width = 2;
	int str_width = 100;
	wxString display_string = grid.GetCellValue(row, col);
	VarInfo * vi = ((VariableGridData*)grid.GetTable())->GetVarInfo(row, col);
	wxString type = vi->UIObject;

	if (vi->Type == VV_NUMBER && vi->IndexLabels.size() > 0
		&& (type == "Choice" || type == "ListBox" || type == "CheckListBox" || type == "RadioChoice"))
	{
		wxArrayString items = vi->IndexLabels;
		long ndx;
		if (display_string.ToLong(&ndx) && (ndx > -1) && (ndx < (int)items.Count()))
			display_string = items[ndx];
	}


	wxString value = display_string.Left(str_width); //+ m_ellipsis;

	wxSize sz = dc.GetTextExtent(value);
	while ((sz.GetWidth() > rect.GetWidth()) && (str_width > dec_width))
	{
		str_width -= dec_width;
		value = display_string.Left(str_width);// +m_ellipsis;
		sz = dc.GetTextExtent(value);
	}

	// erase only this cells background
	wxGridCellRenderer::Draw(grid, attr, dc, rect, row, col, isSelected);
	int hAlign, vAlign;
	attr.GetAlignment(&hAlign, &vAlign);

	SetTextColoursAndFont(grid, attr, dc, isSelected);
	grid.DrawTextRectangle(dc, value,	rect, hAlign, vAlign);

}

wxSize GridCellVarValueRenderer::GetBestSize(wxGrid &grid, wxGridCellAttr &attr, wxDC &dc, int row, int col)
{
	wxString text = grid.GetCellValue(row, col);
	dc.SetFont(attr.GetFont());
	return dc.GetTextExtent(text);
}

wxGridCellRenderer *GridCellVarValueRenderer::Clone() const
{
	return new GridCellVarValueRenderer();
}

void GridCellVarValueRenderer::SetTextColoursAndFont(const wxGrid& grid, const wxGridCellAttr& attr, wxDC& dc, bool isSelected)
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

GridCellVarValueEditor::GridCellVarValueEditor()
{
}

GridCellVarValueEditor::~GridCellVarValueEditor(void)
{
}

void GridCellVarValueEditor::Create(wxWindow *parent, wxWindowID id, wxEvtHandler* pEvtHandler)
{
	m_parent = parent;
	m_text = new wxStaticText(parent, id, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxST_NO_AUTORESIZE | wxST_ELLIPSIZE_END);
	SetControl(m_text);
	wxGridCellEditor::Create(parent, id, pEvtHandler);
}

void GridCellVarValueEditor::Reset()
{
	m_text->SetLabel(m_cell_value);
}


void GridCellVarValueEditor::SetSize(const wxRect &rect_orig)
{	// similar to wxGridCellTextEditor
	wxRect rect(rect_orig);

	// Make the edit control large enough to allow for internal margins
	//
	// TODO: remove this if the text ctrl sizing is improved esp. for unix
	//
#if defined(__WXGTK__)
	if (rect.x != 0)
	{
		rect.x += 1;
		rect.y += 1;
		rect.width -= 1;
		rect.height -= 1;
	}
#elif defined(__WXMSW__)
	if (rect.x == 0)
		rect.x += 2;
	else
		rect.x += 3;

	if (rect.y == 0)
		rect.y += 2;
	else
		rect.y += 3;

	rect.width -= 2;
	rect.height -= 2;
#elif defined(__WXOSX__)
	rect.x += 1;
	rect.y += 1;

	rect.width -= 1;
	rect.height -= 1;
#else
	int extra_x = (rect.x > 2) ? 2 : 1;
	int extra_y = (rect.y > 2) ? 2 : 1;

#if defined(__WXMOTIF__)
	extra_x *= 2;
	extra_y *= 2;
#endif

	rect.SetLeft(wxMax(0, rect.x - extra_x));
	rect.SetTop(wxMax(0, rect.y - extra_y));
	rect.SetRight(rect.GetRight() + 2 * extra_x);
	rect.SetBottom(rect.GetBottom() + 2 * extra_y);
#endif

	wxGridCellEditor::SetSize(rect);
}

void GridCellVarValueEditor::PaintBackground(wxDC& WXUNUSED(dc),
	const wxRect& WXUNUSED(rectCell),
	const wxGridCellAttr& WXUNUSED(attr))
{
	// don't do anything here to minimize flicker
}

bool GridCellVarValueEditor::IsAcceptedKey(wxKeyEvent& event)
{
	switch (event.GetKeyCode())
	{
	case WXK_DELETE:
	case WXK_BACK:
	case WXK_ESCAPE:
		return true;

	default:
		return wxGridCellEditor::IsAcceptedKey(event);
	}
}


bool GridCellVarValueEditor::DisplayEditor(wxUIObject *obj, wxString &name, wxGrid *grid, VarValue *vv, VarInfo *vi)
{
	wxString type = obj->GetTypeName();
	if (type == "HourlyFactor")
	{
		obj->CreateNative(grid);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);
		AFHourlyFactorCtrl *hf = obj->GetNative<AFHourlyFactorCtrl>();
		wxCommandEvent evt = wxCommandEvent(wxEVT_BUTTON);
		hf->OnPressed(evt);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "DiurnalPeriod")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "ListBox")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "RadioChoice")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "Slider")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "SchedNumeric")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "TOUSchedule")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "PTLayout")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "MaterialProperties")
	{
		obj->CreateNative(grid);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);
		MatPropCtrl *mp = obj->GetNative<MatPropCtrl>();
		wxCommandEvent evt = wxCommandEvent(wxEVT_BUTTON);
		mp->OnButton(evt);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "TroughLoop")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "MonthlyFactor")
	{
		obj->CreateNative(grid);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);
		AFMonthlyFactorCtrl *mf = obj->GetNative<AFMonthlyFactorCtrl>();
		wxCommandEvent evt = wxCommandEvent(wxEVT_BUTTON);
		mf->OnPressed(evt);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "SearchListBox")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "DataArray")
	{
		obj->CreateNative(grid);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);
		AFDataArrayButton *da = obj->GetNative<AFDataArrayButton>();
		wxCommandEvent evt = wxCommandEvent(wxEVT_BUTTON);
		da->OnPressed(evt);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "DataMatrix")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "ShadingFactors")
	{
		obj->CreateNative(grid);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);
		ShadingButtonCtrl *sf = obj->GetNative<ShadingButtonCtrl>();
		wxCommandEvent evt = wxCommandEvent(wxEVT_BUTTON);
		sf->OnPressed(evt);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "ValueMatrix")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "MonthByHourFactors")
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "Library") 
	{
		VariablePopupDialog vpe(grid, obj, name, vv, vi);
		if (vpe.ShowModal() == wxID_OK)
			ActiveInputPage::DataExchange(vpe.GetUIObject(), *vv, ActiveInputPage::OBJ_TO_VAR);
	}

	else return false; // object data exch not handled for this type

	return true;  // all ok!
}

void GridCellVarValueEditor::BeginEdit(int row, int col, wxGrid *pGrid)
{
	/* event values are not preserved*/
	m_cell_value = pGrid->GetTable()->GetValue(row, col);
	m_text->SetLabel(GetDisplayString(m_cell_value, row, col, pGrid));

	VariableGridData *vgd = static_cast<VariableGridData *>(pGrid->GetTable());
	VarValue *vv = vgd->GetVarValue(row, col);
	VarInfo *vi = vgd->GetVarInfo(row, col);
	wxString var_name = vgd->GetValue(row, 0);
	wxString var_label = vgd->GetValue(row, 1);

	wxUIObject *obj = wxUIObjectTypeProvider::Create(vi->UIObject);
	if (obj == 0) return;

	obj->SetName(var_name);

	if (var_label.IsEmpty())
		DisplayEditor(obj, var_name, pGrid, vv, vi);
	else
		DisplayEditor(obj, var_label, pGrid, vv, vi);

	m_new_cell_value = vv->AsString();
	m_text->SetLabel( GetDisplayString( m_new_cell_value, row, col, pGrid));
	pGrid->SaveEditControlValue();
}

wxString GridCellVarValueEditor::GetDisplayString(wxString &var_string, int row, int col, const wxGrid *grid)
{
	wxString display_string = var_string;
	VarInfo * vi = ((VariableGridData*)grid->GetTable())->GetVarInfo(row, col);
	wxString type = vi->UIObject;

	if (vi->Type == VV_NUMBER && vi->IndexLabels.size() > 0
		&& (type == "Choice" || type == "ListBox" || type == "CheckListBox" || type == "RadioChoice"))
	{
		wxArrayString items = vi->IndexLabels;
		long ndx;
		if (display_string.ToLong(&ndx) && (ndx > -1) && (ndx < (int)items.Count()))
			display_string = items[ndx];
	}

	return display_string;
}



bool GridCellVarValueEditor::EndEdit(int row, int col, const wxGrid *grid, const wxString& WXUNUSED(oldval), wxString *newval)
{
//	wxString new_cell_value = m_text->GetLabel();
	wxString new_cell_value = m_new_cell_value;
	if (new_cell_value == m_cell_value)
		return false; // no change

	m_cell_value = new_cell_value;

	if (newval)
		*newval = m_cell_value;

	m_text->SetLabel(GetDisplayString(m_cell_value, row, col, grid));
	return true;
}

void GridCellVarValueEditor::ApplyEdit(int row, int col, wxGrid *grid)
{
	grid->GetTable()->SetValue(row, col, m_cell_value);
	m_cell_value.clear();
	m_new_cell_value.clear();
	VariableGridFrame *vgf = static_cast<VariableGridFrame *>(grid->GetParent());
	vgf->UpdateGrid(); // for comparison views
	grid->Refresh();
}


wxString GridCellVarValueEditor::GetValue() const
{
	return m_cell_value;
}

wxGridCellEditor *GridCellVarValueEditor::Clone() const
{
	return new GridCellVarValueEditor();
}



////////////////////////////////////////////////////////////////////////////////////////

GridCellChoiceRenderer::GridCellChoiceRenderer(const wxString& choices)
{
	if (!choices.empty())
		SetParameters(choices);
}

wxGridCellRenderer *GridCellChoiceRenderer::Clone() const
{
	GridCellChoiceRenderer *renderer = new GridCellChoiceRenderer;
	renderer->m_choices = m_choices;
	return renderer;
}

wxString GridCellChoiceRenderer::GetString(const wxGrid& grid, int row, int col)
{
	wxGridTableBase *table = grid.GetTable();
	wxString text;
	long choiceno;
	table->GetValue(row, col).ToLong(&choiceno);
	if ((choiceno > -1) && (choiceno < (int)m_choices.size()))
		text.Printf(wxT("%s"), m_choices[choiceno].c_str());
	return text;
}

void GridCellChoiceRenderer::Draw(wxGrid& grid,
	wxGridCellAttr& attr,
	wxDC& dc,
	const wxRect& rectCell,
	int row, int col,
	bool isSelected)
{
	wxGridCellRenderer::Draw(grid, attr, dc, rectCell, row, col, isSelected);

	SetTextColoursAndFont(grid, attr, dc, isSelected);

	// draw the text left aligned by default
	int hAlign = wxALIGN_LEFT,
		vAlign = wxALIGN_INVALID;
	attr.GetNonDefaultAlignment(&hAlign, &vAlign);

	wxRect rect = rectCell;
	rect.Inflate(-1);

	grid.DrawTextRectangle(dc, GetString(grid, row, col), rect, hAlign, vAlign);
}

wxSize GridCellChoiceRenderer::GetBestSize(wxGrid& grid,
	wxGridCellAttr& attr,
	wxDC& dc,
	int row, int col)
{
	return DoGetBestSize(attr, dc, GetString(grid, row, col));
}

void GridCellChoiceRenderer::SetParameters(const wxString& params)
{
	if (!params)
	{
		return;
	}

	m_choices.Empty();

	wxStringTokenizer tk(params, wxT(','));
	while (tk.HasMoreTokens())
	{
		m_choices.Add(tk.GetNextToken());
	}
}



////////////////////////////////////////////////////////////////////////////////////////


GridCellChoiceEditor::GridCellChoiceEditor(const wxString& choices)
:wxGridCellChoiceEditor()
{
	m_index = -1;

	if (!choices.empty())
		SetParameters(choices);
}

wxGridCellEditor *GridCellChoiceEditor::Clone() const
{
	GridCellChoiceEditor *editor = new GridCellChoiceEditor();
	editor->m_index = m_index;
	return editor;
}

void GridCellChoiceEditor::BeginEdit(int row, int col, wxGrid* grid)
{
	wxASSERT_MSG(m_control,
		wxT("The GridCellChoiceEditor must be Created first!"));

	wxGridCellEditorEvtHandler* evtHandler = NULL;
	if (m_control)
		evtHandler = wxDynamicCast(m_control->GetEventHandler(), wxGridCellEditorEvtHandler);

	// Don't immediately end if we get a kill focus event within BeginEdit
	if (evtHandler)
		evtHandler->SetInSetFocus(true);

	wxGridTableBase *table = grid->GetTable();

	if (table->CanGetValueAs(row, col, wxGRID_VALUE_NUMBER))
	{
		m_index = table->GetValueAsLong(row, col);
	}
	else
	{
		wxString startValue = table->GetValue(row, col);
		if (startValue.IsNumber() && !startValue.empty())
		{
			startValue.ToLong(&m_index);
		}
		else
		{
			m_index = -1;
		}
	}

	Combo()->SetSelection(m_index);
	Combo()->SetFocus();

#ifdef __WXOSX_COCOA__
	// This is a work around for the combobox being simply dismissed when a
	// choice is made in it under OS X. The bug is almost certainly due to a
	// problem in focus events generation logic but it's not obvious to fix and
	// for now this at least allows to use wxGrid.
	Combo()->Popup();
#endif

	if (evtHandler)
	{
		// When dropping down the menu, a kill focus event
		// happens after this point, so we can't reset the flag yet.
#if !defined(__WXGTK20__)
		evtHandler->SetInSetFocus(false);
#endif
	}
}

bool GridCellChoiceEditor::EndEdit(int WXUNUSED(row),
	int WXUNUSED(col),
	const wxGrid* WXUNUSED(grid),
	const wxString& WXUNUSED(oldval),
	wxString *newval)
{
	long idx = Combo()->GetSelection();
	if (idx == m_index)
		return false;

	m_index = idx;

	if (newval)
		newval->Printf("%ld", m_index);

	return true;
}

void GridCellChoiceEditor::ApplyEdit(int row, int col, wxGrid* grid)
{
	wxGridTableBase * const table = grid->GetTable();
	table->SetValue(row, col, wxString::Format("%ld", m_index));
}

////////////////////////////////////////////////////////////////////////////////////////


wxSize GridCellCheckBoxRenderer::ms_sizeCheckMark;

wxSize GridCellCheckBoxRenderer::GetBestSize(wxGrid& grid,
	wxGridCellAttr& WXUNUSED(attr),
	wxDC& WXUNUSED(dc),
	int WXUNUSED(row),
	int WXUNUSED(col))
{
	// compute it only once (no locks for MT safeness in GUI thread...)
	if (!ms_sizeCheckMark.x)
	{
		ms_sizeCheckMark = wxRendererNative::Get().GetCheckBoxSize(&grid);
	}

	return ms_sizeCheckMark;
}

void GridCellCheckBoxRenderer::Draw(wxGrid& grid,
	wxGridCellAttr& attr,
	wxDC& dc,
	const wxRect& rect,
	int row, int col,
	bool isSelected)
{
	wxGridCellRenderer::Draw(grid, attr, dc, rect, row, col, isSelected);

	// draw a check mark in the centre (ignoring alignment - TODO)
	wxSize size = GetBestSize(grid, attr, dc, row, col);

	// don't draw outside the cell
	wxCoord minSize = wxMin(rect.width, rect.height);
	if (size.x >= minSize || size.y >= minSize)
	{
		// and even leave (at least) 1 pixel margin
		size.x = size.y = minSize;
	}

	// draw a border around checkmark
	int vAlign, hAlign;
	attr.GetAlignment(&hAlign, &vAlign);

	wxRect rectBorder;
	if (hAlign == wxALIGN_CENTRE)
	{
		rectBorder.x = rect.x + rect.width / 2 - size.x / 2;
		rectBorder.y = rect.y + rect.height / 2 - size.y / 2;
		rectBorder.width = size.x;
		rectBorder.height = size.y;
	}
	else if (hAlign == wxALIGN_LEFT)
	{
		rectBorder.x = rect.x + 2;
		rectBorder.y = rect.y + rect.height / 2 - size.y / 2;
		rectBorder.width = size.x;
		rectBorder.height = size.y;
	}
	else if (hAlign == wxALIGN_RIGHT)
	{
		rectBorder.x = rect.x + rect.width - size.x - 2;
		rectBorder.y = rect.y + rect.height / 2 - size.y / 2;
		rectBorder.width = size.x;
		rectBorder.height = size.y;
	}

	bool value;
	wxString cellval(grid.GetTable()->GetValue(row, col));
	value = (cellval == "1");

	int flags = 0;
	if (value)
		flags |= wxCONTROL_CHECKED;

	wxRendererNative::Get().DrawCheckBox(&grid, dc, rectBorder, flags);
}


////////////////////////////////////////////////////////////////////////////////////////

void GridCellCheckBoxEditor::Create(wxWindow* parent,
	wxWindowID id,
	wxEvtHandler* evtHandler)
{
	m_control = new wxCheckBox(parent, id, wxEmptyString,
		wxDefaultPosition, wxDefaultSize,
		wxNO_BORDER);

	wxGridCellEditor::Create(parent, id, evtHandler);
}

void GridCellCheckBoxEditor::SetSize(const wxRect& r)
{
	bool resize = false;
	wxSize size = m_control->GetSize();
	wxCoord minSize = wxMin(r.width, r.height);

	// check if the checkbox is not too big/small for this cell
	wxSize sizeBest = m_control->GetBestSize();
	if (!(size == sizeBest))
	{
		// reset to default size if it had been made smaller
		size = sizeBest;

		resize = true;
	}

	if (size.x >= minSize || size.y >= minSize)
	{
		// leave 1 pixel margin
		size.x = size.y = minSize - 2;

		resize = true;
	}

	if (resize)
	{
		m_control->SetSize(size);
	}

	// position it in the centre of the rectangle (TODO: support alignment?)

#if defined(__WXGTK__) || defined (__WXMOTIF__)
	// the checkbox without label still has some space to the right in wxGTK,
	// so shift it to the right
	size.x -= 8;
#elif defined(__WXMSW__)
	// here too, but in other way
	size.x += 1;
	size.y -= 2;
#endif

	int hAlign = wxALIGN_CENTRE;
	int vAlign = wxALIGN_CENTRE;
	if (GetCellAttr())
		GetCellAttr()->GetAlignment(&hAlign, &vAlign);

	int x = 0, y = 0;
	if (hAlign == wxALIGN_LEFT)
	{
		x = r.x + 2;

#ifdef __WXMSW__
		x += 2;
#endif

		y = r.y + r.height / 2 - size.y / 2;
	}
	else if (hAlign == wxALIGN_RIGHT)
	{
		x = r.x + r.width - size.x - 2;
		y = r.y + r.height / 2 - size.y / 2;
	}
	else if (hAlign == wxALIGN_CENTRE)
	{
		x = r.x + r.width / 2 - size.x / 2;
		y = r.y + r.height / 2 - size.y / 2;
	}

	m_control->Move(x, y);
}

void GridCellCheckBoxEditor::Show(bool show, wxGridCellAttr *attr)
{
	m_control->Show(show);

	if (show)
	{
		wxColour colBg = attr ? attr->GetBackgroundColour() : *wxLIGHT_GREY;
		CBox()->SetBackgroundColour(colBg);
	}
}

void GridCellCheckBoxEditor::BeginEdit(int row, int col, wxGrid* grid)
{
	wxASSERT_MSG(m_control,
		wxT("The wxGridCellEditor must be created first!"));

	wxString cellval(grid->GetTable()->GetValue(row, col));

	if (cellval == "0")
		m_value = false;
	else if (cellval == "1")
		m_value = true;
	else
	{
			// do not try to be smart here and convert it to true or false
			// because we'll still overwrite it with something different and
			// this risks to be very surprising for the user code, let them
			// know about it
		wxFAIL_MSG(wxT("invalid value for a cell with bool editor!"));
	}

	CBox()->SetValue(m_value);
	CBox()->SetFocus();
}

bool GridCellCheckBoxEditor::EndEdit(int WXUNUSED(row),
	int WXUNUSED(col),
	const wxGrid* WXUNUSED(grid),
	const wxString& WXUNUSED(oldval),
	wxString *newval)
{
	bool value = CBox()->GetValue();
	if (value == m_value)
		return false;

	m_value = value;

	if (newval)
		*newval = GetValue();

	return true;
}

void GridCellCheckBoxEditor::ApplyEdit(int row, int col, wxGrid* grid)
{
	wxGridTableBase * const table = grid->GetTable();
	if (table->CanSetValueAs(row, col, wxGRID_VALUE_BOOL))
		table->SetValueAsBool(row, col, m_value);
	else
		table->SetValue(row, col, GetValue());
}

void GridCellCheckBoxEditor::Reset()
{
	wxASSERT_MSG(m_control,
		wxT("The wxGridCellEditor must be created first!"));

	CBox()->SetValue(m_value);
}


bool GridCellCheckBoxEditor::IsAcceptedKey(wxKeyEvent& event)
{
	if (wxGridCellEditor::IsAcceptedKey(event))
	{
		int keycode = event.GetKeyCode();
		switch (keycode)
		{
		case WXK_SPACE:
		case '+':
		case '-':
			return true;
		}
	}

	return false;
}

wxString GridCellCheckBoxEditor::GetValue() const
{
	if (CBox()->GetValue())
		return "1";
	else
		return "0";
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
		m_grid->HideRowLabels();
		m_grid->RegisterDataType("GridCellChoice", new GridCellChoiceRenderer, new GridCellChoiceEditor);
		m_grid->HideRowLabels();
		m_grid->RegisterDataType("GridCellVarValue", new GridCellVarValueRenderer, new GridCellVarValueEditor);
		m_grid->HideRowLabels();


		m_grid->SetTable(m_griddata, true, wxGrid::wxGridSelectRows);


		// update choices as necessary
		for (int row = 0; row < m_grid->GetNumberRows(); row++)
			for (int col = 2; col < m_grid->GetNumberCols(); col++)
//				if (m_griddata->GetTypeName(row, col) == wxGRID_VALUE_CHOICE)
				if (m_griddata->GetTypeName(row, col) == "GridCellChoice")
				{

					m_grid->SetCellRenderer(row, col, new GridCellChoiceRenderer(m_griddata->GetChoices(row,col)));
					m_grid->SetCellEditor(row, col, new GridCellChoiceEditor(m_griddata->GetChoices(row,col)));
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
	if (m_cases.size() > 0)
		for (size_t i = 0; i < m_cases.size(); i++)
			if (m_cases[i]) m_cases[i]->RemoveListener(this);
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



