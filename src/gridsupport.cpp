#include <algorithm>
#include <set>

#include <wx/sizer.h>
#include <wx/bitmap.h>
#include <wx/msgdlg.h>
#include <wx/tokenzr.h>
#include <wx/stattext.h>

#include <wex/metro.h>

#include "widgets.h"
#include "inputpage.h"
#include "object.h"
#include "gridsupport.h"

#define COMPARE_SHOW_ALL 0
#define COMPARE_SHOW_DIFFERENT 1
#define COMPARE_SHOW_SAME 2

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
	VarInfo * vi = ((GridChoiceData*)grid.GetTable())->GetVarInfo(row, col);
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
		hf->DoEdit();
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

	GridChoiceData *vgd = static_cast<GridChoiceData *>(pGrid->GetTable());
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
	VarInfo * vi = ((GridChoiceData*)grid->GetTable())->GetVarInfo(row, col);
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
//	VariableGridFrame *vgf = static_cast<VariableGridFrame *>(grid->GetParent());
//	vgf->UpdateGrid(); // for comparison views
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


wxGridCellRenderer *GridCellArrayRenderer::Clone() const
{
	GridCellArrayRenderer *renderer = new GridCellArrayRenderer;
	return renderer;
}

wxString GridCellArrayRenderer::GetString(const wxGrid& grid, int row, int col)
{
	if (GridChoiceData *vgd = (GridChoiceData *)grid.GetTable())
	{
		wxString text = wxEmptyString;
		if (VarValue *vv = vgd->GetVarValue(row, col))
		{
			if (vv->Type() == VV_ARRAY)
			{
				size_t n;
				float *v = vv->Array(&n);
				if (n == 12)
					text = "monthly...";
				else if (n == 8760)
					text = "hourly...";
				else
					text = wxString::Format("array of size %d", n);
			}
		}
		return text;
	}
	else
		return wxEmptyString;
}

void GridCellArrayRenderer::Draw(wxGrid& grid,
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

wxSize GridCellArrayRenderer::GetBestSize(wxGrid& grid,
	wxGridCellAttr& attr,
	wxDC& dc,
	int row, int col)
{
	return DoGetBestSize(attr, dc, GetString(grid, row, col));
}

////////////////////////////////////////////////////////////////////////////////////////


GridCellArrayEditor::GridCellArrayEditor()
{
}

GridCellArrayEditor::~GridCellArrayEditor(void)
{
}

void GridCellArrayEditor::Create(wxWindow *parent, wxWindowID id, wxEvtHandler* pEvtHandler)
{
	m_parent = parent;
	m_text = new wxStaticText(parent, id, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxST_NO_AUTORESIZE | wxST_ELLIPSIZE_END);
	SetControl(m_text);
	wxGridCellEditor::Create(parent, id, pEvtHandler);
}

void GridCellArrayEditor::Reset()
{
	m_text->SetLabel(m_cell_value);
}


void GridCellArrayEditor::SetSize(const wxRect &rect_orig)
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

void GridCellArrayEditor::PaintBackground(wxDC& WXUNUSED(dc),
	const wxRect& WXUNUSED(rectCell),
	const wxGridCellAttr& WXUNUSED(attr))
{
	// don't do anything here to minimize flicker
}

bool GridCellArrayEditor::IsAcceptedKey(wxKeyEvent& event)
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


bool GridCellArrayEditor::DisplayEditor(wxString &title, wxString &label, wxGrid *grid, VarValue *vv)
{
	ArrayPopupDialog apd(grid, title, label, vv);
	// read only - no updating
	apd.ShowModal();
	return true;  // all ok!
}

void GridCellArrayEditor::BeginEdit(int row, int col, wxGrid *pGrid)
{
	/* event values are not preserved*/
	m_cell_value = GetString(row, col, pGrid);
	m_text->SetLabel(m_cell_value);

	GridChoiceData *vgd = static_cast<GridChoiceData *>(pGrid->GetTable());
	VarValue *vv = vgd->GetVarValue(row, col);
	wxString title = vgd->GetColLabelValue(col) + wxString::Format(" for run %d", row);
	wxString label = vgd->GetColLabelValue(col);

	DisplayEditor( title, label, pGrid, vv);

	m_new_cell_value = m_cell_value;
	m_text->SetLabel(m_new_cell_value);
	pGrid->SaveEditControlValue();
}

wxString GridCellArrayEditor::GetString(int row, int col, const wxGrid *grid)
{
	if (GridChoiceData *vgd = (GridChoiceData *)grid->GetTable())
	{
		wxString text = wxEmptyString;
		if (VarValue *vv = vgd->GetVarValue(row, col))
		{
			if (vv->Type() == VV_ARRAY)
			{
				size_t n;
//				float *v = vv->Array(&n);
				vv->Array(&n);
				if (n == 12)
					text = "monthly...";
				else if (n == 8760)
					text = "hourly...";
				else
					text = wxString::Format("array of size %d", n);
			}
		}
		return text;
	}
	else
		return wxEmptyString;
}



bool GridCellArrayEditor::EndEdit(int row, int col, const wxGrid *grid, const wxString& WXUNUSED(oldval), wxString *newval)
{
	wxString new_cell_value = m_new_cell_value;
	if (new_cell_value == m_cell_value)
		return false; // no change

	m_cell_value = new_cell_value;

	if (newval)
		*newval = m_cell_value;

	m_text->SetLabel(m_cell_value);
	return true;
}

void GridCellArrayEditor::ApplyEdit(int row, int col, wxGrid *grid)
{
// read only display
}


wxString GridCellArrayEditor::GetValue() const
{
	return m_cell_value;
}

wxGridCellEditor *GridCellArrayEditor::Clone() const
{
	return new GridCellArrayEditor();
}

////////////////////////////////////////////////////////////////////////////////////////


ArrayPopupDialog::ArrayPopupDialog(wxWindow *parent, wxString &title, wxString &label, VarValue *vv ) : wxDialog(parent, wxID_ANY, "Array Viewer", wxDefaultPosition, wxDefaultSize, wxRESIZE_BORDER | wxDEFAULT_DIALOG_STYLE), m_vv(vv)
{
	if (!m_vv)  return;

	if (vv->Type() != VV_ARRAY) return;
	
	std::vector<float> vec = vv->Array();

	int rows = vec.size();
	int cols = 2;

	wxExtGridCtrl *grid = new wxExtGridCtrl(this, wxID_ANY);
	grid->CreateGrid(rows, cols);

	grid->GetTable()->SetAttrProvider(new AlignRightGridCellAttrProvider());

	grid->HideRowLabels();
	int vec_size = vec.size();
	wxString index_label = "Index";
	if (vec_size == 12)
		index_label = "Month";
	else if (vec_size == 8760)
		index_label = "Hour";

	grid->SetColLabelValue(0, index_label);

	grid->SetColLabelValue(1, label);


	for (size_t i = 0; i < vec_size; i++)
	{
		if (vec_size != 12) grid->SetCellValue(i, 0, wxString::Format("%d", i));
		grid->SetCellValue(i, 1, wxString::Format("%lg", vec[i]));
	}


	if (vec_size == 12)
	{
		grid->SetCellValue(0, 0, "Jan");
		grid->SetCellValue(1, 0, "Feb");
		grid->SetCellValue(2, 0, "Mar");
		grid->SetCellValue(3, 0, "Apr");
		grid->SetCellValue(4, 0, "May");
		grid->SetCellValue(5, 0, "Jun");
		grid->SetCellValue(6, 0, "Jul");
		grid->SetCellValue(7, 0, "Aug");
		grid->SetCellValue(8, 0, "Sep");
		grid->SetCellValue(9, 0, "Oct");
		grid->SetCellValue(10, 0, "Nov");
		grid->SetCellValue(11, 0, "Dec");
	}

	grid->SetEditable(false);
	
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	// TODO want text extent of title bar title
	wxSize sz = GetTextExtent(title);
	wxString spacer = " ";
	spacer.Pad(sz.GetWidth(), ' ', false);

	sz = GetTextExtent(spacer);
	int width = sz.GetWidth() - 20 ; // subtract scrollbar width

	grid->SetColumnWidth(0, (int)(width / 2.0));
	grid->SetColumnWidth(1, width - (int)(width / 2.0));


	sizer->Add(new wxStaticText(this, wxID_ANY, spacer), 0, wxEXPAND | wxALL, 0);

	sizer->Add(grid,
		0,            // make vertically stretchable
		wxEXPAND |    // make horizontally stretchable
		wxALL,        //   and make border all around
		0);         // set border width to 10

	

	wxBoxSizer *button_sizer = new wxBoxSizer(wxHORIZONTAL);
	button_sizer->Add(
		new wxButton(this, wxID_OK, "OK"),
		0,           // make horizontally unstretchable
		wxALL,       // make border all around (implicit top alignment)
		0);        // set border width to 10

	sizer->Add(
		button_sizer,
		wxSizerFlags(0).Right());


	
	SetTitle(title);
#ifdef __WXMSW__
	SetIcon(wxICON(appicon));
#endif	
	CenterOnParent();
	SetSizerAndFit(sizer); // use the sizer for layout and set size and hints
}



ArrayPopupDialog::ArrayPopupDialog(wxWindow *parent, wxString &title, wxArrayString &labels, std::vector<std::vector<float> > &values_vec) : wxDialog(parent, wxID_ANY, "Array Viewer", wxDefaultPosition, wxDefaultSize, wxRESIZE_BORDER | wxDEFAULT_DIALOG_STYLE)
{
	// check that all vectors the same size
	int values_vec_size = values_vec.size();
	if (values_vec_size <= 0) return;
	int vec_size = values_vec[0].size();

	bool all_same_size = true;

	for (size_t i = 1; i < values_vec_size; i++)
	{
		all_same_size &= (vec_size == values_vec[i].size());
		if (!all_same_size) break;
	}

	if (!all_same_size) return;

	int rows = vec_size;
	int cols = values_vec_size + 1;

	wxExtGridCtrl *grid = new wxExtGridCtrl(this, wxID_ANY);
	grid->CreateGrid(rows, cols);

	grid->Freeze();

	grid->GetTable()->SetAttrProvider(new AlignRightGridCellAttrProvider());

	grid->HideRowLabels();
	wxString index_label = "Index";
	if (vec_size == 12)
		index_label = "Month";
	else if (vec_size == 8760)
		index_label = "Hour";


	grid->SetColLabelValue(0, index_label);


	for (size_t col = 0; col < values_vec_size; col++)
	{
		if (labels.Count()>col)
			grid->SetColLabelValue(col+1, labels[col]);
		for (size_t row = 0; row < vec_size; row++)
			grid->SetCellValue(row, col+1, wxString::Format("%lg", values_vec[col][row]));
	}

	if (vec_size == 12)
	{
		grid->SetCellValue(0, 0, "Jan");
		grid->SetCellValue(1, 0, "Feb");
		grid->SetCellValue(2, 0, "Mar");
		grid->SetCellValue(3, 0, "Apr");
		grid->SetCellValue(4, 0, "May");
		grid->SetCellValue(5, 0, "Jun");
		grid->SetCellValue(6, 0, "Jul");
		grid->SetCellValue(7, 0, "Aug");
		grid->SetCellValue(8, 0, "Sep");
		grid->SetCellValue(9, 0, "Oct");
		grid->SetCellValue(10, 0, "Nov");
		grid->SetCellValue(11, 0, "Dec");
	}
	else
	{
		for (size_t row = 0; row < vec_size; row++)
			grid->SetCellValue(row, 0, wxString::Format("%d", row));
	}

	if (vec_size == 1) grid->SetColumnWidth(0, 0); // hide index column for single values.
	grid->EnableEditing(false);
	grid->Thaw();


	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	
	// for scroll bars for hourly values.
	// TODO want text extent of title bar title - should not have to resize for scollbars!!
	int col_width= 40;
	wxString spacer = " ";
	spacer.Pad(values_vec_size * col_width, ' ', false);

	wxSize sz = GetTextExtent(spacer);
	int width = sz.GetWidth() - 100; // subtract scrollbar width

	int tot_width = 0;
	col_width = (int)(width / values_vec_size);
	for (size_t i = 1; i < cols; i++)
	{
		grid->SetColumnWidth(i, col_width);
		tot_width += col_width;
	}
	grid->SetColumnWidth(0, width - tot_width);


	sizer->Add(new wxStaticText(this, wxID_ANY, spacer), 0, wxEXPAND | wxALL, 0);

	sizer->Add(grid,
		0,            // make vertically stretchable
		wxEXPAND |    // make horizontally stretchable
		wxALL,        //   and make border all around
		0);         // set border width to 10



	wxBoxSizer *button_sizer = new wxBoxSizer(wxHORIZONTAL);
	button_sizer->Add(
		new wxButton(this, wxID_OK, "OK"),
		0,           // make horizontally unstretchable
		wxALL,       // make border all around (implicit top alignment)
		0);        // set border width to 10

	sizer->Add(
		button_sizer,
		wxSizerFlags(0).Right());


	SetTitle(title);
#ifdef __WXMSW__
	SetIcon(wxICON(appicon));
#endif	
	CenterOnParent();
	SetSizerAndFit(sizer); // use the sizer for layout and set size and hints
}



ArrayPopupDialog::~ArrayPopupDialog()
{
}



////////////////////////////////////////////////////////////////////////////////////////

AlignRightGridCellAttrProvider::AlignRightGridCellAttrProvider()
{
	m_attr_to_align_right = new wxGridCellAttr;
	m_attr_to_align_right->SetAlignment(wxALIGN_RIGHT, wxALIGN_CENTER);
}

AlignRightGridCellAttrProvider::~AlignRightGridCellAttrProvider()
{
	m_attr_to_align_right->DecRef();
}

wxGridCellAttr *AlignRightGridCellAttrProvider::GetAttr(int row, int col,
	wxGridCellAttr::wxAttrKind  kind /* = wxGridCellAttr::Any */) const
{
	wxGridCellAttr *attr = wxGridCellAttrProvider::GetAttr(row, col, kind);

	if (!attr)
	{
		attr = m_attr_to_align_right;
		attr->IncRef();
	}
	else
	{
		if (!attr->HasBackgroundColour())
		{
			wxGridCellAttr *attrNew = attr->Clone();
			attr->DecRef();
			attr = attrNew;
			attr->SetAlignment(wxALIGN_RIGHT, wxALIGN_CENTER);
		}
	}

	return attr;
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
	//		wxGridTableBase *table = grid.GetTable();
	if (GridChoiceData *vgd = (GridChoiceData *)grid.GetTable())
	{
		wxString text;
		long choiceno;
		SetParameters(vgd->GetChoices(row, col));
		//table->GetValue(row, col).ToLong(&choiceno);
		vgd->GetValue(row, col).ToLong(&choiceno);
		if ((choiceno > -1) && (choiceno < (int)m_choices.size()))
			text.Printf(wxT("%s"), m_choices[choiceno].c_str());
		return text;
	}
	else
		return wxEmptyString;
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


GridCellChoiceEditor::GridCellChoiceEditor()
:wxGridCellEditor()
{
	m_index = -1;
}

wxGridCellEditor *GridCellChoiceEditor::Clone() const
{
	GridCellChoiceEditor *editor = new GridCellChoiceEditor();
	editor->m_index = m_index;
	editor->m_choices = m_choices;
	return editor;
}

void GridCellChoiceEditor::Create(wxWindow* parent, wxWindowID id, wxEvtHandler* evtHandler)
{
	int style = wxTE_PROCESS_ENTER |
		wxTE_PROCESS_TAB | wxCB_READONLY |
		wxBORDER_NONE;
	m_control = new wxComboBox(parent, id, wxEmptyString,
		wxDefaultPosition, wxDefaultSize, m_choices, style);

	wxGridCellEditor::Create(parent, id, evtHandler);
}


void GridCellChoiceEditor::UpdateComboBox()
{ // original combo box in Create method of ancestor 
//	create and destroy to support sorting and populating with current selections/
	int style = wxTE_PROCESS_ENTER |
		wxTE_PROCESS_TAB | wxCB_READONLY |
		wxBORDER_NONE;

	wxWindow *p = m_control->GetParent();
	wxWindowID id = m_control->GetId();
	wxPoint pt = m_control->GetPosition();
	wxSize sz = m_control->GetSize();
	m_control->Destroy();
	
	m_control = new wxComboBox(p, id, wxEmptyString, pt, sz, m_choices,	style);
}

void GridCellChoiceEditor::SetSize(const wxRect& rect)
{
	wxASSERT_MSG(m_control,
		wxT("The GridCellChoiceEditor must be created first!"));

	// Check that the height is not too small to fit the combobox.
	wxRect rectTallEnough = rect;
	const wxSize bestSize = m_control->GetBestSize();
	const wxCoord diffY = bestSize.GetHeight() - rectTallEnough.GetHeight();
	if (diffY > 0)
	{
		// Do make it tall enough.
		rectTallEnough.height += diffY;

		// Also centre the effective rectangle vertically with respect to the
		// original one.
		rectTallEnough.y -= diffY / 2;
	}
	//else: The rectangle provided is already tall enough.

	wxGridCellEditor::SetSize(rectTallEnough);
}

void GridCellChoiceEditor::PaintBackground(wxDC& dc,
	const wxRect& rectCell,
	const wxGridCellAttr& attr)
{
	// as we fill the entire client area, don't do anything here to minimize
	// flicker

	// TODO: It doesn't actually fill the client area since the height of a
	// combo always defaults to the standard.  Until someone has time to
	// figure out the right rectangle to paint, just do it the normal way.
	wxGridCellEditor::PaintBackground(dc, rectCell, attr);
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

	GridChoiceData *vgd = (GridChoiceData *)grid->GetTable();
	SetParameters(vgd->GetChoices(row, col));
	UpdateComboBox();

	if (vgd->CanGetValueAs(row, col, wxGRID_VALUE_NUMBER))
	{
		m_index = vgd->GetValueAsLong(row, col);
	}
	else
	{
		wxString startValue = vgd->GetValue(row, col);
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

void GridCellChoiceEditor::Reset()
{
	// find the right position, or default to the first if not found
	int pos = Combo()->FindString(m_value);
	if (pos == wxNOT_FOUND)
		pos = 0;
	Combo()->SetSelection(pos);
}

void GridCellChoiceEditor::SetParameters(const wxString& params)
{
	if (!params)
	{
		// what can we do?
		return;
	}

	m_choices.Empty();

	wxStringTokenizer tk(params, wxT(','));
	while (tk.HasMoreTokens())
	{
		m_choices.Add(tk.GetNextToken());
	}
}

// return the value in the text control
wxString GridCellChoiceEditor::GetValue() const
{
	return Combo()->GetValue();
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


