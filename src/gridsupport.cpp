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
#include <wx/stattext.h>
#include <wx/busyinfo.h>
#include <wx/clipbrd.h>


#include <wex/metro.h>
#include <wex/ole/excelauto.h>

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
	if (type == "LossAdjustment")
	{
		obj->CreateNative(grid);
		obj->Show(false); // hide label drawing on grid
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);
		AFLossAdjustmentCtrl *la = obj->GetNative<AFLossAdjustmentCtrl>();
		la->DoEdit();
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::OBJ_TO_VAR);
		obj->DestroyNative(); // remove display from grid
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
//		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);
		AFDataArrayButton *da = obj->GetNative<AFDataArrayButton>();
		wxArrayString il = vi->IndexLabels;
		int mode = 2; // variable size
		if (il.Count() > 0) da->SetDataLabel(il[0]); // first Index label element is column label
		if (il.Count() < 2)	{
			size_t s = vv->Length();
			if (s == 8760)
				mode = 0;
			else if (s == (s / 8760) * 8760)
				mode = 1;
		}
		else {
			mode = wxAtoi(il[1]);
		}
		da->SetMode(mode); // resetting to 8760 for multiples (overwriting values)
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);
		wxCommandEvent evt = wxCommandEvent(wxEVT_BUTTON);
		da->OnPressed(evt);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "DataLifetimeArray")
	{
		obj->CreateNative(grid);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);
		AFDataLifetimeArrayButton *da = obj->GetNative<AFDataLifetimeArrayButton>();
		wxCommandEvent evt = wxCommandEvent(wxEVT_BUTTON);
		da->OnPressed(evt);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::OBJ_TO_VAR);
	}

	else if (type == "StringArray")
	{
		obj->CreateNative(grid);
		ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);
		AFStringArrayButton *da = obj->GetNative<AFStringArrayButton>();
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
	else if (type == "DataLifetimeMatrix")
	{
	obj->CreateNative(grid);
	AFDataLifetimeMatrixButton* da = obj->GetNative<AFDataLifetimeMatrixButton>();
	ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::VAR_TO_OBJ);
	da->SetColumnLabels(vi->Units); // column headers for inputs browser display
	wxCommandEvent evt = wxCommandEvent(wxEVT_BUTTON);
	da->OnPressed(evt);
	ActiveInputPage::DataExchange(obj, *vv, ActiveInputPage::OBJ_TO_VAR);
	}
	else if (type == "ShadingFactors")
	{
		if (vi && vi->Group.Lower()=="shading and snow") // PV
			obj->Property("ShowDBOptions").Set(true);
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

	// necessary for correct event processing to call HideCellEditor in wxGrid base class
	m_text->SetFocus();

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

	wxString var_name = vgd->GetVarName(row,col);
	wxString var_label = vi->Label;
	//wxString var_name = vgd->GetValue(row, 0);
	//wxString var_label = vgd->GetValue(row, 1);

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


void GridCellArrayEditor::Create(wxWindow *parent, wxWindowID id, wxEvtHandler* pEvtHandler)
{
	m_parent = parent;
//	m_text = new wxStaticText(parent, id, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxST_NO_AUTORESIZE | wxST_ELLIPSIZE_END);
//	SetControl(m_text);
	long style = wxTE_PROCESS_ENTER | wxTE_PROCESS_TAB | wxNO_BORDER;

	wxTextCtrl* const text = new wxTextCtrl(parent, id, wxEmptyString,
		wxDefaultPosition, wxDefaultSize,	style);
	text->SetMargins(0, 0);
	m_control = text;
	SetControl(m_control);
//	SetWindow(m_control);
	wxGridCellEditor::Create(parent, id, pEvtHandler);
}

void GridCellArrayEditor::Reset()
{
//	m_text->SetLabel(m_cell_value);
	Text()->SetValue(m_cell_value);
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

	// fixes parametric bug from Aron 11/12/14
	// necessary for correct event processing to call HideCellEditor in wxGrid base class
	Text()->SetFocus();
	return true;  // all ok!
}

void GridCellArrayEditor::BeginEdit(int row, int col, wxGrid *pGrid)
{
	/* event values are not preserved*/
	m_cell_value = GetString(row, col, pGrid);
//	m_text->SetLabel(m_cell_value);
	Text()->SetValue(m_cell_value);

	GridChoiceData *vgd = static_cast<GridChoiceData *>(pGrid->GetTable());
	VarValue *vv = vgd->GetVarValue(row, col);
	wxString title = vgd->GetColLabelValue(col) + wxString::Format(" for run %d", row + 1);
	wxString label = vgd->GetColLabelValue(col);

	DisplayEditor( title, label, pGrid, vv);

	m_new_cell_value = m_cell_value;
//	m_text->SetLabel(m_new_cell_value);
	Text()->SetValue(m_cell_value);
	//	pGrid->SaveEditControlValue();
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



bool GridCellArrayEditor::EndEdit(int , int , const wxGrid *, const wxString& WXUNUSED(oldval), wxString *newval)
{
	wxString new_cell_value = m_new_cell_value;
	if (new_cell_value == m_cell_value)
		return false; // no change

	m_cell_value = new_cell_value;

	if (newval)
		*newval = m_cell_value;

//	m_text->SetLabel(m_cell_value);
	Text()->SetValue(m_cell_value);

	return true;
}

void GridCellArrayEditor::ApplyEdit(int , int , wxGrid *)
{
// read only display
	m_cell_value.clear();
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



wxGridCellRenderer *GridCellCalculatedRenderer::Clone() const
{
	GridCellCalculatedRenderer *renderer = new GridCellCalculatedRenderer;
	return renderer;
}

wxString GridCellCalculatedRenderer::GetString(const wxGrid& grid, int row, int col)
{
	if (GridChoiceData *vgd = (GridChoiceData *)grid.GetTable())
	{
		wxString text = wxEmptyString;
		if (VarValue *vv = vgd->GetVarValue(row, col))
		{
			text = vv->AsString();
		}
		return text;
	}
	else
		return wxEmptyString;
}

void GridCellCalculatedRenderer::Draw(wxGrid& grid,
	wxGridCellAttr& attr,
	wxDC& dc,
	const wxRect& rectCell,
	int row, int col,
	bool isSelected)
{
//	grid.RefreshAttr(row, col);
	wxGridCellRenderer::Draw(grid, attr, dc, rectCell, row, col, isSelected);
//	grid.RefreshAttr(row, col);

	SetTextColoursAndFont(grid, attr, dc, isSelected);
//	grid.RefreshAttr(row, col);

	// draw the text left aligned by default
	int hAlign = wxALIGN_LEFT,
		vAlign = wxALIGN_INVALID;
	attr.GetNonDefaultAlignment(&hAlign, &vAlign);

	wxRect rect = rectCell;
	rect.Inflate(-1);

	grid.DrawTextRectangle(dc, GetString(grid, row, col), rect, hAlign, vAlign);
//	grid.RefreshAttr(row, col);
}

wxSize GridCellCalculatedRenderer::GetBestSize(wxGrid& grid,
	wxGridCellAttr& attr,
	wxDC& dc,
	int row, int col)
{
	return DoGetBestSize(attr, dc, GetString(grid, row, col));
}



GridCellCalculatedEditor::GridCellCalculatedEditor()
{
}


void GridCellCalculatedEditor::Create(wxWindow *parent, wxWindowID id, wxEvtHandler* pEvtHandler)
{
	m_parent = parent;
	long style = wxTE_READONLY | wxNO_BORDER;

	wxTextCtrl* const text = new wxTextCtrl(parent, id, wxEmptyString,
		wxDefaultPosition, wxDefaultSize, style);
	text->SetMargins(0, 0);
	m_control = text;
	SetControl(m_control);
//	SetWindow(m_control);
	wxGridCellEditor::Create(parent, id, pEvtHandler);
}

void GridCellCalculatedEditor::Reset()
{
	//	m_text->SetLabel(m_cell_value);
	Text()->SetValue(m_cell_value);
}


void GridCellCalculatedEditor::SetSize(const wxRect &rect_orig)
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

void GridCellCalculatedEditor::PaintBackground(wxDC& WXUNUSED(dc),
	const wxRect& WXUNUSED(rectCell),
	const wxGridCellAttr& WXUNUSED(attr))
{
}

bool GridCellCalculatedEditor::IsAcceptedKey(wxKeyEvent& event)
{
		return wxGridCellEditor::IsAcceptedKey(event);
}


bool GridCellCalculatedEditor::DisplayEditor(wxString &, wxString &, wxGrid *, VarValue *)
{
//	Text()->SetFocus();

	return true;  // all ok!
}

void GridCellCalculatedEditor::BeginEdit(int row, int col, wxGrid *pGrid)
{
	m_cell_value = GetString(row, col, pGrid);
	Text()->SetValue(m_cell_value);

	m_new_cell_value = m_cell_value;
	Text()->SetValue(m_cell_value);
}

wxString GridCellCalculatedEditor::GetString(int row, int col, const wxGrid *grid)
{
	if (GridChoiceData *vgd = (GridChoiceData *)grid->GetTable())
	{
		wxString text = wxEmptyString;
		if (VarValue *vv = vgd->GetVarValue(row, col))
		{
			text = vv->AsString();
		}
		return text;
	}
	else
		return wxEmptyString;
}



bool GridCellCalculatedEditor::EndEdit(int , int , const wxGrid *, const wxString& WXUNUSED(oldval), wxString *newval)
{
	wxString new_cell_value = m_new_cell_value;
	if (new_cell_value == m_cell_value)
		return false; // no change

	m_cell_value = new_cell_value;

	if (newval)
		*newval = m_cell_value;

	Text()->SetValue(m_cell_value);

	return true;
}

void GridCellCalculatedEditor::ApplyEdit(int , int , wxGrid *)
{
	// read only display
	m_cell_value.clear();
}


wxString GridCellCalculatedEditor::GetValue() const
{
	return m_cell_value;
}

wxGridCellEditor *GridCellCalculatedEditor::Clone() const
{
	return new GridCellCalculatedEditor();
}

////////////////////////////////////////////////////////////////////////////////////////



enum { ID_APD_CLIPBOARD, ID_APD_CSV, ID_APD_EXCEL};

BEGIN_EVENT_TABLE(ArrayPopupDialog, wxDialog)
	EVT_BUTTON(ID_APD_CLIPBOARD, ArrayPopupDialog::OnCommand)
	EVT_BUTTON(ID_APD_CSV, ArrayPopupDialog::OnCommand)
	EVT_BUTTON(ID_APD_EXCEL, ArrayPopupDialog::OnCommand)
END_EVENT_TABLE()

ArrayPopupDialog::ArrayPopupDialog(wxWindow *parent, const wxString &title, const wxString &label, VarValue *vv ) 
	: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(600, 500), wxRESIZE_BORDER | wxDEFAULT_DIALOG_STYLE), m_vv(vv)
{
	
#ifdef __WXMSW__
	SetIcon(wxICON(appicon));
#endif	
	SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

	if (!m_vv)  return;

	if (vv->Type() != VV_ARRAY) return;
	
	std::vector<double> vec = vv->Array();

	int rows = vec.size();
	int cols = 2;

	m_grid = new wxGrid(this, wxID_ANY);
	m_grid->CreateGrid(rows, cols);

	m_grid->GetTable()->SetAttrProvider(new AlignRightGridCellAttrProvider());

	m_grid->HideRowLabels();
	int vec_size = vec.size();
	wxString index_label = "Index";
	if (vec_size == 12)
		index_label = "Month";
	else if (vec_size == 8760)
		index_label = "Hour";

	m_grid->SetColLabelValue(0, index_label);

	m_grid->SetColLabelValue(1, label);


	for (int i = 0; i < vec_size; i++)
	{
		if (vec_size != 12) m_grid->SetCellValue(i, 0, wxString::Format("%d", i));
		m_grid->SetCellValue(i, 1, wxString::Format("%lg", vec[i]));
	}


	if (vec_size == 12)
	{
		m_grid->SetCellValue(0, 0, "Jan");
		m_grid->SetCellValue(1, 0, "Feb");
		m_grid->SetCellValue(2, 0, "Mar");
		m_grid->SetCellValue(3, 0, "Apr");
		m_grid->SetCellValue(4, 0, "May");
		m_grid->SetCellValue(5, 0, "Jun");
		m_grid->SetCellValue(6, 0, "Jul");
		m_grid->SetCellValue(7, 0, "Aug");
		m_grid->SetCellValue(8, 0, "Sep");
		m_grid->SetCellValue(9, 0, "Oct");
		m_grid->SetCellValue(10, 0, "Nov");
		m_grid->SetCellValue(11, 0, "Dec");
	}

	m_grid->EnableEditing(false);
	
	wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	// TODO want text extent of title bar title
	wxSize sz = GetTextExtent(title);
	wxString spacer = " ";
	spacer.Pad(sz.GetWidth(), ' ', false);
	sz = GetTextExtent(spacer);
	int width = sz.GetWidth() - 20 ; // subtract scrollbar width

	m_grid->SetColSize(0, (int)(width / 2.0));
	m_grid->SetColSize(1, width - (int)(width / 2.0));
	
	wxBoxSizer *cf_tools = new wxBoxSizer(wxHORIZONTAL);
	cf_tools->Add(new wxMetroButton(this, ID_APD_CLIPBOARD, "Copy to clipboard"), 0, wxALL, 0);
	cf_tools->Add(new wxMetroButton(this, ID_APD_CSV, "Save as CSV"), 0, wxALL, 0);
#ifdef __WXMSW__
	cf_tools->Add(new wxMetroButton(this, ID_APD_EXCEL, "Send to Excel"), 0, wxALL, 0);
#endif
	cf_tools->AddStretchSpacer();
	cf_tools->Add(new wxMetroButton(this, wxID_OK, "Close"), 0, wxALL, 0);

	sizer->Add(cf_tools, 0, wxALL|wxEXPAND, 0 );

	sizer->Add(m_grid,
		1,            // make vertically stretchable
		wxEXPAND |    // make horizontally stretchable
		wxALL,        //   and make border all around
		0);         // set border width to 10

	SetSizer(sizer); // use the sizer for layout and set size and hints
}



ArrayPopupDialog::ArrayPopupDialog(wxWindow *parent, const wxString &title, const wxArrayString &labels, std::vector<std::vector<double> > &values_vec)
: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(600, 500), wxRESIZE_BORDER | wxDEFAULT_DIALOG_STYLE)
{
#ifdef __WXMSW__
	SetIcon(wxICON(appicon));
#endif	
	SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

	// check that all vectors the same size
	int values_vec_size = values_vec.size();
	if (values_vec_size <= 0) return;
	int vec_size = values_vec[0].size();

	bool all_same_size = true;

	for (int i = 1; i < values_vec_size; i++)
	{
		all_same_size &= (vec_size == (int)values_vec[i].size());
		if (!all_same_size) break;
	}

	if (!all_same_size) return;

	int rows = vec_size;
	int cols = values_vec_size + 1;

	m_grid = new wxGrid(this, wxID_ANY);
	
	m_grid->CreateGrid(rows, cols);

	m_grid->Freeze();

	m_grid->GetTable()->SetAttrProvider(new AlignRightGridCellAttrProvider());

	m_grid->HideRowLabels();
	wxString index_label = "Index";
	if (vec_size == 12)
		index_label = "Month";
	else if (vec_size == 8760)
		index_label = "Hour";


	m_grid->SetColLabelValue(0, index_label);


	for (int col = 0; col < values_vec_size; col++)
	{
		if ((int)labels.Count()>col)
			m_grid->SetColLabelValue(col+1, labels[col]);
		for (int row = 0; row < vec_size; row++)
			m_grid->SetCellValue(row, col+1, wxString::Format("%lg", values_vec[col][row]));
	}

	if (vec_size == 12)
	{
		m_grid->SetCellValue(0, 0, "Jan");
		m_grid->SetCellValue(1, 0, "Feb");
		m_grid->SetCellValue(2, 0, "Mar");
		m_grid->SetCellValue(3, 0, "Apr");
		m_grid->SetCellValue(4, 0, "May");
		m_grid->SetCellValue(5, 0, "Jun");
		m_grid->SetCellValue(6, 0, "Jul");
		m_grid->SetCellValue(7, 0, "Aug");
		m_grid->SetCellValue(8, 0, "Sep");
		m_grid->SetCellValue(9, 0, "Oct");
		m_grid->SetCellValue(10, 0, "Nov");
		m_grid->SetCellValue(11, 0, "Dec");
	}
	else
	{
		for (int row = 0; row < vec_size; row++)
			m_grid->SetCellValue(row, 0, wxString::Format("%d", row));
	}

	if (vec_size == 1) m_grid->SetColSize(0, 0); // hide index column for single values.
	m_grid->EnableEditing(false);
	m_grid->Thaw();


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
	for (int i = 1; i < cols; i++)
	{
		m_grid->SetColSize(i, col_width);
		tot_width += col_width;
	}
	m_grid->SetColSize(0, width - tot_width);
	
	wxBoxSizer *cf_tools = new wxBoxSizer(wxHORIZONTAL);
	cf_tools->Add(new wxMetroButton(this, ID_APD_CLIPBOARD, "Copy to clipboard"), 0, wxALL, 2);
	cf_tools->Add(new wxMetroButton(this, ID_APD_CSV, "Save as CSV"), 0, wxALL, 2);
#ifdef __WXMSW__
	cf_tools->Add(new wxMetroButton(this, ID_APD_EXCEL, "Send to Excel"), 0, wxALL, 2);
#endif
	cf_tools->AddStretchSpacer();
	cf_tools->Add(new wxMetroButton(this, wxID_OK, "Close"), 0, wxALL, 2);

	sizer->Add(cf_tools, 0, wxALL|wxEXPAND, 0);

	sizer->Add(m_grid,
		1,            // make vertically stretchable
		wxEXPAND |    // make horizontally stretchable
		wxALL,        //   and make border all around
		0);         // set border width to 10
	
	SetSizer(sizer); // use the sizer for layout and set size and hints
}

void ArrayPopupDialog::OnCommand(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_APD_CLIPBOARD:
		CopyToClipboard();
		break;
	case ID_APD_CSV:
		SaveToCSV();
		break;
#ifdef __WXMSW__
	case ID_APD_EXCEL:
		SendToExcel();
		break;
#endif
	}
}

void ArrayPopupDialog::GetTextData(wxString &dat, char sep)
{
	dat = wxEmptyString;
	if (!m_grid)
		return;

	wxGridTableBase *m_grid_data = m_grid->GetTable();
	size_t approxbytes = m_grid_data->GetNumberRows() * 15 * m_grid_data->GetNumberCols();
	dat.Alloc(approxbytes);

	int c;

	for (c = 0; c<m_grid_data->GetNumberCols(); c++)
	{
		wxString label = m_grid_data->GetColLabelValue(c);
		label.Replace('\n', " | ");

		if (sep == ',')
			dat += '"' + label + '"';
		else
			dat += label;

		if (c < m_grid_data->GetNumberCols() - 1)
			dat += sep;
		else
			dat += '\n';
	}

	for (int r = 0; r<m_grid_data->GetNumberRows(); r++)
	{
		for (c = 0; c<m_grid_data->GetNumberCols(); c++)
		{
			dat += m_grid_data->GetValue(r, c);

			if (c < m_grid_data->GetNumberCols() - 1)
				dat += sep;
			else
				dat += '\n';
		}
	}
}


void ArrayPopupDialog::CopyToClipboard()
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

void ArrayPopupDialog::SaveToCSV()
{
	wxFileDialog fdlg(this, "Save as CSV", wxEmptyString, "results.csv", "Comma-separated values (*.csv)|*.csv", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
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

void ArrayPopupDialog::SendToExcel()
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

void ArrayPopupDialog::SendToExcelSheet(wxExcelAutomation& xl, wxString &sheetName)
{
	wxBusyInfo busy("Processing data table... please wait");
	wxString dat;
	GetTextData(dat, '\t');

	// strip commas per request from Paul 5/23/12 meeting
	dat.Replace(",", "");

#ifdef __WXMSW__

//	if (wxTheClipboard->Open())
	{
//		wxTheClipboard->SetData(new wxTextDataObject(dat));
//		wxTheClipboard->Close();
		xl.PasteNewWorksheet(sheetName, dat);
	}
#endif
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


VariablePopupDialog::VariablePopupDialog(wxWindow *parent, wxUIObject *obj, wxString &name, VarValue *vv, VarInfo *vi)
	: wxDialog(parent, wxID_ANY, "Variable Editor", wxDefaultPosition, wxDefaultSize), m_obj(obj), m_vv(vv), m_vi(vi)
{
	if ((m_vv == 0) || (m_vi == 0) || (m_obj == 0)) return;

	// causing "Library not found" message from parametrics
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

GridCellChoiceRenderer::GridCellChoiceRenderer(const wxString& choices)
{
	m_init = true;
	SetParameters(choices);
}

GridCellChoiceRenderer::GridCellChoiceRenderer()
{
	m_init = false;
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
		if (!m_init)
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
{
	m_index = -1;
	m_init = false;
}

GridCellChoiceEditor::GridCellChoiceEditor(const wxString& choices)
{
	m_index = -1;
	SetParameters(choices);
	m_init = true;
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
	wxComboBox* cbo = new wxComboBox(parent, id, wxEmptyString,
		wxDefaultPosition, wxDefaultSize, m_choices, style);
	m_control = cbo;
	wxGridCellEditor::Create(parent, id, evtHandler);
}


void GridCellChoiceEditor::UpdateComboBox()
{ 
	Combo()->Clear();

	Combo()->Append(m_choices);

	Combo()->Refresh();
	Combo()->Update();
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


void GridCellChoiceEditor::PaintBackground(wxDC& ,
	const wxRect& ,
	const wxGridCellAttr& )
{
	// as we fill the entire client area, don't do anything here to minimize
	// flicker

	// TODO: It doesn't actually fill the client area since the height of a
	// combo always defaults to the standard.  Until someone has time to
	// figure out the right rectangle to paint, just do it the normal way.
//	wxGridCellEditor::PaintBackground(dc, rectCell, attr);
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
	if (!m_init)
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
		wxString choice = tk.GetNextToken();
		m_choices.Add(choice);
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


