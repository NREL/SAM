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

#ifndef __gridsupport_h
#define __gridsupport_h

#include <wx/string.h>
#include <wx/grid.h>
#include <wx/frame.h>
#include <wx/renderer.h>
#include <wx/event.h>
#include <wx/dc.h>
#include <wx/button.h>
#include <wx/stattext.h>

#include <wex/uiform.h>
#include <wex/numeric.h>
#include <wex/exttext.h>
#include <wex/extgrid.h>
#include <wex/radiochoice.h>
#include <wex/diurnal.h>

#include "variables.h"
// UI widgets
#include "widgets.h"
#include "lossadj.h"
#include "ptlayoutctrl.h"
#include "troughloop.h"
#include "materials.h"
#include "shadingfactors.h"
#include "library.h"

#include <vector>

class GridChoiceData : public wxGridTableBase
{
public:
	virtual wxString GetChoices(int row, int col)=0;
	virtual VarValue* GetVarValue(int row, int col) = 0;
	virtual VarInfo* GetVarInfo(int row, int col) = 0;
	virtual wxString GetVarName(int row, int col) = 0;
};


class GridCellVarValueRenderer : public wxGridCellRenderer
{
public:
	GridCellVarValueRenderer();
	virtual ~GridCellVarValueRenderer(void);

	virtual void Draw(wxGrid &grid, wxGridCellAttr &attr, wxDC &dc, const wxRect &rect, int row, int col, bool isSelected);
	virtual wxSize GetBestSize(wxGrid &grid, wxGridCellAttr& attr, wxDC &dc, int row, int col);
	virtual wxGridCellRenderer *Clone() const;
	void SetTextColoursAndFont(const wxGrid& grid, const wxGridCellAttr& attr, wxDC& dc, bool isSelected);

private:
	wxString m_ellipsis;

};


class VariablePopupDialog : public wxDialog
{
public:
	VariablePopupDialog(wxWindow *parent, wxUIObject *obj, wxString &name, VarValue *vv, VarInfo *vi);
	~VariablePopupDialog();

	wxUIObject *GetUIObject();
private:
	wxUIObject *m_obj;
	VarValue *m_vv;
	VarInfo *m_vi;
};


class GridCellVarValueEditor : public wxEvtHandler, public wxGridCellEditor
{
public:
	GridCellVarValueEditor();
	virtual ~GridCellVarValueEditor(void);

	virtual void PaintBackground(wxDC& dc,
		const wxRect& rectCell,
		const wxGridCellAttr& attr);

	virtual void Create(wxWindow *parent, wxWindowID id, wxEvtHandler* pEvtHandler);
	virtual bool IsAcceptedKey(wxKeyEvent& event);
	virtual void SetSize(const wxRect &rect_orig);
	virtual void BeginEdit(int row, int col, wxGrid *pGrid);
	virtual bool EndEdit(int row, int col, const wxGrid *grid, const wxString &oldval, wxString *newval);
	virtual void ApplyEdit(int row, int col, wxGrid *grid);
	virtual void Reset();
	virtual wxString GetValue() const;
	virtual wxGridCellEditor *Clone() const;

private:
	wxString m_cell_value;
	wxString m_new_cell_value;
	wxStaticText *m_text;
	VarInfo *m_var_info;
	VarValue *m_var_value;
	wxWindow *m_parent;
	VariablePopupDialog *m_vpe;
	bool DisplayEditor(wxUIObject *obj, wxString &name, wxGrid *grid, VarValue *vv, VarInfo *vi);
	wxString GetDisplayString(wxString &var_string, int row, int col, const wxGrid *grid);

	DECLARE_NO_COPY_CLASS(GridCellVarValueEditor)
};

// renders a text string using arrays from the corresponding VarValue
// based on wxGridCellStringRenderer
class GridCellArrayRenderer : public wxGridCellStringRenderer
{
public:

	virtual void Draw(wxGrid& grid,
		wxGridCellAttr& attr,
		wxDC& dc,
		const wxRect& rect,
		int row, int col,
		bool isSelected);
	virtual wxSize GetBestSize(wxGrid& grid,
		wxGridCellAttr& attr,
		wxDC& dc,
		int row, int col);
	virtual wxGridCellRenderer *Clone() const;

protected:
	wxString GetString(const wxGrid& grid, int row, int col);

};

class ArrayPopupDialog : public wxDialog
{
public:
	ArrayPopupDialog(wxWindow *parent, const wxString &title, const wxString &label, VarValue *vv);
	ArrayPopupDialog(wxWindow *parent, const wxString &title, const wxArrayString &labels, std::vector<std::vector<float> > &values_vec);

private:
	void OnCommand(wxCommandEvent &evt);

	void CopyToClipboard();
	void SaveToCSV();
	void SendToExcel();
	void GetTextData(wxString &dat, char sep);

	void CreateUI();

	VarValue *m_vv;
	wxGrid *m_grid;

	DECLARE_EVENT_TABLE()
};

class AlignRightGridCellAttrProvider : public wxGridCellAttrProvider
{
public:
	AlignRightGridCellAttrProvider();
	virtual ~AlignRightGridCellAttrProvider();

	virtual wxGridCellAttr *GetAttr(int row, int col,
		wxGridCellAttr::wxAttrKind  kind) const;

private:
	wxGridCellAttr *m_attr_to_align_right;
};

class GridCellArrayEditor : public wxEvtHandler, public wxGridCellEditor
{
public:
	GridCellArrayEditor();

	virtual void PaintBackground(wxDC& dc,
		const wxRect& rectCell,
		const wxGridCellAttr& attr);

	virtual void Create(wxWindow *parent, wxWindowID id, wxEvtHandler* pEvtHandler);
	virtual bool IsAcceptedKey(wxKeyEvent& event);
	virtual void SetSize(const wxRect &rect_orig);
	virtual void BeginEdit(int row, int col, wxGrid *pGrid);
	virtual bool EndEdit(int row, int col, const wxGrid *grid, const wxString &oldval, wxString *newval);
	virtual void ApplyEdit(int row, int col, wxGrid *grid);
	virtual void Reset();
	virtual wxString GetValue() const;
	virtual wxGridCellEditor *Clone() const;

private:
	wxString m_cell_value;
	wxString m_new_cell_value;
//	wxStaticText *m_text;
	wxTextCtrl *Text() const { return (wxTextCtrl *)m_control; }
	VarInfo *m_var_info;
	VarValue *m_var_value;
	wxWindow *m_parent;
	VariablePopupDialog *m_vpe;
	bool DisplayEditor(wxString &title, wxString &label, wxGrid *grid, VarValue *vv);
	wxString GetString(int row, int col, const wxGrid *grid);

	DECLARE_NO_COPY_CLASS(GridCellArrayEditor)
};



// renders a text string using calculated values from the corresponding VarValue
// based on wxGridCellStringRenderer
class GridCellCalculatedRenderer : public wxGridCellStringRenderer
{
public:

	virtual void Draw(wxGrid& grid,
		wxGridCellAttr& attr,
		wxDC& dc,
		const wxRect& rect,
		int row, int col,
		bool isSelected);
	virtual wxSize GetBestSize(wxGrid& grid,
		wxGridCellAttr& attr,
		wxDC& dc,
		int row, int col);
	virtual wxGridCellRenderer *Clone() const;

protected:
	wxString GetString(const wxGrid& grid, int row, int col);

};

class CalculatedGridCellAttrProvider : public wxGridCellAttrProvider
{
public:
	CalculatedGridCellAttrProvider();
	virtual ~CalculatedGridCellAttrProvider();

	virtual wxGridCellAttr *GetAttr(int row, int col,
		wxGridCellAttr::wxAttrKind  kind) const;

private:
	wxGridCellAttr *m_attr_to_calculated;
};


class GridCellCalculatedEditor : public wxEvtHandler, public wxGridCellEditor
{
public:
	GridCellCalculatedEditor();

	virtual void PaintBackground(wxDC& dc,
		const wxRect& rectCell,
		const wxGridCellAttr& attr);

	virtual void Create(wxWindow *parent, wxWindowID id, wxEvtHandler* pEvtHandler);
	virtual bool IsAcceptedKey(wxKeyEvent& event);
	virtual void SetSize(const wxRect &rect_orig);
	virtual void BeginEdit(int row, int col, wxGrid *pGrid);
	virtual bool EndEdit(int row, int col, const wxGrid *grid, const wxString &oldval, wxString *newval);
	virtual void ApplyEdit(int row, int col, wxGrid *grid);
	virtual void Reset();
	virtual wxString GetValue() const;
	virtual wxGridCellEditor *Clone() const;

private:
	wxString m_cell_value;
	wxString m_new_cell_value;
	//	wxStaticText *m_text;
	wxTextCtrl *Text() const { return (wxTextCtrl *)m_control; }
	VarInfo *m_var_info;
	VarValue *m_var_value;
	wxWindow *m_parent;
	VariablePopupDialog *m_vpe;
	bool DisplayEditor(wxString &title, wxString &label, wxGrid *grid, VarValue *vv);
	wxString GetString(int row, int col, const wxGrid *grid);

	DECLARE_NO_COPY_CLASS(GridCellCalculatedEditor)
};




// renders a text string using the corresponding VarInfo in GridTableBase for wxUIObject Choice
// based on wxGridCellEnumRenderer
class GridCellChoiceRenderer : public wxGridCellStringRenderer
{
public:
	GridCellChoiceRenderer();
	GridCellChoiceRenderer(const wxString& choices);

	virtual void Draw(wxGrid& grid,
		wxGridCellAttr& attr,
		wxDC& dc,
		const wxRect& rect,
		int row, int col,
		bool isSelected);
	virtual wxSize GetBestSize(wxGrid& grid,
		wxGridCellAttr& attr,
		wxDC& dc,
		int row, int col);
	virtual wxGridCellRenderer *Clone() const;
	void SetParameters(const wxString& params);

protected:
	wxString GetString(const wxGrid& grid, int row, int col);
	bool m_init;

	wxArrayString m_choices;
};

class GridCellChoiceEditor : public wxGridCellEditor
{
public:
	GridCellChoiceEditor();
	GridCellChoiceEditor(const wxString& choices);

	virtual void Create(wxWindow* parent,
		wxWindowID id,
		wxEvtHandler* evtHandler);

	virtual void SetSize(const wxRect& rect);

	
	virtual void PaintBackground(wxDC& dc,
		const wxRect& rectCell,
		const wxGridCellAttr& attr);


	virtual void Reset();

	// parameters string format is "item1[,item2[...,itemN]]"
	virtual void SetParameters(const wxString& params);

	virtual wxGridCellEditor *Clone() const;

	// added GetValue so we can get the value which is in the control
	virtual wxString GetValue() const;
	virtual void BeginEdit(int row, int col, wxGrid* grid);
	virtual bool EndEdit(int row, int col, const wxGrid* grid,
		const wxString& oldval, wxString *newval);
	virtual void ApplyEdit(int row, int col, wxGrid* grid);

	// update with choices based on table base row and column
	void UpdateComboBox();
private:
	long m_index;
	bool m_init;
	wxComboBox *Combo() const { return (wxComboBox *)m_control; }
	wxString        m_value;
	wxArrayString   m_choices;

	wxDECLARE_NO_COPY_CLASS(GridCellChoiceEditor);
};


// renders check box based on wxGridCellBoolRenderer and wxGridCellBoolEditor
// renderer for boolean fields
class GridCellCheckBoxRenderer : public wxGridCellRenderer
{
public:
	// draw a check mark or nothing
	virtual void Draw(wxGrid& grid,
		wxGridCellAttr& attr,
		wxDC& dc,
		const wxRect& rect,
		int row, int col,
		bool isSelected);

	// return the checkmark size
	virtual wxSize GetBestSize(wxGrid& grid,
		wxGridCellAttr& attr,
		wxDC& dc,
		int row, int col);

	virtual wxGridCellRenderer *Clone() const
	{
		return new GridCellCheckBoxRenderer;
	}

private:
	static wxSize ms_sizeCheckMark;
};

class GridCellCheckBoxEditor : public wxGridCellEditor
{
public:
	GridCellCheckBoxEditor() { }

	virtual void Create(wxWindow* parent,
		wxWindowID id,
		wxEvtHandler* evtHandler);

	virtual void SetSize(const wxRect& rect);
	virtual void Show(bool show, wxGridCellAttr *attr = NULL);

	virtual bool IsAcceptedKey(wxKeyEvent& event);
	virtual void BeginEdit(int row, int col, wxGrid* grid);
	virtual bool EndEdit(int row, int col, const wxGrid* grid,
		const wxString& oldval, wxString *newval);
	virtual void ApplyEdit(int row, int col, wxGrid* grid);

	virtual void Reset();
//	virtual void StartingClick();
//	virtual void StartingKey(wxKeyEvent& event);

	virtual wxGridCellEditor *Clone() const
	{
		return new GridCellCheckBoxEditor;
	}

	virtual wxString GetValue() const;

protected:
	wxCheckBox *CBox() const { return (wxCheckBox *)m_control; }

private:
	bool m_value;

	wxDECLARE_NO_COPY_CLASS(GridCellCheckBoxEditor);
};



#endif
