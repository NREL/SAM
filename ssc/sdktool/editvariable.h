#ifndef __EditVariableForm_h
#define __EditVariableForm_h

#include <wx/wx.h>
#include <wx/grid.h>

#include "dllinvoke.h"

class wxNumericCtrl;
class wxExtGridCtrl;
class wxRadioBox;

class EditVariableDialog : public wxDialog
{
public:
	EditVariableDialog( wxWindow *parent, const wxString &title );


	void SetVarData( var_data &data ) { m_var = data; UpdateForm(); }
	void GetVarData( var_data &data ) { data = m_var; }
	void UpdateForm();

private:	
	wxRadioBox *rbgVarType;
	wxExtGridCtrl *grdArrMat;
	wxButton *btnChooseFile;
	wxTextCtrl *txtValue;
	wxNumericCtrl *numValue;
	wxNumericCtrl *numRows;
	wxNumericCtrl *numCols;

	void OnShortcut( wxCommandEvent &evt);
	void OnTypeChange( wxCommandEvent &evt );
	void OnTextChange( wxCommandEvent &evt );
	void OnNumChange( wxCommandEvent &evt);
	void OnGridCellChange( wxGridEvent &evt );
	void OnRowsColsChange( wxCommandEvent &evt );
	void OnChooseFile( wxCommandEvent &evt );
	
	var_data m_var;

	DECLARE_EVENT_TABLE()
};

#endif

