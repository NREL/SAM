#ifndef __parametric_qs_h
#define __parametric_qs_h

#include <wx/wx.h>
#include <wex/numeric.h>
#include <vector>
#include "case.h"

class Parametric_QS : public wxDialog
{
public:
	Parametric_QS(wxWindow *parent, Case *c);

	void InitForm(CaseWindow *cwin, ParametricData &par);
	void UpdateFromParametricData();
	ParametricData &GetParametricData();

	void OnEnableChange(wxCommandEvent &evt);
	void OnRemoveVariable(wxCommandEvent &evt);
	void OnAddVariable(wxCommandEvent &evt);
	void OnVariableSelect(wxCommandEvent &evt);
	void OnVarDblClick(wxCommandEvent &evt);
	void OnEditValues(wxCommandEvent &evt);
	void OnValueDblClick(wxCommandEvent &evt);

	void RefreshVariableList();
	void RefreshValuesList();

	wxString GetBaseCaseValue(const wxString &varname);
	wxArrayString GetValuesList(const wxString &varname);
	wxArrayString GetValuesDisplayList(const wxString &varname);
	void SetValuesList(const wxString &varname, const wxArrayString &values);

	bool ShowEditValuesDialog(const wxString &title,
		wxArrayString &values, const wxString &varname);
	bool ShowNumericValuesDialog(const wxString &title,	wxArrayString &values);
	bool ShowFixedDomainDialog(const wxString &title,
		const wxArrayString &names, const wxArrayString &labels, wxArrayString &list,
		bool expand_all);

	void UpdateCaseParametricData();

	void OnCommand( wxCommandEvent &evt );

private:
	wxListBox *lstValues;
	wxListBox *lstVariables;

	Case *m_case;
	wxArrayString m_input_names;
	std::vector<wxArrayString> m_input_values;

	DECLARE_EVENT_TABLE()
};

#endif

