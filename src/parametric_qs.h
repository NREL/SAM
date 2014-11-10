#ifndef __parametric_qs_h
#define __parametric_qs_h

#include <wx/wx.h>
#include <wex/numeric.h>
#include <vector>
#include "case.h"

class Parametric_QS : public wxPanel
{
public:
	Parametric_QS(wxWindow *parent, Case *c);

	wxListBox *lstValues;
	wxListBox *lstVariables;
	wxCheckBox *chkEnable;
	wxButton *btnRemove;
	wxStaticText *Label1;
	wxButton *btnEditValues;
	wxStaticText *Label2;
	wxButton *btnRemoveVar;
	wxButton *btnAddVar;


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

private:
	Case *m_case;
	wxArrayString m_input_names;
	std::vector<wxArrayString> m_input_values;
	DECLARE_EVENT_TABLE()
};

class Parametric_QSDialog : public wxDialog
{
public:
	Parametric_QSDialog(wxWindow *parent, const wxString &title, Case *c);

	Parametric_QS *GetPanel() { return mPanel; }
	void OnCommand(wxCommandEvent &evt);
	void OnClose(wxCloseEvent &evt);
private:
	Parametric_QS *mPanel;
	DECLARE_EVENT_TABLE()
};

#endif

