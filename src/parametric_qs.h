#ifndef __parametric_qs_h
#define __parametric_qs_h

#include <wx/wx.h>
#include <wex/numeric.h>

class Case;
class CaseWindow;
class ParametricData;

class Parametric_QS : public wxPanel
{
public:
	Parametric_QS(wxWindow *parent, ParametricData &par, int id = -1);
//	virtual ~Parametric_QS();

	wxStaticBox *GroupBox1;
	wxNumericCtrl *numMaxCpus;
	wxStaticText *Label3;
	wxStaticBox *grpOutline;
	wxButton *btnMoveDown;
	wxButton *btnMoveUp;
	wxButton *btnLinkages;
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
//	void OnRemoveSim(wxCommandEvent &evt);
	void OnRemoveVariable(wxCommandEvent &evt);
	void OnAddVariable(wxCommandEvent &evt);
	void OnVariableSelect(wxCommandEvent &evt);
	void OnVarDblClick(wxCommandEvent &evt);
	void OnEditValues(wxCommandEvent &evt);
	void OnEditLinkages(wxCommandEvent &evt);
	void OnMoveUpDown(wxCommandEvent &evt);
	void OnValueDblClick(wxCommandEvent &evt);
	void OnValueSelection(wxCommandEvent &evt);
//	void OnNumCpusChange( wxCommandEvent &evt);

	void RefreshVariableList();
	void RefreshValuesList();

	wxArrayString GetValuesList(const wxString &varname);

private:
	CaseWindow *mCaseWin;
	Case *mCase;
	ParametricData &m_par;

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

