#ifndef __DefaultsManager_h
#define __DefaultsManager_h

//#include <wx/wx.h>
#include <wx/frame.h>
#include <wex/uiform.h>
#include <wex/exttext.h>
#include <wex/extgrid.h>
#include <wex/numeric.h>

#include "variables.h"
#include "main.h"


class DefaultsManager : public wxPanel
{
public:
	DefaultsManager(wxWindow *parent, int id=-1);
	virtual ~DefaultsManager();
	/* class members */

	wxButton *btnRWAll;
	wxStaticBox *GroupBox1;
	wxStaticText *Label9;
	wxExtGridCtrl *grdModifyTable;
	wxButton *btnModifyMultiple;
	wxNumericCtrl *numTableVars;
	wxButton *btnClose;
	wxStaticText *Label8;
	wxStaticText *Label7;
	wxTextCtrl *txtOutput;
	wxCheckListBox *cklConfigurations;
	wxButton *btnUnselAll;
	wxButton *btnSelAll;
	wxStaticText *Label5;
	wxStaticText *Label4;
	wxStaticText *Label6;
	wxButton *btnApplyLibrary;
	wxExtTextCtrl *txtLibSuffix;
	wxCheckBox *chkAdd;
	wxButton *btnRemove;
	wxButton *btnQuery;
	wxButton *btnModify;
	wxCheckBox *chkModifyType;
	wxComboBox *cboVarType;
	wxListBox *lstLibTypes;
	wxListBox *lstLibItems;
	wxExtTextCtrl *txtVarValue;
	wxExtTextCtrl *txtVarName;
	wxStaticText *lblAllConfigurations;
	wxButton *btnLookup;
	wxStaticText *Label3;
	wxStaticText *Label2;
	wxStaticText *Label1;

	void ClearLog();
	void Log(const wxString &s);

	void OnSelUnselAll(wxCommandEvent &evt);
	void OnRemove(wxCommandEvent &evt);
	void OnModify(wxCommandEvent &evt);
	void OnQuery(wxCommandEvent &evt);
	void OnLookup(wxCommandEvent &evt);
	void OnApplyLib(wxCommandEvent &evt);
	void OnLibTypeSel(wxCommandEvent &evt);
	void OnGridDoubleClick(wxGridEvent &evt);
	void OnNumRowsChange(wxCommandEvent &evt);
	void OnModifyMultiple(wxCommandEvent &evt);
	void OnRWAll( wxCommandEvent &evt );

	void ModifyAdd(int cfg_idx, const wxString &var_name,
		const wxString &var_value,
		bool en_type_change, int var_type,
		bool en_add );

	wxString LookupVariable();

private:	
	wxArrayString mTechList, mFinList;
	DECLARE_EVENT_TABLE()
};

class DefaultsManagerDialog : public wxDialog
{
public:
	DefaultsManagerDialog(wxWindow *parent, const wxString &title, void *data = NULL);

	DefaultsManager *GetPanel() { return mPanel; }
	void OnCommand(wxCommandEvent &evt);
	void OnClose(wxCloseEvent &evt);
private:
	DefaultsManager *mPanel;
	DECLARE_EVENT_TABLE()
};

#endif

