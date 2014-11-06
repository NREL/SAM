#ifndef __NumericVarEditForm_h
#define __NumericVarEditForm_h

#include <wx/wx.h>
#include <wex/numeric.h>

class NumericVarEditForm : public wxPanel
{
public:
	NumericVarEditForm(wxWindow *parent, int id=-1);

	wxStaticBox *GroupBox1;
	wxStaticBox *GroupBox2;
	wxButton *cmdCancel;
	wxButton *btnHelp;
	wxButton *cmdOk;
	wxButton *cmdUpdateValues;
	wxStaticText *lblNotification;
	wxButton *cmdMoveDown;
	wxListBox *lstValues;
	wxButton *cmdRemove;
	wxButton *cmdAddBefore;
	wxButton *cmdAddAfter;
	wxButton *cmdMoveUp;
	wxStaticText *Label3;
	wxStaticText *Label2;
	wxStaticText *Label1;
	wxNumericCtrl *numIncr;
	wxNumericCtrl *numEnd;
	wxNumericCtrl *numStart;

	void SetValues(const wxArrayString &values, bool int_only=false);
	void OnCommand(wxCommandEvent &evt);
	void GenerateValues();
	bool CheckRanges();

	bool bIntOnly;

	DECLARE_EVENT_TABLE()
};

class NumericVarEditFormDialog : public wxDialog
{
public:
	NumericVarEditFormDialog(wxWindow *parent, const wxString &title, void *data = NULL);

	NumericVarEditForm *GetPanel() { return mPanel; }
	void OnCommand(wxCommandEvent &evt);
	void OnClose(wxCloseEvent &evt);
private:
	NumericVarEditForm *mPanel;
	DECLARE_EVENT_TABLE()
};

#endif

