#ifndef __configsel_h
#define __configsel_h

#include <vector>

#include <wx/dialog.h>
#include <wx/event.h>

#include "main.h"

class wxChoiceTree;
class wxChoiceTreeItem;

class ConfigDialog : public wxDialog
{
public:
	ConfigDialog( wxWindow *parent, const wxSize &size = wxSize(700,570) );

	void SetConfiguration(const wxString &t, const wxString &f);
	bool GetConfiguration(wxString &t, wxString &f);

	void ShowResetCheckbox(bool b);
	bool ResetToDefaults();

private:
	void PopulateTech();
	bool ValidateSelections();
	void SelectExpandByMetaTag(wxChoiceTree *aftree, const wxString &tag);
	wxChoiceTreeItem *FindTag(wxChoiceTreeItem *parent, const wxString &tag);
	bool CheckItemForMetaTag(ConfigDatabase::TreeItem *item, wxArrayString *metalist);
	void AppendItem( wxChoiceTree *aftree, wxChoiceTreeItem *parent, ConfigDatabase::TreeItem *item, wxArrayString *metalist);
	void PopulateTree( wxChoiceTree *aftree, std::vector<ConfigDatabase::TreeItem*> &items, wxArrayString *metalist=NULL);
	void OnTechTree(wxCommandEvent &evt);
	void OnDoubleClick(wxCommandEvent &evt);

	wxChoiceTree *m_pTech;
	wxChoiceTree *m_pFin;

	wxString m_t, m_f;

	void OnHelp( wxCommandEvent &evt );

	wxCheckBox *m_pChkUseDefaults;
	DECLARE_EVENT_TABLE();
};

bool ShowConfigurationDialog( wxWindow *parent, wxString *tech, wxString *fin, bool *reset );


#endif

