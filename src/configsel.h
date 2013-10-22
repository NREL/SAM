#ifndef __configsel_h
#define __configsel_h

#include <vector>

#include <wx/dialog.h>
#include <wx/event.h>

class wxChoiceTree;
class wxChoiceTreeItem;


class ConfigTree
{
public:
	ConfigTree();
	~ConfigTree();
	static ConfigTree &Get();

	bool Load( const wxString &cfg_file );

	struct TreeItem
	{
		~TreeItem() { Clear(); }
		void Clear() { 
			for( std::vector<TreeItem*>::iterator it = Children.begin();
				it != Children.end(); ++it ) delete *it;
			Children.clear();
		}

		wxString Label;
		wxString Description;
		wxString BmpName;
		wxString TypeTag;
		std::vector<TreeItem*> Children;
	};

	std::vector<TreeItem*> Tech() { return m_techTreeRoot.Children; }
	std::vector<TreeItem*> Fin() { return m_finTreeRoot.Children; }

private:	
	TreeItem m_techTreeRoot;
	TreeItem m_finTreeRoot;
};



class ConfigCtrl : public wxPanel
{
public:
	ConfigCtrl(wxWindow *parent, int id, 
		const wxPoint &pos=wxDefaultPosition, const wxSize &sz=wxDefaultSize, 
		long flags = wxBORDER_NONE);

	void SetConfiguration(const wxString &t, const wxString &f);
	bool GetConfiguration(wxString &t, wxString &f);

	void PopulateTech();
	bool ValidateSelections();
private:
	void SelectExpandByMetaTag(wxChoiceTree *aftree, const wxString &tag);
	wxChoiceTreeItem *FindTag(wxChoiceTreeItem *parent, const wxString &tag);
	bool CheckItemForMetaTag(ConfigTree::TreeItem *item, wxArrayString *metalist);
	void AppendItem( wxChoiceTree *aftree, wxChoiceTreeItem *parent, ConfigTree::TreeItem *item, wxArrayString *metalist);
	void PopulateTree( wxChoiceTree *aftree, std::vector<ConfigTree::TreeItem*> &items, wxArrayString *metalist=NULL);
	void OnTechTree(wxCommandEvent &evt);
	void OnDoubleClick(wxCommandEvent &evt);

	wxChoiceTree *m_pTech;
	wxChoiceTree *m_pFin;

	wxString m_t, m_f;

	DECLARE_EVENT_TABLE();
};

class ConfigDialog : public wxDialog
{
public:
	ConfigDialog( wxWindow *parent, const wxSize &size = wxSize(700,570) );

	void SetConfiguration(const wxString &t, const wxString &f);
	void GetConfiguration(wxString &t, wxString &f);

	void ShowResetCheckbox(bool b);
	bool ResetToDefaults();

private:
	void OnCommand(wxCommandEvent &evt);
	ConfigCtrl *m_cfgCtrl;
	wxCheckBox *m_pChkUseDefaults;
	DECLARE_EVENT_TABLE();
};

bool ShowConfigurationDialog( wxWindow *parent, wxString *tech, wxString *fin, bool *reset );


#endif

