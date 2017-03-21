#ifndef __NSRDBDialog_h
#define __NSRDBDialog_h

#include <wx/dialog.h>


class wxComboBox;
class wxCheckListBox;
class wxButton;
class wxTextCtrl;

class NSRDBDialog : public wxDialog
{

public:
	NSRDBDialog(wxWindow *parent, const wxString &title);
	wxString &GetWeatherFile() {
		return m_weatherFile;
	};

	struct LinkInfo
	{
		wxString name;
		wxString displayName;
		wxString type;
		wxString year; // number or "tmy"
		wxString URL;
		int interval;
	};


private:
	void OnEvt(wxCommandEvent &);

	void GetResources();

	std::vector<LinkInfo> m_links;
	wxString m_weatherFile;
	wxComboBox *m_cboFilter;
	wxComboBox *m_cboWeatherFile;
	wxCheckListBox *m_chlResources;
	wxButton *m_btnChkAll, *m_btnChkFiltered, *m_btnChkNone, *m_btnResources, *m_btnFolder;
	wxTextCtrl *m_txtFolder;
	wxTextCtrl *m_txtAddress;

	DECLARE_EVENT_TABLE()
};


#endif