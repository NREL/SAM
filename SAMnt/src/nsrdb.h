#ifndef __NSRDBDialog_h
#define __NSRDBDialog_h

#include <wx/dialog.h>


class wxComboBox;
class wxCheckListBox;
class wxButton;
class wxTextCtrl;
class wxCheckbox;
//class wxDirPickerCtrl;
//class wxFileDirPickerEvent;

class NSRDBDialog : public wxDialog
{

public:
	NSRDBDialog(wxWindow *parent, const wxString &title);
	wxString &GetWeatherFile() {
		return m_weatherFile;
	};
	wxString &GetWeatherFolder() {
		return m_weatherFolder;
	};
	wxString &GetAddFolder() {
		return m_addFolder;
	};

	struct LinkInfo
	{
		wxString name;
		wxString displayName;
		wxString type;
		wxString year; // number or "tmy"
		wxString URL;
		wxString interval;
		wxString location;
		wxString display;
		LinkInfo(wxString &_n, wxString &_dn, wxString &_t, wxString &_y, wxString &_u, wxString &_i, wxString &_l)
			: name(_n), displayName(_dn), type(_t), year(_y), URL(_u), interval(_i), location(_l)
		{
			display = location + "_" + name + "_" + type + "_" + interval + "_" + year;
		}
	};


private:
	void OnEvt(wxCommandEvent &);
//	void OnDir(wxFileDirPickerEvent &);

	void GetResources();

	std::vector<LinkInfo> m_links;
	wxString m_weatherFile;
	wxString m_weatherFolder;
	wxString m_addFolder;
	//	wxComboBox *m_cboFilter;
	wxComboBox *m_cboWeatherFile;
	wxCheckListBox *m_chlResources;
//	wxButton *m_btnChkAll, *m_btnChkFiltered, *m_btnChkNone, *m_btnResources, *m_btnFolder, *m_btnDownload;
	wxButton *m_btnChkAll, *m_btnChkNone, *m_btnResources, *m_btnFolder;
	wxTextCtrl *m_txtFolder;
	wxTextCtrl *m_txtAddress;
	wxCheckBox *m_chkFolder;
//	wxDirPickerCtrl *m_dirpicker;

	DECLARE_EVENT_TABLE()
};


#endif