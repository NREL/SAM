#ifndef __WindToolkitDialog_h
#define __WindToolkitDialog_h

#include <wx/dialog.h>


class wxComboBox;
class wxRadioButton;
class wxTextCtrl;

class WindToolkitDialog : public wxDialog
{

public:
	WindToolkitDialog(wxWindow *parent, const wxString &title);

	bool IsAddressMode();
	wxString GetAddress();
	double GetLatitude();
	double GetLongitude();
	wxString GetYear();

private:
	void OnEvt(wxCommandEvent &);

	wxComboBox *cboYears;
	wxRadioButton *radAddress, *radLatLon;
	wxTextCtrl *txtAddress, *txtLat, *txtLon;

	DECLARE_EVENT_TABLE()
};


#endif