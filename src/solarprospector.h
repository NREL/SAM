#ifndef __SolarProspectorDialog_h
#define __SolarProspectorDialog_h

#include <wx/dialog.h>


class wxComboBox;
class wxRadioButton;
class wxTextCtrl;

class SolarProspectorDialog : public wxDialog
{

public:
	//enum { SOLAR_PROSPECTOR, WIND_MAPSDB };

	SolarProspectorDialog(wxWindow *parent, const wxString &title);

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