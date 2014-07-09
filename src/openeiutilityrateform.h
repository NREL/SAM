#ifndef __OpenEIUtilityRateForm_h
#define __OpenEIUtilityRateForm_h

#include <wx/wx.h>
#include "openeiapi.h"

class AFLabel;
class wxHyperlinkCtrl;
class wxExtTextCtrl;

class OpenEIUtilityRateDialog : public wxDialog
{
public:
	OpenEIUtilityRateDialog(wxWindow *parent, const wxString &title, const wxString &market);


/*user.class.start*/

	void StartHttp();
	void QueryUtilities();
	void QueryRates(const wxString &utility_name);
	void OnEvent(wxCommandEvent &evt);
	void OnTimer(wxTimerEvent &evt);
	void UpdateRateList();
	void UpdateUtilityList();
	void UpdateRateData();

	void OnCommand( wxCommandEvent & );
	void OnClose( wxCloseEvent & );
	bool IsBusy();

	virtual int ShowModal();

	OpenEI::RateData GetCurrentRateData();
private:

	wxButton *btnApply;
	wxStaticText *lblStatus;
	wxHyperlinkCtrl *hypOpenEILink;
	wxTextCtrl *txtRateDescription;
	wxExtTextCtrl *txtRateEndDate;
	wxExtTextCtrl *txtRateStartDate;
	wxExtTextCtrl *txtRateName;
	wxListBox *lstRates;
	wxListBox *lstUtilities;
	wxButton *btnClose;
	wxExtTextCtrl *txtUtilitySearch;
	wxButton *btnQueryAgain;
	wxComboBox *cboResCom;

	wxArrayString mUtilityCompanies;
	std::vector<OpenEI::RateInfo> mUtilityRates;
	OpenEI::RateData mRateData;
	wxArrayString mGUIDList;
	wxTimer mTimer;
	bool mBusy;
	OpenEI api;
	
/*user.class.end*/
	DECLARE_EVENT_TABLE()
};

#endif

