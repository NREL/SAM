//#include <wx/wx.h>

#include <wx/busyinfo.h>
#include <wx/grid.h>
#include <wx/hyperlink.h>

#include <wex/metro.h>
#include <wex/exttext.h>
//#include "afeditctrls.h"
//#include "schedctrl.h"
//#include "wfgridctrl.h"


#include "OpenEIUtilityRateForm.h"
#include "openeiapi.h"


enum {
  ID_btnApply,
  ID_lblStatus,
  ID_hypOpenEILink,
  ID_txtRateDescription,
  ID_txtRateEndDate,
  ID_txtRateStartDate,
  ID_txtRateName,
  ID_lstRates,
  ID_lstUtilities,
  ID_btnClose,
  ID_txtUtilitySearch,
  ID_btnQueryAgain,
  ID_cboResCom };

BEGIN_EVENT_TABLE( OpenEIUtilityRateDialog, wxDialog )
	EVT_TIMER( wxID_ANY, OpenEIUtilityRateDialog::OnTimer )
	EVT_BUTTON( ID_btnQueryAgain, OpenEIUtilityRateDialog::OnEvent )
	EVT_COMBOBOX( ID_cboResCom, OpenEIUtilityRateDialog::OnEvent )
	EVT_LISTBOX( ID_lstUtilities, OpenEIUtilityRateDialog::OnEvent )
	EVT_LISTBOX( ID_lstRates, OpenEIUtilityRateDialog::OnEvent )
	EVT_TEXT( ID_txtUtilitySearch, OpenEIUtilityRateDialog::OnEvent )
	EVT_BUTTON( ID_btnApply, OpenEIUtilityRateDialog::OnCommand)
	EVT_BUTTON( ID_btnClose, OpenEIUtilityRateDialog::OnCommand)
	EVT_CLOSE( OpenEIUtilityRateDialog::OnClose )
END_EVENT_TABLE()

OpenEIUtilityRateDialog::OpenEIUtilityRateDialog(wxWindow *parent, const wxString &title, const wxString &market)
	 : wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxSize(800,600), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
{
	wxArrayString _data_cboResCom;
	_data_cboResCom.Add("All Schedules");
	_data_cboResCom.Add("Residential Only");
	_data_cboResCom.Add("Commercial Only");
	_data_cboResCom.Add("Lighting Only");
	cboResCom = new wxComboBox(this, ID_cboResCom, "All Schedules", wxPoint(513,6), wxSize(127,21), _data_cboResCom, wxCB_READONLY);

	int cbo_ndx=0;
	for (int i=0; i<_data_cboResCom.Count(); i++)
		if (_data_cboResCom[i].First(market) != wxNOT_FOUND)
		{
			cbo_ndx = i;
			break;
		}
	cboResCom->SetSelection(cbo_ndx);


	btnQueryAgain = new wxButton(this, ID_btnQueryAgain, "Refresh", wxPoint(210,6), wxSize(65,21));

	txtUtilitySearch = new wxExtTextCtrl(this, ID_txtUtilitySearch, "", wxPoint(120,6), wxSize(88,21));
	txtUtilitySearch->SetForegroundColour( wxColour(0, 0, 0) );
	txtUtilitySearch->SetBackgroundColour( wxColour(255, 255, 255) );


	wxArrayString _data_lstUtilities;
	lstUtilities = new wxListBox(this, ID_lstUtilities, wxPoint(9,30), wxSize(266,450), _data_lstUtilities, wxLB_SINGLE|wxLB_HSCROLL);

	wxArrayString _data_lstRates;
	lstRates = new wxListBox(this, ID_lstRates, wxPoint(288,30), wxSize(353,144), _data_lstRates, wxLB_SINGLE|wxLB_HSCROLL);

	txtRateName = new wxExtTextCtrl(this, ID_txtRateName,"", wxPoint(375,201), wxSize(253,21));
	txtRateName->SetEditable( false );
	txtRateName->SetForegroundColour( wxColour(0, 0, 0) );
	txtRateName->SetBackgroundColour( wxColour(255, 255, 255) );

	txtRateStartDate = new wxExtTextCtrl(this, ID_txtRateStartDate, "", wxPoint(375,399), wxSize(253,21));
	txtRateStartDate->SetEditable( false );
	txtRateStartDate->SetForegroundColour( wxColour(0, 128, 192) );
	txtRateStartDate->SetBackgroundColour( wxColour(255, 255, 255) );

	txtRateEndDate = new wxExtTextCtrl(this, ID_txtRateEndDate, "", wxPoint(375,423), wxSize(253,21));
	txtRateEndDate->SetEditable( false );
	txtRateEndDate->SetForegroundColour( wxColour(0, 128, 192) );
	txtRateEndDate->SetBackgroundColour( wxColour(255, 255, 255) );

	txtRateDescription = new wxTextCtrl(this, ID_txtRateDescription, "", wxPoint(375,225), wxSize(252,168),wxTE_MULTILINE|wxTE_DONTWRAP|wxTE_PROCESS_TAB);
	txtRateDescription->SetFont(wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "courier"));
	txtRateDescription->ChangeValue("");
	txtRateDescription->SetEditable( false );

	hypOpenEILink = new wxHyperlinkCtrl(this, ID_hypOpenEILink, "Go to rate page on OpenEI.org...", "http://en.openei.org/wiki/Gateway:Utilities", wxPoint(294,450), wxSize(281,21));
	
	lblStatus = new wxStaticText(this, ID_lblStatus, "", wxPoint(9,486), wxSize(302,21));
	
	btnApply = new wxButton(this, ID_btnApply, "Download and apply utility rate", wxPoint(318,486), wxSize(236,21));
	btnClose = new wxButton(this, ID_btnClose, "Close", wxPoint(558,486), wxSize(80,21));
	
	wxBoxSizer *sz_left_top = new wxBoxSizer( wxHORIZONTAL );
	sz_left_top->Add( new wxStaticText( this, wxID_ANY, "  Search:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
	sz_left_top->Add( txtUtilitySearch, 1, wxALL|wxEXPAND, 4 );
	sz_left_top->Add( btnQueryAgain, 0, wxALL|wxEXPAND, 4 );

	wxBoxSizer *sz_left = new wxBoxSizer( wxVERTICAL );
	sz_left->Add( sz_left_top, 0, wxALL|wxEXPAND, 0 );
	sz_left->Add( lstUtilities, 1, wxALL|wxEXPAND, 0 );


	wxBoxSizer *sz_right_top = new wxBoxSizer( wxHORIZONTAL );
	sz_right_top->Add( new wxStaticText(this, wxID_ANY, "Available rate schedules"), 1, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_right_top->Add( cboResCom, 0, wxALL|wxEXPAND, 3 );

	wxFlexGridSizer *sz_right_grid = new wxFlexGridSizer(2);
	sz_right_grid->AddGrowableCol(1);
	sz_right_grid->Add( new wxStaticText(this, wxID_ANY, "Name"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_right_grid->Add( txtRateName, 1, wxALL|wxEXPAND, 2 );	
	sz_right_grid->Add( new wxStaticText(this, wxID_ANY, "Description"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_right_grid->Add( txtRateDescription, 1, wxALL|wxEXPAND, 2 );	
	sz_right_grid->Add( new wxStaticText(this, wxID_ANY, "Start"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_right_grid->Add( txtRateStartDate, 1, wxALL|wxEXPAND, 2 );	
	sz_right_grid->Add( new wxStaticText(this, wxID_ANY, "End"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	sz_right_grid->Add( txtRateEndDate, 1, wxALL|wxEXPAND, 2 );
	
	wxBoxSizer *sz_right = new wxBoxSizer(wxVERTICAL);
	sz_right->Add( sz_right_top, 0, wxALL|wxEXPAND );
	sz_right->Add( lstRates, 1, wxALL|wxEXPAND );
	sz_right->Add( sz_right_grid, 2, wxALL|wxEXPAND );
	sz_right->Add( hypOpenEILink, 0, wxALL|wxEXPAND );


	wxBoxSizer *sz_main = new wxBoxSizer(wxHORIZONTAL ); 
	sz_main->Add( sz_left, 2, wxALL|wxEXPAND, 4 );
	sz_main->Add( sz_right, 3, wxALL|wxEXPAND, 4 );
	
	wxBoxSizer *sz_bottom = new wxBoxSizer(wxHORIZONTAL );
	sz_bottom->Add( lblStatus, 1, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
	sz_bottom->Add( btnApply, 0, wxALL|wxEXPAND, 4 );
	sz_bottom->Add( btnClose, 0, wxALL|wxEXPAND, 4 );

	wxBoxSizer *sz_top = new wxBoxSizer(wxVERTICAL);
	sz_top->Add( sz_main, 1, wxALL|wxEXPAND, 4 );
	sz_top->Add( sz_bottom, 0, wxALL|wxEXPAND, 4 );

	SetSizer( sz_top );
	
	//lblStatus->Hide();
	mTimer.SetOwner( this );
	mBusy = false;
}

void OpenEIUtilityRateDialog::StartHttp()
{
	lblStatus->SetLabel("Connecting to OpenEI...");
	lblStatus->Show();
	mTimer.Start( 300, true );
}

void OpenEIUtilityRateDialog::QueryUtilities()
{
	lblStatus->SetLabel("Loading utility companies...");
	wxString err;
	wxBusyInfo busy("Communicating with OpenEI.org... please wait", this);
	if (!api.QueryUtilityCompanies(mUtilityCompanies, &err))
	{
		wxMessageBox("Error:\n\n" + err);
		return;
	}

	txtUtilitySearch->SetValue(wxEmptyString);
	UpdateUtilityList();
	lblStatus->SetLabel("Ready.");
	txtUtilitySearch->SetFocus();
}

int OpenEIUtilityRateDialog::ShowModal()
{
	StartHttp();
	return wxDialog::ShowModal();
}

void OpenEIUtilityRateDialog::QueryRates(const wxString &utility_name)
{
	lblStatus->SetLabel("Loading rates for " + utility_name + "...");
	wxString err;
	//wxBusyInfo busy("Communicating with OpenEI.org... please wait", this);
	if (!api.QueryUtilityRates( utility_name, mUtilityRates, &err ))
	{
		wxMessageBox("Error:\n\n" + err);
		return;
	}

	if (mUtilityRates.size() == 0)
		lblStatus->SetLabel("No rates for " + utility_name);
	else
		lblStatus->SetLabel("Ready.");

	UpdateRateList();
}

void OpenEIUtilityRateDialog::UpdateUtilityList()
{
	
	lstUtilities->Freeze();
	lstUtilities->Clear();

	wxString filter = txtUtilitySearch->GetValue().Lower();
	if (filter.IsEmpty())
	{
		lstUtilities->Append( mUtilityCompanies );
	}
	else if (filter.Len() <= 2)
	{
		for (int i=0;i<mUtilityCompanies.Count();i++)
		{
			if (mUtilityCompanies[i].Left( filter.Len() ).Lower() == filter)
				lstUtilities->Append( mUtilityCompanies[i] );
		}
	}
	else
	{
		for (int i=0;i<mUtilityCompanies.Count();i++)
		{
			if (mUtilityCompanies[i].Lower().Find(filter) >= 0)
				lstUtilities->Append( mUtilityCompanies[i] );
		}
	}

	lstUtilities->Thaw();
}

void OpenEIUtilityRateDialog::UpdateRateList()
{
	lstRates->Freeze();
	lstRates->Clear();

	mGUIDList.Clear();
	for (int i=0;i<mUtilityRates.size();i++)
	{
		if (cboResCom->GetSelection() == 1 && mUtilityRates[i].Sector.Lower() != "residential")
			continue;
	
		if (cboResCom->GetSelection() == 2 && mUtilityRates[i].Sector.Lower() != "commercial")
			continue;

		if (cboResCom->GetSelection() == 3 && mUtilityRates[i].Sector.Lower() != "lighting")
			continue;

		wxString rate = mUtilityRates[i].Sector + "-" + mUtilityRates[i].Name;
		lstRates->Append( rate );
		mGUIDList.Add( mUtilityRates[i].GUID );
	}

	lstRates->Thaw();
	UpdateRateData();
}

OpenEI::RateData OpenEIUtilityRateDialog::GetCurrentRateData()
{
	return mRateData;
}

void OpenEIUtilityRateDialog::UpdateRateData()
{
	int idx = lstRates->GetSelection();
	wxString guid;
	if (idx >= 0 && idx < mGUIDList.Count())
		guid = mGUIDList[idx];

	wxString ssel = lstRates->GetStringSelection();

	if (guid.IsEmpty())
	{
		txtRateName->SetValue(wxEmptyString);
		txtRateDescription->SetValue(wxEmptyString);
		txtRateStartDate->SetValue(wxEmptyString);
		txtRateEndDate->SetValue(wxEmptyString);
//		hypOpenEILink->SetURL("http://en.openei.org/wiki/Gateway:Utilities");
		hypOpenEILink->SetURL("http://en.openei.org/wiki/Utility_Rate_Database");
	}
	else
	{
		mRateData.Reset();
	
		lblStatus->SetLabel("Retrieving rate data for " + ssel + "...");
		wxString json_url;
		wxBusyInfo busy("Communicating with OpenEI.org... please wait", this);
		if (api.RetrieveUtilityRateData(guid, mRateData, &json_url))
		{
			
			txtRateName->SetValue( mRateData.Header.Utility + ": " + mRateData.Header.Name );
			txtRateStartDate->SetValue( mRateData.StartDate );
			txtRateEndDate->SetValue( mRateData.EndDate );
			
			wxString desc = mRateData.Header.Description + "\n\n";

			desc += wxString::Format("Has Flat Rate? %s\n", mRateData.HasFlatRate?"yes":"no");
			desc += wxString::Format("Has Energy Charges? %s\n", mRateData.HasEnergyCharge?"yes":"no");
			desc += wxString::Format("Has Demand Charges? %s\n", mRateData.HasDemandCharge?"yes":"no");
			desc += wxString::Format("\nGUID: '%s'\n", mRateData.Header.GUID.c_str() );
			desc += wxString::Format("\nEnergy comments: '%s'\n", mRateData.Header.EnergyComments.c_str());
			desc += wxString::Format("\nDeamand comments: '%s'\n", mRateData.Header.DemandComments.c_str());

			txtRateDescription->SetValue( desc );
			
//			hypOpenEILink->SetURL("http://en.openei.org/wiki/Data:" + guid);
			hypOpenEILink->SetURL("http://dev.openei.org/apps/USURDB/rate/view/" + guid);

			lblStatus->SetLabel("Ready.");
		}
		else
			lblStatus->SetLabel("Could not get rate data for " + ssel );

	}
}

void OpenEIUtilityRateDialog::OnTimer(wxTimerEvent &evt)
{
	mBusy = true;
	QueryUtilities();
	mBusy = false;
}

void OpenEIUtilityRateDialog::OnEvent(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_cboResCom:
		UpdateRateList();
		break;
	case ID_lstUtilities:
		QueryRates( lstUtilities->GetStringSelection() );
		break;
	case ID_lstRates:
		UpdateRateData();
		break;
	case ID_btnQueryAgain:
		QueryUtilities();
		break;
	case ID_txtUtilitySearch:
		UpdateUtilityList();
		break;
	}
}

bool OpenEIUtilityRateDialog::IsBusy()
{
	return mBusy;
}

void OpenEIUtilityRateDialog::OnCommand( wxCommandEvent &evt )
{
	if (evt.GetId() == ID_btnClose)
	{
		if (IsBusy())
		{
			wxMessageBox("Busy processing information, please wait...");
			return;
		}

		EndModal(wxID_CANCEL);
	}
	else
	{
		OpenEI::RateData dat = GetCurrentRateData();
		if (dat.Header.GUID.IsEmpty())
		{
			wxMessageBox("No rate data selected.");
			return;
		}

		EndModal(wxID_OK);
	}
}

void OpenEIUtilityRateDialog::OnClose(wxCloseEvent &evt)
{
	if (IsBusy())
	{
		wxMessageBox("Busy processing information, please wait...");
		return;
	}

	EndModal(wxID_CANCEL);
}
