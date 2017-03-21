#include <wx/checklst.h>
#include <wx/combobox.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/sizer.h>

#include <wex/easycurl.h>
#include <wex/jsonval.h>
#include <wex/jsonreader.h>

#include "NSRDB.h"
#include "main.h"


enum {
	ID_txtAddress, ID_txtFolder, ID_cboFilter, ID_cboWeatherFile, ID_chlResources,
	ID_btnChkAll, ID_btnChkFiltered, ID_btnChkNone, ID_btnResources, ID_btnFolder
};

BEGIN_EVENT_TABLE( NSRDBDialog, wxDialog )
	EVT_BUTTON(ID_btnChkAll, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnChkFiltered, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnChkNone, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnResources, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnFolder, NSRDBDialog::OnEvt)
	EVT_BUTTON(wxID_HELP, NSRDBDialog::OnEvt)
END_EVENT_TABLE()

NSRDBDialog::NSRDBDialog(wxWindow *parent, const wxString &title)
	 : wxDialog( parent, wxID_ANY, title,  wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{

	m_txtFolder = new wxTextCtrl(this, ID_txtFolder, "");
	m_txtAddress = new wxTextCtrl(this, ID_txtAddress, "");

	m_btnFolder = new wxButton(this, ID_btnFolder, "...");
	m_btnChkAll = new wxButton(this, ID_btnChkAll, "Select all");
	m_btnChkFiltered = new wxButton(this, ID_btnChkFiltered, "Select filtered");
	m_btnChkNone = new wxButton(this, ID_btnChkNone, "Unselect all");
	m_btnResources = new wxButton(this, ID_btnResources, "Update resources");

	m_cboFilter = new wxComboBox(this, ID_cboFilter, ""); // populate with returned resources type and interval
	m_cboWeatherFile = new wxComboBox(this, ID_cboWeatherFile, ""); // populate with selected resources
	m_chlResources = new wxCheckListBox(this, ID_chlResources); // populate with returned resources

	wxString msg = "Enter your location as a street address or latitude, longitude:\n\n	Examples:\n  15031 denver west parkway golden co\n   40.1,-109.3\n\n	The email address you used to register SAM will be sent to the NSRDB at NREL.\n	If you do not want share your email address with the NSRDB, press Cancel now.";


	wxBoxSizer *szAddress = new wxBoxSizer(wxHORIZONTAL);
	szAddress->Add(new wxStaticText(this, wxID_ANY, "Address"), 0, wxALL , 5);
	szAddress->Add(m_txtAddress, 0, wxALL, 5);
	szAddress->Add(m_btnResources, 0, wxALL, 5);

	wxBoxSizer *szFolder = new wxBoxSizer(wxHORIZONTAL);
	szFolder->Add(new wxStaticText(this, wxID_ANY, "Download Folder"), 0, wxALL , 5);
	szFolder->Add(m_txtFolder, 0, wxALL, 5);
	szFolder->Add(m_btnFolder, 0, wxALL, 5);

	wxBoxSizer *szWeatherFile = new wxBoxSizer(wxHORIZONTAL);
	szWeatherFile->Add(new wxStaticText(this, wxID_ANY, "Weather file to use in SAM"), 0, wxALL , 5);
	szWeatherFile->Add(m_cboWeatherFile, 0, wxALL, 5);

	wxBoxSizer *szFilter = new wxBoxSizer(wxHORIZONTAL);
	szFilter->Add(new wxStaticText(this, wxID_ANY, "Filter list"), 0, wxALL , 5);
	szFilter->Add(m_cboFilter, 0, wxALL, 5);


	wxBoxSizer *szChkBtn = new wxBoxSizer(wxHORIZONTAL);
	szChkBtn->Add(m_btnChkFiltered, 0, wxALL, 2);
	szChkBtn->Add(m_btnChkAll, 0, wxALL, 2);
	szChkBtn->Add(m_btnChkNone, 0, wxALL, 2);


	wxBoxSizer *szgrid = new wxBoxSizer(wxVERTICAL);
	szgrid->Add(szChkBtn, 0, wxALL , 1);
	szgrid->Add( m_chlResources, 1, wxALL|wxEXPAND, 10 );


	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add(new wxStaticText(this, wxID_ANY, msg), 0, wxALL , 5);
	szmain->Add(szAddress, 0, wxLEFT | wxRIGHT | wxTOP, 10);
	szmain->Add(szFilter, 0, wxLEFT | wxRIGHT | wxTOP, 10);
	szmain->Add(szgrid, 0, wxLEFT | wxRIGHT | wxTOP, 10);
	szmain->Add(szFolder, 0, wxLEFT | wxRIGHT | wxTOP, 10);
	szmain->Add(szWeatherFile, 0, wxLEFT | wxRIGHT, 10);


	szmain->Add( CreateButtonSizer( wxHELP|wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );

	SetSizer( szmain );
	Fit();

	m_txtAddress->SetFocus();
}

void NSRDBDialog::OnEvt( wxCommandEvent &e )
{
//extern void helpcontext( const wxString & ); // defined in sammdi.h

	switch( e.GetId() )
	{
	case wxID_HELP:
		SamApp::ShowHelp("download_weather_file");
		break;
	case ID_btnResources:
		GetResources();
		break;
	}
}


void NSRDBDialog::GetResources()
{
	// hit api with address and return available resources
	wxString loc = m_txtAddress->GetValue();
	if (loc == "") return;

	wxString locname = "";

	bool is_addr = false;
	const wxChar* locChars = loc.c_str();
	for (int i = 0; i < loc.Len(); i++) 
	{
		if (isalpha(locChars[i]))
			is_addr = true;
	}
	double lat, lon;
	if (is_addr)	//entered an address instead of a lat/long
	{
		if (!wxEasyCurl::GeoCode(loc, &lat, &lon))
		{
			wxMessageBox("Failed to geocode address");
			return;
		}
		else
			locname = wxString::Format("lat%.5lf_lon%.5lf", lat, lon);
	}
	else
	{
		wxArrayString parts = wxSplit(loc, ',');
		if (parts.Count() < 2)
		{
			wxMessageBox("Incorrectly formatted latitude, longitude.");
			return ;
		}

		if (!parts[0].ToDouble(&lat) || !parts[1].ToDouble(&lon))
			locname = wxString::Format("lat%.5lf_lon%.5lf", lat, lon);
	}

	//Create URL for weather file download
	wxString url;
	url = SamApp::WebApi("nsrdb_list_all");
	url.Replace("<LAT>", wxString::Format("%lg", lat), 1);
	url.Replace("<LON>", wxString::Format("%lg", lon), 1);

	//Download the weather file
	wxEasyCurl curl;
	bool ok = curl.Get(url, "Downloading data from wind toolkit...", SamApp::Window());	//true won't let it return to code unless it's done downloading
	// would like to put some code here to tell it not to download and to give an error if hits 404 Not Found

	if (!ok)
	{
		wxMessageBox("Failed to download data from web service.");
		return;
	}

//	wxMessageBox(curl.GetDataAsString());
	wxString json_data = curl.GetDataAsString();
	if (json_data.IsEmpty())
	{
		wxMessageBox("Failed to download data from web service.");
		return;
	}

	wxJSONReader reader;
	//	reader.SetSkipStringDoubleQuotes(true);
	wxJSONValue root;
	if (reader.Parse(json_data, &root) != 0)
	{
		wxMessageBox("Could not process returned JSON for " + locname);
		return;
	}


	m_links.clear();
	wxJSONValue output_list = root["outputs"];
	for (int i_outputs = 0; i_outputs<output_list.Size(); i_outputs++)
	{
		wxJSONValue out_item = output_list[i_outputs];
		wxJSONValue links_list = out_item["links"];
		for (int i_links = 0; i_links < links_list.Size(); i_links++)
		{
			LinkInfo x;
			x.name = json_string(output_list[i_outputs].Item("name"));
			x.displayName = json_string(output_list[i_outputs].Item("displayName"));
			x.type = json_string(output_list[i_outputs].Item("type"));

			x.year = json_string(links_list[i_links].Item("year"));
			x.URL = json_string(links_list[i_links].Item("link"));
			x.interval = json_integer(links_list[i_links].Item("interval"));
			//		wxLogStatus("urdb startdate=" + x.StartDate);
			//		wxLogStatus("urdb enddate=" + x.EndDate);
			wxLogStatus("link info: " + x.displayName + "," + x.name + "," + x.type + "," + x.year + "," + wxString::Format("%d",x.interval) + "," + x.URL);
			m_links.push_back(x);
		}
		wxMessageBox(wxString::Format("processed %d links", links_list.Size()));
	}
	
	wxMessageBox(wxString::Format("processed %d outputs", output_list.Size()));

	wxMessageBox(wxString::Format("processed %d structures", (int)m_links.size()));

}