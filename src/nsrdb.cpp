#include <wx/checklst.h>
#include <wx/combobox.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/sizer.h>
#include <wx/dirdlg.h>

#include <wex/easycurl.h>
#include <wex/jsonval.h>
#include <wex/jsonreader.h>

#include "NSRDB.h"
#include "main.h"


enum {
	ID_txtAddress, ID_txtFolder, ID_cboFilter, ID_cboWeatherFile, ID_chlResources,
	ID_btnChkAll, ID_btnChkFiltered, ID_btnChkNone, ID_btnResources, ID_btnFolder, ID_btnDownload
};

BEGIN_EVENT_TABLE( NSRDBDialog, wxDialog )
	EVT_BUTTON(ID_btnChkAll, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnChkFiltered, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnChkNone, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnResources, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnFolder, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnDownload, NSRDBDialog::OnEvt)
	EVT_BUTTON(wxID_HELP, NSRDBDialog::OnEvt)
END_EVENT_TABLE()

NSRDBDialog::NSRDBDialog(wxWindow *parent, const wxString &title)
	 : wxDialog( parent, wxID_ANY, title,  wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{
	wxString dnpath;
	SamApp::Settings().Read("NSRDBDownloadFolder", &dnpath);
	if (dnpath.Len() <=0)
		SamApp::Settings().Read("solar_download_path", &dnpath);
	m_txtFolder = new wxTextCtrl(this, ID_txtFolder, dnpath, wxDefaultPosition, wxSize(500, 30));
	m_txtAddress = new wxTextCtrl(this, ID_txtAddress, "", wxDefaultPosition, wxSize(500, 30));

	m_btnFolder = new wxButton(this, ID_btnFolder, "...", wxDefaultPosition, wxSize(30, 30));
	m_btnChkAll = new wxButton(this, ID_btnChkAll, "Select all");
	m_btnChkFiltered = new wxButton(this, ID_btnChkFiltered, "Select filtered");
	m_btnChkNone = new wxButton(this, ID_btnChkNone, "Unselect all");
	m_btnResources = new wxButton(this, ID_btnResources, "Update resources");
	m_btnDownload = new wxButton(this, ID_btnDownload, "Download");

	m_cboFilter = new wxComboBox(this, ID_cboFilter, ""); // populate with returned resources type and interval
	m_cboWeatherFile = new wxComboBox(this, ID_cboWeatherFile, ""); // populate with selected resources
	m_chlResources = new wxCheckListBox(this, ID_chlResources, wxDefaultPosition, wxSize(800,600)); // populate with returned resources

	wxString msg = "Enter your location as a street address or latitude, longitude:\nExamples:\n  15031 denver west parkway golden co\n   40.1,-109.3\nThe email address you used to register SAM will be sent to the NSRDB at NREL. If you do not want share your email address with the NSRDB, press Cancel now.";


	wxBoxSizer *szAddress = new wxBoxSizer(wxHORIZONTAL);
	szAddress->Add(new wxStaticText(this, wxID_ANY, "Address"), 0, wxALL , 1);
	szAddress->Add(m_txtAddress, 0, wxALL|wxEXPAND, 5);
	szAddress->Add(m_btnResources, 0, wxALL, 1);

	wxBoxSizer *szFolder = new wxBoxSizer(wxHORIZONTAL);
	szFolder->Add(new wxStaticText(this, wxID_ANY, "Download Folder"), 0, wxALL , 1);
	szFolder->Add(m_txtFolder, 0, wxALL | wxEXPAND, 5);
	szFolder->Add(m_btnFolder, 0, wxALL, 1);
	szFolder->Add(m_btnDownload, 0, wxALL, 1);

	wxBoxSizer *szWeatherFile = new wxBoxSizer(wxHORIZONTAL);
	szWeatherFile->Add(new wxStaticText(this, wxID_ANY, "Weather file to use in SAM"), 0, wxALL , 1);
	szWeatherFile->Add(m_cboWeatherFile, 0, wxALL | wxEXPAND, 5);

	wxBoxSizer *szFilter = new wxBoxSizer(wxHORIZONTAL);
	szFilter->Add(new wxStaticText(this, wxID_ANY, "Filter list"), 0, wxALL , 1);
	szFilter->Add(m_cboFilter, 0, wxALL | wxEXPAND, 5);


	wxBoxSizer *szChkBtn = new wxBoxSizer(wxHORIZONTAL);
	szChkBtn->Add(m_btnChkFiltered, 0, wxALL, 2);
	szChkBtn->Add(m_btnChkAll, 0, wxALL, 2);
	szChkBtn->Add(m_btnChkNone, 0, wxALL, 2);


	wxBoxSizer *szgrid = new wxBoxSizer(wxVERTICAL);
	szgrid->Add(szChkBtn, 0, wxALL , 1);
	szgrid->Add( m_chlResources, 1, wxALL|wxEXPAND, 10 );


	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add(new wxStaticText(this, wxID_ANY, msg), 0, wxALL | wxEXPAND, 5);
	szmain->Add(szAddress, 0, wxALL | wxEXPAND, 1);
	szmain->Add(szFilter, 0, wxALL | wxEXPAND, 1);
	szmain->Add(szgrid, 0, wxALL | wxEXPAND, 10);
	szmain->Add(szFolder, 0, wxALL | wxEXPAND, 1);
	szmain->Add(szWeatherFile, 0, wxALL | wxEXPAND, 1);
	szmain->Add( CreateButtonSizer( wxHELP|wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 2 );

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
		case ID_btnChkAll:
			{
				for (int i = 0; i < m_chlResources->GetCount(); i++)
					m_chlResources->Check(i, true);
			}
			break;
		case ID_btnChkNone:
			{
				for (int i = 0; i < m_chlResources->GetCount(); i++)
					m_chlResources->Check(i,false);
			}
			break;
		case ID_btnFolder:
			{
				wxDirDialog dlg(SamApp::Window(), "Choose Download Folder", m_txtFolder->GetValue(), wxDD_DEFAULT_STYLE | wxDD_DIR_MUST_EXIST);
				if (dlg.ShowModal() == wxID_OK)
				{
					m_txtFolder->SetValue( dlg.GetPath());
					SamApp::Settings().Write("NSRDBDownloadFolder", dlg.GetPath());
				}
			}
			break;
		case ID_btnDownload:
			{
				wxEasyCurl curl;
				for (int i = 0; i < m_chlResources->GetCount(); i++)
				{
					if (m_chlResources->IsChecked(i))
					{
						wxString url = m_links[i].URL;
						wxString curstr = m_chlResources->GetString(i);
						//Download the weather file
						bool ok = curl.Get(url, "Downloading " + curstr + " from NSRDB...", SamApp::Window());
						if (!ok)
						{
							wxMessageBox("Failed to download data from web service.");
							break;
						}
						else
						{
							wxString fn = curstr + ".csv";
							fn = m_txtFolder->GetValue() + "/" + fn;
							if (!curl.WriteDataToFile(fn))
							{
								wxMessageBox("Failed to download " + fn + " from web service.");
								continue;
							}
						}
					}
				}
			}
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
	bool ok = curl.Get(url, "Downloading data from NSRDB...", SamApp::Window());

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

	m_chlResources->Clear();
	m_links.clear();
	wxJSONValue output_list = root["outputs"];
	for (int i_outputs = 0; i_outputs<output_list.Size(); i_outputs++)
	{
		wxJSONValue out_item = output_list[i_outputs];
		wxJSONValue links_list = output_list[i_outputs]["links"];
		for (int i_links = 0; i_links < links_list.Size(); i_links++)
		{
			wxString name = output_list[i_outputs]["name"].AsString();
			wxString displayName = output_list[i_outputs]["displayName"].AsString();
			wxString type = output_list[i_outputs]["type"].AsString();

			wxString year = links_list[i_links]["year"].AsString();
			wxString URL = links_list[i_links]["link"].AsString();
			URL.Replace("yourapikey", "<SAMAPIKEY>");
			URL.Replace("youremail", "<USEREMAIL>");
			wxString interval = links_list[i_links]["interval"].AsString();
//			wxLogStatus("link info: %s, %s, %s, %s, %d, %s", x.displayName.c_str(), x.name.c_str(), x.type.c_str(), x.year.c_str(), x.interval, x.URL.c_str());
			m_links.push_back(LinkInfo(name, displayName, type, year, URL, interval, locname));
		}
	}

	m_chlResources->Freeze();
	for (size_t i = 0; i < m_links.size(); i++)
	{
		m_chlResources->Insert(m_links[i].display, i);
	}
	m_chlResources->Thaw();

}