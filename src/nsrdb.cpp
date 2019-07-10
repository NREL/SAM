/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include<algorithm>

#include <wx/checklst.h>
#include <wx/combobox.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/sizer.h>
#include <wx/dirdlg.h>
#include <wx/progdlg.h>
#include <wx/checkbox.h>
#include <wx/tokenzr.h>
#include <wx/srchctrl.h>

#include <wex/easycurl.h>
#include <wex/jsonval.h>
#include <wex/jsonreader.h>

#include "nsrdb.h"
#include "main.h"


enum {
	ID_txtAddress, ID_txtFolder, ID_cboFilter, ID_cboWeatherFile, ID_chlResources,
	ID_btnChkAll, ID_btnSelectFiltered, ID_btnUnselectFiltered, ID_btnChkPsm30, ID_btnChkPsm60, ID_btnChkNone, ID_btnResources, ID_btnFolder, ID_search
};

BEGIN_EVENT_TABLE( NSRDBDialog, wxDialog )
	EVT_BUTTON(ID_btnChkAll, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnSelectFiltered, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnUnselectFiltered, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnChkPsm30, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnChkPsm60, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnChkNone, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnResources, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnFolder, NSRDBDialog::OnEvt)
	EVT_TEXT(ID_search, NSRDBDialog::OnEvt)
	EVT_BUTTON(wxID_OK, NSRDBDialog::OnEvt)
	EVT_CHECKLISTBOX(ID_chlResources, NSRDBDialog::OnEvt)
	EVT_BUTTON(wxID_HELP, NSRDBDialog::OnEvt)
END_EVENT_TABLE()

NSRDBDialog::NSRDBDialog(wxWindow *parent, const wxString &title)
	 : wxDialog( parent, wxID_ANY, title,  wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{
	wxString dnpath;
	SamApp::Settings().Read("NSRDBDownloadFolder", &dnpath);
	if (dnpath.Len() <=0)
		SamApp::Settings().Read("solar_download_path", &dnpath);
	m_txtFolder = new wxTextCtrl(this, ID_txtFolder, dnpath);// , wxDefaultPosition, wxSize(500, 30));
	m_txtFolder->SetValue(dnpath);

	m_txtAddress = new wxTextCtrl(this, ID_txtAddress, "40.1,-109.3");// , wxDefaultPosition, wxSize(500, 30));

	m_btnFolder = new wxButton(this, ID_btnFolder, "...", wxDefaultPosition, wxSize(30, 30));
	m_btnChkAll = new wxButton(this, ID_btnChkAll, "Select all");
	m_btnChkPsm30 = new wxButton(this, ID_btnChkPsm30, "Select PSM V3 30-minute"); //cpg
	m_btnChkPsm60 = new wxButton(this, ID_btnChkPsm60, "Select PSM V3 hourly"); //cpg
	m_btnChkNone = new wxButton(this, ID_btnChkNone, "Clear all");
	m_btnUnselectFiltered = new wxButton(this, ID_btnUnselectFiltered, "Clear filtered");
	m_btnSelectFiltered = new wxButton(this, ID_btnSelectFiltered, "Select filtered");
	m_btnResources = new wxButton(this, ID_btnResources, "Find");
	m_search = new wxSearchCtrl(this, ID_search, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER | wxTE_PROCESS_TAB);
	m_cboWeatherFile = new wxComboBox(this, ID_cboWeatherFile, ""); // populate with selected resources
	m_chlResources = new wxCheckListBox(this, ID_chlResources, wxDefaultPosition, wxSize(800,200)); // populate with returned resources

	wxString msg = "Use this window to list all weather files available from the NSRDB for a given location, and choose files to download and add to your solar resource library.\nType an address or latitude and longtitude, for example, \"15031 denver west parkway golden co\" or \"40.1,-109.3\", and click Find to list available files.\nWhen the list appears, choose the file or files you want to download. PSM V3 files are the most up-to-date.\n\nThe email address you used to register SAM will be sent to the NREL NSRDB. If you do not want share your email address with the NSRDB, click Cancel now.";

	wxBoxSizer *szAddress = new wxBoxSizer(wxHORIZONTAL);
	szAddress->Add(new wxStaticText(this, wxID_ANY, "1. Find location:"), 0, wxALL , 2);
	szAddress->Add(m_txtAddress, 5, wxALL|wxEXPAND, 2);
	szAddress->Add(m_btnResources, 0, wxALL, 2);

	wxBoxSizer *szFolder = new wxBoxSizer(wxHORIZONTAL);
	szFolder->Add(new wxStaticText(this, wxID_ANY, "3. Choose download folder:"), 0, wxALL, 2);
	szFolder->Add(m_txtFolder, 5, wxALL | wxEXPAND, 2);
	szFolder->Add(m_btnFolder, 0, wxALL, 2);

	wxBoxSizer *szWeatherFile = new wxBoxSizer(wxHORIZONTAL);
	szWeatherFile->Add(new wxStaticText(this, wxID_ANY, "4. Choose file for simulation (optional):"), 0, wxALL , 2);
	szWeatherFile->Add(m_cboWeatherFile, 5, wxALL | wxEXPAND, 2);

	wxBoxSizer *szChkBtn = new wxBoxSizer(wxHORIZONTAL);
	szChkBtn->Add(m_search, 0, wxALIGN_CENTER_VERTICAL | wxALL, 2);
	szChkBtn->Add(m_btnSelectFiltered, 0, wxALL, 2);
	szChkBtn->Add(m_btnUnselectFiltered, 0, wxALL, 2);
	szChkBtn->Add(m_btnChkPsm60, 0, wxALL, 2); //cpg was commented out see line above
	szChkBtn->Add(m_btnChkPsm30, 0, wxALL, 2); //cpg was commented out see line above
	szChkBtn->Add(m_btnChkAll, 0, wxALL, 2);
	szChkBtn->Add(m_btnChkNone, 0, wxALL, 2);

	wxBoxSizer *szgrid = new wxBoxSizer(wxVERTICAL);
	szgrid->Add(new wxStaticText(this, wxID_ANY, "2. Choose files to download:"), 0, wxALL, 2);
	szgrid->Add(m_chlResources, 10, wxALL | wxEXPAND, 1);
	szgrid->Add(szChkBtn, 0, wxALL, 1);

	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add(new wxStaticText(this, wxID_ANY, msg), 0, wxALL | wxEXPAND, 10);
	szmain->Add(szAddress, 0,  wxEXPAND, 1);
	szmain->Add(szgrid, 10, wxALL | wxEXPAND, 1);
	szmain->Add(szFolder, 0, wxALL | wxEXPAND, 1);
	szmain->Add(szWeatherFile, 0, wxALL | wxEXPAND, 1);
	szmain->Add( CreateButtonSizer( wxHELP|wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );

	SetSizer( szmain );
	Fit();
	m_txtAddress->SetFocus();
}


void NSRDBDialog::OnEvt( wxCommandEvent &e )
{
	switch( e.GetId() )
	{
		case wxID_HELP:
			SamApp::ShowHelp("download_weather_file");
			break;
		case ID_btnResources:
			{
				GetResources();
				std::sort(m_links.begin(),m_links.end());
				// select first item (should be tmy)
				//if (m_links.size() > 0) m_links[0].is_selected = true;
				RefreshList();
			}
			break;
		case ID_search:
			{
				for (size_t i = 0; i < m_links.size(); i++)
				{
					if (m_links[i].display.Lower().Contains(m_search->GetValue().Lower()))
						m_links[i].is_visible = true;
					else
						m_links[i].is_visible = false;
				}
				RefreshList();
			}
			break;
		case ID_btnSelectFiltered:
			{
				for (size_t i = 0; i < m_links.size(); i++)
				{
					if (m_links[i].is_visible)
						m_links[i].is_selected = true;
				}
				RefreshList();
			}
			break;
		case ID_btnUnselectFiltered:
			{
				for (size_t i = 0; i < m_links.size(); i++)
				{
					if (m_links[i].is_visible)
						m_links[i].is_selected = false;
				}
				RefreshList();
			}
			break;
		case ID_btnChkPsm30:
			{
				for (size_t i = 0; i < m_links.size(); i++)
				{
					if ((m_links[i].name.Lower() == "psmv3") && (m_links[i].interval.Lower() == "30"))
						m_links[i].is_selected = true;
					else
						m_links[i].is_selected = false;
				}
				RefreshList();
			}
			break;
		case ID_btnChkPsm60:
			{
				for (size_t i = 0; i < m_links.size(); i++)
				{
					if ((m_links[i].name.Lower() == "psmv3") && (m_links[i].interval.Lower() == "60"))
						m_links[i].is_selected = true;
					else
						m_links[i].is_selected = false;
				}
				RefreshList();
			}
			break;
		case ID_btnChkAll:
			{
				for (size_t i = 0; i < m_links.size(); i++)
					m_links[i].is_selected = true;
				RefreshList();
			}
			break;
		case ID_btnChkNone:
			{
				for (size_t i = 0; i < m_links.size(); i++)
					m_links[i].is_selected = false;
				RefreshList();
			}
			break;
		case ID_chlResources:
			{
				for (size_t i = 0; i < m_links.size(); i++)
				{
					if (m_links[i].display == m_chlResources->GetString(e.GetInt()))
						m_links[i].is_selected = m_chlResources->IsChecked(e.GetInt());
				}
				RefreshList();
			}
			break;
		case ID_btnFolder:
			{
				wxDirDialog dlg(SamApp::Window(), "Choose Download Folder", m_txtFolder->GetValue(), wxDD_DEFAULT_STYLE | wxDD_DIR_MUST_EXIST);
				if (dlg.ShowModal() == wxID_OK)
				{
					m_txtFolder->SetValue( dlg.GetPath());
					SamApp::Settings().Write("solar_data_paths", dlg.GetPath());
				}
			}
			break;
		case wxID_OK:
			{
				wxEasyCurl curl;
				wxArrayInt arychecked;
				for (size_t i = 0; i < m_links.size(); i++)
				{
					if (m_links[i].is_selected) 
					{
						arychecked.push_back((int)i);
					}
				}
				if (arychecked.Count() > 0)
				{
					bool stopped = false;
					int num_downloaded = 0;
					// check for valid download folder 
					wxString default_dnload_path;
					SamApp::Settings().Read("solar_download_path", &default_dnload_path);
					m_weatherFolder = m_txtFolder->GetValue();
					if (!wxDirExists(m_weatherFolder))
					{
						wxMessageBox("Please select a valid folder for downloads.");
						// reset to download folder
						m_txtFolder->SetValue(default_dnload_path);
						stopped = true;
					}
					// check for existing library entries
					wxString solar_resource_db = SamApp::GetUserLocalDataDir() + "/SolarResourceData.csv";
					Library *l = Library::Find(wxFileName(solar_resource_db).GetName());
					if ( !wxFileExists( solar_resource_db ) ) 
					{
						ScanSolarResourceData( solar_resource_db );
						l = Library::Load(solar_resource_db);
					}
					if (l==NULL)
					{
						wxMessageBox("Library " + solar_resource_db + " not found.");
						stopped = true;
					}
					if (!stopped)
					{
						for (size_t i = 0; i < arychecked.Count(); i++)
						{
							if (l->FindEntry(m_links[arychecked[i]].display) > -1)
							{
								wxMessageBox("Weather file " + m_links[arychecked[i]].display + " already in library\nPlease make a different selection.");
								stopped = true;
								break;
							}
						}
					}
					if (!stopped)
					{
						wxProgressDialog pdlg("Downloading NSRDB Data", "", (int)arychecked.Count(), this,
							wxPD_SMOOTH | wxPD_CAN_ABORT | wxPD_APP_MODAL | wxPD_AUTO_HIDE);
	#ifdef __WXMSW__
						pdlg.SetIcon(wxICON(appicon));
	#endif
						pdlg.Show();
						for (size_t i = 0; i < arychecked.Count(); i++)
						{
							int ndx = arychecked[i];
							wxString url = m_links[ndx].URL;
							// SAM specific attributes to minimize files size 
							wxString attr = m_links[ndx].attributes;
							wxString curstr = m_links[ndx].display;
							//Download the weather file
							pdlg.Update(i+1, "Downloading " + curstr + " from NSRDB...");
#ifdef __DEBUG__
			wxLogStatus("downloading (%d of %d): %s", (int)(i+1), (int)arychecked.Count(), (const char*)url.c_str());
#endif
							bool ok = curl.Get(url+attr);
							// try without attributes
							if (ok && (curl.GetDataAsString().Length() < 1000)) 
							{
								ok = curl.Get(url);
							}
							if (!ok)
							{
								wxMessageBox("Failed to download " + curstr + " from web service.");
								break;
							}
							else if (curl.GetDataAsString().Length() < 1000)
							{
								wxMessageBox("Failed to download " + curstr + " from web service." + curl.GetDataAsString());
								break;
							}
							else
							{
								wxString fn = curstr + ".csv";
								fn = m_weatherFolder + "/" + fn;
								if (!curl.WriteDataToFile(fn))
								{
									wxMessageBox("Failed to write " + fn);
									break;
								}
								num_downloaded++;
							}
							if (pdlg.WasCancelled())
								break;
						}
						if (!stopped && (num_downloaded > 0))
						{
							m_weatherFile = "";
							if (default_dnload_path != m_weatherFolder)
							{
								wxArrayString paths;
								wxString buf;
								if (SamApp::Settings().Read("solar_data_paths", &buf))
									paths = wxStringTokenize(buf, ";");
								if (paths.Index(m_weatherFolder) == wxNOT_FOUND)
								{
									paths.Add(m_weatherFolder);
									SamApp::Settings().Write("solar_data_paths", wxJoin(paths, ';'));
								}
							}
							if (m_cboWeatherFile->GetStringSelection().Len() > 0)
							{
								wxString fn = m_cboWeatherFile->GetStringSelection() + ".csv";
								fn = m_weatherFolder + "/" + fn;
								m_weatherFile = fn;
							}
							else // select first checked
							{
								wxString fn = m_links[arychecked[0]].display + ".csv";
								fn = m_weatherFolder + "/" + fn;
								m_weatherFile = fn;
							}
							EndModal(wxID_OK);
						}
					}
				}
			}
			break;
	}
}

void NSRDBDialog::RefreshList()
{
	// refresh resource list and combo box for selected weather file
	m_chlResources->Freeze();
	m_cboWeatherFile->Freeze();
	int chl_ndx = m_chlResources->GetTopItem();
	m_chlResources->Clear();
	m_cboWeatherFile->Clear();
	for (size_t i = 0; i < m_links.size(); i++)
	{
		if (m_links[i].is_visible)
		{
			int ndx = m_chlResources->Append(m_links[i].display);
			if (m_links[i].is_selected)
			{
				m_cboWeatherFile->Append(m_links[i].display);
				m_chlResources->Check(ndx, true);
			}
			else
				m_chlResources->Check(ndx, false);
		}
	}
	m_cboWeatherFile->Thaw();
	m_chlResources->Thaw();
	if (chl_ndx > -1 && chl_ndx < (int)m_chlResources->GetCount()) m_chlResources->SetFirstItem(chl_ndx);
}

void NSRDBDialog::GetResources()
{
	// hit api with address and return available resources
	wxString loc = m_txtAddress->GetValue();
	if (loc == "") return;

	wxString locname = "";

	bool is_addr = false;
	const wxChar* locChars = loc.c_str();
	for (int i = 0; i < (int)loc.Len(); i++) 
	{
		if (isalpha(locChars[i]))
			is_addr = true;
	}
	double lat, lon;
	if (is_addr)	//entered an address instead of a lat/long
	{
		if (!wxEasyCurl::GeoCodeDeveloper(loc, &lat, &lon))
		{
			wxMessageBox("Failed to geocode address");
			return;
		}
		else
			locname = wxString::Format("lat_%.5lf_lon_%.5lf", lat, lon);
	}
	else
	{
		wxArrayString parts = wxSplit(loc, ',');
		if (parts.Count() < 2)
		{
			wxMessageBox("Incorrectly formatted latitude, longitude.");
			return ;
		}
		else
		{
			parts[0].ToDouble(&lat);
			parts[1].ToDouble(&lon);
			locname = wxString::Format("lat_%.5lf_lon_%.5lf", lat, lon);
		}
	}

	//Create URL for weather file download
	wxString url;
	url = SamApp::WebApi("nsrdb_query");
	url.Replace("yourapikey", "<SAMAPIKEY>");
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
#ifdef __DEBUG__
	wxLogStatus("url: %s", (const char *)url.c_str());
#endif
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
		wxMessageBox("Could not process returned JSON for " + loc);
		return;
	}

	// valid filename from loc (user entered value)
	// replace spaces for SDK user friendly name
	loc.Replace("\\", "_"); 
	loc.Replace("/", "_"); 
	loc.Replace(" ", "_");
	loc.Replace(",", "_");
	loc.Replace("(", "_"); 
	loc.Replace(")", "_");
	loc.Replace("__", "_");

	m_chlResources->Clear();
	m_cboWeatherFile->Clear();
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
			wxString interval = links_list[i_links]["interval"].AsString();
			URL.Replace("yourapikey", "<SAMAPIKEY>");
			URL.Replace("youremail", "<USEREMAIL>");
			// URL - min attributes for each type 
			wxString attributes = "";
			if ((name.Trim() == "psmv3") && (year.Trim() != "tmy")) //replace psm with psmv3 10/22/2018, relative_humidity not available for tmy
				attributes = "&attributes=dhi,dni,dew_point,air_temperature,surface_pressure,wind_speed,wind_direction,surface_albedo";
				//attributes = "&attributes=dhi,dni,dew_point,surface_air_temperature_nwp,surface_pressure_background,surface_relative_humidity_nwp,wind_speed_10m_nwp";
			else if ((name.Trim() == "mts2") || (name.Trim() == "mts2-tmy"))
				attributes = "&attributes=dhi,dni,dew_point,temp_dryb,atm_pres,rel_hum,wind_spd";
			else if (name.Trim() == "mts1") // untested
				attributes = "&attributes=dhi,dni,dew_point,temp_dryb,atm_pres,rel_hum,wind_spd";
			else if (name.Trim() == "suny-international" && (year.Trim() != "tmy"))
				attributes = "&attributes=dhi,dni,dew_point,surface_temperature,surface_pressure,relative_humidity,snow_depth,wspd";
			else
				attributes = "";
#ifdef __DEBUG__
			wxLogStatus("link info: %s, %s, %s, %s, %s, %s", displayName.c_str(), name.c_str(), type.c_str(), year.c_str(), interval.c_str(), URL.c_str());
#endif
			// SAM does not recognize spectral datasets at this time
			// skip psm files per 10/25/2018 meeting
			if ((name.Lower() != "spectral-tmy") 
				&& (name.Lower() != "psm"))
				m_links.push_back(LinkInfo(name, displayName, type, year, URL, interval, loc, attributes));
		}
	}
}
