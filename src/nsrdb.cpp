/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

#include <rapidjson/reader.h>

#include "nsrdb.h"
#include "main.h"
#include "geotools.h"


enum {
	ID_txtAddress, ID_txtFolder, ID_cboFilter, /*ID_cboWeatherFile,*/ ID_chlResources,
	ID_btnSelectAll, ID_btnClearAll, ID_btnSelectFiltered, ID_btnShowSelected, ID_btnShowAll, ID_chk60, ID_chk30, ID_chk15, ID_chk10, ID_chk5, ID_chkTmy, ID_chkTgy, ID_chkTdy, ID_btnResources, ID_btnFolder, ID_search
};

BEGIN_EVENT_TABLE( NSRDBDialog, wxDialog )
	EVT_BUTTON(ID_btnSelectAll, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnClearAll, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnSelectFiltered, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnShowSelected, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnShowAll, NSRDBDialog::OnEvt)
	EVT_CHECKBOX(ID_chk60, NSRDBDialog::OnEvt)
	EVT_CHECKBOX(ID_chk30, NSRDBDialog::OnEvt)
	EVT_CHECKBOX(ID_chk15, NSRDBDialog::OnEvt)
	EVT_CHECKBOX(ID_chk10, NSRDBDialog::OnEvt)
	EVT_CHECKBOX(ID_chk5, NSRDBDialog::OnEvt)
	EVT_CHECKBOX(ID_chkTmy, NSRDBDialog::OnEvt)
	EVT_CHECKBOX(ID_chkTgy, NSRDBDialog::OnEvt)
	EVT_CHECKBOX(ID_chkTdy, NSRDBDialog::OnEvt)
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

	m_txtAddress = new wxTextCtrl(this, ID_txtAddress, "39.74,-105.17");// , wxDefaultPosition, wxSize(500, 30));
	m_txtLatLon = new wxTextCtrl(this, ID_txtAddress, "", wxDefaultPosition, wxDefaultSize, wxTE_READONLY);

	m_btnFolder = new wxButton(this, ID_btnFolder, "...", wxDefaultPosition, wxSize(30, 30));
	m_btnSelectAll = new wxButton(this, ID_btnSelectAll, "Select all");
	m_btnClearAll = new wxButton(this, ID_btnClearAll, "Clear all");
	m_chk60 = new wxCheckBox(this, ID_chk60, "60 min");
	m_chk30 = new wxCheckBox(this, ID_chk30, "30 min");
	m_chk15 = new wxCheckBox(this, ID_chk15, "15 min");
	m_chk10 = new wxCheckBox(this, ID_chk10, "10 min");
	m_chk5 = new wxCheckBox(this, ID_chk5, "5 min");
	m_chkTmy = new wxCheckBox(this, ID_chkTmy, "TMY");
	m_chkTgy = new wxCheckBox(this, ID_chkTgy, "TGY");
	m_chkTdy = new wxCheckBox(this, ID_chkTdy, "TDY");
	m_btnShowSelected = new wxButton(this, ID_btnShowSelected, "Show selected");
	m_btnShowAll = new wxButton(this, ID_btnShowAll, "Show all");
	m_btnSelectFiltered = new wxButton(this, ID_btnSelectFiltered, "Select filtered");
	m_btnResources = new wxButton(this, ID_btnResources, "Find");
	m_search = new wxSearchCtrl(this, ID_search, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER | wxTE_PROCESS_TAB);
	//m_cboWeatherFile = new wxComboBox(this, ID_cboWeatherFile, ""); // populate with selected resources
	m_chlResources = new wxCheckListBox(this, ID_chlResources, wxDefaultPosition, wxSize(800,200)); // populate with returned resources

	wxString msg = "Use this window to list all weather files available from the NSRDB for a given location, and choose files to download and add to your solar resource library.\n";
	msg += "Type an address or latitude and longtitude, for example, \"15031 denver west parkway golden co\" or \"39.74,-105.17\", and click Find to list available files.\n";
	msg += "When the list appears, select the file or files you want to download, or use the filter and auto-select buttons to find and select files.\n";
	msg += "Choose the download folder where you want SAM to save files, or use the default SAM Downloaded Weather Files folder.\n";
	msg += " SAM automatically adds the folder to the list of folders it uses to populate your solar resource library.";
	msg += "Click OK to download the selected files and add them to your solar resource library.";

	wxBoxSizer *szAddress = new wxBoxSizer(wxHORIZONTAL);
	szAddress->Add(new wxStaticText(this, wxID_ANY, "1. Location:"), 0, wxALL , 2);
	szAddress->Add(m_txtAddress, 5, wxALL|wxEXPAND, 2);
	szAddress->Add(m_btnResources, 0, wxALL, 2);

	wxBoxSizer* szLatLon = new wxBoxSizer(wxHORIZONTAL);
	szLatLon->Add(15, 0, 0);
	szLatLon->Add(new wxStaticText(this, wxID_ANY, "Latitude and longitude:"), 0, wxALL, 2);
	szLatLon->Add(m_txtLatLon, 5, wxALL | wxEXPAND, 1);

	wxBoxSizer *szFolder = new wxBoxSizer(wxHORIZONTAL);
	szFolder->Add(new wxStaticText(this, wxID_ANY, "3. Choose download folder:"), 0, wxALL, 2);
	szFolder->Add(m_txtFolder, 5, wxALL | wxEXPAND, 2);
	szFolder->Add(m_btnFolder, 0, wxALL, 2);
/*
	wxBoxSizer *szWeatherFile = new wxBoxSizer(wxHORIZONTAL);
	szWeatherFile->Add(new wxStaticText(this, wxID_ANY, "4. Choose file for simulation (optional):"), 0, wxALL , 2);
	szWeatherFile->Add(m_cboWeatherFile, 5, wxALL | wxEXPAND, 2);
*/	
	wxBoxSizer* szChkBtn = new wxBoxSizer(wxHORIZONTAL);
	szChkBtn->Add(new wxStaticText(this, wxID_ANY, "Auto-select options:"), 0, wxALL, 2);
	szChkBtn->Add(m_chk60, 0, wxALL, 2);
	szChkBtn->Add(m_chk30, 0, wxALL, 2);
	szChkBtn->Add(m_chk15, 0, wxALL, 2);
	szChkBtn->Add(m_chk10, 0, wxALL, 2);
	szChkBtn->Add(m_chk5, 0, wxALL, 2);
	szChkBtn->Add(m_chkTmy, 0, wxALL, 2);
	szChkBtn->Add(m_chkTgy, 0, wxALL, 2);
	szChkBtn->Add(m_chkTdy, 0, wxALL, 2);

	wxBoxSizer* szFilter = new wxBoxSizer(wxHORIZONTAL);
	szFilter->Add(new wxStaticText(this, wxID_ANY, "Filter:"), 0, wxALL, 2);
	szFilter->Add(m_search, 0, wxALIGN_CENTER_VERTICAL | wxALL, 2);
	szFilter->Add(m_btnSelectFiltered, 0, wxALL, 2);
	szFilter->Add(m_btnShowSelected, 0, wxALL, 2);
	szFilter->Add(m_btnShowAll, 0, wxALL, 2);
	szFilter->Add(m_btnSelectAll, 0, wxALL, 2);
	szFilter->Add(m_btnClearAll, 0, wxALL, 2);
	
	wxBoxSizer *szgrid = new wxBoxSizer(wxVERTICAL);
	szgrid->Add(new wxStaticText(this, wxID_ANY, "2. Select files to download:"), 0, wxALL, 2);
	szgrid->Add(m_chlResources, 10, wxALL | wxEXPAND, 1);
	szgrid->Add(szFilter, 0, wxALL, 1);
	szgrid->Add(szChkBtn, 0, wxALL, 1);

	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add(new wxStaticText(this, wxID_ANY, msg), 0, wxALL | wxEXPAND, 10);
	szmain->Add(szAddress, 0,  wxEXPAND, 1);
	szmain->Add(szLatLon, 0, wxEXPAND, 1);
	szmain->Add(szgrid, 10, wxALL | wxEXPAND, 1);
	szmain->Add(szFolder, 0, wxALL | wxEXPAND, 1);
	//szmain->Add(szWeatherFile, 0, wxALL | wxEXPAND, 1);
	szmain->Add( CreateButtonSizer( wxHELP|wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );

	ResetAll();
	SetSizer( szmain );
	Fit();
	m_txtAddress->SetFocus();
}


void NSRDBDialog::OnEvt( wxCommandEvent &e )
{
	switch( e.GetId() )
	{
		case wxID_HELP:
			SamApp::ShowHelp("nsrdb_advanced_download");
			break;
		case ID_btnResources:
			{
				ResetAll();
				GetResources();
				//std::sort(m_links.begin(),m_links.end());
				// select first item (should be tmy)
				//if (m_links.size() > 0) m_links[0].is_selected = true;
				RefreshList(0);
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
				RefreshList(0);
			}
			break;
		case ID_btnSelectFiltered:
			{
				size_t count = 0;
				size_t first_item = 0;
				if (m_search->GetValue() == "")
				{
					wxMessageBox("Type a keyword in the Search box before clicking Select Filtered.", "NSRDB Download Message", wxOK, this);
					break;
				}
				for (size_t i = 0; i < m_links.size(); i++)
				{
					m_links[i].is_visible = true;
					if (m_links[i].display.Lower().Contains(m_search->GetValue().Lower()))
					{
						if (count == 0) first_item = i;
						m_links[i].is_selected = true;
						count++;
					}
				}
				m_search->SetValue("");
				RefreshList(first_item);
			}
			break;
		case ID_btnShowSelected:
			{
				for (size_t i = 0; i < m_links.size(); i++)
				{
					if (m_links[i].is_selected) m_links[i].is_visible = true;
					else m_links[i].is_visible = false;
				}
				RefreshList(0);
			}
			break;
		case ID_btnShowAll:
			{
				for (size_t i = 0; i < m_links.size(); i++)
					m_links[i].is_visible = true;
				RefreshList(0);
			}
		break;
		case ID_chk60:
			{
				size_t x = SelectItems("_60", m_chk60);
				RefreshList(x);
			}
			break;
		case ID_chk30:
			{
				size_t x = SelectItems("_30", m_chk30);
				RefreshList(x);
			}
			break;
		case ID_chk15:
		{
			size_t x = SelectItems("_15", m_chk15);
			RefreshList(x);
		}
		break;
		case ID_chk10:
		{
			size_t x = SelectItems("_10", m_chk10);
			RefreshList(x);
		}
		break;
		case ID_chk5:
			{
				size_t x = SelectItems("_5", m_chk5);
				RefreshList(x);
			}
			break;
		case ID_chkTmy:
			{
				size_t x = SelectItems("_tmy", m_chkTmy);
				RefreshList(x);
			}
			break;
		case ID_chkTgy:
			{
				size_t x = SelectItems("_tgy", m_chkTgy);
				RefreshList(x);
			}
			break;
		case ID_chkTdy:
			{
				size_t x = SelectItems("_tdy", m_chkTdy);
				RefreshList(x);
			}
			break;
		case ID_btnSelectAll:
			{
				for (size_t i = 0; i < m_links.size(); i++)
				{
					m_links[i].is_selected = true;
					m_links[i].is_visible = true;
				}
				RefreshList(0);
			}
			break;
		case ID_btnClearAll:
			{
				m_search->SetValue("");
				for (size_t i = 0; i < m_links.size(); i++)
				{
					m_links[i].is_selected = false;
					m_links[i].is_visible = true;
				}
				m_chk60->SetValue(false);
				m_chk30->SetValue(false);
				m_chk15->SetValue(false);
				m_chk10->SetValue(false);
				m_chk5->SetValue(false);
				m_chkTmy->SetValue(false);
				m_chkTgy->SetValue(false);
				m_chkTdy->SetValue(false);
				RefreshList(0);
			}
			break;
		case ID_chlResources:
			{
				for (size_t i = 0; i < m_links.size(); i++)
				{
					if (m_links[i].display == m_chlResources->GetString(e.GetInt()))
						m_links[i].is_selected = m_chlResources->IsChecked(e.GetInt());
				}
			}
			break;
		case ID_btnFolder:
			{
				wxDirDialog dlg(SamApp::Window(), "Choose Download Folder", m_txtFolder->GetValue(), wxDD_DEFAULT_STYLE | wxDD_DIR_MUST_EXIST);
				if (dlg.ShowModal() == wxID_OK)
				{
					m_txtFolder->SetValue( dlg.GetPath());
					//SamApp::Settings().Write("solar_data_paths", dlg.GetPath());
                    wxArrayString paths;
                    wxString buf;
                    if (SamApp::Settings().Read("solar_data_paths", &buf))
                        paths = wxStringTokenize(buf, ";");
                    if (paths.Index(dlg.GetPath()) == wxNOT_FOUND)
                    {
                        paths.Add(dlg.GetPath());
                        SamApp::Settings().Write("solar_data_paths", wxJoin(paths, ';'));
                    }
				}
			}
			break;
		case wxID_OK:
			{
				wxEasyCurl curl;
				wxArrayInt arychecked;
				wxString file_list = "";
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
					wxString library_fn = "";
					// check for valid download folder 
					wxString default_dnload_path;
					SamApp::Settings().Read("solar_download_path", &default_dnload_path);
					m_weatherFolder = m_txtFolder->GetValue();
					if (!wxDirExists(m_weatherFolder))
					{
						wxMessageBox("Choose a valid folder for weather file downloads.", "NSRDB Download Message", wxOK, this);
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
						wxMessageBox("Library " + solar_resource_db + " not found.", "NSRDB Download Message", wxOK, this);
						stopped = true;
					}
					if (!stopped)
					{
						wxProgressDialog pdlg("Downloading from NSRDB", "", (int)arychecked.Count(), this,
							wxPD_SMOOTH | wxPD_CAN_ABORT | wxPD_APP_MODAL | wxPD_AUTO_HIDE);
	#ifdef __WXMSW__
						pdlg.SetIcon(wxICON(appicon));
	#endif
						pdlg.Show();
						for (size_t i = 0; i < arychecked.Count(); i++)
						{
							if (l->FindEntry(m_links[arychecked[i]].display) > -1)
							{
								wxMessageBox("Skipping download.\n\n" + m_links[arychecked[i]].display + "\n\nFile is already in your solar resource library.", "NSRDB Download Message", wxOK, this);
								continue;
							}

							int ndx = arychecked[i];
							wxString url = m_links[ndx].URL;
							wxString curstr = m_links[ndx].display;
							//Download the weather file
							pdlg.Update(i+1, "File: " + curstr );
#ifdef __DEBUG__
			wxLogStatus("downloading (%d of %d): %s", (int)(i+1), (int)arychecked.Count(), (const char*)url.c_str());
#endif
							bool ok = curl.Get(url+"&utc=false");
							if (!ok)
								wxMessageBox("Download failed.\n\n" + curstr +"\n\nThere may be a problem with your internet connection,\nor the NSRDB web service may be down.", "NSRDB Download Message", wxOK, this);
							else if (curl.GetDataAsString().Length() < 1000)
								wxMessageBox("Weather file not available.\n\n" + curstr + "\n\n" + curl.GetDataAsString(), "NSRDB Download Message", wxOK, this);
							else
							{
								wxString fn = curstr + ".csv";
								fn = m_weatherFolder + "/" + fn;
								file_list += fn + "\n";
								if (!curl.WriteDataToFile(fn))
								{
									wxMessageBox("Failed to write file.\n\n" + fn, "NSRDB Download Message", wxOK, this);
									//break;
								}
								num_downloaded++;
							}
							if (pdlg.WasCancelled())
								break;
						}
						if (!stopped && (num_downloaded > 0))
						{
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
							if (file_list != "") wxMessageBox("Download complete.\n\nThe following files have been downloaded and added to your solar resource library:\n\n" + file_list, "NSRDB Download Message",wxOK,this);
							EndModal(wxID_OK);
						}
					}
				}
				else wxMessageBox("Nothing to download.\n\nChoose a file to download, or type a location and click Find to generate a list of available files.", "NSRDB Download Message", wxOK, this);
			}
			break;
	}
}

size_t NSRDBDialog::SelectItems( wxString str_filter, wxCheckBox *check_box )
{
	size_t count = 0;
	size_t item_first = 0;
	if (m_chlResources->IsEmpty()) wxMessageBox("No files to choose. Type a location and click Find to find files.", "NSRDB Download Message", wxOK, this);
	else
	{
		for (size_t i = 0; i < m_links.size(); i++)
		{
			m_links[i].is_visible = true;
			if (m_links[i].display.Lower().Contains(str_filter) && check_box->IsChecked())
			{
				if (count == 0) item_first = i;
				m_links[i].is_selected = true;
				count++;
			}
			else if (m_links[i].display.Lower().Contains(str_filter) && !check_box->IsChecked()) m_links[i].is_selected = false;
		}
	}
	return item_first;
}

void NSRDBDialog::RefreshList( size_t first_item )
{
	m_chlResources->Freeze();
	m_chlResources->Clear();
	for (size_t i = 0; i < m_links.size(); i++)
	{
		if (m_links[i].is_visible)
		{
			int ndx = m_chlResources->Append(m_links[i].display);
			if (m_links[i].is_selected)
				m_chlResources->Check(ndx, true);
			else
				m_chlResources->Check(ndx, false);
		}
	}
	m_chlResources->Thaw();
	if ( first_item > 0 )
		m_chlResources->SetFirstItem(first_item);
}

void NSRDBDialog::ResetAll()
{
	m_txtLatLon->Clear();
	m_chlResources->Clear();
	m_chlResources->Disable();
	m_links.clear();
	m_chk60->SetValue(false);
	m_chk30->SetValue(false);
	m_chk15->SetValue(false);
	m_chk10->SetValue(false);
	m_chk5->SetValue(false);
	m_chkTmy->SetValue(false);
	m_chkTgy->SetValue(false);
	m_chkTdy->SetValue(false);
	m_chk60->Disable();
	m_chk30->Disable();
	m_chk15->Disable();
	m_chk10->Disable();
	m_chk5->Disable();
	m_chkTmy->Disable();
	m_chkTgy->Disable();
	m_chkTdy->Disable();
	m_search->Disable();
	m_btnSelectFiltered->Disable();
	m_btnShowSelected->Disable();
	m_btnShowAll->Disable();
	m_btnSelectAll->Disable();
	m_btnClearAll->Disable();
}
void NSRDBDialog::GetResources()
{
	// hit api with address and return available resources
	wxString location = m_txtAddress->GetValue();
	if (location == "")
	{
		wxMessageBox("Type a latitude-longitude pair (lat, lon), street address, or location name and click Find.", "NSRDB Download Message", wxOK, this);
		return;
	}

	wxString locname = "";

	bool is_addr = false;
	const wxChar* locChars = location.c_str();
	for (int i = 0; i < (int)location.Len(); i++) 
	{
		if (isalpha(locChars[i]))
			is_addr = true;
	}
	double lat, lon;
	if (is_addr)	//entered an address instead of a lat/long
	{
		// use GeoTools::GeocodeGoogle for non-NREL builds and set google_api_key in private.h
		if (!GeoTools::GeocodeDeveloper(location, &lat, &lon))
		{
			wxMessageBox("Failed to geocode address.\n\n" + location, "NSRDB Download Message", wxOK, this);
			return;
		}
		else
			locname = wxString::Format("lat_%.5lf_lon_%.5lf", lat, lon);
	}
	else
	{
		wxArrayString parts = wxSplit(location, ',');
		if (parts.Count() < 2)
		{
			wxMessageBox("Type a valid latitude-longitude (lat, lon), street address, or location name.", "NSRDB Download Message", wxOK, this);
			return ;
		}
		else
		{
			parts[0].ToDouble(&lat);
			parts[1].ToDouble(&lon);
			locname = wxString::Format("lat_%.5lf_lon_%.5lf", lat, lon);
		}
	}

	// NSRDB Data Query returns a list of links to all available files for a location
	// https://developer.nrel.gov/docs/solar/nsrdb/nsrdb_data_query/
	wxString url;
	url = SamApp::WebApi("nsrdb_query");
	url.Replace("<LAT>", wxString::Format("%lg", lat), 1);
	url.Replace("<LON>", wxString::Format("%lg", lon), 1);

	//Download the weather file
	wxEasyCurl curl;
	bool ok = curl.Get(url, "Getting list of available files from NSRDB...", SamApp::Window());

	if (!ok)
	{
		wxMessageBox("NSRDB Data Query failed.\n\nThere may be a problem with your internet connection,\nor the NSRDB web service may be down.", "NSRDB Download Message", wxOK, this);
		return;
	}
#ifdef __DEBUG__
	wxLogStatus("url: %s", (const char *)url.c_str());
#endif
	wxString json_data = curl.GetDataAsString();
	if (json_data.IsEmpty())
	{
		wxMessageBox("NSRDB Data Query failed.\n\nJSON data empty.\nPlease try again and contact SAM Support if you continue to have trouble.", "NSRDB Download Message", wxOK, this);
		return;
	}


	rapidjson::Document reader;
	reader.Parse(json_data.c_str());

	if (reader.HasParseError())
	{
		wxMessageBox("NSRDB Data Query failed.\n\nCould not process JSON from NSRDB Data Query response for\n" + location + "\n\nPlease try again and contact SAM Support if you continue to have trouble.", "NSRDB Download Message", wxOK, this);
		return;
	}

	if (reader.HasMember("metadata"))
		if (reader["metadata"].HasMember("resultset"))
			if (reader["metadata"]["resultset"].HasMember("count"))
				if (reader["metadata"]["resultset"]["count"].IsInt())
					if (reader["metadata"]["resultset"]["count"].GetInt() == 0)	{
						wxMessageBox("No Weather Files Found.\n\nNSRDB Data Query did not return any files for\n" + location, "NSRDB Download Message", wxOK, this);
						return;
					}

	if (reader.HasMember("error"))
		if (reader["error"].HasMember("message") && reader["error"].HasMember("code"))
			if (reader["error"]["message"].IsString() && reader["error"]["code"].IsString()) {
				wxString message = reader["error"]["message"].GetString();
				wxString code = reader["error"]["code"].GetString();
		        wxMessageBox( wxString::Format("NSRDB API error!\n\nMessage: %s\n\nCode: %s ", message.c_str(), code.c_str()));
				return;
			}


	// format location to use in file name
	location.Replace("\\", "_"); 
	location.Replace("/", "_"); 
	location.Replace(" ", "_");
	location.Replace(",", "_");
	location.Replace("(", "_"); 
	location.Replace(")", "_");
	location.Replace("__", "_");

	m_txtLatLon->SetValue(wxString::Format("%f,%f", lat, lon));


	if (!reader.HasMember("outputs")) return; // error message?
	if (!reader["outputs"].IsArray()) return; // error message?

	auto output_list = reader["outputs"].GetArray();

	for (size_t i_outputs = 0; i_outputs<output_list.Size(); i_outputs++)
	{

		if (!output_list[i_outputs].HasMember("links")) return; // error message?
		if (!output_list[i_outputs]["links"].IsArray()) return; // error message?

		auto links_list = output_list[i_outputs]["links"].GetArray();

		for (size_t i_links = 0; i_links < links_list.Size(); i_links++)
		{
			// check for validity
			wxString name = output_list[i_outputs]["name"].GetString();
			wxString displayName = output_list[i_outputs]["displayName"].GetString();

			wxString year, interval;
			// year may be a number like 2018 or a string like "tmy-2020"
			if (links_list[i_links]["year"].IsInt())
				year = wxString::Format("%d", links_list[i_links]["year"].GetInt());
			else
				year = links_list[i_links]["year"].GetString();
			// interval should be a number like 5, 15, 30, 60
			if (links_list[i_links]["interval"].IsInt())
				interval = wxString::Format("%d", links_list[i_links]["interval"].GetInt());

			wxString URL = links_list[i_links]["link"].GetString();
            URL.Replace("yourapikey", "<SAMAPIKEY>");
			URL.Replace("youremail", "<USEREMAIL>");

			/*
			datasets have different available intervals in addition to 60 (all datasets have 60 minute data):
			psm3 https://developer.nrel.gov/docs/solar/nsrdb/psm3-download/ 30
			psm3-2-2 https://developer.nrel.gov/docs/solar/nsrdb/psm3-2-2-download/ 30
			nsrdb-GOES-aggregated-v4-0-0 https://developer.nrel.gov/api/nsrdb/v2/solar/nsrdb-GOES-aggregated-v4-0-0-download 30
			psm3-5min https://developer.nrel.gov/docs/solar/nsrdb/psm3-5min-download/ 5,15,30
			suny-india https://developer.nrel.gov/docs/solar/nsrdb/suny-india-data-download/ 15,30
			msg-iodc https://developer.nrel.gov/docs/solar/nsrdb/meteosat-download/ 15,30
			msg-v1-0-0 https://developer.nrel.gov/docs/solar/nsrdb/nsrdb-msg-v1-0-0-download/ 15,30
			himawari https://developer.nrel.gov/docs/solar/nsrdb/himawari-download/ 10,30
			himawari7 https://developer.nrel.gov/docs/solar/nsrdb/himawari7-download/ 30
			*/

#ifdef __DEBUG__
			wxLogStatus("link info: %s, %s, %s, %s, %s, %s", displayName.c_str(), name.c_str(), /*type.c_str(),*/ year.c_str(), interval.c_str(), URL.c_str());
#endif
			// skip some datasets
			if ((name.Lower() != "spectral-india-tmy") // not compatible with SAM
				&& (name.Lower() != "full-disc") // only covers a few years and is similar to PSM V3 from solar modeling perspective
				&& (name.Lower() != "nsrdb-goes-full-disc-v4-0-0") // similar to GOES V4 from solar modeling perspective
				&& (name.Lower() != "philippines") // basic solar resource data only tamb, dhi, dni, ghi, wind (not enough data for CSP or PV thermal models)
				&& (name.Lower() != "vietnam")) // // basic solar resource data only tamb, dhi, dni, ghi, wind  (not enough data for CSP or PV thermal models)
			{
				m_links.push_back(LinkInfo(name, displayName, year, URL, interval, location));

				// enable list, search, and buttons
				m_chlResources->Enable();
				m_search->Enable();
				m_btnSelectFiltered->Enable();
				m_btnShowSelected->Enable();
				m_btnShowAll->Enable();
				m_btnSelectAll->Enable();
				m_btnClearAll->Enable();

				// enable filter options that are available for this location
				m_chk60->Enable(); // all datasets have hourly data
				if (interval.IsSameAs("30"))
					m_chk30->Enable();
				if (interval.IsSameAs("15"))
					m_chk15->Enable();
				if (interval.IsSameAs("10"))
					m_chk10->Enable();
				if (interval.IsSameAs("5"))
					m_chk5->Enable();
				// may be names like "tmy" or "tmy-2020" so use Contains() for these
				if (year.Lower().Contains("tmy"))
					m_chkTmy->Enable();
				if (year.Lower().Contains("tgy"))
					m_chkTgy->Enable();
				if (year.Lower().Contains("tdy"))
					m_chkTdy->Enable();
			}
		}
	}
}
