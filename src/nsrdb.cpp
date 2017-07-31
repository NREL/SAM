/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#include <wx/checklst.h>
#include <wx/combobox.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/sizer.h>
#include <wx/dirdlg.h>
#include <wx/progdlg.h>
#include <wx/checkbox.h>
#include <wx/tokenzr.h>
//#include <wx/filepicker.h>

#include <wex/easycurl.h>
#include <wex/jsonval.h>
#include <wex/jsonreader.h>

#include "nsrdb.h"
#include "main.h"


enum {
	ID_txtAddress, ID_txtFolder, ID_cboFilter, ID_cboWeatherFile, ID_chlResources,
	ID_btnChkAll, ID_btnChkFiltered, ID_btnChkNone, ID_btnResources, ID_btnFolder, ID_btnDownload, ID_dirPicker
};

BEGIN_EVENT_TABLE( NSRDBDialog, wxDialog )
	EVT_BUTTON(ID_btnChkAll, NSRDBDialog::OnEvt)
//	EVT_BUTTON(ID_btnChkFiltered, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnChkNone, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnResources, NSRDBDialog::OnEvt)
	EVT_BUTTON(ID_btnFolder, NSRDBDialog::OnEvt)
//	EVT_BUTTON(ID_btnDownload, NSRDBDialog::OnEvt)
	EVT_BUTTON(wxID_OK, NSRDBDialog::OnEvt)
	EVT_CHECKLISTBOX(ID_chlResources, NSRDBDialog::OnEvt)
//	EVT_DIRPICKER_CHANGED(ID_dirPicker, NSRDBDialog::OnDir)
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
	m_txtAddress = new wxTextCtrl(this, ID_txtAddress, "40.1,-109.3");// , wxDefaultPosition, wxSize(500, 30));

	m_btnFolder = new wxButton(this, ID_btnFolder, "...", wxDefaultPosition, wxSize(30, 30));
	m_btnChkAll = new wxButton(this, ID_btnChkAll, "Select all");
//	m_btnChkFiltered = new wxButton(this, ID_btnChkFiltered, "Select filtered");
	m_btnChkNone = new wxButton(this, ID_btnChkNone, "Unselect all");
	m_btnResources = new wxButton(this, ID_btnResources, "Update List");
//	m_dirpicker = new wxDirPickerCtrl(this, ID_dirPicker, dnpath, "...", wxDefaultPosition, wxSize(500, 30), wxDIRP_DIR_MUST_EXIST);
//	m_btnDownload = new wxButton(this, ID_btnDownload, "Download");

//	m_cboFilter = new wxComboBox(this, ID_cboFilter, ""); // populate with returned resources type and interval
	m_cboWeatherFile = new wxComboBox(this, ID_cboWeatherFile, ""); // populate with selected resources
	m_chlResources = new wxCheckListBox(this, ID_chlResources, wxDefaultPosition, wxSize(800,400)); // populate with returned resources

	m_chkFolder = new wxCheckBox(this, wxID_ANY, "Add Download Location");

	wxString msg = "Enter your location as a street address or latitude, longitude:\nExamples:\n  15031 denver west parkway golden co\n   40.1,-109.3\nThe email address you used to register SAM will be sent to the NSRDB at NREL. If you do not want share your email address with the NSRDB, press Cancel now.\n";


	wxBoxSizer *szAddress = new wxBoxSizer(wxHORIZONTAL);
	szAddress->Add(new wxStaticText(this, wxID_ANY, "Address"), 0, wxALL , 2);
	szAddress->Add(m_txtAddress, 5, wxALL|wxEXPAND, 2);
	szAddress->Add(m_btnResources, 0, wxALL, 2);

	wxBoxSizer *szFolder = new wxBoxSizer(wxHORIZONTAL);
//	szFolder->Add(new wxStaticText(this, wxID_ANY, "Download Folder"), 0, wxALL , 10);
	szFolder->Add(m_chkFolder, 0, wxALL,2);
	szFolder->Add(m_txtFolder, 5, wxALL | wxEXPAND, 2);
	szFolder->Add(m_btnFolder, 0, wxALL, 2);
//	szFolder->Add(m_btnDownload, 0, wxALL, 10);

	wxBoxSizer *szWeatherFile = new wxBoxSizer(wxHORIZONTAL);
	szWeatherFile->Add(new wxStaticText(this, wxID_ANY, "Weather file to use in SAM"), 0, wxALL , 2);
	szWeatherFile->Add(m_cboWeatherFile, 5, wxALL | wxEXPAND, 2);

//	wxBoxSizer *szFilter = new wxBoxSizer(wxHORIZONTAL);
//	szFilter->Add(new wxStaticText(this, wxID_ANY, "Filter list"), 0, wxALL , 10);
//	szFilter->Add(m_cboFilter, 5, wxALL | wxEXPAND, 10);


	wxBoxSizer *szChkBtn = new wxBoxSizer(wxHORIZONTAL);
//	szChkBtn->Add(m_btnChkFiltered, 0, wxALL, 10);
	szChkBtn->Add(m_btnChkAll, 0, wxALL, 2);
	szChkBtn->Add(m_btnChkNone, 0, wxALL, 2);


	wxBoxSizer *szgrid = new wxBoxSizer(wxVERTICAL);
	szgrid->Add(szChkBtn, 0, wxALL , 1);
	szgrid->Add( m_chlResources, 10, wxALL|wxEXPAND, 1 );


	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add(new wxStaticText(this, wxID_ANY, msg), 0, wxALL | wxEXPAND, 10);
	szmain->Add(szAddress, 0,  wxEXPAND, 1);
//	szmain->Add(szFilter, 0, wxALL | wxEXPAND, 10);
	szmain->Add(szgrid, 10, wxALL | wxEXPAND, 1);
	szmain->Add(szFolder, 0, wxALL | wxEXPAND, 1);
//	szmain->Add(m_dirpicker, 0, wxALL | wxEXPAND, 10);
	szmain->Add(szWeatherFile, 0, wxALL | wxEXPAND, 1);
	szmain->Add( CreateButtonSizer( wxHELP|wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );

	SetSizer( szmain );
	Fit();
	m_txtAddress->SetFocus();
}

/*
void NSRDBDialog::OnDir(wxFileDirPickerEvent &e)
{
	wxMessageBox("Valid Event");
}
*/

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
				for (size_t i = 0; i < m_chlResources->GetCount(); i++)
					m_chlResources->Check(i, true);
			}
			break;
		case ID_btnChkNone:
			{
				for (size_t i = 0; i < m_chlResources->GetCount(); i++)
					m_chlResources->Check(i,false);
			}
			break;
		case ID_chlResources:
			{
				wxArrayInt arychecked;
				unsigned int num = m_chlResources->GetCheckedItems(arychecked);
				if (num > 0)
				{
					m_cboWeatherFile->Clear();
					m_cboWeatherFile->Freeze();
					for (size_t i = 0; i < arychecked.Count(); i++)
					{
						m_cboWeatherFile->Insert(m_links[arychecked[i]].display, i);
					}
					m_cboWeatherFile->Thaw();
				}
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
//		case ID_btnDownload:
		case wxID_OK:
			{
				wxEasyCurl curl;
				wxArrayInt arychecked;
				unsigned int num = m_chlResources->GetCheckedItems(arychecked);
				if (num > 0)
				{
					bool stopped = false;
					wxProgressDialog pdlg("Downloading NSRDB Data", "", num, this,
						wxPD_SMOOTH | wxPD_CAN_ABORT | wxPD_APP_MODAL | wxPD_AUTO_HIDE);
#ifdef __WXMSW__
					pdlg.SetIcon(wxICON(appicon));
#endif
					pdlg.Show();

					//for (int i = 0; i < m_chlResources->GetCount(); i++)
					for (size_t i = 0; i < arychecked.Count() && !stopped; i++)
					{
						//					if (m_chlResources->IsChecked(i))
						//					{
						int ndx = arychecked[i];
						wxString url = m_links[ndx].URL;
						wxString curstr = m_chlResources->GetString(ndx);
						//Download the weather file
						pdlg.Update(i+1, "Downloading " + curstr + " from NSRDB...");
						//						bool ok = curl.Get(url, "Downloading " + curstr + " from NSRDB...", SamApp::Window());
						bool ok = curl.Get(url);
						if (!ok)
						{
							wxMessageBox("Failed to download " + curstr + " from web service.");
							stopped = true;
						}
						else
						{
							if (!wxDirExists(m_txtFolder->GetValue()))
								//							if (!wxDirExists(m_dirpicker->GetTextCtrlValue()))
							{
								wxMessageBox("Please select a valid folder for downloads.");
								stopped = true;
							}
							else
							{
								wxString fn = curstr + ".csv";
								fn = m_txtFolder->GetValue() + "/" + fn;
								if (!curl.WriteDataToFile(fn))
								{
									wxMessageBox("Failed to write " + fn);
									stopped = true;
								}
							}
						}
						if (pdlg.WasCancelled())
							stopped = true;
						//					}
					}
					if (!stopped)
					{
						m_weatherFolder = m_txtFolder->GetValue();
						m_weatherFile = "";
						m_addFolder = "no";
						if (m_chkFolder->IsChecked())
						{ // update solar resource library
							wxString buf;
							wxArrayString paths;
							if (SamApp::Settings().Read("solar_data_paths", &buf))
								paths = wxStringTokenize(buf, ";");
							paths.Add(m_weatherFolder);
							SamApp::Settings().Write("solar_data_paths", wxJoin(paths, ';'));
							m_addFolder = "yes";
						}
						if (m_cboWeatherFile->GetStringSelection().Len() > 0)
						{
							wxString fn = m_cboWeatherFile->GetStringSelection() + ".csv";
							fn = m_txtFolder->GetValue() + "/" + fn;
							m_weatherFile = fn;
						}
						EndModal(wxID_OK);
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
