/*
BSD 3-Clause License

Copyright (c) Alliance for Energy Innovation, LLC. See also https://github.com/NatLabRockies/SAM/blob/develop/LICENSE
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
#include <wex/csv.h>

#include <rapidjson/reader.h>

#include "nohrsc.h"
#include "main.h"
#include "geotools.h"


enum {
	ID_txtAddress, /*ID_txtFolder,*/ ID_cboFilter, /*ID_cboWeatherFile,*/ ID_chlResources,
	ID_btnSelectAll, ID_btnClearAll, ID_btnSelectFiltered, ID_btnShowSelected, ID_btnShowAll, ID_chk60, ID_chk30, ID_chk15, ID_chk10, ID_chk5, ID_chkTmy, ID_chkTgy, ID_chkTdy, ID_btnResources, /*ID_btnFolder,*/ ID_search, ID_btnSaveToFile
};

BEGIN_EVENT_TABLE(NOHRSCDialog, wxDialog)
//EVT_BUTTON(ID_btnSelectAll, NOHRSCDialog::OnEvt)
//EVT_BUTTON(ID_btnClearAll, NOHRSCDialog::OnEvt)
//EVT_BUTTON(ID_btnSelectFiltered, NOHRSCDialog::OnEvt)
//EVT_BUTTON(ID_btnShowSelected, NOHRSCDialog::OnEvt)
//EVT_BUTTON(ID_btnShowAll, NOHRSCDialog::OnEvt)
//EVT_CHECKBOX(ID_chk60, NOHRSCDialog::OnEvt)
//EVT_CHECKBOX(ID_chk30, NOHRSCDialog::OnEvt)
//EVT_CHECKBOX(ID_chk15, NOHRSCDialog::OnEvt)
//EVT_CHECKBOX(ID_chk10, NOHRSCDialog::OnEvt)
//EVT_CHECKBOX(ID_chk5, NOHRSCDialog::OnEvt)
//EVT_CHECKBOX(ID_chkTmy, NOHRSCDialog::OnEvt)
//EVT_CHECKBOX(ID_chkTgy, NOHRSCDialog::OnEvt)
//EVT_CHECKBOX(ID_chkTdy, NOHRSCDialog::OnEvt)
EVT_BUTTON(ID_btnResources, NOHRSCDialog::OnEvt)
//EVT_BUTTON(ID_btnFolder, NOHRSCDialog::OnEvt)
EVT_TEXT(ID_search, NOHRSCDialog::OnEvt)
EVT_BUTTON(wxID_OK, NOHRSCDialog::OnEvt)
EVT_CHECKLISTBOX(ID_chlResources, NOHRSCDialog::OnEvt)
EVT_BUTTON(wxID_HELP, NOHRSCDialog::OnEvt)
//EVT_BUTTON(wxCANCEL, NOHRSCDialog::OnEvt)
EVT_TREELIST_SELECTION_CHANGED(ID_chlResources, NOHRSCDialog::OnTreeListSelChanging)
//EVT_TREELIST_SEL_CHANGING(ID_chlResources, NOHRSCDialog::OnTreeSelChanging)
// TODO: figure out if we can actually bind to these events, right now they do nothing
EVT_DATAVIEW_ITEM_COLLAPSED(ID_chlResources, NOHRSCDialog::OnTreeListItemCollapsed)
EVT_DATAVIEW_ITEM_COLLAPSING(wxEVT_DATAVIEW_ITEM_COLLAPSING, NOHRSCDialog::OnTreeListItemCollapsed)
EVT_BUTTON(ID_btnSaveToFile, NOHRSCDialog::OnSaveToFile)
END_EVENT_TABLE()

NOHRSCDialog::NOHRSCDialog(wxWindow* parent, const wxString& title)
	: wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{

	// Load in the NOHRSC database
	wxString const path = SamApp::GetAppPath();

	m_db = std::unique_ptr<NOHRSCDatabase>(new NOHRSCDatabase(path + "/../snow/stations.csv"));


	// I think we can perhaps prepopulate this with the user's lat lon from the weather file
	// and if not, we can use the same geocoding code to generate the lat lons as needed. 
	m_txtAddress = new wxTextCtrl(this, ID_txtAddress, "39.74, -105.17");// , wxDefaultPosition, wxSize(500, 30));
	// latlon read only now
	m_txtLatLon = new wxTextCtrl(this, ID_txtAddress, "", wxDefaultPosition, wxDefaultSize, wxTE_READONLY);


	m_btnResources = new wxButton(this, ID_btnResources, "Find");
	m_search = new wxSearchCtrl(this, ID_search, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER | wxTE_PROCESS_TAB);
	m_search->SetDescriptiveText("Filter by year");
	m_chlResources = new wxTreeListCtrl(this, ID_chlResources, wxDefaultPosition, wxSize(800, 200)); // populate with returned resources
	m_chlResources->AppendColumn("Station ID", wxCOL_WIDTH_AUTOSIZE);
	m_chlResources->AppendColumn("Distance (km)", wxCOL_WIDTH_AUTOSIZE, wxALIGN_LEFT, wxCOL_SORTABLE | wxCOL_RESIZABLE);
	m_chlResources->AppendColumn("Years available", wxCOL_WIDTH_AUTOSIZE);
	m_chlResources->AppendColumn("Latitude, Longitude", wxCOL_WIDTH_AUTOSIZE);
	m_chlResources->AppendColumn("UTC Offset");
	m_btnSaveToFile = new wxButton(this, ID_btnSaveToFile, "Save to file");

	wxString msg = "Use this window to enter your location and retrieve a list of nearby stations with snow data. ";
	msg += "The data will be downloaded and stored into the snow data array.";

	wxBoxSizer* szStationID = new wxBoxSizer(wxHORIZONTAL);
	szStationID->Add(new wxStaticText(this, wxID_ANY, "Location: "), 0, wxALL, 2);
	szStationID->Add(m_txtAddress, 5, wxALL | wxEXPAND, 2);
	szStationID->Add(m_btnResources, 0, wxALL, 2);

	wxBoxSizer* szLatLon = new wxBoxSizer(wxHORIZONTAL);
	szLatLon->Add(15, 0, 0);
	szLatLon->Add(new wxStaticText(this, wxID_ANY, "Latitude and Longitude:"), 0, wxALL, 2);
	szLatLon->Add(m_txtLatLon, 5, wxALL | wxEXPAND, 1);

	wxBoxSizer* szFilter = new wxBoxSizer(wxHORIZONTAL);

	szFilter->Add(m_search, 0, wxALIGN_CENTER_VERTICAL | wxALL, 2);


	wxBoxSizer* szgrid = new wxBoxSizer(wxVERTICAL);
	szgrid->Add(new wxStaticText(this, wxID_ANY, "2. Select files to download:"), 0, wxALL, 2);
	szgrid->Add(m_chlResources, 10, wxALL | wxEXPAND, 1);
	szgrid->Add(szFilter, 0, wxALL, 1);


	wxBoxSizer* szmain = new wxBoxSizer(wxVERTICAL);
	szmain->Add(new wxStaticText(this, wxID_ANY, msg), 0, wxALL | wxEXPAND, 10);
	szmain->Add(szStationID, 0, wxEXPAND, 1);
	szmain->Add(szLatLon, 0, wxEXPAND, 1);
	szmain->Add(szgrid, 10, wxALL | wxEXPAND, 1);

	// --- Custom button sizer with Save to file, Help, OK, Cancel ---
	wxBoxSizer* szButtons = new wxBoxSizer(wxHORIZONTAL);
	szButtons->Add(m_btnSaveToFile, 0, wxALL, 0);
	szButtons->AddStretchSpacer(1);
	szButtons->Add(new wxButton(this, wxID_OK, "Download Snow Depth Array"), 0, wxLEFT, 10); // Changed label here
	szButtons->Add(new wxButton(this, wxID_HELP), 0, wxLEFT, 10);
	szButtons->Add(new wxButton(this, wxID_CANCEL), 0, wxLEFT, 10);
	szmain->Add(szButtons, 0, wxALL | wxEXPAND, 10);
	// --------------------------------------------------------------

	ResetAll();
	SetSizer(szmain);
	Fit();
	m_txtAddress->SetFocus();
}

void NOHRSCDialog::OnTreeListItemCollapsed(wxDataViewEvent& event)
{
	//wxMessageBox("Item Collapsed", "NOHRSC Download Message", wxOK, this);
	// Check if the currently selected item is underneath the item that was collapsed
	// if so, then we want to try to open the next item to the one that was collapsed, if extant
	// and select its first child
	wxTreeListItem current_selection = m_chlResources->GetSelection();
	wxTreeListItem collapsed_item = static_cast<wxTreeListModelNode *>(event.GetItem().GetID());

	if (m_chlResources->GetItemParent(current_selection) == collapsed_item) {
		wxTreeListItem next_item = m_chlResources->GetNextSibling(collapsed_item);
		if (next_item.IsOk()) {
			m_chlResources->Select(next_item);
			m_chlResources->Expand(next_item);
			m_chlResources->Select(m_chlResources->GetFirstChild(next_item));
		}
	}
	//wxTreeListItem item = event.GetItem();
	//wxTreeListItem parentItem = m_chlResources->GetItemParent(item);
	//if (parentItem.IsOk()) {
	//	m_chlResources->Select(parentItem);
	//}
	//wxMessageBox(m_chlResources->GetItemText(item), "NOHRSC Download Message", wxOK, this);
}


void NOHRSCDialog::OnTreeListSelChanging(wxTreeListEvent& event)
{
	
	wxTreeListItem item = event.GetItem();

	wxTreeListItem root = m_chlResources->GetRootItem();
	wxTreeListItem parentItem = m_chlResources->GetItemParent(item);
	//// We must have children of every item. 
	//// TODO: If one parent is closed while a child is selected, we seem to 
	//// still change to the parent for a diff station. 
	if (root == parentItem) {
		m_chlResources->Select(m_chlResources->GetFirstChild(item));
	}
	wxTreeListItem new_selection = m_chlResources->GetSelection();
	// now, check if the selected item has a child
	wxTreeListItem new_selection_first_child = m_chlResources->GetFirstChild(m_chlResources->GetSelection());

	//wxMessageBox(m_chlResources->GetItemText(new_selection), "NOHRSC Download Message", wxOK, this);
	//if (new_selection_first_child.IsOk()) {
	//	m_chlResources->Select(new_selection_first_child);
	//	//wxMessageBox("firstchildisok.", "NOHRSC Download Message", wxOK, this);
	//}
	//else {
	//	//wxMessageBox("FirstChildNotOk", "NOHRSC Download Message", wxOK, this);
	//}
	//wxTr
	//if (if m_chlResources->isVla(m_chlResources->GetFirstChild(new_selection)))
}

void NOHRSCDialog::OnEvt(wxCommandEvent& e)
{
	switch (e.GetId())
		{
		case wxID_HELP:
			SamApp::ShowHelp("window-reference/win_nohrsc_advanced_download");
			break;
	case ID_btnResources:
		{
			ResetAll();
			GetResources();
			RefreshList();
		}
		break;
	case ID_search:
		{
			FilterItemsByYear(m_search->GetValue());
			RefreshList();
		}
		break;
	case wxID_OK:
	{
		
		wxArrayInt arychecked;
		wxString file_list = "";

		wxTreeListItem selectedItem = m_chlResources->GetSelection();

		if (!selectedItem.IsOk())
		{
			wxMessageBox("Please select a year to download.", "NOHRSC Download Message", wxOK, this);
			break;
		}
		// Check if the item is actually a leaf node
		if (m_chlResources->GetItemParent(selectedItem) == m_chlResources->GetRootItem())
		{
			wxMessageBox("You must select a year to download.", "NOHRSC Download Message", wxOK, this);
			break;
		}
		wxString stationID = m_chlResources->GetItemText(m_chlResources->GetItemParent(selectedItem));
		wxString beginYear = m_chlResources->GetItemText(selectedItem);
		m_coords = m_chlResources->GetItemText(m_chlResources->GetItemParent(selectedItem), 3);
		wxString tz_utc = m_chlResources->GetItemText(m_chlResources->GetItemParent(selectedItem), 4);
		// convert beginyear to int
		if (DownloadNOHRSC(beginYear, stationID, tz_utc)) {
			if (OnSaveToArray(e)) {
				m_year = beginYear;
				m_stationID = stationID;
				UpdateUIMetadata();
				EndModal(wxID_OK);
			}
			else {
				wxMessageBox("Error saving data to snow array. Try again, or choose a different year. ", "NOHRSC Download Message", wxOK, this);
			}
		}
		else {
			wxMessageBox("Error downloading snow data from NOHRSC. The service may be offline. ", "NOHRSC Download Message", wxOK, this);
		}

		break;
		
	}
	break;
	}
}

void NOHRSCDialog::OnSaveToFile(wxCommandEvent& WXUNUSED(event))
{
	// Get selected station/year
	wxTreeListItem selectedItem = m_chlResources->GetSelection();
	if (!selectedItem.IsOk() || m_chlResources->GetItemParent(selectedItem) == m_chlResources->GetRootItem())
	{
		wxMessageBox("Please select a year to download.", "NOHRSC Download Message", wxOK, this);
		return;
	}
	wxString stationID = m_chlResources->GetItemText(m_chlResources->GetItemParent(selectedItem));
	wxString beginYear = m_chlResources->GetItemText(selectedItem);
	wxString tz_utc = m_chlResources->GetItemText(m_chlResources->GetItemParent(selectedItem), 4);
	// File dialog for user to select filename
	wxFileDialog saveFileDialog(
		this,
		_("Save NOHRSC file"),
		"", // No default path
		wxString::Format("%s_%s.csv", stationID, beginYear),
		"CSV files (*.csv)|*.csv|All files (*.*)|*.*",
		wxFD_SAVE | wxFD_OVERWRITE_PROMPT
	);

	if (saveFileDialog.ShowModal() == wxID_CANCEL)
		return;

	// Download and save
	if (!DownloadNOHRSC(beginYear, stationID, tz_utc))
		return;

	// WriteDatatoFile expects just the filename (without path or extension)
	wxFileName fn(saveFileDialog.GetPath());
	wxString fileName = fn.GetName();
	wxString saveDir = fn.GetPath();

	// Write file to the selected directory
	if (!WriteDatatoFile(saveDir + "/" + fileName))
		return;

	wxMessageBox("File saved successfully:\n" + saveFileDialog.GetPath(), "NOHRSC Download", wxOK | wxICON_INFORMATION, this);

	// Close the dialog after successful save
	EndModal(wxID_OK);
}

void NOHRSCDialog::FilterItemsByYear(wxString str_filter) {

	for (auto& station : m_links) {
		station.is_visible = false;
		for (auto & year : station.stationYears) {
			year.is_visible = false;
			if (year.display.Contains(str_filter)) {
				year.is_visible= true;
				station.is_visible = true;
			}
		}
	}
}

void NOHRSCDialog::RefreshList()
{
	m_chlResources->Freeze();
	m_chlResources->DeleteAllItems();
	wxTreeListItem root_itm = m_chlResources->GetRootItem();
	for (const auto & station : m_links) {
		if (station.is_visible) {
			wxTreeListItem ndx = m_chlResources->AppendItem(root_itm, station.display);
			m_chlResources->SetItemText(ndx, 1, station.distance);
			m_chlResources->SetItemText(ndx, 2, station.yearRange());
			m_chlResources->SetItemText(ndx, 3, station.getLatLon());
			m_chlResources->SetItemText(ndx, 4, station.getUTC());
			for (const auto & year : station.stationYears) {
				if (year.is_visible) {
					m_chlResources->AppendItem(ndx, year.display);
				}
			}
		}
	}
	m_chlResources->Thaw();
}

void NOHRSCDialog::ResetAll()
{
	m_txtLatLon->Clear();
	m_chlResources->DeleteAllItems();
	m_links.clear();
}

void NOHRSCDialog::GetResources()
{
	// hit api with address and return available resources
	wxString location = m_txtAddress->GetValue();
	if (location == "")
	{
		wxMessageBox("Type a latitude-longitude pair (lat, lon), street address, or location name and click Find.", "NOHRSC Download Message", wxOK, this);
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
		// use GeoTools::GeocodeGoogle for non-NLR builds and set google_api_key in private.h
		if (!GeoTools::GeocodeDeveloper(location, &lat, &lon))
		{
			wxMessageBox("Failed to geocode address.\n\n" + location, "NOHRSC Download Message", wxOK, this);
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
			wxMessageBox("Type a valid latitude-longitude (lat, lon), street address, or location name.", "NOHRSC Download Message", wxOK, this);
			return;
		}
		else
		{
			parts[0].ToDouble(&lat);
			parts[1].ToDouble(&lon);
			locname = wxString::Format("lat_%.5lf_lon_%.5lf", lat, lon);
		}
	}
	m_txtLatLon->SetValue(wxString::Format("%f,%f", lat, lon));
	m_links = m_db->getNearbyStations(lat, lon);

}


bool NOHRSCDialog::DownloadNOHRSC(wxString beginYear, wxString stationID, wxString tzutc) {
	wxEasyCurl curl;
	m_curl = curl;

	// If tzutc is negative, that means we actually need to start late in the prior year
	int i_tzutc = std::stoi(tzutc.ToStdString());
	int i_beginYear = std::stoi(beginYear.ToStdString());
	int i_endYear = i_beginYear + 1;
	int i_beginHour;
	int i_endHour;
	int i_beginEndMonth = 1;
	if (i_tzutc > 0) {
		i_beginYear -= 1;
		i_endYear -= 1;
		i_beginHour = 24 - i_tzutc;
		i_beginEndMonth = 12;
	}
	else {
		i_beginHour = -1*i_tzutc;
	}
	i_endHour = i_beginHour - 1;

	// url data=11
	wxString url = SamApp::WebApi("noaa_snow");
	url.Replace("<STATION>", wxString::Format("%s", stationID));
	url.Replace("<BEGINYEAR>", wxString::Format("%d", i_beginYear));
	url.Replace("<BEGINMONTH>", wxString::Format("%d", i_beginEndMonth));
	url.Replace("<BEGINHOUR>", wxString::Format("%d", i_beginHour));
	url.Replace("<ENDYEAR>", wxString::Format("%d", i_endYear));
	url.Replace("<ENDMONTH>", wxString::Format("%d", i_beginEndMonth));
	url.Replace("<ENDHOUR>", wxString::Format("%d", i_endHour));
	url.Replace("<DATA>", wxString::Format("%d", 11));

	// humanUrl data=0
	wxString humanUrl = SamApp::WebApi("noaa_snow");
	humanUrl.Replace("<STATION>", wxString::Format("%s", stationID));
	humanUrl.Replace("<BEGINYEAR>", wxString::Format("%d", i_beginYear));
	humanUrl.Replace("<BEGINMONTH>", wxString::Format("%d", i_beginEndMonth));
	humanUrl.Replace("<BEGINHOUR>", wxString::Format("%d", i_beginHour));
	humanUrl.Replace("<ENDYEAR>", wxString::Format("%d", i_endYear));
	humanUrl.Replace("<ENDMONTH>", wxString::Format("%d", i_beginEndMonth));
	humanUrl.Replace("<ENDHOUR>", wxString::Format("%d", i_endHour));
	humanUrl.Replace("<DATA>", wxString::Format("%d", 0));

	m_url = humanUrl;

	bool ok = m_curl.Get(url + "&utc=false", "Download NOHRSC data", this);
	if (!ok)
	{
		wxMessageBox("NOHRSC Data Query failed.\n\nThere may be a problem with your internet connection,\nor the NOHRSC web service may be down.", "NOHRSC Download Message", wxOK, this);
		return false;
	}
	else if (m_curl.GetDataAsString().Length() < 1000)
	{
		wxMessageBox("Snow data not available.\n\n" + url + "\n\n" + m_curl.GetDataAsString(), "NOHRSC Download Message", wxOK, this);
		return false;
	}
	return true;
}	

bool NOHRSCDialog::WriteDatatoFile(wxString filePath) {
	// filePath should be the full path (including filename, no extension)
	filePath += ".csv";
	// if there is data present, then write it
	if (!m_curl.WriteDataToFile(filePath)) {
		wxMessageBox("Failed to write file.\n\n" + filePath, "NOHRSC Download Message", wxOK, this);
		return false;
	}
	return true;
}


// Source: https://docs.wxwidgets.org/3.2/classwx_file_dialog.html
bool NOHRSCDialog::OnSaveAs(wxCommandEvent& WXUNUSED(event))
{
	wxFileDialog
		saveFileDialog(this, _("Save XYZ file"), "", "",
			"XYZ files (*.xyz)|*.xyz", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

	if (saveFileDialog.ShowModal() == wxID_CANCEL)
		return false;     // the user changed idea...

	// save the current contents in the file;
	// this can be done with e.g. wxWidgets output streams:
	wxFileOutputStream output_stream(saveFileDialog.GetPath());
	if (!output_stream.IsOk())
	{
		wxLogError("Cannot save current contents in file '%s'.", saveFileDialog.GetPath());
		return false;
	}
	return true;
}

bool NOHRSCDialog::OnSaveToArray(wxCommandEvent& WXUNUSED(event)) {
	Case* c = SamApp::Window()->GetCurrentCase();
	VarValue* vv = c->Values(0).Get("snow_array");
	

	wxCSVData csvData;
	bool readSuccess = csvData.ReadString(m_curl.GetDataAsString());
	if (!readSuccess) {
		return false;
	}

	std::vector<double> l;
	l.reserve(8760);
	// Loop to obtain the first value not in an assimilation period
	// This is only relevant if the first value of the series is an assimilated value
	// which is very unlikely. 
	// if it is, we just feed backward the first non assimilated value into all the assimilated values

	wxString nonAssim;
	double dNonAssim;
	for (size_t i = 0; i < 8760; i++) {
		wxString isAssim = csvData.Get(i + 1, 8);
		if (isAssim != "Assim") {
			nonAssim = csvData.Get(i + 1, 3);
			nonAssim.ToDouble(&dNonAssim);
			break;
		}
	}

	// main loop to actuall
	for (size_t i = 0; i < 8760; i++) {
		wxString isAssim = csvData.Get(i + 1, 8);
		if (isAssim != "Assim") {
			// Update the most recent non assim value with the current non assim value
			nonAssim = csvData.Get(i + 1, 3);
			nonAssim.ToDouble(&dNonAssim);
		}
		l.push_back(dNonAssim);
	}
	if (vv) {
		vv->Set(l);
	}
	c->VariableChanged("snow_array",0);


	return true;
}

void NOHRSCDialog::UpdateUIMetadata() {
	Case* c = SamApp::Window()->GetCurrentCase();
	VarValue* year = c->Values(0).Get("nohrsc_year");
	VarValue* stID = c->Values(0).Get("nohrsc_station_id");
	VarValue* coords = c->Values(0).Get("nohrsc_coords");
	year->Set(m_year);
	stID->Set(m_stationID);
	coords->Set(m_coords);
	c->VariableChanged("nohrsc_year",0);
	c->VariableChanged("nohrsc_coords", 0);
	c->VariableChanged("nohrsc_station_id", 0);

}