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


#ifndef __NOHRSCDialog_h
#define __NOHRSCDialog_h

#include <wx/dialog.h>
#include <wx/treelist.h>
#include <wex/csv.h>
#include <wx/wfstream.h>
#include <numeric>
#include <memory>
class wxComboBox;
class wxCheckListBox;
class wxButton;
class wxTextCtrl;
class wxCheckbox;
class wxSearchCtrl;
class wxTreeListCtrl;




class NOHRSCDialog : public wxDialog
{

public:
	NOHRSCDialog(wxWindow* parent, const wxString& title);
	//wxString& GetWeatherFile() {
	//	return m_weatherFile;
	//};
	//wxString& GetWeatherFolder() {
	//	return m_weatherFolder;
	//};
	//wxString& GetAddFolder() {
	//	return m_addFolder;
	//};
	wxString& GetNOHRSCYear() {
		return m_year;
	}

	wxString& GetNOHRSCURL() {
		return m_url;
	}

	wxString& GetNOHRSCstationID() {
		return m_stationID;
	}


	struct StationYear
	{
		wxString stationID; // dataset - e.g. psm
		int year; // number or "tmy"
		// starts at 0 (0 indicates only baseYear
		wxString URL;
		wxString display;
		//bool is_selected;
		bool is_visible;
		StationYear(wxString& stationID, int year)
			: stationID(stationID), year(year)
		{
			display = wxString::Format(wxT("%d"), year);
			is_visible = true;
			//is_selected = false;
			URL = wxString::Format(wxT("https://www.nohrsc.noaa.gov/interactive/html/graph.html?station=%s&w=600&h=400&o=a&uc=0&by=%d&bm=1&bd=1&bh=6&ey=%d&em=1&ed=1&eh=5&data=11&units=1&region=us"), stationID, year, year + 1);
		}
	};

	struct StationRoot
	{
		wxString stationID; // dataset - e.g. psm
		//wxString /*displayName*/;
		int baseYear; // number or "tmy"
		int yearCount; // number of years after baseyear that the station has data
		// starts at 0 (0 indicates only baseYear
		wxString URL;
		//wxString interval; // 30 or 60 
		//wxString location; // lat and lon
		wxString display;
		wxString distance;
		wxString lat;
		wxString lon;
		wxString utc;
		//bool is_selected;
		bool is_visible;
		std::vector<StationYear> stationYears;
		StationRoot(wxString& stationID, int baseYear, int yearCount, float fDistance, wxString& lat, wxString& lon, wxString& utc)
			: stationID(stationID), baseYear(baseYear), yearCount(yearCount), lat(lat), lon(lon), utc(utc)
		{
			distance = wxString::Format(wxT("%.2f"), fDistance);
			display = stationID;
			is_visible = true;
			//is_selected = false;
			//https://www.nohrsc.noaa.gov/interactive/html/graph.html?station=KDEN&w=600&h=400&o=a&uc=0&by=2024&bm=1&bd=1&bh=6&ey=2025&em=1&ed=1&eh=5&data=11&units=1&region=us
			URL = wxString::Format(wxT("https://www.nohrsc.noaa.gov/interactive/html/graph.html?station=%s&w=600&h=400&o=a&uc=0&by=%d&bm=1&bd=1&bh=6&ey=%d&em=1&ed=1&eh=5&data=11&units=1&region=us"), stationID, baseYear, baseYear + 1);
			for (int i = 0; i < yearCount; i++)
			{
				stationYears.push_back((StationYear(stationID, baseYear + i)));
			}
		}
		StationRoot(StationRoot&& other) noexcept = default;
		StationRoot& operator=(StationRoot&& other) noexcept = default;
		StationRoot(const StationRoot&) = delete;
		StationRoot& operator=(const StationRoot&) = delete;

		wxString yearRange() const {
			if (yearCount == 0)
				return wxString::Format(wxT("%d"), baseYear);
			else
				return wxString::Format(wxT("%d-%d"), baseYear, baseYear + yearCount - 1);
		}

		wxString getLatLon() const {
			return wxString::Format(wxT("%s, %s"), lat, lon);
		}

		wxString getUTC() const {
			return utc;
		}
		// for sorting
		bool operator < (const StationRoot& li) const
		{

			// Sort on distance (lowest first) then alpha (lowest first)
			if (distance == li.distance)
			{
				return (stationID.Lower() < li.stationID.Lower());
			}
			else
				return (distance < li.distance);
		}
	};


	class NOHRSCDatabase {

		public:

			NOHRSCDatabase(const wxString& csvFilePath) {
				//wxFileInputStream in(csvFilePath);
				m_csvData.ReadFile(csvFilePath);
			}

			std::vector<NOHRSCDialog::StationRoot> getNearbyStations(float lat, float lon) {
				std::vector<NOHRSCDialog::StationRoot> stations;

				stations.reserve(50);

				std::vector<size_t> idxVector(m_csvData.NumRows() - 1);
				std::vector<float> distances(m_csvData.NumRows() - 1);
				std::iota(idxVector.begin(), idxVector.end(), 0); // Fill with 1, 2, ..., m_csvData.NumRows()-1

				for (size_t i = 1; i < m_csvData.NumRows(); i++) {
					//wxString stationID = m_csvData.Get(i, 0);
					wxString stlon = m_csvData.Get(i, 1);
					wxString stlat = m_csvData.Get(i, 2);
					//wxString baseYear = m_csvData.Get(i, 3);
					//wxString yearCount = m_csvData.Get(i, 4);
					//wxString startDate = m_csvData.Get(i, 3);

					float flat, flon;
					flat = wxAtof(stlat);
					flon = wxAtof(stlon);
					// Convert to float
					//float hardcodedLat, hardcodedLon;

					float fDistance = haversineDistance(lat, lon, flat, flon);
					// Create StationRoot object and add to vector
					distances[i - 1] = (fDistance);
				}

				// Sort the indices by distance
				std::sort(idxVector.begin(), idxVector.end(), [&distances](size_t a, size_t b) {
					return distances[a] < distances[b];
					});

				for (size_t i = 0; i < 50 && i < distances.size(); i++) {
					size_t idx = idxVector[i];
					wxString stationID = m_csvData.Get(idx + 1, 0);
					wxString baseYear = m_csvData.Get(idx + 1, 3);
					wxString yearCount = m_csvData.Get(idx + 1, 4);
					wxString stLon = m_csvData.Get(idx+1, 1);
					wxString stLat = m_csvData.Get(idx+1, 2);
					wxString utc = m_csvData.Get(idx + 1, 5);
					float fDistance = distances[idx];
					// Create StationRoot object and add to vector
					stations.emplace_back(stationID, wxAtoi(baseYear), wxAtoi(yearCount), fDistance, stLat, stLon, utc);
				}
				return stations;
			}

		private:
			wxCSVData m_csvData;

			float haversineDistance(float lat1, float lon1, float lat2, float lon2)
			{
				// Haversine formula to calculate distance between two lat/lon points
				const double R = 6371e3; // metres
				double phi1 = lat1 * M_PI / 180;
				double phi2 = lat2 * M_PI / 180;
				double delta_phi = (lat2 - lat1) * M_PI / 180;
				double delta_lambda = (lon2 - lon1) * M_PI / 180;
				double a = sin(delta_phi / 2) * sin(delta_phi / 2) +
					cos(phi1) * cos(phi2) *
					sin(delta_lambda / 2) * sin(delta_lambda / 2);
				double c = 2 * atan2(sqrt(a), sqrt(1 - a));
				return R * c / 1000; // in metres
			}

		};




	private:
		void OnEvt(wxCommandEvent&);
		void OnTreeListItemCollapsed(wxDataViewEvent& event);
		void OnTreeListSelChanging(wxTreeListEvent& event);
		void GetResources();
		void RefreshList();
		void ResetAll();
		size_t SelectItems(wxString, wxCheckBox*);
		void FilterItemsByYear(wxString);
		bool DownloadNOHRSC(wxString, wxString, wxString);
		bool WriteDatatoFile(wxString);
		bool OnSaveAs(wxCommandEvent& WXUNUSED(event));
		void OnSaveToFile(wxCommandEvent& event); // Add handler declaration
		bool OnSaveToArray(wxCommandEvent& WXUNUSED(event));
		void UpdateUIMetadata();

		std::vector<StationRoot> m_links;
		wxString m_weatherFile;
		wxString m_weatherFolder;
		wxString m_addFolder;

		wxString m_stationID;
		wxString m_url;
		wxString m_year;
		wxString m_coords;


		wxComboBox* m_cboWeatherFile;
		//wxCheckListBox* m_chlResources;
		wxTreeListCtrl* m_chlResources;
		//wxCheckBox* m_chk60, * m_chk30, * m_chk15, * m_chk10, * m_chk5, * m_chkTmy, * m_chkTgy, * m_chkTdy;
		//wxButton* m_btnSelectAll, * m_btnClearAll, * m_btnSelectFiltered, * m_btnShowSelected, * m_btnShowAll, * m_btnResources, * m_btnFolder;
		wxButton* m_btnResources, * m_btnFolder;
		wxButton* m_btnSaveToFile; // Add member for the new button
		wxTextCtrl* m_txtFolder;
		wxTextCtrl* m_txtAddress;
		wxTextCtrl* m_txtLatLon;
		wxSearchCtrl* m_search;
		wxEasyCurl m_curl;
		std::unique_ptr<NOHRSCDatabase> m_db;
		DECLARE_EVENT_TABLE()
	
};





#endif
