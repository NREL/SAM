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

#ifndef __WaveDownloadDialog_h
#define __WaveDownloadDialog_h

#include <wx/dialog.h>


class wxComboBox;
class wxCheckListBox;
class wxRadioButton;
class wxComboBox;
class wxButton;
class wxTextCtrl;
class wxCheckbox;
class wxSearchCtrl;
class wxArrayString;

class WaveDownloadDialog : public wxDialog
{

public:
	WaveDownloadDialog(wxWindow *parent, const wxString &title);
	wxString &GetWeatherFile() {
		return m_weatherFile;
	};
	wxString &GetWeatherFolder() {
		return m_weatherFolder;
	};
	wxString &GetAddFolder() {
		return m_addFolder;
	};

    bool IsAddressMode();
    wxString GetEndpoint();
    bool IsSingleYear();
    wxString GetAddress();
    double GetLatitude();
    double GetLongitude();
    wxString GetYear();
    wxArrayString GetMultiYear();

	struct LinkInfo
	{
		wxString name; // dataset - e.g. psm
		wxString displayName;
		wxString year; // number or "tmy"
		wxString URL;
		wxString interval; // 30 or 60 
		wxString location; // lat and lon
		wxString display;
		wxString attributes; // limit column and file size to SAM specific per NSRDB
		bool is_selected;
		bool is_visible;
		LinkInfo(wxString &_n, wxString &_dn, wxString &_y, wxString &_u, wxString &_i, wxString &_l, wxString &_a)
			: name(_n), displayName(_dn), year(_y), URL(_u), interval(_i), location(_l), attributes(_a)
		{
			display = location + "_" + name + "_" + interval + "_" + year;
			is_visible = true;
			is_selected = false;
		}
		// for sorting
		bool operator < (const LinkInfo &li) const
		{
			// Sort per name (psm, mts3, mts2, mts1, suny) and year (tmy, yyyy) and interval 60, 30
			if (name == li.name)
			{
				if (year == li.year)
					return ( atoi(interval.c_str()) > atoi(li.interval.c_str()));
				else if (year.Contains("tmy"))
					return true;
				else if (li.year.Contains("tmy"))
					return false;
				else
					return ( atoi(year.c_str()) > atoi(li.year.c_str()));
			}
			else if (name.Lower() == "psm")
				return true;
			else if (li.name.Lower() == "psm")
				return false;
			else if (name.Lower() == "mts3")
				return true;
			else if (li.name.Lower() == "mts3")
				return false;
			// not sure why mts2-tmy and mts2 are two separate types.
			else if (name.Lower() == "mts2-tmy")
				return true;
			else if (li.name.Lower() == "mts2-tmy")
				return false;
			else if (name.Lower() == "mts2")
				return true;
			else if (li.name.Lower() == "mts2")
				return false;
			else if (name.Lower() == "mts1")
				return true;
			else if (li.name.Lower() == "mts1")
				return false;
			else if (name.Lower() == "suny")
				return true;
			else if (li.name.Lower() == "suny")
				return false;
			else
				return true;
		}
	};


private:
	void OnEvt(wxCommandEvent &);

	void GetResources();
	void RefreshList(size_t );
	size_t SelectItems( wxString, wxCheckBox * );
    wxRadioButton* radSingleYear;
    wxRadioButton* radMultiYear;
    wxRadioButton* radAllYear;
    wxRadioButton* radHawaii;
    wxTextCtrl* txtAddress, * txtLat, * txtLon;
    wxTextCtrl* txtSingleYear, * txtStartYear, * txtEndYear;
    wxListBox* lstYears;
    wxCheckBox* all_years_chk;
    wxComboBox* cboEndpoint;
    wxComboBox* cboSingleYear;
	std::vector<LinkInfo> m_links;
	wxString m_weatherFile;
	wxString m_weatherFolder = ::wxGetHomeDir() + "/SAM Downloaded Weather Files";;
	wxString m_addFolder;
	wxComboBox *m_cboWeatherFile;
	wxCheckListBox *m_chlResources;
	wxButton *m_btnSelectAll, *m_btnClearAll, *m_btnSelectFiltered, *m_btnShowSelected, *m_btnShowAll, *m_btnResources, *m_btnFolder;
	wxTextCtrl *m_txtFolder;
	wxTextCtrl *m_txtAddress;
	wxSearchCtrl *m_search;

	DECLARE_EVENT_TABLE()
};
#endif
