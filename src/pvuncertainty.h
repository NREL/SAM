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

#ifndef __PVUncertainty_h
#define __PVUncertainty_h

#include <wx/panel.h>

class wxTextCtrl;
class wxSnapLayout;
class Case;
class wxExtGridCtrl;
class wxPLPlotCtrl;
class wxNumericCtrl;
class wxStaticText;
class AFToolTipCtrl;
class wxDVPnCdfCtrl;

class UncertaintySource : public wxPanel
{
public:
	UncertaintySource(wxWindow* parent, std::string& source_label, std::string& source_info, std::string& initial_value);
	void OnEdit(wxCommandEvent& evt);
	void PopulateDistInfoText(int i, InputDistDialog& dlg);
	void OnToolTip(wxCommandEvent& evt);

	// add setter and getter
	wxString m_infoDistDialog;


private:
	wxStaticText* m_source;
	wxTextCtrl* m_distInfo;
	AFToolTipCtrl* m_tt;
	std::string m_label; // information icon - tool tip
	std::string m_info; // information icon - tool tip
	DECLARE_EVENT_TABLE();

};


class PVUncertaintyForm : public wxPanel
{
public:
	PVUncertaintyForm( wxWindow *parent, Case *cc );
    ~PVUncertaintyForm();

protected:

	void OnCopyTable(wxCommandEvent&);
	void OnSimulate(wxCommandEvent&);
	void OnSetPValue(wxCommandEvent&);
	void OnSelectFolder( wxCommandEvent & );
//	void GetTextData(wxString& dat, char sep, bool withHeader = true);

private:
	Case *m_case;
	wxTextCtrl *m_folder;
	StochasticData m_sd; // this will need to be persisted separately from case->Stochastic

	wxDVPnCdfCtrl* m_pnCdfAll; // overall 
	wxDVPnCdfCtrl* m_pnCdfIV; // interannual variability 
	wxDVPnCdfCtrl* m_pnCdfUS; // uncertainty sources 

	std::vector< UncertaintySource* > m_uncertaintySources;
    //std::vector<double> m_uncertaintySourcesFactor;
    //std::vector<wxDVTimeSeriesDataSet*> m_tsDataSets;
	double m_pUS[10000]; // testing uncertainty sources
	double m_pIV[100]; // testing interannual variablility - assumes 100 or less weather files
	double m_pAll[1000000]; // testing overall combination
//	wxExtGridCtrl *m_grid;
	std::vector<wxWindow*> m_graphs;
	wxNumericCtrl *m_puser;
	wxSnapLayout *m_layout;

	void SetPValue(double pValue);

	DECLARE_EVENT_TABLE();
};


#endif
