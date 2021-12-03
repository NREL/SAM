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

#ifndef __uncertainties_h
#define __uncertainties_h

#include <wx/datstrm.h>
#include <wx/arrstr.h>
#include <wx/panel.h>
#include <wx/splitter.h>

#include <wex/plot/plplotctrl.h>


// forwards
class wxMetroButton;
class wxRadioChoice;
class wxScrolledWindow;
class wxExtTextCtrl;
class wxCheckListBox;
class wxSnapLayout;
class wxDVSelectionListCtrl;
class wxSlider;
class wxCheckBox;
class wxChoice;
class wxSearchCtrl;
class MetricsTable;

class Case;
class Simulation;

class Uncertainties
{
public:

	static std::vector<wxColour> &Colours();

	Uncertainties();

	void Copy( Uncertainties *gr );
	bool SameAs( Uncertainties *gr );

	bool Read( wxInputStream &is );
	bool Write( wxOutputStream &os );

	enum { BAR, STACKED, LINE, SCATTER, CONTOUR };
	int Type;

	wxArrayString Y;
	
	wxString XLabel;
	wxString YLabel;
	wxString Title;

	bool ShowXValues;
	bool ShowYValues;
	bool ShowLegend;
	int LegendPos;

	int Size;
	bool CoarseGrid;
	bool FineGrid;
	double YMin, YMax;
	wxString Notes;
	double FontScale;
	int FontFace; // 0=system,1=arial,2=times,3=metro	
};

BEGIN_DECLARE_EVENT_TYPES()
	DECLARE_EVENT_TYPE( wxEVT_Uncertainties_SELECT, 0)
END_DECLARE_EVENT_TYPES()

#define EVT_Uncertainties_SELECT(id, func) EVT_COMMAND(id, wxEVT_Uncertainties_SELECT, func)

class UncertaintiesCtrl : public wxPLPlotCtrl
{
public:
	UncertaintiesCtrl( wxWindow *parent, int id );
	
	// multiple variables over a single simulation
	int Display(Simulation *sim, Uncertainties &g);
	// multiple variables over a single simulation
	int Figure2(Simulation *sim);
	// multiple variables over a single simulation
	int Figure5(Simulation *sim);
	// multiple variables over a single simulation
	int Figure10(Simulation *sim);

	// one variable over multiple simulations
	int Display(std::vector<Simulation *> sims, Uncertainties &g);

	int IntersectionLines(const double &x_min, const double &x_max, const double &y_min, const double  &y_max, const wxString &label, const wxPLOutputDevice::Style &style);


	void SetUncertainties( const Uncertainties &g ) { m_g = g; }
	Uncertainties GetUncertainties() { return m_g; }
protected:
	Simulation *m_s;
	Uncertainties m_g;

	void OnLeftDown( wxMouseEvent & );

	DECLARE_EVENT_TABLE();
};

BEGIN_DECLARE_EVENT_TYPES()
	DECLARE_EVENT_TYPE( wxEVT_Uncertainties_PROPERTY_CHANGE, 0)
END_DECLARE_EVENT_TYPES()

#define EVT_Uncertainties_PROPERTY_CHANGE(id, func) EVT_COMMAND(id, wxEVT_Uncertainties_PROPERTY_CHANGE, func)

class UncertaintiesProperties : public wxPanel
{
public:
	UncertaintiesProperties( wxWindow *parent, int id );

	void SetupVariables( Simulation *sim );
	void Clear();

	void Set( const Uncertainties &g );
	void Get( Uncertainties &g );

private:
	wxArrayString m_names;
	wxArrayString m_selected;
	Simulation *m_sim;

	wxRadioChoice *m_type;
	wxExtTextCtrl *m_title;
	wxDVSelectionListCtrl *m_Y;

	wxSearchCtrl *m_srch;

	wxExtTextCtrl *m_xlabel;
	wxExtTextCtrl *m_ylabel;
	wxSlider *m_scale;
	wxSlider *m_size;
	wxCheckBox *m_coarse, *m_fine;
	wxCheckBox *m_showLegend;
	wxChoice *m_legendPos;
	wxChoice *m_font;
	
	void OnEdit(wxCommandEvent &);
	void OnSearch(wxCommandEvent &);
	void OnSlider(wxScrollEvent &);
	void SendChangeEvent();

	DECLARE_EVENT_TABLE();

};

class UncertaintiesViewer : public wxPanel
{
public:
	UncertaintiesViewer( wxWindow *parent );
	
	void Setup( Simulation *sim );
	
	void SetUncertainties( std::vector<Uncertainties> &gl );
	void GetUncertainties( std::vector<Uncertainties> &gl );

private:
    // display table of standard deviations
    void DisplayStdDevs();

	// display a set of probability of exceedances in a table
    void DisplayProbOfExceedances();

	UncertaintiesCtrl *m_current;
    MetricsTable *m_stddevTable;
    MetricsTable *m_exceedanceTable;
	wxSnapLayout *m_layout;
	std::vector<Uncertainties> m_Uncertainties;

	Simulation *m_sim;


	DECLARE_EVENT_TABLE();
};

#endif

