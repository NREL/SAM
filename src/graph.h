#ifndef __graph_h
#define __graph_h

#include <wx/datstrm.h>
#include <wx/arrstr.h>
#include <wx/panel.h>

#include <wex/plot/plplotctrl.h>

// forwards
class wxRadioChoice;
class wxScrolledWindow;
class Case;
class DataProvider;
class wxExtTextCtrl;
class wxCheckListBox;

class Graph
{
public:
	Graph();

	void Copy( Graph *gr );
	bool SameAs( Graph *gr );

	bool Read( wxInputStream &is );
	bool Write( wxOutputStream &os );

	enum { BAR, STACKED, LINE, SCATTER };
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
};

class GraphCtrl : public wxPLPlotCtrl
{
public:
	GraphCtrl( wxWindow *parent, int id );
	void Display( DataProvider *data, Graph &g );

	Graph &GetGraph() { return m_g; }
protected:
	DataProvider *m_d;
	Graph m_g;
};

class GraphViewer : public wxPanel
{
public:
	GraphViewer( wxWindow *parent );
	virtual ~GraphViewer();

	void Setup( Case *c, DataProvider *dp );
	void Clear();

private:
	wxArrayString m_names, m_labels;

	wxRadioChoice *m_type;
	wxExtTextCtrl *m_title;
	wxCheckListBox *m_Y;
	wxExtTextCtrl *m_xlabel;
	wxExtTextCtrl *m_ylabel;
	wxSlider *m_scale;
	wxSlider *m_size;
	wxCheckBox *m_coarse, *m_fine;

	GraphCtrl *m_graph;

	void UpdateCurrentGraph();

	void OnEdit( wxCommandEvent & );
	void OnSlider( wxScrollEvent & );

	wxScrolledWindow *m_scroll;
	std::vector<GraphCtrl*> m_graphs;

	Case *m_case;
	DataProvider *m_data;


	DECLARE_EVENT_TABLE();
};

#endif

