#ifndef __graph_h
#define __graph_h

#include <wx/datstrm.h>
#include <wx/arrstr.h>
#include <wx/panel.h>
#include <wx/splitter.h>

#include <wex/plot/plplotctrl.h>

// forwards
class wxRadioChoice;
class wxScrolledWindow;
class Case;
class DataProvider;
class wxExtTextCtrl;
class wxCheckListBox;
class wxSnapLayout;

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

BEGIN_DECLARE_EVENT_TYPES()
	DECLARE_EVENT_TYPE( wxEVT_GRAPH_SELECT, 0)
END_DECLARE_EVENT_TYPES()

#define EVT_GRAPH_SELECT(id, func) EVT_COMMAND(id, wxEVT_GRAPH_SELECT, func)

class GraphCtrl : public wxPLPlotCtrl
{
public:
	GraphCtrl( wxWindow *parent, int id );
	void Display( DataProvider *data, Graph &g );

	Graph &GetGraph() { return m_g; }
protected:
	DataProvider *m_d;
	Graph m_g;

	void OnLeftDown( wxMouseEvent & );

	DECLARE_EVENT_TABLE();
};

BEGIN_DECLARE_EVENT_TYPES()
	DECLARE_EVENT_TYPE( wxEVT_GRAPH_PROPERTY_CHANGE, 0)
END_DECLARE_EVENT_TYPES()

#define EVT_GRAPH_PROPERTY_CHANGE(id, func) EVT_COMMAND(id, wxEVT_GRAPH_PROPERTY_CHANGE, func)

class GraphProperties : public wxPanel
{
public:
	GraphProperties( wxWindow *parent, int id );

	void SetupVariables( DataProvider *dp );
	void Clear();

	void Set( const Graph &g );
	void Get( Graph &g );

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
	
	void OnEdit( wxCommandEvent & );
	void OnSlider( wxScrollEvent & );
	void SendChangeEvent();

	DECLARE_EVENT_TABLE();

};

class GraphViewer : public wxSplitterWindow
{
public:
	GraphViewer( wxWindow *parent );
	
	void Setup( Case *c, DataProvider *dp );
	
	GraphCtrl *CreateNewGraph();
	void DeleteGraph( GraphCtrl * );

	GraphCtrl *Current();
private:
	void UpdateGraph();
	void UpdateProperties();
	void OnCommand( wxCommandEvent & );
	void OnGraphSelect( wxCommandEvent & );
	void SetCurrent( GraphCtrl *gc );

	GraphCtrl *m_current;
	GraphProperties *m_props;
	wxSnapLayout *m_layout;
	std::vector<GraphCtrl*> m_graphs;

	Case *m_case;
	DataProvider *m_data;


	DECLARE_EVENT_TABLE();
};

#endif

