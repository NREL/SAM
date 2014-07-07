#include <wx/panel.h>
#include <wx/button.h>
#include <wx/busyinfo.h>
#include <wx/clipbrd.h>

#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>
#include <wex/plot/pllineplot.h>
#include <wex/dview/dvtimeseriesctrl.h>
#include <wex/dview/dvtimeseriesdataset.h>
#include <wex/metro.h>
#include <wex/utils.h>
#include <wex/snaplay.h>
#include <wex/ole/excelauto.h>

#include "parametric.h"
#include "main.h"
#include "casewin.h"
#include "variablegrid.h"


// TODO repeated from Results.cpp - need to combine into common location
class TimeSeries8760 : public wxDVTimeSeriesDataSet
{
	float *m_pdata;
	wxString m_label, m_units;
public:
	TimeSeries8760(float *p, const wxString &label, const wxString &units)
		: wxDVTimeSeriesDataSet(), m_pdata(p), m_label(label), m_units(units) { }
	virtual wxRealPoint At(size_t i) const
	{
		if (i < 8760) return wxRealPoint(i, m_pdata[i]);
		else return wxRealPoint(0, 0);
	}
	virtual size_t Length() const { return 8760; }
	virtual double GetTimeStep() const { return 1.0; }
	virtual double GetOffset() const { return 0.0; }
	virtual wxString GetSeriesTitle() const { return m_label; }
	virtual wxString GetUnits() const { return m_units; }
	virtual void SetDataValue(size_t i, double newYValue) { /* nothing to do */ }
};




ParametricData::ParametricData( Case *c )
	: m_case( c )
{
}

ParametricData::~ParametricData()
{
	ClearRuns();
}

void ParametricData::Copy(ParametricData &rhs)
{
	ClearRuns();
	Setup = rhs.Setup;
	for (size_t i = 0; i < rhs.Runs.size(); i++)
	{
		Simulation *s = new Simulation(m_case, rhs.Runs[i]->GetName());
		Runs.push_back(s);
	}
}

void ParametricData::ClearRuns()
{
	for( size_t i=0;i<Runs.size();i++ )
		delete Runs[i];
	Runs.clear();
}

int ParametricData::FindSetup(wxString &name)
{
	int ndx = -1;
	for (size_t i = 0; i < Setup.size(); i++)
	{
		if (Setup[i].Name == name)
		{
			ndx = (int)i;
			break;
		}
	}
	return ndx;
}

bool ParametricData::RemoveSetup(wxString &name)
{
	bool removed = false;
	int ndx = FindSetup(name);
	if (ndx > -1)
	{
		int sz = Setup.size();
		Setup.erase(Setup.begin() + ndx);
		removed = (Setup.size() == size_t(sz - 1));
	}
	return removed;
}

void ParametricData::Write( wxOutputStream &_O )
{
	wxDataOutputStream out( _O );

	out.Write8( 0x2b );
	out.Write8( 1 );

	out.Write32( Setup.size() );
	for( size_t i=0;i<Setup.size();i++ )
	{
		out.WriteString( Setup[i].Name );
		out.Write32( Setup[i].Values.size() );
		for( size_t k=0;k<Setup[i].Values.size();k++ )
			Setup[i].Values[k].Write( _O );
	}

	out.Write32( Runs.size() );
	for( size_t i=0;i<Runs.size();i++ )
		Runs[i]->Write( _O );

	out.Write8( 0x2b );
}

bool ParametricData::Read( wxInputStream &_I )
{
	wxDataInputStream in( _I );

	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8();

	size_t n = in.Read32();
	Setup.clear();
	for( size_t i=0;i<n;i++ )
	{
		Var x;
		x.Name = in.ReadString();
		size_t m = in.Read32();
		for( size_t k=0;k<m;k++ )
		{
			VarValue vv;
			vv.Read( _I );
			x.Values.push_back( vv );
		}
		Setup.push_back( x );
	}

	n = in.Read32();
	ClearRuns();
	for( size_t i=0;i<n;i++ )
	{
		Simulation *sim = new Simulation( m_case, "Parametric Run" );
		sim->Read( _I );
		Runs.push_back( sim );
	}

	return in.Read8() == code;
}
////////////////////////////////////////////////////////////////////////////////////////////////

//BEGIN_EVENT_TABLE(ParametricGrid, wxGrid)
BEGIN_EVENT_TABLE(ParametricGrid, wxExtGridCtrl)
EVT_GRID_CELL_LEFT_CLICK(ParametricGrid::OnLeftClick)
END_EVENT_TABLE()


ParametricGrid::ParametricGrid(wxWindow *parent, wxWindowID id, const wxPoint &pos, const wxSize &size, long style, const wxString &name)
//: wxGrid(parent, id, pos, size, style, name)
: wxExtGridCtrl(parent, id, pos, size)
{
}

ParametricGrid::~ParametricGrid()
{
}

void ParametricGrid::OnLeftClick(wxGridEvent &evt)
{
	SetGridCursor(evt.GetRow(), evt.GetCol());
	evt.Skip();
}






////////////////////////////////////////////////////////////////////////////////////////////////

enum { ID_SELECT_INPUTS, ID_SELECT_OUTPUTS, ID_NUMRUNS, ID_RUN, ID_CLEAR, ID_GRID, ID_INPUTMENU_FILL_DOWN_SEQUENCE, ID_INPUTMENU_FILL_DOWN_ONE_VALUE, ID_INPUTMENU_FILL_DOWN_EVENLY, ID_OUTPUTMENU_ADD_PLOT, ID_OUTPUTMENU_REMOVE_PLOT, ID_OUTPUTMENU_SHOW_DATA, ID_OUTPUTMENU_CLIPBOARD, ID_OUTPUTMENU_CSV, ID_OUTPUTMENU_EXCEL, ID_SHOW_ALL_INPUTS };



BEGIN_EVENT_TABLE(ParametricViewer, wxPanel)
EVT_BUTTON(ID_SELECT_INPUTS, ParametricViewer::OnCommand)
EVT_BUTTON(ID_SELECT_OUTPUTS, ParametricViewer::OnCommand)
EVT_NUMERIC(ID_NUMRUNS, ParametricViewer::OnCommand)
EVT_BUTTON(ID_RUN, ParametricViewer::OnCommand)
EVT_BUTTON(ID_CLEAR, ParametricViewer::OnCommand)
EVT_BUTTON(ID_OUTPUTMENU_CLIPBOARD, ParametricViewer::OnMenuItem)
EVT_BUTTON(ID_OUTPUTMENU_CSV, ParametricViewer::OnMenuItem)
EVT_BUTTON(ID_OUTPUTMENU_EXCEL, ParametricViewer::OnMenuItem)
EVT_GRID_CMD_LABEL_RIGHT_CLICK(ID_GRID, ParametricViewer::OnGridColLabelRightClick)
EVT_MENU(ID_OUTPUTMENU_ADD_PLOT, ParametricViewer::OnMenuItem)
EVT_MENU(ID_OUTPUTMENU_REMOVE_PLOT, ParametricViewer::OnMenuItem)
EVT_MENU(ID_OUTPUTMENU_SHOW_DATA, ParametricViewer::OnMenuItem)
EVT_MENU(ID_OUTPUTMENU_CLIPBOARD, ParametricViewer::OnMenuItem)
EVT_MENU(ID_OUTPUTMENU_CSV, ParametricViewer::OnMenuItem)
EVT_MENU(ID_OUTPUTMENU_EXCEL, ParametricViewer::OnMenuItem)
EVT_MENU(ID_INPUTMENU_FILL_DOWN_ONE_VALUE, ParametricViewer::OnMenuItem)
EVT_MENU(ID_INPUTMENU_FILL_DOWN_SEQUENCE, ParametricViewer::OnMenuItem)
EVT_MENU(ID_INPUTMENU_FILL_DOWN_EVENLY, ParametricViewer::OnMenuItem)
EVT_MENU(ID_SHOW_ALL_INPUTS, ParametricViewer::OnMenuItem)
END_EVENT_TABLE()



ParametricViewer::ParametricViewer(wxWindow *parent, Case *cc) : wxPanel(parent, wxID_ANY), m_case(cc)
//: wxSplitterWindow(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE | wxSP_NOBORDER), m_case(cc)
{
	m_current_graph = 0;

	wxBoxSizer *main_sizer = new wxBoxSizer(wxHORIZONTAL);
	wxSplitterWindow *splitter = new wxSplitterWindow(this, wxID_ANY);
	main_sizer->Add(splitter, 1, wxBOTTOM | wxLEFT | wxEXPAND, 5);


	wxPanel *top_panel = new wxPanel(splitter);

	wxBoxSizer *tool_sizer = new wxBoxSizer(wxHORIZONTAL);
	tool_sizer->Add(new wxStaticText(top_panel, wxID_ANY, "Parametric simulations:"), 0, wxALIGN_CENTER_VERTICAL, 2);
	tool_sizer->Add(new wxButton(top_panel, ID_SELECT_INPUTS, "Input parameters..."), 0, wxALL | wxEXPAND, 2);
	tool_sizer->Add(new wxButton(top_panel, ID_SELECT_OUTPUTS, "Output metrics..."), 0, wxALL | wxEXPAND, 2);
	tool_sizer->Add(new wxStaticText(top_panel, wxID_ANY, "Number of runs:"), 0, wxALIGN_CENTER_VERTICAL, 2);
	m_num_runs_ctrl = new wxNumericCtrl(top_panel, ID_NUMRUNS, 0, wxNumericCtrl::INTEGER, wxDefaultPosition, wxSize(50, 24));
	tool_sizer->Add(m_num_runs_ctrl, 0, wxALL, 2);
	tool_sizer->Add(new wxButton(top_panel, ID_RUN, "Run parametric simulations"), 0, wxALL | wxEXPAND, 2);
	tool_sizer->AddStretchSpacer();
	tool_sizer->Add(new wxButton(top_panel, ID_CLEAR, "Clear results"), 0, wxALL | wxEXPAND, 2);

	// main grid menu (also available as right click in upper left of grid)
	wxBoxSizer *grid_menu_sizer = new wxBoxSizer(wxHORIZONTAL);
	grid_menu_sizer->Add(new wxButton(top_panel, ID_OUTPUTMENU_CLIPBOARD, "Copy to clipboard"), 0, wxALIGN_CENTER_VERTICAL, 2);
	grid_menu_sizer->Add(new wxButton(top_panel, ID_OUTPUTMENU_CSV, "Save as CSV"), 0, wxALL | wxEXPAND, 2);
#ifdef __WXMSW__
	grid_menu_sizer->Add(new wxButton(top_panel, ID_OUTPUTMENU_EXCEL, "Send to Excel"), 0, wxALL | wxEXPAND, 2);
#endif

	m_grid = new ParametricGrid(top_panel, ID_GRID);

	m_grid->RegisterDataType("GridCellCheckBox", new GridCellCheckBoxRenderer, new GridCellCheckBoxEditor);
	m_grid->RegisterDataType("GridCellChoice", new GridCellChoiceRenderer, new GridCellChoiceEditor);
	m_grid->RegisterDataType("GridCellVarValue", new GridCellVarValueRenderer, new GridCellVarValueEditor);
	m_grid->RegisterDataType("GridCellArray", new GridCellArrayRenderer, new GridCellArrayEditor);

	m_grid_data = new ParametricGridData(m_case);
	m_grid->SetTable(m_grid_data);
	m_num_runs_ctrl->SetValue(m_grid_data->GetNumberRows());
	m_input_names = m_grid_data->GetInputNames();
	m_output_names = m_grid_data->GetOutputNames();

	wxBoxSizer *par_sizer = new wxBoxSizer(wxVERTICAL);
	par_sizer->Add(tool_sizer, 0, wxALL | wxEXPAND, 2);
	par_sizer->Add(grid_menu_sizer, 0, wxALL | wxEXPAND, 2);
	par_sizer->Add(m_grid, 1, wxALL | wxEXPAND, 0);

	UpdateGrid();

	top_panel->SetSizer(par_sizer);
	m_layout = new wxSnapLayout(splitter, wxID_ANY);

	splitter->SetMinimumPaneSize(200);
	splitter->SplitHorizontally(top_panel, m_layout, 500);

	SetSizer(main_sizer);
	main_sizer->SetSizeHints(this);

	// check that base case is run and if not run 
	if ((!m_case->BaseCase().Ok()) || (m_case->BaseCase().ListOutputs().Count() <= 0))
		m_case->BaseCase().Invoke(true);

	if (m_grid_data->GetNumberRows() <= 0)
	{
		m_num_runs_ctrl->SetValue(5); // default to 5 runs
		UpdateNumRuns();
	}
}

GraphCtrl *ParametricViewer::CreateNewGraph()
{
	GraphCtrl *gc = new GraphCtrl(m_layout, wxID_ANY);
	m_graphs.push_back(gc);
	m_layout->Add(gc);
	return gc;
}

void ParametricViewer::DeleteGraph(GraphCtrl *gc)
{
	std::vector<wxWindow*>::iterator it = std::find(m_graphs.begin(), m_graphs.end(), gc);
	if (it != m_graphs.end())
	{
		if (m_current_graph == *it)
			m_current_graph = 0;

		m_layout->Delete(*it);
		m_graphs.erase(it);
	}
}

void ParametricViewer::DeleteAll()
{
	for (std::vector<wxWindow*>::iterator it = m_graphs.begin();
		it != m_graphs.end();
		++it)
	{
		m_layout->Delete(*it);
		m_graphs.erase(it);
	}

	m_current_graph = 0;
}

void ParametricViewer::SetGraphs(std::vector<Graph> &gl)
{
	DeleteAll();

	for (size_t i = 0; i<gl.size(); i++)
	{
		GraphCtrl *gc = CreateNewGraph();
		gc->SetGraph(gl[i]);
	}
}


GraphCtrl *ParametricViewer::CurrentGraph()
{
	return m_current_graph;
}

void ParametricViewer::UpdateGraph()
{
	if (!m_current_graph) return;
	Graph g = m_current_graph->GetGraph();
}

void ParametricViewer::OnGraphSelect(wxCommandEvent &evt)
{
	if (GraphCtrl *gc = dynamic_cast<GraphCtrl*>(evt.GetEventObject()))
		SetCurrent(gc);
}

void ParametricViewer::SetCurrent(GraphCtrl *gc)
{
	if (m_current_graph)
	{
		m_layout->ClearHighlights();
		m_current_graph = 0;
	}

	m_current_graph = gc;

	if (m_current_graph)
		m_layout->Highlight(m_current_graph);

}


void ParametricViewer::OnCommand(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_SELECT_INPUTS:
		SelectInputs();
		UpdateGrid();
		break;
	case ID_SELECT_OUTPUTS:
		SelectOutputs();
		UpdateGrid();
		break;
	case ID_NUMRUNS:
		UpdateNumRuns();
		UpdateGrid();
		break;
	case ID_RUN:
		RunSimulations();
		UpdateGrid();
		break;
	case ID_CLEAR:
		ClearResults();
		UpdateGrid();
		break;
	}
}

void ParametricViewer::OnMenuItem(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_OUTPUTMENU_ADD_PLOT:
		AddPlot(m_grid_data->GetVarName(0,m_selected_grid_col));
		break;
	case ID_OUTPUTMENU_REMOVE_PLOT:
		RemovePlot(m_grid_data->GetVarName(0,m_selected_grid_col));
		break;
	case ID_OUTPUTMENU_SHOW_DATA:
		ShowAllData();
		break;
	case ID_OUTPUTMENU_CLIPBOARD:
		CopyToClipboard();
		break;
	case ID_OUTPUTMENU_CSV:
		SaveToCSV();
		break;
#ifdef __WXMSW__
	case ID_OUTPUTMENU_EXCEL:
		SendToExcel();
		break;
#endif
	case ID_INPUTMENU_FILL_DOWN_ONE_VALUE:
		FillDown(1);
		break;
	case ID_INPUTMENU_FILL_DOWN_SEQUENCE:
		FillDown(2);
		break;
	case ID_INPUTMENU_FILL_DOWN_EVENLY:
		FillDown(-1);
		break;
	case ID_SHOW_ALL_INPUTS:
		if (m_grid_data->GetRuns().size() > m_selected_grid_row)
		{
			if (m_grid_data->GetRuns()[m_selected_grid_row])
			{
				new VariableGridFrame(this, &SamApp::Project(), m_case, m_grid_data->GetRuns()[m_selected_grid_row]->GetInputVarTable(), wxString::Format("Parametric run %d inputs", m_selected_grid_row+1));
			}
		}
		break;
	}
}

void ParametricViewer::GetTextData(wxString &dat, char sep)
{
	dat = wxEmptyString;
	if (!m_grid)
		return;

	size_t approxbytes = m_grid_data->GetNumberRows() * 15 * m_grid_data->GetNumberCols();
	dat.Alloc(approxbytes);

	size_t c;

	for (c = 0; c<m_grid_data->GetNumberCols(); c++)
	{
		wxString label = m_grid_data->GetColLabelValue(c);
		label.Replace('\n', " | ");

		if (sep == ',')
			dat += '"' + label + '"';
		else
			dat += label;

		if (c < m_grid_data->GetNumberCols() - 1)
			dat += sep;
		else
			dat += '\n';
	}

	for (size_t r = 0; r<m_grid_data->GetNumberRows(); r++)
	{
		for (c = 0; c<m_grid_data->GetNumberCols(); c++)
		{
			// choice values - can handle hourly and monthly similarly
			if (m_grid_data->GetTypeName(r, c) == "GridCellChoice")

				dat += m_grid_data->GetChoice(r, c);
			else 
				dat += m_grid_data->GetValue(r, c);

			if (c < m_grid_data->GetNumberCols() - 1)
				dat += sep;
			else
				dat += '\n';
		}
	}
}


void ParametricViewer::CopyToClipboard()
{
	wxBusyInfo busy("Processing data table... please wait");
	wxString dat;
	GetTextData(dat, '\t');

	// strip commas per request from Paul 5/23/12 meeting
	dat.Replace(",", "");

	if (wxTheClipboard->Open())
	{
		wxTheClipboard->SetData(new wxTextDataObject(dat));
		wxTheClipboard->Close();
	}
}

void ParametricViewer::SaveToCSV()
{
	wxFileDialog fdlg(this, "Save as CSV", wxEmptyString, "results.csv", "Comma-separated values (*.csv)|*.csv", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
	if (fdlg.ShowModal() != wxID_OK) return;

	FILE *fp = fopen(fdlg.GetPath().c_str(), "w");
	if (!fp)
	{
		wxMessageBox("Could not open file for write:\n\n" + fdlg.GetPath());
		return;
	}

	wxBusyInfo busy("Writing CSV file... please wait");

	wxString dat;
	GetTextData(dat, ',');
	fputs(dat.c_str(), fp);
	fclose(fp);

}

void ParametricViewer::SendToExcel()
{
	wxBusyInfo busy("Processing data table... please wait");
	wxString dat;
	GetTextData(dat, '\t');

	// strip commas per request from Paul 5/23/12 meeting
	dat.Replace(",", "");

#ifdef __WXMSW__
	wxExcelAutomation xl;
	if (!xl.StartExcel())
	{
		wxMessageBox("Could not start Excel.");
		return;
	}

	xl.Show(true);

	if (!xl.NewWorkbook())
	{
		wxMessageBox("Could not create a new Excel worksheet.");
		return;
	}
	if (wxTheClipboard->Open())
	{
		wxTheClipboard->SetData(new wxTextDataObject(dat));
		wxTheClipboard->Close();
		xl.PasteClipboard();
	}
#endif
}


void ParametricViewer::OnGridColLabelRightClick(wxGridEvent &evt)
{
	m_selected_grid_col = evt.GetCol();
	m_selected_grid_row = evt.GetRow();
	if (m_selected_grid_row < 0)
	{
		if (m_selected_grid_col < 0) // upper left corner of grid
		{
			//	Grid menu
			wxPoint point = evt.GetPosition();
			wxMenu *menu = new wxMenu;
			menu->Append(ID_OUTPUTMENU_CLIPBOARD, _T("Copy to clipboard"));
			menu->Append(ID_OUTPUTMENU_CSV, _T("Save as CSV"));
#ifdef __WXMSW__
			menu->Append(ID_OUTPUTMENU_EXCEL, _T("Send to Excel"));
#endif
			PopupMenu(menu, point);
		}
		else // header with variables
		{
			if (m_grid_data->IsInput(m_selected_grid_col))
			{
				// input menu
				wxPoint point = evt.GetPosition();
				wxMenu *menu = new wxMenu;
				menu->Append(ID_INPUTMENU_FILL_DOWN_ONE_VALUE, _T("Fill down one value"));
				menu->Append(ID_INPUTMENU_FILL_DOWN_SEQUENCE, _T("Fill down sequence"));
				menu->Append(ID_INPUTMENU_FILL_DOWN_EVENLY, _T("Fill down evenly"));
				PopupMenu(menu, point);
			}
			else
			{
				//	Output menu
				wxPoint point = evt.GetPosition();
				wxMenu *menu = new wxMenu;
				menu->Append(ID_OUTPUTMENU_ADD_PLOT, _T("Add plot"));
				menu->Append(ID_OUTPUTMENU_REMOVE_PLOT, _T("Remove plot"));
				menu->Append(ID_OUTPUTMENU_SHOW_DATA, _T("Show all data"));
				int ndx = m_plot_var_names.Index(m_grid_data->GetVarName(0,m_selected_grid_col));
				menu->Enable(ID_OUTPUTMENU_ADD_PLOT, (ndx == wxNOT_FOUND));
				menu->Enable(ID_OUTPUTMENU_REMOVE_PLOT, (ndx != wxNOT_FOUND));
				PopupMenu(menu, point);
			}
		}
	}
	else if (m_selected_grid_col < 0) // row header
	{
		//	row menu
		wxPoint point = evt.GetPosition();
		wxMenu *menu = new wxMenu;
		menu->Append(ID_SHOW_ALL_INPUTS, _T("Show inputs"));
		PopupMenu(menu, point);
	}
}


void ParametricViewer::ShowAllData()
{
	int col = m_selected_grid_col;
	std::vector<std::vector<float> > values_vec;
	wxArrayString labels;
	for (int row = 0; row < m_grid_data->GetNumberRows(); row++)
	{
		std::vector<float> vec = m_grid_data->GetArray(row, col);
		if (vec.size() == 0) // single values
			vec.push_back(m_grid_data->GetDouble(row, col));
		values_vec.push_back(vec);
		labels.push_back(wxString::Format("Run %d", row + 1));
	}
	ArrayPopupDialog *apd = new ArrayPopupDialog(this, m_grid_data->GetColLabelValue(col), labels, values_vec);
	apd->ShowModal();
}

void ParametricViewer::FillDown(int rows)
{
	// get first two values in column and fill down
	int col = m_selected_grid_col;
	if (rows<0)
		m_grid_data->FillEvenly(col);
	else
		m_grid_data->FillDown(col,rows);
}


bool ParametricViewer::Plot(int col, Graph &g)
{
	bool ret_val = false;

	if ((m_grid_data->GetRowsCount() > 0) && (col >= 0) && (col < m_grid_data->GetColsCount()))
	{
		if (VarValue *vv = m_grid_data->GetVarValue(0, col))
		{
			g.Y.push_back(m_grid_data->GetVarName(0,col));
			switch (vv->Type())
			{
				case VV_NUMBER:
				{
					g.Type = Graph::BAR;
					g.Size = 15; // bar size
					ret_val = true;
					g.YLabel = m_grid_data->GetColLabelValue(col);
					if (!m_grid_data->GetUnits(col).IsEmpty())
						g.YLabel += " (" + m_grid_data->GetUnits(col) + ")";
					g.XLabel = "Run number";
				}
				break;
				// arrays - determine if monthly or hourly
				case VV_ARRAY:
				{
					size_t n;
					float *y = m_grid_data->GetArray(0, col, &n); // checked above for rows>0

					if (n == 12) // asume monthly
					{
						g.Type = Graph::BAR;
						g.YLabel = m_grid_data->GetColLabelValue(col);
						if (!m_grid_data->GetUnits(col).IsEmpty())
							g.YLabel += " (" + m_grid_data->GetUnits(col) + ")";
						g.XLabel = "Run number";
						ret_val = true;
					}
					else if (n == 8760)
					{
//						g.Type = Graph::LINE;
//						g.YLabel = m_grid_data->GetColLabelValue(col);
//						if (!m_grid_data->GetUnits(col).IsEmpty())
//							g.YLabel += " (" + m_grid_data->GetUnits(col) + ")";
//						g.XLabel = "Run number";
						g.Type = -1; // DView - do not use GraphCtrl
						ret_val = true;
					}
				}
				break;
			}
		}
	}
	return ret_val;
}

void ParametricViewer::AddAllPlots()
{
	for (int i = 0; i < m_output_names.Count(); i++)
		AddPlot(m_output_names[i]);
}

void ParametricViewer::RemoveAllPlots()
{
	int i = m_plot_var_names.Count()-1;
	while (i >= 0)
	{
		RemovePlot(m_plot_var_names[i]);
		i--;
	}
}


void ParametricViewer::AddPlot(const wxString &output_name)
{
	// check if already plotted
	int col = m_grid_data->GetColumnForName(output_name);
	if (col != wxNOT_FOUND)
	{
		int ndx = m_plot_var_names.Index(output_name);
		if (ndx == wxNOT_FOUND)
		{
			Graph g;
			if (Plot(col, g))
			{
				m_plot_var_names.push_back(output_name);
				if (g.Type >= 0)
				{
					GraphCtrl *gc = new GraphCtrl(m_layout, wxID_ANY);
					gc->Display(m_grid_data->GetRuns(), g);
					m_graphs.push_back(gc);
					// TODO sizing
					m_layout->Add(gc, 800, 400);
				}
				else // DView
				{
					wxDVTimeSeriesCtrl *dv = new wxDVTimeSeriesCtrl(this, wxID_ANY, wxDV_RAW, wxDV_AVERAGE);
					for (size_t row = 0; row < m_grid_data->GetRowsCount(); row++)
					{
						size_t n;
						float *y = m_grid_data->GetArray(row, col, &n);
						//if (n == 8760)
						dv->AddDataSet(new TimeSeries8760(y, m_grid_data->GetColLabelValue(col) + wxString::Format(" : run(%d)", row + 1), m_grid_data->GetUnits(col)), wxEmptyString, true);
						dv->SelectDataSetAtIndex(row);
					}
					m_graphs.push_back(dv);
					// TODO - good way to size dview control
					//m_layout->Add(dv, dv->GetBestSize().GetWidth(), dv->GetBestSize().GetHeight());
					//m_layout->Add(dv, 1000, 300); // twice default width in wxSnapLayout
					m_layout->Add(dv, 800, 400);
				}
			}
		}
	}
}

void ParametricViewer::RemovePlot(const wxString &output_name)
{
	int col = m_grid_data->GetColumnForName(output_name);
	if (col != wxNOT_FOUND)
	{
		int ndx = m_plot_var_names.Index(output_name);
		if (ndx != wxNOT_FOUND)
		{
			m_plot_var_names.Remove(output_name);
			std::vector<wxWindow*>::iterator it = m_graphs.begin() + ndx;
			m_layout->Delete(*it);
			m_graphs.erase(it);
		}
	}
}

void ParametricViewer::UpdateNumRuns()
{
	// update number of runs - here and when number of runs change
	m_grid_data->UpdateNumberRows(m_num_runs_ctrl->AsInteger());
}

void ParametricViewer::RunSimulations()
{
	RemoveAllPlots();
	m_grid_data->RunSimulations();
	AddAllPlots();
	UpdateGrid();
}

void ParametricViewer::ClearResults()
{
	RemoveAllPlots();
	m_grid_data->ClearResults();
	m_input_names.Clear();
	m_output_names.Clear();
	m_grid_data->UpdateInputs(m_input_names);
	m_grid_data->UpdateOutputs(m_output_names);
	m_grid_data->UpdateNumberRows(0);
	UpdateNumRuns();
}

void ParametricViewer::SelectInputs()
{
	wxArrayString names, labels;
	wxString case_name(SamApp::Project().GetCaseName(m_case));

	ConfigInfo *ci = m_case->GetConfiguration();
	VarInfoLookup &vil = ci->Variables;

	for (VarInfoLookup::iterator it = vil.begin(); it != vil.end(); ++it)
	{
		wxString name = it->first;
		VarInfo &vi = *(it->second);

		// skip calculated and indicator values
		if (vi.Flags & VF_CALCULATED || vi.Flags & VF_INDICATOR) continue;

		wxString label = vi.Label;
		if (label.IsEmpty())
			label = "{ " + name + " }";
		if (!vi.Units.IsEmpty())
			label += " (" + vi.Units + ")";
		if (!vi.Group.IsEmpty())
			label = vi.Group + "/" + label;
		
		labels.Add(label);
		names.Add(name);
	}

	wxSortByLabels(names, labels);
	SelectVariableDialog dlg(this, "Select Inputs");
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames(m_input_names);
	if (dlg.ShowModal() == wxID_OK)
	{
		RemoveAllPlots();
		m_input_names = dlg.GetCheckedNames();
		m_grid_data->UpdateInputs(m_input_names);
		RunSimulations();
	}
}

void ParametricViewer::SelectOutputs()
{
	wxArrayString names, labels;
	wxString case_name(SamApp::Project().GetCaseName(m_case));
	wxArrayString output_names, output_labels;

	if (m_grid_data->GetRuns().size() > 0)
	{ // from PopulateSelectionList in results.cpp
		int an_period = -1;
		Simulation *sim = m_grid_data->GetRuns()[0];
		ConfigInfo *config = m_case->GetConfiguration();
		if (config != 0)
		{
			wxString an_var = config->Settings["analysis_period_var"];
			if (!an_var.IsEmpty())
			{
				if (VarValue *vv = sim->GetValue(an_var))
					if (vv->Type() == VV_NUMBER)
						an_period = (int)vv->Value();
			}
		}

		std::vector<size_t> varlengths;
		sim->GetVariableLengths(varlengths);

		names.Clear();

		for (size_t i = 0; i<varlengths.size(); i++)
		{
			wxArrayString list;
			sim->ListByCount(varlengths[i], list);

			if (list.Count() == 0)
				continue;

			wxString group;
			if (varlengths[i] == 1)
				group = "Single Values";
			else if (varlengths[i] == 12)
				group = "Monthly Data";
			else if (varlengths[i] == 8760)
				group = "Hourly Data";
			else if (varlengths[i] == an_period)
				group = "Annual Data";
			else if (varlengths[i] == (an_period - 1) * 12)
				group = "Lifetime Monthly Data";
			else if (varlengths[i] == (an_period - 1) * 8760)
				group = "Lifetime Hourly Data";
			else
				group.Printf("Data: %d values", (int)varlengths[i]);

			for (size_t j = 0; j<list.Count(); j++)
			{
				wxString name = list[i];
				wxString label(sim->GetLabel(list[j]));
				if (label.IsEmpty())
					label = "{ " + name + " }";
				wxString units(sim->GetUnits(list[j]));
				if (!units.IsEmpty())
					label += " (" + units + ")";
				label = group + "/" + label;
				labels.Add(label);
				names.Add(list[j]);
			}
		}
	}
	wxSortByLabels(names, labels);
	SelectVariableDialog dlg(this, "Select Outputs");
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames(m_output_names);
	if (dlg.ShowModal() == wxID_OK)
	{
		RemoveAllPlots();
		m_output_names = dlg.GetCheckedNames();
		m_grid_data->UpdateOutputs(m_output_names);
		RunSimulations();
	}
}

void ParametricViewer::UpdateGrid()
{
	// update grid data with m_par updates from configure and number of runs
	//m_grid->SetTable(m_grid_data);
	//m_grid->ForceRefresh();
	// setting with attr in table base
	for (int col = 0; col < m_grid_data->GetNumberCols(); col++)
	{
		for (int row = 0; row < m_grid_data->GetNumberRows(); row++)
		{
			if (m_grid_data->IsInput(col))
				m_grid->SetReadOnly(row, col, false);
			else
			{
	//			bool readonly = (m_grid_data->GetTypeName(row, col) == wxGRID_VALUE_STRING);
	//			m_grid->SetReadOnly(row, col, readonly);
			}
		}
	}
	m_grid->AutoSizeColumns();
	
}


////////////////////////////////////////////////////////////////////////////////////////////////

// m_par contains list of inputs and outputs
ParametricGridData::ParametricGridData(Case *cc) :m_case(cc), m_par(cc->Parametric())
{
	m_color_for_inputs = wxColour("LIGHT BLUE");
	m_color_for_valid_outputs = wxColour("PALE GREEN");
	m_color_for_invalid_outputs = wxColour("VIOLET RED");
	m_attr_for_inputs = new wxGridCellAttr;
	m_attr_for_inputs->SetBackgroundColour(m_color_for_inputs);
	m_attr_for_valid_outputs = new wxGridCellAttr;
	m_attr_for_valid_outputs->SetBackgroundColour(m_color_for_valid_outputs);
	m_attr_for_invalid_outputs = new wxGridCellAttr;
	m_attr_for_invalid_outputs->SetBackgroundColour(m_color_for_invalid_outputs);

	m_rows = 0;
	m_cols = 0;
	Init();
}

ParametricGridData::~ParametricGridData()
{
	m_attr_for_inputs->DecRef();
	m_attr_for_valid_outputs->DecRef();
	m_attr_for_invalid_outputs->DecRef();
}

void ParametricGridData::Init()
{ // assumes m_par has been read in and is sorted properly..
	if (m_par.Setup.size()<1) return;
	m_col_hdrs.Clear();
	m_input_names.Clear();
	m_output_names.Clear();
	m_var_names.Clear();

	m_cols = m_par.Setup.size();

	for (int i = 0; i < m_cols; i++)
	{
		wxString var_name = m_par.Setup[i].Name;
		if (IsInput(var_name))
			m_input_names.push_back(var_name);
		else
			m_output_names.push_back(var_name);
	}
	// sorted order
	for (size_t i = 0; i < m_input_names.Count(); i++)
		m_var_names.push_back(m_input_names[i]);
	for (size_t i = 0; i < m_output_names.Count(); i++)
		m_var_names.push_back(m_output_names[i]);

	m_rows = m_par.Runs.size();
}

void ParametricGridData::UpdateSetup()
{ // assumes inputs or outputs have been updated
	// sorts m_var_name and m_par.Setup based in inputs first and then outputs.
// used instead of inserting inputs so that additional sorting can be performed
	std::vector<ParametricData::Var> sorted_setup;
	int i, ndx;
	for (i = 0; i < m_input_names.Count(); i++)
	{
		ndx = m_par.FindSetup(m_input_names[i]);
		if (ndx < 0)
		{
			// throw error?
			return;
		}
		sorted_setup.push_back(m_par.Setup[ndx]);
	}

	for (i = 0; i < m_output_names.Count(); i++)
	{
		ndx = m_par.FindSetup(m_output_names[i]);
		if (ndx < 0)
		{
			// throw error?
			return;
		}
		sorted_setup.push_back(m_par.Setup[ndx]);
	}
	m_par.Setup = sorted_setup;

	// sorted order
	m_var_names.Clear();
	for (size_t i = 0; i < m_input_names.Count(); i++)
		m_var_names.push_back(m_input_names[i]);
	for (size_t i = 0; i < m_output_names.Count(); i++)
		m_var_names.push_back(m_output_names[i]);

	m_cols = m_par.Setup.size();
	m_rows = m_par.Runs.size();

}


int ParametricGridData::GetNumberRows()
{
	return m_rows;
}

int ParametricGridData::GetNumberCols()
{
	return m_cols;
}

bool ParametricGridData::IsEmptyCell(int row, int col)
{
	bool emptycell = true;
	if ((row>-1 && row<m_rows) && (col > -1 && col <m_cols))
	{
		if (IsInput(col))
		{
			if (row < (int)m_par.Setup[col].Values.size())
				emptycell = (m_par.Setup[col].Values[row].AsString() == wxEmptyString);
		}
		else
		{
			if (row < (int)m_par.Runs.size())
				if (VarValue *vv = m_par.Runs[row]->GetOutput(m_var_names[col]))
					emptycell = (vv->AsString() == wxEmptyString);
		}
	}
	return emptycell;
}

bool ParametricGridData::IsInput(int col)
{
	if ((col>-1) && (col < m_cols))
		if (VarValue *vv = m_par.GetCase()->Values().Get(m_par.Setup[col].Name))
			return true;
		else
			return false;
	else
		return false;
}

bool ParametricGridData::IsInput(wxString &var_name)
{
	if (VarValue *vv = m_par.GetCase()->Values().Get(var_name))
		return true;
	else
		return false;
}


wxString ParametricGridData::GetColLabelValue(int col)
{
	wxString col_label = wxEmptyString;
	wxString units = wxEmptyString;
	if ((col>-1) && (col < m_cols))
	{
		if (IsInput(col)) // label if non-blank
		{
			if (VarInfo *vi = m_par.GetCase()->Variables().Lookup(m_var_names[col]))
			{
				col_label = vi->Label;
				units = vi->Units;
			}
		}
		else
		{
			if (m_par.Runs.size() > 0)
			{
				col_label = m_par.Runs[0]->GetLabel(m_var_names[col]);
				units = m_par.Runs[0]->GetUnits(m_var_names[col]);
			}
		}
		if (col_label.IsEmpty()) 
			col_label = m_var_names[col];
		if (!units.IsEmpty())
			col_label += " (" + units + ")";
	}
	return col_label;
}


VarInfo* ParametricGridData::GetVarInfo(int row, int col)
{
	VarInfo* vi = NULL;
	if ((col>-1) && (col < m_cols))
	{
		if (IsInput(col))
			vi = m_par.GetCase()->Variables().Lookup(m_var_names[col]);
	}
	return vi;
}

void ParametricGridData::SetVarInfo(int row, int col, VarInfo *vi)
{
	if ((col>-1) && (col < m_cols))
	{
		if (IsInput(col))
			if (VarInfo *var_info = m_par.GetCase()->Variables().Lookup(m_var_names[col]))
				var_info = vi;
	}
}

int ParametricGridData::GetColumnForName(const wxString &name)
{
	return m_var_names.Index(name);
}

VarValue* ParametricGridData::GetVarValue(int row, int col)
{
	VarValue* vv = NULL;
	if ((col>-1) && (col < m_cols))
	{
		if (IsInput(col))
		{
			if (row < (int)m_par.Setup[col].Values.size())
				vv = &m_par.Setup[col].Values[row];
		}
		else
		{
			if (row < (int)m_par.Runs.size())
				vv = m_par.Runs[row]->GetOutput(m_var_names[col]);
		}
	}
	return vv;

}

void ParametricGridData::SetVarValue(int row, int col, VarValue *vv)
{
	if ((col>-1) && (col < m_cols))
	{
		if (IsInput(col))
		{
			if (row < (int)m_par.Setup[col].Values.size())
				if (VarValue *var_value = &m_par.Setup[col].Values[row])
					var_value = vv;
		}
		else
		{
			if (row < (int)m_par.Runs.size())
				if (VarValue *var_value = m_par.Runs[row]->GetOutput(m_var_names[col]))
					var_value = vv;
		}
	}
}


int ParametricGridData::GetMaxChoice(int row, int col)
{
	int max_choice = 0;
	if ((col>-1) && (col < m_cols))
	{
		if (VarInfo *vi = GetVarInfo(row, col))
		{
			wxArrayString as = vi->IndexLabels;
			max_choice = as.Count();
		}
	}
	return max_choice;
}

wxString ParametricGridData::GetChoice(int row, int col)
{
	wxString ret_str = wxEmptyString;
	if ((col>-1) && (col < m_cols))
	{
		if (VarInfo *vi = GetVarInfo(row, col))
		{
			wxArrayString as = vi->IndexLabels;
			int ndx = (int)GetDouble(row, col);
			if ((as.Count() > 0) && (ndx >= 0) && (ndx < as.Count()))
				ret_str = as[ndx];
		}
	}
	return ret_str;
}

wxString ParametricGridData::GetChoices(int row, int col)
{
	wxString ret_str = wxEmptyString;
	if ((col>-1) && (col < m_cols))
	{
		if (VarInfo *vi = GetVarInfo(row,col))
		{
			wxArrayString as = vi->IndexLabels;
			if (as.Count() > 0)
			{
				for (int i = 0; i < (int)as.Count() - 1; i++)
					ret_str += as[i] + ",";
				ret_str += as[as.Count() - 1];
			}
		}
	}
	return ret_str;
}

wxString ParametricGridData::GetValue(int row, int col)
{
	wxString value = wxEmptyString;
	{
		if ((col > -1) && (col < m_cols))
		{
			if (VarValue *vv = GetVarValue(row, col))
				value = vv->AsString();
		}
	}
	return value;
}

void ParametricGridData::SetValue(int row, int col, const wxString& value)
{
	if ((col > -1) && (col < m_cols))
	{
		if (IsInput(col))
		{
			VarValue *vv = &m_par.Setup[col].Values[row];
			VarValue::Parse(vv->Type(), value, *vv);
			if (row < m_valid_run.size())
				m_valid_run[row] = false;
			else // should not happen!
				m_valid_run.push_back(row);
			// set for simulation (repeated in run simulation below for copying
			m_par.Runs[row]->Override(m_var_names[col], *vv);
		}
	}
}


wxString ParametricGridData::GetTypeName(int row, int col)
{
	if ((col > -1) && (col < m_cols))
	{
		if (VarInfo *vi = GetVarInfo(row, col))
		{
			 // TODO - better control list maintenance here and in UIEditorPanel
			wxString type = vi->UIObject;
			if (type == "Numeric")
				return wxGRID_VALUE_STRING;
			else if (type == "Choice")
				return "GridCellChoice";
			else if (type == "ListBox")
				return "GridCellVarValue";
			else if (type == "RadioChoice")
				return "GridCellVarValue";
			else if (type == "TextEntry")
				return wxGRID_VALUE_STRING;
			else if (type == "Slider")
				return "GridCellVarValue";
			else if (type == "CheckBox")
				return "GridCellCheckBox";
			else if (type == "SchedNumeric")
				return "GridCellVarValue";
			else if (type == "TOUSchedule")
				return "GridCellVarValue";
			else if (type == "PTLayout")
				return "GridCellVarValue";
			else if (type == "MaterialProperties")
				return "GridCellVarValue";
			else if (type == "TroughLoop")
				return "GridCellVarValue";
			else if (type == "MonthlyFactor")
				return "GridCellVarValue";
			else if (type == "SearchListBox")
				return "GridCellVarValue";
			else if (type == "DataArray")
				return "GridCellVarValue";
			else if (type == "DataMatrix")
				return "GridCellVarValue";
			else if (type == "ShadingFactors")
				return "GridCellVarValue";
			else if (type == "ValueMatrix")
				return "GridCellVarValue";
			else if (type == "MonthByHourFactors")
				return "GridCellVarValue";
			else if (type == "Library")
				return "GridCellVarValue";
			else if (type == "HourlyFactor")
				return "GridCellVarValue";
			else if (type == "DiurnalPeriod")
				return "GridCellVarValue";
			else if (vi->UIObject == VUIOBJ_NONE)
				return wxGRID_VALUE_STRING;
			else 
				return wxGRID_VALUE_STRING;
		}
		else
		{
			if (VarValue *vv = GetVarValue(row, col))
			{
				switch (vv->Type())
				{
				case VV_ARRAY:
					{
						size_t n;
						vv->Array(&n);
						if (n>1)
							return "GridCellArray";
						else
							return wxGRID_VALUE_STRING;
					}
					break;
				default:
					return wxGRID_VALUE_STRING;
				}
			}
			else
				return wxGRID_VALUE_STRING;
		}
	}
	else
		return wxGRID_VALUE_STRING;
}


void ParametricGridData::SetColLabelValue(int col, const wxString &label)
{
	if ((col > 0) && (col < (int)m_col_hdrs.Count()))
	{
		m_col_hdrs[col] = label;
	}
}


bool ParametricGridData::AppendCols(size_t ncols)
{
	if (GetView())
	{
		wxGridTableMessage msg(this,
			wxGRIDTABLE_NOTIFY_COLS_APPENDED,
			ncols);
		GetView()->ProcessTableMessage(msg);
	}
	return true;
}

bool ParametricGridData::InsertCols(size_t pos, size_t ncols)
{
	if (GetView())
	{
		wxGridTableMessage msg(this,
			wxGRIDTABLE_NOTIFY_COLS_INSERTED,
			pos,
			ncols);
		GetView()->ProcessTableMessage(msg);
	}
	return true;
}

bool ParametricGridData::DeleteCols(size_t pos, size_t ncols)
{
	if (GetView())
	{
		wxGridTableMessage msg(this,
			wxGRIDTABLE_NOTIFY_COLS_DELETED,
			pos,
			ncols);
		GetView()->ProcessTableMessage(msg);
	}
	return true;
}

bool ParametricGridData::AppendRows(size_t nrows)
{
	if (GetView())
	{
		wxGridTableMessage msg(this,
			wxGRIDTABLE_NOTIFY_ROWS_APPENDED,
			nrows);
		GetView()->ProcessTableMessage(msg);
	}
	return true;
}

bool ParametricGridData::InsertRows(size_t pos, size_t nrows)
{
	if (GetView())
	{
		wxGridTableMessage msg(this,
			wxGRIDTABLE_NOTIFY_ROWS_INSERTED,
			pos,
			nrows);
		GetView()->ProcessTableMessage(msg);
	}
	return true;
}

bool ParametricGridData::DeleteRows(size_t pos, size_t nrows)
{
	if (GetView())
	{
		wxGridTableMessage msg(this,
			wxGRIDTABLE_NOTIFY_ROWS_DELETED,
			pos,
			nrows);
		GetView()->ProcessTableMessage(msg);
	}
	return true;
}

void ParametricGridData::AddSetup(ParametricData::Var &var)
{
	m_par.Setup.push_back(var);
	AppendCols();
}

void ParametricGridData::DeleteSetup(wxString &var_name)
{
	if (m_par.RemoveSetup(var_name))
	{
		DeleteCols();
		// reset simulaiton input to base case input
		for (int row = 0; row < m_rows; row++)
		{
			if (VarValue *vv = m_case->BaseCase().GetInput(var_name))
			{
				m_par.Runs[row]->Override(var_name, *vv);
				m_valid_run[row] = false;
			}
		}
		// TODO invalidated results.
	}
}

void ParametricGridData::UpdateNumberRows(int rows)
{
	if (rows < 0) return;
	if (m_rows != rows)
	{
		// go through all variables and append VarValues as necessary
		for (size_t i = 0; i < m_var_names.Count(); i++)
		{
			// update Setup
			int ndx = m_par.FindSetup(m_var_names[i]);
			if (ndx > -1)
			{
				if ((m_par.Setup[i].Values.size() < rows) && (IsInput(i)))
				{
					while (m_par.Setup[i].Values.size() < rows)
					{ // inputs
						if (VarValue *vv = m_case->Values().Get(m_var_names[i]))
							m_par.Setup[i].Values.push_back(*vv);
					}
				}
				else if (m_par.Setup[i].Values.size() > rows)
				{
					while (m_par.Setup[i].Values.size() > rows)
						m_par.Setup[i].Values.pop_back();
				}
			}
		}

		// update Runs
		if (m_par.Runs.size() < rows)
		{
			for (int num_run = m_par.Runs.size(); num_run < rows; num_run++)
			{
				Simulation *s = new Simulation(m_case, wxString::Format("Run %d", num_run));
				s->Copy(m_case->BaseCase());
				m_par.Runs.push_back(s);
			}
		}
		else if (m_par.Runs.size() > rows)
		{
			while (m_par.Runs.size() > rows)
				m_par.Runs.pop_back();
		}

		// update valid runs
		if (m_valid_run.size() < rows)
		{
			for (int num_run = m_valid_run.size(); num_run < rows; num_run++)
				m_valid_run.push_back(false);
		}
		else if (m_valid_run.size() > rows)
		{
			while (m_valid_run.size() > rows)
				m_valid_run.pop_back();
		}

		if (m_rows > rows)
		{
			DeleteRows(rows, m_rows - rows);
		}
		else if (m_rows < rows)
		{
			AppendRows(rows - m_rows);
		}
		m_rows = rows;
	}
}

wxArrayString ParametricGridData::GetInputNames()
{
	return m_input_names;
}

wxArrayString ParametricGridData::GetOutputNames()
{
	return m_output_names;
}

 //does nothing ? 
void ParametricGridData::UpdateView()
{
	if (GetView())
	{
		wxGridTableMessage msg2(this,
			wxGRIDTABLE_REQUEST_VIEW_GET_VALUES);
//		wxGridTableMessage msg(this,
//			wxGRIDTABLE_REQUEST_VIEW_SEND_VALUES);
//		GetView()->ProcessTableMessage(msg);
		GetView()->ProcessTableMessage(msg2);
		GetView()->Update();
	}
}

double ParametricGridData::GetDouble(int row, int col)
{
	double ret_val = 0;
	if (VarValue *vv = GetVarValue(row, col))
	{
		if (vv->Type() == VV_NUMBER)
			ret_val = vv->Value();
	}
	return ret_val;
}

std::vector<float> ParametricGridData::GetArray(int row, int col)
{
	std::vector<float> ret_val;
	if (VarValue *vv = GetVarValue(row, col))
	{
		if (vv->Type() == VV_ARRAY)
			ret_val = vv->Array();
	}
	return ret_val;
}

float *ParametricGridData::GetArray(int row, int col, size_t *n)
{
	float *ret_val;
	if (VarValue *vv = GetVarValue(row, col))
	{
		if (vv->Type() == VV_ARRAY)
			ret_val = vv->Array(n);
	}
	return ret_val;
}

wxString ParametricGridData::GetUnits(int col)
{
	wxString  ret_val = wxEmptyString;
	if (m_rows > 0)
	{
		if (IsInput(col))
		{
			if (VarInfo *vi = GetVarInfo(0, col))
				ret_val = vi->Label;
		}
		else // output
		{
			if (m_par.Runs.size() > 0)
				ret_val = m_par.Runs[0]->GetUnits(m_var_names[col]);
		}
	}
	return ret_val;
}

wxString ParametricGridData::GetVarName(int row, int col)
{
	wxString  ret_val=wxEmptyString;
	if ((col > -1) && (col < m_var_names.Count()) && (row > -1) && (row < m_rows))
	{
		ret_val = m_var_names[col];
	}
	return ret_val;
}

void ParametricGridData::FillDown(int col, int rows)
{
	if (m_rows > 0)
	{
		if (VarValue *vv = GetVarValue(0, col))
		{
			switch (vv->Type())
			{
				//single value only
			case VV_NUMBER:
				if (m_rows > 2)
				{
					double num0 = GetDouble(0, col);
					double num1 = GetDouble(1, col); 
					int start_row = 2;
					if (rows == 1)
					{
						num1 = GetDouble(0, col);
						start_row = 1;
					}
					double diff = num1 - num0;
					double old_val = num1;
					// handle choices
					int max_choice = GetMaxChoice(0, col);
					for (int row = start_row; row < m_rows; row++)
					{
						double new_val = old_val + diff;
						if (max_choice > 0) new_val = (int)new_val % max_choice;
						SetValue(row, col, wxString::Format("%lg", new_val));
						old_val = new_val;
					}
					UpdateView(); 
				}
				break;
			}
		}
	}

}


void ParametricGridData::FillEvenly(int col)
{
	if (m_rows > 0)
	{
		if (VarValue *vv = GetVarValue(0, col))
		{
			switch (vv->Type())
			{
				//single value only
			case VV_NUMBER:
				if (m_rows > 2)
				{
					double num0 = GetDouble(0, col);
					double num1 = GetDouble(m_rows-1, col);
					double diff = (num1 - num0)/(m_rows-1);
					double old_val = num0;
					// handle choices
					int max_choice = GetMaxChoice(0, col);
					for (int row = 1; row < m_rows-1; row++)
					{
						double new_val = old_val + diff;
						if (max_choice > 0) new_val = (int)new_val % max_choice;
						SetValue(row, col, wxString::Format("%lg", new_val));
						old_val = new_val;
					}
					UpdateView();
				}
				break;
			}
		}
	}

}


std::vector<Simulation *> ParametricGridData::GetRuns()
{
	return m_par.Runs;
}

bool ParametricGridData::RunSimulations(int row)
{
	for (size_t i = 0; i < m_par.Runs.size(); i++)
	{
		if (!m_valid_run[i])
		{
			m_par.Runs[i]->Clear();
			// override values here to handle copying
			for (int col = 0; col < m_cols; col++)
			{
				if (IsInput(col))
				{
					if (VarValue *vv = &m_par.Setup[col].Values[i])
					{
						// set for simulation
						m_par.Runs[i]->Override(m_var_names[col], *vv);
					}
				}
			}
			// Excel exchange if necessary
			ExcelExchange &ex = m_case->ExcelExch();
			if (ex.Enabled)
				ExcelExchange::RunExcelExchange(ex, m_case->Values(), m_par.Runs[i]);

			// invoke simulation
			//update results in grid - send message to grid to update
			if (m_par.Runs[i]->Invoke(true))
			{
				// update outputs
				for (int col = 0; col < m_cols; col++)
				{
					if (!IsInput(col))
					{
						if (VarValue *vv = m_par.Runs[i]->Outputs().Get(m_var_names[col]))
						{
							if (m_par.Setup[col].Values.size() > i)
								m_par.Setup[col].Values[i] = *vv;
							else
								m_par.Setup[col].Values.push_back(*vv);
						}
					}
				}
				// update row status
				if (i < m_valid_run.size())
					m_valid_run[i] = true;
				else // should not happen!
					m_valid_run.push_back(true);
				//			UpdateView();
			}
			else
			{
				wxShowTextMessageDialog(wxJoin(m_par.Runs[i]->GetErrors(), '\n'));
				return false;
			}
		}
	}
	return true;
}

void ParametricGridData::UpdateInputs(wxArrayString &input_names)
{
	for (size_t i = 0; i < input_names.size(); i++)
	{
		int ndx = m_par.FindSetup(input_names[i]);
		if (ndx < 0)
		{
			std::vector<VarValue> vvv;
			ParametricData::Var pv;
			for (int num_run = 0; num_run < m_rows; num_run++)
			{ // add values for inputs only
				if (VarValue *vv = m_case->Values().Get(input_names[i]))
					vvv.push_back(*vv);
			}
			pv.Name = input_names[i];
			pv.Values = vvv;
			AddSetup(pv);
		}
	}
	// remove any variables not selected
	wxArrayString to_remove;
	for (size_t i = 0; i < m_par.Setup.size(); i++)
	{
		wxString var_name = m_par.Setup[i].Name;
		if ((input_names.Index(var_name) == wxNOT_FOUND) && (IsInput(var_name)))
			to_remove.push_back(var_name);
	}
	for (size_t i = 0; i < to_remove.Count(); i++)
	{
		DeleteSetup(to_remove[i]);
	}
	m_input_names = input_names;
	UpdateSetup();
	// reset inputs

//	UpdateView();
}

void ParametricGridData::UpdateOutputs(wxArrayString &output_names)
{
	for (size_t i = 0; i < output_names.size(); i++)
	{
		int ndx = m_par.FindSetup(output_names[i]);
		if (ndx < 0)
		{
			std::vector<VarValue> vvv;
			ParametricData::Var pv;
			for (int num_run = 0; num_run < m_rows; num_run++)
			{ // add values for inputs only
				if (VarValue *vv = m_case->Values().Get(output_names[i]))
					vvv.push_back(*vv);
			}
			pv.Name = output_names[i];
			pv.Values = vvv;
			AddSetup(pv);
		}
	}
	// remove any variables not selected
	wxArrayString to_remove;
	for (size_t i = 0; i < m_par.Setup.size(); i++)
	{
		wxString var_name = m_par.Setup[i].Name;
		if ((output_names.Index(var_name) == wxNOT_FOUND) && (!IsInput(var_name)))
			to_remove.push_back(var_name);
	}
	for (size_t i = 0; i < to_remove.Count(); i++)
		DeleteSetup(to_remove[i]);
	m_output_names = output_names;
	UpdateSetup();
//	UpdateView();
}

void ParametricGridData::ClearResults()
{
	// clear inputs and outputs
	for (int col = 0; col < m_cols; col++)
		m_par.Setup[col].Values.clear();
	// clear simulation results
	for (int row = 0; row < m_rows; row++)
		m_par.Runs[row]->Clear();
}

wxGridCellAttr *ParametricGridData::GetAttr(int row, int col, wxGridCellAttr::wxAttrKind kind)
{

	wxGridCellAttr *attr = NULL;
	if (GetAttrProvider())
	{
		attr = GetAttrProvider()->GetAttr(row, col, kind);

		if (!IsInput(col))
		{ 
			if (!attr)
			{
				if ((row < m_valid_run.size()) && (m_valid_run[row]))
					attr = m_attr_for_valid_outputs;
				else
					attr = m_attr_for_invalid_outputs;
				attr->IncRef();
			}
			else
			{
				if (!attr->HasBackgroundColour())
				{
					wxGridCellAttr *attrNew = attr->Clone();
					attr->DecRef();
					attr = attrNew;
					if ((row < m_valid_run.size()) && (m_valid_run[row]))
						attr->SetBackgroundColour(m_color_for_valid_outputs);
					else
						attr->SetBackgroundColour(m_color_for_invalid_outputs);
				}
			}
		}
	}
	return attr;
}

