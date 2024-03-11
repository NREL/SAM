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

#include <iostream>
#include <fstream>

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
#ifdef __WXMSW__
#include <wex/ole/excelauto.h>
#endif

#include "parametric.h"
#include "main.h"
#include "results.h"
#include "casewin.h"
#include "variablegrid.h"


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
	for (size_t i = 0; i < rhs.QuickSetup.size(); i++)
	{
		QuickSetup.push_back(rhs.QuickSetup[i]);
	}
	QuickSetupMode = rhs.QuickSetupMode;
}

void ParametricData::ClearRuns()
{
	for( size_t i=0;i<Runs.size();i++ )
		delete Runs[i];
	Runs.clear();
}

// limits number of inputs to 65,536 on 32bit...
int ParametricData::FindSetup(wxString &name, bool IsInput)
{
	int ndx = -1;
	for (size_t i = 0; i < Setup.size(); i++)
	{
		if (Setup[i].Name == name && Setup[i].IsInput == IsInput)
		{
			ndx = (int)i;
			break;
		}
	}
	return ndx;
}

bool ParametricData::RemoveSetup(wxString &name, bool IsInput)
{
	bool removed = false;
	int ndx = FindSetup(name, IsInput);
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
	out.Write8( 5 ); // version

	out.Write32( Setup.size() );
	for( size_t i=0;i<Setup.size();i++ )
	{
		out.WriteString(Setup[i].Name);
		out.WriteString(Setup[i].varName);
		out.Write8(Setup[i].ndxHybrid);
		out.Write32( Setup[i].Values.size() );
		for( size_t k=0;k<Setup[i].Values.size();k++ )
			Setup[i].Values[k].Write( _O );
		// version 4 upgrade
		if (!Setup[i].IsInput)
			out.Write8(0);
		else
			out.Write8(1);
	}

	out.Write32( Runs.size() );
	for( size_t i=0;i<Runs.size();i++ )
		Runs[i]->Write( _O );

	out.Write32(QuickSetup.size());
	for (size_t i = 0; i < QuickSetup.size(); i++)
	{
		out.Write32(QuickSetup[i].Count());
		for (size_t k = 0; k < QuickSetup[i].Count(); k++)
			out.WriteString(QuickSetup[i].Item(k));
	}
	out.Write32(QuickSetupMode);

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
		if (ver > 4) {
			x.varName = in.ReadString();
			x.ndxHybrid = in.Read8();
		}
		else {
			x.varName = x.Name;
			x.ndxHybrid = 0;
		}
		size_t m = in.Read32();
		for( size_t k=0;k<m;k++ )
		{
			VarValue vv;
			vv.Read( _I );
			x.Values.push_back( vv );
		}
		// version 4
		if (ver > 3)
			x.IsInput = (in.Read8() > 0);
		else
			x.IsInput = true; // default assumption of input.
		Setup.push_back( x );
	}

	n = in.Read32();
	ClearRuns();
	for( size_t i=0;i<n;i++ )
	{
		Simulation *sim = new Simulation( m_case, wxString::Format("Parametric #%d",(int)(i+1)) );
		sim->Read( _I );
		Runs.push_back( sim );
	}

	if (ver > 1)
	{
		n = in.Read32();
		QuickSetup.clear();
		for (size_t i = 0; i < n; i++)
		{
			wxArrayString vals;
			size_t m = in.Read32();
			for (size_t k = 0; k < m; k++)
				vals.Add(in.ReadString());
			QuickSetup.push_back(vals);
		}
	}
	
	if (ver > 2)
		QuickSetupMode = in.Read32();
	else
		QuickSetupMode = 0;
	
	return in.Read8() == code;
}
////////////////////////////////////////////////////////////////////////////////////////////////
//    void EnablePasteEvent(bool b); // sent as GRID_CELL_CHANGE with GetRow() = -1 and GetCol() = -1
BEGIN_EVENT_TABLE(ParametricGrid, wxExtGridCtrl)
EVT_GRID_CELL_LEFT_CLICK(ParametricGrid::OnLeftClick)
EVT_GRID_COL_SORT(ParametricGrid::OnColSort)
EVT_GRID_CELL_CHANGE(ParametricGrid::OnGridCellChange)
END_EVENT_TABLE()


ParametricGrid::ParametricGrid(wxWindow *parent, wxWindowID id, const wxPoint &pos, const wxSize &size, long , const wxString &)
: wxExtGridCtrl(parent, id, pos, size)
{
} 

ParametricGrid::~ParametricGrid()
{
}

void ParametricGrid::OnGridCellChange(wxGridEvent& evt)
{
	if (evt.GetRow() == -1 && evt.GetCol() == -1) { // paste event
		// validation for library values ( SAM issue 1314)
		if (ParametricGridData* pgd = static_cast<ParametricGridData*>(GetTable())) {
			for (size_t iCol = 0; iCol < pgd->GetNumberCols(); iCol++) {
				if (pgd->IsInput(iCol)) {
					if (VarInfo* vi = pgd->GetVarInfo(0, iCol)) {
						if (vi->UIObject == "Library") { // MUST be set in UI Editor
							for (size_t iRow = 0; iRow < pgd->GetNumberRows(); iRow++) {
								if (wxUIObject* obj = wxUIObjectTypeProvider::Create(vi->UIObject)) {
									wxWindow* ctrl = obj->CreateNative(this);
									if (LibraryCtrl* ll = obj->GetNative<LibraryCtrl>()) {
										wxArrayString lib = vi->IndexLabels;
										if (lib.Count() > 0)
											ll->SetLibrary(lib[0], "*"); 
										auto val = pgd->GetValue(iRow, iCol);
										if (!ll->SetEntrySelection(val)) {
											wxMessageBox("Row " + wxString::Format("%d", (int)iRow + 1) + ": '" + val + "' not found in " + lib[0] + " library.", "Library Selection Error", wxOK | wxICON_ERROR);
											pgd->SetValue(iRow, iCol, ""); // fails for modules
											pgd->SetVarValue(iRow, iCol, NULL);
										}
									}
									ctrl->Destroy();
								}
							}
						}
					}
				}
			}
		}
	}
}


void ParametricGrid::OnLeftClick(wxGridEvent &evt)
{
	SetGridCursor(evt.GetRow(), evt.GetCol());
	evt.Skip();
}

void ParametricGrid::OnColSort(wxGridEvent& evt)
{
//	SetGridCursor(evt.GetRow(), evt.GetCol());
	evt.Skip();
	int col = evt.GetCol();
		// check to see if number (single value) for sorting
	if (ParametricGridData* pgd = static_cast<ParametricGridData*>(GetTable())) {
		if (col >= 0) { //-1,-1 is top left corner
			if (auto vv = pgd->GetVarValue(0, col)) { // assumption that all column values are of same type
				if (vv->Type() == VV_NUMBER) {
					// code to set indicator for column header in pgd->GetColLabelValue
					int sortCol = pgd->GetSortColumn();
					bool asc = true;
					if (col == sortCol)
						asc = !pgd->IsSortedAscending();
					//					else
					//						UnsetSortingColumn();
										//SetSortingColumn(col, asc); //- does not work for custom grids for indicator
										// end indicator code
										// actual sorting of data
					pgd->SortColumn(col, asc);
					AutoSizeColLabelSize(col);
				}
			}
		}
//		else {
//			// clear sorting 
//			pgd->ClearSorting();
//			AutoSizeColumns();
//		}
	}
	
}

////////////////////////////////////////////////////////////////////
enum { ID_FILTER_COLUMN_TYPE = wxID_HIGHEST + 294, ID_FILTER_COLUMN_CRITERIA, ID_FILTER_APPLY, ID_FILTER_REMOVE };

class FilterColumnDialog : public wxDialog
{
	wxComboBox* cboFilterType;
	wxNumericCtrl* numFilterCriteria;
	ParametricViewer::ColumnFilter m_sColumnFilter;
	wxButton* btnApplyFilter;
	wxButton* btnRemoveFilter;

public:

	FilterColumnDialog(wxWindow* parent, int id, ParametricViewer::ColumnFilter sColumnFilter, wxPoint position = wxDefaultPosition )
		: wxDialog(parent, id, "Filter column", position, wxScaleSize(600, 350),
			wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER), m_sColumnFilter(sColumnFilter)
	{
		//	enum ColumnFilterType {cft_less_than, cft_greater_than, cft_equal_to};
		wxArrayString choices;
		choices.Add("Is less than");
		choices.Add("Is greater than");
		choices.Add("Is equal to");
		cboFilterType = new wxComboBox(this, ID_FILTER_COLUMN_TYPE, choices[(int)m_sColumnFilter.filterType], wxDefaultPosition, wxDefaultSize, choices);
		cboFilterType->SetSelection((int)sColumnFilter.filterType);
		numFilterCriteria = new wxNumericCtrl(this, ID_FILTER_COLUMN_CRITERIA, sColumnFilter.filterCriteria);
		btnApplyFilter = new wxButton(this, ID_FILTER_APPLY, "Apply filter");
		btnRemoveFilter = new wxButton(this, ID_FILTER_REMOVE, "Remove filter");

		wxBoxSizer *szInputs = new wxBoxSizer(wxHORIZONTAL);
		szInputs->Add(cboFilterType, 0, wxLEFT | wxRIGHT | wxEXPAND, 1);
		szInputs->Add(numFilterCriteria, 0, wxLEFT | wxRIGHT | wxEXPAND, 1);

		wxBoxSizer* szButtons = new wxBoxSizer(wxHORIZONTAL);
		szButtons->Add(btnApplyFilter, 0, wxLEFT | wxRIGHT | wxEXPAND, 1);
		szButtons->Add(btnRemoveFilter, 0, wxLEFT | wxRIGHT | wxEXPAND, 1);

		wxBoxSizer* szMain = new wxBoxSizer(wxVERTICAL);
		szMain->Add(szInputs);
		szMain->Add(szButtons);

		SetSizerAndFit(szMain);
	}

	~FilterColumnDialog()
	{
	}

	ParametricViewer::ColumnFilter& GetColumnFilter()
	{
		return m_sColumnFilter;
	}

	void OnApplyFilter(wxCommandEvent&)
	{
		m_sColumnFilter.filterType = (ParametricViewer::ColumnFilterType)cboFilterType->GetSelection();
		m_sColumnFilter.filterCriteria = numFilterCriteria->Value();
		EndModal(wxID_OK);
	}

	void OnRemoveFilter(wxCommandEvent&)
	{
		m_sColumnFilter.filterColumn = -1;
		m_sColumnFilter.filterType = (ParametricViewer::ColumnFilterType)cboFilterType->GetSelection();
		m_sColumnFilter.filterCriteria = numFilterCriteria->Value();
		EndModal(wxID_OK);
	}


	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(FilterColumnDialog, wxDialog)
EVT_BUTTON(ID_FILTER_APPLY, FilterColumnDialog::OnApplyFilter)
EVT_BUTTON(ID_FILTER_REMOVE, FilterColumnDialog::OnRemoveFilter)
END_EVENT_TABLE()



////////////////////////////////////////////////////////////////////////////////////////////////

enum { ID_SELECT_INPUTS = wxID_HIGHEST+494, ID_SELECT_OUTPUTS, ID_NUMRUNS, 
	ID_RUN, ID_CLEAR, ID_GRID, ID_INPUTMENU_FILL_DOWN_SEQUENCE, 
	ID_INPUTMENU_FILL_DOWN_ONE_VALUE, ID_INPUTMENU_FILL_DOWN_EVENLY, 
	ID_OUTPUTMENU_ADD_PLOT, ID_OUTPUTMENU_REMOVE_PLOT, 
	ID_OUTPUTMENU_SHOW_DATA, ID_OUTPUTMENU_CLIPBOARD, 
	ID_OUTPUTMENU_CSV, ID_OUTPUTMENU_EXCEL, ID_CLEAR_SORTING, ID_FILTER_COLUMN, ID_CLEAR_FILTERS,
	ID_SHOW_ALL_INPUTS, ID_NEW_CASE, ID_QUICK_SETUP, ID_IMPORT, ID_EXPORT_MENU, ID_GEN_LK };



BEGIN_EVENT_TABLE(ParametricViewer, wxPanel)
	EVT_BUTTON(ID_SELECT_INPUTS, ParametricViewer::OnCommand)
	EVT_BUTTON(ID_SELECT_OUTPUTS, ParametricViewer::OnCommand)
	EVT_NUMERIC(ID_NUMRUNS, ParametricViewer::OnCommand)
	EVT_BUTTON(ID_RUN, ParametricViewer::OnCommand)
//	EVT_BUTTON(ID_GEN_LK, ParametricViewer::OnCommand)
	EVT_BUTTON(ID_QUICK_SETUP, ParametricViewer::OnCommand)
	EVT_BUTTON(ID_IMPORT, ParametricViewer::OnCommand)
	EVT_BUTTON(ID_EXPORT_MENU, ParametricViewer::OnCommand)
	
	EVT_BUTTON(ID_OUTPUTMENU_CLIPBOARD, ParametricViewer::OnMenuItem)
	EVT_BUTTON(ID_OUTPUTMENU_CSV, ParametricViewer::OnMenuItem)
	EVT_BUTTON(ID_OUTPUTMENU_EXCEL, ParametricViewer::OnMenuItem)
	EVT_BUTTON(ID_GEN_LK, ParametricViewer::OnMenuItem)
	EVT_BUTTON(ID_CLEAR, ParametricViewer::OnCommand)
	
	EVT_MENU(ID_OUTPUTMENU_CLIPBOARD, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_OUTPUTMENU_CSV, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_GEN_LK, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_CLEAR_SORTING, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_CLEAR_FILTERS, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_FILTER_COLUMN, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_OUTPUTMENU_EXCEL, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_CLEAR, ParametricViewer::OnCommand)

	EVT_GRID_CMD_COL_SORT(ID_GRID, ParametricViewer::OnGridColSort)
	EVT_GRID_CMD_LABEL_RIGHT_CLICK(ID_GRID, ParametricViewer::OnGridColLabelRightClick)
	EVT_MENU(ID_OUTPUTMENU_ADD_PLOT, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_OUTPUTMENU_REMOVE_PLOT, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_OUTPUTMENU_SHOW_DATA, ParametricViewer::OnMenuItem)
//	EVT_MENU(ID_OUTPUTMENU_CLIPBOARD, ParametricViewer::OnMenuItem)
//	EVT_MENU(ID_OUTPUTMENU_CSV, ParametricViewer::OnMenuItem)
//	EVT_MENU(ID_OUTPUTMENU_EXCEL, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_INPUTMENU_FILL_DOWN_ONE_VALUE, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_INPUTMENU_FILL_DOWN_SEQUENCE, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_INPUTMENU_FILL_DOWN_EVENLY, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_SHOW_ALL_INPUTS, ParametricViewer::OnMenuItem)
	EVT_MENU(ID_NEW_CASE, ParametricViewer::OnMenuItem)
	END_EVENT_TABLE()



ParametricViewer::ParametricViewer(wxWindow *parent, Case *cc)
	: wxPanel(parent, wxID_ANY), m_case(cc)
{
	m_current_graph = 0;

	wxSplitterWindow *splitter = new wxSplitterWindow(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_NOBORDER|wxSP_LIVE_UPDATE | wxSP_3DSASH);

	wxPanel *top_panel = new wxPanel(splitter);
	top_panel->SetBackgroundColour( wxMetroTheme::Colour( wxMT_FOREGROUND ) );

	wxBoxSizer *tool_sizer = new wxBoxSizer(wxHORIZONTAL);
	//tool_sizer->Add(new wxStaticText(top_panel, wxID_ANY, "Parametrics:"), 0, wxALIGN_CENTER_VERTICAL|wxRIGHT, 5);
	tool_sizer->Add(new wxMetroButton(top_panel, ID_QUICK_SETUP, "Quick setup..."), 0, wxALIGN_CENTER_VERTICAL, 0);
	tool_sizer->Add(new wxMetroButton(top_panel, ID_SELECT_INPUTS, "Inputs..."), 0, wxALL | wxEXPAND, 0);
	tool_sizer->Add(new wxMetroButton(top_panel, ID_SELECT_OUTPUTS, "Outputs..."), 0, wxALL | wxEXPAND, 0);
	tool_sizer->Add(new wxMetroButton(top_panel, ID_RUN, "Run simulations", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW), 0, wxALL | wxEXPAND, 0);
//	tool_sizer->Add(new wxMetroButton(top_panel, ID_GEN_LK, "Generate lk for SDKtool", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_RIGHTARROW), 0, wxALL | wxEXPAND, 0);
	tool_sizer->AddStretchSpacer();
	wxStaticText *lblruns = new wxStaticText(top_panel, wxID_ANY, "Number of runs:");
	lblruns->SetForegroundColour( *wxWHITE );
	tool_sizer->Add( lblruns, 0, wxALIGN_CENTER_VERTICAL|wxLEFT|wxRIGHT, 3);
	m_num_runs_ctrl = new wxNumericCtrl(top_panel, ID_NUMRUNS, 0, wxNUMERIC_INTEGER);
	wxSize bestsz( m_num_runs_ctrl->GetBestSize() );
	m_num_runs_ctrl->SetInitialSize( wxSize( bestsz.x/2, bestsz.y ) );
	m_num_runs_ctrl->SetEditable(true);
	tool_sizer->Add(m_num_runs_ctrl, 0, wxALL | wxALIGN_CENTER_VERTICAL, 0);
	m_run_multithreaded = new wxCheckBox(top_panel, wxID_ANY, "Run multi-threaded?", wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT);
	m_run_multithreaded->SetValue(true);
	m_run_multithreaded->Hide();

	//tool_sizer->Add(m_run_multithreaded, 0, wxALIGN_CENTER_VERTICAL, 1);
	tool_sizer->AddStretchSpacer();
	tool_sizer->Add(new wxMetroButton(top_panel, ID_IMPORT, "Import", wxNullBitmap, wxDefaultPosition, wxDefaultSize), 0, wxALL | wxEXPAND, 0);
	tool_sizer->Add(new wxMetroButton(top_panel, ID_EXPORT_MENU, "Export", wxNullBitmap, wxDefaultPosition, wxDefaultSize, wxMB_DOWNARROW), 0, wxALL | wxEXPAND, 0);



	/*
	// main grid menu (also available as right click in upper left of grid)
	wxBoxSizer *grid_menu_sizer = new wxBoxSizer(wxHORIZONTAL);
	grid_menu_sizer->Add(new wxButton(top_panel, ID_OUTPUTMENU_CLIPBOARD, "Copy to clipboard", wxDefaultPosition,wxDefaultSize,wxBU_EXACTFIT), 0, wxALIGN_CENTER_VERTICAL, 2);
	grid_menu_sizer->Add(new wxButton(top_panel, ID_OUTPUTMENU_CSV, "Save as CSV", wxDefaultPosition,wxDefaultSize,wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 2);
#ifdef __WXMSW__
	grid_menu_sizer->Add(new wxButton(top_panel, ID_OUTPUTMENU_EXCEL, "Send to Excel", wxDefaultPosition,wxDefaultSize,wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 2);
#endif
	grid_menu_sizer->AddStretchSpacer();
	grid_menu_sizer->Add(new wxButton(top_panel, ID_CLEAR, "Clear results", wxDefaultPosition,wxDefaultSize,wxBU_EXACTFIT), 0, wxALL | wxEXPAND, 2);
	*/

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
	par_sizer->Add(tool_sizer, 0, wxALL | wxEXPAND, 0);
	//par_sizer->Add(grid_menu_sizer, 0, wxALL | wxEXPAND, 2);
	par_sizer->Add(m_grid, 1, wxALL | wxEXPAND, 0);

	UpdateGrid();

	top_panel->SetSizer(par_sizer);
	m_layout = new wxSnapLayout(splitter, wxID_ANY);

	splitter->SetMinimumPaneSize(200);
	splitter->SplitHorizontally(top_panel, m_layout, (int)(550*wxGetScreenHDScale()));
	
	wxBoxSizer *main_sizer = new wxBoxSizer(wxHORIZONTAL);
	main_sizer->Add(splitter, 1, wxALL|wxEXPAND, 0);
	SetSizer(main_sizer);
	main_sizer->SetSizeHints(this);
	
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

wxString ParametricViewer::RunSimulationsFromMacro()
{
	if ((m_input_names.Count() <= 0) || (m_output_names.Count() <= 0))
	{
		return("You must set up parametric inputs and outputs before running parametric simulations. Incomplete parametric setup.");
	}

	RemoveAllPlots();
	m_grid_data->RunSimulations_multi();
	AddAllPlots();
	UpdateGrid();

	return wxString();
}

bool ParametricViewer::ImportFromMacro(wxString path) {
	bool ret = false;
	int row, col;
	wxArrayString empty;
	m_grid_data->UpdateInputs(empty);

	wxArrayString vals = getFromCSV(path, row, col);
	ImportData(vals, row, col);
	ret = m_grid->SetTable(m_grid_data);
	UpdateGrid();
	return ret;
}


bool ParametricViewer::ExportFromMacro(wxString path, bool asExcel) {
	if (asExcel) {
		wxString dat;
		GetTextData(dat, '\t');

		// strip commas per request from Paul 5/23/12 meeting
		dat.Replace(",", "");

#ifdef __WXMSW__
		wxExcelAutomation xl;
		if (!xl.StartExcel())
		{
			wxMessageBox("Could not start Excel.");
			return false;
		}
		
		if (!xl.NewWorkbook())
		{
			wxMessageBox("Could not create a new Excel worksheet.");
			return false;
		}
		if (wxTheClipboard->Open())
		{
			wxTheClipboard->SetData(new wxTextDataObject(dat));
			wxTheClipboard->Close();
			xl.PasteClipboard();
			if (xl.SaveClose(path)) return true;
			return false;
		}
#endif
		return false;


	}
	else {
		FILE *fp = fopen(path.c_str(), "w");
		if (!fp)
		{
			return false;
		}

		wxString dat;
		GetTextData(dat, ',');
		fputs(dat.c_str(), fp);
		fclose(fp);
	}
	return true;
}

bool ParametricViewer::SetInputFromMacro(wxString varName, int index, wxString val) {
	int c = m_grid_data->GetColumnForName(varName);
	if (c == wxNOT_FOUND)
		return false;
	if (index < 0 || index >= m_grid_data->GetNumberRows())
		return false;
	m_grid_data->SetValue(index, c, val);
	if (m_grid_data->GetValue(index, c) == val) {
		m_grid->SetTable(m_grid_data);
		UpdateGrid();
		return true;
	}
	return false;
}


void ParametricViewer::OnCommand(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_IMPORT:
	{
		wxArrayString empty;
		m_grid_data->UpdateInputs(empty);
		wxFileDialog openFileDialog(this, ("Open parametric table file"), "", "", "csv and excel files (*.csv; *.xlsx)|*.csv; *.xlsx", wxFD_OPEN | wxFD_FILE_MUST_EXIST);
		if (openFileDialog.ShowModal() == wxID_CANCEL) return;
		wxArrayString filePath = wxSplit(openFileDialog.GetPath(), '.');
		wxArrayString vals;
		int row, col;
		if (filePath[filePath.GetCount()-1].IsSameAs("xlsx", false)) {
			vals = getFromExcel(openFileDialog.GetPath(), row, col);
		}
		else {
			vals = getFromCSV(openFileDialog.GetPath(), row, col);
		}
		ImportData(vals, row, col);
		m_grid->SetTable(m_grid_data);
		UpdateGrid();
		break;
	}
	case ID_EXPORT_MENU:
		{
			wxMetroPopupMenu menu;
			menu.Append(ID_OUTPUTMENU_CLIPBOARD, "Copy to clipboard");
			menu.Append(ID_OUTPUTMENU_CSV, "Save as CSV");
#ifdef __WXMSW__
			menu.Append(ID_OUTPUTMENU_EXCEL, "Send to Excel");	
#endif
			menu.Append(ID_GEN_LK, "Generate LK for SDKTool");

			wxPoint p(wxDefaultPosition);
			wxWindow *btn = dynamic_cast<wxWindow*>( evt.GetEventObject() );
			if ( btn )
			{
				wxSize cs( btn->GetClientSize() );
				p = btn->ClientToScreen( wxPoint( cs.x, cs.y ) );
			}
			
			menu.Popup( btn ? btn : this, p, wxTOP|wxRIGHT );
		}
		break;
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
//	case ID_GEN_LK:
//		Generate_lk();
//		break;
	case ID_CLEAR:
		ClearResults();
		UpdateGrid();
		break;
	case ID_QUICK_SETUP:
		{
			Parametric_QS pqs(this, m_case);
			pqs.CenterOnParent();
			if (pqs.ShowModal() == wxID_OK)
			{
				m_grid->Freeze();
				m_grid_data->Init();
				m_num_runs_ctrl->SetValue(m_grid_data->GetNumberRows());
				m_input_names = m_grid_data->GetInputNames();
				m_output_names = m_grid_data->GetOutputNames();
				m_grid_data->UpdateInputs(m_input_names);
				m_grid_data->UpdateOutputs(m_output_names);
				m_grid->SetTable(m_grid_data);
				UpdateNumRuns();
				UpdateGrid();
				m_grid->Thaw();
			}
		}
		break;
	}
}

void ParametricViewer::OnMenuItem(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_OUTPUTMENU_ADD_PLOT:
		AddPlot(m_grid_data->GetVarName(0, m_selected_grid_col));
		break;
	case ID_OUTPUTMENU_REMOVE_PLOT:
		RemovePlot(m_grid_data->GetVarName(0, m_selected_grid_col));
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
	case ID_GEN_LK:
		Generate_lk();
		break;
#ifdef __WXMSW__
	case ID_OUTPUTMENU_EXCEL:
		SendToExcel();
		break;
#endif
	case ID_CLEAR_SORTING:
		m_grid_data->ClearSorting();
		UpdateGrid();
		RemoveAllPlots();
		AddAllPlots();
		break;
	case ID_CLEAR_FILTERS:
		m_columnFilters.clear();
		UpdateGrid();
		RemoveAllPlots();
		AddAllPlots();
		break;
	case ID_FILTER_COLUMN:
		FilterColumn(m_selected_grid_col);
		break;
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
		if ((int)m_grid_data->GetRuns().size() > m_grid_data->GetRunNumberForRowNumber(m_selected_grid_row)){
			if (m_grid_data->GetRuns()[m_grid_data->GetRunNumberForRowNumber(m_selected_grid_row)])	{
				std::vector<VarTable*> pvts;
				for (size_t i = 0; i < m_case->GetConfiguration()->Technology.size(); i++)
					pvts.push_back(m_grid_data->GetRuns()[m_grid_data->GetRunNumberForRowNumber(m_selected_grid_row)]->GetInputVarTable(i));
				new VariableGridFrame(this, &SamApp::Project(), m_case, &pvts, wxString::Format("Parametric run %d inputs", m_selected_grid_row + 1));
			}
		}
		break;
	case ID_NEW_CASE:
		if ((int)m_grid_data->GetRuns().size() > m_grid_data->GetRunNumberForRowNumber(m_selected_grid_row))
		{
			if (m_grid_data->GetRuns()[m_grid_data->GetRunNumberForRowNumber(m_selected_grid_row)])
			{
			//	new VariableGridFrame(this, &SamApp::Project(), m_case, m_grid_data->GetRuns()[m_selected_grid_row]->GetInputVarTable(), wxString::Format("Parametric run %d inputs", m_selected_grid_row + 1));
				// create new case with updated vartable
				if (Case* dup = dynamic_cast<Case*>(m_case->Duplicate()))
				{
					// update var table
					auto pvtParametric = m_grid_data->GetRuns()[m_grid_data->GetRunNumberForRowNumber(m_selected_grid_row)]->GetInputVarTable(0); // TODO:hybrids
					for (auto it = pvtParametric->begin(); it != pvtParametric->end(); ++it) {
						if (auto pvv = dup->Values(0).Get(it->first)) {
							if (pvv->Type() == it->second->Type())
								pvv->Copy(*it->second);
						}
					}

					wxString case_name = wxString::Format("Parametric run %d inputs", m_selected_grid_row + 1);
					SamApp::Project().AddCase(SamApp::Window()->GetUniqueCaseName(case_name), dup);
					SamApp::Window()->CreateCaseWindow(dup);
				}
			}
		}
		break;
	}
}

int ParametricViewer::GetColumnFiltersIndexForColumn(int& col)
{
	int ndx = -1;
	bool found = false;
	for (size_t i = 0; i < m_columnFilters.size() && !found; i++) {
		if (m_columnFilters[i].filterColumn == col) {
			found = true;
			ndx = i;
		}
	}
	return ndx;
}


void ParametricViewer::FilterColumn(int& col)
{
	int ndx = GetColumnFiltersIndexForColumn(col);
	ColumnFilter sColumnFilter(col);
	if (ndx > -1)
		sColumnFilter = m_columnFilters[ndx];
	FilterColumnDialog dlg(this, wxID_ANY, sColumnFilter);
	if (dlg.ShowModal() == wxID_OK) {
		sColumnFilter = dlg.GetColumnFilter();
		if (sColumnFilter.filterColumn > -1) {
			if (ndx > -1)
				m_columnFilters[ndx] = sColumnFilter;
			else
				m_columnFilters.push_back(sColumnFilter);
		}
		else {
			m_columnFilters.erase(m_columnFilters.begin() + ndx);
		}
		UpdateGrid();
		RemoveAllPlots();
		AddAllPlots();
	}

}


void ParametricViewer::GetTextData(wxString &dat, char sep)
{
	dat = wxEmptyString;
	if (!m_grid)
		return;

	size_t approxbytes = m_grid_data->GetNumberRows() * 15 * m_grid_data->GetNumberCols();
	dat.Alloc(approxbytes);

	int c;

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

	for (int r = 0; r<m_grid_data->GetNumberRows(); r++)
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

bool ParametricViewer::ImportAsNumber(wxString& vals, VarValue& vv) {
	double valNum = 0.0;
	if (vals.ToDouble(&valNum)) {
		vv.Set(valNum);		
		return true;
	}
	return false;
}

bool ParametricViewer::ImportAsArray(wxString& vals, VarValue& vv) {
	wxArrayString entries = wxSplit(vals, ';');
	if (entries.Count() < 2) return false;
	double valNum = 0.0;
	std::vector<double> arr;
	for (size_t i = 0; i < entries.Count() - 1; i++) {
		if (entries[i].ToDouble(&valNum)) {
			arr.push_back(valNum);
		}
		else {
			return false;
		}
	}
	vv.Set(arr);
	return true;
}

bool ParametricViewer::ImportAsMatrix(wxString& vals, VarValue& vv) {
	wxArrayString rows = wxSplit(vals, '[');
	if (rows.Count() < 2) return false;
	double valNum = 0.0;
	size_t nr = rows.Count() - 1;
	size_t nc = 0;
	double* valArr = nullptr;
	for (size_t i = 0; i < nr; i++) {
		if (rows[i + 1].Find(']') != -1) {
			wxArrayString entries = wxSplit(rows[i + 1].SubString(0, rows[i + 1].Len() - 2), ';');
			if (i == 0) {
				nc = entries.Count();
				valArr = new double[nr * nc];
			}
			for (size_t j = 0; j < nc; j++) {
				if (entries[j].ToDouble(&valNum)) {
					valArr[i*nc + j] = valNum;
				}
				else {
					return false;
				}
			}
		}
		else {
			return false;
		}
	}
	if (nc > 0) {
		vv.Set(valArr, nr, nc);
		delete valArr;
		return true;
	}
	return false;
}

bool ParametricViewer::ImportAsTable(wxString& vals, VarValue& vv) {
	wxArrayString entries = wxSplit(vals, '|');
	if (entries.Count() < 2) return false;
	VarTable vt;
	for (size_t v = 0; v < entries.Count(); v++) {
		wxArrayString var = wxSplit(entries[v], ':');
		if (var.Count() < 2) return false;
		wxArrayString typeVal = wxSplit(var[1], '=');
		int type = wxAtoi(typeVal[0]);
		VarValue vv;
		if (type == 1) {
			if (!ImportAsNumber(typeVal[1], vv)) return false;
		}
		else if (type == 2) {
			if (!ImportAsArray(typeVal[1], vv)) return false;
		}
		else if (type == 3) {
			if (!ImportAsMatrix(typeVal[1], vv)) return false;
		}
		else if (type == 4) {
			vv.Set(typeVal[1]);
		}
		else {
			return false;
		}
		vt.Set(var[0], vv);
	}
	vv.Set(vt);
	return true;
}

/* Values are rows and variables or labels are columns
void ParametricViewer::ImportData(wxArrayString& vals, int& row, int& col) {
	wxArrayString inputNames, outputNames;
	bool inputcol = true;
	wxArrayString allOutputNames, allOutputLabels;
	Simulation::ListAllOutputs(m_case->GetConfiguration(), &allOutputNames, &allOutputLabels, NULL, NULL, NULL);
	VarInfoLookup &vil = m_case->GetConfiguration()->Variables[0];  // TODO: hybrids


	for (int c = 0; c < col; c++) {
		if (vals[c*row].Len() == 0)
			continue;
		// get the VarInfo corresponding to column header
		wxArrayString splitUnit = wxSplit(vals[c*row], '(');
		wxString name = splitUnit[0]; // fails to get "(year 1)" values
		if (splitUnit.size() > 2)
			name = name + "(" + splitUnit[1];
		name = name.Trim();
		VarInfo* vi = vil.Lookup(name);
		inputcol = true;

		// if not name is not of variable, see if it's a label
		if (!vi) {
			wxString vn = vil.LookupByLabel(name);
			if (vn.Len() > 0) {
				vi = vil.Lookup(vn);
				// calculated variables are not inputs
				if (vi->Flags & VF_CALCULATED) {
					// issue with "Total installed cost" label is both SSC_INPUT total_installed_cost and SSC_OUTPUT total_cost
					inputcol = false;
					vi = NULL;
				}
				else
					name = vn;
			}
		}
		
		// if not input or already listed as input, see if output
		if (!vi || (inputNames.Index(name) != wxNOT_FOUND)) {
//			bool found = false;
			bool found = name.Lower()== "run";

			for (size_t i = 0; i < allOutputNames.size() && !found; i++)
			{
				if (name.IsSameAs(allOutputNames[i], false)) {
					outputNames.push_back(allOutputNames[i]);
					inputcol = false;
					found = true;
					break;
				}
				else if (name.IsSameAs(allOutputLabels[i].Trim(), false)) {
					outputNames.push_back(allOutputNames[i]);
					inputcol = false;
					found = true;
					break;
				}
			}
			if (found) continue;
			wxMessageBox("Error: could not identify parametric variable " + vals[c*row]);
			continue;
		}
		if (inputcol && !((vi->Flags & VF_PARAMETRIC) && !(vi->Flags & VF_INDICATOR) && !(vi->Flags & VF_CALCULATED))) {
			wxMessageBox("Error: " + name + " cannot be parametrized.");
			continue;
		}

		// import column values
		std::vector<VarValue> vvv;
		ParametricData::Var pv;
		int type = vi->Type;
		for (int r = 1; r < row; r++) {
			VarValue vv;
			if (vals[c*row + r].Len() == 0) {
				vv = vi->DefaultValue;
				vvv.push_back(vv);
				continue;
			}
			switch (type) {
			case VV_NUMBER:
				if (ImportAsNumber(vals[c*row + r], vv))
					vvv.push_back(vv);
				break;
			case VV_ARRAY:
				if (ImportAsArray(vals[c*row + r], vv))
					vvv.push_back(vv);
				break;
			case VV_MATRIX:
				if (ImportAsMatrix(vals[c*row + r], vv))
					vvv.push_back(vv);
				break;
			case VV_STRING:
				vv.Set(vals[c*row + r]);
				vvv.push_back(vv);
				break;
			case VV_TABLE:
				if (ImportAsTable(vals[c*row + r], vv))
					vvv.push_back(vv);
				break;
			case VV_BINARY:
			case VV_INVALID:
			default:
				break;
			}

		}
		pv.Values = vvv;
		pv.Name = name;
		pv.IsInput = inputcol;
		if (!m_grid_data->IsValid(pv)) {
			wxString typeS = m_case->BaseCase().GetInput(pv.Name, 0)->TypeAsString(); // TODO: hybrids
			wxString typeS2 = pv.Values[0].TypeAsString();
			wxString errorStr = "Import Error: Value type of " + vals[c*row] + " is {" + typeS2 + "}, should be {" + typeS + "}.";
			// some variables listed as {array} but can be single-value number
			if (typeS == "array" && typeS2 == "number") {
				errorStr += "\nTip: Insert ';' after a number to convert it to a single-entry array.";
			}
			wxMessageBox(errorStr);
		}
		m_grid_data->AddSetup(pv);
		inputNames.push_back(name);
	}
	m_input_names = inputNames;
	m_output_names = outputNames;
	m_grid_data->UpdateInputs(inputNames);
	m_grid_data->UpdateOutputs(outputNames);
	m_num_runs_ctrl->SetValue(row - 1);
	m_grid_data->UpdateNumberRows(row-1);
}
*/

void ParametricViewer::ImportData(wxArrayString& vals, int& row, int& col) {
	wxArrayString inputNames, outputNames;
	bool inputrow = true;
	wxArrayString allOutputNames, allOutputLabels;
	Simulation::ListAllOutputs(m_case->GetConfiguration(), &allOutputNames, &allOutputLabels, NULL, NULL, NULL);
//	VarInfoLookup& vil = m_case->GetConfiguration()->Variables[0];  // TODO: hybrids


	for (int r = 0; r < row; r++) {
		if (vals[r * col].Len() == 0)
			continue;
		VarInfo* vi = NULL;
		// split into Label | ndxHybrid | variable name
		wxString name, varName;
		int ndxHybrid = 0;
		wxArrayString splLabelNdxHybridVarName = wxSplit(vals[r], '|');
		if (splLabelNdxHybridVarName.size() == 3) {
			// get the VarInfo corresponding to row header
			wxArrayString splitUnit = wxSplit(splLabelNdxHybridVarName[0], '(');
			name = splitUnit[0]; // fails to get "(year 1)" values
			if (splitUnit.size() > 2)
				name = name + "(" + splitUnit[1];
			name = name.Trim();
			if (!splLabelNdxHybridVarName[1].Trim().ToInt(&ndxHybrid))
				continue;
			VarInfoLookup& vil = m_case->GetConfiguration()->Variables[ndxHybrid];

			varName = splLabelNdxHybridVarName[2].Trim();
			varName = varName.Trim(false);
			vi = vil.Lookup(varName);
		}
		else {
			name = vals[r];
			varName = name;
		}

		inputrow = true;
		/*
		// if not name is not of variable, see if it's a label
		if (!vi) 
		{
			wxString vn = vil.LookupByLabel(name);
			if (vn.Len() > 0) {
				vi = vil.Lookup(vn);
				// calculated variables are not inputs
				if (vi->Flags & VF_CALCULATED) {
					// issue with "Total installed cost" label is both SSC_INPUT total_installed_cost and SSC_OUTPUT total_cost
					inputrow = false;
					vi = NULL;
				}
				else
					name = vn;
			}
		}
		*/
		// if not input or already listed as input, see if output
		if (!vi || (inputNames.Index(name) != wxNOT_FOUND)) {
			//			bool found = false;
			bool found = name.Lower() == "run";

			for (size_t i = 0; i < allOutputNames.size() && !found; i++)
			{
				if (name.IsSameAs(allOutputNames[i], false)) {
					outputNames.push_back(allOutputNames[i]);
					inputrow = false;
					found = true;
					break;
				}
				else if (name.IsSameAs(allOutputLabels[i].Trim(), false)) {
					outputNames.push_back(allOutputNames[i]);
					inputrow = false;
					found = true;
					break;
				}
			}
			if (found) continue;
			wxMessageBox("Error: could not identify parametric variable " + vals[r * col]);
			continue;
		}
		if (inputrow && !((vi->Flags & VF_PARAMETRIC) && !(vi->Flags & VF_INDICATOR) && !(vi->Flags & VF_CALCULATED))) {
			wxMessageBox("Error: " + name + " cannot be parametrized.");
			continue;
		}

		// import column values
		std::vector<VarValue> vvv;
		ParametricData::Var pv;
		int type = vi->Type;
		for (int c = 1; c < col; c++) {
			VarValue vv;
			if (vals[c * row + r].Len() == 0) {
				vv = vi->DefaultValue;
				vvv.push_back(vv);
				continue;
			}
			switch (type) {
			case VV_NUMBER:
				if (ImportAsNumber(vals[c * row + r], vv))
					vvv.push_back(vv);
				break;
			case VV_ARRAY:
				if (ImportAsArray(vals[c * row + r], vv))
					vvv.push_back(vv);
				break;
			case VV_MATRIX:
				if (ImportAsMatrix(vals[c * row + r], vv))
					vvv.push_back(vv);
				break;
			case VV_STRING:
				vv.Set(vals[c * row + r]);
				vvv.push_back(vv);
				break;
			case VV_TABLE:
				if (ImportAsTable(vals[c * row + r], vv))
					vvv.push_back(vv);
				break;
			case VV_BINARY:
			case VV_INVALID:
			default:
				break;
			}

		}
		pv.Values = vvv;
		pv.Name = name;
		pv.varName = varName; 
		pv.IsInput = inputrow;
		pv.ndxHybrid = ndxHybrid;
		if (!m_grid_data->IsValid(pv)) {
			wxString typeS = m_case->BaseCase().GetInput(pv.varName, ndxHybrid)->TypeAsString(); 
			wxString typeS2 = pv.Values[0].TypeAsString();
			wxString errorStr = "Import Error: Value type of " + vals[r * col] + " is {" + typeS2 + "}, should be {" + typeS + "}.";
			// some variables listed as {array} but can be single-value number
			if (typeS == "array" && typeS2 == "number") {
				errorStr += "\nTip: Insert ';' after a number to convert it to a single-entry array.";
			}
			wxMessageBox(errorStr);
		}
		m_grid_data->AddSetup(pv);
		inputNames.push_back(name);
	}
	m_input_names = inputNames;
	m_output_names = outputNames;
	m_grid_data->UpdateInputs(inputNames);
	m_grid_data->UpdateOutputs(outputNames);
	m_num_runs_ctrl->SetValue(col - 1);
	m_grid_data->UpdateNumberRows(col - 1);
}


void ParametricViewer::CopyToClipboard()
{
	wxBusyInfo busy("Processing data table... please wait");
	wxString dat;

	wxString sep = ",";
	wxString header = "run";
	//  e.g. run,1,2,3,...
	for (int run = 1; run < m_grid_data->GetNumberRows() + 1; run++) {
		header += sep + wxString::Format("%d", run);
	}
	header += "\n";
	dat << header;

	for (int col = 0; col < m_grid_data->GetNumberCols(); col++) {
		if (VarValue* vv = m_grid_data->GetVarValue(0, col)) {
			wxString apddat;
			if (vv->Type() == VV_STRING) {
				apddat = m_grid_data->GetColLabelValue(col);

				for (int row = 0; row < m_grid_data->GetNumberRows(); row++)
					apddat += "," + m_grid_data->GetValue(row, col);
				apddat += "\n";
			}
			else {

				std::vector<std::vector<double> > values_vec;
				wxArrayString labels;
				for (int row = 0; row < m_grid_data->GetNumberRows(); row++)
				{
					std::vector<double> vec = m_grid_data->GetArray(row, col);
					if (vec.size() == 0) // single values
						vec.push_back(m_grid_data->GetDouble(row, col));
					values_vec.push_back(vec);
					labels.push_back(wxString::Format("Run %d", row + 1));
				}
				ArrayPopupDialog apd(this, m_grid_data->GetColLabelValue(col).ToAscii(' '), labels, values_vec);

				apd.GetParametricTextData(apddat, ',');
			}
			dat << apddat;
		}
	}

	if (wxTheClipboard->Open())
	{
		wxTheClipboard->SetData(new wxTextDataObject(dat));
		wxTheClipboard->Close();
	}
}


wxArrayString ParametricViewer::getFromCSV(const wxString& input_name, int& row, int& col) {
	wxArrayString vals;
	wxCSVData csv;
	csv.SetSeparator(',');
	csv.ReadFile(input_name);
	row = csv.NumRows();
	col = csv.NumCols();
	for (int c = 0; c < col; c++) {
		for (int r = 0; r < row; r++) {
			wxString val = csv(r, c);
			if (val.Len() > 0 && (val[0] == '$')) {
				val = val.Mid(1);
			}
			val = val.Trim();
			vals.push_back(val);
		}
	}
	csv.Clear();
	return vals;
}

wxArrayString ParametricViewer::getFromExcel(const wxString& input_name, int& row, int& col) {
#ifdef __WXMSW__
	wxArrayString vals;
	wxExcelAutomation xl;
	xl.StartExcel();
	xl.OpenFile(input_name);
	xl.getUsedCellRange(row, col, vals);
	xl.CloseAllNoSave();
	return vals;
#else
	return wxArrayString();
#endif // __WXMSW__
}

void ParametricViewer::SaveToCSV()
{
	wxFileDialog fdlg(this, "Save as CSV", wxEmptyString, "results.csv", "Comma-separated values (*.csv)|*.csv", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
	if (fdlg.ShowModal() != wxID_OK) return;


	std::ofstream fp(fdlg.GetPath().ToStdString(), std::ios::out | std::ios::binary);
	if (!fp.is_open())
	{
		wxMessageBox("Could not open file for write:\n\n" + fdlg.GetPath());
		return;
	}

	wxBusyInfo busy("Writing CSV file... please wait");

	wxString sep = ",";
	wxString header = "run";
	//  e.g. run,1,2,3,...
	for (int run = 1; run < m_grid_data->GetNumberRows()+1; run++) {
		header += sep + wxString::Format("%d", run);
	}
	header += "\n";
	fp << header;

	for (int col = 0; col < m_grid_data->GetNumberCols(); col++) {
		if (VarValue* vv = m_grid_data->GetVarValue(0, col)) {
			wxString title = m_grid_data->GetColLabelValue(col) + wxString::Format(" | %d | ", m_grid_data->GetNdxHybrid(0, col)) + m_grid_data->GetVarName(0, col);

			wxString dat;
			if (vv->Type() == VV_STRING) {
//				dat = '"' + m_grid_data->GetColLabelValue(col) + '"';
				dat = '"' + title + '"';

				for (int row = 0; row < m_grid_data->GetNumberRows(); row++)
					dat += "," + m_grid_data->GetValue(row, col);
				dat += "\n";
			}
			else {
				std::vector<std::vector<double> > values_vec;
				wxArrayString labels;
				for (int row = 0; row < m_grid_data->GetNumberRows(); row++)
				{
					std::vector<double> vec = m_grid_data->GetArray(row, col);
					if (vec.size() == 0) // single values
						vec.push_back(m_grid_data->GetDouble(row, col));
					values_vec.push_back(vec);
					labels.push_back(wxString::Format("Run %d", row + 1));
				}
//				ArrayPopupDialog apd(this, m_grid_data->GetColLabelValue(col).ToAscii(' '), labels, values_vec);
				ArrayPopupDialog apd(this, title.ToAscii(' '), labels, values_vec);

				apd.GetParametricTextData(dat, ',');
			}
			fp << dat;
		}
	}
	fp.close();


}

void ParametricViewer::SendToExcel()
{
	wxBusyInfo busy("Processing data table... please wait");

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

	for (int col = m_grid_data->GetNumberCols()-1; col >= 0; col--) { // reorder to match list of Excel sheets with parametric table
		if (VarValue* vv = m_grid_data->GetVarValue(0, col)) {
			wxString sheetName = m_grid_data->GetColLabelValue(col).ToAscii(' ');
			// valid sheet names - no \ / ? * [ ] and 31 characters max
			sheetName.Replace("\\", "_");
			sheetName.Replace("/", "_");
			sheetName.Replace("?", "_");
			sheetName.Replace("*", "_");
			sheetName.Replace("[", "_");
			sheetName.Replace("]", "_");
			if (m_grid_data->IsInput(col)) sheetName = "IN_" + sheetName;
			sheetName = sheetName.Left(31);

			if (vv->Type() == VV_STRING) {
				wxString header = "Index\t";
				wxString dat = "0\t";
				for (int row = 0; row < m_grid_data->GetNumberRows(); row++) {
					header += wxString::Format("Run %d", row + 1);
					dat += m_grid_data->GetValue(row, col);
					if (row < m_grid_data->GetNumberRows() - 1) {
						header += '\t';
						dat += '\t';
					}
					else {
						header += '\n';
						dat += '\n';
					}
				}
				dat = header + dat;
				dat.Replace(",", "");
				xl.PasteNewWorksheet(sheetName, dat);
			}
			else {
				std::vector<std::vector<double> > values_vec;
				wxArrayString labels;
				for (int row = 0; row < m_grid_data->GetNumberRows(); row++)
				{
					std::vector<double> vec = m_grid_data->GetArray(row, col);
					if (vec.size() == 0) // single values
						vec.push_back(m_grid_data->GetDouble(row, col));
					values_vec.push_back(vec);
					labels.push_back(wxString::Format("Run %d", row + 1));
				}
				ArrayPopupDialog apd(this, m_grid_data->GetColLabelValue(col).ToAscii(' '), labels, values_vec);
				apd.SendToExcelSheet(xl, sheetName);
			}
		}
	}

#endif
}


void ParametricViewer::OnGridColSort(wxGridEvent& evt)
{
	RemoveAllPlots();
	AddAllPlots();
}

void ParametricViewer::OnGridColLabelRightClick(wxGridEvent & evt)
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
			if (m_grid_data->IsSorted()) {
				menu->AppendSeparator();
				menu->Append(ID_CLEAR_SORTING, _T("Clear column sorting"));
			}
			if (m_columnFilters.size() > 0) {
				menu->AppendSeparator();
				menu->Append(ID_CLEAR_FILTERS, _T("Clear all column filters"));
			}
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
				if (auto vv = m_grid_data->GetVarValue(0, m_selected_grid_col)) {
					if (vv->Type() == VV_NUMBER) {
						menu->AppendSeparator();
						menu->Append(ID_FILTER_COLUMN, _T("Filter column"));
					}
				}
				PopupMenu(menu, point);
			}
			else
			{
				//	Output menu
				wxPoint point = evt.GetPosition();
				wxMenu *menu = new wxMenu;
				menu->Append(ID_OUTPUTMENU_ADD_PLOT, _T("Add plot"));
				menu->Append(ID_OUTPUTMENU_REMOVE_PLOT, _T("Remove plot"));
				menu->AppendSeparator();
				menu->Append(ID_OUTPUTMENU_SHOW_DATA, _T("Show all data"));
				int ndx = m_plot_var_names.Index(m_grid_data->GetVarName(0,m_selected_grid_col));
				menu->Enable(ID_OUTPUTMENU_ADD_PLOT, (ndx == wxNOT_FOUND));
				menu->Enable(ID_OUTPUTMENU_REMOVE_PLOT, (ndx != wxNOT_FOUND));
				if (auto vv = m_grid_data->GetVarValue(0, m_selected_grid_col)) {
					if (vv->Type() == VV_NUMBER) {
						menu->AppendSeparator();
						menu->Append(ID_FILTER_COLUMN, _T("Filter column"));
					}
				}
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
		menu->Append(ID_NEW_CASE, _T("Create new case"));
		PopupMenu(menu, point);
	}
}


void ParametricViewer::ShowAllData()
{
	int col = m_selected_grid_col;
	std::vector<std::vector<double> > values_vec;
	wxArrayString labels;
	for (int row = 0; row < m_grid_data->GetNumberRows(); row++)
	{
		std::vector<double> vec = m_grid_data->GetArray(row, col);
		if (vec.size() == 0) // single values
			vec.push_back(m_grid_data->GetDouble(row, col));
		values_vec.push_back(vec);
		labels.push_back(wxString::Format("Run %d", row + 1));
	}
	ArrayPopupDialog apd(this, m_grid_data->GetColLabelValue(col).ToAscii(' '), labels, values_vec);
	apd.ShowModal();
}

void ParametricViewer::FillDown(int rows)
{
	// saves current editor value
	m_grid->SaveEditControlValue();
	// get first two values in column and fill down
	int col = m_selected_grid_col;
	if (rows<0)
		m_grid_data->FillEvenly(col);
	else
		m_grid_data->FillDown(col,rows);
}

bool ParametricViewer::IsLineInputs()
{
	bool retVal = false;
	if (m_input_names.Count() == 1) {
//		if (VarValue* vv = m_case->Values(0).Get(m_input_names[0])) // TODO: hybrid 
		if (VarValue* vv = m_case->Values(m_grid_data->GetParametricData().Setup[0].ndxHybrid).Get(m_grid_data->GetParametricData().Setup[0].varName))
			retVal = (vv->Type() == VV_NUMBER);
	}
	return retVal;
}

bool ParametricViewer::IsContourInputs()
{
	bool retVal = false;
	if (m_input_names.Count() == 2) {
//		if (VarValue* vv1 = m_case->Values(0).Get(m_input_names[0])) {
//			if (VarValue* vv2 = m_case->Values(0).Get(m_input_names[1])) {
		if (VarValue* vv1 = m_case->Values(m_grid_data->GetParametricData().Setup[0].ndxHybrid).Get(m_grid_data->GetParametricData().Setup[0].varName)) {
			if (VarValue* vv2 = m_case->Values(m_grid_data->GetParametricData().Setup[0].ndxHybrid).Get(m_grid_data->GetParametricData().Setup[1].varName)) {
				retVal = ((vv1->Type() == VV_NUMBER) && (vv2->Type() == VV_NUMBER));
			}
		}
	}
	return retVal;
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
				// line plot if single input and contour if two inputs
				if (IsLineInputs()) {
					g.Type = Graph::LINE;
					//g.Size = 3;
					ret_val = true;
					g.YLabel = m_grid_data->GetColLabelValue(col).ToAscii(' ');
					if (auto pxvv = m_grid_data->GetVarValue(0, 0)) {
						if (VarInfo* vi = m_grid_data->GetVarInfo(0, 0)) {
							g.XLabel = vi->Label;
							g.X.push_back(m_grid_data->GetVarName(0, 0));
							if (!m_grid_data->GetUnits(col).IsEmpty()) {
								g.XLabel += " (" + vi->Units + ")";
							}
							g.ShowLegend = false;
						}
					}
					else {
						g.XLabel = "Run number";
					}
				}
				else if (IsContourInputs()) {
					// contour plot
					g.Type = Graph::CONTOUR;
					g.Size = 3; // bar size
					ret_val = true;
					g.YLabel = m_grid_data->GetColLabelValue(col).ToAscii(' ');
					if (auto pxvv = m_grid_data->GetVarValue(0, 0)) {
						if (VarInfo* vi = m_grid_data->GetVarInfo(0, 0)) {
							g.XLabel = vi->Label;
							g.X.push_back(m_grid_data->GetVarName(0, 0));
							if (!m_grid_data->GetUnits(col).IsEmpty()) {
								g.XLabel += " (" + vi->Units + ")";
							}
							g.ShowLegend = false;
						}
					}
					if (auto pyvv = m_grid_data->GetVarValue(0, 1)) {
						if (VarInfo* vi = m_grid_data->GetVarInfo(0, 1)) {
							g.YLabel = vi->Label;
							g.X.push_back(m_grid_data->GetVarName(0, 1));
							if (!m_grid_data->GetUnits(col).IsEmpty()) {
								g.YLabel += " (" + vi->Units + ")";
							}
							g.ShowLegend = false;
						}
					}
					g.Title = m_grid_data->GetColLabelValue(col).ToAscii(' ');
				}
				else { // default behavior in past versions
					g.Type = Graph::BAR;
					g.Size = 15; // bar size
					ret_val = true;
					g.YLabel = m_grid_data->GetColLabelValue(col).ToAscii(' ');
					if (!m_grid_data->GetUnits(col).IsEmpty())
						g.YLabel += " (" + m_grid_data->GetUnits(col) + ")";
					g.XLabel = "Run number";
				}
				break;
				// arrays - determine if monthly or hourly
			}
			case VV_ARRAY:
			{
				size_t n;
				m_grid_data->GetArray(0, col, &n); // checked above for rows>0

				if (n == 12) // assume monthly
				{
					g.Type = Graph::BAR;
					g.YLabel = m_grid_data->GetColLabelValue(col).ToAscii(' ');
					g.XLabel = "Run number";
					ret_val = true;
				}
				else if (n == 8760)
				{
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
	for (int i = 0; i < (int)m_output_names.Count(); i++)
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


void ParametricViewer::AddPlot(const wxString& output_name)
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
					GraphCtrl* gc = new GraphCtrl(m_layout, wxID_ANY);
					if (m_input_names.Count() > 2) { // previous bar graph behavior with runs sorted
						std::vector<Simulation*> orderedSims;
						auto rowOrder = m_grid_data->GetRowSortOrder();
						auto unorderedSim = m_grid_data->GetRuns();
						if (unorderedSim.size() == rowOrder.size()) {
							for (size_t i = 0; i < rowOrder.size(); i++)
								orderedSims.push_back(unorderedSim[rowOrder[i]]);
							gc->Display(orderedSims, g);
						}
						else {
							gc->Display(m_grid_data->GetRuns(), g);
						}
					}
					else {
						if ((IsLineInputs() || IsContourInputs()) && (g.X.size()!=0))
							gc->DisplayParametrics(m_grid_data->GetRuns(), g);
						else // old type monthly, etc
							gc->Display(m_grid_data->GetRuns(), g);
					}
					m_graphs.push_back(gc);
					m_layout->Add(gc, 800, 400);
				}
				else // DView
				{
					wxDVTimeSeriesCtrl* dv = new wxDVTimeSeriesCtrl(this, wxID_ANY, wxDV_RAW, wxDV_AVERAGE);
					for (int row = 0; row < m_grid_data->GetRowsCount(); row++)
					{
						size_t n;
						double* y = m_grid_data->GetArray(row, col, &n);
						size_t steps_per_hour = n / 8760;
						if (steps_per_hour > 0
							&& steps_per_hour <= 60
							&& n == steps_per_hour * 8760)
						{
							dv->AddDataSet(new TimeSeriesData(y, n, 1.0 / steps_per_hour, 0.0, // by default not instantaneous values for hourly data
								m_grid_data->GetColLabelValue(col).ToAscii(' ') + wxString::Format(" : run(%d)", (int)(row + 1)),
								m_grid_data->GetUnits(col)), true);
							dv->SelectDataSetAtIndex(row);
						}
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
	// check that inputs and outputs are selected
	if ((m_input_names.Count() <= 0) || (m_output_names.Count() <= 0))
	{
		wxMessageBox("You must set up parametric inputs and outputs before running parametric simulations.", "Incomplete parametric setup");
		return;
	}

	RemoveAllPlots();
	if (m_run_multithreaded->GetValue())
		m_grid_data->RunSimulations_multi();
	else
		m_grid_data->RunSimulations_single();
	AddAllPlots();
	UpdateGrid();
}

void ParametricViewer::Generate_lk()
{
	// check that inputs and outputs are selected
	if ((m_input_names.Count() <= 0) || (m_output_names.Count() <= 0))
	{
		wxMessageBox("You must set up parametric inputs and outputs before generating lk scripts.", "Incomplete parametric setup");
		return;
	}

	m_grid_data->Generate_lk();
}


void ParametricViewer::ClearResults()
{
	RemoveAllPlots();
	m_grid_data->ClearResults();
}

void ParametricViewer::SelectInputs()
{
	wxArrayString names, labels;
	wxString case_name(SamApp::Project().GetCaseName(m_case));

	ConfigInfo *ci = m_case->GetConfiguration();
	if (!ci) return;

	SelectVariableDialog dlg(this, "Select Inputs");


	for (size_t ndxHybrid = 0; ndxHybrid < ci->Technology.size(); ndxHybrid++) {
		VarInfoLookup& vil = ci->Variables[ndxHybrid]; 

		for (VarInfoLookup::iterator it = vil.begin(); it != vil.end(); ++it)
		{
			wxString name = it->first;
			if (ci->Technology.size() > 1) name = ci->Technology[ndxHybrid].Lower() + "_" + name;
			VarInfo& vi = *(it->second);

			// update to select only "Parametric" variables and NOT calculated variables
			if ((vi.Flags & VF_PARAMETRIC) && !(vi.Flags & VF_INDICATOR) && !(vi.Flags & VF_CALCULATED))
			{
				wxString label = dlg.PrettyPrintLabel(name, vi);
				labels.Add(label);
				names.Add(name);
			}
		}
	}
	wxSortByLabels(names, labels);
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames(m_input_names);
	if (dlg.ShowModal() == wxID_OK)
	{
		RemoveAllPlots();
		m_input_names = dlg.GetCheckedNames();

		m_grid_data->UpdateInputs(m_input_names);
	}
}

void ParametricViewer::SelectOutputs()
{
	wxArrayString output_names, output_labels;
	wxArrayString names, labels, units, groups, types;
	Simulation::ListAllOutputs(m_case->GetConfiguration(),
		&names, &labels, &units, &groups, &types);

	SelectVariableDialog dlg(this, "Select Outputs");
	for (size_t i = 0; i<labels.size(); i++)
	{
		wxString label = dlg.PrettyPrintLabel(names[i], labels[i], types[i], units[i], groups[i], true );
		output_labels.Add(label);
		output_names.Add(names[i]);
	}

	wxSortByLabels(output_names, output_labels);
	dlg.SetItems(output_names, output_labels);
	dlg.SetCheckedNames(m_output_names);
	if (dlg.ShowModal() == wxID_OK)
	{
		RemoveAllPlots();
		m_output_names = dlg.GetCheckedNames();

		m_grid_data->UpdateOutputs(m_output_names);
	}
}

bool ParametricViewer::ShowRow(int& row)
{ // assumption at this point is VV_NUMBER type column to setup a column filter
	bool showRow = true;
	for (size_t i = 0; i < m_columnFilters.size() && showRow; i++) {
		int col = m_columnFilters[i].filterColumn;
		if (auto vv = m_grid_data->GetVarValue(row, col)) {
			if (vv->Type() == VV_NUMBER) {// extra paranoid
				switch (m_columnFilters[i].filterType) {
				case cft_less_than:
					showRow = (vv->Value() < m_columnFilters[i].filterCriteria);
					break;
				case cft_greater_than:
					showRow = (vv->Value() > m_columnFilters[i].filterCriteria);
					break;
				case cft_equal_to: // may need tolerance
					showRow = (vv->Value() == m_columnFilters[i].filterCriteria);
					break;
				default:
					showRow = false;
					break;
				}
			}
		}
	}
	return showRow;
}


void ParametricViewer::UpdateGrid()
{
	// update grid data with m_par updates from configure and number of runs
	// cancel editing of cell
	//m_grid->HideCellEditControl();
	//m_grid->SaveEditControlValue();

//	m_grid->SetTable(m_grid_data);
	//m_grid->ForceRefresh();
	// setting with attr in table base
	
	m_grid->Freeze();
	for (int col = 0; col < m_grid_data->GetNumberCols(); col++)
	{
		for (int row = 0; row < m_grid_data->GetNumberRows(); row++)
		{
			if (m_grid_data->IsInput(col))
				m_grid->SetReadOnly(row, col, false);
			else
			{
//				bool readonly = (m_grid_data->GetTypeName(row, col) == wxGRID_VALUE_STRING);
//				m_grid->SetReadOnly(row, col, readonly);
			}
		}
	}
	
	for (int row = 0; row < m_grid_data->GetNumberRows(); row++) {
		if (ShowRow(row))
			m_grid->ShowRow(row);
		else
			m_grid->HideRow(row);
	}

	m_grid->AutoSizeColumns();
	m_grid->Thaw();
}


////////////////////////////////////////////////////////////////////////////////////////////////

// m_par contains list of inputs and outputs
ParametricGridData::ParametricGridData(Case *cc) :m_par(cc->Parametric()), m_case(cc)
{
	m_color_for_inputs = wxColour("LIGHT BLUE");
	m_color_for_valid_outputs = wxColour(145,210,142);
	m_color_for_invalid_outputs = wxColour(235, 235, 235);
	m_attr_for_inputs = new wxGridCellAttr;
	m_attr_for_inputs->SetBackgroundColour(m_color_for_inputs);
	m_attr_for_valid_outputs = new wxGridCellAttr;
	m_attr_for_valid_outputs->SetBackgroundColour(m_color_for_valid_outputs);
	m_attr_for_invalid_outputs = new wxGridCellAttr;
	m_attr_for_invalid_outputs->SetBackgroundColour(m_color_for_invalid_outputs);

	m_rows = 0;
	m_cols = 0;
	ClearSorting();
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
	m_col_hdrs.Clear();
	m_input_names.Clear();
	m_output_names.Clear();
	m_var_names.Clear();
	m_valid_run.clear();

	m_cols = m_par.Setup.size();

	for (int i = 0; i < m_cols; i++)
	{
		wxString var_name = m_par.Setup[i].Name;
		if (IsInput(m_par.Setup[i]))
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

	// set m_valid_runs according to data that is read in
	for (int i = 0; i < m_rows; i++)
		m_valid_run.push_back(true);
}

void ParametricGridData::UpdateSetup()
{ // assumes inputs or outputs have been updated
	// sorts m_var_name and m_par.Setup based in inputs first and then outputs.
// used instead of inserting inputs so that additional sorting can be performed
	std::vector<ParametricData::Var> sorted_setup;
	int i, ndx;
	for (i = 0; i < (int)m_input_names.Count(); i++)
	{
		ndx = m_par.FindSetup(m_input_names[i], true);
		if (ndx < 0)
		{
			// throw error?
			wxMessageBox("Not here input");
			return;
		}
		sorted_setup.push_back(m_par.Setup[ndx]);
	}

	for (i = 0; i < (int)m_output_names.Count(); i++)
	{
		ndx = m_par.FindSetup(m_output_names[i], false);
		if (ndx < 0)
		{
			// throw error?
			wxMessageBox("Not here output");
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
	{
		return m_par.Setup[col].IsInput;
	}
	else
		return false;
}

bool ParametricGridData::IsInput(ParametricData::Var &var)
{
	return var.IsInput;
}


wxString ParametricGridData::GetColLabelValue(int col)
{
	wxString col_label = wxEmptyString;
	wxString col_units = wxEmptyString;
	if ((col>-1) && (col < m_cols))
	{
		if (IsInput(col)) // label if non-blank
		{
//			if (VarInfo* vi = m_par.GetCase()->Variables(0).Lookup(m_var_names[col]))
			if (VarInfo* vi = m_par.GetCase()->Variables(m_par.Setup[col].ndxHybrid).Lookup(m_par.Setup[col].varName))
			{
//				col_label = m_var_names[col];
				col_label = vi->Label;
				col_units = vi->Units;
			}
		}
		else
		{
			if (m_par.Runs.size() > 0)
			{
				col_label = m_par.Runs[0]->GetLabel(m_var_names[col]);
//				col_label = m_var_names[col];
				col_units = m_par.Runs[0]->GetUnits(m_var_names[col]);
			}
			if (col_label.IsEmpty() || (col_label.Left(11) == "<not found:"))
			{
				wxArrayString names, labels, units, groups;
				Simulation::ListAllOutputs(m_case->GetConfiguration(),
					&names, &labels, &units, &groups, NULL, true);
				int ndx = names.Index(m_var_names[col]);
				if (ndx == wxNOT_FOUND)
					col_label = m_var_names[col];
				else
				{
//					col_label = names[ndx];
					col_label = labels[ndx];
					col_units = units[ndx];
				}
			}
		}
		if (col_units.length() > 0 && col_units != " ")
			col_label += " (" + col_units + ")";

		// sorting indicator
		if (IsSorted()) {
			int sortCol = GetSortColumn();
			if (col == sortCol) {
				if (auto pGrid = GetView()) {
					if (IsSortedAscending())
						col_label += L" \x2303";
					else
						col_label += L" \x2304";
				}
			}
		}
	}
	return col_label;
}

bool ParametricGridData::IsValid(const ParametricData::Var& pv) {
	VarValue* vv = m_par.GetCase()->BaseCase().GetInput(pv.varName,pv.ndxHybrid);
	if (vv == nullptr) {
		return false;
	}
	for (size_t i = 0; i < pv.Values.size(); i++) {
		if (vv->Type() != pv.Values[i].Type()) return false;
	}
	return true;
}

VarInfo* ParametricGridData::GetVarInfo(int , int col)
{
	VarInfo* vi = NULL;
	if ((col>-1) && (col < m_cols))
	{
		if (IsInput(col))
			vi = m_par.GetCase()->Variables(m_par.Setup[col].ndxHybrid).Lookup(m_par.Setup[col].varName); 
//		vi = m_par.GetCase()->Variables(0).Lookup(m_var_names[col]); // TODO: hybrids
	}
	return vi;
}

void ParametricGridData::SetVarInfo(int , int col, VarInfo *vi)
{
	if ((col>-1) && (col < m_cols))
	{
		if (IsInput(col))
//			if (VarInfo *var_info = m_par.GetCase()->Variables(0).Lookup(m_var_names[col]))
			if (VarInfo* var_info = m_par.GetCase()->Variables(m_par.Setup[col].ndxHybrid).Lookup(m_par.Setup[col].varName))
				var_info = vi;
	}
}

int ParametricGridData::GetSortColumn()
{
	return m_sortColumn;
}

void ParametricGridData::ClearSorting()
{
	m_rowSortOrder.clear();
	m_sortColumn = -1;
}

bool ParametricGridData::IsSorted()
{
	return (m_rowSortOrder.size() > 0);
}

bool ParametricGridData::IsSortedAscending()
{
	bool bRetVal = false;
	if (IsSorted()) {
		if (m_rowSortOrder.size() > 1)
			bRetVal = (m_rowSortOrder[0] <= m_rowSortOrder[1]);
	}
	return bRetVal;
}


void ParametricGridData::SortColumn(const int& col, const bool& asc)
{
	if ((col > -1) && (col < m_cols)) {
		ClearSorting();
		m_sortColumn = col;
		if (auto vv = GetVarValue(0, col)) {
			if (vv->Type() == VV_NUMBER) {
				for (int r = 0; r < m_rows; r++) {
					m_rowSortOrder.push_back(std::make_pair(GetDouble(r, col), r));
				}
				if (asc)
					std::sort(m_rowSortOrder.begin(), m_rowSortOrder.end(), [](std::pair<double, int> a, std::pair<double, int> b) {return a.first < b.first; });
				else
					std::sort(m_rowSortOrder.begin(), m_rowSortOrder.end(), [](std::pair<double, int> a, std::pair<double, int> b) {return a.first > b.first; });
			}
		}
	}
}


int ParametricGridData::GetRunNumberForRowNumber(const int& rowNum)
{
	int runNumber = -1;
	if (rowNum >= 0 && rowNum < m_rows) {
		runNumber = rowNum;
		if (m_rowSortOrder.size() == m_rows)
			runNumber = m_rowSortOrder[rowNum].second;
	}
	return runNumber;
}

std::vector<int> ParametricGridData::GetRowSortOrder()
{
	std::vector<int> rowOrder;
	if (m_rowSortOrder.size() == m_rows) {
		for (size_t i = 0; i < m_rowSortOrder.size(); i++)
			rowOrder.push_back(m_rowSortOrder[i].second);
	}
	else {
		for (int i = 0; i < m_rows; i++)
			rowOrder.push_back(i);
	}
	return rowOrder;
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
		// sorted row
		int sorted_row = row;
		if (m_rowSortOrder.size() == m_rows) // sort order set
			sorted_row = m_rowSortOrder[row].second;
		if (IsInput(col))
		{
			if (row < (int)m_par.Setup[col].Values.size())
				vv = &m_par.Setup[col].Values[sorted_row];
		}
		else
		{
			if (row < (int)m_par.Runs.size())
				vv = m_par.Runs[sorted_row]->GetOutput(m_var_names[col]);
		}
	}
	return vv;

}

void ParametricGridData::SetVarValue(int row, int col, VarValue *vv)
{
	if ((col>-1) && (col < m_cols))
	{
		int sorted_row = row;
		if (m_rowSortOrder.size() == m_rows) // sort order set
			sorted_row = m_rowSortOrder[row].second;
		if (IsInput(col))
		{
			if (row < (int)m_par.Setup[col].Values.size())
				if (VarValue *var_value = &m_par.Setup[col].Values[sorted_row])
					var_value = vv;
		}
		else
		{
			if (row < (int)m_par.Runs.size())
				if (VarValue *var_value = m_par.Runs[sorted_row]->GetOutput(m_var_names[col]))
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
			if ((as.Count() > 0) && (ndx >= 0) && (ndx < (int)as.Count()))
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
		// sorted row
		//int sorted_row = row;
		//if (m_rowSortOrder.size() == m_rows) // sort order set
		//	sorted_row = m_rowSortOrder[row].second;

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
		// sorted row
		//int sorted_row = row;
		//if (m_rowSortOrder.size() == m_rows) // sort order set
		//	sorted_row = m_rowSortOrder[row].second;

		if (IsInput(col))
		{
			if ( row >= 0 && row < (int)m_valid_run.size())
			{
				VarValue *vv = &m_par.Setup[col].Values[row];
				VarValue::Parse(vv->Type(), value, *vv);
				m_valid_run[row] = false;
				ClearResults(row);
			}
			else // should not happen!
				m_valid_run.push_back(false);
			// set for simulation (repeated in run simulation below for copying
			//m_par.Runs[row]->Override(m_var_names[col], *vv);
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
			else if (type == "MultilineText")
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
			else if (type == "DataArrayTable")
				return "GridCellVarValue";
			else if (type == "DataLifetimeArray")
				return "GridCellVarValue";
			else if (type == "DataLifetimeMatrix")
				return "GridCellVarValue";
			else if (type == "ShadingFactors")
				return "GridCellVarValue";
			else if (type == "ValueMatrix")
				return "GridCellVarValue";
			else if (type == "MonthByHourFactors")
				return "GridCellVarValue";
			else if (type == "Library")
				return "GridCellVarValue";
			else if (type == "LossAdjustment")
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

void ParametricGridData::DeleteSetup(ParametricData::Var &var)
{
	if (m_par.RemoveSetup(var.Name, var.IsInput))
	{
		DeleteCols();

		wxString varname;
		size_t ndxHybrid;
		UpdateVarNameNdxHybrid(var.Name, &varname, &ndxHybrid);

		// reset simulation input to base case input
		for (int row = 0; row < m_rows; row++)
		{
			m_par.Runs[row]->Clear(); // resets simulation private members
			if (VarValue *vv = m_case->BaseCase().GetInput(var.Name,ndxHybrid))
			{
				m_par.Runs[row]->Override(varname, *vv,ndxHybrid);
				m_valid_run[row] = false;
			}
		}
		// TODO invalidated results.
	}
}


bool ParametricGridData::UpdateVarNameNdxHybrid(const wxString& input_name, wxString* var_name, size_t* ndx_hybrid)
{
	*ndx_hybrid = 0;
	*var_name = input_name;
	if (!m_case) return false;
	// decode if necessary for hybrids varname for unsorted index
	wxArrayString as = wxSplit(input_name, '_');
	for (size_t j = 0; j < m_case->GetConfiguration()->Technology.size(); j++) {
		if (m_case->GetConfiguration()->Technology[j].Lower() == as[0]) {
			*ndx_hybrid = j;
			*var_name = input_name.Right(input_name.length() - (as[0].length() + 1));
		}
	}
	return true;
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
			int ndx = m_par.FindSetup(m_var_names[i], true);
			if (ndx > -1)
			{
				if (((int)m_par.Setup[i].Values.size() < rows) && (IsInput(i)))
				{
					while ((int)m_par.Setup[i].Values.size() < rows)
					{ // inputs
						wxString varname;
						size_t ndxHybrid;
						UpdateVarNameNdxHybrid(m_var_names[i], &varname, &ndxHybrid);
						if (VarValue *vv = m_case->Values(ndxHybrid).Get(varname))
							m_par.Setup[i].Values.push_back(*vv);
					}
				}
				else if ((int)m_par.Setup[i].Values.size() > rows)
				{
					while ((int)m_par.Setup[i].Values.size() > rows)
						m_par.Setup[i].Values.pop_back();
				}
			}
		}

		// update Runs
		if ((int)m_par.Runs.size() < rows)
		{
			for (int num_run = m_par.Runs.size(); num_run < rows; num_run++)
			{
				Simulation *s = new Simulation(m_case, wxString::Format("Parametric #%d", (int)(num_run+1)));
				//s->Copy(m_case->BaseCase());
				m_par.Runs.push_back(s);
			}
		}
		else if ((int)m_par.Runs.size() > rows)
		{
			//apd note on 8/5/2014: is this a memory leak??
			while ((int)m_par.Runs.size() > rows)
				m_par.Runs.pop_back();
		}

		// update valid runs
		if ((int)m_valid_run.size() < rows)
		{
			for (int num_run = m_valid_run.size(); num_run < rows; num_run++)
				m_valid_run.push_back(false);
		}
		else if ((int)m_valid_run.size() > rows)
		{
			while ((int)m_valid_run.size() > rows)
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

std::vector<double> ParametricGridData::GetArray(int row, int col)
{
	std::vector<double> ret_val;
	if (VarValue *vv = GetVarValue(row, col))
	{
		if (vv->Type() == VV_ARRAY)
			ret_val = vv->Array();
	}
	return ret_val;
}

double *ParametricGridData::GetArray(int row, int col, size_t *n)
{
	double *ret_val = NULL;
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
	wxString  ret_val = wxEmptyString;
	if ((col > -1) && (col < (int)m_var_names.Count()) && (row > -1) && (row < m_rows))
	{
		if (IsInput(col))
			ret_val = m_par.Setup[col].varName;
		else
			ret_val = m_var_names[col];
	}
	return ret_val;
}

int ParametricGridData::GetNdxHybrid(int row, int col)
{
	int  ret_val = 0;
	if ((col > -1) && (col < (int)m_var_names.Count()) && (row > -1) && (row < m_rows))
	{
		if (IsInput(col))
			ret_val = m_par.Setup[col].ndxHybrid;
		else {
			if (m_case->GetConfiguration()->Technology.size() > 1)
				ret_val = (int)m_case->GetConfiguration()->Technology.size() - 1;
		}
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
//	return m_par.Runs;
	std::vector<Simulation*> sims;
	auto view = GetView();
	for (size_t i = 0; i < m_par.Runs.size(); i++) {
		if (view->IsRowShown(i))
			sims.push_back(m_par.Runs[i]);
	}
	return sims;
}



bool ParametricGridData::RunSimulations_multi()
{
	wxStopWatch sw;

	int nthread = 1;
	nthread = wxThread::GetCPUCount();

	SimulationDialog tpd("Preparing simulations...", nthread);

	int total_runs = 0;
	for (size_t i = 0; i < m_par.Runs.size(); i++)
		if (!m_valid_run[i]) total_runs++;
	if (total_runs == 0) total_runs = m_par.Runs.size();

	std::vector<Simulation*> sims;
	for (size_t i = 0; i < m_par.Runs.size(); i++)
	{
		// reset all simulation objects to copy over latest value if changed on input page (no listeners)
		m_valid_run[i] = false;

		if (!m_valid_run[i])
		{
			m_par.Runs[i]->Clear();

			for (int col = 0; col < m_cols; col++)
			{
				if (IsInput(col))
				{
					if (VarValue *vv = &m_par.Setup[col].Values[i])
					{
						// set for simulation
//						m_par.Runs[i]->Override(m_var_names[col], *vv, m_par.Setup[col].ndxHybrid); // TODO: hybrids
						m_par.Runs[i]->Override(m_par.Setup[col].varName, *vv, m_par.Setup[col].ndxHybrid); 
					}
				}
			}
			// Excel exchange if necessary
			ExcelExchange &ex = m_case->ExcelExch();
			if (ex.Enabled) {
				for (size_t ndxHybrids = 0; ndxHybrids < m_case->GetConfiguration()->Technology.size(); ndxHybrids++)
				ExcelExchange::RunExcelExchange(ex, m_case->Values(ndxHybrids), m_par.Runs[i]);
			}

			if (!m_par.Runs[i]->Prepare())
				wxMessageBox(wxString::Format("internal error preparing simulation %d for parametric: %s", (int)(i + 1), m_par.Runs[i]->GetErrors()[0]));

			tpd.Update(0, (float)i / (float)total_runs * 100.0f, wxString::Format("%d of %d", (int)(i + 1), (int)total_runs));
		}

		m_par.Runs[i]->SetName( wxString::Format("Parametric #%d", (int)(i+1) ) );
		sims.push_back(m_par.Runs[i]);
	}


//	int time_prep = sw.Time();
	sw.Start();

	if ( nthread > (int)sims.size() ) nthread = sims.size();
	tpd.NewStage("Calculating...", nthread);

	Simulation::DispatchThreads(tpd, sims, nthread);

//	int time_sim = sw.Time();
	sw.Start();

	for (size_t i = 0; i < m_par.Runs.size(); i++)
	{
		if (!m_valid_run[i])
		{
			if (m_par.Runs[i]->Ok())
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
			}
			else
			{
				wxShowTextMessageDialog(wxJoin(m_par.Runs[i]->GetErrors(), '\n'));
				return false;
			}
		}
	}
	sims.clear();

//	int time_outputs = sw.Time();

	return true;
};

			

bool ParametricGridData::RunSimulations_single()
{
	for (size_t i = 0; i < m_par.Runs.size(); i++)
	{
		// reset all simulation objects to copy over latest value if changed on input page (no listeners)
		m_valid_run[i] = false;

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
						m_par.Runs[i]->Override(m_par.Setup[col].varName, *vv, m_par.Setup[col].ndxHybrid);
					}
				}
			}
			// Excel exchange if necessary
			ExcelExchange &ex = m_case->ExcelExch();
			if (ex.Enabled)
				ExcelExchange::RunExcelExchange(ex, m_case->Values(0), m_par.Runs[i]);

			// invoke simulation
			//update results in grid - send message to grid to update
			if (m_par.Runs[i]->Invoke())
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


bool ParametricGridData::Generate_lk()
{
	wxString fld = "c:/test";
	// TODO: prompt user for folder
	wxDirDialog *d = new wxDirDialog(NULL, "Choose a directory");
	if (d->ShowModal() == wxID_OK)
	{
		fld = d->GetPath();
	}

	for (size_t i = 0; i < m_par.Runs.size(); i++)
	{
		// reset all simulation objects to copy over latest value if changed on input page (no listeners)
		m_valid_run[i] = false;

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
						m_par.Runs[i]->Override(m_par.Setup[col].varName, *vv, m_par.Setup[col].ndxHybrid);
					}
				}
			}
			// Excel exchange if necessary
			ExcelExchange &ex = m_case->ExcelExch();
			if (ex.Enabled)
				ExcelExchange::RunExcelExchange(ex, m_case->Values(0), m_par.Runs[i]);// TODO: hybrids

			wxString file = fld + wxString::Format("/run%d.lk", (int)(i+1));
			if (FILE *fp = fopen(file.c_str(), "w"))
			{
				fprintf(fp, "clear();\n");
				if (m_par.Runs[i]->Generate_lk(fp))
				{

					// outputs
					for (int col = 0; col < m_cols; col++)
					{
						if (!IsInput(col))
						{
							// process outputs as desired
							fprintf(fp, "outln( '%s = ' + var('%s'));\n", (const char*)m_var_names[col].c_str(), (const char*)m_var_names[col].c_str());
						}
					}
				}
				fclose(fp);
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
		int ndx = m_par.FindSetup(input_names[i], true);
		if (ndx < 0)
		{
			size_t ndxHybrid = 0;
			wxString varName = input_names[i];
			// decode hybrids if necessary
			UpdateVarNameNdxHybrid(input_names[i], &varName, &ndxHybrid);
			/*
			if (m_case->GetConfiguration()->Technology.size() > 1) {
				// split hybrid name and match with Technology name or use "Hybrid" for remainder
				wxArrayString as = wxSplit(input_names[i], '_');
				for (size_t j = 0; j < m_case->GetConfiguration()->Technology.size(); j++) {
					if (m_case->GetConfiguration()->Technology[j].Lower() == as[0]) {
						ndxHybrid = j;
						varName = input_names[i].Right(input_names[i].length()-(as[0].length()+1));
					}
				}
/*				// if "Hybrids" then no prepending was added
				if (varName == input_names[i])
					ndxHybrid = m_case->GetConfiguration()->Technology.size() - 1;
			}
*/
			std::vector<VarValue> vvv;
			ParametricData::Var pv;
			for (int num_run = 0; num_run < m_rows; num_run++)
			{ // add values for inputs only
				if (VarValue *vv = m_case->Values(ndxHybrid).Get(varName)) 
					vvv.push_back(*vv);
			}
			pv.Name = input_names[i];
			pv.Values = vvv;
			pv.IsInput = true;
			pv.ndxHybrid = ndxHybrid;
			pv.varName = varName;
			AddSetup(pv);
		}
	}
	// remove any variables not selected
	std::vector< ParametricData::Var> to_remove;
	for (size_t i = 0; i < m_par.Setup.size(); i++)
	{
		wxString var_name = m_par.Setup[i].Name;
		if ((input_names.Index(var_name) == wxNOT_FOUND) && (m_par.Setup[i].IsInput))
				to_remove.push_back(m_par.Setup[i]);
	}
	ParametricData::Var pv;
	for (size_t i = 0; i < to_remove.size(); i++)
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
		int ndx = m_par.FindSetup(output_names[i], false);
		if (ndx < 0)
		{
			size_t ndxHybrid = 0;
			// decode hybrids if necessary
			if (m_case->GetConfiguration()->Technology.size() > 1) {
				// TODO split hybrid name and match with Technology name or use "Hybrid" for remainder
			}
			std::vector<VarValue> vvv;
			ParametricData::Var pv;
			for (int num_run = 0; num_run < m_rows; num_run++)
			{ // add values for inputs only
				if (VarValue *vv = m_case->Values(ndxHybrid).Get(output_names[i]))
					vvv.push_back(*vv);
			}
			pv.Name = output_names[i];
			pv.Values = vvv;
			pv.IsInput = false;
			pv.ndxHybrid = ndxHybrid;
			AddSetup(pv);
		}
	}
	// remove any variables not selected
	std::vector< ParametricData::Var> to_remove;
	for (size_t i = 0; i < m_par.Setup.size(); i++)
	{
		wxString var_name = m_par.Setup[i].Name;
		if ((output_names.Index(var_name) == wxNOT_FOUND) && (!IsInput(m_par.Setup[i])))
			to_remove.push_back(m_par.Setup[i]);
	}
	for (size_t i = 0; i < to_remove.size(); i++)
		DeleteSetup(to_remove[i]);
	m_output_names = output_names;
	UpdateSetup();
//	UpdateView();
}

void ParametricGridData::ClearResults()
{
	// clear simulation results
	for (int row = 0; row < m_rows; row++)
		ClearResults(row);
}

void ParametricGridData::ClearResults(int row)
{
	// clear simulation results
	if ((row > -1) && (row < m_rows))
	{
		m_par.Runs[row]->Clear();
		m_valid_run[row] = false;
	}
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
				if ((row < (int)m_valid_run.size()) && (m_valid_run[row]))
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
					if ((row < (int)m_valid_run.size()) && (m_valid_run[row]))
						attr->SetBackgroundColour(m_color_for_valid_outputs);
					else
						attr->SetBackgroundColour(m_color_for_invalid_outputs);
				}
			}
		}
	}
	return attr;
	
}

//////////////////////////////////////////////////////////////////////////////////////////

enum {
	ID_lstValues = wxID_HIGHEST + 495,
	ID_lstVariables,
	ID_btnEditValues,
	ID_btnRemoveVar,
	ID_btnAddVar,
	ID_setupOption,
	ID_numberRuns
};

BEGIN_EVENT_TABLE(Parametric_QS, wxDialog)
EVT_BUTTON(ID_btnAddVar, Parametric_QS::OnAddVariable)
EVT_BUTTON(ID_btnRemoveVar, Parametric_QS::OnRemoveVariable)
EVT_LISTBOX(ID_lstVariables, Parametric_QS::OnVariableSelect)
EVT_LISTBOX_DCLICK(ID_lstVariables, Parametric_QS::OnVarDblClick)
EVT_BUTTON(ID_btnEditValues, Parametric_QS::OnEditValues)
EVT_LISTBOX_DCLICK(ID_lstValues, Parametric_QS::OnValueDblClick)
EVT_BUTTON(wxID_OK, Parametric_QS::OnCommand)
EVT_BUTTON(wxID_HELP, Parametric_QS::OnCommand)
EVT_RADIOBUTTON(ID_setupOption, Parametric_QS::OnCommand)
END_EVENT_TABLE()

Parametric_QS::Parametric_QS(wxWindow *parent, Case *c)
: wxDialog(parent, wxID_ANY, "Parametric Quick Setup", wxDefaultPosition, wxScaleSize(550, 350), wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
m_case(c)
{
	lstVariables = new wxListBox(this, ID_lstVariables);
	lstValues = new wxListBox(this, ID_lstValues);


	wxBoxSizer *bsvars = new wxBoxSizer(wxHORIZONTAL);
	bsvars->Add(new wxStaticText(this, wxID_ANY, "Variables:"), 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL, 3);
	bsvars->AddStretchSpacer();
	bsvars->Add(new wxButton(this, ID_btnAddVar, "Add", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL, 3);
	bsvars->Add(new wxButton(this, ID_btnRemoveVar, "Remove", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL, 3);

	wxBoxSizer *bsvals = new wxBoxSizer(wxHORIZONTAL);
	bsvals->Add(new wxStaticText(this, wxID_ANY, "Selected variable values:"), 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL, 3);
	bsvals->AddStretchSpacer();
	bsvals->Add(new wxButton(this, ID_btnEditValues, "Edit", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL, 3);

	wxFlexGridSizer *fgs = new wxFlexGridSizer(2, 2, 5, 25);

	fgs->Add(bsvars, 3, wxEXPAND | wxALL, 3);
	fgs->Add(bsvals, 3, wxEXPAND | wxALL, 3);
	fgs->Add(lstVariables, 3, wxEXPAND | wxALL, 3);
	fgs->Add(lstValues, 3, wxEXPAND | wxALL, 3);

	fgs->AddGrowableCol(0, 1);
	fgs->AddGrowableCol(1, 1);
	fgs->AddGrowableRow(1, 1);

	rchSetupOption = new wxRadioChoice(this, ID_setupOption);
	rchSetupOption->Add("All combinations");
	rchSetupOption->Add("Independent");
	rchSetupOption->Add("Linked");
	rchSetupOption->SetHorizontal(true);
	rchSetupOption->SetSelection(0);
	wxBoxSizer *choice_sizer = new wxBoxSizer(wxHORIZONTAL);
	choice_sizer->Add(new wxStaticText(this, wxID_ANY, "Setup mode:"), 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL, 3);
	choice_sizer->Add(rchSetupOption, 2, wxEXPAND | wxALL, 2);

	numberRuns = new wxNumericCtrl(this, ID_numberRuns, 0, wxNUMERIC_INTEGER);
	numberRuns->SetFormat(0, true);
	numberRuns->SetEditable(false);
	wxBoxSizer *numrun_sizer = new wxBoxSizer(wxHORIZONTAL);
	numrun_sizer->Add(new wxStaticText(this, wxID_ANY, "Number of simulations:"), 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL, 3);
	numrun_sizer->Add(numberRuns, 0, wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL, 3);

	wxBoxSizer *button_sizer = new wxBoxSizer(wxHORIZONTAL);
	button_sizer->Add(numrun_sizer);
	button_sizer->AddStretchSpacer();
//	button_sizer->Add(CreateButtonSizer(wxOK | wxCANCEL | wxHELP), 1, wxALL | wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALIGN_RIGHT, 5);
	button_sizer->Add(CreateButtonSizer(wxOK | wxCANCEL | wxHELP), 1, wxALL | wxEXPAND, 5);


	wxBoxSizer *main_sizer = new wxBoxSizer(wxVERTICAL);
	main_sizer->Add(choice_sizer, 0, wxEXPAND | wxALL, 2);
	main_sizer->Add(fgs, 1, wxALL | wxEXPAND, 5);
	main_sizer->Add(button_sizer, 0, wxEXPAND | wxALL,2);
	SetSizer(main_sizer);

	UpdateFromParametricData();
}

bool Parametric_QS::UpdateLinkedEnabled()
{
	size_t num_runs = 0;
	bool enable_linked = true;
	if (m_input_values.size() > 0)
	{
		num_runs = m_input_values[0].Count() - 1;
		for (size_t i = 1; i < m_input_values.size(); i++)
		{
			if ((m_input_values[i].Count() - 1) != num_runs)
			{
				enable_linked = false;
				break;
			}
		}
		rchSetupOption->Enable(2, enable_linked);
	}
	return enable_linked;
}

size_t Parametric_QS::UpdateNumberRuns()
{
	size_t num_runs = 0;
	if (m_input_values.size() > 0)
	{
		int sel_mode = rchSetupOption->GetSelection();
		switch (sel_mode)
		{
			// Note: count-1 is used since first value is variable name and not a input value
			case 0:
			{
				// all combinations
				num_runs = 1;
				for (size_t i = 0; i < m_input_values.size(); i++)
					num_runs *= m_input_values[i].Count() - 1;
				break;
			}
			case 1:
			{
				// independent
				for (size_t i = 0; i < m_input_values.size(); i++)
					num_runs += m_input_values[i].Count() - 1;
				break;
			}
			case 2:
			{
				// linked - initially max number of values and other inputs set to base case values if less than max number of values
				// 5/14/15 meeting - update to only if all inputs have same number of values.
				num_runs = m_input_values[0].Count() -1;
				break;
			}
		}
		numberRuns->SetValue((double)num_runs);
		if (!UpdateLinkedEnabled() && (sel_mode == 2))
		{
			wxMessageBox("All inputs must have samed number of values to be linked.", "Linked disabled");
			rchSetupOption->SetSelection(0);
			numberRuns->SetValue((double)UpdateNumberRuns());
		}
	}
	return num_runs;
}

void Parametric_QS::OnCommand(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
		case wxID_OK:
		{
			if (UpdateNumberRuns() < 10000)
			{
				if (wxYES == wxMessageBox("Overwrite parametric table inputs with quick setup inputs?", "Overwrite table", wxYES_NO))
				{
					UpdateCaseParametricData();
					EndModal(wxID_OK);
				}
			}
			else if(UpdateNumberRuns() < 100000)
			{
				if (wxYES == wxMessageBox("Are you sure you want to setup more than 10,000 simulations?", "Confirm simulations", wxYES_NO))
				{
					if (wxYES == wxMessageBox("Overwrite parametric table inputs with quick setup inputs?", "Overwrite table", wxYES_NO))
					{
						UpdateCaseParametricData();
						EndModal(wxID_OK);
					}
				}
			}
			else
			{
				wxMessageBox("Please reduce the total number of runs to less than 100,000.", "Quick Setup Error");
			}
			break;
		}
		case wxID_HELP:
		{
			SamApp::ShowHelp("parametric_quick_setup");
			break;
		}
		case  ID_setupOption:
		{
			UpdateNumberRuns();
			break;
		}
		default:
		{
			EndModal(wxID_OK);
		}
	}
}

void Parametric_QS::UpdateFromParametricData()
{
	ParametricData &par = m_case->Parametric();
	m_input_values = par.QuickSetup;
	for (size_t i = 0; i < m_input_values.size(); i++)
	{
		if (m_input_values[i].Count() > 0)
			m_input_names.Add(m_input_values[i].Item(0));
	}
	rchSetupOption->SetSelection((int)par.QuickSetupMode);
	RefreshVariableList();
	RefreshValuesList();
}


void Parametric_QS::OnEditValues(wxCommandEvent &)
{
	if (!m_case)
		return;

	int idx = lstVariables->GetSelection();
	if (idx < 0 || idx > (int)m_input_names.Count())
		wxMessageBox("No variable selected!");
	else
	{
		wxString var_name;
		size_t ndx_hybrid;
		UpdateVarNameNdxHybrid(m_input_names[idx], &var_name, &ndx_hybrid);
		wxArrayString values = GetValuesList(m_input_names[idx]);
		VarInfo *varinfo = m_case->Variables(ndx_hybrid).Lookup(var_name);
		if (varinfo)
		{
			if (ShowEditValuesDialog(
				"Edit Parametric Values for '" + varinfo->Label +
				((varinfo->Units != "") ? (" (" + varinfo->Units + ")'") : "'"),
				values, var_name, ndx_hybrid))
			{
				SetValuesList(m_input_names[idx], values);
				RefreshValuesList();
			}
		}

	}
}

bool Parametric_QS::ShowFixedDomainDialog(const wxString &title,
	const wxArrayString &names, const wxArrayString &labels, wxArrayString &list,
	bool expand_all)
{
	SelectVariableDialog dlg(this, title);
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames(list);
	if (expand_all)
		dlg.ShowAllItems();

	if (dlg.ShowModal() == wxID_OK)
	{
		wxArrayString names = dlg.GetCheckedNames();

		// remove any from list
		int i = 0;
		while (i<(int)list.Count())
		{
			if (names.Index(list[i]) < 0)
				list.RemoveAt(i);
			else
				i++;
		}

		// append any new ones
		for (i = 0; i<(int)names.Count(); i++)
		{
			if (list.Index(names[i]) < 0)
				list.Add(names[i]);
		}


		return true;
	}
	else
		return false;
}


bool Parametric_QS::ShowEditValuesDialog(const wxString &title,
	wxArrayString &values, const wxString &varname, size_t& ndxHybrid)
{

	VarInfo *vi = m_case->Variables(ndxHybrid).Lookup(varname); 
	if (!vi)
		return false;
	VarValue *vv = m_case->Values(ndxHybrid).Get(varname); 
	if (!vv)
		return false;

	int i;
	int vvtype = vv->Type();
	unsigned long vf = vi->Flags;


	if (vvtype == VV_NUMBER
		&& vi->IndexLabels.Count() > 0 && vi->UIObject != "Numeric")
	{
		// fixed domain selection (combo box, list, radio choice etc)
		wxArrayString fixed_items = vi->IndexLabels;
		wxArrayString cur_items;
		for (i = 0; i<(int)values.Count(); i++)
		{
			int item_i = atoi(values[i].c_str());
			if (item_i >= 0 && item_i < (int)fixed_items.Count())
				cur_items.Add(fixed_items[item_i]);
		}

		if (ShowFixedDomainDialog(title, fixed_items, fixed_items, cur_items, true))
		{
			// translate back to integer values
			values.Clear();
			for (int i = 0; i<(int)cur_items.Count(); i++)
				values.Add(wxString::Format("%d", fixed_items.Index(cur_items[i])));

			return true;
		}
		else
			return false;
	}
	else if (vf & VF_LIBRARY)
	{
		wxArrayString fixed_items;
		wxArrayString lib_fields = vi->IndexLabels;
		if (lib_fields.Count() > 0)
		{
			wxString name = lib_fields[0];
			if (Library *lib = Library::Find(name))
			{
				fixed_items = lib->ListEntries();
				return ShowFixedDomainDialog(title, fixed_items, fixed_items, values, true);
			}
		}
	}
	// for single value schedules, e.g. degradation and o and m costs
	else if ((vvtype == VV_ARRAY) && (vv->Array().size() == 1))
	{
		return ShowNumericValuesDialog(title, values);
	}
	else if (vvtype == VV_NUMBER)
	{
		return ShowNumericValuesDialog(title, values);
	}

	wxMessageBox("Value type for \"" + vi->Label + "\" is not numeric. Cannot edit in Quick Setup.");
	return false;
}


bool Parametric_QS::ShowNumericValuesDialog(const wxString &title,
	wxArrayString &values)
{
	NumericRangeDialog dlg(this, title);
	dlg.SetValues(values, false);

	if (dlg.ShowModal() == wxID_OK)
	{
		values = dlg.GetValues();
		return true;
	}
	else
		return false;
}


void Parametric_QS::OnValueDblClick(wxCommandEvent &evt)
{
	OnEditValues(evt);
}


void Parametric_QS::OnRemoveVariable(wxCommandEvent &)
{
	if (!m_case)
		return;

	int idx = lstVariables->GetSelection();
	if (idx < 0)
		wxMessageBox("No variable selected!");
	else
	{
		wxString name = "";
		if ((idx >= 0) && (idx < (int)m_input_names.Count()))
			name = m_input_names[idx];

		for (std::vector<wxArrayString>::iterator it = m_input_values.begin();
			it != m_input_values.end(); ++it)
		{
			if ((*it).Item(0) == name)
			{
				m_input_values.erase(it);
				break;
			}
		}

		m_input_names.RemoveAt(idx);
	}

	RefreshVariableList();

	if (lstVariables->GetCount() > 0)
		lstVariables->Select(idx - 1 >= 0 ? idx - 1 : idx);

	RefreshValuesList();

}

bool Parametric_QS::UpdateVarNameNdxHybrid(const wxString& input_name, wxString* var_name, size_t* ndx_hybrid)
{
	*ndx_hybrid = 0;
	*var_name = input_name;
	// decode if necessary for hybrids varname for unsorted index
	if (m_case->GetConfiguration()->Technology.size() > 1) {
		// split hybrid name and match with Technology name or use "Hybrid" for remainder
		wxArrayString as = wxSplit(input_name, '_');
		for (size_t j = 0; j < m_case->GetConfiguration()->Technology.size(); j++) {
			if (m_case->GetConfiguration()->Technology[j].Lower() == as[0]) {
				*ndx_hybrid = j;
				*var_name = input_name.Right(input_name.length() - (as[0].length() + 1));
			}
		}
	}
	return true;
}


void Parametric_QS::OnAddVariable(wxCommandEvent &)
{
	if (!m_case)
		return;

	wxArrayString names, labels, varnames;
	wxString case_name(SamApp::Project().GetCaseName(m_case));

	ConfigInfo *ci = m_case->GetConfiguration();

	if (!ci)
		return;

	SelectVariableDialog dlg(this, "Select Inputs");


	for (size_t ndxHybrid = 0; ndxHybrid < ci->Technology.size(); ndxHybrid++) {
		VarInfoLookup& vil = ci->Variables[ndxHybrid];

		for (VarInfoLookup::iterator it = vil.begin(); it != vil.end(); ++it)
		{
			wxString name = it->first;
			wxString varname = name;
			if (ci->Technology.size() > 1) name = ci->Technology[ndxHybrid].Lower() + "_" + name;
			VarInfo& vi = *(it->second);

			// update to select only "Parametric" variables
			if ((vi.Flags & VF_PARAMETRIC) && !(vi.Flags & VF_INDICATOR) && !(vi.Flags & VF_CALCULATED))
			{
				wxString label = dlg.PrettyPrintLabel(name, vi);
				labels.Add(label);
				names.Add(name);
			}
		}
	}

	wxSortByLabels(names, labels);
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames(m_input_names);
	if (dlg.ShowModal() == wxID_OK)
	{
		m_input_names = dlg.GetCheckedNames();

		RefreshVariableList();
		if (m_input_names.Count() > 0)
			lstVariables->SetSelection(0);
		else
			m_input_values.clear();
		RefreshValuesList();
	}
}



void Parametric_QS::OnVariableSelect(wxCommandEvent &)
{
	RefreshValuesList();
}

void Parametric_QS::OnVarDblClick(wxCommandEvent &evt)
{
	RefreshValuesList();
	OnEditValues(evt);
}

void Parametric_QS::RefreshValuesList()
{
	if (!m_case)
		return;

	wxArrayString items;

	int idx = lstVariables->GetSelection();

	if (idx >= 0 && idx < (int)m_input_names.Count())
	{
		wxString name = m_input_names[idx];
		items = GetValuesDisplayList(m_input_names[idx]);
		if (items.Count() == 0) // add base case value
		{
			wxArrayString values;
			values.Add(m_input_names[idx]);
			wxString val = GetBaseCaseValue(m_input_names[idx]);
			values.Add(val);
			m_input_values.push_back(values);
			items = GetValuesDisplayList(m_input_names[idx]);
		}
	}

	lstValues->Freeze();
	lstValues->Clear();
	lstValues->Append(items);
	lstValues->Thaw();
	UpdateNumberRuns();
}


wxString Parametric_QS::GetBaseCaseValue(const wxString &input_name)
{
	wxString val;
	size_t ndx_hybrid;
	wxString var_name;
	UpdateVarNameNdxHybrid(input_name, &var_name, &ndx_hybrid);

	VarValue *vv = m_case->Values(ndx_hybrid).Get(var_name);
	if (vv)
		val = vv->AsString();
	return val;
}


wxArrayString Parametric_QS::GetValuesList(const wxString &input_name)
{
	wxArrayString list;
	for (int i = 0; i < (int)m_input_values.size(); i++)
	{
		if (m_input_values[i].Count() > 0 && m_input_values[i].Item(0) == input_name)
		{
			for (int j = 1; j < (int)m_input_values[i].Count(); j++)
				list.Add(m_input_values[i].Item(j));
			break;
		}
	}
	return list;
}

wxArrayString Parametric_QS::GetValuesDisplayList(const wxString &input_name)
{
	wxArrayString list;

	wxString var_name;
	size_t ndx_hybrid;

	UpdateVarNameNdxHybrid(input_name, &var_name, &ndx_hybrid);

	VarInfo *vi = m_case->Variables(ndx_hybrid).Lookup(var_name); 
	if (!vi)
		return list;
	VarValue *vv = m_case->Values(ndx_hybrid).Get(var_name);
	if (!vv)
		return list;

	int vvtype = vv->Type();


	if (vvtype == VV_NUMBER
		&& vi->IndexLabels.Count() > 0 && vi->UIObject != "Numeric")
	{
		// fixed domain selection (combo box, list, radio choice etc)
		wxArrayString fixed_items = vi->IndexLabels;
		for (size_t i = 0; i < m_input_values.size(); i++)
		{
			if (m_input_values[i].Count() > 0 && m_input_values[i].Item(0) == input_name)
			{
				for (size_t j = 1; j < m_input_values[i].Count(); j++)
				{
					int item_i = atoi(m_input_values[i].Item(j).c_str());
					if (item_i >= 0 && item_i < (int)fixed_items.Count())
						list.Add(fixed_items[item_i]);
				}
				break;
			}
		}
	}
	else
	{
		for (int i = 0; i < (int)m_input_values.size(); i++)
		{
			if (m_input_values[i].Count() > 0 && m_input_values[i].Item(0) == input_name)
			{
				for (int j = 1; j < (int)m_input_values[i].Count(); j++)
					list.Add(m_input_values[i].Item(j));
				break;
			}
		}
	}
	return list;
}

void Parametric_QS::SetValuesList(const wxString &input_name, const wxArrayString &values)
{
	int idx = -1;
	if (values.Count() <= 0) return;
	for (int i = 0; i <(int) m_input_values.size(); i++)
	{
		if (m_input_values[i].Count() > 0 && m_input_values[i].Item(0) == input_name)
		{
			idx = i;
			break;
		}
	}
	wxArrayString vals;
	vals.Add(input_name);
	for (int i = 0; i < (int)values.Count(); i++)
		vals.Add(values[i]);
	if (idx > -1)
		m_input_values[idx] = vals;
	else
		m_input_values.push_back(vals);
}

void Parametric_QS::UpdateCaseParametricData()
{
	ParametricData &par = m_case->Parametric();

//	if (UpdateNumberRuns())
//	{
		size_t num_runs = (size_t)numberRuns->Value();
//		if (num_runs <= 0) return; // or error message

		// save original outputs
		wxArrayString outputs;
		for (size_t i = 0; i < par.Setup.size(); i++)
		{
			if (par.Setup[i].IsInput)
				continue;
			outputs.Add(par.Setup[i].Name);
		}


		par.ClearRuns();
		par.Setup.clear();

		// create new inputs
		for (size_t i = 0; i < m_input_names.Count(); i++)
		{
			size_t ndxHybrid;
			wxString varName;
			UpdateVarNameNdxHybrid(m_input_names[i], &varName, &ndxHybrid);
			std::vector<VarValue> vvv;
			ParametricData::Var pv;
			if (VarValue *vv = m_case->Values(ndxHybrid).Get(varName)) 
			{
				for (size_t num_run = 0; num_run < num_runs; num_run++)
				{ // add values for inputs only
					vvv.push_back(*vv);
				}
			}
			pv.Name = m_input_names[i];
			pv.Values = vvv;
			pv.IsInput = true;
			pv.ndxHybrid = ndxHybrid;
			pv.varName = varName;
			par.Setup.push_back(pv);
		}

		for (size_t num_run = 0; num_run < num_runs; num_run++)
		{
			Simulation *s = new Simulation(m_case, wxString::Format("Parametric #%d", ((int)num_run + 1)));
			par.Runs.push_back(s);
		}
		
		// set values - can do this once and set num_runs
		switch (rchSetupOption->GetSelection())
		{
			case 0: // all combinations
			{
				size_t repeat = 1;
				for (size_t col = 0; col < m_input_names.Count(); col++)
				{
					size_t row = 0;
					wxArrayString vals = GetValuesList(m_input_names[col]);
					while (row < num_runs - 1)
					{
						for (size_t j = 0; j < vals.Count(); j++)
						{
							for (size_t k = 0; k < repeat; k++)
							{
								wxString value = vals[j];
								VarValue *vv = &par.Setup[col].Values[row];
								VarValue::Parse(vv->Type(), value, *vv);
								row++;
							}
						}
					}
					repeat *= vals.Count();
				}
				break;
			}
			case 1: // independent
			{
				size_t row = 0;
				for (size_t col = 0; col < m_input_names.Count(); col++)
				{
					wxArrayString vals = GetValuesList(m_input_names[col]);
					for (size_t j = 0; j < vals.Count(); j++)
					{
							wxString value = vals[j];
							VarValue *vv = &par.Setup[col].Values[row];
							VarValue::Parse(vv->Type(), value, *vv);
							row++;
					}
				}
				break;
			}
			case 2: // linked - first cut is values and if < num_rows base value
			{
				for (size_t col = 0; col < m_input_names.Count(); col++)
				{
					wxArrayString vals = GetValuesList(m_input_names[col]);
					for (size_t j = 0; j < vals.Count(); j++)
					{
						wxString value = vals[j];
						VarValue *vv = &par.Setup[col].Values[j];
						VarValue::Parse(vv->Type(), value, *vv);
					}
				}
				break;
			}
		}


		// add original outputs back 
		for (size_t i = 0; i < outputs.Count(); i++)
		{
			std::vector<VarValue> vvv;
			ParametricData::Var pv;
			if (VarValue *vv = m_case->Values(0).Get(outputs[i])) // TODO: hybrids??
			{
				for (size_t num_run = 0; num_run < num_runs; num_run++)
				{ // add values for inputs only
					vvv.push_back(*vv);
				}
			}
			pv.Name = outputs[i];
			pv.Values = vvv;
			pv.IsInput = false;
			par.Setup.push_back(pv);
		}

		// save current quick setup to be reloaded
		par.QuickSetup = m_input_values;
		par.QuickSetupMode = rchSetupOption->GetSelection();
//	}
}



void Parametric_QS::RefreshVariableList()
{
	if (!m_case)
		return;

	lstVariables->Freeze();
	lstVariables->Clear();

	for (size_t i = 0; i<m_input_names.Count(); i++)
	{
		size_t ndx_hybrid;
		wxString var_name;
		UpdateVarNameNdxHybrid(m_input_names[i], &var_name, &ndx_hybrid);

		VarInfo *vi = m_case->Variables(ndx_hybrid).Lookup(var_name);
		if (!vi)
		{
			lstVariables->Append("<<Label Lookup Error>>");
			continue;
		}
		wxString suffix = "";

		if (vi->Units.Trim().length() > 0 )
			suffix += " (" + vi->Units + ") ";

		if (!vi->Group.IsEmpty())
			lstVariables->Append(vi->Group + "/" + vi->Label + suffix);
		else
			lstVariables->Append(vi->Label + suffix);

		// update m_input_values if necessary
		// add items
		wxArrayString items = GetValuesDisplayList(m_input_names[i]);
		if (items.Count() == 0) // add base case value
		{
			wxArrayString values;
			values.Add(m_input_names[i]);
			wxString val = GetBaseCaseValue(m_input_names[i]);
			values.Add(val);
			m_input_values.push_back(values);
		}
		// remove items
		std::vector<size_t>  to_remove;
		for (size_t i = 0; i < m_input_values.size(); i++)
		{	
			if (m_input_values[i].Count() <= 0)
				to_remove.push_back(i);
			else if (m_input_names.Index(m_input_values[i].Item(0)) == wxNOT_FOUND)
				to_remove.push_back(i);
		}
		if (to_remove.size() > 0)
		{
			for (int i = to_remove.size() - 1; i >= 0; i--)
				if (m_input_values.size() > to_remove[i])
					m_input_values.erase(m_input_values.begin() + to_remove[i]);
		}

	}


	lstVariables->Thaw();

	lstValues->Clear();
}

