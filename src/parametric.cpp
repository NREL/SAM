#include <wx/panel.h>
#include <wx/button.h>

#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>
#include <wex/plot/pllineplot.h>
#include <wex/dview/dvtimeseriesctrl.h>
#include <wex/dview/dvtimeseriesdataset.h>
#include <wex/metro.h>
#include <wex/utils.h>

#include "parametric.h"
#include "main.h"
#include "casewin.h"


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

BEGIN_EVENT_TABLE(ParametricGrid, wxExtGridCtrl)
EVT_GRID_CELL_LEFT_CLICK(ParametricGrid::OnLeftClick)
END_EVENT_TABLE()


ParametricGrid::ParametricGrid(wxWindow *parent, wxWindowID id, const wxPoint &pos, const wxSize &size, long style, const wxString &name)
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

enum { ID_SELECT_INPUTS, ID_SELECT_OUTPUTS, ID_NUMRUNS, ID_RUN, ID_CLEAR, ID_GRID };



BEGIN_EVENT_TABLE(ParametricViewer, wxPanel)
EVT_BUTTON(ID_SELECT_INPUTS, ParametricViewer::OnCommand)
EVT_BUTTON(ID_SELECT_OUTPUTS, ParametricViewer::OnCommand)
EVT_NUMERIC(ID_NUMRUNS, ParametricViewer::OnCommand)
EVT_BUTTON(ID_RUN, ParametricViewer::OnCommand)
EVT_BUTTON(ID_CLEAR, ParametricViewer::OnCommand)
EVT_GRID_CMD_LABEL_RIGHT_CLICK(ID_GRID, ParametricViewer::OnGridColLabelRightClick)
END_EVENT_TABLE()



ParametricViewer::ParametricViewer(wxWindow *parent, Case *cc)
: wxPanel(parent, wxID_ANY), m_case(cc)
{
	wxBoxSizer *par_sizer = new wxBoxSizer(wxHORIZONTAL);
	par_sizer->Add(new wxButton(this, ID_SELECT_INPUTS, "Select Inputs..."), 0, wxALL | wxEXPAND, 2);
	par_sizer->Add(new wxButton(this, ID_SELECT_OUTPUTS, "Select Outputs..."), 0, wxALL | wxEXPAND, 2);
	par_sizer->Add(new wxStaticText(this, wxID_ANY, "   Number of Runs:"), 0, wxALIGN_CENTER_VERTICAL, 2);
	m_num_runs_ctrl = new wxNumericCtrl(this, ID_NUMRUNS, 0, wxNumericCtrl::INTEGER, wxDefaultPosition, wxSize(50, 24));
	par_sizer->Add(m_num_runs_ctrl, 0, wxALL, 2);
	par_sizer->AddStretchSpacer();
	par_sizer->Add(new wxButton(this, ID_RUN, "Run parametric simulation"), 0, wxALL | wxEXPAND, 2);
	par_sizer->Add(new wxButton(this, ID_CLEAR, "Clear results"), 0, wxALL | wxEXPAND, 2);


	m_grid = new ParametricGrid(this, ID_GRID);

	m_grid->RegisterDataType("GridCellCheckBox", new GridCellCheckBoxRenderer, new GridCellCheckBoxEditor);
	m_grid->RegisterDataType("GridCellChoice", new GridCellChoiceRenderer, new GridCellChoiceEditor);
	m_grid->RegisterDataType("GridCellVarValue", new GridCellVarValueRenderer, new GridCellVarValueEditor);

	m_grid_data = new ParametricGridData(m_case);
	m_grid->SetTable(m_grid_data);
	m_num_runs_ctrl->SetValue(m_grid_data->GetNumberRows());
	m_input_names = m_grid_data->GetInputNames();
	m_output_names = m_grid_data->GetOutputNames();

	m_par_sizer = new wxBoxSizer(wxVERTICAL);
	m_par_sizer->Add(par_sizer, 0, wxALL | wxEXPAND, 2);
	m_par_sizer->Add(m_grid, 1, wxALL | wxEXPAND, 0);

	SetSizer(m_par_sizer);
	UpdateGrid();
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


void ParametricViewer::OnGridColLabelRightClick(wxGridEvent &evt)
{
	if (evt.GetRow() < 0 && evt.GetCol() >= 0)
	{
		if (m_grid_data->IsInput(evt.GetCol()))
			// show input menu
			wxMessageBox("Input menu");
		else // plot export
		{
		//	wxMessageBox("Output menu");
			// test plotting 
			int col = evt.GetCol();
			if (m_grid_data->GetRowsCount() > 0)
			{
				if (VarValue *vv = m_grid_data->GetVarValue(0, col))
				{
					switch (vv->Type())
					{
						//single value only
						case VV_NUMBER:
							{	
							std::vector<wxRealPoint> bar_data;
							double max_x = m_grid_data->GetRowsCount() + 1;
							double max_y = -9e99;
							for (size_t row = 0; row < m_grid_data->GetRowsCount(); row++)
							{
								double y = m_grid_data->GetDouble(row, col);
								if (y>max_y) max_y = y;
								bar_data.push_back(wxRealPoint(row + 1, y));
							}
							wxPLPlotCtrl *par_plot = new wxPLPlotCtrl(this, wxID_ANY);
							wxPLBarPlot *bar;
							par_plot->AddPlot(bar = new wxPLBarPlot(bar_data, m_grid_data->GetColLabelValue(col), wxMetroTheme::Colour(wxMT_ACCENT)));
							par_plot->GetXAxis1()->SetWorld(0, max_x);
							par_plot->GetYAxis1()->SetWorld(0, max_y);
							m_par_sizer->Add(par_plot, 1, wxALL | wxEXPAND, 0);
							m_par_sizer->Layout();
							Update();
							}
							break;

						// arrays - determine if monthly or hourly
						case VV_ARRAY:
							{
							wxArrayString line_colors;
							line_colors.push_back("BLUE");
							line_colors.push_back("RED");
							line_colors.push_back("GREEN");
							line_colors.push_back("ORANGE");
							line_colors.push_back("TAN");
							line_colors.push_back("VIOLET");
							line_colors.push_back("YELLOW");
							line_colors.push_back("MAROON");
							line_colors.push_back("BLACK");
							line_colors.push_back("CYAN");
							wxPLPlotCtrl *par_plot = new wxPLPlotCtrl(this, wxID_ANY);
							wxPLLinePlot *line;
							for (size_t row = 0; row < m_grid_data->GetRowsCount(); row++)
							{
								wxPLLinePlot *line;
								std::vector<wxRealPoint> line_data;
								std::vector<float> y = m_grid_data->GetArray(row, col);
								for (size_t i = 0; i< y.size(); i++)
									line_data.push_back(wxRealPoint(i, y[i]));
								par_plot->AddPlot(line = new wxPLLinePlot(line_data, m_grid_data->GetColLabelValue(col) + wxString::Format(": run(%d)",row+1), wxTheColourDatabase->Find(line_colors[row]) ));
							}
							m_par_sizer->Add(par_plot, 1, wxALL | wxEXPAND, 0);
							m_par_sizer->Layout();
							Update();
							// test dview control for 8760 data
							wxDVTimeSeriesCtrl *dv = new wxDVTimeSeriesCtrl(this, wxID_ANY, RAW_DATA_TIME_SERIES, AVERAGE);
							for (size_t row = 0; row < m_grid_data->GetRowsCount(); row++)
							{
								size_t n;
								float *y = m_grid_data->GetArray(row, col, &n);
								//if (n == 8760)
									dv->AddDataSet(new TimeSeries8760(y, m_grid_data->GetColLabelValue(col) + wxString::Format(": run(%d)", row + 1), m_grid_data->GetUnits(col) ), wxEmptyString, true );
						}
							m_par_sizer->Add(dv, 1, wxALL | wxEXPAND, 0);
							m_par_sizer->Layout();
							Update();
						}
							break;

					}
				}
			}
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
	//  call run simulations from parametric grid data to update all inputs with current grid values
	m_grid_data->RunSimulations();
}

void ParametricViewer::ClearResults()
{
	// TODO - verify this is what we want
	m_grid_data->ClearResults();
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
		if (!label.IsEmpty())
		{
			if (!vi.Units.IsEmpty())
				label += " (" + vi.Units + ")";

			label += "  ";
		}

		label += "{ " + name + " }";

		/*
		if (vi.Group.IsEmpty()) label = "-Unsorted-/" + label;
		else label = vi.Group + "/" + label;

		label = "[" + case_name + "] Inputs/" + label;
		*/
		labels.Add(label);
		names.Add(name);
	}

	wxSortByLabels(names, labels);
	SelectVariableDialog dlg(this, "Select Inputs");
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames(m_input_names);
	if (dlg.ShowModal() == wxID_OK)
	{
		m_input_names = dlg.GetCheckedNames();
		m_grid_data->UpdateInputs(m_input_names);
	}
}

void ParametricViewer::SelectOutputs()
{
	wxArrayString names, labels;
	wxString case_name(SamApp::Project().GetCaseName(m_case));
	wxArrayString output_names, output_labels;
	Simulation::ListAllOutputs(m_case->GetConfiguration(), &output_names, &output_labels, 0);

	for (int j = 0; j<(int)output_labels.size(); j++)
	{
		if (!output_labels[j].IsEmpty())
		{
			names.Add(output_names[j]);
			labels.Add(output_labels[j]);
//			labels.Add("[" + case_name + "] Outputs/" + output_labels[j]);
		}
	}

	wxSortByLabels(names, labels);
	SelectVariableDialog dlg(this, "Select Outputs");
	dlg.SetItems(names, labels);
	dlg.SetCheckedNames(m_output_names);
	if (dlg.ShowModal() == wxID_OK)
	{
		m_output_names = dlg.GetCheckedNames();
		m_grid_data->UpdateOutputs(m_output_names);
	}
}

void ParametricViewer::UpdateGrid()
{
	// update grid data with m_par updates from configure and number of runs
	m_grid->SetTable(m_grid_data);
	m_grid->ForceRefresh();
	for (int col = 0; col < m_grid_data->GetNumberCols(); col++)
	{
		for (int row = 0; row < m_grid_data->GetNumberRows(); row++)
		{
			if (m_grid_data->IsInput(col))
				m_grid->SetReadOnly(row, col, false);
			else
				m_grid->SetReadOnly(row, col, true);
		}
	}
}


////////////////////////////////////////////////////////////////////////////////////////////////

// m_par contains list of inputs and outputs
ParametricGridData::ParametricGridData(Case *cc) :m_case(cc), m_par(cc->Parametric())
{
	m_rows = 0;
	m_cols = 0;
	Init();
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
	if ((col>-1) && (col < m_cols))
	{
		if (IsInput(col)) // label if non-blank
		{
			if (VarInfo *vi = m_par.GetCase()->Variables().Lookup(m_var_names[col]))
				col_label = vi->Label;
		}
		else
		{
			if (m_par.Runs.size() > 0)
				col_label = m_par.Runs[0]->GetLabel(m_var_names[col]);
		}
		if (col_label.IsEmpty()) 
			col_label = m_var_names[col];
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
			// set for simulation
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
			else // TODO for outputs
			{
				/*
				if (VarValue *vv = GetVarValue(row,col))
				{
					switch (vv->Type)
					{
					case VV_ARRAY:
						return single column grid for read only editor 
						return GridCellWrapTextRenderer to show long arrays?
					}
				}
				else
				*/
					return wxGRID_VALUE_STRING;
			}
		}
		else
			return wxGRID_VALUE_STRING;
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
	//if (IsInput) append only for now
	m_par.Setup.push_back(var);
	AppendCols();
}

void ParametricGridData::DeleteSetup(wxString &var_name)
{
	if (m_par.RemoveSetup(var_name))
		DeleteCols();
}

void ParametricGridData::UpdateNumberRows(int rows)
{
	if (rows < 0) return;
	if (m_rows != rows)
	{
		if (rows > m_rows) // append rows
		{
			// go through all variables and append VarValues as necessary
			for (size_t i = 0; i < m_var_names.Count(); i++)
			{
				// update Setup
				int ndx = m_par.FindSetup(m_var_names[i]);
				if (ndx > -1)
				{
					// TODO - update for outputs??
					if ((m_par.Setup[i].Values.size() < rows) && (IsInput(i)))
					{
						while (m_par.Setup[i].Values.size() < rows)
						{ // inputs
							if (VarValue *vv = m_case->Values().Get(m_var_names[i]))
								m_par.Setup[i].Values.push_back(*vv);
							// outputs? - only update from simulations!
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

		}
		else // delete rows
		{
			// TODO
		}
		m_rows = rows;
		UpdateView();
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

/* does nothing ? */
void ParametricGridData::UpdateView()
{
	if (GetView())
	{
		wxGridTableMessage msg(this,
			wxGRIDTABLE_REQUEST_VIEW_GET_VALUES);
		GetView()->ProcessTableMessage(msg);
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
	wxString  ret_val;
	if (m_rows > 0)
	{
		if (VarInfo *vi = GetVarInfo(0, col))
		{
			ret_val = vi->Label;
		}
	}
	return ret_val;
}

bool ParametricGridData::RunSimulations(int row)
{
	for (size_t i = 0; i < m_par.Runs.size(); i++)
	{
		// Excel exchange if necessary
		ExcelExchange &ex = m_case->ExcelExch();
		if (ex.Enabled)
			ExcelExchange::RunExcelExchange(ex, m_case->Values(), m_par.Runs[i]);

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
			UpdateView();
		}
		else
		{
			wxShowTextMessageDialog(wxJoin(m_par.Runs[i]->GetErrors(), '\n'));
			return false;
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
		DeleteSetup(to_remove[i]);
	m_input_names = input_names;
	UpdateSetup();
	UpdateView();
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
	UpdateView();
}

void ParametricGridData::ClearResults(int row)
{
	for (int col = 0; col < m_cols; col++)
	{
		if (!IsInput(col))
		{
			m_par.Setup[col].Values.clear();
		}
	}
}