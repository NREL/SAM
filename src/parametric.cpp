#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/button.h>

#include <wex/plot/plplotctrl.h>
#include <wex/plot/plbarplot.h>
#include <wex/metro.h>
#include <wex/extgrid.h>

#include "parametric.h"
#include "case.h"



ParametricData::ParametricData( Case *c )
	: m_case( c )
{
}

ParametricData::~ParametricData()
{
	ClearRuns();
}

void ParametricData::ClearRuns()
{
	for( size_t i=0;i<Runs.size();i++ )
		delete Runs[i];
	Runs.clear();
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

enum {	ID_CONFIGURE};



BEGIN_EVENT_TABLE(ParametricViewer, wxPanel)
	EVT_BUTTON(ID_CONFIGURE, ParametricViewer::OnCommand)
END_EVENT_TABLE()



ParametricViewer::ParametricViewer( wxWindow *parent, Case *cc )
	: wxPanel( parent, wxID_ANY ), m_case( cc ), m_par( cc->Parametric() ) 
{
	wxBoxSizer *par_sizer = new wxBoxSizer( wxHORIZONTAL );
	par_sizer->Add( new wxButton( this, ID_CONFIGURE, "Configure..."), 0, wxALL|wxEXPAND, 2 );
	par_sizer->Add( new wxButton( this, wxID_ANY, "Run parametric simulation"), 0, wxALL|wxEXPAND, 2 );
	par_sizer->AddStretchSpacer();
	par_sizer->Add( new wxButton( this, wxID_ANY, "Clear results"), 0, wxALL|wxEXPAND, 2 );
	
	/* Mock-up
	wxExtGridCtrl *par_grid = new wxExtGridCtrl( this, wxID_ANY );
	par_grid->CreateGrid( 7, 4 );

	wxString grid[8][5]= {
		{ "Tilt", "Input 2", "LCOEreal", "AnnOutput", "NPV" },
		{ "10", "1", "1",   "1", "-4" },
		{ "15", "2", "3.1", "2", "-3" },
		{ "20", "3", "4.1", "3", "-2" },
		{ "25", "4", "3.1", "4", "-1" },
		{ "30", "5", "2.1", "5", "-2" },
		{ "35", "6", "1.1", "6", "-3" },
		{ "40", "7", "1.1", "7", "-4" } };

	for( size_t r=1;r<8;r++ )
	{
		for( size_t c=1;c<5;c++ )
		{
			if ( r == 1 ) par_grid->SetColLabelValue( c-1, grid[0][c-1] );
			par_grid->SetCellValue( grid[r][c], r-1, c-1 );
			if ( c <= 2 ) par_grid->SetCellBackgroundColour( wxColour(244,244,210), r-1, c-1 );
		}
	}
	par_grid->AutoSizeColumns();

	wxPLPlotCtrl *par_plot = new wxPLPlotCtrl( this, wxID_ANY );
	std::vector<wxRealPoint> bar_data;
	bar_data.push_back( wxRealPoint( 1, 3 ) );
	bar_data.push_back( wxRealPoint( 2, 2.9 ) );
	bar_data.push_back( wxRealPoint( 3, 3.2 ) );
	bar_data.push_back( wxRealPoint( 4, 3.7 ) );
	bar_data.push_back( wxRealPoint+( 5, 2.2 ) );
	bar_data.push_back( wxRealPoint( 6, 1.7 ) );
	wxPLBarPlot *bar0, *bar1;
	par_plot->AddPlot( bar0 = new wxPLBarPlot( bar_data, "Var2 run^0", wxMetroTheme::Colour( wxMT_ACCENT ) ) );
	for( size_t i=0;i<bar_data.size();i++ ) bar_data[i].y *= 0.7 + ((double)i)/5;
	par_plot->AddPlot( bar1 = new wxPLBarPlot( bar_data, "Var1 run^0", wxMetroTheme::Colour( wxMT_FOREGROUND ) ) );
	par_plot->GetXAxis1()->SetWorld( 0, 7 );
	par_plot->GetYAxis1()->SetWorld( 0, 5 );
	std::vector<wxPLBarPlot*> bar_group;
	bar_group.push_back( bar0 );
	bar_group.push_back( bar1 );
	bar0->SetGroup( bar_group );
	bar1->SetGroup( bar_group );
	bar0->SetThickness( 30 );
	bar1->SetThickness( 30 );
	*/


	wxBoxSizer *par_vsizer = new wxBoxSizer( wxVERTICAL );
	par_vsizer->Add( par_sizer, 0, wxALL|wxEXPAND, 2 );
//	par_vsizer->Add( m_grid, 1, wxALL|wxEXPAND, 0 );
//	par_vsizer->Add( par_plot, 1, wxALL|wxEXPAND, 0 );

	SetSizer( par_vsizer );
}


void ParametricViewer::OnCommand(wxCommandEvent &evt)
{
	switch (evt.GetId())
	{
	case ID_CONFIGURE:
		Configure();
		UpdateGrid();
		break;
	}
}


void ParametricViewer::Configure()
{

}

void ParametricViewer::UpdateGrid()
{

}


////////////////////////////////////////////////////////////////////////////////////////////////

// m_par contains list of inputs and outputs
ParametricGridData::ParametricGridData( ParametricData &par)
{
	*m_par = par;
	Init();
	m_rows = 0;
	m_cols = 0;
}

void ParametricGridData::Init()
{
	if (!m_par) return;
	m_col_hdrs.Clear();
	m_var_labels.Clear();
	m_var_names.Clear();

	m_cols = m_par->Setup.size();

	for (size_t i = 0; i < (size_t)m_cols; i++)
		m_var_names.push_back(m_par->Setup[i].Name);
	
	m_rows = m_par->Runs.size();
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
			if (row < (int)m_par->Setup[col].Values.size())
				emptycell = (m_par->Setup[col].Values[row].AsString() == wxEmptyString);
		}
		else
		{
			if (row < (int)m_par->Runs.size())
				if (VarValue *vv = m_par->Runs[row]->GetOutput(m_var_names[col]))
					emptycell = (vv->AsString() == wxEmptyString);
		}
	}
	return emptycell;
}

bool ParametricGridData::IsInput(int col)
{
	if ((col>-1)&&(col<m_cols)&&(m_par->GetCase()->Values().Get(m_var_names[col])))
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
			if (VarInfo *vi = m_par->GetCase()->Variables().Lookup(m_var_names[col]))
				col_label = vi->Label;
		}
		else
		{
			if (m_par->Runs.size() > 0)
				col_label = m_par->Runs[0]->GetLabel(m_var_names[col]);
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
			vi = m_par->GetCase()->Variables().Lookup(m_var_names[col]);
	}
	return vi;
}

void ParametricGridData::SetVarInfo(int row, int col, VarInfo *vi)
{
	if ((col>-1) && (col < m_cols))
	{
		if (IsInput(col))
			if (VarInfo *var_info = m_par->GetCase()->Variables().Lookup(m_var_names[col]))
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
			if (row < (int)m_par->Setup[col].Values.size())
				vv = &m_par->Setup[col].Values[row];
		}
		else
		{
			if (row < (int)m_par->Runs.size())
				vv = m_par->Runs[row]->GetOutput(m_var_names[col]);
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
			if (row < (int)m_par->Setup[col].Values.size())
				if (VarValue *var_value = &m_par->Setup[col].Values[row])
					var_value = vv;
		}
		else
		{
			if (row < (int)m_par->Runs.size())
				if (VarValue *var_value = m_par->Runs[row]->GetOutput(m_var_names[col]))
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
			for (int i = 0; i < (int)as.Count() - 1; i++)
				ret_str += as[i] + ",";
			ret_str += as[as.Count() - 1];
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
			VarValue vv = m_par->Setup[col].Values[row];
			VarValue::Parse(vv.Type(), value, vv);
		}
		// outputs are calculated in simulations
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
