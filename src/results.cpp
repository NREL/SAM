#include <algorithm>

#include <wx/panel.h>
#include <wx/clipbrd.h>
#include <wx/dcbuffer.h>
#include <wx/statline.h>
#include <wx/busyinfo.h>
#include <wx/splitter.h>
#include <wx/datstrm.h>
#include <wx/filedlg.h>
#include <wx/filename.h>

#include <wex/extgrid.h>
#include <wex/metro.h>
#include <wex/dview/dvplotctrl.h>
#include <wex/dview/dvselectionlist.h>
#include <wex/dview/dvcolourmap.h>
#include <wex/dview/dvstatisticstablectrl.h>
#include <wex/plot/plplotctrl.h>
#include <wex/plot/plaxis.h>
#include <wex/plot/pllineplot.h>
#include <wex/numeric.h>
#include <wex/ole/excelauto.h>
#include <wex/csv.h>
#include <wex/utils.h>
#include <wex/snaplay.h>

#include "main.h"
#include "variables.h"
#include "simulation.h"
#include "results.h"
#include "casewin.h"
#include "graph.h"
#include "invoke.h"
#include "lossdiag.h"

static wxString UnsplitCells(const matrix_t<wxString> &table, char colsep, char rowsep, bool quote_colsep)
{
	wxString result = "";
	int r, c;

	for (r = 0; r<table.nrows(); r++)
	{
		for (c = 0; c<table.ncols(); c++)
		{
			if (quote_colsep && table.at(r, c).Find(colsep) != wxNOT_FOUND)
			{
				result = result + '"' + table.at(r, c) + '"';
			}
			else
			{
				result = result + table.at(r, c);
			}

			if (c < table.ncols() - 1)
			{
				result = result + colsep;
			}
		}

		if (r < table.nrows() - 1)
		{
			result = result + rowsep;
		}
	}
	return result;
}




void PopulateSelectionList( wxDVSelectionListCtrl *sel, wxArrayString *names, Simulation *sim )
{		
	int an_period = -1;
	ConfigInfo *config = sim->GetCase()->GetConfiguration();
	if ( config != 0 )
	{
		wxString an_var = config->Settings["analysis_period_var"];
		if ( !an_var.IsEmpty() )
		{
			if ( VarValue *vv = sim->GetValue( an_var ) )
				if ( vv->Type() == VV_NUMBER )
					an_period = (int) vv->Value();
		}
	}

	std::vector<size_t> varlengths;
	sim->GetVariableLengths( varlengths );

	names->Clear();

	for (size_t i=0;i<varlengths.size();i++)
	{		
		wxArrayString list;		
		sim->ListByCount( varlengths[i], list );

		if (list.Count() == 0)
			continue;
		
		int steps_per_hour = varlengths[i]/8760;
		if ( steps_per_hour*8760 != varlengths[i] )
			steps_per_hour = -1;

		wxString group;
		if (varlengths[i] == 1)
			group = "Single Values";
		else if (varlengths[i] == 12)
			group = "Monthly Data";
		else if (varlengths[i] == 8760)
			group = "Hourly Data";
		else if ( steps_per_hour >= 2 && steps_per_hour <= 60 )
			group = wxString::Format("%d Minute Data",60/steps_per_hour);
		else if (varlengths[i] == an_period)
			group = "Annual Data";
		else if (varlengths[i] == (an_period-1)*12)
			group = "Lifetime Monthly Data";
		else if (varlengths[i] == (an_period-1)*8760)
			group = "Lifetime Hourly Data";
		else
			group.Printf("Data: %d values", (int)varlengths[i]);
		
		wxArrayString labels;
		for ( size_t j=0;j<list.Count();j++)
		{
			wxString label( sim->GetLabel( list[j] ) );
			wxString units( sim->GetUnits( list[j] ) );
			if ( !units.IsEmpty() )
				label += " (" + units + ")";
			labels.Add( label );
		}

		wxSortByLabels( list, labels );

		for (size_t j=0;j<list.Count();j++)
		{
			if (!labels[j].IsEmpty())
			{
				sel->Append( labels[j], group );
				names->Add(list[j]);
			}
		}
	}
}


ResultsCallbackContext::ResultsCallbackContext( ResultsViewer *rv, const wxString &desc )
	: CaseCallbackContext( rv->GetSimulation()->GetCase(), desc ), m_resview( rv )
{
}

ResultsViewer *ResultsCallbackContext::GetResultsViewer()
{
	return m_resview;
}

Simulation *ResultsCallbackContext::GetSimulation()
{
	return m_resview->GetSimulation();
}

void ResultsCallbackContext::SetupLibraries( lk::env_t *env )
{
	env->register_funcs( invoke_resultscallback_funcs(), this );
}


enum { ID_CF_COPY = wxID_ANY , ID_CF_SAVECSV, ID_CF_SENDEXCEL, ID_CF_SENDEQNEXCEL };

BEGIN_EVENT_TABLE( ResultsViewer, wxMetroNotebook )	
	EVT_BUTTON(ID_CF_COPY, ResultsViewer::OnCommand)
	EVT_BUTTON(ID_CF_SAVECSV, ResultsViewer::OnCommand)
	EVT_BUTTON(ID_CF_SENDEXCEL, ResultsViewer::OnCommand)
	EVT_BUTTON(ID_CF_SENDEQNEXCEL, ResultsViewer::OnCommand)
END_EVENT_TABLE()

enum { PAGE_SUMMARY,
	PAGE_LOSS_DIAGRAM,
	PAGE_GRAPHS,
	PAGE_DATA,
	PAGE_CASH_FLOW,
	PAGE_HOURLY,
	PAGE_DAILY,
	PAGE_PROFILES,
	PAGE_STATISTICS,
	PAGE_HEAT_MAP,
	PAGE_SCATTER,
	PAGE_PDF_CDF,
	PAGE_DURATION_CURVE };

ResultsViewer::ResultsViewer( wxWindow *parent, int id )
	: wxMetroNotebook( parent, id, wxDefaultPosition, wxDefaultSize, wxMT_LIGHTTHEME ),
	//: wxNotebook( parent, id, wxDefaultPosition, wxDefaultSize ),
	 m_sim( 0 )
{
	m_summaryLayout = new wxSnapLayout( this, wxID_ANY );
	AddPage( m_summaryLayout, "Summary", true );
	m_metricsTable = new MetricsTable( m_summaryLayout );
	matrix_t<wxString> data( 1, 2 );
	data.at(0,0) = "Metric"; data.at(0,1) = "Value";
	m_metricsTable->SetData( data );	
	m_summaryLayout->Add( m_metricsTable );

	m_lossDiagramScroller = new wxScrolledWindow( this );
	m_lossDiagramScroller->SetBackgroundColour( *wxWHITE );
	m_lossDiagram = new LossDiagramCtrl( m_lossDiagramScroller );
	AddPage( m_lossDiagramScroller, "Losses" );

	m_graphViewer = new GraphViewer( this );
	AddPage( m_graphViewer, "Graphs" );

	m_tables = new TabularBrowser( this );
	AddPage( m_tables, "Data" );
	
	
	wxPanel *cf_panel = new wxPanel( this );
	AddPage( cf_panel, "Cash flow" );

	wxBoxSizer *cf_main_sizer = new wxBoxSizer(wxHORIZONTAL);
	m_cf_splitter = new wxSplitterWindow(cf_panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_NOBORDER|wxSP_LIVE_UPDATE|wxSP_3DSASH);
	cf_main_sizer->Add(m_cf_splitter, 1, wxBOTTOM | wxLEFT | wxEXPAND, 5);


	m_cf_top_panel = new wxPanel(m_cf_splitter);



	m_cashFlowTable = new wxExtGridCtrl(m_cf_top_panel, wxID_ANY);
	m_cashFlowTable->SetFont( *wxNORMAL_FONT );
	m_cashFlowTable->CreateGrid(1,1);
	m_cashFlowTable->SetRowLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);
	m_cashFlowTable->SetDefaultCellAlignment(wxALIGN_RIGHT,wxALIGN_CENTRE);
	m_cashFlowTable->DisableCellEditControl();
	m_cashFlowTable->DisableDragCell();
	m_cashFlowTable->DisableDragRowSize();
	m_cashFlowTable->DisableDragColMove();
	m_cashFlowTable->DisableDragGridSize();
	m_cashFlowTable->SetCellValue(0,0,"No data.");
	m_cashFlowTable->EnableEditing(false);
	m_cashFlowTable->EnableCopyPaste(true);
	m_cashFlowTable->EnablePasteEvent(false);
	m_depreciationTable = new wxExtGridCtrl(m_cf_splitter, wxID_ANY);
	m_depreciationTable->SetFont(*wxNORMAL_FONT);
	m_depreciationTable->CreateGrid(1, 1);
	m_depreciationTable->SetRowLabelAlignment(wxALIGN_LEFT, wxALIGN_CENTRE);
	m_depreciationTable->SetDefaultCellAlignment(wxALIGN_RIGHT, wxALIGN_CENTRE);
	m_depreciationTable->DisableCellEditControl();
	m_depreciationTable->DisableDragCell();
	m_depreciationTable->DisableDragRowSize();
	m_depreciationTable->DisableDragColMove();
	m_depreciationTable->DisableDragGridSize();
	m_depreciationTable->SetCellValue(0, 0, "No data.");
	m_depreciationTable->EnableEditing(false);
	m_depreciationTable->EnableCopyPaste(true);
	m_depreciationTable->EnablePasteEvent(false);
	wxBoxSizer *cf_tools = new wxBoxSizer(wxHORIZONTAL);
	cf_tools->Add(new wxButton(m_cf_top_panel, ID_CF_COPY, "Copy to clipboard"), 0, wxALL, 2);
	cf_tools->Add(new wxButton(m_cf_top_panel, ID_CF_SAVECSV, "Save as CSV"), 0, wxALL, 2);
#ifdef __WXMSW__
	cf_tools->Add(new wxButton(m_cf_top_panel, ID_CF_SENDEXCEL, "Send to Excel"), 0, wxALL, 2);
	cf_tools->Add(new wxButton(m_cf_top_panel, ID_CF_SENDEQNEXCEL, "Send to Excel with Equations"), 0, wxALL, 2);
#endif
	wxBoxSizer *cf_top_sizer = new wxBoxSizer( wxVERTICAL );
	cf_top_sizer->Add(cf_tools, 0, wxALL | wxEXPAND, 2);
	cf_top_sizer->Add(m_cashFlowTable, 1, wxALL | wxEXPAND, 0);
	m_cf_top_panel->SetSizer(cf_top_sizer);

	m_cf_splitter->SetMinimumPaneSize(100);
	m_cf_splitter->SetSashGravity(1.0);
	m_cf_splitter->SplitHorizontally(m_cf_top_panel, m_depreciationTable, -200);

	cf_panel->SetSizer(cf_main_sizer);
	cf_main_sizer->SetSizeHints(cf_panel);


	m_timeSeries = new wxDVTimeSeriesCtrl( this, wxID_ANY,  wxDV_RAW, wxDV_AVERAGE );
	AddPage( m_timeSeries, "Time series" );

	m_dailySeries = new wxDVTimeSeriesCtrl(this, wxID_ANY, wxDV_DAILY, wxDV_AVERAGE);
	AddPage( m_dailySeries, "Daily" );
		
	m_profilePlots = new wxDVProfileCtrl( this, wxID_ANY );
	AddPage( m_profilePlots, "Profiles" );

	m_statTable = new wxDVStatisticsTableCtrl( this, wxID_ANY );
	AddPage( m_statTable, "Statistics" );

	m_dMap = new wxDVDMapCtrl( this, wxID_ANY );
	AddPage( m_dMap, "Heat map" );
		
	m_scatterPlot = new wxDVScatterPlotCtrl( this, wxID_ANY );
	AddPage( m_scatterPlot, "Scatter" );

	m_pnCdf = new wxDVPnCdfCtrl( this, wxID_ANY );
	AddPage( m_pnCdf, "PDF / CDF" );
	
	m_durationCurve = new wxDVDCCtrl( this, wxID_ANY );
	AddPage( m_durationCurve, "Duration curve" );
}

wxDVPlotCtrlSettings ResultsViewer::GetDViewState()
{
	wxDVPlotCtrlSettings settings;

	settings.SetProperty(wxT("tabIndex"), GetSelection());

	//***TimeSeries Properties***
	settings.SetProperty(wxT("tsAxisMin"), m_timeSeries->GetViewMin());
	settings.SetProperty(wxT("tsAxisMax"), m_timeSeries->GetViewMax());	
	settings.SetProperty(wxT("tsTopSelectedNames"), m_timeSeries->GetDataSelectionList()->GetSelectedNamesInCol(0));
	settings.SetProperty(wxT("tsBottomSelectedNames"), m_timeSeries->GetDataSelectionList()->GetSelectedNamesInCol(1));
	
	settings.SetProperty(wxT("tsDailyAxisMin"), m_dailySeries->GetViewMin());
	settings.SetProperty(wxT("tsDailyAxisMax"), m_dailySeries->GetViewMax());	
	settings.SetProperty(wxT("tsDailyTopSelectedNames"), m_dailySeries->GetDataSelectionList()->GetSelectedNamesInCol(0));
	settings.SetProperty(wxT("tsDailyBottomSelectedNames"), m_dailySeries->GetDataSelectionList()->GetSelectedNamesInCol(1));

	//***DMap Tap Properties***
	settings.SetProperty(wxT("dmapCurrentName"), m_dMap->GetCurrentDataName());
	settings.SetProperty(wxT("dmapZMin"), m_dMap->GetZMin());
	settings.SetProperty(wxT("dmapZMax"), m_dMap->GetZMax());
	settings.SetProperty(wxT("dmapXMin"), m_dMap->GetXMin());
	settings.SetProperty(wxT("dmapXMax"), m_dMap->GetXMax());
	settings.SetProperty(wxT("dmapYMin"), m_dMap->GetYMin());
	settings.SetProperty(wxT("dmapYMax"), m_dMap->GetYMax());
	settings.SetProperty(wxT("dmapColourMap"), m_dMap->GetCurrentColourMap()->GetName());
	//Add link to ts

	//***Monthly Profile Properties***
	settings.SetProperty(wxT("profileJanSelected"), m_profilePlots->IsMonthIndexSelected(0));
	settings.SetProperty(wxT("profileFebSelected"), m_profilePlots->IsMonthIndexSelected(1));
	settings.SetProperty(wxT("profileMarSelected"), m_profilePlots->IsMonthIndexSelected(2));
	settings.SetProperty(wxT("profileAprSelected"), m_profilePlots->IsMonthIndexSelected(3));
	settings.SetProperty(wxT("profileMaySelected"), m_profilePlots->IsMonthIndexSelected(4));
	settings.SetProperty(wxT("profileJunSelected"), m_profilePlots->IsMonthIndexSelected(5));
	settings.SetProperty(wxT("profileJulSelected"), m_profilePlots->IsMonthIndexSelected(6));
	settings.SetProperty(wxT("profileAugSelected"), m_profilePlots->IsMonthIndexSelected(7));
	settings.SetProperty(wxT("profileSepSelected"), m_profilePlots->IsMonthIndexSelected(8));
	settings.SetProperty(wxT("profileOctSelected"), m_profilePlots->IsMonthIndexSelected(9));
	settings.SetProperty(wxT("profileNovSelected"), m_profilePlots->IsMonthIndexSelected(10));
	settings.SetProperty(wxT("profileDecSelected"), m_profilePlots->IsMonthIndexSelected(11));
	settings.SetProperty(wxT("profileAnnualSelected"), m_profilePlots->IsMonthIndexSelected(12));

	settings.SetProperty(wxT("profileSelectedNames"), m_profilePlots->GetDataSelectionList()->GetSelectedNamesInCol(0));

	//***Statistics Table Properties:  None

	//***PDF CDF Tab Properties***
	settings.SetProperty(wxT("pnCdfCurrentName"), m_pnCdf->GetCurrentDataName());
	settings.SetProperty(wxT("pnCdfNormalize"), int(m_pnCdf->GetNormalizeType()));
	settings.SetProperty(wxT("pnCdfBinSelectionIndex"), m_pnCdf->GetBinSelectionIndex());
	settings.SetProperty(wxT("pnCdfBins"), m_pnCdf->GetNumberOfBins());
	settings.SetProperty(wxT("pnCdfYMax"), m_pnCdf->GetYMax());


	//*** DURATION CURVE PROPERTIES*** 
	settings.SetProperty(wxT("dcSelectedNames"), m_durationCurve->GetDataSelectionList()->GetSelectedNamesInCol(0));


	//*** SCATTER PLOT PROPERTIES ***
	settings.SetProperty(wxT("scatterXDataName"), m_scatterPlot->GetScatterSelectionList()->GetSelectedNamesInCol(0));
	settings.SetProperty(wxT("scatterYDataNames"), m_scatterPlot->GetScatterSelectionList()->GetSelectedNamesInCol(1));


	return settings;
}

void ResultsViewer::SetDViewState( wxDVPlotCtrlSettings &settings )
{
	long i;
	settings.GetProperty(wxT("tabIndex")).ToLong(&i);
	SetSelection(i);

	//***TimeSeries Properties***
	m_timeSeries->SetTopSelectedNames(settings.GetProperty(wxT("tsTopSelectedNames")));
	m_timeSeries->SetBottomSelectedNames(settings.GetProperty(wxT("tsBottomSelectedNames")));

	//Set min/max after setting plots to make sure there is an axis to set.
	double min, max;
	settings.GetProperty(wxT("tsAxisMin")).ToDouble(&min);
	settings.GetProperty(wxT("tsAxisMax")).ToDouble(&max);
	m_timeSeries->SetViewMin(min);
	m_timeSeries->SetViewMax(max);

	
	m_dailySeries->SetTopSelectedNames(settings.GetProperty(wxT("tsDailyTopSelectedNames")));
	m_dailySeries->SetBottomSelectedNames(settings.GetProperty(wxT("tsDailyBottomSelectedNames")));
	
	settings.GetProperty(wxT("tsDailyAxisMin")).ToDouble(&min);
	settings.GetProperty(wxT("tsDailyAxisMax")).ToDouble(&max);
	m_dailySeries->SetViewMin(min);
	m_dailySeries->SetViewMax(max);
	

	//***DMap Tab Properties***
	m_dMap->SetCurrentDataName(settings.GetProperty(wxT("dmapCurrentName")));
	m_dMap->SetColourMapName(settings.GetProperty(wxT("dmapColourMap"))); //Do this before setting z min/max.

	settings.GetProperty(wxT("dmapZMin")).ToDouble(&min);
	settings.GetProperty(wxT("dmapZMax")).ToDouble(&max);
	m_dMap->SetZMin(min);
	m_dMap->SetZMax(max);
	settings.GetProperty(wxT("dmapXMin")).ToDouble(&min);
	settings.GetProperty(wxT("dmapXMax")).ToDouble(&max);
	m_dMap->SetXMin(min);
	m_dMap->SetXMax(max);
	settings.GetProperty(wxT("dmapYMin")).ToDouble(&min);
	settings.GetProperty(wxT("dmapYMax")).ToDouble(&max);
	m_dMap->SetYMin(min);
	m_dMap->SetYMax(max);


	//***Monthly Profile Properties***
	m_profilePlots->SetMonthIndexSelected(0, settings.GetProperty(wxT("profileJanSelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(1, settings.GetProperty(wxT("profileFebSelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(2, settings.GetProperty(wxT("profileMarSelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(3, settings.GetProperty(wxT("profileAprSelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(4, settings.GetProperty(wxT("profileMaySelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(5, settings.GetProperty(wxT("profileJunSelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(6, settings.GetProperty(wxT("profileJulSelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(7, settings.GetProperty(wxT("profileAugSelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(8, settings.GetProperty(wxT("profileSepSelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(9, settings.GetProperty(wxT("profileOctSelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(10, settings.GetProperty(wxT("profileNovSelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(11, settings.GetProperty(wxT("profileDecSelected")) == wxT("1"));
	m_profilePlots->SetMonthIndexSelected(12, settings.GetProperty(wxT("profileAnnualSelected")) == wxT("1"));

	m_profilePlots->SetSelectedNames(settings.GetProperty(wxT("profileSelectedNames")));

	//***Statistics Table Properties:  None

	//***PDF CDF Tab Properties***
	long normalize;
	settings.GetProperty(wxT("pnCdfNormalize")).ToLong(&normalize);
	m_pnCdf->SetNormalizeType( wxPLHistogramPlot::NormalizeType(normalize));
	long binIndex;
	settings.GetProperty(wxT("pnCdfBinSelectionIndex")).ToLong(&binIndex);
	m_pnCdf->SetBinSelectionIndex(binIndex);
	long bins;
	settings.GetProperty(wxT("pnCdfBins")).ToLong(&bins);
	m_pnCdf->SetNumberOfBins(bins);
	m_pnCdf->SetCurrentDataName(settings.GetProperty(wxT("pnCdfCurrentName")), true);
	double yMax;
	settings.GetProperty(wxT("pnCdfYMax")).ToDouble(&yMax);
	m_pnCdf->SetYMax(yMax);


	//*** DURATION CURVE PROPERTIES ***
	m_durationCurve->SetSelectedNames(settings.GetProperty(wxT("dcSelectedNames")), true);
	
	
	//*** SCATTER PLOT PROPERTIES ***
	m_scatterPlot->SetXSelectedName(settings.GetProperty(wxT("scatterXDataName")));
	m_scatterPlot->SetYSelectedNames(settings.GetProperty(wxT("scatterYDataNames")));
		

	Refresh();
	Update();
}

	
wxString ResultsViewer::GetCurrentContext() const
{
	switch( GetSelection() )
	{
	case 0: return "summary";
	case 1: return "graphs";
	case 2: return "data";
	case 3: return "cashflow";
	case 4: return "dview_hourly";
	case 5: return "dview_daily";
	case 6: return "dview_profiles";
	case 7: return "dview_heatmap";
	case 8: return "dview_scatter";
	case 9: return "dview_pdfcdf";
	case 10: return "dview_durationcurve";
	default: return "results";
	}
}

ResultsViewer::~ResultsViewer()
{
	for( size_t i=0;m_tsDataSets.size();i++ )
		delete m_tsDataSets[i];
}

class TimeSeries : public wxDVTimeSeriesDataSet
{
	float *m_pdata;
	size_t m_len;
	double m_tsHour;
	wxString m_label, m_units;
public:
	TimeSeries( float *p, size_t len, double ts_hour, const wxString &label, const wxString &units )
		: wxDVTimeSeriesDataSet(), m_pdata(p), m_len(len), m_tsHour(ts_hour), m_label(label), m_units(units) { }
	virtual wxRealPoint At(size_t i) const
	{
		if ( i < m_len ) return wxRealPoint(i*m_tsHour, m_pdata[i]);
		else return wxRealPoint(0,0);
	}
	virtual size_t Length() const { return m_len; }
	virtual double GetTimeStep() const { return m_tsHour; }
	virtual double GetOffset() const { return 0.0; }
	virtual wxString GetSeriesTitle() const { return m_label; }
	virtual wxString GetUnits() const { return m_units; }
	virtual void SetDataValue(size_t i, double newYValue) { /* nothing to do */ }
};

class ExcelExchSummary : public MetricsTable
{
public:
	ExcelExchSummary( wxWindow *parent, Simulation *sim )
		: MetricsTable( parent )
	{
		ConfigInfo *ci = sim->GetCase()->GetConfiguration();
		std::vector<ExcelExchange::Captured> &summ = sim->GetCase()->ExcelExch().Summary;
		matrix_t<wxString> mat( summ.size()+1, 3 );
		mat(0,0) = "Captured from Excel";
		mat(0,1) = "Range";
		mat(0,2) = "Value";
		for( size_t i=0;i<summ.size();i++ )
		{
			mat(i+1,0) = ci->Variables.Label( summ[i].Name );
			mat(i+1,1) = summ[i].Range;
			mat(i+1,2) = summ[i].Value;
		}
		SetData( mat );
	}
};

void ResultsViewer::Setup( Simulation *sim )
{
	m_sim = sim;
	ConfigInfo *cfg = ( m_sim != 0 ? m_sim->GetCase()->GetConfiguration() : 0 );

	if ( cfg == 0 || m_sim == 0 )
	{
		Clear();
		return;
	}

	// save the current view
	StringHash viewinfo;
	SavePerspective( viewinfo );

	// update metrics
	m_metricsTable->Clear();

	m_metrics.clear();
	ResultsCallbackContext cc( this, "Metrics callback: " + cfg->Technology + ", " + cfg->Financing );
		
	// first try to invoke a T/F specific callback if one exists
	if ( lk::node_t *metricscb = SamApp::GlobalCallbacks().Lookup( "metrics", cfg->Technology + "|" + cfg->Financing ))
		cc.Invoke( metricscb, SamApp::GlobalCallbacks().GetEnv() );

	// if no metrics were defined, run it T & F one at a time
	if ( m_metrics.size() == 0 )
	{
		if ( lk::node_t *metricscb = SamApp::GlobalCallbacks().Lookup( "metrics", cfg->Technology ))
			cc.Invoke( metricscb, SamApp::GlobalCallbacks().GetEnv() );
		
		if ( lk::node_t *metricscb = SamApp::GlobalCallbacks().Lookup( "metrics", cfg->Financing ))
			cc.Invoke( metricscb, SamApp::GlobalCallbacks().GetEnv() );
	}
	
	if ( m_metrics.size() > 0 && m_sim->Outputs().size() > 0 )
	{
		matrix_t<wxString> metrics;
		metrics.resize( m_metrics.size()+1, 2 );
		metrics(0,0) = "Metric";
		metrics(0,1) = "Value";

		for( size_t i=0;i<m_metrics.size();i++ )
		{
			MetricData &md = m_metrics[i];
			wxString slab( md.var );
			wxString sval( "<inval>" );

			double value = 0.0;
			if ( VarValue *vv = m_sim->GetValue( md.var ) )
			{
				value = md.scale*(double)vv->Value();

				int deci = md.deci;
				if ( md.mode == 'g' ) deci = wxNumericCtrl::GENERIC;
				else if ( md.mode == 'e' ) deci = wxNumericCtrl::EXPONENTIAL;
				else if ( md.mode == 'h' ) deci = wxNumericCtrl::HEXADECIMAL;
			
				slab = md.label;
				if ( slab.IsEmpty() )
					slab = m_sim->GetLabel( md.var );
				
				wxString post = md.post;
				if ( post.IsEmpty() )
					post = " " + m_sim->GetUnits( md.var );
				
				sval = wxNumericCtrl::Format( value, wxNumericCtrl::REAL, 
					deci, md.thousep, md.pre, post );
			}

			metrics(i+1, 0) = slab;
			metrics(i+1, 1) = sval;
		}

		m_metricsTable->SetData( metrics );
	}
	else if ( m_sim->Outputs().size() > 0 )
	{
		wxArrayString mvars;
		std::vector<double> mvals;
		wxArrayString vars = m_sim->ListOutputs();
		for( size_t i=0;i<vars.size();i++ )
			if ( VarValue *vv = m_sim->GetValue( vars[i] ) )
				if ( vv->Type() == VV_NUMBER )
				{
					mvars.Add( vars[i] );
					mvals.push_back( vv->Value() );
				}

		if ( mvars.size() > 0 )
		{
			matrix_t<wxString> metrics( mvars.size()+1, 2 );
			metrics(0,0) = "Auto-metric";
			metrics(0,1) = "Value";
			for( size_t i=0;i<mvars.size();i++ )
			{
				wxString label = m_sim->GetLabel( mvars[i] );
				if ( label.IsEmpty() ) label = mvars[i];
				metrics(i+1,0) = label;

				metrics(i+1,1) = wxString::Format("%lg", mvals[i]) + " " + m_sim->GetUnits(mvars[i]);
			}
			m_metricsTable->SetData( metrics );
		}
	}
	else
	{
		matrix_t<wxString> metrics( 2, 1 );
		metrics(0,0) = "No results are available.";
		metrics(1,0) = "Click the 'Simulate' button first to run a simulation.";
		m_metricsTable->SetData( metrics );
	}


	// update excel exchange outputs
	size_t i=0;
	while( i < m_summaryLayout->Count() )
	{
		if ( ExcelExchSummary *exsum = dynamic_cast<ExcelExchSummary*>( m_summaryLayout->Get(i) ) )
			m_summaryLayout->Delete( exsum );
		else
			i++;
	}

	if ( sim->GetCase()->ExcelExch().Enabled && sim->GetCase()->ExcelExch().Summary.size() > 0 )
		m_summaryLayout->Add( new ExcelExchSummary( m_summaryLayout, sim ) );


	// setup loss diagram
	m_lossDiagram->GetDiagram().Clear();
	m_lossDiagram->GetDiagram().SetCaseName( SamApp::Window()->Project().GetCaseName( m_sim->GetCase() ) );
	if ( m_lossDiagram->GetDiagram().SetupFromCase() )
	{
		m_lossDiagram->InvalidateBestSize();
		wxSize ldsz = m_lossDiagram->GetBestSize();
		m_lossDiagram->SetSize( 20, 20, ldsz.x, ldsz.y);
		m_lossDiagramScroller->SetScrollbars(1, 1, ldsz.x+40, ldsz.y+40, 0, 0);
	}
	if ( m_lossDiagram->GetDiagram().Size() > 0 )
		ShowPage( PAGE_LOSS_DIAGRAM );
	else
		HidePage( PAGE_LOSS_DIAGRAM );
		
	// setup time series datasets
	wxDVPlotCtrlSettings viewstate = GetDViewState();
	RemoveAllDataSets();
	wxArrayString vars = m_sim->ListOutputs();
	for( size_t i=0;i<vars.size();i++ )
	{
		if ( VarValue *vv = m_sim->GetValue( vars[i] ) )
		{
			if ( vv->Type() == VV_ARRAY )
			{
				size_t n = 0;
				float *p = vv->Array( &n );
				
				size_t steps_per_hour = n/8760;
				if ( steps_per_hour > 0 
					&& steps_per_hour <= 60 
					&& n == steps_per_hour*8760 )
				{
					wxString group( "Hourly Data" );
					if ( steps_per_hour > 1 )
						group = wxString::Format( "%lg Minute Data", 60.0/steps_per_hour );

					wxLogStatus("Adding time series dataset: %d len, %lg time step", (int)n, 1.0/steps_per_hour );
					AddDataSet( new TimeSeries( p, n, 1.0/steps_per_hour,
						m_sim->GetLabel(vars[i]), 
						m_sim->GetUnits(vars[i])), 
						group );
				}
			}
		}
	}

	m_statTable->RebuildDataViewCtrl();
	SetDViewState( viewstate );

	// setup graphs
	m_graphViewer->Setup( m_sim );

	m_tables->Setup( m_sim );

	// build cashflow
	

	m_cashflow.clear();
	if ( lk::node_t *cfcb = SamApp::GlobalCallbacks().Lookup( "cashflow", cfg->Financing ))
	{
		m_cashFlowTable->Freeze();
		m_cashFlowTable->ClearGrid();
		m_depreciationTable->Freeze();
		m_depreciationTable->ClearGrid();

		m_depreciationTable->ResizeGrid(20, 16);

		ResultsCallbackContext cc( this, "Cashflow callback: " + cfg->Financing );
		if ( !cc.Invoke( cfcb, SamApp::GlobalCallbacks().GetEnv() ) )
			wxLogStatus( "error running cashflow script." );
		
		int nyears = 0;
		if ( VarValue *vv = m_sim->GetValue( cfg->Settings[ "analysis_period_var" ] ) )
			nyears = (int)vv->Value();
		if ( nyears < 16 ) nyears = 16;
		if ( nyears > 100 ) nyears = 100;
		m_cashFlowTable->ResizeGrid(400, nyears);
		for (size_t c = 0; c<nyears; c++)
			m_cashFlowTable->SetColLabelValue( c, wxString::Format("%d", (int)c) );

		size_t cashflow_row = 0, depreciation_row = 0;
		for( size_t r=0;r<m_cashflow.size() && r < 400;r++ )
		{
			CashFlowLine &cl = m_cashflow[r];

			if (cl.type == CashFlowLine::SPACER)
			{
				m_cashFlowTable->SetRowLabelValue(cashflow_row, wxEmptyString);
				cashflow_row++;
			}
			else if (cl.type == CashFlowLine::HEADER)
			{
				m_cashFlowTable->SetRowLabelValue(cashflow_row, cl.name);
				if ( cl.scale == 1.0 )
				{
					wxColour bgcol( m_cashFlowTable->GetGridRowLabelWindow()->GetBackgroundColour() );
					for( size_t c=0;c<nyears;c++ )
						m_cashFlowTable->SetCellBackgroundColour( cashflow_row, c, bgcol );
				}
				cashflow_row++;
			}
			else if (cl.type == CashFlowLine::VARIABLE)
			{
				wxString label = m_sim->GetLabel(cl.name);
				wxString units = m_sim->GetUnits(cl.name);
				if (!units.IsEmpty()) label += " (" + units + ")";
				m_cashFlowTable->SetRowLabelValue(cashflow_row, label);

//				if( cl.coloff != 0 )
//					wxLogStatus("colloffset nonzero in cashflow");

				if (VarValue *vv = m_sim->GetValue(cl.name))
				{
					float _val = 0.0f;
					float *p = &_val;
					size_t n = 1;

					if (vv->Type() == VV_ARRAY) p = vv->Array(&n);
					else if (vv->Type() == VV_NUMBER) _val = vv->Value();

					for (size_t i = 0; i < n && i+cl.coloff < nyears; i++)
					{
						float fval = p[i] * cl.scale;
						wxString sval;
						if (cl.digits >= 0 && fval != 0.0f)
							sval = wxNumericCtrl::Format(fval, wxNumericCtrl::REAL, cl.digits, true, wxEmptyString, wxEmptyString);
						else if (cl.digits == -3) // integer cast
							sval = wxString::Format("%d", (int)fval);
						else // cl.digits == -2 // generic format
							sval = wxString::Format("%g", fval);

						m_cashFlowTable->SetCellValue(sval, cashflow_row, cl.coloff+i);
					}
				}
				cashflow_row++;
			}
			else if (cl.type == CashFlowLine::CELLHEADER)
			{ // cl.name contains comma separated values for row
				wxArrayString list = wxSplit(cl.name, ',');
				size_t n = list.size();
				if (n == 0)
				{
					list.Add(""); // handle empty string
					n = 1;
				}
				m_depreciationTable->SetRowLabelValue(depreciation_row, list[0]);
				for (size_t i = 1; i < n && i < nyears; i++)
				{
					m_depreciationTable->SetCellValue(list[i], depreciation_row, i - 1);
				}
				depreciation_row++;
			}
			else if (cl.type == CashFlowLine::CELLCOLHEADER)
			{ // cl.name contains comma separated values for row
				wxArrayString list = wxSplit(cl.name, ',');
				size_t n = list.size();
				if (n == 0)
				{
					list.Add(""); // handle empty string
					n = 1;
				}
				for (size_t i = 0; i < n && i < nyears; i++)
				{
					m_depreciationTable->SetColLabelValue(i, list[i]);
				}
				//depreciation_row++;
			}
			else if (cl.type == CashFlowLine::CELLVARIABLE)
			{
				wxArrayString list = wxSplit(cl.name, ',');
				size_t n = list.size();
				if (n == 0)
				{
					list.Add(""); // handle empty string
					n = 1;
				}
				m_depreciationTable->SetRowLabelValue(depreciation_row, list[0]);
				for (size_t i = 1; i < n && i < nyears; i++)
				{
					if (VarValue *vv = m_sim->GetValue(list[i]))
					{
						float _val = 0.0f;
						float *p = &_val;
						size_t m = 1;

						if (vv->Type() == VV_ARRAY) p = vv->Array(&n);
						else if (vv->Type() == VV_NUMBER) _val = vv->Value();

						wxString sval;
						for (size_t j = 0; j < m && j < nyears; j++)
						{
							float fval = p[j] * cl.scale;
							if (cl.digits > 0 && fval != 0.0f)
								sval += wxNumericCtrl::Format(fval, wxNumericCtrl::REAL, cl.digits, true, wxEmptyString, wxEmptyString);
							else if (cl.digits == -2)
								sval += wxString::Format("%d", (int)fval);
							else
								sval += wxString::Format("%g", fval);
							if (j < m-1) sval += ";";
						}
						m_depreciationTable->SetCellValue(sval, depreciation_row, i - 1);
					}
				}
				depreciation_row++;
			}
			else
			{
				m_cashFlowTable->SetCellValue("'" + cl.name + "' not found.", r, 0);
				cashflow_row++;
			}
		}

		m_cashFlowTable->SetRowLabelSize(wxGRID_AUTOSIZE);
		m_cashFlowTable->SetColLabelSize(wxGRID_AUTOSIZE);
		m_depreciationTable->SetRowLabelSize(wxGRID_AUTOSIZE);
		m_depreciationTable->SetColLabelSize(wxGRID_AUTOSIZE);
		m_cashFlowTable->AutoSize();
		m_depreciationTable->AutoSize();
		m_cashFlowTable->EnableCopyPaste(true);
		m_cashFlowTable->ResizeGrid(cashflow_row, nyears);
		m_depreciationTable->EnableCopyPaste(true);
		m_depreciationTable->ResizeGrid(depreciation_row, 16);

		m_cashFlowTable->Thaw();
		m_depreciationTable->Thaw();

		m_depreciationTable->Show(depreciation_row > 0);
		if (!m_depreciationTable->IsShown())
		{
			m_cf_splitter->Unsplit();
		}
	}

	if ( m_cashflow.size() > 0 ) ShowPage( PAGE_CASH_FLOW );
	else HidePage( PAGE_CASH_FLOW );

	CreateAutoGraphs();

	m_summaryLayout->AutoLayout();
	
	// load the formerly saved perspective
	LoadPerspective( viewinfo );
}

void ResultsViewer::AddDataSet(wxDVTimeSeriesDataSet *d, const wxString& group, bool update_ui)
{
	//Take ownership of the data Set.  We will delete it on destruction.
	m_tsDataSets.push_back(d);
	
	m_timeSeries->AddDataSet(d, group, update_ui);
	m_dailySeries->AddDataSet(d, group, update_ui);
	m_dMap->AddDataSet(d, group, update_ui);
	m_profilePlots->AddDataSet(d, group, update_ui);
	m_statTable->AddDataSet(d, group);
	m_pnCdf->AddDataSet(d, group, update_ui); 
	m_durationCurve->AddDataSet(d, group, update_ui);
	m_scatterPlot->AddDataSet(d, group, update_ui);
}


void ResultsViewer::GetExportData(int data, matrix_t<wxString> &table)
{
	if (data & EXP_CASHFLOW)
	{
		int nrows = m_cashFlowTable->GetNumberRows();
		int ncols = m_cashFlowTable->GetNumberCols();
		int ndepr_rows = m_depreciationTable->GetNumberRows();
		int ndepr_cols = m_depreciationTable->GetNumberCols();
		int r, c;

		table.resize(nrows + ndepr_rows + 2, ncols + ndepr_cols + 2);

		for (c = 0; c<ncols; c++)
			table.at(0, c + 1) = wxString::Format( "%d", c);

		for (r = 0; r<nrows; r++)
		{
			table.at(r + 1, 0) = m_cashFlowTable->GetRowLabelValue(r);
			for (c = 0; c<ncols; c++)
				table.at(r + 1, c + 1) = m_cashFlowTable->GetCellValue(r, c);
		}

		for (c = 0; c<ndepr_cols; c++)
			table.at(nrows, c + 1) = " ";

		for (r = 0; r<ndepr_rows; r++)
		{
			table.at(nrows+r + 1, 0) = m_depreciationTable->GetRowLabelValue(r);
			for (c = 0; c<ndepr_cols; c++)
				table.at(nrows + r + 1, c + 1) = m_depreciationTable->GetCellValue(r, c);
		}
	}
}



void ResultsViewer::Export(int data, int mechanism)
{
	matrix_t<wxString> table;

	GetExportData(data, table);

	if (table.nrows() == 0 || table.ncols() == 0)
	{
		wxMessageBox("No data to export!");
		return;
	}


	// now select export mechanism
	if (mechanism & EXP_COPY_CLIPBOARD)
	{
		if (wxTheClipboard->Open())
		{
			wxString tab_data = UnsplitCells(table, '\t', '\n', false);
			// remove commas per request from Paul 5/23/12 meeting
			tab_data.Replace(",", "");

			wxTheClipboard->SetData(new wxTextDataObject(
				tab_data));
			wxTheClipboard->Close();
		}
	}

	if (mechanism & EXP_SAVE_CSV)
	{
		wxString csv_data = UnsplitCells(table, ',', '\n', true);

		wxFileDialog fdlg(this, "Save Data", "", "samdata.csv",
			"CSV Data Files (*.csv)|*.csv", wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

		if (fdlg.ShowModal() == wxID_OK)
		{
			wxString fn = fdlg.GetPath();
			if (fn != "")
			{
				// ensure the extension is attached
				wxString ext;
				wxFileName::SplitPath(fn, NULL, NULL, NULL, &ext);
				if (ext.Lower() != "csv")
					fn += ".csv";

				FILE *fp = fopen(fn.c_str(), "w");
				if (fp)
				{
					fputs(csv_data.c_str(), fp);
					fclose(fp);
				}
				else
					wxMessageBox("Could not write to file:\n\n" + fn, "Save Error", wxICON_ERROR);
			}
		}
	}

#ifdef __WXMSW__
	if (mechanism & EXP_SEND_EXCEL)
	{
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
		// Excel 2013 requires starting before copying to clipboard
		if (wxTheClipboard->Open())
		{
			wxString tab_data = UnsplitCells(table, '\t', '\n', false);
			// remove commas per request from Paul 5/23/12 meeting
			tab_data.Replace(",", "");

			wxTheClipboard->SetData(new wxTextDataObject(
				tab_data));
			wxTheClipboard->Close();
		}
		xl.PasteClipboard();
		xl.AutoFitColumns();
		xl.SetSelectedCellsFontSize(9);
		xl.SetRowColBold(1, true);
		if (data == EXP_CASHFLOW) xl.SetRowColBold(-1, true);

	}
#endif


}

void ResultsViewer::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case ID_CF_COPY:
		Export(EXP_CASHFLOW, EXP_COPY_CLIPBOARD);
		break;
	case ID_CF_SAVECSV:
		Export(EXP_CASHFLOW, EXP_SAVE_CSV);
		break;
	case ID_CF_SENDEXCEL:
		Export(EXP_CASHFLOW, EXP_SEND_EXCEL);
		break;
	case ID_CF_SENDEQNEXCEL:
		ExportEqnExcel();
		break;
	}
}

class AutoGraphCtrl : public GraphCtrl
{
public:
	AutoGraphCtrl( wxWindow *parent, Simulation *sim, Graph &g )
		: GraphCtrl( parent, wxID_ANY )
	{
		Display( sim, g );
	}
	virtual ~AutoGraphCtrl() { }
};

void ResultsViewer::CreateAutoGraphs()
{
	ConfigInfo *cfg = (m_sim != 0 ? m_sim->GetCase()->GetConfiguration() : 0);
	if (!cfg)
	{
		wxMessageBox("no configuration could be determined");
		return;
	}

	m_autographs.clear();
	ResultsCallbackContext cc(this, "Create autographs callback: " + cfg->Technology);

	if (lk::node_t *cfcb = SamApp::GlobalCallbacks().Lookup("autographs", cfg->Technology + "|" + cfg->Financing))
	{
		if (!cc.Invoke(cfcb, SamApp::GlobalCallbacks().GetEnv()))
			wxLogStatus("error running create autographs script.");
	}

	if ( m_autographs.size() == 0 )
	{
		if (lk::node_t *cfcb = SamApp::GlobalCallbacks().Lookup("autographs", cfg->Technology))
		{
			if (!cc.Invoke(cfcb, SamApp::GlobalCallbacks().GetEnv()))
				wxLogStatus("error running create autographs script.");
		}

		if (lk::node_t *cfcb = SamApp::GlobalCallbacks().Lookup("autographs", cfg->Financing))
		{
			if (!cc.Invoke(cfcb, SamApp::GlobalCallbacks().GetEnv()))
				wxLogStatus("error running create autographs script.");
		}
	}


	// clear all the current autographs
	size_t i=0;
	while( i<m_summaryLayout->Count() )
	{
		if( AutoGraphCtrl *ag = dynamic_cast<AutoGraphCtrl*>( m_summaryLayout->Get(i) ) )
			m_summaryLayout->Delete( ag );
		else
			i++;
	}

	if ( !m_sim || m_sim->Outputs().size() == 0 ) return;

	for( size_t i=0;i<m_autographs.size();i++ )
	{
		Graph g;
		g.Y = wxSplit( m_autographs[i].yvals, ',' );
		g.Title = m_autographs[i].title;
		g.XLabel = m_autographs[i].xlabel;
		g.YLabel = m_autographs[i].ylabel;
		if (m_autographs[i].legend_pos.Lower() == "right")
			g.LegendPos = wxPLPlotCtrl::RIGHT;
		else if (m_autographs[i].legend_pos.Lower() == "floating")
			g.LegendPos = wxPLPlotCtrl::FLOATING;
		else
			g.LegendPos = wxPLPlotCtrl::BOTTOM;
		g.ShowXValues = m_autographs[i].show_xvalues;
		g.ShowLegend = m_autographs[i].show_legend;
		g.Size = m_autographs[i].size;
		m_summaryLayout->Add( new AutoGraphCtrl( m_summaryLayout, m_sim, g ) );
	}
}

void ResultsViewer::ExportEqnExcel()
{
	if ( !m_sim ) return;

	ConfigInfo *cfg = ( m_sim != 0 ? m_sim->GetCase()->GetConfiguration() : 0);
	if ( !cfg )
	{
		wxMessageBox("no configuration could be determined");
		return;
	}

	if (lk::node_t *cfcb = SamApp::GlobalCallbacks().Lookup("cashflow_to_excel", cfg->Financing))
	{
		CaseCallbackContext cc( m_sim->GetCase(), "Cashflow to Excel callback: " + cfg->Financing);
		if (!cc.Invoke(cfcb, SamApp::GlobalCallbacks().GetEnv()))
			wxLogStatus("error running cashflow to excel script.");
	}
}

void ResultsViewer::RemoveAllDataSets()
{
	m_timeSeries->RemoveAllDataSets();
	m_dailySeries->RemoveAllDataSets();
	m_dMap->RemoveAllDataSets();
	m_profilePlots->RemoveAllDataSets();
	m_statTable->RemoveAllDataSets();
	m_pnCdf->RemoveAllDataSets();
	m_durationCurve->RemoveAllDataSets();
	m_scatterPlot->RemoveAllDataSets();
	
	for (int i=0; i<m_tsDataSets.size(); i++)
		delete m_tsDataSets[i];

	m_tsDataSets.clear();
}

void ResultsViewer::SavePerspective( StringHash &map )
{
	// save information about the current view
	map[ "navigation" ] = wxString::Format( "%d", GetSelection() );
}

void ResultsViewer::LoadPerspective( StringHash &map )
{
	int nnav = wxAtoi( map["navigation"] );
	if ( nnav >= 0 && nnav < GetPageCount() )
		SetSelection( nnav );
}


void ResultsViewer::SetGraphs( std::vector<Graph> &gl )
{
	m_graphViewer->SetGraphs( gl );
}

void ResultsViewer::GetGraphs( std::vector<Graph> &gl )
{
	m_graphViewer->GetGraphs( gl );
}

void ResultsViewer::Clear()
{
	m_sim = 0;

	matrix_t<wxString> metrics(2,1);
	metrics(0,0) = "Metrics";
	metrics(1,0) = "No data available.";
	m_metricsTable->SetData( metrics );

	RemoveAllDataSets();
}


enum { ID_METRICS_COPY_TSV = wxID_HIGHEST+258, ID_METRICS_COPY_CSV };

#define MT_BORDER 1
#define MT_SPACE 4

BEGIN_EVENT_TABLE( MetricsTable, wxWindow)
	EVT_SIZE( MetricsTable::OnResize)
	EVT_PAINT( MetricsTable::OnPaint)
	EVT_RIGHT_DOWN(MetricsTable::OnRightClick)
	EVT_MENU( ID_METRICS_COPY_TSV, MetricsTable::OnContextMenu )
	EVT_MENU( ID_METRICS_COPY_CSV, MetricsTable::OnContextMenu )
END_EVENT_TABLE()

MetricsTable::MetricsTable(wxWindow *parent)
	: wxWindow(parent,-1,wxDefaultPosition, wxDefaultSize, wxCLIP_CHILDREN)
{
	m_rowHeight = 10;
	SetFont( *wxNORMAL_FONT );
	SetBackgroundStyle(wxBG_STYLE_CUSTOM);
}

void MetricsTable::OnContextMenu(wxCommandEvent &evt)
{
	wxString sep = evt.GetId() == ID_METRICS_COPY_CSV ? "," : "\t";

	wxString tdat;
	for (int r=0;r<m_table.nrows();r++)
		for (int c=0;c<m_table.ncols();c++)
			tdat += m_table.at(r,c) + (c==m_table.ncols()-1 ? "\n" : sep);

	if (wxTheClipboard->Open())
	{
	// This data objects are held by the clipboard, 
	// so do not delete them in the app.
		wxTheClipboard->SetData( new wxTextDataObject(tdat) );
		wxTheClipboard->Close();
	}
}

void MetricsTable::OnRightClick(wxMouseEvent &evt)
{
	wxMenu popup;
	popup.Append( ID_METRICS_COPY_TSV, "Copy table");
	popup.Append( ID_METRICS_COPY_CSV, "Copy as CSV");
	PopupMenu( &popup, evt.GetPosition() );
}

void MetricsTable::SetData(const matrix_t<wxString> &data)
{
	m_table = data;
	if ( m_table.nrows() > 0 && m_table.ncols() > 0 )
	{
		m_cellsz.resize_fill( m_table.nrows(), m_table.ncols(), wxSize(40,15) );
		wxClientDC dc( const_cast<MetricsTable*>(this) );
		dc.SetFont( GetFont() );
		for (size_t r=0;r<m_table.nrows();r++)
			for (size_t c=0;c<m_table.ncols();c++)
				m_cellsz(r,c) = dc.GetTextExtent( m_table(r,c) );

	
		m_colxsz.resize( m_table.ncols(), 1 );
		for (size_t c=0;c<m_table.ncols();c++)
		{
			m_colxsz[c] = 0;
			for (size_t r=0;r<m_table.nrows();r++)
				if (m_cellsz(r,c).x > m_colxsz[c])
					m_colxsz[c] = m_cellsz(r,c).x;

			m_colxsz[c] += MT_SPACE*2;
		}
			
		m_rowHeight = dc.GetCharHeight() + MT_SPACE;
	}

	InvalidateBestSize();
	Refresh();
}

void MetricsTable::Clear()
{
	m_table.clear();
	Refresh();
}

wxSize MetricsTable::DoGetBestSize() const
{
	int width = 0, height = 0;
	for( size_t i=0;i<m_colxsz.size();i++ )
		width += m_colxsz[i];
	width += 2*MT_BORDER;
	if ( width < 100 ) width = 100;

	height = m_rowHeight * m_table.nrows() + 2*MT_BORDER;
	if ( height < 10 ) height = 10;

	return wxSize(width,height);
}


void MetricsTable::OnPaint(wxPaintEvent &evt)
{
	wxAutoBufferedPaintDC dc(this);
	int cwidth, cheight;
	GetClientSize(&cwidth, &cheight);
	
	dc.SetPen( wxPen( wxColour(50,50,50), 1) );
	dc.SetBrush( wxBrush( wxColour(50,50,50), wxSOLID ) );
	dc.DrawRectangle(0,0,cwidth,cheight);

	dc.SetFont( GetFont() );

	int ch = dc.GetCharHeight();

	int nrows = m_table.nrows();
	int ncols = m_table.ncols();

	if ( nrows == 0 || ncols == 0 ) return;

	int r,c;

	dc.SetTextForeground( *wxWHITE );

	for (int i=0;i<m_table.ncols();i++)
	{
		if (m_table.at(0,i).IsEmpty())
			continue;

		int xpos = MT_BORDER;
		for (int k=0;k<i && k<m_colxsz.size();k++)
			xpos += m_colxsz[k];

		dc.DrawText(m_table.at(0,i), xpos+MT_SPACE, MT_BORDER+1 );
	}
	
	
	int wy = MT_BORDER+m_rowHeight;

	dc.SetPen( wxPen( *wxWHITE, 1) );
	dc.SetBrush( wxBrush( *wxWHITE, wxSOLID ) );

	wxRect tabR( MT_BORDER, wy,cwidth-MT_BORDER-MT_BORDER,cheight-wy-MT_BORDER );

	dc.DrawRectangle( tabR );
	dc.SetClippingRegion( tabR );

	// draw table;

	dc.SetFont( GetFont() );
	dc.SetTextForeground(*wxBLACK);
	
	for (r=1;r<nrows;r++)
	{
		int xpos = MT_BORDER;
		for (c=0;c<ncols;c++)
		{
			dc.DrawText(m_table.at(r,c), xpos+MT_SPACE, wy+1);			
			xpos += m_colxsz[c];
		}

		wy += m_rowHeight;
	}

	dc.SetPen( wxPen(wxColour(240,240,240), 1) );
	for (r=1;r<nrows;r++)
		dc.DrawLine(MT_BORDER,r*m_rowHeight+tabR.y, cwidth-MT_BORDER, r*m_rowHeight+tabR.y);

	dc.DestroyClippingRegion();
}

void MetricsTable::OnResize(wxSizeEvent &evt)
{
	Refresh();
}






class TabularBrowser::ResultsTable : public wxGridTableBase
{
public:

	struct ColData
	{
		wxString Label;
		float *Values;
		float SingleValue;
		size_t N;
	};

	size_t MaxCount;
	size_t MinCount;
	std::vector<ColData*> Table;


	ResultsTable()
	{
		MinCount = 0;
		MaxCount = 0;
	}

	virtual ~ResultsTable()
	{
		ReleaseData();
	}

    virtual int GetNumberRows()
	{
		return MaxCount;
	}

    virtual int GetNumberCols()
	{
		return Table.size();
	}

    virtual bool IsEmptyCell( int row, int col )
	{
		return (col < 0 || col >= Table.size()
			|| row >= Table[col]->N || row < 0);
	}

    virtual wxString GetValue( int row, int col )
	{
		if ( col >= 0 && col < Table.size() && row >= 0 && row < Table[col]->N )
		{
			return wxString::Format("%g", Table[col]->Values[row]);
		}
		else return wxEmptyString;
	}

    virtual void SetValue( int row, int col, const wxString &)
	{
		// nothing to do
	}

	virtual wxString GetColLabelValue( int col )
	{
		if (col >= 0 && col < Table.size())	return Table[col]->Label;
		else return wxEmptyString;
	}

	virtual wxString GetRowLabelValue( int row )
	{
		if ( MinCount == MaxCount && MaxCount == 8760 )
			return wxFormatTime( row );
		else
			return wxString::Format("%d",row+1);
	}

    virtual wxString GetTypeName( int row, int col )
	{
		return wxGRID_VALUE_STRING;
	}

	void LoadData( Simulation *results, const wxArrayString &vars)
	{
		MaxCount = 0;
		MinCount = 0;
		Table.clear();

		if ( vars.size() == 0 ) return;

		MinCount = 10000000;
		
		for (size_t i=0;i<vars.size();i++)
		{
			if( VarValue *vv = results->GetValue( vars[i] ) )
			{
				Table.push_back( new ColData() );
				ColData &cc = *Table[ Table.size()-1 ];

				cc.Label = vars[i];
				wxString label = results->GetLabel( vars[i] );
				if ( !label.IsEmpty() ) cc.Label = label;

				wxString units = results->GetUnits( vars[i] );
				if ( !units.IsEmpty() ) cc.Label += "\n(" + units + ")";

				cc.Values = 0;
				cc.N = 1;

				if ( vv->Type() == VV_ARRAY )
					cc.Values = vv->Array( &cc.N );
				else if ( vv->Type() == VV_NUMBER )
				{
					cc.SingleValue = vv->Value();
					cc.Values = &cc.SingleValue;
					cc.N = 1;
				}
				
				if ( cc.N > MaxCount )
					MaxCount = cc.N;

				if ( cc.N < MinCount )
					MinCount = cc.N;

			}
		}
	}

	void ReleaseData()
	{
		for( size_t i=0;i<Table.size();i++ )
			delete Table[i];

		Table.clear();
		MaxCount = 0;
	}

};


enum { IDOB_COPYCLIPBOARD=wxID_HIGHEST+494, 
	IDOB_SAVECSV, IDOB_SENDEXCEL, IDOB_EXPORTMODE, IDOB_CLEAR_ALL, 
	IDOB_VARSEL, IDOB_GRID };


BEGIN_EVENT_TABLE( TabularBrowser, wxPanel )
	EVT_BUTTON( IDOB_COPYCLIPBOARD,  TabularBrowser::OnCommand )
	EVT_BUTTON( IDOB_SAVECSV,  TabularBrowser::OnCommand )
	EVT_BUTTON( IDOB_SENDEXCEL, TabularBrowser::OnCommand )
	EVT_BUTTON( IDOB_CLEAR_ALL, TabularBrowser::OnCommand )
	EVT_DVSELECTIONLIST(IDOB_VARSEL, TabularBrowser::OnVarSel)
END_EVENT_TABLE()

TabularBrowser::TabularBrowser( wxWindow *parent )
	: wxPanel(parent )
{
	m_gridTable = NULL;


	wxBoxSizer *tb_sizer = new wxBoxSizer(wxHORIZONTAL);	
	tb_sizer->Add( new wxButton( this, IDOB_CLEAR_ALL, "Clear all selections "), 0, wxEXPAND|wxALL, 2);
	tb_sizer->Add( new wxButton( this, IDOB_COPYCLIPBOARD, "Copy to clipboard"), 0, wxEXPAND|wxALL, 2);
	tb_sizer->Add( new wxButton( this, IDOB_SAVECSV, "Save as CSV..."), 0, wxEXPAND|wxALL, 2);
#ifdef __WXMSW__
	tb_sizer->Add( new wxButton( this, IDOB_SENDEXCEL, "Send to Excel"), 0, wxEXPAND|wxALL, 2);
#endif
	tb_sizer->AddStretchSpacer(1);

	wxSplitterWindow *splitwin = new wxSplitterWindow(this, wxID_ANY, 
		wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE ); 
	splitwin->SetMinimumPaneSize(210);


	m_varSel = new wxDVSelectionListCtrl(splitwin, IDOB_VARSEL, 1, wxDefaultPosition, wxDefaultSize,
		wxDVSEL_NO_COLOURS );
	m_varSel->SetBackgroundColour( *wxWHITE );


	m_grid = new wxExtGridCtrl(splitwin, IDOB_GRID);
	m_grid->EnableEditing(false);
	m_grid->DisableDragCell();
	m_grid->DisableDragRowSize();
	m_grid->DisableDragColMove();
	m_grid->DisableDragGridSize();
	m_grid->SetDefaultCellAlignment( wxALIGN_RIGHT, wxALIGN_CENTER );
	m_grid->SetRowLabelAlignment( wxALIGN_LEFT, wxALIGN_CENTER );

	splitwin->SetMinimumPaneSize( 170 );
	splitwin->SplitVertically(m_varSel, m_grid, 210);


	wxBoxSizer *szv_main = new wxBoxSizer(wxVERTICAL);
	szv_main->Add( tb_sizer, 0, wxALL|wxEXPAND, 1 );
	//szv_main->Add( new wxStaticLine( this ), 0, wxALL|wxEXPAND);
	szv_main->Add( splitwin, 1, wxALL|wxEXPAND, 1 );

	SetSizer( szv_main );

}

void TabularBrowser::UpdateGrid()
{
	if ( !m_sim )
	{
		m_grid->ResizeGrid(1,1);
		m_grid->SetCellValue(0,0,"No data");
		return;
	}

	m_grid->Freeze();

	if (m_gridTable) m_gridTable->ReleaseData();
	
	m_gridTable = new ResultsTable( );
	m_gridTable->LoadData( m_sim, m_selectedVars );
	m_gridTable->SetAttrProvider( new wxExtGridCellAttrProvider );
	m_grid->SetTable(m_gridTable, true);

	if ( m_gridTable->MinCount == m_gridTable->MaxCount
		&& m_gridTable->MaxCount == 8760 )	
		m_grid->SetRowLabelSize( 110 );
	else
		m_grid->SetRowLabelSize( 55 );

	wxClientDC cdc(this);
	wxFont f = *wxNORMAL_FONT;
	f.SetWeight( wxFONTWEIGHT_BOLD );
	cdc.SetFont( f );

	for (int i=0;i<m_gridTable->Table.size();i++)
	{
		wxArrayString lines = wxSplit(m_gridTable->Table[i]->Label, '\n');
		int w = 40;
		for( size_t k=0;k<lines.size();k++ )
		{
			int cw = cdc.GetTextExtent( lines[k] ).x;
			if ( cw > w )
				w = cw;
		}

		m_grid->SetColSize(i, w+6);
	}

	m_grid->SetColLabelSize( wxGRID_AUTOSIZE );
	m_grid->Thaw();


	m_grid->Layout();
	m_grid->GetParent()->Layout();
	m_grid->ForceRefresh();

	
}

void TabularBrowser::Setup( Simulation *data )
{
	m_sim = data;
	
	UpdateAll();
	UpdateGrid();
}

void TabularBrowser::UpdateAll()
{
	m_names.Clear();

	int vsx, vsy;
	m_varSel->GetViewStart( &vsx, &vsy );
	m_varSel->RemoveAll();

	if ( !m_sim ) return;

	m_varSel->Freeze();
	PopulateSelectionList( m_varSel, &m_names, m_sim );

	size_t i=0;
	while (i<m_selectedVars.Count())
	{
		int idx = m_names.Index( m_selectedVars[i] );
		if (idx < 0)
			m_selectedVars.RemoveAt(i);
		else
		{
			m_varSel->SelectRowInCol( idx );
			i++;
		}
	}

	m_varSel->ExpandSelections();
	m_varSel->Scroll( vsx, vsy );
	m_varSel->Thaw();

	UpdateGrid();
}


void TabularBrowser::OnCommand(wxCommandEvent &evt)
{
	switch(evt.GetId())
	{
	case IDOB_CLEAR_ALL:
		{
			m_selectedVars.Clear();
			UpdateAll();
		}
		break;
	case IDOB_COPYCLIPBOARD:
	case IDOB_SENDEXCEL:
		{
			wxBusyInfo busy("Processing data table... please wait");
			wxString dat;
			GetTextData(dat, '\t');
			
			// strip commas per request from Paul 5/23/12 meeting
			dat.Replace(",","");

#ifdef __WXMSW__
			if (evt.GetId() == IDOB_SENDEXCEL)
			{
							
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
					wxTheClipboard->SetData( new wxTextDataObject(dat) );
					wxTheClipboard->Close();
					xl.PasteClipboard();
				}
			}
#endif
			if (evt.GetId() == IDOB_COPYCLIPBOARD)
			{
				if (wxTheClipboard->Open())
				{
					wxTheClipboard->SetData( new wxTextDataObject(dat) );
					wxTheClipboard->Close();
				}
			}
		}
		break;
	case IDOB_SAVECSV:
		{
			wxFileDialog fdlg(this, "Save results as CSV", wxEmptyString, "results.csv", "Comma-separated values (*.csv)|*.csv", wxFD_SAVE|wxFD_OVERWRITE_PROMPT);
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
			fputs( dat.c_str(), fp );
			fclose(fp);
		}
		break;
	}
}

void TabularBrowser::OnVarSel( wxCommandEvent & )
{	
	int row, col;
	bool checked;
	m_varSel->GetLastEventInfo( &row, &col, &checked );

	
	if ( row >= 0 && row < (int)m_names.size() )
	{
		wxString name = m_names[ row ];

		if ( checked && m_selectedVars.Index( name ) == wxNOT_FOUND)
			m_selectedVars.Add( name );
		
		if (!checked && m_selectedVars.Index( name ) != wxNOT_FOUND)
			m_selectedVars.Remove( name );

		UpdateGrid();
	}
}

void TabularBrowser::GetTextData(wxString &dat, char sep)
{
	dat = wxEmptyString;
	if (!m_gridTable)
		return;

	size_t approxbytes = m_gridTable->MaxCount * 15 * m_gridTable->Table.size();
	dat.Alloc(approxbytes);

	size_t c;

	for (c=0;c<m_gridTable->Table.size();c++)
	{
		wxString label = m_gridTable->Table[c]->Label;
		label.Replace( '\n', " | " );

		if (sep == ',')
			dat += '"' + label + '"';
		else
			dat += label;

		if (c < m_gridTable->Table.size()-1)
			dat += sep;
		else
			dat += '\n';
	}

	for (size_t r=0;r<m_gridTable->MaxCount;r++)
	{
		for (c=0;c<m_gridTable->Table.size();c++)
		{
			if (r < m_gridTable->Table[c]->N)
				dat += wxString::Format("%g", m_gridTable->Table[c]->Values[r]);
			
			if (c < m_gridTable->Table.size()-1)
				dat += sep;
			else
				dat += '\n';
		}
	}
}
