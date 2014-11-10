#include <wx/panel.h>
#include <wx/choice.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/statline.h>
#include <wx/scrolwin.h>
#include <wx/dialog.h>
#include <wx/checkbox.h>

#include <wex/numeric.h>
#include <wex/utils.h>

#include "main.h"
#include "widgets.h"
#include "adjfac.h"
#include "variables.h"

/* hour: 0 = jan 1st 12am-1am, returns 1-12 */

enum { ID_MONTH_SEL = wxID_HIGHEST+495, ID_DAY_SEL, ID_HOUR_SEL, ID_TIME };

class HourOfYearPickerCtrl : public wxPanel
{
	wxChoice *m_month;
	wxChoice *m_day;
	wxChoice *m_hour;
	//wxTextCtrl *m_text;
public:
	HourOfYearPickerCtrl( wxWindow *win, int id, const wxPoint &pos = wxDefaultPosition, const wxSize &size = wxDefaultSize )
		: wxPanel( win, id )
	{
		wxString months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
		m_month = new wxChoice( this, ID_MONTH_SEL, wxDefaultPosition, wxDefaultSize, 12, months );
		m_month->SetSelection( 0 );
		m_day = new wxChoice( this, ID_DAY_SEL );
		UpdateDay();
		m_hour = new wxChoice( this, ID_HOUR_SEL );
		m_hour->Append( "12 am" );
		for( int i=1;i<=11;i++ ) m_hour->Append( wxString::Format("%d am", i) );
		m_hour->Append( "12 pm" );
		for( int i=1;i<=11;i++ ) m_hour->Append( wxString::Format("%d pm", i) );
		m_hour->SetSelection(12);

		//m_text = new wxTextCtrl( this, ID_TIME, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );

		wxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
		sizer->Add( m_month, 1, wxALL|wxEXPAND, 2 );
		sizer->Add( m_day, 0, wxALL|wxEXPAND, 2 );
		sizer->Add( m_hour, 0, wxALL|wxEXPAND, 2 );
		//sizer->Add( m_text, 0, wxALL|wxEXPAND, 2 );
		SetSizer( sizer );
	}

	void UpdateDay()
	{
		int mo = m_month->GetSelection();
		int dy = m_day->GetSelection();
		m_day->Clear();
		for( int i=1;i<=wxNDay[mo];i++ ) m_day->Append( wxString::Format("%d", i ) );
		if ( dy < 0 ) dy = 0;
		if ( dy >= (int)m_day->GetCount() ) dy = m_day->GetCount()-1;
		m_day->SetSelection( dy );
	}
	
	void OnCommand( wxCommandEvent &evt )
	{
		if ( evt.GetId() == ID_MONTH_SEL )
			UpdateDay();

		/*
		if ( evt.GetId() == ID_TIME ) 
		{
			SetTime( wxAtoi( m_text->GetValue() ) );
			m_text->SelectAll();
		}
		else m_text->ChangeValue( wxString::Format("%d", GetTime()) );
		*/
	}

	void SetTime( int time )
	{
		int mo, dy, hr;
		wxTimeToMDHM( time, &mo, &dy, &hr );

		m_month->SetSelection( mo-1 );
		UpdateDay();
		m_day->SetSelection( dy-1 );
		m_hour->SetSelection( hr );
	}

	int GetTime( )
	{
		return wxMDHMToTime( m_month->GetSelection()+1, m_day->GetSelection()+1, m_hour->GetSelection() );
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( HourOfYearPickerCtrl, wxPanel )
	EVT_CHOICE( ID_MONTH_SEL, HourOfYearPickerCtrl::OnCommand )
	EVT_CHOICE( ID_DAY_SEL, HourOfYearPickerCtrl::OnCommand )
	EVT_CHOICE( ID_HOUR_SEL, HourOfYearPickerCtrl::OnCommand )
END_EVENT_TABLE()
// EVT_TEXT_ENTER( ID_TIME, HourOfYearPickerCtrl::OnCommand )

enum{ ID_ADD_PERIOD = wxID_HIGHEST+452, 
	ID_DELETE_PERIOD };

class PeriodFactorCtrl : public wxPanel
{
	struct period { 
		wxPanel *panel;
		HourOfYearPickerCtrl *start, *end;
		wxNumericCtrl *factor;
		wxButton *delbtn;
	};

	std::vector<period> m_periods;

	wxBoxSizer *m_sizer;
public:
	PeriodFactorCtrl( wxWindow *parent ) : wxPanel( parent, wxID_ANY )
	{
		m_sizer = new wxBoxSizer( wxVERTICAL );
		m_sizer->Add( new wxButton( this, ID_ADD_PERIOD, "Add period..."), 0, wxALL, 3 );

		SetSizer( m_sizer );
	}

	period &CreatePeriod()
	{
		period pp;
		
		pp.panel = new wxPanel( this );
		pp.start = new HourOfYearPickerCtrl( pp.panel , wxID_ANY );
		pp.end = new HourOfYearPickerCtrl( pp.panel, wxID_ANY );
		pp.factor = new wxNumericCtrl( pp.panel, wxID_ANY );
		pp.delbtn = new wxButton( pp.panel, ID_DELETE_PERIOD, "Delete" );

		wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
		sizer->Add( new wxStaticText( pp.panel, wxID_ANY, "  Start time:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
		sizer->Add( pp.start, 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
		sizer->Add( new wxStaticText( pp.panel, wxID_ANY, "  End time:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
		sizer->Add( pp.end, 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
		sizer->Add( new wxStaticText( pp.panel, wxID_ANY, "  Adjustment factor:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
		sizer->Add( pp.factor, 0, wxALL|wxALIGN_CENTER_VERTICAL, 0 );
		sizer->AddStretchSpacer();
		sizer->Add( pp.delbtn, 0, wxALL|wxALIGN_CENTER_VERTICAL, 4 );
		pp.panel->SetSizer( sizer );

		m_periods.push_back( pp );

		m_sizer->Add( pp.panel, 0, wxALL|wxEXPAND, 2 );
		Fit();
		
		UpdateParentLayout();

		return m_periods[m_periods.size()-1];
	}

	void Clear()
	{
		for( size_t i=0;i<m_periods.size();i++ )
			m_periods[i].panel->Destroy();

		Fit();
		UpdateParentLayout();
	}

	void UpdateParentLayout()
	{
		if ( !IsShown() ) return;

		if ( wxScrolledWindow *sw = dynamic_cast<wxScrolledWindow*>( GetParent() ) )
		{
			sw->FitInside();
			sw->Layout();
		}
	}

	void OnCommand( wxCommandEvent &evt )
	{
		if ( evt.GetId() == ID_ADD_PERIOD )
			CreatePeriod();
		else if ( evt.GetId() == ID_DELETE_PERIOD )
		{
			for( size_t i=0;i<m_periods.size();i++ )
			{
				if ( m_periods[i].delbtn == evt.GetEventObject() )
				{
					m_periods[i].panel->Destroy();
					m_periods.erase( m_periods.begin() + i );
					UpdateParentLayout();
					break;
				}
			}
		}
	}

	void Set( const matrix_t<float> &data )
	{
		Clear();
		if ( data.ncols() != 3 ) return;
		for( size_t r=0;r<data.nrows();r++ )
		{
			period &x = CreatePeriod();
			x.start->SetTime( data(r,0) );
			x.end->SetTime( data(r,1) );
			x.factor->SetValue( data(r,2) );
		}
	}

	void Get( matrix_t<float> &data )
	{
		if ( m_periods.size() == 0 )
		{
			data.clear();
			return;
		}

		data.resize_fill( m_periods.size(), 3, 1.0f );
		for( size_t i=0;i<m_periods.size();i++ )
		{
			data(i,0) = m_periods[i].start->GetTime();
			data(i,1) = m_periods[i].end->GetTime();
			data(i,2) = (float)m_periods[i].factor->Value();
		}


	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( PeriodFactorCtrl, wxPanel )
	EVT_BUTTON( ID_ADD_PERIOD, PeriodFactorCtrl::OnCommand )
	EVT_BUTTON( ID_DELETE_PERIOD, PeriodFactorCtrl::OnCommand )
END_EVENT_TABLE( )


enum { ID_ENABLE_HOURLY = ::wxID_HIGHEST+999 , ID_ENABLE_PERIODS };

class HourlyFactorDialog : public wxDialog
{
	wxScrolledWindow *m_scrollWin;
	wxNumericCtrl *m_factor;

	wxCheckBox *m_enableHourly;
	AFDataArrayButton *m_hourly;

	wxCheckBox *m_enablePeriods;
	PeriodFactorCtrl *m_periods; 
public:
	HourlyFactorDialog( wxWindow *parent )
		: wxDialog( parent, wxID_ANY, "Edit Hourly Factors", wxDefaultPosition, wxSize(850,450), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		SetEscapeId( wxID_CANCEL );

		m_scrollWin = new wxScrolledWindow( this, wxID_ANY );
		m_scrollWin->SetBackgroundColour( *wxWHITE );
		m_scrollWin->SetScrollRate( 50, 50 );
		
		m_factor = new wxNumericCtrl( m_scrollWin, wxID_ANY );
		
		m_enableHourly = new wxCheckBox( m_scrollWin, ID_ENABLE_HOURLY, "Enable hourly factors" );
		m_hourly = new AFDataArrayButton( m_scrollWin, wxID_ANY );
		m_hourly->SetMode( DATA_ARRAY_8760_ONLY );

		m_enablePeriods = new wxCheckBox( m_scrollWin, ID_ENABLE_PERIODS, "Enable hourly factors with custom periods" );
		m_periods = new PeriodFactorCtrl( m_scrollWin );

		wxSizer *scroll = new wxBoxSizer( wxVERTICAL );
		
		scroll->Add( new wxStaticText( m_scrollWin, wxID_ANY, "Annual factor (0..1)"), 0, wxALL|wxEXPAND, 5 );
		scroll->Add( m_factor, 0, wxALL, 5 );
		scroll->Add( new wxStaticLine( m_scrollWin ), 0, wxALL|wxEXPAND );
		
		scroll->Add( m_enableHourly, 0, wxALL|wxEXPAND, 5 );
		scroll->Add( m_hourly, 0, wxALL, 5 );
		scroll->Add( new wxStaticLine( m_scrollWin ), 0, wxALL|wxEXPAND );
				
		scroll->Add( m_enablePeriods, 0, wxALL|wxEXPAND, 5 );
		scroll->Add( m_periods, 0, wxALL, 5 );
		scroll->Add( new wxStaticLine( m_scrollWin ), 0, wxALL|wxEXPAND );
		
		m_scrollWin->SetSizer( scroll );

		wxSizer *box = new wxBoxSizer(wxVERTICAL);
		box->Add( m_scrollWin, 1, wxALL|wxEXPAND );
		box->Add( CreateButtonSizer( wxOK|wxCANCEL|wxHELP ), 0, wxALL|wxEXPAND, 5 );
		SetSizer( box );

		UpdateVisibility();
	}

	
	void UpdateVisibility()
	{
		m_hourly->Show( m_enableHourly->IsChecked() );
		m_periods->Show( m_enablePeriods->IsChecked() );

		m_scrollWin->Layout();
		m_scrollWin->FitInside();
		m_scrollWin->Refresh();
	}

	void Set( const AFHourlyFactorCtrl::FactorData &data )
	{
		m_factor->SetValue( data.factor );
		m_enableHourly->SetValue( data.en_hourly );
		m_hourly->Set( data.hourly );
		m_enablePeriods->SetValue( data.en_periods );
		m_periods->Set( data.periods );
		UpdateVisibility();
	}

	void Get( AFHourlyFactorCtrl::FactorData &data )
	{
		data.factor = (float)m_factor->Value();
		data.en_hourly = m_enableHourly->GetValue();
		data.hourly = m_hourly->Get();
		data.en_periods = m_enablePeriods->GetValue();
		m_periods->Get( data.periods );
	}
	
	void OnCommand( wxCommandEvent &e )
	{
		switch( e.GetId() )
		{
		case wxID_HELP:
			SamApp::ShowHelp("hourly_factors");
			break;
		case ID_ENABLE_HOURLY:
		case ID_ENABLE_PERIODS:
			UpdateVisibility();
			break;
		}
	}
	
	
	void OnClose( wxCloseEvent & )
	{
		EndModal( wxID_CANCEL );
	}

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE( HourlyFactorDialog, wxDialog )
	EVT_CLOSE( HourlyFactorDialog::OnClose )
	EVT_CHECKBOX( ID_ENABLE_HOURLY, HourlyFactorDialog::OnCommand )
	EVT_CHECKBOX( ID_ENABLE_PERIODS, HourlyFactorDialog::OnCommand )
	EVT_BUTTON( wxID_HELP, HourlyFactorDialog::OnCommand )
END_EVENT_TABLE()



AFHourlyFactorCtrl::AFHourlyFactorCtrl( wxWindow *parent, int id,
	const wxPoint &pos, const wxSize &size)
	: wxPanel( parent, id, pos, size )
{
	m_button = new wxButton( this, wxID_ANY, "Edit factors...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT );
	m_label = new wxStaticText( this, wxID_ANY, wxEmptyString );
	m_label->SetForegroundColour( wxColour(29,80,173) );

	wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
	sizer->Add( m_button, 0, wxALL|wxALIGN_LEFT|wxALIGN_TOP, 0 );
	sizer->Add( m_label, 1, wxALL|wxALIGN_LEFT|wxALIGN_TOP, 4 );
	
	SetSizer( sizer );

	m_data.factor = 1.0f;
	m_data.en_hourly = false;
	m_data.hourly.resize( 8760, 1.0f );
	m_data.en_periods = false;
	m_data.periods.resize_fill( 1, 3, 1.0f );
	UpdateText();
}

void AFHourlyFactorCtrl::UpdateText()
{
	wxString txt( wxString::Format( "Annual factor: %.2f", m_data.factor ) );
	
	float avg = 0;
	for( size_t i=0;i<m_data.hourly.size();i++ ) avg += m_data.hourly[i];
	if ( m_data.hourly.size() > 0 ) avg /= m_data.hourly.size();
	else avg = 0;

	txt += wxString("\n") + (m_data.en_hourly ? wxString::Format( "Hourly factors: Avg = %.2f", avg ) : "Hourly factors: None");
	txt += wxString("\n") + (m_data.en_periods ? wxString::Format("Custom periods: %d", (int)m_data.periods.nrows()) : "Custom periods: None");
	m_label->SetLabel( txt );
}

void AFHourlyFactorCtrl::Write( VarValue *vv )
{
	vv->SetType( VV_TABLE );
	VarTable &tab = vv->Table();

	tab.Set( "factor", VarValue( m_data.factor ) );
	tab.Set( "en_hourly", VarValue( (bool)m_data.en_hourly ) );
	tab.Set( "hourly", VarValue( m_data.hourly ) );
	tab.Set( "en_periods", VarValue( (bool)m_data.en_periods ) );
	tab.Set( "periods", VarValue( m_data.periods ) );
}

bool AFHourlyFactorCtrl::Read( VarValue *root )
{
	if ( root->Type() == VV_TABLE )
	{
		VarTable &tab = root->Table();
		if ( VarValue *vv = tab.Get("factor") ) m_data.factor = vv->Value();
		if ( VarValue *vv = tab.Get("en_hourly") ) m_data.en_hourly = vv->Boolean();
		if ( VarValue *vv = tab.Get("hourly") ) m_data.hourly = vv->Array();
		if ( VarValue *vv = tab.Get("en_periods") ) m_data.en_periods = vv->Boolean();
		if ( VarValue *vv = tab.Get("periods") ) m_data.periods = vv->Matrix();
		UpdateText();
		return true;
	}
	else
		return false;
}

bool AFHourlyFactorCtrl::DoEdit()
{
	HourlyFactorDialog dlg( this );
	dlg.Set( m_data );
	if ( dlg.ShowModal() == wxID_OK )
	{
		dlg.Get( m_data );
		UpdateText();
		return true;
	}

	return false;
}

void AFHourlyFactorCtrl::OnPressed( wxCommandEvent &evt )
{
	if ( evt.GetEventObject() == m_button )
		if ( DoEdit() )
		{
			evt.SetEventObject(this); // handles updating case from ui in OnNativeEvent in inputpage.cpp
			evt.Skip();  // allow event to propagate indicating underlying value changed
		}
}

BEGIN_EVENT_TABLE( AFHourlyFactorCtrl, wxPanel )
	EVT_BUTTON( wxID_ANY, AFHourlyFactorCtrl::OnPressed )
END_EVENT_TABLE()

