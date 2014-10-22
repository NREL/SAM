//#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/combobox.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
//#include <limits>
//
#include "solarprospector.h"
#include "main.h"

static const char *help_text = 
	"TMY is typical meteorological year with typical months chosen using NREL TMY weighting factors.\n"
	"TGY is typical global year data with typical months based only on global horizontal irradiance.\n"
	"TDY is typical DNI year with months based only on direct normal (beam) irradiance.\n\n"
	
	"Year numbers are for single year data files with data for the given year.\n\n"
	
	"See Help for details.";

enum {
	ID_radAddress, ID_radLatLon, ID_cboYears,
	ID_txtAddress, ID_txtLat, ID_txtLon
};

BEGIN_EVENT_TABLE( SolarProspectorDialog, wxDialog )
	EVT_RADIOBUTTON( ID_radAddress, SolarProspectorDialog::OnEvt )
	EVT_RADIOBUTTON( ID_radLatLon, SolarProspectorDialog::OnEvt )
	EVT_BUTTON( wxID_HELP, SolarProspectorDialog::OnEvt )
END_EVENT_TABLE()

SolarProspectorDialog::SolarProspectorDialog(wxWindow *parent, const wxString &title)
	 : wxDialog( parent, wxID_ANY, title,  wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{
	radAddress = new wxRadioButton( this, ID_radAddress, "Enter street address or zip code:" );
	radLatLon = new wxRadioButton( this, ID_radLatLon, "Enter location coordinates (deg):" );
	txtAddress = new wxTextCtrl(this, ID_txtAddress, "Denver, CO");

	txtLat = new wxTextCtrl(this, ID_txtLat, "40", wxDefaultPosition, wxDefaultSize, 0, ::wxTextValidator(wxFILTER_NUMERIC) );
	txtLon = new wxTextCtrl(this, ID_txtLon, "-116", wxDefaultPosition, wxDefaultSize, 0, ::wxTextValidator(wxFILTER_NUMERIC) );

	wxArrayString years;
	years.Add("TMY");
	years.Add("TDY");
	years.Add("TGY");
	years.Add("1998");
	years.Add("1999");
	years.Add("2000");
	years.Add("2001");
	years.Add("2002");
	years.Add("2003");
	years.Add("2004");
	years.Add("2005");
	years.Add("2006");
	years.Add("2007");
	years.Add("2008");
	years.Add("2009");

	wxString InitialValue = "TMY";
	cboYears = new wxComboBox(this, ID_cboYears, InitialValue, wxDefaultPosition, wxDefaultSize, years, wxCB_READONLY);

	wxBoxSizer *szll = new wxBoxSizer( wxHORIZONTAL );
	szll->Add( new wxStaticText( this, wxID_ANY, "Latitude" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	szll->Add( txtLat, 0, wxALL, 5 );
	szll->Add( new wxStaticText( this, wxID_ANY, "Longitude" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 5 );
	szll->Add( txtLon, 0, wxALL, 5 );
	
	wxFlexGridSizer *szgrid = new wxFlexGridSizer( 2 );
	szgrid->AddGrowableCol( 1 );
	szgrid->Add( radAddress, 0, wxALL|wxALIGN_CENTER_VERTICAL, 1 );
	szgrid->Add( txtAddress, 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 1 );
	szgrid->Add( radLatLon, 0, wxALL|wxALIGN_CENTER_VERTICAL, 1 );
	szgrid->Add( szll, 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 1 );

	wxBoxSizer *szyr = new wxBoxSizer( wxHORIZONTAL );
	szyr->Add( new wxStaticText( this, wxID_ANY, "Select data year"), wxALL|wxALIGN_CENTER_VERTICAL, 15 );
	szyr->Add( cboYears, 0, wxALL, 5 );

	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add( szgrid, 0, wxLEFT|wxRIGHT|wxTOP, 10 );
	szmain->Add( szyr, 0, wxLEFT|wxRIGHT, 10 );


	wxStaticText *note = new wxStaticText(this, wxID_ANY, help_text);
	note->Wrap( 550 );
	szmain->Add( note , 0, wxLEFT|wxRIGHT|wxALIGN_CENTER, 10 );


	szmain->Add( CreateButtonSizer( wxHELP|wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );

	SetSizer( szmain );
	Fit();

	radAddress->SetValue( true );
	txtAddress->SetFocus();
	txtAddress->SelectAll();
	txtLat->Enable( false );
	txtLon->Enable( false );
}

void SolarProspectorDialog::OnEvt( wxCommandEvent &e )
{
//extern void helpcontext( const wxString & ); // defined in sammdi.h

	switch( e.GetId() )
	{
	case wxID_HELP:
		SamApp::ShowHelp("download_weather_file");
		break;
	case ID_radAddress:
	case ID_radLatLon:
		{
			bool addr = radAddress->GetValue();
			txtAddress->Enable( addr );
			txtLat->Enable( !addr );
			txtLon->Enable( !addr );
		}
		break;
	}
}

bool SolarProspectorDialog::IsAddressMode()
{
	return radAddress->GetValue();
}

wxString SolarProspectorDialog::GetAddress()
{
	return txtAddress->GetValue();
}

double SolarProspectorDialog::GetLatitude()
{
	double num = std::numeric_limits<double>::quiet_NaN();
	txtLat->GetValue().ToDouble(&num);
	return num;
}

double SolarProspectorDialog::GetLongitude()
{
	double num = std::numeric_limits<double>::quiet_NaN();
	txtLon->GetValue().ToDouble(&num);
	return num;
}

wxString SolarProspectorDialog::GetYear()
{
	return cboYears->GetStringSelection();
}
