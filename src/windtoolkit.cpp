//#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/combobox.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
//#include <limits>
//
#include "windtoolkit.h"
#include "main.h"

static const char *help_text =
"NREL WIND Toolkit data is only available for locations in the continental United States. Each weather file contains wind resource data for a single year.\n\n"
"See Help for details.";

enum {
	ID_radAddress, ID_radLatLon, ID_cboYears,
	ID_txtAddress, ID_txtLat, ID_txtLon
};

BEGIN_EVENT_TABLE( WindToolkitDialog, wxDialog )
	EVT_RADIOBUTTON( ID_radAddress, WindToolkitDialog::OnEvt )
	EVT_RADIOBUTTON( ID_radLatLon, WindToolkitDialog::OnEvt )
	EVT_BUTTON( wxID_HELP, WindToolkitDialog::OnEvt )
END_EVENT_TABLE()

WindToolkitDialog::WindToolkitDialog(wxWindow *parent, const wxString &title)
	 : wxDialog( parent, wxID_ANY, title,  wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{
	radAddress = new wxRadioButton( this, ID_radAddress, "Enter street address or zip code:" );
	radLatLon = new wxRadioButton( this, ID_radLatLon, "Enter location coordinates (deg):" );
	txtAddress = new wxTextCtrl(this, ID_txtAddress, "Denver, CO");

	txtLat = new wxTextCtrl(this, ID_txtLat, "40", wxDefaultPosition, wxDefaultSize, 0, ::wxTextValidator(wxFILTER_NUMERIC) );
	txtLon = new wxTextCtrl(this, ID_txtLon, "-116", wxDefaultPosition, wxDefaultSize, 0, ::wxTextValidator(wxFILTER_NUMERIC) );

	wxArrayString years;
	years.Add("2007");
	years.Add("2008");
	years.Add("2009");
	years.Add("2010");
	years.Add("2011");
	years.Add("2012");

	wxString InitialValue = "2012";
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
	szyr->Add( new wxStaticText( this, wxID_ANY, "Select year"), wxALL|wxALIGN_CENTER_VERTICAL, 15 );
	szyr->Add( cboYears, 0, wxALL, 5 );

	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add( szgrid, 0, wxLEFT|wxRIGHT|wxTOP, 10 );
	szmain->Add( szyr, 0, wxLEFT|wxRIGHT, 10 );

	wxStaticText *note = new wxStaticText(this, wxID_ANY, help_text);
	note->Wrap(550);
	szmain->Add(note, 0, wxLEFT | wxRIGHT | wxALIGN_CENTER, 10);

	szmain->Add( CreateButtonSizer( wxHELP|wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );

	SetSizer( szmain );
	Fit();

	radAddress->SetValue( true );
	txtAddress->SetFocus();
	txtAddress->SelectAll();
	txtLat->Enable( false );
	txtLon->Enable( false );
}

void WindToolkitDialog::OnEvt( wxCommandEvent &e )
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

bool WindToolkitDialog::IsAddressMode()
{
	return radAddress->GetValue();
}

wxString WindToolkitDialog::GetAddress()
{
	return txtAddress->GetValue();
}

double WindToolkitDialog::GetLatitude()
{
	double num = std::numeric_limits<double>::quiet_NaN();
	txtLat->GetValue().ToDouble(&num);
	return num;
}

double WindToolkitDialog::GetLongitude()
{
	double num = std::numeric_limits<double>::quiet_NaN();
	txtLon->GetValue().ToDouble(&num);
	return num;
}

wxString WindToolkitDialog::GetYear()
{
	return cboYears->GetStringSelection();
}
