/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

//#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/combobox.h>
#include <wx/listbox.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
//#include <limits>
//
#include "windtoolkit.h"
#include "main.h"

static const char *help_text = "Notes:\n1. Use your mouse to choose one or more measurement heights.\n2. Each weather file contains wind resource data for a single year.\n3. NREL WIND Toolkit data is only available for locations in the continental United States.\n\nSee Help for details.";

enum {
	ID_radAddress, ID_radLatLon, ID_cboYears,
	ID_txtAddress, ID_txtLat, ID_txtLon, ID_lsthubheights
};

BEGIN_EVENT_TABLE( WindToolkitDialog, wxDialog )
	EVT_RADIOBUTTON( ID_radAddress, WindToolkitDialog::OnEvt )
	EVT_RADIOBUTTON( ID_radLatLon, WindToolkitDialog::OnEvt )
	EVT_BUTTON( wxID_HELP, WindToolkitDialog::OnEvt )
END_EVENT_TABLE()

WindToolkitDialog::WindToolkitDialog(wxWindow *parent, const wxString &title)
	 : wxDialog( parent, wxID_ANY, title,  wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{
	radAddress = new wxRadioButton( this, ID_radAddress, "Street address or zip code " );
	radLatLon = new wxRadioButton( this, ID_radLatLon, "Coordinates (DD):" );
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
	years.Add("2013");

	wxString InitialValue = "2013";
	cboYears = new wxComboBox(this, ID_cboYears, InitialValue, wxDefaultPosition, wxDefaultSize, years, wxCB_READONLY);

	wxArrayString hubheights;
	hubheights.Add("10");
	hubheights.Add("40");
	hubheights.Add("60");
	hubheights.Add("80");
	hubheights.Add("100");
	hubheights.Add("120");
	hubheights.Add("140");
	hubheights.Add("160");
	hubheights.Add("200");

	lstHubheights = new wxListBox(this, ID_lsthubheights, wxDefaultPosition, wxDefaultSize, hubheights, wxLB_MULTIPLE);

	wxBoxSizer *szll = new wxBoxSizer( wxHORIZONTAL );
	szll->Add( new wxStaticText( this, wxID_ANY, "Latitude " ), 0, wxLEFT | wxTOP | wxBOTTOM |wxALIGN_CENTER_VERTICAL, 5 );
	szll->Add( txtLat, 0, wxALL, 5 );
	szll->Add( new wxStaticText( this, wxID_ANY, "Longitude " ), 0, wxLEFT | wxTOP | wxBOTTOM |wxALIGN_CENTER_VERTICAL, 5 );
	szll->Add( txtLon, 0, wxALL, 5 );
	
	wxFlexGridSizer *szgrid = new wxFlexGridSizer( 2 );
	szgrid->AddGrowableCol( 1 );
	szgrid->Add( radAddress, 0, wxLEFT | wxTOP | wxBOTTOM |wxALIGN_CENTER_VERTICAL, 1 );
	szgrid->Add( txtAddress, 0, wxRIGHT | wxTOP | wxBOTTOM |wxEXPAND|wxALIGN_CENTER_VERTICAL, 1 );
	szgrid->Add( radLatLon, 0, wxLEFT | wxTOP | wxBOTTOM |wxALIGN_CENTER_VERTICAL, 1 );
	szgrid->Add( szll, 0, wxRIGHT | wxTOP | wxBOTTOM |wxEXPAND|wxALIGN_CENTER_VERTICAL, 1 );

	wxBoxSizer *szyrmh = new wxBoxSizer( wxHORIZONTAL );
	szyrmh->Add( new wxStaticText( this, wxID_ANY, "Year "), 0, wxLEFT | wxTOP | wxBOTTOM, 5 );
	szyrmh->Add( cboYears, 0, wxRIGHT | wxTOP | wxBOTTOM, 5 );
	szyrmh->Add(new wxStaticText(this, wxID_ANY, ""), 0, wxALL, 5); // spacer
	szyrmh->Add(new wxStaticText(this, wxID_ANY, "Measurement height (m) "), 0, wxLEFT|wxTOP|wxBOTTOM , 5);
	szyrmh->Add(lstHubheights, 0, wxRIGHT | wxTOP | wxBOTTOM, 5);

	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add(szgrid, 0, wxLEFT|wxRIGHT|wxTOP, 10 );
	szmain->Add(szyrmh, 0, wxLEFT | wxRIGHT, 10);

	wxStaticText *note = new wxStaticText(this, wxID_ANY, help_text);
	note->Wrap(550);
	szmain->Add(note, 0, wxLEFT | wxRIGHT | wxALIGN_LEFT, 10);

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
		SamApp::ShowHelp("wind_toolkit_download");
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

wxArrayString WindToolkitDialog::GetHubHeights()
{
	wxArrayString hh;
	for (size_t i = 0; i < lstHubheights->GetCount(); i++)
		if (lstHubheights->IsSelected(i))
			hh.Add(lstHubheights->GetString(i));
	return hh;
}
