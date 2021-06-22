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
#include "wectoolkit.h"
#include "main.h"

static const char *help_text =
"Enter the details for your Wave Energy Converter (WEC) imported power matrix below to save the matrix in your SAM library";

enum {
	ID_txtName, ID_txtTech, ID_txtType,
	ID_txtDiam, ID_txtMass, ID_txtFound, ID_txtMoor, ID_txtStrMat, ID_txtPower
};

BEGIN_EVENT_TABLE( WECToolkitDialog, wxDialog )
	EVT_BUTTON( wxID_HELP, WECToolkitDialog::OnEvt )
END_EVENT_TABLE()

WECToolkitDialog::WECToolkitDialog(wxWindow *parent, const wxString &title)
	 : wxDialog( parent, wxID_ANY, title,  wxDefaultPosition, wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{

	txtName = new wxTextCtrl(this, ID_txtName, "RM3", wxDefaultPosition, wxDefaultSize, 0);
	txtTech = new wxTextCtrl(this, ID_txtTech, "Heaving Buoy", wxDefaultPosition, wxDefaultSize, 0);
    txtType = new wxTextCtrl(this, ID_txtType, "Hydraulic Drivetrain", wxDefaultPosition, wxDefaultSize, 0);
    txtDiam = new wxTextCtrl(this, ID_txtType, "20", wxDefaultPosition, wxDefaultSize, 0, ::wxTextValidator(wxFILTER_NUMERIC));
    txtMass = new wxTextCtrl(this, ID_txtType, "687", wxDefaultPosition, wxDefaultSize, 0, ::wxTextValidator(wxFILTER_NUMERIC));
    txtFound = new wxTextCtrl(this, ID_txtType, "Floating", wxDefaultPosition, wxDefaultSize, 0);
    txtMoor = new wxTextCtrl(this, ID_txtType, "Slack Mooring", wxDefaultPosition, wxDefaultSize, 0);
    txtStrMat = new wxTextCtrl(this, ID_txtStrMat, "A36 Steel", wxDefaultPosition, wxDefaultSize, 0);

	


	wxBoxSizer *szname = new wxBoxSizer( wxHORIZONTAL );
	szname->Add( new wxStaticText( this, wxID_ANY, "Device Name"), wxALL|wxALIGN_CENTER_VERTICAL, 15 );
	szname->Add( txtName, 0, wxALL, 5 );

    wxBoxSizer* sztech = new wxBoxSizer(wxHORIZONTAL);
    sztech->Add(new wxStaticText(this, wxID_ANY, "Technology"), wxALL | wxALIGN_CENTER_VERTICAL, 15);
    sztech->Add(txtTech, 0, wxALL, 5);

    wxBoxSizer* sztype = new wxBoxSizer(wxHORIZONTAL);
    sztype->Add(new wxStaticText(this, wxID_ANY, "PTO Type"), wxALL | wxALIGN_CENTER_VERTICAL, 15);
    sztype->Add(txtType, 0, wxALL, 5);

    wxBoxSizer* szdiam = new wxBoxSizer(wxHORIZONTAL);
    szdiam->Add(new wxStaticText(this, wxID_ANY, "Characteristic Diamteter (m)"), wxALL | wxALIGN_CENTER_VERTICAL, 15);
    szdiam->Add(txtDiam, 0, wxALL, 5);

    wxBoxSizer* szmass = new wxBoxSizer(wxHORIZONTAL);
    szmass->Add(new wxStaticText(this, wxID_ANY, "Unballasted Structural Mass (Tonnes)"), wxALL | wxALIGN_CENTER_VERTICAL, 15);
    szmass->Add(txtMass, 0, wxALL, 5);

    wxBoxSizer* szfound = new wxBoxSizer(wxHORIZONTAL);
    szfound->Add(new wxStaticText(this, wxID_ANY, "Foundation Type"), wxALL | wxALIGN_CENTER_VERTICAL, 15);
    szfound->Add(txtFound, 0, wxALL, 5);

    wxBoxSizer* szmoor = new wxBoxSizer(wxHORIZONTAL);
    szmoor->Add(new wxStaticText(this, wxID_ANY, "Mooring Type"), wxALL | wxALIGN_CENTER_VERTICAL, 15);
    szmoor->Add(txtMoor, 0, wxALL, 5);

    wxBoxSizer* szstrmat = new wxBoxSizer(wxHORIZONTAL);
    szstrmat->Add(new wxStaticText(this, wxID_ANY, "Primary Structural Material"), wxALL | wxALIGN_CENTER_VERTICAL, 15);
    szstrmat->Add(txtStrMat, 0, wxALL, 5);


	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add( szname, 0, wxLEFT|wxRIGHT|wxTOP, 10 );
	szmain->Add(sztech, 0, wxLEFT | wxRIGHT, 10);
	szmain->Add(sztype, 0, wxLEFT | wxRIGHT, 10);
    szmain->Add(szdiam, 0, wxLEFT | wxRIGHT, 10);
    szmain->Add(szmass, 0, wxLEFT | wxRIGHT, 10);
    szmain->Add(szfound, 0, wxLEFT | wxRIGHT, 10);
    szmain->Add(szmoor, 0, wxLEFT | wxRIGHT, 10);
    szmain->Add(szstrmat, 0, wxLEFT | wxRIGHT, 10);


	wxStaticText *note = new wxStaticText(this, wxID_ANY, help_text);
	note->Wrap(550);
	szmain->Add(note, 0, wxLEFT | wxRIGHT | wxALIGN_CENTER, 10);

	szmain->Add( CreateButtonSizer( wxHELP|wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );

	SetSizer( szmain );
	Fit();

}

void WECToolkitDialog::OnEvt( wxCommandEvent &e )
{
//extern void helpcontext( const wxString & ); // defined in sammdi.h

	switch( e.GetId() )
	{
	case wxID_HELP:
		SamApp::ShowHelp("wind_toolkit_download");
		break;
	}
}

wxString WECToolkitDialog::GetName()
{
	return txtName->GetValue();
	
}

wxString WECToolkitDialog::GetTech()
{
    return txtTech->GetValue();

}

wxString WECToolkitDialog::GetType()
{
    return txtType->GetValue();

}

double WECToolkitDialog::GetDiam()
{
    double num = std::numeric_limits<double>::quiet_NaN();
    txtDiam->GetValue().ToDouble(&num);
    return num;

}

double WECToolkitDialog::GetMass()
{
    double num = std::numeric_limits<double>::quiet_NaN();
    txtMass->GetValue().ToDouble(&num);
    return num;

}

wxString WECToolkitDialog::GetFound()
{
    return txtFound->GetValue();

}

wxString WECToolkitDialog::GetMoor()
{
    return txtMoor->GetValue();

}

wxString WECToolkitDialog::GetStrMat()
{
    return txtStrMat->GetValue();

}

