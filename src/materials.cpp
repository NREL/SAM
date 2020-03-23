/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <algorithm>

#include <wx/filename.h>
#include <wx/clipbrd.h>

#include <wex/csv.h>
#include <wex/utils.h>

#include "materials.h"

#include "main.h"

DEFINE_EVENT_TYPE( wxEVT_MATPROPCTRL_CHANGE )

BEGIN_EVENT_TABLE(MatPropCtrl, wxButton)
	EVT_BUTTON( wxID_ANY, MatPropCtrl::OnButton)
END_EVENT_TABLE()

MatPropCtrl::MatPropCtrl(wxWindow *parent, int id, const wxPoint &pos, const wxSize &sz)
	: wxButton(parent, id, "Edit...", pos, sz)
{
	m_data.resize_fill(1, MPC_NCOLS, 0.0);
	for (int i=0;i<MPC_NCOLS;i++)
		m_data.at(0,i) = 0;
}

void MatPropCtrl::SetData(const matrix_t<double> &data)
{
	if ( data.ncols() == 7 )
		m_data = data;
}

matrix_t<double> MatPropCtrl::GetData()
{
	return m_data;
}

void MatPropCtrl::OnButton(wxCommandEvent &)
{
	MatPropDialog dlg(this);
	dlg.SetData( m_data );
	if (dlg.ShowModal() == wxID_OK)
	{
		m_data = dlg.GetData();

		// FIRE CHANGED EVENT
		wxCommandEvent change(wxEVT_MATPROPCTRL_CHANGE, GetId() );
		change.SetEventObject( this );
		GetEventHandler()->ProcessEvent(change);
	}
}




/********* Dialog **********/

static const char *mat_col_labels[MPC_NCOLS] =
{ "Temperature\n('C)",
  "Specific Heat\n(kJ/kg-K)",
  "Density\n(kg/m3)",
  "Viscosity\n(Pa-s)",
  "Kinematic Viscosity\n(m2-s)",
  "Conductivity\n(W/m-K)",
  "Enthalpy\n(J/kg)" };

enum { ID_GRID = wxID_HIGHEST+132, ID_NUMPOINTS,
	ID_IMPORT, ID_EXPORT };

BEGIN_EVENT_TABLE( MatPropDialog, wxDialog )
	EVT_BUTTON( ID_IMPORT, MatPropDialog::OnCommand )
	EVT_BUTTON( ID_EXPORT, MatPropDialog::OnCommand )
	EVT_NUMERIC( ID_NUMPOINTS, MatPropDialog::OnNumPointsChange )
	EVT_GRID_CMD_CELL_CHANGED( ID_GRID, MatPropDialog::OnGridCellChange)
	EVT_CLOSE( MatPropDialog::OnCloseWindow )
	EVT_BUTTON( wxID_HELP, MatPropDialog::OnCommand)
END_EVENT_TABLE()

MatPropDialog::MatPropDialog(wxWindow *parent)
	: wxDialog(parent, -1, "Edit Material Properties", wxDefaultPosition,
		  wxScaleSize(670, 530), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER)
{
	m_grid = new wxGrid(this, ID_GRID);

	m_grid->CreateGrid(1,MPC_NCOLS);
	m_grid->EnableEditing(true);
	m_grid->DisableDragCell();
	m_grid->DisableDragColSize();
	m_grid->DisableDragRowSize();
	m_grid->DisableDragColMove();
	m_grid->DisableDragGridSize();

	for (int i=0;i<MPC_NCOLS;i++)
		m_grid->SetColLabelValue(i, mat_col_labels[i]);
	
	m_grid->SetColLabelSize(wxGRID_AUTOSIZE);
	m_grid->SetColLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);
	m_grid->AutoSize();

	m_numPoints = new wxNumericCtrl(this, ID_NUMPOINTS, 1, wxNUMERIC_INTEGER );

	wxBoxSizer *bxtop = new wxBoxSizer(wxHORIZONTAL);
	bxtop->Add( new wxStaticText( this,-1, "Number of data points:" ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2);
	bxtop->Add( m_numPoints, 0, wxALL|wxEXPAND, 2);
	bxtop->AddStretchSpacer(1);
	bxtop->Add( new wxButton(this, ID_IMPORT, "Import..."), 0, wxALL, 2);
	bxtop->Add( new wxButton(this, ID_EXPORT, "Export..."), 0, wxALL, 2);

	wxBoxSizer *bxmain = new wxBoxSizer(wxVERTICAL);
	bxmain->Add(bxtop,0,wxALL|wxEXPAND, 3);
	bxmain->Add(m_grid, 1, wxALL|wxEXPAND, 3);
	bxmain->Add( CreateButtonSizer( wxOK|wxCANCEL|wxHELP ), 0, wxALL|wxEXPAND, 6);

	SetSizer( bxmain );
}

void MatPropDialog::SetData(const matrix_t<double> &data)
{
	int nrows = data.nrows();

	ChangeNumRows(nrows, false);
	
	m_grid->Freeze();

	for (int r=0;r<nrows;r++)
	{
		for (size_t c=0;c<MPC_NCOLS;c++)
		{
			wxString val = "0";
			if (c<data.ncols()) val.Printf("%lg", data.at(r,c));
			m_grid->SetCellValue(r,c,val);
		}
	}

	m_numPoints->SetValue(nrows);

	m_grid->SetRowLabelSize(wxGRID_AUTOSIZE);
	m_grid->SetRowLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);
	m_grid->SetColLabelSize(wxGRID_AUTOSIZE);
	m_grid->SetColLabelAlignment(wxALIGN_LEFT,wxALIGN_CENTRE);
	m_grid->AutoSize();
	
	m_grid->Thaw();
	m_grid->Layout();
	
	Layout();
	m_grid->ForceRefresh();

}

matrix_t<double> MatPropDialog::GetData()
{
	matrix_t<double> dat;
	int nrows = m_grid->GetNumberRows();
	dat.resize_fill(nrows, MPC_NCOLS, 0.0);

	for (int r=0;r<nrows;r++)
		for (int c=0;c<MPC_NCOLS;c++)
			dat.at(r,c) = (double) wxAtof( m_grid->GetCellValue(r,c) );

	return dat;
}

void MatPropDialog::OnCloseWindow(wxCloseEvent &)
{
	EndModal(wxID_CANCEL);
}

void MatPropDialog::OnCommand(wxCommandEvent &evt)
{
	switch(evt.GetId())
	{
	case ID_IMPORT: Import(); break;
	case ID_EXPORT: Export(); break;
	case wxID_HELP: SamApp::ShowHelp("edit_material_properties"); break;
	}
}

void MatPropDialog::OnNumPointsChange(wxCommandEvent &)
{
	ChangeNumRows( m_numPoints->AsInteger() );
}

void MatPropDialog::ChangeNumRows(int nrows, bool do_layout)
	{
	if (nrows < 1)
		nrows = 1;

	if (nrows != m_numPoints->AsInteger())
		m_numPoints->SetValue(nrows);
	

	m_grid->Freeze();

	if (m_grid->GetNumberRows() > nrows)
		m_grid->DeleteRows( nrows, m_grid->GetNumberRows() - nrows );

	if (m_grid->GetNumberRows() < nrows)
		m_grid->AppendRows( nrows - m_grid->GetNumberRows() );

	m_grid->Thaw();

	if (do_layout)
	{
		m_grid->SetRowLabelSize(wxGRID_AUTOSIZE);
		m_grid->AutoSize();
		m_grid->Layout();
		m_grid->GetParent()->Layout();
		m_grid->ForceRefresh();
	}
}


void MatPropDialog::OnGridCellChange(wxGridEvent &evt)
{
	int r = evt.GetRow();
	int c = evt.GetCol();
	double val = wxAtof( m_grid->GetCellValue(r,c) );
	m_grid->SetCellValue( r, c, wxString::Format("%lg", val) );
}

void MatPropDialog::Import()
{
	wxFileDialog fdlg(this, "Import Material Properties Table", "", "",
		"CSV Files (*.csv)|*.csv|All Files (*.*)|*.*", wxFD_OPEN );

	if (fdlg.ShowModal() == wxID_OK)
	{
		wxCSVData csv;
		if ( !csv.ReadFile( fdlg.GetPath() ) )
		{
			wxMessageBox("Could not read file:\n\n" + fdlg.GetPath(), "Error", wxICON_ERROR|wxOK);
			return;
		}


		if ( csv.NumCols() != 7 || csv.NumRows() < 2 )
		{
			wxMessageBox("Invalid material data. Must have exactly seven columns.");
			return;
		}
		

		matrix_t<double> grid;
		grid.resize_fill( csv.NumRows(), csv.NumCols(), 0.0f );
		for (size_t r=0;r<grid.nrows();r++)
			for (size_t c=0;c<grid.ncols();c++)
				grid.at(r,c) = (float)wxAtof( csv(r,c) );

		SetData( grid );
	}

}

void MatPropDialog::Export()
{
	wxFileDialog fdlg(this, "Export Material Properties Data", "", "property_table.csv",
		"CSV Files (*.csv)|*.csv|All Files (*.*)|*.*", wxFD_SAVE | wxFD_OVERWRITE_PROMPT );

	if (fdlg.ShowModal() == wxID_OK)
	{
		wxCSVData csv;		
		matrix_t<double> dat = GetData();
		for (size_t r=0;r<dat.nrows();r++)
			for (size_t c=0;c<dat.ncols();c++)
				csv.Set( r, c, wxString::Format("%g", dat(r,c)) );

		if ( !csv.WriteFile( fdlg.GetPath() ) )
			wxMessageBox("Could not open file for writing:\n\n"+fdlg.GetPath(), "Error", wxICON_ERROR|wxOK);
	}
}


/*  1/18/2010 fluid property equations from TRNSYS sam_mw_pt_Type229.f90 and sam_trough_model_type805.f90

! The following scheme is used to define the substance number.  Some fluids are
! blocked off for the trough model, and some are blocked off for future use. 
! Any fluid number specified greater than 35 will be a user-defined fluid.
!    1.) Air
!    2.) Stainless_AISI316
!    3.) Water (liquid)
!    4.) Steam
!    5.) CO2
!    6.) Salt (68% KCl, 32% MgCl2)
!    7.) Salt (8% NaF, 92% NaBF4)
!    8.) Salt (25% KF, 75% KBF4)
!    9.) Salt (31% RbF, 69% RbBF4)
!    10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
!    11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
!    12.) Salt (58% KF, 42% ZrF4)
!    13.) Salt (58% LiCl, 42% RbCl)
!    14.) Salt (58% NaCl, 42% MgCl2)
!    15.) Salt (59.5% LiCl, 40.5% KCl)
!    16.) Salt (59.5% NaF, 40.5% ZrF4)
!    17.) Salt (60% NaNO3, 40% KNO3)
!    18.) Nitrate Salt**
!    19.) Caloria HT 43**
!    20.) Hitec XL**
!    21.) Therminol VP-1**
!    22.) Hitec**
!    23.) Dowtherm Q**
!    24.) Dowtherm RP**
!    25.) Salt XL**
!    26.) Argon (ideal gas properties)
!    27.) Hydrogen (ideal gas properties)
!    28.) -blank-
!    29.) -blank-
!    30.) -blank-
!    31.) -blank-
!    32.) -blank-
!    33.) -blank-
!    34.) -blank-
!    35.) -blank-
!    36+) User specified (lookup tables)
*/

wxString substance_flname(int flnum)
{
	wxString name="";

	switch(flnum)
	{
	case 1: //   1.) Air
		name = "Air";
		break;
	case 2: //   2.) Stainless_AISI316
		name = "Stainless_AISI316";
		break;
	case 3: //   3.) Water (liquid)
		name = "Water (liquid)";
		break;
	case 4: //   4.) Steam
		name = "Steam";
		break;
	case 5: //   5.) CO2
		name = "CO2";
		break;
	case 6: //   6.) Salt (68% KCl, 32% MgCl2)
		name = "Salt (68% KCl, 32% MgCl2)";
		break;
	case 7: //   7.) Salt (8% NaF, 92% NaBF4)
		name = "Salt (8% NaF, 92% NaBF4)";
		break;
	case 8: //   8.) Salt (25% KF, 75% KBF4)
		name = "Salt (25% KF, 75% KBF4)";
		break;
	case 9: //   9.) Salt (31% RbF, 69% RbBF4)
		name = "Salt (31% RbF, 69% RbBF4)";
		break;
	case 10: //   10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
		name = "Salt (46.5% LiF, 11.5%NaF, 42%KF)";
		break;
	case 11: //   11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
		name = "Salt (49% LiF, 29% NaF, 29% ZrF4)";
		break;
	case 12: //   12.) Salt (58% KF, 42% ZrF4)
		name = "Salt (58% KF, 42% ZrF4)";
		break;
	case 13: //   13.) Salt (58% LiCl, 42% RbCl)
		name = "Salt (58% LiCl, 42% RbCl)";
		break;
	case 14: //   14.) Salt (58% NaCl, 42% MgCl2)
		name = "Salt (58% NaCl, 42% MgCl2)";
		break;
	case 15: //   15.) Salt (59.5% LiCl, 40.5% KCl)
		name = "Salt (59.5% LiCl, 40.5% KCl)";
		break;
	case 16: //   16.) Salt (59.5% NaF, 40.5% ZrF4)
		name = "Salt (59.5% NaF, 40.5% ZrF4)";
		break;
	case 17: //   17.) Salt (60% NaNO3, 40% KNO3)
		name = "Salt (60% NaNO3, 40% KNO3)";
		break;
	case 18: //   18.) Nitrate Salt** type 805 dens_salt(Tc)
		name = "Nitrate Salt";
		break;
	case 19: //   19.) Caloria HT 43** type 805 dens_caloria(Tc)
		name = "Caloria HT 43";
		break;
	case 20: //   20.) Hitec XL** type 805 dens_salt_xl(Tc)
		name = "Hitec XL";
		break;
	case 21: //   21.) Therminol VP-1** type 805 dens_therminol(Tc)
		name = "Therminol VP-1";
		break;
	case 22: //   22.) Hitec** type 805 dens_salt_hitec(Tc)
		name = "Hitec";
		break;
	case 23: //   23.) Dowtherm Q** type 805 dens_Dowtherm_Q(Tc)
		name = "Dowtherm Q";
		break;
	case 24: //   24.) Dowtherm RP** type 805 dens_Dowtherm_RP(Tc)
		name = "Dowtherm RP";
		break;
	case 25: //   25.) Salt XL** type 805 dens_salt_xl(Tc)
		name = "Salt XL";
		break;
	case 26: //   26.) Argon (ideal gas properties)
		name = "Argon";
		break;
	case 27: //   27.) Hydrogen (ideal gas properties)
		name = "Hydrogen";
		break;
	case 28: //   28.) -blank-
	case 29: //   29.) -blank-
		name = "Therminol 66";
		break;
	case 30: //   30.) -blank-
		name = "Therminol 59";
		break;
	case 31: //   31.) Pressurized water at 23 bar (2.10.17 twn: added)
		name = "Pressure water at 23 bar";
		break;
	case 32: //   32.) -blank-
	case 33: //   33.) -blank-
	case 34: //   34.) -blank-
	case 35: //   35.) -blank-
		name = "-blank-";
		break;
	case 36: //   36+) User specified (lookup tables)
		name = "User specified (lookup tables)";
		break;
	}
	return name;
}


double substance_dens(int flnum, double Tc)
{
/*
This function accepts as inputs temperature [K] and pressure [Pa]
This function outputs density in units of [kg/m^3]
*/
	double T = Tc + 273.15; // C to K
	double v,R_air;
	double P=1.0; // default pressure used 1Pa
	double density=1.0; // default - Type 229=0, Type 805=1

	switch(flnum)
	{
	case 1: //   1.) Air
		R_air = 287; // Gas constant [J/kg-K]
		v = R_air*T/P;
		density = 1.0/v;
		break;
	case 2: //   2.) Stainless_AISI316
		density = 8349.38 - 0.341708*T - 0.0000865128*T*T; // !EES
		break;
	case 3: //   3.) Water (liquid)
		break;
	case 4: //   4.) Steam
		break;
	case 5: //   5.) CO2
		break;
	case 6: //   6.) Salt (68% KCl, 32% MgCl2)
		density = 1E-10*T*T*T - 3E-07*T*T - 0.4739*T + 2384.2;
		break;
	case 7: //   7.) Salt (8% NaF, 92% NaBF4)
		density = 8E-09*T*T*T - 2E-05*T*T - 0.6867*T + 2438.5;
		break;
	case 8: //   8.) Salt (25% KF, 75% KBF4)
		density = 2E-08*T*T*T - 6E-05*T*T - 0.7701*T + 2466.1;
		break;
	case 9: //   9.) Salt (31% RbF, 69% RbBF4)
		density = -1E-08*T*T*T + 4E-05*T*T - 1.0836*T + 3242.6;
		break;
	case 10: //   10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
		density =  -2E-09*T*T*T + 1E-05*T*T - 0.7427*T + 2734.7;
		break;
	case 11: //   11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
		density = -2E-11*T*T*T + 1E-07*T*T - 0.5172*T + 3674.3;
		break;
	case 12: //   12.) Salt (58% KF, 42% ZrF4)
		density =  -6E-10*T*T*T + 4E-06*T*T - 0.8931*T + 3661.3;
		break;
	case 13: //   13.) Salt (58% LiCl, 42% RbCl)
		density = -8E-10*T*T*T + 1E-06*T*T - 0.689*T + 2929.5;
		break;
	case 14: //   14.) Salt (58% NaCl, 42% MgCl2)
		density = -5E-09*T*T*T + 2E-05*T*T - 0.5298*T + 2444.1;
		break;
	case 15: //   15.) Salt (59.5% LiCl, 40.5% KCl)
		density = 1E-09*T*T*T - 5E-06*T*T - 0.864*T + 2112.6;
		break;
	case 16: //   16.) Salt (59.5% NaF, 40.5% ZrF4)
		density =  -5E-09*T*T*T + 2E-05*T*T - 0.9144*T + 3837.0;
		break;
	case 17: //   17.) Salt (60% NaNO3, 40% KNO3)
		density = -1E-07*T*T*T + 0.0002*T*T - 0.7875*T + 2299.4;
		density = std::max(density, 1000.0);
		break;
/*
case(18:25) !  18-25.) Call trough properties
density = Dens_fluid((T-273.15),int(Fnumd)) !Trough calcs take fluid properties in degC, not K
*/
	case 18: //   18.) Nitrate Salt** type 805 dens_salt(Tc)
		density = 2090 - 0.636 * Tc;
		density = std::max(density, 1000.0);
		break;
	case 19: //   19.) Caloria HT 43** type 805 dens_caloria(Tc)
		density = 885 - 0.6617 * Tc - 0.0001265 * Tc*Tc;
		density = std::max(density, 100.0);
		break;
	case 20: //   20.) Hitec XL** type 805 dens_salt_xl(Tc)
		density = 2240 - 0.8266 * Tc;
		density = std::max(density, 800.0);
		break;
	case 21: //   21.) Therminol VP-1** type 805 dens_therminol(Tc)
		density = 1074.0 - 0.6367 * Tc - 0.0007762 * Tc*Tc;
		density = std::max(density, 400.0);
		break;
	case 22: //   22.) Hitec** type 805 dens_salt_hitec(Tc)
		density = 2080 - 0.733 * Tc;
		density = std::max(density, 1000.0);
		break;
	case 23: //   23.) Dowtherm Q** type 805 dens_Dowtherm_Q(Tc)
		density = -0.757332 * Tc + 980.787;
		density = std::max(density, 100.0);
		break;
	case 24: //   24.) Dowtherm RP** type 805 dens_Dowtherm_RP(Tc)
		density = -0.000186495 * Tc*Tc - 0.668337 * Tc + 1042.11;
		density = std::max(density, 200.0);
		break;
	case 25: //   25.) Salt XL** type 805 dens_salt_xl(Tc)
		density = 2240 - 0.8266 * Tc;
		density = std::max(density, 800.0);
		break;
	case 26: //   26.) Argon (ideal gas properties)
		density = P/(208.13*T);
		density = std::max(density, 1e-10);
		break;
	case 27: //   27.) Hydrogen (ideal gas properties)
		density = P/(4124.*T);
		density = std::max(density, 1e-10);
		break;
	case 28: //   28.) T-91 Steel: "Thermo hydraulic optimisation of the EURISOL DS target" - Paul Scherrer Institut
		density = -0.3289*Tc + 7742.5;
		break;
	case 29: //   29.) Therminol 66: Reference: Therminol Reference Disk by Solutia: www.therminol.com/pages/tools/toolscd.asp
		density = -0.7146*Tc + 1024.8;
		break;
	case 30: //   30.) Therminol 59: Reference: Therminol Reference Disk by Solutia: www.therminol.com/pages/tools/toolscd.asp
		density = -0.0003*Tc*Tc - 0.6963*Tc + 988.44;
		break;
	case 31: //   31.) Pressurized water at 23 bar (2.10.17 twn: added)
		density = -0.0023*Tc*Tc - 0.2337*Tc + 1005.6;
		break;
	case 32: //   32.) -blank-
	case 33: //   33.) -blank-
	case 34: //   34.) -blank-
	case 35: //   35.) -blank-
		break;
	case 36: //   36+) User specified (lookup tables)
/*
!Call the user-defined property table
lb=fl_bounds(fnum-35)
ub=fl_bounds(fnum-35+1)-1
if(ub.lt.lb) ub=size(fprop(1,:))
dxx(:)=fprop(1,lb:ub)
dyy(:)=fprop(3,lb:ub)
call interp(T,size(dxx),dxx,dyy,Gjsav,Density)
if((Gjsav.eq.ub).or.(Gjsav.eq.lb)) dum=t_warn(T,dxx(lb),dxx(ub),"User-specified fluid")
*/
		break;
	}
	return density;
}


double substance_sph(int flnum, double Tc)
{
/*
This function accepts as inputs temperature [K] and pressure [Pa]
This function outputs specific heat (Cp) in units of [kJ/kg-K]
*/
	double T = Tc + 273.15; // C to K
//	double P=1.0; // default pressure used 1Pa
	double specific_heat=1.0; // default - Type 229=0, Type 805=1

	switch(flnum)
	{
	case 1: //   1.) Air
		specific_heat = 1.03749 - 0.000305497*T + 7.49335E-07*T*T - 3.39363E-10*T*T*T;
		break;
	case 2: //   2.) Stainless_AISI316
		specific_heat = 0.368455 + 0.000399548*T - 1.70558E-07*T*T; //!EES
		break;
	case 3: //   3.) Water (liquid)
		break;
	case 4: //   4.) Steam
		break;
	case 5: //   5.) CO2
		break;
	case 6: //   6.) Salt (68% KCl, 32% MgCl2)
		specific_heat = 1.156;
		break;
	case 7: //   7.) Salt (8% NaF, 92% NaBF4)
		specific_heat = 1.507;
		break;
	case 8: //   8.) Salt (25% KF, 75% KBF4)
		specific_heat = 1.306;
		break;
	case 9: //   9.) Salt (31% RbF, 69% RbBF4)
		specific_heat = 9.127;
		break;
	case 10: //   10.) Salt (46.5% LiF, 11.5%NaF, 42%KF)
		specific_heat = 2.010; 
		break;
	case 11: //   11.) Salt (49% LiF, 29% NaF, 29% ZrF4)
		specific_heat = 1.239;
		break;
	case 12: //   12.) Salt (58% KF, 42% ZrF4)
		specific_heat = 1.051; 
		break;
	case 13: //   13.) Salt (58% LiCl, 42% RbCl)
		specific_heat = 8.918;
		break;
	case 14: //   14.) Salt (58% NaCl, 42% MgCl2)
		specific_heat = 1.080;
		break;
	case 15: //   15.) Salt (59.5% LiCl, 40.5% KCl)
		specific_heat = 1.202;
		break;
	case 16: //   16.) Salt (59.5% NaF, 40.5% ZrF4)
		specific_heat = 1.172;
		break;
	case 17: //   17.) Salt (60% NaNO3, 40% KNO3)
		specific_heat =  -1E-10*T*T*T + 2E-07*T*T + 5E-06*T + 1.4387;
		break;
/*
case(18:25) !  18-25.) trough properties
specheat = Cp_fluid((T-273.15),int(fnumd))/1000.d0
*/
	case 18: //   18.) Nitrate Salt** type 805 Cp_salt(Tc)
		specific_heat = (1443 + 0.172 * Tc) / 1000.0;
		break;
	case 19: //   19.) Caloria HT 43** type 805 Cp_caloria(Tc)
		specific_heat = (3.88 * Tc + 1606.0) / 1000.0;
		break;
	case 20: //   20.) Hitec XL** type 805 Cp_salt_xl(Tc) 
		specific_heat = std::max((1536 - 0.2624 * Tc - 0.0001139 * Tc * Tc),1000.0) / 1000.0;
		break;
	case 21: //   21.) Therminol VP-1** type 805 Cp_therminol(Tc)
		specific_heat = (1000 * (1.509 + 0.002496 * Tc + 0.0000007888 * Tc*Tc) ) / 1000.0;
		break;
	case 22: //   22.) Hitec** type 805 Cp_salt_hitec(Tc)
		specific_heat = (1560 - 0.0 * Tc) / 1000.0;
		break;
	case 23: //   23.) Dowtherm Q** type 805 Cp_Dowtherm_Q(Tc)
		specific_heat = (-0.00053943 * Tc*Tc + 3.2028 * Tc + 1589.2) / 1000.0;
		break;
	case 24: //   24.) Dowtherm RP** type 805 Cp_Dowtherm_RP(Tc)
		specific_heat = (-0.0000031915 * Tc*Tc + 2.977 * Tc + 1560.8) / 1000.0;
		break;
	case 25: //   25.) Salt XL** type 805 Cp_salt_xl(Tc) 
		specific_heat = std::max((1536 - 0.2624 * Tc - 0.0001139 * Tc * Tc),1000.0) / 1000.0;
		break;
	case 26: //   26.) Argon (ideal gas properties)
		specific_heat = 0.5203; //Cp only, Cv is different
		break;
	case 27: //   27.) Hydrogen (ideal gas properties)
		specific_heat = std::max((-45.4022 + 0.690156*T - 0.00327354*T*T + 0.00000817326*T*T*T - 1.13234E-08*T*T*T*T + 8.24995E-12*T*T*T*T*T - 2.46804E-15*T*T*T*T*T*T),14.7);
		break;
	case 28://T-91 Steel: "Thermo hydraulic optimisation of the EURISOL DS target" - Paul Scherrer Institut
		specific_heat = 0.0004*Tc*Tc + 0.2473*Tc + 450.08;
		break;
	case 29:// Therminol 66: Reference: Therminol Reference Disk by Solutia: www.therminol.com/pages/tools/toolscd.asp
		specific_heat = 0.0036*Tc + 1.4801;
		break;
	case 30://Therminol 59: Reference: Therminol Reference Disk by Solutia: www.therminol.com/pages/tools/toolscd.asp
		specific_heat = 0.0033*Tc + 1.6132;
		break;
	case 31: //   31.) Pressurized water at 23 bar (2.10.17 twn: added)
		specific_heat = 1.E-5*Tc*Tc - 0.0014*Tc + 4.2092;
		break;
	case 32: //   32.) -blank-
	case 33: //   33.) -blank-
	case 34: //   34.) -blank-
	case 35: //   35.) -blank-
		break;
	case 36: //   36+) User specified (lookup tables)
/*
!Call the user-defined property table
lb=fl_bounds(fnum-35)
ub=fl_bounds(fnum-35+1)-1
if(ub.lt.lb) ub=size(fprop(1,:))
dxx(:)=fprop(1,lb:ub)
dyy(:)=fprop(2,lb:ub)
call interp(T,size(dxx),dxx,dyy,Gjsav,specific_heat)
if((Gjsav.eq.ub).or.(Gjsav.eq.lb)) dum=t_warn(T,dxx(lb),dxx(ub),"User-specified fluid")
*/
		break;
	}
	return specific_heat;
}
