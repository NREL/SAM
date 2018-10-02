/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (“Alliance”) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
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
*  the underlying software originally provided by Alliance as “System Advisor Model” or “SAM”. Except
*  to comply with the foregoing, the terms “System Advisor Model”, “SAM”, or any confusingly similar
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

#ifndef __matpropctrl_h
#define __matpropctrl_h

#include <wx/wx.h>
#include <wx/grid.h>
#include <wx/treectrl.h>

#include <wex/numeric.h>

#include "object.h"


BEGIN_DECLARE_EVENT_TYPES()
DECLARE_EVENT_TYPE( wxEVT_MATPROPCTRL_CHANGE, 0 )
END_DECLARE_EVENT_TYPES()

#define EVT_MATPROPCTRL(id, func) EVT_COMMAND(id, wxEVT_MATPROPCTRL_CHANGE, func)

// column indexers
enum { 
	MPC_TEMP,
	MPC_CP,
	MPC_RHO,
	MPC_MU,
	MPC_NU,
	MPC_K,
	MPC_H,
	MPC_NCOLS // number of data columns
};

class MatPropCtrl : public wxButton
{
public:
	MatPropCtrl(wxWindow *parent, int id, 
		const wxPoint &pos = wxDefaultPosition, const wxSize &sz = wxDefaultSize);

	void SetData(const matrix_t<float> &data);
	matrix_t<float> GetData();
	
	void OnButton(wxCommandEvent &evt);

private:

	matrix_t<float> m_data;
	DECLARE_EVENT_TABLE();
};


class MatPropDialog : public wxDialog
{
public:
	MatPropDialog(wxWindow *parent);

	void SetData(const matrix_t<float> &data);
	matrix_t<float> GetData();

	void ChangeNumRows(int nrows, bool do_layout=true);

	void Import();
	void Export();

private:
	void OnCloseWindow(wxCloseEvent &evt);
	void OnCommand(wxCommandEvent &evt);
	void OnNumPointsChange(wxCommandEvent &evt);
	void OnGridCellChange(wxGridEvent &evt);

	wxGrid *m_grid;
	wxNumericCtrl *m_numPoints;


	DECLARE_EVENT_TABLE();
};


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

wxString substance_flname(int flnum);
double substance_dens(int flnum, double Tc);
double substance_sph(int flnum, double Tc);


#endif
