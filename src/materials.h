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
