/*
BSD 3-Clause License

Copyright (c) Alliance for Sustainable Energy, LLC. See also https://github.com/NREL/SAM/blob/develop/LICENSE
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <wx/sizer.h>
#include <wx/checklst.h>
#include <wx/checkbox.h>
#include <wx/spinctrl.h>

#include <ptesdesignptdialog.h>
#include "main.h"
#include "script.h"
#include <vector>
#include <functional>
using std::vector;
using std::string;

BEGIN_EVENT_TABLE(PTESDesignPtDialog, wxDialog)
    EVT_BUTTON(wxID_OK, PTESDesignPtDialog::OnEvt)
END_EVENT_TABLE()

PTESDesignPtDialog::VarModel::VarModel(string var_name, string display_name, string description) :
    kVarName(var_name),
    kDisplayName(display_name),
    kDescription(description)
{

}

PTESDesignPtDialog::FluidVarModel::FluidVarModel(FluidType type)
    :
    kType(type)
{
    SetFluidTypeString(type);
}

vector<string> PTESDesignPtDialog::FluidVarModel::GetFluidMaterials()
{
    vector<string> fluid_types;

    switch (this->kType)
    {
    case(kWF):
    {
        fluid_types.push_back("Nitrogen");
        fluid_types.push_back("Argon");
        fluid_types.push_back("Hydrogen");
        fluid_types.push_back("Helium");
        fluid_types.push_back("Air");
        break;
    }
    case(kHF):
    {
        fluid_types.push_back("NitrateSalt");
        fluid_types.push_back("ChlorideSalt");
        break;
    }
    case(kCF):
    {
        fluid_types.push_back("Glycol");
        fluid_types.push_back("Methanol");
    }
    }


    return fluid_types;
}

string PTESDesignPtDialog::FluidVarModel::GetSelectedMaterial()
{
    if (this->combo_box_ != nullptr)
    {
        return combo_box_->GetValue();
    }
}

void PTESDesignPtDialog::FluidVarModel::SetFluidTypeString(FluidType type)
{
    switch (type)
    {
    case (kWF):
    {
        this->fluid_type_string_ = "Working Fluid";
        break;
    }
    case (kHF):
    {
        this->fluid_type_string_ = "Hot Fluid";
        break;
    }
    case (kCF):
    {
        this->fluid_type_string_ = "Cold Fluid";
        break;
    }
    }
}

PTESDesignPtDialog::PTESDesignPtDialog(wxWindow* parent, const wxString& title, lk::invoke_t& cxt)
    : PTESDesignPtDialog(parent, title)
{
}

PTESDesignPtDialog::PTESDesignPtDialog(wxWindow* parent, const wxString& title)
    :
    wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, wxMINIMIZE_BOX | wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    kMargin(5),
    kTxtCtrlHeight(20),
    kTxtCtrlWidth(150),
    working_fluid_(FluidVarModel::kWF),
    hot_fluid_(FluidVarModel::kHF),
    cold_fluid_(FluidVarModel::kCF)
{

    // Initialize
    m_result_code = -1;

    // Generate Component Variables
    {
        component_var_vec_.push_back(VarModel("hx_eff", "HX Eff", "Heat Exchanger Effectiveness"));
        component_var_vec_.push_back(VarModel("eta", "ETA", "Polytropic Efficiency of Compressors and Expanders"));
        component_var_vec_.push_back(VarModel("eta_pump", "Pump ETA", "Polytropic Efficiency of Air Pump"));
        component_var_vec_.push_back(VarModel("ploss_working", "Pressure Loss Fraction WF", "Fractional Pressure Loss of Working Fluid in Each Heat Exchanger"));
        component_var_vec_.push_back(VarModel("ploss_liquid", "Pressure Loss Fraction Air", "Fractional Pressure Loss of Air"));
        component_var_vec_.push_back(VarModel("motor_eff", "Motor Efficiency", "Motor Efficiency"));
        component_var_vec_.push_back(VarModel("gen_eff", "Generator Efficiency", "Generator Efficiency"));
    }

    // Generate Cycle Variables
    {
        cycle_var_vec_.push_back(VarModel("T0", "Ambient Temperature (K)", "Ambient Temperature (K)"));
        cycle_var_vec_.push_back(VarModel("P0", "Ambient Pressure (Pa)", "Ambient Pressure (Pa)"));
        cycle_var_vec_.push_back(VarModel("P1", "P1 (Pa)", "Lowest Presure in Cycle (Pa)"));
        cycle_var_vec_.push_back(VarModel("T_compressor_inlet", "Temperature Compressor Inlet (K)", "Compressor Inlet Temperature (K)"));
        cycle_var_vec_.push_back(VarModel("T_compressor_outlet", "Temperature Compressor Outlet (K)", "Compressor Outlet Temperature (K)"));
        cycle_var_vec_.push_back(VarModel("power_output", "Power Output (W)", "Power Output (W)"));
        cycle_var_vec_.push_back(VarModel("charge_time_hr", "Charge Time (hr)", "Charge Time (hr)"));
        cycle_var_vec_.push_back(VarModel("discharge_time_hr", "Discharge Time (hr)", "Discharge Time (hr)"));
        cycle_var_vec_.push_back(VarModel("alpha", "Air to WF Heat Rate Ratio", "mdot cp (air) / mdot cp (WF)"));
    }

    // Combine all into main vertical sizer
    wxBoxSizer* szmain = new wxBoxSizer(wxVERTICAL);

    // Have Two Vertical Sizers Side By Side
    wxBoxSizer* h_body = new wxBoxSizer(wxHORIZONTAL);

    // Tabs
    wxPanel* tabpanel = new wxPanel(this, wxID_ANY);
    ntbook_ = new wxMetroNotebook(tabpanel, wxID_ANY);
    {
        vector<wxTextCtrl*> component_textctrl_vec;
        wxWindow* component_tab = GenerateTabWindow(component_var_vec_);

        vector<wxTextCtrl*> cycle_textctrl_vec;
        wxWindow* cycle_tab = GenerateTabWindow(cycle_var_vec_);

       
        wxWindow* fluid_tab = GenerateFluidTab(working_fluid_, hot_fluid_, cold_fluid_);

        ntbook_->AddPage(cycle_tab, "Cycle");
        ntbook_->AddPage(component_tab, "Component");
        ntbook_->AddPage(fluid_tab, "Fluid");

        wxBoxSizer* panelSizer = new wxBoxSizer(wxHORIZONTAL);
        panelSizer->Add(ntbook_, 1, wxEXPAND);
        panelSizer->SetMinSize(400, 350);

        tabpanel->SetSizer(panelSizer);
        tabpanel->Fit();

        szmain->Add(tabpanel, 1, wxEXPAND);
    }

    szmain->Add(CreateButtonSizer(wxOK | wxCANCEL), 0, wxBOTTOM | wxRIGHT | wxLEFT | wxEXPAND, kMargin * 4);

    SetSizer(szmain);
    Fit();
}

/// <summary>
/// Generates A Tab Window 
/// </summary>
/// <param name="var_vec">Vector of VarModels</param>
/// <returns></returns>
wxWindow* PTESDesignPtDialog::GenerateTabWindow(vector<VarModel>& var_vec)
{
    // Make Window
    wxWindow* tab_window = new wxWindow(ntbook_, wxID_ANY);
    wxBoxSizer* window_szr = new wxBoxSizer(wxHORIZONTAL);

    // Have Two Vertical Sizers Side By Side
    wxBoxSizer* h_body = new wxBoxSizer(wxHORIZONTAL);

    wxBoxSizer* v_left = new wxBoxSizer(wxVERTICAL); // Left Column
    wxBoxSizer* v_right = new wxBoxSizer(wxVERTICAL); // Right Column


    // Loop through variables, adding text controls
    int count = var_vec.size();
    int count_left = std::round(count / 2.0);
    int count_right = count - count_left;
    int index = 0;
    wxBoxSizer* column = v_left;

    for (VarModel& var : var_vec)
    {
        if (index >= count_left)
            column = v_right;

        // Collect Variable Info
        string name = var.kDisplayName;
        string desc = var.kDescription;

        wxStaticText* label = new wxStaticText(tab_window, wxID_ANY, name, wxDefaultPosition, wxSize(kTxtCtrlWidth * 1.25, -1));
        label->SetToolTip(desc);
        column->Add(label, 0, wxTOP, kMargin);

        wxTextCtrl* text_ctrl = new wxTextCtrl(tab_window, wxID_ANY, "", wxDefaultPosition, wxSize(kTxtCtrlWidth, kTxtCtrlHeight));
        text_ctrl->SetToolTip(desc);
        column->Add(text_ctrl, 0, wxTOP, kMargin);
        var.SetTextCtrl(text_ctrl);

        index++;
    }

    h_body->Add(v_left, 0, wxRIGHT, kMargin);
    h_body->Add(v_right, 0, wxLEFT, kMargin);
    window_szr->Add(h_body, 0, wxALL, kMargin * 4);


    tab_window->SetSizer(window_szr);
    return tab_window;
}

/// <summary>
/// Generates the Fluid Tab Window and returns the window
/// </summary>
/// <param name="wf">Working Fluid</param>
/// <param name="hf">Hot Fluid</param>
/// <param name="cf">Cold Fluid</param>
/// <returns></returns>
wxWindow* PTESDesignPtDialog::GenerateFluidTab(FluidVarModel& wf, FluidVarModel& hf, FluidVarModel& cf)
{
    // Group Fluids
    vector<std::reference_wrapper<FluidVarModel>> fluids = { wf, hf, cf };


    // Make Window
    wxWindow* tab_window = new wxWindow(ntbook_, wxID_ANY);
    wxBoxSizer* window_szr = new wxBoxSizer(wxVERTICAL);
    wxBoxSizer* content_szr = new wxBoxSizer(wxVERTICAL);

    // Create Combo Box for each fluid Type (WF HF CF)
    for (FluidVarModel& fluid : fluids)
    {
        // Label
        string lable_string = fluid.GetFluidTypeString();
        wxStaticText* label = new wxStaticText(tab_window, wxID_ANY, lable_string, wxDefaultPosition, wxSize(kTxtCtrlWidth * 1.25, -1));
        content_szr->Add(label, 0, wxTOP, kMargin);

        // Combo Box
        wxComboBox* combo = new wxComboBox(tab_window, wxID_ANY, "", wxDefaultPosition, wxSize(kTxtCtrlWidth, -1));
        vector<string> fluid_options = fluid.GetFluidMaterials();
        for (string s : fluid_options)
            combo->AppendString(s);
        combo->SetValue(fluid_options[0]);
        fluid.SetComboBox(combo);

        // Add to tab
        content_szr->Add(combo, 0, wxTOP, kMargin);
    }

    window_szr->Add(content_szr, 0, wxALL, kMargin * 4);
    tab_window->SetSizer(window_szr);

    return tab_window;
}

void PTESDesignPtDialog::OnEvt(wxCommandEvent& e)
{
    switch (e.GetId())
    {
        case wxID_OK:
        {
            //auto text = this->test_ctrl_->GetValue().ToStdString();

            VarModel& var = component_var_vec_[0];

            string text = var.txt_ctrl_->GetValue();
            string text2 = working_fluid_.GetSelectedMaterial();


            EndModal(wxID_OK);
        }
    }
}


