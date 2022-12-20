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
#include <wex/metro.h>

#include <ptesdesignptdialog.h>
#include "main.h"
#include "script.h"

#include <ssc/sscapi.h>
#include <vector>
#include <functional>
#include <sstream>

using std::vector;
using std::string;


BEGIN_EVENT_TABLE(PTESDesignPtDialog, wxDialog)
    EVT_BUTTON(wxID_OK, PTESDesignPtDialog::OnEvt)
END_EVENT_TABLE()

PTESDesignPtDialog::VarModel::VarModel(string var_name, string display_name, string description, double default_value) :
    kVarName(var_name),
    kDisplayName(display_name),
    kDescription(description),
    default_value_(default_value)
{

}

double PTESDesignPtDialog::VarModel::GetValue(bool& flag)
{
    if (this->txt_ctrl_ == nullptr)
    {
        flag = false;
        return 0;
    }

    // Parse String
    std::stringstream s(this->txt_ctrl_->GetValue().ToStdString());
    double d = 0;
    s >> d;
    if (s.fail())
    {
        flag = false;
        return 0;
    }
    else
    {
        flag = true;
        return d;
    }

}

PTESDesignPtDialog::FluidVarModel::FluidVarModel(string name, FluidType type)
    :
    kVarName(name),
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
    if (this->choice_ != nullptr)
    {
        return choice_->GetStringSelection();
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
    working_fluid_("working_fluid_type", FluidVarModel::kWF),
    hot_fluid_("hot_fluid_type", FluidVarModel::kHF),
    cold_fluid_("cold_fluid_type", FluidVarModel::kCF)
{
    // Initialize
    m_result_code = -1;

    // Generate Component Variables
    {
        component_var_vec_.push_back(VarModel("hx_eff", "HX Eff", "Heat Exchanger Effectiveness", 0.98));
        component_var_vec_.push_back(VarModel("eta", "ETA", "Polytropic Efficiency of Compressors and Expanders", 0.90));
        component_var_vec_.push_back(VarModel("eta_pump", "Pump ETA", "Polytropic Efficiency of Air Pump", 0.70));
        component_var_vec_.push_back(VarModel("ploss_working", "Pressure Loss Fraction WF", "Fractional Pressure Loss of Working Fluid in Each Heat Exchanger", 0.01));
        component_var_vec_.push_back(VarModel("ploss_air", "Pressure Pressure Loss Fraction Air", "Fractional Pressure Loss of Air", 0.005));
        component_var_vec_.push_back(VarModel("ploss_liquid", "Pressure Loss Fraction Liquid", "Fractional Pressure Loss of Hot and Cold Resevoir HX", 0.02));
        component_var_vec_.push_back(VarModel("motor_eff", "Motor Efficiency", "Motor Efficiency", 0.97216));
        component_var_vec_.push_back(VarModel("gen_eff", "Generator Efficiency", "Generator Efficiency", 0.97216));
    }

    // Generate Cycle Variables
    {
        cycle_var_vec_.push_back(VarModel("T0", "Ambient Temperature (K)", "Ambient Temperature (K)", 288));
        cycle_var_vec_.push_back(VarModel("P0", "Ambient Pressure (Pa)", "Ambient Pressure (Pa)", 1e5));
        cycle_var_vec_.push_back(VarModel("P1", "P1 (Pa)", "Lowest Presure in Cycle (Pa)", 5e5));
        cycle_var_vec_.push_back(VarModel("T_compressor_inlet", "Temperature Compressor Inlet (K)", "Compressor Inlet Temperature (K)", 600));
        cycle_var_vec_.push_back(VarModel("T_compressor_outlet", "Temperature Compressor Outlet (K)", "Compressor Outlet Temperature (K)", 800));
        cycle_var_vec_.push_back(VarModel("power_output", "Power Output (W)", "Power Output (W)", 100e6));
        cycle_var_vec_.push_back(VarModel("charge_time_hr", "Charge Time (hr)", "Charge Time (hr)", 10));
        cycle_var_vec_.push_back(VarModel("discharge_time_hr", "Discharge Time (hr)", "Discharge Time (hr)", 10));
        cycle_var_vec_.push_back(VarModel("alpha", "Air to WF Heat Rate Ratio", "mdot cp (air) / mdot cp (WF)", 2));
    }

    // Setup UI
    this->InitializeUI();
}

void PTESDesignPtDialog::InitializeUI()
{
    // Combine all into main vertical sizer
    wxBoxSizer* szmain = new wxBoxSizer(wxVERTICAL);
    this->SetBackgroundColour(*wxWHITE);

    wxStaticText* header_text = new wxStaticText(this, wxID_ANY, "PTES Design Point");
    header_text->SetFont(wxMetroTheme::Font(wxMT_LIGHT, 17));
    szmain->Add(header_text, 0, wxALIGN_LEFT | wxALL, kMargin * 2);

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

    // Okay Cancel Buttons
    szmain->Add(CreateButtonSizer(wxOK | wxCANCEL), 0, wxALL | wxEXPAND, kMargin * 4);

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
    tab_window->SetBackgroundColour(*wxWHITE);
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

        // Set Default Value
        std::ostringstream val;
        val << var.default_value_;
        val.precision(2);
        text_ctrl->SetValue(val.str());
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
    tab_window->SetBackgroundColour(*wxWHITE);
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
        //wxComboBox* combo = new wxComboBox(tab_window, wxID_ANY, "", wxDefaultPosition, wxSize(kTxtCtrlWidth, -1));
        wxChoice* choice = new wxChoice(tab_window, wxID_ANY, wxDefaultPosition, wxSize(kTxtCtrlWidth, -1));

        vector<string> fluid_options = fluid.GetFluidMaterials();
        for (string s : fluid_options)
        {
            //combo->AppendString(s);
            choice->AppendString(s);
        }
            
        //combo->SetValue(fluid_options[0]);
        choice->SetSelection(0);
        fluid.SetChoice(choice);

        // Add to tab
        content_szr->Add(choice, 0, wxTOP, kMargin);
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

            RunSSCModule();
            EndModal(wxID_OK);
        }
    }
}

void PTESDesignPtDialog::RunSSCModule()
{
    // Create SSC Data
    ssc_data_t data = ssc_data_create();

    // Collect Variables Values and save to SSC Data
    bool is_valid = true;
    for (vector<VarModel> var_vec : { component_var_vec_, cycle_var_vec_ }) // Cycle and Component Variables
    {
        for (VarModel& var : var_vec)
        {
            // Try to Parse
            bool flag = false;
            double val = var.GetValue(flag);

            // Set Data
            if (flag == true)
            {
                ssc_data_set_number(data, var.kVarName.c_str(), val);
            }
            else
            {
                is_valid = false;
            }
        }
    }

    // Collect Fluid Values and save to SSC Data
    for (FluidVarModel var : { working_fluid_, hot_fluid_, cold_fluid_ })
        ssc_data_set_string(data, var.kVarName.c_str(), var.GetSelectedMaterial().c_str());
    if (is_valid == false)
    {
        // There is an invalid user input
    }

    // Set Input Data

    // Run Module
    ssc_module_t module = ::ssc_module_create("ptes_design_point");
    if (ssc_module_exec(module, data) == 0)
    {
        // Error
        int x = 0;
    }

    // Collect Results
    ssc_number_t hp_COP, cycle_eff, Th_hot, Tc_hot, Tc_cold,
                 hp_parasitic_fraction, hp_hot_pump_power, hp_cold_pump_power,
                 pc_parasitic_fraction, pc_hot_pump_power, pc_cold_pump_power;
    vector<ssc_bool_t> flag_vec;
    {
        flag_vec.push_back(ssc_data_get_number(data, "hp_COP", &hp_COP));
        flag_vec.push_back(ssc_data_get_number(data, "cycle_eff", &cycle_eff));
        flag_vec.push_back(ssc_data_get_number(data, "Th_hot", &Th_hot));
        flag_vec.push_back(ssc_data_get_number(data, "Tc_hot", &Tc_hot));
        flag_vec.push_back(ssc_data_get_number(data, "Tc_hot", &Tc_cold));
        flag_vec.push_back(ssc_data_get_number(data, "hp_parasitic_fraction", &hp_parasitic_fraction));
        flag_vec.push_back(ssc_data_get_number(data, "hp_hot_pump_power", &hp_hot_pump_power));
        flag_vec.push_back(ssc_data_get_number(data, "hp_cold_pump_power", &hp_cold_pump_power));
        flag_vec.push_back(ssc_data_get_number(data, "pc_parasitic_fraction", &pc_parasitic_fraction));
        flag_vec.push_back(ssc_data_get_number(data, "pc_hot_pump_power", &pc_hot_pump_power));
        flag_vec.push_back(ssc_data_get_number(data, "pc_cold_pump_power", &pc_cold_pump_power));
    }
    bool result_flag = true;
    for (ssc_bool_t b : flag_vec)
    {
        if (b == false)
        {
            result_flag = false;
            break;
        }
    }
    if (result_flag == false)
    {
        // Unsuccessful Result Retrievel
    }

    int i = 0;
}
