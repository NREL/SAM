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

BEGIN_EVENT_TABLE(PTESDesignPtDialog::ConfirmDlg, wxDialog)
    EVT_BUTTON(wxID_OK, PTESDesignPtDialog::ConfirmDlg::OnEvt)
END_EVENT_TABLE()

// ----------------------------- VarModel

/// <summary>
/// Default Constructor
/// </summary>
PTESDesignPtDialog::VarModel::VarModel(string var_name, string display_name, string description, double default_value) :
    kVarName(var_name),
    kDisplayName(display_name),
    kDescription(description),
    default_value_(default_value)
{

}

/// <summary>
/// Get Double from TextCtrl
/// </summary>
/// <param name="flag">success flag</param>
/// <returns></returns>
double PTESDesignPtDialog::VarModel::GetValue(bool& flag)
{
    if (this->txt_ctrl_ == nullptr)
    {
        flag = true;
        return this->default_value_;
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

/// <summary>
/// Set Variable Model's text value
/// </summary>
/// <param name="val"></param>
void PTESDesignPtDialog::VarModel::SetTextValue(string val)
{
    this->txt_ctrl_->SetValue(val);
}

/// <summary>
/// Set Variable to be Ready Only on GUI
/// </summary>
/// <param name="flag"></param>
void PTESDesignPtDialog::VarModel::SetReadonly(bool flag)
{
    this->txt_ctrl_->SetEditable(!flag);
}

/// <summary>
/// Set Whether Variable Text Control is Displayed
/// Text Control must already be defined
/// </summary>
/// <param name="flag"></param>
void PTESDesignPtDialog::VarModel::SetVisible(bool flag)
{
    if (flag)
        this->txt_ctrl_->Show();
    else
        this->txt_ctrl_->Hide();
}

// ---------------------------- Fluid Var Model

/// <summary>
/// Default Constructor
/// </summary>
/// <param name="name">Display Name of Fluid</param>
/// <param name="type">Type of Fluid (WF, HF, CF)</param>
PTESDesignPtDialog::FluidVarModel::FluidVarModel(string name, FluidType type)
    :
    kVarName(name),
    kType(type)
{
    SetFluidTypeString(type);
}

/// <summary>
/// Get Fluid Material Options
/// </summary>
/// <returns></returns>
vector<string> PTESDesignPtDialog::FluidVarModel::GetFluidMaterials()
{
    vector<string> fluid_types;

    switch (this->kType)
    {
    case(kWF):
    {
        fluid_types.push_back("Air");
        fluid_types.push_back("Nitrogen");
        fluid_types.push_back("Argon");
        fluid_types.push_back("Hydrogen");
        fluid_types.push_back("Helium");
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

/// <summary>
/// Get User Selected Material from wxChoice
/// </summary>
/// <returns></returns>
string PTESDesignPtDialog::FluidVarModel::GetSelectedMaterial()
{
    if (this->choice_ != nullptr)
    {
        return choice_->GetStringSelection();
    }
}

/// <summary>
/// Set Type of Fluid (WF, HF, CF)
/// </summary>
/// <param name="type"></param>
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

// --------------------------- Confirm Dialog

/// <summary>
/// Constructor for dialog that displays results and asks to confirm them
/// </summary>
/// <param name="parent"></param>
/// <param name="title"></param>
/// <param name="cmod_io"></param>
/// <param name="margin"></param>
PTESDesignPtDialog::ConfirmDlg::ConfirmDlg(wxWindow* parent, const wxString& title, vector<CmodIOModel> cmod_io, double margin)
    :
    wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, wxMINIMIZE_BOX | wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    kMargin(margin)
{
    // Initialize
    result_code_ = -1;

    this->SetBackgroundColour(*wxWHITE);

    // Populate Dialog
    wxBoxSizer* window_szr = new wxBoxSizer(wxVERTICAL);

    wxStaticText* header_text = new wxStaticText(this, wxID_ANY, "Accept Design Point Results?");
    header_text->SetFont(wxMetroTheme::Font(wxMT_LIGHT, 17));
    window_szr->Add(header_text, 0, wxALIGN_LEFT | wxALL, kMargin * 2);

    wxFlexGridSizer* flx = new wxFlexGridSizer(2);
    flx->SetNonFlexibleGrowMode(wxFLEX_GROWMODE_SPECIFIED);
    bool is_left = true;
    int index = 0;

    // Loop through Result Data
    int size = cmod_io.size();
    for (int i = 0; i < size; i += 2)
    {
        // Left Column
        wxTextCtrl* val_label_left;
        {
            CmodIOModel& io = cmod_io[i];

            string name = io.name_;
            string desc = io.label_;
            string unit = io.unit_;
            string label_text = desc;
            if (unit != "")
                label_text += " (" + unit + ")";

            double value = io.val_num_;

            wxStaticText* name_label_left = new wxStaticText(this, wxID_ANY, label_text, wxDefaultPosition, wxDefaultSize);
            name_label_left->Wrap(175);

            val_label_left = new wxTextCtrl(this, wxID_ANY, std::to_string(value), wxDefaultPosition, wxSize(150, -1));
            val_label_left->SetEditable(false);

            // Add Left Label
            flx->Add(name_label_left, 1, wxALIGN_LEFT | wxRIGHT | wxBOTTOM, margin);
        }

        // Right Column
        wxTextCtrl* val_label_right;
        if (i < cmod_io.size() - 1)
        {
            CmodIOModel& io_right = cmod_io[i + 1];

            string name_right = io_right.name_;
            string desc_right = io_right.label_;
            string unit_right = io_right.unit_;
            string label_text_right = desc_right;
            if (unit_right != "")
                label_text_right += " (" + unit_right + ")";

            double value_right = io_right.val_num_;

            wxStaticText* name_label_right = new wxStaticText(this, wxID_ANY, label_text_right, wxDefaultPosition, wxDefaultSize);
            name_label_right->Wrap(175);

            val_label_right = new wxTextCtrl(this, wxID_ANY, std::to_string(value_right), wxDefaultPosition, wxSize(150, -1));
            val_label_right->SetEditable(false);

            // Add Right Label
            flx->Add(name_label_right, 1, wxALIGN_LEFT | wxLEFT | wxBOTTOM, margin);
        }
        else
        {
            // Add filler label
            wxStaticText* filler = new wxStaticText(this, wxID_ANY, "", wxDefaultPosition, wxDefaultSize);
            flx->Add(filler, 1, wxALIGN_LEFT | wxLEFT | wxBOTTOM, margin);
        }

        // Add Left Value
        flx->Add(val_label_left, 0, wxALIGN_LEFT | wxRIGHT | wxBOTTOM, margin);
        
        // Add Right Value (if necessary)
        if (i < cmod_io.size() - 1)
            flx->Add(val_label_right, 0, wxALIGN_LEFT | wxLEFT | wxBOTTOM, margin);


        // Make dummy panels for spacing
        for (int i : {0, 1})
        {
            wxPanel* dummy_panel = new wxPanel(this, wxID_ANY, wxDefaultPosition, wxSize(0, 0));
            flx->Add(dummy_panel, 0, wxBOTTOM, margin * 2);
        }
    }

    window_szr->Add(flx, 0, wxALL, kMargin * 4);

    window_szr->Add(CreateButtonSizer(wxOK | wxCANCEL), 0, wxBOTTOM | wxLEFT | wxRIGHT | wxEXPAND, kMargin * 4);

    this->SetSizer(window_szr);
    Fit();
}

/// <summary>
/// Catch Confirm Dialog Btn Events
/// </summary>
/// <param name="e"></param>
void PTESDesignPtDialog::ConfirmDlg::OnEvt(wxCommandEvent& e)
{
    switch (e.GetId())
    {
        case wxID_OK:
        {
            int x = 0;
            result_code_ = 0;

            EndModal(wxID_OK);
        }
    }
}

// --------------------------- PTES Design Point Dialog

/// <summary>
/// Construct PTESDesignPtDialog without cxt 
/// </summary>
/// <param name="parent"></param>
/// <param name="title"></param>
PTESDesignPtDialog::PTESDesignPtDialog(wxWindow* parent, const wxString& title)
    :
    wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize, wxMINIMIZE_BOX | wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    kMargin(5),
    kTxtCtrlHeight(-1),
    kTxtCtrlWidth(150),
    working_fluid_("working_fluid_type", FluidVarModel::kWF)
    //hot_fluid_("hot_fluid_type", FluidVarModel::kHF),
    //cold_fluid_("cold_fluid_type", FluidVarModel::kCF)
{
    // Initialize
    result_code_ = -1;

    // Generate Component Variables
    {
        component_var_vec_.push_back(VarModel("hx_eff", "HX Eff", "Heat Exchanger Effectiveness", 0.98));
        component_var_vec_.push_back(VarModel("eta", "ETA", "Polytropic Efficiency of Compressors and Expanders", 0.90));
        component_var_vec_.push_back(VarModel("eta_pump", "Pump ETA", "Polytropic Efficiency of Air Pump", 0.70));
        component_var_vec_.push_back(VarModel("ploss_working", "Pressure Loss Fraction WF", "Fractional Pressure Loss of Working Fluid in Each Heat Exchanger", 0.01));
        component_var_vec_.push_back(VarModel("ploss_air", "Pressure Loss Fraction Air", "Fractional Pressure Loss of Air", 0.005));
        component_var_vec_.push_back(VarModel("ploss_liquid", "Pressure Loss Fraction Liquid", "Fractional Pressure Loss of Hot and Cold Reservoir HX", 0.02));
        component_var_vec_.push_back(VarModel("motor_eff", "Motor Efficiency", "Motor Efficiency", 0.97216));
        component_var_vec_.push_back(VarModel("gen_eff", "Generator Efficiency", "Generator Efficiency", 0.97216));
    }

    // Generate Cycle Variables
    {
        cycle_var_vec_.push_back(VarModel("T0", "Ambient Temperature (K)", "Ambient Temperature (K)", 288));
        cycle_var_vec_.push_back(VarModel("P1", "P1 (Pa)", "Lowest Presure in Cycle (Pa)", 5e5));
        cycle_var_vec_.push_back(VarModel("T_compressor_inlet", "Temperature Compressor Inlet (K)", "Compressor Inlet Temperature (K)", 600));
        cycle_var_vec_.push_back(VarModel("T_compressor_outlet", "Temperature Compressor Outlet (K)", "Compressor Outlet Temperature (K)", 800));
        cycle_var_vec_.push_back(VarModel("power_output", "Power Output (W)", "Power Output (W)", 100e6));
        cycle_var_vec_.push_back(VarModel("charge_time_hr", "Charge Time (hr)", "Charge Time (hr)", 10));
        cycle_var_vec_.push_back(VarModel("discharge_time_hr", "Discharge Time (hr)", "Discharge Time (hr)", 10));
        cycle_var_vec_.push_back(VarModel("P0", "Ambient Pressure (Pa)", "Ambient Pressure (Pa)", 1e5));
    }

    // Setup SSC
    this->SetupSSC();

    // Setup UI
    this->InitializeUI();
}
 
/// <summary>
/// Destructor
/// </summary>
PTESDesignPtDialog::~PTESDesignPtDialog()
{
    ssc_module_free(module_);
}

/// <summary>
/// Get Design Point Calculation
/// </summary>
/// <param name="key">Result Name</param>
/// <returns></returns>
ssc_number_t PTESDesignPtDialog::GetResult(string key)
{
    if (ssc_num_result_map_.find(key) != ssc_num_result_map_.end())
        return ssc_num_result_map_[key];

    else
        return NULL;
}

/// <summary>
/// Get Map of Result Values
/// </summary>
/// <returns></returns>
std::map<string, ssc_number_t> PTESDesignPtDialog::GetResultNumMap()
{
    if (has_run_ == false)
        return std::map<string, ssc_number_t>();

    return this->ssc_num_result_map_;
}

/// <summary>
/// Changes Default Value
/// </summary>
/// <param name="name">variable name</param>
/// <param name="value">value</param>
/// <returns></returns>
bool PTESDesignPtDialog::SetInputVal(string name, double value, bool is_readonly, bool is_visible)
{
    // Check in Cycle
    for (vector<VarModel> vec : { component_var_vec_, cycle_var_vec_ })
    {
        for (VarModel& var : vec)
        {
            if (var.kVarName == name)
            {
                var.SetTextValue(std::to_string(value));
                var.SetReadonly(is_readonly);
                var.SetVisible(is_visible);
                return true;
            }
        }
    }

    return false;
}

/// <summary>
/// Add input variable that is passed on to the CMOD
/// Not Shown in dialog
/// </summary>
/// <param name="name"></param>
/// <param name="value"></param>
void PTESDesignPtDialog::AddHiddenInputVar(string name, double value)
{
    VarModel var(name, "", "", value);
    cycle_var_vec_.push_back(var);
}

/// <summary>
/// Set the Storage Fluid Material Properties
/// </summary>
/// <param name="hot_fluid_id">Hot Storage Fluid ID</param>
/// <param name="cold_fluid_id">Cold Storage Fluid ID</param>
/// <param name="hot_ud_props">Hot Storage User Defined Properties</param>
/// <param name="cold_ud_props">Cold Storage User Defined Properties</param>
void PTESDesignPtDialog::SetHTFProps(int hot_fluid_id, int cold_fluid_id, vector<vector<double>> hot_ud_props, vector<vector<double>> cold_ud_props)
{
    this->hot_fluid_id_ = hot_fluid_id;
    this->cold_fluid_id_ = cold_fluid_id;
    this->hot_ud_fluid_props_ = hot_ud_props;
    this->cold_ud_fluid_props_ = cold_ud_props;

    this->is_htf_set_ = true;
}

/// <summary>
/// Set Up SSC Compute Module Connection
/// </summary>
void PTESDesignPtDialog::SetupSSC()
{
    // Create SSC Data
    data_ = ssc_data_create();

    // Create SSC Module
    module_ = ssc_module_create("ptes_design_point");

    // Fill Result Variable Map
    int i = 0;
    ssc_info_t p_inf = NULL;
    while (p_inf = ssc_module_var_info(module_, i++))
    {
        int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
        int data_type = ssc_info_data_type(p_inf); // SSC_INVALID, SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX

        const char* name = ssc_info_name(p_inf);
        const char* label = ssc_info_label(p_inf);
        const char* units = ssc_info_units(p_inf);
        const char* meta = ssc_info_meta(p_inf);
        const char* group = ssc_info_group(p_inf);

        // Store Number Outputs in result map
        if (var_type == SSC_OUTPUT && data_type == SSC_NUMBER)
        {
            ssc_number_t x = 0;
            ssc_num_result_map_.insert(std::pair<string, ssc_number_t>(name, x));
        }

        CmodIOModel io(var_type, data_type, name, label, units, meta, group);
        cmod_vec_.push_back(io);
    }

    int x = 0;
}

/// <summary>
/// Run Compute Module
/// </summary>
string PTESDesignPtDialog::RunSSCModule()
{
    // Check if Hot and Cold Fluid Materials have been defined
    if (this->is_htf_set_ == false)
        return "Reservoir fluids not set";


    // Collect Input Variables Values and save to SSC Data
    string error_var = "";
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
                ssc_data_set_number(data_, var.kVarName.c_str(), val);
            }
            else
            {
                error_var = var.kDisplayName;
                break;
            }
        }
        if (error_var != "")
            break;
    }
    
    // Send variables not included in dialog
    ssc_data_set_number(data_, "alpha", 2);

    // Set Working Fluid
    ssc_data_set_string(data_, working_fluid_.kVarName.c_str(), working_fluid_.GetSelectedMaterial().c_str());

    if (error_var != "")
    {
        // There is an invalid user input
        return "Invalid User Input: " + error_var;
    }

    // Send hot and cold fluid properties
    {
        // Convert User Defined Props to array
        int hot_cols = hot_ud_fluid_props_[0].size();
        int hot_rows = hot_ud_fluid_props_.size();
        double_vec hot_flat;
        for (double_vec row : hot_ud_fluid_props_)
            for (double val : row)
                hot_flat.push_back(val);
        int cold_cols = cold_ud_fluid_props_[0].size();
        int cold_rows = cold_ud_fluid_props_.size();
        double_vec cold_flat;
        for (double_vec row : cold_ud_fluid_props_)
            for (double val : row)
                cold_flat.push_back(val);

        ssc_data_set_number(data_, "hot_fluid_id", hot_fluid_id_);
        ssc_data_set_number(data_, "cold_fluid_id", cold_fluid_id_);
        ssc_data_set_matrix(data_, "hot_ud_fluid_props", hot_flat.data(), hot_rows, hot_cols);
        ssc_data_set_matrix(data_, "cold_ud_fluid_props", cold_flat.data(), cold_rows, cold_cols);
    }

    // Run Module
    if (ssc_module_exec(module_, data_) == 0)
    {
        // Error
        return "Error Calculating Design Point";
    }

    // Collect Results from cmod
    vector<ssc_bool_t> flag_vec;
    for (auto& val : ssc_num_result_map_)
    {
        ssc_bool_t flag = ssc_data_get_number(data_, val.first.c_str(), &val.second);
        flag_vec.push_back(flag);
    }
    for (CmodIOModel& val : cmod_vec_)
    {
        ssc_data_get_number(data_, val.name_.c_str(), &val.val_num_);
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
        return "Error Retrieving Result Data";
    }

    has_run_ = true;
    result_code_ = 0;
    return "";
}

/// <summary>
/// Build GUI
/// </summary>
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


        wxWindow* fluid_tab = GenerateFluidTab(working_fluid_);

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
    szmain->Add(CreateButtonSizer(wxOK | wxCANCEL), 0, wxBOTTOM | wxLEFT | wxRIGHT | wxEXPAND, kMargin * 4);

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
wxWindow* PTESDesignPtDialog::GenerateFluidTab(FluidVarModel& wf)
{
    // Group Fluids
    //vector<std::reference_wrapper<FluidVarModel>> fluids = { wf, hf, cf };
    vector<std::reference_wrapper<FluidVarModel>> fluids = { wf };

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

/// <summary>
/// Show Confirm Results Dialog Box
/// </summary>
/// <returns>True: accept results, False: cancel results</returns>
bool PTESDesignPtDialog::LaunchConfirmDlg()
{
    vector<CmodIOModel> result_vec;

    // Send Results to dialog
    for (CmodIOModel& io : cmod_vec_)
    {
        if (io.group_ == "SAM")
            result_vec.push_back(io);
    }

    ConfirmDlg dlg(this, "", result_vec, this->kMargin);
    dlg.CenterOnParent();
    int code = dlg.ShowModal();

    if (code == wxID_OK)
    {
        return true;
    }
    else
    {
        return false;
    }
}

/// <summary>
/// Catch GUI Events
/// </summary>
/// <param name="e"></param>
void PTESDesignPtDialog::OnEvt(wxCommandEvent& e)
{
    switch (e.GetId())
    {
        case wxID_OK:
        {
            string error_msg = RunSSCModule();

            // Show Confirm Screen
            bool is_confirmed = false;
            if (error_msg == "")
            {
                is_confirmed = LaunchConfirmDlg();
            }
            // Issue running module
            else
            {
                wxMessageBox(error_msg);
            }

            if(is_confirmed)
                EndModal(wxID_OK);
        }
    }
}

