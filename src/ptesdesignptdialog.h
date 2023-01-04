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


#ifndef __PTESDesignPtDialog_h
#define __PTESDesignPtDialog_h

#include <wx/dialog.h>
#include <string>
#include <map>

#include "main.h"
using std::vector;
using std::string;

class PTESDesignPtDialog : public wxDialog
{
    // Internal Class Definitions
private:
    // Model Input Variables
    class VarModel
    {
    public:

        // Methods
        VarModel(string var_name, string display_name, string description, double default_value = 0);
        void SetTextCtrl(wxTextCtrl* txt_ctrl) { txt_ctrl_ = txt_ctrl; }
        double GetValue(bool& flag);
        void SetTextValue(string val);
        void SetReadonly(bool flag);
        void SetVisible(bool flag);

        // Fields
        const string kVarName;
        const string kDisplayName;
        const string kDescription;
        const double default_value_;

        wxTextCtrl* txt_ctrl_ = nullptr;
    };

    // Model Fluid Input Variables
    class FluidVarModel
    {
    public:

        enum FluidType
        {
            kWF,
            kCF,
            kHF
        };

        // Methods
        FluidVarModel(string name, FluidType type);
        vector<string> GetFluidMaterials();
        string GetFluidTypeString() { return fluid_type_string_; }
        void SetChoice(wxChoice* choice) { choice_ = choice; }
        string GetSelectedMaterial();

        // Fields
        const FluidType kType;
        const string kVarName;

    private:
        void SetFluidTypeString(FluidType type);
        string fluid_type_string_;
        wxChoice* choice_ = nullptr;
    };

    // CMOD I/O Class
    class CmodIOModel
    {
    public:
        CmodIOModel(int var_type, int data_type, string name, string label, string unit, string meta, string group)
            :
              var_type_(var_type), data_type_(data_type), name_(name), label_(label), unit_(unit), meta_(meta), group_(group),
              val_num_(0), val_string_("")
        { }

        int var_type_;
        int data_type_;
        string name_;
        string label_;
        string unit_;
        string meta_;
        string group_;

        double val_num_;
        string val_string_;

    private:
        

    };

    // Confirm Results Dialog
    class ConfirmDlg : public wxDialog
    {
    public:
        ConfirmDlg(wxWindow* parent, const wxString& title, vector<CmodIOModel> cmod_io, double margin);
        int GetResultCode() { return result_code_; }

        // Event
        void OnEvt(wxCommandEvent&);
        DECLARE_EVENT_TABLE()
    private:
        const double kMargin;
        int result_code_;
    };


    // Public Methods
public:
    PTESDesignPtDialog(wxWindow* parent, const wxString& title);
    ~PTESDesignPtDialog();
    int GetResultCode() { return result_code_; };
    ssc_number_t GetResult(string result_key);
    std::map<string, ssc_number_t> GetResultNumMap();
    bool SetInputVal(string name, double value, bool is_readonly = false, bool is_visible = true);
    void AddHiddenInputVar(string name, double value);
    void SetHTFProps(int hot_fluid_id, int cold_fluid_id, vector<vector<double>> hot_ud_props, vector<vector<double>> cold_ud_props);
    
private:
    // GUI Properties
    const int kTxtCtrlWidth;
    const int kTxtCtrlHeight;
    const int kMargin;

    // Event
    void OnEvt(wxCommandEvent&);

    // Fields
    int result_code_;
    vector<VarModel> cycle_var_vec_;
    vector<VarModel> component_var_vec_;
    double hot_fluid_id_;
    double cold_fluid_id_;
    vector<double_vec> hot_ud_fluid_props_;
    vector<double_vec> cold_ud_fluid_props_;
    bool is_htf_set_ = false;
    FluidVarModel working_fluid_;
    bool has_run_ = false;

    // UI Fields
    wxMetroNotebook* ntbook_;

    // SSC Fields
    ssc_data_t data_;
    ssc_module_t module_;
    std::map<string, ssc_number_t> ssc_num_result_map_; // result data
    vector<CmodIOModel> cmod_vec_;

    // Private Methods
    void SetupSSC();
    string RunSSCModule();

    // UI Methods
    void InitializeUI();
    wxWindow* GenerateTabWindow(vector<VarModel>& var_vec);
    wxWindow* GenerateFluidTab(FluidVarModel& wf);
    bool LaunchConfirmDlg();

    DECLARE_EVENT_TABLE()
};

#endif // !__PTESDesignPtDialog_h
