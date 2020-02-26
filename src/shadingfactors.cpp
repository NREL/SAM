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


#include <wx/clipbrd.h>
#include <wx/dcbuffer.h>
#include <wx/filename.h>
#include <wx/notebook.h>
#include <wx/statline.h>
#include <wx/tokenzr.h>
#include <wx/textfile.h>

#include <fstream>

#include <wex/csv.h>

#include "main.h"
#include "widgets.h"

#include "variables.h"
#include "shadingfactors.h"

/*********  SHADING BUTTON CTRL ************/

ShadingInputData::ShadingInputData() {
    clear();
}

void ShadingInputData::save(std::vector<double> &data) {
    data.clear();
//	data.push_back(3.0); // version number of data format - allows for expansion of options in future.
    data.push_back(4.0); // version number of data format - allows for expansion of options in future.

    data.push_back((en_mxh && mxh.nrows() == 12 && mxh.ncols() == 24) ? 1.0 : 0.0);
    data.push_back(en_azal ? 1.0 : 0.0);
    data.push_back(en_diff ? 1.0 : 0.0);
    data.push_back(-1.0);
    data.push_back(-1.0);
    data.push_back(-1.0);


    if (mxh.nrows() != 12 || mxh.ncols() != 24)
        mxh.resize_fill(12, 24, 0.0);

    for (int r = 0; r < 12; r++)
        for (int c = 0; c < 24; c++)
            data.push_back(mxh.at(r, c));

    data.push_back(azal.nrows());
    data.push_back(azal.ncols());
    for (size_t r = 0; r < azal.nrows(); r++)
        for (size_t c = 0; c < azal.ncols(); c++)
            data.push_back(azal.at(r, c));

    data.push_back(diff);

    // timestep at end for potential backwards compatibility
    // timestep string shading fractions matrix added timestep x number of parallel strings
    data.push_back((en_timestep && (timestep.nrows() % 8760 == 0)) ? 1.0 : 0.0);
    data.push_back(timestep.nrows());
    data.push_back(timestep.ncols());
    for (size_t r = 0; r < timestep.nrows(); r++)
        for (size_t c = 0; c < timestep.ncols(); c++)
            data.push_back(timestep.at(r, c));

    data.push_back(((string_option >= 0) && (timestep.nrows() % 8760 == 0) && en_timestep) ? string_option : -1);

    data.push_back(data.size() + 1); // verification flag that size is consistent
}

void ShadingInputData::clear() {
    en_mxh = en_azal = en_diff = en_timestep = false;

    timestep.resize(8760, 0);

    mxh.resize_fill(12, 24, 0.0);

    azal.resize_fill(10, 18, 0.0);

    for (int c = 0; c < 18; c++)
        azal.at(0, c) = c * 20;
    for (int r = 0; r < 10; r++)
        azal.at(r, 0) = r * 10;

    diff = 0.0;
    string_option = 0;
}

bool ShadingInputData::load(const std::vector<double> &data) {
    clear();

    if (data.size() < 3) return false;
    if (data.size() != (size_t) data[data.size() - 1]) return false; // verification check

    int idx = 0; // indexer to step through data

    int ver = (int) data[idx++];
    if (ver == 2) {
        en_timestep = data[idx++] > 0 ? true : false;
        en_mxh = data[idx++] > 0 ? true : false;
        en_azal = data[idx++] > 0 ? true : false;
        en_diff = data[idx++] > 0 ? true : false;
        idx++; // skip unused -1
        idx++; // skip unused -1
        idx++; // skip unused -1

        timestep.resize_fill(8760, 1, 0);
        for (int r = 0; r < 8760; r++)
            timestep.at(r, 0) = data[idx++];


        for (int r = 0; r < 12; r++)
            for (int c = 0; c < 24; c++)
                mxh.at(r, c) = data[idx++];

        int nr = (int) data[idx++];
        int nc = (int) data[idx++];
        azal.resize_fill(nr, nc, 1.0);
        for (int r = 0; r < nr; r++)
            for (int c = 0; c < nc; c++)
                azal.at(r, c) = data[idx++];

        diff = data[idx++];

        int verify = data[idx++];

        return idx == verify;
    } else {
        en_mxh = data[idx++] > 0 ? true : false;
        en_azal = data[idx++] > 0 ? true : false;
        en_diff = data[idx++] > 0 ? true : false;
        idx++; // skip unused -1
        idx++; // skip unused -1
        idx++; // skip unused -1


        for (int r = 0; r < 12; r++)
            for (int c = 0; c < 24; c++)
                mxh.at(r, c) = data[idx++];

        int nr = (int) data[idx++];
        int nc = (int) data[idx++];
        azal.resize_fill(nr, nc, 1.0);
        for (int r = 0; r < nr; r++)
            for (int c = 0; c < nc; c++)
                azal.at(r, c) = data[idx++];

        diff = data[idx++];

        en_timestep = data[idx++] > 0 ? true : false;
        nr = (int) data[idx++];
        nc = (int) data[idx++];
        timestep.resize_fill(nr, nc, 0);
        for (int r = 0; r < nr; r++)
            for (int c = 0; c < nc; c++)
                timestep.at(r, c) = data[idx++];

        string_option = data[idx++];

        int verify = data[idx++];

        return idx == verify;
    }
}

void ShadingInputData::write(VarValue *vv) {
    vv->SetType(VV_TABLE);
    VarTable &tab = vv->Table();
    tab.Set("en_string_option", VarValue(true)); // to enable optional values
    if (!en_timestep) string_option = -1;
    tab.Set("string_option", VarValue((int) string_option));
    tab.Set("en_timestep", VarValue((bool) en_timestep));
    tab.Set("timestep", VarValue(timestep));

    tab.Set("en_mxh", VarValue((bool) en_mxh));
    tab.Set("mxh", VarValue(mxh));
    tab.Set("en_azal", VarValue((bool) en_azal));
    tab.Set("azal", VarValue(azal));
    tab.Set("en_diff", VarValue((bool) en_diff));
    tab.Set("diff", VarValue((float) diff));
}

bool ShadingInputData::read(VarValue *root) {
    clear();
    if (root->Type() == VV_TABLE) {
        VarTable &tab = root->Table();
        if (VarValue *vv = tab.Get("string_option")) string_option = vv->Integer();
        if (VarValue *vv = tab.Get("en_timestep")) en_timestep = vv->Boolean();
        if (VarValue *vv = tab.Get("timestep")) timestep = vv->Matrix();
        if (VarValue *vv = tab.Get("en_mxh")) en_mxh = vv->Boolean();
        if (VarValue *vv = tab.Get("mxh")) mxh = vv->Matrix();
        if (VarValue *vv = tab.Get("en_azal")) en_azal = vv->Boolean();
        if (VarValue *vv = tab.Get("azal")) azal = vv->Matrix();
        if (VarValue *vv = tab.Get("en_diff")) en_diff = vv->Boolean();
        if (VarValue *vv = tab.Get("diff")) diff = vv->Value();
        return true;
    } else
        return false;
}


static const char *hourly_text_basic = "Enter or import a beam shading loss percentage for each of the simulation time steps in a single year. No shading is 0%, and full shading is 100%. Choose a time step in minutes equivalent to the weather file time step.\n\nNote that the 3D Shade Calculator automatically populates this beam shading table.";
static const char *hourly_text_strings = "Enter or import a beam shading loss percentage for each of the simulation time steps in a single year. No shading is 0%, and full shading is 100%. Choose a time step in minutes equivalent to the weather file time step. For a subarray of modules with c-Si cells and up to 8 strings of modules, you can use the partial shading model to estimate the impact of partial shading on the subarray's DC output.\n\nIf you use the 3D Shade Calculator to populate this beam shading table, be sure that the active surface subarray number(s) and string number(s) match the system design.";
static const char *mxh_text = "Enter 288 (24 hours x 12 month) beam shading loss percentages that apply to the 24 hours of the day for each month of the year. No shading is 0%, and full shading is 100%. Select a cell or group of cells and type a number to assign values to the table by hand. Click Import to import a table of values from a properly formatted text file. Click Export to export the data to a text file, or to create a template file for importing.";
static const char *azal_text = "Use the Azimuth by Altitude option if you have a set of beam shading losses for different sun positions.\n\n"
                               "1. Define the size of the table by entering values for the number of rows and columns.\n"
                               "2. Enter solar azimuth values (0 to 360 degrees) in the first row of the table, where 0 = north, 90 = east, 180 = south, 270 = west.\n"
                               "3. Enter solar altitude values (0 to 90 degrees) in the first column of the table, where zero is on the horizon.\n"
                               "4. Enter shading losses as the shaded percentage of the beam component of the incident radiation in the remaining table cells. No shading is 0%, and full shading is 100%.\n\n"
                               "Click Paste to populate the table from your computer\'s clipboard, or click Import to import a table of values from a properly formatted text file.  Click Export to export the data to a text file, or to create a template file for importing.";
static const char *diff_text = "The constant sky diffuse shading loss reduces the diffuse irradiance for each time step in the year. Valid values are between 0% and 100%.";

enum {
    ID_ENABLE_HOURLY = ::wxID_HIGHEST + 999,
    ID_ENABLE_MXH, ID_ENABLE_AZAL, ID_ENABLE_DIFF,
    ID_IMPORT_PVSYST_NEAR_SHADING,
    ID_IMPORT_SUNEYE_HOURLY,
    ID_IMPORT_SUNEYE_OBSTRUCTIONS,
    ID_IMPORT_SOLPATH_MXH
};

class ShadingDialog : public wxDialog {
    wxScrolledWindow *m_scrollWin;

    wxCheckBox *m_enableTimestep;
    wxShadingFactorsCtrl *m_timestep;
    wxStaticText *m_textTimestep;

    wxCheckBox *m_enableMxH;
    AFMonthByHourFactorCtrl *m_mxh;
    wxStaticText *m_textMxH;

    wxCheckBox *m_enableAzal;
    AFDataMatrixCtrl *m_azal;
    wxStaticText *m_textAzal;

    wxCheckBox *m_enableDiffuse;
    wxNumericCtrl *m_diffuseFrac;
    wxStaticText *m_textDiffuse;

    bool m_show_db_options;

public:
    ShadingDialog(wxWindow *parent, const wxString &descText, bool show_db_options = false)
            : wxDialog(parent, wxID_ANY,
                       wxString("Edit Shading Data") + wxString((!descText.IsEmpty() ? " for " : "")) + descText,
                       wxDefaultPosition, wxScaleSize(950, 600),
                       wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER) {
        SetEscapeId(wxID_CANCEL);

        m_show_db_options = show_db_options;

        m_scrollWin = new wxScrolledWindow(this, wxID_ANY);
        m_scrollWin->SetScrollRate(50, 50);

        m_enableTimestep = new wxCheckBox(m_scrollWin, ID_ENABLE_HOURLY,
                                          "Enable beam irradiance shading losses by time step");
        m_timestep = new wxShadingFactorsCtrl(m_scrollWin, wxID_ANY, wxDefaultPosition, wxDefaultSize,
                                              m_show_db_options);
        m_timestep->SetInitialSize(wxScaleSize(900, 300));
        m_timestep->SetMinuteCaption("Time step in minutes:");
        m_timestep->SetColCaption(wxString("Strings") + wxString((!descText.IsEmpty() ? " in " : "")) + descText +
                                  wxString((!descText.IsEmpty() ? ":" : "")));
//		m_timestep->SetStringCaption(wxString("Method for converting string losses to subarray:"));
        m_timestep->SetDBCaption(wxString("Enable partial shading model (c-Si modules only)"));

        int num_cols = 8;
        matrix_t<double> ts_data(8760, num_cols, 0.);
        m_timestep->SetData(ts_data);

        m_enableMxH = new wxCheckBox(m_scrollWin, ID_ENABLE_MXH, "Enable month-by-hour beam irradiance shading losses");
        m_mxh = new AFMonthByHourFactorCtrl(m_scrollWin, wxID_ANY);
        m_mxh->SetInitialSize(wxScaleSize(900, 330));

        m_enableAzal = new wxCheckBox(m_scrollWin, ID_ENABLE_AZAL,
                                      "Enable solar azimuth-by-altitude beam irradiance shading losses");
        //m_azal = new AFDataMatrixCtrl(m_scrollWin, wxID_ANY);
        // bottom buttons for import/export and added row and column labels
        m_azal = new AFDataMatrixCtrl(m_scrollWin, wxID_ANY, wxDefaultPosition, wxDefaultSize, false, wxEmptyString,
                                      wxEmptyString, wxEmptyString, -1, true);
        m_azal->SetInitialSize(wxScaleSize(900, 280));
        //m_azal->ShowRowLabels( false );
        m_azal->SetNumRowsLabel("Number of altitude values (rows):");
        m_azal->SetNumColsLabel("Number of azimuth values (columns):");
        m_azal->PasteAppendCols(true);
        m_azal->PasteAppendRows(true);
        m_azal->ShadeR0C0(true);

        matrix_t<double> data(10, 18, 0.);
        for (int c = 0; c < 18; c++)
            data.at(0, c) = c * 20;
        for (int r = 0; r < 10; r++)
            data.at(r, 0) = r * 10;
        m_azal->SetData(data);

        m_enableDiffuse = new wxCheckBox(m_scrollWin, ID_ENABLE_DIFF, "Enable constant sky diffuse shading loss");
        m_diffuseFrac = new wxNumericCtrl(m_scrollWin, wxID_ANY, 0.0);

        wxSizer *import_tools = new wxStaticBoxSizer(wxHORIZONTAL, m_scrollWin,
                                                     "Import shading data from external tools");
        import_tools->Add(new wxButton(m_scrollWin, ID_IMPORT_PVSYST_NEAR_SHADING, "PVsyst near shading..."), 0, wxALL,
                          3);
        import_tools->Add(new wxButton(m_scrollWin, ID_IMPORT_SUNEYE_HOURLY, "SunEye hourly shading..."), 0, wxALL, 3);
        import_tools->Add(new wxButton(m_scrollWin, ID_IMPORT_SUNEYE_OBSTRUCTIONS, "SunEye obstructions table..."), 0,
                          wxALL, 3);
        import_tools->Add(new wxButton(m_scrollWin, ID_IMPORT_SOLPATH_MXH, "SolarPathfinder month-by-hour shading..."),
                          0, wxALL, 3);

        wxColour text_color(0, 128, 192);
        int wrap_width = 700;

        wxSizer *scroll = new wxBoxSizer(wxVERTICAL);
        scroll->Add(import_tools, 0, wxALL, 5);
        scroll->Add(new wxStaticLine(m_scrollWin), 0, wxALL | wxEXPAND);

        scroll->Add(m_enableTimestep, 0, wxALL | wxEXPAND, 5);
        scroll->Add(m_textTimestep = new wxStaticText(m_scrollWin, wxID_ANY,
                                                      !descText.IsEmpty() ? hourly_text_strings : hourly_text_basic), 0,
                    wxALL | wxEXPAND, 10);
        scroll->Add(m_timestep, 0, wxALL, 5);
        m_textTimestep->Wrap(wrap_width);
        m_textTimestep->SetForegroundColour(text_color);

        scroll->Add(new wxStaticLine(m_scrollWin), 0, wxALL | wxEXPAND);
        scroll->Add(m_enableMxH, 0, wxALL | wxEXPAND, 5);
        scroll->Add(m_textMxH = new wxStaticText(m_scrollWin, wxID_ANY, mxh_text), 0, wxALL | wxEXPAND, 10);
        scroll->Add(m_mxh, 0, wxALL, 5);
        m_textMxH->Wrap(wrap_width);
        m_textMxH->SetForegroundColour(text_color);

        scroll->Add(new wxStaticLine(m_scrollWin), 0, wxALL | wxEXPAND);
        scroll->Add(m_enableAzal, 0, wxALL | wxEXPAND, 5);
        scroll->Add(m_textAzal = new wxStaticText(m_scrollWin, wxID_ANY, azal_text), 0, wxALL | wxEXPAND, 10);
        scroll->Add(m_azal, 0, wxALL, 5);
        m_textAzal->Wrap(wrap_width);
        m_textAzal->SetForegroundColour(text_color);

        scroll->Add(new wxStaticLine(m_scrollWin), 0, wxALL | wxEXPAND);
        scroll->Add(m_enableDiffuse, 0, wxALL | wxEXPAND, 5);
        scroll->Add(m_textDiffuse = new wxStaticText(m_scrollWin, wxID_ANY, diff_text), 0, wxALL | wxEXPAND, 10);
        scroll->Add(m_diffuseFrac, 0, wxALL, 5);
        m_textDiffuse->Wrap(wrap_width);
        m_textDiffuse->SetForegroundColour(text_color);

        m_scrollWin->SetSizer(scroll);

        wxSizer *box = new wxBoxSizer(wxVERTICAL);
        box->Add(m_scrollWin, 1, wxALL | wxEXPAND);
        box->Add(CreateButtonSizer(wxOK | wxCANCEL | wxHELP), 0, wxALL | wxEXPAND, 10);
        SetSizer(box);

        UpdateVisibility();
    }

    void UpdateVisibility() {
        m_timestep->Show(m_enableTimestep->IsChecked());
        m_textTimestep->Show(m_enableTimestep->IsChecked());

        m_mxh->Show(m_enableMxH->IsChecked());
        m_textMxH->Show(m_enableMxH->IsChecked());

        m_azal->Show(m_enableAzal->IsChecked());
        m_textAzal->Show(m_enableAzal->IsChecked());

        m_diffuseFrac->Show(m_enableDiffuse->IsChecked());
        m_textDiffuse->Show(m_enableDiffuse->IsChecked());

        m_scrollWin->FitInside();
        m_scrollWin->Refresh();
    }

    void ImportData(wxCommandEvent &e) {
        ShadingInputData dat;
        switch (e.GetId()) {
            case ID_IMPORT_PVSYST_NEAR_SHADING:
                if (ImportPVsystNearShading(dat, this)) {
                    wxMessageBox(Load(dat, false));
                    UpdateVisibility();
                }
                break;
            case ID_IMPORT_SUNEYE_HOURLY:
                if (ImportSunEyeHourly(dat, this)) {
                    wxMessageBox(Load(dat, false));
                    UpdateVisibility();
                }
                break;
            case ID_IMPORT_SUNEYE_OBSTRUCTIONS:
                if (ImportSunEyeObstructions(dat, this)) {
                    wxMessageBox(Load(dat, false));
                    UpdateVisibility();
                }
                break;
            case ID_IMPORT_SOLPATH_MXH:
                if (ImportSolPathMonthByHour(dat, this)) {
                    wxMessageBox(Load(dat, false));
                    UpdateVisibility();
                }
                break;
        }
    }

    void OnCommand(wxCommandEvent &e) {
        switch (e.GetId()) {
            case wxID_HELP:
                SamApp::ShowHelp("edit_shading_data");
                break;
            case ID_ENABLE_HOURLY:
            case ID_ENABLE_MXH:
            case ID_ENABLE_AZAL:
            case ID_ENABLE_DIFF:
                UpdateVisibility();
                break;
        }
    }


    void OnClose(wxCloseEvent &) {
        EndModal(wxID_CANCEL);
    }

    wxString Load(ShadingInputData &sh, bool all = true) {
        wxString stat;

        if (all || sh.en_timestep) {
            m_enableTimestep->SetValue(sh.en_timestep);
            if (sh.timestep.nrows() < 8760 || sh.timestep.ncols() > 8) {
                sh.timestep.resize_fill(8760, 1, 0);
            }
            m_timestep->SetData(sh.timestep);
            size_t ncols = sh.timestep.ncols();
            m_timestep->SetNumCols(ncols);

            size_t nminutes = 60;
            if ((sh.timestep.nrows() / 8760) > 0)
                nminutes /= (sh.timestep.nrows() / 8760);
            m_timestep->SetNumMinutes(nminutes);
            stat += "Updated timestep beam shading losses.\n";
            int string_option = sh.string_option;
            m_timestep->SetDBOption(string_option);
//			m_timestep->SetStringOption(string_option);
        }

        if (all || sh.en_mxh) {
            m_enableMxH->SetValue(sh.en_mxh);
            m_mxh->SetData(sh.mxh);
            stat += "Updated month-by-hour beam shading loss table.\n";
        }

        if (all || sh.en_azal) {
            m_enableAzal->SetValue(sh.en_azal);
            m_azal->SetData(sh.azal);
            stat += "Updated azimuth-by-altitude beam shading factor table.\n";
        }

        if (all || sh.en_diff) {
            m_enableDiffuse->SetValue(sh.en_diff);
            m_diffuseFrac->SetValue(sh.diff);
            stat += "Updated constant sky diffuse factor.\n";
        }

        UpdateVisibility();

        return stat;
    }

    void Save(ShadingInputData &sh) {

        sh.en_timestep = m_enableTimestep->IsChecked();
        m_timestep->GetData(sh.timestep);

        sh.string_option = m_timestep->GetDBOption();
//		sh.string_option = m_timestep->GetStringOption();

        sh.en_mxh = m_enableMxH->IsChecked();
        sh.mxh.copy(m_mxh->GetData());

        sh.en_azal = m_enableAzal->IsChecked();
        m_azal->GetData(sh.azal);

        sh.en_diff = m_enableDiffuse->IsChecked();
        sh.diff = m_diffuseFrac->Value();


    }

DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ShadingDialog, wxDialog)
                EVT_CLOSE(ShadingDialog::OnClose)
                EVT_CHECKBOX(ID_ENABLE_HOURLY, ShadingDialog::OnCommand)
                EVT_CHECKBOX(ID_ENABLE_MXH, ShadingDialog::OnCommand)
                EVT_CHECKBOX(ID_ENABLE_AZAL, ShadingDialog::OnCommand)
                EVT_CHECKBOX(ID_ENABLE_DIFF, ShadingDialog::OnCommand)

                EVT_BUTTON(ID_IMPORT_PVSYST_NEAR_SHADING, ShadingDialog::ImportData)
                EVT_BUTTON(ID_IMPORT_SUNEYE_HOURLY, ShadingDialog::ImportData)
                EVT_BUTTON(ID_IMPORT_SUNEYE_OBSTRUCTIONS, ShadingDialog::ImportData)
                EVT_BUTTON(ID_IMPORT_SOLPATH_MXH, ShadingDialog::ImportData)

                EVT_BUTTON(wxID_HELP, ShadingDialog::OnCommand)

END_EVENT_TABLE()


BEGIN_EVENT_TABLE(ShadingButtonCtrl, wxButton)
                EVT_BUTTON(wxID_ANY, ShadingButtonCtrl::OnPressed)
END_EVENT_TABLE()

ShadingButtonCtrl::ShadingButtonCtrl(wxWindow *parent, int id, bool show_db_options,
                                     const wxPoint &pos, const wxSize &size)
        : wxButton(parent, id, "Edit shading...", pos, size) {
    m_show_db_options = show_db_options;
}

void ShadingButtonCtrl::Write(VarValue *vv) {
    m_shad.write(vv);
}

bool ShadingButtonCtrl::Read(VarValue *vv) {
    return m_shad.read(vv);
}

void ShadingButtonCtrl::OnPressed(wxCommandEvent &evt) {
    ShadingDialog dlg(this, m_descText, m_show_db_options);
    dlg.Load(m_shad);

    if (dlg.ShowModal() == wxID_OK) {
        dlg.Save(m_shad);
        evt.Skip(); // allow event to propagate indicating underlying value changed
    }
}


/***************************************************************
  Utility functions to import externally generated shading data
  **************************************************************/


bool ImportPVsystNearShading(ShadingInputData &dat, wxWindow *parent) {
    //ask about version of PVsyst (5 versus 6) due to change in shading convention
    bool new_version = true;
    wxString msg = "Is this shading file from PVsyst version 6 or newer?";
    msg += "\n\nPVsyst changed their shading convention starting in Version 6 and later such that 0 now equals no shade and 1 equals full shade. ";
    msg += "To import a file from PVsyst versions 6 or newer, click Yes below. However, you may still import a file from versions 5 and older by selecting No below.";
    int ret = wxMessageBox(msg, "Important Notice", wxICON_EXCLAMATION | wxYES_NO, parent);
    if (ret == wxNO)
        new_version = false;

    //read in the file
    wxString buf;
    double diffuse = 0.0;
    int i;
    int j;

    bool readdata = false;
    bool readok = true;
    bool headingok = true;
    bool colok = true;
    int linesok = 0;


    matrix_t<double> azaltvals;
    azaltvals.resize_fill(11, 20, 0.0f);
    azaltvals.at(0, 0) = 0.0f;

    for (i = 1; i < 20; i++) azaltvals.at(0, i) = 20 * (i -
                                                        1);  // removed -180 degree offset to account for new azimuth convention (180=south) 4/2/2012, apd
    for (i = 1; i < 10; i++) azaltvals.at(i, 0) = 10 * i;
    azaltvals.at(10, 0) = 2.0f;

    wxFileDialog fdlg(parent, "Import PVsyst Near Shading File");
    if (fdlg.ShowModal() != wxID_OK) return false;
    wxString file = fdlg.GetPath();

    wxTextFile tf;
    if (!tf.Open(file)) {
        wxMessageBox("Could not open file for read:\n\n" + file);
        return false;
    }

    j = 0;
    buf = tf.GetFirstLine();
    while (!tf.Eof()) {
        wxArrayString lnp = wxStringTokenize(buf, ";:,\t");
        if (readdata == false && j == 0) {
            if (lnp.Count() > 0) {
                if (lnp.Item(0) == "Height") readdata = true;
            }
        } else if (j < 10) {
            j++;
            if (lnp.Count() != 20) {
                colok = false;
                readok = false;
                break;
            } else {
                for (i = 0; i < 20; i++) // read in Altitude in column zero
                {
                    if (i == 0) azaltvals.at(j, i) = (float) wxAtof(lnp[i]);        //do not change azimuth values
                    else {
                        if (lnp.Item(i) == "Behind")
                            azaltvals.at(j, i) = 100;    //"Behind" means it is fully behind another obstruction
                        else if (new_version) //PVsyst versions 6 and newer: 0 means no shade, 1 means full shade
                            azaltvals.at(j, i) = (float) wxAtof(lnp[i]) * 100; //convert to percentage
                        else //PVsyst versions 5 and older: 1 means no shade, 0 means full shade
                            azaltvals.at(j, i) = (1 - (float) wxAtof(lnp[i])) * 100;    //convert from factor to loss
                    }
                }
            }
        } else if (j == 10) {
            if (lnp.Count() == 3) {
                if (new_version) //PVsyst versions 6 and newer: 0 means no shade, 1 means full shade
                    diffuse = (float) wxAtof(lnp[1]) * 100; //convert to percentage
                else //PVsyst versions 5 and older: 1 means no shade, 0 means full shade
                    diffuse = (1 - (float) wxAtof(lnp[1])) * 100; //convert from factor to loss
            } else {
                readok = false;
                colok = false;
                break;
            }
            j++;
        } else j++;

        buf = tf.GetNextLine();
    }

    if (j < 11) {
        readok = false;
        linesok = -1;
    } else if (j > 11) {
        readok = false;
        linesok = 1;
    }
    if (readdata != true) {
        readok = false;
        headingok = false;
    }

    if (readok) {
        // re-sort from small altitude to large, if necessary
        if (azaltvals.at(10, 0) < azaltvals.at(1, 0)) {
            azaltvals.resize_preserve(12, 20, 1.0);

            for (j = 1; j < 12 / 2; j++) {
                for (i = 0; i < 20; i++) azaltvals.at(11, i) = azaltvals.at(j, i);
                for (i = 0; i < 20; i++) azaltvals.at(j, i) = azaltvals.at(11 - j, i);
                for (i = 0; i < 20; i++) azaltvals.at(11 - j, i) = azaltvals.at(11, i);
            }

            azaltvals.resize_preserve(11, 20, 1.0);
        }

        dat.clear();
        dat.en_azal = true;
        dat.azal.copy(azaltvals);

        dat.en_diff = true;
        dat.diff = diffuse;

        return true;
    } else {
        wxString m = "Invalid file format.\n\n";
        if (!headingok) m.Append("Invalid heading format.\n");
        if (!colok) m.Append("Invalid number of columns.\n");
        if (linesok == -1) m.Append("File contains fewer lines than expected.\n");
        if (linesok == 1) m.Append("File contains more lines than expected.\n");
        wxMessageBox(m);

        return false;
    }
}

bool ImportSunEyeHourly(ShadingInputData &dat, wxWindow *parent) {
    wxFileDialog fdlg(parent, "Import SunEye Shading File");
    if (fdlg.ShowModal() != wxID_OK) return false;

    wxTextFile tf;
    if (!tf.Open(fdlg.GetPath())) {
        wxMessageBox("Could not open file for read:\n\n" + fdlg.GetPath());
        return false;
    }

    wxString buf;
    int i;
    bool readdata = false;
    bool readok = true;
    bool headingok = true;
    bool colok = true;
    int linesok = 0;
    int day = 0;
    int start_timestep = 0;
    int start_hour = 0;
    int end_timestep = 0;
    int end_hour = 0;
    int hour_duration = 0; // how many hours (including incomplete hours) in the Suneye file
    double beam[8760];
    for (i = 0; i < 8760; i++) beam[i] = 0.0;

    buf = tf.GetFirstLine();
    while (!tf.Eof()) {
        wxArrayString lnp = wxStringTokenize(buf, ",", wxTOKEN_RET_EMPTY_ALL);
        if (readdata == false) {
            if (lnp.Count() > 0) {
                if (lnp.Item(0) == "begin data") {
                    readdata = true;
                    int iend = lnp.Count() - 1;
                    int icolon = 0;
                    icolon = lnp.Item(1).find(":");
                    if (icolon > 0) {
                        start_timestep = wxAtoi(lnp.Item(1).substr(icolon + 1, 2));
                        start_hour = wxAtoi(lnp.Item(1).substr(0, icolon));
                    }
                    icolon = lnp.Item(iend).find(":");
                    if (icolon > 0) {
                        end_timestep = wxAtoi(lnp.Item(iend).substr(icolon + 1, 2));
                        end_hour = wxAtoi(lnp.Item(iend).substr(0, icolon));
                    }
                    // check for valid duration
                    if ((start_hour == 0) || (end_hour == 0)) {
                        readdata = false;
                        break;
                    }
                    hour_duration = end_hour - start_hour + 1;
                }
            }
        } else {
            // shj update 5/25/11 - read in begin data and to end - no fixed count
            // assume that 15 timestep intervals and use start and end time and adjust to hour
            // JMF update 10/17/2014- average all values for an hour instead of taking the midpoint of the hour
            int index = 1; //keep track of where you are in the row- starts at 1 because of the date column.
            for (i = 0; i < hour_duration; i++) {
                //compute which hour to enter the shading factor into
                int x = day * 24 + start_hour + i;
                if (x >= 8760) {
                    readok = false;
                    break;
                }

                //how many 15-min entries are in this hour?
                int count = 0;
                if (i == 0) //first hour
                    count = (60 - start_timestep) / 15;
                else if (i == hour_duration - 1) //last hour
                    count = end_timestep / 15 + 1;
                else //whole hours in between
                    count = 4;

                //loop through the correct number of 15-timestep entries and to calculate an average shading value
                double total = 0;
                for (int j = 0; j < count; j++) {
                    if (lnp.Item(index).IsEmpty()) total += 0;
                    else total += wxAtof(lnp.Item(index));
                    index++; //don't forget to increment the index so that you read the next cell
                }

                //compute average and assign it to the appropriate hour
                beam[x] = (1 - (total / count)) * 100; //don't forget to convert it to a loss factor
            }
            day++;
        }

        buf = tf.GetNextLine();
    }

    if (day != 365) {
        readok = false;
        if (day - 365 < 0 && day != 0) linesok = -1;
        if (day - 365 > 0 && day != 0) linesok = 1;
    }

    if (readdata == false) {
        readok = false;
        headingok = false;
    }

    if (readok) {
        dat.clear();
        dat.en_timestep = true;
        dat.timestep.clear();
        dat.timestep.resize_fill(8760, 1, 0);
        for (size_t i = 0; i < 8760; i++)
            dat.timestep.at(i, 0) = beam[i];
        return true;
    } else {
        wxString m = "Invalid file format.\n\n";
        if (!headingok) m.Append("Invalid heading format.\n");
        if (!colok) m.Append("Invalid number of columns.\n");
        if (linesok == -1) m.Append("File contains fewer lines than expected.\n");
        if (linesok == 1) m.Append("File contains more lines than expected.\n");
        wxMessageBox(m);
        return false;
    }

}

bool ImportSunEyeObstructions(ShadingInputData &dat, wxWindow *parent) {


    wxFileDialog fdlg(parent, "Import SunEye Obstruction Elevations File");
    if (fdlg.ShowModal() != wxID_OK) return false;
    wxString file = fdlg.GetPath();
    wxTextFile tf;
    if (!tf.Open(file)) {
        wxMessageBox("Could not open file for read:\n\n" + file);
        return false;
    }

    wxString buf;
    int j = -2;
    bool readdata = false;
    bool readok = true;
    bool headingok = true;
    bool colok = true;
    int linesok = 0;
    double azi[361];
    int imageCount = 0;
    int columnCount = 0;
    int elevationStartCol = -1;
    float obstructionStep = 0.0;

    matrix_t<float> azaltvals, obstructions;
    azaltvals.resize_fill(91, 362, 0.0);
    azaltvals.at(0, 0) = 0.;

    buf = tf.GetFirstLine();

    while (!tf.Eof()) {
        wxArrayString lnp = wxStringTokenize(buf, ",");
        if (readdata == false) {
            if (lnp.Count() > 0 && j == -2) {
                if (lnp.Item(0) == "begin data") {
                    readdata = true;
                    j++;
                }
            }
        } else if (readdata == true && j == -1) { // get image count here
            columnCount = lnp.Count();
            for (int i = 0; i < columnCount; i++) {
                int ndx = lnp[i].Lower().Find("elevation");
                if (ndx != wxNOT_FOUND && (ndx < 2)) // Not Average or Maximum
                {
                    imageCount++;
                    // set elevation start column
                    if (elevationStartCol < 0) elevationStartCol = i;
                }
            }
            if (imageCount < 1 || elevationStartCol < 0) {
                wxMessageBox("Error: No 'Elevations' data columns found.");
                return false;
            }
            j++;
        } else {
            if (j == 0) {
                obstructions.resize_fill(362, imageCount, 0.0);
                obstructionStep = 100.0 / imageCount;
            }

            if (j <= 360) {
                if ((int) lnp.Count() < elevationStartCol + imageCount) {
                    wxMessageBox(wxString::Format("Error: Not enough data found at data row=%d.", j));
                    return false;
                } else {
                    azi[j] = wxAtof(lnp[0]); //first column contains compass heading azimuth values (0=north, 90=east)
                    for (int ii = 0; ii < imageCount && (ii + elevationStartCol) < (int) lnp.Count(); ii++)
                        obstructions.at(j, ii) = wxAtof(lnp[ii + elevationStartCol]);
                }
                j++;
            } else j++;
        }
        buf = tf.GetNextLine();
    }


    if (j < 361) {
        readok = false;
        linesok = -1;
    } else if (j > 361) {
        readok = false;
        linesok = 1;
    }
    if (readdata != true) {
        readok = false;
        headingok = false;
    }

    if (readok) {
        //copy azimuth/compass values into the first row
        for (int i = 1; i < 362; i++)
            azaltvals.at(0, i) = azi[i - 1];

        //elevation always goes from 1-90 degrees
        for (int i = 1; i < 91; i++)
            azaltvals.at(i, 0) = i;

        //loop through all azimuth values
        for (int j = 0; j < 362; j++) {
            for (int k = 0; k < imageCount; k++) {  // Sev 150624: loop over images

                if (obstructions.at(j, k) < 0 || obstructions.at(j, k) > 90) //changed from && to || 6/18/15 jmf
                {
                    wxMessageBox("Error: Elevations Must be less than 90 degrees and greater than 0 degrees");
                    return false;
                }

                //changed jmf 6/18/15- values UP TO the altitude value should be fully shaded.
                for (int i = 1; i <= obstructions.at(j, k); i++)
                    azaltvals.at(i, j +
                                    1) += obstructionStep;        // Sev 150624: obstruction amount now increase incrementally instead of going straight from 0 to 100
            }
        }

        dat.clear();

        dat.en_azal = true;
        dat.azal = azaltvals;

        return true;
    } else {
        wxString m = "Invalid file format.\n\n";
        if (!headingok) m.Append("Invalid heading format.\n");
        if (!colok) m.Append("Invalid number of columns.\n");
        if (linesok == -1) m.Append("File contains fewer lines than expected.\n");
        if (linesok == 1) m.Append("File contains more lines than expected.\n");
        wxMessageBox(m);

        return false;
    }
}

bool ImportSolPathMonthByHour(ShadingInputData &dat, wxWindow *parent) {
    wxFileDialog fdlg(parent, "Import Solar Pathfinder Month By Hour Shading File");
    if (fdlg.ShowModal() != wxID_OK) return false;
    wxString file = fdlg.GetPath();

    wxTextFile tf;
    if (!tf.Open(file)) {
        wxMessageBox("Could not open file for read:\n\n" + file);
        return false;
    }

    // read in month by hour grid from first image in shading file - Oliver Hellwig - Solar Pathfinder programmer - every half hour - read 2 value and average for now
    // 12:00 AM	12:30 AM	1:00 AM	1:30 AM	2:00 AM	2:30 AM	3:00 AM	3:30 AM	4:00 AM	4:30 AM	5:00 AM	5:30 AM	6:00 AM	6:30 AM	7:00 AM	7:30 AM	8:00 AM	8:30 AM	9:00 AM	9:30 AM	10:00 AM	10:30 AM	11:00 AM	11:30 AM	12:00 PM	12:30 PM	1:00 PM	1:30 PM	2:00 PM	2:30 PM	3:00 PM	3:30 PM	4:00 PM	4:30 PM	5:00 PM	5:30 PM	6:00 PM	6:30 PM	7:00 PM	7:30 PM	8:00 PM	8:30 PM	9:00 PM	9:30 PM	10:00 PM	10:30 PM	11:00 PM	11:30 PM
    // 12,24,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    // double array 12rowsx24columns Solar pathfinder percentages to fractions

    wxString buf;
    int i, imageCount = 0;
    bool readdata = false;
    bool readok = true;
    int month = 0; //
    double beam[290];
    for (i = 0; i < 290; i++) beam[i] = 0.0;
    beam[0] = 12;
    beam[1] = 24;

    buf = tf.GetFirstLine();

// data at half hour is recorded for hour in 8760 shading file - e.g. Jan-1 5:30 data recoded at hour 5
    while (!tf.Eof()) {
        wxArrayString lnp = wxStringTokenize(buf, ",", wxTOKEN_RET_EMPTY_ALL);
        if (readdata == false) {
            if (lnp.Count() > 0) {
                if (((std::string) (lnp.Item(0))).find("Image Layout Number ") == 0) {
                    imageCount++;
                    month = 0;
                    buf = tf.GetNextLine();
                    readdata = true;
                }
            }
        } else {
            if (month == 11) {
                readdata = false;
            }
            for (i = 0; i < 24; i++) {
                int ndex = i + month * 24 + 2; // skip 12x24 array size
                if (ndex > 289) {
                    readok = false;
                    break;
                }
                // average hour and half hour values starting at midnight (skip row label)
                if (imageCount == 1) {
                    beam[ndex] = 100 - (wxAtof(lnp.Item(2 * i + 1)) + wxAtof(lnp.Item(2 * i + 1 + 1))) /
                                       2.0;    //convert from a factor to a loss
                } else {
                    beam[ndex] += 100 - (wxAtof(lnp.Item(2 * i + 1)) + wxAtof(lnp.Item(2 * i + 1 + 1))) /
                                        2.0;    //convert from a factor to a loss
                }
            }
            month++;
        }
        buf = tf.GetNextLine();
        if (tf.Eof()) readdata = true;
    }

    if (readdata == false || imageCount == 0) {
        readok = false;
    }

    if (readok) {
        dat.clear();
        dat.en_mxh = true;
        dat.mxh.resize_fill(12, 24, 0.0);
        for (int r = 0; r < 12; r++)
            for (int c = 0; c < 24; c++)
                dat.mxh.at(r, c) = beam[24 * r + c + 2] / imageCount;
        return true;
    } else {
        wxString m = "Invalid file format.\n\n";
        wxMessageBox(m);
        return false;
    }
}


DEFINE_EVENT_TYPE(wxEVT_wxShadingFactorsCtrl_CHANGE)

//enum { ISFC_CHOICECOL = wxID_HIGHEST + 857, ISFC_CHOICEMINUTE, ISFC_CHOICESTRING, ISFC_GRID, ISFC_COPY, ISFC_PASTE, ISFC_IMPORT, ISFC_EXPORT };
enum {
    ISFC_CHOICECOL = wxID_HIGHEST + 857,
    ISFC_CHOICEMINUTE,
    ISFC_CHKDB,
    ISFC_GRID,
    ISFC_COPY,
    ISFC_PASTE,
    ISFC_IMPORT,
    ISFC_EXPORT
};

BEGIN_EVENT_TABLE(wxShadingFactorsCtrl, wxPanel)
                EVT_GRID_CMD_CELL_CHANGED(ISFC_GRID, wxShadingFactorsCtrl::OnCellChange)
                EVT_CHOICE(ISFC_CHOICECOL, wxShadingFactorsCtrl::OnChoiceCol)
                EVT_CHOICE(ISFC_CHOICEMINUTE, wxShadingFactorsCtrl::OnChoiceMinute)
                EVT_BUTTON(ISFC_COPY, wxShadingFactorsCtrl::OnCommand)
                EVT_CHECKBOX(ISFC_CHKDB, wxShadingFactorsCtrl::OnCommand)
                EVT_BUTTON(ISFC_PASTE, wxShadingFactorsCtrl::OnCommand)
                EVT_BUTTON(ISFC_EXPORT, wxShadingFactorsCtrl::OnCommand)
                EVT_BUTTON(ISFC_IMPORT, wxShadingFactorsCtrl::OnCommand)
END_EVENT_TABLE()

wxShadingFactorsCtrl::wxShadingFactorsCtrl(wxWindow *parent, int id,
                                           const wxPoint &pos,
                                           const wxSize &sz,
                                           bool show_db_options,
                                           bool sidebuttons)
        : wxPanel(parent, id, pos, sz) {
    m_default_val = 0;
    m_num_minutes = 60;
    m_grid_data = NULL;
    m_show_db_options = show_db_options;
    m_col_arystrvals.Clear();
    m_minute_arystrvals.Clear();
    //m_string_arystrvals.Clear();

    m_grid = new wxExtGridCtrl(this, ISFC_GRID);
    m_grid->CreateGrid(8760, 8);
    m_grid->EnableCopyPaste(true);
    m_grid->EnablePasteEvent(true);
    m_grid->DisableDragCell();
    m_grid->DisableDragRowSize();
    m_grid->DisableDragColMove();
    m_grid->DisableDragGridSize();
    m_grid->SetRowLabelAlignment(wxALIGN_LEFT, wxALIGN_CENTER);


//	m_string_arystrvals.push_back("Database lookup");
//	m_string_arystrvals.push_back("Average of strings");
//	m_string_arystrvals.push_back("Maximum of strings");
//	m_string_arystrvals.push_back("Minimum of strings");
//	m_choice_string_option = new wxChoice(this, ISFC_CHOICESTRING, wxDefaultPosition, wxDefaultSize, m_string_arystrvals);
//	m_choice_string_option->SetBackgroundColour(*wxWHITE);


    m_caption_col = new wxStaticText(this, wxID_ANY, "");
    m_caption_col->SetFont(*wxNORMAL_FONT);

    m_caption_shading_db = new wxStaticText(this, wxID_ANY, "");
    m_caption_shading_db->SetFont(*wxNORMAL_FONT);

    m_chk_shading_db = new wxCheckBox(this, ISFC_CHKDB, "");

//	m_caption_string = new wxStaticText(this, wxID_ANY, "");
//	m_caption_string->SetFont(*wxNORMAL_FONT);

    m_col_arystrvals.push_back("1");
    m_col_arystrvals.push_back("2");
    m_col_arystrvals.push_back("3");
    m_col_arystrvals.push_back("4");
    m_col_arystrvals.push_back("5");
    m_col_arystrvals.push_back("6");
    m_col_arystrvals.push_back("7");
    m_col_arystrvals.push_back("8");
    m_choice_col = new wxChoice(this, ISFC_CHOICECOL, wxDefaultPosition, wxDefaultSize, m_col_arystrvals);
    m_choice_col->SetBackgroundColour(*wxWHITE);

    m_caption_timestep = new wxStaticText(this, wxID_ANY, "");
    m_caption_timestep->SetFont(*wxNORMAL_FONT);

    m_minute_arystrvals.push_back("1");
    m_minute_arystrvals.push_back("3");
    m_minute_arystrvals.push_back("5");
    m_minute_arystrvals.push_back("10");
    m_minute_arystrvals.push_back("15");
    m_minute_arystrvals.push_back("30");
    m_minute_arystrvals.push_back("60");
    m_choice_timestep = new wxChoice(this, ISFC_CHOICEMINUTE, wxDefaultPosition, wxDefaultSize, m_minute_arystrvals);
    m_choice_timestep->SetBackgroundColour(*wxWHITE);


    m_btn_import = new wxButton(this, ISFC_IMPORT, "Import...");
    m_btn_export = new wxButton(this, ISFC_EXPORT, "Export...");
    m_btn_copy = new wxButton(this, ISFC_COPY, "Copy");
    m_btn_paste = new wxButton(this, ISFC_PASTE, "Paste");


    if (!show_db_options) {
        m_caption_col->Show(false);
        m_choice_col->Show(false);
//		m_caption_string->Show(false);
//		m_choice_string_option->Show(false);
    }


    if (sidebuttons) {
        // for side buttons layout
        wxBoxSizer *v_tb_sizer = new wxBoxSizer(wxVERTICAL);
        v_tb_sizer->Add(m_caption_timestep, 0, wxALL | wxEXPAND, 3);
        v_tb_sizer->Add(m_choice_timestep, 0, wxALL | wxEXPAND, 3);
        v_tb_sizer->AddSpacer(5);
        v_tb_sizer->Add(m_caption_shading_db, 0, wxALL | wxEXPAND, 3);
        v_tb_sizer->Add(m_chk_shading_db, 0, wxALL | wxEXPAND, 3);
        if (show_db_options) {
            v_tb_sizer->AddSpacer(5);
            v_tb_sizer->Add(m_caption_col, 0, wxALL | wxEXPAND, 3);
            v_tb_sizer->Add(m_choice_col, 0, wxALL | wxEXPAND, 3);
            //			v_tb_sizer->AddSpacer(5);
//			v_tb_sizer->Add(m_caption_string, 0, wxALL | wxEXPAND, 3);
//			v_tb_sizer->Add(m_choice_string_option, 0, wxALL | wxEXPAND, 3);
        }

        v_tb_sizer->AddSpacer(5);
        v_tb_sizer->Add(m_btn_copy, 0, wxALL | wxEXPAND, 3);
        v_tb_sizer->Add(m_btn_paste, 0, wxALL | wxEXPAND, 3);
        v_tb_sizer->Add(m_btn_import, 0, wxALL | wxEXPAND, 3);
        v_tb_sizer->Add(m_btn_export, 0, wxALL | wxEXPAND, 3);
        v_tb_sizer->AddStretchSpacer();

        wxBoxSizer *h_sizer = new wxBoxSizer(wxHORIZONTAL);
        h_sizer->Add(v_tb_sizer, 0, wxALL | wxEXPAND, 1);
        h_sizer->Add(m_grid, 1, wxALL | wxEXPAND, 1);

        SetSizer(h_sizer);
    } else {
        // for top buttons layout (default)
        wxBoxSizer *h_tb_sizer = new wxBoxSizer(wxHORIZONTAL);
        h_tb_sizer->Add(m_caption_timestep, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
        h_tb_sizer->Add(m_choice_timestep, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
        h_tb_sizer->AddSpacer(5);
        h_tb_sizer->Add(m_caption_shading_db, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
        h_tb_sizer->Add(m_chk_shading_db, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
        if (show_db_options) {
            h_tb_sizer->AddSpacer(5);
            h_tb_sizer->Add(m_caption_col, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
            h_tb_sizer->Add(m_choice_col, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
//			h_tb_sizer->AddSpacer(5);
//			h_tb_sizer->Add(m_caption_string, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
//			h_tb_sizer->Add(m_choice_string_option, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
        }
        /*
        h_tb_sizer->AddSpacer(5);
        h_tb_sizer->Add(m_btn_copy, 0,   wxALL, 3);
        h_tb_sizer->Add(m_btn_paste, 0,  wxALL, 3);
        h_tb_sizer->Add(m_btn_import, 0, wxALL, 3);
        h_tb_sizer->Add(m_btn_export, 0, wxALL, 3);
        */
        h_tb_sizer->AddStretchSpacer();

        wxBoxSizer *v_sizer = new wxBoxSizer(wxVERTICAL);
        v_sizer->Add(h_tb_sizer, 0, wxALL | wxEXPAND, 1);
        v_sizer->Add(m_grid, 1, wxALL | wxEXPAND, 1);

        // bottom buttons per Paul 4/4/16
        wxBoxSizer *h_bb_sizer = new wxBoxSizer(wxHORIZONTAL);
        h_bb_sizer->Add(m_btn_import, 0, wxALL, 3);
        h_bb_sizer->Add(m_btn_export, 0, wxALL, 3);
        h_bb_sizer->Add(new wxStaticLine(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxVERTICAL), 0,
                        wxALL | wxEXPAND, 1);
        h_bb_sizer->Add(m_btn_copy, 0, wxALL, 3);
        h_bb_sizer->Add(m_btn_paste, 0, wxALL, 3);
        h_bb_sizer->AddStretchSpacer();
        v_sizer->Add(h_bb_sizer, 0, wxALL | wxEXPAND, 1);

        SetSizer(v_sizer, false);
    }

}


void wxShadingFactorsCtrl::UpdateNumberColumns(size_t &new_cols) {
    // resize and preserve existing data and fill new data with default.
    m_data.resize_preserve(m_data.nrows(), new_cols, m_default_val);
    SetData(m_data);
}

void wxShadingFactorsCtrl::AverageCols() {
    size_t ncols = m_data.ncols();
    for (size_t nr = 0; nr < m_data.nrows(); nr++) {
        float avg = 0;
        for (size_t nc = 0; nc < ncols; nc++)
            avg += m_data.at(nr, nc);
        avg /= ncols;
        m_data.at(nr, 0) = avg;
    }
    m_data.resize_preserve(m_data.nrows(), 1, m_default_val);
    SetData(m_data);
    m_choice_col->SetSelection(0);
}


void wxShadingFactorsCtrl::UpdateNumberRows(size_t &new_rows) {
    // resize and preserve existing data and fill new data with default.
    m_data.resize_preserve(new_rows, m_data.ncols(), m_default_val);
    SetData(m_data);
}

void wxShadingFactorsCtrl::UpdateNumberMinutes(size_t &new_timesteps) {
    // resize and preserve existing data and fill new data with default.
    // multiple of 8760 timesteps to number of timesteps
    if ((new_timesteps > 0) && (new_timesteps <= 60)) {
        size_t new_rows = 60 / new_timesteps * 8760;
        m_data.resize_preserve(new_rows, m_data.ncols(), m_default_val);
        SetData(m_data);
    }
}

void wxShadingFactorsCtrl::OnCommand(wxCommandEvent &evt) {
    switch (evt.GetId()) {
        case ISFC_CHKDB: {
            bool bol_db = m_chk_shading_db->GetValue();
            m_caption_col->Show(bol_db);
            m_choice_col->Show(bol_db);
            if (!bol_db)
                AverageCols();
        }
            break;
        case ISFC_COPY:
            m_grid->Copy(true);
            break;
        case ISFC_PASTE: {
            // resize rows per data pasted
            if (wxTheClipboard->Open()) {
                wxString data;
                wxTextDataObject textobj;
                if (wxTheClipboard->GetData(textobj)) {
                    data = textobj.GetText();
                    wxTheClipboard->Close();
                }
                if (data.IsEmpty()) return;

#ifdef __WXMAC__
                wxArrayString lines = wxStringTokenize(data, "\r", ::wxTOKEN_RET_EMPTY_ALL);
#else
                wxArrayString lines = wxStringTokenize(data, "\n", ::wxTOKEN_RET_EMPTY_ALL);
#endif
                int ncols = m_grid->GetNumberCols();

                if (m_show_db_options && (lines.Count() > 0)) {
                    wxArrayString col_vals1 = wxStringTokenize(lines[0], "\t", ::wxTOKEN_RET_EMPTY_ALL);
                    ncols = col_vals1.Count();
                    if (ncols > (int) m_col_arystrvals.Count())
                        ncols = m_col_arystrvals.Count();
                    int ndx = m_col_arystrvals.Index(wxString::Format("%d", ncols));
                    if (ndx >= 0)
                        m_choice_col->SetSelection(ndx);
                }
                int nrows = lines.Count() - 1;

                if ((nrows == 0) || (nrows % 8760 != 0)) return;
                int minutes = 60 / (nrows / 8760);
                if (!IsValidMinutes(minutes)) return;


                m_grid->ResizeGrid(nrows, ncols);
                m_data.resize_preserve(nrows, ncols, 0.0);

                m_grid->Paste(wxExtGridCtrl::PASTE_ALL);

                for (size_t r = 0; r < m_data.nrows(); r++)
                    for (size_t c = 0; c < m_data.ncols(); c++)
                        m_data.at(r, c) = atof(m_grid->GetCellValue(r, c).c_str());
                SetData(m_data);
                int ndx = m_minute_arystrvals.Index(wxString::Format("%d", minutes));
                if (ndx >= 0)
                    m_choice_timestep->SetSelection(ndx);
                m_chk_shading_db->SetValue(true);
                m_caption_col->Show(true);
                m_choice_col->Show(true);
            }

        }
            break;
        case ISFC_IMPORT: {
            wxFileDialog dlg(this, "Select data matrix file to import");
            if (dlg.ShowModal() == wxID_OK) {
                if (!Import(dlg.GetPath()))
                    wxMessageBox("Error import data file:\n\n" + dlg.GetPath());
                else {
                    m_chk_shading_db->SetValue(true);
                    m_caption_col->Show(true);
                    m_choice_col->Show(true);
                }
            }
        }
            break;
        case ISFC_EXPORT: {
            wxFileDialog dlg(this, "Select file for data export", wxEmptyString, wxEmptyString,
                             wxFileSelectorDefaultWildcardStr, wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
            if (dlg.ShowModal() == wxID_OK)
                if (!Export(dlg.GetPath()))
                    wxMessageBox("Error exporting data to file:\n\n" + dlg.GetPath());
        }
            break;
    }
}


bool wxShadingFactorsCtrl::Export(const wxString &file) {
    wxCSVData csv;
    for (size_t r = 0; r < m_data.nrows(); r++)
        for (size_t c = 0; c < m_data.ncols(); c++)
            csv(r, c) = wxString::Format("%g", m_data(r, c));

    return csv.WriteFile(file);
}

bool wxShadingFactorsCtrl::IsValidMinutes(int &minutes) {
    return (m_minute_arystrvals.Index(wxString::Format("%d", minutes)) != wxNOT_FOUND);
}

bool wxShadingFactorsCtrl::Import(const wxString &file) {
    wxCSVData csv;
    if (!csv.ReadFile(file)) return false;

    int nrows = csv.NumRows();
    if ((nrows == 0) || (nrows % 8760 != 0)) return false;
    int minutes = 60 / (nrows / 8760);

    if (!IsValidMinutes(minutes)) return false;

    int ncols = m_grid->GetNumberCols();

    if (m_show_db_options && (csv.NumCols() > 0)) {
        ncols = csv.NumCols();
        if (ncols > (int) m_col_arystrvals.Count())
            ncols = m_col_arystrvals.Count();
        int ndx = m_col_arystrvals.Index(wxString::Format("%d", ncols));
        if (ndx >= 0)
            m_choice_col->SetSelection(ndx);
    }

    m_grid->ResizeGrid(nrows, ncols);
    m_data.resize_preserve(nrows, ncols, 0.0f);


    for (size_t r = 0; r < m_data.nrows(); r++)
        for (size_t c = 0; c < m_data.ncols(); c++)
            m_data.at(r, c) = (float) wxAtof(csv(r, c));

    SetData(m_data);
    int ndx = m_minute_arystrvals.Index(wxString::Format("%d", minutes));
    if (ndx >= 0)
        m_choice_timestep->SetSelection(ndx);

    return true;
}


void wxShadingFactorsCtrl::OnChoiceCol(wxCommandEvent &) {
    if ((m_choice_col->GetSelection() != wxNOT_FOUND) &&
        (wxAtoi(m_choice_col->GetString(m_choice_col->GetSelection())) != (int) m_data.ncols())) {
        size_t new_cols = wxAtoi(m_choice_col->GetString(m_choice_col->GetSelection()));
        UpdateNumberColumns(new_cols);
    }
}

void wxShadingFactorsCtrl::OnChoiceMinute(wxCommandEvent &) {
    if ((m_choice_timestep->GetSelection() != wxNOT_FOUND) &&
        (wxAtoi(m_choice_timestep->GetString(m_choice_timestep->GetSelection())) != (int) m_num_minutes)) {
        m_num_minutes = wxAtoi(m_choice_timestep->GetString(m_choice_timestep->GetSelection()));
        UpdateNumberMinutes(m_num_minutes);
    }
}

void wxShadingFactorsCtrl::SetData(const matrix_t<double> &mat) {
    m_data = mat;

    if (m_grid_data) m_grid_data->SetMatrix(NULL);
    m_grid->SetTable(NULL);

    m_grid_data = new wxShadingFactorsTable(&m_data, m_default_val);
    m_grid_data->SetAttrProvider(new wxExtGridCellAttrProvider);

    m_grid->SetTable(m_grid_data, true);

    m_grid->Layout();
    m_grid->Refresh();
}

void wxShadingFactorsCtrl::GetData(matrix_t<double> &mat) {
    mat = m_data;
}


void wxShadingFactorsCtrl::OnCellChange(wxGridEvent &evt) {
    int irow = evt.GetRow();
    int icol = evt.GetCol();

    if (irow == -1 && icol == -1) // paste event generated from base class
    {
        for (int ir = 0; ir < m_grid->GetNumberRows(); ir++)
            for (int ic = 0; ic < m_grid->GetNumberCols(); ic++) {
                float val = (float) wxAtof(m_grid->GetCellValue(ir, ic).c_str());
                m_data.at(ir, ic) = val;
                m_grid->SetCellValue(ir, ic, wxString::Format("%g", val));
            }
    } else {
        float val = (float) wxAtof(m_grid->GetCellValue(irow, icol).c_str());

        if (irow < (int) m_data.nrows() && icol < (int) m_data.ncols()
            && irow >= 0 && icol >= 0)
            m_data.at(irow, icol) = val;

        m_grid->SetCellValue(irow, icol, wxString::Format("%g", val));
    }
    wxCommandEvent dmcevt(wxEVT_wxShadingFactorsCtrl_CHANGE, this->GetId());
    dmcevt.SetEventObject(this);
    GetEventHandler()->ProcessEvent(dmcevt);
}


void wxShadingFactorsCtrl::SetColCaption(const wxString &cap) {
    m_caption_col->SetLabel(cap);
    this->Layout();
}

wxString wxShadingFactorsCtrl::GetColCaption() {
    return m_caption_col->GetLabel();
}

/*
void wxShadingFactorsCtrl::SetStringCaption(const wxString &cap)
{
	m_caption_string->SetLabel(cap);
	this->Layout();
}

wxString wxShadingFactorsCtrl::GetStringCaption()
{
	return m_caption_string->GetLabel();
}
*/

void wxShadingFactorsCtrl::SetMinuteCaption(const wxString &cap) {
    m_caption_timestep->SetLabel(cap);
    this->Layout();
}

wxString wxShadingFactorsCtrl::GetMinuteCaption() {
    return m_caption_timestep->GetLabel();
}

void wxShadingFactorsCtrl::SetDBCaption(const wxString &cap) {
    m_caption_shading_db->SetLabel(cap);
    this->Layout();
}

wxString wxShadingFactorsCtrl::GetDBCaption() {
    return m_caption_shading_db->GetLabel();
}


void wxShadingFactorsCtrl::SetNumMinutes(size_t &minutes) {
    int ndx = m_minute_arystrvals.Index(wxString::Format("%d", (int) minutes));
    if (ndx >= 0)
        m_choice_timestep->SetSelection(ndx);
    UpdateNumberMinutes(minutes);
}

void wxShadingFactorsCtrl::SetNumCols(size_t &cols) {
    int ndx = m_col_arystrvals.Index(wxString::Format("%d", (int) cols));
    if (ndx >= 0)
        m_choice_col->SetSelection(ndx);
    UpdateNumberColumns(cols);
}

void wxShadingFactorsCtrl::SetDBOption(int &db_option) {
    // keep compatibility with shading database = 0 choice
    bool bol_shade_db = (db_option == 0);
    m_chk_shading_db->SetValue(bol_shade_db);
    m_caption_col->Show(bol_shade_db);
    m_choice_col->Show(bol_shade_db);
}

int wxShadingFactorsCtrl::GetDBOption() {
    // keep compatibility with shading database = 0 choice
    if (m_chk_shading_db->GetValue())
        return 0;
    else
        return 1; // average
}

/*
void wxShadingFactorsCtrl::SetStringOption(int &string_option)
{
	if (string_option >= 0 && string_option < (int)m_string_arystrvals.Count())
		m_choice_string_option->SetSelection(string_option);
	else
		m_choice_string_option->SetSelection(0); // default
}

int wxShadingFactorsCtrl::GetStringOption()
{
	if (m_choice_string_option->IsShown())
		return m_choice_string_option->GetSelection();
	else
		return -1; // no string options
}
*/

wxShadingFactorsTable::wxShadingFactorsTable(matrix_t<double> *da, float _def_val, const wxString &_label) {
    label = _label;
    d_mat = da;
    def_val = _def_val;
}

void wxShadingFactorsTable::SetMatrix(matrix_t<double> *da) {
    d_mat = da;
}

int wxShadingFactorsTable::GetNumberRows() {
    if (!d_mat) return 0;

    return (int) d_mat->nrows();
}

int wxShadingFactorsTable::GetNumberCols() {
    if (!d_mat) return 0;

    return (int) d_mat->ncols();
}

bool wxShadingFactorsTable::IsEmptyCell(int, int) {
    return false;
}

wxString wxShadingFactorsTable::GetValue(int row, int col) {
    if (d_mat && row >= 0 && row < (int) d_mat->nrows() && col >= 0 && col < (int) d_mat->ncols())
        return wxString::Format("%g", d_mat->at(row, col));
    else
        return "-0.0";
}

void wxShadingFactorsTable::SetValue(int row, int col, const wxString &value) {
    if (d_mat && row >= 0 && row < (int) d_mat->nrows() && col >= 0 && col < (int) d_mat->ncols())
        d_mat->at(row, col) = wxAtof(value);
}

wxString wxShadingFactorsTable::GetRowLabelValue(int row) {
    if (d_mat) {
        int nmult = d_mat->nrows() / 8760;
        if (nmult != 0) {
            double step = 1.0 / ((double) nmult);
            double tm = step * (row + 1);
            double frac = tm - ((double) (int) tm);
            if (frac == 0.0)
                return wxString::Format("%lg", tm);
            else
                return wxString::Format("   .%lg", frac * 60);
        }
    }

    return wxString::Format("%d", row + 1);
}

wxString wxShadingFactorsTable::GetColLabelValue(int col) {
    wxString col_label = label.IsEmpty() ? "Value" : label;
    if (d_mat->ncols() > 1)
        col_label = wxString::Format("String %d", col + 1);
    return col_label;
}

wxString wxShadingFactorsTable::GetTypeName(int, int) {
    return wxGRID_VALUE_STRING;
}

bool wxShadingFactorsTable::CanGetValueAs(int, int, const wxString &typeName) {
    return typeName == wxGRID_VALUE_STRING;
}

bool wxShadingFactorsTable::CanSetValueAs(int, int, const wxString &typeName) {
    return typeName == wxGRID_VALUE_STRING;
}

bool wxShadingFactorsTable::AppendRows(size_t nrows) {
    if (d_mat && nrows > 0) {
        size_t new_rows = d_mat->nrows() + nrows;
        d_mat->resize_preserve(new_rows, d_mat->ncols(), def_val);

        if (GetView()) {
            wxGridTableMessage msg(this,
                                   wxGRIDTABLE_NOTIFY_ROWS_APPENDED,
                                   nrows);

            GetView()->ProcessTableMessage(msg);
        }
    }

    return true;
}

bool wxShadingFactorsTable::InsertRows(size_t pos, size_t nrows) {

    if (!d_mat) return true;

    if (pos > d_mat->nrows()) pos = d_mat->nrows();

    size_t new_rows = d_mat->nrows() + nrows;
    matrix_t<double> old(*d_mat);
    d_mat->resize_fill(new_rows, d_mat->ncols(), def_val);

    for (size_t r = 0; r < pos && r < old.nrows(); r++)
        for (size_t c = 0; c < old.ncols(); c++)
            d_mat->at(r, c) = old(r, c);

    // r-nrows>=0 since pos>=0
    for (size_t r = pos + nrows; r < new_rows && r - nrows < old.nrows(); r++)
        for (size_t c = 0; c < old.ncols(); c++)
            d_mat->at(r, c) = old(r - nrows, c);

    if (GetView()) {
        wxGridTableMessage msg(this,
                               wxGRIDTABLE_NOTIFY_ROWS_INSERTED,
                               pos,
                               nrows);

        GetView()->ProcessTableMessage(msg);
    }

    return true;
}

bool wxShadingFactorsTable::DeleteRows(size_t pos, size_t nrows) {
    if (!d_mat) return true;

    if (nrows > d_mat->nrows() - pos)
        nrows = d_mat->nrows() - pos;

    size_t new_rows = d_mat->nrows() - nrows;
    matrix_t<double> old(*d_mat);
    d_mat->resize_preserve(new_rows, d_mat->ncols(), def_val);

    for (size_t r = pos; r < new_rows && r + nrows < old.nrows(); r++)
        for (size_t c = 0; c < old.ncols(); c++)
            d_mat->at(r, c) = old(r + nrows, c);

    if (GetView()) {
        wxGridTableMessage msg(this,
                               wxGRIDTABLE_NOTIFY_ROWS_DELETED,
                               pos,
                               nrows);

        GetView()->ProcessTableMessage(msg);
    }

    return true;
}

bool wxShadingFactorsTable::AppendCols(size_t ncols) {
    if (d_mat && ncols > 0) {
        size_t new_cols = d_mat->ncols() + ncols;
        d_mat->resize_preserve(d_mat->nrows(), new_cols, def_val);

        if (GetView()) {
            wxGridTableMessage msg(this,
                                   wxGRIDTABLE_NOTIFY_COLS_APPENDED,
                                   ncols);

            GetView()->ProcessTableMessage(msg);
        }
    }

    return true;
}

bool wxShadingFactorsTable::InsertCols(size_t pos, size_t ncols) {

    if (!d_mat) return true;

    if (pos > d_mat->ncols()) pos = d_mat->ncols();

    size_t new_cols = d_mat->ncols() + ncols;
    matrix_t<double> old(*d_mat);
    d_mat->resize_fill(d_mat->nrows(), new_cols, def_val);

    for (size_t r = 0; r < old.nrows(); r++)
        for (size_t c = 0; c < pos && c < old.ncols(); c++)
            d_mat->at(r, c) = old(r, c);

    // r-ncols>=0 since pos>=0
    for (size_t r = 0; r < old.nrows(); r++)
        for (size_t c = pos + ncols; c < new_cols && r - ncols < old.ncols(); c++)
            d_mat->at(r, c) = old(r, c - ncols);

    if (GetView()) {
        wxGridTableMessage msg(this,
                               wxGRIDTABLE_NOTIFY_COLS_INSERTED,
                               pos,
                               ncols);

        GetView()->ProcessTableMessage(msg);
    }

    return true;
}

bool wxShadingFactorsTable::DeleteCols(size_t pos, size_t ncols) {
    if (!d_mat) return true;

    if (ncols > d_mat->ncols() - pos)
        ncols = d_mat->ncols() - pos;

    size_t new_cols = d_mat->ncols() - ncols;
    matrix_t<double> old(*d_mat);
    d_mat->resize_preserve(d_mat->nrows(), new_cols, def_val);

    for (size_t r = pos; r < old.nrows(); r++)
        for (size_t c = pos; c < new_cols && c + ncols < old.nrows(); c++)
            d_mat->at(r, c) = old(r, c + ncols);

    if (GetView()) {
        wxGridTableMessage msg(this,
                               wxGRIDTABLE_NOTIFY_COLS_DELETED,
                               pos,
                               ncols);

        GetView()->ProcessTableMessage(msg);
    }

    return true;
}


