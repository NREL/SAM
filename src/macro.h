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

#ifndef __macro_h
#define __macro_h

#include <memory>

#include <wx/arrstr.h>
#include <wx/dir.h>
#include <wx/dirctrl.h>
#include <wx/dirdlg.h>
#include <wx/file.h>
#include <wx/filefn.h>
#include <wx/html/htmlwin.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/splitter.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>
#include <wx/wfstream.h>

#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/stdlib.h>
#include <lk/codegen.h>
#include <lk/vm.h>

#include <wex/metro.h>
#include <wex/utils.h>
#include <wex/lkscript.h>
#include <wex/uiform.h>

#include "script.h"
#include "main.h"
#include "casewin.h"

class wxMetroButton;

class wxMetroListBox;

class wxTextCtrl;

class wxHtmlWindow;

class wxPanel;

class Case;

class wxStaticText;


class MacroEngine {

public:
    MacroEngine();

    virtual ~MacroEngine();

    // note: the args variable should be heap allocated with 'new'
    // and will be owned and deleted by this Run method
    bool Run(const wxString &script, lk::vardata_t *args = 0);

    void Stop();

    virtual void Output(const wxString &text);

    virtual void ClearOutput();

    virtual bool IsStopFlagSet();

    static wxArrayString ListMacrosForConfiguration(const wxString &tech, const wxString &fin);

private:
    bool m_stopFlag;
};

class MacroPanel : public wxSplitterWindow, public MacroEngine {
public:
    MacroPanel(wxWindow *parent, Case *cc);

    void ConfigurationChanged();

    virtual void Output(const wxString &text);

    virtual void ClearOutput();

    lk::vardata_t *GetUIArgs();

    void GetUIArgs(lk::vardata_t &table);

    int SetUIArgs(lk::vardata_t &table);

    int ReadUIData(const wxString &file);

    bool WriteUIData(const wxString &file);

private:
    void OnCommand(wxCommandEvent &);

    void OnHtmlLink(wxHtmlLinkEvent &);

    void UpdateHtml();

    Case *m_case;
    wxMetroListBox *m_listbox;
    wxHtmlWindow *m_html;
    wxString m_curMacroPath;
    wxTextCtrl *m_output;
    wxArrayString m_macroList;
    wxMetroButton *m_run, *m_stop, *m_code;
    wxPanel *m_leftPanel;
    wxPanel *m_rightPanel;

    struct ui_item {
        wxString name;
        wxStaticText *label;
        wxWindow *window;
    };

    std::vector<ui_item> m_ui;
    wxPanel *m_macroUI;
    wxFlexGridSizer *m_macroUISizer;

    void ClearUI();

    void CreateUI(const wxString &buf);

    ui_item *FindItem(const wxString &name);

DECLARE_EVENT_TABLE();

};

class FileNameInputCtrl : public wxPanel {
public:
    FileNameInputCtrl(wxWindow *parent, const wxString &_label, const wxString &_filename,
                      const wxString &_filter = "csv,txt");

    wxString GetFileName() {
        return m_fileName;
    }

private:

    void OnSelect(wxCommandEvent &);

    wxTextCtrl *m_text;
    wxString m_fileName, m_filter, m_label, m_dir;

DECLARE_EVENT_TABLE();
};

class FolderNameInputCtrl : public wxPanel {
public:
    FolderNameInputCtrl(wxWindow *parent, const wxString &_label, const wxString &folderName);

    wxString GetFolderName() {
        return m_dir;
    }

private:

    void OnSelect(wxCommandEvent &);

    wxTextCtrl *m_text;
    wxString m_dir, m_label;

DECLARE_EVENT_TABLE();
};


#endif
