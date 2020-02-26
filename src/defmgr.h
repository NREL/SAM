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

#ifndef __defmgr_h
#define __defmgr_h

#include <wx/panel.h>
#include <wx/arrstr.h>

class wxTextCtrl;

class wxCheckListBox;

class wxCheckBox;

class wxButton;

class wxStaticTextCtrl;

class wxChoice;

class AFDataMatrixCtrl;

class ValueEditor : public wxPanel {
public:
    ValueEditor(wxWindow *parent);

    int GetType();

    void Set(const VarValue &vv);

    VarValue Get();

private:
    wxChoice *m_type;
    wxTextCtrl *m_text;
    AFDataMatrixCtrl *m_matrix;
    wxListBox *m_fields;
    wxButton *m_addField, *m_removeField, *m_editField, *m_clearTable;
    wxStaticText *m_valLabel, *m_tabLabel;

    void ValueToForm();

    void UpdateFormUI();

    void OnCommand(wxCommandEvent &);

    void OnEditField(wxCommandEvent &);

    VarValue m_val;

DECLARE_EVENT_TABLE();
};


class DefaultsManager : public wxPanel {
public:
    DefaultsManager(wxWindow *parent);

private:
    wxTextCtrl *m_varName;
    ValueEditor *m_value;

    wxTextCtrl *m_output;
    wxCheckListBox *m_configList;
    wxArrayString m_techList, m_finList;
    wxCheckBox *m_changeType;
    wxCheckBox *m_enableAdd;


    void ClearLog();

    void Log(const wxString &s);

    void OnQuery(wxCommandEvent &evt);

    void OnModify(wxCommandEvent &evt);

    void OnLoad(wxCommandEvent &evt);

    void OnLookupVar(wxCommandEvent &evt);

    void OnDeleteVar(wxCommandEvent &evt);

    void OnPopupMenu(wxCommandEvent &evt);

    void OnListRightClick(wxMouseEvent &evt);

    void OnSaveAsType(wxCommandEvent &evt);

    wxString LookupVariable();

DECLARE_EVENT_TABLE();
};

#endif
