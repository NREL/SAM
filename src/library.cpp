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

#include <vector>
#include <algorithm>

#include <wx/wx.h>
#include <wx/busyinfo.h>
#include <wx/filename.h>
#include <wx/dir.h>
#include <wx/tokenzr.h>

#include <wex/utils.h>

#include <ssc/sscapi.h>

#include "library.h"
#include "object.h"
#include "main.h"


enum {
    ID_ADD_FOLDER = wxID_HIGHEST + 1553,
    ID_REMOVE_FOLDER,
    ID_SELECT_FOLDER
};

class SettingsDialog : public wxDialog {
private:
    wxListBox *m_pathListBox;
    wxTextCtrl *m_downloadPath;
public:
    SettingsDialog(wxWindow *parent, const wxString &title, const wxString &data_file_name = "Weather File")
            : wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxScaleSize(400, 450),
                       wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER) {
        wxSizer *szmain = new wxBoxSizer(wxVERTICAL);
        wxSizer *sizer_wfpaths = new wxStaticBoxSizer(wxVERTICAL, this, data_file_name + " Folders");
        wxSizer *sizer_dnfolder = new wxStaticBoxSizer(wxHORIZONTAL, this,
                                                       "Folder for Downloaded " + data_file_name + "s");

        m_pathListBox = new wxListBox(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, 0, 0, wxLB_SINGLE);
        sizer_wfpaths->Add(m_pathListBox, 1, wxALL | wxEXPAND, 4);
        wxBoxSizer *szbuttons1 = new wxBoxSizer(wxHORIZONTAL);
        szbuttons1->AddStretchSpacer();
        szbuttons1->Add(new wxButton(this, ID_ADD_FOLDER, "Add"), 0, wxALL | wxEXPAND, 4);
        szbuttons1->Add(new wxButton(this, ID_REMOVE_FOLDER, "Remove"), 0, wxALL | wxEXPAND, 4);
        sizer_wfpaths->Add(szbuttons1, 0, wxALL | wxEXPAND, 4);

        m_downloadPath = new wxTextCtrl(this, wxID_ANY, wxEmptyString);
        sizer_dnfolder->Add(m_downloadPath, 1, wxALL | wxEXPAND, 4);
        sizer_dnfolder->Add(
                new wxButton(this, ID_SELECT_FOLDER, " ... ", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0,
                wxALL | wxEXPAND, 4);

        szmain->Add(sizer_wfpaths, 1, wxALL | wxEXPAND, 8);
        szmain->Add(sizer_dnfolder, 0, wxALL | wxEXPAND, 8);
        szmain->Add(CreateButtonSizer(wxOK | wxCANCEL | wxHELP), 0, wxALL | wxEXPAND, 10);

        SetSizer(szmain);
    }

    void OnCommand(wxCommandEvent &evt) {
        wxString dir;
        switch (evt.GetId()) {
            case ID_ADD_FOLDER:
                dir = wxDirSelector("Choose a folder");
                if (!dir.IsEmpty() && m_pathListBox->FindString(dir) < 0)
                    m_pathListBox->Append(dir);
                break;

            case ID_REMOVE_FOLDER:
                if (m_pathListBox->GetSelection() >= 0)
                    m_pathListBox->Delete(m_pathListBox->GetSelection());
                break;

            case ID_SELECT_FOLDER:
                dir = wxDirSelector("Choose folder for downloaded weather files", m_downloadPath->GetValue());
                if (!dir.empty())
                    m_downloadPath->SetValue(dir);
                break;

            case wxID_HELP:
                SamApp::ShowHelp("weather_manage_folders");
                break;
        }
    }

    void SetLibraryPaths(const wxArrayString &list) {
        m_pathListBox->Clear();
        m_pathListBox->Append(list);
    }

    wxArrayString GetLibraryPaths() {
        return m_pathListBox->GetStrings();
    }

    void SetDownloadPath(const wxString &path) {
        m_downloadPath->ChangeValue(path);
    }

    wxString GetDownloadPath() {
        return m_downloadPath->GetValue();
    }

DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(SettingsDialog, wxDialog)
                EVT_BUTTON(ID_ADD_FOLDER, SettingsDialog::OnCommand)
                EVT_BUTTON(ID_REMOVE_FOLDER, SettingsDialog::OnCommand)
                EVT_BUTTON(ID_SELECT_FOLDER, SettingsDialog::OnCommand)
                EVT_BUTTON(wxID_HELP, SettingsDialog::OnCommand)
END_EVENT_TABLE()


class LibManager {
public:
    LibManager() {}

    ~LibManager() {
        DeleteAll();
    }

    void DeleteAll() {
        for (size_t i = 0; i < m_libs.size(); i++)
            delete m_libs[i];
        m_libs.clear();
    }

    std::vector<Library *> m_libs;
};

static LibManager gs_libs;

Library *Library::Load(const wxString &file) {
    Library *l = new Library;
    if (l->Read(file)) {
        // replace an old library if the new
        // one has the same name
        if (Library *old = Find(l->GetName())) {
            std::vector<Library *>::iterator it = std::find(gs_libs.m_libs.begin(), gs_libs.m_libs.end(), old);
            if (it != gs_libs.m_libs.end()) {
                gs_libs.m_libs.erase(it);
                delete old;
            }
        }
        // set case sensitivty for library entries comparison by name
        if (l->GetName() == "CEC Modules")
            l->caseSensitiveFind = false;
        gs_libs.m_libs.push_back(l);
        return l;
    } else {
        delete l;
        return 0;
    }
}

Library *Library::Find(const wxString &name) {
    for (size_t i = 0; i < gs_libs.m_libs.size(); i++)
        if (name == gs_libs.m_libs[i]->GetName())
            return gs_libs.m_libs[i];
    return 0;
}

void Library::UnloadAll() {
    gs_libs.DeleteAll();
}

wxArrayString Library::ListAll() {
    wxArrayString list;
    for (size_t i = 0; i < gs_libs.m_libs.size(); i++)
        list.Add(gs_libs.m_libs[i]->GetName());
    return list;
}

Library::Library() {
    m_startRow = 0;
}

bool Library::Read(const wxString &file) {
    if (!m_csv.ReadFile(file))
        return false;

    m_name = wxFileName(file).GetName();

    return ScanData();

}

bool Library::Read(const wxCSVData &data, const wxString &name) {
    m_csv = data;
    m_name = name;
    return ScanData();
}

bool Library::ScanData() {
    m_errors.clear();

    m_startRow = 2;
    while (m_startRow < m_csv.NumRows()
           && !m_csv(m_startRow, 0).IsEmpty()
           && m_csv(m_startRow, 0)[0] == '[')
        m_startRow++;

    if (m_startRow >= m_csv.NumRows())
        return false;

    size_t nvarlists = m_startRow - 2;

    if (nvarlists < 1)
        return false;

    m_fields.clear();
    size_t ncol = m_csv.NumCols();
    for (size_t i = 0; i < ncol; i++) {
        Field f;
        if (i == 0) f.Name = wxT("Name");
        else {
            f.Name = m_csv(0, i);
            f.Units = m_csv(1, i);
        }

        for (size_t j = 0; j < nvarlists; j++)
            f.Variables.Add(m_csv(2 + j, i));

        f.DataIndex = i;

        m_fields.push_back(f);
    }

    return true;
}

wxString Library::GetName() const {
    return m_name;
}

wxArrayString Library::ListEntries() {
    wxArrayString list;
    size_t r = m_csv.NumRows();
    for (size_t i = m_startRow; i < r; i++)
        list.Add(m_csv(i, 0));
    return list;
}

size_t Library::NumEntries() {
    return m_csv.NumRows() - m_startRow;
}

std::vector<Library::Field> &Library::GetFields() {
    return m_fields;
}

int Library::GetFieldIndex(const wxString &name) {
    for (size_t i = 0; i < m_fields.size(); i++)
        if (m_fields[i].Name.Lower() == name.Lower())
            return i;

    return -1;
}

int Library::FindEntry(const wxString &name) {
    size_t r = m_csv.NumRows();
    for (size_t i = m_startRow; i < r; i++)
        if (name.IsSameAs(m_csv(i, 0), caseSensitiveFind))
            return (int) (i - m_startRow);

    return -1;
}

wxString Library::GetEntryValue(int entry, int field) {
    return m_csv(entry + m_startRow, field);
}

wxString Library::GetEntryName(int entry) {
    return GetEntryValue(entry, 0);
}

bool Library::ApplyEntry(int entry, int varindex, VarTable &tab, wxArrayString &changed) {
    m_errors.Clear();

    if (varindex < 0 || varindex >= (int) m_startRow - 2) {
        m_errors.Add(wxString::Format("invalid varindex of %d", varindex));
        return false;
    }

    size_t row = m_startRow + (size_t) entry;
    if (row >= m_csv.NumRows() || row < m_startRow) {
        m_errors.Add(wxString::Format("invalid entry %d (max %d)", entry, (int) (m_csv.NumRows() - m_startRow)));
        return false;
    }

    for (size_t i = 1; i < m_fields.size(); i++) {
        Field &f = m_fields[i];

        wxString var;
        if (varindex < (int) f.Variables.size())
            var = f.Variables[varindex];

        if (var.IsEmpty()) continue; // skip this variable if no name was found

        if (VarValue *vv = tab.Get(var)) {
            if (VarValue::Parse(vv->Type(), m_csv(row, f.DataIndex), *vv))
                changed.Add(var);
            else
                m_errors.Add("could not parse '" + var + "' to required data type");
        } else
            m_errors.Add("variable '" + var + "' not found in collection");
    }

    return m_errors.Count() == 0;
}


LibraryListView::LibraryListView(LibraryCtrl *parent, int id, const wxPoint &pos,
                                 const wxSize &size)
        : wxListView(parent, id, pos, size, wxLC_REPORT | wxLC_VIRTUAL | wxLC_SINGLE_SEL) {
    m_libctrl = parent;
}

wxString LibraryListView::OnGetItemText(long item, long col) const {
    return m_libctrl->GetCellValue(item, col);
}

/*
wxListItemAttr *LibraryListView::OnGetItemAttr( long item ) const
{
	static wxListItemAttr even( *wxBLACK, *wxWHITE, *wxNORMAL_FONT ),
		odd( *wxBLACK, wxColour(245,245,245), *wxNORMAL_FONT );

	return (item%2==0) ? &even : &odd;
}
*/

enum {
    ID_LIST = wxID_HIGHEST + 495, ID_FILTER, ID_TARGET, ID_REFRESH
};

BEGIN_EVENT_TABLE(LibraryCtrl, wxPanel)
                EVT_LIST_ITEM_SELECTED(ID_LIST, LibraryCtrl::OnSelected)
                EVT_LIST_COL_CLICK(ID_LIST, LibraryCtrl::OnColClick)
                EVT_TEXT(ID_FILTER, LibraryCtrl::OnCommand)
                EVT_BUTTON(ID_REFRESH, LibraryCtrl::OnCommand)
                EVT_CHOICE(ID_TARGET, LibraryCtrl::OnCommand)
END_EVENT_TABLE()


LibraryCtrl::LibraryCtrl(wxWindow *parent, int id, const wxPoint &pos, const wxSize &size)
        : wxPanel(parent, id, pos, size, wxTAB_TRAVERSAL) {
    SetBackgroundColour(*wxWHITE);

    m_sendEvents = true;
    m_nmatches = 0;

    m_label = new wxStaticText(this, wxID_ANY, wxT("Filter:"));
    m_filter = new wxTextCtrl(this, ID_FILTER);
    m_target = new wxChoice(this, ID_TARGET);
    m_notify = new wxStaticText(this, wxID_ANY, wxEmptyString);
    m_notify->SetForegroundColour(*wxRED);
    m_list = new LibraryListView(this, ID_LIST);

    wxBoxSizer *sz_horiz = new wxBoxSizer(wxHORIZONTAL);
    sz_horiz->Add(m_label, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);
    sz_horiz->Add(m_filter, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
    sz_horiz->Add(m_target, 0, wxALL | wxALIGN_CENTER_VERTICAL, 3);
//	sz_horiz->AddStretchSpacer();
    sz_horiz->Add(m_notify, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

//sz_horiz->Add( new wxButton( this, ID_REFRESH, wxT("Refresh list") ), 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );

    wxBoxSizer *sz_vert = new wxBoxSizer(wxVERTICAL);
    sz_vert->Add(sz_horiz, 0, wxALL | wxEXPAND, 0);
    sz_vert->Add(m_list, 1, wxALL | wxEXPAND, 0);
    SetSizer(sz_vert);
}

LibraryCtrl::~LibraryCtrl() {
    // nothing to do
}

bool LibraryCtrl::SetEntrySelection(const wxString &entry) {
    if (Library *lib = Library::Find(m_library)) {
        int entry_idx = lib->FindEntry(entry);
        if (entry_idx >= 0) {
            int to_sel = -1;
            // find in current view

            for (size_t i = 0; i < m_view.size(); i++) {
                if ((int) m_view[i].index == entry_idx) {
                    to_sel = i;
                    break;
                }
            }

            if (to_sel < 0) {
                // selecting item that is not in the currently filtered view,
                // so we must add it to the view
                m_view.push_back(viewable(entry, entry_idx));
                m_list->SetItemCount(m_view.size());
                m_list->Refresh();

                to_sel = m_view.size() - 1;
            }

            m_list->SetItemState(to_sel, wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED,
                                 wxLIST_STATE_SELECTED | wxLIST_STATE_FOCUSED);
            m_list->EnsureVisible(to_sel);

            return true;
        }
    }

    return false;
}

wxString LibraryCtrl::GetEntrySelection() {
    long sel = m_list->GetFirstSelected();
    if (sel >= 0 && sel < (long) m_view.size()) return m_view[sel].name;
    return wxEmptyString;
}

wxString LibraryCtrl::GetCellValue(long item, long col) {
    if (Library *lib = Library::Find(m_library))
        if (item < (long) m_view.size() && col < (long) m_fieldMap.size())
            return lib->GetEntryValue(m_view[item].index, m_fieldMap[col]);

    return wxT("<inval>");
}

void LibraryCtrl::SetLibrary(const wxString &name, const wxString &fields) {
    m_library = name;

    if (Library *lib = Library::Find(m_library)) {
        m_fields.Clear();
        m_fieldMap.clear();

        if (fields == "*") {
            std::vector<Library::Field> &ff = lib->GetFields();
            for (size_t i = 0; i < ff.size(); i++) {
                m_fields.Add(ff[i].Name);
                m_fieldMap.push_back(i);
            }
        } else {
            wxArrayString fnames = wxSplit(fields, ',');
            for (size_t i = 0; i < fnames.size(); i++) {
                int idx = lib->GetFieldIndex(fnames[i]);
                if (idx >= 0) {
                    m_fields.Add(fnames[i]);
                    m_fieldMap.push_back(lib->GetFieldIndex(fnames[i]));
                }
            }
        }

        m_sortDir.resize(m_fieldMap.size(), true);
    }

    ReloadLibrary();

}

void LibraryCtrl::ReloadLibrary() {
    if (Library *lib = Library::Find(m_library)) {
        wxString item = GetEntrySelection();
        wxString tarsel = m_target->GetStringSelection();
        m_target->Clear();

        m_list->ClearAll();
        for (size_t i = 0; i < m_fields.size(); i++) {
            m_list->AppendColumn(m_fields[i]);
            m_target->Append(m_fields[i]);
        }

        if (tarsel.IsEmpty() && m_target->GetCount() > 0)
            m_target->SetSelection(0);
        else
            m_target->SetStringSelection(tarsel);

        m_entries = lib->ListEntries();

        UpdateList();

/*		double sf = wxGetScreenHDScale();
		m_list->SetColumnWidth( 0, (int)(350*sf) );
		for( int i=1;i<m_list->GetColumnCount();i++ )
			m_list->SetColumnWidth( i, (int)(100*sf) );

		for (size_t ic = 0; ic < (size_t)m_list->GetColumnCount(); ic++)
			m_list->SetColumnWidth(ic, (int)(140 * wxGetScreenHDScale()));
*/
        for (int i = 0; i < m_list->GetColumnCount(); ++i) {
            m_list->SetColumnWidth(i, wxLIST_AUTOSIZE);
            const int a_width = m_list->GetColumnWidth(i);
            m_list->SetColumnWidth(i, wxLIST_AUTOSIZE_USEHEADER);
            const int h_width = m_list->GetColumnWidth(i);
            m_list->SetColumnWidth(i, (std::max)(a_width, h_width));
        }


        if (!item.IsEmpty()) SetEntrySelection(item);
    }
        // TODO - address this issue from Nate when using parametrics
        //else
    else if (m_library != "Library Name")
        wxMessageBox("Could not find library: " + m_library);
}

void LibraryCtrl::UpdateList() {
    m_sendEvents = false;

    wxString filter = m_filter->GetValue().Lower();
    wxString sel = GetEntrySelection();

    m_notify->SetLabel(wxEmptyString);
    m_view.clear();
    if (m_entries.size() > 0)
        m_view.reserve(m_entries.size());

    m_nmatches = 0;

    if (Library *lib = Library::Find(m_library)) {
        size_t num_entries = lib->NumEntries();

        size_t target_field_idx = 0;
        int t_sel = m_target->GetSelection();
        if (t_sel >= 0 && t_sel < (int) m_fieldMap.size())
            target_field_idx = m_fieldMap[t_sel];

        for (size_t i = 0; i < num_entries; i++) {
            wxString target = lib->GetEntryValue(i, target_field_idx);

            if (filter.IsEmpty()
                || (filter.Len() <= 2 && target.Left(filter.Len()).Lower() == filter)
                || (target.Lower().Find(filter) >= 0)
                    ) {
                m_view.push_back(viewable(m_entries[i], i));
                m_nmatches++;
            } else if (target == sel)
                m_view.push_back(viewable(m_entries[i], i));
        }
    }

    if (m_nmatches == 0) {
        m_notify->SetLabel("No matches.");
        Layout();
    }

    m_list->SetItemCount(m_view.size());
    m_list->Refresh();
    SetEntrySelection(sel);

    m_sendEvents = true;
}

void LibraryCtrl::OnSelected(wxListEvent &evt) {
    if (!m_sendEvents) return;

    wxCommandEvent issue(wxEVT_LISTBOX, GetId());
    issue.SetEventObject(this);
    issue.SetInt(evt.GetSelection());
    ProcessEvent(issue);

}

bool LibraryCtrl::viewable_compare::operator()(const viewable &lhs, const viewable &rhs) {
    wxString l(col[lhs.index]), r(col[rhs.index]);
    double ln, rn;
    if (l.ToCDouble(&ln) && r.ToCDouble(&rn)) {
        if (dir && ln < rn) return true;
        else if (!dir && ln > rn) return true;
        else return false;
    } else {
        if (dir && l < r) return true;
        else if (!dir && l > r) return true;
        else return false;
    }
}

void LibraryCtrl::OnColClick(wxListEvent &evt) {
    wxBusyCursor wait;

    if (Library *lib = Library::Find(m_library)) {
        m_sendEvents = false;

        wxString sel = GetEntrySelection();

        size_t field_idx = m_fieldMap[evt.GetColumn()];

        wxArrayString column;
        column.reserve(m_entries.size());
        for (size_t i = 0; i < m_entries.size(); i++)
            column.push_back(lib->GetEntryValue(i, field_idx));

        viewable_compare cc(column, !m_sortDir[evt.GetColumn()]);
        std::stable_sort(m_view.begin(), m_view.end(), cc);

        m_sortDir[evt.GetColumn()] = !m_sortDir[evt.GetColumn()];

        m_list->Refresh();
        SetEntrySelection(sel);

        m_sendEvents = true;
    }

}

void LibraryCtrl::OnCommand(wxCommandEvent &evt) {
    if (evt.GetId() == ID_FILTER || evt.GetId() == ID_TARGET) {
        UpdateList();
        if (evt.GetId() == ID_FILTER) {
            wxCommandEvent issue(wxEVT_LISTBOX, GetId());
            issue.SetEventObject(this);
            issue.SetInt(evt.GetSelection());
            ProcessEvent(issue);
        }
    }
}

void LibraryCtrl::SetLabel(const wxString &text) {
    m_label->SetLabel(text);
    Layout();
}

bool ShowSolarResourceDataSettings() {
    wxString dnpath;
    if (!SamApp::Settings().Read("solar_download_path", &dnpath) || dnpath.IsEmpty()) {
        dnpath = ::wxGetHomeDir() + "/SAM Downloaded Weather Files";
        SamApp::Settings().Write("solar_download_path", dnpath);
    }

    if (!wxDirExists(dnpath)) {
        if (wxFileName::Mkdir(dnpath, 511, ::wxPATH_MKDIR_FULL))
            SamApp::Settings().Write("solar_download_path", dnpath);
        else
            wxMessageBox("Please select a Solar \"Resource Data Folder\" in the following dialog.");
    }

    wxString buf;
    wxArrayString paths;
    if (SamApp::Settings().Read("solar_data_paths", &buf))
        paths = wxStringTokenize(buf, ";");

    SettingsDialog dialog(SamApp::Window(), "Solar Resource Data Folder Settings", "Solar Data File");
    dialog.CenterOnParent();
    dialog.SetLibraryPaths(paths);
    dialog.SetDownloadPath(dnpath);
    if (dialog.ShowModal() == wxID_OK) {
        SamApp::Settings().Write("solar_download_path", dialog.GetDownloadPath());
        SamApp::Settings().Write("solar_data_paths", wxJoin(dialog.GetLibraryPaths(), ';'));
        return true;
    } else return false;
}

bool ShowWindResourceDataSettings() {
    wxMessageBox("Wind data settings not supported yet.");
    return false;
}

bool ScanSolarResourceData(const wxString &db_file, bool) {
//	wxBusyInfo *busy = 0;
//	if ( show_busy )
//		busy = new wxBusyInfo("Updating solar resource library...");

    wxArrayString paths;
    paths.Add(SamApp::GetRuntimePath() + "../solar_resource/");

    wxString dnpath;
    if (SamApp::Settings().Read("solar_download_path", &dnpath)
        && wxDirExists(dnpath))
        paths.Add(dnpath);

    wxString slist;
    if (SamApp::Settings().Read("solar_data_paths", &slist)) {
        wxArrayString ll = wxStringTokenize(slist, ";");
        for (size_t i = 0; i < ll.size(); i++)
            if (wxDirExists(ll[i]))
                paths.Add(ll[i]);
    }

    wxCSVData csv;
    csv(0, 0) = "Name";
    csv(2, 0) = "[0]";

    csv(0, 1) = "City";
    csv(2, 1) = "city";

    csv(0, 2) = "State";
    csv(2, 2) = "state";

    csv(0, 3) = "Country";
    csv(2, 3) = "country";

    csv(0, 4) = "Latitude";
    csv(1, 4) = "deg";
    csv(2, 4) = "lat";

    csv(0, 5) = "Longitude";
    csv(1, 5) = "deg";
    csv(2, 5) = "lon";

    csv(0, 6) = "Time zone";
    csv(1, 6) = "hour";
    csv(2, 6) = "tz";

    csv(0, 7) = "Elevation";
    csv(1, 7) = "m";
    csv(2, 7) = "elev";

    csv(0, 8) = "Station ID";
    csv(2, 8) = "station_id";

    csv(0, 9) = "Source";
    csv(2, 9) = "solar_data_source";

    csv(0, 10) = "File name";
    csv(2, 10) = "solar_data_file_name";

    int row = 3;
    wxArrayString errors;
    for (size_t i = 0; i < paths.size(); i++) {
        wxString path(paths[i]);
        wxDir dir(path);
        if (!dir.IsOpened()) {
            wxLogStatus("ScanSolarResourceData: could not open folder " + path);
            continue;
        }

        wxString file;
        bool has_more = dir.GetFirst(&file, wxEmptyString, wxDIR_FILES);
        while (has_more) {
            // process file
            wxString wf = paths[i] + "/" + file;
            wxFileName fnn(wf);
            wxString ext = fnn.GetExt().Lower();
            if (ext != "csv"
                && ext != "tm2"
                && ext != "tm3"
                && ext != "epw"
                && ext != "smw") {

                has_more = dir.GetNext(&file);
                continue;
            }

            ssc_data_t pdata = ssc_data_create();
            ssc_data_set_string(pdata, "file_name", (const char *) wf.c_str());
            ssc_data_set_number(pdata, "header_only", 1);

            if (const char *err = ssc_module_exec_simple_nothread("wfreader", pdata)) {
                errors.Add(wf);
                wxLogStatus("error scanning '" + wf + "'");
                wxLogStatus("\t%s", err);
            } else {
                ssc_number_t val;
                const char *str;

                wxFileName ff(wf);
                ff.Normalize();

                csv(row, 0) = ff.GetName();

                if ((str = ssc_data_get_string(pdata, "city")) != 0)
                    csv(row, 1) = wxString(str);

                if ((str = ssc_data_get_string(pdata, "state")) != 0)
                    csv(row, 2) = wxString(str);

                if ((str = ssc_data_get_string(pdata, "country")) != 0)
                    csv(row, 3) = wxString(str);

                if (ssc_data_get_number(pdata, "lat", &val))
                    csv(row, 4) = wxString::Format("%g", val);

                if (ssc_data_get_number(pdata, "lon", &val))
                    csv(row, 5) = wxString::Format("%g", val);

                if (ssc_data_get_number(pdata, "tz", &val))
                    csv(row, 6) = wxString::Format("%g", val);

                if (ssc_data_get_number(pdata, "elev", &val))
                    csv(row, 7) = wxString::Format("%g", val);

                if ((str = ssc_data_get_string(pdata, "location")) != 0)
                    csv(row, 8) = wxString(str);

                if ((str = ssc_data_get_string(pdata, "source")) != 0)
                    csv(row, 9) = wxString(str);

                csv(row, 10) = ff.GetFullPath();

                row++;
            }

            ssc_data_free(pdata);

            has_more = dir.GetNext(&file);
        }
    }

//	if ( busy ) delete busy;

    size_t nerr = errors.size();
    if (nerr > 0) {
#define NERRMAX 5
        if (nerr > NERRMAX) {
            errors.erase(errors.begin() + NERRMAX, errors.end());
            errors.Add(wxString::Format("and %d more...", nerr - NERRMAX));
        }
        // results in multiple annoying pop-ups, rely on UI form callbacks to display error messages
        //wxMessageBox( "The following weather files in your weather file folders appear to have problems:\n\n" + wxJoin( errors, '\n' ) );
    }

    return csv.WriteFile(db_file);
}

bool ScanWindResourceData(const wxString &db_file, bool show_busy) {
    wxBusyInfo *busy = 0;
    if (show_busy)
        busy = new wxBusyInfo("Updating wind resource library...");

    wxString path = SamApp::GetRuntimePath() + "../wind_resource/";
    wxDir dir(path);
    if (!dir.IsOpened()) {
        if (busy) delete busy;
        return false;
    }

    wxCSVData csv;
    csv(0, 0) = "Name";
    csv(2, 0) = "[0]";

    csv(0, 1) = "City";
    csv(2, 1) = "wind_resource.city";

    csv(0, 2) = "State";
    csv(2, 2) = "wind_resource.state";

    csv(0, 3) = "Country";
    csv(2, 3) = "wind_resource.country";

    csv(0, 4) = "Latitude";
    csv(1, 4) = "deg";
    csv(2, 4) = "wind_resource.lat";

    csv(0, 5) = "Longitude";
    csv(1, 5) = "deg";
    csv(2, 5) = "wind_resource.lon";

    csv(0, 6) = "Location ID";
    csv(2, 6) = "wind_resource.location_id";

    csv(0, 7) = "Elevation";
    csv(1, 7) = "m";
    csv(2, 7) = "wind_resource.elev";

    csv(0, 8) = "Year";
    csv(2, 8) = "wind_resource.year";

    csv(0, 9) = "Description";
    csv(2, 9) = "wind_resource.description";

    csv(0, 10) = "File name";
    csv(2, 10) = "wind_resource.file";

    csv(0, 11) = "Closest Speed Measurement Ht";
    csv(2, 11) = "wind_resource.closest_speed_meas_ht";

    csv(0, 12) = "Closest Direction Measurement Ht";
    csv(2, 12) = "wind_resource.closest_dir_meas_ht";

    int row = 3;
    wxString file;
    bool has_more = dir.GetFirst(&file, "*.srw", wxDIR_FILES);
    while (has_more) {
        // process file
        wxString wf = path + "/" + file;

        ssc_data_t pdata = ssc_data_create();
        ssc_data_set_string(pdata, "file_name", (const char *) wf.c_str());
        ssc_data_set_number(pdata, "scan_header_only", 1);
        ssc_data_set_number(pdata, "requested_ht", 80.0);

        if (const char *err = ssc_module_exec_simple_nothread("wind_file_reader", pdata)) {
            wxLogStatus("error scanning '" + wf + "'");
            wxLogStatus("\t%s", err);
        } else {
            ssc_number_t val;
            const char *str;

            wxFileName ff(wf);
            ff.Normalize();

            csv(row, 0) = ff.GetName();

            if ((str = ssc_data_get_string(pdata, "city")) != 0)
                csv(row, 1) = wxString(str);

            if ((str = ssc_data_get_string(pdata, "state")) != 0)
                csv(row, 2) = wxString(str);

            if ((str = ssc_data_get_string(pdata, "country")) != 0)
                csv(row, 3) = wxString(str);

            if (ssc_data_get_number(pdata, "lat", &val))
                csv(row, 4) = wxString::Format("%g", val);

            if (ssc_data_get_number(pdata, "lon", &val))
                csv(row, 5) = wxString::Format("%g", val);

            if ((str = ssc_data_get_string(pdata, "location_id")) != 0)
                csv(row, 6) = wxString(str);

            if (ssc_data_get_number(pdata, "elev", &val))
                csv(row, 7) = wxString::Format("%g", val);

            if (ssc_data_get_number(pdata, "year", &val))
                csv(row, 8) = wxString::Format("%g", val);

            if ((str = ssc_data_get_string(pdata, "description")) != 0)
                csv(row, 9) = wxString(str);

            csv(row, 10) = ff.GetFullPath();

            if (ssc_data_get_number(pdata, "closest_speed_meas_ht", &val))
                csv(row, 11) = wxString::Format("%g", val);

            if (ssc_data_get_number(pdata, "closest_dir_meas_ht", &val))
                csv(row, 12) = wxString::Format("%g", val);

            row++;
        }

        ssc_data_free(pdata);

        has_more = dir.GetNext(&file);
    }

    if (busy) delete busy;

    return csv.WriteFile(db_file);
}

bool ScanWaveResourceData(const wxString &db_file, bool show_busy) {
    // TODO - update fields based on final file
    wxBusyInfo *busy = 0;
    if (show_busy)
        busy = new wxBusyInfo("Updating wave resource library...");

    wxString path = SamApp::GetRuntimePath() + "../wave_resource/";
    wxDir dir(path);
    if (!dir.IsOpened()) {
        if (busy) delete busy;
        return false;
    }


    wxCSVData csv;
    csv(0, 0) = "Name";
    csv(2, 0) = "[0]";

    csv(0, 1) = "City";
    csv(2, 1) = "city";

    csv(0, 2) = "State";
    csv(2, 2) = "state";

    csv(0, 3) = "Country";
    csv(2, 3) = "country";

    csv(0, 4) = "Latitude";
    csv(1, 4) = "deg";
    csv(2, 4) = "lat";

    csv(0, 5) = "Longitude";
    csv(1, 5) = "deg";
    csv(2, 5) = "lon";

    csv(0, 6) = "Nearby buoy number";
    csv(2, 6) = "nearby_buoy_number";

    csv(0, 7) = "Average power flux";
    csv(1, 7) = "kW/m";
    csv(2, 7) = "average_power_flux";

    csv(0, 8) = "Bathymetry";
    csv(2, 8) = "bathymetry";

    csv(0, 9) = "Sea bed";
    csv(2, 9) = "sea_bed";

    csv(0, 10) = "Time zone";
    csv(2, 10) = "tz";

    csv(0, 11) = "Data source";
    csv(2, 11) = "data_source";

    csv(0, 12) = "Notes";
    csv(2, 12) = "notes";

    csv(0, 13) = "File name";
    csv(2, 13) = "file_name";

    csv(0, 14) = "Frequency distribution";
    csv(2, 14) = "wave_resource_matrix";

    int row = 3;
    wxString file;
    bool has_more = dir.GetFirst(&file, "*.csv", wxDIR_FILES);
    while (has_more) {
        // process file
        wxString wf = path + "/" + file;

        ssc_data_t pdata = ssc_data_create();
        ssc_data_set_string(pdata, "wave_resource_filename", (const char *) wf.c_str());

        if (const char *err = ssc_module_exec_simple_nothread("wave_file_reader", pdata)) {
            wxLogStatus("error scanning '" + wf + "'");
            wxLogStatus("\t%s", err);
        } else {
            ssc_number_t val;
            ssc_number_t *mat;
            int nrows, ncols;
            const char *str;

            wxFileName ff(wf);
            ff.Normalize();

            if ((str = ssc_data_get_string(pdata, "name")) != 0)
                csv(row, 0) = wxString(str);

            if ((str = ssc_data_get_string(pdata, "city")) != 0)
                csv(row, 1) = wxString(str);

            if ((str = ssc_data_get_string(pdata, "state")) != 0)
                csv(row, 2) = wxString(str);

            if ((str = ssc_data_get_string(pdata, "country")) != 0)
                csv(row, 3) = wxString(str);

            if (ssc_data_get_number(pdata, "lat", &val))
                csv(row, 4) = wxString::Format("%g", val);

            if (ssc_data_get_number(pdata, "lon", &val))
                csv(row, 5) = wxString::Format("%g", val);

            if ((str = ssc_data_get_string(pdata, "nearby_buoy_number")) != 0)
                csv(row, 6) = wxString(str);

            if (ssc_data_get_number(pdata, "average_power_flux", &val))
                csv(row, 7) = wxString::Format("%g", val);

            if ((str = ssc_data_get_string(pdata, "bathymetry")) != 0)
                csv(row, 8) = wxString(str);

            if ((str = ssc_data_get_string(pdata, "sea_bed")) != 0)
                csv(row, 9) = wxString(str);

            if (ssc_data_get_number(pdata, "tz", &val))
                csv(row, 10) = wxString::Format("%g", val);

            if ((str = ssc_data_get_string(pdata, "data_source")) != 0)
                csv(row, 11) = wxString(str);

            if ((str = ssc_data_get_string(pdata, "notes")) != 0)
                csv(row, 12) = wxString(str);

            csv(row, 13) = ff.GetFullPath();

            if ((mat = ssc_data_get_matrix(pdata, "wave_resource_matrix", &nrows, &ncols)) != 0) {
                wxString wstr = "";
                for (int r = 0; r < nrows; r++) {
                    wstr += "[";
                    for (int c = 0; c < ncols; c++) {
                        wstr += wxString::Format("%g", mat[r * ncols + c]);
                        if (c < ncols - 1) wstr += ";";
                    }
                    wstr += "]";
                }
                csv(row, 14) = wxString(wstr);
            }
            row++;
        }

        ssc_data_free(pdata);

        has_more = dir.GetNext(&file);
    }

    if (busy) delete busy;

    return csv.WriteFile(db_file);
}
