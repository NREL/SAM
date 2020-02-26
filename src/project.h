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

#ifndef __project_h
#define __project_h

#include <vector>
#include <lk/env.h>

#include "object.h"

class Case;

class ProjectFile;

#define VERSION_VALUE(maj, min, mic)  ( ((size_t)(maj)) * 10000 + ((size_t)(min))*100 + ((size_t)(mic)) )

class ProjectFileEvent {
private:
    int m_type;
    wxString m_str, m_str2;
public:
    enum {
        CASE_ADDED, CASE_DELETED, CASE_RENAMED, PROJECTFILE_DELETED
    };

    ProjectFileEvent(int type) : m_type(type) {}

    ProjectFileEvent(int type, const wxString &str) : m_type(type), m_str(str) {}

    ProjectFileEvent(int type, const wxString &str, const wxString &str2) : m_type(type), m_str(str), m_str2(str2) {}

    int GetType() { return m_type; }

    wxString GetString() { return m_str; }

    wxString GetString2() { return m_str2; }
};

class ProjectFileEventListener {
public:
    virtual void OnProjectFileEvent(ProjectFile *, ProjectFileEvent &) = 0;
};


class ProjectFile {
public:
    ProjectFile();

    ProjectFile(const ProjectFile &cpy);

    virtual ~ProjectFile();

    void Copy(const ProjectFile &rhs, bool listeners_too = true);

    void Clear();

    // managing cases
    void AddCase(const wxString &name, Case *c);

    Case *AddCase(const wxString &name);

    bool DeleteCase(const wxString &name);

    Case *GetCase(const wxString &name);

    wxString GetCaseName(Case *c);

    wxArrayString GetCaseNames();

    std::vector<Case *> GetCases();

    bool RenameCase(const wxString &old_name, const wxString &new_name);

    // simple project file properties
    wxString GetProperty(const wxString &name);

    void SetProperty(const wxString &name, const wxString &value);

    wxArrayString GetProperties();

    // general purpose data storage objects
    void AddObject(const wxString &name, Object *);

    void DeleteObject(const wxString &name);

    bool RenameObject(const wxString &old_name, const wxString &new_name);

    Object *GetObject(const wxString &name);

    wxArrayString GetObjects();

    void Write(wxOutputStream &out);

    bool Read(wxInputStream &in);

    bool WriteArchive(const wxString &file);

    bool ReadArchive(const wxString &file);

    wxString GetLastError() { return m_lastError; }

    bool IsModified() { return m_modified; }

    void SetModified(bool b) { m_modified = b; }

    void AddListener(ProjectFileEventListener *pel);

    void RemoveListener(ProjectFileEventListener *pel);

    void ClearListeners();

    void SendEvent(ProjectFileEvent e);

    void SetSaveHourlyData(bool b) { m_saveHourlyData = b; }

    bool GetSaveHourlyData() { return m_saveHourlyData; }

    void SetVersionInfo(int maj, int min, int mic, int patch);

    size_t GetVersionInfo(int *maj = 0, int *min = 0, int *mic = 0, int *patch = 0);

private:
    ObjectCollection m_cases;
    ObjectCollection m_objects;
    StringHash m_properties;
    wxString m_lastError;
    bool m_saveHourlyData;
    bool m_modified;
    std::vector<ProjectFileEventListener *> m_listeners;
    int m_verMajor, m_verMinor, m_verMicro, m_verPatch;
};


class VersionUpgrade {
public:
    enum {
        FAIL, WARNING, NOTICE, CONFIG_CHANGE, VAR_ADDED, VAR_CHANGED, VAR_DELETED
    };

    struct log {
        log() { type = FAIL; }

        log(int ty, const wxString &m, const wxString &r = wxEmptyString)
                : type(ty), message(m), reason(r) {};

        int type;
        wxString message;
        wxString reason;
    };

    typedef unordered_map<wxString, std::vector<log>, wxStringHash, wxStringEqual> LogInfo;


public:
    VersionUpgrade();

    bool Run(ProjectFile &pf);

    std::vector<log> &GetLog(const wxString &casename = wxEmptyString);

    Case *GetCase() { return m_case; }

    wxString GetName() { return m_name; }

    void ShowReportDialog(const wxString &file, bool modal = false);

    wxString CreateHtmlReport(const wxString &file);

    static lk::fcall_t *invoke_functions();

private:
    LogInfo m_log;
    Case *m_case;
    wxString m_name;
    lk::env_t m_env;
    std::vector<log> m_generalLog;
    int m_pfMajor, m_pfMinor, m_pfMicro;

    void WriteHtml(const wxString &section, const std::vector<log> &log, wxString &html);

    bool Invoke(Case *c, const wxString &name, lk::node_t *root);
};


#endif

