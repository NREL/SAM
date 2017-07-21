/*******************************************************************************************************
*  Copyright 2017 Alliance for Sustainable Energy, LLC
*
*  NOTICE: This software was developed at least in part by Alliance for Sustainable Energy, LLC
*  (�Alliance�) under Contract No. DE-AC36-08GO28308 with the U.S. Department of Energy and the U.S.
*  The Government retains for itself and others acting on its behalf a nonexclusive, paid-up,
*  irrevocable worldwide license in the software to reproduce, prepare derivative works, distribute
*  copies to the public, perform publicly and display publicly, and to permit others to do so.
*
*  Redistribution and use in source and binary forms, with or without modification, are permitted
*  provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer.
*
*  2. Redistributions in binary form must reproduce the above copyright notice, the above government
*  rights notice, this list of conditions and the following disclaimer in the documentation and/or
*  other materials provided with the distribution.
*
*  3. The entire corresponding source code of any redistribution, with or without modification, by a
*  research entity, including but not limited to any contracting manager/operator of a United States
*  National Laboratory, any institution of higher learning, and any non-profit organization, must be
*  made publicly available under this license for as long as the redistribution is made available by
*  the research entity.
*
*  4. Redistribution of this software, without modification, must refer to the software by the same
*  designation. Redistribution of a modified version of this software (i) may not refer to the modified
*  version by the same designation, or by any confusingly similar designation, and (ii) must refer to
*  the underlying software originally provided by Alliance as �System Advisor Model� or �SAM�. Except
*  to comply with the foregoing, the terms �System Advisor Model�, �SAM�, or any confusingly similar
*  designation may not be used to refer to any modified version of this software or any modified
*  version of the underlying software originally provided by Alliance without the prior written consent
*  of Alliance.
*
*  5. The name of the copyright holder, contributors, the United States Government, the United States
*  Department of Energy, or any of their employees may not be used to endorse or promote products
*  derived from this software without specific prior written permission.
*
*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
*  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
*  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER,
*  CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF THEIR
*  EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
*  THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*******************************************************************************************************/

#ifndef __library_h
#define __library_h

#include <wx/string.h>
#include <wx/listctrl.h>
#include <wx/panel.h>

#include <wex/csv.h>

#include "variables.h"


/* Example library.  CSV Format

Name,Area,Pmp,Vmp,Imp,Voc,Isc,a,Il,Io,Rs,Rsh,gamma,NOCT
Units,m2,W,V,A,V,A,n/a,...
[0],pv.mod.cec.area,pv.mod.cec.pmp,pv.mod.cec.vmp,pv.mod.cec.imp,...
[1],pv.mod.cecuser.area,pv.mod.cecuser.pmp,...
Sunpower-215,2,5,1,3,4,4,4,2,4,5
Sunpower-330,4,4,1,5,3,2,5,3,2,1

*/

class Library
{
public:
	Library();

	static Library *Load( const wxString &file );
	static wxArrayString ListAll();
	static void UnloadAll();
	static Library *Find( const wxString &name );
	
	struct Field
	{
		wxString Name;
		wxString Units;
		wxArrayString Variables;
		int DataIndex;
	};

	bool Read( const wxString &file );
	bool Read( const wxCSVData &data, const wxString &name );
	wxString GetName() const;	
	wxArrayString &GetErrors() { return m_errors; }

	wxArrayString ListEntries();
	size_t NumEntries();
	std::vector<Field> &GetFields();
	int GetFieldIndex( const wxString &name );
	int FindEntry( const wxString &name );
	wxString GetEntryValue( int entry, int field );
	wxString GetEntryName( int entry );
	bool ApplyEntry( int entry, int varindex, VarTable &tab, wxArrayString &changed );

private:
	wxCSVData m_csv;

	bool ScanData();

	wxString m_name;
	std::vector<Field> m_fields;
	size_t m_startRow;
	wxArrayString m_errors;
};

bool ShowSolarResourceDataSettings();
bool ScanSolarResourceData( const wxString &db_file, bool show_busy = false );
bool ShowWindResourceDataSettings();
bool ScanWindResourceData( const wxString &db_file, bool show_busy = false );

class LibraryCtrl;
class wxTextCtrl;
class wxStaticText;
class wxChoice;

class LibraryListView : public wxListView
{
	LibraryCtrl *m_libctrl;
public:
	LibraryListView( LibraryCtrl *parent, int id, const wxPoint &pos = wxDefaultPosition,
		const wxSize &size = wxDefaultSize );
	
	virtual wxString OnGetItemText( long item, long col ) const;
//	virtual wxListItemAttr *OnGetItemAttr( long item ) const;
};

#define EVT_LIBRARYCTRL(id, func) EVT_LISTBOX(id,func)

class LibraryCtrl : public wxPanel
{
public:
	LibraryCtrl( wxWindow *parent, int id, const wxPoint &pos = wxDefaultPosition,
		const wxSize &size = wxDefaultSize );
	virtual ~LibraryCtrl();

	void SetLabel( const wxString &label );

	void SetLibrary( const wxString &name, const wxString &fields );
	wxString GetLibrary() { return m_library; }
	void ReloadLibrary();

	bool SetEntrySelection( const wxString &entry );
	wxString GetEntrySelection();


	wxString GetCellValue( long item, long col );
	void UpdateList();

protected:
	void OnSelected( wxListEvent & );
	void OnColClick( wxListEvent & );
	void OnCommand( wxCommandEvent & );
	
private:
	wxString m_library;
	bool m_inclEntryName;

	std::vector<bool> m_sortDir;

	wxArrayString m_fields;
	std::vector<size_t> m_fieldMap;

	wxArrayString m_entries;

	struct viewable
	{
		viewable( const wxString &n, size_t i ) : name(n), index(i) { }
		wxString name;
		size_t index;
	};
	
	class viewable_compare
	{
		wxArrayString &col;
		bool dir;
	public:
		viewable_compare( wxArrayString &c,  bool d ) : col(c), dir(d) { }
		bool operator() ( const viewable &lhs, const viewable &rhs );
	};

	std::vector<viewable> m_view;

	wxStaticText *m_label;
	wxTextCtrl *m_filter;
	wxStaticText *m_notify;
	wxChoice *m_target;
	LibraryListView *m_list;

	bool m_sendEvents;
	
	
	DECLARE_EVENT_TABLE();
};



#endif
