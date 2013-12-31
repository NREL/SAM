#ifndef __library_h
#define __library_h

#include <wx/string.h>

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
	int FindEntry( const wxString &name );
	wxString GetEntryValue( int entry, int field );
	bool ApplyEntry( int entry, int varindex, VarTable &tab, wxArrayString &changed );

private:
	wxCSVData m_csv;

	bool ScanData();

	wxString m_name;
	std::vector<Field> m_fields;
	size_t m_startRow;
	wxArrayString m_errors;
};

bool ScanSolarResourceData();



#endif
