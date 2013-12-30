#ifndef __library_h
#define __library_h

#include <wx/string.h>

#include <wex/csv.h>
/*

Name,Area,Pmp,Vmp,Imp,Voc,Isc,a,Il,Io,Rs,Rsh,gamma,NOCT
Units,m2,W,V,A,V,A,n/a,...
[0],pv.mod.cec.area,pv.mod.cec.pmp,pv.mod.cec.vmp,pv.mod.cec.imp,...
[1],pv.mod.cecuser.area,pv.mod.cecuser.pmp,...
[2]
Sunpower-215,2,5,1,3,4,4,4,2,4,5
Sunpower-330,4,4,1,5,3,2,5,3,2,1

*/

struct FieldHeader
{
	wxString Name;
	wxString Units;
	wxArrayString Variables;
};

class Library
{
public:
	Library();

	bool Load( const wxString &file );
	wxString GetName() const;

	wxArrayString ListEntries();
	size_t NumEntries();

	std::vector<FieldHeader> &GetFields() const;

	int FindEntry( const wxString &name );



private:
	wxCSVData m_csv;

	wxString m_dataFile;
	wxString m_name;
	std::vector<FieldHeader> m_fields;
};


#endif
