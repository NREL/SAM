#ifndef __csvdata_h
#define __csvdata_h

#include <wx/string.h>
#include <wx/stream.h>


#include <unordered_map>
using std::unordered_map;
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'


#include <wx/hashmap.h>
#include <wx/stream.h>

class wxGrid;

class wxCSVData
{
public:
	wxCSVData();
	wxCSVData( const wxCSVData &copy );
	virtual ~wxCSVData();

	void Copy( const wxCSVData &copy );
	wxCSVData &operator=( const wxCSVData &copy );

	void Set( size_t r, size_t c, const wxString &val );
	wxString &operator()(size_t r, size_t c);
	const wxString &Get( size_t r, size_t c ) const;
	const wxString &operator()(size_t r, size_t c) const;

	size_t NumCells() const;
	size_t NumRows();
	size_t NumCols();

	void Clear();
	void Clear( size_t r, size_t c );
	bool IsEmpty( size_t r, size_t c );
	
	bool Read( wxInputStream &in );
	void Write( wxOutputStream &out );

	bool ReadFile( const wxString &file );
	bool WriteFile( const wxString &file );

	bool ReadString( const wxString &data );
	wxString WriteString();

	int GetErrorLine() { return m_errorLine; }

	void SetSeparator( wxUniChar sep ) { m_sep = sep; }
	wxUniChar GetSeparator();
	
protected:
	wxUniChar m_sep;
	bool m_invalidated;
	size_t m_nrows, m_ncols;
	typedef unordered_map<wxUint64, wxString> cell_hash;
	cell_hash m_cells;
	int m_errorLine;
	wxString m_emptyStr;
	
	wxUint64 Encode( size_t r, size_t c ) const;
	void Decode( wxUint64 idx, size_t *r, size_t *c ) const;
	void RecalculateDimensions();
};

#endif
