#ifndef __variable_h
#define __variable_h

#include <vector>

#include <wx/string.h>
#include <wx/arrstr.h>
#include <wx/stream.h>

#include <ssc/sscapi.h>

#include "object.h"

class Variables : public Object
{
public:
	Variables();
	virtual ~Variables();

	static const int INVALID = SSC_INVALID;
	static const int NUMBER = SSC_NUMBER;
	static const int ARRAY = SSC_ARRAY;
	static const int MATRIX = SSC_MATRIX;
	static const int STRING = SSC_STRING;
	static const int TABLE = SSC_TABLE;
		
	virtual Object *Duplicate();
	virtual bool Copy( Object *obj );
	virtual wxString GetTypeName();
	virtual void Write( wxOutputStream & );
	virtual bool Read( wxInputStream & );

	Variables &operator=( const Variables &rhs );

	bool IsAssigned( const wxString &name ) const;
	int Type( const wxString &name ) const;

	void Clear();
	void Clear( const wxString &name );
	void Clear( const wxArrayString &names );

	void Copy( const Variables &rhs, bool erase_first = true );
	wxArrayString List() const;
	
	void Set( const wxString &name, int val );
	void Set( const wxString &name, float val );
	
	void Set( const wxString &name, float *vals, size_t n );
	void Set( const wxString &name, const std::vector<float> &vals );
	
	void Set( const wxString &name, float **mat, size_t rows, size_t cols );
	void Set( const wxString &name, float *mat, size_t rows, size_t cols );
	void Set( const wxString &name, const std::vector< std::vector<float> > &mat );

	void Set( const wxString &name, const wxString &val );
	void Set( const wxString &name, const Variables &tab );

	int Integer( const wxString &name );
	float Value( const wxString &name );
	std::vector<float> Array( const wxString &name );
	std::vector< std::vector<float> > Matrix( const wxString &name );
	wxString String( const wxString &name );
	Variables Table( const wxString &name );
	
	ssc_data_t Data() const { return m_data; }


private:
	Variables( ssc_data_t src );

	void WriteTable( wxOutputStream &, ssc_data_t );
	bool ReadTable( wxInputStream &, ssc_data_t );

	ssc_data_t m_data;
	bool m_owned;
};

class VariableInfo
{
public:
	VariableInfo();
	~VariableInfo();

	bool Load( const wxString &script );
	void Clear();

	wxArrayString ListAll();
	wxArrayString ListAll( const wxString &tech, const wxString &fin );
	int Type( const wxString &name );
	wxString Label( const wxString &name );
	wxString Group( const wxString &name );
	wxString Units( const wxString &name );
	wxArrayString IndexLabels( const wxString &name );
	bool IsCalculated( const wxString &name );
	bool LoadDefaultValue( Variables &v, const wxString &name, 
		const wxString &tech, const wxString &fin );
	bool LoadDefaultValues( Variables &v,
		const wxString &tech, const wxString &fin );

private:
};


#endif
