#ifndef __object_h
#define __object_h


#include <vector>

#ifdef _MSC_VER
#include <unordered_map>
using std::tr1::unordered_map;
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'
#else
#include <tr1/unordered_map>
using std::tr1::unordered_map;
#endif

#include <wx/hashmap.h>
#include <wx/stream.h>


class Object
{
public:
	virtual ~Object() { }

	virtual Object *Duplicate() = 0;
	virtual bool Copy( Object *obj ) = 0;
	virtual wxString GetTypeName() = 0;
	virtual void Write( wxOutputStream & ) = 0;
	virtual bool Read( wxInputStream & ) = 0;
};

class ObjectTypes
{
public:
	static void Register( Object *dummy );
	static Object *Create( const wxString &type );
	static wxArrayString AllTypes();
};

class ObjectCollection : public unordered_map< wxString, Object*, wxStringHash, wxStringEqual >
{
public:
	ObjectCollection();
	virtual ~ObjectCollection();
	
	void Clear();
	virtual void clear() { this->Clear(); }
	void Add( const wxString &name, Object * );
	bool Delete( const wxString &name );
	Object *Lookup( const wxString &name );
	wxArrayString GetNames();
	
	void Write( wxOutputStream &out );
	bool Read( wxInputStream &in ); // does NOT clear first
};



class StringHash : public unordered_map<wxString, wxString, wxStringHash, wxStringEqual>,
	public Object
{
public:
	StringHash();

	virtual Object *Duplicate();
	virtual bool Copy( Object *obj );
	virtual wxString GetTypeName();
	virtual void Write( wxOutputStream & );
	virtual bool Read( wxInputStream & );
};



#if defined(_DEBUG) && defined(_MSC_VER) && defined(_WIN32) && !defined(_WIN64)
#define VEC_ASSERT(x) {if(!(x)) _asm{int 0x03}}
#else
#define VEC_ASSERT(X) assert(X)
#endif

template< typename T >
class matrix_t
{
protected:
	T *t_array;
	size_t n_rows, n_cols;
public:

	matrix_t()
	{
		t_array = new T[1];
		n_rows = n_cols = 1;
	}
		
	matrix_t(size_t len)
	{
		n_rows = n_cols = 0;
		t_array = NULL;
		if (len < 1) len = 1;
		resize( 1, len );
	}

	matrix_t(size_t nr, size_t nc)
	{
		n_rows = n_cols = 0;
		t_array = NULL;
		if (nr < 1) nr = 1;
		if (nc < 1) nc = 1;
		resize(nr,nc);
	}
		
	matrix_t(size_t nr, size_t nc, const T &val)
	{
		n_rows = n_cols = 0;
		t_array = NULL;
		if (nr < 1) nr = 1;
		if (nc < 1) nc = 1;
		resize(nr,nc);
		fill(val);
	}


	virtual ~matrix_t()
	{
		if (t_array) delete [] t_array;
	}
		
	void clear()
	{
		if (t_array) delete [] t_array;
		n_rows = n_cols = 1;
		t_array = new T[1];
	}
		
	void copy( const matrix_t &rhs )
	{
		if (this != &rhs)
		{
			resize( rhs.nrows(), rhs.ncols() );
			size_t nn = n_rows*n_cols;
			for (size_t i=0;i<nn;i++)
				t_array[i] = rhs.t_array[i];
		}
	}

	void assign( const T *pvalues, size_t len )
	{
		resize( len );
		if ( n_cols == len && n_rows == 1 )
			for (size_t i=0;i<len;i++)
				t_array[i] = pvalues[i];
	}
		
	void assign( const T *pvalues, size_t nr, size_t nc )
	{
		resize( nr, nc );
		if ( n_rows == nr && n_cols == nc )
		{
			size_t len = nr*nc;
			for (size_t i=0;i<len;i++)
				t_array[i] = pvalues[i];
		}
	}

	matrix_t &operator=(const matrix_t &rhs)
	{
		copy( rhs );
		return *this;
	}
		
	matrix_t &operator=(const T &val)
	{
		resize(1,1);
		t_array[0] = val;
		return *this;
	}
		
	inline operator T()
	{
		return t_array[0];
	}
		
	bool equals( const matrix_t & rhs )
	{
		if (n_rows != rhs.n_rows || n_cols != rhs.n_cols)
			return false;
			
		size_t nn = n_rows*n_cols;
		for (size_t i=0;i<nn;i++)
			if (t_array[i] != rhs.t_array[i])
				return false;
			
		return true;
	}
		
	inline bool is_single()
	{
		return (n_rows == 1 && n_cols == 1);
	}
			
	inline bool is_array()
	{
		return (n_rows == 1);
	}
		
	void fill( const T &val )
	{
		size_t ncells = n_rows*n_cols;
		for (size_t i=0;i<ncells;i++)
			t_array[i] = val;
	}

	void resize(size_t nr, size_t nc)
	{
		if (nr < 1 || nc < 1) return;
		if (nr == n_rows && nc == n_cols) return;
			
		if (t_array) delete [] t_array;
		t_array = new T[ nr * nc ];
		n_rows = nr;
		n_cols = nc;
	}

	void resize_fill(size_t nr, size_t nc, const T &val)
	{
		resize( nr, nc );
		fill( val );
	}
		
	void resize(size_t len)
	{
		resize( 1, len );
	}
		
	void resize_fill(size_t len, const T &val)
	{
		resize_fill( 1, len, val );
	}
		
	inline T &at(size_t i)
	{
#ifdef _DEBUG
		VEC_ASSERT( i >= 0 && i < n_cols );
#endif
		return t_array[i];
	}

	inline const T&at(size_t i) const
	{
#ifdef _DEBUG
		VEC_ASSERT( i >= 0 && i < n_cols );
#endif
		return t_array[i];
	}
		
	inline T &at(size_t r, size_t c)
	{
#ifdef _DEBUG
		VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols );
#endif
		return t_array[n_cols*r+c];
	}

	inline const T &at(size_t r, size_t c) const
	{
#ifdef _DEBUG
		VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols );
#endif
		return t_array[n_cols*r+c];
	}
		
	inline T &operator()(size_t r, size_t c)
	{
#ifdef _DEBUG
		VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols );
#endif
		return t_array[n_cols*r+c];
	}

	inline const T &operator()(size_t r, size_t c) const
	{
#ifdef _DEBUG
		VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols );
#endif
		return t_array[n_cols*r+c];
	}
		
	T operator[] (size_t i) const
	{
#ifdef _DEBUG
		VEC_ASSERT( i >= 0 && i < n_cols );
#endif
		return t_array[i];
	}
		
	T &operator[] (size_t i)
	{
#ifdef _DEBUG
		VEC_ASSERT( i >= 0 && i < n_cols );
#endif
		return t_array[i];
	}
				
	inline size_t nrows() const
	{
		return n_rows;
	}
		
	inline size_t ncols() const
	{
		return n_cols;
	}
		
	inline size_t ncells() const
	{
		return n_rows*n_cols;
	}
		
	inline size_t membytes() const
	{
		return n_rows*n_cols*sizeof(T);
	}
		
	void size(size_t &nr, size_t &nc) const
	{
		nr = n_rows;
		nc = n_cols;
	}
		
	size_t length() const
	{
		return n_cols;
	}
		
	inline T *data()
	{
		return t_array;
	}

	inline T value() const
	{
		return t_array[0];
	}
};


#endif

