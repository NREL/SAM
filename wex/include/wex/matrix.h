#ifndef __wex_matrix_h
#define __wex_matrix_h

#include<stdlib.h>
#include<assert.h>

#if defined(_DEBUG) && defined(_MSC_VER) && defined(_WIN32) && !defined(_WIN64)
#define VEC_ASSERT(x) {if(!(x)) _asm{int 0x03}}
#else
#define VEC_ASSERT(X) assert(X)
#endif

template< typename T >
class wxMatrix
{
protected:
	T *t_array;
	size_t n_rows, n_cols;
public:

	wxMatrix()
	{
		t_array = 0;
		n_rows = n_cols = 0;
	}

	wxMatrix( const wxMatrix &rhs )
	{
		t_array = 0;
		n_rows = n_cols = 0;
		Copy( rhs );
	}

	wxMatrix(size_t nr, size_t nc)
	{
		n_rows = n_cols = 0;
		t_array = 0;
		if (nr < 1) nr = 1;
		if (nc < 1) nc = 1;
		Resize(nr,nc);
	}
		
	wxMatrix(size_t nr, size_t nc, const T &val)
	{
		n_rows = n_cols = 0;
		t_array = NULL;
		if (nr < 1) nr = 1;
		if (nc < 1) nc = 1;
		ResizeFill(nr,nc,val);
	}


	virtual ~wxMatrix()
	{
		if (t_array) delete [] t_array;
	}
		
	void Clear()
	{
		if (t_array) delete [] t_array;
		t_array = 0;
		n_rows = n_cols = 0;
	}
		
	void Copy( const wxMatrix &rhs )
	{
		if (this != &rhs)
		{
			Resize( rhs.Rows(), rhs.Cols() );
			size_t nn = n_rows*n_cols;
			for (size_t i=0;i<nn;i++)
				t_array[i] = rhs.t_array[i];
		}
	}

	void Assign( const T *pvalues, size_t len )
	{
		Resize( len );
		if ( n_cols == len && n_rows == 1 )
			for (size_t i=0;i<len;i++)
				t_array[i] = pvalues[i];
	}
		
	void Assign( const T *pvalues, size_t nr, size_t nc )
	{
		Resize( nr, nc );
		if ( n_rows == nr && n_cols == nc )
		{
			size_t len = nr*nc;
			for (size_t i=0;i<len;i++)
				t_array[i] = pvalues[i];
		}
	}

	wxMatrix &operator=(const wxMatrix &rhs)
	{
		Copy( rhs );
		return *this;
	}
		
	wxMatrix &operator=(const T &val)
	{
		Resize(1,1);
		t_array[0] = val;
		return *this;
	}
		
	inline operator T()
	{
		return t_array[0];
	}
		
	bool Equals( const wxMatrix & rhs )
	{
		if (n_rows != rhs.n_rows || n_cols != rhs.n_cols)
			return false;
			
		size_t nn = n_rows*n_cols;
		for (size_t i=0;i<nn;i++)
			if (t_array[i] != rhs.t_array[i])
				return false;
			
		return true;
	}
		
	inline bool IsSingle()
	{
		return (n_rows == 1 && n_cols == 1);
	}
			
	inline bool IsArray()
	{
		return (n_rows == 1);
	}
		
	void Fill( const T &val )
	{
		size_t ncells = n_rows*n_cols;
		for (size_t i=0;i<ncells;i++)
			t_array[i] = val;
	}

	void Resize(size_t nr, size_t nc)
	{
		if (nr == 0 || nc == 0) {
			Clear();
			return;
		}

		if (nr == n_rows && nc == n_cols) return;
			
		if (t_array) delete [] t_array;
		t_array = new T[nr * nc];
		n_rows = nr;
		n_cols = nc;
	}

	void ResizeFill(size_t nr, size_t nc, const T &val)
	{
		Resize( nr, nc );
		Fill( val );
	}

	void ResizePreserve( size_t nr, size_t nc, const T &val )
	{
		wxMatrix<T> old( *this );
		Resize( nr, nc );
		Fill( val );
		for( size_t r=0;r<nr && r<old.nrows();r++)
			for( size_t c=0;c<nc && c<old.ncols();c++)
				At(r,c) = old(r,c);
	}
		
	void Resize(size_t len)
	{
		Resize( 1, len );
	}
		
	void ResizeFill(size_t len, const T &val)
	{
		ResizeFill( 1, len, val );
	}
		
	inline T &At(size_t i)
	{
#ifdef _DEBUG
		VEC_ASSERT( i >= 0 && i < n_cols );
#endif
		return t_array[i];
	}

	inline const T&At(size_t i) const
	{
#ifdef _DEBUG
		VEC_ASSERT( i >= 0 && i < n_cols );
#endif
		return t_array[i];
	}
		
	inline T &At(size_t r, size_t c)
	{
#ifdef _DEBUG
		VEC_ASSERT( r >= 0 && r < n_rows && c >= 0 && c < n_cols );
#endif
		return t_array[n_cols*r+c];
	}

	inline const T &At(size_t r, size_t c) const
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
				
	inline size_t Rows() const
	{
		return n_rows;
	}
		
	inline size_t Cols() const
	{
		return n_cols;
	}
		
	inline size_t Cells() const
	{
		return n_rows*n_cols;
	}
		
	inline size_t Bytes() const
	{
		return n_rows*n_cols*sizeof(T);
	}
		
	void Size(size_t &nr, size_t &nc) const
	{
		nr = n_rows;
		nc = n_cols;
	}
		
	size_t Length() const
	{
		return n_cols;
	}
	
	bool Empty() const
	{
		return ( 0 == n_rows*n_cols );
	}

	inline T *Data()
	{
		return t_array;
	}
	
	inline T &RawIndex(size_t idx)
	{
#ifdef _DEBUG
		VEC_ASSERT( idx < n_rows*n_cols );
#endif
		return t_array[idx];
	}

	inline const T &RawIndex(size_t idx) const
	{
#ifdef _DEBUG
		VEC_ASSERT( idx < n_rows*n_cols );
#endif
		return t_array[idx];
	}

	inline T Value() const
	{
		return t_array[0];
	}

};

#endif
