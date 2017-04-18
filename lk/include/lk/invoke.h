#ifndef __lk_invoke_h
#define __lk_invoke_h

#ifdef __cplusplus
extern "C" {
#endif

typedef void* lk_var_t;

#define LK_NULL    1
#define LK_NUMBER  3
#define LK_STRING  4
#define LK_ARRAY   5
#define LK_TABLE   6

struct __lk_invoke_t
{
	void *__pinvoke; // internal calling context reference
	void *__hiter; // internal key interator context
	void *__errbuf; // error message buffer
	void *__sbuf; // internal string storage buffer (utf8)
	void *__callargvec; // internal call argument vector 
	void *__callresult; // internal call result

	int (*doc_mode)( struct __lk_invoke_t* );
	void (*document)( struct __lk_invoke_t*, const char *fn, const char *desc, const char *sig );
	void (*document2)( struct __lk_invoke_t*, const char *fn, const char *notes, 
			const char *desc1, const char *sig1, 
			const char *desc2, const char *sig2 );
	void (*document3)( struct __lk_invoke_t*, const char *fn, const char *notes, 
			const char *desc1, const char *sig1, 
			const char *desc2, const char *sig2, 
			const char *desc3, const char *sig3);

	void (*error)( struct __lk_invoke_t*, const char * );
	int (*arg_count)( struct __lk_invoke_t* );
	lk_var_t (*arg)( struct __lk_invoke_t*, int );

	int (*type)( struct __lk_invoke_t*, lk_var_t );
	const char *(*as_string)( struct __lk_invoke_t*, lk_var_t ); // returns utf8
	double (*as_number)( struct __lk_invoke_t*, lk_var_t );
	int (*as_integer)( struct __lk_invoke_t*, lk_var_t );
	int (*as_boolean)( struct __lk_invoke_t*, lk_var_t );

	int (*vec_count)( struct __lk_invoke_t*, lk_var_t );
	lk_var_t (*vec_index)( struct __lk_invoke_t*, lk_var_t, int );
	
	int (*tab_count) ( struct __lk_invoke_t*, lk_var_t );
	const char * (*tab_first_key)( struct __lk_invoke_t*, lk_var_t );
	const char * (*tab_next_key)( struct __lk_invoke_t*, lk_var_t );
	lk_var_t (*tab_value)( struct __lk_invoke_t*, lk_var_t, const char * );
	
	lk_var_t (*result)( struct __lk_invoke_t* );
	
	// variable modifications
	void (*set_null)  ( struct __lk_invoke_t*, lk_var_t );
	void (*set_string)( struct __lk_invoke_t*, lk_var_t, const char * ); // values are utf8
	void (*set_number)( struct __lk_invoke_t*, lk_var_t, double );

	void (*set_number_vec)( struct __lk_invoke_t*, lk_var_t, double *, int );
	void (*make_vec)( struct __lk_invoke_t*, lk_var_t );
	void (*reserve)( struct __lk_invoke_t*, lk_var_t, int len );
	lk_var_t (*append_number)( struct __lk_invoke_t*, lk_var_t, double );
	lk_var_t (*append_string)( struct __lk_invoke_t*, lk_var_t, const char * );
	lk_var_t (*append_null)( struct __lk_invoke_t*, lk_var_t );

	void (*make_tab)( struct __lk_invoke_t*, lk_var_t );
	lk_var_t (*tab_set_number)( struct __lk_invoke_t*, lk_var_t, const char *, double );
	lk_var_t (*tab_set_string)( struct __lk_invoke_t*, lk_var_t, const char *, const char * );
	lk_var_t (*tab_set_null)( struct __lk_invoke_t*, lk_var_t, const char * );

	// creating, querying, destroying externally defined object types
	int (*insert_object)( struct __lk_invoke_t*, const char *type, void*, void (*)(void *, void *), void * );
	void *(*query_object)( struct __lk_invoke_t*, int );
	void (*destroy_object)( struct __lk_invoke_t*, int );

	// invoking other functions in LK environment (i.e. for callbacks)
	void (*clear_call_args)( struct __lk_invoke_t* );
	lk_var_t (*append_call_arg)( struct __lk_invoke_t* );
	lk_var_t (*call_result)( struct __lk_invoke_t* );
	const char *(*call)( struct __lk_invoke_t*, const char *name ); // returns 0 on success, error message otherwise.
};


// function table must look like
typedef void (*lk_invokable)( struct __lk_invoke_t * );


#define LK_FUNCTION( name ) void name( struct __lk_invoke_t *lk )

#define LK_DOCUMENT( fn, desc, sig ) if (lk->doc_mode(lk)) { lk->document( lk, fn, desc, sig ); return; }
#define LK_DOCUMENT2( fn, notes, desc1, sig1, desc2, sig2 ) if (lk->doc_mode(lk)) { lk->document2( lk, fn, notes, desc1, sig1, desc2, sig2 ); return; }
#define LK_DOCUMENT3( fn, notes, desc1, sig1, desc2, sig2, desc3, sig3 ) if (lk->doc_mode(lk)) { lk->document3( lk, fn, notes, desc1, sig1, desc2, sig2, desc3, sig3 ); return; }

// helper access functions

#define lk_error( msg ) lk->error(lk, msg)
#define lk_arg_count( ) lk->arg_count(lk)
#define lk_arg( idx ) lk->arg(lk, idx)
#define lk_type( var ) lk->type(lk, var)
#define lk_as_string( var ) lk->as_string(lk, var)
#define lk_as_number( var ) lk->as_number(lk, var)
#define lk_as_integer( var ) lk->as_integer(lk, var)
#define lk_as_boolean( var ) lk->as_boolean(lk, var)
#define lk_length( var ) lk->vec_count(lk, var)
#define lk_index( var, idx ) lk->vec_index(lk, var, idx)
#define lk_table_size( var ) lk->tab_count(lk, var)
#define lk_first_key( var ) lk->tab_first_key(lk, var)
#define lk_next_key( var ) lk->tab_next_key(lk, var)
#define lk_value( var, key ) lk->tab_value(lk, var, key)
#define lk_result( ) lk->result(lk)
#define lk_return_number( val ) lk->set_number(lk, lk->result(lk), val )
#define lk_return_string( str ) lk->set_string(lk, lk->result(lk), str )	
#define lk_set_null( var ) lk->set_null(lk, var)
#define lk_set_string( var, str ) lk->set_string(lk, var, str)
#define lk_set_number( var, val ) lk->set_number(lk, var, val)
#define lk_set_number_array( var, arr, len ) lk->set_number_vec(lk, var, arr, len)
#define lk_make_array( var ) lk->make_vec(lk)
#define lk_reserve( var, len ) lk->reserve(lk, var, len)
#define lk_append_number( var, val ) lk->append_number(lk, var, val)
#define lk_append_string( var, str ) lk->append_string(lk, var, str)
#define lk_append_null( var ) lk->append_null(lk, var)
#define lk_make_table( var ) lk->make_tab(lk, var)
#define lk_table_set_number( var, key, val ) lk->tab_set_number( lk, var, key, val )
#define lk_table_set_string( var, key, str ) lk->tab_set_string( lk, var, key, str )
#define lk_table_set_null( var, key ) lk->tab_set_null(lk, var, key)
#define lk_insert_object( type, obj, freefunc, cbdata ) lk->insert_object(lk, type, obj, freefunc, cbdata)
#define lk_query_object( handle ) lk->query_object(lk, handle)
#define lk_destroy_object( handle ) lk->destroy_object(lk, handle)
#define lk_clear_call_args( ) lk->clear_call_args(lk)
#define lk_append_call_arg( ) lk->append_call_arg(lk)
#define lk_call_result( ) lk->call_result(lk)
#define lk_call( name ) lk->call(lk,name)

// DLL must export 2 functions:
// int lk_extension_api_version()
// lk_invokable *lk_function_list()

#define LK_EXTENSION_API_VERSION 1003


#if defined(__WINDOWS__)||defined(WIN32)||defined(_WIN32)||defined(__MINGW___)||defined(_MSC_VER)
#define LKAPIEXPORT __declspec(dllexport)
#else
#define LKAPIEXPORT
#endif


#define LK_BEGIN_EXTENSION() \
	LKAPIEXPORT int lk_extension_api_version() \
	{ return LK_EXTENSION_API_VERSION; } \
	LKAPIEXPORT lk_invokable *lk_function_list() { \
	static lk_invokable _ll[] = {

#define LK_END_EXTENSION() ,0 }; return _ll; }


/* examples: (these two functions are identical)

void mean_func( struct __lk_invoke_t *lk )
{
	LK_DOCUMENT( "mean", "Returns the average of an array of numbers.", "(array):number" );

	lk->set_number( lk->result(lk), 1.3 );
}

LK_FUNCTION( mean_func2 )
{
	LK_DOCUMENT( "mean2", "Returns the average of an array of numbers.", "(array):number" );

	lk_return_number( 1.3 );
}

LK_BEGIN_EXTENSION()
	mean_func, sigma_func, sum_func, 
	xmult_func, average_func
LK_END_EXTENSION()

*/



#ifdef __cplusplus
}
#endif

#endif
