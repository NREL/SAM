#include <cstring>

#include <lk/env.h>
#include <lk/invoke.h>


class extobj : public lk::objref_t
{
	void *m_pObject;
	void (*m_freeFunc)( void *, void * );
	void *m_userData;
	lk_string m_typeName;
public:
	extobj( const char *type, void *p, void (*freef)(void*, void*), void *ud )
		: objref_t()
	{
		m_typeName = lk_string(type);
		m_pObject = p;
		m_freeFunc = freef;
		m_userData = ud;
	}

	virtual ~extobj()
	{
		if (m_freeFunc) (*m_freeFunc)( m_pObject, m_userData );
	}

	void *obj() { return m_pObject; }
	
	virtual lk_string type_name() { return m_typeName; }
};


int _CC_doc_mode( struct __lk_invoke_t *_lk )
{
	return ((lk::invoke_t*)_lk->__pinvoke)->doc_mode() ? 1 : 0;	
}

void _CC_document( struct __lk_invoke_t *_lk, const char *fn, const char *desc, const char *sig )
{
	((lk::invoke_t*)_lk->__pinvoke)->document( lk::doc_t( fn, "", desc, sig ) );
}

void _CC_document2( struct __lk_invoke_t *_lk, const char *fn, const char *notes, 
	const char *desc1, const char *sig1, 
	const char *desc2, const char *sig2 )
{
	((lk::invoke_t*)_lk->__pinvoke)->document( lk::doc_t( fn, notes, desc1, sig1, desc2, sig2 ) );
}

void _CC_document3( struct __lk_invoke_t *_lk, const char *fn, const char *notes, 
	const char *desc1, const char *sig1, 
	const char *desc2, const char *sig2, 
	const char *desc3, const char *sig3 )
{
	((lk::invoke_t*)_lk->__pinvoke)->document( lk::doc_t( fn, notes, desc1, sig1, desc2, sig2, desc3, sig3 ) );
}

void _CC_error( struct __lk_invoke_t *_lk, const char *errmsg )
{
	strncpy((char*)_lk->__errbuf, errmsg, 255);
}

int _CC_arg_count( struct __lk_invoke_t *_lk )
{
	return ((lk::invoke_t*)_lk->__pinvoke)->arg_count();
}

lk_var_t _CC_arg( struct __lk_invoke_t *_lk, int idx )
{
	if ( idx < 0 || idx >= (int)((lk::invoke_t*)_lk->__pinvoke)->arg_count() )
		return 0;
	else
		return (lk_var_t)(& ((lk::invoke_t*)_lk->__pinvoke)->arg( idx ));
}

int _CC_type( struct __lk_invoke_t *_lk, lk_var_t vv)
{
	if (vv != 0) return ((lk::vardata_t*)vv)->type();
	else return 0;
}

const char *_CC_as_string( struct __lk_invoke_t *_lk, lk_var_t vv )
{
	if (vv != 0)
	{
		std::string *sbuf = ((std::string*)_lk->__sbuf);
		sbuf->assign( lk::to_utf8( ((lk::vardata_t*)vv)->as_string() ));
		return sbuf->c_str();
	}
	else return 0;
}

double _CC_as_number( struct __lk_invoke_t *_lk, lk_var_t vv )
{
	if (vv != 0)
		return ((lk::vardata_t*)vv)->as_number();
	else
		return 0.0;
}

int _CC_as_integer( struct __lk_invoke_t *_lk, lk_var_t vv )
{
	if (vv != 0)
		return ((lk::vardata_t*)vv)->as_integer();
	else
		return 0;
}

int _CC_as_boolean( struct __lk_invoke_t *_lk, lk_var_t vv )
{
	if (vv != 0)
		return ((lk::vardata_t*)vv)->as_boolean() ? 1 : 0;
	else
		return 0;
}

int _CC_vec_count( struct __lk_invoke_t *_lk, lk_var_t vv)
{
	if (vv != 0) return ((lk::vardata_t*)vv)->length();
	else return 0;
}

lk_var_t _CC_vec_index( struct __lk_invoke_t *_lk, lk_var_t vv, int idx )
{
	if (vv != 0) return (lk_var_t)((lk::vardata_t*)vv)->index(idx);
	else return 0;
}

int _CC_tab_count( struct __lk_invoke_t *_lk, lk_var_t vv )
{
	if (vv != 0)
		return ((lk::vardata_t*)vv)->hash()->size();
	else return 0;
}

const char * _CC_tab_first_key( struct __lk_invoke_t *_lk, lk_var_t vv )
{
	if (vv != 0)
	{
		lk::varhash_t::iterator *it = (lk::varhash_t::iterator*)_lk->__hiter;
		(*it) = ((lk::vardata_t*)vv)->hash()->begin();

		if ( *it != ((lk::vardata_t*)vv)->hash()->end() )
		{
			(*((std::string*)_lk->__sbuf)) = lk::to_utf8( (*(*it)).first );
			return ((std::string*)_lk->__sbuf)->c_str();
		}
	}
	
	return 0;
}

const char * _CC_tab_next_key( struct __lk_invoke_t *_lk, lk_var_t vv )
{
	if (vv != 0)
	{
		lk::varhash_t::iterator &it = *((lk::varhash_t::iterator*)_lk->__hiter);
		it++;
		if ( it != ((lk::vardata_t*)vv)->hash()->end() )
		{
			(*((std::string*)_lk->__sbuf)) = lk::to_utf8( (*it).first );
			return ((std::string*)_lk->__sbuf)->c_str();
		}
	}
	
	return 0;
}

lk_var_t _CC_tab_value( struct __lk_invoke_t *_lk, lk_var_t vv, const char *key )
{
	if (vv != 0 && key != 0)
		return (lk_var_t) ((lk::vardata_t*)vv)->lookup( key );
	else
		return 0;
}
	
lk_var_t _CC_result( struct __lk_invoke_t *_lk )
{
	return (lk_var_t) &(((lk::invoke_t*)_lk->__pinvoke)->result());
}

// variable modifications
void _CC_set_null( struct __lk_invoke_t *_lk, lk_var_t vv)
{
	if (vv != 0) ((lk::vardata_t*)vv)->nullify();
}

void _CC_set_string( struct __lk_invoke_t *_lk, lk_var_t vv, const char *str )
{
	if (vv != 0) ((lk::vardata_t*)vv)->assign( lk::from_utf8( str ) );
}

void _CC_set_number( struct __lk_invoke_t *_lk, lk_var_t vv, double val )
{
	if (vv != 0) ((lk::vardata_t*)vv)->assign( val );
}

void _CC_set_number_vec( struct __lk_invoke_t *_lk, lk_var_t vv, double *arr, int len )
{
	if (vv != 0 && arr != 0 && len > 0)
	{
		lk::vardata_t &v = *((lk::vardata_t*)vv);
		v.empty_vector();
		v.vec()->reserve( len );
		for (int i=0;i<len;i++)
			v.vec_append( arr[i] );
	}
}

void _CC_make_vec( struct __lk_invoke_t *_lk, lk_var_t vv)
{
	if (vv != 0) ((lk::vardata_t*)vv)->empty_vector();
}

void _CC_reserve( struct __lk_invoke_t *_lk, lk_var_t vv, int len )
{
	if (vv != 0 && len > 0)
	{
		lk::vardata_t &v = *((lk::vardata_t*)vv);
		if (v.type() != lk::vardata_t::VECTOR) v.empty_vector();
		v.vec()->reserve( len );
	}
}

lk_var_t _CC_append_number( struct __lk_invoke_t *_lk, lk_var_t vv, double val )
{
	if (vv != 0)
	{
		lk::vardata_t &v = *((lk::vardata_t*)vv);
		v.vec_append( val );
		return (lk_var_t) v.index( v.length()-1 );
	}
	else return 0;
}

lk_var_t _CC_append_string( struct __lk_invoke_t *_lk, lk_var_t vv, const char *str )
{
	if (vv != 0)
	{
		lk::vardata_t &v = *((lk::vardata_t*)vv);
		v.vec_append( lk::from_utf8( str ) );
		return (lk_var_t) v.index( v.length()-1 );
	}
	else return 0;
}

lk_var_t _CC_append_null( struct __lk_invoke_t *_lk, lk_var_t vv)
{
	if (vv != 0)
	{
		lk::vardata_t &v = *((lk::vardata_t*)vv);
		v.vec()->push_back( lk::vardata_t() );
		return (lk_var_t) v.index( v.length()-1 );
	}
	else return 0;
}

void _CC_make_tab( struct __lk_invoke_t *_lk, lk_var_t vv )
{
	if (vv != 0) ((lk::vardata_t*)vv)->empty_hash();
}

lk_var_t _CC_tab_set_number( struct __lk_invoke_t *_lk, lk_var_t vv, const char *key, double val)
{
	if (vv != 0)
	{
		lk_string ukey = lk::from_utf8( key );
		((lk::vardata_t*)vv)->hash_item( ukey, val );
		return (lk_var_t) ((lk::vardata_t*)vv)->lookup( ukey );
	}
	else return 0;
}

lk_var_t _CC_tab_set_string( struct __lk_invoke_t *_lk, lk_var_t vv, const char *key, const char *str )
{
	if (vv != 0) 
	{
		lk_string ukey = lk::from_utf8( key );
		((lk::vardata_t*)vv)->hash_item( ukey, lk::from_utf8( str ) );
		return (lk_var_t) ((lk::vardata_t*)vv)->lookup( ukey );
	}
	else return 0;
}

lk_var_t _CC_tab_set_null( struct __lk_invoke_t *_lk, lk_var_t vv, const char *key )
{
	if (vv != 0) 
	{
		lk_string ukey = lk::from_utf8( key );
		((lk::vardata_t*)vv)->hash_item( ukey, lk::vardata_t() );
		return (lk_var_t) ((lk::vardata_t*)vv)->lookup( ukey );
	}
	else return 0;
}

int _CC_insert_object( struct __lk_invoke_t*_lk, const char *type, void *obj, void (*free_func)(void *, void *), void *user_data )
{
	return (int) ((lk::invoke_t*)_lk)->env()->insert_object( new extobj( type, obj, free_func, user_data ) );
}

void *_CC_query_object( struct __lk_invoke_t *_lk, int handle)
{
	if ( extobj *p = dynamic_cast<extobj*>(((lk::invoke_t*)_lk)->env()->query_object( handle )) )
		return p->obj();
	else
		return 0;
}

void _CC_destroy_object( struct __lk_invoke_t *_lk, int handle )
{
	if ( extobj *p = dynamic_cast<extobj*>(((lk::invoke_t*)_lk)->env()->query_object( handle )) )
		((lk::invoke_t*)_lk)->env()->destroy_object( p );
}


void _CC_clear_call_args( struct __lk_invoke_t *_lk )
{
	((std::vector<lk::vardata_t>*)_lk->__callargvec)->clear();
}

lk_var_t _CC_append_call_arg( struct __lk_invoke_t *_lk )
{
	std::vector<lk::vardata_t> &args = *((std::vector<lk::vardata_t>*)_lk->__callargvec);
	args.push_back( lk::vardata_t() );
	return (lk_var_t) &args[args.size()-1];
}

lk_var_t _CC_call_result( struct __lk_invoke_t *_lk )
{
	return (lk_var_t) ((lk::vardata_t*)_lk->__callresult);
}

const char *_CC_call( struct __lk_invoke_t *_lk, const char *name )
{
	std::vector<lk::vardata_t> &args = *((std::vector<lk::vardata_t>*)_lk->__callargvec);
	lk::vardata_t &res = *((lk::vardata_t*)_lk->__callresult);

	try {
		((lk::invoke_t*)_lk->__pinvoke)->env()->call( name, args, res );
	} catch ( std::exception &e ) {
		((std::string*)_lk->__sbuf)->assign( e.what() );
		return ((std::string*)_lk->__sbuf)->c_str();
	}

	return 0;
}

namespace lk {

	void external_call( lk_invokable p, lk::invoke_t &cxt )
	{
		lk::varhash_t::iterator hash_iter;
		std::string local_str;
		std::vector< lk::vardata_t > extcall_argvec;
		lk::vardata_t extcall_result;

		char errbuf[256];
		errbuf[0] = 0;
		
		struct __lk_invoke_t ext_call;

		ext_call.__pinvoke = &cxt;
		ext_call.__hiter = &hash_iter;
		ext_call.__errbuf = errbuf;
		ext_call.__sbuf = &local_str;
		ext_call.__callargvec = &extcall_argvec;
		ext_call.__callresult = &extcall_result;

		ext_call.doc_mode = _CC_doc_mode;
		ext_call.document = _CC_document;
		ext_call.document2 = _CC_document2;
		ext_call.document3 = _CC_document3;
		ext_call.error = _CC_error;
		ext_call.arg_count = _CC_arg_count;
		ext_call.arg = _CC_arg;
		ext_call.type = _CC_type;
		ext_call.as_string = _CC_as_string;
		ext_call.as_integer = _CC_as_integer;
		ext_call.as_number = _CC_as_number;
		ext_call.as_boolean = _CC_as_boolean;
		ext_call.vec_count = _CC_vec_count;
		ext_call.vec_index = _CC_vec_index;
		ext_call.tab_count = _CC_tab_count;
		ext_call.tab_first_key = _CC_tab_first_key;
		ext_call.tab_next_key = _CC_tab_next_key;
		ext_call.tab_value = _CC_tab_value;
		ext_call.result = _CC_result;
		ext_call.set_null = _CC_set_null;
		ext_call.set_string = _CC_set_string;
		ext_call.set_number = _CC_set_number;
		ext_call.set_number_vec = _CC_set_number_vec;
		ext_call.make_vec = _CC_make_vec;
		ext_call.reserve = _CC_reserve;
		ext_call.append_number = _CC_append_number;
		ext_call.append_string = _CC_append_string;
		ext_call.append_null = _CC_append_null;
		ext_call.make_tab = _CC_make_tab;
		ext_call.tab_set_number = _CC_tab_set_number;
		ext_call.tab_set_string = _CC_tab_set_string;
		ext_call.tab_set_null = _CC_tab_set_null;
		ext_call.insert_object = _CC_insert_object;
		ext_call.query_object = _CC_query_object;
		ext_call.destroy_object = _CC_destroy_object;
		ext_call.clear_call_args = _CC_clear_call_args;
		ext_call.append_call_arg = _CC_append_call_arg;
		ext_call.call_result = _CC_call_result;
		ext_call.call = _CC_call;

		// call the function with the pointer
		p( &ext_call );

		// throw an error exception if necessary
		if ( errbuf[0] != 0 )
			cxt.error( lk_string( errbuf ) + "\n" );
	}
};
