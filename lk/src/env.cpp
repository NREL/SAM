#include <algorithm>
#include <cstring>
#include <cstdlib>
#include <limits>

#include <lk/env.h>
#include <lk/eval.h>

#if defined(__WINDOWS__)||defined(WIN32)||defined(_WIN32)||defined(__MINGW___)||defined(_MSC_VER)
#include <Windows.h>
static void *dll_open(const char *name) { return (void*) ::LoadLibraryA( name ); }
static void dll_close( void *handle ) { ::FreeLibrary( (HMODULE)handle ); }
static void *dll_sym( void *handle, const char *name ) { return (void*) ::GetProcAddress( (HMODULE)handle, name ); }
#else
#include <dlfcn.h>
static void *dll_open(const char *name) { return dlopen( name, RTLD_NOW ); }
static void dll_close( void *handle ) { dlclose( handle ); }
static void *dll_sym( void *handle, const char *name ) { return dlsym( handle, name ); }
#endif

lk::vardata_t::vardata_t()
{
	m_type = 0;
	set_type( NULLVAL );
}

lk::vardata_t::vardata_t(const vardata_t &cp)
{
	m_type = 0;
	set_type( NULLVAL );
	copy( const_cast<vardata_t&>(cp) );
}

lk::vardata_t::~vardata_t()
{
	nullify();
}

/* private member functions */
void lk::vardata_t::set_type( unsigned char ty )
{
	m_type &= FLAGMASK; // clear all type info
	m_type |= (ty&TYPEMASK); // set lower 4 bits to type
}
		
void lk::vardata_t::assert_modify() throw( error_t )
{
	if ( flagval( CONSTVAL )
		&& flagval( ASSIGNED ) )
	{
		throw error_t( lk_tr("cannot modify a constant value") );
	}

	set_flag( ASSIGNED );
}


/* public interface */

bool lk::vardata_t::as_boolean() const
{
	if (type() == NUMBER
		&& m_u.v == 0.0) return false;

	if (type() == STRING)
	{
		lk_string slower = str();
		for( size_t i=0;i<slower.length();i++ )
			slower[i] = lk::lower_char( slower[i] );

		if (slower == "false" || slower == "f") return false;
	}

	if (type() == REFERENCE) return deref().as_boolean();

	if (type() == NULLVAL) return false;

	return true;
}

unsigned int lk::vardata_t::as_unsigned() const
{
	return (unsigned int)as_number();
}

int lk::vardata_t::as_integer() const
{
	return (int)as_number();
}

lk_string lk::vardata_t::as_string() const
{
	switch ( type() )
	{
	case NULLVAL: return "<null>";
	case REFERENCE: return deref().as_string();
	case NUMBER:
		{
			char buf[512];
			if ( ((double)((int)m_u.v)) == m_u.v )
				sprintf(buf, "%d", (int) m_u.v);
			else
				sprintf(buf, "%lg", m_u.v);
			return lk_string(buf);
		}
	case STRING:
		return *reinterpret_cast< lk_string* >(m_u.p);
	case VECTOR:
		{
			std::vector<vardata_t> &v = *reinterpret_cast< std::vector<vardata_t>* >(m_u.p);

			lk_string s( "[ " );
			for ( size_t i=0;i<v.size();i++ )
			{
				s += v[i].as_string();
				if ( v.size() > 1 && i < v.size()-1 )
					s += ", ";
			}

			s += " ]";
			
			return s;
		}
	case HASH:
		{
			varhash_t &h = *reinterpret_cast< varhash_t* >(m_u.p);
			lk_string s("{ ");

			for ( varhash_t::iterator it = h.begin(); it != h.end(); ++it )
			{
				s += it->first;
				s += "=";
				s += it->second->as_string();
				s += " ";
			}

			s += "}";

			return s;
		}
	case INTFUNC:
	case EXTFUNC:
	case FUNCTION:
		return "<function>";
	default:
		return "<unknown>";
	}
}

static double my_atof( const char *s )
{
#define MAXBUF 128

	char buf[MAXBUF];
	char *p = buf;
	while( *s && (p-buf) < MAXBUF-1 )
	{
		if ( *s != ',' )
			*p++ = *s;

		s++;
	}
	
	*p = 0;

	return ::atof( buf );

#undef MAXBUF
}

double lk::vardata_t::as_number() const
{
	switch( type() )
	{
	case NULLVAL: return 0;
	case NUMBER: return m_u.v;
	case STRING: return my_atof( (const char*) str().c_str() );
	case REFERENCE: return deref().as_number();
	default:
		return std::numeric_limits<double>::quiet_NaN();
	}
}

void lk::vardata_t::deep_localize()
{
	switch( type() )
	{
	case REFERENCE:
		copy( deref() );
		break;
	case VECTOR:
	{
		for( size_t i=0;i<length();i++ )
			index(i)->deep_localize();
	}
		break;
	case HASH:
	{
		varhash_t &hh = *hash();
		for( varhash_t::iterator it = hh.begin();
			it != hh.end();
			++it )
			it->second->deep_localize();
	}
		break;
		
	}
}

bool lk::vardata_t::copy( vardata_t &rhs ) throw( error_t )
{
	switch( rhs.type() )
	{
	case NULLVAL:
		assert_modify();
		nullify();
		return true;
	case REFERENCE:
		assert_modify();
		nullify();
		set_type( REFERENCE );
		if ( rhs.m_u.p == this )
			throw error_t( lk_tr("internal error: copying self-referential reference") );
		m_u.p = rhs.m_u.p;
		return true;
	case NUMBER:
		assign( rhs.m_u.v );
		return true;
	case STRING:
		assign( rhs.str() );
		return true;
	case VECTOR:
		{
			resize( rhs.length() );
			std::vector<vardata_t> &v = *reinterpret_cast< std::vector<vardata_t>* >(m_u.p);
			std::vector<vardata_t> *rv = rhs.vec();
			for (size_t i=0;i<v.size();i++)
				v[i].copy( (*rv)[i] );
		}
		return true;
	case HASH:
		{
			empty_hash();			
			varhash_t &h = (*reinterpret_cast< varhash_t* >(m_u.p));
			varhash_t &rh = (*reinterpret_cast< varhash_t* >(rhs.m_u.p));

			for (varhash_t::iterator it = rh.begin();
				it != rh.end();
				++it)
			{
				vardata_t *cp = new vardata_t;
				cp->copy( *it->second );
				h[ (*it).first ] = cp;
			}
			
		}
		return true;
		
	case EXTFUNC:		
	case INTFUNC:		
	case FUNCTION:		
		assert_modify();
		nullify();
		set_type( rhs.type() );
		m_u.p = rhs.m_u.p;
		return true;

	default:
		return false;
	}
}

bool lk::vardata_t::equals(vardata_t &rhs) const
{
	if (type() != rhs.type()) return false;

	switch( type() )
	{
	case NULLVAL:
		return true;

	case NUMBER:
		return m_u.v == rhs.m_u.v;

	case STRING:
		return str() == rhs.str();		

	case VECTOR:
	{
		size_t len = vec()->size();
		if ( len != rhs.vec()->size() )
			return false;
		for( size_t i=0;i<len;i++ )
			if ( !(*vec())[i].equals( (*rhs.vec())[i] ) )
				return false;

		return true;
	}
		break;

	case HASH:
	{
		varhash_t *h1 = hash();
		varhash_t *h2 = rhs.hash();

		// if number of pairs is different, not equal
		if ( h1->size() != h2->size() )
			return false;

		for( varhash_t::iterator it = h1->begin();
			it != h1->end();
			++it )
		{
			// if second hash doesn't have this key, not equal
			varhash_t::iterator it2 = h2->find( it->first );
			if ( it2 == h2->end() )
				return false;

			// if the values of this key are different, not equal
			if ( ! it->second->equals( *it2->second ) )
				return false;
		}

		return true;
	}
		break;

	case FUNCTION:
		return func() == rhs.func();

	case INTFUNC:
		return faddr() == rhs.faddr();

	case EXTFUNC:
		return fcall() == rhs.fcall();

	default:
		return false;
	}
}

bool lk::vardata_t::lessthan(vardata_t &rhs) const
{
	if (type() != rhs.type()) return false;

	switch( type() )
	{
	case NUMBER:
		return m_u.v < rhs.m_u.v;
	case STRING:
		return str() < rhs.str();
	default:
		return false;
	}
}

const char *lk::vardata_t::typestr() const
{
	switch( type() )
	{
	case NULLVAL: return "null";
	case REFERENCE: return "reference";
	case NUMBER: return "number";
	case STRING: return "string";
	case VECTOR: return "array";
	case HASH: return "table";
	case INTFUNC:
	case EXTFUNC:
	case FUNCTION:
		return "function";
	default: return "unknown";
	}
}

void lk::vardata_t::nullify()
{
	switch( type() )
	{
	case STRING:
		delete reinterpret_cast<lk_string*>(m_u.p);
		break;
	case HASH:
		{
			varhash_t *h = reinterpret_cast<varhash_t*>(m_u.p);
			for (varhash_t::iterator it = h->begin();
				it != h->end();
				++it)
				delete it->second;
			delete h;
		}
		break;
	case VECTOR:
		delete reinterpret_cast<std::vector<vardata_t>*>(m_u.p);
		break;

	// note: functions not deleted here because they 
	// are pointers into the abstract syntax tree
	}

	set_type( NULLVAL );
}

void lk::vardata_t::assign( double d ) throw( error_t )
{
	assert_modify();

	nullify();
	set_type( NUMBER );
	m_u.v = d;
}

void lk::vardata_t::assign( const char *s ) throw( error_t )
{
	assert_modify();

	if ( type() != STRING )
	{
		nullify();
		set_type( STRING );
		m_u.p = new lk_string(s);
	}
	else
	{
		*reinterpret_cast<lk_string*>(m_u.p) = s;
	}
}

void lk::vardata_t::assign( const lk_string &s ) throw( error_t )
{
	assert_modify();

	if ( type() != STRING)
	{
		nullify();
		set_type( STRING );
		m_u.p = new lk_string(s);
	}
	else
	{
		*reinterpret_cast<lk_string*>(m_u.p) = s;
	}
}

void lk::vardata_t::empty_vector()  throw( error_t )
{
	assert_modify();

	nullify();
	set_type( VECTOR );
	m_u.p = new std::vector<vardata_t>;
}

void lk::vardata_t::empty_hash() throw( error_t )
{
	assert_modify();

	nullify();
	set_type( HASH );
	m_u.p = new varhash_t;
}

void lk::vardata_t::assign( const lk_string &key, vardata_t *val ) throw( error_t )
{
	assert_modify();

	if ( type() != HASH )
	{
		nullify();
		set_type( HASH );
		m_u.p = new varhash_t;
	}

	(*reinterpret_cast< varhash_t* >(m_u.p))[ key ] = val;
}

void lk::vardata_t::unassign( const lk_string &key ) throw( error_t )
{
	assert_modify();

	if (type() != HASH) return;

	varhash_t &h = (*reinterpret_cast< varhash_t* >(m_u.p));
			
	varhash_t::iterator it = h.find( key );
	if (it != h.end())
	{
		delete (*it).second; // delete the associated data
		h.erase( it );
	}
}

void lk::vardata_t::assign( expr_t *func ) throw( error_t )
{
	assert_modify();

	nullify();
	set_type( FUNCTION );
	m_u.p = func;
}

void lk::vardata_t::assign( vardata_t *ref ) throw( error_t )
{
	assert_modify();

	nullify();
	set_type( REFERENCE );
	if ( ref == this )
		throw error_t( lk_tr("internal error: assigning self-referential reference") );
	m_u.p = ref;
}

void lk::vardata_t::assign_fcall( fcallinfo_t *fci ) throw( error_t )
{
	assert_modify();

	nullify();
	set_type( EXTFUNC );
	m_u.p = fci;
}


void lk::vardata_t::assign_faddr( size_t ip ) throw( error_t )
{
	assert_modify();

	nullify();
	set_type( INTFUNC );
	m_u.p = (void*)ip;
}


void lk::vardata_t::resize( size_t n ) throw( error_t )
{
	assert_modify();

	if ( type() != VECTOR )
	{
		nullify();
		set_type( VECTOR );
		m_u.p = new std::vector<vardata_t>;
	}

	reinterpret_cast<std::vector<vardata_t>*>(m_u.p)->resize( n );
}


double lk::vardata_t::num() const throw(error_t) 
{
	if ( type() != NUMBER ) throw error_t( lk_tr("access violation: expected numeric, but found") + " " + typestr() );
	return m_u.v;
}

lk_string lk::vardata_t::str() const throw(error_t)
{
	if ( type() != STRING ) throw error_t( lk_tr("access violation: expected string, but found") + " " + typestr() );
	return *reinterpret_cast<lk_string*>(m_u.p);
}

lk::vardata_t *lk::vardata_t::ref() const
{
	if ( type() != REFERENCE )
		return 0;
	else
		return reinterpret_cast<vardata_t*>(m_u.p);
}

std::vector<lk::vardata_t> *lk::vardata_t::vec() const throw(error_t)
{
	if (type() != VECTOR) throw error_t( lk_tr("access violation: expected array, but found ") + " " + typestr() );
	return reinterpret_cast< std::vector<vardata_t>* >(m_u.p);
}

void lk::vardata_t::vec_append( double d ) throw(error_t)
{
	assert_modify();

	vardata_t v;
	v.assign(d);
	vec()->push_back( v );
}

void lk::vardata_t::vec_append( const lk_string &s ) throw(error_t)
{
	assert_modify();

	vardata_t v;
	v.assign(s);
	vec()->push_back( v );
}

size_t lk::vardata_t::length() const
{
	switch(type())
	{
	case VECTOR:
		return reinterpret_cast<std::vector<vardata_t>* >(m_u.p)->size();
	default:
		return 0;
	}
}

lk::expr_t *lk::vardata_t::func() const throw(error_t)
{
	if (type() != FUNCTION) throw error_t( lk_tr("access violation: expected code expression pointer, but found") + " " + typestr() );
	return reinterpret_cast< expr_t* >(m_u.p);
}

lk::fcallinfo_t *lk::vardata_t::fcall() const throw(error_t)
{
	if (type() != EXTFUNC) throw error_t( lk_tr("access violation: expected external function pointer, but found") + " " + typestr() );
	return reinterpret_cast< fcallinfo_t* >(m_u.p);
}

size_t lk::vardata_t::faddr() const throw(error_t)
{
	if (type() != INTFUNC) throw error_t( lk_tr("access violation: expected internal function pointer, but found") + " " + typestr() );
	return reinterpret_cast< size_t >(m_u.p);
}

lk::varhash_t *lk::vardata_t::hash() const throw(error_t)
{
	if ( type() != HASH ) throw error_t( lk_tr("access violation: expected hash table, but found") + " " + typestr() );
	return reinterpret_cast< varhash_t* > (m_u.p);
}

void lk::vardata_t::hash_item( const lk_string &key, double d ) throw(error_t)
{
	assert_modify();

	varhash_t *h = hash();
	varhash_t::iterator it = h->find(key);
	if (it != h->end())
		(*it).second->assign(d);
	else
	{
		vardata_t *t = new vardata_t;
		t->assign(d);
		(*h)[key] = t;
	}
}

void lk::vardata_t::hash_item( const lk_string &key, const lk_string &s ) throw(error_t)
{
	assert_modify();

	varhash_t *h = hash();
	varhash_t::iterator it = h->find(key);
	if (it != h->end())
		(*it).second->assign(s);
	else
	{
		vardata_t *t = new vardata_t;
		t->assign(s);
		(*h)[key] = t;
	}
}

void lk::vardata_t::hash_item( const lk_string &key, const vardata_t &v ) throw(error_t)
{
	assert_modify();

	varhash_t *h = hash();
	varhash_t::iterator it = h->find(key);
	if (it != h->end())
		(*it).second->copy( const_cast<vardata_t&>(v) );
	else
	{
		vardata_t *t = new vardata_t;
		t->copy( const_cast<vardata_t&>(v) );
		(*h)[key] = t;
	}
}

lk::vardata_t &lk::vardata_t::hash_item( const lk_string &key ) throw(error_t)
{
	assert_modify();
	
	varhash_t *h = hash();
	varhash_t::iterator it = h->find(key);
	if (it != h->end())
	{
		(*it).second->nullify();
		return *(*it).second;
	}
	else
	{
		vardata_t *t = new vardata_t;
		(*h)[key] = t;
		return *t;
	}
}


lk::vardata_t *lk::vardata_t::index(size_t idx) const throw(error_t)
{
	if (type() != VECTOR) throw error_t( lk_tr("access violation: expected array for indexing, but found") + " " + typestr() );
	std::vector<vardata_t> &m = *reinterpret_cast< std::vector<vardata_t>* >(m_u.p);
	if (idx >= m.size()) throw error_t( (const char*)lk_tr("array index out of bounds at %d (length: %d)").c_str(), (int)idx, (int)m.size());

	return &m[idx];
}

lk::vardata_t *lk::vardata_t::lookup( const lk_string &key ) const throw(error_t)
{
	if (type() != HASH) throw error_t( lk_tr("access violation: expected hash table, but found") + " " + typestr() );
	varhash_t &h = *reinterpret_cast< varhash_t* >(m_u.p);
	varhash_t::iterator it = h.find( key );
	if ( it != h.end() )
		return (*it).second;
	else
		return 0;
}


lk::env_t::env_t() : m_parent(0), m_varIter(m_varHash.begin()) {  }
lk::env_t::env_t(env_t *p) : m_parent(p), m_varIter(m_varHash.begin()) {  }

lk::env_t::~env_t()
{
	clear_objs();
	clear_vars();

	// unload any extension dlls	
	for ( std::vector<dynlib_t>::iterator it = m_dynlibList.begin();
		it != m_dynlibList.end();
		++it )
		dll_close( (*it).handle );

	m_dynlibList.clear();
}

void lk::env_t::clear_objs()
{

	// delete the referenced objects
	for ( size_t i=0;i<m_objTable.size();i++ )
		if ( m_objTable[i] )
			delete m_objTable[i];

	m_objTable.clear();
}

void lk::env_t::clear_vars()
{
	for ( varhash_t::iterator it = m_varHash.begin(); it !=m_varHash.end(); ++it )
		delete it->second; // delete the var_data object
	m_varHash.clear();
}

void lk::env_t::assign( const lk_string &name, vardata_t *value )
{
	vardata_t *x = lookup(name, false);
			
	if (x && x!=value)
		delete x;

	m_varHash[name] = value;
}

void lk::env_t::unassign( const lk_string &name )
{
	varhash_t::iterator it = m_varHash.find( name );
	if (it != m_varHash.end())
	{
		delete (*it).second; // delete the associated data
		m_varHash.erase( it );
	}
}

lk::vardata_t *lk::env_t::lookup( const lk_string &name, bool search_hierarchy )
{
	varhash_t::iterator it = m_varHash.find( name );
	if ( it != m_varHash.end() )
		return (*it).second;
	else if (search_hierarchy && m_parent)
		return m_parent->lookup( name, true );
	else
		return 0;
}
		
bool lk::env_t::first( lk_string &key, vardata_t *&value )
{
	m_varIter = m_varHash.begin();
	if (m_varIter != m_varHash.end())
	{
		key = m_varIter->first;
		value = m_varIter->second;
		return true;
	}
	else return false;
}

bool lk::env_t::next( lk_string &key, vardata_t *&value )
{
	if (m_varIter == m_varHash.end()) return false;

	++m_varIter;

	if (m_varIter != m_varHash.end())
	{
		key = m_varIter->first;
		value = m_varIter->second;
		return true;
	}

	return false;
}

void lk::env_t::set_parent( env_t *p )
{
	m_parent = p;
}

lk::env_t *lk::env_t::parent()
{
	return m_parent;
}

lk::env_t *lk::env_t::global()
{
	env_t *p = this;

	while (p->parent())
		p = p->parent();

	return p;
}

unsigned int lk::env_t::size()
{
	return m_varHash.size();
}

bool lk::env_t::register_ext_func( lk_invokable f, void *user_data )
{
	lk::doc_t d;
	if ( lk::doc_t::info(f, d) && !d.func_name.empty())
	{
		fcallinfo_t x;
		x.f = 0;
		x.f_ext = f;
		x.user_data = user_data;
		m_funcHash[d.func_name] = x;
		return true;
	}

	return false;
}

void lk::env_t::unregister_ext_func( lk_invokable f )
{
	for ( lk::funchash_t::iterator it = m_funcHash.begin();
		it != m_funcHash.end();
		it++ )
	{
		if ( (*it).second.f_ext == f )
			m_funcHash.erase( it );
	}
}


bool lk::env_t::register_func( fcall_t f, void *user_data )
{
	lk::doc_t d;
	if ( lk::doc_t::info(f, d) && !d.func_name.empty())
	{
		fcallinfo_t x;
		x.f = f;
		x.f_ext = 0;
		x.user_data = user_data;
		m_funcHash[d.func_name] = x;
		return true;
	}

	return false;
}

bool lk::env_t::register_funcs( std::vector<fcall_t> l, void *user_data )
{
	bool ok = true;
	for (size_t i=0;i<l.size();i++)
		ok = ok && register_func( l[i], user_data );
	return ok;
}

bool lk::env_t::register_funcs( fcall_t list[], void *user_data )
{
	bool ok = true;
	int idx = 0;
	while( ok && list[idx] != 0 )
		ok = ok && register_func( list[idx++], user_data );
	return ok;
}

lk::fcallinfo_t *lk::env_t::lookup_func( const lk_string &name )
{
	funchash_t::iterator it = m_funcHash.find( name );
	if ( it != m_funcHash.end() )
	{
		return &(*it).second;
	}
	else if ( m_parent )
	{
		return m_parent->lookup_func( name );
	}
	else
	{
		return 0;
	}
}

std::vector<lk_string> lk::env_t::list_funcs()
{
	std::vector<lk_string> list;
	for (funchash_t::iterator it = m_funcHash.begin();
			it != m_funcHash.end();
			++it)
		list.push_back( (*it).first );

	return list;
}

size_t lk::env_t::insert_object( objref_t *o )
{
	if ( env_t *g = global() )
	{
		o->m_env = g;

		int iempty = -1;
		for( size_t i=0;i<g->m_objTable.size();i++ )
		{
			if ( g->m_objTable[i] == o )
				return i+1;

			if ( g->m_objTable[i] == 0 && iempty < 0 )
				iempty = (int) i;			
		}

		if( iempty >= 0 )
		{
			g->m_objTable[iempty] = o;
			return (size_t)(iempty+1);
		}

		g->m_objTable.push_back( o );
		return g->m_objTable.size();
	}
	else
		return 0;
}

bool lk::env_t::destroy_object( objref_t *o )
{
	if ( env_t *g = global() )
	{
		std::vector< objref_t* >::iterator pos = std::find(g->m_objTable.begin(), g->m_objTable.end(), o);
		if (pos != g->m_objTable.end())
		{
			if (*pos != 0)
				delete (*pos);

			*pos = 0;
			return true;
		}
		else
			return false;
	}
	else return false;
}

lk::objref_t *lk::env_t::query_object( size_t ref )
{
	if ( env_t *g = global() )
	{
		ref--;
		if (ref < g->m_objTable.size())
			return g->m_objTable[ref];
		else
			return 0;
	}
	else return 0;
}

void lk::env_t::call( const lk_string &name, std::vector< vardata_t > &args, vardata_t &result ) throw( lk::error_t )
{
	vardata_t *f = lookup(name, true);
	if (!f)	throw lk::error_t( lk_tr("could not locate function name in environment: ") + name );

	if ( expr_t *def = dynamic_cast<expr_t*>( f->deref().func() ))
	{
		list_t *argnames = dynamic_cast<list_t*>( def->left );
		node_t *block = def->right;

		env_t frame( this );

		int nargs_expected = 0;
		if ( argnames ) nargs_expected = argnames->items.size();

		int nargs_given = args.size();
		if (nargs_given < nargs_expected)
			throw error_t( lk_tr("too few arguments provided in env::call internal method to function: ") + name );

		vardata_t *__args = new vardata_t;
		__args->empty_vector();

		for (size_t aidx=0; aidx < args.size(); aidx++ )
		{
			__args->vec()->push_back( vardata_t( args[aidx] ) );

			if ( argnames && aidx < argnames->items.size() )
			{
				if (iden_t *id = dynamic_cast<iden_t*>(argnames->items[aidx]))
					frame.assign( id->name, new vardata_t( args[aidx] ));
			}
		}

		frame.assign("__args", __args);


		lk::eval ev( block, &frame );
		if ( !ev.run() )
			throw error_t( lk_tr("error inside function call invoked from env::call") );
	}
	else
		throw error_t( lk_tr("function call fail: could not locate internal pointer to ") + name);
}

bool lk::env_t::load_library( const lk_string &path )
{
	for ( std::vector<dynlib_t>::iterator it = m_dynlibList.begin();
		it != m_dynlibList.end();
		++it )
		if ( (*it).path == path ) return true;

	dynlib_t x;
	x.path = path;
	void *pdll = ::dll_open( path.c_str() );
	if (!pdll)
		return false;

	int (*verfunc)() = ( int(*)() ) dll_sym( pdll, "lk_extension_api_version" );
	if ( verfunc == 0 )
	{
		dll_close( pdll );
		throw error_t( lk_tr("could not locate symbol") + " 'lk_extension_api_version'\n");
	}

	int ver = verfunc();
	if (ver != LK_EXTENSION_API_VERSION)
	{
		dll_close( pdll );
		throw error_t( (const char*)lk_tr("invalid extension version: %d (engine api: %d)\n").c_str(), ver, LK_EXTENSION_API_VERSION);
	}

	lk_invokable *(*listfunc)() = (lk_invokable*(*)())dll_sym( pdll, "lk_function_list" );
	if (listfunc == 0)
	{
		dll_close( pdll );
		throw error_t( lk_tr("could not locate symbol") + " 'lk_function_list'\n");
	}


	x.handle = pdll;
	x.functions = listfunc();
			
	int idx = 0;
	while ( x.functions[idx] != 0 )
	{
		global()->register_ext_func( x.functions[idx], 0 );
		idx ++;
	}

	m_dynlibList.push_back( x );
	return true;
}

std::vector<lk::env_t::dynlib_t*> lk::env_t::libraries()
{
	std::vector<lk::env_t::dynlib_t*> list;
	for ( std::vector<dynlib_t>::iterator it = m_dynlibList.begin();
		it != m_dynlibList.end();
		++it )
		list.push_back( &(*it) );
	return list;
}

bool lk::env_t::unload_library( const lk_string &path )
{
	for ( std::vector<dynlib_t>::iterator it = m_dynlibList.begin();
		it != m_dynlibList.end();
		++it )
	{
		if ( (*it).path == path )
		{
			lk_invokable *list = (*it).functions;
			while (*list != 0)
			{
				global()->unregister_ext_func( *list );
				list++;
			}

			dll_close( (*it).handle );

			m_dynlibList.erase( it );
			return true;
		}
	}

	return false;
}

bool lk::doc_t::info( fcallinfo_t *f, doc_t &d )
{
	if (f!=0)
	{
		lk::vardata_t dummy_var;
		lk::invoke_t cxt( 0, dummy_var, 0);
		cxt.m_docPtr = &d; // possible b/c friend class
		d.m_ok = false;

		 // each function begins LK_DOC which calls invoke_t::document(..), should set m_ok to true
		if (f->f) (*(f->f))( cxt );
		else external_call( f->f_ext, cxt );

		return d.m_ok;
	}
	else
		return false;
}


bool lk::doc_t::info( fcall_t f, doc_t &d )
{
	if (f!=0)
	{
		lk::vardata_t dummy_var;
		lk::invoke_t cxt( 0, dummy_var, 0);
		cxt.m_docPtr = &d; // possible b/c friend class
		d.m_ok = false;
		(*f)( cxt ); // each function begins LK_DOC which calls invoke_t::document(..), should set m_ok to true
		return d.m_ok;
	}
	else
		return false;
}


bool lk::doc_t::info( lk_invokable f, doc_t &d )
{
	if (f!=0)
	{
		lk::vardata_t dummy_var;
		lk::invoke_t cxt( 0, dummy_var, 0);
		cxt.m_docPtr = &d; // possible b/c friend class
		d.m_ok = false;
		lk::external_call( f, cxt ); // each function begins LK_DOC which calls invoke_t::document(..), should set m_ok to true
		return d.m_ok;
	}
	else
		return false;
}

void lk::doc_t::copy_data( lk::doc_t *p )
{
	if (p == 0) return;
	func_name = p->func_name;
	notes = p->notes;
	desc1 = p->desc1;
	desc2 = p->desc2;
	desc3 = p->desc3;
	sig1 = p->sig1;
	sig2 = p->sig2;
	sig3 = p->sig3;
	has_2 = p->has_2;
	has_3 = p->has_3;
}

bool lk::invoke_t::doc_mode()
{
	return m_docPtr != 0;
}

void lk::invoke_t::document(doc_t d)
{
	if (m_docPtr != 0)
	{
		m_docPtr->copy_data(&d);
		m_docPtr->m_ok = true;
	}
}
