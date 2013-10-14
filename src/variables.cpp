#include <wx/datstrm.h>
#include <wx/tokenzr.h>

#include <lk_parse.h>
#include <lk_env.h>
#include <lk_stdlib.h>
#include <lk_eval.h>


#include "variables.h"



VarTable::VarTable()
{
	/* nothing to do */
}

VarTable::~VarTable()
{
	clear();
}

void VarTable::clear()
{
	for( iterator it = begin(); it != end(); ++it )
		delete it->second;

	VarTableBase::clear();
}

wxArrayString VarTable::ListAll( std::vector<VarValue*> *vals )
{
	wxArrayString list;
	for( iterator it = begin(); it != end(); ++it )
	{
		list.Add( it->first );
		if ( vals ) vals->push_back( it->second );
	}
	return list;
}

VarValue *VarTable::Set( const wxString &name, const VarValue &val )
{
	VarValue *vv = 0;
	iterator it = find( name );
	if ( it == end() )
	{
		vv = new VarValue(val);
		(*this)[ name ] = vv;
	}
	else
	{
		it->second->Copy(val);
		vv = it->second;
	}
	return vv;
}

VarValue *VarTable::Get( const wxString &name )
{
	iterator it = find( name );
	if ( it == end() ) return 0;
	else return it->second;
}

void VarTable::Copy( const VarTable &rhs )
{
	clear();
	for( const_iterator it = rhs.begin();
		it != rhs.end();
		++it )
		Set( it->first, *(it->second) );
}

void VarTable::Write( wxOutputStream &_O )
{
	wxDataOutputStream out(_O);
	out.Write8( 0xf9 );
	out.Write8( 1 );

	out.Write32( size() );
	for( iterator it = begin(); it != end(); ++it )
	{
		out.WriteString( it->first );
		it->second->Write( _O );
	}

	out.Write8( 0xf9 );
}

bool VarTable::Read( wxInputStream &_I )
{
	clear();
	
	wxDataInputStream in(_I);
	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8();

	bool ok = true;
	size_t n = in.Read32();
	for ( size_t i=0;i<n;i++ )
	{
		wxString name = in.ReadString();
		VarValue *value = new VarValue;
		ok = ok && value->Read( _I );
		
		if ( find(name) == end() ) (*this)[name] = value;
		else delete value;
	}
	
	return in.Read8() == code;
}

VarValue::VarValue()
{
	m_type = VV_INVALID;
}

VarValue::VarValue( int i )
{
	m_type = VV_NUMBER;
	m_val = (float)i;
}

VarValue::VarValue( float f )
{
	m_type = VV_NUMBER;
	m_val = f;
}

VarValue::VarValue( float *arr, size_t n )
{
	m_type = VV_ARRAY;
	m_val.assign( arr, n );
}

VarValue::VarValue( float *mat, size_t r, size_t c )
{
	m_type = VV_MATRIX;
	m_val.assign( mat, r, c );
}

VarValue::VarValue( const ::matrix_t<float> &m )
{
	m_type = VV_MATRIX;
	m_val = m;
}

VarValue::VarValue( const wxString &s )
{
	m_type = VV_STRING;
	m_str = s;
}

VarValue::VarValue( const VarTable &t )
{
	m_type = VV_TABLE;
	m_tab.Copy( t );
}

VarValue::~VarValue()
{
	// nothing to do 
}

VarValue &VarValue::operator=( const VarValue &rhs )
{
	Copy( rhs );
	return *this;
}

void VarValue::Copy( const VarValue &rhs )
{
	m_type = rhs.m_type;
	m_str = rhs.m_str;
	m_val = rhs.m_val;
	m_tab = rhs.m_tab;
}


void VarValue::Write( wxOutputStream &_O )
{
	wxDataOutputStream out( _O );

	out.Write8( 0xf2 );
	out.Write8( 1 );

	out.Write8( m_type );
	
	switch( m_type )
	{
	case VV_INVALID:
		break; // no data to be written
	case VV_NUMBER:
	case VV_ARRAY:
	case VV_MATRIX:
		out.Write32( m_val.nrows() );
		out.Write32( m_val.ncols() );
		for ( size_t r=0;r<m_val.nrows();r++ )
			for( size_t c=0;c<m_val.ncols();c++ )
				out.WriteFloat( m_val(r,c) );
		break;
	case VV_TABLE:
		m_tab.Write( _O );
		break;
	case VV_STRING:
		out.WriteString( m_str );
		break;
	}

	out.Write16( 0xf2 );
}

bool VarValue::Read( wxInputStream &_I )
{
	wxDataInputStream in( _I );

	wxUint8 code = in.Read8();
	wxUint8 ver = in.Read8();

	m_type = in.Read8();

	size_t nr, nc;
	switch( m_type )
	{
	case VV_INVALID:
		break;
	case VV_NUMBER:
	case VV_ARRAY:
	case VV_MATRIX:
		nr = in.Read32();
		nc = in.Read32();
		if ( nr*nc < 1 ) return false; // big error
		m_val.resize_fill( nr, nc, 0.0f );
		for ( size_t r=0;r<nr;r++ )
			for( size_t c=0;c<nc;c++ )
				m_val(r,c) = in.ReadFloat();
		break;
	case VV_TABLE:
		m_tab.Read( _I );
		break;
	case VV_STRING:
		m_str = in.ReadString();
		break;
	}

	return in.Read8() == code;
}
	
int VarValue::Type() { return m_type; }
void VarValue::SetType( int ty ) { m_type = ty; }
void VarValue::Set( int val ) { m_type = VV_NUMBER; m_val = (float)val; }
void VarValue::Set( float val ) { m_type = VV_NUMBER; m_val = val; }
void VarValue::Set( double val ) { m_type = VV_NUMBER; m_val = val; }
void VarValue::Set( float *val, size_t n ) { m_type = VV_ARRAY; m_val.assign( val, n ); }
void VarValue::Set( float *mat, size_t r, size_t c ) { m_type = VV_MATRIX; m_val.assign( mat, r, c ); }
void VarValue::Set( const ::matrix_t<float> &mat ) { m_type = VV_MATRIX; m_val = mat; }
void VarValue::Set( const wxString &str ) { m_type = VV_STRING; m_str = str; }
void VarValue::Set( const VarTable &tab ) { m_type = VV_TABLE; m_tab.Copy( tab ); }

int VarValue::Integer()
{
	if ( m_type == VV_NUMBER ) return (int)(float)m_val;
	else return 0;
}

float VarValue::Value()
{
	if ( m_type == VV_NUMBER) return (float)m_val;
	else return std::numeric_limits<float>::quiet_NaN();
}

float *VarValue::Array( size_t *n )
{
	if ( m_type == VV_ARRAY )
	{
		if ( n != 0 ) *n = m_val.length();
		return m_val.data();
	}
	else
	{
		if ( n != 0 ) *n = 0;
		return 0;
	}
}

std::vector<float> VarValue::Array()
{
	if ( m_type == VV_ARRAY )
	{
		std::vector<float> vec( m_val.length(), 0.0f );
		for( size_t i=0;i<m_val.length();i++)
			vec[i] = m_val[i];
		return vec;
	}
	else
		return std::vector<float>();
}

matrix_t<float> &VarValue::Matrix()
{
	return m_val;
}

wxString VarValue::String()
{
	return m_str;
}

VarTable &VarValue::Table()
{
	return m_tab;
}

bool VarValue::Read( const lk::vardata_t &val, bool change_type )
{
	switch (val.type())
	{
	case lk::vardata_t::NUMBER:
		if ( Type() == VV_NUMBER || change_type ) Set( (float) val.as_number() );
		break;
	case lk::vardata_t::STRING:
		if ( Type() == VV_STRING || change_type ) Set( val.as_string() );
		break;
	case lk::vardata_t::VECTOR:
		{
			size_t dim1 = val.length(), dim2 = 0;
			for (size_t i=0;i<val.length();i++)
			{
				lk::vardata_t *row = val.index(i);
				if (row->type() == lk::vardata_t::VECTOR && row->length() > dim2 )
					dim2 = row->length();
			}

			if (dim2 == 0 && dim1 > 0)
			{
				float *vec = new float[dim1];
				for( size_t i=0;i<dim1;i++)
					vec[i] = (float)val.index(i)->as_number();


				if ( Type() == VV_ARRAY || change_type )
					Set( vec, dim1 );
				else if ( Type() == VV_MATRIX )
					Set( vec, dim1, 1 );

				delete [] vec;
			}
			else if ( dim1 > 0 && dim2 > 0 )
			{
				if ( Type() == VV_MATRIX || change_type )
				{
					m_type = VV_MATRIX;
					m_val.resize_fill( dim1, dim2, 0.0f );

					for ( size_t i=0;i<dim1;i++)
					{
						for ( size_t j=0;j<dim2;j++ )
						{
							double x = 0;
							if ( val.index(i)->type() == lk::vardata_t::VECTOR
								&& j < val.index(i)->length() )
								x = (float)val.index(i)->index(j)->as_number();

							m_val.at(i,j) = x;
						}
					}
				}
			}
		}
		break;
	case lk::vardata_t::HASH:		
		{
			if ( Type() == VV_TABLE || change_type )
			{
				Set( VarTable() ); // switch to an empty table
				
				lk::varhash_t &hash = *val.hash();
				for ( lk::varhash_t::iterator it = hash.begin();
					it != hash.end();
					++it )
				{
					VarValue *item = Table().Set( (*it).first, VarValue() );
					item->Read( *(*it).second );
				}
			}
		}
		break;
	}

	return true;
}

bool VarValue::Write( lk::vardata_t &val )
{
	switch( Type() )
	{
	case VV_NUMBER:
		val.assign( (double) Value() );
		break;
	case VV_STRING:
		val.assign( String() );
		break;
	case VV_ARRAY:
		{
			size_t n = 0;
			float *p = Array( &n );
			val.empty_vector();
			if ( n > 0 )
			{
				val.vec()->reserve( (size_t) n  );
				for (int i=0;i<n;i++)
					val.vec_append( (double)p[i] );
			}
		}
		break;
	case VV_MATRIX:
		{
			::matrix_t<float> &mat = Matrix();
			val.empty_vector();
			val.vec()->reserve( mat.nrows() );
			for (int i=0;i<mat.nrows();i++)
			{
				val.vec()->push_back( lk::vardata_t() );
				val.vec()->at(i).empty_vector();
				val.vec()->at(i).vec()->reserve( mat.ncols() );
				for (int j=0;j<mat.ncols();j++)
					val.vec()->at(i).vec_append( mat.at(i,j) );
			}
		}
		break;
	case VV_TABLE:
		{
			VarTable &tab = Table();
			for (VarTableBase::iterator it = tab.begin();
				it != tab.end();
				++ it )
			{
				lk::vardata_t &xvd = val.hash_item( it->first );
				it->second->Write( xvd );
			}
		}
		break;
	}

	return true;
}

VarDatabase::VarDatabase()
{
}

VarDatabase::~VarDatabase()
{
}

void VarDatabase::Add( const wxString &name, int type,
	const wxString &label, const wxString &units,
	const wxString &group, const wxString &indexlabels, 
	unsigned long flags, const VarValue &defval )
{
	VarInfo *vv = 0;
	varinfo_hash_t::iterator it = m_hash.find( name );
	if ( it == m_hash.end() )
	{
		vv = new VarInfo;
		m_hash[name] = vv;
	}
	else
		vv = it->second;

	vv->Name = name;
	vv->Type = type;
	vv->Label = label;
	vv->Units = units;
	vv->Group = group;
	vv->IndexLabels = wxStringTokenize( indexlabels, "," );
	vv->Flags = flags;
	vv->DefaultValue = defval;
}

void VarDatabase::Clear()
{
	// delete the variable info
	for ( varinfo_hash_t::iterator it = m_hash.begin();
		it != m_hash.end();
		++it )
		delete it->second;

	m_hash.clear();
	

	// clear the equation fast lookup tables
	m_eqnLookup.clear();
	m_eqnIndices.clear();
	
	// delete the affected variable table
	for ( arraystring_hash_t::iterator it = m_affected.begin();
		it != m_affected.end();
		++it )
		delete (*it).second;

	m_affected.clear();

	
	// delete the equations
	for ( std::vector<eqn_data*>::iterator it = m_equations.begin();
		it != m_equations.end();
		++it )
	{
		delete (*it)->tree;
		delete (*it);
	}
	m_equations.clear();
}

wxArrayString VarDatabase::ListAll()
{
	wxArrayString list;
	for( varinfo_hash_t::iterator it = m_hash.begin(); it != m_hash.end(); ++it )
		list.Add( it->first );
	return list;
}

int VarDatabase::Type( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->Type;
	else return VV_INVALID;
}

wxString VarDatabase::Label( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->Label;
	else return "<not found>";
}

wxString VarDatabase::Group( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->Group;
	else return wxEmptyString;
}

wxString VarDatabase::Units( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->Units;
	else return wxEmptyString;
}

wxArrayString VarDatabase::IndexLabels( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->IndexLabels;
	else return wxArrayString();
}

unsigned long VarDatabase::Flags( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->Flags;
	else return 0;
}

VarValue &VarDatabase::InternalDefaultValue( const wxString &name )
{
	if ( VarInfo *v = Lookup(name) ) return v->DefaultValue;
	else return m_invVal;
}

VarInfo *VarDatabase::Lookup( const wxString &name )
{
	varinfo_hash_t::iterator it = m_hash.find(name);
	if ( it == m_hash.end() ) return 0;
	else return it->second;
}


bool VarDatabase::AddEquation( const wxString &inputs, const wxString &outputs, const wxString &script, wxArrayString *errors )
{
	lk::input_string data( script );
	lk::parser parse( data );
	lk::node_t *tree = parse.script();

	if (parse.error_count() != 0
		|| parse.token() != lk::lexer::END)
	{
		if ( errors )
		{
			errors->Add("fail: parsing did not reach end for [" + outputs + "] = f( " + inputs + ")");				
			for (int x=0; x < parse.error_count(); x++)
				errors->Add( parse.error(x) );
		}
		if ( tree ) delete tree;
		return false;
	}

	eqn_data *ed = new eqn_data;
	ed->tree = tree;
	ed->inputs = wxStringTokenize( inputs, ", ;" );
	ed->outputs = wxStringTokenize( outputs, ", ;" );
	m_equations.push_back( ed );

	for( size_t i=0;i<ed->outputs.size();i++ )
	{
		wxString output = ed->outputs[i];

		// update fast lookup
		m_eqnLookup[ output ] = ed;
		m_eqnIndices[ output ] = m_equations.size()-1;

		// updated affected variable table
		// this is hash table that stores a list of
		// all the variables affected when a variable changes
		// for example:  equation  z = a*b-c;
		//               equation  w = c*z;
		//   affected[ a ] = z
		//   affected[ b ] = z
		//   affected[ c ] = z, w
		//   affected[ z ] = w
		//
		// thus we can recursively mark all variables whose values 
		// are invalidated and need to be recalculated when a certain
		// variable changes value
		//
		// the marking and recalculation happens in VarTable, not here

		for( size_t j=0;j<ed->inputs.size();j++ )
		{
			wxString input = ed->inputs[j];

			arraystring_hash_t::iterator it = m_affected.find( input );
			if ( it == m_affected.end() )
			{
				wxArrayString *as = new wxArrayString;
				as->Add( output );
				m_affected[ input ] = as;
			}
			else
			{
				if ( it->second->Index( output ) < 0)
					it->second->Add( output );
			}
		}
	}

	return true;
}


lk::node_t *VarDatabase::GetEquation( const wxString &var, wxArrayString *inputs, wxArrayString *outputs )
{
	eqndata_hash_t::iterator it = m_eqnLookup.find( var );
	if ( it == m_eqnLookup.end() ) return 0;

	if ( inputs ) *inputs = it->second->inputs;
	if ( outputs ) *outputs = it->second->outputs;
	return it->second->tree;
}

VarDatabase::eqn_data *VarDatabase::GetEquationData( const wxString &var )
{
	eqndata_hash_t::iterator it = m_eqnLookup.find( var );
	if ( it == m_eqnLookup.end() ) return 0;
	return it->second;
}

int VarDatabase::GetEquationIndex( const wxString &var )
{
	eqnindex_hash_t::iterator it = m_eqnIndices.find( var );
	if ( it == m_eqnIndices.end() ) return -1;
	return (int) it->second;
}

wxArrayString *VarDatabase::GetAffectedVariables( const wxString &var )
{
	arraystring_hash_t::iterator it = m_affected.find( var );
	if ( it != m_affected.end() ) return (it->second);
	else return 0;
}







VarTableScriptEnvironment::VarTableScriptEnvironment( VarTable *vt )
	: m_vars( vt )
{
	register_funcs( lk::stdlib_basic() );
	register_funcs( lk::stdlib_math() );
	register_funcs( lk::stdlib_string() );
}

VarTableScriptEnvironment::~VarTableScriptEnvironment( ) { /* nothing to do */ }

bool VarTableScriptEnvironment::special_set( const lk_string &name, lk::vardata_t &val )
{
	if ( VarValue *vv = m_vars->Get( name ) )
		return vv->Read( val );
	else return false;
}

bool VarTableScriptEnvironment::special_get( const lk_string &name, lk::vardata_t &val )
{
	if ( VarValue *vv = m_vars->Get( name ) )
		return vv->Write( val );
	else return false;
}








VarEvaluator::VarEvaluator( VarTable *vars, VarDatabase *db )
	: m_vars( vars ), m_vdb( db )
{
	Reset();
}

void VarEvaluator::Reset()
{
	m_eqns = m_vdb->GetEquations();
	m_status.resize( m_eqns.size(), INVALID );
	m_errors.Clear();
	m_updated.Clear();
}

int VarEvaluator::CalculateAll()
{
	m_errors.Clear();
	m_updated.Clear();

	// invalidate all equations
	for( size_t i=0;i<m_status.size();i++ ) m_status[i] = INVALID;

	return Calculate( );
}


int VarEvaluator::Calculate( )
{
	std::vector<size_t> remaining;
	for( size_t i=0;i<m_status.size();i++ )
		if ( m_status[i] == INVALID )
			remaining.push_back( i );

	if ( remaining.size() == 0 ) return 0; // all equations up to date

	size_t ncalculated; // count all equations processed in current interation
	size_t niterations = 0;
	size_t nevals = 0; // number of equations evaluated
	
	do
	{
		niterations++;
		ncalculated = 0;

		// pass through all the equations in the list
		for( size_t i=0;i<m_status.size();i++ )
		{
			VarDatabase::eqn_data *cur_eqn = m_eqns[i];

			// skip equations that have already been evaluated
			if ( m_status[i] == OK )
				continue;

			wxArrayString &inputs = cur_eqn->inputs;

			// check that all inputs for the current equation have already been evaluated
			bool can_eval = true;
			for( size_t j=0;j<inputs.Count();j++ )
			{
				int idx = m_vdb->GetEquationIndex( inputs[j] );
				if ( idx >= 0 && idx < m_status.size() 
					&& m_status[idx] == INVALID )
				{
					// if an equation was found for an input and it has not yet evaluated,
					// we cannot yet evaluate this equation
						can_eval = false;
				}
			}

			if (can_eval)
			{
				// setup the LK environment
				bool eval_ok = true;

				std::vector<lk_string> lkerrors;
				unsigned int lkctl = lk::CTL_NONE;
				lk::vardata_t lkresult;
				VarTableScriptEnvironment lkenv( m_vars );

				// execute the parse tree, check for errors
				if ( !lk::eval( cur_eqn->tree, &lkenv, lkerrors, lkresult, 0, lkctl, 0, 0 ) )
				{
					for ( std::vector<lk_string>::iterator it = lkerrors.begin();
						it != lkerrors.end(); ++it )
						m_errors.Add( "lk engine: " + *it );

					eval_ok = false;
				}

				// evaluate multiple input/output expression
				if ( eval_ok ) 
				{
					// mark this equation as evaluated
					m_status[i] = OK;
					nevals++;

					// all inputs and outputs have been set
					// mark all outputs as calculated also (so we don't 
					// re-run the MIMO equation for each output
					wxArrayString &out = cur_eqn->outputs;
					for (size_t j=0;j<out.Count();j++)
					{
						m_updated.Add( out[j] );
						
						int idx = m_vdb->GetEquationIndex( out[j] );
						if ( idx >= 0 && idx < m_status.size() )
						{
							// mark equation as evaluated
							m_status[idx] = OK;
							ncalculated++;
							std::vector<size_t>::iterator it = std::find( remaining.begin(), remaining.end(), (size_t)idx );
							if ( it != remaining.end() )
								remaining.erase( it );
						}
					}
				}
				else
				{
					m_errors.Add( "fail: [" + wxJoin(cur_eqn->outputs, ',') 
						+ "] = f( " + wxJoin(cur_eqn->inputs, ',') + " )" );
				}			
			} // can eval?

		}// for each equation in the list

		if ( ncalculated == 0 && remaining.size() > 0 )
		{
			m_errors.Add( "no variables calculated in a single iteration, cannot make progress!" );
			for (size_t i=0;i<remaining.size();i++)
			{
				VarDatabase::eqn_data *eqn = m_eqns[ remaining[i] ];
				m_errors.Add("eqn not evaluated: [" + wxJoin(eqn->outputs, ',') + "] = f( " + wxJoin(eqn->inputs,',') + ")");
			}
		
			return -1;
		}
	}
	while ( remaining.size() > 0 && ncalculated > 0);

	return nevals;
}

size_t VarEvaluator::MarkAffectedEquations( const wxString &var )
{
	wxArrayString *affectlist = m_vdb->GetAffectedVariables( var );	
	if (!affectlist) return 0;
	int naffected = affectlist->Count();
	for (size_t i=0;i<affectlist->Count();i++)
	{
		int index = m_vdb->GetEquationIndex( var );
		if ( index >= 0 && index < (int)m_status.size() )
			m_status[ index ] = INVALID;

		naffected += MarkAffectedEquations( affectlist->Item(i) );
	}
	return naffected;
}

int VarEvaluator::Changed( const wxString &var )
{	
	m_errors.Clear();
	m_updated.Clear();

	// mark all equations as OK
	for (size_t i=0;i<m_status.size();i++)
		m_status[i] = OK;

	// recursively mark all affected equations by this variable as INVALID
	size_t naffected = MarkAffectedEquations( var );
	if (naffected == 0) return 0;
	
	return Calculate( );
}

