#include <wx/tokenzr.h>
#include <wx/log.h>

#include <lk_parse.h>
#include <lk_env.h>
#include <lk_stdlib.h>
#include <lk_eval.h>

#include "equations.h"



EqnDatabase::EqnDatabase()
{
}

EqnDatabase::~EqnDatabase()
{
	Clear();
}


void EqnDatabase::ScanParseTree( lk::node_t *root, wxArrayString *inputs, wxArrayString *outputs, bool in_assign_lhs )
{
	if (!root) return;

	if ( lk::list_t *n = dynamic_cast<lk::list_t*>( root ) )
	{
		while (n)
		{
			ScanParseTree(  n->item, inputs, outputs );
			n = n->next;
		}
	}
	else if ( lk::iter_t *n = dynamic_cast<lk::iter_t*>( root ) )
	{
		ScanParseTree( n->init, inputs, outputs);
		ScanParseTree( n->test, inputs, outputs);
		ScanParseTree( n->adv, inputs, outputs);
		ScanParseTree( n->block, inputs, outputs );
	}
	else if ( lk::cond_t *n = dynamic_cast<lk::cond_t*>( root ) )
	{
		ScanParseTree( n->test, inputs, outputs );
		ScanParseTree( n->on_true, inputs, outputs);
		ScanParseTree( n->on_false, inputs, outputs);
	}
	else if ( lk::expr_t *n = dynamic_cast<lk::expr_t*>( root ) )
	{
		ScanParseTree( n->left, inputs, outputs, n->oper == lk::expr_t::ASSIGN );
		if ( n->oper == lk::expr_t::ASSIGN )
			if ( lk::iden_t *id = dynamic_cast<lk::iden_t*>( n->left ) )
				if ( id->special )
					outputs->Add( id->name );

		ScanParseTree(  n->right, inputs, outputs );
	}
	else if ( lk::iden_t *n = dynamic_cast<lk::iden_t*>( root ) )
	{
		if ( n->special && !in_assign_lhs )
			inputs->Add( n->name );
	}

	// note: lk structures for constants, literals, null-types don't need handling here
}

bool EqnDatabase::LoadFile( const wxString &file, wxArrayString *errors )
{
	FILE *fp = fopen( (const char*)file.c_str(), "r" );
	if (!fp) return false;
	lk::input_stream in( fp );
	bool ok = Parse( in, errors );
	fclose(fp);
	return ok;
}
bool EqnDatabase::LoadScript( const wxString &text, wxArrayString *errors )
{
	lk::input_string in( text );
	return Parse( in, errors );
}

bool EqnDatabase::Parse( lk::input_base &in, wxArrayString *errors )
{
	lk::parser parse( in );
	lk::node_t *tree = parse.script();

	if ( parse.error_count() != 0 
		|| parse.token() != lk::lexer::END)
	{
		if ( errors )
		{
			for( size_t i=0;i<parse.error_count();i++ )
				errors->Add( parse.error(i) );
			errors->Add( "parsing did not reach end of input" );
		}
		if ( tree ) delete tree;
		return false;
	}
	else
	{
		lk::env_t local_env;
		local_env.register_funcs( lk::stdlib_basic() );
		local_env.register_funcs( lk::stdlib_math() );
		local_env.register_funcs( lk::stdlib_string() );
		lk::vardata_t lkvar;
		std::vector<lk_string> lkerrs;
		unsigned int lkctl = lk::CTL_NONE;
		bool ok = lk::eval( tree, &local_env, lkerrs, lkvar, 0, lkctl, 0, 0 );
		
		if ( !ok && errors )
			for( size_t i=0;i<lkerrs.size();i++ )
				errors->Add( lkerrs[i] );

		size_t neqns_found = 0;
		lk::vardata_t *eqnhash_var = 0;
		lk::varhash_t *hash = 0;
		if ( (eqnhash_var = local_env.lookup( "equations", false ))
			&& (eqnhash_var->type() == lk::vardata_t::HASH)
			&& (hash = eqnhash_var->hash()) )
		{
			for( lk::varhash_t::iterator it = hash->begin();
				it != hash->end();
				++it )
			{
				wxString name = it->first;
				lk::vardata_t &value = it->second->deref();
				
				wxArrayString inputs, outputs;
				lk::node_t *equation = 0;

				if ( value.type() == lk::vardata_t::FUNCTION
					&& value.func() != 0
					&& value.func()->oper == lk::expr_t::DEFINE 
					&& value.func()->right != 0 )
				{
					ScanParseTree( value.func()->right, &inputs, &outputs );
					equation = value.func()->right;
				}

				bool result_is_output = false;
				if ( name.Left(6) != "$MIMO$" && outputs.Index( name ) == wxNOT_FOUND )
				{
					result_is_output = true;
					outputs.Add( name );
				}

				if ( inputs.size() > 0 && outputs.size() > 0 && equation != 0 )
				{
					// now save the equation in the database
					AddEquation( inputs, outputs, equation, result_is_output );
					neqns_found++;
				}
			}

		}

		if ( ok && neqns_found > 0)
			m_trees.push_back( tree );
		else
			delete tree;

		return ok;
	}
}


void EqnDatabase::Clear()
{
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
		delete (*it); // don't delete the tree here: just a reference and will be deleted below.

	m_equations.clear();

	for( size_t i=0;i<m_trees.size();i++)
		delete m_trees[i];

	m_trees.clear();
}


bool EqnDatabase::AddEquation( const wxArrayString &inputs, const wxArrayString &outputs, lk::node_t *tree, bool result_is_output )
{

	eqn_data *ed = new eqn_data;
	ed->tree = tree;
	ed->inputs = inputs;
	ed->outputs = outputs;
	ed->result_is_output = result_is_output;
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


lk::node_t *EqnDatabase::GetEquation( const wxString &var, wxArrayString *inputs, wxArrayString *outputs )
{
	eqndata_hash_t::iterator it = m_eqnLookup.find( var );
	if ( it == m_eqnLookup.end() ) return 0;

	if ( inputs ) *inputs = it->second->inputs;
	if ( outputs ) *outputs = it->second->outputs;
	return it->second->tree;
}

EqnDatabase::eqn_data *EqnDatabase::GetEquationData( const wxString &var )
{
	eqndata_hash_t::iterator it = m_eqnLookup.find( var );
	if ( it == m_eqnLookup.end() ) return 0;
	return it->second;
}

int EqnDatabase::GetEquationIndex( const wxString &var )
{
	eqnindex_hash_t::iterator it = m_eqnIndices.find( var );
	if ( it == m_eqnIndices.end() ) return -1;
	return (int) it->second;
}

wxArrayString *EqnDatabase::GetAffectedVariables( const wxString &var )
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
	bool ok = false;
	if ( VarValue *vv = m_vars->Get( name ) )
		ok = vv->Read( val );

	wxLogStatus("VTSE->special_set( " + name + " ) " + wxString( ok?"ok":"fail") );
	return ok;
}

bool VarTableScriptEnvironment::special_get( const lk_string &name, lk::vardata_t &val )
{
	bool ok = false;
	if ( VarValue *vv = m_vars->Get( name ) )
		ok = vv->Write( val );
	
	wxLogStatus("VTSE->special_get( " + name + " ) " + wxString( ok?"ok":"fail") );
	return ok;
}


EqnEvaluator::EqnEvaluator( VarTable *vars, EqnDatabase *db )
	: m_vars( vars ), m_edb( db )
{
	Reset();
}

void EqnEvaluator::Reset()
{
	m_eqns = m_edb->GetEquations();
	m_status.resize( m_eqns.size(), INVALID );
	m_errors.Clear();
	m_updated.Clear();
}

int EqnEvaluator::CalculateAll()
{
	m_errors.Clear();
	m_updated.Clear();

	// invalidate all equations
	for( size_t i=0;i<m_status.size();i++ ) m_status[i] = INVALID;

	return Calculate( );
}


int EqnEvaluator::Calculate( )
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
			EqnDatabase::eqn_data *cur_eqn = m_eqns[i];

			// skip equations that have already been evaluated
			if ( m_status[i] == OK )
				continue;

			wxArrayString &inputs = cur_eqn->inputs;

			// check that all inputs for the current equation have already been evaluated
			bool can_eval = true;
			for( size_t j=0;j<inputs.Count();j++ )
			{
				int idx = m_edb->GetEquationIndex( inputs[j] );
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
						m_errors.Add( "equation engine: " + *it );

					eval_ok = false;
				}

				// evaluate multiple input/output expression
				if ( eval_ok ) 
				{
					// mark this equation as evaluated
					m_status[i] = OK;
					nevals++;

					// for equations with a single output (not MIMOs)
					// the result of expression evaluation is
					// the value of the single output variable 
					// (i.e. via the 'return' statement)
					if ( cur_eqn->result_is_output && cur_eqn->outputs.size() == 1 )
						lkenv.special_set( cur_eqn->outputs[0], lkresult.deref() );

					// all inputs and outputs have been set
					// mark all outputs as calculated also (so we don't 
					// re-run the MIMO equation for each output
					wxArrayString &out = cur_eqn->outputs;
					for (size_t j=0;j<out.Count();j++)
					{
						m_updated.Add( out[j] );
						
						int idx = m_edb->GetEquationIndex( out[j] );
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
				EqnDatabase::eqn_data *eqn = m_eqns[ remaining[i] ];
				m_errors.Add("eqn not evaluated: [" + wxJoin(eqn->outputs, ',') + "] = f( " + wxJoin(eqn->inputs,',') + ")");
			}
		
			return -1;
		}
	}
	while ( remaining.size() > 0 && ncalculated > 0);

	return nevals;
}

size_t EqnEvaluator::MarkAffectedEquations( const wxString &var )
{
	wxArrayString *affectlist = m_edb->GetAffectedVariables( var );	
	if (!affectlist) return 0;
	int naffected = affectlist->Count();
	for (size_t i=0;i<affectlist->Count();i++)
	{
		int index = m_edb->GetEquationIndex( affectlist->Item(i) );
		if ( index >= 0 && index < (int)m_status.size() )
			m_status[ index ] = INVALID;

		naffected += MarkAffectedEquations( affectlist->Item(i) );
	}
	return naffected;
}

int EqnEvaluator::Changed( const wxString &var )
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

