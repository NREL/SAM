/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <wx/tokenzr.h>
#include <wx/log.h>
#include <wx/file.h>
#include <wx/datstrm.h>

#include <lk/parse.h>
#include <lk/env.h>
#include <lk/stdlib.h>
#include <lk/eval.h>

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
		for( size_t i=0;i<n->items.size();i++ )
			ScanParseTree( n->items[i], inputs, outputs );
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
				if ( id->special && outputs->Index( id->name ) == wxNOT_FOUND )
					outputs->Add( id->name );

		ScanParseTree(  n->right, inputs, outputs );
	}
	else if ( lk::ctlstmt_t *n = dynamic_cast<lk::ctlstmt_t*>( root ) )
	{
		ScanParseTree( n->rexpr, inputs, outputs );
	}
	else if ( lk::iden_t *n = dynamic_cast<lk::iden_t*>( root ) )
	{
		if ( n->special && !in_assign_lhs && inputs->Index( n->name ) == wxNOT_FOUND )
			inputs->Add( n->name );
	}

	// note: lk structures for constants, literals, null-types don't need handling here
}

bool EqnDatabase::LoadFile( const wxString &file, wxArrayString *errors )
{
	wxFile fp( file );
	if ( fp.IsOpened() )
	{
		wxString buf;
		fp.ReadAll( &buf );
		lk::input_string data( buf );
		if ( Parse( data, errors ) ) return true;
		else return false;

	}
	else return false;
}
bool EqnDatabase::LoadScript( const wxString &text, wxArrayString *errors )
{
// check text for preprocessing setup for design point calculations SAM issue #634
	lk::input_string in( text );
	if ( Parse( in, errors ) ) return true;
	else return false;
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
			for( int i=0;i<parse.error_count();i++ )
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
		local_env.register_funcs( lk::stdlib_sysio() );
		local_env.register_funcs( lk::stdlib_math() );
		local_env.register_funcs( lk::stdlib_string() );

		lk::eval e( tree, &local_env );
		
		bool ok = e.run();
		if ( !ok && errors )
			for( size_t i=0;i<e.error_count();i++ )
				errors->Add( e.get_error(i) );

		size_t neqns_found = 0;
		lk::vardata_t *eqnhash_var = 0;
		lk::varhash_t *hash = 0;
		if ( ((eqnhash_var = local_env.lookup( "equations", false ))!=0)
			&& (eqnhash_var->type() == lk::vardata_t::HASH)
			&& ((hash = eqnhash_var->hash()) !=0) )
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

				if ( outputs.size() > 0 && equation != 0 )
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
	// delete the affected variable table
	for ( arraystring_hash_t::iterator it = m_affected.begin();
		it != m_affected.end();
		++it )
		delete (*it).second;

	m_affected.clear();

	// delete the equations
	for ( std::vector<EqnData*>::iterator it = m_equations.begin();
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

	EqnData *ed = new EqnData;
	ed->tree = tree;
	ed->inputs = inputs;
	ed->outputs = outputs;
	ed->result_is_output = result_is_output;
	m_equations.push_back( ed );

	for( size_t i=0;i<ed->outputs.size();i++ )
	{
		wxString output = ed->outputs[i];
		
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

wxArrayString *EqnDatabase::GetAffectedVariables( const wxString &var )
{
	arraystring_hash_t::iterator it = m_affected.find( var );
	if ( it != m_affected.end() ) return (it->second);
	else return 0;
}

EqnFastLookup::EqnFastLookup()
{
	// nothing to do
}

EqnFastLookup::EqnFastLookup( EqnDatabase *db )
{
	m_dbs.push_back( db );
}

void EqnFastLookup::AddDatabase( EqnDatabase *db )
{
	m_dbs.push_back( db );
}

void EqnFastLookup::Add( EqnData *e )
{
	m_eqnList.push_back( e );

	for( size_t i=0;i<e->outputs.size();i++)
	{
		wxString &output = e->outputs[i];

		// update fast lookup
		m_eqnLookup[ output ] = e;
		m_eqnIndices[ output ] = m_eqnList.size()-1;
	}
}

void EqnFastLookup::Add( const std::vector<EqnData*> &list )
{
	for( size_t i=0;i<list.size();i++ )
		Add( list[i] );
}

void EqnFastLookup::Clear()
{
	// clear the equation fast lookup tables
	m_eqnLookup.clear();
	m_eqnIndices.clear();
	
}

size_t EqnFastLookup::GetAffectedVariables( const wxString &var, wxArrayString &list, eqnmark_hash_t &ignore )
{
	size_t n = 0;
	for( size_t i=0;i<m_dbs.size();i++ )
	{
		if( wxArrayString *ll = m_dbs[i]->GetAffectedVariables( var ) )
		{
			for( size_t k=0;k<ll->Count();k++ )
			{
				if ( ignore.find( ll->Item(k) ) == ignore.end() )
				{
					list.Add( ll->Item(k) );
					n++;
				}
			}
		}
	}

	return n;
}

lk::node_t *EqnFastLookup::GetEquation( const wxString &var, wxArrayString *inputs, wxArrayString *outputs )
{
	eqndata_hash_t::iterator it = m_eqnLookup.find( var );
	if ( it == m_eqnLookup.end() ) return 0;

	if ( inputs ) *inputs = it->second->inputs;
	if ( outputs ) *outputs = it->second->outputs;
	return it->second->tree;
}

EqnData *EqnFastLookup::GetEquationData( const wxString &var )
{
	eqndata_hash_t::iterator it = m_eqnLookup.find( var );
	if ( it == m_eqnLookup.end() ) return 0;
	return it->second;
}

int EqnFastLookup::GetEquationIndex( const wxString &var )
{
	eqnindex_hash_t::iterator it = m_eqnIndices.find( var );
	if ( it == m_eqnIndices.end() ) return -1;
	return (int) it->second;
}



EqnEvaluator::EqnEvaluator( VarTable &vars, EqnFastLookup &fl )
	: m_vars( vars ), m_efl( fl )
{
	Reset();
}

void EqnEvaluator::Reset()
{
	m_eqns = m_efl.GetEquations();
	m_status.resize( m_eqns.size(), INVALID );
	m_errors.Clear();
	m_updated.Clear();
}

int EqnEvaluator::CalculateAll()
{
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

//	wxLogStatus("Calculating equations...");
	
	do
	{
		niterations++;
		ncalculated = 0;

		// pass through all the equations in the list
		for( size_t i=0;i<m_status.size();i++ )
		{
			EqnData *cur_eqn = m_eqns[i];

			// skip equations that have already been evaluated
			if ( m_status[i] == OK )
				continue;

			wxArrayString &inputs = cur_eqn->inputs;

			// check that all inputs for the current equation have already been evaluated
			bool can_eval = true;
			for( size_t j=0;j<inputs.Count();j++ )
			{
				int idx = m_efl.GetEquationIndex( inputs[j] );
				if ( idx >= 0 && idx < (int)m_status.size() 
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
				
				lk::env_t env;

				// setup any functions here
				SetupEnvironment( env );

				VarTableScriptInterpreter e( cur_eqn->tree, &env, &m_vars );
			
#ifdef _DEBUG
				wxLogStatus( "solving... [" + wxJoin(cur_eqn->outputs, ',') 
					+ "] = f( " + wxJoin(cur_eqn->inputs, ',') + " )" );
#endif

				// execute the parse tree, check for errors
				if ( !e.run() )
				{
					for ( size_t m=0;m<e.error_count();m++ )
						m_errors.Add( "equation engine: " + e.get_error(m) );

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
						e.special_set( cur_eqn->outputs[0], e.result().deref() );

					// all inputs and outputs have been set
					// mark all outputs as calculated also (so we don't 
					// re-run the MIMO equation for each output
					wxArrayString &out = cur_eqn->outputs;
					for (size_t j=0;j<out.Count();j++)
					{
						m_updated.Add( out[j] );
						
						int idx = m_efl.GetEquationIndex( out[j] );
						if ( idx >= 0 && idx < (int)m_status.size() )
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
				EqnData *eqn = m_eqns[ remaining[i] ];
				m_errors.Add("eqn not evaluated: [" + wxJoin(eqn->outputs, ',') + "] = f( " + wxJoin(eqn->inputs,',') + ")");
			}
		
			return -1;
		}
	}
	while ( remaining.size() > 0 && ncalculated > 0);

	return nevals;
}

size_t EqnEvaluator::MarkAffectedEquations( const wxString &var, EqnFastLookup::eqnmark_hash_t &marked )
{
	wxArrayString affected;
	int naffected = m_efl.GetAffectedVariables( var, affected, marked );	
	if ( naffected == 0 ) return 0;

	for (size_t i=0;i<affected.size();i++)
	{
		int index = m_efl.GetEquationIndex( affected[i] );
		if ( index >= 0 && index < (int)m_status.size() )
			m_status[ index ] = INVALID;

		marked[ affected[i] ] = true;

		naffected += MarkAffectedEquations( affected[i], marked );
	}
	return naffected;
}

int EqnEvaluator::Changed( const wxArrayString &vars )
{
	// mark all equations as OK
	for (size_t i=0;i<m_status.size();i++)
		m_status[i] = OK;
	
//	wxLogStatus(" Marking equations... %d triggers", (int)vars.size() );

	// recursively mark all affected equations by this variable as INVALID
	EqnFastLookup::eqnmark_hash_t marked;
	size_t naffected = 0;
	for( size_t i=0;i<vars.size();i++ )
		naffected += MarkAffectedEquations( vars[i], marked );

	if (naffected == 0) return 0;

//	wxLogStatus(" %d affected variables marked.", (int)naffected );
	
	return Calculate( );
}

void EqnEvaluator::SetupEnvironment( lk::env_t &env )
{
	env.register_funcs( lk::stdlib_basic() );
	env.register_funcs( lk::stdlib_sysio() );
	env.register_funcs( lk::stdlib_math() );
	env.register_funcs( lk::stdlib_string() );
	env.register_funcs( lk::stdlib_wxui() );
}
