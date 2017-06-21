#include <wx/wx.h>
#include <wx/wfstream.h>
#include <wx/datstrm.h>
#include <wx/tokenzr.h>
#include <wx/busyinfo.h>
#include <wx/splitter.h>
#include <wx/config.h>
#include <wx/dynlib.h>

#include <math.h>
#include <cmath>

#include <sstream>

#include <exception>
#include <algorithm>
#include <limits>
#include <numeric>

#include <lk/absyn.h>
#include <lk/env.h>
#include <lk/parse.h>
#include <lk/lex.h>
#include <lk/stdlib.h>

#include "jacobian.h"
#include "search.h"
#include "lu.h"
#include "newton.h"

enum { ID_SOLVE = 8145, ID_DEMO, ID_CMDLINE };

namespace lk {
	//typedef unordered_map< lk_string, double, lk_string_hash, lk_string_equal > valtab_t;
		
	bool nsolver_callback( int iter, double *x, double *resid, const int n, void *data)
	{
		wxTextCtrl *t = (wxTextCtrl*)data;
		if (!t) return true;

		wxString text;
		text.Printf("\niter=%d\n", iter);
		for (int i=0;i<n;i++)
		{
			text += wxString::Format("x[%d]=%.15lf     resid[%d]=%.15lf\n", i, x[i], i, resid[i] );
		}
		t->AppendText( text );
		return true;
	}
		
	class evalexception : public std::exception
	{
	private:
		lk_string m_err;
	public:
		evalexception( lk_string s ) : m_err(s) {  }
		virtual ~evalexception() throw() {  /* nothing to do */ }
		virtual const char *what() { return (const char*)m_err.c_str(); }
	};

	class eqnsolve
	{
	private:
		
		class funccall_t : public node_t
		{
		public:
			lk_string name;
			std::vector< lk::node_t* > args;
			funccall_t( srcpos_t sp, const lk_string &n) : node_t(sp), name(n) {  }
			virtual ~funccall_t() { for (size_t i=0;i<args.size();i++) delete args[i]; }
		};

		lk::env_t *m_envPtr;
		bool m_parsed;
		bool m_haltFlag;
		lexer lex;				
		int m_tokType;	
		std::vector<lk_string> m_errorList;
		std::vector< lk::node_t* > m_eqnList;
		std::vector< lk_string > m_varList;
		
		srcpos_t srcpos() { return srcpos_t(  "", lex.line(), lex.line(), 0 ); }
		int line() { return lex.line(); }
		int error_count() { return m_errorList.size(); }
		lk_string error(int idx);
				
		void skip() { m_tokType = lex.next(); }

		void error( const char *fmt, ... )
		{
			char buf[512];
			va_list list;
			va_start( list, fmt );	
	
			#if defined(_WIN32)&&defined(_MSC_VER)
				_vsnprintf( buf, 510, fmt, list );
			#else
				vsnprintf( buf, 510, fmt, list );
			#endif
	
			m_errorList.push_back(lk_string(buf));
			va_end( list );
		}

		bool match( const char *s )
		{	
			if ( lex.text() != s )
			{
				error("%d: expected '%s'", line(), s);
				return false;
			}	
			skip();
			return true;
		}

		bool match(int t)
		{
			if ( m_tokType != t)
			{
				error("%d: expected '%s'", line(), lk::lexer::tokstr(t));
				return false;
			}	
			skip();
			return true;
		}
		
		int token()
		{
			return m_tokType;
		}

		bool token(int t)
		{
			return (m_tokType==t);
		}

	public:

		eqnsolve( input_base &input )
			: lex( input )
		{
			m_haltFlag = false;
			m_tokType = lex.next();
			m_parsed = false;
			m_envPtr = 0;
		}

		void set( lk::env_t &ee, const lk_string &name, double val )
		{
			lk::vardata_t *v = new lk::vardata_t;
			v->assign( val );
			ee.assign( name, v );
		}

		double get( lk::env_t &ee, const lk_string &name )
		{
			lk::vardata_t *v = ee.lookup( name, true );
			if ( !v || v->type() != lk::vardata_t::NUMBER ) return std::numeric_limits<double>::quiet_NaN();
			else return v->num();
		}

		std::vector< lk_string > errors() { return m_errorList; }

		lk_string errortext()
		{
			lk_string t;
			for (size_t i=0;i<m_errorList.size();i++)
				t += m_errorList[i] + "\n";
			return t;
		}
		lk::node_t *reduce_eqn( lk::node_t *root, lk::env_t &env, double &result, int nreduc ) throw( evalexception )
		{
			if ( lk::expr_t *e = dynamic_cast<lk::expr_t*>( root ) )
			{
				double a;
				lk::node_t *lhs = reduce_eqn( e->left, env, a, nreduc );
				if ( lhs == 0 && e->right == 0 && e->oper == lk::expr_t::NEG )
				{
					delete e->left;
					e->left = 0;
					result = a;
					nreduc++;
					return 0;
				}
				
				double b;
				lk::node_t *rhs = reduce_eqn( e->right, env, b, nreduc );
				if ( lhs == 0 && rhs == 0 )
				{
					switch( e->oper )
					{
					case lk::expr_t::PLUS:
						result = a + b;
						break;
					case lk::expr_t::MINUS:
						result = a - b;
						break;
					case lk::expr_t::MULT:
						result = a * b;
						break;
					case lk::expr_t::DIV:
						{
							if ( b == 0 ) throw evalexception("divide by zero in equation reduction");
							else result = a / b;
							break;
						}
					case lk::expr_t::EXP:
						result = pow( a, b );
						break;
					default:
						throw evalexception("invalid operation encountered during equation reduction");
					}

					if (e->left) delete e->left;
					e->left = 0;
					if (e->right) delete e->right;
					e->right = 0;					
					nreduc++;
					return 0;
				}
				else
				{
					if (lhs == 0)
					{
						if (e->left) delete e->left;
						e->left = new lk::constant_t( e->srcpos(), a );
					}
					else if (e->left != lhs )
						throw evalexception( "internal reduce error on lhs side of expr");

					if (rhs == 0)
					{
						if (e->right) delete e->right;
						e->right = new lk::constant_t( e->srcpos(), b );
					}
					else if (e->right != rhs )
						throw evalexception( "internal reduce error on rhs side of expr");

					
					// check here there is just a variable and a value operation
					lk::iden_t *iden = 0;
					lk::constant_t *cons = 0;
					if ( (iden = dynamic_cast< lk::iden_t*>( e->left ))
						&& (cons = dynamic_cast< lk::constant_t*>( e->right ))
						&& ( e->oper == lk::expr_t::PLUS || e->oper == lk::expr_t::MINUS ))
					{
						nreduc++;
						double fac = (e->oper==lk::expr_t::PLUS) ? -1.0 : 1.0;
						set( env, iden->name, fac*cons->value );
						result = fac*cons->value;
						return 0;
					}
					else if ((iden = dynamic_cast< lk::iden_t*>( e->right ))
						&& (cons = dynamic_cast< lk::constant_t*>( e->left ))
						&& ( e->oper == lk::expr_t::PLUS || e->oper == lk::expr_t::MINUS ))
					{
						nreduc++;
						double fac = (e->oper==lk::expr_t::PLUS) ? -1.0 : 1.0;
						set( env, iden->name, fac*cons->value);
						result = fac*cons->value;
						return 0;
					}
					
					return e;
				}
			}
			else if ( lk::constant_t *c = dynamic_cast<lk::constant_t*>( root ) )
			{
				result = c->value;
				nreduc++;
				return 0;
			}
			else if ( lk::iden_t *i = dynamic_cast<lk::iden_t*>( root ) )
			{
				if ( lk::vardata_t *v = env.lookup(i->name, true)  )
				{
					if ( v->type() == lk::vardata_t::NUMBER )
					{
						result = v->num();
						nreduc++;
						return 0;
					}
				}
			}
			else if ( funccall_t *f = dynamic_cast<funccall_t*>( root ))
			{
				lk::fcallinfo_t *fobj = env.lookup_func( f->name);
				if ( fobj == 0 )
					throw evalexception("undefined function: " + f->name );

				lk::vardata_t zz;
				lk::invoke_t ivk( &env, zz, fobj->user_data );
				std::vector< lk::node_t* > reduced_args;
				std::vector< double > arg_vals;
				bool all_reduced = true;

				for (size_t i=0;i<f->args.size();i++)
				{
					double xarg = 0;
					lk::node_t *reduced = reduce_eqn( f->args[i], env, xarg, nreduc );
					reduced_args.push_back( reduced );
					arg_vals.push_back( xarg );

					if ( reduced == 0 )
					{
						lk::vardata_t ivkarg;
						ivkarg.assign( xarg );
						ivk.arg_list().push_back( ivkarg );
					}
					else
						all_reduced = false;
				}

				if ( all_reduced == false )
				{
					for (size_t i=0;i<f->args.size();i++)
					{
						if ( f->args[i] != reduced_args[i] )
						{
							delete f->args[i];
							f->args[i] = reduced_args[i];
						}
					}

					return f;
				}
				else
				{
					try {
						fobj->f( ivk );
					} catch( lk::error_t &err )
					{
						error( err.what() );
						throw evalexception("reduce failed in function call: " + f->name );
					}
					
					if (ivk.result().type() != lk::vardata_t::NUMBER )
						throw evalexception("function " + f->name + " did not return a number, instead: " + ivk.result().typestr() );

					result = ivk.result().num();
					return 0;
				}
			}

			return root;				
		}

		bool reduce( lk::env_t &env )
		{
			
			int nreduc;
			do
			{
				nreduc = 0;
				for (std::vector<lk::node_t*>::iterator it = m_eqnList.begin();
					it != m_eqnList.end();
					++it )
				{
					double result;
					try {
						lk::node_t *reduced = reduce_eqn( *it, env, result, nreduc );
						if (reduced == 0)
						{
							delete *it;
							*it = 0;
						}
					} catch( evalexception &ex )
					{
						error( ex.what() );
						return false;
					}
				}

				size_t i=0;
				while( i<m_eqnList.size() )
				{
					if ( m_eqnList[i] == 0 )
						m_eqnList.erase( m_eqnList.begin() + i );
					else
						i++;
				}

			} while ( nreduc > 0 );

			return true;
		}

		lk_string print_eqn( lk::node_t *root )
		{
			if ( lk::expr_t *e = dynamic_cast<lk::expr_t*>(root) )
			{
				lk_string str = "(" + print_eqn( e->left );
				str += e->operstr();
				str += print_eqn(e->right);
				str += ")";
				return str;
			}
			else if ( lk::iden_t *i = dynamic_cast<lk::iden_t*>(root) )
			{
				return i->name;
			}
			else if ( lk::constant_t *c = dynamic_cast<lk::constant_t*>(root) )
			{
				return wxString::Format("%lg",c->value);
			}
			else if ( funccall_t *f = dynamic_cast< funccall_t* > (root) )
			{
				lk_string str = f->name + "(";
				for (size_t i=0;i<f->args.size();i++)
					str += print_eqn( f->args[i] ) + wxString((i<f->args.size()-1)?",":")");
				return str;
			}

			return wxEmptyString;
		}

		int solve( lk::env_t &env, int max_iter, double ftol, double appfac, wxTextCtrl *log)
		{
			if (!parse()) return -1;

			if ( log != 0 )
			{
				log->AppendText( "\nParsed equations:\n");
				for (size_t i=0;i<m_eqnList.size();i++)
					log->AppendText("\t" + print_eqn( m_eqnList[i] ) +"\n");
			}

		
			// do reduction in empty environment to avoid eliminating all eqns
			// if variables already have values assigned.
			lk::env_t ereduce;
			ereduce.register_funcs( lk::stdlib_math() );
			if (!reduce( ereduce )) return -2;
			
			if ( log != 0 )
			{
				log->AppendText( "\nReduced equations:\n");
				for (size_t i=0;i<m_eqnList.size();i++)
					log->AppendText("\t" + print_eqn( m_eqnList[i] ) +"\n");
			}

			m_varList.clear();
			for ( size_t i=0;i<m_eqnList.size();i++)
				find_names( m_eqnList[i], m_varList );
			
			int n = m_varList.size();

			if ( n != (int)m_eqnList.size() )
			{
				error("#equations(%d) != #variables(%d)", m_eqnList.size(), n);
				return -2;
			}

			// set all initial conditions NaN variables to 1
			for ( int i=0;i<n;i++ )
			{
				volatile double x = get(env, m_varList[i]);
				if ( x != x )
					set(env, m_varList[i], 1 );
			}

			double *x = new double[n];
			double *resid = new double[n];

			for (int i=0;i<n;i++)
				x[i] = resid[i] = 0.0;

			for (int i=0;i<n;i++)
				x[i] = get(env, m_varList[i] ); // initial values
			
			bool check = false;
			int niter = -1;
			
			m_envPtr = &env;
			try {
				niter = newton<double, eqnsolve>( x, resid, n, check, *this, 
					max_iter, ftol, ftol, appfac,
					nsolver_callback, log);
			} catch( evalexception &ex ) {
				error( ex.what() );
				niter = -9;
			}
			m_envPtr = 0;

			for( int i=0; i < n; i++)
				set(env, m_varList[i], x[i] );

			// copy over all variables assigned in 'ereduce' environment
			lk_string key;
			lk::vardata_t *var;
			bool has_more = ereduce.first( key, var );
			while( has_more )
			{
				set( env, key, var->as_number() );
				has_more = ereduce.next( key, var );
			}

			delete [] x;
			delete [] resid;

			if (check) niter = -999;
			return niter;
		}

		lk_string variables(lk::env_t &env)
		{
			std::stringstream oss;
			oss.setf(std::ios::fixed);
			oss.precision(15);
			lk_string key;
			lk::vardata_t *v;
			bool next = env.first( key, v );
			while( next )
			{
				oss << key  << " = " << v->as_number() << std::endl;
				next = env.next( key, v );
			}
			return lk_string( oss.str().c_str() );
		}


		
		
		void operator() ( const double *x, double *f, int n ) throw( evalexception )
		{
			if (m_envPtr == 0 )
				throw evalexception("internal null environment pointer");
			int i;
			for( i=0; i < n; i++)
				set( *m_envPtr, m_varList[i], x[i] );

			// solve each eqn to get f[i];
			for( i=0;i < n;i++)
				f[i] = eval_eqn( m_eqnList[i], *m_envPtr );
		}
	
		double eval_eqn( lk::node_t *n, lk::env_t &t ) throw( evalexception )
		{
			if ( !n ) return std::numeric_limits<double>::quiet_NaN();
			if ( lk::expr_t *e = dynamic_cast<lk::expr_t*>( n ) )
			{
				double a = eval_eqn( e->left, t );
				double b = eval_eqn( e->right, t );
				switch( e->oper )
				{
				case lk::expr_t::PLUS:
					return a + b;
				case lk::expr_t::MINUS:
					return a - b;
				case lk::expr_t::MULT:
					return a * b;
				case lk::expr_t::DIV:
					{
						if ( b == 0 ) throw evalexception("divide by zero");
						else return a / b;
					}
				case lk::expr_t::EXP:
					return pow( a, b );
				case lk::expr_t::NEG:
					return -a;
				}

			}
			else if ( lk::iden_t *i = dynamic_cast<lk::iden_t*>( n ))
			{
				lk::vardata_t *v = t.lookup( i->name, true );
				if ( v == 0 || v->type() != lk::vardata_t::NUMBER )
					throw evalexception("variable not assigned or not a number: " + i->name);
				return v->num();
			}
			else if ( lk::constant_t *c = dynamic_cast<lk::constant_t*>( n ))
			{
				return c->value;
			}
			else if ( funccall_t *f = dynamic_cast<funccall_t*>( n ))
			{
				void *ud = 0;
				lk::fcallinfo_t * fobj = t.lookup_func( f->name );
				if ( fobj == 0 )
					throw evalexception( "undefined function: " + f->name );

				lk::vardata_t zz;
				lk::invoke_t ivk( &t, zz, fobj->user_data );
				for (size_t i=0;i<f->args.size();i++)
				{
					lk::vardata_t arg;
					arg.assign( eval_eqn( f->args[i], t ) );
					ivk.arg_list().push_back( arg );
				}

				try {
					fobj->f( ivk );
				} catch( lk::error_t &e )
				{
					error( e.what() );
					throw evalexception("function evaluation error in: " + f->name + " " + lk_string(e.what() ) );
				}

				if (ivk.result().type() != lk::vardata_t::NUMBER )
					throw evalexception("function " + f->name + " did not return a number, instead: " + ivk.result().typestr() );
				return ivk.result().num();
			}

			throw evalexception("internal error: faulty equation");

		}
		
		void find_names( node_t *n, std::vector<lk_string> &vars )
		{
			if (!n) return;
			if ( lk::expr_t *e = dynamic_cast<lk::expr_t*>(n) )
			{
				find_names( e->left, vars );
				find_names( e->right, vars );
			}
			else if ( funccall_t *f = dynamic_cast<funccall_t*>(n) )
			{
				for ( size_t i=0;i<f->args.size();i++)
					find_names( f->args[i], vars );
			}
			else if ( lk::iden_t *i = dynamic_cast<lk::iden_t*>(n) )
			{
				if ( std::find( vars.begin(), vars.end(), i->name ) == vars.end() )
					vars.push_back( i->name );
			}
		}
				

		bool parse()
		{
			if (m_parsed) return true;
			m_parsed = true;
			m_errorList.clear();
			for (size_t i=0;i<m_eqnList.size();i++)
				delete m_eqnList[i];
			m_eqnList.clear();

			while ( token() != lk::lexer::INVALID
				&& token() != lk::lexer::END
				&& m_errorList.size() == 0 )
			{

				node_t *lhs = additive();
				if ( !lhs )
				{
					error("%d: could not parse equation lhs", line());
					break;
				}

				match( lk::lexer::OP_ASSIGN );
				node_t *rhs = additive();
				if ( !rhs )
				{
					error("%d: could not parse equation rhs", line());
					delete lhs;
					break;
				}

				lk::expr_t *expr = new lk::expr_t( srcpos(), lk::expr_t::MINUS,  lhs, rhs );
				m_eqnList.push_back( expr );

				match( lk::lexer::SEP_SEMI );
			}

			if (m_errorList.size() > 0)
			{
				for ( size_t i=0;i<m_eqnList.size();i++)
					delete m_eqnList[i];
				m_eqnList.clear();
				return false;
			}

			return true;
		}
		
		lk::node_t *additive()
		{
			if (m_haltFlag) return 0;
	
			node_t *n = multiplicative();
	
			while ( token( lk::lexer::OP_PLUS )
				|| token( lk::lexer::OP_MINUS ) )
			{
				int oper = token(lk::lexer::OP_PLUS) ? expr_t::PLUS : expr_t::MINUS;
				skip();
		
				node_t *left = n;
				node_t *right = multiplicative();
		
				n = new lk::expr_t( srcpos(), oper, left, right );		
			}
	
			return n;
	
		}		

		lk::node_t *multiplicative()
		{
			if (m_haltFlag) return 0;
	
			node_t *n = exponential();
	
			while ( token( lk::lexer::OP_MULT )
				|| token( lk::lexer::OP_DIV ) )
			{
				int oper = token(lk::lexer::OP_MULT) ? expr_t::MULT : expr_t::DIV ;
				skip();
		
				node_t *left = n;
				node_t *right = exponential();
		
				n = new lk::expr_t( srcpos(), oper, left, right );		
			}
	
			return n;
		}

		lk::node_t *exponential()
		{
			if (m_haltFlag) return 0;
	
			node_t *n = unary();
	
			if ( token(lk::lexer::OP_EXP) )
			{
				skip();
				n = new expr_t( srcpos(), expr_t::EXP, n, exponential() );
			}
	
			return n;
		}

		lk::node_t *unary()
		{
			if (m_haltFlag) return 0;

			switch( token() )
			{
			case lk::lexer::OP_MINUS:
				skip();
				return new lk::expr_t( srcpos(), expr_t::NEG, unary(), 0 );
			default:
				return primary();
			}
		}

		lk::node_t *primary()
		{
			if (m_haltFlag) return 0;
	
			node_t *n = 0;
			switch( token() )
			{
			case lk::lexer::SEP_LPAREN:
				skip();
				n = additive();
				match(lk::lexer::SEP_RPAREN);
				return n;
			case lk::lexer::NUMBER:
				n = new lk::constant_t( srcpos(), lex.value() );
				skip();
				return n;
			case lk::lexer::IDENTIFIER:
				{
					lk_string name = lex.text();
					skip();
					if ( token( lk::lexer::SEP_LPAREN ) )
					{
						funccall_t *f =  new funccall_t( srcpos(), name);
						skip();

						while( token() != lk::lexer::INVALID
							&& token() != lk::lexer::END
							&& token() != lk::lexer::SEP_RPAREN )
						{
							f->args.push_back( additive() );
							if ( token() != lk::lexer::SEP_RPAREN )
								match( lk::lexer::SEP_COMMA );
						}

						match( lk::lexer::SEP_RPAREN );
						return f;
					}
					else						
						return new lk::iden_t( srcpos(), name, false, false, false );
				}
				break;
			default:
				error("expected identifier, function call, or constant value");
				m_haltFlag = true;
				break;
			}

			return 0;
		}
	};
};

static const char *eqn_default=
"h=500;\n"
"D=0.1;\n"
"r=D/2;\n"
"k=20;\n"
"Biot=(h*r/2)/k;\n"
"lambda*besj1(lambda)-Biot*besj0(lambda)=0;\n"
"C=1/lambda*besj1(lambda)/(1/2*(besj0(lambda)^2 + besj1(lambda)^2));\n";

class FreesWindow : public wxFrame
{
private:
	wxTextCtrl *m_input;
	wxTextCtrl *m_output;
	wxTextCtrl *m_maxIter;
	wxTextCtrl *m_fTol;
	wxTextCtrl *m_fRelax;
	wxCheckBox *m_showIters;
	wxTextCtrl *m_cmdLine;
	lk::env_t m_evalEnv;

public:
	FreesWindow( )
		: wxFrame( NULL, wxID_ANY, "FREES", wxDefaultPosition, wxSize(800,600) )
	{
		SetBackgroundColour( *wxWHITE );
		SetIcon( wxICON( appicon ) );

		wxBoxSizer *tools = new wxBoxSizer( wxHORIZONTAL );
		tools->Add( new wxButton(this, ID_SOLVE, "Solve" ), 0, wxALIGN_CENTER_VERTICAL|wxALL, 2 );
		tools->Add( new wxStaticText(this, wxID_ANY, "   Iterations:"), 0, wxALIGN_CENTER_VERTICAL|wxALL, 2 );
		tools->Add( m_maxIter = new wxTextCtrl(this, wxID_ANY, "10000"), 0, wxALIGN_CENTER_VERTICAL|wxALL, 2 );
		tools->Add( new wxStaticText(this, wxID_ANY, "   Tolerance:"), 0, wxALIGN_CENTER_VERTICAL|wxALL, 2 );
		tools->Add( m_fTol = new wxTextCtrl(this, wxID_ANY, "1e-11"), 0, wxALIGN_CENTER_VERTICAL|wxALL, 2 );
		tools->Add( new wxStaticText(this, wxID_ANY, "   Relax:"), 0, wxALIGN_CENTER_VERTICAL|wxALL, 2 );
		tools->Add( m_fRelax = new wxTextCtrl(this, wxID_ANY, "0.2"), 0, wxALIGN_CENTER_VERTICAL|wxALL, 2 );
		tools->Add( m_showIters = new wxCheckBox(this, wxID_ANY, "Show intermediates"), 0, wxALIGN_CENTER_VERTICAL|wxALL, 2 );
		tools->Add( new wxButton(this, ID_DEMO, "Demo" ), 0, wxALIGN_CENTER_VERTICAL|wxALL, 2 );
		m_showIters->SetValue( false );

		tools->AddStretchSpacer();
	
		wxSplitterWindow *split = new wxSplitterWindow( this );

		m_input = new wxTextCtrl( split, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE );
		m_input->SetFont( wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "Consolas") );
		m_output = new wxTextCtrl( split, wxID_ANY, "", wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE );
		m_output->SetFont( wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "Consolas") );
		m_output->SetForegroundColour( "FOREST GREEN" );

		m_cmdLine = new wxTextCtrl( this, ID_CMDLINE, "" , wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER);
		m_cmdLine->SetFont( wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "Consolas") );
		m_cmdLine->SetForegroundColour( *wxBLUE );

		
		m_input->SetValue( eqn_default );

		
		wxConfig cfg("FREES", "apdsoft");
		
		wxString eqns, hist;
		if (cfg.Read("eqns", &eqns)) m_input->SetValue(eqns);
		if (cfg.Read("hist", &hist)) m_output->SetValue(hist);
		
		double maxit, ftol, relax;
		if (cfg.Read("maxit", &maxit)) m_maxIter->SetValue( wxString::Format("%lg",maxit) );
		if (cfg.Read("ftol", &ftol)) m_fTol->SetValue( wxString::Format("%lg",ftol) );
		if (cfg.Read("relax", &relax)) m_fRelax->SetValue( wxString::Format("%lg", relax) );
		
		bool intermed;
		if (cfg.Read("intermed", &intermed)) m_showIters->SetValue( intermed );
		

		split->SplitHorizontally( m_input, m_output, 200 );

		wxBoxSizer *sz = new wxBoxSizer( wxVERTICAL );
		sz->Add( tools, 0, wxALL|wxEXPAND );
		sz->Add( split,  1, wxALL|wxEXPAND );
		sz->Add( m_cmdLine, 0, wxALL|wxEXPAND );
		SetSizer( sz );

		m_input->SetFocus();


		m_evalEnv.register_funcs( lk::stdlib_math() );

		wxAcceleratorEntry entries[7];
		entries[0].Set( wxACCEL_NORMAL, WXK_F5, ID_SOLVE );
		entries[1].Set( wxACCEL_NORMAL, WXK_ESCAPE,  wxID_CLOSE );
		/*entries[2].Set( wxACCEL_CTRL,   'o',  wxID_OPEN );
		entries[3].Set( wxACCEL_NORMAL, WXK_F2, wxID_PREFERENCES );
		entries[4].Set( wxACCEL_NORMAL, WXK_F3, ID_SHOW_STATS );
		entries[5].Set( wxACCEL_NORMAL, WXK_F4, ID_ADD_VARIABLE );
		entries[6].Set( wxACCEL_NORMAL, WXK_F5, ID_START );*/
		SetAcceleratorTable( wxAcceleratorTable(2,entries) );
		m_cmdLine->SetFocus();
			
	}

	void OnDemo( wxCommandEvent & )
	{
		m_input->SetValue(eqn_default);
	}

	void OnSolve( wxCommandEvent & )
	{
		wxBusyInfo inf("Solving, please wait");
		wxYield();

		lk::input_string p( m_input->GetValue() );
		lk::eqnsolve ee( p );
				
		wxStopWatch sw;
		int code = ee.solve( m_evalEnv, atoi( m_maxIter->GetValue().c_str() ),
			atof( m_fTol->GetValue().c_str() ),
			atof( m_fRelax->GetValue().c_str() ),
			m_showIters->GetValue() ? m_output : 0 );
		if ( code < 0 )
		{
			m_output->AppendText( ee.errortext() );
		}
		else
		{
			m_output->AppendText(wxString::Format("solved ok (%d iter, %.3lf sec)\n\n", code, sw.Time()/1000.0));
			m_output->AppendText(ee.variables(m_evalEnv));
		}

	}

	void OnCloseFrame( wxCloseEvent & )
	{
		wxConfig cfg("FREES", "apdsoft");
		cfg.Write("eqns", m_input->GetValue());
		cfg.Write("hist", m_output->GetValue());
		cfg.Write("maxit", atof( m_maxIter->GetValue().c_str() ) );
		cfg.Write("ftol", atof( m_fTol->GetValue().c_str() ) );
		cfg.Write("relax", atof( m_fRelax->GetValue().c_str() ) );
		cfg.Write("intermed", m_showIters->GetValue() );
		Destroy();

	}

	void OnCmdLine( wxCommandEvent &evt )
	{
		static wxString fmt = "%lg";
		m_output->AppendText( ">> " + m_cmdLine->GetValue() + "\n");

		wxString text = m_cmdLine->GetValue().Trim().Trim(false);
		wxString var_assign = "ans";
		if (text == "q" || text == "quit")
			Close();
		else if ( text == "help" || text == "?" )
		{
			m_output->AppendText("commands: help quit clear erase short long solve env\n");
			m_cmdLine->Clear();
			return;
		}
		else if ( text == "clear" )
		{
			m_output->Clear();
			m_cmdLine->Clear();
			return;
		}
		else if ( text == "erase" )
		{
			m_evalEnv.clear_vars();
			m_output->AppendText("erased all variables\n");
			m_cmdLine->Clear();
			return;
		}
		else if ( text == "solve" )
		{
			OnSolve( evt );
			m_cmdLine->Clear();
			return;
		}
		else if (text == "env")
		{
			lk_string key;
			lk::vardata_t *v;
			bool next = m_evalEnv.first( key, v );
			while( next )
			{
				m_output->AppendText( key + " = " + wxString::Format(fmt.c_str(), v->as_number() )  + "\n" );
				next = m_evalEnv.next( key, v );
			}
			m_cmdLine->Clear();
			return;
		}
		else if ( text.Index('=') != wxNOT_FOUND )
		{
			int pos = text.Index('=');
			var_assign = text.Left(pos).Trim().Trim(false);
			text = text.Mid(pos+1);
			m_cmdLine->Clear();
		}
		else if ( text == "long" )
		{
			fmt = "%.15lf";
			m_output->AppendText("format set to long\n");
			m_cmdLine->Clear();
			return;
		}
		else if ( text == "short" )
		{
			fmt = "%lg";
			m_output->AppendText("format set to short\n");
			m_cmdLine->Clear();
			return;
		}
		
		lk::input_string input( text );
		lk::eqnsolve ee( input );

		lk::node_t *tree = ee.additive();
		std::vector<lk_string> err = ee.errors();
		if ( tree == 0 || err.size() > 0 )
		{
			if (tree) delete tree;
			for (size_t i=0;i<err.size();i++)
				m_output->AppendText( err[i] + "\n" );
			m_cmdLine->SelectAll();
		}
		else
		{
			double result = 0;
			try {
				result = ee.eval_eqn( tree, m_evalEnv );
				lk::vardata_t *var = new lk::vardata_t;
				var->assign( result );
				m_evalEnv.assign( var_assign, var );
				m_output->AppendText( var_assign + " = " + wxString::Format(fmt.c_str(), result )  + "\n");			
			} catch( lk::evalexception &ex ) {
				m_output->AppendText( wxString(ex.what()) + "\n");
			}
			
			m_cmdLine->Clear();
			delete tree;
		}


	}
	
	void OnCloseRequest( wxCommandEvent & )
	{
		Close();
	}

	DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE( FreesWindow, wxFrame )
	EVT_TEXT_ENTER( ID_CMDLINE, FreesWindow::OnCmdLine )
	EVT_BUTTON( ID_SOLVE, FreesWindow::OnSolve )
	EVT_BUTTON( ID_DEMO, FreesWindow::OnDemo )
	EVT_CLOSE( FreesWindow::OnCloseFrame )
	EVT_MENU( wxID_CLOSE, FreesWindow::OnCloseRequest )
END_EVENT_TABLE()

class FreesApp : public wxApp
{
public:
	bool OnInit()
	{
#ifdef __WXMSW__
    typedef BOOL (WINAPI *SetProcessDPIAware_t)(void); 
    wxDynamicLibrary dllUser32(wxT("user32.dll")); 
    SetProcessDPIAware_t pfnSetProcessDPIAware = 
        (SetProcessDPIAware_t)dllUser32.RawGetSymbol(wxT("SetProcessDPIAware")); 
    if ( pfnSetProcessDPIAware ) 
        pfnSetProcessDPIAware(); 
#endif
		(new FreesWindow)->Show();
		return true;
	}
};

IMPLEMENT_APP( FreesApp );

#ifdef __WXMAC__
#include "../lk_absyn.cpp"
#include "../lk_env.cpp"
#include "../lk_eval.cpp"
#include "../lk_lex.cpp"
#include "../lk_parse.cpp"
#include "../lk_stdlib.cpp"
#include "../lk_math.cpp"
#include "../lk_invoke.cpp"
#endif
