#include <sstream>
#include <fstream>

#include "core.h"
#include "tcskernel.h"

const var_info var_info_invalid = {	0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
tcstypeprovider sg_tcsTypeProvider;

compute_module::compute_module( )
	:  m_infomap(NULL), m_handler(NULL), m_vartab(NULL)
{
	/* nothing to do */
}

compute_module::~compute_module()
{
	if (m_infomap) delete m_infomap;
}

bool compute_module::compute( handler_interface *handler, var_table *data )
{
	m_handler = NULL;
	m_vartab = NULL;

	if (!handler)
	{
		log("no request handler assigned to computation engine", SSC_ERROR);
		return false;
	}
	m_handler = handler;

	if (!data)
	{
		log("no data object assigned to computation engine", SSC_ERROR);
		return false;
	}
	m_vartab = data;

	if (m_varlist.size() == 0)
	{
		log("no variables defined for computation engine", SSC_ERROR);
		return false;
	}
	
	try { // catch any 'general_error' that can be thrown during precheck, exec, and postcheck

		if (!verify("precheck input", SSC_INPUT)) return false;
		exec();
		if (!verify("postcheck output", SSC_OUTPUT)) return false;

	} catch ( general_error &e )	{
		log( e.err_text, SSC_ERROR, e.time );
		return false;
	}
	
	return true;
}

bool compute_module::verify(const std::string &phase, int check_var_type) throw( general_error )
{
	std::vector< var_info* >::iterator it;
	for (it=m_varlist.begin();it!=m_varlist.end();++it)
	{
		var_info *vi = *it;
		if ( vi->var_type == check_var_type
			|| vi->var_type == SSC_INOUT )
		{
			if ( check_required( vi->name ) )
			{
				// if the variable is required, make sure it exists
				// and that it is of the correct data type
				var_data *dat = lookup( vi->name );
				if (!dat)
				{
					log(phase + ": variable '" + std::string(vi->name) + "' required but not assigned");
					return false;
				}
				else if (dat->type != vi->data_type)
				{
					log(phase + ": variable '" + std::string(vi->name) + "' (" + var_data::type_name(dat->type) + ") of wrong type, " + var_data::type_name( vi->data_type ) + " required.");
					return false;
				}

				// now check constraints on it
				std::string fail_text;
				if (!check_constraints( vi->name, fail_text ))
				{
					log(fail_text, SSC_ERROR);
					return false;
				}
			}
		}
	}

	return true;
}

void compute_module::add_var_info( var_info vi[] )
{
	int i=0;
	while ( vi[i].data_type != SSC_INVALID
		&& vi[i].name != NULL )
	{
		m_varlist.push_back( &vi[i] );
		i++;
	}
}

void compute_module::build_info_map()
{
	if (m_infomap) delete m_infomap;

	m_infomap = new unordered_map<std::string, var_info*>;
	
	std::vector<var_info*>::iterator it;
	for (it = m_varlist.begin(); it != m_varlist.end(); ++it)
		(*m_infomap)[ (*it)->name ] = *it;
}

bool compute_module::update( const std::string &current_action, float percent_done, float time )
{
	// forward to handler interface
	if (m_handler) return m_handler->on_update( current_action, percent_done, time);
	else return true;
}

void compute_module::log( const std::string &msg, int type, float time )
{
	// forward to handler interface
	if (m_handler) m_handler->on_log( msg, type, time );

	// also save it in module object
	m_loglist.push_back( log_item( type, msg, time ) );
}

void compute_module::clear_log()
{
	m_loglist.clear();
}

bool compute_module::extproc( const std::string &command, const std::string &workdir )
{
/*
	if (m_handler) return m_handler->on_exec( command, workdir);
	else return false;
*/
	return false; // on_exec removed with rev 578
}

compute_module::log_item *compute_module::log(int index)
{
	if (index >= 0 && index < (int)m_loglist.size())
		return &m_loglist[index];
	else 
		return NULL;
}
	
var_info *compute_module::info(int index)
{
	if (index >= 0 && index < (int)m_varlist.size())
		return m_varlist[index];
	else
		return NULL;
}

const var_info &compute_module::info( const std::string &name ) throw( general_error )
{
	// if there is an info lookup table, use it
	if (m_infomap != NULL)
	{
		unordered_map<std::string, var_info*>::iterator pos = m_infomap->find(name);
		if (pos != m_infomap->end())
			return (*(pos->second));
	}

	// otherwise search
	std::vector< var_info* >::iterator it;
	for (it = m_varlist.begin(); it != m_varlist.end(); ++it)
	{
		if ( (*it)->name == name )
			return *(*it);
	}

	throw general_error("variable information lookup fail: '" + name + "'");
}

bool compute_module::is_ssc_array_output( const std::string &name ) throw( general_error )
{
	// if there is an info lookup table, use it
	if (m_infomap != NULL)
	{
		unordered_map<std::string, var_info*>::iterator pos = m_infomap->find(name);
		if (pos != m_infomap->end())
		{
			if ( ( ((pos->second)->var_type == SSC_OUTPUT) || ((pos->second)->var_type == SSC_INOUT) ) && (pos->second)->data_type == SSC_ARRAY )
				return true;
		}
	}

	// otherwise search
	std::vector< var_info* >::iterator it;
	for (it = m_varlist.begin(); it != m_varlist.end(); ++it)
	{
		if ( ( ( (*it)->var_type == SSC_OUTPUT ) || ( (*it)->var_type == SSC_INOUT ) ) && (*it)->data_type == SSC_ARRAY )
			if ( util::lower_case((*it)->name) == util::lower_case(name) ) return true;
	}

	return false;
}


var_data *compute_module::lookup( const std::string &name ) throw( general_error )
{
	if (!m_vartab) throw general_error("invalid data container object reference");
	return m_vartab->lookup(name);
}

var_data *compute_module::assign( const std::string &name, const var_data &value ) throw( general_error )
{
	if (!m_vartab) throw general_error("invalid data container object reference");
	return m_vartab->assign( name, value );
}

ssc_number_t *compute_module::allocate( const std::string &name, size_t length ) throw( general_error )
{
	var_data *v = assign(name, var_data());
	v->type = SSC_ARRAY;
	v->num.resize_fill( length, 0.0 );
	return v->num.data();
}

ssc_number_t *compute_module::allocate( const std::string &name, size_t nrows, size_t ncols ) throw( general_error )
{
	var_data *v = assign(name, var_data());
	v->type = SSC_MATRIX;
	v->num.resize_fill(nrows, ncols, 0.0);
	return v->num.data();
}

util::matrix_t<ssc_number_t>& compute_module::allocate_matrix( const std::string &name, size_t nrows, size_t ncols ) throw( general_error )
{
	var_data *v = assign(name, var_data());
	v->type = SSC_MATRIX;
	v->num.resize_fill(nrows, ncols, 0.0);
	return v->num;
}

var_data &compute_module::value( const std::string &name ) throw( general_error )
{
	var_data *v = lookup( name );
	if (!v)	throw general_error("ssc variable does not exist: '" + name + "'");
	return (*v);
}

bool compute_module::is_assigned( const std::string &name ) throw( general_error )
{
	return (lookup(name) != 0);
}

int compute_module::as_integer( const std::string &name ) throw( general_error )
{
	var_data &x = value(name);
	if (x.type != SSC_NUMBER) throw cast_error("integer", x, name);
	return (int) x.num;
}

bool compute_module::as_boolean( const std::string &name ) throw( general_error )
{
	var_data &x = value(name);
	if (x.type != SSC_NUMBER) throw cast_error("boolean", x, name);
	return (bool) ( (int)(x.num!=0) );
}

float compute_module::as_float( const std::string &name ) throw( general_error )
{
	var_data &x = value(name);
	if (x.type != SSC_NUMBER) throw cast_error("float", x, name);
	return (float) x.num;
}

ssc_number_t compute_module::as_number( const std::string &name ) throw( general_error )
{
	var_data &x = value(name);
	if (x.type != SSC_NUMBER) throw cast_error("ssc_number_t", x, name);
	return x.num;
}

double compute_module::as_double( const std::string &name ) throw( general_error )
{
	var_data &x = value(name);
	if (x.type != SSC_NUMBER) throw cast_error("double", x, name);
	return (double) x.num;
}

const char *compute_module::as_string( const std::string &name ) throw( general_error )
{
	var_data &x = value(name);
	if (x.type != SSC_STRING) throw cast_error("string", x, name);
	return x.str.c_str();
}

ssc_number_t *compute_module::as_array( const std::string &name, size_t *count ) throw( general_error )
{
	var_data &x = value(name);
	if (x.type != SSC_ARRAY) throw cast_error("array", x, name);
	if (count) *count = x.num.length();
	return x.num.data();
}

std::vector<double> compute_module::as_doublevec( const std::string &name ) throw( general_error )
{
	var_data &x = value(name);
	if (x.type != SSC_ARRAY) throw cast_error("array", x, name);
	size_t len = x.num.length();
	std::vector<double> v(len);
	ssc_number_t *p = x.num.data();
	for (size_t k=0;k<len;k++)
		v[k] = (double) p[k];
	return v;
}

ssc_number_t *compute_module::as_matrix( const std::string &name, size_t *rows, size_t *cols ) throw( general_error )
{
	var_data &x = value(name);
	if (x.type != SSC_MATRIX) throw cast_error("matrix", x, name);
	if (rows) *rows = x.num.nrows();
	if (cols) *cols = x.num.ncols();
	return x.num.data();
}

util::matrix_t<double> compute_module::as_matrix(const std::string &name) throw(general_error)
{
	var_data &x = value(name);
	if (x.type != SSC_MATRIX) throw cast_error("matrix", x, name);

	util::matrix_t<double> mat(x.num.nrows(), x.num.ncols(), 0.0);
	for (size_t r = 0; r<x.num.nrows(); r++)
		for (size_t c = 0; c<x.num.ncols(); c++)
			mat.at(r, c) = (double)x.num(r, c);

	return mat;
}

util::matrix_t<double> compute_module::as_matrix_transpose(const std::string &name) throw(general_error)
{
	var_data &x = value(name);
	if (x.type != SSC_MATRIX) throw cast_error("matrix", x, name);

	util::matrix_t<double> mat(x.num.ncols(), x.num.nrows(), 0.0);
	for (size_t r = 0; r<x.num.nrows(); r++)
		for (size_t c = 0; c<x.num.ncols(); c++)
			mat.at(c, r) = (double)x.num(r, c);

	return mat;
}

bool compute_module::get_matrix(const std::string &name, util::matrix_t<ssc_number_t> &mat) throw(general_error)
{
	var_data &x = value(name);
	if (x.type != SSC_MATRIX) throw cast_error("matrix", x, name);

	size_t nrows, ncols;
	ssc_number_t *arr = as_matrix(name, &nrows, &ncols);

	if (nrows < 1 || ncols < 1)
		return false;

	mat.resize_fill(nrows, ncols, 1.0);
	for (size_t r = 0; r<nrows; r++)
		for (size_t c = 0; c<ncols; c++)
			mat.at(r, c) = arr[r*ncols + c];

	return true;
}



ssc_number_t compute_module::get_operand_value( const std::string &input, const std::string &cur_var_name) throw( general_error )
{	
	if (input.length() < 1) throw check_error(cur_var_name, "input is null to get_operand_value", input);

	if (isalpha(input[0]))
	{
		var_data *v = lookup(input);
		if (!v) throw check_error(cur_var_name, "unassigned referenced",  input );
		if (v->type != SSC_NUMBER) throw check_error(cur_var_name, "number type required", input );
		return v->num;
	}
	else
	{
		double x = 0;
		if (!util::to_double( input, &x )) throw check_error(cur_var_name, "number conversion", input );
		return (ssc_number_t) x;
	}
}

bool compute_module::check_required( const std::string &name ) throw( general_error )
{
	// only check if the variable is required as input to the simulation context
	// if it is an input or an inout variable

	const var_info &inf = info(name);
	if (inf.required_if == NULL || strlen(inf.required_if)==0)
		return false;

	std::string reqexpr = inf.required_if;

	if (reqexpr == "*")
	{
		return true; // Always required
	}
	else if (reqexpr == "?")
	{
		return false; // Always optional
	}
	else if (reqexpr.length() > 2 && reqexpr[0] == '?' && reqexpr[1] == '=')
	{
		// optional but has a default value that is assigned if variable is unassigned
		var_data *v = lookup(name);
		if (!v)
		{
			v = assign(name, m_null_value );

			if ( !var_data::parse( inf.data_type, reqexpr.substr(2), *v ) )
				throw check_error(name, "could not parse default value in required_if spec (" + var_data::type_name(inf.data_type) + ")", reqexpr);
		}

		return true; // a default value has been assigned, so this variable is effectively always required
	}
	else
	{
		// run tests
		std::string::size_type pos = std::string::npos;
		std::vector< std::string > expr_list = util::split(util::lower_case(reqexpr), "&|", true, true );
		
		int cur_result = -1;
		char cur_cond_oper = 0;
		for ( std::vector< std::string >::iterator it = expr_list.begin(); it != expr_list.end(); ++it )
		{
			std::string expr = *it;
			if (expr == "&")
			{
				if (cur_result == 0) // short circuit evaluation
					break;

				cur_cond_oper = '&';
				continue;
			}
			else if (expr == "|")
			{
				if (cur_result > 0) // short circuit evaluation
					break;

				cur_cond_oper = '|';
				continue;
			}
			else
			{
				int expr_result = 0;
				char op = 0;
				if ( (pos=expr.find('=')) != std::string::npos ) op = '=';
				else if ( (pos=expr.find('~')) != std::string::npos) op = '~';
				else if ( (pos=expr.find('<')) != std::string::npos ) op = '<';
				else if ( (pos=expr.find('>')) != std::string::npos ) op = '>';
				else if ( (pos=expr.find(':')) != std::string::npos ) op = ':';

				if (!op) throw check_error(name, "invalid operator", expr );

				std::string lhs = expr.substr(0, pos);
				std::string rhs = expr.substr(pos+1);
				
				if (lhs.length() < 1 || rhs.length() < 1) throw check_error(name, "null lhs or rhs in subexpr", expr);

				if (op == ':')
				{
					/* handle built-in test operators */

					if (lhs == "na") // check if variable name in 'rhs' is not assigned
					{
						expr_result = lookup(rhs)==NULL ? 1 : 0;
					}
					else if (lhs == "a") // check if variable name in 'rhs' is assigned
					{
						expr_result = lookup(rhs)!=NULL ? 1 : 0;
					}
					else if (lhs == "abt") // check if variable in 'rhs' is assigned, boolean type, and value true
					{
						var_data *v;
						if ( (v = lookup(rhs)) && v->type == SSC_NUMBER &&  ((int)v->num) != 0)
							return 1;
						else
							return 0;
					}
					else if (lhs == "abf") // check if variable in 'rhs' is assigned, boolean type, and value false
					{
						var_data *v;
						if ( (v = lookup(rhs)) && v->type == SSC_NUMBER &&  ((int)v->num) == 0)
							return 1;
						else
							return 0;
					}
					else if (lhs == "naof") // check if variable is not assigned OR boolean value is 'false'
					{
						var_data *v;
						if ( (v = lookup(rhs)) == 0 ) return 1;
						if ( v->type == SSC_NUMBER && ((int)v->num)==0 ) return 1;

						return 0;
					}
					else
					{
						throw check_error(name, "invalid built-in test", expr);
					}
				}
				else
				{
					ssc_number_t lhs_val = get_operand_value(lhs,name);
					ssc_number_t rhs_val = get_operand_value(rhs,name);

					switch(op)
					{
					case '=': expr_result = lhs_val == rhs_val ? 1 : 0 ; break;
					case '~': expr_result = lhs_val != rhs_val ? 1 : 0; break;
					case '<': expr_result = lhs_val < rhs_val ? 1 : 0 ; break;
					case '>': expr_result = lhs_val > rhs_val ? 1 : 0 ; break;
					default: throw check_error(name, "invalid numerical operator", expr);
					}
				}

				if (cur_result < 0)
				{
					cur_result = expr_result;
				}
				else if (cur_cond_oper == '&')
				{
					cur_result = (cur_result && expr_result);
				}
				else if (cur_cond_oper == '|')
				{
					cur_result = (cur_result || expr_result);
				}

				else
					throw check_error(name, "invalid evaluation sequence", reqexpr);
			}
		}

		return cur_result != 0 ? true : false;
	}
	
	return false;
}

bool compute_module::check_constraints( const std::string &name, std::string &fail_text) throw( general_error )
{
#define fail_constraint( str ) { fail_text = "fail("+name+", "+expr+"): "+std::string(str); return false; }

	const var_info &inf = info(name);

	if (inf.constraints == NULL) return true; // pass if no constraints defined

	var_data &dat = value(name);
	
	std::vector< std::string > exprlist = util::split( inf.constraints, "," );
	for ( std::vector<std::string>::iterator it=exprlist.begin(); it!=exprlist.end(); ++it )
	{
		std::string::size_type pos;
		std::string expr = util::lower_case(*it);
		if (expr == "tmyepw")
		{
			if (dat.type != SSC_STRING || dat.str.length() <= 4)
				fail_constraint("string data type required with length greater than 4 chars: " + dat.str);

			std::string ext = util::lower_case( dat.str.substr( dat.str.length()-3 ) );
			if (ext != "tm2" || ext != "tm3" || ext != "epw" || ext != "csv")
				fail_constraint("file extension was not tm2,tm3,epw,csv: " + ext);
		}
		else if (expr == "local_file")
		{
			if (dat.type != SSC_STRING)
				fail_constraint("string data type required");

			std::ifstream f_in( dat.str.c_str(), std::ios_base::in );
			if (f_in.is_open())
				f_in.close();
			else
				fail_constraint("could not open for read: '" + dat.str + "'");
		}
		else if (expr == "mxh_schedule")
		{
			if (dat.type != SSC_STRING)
				fail_constraint("string data type required");

			if (dat.str.length() != 288)
				fail_constraint( "288 characters required (24x12) but " + util::to_string((int)dat.str.length()) + " found" );
			
			for ( std::string::size_type i=0;i<dat.str.length(); i++)
				if ( dat.str[i] < '0' || dat.str[i] > '9' ) 
					fail_constraint( util::format("invalid character %c at %d", (char)dat.str[i], (int)i) );
		}
		else if (expr == "boolean")
		{
			if (dat.type != SSC_NUMBER)
				fail_constraint("number data type required");

			int val = (int)dat.num;
			if (val != 0 && val != 1)
				fail_constraint("value was not 0 nor 1");
		}
		else if (expr == "integer")
		{
			if (dat.type != SSC_NUMBER)
				fail_constraint("number data type required");

			if ( ((ssc_number_t)((int)dat.num)) != dat.num )
				fail_constraint("number could not be interpreted as an integer: " + util::to_string( (double) dat.num ));
		}
		else if (expr == "tousched")
		{
			if (dat.type != SSC_STRING)
				fail_constraint("string data type required");

			if (dat.str.length() != 288)
				fail_constraint("288 character string required (12x24 values)");

			for (std::string::size_type i=0;i<dat.str.length();i++)
			{
//				if ( dat.str[i] < '1' || dat.str[i] > '9' )
				if ( util::schedule_char_to_int(dat.str[i]) == 0 )
					fail_constraint("all digits must be between 1 and 9, inclusive");
			}
		}
		else if (expr == "positive")
		{
			if (dat.type != SSC_NUMBER) throw constraint_error(name, "cannot test for positive with non-numeric type", expr);
			if (dat.num <= 0.0)
				fail_constraint( util::to_string( (double)dat.num ) );
		}
		else if (expr == "percent")
		{
			if (dat.type != SSC_NUMBER) throw constraint_error(name, "cannot test for percent (%) constraint with non-numeric type", expr);
			if (dat.num < 0.0 || dat.num > 100.0)
				fail_constraint( util::to_string( (double)dat.num ) );
		}
		else if (expr == "factor")
		{
			if (dat.type != SSC_NUMBER) throw constraint_error(name, "cannot test for factor (0..1) constraint with non-numeric type", expr);
			if (dat.num < 0.0 || dat.num > 1.0)
				fail_constraint( util::to_string( (double)dat.num ) );
		}
		else if (expr == "ts_m")
		{
			if (dat.type != SSC_NUMBER)
				fail_constraint("number data type required");

			int val = (int) dat.num;
			if (   val != 1
				&& val != 5
				&& val != 10
				&& val != 15
				&& val != 30
				&& val != 60
				)
			{
				fail_constraint("time step must be 1,5,10,15,30,60 minutes");
			}
		}
		else if ( (pos=expr.find('=')) != std::string::npos )
		{
			std::string test = expr.substr(0, pos);
			std::string rhs = expr.substr(pos+1);

			if (test == "min")
			{
				if (dat.type != SSC_NUMBER) throw constraint_error(name, "cannot test for min with non-numeric type", expr);
				double minval = 0;
				if (!util::to_double( rhs, &minval )) throw constraint_error(name, "test for min requires a number value", expr);
				if ( dat.num < (ssc_number_t)minval )
					fail_constraint( util::to_string( (double)dat.num ) );
			}
			else if (test == "max")
			{
				if (dat.type != SSC_NUMBER) throw constraint_error(name, "cannot test for max with non-numeric type", expr);
				double maxval = 0;
				if (!util::to_double( rhs, &maxval )) throw constraint_error(name, "test for max requires a numeric value", expr);
				if (dat.num > (ssc_number_t)maxval )
					fail_constraint( util::to_string( (double)dat.num ) );
			}
			else if (test == "length")
			{
				if (dat.type != SSC_ARRAY) throw constraint_error(name, "cannot test for length with non-array type", expr);
				int lenval = 0;
				if (!util::to_integer( rhs, &lenval )) throw constraint_error(name, "test for length requires an integer value", expr);
				size_t len = (size_t)lenval;
				if (dat.num.length() != len)
					fail_constraint( util::to_string( (int)dat.num.length() ) );
			}
			else if (test == "length_equal")
			{
				if (dat.type != SSC_ARRAY) throw constraint_error(name, "cannot test for length_equal with non-array type", expr);
				var_data *other = lookup( rhs );
				if (!other) throw constraint_error(name, "length_equal cannot find variable to test against", expr);
				if (other->type == SSC_ARRAY)
				{
					if (dat.num.length() != other->num.length())
						fail_constraint( util::to_string( (int) other->num.length() ) );
				}
				else if (other->type == SSC_NUMBER)
				{
					if (dat.num.length() != (size_t)(ssc_number_t)other->num)
						fail_constraint( util::to_string( (int) other->num ) );
				}
				else throw constraint_error(name, "length_equal must specify a number or array variable to test against", expr);
			}
			else if (test == "length_multiple_of")
			{
				if (dat.type != SSC_ARRAY) throw constraint_error(name, "cannot test for length_multiple_of with non-array type", expr);
				int lenval = 0;
				if (!util::to_integer( rhs, &lenval ) || lenval < 1) throw constraint_error(name, "test for length_multiple_of requires a positive integer value", expr);
				size_t len = (size_t)lenval;
				size_t multiplier = dat.num.length() / len;
				if ( dat.num.length() < len || len*multiplier != dat.num.length() )
					fail_constraint( util::to_string( (int)dat.num.length() ) );
			}
			else if (test == "rows")
			{
				if (dat.type != SSC_MATRIX) throw constraint_error(name, "cannot test for rows with non-matrix type", expr);
				int nrows = 0;
				if (!util::to_integer( rhs, &nrows ) || nrows < 1) throw constraint_error(name, "test for rows requires a positive integer value", expr);
				if ( dat.num.nrows() != (size_t)nrows )
					fail_constraint( util::to_string( (int)dat.num.nrows() ) );
			}
			else if (test == "cols")
			{
				if (dat.type != SSC_MATRIX) throw constraint_error(name, "cannot test for cols with non-matrix type", expr);
				int ncols = 0;
				if (!util::to_integer( rhs, &ncols) || ncols < 1) throw constraint_error(name, "test for cols requires a positive integer value", expr);
				if ( dat.num.ncols() != (size_t)ncols )
					fail_constraint( util::to_string( (int)dat.num.ncols() ) );
			}
		}
		else
		{
			throw constraint_error( name, "invalid test or expression", expr );
		}

	}

	// all constraints passed fine
	return true;

#undef fail_constraint
}

size_t compute_module::check_timestep_seconds( double t_start, double t_end, double t_step ) throw( timestep_error )
{
	if (t_start < 0.0) throw timestep_error(t_start,t_end,t_step, "start time must be 0 or greater");
	if (t_end <= t_start) throw timestep_error(t_start,t_end,t_step, "end time must be greater than start time");
	if (t_end > 8760.0*3600.0) throw timestep_error(t_start,t_end,t_step, "end time cannot be greater than 8760*3600");
	if (t_step < 1.0) throw timestep_error(t_start,t_end,t_step, "time step must be greater or equal to than 1 sec");
	if (t_step > 3600.0) throw timestep_error(t_start,t_end,t_step, "the maximum allowed time step is 3600 sec");

	double duration = t_end - t_start;
	size_t steps = (size_t)(ceil(duration / t_step));

	/* time step notes:
	  
	  The start and end times represent the time at the beginning of an hour.  For example:

	    0 represents 12am on January 1st
		8759 represents 11pm on December 31st
		8760 represents 12am on January 1st of the next year

		As a result, suppose you are simulating only the first twelve hours of January, at 1/2 hour steps.  
		The 'time'  will take values of
	
		DataIndex: 0     1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16    17    18    19    20    21    22    23
		Time:      0     0.5   1     1.5   2     2.5   3     3.5   4     4.5   5     5.5   6     6.5   7     7.5   8     8.5   9     9.5   10    10.5  11    11.5

		Therefore, there will be 24 data values needed.

		To specify this time range, use
			t_start = 0
			t_end = 12
			t_step = 0.5

		Pseudo-code for iteration control is done as follows:

		time = t_start
		while ( time < t_end )
		{
			// do calculations for current time // 
			time = time + t_step
		}
	*/

	size_t max0 = (size_t)( steps*t_step );
	size_t max1 = (size_t)( duration );

	if ( max0 != max1 ) throw timestep_error(t_start, t_end, t_step, 
		util::format("invalid time step, must represent an integer number of minutes steps(%u != %u)", max0, max1).c_str());

	return steps;
}

ssc_number_t *compute_module::accumulate_monthly(const std::string &ts_var, const std::string &monthly_var, double scale) throw( exec_error )
{
		
	size_t count = 0;
	ssc_number_t *ts = as_array(ts_var, &count);

	size_t step_per_hour = count/8760;
	
	if (!ts || step_per_hour < 1 || step_per_hour > 60 || step_per_hour*8760 != count)
		throw exec_error("generic", "Failed to accumulate time series (hourly or subhourly): " + ts_var + " to monthly: " + monthly_var);

	
	ssc_number_t *monthly = allocate( monthly_var, 12 );

	size_t c = 0;
	for (int m=0;m<12;m++) // each month
	{
		monthly[m] = 0;
		for (int d=0;d<util::nday[m];d++) // for each day in each month
			for (int h=0;h<24;h++) // for each hour in each day
				for( size_t j=0;j<step_per_hour;j++ )
					monthly[m] += ts[c++];

		monthly[m] *= scale;
	}

	return monthly;
}

ssc_number_t *compute_module::accumulate_monthly_for_year(const std::string &ts_var, const std::string &monthly_var, double scale, size_t step_per_hour, size_t year) throw(exec_error)
{

	size_t count = 0;
	ssc_number_t *ts = as_array(ts_var, &count);

	size_t annual_values = step_per_hour * 8760;

	if (!ts || step_per_hour < 1 || step_per_hour > 60 || year*step_per_hour * 8760 > count)
		throw exec_error("generic", "Failed to accumulate time series (hourly or subhourly): " + ts_var + " to monthly: " + monthly_var);


	ssc_number_t *monthly = allocate(monthly_var, 12);

	size_t c = (year-1)*annual_values;
	for (int m = 0; m<12; m++) // each month
	{
		monthly[m] = 0;
		for (int d = 0; d<util::nday[m]; d++) // for each day in each month
			for (int h = 0; h<24; h++) // for each hour in each day
				for (size_t j = 0; j<step_per_hour; j++)
					monthly[m] += ts[c++];

		monthly[m] *= scale;
	}

	return monthly;
}

ssc_number_t compute_module::accumulate_annual(const std::string &ts_var, const std::string &annual_var, double scale) throw( exec_error )
{
	size_t count = 0;
	ssc_number_t *ts = as_array(ts_var, &count);

	size_t step_per_hour = count/8760;

	if (!ts || step_per_hour < 1 || step_per_hour > 60 || step_per_hour*8760 != count)
		throw exec_error("generic", "Failed to accumulate time series (hourly or subhourly): " + ts_var + " to annual: " + annual_var);
		
	double annual = 0;
	for ( size_t i=0;i<count;i++ )
		annual += ts[i];

	assign( annual_var, var_data( (ssc_number_t) (annual*scale) ) );

	return (ssc_number_t)(annual*scale);
}

ssc_number_t compute_module::accumulate_annual_for_year( const std::string &ts_var, 
	const std::string &annual_var, 
	double scale,
	size_t step_per_hour, 
	size_t year, 
    size_t steps) throw(exec_error)
{
	size_t count = 0;
	ssc_number_t *ts = as_array(ts_var, &count);

	size_t annual_values = step_per_hour * steps;	

	if (!ts || step_per_hour < 1 || step_per_hour > 60 || year*step_per_hour * steps > count)
		throw exec_error("generic", "Failed to accumulate time series (hourly or subhourly): " + ts_var + " to annual: " + annual_var);

	size_t istart = (year-1)*annual_values;
	size_t iend  = year*annual_values;
	
	double sum = 0;
	for (size_t i = istart; i < iend; i++)
		sum += ts[i];

	assign( annual_var, var_data((ssc_number_t)( sum * scale )));

	return (ssc_number_t)( sum*scale );
}
