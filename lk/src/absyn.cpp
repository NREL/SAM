#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <sstream>

#include <lk/absyn.h>
#include <lk/lex.h>


// static npos storage
const lk::srcpos_t lk::srcpos_t::npos;

bool lk::operator==(const srcpos_t &a, const srcpos_t &b)
{
	return a.line == b.line
		&& a.stmt == b.stmt
		&& a.stmt_end == b.stmt_end
		&& a.file == b.file;
}

static lk_string lk_tr_default( const lk_string &s ) { return s; }
static lk_string (*lk_tr_func)( const lk_string &) = lk_tr_default;

void lk::set_translation_function( lk_string (*f)(const lk_string&) )
{
	lk_tr_func = f;
}

#if defined(LK_USE_WXWIDGETS)

lk_string lk::get_translation( const lk_string &s )
{
	if ( lk_tr_func == 0 || lk_tr_func == lk_tr_default ) return wxGetTranslation( s );
	else return lk_tr_func( s );
}

lk_string lk::to_string( lk_char c )
{
	return lk_string(c);
}

std::string lk::to_utf8( const lk_string &str )
{
	return std::string( (const char*)str.ToUTF8() );
}

lk_string lk::from_utf8( const std::string &str )
{
	return lk_string::FromUTF8( str.c_str() );
}

lk_string lk::from_utf8( const char *str )
{
	return lk_string::FromUTF8( str );
}

lk_char lk::lower_char( lk_char c )
{
	return wxTolower(c);
}

lk_char lk::upper_char( lk_char c )
{
	return wxToupper(c);
}

bool lk::convert_integer(const lk_string &str, int *x)
{
	long lval;
	bool ok = str.ToLong(&lval);
	if (ok)
	{
		*x = (int) lval;
		return true;
	}
	else return false;
}

bool lk::convert_double(const lk_string &str, double *x)
{
	return str.ToDouble(x);
}

#else

	
lk_string lk::get_translation( const lk_string &s )
{
	return lk_tr_func( s );
}

lk_string lk::to_string( lk_char c )
{
	char buf[2] = { c, 0 };
	return std::string(buf);
}

std::string lk::to_utf8( const lk_string &str )
{
	return str;
}

lk_string lk::from_utf8( const std::string &str )
{
	return str;
}

lk_string lk::from_utf8( const char *str )
{
	return std::string( str );
}

lk_char lk::lower_char( lk_char c )
{
	return ::tolower( c );
}

lk_char lk::upper_char( lk_char c )
{
	return ::toupper( c );
}

bool lk::convert_integer(const lk_string &str, int *x)
{
	const lk_char *startp = str.c_str();
	lk_char *endp = NULL;
	*x = ::strtol( startp, &endp, 10 );
	return !*endp && (endp!=startp);
}

bool lk::convert_double(const lk_string &str, double *x)
{
	const lk_char *startp = str.c_str();
	lk_char *endp = NULL;
	*x = ::strtod( startp, &endp );
	return !*endp && (endp!=startp);
}

#endif


int lk::_node_alloc = 0;

const char *lk::expr_t::operstr()
{
	switch(oper)
	{
	case PLUS: return "+";
	case MINUS: return "-";
	case MULT: return "*";
	case DIV: return "/";
	case INCR: return "++";
	case DECR: return "--";
	case DEFINE: return "&define";
	case ASSIGN: return "=";
	case LOGIOR: return "||";
	case LOGIAND: return "&&";
	case NOT: return "!";
	case EQ: return "==";
	case NE: return "!=";
	case LT: return "<";
	case LE: return "<=";
	case GT: return ">";
	case GE: return ">=";
	case EXP: return "^";
	case NEG: return "-";
	case INDEX: return "[]";
	case HASH: return "{}";
	case THISCALL: return "->()";
	case CALL: return "()";
	case SIZEOF: return "&sizeof";
	case KEYSOF: return "&keysof";
	case TYPEOF: return "&typeof";
	case INITVEC: return "&initvec";
	case INITHASH: return "&inithash";
	case SWITCH: return "&switch";
	default:
		return "<!inv!>";
	}
}

const char *lk::ctlstmt_t::ctlstr()
{
	switch( ictl )
	{
	case RETURN : return "&return";
	case EXIT: return "&exit";
	case BREAK: return "&break";
	case CONTINUE: return "&continue";
	default:
		return "<!inv!>";
	}
}

static lk_string spacer(int lev)
{
	lk_string ret;
	for (int i=0;i<lev;i++) ret += lk_string("   ");
	return ret;
}

void lk::pretty_print( lk_string &str, node_t *root, int level )
{
	if (!root) return;

	if ( list_t *n = dynamic_cast<list_t*>( root ) )
	{
		str += spacer(level) + "{\n";
		for( size_t i=0;i<n->items.size();i++ )
		{
			pretty_print( str, n->items[i], level+1 );
			str += "\n";
		}
		str += spacer(level) + "}";
	}
	else if ( iter_t *n = dynamic_cast<iter_t*>( root ) )
	{
		str += spacer(level) + "loop(";

		pretty_print(str, n->init, level+1);
		str += "\n";

		pretty_print(str, n->test, level+1);
		str += "\n";

		pretty_print(str, n->adv, level+1);
		str += "\n";

		pretty_print(str, n->block, level+1 );
		str += "\n" + spacer(level) + ")";
	}
	else if ( cond_t *n = dynamic_cast<cond_t*>( root ) )
	{
		str += spacer(level) + "cond(";
		pretty_print(str, n->test, level + 1 );
		str += "\n";
		pretty_print(str, n->on_true, level+1);
		if (n->on_false )
			str += "\n";
		pretty_print(str, n->on_false, level+1);
		str += " )";
	}
	else if ( expr_t *n = dynamic_cast<expr_t*>( root ) )
	{
		str += spacer(level) + "(";
		str += n->operstr();
		str += "\n";
		pretty_print(str, n->left, level+1 );
		if (n->right)
			str += "\n";
		pretty_print( str, n->right, level+1 );
		str += ")";
	}
	else if ( ctlstmt_t *n = dynamic_cast<ctlstmt_t*>( root ) )
	{
		str += spacer(level) + "(";
		str += n->ctlstr();
		if ( n->rexpr )	str += "  ";		
		pretty_print( str, n->rexpr, level+1 );
		str += ")";
	}
	else if ( iden_t *n = dynamic_cast<iden_t*>( root ) )
	{
		str += spacer(level) + n->name;
	}
	else if ( constant_t *n = dynamic_cast<constant_t*>( root ) )
	{
		char buf[64];
		sprintf(buf, "%lg", n->value );
		str += spacer(level) + buf;
	}
	else if ( literal_t *n = dynamic_cast<literal_t*>( root ) )
	{
		str += spacer(level) + "'";
		str += n->value;
		str += "'";
	}
	else if ( 0 != dynamic_cast<null_t*>( root ) )
	{
		str += spacer(level) + "#null#";
	}
	else
	{
		str += "<!" + lk_tr("unknown node type") + "!>";
	}
}
