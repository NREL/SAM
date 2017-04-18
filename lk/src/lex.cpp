
#include <cstring>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <limits>

#include <lk/lex.h>

lk::input_string::input_string()
{
	m_buf = m_p = 0;
}

lk::input_string::input_string( const lk_string &in )
{
	m_buf = 0;

	std::string utf8 = lk::to_utf8( in );	
	allocate( utf8.length()+1 );
	if ( m_buf != 0 )
	{
		strcpy(m_buf, utf8.c_str());
		m_buf[ utf8.length() ] = 0;
	}
	
	m_p = m_buf;
}

lk::input_string::~input_string()
{
	if ( m_buf != 0 ) delete [] m_buf;
}

bool lk::input_string::allocate( size_t n )
{
	if ( m_buf ) delete [] m_buf;
	m_buf = new char[n];
	return ( 0 != m_buf );
}

char lk::input_string::operator*()
{
	if ( m_p != 0 ) return *m_p;
	else return 0;
}

char lk::input_string::operator++(int)
{
	if ( m_p != 0 )
	{
		if (*m_p) m_p++;
		return *m_p;
	}
	else
		return 0;
}

char lk::input_string::peek()
{
	if (m_p && *m_p) return *(m_p+1); else return 0;
}

lk::input_file::input_file( const lk_string &file )
	: input_string()
{
	int len;
	FILE *f = fopen( file.c_str(), "r" );
	if ( !f ) return;
	
	if ( 0 != fseek( f, 0, SEEK_END ) )
	{
		fclose( f );
		return;
	}
	
	len = ftell( f );
	
	allocate( len + 1 );
	if ( !m_buf )
	{
		fclose( f );
		return;
	}

	rewind( f );
	
	len = (int)fread( m_buf, sizeof(char), len, f );	
	m_buf[len] = 0;
	m_p = m_buf;

	fclose( f );
}

lk::input_file::~input_file()
{
	// nothing to do, parent class will free memory
}

lk::lexer::lexer( input_base &input )
	: p(input)
{
	m_line = 1;
	m_buf.reserve(256); // reserve some initial memory for the token buffer 
	m_val = 0.0;
}	

lk_string lk::lexer::text()
{
	return m_buf;
}

double lk::lexer::value()
{
	return m_val;
}

int lk::lexer::line()
{
	return m_line;
}

lk_string lk::lexer::error()
{
	return m_error;
}

void lk::lexer::whitespace( )
{
	while ( *p == '\n' || *p == ' ' || *p == '\r' || *p == '\t' ) // all other whitespace stripped out when loading input buffer
	{
		if (*p == '\n') m_line++;
		p++;
	}
}

bool lk::lexer::comments()
{
	bool handled_comment = false;
	// handle block comments 
	while ( *p == '/' && p.peek() == '*' )
	{
		handled_comment = true;
		p++;
		p++;
		
		while ( *p && p.peek() )
		{
			if (*p == '*' && p.peek() == '/')
			{
				p++;
				p++;
				break;
			}
			
			if (*p == '\n') m_line++;			
			p++;
		}
		whitespace();		
	}
	
	// handle single line comments
	while ( *p == '/' && p.peek() == '/' )
	{
		handled_comment = true;
		p++;
		p++;
		
		while ( *p && *p != '\n' ) p++;
		
		whitespace();	
	}

	return handled_comment;
}

int lk::lexer::next()
{
	m_buf = "";
	m_val = 0.0;
	
	if (!*p) return END;
	
	bool found_comments = false;
	do
	{
		whitespace();
		found_comments = comments();
		whitespace();
	}
	while ( found_comments );

	
	if (!*p) return END;

	// scan separators and operators
	switch ( (int) *p )
	{
	case ';': p++; return SEP_SEMI;
	case ':': p++; return SEP_COLON;
	case ',': p++; return SEP_COMMA;
	case '(': p++; return SEP_LPAREN;
	case ')': p++; return SEP_RPAREN;
	case '{': p++; return SEP_LCURLY;
	case '}': p++; return SEP_RCURLY;
	case '[': p++; return SEP_LBRACK;
	case ']': p++; return SEP_RBRACK;

	case '+': p++;	if ( (int) *p == '=' ) { p++; return OP_PLUSEQ;  } else if( (int) *p == '+' ) { p++; return OP_PP; } else return OP_PLUS;
	case '-': p++;	if ( (int) *p == '=' ) { p++; return OP_MINUSEQ; } else if( (int) *p == '@' ) { p++; return OP_MINUSAT; } else if( (int) *p == '-' ) { p++; return OP_MM; } else if ( (int) *p == '>' ) { p++; return OP_REF; } else return OP_MINUS;
	case '*': p++;  if ( (int) *p == '=' ) { p++; return OP_MULTEQ;  } else if( (int) *p == '*' ) { p++; return OP_EXP; } else return OP_MULT;
	case '/': p++;  if ( (int) *p == '=' ) { p++; return OP_DIVEQ;   } else return OP_DIV;

	case '^': p++; return OP_EXP;
	case '.': p++; return OP_DOT;
	case '?': p++;  if ( (int) *p == '@' ) { p++; return OP_QMARKAT; } return OP_QMARK;
	case '#': p++; return OP_POUND;
	case '~': p++; return OP_TILDE;
	case '@': p++; return OP_AT;
	case '%': p++; return OP_PERCENT;
	case '&': p++;  if( (int) *p == '&' ) { p++; return OP_LOGIAND; } else return OP_BITAND;
	case '|': p++;  if( (int) *p == '|' ) { p++; return OP_LOGIOR; } else return OP_BITOR;
	case '!': p++;	if( (int) *p == '=' ) { p++; return OP_NE; } else return OP_BANG;
	case '=': p++; if( (int) *p == '=' ) { p++; return OP_EQ; } else return OP_ASSIGN;
	case '<': p++; if( (int) *p == '=' ) { p++; return OP_LE; } else return OP_LT;
	case '>': p++; if( (int) *p == '=' ) { p++; return OP_GE; } else return OP_GT;
	}

	if ( *p == '$' )
	{
		p++;
		if ( *p == '{' )
		{
			p++;
			while( isalnum(*p) || *p == '_' || *p == '.' || *p == '*' || *p == '-' || *p == '%' )
			{
				m_buf += *p;
				p++;
			}

			if ( *p == '}' )
				p++;
			else
			{
				m_error = lk_tr("expected '}' to close special identifier");
				return INVALID;
			}

			return SPECIAL;
		}
		else
		{
			m_error = lk_tr("expected '{' to follow $ when starting a special identifier");
			return INVALID;
		}
	}

	// scan identifiers
	if ( isalpha( *p ) || *p == '_' )
	{
		while ( isalnum(*p) || *p == '_' )
		{
			m_buf += *p;
			p++;
		}

		return IDENTIFIER;
	}
	
	// scan numbers
	if ( isdigit( *p ) )
	{
		while( isdigit(*p) || *p == '.')
		{
			m_buf += *p;

			if ( *p == '.' && p.peek() == '#' )
			{
				p++; // skip .
				p++; // skip #
				while( isalpha(*p) )
					p++; // skip QNAN characters

				m_val = std::numeric_limits<double>::quiet_NaN();
				return NUMBER;
			}

			p++;
		}
		
		if ( *p != 0 )
		{			
			if ( *p == 'e' || *p == 'E' )
			{
				m_buf += 'e';
				p++;
				if (*p == '+' || *p == '-')
				{
					m_buf += *p;
					p++;
				}
				
				while ( isdigit(*p) ) 
				{
					m_buf += *p;
					p++;
				}
			}
			else if ( *p == 'T')
			{
				m_buf += "e+12";
				p++;
			}
			else if ( *p == 'G')
			{
				m_buf += "e+9";
				p++;
			}
			else if ( *p == 'k')
			{
				m_buf += "e+3";
				p++;
			}
			else if ( *p == 'm') 
			{
				m_buf += "e-3";
				p++;
			}
			else if ( *p == 'M') 
			{
				m_buf += "e+6";
				p++;
			}
			else if ( *p == 'u')
			{
				m_buf += "e-6";
				p++;
			}
			else if ( *p == 'n')
			{
				m_buf += "e-9";
				p++;
			}
			else if ( *p == 'p')
			{
				m_buf += "e-12";
				p++;
			}
			else if ( *p == 'f')
			{
				m_buf += "e-15";
				p++;
			}
			else if ( *p == 'a')
			{
				m_buf += "e-18";
				p++;
			}
		}
		
		m_val = atof( m_buf.c_str() );		
		return NUMBER;
	}
	
	// scan literal strings
	if ( *p == '"' || *p == '\'' )
	{
		while (*p == '"' || *p == '\'' )
		{ // allow multiple literals to be concatenated together

			char qclose = *p;

			p++;
		
			while ( *p && *p != qclose )
			{
				if ( *p == '\\' && p.peek() != 0 )
				{
					char cerr=0;
					switch (p.peek())
					{
					case '"':  m_buf += '"';  p++; break;
					case '\'': m_buf += '\''; p++; break;
					case 'n':  m_buf += '\n'; p++; break;
					case 'r':  m_buf += '\r'; p++; break;
					case 't':  m_buf += '\t'; p++; break;
					case '\\': m_buf += '\\'; p++; break;
					case '/': m_buf += '/'; p++; break; // allow \/ in json strings
					case 'u':
#ifdef LK_UNICODE
						{
							p++; // skip the slash
							p++; // skip the 'u'
							std::string buf;
							// read four digits
							for ( size_t k=0; *p && k<4;k++ )
							{
								buf += *p;
								if ( k < 3 ) p++; // leave the last one on the end string
							}
							// convert unicode char constant to string
							unsigned int uch = 0;
							sscanf( buf.c_str(), "%x", &uch );
							//m_buf += lk_string::Format("{u:%x,'%s' {", uch, buf.c_str()) + lk_char(uch) + lk_string("}}");
							if ( uch != 0 )
								m_buf += lk_char(uch);
						}
						break;
#else
						m_error = lk_tr("unicode character constant encountered while parsing with an ansi string build of LK.");
						return INVALID;
#endif
					default:
						cerr = *p;
						p++; // skip escape sequence
						while (*p && *p != qclose) p++; // skip to end of string literal despite error
					
						m_error = lk_tr("invalid escape sequence") + "\\" + cerr;
						return INVALID;
					}
				}
				else if (*p == '\n')
				{
					while (*p && *p != qclose) p++; // skip to end of string literal despite error
				
					m_error = lk_tr("newline found within string literal");
					return INVALID;
				}
				else
					m_buf += *p;
			
				p++;
			}
		
			if (*p) p++; // skip last quote

			
			whitespace(); // skip white space to move to next literal if there is another one
		} // loop to support multiple literals
			 	
		return LITERAL;
	}

	m_error = lk_tr("token beginning with") + " '";
	m_error += *p;
	m_error += "'  ascii: ";
	char buf[16];
	sprintf(buf, "%d", (int)*p);
	m_error += buf;

	return INVALID;
}


const char *lk::lexer::tokstr(int t)
{
	switch(t)
	{
	case END: return "<end>";
	case IDENTIFIER: return "<identifer>";
	case NUMBER: return "<number>";
	case LITERAL: return "<literal>";
	case SEP_SEMI: return ";";
	case SEP_COLON: return ":";
	case SEP_COMMA: return ",";
	case SEP_LPAREN: return "(";
	case SEP_RPAREN: return ")";
	case SEP_LCURLY: return "{";
	case SEP_RCURLY: return "}";
	case SEP_LBRACK: return "[";
	case SEP_RBRACK: return "]";
	case OP_PLUS: return "+";
	case OP_MINUS: return "-";
	case OP_MULT: return "*";
	case OP_DIV: return "/";
	case OP_EXP: return "^";
	case OP_DOT: return ".";
	case OP_QMARK: return "?";
	case OP_POUND: return "#";
	case OP_TILDE: return "~";
	case OP_PERCENT: return "%";
	case OP_AT: return "@";
	case OP_LOGIAND: return "&&";
	case OP_LOGIOR: return "||";
	case OP_BITAND: return "&";
	case OP_BITOR: return "|";
	case OP_BANG: return "!";
	case OP_ASSIGN: return "=";
	case OP_REF: return "->";
	case OP_PP: return "++";
	case OP_MM: return "--";
	case OP_LT: return "<";
	case OP_GT: return ">";
	case OP_EQ: return "==";
	case OP_NE: return "!=";
	case OP_LE: return "<=";
	case OP_GE: return ">=";
	case OP_PLUSEQ: return "+=";
	case OP_MINUSEQ: return "-=";
	case OP_MULTEQ: return "*=";
	case OP_DIVEQ: return "/=";
	default:
		return "<invalid>";
	}
}
