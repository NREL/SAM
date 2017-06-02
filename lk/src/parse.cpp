#include <cstdarg>
#include <cstdlib>
#include <cstring>

#include <lk/parse.h>

lk::parser::parser( input_base &input, const lk_string &name )
	: lex( input )
{
	m_haltFlag = false;
	m_lastLine = lex.line();
	m_lastStmt = 0;
	m_lastBlockEnd = 0;
	m_tokType = lex.next();
	m_name = name;
}


void lk::parser::add_search_paths( const std::vector<lk_string> &paths )
{
	for( size_t i=0;i<paths.size();i++ )
		m_searchPaths.push_back( paths[i] );
}

int lk::parser::token()
{
	return m_tokType;
}

bool lk::parser::token(int t)
{
	return (m_tokType==t);
}

lk_string lk::parser::error( int idx, int *line )
{
	if (idx >= 0 && idx < (int)m_errorList.size())
	{
		if ( line != 0 ) *line = m_errorList[idx].line;
		return m_errorList[idx].text;
	}
	return lk_string("");
}


bool lk::parser::match( const char *s )
{
	if ( m_tokType == lk::lexer::END )
	{
		error( lk_tr("reached end of input, but expected" ) + " '" + s + "'" );
		return false;
	}
	
	if ( lex.text() != s )
	{
		error( lk_tr("expected") + " '" + s + "' " + lk_tr("but found") + " '" + lex.text() );
		return false;
	}
	
	skip();
	return true;
}

bool lk::parser::match(int t)
{
	if (m_tokType == lk::lexer::END )
	{
		error( lk_tr("reached end of input, but expected token") + " '" + lk::lexer::tokstr(t) + "'");
		return false;
	}
	
	if ( m_tokType != t)
	{
		error( lk_tr("expected") 
			+ " '" + lk::lexer::tokstr(t) + "' " 
			+ lk_tr("but found") 
			+ " '" + lk::lexer::tokstr(m_tokType) + "' " 
			+ lex.text() );
		return false;
	}
	
	skip();
	return true;
}

lk::srcpos_t lk::parser::srcpos()
{
	return srcpos_t( m_name, line(), m_lastStmt );
}

void lk::parser::skip()
{	
	m_lastLine = lex.line(); // update code line to last successfully accepted token
	m_tokType = lex.next();
	
	if (m_tokType == lk::lexer::INVALID)
		error( lk_tr("invalid sequence in input:") + lex.error() );
}

void lk::parser::error( const char *fmt, ... )
{
	char buf[512];
	va_list list;
	va_start( list, fmt );
#ifdef _WIN32
	_vsnprintf( buf, 480, fmt, list );
#else
	vsnprintf( buf, 480, fmt, list );
#endif
	
	va_end( list );

	lk_string s(buf);
	error( s );
}

void lk::parser::error( const lk_string &s )
{
	char buf[512];
	
	if ( !m_name.empty() )
		sprintf(buf, "[%s] %d: ", (const char*)m_name.c_str(), m_lastLine );
	else
		sprintf(buf, "%d: ", m_lastLine);
	
	errinfo e;
	e.text = lk_string(buf) + s;
	e.line = m_lastLine;
	m_errorList.push_back( e );
}

lk::node_t *lk::parser::script()
{
	list_t *head = 0;
	node_t *stmt;

	srcpos_t startpos = srcpos();

	while ( !token(lk::lexer::END)
	   && !token(lk::lexer::INVALID)
		&& (stmt = statement()) )
	{
		if ( 0 == head ) head = new list_t( startpos );
		head->items.push_back( stmt );
	}

	return head;
}


lk::node_t *lk::parser::block()
{
	if ( token( lk::lexer::SEP_LCURLY ) )
	{
		match(lk::lexer::SEP_LCURLY);
		
		node_t *n = statement();
		
		if (!token( lk::lexer::SEP_RCURLY ))
		{
			list_t *head = new list_t( srcpos() );
			head->items.push_back( n );
						
			while ( token() != lk::lexer::END
				&& token() != lk::lexer::INVALID
				&& token() != lk::lexer::SEP_RCURLY
				&& !m_haltFlag )
			{
				head->items.push_back( statement() );
			}
			
			n = head;
			
		}
		
		m_lastBlockEnd = line();
		if (!match( lk::lexer::SEP_RCURLY ))
		{
			m_haltFlag = true;
			if (n) delete n;
			return 0;
		}
			
		return n;
	}
	else
		return statement();		
}

lk::node_t *lk::parser::statement()
{
	node_t *stmt = 0;

	// update the line number in the source position data
	// at the beginning of each logical statement in the code
	m_lastStmt = line();
	
	if (lex.text() == "function")
	{
		skip();
		// syntactic sugar for 'const my_function = define(...) {  };'
		// function my_function(...) {  }
		if ( token() != lexer::IDENTIFIER )
			error( lk_tr("function name missing") );
		
		srcpos_t pos = srcpos();

		lk_string name = lex.text();
		skip();

		match( lk::lexer::SEP_LPAREN );
		node_t *a = identifierlist( lk::lexer::SEP_COMMA, lk::lexer::SEP_RPAREN );
		match( lk::lexer::SEP_RPAREN );
		
		node_t *b = block();
		
		// save line position at end of function block as stmt_end
		// for debugging information in code generator when generating
		// implicit return statements at the end of a function
		pos.stmt_end = m_lastBlockEnd;
		expr_t *expr = new expr_t( pos, expr_t::DEFINE,	a, b );

		pos.stmt_end = pos.stmt;
		node_t *asgn = new expr_t( pos, expr_t::ASSIGN,	
			new iden_t( pos, name, true, false, false ),
			expr );

		return asgn;
	}
	else if (lex.text() == "if")
	{
		return test();
	}
	else if (lex.text() == "while" || lex.text() == "for")
	{
		return loop();
	}
	else if (token(lk::lexer::SEP_LCURLY))
	{
		return block();
	}
	else if (lex.text() == "return")
	{
		skip();
		lk::node_t *rval = 0;
		if ( token() != lk::lexer::SEP_SEMI )
			rval = ternary();
		stmt = new ctlstmt_t( srcpos(), ctlstmt_t::RETURN, rval );
	}
	else if (lex.text() == "exit")
	{
		stmt = new ctlstmt_t( srcpos(), ctlstmt_t::EXIT );
		skip();
	}
	else if (lex.text() == "break")
	{
		stmt = new ctlstmt_t( srcpos(), ctlstmt_t::BREAK );
		skip();
	}
	else if (lex.text() == "continue")
	{
		stmt = new ctlstmt_t( srcpos(), ctlstmt_t::CONTINUE );
		skip();
	}
	else if ( lex.text() == "import" )
	{
		skip();
		if ( token() != lk::lexer::LITERAL )
		{
			error( lk_tr("literal required after import statement") );
			skip();
			if ( token() == lk::lexer::SEP_SEMI )
				skip();

			return statement();
		}

		lk_string file = lex.text();
		skip();


		std::vector<lk_string> attempted_paths;
		attempted_paths.push_back( file );
		for( size_t i=0;i<m_searchPaths.size();i++ )
			attempted_paths.push_back( m_searchPaths[i] + "/" + file );

		lk_string expanded_path;
		bool import_found = false;
		lk_string src_text;


		for( size_t i=0;i<attempted_paths.size();i++ )
		{
			if (FILE *fp = fopen( (const char*)attempted_paths[i].c_str(), "r" ))
			{
				expanded_path = attempted_paths[i];
				import_found = true;
				char c;
				while ( (c=fgetc(fp))!=EOF )
					src_text += c;
				fclose(fp);
				break;
			}
		}
		
		for (size_t k=0;k<m_importNameList.size();k++)
		{
			if (m_importNameList[k] == expanded_path)
			{
				error( lk_tr("invalid circular import of: ") + file );
				m_haltFlag = true;
				return 0;
			}
		}

		if ( import_found )
		{
			m_importNameList.push_back( expanded_path );

			lk::input_string p( src_text );
			lk::parser parse( p, file );
			
			// pass on the imported names list to avoid circular imports
			parse.m_importNameList = m_importNameList;
			// pass on the search paths for nested imports
			parse.m_searchPaths = m_searchPaths;

			lk::node_t *tree = parse.script();

			if ( parse.error_count() != 0
				|| parse.token() != lk::lexer::END
				|| tree == 0 )
			{
				error( lk_tr("parse errors in import: " + file ));
				
				int i=0;
				while ( i < parse.error_count() )
					error( "\t%s", (const char*)parse.error(i++).c_str());
				
				if ( tree != 0 )
					delete tree;

				m_haltFlag = true;
				return 0;
			}
			else
				stmt = tree;
		}
		else
		{
			error( lk_tr("could not locate: ") + file ) ;
			return 0;
		}
	}
	else if (lex.text() == "enum")
	{
		stmt = enumerate();
	}
	else 	
		stmt = assignment();
	
	if ( stmt == 0 )
	{
		error( lk_tr("empty program statement encountered") );
		return 0;
	}

	// require semicolon at end of a statement
	if ( !match(lk::lexer::SEP_SEMI) )
	{
		if ( stmt != 0 ) delete stmt;
		m_haltFlag = true;
		return 0;
	}

	return stmt;
}

lk::node_t *lk::parser::enumerate()
{
	match("enum");
	match( lk::lexer::SEP_LCURLY );

	double cur_value = 0;
	list_t *head=0;
	srcpos_t startpos = srcpos();

	while ( !m_haltFlag && token(lk::lexer::IDENTIFIER) )
	{
		int line_num = line();
		lk_string name = lex.text();
		skip();

		if (token(lk::lexer::OP_ASSIGN))
		{
			skip();
			bool plus = false;
			if (token(lk::lexer::OP_PLUS))
			{
				plus = true;
				skip();
			}

			if (token(lk::lexer::NUMBER))
			{
				if (plus)
					cur_value += lex.value() - 1.0; // to adjust for +1 already added
				else if ( lex.value() > cur_value )
					cur_value = lex.value();
				else
					error( lk_tr("values in enumeration must increase") );

				skip();
			}
			else
				error( lk_tr("enumerate statements can only contain numeric assignments") );

		}

		if (!head) head = new list_t( startpos );

		head->items.push_back( new expr_t( srcpos(), expr_t::ASSIGN,
							new iden_t( srcpos_t( m_name, line_num, m_lastStmt ), name, true, false, false ),
							new constant_t( srcpos(), cur_value  ) ) );

		cur_value += 1.0;

		if ( token() != lk::lexer::SEP_RCURLY )
			if ( !match( lk::lexer::SEP_COMMA ) )
				m_haltFlag = true;
	}

	if ( !head ) error( lk_tr("enumeration must have one or more identifiers") );

	match( lk::lexer::SEP_RCURLY );

	return head;
}

lk::node_t *lk::parser::test()
{
	srcpos_t pos = srcpos();

	match("if");
	match( lk::lexer::SEP_LPAREN );
	node_t *test = logicalor();
	match( lk::lexer::SEP_RPAREN );
	node_t *on_true = block();

	cond_t *c_top = new cond_t( pos, test, on_true, 0, false );

	if ( lex.text() == "else" )
	{
		// update statement line since 'else' is like a statement
		m_lastStmt = line();
		skip();
		c_top->on_false = block();
	}
	else if ( lex.text() == "elseif" )
	{
		cond_t *tail = c_top;

		while( lex.text() == "elseif" )
		{
			// update statement line since 'elseif' is like a statement
			m_lastStmt = line();
			pos = srcpos();

			skip();
			match( lk::lexer::SEP_LPAREN );
			test = logicalor();
			match( lk::lexer::SEP_RPAREN );
			on_true = block();

			cond_t *link = new cond_t( pos, test, on_true, 0, false );
			tail->on_false = link;
			tail = link;
		}

		if ( lex.text() == "else" )
		{
			m_lastStmt = line();

			skip();
			tail->on_false = block();
		}
	}

	return c_top;
}

lk::node_t *lk::parser::loop()
{
	iter_t *it = 0;
	
	if ( lex.text() == "while" )
	{
		it = new iter_t( srcpos(), 0, 0, 0, 0 );
		skip();
		match( lk::lexer::SEP_LPAREN );
		it->test = logicalor();
		match( lk::lexer::SEP_RPAREN );

		it->block = block();
	}
	else if ( lex.text() == "for" )
	{
		it = new iter_t( srcpos(), 0, 0, 0, 0 );
		skip();

		match( lk::lexer::SEP_LPAREN );
		
		if ( !token( lk::lexer::SEP_SEMI ) )
			it->init = assignment();
		
		match( lk::lexer::SEP_SEMI );

		if ( !token( lk::lexer::SEP_SEMI ) )
			it->test = logicalor();

		match( lk::lexer::SEP_SEMI );

		if ( !token( lk::lexer::SEP_RPAREN ) )
			it->adv = assignment();

		match( lk::lexer::SEP_RPAREN );

		it->block = block();
	}
	else
	{
		error( lk_tr("invalid looping construct") );
		m_haltFlag = true;
	}

	return it;
}

lk::node_t *lk::parser::define()
{
	if (m_haltFlag) return 0;
	
	srcpos_t pos = srcpos();
	match("define");
	match( lk::lexer::SEP_LPAREN );
	node_t *a = identifierlist( lk::lexer::SEP_COMMA, lk::lexer::SEP_RPAREN );
	match( lk::lexer::SEP_RPAREN );
	node_t *b = block();

	pos.stmt_end = m_lastBlockEnd; // take note of where the end of the block is for implicit return statements in debugging
	
	return new expr_t( pos, expr_t::DEFINE, a, b );
}

lk::node_t *lk::parser::assignment()
{
	if (m_haltFlag) return 0;
	
	node_t *n = ternary();
	
	if ( token(lk::lexer::OP_ASSIGN) )
	{
		skip();		
		n = new expr_t(srcpos(), expr_t::ASSIGN, n, assignment() );
	}
	else if ( token(lk::lexer::OP_PLUSEQ) )
	{
		skip();
		n = new expr_t(srcpos(), expr_t::PLUSEQ, n, ternary() );
	}
	else if ( token(lk::lexer::OP_MINUSEQ) )
	{
		skip();
		n = new expr_t(srcpos(), expr_t::MINUSEQ, n, ternary() );
	}
	else if ( token(lk::lexer::OP_MULTEQ) )
	{
		skip();
		n = new expr_t(srcpos(), expr_t::MULTEQ, n, ternary() );
	}
	else if ( token(lk::lexer::OP_DIVEQ) )
	{
		skip();
		n = new expr_t(srcpos(), expr_t::DIVEQ, n, ternary() );
	}
	else if ( token(lk::lexer::OP_MINUSAT) )
	{
		skip();
		n = new expr_t(srcpos(), expr_t::MINUSAT, n, ternary() );
	}

	return n;
}

lk::node_t *lk::parser::ternary()
{
	if (m_haltFlag) return 0;
	
	node_t *test = logicalor();
	if ( token(lk::lexer::OP_QMARK) )
	{
		skip();
		node_t *rtrue = ternary();
		match( lk::lexer::SEP_COLON );
		node_t *rfalse = ternary();
		
		return new lk::cond_t(srcpos(), test, rtrue, rfalse, true);
	}
	else
		return test;
}

lk::node_t *lk::parser::logicalor()
{
	if (m_haltFlag) return 0;
	
	node_t *n = logicaland();
	while ( token(lk::lexer::OP_LOGIOR) )
	{
		skip();		
		node_t *left = n;
		node_t *right = logicaland();
		n = new lk::expr_t( srcpos(), expr_t::LOGIOR, left, right );
	}
	
	return n;
}

lk::node_t *lk::parser::logicaland()
{
	if (m_haltFlag) return 0;
	
	node_t *n = equality();
	while ( token(lk::lexer::OP_LOGIAND) )
	{
		skip();		
		node_t *left = n;
		node_t *right = equality();
		n = new lk::expr_t( srcpos(), expr_t::LOGIAND, left, right );
	}
	
	return n;
}

lk::node_t *lk::parser::equality()
{
	if (m_haltFlag) return 0;
	
	node_t *n = relational();
	
	while ( token( lk::lexer::OP_EQ )
		|| token( lk::lexer::OP_NE ) )
	{
		int oper = token(lk::lexer::OP_EQ) ? expr_t::EQ : expr_t::NE;
		skip();
		
		node_t *left = n;
		node_t *right = relational();
		
		n = new lk::expr_t( srcpos(), oper, left, right );		
	}
	
	return n;
}

lk::node_t *lk::parser::relational()
{
	if (m_haltFlag) return 0;
	
	node_t *n = additive();
	
	while ( token( lk::lexer::OP_LT )
	  || token( lk::lexer::OP_LE )
	  || token( lk::lexer::OP_GT )
	  || token( lk::lexer::OP_GE ) )
	{
		int oper = expr_t::INVALID;
		
		switch( token() )
		{
		case lk::lexer::OP_LT: oper = expr_t::LT; break;
		case lk::lexer::OP_LE: oper = expr_t::LE; break;
		case lk::lexer::OP_GT: oper = expr_t::GT; break;
		case lk::lexer::OP_GE: oper = expr_t::GE; break;
		default:
			error( lk_tr("invalid relational operator: ") + lk::lexer::tokstr( token() ) );
		}
		
		skip();
		
		node_t *left = n;
		node_t *right = additive();
		
		n = new lk::expr_t( srcpos(), oper, left, right );		
	}
	
	return n;	
}



lk::node_t *lk::parser::additive()
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

lk::node_t *lk::parser::multiplicative()
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

lk::node_t *lk::parser::exponential()
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

lk::node_t *lk::parser::unary()
{
	if (m_haltFlag) return 0;

	switch( token() )
	{
	case lk::lexer::OP_BANG:
		skip();
		return new lk::expr_t( srcpos(), expr_t::NOT, unary(), 0 );
	case lk::lexer::OP_MINUS:
	{
		skip();
		// little optimization here: if unary() expression is a constant number, 
		// just negate it right here and return that rather than a new expression
		lk::node_t *un = unary();
		if ( lk::constant_t *cnst = dynamic_cast<lk::constant_t*>( un ) )
		{
			cnst->value = 0 - cnst->value;
			return cnst;
		}
		else return new lk::expr_t( srcpos(), expr_t::NEG, un, 0 );
	}
	case lk::lexer::OP_POUND:
		skip();
		return new lk::expr_t( srcpos(), expr_t::SIZEOF, unary(), 0 );
	case lk::lexer::OP_AT:
		skip();
		return new lk::expr_t( srcpos(), expr_t::KEYSOF, unary(), 0 );
	case lk::lexer::IDENTIFIER:
		if (lex.text() == "typeof")
		{
			skip();
			match( lk::lexer::SEP_LPAREN );
			node_t *id = 0;
			if ( token() == lk::lexer::IDENTIFIER )
			{
				id = new iden_t( srcpos(), lex.text(), false, false, false );
				skip();
			}
			else
			{
				error( lk_tr("expected identifier in typeof(...) expression") );
				return 0;
			}
			match( lk::lexer::SEP_RPAREN );
			return new lk::expr_t( srcpos(), expr_t::TYPEOF, id, 0 );
		}
	default:
		return postfix();
	}		
}

lk::node_t *lk::parser::postfix()
{
	if (m_haltFlag) return 0;
	
	node_t *left = primary();
	
	while ( 1 )
	{
		if ( token( lk::lexer::SEP_LBRACK ) )
		{
			skip();			
			node_t *right = ternary();
			match( lk::lexer::SEP_RBRACK );
			left = new expr_t( srcpos(), expr_t::INDEX, left, right );
		}
		else if ( token( lk::lexer::SEP_LPAREN ) )
		{
			skip();
			node_t *right = ternarylist(lk::lexer::SEP_COMMA, lk::lexer::SEP_RPAREN);
			match( lk::lexer::SEP_RPAREN );
			left = new expr_t( srcpos(), expr_t::CALL, left, right );
		}
		else if ( token( lk::lexer::SEP_LCURLY ) )
		{
			skip();
			node_t *right = ternary();
			match( lk::lexer::SEP_RCURLY );
			left = new expr_t( srcpos(), expr_t::HASH, left, right );
		}
		else if ( token( lk::lexer::OP_DOT ) )
		{
			// x.value is syntactic sugar for x{"value"}
			skip();			
			left = new expr_t( srcpos(), expr_t::HASH, left, new literal_t( srcpos(), lex.text() ) );
			skip();
		}
		else if ( token (lk::lexer::OP_REF ) )
		{
/*
			pure syntactic translation for:

			obj->append(x) is syntactic sugar for 
					obj.append(obj, x)
			and     obj{"append"}( obj, x )

			basic class definition of a pair with a method:
		
			pair = define(f,s) {
			  obj.first = f;
			  obj.second = s;
			  obj.sum = define(this, balance) {
				 return this.first + this.second + balance;
			  };
			  return obj;
			};
		
			usage:
	
			arr[0] = pair(1,2);
			arr[1] = pair(3,4);

			echo( arr[0]->sum(3) );
			echo( arr[1]->sum(5) );
*/
	
			skip();
			
			if ( !token( lk::lexer::IDENTIFIER ) )
			{
				error( lk_tr("expected method-function name after dereference operator ->") );
				skip();
			}			

			lk_string method_iden = lex.text();			
			skip();

			match( lk::lexer::SEP_LPAREN );
			list_t *arg_list = ternarylist( lk::lexer::SEP_COMMA, lk::lexer::SEP_RPAREN );
			match( lk::lexer::SEP_RPAREN );
			

			// at execution, the THISCALL mode
			// will cause a reference to the result of
			// the left hand primary expression to be passed
			// as the first argument in the list
			left = new expr_t( srcpos(), expr_t::THISCALL,
						new expr_t( srcpos(), expr_t::HASH, 
							left,  // primary expression on left of ->  (i.e. 'obj')
							new literal_t( srcpos(), method_iden )),
						arg_list );

		}
		else if ( token( lk::lexer::OP_PP ) )
		{
			left = new expr_t( srcpos(), expr_t::INCR, left, 0 );
			skip();
		}
		else if ( token( lk::lexer::OP_MM ) )
		{
			left = new expr_t( srcpos(), expr_t::DECR, left, 0 );
			skip();
		}
		else if ( token( lk::lexer::OP_QMARKAT ) )
		{
			skip();
			left = new expr_t( srcpos(), expr_t::WHEREAT, left, ternary() );
		}
		else
			break;
	}
	
	return left;
}

lk::node_t *lk::parser::primary()
{
	if (m_haltFlag) return 0;
	
	node_t *n = 0;
	switch( token() )
	{
	case lk::lexer::SEP_LPAREN:
		skip();
		n = ternary();
		match(lk::lexer::SEP_RPAREN);
		return n;
	case lk::lexer::SEP_LBRACK:
		skip();
		n = ternarylist(lk::lexer::SEP_COMMA, lk::lexer::SEP_RBRACK);
		match( lk::lexer::SEP_RBRACK );
		return new lk::expr_t( srcpos(), lk::expr_t::INITVEC, n, 0 );
	case lk::lexer::SEP_LCURLY:
		{			
			srcpos_t startpos = srcpos();
			skip();
			list_t *head = 0;
			while ( token() != lk::lexer::INVALID
				&& token() != lk::lexer::END
				&& token() != lk::lexer::SEP_RCURLY
				&& !m_haltFlag )
			{
				if ( !head ) head = new list_t( startpos );

				if( token() != lk::lexer::IDENTIFIER && token() != lk::lexer::LITERAL )
				{
					error( lk_tr("invalid key syntax in table initializer") );
					m_haltFlag = true;
					return 0;
				}

				lk_string key( lex.text() );
				skip();
				if ( token() == lk::lexer::OP_ASSIGN || token() == lk::lexer::SEP_COLON )
				{
					skip();
				}
				else
				{
					error( lk_tr( "expected = or : in table initializer syntax" ) );
					m_haltFlag = true;
					return 0;
				}

				head->items.push_back( new lk::expr_t( srcpos(), lk::expr_t::ASSIGN, 
					new lk::literal_t( srcpos(), key ),
					ternary() ) );

				if ( token() != lk::lexer::SEP_RCURLY )
					if (!match( lk::lexer::SEP_COMMA ))
						m_haltFlag = true;
			}
			match(lk::lexer::SEP_RCURLY );
			return new lk::expr_t( srcpos(), lk::expr_t::INITHASH, head, 0 );
		}		
	case lk::lexer::OP_QMARK:
	{
		// inline integer expression switch: ?? opt [ 1, 2, 3, 4, 5, 6 ]
		skip();
		node_t *value = primary();
		match( lk::lexer::SEP_LBRACK );
		list_t *list = ternarylist( lk::lexer::SEP_COMMA, lk::lexer::SEP_RBRACK );
		match( lk::lexer::SEP_RBRACK );
		return new lk::expr_t( srcpos(), lk::expr_t::SWITCH, value, list );
	}
	case lk::lexer::NUMBER:
		n = new lk::constant_t( srcpos(), lex.value() );
		skip();
		return n;
	case lk::lexer::LITERAL:
		n = new lk::literal_t( srcpos(), lex.text() );
		skip();
		return n; 
	case lk::lexer::SPECIAL: // special identifiers like ${ab.fkn_34}
		n = new lk::iden_t( srcpos(), lex.text(), false, false, true );
		skip();
		return n;
	case lk::lexer::IDENTIFIER:
		if (lex.text() == "define")
		{
			n = define();
		}
		else if (lex.text() == "true")
		{
			n = new lk::constant_t( srcpos(), 1.0 );
			skip();
		}
		else if (lex.text() == "false")
		{
			n = new lk::constant_t( srcpos(), 0.0 );
			skip();
		}
		else if (lex.text() == "null")
		{
			n = new lk::null_t( srcpos() );
			skip();
		}
		else
		{
			bool constval = false;
			bool globalval = false;
			
			if ( lex.text() == "const" )
			{
				constval = true;
				skip();
			} 
			else if ( lex.text() == "global" )
			{
				globalval = true;
				skip();
			}

			// can't have both const global - not needed in practice.
			
			n = new lk::iden_t( srcpos(), lex.text(), constval, globalval, false );
			match(lk::lexer::IDENTIFIER);
		}
		return n;
	default:
		error( lk_tr("invalid expression beginning with") + " '" + lk::lexer::tokstr(token()) + "'" );
		m_haltFlag = true;
		return 0;
	}
}

lk::list_t *lk::parser::ternarylist( int septok, int endtok)
{
	srcpos_t startpos = srcpos();
	list_t *head=0;
	while ( token() != lk::lexer::INVALID
		&& token() != lk::lexer::END
		&& token() != endtok 
		&& !m_haltFlag )
	{
		if ( !head ) head = new list_t( startpos );
		head->items.push_back( ternary() );
		if ( token() != endtok )
			if (!match( septok ))
				m_haltFlag = true;
	}
	
	return head;
}


lk::list_t *lk::parser::identifierlist( int septok, int endtok)
{
	srcpos_t startpos = srcpos();
	list_t *head=0;
		
	while ( !m_haltFlag && token(lk::lexer::IDENTIFIER) )
	{
		if ( !head ) head = new list_t( startpos );
		head->items.push_back( new iden_t( srcpos(), lex.text(), false, false, false ) );
		skip();

		if ( token() != endtok )
			if (!match( septok ))
				m_haltFlag = true;
	}
	
	return head;
}
