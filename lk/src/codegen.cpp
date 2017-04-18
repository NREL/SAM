#include <limits>
#include <numeric> 

#include <lk/stdlib.h>
#include <lk/codegen.h>

namespace lk {

bool codegen::error( const lk_string &s )
{
	m_errStr = s;
	return false;
}

bool codegen::error( const char *fmt, ... )
{
	char buf[512];
	va_list args;
	va_start( args, fmt );
	vsprintf( buf, fmt, args );
	va_end( args );
	m_errStr = buf;	
	return false;
}

// context flags for pfgen()
#define F_NONE 0x00
#define F_MUTABLE 0x01

codegen::codegen() {
	m_labelCounter = 1;
}
	
size_t codegen::get( bytecode &bc )
{
	if ( m_asm.size() == 0 ) return 0;

	bc.program.resize( m_asm.size(), 0 );
	bc.debuginfo.resize( m_asm.size(), srcpos_t() );

	for( size_t i=0;i<m_asm.size();i++ )
	{
		instr &ip = m_asm[i];
		if ( ip.label ) m_asm[i].arg = m_labelAddr[ *ip.label ];
		bc.program[i] = (((unsigned int)ip.op)&0x000000FF) | (((unsigned int)ip.arg)<<8);
		bc.debuginfo[i] = m_asm[i].pos;
	}

	bc.constants = m_constData;
	bc.identifiers = m_idList;

	return m_asm.size();
}


void codegen::textout( lk_string &assembly, lk_string &bytecode )
{
	char buf[128];		
		
	for( size_t i=0;i<m_asm.size();i++ )
	{
		instr &ip = m_asm[i];

		if ( ip.label )
			m_asm[i].arg = m_labelAddr[ *ip.label ];

		bool has_label = false;
		// determine if there's a label for this line (not super efficient)
		for( LabelMap::iterator it = m_labelAddr.begin();
			it != m_labelAddr.end();
			++it )
			if ( (int)i == it->second )
			{
				sprintf(buf, "%4s:", (const char*)it->first.c_str() );
				assembly += buf;
				has_label = true;
			}

		if ( !has_label )
			assembly += "     ";

			
		size_t j=0;
		while( op_table[j].name != 0 )
		{
			if ( ip.op == op_table[j].op )
			{
				sprintf( buf, "%4d{%4d} %4s ", ip.pos.line, ip.pos.stmt, op_table[j].name );
				assembly += buf;

				if ( ip.label )
				{
					assembly += (*ip.label);
				}
				else if ( ip.op == PSH )
				{
					static const size_t MAXWIDTH = 24;
					lk_string nnl( m_constData[ip.arg].as_string() );
					if ( nnl.size() > MAXWIDTH )
					{
						nnl = nnl.substr( 0, MAXWIDTH );
						nnl += "...";
					}					
					lk::replace( nnl, "\n", "" );
					assembly += nnl ;
				}
				else if ( ip.op == SET || ip.op == GET || ip.op == RREF 
					|| ip.op == LREF || ip.op == LCREF || ip.op == LGREF || ip.op == ARG )
				{
					assembly += m_idList[ip.arg];
				}
				else if ( ip.op == TCALL || ip.op == CALL || ip.op == VEC || ip.op == HASH || ip.op == SWI )
				{
					sprintf(buf, "(%d)", ip.arg );
					assembly += buf;
				}

				assembly += '\n';

				unsigned int bc = (((unsigned int)ip.op)&0x000000FF) | (((unsigned int)ip.arg)<<8);
				sprintf(buf, "0x%08X\n", bc);
				bytecode += buf;
				break;
			}
			j++;
		}
	}
		
	for( size_t i=0;i<m_constData.size();i++ )
		bytecode += ".data " + m_constData[i].as_string() + "\n";

	for( size_t i=0;i<m_idList.size();i++ )
		bytecode += ".id " + m_idList[i] + "\n";
}

bool codegen::generate( lk::node_t *root )
{
	m_idList.clear();
	m_constData.clear();
	m_asm.clear();
	m_labelAddr.clear();
	m_labelCounter = 0;
	m_breakAddr.clear();
	m_continueAddr.clear();

	return pfgen(root, F_NONE );
}

int codegen::place_identifier( const lk_string &id )
{
	for( size_t i=0;i<m_idList.size();i++ )
		if ( m_idList[i] == id )
			return (int)i;

	m_idList.push_back( id );
	return m_idList.size() - 1;
}

int codegen::place_const( vardata_t &d )
{
	if ( d.type() == vardata_t::HASH && d.hash()->size() == 2 )
		printf("stop here" );

	for( size_t i=0;i<m_constData.size();i++ )
		if ( m_constData[i].equals( d ) )
			return (int)i;

	m_constData.push_back( d );
	return (int)m_constData.size()-1;
}

int codegen::const_value( double value )
{
	vardata_t x;
	x.assign( value );
	return place_const( x );
}
int codegen::const_literal( const lk_string &lit )
{
	vardata_t x;
	x.assign( lit );
	return place_const( x );
}

lk_string codegen::new_label()
{
	char buf[128];
	sprintf(buf, "L%d", m_labelCounter++);
	return lk_string(buf);
}

void codegen::place_label( const lk_string &s )
{
	m_labelAddr[ s ] = (int)m_asm.size();
}

int codegen::emit( srcpos_t pos, Opcode o, int arg )
{
	// copy previous line's position if parser doesn't know the statement line,
	// such as when generating if-elseif-else structures for the 'J' instruction
	// or for implicit function returns
	if ( pos == srcpos_t::npos && m_asm.size() > 0 )
		pos = m_asm.back().pos; 

	m_asm.push_back( instr( pos, o, arg ) );
	return m_asm.size();
}

int codegen::emit( srcpos_t pos, Opcode o, const lk_string &L )
{
	// copy previous line's position if parser doesn't know the statement line,
	// such as when generating if-elseif-else structures for the 'J' instruction
	// or for implicit function returns
	if ( pos == srcpos_t::npos && m_asm.size() > 0 )
		pos = m_asm.back().pos; 

	m_asm.push_back( instr(pos, o, 0, (const char*) L.c_str()) );
	return m_asm.size();
}

bool codegen::initialize_const_vec( lk::list_t *v, vardata_t &vvec )
{
	if ( !v ) return true; // empty vector

	for( std::vector<node_t*>::iterator it = v->items.begin();
		it != v->items.end();
		++it )
	{
		if ( lk::constant_t *cc = dynamic_cast<constant_t*>( *it ) )				
			vvec.vec_append( cc->value );
		else if ( lk::literal_t *cc = dynamic_cast<literal_t*>( *it ) )
			vvec.vec_append( cc->value );
		else if ( lk::expr_t *expr = dynamic_cast<expr_t*>( *it ) )
		{
			if ( expr->oper == expr_t::INITVEC )
			{
				lk::vardata_t subvec;
				subvec.empty_vector();
				if ( !initialize_const_vec( dynamic_cast<list_t*>(expr->left), subvec ) )
					return false;
				vvec.vec()->push_back( subvec );
			}
			else if ( expr->oper == expr_t::INITHASH )
			{
				lk::vardata_t subhash;
				subhash.empty_hash();
				if ( !initialize_const_hash( dynamic_cast<list_t*>(expr->left), subhash ) )
					return false;
				vvec.vec()->push_back( subhash );
			}
			else
				return false;
		}
		else
			return false;
	}

	return true;
}

bool codegen::initialize_const_hash( lk::list_t *v, vardata_t &vhash )
{
	if ( !v ) return true;

	for( std::vector<node_t*>::iterator it = v->items.begin();
		it != v->items.end();
		++it )
	{
		expr_t *assign = dynamic_cast<expr_t*>( *it );

		if (assign && assign->oper == expr_t::ASSIGN)
		{
			lk_string key;
			vardata_t val;
			if ( lk::literal_t *pkey = dynamic_cast<literal_t*>(assign->left) ) key = pkey->value;
			else return false;
				
			if ( lk::constant_t *cc = dynamic_cast<constant_t*>(assign->right) )				
				val.assign( cc->value );
			else if ( lk::literal_t *cc = dynamic_cast<literal_t*>(assign->right) )
				val.assign( cc->value );
			else if ( lk::expr_t *expr = dynamic_cast<expr_t*>(assign->right) )
			{
				if ( expr->oper == expr_t::INITVEC )
				{
					val.empty_vector();
					if ( !initialize_const_vec( dynamic_cast<list_t*>(expr->left), val ) )
						return false;
				}
				else if ( expr->oper == expr_t::INITHASH )
				{
					val.empty_hash();
					if ( !initialize_const_hash( dynamic_cast<list_t*>(expr->left), val ) )
						return false;
				}
				else
					return false;
			}
			else
				return false;

			vhash.hash_item(key).copy( val );
		}
		else
			return false;
	}

	return true;
}

bool codegen::pfgen_stmt( lk::node_t *root, unsigned int flags )
{
	bool ok = pfgen( root, flags );

	// expressions always leave their value on the stack, so clean it up
	if (expr_t *e = dynamic_cast<expr_t*>(root)) 
	{
		emit( e->srcpos(), POP );
	}
	else if ( cond_t *c = dynamic_cast<cond_t*>(root))
	{
		// inline ternary expressions also leave value on stack
		if ( c->ternary )
			emit( c->srcpos(), POP );
	}
	return ok;
}

bool codegen::pfgen( lk::node_t *root, unsigned int flags )
{
	if ( !root ) return true;

	if ( list_t *n = dynamic_cast<list_t*>( root ) )
	{
		for( std::vector<node_t*>::iterator it = n->items.begin();
			it != n->items.end();
			++it )
			if ( !pfgen_stmt( *it, flags ) )
				return false;
	}
	else if ( iter_t *n = dynamic_cast<iter_t*>( root ) )
	{
		if ( n->init && !pfgen_stmt( n->init, flags ) ) return false;

		// labels for beginning, advancement, and outside end of loop
		lk_string Lb = new_label();
		lk_string Lc = new_label();
		lk_string Le = new_label();
			
		m_continueAddr.push_back( Lc );
		m_breakAddr.push_back( Le );

		place_label( Lb ) ;

		if ( !pfgen( n->test, flags ) ) return false;

		emit( n->srcpos(), JF, Le );
			
		pfgen_stmt( n->block, flags );

		place_label( Lc );
		if ( n->adv && !pfgen_stmt( n->adv, flags ) ) return false;

		emit( n->srcpos(), J, Lb );
		place_label( Le );

		m_continueAddr.pop_back();
		m_breakAddr.pop_back();
	}
	else if ( cond_t *n = dynamic_cast<cond_t*>( root ) )
	{	
		bool ternary = n->ternary;

		lk_string L1 = new_label();
		lk_string L2 = L1;

		pfgen( n->test, flags );
		emit( n->srcpos(), JF, L1 );
		
		// if an inline ternary conditional expression,
		// don't pop the expression value off the stack as in 
		// an expression
		if ( ternary ) pfgen( n->on_true, false );
		else pfgen_stmt( n->on_true, flags );

		if ( n->on_false )
		{
			L2 = new_label();

			// use previous assembly output line as statement position for debugging
			// since it's unknown at parse time
			emit( srcpos_t::npos, J, L2 );
			place_label( L1 );

			if ( ternary ) pfgen( n->on_true, false );
			else pfgen_stmt( n->on_false, flags );
		}
		place_label( L2 );
	}
	else if ( expr_t *n = dynamic_cast<expr_t*>( root ) )
	{
		switch( n->oper )
		{
		case expr_t::PLUS:
			pfgen( n->left, flags );
			pfgen( n->right, flags );
			emit(  n->srcpos(), ADD );
			break;
		case expr_t::MINUS:
			pfgen( n->left, flags );
			pfgen( n->right, flags );
			emit( n->srcpos(), SUB );
			break;
		case expr_t::MULT:
			pfgen( n->left, flags );
			pfgen( n->right, flags );
			emit( n->srcpos(), MUL );
			break;
		case expr_t::DIV:
			pfgen( n->left, flags );
			pfgen( n->right, flags );
			emit( n->srcpos(), DIV );
			break;
		case expr_t::LT:
			pfgen( n->left, flags );
			pfgen( n->right, flags );
			emit( n->srcpos(), LT );
			break;
		case expr_t::GT:
			pfgen( n->left, flags );
			pfgen( n->right, flags );
			emit( n->srcpos(), GT );
			break;
		case expr_t::LE:
			pfgen( n->left, flags );
			pfgen( n->right, flags );
			emit( n->srcpos(), LE );
			break;
		case expr_t::GE:
			pfgen( n->left, flags );
			pfgen( n->right, flags );
			emit( n->srcpos(), GE );
			break;
		case expr_t::NE:
			pfgen( n->left, flags );
			pfgen( n->right, flags );
			emit( n->srcpos(), NE );
			break;
		case expr_t::EQ:
			pfgen( n->left, flags );
			pfgen( n->right, flags );
			emit( n->srcpos(), EQ );
			break;
		case expr_t::INCR:
			pfgen( n->left, flags|F_MUTABLE );
			emit( n->srcpos(), INC );
			break;
		case expr_t::DECR:
			pfgen( n->left, flags|F_MUTABLE );
			emit( n->srcpos(), DEC );
			break;
		case expr_t::LOGIOR:
		{
			lk_string Lsc = new_label();
			pfgen(n->left, flags );
			emit( n->srcpos(), DUP );
			emit( n->srcpos(), JT, Lsc );
			pfgen(n->right, flags);
			emit( n->srcpos(), OR );
			place_label( Lsc );
		}
			break;
		case expr_t::LOGIAND:
		{
			lk_string Lsc = new_label();
			pfgen(n->left, flags );
			emit( n->srcpos(), DUP );
			emit( n->srcpos(), JF, Lsc );
			pfgen(n->right, flags );
			emit( n->srcpos(), AND );
			place_label( Lsc );
		}
			break;
		case expr_t::NOT:
			pfgen(n->left, flags);
			emit( n->srcpos(), NOT );
			break;
		case expr_t::NEG:
			pfgen(n->left, flags);
			emit( n->srcpos(), NEG );
			break;				
		case expr_t::EXP:
			pfgen(n->left, flags);
			pfgen(n->right, flags);
			emit( n->srcpos(), EXP );
			break;
		case expr_t::INDEX:
			pfgen(n->left, flags );
			pfgen(n->right, F_NONE);
			emit( n->srcpos(), IDX, flags&F_MUTABLE );
			break;
		case expr_t::HASH:
			pfgen(n->left, flags);
			pfgen(n->right, F_NONE);
			emit( n->srcpos(), KEY, flags&F_MUTABLE );
			break;
		case expr_t::MINUSAT:
			pfgen(n->left, F_NONE );
			pfgen(n->right, flags);
			emit( n->srcpos(), MAT );
			break;
		case expr_t::WHEREAT:
			pfgen(n->left, F_NONE );
			pfgen(n->right, flags);
			emit( n->srcpos(), WAT );
			break;
		case expr_t::PLUSEQ:
			pfgen(n->left, F_NONE);
			pfgen(n->right, F_NONE);
			emit( n->srcpos(), ADD );
			pfgen(n->left, F_MUTABLE );
			emit( n->srcpos(), WR );
			break;
		case expr_t::MINUSEQ:
			pfgen(n->left, F_NONE);
			pfgen(n->right, F_NONE);
			emit( n->srcpos(), SUB );
			pfgen(n->left, F_MUTABLE );
			emit( n->srcpos(), WR );
			break;
		case expr_t::MULTEQ:
			pfgen(n->left, F_NONE);
			pfgen(n->right, F_NONE);
			emit( n->srcpos(), MUL );
			pfgen(n->left, F_MUTABLE );
			emit( n->srcpos(), WR );
			break;
		case expr_t::DIVEQ:
			pfgen(n->left, F_NONE);
			pfgen(n->right, F_NONE);
			emit( n->srcpos(), DIV );
			pfgen(n->left, F_MUTABLE );
			emit( n->srcpos(), WR );
			break;
		case expr_t::ASSIGN:
			{
				if ( !pfgen( n->right, flags ) ) return false;

				// if on the LHS of the assignment we have a special variable i.e. ${xy}, use a 
				// hack to assign the value to the storage location
				if ( lk::iden_t *iden = dynamic_cast<lk::iden_t*>(n->left) )
				{
					if ( iden->special )
					{
						emit( n->srcpos(), SET, place_identifier(iden->name) );
						return true;
					}
				}

				if ( !pfgen( n->left, F_MUTABLE ) ) return false;
				emit( n->srcpos(), WR );
			}
			break;
		case expr_t::CALL:
		case expr_t::THISCALL:
		{
			// make space on stack for the return value
			emit( n->srcpos(), NUL );

			// evaluate all the arguments and pushon to stack
			list_t *argvals = dynamic_cast<list_t*>(n->right);
			int nargs = 0;
			if( argvals )
			{			
				for( std::vector<node_t*>::iterator it = argvals->items.begin();
					it != argvals->items.end();
					++it )
				{
					pfgen( *it, F_NONE );
					nargs++;
				}
			}
			expr_t *lexpr = dynamic_cast<expr_t*>(n->left);
			if ( n->oper == expr_t::THISCALL && 0 != lexpr )
			{

				pfgen( lexpr->left, F_NONE );
				emit( n->srcpos(), DUP );
				pfgen( lexpr->right, F_NONE );
				emit( n->srcpos(), KEY );
			}
			else
				pfgen( n->left, F_NONE );

			emit( n->srcpos(),  (n->oper == expr_t::THISCALL)? TCALL : CALL, nargs );
		}
			break;
		case expr_t::SIZEOF:
			pfgen( n->left, F_NONE );
			emit( n->srcpos(), SZ );
			break;
		case expr_t::KEYSOF:
			pfgen( n->left, F_NONE );
			emit( n->srcpos(), KEYS );
			break;
		case expr_t::TYPEOF:
			if ( iden_t *iden = dynamic_cast<iden_t*>( n->left ) )
				emit( n->srcpos(), TYP, place_identifier( iden->name ) );
			else
				return error( lk_tr("invalid 'typeof' expression, identifier required") );
			break;
		case expr_t::INITVEC:
		{
			list_t *p = dynamic_cast<list_t*>( n->left );
			vardata_t cvec;
			cvec.empty_vector();
			if ( p && initialize_const_vec( p, cvec ) )
			{
				emit( n->srcpos(), PSH, place_const( cvec ) );
			}
			else
			{
				int len = 0;
				if ( p )
				{
					for( std::vector<node_t*>::iterator it = p->items.begin();
						it != p->items.end();
						++it )
					{
						pfgen( *it, F_NONE );
						len++;
					}
				}
				emit( n->srcpos(), VEC, len );
			}
		}
			break;
		case expr_t::INITHASH:
		{
			list_t *p = dynamic_cast<list_t*>( n->left );
			vardata_t chash;
			chash.empty_hash();
			if ( p && initialize_const_hash( p, chash ) )
			{
				emit( n->srcpos(), PSH, place_const( chash ) );
			}
			else
			{
				int len = 0;
				if ( p )
				{	
					for( std::vector<node_t*>::iterator it = p->items.begin();
						it != p->items.end();
						++it )
					{
						expr_t *assign = dynamic_cast<expr_t*>( *it );
						if (assign && assign->oper == expr_t::ASSIGN)
						{
							pfgen( assign->left, F_NONE );
							pfgen( assign->right, F_NONE );
							len++;
						}
					}
				}

				emit( n->srcpos(), HASH, len );
			}
		}
			break;
		case expr_t::SWITCH:
		{
			lk_string Le( new_label() );
			std::vector<lk_string> labels;
			
			list_t *p = dynamic_cast<list_t*>( n->right );

			pfgen( n->left, F_NONE );
			emit( n->srcpos(), SWI,  p ? (int) p->items.size() : 0 );

			if ( p )
			{
				for( size_t i=0;i<p->items.size();i++ )
				{
					labels.push_back( new_label() );
					emit( n->srcpos(), J, labels.back() );
				}
				
				for( size_t i=0;i<p->items.size();i++ )
				{
					place_label( labels[i] );
					pfgen( p->items[i], F_NONE );
					if ( i < p->items.size()-1 )
						emit( p->items[i] ? p->items[i]->srcpos() : n->srcpos(), J, Le );
				}
			}

			place_label( Le );
		}
			break;

		case expr_t::DEFINE:
		{
			lk_string Le( new_label() );
			lk_string Lf( new_label() );
			emit( n->srcpos(), J, Le );
			place_label( Lf );

			list_t *p = dynamic_cast<list_t*>(n->left );
			if( p )
			{
				for( size_t i=0;i<p->items.size();i++ )
				{
					iden_t *id = dynamic_cast<iden_t*>( p->items[i] );
					emit( p->items[i] ? p->items[i]->srcpos() : n->srcpos(), ARG, place_identifier(id->name) );
				}
			}

			pfgen( n->right, F_NONE );

			// if the last statement in the function block,
			// is not a return issue an implicit return statement
			if ( m_asm.back().op != RET )
			{
				// for implicit return at end of function, use last code line number of block
				srcpos_t posend = n->srcpos();
				posend.stmt = posend.stmt_end;
				emit( posend, RET, 0 );
			}

			place_label( Le );
			emit( n->srcpos(), FREF, Lf );

		}
			break;

		default:
			return false;
		}
	}
	else if ( ctlstmt_t *n = dynamic_cast<ctlstmt_t*>( root ) )
	{
		switch( n->ictl )
		{
		case ctlstmt_t::RETURN:
			pfgen(n->rexpr, F_NONE );
			emit( n->srcpos(), RET, n->rexpr ? 1 : 0 );
			break;

		case ctlstmt_t::BREAK:
			if ( m_breakAddr.size() == 0 )
				return error( lk_tr("cannot break from outside a loop") );

			emit( n->srcpos(), J, m_breakAddr.back() );
			break;

		case ctlstmt_t::CONTINUE:
			if ( m_continueAddr.size() == 0 )
				return error( lk_tr("cannot continue from outside a loop") );

			emit( n->srcpos(), J, m_continueAddr.back() );
			break;

		case ctlstmt_t::EXIT:
			emit( n->srcpos(), END );
			break;

		default:
			return false;
		}
	}
	else if ( iden_t *n = dynamic_cast<iden_t*>( root ) )
	{			
		if ( n->special )
		{
			emit( n->srcpos(), GET, place_identifier(n->name) );
			return true;
		}
		else
		{
			Opcode op = RREF;

			if ( flags & F_MUTABLE )
			{
				if ( n->globalval ) op = LGREF;
				else if ( n->constval ) op = LCREF;
				else op = LREF;
			}

			emit( n->srcpos(), op, place_identifier(n->name) );
		}
	}
	else if ( null_t *n = dynamic_cast<null_t*>(root) )
	{
		emit( n->srcpos(), NUL );
	}
	else if ( constant_t *n = dynamic_cast<constant_t*>(root ) )
	{
		emit( n->srcpos(), PSH, const_value( n->value ) );
	}
	else if ( literal_t *n = dynamic_cast<literal_t*>(root ) )
	{
		emit( n->srcpos(), PSH, const_literal( n->value ) );
	}

	return true;
}

}; // namespace lk
