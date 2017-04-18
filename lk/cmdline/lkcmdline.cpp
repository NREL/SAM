#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <memory>

#include <lk/absyn.h>
#include <lk/env.h>
#include <lk/eval.h>
#include <lk/parse.h>
#include <lk/lex.h>
#include <lk/stdlib.h>
#include <lk/invoke.h>
#include <lk/codegen.h>
#include <lk/vm.h>

void fcall_out( lk::invoke_t &cxt )
{
	LK_DOC("out", "Output data to the console.", "(...):none");
	for (size_t i=0;i<cxt.arg_count();i++)
		fputs( lk::to_utf8(cxt.arg(i).as_string()).c_str(), stdout );
}

void fcall_outln( lk::invoke_t &cxt )
{
	LK_DOC("outln", "Output data to the console followed by a newline.", "(...):none");
	for (size_t i=0;i<cxt.arg_count();i++)
		fputs( lk::to_utf8(cxt.arg(i).as_string()).c_str(), stdout );
	
	fputs( "\n", stdout );
	fflush( stdout );
}
void fcall_in(  lk::invoke_t &cxt )
{
#define NBUF 2048
	LK_DOC("in", "Input text from the user.", "(none):string");
	char buf[NBUF];
	fgets( buf, NBUF-1, stdin );
	cxt.result().assign( lk::from_utf8( buf ) );	
}

int main(int argc, char *argv[])
{
	bool parse_only = false;
	bool use_vm = true;
	
	if ( argc <= 1 )
	{
		printf("no input file specified\n");
		return -1;
	}
	
	if ( argc > 2 )
	{
		if( strcmp( argv[2], "--parse" ) == 0 ) parse_only = true;
		if( strcmp( argv[2], "--eval" ) == 0 ) use_vm = false;
	}
	
	lk::input_file p( argv[1] );
	lk::parser parse( p );

	std::auto_ptr<lk::node_t> tree( parse.script() );			
	int i=0;
	while ( i < parse.error_count() )
		printf( "%s\n", parse.error(i++).c_str() );
	
	if ( tree.get() == NULL || parse.token() != lk::lexer::END)
		printf("parsing did not reach end of input\n");

	if ( tree.get() == NULL || parse.error_count() > 0 || parse.token() != lk::lexer::END )
		return -1;
	
	if ( parse_only ) return 0;
	
	lk::env_t env;
	env.register_func( fcall_in );
	env.register_func( fcall_out );
	env.register_func( fcall_outln );

	env.register_funcs( lk::stdlib_basic() );
	env.register_funcs( lk::stdlib_string() );
	env.register_funcs( lk::stdlib_math() );

	if ( use_vm )
	{
		lk::codegen C;
		if ( C.generate( tree.get() ) )
		{
			lk::bytecode bc;
			C.get( bc );
			
			lk::vm V;
			V.load( &bc );
			V.initialize( &env );
			if ( !V.run() )
			{
				printf("vm: %s\n", (const char*)V.error().c_str());
				return -1;
			}
		}
		else
		{
			printf("codegen: %s\n", (const char*)C.error().c_str() );
			return -1;
		}
	}
	else
	{
		lk::eval ev( tree.get(), &env );
		if ( !ev.run() )
		{
			for( size_t i=0;i<ev.error_count();i++ )
				printf("eval: %s\n", (const char*) ev.get_error(i).c_str() );
			
			return -1;
		}
		
	}
		
	return 0;
}
