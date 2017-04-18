#ifndef __lk_parse_h
#define __lk_parse_h

#include <vector>
#include <lk/lex.h>
#include <lk/absyn.h>

namespace lk
{
	class parser
	{
	public:
		parser( input_base &input, const lk_string &name = "" );

		void add_search_path( const lk_string &path ) { m_searchPaths.push_back( path ); }
		void add_search_paths( const std::vector<lk_string> &paths );
		std::vector<lk_string> get_search_paths() const { return m_searchPaths; }
				
		node_t *script();
		node_t *block();
		node_t *statement();
		node_t *test();
		node_t *enumerate();
		node_t *loop();
		node_t *define();
		node_t *assignment();		
		node_t *ternary();
		node_t *logicalor();
		node_t *logicaland();
		node_t *equality();
		node_t *relational();
		node_t *additive();
		node_t *multiplicative();
		node_t *exponential();
		node_t *unary();
		node_t *postfix();
		node_t *primary();		
		
		srcpos_t srcpos();
		int line() { return lex.line(); }
		int error_count() { return m_errorList.size(); }
		lk_string error(int idx, int *line = 0);

		int token();
		bool token(int t);
		
		void skip();
		bool match(int t);
		bool match( const char *s );
		
	private:
		list_t *ternarylist( int septok, int endtok );
		list_t *identifierlist( int septok, int endtok );

	
		void error( const lk_string &s );
		void error( const char *fmt, ... );
		
		lexer lex;				
		int m_tokType;
		int m_lastLine;
		int m_lastStmt;
		int m_lastBlockEnd;
		lk_string m_lexError;
		bool m_haltFlag;
		struct errinfo { int line; lk_string text; };
		std::vector<errinfo> m_errorList;
		std::vector< lk_string > m_importNameList, m_searchPaths;
		lk_string m_name;
	};
};

#endif
