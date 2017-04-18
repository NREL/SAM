#ifndef __lk_absyn_h
#define __lk_absyn_h

#include <vector>

#include <unordered_map>
using std::unordered_map;

#ifdef _MSC_VER
#pragma warning(disable: 4290)  // ignore warning: 'C++ exception specification ignored except to indicate a function is not __declspec(nothrow)'
#endif


#if defined(LK_USE_WXWIDGETS)
#include <wx/string.h>
#include <wx/hashmap.h>
#include <wx/intl.h>

#if wxCHECK_VERSION(2,9,0)
typedef wxUniChar lk_char;
#define LK_UNICODE 1
#else
typedef wxChar lk_char;
#endif

typedef wxString lk_string;
typedef wxStringHash lk_string_hash;
typedef wxStringEqual lk_string_equal;

#else
#include <string>

typedef std::string::value_type lk_char;
typedef std::string lk_string;

typedef std::hash<std::string> lk_string_hash;
typedef std::equal_to<std::string> lk_string_equal;

#endif

#define lk_tr(s) lk::get_translation(s)

namespace lk
{
	lk_string get_translation( const lk_string & );
	void set_translation_function( lk_string (*f)(const lk_string&) );

	lk_char lower_char( lk_char c );
	lk_char upper_char( lk_char c );
	bool convert_integer( const lk_string &str, int *x );
	bool convert_double( const lk_string &str, double *x );
	std::string to_utf8( const lk_string &str );
	lk_string from_utf8( const std::string &str );
	lk_string from_utf8( const char *str );
	lk_string to_string( lk_char c );

	extern int _node_alloc;
	
	class attr_t
	{
	public:
		virtual ~attr_t() {  };
	};

	class srcpos_t
	{
	public:
		static const srcpos_t npos;

		srcpos_t() { line = 0; stmt = 0; stmt_end = 0; }
		srcpos_t( const lk_string &f, int l, int s, int e=0 ) : file(f), line(l), stmt(s), stmt_end(e) { }
		lk_string file;
		int line, stmt, stmt_end;
	};
	
	bool operator==(const srcpos_t &, const srcpos_t &);

	class node_t
	{
	private:
		srcpos_t m_srcpos;
	public:
		attr_t *attr;
		node_t( srcpos_t pos ) : m_srcpos(pos), attr(0) { _node_alloc++; }
		virtual ~node_t() { _node_alloc--; if (attr) delete attr; }
		inline int line() { return m_srcpos.line; }
		inline lk_string file() { return m_srcpos.file; }
		inline srcpos_t srcpos() { return m_srcpos; }

	};
	
	class list_t : public node_t
	{
	public:
		std::vector<node_t*> items;
		list_t( srcpos_t pos ) : node_t(pos) { }
		virtual ~list_t() { for( size_t i=0;i<items.size();i++ ) delete items[i]; }
	};
					
	class iter_t : public node_t
	{
	public:
		node_t *init, *test, *adv, *block;
		iter_t( srcpos_t pos, node_t *i, node_t *t, node_t *a, node_t *b ) : node_t(pos), init(i), test(t), adv(a), block(b) { }
		virtual ~iter_t() { if (init) delete init; if (test) delete test; if (adv) delete adv; if (block) delete block; }
	};
	
	class cond_t : public node_t
	{
	public:
		node_t *test, *on_true, *on_false;
		bool ternary;
		cond_t(srcpos_t pos, node_t *t, node_t *ot, node_t *of, bool ter) : node_t(pos), test(t), on_true(ot), on_false(of), ternary(ter) { }
		virtual ~cond_t() { if (test) delete test; if (on_true) delete on_true; if (on_false) delete on_false; }
	};
		
	class expr_t : public node_t
	{
	public:
		enum {
			INVALID,
			
			PLUS,
			MINUS,
			MULT,
			DIV,
			INCR,
			DECR,
			DEFINE,
			ASSIGN,
			LOGIOR,
			LOGIAND,
			NOT,
			EQ,
			NE,
			LT,
			LE,
			GT,
			GE,
			EXP,
			NEG,
			INDEX,
			HASH,
			CALL,
			THISCALL,
			SIZEOF,
			KEYSOF,
			TYPEOF,
			INITVEC,
			INITHASH,
			SWITCH,
			PLUSEQ,
			MINUSEQ,
			MULTEQ,
			DIVEQ,
			MINUSAT,
			WHEREAT
		};
		int oper;
		node_t *left, *right;
		const char *operstr();
		expr_t(srcpos_t pos, int op, node_t *l, node_t *r) : node_t(pos), oper(op), left(l), right(r) {  }
		virtual ~expr_t() { if (left) delete left; if (right) delete right; }
	};
				
	class iden_t : public node_t
	{
	public:
		lk_string name;
		bool constval;
		bool globalval;
		bool special;
		iden_t( srcpos_t pos, const lk_string &n, bool cons, bool glob, bool speci) : node_t(pos), name(n), constval(cons), globalval(glob), special(speci) {  }
		virtual ~iden_t() { }
	};

	class ctlstmt_t : public node_t
	{
	public:
		enum { 
			INVALID,

			RETURN,
			EXIT,
			BREAK,
			CONTINUE
		};

		const char *ctlstr();

		int ictl;
		node_t *rexpr;
		ctlstmt_t( srcpos_t pos, int ctl, node_t *ex = 0) : node_t(pos), ictl(ctl), rexpr(ex) { }
		virtual ~ctlstmt_t() { if ( rexpr ) delete rexpr; }
	};
				
	class constant_t : public node_t
	{
	public:		
		double value;
		constant_t( srcpos_t pos, double v) : node_t(pos), value(v) {  }
		virtual ~constant_t() {  }
	};
	
	class literal_t : public node_t
	{
	public:
		lk_string value;
		literal_t( srcpos_t pos, const lk_string &s) : node_t(pos), value(s) {  }
		virtual ~literal_t() {  }
	};

	class null_t : public node_t
	{
	public:
		null_t( srcpos_t pos ) : node_t(pos) {  }
		virtual ~null_t() {  }
	};

	void pretty_print( lk_string &str, node_t *root, int level );
};

#endif
