#ifndef __lk_eval_h
#define __lk_eval_h

#include <lk/absyn.h>
#include <lk/env.h>

namespace lk {
	
	enum { CTL_NONE, CTL_BREAK, CTL_CONTINUE, CTL_RETURN, CTL_EXIT };
	

	class eval
	{
		lk::node_t *m_tree;
		lk::env_t m_localEnv;
		lk::env_t *m_env;
		vardata_t m_result;
		std::vector< lk_string > m_errors;

	public:
		eval( lk::node_t *tree );
		eval( lk::node_t *tree, lk::env_t *env );
		virtual ~eval();

		bool run();
		
		virtual bool special_set( const lk_string &name, vardata_t &val );
		virtual bool special_get( const lk_string &name, vardata_t &val );
		inline virtual bool on_run( int /* line */ ) { return true; }

		std::vector<lk_string> &errors() { return m_errors; }
		size_t error_count() { return m_errors.size(); }
		lk_string get_error( size_t i ) { if ( i < m_errors.size() ) return m_errors[i]; else return lk_string(""); }

		lk::env_t &env() { return *m_env; }
		vardata_t &result() { return m_result; }

	protected:
		bool interpret( node_t *root,
			lk::env_t *cur_env,
			vardata_t &result,
			unsigned int flags, /* normally 0 */
			unsigned int &ctl_id);

		bool do_op_eq( void (*oper)(lk::vardata_t &, lk::vardata_t &), 
			lk::expr_t *n, lk::env_t *cur_env, unsigned int &flags, unsigned int &ctl_id,
			lk::vardata_t &result, lk::vardata_t &l, lk::vardata_t &r );
	};

	
};

#endif
