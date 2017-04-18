#include <numeric>
#include <limits>
#include <math.h>

#include "numeric_solvers.h"
#include "csp_solver_util.h"

#include <algorithm>
#include <cmath>
#include <limits>

int C_import_mono_eq::operator()(double x, double *y)
{
	return mf_monotonic_function(x, y);
}

C_monotonic_eq_solver::C_monotonic_eq_solver(C_monotonic_equation & f): mf_mono_eq(f)
{
	m_x_guess = m_x_neg_err = m_x_pos_err = m_y_err_pos = m_y_err_neg =
		m_y_err = 
		m_func_x_lower = m_func_x_upper = std::numeric_limits<double>::quiet_NaN();

	m_is_pos_bound = m_is_neg_bound =
		m_is_pos_error = m_is_neg_error =
		m_is_pos_error_prev = m_is_neg_error_prev =
		false;

	m_iter = -1;

	// Set default settings:
	m_tol = 0.001;
	m_is_err_rel = true;
	m_iter_max = 50;
}

void C_monotonic_eq_solver::settings( double tol, int iter_limit, double x_lower, double x_upper, bool is_err_rel)
{
	m_tol = tol;
	m_func_x_lower = x_lower;
	m_func_x_upper = x_upper;

	m_is_err_rel = is_err_rel;

	m_iter_max = std::max(1, iter_limit);
}

double C_monotonic_eq_solver::check_against_limits(double x)
{
	if( m_func_x_lower != m_func_x_lower && m_func_x_upper != m_func_x_upper )
	{
		return x;
	}
	else if( m_func_x_lower != m_func_x_lower )
	{
		return std::min(m_func_x_upper, x);
	}
	else if( m_func_x_upper != m_func_x_upper )
	{
		return std::max(m_func_x_lower, x);
	}
	else
	{
		return std::min(m_func_x_upper, std::max(x, m_func_x_lower));
	}
}

double C_monotonic_eq_solver::calc_x_intercept(double x1, double y1, double x2, double y2)
{
	return (x2 - x1) / (y2 - y1)*(-y1) + x1;
}

int C_monotonic_eq_solver::solve(double x_guess_1, double x_guess_2, double y_target,
	double &x_solved, double &tol_solved, int &iter_solved)
{
	// Set / reset vector that tracks calls to equation
	ms_eq_call_tracker.resize(0);
	ms_eq_call_tracker.reserve(m_iter_max);

	// Check that x guesses fall with bounds (set during initialization)
	x_guess_1 = check_against_limits(x_guess_1);
	x_guess_2 = check_against_limits(x_guess_2);

	// Check that guesses are different
	if( x_guess_1 == x_guess_2 )
	{
		x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		iter_solved = 0;
		return EQUAL_GUESS_VALUES;
	}
	
	// Call function with x guesses
	double y1, y2;
	if( call_mono_eq(x_guess_1, &y1) != 0 )
	{
		y1 = std::numeric_limits<double>::quiet_NaN();
	}
	if( call_mono_eq(x_guess_2, &y2) != 0 )
	{
		y2 = std::numeric_limits<double>::quiet_NaN();
	}
	
	return solver_core(x_guess_1, y1, x_guess_2, y2, y_target, x_solved, tol_solved, iter_solved);
}

int C_monotonic_eq_solver::solve(S_xy_pair solved_pair_1, S_xy_pair solved_pair_2, double y_target,
	double &x_solved, double &tol_solved, int &iter_solved)
{
	// We could also call the equation before solving for an x value, so this solve method
	//    allows us to pass in exactly 2 x-y pairs
	// .... could improve this in future to accept a variable number of x-y pairs

	// Set / reset vector that tracks calls to equation
	ms_eq_call_tracker.resize(0);
	ms_eq_call_tracker.reserve(m_iter_max);

	// Get x & y values from solved_pairs
	double x_guess_1 = solved_pair_1.x;
	double x_guess_2 = solved_pair_2.x;

	// Check that x guesses fall with bounds (set during initialization)
	x_guess_1 = check_against_limits(x_guess_1);
	x_guess_2 = check_against_limits(x_guess_2);

	// Check that guesses are different
	if( x_guess_1 == x_guess_2 )
	{
		x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		iter_solved = 0;
		return EQUAL_GUESS_VALUES;
	}

	double y1 = solved_pair_1.y;
	double y2 = solved_pair_2.y;

	return solver_core(x_guess_1, y1, x_guess_2, y2, y_target, x_solved, tol_solved, iter_solved);
}

int C_monotonic_eq_solver::solver_core(double x_guess_1, double y1, double x_guess_2, double y2, double y_target,
	double &x_solved, double &tol_solved, int &iter_solved)
{
	// At this point, upstream 'solve' methods should have:
	// 1) Set/reset tracking vector
	// 2) Checked X values
	// 3) Found (but not checked) y values corresponding to each X value	
	// *****************************************************************

	// Check whether function returned real results
	if( y1 != y1 && y2 != y2 )
	{
		// Neither x guess produced a real result. Get out.
		x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
		iter_solved = 0;
		return NO_SOLUTION;
	}
	else if( y1 != y1 )
	{
		// y2 produced a real result, but y1 did not
		// So try guessing a new x1 on other side of x2
		x_guess_1 = x_guess_2 + (x_guess_2 - x_guess_1);
		// Check against bounds
		x_guess_1 = check_against_limits(x_guess_1);
		// Check that guesses are different
		if( x_guess_1 == x_guess_2 )
		{
			x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
			iter_solved = 0;
			return NO_SOLUTION;
		}

		if( call_mono_eq(x_guess_1, &y1) != 0 )
		{
			y1 = std::numeric_limits<double>::quiet_NaN();
		}

		// Check if this worked...
		if( y1 != y1 )
		{
			x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
			iter_solved = 0;
			return NO_SOLUTION;
		}
	}
	else if( y2 != y2 )
	{
		// y1 produced a real result, but y2 did not
		// So try guessing a new x2 on the other side of x1
		x_guess_2 = x_guess_1 + (x_guess_1 - x_guess_2);
		// Check against bounds
		x_guess_2 = check_against_limits(x_guess_2);
		// Check that guesses are different
		if( x_guess_1 == x_guess_2 )
		{
			x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
			iter_solved = 0;
			return NO_SOLUTION;
		}
		
		if( call_mono_eq(x_guess_2, &y2) != 0 )
		{
			y2 = std::numeric_limits<double>::quiet_NaN();
		}

		// Check if this worked...
		if( y2 != y2 )
		{
			x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
			iter_solved = 0;
			return NO_SOLUTION;
		}
	}

	// Ok, at this point, we should have two x guesses and two real y results
	// Now, need to calculate the slope of the error w/r/t x
	// So calculate errors
	double E1 = y1 - y_target;
	double E2 = y2 - y_target;

	// If tolerance corresponds to relative error, then calculate it
	if( m_is_err_rel )
	{
		if( y_target == 0 )
			return REL_TOL_WITH_0_TARGET;
		E1 = E1 / fabs(y_target);
		E2 = E2 / fabs(y_target);
	}

	// x guesses might be lucky, check against tolerance
	if( fabs(E1) < m_tol )
	{
		// Last call to equation was with x_guess_2, so call again here...
		// ... if equation is setting member data, this is required to have outputs matching calculated solution
		call_mono_eq(x_guess_1, &y1);
		x_solved = x_guess_1;
		tol_solved = E1;
		iter_solved = 0;
		return CONVERGED;
	}
	if( fabs(E2) < m_tol )
	{
		x_solved = x_guess_2;
		tol_solved = E2;
		iter_solved = 0;
		return CONVERGED;
	}
	// ***************************************************

	// Calculate slope of error vs x
	double E_slope = (E2 - E1) / (x_guess_2 - x_guess_1);

	// Now can set some upper and lower bound and error information...
	if( E1 > 0.0 && E2 > 0.0 )
	{
		// Error/x slope is positive, and errors are positive, so we know upper bound and error
		m_is_pos_bound = true;
		m_is_pos_error = true;
		m_is_pos_error_prev = true;
		m_is_neg_bound = false;
		m_is_neg_error = false;
		m_is_neg_error_prev = false;
		double x_pos_err_prev = std::numeric_limits<double>::quiet_NaN();
		double y_err_pos_prev = std::numeric_limits<double>::quiet_NaN();
		if( E1 < E2 )
		{
			m_x_pos_err = x_guess_1;
			m_y_err_pos = E1;
			x_pos_err_prev = x_guess_2;
			y_err_pos_prev = E2;
		}
		else
		{
			m_x_pos_err = x_guess_2;
			m_y_err_pos = E2;
			x_pos_err_prev = x_guess_1;
			y_err_pos_prev = E1;
		}

		m_x_guess = calc_x_intercept(m_x_pos_err, m_y_err_pos, x_pos_err_prev, y_err_pos_prev);
		m_x_guess = check_against_limits(m_x_guess);
	}
	else if( E2 < 0.0 && E1 < 0.0 )
	{
		// Error/x slope is positive, and x values are both negative, so we know
		m_is_neg_bound = true;
		m_is_neg_error = true;
		m_is_neg_error_prev = true;
		m_is_pos_bound = false;
		m_is_pos_error = false;
		m_is_pos_error_prev = false;
		double x_neg_err_prev = std::numeric_limits<double>::quiet_NaN();
		double y_err_neg_prev = std::numeric_limits<double>::quiet_NaN();
		if( E1 < E2 )
		{
			m_x_neg_err = x_guess_2;
			m_y_err_neg = E2;
			x_neg_err_prev = x_guess_1;
			y_err_neg_prev = E1;
		}
		else
		{
			m_x_neg_err = x_guess_1;
			m_y_err_neg = E1;
			x_neg_err_prev = x_guess_2;
			y_err_neg_prev = E2;
		}

		m_x_guess = calc_x_intercept(m_x_neg_err, m_y_err_neg, x_neg_err_prev, y_err_neg_prev);
		m_x_guess = check_against_limits(m_x_guess);
	}
	else if( E1 > 0.0 )		// So, per above logic, x_guess_2 < 0.0
	{
		// Error/x slope is positive, and have both a negative and positive error
		m_is_pos_bound = true;
		m_is_pos_error = true;
		m_is_pos_error_prev = false;
		m_is_neg_bound = true;
		m_is_neg_error = true;
		m_is_neg_error_prev = false;

		m_x_pos_err = x_guess_1;
		m_y_err_pos = E1;

		m_x_neg_err = x_guess_2;
		m_y_err_neg = E2;

		m_x_guess = calc_x_intercept(m_x_pos_err, m_y_err_pos, m_x_neg_err, m_y_err_neg);
	}
	else		// E1 < 0.0   && E2 > 0.0
	{
		// Error/x slope is positive, and have both a negative and positive error
		m_is_pos_bound = true;
		m_is_pos_error = true;
		m_is_pos_error_prev = false;
		m_is_neg_bound = true;
		m_is_neg_error = true;
		m_is_neg_error_prev = false;

		m_x_pos_err = x_guess_2;
		m_y_err_pos = E2;

		m_x_neg_err = x_guess_1;
		m_y_err_neg = E1;

		m_x_guess = calc_x_intercept(m_x_pos_err, m_y_err_pos, m_x_neg_err, m_y_err_neg);
	}

	// ******************************
	// Need to send a x_guess into the iteration loop!!!!!
	// ******************************

	// Set error such that iteration loop is entered
	m_y_err = 999.9*m_tol;
	m_iter = 0;		// Counter is first line inside loop, so first iteration = 1

	// Start iteration loop
	while( fabs(m_y_err) > m_tol || m_y_err != m_y_err )
	{
		m_iter++;		// First iteration = 1

		// Check if distance between bounds is too small
		double diff_x_bounds = std::numeric_limits<double>::quiet_NaN();
		if( E_slope > 0.0 )
		{
			if( m_x_pos_err != m_x_pos_err )
			{	// Haven't set x bound on positive error. Because slope > 0, then check neg err x against Upper bound
				diff_x_bounds = m_func_x_upper - m_x_neg_err;
			}
			else if( m_x_neg_err != m_x_neg_err )
			{	// Haven't set x bound on negative error. Because slope > 0, then check pos err x against Lower bound
				diff_x_bounds = m_x_pos_err - m_func_x_lower;
			}
			else
			{	// Both bounds are set, so compare them
				diff_x_bounds = m_x_pos_err - m_x_neg_err;
			}
		}
		else
		{
			if( m_x_pos_err != m_x_pos_err )
			{	// Haven't set x bound on positive error. Because slope < 0, then check neg err x against Lower bound
				diff_x_bounds = m_x_neg_err - m_func_x_lower;
			}
			else if( m_x_neg_err != m_x_neg_err )
			{	// Haven't set x bound on negative error. Because slope < 0, then check pos err x against Upper bound
				diff_x_bounds = m_func_x_upper - m_x_pos_err;
			}
			else
			{	// Both bounds are set, so compare them
				diff_x_bounds = m_x_neg_err - m_x_pos_err;
			}
		}
		if( m_is_err_rel )
		{
			diff_x_bounds = diff_x_bounds / std::max(m_x_neg_err, m_x_pos_err);
		}
		if( fabs(diff_x_bounds) < m_tol / 10.0 )
		{	// Assumes if x values are too close, then *something* is preventing convergence

			// 1) Solver can't find a negative error
			if( !m_is_neg_error && m_is_pos_error )
			{
				x_solved = m_x_pos_err;
				tol_solved = m_y_err_pos;
				iter_solved = m_iter;

				// Call function again with value we know produces a result
				double y_eq;
				call_mono_eq(x_solved, &y_eq);

				if(E_slope > 0.0)
					return SLOPE_POS_NO_NEG_ERR;
				else
					return SLOPE_NEG_NO_NEG_ERR;
			}
			// 2) Solver can't find a positive error
			else if( m_is_neg_error && !m_is_pos_error )
			{
				x_solved = m_x_neg_err;
				tol_solved = m_y_err_neg;
				iter_solved = m_iter;

				// Call function again with value we know produces a result
				double y_eq;
				call_mono_eq(x_solved, &y_eq);

				if(E_slope > 0.0)
					return SLOPE_POS_NO_POS_ERR;
				else
					return SLOPE_NEG_NO_POS_ERR;
			}
			// 3) Solver has bound both negative and positive errors
			else
			{	// Function is returning a value, but solver hasn't converged within tolerance
				x_solved = m_x_guess;
				tol_solved = m_y_err;
				iter_solved = m_iter;
				
				// Call function again with value we know produces a result
				double y_eq;
				call_mono_eq(x_solved, &y_eq);

				if(E_slope > 0.0)
					return SLOPE_POS_BOTH_ERRS;
				else
					return SLOPE_NEG_BOTH_ERRS;
			}
		}
		// Also check against iteration limit
		if( m_iter > m_iter_max )
		{
			// 1) Solver can't find a negative error
			if( !m_is_neg_error && m_is_pos_error )
			{
				x_solved = m_x_pos_err;
				tol_solved = m_y_err_pos;
				iter_solved = m_iter;

				// Call function again with value we know produces a result
				double y_eq;
				call_mono_eq(x_solved, &y_eq);

				if(E_slope > 0.0)
					return MAX_ITER_SLOPE_POS_NO_NEG_ERR;
				else
					return MAX_ITER_SLOPE_NEG_NO_NEG_ERR;
			}
			// 2) Solver can't find a positive error
			else if( m_is_neg_error && !m_is_pos_error )
			{
				x_solved = m_x_neg_err;
				tol_solved = m_y_err_neg;
				iter_solved = m_iter;

				// Call function again with value we know produces a result
				double y_eq;
				call_mono_eq(x_solved, &y_eq);

				if(E_slope > 0.0)
					return MAX_ITER_SLOPE_POS_NO_POS_ERR;
				else
					return MAX_ITER_SLOPE_NEG_NO_POS_ERR;
			}
			// 3) Solver has bound both negative and positive errors
			else
			{	// Function is returning a value, but solver hasn't converged within tolerance
				x_solved = m_x_guess;
				tol_solved = m_y_err;
				iter_solved = m_iter;

				// Call function again with value we know produces a result
				double y_eq;
				call_mono_eq(x_solved, &y_eq);

				if(E_slope > 0.0)
					return MAX_ITER_SLOPE_POS_BOTH_ERRS;
				else
					return MAX_ITER_SLOPE_NEG_BOTH_ERRS;
			}
		}

		// **************************************************************
		// **************************************************************

		// Subsequent iterations need to re-calculate x
		if( m_iter > 1 )
		{
			if( m_y_err != m_y_err )
			{	// Function isn't returning a real value with which to calculate an error
				if( !m_is_neg_bound && !m_is_pos_bound )
				{	// This shouldn't occur, as we need at least one bound to find the error slope, but let's keep it...
					x_solved = tol_solved = std::numeric_limits<double>::quiet_NaN();
					iter_solved = m_iter;
					return NO_SOLUTION;
				}
				else if( m_is_neg_bound && !m_is_pos_bound )
				{	// know that we have a negative error bound, so assume that x that caused NaN is pos err bound
					m_x_pos_err = m_x_guess;
					m_is_pos_bound = true;
					m_is_pos_error = false;
					m_is_pos_error_prev = false;
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
					// Should need to check against bounds if both 'is_bounds' = true
				}
				else if( !m_is_neg_bound && m_is_pos_bound )
				{	// know that we have an upper bound, so assume that x that caused NaN is lower bound
					m_x_neg_err = m_x_guess;
					m_is_neg_bound = true;
					m_is_neg_error = false;
					m_is_neg_error_prev = false;
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
					// Should need to check against bounds if both 'is_bounds' = true
				}
				else if( m_is_neg_error && !m_is_pos_error )
				{	// Know both bounds, but don't have positive error, so assume that x that caused NaN is pos error bound
					// so keep using bisection and hopefully get a real number out of function
					m_x_pos_err = m_x_guess;
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
				}
				else if( !m_is_neg_error && m_is_pos_error )
				{	// Know both bounds, but don't have negative error, so assume that x that caused NaN is neg error bound
					// so keep using bisection and hopefully get a real number out of function
					m_x_neg_err = m_x_guess;
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
				}
				else
				{	// This shouldn't happen, so let's throw an exception
					throw(C_csp_exception("Numerical solver iteration with a NaN error found an unexpected case"));
				}
			}
			else if( m_y_err > 0.0 )
			{
				double x_pos_err_prev = std::numeric_limits<double>::quiet_NaN();
				double y_err_pos_prev = std::numeric_limits<double>::quiet_NaN();
				if( m_is_pos_error )
				{
					if (m_y_err > m_y_err_pos)
					{	// Equation not behaving monotonically, so disregard previous smaller error and use current larger
						m_is_pos_error_prev = false;
					}
					else
					{
						m_is_pos_error_prev = true;
						x_pos_err_prev = m_x_pos_err;
						y_err_pos_prev = m_y_err_pos;
					}					
				}
				else
				{
					m_is_pos_error_prev = false;
				}
				m_x_pos_err = m_x_guess;
				m_y_err_pos = m_y_err;
				m_is_pos_bound = true;
				m_is_pos_error = true;
				if( !m_is_neg_bound )
				{	// Only have positive error, and getting another positive error, so...
					if( m_is_pos_error_prev )
					{	// If we have two positive errors, then linearly interpolate using them
						m_x_guess = calc_x_intercept(m_x_pos_err, m_y_err_pos, x_pos_err_prev, y_err_pos_prev);
					}
					else if( E_slope > 0.0 )
					{
						m_x_guess = m_x_pos_err - 0.5*std::max(x_guess_1, x_guess_2);
					}
					else
					{

						m_x_guess = m_x_pos_err + 0.5*std::max(fabs(x_guess_1), fabs(x_guess_2));
					}
					m_x_guess = check_against_limits(m_x_guess);
				}
				else if( !m_is_neg_error )           //( (m_is_neg_bound && !m_is_pos_bound) || (m_is_neg_bound && m_is_pos_bound) )
				{	// Have a negative error bound, but not error
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
				}
				else
				{
					m_x_guess = calc_x_intercept(m_x_neg_err, m_y_err_neg, m_x_pos_err, m_y_err_pos);
				}
			}
			else		// (m_y_err < 0.0)
			{
				double x_neg_err_prev = std::numeric_limits<double>::quiet_NaN();
				double y_err_neg_prev = std::numeric_limits<double>::quiet_NaN();
				if( m_is_neg_error )
				{
					if (m_y_err < m_y_err_neg)
					{	// Equation not behaving monotonically, so disregard previous smaller error and use current larger
						m_is_neg_error_prev = false;
					}
					else
					{
						m_is_neg_error_prev = true;
						x_neg_err_prev = m_x_neg_err;
						y_err_neg_prev = m_y_err_neg;
					}					
				}
				else
				{
					m_is_pos_error_prev = false;
				}
				m_x_neg_err = m_x_guess;
				m_y_err_neg = m_y_err;
				m_is_neg_bound = true;
				m_is_neg_error = true;
				if( !m_is_pos_bound )
				{	// Only have negative error, and getting another negative error, so use bisection
					if( m_is_neg_error_prev )
					{
						m_x_guess = calc_x_intercept(m_x_neg_err, m_y_err_neg, x_neg_err_prev, y_err_neg_prev);
					}
					else if( E_slope > 0.0 )
					{
						m_x_guess = m_x_neg_err + 0.5*std::max(fabs(x_guess_1), fabs(x_guess_2));
					}
					else
					{
						m_x_guess = m_x_neg_err - 0.5*std::max(x_guess_1, x_guess_2);
					}
					m_x_guess = check_against_limits(m_x_guess);
				}
				else if( !m_is_pos_error )		//( (!m_is_neg_bound && m_is_pos_bound) || (m_is_neg_bound && m_is_pos_bound) )
				{	// Have a positive error bound, but not error
					m_x_guess = 0.5*(m_x_pos_err + m_x_neg_err);
				}
				else
				{
					m_x_guess = calc_x_intercept(m_x_neg_err, m_y_err_neg, m_x_pos_err, m_y_err_pos);
				}
			}
		}

		// Call function with new x_guess
		double y_calc;
		if(call_mono_eq(m_x_guess, &y_calc) != 0)
		{
			y_calc = std::numeric_limits<double>::quiet_NaN();
		}

		m_y_err = y_calc - y_target;

		if( m_is_err_rel )
			m_y_err = m_y_err / fabs(y_target);

	}	// End of iteration on x

	x_solved = m_x_guess;
	tol_solved = m_y_err;
	iter_solved = m_iter;

	return CONVERGED;
}

int C_monotonic_eq_solver::call_mono_eq(double x, double *y)
{
	ms_eq_tracker_temp.err_code = mf_mono_eq(x, y);

	ms_eq_tracker_temp.x = x;
	ms_eq_tracker_temp.y = *y;
	
	ms_eq_call_tracker.push_back(ms_eq_tracker_temp);

	return ms_eq_tracker_temp.err_code;
}

int C_monotonic_eq_solver::test_member_function(double x, double *y)
{
	return mf_mono_eq(x,y);
}

