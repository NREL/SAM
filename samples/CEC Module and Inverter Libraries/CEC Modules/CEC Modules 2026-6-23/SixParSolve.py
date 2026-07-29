"""
CEC 6-Parameter PV Module Single-Diode Model Solver

Solves for the six parameters (a, I_L, I_o, R_s, R_sh, Adjust) of the California
Energy Commission (CEC) photovoltaic module performance model using manufacturer
datasheet specifications as inputs.

The model is based on the five-parameter single-diode equivalent circuit:

    I = I_L - I_o * (exp((V + I*R_s) / a) - 1) - (V + I*R_s) / R_sh

where the five circuit parameters (a, I_L, I_o, R_s, R_sh) are determined at Standard
Test Conditions (STC: 1000 W/m^2, 25 C cell temperature, AM1.5G spectrum). A sixth
parameter "Adjust" scales the short-circuit and open-circuit temperature coefficients
(alpha_sc, beta_oc) to match the manufacturer's maximum power temperature coefficient
(gamma_Pmp).

Temperature dependencies of the circuit parameters follow semiconductor physics, based on DeSoto:
    - a(T) = a_ref * T / T_ref
    - I_L(T) = I_L,ref + alpha_sc * (T - T_ref)
    - I_o(T) = I_o,ref * (T/T_ref)^3 * exp(...)
    - E_g(T) = E_g,ref * (1 - 0.0002677 * (T - T_ref))
    - R_s and R_sh are held constant at SRC values           (Dobos simplification)

The gamma_Pmp temperature coefficient is fitted by evaluating the maximum power point
over a range of temperatures (default: 10 C to 50 C at 3 C intervals) and averaging
the slope of P_max(T).

References
----------
[1] De Soto, W., Klein, S. A., and Beckman, W. A., 2006, "Improvement and Validation
    of a Model for Photovoltaic Array Performance," Solar Energy, 80(1), pp. 78-88.
    (Originally presented as: De Soto, W., 2004, "Improvement and Validation of a
    Model for Photovoltaic Array Performance," M.S. thesis, University of
    Wisconsin-Madison.)

[2] Dobos, A. P., 2012, "An Improved Coefficient Calculator for the California
    Energy Commission 6 Parameter Photovoltaic Module Model," Journal of Solar Energy
    Engineering, 134(2), 021011. DOI: 10.1115/1.4005759
"""

from pathlib import Path
import sys
import pyomo.environ as pyo
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pyomo.util.infeasible import log_infeasible_constraints, log_infeasible_bounds
import logging
from datetime import datetime
import multiprocessing as mp

NUM_OF_WORKERS = max(1, mp.cpu_count() - 2)
MAX_ITER = 3000
INFEASIBILITY_THRESHOLD = 0.5

logging.getLogger('pyomo.core').setLevel(logging.ERROR)
solve_log = logging.getLogger('solve_log')
solve_log.setLevel(logging.INFO)
solve_log = logging.LoggerAdapter(solve_log, {"tag": solve_log})

IL_SCALING = 1e8
RSH_SCALING = 1e-3
test_data_cols = ['A_c', 'N_s', 'I_sc_ref', 'V_oc_ref', 'I_mp_ref', 'V_mp_ref', 'T_NOCT',
                  'gamma_pmp', 'alpha_sc', 'beta_oc']
model_param_cols = ["a_py", "Il_py", "Io_py", "Rs_py", "Rsh_py", "Adj_py"]
iv_diff_cols = ['d_Isc', 'd_Imp', 'd_Vmp', 'd_Pmp']
solve_tracking_cols = ['solve_return_code', 'solve_pass']


def current_at_voltage_cec(Vmodule, IL_ref, IO_ref, RS, A_ref, RSH_ref, I_mp_ref):
    """
    Solve for current I at a given voltage V using Newton-Raphson on the single diode equation.

    Iteratively solves:
        I = I_L - I_o * (exp((V + I*R_s) / a) - 1) - (V + I*R_s) / R_sh

    The Newton step uses the analytical derivative (Jacobian) of the implicit equation.
    Used for plotting only, so numerical precision is only 1e-4.
    Adapted from ssc/shared/6par_solve.h L 423.

    For better stability and precision, use pvlib.pvsystem.i_from_v().
    """
    F = 0
    Fprime = 0
    Iold = 0.0
    Inew = I_mp_ref

    it = 0
    maxit = 4000
    while (abs(Inew - Iold) > 1.0e-4 and it < maxit ):
        Iold = Inew

        F = IL_ref - Iold - IO_ref * \
            (np.exp((Vmodule + Iold * RS) / A_ref) - 1.0) - \
            (Vmodule + Iold * RS) / RSH_ref

        Fprime = -1.0 - IO_ref * (RS / A_ref) * \
            np.exp((Vmodule + Iold * RS) / A_ref) - \
            (RS / RSH_ref)

        Inew = max(0.0, (Iold - (F / Fprime)))

    return Inew

def cec_model_params_at_condition(model, Irr, T_cell_K):
    """
    Compute temperature- and irradiance-adjusted single diode parameters.

    Applies the DeSoto/Dobos temperature and irradiance corrections to translate
    the five STC circuit parameters to operating conditions:
        - a(T) = a_ref * T / T_ref                             [Dobos eq.3]
        - I_o(T) from semiconductor physics                    [Dobos eq.5]
        - I_L(S,T) = S/S_ref * (I_L,ref + alpha_sc' * dT)      [Dobos eq.4]
        - R_sh(S) = R_sh,ref * S_ref / S                       [DeSoto Sec.3.5]
        - R_s = R_s,ref (held constant)                        [Dobos simplification]

    Parameters
    ----------
    model : pyo.ConcreteModel
        Solved model with populated parameters.
    Irr : float
        Irradiance [W/m^2].
    T_cell_K : float
        Cell temperature [K].

    Returns
    -------
    tuple : (IL_oper, IO_oper, Rs, A_oper, Rsh_oper)
    """
    m = model.solver
    Tc_ref = pyo.value(m.Tref)
    a = pyo.value(m.par.a)
    Io = pyo.value(m.par.Io)
    eg0 = pyo.value(m.Egref)
    Adj = pyo.value(m.par.Adj)
    alpha = pyo.value(m.aIsc)
    Il = pyo.value(m.par.Il)
    Io = pyo.value(m.par.Io)
    Rs = pyo.value(m.par.Rs)
    Rsh = pyo.value(m.par.Rsh)

    I_ref = 1000
    muIsc = alpha * (1-Adj/100)
    
    A_oper = a * T_cell_K / Tc_ref
    EG = eg0 * (1-0.0002677*(T_cell_K-Tc_ref))
    k = pyo.value(m.k)     # Boltzmann constant eV/K
    # instead of 1/KB, is 11600 in L129
    IO_oper = Io * np.power(T_cell_K/Tc_ref, 3) * np.exp(eg0 / (k*(Tc_ref)) - (EG / (k*(T_cell_K))))
    
    Rsh_oper = Rsh*(I_ref/Irr)
    IL_oper = Irr/I_ref *( Il + muIsc*(T_cell_K-Tc_ref) )
    if IL_oper < 0.0:
        IL_oper = 0.0
    
    return IL_oper, IO_oper, Rs, A_oper, Rsh_oper


def cec_model_ivcurve(model, Irr, T_cell_K, vmax, npts):
    """
    Calculate the IV curve with voltage on x-axis and current on y-axis at the given irradiance and temperature
    """
    I_mp_ref = pyo.value(model.solver.Imp)
    IL_oper, IO_oper, Rs, A_oper, Rsh_oper = cec_model_params_at_condition(model, Irr, T_cell_K)

    y_I = []

    V = np.linspace(0, vmax, npts)
    for i, v in enumerate(V):
        I = current_at_voltage_cec( v, IL_oper, IO_oper, Rs, A_oper, Rsh_oper, I_mp_ref )	
        y_I.append(I)
    return V, y_I
    
def plot_iv_curve(model, linestyle='solid', label=None, plot_anchors=False):
    """
    Plot IV curves for a range of conditions using a model from `create_model` with all the test and model parameters populated

    Model must have populated data from `test_data_cols` and `model_param_cols`
    """
    curves = [ [ 1000, 2, 'black' ],
               [ 1000,  25, 'red' ],
               [ 1000,  47, 'orange' ],
               [ 800,  0, 'blue' ],
               [ 400,  0, 'green' ] ]
        
    vmax = pyo.value(model.solver.Voc)
    alpha_sc = pyo.value(model.solver.aIsc)
    I_sc_ref = pyo.value(model.solver.Isc)
    beta_oc = pyo.value(model.solver.bVoc)
    V_oc_ref = pyo.value(model.solver.Voc)
    gamma_pmp = pyo.value(model.solver.gPmp)
    V_mp_ref = pyo.value(model.solver.Vmp)
    I_mp_ref = pyo.value(model.solver.Imp)

    npts = 250
    for curve in curves:
        Irr = curve[0]
        Tc = curve[1]

        V_oc = beta_oc * (Tc - 25) + V_oc_ref
        x_V, y_I = cec_model_ivcurve(model, Irr, Tc + 273.15, V_oc, npts)
        plt.plot(x_V, y_I, label=f"{label} {Irr} W/m^2 {Tc} C", color=curve[2], linestyle=linestyle)

        if plot_anchors and Irr == 1000:
            I_sc = alpha_sc * (Tc - 25) + I_sc_ref
            plt.plot(0, I_sc, marker="o", markersize=10, alpha=0.5, color=curve[2])
            plt.plot(V_oc, 0, marker="v", markersize=10, alpha=0.5, color=curve[2])
            
            P_mp = (gamma_pmp * (Tc - 25) * 1e-2 + 1) * (V_mp_ref * I_mp_ref)
            mp_ind = np.argmax(x_V * y_I)
            if Tc == 25:
                I_mp = I_mp_ref
                V_mp = V_mp_ref
            else:
                I_mp = y_I[mp_ind]
                V_mp = P_mp / I_mp
            plt.plot(V_mp, I_mp, marker="*", markersize=10, alpha=0.5, color=curve[2])


def create_model(gamma_curve_dt=5):
    """
    Create a Pyomo model for the CEC 6-parameter single-diode PV module model.

    Constructs a nonlinear system of equations (f0-f7) that determines the six circuit
    parameters (a, I_L, I_o, R_s, R_sh, Adjust) from manufacturer datasheet values at
    STC. The system consists of:

        f0-f3: Four constraints anchoring the I-V curve at STC key points
               (short circuit, open circuit, max power point, dP/dV=0 at MPP).
               [Dobos eqs.11-14+19; DeSoto eqs.4.7, 4.13-4.15]
        f4:    Open-circuit voltage temperature correction at T_ref + dT.
               [Dobos eq.22]
        f5/f6: Per-temperature MPP constraints (dP/dV=0 and I-V equation) with
               temperature-adjusted a, I_o, I_L, E_g; R_s and R_sh held constant.
               [Dobos eqs.23-24; DeSoto eqs.4.7, 4.13-4.15 with Ch.3-4 temp deps]
        f7:    Gamma (max power temperature coefficient) matching.
               [Dobos eq.26]

    Parameters
    ----------
    gamma_curve_dt : float, default 5
        Temperature step [K] for sampling the gamma fitting range (10 C to 50 C).
        Smaller values give more temperature points (more f5/f6 constraints, better
        gamma accuracy, harder to solve). Larger values (e.g. 10) reduce constraints
        for easier convergence at the cost of gamma fitting precision.

    Returns
    -------
    model : pyo.ConcreteModel
        Pyomo model with all constraints defined. Call `set_parameters` to populate
        datasheet inputs, then `set_initial_guess` and `solve_model` to solve.
    """
    model = pyo.ConcreteModel()
    model.solver = pyo.Block()
    m = model.solver

    m.Vmp = pyo.Param(domain=pyo.NonNegativeReals, mutable=True)
    m.Imp = pyo.Param(domain=pyo.NonNegativeReals, mutable=True)
    m.Voc = pyo.Param(domain=pyo.NonNegativeReals, mutable=True)
    m.Isc = pyo.Param(domain=pyo.NonNegativeReals, mutable=True)
    m.aIsc = pyo.Param(domain=pyo.Reals, mutable=True, units=pyo.units.A/pyo.units.K)
    m.bVoc = pyo.Param(domain=pyo.Reals, mutable=True, units=pyo.units.V/pyo.units.K)
    m.gPmp = pyo.Param(domain=pyo.Reals, mutable=True, units=pyo.units.percent/pyo.units.K)
    m.Egref = pyo.Param(domain=pyo.NonNegativeReals, mutable=True, initialize=1.121)
    m.k = pyo.Param(domain=pyo.NonNegativeReals, mutable=True, initialize=8.617332478e-05, units=pyo.units.eV/pyo.units.K)
    m.Tref = pyo.Param(domain=pyo.NonNegativeReals, mutable=True, units=pyo.units.K)

    # Six circuit parameters to solve for
    m.par = pyo.Block()
    m.par.a = pyo.Var(domain=pyo.NonNegativeReals, bounds=(0.05, 15), initialize=7.75)       # Modified ideality factor [V], Dobos eq.2
    m.par.Il = pyo.Var(domain=pyo.NonNegativeReals, bounds=(0.01, 20), initialize=10.25)      # Light current [A]
    m.par.Io = pyo.Var(domain=pyo.NonNegativeReals, bounds=(1e-13, 1e-7), initialize=5e-8)    # Diode reverse saturation current [A]
    m.par.Rs = pyo.Var(domain=pyo.NonNegativeReals, bounds=(0.001, 75), initialize=32.5)      # Series resistance [Ohm]
    m.par.Rsh = pyo.Var(domain=pyo.NonNegativeReals, bounds=(1, 1e6), initialize=5e5)         # Shunt resistance [Ohm]
    m.par.Adj = pyo.Var(domain=pyo.Reals, bounds=(-100, 100), initialize=1)                   # Adjust parameter [%], Dobos eqs.8-9

    # f0: Short-circuit condition (V=0, I=Isc). Single diode eq. evaluated at SC.
    m.par.f0 = pyo.Constraint(expr=m.par.Il - m.par.Io*( pyo.exp( m.Isc*m.par.Rs / m.par.a ) - 1 ) - m.Isc*m.par.Rs/m.par.Rsh - m.Isc == 0)
    # f1: Open-circuit condition (V=Voc, I=0). Single diode eq. evaluated at OC.
    m.par.f1 = pyo.Constraint(expr=m.par.Io*( pyo.exp( m.Voc/m.par.a ) - 1 ) + m.Voc/m.par.Rsh -m.par.Il == 0)
    # f2: Maximum power point condition (V=Vmp, I=Imp). Single diode eq. evaluated at MPP.
    m.par.f2 = pyo.Constraint(expr=m.par.Il - m.par.Io*( pyo.exp( (m.Vmp + m.Imp*m.par.Rs) / m.par.a ) - 1 ) - (m.Vmp + m.Imp*m.par.Rs)/m.par.Rsh - m.Imp == 0)
    # f3: MPP derivative constraint (dP/dV=0 at MPP)
    m.par.f3 = pyo.Constraint(expr=m.Imp - m.Vmp*(
        ( m.par.Io/m.par.a*pyo.exp( (m.Vmp + m.Imp*m.par.Rs)/m.par.a ) + 1/m.par.Rsh )
        /( 1 + m.par.Io*m.par.Rs/m.par.a*pyo.exp( (m.Vmp + m.Imp*m.par.Rs)/m.par.a ) + m.par.Rs/m.par.Rsh ) ) == 0)

    # f4: Open-circuit voltage temperature correction.
    m.par.dT = pyo.Param(initialize=5)

    m.par.aT = pyo.Expression(expr=m.par.a*(m.Tref+m.par.dT)/m.Tref)                                                                  # Dobos eq.3
    m.par.VocT = pyo.Expression(expr=m.bVoc*(1+m.par.Adj/100.0)*m.par.dT + m.Voc)                                                      # Dobos eq.21 (Voc at T')
    m.par.Eg = pyo.Expression(expr=(1-0.0002677*m.par.dT)*m.Egref)                                                                     # Dobos eq.6
    m.par.IoT = pyo.Expression(expr=m.par.Io*( (m.Tref+m.par.dT)/m.Tref )**3 *pyo.exp( 11600 * (m.Egref/m.Tref - m.par.Eg/(m.Tref+m.par.dT))))  # Dobos eq.5; 11600 ~= 1/k_B [K/eV]
    m.par.f4 = pyo.Constraint(expr=m.par.Il+m.aIsc*(1-m.par.Adj/100)*m.par.dT - m.par.IoT*(pyo.exp( m.par.VocT/m.par.aT ) - 1 ) - m.par.VocT/m.par.Rsh == 0)  # Dobos eq.22

    # Gamma (maximum power temperature coefficient) fitting block.
    # Evaluates P_max(T) over a range of temperatures by solving the single diode
    # equation simultaneously with the MPP derivative condition at each temperature.
    temperatures = np.arange(10 + 273.15, 50 + 273.15, gamma_curve_dt)

    def gamma_expr(b, t):
        """Finite-difference gamma between adjacent temperature points [%/K]"""
        return (b.pt[t].Pmp_Tc-b.pt[t-1].Pmp_Tc)*100/(m.Vmp*m.Imp*(b.pt[t].Tc-b.pt[t-1].Tc))

    def gamma_blocks(b, i):
        """Per-temperature block: solves for Vmp(T) and Imp(T) using temperature-adjusted parameters."""
        b.Tc = pyo.Param(initialize=temperatures[i - 1])
        b.Vmp_Tc = pyo.Var(domain=pyo.NonNegativeReals)
        b.Imp_Tc = pyo.Var(domain=pyo.NonNegativeReals)

        # Temperature-adjusted circuit parameters at cell temperature Tc
        b.a_Tc = pyo.Expression(expr=m.par.a * b.Tc / m.Tref)                                                       # Dobos eq.3; DeSoto: a = N_ser*n*kT/q, so a(T) = a_ref*T/T_ref
        b.Eg_Tc = pyo.Expression(expr=m.Egref * (1-0.0002677*(b.Tc-m.Tref)))                                        # Dobos eq.6; DeSoto Ch.4: linear bandgap approx for Si
        b.Io_Tc = pyo.Expression(expr=m.par.Io* ( b.Tc/m.Tref)**3 * pyo.exp((1/m.k)*(m.Egref/m.Tref-b.Eg_Tc/b.Tc))) # Dobos eq.5; DeSoto Ch.3: semiconductor I_o(T)
        b.Il_Tc = pyo.Expression(expr=m.par.Il + (m.aIsc*(1-m.par.Adj/100))*(b.Tc-m.Tref))                          # Dobos eq.4+8; DeSoto eq.4.4 with Adjust-scaled alpha_sc

        b.f_5 = pyo.Constraint(expr=b.Imp_Tc - b.Vmp_Tc *( b.Io_Tc/b.a_Tc*pyo.exp( (b.Vmp_Tc+b.Imp_Tc*m.par.Rs)/b.a_Tc ) + 1/m.par.Rsh )
                            / ( 1 + m.par.Rs/m.par.Rsh + b.Io_Tc*m.par.Rs/b.a_Tc*pyo.exp( (b.Vmp_Tc+b.Imp_Tc*m.par.Rs)/b.a_Tc ) ) == 0)
        b.f_6 = pyo.Constraint(expr=b.Il_Tc - b.Io_Tc*(pyo.exp( (b.Vmp_Tc+b.Imp_Tc*m.par.Rs)/b.a_Tc ) - 1) - (b.Vmp_Tc + b.Imp_Tc*m.par.Rs)/m.par.Rsh - b.Imp_Tc == 0)
        b.Pmp_Tc = pyo.Expression(expr=b.Vmp_Tc * b.Imp_Tc)                                                          # Dobos eq.25

    nTc = len(temperatures)
    m.gamma = pyo.Block()
    g = m.gamma
    g.i = pyo.RangeSet(nTc)
    g.d_i = pyo.RangeSet(2, nTc)
    g.pt = pyo.Block(g.i, rule=gamma_blocks)

    g.gamma_Tc = pyo.Expression(g.d_i, rule=gamma_expr)
    g.gamma_avg = pyo.Expression(expr=pyo.summation(g.gamma_Tc) / len(g.d_i))

    # f_7: Match modeled gamma to manufacturer-specified gamma_Pmp
    g.f_7 = pyo.Constraint(expr=(g.gamma_avg - m.gPmp) == 0)

    # Sanity checks: verify that the solved parameters reproduce datasheet currents.
    model.sanity = pyo.Block()
    s = model.sanity

    # f_8: Solve for Imp independently at V=Vmp using the single diode eq.
    s.Imp_calc = pyo.Var(domain=pyo.NonNegativeReals)
    s.f_8 = pyo.Constraint(expr=m.par.Il - s.Imp_calc - m.par.Io * (pyo.exp((m.Vmp + s.Imp_calc * m.par.Rs) / m.par.a) - 1.0) - (m.Vmp + s.Imp_calc * m.par.Rs) / m.par.Rsh == 0)

    # f_9: Verify current at Voc is ~0 (should be negligible for a valid solution)
    s.Ioc_calc = pyo.Var(domain=pyo.NonNegativeReals, initialize=0)
    s.f_9 = pyo.Constraint(expr=m.par.Il - s.Ioc_calc - m.par.Io * (pyo.exp((m.Voc + s.Ioc_calc * m.par.Rs) / m.par.a) - 1.0) - (m.Voc + s.Ioc_calc * m.par.Rs) / m.par.Rsh == 0)
    # examine solved modules
    model.scaling_factor = pyo.Suffix(direction=pyo.Suffix.EXPORT)
    return model


def solve_model(model, solver, tee=False):
    """
    Solve the model with scaling factors, multiple tries and separating steps

    Solution may not be optimal! Solutions may have slight infeasibility. 
    Caller needs to check whether it is above an acceptable threshold using the log functions by setting tee=True,
    or after the function, using `get_constraint_infeas` or `get_curve_diffs`.
    """
    model.scaling_factor[model.solver.par.Io] = IL_SCALING
    model.scaling_factor[model.solver.par.Rsh] = RSH_SCALING
    
    scaled_model = pyo.TransformationFactory('core.scale_model').create_using(model)

    scaled_model.obj_zero = pyo.Objective(rule=0)
    res = None
    try:
        res = solver.solve(scaled_model, tee=tee)
    except Exception as e:
        if tee:
            log_infeasible_bounds(scaled_model, logger=solve_log, tol=1e-7)
            log_infeasible_constraints(scaled_model, logger=solve_log, tol=1e-7)
        else:
            return None, scaled_model, 0

    if 'iterations exceeded' in res.solver.message.lower():
        solver.options["max_iter"] = MAX_ITER * 2
        try:
            res = solver.solve(scaled_model, tee=tee)
        except Exception as e:
            if tee:
                log_infeasible_bounds(scaled_model, logger=solve_log, tol=1e-7)
                log_infeasible_constraints(scaled_model, logger=solve_log, tol=1e-7)
            else:
                return None, scaled_model, 0
            
    # Exit status is 'Converged to a point of local infeasibility. Problem may be infeasible.'
    # However, try to see if we can push the solution a little closer to fitting the IV curves before returning the approximate solution
    elif 'locally infeasible' in res.solver.message.lower():
        try:
            res = solver.solve(scaled_model, tee=tee)
        except Exception:
            return None, scaled_model, -1

        if 'locally infeasible' in res.solver.message:
            scaled_model.solver.gamma.deactivate()
            try:
                res = solver.solve(scaled_model, tee=tee)
            except Exception:
                return None, scaled_model, -2

            scaled_model.solver.gamma.activate()
            try:
                res = solver.solve(scaled_model, tee=tee)
            except Exception:
                return None, scaled_model, -3
            if res:
                pyo.TransformationFactory('core.scale_model').propagate_solution(scaled_model, model)
                return res, scaled_model, 2
            else:
                return None, scaled_model, -4

    elif res is None or res.solver.status != 'ok':
        if tee:
            log_infeasible_bounds(scaled_model, logger=solve_log, tol=1e-7)
            log_infeasible_constraints(scaled_model, logger=solve_log, tol=1e-7)
        return None, scaled_model, -5

    pyo.TransformationFactory('core.scale_model').propagate_solution(scaled_model, model)
    return res, scaled_model, 4


def get_iterations(log_file):
    """
    Get the number of IPOPT iterations from the log file
    """
    with open(log_file, 'r') as f:
        for line in f:
            if "Number of Iterations....:" in line:
                it = line.split(": ")[1]
                it_n = int(it)
                return it_n


def get_constraint_infeas(model):
    """
    Get the magnitude of infeasibility for each constraint in the pyomo model. Model can be the original or the scaled version
    """
    if hasattr(model.solver.par, 'f0'):
        vals = (pyo.value(model.solver.par.f0), pyo.value(model.solver.par.f1), pyo.value(model.solver.par.f2), pyo.value(model.solver.par.f3), pyo.value(model.solver.par.f4), pyo.value(model.solver.gamma.f_7))
    else:
        if hasattr(model.solver.gamma, 'f_7'):
            vals = (pyo.value(model.solver.par.scaled_f0), pyo.value(model.solver.par.scaled_f1), pyo.value(model.solver.par.scaled_f2), pyo.value(model.solver.par.scaled_f3), pyo.value(model.solver.par.scaled_f4), pyo.value(model.solver.gamma.f_7))
        else:
            vals = (pyo.value(model.solver.par.scaled_f0), pyo.value(model.solver.par.scaled_f1), pyo.value(model.solver.par.scaled_f2), pyo.value(model.solver.par.scaled_f3), pyo.value(model.solver.par.scaled_f4))
    return [abs(v) for v in vals]


def solve_model_best_solution(model, solver, tee=False):
    """
    Solve the model and return the best solution regardless of regardless of whether IPOPT has converged or exited gracefully

    Solution may not be optimal! Solutions may have a lot of infeasibility. 
    Caller needs to check whether it is above an acceptable threshold using the log functions by setting tee=True,
    or after the function, using `get_constraint_infeas` or `get_curve_diffs`.
    """
    try:
        il_scaling = 10**min(12, -int(np.log10(pyo.value(model.solver.par.Io))))
        rsh_scaling = 10**min(5, -int(np.log10(pyo.value(model.solver.par.Rsh))))
    except Exception:
        il_scaling = IL_SCALING
        rsh_scaling = RSH_SCALING
    model.scaling_factor[model.solver.par.Io] = il_scaling
    model.scaling_factor[model.solver.par.Rsh] = rsh_scaling
    
    scaled_model = pyo.TransformationFactory('core.scale_model').create_using(model)

    if hasattr(scaled_model.solver, "f_0"):
        scaled_model.obj_gamma = pyo.Objective(rule=scaled_model.solver.gamma.f_7 ** 0.5 + scaled_model.solver.f_0 ** 0.5)
    elif hasattr(scaled_model.solver.gamma, "f_7"):
        scaled_model.obj_gamma = pyo.Objective(rule=scaled_model.solver.gamma.f_7 ** 0.5)

    scaled_model.sanity.scaled_f_9.deactivate()
    
    res = None
    try:
        res = solver.solve(scaled_model, tee=tee, logfile='ipopt_output.log')
    except Exception as e:
        pass

    if res is not None and res.solver.status == "ok":
        pyo.TransformationFactory('core.scale_model').propagate_solution(scaled_model, model)
        return res, scaled_model, 1

    it_n = get_iterations('ipopt_output.log')

    solver.options['max_iter'] = it_n - 1             

    try:
        res = solver.solve(scaled_model, tee=tee, logfile='ipopt_output.log')
    except Exception:
        pass

    while 'infeasible' in res.solver.message:
        it_n = get_iterations('ipopt_output.log')
        solver.options['max_iter'] = it_n - 1     
        res = solver.solve(scaled_model, tee=tee, logfile='ipopt_output.log')

    pyo.TransformationFactory('core.scale_model').propagate_solution(scaled_model, model)

    # get somewhat stable params
    infeas = sum(get_constraint_infeas(model))
    attempts = 0
    while infeas > 10 and attempts < 10:
        res = solver.solve(scaled_model, tee=tee, logfile='ipopt_output.log')
        infeas = sum(get_constraint_infeas(scaled_model))
        pyo.TransformationFactory('core.scale_model').propagate_solution(scaled_model, model)
        attempts += 1

    return res, scaled_model, 2


def set_parameters(m, r: pd.Series):
    """
    Set the manufacturer datasheet inputs on the model.solver block.

    These are the STC key points and temperature coefficients from the CEC module
    database that define the right-hand side of the constraint system [Dobos Table 1]:
        Vmp, Imp, Voc, Isc       - I-V curve key points at STC
        alpha_sc                 - Temperature coefficient of Isc [A/K]
        beta_oc                  - Temperature coefficient of Voc [V/K]
        gamma_pmp                - Temperature coefficient of Pmp [%/K]
        T_ref = 298.15 K         - STC cell temperature (25 C)
    """
    try:
        m.Vmp.set_value(r['V_mp_ref'])
        m.Imp.set_value(r['I_mp_ref'])
        m.Voc.set_value(r['V_oc_ref'])
        m.Isc.set_value(r['I_sc_ref'])
        m.aIsc.set_value(r['alpha_sc'])
        m.bVoc.set_value(r['beta_oc'])
        m.gPmp.set_value(r['gamma_pmp'])
        m.Tref.set_value(25 + 273.15)
        return True
    except Exception:
        return False

def set_initial_guess(model, a, Il, Io, Rs, Rsh, Adj):
    """
    Set initial values for the six circuit parameters before solving.

    Good initial guesses are critical for convergence of the nonlinear system.
    """
    m = model.solver
    m.par.a.set_value(a)
    m.par.Il.set_value(Il)
    m.par.Io.set_value(Io)
    m.par.Rs.set_value(Rs)
    m.par.Rsh.set_value(Rsh)
    m.par.Adj.set_value(Adj)
    model.sanity.Imp_calc.set_value(pyo.value(m.Imp))

# intercept, coefficients
Voc_Vmp_to_a = [0.22328699, 0.03106774, 0.00738466]
Isc_Imp_to_Il = [0.0153132, 0.96058469, 0.04133798]
Isc_Imp_Voc_Vmp_to_Rs = [0.28969838, 0.61785177, -0.6759943, 0.02241771, 0.0218622]

def set_empirical_initial_guess(model):
    """
    Set initial guesses using empirical regressions fitted to previously-solved modules.

    Uses linear regressions of datasheet values (Voc, Vmp, Isc, Imp) to predict
    a, I_L, and R_s using coefficients fitted to the solver's own successfully-solved module database.
    """
    m = model.solver

    Vmp = pyo.value(m.Vmp)
    Imp = pyo.value(m.Imp)
    Voc = pyo.value(m.Voc)
    Isc = pyo.value(m.Isc)
    aIsc = pyo.value(m.aIsc)
    bVoc = pyo.value(m.bVoc)
    gPmp = pyo.value(m.gPmp)

    a = Voc_Vmp_to_a[0] + Voc_Vmp_to_a[1] * Voc + Voc_Vmp_to_a[2] * Vmp
    Il = Isc_Imp_to_Il[0] + Isc_Imp_to_Il[1] * Isc + Isc_Imp_to_Il[2] * Imp
    Rs = Isc_Imp_Voc_Vmp_to_Rs[0] + Isc_Imp_Voc_Vmp_to_Rs[1] * Isc + Isc_Imp_Voc_Vmp_to_Rs[2] * Imp \
        + Isc_Imp_Voc_Vmp_to_Rs[3] * Voc + Isc_Imp_Voc_Vmp_to_Rs[4] * Vmp
    
    m.par.a.set_value(a)
    m.par.Il.set_value(Il)
    m.par.Rs.set_value(Rs)
    for i in m.gamma.i:
        m.gamma.pt[i].Vmp_Tc.set_value(Vmp)
        m.gamma.pt[i].Imp_Tc.set_value(Imp)


def find_closest(df_solved: pd.DataFrame, r: pd.Series):
    """
    Find the previously-solved module closest to the target in datasheet-parameter space.

    Uses Euclidean distance across (Vmp, Imp, Voc, Isc, alpha_sc, beta_oc, gamma_pmp)
    to identify the best initial guess donor. 
    """
    df_solved = df_solved.fillna(0)
    r = r.fillna(0)
    diff = ((df_solved['V_mp_ref'] - r['V_mp_ref']) ** 2 + 
            (df_solved['I_mp_ref'] - r['I_mp_ref']) ** 2 + 
            (df_solved['V_oc_ref'] - r['V_oc_ref']) ** 2 + 
            (df_solved['I_sc_ref'] - r['I_sc_ref']) ** 2 +
            (df_solved['alpha_sc'] - r['alpha_sc']) ** 2 + 
            (df_solved['beta_oc'] - r['beta_oc']) ** 2 +
            (df_solved['gamma_pmp'] - r['gamma_pmp']) ** 2)
        
    params_closest = df_solved.iloc[diff.argmin()]
    return params_closest


def get_params_from_model(model):
    """
    Extract the six solved circuit parameters from the Pyomo model.

    Handles both the original and scaled (via core.scale_model) model representations,
    un-doing the Io and Rsh scaling factors applied for numerical conditioning.

    Returns
    -------
    tuple : (a, Il, Io, Rs, Rsh, Adj)
    """
    if hasattr(model.solver.par, 'scaled_a'):
        a = pyo.value(model.solver.par.scaled_a)
        Il = pyo.value(model.solver.par.scaled_Il)
        Io = pyo.value(model.solver.par.scaled_Io / IL_SCALING)
        Rs = pyo.value(model.solver.par.scaled_Rs)
        Rsh = pyo.value(model.solver.par.scaled_Rsh / RSH_SCALING)
        Adj = pyo.value(model.solver.par.scaled_Adj)
    else:
        a = pyo.value(model.solver.par.a)
        Il = pyo.value(model.solver.par.Il)
        Io = pyo.value(model.solver.par.Io)
        Rs = pyo.value(model.solver.par.Rs)
        Rsh = pyo.value(model.solver.par.Rsh)
        Adj = pyo.value(model.solver.par.Adj)
    return a, Il, Io, Rs, Rsh, Adj


def get_curve_diffs(r, model):
    """
    Calculate normalized deviations between the modeled and datasheet I-V curve at STC.

    Computes the I-V curve at STC (1000 W/m^2, 25 C) and measures relative errors
    in Isc, Imp, Vmp, and Pmp against the manufacturer datasheet values. 

    Returns
    -------
    tuple : (d_Isc, d_Imp, d_Vmp, d_Pmp) — fractional deviations (unitless)
    """
    x_V, y_I = cec_model_ivcurve(model, 1000, 25 + 273.15, r['V_oc_ref'], 150)
    p = x_V * y_I
    mp_ind = np.argmax(p)
    d_I_sc = (y_I[0] - r['I_sc_ref']) / r['I_sc_ref']
    d_I_mp = (y_I[mp_ind] - r['I_mp_ref']) / r['I_mp_ref']
    d_V_mp = (x_V[mp_ind] - r['V_mp_ref']) / r['V_mp_ref']
    d_P_mp = (p[mp_ind] - r['V_mp_ref'] * r['I_mp_ref']) / (r['V_mp_ref'] * r['I_mp_ref'])
    return d_I_sc, d_I_mp, d_V_mp, d_P_mp


def read_prepare_file(xlsx_file):
    """
    Read the CEC Module Excel Spreadsheet and prepare it for solving.

    Reads the CEC PV module database (as published by the California Energy
    Commission), drops non-essential columns, renames to SAM conventions,
    and converts alpha_sc and beta_oc from %/C to absolute units (A/K and V/K).
    """

    df = pd.read_excel(xlsx_file, skiprows=list(range(0, 16)) + [17])

    # Clean up data
    df = df.replace(r"\,", "", regex=True)
    df = df.replace(r"\,", "", regex=True)
    df = df.replace(r"\n+|\r+|\t", " ", regex=True)
    df = df.replace(r"[^\x00-\x7F]+", " ", regex=True)
    # Avoid replacing periods in numbers
    df["Manufacturer"] = df["Manufacturer"].replace(r"\.", "", regex=True)
    df["Model Number"] = df["Model Number"].replace(r"\.", "", regex=True)

    # Identify bifacial modules
    description = df["Description"]
    bifacial = []
    for text in description:
        if "bifacial" in str(text).lower():
            bifacial.append(1)
        else:
            bifacial.append(0)

    # Drop columns we don't need for SAM library
    df = df.drop(columns=["Description", 
        "Safety Certification",
        "Notes",
        "Design Qualification Certification\n(Optional Submission)",
        "Performance Evaluation (Optional Submission)",
        "Family", 
        "N_p",
        "αIpmax",
        "βVpmax",
        "IPmax, low",
        "VPmax, low",
        "IPmax, NOCT",
        "VPmax, NOCT",
        "Mounting",
        "Type",
        "Geometric Multiplier",
        "P2/Pref",
        "CEC Listing Date",
        "Last Update"])

    # Rename column labels to match SAM library
    df = df.rename(columns={"Nameplate Isc": "I_sc_ref",
        "Nameplate Voc": "V_oc_ref",
        "Nameplate Ipmax": "I_mp_ref",
        "Nameplate Vpmax": "V_mp_ref",
        "Nameplate Pmax": "STC",
        "Average NOCT": "T_NOCT",
        "γPmax": "gamma_pmp",
        "αIsc": "alpha_sc",
        "βVoc": "beta_oc",
        "Short Side": "Width",
        "Long Side": "Length"})

    # Add bifacial column
    df["Bifacial"] = bifacial

    # Add test data columns
    df[test_data_cols] = df[test_data_cols].astype(float)
    df["alpha_sc"] *= df["I_sc_ref"] * 1e-2
    df["beta_oc"] *= df["V_oc_ref"] * 1e-2

    df = df.drop_duplicates()
    
    # Set empty data to null
    for col in model_param_cols:
        df[col] = None
    for col in solve_tracking_cols:
        df[col] = None
    return df


def _validate_module_row(r):
    """Return an error string if the module row has invalid data, else None."""
    if r['V_mp_ref'] < 0:
        return "Vmp < 0"
    if r['V_oc_ref'] < r['V_mp_ref']:
        return "Voc < Vmp"
    return None


def _save_plot(r, i, plot_output_path):
    """Save the current matplotlib figure with standard formatting."""
    plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
    plt.xlabel("Voltage")
    plt.ylabel("Current")
    plt.title(f"{r['Manufacturer']} {r['Model Number']}")
    plt.tight_layout()
    plt.savefig(plot_output_path / f"IV_curve_{i}.png")
    plt.close()


def _extract_results(r, model, scaled_model, solve_code, plot_output_path=None, i=None):
    """
    Extract solved parameters and IV-curve deviations into the result row.

    Returns the updated row `r`. Sets r['Error'] if infeasibility exceeds threshold.
    """
    r['solve_return_code'] = solve_code

    d_I_sc, d_I_mp, d_V_mp, d_P_mp = get_curve_diffs(r, model)
    diffs = [d_I_sc, d_I_mp, d_V_mp, d_P_mp]

    infeas_sum = np.abs(diffs).sum()
    infeas_max = np.abs(diffs).max()

    if infeas_max > INFEASIBILITY_THRESHOLD or infeas_sum > INFEASIBILITY_THRESHOLD:
        r['Error'] = f"Infeasibility > {INFEASIBILITY_THRESHOLD}"
        if plot_output_path:
            plt.close()
        return r

    a, Il, Io, Rs, Rsh, Adj = get_params_from_model(scaled_model)
    for col, val in zip(iv_diff_cols, diffs):
        r[col] = val
    for col, val in zip(model_param_cols, [a, Il, Io, Rs, Rsh, Adj]):
        r[col] = val
    r['Error'] = None

    if plot_output_path:
        _save_plot(r, i, plot_output_path)
    return r


def run_solve(i, r, gamma_curve_dt, initial_guess_fn, solve_pass_label,
              plot_output_path=None, solver=None, solve_fn=solve_model):
    """
    Solve a single module's six CEC parameters.

    The solving strategy is controlled by the caller via:

    Parameters
    ----------
    i : int
        Row index (used for error messages and plot filenames).
    r : pd.Series
        Module datasheet row (modified in place with results).
    gamma_curve_dt : float
        Temperature step for gamma fitting (smaller = more precise, harder to solve).
    initial_guess_fn : callable(model) -> None
        Sets the initial guess on the model. Examples:
        - ``set_empirical_initial_guess`` for first-pass solves
        - ``lambda m: set_initial_guess(m, *closest[model_param_cols])`` for bootstrapping
    solve_pass_label : str
        Label stored in r['solve_pass'] for tracking which strategy produced the solution.
    plot_output_path : Path or None
        Directory for saving IV-curve plots. None disables plotting.
    solver : pyomo SolverFactory or None
        Reusable IPOPT solver instance. Created if None (costs startup time when
        solving many modules — prefer passing a shared instance).
    solve_fn : callable
        The model-solving function to use (default: ``solve_model``).
    """
    if solver is None:
        solver = pyo.SolverFactory('ipopt')
    solver.options["max_iter"] = MAX_ITER

    r['solve_pass'] = solve_pass_label

    error = _validate_module_row(r)
    if error:
        r['Error'] = error
        return r

    model = create_model(gamma_curve_dt=gamma_curve_dt)
    if not set_parameters(model.solver, r):
        r['Error'] = f"Parameter missing or out of bounds for row {i}"
        return r

    if plot_output_path:
        plt.figure()

    initial_guess_fn(model)

    if plot_output_path:
        plot_iv_curve(model, linestyle=(0, (1, 10)), label="Initial Guess")

    res, scaled_model, solve_code = solve_fn(model, solver, tee=False)

    if plot_output_path:
        if res and res.solver.status == 'ok':
            plot_iv_curve(model, linestyle='-', label="Optimal", plot_anchors=True)
        else:
            plot_iv_curve(model, linestyle='dotted', label="Approx", plot_anchors=True)

    return _extract_results(r, model, scaled_model, solve_code, plot_output_path, i)


def parallel_run(solve_fn, df, plot_output_path=None, extra_args=None):
    """
    Run ``solve_fn`` in parallel across all rows of ``df``.

    Parameters
    ----------
    solve_fn : callable(i, r, *extra_args, plot_output_path) -> pd.Series
        Per-row solve function (e.g. ``_make_first_pass_fn``).
    df : pd.DataFrame
        Rows to solve.
    plot_output_path : Path or None
        Passed through to solve_fn.
    extra_args : list or None
        Additional positional args inserted between (i, r) and plot_output_path.
    """
    extra_args = extra_args or []
    with mp.Pool(NUM_OF_WORKERS) as pool:
        results = [
            pool.apply_async(solve_fn, [idx, row] + extra_args + [plot_output_path])
            for idx, row in df.iterrows()
        ]
        results = [res.get() for res in results]
    return pd.DataFrame(results)


def sequential_run(solve_fn, df, plot_output_path=None, extra_args=None):
    """
    Run ``solve_fn`` sequentially across all rows of ``df``, sharing a single
    IPOPT solver instance for efficiency.
    """
    solver = pyo.SolverFactory('ipopt')
    extra_args = extra_args or []
    results = []
    for i, r in df.iterrows():
        row = solve_fn(i, r, *extra_args, plot_output_path=plot_output_path, solver=solver)
        results.append(row)
    return pd.DataFrame(results)


# --- Top-level solve wrappers (must be picklable for multiprocessing) ---

def solve_first_pass(i, r, plot_output_path=None, solver=None):
    """Solve a single module using empirical initial guesses (first pass)."""
    return run_solve(i, r,
                     gamma_curve_dt=3,
                     initial_guess_fn=set_empirical_initial_guess,
                     solve_pass_label='first_pass',
                     plot_output_path=plot_output_path,
                     solver=solver)


def solve_bootstrapping(i, r, solved_df, plot_output_path=None, solver=None):
    """Solve a single module using bootstrapped initial guesses from previously-solved modules."""
    cec_closest = find_closest(solved_df, r)
    return run_solve(i, r,
                     gamma_curve_dt=3,
                     initial_guess_fn=lambda m: set_initial_guess(m, *cec_closest[model_param_cols]),
                     solve_pass_label='bootstrapping',
                     plot_output_path=plot_output_path,
                     solver=solver)


def solve_bootstrapping_reduced(i, r, solved_df, plot_output_path=None, solver=None):
    """Solve a single module with bootstrapped guesses and reduced gamma temperature sampling."""
    cec_closest = find_closest(solved_df, r)
    return run_solve(i, r,
                     gamma_curve_dt=10,
                     initial_guess_fn=lambda m: set_initial_guess(m, *cec_closest[model_param_cols]),
                     solve_pass_label='bootstrapping_reduced',
                     plot_output_path=plot_output_path,
                     solver=solver)


def solve_approx(solved_df, unsolved_df, plot_output_path=None):
    """
    Solve unsolved modules with reduced gamma sampling and ``solve_model_best_solution``.

    Not recommended unless an approximate solution is acceptable.
    Accuracy of IV curve to test data should be examined visually via
    ``create_model_with_solution`` and ``plot_iv_curve``.
    """
    solver = pyo.SolverFactory('ipopt')
    results = []
    for i, r in unsolved_df.iterrows():
        cec_closest = find_closest(solved_df, r)
        row = run_solve(i, r,
                        gamma_curve_dt=15,
                        initial_guess_fn=lambda m, cc=cec_closest: set_initial_guess(m, *cc[model_param_cols]),
                        solve_pass_label='approx',
                        plot_output_path=plot_output_path,
                        solver=solver,
                        solve_fn=solve_model_best_solution)
        results.append(row)
    return pd.DataFrame(results)


def create_model_with_solution(sol_row):
    """
    Create a model pre-populated with a known solution for post-hoc analysis.

    Useful for plotting I-V curves or inspecting constraint residuals of a
    previously-solved module. The model is not solved — the six parameters are
    set directly from the input row.

    Parameters
    ----------
    sol_row : pd.Series or pd.DataFrame
        Must contain columns from both `test_data_cols` and `model_param_cols`.
    """
    model = create_model(gamma_curve_dt=3)
    if isinstance(sol_row, pd.Series):
        if not set_parameters(model.solver, sol_row):
            raise RuntimeError("Test data parameters could not be set")
        set_initial_guess(model, *sol_row[model_param_cols])
    elif isinstance(sol_row, pd.DataFrame):
        if not set_parameters(model.solver, sol_row.to_dict('records')[0]):
            raise RuntimeError("Test data parameters could not be set")
        set_initial_guess(model, *sol_row[model_param_cols].values[0])
    return model


def create_sam_library_file(df, date, version, filename_date):
    """
    Export a dataframe of solved module parameters to a CSV file in the SAM library format.

    Parameters
    ----------
    df : pd.DataFrame
        Solved module parameters from the solver.
    date : str
        Date string for the "Date" column.
    version : str
        SAM version string for the "Version" column.
    filename_date : str
        Date string used in the output filename.
    """

    sam_library_df = df.copy()
    sam_library_df = sam_library_df.rename(columns={"a_py": "a_ref",
        "Il_py": "I_L_ref",
        "Io_py": "I_o_ref",
        "Rs_py": "R_s",
        "Rsh_py": "R_sh_ref",
        "Adj_py": "Adjust"})

    sam_library_df.insert(0,
        "Name",
        sam_library_df["Manufacturer"].astype(str) + " " + sam_library_df["Model Number"].astype(str))

    sam_library_df = sam_library_df.drop(columns=["Model Number",
        "d_Isc",
        "d_Imp",
        "d_Vmp",
        "d_Pmp",
        "Error"])

    sam_library_df["Version"] = version
    sam_library_df["Date"] = date

    headers = {"Name": ["Units", "[0]"],
        "Manufacturer": ["", "lib_manufacturer"],
        "Technology": ["", "cec_material"],
        "Bifacial": ["", "lib_is_bifacial"],
        "STC": ["", ""],
        "PTC": ["", ""],
        "A_c": ["m2", "cec_area"],
        "Length": ["m", "lib_length"],
        "Width": ["m", "lib_width"],
        "N_s": ["", "cec_n_s"],
        "I_sc_ref": ["A", "cec_i_sc_ref"],
        "V_oc_ref": ["V", "cec_v_oc_ref"],
        "I_mp_ref": ["A", "cec_i_mp_ref"],
        "V_mp_ref": ["V", "cec_v_mp_ref"],
        "alpha_sc": ["A/K", "cec_alpha_sc"],
        "beta_oc": ["V/K", "cec_beta_oc"],
        "T_NOCT": ["C", "cec_t_noct"],
        "a_ref": ["V", "cec_a_ref"],
        "I_L_ref": ["A", "cec_i_l_ref"],
        "I_o_ref": ["A", "cec_i_o_ref"],
        "R_s": ["Ohm", "cec_r_s"],
        "R_sh_ref": ["Ohm", "cec_r_sh_ref"],
        "Adjust": ["%", "cec_adjust"],
        "gamma_pmp": ["%/K", "cec_gamma_pmp"],
        "BIPV": ["", ""],
        "Version": ["", ""],
        "Date": ["", ""]}
    
    header_df = pd.DataFrame.from_dict(headers)
    sam_library_df = pd.concat([header_df, sam_library_df])
    sam_library_df.to_csv(f"CEC Modules {filename_date}.csv", index=False)


def main():
    run_parallel = True 
    create_SAM_library = True
    plot_output_path = None  # Set to a Path to enable IV-curve plots

    if not pyo.SolverFactory('ipopt').available():
        raise RuntimeError("IPOPT solver not available. Install it to your Python environment from conda: `conda install -c conda-forge ipopt`")

    if len(sys.argv) > 1:
        filename = Path(sys.argv[1])

    if not filename.exists():
        raise RuntimeError(f"CEC Module Excel Spreadsheet file path does not exist. {filename}")
    
    filename_date = filename.stem.split("-")
    filename_date = f"{filename_date[-3]}-{filename_date[-2]}-{filename_date[-1]}"
    all_cec_modules_df = read_prepare_file(filename)

    if plot_output_path:
        plot_output_path.mkdir(parents=True, exist_ok=True)

    run_fn = parallel_run if run_parallel else sequential_run

    print(f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}: Solving for {len(all_cec_modules_df)} Modules")

    # Pass 1: empirical initial guesses
    print(f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}: Starting First Pass Solve")
    all_cec_modules_df = run_fn(solve_first_pass, all_cec_modules_df, plot_output_path)
    all_cec_modules_df.to_csv(f"cec_modules_params_{filename_date}.csv", index=False)

    solved_df = all_cec_modules_df[~all_cec_modules_df['Rsh_py'].isna()]
    unsolved_df = all_cec_modules_df[all_cec_modules_df['Rsh_py'].isna()]
    print(f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}: Solved {len(solved_df)}. {len(unsolved_df)} remaining.")

    # Pass 2: bootstrapped from closest solved module
    print(f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}: Starting Second Pass Solve")
    df = run_fn(solve_bootstrapping, unsolved_df, plot_output_path, extra_args=[solved_df])
    all_cec_modules_df = pd.concat([solved_df, df]).sort_index()
    all_cec_modules_df.to_csv(f"cec_modules_params_{filename_date}.csv", index=False)

    solved_df = all_cec_modules_df[~all_cec_modules_df['Rsh_py'].isna()]
    unsolved_df = all_cec_modules_df[all_cec_modules_df['Rsh_py'].isna()]
    print(f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}: Solved {len(solved_df)}. {len(unsolved_df)} remaining.")

    # Pass 3: bootstrapped with reduced gamma temperature sampling
    print(f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}: Starting Final Pass Solve")
    df = run_fn(solve_bootstrapping_reduced, unsolved_df, plot_output_path, extra_args=[solved_df])
    all_cec_modules_df = pd.concat([solved_df, df]).sort_index()
    all_cec_modules_df.to_csv(f"cec_modules_params_{filename_date}.csv", index=False)

    solved_df = all_cec_modules_df[~all_cec_modules_df['Rsh_py'].isna()]
    unsolved_df = all_cec_modules_df[all_cec_modules_df['Rsh_py'].isna()]
    print(f"{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}: Solved {len(solved_df)}. {len(unsolved_df)} unsolved.")

    if create_SAM_library:
        create_sam_library_file(solved_df, "6/23/2025", "2025.7.3", filename_date)


if __name__ == "__main__":
    main()
    