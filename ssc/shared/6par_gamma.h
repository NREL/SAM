
template< typename Real >
class __PTnonlinear
{
private:
	Real Tc, Io, Il, a, Rs, Rsh;
	Real Vguess, Iguess;		

public:
	__PTnonlinear( Real _T, Real _Io, Real _Il, Real _a, Real _aIsc, Real _Adj, Real _V, Real _I, Real _Rs, Real _Rsh, Real Egref, Real Tref )
	{	
		Tc = _T+273.15;
		
		Rs = _Rs;
		Rsh = _Rsh;
		
		// initial conditions
		Vguess = _V;
		Iguess = _I;
		
		// calculate 3 temp dependent parameters		
		Real Eg = (1-0.0002677*(Tc-Tref))*Egref;
			
		a = _a*Tc/Tref;
		Io = _Io* pow( Tc/Tref, 3)*exp( 11600 * (Egref/Tref - Eg/Tc));
		Il = _Il + (_aIsc*(1-_Adj/100))*(Tc-Tref);
	}
	
	void operator() ( const Real x[2], Real f[2] )
	{	
		Real V = x[0];
		Real I = x[1];
		
		f[0] = V*( Io/a*exp( (V+I*Rs)/a ) + 1/Rsh ) / ( 1 + Rs/Rsh + Io*Rs/a*exp( (V+I*Rs)/a ) ) - I;
		f[1] = Il - Io*(exp( (V+I*Rs)/a ) - 1) - (V + I*Rs)/Rsh - I;
	}
	
	int solve( Real *Pt )
	{
		Real x[2], resid[2];
		x[0] = Vguess;
		x[1] = Iguess;
		
		bool check = false;
		int niter = newton<Real, __PTnonlinear, 2>(x, resid, check, *this, 100, 1e-7, 1e-7, 0.9);
		if (check) niter = -5;
		
		if (niter >= 0 && check == false)
		{
			*Pt = x[0] * x[1]; // calculate power (P=V*I)
			return niter;
		}
		else
			return niter;
	}
};

template< typename Real >
bool mod6par_gamma_approx( Real *result, 
	Real Io, Real Il, Real a, 
	Real aIsc, Real Adj, Real Vmp, Real Imp, Real Rs, Real Rsh, Real Egref, Real Tref )
{
	Real Tc_last=-999, Pmax_last=-999;
	Real gamma, gamma_sum = 0.0;
	int nsum = 0;
	float nfail = 0;
	float ntotal = 0;
	
	for (Real Tc = -10; Tc <= 50.0; Tc += 3.0 )
	{
		__PTnonlinear<Real> power_t( Tc,
			Io, Il, a, aIsc, Adj, Vmp, Imp, Rs, Rsh, Egref, Tref );
		
		Real Pmax = 0.0;
		int niter = power_t.solve( &Pmax );
		if ( niter >= 0 )
		{
			if (Tc_last != -999)
			{
				gamma = (Pmax-Pmax_last)*100/(Vmp*Imp*(Tc-Tc_last));
				gamma_sum += gamma;
				nsum ++;
			}
			
			Tc_last = Tc;
			Pmax_last = Pmax;
		}
		else
		{
			nfail++;
		}
		
		ntotal++;		
	}
	
	if (nsum > 2)
		*result = gamma_sum / nsum;
			
	return ( nsum > 2 && nfail/ntotal < 0.3f ); // accept 1/3 of solutions fail over course of Tc iterations
}
