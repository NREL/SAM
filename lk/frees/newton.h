
template< typename Real > Real mymax( Real a, Real b ) { return a > b ? a : b; }

template< typename Real, typename F >
int newton( Real *x, Real *residual, const int n, bool &check, F &func, 
	int MAXITER, const Real TOLF, const Real TOLMIN, const Real STPMX,
	bool (*notify)(int iter, Real *, Real *, const int, void *) = 0,
	void *notify_data = 0)
{
	const Real TOLX = std::numeric_limits<Real>::epsilon();
	
	int i,j,its;
	Real den,f,fold,stpmax,sum,temp,test;
	Real *g = new Real[n];
	Real *p = new Real[n];
	Real *xold = new Real[n];
	Real **fjac = new Real*[n];	
	Real **lu = new Real*[n];
	for (i=0;i<n;i++)
	{
		fjac[i] = new Real[n];
		lu[i] = new Real[n];
	}

	int *permute = new int[n];

#define NEWT_FREEMEM() { delete [] g; delete [] p; delete [] xold; delete [] permute; for (int k=0;k<n;k++) { delete [] fjac[k]; delete [] lu[k]; } delete [] fjac; delete [] lu; }
		
	f = fminsum<Real, F>(x, residual, n, func);
	test=0.0;
	for (i=0;i<n;i++)
		if (fabs(residual[i]) > test) 
			test=fabs(residual[i]);
		
	if (test < 0.01*TOLF)
	{
		check = false;
		NEWT_FREEMEM();
		return 0;
	}
	
	sum=0.0;
	for (i=0;i<n;i++)
		sum += x[i]*x[i];
		
	stpmax = STPMX*mymax(sqrt(sum), (Real)n);
	for (its=0;its<MAXITER;its++)
	{
		if ( notify != 0 )
		{
			bool ok = (*notify)(its, x, residual, n, notify_data);
			if (!ok)
			{
				NEWT_FREEMEM();
				return -3;
			}
		}

		jacobian<Real, F>( x, residual, fjac, n, n, func, 1e-8 );
		
		for (i=0;i<n;i++) 
		{
			sum=0.0;
			for (j=0;j<n;j++) sum += fjac[j][i]*residual[j];
			g[i]=sum;
		}
		
		for (i=0;i<n;i++)
			xold[i]=x[i];
			
		fold=f;
		
		for (i=0;i<n;i++)
			p[i] = -residual[i];
		
				
		if (!lu_decomp<Real>( fjac, lu, permute, n )) { NEWT_FREEMEM(); return false; }
		lu_solve<Real>( lu, permute, p, p, n );
		
		if (!search<Real, F>(xold, fold, g, p, x, f, stpmax, check, func, residual, n))
		{
			NEWT_FREEMEM();
			return -2;
		}
		
		test=0.0;
		for (i=0;i<n;i++)
			if (fabs(residual[i]) > test)
				test=fabs(residual[i]);
				
		if (test < TOLF)
		{
			NEWT_FREEMEM();
			check=false;
			return its+1;
		}
		
		if (check) {
			test=0.0;
			den=mymax(f,0.5*n);
			for (i=0;i<n;i++) {
				temp=fabs(g[i])*mymax(fabs(x[i]),1.0)/den;
				if (temp > test) test=temp;
			}
			check=(test < TOLMIN) ? true : false;
			NEWT_FREEMEM();
			return its+1;
		}
		
		test=0.0;
		for (i=0;i<n;i++) {
			temp=(fabs(x[i]-xold[i]))/mymax(fabs(x[i]),1.0);
			if (temp > test) test=temp;
		}
		
		if (test < TOLX)
		{
			NEWT_FREEMEM();
			return its+1;
		}
	}
#undef NEWT_FREEMEM
	return -1;
}