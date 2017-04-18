

template< typename Real, typename F, int n >
int newton( Real x[n], Real residual[n], bool &check, F &func, 
	int MAXITER, const Real TOLF, const Real TOLMIN, const Real STPMX,
	bool (*notify)(int iter, Real x[], Real resid[], const int, void *) = 0,
	void *notify_data = 0)
{
	const Real TOLX = std::numeric_limits<Real>::epsilon();
	
	int i,j,its;
	Real den,f,fold,stpmax,sum,temp,test;
	Real g[n],p[n],xold[n];
	Real fjac[n][n];
	
	Real lu[n][n];
	int permute[n];
		
	f = fminsum<Real, F, n>(x, residual, func);
	test=0.0;
	for (i=0;i<n;i++)
		if (fabs(residual[i]) > test) 
			test=fabs(residual[i]);
		
	if (test < 0.01*TOLF)
	{
		check = false;
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
				return -3;
		}

		jacobian<Real, F, n, n>( x, residual, fjac, func, 1e-8 );
		
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
		
				
		if (!lu_decomp<Real, n>( fjac, lu, permute )) return false;			
		lu_solve<Real, n>( lu, permute, p, p );
		
		if (!search<Real, F, n>(xold, fold, g, p, x, f, stpmax, check, func, residual))
			return -2;
		
		test=0.0;
		for (i=0;i<n;i++)
			if (fabs(residual[i]) > test)
				test=fabs(residual[i]);
				
		if (test < TOLF)
		{
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
			return its+1;
		}
		
		test=0.0;
		for (i=0;i<n;i++) {
			temp=(fabs(x[i]-xold[i]))/mymax(fabs(x[i]),1.0);
			if (temp > test) test=temp;
		}
		
		if (test < TOLX)
			return its+1;
	}
	
	return -1;
}
