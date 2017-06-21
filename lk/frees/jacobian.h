
template< typename Real, typename F >
void jacobian( Real *x, Real *f, Real **J, const int n, const int m, F &func, Real epsrel )
{
	// create copy of x input vector
	Real *x1 = new Real[n];
	for ( int j=0; j < n; j++ )
		x1[j] = x[j];
	
	// storage for result of calculating function at x1
	Real *f1 = new Real[m];
	
	for ( int j=0 ; j < n; j++ )
	{
		Real xj = x[j];
		Real dx = epsrel * fabs( xj );
		
		if (dx == 0)
			dx = epsrel;
		
		// overwrite j position with forward difference value
		x1[j] = xj + dx;
		dx = x1[j] - xj; // trick from NR to reduce finite precision error, esp with float or double
		
		func( x1, f1, n );
		
		// replace original xj value
		x1[j] = xj;
		
		// compute forward difference derivate for each row at current column
		// i.e. Jij = dFi / dxj
		for ( int i=0; i < m; i++ )
			J[i][j] = ( f1[i] - f[i] ) / dx;
	}

	delete [] x1;
	delete [] f1;
}