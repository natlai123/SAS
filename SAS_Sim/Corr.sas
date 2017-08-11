/***********************************************************************
 Chapter 10: Building Correlation and Covariance Matrices
 ***********************************************************************/
proc iml;

/********************************************************************
 Converting between Correlation and Covariance Matrices
 *******************************************************************/

/* convert a covariance matrix, S, to a correlation matrix */
start Cov2Corr(S);
   D = sqrt(vecdiag(S));
   return( S / D` / D );        /* divide columns, then divide rows */
finish;

/* R = correlation matrix
   sd = (vector of) standard deviations for each variable 
   Return covariance matrix with sd##2 on the diagonal */
start Corr2Cov(R, sd);
   std = colvec(sd);                  /* convert to a column vector */
   return( std` # R # std );
finish;

/********************************************************************
 Testing Whether a Matrix Is a Covariance Matrix
 *******************************************************************/

/* finite-precision test of whether a matrix is symmetric */
start SymCheck(A);
   B = (A + A`)/2;
   scale = max(abs(A));
   delta = scale * constant("SQRTMACEPS");
   return( all( abs(B-A)< delta ) );
finish;

/* Add a multiple of diag(A) so that A is diagonally dominant. */
start Ridge(A, scale);         /* Input scale >= 1                  */
   d = vecdiag(A);
   s = abs(A)[,+] - d;         /* sum(abs of off-diagonal elements) */
   lambda = scale * (max(s/d) - 1); 
   return( A + lambda*diag(d) );
finish;

/* Return NumSamples x (N*N) matrix. Each row contains an N x N 
   symmetric PD matrix. The scale parameter is used for the ridging.*/
start RandSymUsingRidge(NumSamples, N, scale);
   Y = j(NumSamples, N##2);            /* allocate return values    */
   v = j(NumSamples, N*(N+1)/2);       /* allocate lower triangular */
   call randgen(v, "Uniform");         /* fill with random          */
   do i = 1 to NumSamples;
      A = sqrvech(v[i,]);
      B = Ridge(A, scale);
      Y[i,] = shape(B, 1);             /* pack matrix in row of Y   */
   end;
   return( Y );
finish;

/********************************************************************
 Generating a Covariance Matrix with a Known Structure
 *******************************************************************/

/* variance components: diag({var1, var2,..,varN}), var_i>0 */
start VarComp(v);
   return( diag(v) );
finish;

/* compound symmetry, v>0:
   {v+v1    v1    v1    v1,
      v1  v+v1    v1    v1,
      v1    v1  v+v1    v1,
      v1    v1    v1  v+v1  };
*/   
start CompSym(N, v, v1);
   return( j(N,N,v1) + diag( j(N,1,v) ) );
finish;

/* AR1 is special case of Toeplitz */
/* autoregressive(1):
   s##2 * {1      rho    rho##2 rho##3,
           rho    1      rho    rho##2,
           rho##2 rho    1      rho   ,
           rho##3 rho##2 rho    1     }; 
   Let u = {rho rho##2 rho##3} 
*/
start AR1(N, s, rho);
   u = cuprod(j(1,N-1,rho));                  /* cumulative product */
   return( s##2 # toeplitz(1 || u) );
finish;


/********************************************************************
 The Nearest Correlation Matrix
 *******************************************************************/

/* Project symmetric X onto S={positive semidefinite matrices}.
   Replace any negative eigenvalues of X with zero */
start ProjS(X);
   call eigen(D, Q, X);               /* notice that X = Q*D*Q`     */
   V = choose(D>0, D, 0);
   W = Q#sqrt(V`);                    /* form Q*diag(V)*Q`          */
   return( W*W` );                    /* W*W` = Q*diag(V)*Q`        */
finish;

/* project square X onto U={matrices with unit diagonal}.
   Return X with the diagonal elements replaced by ones. */
start ProjU(X);
   n = nrow(X);
   Y = X;
   Y[do(1, n*n, n+1)] = 1;            /* set diagonal elements to 1 */
   return ( Y );
finish;

/* the matrix infinity norm is the max abs value of the row sums */
start MatInfNorm(A);
   return( max(abs(A[,+])) );
finish;

/* Given a symmetric matrix, A, project A onto the space of PSD 
   matrices. The function uses the algorithm of Higham (2002) to 
   return the matrix X that is closest to A in the Frobenius norm.  */
start NearestCorr(A);
   maxIter = 100; tol  = 1e-8;        /* initialize parameters      */
   iter = 1;      maxd = 1;           /* initial values             */ 
   Yold = A;  Xold = A;  dS = 0;

   do while( (iter <= maxIter) & (maxd > tol) );
     R = Yold - dS;                   /* dS is Dykstra's correction */
     X = ProjS(R);                    /* project onto S={PSD}       */
     dS = X - R;
     Y = ProjU(X);                    /* project onto U={Unit diag} */

     /* How much has X changed? (Eqn 4.1) */
     dx = MatInfNorm(X-Xold) / MatInfNorm(X);
     dy = MatInfNorm(Y-Yold) / MatInfNorm(Y);
     dxy = MatInfNorm(Y - X) / MatInfNorm(Y);
     maxd = max(dx,dy,dxy);
     iter = iter + 1; 
     Xold = X;  Yold = Y;             /* update matrices            */
   end;
   return( X );                       /* X is positive semidefinite */
finish;

store module=_all_;
quit;


