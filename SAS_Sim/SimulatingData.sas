/***********************************************************************
 This file contains all SAS/IML function modules in
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 This program saves functions to the SAS/IML storage library.
 To use these functions in a SAS/IML program, run this program
 and then load the functions, as follows:

 %include "C:\<path>\SimulatingData.sas";
 proc iml;
 load module=_all_;
 <...call the functions...>

 ***********************************************************************/
/*
This file is a concatenation of the following files:
MV.sas           - functions from Chapter 8: Simulating Data from Basic Multivariate Distributions
RandMVBinary.sas - functions from Chapter 9: Advanced Simulation of Multivariate Data that generate multivariate binary variables
Corr.sas         - functions from Chapter 10: Building Correlation and Covariance Matrices
Spatial.sas      - functions from Chapter 14: Simulating Data from Spatial Models
Boot.sas         - functions from Chapter 15: Resampling and Bootstrap Methods
Utility.sas      - functions from Appendix A: A SAS/IML Primer 
RandMVOrd.sas    - functions from Appendix B: Generating Multivariate Ordinal Variables
RandCorr.sas     - functions from Appendix C: Random Correlation Matrices
RandFleishman.sas- functions from Appendix D: Functions for Simulating Data by Using Fleishman's Transformation 
*/


/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 8: Simulating Data from Basic Multivariate Distributions
 ***********************************************************************/


proc iml;

/* Given a p-dimensional MVN distribution and p-k fixed values for
   the variables x_{k+1},...,x_p, return the conditional mean and
   covariance for first k variables, conditioned on the last p-k 
   variables. The conditional mean is returned as a column vector. */
start CondMVNMeanCov(m, S, _mu, Sigma, _v);
   mu = colvec(_mu);  v = colvec(_v);
   p = nrow(mu);      k = p - nrow(v);

   mu1 = mu[1:k]; 
   mu2 = mu[k+1:p]; 
   Sigma11 = Sigma[1:k, 1:k];
   Sigma12 = Sigma[1:k, k+1:p]; *Sigma21 = T(Sigma12);
   Sigma22 = Sigma[k+1:p, k+1:p];
   m = mu1 + Sigma12*solve(Sigma22, (v - mu2));
   S = Sigma11 - Sigma12*solve(Sigma22, Sigma12`);
finish;

 /* Given a p-dimensional MVN distribution and p-k fixed values
   for the variables x_{k+1},...,x_p, simulate first k 
   variables conditioned on the last p-k variables. */
start CondMVN(N, mu, Sigma, v);
   run CondMVNMeanCov(m, S, mu, Sigma, v);
   return( RandNormal(N, m`, S) );              /* m` is row vector */
finish;

/* Sample from a multivariate Cauchy distribution */
start RandMVCauchy(N, p);
   z = j(N,p,0);  y = j(N,1);         /* allocate matrix and vector */
   call randgen(z, "Normal"); 
   call randgen(y, "Gamma", 0.5);     /* alpha=0.5, unit scale      */
   return( z / sqrt(2*y) );
finish;


/********************************************************************
 Reordering Multivariate Data: The Iman-Conover Method
 *******************************************************************/

/* Use Iman-Conover method to generate MV data with known marginals
   and known rank correlation. */
start ImanConoverTransform(Y, C);
   X = Y; 
   N = nrow(X);
   R = J(N, ncol(X));
   /* compute scores of each column */
   do i = 1 to ncol(X);
      h = quantile("Normal", rank(X[,i])/(N+1) );
      R[,i] = h;
   end;
   /* these matrices are transposes of those in Iman & Conover */
   Q = root(corr(R)); 
   P = root(C); 
   S = solve(Q,P);                      /* same as  S = inv(Q) * P; */
   M = R*S;             /* M has rank correlation close to target C */

   /* reorder columns of X to have same ranks as M.
      In Iman-Conover (1982), the matrix is called R_B. */
   do i = 1 to ncol(M);
      rank = rank(M[,i]);
      tmp = X[,i];       /* TYPO in first edition */
      call sort(tmp);
      X[,i] = tmp[rank];
   end;
   return( X );
finish;

store module=_all_;
quit;

/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 9: Advanced Simulation of Multivariate Data
 ***********************************************************************/

proc iml;

/* Bisection: find root on bracketing interval [a,b]. 
    If x0 is the true root, find c such that 
    either |x0-c| < dx or |f(c)| < dy. 
    You could pass dx and dy as parameters. */
start Bisection(a, b);
   dx = 1e-6; dy = 1e-4;
   do i = 1 to 100;                               /* max iterations */
      c = (a+b)/2;
      if abs(MVBFunc(c)) < dy | (b-a)/2 < dx then 
         return(c);
      if MVBFunc(a)#MVBFunc(c) > 0 then a = c;
      else b = c;
   end;
   return (.);                                    /* no convergence */
finish;


/********************************************************************
 Generating Multivariate Binary Variates
 *******************************************************************/

/* Let X1, X2,...,Xd be binary variables, let 
   p = (p1,p2,...,pd) the their expected values and let
   Delta be the d x d matrix of correlations.
   This function returns 1 if p and Delta are feasible for binary 
   variables. The function also computes lower and upper bounds on the
   correlations, and returns them in LBound and UBound, respectively */
start CheckMVBinaryParams(LBound, UBound, _p, Delta);
   p = rowvec(_p);    q = 1 - p;         /* make p a row vector     */
   d = ncol(p);                          /* number of variables     */

   /* 1. check range of Delta; make sure p and Delta are feasible   */
   PP = p`*p;        PQ = p`*q;
   QP = q`*p;        QQ = q`*q;
   A = -sqrt(PP/QQ); B = -sqrt(QQ/PP);   /* matrices                */
   LBound = choose(A>B,A,B);             /* elementwise max(A or B) */
   LBound[loc(I(d))] = 1;                /* set diagonal to 1       */
   A =  sqrt(PQ/QP); B =  sqrt(QP/PQ);
   UBound = choose(A<B,A,B);             /* min(A or B)             */
   UBound[loc(I(d))] = 1;                /* set diagonal to 1       */

   /* return 1 <==> specified  means and correlations are feasible  */
   return( all(Delta >= LBound) & all(Delta <= UBound) );
finish;


/***********************************************************************/

/* Objective: Find correlation, rho, that is zero of this function.
   Global variables:
   pj = prob of success for binary var Xj
   pk = prob of success for binary var Xk
   djk = target correlation between Xj and Xk    */
start MVBFunc(rho)   global(pj, pk, djk);
   Phi = probbnrm(quantile("Normal",pj), quantile("Normal",pk), rho);
   qj = 1-pj; qk = 1-pk;
   return( Phi - pj*pk - djk*sqrt(pj*qj*pk*qk) );
finish;

/***********************************************************************/

start RandMVBinary(N, p, Delta) global(pj, pk, djk);
   /* 1. Check parameters. Compute lower/upper bounds for all (j,k) */
   if ^CheckMVBinaryParams(LBound, UBound, p, Delta) then do;
      print "The specified correlation is invalid." LBound Delta UBound;
      STOP;
   end;

   q = 1 - p;  
   d = ncol(Delta);                          /* number of variables  */

   /* 2. Construct intermediate correlation matrix by solving the 
         bivariate CDF (PROBBNRM) equation for each pair of vars */
   R = I(d);
   do j = 1 to d-1;
      do k = j+1 to d;
         pj=p[j]; pk=p[k]; djk = Delta[j,k];      /* set global vars */
         /* TYPO in first edition: search for root on [-1,1] */
         *R[j,k] = bisection(-1, 1);               /* pre-12.1 */
         R[j,k] = froot("MVBFunc", {-1 1});       /* 12.1 */
         R[k,j] = R[j,k];
      end;
   end;

   /* 3: Generate MV normal with mean 0 and covariance R */
   X = RandNormal(N, j(1,d,0), R);
   /* 4: Obtain binary variable from normal quantile */
   do j = 1 to d;
      X[,j] = (X[,j] <= quantile("Normal", p[j])); /* convert to 0/1 */
   end;
   return (X);
finish;

store module=_all_;
quit;

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


/***********************************************************************
 Chapter 14: Simulating Data from Spatial Models
 ***********************************************************************/

proc iml;

/********************************************************************
 Simulating Data from a Gaussian Random Field
 *******************************************************************/

/* Define Gaussian covariance function V(s). Each row of s is a point. 
   c0 = scale;  a0=range;  s is n x p matrix */
start GaussianCov(c0, a0, s);
   h = distance(s);          /* n x n matrix of pairwise distances  */
   return ( c0#exp( -(h##2/a0##2) ) );
finish;


/* Simulate GRF unconditionally. Each row of s is a point.
   s is n x p matrix of n points in p-dimensional space */
start UncondSimGRF(NumSamples, s, param);
   mean = param[1]; scale=param[2]; range=param[3];
   C = GaussianCov(scale, range, s);
   return( RandNormal( NumSamples, j(1,nrow(s),mean), C) );
finish;


/* Given a p-dimensional MVN distribution and p-k fixed values for
   the variables x_{k+1},...,x_p, return the conditional mean and
   covariance for first k variables, conditioned on the last p-k 
   variables. The conditional mean is returned as a column vector. */
start CondMVNMeanCov(m, S, _mu, Sigma, _v);
   mu = colvec(_mu);  v = colvec(_v);
   p = nrow(mu);      k = p - nrow(v);

   mu1 = mu[1:k]; 
   mu2 = mu[k+1:p]; 
   Sigma11 = Sigma[1:k, 1:k];
   Sigma12 = Sigma[1:k, k+1:p]; *Sigma21 = T(Sigma12);
   Sigma22 = Sigma[k+1:p, k+1:p];
   m = mu1 + Sigma12*solve(Sigma22, (v - mu2));
   S = Sigma11 - Sigma12*solve(Sigma22, Sigma12`);
finish;


/* Given a p-dimensional MVN distribution and p-k fixed values
   for the variables x_{k+1},...,x_p, simulate first k 
   variables conditioned on the last p-k variables. */
start CondMVN(N, mu, Sigma, v);
   run CondMVNMeanCov(m, S, mu, Sigma, v);
   return( RandNormal(N, m`, S) );            /* mean is row vector */
finish;


/* Move the k columns specified by idx to the end of y */
start MoveColsToEnd(y, idx);
   i = setdif(1:ncol(y), idx);
   return( y[ , i||idx] );
finish;


/* Move last k cols of y to positions specified by idx, k=ncol(idx) */
start ReorderLastCols(y, idx);
   p = ncol(y);   k = p - ncol(idx);
   i = setdif(1:p, idx);              /* indices of other columns   */
   v = j(nrow(y),p);                  /* result vector              */
   v[,idx] = y[ ,k+1:p];              /* last k columns move to idx */
   v[,i] = y[ ,1:k];                  /* others interspersed        */
   return( v );
finish;

/********************************************************************
 Simulating Data from a Homogeneous Poisson Process
 *******************************************************************/

/* simulate n points uniformly and independently on [0,a]x[0,b] */
start Uniform2d(n, a, b);
   u = j(n, 2);
   call randgen(u, "Uniform");
   return( u # (a||b) );                    /* scale to [0,a]x[0,b] */
finish;

start HomogPoissonProcess(lambda, a, b);
   n = rand("Poisson", lambda*a*b);
   return( Uniform2d(n, a, b) );
finish;


/********************************************************************
 Simulating Data from an Inhomogeneous Poisson Process
 *******************************************************************/

/* intensity of an inhomogeneous Poisson process */
start Intensity(x,y);
   return( 100/((x+y)##2 +1) );
finish;

/* simulate inhomogeneous Poisson process 
   lambda0 is intensity of underlying homogeneous Poisson process */
start InhomogPoissonProcess(lambda0, a, b);
   u = HomogPoissonProcess(lambda0, a, b);
   lambda = Intensity(u[,1], u[,2]);
   r = rand("Bernoulli", lambda/lambda0);
   return( u[loc(r),] );   
finish;

store module=_all_;
quit;


/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 15: Resampling and Bootstrap Methods
 ***********************************************************************/

proc iml;

/********************************************************************
 Resampling Univariate Data with SAS/IML Software
 *******************************************************************/

/* Random sampling with replacement and uniform probability.
   Input: A is an input vector. 
   Output: (n x k) matrix of random values from A. */
start SampleReplace(A, n, k);
   r = j(n, k);                          /* allocate result matrix  */
   call randgen(r, "Uniform");           /* fill with random U(0,1) */
   r = ceil(nrow(A)*ncol(A)*r);          /* integers 1,2,...,ncol(A)*/
   return(shape(A[r], n));               /* reshape and return      */
finish;


start Skewness(X);
   /* Compute sample skewness for columns of X */
   n = countn(x, "col");
   c = x - mean(x);
   k2 = (c##2)[+,] / (n-1);              /* variance = k2           */
   k3 = (c##3)[+,] # n / ((n-1)#(n-2));
   skew = k3 / k2##1.5;
   return( skew );
finish;

start Kurtosis(X);
   /* Compute sample (excess) kurtosis for columns of X */
   n = countn(x, "col");
   c2 = (x - mean(x))##2;
   m2 = c2[+,]/n;       /* 2nd sample central moment of each column */
   m4 = (c2##2)[+,]/n;  /* 4th sample central moment                */

   k2 = m2 # n / (n-1);                 /* variance = k2            */
   k4 = n##2 /((n-1)#(n-2)#(n-3)) # ((n+1)#m4 - 3*(n-1)#m2##2);
   kurtosis = k4 / k2##2;               /* excess kurt = k4 / k2##2 */
   return( kurtosis );
finish;


start Moments(X);
   /* Compute sample moments for columns of X.
     Return 4 x p matrix, M, where 
     M[1,] contains mean of each column of X
     M[2,] contains variance of each column of X
     M[3,] contains skewness of each column of X
     M[4,] contains kurtosis of each column of X
   */
   n = (x^=.)[+,];          /* or countn(x, "col") for SAS/IML 9.22 */
   m1 =x[:,];               /* or mean(x)                           */
   c = x-m1;
   m2 = (c##2)[+,]/n;       /* 2nd sample central moment            */
   m3 = (c##3)[+,]/n;       /* 3rd sample central moment            */
   m4 = (c##4)[+,]/n;       /* 4th sample central moment            */

   M = j(4, ncol(X));             
   M[1,] = m1;                    
   M[2,] = n/(n-1) # m2;                /* variance = k2            */
   k3 = n##2 /((n-1)#(n-2)) # m3;
   M[3,] = k3 / (M[2,])##1.5;           /* skew = k3 / k2##1.5      */
   k4 = n##2 /((n-1)#(n-2)#(n-3)) # ((n+1)#m4 - 3*(n-1)#m2##2);
   M[4,] = k4 / (M[2,])##2;             /* excess kurt = k4 / k2##2 */
   return( M );
finish;

/********************************************************************
 The Smooth Bootstrap Method
 *******************************************************************/

/* Smooth bootstrap.
   Input: A is an input vector with N elements.
   Output: (B x N) matrix. Each row is a sample. 
   Prior to SAS/IML 12.1, use the SampleReplace module */
start SmoothBootstrap(x, B, Bandwidth);
   N = nrow(x) * ncol(x);
   /* s = SampleReplace(x, B, N); */       /* prior to SAS/IML 12.1 */
   s = Sample(x, N // B);                        /* B x N matrix    */
   eps = j(B, N);                                /* allocate vector */
   call randgen(eps, "Normal", 0, Bandwidth);    /* fill vector     */
   return( s + eps );                            /* add random term */
finish;

store module=_all_;
quit;


/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Appendix A: A SAS/IML Primer
 ***********************************************************************/

proc iml;

/* Return ordered pairs on a regular grid of points.
   Return value is an (Nx*Ny x 2) matrix */
start Expand2DGrid( _x, _y );
   x  = colvec(_x); y  = colvec(_y);
   Nx = nrow(x);    Ny = nrow(y);
   x = repeat(x, Ny);
   y = shape( repeat(y, 1, Nx), 0, 1 );
   return ( x || y );
finish;

/* compute Euclidean distance between points in x and points in y.
   x is a p x d matrix, where each row is a point in d dimensions.
   y is a q x d matrix.
   The function returns the p x q matrix of distances, D, such that
   D[i,j] is the distance between x[i,] and y[j,]. */
start PairwiseDist(x, y);
   if ncol(x)^=ncol(y) then return (.);       /* Error              */
   p = nrow(x);  q = nrow(y);
   idx = T(repeat(1:p, q));                   /* index matrix for x */
   jdx = shape(repeat(1:q, p), p);            /* index matrix for y */
   diff = abs(X[idx,] - Y[jdx,]);
   return( shape( sqrt(diff[,##]), p ) );
finish;

/* compute Euclidean distance between points in x.
   x is a pxd matrix, where each row is a point in d dimensions. */
start EuclideanDistance(x);   /* in place of 12.1 DISTANCE function */
   y=x;
   return( PairwiseDist(x,y) );
finish;


/* Bisection: find root on bracketing interval [a,b]. 
    If x0 is the true root, find c such that 
    either |x0-c| < dx or |f(c)| < dy. 
    You could pass dx and dy as parameters. */
start Bisection(a, b);
   dx = 1e-6; dy = 1e-4;
   do i = 1 to 100;                               /* max iterations */
      c = (a+b)/2;
      if abs(Func(c)) < dy | (b-a)/2 < dx then 
         return(c);
      if Func(a)#Func(c) > 0 then a = c;
      else b = c;
   end;
   return (.);                                    /* no convergence */
finish;


/* function that duplicates the SQRVECH function */
start MySqrVech(x);
   m = nrow(x)*ncol(x);
   n = floor( (sqrt(8*m+1)-1)/2 );
   if m ^= n*(n+1)/2 then do;
      print "Invalid length for input vector"; STOP;
   end;
   U = j(n,n,0);
   col = repeat(1:nrow(U), nrow(U));
   row = T(col);
   idx = loc(row<=col);       /* indices of upper triangular matrix */
   U[idx] = x;                /* assign values to upper triangular  */
   L = T(U);                  /* copy to lower triangular           */
   idx = loc(row=col);        /* indices of diagonal elements       */
   L[idx] = 0;                /* zero out diagonal for L            */
   return( U + L );           /* return symmetric matrix            */
finish;


/* Random sampling with replacement and uniform probability.
   Input: A is an input vector. 
   Output: (n x k) matrix of random values from A. */
start SampleReplace(A, n, k);
   r = j(n, k);                          /* allocate result matrix  */
   call randgen(r, "Uniform");           /* fill with random U(0,1) */
   r = ceil(nrow(A)*ncol(A)*r);          /* integers 1,2,...,ncol(A)*/
   return(shape(A[r], n));               /* reshape and return      */
finish;


/* Formulas for skewness and kurtosis from Kendall and Stuart (1969) 
   The Advanced Theory of Statistics, Volume 1, p. 85. 
*/
/* Compute sample skewness for columns of x */
start Skewness(x);
   n = (x^=.)[+,];                           /* countn(x, "col")    */
   c = x - x[:,];                            /* x - mean(x)         */
   k2 = (c##2)[+,] / (n-1);                  /* variance = k2       */
   k3 = (c##3)[+,] # n / ((n-1)#(n-2));
   skew = k3 / k2##1.5;
   return( skew );
finish;

/* Compute sample (excess) kurtosis for columns of x */
start Kurtosis(x);
   n = (x^=.)[+,];                           /* countn(x, "col")    */
   c = x - x[:,];                            /* x - mean(x)         */
   c2 = c##2;
   m2 = c2[+,]/n;                            /* 2nd central moments */
   m4 = (c2##2)[+,]/n;                       /* 4th central moments */

   k2 = m2 # n / (n-1);                      /* variance = k2       */
   k4 = n##2 /((n-1)#(n-2)#(n-3)) # ((n+1)#m4 - 3*(n-1)#m2##2);
   kurtosis = k4 / k2##2;                    /* excess kurtosis     */
   return( kurtosis );
finish;

/* Return 4 x p matrix, M, where
     M[1,] contains mean of each column of X
     M[2,] contains variance of each column of X
     M[3,] contains skewness of each column of X
     M[4,] contains kurtosis of each column of X   */
start Moments(X);
   n = (x^=.)[+,];                           /* countn(x, "col")    */
   m1 =x[:,];                                /* mean(x)             */
   c = x-m1;
   m2 = (c##2)[+,]/n;                        /* 2nd central moments */
   m3 = (c##3)[+,]/n;                        /* 3rd central moments */
   m4 = (c##4)[+,]/n;                        /* 4th central moments */

   M = j(4, ncol(X));             
   M[1,] = m1;                    
   M[2,] = n/(n-1) # m2;                      /* variance           */
   k3 = n##2 /((n-1)#(n-2)) # m3;
   M[3,] = k3 / (M[2,])##1.5;                 /* skewness           */
   k4 = n##2 /((n-1)#(n-2)#(n-3)) # ((n+1)#m4 - 3*(n-1)#m2##2);
   M[4,] = k4 / (M[2,])##2;                   /* excess kurtosis    */
   return( M );
finish;

store module=_all_;
quit;


/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Appendix B: Generating Multivariate Ordinal Variables
 ***********************************************************************/

proc iml;

    /* P1   P2    P3  */
/* example matrix of PMFs */
/*
P = {0.25  0.50  0.20 ,
     0.75  0.20  0.15 ,
      .    0.30  0.25 ,
      .     .    0.40 };
*/

/* OrdN: number of values for each variable */
start OrdN(P);
   return( countn(P, "col") );
finish;

/* OrdMean: Expected value for each variable is Sigma_i (i*p[i])    */
start OrdMean(P);
   x = T(1:nrow(P));                 /* values of ordinal vars      */
   return( (x#P)[+,] );              /* expected values E(X)        */
finish;

/* OrdVar: variance for each variable */
start OrdVar(P);
   d = ncol(P);   m = OrdMean(P);
   x = T(1:nrow(P));                 /* values of ordinal vars      */
   var = j(1, d, 0);
   do i = 1 to d;
      var[i] = sum( (x - m[i])##2 # P[,i] );    /* defn of variance */
   end;
   return( var );
finish;

/* OrdCDF: Given PMF, compute CDF = cusum(PDF) */
start OrdCDF(P);
   cdf = j(nrow(P), ncol(P));        /* cumulative probabilities    */
   do i = 1 to ncol(P);
      cdf[,i] = cusum(P[,i]);
   end;
   return( choose(P=., ., cdf) );    /* missing vals for short cols */
finish;


/* Function that returns ordered pairs on a uniform grid of points.
   Return value is an (Nx*Ny x 2) matrix */
start Expand2DGrid( _x, _y );
   x  = colvec(_x); y  = colvec(_y);
   Nx = nrow(x);    Ny = nrow(y);
   x = repeat(x, Ny);
   y = shape( repeat(y, 1, Nx), 0, 1 );
   return ( x || y );
finish;


/* OrdQuant: Compute normal quantiles for CDF(P) */
start OrdQuant(P);
   N = OrdN(P);
   CDF = OrdCDF(P);
   /* QUANTILE function in SAS/IML 12.1 does not accept 1 as parameter */
   /* Replace 1 with missing value to prevent error */
   CDF = choose(CDF > 1 - 2e-6, ., CDF);
   quant = quantile( "Normal", cdf );
   do j = 1 to ncol(P);      /* set upper quantile to .I = infinity */
      quant[N[j],j] = .I;    /* .I has special meaning to BIN func  */
   end;                      
   return( quant );
finish;

/* OrdFindRoot: Use bisection to find the MV normal correlation that 
   produces a specified MV ordinal correlation. */
start OrdFindRoot(P1, P2,  target);
   N1 = countn(P1);   N2 = countn(P2);
   q1 = OrdQuant(P1); q2 = OrdQuant(P2);
   v1 = q1[1:N1-1];   v2 = q2[1:N2-1];
   g = Expand2DGrid(v1, v2);
   /* find value of rho so that sum(probbnrm(g[,1], g[,2], rho))=target */
   /* Bisection: find root on bracketing interval [a,b] */
   a = -1; b = 1;                 /* look for correlation in [-1,1] */
   dx = 1e-8; dy = 1e-5;
   do i = 1 to 100;               /* iterate until convergence      */
      c = (a+b)/2;
      Fc = sum( probbnrm(g[,1], g[,2], c) ) - target;
      if (abs(Fc) < dy) | (b-a)/2 < dx then 
         return(c);
      Fa = sum( probbnrm(g[,1], g[,2], a) ) - target;
      if Fa#Fc > 0 then a = c;
      else b = c;
   end;
   return (.);                    /* no convergence                 */
finish;

/* alternative root-finding algorithm that uses FROOT (SAS 12.1) */
/*
start MMRoot(x) global(_grid, _target);
   return( sum( probbnrm(_grid[,1], _grid[,2], x) ) - _target );
finish;

start AltOrdFindRoot(P1, P2,  target) global(_grid, _target);
   N1 = countn(P1);   N2 = countn(P2);
   q1 = OrdQuant(P1); q2 = OrdQuant(P2);
   v1 = q1[1:N1-1];   v2 = q2[1:N2-1];
   _grid = Expand2DGrid(v1, v2);
   _target = target;
   return( froot("MMRoot", {-1 1}) );
finish;
*/

/* OrdMVCorr: Compute a MVN correlation matrix from the PMF and 
   the target correlation matrix for the ordinal variables. */
start OrdMVCorr(P, Corr);
   d = ncol(P);
   N = OrdN(P);
   mean = OrdMean(P);
   var  = OrdVar(P);
   cdf  = OrdCDF(P);
   R = I(d);
   do i = 1 to d-1;
      sumCDFi = sum(cdf[1:N[i]-1, i]); 
      do j = i+1 to d;
         sumCDFj = sum(cdf[1:N[j]-1, j]); 
         hStar = Corr[i,j] * sqrt(var[i]*var[j]) + mean[i]*mean[j] 
                 - N[i]*N[j] + N[i]*sumCDFj + N[j]*sumCDFi;
         R[i,j] = OrdFindRoot(P[,i], P[,j], hStar);
         R[j,i] = R[i,j];
      end;
   end;
   return(R);
finish;

/* RandMVOrdinal: 
   N     Number of desired observations from MV ordinal distribution, 
   P     Matrix of PMF for ordinal vars. The j_th col is the j_th PMF.
         Use missing vals if some vars have fewer values than others.
   Corr  Desired correlation matrix for ordinal variables. Not every
         matrix is a valid as the correlation of ordinal variables. */
start RandMVOrdinal(N, P, Corr);
   d = ncol(P);
   C = OrdMVCorr(P, Corr);     /* 1. compute correlation matrix, C  */
   mu = j(1, d, 0);
   X = RandNormal(N, mu, C);   /* 2. simulate X ~ MVN(0,C)          */
   N = OrdN(P);
   quant = OrdQuant(P);        /* compute normal quantiles for PMFs */
   do j = 1 to d;              /* 3. convert to ordinal             */
      X[,j] = bin(X[,j], .M // quant[1:N[j],j]);
   end;
   return(X);
finish;


store module=_all_;
quit;

/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Appendix C: Random Correlation Matrices
 ***********************************************************************/

proc iml;

/********************************************************************
 Generating Random Orthogonal Matrices
 *******************************************************************/

/* Generate random orthogonal matrix G. W. Stewart (1980).    
   Function based on the QMULT MATLAB routine by Higham (1991). */
start RandOrthog(n);
A = I(n);                            /* identity matrix */
d = j(n,1,0);
d[n] = sgn(RndNormal(1,1));          /* +/- 1           */
do k = n-1 to 1 by -1; 
   /* generate random Householder transformation */
   x = RndNormal(n-k+1,1);           /* column vector from N(0,1) */
   s = sqrt(x[##]);                  /* norm(x) */
   sgn = sgn( x[1] );
   s = sgn*s;
   d[k] = -sgn;
   x[1] = x[1] + s;
   beta = s*x[1];
   /* apply the Householder transformation to A */
   y = x`*A[k:n, ];
   A[k:n, ] = A[k:n, ] - x*(y/beta);
end;
A = d # A; /* change signs of i_th row when d[i]=-1 */
return(A);
finish;

/* helper functions */
/* return matrix of same size as A with 
   m[i,j]= {  1 if A[i,j]>=0
           { -1 if A[i,j]< 0
   Similar to the SIGN function, except SIGN(0)=0 */
start sgn(A);
   return( choose(A>=0, 1, -1) );
finish;

/* return (r x c) matrix of standard normal variates */
start RndNormal(r,c);
   x = j(r,c);
   call randgen(x, "Normal");
   return(x);
finish;


/********************************************************************
 Random Matrix with Specified Eigenvalues
 *******************************************************************/

start RandMatWithEigenval(lambda);
   n = ncol(lambda);                 /* assume lambda is row vector */
   Q = RandOrthog(n);
   return( Q`*diag(lambda)*Q );
finish;

/********************************************************************
 Applying Givens Rotations
 *******************************************************************/

/* apply Givens rotation to A in (i,j) position. Naive implementation is
   G = I(nrow(A));  G[i,i]=c;  G[i,j]=s; G[j,i]=-s;  G[j,j]=c;   
   A = G`*A*G; */
start ApplyGivens(A,i,j);
   Aii = A[i,i];   Aij = A[i,j];   Ajj = A[j,j];
   t = (Aij + sqrt(Aij##2 - (Aii-1)*(Ajj-1))) / (Ajj - 1);
   c = 1/sqrt(1+t##2);
   s = c*t;
   Ai = A[i,]; Aj = A[j,];    /* linear combo of i_th and j_th ROWS */
   A[i,] = c*Ai - s*Aj;    A[j,] = s*Ai + c*Aj;
   Ai = A[,i]; Aj = A[,j];    /* linear combo of i_th and j_th COLS */
   A[,i] = c*Ai - s*Aj;    A[,j] = s*Ai + c*Aj;
finish;

/********************************************************************
 Random Correlation Matrices
 *******************************************************************/

/* Generate random correlation matrix (Davies and Higham (2000))
   Input: lambda = desired eigenvalues (scaled so sum(lambda)=n)
   Output: random NxN matrix with eigenvalues given by lambda  */
start RandCorr(_lambda);
lambda = rowvec(_lambda);                   /* ensure row vector   */
n = ncol(lambda);
lambda = n * _lambda /sum(_lambda);         /* ensure sum(lambda)=n */
maceps = constant("MACEPS");

corr = RandMatWithEigenval(lambda);
convergence = 0;
do iter = 1 to n while (^convergence);
   d = vecdiag(corr);
   if all( abs(d-1) < 10*maceps ) then      /* diag=1 ==> done      */
      convergence=1;
   else do;                         /* apply Givens rotation        */
      idxgt1 = loc(d>1);
      idxlt1 = loc(d<1);   
      i = idxlt1[1];                /* first index for which d[i]<1 */
      j = idxgt1[ncol(idxgt1)];     /* last index for which d[j]>1  */
      if i > j then do;             /* -or- */
         i = idxgt1[1];             /* first index for which d[i]>1 */
         j = idxlt1[ncol(idxlt1)];  /* last index for which d[j]<1  */
      end;
      run ApplyGivens(Corr,i,j);
      corr[i,i] = 1;                /* avoid rounding error: diag=1 */
   end;
end;
return(corr);
finish;


store module=_all_;
quit;


/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Appendix D: Functions for Simulating Data by Using Fleishman's Transformation
 ***********************************************************************/

proc iml;
/* Compute sample mean, var, skew, and kurtosis. Formulas from
   Kendall, M.G., Stuart, A. (1969) The Advanced Theory of Statistics,
             Volume 1, p. 85.
/* Return 4 x p matrix, M, where
     M[1,] contains mean of each column of X
     M[2,] contains variance of each column of X
     M[3,] contains skewness of each column of X
     M[4,] contains kurtosis of each column of X
*/
start Moments(X);
   n = (x^=.)[+,];                           /* countn(x, "col")    */
   m1 =x[:,];                                /* mean(x)             */
   c = x-m1;
   m2 = (c##2)[+,]/n;                        /* 2nd central moments */
   m3 = (c##3)[+,]/n;                        /* 3rd central moments */
   m4 = (c##4)[+,]/n;                        /* 4th central moments */

   M = j(4, ncol(X));             
   M[1,] = m1;                    
   M[2,] = n/(n-1) # m2;                      /* variance           */
   k3 = n##2 /((n-1)#(n-2)) # m3;
   M[3,] = k3 / (M[2,])##1.5;                 /* skewness           */
   k4 = n##2 /((n-1)#(n-2)#(n-3)) # ((n+1)#m4 - 3*(n-1)#m2##2);
   M[4,] = k4 / (M[2,])##2;                   /* excess kurtosis    */
   return( M );
finish;

/*********************************************************/
/* Use Newton's method to fit Fleishman's model          */
/*********************************************************/
start Fleishman(coef);
   b = coef[1]; c = coef[2]; d = coef[3];
   b2 = b##2; c2 = c##2; d2 = d##2; bd = b#d; 
   var = b2 + 6#bd + 2#c2 + 15#d2;               /* variance */
   skew = 2#c#(b2 + 24#bd + 105#d2 + 2);         /* skewness */
   kurt = 24#(bd + c2#(1 + b2 + 28#bd) + d2 #
       (12 + 48#bd + 141#c2 + 225#d2));          /* excess kurtosis */
   return( var // skew // kurt );
finish;

/* Find the root of this function */
start FlFunc(x) global (g_target);   /* g_target=(skewness, kurtosis) */
   return ( Fleishman(x) - (1 // g_target[1] // g_target[2]) );
finish FlFunc;

/* derivatives of the Fleishman function */
start FlDeriv(x);
   b = x[1]; c = x[2]; d = x[3];
   b2 = b##2; c2 = c##2; d2 = d##2; bd = b#d; 
   df1db = 2#b + 6#d;
   df1dc = 4#c;
   df1dd = 6#b + 30#d;
   df2db = 4#c#(b + 12#d);
   df2dc = 2#(b2 + 24#bd + 105#d2 + 2);
   df2dd = 4#c#(12#b + 105#d);
   df3db = 24#(d + c2#(2#b + 28#d) + 48#d##3);
   df3dc = 48#c#(1 + b2 + 28#bd + 141#d2);
   df3dd = 24#(b + 28#b#c2 + 2#d#(12 + 48#bd + 141#c2 + 225#d2)
           + d2#(48#b + 450#d));
   J = (df1db || df1dc || df1dd) // 
       (df2db || df2dc || df2dd) //
       (df3db || df3dc || df3dd);
   return( J );
finish FlDeriv;


/* Newton's method to find roots of a function.
    You must supply the FLFUNC and FLDERIV functions
    that compute the function and the Jacobian matrix.
    Input: x0 is the starting guess
           optn[1] = max number of iterations
           optn[2] = convergence criterion for || f ||
    Output: x contains the approximation to the root */
start Newton(x, x0, optn);
   maxIter = optn[1]; converge = optn[2];
   x = x0;
   f = FLFunc(x);
   do iter = 1 to maxIter while(max(abs(f)) > converge);
      J = FLDeriv(x); 
      delta = -solve(J, f);                    /* correction vector */
      x = x + delta;                           /* new approximation */
      f = FLFunc(x);         
   end;
   /* return missing if no convergence */
   if iter > maxIter then x = j(nrow(x0),ncol(x0),.);
finish Newton;


/* Given (skew, kurt), produce an initial guess of the Fleishman 
   coefficients to use for Newton's algorithm.  This guess is 
   produced by a polynomial regression. */
start FleishmanIC(skew, kurt);
   c = j(3,1);
   c[1] = 0.95357 -0.05679*kurt +          /* c1 = Quad(skew, kurt) */
          0.03520*skew##2 + 0.00133*kurt##2;
   c[2] = 0.10007*skew + 0.00844*skew##3;       /* c2 = Cubic(skew) */
   c[3] = 0.30978 -0.31655*c[1];                /* c3 = Linear(c1)  */
   return (c);
finish;


start FitFleishmanFromSK(skew, kurt) global (g_target);
   /* 1. check that (skew,kurt) is in the feasible region */
   if kurt < -1.13168+1.58837*skew##2 then return({. . . .});
   /* 2. Initial guess for nonlinear root finding */
   x0 = FleishmanIC(skew, kurt);
   optn = {25, 1e-5};   /* maximum iterations, convergence criterion */

   /* 3. Find cubic coefficients (c1, c2, c3) so that 
         -c2+c1*Z+c2*Z##2+c3*Z##3 has target (skew,kurt). */
   g_target = skew || kurt;                  /* set global variable */
   run Newton(coef, x0, optn);               /* find (c1, c2, c3)   */
   return( -coef[2] // coef );
finish;


/* Fit Fleishman model. Given data x, find coefficients 
   c = {c0, c1, c2, c3} so that the standardized data is modeled by 
   c0+c1*Z+c2*Z##2+c3*Z##3 for Z~N(0,1) */
start FitFleishman(x);
   m = Moments(x);                        /* compute sample moments */
   return( FitFleishmanFromSK(m[3], m[4]) );
finish;


/* return N x NumSamples matrix of samples from Fleishman distrib */
start RandFleishman(N, NumSamples, coef);
   /* fill each element of X with sample from Fleishman distribution 
      with given coefficients */
   Z = j(N, NumSamples);
   call randgen(Z, "Normal");
   X = coef[1] + Z#(coef[2] + Z#(coef[3]+Z#coef[4]));
   return( X );
finish;


/******************************************************/
/* Implement the Vale-Maurelli (1983) method
   of generating multivariate nonnormal data. */

/* solve the Vale-Maurelli cubic equation to find the intermediate 
   correlation between two normal variables that gives rise to a target
   correlation (rho) between the two transformed nonnormal variables. */
start SolveCorr(rho, coef1, coef2);
   a1 = coef1[1]; a1 = coef2[1]; b1 = coef1[2]; b2 = coef2[2];
   c1 = coef1[3]; c2 = coef2[3]; d1 = coef1[4]; d2 = coef2[4];
   coef = (6*d1*d2) ||
          (2*c1*c2) ||
          (b1*b2+3*b1*d2+3*d1*b2+9*d1*d2) ||
          -rho;
   roots = polyroot(coef); /* solve for zero of cubic polynomial */
   /* roots is a 3x2 matrix of complex roots */
   realIdx = loc(abs(roots[,2]<1e-8)); /* extract the real root(s) */
   r = roots[realIdx,1][1];            /* return smallest real root */
   return (r);
finish;

/* Given a target correlation matrix, R, and target values of skewness
   and kurtosis for each marginal distribution, find the "intermediate"
   correlation matrix, V */
start VMTargetCorr(R, skew, kurt);
   V = j( nrow(R), ncol(R), 1);
   do i = 2 to nrow(R);
      ci = FitFleishmanFromSK(skew[i], kurt[i]);
      do j = 1 to i-1;
         cj = FitFleishmanFromSK(skew[j], kurt[j]);
         V[i,j] = SolveCorr(R[i,j], ci, cj);
         V[j,i] = V[i,j];
      end;
   end;
   return (V);
finish;

/* simulate random values from a multivariate nonnormal distribution
   such that
   1) Each marginal distribution has a specified skewness and kurtosis
   2) The marginal variables have the correlation matrix R */
start RandValeMaurelli(N, R, skew, kurt);
   /* compute Fleishman coefficients that match marginal moments */
   c = j(ncol(R), 4);
   do i = 1 to ncol(R);
      c[i,] = T( FitFleishmanFromSK(skew[i], kurt[i]) );
   end;
   /* adjust correlation matrix */
   V = j(nrow(R), ncol(R), 1);
   do i = 2 to nrow(R);
      do j = 1 to i-1;
         V[i,j] = SolveCorr(R[i,j], c[i,], c[j,]);
         V[j,i] = V[i,j];
      end;
   end;

   call eigen(D, U, V);      /* eigenvector decomposition: V=U*D*U` */
   F = sqrt(D`) # U;         /* F is a square root matrix for V     */
   X = j(N, ncol(R));
   call randgen(X, "Normal");               /* uncorrelated normals */
   Y = X*F`;                                /* correlated normals   */
   do i = 1 to ncol(R);
      w = Y[,i];             /* apply Fleishman transformation      */
      X[,i] = c[i,1] + w#(c[i,2] + w#(c[i,3] + w#c[i,4])); 
   end;
   return(X);
finish;

store module=_all_; /* store functions for later use */
quit;