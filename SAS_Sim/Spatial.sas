/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 14: Simulating Data from Spatial Models

 This program saves functions to the SAS/IML storage library.
 To use these functions in a SAS/IML program, run this program
 and then load the functions, as follows:

 %include "C:\<path>\Spatial.sas";
 proc iml;
 load module=_all_;
 <...call the functions...>

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


