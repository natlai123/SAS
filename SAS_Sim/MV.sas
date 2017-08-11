/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 8: Simulating Data from Basic Multivariate Distributions

 This program saves functions to the SAS/IML storage library.
 To use these functions in a SAS/IML program, run this program
 and then load the functions, as follows:

 %include "C:\<path>\MV.sas";
 proc iml;
 load module=_all_;
 <...call the functions...>
 
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

