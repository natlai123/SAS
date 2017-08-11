/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Appendix D: Functions for Simulating Data by Using Fleishman's Transformation

 This program saves functions to the SAS/IML storage library.
 To use these functions in a SAS/IML program, run this program
 and then load the functions, as follows:

 %include "C:\<path>\RandFleishman.sas";
 proc iml;
 load module=_all_;
 <...call the functions...>

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