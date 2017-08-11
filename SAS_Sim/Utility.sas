/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Appendix A: A SAS/IML Primer

 This program saves functions to the SAS/IML storage library.
 To use these functions in a SAS/IML program, run this program
 and then load the functions, as follows:

 %include "C:\<path>\Utility.sas";
 proc iml;
 load module=_all_;
 <...call the functions...>

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


