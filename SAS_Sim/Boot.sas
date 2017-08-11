/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 15: Resampling and Bootstrap Methods

 This program saves functions to the SAS/IML storage library.
 To use these functions in a SAS/IML program, run this program
 and then load the functions, as follows:

 %include "/folders/myfolders/SAS_Sim/Boot.sas";
 proc iml;
 load module=_all_;
 <...call the functions...>
 
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


