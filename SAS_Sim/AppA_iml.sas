/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Appendix A: A SAS/IML Primer
 ***********************************************************************/

proc iml;
/* read variables from a SAS data set into vectors */
varNames = {"Name" "Age" "Height"};
use Sashelp.Class(OBS=3);  /* open data set for reading             */
read all var varNames;     /* create three vectors: Name,...,Height */
close Sashelp.Class;       /* close the data set                    */
print Name Age Height;

/* read variables from a SAS data set into a matrix */
varNames = {"Age" "Height" "Weight"};
use Sashelp.Class(OBS=3);
read all var varNames into m;   /* create matrix with three columns */
close Sashelp.Class;
print m[colname=VarNames];

/* read all numeric variables from a SAS data set into a matrix */
use Sashelp.Class;
read all var _NUM_ into y[colname=NumericNames]; 
close Sashelp.Class;
print NumericNames; 

/* create SAS data set from vectors */
x = T(1:10);                     /* {1,2,3,...,10}                  */
y = T(10:1);                     /* {10,9,8,...,1}                  */
create OutData var {x y};        /* create Work.OutData for writing */
append;                          /* write data in x and y           */
close OutData;                   /* close the data set              */

/* create SAS data set from a matrix */
z = x || y;                      /* horizontal concatenation        */
create OutData2 from x[colname={"Count" "Value"}];
append from x;
close OutData2;


/***********************************************************************/

proc iml;
N = 2;                                    /* size of each sample    */
NumSamples = 3;                           /* number of samples      */
ID = repeat( T(1:NumSamples), 1, N);      /* {1  1,
                                              2  2,
                                              3  3} */
SampleID = colvec(ID);                    /* convert to long vector */
ID = repeat(1:NumSamples, 1, N);          /* {1  2  3  1  2  3  ... */
ReplID = colvec(ID);                      /* convert to long vector */
print SampleID ReplID;


/***********************************************************************/

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

/* test the module */
x = {0,1,2};  y = {-1,0,1};
g = Expand2DGrid(x,y);
print g;


/***********************************************************************/

proc iml;
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

x = { 1  0, 
      1  1,
     -1 -1};
y = { 0  0,
     -1  0};
P = PairwiseDist(x,y);        /* not printed */
D = EuclideanDistance(x);
print D;

/***********************************************************************/

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

/* test it: Find q such that F(q) = target */
start Func(x) global(target);
   cdf = (x + x##3 + x##5)/3;
   return( cdf-target );
finish;

target = 0.5;               /* global variable used by Func module */
q = Bisection(0,1);         /* find root on interval [0,1]         */
print q;


/***********************************************************************/

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

y = 1:15;
z = MySqrVech(y); 
print z;

/***********************************************************************/

/* Random sampling with replacement and uniform probability.
   Input: A is an input vector. 
   Output: (n x k) matrix of random values from A. */
start SampleReplace(A, n, k);
   r = j(n, k);                          /* allocate result matrix  */
   call randgen(r, "Uniform");           /* fill with random U(0,1) */
   r = ceil(nrow(A)*ncol(A)*r);          /* integers 1,2,...,ncol(A)*/
   return(shape(A[r], n));               /* reshape and return      */
finish;

x = {A B C A A B};
call randseed(1);
s = SampleReplace(x, 3, 4);
print s;

/***********************************************************************/

proc iml;
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

/* for the Gamma(4) distribution, the skewness
   is 2/sqrt(4) = 1 and the kurtosis is 6/4 = 1.5 */
call randseed(1);
x = j(10000,1);
call randgen(x, "Gamma", 4);
skew = skewness(x);
kurt = kurtosis(x);
print skew kurt;


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
