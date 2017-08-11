/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.
	
 Chapter 10: Building Correlation and Covariance Matrices
 
 	Check if Covariance/ Correlation Matrix is 
 		Symmetric 
 		Positive-Definite/ Positive semi-definite
 
	10.4.1 Estimate a Covariance Matrix from Data 
	10.4.2 Generating a Symmetric Matrix 
	10.4.3 Generating a Diagonally Dominant Covariance Matrix 
	10.4.4 Generating a Covariance Matrix with a Known Structure 
	10.5 Generating Matrices from the Wishart Distribution 
 
 	10.6 Generating Random Correlation Matrices: RANDCORR 
 
 The following options can be used to convert a matrix that is not positive definite into a matrix that is
positive definite:
	
	Ridge factor
	Shrinkage methods
	The Nearest Correlation Matrix: NEARESTCORR
 
 
 ***********************************************************************/

ods graphics on;

/********************************************************************

 Converting between Correlation and Covariance Matrices

*********************************************************************/

proc iml;
/* convert a covariance matrix, S, to a correlation matrix. R = D^-1*S*D^-1 */
start Cov2Corr(S);
   D = sqrt(vecdiag(S));			/*D = {1 4 9} = sd*/
   return( S / D` / D );        /* divide columns, then divide rows */
finish;

/* R = correlation matrix
   sd = (vector of) standard deviations for each variable 
   Return covariance matrix with sd##2 on the diagonal */
start Corr2Cov(R, sd);
   std = colvec(sd);                  /* convert to a column vector */
   return( std` # R # std );
finish;

S = {1.0  1.0  8.1,                /* covariance matrix             */
     1.0 16.0 18.0,
     8.1 18.0 81.0 };
Corr = Cov2Corr(S);                /* convert to correlation matrix */

sd = sqrt(vecdiag(S));             /* sd = {1 4 9}                  */
Cov = Corr2Cov(Corr, sd);          /* convert to covariance matrix  */
print S[label="S then Divided by {1 4 9} -> Corr"], Corr, Cov;


/********************************************************************
 
	 Testing Whether a Matrix Is a Covariance Matrix
 
A positive definite matrix is a matrix that has all positive eigenvalues. A positive semidefinite (PSD)
matrix is a matrix that has nonnegative eigenvalues.
 
Suppose that you want to test whether a given matrix is a valid covariance matrix. You have to check
two properties: that the matrix is symmetric, and that the matrix is positive semidefinite. 

1. Use the ROOT function to compute a Cholesky decomposition. If the decomposition succeeds,
the matrix is PSD.
2. Use the EIGVAL function to compute the eigenvalues of A. If no eigenvalue is negative, A is
PSD. If all eigenvalues are positive, A is positive definite.

*********************************************************************/

proc iml;
A = { 2 -1  0,
     -1  2 -1,
      0 -1  2 };

/* finite-precision test of whether a matrix is symmetric */
start SymCheck(A);
   B = (A + A`)/2;
   scale = max(abs(A));
   delta = scale * constant("SQRTMACEPS");
   return( all( abs(B-A)< delta ) );
finish;

/* test a matrix for symmetry */
IsSym = SymCheck(A);
print IsSym;

/*https://v8doc.sas.com/sashtml/lgref/z1324964.htm*/

/*proc iml;
A = { 2 -1  0,
     -1  2 -1,
      0 -1  2 };

   B = (A + A`)/2;
   print B;
   scale = max(abs(A));
   print scale;
   t = constant("SQRTMACEPS");
   print t;
   delta = scale * constant("SQRTMACEPS");
   print delta;
  
   D = abs(B-A);
   PRINT D;*/

/***********************************************************************/

G = root(A);
G = root(A, "NoError");                   /* SAS/IML 12.1 and later */
if G=. then print "The matrix is not positive semidefinite";
eigval = eigval(A);
print eigval;
if any(eigval<0) then print "The matrix is not positive semidefinite";

/*Although the EIGVAL function is not as fast as the ROOT function, the EIGVAL function has
the advantage that you can inspect the eigenvalues. In particular, the magnitude of the smallest
eigenvalue gives information about how close the matrix is to being PSD. A small negative
eigenvalue means that the matrix is close to being semidefinite.*/

/********************************************************************

 	Techniques to Build a Covariance Matrix
 
• Estimate a covariance matrix from real or simulated data.
• Generate a symmetric matrix that is diagonally dominant.
• Generate a matrix that has a special structure that is known to be PD.

A covariance matrix that is computed from a data matrix, X, is symmetric positive semidefinite 
if the following conditions are true:

• No column of X is a linear combination of other columns.
• The number of rows of X exceeds the number of columns.
• If an observation contains a missing value in any variable, then the obs is not used to form the
covariance matrix. The NOMISS option in the PROC CORR statement ensures this condition is satisfied.

*********************************************************************/

/* Method 1: Base SAS approach: Check output data */
proc corr data=Sashelp.Class COV NOMISS outp=Pearson;
   var Age Height Weight;
   ods select Cov;
run;

/* Method 2: equivalent SAS/IML computation */
proc iml;
use Sashelp.Class;
read all var {"Age" "Height" "Weight"} into X;
close Sashelp.Class;
Cov = cov(X);
print Cov;
Corr = Cov2Corr(Cov); 
print Corr;

/***********************************************************************/

	/*10.4.2 Generating a Symmetric Matrix*/

proc iml;
N = 4;                            /* want 4x4 symmetric matrix      */
call randseed(1);
v = j(N*(N+1)/2, 1);              /* allocate lower triangular      */
call randgen(v, "Uniform");       /* fill with random               */
x = sqrvech(v);                   /* create symmetric matrix from v */
print x[format=5.3];
eigval = eigval(x);
print eigval;
if any(eigval<0) then print "The matrix is not positive semidefinite";

/***********************************************************************/

	/*10.4.3 Generating a Diagonally Dominant Covariance Matrix*/

/* A symmetric matrix with positive diagonal entries is PD if it is row diagonally dominant. 
A matrix A is row diagonally dominant if the magnitude of the diagonal element is greater 
than the sum of the magnitudes of the off-diagonal elements in the  same row.*/

/* Add a multiple of diag(A) so that A is diagonally dominant. */
start Ridge(A, scale);         /* Input scale >= 1                  */
   d = vecdiag(A);
   s = abs(A)[,+] - d;         /* sum(abs of off-diagonal elements) */
   lambda = scale * (max(s/d) - 1); 
   return( A + lambda*diag(d) );
finish;

/* assume x, which has defined above, is symmetric matrix */
H = Ridge(x, 1.01);    /* scale > 1 => H is pos definite; Change scale to 0.2 and H becomes non-PD*/
print H;
eigval = eigval(H);
print eigval;
if any(eigval<0) then print "The matrix is not positive semidefinite";

/*If the scale parameter equals unity, then H is PD. If the parameter is greater than 1,
 H is positive definite.*/

/****************************************************************************************

	10.4.2 Generating a Symmetric Matrix

****************************************************************************************/

proc iml;
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

N = 4;                             /* get 4x4 symmetric PD matrices */
call randseed(1);
Y = RandSymUsingRidge(5, N, 1.1);          /* get five 4x4 matrices */
/* print first matrix */
Y1 = shape(Y[1,], N);
print Y1;
quit;


/**********************/


/****************************************************************************************

	10.4.4 Generating a Covariance Matrix with a Known Structure 

Four common covariance structures that are used in mixed modeling and that
are supported by the MIXED procedure in SAS: 

	A diagonal structure, 
	Compound symmetry, 
	Toeplitz,
	First-order autoregressive (AR(1)).

****************************************************************************************/


proc iml;
/* variance components: diag({var1, var2,..,varN}), var_i>0 */
start VarComp(v);
   return( diag(v) );
finish;
/*As long as the diagonal elements are positive, the resulting matrix is a covariance matrix.*/

vc = VarComp({16,9,4,1});
print vc;
eigval = eigval(vc);
print eigval;
if any(eigval<0) then print "The matrix is not positive semidefinite";

/* compound symmetry, v>0:
   {v+v1    v1    v1    v1,
      v1  v+v1    v1    v1,
      v1    v1  v+v1    v1,
      v1    v1    v1  v+v1  };
*/   
start CompSym(N, v, v1);
   return( j(N,N,v1) + diag( j(N,1,v) ) );
finish;
/*To guarantee that a compound symmetric matrix is PD, choose v > 0 and v1 > -v/N.*/

cs = CompSym(4, 4, 1);
print cs;
eigval = eigval(cs);
print eigval;
if any(eigval<0) then print "The matrix is not positive semidefinite";

/* Toeplitz:
   {s##2 s1   s2   s3,
    s1   s##2 s1   s2,
    s2   s1   s##2 s1,
    s3   s2   s1   s##2 };
   Let u = {s1 s2 s3};
*/   
toep = toeplitz( {4 1 3 2} );
print toep;
eigval = eigval(toep);
print eigval;
if any(eigval<0) then print "The matrix is not positive semidefinite";


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
   return( s##2 # toeplitz(1 || u) );	/*http://support.sas.com/documentation/cdl/en/imlug/65547/HTML/default/viewer.htm#imlug_langref_sect084.htm*/
finish;

ar1 = AR1(4, 1, 0.25);
print ar1;
eigval = eigval(ar1);
print eigval;
if any(eigval<0) then print "The matrix is not positive semidefinite";
quit;

proc iml;
N=6;
rho=0.25;
s=1;
  u = cuprod(j(1,N-1,rho));      /*1x(N-1) vector*/       
  print u;
  d=toeplitz(1 || u);			 /*toeplitz(1xN vector)-> NxN matrix*/
print d;
cor= s##2 #d;
print cor;
quit;

/*Exercise 10.3: Write a SAS/IML function that constructs a matrix with uniform correlation structure.*/

proc iml;
start UniCorr(N, v1);
   return( j(N,N,v1) + diag( j(N,1,(1-v1)) ) );
finish;
/*To guarantee that a compound symmetric matrix is PD, choose v > 0 and v1 > -v/N.*/

uc = UniCorr(4, 0.6);	/*Every off-diagonal element of th correlation matrix is v1*/
print uc;
eigval = eigval(uc);
print eigval;
if any(eigval<0) then print "The matrix is not positive semidefinite";
quit;

/********************************************************************
 
 	
 	10.5: Generating Matrices from the Wishart Distribution
 
 Sampling Distribtuion of sample covariance matrix
 
*********************************************************************/

proc iml;
call randseed(12345);
NumSamples = 1000;                /* number of Wishart draws        */
N = 50;                           /* MVN sample size                */
Sigma = {9 1, 
         1 1};
PRINT SIGMA;
/* Simulate matrices. Each row is scatter matrix */
A = RandWishart(NumSamples, N-1, Sigma); 	/*A=(N-1)B ~W(Cov, N-1)*/
*print A;
B = A / (N-1);                    /* each row is covariance matrix  */
*print B;
S1 = shape(B[1,], 2, 2);          /* first row, reshape into 2 x 2  */
S2 = shape(B[2,], 2, 2);          /* second row, reshape into 2 x 2 */
print S1 S2;                      /* two 2 x 2 covariance matrices  */

SampleMean = shape(B[:,], 2, 2);  /* mean covariance matrix         */
print SampleMean;
/* for exercise */
create CovB from B[c={"B11" "B12" "B21" "B22" }];
append from B;
close CovB;

proc univariate data=CovB;
   var B11 B12 B22;
   histogram B11 B12 B22;
   ods select Histogram;
run;

proc means data=CovB mean stddev P5 P95;
   var B11 B12 B22;
run;

/**********************/

/********************************************************************

	Simulating the sampling distribution of sample covariance
 
 Comparison between Wishart and SamplCov datasets
 
*********************************************************************/

proc iml;
    Numsample = 1000;   	   
    N = 100;       
	Mean = {1, 2, 3};
	Cov = {3 2 1,                  
    	   2 4 0,
       	   1 0 5};
    PRINT Cov;                   
	call randseed(4321); 
	SampleCov= J(Numsample, 9 , .);
	do i = 1 to Numsample;
	X = RandNormal(N, Mean, Cov); 
	c=cov(x);
	SampleCov[i,] = shape(c, 1, 9);
	end;
	S1 = shape(SampleCov[1,], 3, 3);          /* first row, reshape into 3 x 3  */
	S2 = shape(SampleCov[2,], 3, 3);          /* second row, reshape into 3 x 3 */
	print S1 S2;                      
	SampleMean = shape(SampleCov[:,], 3, 3);
	print SampleMean;
	create SampleCov from SampleCov[c={"B11" "B12" "B13" "B21" "B22" "B23" "B31" "B32" "B33"}];  
	append from SampleCov;  
	close SampleCov;
quit;

proc iml;
    Numsample = 1000;   	   
    N = 100;       
	Mean = {1, 2, 3};
	Cov = {3 2 1,                  
    	   2 4 0,
       	   1 0 5};
    call randseed(4321); 
    A = RandWishart(NumSample, N-1, Cov);  
    W = A / (N-1);
    SampleMean = shape(A[:,], 3, 3);
	print SampleMean;
	create Wishart from W[c={"W11" "W12" "W13" "W21" "W22" "W23" "W31" "W32" "W33"}];  
	append from W; 
quit;

*PROC COMPARE base=Wishart compare=SampleCov; run; 

proc means data=SampleCov  mean stddev P5 P95;
   var B11 B22 B33;
run;
proc means data=Wishart mean stddev P5 P95;
   var W11 W22 W33;
run;

proc univariate data=SampleCov;
   title "Sampling Distribution of Sample Covariance, N=100";
   histogram B11 B22 B33;
   ods select Histogram;
run; 
 
proc univariate data=Wishart;
   title "Draws from Wishart Distribution, N=100";
   histogram W11 W22 W33;
   ods select Histogram;
run; 

/*************************/


/*********************************************************************/


/* Functions to generate a random correlation matrix 
		
		The main function is RandCorr */
 
proc iml;
/* Generate random orthogonal matrix G. W. Stewart (1980).    
   Algorithm from QMULT MATLAB routine by Higham (1991) */
start RandOrthog(n);
   A = I(n);                            /* identity matrix          */
   d = j(n,1,0);
   d[n] = sgn(RndNormal(1,1));          /* +/- 1                    */
   do k = n-1 to 1 by -1; 
      /* generate random Householder transformation */
      x = RndNormal(n-k+1,1);           /* column vector from N(0,1) */
      s = sqrt(x[##]);                  /* norm(x)                  */
      sgn = sgn( x[1] );
      s = sgn*s;
      d[k] = -sgn;
      x[1] = x[1] + s;
      beta = s*x[1];
      /* apply the Householder transformation to A */
      y = x`*A[k:n, ];
      A[k:n, ] = A[k:n, ] - x*(y/beta);
   end;
   A = d # A;              /* change signs of i_th row when d[i]=-1 */
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

start RandMatWithEigenval(lambda);
   n = ncol(lambda);                 /* assume lambda is row vector */
   Q = RandOrthog(n);
   return( Q`*diag(lambda)*Q );
finish;

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

/* Generate random correlation matrix (Davies and Higham (2000))
   Input: lambda = desired eigenvalues (scaled so sum(lambda)=n)
   Output: random N x N matrix with eigenvalues given by lambda  */
start RandCorr(_lambda);
   lambda = rowvec(_lambda);                   /* ensure row vector   */
   n = ncol(lambda);
   lambda = n * lambda /sum(lambda);          /* ensure sum(lambda)=n */
   maceps = constant("MACEPS");

   corr = RandMatWithEigenval(lambda);
   convergence = 0;
   do iter = 1 to n while (^convergence);
      d = vecdiag(corr);
      if all( abs(d-1) < 1e3*maceps ) then     /* diag=1 ==> done     */
         convergence=1;
      else do;                        /* apply Givens rotation        */
         idxgt1 = loc(d>1);
         idxlt1 = loc(d<1);   
         i = idxlt1[1];               /* first index for which d[i]<1 */
         j = idxgt1[ncol(idxgt1)];    /* last index for which d[j]>1  */
         if i > j then do;            /* -or- */
            i = idxgt1[1];            /* first index for which d[i]>1 */
            j = idxlt1[ncol(idxlt1)]; /* last index for which d[j]<1  */
         end;
         run ApplyGivens(Corr,i,j);
         corr[i,i] = 1;               /* avoid rounding error: diag=1 */
      end;
   end;
   return(corr);
finish;

store module=_all_;
quit;
/* Helper functions have been defined and saved. */


/********************************************************************
 
 
 	Generating Random Correlation Matrices
 
 Input Spectrum (the set of eigenvalues for a correlation matirx) 
 Output a Random Correlation Matrix
 
 Uncorrelated data have a correlation matrix for which all eigenvalues 
 are close to unity. Highly correlated data have one large eigenvalue 
 and many smaller eigenvalues. There are many possibilities in between.
 
 
*********************************************************************/

/*The eigenvalues of an n  n correlation matrix are real, nonnegative, and sum to 
n because the trace of a matrix is the sum of its eigenvalues.*/

proc iml;	
start Cov2Corr(S);
   D = sqrt(vecdiag(S));			/*D = {1 4 9} = sd*/
   return( S / D` / D );        /* divide columns, then divide rows */
finish;

Cov = {3 2 1,                  
  	   2 4 0,  
  	   1 0 5};
Cor=Cov2Corr(Cov);  	   
eigval = eigval(Cor);
Sum = eigval[+,];
print Cov Cor, eigval, sum;		/**/
quit;


/* Define and store the functions for random correlation matrices */
%include "/folders/myfolders/SAS_Sim/RandCorr.sas";

proc iml;
load module=_all_;                     /* load the modules */
/* test it: generate 4 x 4 matrix with given spectrum */
*call randseed(4321);
lambda = {2 1 0.75 0.25};           /* notice that sum(lambda) = 4  */
R = RandCorr(lambda);               /* R has lambda for eigenvalues */
eigvalR = eigval(R);                /* verify eigenvalues           */
print R, eigvalR;

/* for the exercise: generate 1,000 3x3 matrices with given spectrum */
lambda = {1.5 1 0.5};               /* notice that sum(lambda) = 3  */
result = j(1000,3);
do i = 1 to nrow(result);
   R = RandCorr(lambda`);
   result[i, ] = T( R[{2 3 6}] );
end;
create RandCorrSpectrum from result[c={R12 R13 R23}];
append from result;
close RandCorrSpectrum;

proc univariate data=RandCorrSpectrum;
   histogram R12 R13 R23;
   ods select Histogram;
run;


/**********************/


/********************************************************************


	 When Is a Correlation Matrix Not a Correlation Matrix?
	 
Mathematically, the problem is that correlations between variables are not independent. 
They are coupled together by the requirement that a true correlation matrix is positive 
semidefinite.

Sometimes an estimated sample covariance or correlation matrix is not positive definite. In SAS
software, this can occur when the data contain missing values. By default, the CORR procedure
computes pairwise correlations, which means that correlations between variables are based on pairs
of variables, not on the entire multivariate data. To override the default behavior, use the NOMISS
option. The NOMISS option instructs PROC CORR to exclude observations for which any variable
is missing, which is a process known as listwise deletion. This option ensures that the resulting
covariance matrix is positive semidefinite.

The following options can be used to convert a matrix that is not positive definite into a matrix that is
positive definite:
	
	Ridge factor
	Shrinkage methods
	The Nearest Correlation Matrix	

*********************************************************************/

proc iml;
C = {1.0 0.3 0.9,
     0.3 1.0 0.9,
     0.9 0.9 1.0};
eigval = eigval(C);
print eigval;
/*Not every symmetric matrix with unit diagonal is a correlation matrix.*/


/********************************************************************
 	
 	The Nearest Correlation Matrix
 
*********************************************************************/

proc iml;
/* Project symmetric X onto S={positive semidefinite matrices}.
   Replace any negative eigenvalues of X with zero */
start ProjS(X);						  /*CALL EIGEN (evals, evecs, A<VECL=vl>);*/
   call eigen(D, Q, X);               /* notice that X = Q*D*Q`     */
   V = choose(D>0, D, 0);		  /*Replace any negative eigenvalues with zeros*/
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
/*A matrix norm is a natural extension of the notion of a vector norm to matrices.*/  
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

/* finance example */
C = {1.0 0.3 0.9,
     0.3 1.0 0.9,
     0.9 0.9 1.0};
R = NearestCorr(C);
print C, R[format=7.4];
quit;


/**********************************************************************************************

Exercise 10.6: Use the method in Section 10.4.2 to generate a random symmetric matrix of size
n. Run Higham’s algorithm and time how long it takes for n = 50, 100, 150, ... 300. Plot the time
versus the matrix size.

***********************************************************************************************/

%include "/folders/myfolders/SAS_Sim/Corr.sas";
proc iml;
load module=_all_; 
call randseed(1);
size = T( do(50, 300, 50) );
time = j(nrow(size), 1);
do i = 1 to nrow(size);
   t0 = time();
   N = size[i];
   v = j(N*(N+1)/2, 1);
   call randgen(v, "Uniform");
   A = sqrvech(v);
   x = NearestCorr(A);
   time[i] = time() - t0;
end;
create Timing var {"Size" "Time"}; append; close Timing;
quit;

proc sgplot data=Timing;
   series x=Size y=Time ;
   scatter x=Size y=Time / markerattrs=(symbol=star);
   xaxis grid; yaxis grid;
run;

/*http://blogs.sas.com/content/sgf/2014/10/17/symbols-in-sas-9-4-graphs-unlimited-possibilities/*/

/**********************/

