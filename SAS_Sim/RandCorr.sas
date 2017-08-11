/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Appendix C: Random Correlation Matrices

 This program saves functions to the SAS/IML storage library.
 To use these functions in a SAS/IML program, run this program
 and then load the functions, as follows:

 %include "/folders/myfolders/SAS_Sim/RandCorr.sas";
 proc iml;
 load module=_all_;
 <...call the functions...>

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


