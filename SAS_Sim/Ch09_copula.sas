/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 9: Advanced Simulation of Multivariate Data (This chpater is fine)
 
 9.2 Generating Multivariate Binary Variates 
 		Solving for the Intermediate Correlations-important topic!!!
 9.3 Generating Multivariate Ordinal Variates
 9.4 Reordering Multivariate Data: The Iman-Conover Method
 9.5 Generating Data from Copulas
 		Normal Copula (NORTA Method)
 	
 	The goal is to simulate from a joint multivariate distribution with
	a specified correlation and a specified set of marginal distributions.

The beauty of the copula approach is that the copula separates the problem of modeling the marginals
from the problem of fitting the correlations between the variables. These two steps can be carried out
independently. The copula also makes it easy to simulate data from the model: Generate correlated
uniform variates according to the copula (as in Figure 9.11), and then generate the random variates
for each coordinate by applying the appropriate inverse distribution function: X=F^(-1)(U)
The generation of the random uniform variates depends on the choice of the copula, but for a normal
copula you can use the technique in Section 9.5.1.
	
Sklar’s theorem (Schmidt 2007) says that every joint distribution can be written as a function 
(called the copula) of the marginal distributions. The word “copula” means to link or join, 
and that is exactly what a mathematical copula does: It joins the marginal distributions in such 
a way as to form a joint distribution with a given correlation structure.
 
Fact 1: If F is the cumulative distribution function of a continuous random variable, X, 
then the random variable U=F(X) is distributed as U(0,1). The random variable U is called the
grade of X.
 
Fact 2: If U is a uniform random variable on 0; 1 and F is the cumulative distribution function of
a continuous random variable, then the random variable X=F^(-1)(U) is distributed as F.
 
*************************************************************************/

ods graphics on;

/********************************************************************
 
 
 9.2 Generating Multivariate Binary Variates
 
 	Emrich-Piedmonte Algorithm
    See "/folders/myfolders/SAS_Sim/RandMVBinary.sas"
    
*********************************************************************/

/* Algorithm from 
   Emrich, L.J.  and Piedmonte, M. R., 1991,
   "A Method for Generating High-Dimensional Multivariate Binary Variables",
   The American Statistician, 45, p. 302--304
   
The RANDMVBINARY function implements the Emrich-Piedmonte algorithm. Given a vector of
probabilities, p, and a desired correlation matrix Delta, the RANDMVBINARY function returns
an N  d matrix that contains zeros and ones. Each column of the returned matrix is a binary
variate, and the sample mean and correlation of the simulated data should be close to the 
specified parameters.
   
*/

proc iml;

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

start RandMVBinary(N, p, Delta) global(pj, pk, djk);
   /* 1. Check parameters. Compute lower/upper bounds for all (j,k) */
   if ^CheckMVBinaryParams(LBound, UBound, p, Delta) then do;
      print "The specified correlation is invalid." LBound Delta UBound;
      STOP;
   end;
   q = 1 - p;  
   d = ncol(Delta);                      /* d is the number of variables  */
   /* 2. Construct intermediate correlation matrix by solving the 
         bivariate CDF (PROBBNRM) equation for each pair of vars */
   R = I(d);
   do j = 1 to d-1;
      do k = j+1 to d;
         pj=p[j]; pk=p[k]; djk=Delta[j,k];      /* set global vars */
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

call randseed(1234);
p = {0.25 0.75 0.5};               /* expected values of the X[j]   */
Delta = { 1    -0.1  -0.25,
         -0.1   1     0.25,
         -0.25  0.25  1    };      /* correlations between the X[j] */
X = RandMVBinary(1000, p, Delta);

/* compare sample estimates to parameters */
mean = mean(X);
corr = corr(X);
print p, mean, Delta, corr[format=best6.], X;
quit;


/***********************************************************************/

/* Dissecting CheckMVBinaryParams(LBound, UBound, _p, Delta)*/

proc iml;
	_p = {0.25 0.75 0.5};			 /* expected values of the X[j] */
	Delta = { 1 -0.1 -0.25,
	-0.1 1 0.25,
	-0.25 0.25 1 };
   p = rowvec(_p);    q = 1 - p;         /* make p a row vector     */
   d = ncol(p);                          /* number of variables     */

   /* 1. check range of Delta; make sure p and Delta are feasible   */
   PP = p`*p;        PQ = p`*q;
   QP = q`*p;        QQ = q`*q;
   PRINT PP PQ QP QQ;
   A = -sqrt(PP/QQ); B = -sqrt(QQ/PP);   /* matrices                */
   PRINT A, B;
   LBound = choose(A>B,A,B);             /* elementwise max(A or B) */
   LBound[loc(I(d))] = 1;                /* set diagonal to 1       */
   A =  sqrt(PQ/QP); B =  sqrt(QP/PQ);
   PRINT A ,B;
   UBound = choose(A<B,A,B);             /* min(A or B)             */
   UBound[loc(I(d))] = 1;                /* set diagonal to 1       */
   PRINT LBound, UBound;

  kk= all(Delta >= LBound) & all(Delta <= UBound) ;
  PRINT KK;
  
  PRINT LBound Delta UBound; 
quit;

 /*If you change,say,Delta(1,3) from 0.25 to 0.75, 
  the target correlation is incorrect*/


/***********************************************************************

	9.3 Generating Multivariate Ordinal Variates

This section simulates data from a multivariate distribution with d > 2 
ordinal variables,X1, X2,..., Xd. Assume that the j th variable has Nj values, 
1,2, ... , Nj. Each random variable is defined by its probability mass 
function (PMF): P(X_j=i)=P_ij for i=1,2,...,Nj.

Specify Probabilities, Correlation Matrix, Simulate.

See "/folders/myfolders/SAS_Sim/RandMVOrd.sas"

***********************************************************************/

/* Figure 9.2: Plot for mean mapping method of generating correlated ordinal data */

proc iml;
call streaminit(1);
X = RandNormal(1e4, {0 0}, {1 0.5137, 0.5137 1});
create BivarNormal from X[c={"x" "y"}];
append from X;
close BivarNormal;

Y = {0 -0.842,
     0 -0.385,
     0 0.253,
 0.524 -0.842, 
 0.524 -0.385,
 0.524 0.253}; 
create inter from Y[c={p1 p2}];append from Y;close inter;

Y = {-1.2 -1.5,
     -1.2 -0.75,
     -1.2 -0.2,
     -1.2  0.8,
   0.05 -1.5,
   0.05 -0.75,
   0.05 -0.2,
   0.05  0.8,
      1 -1.5,
      1 -0.75,
      1 -0.2,
      1  0.8}; 
b1 = Y[,1]; b2 = Y[,2];
labl = {"(1,1)", "(1,2)", "(1,3)", "(1,4)", 
        "(2,1)", "(2,2)", "(2,3)", "(2,4)", 
        "(3,1)", "(3,2)", "(3,3)", "(3,4)" };

create label var {b1 b2 labl};append;close label;
quit;

data Bivar;
merge BivarNormal inter label;
run;

ods graphics / antialiasmax=10000;
proc sgplot data=Bivar noautolegend nocycleattrs;
   ellipse x=x y=y / alpha=0.25  transparency=0.5; 
   ellipse x=x y=y / alpha=0.50  transparency=0.5; 
   ellipse x=x y=y / alpha=0.75  transparency=0.5; 
   refline 0 0.524 / axis=x lineattrs=(thickness=2);
   refline -0.842 -0.385 0.253 / axis=y lineattrs=(thickness=2);
   scatter x=p1 y=p2 / markerattrs=(size=10 symbol=CircleFilled);
   scatter x=b1 y=b2 / markerattrs=(size=0) datalabel=labl
                    datalabelattrs=(size=14);
   xaxis display=(nolabel) min=-2 max=2.1 offsetmax=0;
   yaxis display=(nolabel) min=-2 max=2.1 offsetmax=0;
run;


/***********************************************************************/

/*******************************************************************
 
   SAS/IML functions for the implementation of 
   Kaiser, S. and Tr\"ager, D. and Leisch, F., (2011)
   "Generating Correlated Ordinal Random Values,"
   Technical ReportL University of Munich, Department of Statistics
   http://epub.ub.uni-muenchen.de/12157/
   
   "Mean mapping method" generalizes the Emrich-Piedmonte method in 9.2
   
********************************************************************/

proc iml;
/* OrdN: number of values for each variable */
start OrdN(P);
   return( countn(P, "col") );
finish;

/* OrdMean: E(A) = sum(i*p[i]) = expected value for each variable   */
start OrdMean(P);
   x = T(1:nrow(P));                 /* values of ordinal vars      */
   return( (x#P)[+,] );              /* expected values E(X)        */
finish;

/* OrdVar: variance for each variable */
start OrdVar(P);
   d = ncol(P);   m = OrdMean(P);
   x = T(1:nrow(P));                 /* values                      */
   var = j(1, d, 0);
   do i = 1 to d;
      var[i] = sum( (x - m[i])##2 # P[,i] );    /* defn of variance */
   end;
   return( var );
finish;

/* OrdCDF: Given PMF, compute CDF = cusum(PDF) */
start OrdCDF(P);
   d = ncol(P);
   cdf = j(nrow(P), ncol(P));        /* cumulative probabilities    */
   do i = 1 to d;
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
   /* QUANTILE function does not accept 1 as parameter */
   /* Replace 1 with missing value to prevent error */
   idx = loc(CDF > 1 - 2e-6 );
   CDF[idx] = .;
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
   /* find rho such that sum(probbnrm(g[,1], g[,2], rho))=target    */
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

/* test the function */
/*
Corr = {1.0  0.4  0.3,
        0.4  1.0  0.4,
        0.3  0.4  1.0 };
R = OrdMVCorr(P, Corr);
print R;
*/

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
/* Helper functions have been defined and saved. */


/********************************************************************
 
 
 	Generating Multivariate Ordinal Variates
 
 
 ******************************************************************************************/

/*Exercise 9.1: Let Z be an ordinal random variable with outcomes 1–4 and probability vector
f0.2, 0.15, 0.25, 0.4}. Use the “Table” distribution (see Section 2.4.5) to simulate 10,000 
observations from this distribution. Compute the sample mean and variance of the data.*/

/*************************************************/
		/* Answer to exercise 9.1 */
/*************************************************/

data Table(keep=x);
	call streaminit(4321);
	array p[4] _temporary_ (0.2 0.15 0.25 0.4);
	do i = 1 to 10000;
 	  x = rand("Table", of p[*]);           /* sample with replacement */
 	  output;
	end;
run;

proc means data=Table Mean Var;
run;

proc freq data = Table;
	tables x / plots=freqplot;
run;

/***********************************************************************


 	Generating Multivariate Ordinal Variates

Specify Probabilities, Correlation Matrix, Simulate.

***********************************************************************/

/* Define and store the functions for random ordinal variables */
%include "/folders/myfolders/SAS_Sim/RandMVOrd.sas"; 

proc iml;
load module=_all_;                     /* load the modules */
    /* P1   P2    P3  */
P = {0.25  0.50  0.20 ,
     0.75  0.20  0.15 ,
      .    0.30  0.25 ,
      .     .    0.40 };

/* expected values and variance for each ordinal variable */
Expected = OrdMean(P) // OrdVar(P);
varNames = "X1":"X3";
print Expected[r={"Mean" "Var"} c=varNames];
/* test the RandMVOrd function */
Delta = {1.0  0.4  0.3,
         0.4  1.0  0.4,
         0.3  0.4  1.0 };

call randseed(54321);
X = RandMVOrdinal(1000, P, Delta);
first = X[1:5,];
print first[label="First 5 Obs: Multivariate Ordinal"];

/***** for exercise *****/
create MVOrdSim from X[c={x1 x2 x3}]; append from X; close;
mv = mean(X) // var(X);   
corr = corr(X);
varNames = "X1":"X3";
print mv[r={"Mean" "Var"} c=varNames], corr;
quit;

/**************************/
/* Answer to exercise 9.2 */
/**************************/

proc freq data=MVOrdSim;
run;

/**********************/


/*************************************************************************************


 	Reordering Multivariate Data: The Iman-Conover Method
 
 
There is nothing random in the Iman-Conover method. That is, given a rank correlation, 
the algorithm constructs the same matrix M every time. Therefore, the randomness in a 
simulation comes from simulating the marginal distributions of Y . The Iman-Conover 
algorithm simply rearranges the elements of columns of your data matrix.
 
**************************************************************************************/

/* Use Iman-Conover method to generate MV data with known marginals
   and known rank correlation. */
  
proc iml;
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
   Q = root(corr(R));  		  /*Cholesky*/
   P = root(C); 
   S = solve(Q,P);            /* same as  S = inv(Q) * P; */
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

/* Step 1: Specify marginal distributions */
call randseed(1);
N = 100;
A = j(N,4);   y = j(N,1);
distrib = {"Normal" "Lognormal" "Expo" "Uniform"};
do i = 1 to ncol(distrib);
   call randgen(y, distrib[i]);
   A[,i] = y;
end;

/* Step 2: specify target rank correlation */
C = { 1.00  0.75 -0.70  0,
      0.75  1.00 -0.95  0,
     -0.70 -0.95  1.00 -0.2,
      0     0    -0.2   1.0};

X = ImanConoverTransform(A, C);	  
RankCorr = corr(X, "Spearman");	  /*X is a re-ordering of rows per col to cater to the rank corr*/
print RankCorr[format=5.2], A X;  /*Note,, for instance, that 1.1333089 appears once in A and X.*/
create MVData from X[c=("x1":"x4")];  append from X;  close MVData;
quit;

proc corr data=MVData Pearson Spearman noprob plots=matrix(hist);
   var x1-x4;
run;

proc univariate data=MVData;
   var x2 x3;
   histogram x2 / lognormal endpoints=(0 to 22 by 2);
   histogram x3 / exponential endpoints=(0 to 7);
   ods select histogram;
run;

/**********************/



/***********************************************************************************

 
 	9.5 Generating Data from Copulas


The word “copula” means to link or join, and that is exactly what a mathematical 
copula does: It creates a multivariate distribution by joining univariate marginal 
distributions.

************************************************************************************/

/*An Example to simulate data from a bivariate distribution (Normal Copula)

• The correlation between the variables is 0.6.
• The marginal distribution of the first variable is Gamma(4) with unit scale.
• The marginal distribution of the second variable is standard exponential.*/

proc iml;
call randseed(12345);
Sigma = {1.0  0.6,
         0.6  1.0};
Z = RandNormal(1e4, {0,0}, Sigma);	/*1e4x2 vectors*/
U = cdf("Normal", Z);           /* columns of U are U(0,1) variates, grade of X */
/*create GRADE from U[c=("U1":"U2")];  append from U;  close GRADE;	
quit;
proc univariate data = GRADE;
histogram U1;
histogram U2;
run;*/
gamma = quantile("Gamma", U[,1], 4);      /* gamma ~ Gamma(alpha=4) */
expo = quantile("Expo", U[,2]);           /* expo ~ Exp(1)          */
X = gamma || expo;
/*create GA_EXP var {gamma expo}; append;  close GA_EXP;	
quit;
proc univariate data = GA_EXP;
histogram gamma;
histogram expo;
run;*/

/* if Z~MVN(0,Sigma), corr(X) is often close to Sigma,
   where X=(X1,X2,...,Xm) and X_i = F_i^{-1}(Phi(Z_i)) */
rhoZ = corr(Z)[1,2];                    /* extract corr coefficient */
rhoX = corr(X)[1,2];
print rhoZ rhoX;
/*The correlation of the transformed variates (0.564) is not as close, and a two-sided 95% 
confidence interval for the correlation coefficient does not include 0.6*/

/* even though corr(X) ^= Sigma, you can often choose a target
   correlation, such as 0.6, and then choose Sigma so that corr(X)
   has the target correlation. */
Z0=Z; U0=U; X0=X;                             /* save original data */ 
Sigma = I(2);
rho = T( do(0.62, 0.68, 0.01) );
rhoTarget = j(nrow(rho), 1);
*do i = 1 to nrow(rho);
*   Sigma[1,2]=rho[i]; Sigma[2,1]=Sigma[1,2];	/*Sigma is based on rho*/
 *  Z = RandNormal(1e4, {0,0}, Sigma);           /* Z ~ MVN(0,Sigma) */
  * U = cdf("Normal", Z);                        /* U_i ~ U(0,1)     */
   *gamma = quantile("Gamma", U[,1], 4);         /* X_1 ~ Gamma(4)   */
   *expo = quantile("Expo", U[,2]);              /* X_2 ~ Expo(1)    */
   *X = gamma||expo;
   *rhoTarget[i] = corr(X)[1,2];            /* corr(X) = ? -> 0.64 (intermediate correlation) */
*end;
*print rho rhoTarget[format=6.4];

/*Finding the intermediate value often requires finding the root of an equation that involves the CDF
and inverse CDF. In fact, Step 2 in Section 9.2 is an example of using such an equation to generate
correlated multivariate binary data.*/

RankCorrZ = corr(Z0, "Spearman")[1,2];
RankCorrU = corr(U0, "Spearman")[1,2];
RankCorrX = corr(X0, "Spearman")[1,2];
print RankCorrZ RankCorrU RankCorrX;
/*Because rank correlations are invariant under monotonic transformations of the data, the rank
correlations of Z, U, and X are equal.*/

/*Visualizing the dependencies between each of the three sets of variables: the normal variates (Z),
the uniform variates (U), and the transformed variates (X).*/
Q = Z||U||X;
labels = {Z1 Z2 U1 U2 X1 X2};
create CorrData from Q[c=labels];
append from Q;
close CorrData;

/*	Z (bivariate normal) -> U (uniform) -> X (gamma, expo) */
proc sgplot data=CorrData(obs=1000);
title "Dependency between Z1 and Z2 (Bivariate Normal)";
   scatter x=Z1 y=Z2; 		/*rho = 0.6*/
run;
title;
proc sgplot data=CorrData(obs=1000);
title1 "Dependency between U1 and U2";
title2 "Visualizing the copula function";	/*Figure 9.11*/
   scatter x=U1 y=U2;
run;
title;
proc sgplot data=CorrData(obs=1000);
title "Dependency between Gamma and Exponential variates";
   scatter x=X1 y=X2;
run;
title;
/*Observe the dependency between the variates*/

/*Exercise 9.5: The FISHER option in the PROC CORR statement computes a confidence interval
for the correlation and can test the hypothesis that it has a specified value. Verify that the 95%
two-sided confidence interval for the correlation coefficient contains 0.6 and that the null hypothesis
(rho = 0.6) is not rejected. Repeat the analysis for the X1 and X2 variables.*/

proc corr data=CorrData fisher(rho0=0.6);
   var Z1 Z2;     
   ods select FisherPearsonCorr;
run;
proc corr data=CorrData fisher(rho0=0.6);
   var U1 U2;     
   ods select FisherPearsonCorr;
run;
proc corr data=CorrData fisher(rho0=0.6);
   var X1 X2;     
   ods select FisherPearsonCorr;
run;
/*All null hypotheses that rho0 = 0.6 are not rejected*/

/**********************/


/***********************************************************************/

/*Fitting and Simulating Data from a Copula Model*/

/***********************************************************************/

/* Step 1: Model the marginal distributions. 
   Step 2: fit normal copula
   Step 3: simulate data, transformed to uniformity */
  
proc print data = MVData;	/*The MVData is generated in the The Iman-Conover Method Section*/
run;
  
proc copula data=MVData;
   var x1-x4;
   fit normal;
   simulate / seed=1234  ndraws=100
              marginals=empirical  outuniform=UnifData; 
run;   /*The UnifData data set contains the transformed (uniformity) simulated data*/

/*Step 4: use inverse CDF to invert uniform marginals and 
 		  recover the modeled form of the marginals*/
data Sim;
set UnifData;
normal = quantile("Normal", x1);
lognormal = quantile("LogNormal", x2);
expo = quantile("Exponential", x3);
uniform = x4;
run;
/* Compare original distribution of data to simulated data */
proc corr data=MVData Spearman noprob plots=matrix(hist);
   title "Original Data";
   var x1-x4;
run;

proc corr data=Sim Spearman noprob plots=matrix(hist);
   title "Simulated Data";
   var normal lognormal expo uniform;
run;

/*You can simulate many samples from the COPULA function by using a single call. For example,
to simulate 100 samples of size 20, use the %SYSEVALF macro to generate the product of these
numbers:
%let N = 20;
%let NumSamples=100;
proc copula data=MVData;
...
simulate / ndraws=%sysevalf(&N*&NumSamples) outuniform=UnifData;
...
You can then use a separate DATA step to assign values for the usual SampleID variable, as follows:
data UnifData;
set UnifData;
SampleID = 1 + floor((_N_-1) / &N);
run;*/

/*Exercise 9.7: Use the FISHER option in the PROC CORR statement to compute 95% confidence
intervals for the Spearman correlation of the variables in the Sim data set. Show that these confidence
intervals contain the parameter values in Section 9.4 that generated the data.*/

proc corr data=Sim Spearman FISHER noprob;
   var normal lognormal expo uniform;
   ods select FisherSpearmanCorr;
run;

/**********************/

