/***********************************************************************
 
 
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Appendix B: Generating Multivariate Ordinal Variables

 This program saves functions to the SAS/IML storage library.
 To use these functions in a SAS/IML program, run this program
 and then load the functions, as follows:

 %include "/folders/myfolders/SAS_Sim/RandMVOrd.sas";
 proc iml;
 load module=_all_;
 <...call the functions...>
 
http://blogs.sas.com/content/iml/2011/09/12/storing-and-loading-modules.html

• The ORDMEAN function, which returns the expected values for a set of ordinal random
variables. The syntax is mean = OrdMean(P).

• The ORDVAR function, which returns the variance for a set of ordinal random variables. The
syntax is var = OrdVar(P).

• The RANDMVORDINAL function, which generates N observations from a multivariate
distribution of correlated ordinal variables. The syntax is X = RandMVOrd(N, P, Corr),
where Corr is the d  d correlation matrix.


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

