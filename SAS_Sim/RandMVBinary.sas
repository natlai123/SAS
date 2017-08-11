/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 9: Advanced Simulation of Multivariate Data

 This program saves functions to the SAS/IML storage library.
 To use these functions in a SAS/IML program, run this program
 and then load the functions, as follows:

 %include "/folders/myfolders/SAS_Sim/RandMVBinary.sas";
 proc iml;
 load module=_all_;
 <...call the functions...>

 ***********************************************************************/

proc iml;

/********************************************************************
 Generating Multivariate Binary Variates
 *******************************************************************/

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


/***********************************************************************/

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

/***********************************************************************/

start RandMVBinary(N, p, Delta) global(pj, pk, djk);
   /* 1. Check parameters. Compute lower/upper bounds for all (j,k) */
   if ^CheckMVBinaryParams(LBound, UBound, p, Delta) then do;
      print "The specified correlation is invalid." LBound Delta UBound;
      STOP;
   end;

   q = 1 - p;  
   d = ncol(Delta);                          /* number of variables  */

   /* 2. Construct intermediate correlation matrix by solving the 
         bivariate CDF (PROBBNRM) equation for each pair of vars */
   R = I(d);
   do j = 1 to d-1;
      do k = j+1 to d;
         pj=p[j]; pk=p[k]; djk = Delta[j,k];      /* set global vars */
         /* TYPO in first edition: search for root on [-1,1] */
         *R[j,k] = bisection(-1, 1);              /* pre-12.1 */
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

store module=_all_;
quit;

