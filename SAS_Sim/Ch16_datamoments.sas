/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 16: Moment Matching and the Moment-Ratio Diagram
 ***********************************************************************/

ods graphics on;

/********************************************************************
 The Moment-Ratio Diagram
 *******************************************************************/

/* SAS code to display a moment-ratio diagram in the background of 
   a scatter plot of the sample skewness and kurtosis for simulated
   samples. */

/* Simple macro to return the full kurtosis from the excess kurtosis */
%macro Ex2Full(k); ((&k)+3)  %mend;
/* Simple macro to return the excess kurtosis from the full kurtosis */
%macro Full2Ex(k); ((&k)-3)  %mend;

/* Main macro: Plot the moment-ratio diagram for a given data set and a
   given annotation data set. By default, the (skew, kurt) scatter plot
   is fully opaque, but you can set the transparency in the macro call. */
%macro PlotMRDiagram(DS, annoDS, Transparency=0);
/* given a data set that contains variables KURT and SKEW, this macro
   adds the FULLKURT variable and labels for the three variables */
data &ds;
   set &ds;
   FullKurt = Kurt+3;
   label Kurt="Excess Kurtosis"
      FullKurt="Full Kurtosis"
      Skew="Skewness";
run;

proc sgplot data=&DS sganno=&annoDS noautolegend;
   scatter x=Skew y=Kurt /     transparency=&Transparency;
   scatter x=Skew y=FullKurt / y2axis transparency=1;  /* invisible */
   refline 0 / axis=x transparency=0.2;
   xaxis grid values=(-2.5 to 2.5 by 0.5);
   yaxis  reverse grid values=(-2 to 10);
   y2axis reverse grid values=( 1 to 13);
run;
%mend PlotMRDiagram;

/* For the book, create a second macro that plots the moment-ratio 
   diagram for a given data set and also displays reference lines for the 
   true skewness and kurtosis of the sample. */
%macro PlotMRDiagramRef(DS, annoDS, Transparency=0, 
                        SkewRef=1.152, KurtRef=2.122);
/* given a data set that contains variables KURT and SKEW, this macro
   adds the FULLKURT variable and labels for the three variables */
data &ds;
   set &ds;
   FullKurt = Kurt+3;
   label Kurt="Excess Kurtosis"
      FullKurt="Full Kurtosis"
      Skew="Skewness";
run;

proc sgplot data=&DS sganno=&annoDS noautolegend;
   scatter x=Skew y=Kurt /     transparency=&Transparency;
   scatter x=Skew y=FullKurt / y2axis transparency=1;  /* invisible */
   refline 0 / axis=x transparency=0.2;
   refline &SkewRef / axis=x transparency=0.5;
   refline &KurtRef / axis=y transparency=0.5;
   xaxis grid values=(-2.5 to 2.5 by 0.5);
   yaxis  reverse grid values=(-2 to 10);
   y2axis reverse grid values=( 1 to 13);
run;
%mend PlotMRDiagramRef;


/* For the book, create a second macro that plots the moment-ratio 
   diagram for a given data set and also displays reference lines for the 
   true skewness and kurtosis of the sample. */
%macro PlotMRDiagramRef(DS, annoDS, Transparency=0, 
                        SkewRef=1.152, KurtRef=2.122);
/* given a data set that contains variables KURT and SKEW, this macro
   adds the FULLKURT variable and labels for the three variables */
data &ds;
   set &ds;
   FullKurt = Kurt+3;
   label Kurt="Excess Kurtosis"
      FullKurt="Full Kurtosis"
      Skew="Skewness";
run;

proc sgplot data=&DS sganno=&annoDS noautolegend;
   scatter x=Skew y=Kurt /     transparency=&Transparency;
   scatter x=Skew y=FullKurt / y2axis transparency=1;  /* invisible */
   refline 0 / axis=x transparency=0.2;
   refline &SkewRef / axis=x transparency=0.5;
   refline &KurtRef / axis=y transparency=0.5;
   xaxis grid values=(-2.5 to 2.5 by 0.5);
   yaxis  reverse grid values=(-2 to 10);
   y2axis reverse grid values=( 1 to 13);
run;
%mend PlotMRDiagramRef;

/* Create the pieces of the annotation data set */
/* All computations are in terms of EXCESS kurtosis.
   Use Y2AXIS to add FULL kurtosis axis. */
%let xL = -2.4;
%let xR =  2.4;
%let yB = -2.0;
%let yT = 10.0;
%let yB2 = %sysevalf(&yB+3);
%let yT2 = %sysevalf(&yT+3);

/* annotation data set for BOUNDARY of feasible (skew, kurt) region */
data Boundary;
length function $12 Curve $12 LineColor $20;
retain DrawSpace 'DataValue'
       LineColor 'Black'
       Curve "Boundary";
function = "polyline"; 
x1=&xL; y1=1+x1**2; y1=%Full2Ex(y1); output;
function = "polycont";
drop x;
do x = &xL to &xR by 0.1;
   x1=x; y1=1+x1**2;  y1=%Full2Ex(y1);
   output;
end;
run;

/* annotation data set for BETA region of moment-ratio diagram */
data Beta;
length function $12 Curve $12 Label $24 LineColor $20;
retain DrawSpace 'DataValue'
       Display 'All'
       LineColor 'LightGray' 
       Curve "Beta";
function = "Text"; Anchor="Left ";
label = "Beta";
x1=&xL; y1=2+x1**2; y1=%Full2Ex(y1); output;
Transparency = 0.5; FillTransparency = 0.5;
FillColor = 'LightGray';
function = "polygon"; 
x1=&xL; y1=1+x1**2; y1=%Full2Ex(y1); output;
function = "polycont";
n=1;
drop x;
do x = &xL to &xR+0.05 by 0.1;
   x1=x; y1=1+x1**2; y1=%Full2Ex(y1);
   output;
end;
do x = &xR to &xL-0.05 by -0.1;
   x1=x; y1=3+1.5*x1**2; y1=%Full2Ex(y1);
   output;
end;
run;

/* annotation data set for LOGNORMAL curve of moment-ratio diagram */
data LogNormal;
length function $12 Label $24 Curve $12 LineColor $20;
retain DrawSpace 'DataValue'
       LineColor 'DarkGreen'
       Curve "LogNormal";
function = "Text"; Anchor="Right";
label = "LogN";
drop var var0;
var0 = 0.355; var=var0;
x1= (exp(var)+2)*sqrt(exp(var)-1);
y1 = exp(4*var) + 2*exp(3*var) + 3*exp(2*var) - 6;
output;
function = "polyline"; Anchor=" ";
output;
function = "polycont";
do var = Var0 to 0.005 by -0.005;
   x1= (exp(var)+2)*sqrt(exp(var)-1);
   y1 = exp(4*var) + 2*exp(3*var) + 3*exp(2*var) - 6;
   output;
end;
x1=0; y1=0;output;
run;

/* annotation data set for LOGNORMAL curve of moment-ratio diagram */
data MinusLogNormal;
set LogNormal;
x1 = -x1;
run;

/* annotation data set for GAMMA curve of moment-ratio diagram */
data GammaCurve;
retain DrawSpace 'DataValue';
length function $12 Label $24 Curve $12 LineColor $20;
retain LineColor 'Magenta' Curve "Gamma";
drop a a0 dx N i;
function = "Text"; Anchor="Right";
label = "Gam";
a0 = (2/&xR)**2;
a = a0;
x1 = 2/sqrt(a); y1= 6/a; output;
Transparency = 0.5;
function = "polyline"; Anchor=" ";  output;
dx=0.1; N=floor(&xR/dx);
function ="polycont";
do i = 2 to N;
   a = (2/(2/sqrt(a) - 0.1))**2;
   x1 = 2/sqrt(a); y1= 6/a;
   if (&xL<= x1 <=&xR) & (&yB <= y1 <= &yT) then output;
end;
x1=0; y1=0; output;
run;

/* annotation data set for special points in the moment-ratio diagram:
   The Exponential point, Normal point, Gumbel point, and points for the 
   t distribution of DOF 5,6,7,8,10,11,12. Also text that marks the 
   invalid region. */
data MRPoints;
retain DrawSpace 'DataValue';
length function $12 Label $24 Curve $12;
function = "Text"; Curve="Exponential";
Label="E"; x1 = 2; y1= 6;
output;
Label="N"; x1 = 0; y1= 0; Curve="Normal";
output;
Label="G"; x1 = 1.14; y1= 2.4; Curve="Gumbel";
output;
Curve="T";
drop nu;
do nu=5 to 8;
   Label=cats("T",nu); x1=0; y1 = 6/(nu-4); output;
end;
do nu=10 to 12;
   Label="."; x1=0; y1 = 6/(nu-4); output;
end;
Curve="Region";
do x1=-2,2;
  function="Text";     Label="Invalid"; y1=-1; output;
  function="TextCont"; Label="Region"; output;
end;
run;

/* annotation data set for showing the Johnson system */
data JohnsonPoints;
retain DrawSpace 'DataValue';
length function $12 Label $24 Curve $12;
function = "Text"; 
Curve="Region";
do x1=-2,2;
  function="Text";     Label="Invalid"; y1=-1; output;
  function="TextCont"; Label="Region"; output;
end;
do x1=-1.5,1.5;
  function="Text";     Label="S(*ESC*){sub 'B'}"; y1=2; output;
end;
do x1=-1,1;
  function="Text";     Label="S(*ESC*){sub 'U'}"; y1=7; output;
end;
run;


/* Concatenate the pieces of the moment-ratio diagram */
data anno;
set Beta Boundary LogNormal GammaCurve MRPoints;
run;

data JohnsonAnno;
set Boundary LogNormal MinusLogNormal JohnsonPoints;
run;

/************************************************/
/* Example of creating a moment-ratio diagram   */
/************************************************/

/* simulate 100 samples of size N=50 from a Gamma(4) distribution */
%let N = 50;
%let NumSamples = 100;
data Gamma(keep=x SampleID);
call streaminit(12345);
a = 4;
do SampleID = 1 to &NumSamples;
   do i = 1 to &N;
      x = rand("Gamma", a);
      output;
   end;
end;
run;

/* compute the skewness and kurtosis of each sample */
proc means data=Gamma noprint;
  by SampleID;
  var x;
  output out=MomentsGamma skew=Skew kurt=Kurt;
run;

title "Moment-Ratio Diagram";
title2;
%PlotMRDiagram(MomentsGamma, anno, Transparency=1);
title; title2;

/* Generate Johnson's SU and SB diagram */
title "Johnson's System";
title2;
%PlotMRDiagram(MomentsGamma, JohnsonAnno, Transparency=1);
title; title2;

/**********************/
/* Answer to exercise */
/**********************/
/* show that the Gamma(4) and Gumbel distribution 
   have similar shapes */
data pdf;
label Gamma4 = "Gamma(4)"
      Gumbel = "Gumbel(3.1, 1.56)";
drop a z sigma mu;
a = 4;
/* compute Gumbel parameters (mu, sigma) so that 
   (1) mean(Gamma(4)) = mean(Gumbel(mu,sigma)), or
       4 = mu+sigma*gamma, where gamma=0.5772...
   (2) var(Gamma(4)) = var(Gumbel(mu,sigma)), or
       4 = (pi*sigma)**2 / 6 */
sigma = sqrt(6*a)/constant("Pi");
mu = a - sigma*constant("Euler");
do x=0 to 10 by 0.1;
   Gamma4 = pdf("Gamma", x, a);
   z = (x-mu)/sigma;
   Gumbel = (1/sigma) * exp(-z - exp(-z));         /* pdf of Gumbel */
   output;
end;
run;

proc sgplot data=pdf;
   series x=x y=Gamma4;
   series x=x y=Gumbel;
   yaxis label="Density";
run;
/**********************/


/********************************************************************
 Moment Matching As a Modeling Tool
 *******************************************************************/


/********************************************************************
 Plotting Variation of Skewness and Kurtosis on a Moment-Ratio Diagram
 *******************************************************************/

%let N = 200;
/* data step used to generate the fake data */
data MRData(keep=x);
call streaminit(12345);
do i = 1 to &N;
   x = rand("Gamma", 4);
   output;
end;
run;
data MRData;
input x @@;
datalines;
 4.54 4.57 7.18 5.03 3.70  4.11 2.79 2.30 1.75 2.08 1.70 4.83 4.57 11.51
 2.36 6.47 3.86 7.14 6.96  3.59 5.81 5.66 7.07 2.29 4.42 1.01 6.49  2.59
 5.36 3.90 6.50 4.97 5.29  4.83 4.62 3.04 3.67 3.68 4.09 4.95 1.66  4.07
 4.31 2.20 2.29 6.38 3.58  4.11 2.50 2.94 1.47 6.77 9.54 2.14 2.84  3.25 
 2.65 5.62 4.41 1.18 3.76  0.95 4.67 5.17 1.08 4.09 2.84 1.96 6.23  3.48
 5.41 6.17 7.71 2.84 2.32  4.40 3.21 2.22 0.56 3.53 3.03 1.35 1.97  1.61
 3.02 2.49 4.06 2.82 6.22 13.18 4.04 3.56 3.65 2.48 3.90 3.44 5.11  3.93
 1.69 6.12 2.75 4.60 5.97  1.75 2.01 4.02 2.34 7.20 0.69 3.10 3.92 11.71
 1.56 3.03 4.01 2.61 2.88  5.97 6.24 7.89 5.11 3.36 1.56 7.50 2.16  1.33
 1.42 2.76 2.17 3.41 3.47  3.15 4.08 2.29 3.95 5.42 1.77 2.80 9.69  3.95 
 9.04 1.38 2.61 1.14 5.24  1.42 2.06 5.46 3.72 9.80 2.77 1.71 7.25  2.86
 5.15 2.94 3.00 1.90 4.61  3.64 7.54 1.85 2.50 0.95 1.14 1.85 3.97  6.06
 4.47 6.69 2.02 6.04 5.63  5.17 2.12 3.70 1.72 3.50 3.73 8.03 6.87  5.01
 1.07 5.17 4.97 2.99 2.45  5.82 5.50 5.34 4.65 4.73 2.91 4.75 1.45  4.27
 3.71 3.16 5.82 6.24 
;

proc means data=MRData N min max mean std skew kurt maxdec=3;
run;

proc univariate data=MRData;
   histogram x / midpoints=(0 to 13);
   ods select Histogram;
run;

/***********************************************************************/

/* use SURVEYSELECT to generate bootstrap resamples */
proc surveyselect data=MRData out=BootSamp noprint
     seed=12345 method=urs rep=100 rate=1;
run;

proc means data=BootSamp noprint;
   by Replicate;
   freq NumberHits;
   var x;
   output out=MomentsBoot skew=Skew kurt=Kurt;
run;

title "Moment-Ratio Diagram";
title2 "100 Bootstrap Resamples, N=200";
%PlotMRDiagramRef(MomentsBoot, anno, Transparency=0.4);
title; title2;

/********************************************************************
 Fitting a Gamma Distribution to Data
 *******************************************************************/

proc univariate data=MRData;
   var x;
   histogram x / gamma(theta=0);
   inset skewness kurtosis / format=5.3;
   ods select Histogram ParameterEstimates;
run;

%let N = 200;                              /* match the sample size */
%let NumSamples = 100;
data Gamma(keep=x SampleID);
call streaminit(12345);
do SampleID = 1 to &NumSamples;
   do i = 1 to &N;
      x = 1.12 * rand("Gamma", 3.58);
      output;
   end;
end;
run;

proc means data=Gamma noprint;
   by SampleID;
   var x;
   output out=MomentsGamma mean=Mean var=Var skew=Skew kurt=Kurt;
run;

title "Moment-Ratio Diagram";
title2 "&NumSamples Samples from Fitted Gamma, N=&N";
%PlotMRDiagramRef(MomentsGamma, anno, Transparency=0.4);
title; title2;

/**********************/
/* Answer to exercise */
/**********************/
data GammaEx(keep=x N SampleID);
call streaminit(12345);
do N = 200, 10000;
   do SampleID = 1 to 100;
      do i = 1 to N;
         x = 1.12 * rand("Gamma", 3.58);
         output;
      end;
   end;
end;
run;

proc means data=GammaEx noprint;
   by N SampleID;
   var x;
   output out=MomentsGammaEx skew=Skew kurt=Kurt;
run;

proc sgplot data=MomentsGammaEx;
   scatter x=Skew y=Kurt / group=N;
   refline  1.152 / axis=x;
   refline 2.122 / axis=y ;
   xaxis grid;
   yaxis  reverse grid;
run;
/**********************/


/**********************/
/* Answer to exercise */
/**********************/
proc univariate data=MRData;
   histogram x / Gumbel;
run;

data GumbelEx(keep=x SampleID);
call streaminit(12345);
mu = 3.058068; sigma = 1.639464;
do SampleID = 1 to 100;
   do i = 1 to 200;
      E = rand("Exponential"); 
      x = mu + sigma*(-log(E));
      output;
   end;
end;
run;

proc means data=GumbelEx noprint;
   by SampleID;
   var x;
   output out=MomentsGumbelEx skew=Skew kurt=Kurt;
run;

proc sgplot data=MomentsGumbelEx;
   scatter x=Skew y=Kurt;
   refline  1.152 / axis=x;
   refline 2.122 / axis=y ;
   xaxis grid;
   yaxis  reverse grid;
run;
/**********************/


/********************************************************************
 The Johnson System
 *******************************************************************/

proc univariate data=MRData;
   var x;
   histogram x / sb(fitmethod=Moments theta=est sigma=est);
   ods output ParameterEstimates=PE;
   ods select Histogram ParameterEstimates;
run;

/* The parameter estimates are stored in the first four observations 
   the PE data set. Transpose the observations to create variables 
   named Gamma, Delta, Sigma, and Theta that contain the estimates */
proc transpose data=PE(obs=4 keep=Symbol Estimate) out=Wide;
   id Symbol;
   var Estimate;
run;

/* Read the parameter estimates in the Wide data set and simulate 
   from SB distribution with those parameter values */
%let N = 200;
%let NumSamples = 100;

data SB(keep=x SampleID);
call streaminit(12345);
set Wide;      /* read estimates for gamma, delta, theta, and sigma */
/* use the estimates to simulate random data from Johnson's Sb      */
do SampleID = 1 to &NumSamples;
   do i = 1 to &N;
      Y = (rand("Normal")-gamma) / delta;
      expY = exp(Y);
      x = ( sigma*expY + theta*(expY + 1) ) / (expY + 1);
      output;
   end;
end;
STOP;
run;

/* Compute the sample moments and plot them on the M-R diagram */
proc means data=SB noprint;
   by SampleID;
   var x;
   output out=MomentsSB mean=Mean var=Var skew=Skew kurt=Kurt;
run;

title "Moment-Ratio Diagram";
title2 "&NumSamples Samples from Johnson SB, N=&N";
%PlotMRDiagramRef(MomentsSB, anno, Transparency=0.4);
title; title2;

/********************************************************************
 Fleishman's Method
 *******************************************************************/

data FleishmanBoundary;
length function $12 Curve $12 LineColor $20;
retain DrawSpace 'DataValue'
       LineColor 'Brown'
       LinePattern 'Dash'
       Curve "FBoundary";
function = "polyline"; 
x1=&xL; y1= 1.86832+1.58837*x1**2; y1=%Full2Ex(y1); output;
function = "polycont";
drop x;
do x = &xL to &xR by 0.1;
   x1=x; y1=1.86832+1.58837*x1**2;  y1=%Full2Ex(y1);
   output;
end;
run;

data FleishmanLabel;
length function $12 Curve $12; /* $ */
retain DrawSpace 'DataValue'
       Curve "FBoundary";
function = "Text"; Anchor="Left "; width=100;
label = "Fleishman Region";
x1=-2; y1=8; output;
run;

/* Moment-Ratio diagram with Fleishman region */
data Fanno;
set Beta Boundary 
    FleishmanBoundary FleishmanLabel
    LogNormal GammaCurve MRPoints;
run;

/***************************************************/
title "Moment-Ratio Diagram with Fleishman Region";
%PlotMRDiagram(MomentsGamma, Fanno, Transparency=1);
title; title2;


/***************************************************/
proc iml;

/*********************************************************/
/***** Modules for descriptive statistics          *******/
/*********************************************************/

/* Formulas for skew and kurtosis: 
   Kendall, M.G., Stuart, A. (1969) The Advanced Theory of Statistics,
             Volume 1, p. 85 */

start Skewness(X);
   /* Compute sample skewness for columns of X */
   n = (x^=.)[+,];          /* or countn(x, "col") for SAS/IML 9.22 */
   c = x - x[:,];           /* or x - mean(x) for SAS/IML 9.22      */
   k2 = (c##2)[+,] / (n-1); /* variance = k2                        */
   k3 = (c##3)[+,] # n / ((n-1)#(n-2));
   skew = k3 / k2##1.5;
   return( skew );
finish;

start Kurtosis(X);
   /* Compute sample (excess) kurtosis for columns of X */
   n = (x^=.)[+,];          /* or countn(x, "col") for SAS/IML 9.22 */
   c = x - x[:,];           /* or x - mean(x) for SAS/IML 9.22      */
   c2 = c##2;
   m2 = c2[+,]/n;           /* 2nd sample central moment            */
   m4 = (c2##2)[+,]/n;      /* 4th sample central moment            */

   k2 = m2 # n / (n-1);     /* variance = k2                        */
   k4 = n##2 /((n-1)#(n-2)#(n-3)) # ((n+1)#m4 - 3*(n-1)#m2##2);
   kurtosis = k4 / k2##2;   /* excess kurt = k4 / k2##2             */
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

/*********************************************************/
/* Modules to use Newton's method to fit Fleishman model */
/*********************************************************/
/* Given cubic coefficients, you can determine the variance,
   skewness, and kurtosis of the Fleishman distribution, (v,s,k). 
   Given coefficients and a target for skewness (s0) and
   kurtosis (k0), this function returns the vector {v-1, s-s0, k-k0}. 
   Consequently, if coefficients for which this function returns
   (0,0,0) correspond to a Fleishman distribution with unit variance,
   skewness s0, and kurtosis k0.
    */
/* Compute variance, skewness, and kurtosis from cubic coefficients */
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
start FlFunc(x) global (g_target); /* g_target=(skewness, kurtosis) */
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
    You must supply the functions that compute 
    the function and the Jacobian matrix.
    Input: x0 is the starting guess
           optn[1] = max number of iterations
           optn[2] = convergence criterion for || f ||
    Output: x contains the approximation to the root */
start Newton(x, x0, optn);
   maxIter = optn[1]; converge = optn[2];
   x = x0;
   f = FlFunc(x);
   do iter = 1 to maxIter while(max(abs(f)) > converge);
      J = FlDeriv(x); 
      delta = -solve(J, f);                    /* correction vector */
      x = x + delta;                           /* new approximation */
      f = FlFunc(x);         
   end;
   /* return missing if no convergence */
   if iter > maxIter then x = j(nrow(x0),ncol(x0),.);
finish Newton;


/*********************************************************/
/***** Modules to fit and simulate from Fleishman model **/
/*********************************************************/

start FleishmanIC(skew, kurt);
c = j(3,1);
c[1] = 0.95357 -0.05679*kurt + 0.03520*skew##2 + 0.00133*kurt##2; /* c1 = Quad(skew, kurt) */
c[2] = 0.10007*skew + 0.00844*skew##3;          /* c2 = Cubic(skew) */
c[3] = 0.30978 -0.31655*c[1];                   /* c3 = Linear(c1)  */
return (c);
finish;


/* given data x, find coefficients c = {c0, c1, c2, c3}
   so that the standardized data are modeled by 
   c0+c1*Z+c2*Z##2+c3*Z##3 for Z~N(0,1) */
start FitFleishman(x);
   /* 1. Find sample moments */
   m = Moments(x);
   skew = m[3]; kurt = m[4];                     /* excess kurtosis */
   return( FitFleishmanFromSK(skew, kurt) );
finish;


start FitFleishmanFromSK(skew, kurt) global (g_target);
   /* 1. check that (skew,kurt) is in the obtainable region */
   if kurt < -1.13168+1.58837*skew##2 then return({. . . .});
   /* 2. Initial guess for nonlinear root finding */
   x0 = FleishmanIC(skew, kurt);
   optn = { 25, 1e-5 }; /* maximum iterations, convergence criterion */

   /* 3. Find cubic coefficients (c1, c2, c3) so that 
         c0+c1*Z+c2*Z##2+c3*Z##3 has target (skew,kurt),
         where c0 = -c2. */
   g_target = skew||kurt;                    /* set global variable */
   run Newton(coef, x0, optn);
   return( -coef[2] // coef );               /* c0 = -c2            */
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

/* Function that returns ordered pairs on a uniform grid of points.
   Return value is an (Nx*Ny x 2) matrix */
start Expand2DGrid( _x, _y );
   x  = colvec(_x); y  = colvec(_y);
   Nx = nrow(x);    Ny = nrow(y);
   x = repeat(x, Ny);
   y = shape( repeat(y, 1, Nx), 0, 1 );
   return ( x || y );
finish;

/******************************************************/
/* The following modules implement the Vale-Maurelli method
   of generating multivariate nonnormal data:
   Vale, C. and Maurelli, V. (1983) "Simulating multivariate
   nonnormal distributions," Psychometrika, 48(3):465-471
*/
/******************************************************/
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
   realIdx = loc(abs(roots[,2]<1e-8)); /* extract the real root(s)  */
   r = roots[realIdx,1][1];            /* return smallest real root */
   return (r);
finish;

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

start RandValeMaurelli(N, R, skew, kurt);
   /* compute Fleishman coefficients that match marginal moments */
   c = j(ncol(R), 4);
   do i = 1 to ncol(R);
      c[i,] = T( FitFleishmanFromSK(skew[i], kurt[i]) );
   end;
   /* adjust correlation matrix (same algorithm as VMTargetCorr) */
   V = j( nrow(R), ncol(R), 1);
   do i = 2 to nrow(R);
      do j = 1 to i-1;
         V[i,j] = SolveCorr(R[i,j], c[i,], c[j,]);
         V[j,i] = V[i,j];
      end;
   end;

   call eigen(D, U, V);      /* eigenvector decomposition: V=U*D*U` */
   F = sqrt(D`) # U;         /* F is a square root matrix for V     */
   X = j(N, ncol(R));
   call randgen(X, "Normal");     /* uncorrelated normals           */
   Y = X*F`;                      /* correlated normals             */
   do i = 1 to ncol(R);
      w = Y[,i];                  /* apply Fleishman transformation */
      X[,i] = c[i,1] + w#(c[i,2] + w#(c[i,3] + w#c[i,4])); /* reuse X */
   end;
   return(X);
finish;

store module=_all_;
quit;
/* Helper functions have been defined and saved. */


/* Define and store the Fleishman modules */
*%include "C:\<path>\RandFleishman.sas"; 
%include "RandFleishman.sas"; 

/* Use Fleishman's cubic transformation to model data */
proc iml;
load module=_all_;                        /* load Fleishman modules */
use MRData;  read all var {x};  close MRData;

c = FitFleishman(x);      /* fit model to data; obtain coefficients */

/* Simulate 100 samples, each with nrow(x) observations */
call randseed(12345);
Y = RandFleishman(nrow(x), 100, c);
Y = mean(x) + std(x) * Y;                 /* translate and scale Y  */

varNames = {"Mean" "Var" "Skew" "Kurt"};
m = T( Moments(Y) );
create MomentsF from m[c=varNames];  append from m;  close MomentsF;
quit;

title "Moment-Ratio Diagram";
title2 "&NumSamples Samples from Fleishman Distribution, N=&N";
%PlotMRDiagramRef(MomentsF, Fanno, Transparency=0.4);
title; title2;

/********************************************************************
 Comparing Simulations and Choosing Models
 *******************************************************************/


/******************************************************/
/* For fun, overlay (skew, kurt) for all four methods */
/******************************************************/
data All;
set MomentsBoot(in=boot)
    MomentsF(in=fleish)
    MomentsGamma(in=gamma)
    MomentsSB(in=sb);
length Group $9.; /* $ */
if boot then Group="Boot";
else if fleish then Group="Fleishman";
else if gamma then Group="Gamma";
else if sb then Group="SB";
run;

proc sgplot data=All;
scatter x=skew y=kurt / group=Group transparency=0.5 
        markerattrs=(symbol=circlefilled size=14);
run;
/*********************************************/

/********************************************************************
 The Moment-Ratio Diagram As a Tool for Designing Simulation Studies
 *******************************************************************/

proc iml;
/* Assume X is an N x p matrix that contains p samples of size N. 
   Return a 2 x p matrix where the first row is xbar - t*s/sqrt(N) and 
   the second row is xbar + t*s/sqrt(N), where s=Std(X) and t is the 
   1-alpha/2 quantile of the t distribution with N-1 degrees of freedom */
start NormalCI(X, alpha);
   N = nrow(X);
   xbar = mean(X);
   t = quantile("t", 1-alpha/2, N-1);
   dx = t*std(X)/sqrt(N);
   return ( (xbar - dx) // (xbar + dx) );
finish;

call randseed(12345);
load module=_all_;                         /* load Fleishman modules */

/* 1. Create equally spaced grid in (skewness, kurtosis) space:
      {0, 0.2, 0.4,..., 2.4} x {-2, -1.5, -1,..., 10} */
sk = Expand2DGrid( do(0,2.4,0.2), do(-2,10,0.5) );
skew = sk[,1]; kurt = sk[,2];
/* 1a. Discard invalid pairs */
idx =  loc(kurt > (-1.2264489 + 1.6410373# skew##2));
sk = sk[idx, ];                              /* keep these values    */
skew = sk[,1]; kurt = sk[,2];

/* for each (skew, kurt), use simul to estimate coverage prob for CI */
N = 25;
NumSamples = 1e4;
Prob = j(nrow(sk), 1, .);
do i = 1 to nrow(sk);
   c = FitFleishmanFromSK(skew[i], kurt[i]); /* find Fleishman coefs */
   X = RandFleishman(N, NumSamples, c);      /* generate samples     */
   CI = NormalCI(X, 0.05);                   /* compute normal CI    */
   Prob[i] = ( CI[1,]<0 & CI[2,]>0 )[:];     /* mean of indicator var*/
end;   
create MRGrid var {"Skew" "Kurt" "Prob"};  append;  close;
quit;

proc rsreg data=MRGrid plots=Contour;
   model Prob = Kurt Skew;
   ods select Contour;
run;

/********************************************************************
 Extensions to Multivariate Data
 *******************************************************************/

/* Define and store the Vale-Maurelli modules */
*%include "C:\<path>\RandFleishman.sas"; 
%include "RandFleishman.sas"; 

proc iml;
load module=_all_;      /* load Fleishman and Vale-Maurelli modules */

/* Find target correlation for given skew, kurtosis, and corr.
   The (skew,kurt) correspond to Gamma(4), Exp(1), and t5.   */ 
skew = {1   2 0};
kurt = {1.5 6 6};
R = {1.0  0.3 -0.4,
     0.3  1.0  0.5,
    -0.4  0.5  1.0 };

V = VMTargetCorr(R, skew, kurt);
print V[format=6.4];
/* generate samples from Vale-Maurelli distribution */
N = 10000;
call randseed(54321);
X = RandValeMaurelli(N, R, skew, kurt);
create VM from X[c=("x1":"x3")]; append from X; close VM;
quit;

proc means data=VM N Mean Var Skew Kurt;
run;

proc corr data=VM(obs=1000) noprob plots(maxpoints=NONE)=matrix(histogram);
ods select PearsonCorr MatrixPlot;
run;
