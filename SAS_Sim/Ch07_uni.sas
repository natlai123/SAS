/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 7: Advanced Simulation of Univariate Data
 
 Adding Location and Scale Parameters				
 Inverse CDF Sampling: Compare the three plotting options
 Finite Mixture Distributions
 The Contaminated Normal Distribution
 The Acceptance-Rejection Technique
 
 Topics I skipped:
 Simulating Surival Data
 Simulating from Less Common Univariate Distributions


Marke. Johnson(1987) p19: General methods for continues unovariate generation are:

	Inverse CDF Sampling 
	Transformation based on special distributional relationships
	Acceptance-rejection methods 

 
 ***********************************************************************/

%macro ODSOff(); /* Call prior to BY-group processing */
ods graphics off;
ods exclude all;
ods noresults;
%mend;

%macro ODSOn(); /* Call after BY-group processing */
ods graphics on;
ods exclude none;
ods results;
%mend;

/********************************************************************

/********************************************************************
 
 Adding Location and Scale Parameters
 
 	Exponential
 	Gamma 
 	Uniform
 	Lognormal

*******************************************************************/

/*
Fortunately, you can include location and scale parameters even when a function does not explicitly
support them:
• To simulate a random variate Y with location parameter  and scale parameter , use the
RAND function to generate the standard random variate X and form Y = theta + sigma*X.
• To compute a PDF that incorporates location and scale, call the PDF function as (1/sigma)*
PDF("DistribName", (x-theta)/sigma).
• To compute a CDF that incorporates location and scale, call the CDF function as
CDF("DistribName", (x-theta)/sigma).
• To compute a quantile that incorporates location and scale, call the QUANTILE function as x
= theta + sigma*QUANTILE("DistribName", p), where p is the probability such that
P(x<=X) = p.
*/


/**********************/
/* Answer to exercise */
/**********************/

/*Exercise 7.1: Simulate 1,000 exponential variates with scale parameter 2. Overlay the
corresponding PDF. Compute the sample median. Is it close to the theoretical value, which
is 2ln(2) ~= 1.386?*/

data Expon;
call streaminit(0);
do i = 1 to 1000; 		 /*1000 samples gives 1.28. 10000 gets real close tho*/
   x=2*rand("expo");
   output;
end;
run;

ods graphics on;
proc univariate data=Expon;
   var x;
   histogram x/exponential(sigma=2) midpoints=(0.5 to 15);
   ods select BasicMeasures Histogram;
run;


/*Exercise 7.2: Simulate 1,000 lognormal variates by simulating Y  N(1.5, 3) and applying the
transformation X = exp**(Y) . Use PROC UNIVARIATE to verify that X is lognormally distributed
with parameters 1.5 and 3.*/

/*See http://blogs.sas.com/content/iml/2014/06/04/simulate-lognormal-data-with-specified-mean-and-variance.html*/

data convert;
m = 1.5; v = 3;                /* mean and variance of Y */
phi = sqrt(v + m**2);
mu    = log(m**2/phi);          /* mean of log(Y)    */
sigma = sqrt(log(phi**2/m**2)); /* std dev of log(Y) */
run;
 
proc print noobs; run;

data lognormal;
call streaminit(1);
keep x y i;
m = 1.5; v = 3;      			/* specify mean and variance of Y */
phi = sqrt(v + m**2);
mu    = log(m**2/phi);
sigma = sqrt(log(phi**2/m**2));
do i = 1 to 1000;
   x = rand('Normal', mu, sigma);
   y = exp(x);
   output;
end;
run;

ods select Moments Histogram ParameterEstimates;
proc univariate data=lognormal;
   var y;
   histogram y / lognormal(zeta=EST sigma=EST);
run;



 /********************************************************************
  
  
 	Inverse CDF Sampling
 
 
 *******************************************************************/


/* Inverse CDF algorithm */

/*If you know the cumulative distribution function (CDF) of a probability distribution, then you can
always generate a random sample from that distribution. However, this method of sampling can be
computationally expensive unless you have a formula for the inverse CDF. 

This sampling technique uses the fact that a continuous CDF, F , is a one-to-one mapping of the
domain of the CDF into the interval (0, 1). Therefore, if U is a random uniform variable on (0, 1),
then X = F^-1(U) has the distribution F (Think of it as normally we start from the x axis, go up the 
CDF curve and to the left onto the y axis. Now reverse the procedure and start from drawing numbers 
between 0 and 1 on the y axis and then map to the x axis.)

The following DATA step generates random values from the exponential distribution by generating
random uniform values from U(0, 1) and applying the inverse CDF of the exponential distribution.*/

%let N = 100;           /* Varying the sample size and check the KS CV AD Statistics*/
data Exp(keep=x);
call streaminit(12345);
do i = 1 to &N;
   u = rand("Uniform");
   x = -log(1-u);	 	/* Inverse CDF function of the Exp Distribution (calculation involved)*/
   output;
end;
run;

proc univariate data=Exp;
   histogram x / exponential(sigma=1) endpoints=0 to 6 by 0.5;	/*PDF of Exp Distribution*/
   cdfplot x / exponential(sigma=1);							/*CDF of Exp Distribution*/
   ods select GoodnessOfFit Histogram CDFPlot; 	
run; 


/*Exercise 7.5: The CDF function F(x) = x^2 defines a distribution on the interval [0, 1]. 
Use the inverse function to generate 100 random values from F . Use PROC UNIVARIATE to 
plot the empirical CDF, which should approximate F.*/

%let N = 100; 
data quad(keep=x);
call streaminit(12345);
do i = 1 to &N;
	u = rand("Uniform");
	x = sqrt(u);
	output;
end;
run;

proc sort data = quad;  /*For processing CDF*/
by x;
run;

proc univariate data = quad;
   histogram x / midpoints=(0 to 1 by 0.05);	/*control the bin size for PDF f(x) = 2x*/
   cdfplot x;							
   ods select Histogram CDFPlot;
run; 


/*Plotting the theoretical PDF*/

data PDF;				 		
do T = 0 to 1 by 0.01;
   Y_pdf = 2*T;
   output;
end;
run;

data Cont_pdf;
merge quad PDF;
run;

proc template;
define statgraph HistPDF;
dynamic _Title _binstart _binstop _binwidth;
begingraph;
   entrytitle _Title;
   layout overlay / xaxisopts=(linearopts=(viewmax=_binstop));
   histogram X / scale=density endlabels=true xvalues=leftpoints 
         binstart=_binstart binwidth=_binwidth;
   seriesplot x=T y=Y_pdf / name='PDF' legendlabel="PDF" 
         lineattrs=(thickness=2);
   discretelegend 'PDF';
   endlayout;
endgraph;
end;
run;

proc sgrender data=Cont_pdf template=HistPDF;
dynamic _Title="Sample PDF (2x) from the CDF Distribution (x^2), N=&N"
   _binstart=0                        /* left endpoint of first bin */
   _binstop= 1                      /* right endpoint of last bin */
   _binwidth=0.02;                       /* width of bins              */
run;

/*Plotting the Simulated CDF*/

proc sort data = quad;  /*For processing CDF*/
by x;
run;

data ecdf;
set quad nobs=totalobs;
   ecdf_x = _n_ / totalobs;
   n = _n_;
run;

/*Compare the three CDF plotting options*/
proc univariate data = quad;
   histogram x / midpoints= (0 to 1 by 0.025);	
   cdfplot x;							
   ods select Histogram CDFPlot;
run; 

proc sgplot data=ecdf;
   series y=ecdf_x x=x / lineattrs=(color=red);
run;

title "Empirical CDF";
title2 "STEP and FRINGE Statements";
proc sgplot data=ecdf noautolegend;
   step x=x y=ECDF_X;          /* variable names created by PROC UNIVARIATE */
   fringe x;
   xaxis grid label="x" offsetmin=0 offsetmax=0.05;
   yaxis grid min=0 label="Cumulative Proportion";
run;


/***********************************************************************
	
	7.4.2 Root Finding and the Inverse Transformation Algorithm

***********************************************************************/

/*Even if you cannot invert the CDF, you can still use the inverse CDF algorithm by numerically
solving for the quantiles of the CDF. However, because this method is computationally expensive,
you should use this technique only when direct methods are not available.
If F is a distribution function, then you can simulate values from F by doing the following:

1. Generate a random uniform value u in [0, 1].
2. Use bisection or some other root-finding algorithm to find the value x such that F(x)=u.
3. Repeat these steps until you have generated N values of x.*/

proc iml;
/* a quantile is a zero of the following function */
start Func(x) global(target);
   cdf = (x + x##3 + x##5)/3;
   return( cdf-target );
finish;

/* test bisection module */
target = 0.5;                /* global variable used by Func module */
/* for SAS/IML 9.3 and before, use q = Bisection(0,1); */ 
q = froot("Func", {0 1});    /* SAS/IML 12.1                        */
p = (q + q##3 + q##5)/3;     /* check whether F(q) = target         */
print q p[label="CDF(q)"];  
N = 100;
call randseed(12345);
u = j(N,1); x = j(N,1);
call randgen(u, "Uniform");             /* u ~ U(0,1)        */
do i = 1 to N;
   target = u[i];
   /* for SAS/IML 9.3 and before, use x[i] = Bisection(0,1); */ 
   x[i] = froot("Func", {0 1});         /* SAS/IML 12.1      */
end;
create Poly var {"x"}; append; close Poly;
quit;

ods graphics on;

proc univariate data=Poly;
   histogram x / endpoints=0 to 1 by 0.05;
   cdfplot x;
   ods select Histogram CDFPlot;
run;


/********************************************************************
 
 		Finite Mixture Distributions
 		
 	(A mixed distribution bridging a number of Distributions 
 	 via a "table" distribution)
 
*********************************************************************/


%let N = 100;                                 /* size of sample     */
data Calls(drop=i);
call streaminit(12345);
array prob [3] _temporary_ (0.5 0.3 0.2);
do i = 1 to &N;
   type = rand("Table", of prob[*]);          /* returns 1, 2, or 3 */
   if type=1 then      x = rand("Normal",  3, 1);   
   else if type=2 then x = rand("Normal",  8, 2);
   else                x = rand("Normal", 10, 3);              
   output;
end;
run;

proc univariate data=Calls;
   ods select Histogram;
   histogram x / vscale=proportion
   kernel(lower=0 c=SJPI);      		/*Using a ifferent Kernal Option*/		        
run;

proc univariate data=Calls;
   ods select Histogram;
   histogram x / vscale=proportion
   kernel(lower=0 c=MISE);              /*Using a ifferent Kernal Option*/	
run;

/*Exercise 7.6: Assume that every call requires at least one minute to handle. Modify the DATA
step in this section so that if x is less than 1, then it is replaced by the value 1. (This results in a
mixture of a point mass and the truncated mixture distribution.) Simulate 1,000 samples from the
distribution. Use PROC MEANS to compute the total time that is required to answer 100 calls. Use
PROC UNIVARIATE to examine the distribution of the total times. Based on the simulation, predict
how often the call center will need 11 or more hours to service 100 call.*/

%let N = 1000;                                 /* size of sample     */
data Calls_t(drop=i);
call streaminit(12345);
array prob [3] _temporary_ (0.5 0.3 0.2);
do i = 1 to &N;
   type = rand("Table", of prob[*]);          /* returns 1, 2, or 3 */
   if type=1 then      x = rand("Normal",  3, 1);   
   else if type=2 then x = rand("Normal",  8, 2);
   else                x = rand("Normal", 10, 3);  
   if x < 1 then x = 1;
   output;
end;
run;

proc means data = Calls_t; 	
var x;
run;			/*Total time to answer 100 calls is 607.75 mins (10.1291 hours)*/

proc univariate data=Calls_t;
   ods select Histogram;
   histogram x / vscale=proportion
   kernel(lower=0 c=SJPI);
run;

data Calls_t;
set Calls_t;
InD = (x>=6.66);  /*11hrs for 100 calls ~ 6.66 mins for 1 call*/
run;

proc sort data=Calls_t; 
   by x; 
run; 

proc freq data=Calls_t;  
   tables InD / nocum;
run;

/* 43.20% of the time */



/***********************************************************************

	7.5.2 The Contaminated Normal Distribution 
	
	(A mixed distribution bridging 2 Normal Distributions with 
	same maens but very different variances via a Bernoulli)

***********************************************************************/


data j;
call streaminit(seed=0);
do i=1 to 100;
x=rand("Bernoulli", 0.1);
output;
end;
run;


%let std = 10;                        /* magnitude of contamination */
%let N = 100;                         /* size of sample             */
data CN(keep=x);
call streaminit(12345);
do i = 1 to &N;
   if rand("Bernoulli", 0.1) then  	  /*0.1 chance goes to N(0,10)*/
      x = rand("Normal", 0, &std);
   else 
      x = rand("Normal");
   output;
end;
run;

proc univariate data=CN;
   var x;
   histogram x / kernel vscale=proportion endpoints=-15 to 21 by 1;
   qqplot x;
run;

/*Exercise 7.7: It is possible to create a continuous mixture distribution by replacing a parameter
with a random variable (Devroye 1986, p.16). Simulate data from the distribution N(mu, 1), where
mu ~ U(0,1). Draw a histogram of the result. The mean of the simulated data should be close to
E(mu)=0.5.*/


/* Generate a continuous mixture distribution:
   X ~ N(mu, 1) where mu~U(0,1) */

data ContMix(keep=x);
call streaminit(12345);
do i = 1 to 10000;
   mu = rand("Uniform", 0 , 10);               /* mu ~ U(0,10)  */
   x = rand("Normal", mu);             /* x ~ N(mu, 1) */
   output;
end;
run;

proc univariate data=ContMix;
   var x; histogram x / normal;
run;


/**********************/




/********************************************************************
 
 	The Acceptance-Rejection Technique
 
The rejection method is a technique for simulating values from a distribution (called the instrumental
distribution) that is subject to constraints. The idea is that you generate values from the instrumental
distribution and then throw away (reject) any values that do not meet the constraints. It is also
known as the acceptance-rejection technique. This section explores an instructive application of the
rejection method: simulating data from a truncated normal distribution (Robert 1995).
 
*********************************************************************/

%let N = 100;                          /* size of sample */
data TruncNormal(keep=x);
	call streaminit(12345);
	a = 0;
	do i = 1 to &N;
  	 *do until( x>=a );        /* reject x < a (If encounter a neg, re-run the loop then output) */
		*	x = rand("Normal");
  	 *end;
    do until( (x>0 & x<2) ); /* x in (0, 2) */
		x = rand("Normal");
  	 end;
   output;
end;
run;

proc univariate data = TruncNormal;
	histogram x / midpoints= (0 to 1 by 0.05);
run;

data Inver_trunc(keep=x3);
	call streaminit(12345);
	Phi_a = cdf("Normal", 0); /* a = 0 */
	Phi_b = 1; /* b = infinity */
	do i = 1 to &N;
		u = rand("Uniform");
		x3 = quantile("Normal", Phi_a + u*(Phi_b - Phi_a));
		output;
	end;
run;

proc univariate data = Inver_trunc;
	histogram x3 / midpoints= (0 to 1 by 0.05);
run;

/*This technique is appropriate when the truncation points are near the middle of the normal
distribution. For truncation points that are in the tails of the normal distribution, see the accept-reject
algorithm in Robert (1995). See also Devroye (1986, p. 380).*/

proc iml;
%let N = 100;
call randseed(12345);
multiple = 2.5;                   /* choose value > 2               */
y = j(multiple * &N, 1);          /* allocate more than you need    */
call randgen(y, "Normal");        /* y ~ N(0,1)                     */
idx = loc(y > 0);                 /* acceptance step                */
x = y[idx];
x = x[1:&N];                      /* discard any extra observations */
p = 0.5;                  /* prob of accepting instrumental variate */
F = quantile("NegBin", 0.999, p, &N); 
M = F + &N;               /* Num Trials = failures + successes      */
print M;
quit;

/*The event “accept the instrumental variate” is a Bernoulli random variable
with probability of success p. Negative binomial is the distribution of the 
number of failures before k successes in a sequence of independent Bernoulli 
trials*/


/*Exercise 7.10: Generate 10,000 samples of size 248 from the normal distribution and discard any
negative observations to obtain a truncated normal sample. The size of the truncated normal sample
varies. Plot a histogram of the sample sizes. How many samples have fewer than 100 observations?*/

proc iml;
call randseed(12345);
N = 248;
NumSamples = 10000;
y = j(NumSamples, N);             /* allocate more than you need    */
call randgen(y, "Normal");        /* y ~ N(0,1)                     */
idx = loc(y < 0);                 /* rejection step                 */
y[idx] = .;                       /* replace neg vals with missing  */
c = countn(y, "row");             /* count nonmissing in each row   */
m = mean(c>= 100);                /* proportion for which more than */
print m;                          /*     100 obs were accepted      */
create countsample var {"c"}; append; close countsample;
quit;

proc univariate data = countsample nextrval=10;
histogram c/ normal; 
run;

/* 4 samples have fewer than 100 observations. BTW, CLT at work */

/**********************/




/********************************************************************

 
 	Simulating Survival Data

  
*******************************************************************/

/* sigma is scale parameter; use sigma=1/lambda for a rate parameter */

%macro RandExp(sigma);
   ((&sigma) * rand("Exponential"))
%mend;

data LifeData;						   /*Generatingdata for survival times.*/
call streaminit(1);
do PatientID = 1 to 100;
   t = %RandExp(1/0.01);               /* hazard rate = 0.01 */
   output;
end;
run;

proc lifetest data=LifeData;
   time t;
   ods select Quartiles Means;
run;

/*Censored observations*/
   
/*If a subject completes the study without experiencing the event, the event time for that subject 
is said to be censored. Similarly, patients drop out of the study prior to experiencing the event 
are said to be censored*/

data CensoredData(keep= PatientID t Censored);
call streaminit(1);
HazardRate = 0.01;         /* rate at which subject experiences event */
CensorRate = 0.001;        /* rate at which subject drops out         */
EndTime = 365;             /* end of study period                     */
do PatientID = 1 to 100;
   tEvent = %RandExp(1/HazardRate);
   c = %RandExp(1/CensorRate);
   t = min(tEvent, c, EndTime);
   Censored = (c < tEvent | tEvent > EndTime); 
   output;
end;
run;

proc lifetest data=CensoredData plots=(survival(atrisk CL));
   time t*Censored(1);
   *ods select Quartiles Means CensoredSummary SurvivalPlot;
run;


/*Exercise 7.8: Simulate survival times that are lognormally distributed. For example, use
exp(rand("Normal", 4, 1)) as the time to event. First, assume no censoring and an arbitrarily
long study. Next, assume a 365-day study and a constant rate of dropping out.*/


data CensoredData_log;
call streaminit(1);
*HazardRate = 0.01;         /* rate at which subject experiences event */
CensorRate = 0.001;        /* rate at which subject drops out         */
EndTime = 365;             /* end of study period                     */
do PatientID = 1 to 100;
   tEvent = (exp(rand("Normal", 4, 1)));
   c = %RandExp(1/CensorRate);
   t = min(tEvent, c, EndTime);
   Censored = (c < tEvent | tEvent > EndTime); 
   output;
end;
run;

proc lifetest data=CensoredData_log plots=(survival(atrisk CL));
   *time tEvent;
   time t*Censored(1);
   *ods select Quartiles Means CensoredSummary SurvivalPlot;
run;





/********************************************************************


			/***************************

				Topics to be continue

			***************************/


/********************************************************************

 Simulating from Less Common Univariate Distributions

 *******************************************************************/

/* Simulate from inverse Gaussian (Devroye, p. 149) */

data InvGauss(keep= X);
mu = 1.5;                             /* mu > 0     */
lambda = 2;                           /* lambda > 0 */
c = mu/(2 * lambda);
call streaminit(1);
do i = 1 to 1000;
   muY = mu * rand("Normal")**2;      /* or mu*rand("ChiSquare", 1) */
   X = mu + c*muY - c*sqrt(4*lambda*muY + muY**2);
   /* return X with probability mu/(mu+X); otherwise mu**2/X */
   if rand("Uniform") > mu/(mu+X) then /* or rand("Bern", X/(mu+X)) */
      X = mu*mu/X;
   output;
end;
run;

proc univariate data=InvGauss;
   var X;
   histogram X ;
run;

/***********************************************************************/

/* Simulate from Pareto (Devroye, p. 29) */
data Pareto(keep= X);
a = 4;                    /* alpha > 0                              */
k = 1.5;                  /* scale > 0 determines lower limit for x */
call streaminit(1);
do i = 1 to 1000;
   U = rand("Uniform");
   X = k / U**(1/a);
   output;
end;
run;

/***********************************************************************/

/* Johnson SB(threshold=theta, scale=sigma, shape=delta, shape=gamma) */
data SB(keep= X);
call streaminit(1);
theta = -0.6;   scale = 18;   delta = 1.7;    gamma = 2;
do i = 1 to 1000;
   Y = (rand("Normal")-gamma) / delta;
   expY = exp(Y);
   /* if theta=0 and sigma=1, then X = logistic(Y) */
   X = ( sigma*expY + theta*(expY + 1) ) / (expY + 1);
   output;
end;
run;

/***********************************************************************/

/* Johnson SU(threshold=theta, scale=sigma, shape=delta, shape=gamma) */
data SU(keep= X);
call streaminit(1);
theta = 1;  sigma = 5;   delta = 1.5;  gamma = -1;
do i = 1 to 10000;
   Y = (rand("Normal")-gamma) / delta;
   X = theta + sigma * sinh(Y);
   output;
end;
run;

proc univariate data=SU;
   histogram x / su noplot;
   ods select ParameterEstimates;
run;

/**********************/
/* Answer to exercise */
/**********************/

/* Johnson SU(threshold=theta, scale=sigma, shape=delta, shape=gamma) */
/*
data SUSamples(keep= SampleID X);
call streaminit(1);
theta = 1;  sigma = 5;   delta = 1.5;  gamma = -1;
do SampleID = 1 to 1000;
   do i = 1 to 100;
      Y = (rand("Normal")-gamma) / delta;
      X = theta + sigma * sinh(Y);
      output;
   end;
end;
run;

%ODSOff
proc univariate data=SUSamples;
   by SampleID;
   histogram X / noplot SU(theta=est sigma=est);
   ods output ParameterEstimates=PE(where=(Symbol^=" "));
run;
%ODSOn

proc univariate data=PE;
   class Symbol;
   histogram Estimate;
   ods select Histogram;
run;

proc means data=PE mean std p5 p95 min max;
   class Symbol;
   var Estimate;
run;
*/
/**********************/

