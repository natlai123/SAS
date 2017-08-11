/***********************************************************************
 Programs for Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 3: Preliminary and Background Information
 
  Essential Functions for Working with Statistical Distributions
  Random Number Streams in SAS
  Checking the Correctness of Simulated Data
  Overlaying a Theoretical Density on a Histogram
  QQ Plot
  Using ODS Statements to Control Output
  
 ***********************************************************************/
/*

PDF function: This function is the probability density function. 
	It returns the probability density at a given point for a 
	variety of distributions. (For discrete distribution, the 
	PDF function evaluates the probability mass function.)
	
CDF function: This function is the cumulative distribution function.
	The CDF returns the probability that an observation from the specified 
	distribution is less than or equal to a particular value. For continuous 
	distributions, this is the area under the PDF up to a certain point.
	
QUANTILE function: This function is closely related to the CDF function, 
	but solves an inverse problem. Given a probability, P, it returns the 
	smallest value, q, for which CDF(q) is greater than or equal to P.

RAND function: This function generates a random sample from a distribution. 
	In SAS/IML software, use the RANDGEN subroutine, which fills up an entire 
	matrix at once.
*/

/********************************************************************
	
	Essential Functions for Working with Statistical Distributions

*********************************************************************/

/*Plotting the Standard Normal PDF Curve*/

data pdf;
do x = -3 to 3 by 0.1;
   y = pdf("Normal", x);
   output;
end;
   x0 = 0; pdf0 = pdf("Normal", x0); output;  /*Get the (x,y) = (0, 0.3989)*/
   x0 = 1; pdf0 = pdf("Normal", x0); output;  /*Get the (x,y) = (1, 0.2419)*/
run;

proc sgplot data=pdf noautolegend;
   series x=x y=y;
   scatter x=x0 y=pdf0;
   vector x=x0 y=pdf0 /xorigin=x0 yorigin=0 noarrowheads lineattrs=(color=gray);
   xaxis grid label="x"; yaxis grid label="Normal PDF";
   refline 0 / axis=y;
run;

/*Trial*/
data cdf;
y = cdf("Normal", 0 );
output;
y = cdf("Normal", 1 );
output;
y = cdf("Normal", 2 );
output;
y = cdf("Normal", 3 );
output;
y = cdf("Normal", 4 );
output;
run;


/*Ploting the Standard Expoential PDF*/

data exp_pdf;
do x = 0 to 5 by 0.1;
   y = pdf("Exponential", x, 1);
   output;
end;
   x0 = 0; pdf0 = pdf("Exponential", x0); output;  
   x0 = 1; pdf0 = pdf("Exponential", x0); output;  
run;

proc sgplot data = exp_pdf;
	series x=x y=y;
	scatter x=x0 y=pdf0;
	vector x=x0 y=pdf0/ xorigin=x0 yorigin=0 noarrowheads lineattrs=(color=red);
	xaxis grid label = "x"; yaxis grid label = "Exponential PDF";
	refline 0 / axis=y;
run;


/***********************************************************************/

/*Plotting the Standard Normal CDF Curve*/

data cdf;
do x = -3 to 3 by 0.1;
   y = cdf("Normal", x);
   output;
end;
   x0 = 0;     cdf0 = cdf("Normal", x0); output;
   x0 = 1.645; cdf0 = cdf("Normal", x0); output;
run;

ods graphics / height=500;
proc sgplot data=cdf noautolegend;
   series x=x y=y;
   scatter x=x0 y=cdf0;
   vector x=x0 y=cdf0 /xorigin=x0 yorigin=0 noarrowheads lineattrs=(color=gray);
   vector x=x0 y=cdf0 /xorigin=-3 yorigin=cdf0 noarrowheads lineattrs=(color=black);
   xaxis grid label="x";
   yaxis grid label="Normal CDF" values=(0 to 1 by 0.05);
   refline 0 1/ axis=y;
run;
ods graphics / reset;


/*Ploting the Standard Expoential CDF*/

data cdf;
do x = 0 to 5 by 0.1;
   y = cdf("Exponential", x, 1);
   output;
end;
   x0 = 1;     cdf0 = cdf("Exponential", x0, 1); output;
   x0 = 3; cdf0 = cdf("Exponential", x0, 1); output;
run;

ods graphics / height=500;
proc sgplot data=cdf noautolegend;
   series x=x y=y;
   scatter x=x0 y=cdf0;
   vector x=x0 y=cdf0 /xorigin=x0 yorigin=0 noarrowheads lineattrs=(color=gray);
   vector x=x0 y=cdf0 /xorigin=0 yorigin=cdf0 noarrowheads lineattrs=(color=black);
   xaxis grid label="x";
   yaxis grid label="Exponential CDF" values=(0 to 1 by 0.05);
   refline 0 1/ axis=y;
run;
ods graphics / reset;


/***********************************************************************/

/*		Quantile Function (Inverse CDF Function)  */

/***********************************************************************/

data invcdf;
do y = 0 to 1 by 0.001;
   x = quantile("Normal", y);
   output;
end;
run;

proc sgplot data=invcdf noautolegend;
   series x=x y=y;
   xaxis grid label="x";
   yaxis grid label="Normal Inverse CDF" values=(0 to 1 by 0.05);
   refline 0 1/ axis=y;
run;


/********************************************************************

 	
 	Random Number Streams in SAS


*********************************************************************/


data a;
call streaminit(4321); 
do i = 1 to 10;  x=rand("uniform"); output;  end;
run;

data b;
call streaminit(4321); 
do i = 1 to 10;  x=rand("uniform"); output;  end;
run;

proc compare base=a compare=b; run;      /* show they are identical */

/***********************************************************************/

data a;
call streaminit(0);                   /* different stream each time */
do i = 1 to 10;  x=rand("uniform"); output;  end;
run;

data b;
call streaminit(&sysrandom);      /* use SYSRANDOM to set same seed */
%put &sysrandom;				  /*Track the see number*/
do i = 1 to 10;  x=rand("uniform"); output;  end;
run;

proc compare base=a compare=b short;     /* show they are identical */
run;


/********************************************************************


	 Checking the Correctness of Simulated Data


********************************************************************/


%let N=500;     					   /*Change it to 10000, 5000*/
data Gamma(keep=x);
call streaminit(4321);
do i = 1 to &N;
   x = rand("Gamma", 4);               /* shape=4, unit scale */
   output;
end;
run;

/* fit Gamma distrib to data; compute GOF tests */
proc univariate data=Gamma;
   var x;
   histogram x / gamma(alpha=EST scale=1); 
   qqplot x /  gamma(alpha=EST scale=1 THETA=EST SIGMA=EST); 
   *ods select Moments ParameterEstimates GoodnessOfFit;
run;


/*Exerise 3.7*/

proc sort data = Gamma out = QQG; by x; run;

data QQG;
set QQG nobs = Nobs;
v = (_N_ - 0.375)/(Nobs +0.2);
q = quantile("Gamma", v, 4);
label x = "Simulated Data" q = "Gamma Quantiles";
run;

proc sgplot data = QQG;
	scatter x=q  y=x;
	 lineparm x=0 y=0 slope=1;
	xaxis grid; yaxis grid;
run;	


/***********************************************************************/

%let N=100;
data Geometric(keep=x);
call streaminit(4321);
do i = 1 to &N;
   x = rand("Geometric", 0.5);      /* number of tosses until heads */
   output;
end;
run;

proc means data = Geometric;    	/*means should be close to 2*/
run;

/* For the geometric distribution, PDF("Geometric",t,0.5) computes the 
   probability of t FAILURES, t=0,1,2,...  Use PDF("Geometric",t-1,0.5) 
   to compute the number of TOSSES until heads appears, t=1,2,3,.... 
   
   Given that PDF('GEOMETRIC',m,p) where m is a numeric random variable 
   that denotes the number of failures before the first success and  p
   is a numeric probability of success. 
*/
  
data PMF(keep=T Y);
do T = 1 to 9;
   Y = pdf("Geometric", T-1, 0.5);
   output;
end;
run;

data Discrete;                     /*For plotting*/ 
   merge Geometric PMF;
run;



/* GTL syntax changed at 9.4 */
%macro ScaleOpt;
   %if %sysevalf(&SysVer < 9.4) %then pct;  %else proportion;
%mend;

proc template;
define statgraph BarPMF;
dynamic _Title;                        /* specify title at run time */
begingraph;
   entrytitle _Title;
   layout overlay / yaxisopts=(griddisplay=on)
                    xaxisopts=(type=discrete);
   barchart    x=X / name='bar' legendlabel='Sample' stat=%ScaleOpt;
   scatterplot x=T y=Y / name='pmf' legendlabel='PMF';
   discretelegend 'bar' 'pmf';
   endlayout;
endgraph;
end;
run;

proc sgrender data=Discrete template=BarPMF;
   dynamic _Title = "Sample from Geometric(0.5) Distribution (N=&N)";
run;


/**********************/
/* Answer to exercise */
/**********************/
%let N = 100;
data NegBin(keep=x);
call streaminit(0);
do i = 1 to &N;
   x = rand("NegBin", 0.5, 3);
   output;
end;
run;

data PMF(keep=T Y);
do T = 0 to 15;
   Y = pdf("NegBin", T, 0.5, 3);
   output;
end;
run;

data Discrete;
merge NegBin PMF;
run;

proc sgrender data=Discrete template=BarPMF;
   dynamic _Title = "Sample from NegBin(0.5,3) Distribution (N=&N)";
run;
/**********************/


/***********************************************************************/

/*Overlaying a Theoretical Density on a Histogram: Gamma Distribution*/

data PDF(keep=T Y);
do T = 0 to 13 by 0.1;
   Y = pdf("Gamma", T, 4);
   output;
end;
run;

data Cont;
   merge Gamma PDF;
run;

proc template;
define statgraph HistPDF;
dynamic _Title _binstart _binstop _binwidth;
begingraph;
   entrytitle _Title;
   layout overlay / xaxisopts=(linearopts=(viewmax=_binstop));
   histogram X / scale=density endlabels=true xvalues=leftpoints 
         binstart=_binstart binwidth=_binwidth;
   seriesplot x=T y=Y / name='PDF' legendlabel="PDF" 
         lineattrs=(thickness=2);
   discretelegend 'PDF';
   endlayout;
endgraph;
end;
run;

proc sgrender data=Cont template=HistPDF;
dynamic _Title="Sample from Gamma(4) Distribution (N=&N)"
   _binstart=0                        /* left endpoint of first bin */
   _binstop=13                        /* right endpoint of last bin */
   _binwidth=1;                       /* width of bins              */
run;


/*Overlaying a Theoretical Density on a Histogram: Chi(3) Distribution*/

%let N=500;     					   
data Chi(keep=x);
call streaminit(4321);
do i = 1 to &N;
   x = rand("Chisquare", 3);               /* PDF('CHISQUARE',x,df <,nc> )*/
   output;
end;
run;

data Chi_PDF(keep=T Y);
do T = 0 to 15 by 0.1;
   Y = pdf("Chisquare", T, 3);
   output;
end;
run;

data Cont_chi;
   merge Chi Chi_PDF;
run;

proc sgrender data=Cont_chi template=HistPDF;
dynamic _Title="Sample from ChiSquare(3) Distribution (N=&N)"
   _binstart=0                        /* left endpoint of first bin */
   _binstop=15                        /* right endpoint of last bin */
   _binwidth=1;                       /* width of bins              */
run;



/***********************************************************************/

/*The Quantile-Quantile Plot*/


/*Proc Univariate*/

data Exponential(keep=x);
call streaminit(4321);
sigma = 10;
do i = 1 to &N;
   x = sigma * rand("Exponential");
   output;
end;
run;

/* create an exponential Q-Q plot */
proc univariate data=Exponential;
   var x;
   qqplot x / exp (THETA=EST SIGMA=EST);   	 /*exp creates an exponential quantile plot.*/
run;


/*SGPLOT*/

%let N = 100;
data Normal(keep=x);
call streaminit(4321);
do i = 1 to &N;
   x = rand("Normal");                         /* N(0, 1) */
   output;
end;
run;

/* Manually create a Q-Q plot */
proc sort data=Normal out=QQ; by x; run;             /* 1 */

data QQ;
set QQ nobs=NObs;
v = (_N_ - 0.375) / (NObs + 0.25);                   /* 2 */
q = quantile("Normal", v);                           /* 3 */
label x = "Observed Data" q = "Normal Quantiles";
run;

proc sgplot data=QQ;                                 /* 4 */
   scatter x=q y=x;
   lineparm x=0 y=0 slope=1;
   xaxis grid;  yaxis grid;
run;


proc iml;
y = {1.7, 1.0, 0.5, 3.5, 1.9, 0.7, 0.4, 
     5.1, 0.2, 5.6, 4.6, 2.8, 3.8, 1.4, 
     1.6, 0.9, 0.3, 0.4, 1.9, 0.5};
 
n = nrow(y);
call sort(y, 1); /* 1 */
v = ((1:n) - 0.375) / (n + 0.25);  /* 2 (Blom, 1958) */
q = quantile("Exponential", v, 2); /* 3: Standardized 2 is the scale parameter */
Run Scatter(y, q);
q = quantile("Exponential", v);    /* 3 Non-standardized*/
Run Scatter(y, q); 
quit;



/*Simulating data from N(1,3) and creating a qq-plot*/

data sim_normal (keep = x);
call streaminit(4321);
do i = 1 to 500;
   x = rand("Normal", 1 , 3);                         /* N(1, 3) */
   output;
end;
run;

proc sort data=sim_normal out=QQ_norm; by x; run;           /* 1 */

data QQ_norm;
set QQ_norm nobs=NObs;
v = (_N_ - 0.375) / (NObs + 0.25);                			/* 2 */
q = quantile("Normal", v);                          	    /* 3: q = quantile("Normal", v, 3); */
label x = "Observed Data" q = "Normal(1,3) Quantiles";
run;

proc sgplot data=QQ_norm;                                   /* 4 */
   scatter x=q y=x;
   lineparm x=0 y=1 slope=3;
   xaxis grid;  yaxis grid;
run;


/* The scale parameter shifts the intercept by 3*/
data QQ_norm;
set QQ_norm nobs=NObs;
v = (_N_ - 0.375) / (NObs + 0.25);                		
q = quantile("Normal", v);     
q3 = quantile("Normal", v, 3);                    	    
run;




/********************************************************************
 Using ODS Statements to Control Output
 *******************************************************************/

ods trace on;
ods graphics off;
proc freq data=Sashelp.Class;
   tables sex / chisq;
run;
ods trace off;

ods select OneWayChiSq;;
ods exclude OneWayFreqs;
proc freq data=Sashelp.Class;
   tables sex;
   ods output OneWayFreqs=Freqs;
run;

proc contents data=Freqs short order=varnum; 
run;

ods graphics on;
proc freq data=Sashelp.Class;
   tables age / plot=FreqPlot;
   ods select FreqPlot;
run;
