/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 13: Simulating Data from Time Series Models
 ***********************************************************************/

%macro ODSOff();               /* Call prior to BY-group processing */
ods graphics off;
ods exclude all;
ods noresults;
%mend;

%macro ODSOn();                /* Call after BY-group processing    */
ods graphics on;
ods exclude none;
ods results;
%mend;

/********************************************************************
 Simulating Data from Autoregressive and Moving Average (ARMA) Models
 *******************************************************************/

%let N = 100;
data AR1(keep=t y);
call streaminit(12345);
phi = 0.4;   yLag = 0; 
do t = -10 to &N;
   eps = rand("Normal");                /* variance of 1            */
   y = phi*yLag + eps;                  /* expected value of Y is 0 */
   if t>0 then output;
   yLag = y;
end;
run; 

ods graphics on;
proc arima data=AR1 plots(unpack only)=(series(corr));
   identify var=y nlag=1;                       /* estimate AR1 lag */
   estimate P=1;
   ods select SeriesPlot ParameterEstimates FitStatistics; 
quit;

/***********************************************************************/

%let N = 100;
%let NumSamples = 1000;
/* simulate 1,000 time series from model */
data AR1Sim(keep=SampleID t y);
phi = 0.4;
call streaminit(12345);
do SampleID = 1 to &NumSamples;
   yLag = 0; 
   do t = -10 to &N;
      y = phi*yLag + rand("Normal"); 
      if t>0 then output;
      yLag = y;
   end;
end;
run; 

/* estimate AR(1) model for each simulated time series */
proc arima data=AR1Sim plots=none;
   by SampleID;
   identify var=y nlag=1 noprint;               /* estimate AR1 lag */
   estimate P=1 outest=AR1Est(where=(_TYPE_="EST")) noprint;
quit;

/* analyze sampling distribution */
ods graphics on;
proc univariate data=AR1Est;
   var AR1_1;
   histogram AR1_1 / normal;
   ods select histogram;
run;

proc means data=Ar1Est Mean Std P5 P95; 
   var AR1_1;
run;

/***********************************************************************/

/* simulate data from ARMA(p,q) model */
%let N = 5000;
%let p = 3;                                    /* order of AR terms */
%let q = 1;                                    /* order of MA terms */
data ARMASim(keep= t y);
call streaminit(12345);
array phi phi1 - phi&p (-0.6, 0, -0.3);        /* AR coefficients   */
array theta theta1 - theta&q ( 0.1);           /* MA coefficients   */
array yLag   yLag1 - yLag&p;           /* save p lagged values of y */
array errLag errLag1 - errLag&q;       /* save q lagged error terms */
/* set initial values to zero */
do j = 1 to dim(yLag);   yLag[j] = 0;   end;
do j = 1 to dim(errLag); errLag[j] = 0; end;

/* "steady state" method: discard first 100 terms */
do t = -100 to &N;
   /* y_t is a function of values and errors at previous times      */
   e = rand("Normal");              
   y = e;
   do j = 1 to dim(phi);                                /* AR terms */
      y = y + phi[j] * yLag[j];
   end;
   do j = 1 to dim(theta);                              /* MA terms */
      y = y - theta[j] * errLag[j];
   end;
   if t > 0 then output;

   /* update arrays of lagged values */
   do j = dim(yLag) to 2 by -1; yLag[j] = yLag[j-1]; end;
   yLag[1] = y;
   do j = dim(errLag) to 2 by -1; errLag[j] = errLag[j-1]; end;
   errLag[1] = e;
end;
run;

proc sgplot data=ARMASim(obs=204);
   series x=t y=y;
   refline 0 / axis=y;
   xaxis values=(0 to 204 by 12);
run;

/**********************/
/* Answer to exercise */
/**********************/
/* estimate ARMA(3,1) model for simulated time series */
proc arima data=ARMASim plots(unpack)=(series(corr));
   identify var=y nlag=3 ;                       /* estimate AR lag */
   estimate P=3 Q=1 outest=AR31Est(where=(_TYPE_="EST")) noprint;
quit;

proc print data=AR31Est; run;
/**********************/


/***********************************************************************/

/* use the ARMASIM function to simulate a time series */
%let N = 100;
proc iml;
phi   = {1 -0.4};   /* AR coefficients: Notice the negative sign!   */
theta = {1};        /* MA coefficients: Use {1, 0.1} to add MA term */
mu = 0;             /* mean of process                              */
sigma = 1;          /* std dev of process                           */
seed = -54321;      /* use negative seed if you call ARMASIM twice  */
yt = armasim(phi, theta, mu, sigma, &N, seed); 
y1 = armasim(phi, theta, mu, sigma, 5,  12345);  /* 5 obs           */
y2 = armasim(phi, theta, mu, sigma, 5,  12345);  /* same 5 obs      */
y3 = armasim(phi, theta, mu, sigma, 5, -12345);  /* 5 obs           */
y4 = armasim(phi, theta, mu, sigma, 5, -12345);  /* different 5 obs */
print y1 y2 y3 y4;
NumSamples = 1000;
Y = j(NumSamples, &N);
do i = 1 to NumSamples;
   z = armasim(phi, theta, mu, sigma, &N, -12345);  /* use seed < 0 */
   Y[i,] = z`;                  /* put i_th time series in i_th row */
end;
SampleID = repeat(T(1:NumSamples),1,&N);
create AR1Sim var {SampleID Y}; append; close AR1Sim;
quit;

/**********************/
/* Answer to exercise */
/**********************/
%ODSOff
proc arima data=AR1Sim;
   by SampleID;
   identify var=y nlag=1 noprint;               /* estimate AR1 lag */
   estimate P=1 outmodel=Model(keep=_PARM_ _VALUE_);
run; quit;
%ODSOn

data AR1Estimates;
   set Model(where=(_PARM_="MU" | _PARM_="AR")); 
   By = ceil(_N_/2); rename _PARM_=_NAME_;
run;

proc transpose data= AR1Estimates 
   out=AR1Est(drop=By _NAME_ _LABEL_);
   by By;
   var _VALUE_;
run;

proc sgplot data=AR1Est;
   scatter x=Mu y=AR;
   ellipse x=Mu y=AR;
   refline 0 /axis=x;
   refline 0.4 / axis=y;
run;
/**********************/


/********************************************************************
 Simulating Data from Multivariate ARMA Models
 *******************************************************************/

proc iml;
Phi = {0.70 0.00 0.00,                      /* AR coefficients      */
       0.30 0.60 0.00,
       0.10 0.20 0.50};
Theta = j(3,3,0);                           /* MA coefficients = 0  */
mu = {0,0,0};                               /* mean = 0             */
sigma = {1.0  0.4 0.16,                     /* covariance of errors */
         0.4  1.0 0.4,
         0.16 0.4 1.0};
call varmasim(y, Phi, Theta, mu, sigma, &N) seed=54321; 

create MVAR1 from y[colname={"y1" "y2" "y3"}];
append from y;
close MVAR1;
quit;

proc statespace data=MVAR1 interval=day armax=1 lagmax=1;
   var y1-y3;
   ods select FittedModel.TransitionMatrix FittedModel.VarInnov;
run;
