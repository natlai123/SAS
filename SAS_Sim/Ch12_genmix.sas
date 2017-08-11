/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 12: Simulating Data for Advanced Regression Models
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
 Generalized Linear Regression Model
 *******************************************************************/

%let N = 150;
data LogisticData;
array xx1{&N} _temporary_;
array xx2{&N} _temporary_ ;
call streaminit(1);

/* read or simulate fixed effects */
do i = 1 to &N;
   xx1{i} = rand("Uniform");  xx2{i} = rand("Normal", 0, 2);
end;

/* simulate logistic model */
do i = 1 to &N;
   x1 = xx1{i};  x2 = xx2{i};
   /* linear model with parameters  {2, -4, 1} */
   eta = 2 - 4*x1 + 1*x2;             /* eta = X*beta. NO epsilon!  */
   mu = exp(eta) / (1+exp(eta));      /* transform by inverse logit */
   y = rand("Bernoulli", mu);         /* binary response            */
   output;
end;
run;

ods graphics on;
proc logistic data=LogisticData plots(only)=Effect;
   model y(Event='1') = x1 x2 / clparm=wald;
   ods select ParameterEstimates CLParmWald EffectPlot;
run;

/**********************/
/* Answer to exercise */
/**********************/
data LogisticSim;
array xx1{&N} _temporary_;
array xx2{&N} _temporary_ ;
call streaminit(1);
N = 100;
drop i N mu;
do i = 1 to N;
   xx1{i} = rand("Uniform");  xx2{i} = rand("Normal", 0, 2);
end;

do SampleID = 1 to 10000;
do i = 1 to N;
   x1 = xx1{i};  x2 = xx2{i};
   eta = 2 - 4*x1 + 1*x2;            
   mu = exp(eta) / (1+exp(eta));     
   y = rand("Bernoulli", mu);        
   output;
end;
end;
run;

%ODSOff;
options nonotes;
proc logistic data=LogisticSim;
   by SampleID;
   model y(Event='1') = x1 x2;
   ods output ParameterEstimates=PE;
run;
options notes;
%ODSOn;

proc univariate data=PE;
   class Variable;
   var Estimate;
   histogram Estimate;
   ods select Quantiles Histogram;
run;
/**********************/


/***********************************************************************/

%let N = 100;
data PoissonData;
array xx1{&N} _temporary_;
array xx2{&N} _temporary_ ;
call streaminit(1);

/* read or simulate fixed effects */
do i = 1 to &N;
   xx1{i} = rand("Uniform");  xx2{i} = rand("Normal", 0, 2);
end;

/* simulate regression model */
do i = 1 to &N;
   x1 = xx1{i};  x2 = xx2{i};
   eta = 2 - 4*x1 + 1*x2;             /* eta = X*beta               */
   mu = exp(eta);                     /* transform linear predictor */
   y = rand("Poisson", mu);           /* Poisson response           */
   output;
end;
run;

proc genmod data=PoissonData;
   model y = x1 x2 / dist=Poisson;
   ods select ParameterEstimates;
run;

/********************************************************************
 Linear Mixed Models
 *******************************************************************/

/* simple random effect model with repeated measurements */
%let var_A  = 4;    /* variance of random effect (intercept)        */
%let sigma2 = 2;    /* variance of residual, e ~ N(0, sqrt(sigma2)) */
%let L = 3;         /* num levels in random effect A                */
%let k = 5;         /* num repeated measurements in each level of A */
data RandomEffects(drop=mu rndA);
call streaminit(12345);
mu = 5;
do a = 1 to &L;
   rndA = rand("Normal", 0, sqrt(&var_A));
   do rep = 1 to &k;
      y = mu + rndA + rand("Normal", 0, sqrt(&sigma2));
      output;
   end; 
end;
run; 

proc mixed data=RandomEffects CL;
   class a;
   model y = ;                      /* no fixed effects             */
   random int / subject=a;          /* a is random intercept effect */
   ods select CovParms;
run;

/**********************/
/* Answer to exercise */
/**********************/
/*
data RandomEffects;
call streaminit(0);
mu = 5;
do SampleID=1 to 10000;
do a = 1 to &L;
   r_a = rand("Normal", 0, sqrt(&var_a));
   do rep = 1 to &k;
      e = rand("Normal", 0, sqrt(&sigma2));
      y = mu + r_a + e;
      output;
   end; 
end;
end;
run; 

%ODSOff;
options nonotes;
proc mixed data=RandomEffects CL;
   by SampleID;
   class a;
   model y = ;
   random int / subject=a;
   ods output CovParms=CP;
run;
options notes;
%ODSOn;

proc univariate data=CP(where=(CovParm="Intercept"));
   title "Intercept";
   var Estimate;
   histogram Estimate;
run;

proc univariate data=CP(where=(CovParm="Residual"));
   title "Residual";
   var Estimate;
   histogram Estimate;
run;
*/
/**********************/


/***********************************************************************/


/***********************************************************************/

proc iml;
k=3;                            /* 3 measurements on growth curve   */
s=5;                            /* 5 individuals                    */
sigma2_R = 1.4;                 /* Parameter 1: residual covariance */
sigma2_CS = 2;                  /* Parameter 2: common covariance   */
B = sigma2_R*j(k,k,1) + sigma2_CS*I(k);     /* cs matrix            */
R = B;                          /* first block                      */
do i = 2 to s;                  /* create block-diagonal matrix     */
   R = block(R, b);             /*    with s blocks                 */ 
end;
R = I(s) @ B;                              /* block-diagonal matrix */ 

/***********************************************************************/

beta = {0, -2};                     /* parameters for fixed effects */
Week = T(repeat(1:k, 1,s));         /* column: 1,2,3,1,2,3,...      */
X = j(nrow(Week),1,1) || Week;      /* add intercept                */
Indiv = colvec(repeat(T(1:s),1,k)); /* 1,1,1,2,2,2,3,3,3,...        */

call randseed(1234);
create Mix var {"WtLoss" "Week" "Indiv"};     /* name the variables */

/* random effects */
zero = j(1, k*s, 0);            /* the zero vector                  */
eps = RandNormal(1, zero, R);   /* eps ~ MVN(0,R)                   */
WtLoss = X*beta + eps`;         /* fixed effects, correlated errors */

append; 
close;
quit;

proc sgplot data=Mix;
   label WtLoss = "Weight Loss";
   series x=Week y=WtLoss / group=Indiv markers;
   refline 0 / axis=y;
   xaxis integer;
run;

proc mixed data=Mix CL;
   class Indiv;
   model WtLoss = Week;
   repeated / type=cs subject=Indiv R;
   ods select Dimensions R CovParms;
run;

/**********************/
/* Answer to exercise */
/**********************/
/* rerun simulation 1,000 times and look at distribution of 
   covariance parameter estimates */
proc iml;
k=3;                            /* 3 measurements on growth curve   */
s=5;                            /* 5 individuals                    */
sigma2_R = 1.4;                 /* Parameter 1: residual covariance */
sigma2_CS = 2;                  /* Parameter 2: common covariance   */
B = sigma2_R*j(k,k,1) + sigma2_CS*I(k);                /* cs matrix */
R = I(s) @ B;
beta = {0, -2};                         /* params for fixed effects */
Week = T(repeat(1:k, 1,s));             /* column: 1,2,3,1,2,3,...  */
X = j(nrow(Week),1,1) || Week;          /* add intercept            */
Indiv = colvec(repeat(T(1:s),1,k));     /* 1,1,1,2,2,2,3,3,3,...    */
zero = j(1, k*s, 0);                    /* the zero vector          */
call randseed(1234);
create Mix var {"SampleID" "WtLoss" "Week" "Indiv"};

/* random effects */
do i = 1 to 1000;
   eps = RandNormal(1, zero, R);
   WtLoss = X*beta + eps`;
   SampleID = repeat(i, k*s);
   append; 
end;
close;

%ODSOff; options nonotes;
proc mixed data=Mix;
   by SampleID;
   class Indiv;
   model WtLoss = Week;
   repeated / type=cs subject=Indiv;
   ods output CovParms=CovParms;
run;
%ODSOn; options notes;

proc means data=CovParms Mean Median StdDev CLM Max;
   class CovParm;
   var   Estimate;
run;

ods graphics on;
proc univariate data=CovParms;
   class CovParm;
   histogram Estimate;
   ods select Histogram;
run;
/**********************/


/***********************************************************************/

/* Getting Started example in PROC MIXED documentation */
data heights;
  input Family Gender$ Height @@;
  datalines;
1 F 67   1 F 66   1 F 64   1 M 71   1 M 72   
2 F 63   2 F 63   2 F 67   2 M 69   2 M 68   2 M 70   
3 F 63   3 M 64   
4 F 67   4 F 66   4 M 67   4 M 67   4 M 69
;
run;

/* Model data. Save parameter estimates */
proc mixed data=heights;
   class Family Gender;
   model Height = Gender / solution outpm=outpm;
   random Family Family*Gender;
   ods select CovParms SolutionF;
   ods output CovParms=CovParms SolutionF=SolutionF;
run;

/***********************************************************************/

proc glimmix data=heights outdesign(names novar)=All;
   class Family Gender;
   model Height = Gender;
   random Family Family*Gender;
   ods select ColumnNames;
run;

/**********************/
/* Answer to exercise */
/**********************/
/* The following statements use the OUTDESIGN= option in the PROC GLMMOD 
   statement to write the design matrices to data sets. You do not need 
   the response variable, so you can use the DROP= data set option to 
   exclude the Height variable. */

/* create dummy vars (including intercept) for fixed effects */
proc glmmod data=Heights outdesign=Fixed(drop=Height);
   class Gender;
   model Height = Gender;
run;

/* create dummy vars (drop intercept) for random effects */
proc glmmod data=Heights outdesign=Random(drop=Height);
   class Gender Family;
   model Height = Family Family*Gender /noint;
run;
/**********************/


/***********************************************************************/

proc iml;
FixedVar = "_X1":"_X3";
RandomVar = "_Z1":"_Z12";
use All;
read all var FixedVar into X;
read all var RandomVar into Z;
close All;
/* read parameter estimates for fxed effects into beta */
use SolutionF; read all var {Estimate} into beta; close;
eta = X*beta;                          /* linear predictor */

/***********************************************************************/

/* read estimates for covariance parameters */
use CovParms; read all var {Estimate} into var; close;
varF   = var[1];                       /* Var(Family)         */
varFG  = var[2];                       /* Var(Family*Gender)  */
sigma2 = var[3];                       /* sigma2 = Var(Error) */
/* Define covariance matrix G for random effects */
G = diag( repeat(varF,4) // repeat(varFG,8) );

/***********************************************************************/

call randseed(1);
zero = repeat(0, nrow(G));            /* the zero vector             */
NumSamples = 5;
/* first attempt (less efficient) */
eps = J(nrow(X),1);                   /* allocate error term         */
Y = j(nrow(X), NumSamples);           /* allocate cols for responses */
do j = 1 to NumSamples;               /* simulate mixed model        */
   gamma = RandNormal(1, zero, G);               /* gamma ~ MVN(0,G) */
   call randgen(eps, "Normal", 0, sqrt(sigma2)); /* eps ~ N(0,sigma))*/
   Y[,j] = eta + Z*gamma` + eps;
end;

/* second attempt (more efficient) */
gamma = RandNormal(NumSamples, zero, G);  /* each column is MVN(0,G) */
eps = J(nrow(X), NumSamples);                 /* allocate error term */
call randgen(eps, "Normal", 0, sqrt(sigma2)); /* eps[i,j]~N(0,sigma) */
Y = eta + Z*gamma` + eps;
yNames = "y1":("y"+strip(char(NumSamples)));            /* "y1":"y5" */
create Sim from Y[c=yNames];  append from Y;  close;
quit;

/* add subject identifier */
data Sim;  
merge OutPM Sim;   /* merge simulated responses and predicted means */
N=_N_;   
run;

proc sgplot data=Sim nocycleattrs;
   /* trick: Use scatter plot to show markers by gender */
   series x=N y=y1; scatter x=N y=y1 / group=Gender;
   series x=N y=y2; scatter x=N y=y2 / group=Gender;
   series x=N y=y3; scatter x=N y=y3 / group=Gender;
   series x=N y=y4; scatter x=N y=y4 / group=Gender;
   series x=N y=y5; scatter x=N y=y5 / group=Gender;

   /* show means for females and males */
   refline 64.85 68.21 / axis=y lineattrs=(pattern=dash); 
   refline 5.5 11.5 13.5 / axis=x;              /* separate families */
   xaxis values=(1 to 18) label="Subjects";
   yaxis label="Height (in)";
run;

/**********************/
/* Answer to exercise */
/**********************/
proc iml;
/* read design matrices for fixed and random effects */
use Fixed; read all var _NUM_ into X; close;
use Random; read all var _NUM_ into Z; close;
/* read parameter estimates for fxed effects into beta */
use SolutionF; read all var {Estimate} into beta; close;
eta = X*beta;                                /* linear predictor    */

/* read estimates for covariance parameters */
use CovParms; read all var {Estimate} into var; close;
varF   = var[1];                             /* Var(Family)         */
varFG  = var[2];                             /* Var(Family*Gender)  */
sigma2 = var[3];                             /* sigma2 = Var(Error) */

/* Define covariance matrix G for random effects */
G = diag( repeat(varF,4) // repeat(varFG,8) );
zero = repeat(0, nrow(G));                   /* the zero vector     */
call randseed(1);

NumSamples = 100;

gamma = RandNormal(NumSamples, zero, G);     /* gamma ~ MVN(0,G)    */
eps = J(nrow(X),NumSamples);                 /* allocate error term */
call randgen(eps, "Normal", 0, sqrt(sigma2));  /* eps ~ N(0,sigma)) */
Y = eta + Z*gamma` + eps;

Subject = repeat(T(1:nrow(X)), 1, NumSamples);
create Box var {Subject Y};   append;   close Box;
quit;

proc sgplot data=Box;
   vbox Y / category=Subject;
   /* show means for females and males */
   refline 64.85 68.21 / axis=y lineattrs=(pattern=dash); 
   refline 5.5 11.5 13.5 / axis=x;           /* separate families   */
   yaxis label="Height (in)";
   xaxis type=linear;
run;
/**********************/


/********************************************************************
 Survival Analysis Models
 *******************************************************************/

%macro RandExp(sigma);
   ((&sigma) * rand("Exponential"))
%mend;

%let N = 100;
data PHData(keep=x1 x2 t censored);
array xx1{&N} _temporary_;
array xx2{&N} _temporary_ ;
call streaminit(1);

/* read or simulate fixed effects */
do i = 1 to &N;
   xx1{i} = rand("Normal");  xx2{i} = rand("Normal");
end;

/* simulate regression model */
baseHazardRate = 0.002;  censorRate = 0.001;
do i = 1 to &N;
   x1 = xx1{i};  x2 = xx2{i};
   eta = -2*x1 + 1*x2;             /* form the linear predictor     */

   /* construct time of event and time of censoring */
   tEvent = %RandExp( 1/(baseHazardRate * exp(eta)) );
   c  = %RandExp( 1/censorRate );  /* rate parameter = censorRate   */
   t  = min(tEvent, c);            /* time of event or censor time  */
   censored = (c < tEvent);        /* indicator variable: censored? */
   output;
end;
run;

ods graphics on;
proc phreg data=PHData plots(overlay CL)=(Survival);
   model t * censored(1) = x1-x2;  
   ods select CensoredSummary ParameterEstimates 
              ReferenceSet SurvivalPlot;
run;  

/***********************************************************************/

%let N = 100;
data survsamp(keep=Treatment t Censored);
call streaminit(1);
array rate{2} (0.05 0.08);
do Treatment = 1 to dim(rate);
   do i = 1 to &N;
      censored = rand("Bernoulli", 0.2);
      t = %RandExp( 1/rate{Treatment} );
      output;
   end;
end;
run;

ods graphics on;
proc lifetest data=survsamp plots=(survival);
   strata Treatment;
   time t * censored(1);
   ods select Quartiles HomTests SurvivalPlot;
run;

/********************************************************************
 Nonparametric Models
 *******************************************************************/

data NonParam;
call streaminit(1);
do x = 1 to 30 by 0.1;
   f = sin(x/5) + 0.2*cos(x);
   y = f + rand("Normal", 0, 0.2);
   output;
end;

ods graphics on;
proc loess data=NonParam;
   model y = x;
   score /;
   ods output ScoreResults = Score;
   ods select FitPlot;
run;

proc iml;
use Score; read all var {f p_y}; close;
RMSE = sqrt( ssq(f-p_y)/nrow(f) );          /* SSQ = sum of squares */ 
print RMSE;

proc sgplot data=Score;
   series x=x y=f / legendlabel="True Model";
   series x=x y=p_y / legendlabel="Loess(0.129)";
run;
