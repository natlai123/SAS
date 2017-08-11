/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 11: Simulating Data for Basic Regression Models
 
 Technique 1: Put explanatory variables into arrays 
 Technique 2: Put simulation loop inside loop over observations
 A Linear Model Based on Real Data
 Simulating Data from Regression Models in SAS/IML Software
 Original data not available. Simulate from summary statistics. 

*************************************************************************/

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
 	

 	Simple Linear Regression Models


*********************************************************************/

%let N = 50;                            /* size of each sample      */
data Reg1(keep=x y);
call streaminit(1);
do i = 1 to &N;
   x = rand("Uniform");                 /* explanatory variable     */
   eps = rand("Normal", 0, 0.5);        /* error term               */
   y = 1 - 2*x + eps;                   /* parameters are 1 and -2  */
   output;
end;
run;

proc reg data=Reg1;
   model y = x;
   ods exclude NObs;
quit;

/***********************************************************************/

/* Simulate multiple samples from a regression model */

/* Technique 1: Put explanatory variables into arrays (time: 0.0114212036sec)*/
%let N = 50;                           /* size of each sample      */
%let NumSamples = 1000;                 /* number of samples        */
data RegSim1(keep= SampleID x y time);
t0 = time();
array xx{&N} _temporary_;              /* do not output the array  */
call streaminit(1);
do i = 1 to &N;                        /* create x values one time */
   xx{i} = rand("Uniform");
end;

do SampleID = 1 to &NumSamples;
   do i = 1 to &N;
      x = xx{i};                /* use same values for each sample */
      y = 1 - 2*x + rand("Normal", 0, 0.5); /* params are 1 and -2 */	
      output;
   end;
end;
   time = time() - t0;
   output;
run;

/***********************************************************************/

/* Technique 2: Put simulation loop inside loop over observations (time: 0.0102710724sec)*/
data RegSim2(keep= SampleID i x y time);
t0=time();
call streaminit(1);
do i = 1 to &N;
   x = rand("Uniform");          /* use this value NumSamples times */
   eta = 1 - 2*x;                /* parameters are 1 and -2         */
   do SampleID = 1 to &NumSamples;
      y = eta + rand("Normal", 0, 0.5); 
      output;
   end;
end;
time = time() - t0;
output;
run;

proc sort data=RegSim2;
   by SampleID i;
run;

proc reg data=RegSim2 outest=OutEst NOPRINT;
   by SampleID;
   model y = x;
quit;

ods graphics on;
proc corr data=OutEst noprob plots=scatter(alpha=.05 .1 .25 .50);
   label x="Estimated Coefficient of x"
         Intercept="Estimated Intercept";
   var Intercept x;
   ods exclude VarInformation;
run;

proc univariate data=OutEst cibasic;
	var _RMSE_;
	histogram _RMSE_ / normal;
run;


/***********************************************************************/

	/* A Linear Model Based on Real Data */

/***********************************************************************/

ods graphics off;
proc reg data=Sashelp.Class;
   model Weight = Height;
   ods select FitStatistics ParameterEstimates;
quit;

/***********************************************************************/

data StudentData(keep= Height Weight);
call streaminit(1);
set Sashelp.Class;           /* implicit loop over observations     */
b0 = -143; b1 = 3.9;         /* parameter estimates from regression */
rmse = 11.23;                /* estimate of scale of error term     */
Weight = b0 + b1*Height + rand("Normal", 0, rmse);
run;

/**********************/
/* Answer to exercise */
/**********************/

proc reg data=Sashelp.Class;
   model Weight = Height;
   output out=ClassPred p=Pred;    /* linear predictor for each obs */
quit;

data StudentData2(keep= Height Weight);
call streaminit(1);
set ClassPred;               /* implicit loop; read Pred            */
rmse = 11.23;                /* estimate of scale of error term     */
Weight = Pred + rand("Normal", 0, rmse);
run;

/**********************/

proc compare base=StudentData compare=StudentData2;run;

/***********************************************************************/

/* duplicate data by using sequential access followed by a sort     */

%let NumSamples = 100;              /* number of samples            */
data StudentSim(drop= b0 b1 rmse);
b0 = -143; b1 = 3.9; rmse = 11.23;  /* parameter estimates          */
call streaminit(1);
set Sashelp.Class;                  /* implicit loop over obs       */
i = _N_;
eta = b0 + b1*Height;               /* linear predictor for student */
do SampleID = 1 to &NumSamples;
   Weight = eta + rand("Normal", 0, rmse);
   output;
end;
run;

proc sort data=StudentSim;
   by SampleID i;
run;

/* Alternative: simulate weights by using random access and no sort */
data StudentSim2(drop= b0 b1 rmse);
b0 = -143; b1 = 3.9; rmse = 11.23;  /* parameter estimates          */
call streaminit(1);
do SampleID = 1 to &NumSamples;
   do i = 1 to NObs;                /* NObs defined at compile time */
      set Sashelp.Class point=i nobs=NObs;         /* random access */
      eta = b0 + b1*Height;         /* linear predictor for student */
      Weight = eta + rand("Normal", 0, rmse);
      output;
   end;
end;
STOP;     /* IMPORTANT: Use STOP with POINT= option in the SET stmt */
run;

/***********************************************************************/

proc iml;
call randseed(1);
beta = {-143, 3.9};  rmse = 11.23;
use Sashelp.Class NOBS N;                /* N = sample size         */
read all var {Height} into X1;           /* read data               */
close Sashelp.Class;

X = j(N,1,1) || X1;                      /* add intercept column    */
print X;
eta = X*beta;                            /* linear predictor (Dependent Variable) */
print eta;
eps = j(N, 1);                           /* allocate N x 1 vector   */
do i = 1 to &NumSamples;
   call randgen(eps, "Normal", 0, rmse); /* fill with random normal */
   Weight = eta + eps;                   /* one simulated response  */
   /* conduct further analysis */
end;
print Weight;
/*The code below generate long format dataset*/
eps = j(N * &NumSamples, 1);             /* allocate long vector  &NumSamples=100 */
*print eps;
call randgen(eps, "Normal", 0, rmse);    /* fill with random normal */
Wt = repeat(eta, &NumSamples) + eps;     /* simulate all responses  */
ID = repeat( T(1:&NumSamples), 1, N );   /* 1,1,1,...,2,2,2,...     */
Ht = repeat( X1, &NumSamples );
create SimReg var {ID Ht Wt};  append;  close SimReg; 


proc reg data=SimReg outest=Est NOPRINT;
   by ID;
   model Wt = Ht;
quit;

proc sgplot data=Est;
   scatter x=Intercept y=Ht;
run;


/**********************/


/***********************************************************************/

/* Original data not available. Simulate from summary statistics.   */

data StudentModel(keep= Height Weight SampleID);
call streaminit(1);
b0 = -143; b1 = 3.9;         /* parameter estimates from regression */
rmse = 11.23;                /* estimate of scale of error term     */
do SampleID = 1 to 1000;
do i = 1 to 19;
   Height = rand("Normal", 62.3, 5.13);  /* Height is random normal */
   Weight = b0 + b1*Height + rand("Normal", 0, rmse);
   output;
end;
end;
run;

proc reg data=StudentModel outest=Est NOPRINT;
   by SampleID;
   model Weight = Height;
quit;

proc sgplot data=Est;
   scatter x=Intercept y=Height;
run;

ods graphics on;
proc corr data=Est noprob plots=scatter(alpha=.05 .1 .25 .50);
   label Height="Estimated Coefficient of Height"
         Intercept="Estimated Intercept";
   var Intercept Height;
   ods exclude VarInformation;
run;


/***********************************************************************/

/*Linear Model with a Single Classification Variable*/

data AnovaData(keep=Treatment Y);
call streaminit(1);
grandmean = 20;
array effect{6} _temporary_ (9 -6 -6 4 0 0);
array std{6}    _temporary_ (6  2  4 4 1 2);
do i = 1 to dim(effect);       /* number of treatment levels        */
   Treatment = i;
   do j = 1 to 5;              /* number of obs per treatment level */
      Y = grandmean + effect{i} + rand("Normal", 0, std{i});
      output;
   end;
end;
run;

proc ANOVA data=AnovaData;
   class Treatment;
   model Y = Treatment;
   ods exclude ClassLevels NObs;
run;


proc iml;
call randseed(1);
grandmean = 20;
effect = {9 -6 -6 4 0 0};
std    = {6  2  4 4 1 2};
N = 5; 

/* each row of Y is a treatment */
Y = j(ncol(effect),N,.);  /* allocate matrix; fill with missing values */
ei = j(1,N); 

do i = 1 to ncol(effect);
   call randgen(ei, "Normal", 0, std[i]);
   Y[i,] = grandmean + effect[i] + ei;
end;
print Y;

/**********************/
/* Answer to exercise */
/**********************/

data Unbalanced(keep=Treatment Y);
call streaminit(1);
grandmean = 20;
array size{6}   _temporary_ (5 10 10 12 6 8);
array effect{6} _temporary_ (9 -6 -6 4 0 0);
array std{6}    _temporary_ (6  2  4 4 1 2);
do i = 1 to dim(effect);       /* number of treatment levels        */
   Treatment = i;
   do j = 1 to size{i};        /* number of obs per treatment level */
      Y = grandmean + effect{i} + rand("Normal", 0, std{i});
      output;
   end;
end;
run;


proc glm data=Unbalanced;
   class Treatment;
   model Y = Treatment;
quit;


proc iml;
call randseed(1);
grandmean = 20;
size   = {5 10 10 12 6 8};
effect = {9 -6 -6 4 0 0};
std    = {6  2  4 4 1 2};
N = max(size); 

/* each row of Y is a treatment */
Y = j(ncol(effect),N,.);  /* allocate matrix; fill with missing values */
ei = j(1,N); 

do i = 1 to ncol(effect);
   call randgen(ei, "Normal", 0, std[i]);
   Y[i, 1:size[i]] = grandmean + effect[i] + ei[ ,1:size[i]];
end;

print Y;
quit;


/**********************

	Econ 850 Q4*/

	For n = 500; 1000; 2000; 3000; 4000; 6000, and 8000
	  t1(sec)= 0.007194 0.0149448 0.030756 0.0441599 0.073509 0.1315119 0.238663
	
	For n = 500; 1000; 2000; 3000; 4000; 6000, and 8000
	  t2(sec)= 0.000123 0.0002861 0.000391 0.0003691 0.0003011 0.0022242


***********************************************************************/


%let nCont = 20;              /* number of contin vars               */
%let nClas = 10;              /* number of class vars                */
%let nLevels = 3;            /* number of levels for each class var */
%let N = 8000;
/* Simulate GLM data with continuous and class variables */
data GLMData(drop=i j);   
array x{&nCont} x1-x&nCont;  
array c{&nClas} c1-c&nClas;  
call streaminit(1);

/* simulate the model */
do i = 1 to &N; 
   do j = 1 to &nCont;              /* continuous vars for i_th obs */
      x{j} = rand("Uniform");       /* uncorrelated uniform         */
   end; 
   do j = 1 to &nClas;              /* class vars for i_th obs      */
      c{j} = ceil(&nLevels*rand("Uniform"));    /* discrete uniform */
   end; 
   /* specify regression coefficients and magnitude of error term */
   y = 2*x{1} - 3*x{&nCont} + c{1} + rand("Normal"); 
   output;  
end;  
run;

proc iml;
use GLMData;
read all var _NUM_ into Q;
close GLMData;
y = Q[,31];
X = Q[,1:30];
t0 = time();
beta=(GINV(X))*y;
time1 = time() - t0;
t1 = time();
Pred=X*beta;
time2 = time() - t1;
print time1, time2;
quit;

proc glm data=GLMData;  
   class c1-c&nClas;  
   model y = x1-x&nCont c1-c&nClas / SS3;  
   ods select ModelANOVA;
quit;


/********************************************************************
 	
 	The Power of a Regression Test: F-test 
 	
*********************************************************************/

data Reg1(drop=i);
call streaminit(1);
do i = 1 to &N;
   x = rand("Normal");
   z = rand("Normal");
   y = 1 + 1*x + 0*z + rand("Normal");       /* eps ~ N(0,1) */
   output;
end;
run;

proc reg data=Reg1;
   model y = x z;
   z0: test z=0;
   ods select TestANOVA;
quit;

/*********************************************************************/

/*Greene (2000, Ch. 15)*/

%let N = 50;
%let NumSamples = 1000;                     /* number of samples    */  
data RegSim(drop= i eta sx);
call streaminit(1);
do i = 1 to &N;
   x = rand("Normal");  z = rand("Normal");
   sx = exp(x/5);                           /* StdDev for model 3   */
  *gamma=0;			/*For Exercise 11.13*/
   do gamma = 0 to 1 by 0.1;
      eta = 1 + 1*x + gamma*z;              /* linear predictor     */
      do SampleID = 1 to &NumSamples;
         /* Model 1: e ~ N(0,1)         */
         /* Model 2: e ~ t(5)           */
         /* Model 3: e ~ N(0, exp(x/5)) */
         Type = 1;  y = eta + rand("Normal");         output;
         Type = 2;  y = eta + rand("T", 5);           output;
         Type = 3;  y = eta + rand("Normal", 0, sx);  output;
      end; 
   end;                                     /* end gamma loop       */
end;                                        /* end observation loop */
run;

proc sort data=RegSim out=Sim;
   by Type gamma SampleID;
run;

/* Turn off output when calling PROC for simulation */
%ODSOff
proc reg data=Sim;
   by Type gamma SampleID;
   model y = x z;
   test z=0;
   ods output TestANOVA=TestAnova;
quit;
%ODSOn

/* 3. Construct an indicator variable for observations that reject H0 */
data Results;
   set TestANOVA(where=(Source="Numerator"));
   Reject = (ProbF <= 0.05);                  /* indicator variable */
run;

/* count number of times H0 was rejected */
proc freq data=Results noprint;
   by Type gamma;
   tables Reject / nocum out=Signif(where=(reject=1));
run;

data Signif;
   set Signif;
   proportion = percent / 100;     /* convert percent to proportion */
run;

proc format;
   value ErrType 1="e ~ N(0,1)"  2="e ~ t(5)"  3="e ~ Hetero";
run;

title "Power of F Test for gamma=0";
title2 "N = &N";
proc sgplot data=Signif;
   format Type ErrType.;
   series x=gamma y=proportion / group=Type;
   yaxis min=0 max=1 label="Power (1 - P[Type II Error])" grid;
   xaxis label="Gamma" grid;
   keylegend / across=1 location=inside position=topleft;
run;

/**********************
Set gamma 0 in the regression model. When the error term follows an N(0,1) distribution, the 
F statistic in the TestANOVA table follows an F2,N-2 distribution. Examine the sampling 
distribution for the other choices of the error distribution. Does the F statistic appear to be
sensitive to the shape of the error distribution? No reallys
**********************/

/* generate the data and run the following */

ods graphics on;
proc univariate data=TestANOVA(where=(Source="Numerator"));
   class Type;
   var FValue;
   histogram FValue;
run;

/*The larger F Statistics, the more likely the null is being rejected*/

/**********************/


/********************************************************************
 
 	Linear Models with Interaction and Polynomial Effects
 
*********************************************************************/

data Interactions;
y = 0;                            /* the value of y does not matter */
do drug = 1 to 3;
   do disease = 1 to 2;
      do subject = 1 to 5;
         output;
      end;
   end;
end;
run;

proc glmmod data=Interactions noprint
            outparm=Parm outdesign=Design(drop=y);        /* DROP y */
   class drug disease;
   model y = drug | disease;
run;

proc iml;
call randseed(1);
use Design;
read all var _NUM_ into X;
close Design;

/* Intcpt |--Drug--|Disease|-----Interactions-----| */
beta = {0, 0, 0, 0,  0, 0,  10, 0, 15, -5, 20, -10};
eps = j(nrow(X),1);                        /* allocate error vector */
call randgen(eps, "Normal");
y = X*beta + eps;

create Y var{y};  append;  close Y;        /* write to data set     */

data D;
   merge Y Interactions(drop=y);
run;

ods graphics on;
proc glm data=D;
   class drug disease;
   model y = drug | disease / solution p;
run;

/***********************************************************************/

proc logistic data=Interactions 
              outdesignonly outdesign=DesignRef(drop=y);
   class drug disease / param=reference;
   model y = drug | disease;
run;

/********************************************************************
 
 	Outliers and Robust Regression Models

*********************************************************************/

%let N = 100;                           /* size of each sample      */
data RegOutliers(keep=x y Contaminated);
array xx{&N} _temporary_;  
p = 0.1;                                /* prob of contamination    */
call streaminit(1);
/* simulate fixed effects */
do i = 1 to &N;
   xx{i} = rand("Uniform");
end;
/* simulate regression model */
do i = 1 to &N;
   x = xx{i};
   Contaminated = rand("Bernoulli",p);
   if Contaminated then eps = rand("Normal", 0, 10);
   else                 eps = rand("Normal", 0, 1);
   y = 1 - 2*x + eps;                   /* parameters are 1 and -2  */
   output;
end;
run;

proc format;
   value Contam 0="N(0, 1)"  1="N(0, 10)";
run;

proc sgplot data=RegOutliers(rename=(Contaminated=Distribution));
   format Distribution Contam.;
   scatter x=x y=y / group=Distribution;
   lineparm x=0 y=1 slope=-2;                   /* requires SAS 9.3 */
run;

proc robustreg data=RegOutliers method=lts FWLS;
   model y = x;
   ods select LTSEstimates ParameterEstimatesF;
run;

/***********************************************************************/

/* simulate outliers and high-leverage points for regression data   */
%let N = 100;
proc iml;
call randseed(1);
mu =   {0 0 0};                      /* means                       */
Cov = {10 3 -2, 3 6 1, -2 1 2};      /* covariance for X            */
kX = 25;                             /* contamination factor for  X */
pX = 0.15;                           /* prob of contamination for X */
kY = 10;                             /* contamination factor for  Y */
pY = 0.25;                           /* prob of contamination for Y */

/* simulate contaminated normal (mixture) distribution */
call randgen(N1, "Binomial", 1-pX, &N); /* N1=num of uncontaminated */
X = j(&N, ncol(mu));
X[1:N1,] = RandNormal(N1, mu, Cov);        /* draw N1 from uncontam */
X[N1+1:&N,] = RandNormal(&N-N1, mu, kX*Cov); /* N-N1 from contam    */

/* simulate error term according to contaminated normal */
outlier = j(&N, 1);
call randgen(outlier, "Bernoulli", pY);    /* choose outliers       */
eps = j(&N, 1);
call randgen(eps, "Normal", 0, 1);         /* uncontaminated N(0,1) */
outlierIdx = loc(outlier);
if ncol(outlierIdx)>0 then                 /* if outliers...        */
   eps[outlierIdx] = kY * eps[outlierIdx]; /* set eps ~ N(0,kY)     */

/* generate Y according to regression model */
beta = {2, 1, -1};               /* params, not including intercept */
Y = 1 + X*beta + eps;
/* write SAS data set */
varNames = ('x1':'x3') || {"Y" "Outlier"};
output = X || Y || outlier;
create Robust from output[c=varNames];  append from output;  close;
quit;

proc robustreg data=Robust method=LTS plots=RDPlot;
   model Y = x1-x3 / leverage;
   output out=Out outlier=RROutlier leverage=RRLeverage;
   ods select LTSEstimates DiagSummary RDPlot;
run;

/**********************/
/* Answer to exercise */
/**********************/
proc freq data=Out;
   tables Outlier*RROutlier / nopercent norow nocol;
run;
/**********************/

