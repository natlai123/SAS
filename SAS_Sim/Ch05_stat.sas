/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 5: Using Simulation to Evaluate Statistical Techniques
 
  Confidence Interval for a Mean
  Computing Coverage in the SAS/IML Language
  Assessing the t test in SAS/IML Software
  A Simulated Power Analysis 
  Effect of Sample Size on the Power of the t Test
  Using Simulation to Compute p-Values
  
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
 Confidence Interval for a Mean
 *******************************************************************/
/*
Coverage probability

Simulate many samples of size n from the population.
Compute the confidence interval for each sample.
Compute the proportion of samples for which the (known) population parameter 
is contained in the confidence interval. That proportion is an estimate for 
the empirical coverage probability for the CI*/


/*Normal Distribution*/

%let N = 50;                                /* size of each sample  */
%let NumSamples = 10000;                    /* number of samples    */  
/* 1. Simulate obs from N(0,1) */
data Normal(keep=SampleID x);				/*500000x2 table*/
call streaminit(123);
do SampleID = 1 to &NumSamples;             /* simulation loop      */
   do i = 1 to &N;                          /* N obs in each sample */
      x = rand("Normal");                   /* x ~ N(0,1)           */
      output;
   end;
end;
run;  

/* 2. Compute the confidence interval statistics for each sample */
proc means data=Normal noprint;				/*10000 rows*/
   by SampleID;
   var x;
   output out=OutStats mean=SampleMean lclm=Lower uclm=Upper;
run;  

proc format;                  /* display 0/1 as "No"/"Yes" */
   value YorN 0="Interval not containing 0" 1="Interval Containing 0";
run;

/* how many CIs include parameter? */
data OutStats;  set OutStats;
   ParamInCI = (Lower<0 & Upper>0);           /* indicator variable */
run;

ods graphics / width=6.5in height=4in;
proc sgplot data=OutStats(obs=100);
format ParamInCI YorN.;
   title "95% Confidence Intervals for the Mean";
   scatter x=SampleID y=SampleMean / group=ParamInCI markerattrs=(symbol=CircleFilled);
   highlow x=SampleID low=Lower high=Upper / group=ParamInCI legendlabel="95% CI";
   refline 0 / axis=y;
   yaxis display=(nolabel);
run;

/* Nominal coverage probability is 95%. Estimate true coverage. */
proc freq data=OutStats;
   tables ParamInCI / nocum binomial(level='1' p=0.95);
run;

/* The output from the BINOMIAL option estimates that the true coverage is in the 
interval [0.9422,0.951], which includes 0.95. Thus the simulation supports the 
assertion that the standard CI of the mean has 95% coverage when a sample is drawn 
from a normal population.*/

/*
If you specify the BINOMIAL option for a one-way table, PROC FREQ displays the estimate of the
binomial Proportion, which is the proportion of observations in the first class listed in the 
one-way table. PROC FREQ also displays the asymptotic standard error (ASE) and the asymptotic 
(Wald) and exact (Clopper-Pearson) confidence limits by default. For the binomial proportion 
test, PROC FREQ displays the asymptotic standard error under the null hypothesis (ASE Under H0),
the standardized test statistic (Z), and the one-sided and two-sided probability values.*/


/********************************************************************

/*Exercise 5.1: Let P be the proportion of confidence intervals that contain zero. Rerun the program
10 times using 0 as the seed value, and record the range of values of P that you observe. Reduce the
number of samples to 1,000 and run the new program 10 times. Compare the range of P values.
Explain what you observe.*/

/**********************/
/* Answer to exercise */
/**********************/


%macro DoExercise(N, NumSamples, Ntrial, seed);
   title1 "Exercise with N=&N and NumSamples=&NumSamples";
   title2 "Information of CI not containing the mean, 0.";
   /* 1. Simulate obs from N(0,1) */
   data Normal(keep=trial SampleID x);			/* 500000x3 table */
   call streaminit(&seed);
   do trial=1 to &Ntrial;						/* loop for trial 		*/
   	do SampleID = 1 to &NumSamples;             /* simulation loop      */
      do i = 1 to &N;                           /* N obs in each sample */
    x = rand("Normal");                   		/* x ~ N(0,1)           */
    output;
      end;
   	end;
   end;
   run;  

   /* 2. Compute statistics for each sample */
   proc means data=Normal noprint;				/**/
      by trial SampleID;
      var x;
      output out=OutStats mean=SampleMean lclm=Lower uclm=Upper;
   run;  

   /* How many CIs include parameter? */
   data OutStats;  set OutStats;
      ParamInCI = (Lower<0 & Upper>0);       /* indicator variable */
   run;
   /* Nominal coverage probability is 95%. Estimate true coverage. */
   proc freq data=OutStats noprint;
      by trial;
      tables ParamInCI / nocum out=DTrials; 
   run;

   proc means data=DTrials(where=(ParamInCI=0)) mean std min max range maxdec=3;
      var percent;
   run;
   title;
%mend;

%DoExercise(50, 10000, 10, 0);             /* std and range of P are small */
%DoExercise(50, 1000, 10, 0);              /* more variation in P          */

/*Since the 'population' distribution is normal. The confidence interval contains 
the population mean with a probability of close to 0.95. This is only true when the 
population is normally distributed (which is never true in practice) or the sample 
sizes are large enough that you can invoke the Central Limit Theorem. Simulation 
enables you to estimate the coverage probability for small samples when the population
is not normal. The higher the percentage (closer to 5%), the closer to the true 
expected coverage probability, 95%. If the data are not normally distributed, then 
the interval contains the population mean  with a probability that is different 
from 0.95. */


/**********************/

/* Exponential data */

%let N = 50;                                /* size of each sample  */
%let NumSamples = 10000;                    /* number of samples    */

data Exp(keep=SampleID x);
call streaminit(321);
do SampleID = 1 to &NumSamples;             /* simulation loop      */
   do i = 1 to &N;                          /* N obs in each sample */
      x = rand("Expo") - 1;                 /* x ~ Exp(1) - 1       */
      output;
   end;
end;
run;

/* 2. Compute confidence interval for each sample */
proc means data=Exp noprint;
  by SampleID;
  var x;
  output out=OutStats mean=SampleMean lclm=Lower uclm=upper;
run;

/* 3. Analyze sampling distribution of statistic */
/* how many CIs don't include parameter? Create indicator variable */
data OutStats;
  set OutStats;
  ParamInCI = (Lower<0 & Upper>0);
run;

ods graphics / width=6.5in height=4in;
proc sgplot data=OutStats(obs=100);
format ParamInCI YorN.;
   title "95% Confidence Intervals for the Mean";
   scatter x=SampleID y=SampleMean / group=ParamInCI markerattrs=(symbol=CircleFilled);
   highlow x=SampleID low=Lower high=Upper / group=ParamInCI legendlabel="95% CI";
   refline 0 / axis=y;
   yaxis display=(nolabel);
run;

/*Exercise 5.3: Use the BINOMIAL option on the TABLES statement to show that a 95%  
confidence interval about the estimate of 0.9344 does not include 0.95.*/  
/* Nominal coverage probability is 95%. Estimate true coverage. */

proc freq data=OutStats;
  tables ParamInCI / nocum binomial(level='1' p=0.95);  
run;

/*Since 0.95 is out of the range (0.9295, 0.9344), for data drawn from the 
exponential data, the coverage probability is less than 95%.*/


/***********************************************************************

5.2.3 Computing Coverage in the SAS/IML Language

***********************************************************************/

%let N = 50;                          /* size of each sample */
%let NumSamples = 10000;              /* number of samples   */  
proc iml;
call randseed(321);
x = j(&N, &NumSamples);               /* each column is a sample    */
call randgen(x, "Normal");            /* x ~ N(0,1)  x=50x1000 table*/

SampleMean = mean(x);                 /* mean of each column        */
s = std(x);                           /* std dev of each column     */
talpha = quantile("t", 0.975, &N-1);
Lower = SampleMean - talpha * s / sqrt(&N);
Upper = SampleMean + talpha * s / sqrt(&N);

ParamInCI = (Lower<0 & Upper>0);      /* indicator variable         */
PctInCI = ParamInCI[:];               /* pct that contain parameter */
print PctInCI;
quit;


/********************************************************************

 Assessing the Two-Sample t Test for Equality of Means

 *******************************************************************/

/* Test sensitivity of t test to equal variances */
%let n1 = 10;
%let n2 = 10;
%let NumSamples = 10000;                /* number of samples        */

/* Scenario 1: (x1 | c=1) ~ N(0,1);  (x1 | c=2) ~ N(0,1);  			*/
/* Scenario 2: (x2 | c=1) ~ N(0,1);  (x2 | c=2) ~ N(0,10);          */

data EV(drop=i);
label x1 = "Normal data, same variance"
      x2 = "Normal data, different variance";
call streaminit(321);
do SampleID = 1 to &NumSamples;
   c = 1;                               /* Sample from first group  */
   do i = 1 to &n1;
      x1 = rand("Normal");  
      x2 = x1;              			/* The two groups share the same population distribution*/
      output;
   end;
   c = 2;                               /* sample from second group */
   do i = 1 to &n2;
      x1 = rand("Normal");
      x2 = rand("Normal", 0, 10);  	    /* The two groups share different population distribution*/
      output;
   end;
end;
run;

/* 2. Compute statistics */
%ODSOff                          /* suppress output                 */
proc ttest data=EV; 
   by SampleID; 
   class c;                      /* compare c=1 to c=2              */
   var x1-x2;                    /* run t test on x1 and also on x2 */
   ods output ttests=TTests(where=(method="Pooled")); 
run; 
%ODSOn                           /* enable output                   */

/* 3. Construct indicator var for tests that reject H0 at 0.05 significance */ 
data Results; 
   set TTests; 
   RejectH0 = (Probt <= 0.05);           /* H0: mu1 = mu2           */
run; 

/* 3b. Compute proportion: (# that reject H0)/NumSamples */ 
proc sort data=Results; 
   by Variable; 
run; 

proc freq data=Results; 
   by Variable; 
   tables RejectH0 / nocum binomial(level = "1" p=0.05); 
   					/*test if pooled ttest overreject or underreject the null*/
run;

/*Answer for Exercise 5.5*/
/*Since the p-value for the One-sided Binomial test of Pr > Z is <.0001, we may 
conclude that the t test rejects the null hypothesis more than 5% of the time 
(overreject) when the population variances are not equal. Consequently, it is 
wise to use the Satterthwaite test rather than the pooled-variance test if you
suspect unequal variances.*/

/***********************************************************************/

/* Testing assumption of normal data */
/* Scenario 3: (x3 | c=1)~Exp(1);  (x3 | c=2)~Exp(1);  */
/* Scenario 4: (x4 | c=1)~N(0,10); (x4 | c=2)~Exp(10); */

data NND(drop=i);
label x3 = "Exponential data, same variance"
      x4 = "Normal vs. Exponential data, difference variance";
call streaminit(321);
do SampleID = 1 to &NumSamples;
   c = 1;
   do i = 1 to &n1;
      x3 = rand("Exponential");              /* mean = StdDev = 1   */
      x4 = rand("Normal", 10);               /* mean=10; StdDev = 1 */
      output;
   end;
   c = 2;
   do i = 1 to &n2;
      x3 = rand("Exponential");              /* mean = StdDev = 1   */
      x4 = 10 * rand("Exponential");         /* mean = StdDev = 10  */
      output;
   end;
end;
run;

/* 2. Compute statistics */
%ODSOff 
proc ttest data=NND; 
  by SampleID; 
  class c; 
  var x3-x4; 
  ods output ttests=TTests(where=(method="Pooled")); 
run; 
%ODSOn

/* 3. Analyze sampling distribution of statistic */
/* 3a. Construct indicator var for tests that reject H0 at 0.05 significance */ 
data Results; 
  set TTests; 
  RejectH0 = (Probt <= 0.05); 
run; 

/* 3b. Compute proportion: (# that reject H0)/NumSamples */ 
proc sort data=Results; by Variable; run; 

proc freq data=Results; 
  by Variable; 
  tables RejectH0 / nocum; 
run;

/***********************************************************************

Assessing the t test in SAS/IML Software

***********************************************************************/

%let n1 = 10;
%let n2 = 10;
%let NumSamples = 1e4;                /* number of samples */  

proc iml;
/* 1. Simulate the data by using RANDSEED and RANDGEN, */
call randseed(321);
x = j(&n1, &NumSamples);              /* allocate space for Group 1 */
y = j(&n2, &NumSamples);              /* allocate space for Group 2 */
call randgen(x, "Normal", 10);        /* fill matrix from N(0,10)   */
call randgen(y, "Exponential");       /* fill from Exp(1)           */
y = 10 * y;                           /* scale to Exp(10)           */

/* 2. Compute the t statistics; VAR operates on columns */
meanX = mean(x);  varX = var(x);      /* mean & var of each sample  */
meanY = mean(y);  varY = var(y);
/* compute pooled standard deviation from n1 and n2 */
poolStd = sqrt( ((&n1-1)*varX + (&n2-1)*varY)/(&n1+&n2-2) );

/* compute the t statistic */
t = (meanX - meanY) / (poolStd*sqrt(1/&n1 + 1/&n2));

/* 3. Construct indicator var for tests that reject H0 */ 
alpha = 0.05;
RejectH0 = (abs(t)>quantile("t", 1-alpha/2, &n1+&n2-2));  /* 0 or 1 */

/* 4. Compute proportion: (# that reject H0)/NumSamples */ 
Prob = RejectH0[:];
print Prob; /*SAS/IML Summary of t Tests on 10,000 Samples, Nonnormal Data*/
quit;

/*Conclusion: using pooled t-test which assumses equal variances (as in this case (=10) ) 
and same underlying "population" distribution (violated in this case (normal vs expo) ) 
overreject the null that the group means are the same with type 1 error rate around 11%. 
This suggests that the pooled t test should not be used when underlying "population" 
distributions differ*/


/********************************************************************

 Evaluating the Power of the t Test

 *******************************************************************/

proc power;
  twosamplemeans  power = .           /* missing ==> "compute this" */
    meandiff= 0 to 2 by 0.1           /* delta = 0, 0.1, ..., 2     */
    stddev=1                          /* N(delta, 1)                */
    ntotal=20; *, 40, 100;            /* 20 obs in the two samples  */
  plot x=effect markers=none;
  ods output Output=Power;            /* output results to data set */
run;

/*Ron Cody: SAS by Examples
Computing Sample Size for an Unpaired t-Test*/

proc power;
title "Sample Size Requirements for a T-Test";
twosamplemeans
groupmeans = (20 30) (22 28)
stddev = 10 15
power= .80 .90
npergroup = .;
plot x = power min = .70 max = .90;
run;

/***********************************************************************/

/* A Simulated Power Analysis */

/***********************************************************************/


%let n1 = 10;
%let n2 = 10;
%let NumSamples = 10000;               /* number of samples */  

data PowerSim(drop=i);					/* 4200000x4 table =21x10000x20 */
call streaminit(321);
do Delta = 0 to 2 by 0.1;
   do SampleID = 1 to &NumSamples;
      c = 1;
      do i = 1 to &n1;
         x1 = rand("Normal");				/*The null hypothesis		*/
         output;
      end;
      c = 2;
      do i = 1 to &n2;
         x1 = rand("Normal", Delta, 1);     /*The alternative hypothesis -> power*/
         output;
      end;
   end;
end;
run;

/* 2. Compute statistics */
%ODSOff 
proc ttest data=PowerSim; 
   by Delta SampleID; 
   class c; 			/*Check difference between two groups per 21 Delta 1000 Samples*/
   var x1; 
   ods output ttests=TTests(where=(method="Pooled")); 
run; 
%ODSOn

/* 3. Analyze sampling distribution of statistic */
/* 3a. Construct indicator var for obs that reject H0 at 0.05 significance */ 
data Results; 
   set TTests; 
   RejectH0 = (Probt <= 0.05); 	     /* p-value < Type I error rate (=0.05)*/
run; 

/* 3b. Compute proportion: (# that reject H0)/NumSamples */ 
proc freq data=Results; 
   by Delta; 
   tables RejectH0 / nocum out=SimPower(where=(RejectH0=1));
run;

proc power;
  twosamplemeans  power = .           
    meandiff= 0 to 2 by 0.1           
    stddev=1                          
    ntotal=20;             
  plot x=effect markers=none;
  ods output Output=Power;           
run;

/* merge simulation estimates and values from PROC POWER */
data Combine;
   set SimPower Power;
   p = percent / 100;
   label p="Power";
run;

proc sgplot data=Combine noautolegend;
   title "Power of the t Test";
   title2 "Samples are N(0,1) and N(delta,1), n1=n2=10";
   series x=MeanDiff y=Power;
   scatter x=Delta y=p;
   xaxis label="Difference in Population Means (mu2 - mu1)";
run;

/*Exercise 5.9: In Figure 5.9, each estimate is the proportion of times that a binary variable,
RejectH0, equals 1. Use the BINOMIAL option in the TABLES statement in PROC FREQ to
compute 95% confidence intervals for the proportion. Add these confidence intervals to Figure 5.9
by using the YERRORLOWER= option and YERRORUPPER= option in the SCATTER statement.*/

proc freq data=Results noprint;
   by Delta;
   tables RejectH0 / nocum binomial(level='1');
   output out=Est binomial;
run;

data Combine;
   set Est Power;
run;

proc sgplot data=Combine noautolegend;
   series x=MeanDiff y=Power;
   scatter x=Delta y=_BIN_ / yerrorlower=L_Bin yerrorupper=U_Bin;
   yaxis min=0 max=1 label="Power (1 - P[Type II Error])" grid;
   xaxis label="Difference in Population Means (mu2 - mu1)" grid;
run;

/**********************/


/********************************************************************
 Effect of Sample Size on the Power of the t Test
 *******************************************************************/
ODS PDF FILE = '/Users/nathaniellai/Desktop/folders/myfolders/SAS_Sim/Size_Power.pdf' compress=8;

/*Assuming that mu2 = mu1 + 0.5 (delta = 0.5), what should the sample size be so 
that the null hypothesis is rejected (at the 5% sig. lvl) 80% of the time?*/

proc power;
title "Sample Size Requirements for a T-Test";
twosamplemeans
meandiff= 0.5
stddev = 1
power= .80 
npergroup = .;
plot y = power min = .6 max = .95  yopts=(ref=0.8);
ods output Output=req_samp_size;
run;

/*This offical code appears later*/
/*proc power;
twosamplemeans
   meandiff = 0.5
   stddev = 1
   alpha = 0.05
   ntotal = 80 to 200 by 10
   power = .;
plot markers=none;
ods output Output=Power;
run;*/

/*Recreating using Simulation with N = 40,45,...,100*/
/* The null hypothesis for the t test is H0: mu1 = mu2.
   Assume that mu2 = mu1 + delta.
   Find sample size N that rejects H0 80% of the time.  */
%let NumSamples = 1000;            /* number of samples */

data PowerSizeSim(drop=i Delta);
call streaminit(321);
Delta = 0.5;                       /* true difference between means */
do N =  40 to 100 by 5;            /* sample size                   */
   do SampleID = 1 to &NumSamples;
      do i = 1 to N;
         c = 1; x1 = rand("Normal");           output;
         c = 2; x1 = rand("Normal", Delta, 1); output;
      end;
   end;
end;
run;

/* 2. Compute statistics */
%ODSOff 
proc ttest data=PowerSizeSim; 
   by N SampleID; 
   class c; 
   var x1; 
   ods output ttests=TTests(where=(method="Pooled")); 
run; 
%ODSOn

/* 3. Construct indicator var for obs that reject H0 */ 
data ResultsSize; 
set TTests; 
RejectH0 = (Probt <= 0.05); 
run; 

proc freq data=ResultsSize noprint; 
   by N; 
   tables RejectH0 / out=SimPower(where=(RejectH0=1));
run;

proc power;
twosamplemeans
   meandiff = 0.5
   stddev = 1
   alpha = 0.05
   ntotal = 80 to 200 by 10
   power = .;
plot markers=none;
ods output Output=Power;
run;

data Combine;
set SimPower Power;
p = percent / 100;
NSamp = NTotal / 2;
run;

proc sgplot data=Combine noautolegend;
   title "Power of the t Test by Sample Size";
   title2 "Samples are N(0,1) and N(0.5,1), n1=n2=N";
   label N="Size of each sample"  p="Power";
   refline 0.8 / axis=y;
   series x=NSamp y=Power;
   scatter x=N y=p;
   xaxis TYPE= DISCRETE;
run;
title; title2;
* Close the PDF file;
ODS PDF CLOSE;

/********************************************************************
 
 Using Simulation to Compute p-Values
 
*******************************************************************/

proc iml;
Observed = {8 4 4 3 6 11};                       /* observed counts */
k = ncol(Observed);                              /*  6              */
N = sum(Observed);                               /* 36              */
p = j(1, k, 1/k);                                /* {1/6,...,1/6}   */
Expected = N*p;                                  /* {6,6,...,6}     */
qObs = sum( (Observed-Expected)##2/Expected );   /* q=7.667         */

/* simulate from null hypothesis */
NumSamples = 10000;
counts = RandMultinomial(NumSamples, N, p);      /* 10,000 samples  */
*print counts;
Q = ((counts-Expected)##2/Expected )[ ,+];       /* sum each row    */
pval = sum(Q>=qObs) / NumSamples;                /* proportion > q  */
print qObs pval;
call symputx("qObs", qObs);               /* create macro variables */
call symputx("pval", pval);
create chi2 var {Q}; append; close chi2;
quit;

proc sgplot data=chi2;
   title "Distribution of Test Statistic under Null Hypothesis";
   histogram Q / binstart=0 binwidth=1;
   refline &qObs / axis=x;
   inset "p-value = &pval";
   xaxis label="Test Statistic";
run;

/**********************/
/* Answer to exercise */
/**********************/

proc iml;
Observed = {8 4 4 3 6 11};            /* observed counts */
k = ncol(Observed);                   /*  6              */
N = sum(Observed);                    /* 36              */
p = j(1, k, 1/k);                     /* {1/6,...,1/6}   */

NumSamples = 10000;
freq = RandMultinomial(NumSamples, N, p);        /* 10,000 samples  */
*print freq;
x = repeat(1:k, NumSamples);				/*10000x6*/
*print x;
SampleID = repeat(T(1:NumSamples), 1, k); 	/*10000x6*/
*print sampleID;
create die var {"SampleID" "Freq" "x"}; append; close;
quit;

proc freq data=die noprint;
  by SampleID;
  weight Freq;
  tables x / chisq;
  output out=chi2 chisq;
run;

proc sgplot data=chi2;
  title "Distribution of Test Statistic under Null Hypothesis";
  histogram _pchi_ / binstart=0 binwidth=1;
  refline 7.67 / axis=x;
  xaxis label="Test Statistic";
run;

/**********************
Recall
***********************/

proc iml;
call randseed(4321);                   /* set random number seed */
c = {"black", "brown", "white"};
prob = {0.5     0.2       0.3};        /* probabilities of pulling each color */
x = RandMultinomial(10, 100, prob);  /* 10 draws of 100 socks with 3 colors -> 10x3 */
print x;

y = repeat(1:3, 5);
print y;  					/*5x3*/

z = repeat(T(1:5), 1, 3);
print z;					/*5x3*/
quit;