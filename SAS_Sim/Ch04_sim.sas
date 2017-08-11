/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 4: Simulating Data to Estimate Sampling Distributions
 
  Simulation by Using the DATA Step and SAS Procedures
  Using a Sampling Distribution to Estimate Probability
  Properties for Sampling Distribution for Normal statistics
  The Effect of Sample Size on Smapling Distrbution
  Bias of kurtosis in small samples
  Simulating Data Using SAS/IML 
  The Sampling Distribution of Pearson Correlations
  
 ***********************************************************************/


/********************************************************************
 Simulation by Using the DATA Step and SAS Procedures
 *******************************************************************/

/*Sampling distribution of U(0,1) with 1000 sample size of 10*/


%let N = 10;                           /* size of each sample */
%let NumSamples = 1000;                /* number of samples   */  
/* 1. Simulate data */
data SimUni;						   /*SimUni is a 10000x3 tabe*/
call streaminit(123);
do SampleID = 1 to &NumSamples;
   do i = 1 to &N;
      x = rand("Uniform");
      output;
   end;
end;
run;

/* 2. Compute mean for each sample */
proc means data=SimUni noprint;
   by SampleID;
   var x;
   output out=OutStatsUni mean=SampleMean;  /* 1000x4 table*/ 
run;

/* 3. Analyze ASD: summarize and create histogram */
proc means data=OutStatsUni N Mean Std P5 P95;
   var SampleMean;
run;

ods graphics on;                              /* use ODS graphics   */
proc univariate data=OutStatsUni;
   label SampleMean = "Sample Mean of U(0,1) Data";
   histogram SampleMean / normal;             /* overlay normal fit */
   ods select Histogram;
run;

proc univariate data=OutStatsUni noprint;
   var SampleMean;
   output out=Pctl95 N=N mean=Mean pctlpts=2.5 97.5 pctlpre=Pctl;
run;

proc print data=Pctl95 noobs; 
run;


/*Exercise 4.1: The standard exponential distribution has a mean and variance that are both 1.
Generate 1,000 samples of size N D 64, and compute the mean and standard deviation of the ASD.
Is the standard deviation close to 1/8?*/

/*Sampling distribution of Exp(1,1) with 1000 sample size of 64*/

%let samplesize = 64;
%let sample_num = 1000;
%let sigma = 1;

data SimExp;
call streaminit(123);
do Sample_ID = 1 to &sample_num;
	do i = 1 to &samplesize;
		x = &sigma * rand("Exponential");
		output;
	end;
end;
run;

proc means data=SimExp noprint;
   by Sample_ID;
   var x;
   output out=OutStatsExp mean=SampleMean;  /* 1000x4 table*/ 
run;

proc means data=OutStatsExp N Mean Std P5 P95;
   var SampleMean;
run;

ods graphics on;                              /* use ODS graphics   */
proc univariate data=OutStatsExp;
   label SampleMean = "Sample Mean of Exp(1,1) Data";
   histogram SampleMean / normal;             /* overlay normal fit */
   ods select Histogram;
run;

proc univariate data=OutStatsExp noprint;
   var SampleMean;
   output out=Pctl95_exp N=N mean=Mean pctlpts=2.5 97.5 pctlpre=Pctl std= std;  
run;

proc print data=Pctl95_exp noobs; 
run;   			/*Standard deviation close to 0.125*/



/***********************************************************************/

/*4.4.3 Using a Sampling Distribution to Estimate Probability*/

/*What is the probability that the mean of the sample is greater than 0.7?*/

data Prob;
   set OutStatsUni;
   LargeMean = (SampleMean>0.7);       /* create indicator variable: P-value */
run;

proc freq data=Prob;
   tables LargeMean / nocum;           /* compute proportion        */
run;						  /* Only 1.9% of the simulated means are greater than 0.7. It is
rare to observe a mean this large in a random draw of size 10 from the uniform distribution.*/

/*Alternativly using proc format*/

proc format;
   value CutVal low-<0.7="less than 0.7"  0.7-high="greater than 0.7";
run;

proc freq data = Prob;
	table LargeMean / nocum format = CutVal.;
run;	

/**********************/
/* Answer to exercise */
/**********************/
proc freq data=OutStatsUni;
   format SampleMean CutVal.;
   tables SampleMean / nocum;    /* compute proportion */
run;
/**********************/


/***********************************************************************/

/*Properties for Sampling Distribution for Normal statistics

• Given a sample with an odd number of points, for example N = 2k + 1, the variance of the
sample mean is about 64% smaller than the variance of the sample median. More precisely,
the ratio of the variances is (4k)/(pi*(2k+1)), which approaches 2=/pi or 0.64 for large k.

• The sampling distribution of the variance follows a scaled chi-square distribution with N - 1
degrees of freedom

*/

/*Sampling distribution of N(0,1) with 10000 sample size of 31*/

%let N = 31;                           /* size of each sample -> k =15*/
%let NumSamples = 10000;               /* number of samples   */
/* 1. Simulate data */
data SimNormal;
call streaminit(123);
do SampleID = 1 to &NumSamples;
   do i = 1 to &N;
      x = rand("Normal");
      output;
   end;
end;
run;

/* 2. Compute statistics for each sample */
proc means data=SimNormal noprint;
   by SampleID;
   var x;
   output out=OutStatsNorm mean=SampleMean median=SampleMedian var=SampleVar;
run; 	/*Generating sampling distributions for the mean, median and variance*/ 


/* variances of sampling distribution for mean and median */
proc means data=OutStatsNorm Var;
   var SampleMean SampleMedian;
run;

proc sgplot data=OutStatsNorm;
   title "Sampling Distributions of Mean and Median for N(0,1) Data";
   density SampleMean /   type=kernel legendlabel="Mean";
   density SampleMedian / type=kernel legendlabel="Median";
   refline 0 / axis=x;
run;

/***********************************************************************/

/*Fitting a Chisquare(n-1) to the sample variance calculated above*/

/* scale the sample variances by (N-1)/sigma^2 */
data OutStatsNorm;
   set OutStatsNorm;
   ScaledVar = SampleVar * (&N-1)/1; 
run;

/* Fit chi-square distribution to data */
proc univariate data=OutStatsNorm;
   label ScaledVar = "Variance of Normal Data (Scaled)";
   histogram ScaledVar / gamma(alpha=15 sigma=2);   /* gamma(d/2, 2) = chi-square(d) */
   ods select Histogram;
run;

/***********************************************************************/

/*The Effect of Sample Size on Smapling Distrbution*/

%let NumSamples = 1000;                /* number of samples */
/* 1. Simulate data.  Create a 1190000x4 table (1190000=(10+30+50+100+1000)*1000)*/
data SimUniSize;
call streaminit(123);
do N = 10, 30, 50, 100, 1000;
   do SampleID = 1 to &NumSamples;
      do i = 1 to N;
         x = rand("Uniform");
         output;
      end;
   end;
end;
run;  

/* 2. Compute mean for each sample using by sampleID -> create a 5000x5 table (5 classes)*/
proc means data=SimUniSize noprint;
   by N SampleID;
   var x;
   output out=OutStats mean=SampleMean;
run;

/* 3. Summarize approx. sampling distribution of statistic */
proc means data=OutStats Mean Std;
   class N;
   var SampleMean;
run;

proc means data=OutStats noprint;       /*Save the results into Out dataset*/
  class N;
  var SampleMean;
  output out=out(where=(_TYPE_=1)) Mean=Mean Std=Std;  
run;

proc iml;			/*plotting the noral pdfs for N = 10, 30, 50 , 100, 1000 using out dataset*/
use out;
read all var {N Mean Std};
close out;

NN = N;
*print NN;
x = T( do(0.1, 0.9, 0.0025) );
*print x;

create Convergence var {N x pdf};
do i = 1 to nrow(NN);   						/*do i = 1 to 5*/
   N = j(nrow(x), 1, NN[i]); 	/*Create 5 vectors of length x axis with values 10, 30, 50 ,100, 1000*/
   pdf = pdf("Normal", x, Mean[i], Std[i]);    /*Recall Out was inputted as a matrix*/
   append;
end;
close Convergence;

quit;

ods graphics / ANTIALIASMAX=1300;
proc sgplot data=Convergence;
   title "Sampling Distribution of Sample Mean";
   label pdf = "Density"
	 N = "Sample Size";
   series x=x y=pdf / group=N;
run;

/**********************/
/* Answer to exercise */
/**********************/
/* Partial solution. Generate data and use the following: */

proc means data=OutStats noprint;
   class N;
   var SampleMean;
   output out=stderr std=s;
run;

proc sgplot data=stderr;
   series x=N y=s;
   yaxis min=0;
run;

/**********************/


/***********************************************************************/

/* Bias of kurtosis in small samples */

%let N = 50;                         /* Change the size of each sample */
%let NumSamples = 1000;              /* number of samples   */  
data SimSK(drop=i);
call streaminit(123);
do SampleID = 1 to &NumSamples;      /* simulation loop             */
   do i = 1 to &N;                   /* N obs in each sample        */
      Normal      = rand("Normal");  /* kurt=0                      */
      t           = rand("t", 5);    /* kurt=6 for t, exp, and logn */
      Exponential = rand("Expo");
      LogNormal   = exp(rand("Normal", 0, 0.503)); 
      output;
   end;
end;
run;

proc means data=SimSK noprint;
   by SampleID;
   var Normal t Exponential LogNormal;
   output out=Moments(drop=_type_ _freq_) Kurtosis=;
run;

proc transpose data=Moments out=Long(rename=(col1=Kurtosis));  /*For plotting*/
   by SampleID;
run;

proc sgplot data=Long;
   title "Vertical Boxplot for Kurtosis Bias in Small Samples: N=&N";
   label _Name_ = "Distribution";
   vbox Kurtosis / category=_Name_ meanattrs=(symbol=Diamond);
   refline 0 6 / axis=y;
   yaxis max=30;
   xaxis discreteorder=DATA;
run;

/*Histogram view*/

proc sgplot data=Moments;
   title "Normal Kurtosis Bias in Small Samples: N=&N";
   histogram Normal;
   refline 0 / axis=x;
   yaxis max=30;
   xaxis discreteorder=DATA;
run;
title;
proc sgplot data=Moments;
   title "t Kurtosis Bias in Small Samples: N=&N";
   histogram t;
   refline  6/ axis=x;
   yaxis max=30;
   xaxis discreteorder=DATA;
run;
title;

proc sgplot data=Moments;
   title "Exponential Kurtosis Bias in Small Samples: N=&N";
   histogram Exponential;
   refline 6 / axis=x;
   yaxis max=30;
   xaxis discreteorder=DATA;
run;
title;

proc sgplot data=Moments;
   title "LogNormal Kurtosis Bias in Small Samples: N=&N";
   histogram LogNormal;
   refline 6 / axis=x;
   yaxis max=30;
   xaxis discreteorder=DATA;
run;
title;

/*
Exercise 4.5: Repeat the simulation and redraw Figure 4.12 for samples that contain N = 2000
observations. Compare the range of sample kurtosis values for N = 50 and N = 2000.

Exercise 4.6: Is the skewness statistic also biased for small samples? Modify the example in this
section to compute and plot the skewness of 1,000 random samples for N = 50. The skewness for
each distribution is given in Table 4.1.
*/


/********************************************************************
 
 
 Simulating Data by Using the SAS/IML Language
 
 
*********************************************************************/

%let N = 10;
%let NumSamples = 1000;
proc iml;
call randseed(123);
x = j(&NumSamples,&N);       /* many samples (rows), each of size N */
call randgen(x, "Uniform");  /* 1. Simulate data                    */
s = x[,:];                   /* 2. Compute statistic for each row   */
Mean = mean(s);              /* 3. Summarize and analyze ASD        */
StdDev = std(s);
call qntl(q, s, {0.05 0.5 0.95});
print Mean StdDev (q`)[colname={"5th Pctl" "50th Pctl" "95th Pctl"}];

/* compute proportion of statistics greater than 0.7 */
Prob = mean(s > 0.7);
print Prob[format=percent7.2];

/* 
Exercise 4.7: Rewrite the simulation so that each column of x is a sample. The column means form
the ASD. Use the T function, which transposes a matrix, prior to computing the summary statistics.
*/

proc iml;
call randseed(123);
x = t(j(&N, &NumSamples));       /* many samples (columns), each of size N */
call randgen(x, "Uniform");  /* 1. Simulate data                    */
x = t(x);
s = t(x[:,]);                   /* 2. Compute statistic for each row   */
Mean = mean(s);              /* 3. Summarize and analyze ASD        */
StdDev = std(s);
call qntl(q, s, {0.05 0.5 0.95});
print q;
print Mean StdDev (t(q))[colname={"5th Pctl" "50th Pctl" "95th Pctl"}];

/*The results are not the same because the randgen function generates the result differently but 
with t() in the second line, this is changed*/


/*
Exercise 4.8: Use the SAS/IML language to estimate the sampling distribution for the maximum
of 10 uniform random variates. Display the summary statistics. 
*/

proc iml;
call randseed(123);
x = j(10000, 10);
call randgen(x, "Uniform");  * 1. Simulate data;
s = x[,<>];                  * 2. Compute statistic for each row(sample): element maximum operator: <>;
Mean = mean(s);              * 3. Summarize and analyze ASD;
StdDev = std(s);
call qntl(q, s, {0.05 0.95});
print Mean StdDev (q`)[colname={"5th Pctl" "95th Pctl"}];
create MaxDist var {s}; append; close MaxDist;

proc univariate data=MaxDist(rename=(s=Max));
   label Max = "Maximum of Uniform Sample, N=10";
   histogram Max;
   ods select Histogram;
run;  

 
/**********************/


/***********************************************************************/


/*Reshaping Matrices*/


/***********************************************************************/

%let N = 10;
%let NumSamples = 1000;

proc iml;
call randseed(123);
x = j(&NumSamples,&N);       /* many samples (rows), each of size N */
/* "long" format: first generate data IN ROWS... */
call randgen(x, "Uniform");       /* 1. Simulate data (all samples) */
ID = repeat( T(1:&NumSamples), 1, &N); /* {1   1 ...   1,
                                           2   2 ...   2,
                                         ... ... ... ...
                                         1000 1000 ... 1000} -> 1000x10 table*/
*print id;
/* ...then convert to long vectors and write to SAS data set */
SampleID = shape(ID, 0, 1);     /* 1 col, 10000 rows as necessary */
*print sampleid;
z = shape(x, 0, 1);				/* 1 col, 10000 rows as necessary */
*print z;
create Long var{SampleID z}; append; close Long;		/*10000x2*/
create Long2 var{ID x}; append; close Long2;			/*10000x2*/
/*The two tables are the same with different column names*/


/***********************************************************************/

/*4.5.3 The Sampling Distribution of Pearson Correlations*/

%let N = 20;                      /* size of each sample */
%let NumSamples = 1000;           /* number of samples   */  
proc iml;
call randseed(123);
mu = {0 0}; Sigma = {1 0.3, 0.3 1};
rho = j(&NumSamples, 1);          /* allocate vector for results  1000x1  */
do i = 1 to &NumSamples;          /* simulation loop                */
   x = RandNormal(&N, mu, Sigma); /* simulated data in N x 2 matrix */
   rho[i] = corr(x)[1,2];         /* Pearson correlation            */
end;
*print x, rho;
/* compute quantiles of ASD; print with labels */
call qntl(q, rho, {0.05 0.25 0.5 0.75 0.95});
*print (q`)[colname={"P5" "P25" "Median" "P75" "P95"}];
create corr var {"Rho"}; append; close;       /* write ASD */
quit;


/* 3. Visualize approx. sampling distribution of statistic */
ods graphics on;
proc univariate data=Corr;
   label Rho = "Pearson Correlation Coefficient";
   histogram Rho / kernel;
   ods select Histogram;
run;

/*Exercise 4.9: Use the ASD to estimate the probability that the sample correlation coefficient
is negative for a sample of size 20 from the bivariate normal distribution in this section. (See
Section 4.4.3.)*/

data ProbCorr;
set corr;
t = (Rho < 0);
run;
ods graphics off;

proc freq data=ProbCorr;
   tables t / nocum ;
run;

/**********************/

