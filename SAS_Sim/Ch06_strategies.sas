/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 6: Strategies for Efficient and Effective Simulation
 
 The Design of a Simulation Study
 Writing Efficient Simulations
 Profiling a SAS Simulation
 
/***********************************************************************

The chapter contains some general tips on simulation, as below:
 
When using a by statement:

• Identify each sample by a unique value of the SampleID variable. Use a BY statement to
compute statistics for each BY group.

• Suppress all output during the BY group analysis. Many procedures have a NOPRINT option
in the PROC statement. Otherwise, use the method described in Section 6.4.2 to suppress
ODS output.

When you use the SAS/IML in-memory technique, do the following:

• Use the J function to allocate a vector (or matrix) to store the simulated data before you call
the RANDGEN subroutine. This enables you to generate an entire sample (or even multiple
samples) with a single call to the RANDGEN subroutine. Do not generate one random value
at a time.

• When possible, compute statistics for all samples with a single call. For example, the MEAN
function can compute the means of all columns in a matrix. The subscript reduction operator
x[,:] computes the means of all rows in the x matrix.

• If you are analyzing multivariate data, then it is often convenient to generate the data in a DO
loop. At each iteration, generate a single sample and compute the statistic on that sample.
This approach is described further in Chapter 8, “Simulating Data from Basic Multivariate
Distributions
 
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
 
 The Design of a Simulation Study
 
*******************************************************************/

%let N = 20;
%let NumSamples = 1000;                /* number of samples    */  
data RegSim;
call streaminit(1);
do i = 1 to &N;
   x = (i-1)/(&N-1);
   do beta = 0 to 3 by 0.2;
      eta = 1 + beta*x;                /* linear predictor     */
      do SampleID = 1 to &NumSamples;
         y = eta + rand("Normal");
         output;
      end; 
   end;                                /* end beta loop        */
end;                                   /* end observation loop */
run;

proc sort data=RegSim out=Sim;
   by beta SampleID;
run;

/* Turn off output when calling PROC for simulation */
%ODSOff
proc reg data=Sim;
   by beta SampleID;
   model y = x;
   test x=0;
   ods output TestANOVA=TestAnova;
quit;
%ODSOn

/* 3. Construct an indicator variable for observations that reject H0 */
data Results;
   set TestANOVA(where=(Source="Numerator"));
   Reject = (ProbF <= 0.05);           /* indicator variable    */
run;

/* count number of times H0 was rejected */
proc freq data=Results noprint;
   by beta;
   tables Reject / nocum binomial(level='1');
   output out=Est binomial;
run;

title "Preliminary Power of Test for Beta=0, OLS";
title2 "Normal Errors, Equally Spaced Design, N = &N, &NumSamples Samples";
proc sgplot data=Est noautolegend;
   series x=beta y=_BIN_;
   scatter x=beta y=_BIN_ / yerrorlower=L_Bin yerrorupper=U_Bin;
   yaxis min=0 max=1 label="Power (1 - P[Type II Error])" grid;
   xaxis label="Beta" grid;
run;

/********************************************************************
 
 Writing Efficient Simulations
 
 *******************************************************************/

/* suppress output to ODS destinations */
ods graphics off;
ods exclude all; 
ods noresults;

ods graphics on;
ods exclude none; 
ods results;

%macro ODSOff;                 /* Call prior to BY-group processing */
ods graphics off;
ods exclude all;
ods noresults;
%mend;

%macro ODSOn;                  /* Call after BY-group processing    */
ods graphics on;
ods exclude none;
ods results;
%mend;

%let N = 31;                           /* size of each sample */
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

%ODSOff
proc means data=SimNormal;
   by SampleID;
   var x;
   ods output Summary=Desc;
run;
%ODSOn

/***********************************************************************/

*options nonotes;  
*options notes;  

*proc printto log=<>;
*run;

/***********************************************************************/

/*************************************/
/* DO NOT USE THIS CODE: INEFFICIENT */
/*************************************/

%macro Simulate(N, NumSamples);
options nonotes;                       /* turn off notes to log     */
proc datasets nolist; 
   delete OutStats;                    /* delete data if it exists  */
run;

%do i = 1 %to &NumSamples;
   data Temp;                          /* create one sample         */
   call streaminit(0);
   do i = 1 to &N;
      x = rand("Uniform");
      output;
   end;
   run;

   proc means data=Temp noprint;       /* compute one statistic     */
      var x;
      output out=Out mean=SampleMean;
   run;
   
   proc append base=OutStats data=Out;          /* accumulate stats */
   run;  
%end;
options notes;
%mend;

/* call macro to simulate data and compute ASD */
%Simulate(10, 100)               /* means of 100 samples of size 10 */

/***********************************************************************/

/*Profiling a SAS/IML Simulation*/

/*Suppose that part of your simulation involves finding the eigenvalues of a large matrix, and that the
size of the matrix varies with the number of variables in the simulation. The following SAS/IML
program generates random symmetric n  n matrices for various values of n, and times how long it
takes to compute the eigenvalues:*/

proc iml;
size = do(500, 2000, 250);    /* 500, 1000, ..., 2000  (1x7 Vector) */
*print size;
time = j(1, ncol(size));      /* allocate vector for results        */
*print time;
call randseed(12345); 
do i = 1 to ncol(size);		  /*The size of the matrix expands 		*/
   n = size[i];
   r = j(n*(n+1)/2, 1);       /* generate lower triangular elements */
   call randgen(r, "uniform");
   A = sqrvech(r);            /* create symmetric matrix            */

   t0 = time();
   evals = eigval(A);
   time[i] = time()-t0;       /* elapsed time for computation       */
end;
create eigen var {"Size" "Time"}; append; close;
quit;

proc sgplot data=eigen;
   title "Performance of Eigenvalue Computation";
   series x=Size y=Time / markers;
   yaxis grid label="Time to Compute Eigenvalues (s)";
   xaxis grid label="Size of Matrix";
run;

/*If you run the above code several times, you will notice that the time needed 
for simulation changes (approximately requiring 2.3 seconds, way faster than the 
one reported on Figure 6.3 on the book)*/

/*Exercise 6.2: Run a DATA step that generates one million samples, each containing 1,000 random
normal observations. Check the SAS log to determine how long it takes to create that simulated data.*/

/*Don't run 1 million samples. It takes forever and screw up SAS University Edition*/
%let N = 1000;
%let NumSamples = 10000;            
data MilSim;
call streaminit(1);
do SampleID = 1 to &NumSamples;
	do i = 1 to &N;    
         x = rand("Normal");
         output;
      end; 
   end;                                
run;                              




