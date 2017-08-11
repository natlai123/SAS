/*Exercise 4.6: Is the skewness statistic also biased for small samples? Modify the example in this
section to compute and plot the skewness of 1,000 random samples for N = 50. The skewness for
each distribution is given in Table 4.1.*/

/* Bias of Skewness in small samples */

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
   output out=Moments(drop=_type_ _freq_) Skewness=;
run;

proc transpose data=Moments out=Long(rename=(col1=Skewness));  /*For plotting*/
   by SampleID;
run;

proc sgplot data=Long;
   title "Vertical Boxplot for Skewness  Bias in Small Samples: N=&N";
   label _Name_ = "Distribution";
   vbox Skewness / category=_Name_ meanattrs=(symbol=Diamond);
   refline 0 / axis=y lineattrs=(color=black);
   refline 2 / axis=y lineattrs=(color=red) lineattrs=(pattern=dot);
   refline 1.764 / axis=y lineattrs=(color=blue);   
   yaxis max=6;
   xaxis discreteorder=DATA;
run;

/*Histogram view*/

proc sgplot data=Moments;
   title "Normal Skewness Bias in Small Samples: N=&N";
   histogram Normal;
   refline 0 / axis=x;
   yaxis max=30;
   xaxis discreteorder=DATA;
run;

proc sgplot data=Moments;
   title "t Skewness  Bias in Small Samples: N=&N";
   histogram t;
   refline  0/ axis=x;
   yaxis max=30;
   xaxis discreteorder=DATA;
run;

proc sgplot data=Moments;
   title "Exponential Skewness  Bias in Small Samples: N=&N";
   histogram Exponential;
   refline 2 / axis=x;
   yaxis max=30;
   xaxis discreteorder=DATA;
run;

proc sgplot data=Moments;
   title "LogNormal Skewness Bias in Small Samples: N=&N";
   histogram LogNormal;
   refline 1.764 / axis=x;
   yaxis max=30;
   xaxis discreteorder=DATA;
run;

/* The sample skewness for Normal and t distributions are not biased. With a 
sample size of 50, that of exponential is slightly underestimeated while that 
of lognormal is greatly underestimated.  */