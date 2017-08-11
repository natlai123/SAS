/***********************************************************************

Progams from http://support.sas.com/kb/24/982.html
	Bootstrap from Regression Estimates
	
***********************************************************************/

%inc "/folders/myfolders/SAS_Sim/Boot_Macro.sas";
option nosymbolgen; 

title 'Spatial Test Data from Efron and Tibshirani, pp 180 & 183';
   data spatial;
      input a b @@;
   cards;
   48 42 36 33 20 16 29 39 42 38 42 36 20 15 42 33 22 20 41 43 45 34
   14 22  6  7  0 15 33 34 28 29 34 41  4 13 32 38 24 25 47 27 41 41
   24 28 26 14 30 28 41 40
   ;

   %macro analyze(data=,out=);
      proc means noprint data=&data vardef=n;
         output out=&out(drop=_freq_ _type_) var=var_a var_b;
         var a b;
         %bystmt;
      run;
   %mend;

   title2 'Jackknife Interval with Bias Correction';
   %jack(data=spatial,alpha=.10);

   title2 'Normal ("Standard") Confidence Interval with Bias Correction';
   %boot(data=spatial,alpha=.10,samples=2000,random=123);

   title2 'Normal ("Standard") Confidence Interval without Bias Correction';
   %bootse(alpha=.10,biascorr=0);

   title2 'Efron''s Percentile Confidence Interval';
   %bootci(percentile,alpha=.10)

   title2 'Hybrid Confidence Interval';
   %bootci(hybrid,alpha=.10)

   title2 'BC Confidence Interval';
   %bootci(bc,alpha=.10)

   title2 'BCa Confidence Interval';
   %bootci(bca,alpha=.10)


   title2 'Resampling with Computation of Studentizing Statistics';
   %macro analyze(data=,out=);
      proc means noprint data=&data vardef=n;
         output out=&out(drop=_freq_ _type_)
            var=var_a var_b kurtosis=kurt_a kurt_b;
         var a b;
         %bystmt;
      run;
      data &out;
         set &out;
         stud_a=var_a*sqrt(kurt_a+2);
         stud_b=var_b*sqrt(kurt_b+2);
         drop kurt_a kurt_b;
      run;
   %mend;

   %boot(data=spatial,stat=var_a var_b,samples=2000,random=123);

   title2 'T Confidence Interval';
   %bootci(t,stat=var_a var_b,student=stud_a stud_b,alpha=.10)
/*If you want to compute all the varieties of confidence intervals, you can use the %ALLCI macro:*/
   title2 'All Jackknife and Bootstrap Confidence Intervals';
   %allci(stat=var_a var_b,student=stud_a stud_b,alpha=.10)





/***************************************************************/

title 'Cement Hardening Data from Hjorth, p 31';
   data cement;
      input x1-x4 y;
      label x1='3CaOAl2O3'
            x2='3CaOSiO2'
            x3='4CaOAl2O3Fe2O3'
            x4='2CaOSiO2';
   cards;
    7 26  6 60  78.5
    1 29 15 52  74.3
   11 56  8 20 104.3
   11 31  8 47  87.6
    7 52  6 33  95.9
   11 55  9 22 109.2
    3 71 17  6 102.7
    1 31 22 44  72.5
    2 54 18 22  93.1
   21 47  4 26 115.9
    1 40 23 34  83.8
   11 66  9 12 113.3
   10 68  8 12 109.4
   ;

   proc reg data=cement;
      model y=x1-x4;
      output out=cemout r=resid p=pred;
   run;

%inc "/folders/myfolders/SAS_Sim/Boot_Macro.sas";
   %macro analyze(data=,out=);
      options nonotes;
      proc reg data=&data noprint
               outest=&out(drop=Y _IN_ _P_ _EDF_);
         model y=x1-x4/selection=rsquare start=4;
         %bystmt;
      run;
      options notes;
   %mend;

   title2 'Resampling Observations';
   title3 '(bias correction for _RMSE_ is wrong)';
   %boot(data=cement,random=123)

   title2 'Resampling Residuals';
   title3 '(bias correction for _RMSE_ is wrong)';
   %boot(data=cemout,residual=resid,equation=y=pred+resid,random=123)