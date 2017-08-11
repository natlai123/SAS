/*The code in this file is hard to interpret and definitely elementary*/

/*Exercise 5.6: The relative magnitudes of the population variances determine the proportion of
samples in which the t test rejects the null hypothesis. Rerun the simulation when x2 (for c=2) is
drawn from the N.0; 2/, N.0; 5/, and N.0; 100/ distributions. How sensitive is the pooled-variance
t test to differences in the population variances?*/

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

%let n1 = 10;
%let n2 = 10;
%let NumSamples = 10000;                /* number of samples        */

/*N(0,2)*/

data EV(drop=i);
label x1 = "Normal data, same variance"
      x2 = "Normal data, different variance";
call streaminit(321);
do SampleID = 1 to &NumSamples;
   c = 1;                               /* sample from first group  */
   do i = 1 to &n1;
      x1 = rand("Normal");  
      x2 = x1;              			/* The two groups share the same population distribution*/
      output;
   end;
   c = 2;                               /* sample from second group */
   do i = 1 to &n2;
      x1 = rand("Normal");
      x2 = rand("Normal", 0, 2);  	    /* The two groups share different population distribution*/
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
title "When x2 is drawn from N(0,2)";
   by Variable; 
   tables RejectH0 / nocum binomial(level = "1" p=0.05); 
   					/*test if pooled ttest overreject or underreject the null*/
run;


/*N(0,5)*/

data EV(drop=i);
label x1 = "Normal data, same variance"
      x2 = "Normal data, different variance";
call streaminit(321);
do SampleID = 1 to &NumSamples;
   c = 1;                               /* sample from first group  */
   do i = 1 to &n1;
      x1 = rand("Normal");  
      x2 = x1;              			/* The two groups share the same population distribution*/
      output;
   end;
   c = 2;                               /* sample from second group */
   do i = 1 to &n2;
      x1 = rand("Normal");
      x2 = rand("Normal", 0, 5);  	    /* The two groups share different population distribution*/
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
title "When x2 is drawn from N(0,5)";
   by Variable; 
   tables RejectH0 / nocum binomial(level = "1" p=0.05); 
   					/*test if pooled ttest overreject or underreject the null*/
run;



/*N(0,10)*/

data EV(drop=i);
label x1 = "Normal data, same variance"
      x2 = "Normal data, different variance";
call streaminit(321);
do SampleID = 1 to &NumSamples;
   c = 1;                               /* sample from first group  */
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
title "When x2 is drawn from N(0,10)";
   by Variable; 
   tables RejectH0 / nocum binomial(level = "1" p=0.05); 
   					/*test if pooled ttest overreject or underreject the null*/
run;



/*N(0,100)*/

data EV(drop=i);
label x1 = "Normal data, same variance"
      x2 = "Normal data, different variance";
call streaminit(321);
do SampleID = 1 to &NumSamples;
   c = 1;                               /* sample from first group  */
   do i = 1 to &n1;
      x1 = rand("Normal");  
      x2 = x1;              			/* The two groups share the same population distribution*/
      output;
   end;
   c = 2;                               /* sample from second group */
   do i = 1 to &n2;
      x1 = rand("Normal");
      x2 = rand("Normal", 0, 100);  	    /* The two groups share different population distribution*/
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
title "When x2 is drawn from N(0,100)";
   by Variable; 
   tables RejectH0 / nocum binomial(level = "1" p=0.05); 
   					/*test if pooled ttest overreject or underreject the null*/
run;
