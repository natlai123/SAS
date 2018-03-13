%let path = /folders/myshortcuts/sf__myfolders/ECST131 ;
libname statdata "&path";

*******************************************************;

/*Using PROC MEANS to Generate Descriptive Statistics*/

proc means data=statdata.testscores maxdec=2 fw=10 printalltypes
           n nmiss mean median std var q1 q3;
   class Gender;
   var SATScore;
   title 'Selected Descriptive Statistics for SAT Scores';
run;
title; 

/*Using SAS to Picture Your Data*/

*ods graphics on/width=600;
proc univariate data=statdata.testscores ;
   var SATScore;
   id idnumber;
   histogram SATScore / normal(mu=est sigma=est);
   inset skewness kurtosis / position=ne;
   probplot SATScore / normal(mu=est sigma=est);
   inset skewness kurtosis;
   title 'Descriptive Statistics Using PROC UNIVARIATE';
run;
title;

proc sgplot data=statdata.testscores;
   refline 1200 / axis=y lineattrs=(color=blue);
   vbox SATScore / datalabel=IDNumber;
   format IDNumber 8.0;
   title "Box Plots of SAT Scores";
run;

/*Practice*/

proc univariate data=statdata.normtemp noprint;
   var BodyTemp HeartRate;
   histogram BodyTemp HeartRate / normal(mu=est sigma=est noprint);
   inset min max skewness kurtosis / position=ne;
   probplot BodyTemp HeartRate / normal(mu=est sigma=est);
   qqplot BodyTemp HeartRate / normal(mu=est sigma=est);
   cdfplot BodyTemp HeartRate / normal(mu=est sigma=est);
   inset min max skewness kurtosis;
   title 'Descriptive Statistics Using PROC UNIVARIATE';
run;
title;

proc sgplot data=statdata.normtemp;
   refline 98.6 / axis=y lineattrs=(color=blue);
   vbox BodyTemp / datalabel=ID;
   format ID 3.;
   title "Box Plots of Body Temps";
run;

proc sgplot data=statdata.normtemp;
   vbox HeartRate / datalabel=ID;
   format ID 3.;
   title "Box Plots of Heart Rate";
run;
title;

ods select testsforlocation;
proc univariate data=statdata.testscores mu0=1200;
   var SATScore;
   title 'Testing Whether the Mean of SAT Scores=1200';
run;

/*****************************************************************************/

proc sgplot data=statdata.ameshousing3;
   vbox SalePrice / category=Central_Air
                    connect=mean;
   title "Sale Price Differences across Central Air";
run;
 
proc sgplot data=statdata.ameshousing3;
   vbox SalePrice / category=Fireplaces
                    connect=mean;
   title "Sale Price Differences across Fireplaces";
run;
 
proc sgplot data=statdata.ameshousing3;
   vbox SalePrice / category=Heating_QC
                    connect=mean;
   title "Sale Price Differences across Heating Quality";
run;

*The two-sample t-test is a hypothesis test for answering questions 
about the means of two populations. This test enables you to examine 
the differences between populations for one or more continuous variables. 
You can assess whether the means of the two populations are statistically 
different from each other. The null hypothesis for the two-sample t-test is 
that the means for the two groups are equal, or that μ1 - μ2 equals 0.

When you compare the means of two populations using a two-sample t-test, you 
make three assumptions: the data contains independent observations, the 
distributions of the two populations are normal, and the variances in these 
normal distributions are equal.

To evaluate the assumption of equal variances in the two populations, you can 
use the F-test for equality of variances. To test the hypothesis, you calculate 
the F statistic, which is the ratio of the maximum sample variance of the two groups
to the minimum sample variance of the two groups. By construction, the F statistic 
is always greater than or equal to 1.;

PROC TTEST DATA=SAS-data-set <options eg sides = U>;
        CLASS variables;
        VAR variables;
RUN;

proc ttest data=statdata.testscores 
           plots(shownull)=interval; 
   class Gender;
   var SATScore;
   title "Two-Sample t=Test Comparing Girls to Boys";
run;

*Result: The p-value of the F-test is 0.7446, and this probability is greater than 0.05, 
your alpha, so you fail to reject the null hypothesis and can proceed as if the variances 
are equal between the groups. You should use the equal variance t-test, or the Pooled t-test. 
By default, SAS shows the 95% intervals for both the Pooled method, assuming equal variances 
for group 1 and group 2, and the Satterthwaite method, assuming unequal variances.

An advantage of a one-sided test is that it can increase the power of a statistical test, 
meaning that if you are right about the direction of the true difference, you will more likely
detect a significant difference with a one-sided test than with a two-sided test. Power is the probability that your test will reject the null hypothesis
when the null hypothesis is false, or the probability that you will detect a difference when a 
difference actually exists.;

proc ttest data=statdata.testscores 
           plots = interval h0=0 sides=U;
   class Gender;
   var SATScore;
   title "One-Sided t-Test Comparing Girls to Boys";
run;
title;

proc ttest data=statdata.testscores 
           plots(shownull)=interval h0=0 sides=U;
   class Gender;
   var SATScore;
   title "One-Sided t-Test Comparing Girls to Boys";
run;
title;

proc ttest data = statdata.german plot(shownull) = interval h0 = 0 sides = L;
class Group;
Var Change;
run;

proc ttest data=statdata.german plots(shownull)=interval 
           h0=0 sides=L;
   class Group;
   var Change;
   title 'German Training, Comparing Treatment to Control';   
   title2 'One-Sided t-Test';
run;
title;

/*ANOVA

ANOVA can be thought of as linear regression on dummy variables. It is only in the 
interpretation of the model that a distinction is made.

You calculate the variability between the means and the variability of observations 
within each group, and then calculate a ratio between these two measurements. If the 
between-group variability is significantly larger than the within-group variability, 
you reject the null that all of the group means are equal. So, you partition out the 
variability using sums of squares. For ANOVA, you calculate three types of sums of 
squares: Between Group Variation (model sums of squares or SSM, Within Group Variation, 
and Total Variation

The first assumption is one of independent observations
The second assumption is that the error terms are normally distributed. You verify 
this assumption by examining diagnostic plots of the residuals.
The third assumption is that the error terms have equal variances across treatments.*/


proc print data=statdata.mggarlic (obs=10);
   title "Partial Listing of Garlic Data";
run;

proc means data=statdata.mggarlic printalltypes maxdec=3;
    var BulbWt;
    class Fertilizer;
    title "Descriptive Statistics of Garlic Weight";
run;

ods graphics on / width=700;
proc sgplot data=statdata.mggarlic;
    vbox BulbWt / category=Fertilizer connect= mean datalabel=BedID;
    format BedID 5.;
    title "Box Plots of Garlic Weight";
run;
title;

*The groups are not equally sized -> unbalanced;

ods graphics on / width=700;

proc glm data=statdata.mggarlic plots(only)=diagnostics(unpack) ;
   class Fertilizer;
   model BulbWt=Fertilizer;
   means Fertilizer / hovtest;
   title "Testing for Equality of Means with PROC GLM";
run;
quit;
title;

*Interpretation of results

you want to see a random scatter of residuals above and below 0 for 
the four fertilizer groups. The plot looks good. 

You use Levene's Test for Homogeneity to formally test the equal variance
assumption. Because the p-value of 0.4173 is greater than 0.05, you fail to
reject the null and conclude that the variances are equal. This is good. 
You verified the equal variance assumption.

Because the residuals follow the diagonal reference line fairly closely,
you can say that they are approximately normal. 

SAS calculates this by dividing the model sum of squares by the model Degrees 
of Freedom, which gives you the average sum of squares for the model. The mean
square error is 0.00078, which is an estimate of the population variance. SAS 
calculates this by dividing the error sum of squares by the error Degrees of 
Freedom, which gives you the average sum of squares for the error. SAS calculates 
the F-statistic by dividing the MSM by the MSE. The F statistic is 1.96. Because 
the corresponding p-value of .1432 is greater than 0.05, you can conclude that
there is not a statistically significant difference between the mean bulb weights
for the four fertilizers.

It's important for you to realize that the one-way ANOVA is an omnibus test 
statistic and cannot tell you which specific groups are significantly different 
from each other, only that at least two groups are different. To determine which 
specific groups differ from each other, you need to use a post-hoc test. 

All in all, the PROC GLM output supports your conclusion that there's not a 
statistically significant difference between the mean bulb weights for the four 
fertilizers. (F-test);

*Practice;
proc glm data=statdata.ads plots(only)=diagnostics(unpack);
   class Ad;
   model Sales=Ad;
   means Ad / hovtest;
   title 'Testing for Equality of Ad Type on Sales';
run;
quit;
title;

/*
 ANOVA with Data from a Randomized Block Design
 
 Along with the three original ANOVA assumptions of independent observations, normally 
 distributed errors, and equal variances across treatments, you make two more assumptions
 when you include a blocking factor in the model. First, you assume that the treatments 
 are randomly assigned within each block. In the T-cell count example, this means that you 
 assume the three medications are randomly assigned to each of the three age groups. 
 Next, you assume that the effects of the treatment factor are constant across the levels 
 of the blocking factor, meaning that the effects of the treatment factor don't depend on 
 the block they are in. When the effects of the treatment factor are not constant across 
 the levels of another variable, it's called interaction. But when you use a randomized 
 block design, you assume that the effects are the same within each block. In other words, 
 you assume that there are no interactions with the blocking variable
 
 The farmers divide the farm into eight sectors, each of which has four beds, and in each of
 the four beds, they randomly assign each of the four fertilizers. An experimental design 
 like this is often referred to as a randomized block design. As you can see in this ANOVA 
 model, Sector is the blocking variable.
 */

proc print data=statdata.mggarlic_block (obs=10);
run;

proc glm data=statdata.mggarlic_block plots(only)=diagnostics(unpack);
     class Fertilizer Sector;
     model BulbWt=Fertilizer Sector;
     title "ANOVA for Randomized Block Design";
run;
quit;
title;

*If you compare this MSE, which is 0.00039, to the MSE in the model that included Fertilizer
only, 0.00077966, you see that it decreased. R-square for this model is much greater than 
that in the previous model without the blocking factor: 0.736 versus 0.173. 


The Type III SS test at the bottom of the output tests for differences due to each variable, 
controlling for or adjusting for the other variable. How about the blocking variable? Again 
you might ask: did it help the model? The rule of thumb that most statisticians use is that 
if the F-value is greater than 1, then it helped to add the blocking factor to your model. 
Because this F-value of 6.53 is greater than 1, adding Sector as a blocking factor helped to
decrease the unexplained variability of the response, bulb weight.

You determined from the randomized block design that one of the fertilizer types is different
from the rest because your p-value for Fertilizer was significant.;

proc glm data=statdata.ads1 plots(only)=diagnostics(unpack);
   class Ad Area;
   model Sales=Ad Area;
   title 'ANOVA for Randomized Block Design';
run;
quit;
title;

/*
 * If the spread of the fitted-value distribution is large compared with the spread of 
 * the residual distribution, then the [explanatory]variable is influential.
 * 
 * To determine which means differ from other means, your next step is to conduct ANOVA 
 * post hoc tests and control the error rate using a multiple comparison method.
 */

proc sgscatter data=statdata.ameshousing3;
   plot SalePrice*Gr_Liv_Area / reg;
   title "Associations of Above Grade Living Area with Sale Price";
run;

%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
         Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom;
 
options nolabel;

proc sgscatter data=statdata.ameshousing3;
   plot SalePrice*(&interval) / reg ;
   title "Associations of Interval Variables with Sale Price";
run;
title;

/*****************************************************************************/

proc print data=statdata.fitness;
run;

proc corr data=statdata.fitness rank
     plots(only)=scatter(nvar=all ellipse=none);
   var RunTime Age Weight Run_Pulse
       Rest_Pulse Maximum_Pulse Performance;
   with Oxygen_Consumption;
   title "Correlations and Scatter Plots with Oxygen_Consumption";
run;
title;


proc corr data = statdata.fitness
    plots =matrix;
   var RunTime Age Weight Run_Pulse
       Rest_Pulse Maximum_Pulse Performance;
   with Oxygen_Consumption;
   title "Correlations and Scatter Plots with Oxygen_Consumption";
run;
title;

ods graphics on / imagemap=on;
proc corr data=statdata.fitness nosimple
     plots=matrix(nvar=all histogram); 
   var RunTime Age Weight Run_Pulse
       Rest_Pulse Maximum_Pulse Performance;
   id name;
   title "Correlation Matrix and Scatter Plot Matrix of Fitness Predictors";
run;
title;

* table of Pearson correlation statistics;

ods graphics on / imagemap=off;
proc corr data=statdata.fitness 
     plots=matrix(nvar=all); 
   var RunTime Age Weight Run_Pulse
       Rest_Pulse Maximum_Pulse Performance;
   id name;
   title "Correlation Matrix and Scatter Plot Matrix of Fitness Predictors";
run;
title;

/*Practice*/

Proc UNIVARIATE data = statdata.BodyFat2;
Var Age Weight Height Neck Chest Abdomen Hip Thigh Knee Ankle Biceps Forearm Wrist;
 id case;
 histogram / normal (mu=est sigma=est);
 probplot / normal (mu=est sigma=est);
 inset skewness kurtosis;
title "Predictors of % Body Fat";
run;
title;
run;

data statdata.bodyfat2;
   set statdata.bodyfat;
   if Height < 30 then Height=Height + 40;
run;

proc corr data = statdata.bodyfat2 rank
 plot(only) = scatter(nvar=all ellipse=none);
 var Age Weight Height Neck Chest Abdomen Hip Thigh Knee Ankle Biceps Forearm Wrist;
 with PctBodyFat2;
 title "Correlations and Scatter Plots with Body Fat %";
run;
title;

ods graphics on / imagemap=off;

proc corr data=statdata.bodyfat2 nosimple
     plots=matrix(nvar=all histogram);
   var Age Weight Height;
   title "Correlations and Scatter Plot Matrix of Basic Measures";
run;

proc corr data=statdata.bodyfat2 nosimple;
   var Neck Chest Abdomen Hip Thigh
       Knee Ankle Biceps Forearm Wrist;
   title "Correlations of Circumferences";
run;

proc corr data=statdata.bodyfat2 nosimple
     plots=matrix ;
   var Neck Chest Abdomen Hip Thigh
       Knee Ankle Biceps Forearm Wrist;
   with Age Weight Height;
   title "Correlations and Scatter Plot Matrix between";
   title2 "Basic Measures and Circumferences";
run;

title;
ods graphics off;
 
/****************************************************************************/
/*Post Hoc Test*/

ods graphics on / width=700;
*Recall;
ods select lsmeans diff meanplot diffplot controlplot;

proc sgplot data=statdata.mggarlic;
    vbox BulbWt / category=Fertilizer connect= mean datalabel=BedID;
    format BedID 5.;
    title "Box Plots of Garlic Weight";
run;
title;

*The groups are not equally sized -> unbalanced;

ods graphics on / width=700;

proc glm data=statdata.mggarlic plots(only)=diagnostics(unpack) ;
   class Fertilizer;
   model BulbWt=Fertilizer;
   means Fertilizer / hovtest;
   title "Testing for Equality of Means with PROC GLM";
run;
quit;
title;

proc glm data=statdata.mggarlic_block;
    class Fertilizer Sector;
    model BulbWt=Fertilizer Sector;
    lsmeans Fertilizer / pdiff=all adjust=tukey;
    lsmeans Fertilizer / pdiff=controlu('4') adjust=dunnett;
    lsmeans Fertilizer / pdiff=all adjust= t;
    * t-test p value much smaller not reliable; 
    title "Garlic Data: Multiple Comparisons";
run;
quit;
title;

proc glm data=statdata.ads1;
   class Ad Area;
   model Sales=Ad Area;
    lsmeans Ad/ pdiff=all adjust=tukey;
    lsmeans Ad / pdiff=controlu('display') adjust=dunnett;
    lsmeans Ad / pdiff=all adjust=t;
   title 'Pairwise Differences for Ad Types on Sales';
run;
quit;
title;

ods trace off;

/*
Two-way ANOVA

A | B | C @2 would result in only those effects that contain two or fewer variables: 
in this case, A B A*B C A*C and B*C.
*/

proc format;
   value dosef
   1="Placebo"
   2="100 mg"
   3="200mg"
   4="500mg";
run;

proc means data=statdata.drug
           mean var std printalltypes;
   class Disease DrugDose;
   var BloodP;
   output out=means mean=BloodP_Mean;
   format DrugDose dosef.;
   title "Selected Descriptive Statistics for Drug Data Set";
run;
title;

*The Means data set contains the variable _TYPE_, with values ranging from 0 to 3 to 
represent the four tables this PROC MEANS program generates. Type 0 gives you the 
mean blood pressure change of all observations, regardless of disease type or drug dose.
Type 1 gives you the mean blood pressure for each drug dose, regardless of disease type. 
Type 2 gives you the mean blood pressure for each disease type, regardless of drug dose.
And Type 3 gives you the mean blood pressure for each disease type and drug dose combination.;

ods graphics on / width=800;

proc sgplot data=means;
   where _TYPE_=3;     
   scatter x=DrugDose y=BloodP_Mean / 
           group=Disease markerattrs=(size=10);
   series x=DrugDose y=BloodP_Mean / group=Disease        
          lineattrs=(thickness=2);
   xaxis integer;
   format DrugDose dosef.;  
   title "Plot of Stratified Means in Drug Data Set";
run;
title;

ods graphics on / width=800;

proc glm data=statdata.drug;
   class DrugDose Disease;
   model Bloodp=DrugDose Disease DrugDose*Disease;
   format DrugDose dosef.;  
   title "Analyze the Effects of DrugDose and Disease";
   title2 "Including Interaction";
run;
quit;
title;

*When you check the log, you see that it looks fine. No errors or warnings are present. 
Now you can examine the output. The p-value for the overall model is very small, so what 
does this tell you? You can reject the null hypothesis and conclude that at least one of 
the effects in the model is significant, in other words, there is at least one difference 
among the 12 group means, one for each drug dose and disease combination. Which factors 
explain this difference? You'll see in just a few moments. 

The R square is 0.3479, so approximately 35% of the variation in blood pressure change 
can be explained by the model. The average blood pressure change of all the observations 
is –2.294, which is exactly what the PROC MEANS output showed. 

The next tables show the breakdown of the main effects and interaction term in the model.
Look at the Type I and Type III Sums of Squares values. Do you know why their values are 
not exactly the same? You don't have a balanced design in this experiment. In other words, 
you have a different number of observations in each drug dose and disease combination group. 
In most situations, you will want to use the Type III SS. The Type I, or sequential SS, are
the sums of squares you obtain from fitting the effects in the order you specify in the model. 
The Type III, or marginal SS, are the sums of squares you obtain from fitting each effect after
all the other terms in the model, that is, the sums of squares for each effect corrected for 
the other terms in the model. Type III SS does not depend upon the order you specify effects 
in the model.;

ods graphics on / width=800;
ods select meanplot lsmeans slicedanova;  

proc glm data=statdata.drug;
   class DrugDose Disease;
   model Bloodp=DrugDose Disease DrugDose*Disease;
   lsmeans DrugDose*Disease / slice=Disease; 
   format DrugDose dosef.;
   title "Analyze the Effects of DrugDose";
   title2 "at Each Level of Disease";
run;
quit;
title;

*You add the LSMEANS statement to request the least squares mean for each unique DrugDose and 
Disease combination. You add the SLICE option to test the effect of DrugDose within each Disease.

SAS creates two types of mean plots when you use the LSMEANS statement with an interaction term.
The first plot simply displays the least squares mean for every effect level. SAS plots each 
effect level on the horizontal axis and the LSMean of blood pressure on the vertical axis. 

In this second plot, you can basically see what you've seen earlier. You can look a little 
closer at the combination levels if you want. You can see that the greatest increase in blood 
pressure change is at the drug dosage level of 200mg for patients with disease B, and that the 
greatest decrease in blood pressure change is at the drug dosage level of 200mg for patients 
with disease A. 

Based on these results, what treatment plan would you recommend to patients? It seems that you 
would want to aggressively treat blood pressure in patients with disease A with high dosages 
of the drug to decrease blood pressure. For those patients with disease B, perhaps a disease 
caused by a traumatic event, you might not want to use the drug at all because it appears to 
increase blood pressure. For those patients with disease C, you might want to look into an 
alternative drug because this drug doesn't appear to have any effect on blood pressure;

/*Practice*/

proc means data=statdata.concrete mean var std printalltypes;
   class Brand Additive;
   var Strength;
   output out=means mean=Strength_Mean;
   title 'Selected Descriptive Statistics for Concrete Data Set';
run;

proc sgplot data=means;
   where _TYPE_=3;
   scatter x=Additive y=Strength_Mean / group=Brand 
           markerattrs=(size=10);
   xaxis integer;
   title 'Plot of Stratified Means in Concrete Data Set';
run;
title;

ods graphics on / width=800;

proc glm data = statdata.concrete;
   class Brand Additive;
   model Strength = Brand Additive;
   lsmeans Additive;  
   title 'Analyze the Effects of Additive and Brand';
   title2 'on Concrete Strength without Interaction';
run;
quit;
title;

/*Store Statement /label<option 'labelname'> and Proc PLM

The use of item stores and PROC PLM enables you to separate common post-processing tasks,
such as testing for treatment differences and predicting new observations under a fitted 
model, from the process of model building and fitting. A numerically expensive model fitting 
technique can be applied once to produce a source item store. The PLM procedure can then be 
called multiple times, and the results of the fitted model are analyzed without incurring the 
model fitting expenditure again.

Selected PROC PLM option:

RESTORE	specifies the source item store for processing.
Selected PROC PLM procedure statements:
EFFECTPLOT produces a display of the fitted model and provides options for changing and 
enhancing the displays.
LSMEANS computes and compares least squares means (LS-means) of fixed effects.
LSMESTIMATE	provides custom hypothesis tests among least squares means.
SHOW uses the Output Delivery System to display contents of the item store. 
This statement is useful for verifying that the contents of the item store apply to the 
analysis and for generating ODS tables.
SLICE provides a general mechanism for performing a partitioned analysis of the LS-means 
for an interaction. This analysis is also known as an analysis of simple effects. 
The SLICE statement uses the same options as the LSMEANS statement.
WHERE is used in the PLM procedure when the item store contains BY-variable information 
and you want to apply the PROC PLM statements to only a subset of the BY groups.
*/

proc print data = statdata.ameshousing3;
run;

ods graphics on;
proc glm data=statdata.ameshousing3 
         order=internal 
         plots(only)=intplot;
   class Season_Sold Heating_QC;
   model SalePrice=Heating_QC | Season_Sold;
   lsmeans Heating_QC*Season_Sold / diff slice=Heating_QC;
   *lsmeans Season_Sold*Heating_QC / diff slice=Season_Sold;
   format Season_Sold Season.;
   store out=interact;
   title "Model with Heating Quality and Season as Interacting Predictors";
run;
title;

proc plm restore=interact plots=all;
   slice Heating_QC*Season_Sold / sliceby=Heating_QC adjust=tukey;
   effectplot interaction(sliceby=Heating_QC) / clm;
run;

*Recall that the LSMEANS option computes and compares least squares means of fixed effects. 
This LSMEANS statement specifies the interaction term Heating_QC by Season_Sold. By specifying 
slice=Heating_QC, we tell SAS to to slice the interaction effect by the different levels of 
Heating_QC.;

/*Practice*/

proc sgplot data = Statdata.Drug;
 vline DrugDose / group=Disease 
                    stat=mean 
                    response=BloodP 
                    markers;
   format DrugDose dosefmt.;
run;

ods graphics on;

proc glm data = Statdata.Drug plots(only)=intplot;
 class DrugDose Disease;
   model BloodP=DrugDose|Disease;
   lsmeans DrugDose*Disease / slice=Disease;
run;
quit;

/*
 
Linear Regression

A linear function of the Xs accurately models the mean of the Ys.
The errors are normally distributed with a mean of 0.
The errors have constant variance.
The errors are independent.
*/

ods graphics on;

proc reg data=statdata.ameshousing3 ;
    model SalePrice=Basement_Area Lot_Area;
    title "Model with Basement Area and Lot Area";
run;
quit;

proc glm data=statdata.ameshousing3 
         plots(only)=(contourfit);
    model SalePrice=Basement_Area Lot_Area;
    store out=multiple;
    title "Model with Basement Area and Gross Living Area";
run;
quit;

proc plm restore=multiple plots=all;
    effectplot contour (y=Basement_Area x=Lot_Area);
    effectplot slicefit(x=Lot_Area sliceby=Basement_Area=250 to 1000 by 250);
run; 

title;

/*The STORE statement applies to the following SAS/STAT procedures: GENMOD, GLIMMIX, GLM, GLMSELECT,
 LOGISTIC, MIXED, ORTHOREG, PHREG, PROBIT, SURVEYLOGISTIC, SURVEYPHREG, and SURVEYREG. This statement
 requests that the procedure save the context and results of the statistical analysis into an item store.
 An item store is a binary file format that cannot be modified by the user. The contents of the item
 store can be processed with the PLM procedure.

One example of item-store use is to perform a time-consuming analysis and to store its results by using
 the STORE statement. At a later time, you can then perform specific statistical analysis tasks based on
 the saved results of the previous analysis, without having to fit the model again.

In the STORE statement:

item-store-name is a usual one- or two-level SAS name, similar to the names that are used for SAS data
 sets. If you specify a one-level name, then the item store resides in the Work library and is deleted
 at the end of the SAS session. Because item stores usually are used to perform postprocessing tasks, 
 typical usage specifies a two-level name of the form libname.membername.
 label identifies the estimate on the output. A label is optional but must be enclosed in quotation marks.

/*Practice*/

proc reg data = statdata.bodyfat2;
model PctBodyFat2 = Age Weight Height Neck Chest Abdomen Hip Thigh Knee Ankle Biceps Forearm Wrist;
run;
quit;

proc glm data = statdata.bodyfat2;
model PctBodyFat2 = Age Weight Height Neck Chest Abdomen Hip Thigh Ankle Biceps Forearm Wrist;
store out=multiple;
run;

/*
In the all-possible regressions method, SAS computes all possible models and ranks the 
results. Then, to evaluate the models, you compare statistics side by side.These statistics 
include the familiar R2 and adjusted R2 values, along with a new statistic, the Cp statistic. 

The second category is stepwise selection methods. Here you choose a selection method—stepwise,
forward, or backward—and SAS constructs a model based on that method. 
*/


************************************************************************;
%let path=/folders/myshortcuts/_myfolders/ECST131;
libname statdata "&path";

proc reg data=statdata.fitness;
   model Oxygen_Consumption = RunTime / cli clm;
   id name RunTime;
   title 'Predicting Oxygen_Consumption from RunTime';
run;
quit;
title;

/*
The Model Sum of Squares is 633.01. This is the amount of variability that the model explains.
The Error Sum of Squares is 218.54. This is the amount of variability that the model does not explain.
The Total Sum of Squares is 851.55, which is the total amount of variability in the response.
The Mean Square column indicates the ratio of the sum of squares and the degrees of freedom.
The mean square model is 633.01. This is calculated by dividing the model sum of squares by the model DF,
which gives us the average sum of squares for the model.
The mean square error is 7.54, which is an estimate of the population variance. This is calculated 
by dividing the error sum of squares by the error DF, which gives us the average sum of squares for
the error.

The Root MSE is 2.75. This is the square root of the mean square error in the Analysis of Variance table. 
The Root MSE is a measure of the standard deviation of Oxygen_Consumption at each value of RunTime.
The Dependent Mean is 47.38, which is the average of Oxygen_Consumption for all 31 subjects.
The Coefficient of Variation is 5.79. This is the size of the standard deviation relative to the mean.
The R-square value is .743, which is calculated by dividing the mean square for the model by the total 
sum of squares. The R-square value is between 0 and 1 and measures the proportion of variation 
observed in the response that the regression line explains.

Mean Square Between and Mean Square Within are used to calculate the F-ratio: 

How do you interpret a 95% prediction interval (which is not equal to confidence inteval)? 
If you create a 95% prediction interval, the interpretation is that you are 95% confident that 
your interval contains the new observation. 

For a given set of data, why is a prediction interval wider than a confidence interval? 
A prediction interval is wider than a confidence interval because single observations have 
more variability than sample means.

The difference between a prediction interval and a confidence interval is the standard error.

The standard error for a confidence interval on the mean takes into account the uncertainty 
due to sampling. The line you computed from your sample will be different from the line that 
would have been computed if you had the entire population, the standard error takes this 
uncertainty into account.

The standard error for a prediction interval on an individual observation takes into account 
the uncertainty due to sampling like above, but also takes into account the variability of the 
individuals around the predicted mean. The standard error for the prediction interval will be 
wider than for the confidence interval and hence the prediction interval will be wider than 
the confidence interval.
*/

data need_predictions;
   input RunTime @@;
   datalines;
9 10 11 12 13
;
run;

data predoxy;
   set need_predictions 
       statdata.fitness;
run;

proc reg data=predoxy;
   model Oxygen_Consumption=RunTime / p;
   id RunTime;
   title 'Oxygen_Consumption=RunTime with Predicted Values';
run;
quit;
title;

*Here's an important reminder. When you use a model to predict future values of the response 
variable given certain values of the predictor variable, you must stay within the range of 
values for the predictor variable used to create the model. For example, in the original 
Fitness data set, values of RunTime range from a little over 8 minutes to a little over 14 
minutes. Based on that data, you shouldn't try to predict what Oxygen_Consumption would be 
for a RunTime value outside that range. The relationship between the predictor variable and 
the response variable might be different beyond the range of the data.;

proc reg data=statdata.fitness outest=estimates; 
   model Oxygen_Consumption=RunTime;
run;
quit;
 
proc print data=estimates;
   title "OUTEST= Data Set from PROC REG";
run;
title;

proc score data=need_predictions score=estimates
     out=scored type=parms; 
   var RunTime; 
run;
 
proc print data=Scored;
   title "Scored New Observations";
run;

title;

/*Practice*/

proc reg data = statdata.bodyfat2 outest = estimates;
model PctBodyFat2 = Weight / clm cli;
run;
quit;

proc score data = predictions score = estimates out = scored type= parms;
var PctBodyFat2;
title "Scored New Observations";
run; 

/*To select the best model for prediction, meaning that you want the most accurate model for
predicting future values of Y, you should use Mallows' criterion for Cp. Using this criterion, 
you look for models where Cp is less than or equal to p, which is the number of parameters in 
the model, including the intercept. A good candidate model would be the smallest model where 
Cp is less than or equal to p. 

To select the best model for parameter estimation, you should use Hocking's criterion for Cp. 
For parameter estimation, Hocking recommends a model where Cp<=2p – pfull +1, where p is the 
number of parameters in the model, including the intercept.*/

libname statdata "/folders/myfolders/ECST131"; 
libname library "/folders/myfolders/ECST131";

ods graphics / imagemap=on;
 
proc reg data=statdata.fitness plots(only)=(cp);
   ALL_REG: model oxygen_consumption = 
                  Performance RunTime Age Weight
                  Run_Pulse Rest_Pulse Maximum_Pulse
                  / selection=cp adjrsq rsquare best=20;
title 'Best Models Using All-Regression Option';
run;
quit;
title;

proc reg data=statdata.fitness;
   PREDICT_Mallows: model Oxygen_Consumption 
                  = RunTime Age Run_Pulse Maximum_Pulse; 
   EXPLAIN_Hocking: model Oxygen_Consumption 
                  = RunTime Age Weight Run_Pulse Maximum_Pulse; 
   title 'Check "Best" Two Candidate Models';
run;
quit;
title;


proc reg data = statdata.bodyfat2 plot(only) = (cp);
  ALL_PctBodyFat2: model PctBodyFat2 = Age Weight Height Neck Chest Abdomen
                   Hip Thigh Knee Ankle Biceps Forearm Wrist 
                   / selection=cp adjrsq rsquare best=60;
run;
quit;


/*
 * Step-wise Selection Method
 */

*Forward selection
Backward selection
Stepwise selection

There is no one method that is best. In addition, you need to be cautious when
reporting statistical quantities produced by these methods. Using automated model
selection results in biases in parameter estimates, predictions, and standard errors,
incorrect calculation of degrees of freedom, and p-values that tend to err on the side 
of overestimating significance. 

So, how can you avoid these issues? One way is to hold out some of your data in order 
to perform an honest assessment of how well your model performs on a different sample of 
data than you used to develop the model. You split your data into two data sets: the 
training data and the holdout data, which is also called the validation data. You use the 
training data to build your model, and you use the holdout data to assess and compare 

One last thing to keep in mind is that the stepwise techniques don’t take any any collinearity
in your model into account. Collinearity means that predictor variables in the same model are 
highly correlated. If collinearity is present in your model, you might want to consider first 
reducing the collinearity as much as possible and then running stepwise methods on the remaining 
variables.
;

libname statdata "/folders/myfolders/ECST131"; 
libname library "/folders/myfolders/ECST131";

%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
              Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom ;

ods graphics on;

proc glmselect data=statdata.ameshousing3 plots=all;
   STEPWISE: model SalePrice=&interval / selection=stepwise 
                   details=steps select=SL slstay=0.05 slentry=0.05 showpvalues;
   title "Stepwise Model Selection for SalePrice - SL 0.05";
run;
title;

*Omit Selection option SAS chooses stepwise selection, SL = significance level;

/*Optional code that will execute forward and backward selection, each with slentry and slstay = 0.05.
proc glmselect data=statdata.ameshousing3 plots=all;
   FORWARD: model SalePrice=&interval / selection=forward details=steps select=SL slentry=0.05;
   title "Forward Model Selection for SalePrice - SL 0.05";
run;

proc glmselect data=statdata.ameshousing3 plots=all;
   BACKWARD: model SalePrice=&interval / selection=backward details=steps select=SL slstay=0.05;
   title "Backward Model Selection for SalePrice - SL 0.05";
run;
title;
*/

proc glmselect data = statdata.bodyfat2 plots=all;
   STEPWISE: model PctBodyFat2 = Age Weight Height Neck 
   Chest Abdomen Hip Thigh Knee Ankle Biceps Forearm Wrist 
   / selection = stepwise SELECT=SL showpvalues;
 title 'SL STEPWISE Selection with PctBodyFat2';
run;
quit;
title;

proc glmselect data = statdata.bodyfat2 plots=all;
   FORWARD: model PctBodyFat2 = Age Weight Height Neck 
   Chest Abdomen Hip Thigh Knee Ankle Biceps Forearm Wrist 
   / selection = FORWARD SELECT=SL SLENTRY=0.05 showpvalues;
 title 'SL FORWARD Selection with PctBodyFat2';
run;
quit;
title;

*Each information criterion searches for a model that minimizes the unexplained 
variability with as few effects in the model as possible. In other words, they 
search for the most parsimonious model.The calculation for each information 
criterion begins with nlog(SSE/n). It then invokes a penalty representing the 
complexity of the model. 

Information Criterion	Penalty Component
AIC	                     2p + n + 2
AICC	                 [n(n + p)] / (n - p - 2)
BIC	                     2(p + 2)q - 2q^2, where q = (nσ-hat2)/SSE. 
SBC	                     plog(n)
;

libname statdata "/folders/myfolders/ECST131"; 
libname library "/folders/myfolders/ECST131";

%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
              Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom ;

ods graphics on;
proc glmselect data=statdata.ameshousing3 plots=all;
   STEPWISEAIC: model SalePrice = &interval / selection=stepwise details=steps select=AIC SHOWPVALUES;
   title "Stepwise Model Selection for SalePrice - AIC";
run;

proc glmselect data=statdata.ameshousing3 plots=all;
   STEPWISEBIC: model SalePrice = &interval / selection=stepwise details=steps select=BIC SHOWPVALUES;
   title "Stepwise Model Selection for SalePrice - BIC";
run;

proc glmselect data=statdata.ameshousing3 plots=all;
   STEPWISEAICC: model SalePrice = &interval / selection=stepwise details=steps select=AICC SHOWPVALUES;
   title "Stepwise Model Selection for SalePrice - AICC";
run;

proc glmselect data=statdata.ameshousing3 plots=all;
   STEPWISESBC: model SalePrice = &interval / selection=stepwise details=steps select=SBC SHOWPVALUES;
   title "Stepwise Model Selection for SalePrice - SBC";
run;
title;


/*
 * Residual plots and other diagnostic plots. 
 */

ods graphics / imagemap=on;

proc reg data=statdata.fitness; 
   PREDICT: model Oxygen_Consumption =
                  RunTime Age Run_Pulse Maximum_Pulse; 
   id Name; 
   title 'PREDICT Model - Plots 4of Diagnostic Statistics';
run;
quit;

title;

ods graphics / imagemap=on width=800;

proc reg data=statdata.fitness
         plots(only)=(QQ RESIDUALBYPREDICTED RESIDUALS); 
   PREDICT: model Oxygen_Consumption =
                  RunTime Age Run_Pulse Maximum_Pulse; 
   id Name; 
   title 'PREDICT Model - Plots of Diagnostic Statistics';
run;
quit;
title;

*STUDENT residuals are calculated by dividing the residuals by their standard errors, 
so you can think of each STUDENT residual as roughly equivalent to a z-score. 
Typically, z-scores is considered to be large if their absolute value is greater than 2. 
But for large sample size, a larger cutoff value of z-score (eg.3) may be appropriate.

The Cook's D statistic is calculated as if that observation weren't in the data set. The 
Cook's D statistic measures the distance between the set of parameter estimates with that 
observation deleted from your regression analysis and the set of parameter estimates with 
all the observations in your regression analysis. 

If any observation has a Cook's D statistic greater than 4 divided by n, where n is the 
sample size, that observation is influential.

RSTUDENT residuals are similar to STUDENT residuals. Remember that STUDENT residuals are the 
residuals divided by their standard errors. For each observation, the RSTUDENT residual is 
the residual divided by the standard error estimated with the current observation deleted. 

First, if the RSTUDENT residual is different from the STUDENT residual, the observation is 
probably influential. Second, if the absolute value of the RSTUDENT residuals is greater than 
2 or 3, you've probably detected an influential observation

DFFITS measures the impact that each observation has on its own predicted value. For each
observation, DFFITS is calculated using two predicted values. The first predicted value is 
calculated from a model using the entire data set to estimate model parameters. The second 
predicted value is calculated from a model using the data set with that particular observation 
removed to estimate model parameters. The difference between the two predicted values is divided 
by the standard error of the predicted value, without the observation. If the standardized 
difference between these predicted values is large, that particular observation has a large 
effect on the model fit. The rule of thumb for DFFITS has two versions. The general cutoff value is 2.
The more precise cutoff is 2 times the square root of p divided by n, where p is the number of 
terms in the model, including the intercept, and n is the sample size. DFFITS is most 
useful for predictive models.

;

libname statdata "/folders/myfolders/ECST131"; 
libname library "/folders/myfolders/ECST131";

%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
              Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom ;

ods select none;
proc glmselect data=statdata.ameshousing3 plots=all;
   STEPWISE: model SalePrice = &interval / selection=stepwise 
                                details=steps select=SL slentry=0.05 slstay=0.05;
   title "Stepwise Model Selection for SalePrice - SL 0.05";
run;
quit;
ods select all;

ods graphics on;
ods output RSTUDENTBYPREDICTED=Rstud 
           COOKSDPLOT=Cook
           DFFITSPLOT=Dffits 
           DFBETASPANEL=Dfbs;
proc reg data=statdata.ameshousing3 
         plots(only label)=
              (RSTUDENTBYPREDICTED 
               COOKSD 
               DFFITS 
               DFBETAS);
   SigLimit: model SalePrice = &_GLSIND;
   *The macro variable stores the list of effects that are in the
    model that PROC GLMSELECT selects;
   title 'SigLimit Model - Plots of Diagnostic Statistics';

run;
quit;
title;


 /* Before running the code below,*/
 /* run the code from the previous demo, 
 /*Looking for Influential Observations, Part 1.*/
 /* Run both programs in the same SAS session.*/

title;
proc print data=Rstud;
run;

proc print data=Cook;
run;

proc print data=Dffits;
run;

proc print data=Dfbs;
run;

data Dfbs01;
   set Dfbs (obs=300);
run;

data Dfbs02;
   set Dfbs (firstobs=301);
run;

data Dfbs2;
   update Dfbs01 Dfbs02;
   by Observation;
run;


data influential;
/*  Merge datasets from above.*/
    merge Rstud
          Cook 
          Dffits
          Dfbs2;
    by observation;

/*  Flag observations that have exceeded at least one cutpoint;*/
    if (ABS(Rstudent)>3) or (Cooksdlabel ne ' ') or Dffitsout then flag=1;
    array dfbetas{*} _dfbetasout: ;
    do i=2 to dim(dfbetas);
       if dfbetas{i} then flag=1;
    end;

/*  Set to missing values of influence statistics for those*/
/*  that have not exceeded cutpoints;*/
    if ABS(Rstudent)<=3 then RStudent=.;
    if Cooksdlabel eq ' ' then CooksD=.;

/*  Subset only observations that have been flagged.*/
    if flag=1;
    drop i flag;
run;

title;
proc print data=influential;
   id observation;
   var Rstudent CooksD Dffitsout _dfbetasout:; 
run;

/* Collinearity can hide significant effects. This is a good reason to deal 
with collinearity before using any automated model selection tool. 
Second, collinearity increases the variance (standard error) of the parameter 
estimates, which tells us how variable the corresponding parameter estimate is,
making the parameter estimates unstable and increasing the prediction 
error of the model.*/

libname statdata "/folders/myfolders/ECST131"; 
libname library "/folders/myfolders/ECST131";

proc reg data=statdata.fitness;
   PREDICT: model Oxygen_Consumption = 
                  RunTime Age Run_Pulse Maximum_Pulse;
   FULL: model Oxygen_Consumption = 
               Performance RunTime Age Weight
               Run_Pulse Rest_Pulse Maximum_Pulse; 
   title 'Collinearity: Full Model';
run;
quit;

title;

/*The output for the FULL model includes all of the predictor variables. 
At less than .0001, the p-value for the full model is highly significant. 
At .8026, the adjusted R-square value is also fairly high, indicating that the model fits 
the data well. However, the adjusted R-square value fell from .8102 in the PREDICT model 
to .8026 in the FULL model. This indicates that the additional explained variability 
is not enough to justify the three additional predictors that are included in the FULL model
When an overall model is highly significant but the individual variables don't tell the 
same story, it's a warning sign of collinearity.

VIF or variance inflation factor, measures the magnitude of collinearity in a model. Also
check out COLLIN and COLLINOINT. COLLIN includes the intercept when analyzing collinearity. 
COLLIN not only helps measure the magnitude of collinearity, but also helps identify the 
predictors that are causing the problem. COLLINOINT requests the same analysis as COLLIN 
but excludes the intercept. 

For each predictor, the VIF is 1 divided by the quantity of 1 minus R2, where R2 is 
calculated for each predictor from a model that takes that predictor and regresses it 
on all the other predictors in the model. If the VIF is greater than 10 ,the approximate 
cutoff value.for any predictors in the model, those predictors are probably involved in 
collinearity.*/

proc reg data=statdata.fitness;
   FULL: model Oxygen_Consumption = 
               Performance RunTime Age Weight
               Run_Pulse Rest_Pulse Maximum_Pulse
               / vif; 
   title 'Collinearity: Full Model with VIF';
run;
quit;

title;

proc reg data=statdata.fitness;
   NOPERF: model Oxygen_Consumption = 
                 RunTime Age Weight
                 Run_Pulse Rest_Pulse Maximum_Pulse
                 / vif; 
   title 'Dealing with Collinearity';
run;
quit;

title;

proc reg data=statdata.fitness;
   NOPERFMAX: model Oxygen_Consumption = 
                    RunTime Age Weight
                    Run_Pulse Rest_Pulse
                    / vif; 
   title 'Dealing with Collinearity';
run;
quit;

title;


