*****************************************************************
SAS ECST131 Chapter 6 
Sample code for 
Scoring Predictive Models

In this demonstration, we use code that scores the data in two 
different ways and then compares the output from the two methods. 
First, the PROC PLM step uses the SCORE statement to score the data. 
The second method generates scoring code by using the CODE statement 
in PROC PLM, and then uses a DATA step to do the scoring. For 
demonstration purposes, we'll score the validation data set from 
the previous demonstration, Statdata.AmesHousing4. However, when 
you score data in your work environment, you'll be scoring data that 
was not used in either training or validation. 

******************************************************************;

%let path=/folders/myshortcuts/_myfolders/ECST131;
libname statdata "&path";

%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
              Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom;        /*8 variables*/
%let categorical=House_Style2 Overall_Qual2 Overall_Cond2 Fireplaces 
                 Season_Sold Garage_Type_2 Foundation_2 Heating_QC 
                 Masonry_Veneer Lot_Shape_2 Central_Air;

ods graphics;

proc glmselect data=statdata.ameshousing3
               plots=all 
               valdata=statdata.ameshousing4;
   class &categorical / param=glm ref=first;
   model SalePrice=&categorical &interval / showpvalues
                  selection=backward
                  select=sbc 
                  choose=validate;    
   store out=work.amesstore;
   title "Selecting the Best Model using Honest Assessment";
run;

title;

/*Note:*/
/*In your current SAS session, you must first run the code from the previous 
demonstration (shown above) before running the code below.
Replace my-file-path with the path to your course practice files.*/

/*Method 2*/
proc plm restore=work.amesstore; /*Usually in a permanent file*/
   score data=statdata.ameshousing4 out=scored;
   code file="/folders/myshortcuts/_myfolders/scoring.sas";
run;

/*Check log*/


/*Method 3*/
data scored2; /*Scoring using data step*/
   set statdata.ameshousing4;
   %include "/folders/myshortcuts/_myfolders/scoring.sas";
run;

/*The %INCLUDE statement must specify the same location and the name of the 
SAS program file that was created in the CODE statement in PROC PLM. Remember
that, if we had made any transformations to the original data set before building
the model, we would need to perform those transformations here in the DATA step 
before the %INCLUDE statement. */

/*PROC COMPARE step compares the values of the scored variable in the two output data 
sets, Scored and Scored2. By default, the SCORE statement in PROC PLM uses the name 
Predicted for the scored variable, as shown here in the VAR statement. The WITH statement 
specifies the name of the scored variable in the Scored2 data set, which is 
P-underscore-SalePrice. As you can see, the DATA step names added P-underscore to the 
original variable name. */

proc compare base=scored compare=scored2 criterion=0.0001;
   var Predicted;
   with P_SalePrice;
run;

/*Practice Question*/

proc glmselect data=statdata.ameshousing3
               seed=8675309
               noprint;
   class &categorical / param=reference ref=first;
   model SalePrice=&categorical &interval / 
               selection=stepwise
               select=aic 
               choose=validate;
   partition fraction(validate=0.3333);
   
   score data=statdata.ameshousing4 out=score1;     /*****/
   store out=store1;
   
   title "Selecting the Best Model using Honest Assessment";
run;

proc print data = score1;
run;

proc plm restore=store1;
   score data=statdata.ameshousing4 out=score2;
run;

proc print data = scored2;
run;

proc compare base=score1 compare=score2 criterion=0.0001;
   var P_SalePrice;
   with Predicted;
run;
