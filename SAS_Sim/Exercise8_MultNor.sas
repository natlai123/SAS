/*Exercise 8.2: The numeric variables in the Sashelp.Iris data set are SepalLength, SepalWidth,
PetalLength, and PetalWidth. There are three species of flowers in the data. Use PROC CORR to
visualize these variables for the “Virginica” species. Do the data appear to be multivariate normal?
Repeat the analysis for the “Setosa” species*/

/**********************/
/* Answer to exercise */
/**********************/

proc print data = Sashelp.iris ;
run;

proc means data = sashelp.iris;
by species;
run;

proc corr data=Sashelp.iris noprob ;*plot=matrix(histogram);
   by Species;
run;

/*If you want to use ODS statistical graphics to display the multiple ellipses, you need to 
use a little trick. Because the ELLIPSE statement in PROC SGPLOT does not support a GROUP= option 
as of SAS 9.4m2, you have to reshape the data so that each group becomes a new variable. This is 
equivalent to transposing the data from a "long form" to a "wide form." From my previous blog post, 
here is one way to create six variables that represent the petal length and width variables for each 
of the three species of iris in the sashelp.iris data set:*/

/*Note taht the dataset is balanced (esach species has 50 obs)*/

data Wide;
/*   |-- PetalLength --| |--- PetalWidth ---|  */
keep L_Set L_Vers L_Virg W_Set W_Vers W_Virg;  /* names of new variables */
merge sashelp.iris(where=(Species="Setosa") 
              rename=(PetalLength=L_Set PetalWidth=W_Set))
      sashelp.iris(where=(Species="Versicolor") 
              rename=(PetalLength=L_Vers PetalWidth=W_Vers))
      sashelp.iris(where=(Species="Virginica") 
              rename=(PetalLength=L_Virg PetalWidth=W_Virg));
run;

title "95% Prediction Ellipses for Each Group";
proc sgplot data=Wide;
  scatter x=L_Set  y=W_Set  / jitter name="Set"  legendlabel="Setosa";
  scatter x=L_Virg y=W_Virg / jitter name="Virg" legendlabel="Virginica";
  scatter x=L_Vers y=W_Vers / jitter name="Vers" legendlabel="Versicolor";
  ellipse x=L_Set  y=W_Set  / lineattrs=GraphData1;
  ellipse x=L_Virg y=W_Virg / lineattrs=GraphData2;
  ellipse x=L_Vers y=W_Vers / lineattrs=GraphData3;
  keylegend "Set" "Virg" "Vers" / title="Species:";
  xaxis label="Petal Length (mm)";
  yaxis label="Petal Width (mm)";
run;

/%inc "/folders/myfolders/SAS_Macro/MULTNORM.sas";
%multnorm(data=SasHelp.iris, var=SepalWidth SepalLength, plot=mult)

/*%multnorm(data=SasHelp.irirs, var=PetalWidth PetalLength,plot=mult)

proc iml;
x = randnormal(1,100, p);
print x;
