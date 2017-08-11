/************************************************************************************
 
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

Chapter 8: Simulating Data from Basic Multivariate Distributions
 
	8.1 Overview of Simulation from Multivariate Distributions . . . .. . . 129
	8.2 The Multinomial Distribution . . . . . . . . . . .  . . . . . . . . 130
	8.3 Multivariate Normal Distributions . . . . . . . . . . . . . . . . . 133
	8.3.1 Simulating Multivariate Normal Data in SAS/IML Software . . . . . 133
	8.3.2 Simulating Multivariate Normal Data in SAS/STAT Software . . . .  136
	8.4 Generating Data from Other Multivariate Distributions . . . . . . . 137
	8.5 Mixtures of Multivariate Distributions . . .. . . . . . . . . . . . 138
	8.5.1 The Multivariate Contaminated Normal Distribution . . . . . . . . 138
	8.5.2 Mixtures of Multivariate Normal Distributions . . . . . . . . . . 140
	8.6 Conditional Multivariate Normal Distributions . . . . . . . . . . . 142
	8.7 Methods for Generating Data from Multivariate Distributions . . . . 144
	8.7.1 The Conditional Distribution Technique . . . . . . . . .  . . . . 145
	8.7.2 The Transformation Technique . . . . . . . . . . . . .. . . . . . 146
	8.8 The Cholesky Transformation . . . . . . . . . . . . . . . . . . . . 146
	8.9 The Spectral Decomposition . . . .  . . . . . . . . . . . . . . . . 150

**************************************************************************************

	
	How to simulate p variables with a prescribed correlation structure?


*********************************************************************************

	
 	8.2: The Multinomial Distribution (RANDNORMAL & SIMNORMAL)
		
	A multivariate discrete distribution related to the	“Table” distribution.
	Notice that if there are only two categories with selection probabilities p 
	and 1 - p, then the multinomial distribution is equivalent to the binomial 
	distribution with parameter p.	
		
		
**********************************************************************************/

%let N = 1000;                      /* size of each sample          */
proc iml;
	call randseed(4321);                /* set seed for RandMultinomial */
	prob = {0.5 0.2 0.3};
	X = RandMultinomial(&N, 100, prob);     /* one sample, 1000 x 3 matrix */

/* print a few results */
	c = {"black", "brown", "white"};
	first = X[1:5,];
	print first[colname=c label="First 5 Obs: Multinomial"];
	mean = mean(X);
	std = std(X);
	corr = corr(X);
	print mean[colname=c], 
      std[colname=c], 
      corr[colname=c rowname=c format=BEST5.];
	create MN from X[c=c]; append from X; close MN;
quit;

/*Notice that the sum across each row is 100 because each row is the frequency
 distribution that results from drawing 100 socks (with replacement).*/

ods graphics on;
proc kde data=MN;	
   bivar black brown / plots=ContourScatter;
run;

/*For PROC KDE, see https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_kde_sect001.htm*/


/*Figure 8.2 shows that the variables are negatively correlated with each other. 
This makes sense: There are exactly 100 items in each draw, so if you draw more 
of one color, you will have fewer of the other colors.*/


/*Exercise 8.1: A technique that is used to visualize scatter plots that suffer from overplotting is
to jitter the values by a small amount. Write a DATA step that adds a uniform random value in
[-1/2, 1/2] to each component of the multinomial data. Repeat the call to PROC KDE or use PROC
SGPLOT to visualize the jittered distribution.*/

%let N = 1000;                      /* size of each sample          */
proc iml;
	call randseed(4321);                /* set seed for RandMultinomial */
	prob = {0.5 0.2 0.3};
	X = RandMultinomial(&N, 100, prob);     /* one sample, 1000 x 3 matrix */
	U = J(1000, 3);				
	call randgen(U, "Uniform", -0.5, 0.5); 		/*Jittered distribution began*/
	J = X + U;

	c = {"black", "brown", "white"};     /* print a few results */
	first = J[1:5,];
	print first[colname=c label="First 5 Obs: Multinomial (Jittered)"];
	mean = mean(J);
	std = std(J);
	corr = corr(J);
	print mean[colname=c], 
    	  std[colname=c], 
      	  corr[colname=c rowname=c format=BEST5.];
	create MNJ from J[c=c]; append from J; close MN;
quit;

	ods graphics on;
proc kde data=MNJ;
   bivar black brown / plots=ContourScatter;
run;

	/*Fitting a regression line on black and brown*/
	ods graphics off;
proc reg data=MNJ;
   model black = brown;
   ods output ParameterEstimates=PE;
run;

data _null_;
   set PE;
   if _n_ = 1 then call symput('Int', put(estimate, BEST6.));    
   else            call symput('Slope', put(estimate, BEST6.));  
run;

proc sgplot data = MNJ;
	reg x=black y=brown;
	scatter x=black y=brown;
 	  inset "Intercept = &Int" "Slope = &Slope" / 
         border title="Parameter Estimates" position=topright;
run;

/* A side-note:
See the relationship of correlation and simple regression coefficient on 
https://stats.stackexchange.com/questions/32464/how-does-the-correlation-coefficient-differ-from-regression-slope

Consider the relationship between black and brown, 
Std_black/Std_brown = 4.7832916/3.9881208  <=>  -0.58/-0.48 */



/*************************************************************************
 			
 			
 	 Multivariate Normal Distributions
 
 You need to provide the mean and covariance matrix of the distribution.
 
 
*************************************************************************/


/* Trivariate normal */

proc iml;
/* specify the mean and covariance of the population */
	Mean = {1, 2, 3};
	Cov = {3 2 1,                  
    	   2 4 0,
       	   1 0 5};
    %let N = 1000;                               /* size of each sample */
	call randseed(4321);  
	X = RandNormal(&N, Mean, Cov);               /* 1000 x 3 matrix     */

	SampleMean = mean(X);                        /* Sample mean of each column */
	SampleCov =  cov(X);                         /* Sample covariance   */

	c = "x1":"x3";
	print (X[1:5,])[label="First 5 Obs: MV Normal"];
	print SampleMean[colname=c];
	print SampleCov[colname=c rowname=c];

	create MVN from X[colname=c];  append from X;  close MVN;
quit;

	/* Alternative Create Statement:
	create TOut from X[c={X1 X2 X3}];
	append from X;
	close TOut;*/

ods graphics on;			/* create scatter plot matrix of simulated data */
proc corr data=MVN COV plots(maxpoints=NONE)=matrix(histogram);
   var x:;
run;
							/*Goodness of fit test for Multivarate Normal*/
	
	%inc "/folders/myfolders/SAS_Macro/MULTNORM.sas";	
	%multnorm(data=MVN, var=x1 x2 x3, plot=mult)

/*For p-dimensional MVN data, the squared distances are distributed as chi-square 
with p degrees of freedom. Consequently, a plot ofthe squared distance versus quantiles 
of a chi-square distribution will fall along a straight line for data that are 
multivariate normal.*/


/*An example usnig %MULTNORMAL form http://support.sas.com/kb/24/983.html*/

 data cork;
           input n e s w @@;
           datalines;
         72 66 76 77   91 79 100 75
         60 53 66 63   56 68 47 50
         56 57 64 58   79 65 70 61
         41 29 36 38   81 80 68 58
         32 32 35 36   78 55 67 60
         30 35 34 26   46 38 37 38
         39 39 31 27   39 35 34 37
         42 43 31 25   32 30 30 32
         37 40 31 25   60 50 67 54
         33 29 27 36   35 37 48 39
         32 30 34 28   39 36 39 31
         63 45 74 63   50 34 37 40
         54 46 60 52   43 37 39 50
         47 51 52 43   48 54 57 43
         ;
         
	%inc "/folders/myfolders/SAS_Macro/MULTNORM.sas";
	%multnorm(data=cork, var=n e s w, plot=mult)  

	/***********************************************************/

/*Exercise 8.2: The numeric variables in the Sashelp.Iris data set are SepalLength, SepalWidth,
PetalLength, and PetalWidth. There are three species of flowers in the data. Use PROC CORR to
visualize these variables for the “Virginica” species. Do the data appear to be multivariate normal?
Repeat the analysis for the “Setosa” species.*/

	/* Create a TYPE=COV data set */

Data MyCov(type=COV);
	input _TYPE_ $ 1-8 _NAME_ $ 9-16 x1 x2 x3;
	datalines;
COV     x1      3 2 1
COV     x2      2 4 0
COV     x3      1 0 5
MEAN            1 2 3
run;

proc simnormal data=MyCov outsim=MVN
               nr = 1000                /* size of sample     */
               seed = 12345;            /* random number seed */
   var x1-x3;
run;

proc corr data = MVN COV plots(maxpoints=NONE)=matrix(histogram);
	var x1-x3;
run;

	%inc "/folders/myfolders/SAS_Macro/MULTNORM.sas";
	%multnorm(data=MVN, var=x1 x2 x3, plot=both)

/*See "/folders/myfolders/SAS_Sim/Exercise8_MultNor.sas" */

/********************************************************************


	 Generating Data from Other Multivariate Distributions
	 
 
• The RANDDIRICHLET function generates a random sample from a Dirichlet distribution,
	which is not used in this book.
	
• The RANDMVT function generates a random sample from a multivariate Student’s t
	distribution (Kotz and Nadarajah 2004).
	
• The RANDWISHART function, which is described in Section 10.5, generates a random
	sample from a Wishart distribution.
	
	
*********************************************************************/

	/* Simulating a Multivariate T Disbitution */

proc iml; 				
	Mean = {1, 2, 3};
	Cov = {3 2 1, 
 	      2 4 0,
 	      1 0 5};
	call randseed(4321);               
									  /*X = RandMVT(NumSamples, DF, Mean, S);*/
	X = RandMVT(500, 4, Mean, Cov);   /* 500 draws; 4 degrees of freedom */
	create TOut from X[c={X1 X2 X3}]; append from X; close TOut;
quit;
	
	/* The StdDev and range of the t-distributed data are 
 	  larger than that would be expected for normal data */

proc corr data=TOut noprob plots=matrix(histogram);
run;

/**********************/



/**************************************************************************
 
 
 		8.5: Mixtures of Multivariate Distributions
 
 	This results in a distribution with heavier tails than normality. 
 	This is also a convenient way to generate data with outliers.
 	
	The following SAS/IML statements call the RANDNORMAL function to 
	generate N1 observations from MVN(mu, Cov_matrix) and N - N1 
	observations from MVN(mu, k2*Cov_matrix), where N1 is chosen 
	randomly from a binomial distribution with parameter 1-p:
 		
 		
***************************************************************************/

/***************************************************************************

	 Create Multivariate Contaminated Normal Distribution 
	 
***************************************************************************/

proc iml;
	%let N = 500;
	mu =   {0 0 0};                            /* vector of means       */
	Cov = {10  3  -2,
   	       3  6   1,
          -2  1   2};
	k2 = 100;                                  /* contamination factor  */
	p = 0.1;                                   /* prob of contamination */

	call randseed(1);
	call randgen(N1, "Binomial", 1-p, &N); 	   /* N1 unallocated ==> scalar */
	print N1[label="Number of Noramal obs with 100*Cov_matrix"];

	X = j(&N, ncol(mu));
	X[1:N1,] = RandNormal(N1, mu, Cov);               /* uncontaminated */
	X[N1+1:&N,] = RandNormal(&N-N1, mu, k2*Cov);      /* contaminated   */
	*print x;
	create Contam from X[c=('x1':'x3')]; append from X; close Contam;
quit;

proc corr data=Contam cov plots=matrix(histogram);
   var x1-x3;
run;

proc sgplot data=Contam noautolegend;
   scatter x=x1 y=x2;
   ellipse x=x1 y=x2 / alpha=0.05;
   ellipse x=x1 y=x2 / alpha=0.1;
   ellipse x=x1 y=x2 / alpha=0.2;
   ellipse x=x1 y=x2 / alpha=0.5;
run;

	%inc "/folders/myfolders/SAS_Macro/MULTNORM.sas";
	%multnorm(data=Contam, var=x1 x2 x3, plot=mult)
	
	/*Obviously, from the QQplot, outliers exists*/

/******************************************************************************************

	
		Mixture of Multivariate Normal Distributions

	Simulate data from a mixture of k MVN distributions, each with
	MVN(mu_i, Cov_matrix_i)


******************************************************************************************/

proc iml;				/*Mixture of Trivariate Normal Distrubution 3 groups each wif 3 RVs*/
	call randseed(12345);
	pi = {0.35 0.5 0.15};                  /* mixing probs for k groups */
	NumObs = 100;                          /* total num obs to sample   */
	N = RandMultinomial(1, NumObs, pi);	 /*A Draw of sample size from the Multinomial Distribution*/
	print N;
	varNames={"x1" "x2" "x3"};		 /* 3 groups of MVN each with 3 RVs */ 
	mu =   {32   16   5,                   /* means of Group 1     */
	        30    8   4,                   /* means of Group 2     */
	        49    7   5};                  /* means of Group 3     */
	print mu;
	/* specify lower-triangular within-group covariances */
	/*    c11 c21 c31 c22 c32 c33 */
	Cov = {17  7   3  5   1   1,                /* cov of Group 1      */
    	   90 27  16  9   5   4,                /* cov of Group 2      */
    	  103 16  11  4   2   2};               /* cov of Group 3      */
	
	/* Generate mixture distribution: Sample from 
 		MVN(mu[i,], Cov[i,]) with probability pi[i] */
	
	p = ncol(pi);                     /* number of variables p=3 */
	X = j(NumObs, p);   				  /*100x3 matrix*/
	Group = j(NumObs, 1);	  		  /*100x1 vector*/ 
	b = 1;                            /* beginning index      */
	
	start Sim_MMND(p, N, b, X, Cov, mu, Group);
	do i = 1 to p;	/*p=3*/
   		e = b + N[i] - 1;                        /* ending index e ={25 50 22}  */
   		c = sqrvech(Cov[i,]);                    /* cov of group (dense) */
  		print c[label="Group Cov Matrix"];		/*The variance of Xs gets larger across group*/
  		X[b:e, ] = RandNormal(N[i], mu[i,], c);  /* i_th MVN sample      */
		Group[b:e] = i;   			/*Assign Group1 Group2 Group3 to the group index vector*/
   		b = e + 1;                    /* next group starts at this index */
	end;
	finish;
	call Sim_MMND(p, N, b, X, Cov, mu, Group);
									  /*Outputting data*/
	Y = Group || X;					  /*Horizontally joining Matrices*/
	print Y;
	create F from Y[c=("Group" || varNames)];  append from Y;  close F;
quit;			/*c=colname*/

proc sgscatter data=F;
   compare y=x2 x=(x1 x3) / group=Group markerattrs=(Size=12);
run;


/*Exercise 8.6: Encapsulate the SAS/IML statements into a function that generates N random
observations from a mixture of MVN distributions.*/ 



/********************************************************************
 
 
	Conditional Multivariate Normal Distributions

Conditional distribution is also MVN and you can compute the 
conditional mean and covariance matrix directly

*******************************************************************/

/* k conditional variables, p-k fixed variables */

proc iml;
/* Given a p-dimensional MVN distribution and p-k fixed values for
   the variables x_{k+1},...,x_p, this module returns the conditional 
   mean and covariance for first k variables, conditioned on the last p-k 
   variables. The conditional mean is returned as a column vector. */
start CondMVNMeanCov(m, S, _mu, Sigma, _v);
   mu = colvec(_mu);  v = colvec(_v);   /*The COLVEC function converts a matrix into a column vector.*/
   p = nrow(mu);      k = p - nrow(v);
 
   mu1 = mu[1:k]; 
   mu2 = mu[k+1:p]; 
   Sigma11 = Sigma[1:k, 1:k];
   Sigma12 = Sigma[1:k, k+1:p]; *Sigma21 = T(Sigma12);
   Sigma22 = Sigma[k+1:p, k+1:p];
   m = mu1 + Sigma12*solve(Sigma22, (v - mu2));			/*N(m,S)*/
   S = Sigma11 - Sigma12*solve(Sigma22, Sigma12`);		/*m=mean of the conditional dist*/
finish;													/*S=var of the conditional dist*/
mu = {1 2 3};                                /* 3D MVN example: v1 v2 vary with v3 fixed*/
Sigma = {3 2 1, 
         2 4 0,
         1 0 9};
v3 = 2;                                           /* Given the value of x3 is 2 */
run CondMVNMeanCov(m, c, mu, Sigma, v3);
print m, c;									 /*Only a bivariate normal dist. left*/
 /* Given a p-dimensional MVN distribution and p-k fixed values
   for the variables x_{k+1},...,x_p, simulate first k 
   variables conditioned on the last p-k variables. */
start CondMVN(N, mu, Sigma, v);
   run CondMVNMeanCov(m, S, mu, Sigma, v);		/*Read in N(m,S)*/
   return( RandNormal(N, m`, S) );              /* m` is row vector */
finish;		/*multivariate normal distribution: RANDNORMAL (N, Mean, Cov )*/

call randseed(1234);
N = 1000;
z = CondMVN(N, mu, Sigma, v3);   /* simulate 2D conditional distrib */
*print z;
varNames = "x1":"x2";
create mvn2 from z[c=varNames]; append from z; close mvn2;
quit;

proc univariate data = mvn2;
   histogram  / normal(sigma=est);
   cdfplot  / normal(sigma=est);		
   qqplot ;
   ods select GoodnessOfFit Histogram CDFPlot QQplot; 	
run; 

proc sgplot data=mvn2 noautolegend;
   scatter x=x1 y=x2;
   ellipse x=x1 y=x2 / alpha=0.05;
   ellipse x=x1 y=x2 / alpha=0.1;
   ellipse x=x1 y=x2 / alpha=0.2;
   ellipse x=x1 y=x2 / alpha=0.5;
run;

proc sgplot data=mvn2 noautolegend;
   scatter x=x1 y=x2 / jitter;
   ellipse x=x1 y=x2 / alpha=0.05;
   ellipse x=x1 y=x2 / alpha=0.1;
   ellipse x=x1 y=x2 / alpha=0.2;
   ellipse x=x1 y=x2 / alpha=0.5;
run;

/*The JITTER option used after scatter is used to slightly displace some of the observations. 
Without the option, some markers overlap because the data are rounded to the nearest millimeter.*/

/*Exercise 8.7: Call the CondMVNMeanCov function with the input v = {2, 3}. What is the
mean and variance of the resulting conditional distribution? Call the CondMVN function with the
same input. Simulate 1,000 observations from the conditional distribution and create a histogram of
the simulated data.*/

proc iml;
start CondMVNMeanCov(m, S, _mu, Sigma, _v);
   mu = colvec(_mu);  v = colvec(_v);   /*The COLVEC function converts a matrix into a column vector.*/
   p = nrow(mu);      k = p - nrow(v);
 
   mu1 = mu[1:k]; 
   mu2 = mu[k+1:p]; 
   Sigma11 = Sigma[1:k, 1:k];
   Sigma12 = Sigma[1:k, k+1:p]; *Sigma21 = T(Sigma12);
   Sigma22 = Sigma[k+1:p, k+1:p];
   m = mu1 + Sigma12*solve(Sigma22, (v - mu2));			/*N(m,S)*/
   S = Sigma11 - Sigma12*solve(Sigma22, Sigma12`);		/*m=mean of the conditional dist*/
finish;	
mu = {1 2 3};                                    
Sigma = {3 2 1, 
         2 4 0,
         1 0 9};
v = {2, 3};
run CondMVNMeanCov(m, c, mu, Sigma,v);
print m , c;
start CondMVN(N, mu, Sigma, v);
   run CondMVNMeanCov(m, S, mu, Sigma, v);		/*Read in N(m,S)*/
   return( RandNormal(N, m`, S) );              /* m` is row vector */
finish;
call randseed(1234);
N = 1000;
z = CondMVN(N, mu, Sigma, v);   
*print z;
varNames = "x1";
create mvn3 from z[c=varNames]; append from z; close mvn3;
quit;

/*proc sgplot data=mvn3 noautolegend;
histogram x1 ; 
density x1 / type=normal;
density x1 / type=kernel;
run;*/

/*Verify x1 is normally distribtued*/
proc univariate data = mvn3;
   histogram x1 / normal(sigma=est) ;*endpoints=0 to 6 by 0.5;	
   cdfplot x1 / normal(sigma=est);		
   qqplot x1;
   ods select GoodnessOfFit Histogram CDFPlot QQplot; 	
run; 


/********************************************************************
	
	Methods for Generating Data from Multivariate Distributions


Generating data from multivariate distributions with correlated 
components and specified marginal distributions.

The simulation literature contains three general techniques that are used frequently:

	Conditional distribution technique, 
	Transformation technique, and 
	Copula technique, which combines features of the first two techniques.

1. Generate x3 from a univariate normal distribution with mean _3 and variance Cov_33. Pass this x3 
value into the CondMVNMeanCov module to get a two-dimensional conditional mean, and covariance matrix, A.

2. Generate x2 from a univariate normal distribution with mean_2 and variance A_22. Pass this
x2 value into the CondMVNMeanCov module to get a one-dimensional conditional mean and covariance matrix.

3. Generate x1 from a univariate normal distribution with the mean and variance found in the
previous step.1. 


*********************************************************************/

/*Exercise 8.8: Write a SAS/IML function that uses the conditional technique to simulate from the
three-dimensional MVN distribution that is specified in Section 8.6. Compare the time it takes to
simulate 10,000 observations by using the conditional distribution technique with the time it 
takes to generate the same number of observations by using the RANDNORMAL function.
proc iml;*/ 

/*As expected, Rick wants to boost how fast SAS IML is. The time difference is samll 
0.05 seconds vs 0.01 second */

/* k conditional variables, p-k fixed variables */

proc iml;
start CondMVNMeanCov(m, S, _mu, Sigma, _v);
   mu = colvec(_mu);  v = colvec(_v); 
   p = nrow(mu);      k = p-nrow(v);
   mu1 = mu[1:k]; 
   mu2 = mu[k+1:p]; 
   Sigma11 = Sigma[1:k, 1:k];
   Sigma12 = Sigma[1:k, k+1:p]; 				*Sigma21 = T(Sigma12);
   Sigma22 = Sigma[k+1:p, k+1:p];
   m = mu1 + Sigma12*solve(Sigma22, (v - mu2));			/*N(m,S)*/
   S = Sigma11 - Sigma12*solve(Sigma22, Sigma12`);		/*m=mean of the conditional dist*/
finish;	

start Sim_Cont_3(mu, Sigma, x_f, x2, x3, N);		/*The required function*/	
	do i =1 to N;
		y=x3[i];								/*Input the x3 generated*/
		run CondMVNMeanCov(m, c, mu, Sigma, y);
		*print m, c;							/*Get a 2x1 vector of means and 2x2 Cov matrix*/
		u= m[2,1]; g=sqrt(c[2,2]);				/*Randgen "Normal" takes std instead of variance*/	
		t = j(1,1);
		call randgen(t, "Normal", u, g);		/*Simulate from the marginal distribution x2*/
		x2[i]=t;
		v=j(1,2);
		v[1,1] = x2[i]; v[1,2] = x3[i];
		*print v;
		run CondMVNMeanCov(m, c, mu, Sigma, v);	/*Conditional on the outcome of marginal Dist x3 and x2*/
		*print m, c;
		h=j(1,1);
		p = sqrt(c);
		call randgen(h, "Normal", m , p);
		*print h;
		x_f[i] = h;
		*print x_f;
	end;
  return ( x_f || x2 );
finish;

	mu = {1 2 3};   		                             
	Sigma = {3 2 1,     /*When using proc univariate, should expect to see x1~N(1,1.73)*/
 	        2 4 0,			   					    				 /* x2~N(2,2)   */
 	        1 0 9};													 /* x3~N(3,3)   */
	N = 1000;
	call randseed(1234);
	x3 = j(N,1);
	call randgen(x3, "Normal", 3, 3);				/*mu=3, sigma=3*/
	x2 = j(N,1);
	x_f = j(N,1);
	z= j(N,3,1);
	z[,3] = x3;
	z[,1:2] = Sim_Cont_3 (mu, Sigma, x_f, x2, x3, N);
	*print z;
	varNames = "x1":"x3";
	create mvn3_con from z[c=varNames]; append from z; close mvn3_con;
quit;

proc univariate data = mvn3_con;
   histogram / normal(sigma=est);
   cdfplot / normal(sigma=est);		
   qqplot;
   ods select GoodnessOfFit Histogram CDFPlot QQplot; 	
run; 

/*Compared to:*/
proc iml;
	mu = {1 2 3};   		                             
	Sigma = {3 2 1, 
 	        2 4 0,
 	        1 0 9};
	N = 1000;	
	z = RANDNORMAL(N, mu, Sigma);   
varNames = "x1";
create mvn4 from z[c=varNames]; append from z; close mvn4;
quit;

proc univariate data = mvn4;
   histogram / normal(sigma=est);
   cdfplot / normal(sigma=est);		
   qqplot;
   ods select GoodnessOfFit Histogram CDFPlot QQplot; 	
run; 


/*********************************************************************

	8.7.2 The Transformation Technique 

*********************************************************************/

/* Sample from a multivariate Cauchy distribution */

proc iml;
	start RandMVCauchy(N, p);
     	 z = j(N,p,0);  y = j(N,1);         /* allocate matrix and vector */
 	 	 call randgen(z, "Normal"); 
  	     call randgen(y, "Gamma", 0.5);     /* alpha=0.5, unit scale      */
  	   return( z / sqrt(2*y) );
	finish;

/* call the function to generate multivariate Cauchy variates */
	N=1000; p = 3;
	x = RandMVCauchy(N, p);
	varNames = "x1":"x3";
	create mvn_cauchy from x[c=varNames]; append from x; close mvn_cauchy;
quit;

proc univariate data = mvn_cauchy;
   histogram / normal(sigma=est);
   cdfplot / normal(sigma=est);		
   qqplot;
   ods select GoodnessOfFit Histogram CDFPlot QQplot; 	
run; 


/********************************************************************
 
 
 	The Cholesky Transformation
 
 
*********************************************************************/

proc iml;
	Sigma = {9 1, 
 	        1 1};
	U = root(Sigma);				/*Upper Tri Matrix*/
	print U[format=BEST5.];                 /* U`*U = Sigma */
	/* generate x,y ~ N(0,1), corr(x,y)=0 */
	call randseed(12345);
	xy = j(2, 1000);			/*Uncorrelated*/

	call randgen(xy, "Normal");             /* each col is indep N(0,1) */
	*print xy;
	L = U`; 
	zw = L * xy;         /* Cholesky transformation induces correlation */
	*print zw;
	cov = cov(zw`);      /* check covariance of transformed variables   */
	COV_LL = L*L`;
	print cov[format=BEST5.] COV_LL;
	/* Start with MVN(0, Sigma) data. Apply inverse of L. */
	zw = T( RandNormal(1000, {0, 0}, Sigma) );		/*zw is a row vector*/
	*print zw;
	xy = trisolv(4, L, zw);     /*more efficient than solve(L,zw) OR INV(L) to get L^(-1)*/
	*print xy;		/*form=4 http://support.sas.com/documentation/cdl/en/imlug/66845/HTML/default/viewer.htm#imlug_langref_sect467.htm*/
	tcov = cov(xy`);
	print tcov[format=5.3 label="tcov (Uncorrelated again)"]; 
	x=xy[1,]; y=xy[2,]; z=zw[1,]; w=zw[2,];
varNames = {"x" "y" "z" "w"};
create Chol_xy_zw var varNames; append;
close Chol_xy_zw;
quit;

proc sgscatter data=Chol_xy_zw;
	plot y*x w*z / rows=2 columns=1 axisextent=DATA ;
run;


/********************************************************************


	The Spectral Decomposition


*********************************************************************/

data A(type=corr);
_type_='CORR';
input x1-x3;
cards;
1.0  .   .
0.7 1.0  .
0.2 0.4 1.0
;
run;

/* obtain factor pattern matrix from PROC FACTOR */
proc factor data=A N=3 eigenvectors;
   ods select FactorPattern;
run;

/* Perform the same computation in SAS/IML language */
proc iml;
R = {1.0 0.7 0.2,
     0.7 1.0 0.4,
     0.2 0.4 1.0};

/* factor pattern matrix via the eigenvalue decomp.
   R = U*diag(D)*U` = H`*H = F*F` */
call eigen(D, U, R);
F = sqrt(D`) # U;                   /* F is returned by PROC FACTOR */
Verify = F*F`;
print F[format=8.5] Verify;
z = j(1000, 3);            
call randgen(z, "Normal");   /* uncorrelated normal obs: z~MVN(0,I) */

/* Compute x`=F*z` or its transpose x=z*F` */
x = z*F`;                    /* x~MVN(0,R) where R=FF`= corr matrix */
corr = corr(x);              /* sample correlation is close to R    */
print corr[format=5.3];









