/************************************************************
A SAS/IML Companion for Linear Models 2010 Jamis J. Perrett
 *************************************************************


/**********************************************************/
/****** Chapter 4: The Multovariate Normal Distriution ****/
/**********************************************************/

/*Bivariate Normal Distribution*/

PROC IML;
	x={4.4 5.5,2.4 3.1,5.5 6.1,7.6 6.3,7.4 7.5,		/*10x2*/
	8.5 10.2,0.6 1.5,4.5 4.1,7.2 5.8,2.8 2.5};
	n=NROW(x);
	jn=J(n,1);									
	jnn=J(n,n,1);
	in=I(n);
	mean=(1/n)*(jn`*x)`;
	cov=(1/(n-1))*(x`*(in-(1/n)*jnn)*x);
	PRINT x, mean cov;
QUIT;


/**/

PROC IML;
	a={-2 -1 0 1 2,2 -1 -2 -1 2};
	CALL RANDSEED (1234);
	x=NORMAL(REPEAT(0,1000,5));
	*PRINT X;
	b={1,2};
	y=a*x`+b;
	*PRINT Y;
	n=NROW(x);
	jn=J(n,1);
	jnn=J(n,n,1);
	in=I(n);
	mean_x=(1/n)*(jn`*x);
	cov_x=(1/(n-1))*(x`*(in-(1/n)*jnn)*x);

	mean1_y=a*mean_x`+b;
	cov1_y=a*cov_x*a`;

	mean2_y= (1/n)*(jn`*y`)`;					/* Equation 7.1 and 7.2 */
	cov2_y= (1/(n-1))*(y*(in-(1/n)*jnn)*y`);

	PRINT mean_x[FORMAT=5.2], cov_x[FORMAT=5.2],
	mean1_y[FORMAT=5.2], cov1_y[FORMAT=5.2],
	mean2_y[FORMAT=5.2], cov2_y[FORMAT=5.2];
QUIT;


