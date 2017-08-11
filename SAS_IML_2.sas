/************************************************************
A SAS/IML Companion for Linear Models 2010 Jamis J. Perrett
 *************************************************************
Moore-Penrose inverse

/*******************************************/
/****** Chapter 4: Matrix Manupulations ****/
/*******************************************/


/*Extracting elements from a matrix*/
PROC IML ;
	a={10 88 92 6, 9 91 95 4, 10 85 83 8, 8 90 87 10, 10 92 96 1};
	a1334=a[1:3, 3:4];
	a12=a[1, 2];
	a1314=a[{1 3}, {1 4}];
	ac12=a[, 1:2];
	a_=a[];
	PRINT a, a1334 a12, a1314 ac12 a_;
	QUIT;

	/*Diag function & vecdiag function*/
PROC IML ;
	a={10 88 92, 9 91 95, 10 85 83, 8 90 87, 10 92 96};
	b=a[2:4, 1:3];
	diag_b=DIAG(b);
	vec_b=VECDIAG(b);
	PRINT a b diag_b vec_b;
	QUIT;

	/*Operator*/
PROC IML ;
	a={10 88 92, 9 91 95, 10 85 83, 8 90 87, 10 92 96};
	sum_c12=a[+, 1:2];
	prod_r23=a[2:3, #];
	max_1213=a[1:2, 1:3][<>];
	PRINT a sum_c12 prod_r23, max_1213;
	QUIT;

	/*J function and I function*/
PROC IML ;
	n=5;
	m=3;
	mat={1 2 3, 4 5 6, 7 8 9};
	im=I(m);
	jm=J(m);

	/*Return a 3x3 matrix of 1s*/
	jnm=J(n, m);
	zero=J(m, n, 0);
	jm_vec=J(m, 1);
	jm_hor=J(1, m);
	PRINT im jm jnm zero jm_vec jm_hor;
	QUIT;

	/*Direct (Kronecker) product and block-diagonal matrix*/
PROC IML ;
	n=5;
	m=3;
	im=I(m);
	jm=J(m);
	jn=J(n);
	im_a_jm=im@jm;
	jm_block_jn=BLOCK(jm, jn);
	PRINT im jm jn, im_a_jm, jm_block_jn;
	QUIT;

	/*The matrix im_a_jm is created by multiplying every element of IM by the matrix JM.*/
	/*Design matrices*/
PROC IML ;
	x1=DESIGN({1, 1, 1, 2, 2, 2, 3, 3, 3});
	x2=DESIGNF({1, 1, 2, 2, 3});
	PRINT x1 x2;
	QUIT;

	/*Hermite and echelon reduction matrices*/
PROC IML ;
	RESET FUZZ;
	a={1 2 1, 2 3 2, 1 2 1};
	e=ECHELON(a);
	h=HERMITE(a);
	*print h;
	c=a*h;

	/* A*H = A */
	*print c;
	ginv=GINV(a);

	/*A^{{{\mathrm  g}}} is a generalized inverse of A if it satisfies
	the condition A(A^g)A=A.*/
	b=h*GINV(a);

	/*A^g = H*ginv(A)*/
	ba=b*a;

	/* b*a=H*/
	aba=a*b*a;
	PRINT a, e, h, ginv, b, ba, aba;
	QUIT;

	/*Repeat function*/
PROC IML ;
	n=5;
	m=3;
	mat={1 2 3, 4 5 6, 7 8 9};
	rep=REPEAT(mat, m, n);
	norm=NORMAL(REPEAT(0, n, 1));
	PRINT mat, rep norm;
	QUIT;

	/****** Exercises ******/
	/*
	4.1 Use the following data to create the 3×4 matrix A. Create a row vector
	COL_TOT_A consisting of the column totals for the columns of the matrix
	A. Create a column vector ROW_TOT_A consisting of the row totals for the
	rows of the matrix A. Re-direct the output to a pdf document. Print the matrix
	A, the row vector COL_TOT_A, and the column vector ROW_TOT_A to the
	PDF document. The PDF document should also contain a descriptive title at the
	top of the document and descriptive labels (rather than the defaults) centered
	above each matrix and vector.*/
proc iml ;
	a={1 3 5 7, 2 4 6 8, 9 0 9 0};
	COL_TOT_A=a[+, ];
	print COL_TOT_A;
	ROW_TOT_A=a[, +];
	rowname={"Row 1", "Row 2", "Row 3"};
	print ROW_TOT_A[COLNAME="Row Total" ROWNAME=rowname];
	quit;

	/*4.2 Use the following matrix A to create a 2×2 submatrix, B, consisting of the 2,2;
	2,3; 3,2; and 3,3 elements of the matrix A. Create a row vector COL_TOT_B
	consisting of the column totals for the columns of thematrix B. Create a column
	vector ROW_TOT_B consisting of the row totals for the rows of the matrix B.
	Re-direct the output to an HTML document. Print the matrix B, the row vector
	COL_TOT_B, and the column vector ROW_TOT_B to the HTML document.
	The HTML document should also contain a descriptive title at the top of the
	document and descriptive labels (rather than the defaults) centered above each
	matrix and vector*/
proc iml ;
	a={1 3 5 7, 2 4 6 8, 9 0 9 0};
	B=a[{2 3}, {2 3}];
	print B;
	COL_TOT_B=B[+, ];
	print COL_TOT_B;
	ROW_TOT_B=B[, +];
	print ROW_TOT_B;
	quit;

	/*4.3 Create a column vector, C, consisting of the diagonal elements of the
	matrix, D.*/
proc iml ;
	d={1 2 3, 2 3 4, 3 4 5};
	C=vecdiag(d);
	print C;
	quit;

	/************ Tough question!!!!! **************/
	/*4.4*/
proc iml ;
	A=J(6, 6, 0);
	A[1:3, 4]=1;
	*A[2,2:6] = 1;
	*A[3,2:4] = 1;
	*A[1,1:3] = 1;
	*A[, 3] =1;
	*print A;
	SP={0 0};
	EP={0 0};
	start modu;
	length=A[+];
	PRINT length;

	if A[+, ][<>]>1 then
		do;

			/*execute if the ship is vertical*/
			SP[1, 1]=A[, <>][<:>];
			SP[1, 2]=A[<>, ][<:>];
			PRINT SP;
			EP[1, 1]=SP[1, 1]+length-1;
			EP[1, 2]=A[<>, ][<:>];
			print EP;
		end;

	if A[, +][<>]>1 then
		do;

			/*execute if the ship is horizontal*/
			SP[1, 1]=A[, <>][<:>];
			SP[1, 2]=A[<>, ][<:>];
			print SP;
			EP[1, 1]=A[, <>][<:>];
			EP[1, 2]=SP[1, 2]+length-1;
			PRINT EP;
		end;
	finish;
run modu;

print A, SP, EP, length;
quit;

/*4.5 Use the direct product operator, @, the functions I and J, and the horizontal
concatenation operator, ||, where appropriate to produce the following matrices:*/
proc iml ;
	jm=j(3, 3, 1);
	jn=I(3);
	M1=jn@jm;
	print M1;
	quit;

proc iml ;
	jm=j(2, 2, 1);
	jn=I(3);
	M2=jn@jm;
	print M2;
	quit;

proc iml ;
	jm=j(2, 1, 1);
	jn=I(3);
	M3=jn@jm;
	print M3;
	quit;

proc iml ;
	k1=j(6, 1, 1);
	print k1;
	k2=I(2);
	k3=J(3, 1, 1);
	k4=I(3);
	M2=k2@k3;
	print M2;
	M3=k4//k4;
	print M3;
	M=k1||M2||M3;
	print M;
	quit;

proc iml ;
	k1=j(6, 1, 1);
	I=I(3);
	k2=J(2, 1, 1);
	L=I@k2;
	*print L;
	M=k1||L;
	O={"C" "A1" "A2" "A3"};
	print M[c=O];
	quit;

proc iml ;
	k1=j(6, 1, 1);
	x=DESIGN({1, 1, 2, 2, 3, 3});
	M=k1||x;
	O={"C" "A1" "A2" "A3"};
	print M[c=O];
	quit;

	/*Create a module named STDEV that computes the standard deviations of each
	of the columns of an m×n matrix. Check it with the following matrix, S, to see
	if it returns the values 1, 2, 3, and 4.*/
PROC IML ;
	S={0 3 1 1, 1 5 4 5, 2 7 7 9};
	print S;
	STDEV=0;
	START STD_MANUAL;
	STDEV=(((S-S[:, ])[##, ])/2)##(.5);
	FINISH;
RUN STD_MANUAL;

PRINT STDEV;
STD=STD(S);
PRINT STD;
QUIT;

/*4.9 Create a module named COLTOT that gives column totals (sums) for the
columns of a given matrix. Check it with the matrix, S, from Exercise 4.9 to
see if it returns the values 3, 15, 12, and 15.*/
proc iml ;
	S={0 3 1 1, 1 5 4 5, 2 7 7 9};
	print S;
	COLTOT=S[+, ];
	print COLTOT;
	RCTOTALS=S[, +];
	print RCTOTALS;
	Z_VEC_1=(RCTOTALS-MEAN(RCTOTALS))/STD(RCTOTALS);
	Z_VEC_2=(RCTOTALS-RCTOTALS[:, ])/((((RCTOTALS-RCTOTALS[:, ])[##, ])/2)##(.5));
	PRINT Z_VEC_1, Z_VEC_2;
	quit;

	/*4.12 Consider the experiment of rolling one or more six-sided fair dice and adding
	up the values of the dice as the result. Create a module named
	
	DICE(cubes, num)
	
	that accepts the inputs cubes, the number of dice; and num, the result of interest.
	The module then determines the number of possible ways the result, num, can
	be rolled by the dice. The module then prints a sentence indicating the number
	of cubes used, the result of interest, and the number of ways that result can
	occur. So, assuming the input values were cubes=2 and num=4, the output
	would read, “There are 2 ways the number 4 can occur by rolling 2 dice.” This
	experiment counts the occurrence of the first cube reading a 1 and the second
	cube reading a 3 (1 and 3) the same as if the first cube reads a 3 and the second
	cube reads a 1 (3 and 1). Consequently, there are two ways to obtain a result
	of 4 by rolling two dice: (1 and 3) and (2 and 2). Test the module DICE using
	cubes=1, num=3 and cubes=4, num=7.*/
	
	
	/*4.13 Repeat the previous problem but consider the occurrence of the first cube reading
	a 1 and the second cube reading a 3 (1 and 3) different from the occurrence
	of the first cube reading a 3 and the second cube reading a 1 (3 and 1). Test the
	module using cubes=1, num=3 and cubes=4, num=7.*
	
	
	
	/********************************************************/
	/****** Chapter 5: Mathematical and Statistic Basics ****/
	/********************************************************/
PROC IML ;
	a={1 2 3, 4 5 6, 7 8 9};
	trace=TRACE(a);
	t_a=T(a);
	PRINT a t_a trace;
	b={2 1 1 5, 4 -6 0 -2, -2 7 2 9};
	a_different_rank=rank(b);
	rank=ROUND(TRACE(GINV(b)*b));
	PRINT b, a_different_rank, rank;
	QUIT;

	/*This statement makes use of the fact that the rank of A is equal to the rank of AA−,
	and since AA− is idempotent, its rank is equal to its trace. For a proof, refer to
	Graybill, 2000.
	
	It is of note that this method for finding the rank of the matrix A requires inverting
	the matrix A. If the matrix A is large, its inversion can be computational intensive.
	A more efficient way to find the rank of a matrix is to find it in such a way that does
	not require inverting the matrix.*/
PROC IML ;
	a={1 2 3, 4 5 6, 7 8 9};
	PRINT a;
	r1=ROUND(TRACE(GINV(a)*a));
	START myrank(mat);
	e=echelon(mat);
	e=(e^=0)[, +];
	myrank=(e^=0)[+, ];
	RETURN(myrank);
	FINISH;
	r2=MYRANK(a);
	h=hermite(a);
	print h;
	r3=TRACE(HERMITE(a));
	PRINT r1 r2 r3;

	/*Unimodular Matrix*/
	U={2 3 2, 4 2 3, 9 6 7};
	Det_UM=Det(U);
	PRINT Det_UM;
	QUIT;

	/*Moore-Penrose inverse*/
PROC IML ;
	/* GINV(A)*y = x <=> y = A*x */
	A={1 3, 5 7, 11 13};
	y={17, 19, 23};
	MP=ginv(A);

	/*  GINV(A) = INV(A` * A) * A`  */
	I=MP*A;
	x=GINV(A)*y;
	K=INV(A` * A) * A`;
	PRINT A, MP, I, x, K;
	QUIT;

	/*Descriptiv Statistics*/
DATA outcomes;
	INPUT col1 col2 col3;
	DATALINES;
1 2 3
4 5 6
7 8 9
;
RUN;

PROC IML ;
	USE outcomes;
	SUMMARY VAR {col1 col2 col3}
STAT {N SUM MAX MEAN VAR STD}
OPT {SAVE};
	CLOSE outcomes;
	PRINT _NOBS_, col1, col2, col3;
	QUIT;

	/*Sum Statement*/
PROC IML ;
	x={1 2 3, 4 5 6, 7 8 9};
	vec_sum=x[1, +];
	col_tot=x[+, ];
	mat_sum=SUM(x);
	submat=x[1:2, 1:2];
	sm_sum=SUM(submat);
	PRINT vec_sum col_tot mat_sum sm_sum;
	QUIT;

PROC IML ;
	x={1 2 3, 4 5 6, 7 8 9};
	vec_mean=x[1, :];
	mat_mean=x[:];
	submat=x[1:2, 1:2];
	sm_mean=submat[:];
	PRINT vec_mean mat_mean sm_mean;
	QUIT;

	/*Variance*/
PROC IML ;
	x={1 2 3, 4 5 6, 7 8 9};
	ssq=SSQ(x[1, ]);
	sum=x[1, +];
	vec_var=(ssq-sum*sum/NCOL(x))/(NCOL(x)-1);
	jn=J(NROW(x));
	mat_mean=x[:];
	mat_var=SSQ(x-mat_mean*jn)/(NROW(x)*NCOL(x)-1);
	submat=x[1:2, 1:2];
	print submat;
	ssq_sub=SSQ(submat);
	sum_sub=SUM(submat);
	PRINT SUM_SUB;
	sm_var=(ssq_sub - sum_sub*sum_sub/(NCOL(submat) *NROW(submat))) / 
		(NCOL(submat)*NROW(submat)-1);
	PRINT vec_var mat_var sm_var;
	QUIT;

	/*************************************************************/
	/* Counting the frequency !! Read the next secton first */
	/*************************************************************/
	
PROC IML ;
	scores={90, 62, 66, 68, 70, 72, 73, 74, 78, 78, 78, 79, 80, 81, 82, 82, 82, 
		84, 84, 85, 85, 85, 85, 87, 88, 89, 89, 89, 89, 61};
	*PRINT scores;
	count=NROW(scores);
	sum=SUM(scores);
	mean=sum/count;

	/* =scores[:] */
	median=MEDIAN(scores);

	/** Frequency Distribution **/
	START freq(x);
	unique=UNION(x)`;

	/** column vector of unique scores **/
	observed=J(NROW(unique), 1);

	DO i=1 TO NROW(unique);

		/*A column vector of 1s*/
		observed[i]=NCOL(LOC(x=unique[i]));
	END;
	freq=unique||observed;
	f_Label={" SCORE" "FREQUENCY"};
	PRINT "Frequency Distribution", freq [COLNAME=f_label];
	FINISH;
	CALL FREQ(scores);
	START mode(x);
	unique=UNION(x)`;
	observed=J(NROW(unique), 1);

	DO i=1 TO NROW(unique);
		observed[i]=NCOL(LOC(x=unique[i]));
	END;
	max=MAX(observed);
	mode=unique[LOC(observed=max), 1];
	RETURN(mode);
	FINISH;
	mode=MODE(scores);
	print mode;
	count=NROW(scores);
	sum=SUM(scores);
	ssq=SSQ(scores);
	variance=(ssq-sum*sum/count)/(count-1);
	stdev=SQRT(variance);
	quartiles=QUARTILE(scores);

	/*min Q1 Q2 Q3 max*/
	START percentile(x, pct);
	CALL SORT(x, 1);

	/*CALL SORT( matrix, by<, descend >*/
	count=NROW(x);
	pct=pct/100*(count+1);
	fpct=FLOOR(pct);
	cpct=CEIL(pct);

	IF fpct=cpct THEN
		percentile=x[pct];
	ELSE
		percentile=x[fpct]+(pct-fpct)* (x[cpct]-x[fpct]);
	RETURN(percentile);
	FINISH;
	percentile=PERCENTILE(scores, 75);
	CALL SORT(scores, 1);
	quantile=scores[CEIL(count*.45)];
	PRINT count sum mean median mode, variance stdev quartiles, "75th Percentile:" 
		percentile, "45th Quantile:" quantile;
	QUIT;

	/* Disecting the above code block */
PROC IML ;
	scores={90, 62, 66, 68, 70, 72, 73, 74, 78, 78, 78, 79, 80, 81, 82, 82, 82, 
		84, 84, 85, 85, 85, 85, 87, 88, 89, 89, 89, 89, 61};
	print scores;
	unique=UNION(scores)`;

	/** column vector of unique and sorted scores **/
	print unique;
	observed=J(NROW(unique), 1);
	x1=LOC(scores=unique[1]);
	x2=LOC(scores=unique[2]);
	x3=LOC(scores=unique[3]);
	x4=LOC(scores=unique[4]);
	print x1 x2 x3 x4;
	y1=NCOL(LOC(scores=unique[1]));
	y2=NCOL(LOC(scores=unique[2]));
	y3=NCOL(LOC(scores=unique[3]));
	y4=NCOL(LOC(scores=unique[4]));
	print y1 y2 y3 y4;

	DO i=1 TO NROW(unique);

		/*A column vector of 1s*/
		observed[i]=NCOL(LOC(scores=unique[i]));
	END;
	freq=unique||observed;
	f_Label={" SCORE" "FREQUENCY"};
	PRINT "Frequency Distribution" freq [COLNAME=f_label];
	max=MAX(observed);
	print max;
	mode=unique[LOC(observed=max), 1];
	print mode;
	QUIT;

PROC IML ;
	scores={90, 62, 66, 68, 70, 72, 73, 74, 78, 78, 78, 79, 80, 81, 82, 82, 82, 
		84, 84, 85, 85, 85, 85, 87, 88, 89, 89, 89, 89, 61};
	print scores;
	count=nrow(scores);
	quartiles=QUARTILE(scores);
	CALL SORT(scores, 1);

	/*CALL SORT( matrix, by<, descend >*/
	print scores;

	/*scores is now sorted*/
	count=NROW(scores);
	print count;
	pct=75/100*(count+1);

	/*What is hte 75th percentile of the vector scores*/
	PRINT pct;
	fpct=FLOOR(pct);
	print fpct;
	cpct=CEIL(pct);
	print cpct;

	IF fpct=cpct THEN
		percentile=scores[pct];
	ELSE
		percentile=scores[fpct]+(pct-fpct)* (scores[cpct]-scores[fpct]);
	print percentile;
	CALL SORT(scores, 1);
	quantile=scores[CEIL(count*.45)];
	PRINT quartiles, "75th Percentile:" percentile, "45th Quantile:" quantile;
	QUIT;
	

/***********************************/
/**** Chapter 6: Linear Algebra ****/
/***********************************/


/*Example 6.3 – Singular value decomposition (Moore-Penrose inverse):Call SVD(u, q, v, a);*/

Proc IML;
a = {1 2 3 4,
5 6 7 8,
9 0 1 2};
RESET FUZZ;
CALL SVD(u,q,v,a);
upu = u`*u;
vpv = v`*v;
vvp = v*v`;
uup = u*u`;
a2 = u*DIAG(q)*v`;
ginva=GINV(a);
m = NROW(a); n = NCOL(a);
DO i=1 TO n;
IF q[i] <= 1E-12 * q[1] then q[i] = 0;
ELSE q[i] = 1 / q[i];
END;
ga = v*DIAG(q)*u`;
PRINT a a2, u, q, v, upu uup,  vpv vvp, ginva ga;
quit;

proc iml;			/* This won't work because a is not a square matrix */
a = {1 2 3 4,
5 6 7 8,
9 0 1 2};
f = a` *a;
ginva_2 = INV(f) * a`;
print ginva_2;
QUIT;	


/*Example 6.4 – Gram-Schmidt orthonormalization: Call GSORTH(p, t, lindep, a);*/

PROC IML;
a={1 2 3,2 3 5,3 7 11};
CALL GSORTH(p,t,lindep,a);
p_inv=INV(p);
b=p*t;
PRINT a b,p p_inv,t lindep;
QUIT;


/*Example 6.5 – Solving AX=0 for X (NULL SPACE):
http://support.sas.com/documentation/cdl/en/imlug/66112/HTML/default/viewer.htm#imlug_langref_sect178.htm*/

PROC IML;
RESET FUZZ;
a={10 5 15, 12 6 18, 14 7 21, 16 8 24};
x=HOMOGEN(a);
zero1=a*x[,1];
zero2=a*x[,2];
e = echelon(a);
rank=ROUND(TRACE(GINV(a)*a));
PRINT a x zero1 zero2 rank e ;
QUIT;


PROC IML;
a={1 2 3,6 5 4,0 7 8};
b={1, 2, 3};
x1=SOLVE(a,b);
x2=GINV(a)*b;
x3=INV(a)*b;
ax1=a*x1;
ax2=a*x2;
ax3=a*x3;
PRINT a b x1 x2 x3 ax1 ax2 ax3;
QUIT;


/*eigenvalues and eigenvectors

• Verifying orthogonality of vectors
• Determining the existence of a solution vector in a linear system
• Determining if a square matrix is singular or nonsingular
• Determining if a symmetric matrix is definite or semidefinite in nature
• Determining whether or not a symmetric matrix is idempotent
• Computing the rank or trace of a matrix */


/*Example 6.7 – Definite nature of a symmetric matrix using eigenvalues:*/

Proc IML;
a={ 1 0 1,
0 4 -1,
1 -1 2};
RESET FUZZ;
eigval=EIGVAL(a);
rank=ROUND(TRACE(GINV(a)*a)); /** rank of matrix A **/
PRINT a ,eigval ,rank;
QUIT;

/*This example demonstrates the following about A:
Because the eigenvalues are all > 0 for the symmetric matrix A,
1. A is positive definite (Graybill, 2000).
2. The rank of A is n=3.
3. A is nonsingular.*/


/*Example 6.8 – Eigenvalues and eigenvectors:*/

Proc IML;
A={ 1 0 1,
0 4 -1,
1 -1 2};
RESET FUZZ;
CALL EIGEN(eigval,eigvec,a);
ax1=a*eigvec[,1];
ax2=a*eigvec[,2];					
ax3=a*eigvec[,3];
lx1=eigval[1]*eigvec[,1];
lx2=eigval[2]*eigvec[,2];			/* ax1 and lx1 hould be equal*/
lx3=eigval[3]*eigvec[,3];
print a, eigval, eigvec, ax1 lx1, ax2 lx2, ax3 lx3;
det_a1 = det(A-(eigval[1])*I(3));	/* Characteristic polynomial/equation should be set to zero*/
det_a2 = det(A-(eigval[2])*I(3));	
det_a3 = det(A-(eigval[3])*I(3));	
print det_a1 det_a2 det_a3;
zero1 = (A-(eigval[1])*I(3))*eigvec[,1];
zero2 = (A-(eigval[2])*I(3))*eigvec[,2];
zero3 = (A-(eigval[3])*I(3))*eigvec[,3];
print zero1 zero2 zero3; 
QUIT;










