/*Stistical Programming with SAS/IML Language
Most of the code in the book are in the ipython file. 

A SAS/IML Companion for Linear Models 2010
Jamis J. Perrett (Chpater 1 -3)
*/


/**************************************/
/****** Chapter 1: Introduction *******/
/**************************************/


PROC IML;
a={1 2 3,2 3 1,2 4 6};
sum_a=SUM(a);
PRINT A SUM_A;
QUIT;


/* Solving: Ax = c */
PROC IML;
a={5 3-2,
3 4 -3,
4 2 -5};
c={5,2,-7};
x1=INV(a)*c;
x2 = solve(a,c);
PRINT x1 x2;

/*Creating a matrix in 3 three ways

• Creating a matrix by manual entry
• Creating a matrix from a SAS data set
• Creating a matrix from a text file

*/
/*Creating a matrix*/
PROC IML;
a={10 88 92,9 91 95,10 85 83,8 90 87,10 92 96};
CREATE dset_a FROM a [COLNAME={a1,a2,a3}];
APPEND FROM a;
QUIT;

PROC PRINT DATA=dset_a;
RUN;

/*Creating two vectors*/
PROC IML;
names={"Rob","Joe","Erich","Lisa","Mike"};
scores={92,95,83,87,96};
CREATE classdat VAR{Names Scores};
APPEND;
QUIT;
PROC PRINT DATA=classdat;
RUN;


/*CREATE SAS-data-set FROM matrix-name <[COLNAME=column-name 
ROWNAME=row-name]> ;*/

PROC IML;
a={10 88 92,
9 91 95,
10 85 83,
8 90 87,
10 92 96};
PRINT a;
a1 = A[,1];		/*not the colon operator, : , which gets the average column values*/
a2 = A[,2]; 
a3 = A[,3];
CREATE DSET_A VAR {a1,a2,a3};		/*Create a table of a1 a2 a3*/
append;
close DSET_A;
QUIT;

PROC IML;
positions = {Rob President,
Joe Supervisor,
Erich Supervisor,
Lisa Manager,
Mike Director};
PRINT positions;
QUIT;

PROC IML;
USE dset_a;
READ ALL VAR {a1,a2,a3} INTO a;
CLOSE dset_a;
PRINT a;
QUIT;


/************************************/


/********************************************/
/**** Chapter 2: IML Language Structure *****/
/********************************************/


/************************************/

/*LOC Function*/

data MagicUsers;
infile datalines dsd;
length Name $11 Profession $7 Source $20;
input Name Profession Power Source;
datalines;
Morgana,    Witch,   7, Authurian Legend
Merlin,     Wizard, 10, Authurian Legend
Gryffindor, Wizard,  8, Harry Potter Books
Hufflepuff, Witch,   8, Harry Potter Books
Ravenclaw,  Witch,   8, Harry Potter Books
Slytherin,  Wizard,  8, Harry Potter Books
Glinda,     Witch,   5, Oz Books
Elphaba,    Witch,   6, Oz Books
Diggs,      Wizard,  1, Oz Books
;

proc iml;
use MagicUsers;
  read all var {Name Profession Power};
close MagicUsers;
ndx = loc(Profession="Witch"); /** find the indices for witches    **/
Names = Name[ndx];             /** subset the names of the witches **/
print Names, ndx;
quit;

/*Exercise 2.4 Tranfer the idea of the following using loc function*/

/*PROC IML;  						*Don't run*
bmi={15,22,27,19,32,17,23,31,20};
IF 18.5<= bmi <= 25 THEN status="healthy";
ELSE status="unhealthy";
PRINT status;
QUIT;*/

PROC IML;
bmi={15,22,27,19,32,17,23,31,20};
print bmi;
status = j(nrow(bmi),1,"unhealthy");
x =loc((bmi[,1]>18.5)& (bmi[,1]< 25));
print x;
status[x] = "healthy";
print status;
quit;

/*Using the same code: 
A researcher is interested in printing out the status for each of nine subjects
based on their BMI score according to the following criteria: A BMI score
below 17.5 represents a status of “severely underweight”, a BMI score at or
above 17.5 and below 18.5 represents a status of “underweight”, a BMI score
at or above 18.5 and at or below 25 represents a status of “healthy”, a BMI
score above 25 and at or below 30 represents a status of “overweight”, and a
BMI score above 30 represents a status of “severely overweight*/

proc iml;
bmi={15,22,27,19,32,17,23,31,20};
print bmi;
status = j(nrow(bmi),1,"severely underweight");
*status[loc((bmi[,1]>=17.5)& (bmi[,1]< 18.5))] = "underweight";
status[loc((bmi[,1]>=18.5)& (bmi[,1]< 25))] = "healthy";
status[loc((bmi[,1]>=25)& (bmi[,1]<= 30))] = "overweight";
status[loc(bmi[,1]>30)] = "severely overweight";
print status;
quit;


/* Using the LOC function to replace the do loop P34*/

PROC IML;
m={1 2 3,4 5 7};
DO i=1 TO NCOL(m);
DO j=1 TO NROW(m);
odd=MOD(m[j,i],2)>0;
IF (odd)>0 THEN PRINT i (m[j,i]);
END;
END;
QUIT;

/*This next set of code uses the LOC function to replace the inner DO loop.
Notice that in this case, reference is made to M[,i]. When the row of interest is not
specified within the brackets, all rows are included in the reference.*/

PROC IML;
m={1 2 3,4 5 7};
DO i=1 TO NCOL(m);
odd=LOC(MOD(m[,i],2)>0);
IF NCOL(odd)>0 THEN PRINT i (m[odd,i]);
END;
QUIT;

/************************************/

/*Call: CALL name <(arguments)> ;*/


/*A “call” calls a subroutine or function. A difference between calls and functions is
that functions generally return a single result. Calls are subprograms.*/

PROC IML;
mat = {1 2 3,2 4 5,3 5 6};
eigval1=EIGVAL(mat);
eigvec1=EIGVEC(mat);
CALL EIGEN(eigval2,eigvec2,mat);
PRINT eigval1 eigval2,eigvec1 eigvec2;
QUIT;

/*This example computes eigenvalues and eigenvectors using functions, and then
computes the same eigenvalues and eigenvectors using a call statement.*/

/*If a user-defined subroutine is created with the same name as an IML built-in subroutine,
using a CALL statement will implement the IML built-in subroutine, whereas
using a RUN statement will implement the user-defined subroutine.*/

/*Cnotrol Statements*/
PROC IML;
names = {Albert,Bob,Alice,Christopher,Adam,David};
scores = {50,86,89,74,45,88};
grades=J(NROW(names),1,"xxxx");
print grades;
DO i=1 TO NROW(names);
IF scores[i]<55 THEN
grades[i] = "FAIL";
ELSE grades[i] = "PASS";
END;
PRINT 'Names, Scores, and Grades for Exam 1';
PRINT '(Criteria for Passing: 55+)';
PRINT names scores grades;
QUIT;

/*Mathematical Operator*/
/*
Operator Action
+ addition
# multiplication
<> maximum
>< minimum
<:> index of minimum
>:< index of maximum
: mean
## sum of squares
*/

PROC IML;
a={1 2 3 4,
5 6 7 8,
9 0 1 2};
print a;
col_sum=a [+,];
print col_sum;
col_prod=a [#,];
print col_prod;
row_max=a [,<>];
print row_max;
overall_min=a [><];
print overall_min;
overall_max=a[<>];
print overall_max;
row_indx_max=a [,<:>];
print row_indx_max;
row_indx_min=a [,>:<];
print row_indx_min;
quit;

proc iml;
a={1 2 3 4,
5 6 7 8,
9 0 1 2};
print a;
col_1_3_mean=a [:,{1 3}];
print col_1_3_mean;
col_ssq=a[##,];
print col_ssq;
mean_col_max=a[<>,][,:];
print mean_col_max;
mean_row=a[,><];
print mean_row;
mean_row_min=a[,><][:,];
print mean_row_min;
QUIT;

/*Operator  Action
/ division operator
<> element maximum operator
>< element minimum operator
# element-wise multiplication operator
## element-wise power operator*/

PROC IML;
a={1,2,3,4,5};
b={5,4,3,2,1};
div_ab=a/b;
max_ab=a<>b;
min_ab=a><b;
mult_ab=a#b;
pow_ab=a##b;
PRINT a b div_ab max_ab min_ab mult_ab pow_ab;
QUIT;

/*Operator Action
< less than
<= less than or equal to
= equal to
> greater than
>= greater than or equal to
ˆ= not equal to*/

PROC IML;
a={70,70,71,74,72,78,77,75,73,74};
low=a[,1]<72;
high=a[,1]>72;
equal=a[,1]=72;
not_equal=a[,1]^=72;
ge=a[,1]>=72;
le=a[,1]<=72;
PRINT a low high equal not_equal ge le;
QUIT;

PROC IML;
a=1:5;
b=5:1;
PRINT a,b;
QUIT;

PROC IML;
m = {1 2 3,
4 2 6,
5 1 9,
3 7 8};
int = J(4,1,"N");
int[LOC((m[,2] <= m[,1]) & (m[,1] <= m[,3]))] = "Y";
PRINT m int;
QUIT;


/************************************/

/**********************************************/
/**** Chapter 3: IML Programming Features *****/
/**********************************************/


PROC IML;
a={1 2 3,
4 5 6,
7 8 9};
START mat_rank(mat);
e=ECHELON(mat);
e=(e^=0)[, +];
mat_rank=(e^=0)[+, ];
RETURN(mat_rank);
FINISH;
rank=mat_rank(a);
PRINT a rank;
QUIT;

proc iml;
mat = {1 2 3,
4 5 6,
7 8 9};
e=ECHELON(mat);
print e;
e=(e^=0)[, +];			/*Sum across the column*/
print e;
mat_rank=(e^=0)[+, ]; 	/*Sum across the row*/
print mat_rank;
quit;

/** Module **/

PROC IML;
x=5; ** assignment within the global symbol table;
START module1;
x=7; ** assignment within the global symbol table;
FINISH;
START module2(x);
y=2; ** assignment within the local symbol table for
module2;
PRINT x y;
FINISH;
START module3(y,x);
y=8; ** assignment within the local symbol table for
module2;
FINISH;

PRINT x;
RUN module1;
PRINT x;
RUN module2(3); ** local assignment of x=3;
PRINT x;
PRINT "first occurrence of y" y;
RUN module3(y,1); ** local assignment of x=1, output
variable y;
PRINT "second occurrence of y" y;
print "Final Version of x and y:" x y;
QUIT;


/*The Print / Printto Command*/

OPTIONS NODATE NONUMBER;
PROC IML;
TITLE "Intro. Stats. Demographics";
TITLE2 "Males vs. Females";
totals={21 0.4666666667,24 0.5333333333};
rows={"Males","Females"};
cols={" Totals","Percentages"};
RESET FW=4 SPACES=2;
PRINT(totals[1:2,1]) [COLNAME=(cols[1,1]) ROWNAME=rows]
(totals[1:2,2]) [COLNAME=(cols[2,1]) FORMAT=PERCENT.];	/*To print an expression, use parenthnese*/
QUIT;


PROC PRINTTO PRINT='/folders/myfolders/imlfile.txt';
RUN;
OPTIONS NODATE NONUMBER;
PROC IML;
TITLE "Intro. Stats. Demographics";
TITLE2 "Males vs. Females";
totals={21 0.4666666667,24 0.5333333333};
rows={"Males","Females"};
cols={" Totals","Percentages"};
RESET FW=8 SPACES=2;
PRINT(totals[1:2,1]) [COLNAME=(cols[1,1]) ROWNAME=rows]
(totals[1:2,2]) [COLNAME=(cols[2,1]) FORMAT=PERCENT.];
QUIT;
PROC PRINTTO;
RUN;

OPTIONS NODATE NONUMBER;
ODS RTF FILE='/folders/myfolders/imlfile.rtf';
ODS HTML FILE='/folders/myfolders/imlfile.html';
ODS PDF FILE='/folders/myfolders/imlfile.pdf';
PROC IML;
TITLE "Intro. Stats. Demographics";
TITLE2 "Males vs. Females";
totals={21 0.4666666667,24 0.5333333333};
rows={"Males","Females"};
cols={" Totals","Percentages"};
RESET FW=8 SPACES=2;
PRINT (totals[1:2,1]) [COLNAME=(cols[1,1]) ROWNAME=rows]
(totals[1:2,2]) [COLNAME=(cols[2,1]) FORMAT=PERCENT.];
QUIT;
ODS PDF CLOSE;
ODS HTML CLOSE;
ODS RTF CLOSE;


/*Exercise 3.2 Create a modular function named
STANDARDIZE (mat,mean,stdev)
that is passed three input objects: MAT, an input matrix of any dimensions;
MEAN, a scalar constant as a mean; and STDEV, a scalar constant for the
standard deviation. The function will standardize all the values in the input
matrix, MAT, by subtracting MEAN from each element and then dividing the
resulting difference by STDEV. Test the function by passing input matrices of
different dimensions and differing values for MEAN and STDEV. Test it on
the following inputs: mat={1 2 3, 2 4 6};mean=2; stdev=4;*/

PROC IML;
START STANDARDIZE(mat,mean,stdev);
mat_stand = (mat-mean)/stdev;
RETURN(mat_stand);
FINISH;
a={1 2 3, 4 5 6, 7 8 9};
mat={1 2 3, 2 4 6};mean=2; stdev=4;
a_stand = STANDARDIZE(a,mean,stdev);
mat_stand = STANDARDIZE(mat,mean,stdev);
PRINT a a_stand, mat mat_stand;
QUIT;







