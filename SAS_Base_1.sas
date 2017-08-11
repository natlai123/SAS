/*Base SAS code including SAS Programming 2 and Certificate Book*/

libname orion "/folders/myfolders/SAS_Programming/ecprg193";


/* SAS Programming 1 */

/*
options nonumber nodate;
options date;
options number pageno=3;
option pagesize=15;
option linesize=64;
options datastmtchk = allkeywords;
*/


proc optsave out=optionsave;
run;

option date pageno=3 pagesize = 15 linesize = 64;

data countries;
set orion.country;
run;

proc print data = countries;
run;

data countries1;
set orion.country;
where Continent_ID >= 92;
*remember one rule: when you use a WHERE statement in the DATA step,
 the WHERE expression must reference only variables from the input data set.;
run;

options yearcutoff=1925 firstobs=4;
proc print data = countries1;

proc optload data=optionsave;
run;

proc print data = countries;run;
proc print data = countries1;run;



proc contents data=orion._all_ nods;
run;
*<libref.>_ALL_ requests a listing of all files in the library. (Use a period (.) to append
_ALL_ to the libref.) You can specify NODS only when you specify _ALL_.);

proc contents data=orion._all_;
run;

proc datasets;
 contents data= orion.orders;
quit;
*is very functionally the same as;
proc contents data= orion.orders ;
run;
proc contents data= orion.orders position;
run; /*This gives a table of Variables in Creation Order*/
proc contents data= orion.orders varnum;
run;
*The major difference between the CONTENTS procedure and the CONTENTS
statement in PROC DATASETS is the default for libref in the DATA= option. For
PROC CONTENTS, the default is either Work or User. For the CONTENTS statement,
the default is the libref of the procedure input library. Notice also that PROC
DATASETS supports RUN-group processing. It uses a QUIT statement to end the
procedure. The QUIT statement and the RUN statement are not required.

By default, PROC CONTENTS and PROC DATASETS list variables alphabetically. To
list variable names in the order of their logical position (or creation order) 
in the data set, you can specify the VARNUM option in PROC CONTENTS or in the
CONTENTS statement in PROC DATASETS.;


*The NODUPKEY option deletes observations with duplicate BY values.;

proc sort data=orion.orders out=work.custorders ;*nodupkey;
	by Customer_ID;
run;

*To affect any single file, you can use FIRSTOBS= or OBS= as data set options instead
of as system options.;

options firstobs=3;
proc print data=work.custorders;
 by Customer_ID;
 id Customer_ID;
run;

proc sort data=orion.orders out=work.custorders nodupkey;
	by descending Customer_ID;
run;

proc print data=work.custorders (firstobs=3);
by descending Customer_ID;
run;

* descending order has to match as well;

*The NODUPKEY option checks for and eliminates observations with duplicate BY 
variable values. If you specify this option, PROC SORT compares all BY variable 
values for each observation to those for the previous observation written to 
the output data set. If an exact match using the BY variable values is found, 
the observation is not written to the output data set The DUPOUT= option can be
used only with the NODUPKEY option.;

proc sort data=orion.orders out=work.custorders nodupkey dupout=work.duplicates;
	by Customer_ID;
run;

proc print data=work.duplicates;
run;

proc print data=orion.sales noobs;
	var Employee_ID First_Name Last_Name Job_Title;
	format First_Name Last_Name $upcase. 
          Job_Title $quote25.;
run;


*We can suppress the creation of the record with
 the overall mean with the nway option on the proc 
 means statement.;

proc means data=orion.order_fact nway;
	class Product_ID;
	var Total_Retail_Price;
	output out=product_orders sum=Product_Revenue;
run;

proc sort data=product_names;
	by Product_Revenue;
run;

data product_names;
	merge product_orders orion.product_list;
	by Product_ID;
	keep Product_ID Product_Name Product_Revenue;
run;


title 'Top Five Products by Revenue';
proc print data=product_names(obs=5) label;
	label Product_ID='Product Number' Product_Name='Product' 
		Product_Revenue='Revenue';
	format Product_Revenue eurox12.2;
run;

title;
ods trace on;
ods select ExtremeObs;

proc univariate data=orion.shoes_tracker;
	var Product_ID;
run;

ods trace off;
/*ODS csvall include titles:
   The MEANS Procedure
*/
ods csv file="/folders/myshortcuts/_myfolders
/salaries.csv";

proc means data=orion.sales;
	var Salary;
	class Gender Country;
run;

ods csv close;

ods html file="/folders/myshortcuts/_myfolders
/salaries.html";

proc means data=orion.sales;
	var Salary;
	class Gender Country;
run;
ods html close;

ods listing;
proc means data=orion.sales;
	var Salary;
	class Gender Country;
run;
ods listing close;


%let path=/folders/myshortcuts/_myfolders/ecprg193;
libname orion "&path";

proc sort data = orion.sales out= salesss;
by Gender;
run;

proc means data=salesss;
	var Salary;
	class Gender;
	id gender;
run;

proc means data=salesss;
	var Salary;
	by Gender;
	id gender;
run;


*******************************************************;
title 'Order Summary by Year and Type';

proc freq data=orion.orders nlevels;
	tables Order_Date;
	tables Order_Type/nocum;
	tables Order_Date*Order_Type/nopercent norow nocol missing;
	format Order_Date year4. Order_Type ordertypes.;
run;

title;

proc freq data=orion.nonsales2 nlevels;
	tables Gender Country Employee_ID/nocum nopercent noprint;
run;

proc freq data=orion.nonsales2 order=freq nlevels;
	tables Gender Country Employee_ID/nocum nopercent;
run;

proc print data=orion.nonsales2;
	where Gender not in ('F', 'M') or Job_Title is null or Country not in ('AU', 
		'US') or Salary not between 24000 and 500000 or Employee_ID is missing or 
		Employee_ID=120108;
run;

%let path=/folders/myfolders/ecprg193; 
libname orion "&path";

data work.contacts2;
	infile "&path/phone.csv" dlm=',' missover;
	input Name :$20. Phone :$14. Mobile :$14.;
run;

*turnover option does not work here because the data step is using modified list instead
of column input and formatted input;

proc print data=contacts2 noobs;
run;

data work.employees;
	set orion.sales;
	FullName=catx(' ', First_Name, Last_Name);
	Yrs2012=intck('year', Hire_Date, '01JAN2012'd);
	format Hire_Date ddmmyy10.;
	label Yrs2012='Years of Employment in 2012';
run;

*INTCK ( interval, from, to ) ;

proc print data=work.employees label;
	var FullName Hire_Date Yrs2012;
run;

data work.increase;
	set orion.staff;
	where Emp_Hire_Date>='01JUL2010'd;
	Increase=Salary*0.10;
	if Increase>3000;
	NewSalary=Salary+Increase;
	label Employee_ID='Employee ID' Emp_Hire_Date='Hire Date' 
		NewSalary='New Annual Salary';
	format Salary NewSalary dollar10.2 Increase comma5.;
	keep Employee_ID Emp_Hire_Date Salary Increase NewSalary;
run;

proc contents data=work.increase (label = 'SVSVEVEGHTJYRTULKEBUSYRTNHF');
run;

proc print data=work.increase (obs=10 where = (Employee_ID < 120761)) label;
where put(Employee_ID,8.) contains '8';
run;

********** Match-Merge with Non-Matches**********;

/*data empsauc;
merge empsau phonec;
by EmpID;
run;*/
/*data empsauc;
merge empsau(in=Emps)
phonec(in=Cell);
by EmpID;
run;*/
/*date empsauc;
merge empsau(in=Emps)
phonec(in=Cell);
by EmpID;
if Emps=1 and Cell=1;
run;*/

/*SAS creates a temporary numeric variable that indicates whether
the data set contributed data to the current observation. The
temporary variable has two possible values. If the value of the
variable is 0, it indicates that the data set did not contribute to
the current observation. If the value of the variable is 1, the data
set did contribute to the current observation.*/

data empsau;
	input First $ Gender $ EmpID;
	datalines;
Togar   M   121150
Kylie   F   121151
Birin   M   121152
;
run;

data phonec;
	input EmpID Phone $15.;
	datalines;
121150 +61(2)5555-1795
121152 +61(2)5555-1667
121153 +61(2)5555-1348
;
run;

********** Non-Matches from empsau **********;

data empsauc;
	set empsau(in=Emps) phonec(in=Cell);
	by EmpID;
run;

title 'Append';

proc print data=empsauc;
run;

title;

data empsauc1;
	merge empsau(in=Emps) phonec(in=Cell);
	by EmpID;
run;

title 'Merge';

proc print data=empsauc1;
run;

title;

data empsauc2;
	merge empsau(in=Emps) phonec(in=Cell);
	by EmpID;
	if Emps=1 and Cell=0;
run;

title 'if Emps=1 and Cell=0';

proc print data=empsauc2;
run;

title;

data empsauc3;
	merge empsau(in=Emps) phonec(in=Cell);
	by EmpID;

	if Emps=0 and Cell=1;
run;

title 'if Emps=0 and Cell=1';

proc print data=empsauc3;
run;

title;

data empsauc4;
	merge empsau(in=Emps) phonec(in=Cell);
	by EmpID;
	if Emps=0 or Cell=0;
run;

title if 'Emps=0 or Cell=0';

proc print data=empsauc4;
run;

title;

data empsauc5;
	merge empsau(in=Emps) phonec(in=Cell);
	by EmpID;
	if Emps=1 and Cell=1;
run;

title if 'Emps=1 and Cell=1';

proc print data=empsauc5;
run;

title;


***********************************************;

proc sort data=orion.product_list out=work.product;
	by Supplier_ID;
run;

data work.prodsup;
	merge work.product(IN=P) orion.supplier(IN=S);
	by Supplier_ID;

	IF P = 1 and S = 0;
run;

proc print data=work.prodsup;
	var Product_ID Product_Name Supplier_ID Supplier_Name;
run;

****************************************************;

proc sort DATA=orion.customer out=work.customer_sort;
	by Country;
run;

data work.allcustomer;
	merge work.customer_sort(IN=CS) orion.lookup_country(rename=(Start=Country 
		Label=Country_Name) IN=LC);
	By Country;
	keep Customer_ID Country Customer_Name Country_Name;

	IF CS=1 AND LC=1;
run;

proc print data=work.allcustomer;
run;

****************************************************;

proc sort data=orion.orders out=work.orders_sort;
	by Employee_ID;
run;

data work.allorders work.noorders;
	merge orion.staff(IN=Staff) work.orders_sort(IN=Ord);
	By Employee_ID;

	IF Ord=1 then
		output work.allorders;
	ELSE IF Staff=1 & Ord=0 then
		output work.noorders;
	Keep Employee_ID Job_Title Gender Order_ID Order_Type Order_Date;
run;

title "work.allorders Data Set";

proc print data=work.allorders;
run;

title "work.noorders Data Set";

proc print data=work.noorders;
run;

title;
********************************************************************;
%let path=/folders/myfolders/ecprg193;
libname orion "&path";

proc freq data=orion.sales;
	tables Gender Country;
	where Country='AU';
run;

proc freq data=orion.sales;
	tables Gender/nocum nopercent;
	where Country='AU';
run;

proc format ;
	value tiers low-25000='Tier1' 25000<-50000='Tier2' 50000<-100000='Tier3' 
		100000<-high='Tier4';
run;

proc freq data=orion.sales;
	tables Salary;
	format Salary tiers.;
run;

***********************************************************;

proc freq data=orion.sales;
	tables Gender Country;
run;

/*Whenever using by statement, USE PROC SORT FIRST !!!*/
proc sort data=orion.sales out=sorted;
	by Country;
run;

proc freq data=sorted;
	tables Gender;
	by Country;
run;

proc freq data=orion.sales;
	tables Gender*Country;
run;

proc freq data=orion.sales;
	tables Gender* Country/nocol norow nopercent nofreq;
run;

proc freq data=orion.sales;
	tables Gender* Country/list;
run;

proc freq data=orion.sales;
	tables Gender* Country/crosslist;
run;

***********************************************************;
proc format ;
	value ordertypes 1='Retail' 2='Catalog' 3='Internet';
run;

title 'Order Summary by Year and Type';

proc freq data=orion.orders;
	tables Order_Date;
	format Order_Date YEAR4.;
	tables Order_Type/ nofreq nocum;
	format Order_Type ordertypes.;
	tables Order_Date*Order_Type/ norow nocol nopercent;
	format Order_Date YEAR4. Order_Type ordertypes.;
run;
title;


***********************************************************;

proc print data=orion.nonsales2 (obs=20);
run;

/*Check the Problems*/
proc freq data=orion.nonsales2;
	tables Gender Country/nocum nopercent;
run;

proc freq data=orion.nonsales2 order=freq;
	tables Employee_ID/nocum nopercent;
run;

proc freq data=orion.nonsales2 nlevels order=freq;
	tables Job_Title/nocum nopercent;
run;

proc print data=orion.nonsales2;
	where Gender not in ('F', 'M') or Job_Title is null or Country not in ('AU', 
		'US') or Salary not between 24000 and 500000 or Employee_ID is missing 
		or /* or !!!!*/
		Employee_ID=120108;
run;

***********************************************************;
title1 'Unique Customers and Salespersons 
       for Retail Sales';

proc freq data=orion.orders nlevels;
	tables Customer_ID Employee_ID/noprint;
	where Order_Type=1;
run;

title;
title1 'Catalog and Internet Customers';

proc freq data=orion.orders nlevels order=freq;
	tables Customer_ID/nocum;
	where Order_Type ne 1;
run;

title;
***********************************************************;

PROC FREQ data=orion.qtr2_2011 nlevels;
	tables Order_ID Order_Type;
run;

***********************************************************;

proc print data=orion.sales;
run;

proc means data=orion.sales;
	var Salary;
	class Gender Country;
run;

proc means data=orion.nonsales2;
run;

proc univariate data=orion.nonsales2 nextrobs=3;
	var Salary;
	id Employee_ID;
run;

*************************************************************;

proc format ;
	value ordertypes 1='Retail' 2='Catalog' 3='Internet';
run;

title 'Revenue from All Orders';

proc means data=orion.order_fact sum;
	var Total_Retail_Price;
	class Order_Date Order_Type;
	format Order_Type ORDERTYPES. Order_Date YEAR4.;
run;

title;
title 'Number of Missing and Non-Missing 
      Date Values';

proc means data=orion.staff nmiss n nonobs;
	var Birth_Date Emp_Hire_Date Emp_Term_Date;
	class Gender;
run;

title;
***********************************************************;

proc means data=orion.order_fact;
	class Product_ID;
	var Total_Retail_Price;
	output out=product_orders sum=Product_Revenue;
run;

data product_names;
	merge product_orders orion.product_list;
	By Product_ID;
	Keep Product_ID Product_Name Product_Revenue;
run;

Proc sort data=product_names;
	by descending Product_Revenue;
run;

title 'Top Five Products by Revenue';

proc print data=product_names(obs=5) label;
	var Product_Revenue Product_ID Product_Name;
	label Product_ID='Product Number' Product_Name='Product' 
		Product_Revenue='Revenue';
	format Product_Revenue eurox12.2;
run;

title;

proc univariate data=orion.price_current;
run;

*************************************************************;

/* SAS Programming 2

 *************************************************************

Lesson 1 */

%let path=/folders/myfolders/ecprg293;
libname orion "&path";

proc print data=orion.growth (obs=20);
run;

data forecast;
	set orion.growth;
	Year=1;
	Total_Employees=Total_Employees*(1+Increase);
	output;
	Year=2;
	Total_Employees=Total_Employees*(1+Increase);
	output;
run;

proc print data=forecast noobs;
	var Department Total_Employees Year;
run;
