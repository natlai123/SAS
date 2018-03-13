* SAS Programming 1

SAS Programming Lesson 11: Creating Summary Reports;

* ods trace ;
* proc export;

*******************************************************;

********** Match-Merge with Non-Matches **********;

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

title 'Set a Set b';
proc print data=empsauc;
run;
title;

data empsauc1;
	merge empsau(in=Emps) phonec(in=Cell);
	by EmpID;
run;

title 'Merge...by';
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

*****************************;

libname orion "/folders/myfolders/SAS_Programming/ecprg193";

proc sort data=orion.employee_payroll
          out=work.payroll;
   by Employee_ID;
run;

proc sort data=orion.employee_addresses
          out=work.addresses;
   by Employee_ID;
run;

proc print data = payroll (obs=8); run;
proc print data = addresses (obs=8); run;

data work.payadd;
merge payroll (In=p)
	  addresses (In=a);
	  if a and p;
by Employee_ID;
run;

proc print data=work.payadd;
   var Employee_ID Employee_Name Birth_Date Salary;
   format Birth_Date weekdate.;
run;
	  
proc export 
  data=work.payadd
  dbms=xlsx 
  outfile="/folders/myfolders/SAS_Output/payadd.xlsx" 
  replace;
run;	  
	
	
proc sort data = orion.customer 
		  out = work.customer_sort;
by country;
run;	  
	  
data work.allcustomer;
merge work.customer_sort (in = cs)
	  orion.lookup_country (rename=(Start = Country Label=Country_Name) in = lc);
by Country;
keep Customer_ID Country Customer_Name Country_Name;
if cs=1 and lc=1;
run;	  

proc print data=work.allcustomer;
run;


proc sort data = orion.orders 
		out = work.orders_sort;
by Employee_ID;
run;

data work.allorders work.noorders;
merge orion.staff (in=s)
	  work.orders_sort (in=o);
by Employee_ID;
if o = 1 then output work.allorders;
else if s = 1 and o = 0 then output work.noorders;
keep Employee_ID Job_Title Gender Order_ID Order_Type Order_Date;
run;

title "work.allorders Data Set";
proc print data=work.allorders;
run;
title "work.noorders Data Set";
proc print data=work.noorders;
run;
title;


*******************************************;	  
	  
*Proc Freq;
/*Whenever using by statement, USE PROC SORT FIRST !!!*/

libname orion "/folders/myfolders/SAS_Programming/ecprg193";

proc format;
   value tiers low-25000='Tier1'
               25000<-50000='Tier2'
               50000<-100000='Tier3'
               100000<-high='Tier4';
run; 

proc freq data=orion.sales;
   tables Salary;
   format Salary tiers.;
run;

/* proc freq + by */
/**  Add options after "/"   **/

proc sort data=orion.sales out=sorted;
   by Country;
run;

proc freq data=sorted;
   tables Gender; 
   by Country;
run;

proc freq data=orion.sales;
   tables Gender*Country;
   tables Gender*Country / list;
   tables Gender*Country / crosslist;
run;

/* To change the format that SAS applies, you can add another option to the TABLES statement, 
the FORMAT= option. This option allows you to format the frequency value and to change the 
width of the column. In the FORMAT= option, you can specify any standard SAS numeric format 
or a user-defined numeric format. The format length cannot exceed 24. The FORMAT= option applies 
only to crosstabulation tables displayed in the default format. It doesn't apply to 
crosstabulation tables produced with the LIST or CROSSLIST option.*/

proc freq data=orion.sales;
   tables Gender*Country/
          format=24.; 
   format Country $ctryfmt.;
run;


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

/* ORDER=FREQ option displays results in descending frequency order */
/* Notice that the Employee_ID 120108 has a frequency of 2, and you can easily 
	find it because it's listed first.  
NLEVELS displays a table of the distinct values, or levels, for each variable in the TABLES statement
NOPRINT to only see the Number of Variable Levels table and not the individual frequency tables */


/*Examining Your Data*/
proc print data=orion.nonsales2;
	where Gender not in ('F', 'M') or Job_Title is null or Country not in ('AU', 
		'US') or Salary not between 24000 and 500000 or Employee_ID is missing or 
		Employee_ID=120108;
run;


proc format;
   value ordertypes
         1='Retail'
         2='Catalog'
         3='Internet';
run;

title 'Order Summary by Year and Type';
proc freq data=orion.orders;
tables Order_Date YEAR4.;   
table order_type / nocum nopercent;
table order_date*order_type / nocol norow nopercent ;
format  Order_Date year4. order_id ordertypes. ; 
run;
title;

/* OUT = Option */
proc freq data=orion.order_fact noprint;
   tables Product_ID/out=product_orders;
run;

data  product_names ;
 merge product_orders orion.product_list;
   by Product_ID;
   keep Product_ID Product_Name Count;
run;

proc sort data = product_names;
by descending count;
run;

title 'Top Five Products by Number of Orders';
proc print data=product_names(obs=5) label 
           noobs;
   var Count Product_ID Product_Name;
   label Product_ID='Product Number'
         Product_Name='Product'
         Count='Orders';
run;
title;

proc export data = product_names
  dbms=xlsx
  outfile="/folders/myfolders/SAS_Output/product_names.xlsx" 
  replace;
run;	


/*******************************************************/

libname orion "/folders/myfolders/SAS_Programming/ecprg193";

data work.contacts2;
	infile "/folders/myfolders/SAS_Programming/ecprg193/phone.csv" dlm=',' missover;
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

/*******************************************************/

/** Proc Mean **/

*We can suppress the creation of the record with
 the overall mean with the nway option on the proc 
 means statement.;
 
proc means data=orion.nonsales2 n nmiss min max;
   var Salary;
run; 


proc means data=orion.order_fact n nmiss nway;
	class Product_ID;
	var Total_Retail_Price;
	output out=product_orders sum=Product_Revenue;
run;

/*N = number of observations with nonmissing values
NMISS = number of observations with missing values*/



ods trace on;
ods select ExtremeObs;

proc univariate data=orion.shoes_tracker;
	var Product_ID;
run;

ods trace off;
/*ODS csvall include titles:
   The MEANS Procedure*/
ods csv file="/folders/myfolders/SAS_Output/salaries.csv" ;


proc means data=orion.sales;
	var Salary;
	class Gender Country;
run;

ods csv close;

ods html file="/folders/myfolders/SAS_Output/salaries.html";

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


proc means data=orion.order_fact sum;
   var Total_Retail_Price;
   class Order_Date Order_Type;
   format Order_Date year4. 
          Order_Type ordertypes.;
run;


title 'Number of Missing and Non-Missing 
      Date Values';
proc means data=orion.staff nmiss n;
   var Birth_Date Emp_Hire_Date Emp_Term_Date;
   *class Gender;
run;
title;

title 'Number of Missing and Non-Missing 
      Date Values';
proc means data=orion.staff nmiss n;
   var Birth_Date Emp_Hire_Date Emp_Term_Date;
   class Gender;
run;
title;



libname orion "/folders/myfolders/SAS_Programming/ecprg193";

proc sort data = orion.sales out= salesss;
by Gender;
run;

proc means data=salesss;
	var Salary;
	class Gender;
	id gender;
run;



proc means data=orion.order_fact noprint nway;
   class Product_ID;
   var Total_Retail_Price;
   output out=product_orders 
          sum=Product_Revenue;
run;


proc means data=orion.order_fact;
	class Product_ID;
	var Total_Retail_Price;
	output out=product_orders 
		   sum=Product_Revenue;
run;

data product_names;
	merge product_orders orion.product_list;
	By Product_ID;
	Keep Product_ID Product_Name Product_Revenue;
run;

proc sort data=product_names;
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



