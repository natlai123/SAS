
/****************************************************************************/

	/*Base SAS code: SAS Programming 1*/

/****************************************************************************/

/*

This note contains practice codes from SAS Programming 1
It works best and is based on the lessons on SAS Programming 1 offered by 
the SAS Institute and Base SAS certification textbook

Content: 

Example 1: SAS Options
Example 2: proc contents
Example 3: proc print
Example 4: PROC SORT + BY
Example 5: SAS FORMAT 
Example 6: Data Step


*/


/* Initialization */

libname orion "/folders/myshortcuts/SAS/SAS_Programming/ecprg193";
libname orion clear; * Clear the libname;

libname orion "/folders/myshortcuts/SAS/SAS_Programming/ecprg193";


/****************************************************************************/

	/* SAS Programming 1 */

/****************************************************************************/

/*
options nonumber nodate;
options date;
options number pageno=3;
option pagesize=15;
option linesize=64;
options datastmtchk = allkeywords;
*/

/****************************************************************************/

	/* Example 1: SAS Options */
	
/****************************************************************************/

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
*remember when you use a WHERE statement in the DATA step, the WHERE 
expression must reference only variables from the input data set.;
run;

* Specifies the first year of a 100-year span that is used by date informats 
and functions to read a two-digit year.;
options yearcutoff=1925 firstobs=4;
proc print data = countries1;

proc optload data=optionsave;
run;

* Check the difference in the output due to the change in options set by the proc option;
proc print data = countries;run;
proc print data = countries1;run;



/****************************************************************************/

	/* Example 2: proc contents */
	
/****************************************************************************/


proc contents data=orion._all_ nods;
run;
*<libref.>_ALL_ requests a listing of all files in the library. (Use a period (.) to append 
the key word _ALL_ to the libref.) You can specify NODS only when you specify _ALL_ ;

* "nods" stands for "no details". It is a keyword to suppress the descriptor data for each 
individual file in the library. Without it, SAS produces a long list of output as in: ;

proc contents data=orion._all_;
run;


proc datasets;
 contents data= orion.order;
quit;
*is the same as;
proc contents data= orion.orders ;
run;

proc contents data= orion.orders position;
run; 

* "position" generates the "Variables in Creation Order" table;

proc contents data= orion.orders varnum; 
run;

* "VARNUM" returns the number of a variable's position in a SAS data set;

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


/****************************************************************************/

	/* Example 3: proc print */

/****************************************************************************/

proc print data=orion.sales noobs label split=' ';
	label Job_Title = "Job Title"; 
	var Employee_ID First_Name Last_Name Job_Title;
	where First_Name like 'T_m_%' and Job_Title contains "Sales Rep.";
	format First_Name $midcase. 
	       Last_Name $upcase. 
           Job_Title $quote25.;
run;

/* 
 	Special WHERE Operators
BETWEEN - AND
WHERE SAME AND
IS NULL
IS MISSING
LIKE

% any number of characters
_ one character

where Name like '%N'
where Name like 'T_m%'

*/



/****************************************************************************/

	/* Example 4: PROC SORT + BY */

/****************************************************************************/

proc sort data=orion.orders out=work.custorders; 
	by Customer_ID;
run;

* NOTE: The data set WORK.CUSTORDERS has 490 observations and 6 variables.;

proc sort data=orion.orders out=work.custorders nodupkey;
	by Customer_ID;
run;

* NOTE: 415 observations with duplicate key values were deleted.
  NOTE: The data set WORK.CUSTORDERS has 75 observations and 6 variables.;

* The NODUPKEY option deletes observations with duplicate specified in the BY values.;

* PROC SORT replaces the original data set unless you specify an output data set in 
 the OUT= option.;


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

* To affect any single file, you can use FIRSTOBS= or OBS= as data set options instead
 of as system options. Check the second output begins with Obs = 3;

* Without the out= option, proc sort reorders the original dataset;

* Subsetting in the PROC SORT step is more efficient. It selects and sorts only the 
 required observations.;
 
* With more than one variable, specify the keyword DESCENDING before each variable.
 Customer ID=70201 becomes the first obs ... Customer ID=4 the last;

*The NODUPKEY option checks for and eliminates observations with duplicate BY 
variable values. If you specify this option, PROC SORT compares all BY variable 
values for each observation to those for the previous observation written to 
the output data set. If an exact match using the BY variable values is found, 
the observation is not written to the output data set The DUPOUT= option can be
used only with the NODUPKEY option.;


proc sort data = orion.orders out = work.custorders nodupkey dupout=work.duplicates;
	by Customer_ID;
run;

proc print data=work.duplicates;
run;


proc sort data=orion.sales          
          out=work.sales2;   
   by Country descending Salary;
run;

proc print data=work.sales2;
run;



/****************************************************************************/

	/* Example 5: SAS FORMAT */

/****************************************************************************/

	/* SAS Format */
	
proc print data=orion.sales label noobs;
   where Country = 'AU' and 
         Job_Title contains 'Rep';
   label Job_Title = 'Sales Title'
         Hire_Date ='Date Hired';
   format Hire_Date mmddyy10. Salary dollar8.; 
   var Last_Name First_Name Country Job_Title
       Salary Hire_Date;
run;

title1 'US Sales Employees';
title2 'Earning Under $26,000';

proc print data=orion.sales label noobs;
   where Country ='US' and 
   	     Salary < 26000;
   label Job_Title='Title'
         Hire_Date='Date Hired'
         Salary = 'Salary'
         First_Name = 'First Name'
         Last_Name = 'Last Name';
   format Hire_Date monyy7. Salary dollar10.;
   var Employee_ID First_Name Last_Name Job_Title
       Salary Hire_Date;   
run;
title; 


	/* User-defined Format: proc format statement*/

proc format;
   value $ctryfmt 'AU'='Australia'
                  'US'='United States'
                  other='Miscoded';
   value $sports
         'FB'='Football'
         'BK'='Basketball'
         'BS'='Baseball';
run;

proc print data=orion.sales label ;
   var Employee_ID Job_Title Salary 
       Country Birth_Date Hire_Date;
   label Employee_ID = 'Sales ID'
         Job_Title='Job Title'
         Salary = 'Annual Salary'
         Birth_Date = 'Date of Birth'
         Hire_Date = 'Date of Hire';
   format Salary dollar10. 
          Birth_Date Hire_Date monyy7.
          Country $ctryfmt.;
run;

proc format;
   value tiers low-<50000 = 'Tier1'
               50000-<100000 = 'Tier2'
               100000-high = 'Tier3';
run;

proc print data=orion.sales;
   var Employee_ID Job_Title Salary 
       Country Birth_Date Hire_Date;
   format Birth_Date Hire_Date monyy7.
          Salary tiers.;
run;


/* To exclude the first value in a range, you put the less-than symbol after 
the first value (<-)3. To exclude the last value in a range, you put the less-than 
symbol before the last value (-<). And, to exclude both the first and last values, 
you put a less-than symbol in both places (<-<). For continuous range, use keywords.
'low', 'high'. For numerical values, 'low' does not include missing values. For character
values, it does. Multiple values could be included in proc format, as in:
 
proc format;
   value $ctryfmt  'AU'='Australia'
                   'US'='United States'
                   other='Miscoded';
   value tiers     low-<50000 ='Tier1'
                   50000-<100000='Tier2'
                   100000-high  ='Tier3';
   run;

and the example below
*/

data q1birthdays;
   set orion.employee_payroll;
   BirthMonth=month(Birth_Date);
   if BirthMonth le 3;
run;

proc format;
	value $Gender 
		'F' = 'Female'
		'M' = 'Memale'
		other = 'Invalid code'; 
	value MNAME 
	     1 = 'January'
		 2 = 'February'
	     3 = 'March';
   value salrange 20000-<100000= 'Below $100,000'
         100000-500000='$100,000 or more'
         .='Missing salary'
         other='Invalid salary';	     
run;

title 'Employees with Birthdays in Q1';
proc print data=q1birthdays;
   var Employee_ID Employee_Gender Salary
       BirthMonth;
	format Employee_Gender $Gender. 
		   BirthMonth MNAME.  
		   Salary salrange.;		   
run;




/****************************************************************************/

	/* Example 6: Data Step */

/****************************************************************************/

libname orion "/folders/myshortcuts/SAS/SAS_Programming/ecprg193";

* The WHERE Assignment Statement, 
  SAS date constants, 
  drop keep statement, 
  label and format can be used in proc (temperary) or data step (pernmanently)
  WHERE vs subsetting IF statement (no special operator used in where statement)
  
*When the expression is false, SAS excludes the observation from 
 the output data set and continues processing
 	 
* Remember, the WHERE statement subsets data as SAS reads the data into the PDV;

* SAS processes the DATA step in two phases: the compilation phase and the execution phase.;

/*Example 6.1*/

data work.subset1;
   set orion.sales;
   where Country='AU' and
         Job_Title contains 'Rep';
   Bonus=Salary*.10;
   if Bonus > 2400;
   label Job_Title='Sales Title'
         Hire_Date='Date Hired';
   drop Employee_ID Gender Country
        Birth_Date;
run;

proc print data=work.subset1 noobs;
   var First_name Last_Name Salary 
       Job_Title Bonus Hire_Date;
   format Hire_Date date9.;
run;


/*Example 6.2*/

data work.tony;
   set orion.customer_dim;
   where Customer_FirstName=*'Tony';	/*Sound like Tony*/
run;

proc print data=work.tony;
   var Customer_FirstName Customer_LastName;
run;


/*Example 6.3*/

data work.increase;
   set orion.staff;
   where Emp_Hire_Date>='01JUL2010'd;
   Increase=Salary*0.10;
   if Increase>3000;
   NewSalary=Salary+Increase;
   label Employee_ID='Employee ID'
         Emp_Hire_Date='Hire Date'
         NewSalary='New Annual Salary';
   format Salary NewSalary dollar10.2 
          Increase comma5.;
   keep Employee_ID Emp_Hire_Date Salary 
        Increase NewSalary;
run;

proc contents data=work.increase;
run;

proc print data=work.increase split=' ';
run;


/*Example 6.4*/

* N function holds the count of nonmissing values ;

data work.bigdonations;
   set orion.employee_donations;
   Total=sum(Qtr1,Qtr2,Qtr3,Qtr4);
   NumQtrs=n(Qtr1,Qtr2,Qtr3,Qtr4);
   drop Recipients Paid_By;
   label Qtr1='First Quarter'
         Qtr2='Second Quarter'
         Qtr3='Third Quarter'
         Qtr4='Fourth Quarter';
   if Total < 50 or NumQtrs < 4 then delete;
run;

proc contents data=work.bigdonations;
run;

proc print data=work.bigdonations label noobs;
run;





*******************************************************;

/** Reading Raw Data File **/

/* 
 
list input		standard and/or nonstandard		separated by delimiter
column input	standard						in columns
formatted input	standard and/or nonstandard		in columns


Standard Data	Nonstandard Data

58				(23)
67.23			5,823
5.67E5			$67.23
-23				01/12/2010
00.99			12May2009
1.2E-2


DATA output-SAS-data-set;
        INFILE 'raw-data-file-name';
        INPUT specifications;
RUN; 

Compilation Phrase
	Input Buffer (only when reading a eaw data file)
	PDV _N_ _ERROR_ 
	Descriptor Portion
Execution Phrase
	Write Data from PDV to output 
Repeat


*/	 

%let path=/folders/myfolders/SAS_Programming/ecprg193;
libname orion "&path";

/* Length Statment */
/*The LENGTH statement must precede the INPUT statement to correctly set the length of the variable.*/         

data work.sales2;
   length Employee_ID  8 First_Name $ 12
          Last_Name $ 18 Gender $ 1 
          Salary  8 Job_Title $ 25
          Country $ 2;
   infile "&path/sales.csv" dlm=',';
   input Employee_ID First_Name $ Last_Name $ 
         Gender $ Salary Job_Title $ Country $; 
run;

proc contents data=work.sales2 varnum;
run;

proc print data=work.sales2;
run;


/** Modifed List (informat) for Reading Nonstandard Delimited Data **/

/*

Informat	Raw Data Value	SAS Data Value

COMMA.
DOLLAR.		$12,345			12345
COMMAX.
DOLLARX.	$12.345			12345
EUROX.		€12.345			12345
$CHAR.		##Australia		##Australia
$UPCASE.	au				AU

A SAS informat provides instructions for reading nonstandard values.
			   provides a length for character variables.
               must include a period.
    	       controls the way nonstandard data values are stored in a SAS data set.

*/

data work.subset;
   infile "&path/sales.csv" dlm=',';
   input Employee_ID First_Name :$12. 
         Last_Name :$18. Gender :$1. Salary
         Job_Title :$25. Country :$2.
         Birth_Date :date. Hire_Date :mmddyy.;
   if Country='AU';
   keep First_Name Last_Name Salary 
        Job_Title Hire_Date;
   label Job_Title='Sales Title'
         Hire_Date='Date Hired';
   format Salary dollar12. Hire_Date monyy7.;
run;

proc print data=work.subset label;
run;


/** DATALINES Statement **/

data work.newemps;
   input First_Name $ Last_Name $  
         Job_Title $ Salary :dollar8.;
   datalines;
Steven Worton Auditor $40,450
Merle Hieds Trainee $24,025
Marta Bamberger Manager $32,000
;

proc print data=work.newemps;
run;

data work.newemps2;
   infile datalines dlm=',';
   input First_Name $ Last_Name $
         Job_Title $ Salary :dollar8.;
   datalines;
Steven,Worton,Auditor,$40450
Merle,Hieds,Trainee,$24025
Marta,Bamberger,Manager,$32000
;

proc print data=work.newemps2;
run; 


data work.canada_customers;
   length First Last $ 20 Gender $ 1 
          AgeGroup $ 12;
   infile "&path/custca.csv" dlm=',';
   input First $ Last $ ID Gender $ 
         BirthDate :ddmmyy. Age AgeGroup $;
   format BirthDate monyy7.;
   drop ID Age;
run;

proc contents data = work.canada_customers;
run;

title 'Canadian Customers';
proc print data=work.canada_customers;
run;
title;


/** dsd missover options **/
data work.contacts;
   length Name $ 20 Phone Mobile $ 14;
   infile "&path/phone2.csv" dsd;
   input Name $ Phone $ Mobile $;
run;

proc print data=work.contacts noobs;
run;

data work.contacts2;
   length Name $ 20 Phone Mobile $ 14;
   infile "&path/phone.csv" dlm=',' missover; 
   input Name $ Phone $ Mobile $;
run;

proc print data=contacts2 noobs;
run;



data work.prices;
infile "&path/prices.dat" dlm='*' missover;
input ProductID StartDate :date. EndDate :date. 
	  UnitCostPrice :dollar. UnitSalesPrice :dollar.;
label ProductID ='Product ID'
      StartDate ='Start of Date Range'
      EndDate ='End of Date Range'
      UnitCostPrice ='Cost Price per Unit'
      UnitSalesPrice ='Sales Price per Unit';	  
format StartDate EndDate mmddyy10.
      UnitCostPrice UnitSalesPrice 8.2;
run;

title '2007 Prices';
proc print data = work.prices label;
run;
title;


/** Creating  variables using function **/

libname orion "/folders/myfolders/SAS_Programming/ecprg193";

data work.comp;
   set orion.sales;
   Bonus=500;
   Compensation=sum(Salary,Bonus);
   BonusMonth=month(Hire_Date);
run;

proc print data=work.comp;
   var Employee_ID First_Name Last_Name 
       Salary Bonus Compensation BonusMonth;
run;

/*The DROP statement is a compile-time-only statement. SAS sets a drop flag for 
the dropped variables, but the variables are in the PDV and, therefore, are 
available for processing.*/

data work.birthday;
   set orion.customer;
   Bday2012 = mdy(month(Birth_Date), day(Birth_Date), 2012);
   BdayDOW2012=weekday(Bday2012);
   Age2012=(Bday2012-Birth_Date)/365.25;
   keep Customer_Name Birth_Date Bday2012 BdayDOW2012 Age2012 ;
   format Bday2012 date9. age2012 3.;
run;

proc print data=work.birthday label;
run;


data work.employees;
   set orion.sales;
   FullName=catx(' ',First_Name,Last_Name);
   Yrs2012=intck('year',Hire_Date,'01JAN2012'd);
   format Hire_Date ddmmyy10.;
   label Yrs2012='Years of Employment in 2012';
run; 

proc print data=work.employees label;
   var FullName Hire_Date Yrs2012;
run;


/** If then statment**/

data work.comp;
   set orion.sales;
   if Job_Title='Sales Rep. III' or
      Job_Title='Sales Rep. IV' then
      Bonus=1000;
   else if Job_Title='Sales Manager' then
     Bonus=1500;
   else if Job_Title='Senior Sales Manager'
     then Bonus=2000;
   else if Job_Title='Chief Sales Officer'
     then Bonus=2500;
   else Bonus=500;
run;
 
proc print data=work.comp;
   var Last_Name Job_Title Bonus;
run;


/* Do Group*/

data work.bonus;
   set orion.sales;
   length Freq $12;
   if Country= upcase('US') then 
      do;
         Bonus=500;
         Freq='Once a Year';
      end;
   else do;
          Bonus=300;
          Freq='Twice a Year';
      end;
run;

proc print data=work.bonus;
   var First_Name Last_Name Country 
       Bonus Freq;
run;


data work.region;
   set orion.supplier;
length region $17;   
If Country in ('CA', 'US') then do
	Discount = 0.10;
	DiscountType = 'Required';	
	Region = 'North America';
end;
else do ;
	Discount = 0.05;
	DiscountType = 'Optional';
	Region = 'Not North America';
end;	
Keep Supplier_Name Country Discount DiscountType Region;
run;

proc print data=work.region;
run;

data work.gifts;
   set orion.nonsales;
   length Gift1 $ 6 Gift2 $ 10;
   select(Gender);
      when('F') do;
         Gift1='Scarf';
         Gift2='Pedometer';
      end;
      when('M') do;
         Gift1='Gloves';
         Gift2='Money Clip';
      end;
      otherwise do;
         Gift1='Coffee';
         Gift2='Calendar';
      end;
      end;
    keep Employee_ID First Last 
         Gender Gift1 Gift2;      
run;

proc print data=work.gifts;
run;



