
*************************************************************;

/* SAS Programming 2

 *************************************************************

Lesson 1 */

libname orion "/folders/myfolders/SAS_Programming/ecprg293";


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


/*Conditional Statement*/
/*Summing over rows*/
/*Reading the Raw Data*/
/*Manipulating Character Values*/ 

data work.extended;
	set orion.discount;
	where Start_Date = '01Dec2011'd;
	drop Unit_Sales_Price;
	Promotion = 'Happy Holidays';
	if month(Start_Date) = 12 then
		Seasons = 'Winter';
	output;	
	Start_Date = '01Jul2012'd;
	End_Date = '31Jul2012'd;
	if month(Start_Date) = 7 then 
		Seasons = 'Summer';
	output;
run;

title 'All discount ranges with the Happy
       Holidays promotion';
proc print data=work.extended;
run;
title;

proc export 
  data=work.extended
  dbms=xlsx 
  outfile="/folders/myfolders/SAS_Output/extended.xlsx" 
  replace;
run;	

*************************************************************;

/*Conditional Statement*/

/*
Using ELSE statements with IF-THEN statements can save resources:

• Using IF-THEN statements without the ELSE statement causes SAS to evaluate all
IF-THEN statements.

• Using IF-THEN statements with the ELSE statement causes SAS to execute IF THEN
statements until it encounters the first true statement. Subsequent IF-THEN
statements are not evaluated.

For greater efficiency, construct your IF-THEN/ELSE statements with conditions of
decreasing probability.

You can use a LENGTH statement to specify a length (the number of bytes) for
TestLength before the first value is referenced elsewhere in the DATA step.
General form, LENGTH statement:
LENGTH variable(s) <$> length;
where
• variable(s) names the variable(s) to be assigned a length
• $ is specified if the variable is a character variable
• length is an integer that specifies the length of the variable.

The DROP and KEEP statements differ from the DROP= and
KEEP data set options in the following ways:

• You cannot use the DROP and KEEP statements in SAS procedure steps.
• The DROP and KEEP statements apply to all output data sets that are named in the
DATA statement. To exclude variables from some data sets but not from others, use
the DROP= and KEEP= data set options in the DATA statement.

If you don't specify a select-expression, SAS evaluates each when-expression to produce
a result of true or false.

• If the result is true, SAS executes the statement in the WHEN statement.

• If the result is false, SAS proceeds either to the next when-expression in the current
WHEN statement, or to the next WHEN statement if no more expressions are
present, or to the OTHERWISE statement if one is present. (That is, SAS performs
the action that is indicated in the first true WHEN statement.)

If more than one WHEN statement has a true when-expression, only the first WHEN
statement is used. Once a when-expression is true, no other when-expressions are
evaluated.

If the result of all when-expressions is false and no OTHERWISE statement is
present, SAS issues an error message.

In the example below, the SELECT statement does not specify a select-expression. The
WHEN statements are evaluated in order, and only one is used. For example, if the value
of toy is Bear and the value of month is FEB, only the second WHEN statement is used,
even though the condition in the third WHEN statement is also met. In this case, the
variable price is assigned the value 25.00:

select;
when (toy="Bear" and month in ('OCT', 'NOV', 'DEC')) price=45.00;
when (toy="Bear" and month in ('JAN', 'FEB')) price=25.00;
when (toy="Bear") price=35.00;
otherwise;
end;

If the result of all SELECT-WHEN comparisons is false and no OTHERWISE
statement is present, SAS issues an error message and stops executing the DATA step.

obs = option (specify the last observation to be processed)
firstobs = (specify the first observation to be processed)
can be used in the set and infile statement 

*/

data usa australia other;
	set orion.employee_addresses;
	Country=upcase(country);
	if Country='AU' then
		output australia;
	else if Country='US' then
		output usa;
	else
		output other;
run;

title 'Employees in the United States';
proc print data=usa (obs = 4 firstobs = 4);
run;

title 'Employees in Australia';
proc print data=australia;
run;

title 'Non US and AU Employees (wrongly-coded)';
proc print data=other;
run;
title;

*************************************************************;


/*Filtering using upcase function*/

/*Example 1*/

data work.salesinfo (keep=Employee_ID Job_Title Manager_ID) 
		work.execinfo (keep=Employee_ID Job_Title);
	set orion.employee_organization;
	if Department='Sales' then
		output work.salesinfo;
	else if Department='Executives' then
		output work.execinfo;
run;

/*Example 2*/

data usa australia other;
	set orion.employee_addresses;
	select (upcase(Country));          /***/
		when ('US') output usa;
		when ('AU') output australia;
		otherwise;
	end;
run;

/*Example 3*/

data work.admin work.stock work.purchasing;
	set orion.employee_organization;
	if Department='Administration' then
		output work.admin;
	else if Department='Stock & Shipping' then
		output work.stock;
	else if Department='Purchasing' then
		output work.purchasing;
run;

data work.admin work.stock work.purchasing;
	set orion.employee_organization;
	select;
		when (Department = 'Administration') output work.admin;
		when (Department = 'Stock & Shipping') otput work.stock;
		when (Department = 'Purchasing') output work.purchasing;
		otherwise; *output other;
	end;
run;

title 'Administration Employees';
proc print data=work.admin;
run;
title;

title 'Stock and Shipping Employees';
proc print data=work.stock;
run;
title;

title 'Purchasing Employees';
proc print data=work.purchasing;
run;
title;

*************************************************************;

/*Sum Statement*/

/*The RETAIN statement is a compile-time-only statement that prevents SAS from
reinitializing the variable at the top of the DATA step. Because the variable is
not reinitialized, it retains its value across multiple iterations of the DATA step.*/

/*Example 1*/

libname orion "/folders/myfolders/SAS_Programming/ecprg293";

data mid_q4;
	set orion.order_fact;
	where '01Nov2008'd <=Order_Date <='14Dec2008'd;
	*retain Sales2Dte Num_Orders 0 ;
	Sales2Dte+Total_Retail_Price;
	Num_Orders+1;
run;

proc print data=mid_q4;
	format Sales2Dte DOLLAR10.2;
	var Order_ID Order_Date Total_Retail_Price Sales2Dte Num_Orders;
run;

data mid_q4;
	set orion.order_fact;
	where '01Nov2008'd <=Order_Date <='14Dec2008'd;
	retain Sales2Dte 0;
	retain Num_Orders 0;
	Sales2Dte = Sales2Dte+Total_Retail_Price;
	Num_Orders = Num_Orders+1;
run;

proc print data=mid_q4;
	format Sales2Dte DOLLAR10.2;
	var Order_ID Order_Date Total_Retail_Price Sales2Dte Num_Orders;
run;


/*Example 2*/

libname orion "/folders/myfolders/SAS_Programming/ecprg293";

data typetotals1;
	set orion.order_fact;
	where year(Order_Date)=2009;
	if Order_Type=1 then
		TotalRetail + Quantity;
	else if Order_Type=2 then
		TotalCatalog + Quantity;
	else if Order_Type=3 then
		TotalInternet + Quantity;
	keep Order_ID Order_Type Order_Date Quantity TotalRetail TotalCatalog 
		TotalInternet;
run;

title '2009 Accumulating Totals Broken Out by Type of Order1';
proc print data=typetotals1 (obs = 10);
run;
title;

data typetotals2 (keep=Order_ID Order_Type Order_Date Quantity TotalRetail 
		TotalCatalog TotalInternet);
	set orion.order_fact;
	where year(Order_Date)=2009;
	select (Order_Type);
		when (1) TotalRetail + Quantity;
		when (2) TotalCatalog + Quantity;
		when (3) TotalInternet + Quantity;
		otherwise put "Check unknown Order_Type: " Order_Type=;
	end;
run;

title '2009 Accumulating Totals Broken Out by Type of Order2';
proc print data=typetotals2 (obs = 10);
run;
title;

/*If the result of all SELECT-WHEN comparisons is false and no OTHERWISE
statement is present, SAS issues an error message and stops executing the DATA step.*/

*************************************************************;

/*Summing over rows*/

/*You must set the accumulating variable to zero at the start of each BY group so that 
the summarized value does not consist of the accumulated total from the previous BY-group.
By default, a BY statement creates two temporary variables for each BY variable listed. 
These variables identify the first and last observation in each BY group. The FIRST. variable 
has a value of 1 for the first observation in a BY group; otherwise, it equals 0. The LAST.
variable has a value of 1 for the last observation in a BY group; otherwise, it equals 0.*/

/*Example 1*/

libname orion "/folders/myfolders/SAS_Programming/ecprg293";

proc sort data=orion.specialsals out=salsort;
	by Dept;
run;

data deptsals(keep=Dept DeptSal);
	set SalSort;
	by Dept;
	if First.Dept then
		DeptSal=0; 
		DeptSal+Salary;
	if Last.Dept;
run;

proc print data=deptsals;
	format DeptSal 7.;
	title 'Employee Salaries by Department';
run;
title;


/*Example 2*/

/*M1*/
proc sort data=orion.projsals out=projsort;  /*Sorting by Project and Dept*/
	by Proj Dept;
run;

proc print data = projsort;
run;

/*Extracting the summary report by project and dept (proj and dept are keys in the dataset)*/

data pdsals(keep=Proj Dept DeptSal NumEmps); 
	set projsort;
	by Proj Dept;
	if First.Dept then
		do;
			DeptSal=0;
			NumEmps=0;
		end;
	DeptSal+Salary;
	NumEmps+1;
	if Last.Dept; /**Dept not Proj**/
run;

proc print data=pdsals noobs;
run;

/*Remember that when you use more than one variable in the BY statement, 
a value of 1 for the primary variable forces a value of 1 for the secondary variable.
Remember that every time SAS reaches the last occurrence of a project name, the value of 
LAST.Proj is set to 1. This change forces a change for the value of LAST.Dept. This is 
why the subsetting IF statement is based on the LAST.Dept value.*/

/*M2 different from M1: Method from Little SAS book*/

PROC MEANS sum data=pdsals;
VAR DeptSal;
BY Proj;
OUTPUT OUT = summarydata SUM(DeptSal) = Total_DeptSal_Per_Proj;
RUN;

DATA projsummary (drop = _TYPE_	_FREQ_);
MERGE pdsals summarydata;
BY Proj;
Percent = DeptSal/Total_DeptSal_Per_Proj;
RUN;

PROC PRINT DATA = projsummary label split='*';
format Percent percent.2 Total_DeptSal_Per_Proj DeptSal dollar10.;
label Total_DeptSal_Per_Proj = 'Total Department*Salary Per Project';
run;


/*Practice*/

proc sort data=orion.order_qtrsum out=custsort;
	by Customer_ID Order_Qtr;
run;

proc print data=custsort;
run;

data qtrcustomers;
	set custsort;
	by Customer_ID Order_Qtr;
	if first.Order_Qtr then
		do;
			Total_Sales=0;
			Num_Months=0;
		end;
	Total_Sales + Sale_Amt;
	Num_Months + 1;
	keep Customer_ID Order_Qtr Total_Sales Num_Months;
	if last.Order_Qtr=1;
run;

data qtrcustomers1;
	set custsort;
	by Customer_ID Order_Qtr;
	if first.Order_Qtr then
		do;
			retain Total_Sales 0;
			retain Num_Months 0;
		end;
	Total_Sales=Total_Sales + Sale_Amt;
	Num_Months=Num_Months + 1;
	keep Customer_ID Order_Qtr Total_Sales Num_Months;
	if last.Order_Qtr=1;
run;

title 'Total Sales to Each Customer for Each Quarter';
proc print data=qtrcustomers;
	format Total_Sales DOLLAR11.2;
run;
title;

title 'Total Sales to Each Customer for Each Quarter';
proc print data=qtrcustomers1;
	format Total_Sales DOLLAR11.2;
run;
title;


*Level 2;

proc sort data=orion.usorders04 out=usorders04_sorted;
	by Customer_ID Order_Type;
run;

data Discount1 Discount2 Discount3;
	set usorders04_sorted;
	by Customer_ID Order_Type;
	if first.Order_Type=1 then TotSales=0;
	retain TotSales 0;
	TotSales=TotSales + (Total_Retail_Price*Quantity);
	keep Customer_ID Customer_Name TotSales;
	Format TotSales DOLLAR11.2;
	if last.Order_Type=1 and TotSales >=100 then
		do;
			select(Order_Type);
				when (1) output Discount1;
				when (2) output Discount2;
				when (3) output Discount3;
				otherwise ;
			end;
		end;
run;

title 'Customers Spending $100 or more in Retail Orders';

proc print data=discount1 noobs;
run;

title;
title 'Customers Spending $100 or more in Catalog Orders';

proc print data=discount2 noobs;
run;

title;

title 'Customers Spending $100 or more in Internet Orders';
proc print data=discount3 noobs;
run;
title;

*************************************************************;

/*Reading the Raw Data

Standard Data
Non-standard Data (special character) -> Formated Input

Data ...
infile ...
input ...

modified list (using : colon operator) 
column input -> standard data 
formated input -> nonstandard data

Input Control
   @n = absolute-pointer control (order does not matter)
   +n = relative-pointer control (order matters)
   / = relative line pointer => move the line pointer control forward  (order matters)
   #n = absolute line pointer => must specify n -> line 1 line 3 line 5 ... (order does not matter)
   @ ; = the single trailing @  is a line hold specifier 
   @@ = double trailing @ 
   input = advances the line pointer without reading values from the second record
   
 The single trailing @ holds a raw data record in the input buffer 
 until an INPUT statement without a trailing @ executes or the next 
 iteration of the DATA step begins.
 
 When you use a trailing @, the pointer position does not change and
 no new record is loaded into the input buffer. So, the next INPUT 
 statement for the same iteration of the DATA step continues to read 
 the same record.*/
   
/*Example 1*/   

libname orion "/folders/myfolders/SAS_Programming/ecprg293";

data work.discounts;
	infile "/folders/myfolders/SAS_Programming/ecprg293/offers.dat";
	input @1 Cust_type 4. 
          @5 Offer_dt mmddyy8.
          @14 Item_gp $8.
          @22 Discount percent3.;
run;

proc print data=work.discounts;
	format Offer_dt date9. Discount percent.;
run;


/*Example 2*/   

data salesstaff;
	infile "/folders/myfolders/SAS_Programming/ecprg293/sales1.dat";
	input @1 Employee_ID 6. @21 Last_Name $18. 
          @42 Job_Title $21. @64 Salary dollar8. 
          @87 Hire_Date mmddyy8.;
run;

title 'Australian and US Sales Staff';
proc print data=salesstaff;
format salary dollar8. hire_date mmddyy8.;
run;
title;
run;

libname orion "/folders/myfolders/SAS_Programming/ecprg293";

data au_trainees us_trainees;
drop country; /* "More Efficient" */
infile "/folders/myfolders/SAS_Programming/ecprg293/sales1.dat";
input @1 Employee_ID 6.
	  @73 Country $2.		/*If change @73 to @72 no output would be shown*/
	  @20 Last_Name $10.
	  @43 Job_Title $15.
	  @64 Salary dollar8.
	  @87 Hire_Date mmddyy10.; 
if Job_Title = 'Sales Rep. I';
 	if country =  'AU' then output au_trainees;
 	else if country = 'US' then output us_trainees;
run;

title 'Australian Trainees';
proc print data=au_trainees;
format Salary dollar8. Hire_Date mmddyy10.;
run;
title;

title 'US Trainees';
proc print data=us_trainees;
format Salary dollar8. Hire_Date mmddyy10.;
run;


/*Example 3: Creating a Single Observation from Multiple Records*/

data mycontacts;
	infile "/folders/myfolders/SAS_Programming/ecprg293/address.dat";
	input FullName $30.;
	input;
	input Address2 $25.;
	input Phone $8.;
run;

proc print data=mycontacts;
run;

data work.mycontacts;
	infile "/folders/myfolders/SAS_Programming/ecprg293/address.dat";
	input FullName $30. / 
	/ Address2 $25. /
	Phone $8.;
run;

proc print data=mycontacts;
run;

data work.mycontacts;
	infile "/folders/myfolders/SAS_Programming/ecprg293/address.dat";
	input #1 FullName $30. 
          #3 Address2 $25.  
          #4 Phone $8.;
run;

proc print data=mycontacts;
run;


/*Example 4*/   

data sales_staff2;
	infile "&path/sales2.dat ";
	input  @1 Employee_ID 6. 
	/*#2*/ @20 Last_Name $15. / 
	       @1 Job_Title $15. @22 Hire_Date mmddyy10.
           @33 Salary dollar8. /
   /*#3*/	;
run;

title 'Australian and US Sales Staff';
proc print data=sales_staff2;
run;
title;

libname orion "/folders/myfolders/SAS_Programming/ecprg293";

data salesQ1;
	infile "/folders/myfolders/SAS_Programming/ecprg293/sales.dat";
	input SaleID $4. @6 Location $3. @;	
/*The single trailing @ holds a raw data record in the input buffer 
 until an INPUT statement without a trailing @ executes or the next 
 iteration of the DATA step begins.
 
 When you use a trailing @, the pointer position does not change and
 no new record is loaded into the input buffer. So, the next INPUT 
 statement for the same iteration of the DATA step continues to read 
 the same record.*/
	if Location='USA' then do;
		input @10 SaleDate mmddyy10. @20 Amount 7.;
                 output;
                 end;
	else if Location='EUR' then
		input @10 SaleDate date9. @20 Amount commax7.;
run;

proc print data=work.salesQ1;
run;


/*Practice*/

data au_sales us_sales;
	infile "&path/sales3.dat";
	input @1 Employee_ID 6. @20 Last_Name $20. @43 Job_Title $21. / 
	      @10 Country $2. @;
	if Country=upcase('AU') then
		do;
			input @1 Salary commax7. @24 Hire_Date mmddyy10.;
			output au_sales;
		end;
	else if Country=upcase('US') then
		do;
			input @1 Salary comma7. @24 Hire_Date mmddyy10.;
			output us_sales;
		end;
run;

title 'Australian Sales Staff';
proc print data=au_sales;
run;

title 'US Sales Staff';
proc print data=us_sales;
run;
title;

*************************************************************;


/*Manipulating Character Values*/ 

%let path=/folders/myshortcuts/_myfolders/ecprg293;
libname orion "&path";

/**/

proc print data = orion.biz_list (obs = 10);
run;

data charities1(drop=Len);
	length ID $ 5;
	set orion.biz_list;
	Len=length(Acct_Code);
	if substr(Acct_Code, Len, 1)='2';
	ID=substr(Acct_Code, 1, Len-1);
	Name=propcase(name);
run;

proc print data=charities1;
run;


/*Practice*/

proc print data = orion.au_salesforce (obs = 10);
run;

data work.codes;
	set orion.au_salesforce;
	length FCode1 $ 1 FCode2 $ 1 LCode $ 4;
	rig=right(First_Name);
	FCode1=lowcase(char(First_Name, 1));
	FCode2=lowcase(substr(rig, 12, 1));
	LCode=lowcase(substr(Last_Name, 1, 4));
run;

title 'Extracted Letters for User IDs';
proc print data=work.codes;
	var First_Name FCode1 FCode2 Last_Name LCode;
run;
title;

/**/

proc print data = orion.newcompetitors (obs=10);
run;

data work.small;
set orion.newcompetitors;
	City=propcase(City);
	Store=left(substr(ID, 3));
if substr(Store, 1, 1)=1;
drop store;
run;
	
title 'New Small-Store Competitors';
proc print data=work.small noobs;
run;
title;


/**/

proc print data = orion.contacts;
run;

data labels;
	set orion.contacts;
	length FMName LName $ 15;
	FMName=scan(Name, 2, ',');
	* ', ' works as well;
	LName=scan(Name, 1, ',');
run;

proc print data=labels noobs;
run;

data labels;
	set orion.contacts;
	length FMName LName $ 15 FullName $ 50;
	FMName=scan(Name, 2, ',');
	LName=scan(Name, 1, ',');
	FullName=catx(' ', Title, FMName, LName);
run;

proc print data=labels noobs;
	var Fullname Address1 Address2;
run;


/**/

proc print data = orion.clean_up (obs =10);
run;

data correct1;
	set orion.clean_up;
		/*If I is not specified, FIND searches for the same case as the 
		characters specified in substring. 
		The modifier T trims trailing blanks from string and substring.*/	
	if find(Product, 'Mittens', 'I') > 0 then
	/*if index(Product, 'Mittens') would also work*/
		do;
			substr(Product_ID, 9, 1)='5';
	    end;
run;

proc print data=correct1;
run;

data correct2;
	set orion.clean_up;
	if find(Product, 'Mittens', 'I') > 0 then
		do;
			substr(Product_ID, 9, 1)='5';
			Product=tranwrd(Product, 'Luci ', 'Lucky ');
	    end;
	Product=propcase(Product);
	Product_ID=compress(Product_ID);
run;

proc print data=correct2;
run;


/**/

data names;
	set orion.customers_ex5;
	length FMName LName $ 30 New_Name $ 50;
	FMName=scan(Name, 2, ',');
	LName=propcase(scan(Name, 1, ','));
	if Gender='M' then
		New_Name=catx(' ', 'Mr.', fmname, lname);
	else if Gender='F' then
		New_Name=catx(' ', 'Ms.', fmname, lname);
	keep New_Name Name Gender;
run;

proc print data=names;
run;


data names;
	length New_Name $50 FMnames Last $30;
	set orion.customers_ex5;
	FMnames=scan(Name, 2, ',');
	Last=propcase(scan(Name, 1, ','));
	if Gender="F" then
		New_Name=CATX(' ', 'Ms.', FMNames, Last);
	else if Gender="M" then
		New_Name=CATX(' ', 'Mr.', FMNames, Last);
	keep New_Name Name Gender;
run;

proc print data=names;
run;


/**/

proc print data = orion.customers_ex5 (obs=10);
run;

data work.silver work.gold work.platinum work.other;
	set orion.customers_ex5;
	Customer_ID=tranwrd(Customer_ID, '-00-', '-15-');
	
	if find(Customer_ID, 'Silver', 'I') > 0 then
		do;
			substr(Customer_ID, 1, 6)='Silver';
			output work.silver;
		end;
	else if find(Customer_ID, 'Gold', 'I') > 0 then
		do;
			substr(Customer_ID, 1, 4)='Gold';
			output work.gold;
		end;
	else if find(Customer_ID, 'Platinum', 'I') > 0 then
		do;
			substr(Customer_ID, 1, 8)='Platinum';
			output work.platinum;
		end;
	else output other;
	keep Customer_ID Name Country;
run;

title 'Silver-Level Customers';
proc print data=work.silver noobs;
run;

title 'Gold-Level Customers';
proc print data=work.gold noobs;
run;

title 'Platinum-Level Customers';
proc print data=work.platinum noobs;
run;
title;

title 'Other Customers';
proc print data=work.other noobs;
run;
title;


/**/

proc print data = orion.employee_donations (obs=10);
run;

data work.split;
	set orion.employee_donations;
	PctLoc=find(Recipients, '%');

	if PctLoc > 0 then
		do;
			Charity=substr(Recipients, 1, PctLoc);
			output work.split;
			Charity=substr(Recipients, PctLoc+3);
			output work.split;
		end;
	else
		do;
			Charity=trim(Recipients)!!'100%';
			output work.split;
		end;
	keep Employee_ID Charity;
run;

title 'Charity Contributions for each Employee';
proc print data=work.split noobs;
	var Employee_ID Charity;
run;
title;


/**/

proc print data = orion.employee_donations (obs=10);
run;

data donation_stats;
	set orion.employee_donations;
	keep Employee_ID Total AvgQT NumQT;
	Total=sum(of Qtr1-Qtr4);
	AvgQT=mean(of Qtr1-Qtr4);
	NumQt=n(of Qtr1-Qtr4);
run;

proc print data=donation_stats;
run;


/**/

data sale_stats;
	set orion.orders_midyear;
	MonthAvg=round(mean(of Month1-Month6));
	MonthMax=max(of Month1-Month6);
	MonthSum=sum(of Month1-Month6);
	keep Customer_ID MonthAvg MonthMax MonthSum;
run;

title 'Statistics on Months in which the Customer Placed an Order';
proc print data=sale_stats (obs = 20);
	var Customer_ID MonthAvg MonthMax MonthSum;
run;
title;


/**/

proc print data=orion.convert (obs=10);
run;

data hrdata;
	keep EmpID GrossPay Bonus Phone HireDate;
	set orion.convert;
	EmpID=input(ID, 2.)+11000;
	Bonus=input(GrossPay, comma6.)*.10;
	Phone='(' !! put(Code,3.) !! ') ' !! Mobile;

	/*Same as the above line of code*/
	*Phone = cat('(', Code,') ', mobile);
	*Phone = put(Code, 3.);

	/*Compare to input() function*/
	HireDate=input(Hired, mmddyy10.);
run;

proc print data=hrdata;
	format HireDate mmddyy10.;
run;
