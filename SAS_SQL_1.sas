/*This file contains codes from lessons 1 to 5 of SAS SQL Essentials and 
lesson 11 of SAS Progrmming 2*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

/*SAS SQL Essential*/


********************************************;

/*Material from SAS Programming 2*/


%let path=/folders/myshortcuts/_myfolders/ecprg293;
libname orion "&path";

/*To create a report that creates output tabe
When you're creating a table, if you don't specify a column alias 
for a calculated column using the AS keyword, SAS will assign a 
column name, such as _TEMA001. If other new columns are created 
without the AS keyword, the new names would be _TEMA002, _TEMA003 and so on.*/
  
proc sql ;
   create table direct_reports as
   select Employee_ID, Job_Title, Salary
      from orion.sales_mgmt;
   select *
      from direct_reports;
quit;

proc sql number;
   create table direct_reports as
   select Employee_ID, Job_Title, Salary
      from orion.sales_mgmt;
   select *
      from direct_reports;
quit;

/*Comparing to proc print (1st column)*/
proc print data=direct_reports;
run;
/*To generate the obs column in proc sql*/

data test;
set direct_reports;
Obs = _n_;
run;

proc print data = test noobs;
run;

*Notice that the PROC SQL output shows variable labels and no observation number is shown.
PROC PRINT displays variable names instead of variable labels by default.

Because the SELECT statement runs immediately in the SQL procedure, you must place the TITLE 
and FOOTNOTE statements before the SELECT statement. Alternatively, you can place the TITLE 
or FOOTNOTE statement before the PROC SQL statement.;


/*Practice 1: Contains*/

title 'Employees in Concession Management';

proc sql;
select Employee_ID, Job_Title, Manager_ID
from orion.employee_organization
where Department ? "Concession Management";
quit;


*Joining Tables by Using Full Table Names to Qualify Columns

Prefixing the table name is called qualifying a column. Notice that you don't have to 
specify the libref when you qualify a column in the SELECT clause. In PROC SQL, you can 
use a two-level name for any column, but you are only required to qualify the columns that 
appear in multiple tables. You must qualify the columns that appear in both tables wherever 
you reference them in the SELECT statement. In this example, the Employee_ID column names 
are qualified in both the WHERE clause and the SELECT clause.; 
;

/*Comparing where vs inner join on*/

proc sql feedback;
   select sales_mgmt.Employee_ID, Employee_Name,
          Job_Title, Salary
          /*Two-level name : dataset.column*/
   from orion.sales_mgmt,
        orion.employee_addresses
        /*Libref is orion*/
   where sales_mgmt.Employee_ID =
         employee_addresses.Employee_ID;
quit;


proc sql feedback;
   select sales_mgmt.Employee_ID, Employee_Name,
          Job_Title, Salary
          /*Two-level name : dataset.column*/
   from orion.sales_mgmt inner join
        orion.employee_addresses
        /*Libref is orion*/
   on sales_mgmt.Employee_ID =
         employee_addresses.Employee_ID;
quit;


*Joining Tables by Using Table Aliases to Qualify Columns;

proc sql;
select s.Employee_ID, Employee_Name, Job_Title, Salary
   from orion.sales_mgmt as s,
        orion.employee_addresses as a
   where s.Employee_ID =
         a.Employee_ID and
         s.Employee_ID > 121144; *Cannot be: Employee_ID > "121144";
quit;


/*Basic Join*/

* For the code below:
orion.sales_mgmt has 4 rows
orion.employee_addresses has 424 rows
  A basic join (Cartesian product) will then have 1696 (=4*424) rows and 4 columns (as specified) 
  run the code below;

*Note that assigning aliases does not change the names of the underlying tables. ;

proc sql;
create table Cart_row as 
select s.Employee_ID, Employee_Name, Job_Title, Salary, count(*) as Obs 
/*Obs gives the total number of observations = 1696*/
   from orion.sales_mgmt s, 
        orion.employee_addresses as a;
select*
from Cart_row;
quit;

/*Keyword: Cross join gives the same basic join as above*/

proc sql;
create table Cart_row2 as 
select s.Employee_ID, Employee_Name, Job_Title, Salary, count(*) as Obs 
/*Obs gives the total number of observations = 1696*/
   from orion.sales_mgmt s cross join 
        orion.employee_addresses as a;
select*
from Cart_row2;
quit;


*proc print data = Cart_row; *run;

*Take a monment to digest the three tables. 
 
Basic join is not the same as merging!
A PROC SQL inner join is equivalent to a DATA step merge in which both data sets 
contribute to the merge

SAS Log : NOTE: The execution of this query involves performing one or more Cartesian product 
                joins that can not be optimized.
          NOTE: Table WORK.CART_ROW created, with 1696 rows and 4 columns.
 
One advantage of using PROC SQL to join tables is that you don't have to sort the 
input tables first. 

The DATA step requires the input data sets to be presorted, but PROC SQL does not.

The DATA step does not create a report by default, but PROC SQL does.

To use data step, see the code below;

proc sort data=orion.sales_mgmt out=sales_mgmt;
   by Employee_ID;
run;

proc sort data=orion.employee_addresses out=addresses;
   by Employee_ID;
run;

data sqlEquiv (keep=Employee_ID Job_Title Salary Employee_Name);
   merge sales_mgmt (in=inSalesMgmt) addresses (in=inAddr);
   by Employee_ID;
   if insalesmgmt and inaddr;
run;

proc print data=sqlEquiv noobs label;
   var Employee_ID Employee_Name Job_Title Salary;
run;

/*verses*/

proc sql;
 create table direct_reports1 as
select s.Employee_ID, Employee_Name, Job_Title, Salary
   from orion.sales_mgmt as s,
        orion.employee_addresses a /*aliases keyword "as" can be skipped */
   where s.Employee_ID =
         a.Employee_ID;
select*
from direct_reports1;
quit;

*Both data sets display the same result. But the code for proc step and data step are longers;


/*Practice 2: title statment position, aliases position, 
   spliting column using #, outobs and obs = */

proc sql;
   select d.Product_ID, Product_Name, Start_Date, End_Date, Discount label = "#Discount as Percent of#
   Normal Retail Sales Price" 
      from orion.discount as d, 
           orion.product_dim as p
      where d.Product_ID=p.Product_ID and Discount >= 0.6;
quit;

/*A label can begin with the following characters: a through z, A through Z, 0 through 9, 
an underscore (_), or a blank space. If you begin a label with any other character, 
such as pound sign (#), that character is used as a split character and it splits the
 label onto the next line wherever it appears. For example:

select dropout label=
'#Percentage of#Students Who#Dropped Out'
   from educ(obs=5);
If you need a special character to appear as the first character in the output, 
precede it with a space or a forward slash (/).*/

title "Detail Information for Ordered Products and Quantities."; 
proc sql;
create table q2 as
select Order_ID, f.Product_ID, Product_Name, Quantity 
from  orion.order_fact f,
      orion.product_dim d
where f.Product_ID = d.Product_ID;
select*
from q2 (obs=10);
quit;
title;

/*Alternate Solution*/

title 'Detail Information for Ordered Products and Quantities';
proc sql outobs=10;
create table q3 as 
   select Order_ID, o.Product_ID, 
          Product_Name, Quantity
      from orion.order_fact as o
           INNER JOIN
           orion.product_dim as p
           on o.Product_ID=p.Product_ID;
           select* from q3 ;
quit;
title;


/***************************************************************/


/*Material from SAS SQL Essential 1*/


%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";


/*You can use the VALIDATE statement with any SELECT statement, but not 
with other statements To check the accuracy of a query's syntax without 
executing the query. The VALIDATE statement must appear above the SELECT 
keyword.

NOTE: PROC SQL statement has valid syntax.*/

proc sql;
validate
select Employee_ID, Job_Title
   from orion.staff;
validate
select Employee_ID, Employee_Name,
       Postal_Code
   from orion.employee_addresses
   where Postal_Code contains '33'
   order by Postal_Code; 
quit;

/*To check the syntax of all statements in your PROC SQL program without executing the program,
 you can specify the NOEXEC option in the PROC SQL statement. When you specify the NOEXEC option, 
 you override the EXEC option, which is set by default.
 
NOTE: Statement not executed due to NOEXEC option.*/

proc sql noexec;
select Order_ID, Product_ID
   from orion.order_fact
   where Order_Type=1;
reset exec;
select Product_ID, Product_Name
   from orion.product_dim;
quit;

/*
To tell SAS to execute the coming statement
system options, statement options, and dataset options use reset exec
*/

proc sql feedback;
select *
   from orion.employee_information;
select *
   from orion.sales;
quit;

proc sql;
describe table orion.employee_information;
quit;

/*This table has three character columns: Department, Job_Title, and Employee_Gender. 
Notice that the length of the character columns appears in parentheses. Also, notice that 
additional attributes are shown for some of the colummns. SAS formats are assigned

NOTE: SQL table ORION.EMPLOYEE_INFORMATION was created like:
*/

proc sql;
 create table ORION.EMPLOYEE_INFORMATION1( bufsize=65536 )
   (
    Employee_ID num format=12. label='Employee ID',
    Start_Date num format=DATE9. label='Start Date',
    End_Date num format=DATE9. label='End Date',
    Department char(40),
    Job_Title char(25) label='Employee Job Title',
    Salary num format=DOLLAR12. label='Employee Annual Salary',
    Employee_Gender char(1) label='Employee Gender',
    Birth_Date num format=DATE9. label='Employee Birth Date',
    Employee_Hire_Date num format=DATE9. informat=DATE9. label='Employee Hire Date',
    Employee_Term_Date num format=DATE9. informat=DATE9. label='Employee Termination Date',
    Manager_ID num format=12. label='Manager for Employee'
   );
quit;

proc sql feedback;
select*
   from orion.staff;
describe table orion.staff;
quit;

proc sql outobs=5;
select Employee_ID, Employee_Gender, Salary
	from orion.employee_information;
select Employee_ID, Salary, Employee_Gender
	from orion.employee_information;
quit;

/*
You can use expressions to create values for new columns. 
You can also calculate columns based on conditional logic.
*/

/***************************************************************/


/*Basic Queries*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql feedback;
select Employee_ID, Employee_Gender label = "Gender", 
Salary label= 'Annual Salary', 
Salary*0.1 as Bonus format = dollar.
 from orion.employee_information;
quit;

proc sql;
select Employee_ID, Employee_Gender, 
       int(yrdif(Birth_Date,today(),'Age')) as Age_yrdif,
       int((today()-Birth_Date)/365.25) as Age_int,
       (int(yrdif(Birth_Date,today(),'Age'))- int((today()-Birth_Date)/365.25))
       as dif format = 8., 
       sum( (int(yrdif(Birth_Date,today(),'Age'))- int((today()-Birth_Date)/365.25))) 
       	'#Total#difference'
   from orion.employee_payroll; 
quit; 


/*Creating Columns Conditionally using case*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc print data = orion.staff;
var Employee_ID Job_Title Salary;
run;

proc sql;
select Employee_ID, Job_Title, Salary,
       case scan(Job_Title,-1,' ','I')      
          when 'I' then Salary
          when 'II' then Salary*.07
          when 'III' then Salary*.10
          when 'IV' then Salary*.12 
          else Salary*.08
       end as Bonus format = dollar.
   from orion.staff;
quit; 

proc sql;
select Employee_ID, Job_Title, Salary,
       case       
          when scan(Job_Title,-1,' ','I') = 'I' then Salary*.05
          when scan(Job_Title,-1,' ','I') = 'II' then Salary*.07
          when scan(Job_Title,-1,' ','I') = 'III' then Salary*.10
          when scan(Job_Title,-1,' ','I') = 'IV' then Salary*.12 
          else Salary*.08
       end as Bonus format = dollar.
   from orion.staff;
quit; 

 /*If count is positive, SCAN counts words from left to right in the character string. 
   If count is negative, SCAN  counts words from right to left.
   
   If no ELSE expression is present and every WHEN condition is false, 
   the CASE expression returns a missing value.
   You can specify the condition in two different places in the CASE expression. 
 
   In the shortcut (the code here), or case-operand form, you specify the condition as the case-operand once at the 
   top of the CASE expression, followed by a series of WHEN-THEN clauses. With the shortcut form of CASE 
   expression, you can only use equality tests for validity.
   
   The shortcut form is the more efficient way to create the column. Both ways produce the 
   same result. Typically, you use the standard form when you need to use a comparison 
   operator other than equal
   
  In the standard form, you specify the case-operand as part of the condition in each WHEN-THEN clause.
  With the standard form of CASE expression, you can use equality and non-equality tests for validity*/    

/*Comapre the two blocks of code*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql;
select Employee_ID, Job_Title, Salary,
       case   
          when Salary<=35000 then 'Low'
          when 35000< Salary<=60000 then 'Medium'
          when Salary>60000 then 'High'
          else 'Unclassified'
       end as Salary_Category  
   from orion.staff 
   Group by Salary_Category /*This will be changed to an ordered by clause*/
   Having Salary<100000 ; 
quit; 

proc sql;
select Employee_ID, Job_Title, Salary,
       case   
          when Salary<=35000 then 'Low'
          when 35000< Salary<=60000 then 'Medium'
          when Salary>60000 then 'High'
          else 'Unclassified'
       end as Salary_Category  
   from orion.staff 
   where Salary<100000 
   order by Salary_Category; 
quit; 



/*Create Tables*/

proc sql;
create table work.birth_months as
select Employee_ID, Birth_Date, 
       month(Birth_Date) as Birth_Month, 
       Employee_Gender
   from orion.employee_information;
describe table work.birth_months;
select * 
from work.birth_months;
quit;

/*Practice*/

proc sql;
create table work.bonus as
select  Employee_ID, Employee_Gender, Marital_Status, 
	    Salary format = dollar., 
		Salary/3 as Tax format = dollar.,
		Salary*0.04 as Bonus format = dollar.
from orion.employee_payroll;
select* 
from work.bonus
quit; 


proc sql;
select Employee_ID, Salary, 
case (scan(Job_Title,-1," "))
 	when 'Manager' then "Manager"
 	when 'Director' then "Director"
 	when 'Officer' then "Executive" 
 	when 'President' then "Executive"
 else "N/A"
end as Level,
case (calculated Level)
	when "Manager" then 
		 case 
  			 when Salary < 52000 then 'Low'
  			 when Salary > 72000 then 'High'
   		 else 'Medium'
 		 end  
	when "Director" then 
 		 case  
  		 	 when Salary < 108000 then 'Low'
  			 when Salary > 135000 then 'High'
  		 else 'Medium'
 		 end 
	when "Executive" then 
 		case 
 			 when Salary < 240000 then 'Low'
             when Salary > 300000 then 'High'
  		else 'Medium'
 		end
else "N/A" 
end as Salary_Range

from orion.staff
where calculated level ne "N/A" 
order by Level, Salary desc;
quit;


/*Eliminating Duplicate Rows*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql number;
select distinct Department
   from orion.employee_organization;
quit; 

proc sql number;
select distinct Job_Title
   from orion.employee_organization;
quit; 

/*The last two blocks of code are the same with different orders only (obs =146)*/
proc sql number; 
title "distinct Job_Title, Department";
select distinct Job_Title, Department
   from orion.employee_organization;
quit; 

proc sql number;
title 'distinct Department, Job_Title';
select distinct Department, Job_Title
   from orion.employee_organization;
quit; 


/*Calculated*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql outobs=20;
select Employee_ID, Employee_Gender label='Gender', 
       Salary label = 'Annuel Salary', Salary*.10 as Bonus,
       calculated Bonus/2 as Half 
   from orion.employee_information
   where calculated Bonus<3000;
quit;

/*Practice*/
proc sql;
select distinct City
from orion.employee_addresses;
quit;

proc sql;
select Employee_ID, Recipients, 
sum(Qtr1, Qtr2, Qtr3, Qtr4) as Total
from orion.employee_donations ;
*where calculated Total > 90;
quit;

proc sql;
select Employee_ID, Recipients 
from orion.employee_donations 
where Recipients like "% Inc. 90~%" ESCAPE "~";
quit;

proc sql;
select Employee_ID, Recipients 
from orion.employee_donations 
where Recipients like "% Inc. 90%";
quit;

/*Sublet employees who contributed 90% of their charitable contributions to
 a single company that was incorporated (Inc.) and is the last recipient 
 in the list to receive the 90% contribution. 
 
 An escape character forces the next character to be interpreted literally
 rather than as a wildcard. It is only valid for the LIKE condition with
 which it is assocated. The escape character above is the "~". You can use 
 other characters like "*", etc. */


/*Order By*/

proc sql;
select Employee_ID,
       max(Qtr1,Qtr2,Qtr3,Qtr4)	
   from orion.employee_donations
   where Paid_By="Cash or Check"
   order by 2 desc, Employee_ID;
quit;

proc sql;
select Employee_ID,
       max(Qtr1,Qtr2,Qtr3,Qtr4)	
   from orion.employee_donations
   where Paid_By="Cash or Check"
   order by Recipients;
quit;

/*Label ''*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql;
select Employee_ID 'Employee ID', 
       max(Qtr1,Qtr2,Qtr3,Qtr4) label='Maximum' format=dollar5.
 from orion.employee_donations
 where Paid_By="Cash or Check"
 order by 2 desc, Employee_ID;
quit;

/*spot the mistake*/
proc sql number;
title 'Maximum Quarterly Donation';
select Employee_ID, 'Employee ID',
       'Maximum Donation is:',
       max(Qtr1,Qtr2,Qtr3,Qtr4)
          label='Maximum' format=dollar5.
   from orion.employee_donations
   where Paid_By="Cash or Check"
   order by 3 desc, Employee_ID;
quit; 
title;

/*Practice*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql; 
title "Single Male Employee Salaries";
select Employee_ID 'Employee ID',
       Salary 'Annual Salary' format = COMMA10.2 , 
       Salary/3 as Tax ' Federal Tax Withdrawn' format = COMMA10.2 
   from orion.employee_payroll
   where Marital_Status="S"
         and Employee_Gender ="M" 
         and Employee_Term_Date is missing
   order by salary desc;
quit;

proc sql;
title "Australian Clothing Products";
select Supplier_Name 'Suppplier' format= $18., 
       Product_Group 'Group' format = $12. , 
       Product_Name 'Product' format = $30.
from orion.product_dim
where  Product_Category ='Clothes' and
       Supplier_Country = 'AU' 
order by Product_Name;
quit;

proc sql;
title 'US Customers >50 Years Old as of 02FEB2013';
select Customer_ID format = z7., 
/*The Zw.d format writes standard numeric values one digit per byte and fills in 0s to the 
left of the data value.The Zw.d format rounds to the nearest number that will fit in the 
output field. If w.d is too large to fit, SAS might shift the decimal to the BESTw. format. 
The Zw.d format writes negative numbers with leading minus signs. In addition, it right aligns 
before writing and pads the output with leading zeros.*/
       catx(', ', Customer_LastName, Customer_FirstName) as Name,
       Gender 'Gender',
       int(('02feb2013'd-Birth_Date)/365.25) as Age
from orion.customer
where Country = 'US' and Calculated Age > 50
order by Age desc, Name;
quit;


/*
  Summary Functions

Ensuring Consistent Results When Using ANSI SQL Summary Functions

The ANSI SQL summary functions do not round, and they process values in the order that 
they encounter them. For these reasons, it is possible to obtain different results when 
you run the same PROC SQL query several times. To ensure that you obtain consistent results 
from ANSI SQL summary functions in your queries, you can use a numeric truncation function 
such as INT, ROUND, CEIL, or FLOOR.

*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql number;
select Employee_ID
       label='Employee ID',
       Qtr1,Qtr2,Qtr3,Qtr4,
       sum(Qtr1,Qtr2,Qtr3,Qtr4)
       as SumQ
       label='Annual Donation'
       format=dollar5.
   from orion.employee_donations
   where Paid_By='Cash or Check'
   order by 6 desc;
quit; 

data class;
   set orion.employee_donations;
   where paid_by='Cash or Check';
   SumQ=sum(Qtr1,Qtr2,Qtr3,Qtr4);
   label SumQ='Annual Donation';
   format SumQ dollar5.;
run;

proc sort data=class;
   by descending SumQ;
run;

proc print data=class label noobs;
   var Employee_ID Qtr1 Qtr2 Qtr3 Qtr4 SumQ;
run;


/*Conut Frq N functions*/
proc sql;
select count(*) as Current_Employees
   from orion.employee_information
   where Employee_Term_Date is missing;
quit;

proc sql;
select count(Employee_Term_Date) as Former_Employees
   from orion.employee_information;
quit;

proc sql;
select count(*) as Former_Employees
   from orion.employee_information
    where Employee_Term_Date is not missing;
quit;


proc sql;
select N(Employee_Term_Date) as Former_Employees
   from orion.employee_information;
quit;

proc sql;
select N(Employee_Term_Date) as Former_Employees
   from orion.employee_information
    where Employee_Term_Date is not missing;
quit;

proc sql;
select FREQ(Employee_Term_Date) as Former_Employees
   from orion.employee_information;
quit;

proc sql;
select FREQ(Employee_Term_Date) as Former_Employees
   from orion.employee_information
    where Employee_Term_Date is not missing;
quit;

/*Practice*/

proc sql;
select sum(Dependents) as Total_Dependents
from orion.employee_payroll
where Employee_Term_Date is missing;
quit;

proc sql;
select count(*) label='Employees with Dependents'
   from orion.employee_payroll
   where Employee_Term_Date is missing
         and Dependents >0;
quit;

/*Remerging*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql;
select Employee_Gender as Gender, avg(Salary) as Average 	
   from orion.employee_information
   where Employee_Term_Date is missing;
quit;

proc sql noremerge;
select Employee_Gender as Gender, avg(Salary) as Average 	
   from orion.employee_information
   where Employee_Term_Date is missing;
quit;


/*Proc SQL vs Proc Data Steps*/

proc sql;
title "Male Employee Salaries";
select Employee_ID, Salary format=comma12.,
       Salary/sum(Salary) format=percent7.2
   from orion.employee_information
   where Employee_Gender='M'
         and Employee_Term_Date is missing
   order by 3 desc;
quit;
title;
* Remerging occurs in column 3 
NOTE: The query requires remerging summary statistics back with the original data.;

proc means data=orion.employee_payroll(where=(Employee_Gender='M' 
                                      and Employee_Term_Date is missing)) 
   sum noprint;
   output out=summary sum=TotalSalary;
   var Salary;
run;

data report;
   merge orion.employee_information (where=(Employee_Gender="M" 
                                    and Employee_Term_Date is missing))
         work.summary(keep=TotalSalary);
      retain Total 0;
      if _n_=1 then Total=TotalSalary;
      Percent=Salary / Total;
      keep Employee_ID Salary Percent;
      format salary comma12. Percent percent7.2;
run;

proc sort data=report;
   by descending Percent;
run;

title "Male Employee Salaries – Traditional SAS Programming";
proc print data=report noobs split='*';
   label Percent='*';
run;
title;

/*Notice that the error message also refers to the DBIDIRECTEXEC system option, 
which is a SAS/ACCESS option that prevents the remerging of summary statistics.*/

/*Group by: Cannot specify an expression that is a summary function. */
proc sql;
title "Average Salary by Gender and Marital Status";
select Employee_Gender as Gender, 
       Marital_Status as M_Status,
       avg(Salary) as Average
   from orion.employee_payroll
   where Employee_Term_Date is missing
   group by Gender, M_Status;
quit;
title;

*In the GROUP BY clause, you can specify column aliases as well as 
column names (Employee_Gender, Marital_Status);

/*Having is a SAS enhancement */
proc sql;
select Department, count(*) as Count
from Orion.employee_information
group by department
having Count > 25
order by Count desc ;
quit;

/*Because the WHERE clause is evaluated before a row is available 
for processing and determines which individual rows are available for
grouping, you cannot use a WHERE clause to subset grouped rows by 
referring to the calculated summary column Count.

The HAVING clause can refer to the column alias of a calculated column, 
such as Count, without using the keyword CALCULATED. 

Also, the HAVING clause  can refer to the column alias of a column that 
was created by a summary function with a single argument, but the WHERE clause cannot.*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql number;
select distinct Department, Job_Title,
       find(Job_Title,"manager","i")
          as Position
   from orion.employee_organization
   where calculated Position >= 1;
quit;


/*Counting Rows by Using Boolean Expressions*/

proc sql number;
select Department,
       sum(find(Job_Title,"manager","i")>0)
          as Managers,
       sum(find(Job_Title,"manager","i")=0)
          as Employees,
       Calculated Managers / Calculated Employees "M/E Ratio"
       format = percent7.2
   from orion.employee_information
   group by Department;
quit;

/*Practice*/

proc sql;
title "Cities Where Employees Live";
select City, Count(*) as Count
from orion.employee_addresses 
group by City
order by City;
quit;
title;

proc sql;
title "Age at Employment";
select Employee_ID 'Employee ID', 
       Birth_Date format=mmddyy10. 'Birth Date', 
       Employee_Hire_Date format=mmddyy10. 'Hire Date', 
       int((Employee_Hire_Date-Birth_Date)/365.25) as Age
   from orion.employee_payroll;
quit;
title;

proc sql;
title "Countries and Cities Where Employees Live";
select upcase(Country) 'Country', 
       propcase(City) 'City',
       count(*) 'Employees'
   from orion.employee_addresses
   group by 1,2
   order by 1,2;
quit;
title;


