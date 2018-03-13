/*This file contains codes from lessons 1 to 5 of SAS SQL Essentials and 
lesson 11 of SAS Progrmming 2*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

*%let path=/folders/myshortcuts/_myfolders/ecprg293;
*libname orion "&path";

proc sql feedback ;
 select*
 from orion.sales_mgmt
 order by gender;
quit; 

proc sql;
 select Employee_ID, Job_Title, Salary format = dollarx.
    from orion.sales_mgmt
    where Salary>95000 or Gender = 'F';
quit;

/*To create a report that creates output tabe
When you're creating a table, if you don't specify a column alias 
for a calculated column using the AS keyword, SAS will assign a 
column name, such as _TEMA001. If other new columns are created 
without the AS keyword, the new names would be _TEMA002, _TEMA003 and so on.*/

proc sql;
   create table direct_reports as
   select Employee_ID, Job_Title, Salary
      from orion.sales_mgmt;
      /*View Report*/
     select*
     from direct_reports;
quit; 
 
*To create a report that creates and displays the output table; 
proc sql ;
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
proc sql;
 select*
 from orion.sales_mgmt;
quit; 

proc sql outobs = 5;
 select*
 from orion.employee_addresses;
quit; 

data abb;
set orion.sales_mgmt;
Day="&sysday";
Day1=&sysday;
/* add "" to set the value of Day to a text string*/
run;

proc print data = abb;
run;

proc print data = orion.employee_addresses (obs=5);run;

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
 
 Basic join is not the same as merging !
 A PROC SQL inner join is equivalent to a DATA step merge in which both data sets contribute to the merge

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

/*Practice 2: title statment position, aliases position, spliting column using #*/

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
proc sql;
create table q3 as 
   select Order_ID, o.Product_ID, 
          Product_Name, Quantity
      from orion.order_fact as o
           INNER JOIN
           orion.product_dim as p
           on o.Product_ID=p.Product_ID;
           select* from q3 (obs=10);
quit;
title;



/**************************************************************************************/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql;
/*You can use the VALIDATE statement with any SELECT statement, but not with other statements
To check the accuracy of a query's syntax without executing the query.
The VALIDATE statement must appear above the SELECT keyword.

 NOTE: PROC SQL statement has valid syntax.*/
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


proc sql noexec;
/*To check the syntax of all statements in your PROC SQL program without executing the program,
 you can specify the NOEXEC option in the PROC SQL statement. When you specify the NOEXEC option, 
 you override the EXEC option, which is set by default.
 
  NOTE: Statement not executed due to NOEXEC option.*/
select Order_ID, Product_ID
   from orion.order_fact
   where Order_Type=1;
reset exec;
/*
 to tell SAS to execute the coming statement
 
system options, statement options, and dataset options!!! 
*/
select Product_ID, Product_Name
   from orion.product_dim;
quit;


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
       int((today()-Birth_Date)/365.25) as Age_int
   from orion.employee_payroll; 
quit; 



/*Creating Columns Conditionally*/
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
select Employee_ID, Employee_Gender, Marital_Status, 
Salary format = dollar., 
Salary/3 as Tax format = dollar.,
Salary*0.04 as Bonus format = dollar.
from orion.employee_payroll;
select* 
from work.bonus
quit; 

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

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
select distinct Job_Title, Department
   from orion.employee_organization;
quit; 

proc sql number;
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

/*Inner and Outer Join*/
/*
Situations That Require a Table Alias
In PROC SQL queries, the use of table aliases is usually optional. However, table aliases 
are required when you do the following:
join a table to itself (called a self join); for example:
from orion.staff as s1, orion.staff as s2
reference same-named columns from multiple in-line views or from same-named tables
in different libraries
*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql;
title "Australian Employees' Birth Months";
select p.Employee_ID 'ID',
       Employee_Name as Name format=$25.,
       City format=$25.,
       month(Birth_Date) 'Birth Month' format=3.
   from orion.employee_payroll as p,
        orion.employee_addresses as a
   where p.Employee_ID=a.Employee_ID
         and Country='AU'
   order by 4, City, Employee_Name;
quit;

/*Practice*/

proc sql;
title "Employees with More Than 30 Years of Service";
title2 "As of February 1, 2013";
select Employee_Name 'Employee Name', 
       int((sum('1FEB2013'd,-Employee_Hire_Date))/365.25) as YOS ' Years of Service'
from orion.employee_addresses A, orion.employee_payroll P 
where A.Employee_ID = P.Employee_ID and calculated YOS > 30
order by Employee_Name ;
quit;
title;

proc sql;
title 'Total Quantities Sold by Product ID and Name';
select p.Product_ID, Product_Name, sum(Quantity) 'Total Sold'
from orion.product_dim p, orion.order_fact o
where p.Product_ID = o.Product_ID
         and Order_Date>='01jan2010'd
group by p.Product_ID, Product_Name
order by 3 desc, Product_Name;
quit;
title;

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql;
title "US and Australian Internet Customers";
title2 "Purchasing Foreign-Manufactured Products";
select Customer_Name as Name, 
       Count(*) as Count 'Purchases'
from orion.product_dim p, 
     orion.order_fact o,
     orion.customer c
where p.Product_ID=o.Product_ID
  and o.Customer_ID=c.Customer_ID
  and Employee_ID=99999999
  and p.Supplier_Country ne Country
  and Country in ('US','AU')
   group by Customer_Name
   order by Count desc, Customer_Name;
quit;
title;

*Using Alternative Syntax for an Inner Join;

proc sql;
select c.ID, Name, Action, Amount
   from customers as c
        inner join
        transactions as t
   on c.ID=t.ID;
quit;

*Performing a Left Outer Join;

proc sql;
select *
   from customers as c
        left join
        transactions as t
   on c.ID=t.ID;   
quit;

*Performing a Right Outer Join;

proc sql;
select *
   from customers as c
        right join
        transactions as t
   on c.ID=t.ID;   
quit;

*Performing a Full Outer Join;

proc sql;
select *
   from customers as c
        full join
        transactions as t
   on c.ID=t.ID;   
quit;

/*In a full outer join, does the order of the tables make a difference?
In a full outer join, the order of the tables affects the order of the columns in the 
result set. The columns from the left table appear before the columns from the right table.
*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc means data = orion.employee_payroll n; var Employee_ID; class Marital_Status;
proc print data = orion.employee_payroll noobs;where Marital_Status = 'M';run;
proc print data = orion.employee_donations;run;

proc means data = orion.employee_payroll n; var Employee_ID; class Marital_Status; run;
proc means data = orion.employee_donations n; var Employee_ID; where Recipients is not missing; run;

proc sql number;
select p.Employee_ID, Recipients
   from orion.employee_payroll as p
        left join
        orion.employee_donations as d
   on p.Employee_ID=d.Employee_ID;
quit;

proc sql number;
select d.Employee_ID, Recipients
   from orion.employee_payroll as p
        right join
        orion.employee_donations as d
   on p.Employee_ID=d.Employee_ID;
quit;
*Which is the same as:;
proc sql number;
select Employee_ID, Recipients
from  orion.employee_donations;
quit;

proc means data = orion.employee_payroll n; var Employee_ID; class Marital_Status; run;
proc means data = orion.employee_donations n; var Employee_ID; where Recipients is not missing; run;

proc sql number;
select p.Employee_ID, Recipients
   from orion.employee_payroll as p
        left join
        orion.employee_donations as d
   on p.Employee_ID=d.Employee_ID
   where Marital_Status="M" ;*and Recipients is not missing;
quit;

*Using the COALESCE Function to Overlay Columns;

proc sql feedback;
select*
   from orion.customers c full join orion.transactions t
   on c.ID=t.ID
 order by ID;
quit;

proc sql;
select coalesce(c.ID,t.ID) as ID,
       Name, Action, Amount
   from orion.customers c full join orion.transactions t
   on c.ID=t.ID
 order by ID;
quit;

/*In the SELECT clause, you can use a qualified column name to indicate that 
the column comes from just one of the tables.

However, eliminating a duplicate column is not the same as overlaying a column. 
In this result set, the ID column contains values from only one of the tables.
To overlay columns in SQL joins, you can use the COALESCE function, which is a SAS 
function. The COALESCE function returns the value of the first nonmissing argument 
from two or more arguments that you specify.

An argument can be a column name, a constant, or an expression.
When all arguments are missing, COALESCE returns a missing value. 
All arguments must be of the same type, character or numeric.*/

/*Practice: Level 1*/
%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql number ;
select s.Job_Title, adr.Employee_Name, adr.City
from orion.sales s right join orion.employee_addresses adr 
on  s.Employee_ID = adr.Employee_ID
order by city, job_title, Employee_name;
quit;

proc sql number ;
select Employee_Name 'Name' format=$35., 
       City, Job_Title
   from orion.employee_addresses as a
        left join 
        orion.sales as s
        on a.Employee_ID=s.Employee_ID
   order by City, Job_Title, Employee_Name;
quit;
/*Notice the different orders of the two datasets
But if change the 1st line of code to select Employee_Name 'Name' format=$35., 
       City, Job_Title, the two tables are the same*/

/*Level 2*/

proc sql;
title 'Products That Have Not Been Sold';
select Product_Name, coalesce(o.Product_ID, p.Product_ID) 'Product ID', Quantity
from orion.order_fact o right join orion.product_dim p
on o.Product_ID = p.Product_ID
where Order_ID is missing
order by 2;
quit;
title;
/*Return an empty dataset, denoting that all items were sold*/

/*Complex Self-Join*/

proc sql;
select e.Employee_ID "Employee ID", 
          e.Employee_Name "Employee Name",
          m.Employee_ID "Manager ID",
          m.Employee_Name "Manager Name",
          e.Country
   from orion.employee_addresses as e,
           orion.employee_addresses as m,
           orion.employee_organization as o
   where e.Employee_ID=o.Employee_ID and
              o.Manager_ID=m.Employee_ID and 
              Department contains 'Sales'
   order by Country,4,1;
quit;

/*Practice: Self-Join*/

proc sql;
title 'Trainee and Temporary Employees';
select a1.Employee_ID 'EmployeeID', a1.Employee_Name 'EmployeeName', 
       s.Job_title 'Job Title', s.Manager_ID 'Manager ID', 
       a2.Employee_Name 'Manager Name' /*Self join here*/
from orion.staff s, 
     orion.employee_addresses a1, 
     orion.employee_addresses a2
where s.Employee_ID = a1.Employee_ID and 
      s.Manager_ID = a2.Employee_ID and  /*Self join here*/
      (Job_Title contains 'Trainee' or
              Job_Title contains 'Temp')
order by a1.Employee_ID;
quit;
title;              

/*Complex Join*/

proc contents data = orion.employee_addresses;run;
proc contents data =orion.employee_organization;run;
proc contents data = orion.employee_payroll;run;

proc sql;
title "Employees With More Than 30 Years of Service";
title2 "As of February 1, 2013";
select a.Employee_Name 'Employ Name' format = $20., 
       int(('1FEB2013'd-p.Employee_Hire_Date)/365.25) as YOS 'Years of Service',
       mgr.Employee_Name as Manager_Name 'Manager Name' format = $20. 
from orion.employee_addresses as a, 
     orion.employee_organization as o, 
     orion.employee_payroll as p,
     orion.employee_addresses as mgr
where calculated YOS > 30 and
      a.Employee_ID = o.Employee_ID and 
      o.Employee_ID = p.Employee_ID and 
      o.Manager_ID = mgr.Employee_ID 
order by Manager_Name, YOS desc, a.Employee_Name;
quit;
title;
      
  

/*Inner vs Outer Quries*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql;
select Employee_Name, City, Country 
   from orion.employee_addresses
   where Employee_ID = any /*= in*/
      (select Employee_ID
          from orion.employee_payroll
          where month(Birth_Date)=2)
   order by Employee_Name;
quit;

proc sql;
title 'Level IV Sales Reps Who Earn Less Than';
title2 'Any Lower Level Sales Reps';
select  Employee_ID, Salary
   from orion.staff
   where Job_Title='Sales Rep. IV'  
      and Salary < any 
      (select Salary
          from orion.staff
          where Job_Title in 
             ('Sales Rep. I','Sales Rep. II',
             'Sales Rep. III'));
quit;
title;

proc sql;
title 'Level IV Sales Reps Who Earn Less Than';
title2 'Any Lower Level Sales Reps';
select  Employee_ID, Salary
   from orion.staff
   where Job_Title='Sales Rep. IV'  
      and Salary<
      (select max(Salary)
          from orion.staff
          where Job_Title in 
             ('Sales Rep. I','Sales Rep. II',
             'Sales Rep. III'));
quit;
title;

proc sql;
title 'Home Phone Numbers of Sales Employees';
title2 'Who Made Donations';
select Employee_ID, Phone_Number as Home_Phone
   from orion.employee_phones
   where Phone_Type='Home' and
         Employee_ID in
      (select Employee_ID 
       from orion.employee_donations
          where Employee_ID in 
             (select Employee_ID 
              from orion.sales));
quit;
title;

/*Practice*/

proc sql;
title "Customers Whose Average Retail Sales";
title2 "Exceeds the Average Retail Sales";
title3 "for All Retail Customers";
select Customer_ID, 
       avg(Total_Retail_Price) as MeanSales format=8.2 
from orion.order_fact
where Order_Type=1
group by Customer_ID
having MeanSales > (select avg(Total_Retail_Price)
             from orion.order_fact
             where order_type=1);
quit;
title;

proc sql;
title "Employee IDs for Current Month Anniversaries";
select Employee_ID
   from orion.employee_payroll
   where month(Employee_Hire_Date)=month(today());
quit;
title;

proc sql flow = 15;
title "Employees with Current Month Anniversaries";
select Employee_ID, scan(Employee_Name, 2, ', ', 'I') 
                    as FirstName 'First Name',
       scan(Employee_Name, 1, ', ', 'I') 
                    as LastName 'Last Name'            
from orion.employee_addresses
where Employee_ID in (select Employee_ID
   from orion.employee_payroll
   where month(Employee_Hire_Date)=month(today()))
order by LastName;
quit;
title;
      
title 'Orion Club Low Activity Members';
title2 'with Last Order Date Before 01JAN2012';
proc sql;
select distinct Customer_ID, max(Order_Date) 
               'Order Date' format= date11.
from orion.order_fact
where Order_Date < '01Jan2012'd and 
Customer_ID in (
    select Customer_ID from orion.customer 
    where Customer_Type_ID = (
           select Customer_type_ID 
           from orion.customer_type 
           where Customer_Type ='Orion Club members low activity'))
group by Customer_ID;           
quit;
title;      

/*Alternate Solution:*/
      
title 'Inner Join with In-Line View';
proc sql;
select c.Customer_ID, Date
   from orion.customer_type as t,
        orion.customer as c,
        (select Customer_ID, max(Order_Date) format=date11. as Date
            from orion.order_fact
            where Order_Date < '01JAN2012'd
            group by 1) as of
   where t.Customer_Type_ID=c.Customer_Type_ID
     and c.Customer_ID=of.Customer_ID
     and Customer_Type='Orion Club members low activity';
quit;
title;    

  
/*In-Line View*/
%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql;
title  'Sales Department Average Salary';
title2 'By Job Title';
select Job_Title,
       avg(Salary) as Job_Avg
       format=comma7.
   from orion.employee_payroll as p,
        orion.employee_organization as o
   where p.Employee_ID=o.Employee_ID
         and Employee_Term_Date is missing
         and Department="Sales"
   group by Job_Title;
quit;
title;

*This SELECT statement cannot be used as a subquery because it specifies two columns. 
Remember that a subquery can return only a single column. Fortunately, you can nest this 
query in a different way: you can use it as an in-line view.;

proc sql;
title  'Employees with Salaries less than';
title2 '95% of the Average for their Job';
select Employee_Name, emp.Job_Title, 
       Salary format=comma7., Job_Avg format=comma7.
   from  
   where 
   order by Job_Title, Employee_Name;
quit;
title;
      
proc sql;
title  'Employees with Salaries less than';
title2 '95% of the Average for their Job';
select Employee_Name, emp.Job_Title, 
       Salary format=comma7., Job_Avg format=comma7. , Job_Avg*.95 as Standard format=comma7.
   from (select Job_Title, 
                avg(Salary) as Job_Avg format=comma7.
            from orion.employee_payroll as p, 
                 orion.employee_organization as o
            where p.Employee_ID=o.Employee_ID
                  and Employee_Term_Date is missing
                  and Department="Sales"
            group by Job_Title) as job,
        orion.salesstaff as emp
   where emp.Job_Title=job.Job_Title
         and Salary < Job_Avg*.95
		 and Emp_Term_Date is missing
   order by Job_Title, Employee_Name;
quit;
title;


/****** Using an In-Line View and a Subquery************/
*The final goal is to create a report that shows the manager's 
name and city for each employee who sold Expedition Zero sleeping bags in 2011 ; 

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql;
select Employee_Name format=$25. as Name, City
   from orion.employee_addresses
   where Employee_ID in
       (select Manager_ID 
           from orion.employee_organization as o,
           (select distinct Employee_ID
               from orion.order_fact as of, 
	               orion.product_dim as p
               where of.Product_ID=p.Product_ID
               and year(Order_Date)=2011 
               and Product_Name contains 
                   'Expedition Zero'
               and Employee_ID ne 99999999) as ID
            where o.Employee_ID=ID.Employee_ID);

/*Breaking it down*/

proc sql;
select distinct Employee_ID
   from orion.order_fact as of, 
        orion.product_dim as p
   where of.Product_ID=p.Product_ID
         and year(Order_Date)=2011
         and Product_Name contains 
             'Expedition Zero' 
         and Employee_ID ne 99999999;
quit;
/*Get an Employee_ID*/

/* Notice that the part 1 query references only one column, 
so you can add it as either an in-line view or a noncorrelated subquery.*/

proc sql;
select Manager_ID 
   from orion.employee_organization as o,

   where  ;
quit;

proc sql;
select Manager_ID 
   from orion.employee_organization as o,
      (select distinct Employee_ID
          from orion.order_fact as of, 
	       orion.product_dim as p
          where of.Product_ID=p.Product_ID
	     and year(Order_Date)=2011
	     and Product_Name contains 
                    'Expedition Zero' 
	     and Employee_ID ne 99999999)as ID
   where o.Employee_ID=ID.Employee_ID;
quit;
/*Get an Mnager_ID*/

/*Combining the two subqueries*/

proc sql;
select Employee_Name format=$25. as Name, City
   from orion.employee_addresses
   where Employee_ID in
       (select Manager_ID 
           from orion.employee_organization as o,
           (select distinct Employee_ID
               from orion.order_fact as of, 
	               orion.product_dim as p
               where of.Product_ID=p.Product_ID
               and year(Order_Date)=2011 
               and Product_Name contains 
                   'Expedition Zero'
               and Employee_ID ne 99999999) as ID
            where o.Employee_ID=ID.Employee_ID);


/* Using Multi-way Join to achieve the same result*/

proc sql;
select distinct Employee_Name format=$25. as Name, City  
   from orion.order_fact as of,  
        orion.product_dim as pd,
        orion.employee_organization as eo,
        orion.employee_addresses as ea
   where of.Product_ID=pd.Product_ID
         and of.Employee_ID=eo.Employee_ID
         and ea.Employee_ID=eo.Manager_ID 
         and Product_Name contains 'Expedition Zero'
         and year(Order_Date)=2011
         and eo.Employee_ID ne 99999999;
quit;


proc sql;
title "2011 Total Sales Figures";
select catx(' ',scan(mgr.Employee_Name,2,','),
            scan(mgr.Employee_Name,1,',')) format=$27. 
       as Manager,
       catx(' ',scan(emp.Employee_Name,2,','),
            scan(emp.Employee_Name,1,',')) format=$27. 
       as Employee,
       Sum(Total_Retail_Price) format=comma9.2 
       as Total_Sales 
from orion.order_fact as order,
     orion.employee_organization as org,
     orion.employee_addresses as emp,
     orion.employee_addresses as mgr
where order.Employee_ID=org.Employee_ID
  and order.Employee_ID=emp.Employee_ID
  and mgr.Employee_ID=org.Manager_ID
  and year(Order_Date)=2011
  and order.Employee_ID ne 99999999
group by mgr.Country, mgr.Employee_Name, emp.Employee_Name
order by mgr.Country, mgr.Employee_Name, Total_Sales desc;
quit;
title;

/*Proc and Data steps*/

  /**********************************************************
   Step 1:  Identify the employees who sold Expedition Zero
            merchandise in 2003.
   *********************************************************/
proc sort data=orion.order_fact
            (keep=Product_ID Employee_ID Order_Date
            where=(YEAR(Order_Date)=2011 and Employee_ID ne 99999999))
            out=orders_2011 (Drop=Order_Date);
   by Product_ID;
run;
proc sort data=orion.product_dim (keep=Product_ID Product_Name)
          out=products;
   by Product_ID;
run;
data employees (Keep=Employee_ID);
   merge orders_2011 (In=KeepMe)
         products (where=(Product_Name contains 'Expedition Zero'));
   by Product_ID;
   if KeepMe and Product_Name ne '';
run;

proc sort data=employees nodup;
   by Employee_ID;
run;

  /**********************************************************
   Step 2:  Find the employee identifier for the managers of
            these employees
   *********************************************************/
data manager_id (rename=(Manager_ID=Employee_ID));
   merge employees (in=KeepMe)
         orion.employee_organization (keep=Employee_ID Manager_ID);
   by Employee_ID;
   if KeepMe;
   drop Employee_ID;
run;
proc sort data=manager_id nodup;
   by Employee_ID;
run;

  /**********************************************************
   Step 3:  Obtain the managers' names and city information
   *********************************************************/
proc sort data=orion.employee_addresses (Keep=Employee_ID 
                                         Employee_Name City)
          out=employees;
   by Employee_ID;
run;

data managers;
   length Manager $28.;
   merge manager_id (in=KeepMe)
         employees;
   by Employee_ID;
   if KeepMe;
   Manager=catx(' ',scan(Employee_Name,2,','), 
           scan(Employee_Name,1,','));
   drop Employee_ID Employee_Name;
run;
proc print data=managers noobs;
run;

/*Practice: Level 1*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql;
title "2011 Sales Force Sales Statistics";
title2 "For Employees With $200.00 or More in Sales";
select Country, First_Name, Last_Name, 
       sum(Total_Retail_Price) as Value_Sold format=comma9.2,
       count(distinct Order_ID) as Orders,
       Calculated Value_Sold / Calculated Orders as Avg_Order
from orion.order_fact as of, 
     orion.sales as s 
where of.Employee_ID = s.Employee_ID 
     and year(Order_Date)=2011
group by Country, First_Name, Last_Name
having Value_Sold >= 200
order by Country, Value_Sold desc, Orders desc;
quit;
title;

proc sql;
title "2011 Sales Summary By Country";
select Country, 
       max(Value_Sold) 'Max Value Sold' format=comma9.2,
       max(Orders) 'Max Orders' format=comma7.2, 
       max(Avg_Order) 'Max Average Orders' format = 7.2, 
       min(Avg_Order) 'Min Average Orders' format = 7.2
from (select Country, First_Name, Last_Name, 
       sum(Total_Retail_Price) as Value_Sold,
       count(distinct Order_ID) as Orders,
       Calculated Value_Sold / Calculated Orders as Avg_Order
           from orion.order_fact as of, 
                orion.sales as s 
           where of.Employee_ID = s.Employee_ID 
              and year(Order_Date)=2011
           group by Country, First_Name, Last_Name
           having Value_Sold >= 200)
group by Country     
order by Country;     
quit;
title;

/*Practice: Level 2*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

*Query 1 creates a report aggregating the sum of all salaries for each department;
proc sql;
select Department, sum(salary) as Dept_Salary_Total
from orion.employee_payroll as pay,
     orion.employee_organization as org
where pay.Employee_ID = org.Employee_ID
group by department;
quit;

*Query 2 that creates a report showing the employee ID, name, 
and department for all Orion Star employees.;
proc sql number;
select adr.Employee_ID, Employee_Name, org.Department
from orion.employee_addresses as adr,
     orion.employee_organization as org
where adr.Employee_ID = org.Employee_ID;
quit;

*Final report showing each employee’s salary expressed as a percentage of the 
total salary for that employee’s department.;
proc sql number;
title "Employee Salaries as a Percent of Department Total";
select emp.Department format=$22., 
       emp.Employee_Name format=$28., 
       Salary format=comma9.2, 
       Salary/Dept_Salary_Total as Percent format=percent6.2
from orion.employee_payroll as pay,
    (select adr.Employee_ID, Employee_Name, org.Department
       from orion.employee_addresses as adr,
            orion.employee_organization as org
       where adr.Employee_ID = org.Employee_ID) as emp,
    (select Department, sum(salary) as Dept_Salary_Total
       from orion.employee_payroll as pay,
            orion.employee_organization as org
       where pay.Employee_ID = org.Employee_ID
       group by Department) as sum    
where sum.Department=emp.Department and
         pay.Employee_ID=emp.Employee_ID
order by Department, Percent desc;
quit;
title;

proc sql number;
title "Employee Salaries as a Percent of Department Total";
select emp.Department format=$22.,
       emp.Employee_Name format=$28.,
       Salary format=comma9.2,
       Salary/Dept_Salary_Total as Percent
       format=percent6.2
   from orion.employee_payroll as pay,
    /* In-line View: Employee ID, name and department */
        (select adr.Employee_ID, Employee_Name,
                org.Department
            from orion.employee_addresses as adr,
                  orion.employee_organization as org
            where adr.Employee_ID=org.Employee_ID)
            as emp,
    /* In-line View: Aggregate sum of salary by department */
        (select Department, sum(Salary) as Dept_Salary_Total
            from orion.employee_payroll as pay,
                 orion.employee_organization as org
            where org.Employee_ID=pay.Employee_ID
            group by Department)
            as sum
   where sum.Department=emp.Department and
         pay.Employee_ID=emp.Employee_ID
   order by Department, Percent desc;
quit;
title;


