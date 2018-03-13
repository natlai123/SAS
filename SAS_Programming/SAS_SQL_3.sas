/*This file contains codes from lessons 1 to 5 of SAS SQL Essentials and 
lesson 11 of SAS Progrmming 2*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

/*SAS SQL Essential*/


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

/*same as*/

proc sql;
title "Australian Employees' Birth Months";
select p.Employee_ID 'ID',
       Employee_Name as Name format=$25.,
       City format=$25.,
       month(Birth_Date) 'Birth Month' format=3.
   from orion.employee_payroll as p INNER JOIN
        orion.employee_addresses as a
    ON p.Employee_ID=a.Employee_ID
   where Country='AU'
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

/*same as*/

proc sql;
title "US and Australian Internet Customers";
title2 "Purchasing Foreign-Manufactured Products";
select Customer_Name as Name, 
       Count(*) as Count 'Purchases'
from orion.product_dim p inner join orion.order_fact o  
     on p.Product_ID=o.Product_ID 
     inner join orion.customer c on o.Customer_ID=c.Customer_ID
where Employee_ID=99999999
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

proc means data = orion.employee_payroll n; 
var Employee_ID; 
class Marital_Status;

proc print data = orion.employee_payroll noobs;
where Marital_Status = 'M';
run;

proc print data = orion.employee_donations;
run;

proc means data = orion.employee_payroll n; 
var Employee_ID; class Marital_Status; 
run;

proc means data = orion.employee_donations n; 
var Employee_ID; 
where Recipients is not missing; 
run;

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
/*same as*/
proc sql number;
select p.Employee_ID, Recipients
   from orion.employee_donations as d 
        right join
        orion.employee_payroll as p
   on p.Employee_ID=d.Employee_ID
   where Marital_Status="M" ;*and Recipients is not missing;
quit;

*Using the COALESCE Function to Overlay Columns;

proc sql feedback;
select*
   from orion.transactions t full join orion.customers c 
   on c.ID=t.ID
 order by ID;
quit;

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
       mgr.Employee_Name as Manager_Name 'Manager Name' format = $20. ,
       a.Employee_ID, o.Manager_ID, mgr.Employee_ID 
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

/**/
proc sql;
select Employee_Name, City, Country 
   from orion.employee_addresses
   where Employee_ID = any /*= in*/
      (select Employee_ID
          from orion.employee_payroll
          where month(Birth_Date)=2)
   order by Employee_Name;
quit;


/**/
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

/**/
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


