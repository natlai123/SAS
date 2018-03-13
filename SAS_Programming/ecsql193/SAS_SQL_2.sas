/*This file contains code from lessons 6 to 8 of SAS SQL Essentials*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

/*Chapter 8*/

/*Querying Dictionary Information*/

proc sql;
describe table dictionary.tables;
quit;

/*Displaying Specific Metadata*/

title 'Tables in the ORION Library';
proc sql;
select memname 'Table Names',
       nobs,nvar,crdate
   from dictionary.tables
   where libname='ORION';
quit;
title;

/*Using Dictionary Tables in Other SAS Code*/

title 'Tables in the ORION Library';
proc print data=sashelp.vtable label;
   var memname nobs nvar;
   where libname='ORION';
run;
title;

/*while you can access dictionary information with SAS procedures or the DATA step, 
  it is often more efficient to use PROC SQL instead.
  
   library and table names in uppercase. But, SAS stores column names in mixed case  
*/

title 'Tables Containing an Employee_ID Column';
proc sql;
select memname 'Table Names', name
   from dictionary.columns
   where libname='ORION' and 
         upcase(name)='EMPLOYEE_ID';
quit;
title;

/*Using Dictionary Views*/

title 'SAS Objects by Library';
proc tabulate data=sashelp.vmember format=8.;
   class libname memtype;
   keylabel N=' ';
   table libname, memtype/rts=10 
         misstext='None';
   where libname in ('ORION','SASUSER','SASHELP');
run;
title;

title 'SAS Objects by Library';
proc tabulate data=sashelp.vmember format=8.;
   class libname memtype;
   keylabel N=' ';
   table libname, memtype/rts=10 
         misstext='None';
   where libname in ('ORION','SASHELP');
run;
title;

/*Limiting the Number of Rows That SAS Writes*/

proc sql outobs=10;
title "10 Most Profitable Customers";
select Customer_ID, sum(Unit_Sales_Price-Unit_Cost_Price)
       as Profit_2011 format=comma8.2
   from orion.price_list as p,
        orion.order_fact as o
   where p.Product_ID=o.Product_id
         and year(Order_date)=2011
   group by Customer_ID
   order by Profit_2011 desc;
quit;
title;

/*Limiting the Number of Rows That SAS Reads*/

proc sql inobs=10;
title "orion.price_list - INOBS=10";
select Product_ID, 
       Unit_Cost_price format=comma8.2,
       Unit_Sales_Price format=comma8.2, 
       Unit_Sales_Price-Unit_Cost_Price 
       as Margin format=comma8.2
   from orion.price_list;
quit;
title;


/*Creating User-Defined Macro Variables*/

%let DataSetName=employee_payroll;
%let BigSalary=100000;
%let Libname='orion';

/*Resolving User-Defined Macro Variables*/

proc sql;
   select Employee_ID, Salary
      from orion.&DataSetName
      where Salary>&BigSalary;
quit;

/*Displaying Macro Variable Values*/

%put The value of BigSalary is &BigSalary;
%let DataSetName=Employee_Payroll;
%let BigSalary=100000;
options symbolgen;
proc sql;
title "Salaries > &bigsalary";
   select Employee_ID, Salary
      from orion.&DataSetName
      where Salary > &BigSalary;
quit;
title;

%let DataSetName=Employee_Payroll;
%let BigSalary=100000;
proc sql feedback;
title "Salaries > &bigsalary";
   select  Employee_ID, Salary
      from orion.&DataSetName
      where Salary > &BigSalary;
quit;
title;


/*Using a Query to Generate Macro Values*/

/*Creating a Single Macro Variable*/

%let Dept=Sales;
proc sql noprint;
select avg(Salary)
  into :MEANSALARY
  FROM orion.employee_payroll as p,
        orion.employee_organization as o
   where p.Employee_ID=o.Employee_ID
         and Department=Propcase("&Dept");
reset print number;
title  "&Dept Department Employees Earning";
title2 "More Than The Department Average "
       "Of &MeanSalary";
select p.Employee_ID, Salary
   from orion.employee_payroll as p,
        orion.employee_organization as o
   where p.Employee_ID=o.Employee_ID
         and Department=Propcase("&Dept")
         and Salary > &MeanSalary;
quit;
title;

/*Creating Multiple Macro Variables*/

proc sql noprint;
select avg(Salary),min(Salary),max(Salary)
   into :MeanSalary, :MinSalary, :MaxSalary
   from orion.employee_payroll;
%put Mean: &MeanSalary Min: &MinSalary 
     Max: &MaxSalary;
quit;
/*Let's look at the log. Notice the spaces before the values for MinSalary and MaxSalary.

Remember that macro variables can hold only character values. So, numeric values are converted 
to character values by using the BEST8. format and are right aligned.*/


/*Practice Chpater 7*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql number;
create view orion.phone_list as
select Department format = $25., 
       Employee_Name as Name format = $25., 
       Phone_Number 'Home Phone' format = $16.
from  orion.employee_addresses as adr, 
      orion.employee_organization as org, 
      orion.employee_phones as ph
where adr.Employee_ID=org.Employee_ID and
      adr.Employee_ID=ph.Employee_ID and
      upcase(Phone_Type) = 'HOME';
title 'Engineering Department Home Phone Numbers';
select Name, Phone_Number
from orion.phone_list
where Department="Engineering"
order by Name;      
quit;
title;


/*Practice 8:  Using PROC SQL Options and Displaying the Contents of a Dictionary Table*/

proc sql number;
title 'Dictionary Tables';
select distinct memname, memlabel
from dictionary.dictionaries;
quit;
title;

proc sql number; 
describe table dictionary.columns;
title ' Tables Containing Customer_ID';
select memname, type, length, name
from dictionary.columns
where libname = 'ORION' and 
      UPCASE(name) = 'CUSTOMER_ID'; 
      /*Within the dictionary tables, SAS stores library and table names in uppercase. 
      But, SAS stores column names in the dictionary tables in the same case in which 
      they were defined when created. So, the column names can be all lowercase, all 
      uppercase, or mixed case. */
quit;
title;   
 

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql number; 
title "Dictionary Tables";
select memname as Table, memlabel as Contents, count(*) as Columns
from dictionary.dictionaries
group by Table, Contents;
quit;
title;
 
proc  sql;
title 'Orion Library Tables';
select memname "Table",
       nobs "Rows",
       nvar "Columns", 
       filesize "File Size", 
       maxvar 'Widest Column',
       maxlabel 'Widest Label'
   from dictionary.tables
   where libname='ORION'
         and memtype ne 'VIEW'
   order by memname;
quit;
title;


proc sql number;
title 'ORION Library Table Library';
footnote "* Largest in the Library";
select memname "Table",
    cats(nobs, 
      case 
      when nobs =max(nobs) then '*'
      else ""
      end) "Rows",
    cats(nvar, 
      case 
      when nobs =max(nvar) then '*'
      else ""
      end) "Columns",
    cats(put(filesize,comma12.),
      case 
      when filesize=max(filesize) then "*" 
      else "" 
      end) "File Size (Bytes)",  
    cats(maxvar, 
      case 
      when nobs =max(maxvar) then '*'
      else ""
      end) "Widest Column"
from dictionary.tables 
where libname = 'ORION' and 
      memtype ne 'VIEW';
quit;
title;
footnote;      



/*Practice 8: Creating and Using Macro Variables*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";
%Let DataSet=Price_List;
%Let VariableName= Unit_Sales_Price;
%PUT The value of &DataSet is:;
%PUT The value of &VariableName is:;

proc sql;
title "Highest &VariableName in employee_payroll";
select max(&VariableName)
   from orion.&DataSet ;
quit;
title;

/*Practice: Creating a Macro Variable from an SQL Query*/
 
proc sql number noprint;
title "2011 Purchases by Country";
select Country,
       sum(Total_Retail_Price) format=dollar10.2 as Purchases
   into :Country, 
        :Country_Purchases    
   from orion.customer as c,
            orion.order_fact as o
   where c.Customer_ID=o.Customer_ID
             and year(Order_Date)=2011
   group by Country
   order by Purchases desc;
quit;
title;

proc sql number;
/*Reset statement can go here or not needed*/
title "2011 &Country Customer Purchases";
title2 "Total &Country Purchases: &Country_Purchases";
select Customer_Name, 
          sum(Total_Retail_Price) format=dollar10.2 as Purchases
   from orion.customer as c,
           orion.order_fact as o
   where c.Customer_ID=o.Customer_ID
             and year(Order_Date)=2011
             and Country="US"
   group by Customer_Name
   order by Purchases desc;
quit;
title;
 
 
/*Practice7: Writing Code to Create and Use a View to Provide Consolidated Information*/ 
 
proc sql number;
create view orion.t_shirts as 
select d.Product_ID,
       Supplier_Name format=$20.,
       Product_Name,
       Unit_Sales_Price as Price label='Retail Price'
from orion.product_dim as d, orion.price_list as l
where d.Product_ID = l.Product_ID and 
      lowcase(Product_Name) like "%t-shirt%";
quit; 
 
proc sql number;
title 'Available T-Shirts';
select*
from orion.t_shirts
order by Supplier_Name, Product_ID ;
quit;
title;
 
proc sql number ;
title 'T-shirts under $20';
select Product_ID, Product_Name, Price  format=dollar6.2
from orion.t_shirts
where price < 20
order by Price;
quit;
title;


/*Practice: Writing Code to Create and Use a View That Updates Itself over Time*/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc sql number;
create view orion.current_catalog as
   /*orion.product_dim includes duplicate records*/
   select distinct d.*,
          round((l.Unit_Sales_Price *
            factor**(year(Today())-year(Start_Date))),.01)
             'Current Retail Price' format=dollar13.2 as Price 
      from orion.product_dim as d, 
           orion.price_list as l
      where d.Product_ID=l.Product_ID;
select* from orion.current_catalog;
quit;

proc sql number;
title 'Current Roller Skate Prices';
select Supplier_Name, 
       Product_Name, 
       Price
from orion.current_catalog  
where lowcase(Product_Name) like '%roller skate%'
order by Supplier_Name, Price;
quit;
title;

proc sql number;
title "Current prices > $5.00 higher than original price";
select c.Product_Name, 
       p.Unit_Sales_Price,
       c.Price,
       Price-Unit_Sales_Price as Increase
from orion.current_catalog as c,
     orion.price_list as p
  where c.Product_ID=p.Product_id 
         and calculated Increase gt 5
   order by Increase Desc;
quit;
title;


/*Chapter 6 */ 
 
%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";

proc print data = orion.train_a;
proc print data = orion.train_b;
 
/*Using the UNION Operator with the CORR Modifer*/

proc sql;
select * 
   from orion.train_a
union 
select * 
   from orion.train_b;
quit;

proc sql;
select * 
   from orion.train_a
union all
select * 
   from orion.train_b;
quit;

proc sql;
select * 
   from orion.train_a
union corr
select * 
   from orion.train_b;
quit;

proc sql;
select * 
   from orion.train_a
union corr all
select * 
   from orion.train_b;
quit;


proc sql;
select * 
   from orion.train_a
outer union 
select * 
   from orion.train_b;
quit;

proc sql;
select * 
   from orion.train_a
outer union corr
select * 
   from orion.train_b;
quit;

proc sort data = orion.train_a out = train_a;
by ID;

proc sort data = orion.train_b out = train_b;
by ID;

data datastep1;
set train_a train_b;
by ID;

proc print data = datastep1 label noobs;
run;

data datastep2;
set train_a train_b;
run;

proc print data = datastep2 label noobs;
run;

/*Column names in the result set are determined by the first table.

Corr
It overlays columns by name, not by position.
When used in EXCEPT, INTERSECT, and UNION set operations, it removes any columns not found in both tables.
When used in OUTER UNION set operations, it causes same-named columns to be overlaid.*/

/*Combining Three Queries with the UNION Operator and the ALL Modifier*/

title 'Payroll Report for Level I, II,';
title2 'and III Employees'; 

proc sql;
select 'Total Paid to ALL Level I Staff',  
       sum(Salary) format=comma12.
   from orion.staff
   where scan(Job_Title,-1,' ')='I'
union all
select 'Total Paid to ALL Level II Staff', 
       sum(Salary) format=comma12.
   from orion.staff
   where scan(Job_Title,-1,' ')='II'
union all
select 'Total Paid to ALL Level III Staff',
       sum(Salary) format=comma12.
   from orion.staff
   where scan(Job_Title,-1,' ')='III';
quit;

title;

/*Using the OUTER UNION Operator with the CORR Modifer*/

proc sql;
select * 
   from orion.train_a
outer union corr
select * 
   from orion.train_b
   where EDate is 
         not missing;
quit;

/*Using the EXCEPT Operator with the ALL and CORR Modifers*/

proc sql number ;
select * 
   from orion.train_a
except all corr
select * 
   from orion.train_b
   where EDate is 
         not missing;
quit;


/*Using a Set Operator in an In-Line View*/

title 'Number of Employees Who Completed';
title2 'Training A, But Not Training B';
proc sql;
select count(ID) as Count
   from (select ID, Name from orion.train_a
         except
         select ID, Name from orion.train_b
            where EDate is not missing);
quit;
title;

/*Using the INTERSECT Operator with the ALL Modifier*/

proc sql;
select ID, Name 
   from orion.train_a
intersect all
select ID, Name 
   from orion.train_b
   where EDate is 
         not missing;
quit;

/*Combining Set Operators*/

title "Who on Bob's Team Has Not";
title2 'Started Any Training';
proc sql;
select ID, Name from orion.team
except
(select ID, Name from orion.train_a
 union
 select ID, Name from orion.train_b);
quit;



/*****************************************
Practice: Using Dictionary Tables and Macro Variables
          to Build Low-Maintenance Code
*****************************************/

%let path=/folders/myshortcuts/_myfolders/ecsql193;
libname orion "&path";
 
%let JoinVar=Customer_ID;
%let Library=%upcase(ORION);

  /* 1. Query 1 */
  /* Write a PROC SQL Query that contains the number of tables
     in the Library that contain the JoinVar column, and
     writes that value to a macro variable called Rows. */
    
    proc sql;
    title 'The Macro Variable, Rows, is:' ; 
    select strip(put(count(*),5.)) 
    into :Rows
    from dictionary.columns 
    where Libname = "&Library" and 
          name = "&JoinVar";
    quit;
    title;

  /* 2. Query 2 */
  /* Write a PROC SQL Query that creates a series of macro
     variables called Table1, Table2, ... TableN, etc. 
     (1 for each table in the Library containing the Joinvar
     column) and writes the column name sequentially to
     the Table1-TableN macro variables. N should be the
     number in the Rows macro variable */
    
   proc sql;
   title 'Macro Variables for Table Names';
   select memname
   into :Table1 - :Table&Rows
   from dictionary.columns 
    where Libname = "&Library" and 
          name = "&JoinVar";
    quit;
    title;

  /* 3. Query 3 */
  /* Write a PROC SQL Query that creates a macro variable
     called SourceTables, containing a list of the fully-
     qualified names of all the tables in the Library 
     which contain the column JoinVar, separated by commas. 
     The results will look like this: 
     LIBRARY.Table1, LIBRARY.Table2, ...LIBRARY.TableN
     where N is the number in the Rows macro variable. */
    
    proc sql; 
    select catx('.', libname, memname) 
    into :SourceTables separated by ','
    from dictionary.columns
    where libname="&Library" 
         and NAME="&JoinVar";
    quit;
    

  /*****************************************************
      For the exercise, do not edit below this line!
  *****************************************************/

  /*****************************************************
  This macro program joins all the tables in Library 
  that contain the column JoinVar by JoinVar, producing
  a table called Joined_By_JoinVar. By default, the table 
  is created in the work library. For example, if 
     JoinVar = Employee_ID
  then he table produced is 
     work.Joined_by_employee_id
  *****************************************************/



options mprint;
%macro JoinTheTables(OutLib);
%if &OutLib= %then %let OutLib=work;
%if &Rows gt 1 %then %do;
%do i=1 %to &rows;
proc sql noprint;
   select catx('.',"&&Table&i",Name)
          into :&&Table&i.._Columns separated by "," 
      from dictionary.Columns
      where libname="&Library" 
            and MEMNAME="&&Table&i"
            and Name ne "&JoinVar";
quit;
%end;
%put _user_;
proc sql;;
   create table &OutLib..Joined_by_&JoinVar AS
      select &&Table1..&JoinVar
%do i=1 %to &Rows;
   %let ThisColumn=&&&Table&i.._Columns;
    , &&&ThisColumn
%end;
      from &SourceTables
      where &Table1..&JoinVar=&Table2..&JoinVar
 %do i=2 %to %eval(&Rows-1);
   %let j=%eval(&i+1);
   and &&Table&i...&JoinVar=&&Table&j...&JoinVar 
%end;
;
quit;

%put NOTE:  ************* JoinTheTables Macro *********************;
%put NOTE:  Column &JoinVar found was found in &Rows tables;
%put NOTE:  ************* JoinTheTables Macro *********************;
%end;
%else %if &Rows=1 %then %do;
%put NOTE:  ************* JoinTheTables Macro *********************;
%put NOTE:  Column &JoinVar found only in &Library..&Table1 table;
%put NOTE:  No join could be performed;
%put NOTE:  ************* JoinTheTables Macro *********************;
%end;
%else %do;
%put ERROR:  ************* JoinTheTables Macro *********************;
%put ERROR:  Column &JoinVar not found in any of the &Library tables;
%put ERROR:  ************* JoinTheTables Macro *********************;
%end;
%mend;
%JoinTheTables;
