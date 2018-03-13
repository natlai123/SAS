
libname sasuser "/folders/myshortcuts/sf__myfolders/certprep";
*proc print data = sasuser.payrollmaster;
*run;

/*Check LOG (Debugging)*/
proc sql feedback;
	select * from sasuser.staffchanges;

	/*Comparing outobs = and (obs= )*/
proc sql outobs=10;
	create view abc as select empid, jobcode, salary, salary*.06 as Bonus 
		format=dollar11.2
		/*Keep in mind that new columns exist only for the duration of the query,
		unless a table or a view is created.*/
		from sasuser.payrollmaster where salary<32000 order by jobcode;
	select* from abc;
quit;

proc sql ;
	select salcomps.empid, lastname, newsals.salary, newsalary from 
		sasuser.salcomps, sasuser.newsals (obs=10) where salcomps.empid=newsals.empid 
		order by lastname;
	select* from sasuser.frequentflyers (obs=10);
quit;

/*
Keyword: distinct

To remove rows that contain duplicate values, add the keyword DISTINCT
to the SELECT statement, following the keyword SELECT,
In PROC SQL, you can use the keyword UNIQUE instead of DISTINCT.
However, UNIQUE is not ANSI standard.
*/
libname sasuser "/folders/myshortcuts/sf__myfolders/certprep";

proc sql ;
	select flightnumber, destination from sasuser.internationalflights order by 1;

proc sql ;
	select distinct flightnumber, destination from sasuser.internationalflights 
		order by 1;

proc sql ;
	select unique flightnumber, destination from sasuser.internationalflights 
		order by 1;

	/*Generating a new variable*/
libname sasuser "/folders/myshortcuts/sf__myfolders/certprep";

proc sql outobs=10;
	select flightnumber, date, destination, boarded + transferred + nonrevenue as 
		Total from sasuser.marchflights;
quit;

proc sql outobs=10;
	select flightnumber, date, destination, boarded + transferred + nonrevenue as 
		Total from sasuser.marchflights where total < 100;

	/*This error message is generated because, in SQL queries,
	the WHERE clause is processed before the SELECT clause.
	The SQL processor looks in the table for each column named in
	the WHERE clause. The table Sasuser.Marchflights does not contain
	a column named Total, so SAS generates an error message.*/
quit;

/*
Keyword: Calculated
Remember that PROC SQL does not process the clauses in the same
order as they are written. PROC SQL evaluates the WHERE clause
before the SELECT statement. When PROC SQL processes the WHERE clause,
the Bonus column doesn't yet exist. The WHERE clause works with
columns that exist in the source table.

You can repeat the calculation in the WHERE clause, which follows ANSI guidelines.
This will produce the desired outcome.

You can also use a SAS enhancement to handle this situation. The SAS keyword
CALCULATED enables you to use the results of an expression in the same
SELECT statement or in the WHERE clause.
*/
proc sql outobs=10;
	select flightnumber, date, destination, boarded + transferred + nonrevenue as 
		Total from sasuser.marchflights where calculated total < 100;
quit;

proc sql outobs=10;
	select flightnumber, date, destination, boarded + transferred + nonrevenue as 
		Total, calculated total/2 as Half from sasuser.marchflights;
quit;

/*Order by var DESE ! vs proc sort ... by descending var*/
proc sql ;
	select membertype, sum(milestraveled) as TotalMiles from 
		sasuser.frequentflyers group by membertype order by TotalMiles desc;
quit;

/*Having*/
proc sql ;
	select jobcode, avg(salary) as Avg from sasuser.payrollmaster group by jobcode 
		having Avg>40000 order by jobcode;
quit;

libname sasuser "/folders/myshortcuts/sf__myfolders/certprep";

/*You must place the TITLE and FOOTNOTE statements in either of the following locations:
•before the PROC SQL statement
•between the PROC SQL statement and the SELECT statement.*/
proc sql outobs=20;
	title 'Job Groups with Average Salary';
	title2 '> Company Average';
	select jobcode, avg(salary) as AvgSalary format=dollar11.2, count(*) as Count 
		from sasuser.payrollmaster group by jobcode having avg(salary) >
(select avg(salary) from sasuser.payrollmaster) order by avgsalary desc;
	libname sasuser "/folders/myshortcuts/_myfolders/certprep";

proc sql outobs=15;
	title 'Current Bonus Information';
	title2 'Employees with Salaries > $75,000';
	select empid label='Employee ID', jobcode label='Job Code', salary, salary * 
		.10 as Bonus format=dollar12.2 from sasuser.payrollmaster where salary>75000 
		order by salary desc;
quit;

/*Adding a Character Constant to Output:'bonus is:',
salary * .10 format=dollar12.2  */
proc sql outobs=15;
	title 'Current Bonus Information';
	title2 'Employees with Salaries > $75,000';
	select empid label='Employee ID', jobcode label='Job Code', salary, 
		'bonus is:', salary * .10 format=dollar12.2 Label='Type_Numeric', catx(': ', 
		'Bonus is', put(salary, dollar12.2) ) as Catx_Type_Character from 
		sasuser.payrollmaster where salary>75000 order by salary desc;
quit;

/*I like this piece*/
/*Summarizing and Grouping Data*/
libname sasuser "/folders/myshortcuts/_myfolders/certprep";

proc sql outobs=10;
	select sum(boarded, transferred, nonrevenue) as Total from 
		sasuser.marchflights;
	*Because the function contains multiple arguments, the statistic is 


				calculated across the three columns for each row to produce the output.;

proc sql ;
	select avg(salary)as AvgSalary from sasuser.payrollmaster;
	*The function calculates the statistic down the Salary column 
to display a single value:;

proc sql outobs=10;
	select jobcode, avg(salary) as AvgSalary from sasuser.payrollmaster;

proc sql outobs=10;
	select jobcode, avg(salary) as AvgSalary from sasuser.payrollmaster group by 
		jobcode;

proc sql outobs=10;
	select jobcode from sasuser.payrollmaster group by jobcode;

	/*=Order by*/
	/* WARNING: A GROUP BY clause has been transformed into an ORDER BY clause because neither the
	SELECT clause nor the optional HAVING clause of the associated table-expression referenced a
	summary function.*/
libname sasuser "/folders/myshortcuts/sf__myfolders/certprep";

	/* Keyword: Count
	
	The COUNT summary function counts only the nonmissing values; missing values are ignored.
	Many other summary functions also ignore missing values. For example, the AVG function
	returns the average of the nonmissing values only. When you use a summary function with
	data that contains missing values, the results might not provide the information that you
	expect. It is a good idea to familiarize yourself with the data before you use summary
	functions in queries.
	
	Note:The COUNT summary function is the only function that enables you
	to use an asterisk (*) as an argument.*/
proc print data=sasuser.payrollmaster;
run;

proc sort data=sasuser.payrollmaster;
	by jobcode;
run;

proc means data=sasuser.payrollmaster mean nonobs;
	by jobcode;
	id jobcode;
run;

proc sql ;
	select count(*) as Count from sasuser.payrollmaster;

proc sql ;
	select substr(jobcode, 1, 2) label='Job Category', count(*) as Count from 
		sasuser.payrollmaster group by 1;

proc sql ;
	select count(distinct jobcode) as Count from sasuser.payrollmaster;

proc sql ;
	select jobcode, avg(salary) as AvgSalary format=dollar11.2 from 
		sasuser.payrollmaster group by jobcode;

proc sql flow=15;
	select jobcode, avg(salary) as AvgSalary format=dollar11.2 from 
		sasuser.payrollmaster group by jobcode having avg(salary) < 56000;
	*having AvgSalary >56000;

	/*If you omit the GROUP BY clause in a query that contains a HAVING clause,
	then the HAVING clause and summary functions (if any are specified) treat the
	entire table as one group.*/
	/*
	Remerging occurs whenever any of the following conditions exist:
	
	• The values returned by a summary function are used in a calculation.
	• The SELECT clause specifies a column that contains a summary function
	and other column(s) that are not listed in a GROUP BY clause.
	• The HAVING clause specifies one or more columns or column expressions
	that are not included in a subquery or a GROUP BY clause.
	*/
proc sql ;
	select empid, salary, (salary/sum(salary)) as Percent format=percent8.2 from 
		sasuser.payrollmaster where jobcode contains 'NA';

	/*
	Subqueries = nested queries, inner queries, and sub-selects
	Types = correlated, uncorelated
	
	The table that a subquery references can be either the same as or different
	from the table referenced by the outer query. In the PROC SQL query shown above,
	the subquery selects data from the same table as the outer query.
	
	*/
proc sql ;
	select jobcode, avg(salary) as AvgSalary format=dollar11.2 from 
		sasuser.payrollmaster group by jobcode having avg(salary) >
(select avg(salary) from sasuser.payrollmaster);
quit;

libname sasuser "/folders/myshortcuts/sf__myfolders/certprep";

proc sql ;
	select empid, lastname, firstname, city, state from sasuser.staffmaster where 
		empid in select empid from sasuser.payrollmaster where month(dateofbirth)=2;

	/* using () for the inner qury is better*/
	/*Any vs Max*/
proc sql number;
	select empid, jobcode, dateofbirth from sasuser.payrollmaster where jobcode 
		in ('FA1', 'FA2') and dateofbirth < any
(select dateofbirth from sasuser.payrollmaster where jobcode='FA3');

proc sql number;
	select empid, jobcode, dateofbirth from sasuser.payrollmaster where 
		jobcode=in ('FA1', 'FA2') and dateofbirth < 
(select max(dateofbirth) from sasuser.payrollmaster where jobcode='FA3');

	/*Outer Join*/
proc sql number;
	title 'All On-time March Flights';
	select m.date, m.flightnumber label='Flight Number', m.destination 
		label='Left', f.destination label='Right', delay label='Delay in Minutes' 
		from sasuser.marchflights as m full join sasuser.flightdelays as f on 
		m.date=f.date and m.flightnumber=f.flightnumber where delay <=0 order by 
		delay;

	/*The COALESCE function requires that all arguments have the same data type.*/
	/*Comparing Proc SQL and Data Merge
	
	PROC SQL joins do not require sorted or indexed tables.
	
	PROC SQL joins do not require that the columns in join expressions have the same name.
	
	PROC SQL joins can use comparison operators other than the equal sign (=).
	
	Unlike other queries, an in-line view cannot contain an ORDER BY clause*/
	/*The subquery*/
proc sql ;
	select destination, avg(delay) as average, max(delay) as max, sum(delay > 0) 
		as late, sum(delay <=0) /*Boolean expression*/
		as early from sasuser.flightdelays group by destination;
quit;

proc sql ;
	title "Flight Destinations and Delays";
	select destination, average format=3.0 label='Average Delay', max format=3.0 
		label='Maximum Delay', late/(late+early) as prob format=5.2 
		label='Probability of Delay' from 
(select destination, avg(delay) as average, max(delay) as max, sum(delay > 0) 
		as late, sum(delay <=0) as early from sasuser.flightdelays group by 
		destination) order by 2;

	/*Suppose you want to list the names of supervisors for the crew on the flight
	to Copenhagen on March 4, 2000. To solve this problem, you need to use the
	following four tables.
	
	Data:
	Sasuser.Flightschedule (EmpID, Date, Destination)
	Sasuser.Staffmaster (EmpID, FirstName, LastName, State)
	Sasuser.Payrollmaster (EmpID, JobCode)
	Sasuser.Supervisors (EmpID, State, JobCategory)
	
	•Technique 1: using PROC SQL subqueries, joins, and in-line views
	•Technique 2: using a multi-way join that combines four different tables and
	a reflexive join (joining a table with itself)
	•Technique 3: using traditional SAS programming (a series of PROC SORT and DATA
	steps, followed by a PROC PRINT step)
	
	Supervisors live in the same state as the employees that they supervise.
	There is one supervisor for each state and job category.*
	
	/* Find the crew for the flight. */
	/* Find the State and job code for the crew. */
	/* Find the supervisor IDs. */
	/* Find the names of the supervisors. */
proc print data=sasuser.flightschedule (obs=5);
proc print data=sasuser.staffmaster (obs=5);
proc print data=sasuser.payrollmaster (obs=5);
proc print data=sasuser.supervisors (obs=5);
	/*Technique 1*/
proc sql number;
	select firstname, lastname from sasuser.staffmaster where empid in
	(select empid from sasuser.supervisors as m, (select substr(jobcode, 1, 2) as 
		JobCategory, state from sasuser.staffmaster as s, sasuser.payrollmaster as p 
		where s.empid=p.empid and s.empid in
		(select empid from sasuser.flightschedule where date='04mar2000'd and 
		destination='CPH')) as c where m.jobcategory=c.jobcategory and 
		m.state=c.state);

	/*Technique 2*/
proc sql number;
	select distinct e.firstname, e.lastname from sasuser.flightschedule as a, 
		sasuser.staffmaster as b, sasuser.payrollmaster as c, sasuser.supervisors as 
		d, sasuser.staffmaster as e where a.empid=b.empid and a.empid=c.empid and 
		a.destination='CPH' and a.date='04mar2000'd and 
		d.jobcategory=substr(c.jobcode, 1, 2) and d.state=b.state and d.empid=e.empid;

	/*Reflexive join. As you can see in the FROM clause, Sasuser.Staffmaster
	is assigned a different table alias each time it is read: first b, then e.
	The table is read the first time (alias b) to look up the states of the
	Copenhagen crew members, the second time (alias e) to look up the names of
	the supervisors.*/
proc sql outobs=10;
	select firstname, lastname from sasuser.staffchanges;

proc sql outobs=10;
	select firstname, lastname from sasuser.staffmaster;

proc sql ;
	select firstname, lastname from sasuser.staffchanges intersect all select 
		firstname, lastname from sasuser.staffmaster;

proc sql feedback number;
	select * from sasuser.mechanicslevel1 outer union corr select * from 
		sasuser.mechanicslevel2 outer union corr select * from 
		sasuser.mechanicslevel3 order by empid;

proc sql feedback number;
	select * from sasuser.mechanicslevel1 union corr select * from 
		sasuser.mechanicslevel2 union corr select * from sasuser.mechanicslevel3 
		order by empid;

	/*The Exist Operator*/
libname sasuser "/folders/myshortcuts/sf__myfolders/certprep";

proc sql ;
	select lastname, firstname from sasuser.flightattendants where not exists
(select 1 from sasuser.flightschedule where 
		flightattendants.empid=flightschedule.empid);
		

	/*Creating and Managing Tables Using PROC SQL*/
	
	
data payrollmaster_new;
	set sasuser.payrollmaster;
data payrollmaster1;
	set sasuser.payrollmaster;
data payrollmaster2;
	set sasuser.payrollmaster;
data payrollmaster3;
	set sasuser.payrollmaster;
data payrollmaster4;
set sasuser.payrollmaster;

	
proc sql ;
	create table work.flightdelays2 like sasuser.flightdelays;

proc sql ;
	describe table work.flightdelays2;

	/*Creating Tables that have Integrity Constraint
	
	Constraint Type:
	
	CHECK
	NOT NULL
	UNIQUE
	PRIMARY KEY
	FOREIGN KEY
	
	When you add an integrity constraint to a table that contains data, SAS checks all data
	values to determine whether they satisfy the constraint before the constraint is added.
	
	You can use integrity constraints in two ways, general and referential. General constraints
	enable you to restrict the data values accepted for a column in a single table. Referential
	constraints enable you to link the data values of a column in one table to the data values
	of columns in another table.
	
	A referential integrity constraint is created when a PRIMARY KEY integrity constraint in one
	table is referenced by a FOREIGN KEY integrity constraint in another table. There are two steps
	that must be followed to create a referential integrity constraint:
	
	1.Define a PRIMARY KEY constraint on the first table.
	2.Define a FOREIGN KEY constraint on other tables.
	
	Just like a column, an integrity constraint must have a unique name within the table. If you
	create an integrity constraint by specifying a column constraint in a column specification, then
	SAS automatically assigns a name to the constraint. The form of the constraint name depends on the
	type of constraint, as _CK0001, _FK0003_, etc.
	*/
proc sql ;
	create table work.employees1
(ID char (5) primary key, Name char(10), Gender char(1) not null check(gender 
		in ('M', 'F')), HDate date label='Hire Date');

proc sql ;
	create table work.employees2
(ID char (5), Name char(10), Gender char(1), HDate date label='Hire Date', 
		constraint ID01 primary key (ID), constraint Gender01 not null (Gender), 
		constraint Gender02 check(gender in ('M', 'F')) );

proc sql ;
	create table work.discount2
(Destination char(3) not null, BeginDate num Format=date9., EndDate num 
		format=date9., Discount num check(discount le 0.5));

proc sql ;
	create table work.discount3
(Destination char(3), BeginDate num Format=date9., EndDate num format=date9., 
		Discount num, constraint ok_discount check (discount le .5), constraint 
		notnull_dest not null(destination));

proc sql ;
	insert into work.discount3 values('CDG', '03MAR2000'd, '10MAR2000'd, .15) 
		values('LHR', '10MAR2000'd, '12MAR2000'd, .55);

	/*UNDO_POLICY= REQUIRED/ NONE/ OPTIONAL
	
	Note:The ANSI standard for SQL includes a ROLLBACK statement that is used for UNDO processing.
	The ROLLBACK statement is not currently supported in PROC SQL.*/
proc sql undo_policy=none;
	create table work.discount4
(Destination char(3), BeginDate num Format=date9., EndDate num format=date9., 
		Discount num, constraint ok_discount check (discount le .5), constraint 
		notnull_dest not null(destination));
	insert into work.discount4 values('CDG', '03MAR2000'd, '10MAR2000'd, .15) 
		values('LHR', '10MAR2000'd, '12MAR2000'd, .55);

	/*NOTE: 2 rows were inserted into WORK.DISCOUNT4 -- of these 1 row was rejected as an ERROR,
	leaving 1 row that was inserted successfully.*/
	
proc sql ;
	describe table constraints work.discount4;
	describe table constraints work.employees2;
quit;

/*Updating values in existing table rows*/


proc sql ;
	update work.payrollmaster_new set salary=salary*1.05 where jobcode like '__1';

proc sql ;
	update work.payrollmaster_new set salary=salary* case when substr(jobcode, 3, 
		1)='1' then 1.05 when substr(jobcode, 3, 1)='2' then 1.10 when 
		substr(jobcode, 3, 1)='3' then 1.15 else 1.08 end;

	/*better than using multiple update statements*/
	
proc sql ;
	update work.payrollmaster_new set salary=salary*1.05 where substr(jobcode, 3, 
		1)='1';
	update work.payrollmaster_new set salary=salary*1.10 where substr(jobcode, 3, 
		1)='2';
	update work.payrollmaster_new set salary=salary*1.15 where substr(jobcode, 3, 
		1)='3';

proc sql ;
	update work.payrollmaster_new set salary=salary* case substr(jobcode, 3, 1) 
		when '1' then 1.05 when '2' then 1.10 when '3' then 1.15 else 1.08 end;

proc sql outobs=10;
	select lastname, firstname, jobcode, 
	       case substr(jobcode, 3, 1) 
	       when '1' then 'junior'
	       when '2' then 'intermediate' 
	       when '3' then 'senior' 
	       else 'none' 
	       end 
		as JobLevel 
from sasuser.payrollmaster, sasuser.staffmaster 
where staffmaster.empid=payrollmaster.empid;

/*Delete Clause*/

proc sql;
delete * from work.discount3;

proc sql;
create table work.frequentflyers2 as
select ffid, milestraveled,
pointsearned, pointsused
from sasuser.frequentflyers;

proc sql;
delete from work.frequentflyers2
where pointsearned-pointsused <= 0;

/*Altering Columns in a Table (Alter Table claue)

Note:You cannot use the ALTER TABLE statement with views.
Note:The ALTER TABLE statement also supports similar clauses that add, drop, and modify 
     integrity constraints in an existing table. 
     
You cannot use the ALTER TABLE statement with views.

To modify the attributes of one or more existing columns in a table, use the MODIFY clause
in the ALTER TABLE statement. You can use the MODIFY clause to change a column's

•length (column width) — for a character column only
•informat
•format
•label.Note:You cannot use the MODIFY clause to do the following:
•change a character column to numeric or vice versa. To change a column's data type, drop the 
 column and then add it (and its data) again, or use the DATA step.
•change a column's name. You cannot change this attribute by using the ALTER TABLE statement. 
 Instead, you can use the SAS data set option RENAME= or the DATASETS procedure with the RENAME 
 statement.
     
*/
    
proc sql;
select* from work.payrollmaster4 (obs=10);
alter table work.payrollmaster4
add Bonus num format=comma10.2,
Level char(3);
select* from work.payrollmaster4 (obs=10);
quit;

data work.payrollmaster4_new;
set work.payrollmaster4;
Bonus = 10*empid ;
format Bonus dollar8.;
Level = 'fsfese';

proc print data =  work.payrollmaster4_new;
run;

proc sql;
drop table work.payrollmaster4;


proc sql;
insert into work.payrollmaster4 (Bonus, Level)
values(100, '228')
values(101, '227')
values(102, '226')
values(103, '225')
values(104, '224')
values(105, '223')
values(106, '221');
select* from work.payrollmaster4;

proc sql;
describe table constraints work.payrollmaster4;
delete from work.payrollmaster4
where empid is missing;
select* from  work.payrollmaster4;

proc sql;
alter table work.payrollmaster4
drop bonus, level;

proc sql;
alter table work.payrollmaster4
modify salary format=dollar11.2 label="Salary Amt";
select* from work.payrollmaster4;

proc sql;
alter table work.payrollmaster4
modify salary format=dollar11. label="Salary Amt";
select* from work.payrollmaster4;

/*Note:When modifying the width of a character variable, it is possible to truncate 
 the variable's value if the length specification is too small.*/

proc sql;
select* from work.payrollmaster2;
alter table work.payrollmaster2
modify jobcode char(2);
select * from payrollmaster2;

/*Multiple Actions*/
proc sql;
select * from work.payrollmaster3;
alter table work.payrollmaster3
add Age num
modify dateofhire date format=mmddyy10.
drop dateofbirth, gender;
select* from work.payrollmaster3;

/*Dropping Tables*/
proc sql;
drop table work.payrollmaster4;




