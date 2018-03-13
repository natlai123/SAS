/*Base SAS code including SAS Programming 2 and Certificate Book*/

%let path=/folders/myshortcuts/_myfolders/ecprg293;
libname orion "&path";

/* SAS Programming 2: Do loop and array */

/*Do Loop*/
/*Array*/
/*Restructuring Data set using Loops and Array*/

*****************************************;


/*Do loop*/

/*Example 1*/

data compound;
	Amount=50000;
	Rate=.045;
	do i=1 to 20;
		Yearly+(Yearly+Amount)*Rate;
		output;
	end;
	do i=1 to 80;
		Quarterly+((Quarterly+Amount)*Rate/4);
		output;
	end;
	/*Which is mathematically equal to: */
	Yearlym1=Amount*((1+ Rate)**20) - Amount;
	Quarterlym1=Amount*((1+ Rate/4)**80) - Amount;
	output;
run;

proc print data=work.compound;
run;

/*Again*/

data compound(drop=i);
   Amount=50000;
   Rate=.045;
   do i=1 to 20;
      Yearly+(Yearly+Amount)*Rate;
   end;
   do i=1 to 80;
      Quarterly+((Quarterly+Amount)*Rate/4);
   end;
   	Yearlym1=Amount*((1+ Rate)**20) - Amount;
	Quarterlym1=Amount*((1+ Rate/4)**80) - Amount;
	output;
run;

proc print data=work.compound;
run;


/*Example 2*/

data invest1;
 do Year = 2008 to 2010 by 1 ; /*Range*/
 Capital + 5000;
 Capital + (Capital*0.45);
 output;
 end;
run;

data invest2;
do Year = 2008, 2009, 2010;   /*Item list*/ 
 Capital + 5000;
 Capital + (Capital*0.45);
 output;
 end;
run;

proc print data = invest1;
proc print data = invest2;

/*Example 3*/

/*Improving this code using do loop*/
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
run;

data forecast;
	set orion.growth;
	do Year=1 to 6;
		Total_Employees=Total_Employees*(1+Increase);
		output;
	end;
run;

proc print data=forecast noobs;
run;

/*Practice*/

data future_expenses (drop=start stop);
	Wages=12874000;
	Retire=1765000;
	Medical=649000;
	Year=year(today());
	start=year(today())+1;
	stop=start+9;
	/* insert a DO loop here */
	do i=start to stop;
	*10-year forcast;
        Year = Year + 1; 
		Wages=Wages*1.06;
		Retire=Retire*1.014;
		Medical=Medical*1.095;
		Total_Cost= sum(Wages, Retire, Medical);
		output;
	end;
run;

proc print data=future_expenses;
	format Wages Retire Medical Total_Cost comma14.2;
	var Year Wages Retire Medical Total_Cost;
run;


/*Do until vs Do while*/

/*Example 1*/

data invest;
Capital = 900000000;
	do until (Capital>1000000);
		Year+1;
		Capital +5000;
		Capital+(Capital*.045);
		output;
	end;
run;

proc print data=invest noobs;
run;

data invest2;
Capital = 900000000;
	do while (Capital<=1000000);
		Year+1;
		Capital +5000;
		Capital+(Capital*.045);
		output;
	end;
run;

proc print data=invest2 noobs;
run;

/*Do while will not execute the loop at all*/


/*Example 2*/

data invest;
	do year=1 to 30 until (Capital>250000);
		Capital +5000;
		Capital+(Capital*.045);
		output;
	end;
run;

data invest2;
	do year=1 to 30 while (Capital<=250000);
		Capital +5000;
		Capital+(Capital*.045);
		output;
	end;
run;

proc print data=invest noobs;
	title "Invest";
	format Capital dollar14.2;
run;

proc print data=invest2 noobs;
	title "Invest2";
	format Capital dollar14.2;
run;
title;

data invest;
   do year=1 to 30 until (Capital>250000);
      Capital +5000;
      Capital+(Capital*.045);
   end;
run;

data invest2;
   do year=1 to 30 while (Capital<=250000);
      Capital +5000;
      Capital+(Capital*.045);
   end;
run;

proc print data=invest;
   title "Invest";
   format Capital dollar14.2;
run;

proc print data=invest2;
   title "Invest2";
   format Capital dollar14.2;
run;
title;

/*Examine the results. You can see that the value of Capital is the same in invest
and invest2. The value of Year is 27 in invest and 28 in invest2. Why is that?

Look back at the DATA steps. Neither DO loop executed enough times for the value
of Year to exceed the specified range. Remember, in a DO UNTIL loop, SAS checks
the condition before incrementing the index variable. In a DO WHILE loop, SAS checks
the condition after incrementing the index variable. So both loops executed 27 times,
but the DO WHILE loop incremented the index variable one more time after the 27th
execution of the statements within the loop.*/


/*Practice*/

data future_expenses (drop=start stop);
	Wages=12874000;
	Retire=1765000;
	Medical=649000;
	Income=50000000;
	Year=year(today()) + 1;
	start=year(today())+1;
	stop=start+9;

	do i=start to stop until (Total_Cost > Income);
		Year + 1;
		Wages=Wages*1.06;
		Retire=Retire*1.014;
		Medical=Medical*1.095;
		Income=Income*1.01;
		Total_Cost=Wages+Retire+Medical;
		output;
	end;
run;

proc print data=future_expenses;
	format Wages Retire Medical Total_Cost comma14.2;
	var Year Income Total_Cost;
run;



data expenses;
	Income=50000000;
	Expenses=38750000;

	do Year=1 to 30 until (Expenses > Income);
		Income=Income*1.01;
		Expenses=Expenses*1.02;
	end;
run;

proc print data=expenses;
	format Income Expenses dollar15.2;
run;

data expenses;
	Income=50000000;
	Expenses=38750000;

	do Year=1 to 30 while (Expenses<=Income);
		Income+(Income*.01);
		Expenses+(Expenses*.02);
end;	
	/*adjust value of Year to account for extra
	increment after final loop iteration.*/
	Year = Year -1;
run;

proc print data=expenses;
	format Income Expenses dollar15.2;
run;


/*Nested Loop*/

* Compare the sets of codes below. Check output and index- Quarter
Need only one output data step in each set of codes;

data invest5;
	do Year=1 to 5;
		Capital+5000;
		do Quarter=1 to 4;
			Capital+(Capital*(.045/4));
		end;
	end;
run;

title 'Show final result only (Quarter was wrong)';
proc print data=invest5;
run;
title;

data invest5;
	do Year=1 to 5;
		Capital+5000;
		do Quarter=1 to 4;
			Capital+(Capital*(.045/4));
			output;
		end;
	end;
run;

title 'Show Quarterly results';
proc print data=invest5;
run;
title;

data invest5;
	do Year=1 to 5;
		Capital+5000;
		do Quarter=1 to 4;
			Capital+(Capital*(.045/4));
		end;
		output;
	end;
run;

title 'Show Yearly results (Quarter was wrong)';
proc print data=invest5;
run;
title;

data invest5;
	do Year=1 to 5;
		Capital+5000;
		do Quarter=1 to 4;
			Capital+(Capital*(.045/4));
			output;
		end;
		output;
	end;
run;

title 'Show redundant output statements that causes extra line of Quarter';
proc print data=invest5;
run;
title;


/*Using loop on three observations*/

data invest6 ;
	set orion.banks;
run;

proc print data = invest6;
run;	

*This DATA step iterates three times because the input data set contains 
three observations (Rates). ;

data invest6 ;
	set orion.banks;
	do Year=1 to 5;
		Capital + 5000;
		do Quarter=1 to 4;
			Capital+(Capital*(Rate/4));
		end;
	end;
run;

proc print data=invest6;
run;

data invest6 ;
	set orion.banks;
	do Year=1 to 5;
		Capital + 5000;
		do Quarter=1 to 4;
			Capital+(Capital*(Rate/4));
		end;
		output;
	end;
run;

proc print data=invest6;
run;

data invest6 ;
	set orion.banks;
	do Year=1 to 5;
		Capital + 5000;
				output;
		do Quarter=1 to 4;
			Capital+(Capital*(Rate/4));
					output;
		end;
	end;
run;

title 'run 5x5x3 =75 lines';
proc print data=invest6;
run;
title;


*****************************************;

/*Array*/


/*Example 1*/

data charity;
	set orion.employee_donations;
	keep employee_id qtr1-qtr4 total;
	array contrib{*} qtr1-qtr4;
	do i=1 to dim(contrib);
		contrib{i}=contrib{i}*1.25;
	end;
	Total=sum(of qtr1--qtr4);
	format total dollar9.2;
run;

proc print data=orion.employee_donations;
	var employee_id qtr1-qtr4;
run;


/*Example 2: Array + nested format */

proc print data=charity;
	sum total;
	format qtr1-qtr4 dollar9.2;
run;

data discount_sales (drop=i);
	set orion.orders_midyear;
	*array mon{*} Month1-Month6;
	array mon{6} Month1-Month6;
	do i=1 to dim(mon);
		mon{i}=mon{i}*0.95;
	end;
    Total = sum(of mon{6});
    /*month1-month6, mon; ... work here as well*/
run;

*Nested format for discount_sales data set;
proc format;
value tmissing
  . = 'Unkown'
  other = [dollar9.2];
run;

proc print data=orion.orders_midyear noobs;
run;

proc print data=discount_sales noobs;
format _numeric_ tmissing.;
sum Total;
run;

*notice the keyword/variable '_numeric_'

Variables that are elements of an array do not need to have similar, 
related, or numbered names. They also do not need to be stored 
sequentially or be adjacent to one another. The order of elements in 
an array is determined by the order in which they are listed in the 
ARRAY statement.;


/*Example 3*/

data special_offer;
	set orion.orders_midyear;
	Total_Sales=sum(of month1-month6);   /*Unadjusted amount*/
	array mon{*} Month1-Month3;
	do i=1 to 3;
		mon{i}=mon{i}*0.9;
	end;
	Projected_Sales=sum(of month1-month6); /*Adjusted amount*/
	Difference = Total_Sales - Projected_Sales;
	keep Customer_ID Difference Total_Sales Projected_Sales;
run;

proc print data=special_offer noobs;
title1 'Total Sales with 10% Discount in First Three Months';
title2 '(Difference = Total_Sales - Projected_Sales)';
  var Difference; 
   /*Only listing one var here would still works because of the sum statment below*/
	format Total_Sales Projected_Sales Difference dollar10.2;
	sum Difference Total_Sales Projected_Sales;
run;
title;


/*Example 4*/

data percent(drop=i);
	set orion.employee_donations;
	array contrib{4} qtr1-qtr4;
	array Pct{4};
	Total=sum(of contrib{*});
	do i=1 to 4;
		pct{i}=contrib{i}/Total;
	end;
run;

proc print data=percent;
	var Employee_ID pct1-pct4 qtr1-qtr4 Total;
	format Pct1-Pct4 percent6. qtr1-qtr4 Total dollar9.2;
run;

/*Difference*/
data change;
	set orion.employee_donations;
	drop i;
	array contrib{4} Qtr1-Qtr4;
	array Diff{3};
	do i=1 to 3;
		diff{i}=contrib{i+1}-contrib{i};
	end;
run;

proc print data=change;
	var Employee_ID Qtr1-Qtr4 Diff1-Diff3;
run;

/*Growth Rate*/
data change;
	set orion.employee_donations;
	drop i;
	array contrib{4} Qtr1-Qtr4;
	array Growth{3};
	do i=1 to 3;
		Growth{i}=((contrib{i+1}-contrib{i})/contrib{i});
	end;
run;

proc print data=change;
	var Employee_ID Qtr1-Qtr4 Growth1-Growth3;
	format Growth1-Growth3 percent6.;
run;



/*Assigning Initial Values to an Array (Lookup Table)*/

data change;
	set orion.employee_donations;
	drop i;
	array contrib{4} Qtr1-Qtr4;
	array Goal{4} (10 20 20 15);
	array Diff{4};
	do i=1 to 4;
		diff{i}=contrib{i}-goal{i};
	end;
run;

proc print data=change;
	var Employee_ID Qtr1-Qtr4 Goal1-Goal4 Diff1-Diff4;
run;

data change;
	set orion.employee_donations;
	drop i;
	array contrib{4} Qtr1-Qtr4;
	array Goal{4}_temporary_ (10 20 20 15);  
	                          /*_tempprary_ gets rid of Goal in the output*/
	array Diff{4};
	do i=1 to 4;
		diff{i}=contrib{i}-goal{i};
	end;
run;

proc print data=change;
	var Employee_ID Qtr1-Qtr4 Diff1-Diff4; /*adding Goal1-Goal4 creates error*/
run;


/*Example 5: Can be transformed to creating dummy variables*/

data compare(drop=i);
	set orion.employee_donations;
	array contrib{4} Qtr1-Qtr4;
	array Diff{4};
	array Goal{4} (10, 20, 20, 15);
	array Target{4} $;  
	do i=1 to 4;
		Diff{i}= contrib{i} - goal{i};
		if Diff{i} = . then Target{i} = '';
		else if Diff{i} >= 0 then Target{i} = "Hit";  /*Conditional statements*/
		else Target{i} = "Not Hit";
	end;
run;

proc print data=compare;
	var Employee_ID Goal1-Goal4 Qtr1-Qtr4 Diff1-Diff4 Target1-Target4;
	*Notice that the initial values that you specified in the ARRAY statement are set for 
    Goal1 through Goal4 and retained in the PDV.;
run;

/*Another similar example*/

data preferred_cust;
	set orion.orders_midyear;
	array mon{6} Month1-Month6;
	array target{6} _temporary_ (200, 400, 300, 100, 100, 200);
	array Over{6};
	do i=1 to 6;
		if mon{i} > target{i} then
			Over{i}=mon{i} - target{i};
	end;
	Total_Over=sum(of Over{*});
	if Total_Over > 500;
	keep Customer_ID Over1-Over6 Total_Over;
run;

proc print data=preferred_cust noobs;
run;


/*Practice: Using a Character Array for Table Lookup*/
/*The two versions differs in terms of how they handle the scores of students*/

*Version1;

proc print data = orion.test_answers;
run;

data Passed Failed;
	set orion.test_answers;
	array akey{10} $1 _temporary_ ('A', 'C', 'C', 'B', 'E',   /*The model answers*/
	'E', 'D', 'B', 'B', 'A');
	array S{10} _temporary_;
	array quest{10} Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10;           /*Students' answers*/
	do i=1 to 10;
		if quest{i}=akey{i} then
			S{i}=1;
		else
			S{i}=0;
	end;
	Score=sum(of S{*});
	if Score >=7 then
		output Passed;
	else
		output Failed;
	keep Employee_ID Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10 Score;
run;

title 'Passed';
proc print data=Passed;
	var Employee_ID Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10 Score;
run;

title "Failed";
proc print data=Failed;
	var Employee_ID Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q10 Score;
run;

*Version 2;

data passed failed;
	set orion.test_answers;
	drop i;
	array response{10} Q1-Q10;
	array answer{10} $ 1 _temporary_ 
             ('A', 'C', 'C', 'B', 'E', 'E', 'D', 
		'B', 'B', 'A');
	Score=0;
	do i=1 to 10;
		if answer{i}=response{i} then
			Score+1;
	end;
	if Score ge 7 then
		output passed;
	else
		output failed;
run;

title 'Passed';
proc print data=passed;
run;
title;

title 'Failed';
proc print data=failed;
run;
title;

****************************************;

/*Restructuring Data set*/

* Narrow/long (for proc freq) to wide/short data set ;

/* Wide Dataset -> Narow Dataset -> Wide Dataset
  Using Do-loop and Proc Transpose */

proc print data = orion.employee_donations (obs=10 drop=Recipients Paid_By);
run;

data rotate (keep=Employee_ID Period Amount);
	set orion.employee_donations
            (drop=Recipients Paid_By);
	array contrib{4} qtr1-qtr4;
	do i=1 to 4;
		if contrib{i} ne . then
			do;
				Period=cats("Qtr", i);
				Amount=contrib{i};
				output;
			end;
	end;
run;

proc print data=rotate ;
run;

proc transpose  data = rotate out = rotate1;
by Employee_ID;
id period;
var amount;
run;

proc print data = rotate1;
var Employee_ID	Qtr1 Qtr2 Qtr3 Qtr4;
run;

proc freq data=rotate;
	tables Period ; */ nocum nopct;
run;


/**/

proc print data= orion.travel_expense;
run;

data travel;
	set orion.travel_expense;
	keep Trip_ID Employee_ID Expense_Type Amount;
	array exp{*} exp1-exp5;
	array type{5} $14 _temporary_ 
	('Airfare', 'Hotel', 'Meals', 'Transportation', 'Miscellaneous');
	do i=1 to 5;
		if exp{i} ne . then
			do;
				Expense_Type=type{i};
				Amount=exp{i};
				output;
			end;
	end;
run;

proc print data=travel;
	var Trip_ID Employee_ID Expense_Type Amount;
	format Amount dollar8.2;
run;

proc print data= orion.orders_midyear;
run;


data sixmonths (keep=Customer_ID Month Sales);
	set orion.orders_midyear;
	array mon{6} Month1-Month6;
	do i=1 to 6;
		*if mon{i} ne . then
			do;
				Month=i;
				Sales=mon{i};
				output sixmonths;
			*end;
	end;
run;

proc print data=sixmonths;
run;

****************************************;
