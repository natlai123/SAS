/*Base SAS code including SAS Programming 2 and Certificate Book*/

%let path=/folders/myshortcuts/_myfolders/ecprg293;
libname orion "&path";


/* SAS Programming 2 */

/*Manuplating numeric data*/
/*Debugging Putlog*/
/*Merging data*/
/*Inputing raw data*/

*************************************************************;

/*Manuplating numeric data*/

/*Put() vs Input()*/

proc contents data=orion.shipped varnum;
run;

proc print data = orion.shipped (obs=10);
run;

*By default, PROC CONTENTS and PROC DATASETS list variables alphabetically. To
list variable names in the order of their logical position (or creation order) 
in the data set, you can specify the VARNUM option in PROC CONTENTS or in the 
CONTENTS statement in PROC DATASETS.;

data shipping_notes;
	set orion.shipped;
	length Comment $ 21.;
	Comment=cat('Shipped on ', put(Ship_Date, mmddyy10.));
	Total=Quantity * input(Price, dollar7.2);
run;

proc print data=shipping_notes;
	format Total dollar7.2;
run;


/**/

proc contents data = orion.us_newhire;
run;

proc print data=orion.us_newhire(obs=10);
run;

data us_converted(drop=iden tphone bday);
length Telephone $ 8;
set orion.us_newhire(rename=(ID=iden Telephone=tphone Birthday=bday));
    ID=input((compress(iden, '-')), 12.);

	/*Default length set to 200*/
Telephone=substr(put(tphone, 7.), 1, 3)!! '-' !! substr(put(tphone, 7.), 4, 4);

	/*
	If you change the above line to "Telephone = substr(put(tphone, 8.),1,3)!! '-' ..." the outout will be
	shortened to 54-7887. I suspect the reason is that there is a blank in the original Telephone variable \
	before the numbers such that the put() function (with 8.) keeps the blank and the substr() fn extract
	the blank with the two numbers, casuing the first part of the character phrase to be two-digited
	
	Also the above line of code is the same as:
	Telephone = cat(substr(put(tphone, 7.),1,3),'-', substr(put(tphone, 7.),4,4);
	*/
Birthday=input(bday, date9.);
run;

title 'US New Hires';
proc print data=US_converted;
format Birthday mmddyy10.;
run;
title;


/**/

proc print data = orion.newcompetitors (obs=10);
run;

data work.small;
	set orion.newcompetitors;
	if input(substr(left(substr(ID, 3)), 1, 1), 1.) =1;
	City=propcase(City);
run;

title 'New Small-Store Competitors';
proc print data=work.small noobs;
run;
title;


/**/

proc contents data=orion.biz_list;
run;

proc print data=orion.biz_list (obs=10);
run;

data charities(drop=Code_Rt);
	length ID $ 5;
	set orion.biz_list;
	Code_Rt=right(Acct_Code);
	if char(Code_Rt, 6)='2';
	ID=trim(left(substr(Code_Rt, 1, 5)));
run;

proc print data=charities;
run;

data charities;
	length ID $ 5;
	set orion.biz_list;
	if input(substr(Acct_Code, length(Acct_Code), 1), 1.)=2;
	ID=substr(Acct_Code, 1, length(Acct_Code)-1);
	Name=propcase(Name);
run;

proc print data=charities;
run;


*************************************************************;

/*Debugging Putlog*/

/*Formatting Character Values with the PUTLOG Statement

By default, the PUTLOG statement writes character values with the standard character format $w.
This format left-justifies values and removes leading blanks. But, sometimes you might want to
apply a different format.

The END= option creates a variable that can be used to detect the last observation being read.  
It does not control whether the last observation is executed.

You can use the value of _N_ to conditionally execute a PUTLOG statement
or a series of PUTLOG statements.*/

/**/
 
 data work.donate;
 set orion.donate end = last; 
 if _n_ =1 then putlog "First iteration";
 if last = 1 then do;
 putlog 'Final values of variables';
 putlog _all_;
 end;
 run;

/* 
 Valid examples of putlog include:

 putlog customer=;
 putlog 'Note: write this in the log';
 putlog customer $quote13.;
 putlog _all_; write the values of all the variables to the log.

*/ 

/*Example*/

data us_mailing;
	set orion.mailing_list (obs=10);
	drop Address3;
	length City $ 25 State $ 2 Zip $ 5;
	putlog _n_=;
	putlog "Looking for country";
	if find(Address3, 'US');
	putlog "Found US";
	Name=catx(' ', scan(Name, 2, ','), scan(Name, 1, ','));
	City=scan(Address3, 1, ',');
	State=scan(address3, 2, ',');
	Zip=scan(Address3, 3, ',');
	putlog State= Zip=;
	put State Zip;
run;

proc print data=us_mailing;
	title 'Current Output from Program'; /*State and Zip are truncated.*/ 
run;
title;

data us_mailing;
	set orion.mailing_list (obs=10);
	drop Address3;
	length City $ 25 State $ 2 Zip $ 5;
	putlog _n_=;
	putlog "Looking for country";
	if find(Address3, 'US');
	putlog "Found US";
	Name=catx(' ', scan(Name, 2, ','), scan(Name, 1, ','));
	City=scan(Address3, 1, ',');
	State=scan(address3, 2, ',');
	Zip=scan(Address3, 3, ',');
	putlog State = $quote4. Zip = $quote7.;
run;

proc print data=us_mailing;
	title 'Current Output from Program';
run;
title;

/*The $QUOTEw. format writes a character value enclosed in double quotation marks
and preserves any leading spaces. If the value of the variable City is Philadelphia with a leading
space, the statement shown here writes City=" Philadelphia", including the leading space, to the log.

You can increase the value of the format width beyond the minimum to ensure that you can see
the full value of the variable in the log. The specified format width of 22 is more than wide
enough to accommodate the 12 characters in Philadelphia, the leading space, and the quotation
marks which are part of the format.*/

/*After using the LEFT function to remove the leading blanks.*/

data us_mailing;
	set orion.mailing_list (obs=10);
	drop Address3;
	length City $ 25 State $ 2 Zip $ 5;
	putlog _n_=;
	putlog "Looking for country";
	if find(Address3, 'US');
	putlog "Found US";
	Name=catx(' ', scan(Name, 2, ','), scan(Name, 1, ','));
	City=scan(Address3, 1, ',');
	State=left(scan(address3, 2, ','));   /***/
	Zip=left(scan(Address3, 3, ','));
	putlog State = $quote4. Zip = $quote7.;
run;

proc print data=us_mailing;
	title 'Current Output from Program';
run;
title;

*Deleting putlog statements;

data us_mailing;
	set orion.mailing_list;
	drop Address3;
	length City $ 25 State $ 2 Zip $ 5;
	if find(Address3, 'US');
	Name=catx(' ', scan(Name, 2, ','), scan(Name, 1, ','));
	City=scan(Address3, 1, ',');
	State=left(scan(address3, 2, ','));
	Zip=left(scan(Address3, 3, ','));
run;

proc print data=us_mailing;
	title 'Current Output from Program';
run;
title;


* Proc Sort (this is unrelated to debugeing but uses the same dataset);

proc sort data = us_mailing
 SORTSEQ = Linguistic (STRENGTH = primary); 
by Name;
run;

proc print data=us_mailing;
	title 'Current Output from Program';
run;


*************************************************************;

/*Merging Data*/

proc sort data=orion.order_fact
     out=work.orders_2007;
   by Customer_ID;
   where year(Order_Date)=2007;
run;
 
data custord;
   merge orion.customer
         work.orders_2007;
   by Customer_ID;
run;

proc print data=custord;
run;

data custord;
   merge orion.customer(in=cust)
         work.orders_2007(in=order);
   by Customer_ID;
   if cust and order;
run;

%let path=/folders/myfolders/ecprg293; 
libname orion "&path";

data orderdata(keep=Customer_Name Product_ID
                 Quantity Total_Retail_Price)
     noorders(keep=Customer_Name Birth_Date);
   merge orion.customer
         work.orders_2007(in=order);
   by Customer_ID;
   if order=1 then output orderdata;
   else output noorders;
run;
 
proc print data=orderdata;
run;
 
proc print data=noorders;
run;

* Practice;

proc print data=custord;
run;

proc sort data =  orion.web_products;
by Product_ID;
run;

proc sort data = orion.web_orders;
by Product_ID;
run;

data revenue (keep = Product_ID Price Quantity Product_Name Customer Revenue)
     notsold (keep = Product_ID Price Product_Name)
     invalidcode (keep = Product_ID Quantity Customer);
merge orion.web_products (IN= inproduct) 
      orion.web_orders (IN= inorders);
by Product_ID;
if inorders = 1 and inproduct = 0 then output invalidcode;
else if inorders = 0 and inproduct = 1 then output notsold;
if inorders and inproduct then do; 
        Revenue = Quantity * Price; 
        output revenue;
     end;
run;

title 'Revenue from Orders';
proc print data=revenue;
run;

title 'Products Not Ordered';
proc print data=notsold;
run;

title 'Invalid Orders';
proc print data=invalidcode;
run;
title;




proc sort data=orion.order_fact
          out=work.orders_2007;
   by Customer_ID;
   where year(Order_Date)=2007;
run;

data orderdata(keep=Customer_Name Quantity 
                 Total_Retail_Price) 
     noorders(keep=Customer_Name Birth_Date)
     summary(keep=Customer_Name NumOrders NameNumber);
   merge orion.customer
         work.orders_2007(in=order);
   by Customer_ID;
   if order=1 then
      do;
         output orderdata;
         if first.Customer_ID then NumOrders=0;
         NumOrders+1; 
         NameNumber=catx('',Customer_LastName,NumOrders);
         if last.Customer_ID then output summary;
       end;
   else output noorders;
run;

title 'Summary';
proc print data=summary;
run;
title;



/*Merging Dataset*/

%let path=/folders/myfolders/ecprg293; 
libname orion "&path";

proc sort data=orion.order_fact
          out=work.orders_2007;
   by Customer_ID;
   where year(Order_Date)=2007;
run;
 
data custord;
   merge orion.customer(in=cust)
         work.orders_2007(in=order);
   by Customer_ID;
   if cust=1 and order=1;
   keep Customer_ID Customer_Name Quantity 
        Total_Retail_Price Product_ID;
run;

proc print data=custord;
run;

proc sort data=custord;
   by Product_ID;
run;
 
data custordprod;
   merge custord(in=ord) 
         orion.product_dim(in=prod);
   by Product_ID;
   if ord=1 and prod=1;
   Supplier=catx(' ',Supplier_Country,Supplier_ID);
   keep Customer_Name Quantity 
        Total_Retail_Price Product_ID Product_Name Supplier;
run;

proc print data=custordprod(obs=15) noobs;
run;

/*Merging with Excel File*/

proc sort data=custordprod;
   by Supplier;
run;

libname bonus pcfiles path="&path/BonusGift.xls";

data custordprodgift;
   merge custordprod(in=c)
         bonus.'Supplier$'n(in=s
               rename=(SuppID=Supplier
                       Quantity=Minimum));
   by Supplier;
   if c=1 and s=1 and Quantity>=Minimum;
run;

libname bonus clear;

proc sort data=custordprodGift;
   by Customer_Name;
run;

proc print data=custordprodGift;
   var Customer_Name Gift;
run;

/*Practice*/

proc contents data = orion.web_products2;
run;

proc contents data = orion.web_orders2;
run;

data web_converted;
set orion.web_products2 (rename=(Product_ID = PID));
Product_ID = put(PID, 12.);
run;

data revenue (keep= Product_ID Price Quantity Product_Name Customer Revenue)
     notsold (keep = Product_ID Price Product_Name)
     invalidcode (keep = Product_ID Quantity Customer);
merge web_converted (in = convert rename=(name = Product_name))
      orion.web_orders2 (in = order2 rename=(name = Customer));
by Product_ID;
if convert = 1 and order2 = 1 then do;
  Revenue = Price*Quantity;
  output revenue;
  end;
  else if convert = 1 and order2 = 0 then output notsold;
  else if convert = 0 and order2 = 1 then output invalidcode;
run; 
     
title 'Revenue from Orders';
proc print data=revenue;
run;

title 'Products Not Ordered';
proc print data=notsold;
VAR Product_ID Price Product_Name;
run;

title 'Invalid Orders';
proc print data=invalidcode;
run;
title;


/* Permanent Format */

%let path=/folders/myfolders/ecprg293; 
libname orion "&path";

data country_info;
   keep Start Label FmtName;
   retain FmtName '$country';
   *FmtName =  '$country';
   set orion.country(rename=(Country=Start
                             Country_Name=Label));
run;

proc print data=country_info noobs;
   title 'Country Information';
run;
title;

* Restructuring the dataset first;

data country_info;
keep Start Label FmtName;
set orion.country;
FmtName='$country';
Start = Country;
Label = Country_Name;
run;

*The above codes are inefficient. Use instead this;

data country_info;
   keep Start Label FmtName;
   retain FmtName '$country';
   set orion.country(rename=(Country=Start
                             Country_Name=Label));
run;

proc format cntlin= country_info;
run;
*in this PROC FORMAT, SAS stores the Country format in work.formats;

data country_info;
   keep Start Label FmtName;
   retain FmtName '$country';
   set orion.country(rename=(Country=Start
                             Country_Name=Label));
run;

proc print data=country_info noobs;
   title 'Country Information';
run;
title;

/*Notice that the country data set is now properly structured as a control data set, 
so you can use it to create a format using the CNTLIN= option.*/

*Anytime you use PROC FORMAT to create a format, SAS stores the format as a catalog entry. 
SAS catalogs are special SAS files within a SAS library that store many kinds of information 
in smaller units called entries. A single SAS catalog can contain many different catalog entries.

Entry types include FORMAT for numeric formats and FORMATC for character formats. By default, SAS 
stores formats in the work.formats catalog. These formats exist only for the duration of the SAS session.
For LIBRARY=, if you specify only a libref without a catalog name, SAS permanently stores the formats in the Formats catalog 
in that library. Otherwise, if you specify a libref and a catalog name, SAS permanently stores the format 
in that catalog. After you permanently store a format, you can use it in later SAS sessions or jobs.;

*proc format lib=sasuser cntlin=citycode;
*run;

*In the PROC FORMAT statement, you specify the sasuser library. You can optionally specify sasuser.formats, 
but SAS stores the format in the Formats catalog by default. Then you use the CNTLIN= option to specify the 
SAS data set citycode.

256 characters can be used in a label

Also notice that you can specify a catalog name in the LIBRARY= option, and you can
store formats in any catalog. The catalog name must conform to SAS naming
conventions. For instance: 

proc format lib=library.catalog;

proc format library=orion.myfmts cntlin = country_info;
run;

options nofmterr fmtsearch=(orion work library);
data supplier_country;
   set orion.shoe_vendors;
   Country_Name=put(Supplier_Country,$country.);
run;

proc print data = supplier_country;
run;

*SAS searches in the order specified in the FMTSEARCH= option. By default, SAS searches in the work and library libraries 
first unless you specify them in the option.

By default, the FTMTERR system option is in effect. If you want to avoid error messages and continue to process the step 
when SAS can not load a format, use the NOFMTERR system option.;
  
proc print data=supplier_country;
run;

proc format lib=orion cntlin= country_info;
run;

data supplier_country;
   set orion.shoe_vendors;
   Country_Name=put(Supplier_Country,$country.);
run;

*To access your custom formats it's helpful to understand how SAS looks for formats. 
By default, SAS searches in the work and library libraries for formats. Given that 
SAS automatically searches library.formats, one strategy for accessing your formats 
might be to assign the libref library to the SAS library that contains your format 
catalog, and to name the catalog formats.

But, what if you have more than one permanent format catalog, or if you named the 
format catalog something other than formats? How will SAS know which catalog stores 
your custom format?

A better strategy is to use the FMTSEARCH= system option to control the order in 
which SAS searches for format catalogs. ;

proc print data=orion.employee_addresses2;
run;
      
proc format library=orion fmtlib;
      value $extra
            ' '='Unknown'
            other=[$country30.];
run;
   
* Notice that a length of 30 is specified for the $country format. The default length would be 40.;

proc print data=orion.employee_addresses2;
     format Country $extra.;
run;

proc catalog catalog=orion.formats;
contents;
run;

proc catalog catalog=orion.formats;
delete extra.formatc;
run;

proc catalog catalog=orion.formats;
contents;
run;

proc format library= orion fmtlib;
*select $country;
run;

proc format library= orion fmtlib;
select $country;
run;

*You specify the EXCLUDE statement to exclude catalog entries from processing by the FMTLIB option. 
Catalog entry names are the same as the name of the format they store. You precede names of entries 
that contain character formats with a dollar sign.

FMTLIB document the format in a particular format catalog

You use the CNTLOUT= option to create a SAS data set from a format. You can follow a three-step process for using 
the CNTLOUT= option. First, you create a SAS data set from the values in the format using the CNTLOUT= 
option. Second, you edit the data set. And third, you re-create the format from the updated SAS data 
set using the CNTLIN= option. Let's look at each of these steps.;

/*Study this*/

%let path=/folders/myfolders/ecprg293; 
libname orion "&path";

data country_info;
   keep Start Label FmtName;
   retain FmtName '$country';
   set orion.country(rename=(Country=Start
                             Country_Name=Label));
run;

proc print data=country_info noobs;
   title 'Country Information';
run;
title;

proc format library = orion cntlin = country_info;
run;

proc format library = orion fmtlib;
   select $country;
run;

proc format library=orion cntlout=countryfmt;
   select $country;
run;
 
proc print data=countryfmt;
run;

proc print data=  orion.newcodes;
run;

data countryfmt;
   set countryfmt orion.newcodes;
run;

proc print data=countryfmt;
run;

* combine the two proc format into one;

proc format library=orion.formats cntlin=countryfmt fmtlib;
   select $country;
run;


/* Example Code for using value statment : 

proc format lib=library;

value jobfmt
103='manager'
105='text processor'
111='assoc. technical writer'
112='technical writer'
113='senior technical writer';

value $respnse
'Y'='Yes'
'N'='No'
'U'='Undecided'
'NOP'='No opinion';

run;

When the specified values are character values, they must be enclosed in quotation
marks and must match the case of the variable's values.

Multiple value statements can be added in a proc format step

***/

/*Practice*/

data work.continent_info;
   keep Start Label FmtName;
   retain FmtName 'continent';
   set orion.continent(rename=(Continent_ID=Start
                               Continent_Name=Label));
run;

proc print data=work.continent_info;
   title 'Continent Information';
run;

proc format library=orion.myfmts cntlin=work.continent_info;
   select continent;
   title 'Continent format';
run;

proc print data = orion.country (obs=10);
run;

options fmtsearch=(orion.MyFmts);
data countries;
   set orion.country;
   Continent_Name=put(Continent_ID, continent.);
run;

proc print data=countries(obs=10);
   title 'Continent Names';
run;
title;

proc format library=orion.MyFmts cntlout=continentfmt;
   select continent;
run;

proc sql;
   insert into continentfmt(FmtName, Start, End, Label)
   values('Continent', '90', '90', 'Antarctica')
   values('Continent', '92', '92', 'South America');
quit; 

proc format library=orion.myfmts cntlin=continentfmt;
   select continent;
run;


/*Level 2*/

%let path=/folders/myfolders/ecprg293; 
libname orion "&path";

data work.age_info;
set orion.ages (rename=(First_Age=Start Last_Age=End
             Description = Label));
  retain FmtName '$ages';
run;

proc print data = work.age_info;
run;

proc format library = orion.myfmts fmtlib cntlin=work.age_info;
select $ages;
run;

data sales1;
set orion.sales (keep = Employee_ID Birth_Date);
Ages = intck('year', Birth_Date, today());
Age_Cat = put(Ages, ages.);
run;

*The YRDIF function returns the calculated years between two SAS date values.
 The returned value will be a precise calculation, including multiple decimal
 places. Whereas with INTCK function will just give the rounded value like 10,
 11 and not like 10.2 and 11.5.

The calculation for the number of years from INTCK function is different from 
that generated by YRDIF. This is because the INTCK function bases the interval 
from the start of the respective intervals.

The INTCK function returns the integer count of the number of intervals in years,
months or days between two dates.; 

options fmtsearch=(orion.myfmts);
data sales;
   set orion.sales(keep=Employee_ID Birth_Date);
   Age=int(yrdif(Birth_Date, today(), 'AGE'));
   Age_Cat=put(Age, ages.);
run;

data sales2;
set orion.sales (keep = Employee_ID Birth_Date);
format Birth_Date anydtdte. ;
Ages = floor((today()-Birth_Date)/365);
*format Age_Cat $ages.;
Age_Cat = put(Ages, ages.);
run;

proc print data = sales (obs =5);
format Birth_Date date9.;
  title 'Sales Data Set';
  title2 " Age=int(yrdif(Birth_Date, today(), 'AGE'));";
run;


proc print data=sales1(obs=5);
   format Birth_Date date9.;
   title 'Sales Data Set1';
   title2'Ages = intck('year', Birth_Date, today());';
   title3 'Incorrect Age';
run;
title;

proc print data = sales2 (obs =5);
format Birth_Date date9.;
  title 'Sales Data Set2';
  title2 "Ages = floor((today()-Birth_Date)/365);";
run;
title;

/*Inputing raw data*/

*You can also use a FILENAME statement to associate a fileref with an aggregate storage
location, such as a directory that contains multiple external files.

fileref is a name that you associate with an external file. The name must be 1 to 8 characters
long, begin with a letter or underscore, and contain only letters, numerals, or underscores.

'filename' is the fully qualified name or location of the file.


A libref is used to access SAS data sets in a SAS data library. The INFILE statement references 
the raw data file, so you do not need to use a libref to point to it.

A delimited raw data file features:
It is external to SAS.
It is not software-specific.

Nonstandard numeric data includes
• values that contain special characters, such as percent signs (%), dollar signs ($), and
commas (,) 
• date and time values
• data in fraction, integer binary, real binary, and hexadecimal forms

You can assign date values to variables in assignment statements by using date
constants. To represent a constant in SAS date form, specify the date as 'ddmmmyy' or
'ddmmmyyyy', immediately followed by a D.

General form, date constant:
'ddmmmyy'd
or
“ddmmmyy”d
where
• dd is a one- or two-digit value for the day
• mmm is a three-letter abbreviation for the month (JAN, FEB, and so on)
• yy or yyyy is a two- or four-digit value for the year, respectively.
Be sure to enclose the date in quotation marks.

You can also use SAS time constants and SAS datetime constants in assignment
statements.;
/*
Time='9:25't;
DateTime='18jan2005:9:27:05'dt;
*/


filename abc '/folders/myshortcuts/_myfolders/ecprg193/custca.csv';

data custca;
infile abc dlm=',' obs = 15;
input firstname :$15. lastname :$15. empid gender :$1. date ddmmyy10. age :2. years :$15.;
putlog _all_;
run;

proc contents data = custca varnum;
run;

proc print data = custca;
sum age;
format date mmddyy10.;
run; 

/*Using proc import prdcedure*/

proc import datafile= '/folders/myshortcuts/_myfolders/ecprg193/custca.csv'
 out = custca replace;
 getnames= no;
run;

data custca;
set custca(rename=(
VAR1 = First
VAR2 = Last 
VAR3 = ID
VAR4 = Gender
VAR5 = BirthDate
VAR6 = Age
VAR7 = AgeGroup));
format ID 5.;
RUN;

proc contents data = custca varnum;
run;

proc print data = custca;
run; 

/*Character@*/
%let path=/folders/myfolders/ecprg193; 

data work.canada_customers;
   length First Last $ 20 Gender $ 1 
          AgeGroup $ 12;
   infile "&path/custca.csv" dlm=',';
   input First $ Last $ ID Gender $ 
         BirthDate :ddmmyy. Age AgeGroup $;
   format BirthDate monyy7.;
   AgeGroup = compress(AgeGroup); 
run;

proc contents data = canada_customers varnum;
run;

title 'Canadian Customers';
proc print data=work.canada_customers;
run;
title;

/*
 Creating a Raw Data File p176
  infile <-> file ; input <-> put ;
*/

filename qwe '/folders/myshortcuts/_myfolders/ecprg193/custca_new.csv';

/*colum output*/
data _null_;
 set canada_customers;
 file "/folders/myshortcuts/_myfolders/ecprg193/custca_new.csv";
 put First 1-19 Last 20-39 Gender 40-41 AgeGroup 42-53 ID 54-61
         BirthDate 62-69 Age 70-77;
run;  

/* Colum output playing with the character trailing@ */
data _null_;
 set canada_customers;
 file "/folders/myshortcuts/_myfolders/ecprg193/custca_newLSB.csv";
 put First 1-12 Last 13-22 @25 Gender @27 'Age Group: ' AgeGroup ID 50-56
         BirthDate 57-65 Age 66-70;
run;  

data _null_;
 set canada_customers;
 file "/folders/myshortcuts/_myfolders/ecprg193/custca_dlm.txt" dlm=',' ;
 put (First Last Gender AgeGroup ID) ($)
         BirthDate :Date9. Age 2.;
run; 

/*Now import back*/
data abccc;
 infile "/folders/myshortcuts/_myfolders/ecprg193/custca_newLSB.txt";
 input First $ 1-12 Last $ 13-22 @24 Gender $ @'Age Group: ' AgeGroup :$15.
        ID 51-57 BirthDate 58-66 Age;       /*Adding the $ back*/
run;  
 
proc print data = abccc;
run;

*If you do not execute a FILE statement before a PUT statement in the current
iteration of the DATA step, SAS writes the lines to the SAS log. If you specify the
PRINT fileref in the FILE statement, before the PUT statement, SAS writes the lines to
the procedure output file.;

/*Importing canada_customers back*/
data custca_new replace;
infile "/folders/myshortcuts/_myfolders/ecprg193/custca_new.txt";
Input First $ 1-19 Last $ 20-39 Gender $ 40-41 AgeGroup $ 42-53 ID 54-61
       @62 BirthDate  Age 70-77;
DDate= '01JUN2020'd;
format DDate monyy7.;
run;  

proc contents data = custca_new varnum;
run;

proc print data = custca_new;
format BirthDate ddmmyy10.;
run;

/*Expoting using proc export LSB P286*/
proc export data =  custca_new 
outfile = "/folders/myshortcuts/_myfolders/ecprg193/custca_export_pro.txt"
dbms = dlm replace;
*delimiter= '&';
/*Notice that DDate is outputed in  monyy7 format which was created in the data step. 
 To output as a SAS date (number) one has to make changes usnig the data step */
run;

data custca_new replace;
infile "/folders/myshortcuts/_myfolders/ecprg193/custca_new.txt";
Input First $ Last $ Gender $ AgeGroup $ ID 54-61
      BirthDate Age; /*column input*/
run;  
 
proc contents data = custca_new varnum;
run;

proc print data = custca_new;
format BirthDate ddmmyy10.;
run;


data work.managers;
   infile datalines dlm='/';
   input ID First :$6. Last :$10. Gender $ Salary :comma. 
            Title :$20. HireDate :date.;
   datalines;
120102/Tom/Zhou/M/108,255/Sales Manager/01Jun1993
120103/Wilson/Dawes/M/87,975/Sales Manager/01Jan1978
120261/Harry/Highpoint/M/243,190/Chief Sales Officer/01Aug1991
121143/Louis/Favaron/M/95,090/Senior Sales Manager/01Jul2001
121144/Renee/Capachietti/F/83,505/Sales Manager/01Nov1995
121145/Dennis/Lansberry/M/84,260/Sales Manager/01Apr1980
;

title 'Orion Star Management Team';
proc print data=work.managers noobs;
   format HireDate mmddyy10.;
run;
title;

data work.group1;
input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
RecHR 35-37 TimeMin 39-40 TimeSec 42-43
Tolerance $ 45;
TotalTime=(timemin*60)+timesec;
datalines;
2458  Murray, W           72  185 128 12 38 D
2462  Almers, C           68  171 133 10 5  I
2501  Bonaventure, T      78  177 139 11 13 I
2523  Johnson, R          69  162 114 9  42 S
2539  LaMance, K          75  168 141 11 46 D
2544  Jones, M            79  187 136 12 26 N
2595  Warren, C           77  170 136 12 10 S
;

proc print data=group1;
run;s

/*
DATALINES

•
You can use only one DATALINES statement in a DATA step. Use separate DATA
steps to enter multiple sets of data.
•
You can also use LINES; or CARDS; as the last statement in a DATA step and
immediately preceding the data lines. Both LINES and CARDS are aliases for the
DATALINES statement.
•
If your data contains semicolons, use the DATALINES4 statement plus a null statement
that consists of four semicolons (;;;;)

Notice that you do not need a RUN statement following the null statement (the
semicolon after the data lines). The DATALINES statement functions as a step
boundary, so the DATA step is executed as soon as SAS encounters it.



What does SAS do when it encounters a data error in a raw data record? Ans = A,C,D,E. 

	 a.  prints a ruler and the raw data record in the SAS log
	 c.  assigns a missing value to the variable that the invalid data affects
	 d.  prints a note about the error in the SAS log
	 e.  prints the variable values in the corresponding SAS observation in the SAS log

*/

data SalesStaff;
   infile "&path/sales1.dat";
   input @1 Employee_ID 6.
         @21 Last_Name $18.
         @43 Job_Title $20.
         @64 Salary Dollar8.
         @87 Hire_Date mmddyy10.;
run;

title 'Australian and US Sales Staff';
proc print data=salesstaff;
run;
title;

/*Mixed Input*/
%let path=/folders/myfolders/ecprg293; 
libname orion "&path";

data SalesStaff;
   infile "&path/sales1.dat";
   input Employee_ID 1-6
         @21 Last_Name 18.
         @43 Job_Title $20.
         @64 Salary Dollar8.
         @87 Hire_Date mmddyy10.;
run;


title 'Australian and US Sales Staff';
proc print data=salesstaff;
run;
title;

