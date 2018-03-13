data work.billing;
set sasuser.insure;
run;
proc print data=work.billing keylabel;
label total='Total Balance' balancedue='Balance Due';
run;
