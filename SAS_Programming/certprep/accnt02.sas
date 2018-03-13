data work.balnew2;
set sasuser.insure;
run;
proc print data=work.balnew2
var id name total balancedue;
run;
