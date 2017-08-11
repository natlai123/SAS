/*  Updated 9/12/2007  

The %JACK macro does jackknife analyses for simple random samples, computing approximate standard 
errors, bias-corrected estimates, and confidence intervals assuming a normal sampling distribution.

The %BOOT macro does elementary nonparametric bootstrap analyses for simple random samples, computing 
approximate standard errors, bias-corrected estimates, and confidence intervals assuming a normal 
sampling distribution. Also, for regression models, the %BOOT macro can resample either observations
or residuals.

The %BOOTCI macro computes several varieties of confidence intervals that are suitable for sampling 
distributions that are not normal.

SAS Institute Inc.

License Agreement for Corrective Code or Additional Functionality

*/


%macro jack(      /* Jackknife resampling analysis */
   data=,         /* Input data set. If the data set does not support
                     direct access via the POINT= option, do NOT use
                     the %BYSTMT macro in the %ANALYZE macro. */
   stat=_numeric_,/* Numeric variables in the OUT= data set created
                     by the %ANALYZE macro that contain the values
                     of statistics for which you want to compute
                     jackknife distributions. */
   id=,           /* One or more numeric or character variables that
                     uniquely identify the observations of the OUT=
                     data set within each BY group. No ID variables
                     are needed if the OUT= data set has only one
                     observation per BY group.
                     The ID variables may not be named _TYPE_, _NAME_,
                     or _STAT_. */
   biascorr=1,    /* 1 for bias correction; 0 otherwise. */
   alpha=.05,     /* significance (i.e., one minus confidence) level
                     for confidence intervals; blank to suppress
                     confidence intervals. */
   print=1,       /* 1 to print the jackknife estimates;
                     0 otherwise. */
   chart=1        /* 1 to chart the jackknife resampling distributions;
                     0 otherwise. */
   );

   %if %bquote(&data)= %then %do;
      %put ERROR in JACK: The DATA= argument must be specified.;
      %goto exit;
   %end;

   %global _jackdat; %let _jackdat=&data;

   %global vardef;
   %let vardef=DF;

   %local jack by useby;
   %let useby=0;

   *** compute the actual values of the statistics;
   %let by=;
   %analyze(data=&data,out=JACKACT);
   %if &syserr>4 %then %goto exit;

   *** find number of observations in the input data set;
   %local nobs;
   data _null_;
      call symput('nobs',trim(left(put(_nobs,12.))));
      if 0 then set &data nobs=_nobs;
      stop;
   run;
   %if &syserr>4 %then %goto exit;

   %if &useby %then %do;
      %jackby(data=&data,print=0);
      %if &syserr>4 %then %goto exit;

      %let by=_sample_;
      %analyze(data=JACKDATA,out=JACKDIST);
      %if &syserr>4 %then %goto exit;
   %end;

   %else %do;
      %jackslow(data=&data);
      %if &syserr>4 %then %goto exit;
   %end;

   %if &chart %then %do;
      %if %bquote(&id)^= %then %do;
         proc sort data=JACKDIST; by &id; run;
         proc chart data=JACKDIST(drop=_sample_);
            vbar &stat;
            by &id;
         run;
      %end;
      %else %do;
         proc chart data=JACKDIST(drop=_sample_);
            vbar &stat;
         run;
      %end;
   %end;

   %jackse(stat=&stat,id=&id,alpha=&alpha,biascorr=&biascorr,print=&print)

%exit:;

%mend jack;


%macro jackby( /* Jackknife resampling */
   data=&_jackdat,
   print=0
   );

   data JACKDATA/view=JACKDATA;
      do _sample_=1 to &nobs;
         do _i=1 to &nobs;
            if _i^=_sample_ then do;
               _obs_=_i;
               set &data point=_i;
               output;
            end;
         end;
      end;
      stop;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=JACKDATA; id _sample_ _obs_; run;
   %end;

%exit:;
%mend jackby;


%macro jackslow( /* Uniform jackknife sampling and analysis
                    without BY processing */
   data=&_jackdat
   );

   %put %cmpres(WARNING: Jackknife analysis will be slow because the
        ANALYZE macro did not use the BYSTMT macro.);

   data JACKDIST; set JACKACT; _sample_=0; delete; run;

   options nonotes;
   %local sample;
   %do sample=1 %to &nobs;
      %put Jackknife sample &sample;
      data _TMPD_;
         drop _i;
         do _i=1 to &nobs;
            set &data;
            if _i^=&sample then output;
         end;
         stop;
      run;
      %if &syserr>4 %then %goto exit;

      %analyze(data=_TMPD_,out=_TMPS_);
      %if &syserr>4 %then %goto exit;
      data _TMPS_; set _TMPS_; _sample_=&sample; run;
      %if &syserr>4 %then %goto exit;
      proc append data=_TMPS_ base=JACKDIST; run;
      %if &syserr>4 %then %goto exit;
   %end;

%exit:;
   options notes;
%mend jackslow;


%******************************* JACKSE *******************************;
%macro jackse( /* Jackknife estimates of standard error, bias, and
                  normal confidence intervals */
   stat=,
   id=,
   alpha=.05,
   biascorr=1,
   print=1
   );

   %global _jackdat;
   %if %bquote(&_jackdat)= %then %do;
      %put ERROR in JACKSE: You must run JACK before JACKSE;
      %goto exit;
   %end;

   %if %bquote(&alpha)^= %then %do;
      *** compute confidence level;
      %local conf;
      data _null_;
         conf=100*(1-&alpha);
         call symput('conf',trim(left(put(conf,best8.))));
      run;
   %end;

   %if %bquote(&id)^= %then %do;
      *** sort the actual statistics;
      proc sort data=JACKACT;
         by &id;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   *** transpose the actual statistics in each observation;
   proc transpose data=JACKACT out=JACKACT2 prefix=value;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;

   proc sort data=JACKACT2;
      by %if %bquote(&id)^= %then &id; _name_ ;
   run;
   %if &syserr>4 %then %goto exit;

   %if %bquote(&id)^= %then %do;
      proc sort data=JACKDIST;
         by &id;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   *** compute mean, std, min, max of resampling distribution;
   proc means data=JACKDIST(drop=_sample_) noprint vardef=n;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      output out=JACKTMP2(drop=_type_ _freq_);
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;

   *** transpose statistics for resampling distribution;
   proc transpose data=JACKTMP2 out=JACKTMP3;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      id _stat_;
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;

   proc sort data=JACKTMP3;
      by %if %bquote(&id)^= %then &id; _name_ ;
   run;
   %if &syserr>4 %then %goto exit;

   data JACKSTAT;
      retain &id name value jackmean
             %if &biascorr %then bias;
             stderr
             %if %bquote(&alpha)^= %then alcl;
             %if &biascorr %then biasco;
             %if %bquote(&alpha)^= %then aucl confid method;
             min max n;
      merge JACKACT2(rename=(_name_=name value1=value))
            JACKTMP3(rename=(_name_=name mean=jackmean std=stderr));
      by %if %bquote(&id)^= %then &id; name;
      %if %bquote(&alpha)^= %then %do;
         length method $20;
         retain z; drop z;
         if _n_=1 then do;
            z=probit(1-&alpha/2); put z=;
            confid=&conf;
            method='Jackknife';
         end;
      %end;
      stderr=stderr*sqrt(&nobs-1);
      %if &biascorr %then %do;
         bias=(jackmean-value)*(&nobs-1);
         biasco=value-bias;
         %if %bquote(&alpha)^= %then %do;
            alcl=biasco-z*stderr;
            aucl=biasco+z*stderr;
         %end;
      %end;
      %else %if %bquote(&alpha)^= %then %do;
         alcl=value-z*stderr;
         aucl=value+z*stderr;
      %end;
      label name  ='Name'
            value ='Observed Statistic'
            jackmean='Jackknife Mean'
            %if &biascorr %then %do;
               bias  ='Estimated Bias'
               biasco='Bias-Corrected Statistic'
            %end;
            stderr='Estimated Standard Error'
            %if %bquote(&alpha)^= %then %do;
               alcl  ='Estimated Lower Confidence Limit'
               aucl  ='Estimated Upper Confidence Limit'
               method='Method for Confidence Interval'
               confid='Confidence Level (%)'
            %end;
            min   ='Minimum Resampled Estimate'
            max   ='Maximum Resampled Estimate'
            n     ='Number of Resamples'
            ;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=JACKSTAT label;
         id %if %bquote(&id)^= %then &id; name;
      run;
   %end;

%exit:;

%mend jackse;





%******************************* BOOT *******************************;
%macro boot(      /* Bootstrap resampling analysis */
   data=,         /* Input data set, not a view or a tape file. */
   samples=200,   /* Number of resamples to generate. */
   residual=,     /* Name of variable in the input data set that
                     contains residuals; may not be used with SIZE= */
   equation=,     /* Equation (in the form of an assignment statement)
                     for computing the response variable */
   size=,         /* Size of each resample; default is size of the
                     input data set. The SIZE= argument may not be
                     used with BALANCED=1 or with a nonblank value
                     for RESIDUAL= */
   balanced=,     /* 1 for balanced resampling; 0 for uniform
                     resampling. By default, balanced resampling
                     is used unless the SIZE= argument is specified,
                     in which case uniform resampling is used. */
   random=0,      /* Seed for pseudorandom numbers. */
   stat=_numeric_,/* Numeric variables in the OUT= data set created
                     by the %ANALYZE macro that contain the values
                     of statistics for which you want to compute
                     bootstrap distributions. */
   id=,           /* One or more numeric or character variables that
                     uniquely identify the observations of the OUT=
                     data set within each BY group. No ID variables
                     are needed if the OUT= data set has only one
                     observation per BY group.
                     The ID variables may not be named _TYPE_, _NAME_,
                     or _STAT_ */
   biascorr=1,    /* 1 for bias correction; 0 otherwise */
   alpha=.05,     /* significance (i.e., one minus confidence) level
                     for confidence intervals; blank to suppress normal
                     confidence intervals */
   print=1,       /* 1 to print the bootstrap estimates;
                     0 otherwise. */
   chart=1        /* 1 to chart the bootstrap resampling distributions;
                     0 otherwise. */
   );

   %if %bquote(&data)= %then %do;
      %put ERROR in BOOT: The DATA= argument must be specified.;
      %goto exit;
   %end;

   %global _bootdat; %let _bootdat=&data;

   %local by useby;
   %let useby=0;

   %global usevardf vardef;
   %let usevardf=0;

   *** compute the actual values of the statistics;
   %let vardef=DF;
   %let by=;
   %analyze(data=&data,out=_ACTUAL_);
   %if &syserr>4 %then %goto exit;

   *** compute plug-in estimates;
   %if &usevardf %then %do;
      %let vardef=N;
      %analyze(data=&data,out=_PLUGIN_);
      %let vardef=DF;
      %if &syserr>4 %then %goto exit;
   %end;

   %if &useby=0 %then %let balanced=0;

   %if %bquote(&size)^= %then %do;
      %if %bquote(&balanced)= %then %let balanced=0;
      %else %if &balanced %then %do;
         %put %cmpres(ERROR in BOOT: The SIZE= argument may not be used
              with BALANCED=1.);
         %goto exit;
      %end;
      %if %bquote(&residual)^= %then %do;
         %put %cmpres(ERROR in BOOT: The SIZE= argument may not be used
              with RESIDUAL=.);
         %goto exit;
      %end;
   %end;
   %else %if %bquote(&balanced)= %then %let balanced=1;

   *** find number of observations in the input data set;
   %global _nobs;
   data _null_;
      call symput('_nobs',trim(left(put(_nobs,12.))));
      if 0 then set &data nobs=_nobs;
      stop;
   run;
   %if &syserr>4 %then %goto exit;

   %if &balanced %then
      %bootbal(data=&data,samples=&samples,
               random=&random,print=0);

   %else %if &useby %then
      %bootby(data=&data,samples=&samples,
              random=&random,size=&size,print=0);

   %if &syserr>4 %then %goto exit;

   %if &balanced | &useby %then %do;
      %let by=_sample_;
      %analyze(data=BOOTDATA,out=BOOTDIST);
   %end;

   %else
      %bootslow(data=&data,samples=&samples,
                random=&random,size=&size);

   %if &syserr>4 %then %goto exit;

   %if &chart %then %do;
      %if %bquote(&id)^= %then %do;
         proc sort data=BOOTDIST; by &id; run;
         proc chart data=BOOTDIST(drop=_sample_);
            vbar &stat;
            by &id;
         run;
      %end;
      %else %do;
         proc chart data=BOOTDIST(drop=_sample_);
            vbar &stat;
         run;
      %end;
   %end;

   %bootse(stat=&stat,id=&id,alpha=&alpha,biascorr=&biascorr,print=&print)

%exit:;

%mend boot;


%macro bootbal( /* Balanced bootstrap resampling */
   data=&_bootdat,
   samples=200,
   random=0,
   print=0,
   );

   * Gleason, J.R. (1988) "Algorithms for balanced bootstrap
     simulations," American Statistician, 42, 263-266;
   data BOOTDATA/view=BOOTDATA;
      %bootin;
      drop _a _cbig _ii _j _jbig _k _s;
      array _c(&_nobs) _temporary_;  /* cell counts */
      array _p(&_nobs) _temporary_;  /* pointers */
      do _j=1 to &_nobs;
         _c(_j)=&samples;
      end;
      do _j=1 to &_nobs;
         _p(_j)=_j;
      end;
      _k=&_nobs;                  /* number of nonempty cells left */
      _jbig=_k;                   /* index of largest cell */
      _cbig=&samples;             /* _cbig >= _c(_j) */
      do _sample_=1 to &samples;
         do _i=1 to &_nobs;
            do until(_s<=_c(_j));
               _j=ceil(ranuni(&random)*_k);    /* choose a cell */
               _s=ceil(ranuni(&random)*_cbig); /* accept cell? */
            end;
            _l=_p(_j);
            _obs_=_l;
            _c(_j)+-1;
* put _sample_= _i= _k= _l= @30 %do i=1 %to &_nobs; _c(&i) %end;;
            if _j=_jbig then do;
               _a=floor((&samples-_sample_+_k)/_k);
               if _cbig-_c(_j)>_a then do;
                  do _ii=1 to _k;
                     if _c(_ii)>_c(_jbig) then _jbig=_ii;
                  end;
                  _cbig=_c(_jbig);
               end;
            end;
            if _c(_j)=0 then do;
               if _jbig=_k then _jbig=_j;
               _p(_j)=_p(_k);
               _c(_j)=_c(_k);
               _k+-1;
            end;
            %bootout(_l);
         end;
      end;
      stop;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=BOOTDATA; id _sample_ _obs_; run;
   %end;

%exit:;

%mend bootbal;


%macro bootby( /* Uniform bootstrap resampling */
   data=&_bootdat,
   samples=200,
   random=0,
   size=,
   print=0
   );

   %if %bquote(&size)= %then %let size=&_nobs;

   data BOOTDATA/view=BOOTDATA;
      %bootin;
      do _sample_=1 to &samples;
         do _i=1 to &size;
            _p=ceil(ranuni(&random)*&_nobs);
            _obs_=_p;
            %bootout(_p);
         end;
      end;
      stop;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=BOOTDATA; id _sample_ _obs_; run;
   %end;

%exit:;
%mend bootby;


%macro bootslow( /* Uniform bootstrap resampling and analysis
                    without BY processing */
   data=&_bootdat,
   samples=20,
   random=0,
   size=
   );

   %put %cmpres(WARNING: Bootstrap analysis will be slow because the
        ANALYZE macro did not use the BYSTMT macro.);

   %if %bquote(&size)= %then %let size=&_nobs;

   data BOOTDIST; set _ACTUAL_; _sample_=0; delete; run;

   options nonotes;
   %local sample;
   %do sample=1 %to &samples;
      %put Bootstrap sample &sample;
      data _TMPD_;
         %bootin;
         do _i=1 to &size;
            _p=ceil(ranuni(%eval(&random+&sample))*&_nobs);
            %bootout(_p);
         end;
         stop;
      run;
      %if &syserr>4 %then %goto exit;

      %analyze(data=_TMPD_,out=_TMPS_);
      %if &syserr>4 %then %goto exit;
      data _TMPS_; set _TMPS_; _sample_=&sample; run;
      %if &syserr>4 %then %goto exit;
      proc append data=_TMPS_ base=BOOTDIST; run;
      %if &syserr>4 %then %goto exit;
   %end;

%exit:;
   options notes;
%mend bootslow;



%******************************* BOOTSE *******************************;
%macro bootse( /* Bootstrap estimates of standard error, bias, and
                  normal confidence intervals */
   stat=,
   id=,
   alpha=.05,
   biascorr=1,
   print=1
   );

   %global _bootdat;
   %if %bquote(&_bootdat)= %then %do;
      %put ERROR in BOOTSE: You must run BOOT before BOOTSE;
      %goto exit;
   %end;

   %if %bquote(&alpha)^= %then %do;
      *** compute confidence level;
      %local conf;
      data _null_;
         conf=100*(1-&alpha);
         call symput('conf',trim(left(put(conf,best8.))));
      run;
   %end;

   %if %bquote(&id)^= %then %do;
      *** sort the actual statistics;
      proc sort data=_ACTUAL_;
         by &id;
      run;
      %if &syserr>4 %then %goto exit;
      %if &usevardf %then %do;
         *** sort the plug-in estimates;
         proc sort data=_PLUGIN_;
            by &id;
         run;
         %if &syserr>4 %then %goto exit;
      %end;
   %end;

   *** transpose the actual statistics in each observation;
   proc transpose data=_ACTUAL_ out=_ACTTR_ prefix=value;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;
   proc sort data=_ACTTR_;
      by %if %bquote(&id)^= %then &id; _name_ ;
   run;
   %if &syserr>4 %then %goto exit;

   %if &usevardf %then %do;
      *** transpose the plug-in estimates in each observation;
      proc transpose data=_PLUGIN_ out=_PLUGTR_ prefix=value;
         %if %bquote(&stat)^= %then %do;
            var &stat;
         %end;
         %if %bquote(&id)^= %then %do;
            by &id;
         %end;
      run;
      %if &syserr>4 %then %goto exit;
      proc sort data=_PLUGTR_;
         by %if %bquote(&id)^= %then &id; _name_ ;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   %if %bquote(&id)^= %then %do;
      proc sort data=BOOTDIST;
         by &id;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   *** compute mean, std, min, max of resampling distribution;
   proc means data=BOOTDIST(drop=_sample_) noprint;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      output out=_TMP2_(drop=_type_ _freq_);
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;

   *** transpose statistics for resampling distribution;
   proc transpose data=_TMP2_ out=_TMP3_;
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      id _stat_;
      %if %bquote(&id)^= %then %do;
         by &id;
      %end;
   run;
   %if &syserr>4 %then %goto exit;

   proc sort data=_TMP3_;
      by %if %bquote(&id)^= %then &id; _name_ ;
   run;
   %if &syserr>4 %then %goto exit;

   data BOOTSTAT;
      retain &id name value bootmean
             %if &biascorr %then bias;
             stderr
             %if %bquote(&alpha)^= %then alcl;
             %if &biascorr %then biasco;
             %if %bquote(&alpha)^= %then aucl confid method;
             min max n;
      merge _ACTTR_(rename=(_name_=name value1=value))
            %if &usevardf %then
               _PLUGTR_(rename=(_name_=name value1=plugin));
            _TMP3_(rename=(_name_=name mean=bootmean std=stderr));
      by %if %bquote(&id)^= %then &id; name;
      %if %bquote(&alpha)^= %then %do;
         length method $20;
         retain z; drop z;
         if _n_=1 then do;
            z=probit(1-&alpha/2); put z=;
            confid=&conf;
            method='Bootstrap Normal';
         end;
      %end;
      %if &biascorr %then %do;
         bias=bootmean-%if &usevardf %then plugin; %else value;;
         biasco=value-bias;
         %if %bquote(&alpha)^= %then %do;
            alcl=biasco-z*stderr;
            aucl=biasco+z*stderr;
         %end;
      %end;
      %else %if %bquote(&alpha)^= %then %do;
         alcl=value-z*stderr;
         aucl=value+z*stderr;
      %end;
      label name  ='Name'
            value ='Observed Statistic'
            bootmean='Bootstrap Mean'
            %if &usevardf %then %do;
               plugin='Plug-In Estimate'
            %end;
            %if &biascorr %then %do;
               bias  ='Approximate Bias'
               biasco='Bias-Corrected Statistic'
            %end;
            stderr='Approximate Standard Error'
            %if %bquote(&alpha)^= %then %do;
               alcl  ='Approximate Lower Confidence Limit'
               aucl  ='Approximate Upper Confidence Limit'
               confid='Confidence Level (%)'
               method='Method for Confidence Interval'
            %end;
            min   ='Minimum Resampled Estimate'
            max   ='Maximum Resampled Estimate'
            n     ='Number of Resamples'
            ;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=BOOTSTAT label;
         id %if %bquote(&id)^= %then &id; name;
      run;
   %end;

%exit:;

%mend bootse;


%******************************* BOOTCI *******************************;
%macro bootci(    /* Bootstrap percentile-based confidence intervals.
                     Creates output data set BOOTCI. */
   method,        /* One of the following methods must be specified:
                        PERCENTILE or PCTL
                        HYBRID
                        T
                        BC
                        BCA     Requires the %JACK macro
                     */
   stat=,         /* Numeric variables in the OUT= data set created
                     by the %ANALYZE macro that contain the values
                     of statistics for which you want to compute
                     bootstrap distributions. */
   student=,      /* For the T method only, numeric variables in the
                     OUT= data set created by the %ANALYZE macro that
                     contain the standard errors of the statistics for which
                     you want to compute bootstrap distributions.
                     There must be a one-to-one between the VAR=
                     variables and the STUDENT= variables */
   id=,           /* One or more numeric or character variables that
                     uniquely identify the observations of the OUT=
                     data set within each BY group. No ID variables
                     are needed if the OUT= data set has only one
                     observation per BY group.
                     The ID variables may not be named _TYPE_, _NAME_,
                     or _STAT_ */
   alpha=.05,     /* significance (i.e., one minus confidence) level
                     for confidence intervals */
   print=1);      /* 1 to print the bootstrap confidence intervals;
                     0 otherwise. */

   %global _bootdat;
   %if %bquote(&_bootdat)= %then %do;
      %put ERROR in BOOTCI: You must run BOOT before BOOTCI;
      %goto exit;
   %end;

   *** check method;
   data _null_;
      length method $10;
      method=upcase(symget('method'));
      if method=' ' then do;
         put 'ERROR in BOOTCI: You must specify one of the methods '
             'PCTL, HYBRID, T, BC or BCa';
         abort;
      end;
      else if method='PERCENTILE' then method='PCTL';
      else if method not in ('PCTL' 'HYBRID' 'BC' 'BCA' 'T')
         then do;
         put "ERROR in BOOTCI: Unrecognized method '" method "'";
         abort;
      end;
      call symput('qmethod',method);
   run;
   %if &syserr>4 %then %goto exit;

   %if &qmethod=T %then %do;
      %if %bquote(&stat)= | %bquote(&student)= %then %do;
         data _null_;
   put 'ERROR: VAR= and STUDENT= must be specified with the T method';
         run;
         %goto exit;
      %end;
   %end;

   *** sort resampling distributions;
   %if %bquote(&id)^= %then %do;
      proc sort data=BOOTDIST;
         by &id _sample_;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   *** transpose resampling distributions;
   proc transpose data=BOOTDIST prefix=col
      out=BOOTTRAN(rename=(col1=value _name_=name));
      %if %bquote(&stat)^= %then %do;
         var &stat;
      %end;
      by %if %bquote(&id)^= %then &id; _sample_;
   run;
   %if &syserr>4 %then %goto exit;

   %if &qmethod=T %then %do;
      *** transpose studentizing statistics;
      proc transpose data=BOOTDIST prefix=col
         out=BOOTSTUD(rename=(col1=student _name_=studname));
            var &student;
         by %if %bquote(&id)^= %then &id; _sample_;
      run;
      %if &syserr>4 %then %goto exit;

      data BOOTTRAN;
         merge BOOTTRAN BOOTSTUD;
         label student='Value of Studentizing Statistic'
               studname='Name of Studentizing Statistic';
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   proc sort data=BOOTTRAN;
      by
         %if %bquote(&id)^= %then &id;
         name
         %if &qmethod=BC | &qmethod=BCA %then value;
         %else %if &qmethod=T %then _sample_;
      ;
   run;
   %if &syserr>4 %then %goto exit;

   %if &qmethod=T %then %do;
      *** transpose the actual statistics in each observation
          must get data set in unsorted order for merge;
      proc transpose data=_ACTUAL_ out=_ACTTR_ prefix=value;
         %if %bquote(&stat)^= %then %do;
            var &stat;
         %end;
         %if %bquote(&id)^= %then %do;
            by &id;
         %end;
      run;
      %if &syserr>4 %then %goto exit;

      *** transpose the actual studentizing statistics;
      proc transpose data=_ACTUAL_ prefix=col
            out=_ACTSTUD(rename=(_name_=studname col1=student));
            var &student;
         %if %bquote(&id)^= %then %do;
            by &id;
         %end;
      run;
      %if &syserr>4 %then %goto exit;

      *** merge statistics with studentizing statistics;
      data _ACT_T_;
         merge _ACTTR_ _ACTSTUD;
         label student='Value of Studentizing Statistic'
               studname='Name of Studentizing Statistic';
      run;
      %if &syserr>4 %then %goto exit;

      proc sort data=_ACT_T_;
         by %if %bquote(&id)^= %then &id; _name_ ;
      run;
      %if &syserr>4 %then %goto exit;

      data BOOTTRAN;
         merge BOOTTRAN _ACT_T_(rename=(_name_=name));
         by
            %if %bquote(&id)^= %then &id;
            name
         ;
         value=(value-value1)/student;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   %if &qmethod=BC | &qmethod=BCA %then %do;

      %if &qmethod=BCA %then %do;

         %jack(data=&_bootdat,stat=&stat,id=&id,alpha=&alpha,
               chart=0,print=&print);
         %if &syserr>4 %then %goto exit;

         *** estimate acceleration for BCa;
         proc means data=JACKDIST noprint vardef=df;
            %if %bquote(&stat)^= %then %do;
               var &stat;
            %end;
            output out=JACKSKEW(drop=_type_ _freq_ _sample_) skewness=;
            %if %bquote(&id)^= %then %do;
               by &id;
            %end;
         run;
         %if &syserr>4 %then %goto exit;

         *** transpose skewness;
         proc transpose data=JACKSKEW prefix=col
            out=_ACCEL_(rename=(col1=skewness _name_=name));
            %if %bquote(&stat)^= %then %do;
               var &stat;
            %end;
            %if %bquote(&id)^= %then %do;
               by &id;
            %end;
         run;
         %if &syserr>4 %then %goto exit;

         proc sort data=_ACCEL_;
            by %if %bquote(&id)^= %then &id; name ;
         run;
         %if &syserr>4 %then %goto exit;
      %end;

      *** estimate median bias for BC;
      data _BC_;
         retain _alpha _conf;
         drop value value1;
         if _n_=1 then do;
            _alpha=&alpha;
            _conf=100*(1-_alpha);
            call symput('conf',trim(left(put(_conf,best8.))));
         end;
         merge _ACTTR_(rename=(_name_=name))
               BOOTTRAN;
         by %if %bquote(&id)^= %then &id; name;
         if first.name then do; n=0; _z0=0; end;
         n+1;
         _z0+(value<value1)+.5*(value=value1);
         if last.name then do;
            _z0=probit(_z0/n);
            output;
         end;
      run;
      %if &syserr>4 %then %goto exit;

      *** compute percentiles;
      data BOOTPCTL;
         retain _i _lo _up _nplo _jlo _glo _npup _jup _gup
                alcl aucl;
         drop _alpha _sample_ _conf _i _nplo _jlo _glo _npup _jup _gup
              value;
         merge BOOTTRAN _BC_ %if &qmethod=BCA %then _ACCEL_;;
         by %if %bquote(&id)^= %then &id; name;
         label _lo='Lower Percentile Point'
               _up='Upper Percentile Point'
               _z0='Bias Correction (Z0)';
         if first.name then do;
            %if &qmethod=BC %then %do;
               _lo=probnorm(_z0+(_z0+probit(_alpha/2)));
               _up=probnorm(_z0+(_z0+probit(1-_alpha/2)));
            %end;
            %else %if &qmethod=BCA %then %do;
               drop skewness;
               retain _accel;
               label _accel='Acceleration';
               _accel=skewness/(-6*sqrt(&_nobs))*
                      (&_nobs-2)/&_nobs/sqrt((&_nobs-1)/&_nobs);
               _i=_z0+probit(_alpha/2);
               _lo=probnorm(_z0+_i/(1-_i*_accel));
               _i=_z0+probit(1-_alpha/2);
               _up=probnorm(_z0+_i/(1-_i*_accel));
            %end;
            _nplo=min(n-.5,max(.5,fuzz(n*_lo)));
            _jlo=floor(_nplo); _glo=_nplo-_jlo;
            _npup=min(n-.5,max(.5,fuzz(n*_up)));
            _jup=floor(_npup); _gup=_npup-_jup;
            _i=0;
         end;
         _i+1;
         if _glo then do;
            if _i=_jlo+1 then alcl=value;
         end;
         else do;
            if _i=_jlo then alcl=value;
            else if _i=_jlo+1 then alcl=(alcl+value)/2;
         end;
         if _gup then do;
            if _i=_jup+1 then aucl=value;
         end;
         else do;
            if _i=_jup then aucl=value;
            else if _i=_jup+1 then aucl=(aucl+value)/2;
         end;
         if last.name then do;
            output;
         end;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   %else %do;
      %local conf pctlpts pctlpre pctlname;
      %let pctlpre=a;
      %let pctlname=lcl ucl;
      data _null_;
         _alpha=&alpha;
         _conf=100*(1-_alpha);
         call symput('conf',trim(left(put(_conf,best8.))));
         %if &qmethod=PCTL %then %do;
            _lo=_alpha/2;
            _up=1-_lo;
         %end;
         %else %if &qmethod=HYBRID | &qmethod=T %then %do;
            _up=_alpha/2;
            _lo=1-_up;
         %end;
         _lo=100*_lo;
         _up=100*_up;
         call symput('pctlpts',trim(left(put(_lo,best8.)))||' '||
                               trim(left(put(_up,best8.))));
      run;
      %if &syserr>4 %then %goto exit;

      proc univariate data=BOOTTRAN noprint pctldef=5;
         var value;
         output out=BOOTPCTL n=n
            pctlpts=&pctlpts pctlpre=&pctlpre pctlname=&pctlname;
         by %if %bquote(&id)^= %then &id; name;
      run;
      %if &syserr>4 %then %goto exit;
   %end;

   data BOOTCI;
      retain &id name value alcl aucl confid method n;
      merge
         %if &qmethod=T
            %then _ACT_T_(rename=(_name_=name value1=value));
            %else _ACTTR_(rename=(_name_=name value1=value));
         BOOTPCTL;
      by %if %bquote(&id)^= %then &id; name;
      %if &qmethod=HYBRID %then %do;
         aucl=2*value-aucl;
         alcl=2*value-alcl;
      %end;
      %else %if &qmethod=T %then %do;
         aucl=value-aucl*student;
         alcl=value-alcl*student;
      %end;
      confid=&conf;
      length method $20;
      method='Bootstrap '||symget('method');
      label name  ='Name'
            value ='Observed Statistic'
            alcl  ='Approximate Lower Confidence Limit'
            aucl  ='Approximate Upper Confidence Limit'
            confid='Confidence Level (%)'
            method='Method for Confidence Interval'
            n     ='Number of Resamples'
            ;
   run;
   %if &syserr>4 %then %goto exit;

   %if &print %then %do;
      proc print data=BOOTCI label;
         id %if %bquote(&id)^= %then &id; name;
      run;
   %end;

%exit:
%mend bootci;


%******************************* ALLCI *******************************;
%macro allci(     /* Computes all types of confidence intervals
                     available in BOOTCI. Creates output data set
                     ALLCI. */
   stat=,         /* Numeric variables in the OUT= data set created
                     by the %ANALYZE macro that contain the values
                     of statistics for which you want to compute
                     bootstrap distributions. */
   student=,      /* For the T method only, numeric variables in the
                     OUT= data set created by the %ANALYZE macro that
                     contain the standard errors of the statistics for which
                     you want to compute bootstrap distributions.
                     There must be a one-to-one between the VAR=
                     variables and the STUDENT= variables */
   id=,           /* One or more numeric or character variables that
                     uniquely identify the observations of the OUT=
                     data set within each BY group. No ID variables
                     are needed if the OUT= data set has only one
                     observation per BY group.
                     The ID variables may not be named _TYPE_, _NAME_,
                     or _STAT_ */
   alpha=.05,     /* significance (i.e., one minus confidence) level
                     for confidence intervals */
   keep=,         /* Variables to keep in the output data set
                     containing the confidence intervals; can be used
                     to avoid warnings from PROC TRANSPOSE */
   print=1);      /* 1 to print the bootstrap confidence intervals;
                     0 otherwise. */

   %if %bquote(&keep)^= %then %let keep=(keep=&keep);

   %bootci(bca,stat=&stat,id=&id,alpha=&alpha,print=0)
   data ALLCI; set bootci&keep; run;

   %bootci(bc,stat=&stat,id=&id,alpha=&alpha,print=0)
   proc append data=bootci&keep base=ALLCI force; run;

   %bootci(pctl,stat=&stat,id=&id,alpha=&alpha,print=0)
   proc append data=bootci&keep base=ALLCI force; run;

   %bootci(hybrid,stat=&stat,id=&id,alpha=&alpha,print=0)
   proc append data=bootci&keep base=ALLCI force; run;

   %if %bquote(&student)^= %then %do;
      %bootci(t,stat=&stat,id=&id,student=&student,alpha=&alpha,print=0)
      proc append data=bootci&keep base=ALLCI force; run;
   %end;

   proc append data=bootstat&keep base=ALLCI force; run;
   proc append data=jackstat&keep base=ALLCI force; run;

   %if &print %then %do;
      proc print data=ALLCI label;
         id %if %bquote(&id)^= %then &id; name;
      run;
   %end;

%mend allci;



%macro bystmt;
   %let useby=1;
   by &by;
%mend bystmt;


%macro vardef;
   %let usevardf=1;
   vardef=&vardef
%mend vardef;




%macro bootin; /* INTERNAL USE ONLY
       input an observation from the original data set */
   %if %bquote(&residual)^= %then %do;
      array _r(&_nobs) _temporary_; /* residuals */
      do _i=1 to &_nobs;
         set &data point=_i;
         _r(_i)=&residual;
      end;
   %end;
   %else %do;
      drop _i;
   %end;
%mend bootin;


%macro bootout(obs); /* INTERNAL USE ONLY
       output an observation to the resampled data set */
   %if %bquote(&residual)^= %then %do;
      set &data point=_i;
      &residual=_r(&obs);
      &equation;
   %end;
   %else %do;
      set &data point=&obs;
   %end;
   output;
%mend bootout;

