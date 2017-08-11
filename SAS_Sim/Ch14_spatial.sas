/***********************************************************************
 Programs for 
 Wicklin, Rick, 2013, Simulating Data with SAS, SAS Institute Inc., Cary NC.

 Chapter 14: Simulating Data from Spatial Models
 ***********************************************************************/

ods graphics on;

/********************************************************************
 Simulating Data from a Gaussian Random Field
 *******************************************************************/

proc iml;
/* Define Gaussian covariance function V(s). Each row of s is a point. 
   c0 = scale;  a0=range;  s is n x p matrix */
start GaussianCov(c0, a0, s);
   h = distance(s);          /* n x n matrix of pairwise distances  */
   return ( c0#exp( -(h##2/a0##2) ) );
finish;

/* 1D example of unconditional distrib of spatially correlated data */
scale = 8;   range = 30;    /* params for the Gaussian random field */
s = do(0, 100, 25);         /* evaluate field at these locations    */
Cov = GaussianCov(scale, range, s`);
print Cov[format=6.4 r=("s1":"s5") c=("s1":"s5")];

/* Simulate GRF unconditionally. Each row of s is a point.
   s is n x p matrix of n points in p-dimensional space */
start UncondSimGRF(NumSamples, s, param);
   mean = param[1]; scale=param[2]; range=param[3];
   C = GaussianCov(scale, range, s);
   return( RandNormal( NumSamples, j(1,nrow(s),mean), C) );
finish;

call randseed(12345);
mean = 40;
Z = UncondSimGRF(4, s`, mean||scale||range);      /* 4 realizations */
print (s`)[label="Location"] (Z`)[format=5.3 c=("z1":"z4")];

/*******************************/
/* Create 1000 realizations */
s = {20 50 80};            /* evaluate the field at these locations */
NumSamples = 1000;
Z = UncondSimGRF(NumSamples, s`, mean||scale||range);
N = ncol(s);
SampleID = repeat( T(1:NumSamples), 1, N );
x = repeat( s, 1, NumSamples );
create GRF var {"SampleID" "x" "z"};  append;  close GRF;

/*******************************/
store module=(GaussianCov);
s = do(0, 100, 5);         /* evaluate the field at these locations */
NumSamples = 6;
Z = UncondSimGRF(NumSamples, s`, mean||scale||range);

/* Each row of z is a realization. Construct X and SampleID vars    */
N = ncol(s);
SampleID = repeat( T(1:NumSamples), 1, N );
x = repeat( s, 1, NumSamples );
create GRF1D var {"SampleID" "x" "z"};  append;  close GRF1D;
quit;

proc sgplot data=GRF1D;
   title "Realizations of a Gaussian Random Field";
   series x=x y=z / group=SampleID;
   refline 40 / axis=y;
run;

proc means data=GRF Mean Var Min Max;
   where x in (20 50 80);
   class x;
   var z;
run;

/***********************************************************************/

/* unconditional simulation by using PROC SIM2D */
proc sim2d outsim=GRF narrow; 
   grid x = 0 to 100 by 10 
        y = 0 to 100 by 10;
   simulate form=Gauss scale=8 range=30 numreal=4 seed=12345; 
   mean 40;
run; 

/* define a contour plot template */
proc template;
define statgraph ContourPlotParm;
dynamic _X _Y _Z _TITLE;
begingraph;
   entrytitle _TITLE;
   layout overlay;
      contourplotparm x=_X y=_Y z=_Z / nhint=12 
        contourtype=fill colormodel=twocolorramp name="Contour";
      continuouslegend "Contour" / title=_Z;
   endlayout;
endgraph;
end;
run;

proc sgrender data=GRF template=ContourPlotParm;
where _ITER_ = 1;                               /* or use BY _ITER_ */
dynamic _TITLE="Realization of a 2D Gaussian Random Field"
        _X="gxc" _Y="gyc" _Z="SValue";
run;

/* unconditional simulation */
proc sim2d outsim=GRF plots=(sim); 
   grid x = 0 to 100 by 10 
        y = 0 to 100 by 10;
   simulate scale=8 range=30 form=Gauss numreal=5000 seed=12345; 
   mean 40;
run; 

/**********************/
/* Answer to exercise */
/**********************/
proc univariate data=GRF;
   where gxc=50 and gyc=50;
   var svalue;
   histogram svalue / normal;
run;
/**********************/


/***********************************************************************/

proc iml;
/* Given a p-dimensional MVN distribution and p-k fixed values for
   the variables x_{k+1},...,x_p, return the conditional mean and
   covariance for first k variables, conditioned on the last p-k 
   variables. The conditional mean is returned as a column vector. */
start CondMVNMeanCov(m, S, _mu, Sigma, _v);
   mu = colvec(_mu);  v = colvec(_v);
   p = nrow(mu);      k = p - nrow(v);

   mu1 = mu[1:k]; 
   mu2 = mu[k+1:p]; 
   Sigma11 = Sigma[1:k, 1:k];
   Sigma12 = Sigma[1:k, k+1:p]; *Sigma21 = T(Sigma12);
   Sigma22 = Sigma[k+1:p, k+1:p];
   m = mu1 + Sigma12*solve(Sigma22, (v - mu2));
   S = Sigma11 - Sigma12*solve(Sigma22, Sigma12`);
finish;

/* Given a p-dimensional MVN distribution and p-k fixed values
   for the variables x_{k+1},...,x_p, simulate first k 
   variables conditioned on the last p-k variables. */
start CondMVN(N, mu, Sigma, v);
   run CondMVNMeanCov(m, S, mu, Sigma, v);
   return( RandNormal(N, m`, S) );            /* mean is row vector */
finish;
store module=(CondMVN CondMVNMeanCov);

proc iml;
load module=(GaussianCov CondMVN);             /* load from storage */

/* Move the k columns specified by idx to the end of y */
start MoveColsToEnd(y, idx);
   i = setdif(1:ncol(y), idx);
   return( y[ , i||idx] );
finish;

/* Move last k cols of y to positions specified by idx, k=ncol(idx) */
start ReorderLastCols(y, idx);
   p = ncol(y);   k = p - ncol(idx);
   i = setdif(1:p, idx);              /* indices of other columns   */
   v = j(nrow(y),p);                  /* result vector              */
   v[,idx] = y[ ,k+1:p];              /* last k columns move to idx */
   v[,i] = y[ ,1:k];                  /* others interspersed        */
   return( v );
finish;

/* conditional simulation */
scale=8;  range=30;  mean=40;          /* params for GRF            */
s = do(0, 100, 5);
n = ncol(s);
idx = {3 10 19};                       /* s[idx] are observed locs  */
y  = {41 39.25 39.75};                 /* observed values           */

v = MoveColsToEnd(s,idx);              /* put observed locs last    */
C = GaussianCov(scale, range, v`);     /* compute covariance matrix */
z = CondMVN(1, j(1,n,mean), C, y);     /* conditional simulation    */
w = ReorderLastCols(z||y,idx);         /* restore to spatial order  */
NumSamples=6;
z = CondMVN(NumSamples, j(1,n,mean), C, y);
w = ReorderLastCols(z||repeat(y,NumSamples),idx);
SampleID = repeat( T(1:NumSamples), 1, N );   /* create ID variable */
x = repeat(s,1,NumSamples);           
create UGRF1D var {"SampleID" "x" "w"};  append;  close UGRF1D;
quit;

proc sgplot data=UGRF1D;
   title "Conditional Realizations of a Gaussian Random Field";
   series x=x y=w / group=SampleID;
   refline 40 / axis=y;
run;

/***********************************************************************/

/* conditional simulation */
proc sim2d data=Sashelp.Thick outsim=GRF plots=(sim); 
   coordinates xc=East yc=North;
   grid x = 0 to 100 by 10 
        y = 0 to 100 by 10;
   simulate var=Thick
            scale=8 range=30 form=Gauss numreal=5000 seed=12345; 
   mean 40;
run; 

/********************************************************************
 Simulating Data from a Spatial Point Process
 *******************************************************************/


/********************************************************************
 Simulating Data from a Homogeneous Poisson Process
 *******************************************************************/

proc iml;
/* simulate n points uniformly and independently on [0,a]x[0,b] */
start Uniform2d(n, a, b);
   u = j(n, 2);
   call randgen(u, "Uniform");
   return( u # (a||b) );                    /* scale to [0,a]x[0,b] */
finish;

start HomogPoissonProcess(lambda, a, b);
   n = rand("Poisson", lambda*a*b);
   return( Uniform2d(n, a, b) );
finish;

call randseed(123);
a = 3; b = 2;
u = HomogPoissonProcess(100/(a*b), a, b);
create Homog from u[c={x y}];
append from u;
close Homog;

/**********************/
/* Answer to exercise */
/**********************/
/* generate twice as many data (or double the intensity) */
/*
proc sgplot data=Homog;
   where y< -2/3*x + 2;
   scatter x=x y=y;
   xaxis min=0 max=3;
   yaxis min=0 max=2;
run;
*/
/**********************/


/********************************************************************
 Simulating Data from an Inhomogeneous Poisson Process
 *******************************************************************/

/* intensity of an inhomogeneous Poisson process */
start Intensity(x,y);
   return( 100/((x+y)##2 +1) );
finish;

/* simulate inhomogeneous Poisson process 
   lambda0 is intensity of underlying homogeneous Poisson process */
start InhomogPoissonProcess(lambda0, a, b);
   u = HomogPoissonProcess(lambda0, a, b);
   lambda = Intensity(u[,1], u[,2]);
   r = rand("Bernoulli", lambda/lambda0);
   return( u[loc(r),] );   
finish;

lambda0 = 100;  a = 3;  b = 2;                     /* max intensity */
z = InhomogPoissonProcess(lambda0, a, b);
create Inhomog from z[c={x y}];  append from z;  close Inhomog;
store module=(Uniform2D HomogPoissonProcess);
quit;

proc sgplot data=Homog;
   scatter x=x y=y;
   xaxis grid;
   yaxis grid;
run;

proc sgplot data=InHomog;
   scatter x=x y=y;
   xaxis grid;
   yaxis grid;
run;

/********************************************************************
 Simulating Data from a Regular Process
 *******************************************************************/

/* Simulate regular point process by thinning  */
proc iml;
call randseed(123);
a = 3; b = 2;
load module=(Uniform2D HomogPoissonProcess);   /* load from storage */

u = HomogPoissonProcess(100/6, a, b);
n = nrow(u);
d = distance(u);               /* matrix of pairwise distances      */
age = j(n,1);                  /* give points random "age" in [0,1] */
call randgen(age, "Uniform");
delta = 0.5;                   /* thin points closer than delta     */
d[do(1, n##2, n+1)] = .;       /* assign diagonal to missing        */

/* Sort points (and distances) according to age */
call sortndx(ndx, age, 1, 1);       /* index for age (desc order)   */
z = u[ndx,];                        /* contains the sorted points   */
d = d[ndx, ndx];

/* Retain s if no "older" point within distance delta */
keep = j(n,1,0);                    /* 0/1 indicator variable       */
keep[1] = 1;                        /* keep the oldest point        */
do i = 2 to n;
   v = d[i, 1:i-1];                 /* distances of older points    */
   j = xsect(loc(v < delta),        /* older points that are close  */
             loc(keep) );           /* points that are retained     */
   keep[i] = (ncol(j)=0);           /* retain this point?           */
end;
w = z[loc(keep), ];

/* write the retained and deleted points to a data set */
x1 = z[loc(keep), 1];
y1 = z[loc(keep), 2];
x2 = z[loc(^keep), 1];
y2 = z[loc(^keep), 2];

create Regular var {x1 x2 y1 y2};  append;  close Regular;
quit;

proc sgplot data=Regular nocycleattrs;
   title "Regular Point Pattern";
   scatter x=x2 y=y2 / legendlabel="Deleted"  markerattrs=(symbol=X) transparency=0.5;
   scatter x=x1 y=y1 / legendlabel="Retained" markerattrs=(symbol=CircleFilled size=12);
   xaxis display=(nolabel);
   yaxis display=(nolabel);
run;

/********************************************************************
 Other Techniques for Simulating Data from a Point Process
 *******************************************************************/


/**********************/
/* Answer to exercise */
/**********************/
data Thinning(drop=p);
set Homog;             /* points from a homogeneous Poisson process */
p = 0.5;               /* thinning parameter                        */
Keep = rand("Bernoulli", p);
run;

proc sgplot data=Thinning;
   scatter x=x y=y / group=Keep;
   xaxis display=(nolabel);
   yaxis display=(nolabel);
run;
/**********************/


/**********************/
/* Answer to exercise */
/**********************/
%let NumPts = 5;
proc iml;
use Homog;             /* points from a homogeneous Poisson process */
read next &NumPts var {x y};    /* just choose a few points         */
close Homog;

lambda = 10;                    /* mean of Poisson random variable */
N = j(nrow(x), 1);
call randseed(1);
call randgen(N, "Poisson", lambda);
NumOffspring = sum(N);

pts = j(NumOffspring, 3);
cov = {0.02 0,
       0    0.01};
b = 1;                                      /* beginning index      */
do i = 1 to nrow(x);
   e = b + N[i] - 1;                        /* ending index         */
   mean = x[i] || y[i];
   pts[b:e, 2:3] = RandNormal(N[i], mean, cov);
   pts[b:e, 1] = i;                         /* save group number    */
   b = e + 1;                    /* next group starts at this index */
end;

create Offspring from pts[c={"Group" x y}];
append from pts;
close Offspring;

data Cluster;
set Homog(obs=&NumPts) Offspring(in=c);
Child = c;
run;

proc sgplot data=Cluster;
   scatter x=x y=y / group=Group;
   xaxis display=(nolabel);
   yaxis display=(nolabel);
run;
/**********************/

