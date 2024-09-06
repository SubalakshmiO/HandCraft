*##################### SAS STATISTICAL DATA ANALYSIS;

FILENAME cardata 'C:\Anbu\SAS\BHU_2024\ExampleData\kaggleCarAuction.csv';

PROC IMPORT datafile = cardata out = cars dbms = CSV replace;
   getnames = yes;
   guessingrows = 1000;
RUN;

PROC FREQ data = cars;
   TABLES isbadbuy*isonlinesale / chisq expected;
RUN;


FILENAME busdata 'C:\Anbu\SAS\BHU_2024\ExampleData\Charm_City_Circulator_Ridership.csv';

PROC IMPORT datafile = busdata out = circ dbms = CSV replace;
   getnames = yes;
   guessingrows = 1000;
RUN;

PROC CONTENTS DATA = circ;
RUN;


PROC CORR data = circ;
  VAR orangeAverage purpleAverage;
DATA circ_sub;
  SET circ;
  count = orangeAverage;
  group = "orange";
  OUTPUT;
  count = purpleAverage;
  group = "purple";
  OUTPUT;
  KEEP count group;
RUN;

RUN;


PROC TTEST data = circ_sub;
  VAR count;
  CLASS group;
RUN;


/*######## ODS output;

*The data created by randomly sampling 400elementary schools from the California Department of Education’s 
API 2000 dataset. This data fi le contains a measureof school academic performance as well as other attributes 
of the elementary schools, such as, class size, enrollment,poverty, etc.
*/

libname mydata "C:\Anbu\SAS\BHU_2024\ExampleData";

DATA sasregelemapi;
   SET mydata.elemapi2;
   RUN;

PROC CONTENTS DATA = sasregelemapi;
RUN;

proc means data=sasregelemapi;
var api00 api99;
run;



ods trace on/listing;
proc means data=sasregelemapi;
var api00 api99;
run;
ods trace off;

* format ;
ODS OUTPUT output-object-specification<=SAS-data-set>;


ods output summary=apimeas(keep=api99_mean api99_stddev);
proc means data=sasregelemapi;
var api00 api99;
run;


/* #############Linear Regression;
We welcome you to cite our web book as you would cite other books or journal articles.
Here is the recommended method for citing this book.

Chen, X., Ender, P., Mitchell, M. and Wells, C. (2003). R
egression with SAS, from https://stats.idre.ucla.edu/stat/sas/webbooks/reg/default.htm .

*/


libname mydata "C:\Anbu\SAS\BHU_2024\ExampleData";

DATA sasregelemapi;
   SET mydata.elemapi2;
   RUN;

PROC CONTENTS DATA = sasregelemapi;
RUN;


proc reg data=sasregelemapi;
model api00 = enroll;
run;

proc reg data=sasregelemapi;
  model api00 = enroll ;
  plot api00 * enroll ;
run;  

proc reg data=sasregelemapi;
  model api00 = enroll;
  plot api00 * enroll / pred;
run;  
quit;

/* Another kind of graph that you might want to make is a residual versus fitted plot.  
As shown below, we can use the plot statement to make this graph.  
The keywords residual. and predicted. in this context refer to the residual value and predicted value from 
the regression analysis and can be abbreviated as r. and p. . */

proc reg data=sasregelemapi;
  model api00 = enroll ;
  plot residual. * predicted. ;
run;


proc reg data=sasregelemapi  ;
  model api00 = ell meals yr_rnd mobility acs_k3 acs_46 full emer enroll ;
run;  
ods trace on; 
proc reg data=sasregelemapi  ;
model api00 = ell meals yr_rnd mobility acs_k3 acs_46 full emer enroll / stb;
run;
ods trace off; 
proc reg data=sasregelemapi  ;
model api00 = ell meals yr_rnd mobility acs_k3 acs_46 full emer enroll / r;
run;

ods output FitStatistics=Output parameterestimates=output1;
proc reg data=sasregelemapi  ;
model api00 = ell meals yr_rnd mobility acs_k3 acs_46 full emer enroll;
run;
quit;

*################ reduced models;

proc reg data=sasregelemapi;
model api00 = ell meals yr_rnd mobility acs_k3 acs_46 full emer enroll;
test_class_size: test acs_k3, acs_46;
run;
quit;

*############### Correlations among the variables;

proc corr data=sasregelemapi;
var api00 ell meals yr_rnd mobility acs_k3 acs_46 full emer enroll ;
run;
*################# check normality;

proc univariate data=sasregelemapi noprint;
var  api00;
histogram / cfill=grayd0 normal kernel (color = red);
run;

data sasregelemapi;
set sasregelemapi;
lapi00=log(api00);
run;
 

proc univariate data=sasregelemapi noprint;
var  lapi00;
histogram / cfill=grayd0 normal kernel (color = red);
run;

*################ SAS MACROS

*https://users.phhp.ufl.edu/rlp176/Courses/PHC6089/SAS_Notes/14_Macros.html;


libname mydata "C:\Anbu\SAS\BHU_2024\ExampleData";
/*
For our examples in this chapter, we will use the Framingham dataset 
which was a large longitudinal, epidemiologic study of the risks of heart disease. 
Download and the SAS dataset file fghm113.sas7bdat and place it in a convenient folder.
Then change the LIBNAME path to point to this folder in the following code.

*/

*Create a temporary data set, so that we don't save changes to the original data set.;
DATA fghmTemp;
SET myData.fghm113;
RUN;

/*Now let's code some variables with some more descriptive values.
  SEX (Gender): 1=Men 
				2=Women
  Period (Examination cycle): 1=Period1 
							  2=Period2
							  3=Period3
  BPMEDS (Use of anti-hypertensive meds): 0=Not currently
										  1=Currently use
  CURSMOKE (Currently smoke?): 0=No
							   1=Yes
  DIABETES: 0=Not diabetic
			1=Diabetic
  PREVAP (Have angina pectoric?): 0=No
								  1=Yes
  PREVCHD (Coronary heart disease?): 0=No
									 1=Yes
  PREVMI (Myocardial infarction?): 0=No
								   1=Yes
  PREVSTRK (Had a stroke?): 0=No
				   			1=Yes
  PREVHYP (Hypertensive? sys bp >=140 or dyas bp >= 90): 0=no
														 1=yes
*/

PROC FORMAT;
VALUE YNfmt 0="No"
			1="Yes";
VALUE perfmt 1="Period 1"
			 2="Period 2"
			 3="Period 3";
VALUE gndrfmt 1="Men"
			  2="Women";
RUN;

DATA fghmtemp;
SET fghmtemp;
FORMAT prevap ynfmt.
	   diabetes ynfmt.
	   cursmoke ynfmt.
	   bpmeds ynfmt.
	   prevchd ynfmt.
	   prevmi ynfmt.
	   prevstrk ynfmt.
	   prevhyp ynfmt.
	   sex gndrfmt.;
RUN;

*Check to see if the formatting was done correctly;
PROC CONTENTS DATA = fghmtemp;
RUN;

PROC PRINT DATA = fghmtemp (obs=5);
RUN;

*########### Macro variables;
%LET macro-variable-name = value;
%LET iterations = 5;
%LET winner = Lance Armstrong;

/* To use the MACRO variable, you simply add the ampersand (&) prefix and stick the MACRO variable name where 
you want its value to be substituted.

The following SAS program uses three MACRO variables to specify the dataset name and the two variable names 
from this dataset to use to create a contingency table and chi-square test output. In this case, we produce a 
chi-square test and contingency table between diabetes and previous cardiovascular heart disease.
*/

%LET response = prevchd;
%LET predictor = diabetes;
%LET dataset = fghmtemp;

PROC FREQ DATA = &dataset;
   TITLE "&predictor vs &response";
   TABLE &predictor * &response / CHISQ;
RUN;

TITLE;

*MACRO Functions;

%MACRO macro-name(parameter-1=, parameter-2=,..., parameter-n= );
   macro-text
%MEND macro-name;

*Example macro;

%MACRO twobytwo(dataset, predictor, response);
PROC FREQ DATA = &dataset;
    TITLE "&predictor vs &response";
    TABLE &predictor*&response / CHISQ;
RUN;
TITLE;
%MEND twobytwo;

%twobytwo(fghmtemp, diabetes, prevchd);

*MPRINT System option;

%MACRO twobytwo(dataset, predictor, response);
PROC FREQ DATA = &dataset NOPRINT;
    TITLE "&predictor vs &response";
    TABLE &predictor*&response / CHISQ;
RUN;
TITLE;
%MEND twobytwo;

OPTIONS MPRINT;

%twobytwo(fghmtemp, diabetes, prevchd);

OPTIONS NOMPRINT;

*MACRO %DO Loop;

%DO index-variable = start TO end;
   SAS code;
%END;


*Suppose we want to run our two by two table code but for many different variables such as in the following SAS code;

PROC FREQ DATA = fghmtemp;
   TITLE "prevap vs prevchd";
   TABLE prevap * prevchd / CHISQ;
RUN;

PROC FREQ DATA = fghmtemp;
   TITLE "diabetes vs prevchd";
   TABLE diabetes * prevchd / CHISQ;
RUN;

PROC FREQ DATA = fghmtemp;
   TITLE "prevmi vs prevchd";
   TABLE prevmi * prevchd / CHISQ;
RUN;

PROC FREQ DATA = fghmtemp;
   TITLE "prevstrk vs prevchd";
   TABLE prevstrk * prevchd / CHISQ;
RUN;

PROC FREQ DATA = fghmtemp;
   TITLE "prevhyp vs prevchd";
   TABLE prevhyp * prevchd / CHISQ;
RUN;

/*The macro function %SYSFUNC
allows access by the macro processor to most data step
functions
The COUNTW function is used in SAS to count the number of words in a character string.
The SCAN function extracts words from a character string in SAS. 
SCAN(text, nth-word, [delimiters], [modifiers])
*/

%MACRO twobytwov2(dataset, predictor, response);
   %DO i=1 %TO %SYSFUNC(countw(&predictor, ' '));
        %LET dep = %SCAN(&predictor, &i);
        PROC FREQ DATA = &dataset;
            TITLE "&dep vs &response";
            TABLE &dep * &response / CHISQ;
        RUN;
        TITLE;
    %END;
%MEND;

*%twobytwov2(fghmtemp, prevap diabetes prevmi prevstrk prevhyp, prevchd);

%twobytwov2(fghmtemp, prevap diabetes, prevchd);


*################################# Conditional Logic in MACROs;

%IF condition %THEN action;
   %ELSE %IF condition %THEN action;
   %ELSE action;
   
%IF condition %THEN %DO;
   action;
%END;


*Example;
*we create a MACRO with a parameter that will take the values "Yes" or "No" to request risk difference
output from PROC FREQ in our twobytwo MACRO;

%MACRO twobytwov2(dataset, predictor, response, rd=No);
   %DO i=1 %TO %SYSFUNC(countw(&predictor, ' '));
        %LET dep = %SCAN(&predictor, &i);
        %IF &rd = No %THEN %DO;
            PROC FREQ DATA = &dataset;
                TITLE "&dep vs &response";
                TABLE &dep * &response / CHISQ;
            RUN;
            TITLE;
        %END;
        %ELSE %IF &rd = Yes %THEN %DO;
            PROC FREQ DATA = &dataset;
                TITLE "&dep vs &response";
                TABLE &dep * &response / CHISQ riskdiff;
            RUN;
            TITLE;
        %END;
    %END;
%MEND;

%twobytwov2(dataset=fghmtemp, predictor=diabetes, response=prevchd, rd=Yes);
*%twobytwov2(dataset=fghmtemp, predictor=diabetes, response=prevchd);


/*Data Driven Programs - CALL SYMPUT;

The following SAS program use the expected cell counts, calculated by PROC FREQ, to determine whether 
to return the chi-squared test p-value or Fisher's exact test p-value. Recall that one standard rule of 
thumb is that to use the chi-square test, all expected cell counts should be at least 5.

The following code is some prelimanary code that we run to figure out how to define how MACRO.
*/

/*First, we will need to write and test our code for selecting only the output we want before
  we make our updated macro. We want to run a continuity adjusted chi-square test or 
  Fisher's exact test if the expected counts are too small. We only want the table with row percentages
  and the result of the correct test in our output.*/

*ODS TRACE ON;
PROC FREQ DATA = fghmtemp;
   TABLE prevap*prevchd / EXPECTED chisq;
   ODS OUTPUT CrossTabFreqs = ct ChiSq = chi FishersExact = fet;
RUN;
*ODS TRACE OFF;

*Lot's of missing data. We only want to use non missing values;
PROC PRINT DATA = ct;
RUN;

/* Test how to clean the resulting datasets */
/* Remove the missing values and get the cells with epxected countes less than 5 */
title '/* Test how to clean the resulting datasets */';
PROC PRINT DATA=ct;
   WHERE expected < 5 and prevap ne . and prevchd ne .;
RUN;
title;

*Create an indicator variable that is 1 if there is at least one cell with expected count less than 5;
%LET low_count = 0;
DATA _NULL_; *Run through the data set without creating a new one;
   SET ct;
   IF EXPECTED < 5 and prevap ne . and prevchd ne . THEN CALL SYMPUT('low_count', 1);
RUN;

%PUT &=low_count; *Should show value of 1 in the log file.;

/* Now, that we can check to see if the expected cell counts are less than 5,
 * we need to see how we can clean the chi-square or Fisher tables to print only 
 * the test name and p-value.
 */

PROC PRINT DATA=chi;
RUN;

*Select the row for the continuity adjusted chi-square test;
DATA chi2;
   SET chi;
   WHERE statistic="Continuity Adj. Chi-Square";
   DROP TABLE DF VALUE;
RUN;

PROC PRINT DATA = chi2;
RUN;

*Do the same thing for Fisher's exact test;
PROC PRINT DATA = fet;
RUN;

DATA fet2 (RENAME = (cValue1 = Prob));
   SET fet;
   Statistic = "Fisher's Exact Test";
   WHERE NAME1 = "XP2_FISH";
   KEEP statistic cValue1;
RUN;

DATA fet2;
   RETAIN Statistic Prob;
   SET fet2;
RUN;

PROC PRINT DATA = fet2;
RUN;

/* Macro Excercise;

Exercises
Write a macro that discretizes a quantitative variable into four categories based on quantiles. That is:

if X < Q1 then group = 1

if Q1 < X < M then group = 2

if M < X < Q3 then group = 3

if Q3 < X then group = 4

The macro should have the following defnition - %quartilesmacro(mydata, qvar, round, out);

mydata: dataset containing the quantitative variable

qvar: name of quantitative variable

round: integer representing number of decimal places to round to

out: name of output dataset which contains the categorized variable 
which will have name qvar_cat, e.g. if the qvar is bmi then the output variable is bmi_cat.
*/

%macro quartilesmacro(mydata, qvar, round, out = out_dat);

ODS select none;
proc means data = &mydata Q1 Median Q3 maxdec = &round;
 var &qvar;
 ODS OUTPUT summary = quartiles;
run;
ods select all;

data _null_;
 set quartiles;
 call symput("Q1", strip(&qvar._Q1));
 call symput("M", strip(&qvar._Median));
 call symput("Q3", strip(&qvar._Q3));
run;

data &out;
 set &mydata;
 if not missing(&qvar) and &qvar < &Q1 then &qvar._cat = 1;
 else if &qvar < &M then &qvar._cat = 2;
 else if &qvar < &Q3 then &qvar._cat = 3;
 else &qvar._cat = 4;
run;

%mend;

%quartilesmacro(fghmtemp, bmi, round = 2, out = out_bmi);

proc freq data = out_bmi;
 tables bmi_cat;
run;




*##################### Simulation;

proc datasets lib=work nolist kill;
quit;
libname mydata "C:\Anbu\SAS\BHU_2024\ExampleData";
*https://blogs.sas.com/content/sgf/2021/11/02/creating-simulated-data-sets/;


/*
Important Note: if you want a reproducible series of random numbers using the RAND function, 
you must seed it by a call to STREAMINIT (with a positive integer argument) prior to its use. 
*/
call streaminit(132435); 

data Without;
   do i = 1 to 5;
      x = rand('Uniform');
      output;
   end;   drop i;
run;

data With;
   call streaminit(13579);
   do i = 1 to 5;
      x = rand('Uniform');
      output;
   end;
   drop i;
run;

* Repeat the above to see the difference;

*Old fashioned way to generate "random" events;
data Toss;
   do n = 1 to 10;
      if rand('uniform') lt .5 then Result = 'Tails';
      else Result = 'Heads';
      output;
   end;
run;

*More sophisticated program;
proc format;
   value Heads_Tails 0="Heads" 1="Tails";
run;
 
data Toss2;
   do n = 1 to 10;
      Results = rand('Bernoulli',.5);
      format Results Heads_Tails.;
      output;
   end;
run;

*Creating correlated x-y pairs;
data Corr;
   do i = 1 to 1000;
      x = rand('normal',100,10);
      y = .5*x + rand('Normal',50,10);
      output;
   end;
   drop i;
run;

proc corr data=corr;
var x y;
run;

*############### normal;

%let nobs = 200;
%let mnark1=5;
%let mnark2=0;

/* create a TYPE=COV data set */
data ACorr(type=CORR);
input _TYPE_ $ 1-8 _NAME_ $ 9-16 Y1 Y2 Y3 Y4 Y5 Y6;
datalines;
CORR     Y1      1 0.7 0.6 0.5 0.4 0.3
CORR     Y2      0.7 1 0.7 0.6 0.5 0.4
CORR     Y3      0.6 0.7 1 0.7 0.6 0.5
CORR     Y4      0.5 0.6 0.7 1  0.7 0.6
CORR     Y5      0.4 0.5 0.6 0.7 1 0.7
CORR     Y6      0.3 0.4 0.5 0.6 0.7 1
MEAN             20 19 18 15 15 13
STD		         7.75 8.37 8.94 9.22 9.49 9.49
N				 200 200 200 200 200 200
run;
data _null_;
call streaminit(0);   /* generate seed from system clock */
x = ceil( (2**31 - 1)*rand("uniform") ); 
call symput('useed',x);  **Creates a macro variable;
run;
%put &useed;

proc simnormal data=ACorr outsim=MVNtrt
               nr = &nobs         /* size of sample */
               seed = &useed; /* random number seed */
   var Y1-Y6;
run;
data Mvntrt;
     set Mvntrt;
	 drop Rnum;
	 group=1;
run;
data ACorr(type=CORR);
input _TYPE_ $ 1-8 _NAME_ $ 9-16 Y1 Y2 Y3 Y4 Y5 Y6;
datalines;
CORR     Y1      1 0.7 0.6 0.5 0.4 0.3
CORR     Y2      0.7 1 0.7 0.6 0.5 0.4
CORR     Y3      0.6 0.7 1 0.7 0.6 0.5
CORR     Y4      0.5 0.6 0.7 1  0.7 0.6
CORR     Y5      0.4 0.5 0.6 0.7 1 0.7
CORR     Y6      0.3 0.4 0.5 0.6 0.7 1
MEAN             20 20 19 19 18 17
STD		         7.75 8.37 8.94 9.22 9.49 9.49
N				 200 200 200 200 200 200
run;
data _null_;
call streaminit(0);   /* generate seed from system clock */
x = ceil( (2**31 - 1)*rand("uniform") ); 
call symput('useed',x);
run;

proc simnormal data=ACorr outsim=MVNpbo
               nr = &nobs         /* size of sample */
               seed = &useed; /* random number seed */
   var Y1-Y6;
run;

data Mvnpbo;
     set Mvnpbo;
	 drop Rnum;
	 group=0;
run;

data Mvn;
     set Mvnpbo Mvntrt;
run;

proc sort data=mvn out=mvnall;
by group;
run;


