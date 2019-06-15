********************************************************************************
*                                                                              *
* Exercise 2: Autor (2013)                                                     *
*                                                                              *
********************************************************************************


******************************************
*   Initial options                      *
******************************************

cls
clear all
cap log c
global path "/Users/mac/GitHub/Applied-Econometrics-Stata-/Workshop_2"
cd "$path"
log using "$path/code/Workshop_2.log", replace
set more off

******************************************
*   Load data & labeling                 *
******************************************

use "$path/raw_data/autor2003.dta", clear

******************************************
*   Descriptive statistics               *
******************************************

*Set time series and cross sectional dimensions

tsset state year /*strongly balanced*/

/*How many observations are contained in the data set and how are
they distributed on years.*/

tab year D  /*50 observations from 1975 to 1995*/

/*Log THS employment and the fraction of states with exception
rulings by year in the same diagram. Applying separate y-axes 
for the two variables */

preserve
collapse D  lnths lnemp stfrac, by(year) /*cannot make the graph without collapse*/
gr two 	(connect D year, 	 lcolor(red)  mcolor(red)  lpattern(solid) ) ///
		(connect lnths year, lcolor(blue) mcolor(blue) lpattern(solid) yaxis(2) ) ///
		, legend( lab(1 "Exception ruling") lab(2 "THS employment"))	///
		title("Exception ruling and THS employment by year") xtitle("Year") ///
		saving("graphs/Fig1", replace)
		graph export "graphs/Fig1.eps", replace

/*The figure shows that exception rulings increased gradually starting in 1983.
The fraction of states with exception ruling reached its maximum by 1988
where approximately 80 percent of all states had exception rulings. The blue
line show log THS employment. THS employment increased starting in 1984
and continued to increase throughout the observation period. The THS em-
ployment increase appears with a lag relative to the exception rulings.*/


/*The THS fraction of total employment and log total employment against year. 
Applying separate y-axes for the two variables*/

gr two 	(connect stfrac year, lcolor(red)  mcolor(red)  lpattern(solid) ) ///
		(connect lnemp year,  lcolor(blue) mcolor(blue) lpattern(solid) yaxis(2) ) ///
		, legend( lab(1 "THS fraction") lab(2 "Log of total employment"))	///
		title("Total employment and THS share by year") xtitle("Year")	 ///
		saving("graphs/Fig2", replace)
		graph export "graphs/Fig2.eps", replace

/*The figure illustrates that both total employment and the THS share are in-
creasing over the period considered. Also here the THS employment share
appears to increase with a lag relative to total employment, and it is thus not
clear how the exception ruling rate drives THS employment.*/


/*Log total employment and log THS employment against year*/

gr two 	(connect lnemp year,  lcolor(red)  mcolor(red)  lpattern(solid) ) ///
		(connect lnths year,  lcolor(blue) mcolor(blue) lpattern(solid) yaxis(2) ) ///
		, legend( lab(1 "THS empl") lab(2 "Overall empl")) ///
		title("Total and THS employment by year") xtitle("Year") ///
		saving("graphs/Fig3", replace)
		graph export "graphs/Fig3.eps", replace
restore

/*This figure shows that the log employment in the THS industry is very closely
correlated with overall employment over the period considered.*/

/* Based on the numbers presented in Figure 1-3 it is clear that THS employment
has grown in the same period as the exception rulings appeared across US
states. However, there is a very close connection between THS employment
and total employment, both of which increased in the same period as the
exception rulings swept across the country. It is therefore not possible to
2 conclude based on the graphical evidence whether exception rulings has caused
THS employments. */


*******************************************************
*   Differences-in Differences model                  *
*******************************************************

* xi i.state i.year i.state*year  /*Create dummies and interaction term*/

/*First, we estimate a simple version of the model that does not include
interaction term */

reg lnths D  lnemp                    	    , cluster(state)
outreg2 using "results/table1", replace excel dec(4) ctitle("OLS") ///
addtext(State FE, NO, Year FE, NO, StateXyear, NO, Clustered err, YES)

/*The effect of introducing an exception ruling is to increase THS employment by
54 percent. However, the regression does not include any of the fixed effects.*/

* State dummies, year dummies, and state*time trends;

reg lnths D lnemp i.year                	  , cluster(state) /*Year FE*/
outreg2 using "results/table1", replace excel dec(4) ctitle("OLS") ///
addtext(State FE, NO, Year FE, YES, StateXyear, NO, Clustered err, YES)

reg lnths D lnemp i.year i.state         	  , cluster(state) /*Year and State FE*/
outreg2 using "results/table1", append excel dec(4) ctitle("OLS") ///
addtext(State FE, YES, Year FE, YES, StateXyear, NO, Clustered err, YES)

xi i.state*year /*with the matsize given by the IC version cannot handle ## interaction*/

reg lnths D lnemp i.year i.state  _IstaXyear*  , cluster(state) /*FEs and interaction*/
outreg2 using "results/table1", append excel dec(4) ctitle("OLS") ///
drop(_Iy* _Istate* _IstaXyear* ) addtext(State FE, YES, Year FE, YES, StateXyear, YES, Errors, Cluster)

/*In column 2 of Table 1 the previous regression is repeated while controlling
for year fixed effects. Including year FEs changes the parameter estimate of
interest dramatically to -0.0402. The standard error is also reduced, but the
estimate of B1 is insigniffcant in spite of this. 
In column 3 state fixed effects are included and this reverses back the sign of 
the estimate of B1 and reduces the standard error further, but the estimate is 
still not signiffcantly different from zero. 
Finally, in column 4 state*year (i.e. state specific trends are included),
reduces the standard error further, so that B1 is now signiffcant. 

This result suggests that having an exception ruling increases THS employment by
about 15 percent. */

********************************************************************
*   Simple vs robust vs clustered standard errors                  *
********************************************************************

/* Simple OLS standard errors assuming standard errors that are robust to 
arbitrary form of heteroskedasticity */

reg lnths D lnemp i.year i.state  _IstaXyear*  /*OLS*/
outreg2 using "results/table2", replace excel dec(4) ctitle("OLS") ///
drop(_Iy* _Istate* _IstaXyear* ) addtext(State FE, YES, Year FE, YES, StateXyear, YES, Errors, Simple)


/*Cluster robust standard errors allowing for heteroskedasicity as well
as autocorrelation between observation within states*/

reg lnths D lnemp i.year i.state  _IstaXyear*  , r /*OLS + robust se*/
outreg2 using "results/table2", append excel dec(4) ctitle("OLS") ///
drop(_Iy* _Istate* _IstaXyear* ) addtext(State FE, YES, Year FE, YES, StateXyear, YES, Errors, Robust)

reg lnths D lnemp i.year i.state  _IstaXyear*  , cluster(state) /*OLS + State Cluster*/
outreg2 using "results/table2", append excel dec(4) ctitle("OLS") ///
drop(_Iy* _Istate* _IstaXyear* ) addtext(State FE, YES, Year FE, YES, StateXyear, YES, Errors, Cluster)

/*The table reveals how important the choice of standard errors is. Standard 
errors almost double going from OLS to clustered errors. In particular, it is 
the step where moving from robust errors to clustered errors that increases 
the standard errors. This suggest that it is important to take into account 
within-state autocorrelation of the error terms.*/

*********************************
*   Event case                  *
*********************************

* Plotting Dit against Yer for selected states to check adjusted expctations

gr two 	(connect D year if state==12, lcolor(red) mcolor(red) lpattern(solid) ) ///
		(connect D year if state==16, lcolor(green) mcolor(green) lpattern(solid) ) ///
		(connect D year if state==21, lcolor(blue) mcolor(blue) lpattern(solid) ) ///
		, legend( col(3) lab(1 "State 12") lab(2 "State 14") lab(3 "State 21")) ///
		saving("graphs/Fig4", replace)
		graph export "graphs/Fig4.eps", replace
		
/* Sequence of event dummies covering from four periods before the exception ruling 
to seven years after. */

sort state year
gen TreatYear_temp = year if D==1 & L.D==0
by state: egen TreatYear = max(TreatYear_temp)
drop TreatYear_temp

gen etime = year-TreatYear

* Lags
gen byte D_5=(etime==-5)
gen byte D_4=(etime==-4)
gen byte D_3=(etime==-3)
gen byte D_2=(etime==-2)
gen byte D_1=(etime==-1)
gen byte D0=(etime==0)
gen byte D1=(etime==1)
gen byte D2=(etime==2)
gen byte D3=(etime==3)
gen byte D4=(etime==4)
gen byte D5=(etime==5)
gen byte D6=(etime==6)
gen byte D7=(etime==7)
gen byte D7_=(etime>=7)

/*Estimation of an event study regression and the plot of the estimated parameters 
associated 95% confidence intervals in an event diagram.*/

global lags D_4 D_3 D_2 D_1 D0 D1 D2 D3 D4 D5 D6 D7 

reg  lnths  $lags i.year i.state _IstaXyear* , cluster(state) /*controlling for lags*/
outreg2 using "results/table3", replace excel dec(4) ctitle("SLP") keep(D_5 D_4 D_3 D_2 D_1 D0 D1 D2 D3 D4 D5 D6 D7 ) ///
addtext(State FE, YES, Year FE, YES, StateXt, YES, Clustered err, YES)

* 1.96 for a 95% interval 
gen lnths_up=(_b[D_4]+1.96*_se[D_4])*D_4 +(_b[D_3]+1.96*_se[D_3])*D_3 + (_b[D_2]+1.96*_se[D_2])*D_2 + (_b[D_1]+1.96*_se[D_1])*D_1  ///
			+ (_b[D0]*D0+1.96*_se[D0])*D0  + (_b[D1]+1.96*_se[D1])*D1  + (_b[D2]+1.96*_se[D2])*D2  + (_b[D3]+1.96*_se[D3])*D3 ///
			+ (_b[D4]+1.96*_se[D4])*D4 + (_b[D5]+1.96*_se[D5])*D5 + (_b[D6]+1.96*_se[D6])*D6 + (_b[D7]+1.96*_se[D7])*D7  

gen lnths_low=(_b[D_4]-1.96*_se[D_4])*D_4 +(_b[D_3]-1.96*_se[D_3])*D_3 + (_b[D_2]-1.96*_se[D_2])*D_2 + (_b[D_1]-1.96*_se[D_1])*D_1  ///
			+ (_b[D0]*D0-1.96*_se[D0])*D0  + (_b[D1]-1.96*_se[D1])*D1  + (_b[D2]-1.96*_se[D2])*D2  + (_b[D3]-1.96*_se[D3])*D3 ///
			+ (_b[D4]-1.96*_se[D4])*D4 + (_b[D5]-1.96*_se[D5])*D5 + (_b[D6]-1.96*_se[D6])*D6 + (_b[D7]-1.96*_se[D7])*D7  
			
gen lnths_hat=_b[D_4]*D_4 +_b[D_3]*D_3 + _b[D_2]*D_2 + _b[D_1]*D_1 +  _b[D0]*D0 + _b[D1]*D1  + _b[D2]*D2  + _b[D3]*D3 ///
	+ _b[D4]*D4 + _b[D5]*D5 + _b[D6]*D6 + _b[D7]*D7  

/*The event analysis is conducted for the period from four periods before treatment 
and until seven periods after */

keep if inrange(etime,-4,7)
collapse lnths_hat lnths_up lnths_low, by(etime)
replace lnths_up=. if etime==0
replace lnths_low=. if etime==0

gr two 	(connect lnths_hat etime  , lcolor(red)  lpattern(solid) msymbol(o) mcolor(red)) ///
		(line lnths_up etime  	  , lcolor(gs10) lpattern(dash) ) ///
		(line lnths_low etime     , lcolor(gs10) lpattern(dash) ) ///
		, legend( cols(3) lab(1 "Estimate") lab(2 "Upper CI" ) lab(3 "Lower CI" )) yline(0) 	///
		saving("graphs/Fig5", replace)
		graph export "graphs/Fig5.eps", replace


/*The estimated parameters presented in the graph suggest that there is no
effect before the exception rulings and there appears to be an increase in THS
employment after the ruling has taken place. However, the effect is small and
the estimated effects vary a lot. 

For example, in period 2 after the ruling there appears to be an effect of about 
14 percent, but for the period after the effect is estimated to be only 4 percent. 

In other words, the effect is estimated very imprecisely and is only signiffcantly
different from zero in year 5 after the law change. The pre-law effect is never 
significantly different from zero*/

* Testing for parallel pre-trends with an F-test
test D_4=D_3=D_2=D_1=0
*F(4, 49) distributed and the null hypothesis cannot be rejected at the 5% level.

* Testing for an effect of exceptions to the employment-at-will principle
test D1=D2=D3=D4=D5=D6=D7=0
/* It is F(7, 49) distributed and the p null hypothesis is rejected at the 7.6% level. 
This means that the post exception ruling dummies are jointly borderline significant.
The event analysis does hence not deliver a clear conclusion.*/

*********************************
*   Conclusion                  *
*********************************

/*The overall conclusion is that the data does not give a clear indication about the
effect of the exceptions. When estimating a simple static model there is some
evidence, but when estimating a full-blown event model the effect is only borderline
significant. The results depend a lot on the specification of the error structure and
on the modelling of the effect (dynamic vs static) and the effects are never very
precisely estimated. The evidence is thus too week to conclude anything decisively
about the effect of employment-at-will-exceptions*/

