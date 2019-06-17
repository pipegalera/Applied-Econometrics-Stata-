********************************************************************************
*                                                                              *
* Mock Exam: "The wage premium of union bargaining"                            *
*                                                                              *
********************************************************************************


******************************************
*   Initial options                      *
******************************************

cls
clear all
cap log c
global path "/Users/mac/GitHub/Applied-Econometrics-Stata-/Mock_Exam_1"
cd "$path"
log using "$path/code/Mock_Exam_1.log", replace
set more off

******************************************
*   Load data & labeling                 *
******************************************

use "$path/raw_data/union.dta", clear
lab var id "Social security number (anonymized)"
lab var firmid "Firm ID number (anonymized)"
lab var num_emp "Number of employees in the firm"
lab var wage "Annual earnings for worker"
lab var female "Dummy taking the value 1 if the subject is a female"
lab var age "Age of employee"
lab var union "Dummy taking the value 1 if individual is member of workers union"
lab var union_frac "Fraction of employees within firm who are union members"

******************************************
*   Problem 1                            *
******************************************

* 1) Descriptive analysis
gen treat = 0
replace treat = 1 if (union_frac >=0.5)

tabstat num_emp union age female wage, by(treat) stats(n mean p50 min max) c(s) longstub 

* 2) Degree of wage dispersion

gen ln_wage = ln(wage)

twoway kdensity ln_wage if treat == 1 || kdensity ln_wage if treat == 0, ///
	legend(label(1 "Collective agr.") label(2 "No collective agr.")) title(Union fraction 0.5) name(gr1)

*Does the conclusion depend on how far from the cut-off you delimit the sample?: No

twoway kdensity ln_wage if treat == 1 & union_frac <= 0.6 || kdensity ln_wage if treat == 0 & union_frac >= 0.4, ///
	legend(label(1 "Collective agr.") label(2 "No collective agr.")) title(Union fraction 0.40-0.60) name(gr2)
	
twoway kdensity ln_wage if treat == 1 & union_frac <= 0.55 || kdensity ln_wage if treat == 0 & union_frac >= 0.45, ///
	legend(label(1 "Collective agr.") label(2 "No collective agr.")) title(Union fraction 0.45-0.55) name(gr3)
graph combine gr1 gr2 gr3
graph export "/graphs/Problem1_Q2.png", replace		

******************************************
*   Problem 2: Regression Discontinuity  *
******************************************

* Question 1. Explain how a RD design can be used & assumptions

/*We can use RD to estimate the effect of being covered by a wage agreement
when there is a jump in the probability of being in covered by a union.

We have a sharp RD, where the probability of being covered by a union increases
discretely from 0 to 1 at the 50 percent cutoff.

The RD design boils down to comparing wages just below and above the 50
percent cutoff and attributing the difference in the wage below and above to
the union wage agreement. We only estimate a local treatment effect, which
holds at the 50 percent cutoff.

However, we can only use RD if there is no manipulation of the running
variable. Therefore, if it is the case that workers influence each other to join a
union when for example the profits are large, the RD design will be invalid for
estimating the effect of being covered by a wage agreement.*/

* Question 2. Binned scatterplot around the cut-off defined by the rule

binscatter ln_wage union_frac, rd(0.5) line(lfit) name(gr4)
binscatter ln_wage union_frac, rd(0.5)  line(qfit) name(gr5)
gr combine gr4 gr5
gr export "/graphs/Problem2_Q2.png", replace 

* Question 3. RD analyses where you estimate the effect of collective bargaining on earnings

* Question 3 (A). log_wage = B0 + B1treat + B2union_frac + u

eststo clear
eststo ols: 	      reg ln_wage treat union_frac
eststo ols_robustse:  reg ln_wage treat union_frac, r 
eststo ols_clustered: reg ln_wage treat union_frac, cluster(firmid)
estout using "tables/Problem2_Q3A.xls", cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace


* Question 3 (B). log_wage = B0 + B1treat + B2union_frac + B3X + u

eststo clear
eststo ols_controls: reg ln_wage treat union_frac female age num_emp, cluster(firmid)
estout using "tables/Problem2_Q3B.xls", cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace

/* Question 3 (C). A linear model allowing the slope to be different to the left and to the right of
the cut-off. */

gen union_frac_mc 		= union_frac - 0.5
gen union_frac_mc_treat = treat * union_frac_mc

eststo clear
eststo ols_slopes: 			reg ln_wage treat union_frac_mc union_frac_mc_treat					  , cluster(firmid)
eststo ols_slopes_controls: reg ln_wage treat union_frac_mc union_frac_mc_treat age female num_emp, cluster(firmid)
estout using "tables/Problem2_Q3C.xls", cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace

/* (Question 3 (D). A linear model including polynomials in the running variable, where polyno-
mials are allowed to be different on either side of the cut-off */

gen union_frac_mc_sq 		= union_frac_mc^2
gen union_frac_mc_treat_sq 	= union_frac_mc_treat^2

eststo clear
eststo ols_slopes_sq: 		   reg ln_wage treat union_frac_mc union_frac_mc_treat ///
								   union_frac_mc_sq union_frac_mc_treat_sq		              , cluster(firmid)
eststo ols_slopes_controls_sq: reg ln_wage treat union_frac_mc union_frac_mc_treat ///
								   union_frac_mc_sq union_frac_mc_treat_sq age female num_emp , cluster(firmid)
estout using "tables/Problem2_Q3D.xls", cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace

/* Question 3 (E). Expand the regression model from (a) to incorporate heterogenous effects of collective agreements
for age and gender while still estimating the ATE.*/

su female
scalar female_mean = r(mean)
gen u50_treat_female = treat*(female - female_mean)

su age
scalar age_mean    = r(mean)
gen u50_treat_age    = treat*(age - age_mean)

eststo ols_slopes_het: reg ln_wage treat union_frac_mc union_frac_mc_treat ///
								   union_frac_mc_sq union_frac_mc_treat_sq ///
								   age female num_emp ///
								   u50_treat_female u50_treat_age, cluster(firmid)
estout using "tables/Problem2_Q3E.xls", cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace


/* Question 3 (F). Based on the estimated parameters what is the expected earnings difference
between a 50 years old woman covered by a collective agreement and a man aged 30 who is not 
covered by a collective agreement?*/


lincom _b[treat] + _b[u50_treat_female]* (1 - female_mean) ///
				 + _b[u50_treat_age]* 	 (50 - age_mean)   ///
				 + _b[age] * (50-30) +_b[female]
				 
******************************************
*   Problem 3: Specification checks      *
******************************************

/* Question 1 Nonparametric regression discontinuity design using a triangular kernel and 
conventional standard errors. Conduct the estimation separately for the following three 
bandwidths: 0.025, 0.05, and 0.1. */

gen kernel_0025 = (1- abs(union_frac - 0.5)/ 0.025)
replace kernel_0025 = . if abs(kernel_0025) > 1

gen kernel_005 = (1- abs(union_frac - 0.5)/ 0.05)
replace kernel_005 = . if abs(kernel_005) > 1

gen kernel_01 = (1- abs(union_frac - 0.5)/ 0.1)
replace kernel_01 = . if abs(kernel_01) > 1

eststo clear
eststo ols_0025: reg ln_wage treat union_frac_mc union_frac_mc_treat [pweight=kernel_0025] , cluster(firmid)
eststo ols_005:  reg ln_wage treat union_frac_mc union_frac_mc_treat [pweight=kernel_005]  , cluster(firmid)
eststo ols_01:   reg ln_wage treat union_frac_mc union_frac_mc_treat [pweight=kernel_01]   , cluster(firmid)
estout using "tables/Problem3_Q1.xls", cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) legend style(tex) replace

/* Question 2 Examine whether the variables age, gender, and number of employees are smoothly
distributed around the cut-off and conclude whether the RD design can be credibly
applied to estimate the causal effect of collective agreement on earnings.*/

eststo clear
binscatter age union_frac, rd(0.5) line(lfit) name(gr6)
binscatter age union_frac, rd(0.5) line(qfit) name(gr7)
eststo ols_age:   reg age treat union_frac, cluster(firmid)
eststo ols_age_x: reg age treat union_frac ///
					  union_frac_mc union_frac_mc_treat union_frac_mc_sq union_frac_mc_treat_sq, cluster(firmid)
					  
binscatter female union_frac, rd(0.5) line(lfit) name(gr8)
binscatter female union_frac, rd(0.5) line(qfit) name(gr9)
eststo ols_fem:   reg fem treat union_frac, cluster(firmid)
eststo ols_fem_x: reg fem treat union_frac ///
					  union_frac_mc union_frac_mc_treat union_frac_mc_sq union_frac_mc_treat_sq, cluster(firmid)
					  
binscatter num_emp union_frac, rd(0.5) line(lfit) name(gr10)
binscatter num_emp union_frac, rd(0.5) line(qfit) name(gr11)
eststo ols_num:   reg num_emp treat union_frac, cluster(firmid)
eststo ols_num_x: reg num_emp treat union_frac ///
					  union_frac_mc union_frac_mc_treat union_frac_mc_sq union_frac_mc_treat_sq, cluster(firmid)

gr combine gr6 gr7 gr8 gr9 gr10 gr11
gr export "graphs/Problem3_Q2.xls", replace
estout using "tables/Problem3_Q2.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend style(tex) replace

/* Question 3 Check for manipulation of the running variable. Perform a formal test for manipu-
lation of the running variable and illustrate graphically.*/

twoway__histogram_gen union_frac, start(0) ///
  width(0.025) generate(h x, replace)
twoway (bar h x, barwidth(0.025) ///
  bstyle(histogram)) (lpoly h x if x<0.5, ///
  degree(1) lwidth(1)) (lpoly h x if x>=0.5, ///
  degree(1) lwidth(1))
gr export "graphs/Problem3_Q3.png", replace

DCdensity union_frac, breakpoint(0.5) generate(Xj Yj r0 fhat se_fhat) graphname("graphs/Problem3_Q3.png")

log close
