********************************************************************************
*                                                                              *
* Mock Exam: "The effect of a job search course for unemployed workers"        *
*                                                                              *
********************************************************************************


******************************************
*   Initial options                      *
******************************************

cls
clear all
cap log c
global path "/Users/mac/GitHub/Applied-Econometrics-Stata-/Mock_Exam_2018"
cd "$path"
log using "$path/code/Mock_Exam_2018.log", replace
set more off

******************************************
*   Load data & labeling                 *
******************************************

use "$path/raw_data/job_search.dta", clear
lab var pnr "Social security number (anonymized)"
lab var month "Month after start of unemployment"
lab var age "Age by end of 2013"
lab var yob "Year of birth"
lab var mob "Month of birth"
lab var dob "Day of birth"
lab var educ "Completed further education (no=1, short=2, medium=3, long=4)"
lab var jobcourse "Dummy taking the value 1 if individual is or has been enrolled in job course"
lab var employed "Dummy taking the value 1 if individual is employed"

******************************************
*   Problem 1                            *
******************************************

* Problem 1.1 

gen treat = 0
replace treat = 1 if (dob <=15)

tabstat pnr month age age educ jobcourse, by(treat) stats(n mean p50 min max) c(s) longstub

ttest employed, by(treat)

* Problem 1.2

*share of employed workers as a function of the elapsed time since entering unemployment.

preserve
collapse employed, by(month jobcourse)
twoway (connected employed month if jobcourse==0) (connected employed month if jobcourse==1), ///
		legend(label(1 "Without jobcourse") label(2 "With jobcourse")) title("Panel A") saving(desc2, replace)
restore


preserve
collapse employed, by(month eligible)
twoway (connected employed month if eligible==0) (connected employed month if eligible==1), ///
		legend(label(1 "Ineligible") label(2 "Eligible")) ytitle("Employed") title("Panel B") saving(desc3, replace)
restore


graph combine desc2.gph desc3.gph
graph export "graphs/Problem1_2.png", replace

******************************************
*   Problem 2                            *
******************************************
/*dummies for the elapsed time since the start of the unemployment spell??????*/


* Problem 2.1
gen age_sq = age^2

eststo clear
eststo ols_1: reg employed jobcourse age age_sq i.educ i.month, r
eststo ols_2: reg employed jobcourse age age_sq i.educ i.month, cluster(yob)
eststo ols_3: reg employed jobcourse age age_sq i.educ i.month, cluster(pnr)
estout using "tables/Problem2.1.xls", cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) legend style(tex) replace

* Problem 2.2
eststo clear
eststo ols_4: reg employed treat age age_sq i.educ i.month, r
eststo ols_5: reg employed treat age age_sq i.educ i.month, cluster(yob)
eststo ols_6: reg employed treat age age_sq i.educ i.month, cluster(pnr)
estout using "tables/Problem2.2.xls", cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) legend style(tex) replace

* Problem 2.3
eststo clear
eststo ols_7: reg jobcourse treat age age_sq educ i.month, r
eststo ols_8: reg jobcourse treat age age_sq educ i.month, cluster(yob)
eststo ols_9: reg jobcourse treat age age_sq educ i.month, cluster(prn)
scalar b1st=_b[eligible]
estout using "tables/Problem2.3.xls", cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) legend style(tex) replace

* Problem 2.4
eststo ols_6: reg employed treat age age_sq i.educ i.month, cluster(pnr)
scalar brf=_b[treat]
eststo ols_9: reg jobcourse treat age age_sq educ i.month, cluster(yob)
scalar b1st=_b[treat]
disp "Indirect least squares estimate: " brf/b1st

******************************************
*   Problem 3                            *
******************************************

* Problem 3.1: job search course as an instrument for participation in the job search course.

* Exogenous variation.

* Problem 3.2: binned scatterplot around the 16th day of the month

binscatter age dob, rd(16) line(lfit)
binscatter educ dob if educ, rd(16)  line(qfit)

gen educ_primary=(educ==1)
gen educ_highschool=(educ==2)
gen educ_vocational=(educ==3)
gen educ_higher=(educ==4)
binscatter age dob, rd(16) name(balance_age)
binscatter educ_primary dob, rd(16) name(balance_pri)
binscatter educ_highschool dob, rd(16) name(balance_hs)
binscatter educ_vocational dob, rd(16) name(balance_voc)
binscatter educ_higher dob, rd(16) name(balance_higher)

graph combine balance_age balance_pri balance_hs balance_voc balance_higher
graph export "graphs/Problem3_2.png", replace
graph drop balance_age balance_pri balance_hs balance_voc balance_higher

ttest age, by(treat)
ttest educ, by(treat)

* Problem 3.3: 2SLS to estimate the effect of participation

eststo iv:ivregress 2sls employed age age_sq i.educ i.month (jobcourse=treat), first cluster(pnr)

* Problem 3.4

ttest employed, by(treat) unequal
return list /*r(mu_2);  r(mu_1)*/
scalar E_e1 = r(mu_2) /* E[employment| elegible = 1] = .8621606 */
scalar E_e0 = r(mu_1) /* E[employment| elegible = 0] = .8406495 */

ttest jobcourse, by(treat) unequal
return list /*r(mu_2);  r(mu_1)*/
scalar JC_e1 = r(mu_2) /* E[jobcourse| elegible = 1] = .3852884 */
scalar JC_e0 = r(mu_1) /* E[jobcourse| elegible = 0] = .0 */

scalar Y_diff = E_e1 - E_e0
scalar S_diff = JC_e1 - JC_e0
scalar Wald_estimator = Y_diff/S_diff
mat LATE=[Y_diff\S_diff\Wald_estimator]

frmttable using "tables/Problem3_4", statmat(LATE) sdec(3) title("LATE") ///
	       rtitle("E(Y|Z=1)-E(Y|Z=0)"\"E(S|Z=1)-E(S|Z=0)"\"Wald estimate") replace

******************************************
*   Problem 4                            *
******************************************	

* Problem 4.1   ¿Sin controles? ¿robust se?

eststo clear
eststo iv_2m :ivregress 2sls employed age age_sq educ (jobcourse=treat) if month == 2 , cluster(yob)
eststo iv_4m :ivregress 2sls employed age age_sq educ (jobcourse=treat) if month == 4 , cluster(yob)
eststo iv_6m :ivregress 2sls employed age age_sq educ (jobcourse=treat) if month == 6 , cluster(yob)
eststo iv_8m :ivregress 2sls employed age age_sq educ (jobcourse=treat) if month == 8 , cluster(yob)
eststo iv_10m:ivregress 2sls employed age age_sq educ (jobcourse=treat) if month == 10, cluster(yob)
eststo iv_12m:ivregress 2sls employed age age_sq educ (jobcourse=treat) if month == 12, cluster(yob)

estout, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
estout using "tables/Problem4_1.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace

* Problem 4.2

ttest jobcourse , by(treat)
scalar ED_Z1 = r(mu_2)
scalar ED_Z0 = r(mu_1)
scalar compliers_share = r(mu_2) - r(mu_1) /*P(compliers) = E(D|Z=1) - E(D|Z=0) */


/*P(compliers| educ=1) = P(D|Z=1, educ=1) - P(D|Z=0, educ=1)) */


ttest jobcourse if educ == 1, by(treat)
scalar compliers_educ1 = r(mu_2) - r(mu_1)

ttest jobcourse if educ == 2, by(treat) 
scalar compliers_educ2 = r(mu_2) - r(mu_1)

ttest jobcourse if educ == 3, by(treat) 
scalar compliers_educ3 = r(mu_2) - r(mu_1)

ttest jobcourse if educ == 4, by(treat) 
scalar compliers_educ4 = r(mu_2) - r(mu_1)


mat tab=[ED_Z1\ED_Z0\compliers_share\compliers_educ1\compliers_educ2\compliers_educ3\compliers_educ4] 

frmttable, statmat(tab) sdec(3) title("Characterizing compliers") ///
	       rtitle("E(D|Z=0)"\"E(D|Z=1)"\"Share of compliers" ///
				 \"Share of compliers with no education" ///
				 \"Share of compliers with short education" ///
				 \"Share of compliers with medium education" ///
				 \"Share of compliers with long education")


******************************************
*   Problem 5                            *
******************************************	

* Diff-n-Diff






 
