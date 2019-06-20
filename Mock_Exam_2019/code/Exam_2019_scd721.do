********************************************************************************
*                                                                              *
* Exam 2019: "Measuring the effect of housing subsidies on rents"              *
*                                                                              *
********************************************************************************


******************************************
*   Initial options                      *
******************************************

cls
clear all
cap log c
global path "/Users/mac/GitHub/Applied-Econometrics-Stata-/Exam_2019"
cd "$path"
log using "$path/code/Exam_2019_scd721.log", replace
set more off

******************************************
*   Load data & labeling                 *
******************************************

use "$path/raw_data/groupdata5.dta", clear
gen ln_rent = ln(rent)
global covs age adults children income

******************************************
*   Problem 1                            *
******************************************

* 1 DA

gen D30 = 0
replace D30 = 1 if (m2 >=30 & m2< 55) 
gen D55 = 0
replace D55 = 1 if (m2>= 55)
gen miss = 0 
replace miss = 1 if (income == .)

su pnr
su age adults children m2 rent subsidy income
tabstat su , by(miss) stats(n mean p50 min max) c(s) longstub

	**** Summary statistics years of education 

putexcel set "tables/Problem1_1", modify


	putexcel A1 = ("Summary Statistics")
	putexcel B1 = ("`year'")
	
	putexcel A3 = ("Variable")
	putexcel B3 = ("Observations")
	putexcel C3 = ("Mean")	
	putexcel D3 = ("Standard deviation")

	
	global desc_var age adults children income m2 rent subsidy
	local row = 4

	
foreach v of varlist $desc_var {
	
	qui sum `v'

	local obs     : disp %4.0f r(N)	
	local varlabel: variable label `v'
	local mean    : disp %4.2f r(mean)
	local sd      : disp %4.2f r(sd)
	
	putexcel A`row' = ("`varlabel'")
	putexcel B`row' = ("`obs'")
	putexcel C`row' = ("`mean'")
	putexcel D`row' = ("`sd'")
	
	local row = `row' + 1

}


*twoway kdensity rent if (D30 == 1 & m2<= 30) || kdensity rent if (D30 == 0 & m2<= 30), ///
*	legend(label(1 "Si") label(2 "No")) title(Subvencion) 

* 2 Binscatter

binscatter rent m2, rd(30, 55) line(lfit) title("Panel A") name (gr1)
binscatter subsidy m2, rd(30, 55) line(lfit) title("Panel B") name (gr2)
gr combine gr1 gr2
gr export "graphs/Problem1_2.png", replace 

******************************************
*   Problem 2                            *
******************************************

* 3 Allowing the slope to be different to the left and to the right of the cut-off

gen m2_30 = m2 - 30
gen m2_55 = m2 - 50
gen m2_30_D30 = m2_30*D30
gen m2_55_D55 = m2_55*D55


eststo ols_045:  reg ln_rent D30 m2_30 m2_30_D30 $covs if (m2>=0 & m2<=45), r
outreg2 using "tables/Problem2.2.xls",  replace excel dec(3) se ctitle(OLS_30)
eststo ols_3580: reg ln_rent D55 m2_55 m2_55_D55 $covs if (m2>=35 & m2<=80), r
outreg2 using "tables/Problem2.2.xls",  append  excel dec(3) se ctitle(OLS_55)

* 4  Including polynomials in the running variable

gen m2_30_sq     = m2_30^2
gen m2_55_sq 	 = m2_55^2
gen m2_30_D30_sq = m2_30_D30^2
gen m2_55_D55_sq = m2_55_D55^2

eststo clear
eststo ols_sq30:  reg ln_rent D30 m2_30 m2_30_D30 m2_30_sq m2_30_D30_sq $covs, cluster(m2)
outreg2 using "tables/Problem2.4.xls",  replace excel dec(3) se ctitle(OLS_pol30)
eststo ols_sq50:  reg ln_rent D55 m2_55 m2_55_D55 m2_55_sq m2_55_D55_sq $covs, cluster(m2)
outreg2 using "tables/Problem2.4.xls",  append excel dec(3) se ctitle(OLS_pol55)

tab D30
tab D55

* 5 Robust vs Clustered standard errors

eststo clear
eststo ols_30r:  reg ln_rent D30 m2, r
outreg2 using "tables/Problem2.5.xls",  replace excel dec(3) se ctitle(OLS_30r)


eststo ols_sq30:  reg ln_rent D30 m2, cluster(m2)
outreg2 using "tables/Problem2.5.xls",  append excel dec(3) se ctitle(OLS_30m2)
			   
eststo ols_r:  reg ln_rent D55 m2, r
outreg2 using "tables/Problem2.5.xls",  append excel dec(3) se ctitle(OLS_50r)	
			
eststo ols_m2:  reg ln_rent D55 m2, cluster(m2)
outreg2 using "tables/Problem2.5.xls",  append excel dec(3) se ctitle(OLS_50m2)


* No hetorasticity

reg ln_rent D30 m2
predict ln_rent_pr
scatter ln_rent ln_rent_pr || lfit ln_rent ln_rent_pr
rvfplot, yline(0)


reg ln_rent D30 m2,r
predict ln_rent_pt
scatter ln_rent ln_rent_pt || lfit ln_rent ln_rent_pt
rvfplot 


******************************************
*   Problem 3                            *
******************************************

* 1			
hist m2, xline(30 50)
gr export "graphs/Problem3_1.png", replace 

twoway kdensity m2

* 2
eststo clear
binscatter age m2, rd(30, 55) line(lfit) name (gr3)
binscatter adults m2, rd(30, 55) line(lfit) name (gr4)
binscatter children m2, rd(30, 55) line(lfit) name (gr5)
binscatter income m2, rd(30, 55) line(lfit) name (gr6)
gr combine gr3 gr4 gr5 gr6
gr export "graphs/Problem3_2.png", replace

ttest age if (m2>=0 & m2<=45), by(D30)
return list
scalar age_30 = r(mu_2) - r(mu_1)
ttest adults if (m2>=0 & m2<=45), by(D30)
return list
scalar adults_30 = r(mu_2) - r(mu_1)
ttest children if (m2>=0 & m2<=45), by(D30)
return list
scalar children_30 = r(mu_2) - r(mu_1)
ttest income if (m2>=0 & m2<=45), by(D30)
return list
scalar income_30 = r(mu_2) - r(mu_1)

ttest age if (m2>=35 & m2<=80), by(D55)
return list
scalar age_50 = r(mu_2) - r(mu_1)
ttest adults if (m2>=35 & m2<=80), by(D55)
return list
scalar adults_50 = r(mu_2) - r(mu_1)
ttest children if (m2>=35 & m2<=80), by(D55)
return list
scalar children_50 = r(mu_2) - r(mu_1)
ttest income if (m2>=35 & m2<=80), by(D55)
return list
scalar income_50 = r(mu_2) - r(mu_1)

mat tab=[age_30\adults_30\children_30\income_30\age_50\adults_50\children_50\income_50] 

frmttable, statmat(tab) sdec(3) title("Diff in covariables by cut-off") ///
	       rtitle("Difference in age at c.o 30 " ///
				 \"Difference in # of adults at c.o 30 " ///
				 \"Difference in # of children at c.o 30 " ///
				 \"Difference in income at c.o 30 " ///	
				 \"Difference in age at c.o 50 " ///
				 \"Difference in # of adults at c.o 50 " ///
				 \"Difference in # of children at c.o 50 " ///
				 \"Difference in income at c.o 50 ")
				 
ttest age if (m2>=0 & m2<=45), by(D30)
return list
scalar age_30 = r(mu_2) - r(mu_1)
ttest adults if (m2>=0 & m2<=45), by(D30)
return list
scalar adults_30 = r(mu_2) - r(mu_1)
ttest children if (m2>=0 & m2<=45), by(D30)
return list
scalar children_30 = r(mu_2) - r(mu_1)
ttest income if (m2>=0 & m2<=45), by(D30)
return list
scalar income_30 = r(mu_2) - r(mu_1)

ttest age if (m2>=35 & m2<=80), by(D55)
return list
scalar age_50 = r(mu_2) - r(mu_1)
ttest adults if (m2>=35 & m2<=80), by(D55)
return list
scalar adults_50 = r(mu_2) - r(mu_1)
ttest children if (m2>=35 & m2<=80), by(D55)
return list
scalar children_50 = r(mu_2) - r(mu_1)
ttest income if (m2>=35 & m2<=80), by(D55)
return list
scalar income_50 = r(mu_2) - r(mu_1)

mat tab=[age_30\adults_30\children_30\income_30\age_50\adults_50\children_50\income_50] 

frmttable, statmat(tab) sdec(3) title("Diff in covariables by cut-off") ///
	       rtitle("Difference in age at c.o 30 " ///
				 \"Difference in # of adults at c.o 30 " ///
				 \"Difference in # of children at c.o 30 " ///
				 \"Difference in income at c.o 30 " ///	
				 \"Difference in age at c.o 50 " ///
				 \"Difference in # of adults at c.o 50 " ///
				 \"Difference in # of children at c.o 50 " ///
				 \"Difference in income at c.o 50 ")
				 
				 
tabstat age adults children income m2 rent subsidy if (m2==30), stats(n mean p50 min max) c(s) longstub
tabstat age adults children income m2 rent subsidy if (m2>=0 & m2<=45), stats(n mean p50 min max) c(s) longstub
tabstat age adults children income m2 rent subsidy if (m2==55), stats(n mean p50 min max) c(s) longstub
tabstat age adults children income m2 rent subsidy if (m2>=35 & m2<=80), stats(n mean p50 min max) c(s) longstub

* 3
reg age D30 m2_30 m2_30_D30, cluster(m2)
outreg2 using "tables/Problem3_3.xls",  replace excel dec(3) se ctitle(Age)
reg adults D30 m2_30 m2_30_D30 m2_30_sq m2_30_D30_sq, cluster(m2)
outreg2 using "tables/Problem3_3.xls",  append excel dec(3) se ctitle(Adults)
reg children D30 m2_30 m2_30_D30 m2_30_sq m2_30_D30_sq, cluster(m2)
outreg2 using "tables/Problem3_3.xls",  append excel dec(3) se ctitle(Children)
reg income D30 m2_30 m2_30_D30 m2_30_sq m2_30_D30_sq, cluster(m2)
outreg2 using "tables/Problem3_3.xls",  append excel dec(3) se ctitle(Income)

reg age D55 m2_55 m2_55_D55 m2_55_sq m2_55_D55_sq, cluster(m2)
outreg2 using "tables/Problem3_3.xls",  append excel dec(3) se ctitle(Age)
reg adults D55 m2_55 m2_55_D55 m2_55_sq m2_55_D55_sq, cluster(m2)
outreg2 using "tables/Problem3_3.xls",  append excel dec(3) se ctitle(Adults)
reg children D55 m2_55 m2_55_D55 m2_55_sq m2_55_D55_sq, cluster(m2)
outreg2 using "tables/Problem3_3.xls",  append excel dec(3) se ctitle(Children)
reg income D55 m2_55 m2_55_D55 m2_55_sq m2_55_D55_sq, cluster(m2)
outreg2 using "tables/Problem3_3.xls",  append excel dec(3) se ctitle(Income)


*3 

******************************************
*   Problem 4                            *
******************************************

* 1
reg 		   ln_rent D30 m2_30 m2_30_D30 $covs, cluster(m2)
outreg2 using "tables/Problem4_3.xls",  replace excel dec(3) se ctitle(OLS)
ivregress 2sls ln_rent D30 m2_30 $covs (subsidy=m2_30_D30), cluster(m2)
outreg2 using "tables/Problem4_3.xls",  append excel dec(3) se ctitle(IV)

* 2
ivregress 2sls ln_rent D30 m2_30 $covs (subsidy=m2_30_D30) if (m2>=0 & m2<45), first cluster(m2)
outreg2 using "tables/Problem4_2.xls",  replace excel dec(3) se ctitle(First stage)


******************************************
*   Problem 5                            *
******************************************




log close
