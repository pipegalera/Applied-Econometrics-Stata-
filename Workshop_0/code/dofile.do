******************************************
* 2 Initial options                      *
******************************************

cls
clear all
cap log c
global path "/Users/mac/GitHub/Applied-Econometrics-Stata-/Workshop_0"
cd "$path"
log using "$path/code/Workshop_1.log", replace
set more off

******************************************
* 3 Reading a text data set              *
******************************************

infile storeid str12 status2 empft2 emppt2 nmgrs2 /*
  */ wage_st2 hrsopen2 psoda2 pfry2 pmeal2 /*
  */ using "$path/raw_data/interview2.txt", clear

desc

duplicates report /*finds all duplicate values in each variable */

******************************************
* 4 Sorting and browsing the data        *
******************************************

sort storeid
gsort -storeid /*inverse sorting */

browse storeid empft2 emppt2 nmgrs2

******************************************
* 5 Getting the number of observations   *
******************************************

count
count if status2=="answer"
tab status2, missing /*tabulate missing values */
bysort status2: egen obsstatus2 = count(storeid) /*create a column with the count of status 2 for each restaurant*/
gen obsno=_n /*create a column with the observation number*/
bysort status2: gen obsno2=_n /* cac with the observation number sorted by status 2*/
bysort status2: gen obsno3=_N /* same as before, create a column with the count of status 2 for each restaurant*/

******************************************
* 6 Saving the data set                  *
******************************************

save "$path/mod_data/interview2.dta", replace

******************************************
* 7 Merging data sets                    *
******************************************

duplicates report storeid /*the variable we will merge by is unique or not*/

clear all /*just one dataset in the memory*/

cd "/$path/mod_data/"
use "$path/raw_data/interview1.dta", clear

sort storeid
duplicates report storeid

merge 1:1 storeid using "$path/raw_data/interview2.dta"
drop _merge

******************************************
* 7.1 Labeling data sets                 *
******************************************

label var storeid "Unique fast-food restaurant id"
label var status2 "Character variable for the restaurant’s status for second interview"
label var empft2 "Number of full-time employed in second interview"
label var emppt2 "Number of part-time employed in second interview"
label var nmgrs2 "Number of managers and assistant managers in the second interview"
label var wage_st2 "Hourly starting wage in the second interview"
label var hrsopen2 "Number of hours open per day"
label var psoda2 "Price of medium soda including tax"
label var pfry2 "Price of small fries including tax"
label var pmeal2 "Price of a standard meal including tax"
label var status "Character variable for the restaurant’s status for second interview"
label var empft "Number of full-time employed in first interview"
label var emppt "Number of part-time employed in first interview"
label var nmgrs "Number of managers and assistant managers in the first interview"
label var wage_st "Hourly starting wage in the first interview"
label var hrsopen "Number of hours open per day in the first interview"
label var psoda "Price of medium soda including tax in the first interview"
label var pfry "Price of small fries including tax in the first interview"
label var pmeal "Price of a standard meal including tax in the first interview"
la var nj "Dummy for New Jersey. 1 = New Jersey, 0 = Pennsylvania"
la var co_owned "Dummy equal to 1 if company-owned and dummy equal 0 if franchisee-owned"
la var chain "Character variable for the chain for which the individual restaurant belongs. BurgerKing, KFC, Roy Rogers, and Wendys"

******************************************
* 8 Limiting the data set                *
******************************************

keep if (status2=="answer" & wage_st!=. & wage_st2!=.)| status2=="perman"

******************************************
* 9 Generate new variables               *
******************************************

*construct a variable measuring each store’s total employment
gen emptot	= emppt	* .5 + 	empft	+ nmgrs                                    
gen emptot2 = emppt2* .5 +	empft2 	+ nmgrs2

gen demp  	= emptot2-emptot /*change in total employment*/                                                          
gen dwage 	= wage_st2-wage_st /*change in wages*/ 

*construct dummies and a chain variable
gen kfc = (chain=="KFC")
gen bk 	= (chain=="Burger King")
gen roy = (chain=="Roy Rogers")
gen wen = (chain=="Wendys")
gen chain_num = 1*(chain=="Burger King")+2*(chain=="KFC")+ ///
	3*(chain=="Roy Rogers")+4*(chain=="Wendys")


* interactions between chain dummies and the nj dummy
xi i.chain /*create dummies of the selected string variable (alternative)*/
xi i.chain*i.nj, noomit /*STATA will select all variables starting by "_Ichain" no matter their suffix */

save "$path/mod_data/interview2.dta", replace

******************************************
* 10 Descriptive statistics              *
******************************************

summarize

su wage_st wage_st2, detail /*summarize detailed of selected variables*
At least 25 per cent of the stores’ starting wages are equal to the minimum 
wage of $4.25, but at the second interview less than 10 per cent of the restaurants
have a minimum wage of $4.25. */

tab status2

egen wage_stM = mean(wage_st) /*used egen given that it's the mean of another variable*/

bysort chain: egen wage_stchain = mean(wage_st) /*average starting wage for each of the fast food chains*/

tab chain, g(chaindum)

ta chain nj, column /*fast-food chains distribution across  NJ and Penn */

tab1 chain nj

tabstat empft emppt nmgrs wage_st hrsopen, by(nj)

******************************************
* 11 Graphs                              *
******************************************

graph drop _all /*display every graph created by the do file*/

hist wage_st, name(fig1)
hist wage_st2, name(fig2)
graph combine fig1 fig2, name(fig3)
graph save fig3 "$path/graphs/figure3.gph", replace
graph export "$path/graphs/figure3.png", name(fig3) replace

graph bar (mean) dwage, over(nj) name(fig4)
/* The increase in the minimum wage in New
Jersey had a larger effect of starting wages 
in New Jersey compared to Pennsylvania? This
point it's essential for the Dif-n-Dif*/

preserve /*create a temporary copy of active dataframe*/

keep storeid chain nj wage_st wage_st2 emptot emptot2
rename wage_st wage_st1
rename emptot emptot1
reshape long wage_st emptot, i(storeid) j(time) /*
for every storeid create an after/before variable(time), reshaping
the wages and total employment in just 1 column*/
collapse (mean) wage_st emptot, by(nj time) /*we are just interested in the means*/
graph twoway (connected wage_st time if nj==0, xlabel(1(1)2)) ///
			 (connected wage_st time if nj==1, xlabel(1(1)2)), ///
			 legend(label(1 "Pennsylvania") label(2 "New Jersey")) ///
			 name(fig5)
graph twoway (connected emptot time if nj==0, xlabel(1(1)2)) ///
			 (connected emptot time if nj==1, xlabel(1(1)2)), ///
			 legend(label(1 "Pennsylvania") label(2 "New Jersey")) ///
			 name(fig6)
graph combine fig5 fig6, name(fig7)

/* Both starting wages and employment in the fast food restaurants increase
in New Jersey, whereas starting wages and employment in the fast food restaurants
decrease in Pennsylvania. This contradicts standard economic theory. */

graph save fig7 "$path/graphs/figure7.gph", replace
graph export "$path/graphs/figure7.png", name(fig7) replace

restore /*going back to the original "unchanged" dataframe*/

******************************************
* 12 While loops and locals              *
******************************************

local i = 1
while `i' <= 4 {
	display "Results for chain number `i'
	su wage_st if chain_num == `i'
	local i = `i'+1
	}
	
******************************************
* 13 Accessing estimation results        *
******************************************

su wage_st
return list /*a list of all the currently saved results and their names*/

scalar avgwage = r(mean) /*save the results in a scalar*/

reg demp nj, robust /*surprising positive coefficient of employiment
diff over New Jersey, the state that raised the minimum wage*/

ereturn list /*to see here the OLS coefficients are saved: e(b)*/
matrix beta=e(b)
matrix list beta

scalar constant=beta[1,2]
disp constant

scalar beta1=_b[nj]
disp "The beta estimate for New Jersey is " beta1

scalar se1=_se[nj]
disp "The corresponding standard errors are " se1

scalar t1=beta1/se1
disp "The corresponding t-statistic is " t1
	
******************************************
* 14 Globals                             *
******************************************

global x nj bk kfc roy co_owned
reg demp $x, robust

******************************************
* 15 Producing tables for Excel and TeX  *
******************************************

* ssc install estout, replace

eststo clear
eststo Model1: reg demp nj, robust
eststo Model2: reg demp $x, robust

estout
estout, cells(b(star fmt(3)) se(par fmt(2))) legend
estout, cells(b(star fmt(3)) se(par fmt(2))) legend ///
	stats(r2 N vce, labels("R-squared" "No. of obs.")) ///
	varlabels(nj "New Jersey" bk "Burger King" kfc "KFC" ///
	roy "Roy Rogers" co_owned "Company-owned" ///
	_cons "Constant") varwidth(14)

estout using "$path/results/results1.xls", ///
	cells(b(star fmt(3)) se(par fmt(2))) ///
	legend stats(r2 N vce, labels("R-squared" "No. of obs.")) ///
	varlabels(nj "New Jersey" bk "Burger King" kfc "KFC" ///
	roy "Roy Rogers" co_owned "Company-owned" ///
	_cons "Constant") varwidth(14) replace

estout using "$path/results/results1.txt", ///
	cells(b(star fmt(3)) se(par fmt(2))) ///
	legend stats(r2 N vce, labels("R-squared" "No. of obs.")) ///
	varlabels(nj "New Jersey" bk "Burger King" kfc "KFC" ///
	roy "Roy Rogers" co_owned "Company-owned" ///
	_cons "Constant") varwidth(14) style(tex) replace
	
* ssc install outreg2, replace

label var nj "New Jersey"	
label var bk "Burger King"
label var kfc "KFC"
label var roy "Roy Rogers"
label var co_owned "Company-owned"	

outreg2 using "$path/results/Results4a.dta", replace ctitle(Model1) label: ///
reg demp nj, robust

outreg2 using "$path/results/Results4b.dta", replace ctitle(Model1) label: ///
reg demp $x, robust

preserve
use "$path/results/Results4b.dta", clear
export excel using Results4.xls, replace sheet("Model1")
use "$path/results/Results4b.dta", clear
export excel using Results4.xls, sheetreplace sheet("Model2")
restore
