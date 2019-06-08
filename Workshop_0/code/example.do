* The structure of Stata commands
* command [varlist] [if exp] [in range] [weight] [, options]

******************************************
* 2 Initial options                      *
******************************************

clear all
capture log c
cd "Z:\Stata\"
log using example.log, replace
set more off

******************************************
* 3 Reading a text data set              *
******************************************

infile storeid str12 status2 empft2 emppt2 nmgrs2 /*
  */ wage_st2 hrsopen2 psoda2 pfry2 pmeal2 /*
  */ using interview2.txt, clear

duplicates report storeid
  
******************************************
* 4 Sorting and browsing the data        *
******************************************

sort storeid
gsort -storeid

browse storeid empft2 emppt2 nmgrs2

******************************************
* 5 Getting the number of observations   *
******************************************

count
desc
count if status2=="answer"
ta status2, missing
bysort status2: egen obsstatus2=count(storeid)
gen obsno=_n
bysort status2: gen obsno2=_n
bysort status2: gen obsno3=_N

******************************************
* 6 Saving the data set                  *
******************************************

save interview2.dta, replace
save "Z:\Stata\interview2.dta", replace

******************************************
* 7 Merging data sets                    *
******************************************

duplicates report storeid
clear all
use interview1.dta, clear
sort storeid
duplicates report storeid
merge 1:1 storeid using interview2.dta

******************************************
* 8 Limiting the data set                *
******************************************

drop _merge
keep if (status2=="answer" & wage_st!=. & wage_st2!=.)| ///
	status2=="perman"

******************************************
* 9 Generate new variables               *
******************************************

generate emptot=emppt*.5+empft+nmgrs                                                
gen emptot2=emppt2*.5+empft2+nmgrs2
g demp=emptot2-emptot                                                           
g dwage=wage_st2-wage_st

g bk=1 if chain=="Burger King"
replace bk=0 if chain !="Burger King"

g kfc=1 if chain=="KFC"
recode kfc .=0

g roy=(chain=="Roy Rogers")

g chain_num=1*(chain=="Burger King")+2*(chain=="KFC")+ ///
	3*(chain=="Roy Rogers")+4*(chain=="Wendys")

xi i.chain

xi i.chain*i.nj, noomit

******************************************
* 10 Descriptive statistics              *
******************************************

summarize

su wage_st wage_st2, detail

tabulate status2

egen wage_stM=mean(wage_st)

bysort chain: egen wage_stchain=mean(wage_st)

tabulate chain, g(chaindum)

ta chain nj, column

tab1 chain nj

tabstat empft emppt nmgrs wage_st hrsopen, by(nj)

******************************************
* 11 Graphs                              *
******************************************
graph drop _all

hist wage_st, name(fig1)
hist wage_st2, name(fig2)
graph combine fig1 fig2, name(fig3)
graph save fig1 figure1.gph, replace
graph export figure3.png, name(fig3) replace

graph bar (mean) dwage, over(nj) name(fig4)

preserve
keep storeid chain nj wage_st wage_st2 emptot emptot2
rename wage_st wage_st1
rename emptot emptot1
reshape long wage_st emptot, i(storeid) j(time)
collapse (mean) wage_st emptot, by(nj time)
graph twoway (connected wage_st time if nj==0, xlabel(1(1)2)) ///
			 (connected wage_st time if nj==1, xlabel(1(1)2)), ///
			 legend(label(1 "Pennsylvania") label(2 "New Jersey")) ///
			 name(fig5)
graph twoway (connected emptot time if nj==0, xlabel(1(1)2)) ///
			 (connected emptot time if nj==1, xlabel(1(1)2)), ///
			 legend(label(1 "Pennsylvania") label(2 "New Jersey")) ///
			 name(fig6)
graph combine fig5 fig6, name(fig7)
restore

******************************************
* 12 While loops and locals              *
******************************************

su wage_st if chain_num==1
su wage_st if chain_num==2
su wage_st if chain_num==3
su wage_st if chain_num==4

local ii=1
while `ii'<=4 {
	display "Results for chain no. `ii'"
	su wage_st if chain_num==`ii'
	local ii= `ii'+1
}

levelsof chain, local(levchain)
foreach ii of local levchain {
	display "Results for chain `ii'"
	su wage_st if chain=="`ii'"
}

******************************************
* 13 Accessing estimation results        *
******************************************

su wage_st
return list
scalar avgwage=r(mean)
reg demp nj, robust
ereturn list 
matrix beta=e(b)
matrix list beta
scalar constant=beta[1,2]
disp constant
scalar beta1=_b[nj]
scalar se1=_se[nj]
scalar t1=beta1/se1
disp "The beta estimate for New Jersey is " beta1
disp "The corresponding standard errors are " se1
disp "The corresponding t-statistic is " t1

******************************************
* 14 Globals                             *
******************************************

reg demp nj bk kfc roy co_owned, robust
global x nj bk kfc roy co_owned
reg demp $x, robust

******************************************
* 15 Producing tables for Excel and TeX  *
******************************************

* ssc install estout, replace

eststo clear
reg demp nj, robust
eststo Model1
reg demp $x, robust
eststo Model2 

estout
estout, cells(b(star fmt(3)) se(par fmt(2))) legend
estout, cells(b(star fmt(3)) se(par fmt(2))) legend ///
	stats(r2 N vce, labels("R-squared" "No. of obs.")) ///
	varlabels(nj "New Jersey" bk "Burger King" kfc "KFC" ///
	roy "Roy Rogers" co_owned "Company-owned" ///
	_cons "Constant") varwidth(14)

estout using Results1.xls, cells(b(star fmt(3)) se(par fmt(2))) ///
	legend stats(r2 N vce, labels("R-squared" "No. of obs.")) ///
	varlabels(nj "New Jersey" bk "Burger King" kfc "KFC" ///
	roy "Roy Rogers" co_owned "Company-owned" ///
	_cons "Constant") varwidth(14) replace

estout using Results2.txt, cells(b(star fmt(3)) se(par fmt(2))) ///
	legend stats(r2 N vce, labels("R-squared" "No. of obs.")) ///
	varlabels(nj "New Jersey" bk "Burger King" kfc "KFC" ///
	roy "Roy Rogers" co_owned "Company-owned" ///
	_cons "Constant") varwidth(14) style(tex) replace

reg demp nj, robust
outreg2 using Results3.xls, replace ctitle(Model1)
reg demp $x, robust
outreg2 using Results3.xls, append ctitle(Model2) 

label variable nj "New Jersey"	
label variable bk "Burger King"
label variable kfc "KFC"
label variable roy "Roy Rogers"
label variable co_owned "Company-owned"	

reg demp nj, robust
outreg2 using Results4a, dta replace ctitle(Model1) label
reg demp $x, robust
outreg2 using Results4b, dta replace ctitle(Model1) label

preserve
use Results4a_dta.dta, clear
export excel using Results4.xls, replace sheet("Model1")
use Results4b_dta.dta, clear
export excel using Results4.xls, sheetreplace sheet("Model2")
restore

******************************************
* End of program: Close log              *
******************************************

log c

*clear all
*erase interview2.dta
