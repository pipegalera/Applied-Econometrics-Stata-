********************************************************************************
*                                                                              *
* Exercise 1: Crepon et al (2013)                                              *
*                                                                              *
********************************************************************************


******************************************
*   Initial options                      *
******************************************


cls
clear all
cap log c
global path "/Users/mac/GitHub/Applied-Econometrics-Stata-/Workshop_1"
cd "$path"
log using "$path/code/Workshop_1.log", replace
set more off

******************************************
*   Load data & labeling                 *
******************************************

use "$path/raw_data/CreponData.dta", clear
lab var quintuplets "covered areas of similar size and with comparable populations."
lab var LTFC "average share of individuals with a long-term fixed contract"
lab var LT "long-term employment"
lab var assigned "1 = Treated, 0 = Untreated"
lab var male "1 = Male, 0 = Female"
lab var pastudur "past unemploymnet duration"
lab var pastudursq "past unemployment duration squared"
lab var bac "education dummy"
lab var bac3 "education dummy"
lab var bac4 "education dummy"
lab var bac5 "education dummy"
lab var treatarea "treated area dummy"

******************************************
* Question 1                             *                             
******************************************

ttest LTFC, by(assigned) unequal /*t test with unequal variances*/
ttest LT, by(assigned) unequal

/*The intensive job search assistance by the private provider seems
to increase both measures of employment, but whereas the increase in
long-term fixed contract (LTFC) employment is significant at 1 percent
(t test = 2,52),the increase in in long-term (LT) employment is only 
significant on a 10 percent level (t test = 1,89).*/

*****************************************
* Question 2                            *                                     
*****************************************

/*yic = B0 + B1Zic + uic*/

reg LTFC assigned, r
reg LT assigned, r

*****************************************
* Question 3                            *                                     
*****************************************

/*When we randomize the treatment, we have that the difference indeed
estimates both ATE and ATT. But we need to notice that being assigned
to treatment is not the same as being given treatment. Some of the 
unemployed may refuse to take part in the intensive job search course. 
Therefore, we will only estimate the intention to treat e¤ect (ITT).*/

*****************************************
* Question 4                            *
*****************************************

glo controls i.quintuplets pastudur pastudursq cohort3-cohort10 ///
			 male bac bac3 bac4 bac5

reg LTFC assigned $controls , r
reg LT assigned $controls, r

/*The estimates of the treatment (B1) are very similar and are 
significant at, respectively, 5 and 10 percent*/

*****************************************
* Question 5                            *                                     
*****************************************

/* Given that the treatment is randomized, we would just need that the 
X's are mean independent of the error term: E(X) - E(X| ui) = 0 */

*****************************************
* Question 6                            *                                     
*****************************************

/*Crépon et al. (2013) include explanatory variables to show that
small sample noise does not seem to inuence the estimates.*/


********************************************************************************
* Question 7                                                                   *
********************************************************************************
eststo clear
foreach varii of varlist pastudur pastudursq cohort3 cohort4 cohort5 cohort6 ///
		cohort7 cohort8 cohort9 cohort10 male bac bac3 bac4 bac5  {
		reg `varii' assigned, robust
		eststo `varii'
}	

estout, cells(b(star fmt(3)) t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
*estout using question7.xls, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend

********************************************************************************
* Question 8                                                                   *
********************************************************************************
reg assigned i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5, robust
test pastudur pastudursq cohort3 cohort4 cohort5 cohort6 cohort7 cohort8 cohort9 cohort9 cohort10 male bac bac3 bac4 bac5

********************************************************************************
* Question 9                                                                   *
********************************************************************************
reg LTFC assigned treatarea i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5, robust
reg LT assigned treatarea i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5, robust
*Reparameterization to get estimates and t-values for b1+b2
gen treatarea_notassigned=treatarea*(1-assigned)
reg LTFC assigned treatarea_notassigned i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5, robust
reg LT assigned treatarea_notassigned i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5, robust

********************************************************************************
* Question 10                                                                  *
********************************************************************************
eststo clear
reg LTFC assigned treatarea i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5 if male==1, robust
eststo men_ltfc
reg LT assigned treatarea i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5 if male==1, robust
eststo men_lt
reg LTFC assigned treatarea i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5 if male==0, robust
eststo women_ltfc
reg LT assigned treatarea i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5 if male==0, robust
eststo women_lt

*Reparameterization to get estimates and t-values for b1+b2
reg LTFC assigned treatarea_notassigned i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5 if male==1, robust
eststo men_ltfc2
reg LT assigned treatarea_notassigned i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5 if male==1, robust
eststo men_lt2
reg LTFC assigned treatarea_notassigned i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5 if male==0, robust
eststo women_ltfc2
reg LT assigned treatarea_notassigned i.quintuplets pastudur pastudursq cohort3-cohort10 male bac bac3 bac4 bac5 if male==0, robust
eststo women_lt2

estout, cells(b(star fmt(3)) t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
*estout using question10.xls, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend

reg LTFC assigned treatarea i.quintuplets pastudur pastudursq cohort3-cohort10 male if male==1 & bac3+bac4+bac5==0, robust
reg LTFC assigned treatarea i.quintuplets pastudur pastudursq cohort3-cohort10 male if male==0 & bac3+bac4+bac5==0, robust


log close
