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
lab var treatarea "dummy for being in an area with positive treatment share"

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

glo controls pastudur pastudursq cohort3-cohort10 ///
			 male bac bac3 bac4 bac5

reg LTFC assigned i.quintuplets $controls, r
reg LT   assigned i.quintuplets $controls, r

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

/* Crépon et al. (2013) include explanatory variables to show that
small sample noise does not seem to influence the estimates.*/


*****************************************
* Question 7                            *                                     
*****************************************

/* Explanatory variables for balancing tests:
indicative evidence that the randomization works well*/

eststo clear
foreach i of varlist pastudur pastudursq cohort3 cohort4 cohort5 cohort6 ///
		cohort7 cohort8 cohort9 cohort10 male bac bac3 bac4 bac5  {
		reg `i' assigned, robust
		eststo `i'
}

/*Given that it's not a perfect randomization, since Crépon et al. only use 
235 diferent randomized control areas, it can be that the individuals in the 
areas treate have higher levels of education.*/

/* estout, cells(b(star fmt(3)) t(par fmt(2))) starlevels(
* 0.10 ** 0.05 *** 0.01) legend */
estout using "results/balancetest.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels ///
(* 0.10 ** 0.05 *** 0.01) legend


*****************************************
* Question 8                            *                                     
*****************************************

/*Falsification Test: regress the assignment dummy on all the explanatory variables*/

reg assigned i.quintuplets $controls, r

/* Use of partial F-test to test for joint significance of the explanatory
variables except the quintuplets dummies. */

test pastudur pastudursq cohort3 cohort4 cohort5 cohort6 cohort7 cohort8 ///
		cohort9 cohort9 cohort10 male bac bac3 bac4 bac5

		
/* We obtain a F-statistic of F(15; 11744) = 2:62 with an associated p-value
of only 0:0006. 
The randomization is not working properly and we have non-random selection 
into treatment. This essentially implies that we need to assume unconfoundedness 
and so the best thing to do is to include the explanatory variables. In this 
case, we are lucky as we just saw that this did not change the estimates.*/


*****************************************
* Question 9                            *                                     
*****************************************

reg LTFC assigned treatarea i.quintuplets $controls, r
reg LT assigned treatarea i.quintuplets $controls, r

/* 
The average share with long-term fixed contract for the unemployed assigned to 
treatment will be B0 + B1 (coef assigned) + B2(coef treatarea), whereas the 
average for individuals being in a treatment area, but not being treated is 
given by B0 + B2 (bc the variable assigned would be 0).Finally, average share 
being in a non-treated area (and non-treated) is B0.

Therefore B1 gives the effect of being in a treatment area and being assigned
compared to being in a treatment area and not being assigned. This effect is 
positive and significant at respectively 1% and 5%.*/

*Reparameterization to get estimates and t-values for b1+b2.

gen treatarea_notassigned = treatarea * (1-assigned)
reg LTFC assigned 	treatarea_notassigned i.quintuplets $controls, r
reg LT 	 assigned 	treatarea_notassigned i.quintuplets $controls, r

/* B1+B2 gives the net effect of program assignment. Since this is insignificant,
it appears that e¤ect of the program seems to only redistribute jobs across 
unemployed in local labor markets.

The stable unit treatment value assumption (SUTVA) does not seem to be met.*/

*****************************************
* Question 10                           *                                     
*****************************************


eststo clear
eststo men_ltfc: reg LTFC 	assigned treatarea i.quintuplets $controls if male==1, r
eststo men_lt  : reg LT 	assigned treatarea i.quintuplets $controls if male==1, r

eststo women_ltfc: reg LTFC assigned treatarea i.quintuplets $controls if male==0, r
eststo women_lt  : reg LT   assigned treatarea i.quintuplets $controls if male==0, r

*Reparameterization to get estimates and t-values for b1+b2

eststo men_ltfc2: reg LTFC 	 assigned treatarea_notassigned i.quintuplets $controls if male==1, r
eststo men_lt2  : reg LT  	 assigned treatarea_notassigned i.quintuplets $controls if male==1, r

eststo women_ltfc2: reg LTFC assigned treatarea_notassigned i.quintuplets $controls if male==0, r
eststo women_lt2  : reg LT   assigned treatarea_notassigned i.quintuplets $controls if male==0, r

estout, cells(b(star fmt(3)) t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
estout using "results/manvwoman.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend

/*There are quite different results for men and women as effects for women are 
smaller and insignifcant.*/

reg LTFC assigned treatarea i.quintuplets pastudur pastudursq cohort3-cohort10 male if male==1 & bac3+bac4+bac5==0, r
reg LTFC assigned treatarea i.quintuplets pastudur pastudursq cohort3-cohort10 male if male==0 & bac3+bac4+bac5==0, r

/*However, if looking at individuals with low levels of education,
the e¤ects are of similar size for men and women, but significant at 5%
for men and only 10% for women*/

log close
