********************************************************************************
*                                                                              *
* Exercise 4: Angrist and Krueger (1991)                                       *
*                                                                              *
********************************************************************************


******************************************
*   Initial options                      *
******************************************

cls
clear all
cap log c
global path "/Users/mac/GitHub/Applied-Econometrics-Stata-/Workshop_4"
cd "$path"
log using "$path/code/Workshop_4.log", replace
set more off

******************************************
*   Load data & labeling                 *
******************************************

use "$path/raw_data/AngristKrueger1991.dta", clear
keep if yob>=1930 & yob<=1939 /*Reduce sample size*/
lab var lnwklywge "log weekly earnings"
lab var educ "years of schooling"
lab var race "dummy for race"
lab var smsa "dummy for living in the center city"
lab var married "dummy for being married"
lab var yob "year of birth"
lab var qob "quarter of birth"

******************************************
*   Simplest Regression                  *
******************************************

* lnwages = B0 + B1_X + B2_educ + u

glo controls race smsa neweng midatl enocent wnocent soatl esocent wsocent mt
eststo ols_1: reg lnwklywge educ 		   i.yob
eststo ols_2: reg lnwklywge educ $controls i.yob

estout, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
estout using "results/Table1.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace

/* With the most simple OLS regression, we find an estimate of the returns
to schooling of 7,1%, i.e. one more year of education increases the weekly
wage by 7,1%. Including more explanatory variables reduces the returns to 
education to 6,3%.

Problems: Endogeneity.
Education may be correlated with omitted variables in the error term. Two 
candidates of such omitted variables are ability and motivation: More able or 
motivated persons are more likely to have higher schooling levels and earn 
higher wages.

This implies that OLS is biased and inconsistent and in labor economics this
is often referred to as ability bias. What is the direction of the bias? 
Ability bias will imply that OLS estimate of returns to education is higher 
than the true returns. 

We need an instrument: In the US, most states require students to enter school
in the calendar year they turn 6 years. The ones that had born the December 31
and the 1 of January they are same aged byt the sencond will enter school 1 year
later. Given that compulsory schooling laws typically require that students 
remain in school until their 16th birthday, the second had to be in school also
1 year less.*/

******************************************
*   Graphs                               *
******************************************

* First stage: quarter of birth vs average years of schooling

gen quarter = yob + (qob/10) /*ordering by year of born, including quarters*/

bysort quarter: egen educ_qm 	  = mean(educ)
*gr tw connected educ_qm quarter, mlabel(qob) xlabel(, format(%4.0f))
*gr export "graphs/fig1.eps", replace

/*It shows a saw-toothed relationship between quarter of birth and the average 
years of schooling. The difference in educational levels between individuals 
born in di¤erent quarters is fairly small, but there seems to be a distinct 
pattern that individuals born in 3rd and 4th quarter have higher educational 
levels.*/

* Reduced-form relationship: quarter of birth vs average log weekly earnings
bysort quarter: egen lnwklywge_qm = mean(lnwklywge)
*gr tw connected lnwklywge_qm quarter, mlabel(qob) xlabel(, format(%4.0f))
*gr export "graphs/fig2.eps", replace

/*We find a similar saw-toothed relationship reduced-form relationship. It is
important to notice that the weakly earnings are not trending much over the 
birth cohorts 1930-1939.

Acording to educational psycologist research, the consensus seems to be that 
students starting at an older age are more mature and perform better in school.
If this unobserved ability is also rewarded in the labor market, this would 
imply that individuals born in the fist quarter of the year earn more. Hence, 
this effect will bias the estimate of the returns to schooling downwards. */

******************************************
*   Evaluation of levels of education    *
******************************************

* High school graduate (12 years or more)
bysort educ: gen educ_hs = (educ>=12)

* College graduate (16 years or more)
bysort educ: gen educ_cl = (educ>=16)

* Doctor graduate (20 years or more)
bysort educ: gen educ_phd = (educ>=20)

* At which education level quarters of birth have most effect

eststo clear
eststo ols_all: reg educ 	 i.qob i.yob, r
eststo ols_hs:  reg educ_hs  i.qob i.yob, r
eststo ols_cl:  reg educ_cl  i.qob i.yob, r
eststo ols_phd: reg educ_phd i.qob i.yob, r

estout, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
estout using "results/Table2.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace

/* We only see a distinct pattern for all levels of education and for the
proportion of high school graduates, which they are increasing in quarter
of birth. */

******************************************
*   Creating the Wald estimator          *
******************************************

*Instrumenting the first quarter
gen Z = 0
replace Z = 1 if qob==1

ttest lnwklywge, by(Z) unequal  /*E(Y|Z=0) = 5.903; E(Y|Z=1) =  5.890*/
return list /*r(mu_2);  r(mu_1)*/
scalar Y_diff = r(mu_2)-r(mu_1)
ttest educ, by(Z) unequal /*E(S|Z=0) = 12.797; E(S|Z=1) = 12.688*/
return list
scalar S_diff = r(mu_2)-r(mu_1)
scalar Wald_estimator = Y_diff/S_diff
mat WE=[Y_diff\S_diff\Wald_estimator] 

frmttable using "results/Table3", statmat(WE) sdec(3) title("Wald estimate") ///
	       rtitle("E(Y|Z=1)-E(Y|Z=0)"\"E(S|Z=1)-E(S|Z=0)"\"Wald estimate") replace

* Wald estimator using heteroscedasticity-robust standard errors. Same 10,2%

ivregress 2sls lnwklywge (educ=Z), r

/*This implies that increasing education by one year increases the wage by
10,2%. Hence, we actually find a higher estimate using an instrument to
take account of the endogeneity even though we thought the OLS estimate
was already upward-biased. Why?

We only estimate the local average treatment effects (LATE) and the compliers 
have higher returns to additional schooling. The hypothesis is that the 
compliers have higher discount rates*/

******************************************
*   Compliers                            *
******************************************

* The share of compliers
gen qtr34 = 0
replace qtr34 = 1 if qob>=3 /*Z*/
ttest educ_hs, by(qtr34) /*P(compliers) = E(D|Z=1) - E(D|Z=0) = */
return list /* People in the qrt 3 and 4 have more education */
scalar D_Z_0 = r(mu_1) 
scalar D_Z_1 = r(mu_2) 
scalar compliers_share = D_Z_1 - D_Z_0

/* We see that the share of compliers is low so there is no reason to believe
that LATE is close to ATE. */

* The share of compliers among treated
/*P(compliers| D=1) = P(compliers)*P(D=1|compliers)/ P(D=1) 
					= compliers_share* P(Z=1) / P(D=1)*/
					
su qtr
scalar prob_Z = r(mean)
su educ_hs
scalar prob_D = r(mean)
scalar compliers_among_treated = (compliers_share*prob_Z) / prob_D

* The share of compliers among non-treated
/*P(compliers| D=0) = P(compliers)*P(D=0|compliers)/ P(D=0) 
					= compliers_share* P(Z=0) / P(D=0)
					= compliers_share * [1-P(Z=1)] / [1-P(D=1)] */
					
scalar compliers_among_nontreated = (compliers_share*(1-prob_Z)) / (1-prob_D)

* The share of compliers among black
/*P(compliers| race=1) = P(D|Z=1, race=1) - P(D|Z=0, race=1)) */

ttest educ_hs if race ==1, by(qtr34)		
scalar compliers_share_among_black = r(mu_2) - r(mu_1) 
				
					
mat tab = [D_Z_0\D_Z_1\compliers_share\compliers_among_treated ///
		   \compliers_among_nontreated\compliers_share_among_black]
frmttable using "results/Table4", statmat(tab) sdec(3) title("Characterizing compliers") ///
	       rtitle("E(D|Z=0)"\"E(D|Z=1)"\"Share of compliers"\"Share of compliers among treated" ///
		         \"Share of compliers among non-treated"\"Share of compliers among blacks")
				 



			 

eststo clear
eststo twostage_1: ivregress 2sls lnwklywge 		  i.yob (educ=Z)
eststo twostage_2: ivregress 2sls lnwklywge $controls i.yob (educ=Z)
eststo twostage_3: ivregress 2sls lnwklywge 		  i.yob (educ=i.qob)
eststo twostage_4: ivregress 2sls lnwklywge $controls i.yob (educ=i.qob)

estout, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
estout using "results/Table4.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace

				 
/*The results are very similar to the Wald estimates. When the instrument
is truly exogenous and relevant, the IV estimate is consistent whether
or not we include explanatory variables. The main reason for including
explanatory variables is to obtain more e¢ cient estimates.*/				 
	
******************************************
*   More efficient 2SLS                  *
******************************************
		
/*Constructing a full set of interaction dummies between the three first quarters
of birth dummies and year of birth dummies (29 dummies in total4 ), which they
use as instruments.*/

eststo clear
eststo twostage_5: ivregress 2sls lnwklywge 		   i.yob (educ=i.qob#i.yob)
eststo twostage_6: ivregress 2sls lnwklywge $controls  i.yob (educ=i.qob#i.yob)

estout, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
estout using "results/Table5.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace

/* Return to education 0,089 and 0,081*/

/*Here, we see that estimates which are somewhat lower than previous Wald
estimates and more in the range of the initial OLS estimates. Since the
year of birth dummies are included in the wage equation, the e¤ect of ed-
ucation on the wage is identified by variation in education across quarters
of births within each birth year.*/

******************************************
*   First stage results                  *
******************************************

ivregress 2sls lnwklywge 			 i.yob (educ=i.qob), first
estat firststage
ivregress 2sls lnwklywge 			 i.yob (educ=i.qob#i.yob), first
estat firststage
ivregress 2sls lnwklywge $controls   i.yob (educ=i.qob), first
estat firststage	 			 

/**Looking at the first stage, we see that not all instruments are significant.
Clearly, the F-statistic is well above 10 when using only 3 instruments, but 
with the interactions between quarter and year of birth dummies, the F-statistic 
suggests that the estimates suffer from a weak instrument problem.*/			 

******************************************
*   LIML and JIVE estimates              *
******************************************

* Limited-information maximum likelihood 
eststo clear
eststo liml_1: ivregress liml lnwklywge 		     i.yob (educ=i.qob)
eststo liml_2: ivregress liml lnwklywge $controls  i.yob (educ=i.qob)
eststo liml_3: ivregress liml lnwklywge 		     i.yob (educ=i.qob#i.yob)
eststo liml_4: ivregress liml lnwklywge $controls  i.yob (educ=i.qob#i.yob)

estout, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
estout using "results/Table6.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace

/*LIML gives almost the same estimate for the cases with the three quarter of
birth instruments. This is as we would expect with a F-statistic above 30.
With the 29 instruments, where the F-test is below 5, LIML gives slightly
higher estimates tham 2SLS but the LIML estimates are not significantly different 
from the 2SLS estimates.

Furthermore, the standard errors are only slightly larger. Therefore, the
LIML estimates indicate that the weak instruments problem does not seem
to be a major concern.*/

* JIVE
eststo clear
xi i.qob*i.yob /*jive do not allowed for i. command*/

eststo jive_1: jive lnwklywge            _Iyob_* (educ=_Iqob_*)
eststo jive_2: jive lnwklywge $controls  _Iyob_* (educ=_Iqob_*)
eststo jive_3: jive lnwklywge 		     _Iyob_* (educ=_IqobXyob_*)
eststo jive_4: jive lnwklywge $controls  _Iyob_* (educ=_IqobXyob_*)

estout, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
estout using "results/Table7.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend replace

/*On the other hand, JIVE gives larger estimates and the estimate is only
marginally affected by the changing the specification and by interacting
dummies between the three first quarters of birth dummies and year of
birth dummies. From JIVE estimates, it seems more likely that we are
faced with a weak instrument problem. */

log close
