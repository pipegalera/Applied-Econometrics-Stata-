********************************************************************************
*                                                                              *
* Exercise 3: Lemieux and Milligan (2008)                                      *
*                                                                              *
********************************************************************************


******************************************
*   Initial options                      *
******************************************

cls
clear all
cap log c
global path "/Users/mac/GitHub/Applied-Econometrics-Stata-/Workshop_3"
cd "$path"
log using "$path/code/Workshop_3.log", replace
set more off

******************************************
*   Load data & labeling                 *
******************************************

use "$path/raw_data/LemieuxMilligan.dta", clear
lab var empc "mean employment"
lab var quebec "dummy for Quebec residents"
lab var year "year of the data, 1986 or 1991"
lab var age "relevant ages 20-39"
lab var nobs "number of observations in the cell"

******************************************
*   Descriptive statistics               *
******************************************

twoway scatter empc age if quebec==1 & year==1986, xline(30)
graph export "graphs/Fig1.eps", replace
*binscatter empc age if quebec==1 & year==1986, rd(30) line(none)

/*From graphical inspection, it definitely looks like there is a discontinuity
at 30 years of age in employment.
However, we notice that there is quite some nonlinearity. Below, we will
follow Lemieux and Milligan (2008) and just consider individuals aged 25
or more. This makes sense since then the estimate should not depend
crucially on the type of specification.*/

*******************************************
*  Linear regression discontinuity model  *                     
*******************************************

* Empc =B0 + B1age + B2treat + u

gen treat = (age>=30)
eststo lr_1 :  reg empc age treat if age>=25 & quebec==1 & year==1986 [weight=nobs]
eststo lr_1_r: reg empc age treat if age>=25 & quebec==1 & year==1986 [weight=nobs], r
*reg empc age treat if age>=25 & quebec==1 & year==1986
*gen treat_correct=0.170*(age==30)+0.913*(age==31)+1*(age>=32)
*reg empc age treat_correct if age>=25 & quebec==1 & year==1986 [weight=nobs]

/* When weighting the regression according to the cell number of obser-
vations, we obtain the identical estimates as running the regression on
individual data. We estimate a 4,0 percentage points lower employment
probability due to the higher social assistance benefit level. This effect is
significant at a 1 percent level. This is a local average treatment effect, i.e.
this estimate only holds for individuals around 30 years of age in Quebec.
A similar increase in social assistance benefits could give different effect
in another part of Canada (or the World) and for other ages. The LATE
could be smaller or larger than ATE and ATT. */

*******************************************
*  Extended RD model (1)                  *                     
*******************************************

gen age_sq=age^2
gen age_cu=age^3

eststo lr_2: reg empc age age_sq treat if age>=25 & quebec==1 & year==1986 [weight=nobs], r
eststo lr_3: reg empc age age_sq age_cu treat if age>=25 & quebec==1 & year==1986 [weight=nobs], r 

*We obtain slightly larger estimates of -5,0% (squared) and -4,8% (cubic)

*******************************************
*  Donut RD model                         *                     
*******************************************

eststo lr_donut1: reg empc age treat if age>=25 & quebec==1 & year==1986 & age!=30 [weight=nobs], r
eststo lr_donut2: reg empc age age_sq treat if age>=25 & quebec==1 & year==1986 & age!=30 [weight=nobs], r
eststo lr_donut3: reg empc age age_sq age_cu treat if age>=25 & quebec==1 & year==1986 & age!=30 [weight=nobs] 

/*We also obtain slightly larger estimates using the donut RD: -4,7% (lin-
ear), -5,7% (quadratic), and -5,8% (cubic). This indicates that a sig-
niffcant share of individuals recorded with the age of 30 turn 30 years
fairly late in the year and therefore receive the low social assistance bene-
fit rate.The problem is, that we do not know the exact birthday of each
individual.*/

*******************************************
*  Extended RD model (2)                  *                     
*******************************************

* Empc = B0 + B1age + B2age*treat + B3treat + u
* Taking into account of the slopes being different to the left and right of the cut-off.

gen age_treat =treat*age

eststo lr_4: reg empc age age_treat treat if age>=25 & quebec==1 & year==1986 [weight=nobs], r
expand 21 if quebec==1 & year==1986 & age==20 /*Duplicate observations*/
matrix b=e(b)
matrix list b

gen age_plot=age*(_n<=80)+(age+80-_n)*(_n>80)
gen empc_hat0_q4 = b[1,4] + b[1,1]*age_plot
gen empc_hat1_q4 = b[1,4] + (b[1,1]+b[1,2])*age_plot + b[1,3]

twoway (line empc_hat0_q4 age_plot, sort) (line empc_hat1_q4 age_plot, sort) if quebec==1 & year==1986, xline(30)
graph export "graphs/fig2.eps", replace

/*We construct two lines, one for age < 30 and for one age > 30. If we
measure the difference between these two lines in age = 30, we get the
right LATE estimate.
However, what we accidently do here is that we measure the difference
where age = 0. In this case, we have a fairly fat line to left of the
discontinuity and to the right of the discontinuity a line with a quite
negative slope. When extrapolating the lines to age = 0, we obtain that
the line for the treated is way above the line for the non-treated. This
way, the sign of the estimated effect changes and we obtain a meaningless
estimate.*/

*******************************************
*  Extended RD model (3)                  *                     
*******************************************

*Empci = B0 +B1(age -30)+ B2(age - 30)*treat + B3treat +ui (2)
gen age_mc       = age-30
gen age_mc_treat = treat*age_mc

eststo lr_5: reg empc age_mc age_mc_treat treat ///
				if age>=25 & quebec==1 & year==1986 [weight=nobs], r

/*Centralizing the running variable at age = 30, we estimate an effect of
similar size as before, -5,2% */

predict empc_hat_q6
twoway (scatter empc age) (line empc_hat_q6 age if age <30, sort) ///
						  (line empc_hat_q6 age if age >=30, sort) ////
						  if age>=25 & quebec==1 & year==1986, xline(30)
graph export "graphs/fig3.eps", replace

*******************************************
*  Extended RD model (4)                  *                     
*******************************************

gen age_mc_sq=age_mc^2
gen age_mc_treat_sq=treat*age_mc^2

eststo lr_6: reg empc age_mc age_mc_sq age_mc_treat age_mc_treat_sq treat ///
						   if age>=25 & quebec==1 & year==1986 [weight=nobs], r
predict empc_hat_q7_1

twoway (scatter empc age) (line empc_hat_q7_1 age if age<30, sort) ///
						  (line empc_hat_q7_1 age if age>=30, sort) ///
						   if age>=25 & quebec==1 & year==1986, xline(30)	   
graph export "graphs/fig4.eps", replace

eststo lr_7: reg empc age_mc age_mc_sq age_mc_treat age_mc_treat_sq treat ///
						   if quebec==1 & year==1986 [weight=nobs], r
predict empc_hat_q7_2

twoway (scatter empc age) (line empc_hat_q7_2 age if age<30, sort) ///
					      (line empc_hat_q7_2 age if age>=30, sort) ///
						   if quebec==1 & year==1986, xline(30)
graph export "graphs/fig5.eps", replace

/*In both cases, we estimate negative effects, -6,4% and -2,2% percentage
points, where the latter is when we also include observations for ages 20-
24 years. The estimated effect of -2,2% percentage points is not significant
even at a 10 percent level. Should we be concerned about this? No,
we are not concerned since the non-linearity, which reduced the estimate,
is induced by including ages further away from the cut-off. This is not a
concern since we are after a local estimate at the age of 30. It is important
that we do not see a similar decrease in the estimate when allowing for
squared terms when only considering persons aged 25-39. Furthermore,
it is important that the nonlinearities away from cuto-off can be explained
and does not visually look like the e¤ect of the discontinuity.*/

*******************************************
*  Falsification Test                     *                     
*******************************************

reg nobs age_mc age_mc_sq age_mc_treat age_mc_treat_sq treat if quebec==1 & year==1986

predict nobs_hat_q8

twoway (scatter nobs age) (line nobs_hat_q8 age if age<30, sort) ///
						  (line nobs_hat_q8 age if age>=30, sort) ///
						  if quebec==1 & year==1986, xline(30)
graph export "graphs/fig6.eps", replace

reg nobs age_mc age_mc_sq age_mc_ge30 age_mc_ge30_sq treat ///
						  if age>=25 & quebec==1 & year==1986

/*We need to show that there is no bunching at the age of 30 years.W ith the
existing data, we just depict a scatter plot of the number of observations
against the age. The graph suggests that there is no evidence of bunching
at the age of 30 years.*/

*******************************************
*  Falsification Test  (2)                *                     
*******************************************

/* Test whether other thresholds between ages of 27 and 33 are significant for the
first regression */
eststo clear

foreach i of numlist 27/33 {
 gen placebo`i' = (age>=`i')
 reg empc age placebo`i' if age>=25 & quebec==1 & year==1986 [weight=nobs],r
 eststo placebo`i'
}
estout, cells(b(star fmt(3)) t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
estout using "results/table1.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend

/* Only the e¤ect at ages 30 and 31 are significant. However, the latter gives
a lower e¤ect (of -3:2 percentage points) and it is only significant at 10
percent. */

*******************************************
*  Falsification Test  (3)                *                     
*******************************************

/* No discontinuity in the age for Quebec in 1991 and for the rest of Canada in 
both 1986 and 1991. */

eststo clear
eststo quebec1991:    reg empc age treat if age>=25 & quebec==1 & year==1991 [weight=nobs],r
eststo restCanad1986: reg empc age treat if age>=25 & quebec==0 & year==1986 [weight=nobs],r
eststo restCanad1991: reg empc age treat if age>=25 & quebec==0 & year==1991 [weight=nobs],r

estout, cells(b(star fmt(3)) t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
estout using "results/table2.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend

/*Surprisingly, the estimated effect is significant at, respectively, 10 and 5
percent for Quebec in 1991 and for the rest of Canada in 1986. However,
for Quebec in 1991 the e¤ect is positive. Finally, for the rest of Canada in
1991, the estimated e¤ect is positive and insignificant. It is a bit worrying
to obtain the negative e¤ect significant at 5 percent for the rest of Canada
in 1986, but notice that the estimate is much lower than the estimates for
Quebec in 1986.*/

*******************************************
*  Differences in Differences (DiD)       *                     
*******************************************

/*In this case the identifying assumption is that the macroeco-
nomic developments between 1986 and 1991 a¤ected Quebec and the rest
of Canada in a parallel way. We would need more time periods to show
whether the assumption of parallel trends is likely to be met for Quebec vs.
the rest of Canada.*/

gen treatDiD   = (age<=29)*(quebec==1)*(year==1986)
/*Notice that here we define the treatment as having a lower benefit level
whereas we in the regression discontinuity  analysis defined the treatment as
having a higher benefit level*/
gen placeboDiD = (age>=30)*(quebec==1)*(year==1986)
gen y1986 = (year==1986)
gen age29 = (age<=29)

eststo clear

eststo DiD_age_le29: /// Empc = B0 + B1_Quebec + B2_Year=1986 + B3_Quebec*Year=1986 + u
						reg empc quebec y1986 treatDiD if age<=29 [weight=nobs],r
						
/*We obtain an insignificant estimate of -1,6% percent-age points, that is the 
opposite sign as we expected. Less employment benefits should encourage employment.

This is likely to be due to the parallel trends assumption is being violated. 
In other words, itwas probably the case that the 1991 recession a¤ected Quebec 
more than the rest of Canada. Hence, we suspect that the rest of Canada is a poor
control group in this case.*/

eststo DiDplacebo_age_ge30: /// If we have parallel trends, we should expect that B3 is zero.
						reg empc quebec y1986 placeboDiD if age>=30 [weight=nobs],r
						
/*We can check whether Quebec was affected harder by the recession in 1991 by 
running the same regression on individuals aged 30-39 years. However, we estimate 
B3 to -5,4% (significant at a 1 percent level), so we conclude that the parallel
trends assumption seems to be violated.*/

eststo DiD_1986: ///
						reg empc quebec age29 treatDiD if year==1986 [weight=nobs],r
						
/*Here, the identifying assumption is that the differences in the labor market 
conditions between individuals aged below and above age 30 are similar in Quebec
and the rest of Canada. In this case, we estimate a significant effects of 6,8%
points, which is significant at 5 percent*/
						
eststo DiD_Quebec: ///
						reg empc y1986 age29 treatDiD if quebec==1 [weight=nobs],r

estout, cells(b(star fmt(3)) t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend
estout using "results/table3.xls", cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend

/*Here, the identifying assumption is that the local labor market conditions in 
Quebec were a¤ected similarly for persons aged below and above 30 years between 
1986 and 1991. We obtain an estimate of 6,9 % points, which is significant at 1%*/

log close

