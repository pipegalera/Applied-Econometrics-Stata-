
** We'll use data from the National Health and Nutrition Examination Survey (NHANES) for our examples
webuse nhanes2

** Before we fit the model, let's investigate the variables
codebook highbp age bmi female

** Now we can fit the model
logit highbp age bmi female

** Now we would like to include \stliteral{region} in the model, let's take a look at this variable
codebook region

** For example
logit highbp age bmi i.female i.region

** We can tell Stata to show the base categories for our factor variables
set showbaselevels on

** The \stliteral{i.} operator can be applied to many variables at once:
logit highbp age bmi i.(female region)

** We can use \stliteral{region=3} as the base class on the fly:
logit highbp age bmi i.female b3.region

** We can use the most prevalent category as the base
logit highbp age bmi i.female b(freq).region

** Factor variables can be distributed across many variables
logit highbp age bmi b(freq).(female region)

** The base category can be omitted (with some care here) 
logit highbp age bmi i.female bn.region, noconstant

** We can also include a term for \stliteral{region=4} only
logit highbp age bmi i.female 4.region

** Here is our model with an interaction between \stliteral{age} and \stliteral{female}
logit highbp bmi c.age##female i.region

** To see these names we can replay the model showing the {\it coefficient legend}
logit, coeflegend

** To specify a joint test of the null hypothesis that the coefficients for the levels of \stliteral{region} are all equal to 0
test 2.region 3.region 4.region

** For example, to test all coefficients associated with \stliteral{i.region}
testparm i.region

** To test the coefficients associated with \stliteral{region} we need to store our model results. The name is arbitrary, we'll call them \stliteral{m1}
estimates store m1

** Now we can rerun our model without \stliteral{region}
logit highbp bmi c.age##female if e(sample)

** Now we store the second set of estimates
estimates store m2

** And use the \stliteral{lrtest} command to perform the likelihood ratio test
lrtest m1 m2

** We'll restore the results from \stliteral{m1} which includes \stliteral{region} even though the terms are not collectively significant
estimates restore m1

** \stliteral{test} can also be used to the equality of coefficients
test 3.region = 4.region

** For example, to obtain the difference in coefficients
lincom 3.region - 4.region

** Let's start with \stliteral{margins} in its most basic form
margins

** An alternative is to calculate the predicted probability fixing all the covariates at some value, often the mean
margins, atmeans

** Adding a factor variable specifies that the predictions be repeated at each level of the variable, for example
margins region

** We can obtain \stliteral{margins} for multiple variables
margins region female

** Or combinations of values, for example each combination of \stliteral{region} and \stliteral{female}
margins region#female

** For example to graph the last set of margins
marginsplot

** To obtain the average predicted probability setting \stliteral{age=40} specify
margins, at(age=40)

** \stliteral{at()} accepts number lists, so we can obtain predictions setting \stliteral{age} to \stliteral{20}, \stliteral{30}, ..., \stliteral{70}
margins, at(age=(20(10)70)) vsquish

** []
marginsplot

** For example, what is the predicted probability of high blood pressure for an male who is age 40, with a bmi of 25 and living in the midwest (\stliteral{region=2})? What is the predicted probability if the person is female?
margins female, at(age=40 bmi=25 region=2) 

** We can use the contrast operator \stliteral{r.} to compare the predicted probabilities for males and females
margins r.female, at(age=40 bmi=25 region=2) 

** We can also specify ranges of values for multiple variables, for example multiple values of \stliteral{age} and \stliteral{bmi}
margins, at(age=(20(10)70) bmi=(20(10)40))

** We can also combine the use of factor and continuous variables, for example
margins female, at(age=(20(10)70)) vsquish

** []
marginsplot, legend(order(3 "Males" 4 "Females"))

** The \stliteral{generate(age+5)} requests margins calculated at each observations value of \stliteral{age} plus 5
 margins, at(age=generate(age+5))

** We can specify \stliteral{at()} multiple times, to obtain predictions under different scenarios 
margins, at(age=generate(age)) ///
   at(age=generate(age+5)) at(age=generate(age+10))

** The \stliteral{over()} option produces predictions averaging within groups defined by the factor variable, for example, \stliteral{female}
margins, over(female) 

** Earlier we obtained average predicted probabilities at each level of \stliteral{region} using
margins region

** For pairwise comparisons of these margins we can add the \stliteral{pwcompare} option
margins region, pwcompare

** Adding the \stliteral{groups} option will allow us to see which levels are statistically distinguishable
margins region, pwcompare(groups)

** For example, to compare average predicted probabilities setting \stliteral{female=0} versus \stliteral{female=1} add the \stliteral{r.} prefix
margins r.female

** We can use the \stliteral{@} operator to contrast \stliteral{female} at each level of \stliteral{region}
margins r.female@region

** To perform contrasts at different values of a continuous variable use the \stliteral{at()} option
margins r.female, at(age=(20(10)70)) vsquish

** []
marginsplot, yline(0)

** []
margins, at(age=(20(10)70)) contrast(atcontrast(a) effects) vsquish

** Specifically, we obtained predicted probabilities using each case's observed value of \stliteral{age} and each case's observed value $+ 5$ years
margins, at(age=generate(age)) at(age=generate(age+5))

** For example, to compare the differences between males and females across levels of \stliteral{region} use 
margins r.female#r.region

** To apply Bonferroni's adjustment to an earlier contrast
margins r.female@region, mcompare(bonferroni)

** Specifying adjusted p-values with the \stliteral{pwcompare} option
margins region, mcompare(sidak) pwcompare

** Here is a graph of predicted probabilities across values of \stliteral{bmi}
margins, at(bmi=(12(5)62))

** []
marginsplot

** We can obtain the average marginal effect of \stliteral{bmi}
margins, dydx(bmi)

** We can do the same for all variables in our model
margins, dydx(*)

** For example, we can obtain the derviative with respect to \stliteral{age} at \stliteral{age=20}, \stliteral{30}, ..., \stliteral{70}
margins, dydx(age) at(age=(20(10)70)) vsquish

** Here we do something similar, setting \stliteral{female=0} and then \stliteral{female=1}
margins female, dydx(age) at(age=(20(10)70)) vsquish

** We can, of course, plot these marginal effects, to see how they change with different values of \stliteral{female} and \stliteral{age}
marginsplot

** We've been working with \stliteral{age} and \stliteral{region} but we'll take a look at the new variables
codebook houssiz rural

** Now we can fit our model
poisson houssiz i.region##i.rural age c.age#c.age

** To obtain the average predicted count, using the observed values of all covarites use
margins

** As before, we can request predicted counts at specified values of factor variables
margins region#rural

** And continuous variables
margins, at(age=(20(10)70)) vsquish

** []
marginsplot

** Predicted probability that \stliteral{houssiz=1}
margins rural, predict(pr(1))

** Predicted probability that $3 \leq$ \stliteral{houssiz} $\leq 5$
margins region#rural, predict(pr(3,5))

** To demonstrate this, we'll model self-rated health in a different version of the NHANES dataset
webuse nhanes2f
codebook health

** Our model is
ologit health i.female age c.age#c.age

** By default \stliteral{margins} will produce the average predicted probability of each value of \stliteral{health}
margins

** To request a single outcome we can use \stliteral{predict(outcome(}\emph{\#}\stliteral{))}
margins, predict(outcome(2))

** For multiple responses from a single command, repeat the \stliteral{predict()} option
margins, predict(outcome(1)) predict(outcome(2))

** To obtain predictions across values of \stliteral{age}
margins, at(age=(20(10)70)) pr(out(1)) pr(out(2)) vsquish

** []
marginsplot, legend(order(3 "Poor" 4 "Fair"))
