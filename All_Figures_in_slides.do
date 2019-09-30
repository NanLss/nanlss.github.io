*********************************************
* Pre-Semester Course in Statistics 2019
*********************************************

********************************************************************************
* Tutor: Nan Hu
********************************************************************************

clear
set more off //this command sets off the annoying 'more'-text..to see what I mean, blabla

set obs 10000
set seed 10
*********************************************
* binomial Distribution
*********************************************

* PMF of a binomial with n=2, p=0.5 (this is equal to two bernoulli experiments)* 
*********************************************
gen binom=rbinomial(2,0.5)
histogram binom, xlabel(0(1)2.5)  discrete fraction fcolor(forest_green) lcolor(forest_green) barwidth(0.01) ylabel(#10) ytitle(f(x)) xtitle(X (=Number of Heads Tossed)) title(PMF when Tossing a Fair Coin Twice)


* CDF of a binomial with n=2, p=0.5 (this is equal to two bernoulli experiments)* 
*********************************************
cumul binom, gen(cum)
sort cum
g id= _n
drop if id > 5000
twoway (line cum binom), ytitle(F(x)) ylabel(#10) xtitle(X (=Number of Heads Tossed)) title(CDF when Tossing a Fair Coin Twice)

*********************************************
* Chi Square Distribution
*********************************************

* PDF of a Chi Square Distribution with 10df *
*********************************************
gen chi2=rchi2(10)
kdensity chi2, xlabel(#8) fcolor(forest_green) lcolor(forest_green)  ytitle(f(x)) xtitle(X (Follows a chi2 distribution with 10df))  title(PDF of a Continuous RV (here: Chi^2 distributed))


* CDF of a Chi Square Distribution with 10df *
*********************************************
cumul chi2, gen(cum2)
sort cum2
twoway (line cum2 chi2), xlabel(#8) ytitle(F(x)) ylabel(#10) xtitle(X (Follows a chi2 distribution with 10df)) title(CDF of a Continuous RV (here: Chi^2 distributed))


*********************************************
* Normal Distribution
*********************************************

* PDF of a Normal Distribution with mu=0, sigma^2=1 and a t distribution with 1df*
*********************************************
drop normal t
gen normal=rnormal(0,1.3)
gen t=rt(2)

twoway (kdensity normal) (kdensity t) if t<5 & t>-5 & normal<5 & normal>-5, ytitle(f(x)) xtitle(X) title(PDFs of a N(0,1.2) and a t(2))

clear all
*********************************************
* CLT simulation
*********************************************
clear all
set obs 10000

local N=10 // number of observations per replication
local R=10000 // number of replications

gen smeans=.

quietly{
	forvalues r=1/`R'{
	egen smean=mean(rpoisson(6)) in 1/`N'
	replace smeans=smean in `r' // store each trial
	drop smean
	}
}
	
kdensity smeans, normal ytitle(f(x)) xtitle(sample mean) title(Convergence of the sample mean of Poisson(6) to Normal)


* Simulate a Cauchy Distribution (which is a t(1) dist, for which the CLT does NOT work! *
*********************************************
local N=1000 // number of observations per replication
local R=10000 // number of replications

gen smeans=.

quietly{
	forvalues r=1/`R'{
	egen smean=mean(rt(1)) in 1/`N'
	replace smeans=smean in `r' // store each trial
	drop smean
	}
}
	
sum smeans
kdensity smeans, normal ytitle(f(x)) xtitle(sample mean) title(CLT fails for a Cauchy Distribution)


*********************************************
* Replicate the example t-test: Silver content of Byzantine coins
*********************************************
clear all
use $rawdata\coins.dta, replace
* display mean, median and ..
tabstat x1 x4, statistics(mean median sd variance)

* one variable
ztest x1=6.5
ztest x1=6.7

ztest x1=10
ztest x1=1

test x1=6.5
ttest x1=6.5

* two variables
* different sample sizes, assume known variance
ztest x1=x4, unpaired 

* different sample sizes and different variance
ttest x1=x4, unpaired unequal













