* Estimate Migrant Marginalisation in each country using logistic regression models
* Data used is EU-LFS data changed in 'G1_Prepare.do'

version 15
clear all
set more off
set scheme plotplainblind


cd "C:/data/LFS/data/"
use "G1_Prepare.dta"


	***** Estimate Migrant Marginalisation for each country, to form basis of ORc_quar in excel calibration file
* declare survey design using LFS variable "coeff"
svyset [pw=coeff] 

* Create vars to capture outputs of regression models for future use
gen logoddsc = .
gen ORc = .
gen ubc = .
gen lbc = .

levelsof country, local(cntries)

* Logistic Regression model for each country, calculating odds ratio of migrant v national working bottom quartile job, controling for education gender and age (Age and education centred around means)
	* capturing log odds, odds ratios, upper and lower bound confidence intervals
foreach land of local cntries {
svy: logit quar_isei1 peripmigc agecc female educc if country == `land'
replace logoddsc = _b[peripmigc] if country == `land'
replace ORc = exp(_b[peripmigc]) if country == `land'
replace lbc = exp(_b[peripmigc]-invnormal(0.975)*_se[peripmigc]) if country == `land'
replace ubc = exp(_b[peripmigc]+invnormal(0.975)*_se[peripmigc]) if country == `land'
}


*graphing odds ratios with confidence intervals
	* creating axis var so order low to high marginalisation 
egen orderperipc = axis(ORc), label (cnt)

twoway || rcap lbc ubc orderperipc if countryindex == 1, xtitle("") legend(off) || /*
*/ scatter ORc orderperipc if countryindex == 1, xlabel(#29, valuelabel labs(vsmall)) /*
*/msymbol(T) xtitle("") ytitle("odds ratios and 95% CIS for migrant v native having bottom quartile job", size(small)) /*
*/ title("{bf:Figure 1.} Odds ratios of migrants working bottom quartile job compared to nationals")
graph save Graph "C:\data\LFS\graphs\ORc_quar1718_Frontiers.gph", replace
graph export "C:\data\LFS\graphs\King_Figure_1.tif" // changing to tif format

* creating lists to paste into excel calibration file
* marginalisation (outcome)
sort ORc
list country ORc if countryindex == 1, noobs
* low skill sector
list country countrylowisco if countryindex == 1, noobs
