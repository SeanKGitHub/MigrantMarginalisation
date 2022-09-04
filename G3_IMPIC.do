* loads IMPIC data and generates comprehensive migration policy index for each country in sample for use in QCA

version 15
clear all
set more off

* Creating country level variables from IMPIC data downloaded from http://www.impic-project.eu/data/ 
use "C:\data\LFS\data\impic2016.dta"

** reducing data set to only include countries I have data for in other datasets (split into two lines due to limits of inlist)
gen sample = .
replace sample = 1 if inlist(cntry, "at", "be", "ch", "de", "dk", "es", "fi", "fr", "gb")
replace sample = 1 if inlist(cntry, "gr", "ie", "it", "lu", "nl", "no", "pt", "se")

keep if sample == 1

* comprehensive index using labour, family reunification and asylum, followin helbling et al. 2020 Restricting immigration to foster migrant integration? A comparative study across 22 European countries
gen imCom3 = (AvgS_Reg_A + AvgS_Reg_B + AvgS_Reg_C)/3

* table to copy into excel
list cntry imCom3 if year == 2010, noobs
