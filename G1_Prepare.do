* Load LFS data
* Clean data and generate variables for analysis
* Apply sample restrictions

* Data used is EU-LFS anonymised microdata, access granted and provided by Eurostat. Information on access available here: https://ec.europa.eu/eurostat/web/microdata/european-union-labour-force-survey 
* Data in loaded file 'countrySample1718' are years 2017 and 2018 for 17 countries: Austria, Belgium, Switzerland, Germany, Denmark, Spain, Finland, France, Greece, Ireland, Italy, Luxembourg, Netherlands, Norway, Portugal, Sweden, United Kingdom
* Converted to .dta and appended using Setup file provided by GESIS available here, 'LFS data from 2019 release' - https://www.gesis.org/en/missy/materials/EU-LFS/setups 

* egenmore package installed


version 15
clear all
set more off

* load data
cd C:/data/LFS/
use data/countrySample1718, clear

*recoding gender
gen female = .
replace female = 1 if sex == 2
replace female = 0 if sex == 1

* restricting sample to 20-64 year olds
drop if age < 20 | age > 64

*education dummy variables (3 levels, 1 is lower secondary, 2 is upper secondary, 3 third level)
tabulate hatlev1d, generate(edul) 

* dropping duplicate observations due to households being interviewed in multiple quarterly surveys in the same year
duplicates drop hhnum hhseqnum year country, force

* Creating occupational scale using ISEI
* Creating new variable isco08 for isei merge file to recognise. turning LFS isco 3 digit codes to 4 digit, and 2 digit to 3

gen isco4d = isco3d
replace isco4d = isco4d*10 if isco4d !=.a & isco4d !=.b 
gen isco08 = isco4d

* Merge ISEI score from 
// Source: http://www.harryganzeboom.nl/isco08/index.htm and http://www.harryganzeboom.nl/isco08/isco08.zip		
merge m:1 isco08 using C:/data/LFS/data/cr_isei.dta
* Drop using only cases (i.e., ISCO-ISEI combinations that weren't matched)
drop if _merge == 2

*create occupation quartiles 
egen quar_isei = xtile(isei08), by(country) nq(4) w(coeff)
*dummies for each quartile
tab quar_isei, gen(quar_isei)


*CREATE MIGRANT GROUPS

	*Generating 'PERIPHERY' sending country variable based on country of birth. Includes everywhere except EU15, North America and Oceania. Kogan 2006 uses similar method for 'third country nationals, but also exlcudes japan, which isn't possible with LFS data provided
	gen periphc = .
	replace periphc = 1 if countryb == 2 | countryb == 3 | countryb == 4 | countryb == 7 | countryb == 9 | countryb == 10 | countryb == 11 | countryb == 12 | countryb == 13 | countryb == 14 | countryb == 15 | countryb == 17 | countryb == 18 | countryb == 20

	*dichotomised migrant from periphery country variable for comparing people born in country X vs migrants from periphery group
	gen peripmigc = .
	replace peripmigc = 0 if countryb == 0
	replace peripmigc = 1 if periphc == 1

*check sample sizes of migrants from periphery countries
bys country: tab peripmigc


* Calculate extent of low-skill sector in each country using isco (following green 2019, OECD 2019 and Andersson 2019)
gen iscolow = .
replace iscolow = 1 if isco1d == 500 | isco1d == 900
replace iscolow = 0 if isco1d == 100 | isco1d == 200 | isco1d == 300 | isco1d == 400 | isco1d == 600 | isco1d == 700 | isco1d == 800 | isco1d == 0

gen countrylowisco = .

levelsof country, local(cntries)

foreach place of local cntries {
summarize iscolow if country == `place' [aw=coeff], meanonly
replace countrylowisco = r(mean) if country == `place'
}


* creating mean centred education and age vars
gen agecc = .

levelsof country, local(cntries)

foreach place of local cntries {
summarize age if country == `place', meanonly
replace agecc = age - r(mean) if country == `place'
}

gen educc = .

foreach place of local cntries {
summarize hatlev1d if country == `place', meanonly
replace educc = hatlev1d - r(mean) if country == `place'
}




*alternative country variable for graph axes
gen cnt = country
label define cntlab 1 "AT" 2 "BE" 4 "CH" 7 "DE" 8 "DK" 10 "ES" 11 "FI" 12 "FR" 13 "GR" 16 "IE" 18 "IT" 20 "LU" 23 "NL" 24 "NO" 26 "PT" 28 "SE" 31 "UK"
label values cnt cntlab

*countryindex
bys country: gen countryindex = _n
* country index with two obs, one for men and one for women
bys country female: gen countrygendex = _n

save data/G1_Prepare, replace


