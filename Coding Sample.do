log close _all
clear all
set more off

cd "C:\Users\Jorge Mancilla\Documents\Stata Working Directory\Quantitative Methods II Final Project\"
log using "Regressions for Final Paper.log", replace
***********************************************************
***********************************************************
***********************************************************
*** This program takes the 2012 National Health and Nutrition Study (ENSANUT) data files
*** and runs linear regressions to obtain results for the SOCI 111 Final  Paper
***
*** Stata Version: 11
***
*** INPUT 1: prac_alim_inf_2012_f.dta // CHILD NUTRITIONAL PRACTICES DATASET
*** INPUT 2: General.dta // INDIVIDUAL-LEVEL DATASET WITH GENERAL INFORMATION
*** INPUT 3: Antropometria.dta // ANTHROPOMETRY INFORMATION FOR SUBSAMPLE OF INDIVIDUALS
*** INPUT 4: WHOStandards_Boys.csv // WHO CHILD GROTH STANDARDS (BOYS)
*** INPUT 5: WHOStandards_Girls.csv // WHO CHILD GROTH STANDARDS (GIRLS)
***********************************************************
***********************************************************
***********************************************************
set memory 400m

***********************************************************
***********************************************************
*** COMBINE ALL DATASETS TO FORM ANALYSIS FILE
***********************************************************
***********************************************************

************************************
*** READ IN  CHILD NUTRITIONAL PRACTICES DATA
************************************
use "prac_alim_inf_2012_f.dta", clear
desc

*** KEEP ONLY RELEVANT VARIABLES
keep folio /// Household ID Number
	code_upm /// Sampling Unit
	est_var /// Strata Area
	area /// Urban Area 
	nse10f /// Income Decile
	intp_madre /// Mom's ID Number 
	intp /// Child's ID Number
	edad_meses /// Child's age in months
	sexo /// Child's Sex
	inicio_t /// Child was breastfed within first hours of birth
	lac_adec /// Child is receiving adequate breastfeeding

*** SAVE ANALYSIS DATASET THAT INCLUDES ONLY THE RELEVANT VARIABLES
save "Analysis.dta", replace
clear
	
************************************
*** READ IN INDIVIDUAL-LEVEL DATA
************************************
use "General.dta", clear

*** KEEP ONLY RELEVANT VARIABLES
keep folio /// Household ID Number 
	intp /// Individual ID Number
	h212 /// Individual speaks indigenous language
	h218a /// Individual's education level

*** RENAME intp SO THAT CHILDREN ARE MATCHED TO THEIR MOTHER'S INFORMATION
rename intp intp_madre

*** MERGE MOTHER'S INDIVIDUAL-LEVEL DATA WITH CHILDREN'S NUTRITIONAL DATA.
merge 1:m folio intp_madre using "Analysis.dta"
drop if _merge == 1 
drop _merge

************************************
*** MERGE WITH ANTHROPOMETRY DATA
************************************

*** MERGE WITH ANTHROPOMETRY DATA. KEEP ONLY THE CHILD'S HEIGHT/LENGTH MEASUREMENT
merge 1:1 folio intp code_upm est_var using "Antropometria.dta", force keepusing(talla pondef) update 
** talla is the child's height/length
** pondef is the weight for individuals with anthropometry data.

assert _merge <= 3
drop if _merge == 2
drop _merge

************************************
*** RESTRICT ANALYSIS FILE TO CHILDREN 24 MONTHS OLD OR YOUNGER
************************************
keep if edad_meses <= 24

save "Analysis.dta", replace
desc

***********************************************************
***********************************************************
*** CONSTRUCT VARIABLES NEEDED FOR ANALYSIS
***********************************************************
***********************************************************

************************************
*** STUNTING - DEPENDENT VARIABLE
************************************

*** GET INFO FOR BOYS
insheet using "WHOStandards_Boys.csv", comma clear names
keep month sd p50
gen sexo = 1

save "WHO Standards.dta", replace

*** GET INFO FOR GIRLS
insheet using "WHOStandards_Girls.csv", comma clear names
keep month sd p50
gen sexo = 2

*** COMBINE INFO FOR BOYS AND GIRLS
append using "WHO Standards.dta"

*** CALCULATE 2 STANDARD DEVIATIONS UNDER MEAN
gen who_2sd = p50 - 2*sd
drop sd
save "WHO Standards.dta", replace

*** MERGE ANALYSIS DATASET WITH WHO STANDARDS
use "Analysis.dta", clear
rename edad_meses month
merge m:1 month sexo using "WHO Standards.dta"
drop _merge 

*** IDENTIFY INFANTS WITH STUNTING
gen stunting = 1 if talla <= who_2sd
replace stunting = 0 if talla > who_2sd & talla < 222.2

************************************
*** FEMALE DUMMY
************************************
codebook sexo
gen female_dummy = sexo - 1

************************************
*** URBAN HOUSEHOLD DUMMY
************************************
codebook area
gen urban_dummy = area - 1

************************************
*** INDIGENOUS MOTHER DUMMY
************************************
codebook h212 
recode h212 (2 = 0), gen(indigenous_dummy)

************************************
*** INCOME DECILE 
************************************
codebook nse10f
gen incomedec = nse10f - 1

label define incomedec_lbl 0 "1st Decile" 1 "2nd Decile" 2 "3rd Decile" 3 "4th Decile"  ///
	4 "5th Decile" 5 "6th Decile" 6 "7th Decile" 7 "8th Decile" 8 "9th Decile" 9 "10th Decile"
label values incomedec incomedec_lbl

************************************
*** MOTHER'S EDUCATION 
************************************
codebook h218a

*** SOME ELEMENTARY SCHOOL DUMMY
gen es_dummy = 0
replace es_dummy = 1 if h218a == 2

*** SOME MIDDLE SCHOOL DUMMY
gen ms_dummy = 0
replace ms_dummy = 1 if h218a == 3 | h218a == 6

*** SOME HIGH SCHOOL SCHOOL DUMMY
gen hs_dummy = 0
replace hs_dummy = 1 if h218a == 4 | h218a == 7

*** SOME COLLEGE DUMMY
gen college_dummy = 0
replace college_dummy = 1 if h218a == 5 | (h218a > 8 & h218a < .)

************************************
*** CHILD IS RECEIVING ADEQUATE BREASTFEEDING 
************************************
codebook lac_adec
gen breastfed_adeq_dummy = lac_adec

************************************
*** CHILD WAS BREASTFED DURING FIRST HOURS OF BIRTH DUMMY
************************************
codebook inicio_t
gen breastfed_initial_dummy = inicio_t

***********************************************************
***********************************************************
*** REGRESSIONS
***********************************************************
***********************************************************
svyset[pw=pondef], psu(code_upm) strata(est_var) singleunit(centered)
*** use weight from the anthropometry sample (pondef)

************************************
*** MODEL #1
************************************
svy: logit stunting month female_dummy urban_dummy indigenous_dummy incomedec es_dummy ms_dummy ///
	hs_dummy college_dummy  

************************************
*** MODEL #2
************************************
svy: logit stunting month female_dummy urban_dummy indigenous_dummy incomedec es_dummy ms_dummy ///
	hs_dummy college_dummy breastfed_adeq_dummy breastfed_initial_dummy ///
	c.incomedec#breastfed_adeq_dummy

*** CALCLULATE ODD RATIOS
svy: logistic stunting month female_dummy urban_dummy indigenous_dummy incomedec es_dummy ms_dummy ///
	hs_dummy college_dummy breastfed_adeq_dummy breastfed_initial_dummy ///
	c.incomedec#breastfed_adeq_dummy
	
log close
