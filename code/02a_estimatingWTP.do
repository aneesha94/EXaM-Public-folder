
*	Preparing for willingness-to-pay simulations
********************************************************************************
*	01. Import mixed logit input text file and merge with household characteristics
cd		"$EXaM"
import delimited using "$EXaM_temp/mixed_logit_data_spec_1_copy.txt", varnames(1) clear
save	"$EXaM_temp/02_mixed_logit_input.dta", replace

*	Input file for mixed logit Matlab code (Aug 17, 2018)
preserve
	use		"$EXaM_input/reg_data_children_Aug2010.dta", clear
	keep	hh_id ba_tc_boy ba_tc_hygiene_know_base ba_tc_latrine_density_base ba_tc_boilh2oyester_base ba_tc_numkids_base ba_tc_momeduc_base ba_tc_iron_roof_density_base
	collapse (mean) ba_tc_*, by(hh_id)
	drop if hh_id == .
	duplicates drop
	save	"$EXaM_temp/02_mixed_logit_tmp.dta", replace
restore

preserve
	use		"$EXaM_temp/02_mixed_logit_input.dta", clear
	merge m:1 hh_id using "$EXaM_temp/02_mixed_logit_tmp.dta"
*	Good: nothing from master did not match.
*	Remains unresolved why Kremer limits choice situations to a subset of households.
*	Mixed logit sample is unrelated to waves of survey or data EXaM-selected covariate availability
	drop if _m == 2
	drop _m
	foreach var of varlist ba_tc* {
		replace `var' = 0 if treatment_status == 0
		replace `var' = 0 if `var' == .
	}
	export delimited using "$EXaM_input/02_mixed_logit_data_spec_7_modified_2.txt", novar replace
restore

*	File used for further Stata codes (Aug 17, 2018)
preserve
	use		"$EXaM_input/reg_data_households_Aug2010.dta", clear
	keep	hh_id latrine_density_base hygiene_know_base momeduc_base
	rename latrine_density_base ba_tc_latrine_density_base
	rename hygiene_know_base ba_tc_hygiene_know_base
	rename momeduc_base ba_tc_momeduc_base
	collapse (mean) ba_tc_*, by(hh_id)
	drop if hh_id == .
	duplicates drop
	save	"$EXaM_temp/02_mixed_logit_temp.dta", replace
restore

*	02. Get time valuation + heterogeneity-driving covariates for each household
preserve
	use		"$EXaM_input/prefs-time_Aug2010.dta", clear
	keep	hh_id time_value
	save	"$EXaM_temp/02_hhid_timevalue.dta", replace
restore

use		"$EXaM_temp/02_mixed_logit_temp.dta", clear
foreach var of varlist ba_tc* {
	replace `var' = 0 if `var' == .
}
merge 1:1 hh_id using "$EXaM_temp/02_hhid_timevalue.dta"
egen	time_value_temp = mean(time_value)
replace time_value = time_value_temp if time_value==.
drop	time_value_temp _m
save	"$EXaM_temp/02_hhid_covariate.dta", replace
