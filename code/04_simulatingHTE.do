
*	Nov 2017, Alternative Simulation
*	Simulating heterogeneous treatment effects
********************************************************************************

	clear
	clear mata
	clear matrix
	set more off
	set mem 650m
	set maxvar 32767

	cd		"$EXaM"
	global value_time_25=371/260/8/60*(0.25)
	
	use "$EXaM_temp/02_hhid_covariate.dta", clear
	rename ba_tc_hygiene_know_base ba_tc_dia
	rename ba_tc_latrine_density_base ba_tc_lat
	rename ba_tc_momeduc_base ba_tc_edu
	
	*	Group households by heterogeneity-driving covariates
	*	because households that share the same covariate values should be assigned the same treatment
	foreach x in dia lat edu {
		preserve
		egen group_`x' = group(ba_tc_`x'), missing
		collapse (first) ba_tc* (mean) time_value, by(group_`x')
		save "$EXaM_temp/04_cov_group_`x'.dta", replace
		restore
	}

	preserve
		egen group_all = group(ba_tc_lat ba_tc_dia ba_tc_edu), missing
		collapse (first) ba_tc_* (mean) time_value, by(group_all)
		save "$EXaM_temp/04_cov_group_all.dta", replace
	restore

	set seed 9157

foreach x in lat dia edu all {
	preserve

	use "$EXaM_temp/04_cov_group_`x'.dta", clear
	
	* Simulations 1 based on the coefficients of regression "diarrhea on covariates"
	* On hygiene knowledge
	merge 1:1 _n using "$EXaM_temp/04_OLS_diarrhea_on_hygiene_know_base.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen dia_lat_me = ba_tc_lat * betaba_tc_hygiene_know_base
	gen dia_lat_sd = abs(ba_tc_lat * sdba_tc_hygiene_know_base)
	forval i = 1/$simulation {
		gen draw_dia_lat_`i' = betaba_tc + dia_lat_me
	}	
	drop beta* sd* _merge
	
	* On latrine density
	merge 1:1 _n using "$EXaM_temp/04_OLS_diarrhea_on_latrine_density_base.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen dia_dia_me = ba_tc_lat * betaba_tc_latrine_density_base
	gen dia_dia_sd = abs(ba_tc_lat * sdba_tc_latrine_density_base)
	forval i = 1/$simulation {
		gen draw_dia_dia_`i' = betaba_tc + dia_dia_me
	}	
	drop beta* sd* _merge
	
	* On mother education
	merge 1:1 _n using "$EXaM_temp/04_OLS_diarrhea_on_momeduc_base.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen dia_edu_me = ba_tc_edu * betaba_tc_momeduc_base
	gen dia_edu_sd = abs(ba_tc_edu * sdba_tc_momeduc_base)
	forval i = 1/$simulation {
		gen draw_dia_edu_`i' = betaba_tc + dia_edu_me
	}	
	drop beta* sd* _merge

	* On hygiene knowledge, mother education, and latrine density
	merge 1:1 _n using "$EXaM_temp/04_OLS_diarrhea_on_all.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen dia_all_me = ba_tc_lat * betaba_tc_latrine_density_base + ba_tc_dia * betaba_tc_hygiene_know_base + ba_tc_edu * betaba_tc_momeduc_base
	gen dia_all_sd = abs(ba_tc_lat * sdba_tc_latrine_density_base) + abs(ba_tc_dia * sdba_tc_hygiene_know_base) + abs(ba_tc_edu * sdba_tc_momeduc_base)
	forval i = 1/$simulation {
		gen draw_dia_all_`i' = betaba_tc + dia_all_me
	}
	drop beta* sd* _merge
	
	
	* Simulations 2 based on the coefficients of regression "weight on covariates"
	* On hygiene knowledge
	merge 1:1 _n using "$EXaM_temp/04_OLS_weight_on_hygiene_know_base.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen wei_lat_me = ba_tc_lat * betaba_tc_hygiene_know_base
	gen wei_lat_sd = abs(ba_tc_lat * sdba_tc_hygiene_know_base)
	forval i = 1/$simulation {
		gen draw_wei_lat_`i' = betaba_tc + wei_lat_me
	}	
	drop beta* sd* _merge
	
	* On latrine density
	merge 1:1 _n using "$EXaM_temp/04_OLS_weight_on_latrine_density_base.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen wei_dia_me = ba_tc_lat * betaba_tc_latrine_density_base
	gen wei_dia_sd = abs(ba_tc_lat * sdba_tc_latrine_density_base)
	forval i = 1/$simulation {
		gen draw_wei_dia_`i' = betaba_tc + wei_dia_me
	}	
	drop beta* sd* _merge
	
	* On mother education
	merge 1:1 _n using "$EXaM_temp/04_OLS_weight_on_momeduc_base.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen wei_edu_me = ba_tc_edu * betaba_tc_momeduc_base
	gen wei_edu_sd = abs(ba_tc_edu * sdba_tc_momeduc_base)
	forval i = 1/$simulation {
		gen draw_wei_edu_`i' = betaba_tc + wei_edu_me
	}	
	drop beta* sd* _merge

	* On hygiene knowledge, mother education, and latrine density
	merge 1:1 _n using "$EXaM_temp/04_OLS_weight_on_all.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen wei_all_me = ba_tc_lat * betaba_tc_latrine_density_base + ba_tc_dia * betaba_tc_hygiene_know_base + ba_tc_edu * betaba_tc_momeduc_base
	gen wei_all_sd = abs(ba_tc_lat * sdba_tc_latrine_density_base) + abs(ba_tc_dia * sdba_tc_hygiene_know_base) + abs(ba_tc_edu * sdba_tc_momeduc_base)
	forval i = 1/$simulation {
		gen draw_wei_all_`i' = betaba_tc + wei_all_me
	}
	drop beta* sd* _merge	
	

	* Simulations 3 based on the coefficients of regression "bmi on covariates"
	* On hygiene knowledge
	merge 1:1 _n using "$EXaM_temp/04_OLS_bmi_on_hygiene_know_base.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen bmi_lat_me = ba_tc_lat * betaba_tc_hygiene_know_base
	gen bmi_lat_sd = abs(ba_tc_lat * sdba_tc_hygiene_know_base)
	forval i = 1/$simulation {
		gen draw_bmi_lat_`i' = betaba_tc + bmi_lat_me
	}	
	drop beta* sd* _merge
	
	* On latrine density
	merge 1:1 _n using "$EXaM_temp/04_OLS_bmi_on_latrine_density_base.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen bmi_dia_me = ba_tc_lat * betaba_tc_latrine_density_base
	gen bmi_dia_sd = abs(ba_tc_lat * sdba_tc_latrine_density_base)
	forval i = 1/$simulation {
		gen draw_bmi_dia_`i' = betaba_tc + bmi_dia_me
	}	
	drop beta* sd* _merge
	
	* On mother education
	merge 1:1 _n using "$EXaM_temp/04_OLS_bmi_on_momeduc_base.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen bmi_edu_me = ba_tc_edu * betaba_tc_momeduc_base
	gen bmi_edu_sd = abs(ba_tc_edu * sdba_tc_momeduc_base)
	forval i = 1/$simulation {
		gen draw_bmi_edu_`i' = betaba_tc + bmi_edu_me
	}	
	drop beta* sd* _merge

	* On hygiene knowledge, mother education, and latrine density
	merge 1:1 _n using "$EXaM_temp/04_OLS_bmi_on_all.dta", keep(3)
	foreach var of varlist _all {
		replace `var' = `var'[_n-1] if missing(`var') 
	}	
	gen bmi_all_me = ba_tc_lat * betaba_tc_latrine_density_base + ba_tc_dia * betaba_tc_hygiene_know_base + ba_tc_edu * betaba_tc_momeduc_base
	gen bmi_all_sd = abs(ba_tc_lat * sdba_tc_latrine_density_base) + abs(ba_tc_dia * sdba_tc_hygiene_know_base) + abs(ba_tc_edu * sdba_tc_momeduc_base)
	forval i = 1/$simulation {
		gen draw_bmi_all_`i' = betaba_tc + bmi_all_me
	}
	drop beta* sd* _merg
	
	egen sd_dia_`x' = rowsd(draw_dia_`x'_*)
	egen sd_wei_`x' = rowsd(draw_wei_`x'_*)
	egen sd_bmi_`x' = rowsd(draw_bmi_`x'_*)

	keep group_`x' draw_dia_`x'* draw_wei_`x'* draw_bmi_`x'* sd_dia_`x' sd_wei_`x' sd_bmi_`x'
	save "$EXaM_temp/04_HTE_`x'_$simulation.dta", replace
	
	restore
}



*	Simulations must respect covariates. Households with same covariates must get the same treatment assignment and simulated values
foreach x in lat dia edu {
		preserve
		use "$EXaM_temp/02_hhid_covariate.dta", clear
		rename ba_tc_hygiene_know_base ba_tc_dia
		rename ba_tc_latrine_density_base ba_tc_lat
		rename ba_tc_momeduc_base ba_tc_edu
		egen group_`x' = group(ba_tc_`x'), missing
		merge m:1 group_`x' using "$EXaM_temp/04_HTE_`x'_$simulation.dta", nogen
		keep hh_id draw_dia_`x'* draw_wei_`x'* draw_bmi_`x'* sd_dia_`x' sd_wei_`x' sd_bmi_`x'
		gsort hh_id
		save "$EXaM_temp/04_HTE_`x'_byhhid_$simulation.dta", replace
		restore
	}

		preserve
		use "$EXaM_temp/02_hhid_covariate.dta", clear
		rename ba_tc_hygiene_know_base ba_tc_dia
		rename ba_tc_latrine_density_base ba_tc_lat
		rename ba_tc_momeduc_base ba_tc_edu
		egen group_all = group(ba_tc_lat ba_tc_dia ba_tc_edu), missing
		merge m:1 group_all using "$EXaM_temp/04_HTE_all_$simulation.dta", nogen
		keep hh_id draw_dia_all* draw_wei_all* draw_bmi_all* sd_dia_all sd_wei_all sd_bmi_all
		gsort hh_id
		save "$EXaM_temp/04_HTE_all_byhhid_$simulation.dta", replace
		restore

		
		
*	Export to CSV as the inputs for EXaM algorithm
foreach x in lat dia edu all {
	foreach y in dia wei bmi {
		forval n = 1/$simulation {
		preserve
			use "$EXaM_temp/04_HTE_`x'_byhhid_$simulation.dta", clear
			keep hh_id draw_`y'_`x'_`n'
			rename hh_id subject
			rename draw_`y'_`x'_`n' PTE
			outsheet using "$EXaM_input/WTP_HTE_forPythonEXaMalgorithm/PTE_`y'_`x'_`n'.csv", delim(",") replace
		restore
		}
}
}


*	Export to CSV as the inputs for EXaM algorithm - COARSER VERSION WITH 4 CATEGORIES
foreach x in lat dia edu all {
	use "$EXaM_temp/04_HTE_`x'_byhhid_$simulation.dta", clear
foreach y in dia wei bmi {
forval n = 1/$simulation {
	egen group_`y'_`x'_`n' = cut(draw_`y'_`x'_`n'), group(4)
	bys group_`y'_`x'_`n': egen med_`y'_`x'_`n' = median(draw_`y'_`x'_`n')
	replace draw_`y'_`x'_`n' = med_`y'_`x'_`n'
	keep hh_id draw_* sd_dia_* sd_wei_* sd_bmi_*
	gsort hh_id
		preserve
			keep hh_id draw_`y'_`x'_`n'
			rename hh_id subject
			rename draw_`y'_`x'_`n' PTE
			outsheet using "$EXaM_input/WTP_HTE_forPythonEXaMalgorithm/PTE_`y'_`x'_`n'_COARSE.csv", delim(",") replace
		restore		
	}
	}
	save "$EXaM_temp/04_HTE_`x'_byhhid_coarse_$simulation.dta", replace
}
		
		



		
		
*	Reshape for historgram
*	Separate HTE measures because of different labels
	foreach x in lat dia edu all {
		use  "$EXaM_temp/04_HTE_`x'_byhhid_$simulation.dta", clear
			preserve
				keep hh_id draw_dia_`x'_*
				reshape long draw_dia_`x'_, i(hh_id) j(no)
				label variable draw_dia_`x'_ "{bf:TE Measured by % Reduction in Child Diarrhea} (`x')"
				replace draw_dia_`x' = -100* draw_dia_`x'
				sum draw_dia_`x'_
				save "$EXaM_temp/04_HTE_dia_`x'_$simulation.dta", replace
			restore
			preserve
				keep hh_id draw_wei_`x'_*
				reshape long draw_wei_`x'_, i(hh_id) j(no)
				label variable draw_wei_`x'_ "{bf:TE Measured by Change in Child Weight (kg)} (`x')"
				sum draw_wei_`x'_
				save "$EXaM_temp/04_HTE_wei_`x'_$simulation.dta", replace
			restore
			preserve
				keep hh_id draw_bmi_`x'_*
				reshape long draw_bmi_`x'_, i(hh_id) j(no)
				label variable draw_bmi_`x'_ "{bf:TE Measured by Change in Child BMI (kg/m{superscript:2})} (`x')"
				sum draw_bmi_`x'_
				save "$EXaM_temp/04_HTE_bmi_`x'_$simulation.dta", replace
			restore
	}
	

	
	
*	Draw distribution of HTE histograms
********************************************************************************
	foreach x in lat dia edu all {
		foreach y in dia wei bmi {
			preserve
			use "$EXaM_temp/04_HTE_`y'_`x'_$simulation.dta", clear
				local a: variable label draw_`y'_`x'_
				local a: subinstr local a " (`x')" ""
				label var draw_`y'_`x'_ "`a'" 
			histogram draw_`y'_`x'_, scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle( , size(medium) color(gs0)) ytitle("{bf:Fraction}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs10) lwidth(thin) lcolor(gs0) frac xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) 
			graph export "$EXaM_output/04_HTE_`y'_`x'_$simulation.png", replace
			graph save "$EXaM_output/04_HTE_`y'_`x'_$simulation", replace
			restore
		}
	}
	



	
	
