
*	Nov 2017, Alternative Simulation
*	Simulating willingness-to-pay
********************************************************************************

	clear
	clear mata
	clear matrix
	set more off
	set mem 650m
	set maxvar 32767

	cd		"$EXaM"
	global value_time_25=371/260/8/60*(0.25)
	global correlation=0.38
	global mutiplier_1=32*52/(60*8)
	global mutiplier_2=32*52
	
	set seed 9157
	*	Extract regression output 02_mlogitreg_wtp.xlsx
	*   Column 1 results
	insheet using "$EXaM/code/02b_estimatingWTP/02_col1.csv", clear
    summarize estimations if column1=="Spring protection treatment indicator Mean", meanonly
	global Mean_column_1=r(max)

	summarize estimations if column1=="Distance to source, minutes walk Mean", meanonly
	global Mean_l_column_1=r(max)
	
	summarize estimations if column1=="Spring protection treatment indicator S.D.", meanonly
	global Sd_column_1=r(max)
	
	summarize estimations if column1=="Distance to source, minutes walk S.D.", meanonly
	global Sd_l_column_1=r(max)
	
	summarize estimations if column1=="Treatment * latrine density", meanonly
	global b_treatment_la_1=r(max)
	
	summarize estimations if column1=="Spring protection treatment indicator Mean(sd)", meanonly
	global sd_mean_column_1=r(max)
	
	summarize estimations if column1=="Spring protection treatment indicator S.D.(sd)", meanonly
	global 	sd_sd_column_1=r(max)
	
	*   Column 2 results
	insheet using "$EXaM/code/02b_estimatingWTP/02_col2.csv", clear
    summarize estimations if column2=="Spring protection treatment indicator Mean", meanonly
	global Mean_column_2=r(max)

	summarize estimations if column2=="Distance to source, minutes walk Mean", meanonly
	global Mean_l_column_2=r(max)
	
	summarize estimations if column2=="Spring protection treatment indicator S.D.", meanonly
	global Sd_column_2=r(max)
	
	summarize estimations if column2=="Distance to source, minutes walk S.D.", meanonly
	global Sd_l_column_2=r(max)
	
	summarize estimations if column2=="Treatment * diarrhea prevention", meanonly
	global b_treatment_dia_2=r(max)
	
	summarize estimations if column2=="Spring protection treatment indicator Mean(sd)", meanonly
	global sd_mean_column_2=r(max)
	
	summarize estimations if column2=="Spring protection treatment indicator S.D.(sd)", meanonly
	global 	sd_sd_column_2=r(max)
	
	*   Column 3 results
	insheet using "$EXaM/code/02b_estimatingWTP/02_col3.csv", clear
    summarize estimations if column3=="Spring protection treatment indicator Mean", meanonly
	global Mean_column_3=r(max)

	summarize estimations if column3=="Distance to source, minutes walk Mean", meanonly
	global Mean_l_column_3=r(max)
	
	summarize estimations if column3=="Spring protection treatment indicator S.D.", meanonly
	global Sd_column_3=r(max)
	
	summarize estimations if column3=="Distance to source, minutes walk S.D.", meanonly
	global Sd_l_column_3=r(max)
	
	summarize estimations if column3=="Treatment * mother education", meanonly
	global b_treatment_mome_3=r(max)
	
	summarize estimations if column3=="Spring protection treatment indicator Mean(sd)", meanonly
	global sd_mean_column_3=r(max)
	
	summarize estimations if column3=="Spring protection treatment indicator S.D.(sd)", meanonly
	global 	sd_sd_column_3=r(max)
	
	*   Column 4 results
	insheet using "$EXaM/code/02b_estimatingWTP/02_col4.csv", clear
    summarize estimations if column4=="Spring protection treatment indicator Mean", meanonly
	global Mean_column_4=r(max)

	summarize estimations if column4=="Distance to source, minutes walk Mean", meanonly
	global Mean_l_column_4=r(max)
	
	summarize estimations if column4=="Spring protection treatment indicator S.D.", meanonly
	global Sd_column_4=r(max)
	
	summarize estimations if column4=="Distance to source, minutes walk S.D.", meanonly
	global Sd_l_column_4=r(max)
	
	summarize estimations if column4=="Treatment * latrine density", meanonly
	global b_treatment_la_4=r(max)
	
	summarize estimations if column4=="Treatment * diarrhea prevention", meanonly
	global b_treatment_dia_4=r(max)
	
	summarize estimations if column4=="Treatment * mother education", meanonly
	global b_treatment_mome_4=r(max)
	
	summarize estimations if column4=="Spring protection treatment indicator Mean(sd)", meanonly
	global sd_mean_column_4=r(max)
	
	summarize estimations if column4=="Spring protection treatment indicator S.D.(sd)", meanonly
	global 	sd_sd_column_4=r(max)

*	----------------------------------------------------------------------------	*
*	Empirical part officially starts here											*
*	----------------------------------------------------------------------------	*
	
	use "$EXaM_temp/02_hhid_covariate.dta", clear
	rename ba_tc_hygiene_know_base ba_tc_dia
	rename ba_tc_latrine_density_base ba_tc_lat
	rename ba_tc_momeduc_base ba_tc_edu
	
	*	Group households by heterogeneity-driving covariates
	*	because households that share the same covariate values should be assigned the same treatment
	foreach x in dia lat edu {
		preserve
		egen group_`x' = group(ba_tc_`x'), missing
		collapse (first) ba_tc_`x' (mean) time_value, by(group_`x')
		gen lat_me = ba_tc_`x' * $b_treatment_la_1
		gen dia_me = ba_tc_`x' * $b_treatment_dia_2
		gen edu_me = ba_tc_`x' * $b_treatment_mome_3
		gen all_me = ba_tc_`x' * 0
		save "$EXaM_temp/03_cov_group_`x'.dta", replace
		restore
	}

	preserve
		egen group_all = group(ba_tc_lat ba_tc_dia ba_tc_edu), missing
		collapse (first) ba_tc_* (mean) time_value, by(group_all)
		gen lat_me = ba_tc_lat * $b_treatment_la_1
		gen dia_me = ba_tc_dia * $b_treatment_dia_2
		gen edu_me = ba_tc_edu * $b_treatment_mome_3
		gen all_me = ba_tc_lat * $b_treatment_la_4 + ba_tc_dia * $b_treatment_dia_4 + ba_tc_edu * $b_treatment_mome_4
		save "$EXaM_temp/03_cov_group_all.dta", replace
	restore

	
*	Generate triangular distribution from uniform: Uniform ~ U[0,1) -> Triangular ~ F(c)
*	X = a + sqrt(U * (b-a) * (c-a)) 	for 0 < U < F(c)	 *
*	X = b - sqrt((1-U) * (b-a) * (b-c)) for F(c) <= U < 1	 *

	foreach x in lat dia edu all {
		preserve
			use "$EXaM_temp/03_cov_group_`x'.dta", clear
			foreach z in lat dia edu all {
				foreach y in mu {
					forval i = 1/$simulation {
						gen UN_`y'_`z'_`i' = runiform()
					}
				}
			}
		save "$EXaM_temp/03_cov_group_`x'_DI_$simulation.dta", replace
		restore
	}

	foreach x in lat dia edu all {
		preserve
		use "$EXaM_temp/03_cov_group_`x'_DI_$simulation.dta", clear
			forval i = 1/$simulation {
				* Starting from Covariate in column (1) Baseline Latrine Density (lat)
				gen mu_tr_lat_`i' = $Mean_column_1 + lat_me
				gen sd_tr_lat_`i' = $Sd_column_1
				gen mu_di_lat_`i' = 2*$Mean_l_column_1 + sqrt(UN_mu_lat_`i' * 2 * $Sd_l_column_1 * $Sd_l_column_1 ) if UN_mu_lat_`i' > 0 & UN_mu_lat_`i' < 0.5
				replace mu_di_lat_`i' = -1 * sqrt((1 - UN_mu_lat_`i') * 2 * $Sd_l_column_1 * $Sd_l_column_1 ) if mu_di_lat_`i' == .

				* Column (2) Diarrhea prevention score (dia)
				gen mu_tr_dia_`i' = $Mean_column_2 + dia_me
				gen sd_tr_dia_`i' = $Sd_column_2
				gen mu_di_dia_`i' = 2*$Mean_l_column_2 + sqrt(UN_mu_dia_`i' * 2 * $Sd_l_column_2 * $Sd_l_column_2 ) if UN_mu_dia_`i' > 0 & UN_mu_dia_`i' < 0.5
				replace mu_di_dia_`i' = -1 * sqrt((1 - UN_mu_dia_`i') *2*$Sd_l_column_2 * $Sd_l_column_2) if mu_di_dia_`i' == .
		
				* Column (3) Mother's education (edu)
				gen mu_tr_edu_`i' = $Mean_column_3 + edu_me
				gen sd_tr_edu_`i' = $Sd_column_3
				gen mu_di_edu_`i' = 2*$Mean_l_column_3 + sqrt(UN_mu_edu_`i' * 2 * $Sd_l_column_3 * $Sd_l_column_3 ) if UN_mu_edu_`i' > 0 & UN_mu_edu_`i' < 0.5
				replace mu_di_edu_`i' = -1 * sqrt((1 - UN_mu_edu_`i') *2* $Sd_l_column_3 * $Sd_l_column_3 ) if mu_di_edu_`i' == .
				
				* Column (5) All covariate interaction (all)
				gen mu_tr_all_`i' = $Mean_column_4 + all_me
				gen sd_tr_all_`i' = $Sd_column_4
				gen mu_di_all_`i' = 2*$Mean_l_column_4 + sqrt(UN_mu_all_`i' * 2 * $Sd_l_column_4 * $Sd_l_column_4 ) if UN_mu_all_`i' > 0 & UN_mu_all_`i' < 0.5
				replace mu_di_all_`i' = -1 * sqrt((1 - UN_mu_all_`i') * 2 * $Sd_l_column_4 * $Sd_l_column_4 ) if mu_di_all_`i' == .
		
				gen star_mu_tr_`x'_`i' = rnormal(mu_tr_`x'_`i', sd_tr_`x'_`i')
					
				
				gen ratio_`x'_`i' = star_mu_tr_`x'_`i' / ((-1)*mu_di_`x'_`i'/$correlation)
				gen draw_wdays_`x'_`i' = $mutiplier_1 * ratio_`x'_`i'
				gen draw_usdsp_`x'_`i' = $mutiplier_2 * time_value * ratio_`x'_`i'
				gen draw_usd25_`x'_`i' = $mutiplier_2 * $value_time_25 * ratio_`x'_`i'

			}
	
			egen sd_wdays_`x' = rowsd(draw_wdays_`x'_*)
			egen sd_usdsp_`x' = rowsd(draw_usdsp_`x'_*)
			egen sd_usd25_`x' = rowsd(draw_usd25_`x'_*)

			keep group_`x' draw_* sd_wdays_* sd_usd*
			save "$EXaM_temp/03_WTP_`x'_$simulation.dta", replace

		restore	
}	


*	Simulations must respect covariates. Households with same covariates must get the same treatment assignment and simulated values
foreach x in lat dia edu{
		preserve
		use "$EXaM_temp/02_hhid_covariate.dta", clear
		rename ba_tc_hygiene_know_base ba_tc_dia
		rename ba_tc_latrine_density_base ba_tc_lat
		rename ba_tc_momeduc_base ba_tc_edu
		egen group_`x' = group(ba_tc_`x'), missing
		merge m:1 group_`x' using "$EXaM_temp/03_WTP_`x'_$simulation.dta", nogen
		keep hh_id draw_* sd_wdays_* sd_usd*
		gsort hh_id
		save "$EXaM_temp/03_WTP_`x'_byhhid_$simulation.dta", replace
		restore
	}

		preserve
		use "$EXaM_temp/02_hhid_covariate.dta", clear
		rename ba_tc_hygiene_know_base ba_tc_dia
		rename ba_tc_latrine_density_base ba_tc_lat
		rename ba_tc_momeduc_base ba_tc_edu
		egen group_all = group(ba_tc_lat ba_tc_dia ba_tc_edu), missing
		merge m:1 group_all using "$EXaM_temp/03_WTP_all_$simulation.dta", nogen
		keep hh_id draw_* sd_wdays_* sd_usd*
		gsort hh_id
		save "$EXaM_temp/03_WTP_all_byhhid_$simulation.dta", replace
		restore



*	Export to CSV as the inputs for EXaM algorithm
foreach x in lat dia edu all {
foreach y in wdays usdsp usd25 {
forval n = 1/$simulation {
preserve
	use "$EXaM_temp/03_WTP_`x'_byhhid_$simulation.dta", clear
	keep hh_id draw_`y'_`x'_`n'
	rename hh_id subject
	rename draw_`y'_`x'_`n' WTP
	outsheet using "$EXaM_input/WTP_HTE_forPythonEXaMalgorithm/WTP_`y'_`x'_`n'.csv", delim(",") replace
restore
}
}
}

*	Export to CSV as the inputs for EXaM algorithm - COARSER VERSION WITH 4 CATEGORIES
foreach x in lat dia edu all {
	use "$EXaM_temp/03_WTP_`x'_byhhid_$simulation.dta", clear
foreach y in wdays usdsp usd25 {
forval n = 1/$simulation {
	egen group_`y'_`x'_`n' = cut(draw_`y'_`x'_`n'), group(4)
	bys group_`y'_`x'_`n': egen med_`y'_`x'_`n' = median(draw_`y'_`x'_`n')
	replace draw_`y'_`x'_`n' = med_`y'_`x'_`n'
	keep hh_id draw_* sd_wdays_* sd_usd*
	gsort hh_id
		preserve
			keep hh_id draw_`y'_`x'_`n'
			rename hh_id subject
			rename draw_`y'_`x'_`n' WTP
			outsheet using "$EXaM_input/WTP_HTE_forPythonEXaMalgorithm/WTP_`y'_`x'_`n'_COARSE.csv", delim(",") replace
		restore		
	}
	}
	save "$EXaM_temp/03_WTP_`x'_byhhid_coarse_$simulation.dta", replace
}



*	Reshape for historgram
*	Separate WTP measures because of different labels
	foreach x in lat dia edu all {
		use  "$EXaM_temp/03_WTP_`x'_byhhid_$simulation.dta", clear
			preserve
				keep hh_id draw_wdays_`x'_*
				reshape long draw_wdays_`x'_, i(hh_id) j(no)
				label variable draw_wdays_`x'_ "{bf:WTP Measured by Time Cost of Water Collection} (Unit: Workdays) (`x')"
				sum draw_wdays_`x'_
				save "$EXaM_temp/03_WTP_wdays_`x'_$simulation.dta", replace
			restore
			preserve
				keep hh_id draw_usdsp_`x'_*
				reshape long draw_usdsp_`x'_, i(hh_id) j(no)
				label variable draw_usdsp_`x'_ "{bf:WTP Measured by Time Cost of Water Collection} (Unit: USD of Time Valuation) (`x')"
				sum draw_usdsp_`x'_
				save "$EXaM_temp/03_WTP_usdsp_`x'_$simulation.dta", replace
			restore
			preserve
				keep hh_id draw_usd25_`x'_*
				reshape long draw_usd25_`x'_, i(hh_id) j(no)
				label variable draw_usd25_`x'_ "{bf:WTP Measured by Time Cost of Water Collection} (unit: USD of Time Valuation Assuming 25% Average Kenyan Wage) (`x')"
				sum draw_usd25_`x'_
				save "$EXaM_temp/03_WTP_usd25_`x'_$simulation.dta", replace
			restore
		}


	
*	Draw distribution of WTP histograms
********************************************************************************
	foreach x in lat dia edu all {
		foreach y in wdays usdsp usd25 {
			use "$EXaM_temp/03_WTP_`y'_`x'_$simulation.dta", clear
				local a: variable label draw_`y'_`x'_
				local a: subinstr local a " (`x')" ""
				label var draw_`y'_`x'_ "`a'" 
			egen mean_`y'_`x' = mean(draw_`y'_`x')
			egen sd_`y'_`x' = sd(draw_`y'_`x')
			gen lb2sd = mean_`y'_`x' - 2 * sd_`y'_`x'
			gen ub2sd = mean_`y'_`x' + 2 * sd_`y'_`x'
			gen lb3sd = mean_`y'_`x' - 3 * sd_`y'_`x'
			gen ub3sd = mean_`y'_`x' + 3 * sd_`y'_`x'
			sum draw_`y'_`x'
			
			* Exclude 5% both tails
			histogram draw_`y'_`x'_ if draw_`y'_`x'_ >= lb2sd & draw_`y'_`x'_ <= ub2sd, scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle( , size(medium) color(gs0)) ytitle("{bf:Fraction}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs8) fextend)  fcolor(gs10) lwidth(thin) lcolor(gs0) frac xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) 
			graph export "$EXaM_output/03_WTP_`y'_`x'_2SD_$simulation.png", replace
			graph save "$EXaM_output/03_WTP_`y'_`x'_2SD_$simulation", replace
			
			* Exclude 0.3% both tails
			histogram draw_`y'_`x'_ if draw_`y'_`x'_ >= lb3sd & draw_`y'_`x'_ <= ub3sd, scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle( , size(medium) color(gs0)) ytitle("{bf:Fraction}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs8) fextend)  fcolor(gs10) lwidth(thin) lcolor(gs0) frac xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) 
			graph export "$EXaM_output/03_WTP_`y'_`x'_$simulation.png", replace
			graph save "$EXaM_output/03_WTP_`y'_`x'_$simulation", replace
		}
	}

	
	
	
