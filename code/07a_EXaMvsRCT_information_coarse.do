 
*	Graphing EXaM vs RCT comparison in terms of information, based on HTE estimates (coarse)
********************************************************************************

	clear
	clear mata
	clear matrix
	set more off
	set mem 650m
	set maxvar 32767

	cd		"$EXaM"
	use		"$EXaM_temp/06_p_real_coarse_$simulation.dta", clear
	set seed 463

//	1b. Assign treatment based on EXaM treatment probability
//  (Random permutation)
					
	forval p = 1/$simulation {	
		
		gen treat_EXaM_`p' = .
		replace treat_EXaM_`p' = runiform() <= p_real_`p' ///
			if treat_EXaM_`p' == . 					//  Assign treatment based on EXaM probability
		
		gen cap1_`p' = sum(treat_EXaM_`p')			//  Treatment capacity counter, sup(cap1) = 663 cf. Kremer et al
		gen cap0_`p' = (treat_EXaM_`p' == 0)		//  Control capacity counter, max(cap0) = 1540 - 663 = 877
		replace cap0_`p' = sum(cap0_`p')
		
		*	Treatment and control assignments must respect treatment and control capacity counters
		egen maxcap1_`p' = max(cap1_`p')			
		egen maxcap0_`p' = max(cap0_`p')
		
		if maxcap0_`p' > 877 {
			replace treat_EXaM_`p' = 1	if cap0_`p' > 877
		}
		
		else if maxcap1_`p' > 663 {
			replace treat_EXaM_`p' = 0	if cap1_`p' > 663
		}
		
		drop cap1_`p' cap0_`p' maxcap1_`p' maxcap0_`p'
	}
	
//	1b. Assign treatment based on RCT treatment probability
					
	forval p = 1/$simulation {	
		
		gen treat_RCT_`p' = .
		replace treat_RCT_`p' = runiform() <= 663/1540 ///
			if treat_RCT_`p' == . 					//  Assign treatment based on EXaM probability
		
		gen cap1_`p' = sum(treat_RCT_`p')			//  Treatment capacity counter, sup(cap1) = 663 cf. Kremer et al
		gen cap0_`p' = (treat_RCT_`p' == 0)		//  Control capacity counter, max(cap0) = 1540 - 663 = 877
		replace cap0_`p' = sum(cap0_`p')
		
		*	Treatment and control assignments must respect treatment and control capacity counters
		egen maxcap1_`p' = max(cap1_`p')			
		egen maxcap0_`p' = max(cap0_`p')
		
		if maxcap0_`p' > 877 {
			replace treat_RCT_`p' = 1	if cap0_`p' > 877
		}
		
		else if maxcap1_`p' > 663 {
			replace treat_RCT_`p' = 0	if cap1_`p' > 663
		}
		
		drop cap1_`p' cap0_`p' maxcap1_`p' maxcap0_`p'
	}
	
	
	
gsort subject
	
	
//	1c. Merge with Treatment effects and FE datasets
preserve
	use "$EXaM_temp/04_HTE_all_byhhid_coarse_$simulation.dta", clear
	merge 1:1 hh_id using "$EXaM_temp/07_dia_all_hhFE.dta", nogen
	replace dia_hh_fe = 0 if dia_hh_fe == .
	rename hh_id subject
	replace subject = _n - 1
	keep subject draw_dia_all* dia_hh_fe
	save "$EXaM_temp/07_HTE_dia_all_byhhid_coarse_$simulation.dta", replace
restore
merge 1:1 subject using "$EXaM_temp/07_HTE_dia_all_byhhid_coarse_$simulation.dta", nogen
save "$EXaM_temp/07_EXaMvsRCT_information_coarse_$simulation.dta", replace



//	1d. Regress



*	Regression of Child Diarrhea based on Kremer et al (Original)

*reg c14_d_child_diarrhea ba_tc ba_tc_latrine_density_base ba_tc_hygiene_know_base ba_tc_mom
*> educ_base ba_tc_boy i.hh_id i.before_after if $child_sample, vce(cluster spring_id)
use "$EXaM_input/07_dia_all_hhFE.dta",clear

sum estimate if parm=="1.before_after"
global ba_1 = r(mean)

sum estimate if parm=="2.before_after"
global ba_2 = r(mean)

sum estimate if parm=="3.before_after"
global ba_3 = r(mean)

sum estimate if parm=="_cons"
global con = r(mean)



*	============================================================================
*	NON-ROBUST regression (Robust version starts on line 282)
********************************************************************************

use "$EXaM_temp/07_EXaMvsRCT_information_coarse_$simulation.dta", clear
gen p_RCT = 663/1540
forval n = 1/$simulation {
	gen yi_EXaM_`n' = draw_dia_all_`n' * treat_EXaM_`n' + dia_hh_fe + $con + ($ba_1 - $ba_2 - $ba_3 ) /3
	gen yi_RCT_`n' = draw_dia_all_`n' * treat_RCT_`n' + dia_hh_fe + $con + ($ba_1 - $ba_2 - $ba_3 )/3
	
	*	EXaM
	reg yi_EXaM_`n' treat_EXaM_`n' p_real_`n'
	global degreef = e(df_r)
	gen b_EXaM_`n' = _b[treat_EXaM_`n']
	gen se_EXaM_`n' = _se[treat_EXaM_`n']
	gen p_EXaM_`n' = 2*ttail($degreef, abs( _b[treat_EXaM_`n']/_se[treat_EXaM_`n']))

	*	Prepare for AAIW ExaM
	preserve 
		keep yi_EXaM_`n' treat_EXaM_`n' p_real_`n'
		export excel "$EXaM_temp/07_dataexam_AAIW_coarse_`n'.xlsx", replace
	restore
	
	*	RCT
	reg yi_RCT_`n' treat_RCT_`n'
	gen b_RCT_`n' = _b[treat_RCT_`n']
	gen se_RCT_`n' = _se[treat_RCT_`n']
	gen p_RCT_`n' = 2*ttail(e(df_r),abs( _b[treat_RCT_`n']/_se[treat_RCT_`n']))
	
	*	Prepare for AAIW RCT
	preserve 
		keep yi_RCT_`n' treat_RCT_`n' p_RCT
		export excel "$EXaM_temp/07_datarct_AAIW_coarse_`n'.xlsx", replace
	restore
}

preserve
	keep b_EXaM* p_EXaM* b_RCT* p_RCT* se_EXaM* se_RCT*
	keep if _n == 1
	gen n = _n
	reshape long b_EXaM_ p_EXaM_ b_RCT_ p_RCT_ se_EXaM_ se_RCT_, i(n) j(number)
	drop p_RCT
	save "$EXaM_temp/07_EXaMvsRCT_information_histogram_coarse_$simulation.dta", replace
restore

	use "$EXaM_temp/07_EXaMvsRCT_information_histogram_coarse_$simulation.dta", clear
	egen sd_b_RCT_ = sd(b_RCT)
	egen sd_b_EXaM_ = sd(b_EXaM)
	gen p_b_RCT_ = 2*ttail($degreef, abs(b_RCT_ / sd_b_RCT_))
	gen p_b_EXaM_ = 2*ttail($degreef, abs(b_EXaM_ / sd_b_EXaM_))
	
	foreach var of varlist b_EXaM_ b_RCT_ p_EXaM_ p_RCT_ se_EXaM_ se_RCT_ p_b_RCT_ p_b_EXaM_ {
			sum `var', det
			global mean_`var' = round(r(mean),.0001)
			global medi_`var' = round(r(p50),.0001)
		}

*	b ESTIMATES			
	twoway (histogram b_RCT_, frac scheme(s1manual) width(0.0015) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:ATE Estimates (b Based on Regression)}", size(medium)) xscale(range(-0.15 0.05)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram b_EXaM_, xline($medi_b_EXaM_, lc(red) lpattern(solid)) width(0.0015) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
	graph export "$EXaM_output/07_EXaMvsRCT_information_b_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_b_coarse_medi_$simulation", replace
	
	twoway (histogram b_RCT_, frac scheme(s1manual) width(0.0015) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:ATE Estimates (b Based on Regression)}", size(medium)) xscale(range(-0.15 0.05)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram b_EXaM_, width(0.0015) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_RCT_information_b_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_b_coarse_medi_$simulation", replace
	
	twoway (histogram b_RCT_, frac scheme(s1manual) width(0.0015) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:ATE Estimates (b Based on Regression)}", size(medium)) xscale(range(-0.15 0.05)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram b_EXaM_, xline($mean_b_EXaM_, lc(red) lpattern(solid)) width(0.0015) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
	graph export "$EXaM_output/07_EXaMvsRCT_information_b_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_b_coarse_mean_$simulation", replace

	twoway (histogram b_RCT_, frac scheme(s1manual) width(0.0015) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:ATE Estimates (b Based on Regression)}", size(medium)) xscale(range(-0.15 0.05)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram b_EXaM_, width(0.0015) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_RCT_information_b_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_b_coarse_mean_$simulation", replace
	
*	STANDARD ERRORS OF b ESTIMATES
	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Non-robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_se_EXaM_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))) //////
	(histogram se_EXaM_, width(0.0001) frac fcolor(none) lcolor(red) lwidth(vthin) ///
		xline($medi_se_EXaM_, lc(red) lpattern(solid))), ///
		legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_EXaMvsRCT_information_se_b_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_se_b_coarse_medi_$simulation", replace
	
	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Non-robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram se_EXaM_, width(0.0001) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_RCT_information_se_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_se_coarse_medi_$simulation", replace

	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Non-robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram se_EXaM_, width(0.0001) frac fcolor(none) lcolor(red) lwidth(vthin) ///
		xline($mean_se_EXaM_, lc(red) lpattern(solid)) ///
			xmlabel($mean_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_se_EXaM_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))), ///
		legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_EXaMvsRCT_information_se_b_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_se_b_coarse_mean_$simulation", replace
	
	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Non-robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram se_EXaM_, width(0.0001) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
	
	graph export "$EXaM_output/07_RCT_information_se_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_se_coarse_mean_$simulation", replace
	
*	P-VALUES OF b ESTIMATES
	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (Non-robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(range(0 1) lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#10, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_p_EXaM_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))		) ///
	(histogram p_EXaM_, width(0.05) frac fcolor(none) lcolor(red) lwidth(vthin) ///
		xline($medi_p_EXaM_, lc(red) lpattern(solid))), ///
		legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
	graph export "$EXaM_output/07_EXaMvsRCT_information_p005_b_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_p005_b_coarse_medi_$simulation", replace

	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (Non-robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram p_EXaM_, width(0.05) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/07_RCT_information_p005_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_p005_coarse_medi_$simulation", replace

	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (Non-robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram p_EXaM_, width(0.05) frac fcolor(none) lcolor(red) lwidth(vthin) ///
		xline($mean_p_EXaM_, lc(red) lpattern(solid)) ///
			xmlabel($mean_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_p_EXaM_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))), ///
		legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/07_EXaMvsRCT_information_p005_b_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_p005_b_coarse_mean_$simulation", replace
	
	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (Non-robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram p_EXaM_, width(0.05) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/07_RCT_information_p005_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_p005_coarse_mean_$simulation", replace

*	P-VALUES OF b ESTIMATES (Use standard deviation of b as SE, 20171213)
	twoway (histogram p_b_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_p_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (b, Exact)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(range(0 1) lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#10, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_p_b_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_p_b_EXaM_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))		) ///
	(histogram p_b_EXaM_, width(0.05) frac fcolor(none) lcolor(red) lwidth(vthin) ///
		xline($medi_p_b_EXaM_, lc(red) lpattern(solid))), ///
		legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
	graph export "$EXaM_output/07_EXaMvsRCT_information_p005SD_b_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_p005SD_b_coarse_medi_$simulation", replace


	twoway (histogram p_b_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_p_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (b, Exact)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(range(0 1) lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#10, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_p_b_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram p_b_EXaM_, width(0.05) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/07_RCT_information_p005SD_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_p005SD_coarse_medi_$simulation", replace
	
	twoway (histogram p_b_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_p_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (b, Exact)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(range(0 1) lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#10, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram p_b_EXaM_, width(0.05) frac fcolor(none) lcolor(red) lwidth(vthin) ///
		xline($mean_p_b_EXaM_, lc(red) lpattern(solid)) ///
			xmlabel($mean_p_b_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_p_b_EXaM_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))), ///
		legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/07_EXaMvsRCT_information_p005SD_b_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_p005SD_b_coarse_mean_$simulation", replace
	
	twoway (histogram p_b_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_p_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (b, Exact)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(range(0 1) lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#10, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_p_b_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram p_b_EXaM_, width(0.05) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/07_RCT_information_p005SD_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_p005SD_coarse_mean_$simulation", replace

		
*	============================================================================
*	ROBUST regression
********************************************************************************

use "$EXaM_temp/07_EXaMvsRCT_information_coarse_$simulation.dta", clear
gen p_RCT = 663/1540
forval n = 1/$simulation {
	gen yi_EXaM_`n' = draw_dia_all_`n' * treat_EXaM_`n' + dia_hh_fe + $con + ($ba_1 - $ba_2 - $ba_3 )/3
	gen yi_RCT_`n' = draw_dia_all_`n' * treat_RCT_`n' + dia_hh_fe + $con + ($ba_1 - $ba_2 - $ba_3 )/3
	
	*	EXaM
	reg yi_EXaM_`n' treat_EXaM_`n' p_real_`n', vce(ro) 
	gen b_EXaM_`n' = _b[treat_EXaM_`n']
	gen se_EXaM_`n' = _se[treat_EXaM_`n']
	gen p_EXaM_`n' = 2*ttail(e(df_r),abs( _b[treat_EXaM_`n']/_se[treat_EXaM_`n']))
	
	*	RCT
	reg yi_RCT_`n' treat_RCT_`n', vce(ro)
	gen b_RCT_`n' = _b[treat_RCT_`n']
	gen se_RCT_`n' = _se[treat_RCT_`n']
	gen p_RCT_`n' = 2*ttail(e(df_r),abs( _b[treat_RCT_`n']/_se[treat_RCT_`n']))
	
}

preserve
	keep b_EXaM* p_EXaM* b_RCT* p_RCT* se_EXaM* se_RCT*
	keep if _n == 1
	gen n = _n
	reshape long b_EXaM_ p_EXaM_ b_RCT_ p_RCT_ se_EXaM_ se_RCT_, i(n) j(number)
	drop p_RCT
	save "$EXaM_temp/07_EXaMvsRCT_information_histogram_robust_coarse_$simulation.dta", replace
restore



	use "$EXaM_temp/07_EXaMvsRCT_information_histogram_robust_coarse_$simulation.dta", clear
	foreach var of varlist b_EXaM_ b_RCT_ p_EXaM_ p_RCT_ se_EXaM_ se_RCT_  {
			sum `var', det
			global mean_`var' = round(r(mean),.0001)
			global medi_`var' = round(r(p50),.0001)
		}

	
*	STANDARD ERRORS OF b ESTIMATES
	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_se_EXaM_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))) ///
	(histogram se_EXaM_, xline($medi_se_EXaM_, lc(red) lpattern(solid)) width(0.0001) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_EXaMvsRCT_information_se_b_robust_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_se_b_robust_coarse_medi_$simulation", replace

	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram se_EXaM_, width(0.0001) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_RCT_information_se_robust_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_se_robust_coarse_medi_$simulation", replace
	
	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($mean_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_se_EXaM_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))) ///
	(histogram se_EXaM_, xline($mean_se_EXaM_, lc(red) lpattern(solid)) width(0.0001) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_EXaMvsRCT_information_se_b_robust_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_se_b_robust_coarse_mean_$simulation", replace
	
	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram se_EXaM_, width(0.0001) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_RCT_information_se_robust_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_se_robust_coarse_mean_$simulation", replace
	
*	P-VALUES OF b ESTIMATES
	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (Robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(range(0 1) lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#10, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_p_EXaM_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))		) ///
	(histogram p_EXaM_, width(0.05) frac fcolor(none) lcolor(red) lwidth(vthin) ///
		xline($medi_p_EXaM_, lc(red) lpattern(solid))), ///
		legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
	graph export "$EXaM_output/07_EXaMvsRCT_information_p005_b_robust_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_p005_b_robust_coarse_medi_$simulation", replace

	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (Robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram p_EXaM_, width(0.05) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
	
	graph export "$EXaM_output/07_RCT_information_p005_robust_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_p005_robust_coarse_medi_$simulation", replace
	
	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (Robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($mean_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_p_EXaM_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))) ///
	(histogram p_EXaM_, xline($mean_p_EXaM_, lc(red) lpattern(solid)) width(0.05) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/07_EXaMvsRCT_information_p005_b_robust_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_p005_b_robust_coarse_mean_$simulation", replace
	
	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (Robust)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram p_EXaM_, width(0.05) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces)
	
	graph export "$EXaM_output/07_RCT_information_p005_robust_coarse_mean_$simulation.png", replace	
	graph save "$EXaM_output/07_RCT_information_p005_robust_coarse_mean_$simulation", replace	
	
*	============================================================================
*	Beta Based on Propensity Score Matching
********************************************************************************

*******non-robust part
use "$EXaM_temp/07_EXaMvsRCT_information_coarse_$simulation.dta", clear
set more off
gen p_RCT = 663/1540
forval n = 1/$simulation {
	gen yi_EXaM_`n' = draw_dia_all_`n' * treat_EXaM_`n' + dia_hh_fe + $con + ($ba_1 - $ba_2 - $ba_3 )/3
	gen yi_RCT_`n' = draw_dia_all_`n' * treat_RCT_`n' + dia_hh_fe + $con + ($ba_1 - $ba_2 - $ba_3 )/3
	
	*	EXaM
	gen b_EXaM_`n'=0
	gen se_EXaM_`n'=0

	egen group_`n'=group(p_real_`n')
    	su group_`n', det
	
	forvalues s = 1/`r(max)'{
	
	su group_`n' if group_`n'==`s', det
	scalar r=r(N)
	if r >5{
	reg yi_EXaM_`n' treat_EXaM_`n' if group_`n'==`s' 
	
	gen b_EXaM_`n'_`s' =_b[treat_EXaM_`n']
	gen se_EXaM_`n'_`s' = _se[treat_EXaM_`n']

	
	replace b_EXaM_`n'=b_EXaM_`n'+b_EXaM_`n'_`s'*e(N)/1540
	replace se_EXaM_`n'=se_EXaM_`n'+se_EXaM_`n'_`s'*e(N)/1540

	
	}
	else if r<=5{
	gen b_EXaM_`n'_`s'=0
	replace b_EXaM_`n'=b_EXaM_`n'+b_EXaM_`n'_`s'*0
	gen se_EXaM_`n'_`s'=0
	replace se_EXaM_`n'=se_EXaM_`n'+se_EXaM_`n'_`s'*0

	}
	drop b_EXaM_`n'_`s' 
	drop se_EXaM_`n'_`s'
	scalar drop r
	}
	gen p_EXaM_`n' = 2*ttail(e(df_r),abs( b_EXaM_`n' /se_EXaM_`n'))

	drop group_`n' 
	
	*	RCT
	reg yi_RCT_`n' treat_RCT_`n'
	gen b_RCT_`n' = _b[treat_RCT_`n']
	gen p_RCT_`n' = 2*ttail(e(df_r),abs( _b[treat_RCT_`n']/_se[treat_RCT_`n']))
    gen se_RCT_`n'=_se[treat_RCT_`n']
}


preserve
	keep b_EXaM* b_RCT* p_EXaM* p_RCT* se_EXaM* se_RCT*
	keep if _n == 1
	gen n = _n
	reshape long b_EXaM_ b_RCT_ p_EXaM_ p_RCT_ se_EXaM_ se_RCT_, i(n) j(number)
	save "$EXaM_temp/07_EXaMvsRCT_information_histogram_psm_coarse_$simulation.dta", replace
restore

	use "$EXaM_temp/07_EXaMvsRCT_information_histogram_psm_coarse_$simulation.dta", clear
	egen sd_b_RCT_ = sd(b_RCT)
	egen sd_b_EXaM_ = sd(b_EXaM)
	gen p_b_RCT_ = 2*ttail($degreef, abs(b_RCT_ / sd_b_RCT_))
	gen p_b_EXaM_ = 2*ttail($degreef, abs(b_EXaM_ / sd_b_EXaM_))
	
	foreach var of varlist b_EXaM_ b_RCT_ p_EXaM_ p_RCT_ se_EXaM_ se_RCT_ p_b_RCT_ p_b_EXaM_ {
			sum `var', det
			global mean_`var' = round(r(mean),.0001)
			global medi_`var' = round(r(p50),.0001)
		}

*	BETA ESTIMATES		
	twoway (histogram b_RCT_, frac scheme(s1manual) width(0.0015) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:ATE Estimates (beta Based on Propensity Score Weighting)}", size(medsmall)) xscale(range(-0.15 0.05)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram b_EXaM_, xline($medi_b_EXaM_, lc(red) lpattern(solid)) width(0.0015) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
	graph export "$EXaM_output/07_EXaMvsRCT_information_beta_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_beta_coarse_medi_$simulation", replace

	twoway (histogram b_RCT_, frac scheme(s1manual) width(0.0015) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:ATE Estimates (beta Based on Propensity Score Weighting)}", size(medsmall)) xscale(range(-0.15 0.05)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram b_EXaM_, xline($mean_b_EXaM_, lc(red) lpattern(solid)) width(0.0015) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
	graph export "$EXaM_output/07_EXaMvsRCT_information_beta_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_beta_coarse_mean_$simulation", replace

		twoway (histogram b_RCT_, frac scheme(s1manual) width(0.0015) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:ATE Estimates (beta Based on Propensity Score Weighting)}", size(medium)) xscale(range(-0.15 0.05)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram b_EXaM_, width(0.0015) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_RCT_information_beta_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_beta_coarse_medi_$simulation", replace
	
	
	twoway (histogram b_RCT_, frac scheme(s1manual) width(0.0015) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:ATE Estimates (beta Based on Propensity Score Weighting)}", size(medium)) xscale(range(-0.15 0.05)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram b_EXaM_, width(0.0015) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_RCT_information_beta_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_beta_coarse_mean_$simulation", replace
	
*	STANDARD ERRORS OF BETA ESTIMATES
	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Non-robust, beta Based on Propensity Score Weighting)}", size(small)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_se_EXaM_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))) ///
	(histogram se_EXaM_, xline($medi_se_EXaM_, lc(red) lpattern(solid)) width(0.0001) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_EXaMvsRCT_information_se_beta_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_se_beta_coarse_medi_$simulation", replace
	
	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Non-robust, beta Based on Propensity Score Weighting)}", size(small)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($mean_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_se_EXaM_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))) ///
	(histogram se_EXaM_, xline($mean_se_EXaM_, lc(red) lpattern(solid)) width(0.0001) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_EXaMvsRCT_information_se_beta_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_se_beta_coarse_mean_$simulation", replace

*	P-VALUES OF beta ESTIMATES (Use standard deviation of b as SE, 20171213)
	twoway (histogram p_b_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_p_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (beta, Exact)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(range(0 1) lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#10, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_p_b_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_p_b_EXaM_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))		) ///
	(histogram p_b_EXaM_, width(0.05) frac fcolor(none) lcolor(red) lwidth(vthin) ///
		xline($medi_p_b_EXaM_, lc(red) lpattern(solid))), ///
		legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/07_EXaMvsRCT_information_p005SD_beta_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_p005SD_beta_coarse_medi_$simulation", replace

	twoway (histogram p_b_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_p_b_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (beta, Exact)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(range(0 1) lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#10, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
	(histogram p_b_EXaM_, width(0.05) frac fcolor(none) lcolor(red) lwidth(vthin) ///
		xline($mean_p_b_EXaM_, lc(red) lpattern(solid)) ///
			xmlabel($mean_p_b_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_p_b_EXaM_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))), ///
		legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/07_EXaMvsRCT_information_p005SD_beta_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_p005SD_beta_coarse_mean_$simulation", replace

*******robust part
use "$EXaM_temp/07_EXaMvsRCT_information_coarse_$simulation.dta", clear
set more off
gen p_RCT = 663/1540
forval n = 1/$simulation {
	gen yi_EXaM_`n' = draw_dia_all_`n' * treat_EXaM_`n' + dia_hh_fe + $con + ($ba_1 - $ba_2 - $ba_3 )/3
	gen yi_RCT_`n' = draw_dia_all_`n' * treat_RCT_`n' + dia_hh_fe + $con + ($ba_1 - $ba_2 - $ba_3 )/3
	
	*	EXaM
	gen b_EXaM_`n'=0
	gen se_EXaM_`n'=0

	egen group_`n'=group(p_real_`n')
    	su group_`n', det
	
	forvalues s = 1/`r(max)'{
	
	su group_`n' if group_`n'==`s', det
	scalar r=r(N)
	if r >5{
	reg yi_EXaM_`n' treat_EXaM_`n' if group_`n'==`s', vce(ro)
	
	gen b_EXaM_`n'_`s' =_b[treat_EXaM_`n']
	gen se_EXaM_`n'_`s' = _se[treat_EXaM_`n']

	
	replace b_EXaM_`n'=b_EXaM_`n'+b_EXaM_`n'_`s'*e(N)/1540
	replace se_EXaM_`n'=se_EXaM_`n'+se_EXaM_`n'_`s'*e(N)/1540

	
	}
	else if r<=5{
	gen b_EXaM_`n'_`s'=0
	replace b_EXaM_`n'=b_EXaM_`n'+b_EXaM_`n'_`s'*0
	gen se_EXaM_`n'_`s'=0
	replace se_EXaM_`n'=se_EXaM_`n'+se_EXaM_`n'_`s'*0

	}
	drop b_EXaM_`n'_`s' 
	drop se_EXaM_`n'_`s'
	scalar drop r
	}
	gen p_EXaM_`n' = 2*ttail(e(df_r),abs( b_EXaM_`n' /se_EXaM_`n'))

	drop group_`n' 
	
	
	*	RCT
	reg yi_RCT_`n' treat_RCT_`n', vce(ro)
	gen b_RCT_`n' = _b[treat_RCT_`n']
	gen p_RCT_`n' = 2*ttail(e(df_r),abs( _b[treat_RCT_`n']/_se[treat_RCT_`n']))
    gen se_RCT_`n'=_se[treat_RCT_`n']
}

	

preserve
	keep b_EXaM* b_RCT* p_EXaM* p_RCT* se_EXaM* se_RCT*
	keep if _n == 1
	gen n = _n
	reshape long b_EXaM_ b_RCT_ p_EXaM_ p_RCT_ se_EXaM_ se_RCT_, i(n) j(number)
	save "$EXaM_temp/07_EXaMvsRCT_information_histogram_psm_robust_coarse_$simulation.dta", replace
restore

	use "$EXaM_temp/07_EXaMvsRCT_information_histogram_psm_robust_coarse_$simulation.dta", clear
	foreach var of varlist p_EXaM_ p_RCT_ b_EXaM_ b_RCT_ se_EXaM_ se_RCT_{
			sum `var', det
			global mean_`var'ro = round(r(mean),.0001)
			global medi_`var'ro = round(r(p50),.0001)
		}

*	BETA ESTIMATES	
		
*	STANDARD ERRORS OF BETA ESTIMATES
	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_se_RCT_ro, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Robust, beta Based on Propensity Score Weighting)}", size(small)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_se_RCT_ro, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_se_EXaM_ro, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))) ///
	(histogram se_EXaM_, xline($medi_se_EXaM_ro, lc(red) lpattern(solid)) width(0.0001) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_EXaMvsRCT_information_se_beta_robust_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_p005SD_beta_coarse_mean_$simulation", replace

	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_se_RCT_ro, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (Robust, beta Based on Propensity Score Weighting)}", size(small)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($mean_se_RCT_ro, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_se_EXaM_ro, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))) ///
	(histogram se_EXaM_, xline($mean_se_EXaM_ro, lc(red) lpattern(solid)) width(0.0001) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_EXaMvsRCT_information_se_beta_robust_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_se_beta_robust_coarse_mean_$simulation", replace
