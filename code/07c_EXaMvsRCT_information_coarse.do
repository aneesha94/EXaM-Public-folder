*	============================================================================
*	AAIW - run 07b python code before this
********************************************************************************


	clear
	clear mata
	clear matrix
	set more off
	set mem 650m
	set maxvar 32767
	
	cd		"$EXaM"

	
*	===========================================
*	b ESTIMATES
***********************************************
	import delimited "$EXaM/input/AAIWexam.csv", varnames(nonames) clear
	rename v1 b_EXaM_
	rename v2 se_EXaM_
	rename v3 p_EXaM_
	save "$EXaM_temp/07_AAIWexam.dta", replace
	import delimited "$EXaM/input/AAIWrct.csv", varnames(nonames) clear	
	rename v1 b_RCT_
	rename v2 se_RCT_
	rename v3 p_RCT_
	merge 1:1 _n using "$EXaM_temp/07_AAIWexam.dta"
	
	
	foreach var of varlist b_EXaM_ b_RCT_ p_EXaM_ p_RCT_ se_EXaM_ se_RCT_ {
			sum `var', det
			global mean_`var' = round(r(mean),.0001)
			global medi_`var' = round(r(p50),.0001)
		}
		

*	STANDARD ERRORS OF b ESTIMATES
	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (AAIW)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_se_EXaM_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))) ///
	(histogram se_EXaM_, xline($medi_se_EXaM_, lc(red) lpattern(solid)) width(0.0001) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_EXaMvsRCT_information_se_b_AAIW_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_se_b_AAIW_coarse_medi_$simulation", replace

	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (AAIW)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram se_EXaM_, width(0.0001) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_RCT_information_se_AAIW_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_se_AAIW_coarse_medi_$simulation", replace
	
	
	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (AAIW)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($mean_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_se_EXaM_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))) ///
	(histogram se_EXaM_, xline($mean_se_EXaM_, lc(red) lpattern(solid)) width(0.0001) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_EXaMvsRCT_information_se_b_AAIW_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_se_b_AAIW_coarse_mean_$simulation", replace

	twoway (histogram se_RCT_, frac scheme(s1manual) width(0.0001) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_se_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:Standard Errors of ATE Estimates (AAIW)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($mean_se_RCT_, nolabels format(%9.4fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram se_EXaM_, width(0.0001) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		
	graph export "$EXaM_output/07_RCT_information_se_AAIW_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_se_AAIW_coarse_mean_$simulation", replace

	
*	P-VALUES OF b ESTIMATES
	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (AAIW)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(range(0 1) lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#10, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_p_EXaM_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))		) ///
	(histogram p_EXaM_, width(0.05) frac fcolor(none) lcolor(red) lwidth(vthin) ///
		xline($medi_p_EXaM_, lc(red) lpattern(solid))), ///
		legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
	graph export "$EXaM_output/07_EXaMvsRCT_information_p005_b_AAIW_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_p005_b_AAIW_coarse_medi_$simulation", replace

	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($medi_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (AAIW)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($medi_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram p_EXaM_, width(0.05) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces)
	
	graph export "$EXaM_output/07_RCT_information_p005_AAIW_coarse_medi_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_p005_AAIW_coarse_medi_$simulation", replace
	
	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (AAIW)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($mean_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_p_EXaM_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(red) tlcolor(red))) ///
	(histogram p_EXaM_, xline($mean_p_EXaM_, lc(red) lpattern(solid)) width(0.05) frac fcolor(none) lcolor(red) lwidth(vthin)), ///
	legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/07_EXaMvsRCT_information_p005_b_AAIW_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_EXaMvsRCT_information_p005_b_AAIW_coarse_mean_$simulation", replace

	twoway (histogram p_RCT_, frac scheme(s1manual) width(0.05) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		xline($mean_p_RCT_, lc(blue) lpattern(dash)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:P-values of ATE Estimates (AAIW)}", size(medium)) yscale(lwidth(medium) ///
		lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor("160 213 255") lwidth(vthin) lcolor("160 213 255")    ///
		xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($mean_p_RCT_, nolabels format(%9.2fc) labsize(vsmall) add custom labcolor(blue) tlcolor(blue))) ///
	(histogram p_EXaM_, width(0.05) frac fcolor(none) lcolor(none) lwidth(none)), ///
	legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
	
	graph export "$EXaM_output/07_RCT_information_p005_AAIW_coarse_mean_$simulation.png", replace
	graph save "$EXaM_output/07_RCT_information_p005_AAIW_coarse_mean_$simulation", replace
