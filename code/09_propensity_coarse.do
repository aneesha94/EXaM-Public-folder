
*	Graphing EXaM propensity scores
********************************************************************************
	clear
	clear mata
	clear matrix
	set more off
	set mem 650m
	set maxvar 32767

	cd		"$EXaM"

*	Coarse welfare simulation	
	import delimited using "$EXaM_input/df_results_coarse.csv", varn(1) clear
	
	
	local j = $simulation + 1
	forval i = 2/`j' {
		replace v`i' = subinstr(v`i', "[", "", .)
		replace v`i' = substr(v`i', 1, strpos(v`i',",")-1)
		destring v`i', replace
		local h = `i' - 1
		rename v`i' p_real_`h'
	}
	rename v1 subject
	save "$EXaM_temp/06_p_real_coarse_$simulation.dta", replace
	
	
preserve
	use		"$EXaM_temp/03_WTP_all_byhhid_coarse_$simulation.dta", clear
	gen		subject = _n - 1
	save	"$EXaM_temp/06_WTP_all_byhhid_subject_coarse_$simulation.dta", replace

	use		"$EXaM_temp/04_HTE_all_byhhid_coarse_$simulation.dta", clear
	gen		subject = _n - 1
	save	"$EXaM_temp/06_HTE_all_byhhid_subject_coarse_$simulation.dta", replace

restore
	
	merge 1:1 subject using "$EXaM_temp/06_WTP_all_byhhid_subject_coarse_$simulation.dta", nogen
	merge 1:1 subject using "$EXaM_temp/06_HTE_all_byhhid_subject_coarse_$simulation.dta", nogen
	

*	Generate RCT and EXaM welfare values
*	PTE is multiplied by -1 because unit is changed to reduction
forval i = 1/$simulation {
	capture gen WTP_EXaM_`i' = draw_wdays_all_`i' * (1-p_real_`i')
	capture gen WTP_RCT_`i' = draw_wdays_all_`i' * 663/1540
	capture gen HTE_EXaM_`i' = draw_dia_all_`i' * (1-p_real_`i') * -1
	capture gen HTE_RCT_`i' = draw_dia_all_`i' * 663/1540 * -1
}

keep subject p_real_*
reshape long p_real_, i(subject)
gen p_rct_ = 663/1540
save "$EXaM_temp/09_propensity_coarse_$simulation.dta", replace
use "$EXaM_temp/09_propensity_coarse_$simulation.dta", clear
sum p_rct_, det
global mean_RCT_propensity = 663/1540
sum p_real_, det
global mean_EXaM_propensity = round(r(mean),.01)

		twoway (histogram p_real_, frac scheme(s1manual) width(0.01) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:Propensity Score}", size(medium) color(gs0)) ///
			xline($mean_EXaM_propensity, lc(gs0) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Fraction}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(histogram p_rct_, frac width(0.01) ///
			xline($mean_RCT_propensity, lc(gs0) lpattern(solid) lwidth(medthin)) ///
			fcolor(none) lwidth(thin) lcolor(gs0)), legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces)

histogram p_real_, frac scheme(s1manual) width(0.01) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:Propensity Score p*(epsilon) in EXaM}", size(medium) color(gs0)) ///
			xline($mean_RCT_propensity, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Fraction}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(0(0.5)1, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			xmlabel($mean_RCT_propensity "RCT", labsize(small) add custom labcolor(blue) tlcolor(blue))
			
graph export "$EXaM_output/09_EXaM_propensity_coarse_$simulation.png", replace
graph save "$EXaM_output/09_EXaM_propensity_coarse_$simulation", replace
