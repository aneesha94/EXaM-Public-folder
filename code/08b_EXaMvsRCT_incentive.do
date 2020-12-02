*	Graphing EXaM vs RCT comparison in terms of incentive compatibility (coarse)
********************************************************************************

	clear
	clear mata
	clear matrix
	set more off
	set mem 650m
	set maxvar 32767

	cd		"$EXaM"

*	1. mani10: Line 20
*	2. mani100: Line 162
*	3. mani1000: Line 307
*	4. umani100minus: Line 451
*	5. umani100plus: Line 594
* 	6. additional table: Line 737
	
*	1. mani10
********************************************************************************	

/*	Merge Relevant Datasets */

*	Dictionary
	use "$EXaM_temp/08_WTP_with_manipulator10_$simulation.dta", clear

	gsort hh_id
	keep hh_id
	gen subject = _n-1
	save "$EXaM_temp/08_mani10_dictionary_$simulation.dta", replace

*	Probability manipulated
	import delimited using "$EXaM_input/df_results_mani10.csv", varn(1) clear

	local j = $simulation + 1
	forval i = 2/`j' {
		replace v`i' = subinstr(v`i', "[", "", .)
		replace v`i' = substr(v`i', 1, strpos(v`i',",")-1)
		destring v`i', replace
		local h = `i' - 1
		rename v`i' p_mani_`h'
	}
		rename v1 subject

	save "$EXaM_temp/08_mani10_p_mani_$simulation.dta", replace
	merge 1:1 subject using "$EXaM_temp/06_p_real_$simulation.dta", nogen
	merge 1:1 subject using "$EXaM_temp/08_mani10_dictionary_$simulation.dta", nogen
	save "$EXaM_temp/08_mani10_p_$simulation.dta", replace

*	Merge with WTP manipulated
	preserve
		use "$EXaM_temp/08_WTP_with_manipulator10_$simulation.dta", clear

		gsort hh_id
		rename draw_wdays_all* mani_wtp_mani*
		keep hh_id mani_wtp_mani* subjectno*

		save "$EXaM_temp/08_mani10_wtp_mani_$simulation.dta", replace

	restore
	merge 1:1 hh_id using "$EXaM_temp/08_mani10_wtp_mani_$simulation.dta", nogen

	save "$EXaM_temp/08_mani10_p_wtpmani_$simulation.dta", replace

*	Merge with WTP real
	preserve
		use "$EXaM_temp/03_WTP_all_byhhid_$simulation.dta", clear

		keep hh_id draw_wdays*
		rename draw_wdays_all* mani_wtp_real*
		keep hh_id mani_wtp_real*
		save "$EXaM_temp/08_mani10_wtp_real_$simulation.dta", replace

	restore

	merge 1:1 hh_id using "$EXaM_temp/08_mani10_wtp_real_$simulation.dta", nogen
	save "$EXaM_temp/08_mani10_$simulation.dta", replace
	
/*	Pick manipulated dataset from each simulation */
use "$EXaM_temp/08_mani10_$simulation.dta", clear

forval i = 1/$simulation {
	preserve
	keep if subjectno_`i' == hh_id
	* Modified the definition of w_star, 20170910
	capture gen w_star_mani = (1-p_mani_`i') * mani_wtp_real_`i' 
	capture gen w_star_real = (1-p_real_`i') * mani_wtp_real_`i'
	capture gen gain = w_star_mani - w_star_real
	rename mani_wtp_real_`i' wtp_real
	capture keep subject hh_id w_star_mani w_star_real gain wtp_real
	capture save "$EXaM_temp/08_mani10_`i'_$simulation.dta", replace

	restore
}	

use "$EXaM_temp/08_mani10_1_$simulation.dta", clear

forval i = 2/$simulation {
	capture append using "$EXaM_temp/08_mani10_`i'_$simulation.dta"

}
keep subject hh_id w_star_mani w_star_real gain wtp_real
gen gain2 = gain / abs(wtp_real)
save "$EXaM_temp/08_EXaM_vs_rct_incentive_mani10_$simulation.dta", replace


use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani10_$simulation.dta", clear

sum gain, det
global mean_gain = round(r(mean),.01)
global medi_gain = round(r(p50),.01)
sum gain2, det
global mean_gain2 = round(r(mean),.0001)
global medi_gain2 = round(r(p50),.0001)

* mean
preserve
	replace gain = -59.9 if gain <= -60
	replace gain = 19.9 if gain >= 20
	twoway (histogram gain if gain >= -60 & gain <= 20, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain if gain >= -60 & gain <= 20, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_gain, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $mean_gain 1.02 $mean_gain, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 1.02 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani10_mean_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani10_mean_$simulation", replace
restore

* median
preserve
	replace gain = -59.9 if gain <= -60
	replace gain = 19.9 if gain >= 20
	twoway (histogram gain if gain >= -60 & gain <= 20, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain if gain >= -60 & gain <= 20, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_gain, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $medi_gain 1.02 $medi_gain, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 1.02 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani10_medi_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani10_medi_$simulation", replace
restore

* mean (normalized)
preserve
	replace gain2 = -0.799 if gain2 <= -0.8
	replace gain2 = 0.199 if gain2 >= 0.2
	twoway (histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_gain2, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $mean_gain2 1.02 $mean_gain2, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 1.02 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani10_mean2_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani10_mean2_$simulation", replace
restore

* median (normalized)
preserve
	replace gain2 = -0.799 if gain2 <= -0.8
	replace gain2 = 0.199 if gain2 >= 0.2
	twoway (histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_gain2, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $medi_gain2 1.02 $medi_gain2, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 1.02 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani10_medi2_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani10_medi2_$simulation", replace
restore


*	2. mani100
********************************************************************************
	
/*	Merge Relevant Datasets */

*	Dictionary
	use "$EXaM_temp/08_WTP_with_manipulator100_$simulation.dta", clear

	gsort hh_id
	keep hh_id
	gen subject = _n-1
	save "$EXaM_temp/08_mani100_dictionary_$simulation.dta", replace
	
*	Probability manipulated
	import delimited using "$EXaM_input/df_results_mani100.csv", varn(1) clear

	local j = $simulation + 1
	forval i = 2/`j' {
		replace v`i' = subinstr(v`i', "[", "", .)
		replace v`i' = substr(v`i', 1, strpos(v`i',",")-1)
		destring v`i', replace
		local h = `i' - 1
		rename v`i' p_mani_`h'
	}
		rename v1 subject

	save "$EXaM_temp/08_mani100_p_mani_$simulation.dta", replace
	merge 1:1 subject using "$EXaM_temp/06_p_real_$simulation.dta", nogen
	merge 1:1 subject using "$EXaM_temp/08_mani100_dictionary_$simulation.dta", nogen
	save "$EXaM_temp/08_mani100_p_$simulation.dta", replace

*	Merge with WTP manipulated
	preserve
		use "$EXaM_temp/08_WTP_with_manipulator100_$simulation.dta", clear

		gsort hh_id
		rename draw_wdays_all* mani_wtp_mani*
		keep hh_id mani_wtp_mani* subjectno*

		save "$EXaM_temp/08_mani100_wtp_mani_$simulation.dta", replace

	restore
	merge 1:1 hh_id using "$EXaM_temp/08_mani100_wtp_mani_$simulation.dta", nogen

	save "$EXaM_temp/08_mani100_p_wtpmani_$simulation.dta", replace

*	Merge with WTP real
	preserve
		use "$EXaM_temp/03_WTP_all_byhhid_$simulation.dta", clear

		keep hh_id draw_wdays*
		rename draw_wdays_all* mani_wtp_real*
		keep hh_id mani_wtp_real*
		save "$EXaM_temp/08_mani100_wtp_real_$simulation.dta", replace

	restore

	merge 1:1 hh_id using "$EXaM_temp/08_mani100_wtp_real_$simulation.dta", nogen
	save "$EXaM_temp/08_mani100_$simulation.dta", replace
	
/*	Pick manipulated dataset from each simulation */
use "$EXaM_temp/08_mani100_$simulation.dta", clear

forval i = 1/$simulation {
	preserve
	keep if subjectno_`i' == hh_id
	* Modified the definition of w_star, 20170910
	capture gen w_star_mani = (1-p_mani_`i') * mani_wtp_real_`i' 
	capture gen w_star_real = (1-p_real_`i') * mani_wtp_real_`i'
	capture gen gain = w_star_mani - w_star_real
	rename mani_wtp_real_`i' wtp_real
	capture keep subject hh_id w_star_mani w_star_real gain wtp_real
	capture save "$EXaM_temp/08_mani100_`i'_$simulation.dta", replace

	restore
}	

use "$EXaM_temp/08_mani100_1_$simulation.dta", clear

forval i = 2/$simulation {
	capture append using "$EXaM_temp/08_mani100_`i'_$simulation.dta"

}
keep subject hh_id w_star_mani w_star_real gain wtp_real
gen gain2 = gain / abs(wtp_real)
save "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100_$simulation.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100_$simulation.dta", clear

sum gain, det
global mean_gain = round(r(mean),.01)
global medi_gain = round(r(p50),.01)
sum gain2, det
global mean_gain2 = round(r(mean),.0001)
global medi_gain2 = round(r(p50),.0001)

* mean
preserve
	replace gain = -59.9 if gain <= -60
	replace gain = 19.9 if gain >= 20
	twoway (histogram gain if gain >= -60 & gain <= 20, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#3, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain if gain >= -60 & gain <= 20, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#3, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_gain, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $mean_gain 0.612 $mean_gain, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.612 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100_mean_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100_mean_$simulation", replace
restore

* median
preserve
	replace gain = -59.9 if gain <= -60
	replace gain = 19.9 if gain >= 20
	twoway (histogram gain if gain >= -60 & gain <= 20, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#3, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain if gain >= -60 & gain <= 20, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#3, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_gain, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $medi_gain 0.612 $medi_gain, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.612 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100_medi_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100_medi_$simulation.png", replace
restore

* mean (normalized)
preserve
	replace gain2 = -0.799 if gain2 <= -0.8
	replace gain2 = 0.199 if gain2 >= 0.2
	twoway (histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_gain2, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $mean_gain2 0.612 $mean_gain2, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.612 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100_mean2_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100_mean2_$simulation", replace
restore

* median (normalized)
preserve
	replace gain2 = -0.799 if gain2 <= -0.8
	replace gain2 = 0.199 if gain2 >= 0.2
	twoway (histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_gain2, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $medi_gain2 0.612 $medi_gain2, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.612 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100_medi2_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100_medi2_$simulation", replace
restore


*	3. mani1000
********************************************************************************

/*	Merge Relevant Datasets */

*	Dictionary
	use "$EXaM_temp/08_WTP_with_manipulator1000_$simulation.dta", clear

	gsort hh_id
	keep hh_id
	gen subject = _n-1
	save "$EXaM_temp/08_mani1000_dictionary_$simulation.dta", replace

	
*	Probability manipulated
	import delimited using "$EXaM_input/df_results_mani1000.csv", varn(1) clear

	local j = $simulation + 1
	forval i = 2/`j' {
		replace v`i' = subinstr(v`i', "[", "", .)
		replace v`i' = substr(v`i', 1, strpos(v`i',",")-1)
		destring v`i', replace
		local h = `i' - 1
		rename v`i' p_mani_`h'
	}
		rename v1 subject

	save "$EXaM_temp/08_mani1000_p_mani_$simulation.dta", replace
	merge 1:1 subject using "$EXaM_temp/06_p_real_$simulation.dta", nogen
	merge 1:1 subject using "$EXaM_temp/08_mani1000_dictionary_$simulation.dta", nogen
	save "$EXaM_temp/08_mani1000_p_$simulation.dta", replace

*	Merge with WTP manipulated
	preserve
		use "$EXaM_temp/08_WTP_with_manipulator1000_$simulation.dta", clear

		gsort hh_id
		rename draw_wdays_all* mani_wtp_mani*
		keep hh_id mani_wtp_mani* subjectno*

		save "$EXaM_temp/08_mani1000_wtp_mani_$simulation.dta", replace

	restore
	merge 1:1 hh_id using "$EXaM_temp/08_mani1000_wtp_mani_$simulation.dta", nogen

	save "$EXaM_temp/08_mani1000_p_wtpmani_$simulation.dta", replace

*	Merge with WTP real
	preserve
		use "$EXaM_temp/03_WTP_all_byhhid_$simulation.dta", clear

		keep hh_id draw_wdays*
		rename draw_wdays_all* mani_wtp_real*
		keep hh_id mani_wtp_real*
		save "$EXaM_temp/08_mani1000_wtp_real_$simulation.dta", replace

	restore

	merge 1:1 hh_id using "$EXaM_temp/08_mani1000_wtp_real_$simulation.dta", nogen
	save "$EXaM_temp/08_mani1000_$simulation.dta", replace
	
/*	Pick manipulated dataset from each simulation */
use "$EXaM_temp/08_mani1000_$simulation.dta", clear

forval i = 1/$simulation {
	preserve
	keep if subjectno_`i' == hh_id
	* Modified the definition of w_star, 20170910
	capture gen w_star_mani = (1-p_mani_`i') * mani_wtp_real_`i' 
	capture gen w_star_real = (1-p_real_`i') * mani_wtp_real_`i'
	capture gen gain = w_star_mani - w_star_real
	rename mani_wtp_real_`i' wtp_real
	capture keep subject hh_id w_star_mani w_star_real gain wtp_real
	capture save "$EXaM_temp/08_mani1000_`i'_$simulation.dta", replace

	restore
}	

use "$EXaM_temp/08_mani1000_1_$simulation.dta", clear

forval i = 2/$simulation {
	capture append using "$EXaM_temp/08_mani1000_`i'_$simulation.dta"

}
keep subject hh_id w_star_mani w_star_real gain wtp_real
gen gain2 = gain / abs(wtp_real)
save "$EXaM_temp/08_EXaM_vs_rct_incentive_mani1000_$simulation.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani1000_$simulation.dta", clear

sum gain, det
global mean_gain = round(r(mean),.01)
global medi_gain = round(r(p50),.01)
sum gain2, det
global mean_gain2 = round(r(mean),.0001)
global medi_gain2 = round(r(p50),.0001)

* mean
preserve
	replace gain = -59.9 if gain <= -60
	replace gain = 19.9 if gain >= 20
	twoway (histogram gain if gain >= -60 & gain <= 20, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain if gain >= -60 & gain <= 20, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_gain, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $mean_gain 0.51 $mean_gain, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.51 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani1000_mean_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani1000_mean_$simulation", replace
restore

* median
preserve
	replace gain = -59.9 if gain <= -60
	replace gain = 19.9 if gain >= 20
	twoway (histogram gain if gain >= -60 & gain <= 20, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain if gain >= -60 & gain <= 20, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_gain, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $medi_gain 0.51 $medi_gain, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.51 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani1000_medi_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani1000_medi_$simulation", replace
restore

* mean (normalized)
preserve
	replace gain2 = -0.799 if gain2 <= -0.8
	replace gain2 = 0.199 if gain2 >= 0.2
	twoway (histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_gain2, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $medi_gain2 0.612 $medi_gain2, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.612 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani1000_mean2_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani1000_mean2_$simulation", replace
restore

* median (normalized)
preserve
	replace gain2 = -0.799 if gain2 <= -0.8
	replace gain2 = 0.199 if gain2 >= 0.2
	twoway (histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_gain2, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $medi_gain2 0.612 $medi_gain2, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.612 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani1000_medi2_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani1000_medi2_$simulation", replace
restore


*	4. umani100minus
********************************************************************************

/*	Merge Relevant Datasets */

*	Dictionary
	use "$EXaM_temp/08_WTP_with_manipulator100uminus_$simulation.dta", clear

	gsort hh_id
	keep hh_id
	gen subject = _n-1
	save "$EXaM_temp/08_mani100uminus_dictionary_$simulation.dta", replace

*	Probability manipulated
	import delimited using "$EXaM_input/df_results_mani100uminus.csv", varn(1) clear

	local j = $simulation + 1
	forval i = 2/`j' {
		replace v`i' = subinstr(v`i', "[", "", .)
		replace v`i' = substr(v`i', 1, strpos(v`i',",")-1)
		destring v`i', replace
		local h = `i' - 1
		rename v`i' p_mani_`h'
	}
		rename v1 subject

	save "$EXaM_temp/08_mani100uminus_p_mani_$simulation.dta", replace
	merge 1:1 subject using "$EXaM_temp/06_p_real_$simulation.dta", nogen
	merge 1:1 subject using "$EXaM_temp/08_mani100uminus_dictionary_$simulation.dta", nogen
	save "$EXaM_temp/08_mani100uminus_p_$simulation.dta", replace

*	Merge with WTP manipulated
	preserve
		use "$EXaM_temp/08_WTP_with_manipulator100uminus_$simulation.dta", clear

		gsort hh_id
		rename draw_wdays_all* mani_wtp_mani*
		keep hh_id mani_wtp_mani* subjectno*

		save "$EXaM_temp/08_mani100uminus_wtp_mani_$simulation.dta", replace

	restore
	merge 1:1 hh_id using "$EXaM_temp/08_mani100uminus_wtp_mani_$simulation.dta", nogen

	save "$EXaM_temp/08_mani100uminus_p_wtpmani_$simulation.dta", replace

*	Merge with WTP real
	preserve
		use "$EXaM_temp/03_WTP_all_byhhid_$simulation.dta", clear

		keep hh_id draw_wdays*
		rename draw_wdays_all* mani_wtp_real*
		keep hh_id mani_wtp_real*
		save "$EXaM_temp/08_mani100uminus_wtp_real_$simulation.dta", replace

	restore

	merge 1:1 hh_id using "$EXaM_temp/08_mani100uminus_wtp_real_$simulation.dta", nogen
	save "$EXaM_temp/08_mani100uminus_$simulation.dta", replace

/*	Pick manipulated dataset from each simulation */
use "$EXaM_temp/08_mani100uminus_$simulation.dta", clear

forval i = 1/$simulation {
	preserve
	keep if subjectno_`i' == hh_id
	* Modified the definition of w_star, 20170910
	capture gen w_star_mani = (1-p_mani_`i') * mani_wtp_real_`i' 
	capture gen w_star_real = (1-p_real_`i') * mani_wtp_real_`i'
	capture gen gain = w_star_mani - w_star_real
	rename mani_wtp_real_`i' wtp_real
	capture keep subject hh_id w_star_mani w_star_real gain wtp_real
	capture save "$EXaM_temp/08_mani100uminus_`i'_$simulation.dta", replace

	restore
}	

use "$EXaM_temp/08_mani100uminus_1_$simulation.dta", clear

forval i = 2/$simulation {
	capture append using "$EXaM_temp/08_mani100uminus_`i'_$simulation.dta"

}
keep subject hh_id w_star_mani w_star_real gain wtp_real
gen gain2 = gain / abs(wtp_real)
save "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100uminus_$simulation.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100uminus_$simulation.dta", clear

sum gain, det
global mean_gain = round(r(mean),.01)
global medi_gain = round(r(p50),.01)
sum gain2, det
global mean_gain2 = round(r(mean),.0001)
global medi_gain2 = round(r(p50),.0001)

* mean
preserve
	replace gain = -59.9 if gain <= -60
	replace gain = 19.9 if gain >= 20
	twoway (histogram gain if gain >= -60 & gain <= 20, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain if gain >= -60 & gain <= 20, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_gain, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///		
		(pcarrowi 0 $mean_gain 0.51 $mean_gain, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.51 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uminus_mean_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uminus_mean_$simulation", replace
restore

* median
preserve
	replace gain = -59.9 if gain <= -60
	replace gain = 19.9 if gain >= 20
	twoway (histogram gain if gain >= -60 & gain <= 20, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain if gain >= -60 & gain <= 20, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_gain, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $medi_gain 0.51 $medi_gain, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.51 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uminus_medi_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uminus_medi_$simulation", replace
restore

* mean (normalized)
preserve
	replace gain2 = -0.799 if gain2 <= -0.8
	replace gain2 = 0.199 if gain2 >= 0.2
	twoway (histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_gain2, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $mean_gain2 0.612 $mean_gain2, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.612 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uminus_mean2_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uminus_mean2_$simulation", replace
restore

* median (normalized)
preserve
	replace gain2 = -0.799 if gain2 <= -0.8
	replace gain2 = 0.199 if gain2 >= 0.2
	twoway (histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_gain2, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $medi_gain2 0.612 $medi_gain2, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.612 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uminus_medi2_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uminus_medi2_$simulation", replace
restore

*	5. umani100plus
********************************************************************************

/*	Merge Relevant Datasets */

*	Dictionary
	use "$EXaM_temp/08_WTP_with_manipulator100uplus_$simulation.dta", clear

	gsort hh_id
	keep hh_id
	gen subject = _n-1
	save "$EXaM_temp/08_mani100uplus_dictionary_$simulation.dta", replace
	
*	Probability manipulated
	import delimited using "$EXaM_input/df_results_mani100uplus.csv", varn(1) clear

	local j = $simulation + 1
	forval i = 2/`j' {
		replace v`i' = subinstr(v`i', "[", "", .)
		replace v`i' = substr(v`i', 1, strpos(v`i',",")-1)
		destring v`i', replace
		local h = `i' - 1
		rename v`i' p_mani_`h'
	}
		rename v1 subject

	save "$EXaM_temp/08_mani100uplus_p_mani_$simulation.dta", replace
	merge 1:1 subject using "$EXaM_temp/06_p_real_$simulation.dta", nogen
	merge 1:1 subject using "$EXaM_temp/08_mani100uplus_dictionary_$simulation.dta", nogen
	save "$EXaM_temp/08_mani100uplus_p_$simulation.dta", replace

*	Merge with WTP manipulated
	preserve
		use "$EXaM_temp/08_WTP_with_manipulator100uplus_$simulation.dta", clear

		gsort hh_id
		rename draw_wdays_all* mani_wtp_mani*
		keep hh_id mani_wtp_mani* subjectno*

		save "$EXaM_temp/08_mani100uplus_wtp_mani_$simulation.dta", replace

	restore
	merge 1:1 hh_id using "$EXaM_temp/08_mani100uplus_wtp_mani_$simulation.dta", nogen

	save "$EXaM_temp/08_mani100uplus_p_wtpmani_$simulation.dta", replace

*	Merge with WTP real
	preserve
		use "$EXaM_temp/03_WTP_all_byhhid_$simulation.dta", clear

		keep hh_id draw_wdays*
		rename draw_wdays_all* mani_wtp_real*
		keep hh_id mani_wtp_real*
		save "$EXaM_temp/08_mani100uplus_wtp_real_$simulation.dta", replace

	restore

	merge 1:1 hh_id using "$EXaM_temp/08_mani100uplus_wtp_real_$simulation.dta", nogen
	save "$EXaM_temp/08_mani100uplus_$simulation.dta", replace

/*	Pick manipulated dataset from each simulation */
use "$EXaM_temp/08_mani100uplus_$simulation.dta", clear

forval i = 1/$simulation {
	preserve
	keep if subjectno_`i' == hh_id
	* Modified the definition of w_star, 20170910
	capture gen w_star_mani = (1-p_mani_`i') * mani_wtp_real_`i' 
	capture gen w_star_real = (1-p_real_`i') * mani_wtp_real_`i'
	capture gen gain = w_star_mani - w_star_real
	rename mani_wtp_real_`i' wtp_real
	capture keep subject hh_id w_star_mani w_star_real gain wtp_real
	capture save "$EXaM_temp/08_mani100uplus_`i'_$simulation.dta", replace

	restore
}	

use "$EXaM_temp/08_mani100uplus_1_$simulation.dta", clear

forval i = 2/$simulation {
	capture append using "$EXaM_temp/08_mani100uplus_`i'_$simulation.dta"

}
keep subject hh_id w_star_mani w_star_real gain wtp_real
gen gain2 = gain / abs(wtp_real)
save "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100uplus_$simulation.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100uplus_$simulation.dta", clear

sum gain, det
global mean_gain = round(r(mean),.01)
global medi_gain = round(r(p50),.01)
sum gain2, det
global mean_gain2 = round(r(mean),.0001)
global medi_gain2 = round(r(p50),.0001)

* mean
preserve
	replace gain = -59.9 if gain <= -60
	replace gain = 19.9 if gain >= 20
	twoway (histogram gain if gain >= -60 & gain <= 20, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain if gain >= -60 & gain <= 20, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_gain, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $mean_gain 0.816 $mean_gain, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.816 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uplus_mean_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uplus_mean_$simulation", replace
restore

* median
preserve
	replace gain = -59.9 if gain <= -60
	replace gain = 19.9 if gain >= 20
	twoway (histogram gain if gain >= -60 & gain <= 20, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain if gain >= -60 & gain <= 20, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-60(20)20 20 ">= 20" -60 "<= -60", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_gain, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $medi_gain 0.816 $medi_gain, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.816 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uplus_medi_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uplus_medi_$simulation", replace
restore

* mean (normalized)
preserve
	replace gain2 = -0.799 if gain2 <= -0.8
	replace gain2 = 0.199 if gain2 >= 0.2
	twoway (histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($mean_gain2, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $mean_gain2 0.612 $mean_gain2, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.612 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uplus_mean2_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uplus_mean2_$simulation", replace
restore

* median (normalized)
preserve
	replace gain2 = -0.799 if gain2 <= -0.8
	replace gain2 = 0.199 if gain2 >= 0.2
	twoway (histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac scheme(s1manual) bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(none) lwidth(none) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12))) ///
		(histogram gain2 if gain2 >= -0.8 & gain2 <= 0.2, frac bin(100) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
		ytitle("{bf:Fraction}", size(medium)) xtitle("{bf:WTP Gains From Misreporting}" "{bf:Measured by WTP Gain Relative True WTP for the Treatment} (Percentage)", size(medsmall)) ///
		yscale(lwidth(medium) lcolor(gs5)) xscale(lwidth(medium) lcolor(gs5))  ///
		fcolor(gs16) lwidth(vthin) lcolor(gs0)    ///
		xlabel(-0.8(0.2)0.2 0.2 ">= 20%" 0 "0%" -0.2 "-20%" -0.4 "-40%" -0.6 "-60%" -0.8 "<= -80%", valuelabels labsize(small)) ylabel(#5, format(%9.0g) labsize(vsmall) grid glwidth(thin) glcolor(gs12)) ///
		xmlabel($medi_gain2, nolabels labsize(small) add custom labcolor(red) tlcolor(red))) ///
		(pcarrowi 0 $medi_gain2 0.612 $medi_gain2, lpattern(solid) lc(red) lwidth(medthin) mcolor(none)) ///
		(pcarrowi 0 0 0.612 0 , lpattern(dash) lc(blue) lwidth(medthin) mcolor(none)) ///
		, legend(order (4 3) label(4 "RCT") label(3 "EXAM") position(6) symysize(*.5) forces) 

	graph export "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uplus_medi2_$simulation.png", replace
	graph save "$EXaM_output/08_EXaMvsRCT_incentivegain_mani100uplus_medi2_$simulation", replace
restore


*	6. additional table
********************************************************************************
use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani10_$simulation.dta", clear
keep gain
collapse (p95) p95=gain (p96) p96=gain (p97) p97=gain (p98) p98=gain (p99) p99=gain (max) max=gain
save "$EXaM_temp/08_figure5_mani10.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100_$simulation.dta", clear
keep gain
collapse (p95) p95=gain (p96) p96=gain (p97) p97=gain (p98) p98=gain (p99) p99=gain (max) max=gain
save "$EXaM_temp/08_figure5_mani100.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani1000_$simulation.dta", clear
keep gain
collapse (p95) p95=gain (p96) p96=gain (p97) p97=gain (p98) p98=gain (p99) p99=gain (max) max=gain
save "$EXaM_temp/08_figure5_mani1000.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100uminus_$simulation.dta", clear
keep gain
collapse (p95) p95=gain (p96) p96=gain (p97) p97=gain (p98) p98=gain (p99) p99=gain (max) max=gain
save "$EXaM_temp/08_figure5_mani100uminus.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100uplus_$simulation.dta", clear
keep gain
collapse (p95) p95=gain (p96) p96=gain (p97) p97=gain (p98) p98=gain (p99) p99=gain (max) max=gain
save "$EXaM_temp/08_figure5_mani100uplus.dta", replace

* figure a
use "$EXaM_temp/08_figure5_mani100.dta", replace
* figure b
append using "$EXaM_temp/08_figure5_mani1000.dta"
* figure c
append using "$EXaM_temp/08_figure5_mani100uplus.dta"
* figure d
append using "$EXaM_temp/08_figure5_mani100uminus.dta"
* label
label var p95 "95 percentile"
label var p96 "96 percentile"
label var p97 "97 percentile"
label var p98 "98 percentile"
label var p99 "99 percentile"
label var max "Max"
export excel using "$EXaM_output/08_figure5.xls", replace firstrow(varlabels)

* Normalized by wtp_real
use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani10_$simulation.dta", clear
keep gain2
collapse (p95) p95=gain2 (p96) p96=gain2 (p97) p97=gain2 (p98) p98=gain2 (p99) p99=gain2 (max) max=gain2
save "$EXaM_temp/08_normalizedfigure5_mani10.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100_$simulation.dta", clear
keep gain2
collapse (p95) p95=gain2 (p96) p96=gain2 (p97) p97=gain2 (p98) p98=gain2 (p99) p99=gain2 (max) max=gain2
save "$EXaM_temp/08_normalizedfigure5_mani100.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani1000_$simulation.dta", clear
keep gain2
collapse (p95) p95=gain2 (p96) p96=gain2 (p97) p97=gain2 (p98) p98=gain2 (p99) p99=gain2 (max) max=gain2
save "$EXaM_temp/08_normalizedfigure5_mani1000.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100uminus_$simulation.dta", clear
keep gain2
collapse (p95) p95=gain2 (p96) p96=gain2 (p97) p97=gain2 (p98) p98=gain2 (p99) p99=gain2 (max) max=gain2
save "$EXaM_temp/08_normalizedfigure5_mani100uminus.dta", replace

use "$EXaM_temp/08_EXaM_vs_rct_incentive_mani100uplus_$simulation.dta", clear
keep gain2
collapse (p95) p95=gain2 (p96) p96=gain2 (p97) p97=gain2 (p98) p98=gain2 (p99) p99=gain2 (max) max=gain2
save "$EXaM_temp/08_normalizedfigure5_mani100uplus.dta", replace

* figure a
use "$EXaM_temp/08_normalizedfigure5_mani100.dta", replace
* figure b
append using "$EXaM_temp/08_normalizedfigure5_mani1000.dta"
* figure c
append using "$EXaM_temp/08_normalizedfigure5_mani100uplus.dta"
* figure d
append using "$EXaM_temp/08_normalizedfigure5_mani100uminus.dta"
* label
label var p95 "95 percentile"
label var p96 "96 percentile"
label var p97 "97 percentile"
label var p98 "98 percentile"
label var p99 "99 percentile"
label var max "Max"
export excel using "$EXaM_output/08_normalizedfigure5.xls", replace firstrow(varlabels)
