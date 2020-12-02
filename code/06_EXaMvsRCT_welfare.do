
*	Graphing EXaM vs RCT comparison in terms of welfare (WTP and HTE) (coarse)
********************************************************************************

	clear
	clear mata
	clear matrix
	set more off
	set mem 650m
	set maxvar 32767

	cd		"$EXaM"

*	Eaw welfare simulation	
	import delimited using "$EXaM_input/df_results.csv", varn(1) clear
	
	
	local j = $simulation + 1
	forval i = 2/`j' {
		replace v`i' = subinstr(v`i', "[", "", .)
		replace v`i' = substr(v`i', 1, strpos(v`i',",")-1)
		destring v`i', replace
		local h = `i' - 1
		rename v`i' p_real_`h'
	}
	rename v1 subject
	save "$EXaM_temp/06_p_real_$simulation.dta", replace
	
	
preserve
	use		"$EXaM_temp/03_WTP_all_byhhid_$simulation.dta", clear
	gen		subject = _n - 1
	save	"$EXaM_temp/06_WTP_all_byhhid_subject_$simulation.dta", replace

	use		"$EXaM_temp/04_HTE_all_byhhid_coarse_$simulation.dta", clear
	gen		subject = _n - 1
	save	"$EXaM_temp/06_HTE_all_byhhid_subject_coarse_$simulation.dta", replace

restore
	
	merge 1:1 subject using "$EXaM_temp/06_WTP_all_byhhid_subject_$simulation.dta", nogen
	merge 1:1 subject using "$EXaM_temp/06_HTE_all_byhhid_subject_coarse_$simulation.dta", nogen
	

*	Generate RCT and EXaM welfare values
*	PTE is multiplied by -1 because unit is changed to reduction
forval i = 1/$simulation {
	capture gen WTP_EXaM_`i' = draw_wdays_all_`i' * (1-p_real_`i')
	capture gen WTP_RCT_`i' = draw_wdays_all_`i' * 663/1540
	capture gen HTE_EXaM_`i' = draw_dia_all_`i' * (1-p_real_`i') * -1
	capture gen HTE_RCT_`i' = draw_dia_all_`i' * 663/1540 * -1
}


*	Draw Histograms
	preserve
		* WTP
		keep subject WTP_*
		reshape long WTP_EXaM_ WTP_RCT_, i(subject) j(no)
		sum WTP_EXaM_, det
		global mean_EXaM_WTP = round(r(mean),.1)
		global medi_EXaM_WTP = round(r(p50),.1)
		global stdev_EXaM_WTP = r(sd)
		sum WTP_RCT_, det
		global mean_RCT_WTP = round(r(mean),.1)
		global medi_RCT_WTP = round(r(p50),.1)
		global stdev_RCT_WTP = r(sd)
		save "$EXaM_temp/06_WTP_EXaM_$simulation.dta", replace
		use "$EXaM_temp/06_WTP_EXaM_$simulation.dta", clear
		replace WTP_EXaM_ = . if WTP_EXaM >= -0.745 & WTP_EXaM <= -0.01  // Chop off fraction above 0.08
		twoway (histogram WTP_RCT_ if WTP_RCT_ >= -20 & WTP_RCT_ <= 60, frac scheme(s1manual) width(0.75) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:WTP Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium) color(gs0)) ///
			xline($medi_RCT_WTP, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Fraction}", size(medium)) yscale( lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(0(0.02)0.08 0.08 ">= .08", format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(histogram WTP_EXaM_ if WTP_EXaM_ >= -20 & WTP_EXaM_ <= 60, frac width(0.75) ///
			xline($medi_EXaM_WTP, lc(red) lpattern(solid) lwidth(medthin)) ///
			fcolor(none) lwidth(thin) lcolor(gs0)), legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_WTP_medi_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_WTP_medi_$simulation", replace
		
		twoway (histogram WTP_RCT_ if WTP_RCT_ >= -20 & WTP_RCT_ <= 60, frac scheme(s1manual) width(0.75) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:WTP Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium) color(gs0)) ///
			xline($medi_RCT_WTP, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Fraction}", size(medium)) yscale( lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(0(0.02)0.08 0.08 ">= .08", format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(histogram WTP_EXaM_ if WTP_EXaM_ >= -20 & WTP_EXaM_ <= 60, frac width(0.75) ///
			fcolor(none) lwidth(none)), legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_RCT_welfare_WTP_medi_$simulation.png", replace
		graph save "$EXaM_output/06_RCT_welfare_WTP_medi_$simulation", replace
		
		twoway (histogram WTP_RCT_ if WTP_RCT_ >= -20 & WTP_RCT_ <= 60, frac scheme(s1manual) width(0.75) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:WTP Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium) color(gs0)) ///
			xline($mean_RCT_WTP, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Fraction}", size(medium)) yscale( lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(0(0.02)0.08 0.08 ">= .08", format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(histogram WTP_EXaM_ if WTP_EXaM_ >= -20 & WTP_EXaM_ <= 60, frac width(0.75) ///
			xline($mean_EXaM_WTP, lc(red) lpattern(solid) lwidth(medthin)) ///
			fcolor(none) lwidth(thin) lcolor(gs0)), legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_WTP_mean_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_WTP_mean_$simulation", replace
		
		twoway (histogram WTP_RCT_ if WTP_RCT_ >= -20 & WTP_RCT_ <= 60, frac scheme(s1manual) width(0.75) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:WTP Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium) color(gs0)) ///
			xline($mean_RCT_WTP, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Fraction}", size(medium)) yscale( lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(0(0.02)0.08 0.08 ">= .08", format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(histogram WTP_EXaM_ if WTP_EXaM_ >= -20 & WTP_EXaM_ <= 60, frac width(0.75) ///
			fcolor(none) lwidth(none)), legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_RCT_welfare_WTP_mean_$simulation.png", replace
		graph save "$EXaM_output/06_RCT_welfare_WTP_mean_$simulation", replace

	restore
	
	preserve
		* HTE
		keep subject HTE_*
		reshape long HTE_EXaM_ HTE_RCT_, i(subject) j(no)
		replace HTE_EXaM_ = HTE_EXaM_ * 100
		replace HTE_RCT_ = HTE_RCT_ * 100
		sum HTE_EXaM_, det
		global mean_EXaM_HTE = round(r(mean),.1)
		global medi_EXaM_HTE = round(r(p50),.1)
		global stdev_EXaM_HTE = r(sd)
		sum HTE_RCT_, det
		global mean_RCT_HTE = round(r(mean),.1)
		global medi_RCT_HTE = round(r(p50),.1)
		global stdev_RCT_HTE = r(sd)
		save "$EXaM_temp/06_HTE_EXaM_coarse_$simulation.dta", replace
		use "$EXaM_temp/06_HTE_EXaM_coarse_$simulation.dta", clear
		twoway (histogram HTE_RCT_ if HTE_RCT_ >= 0 & HTE_RCT_ <= 8, frac scheme(s1manual) width(0.085) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:TE Measured by % Reduction in Child Diarrhea}", size(medium) color(gs0)) ///
			xline($medi_RCT_HTE, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Fraction}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(histogram HTE_EXaM_ if HTE_EXaM_ >= 0 & HTE_EXaM_ <= 8, frac width(0.085) ///
			xline($medi_EXaM_HTE, lc(red) lpattern(solid) lwidth(medthin)) ///
			fcolor(none) lwidth(thin) lcolor(gs0)), legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_HTE_medi_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_HTE_medi_$simulation", replace
		
		twoway (histogram HTE_RCT_ if HTE_RCT_ >= 0 & HTE_RCT_ <= 8, frac scheme(s1manual) width(0.085) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:TE Measured by % Reduction in Child Diarrhea}", size(medium) color(gs0)) ///
			xline($medi_RCT_HTE, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Fraction}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(histogram HTE_EXaM_ if HTE_EXaM_ >= 0 & HTE_EXaM_ <= 8, frac width(0.085) ///
			fcolor(none) lwidth(none)), legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_RCT_welfare_HTE_medi_$simulation.png", replace
		graph save "$EXaM_output/06_RCT_welfare_HTE_medi_$simulation.png", replace

		twoway (histogram HTE_RCT_ if HTE_RCT_ >= 0 & HTE_RCT_ <= 8, frac scheme(s1manual) width(0.085) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:TE Measured by % Reduction in Child Diarrhea}", size(medium) color(gs0)) ///
			xline($mean_RCT_HTE, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Fraction}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(histogram HTE_EXaM_ if HTE_EXaM_ >= 0 & HTE_EXaM_ <= 8, frac width(0.085) ///
			xline($mean_EXaM_HTE, lc(red) lpattern(solid) lwidth(medthin)) ///
			fcolor(none) lwidth(thin) lcolor(gs0)), legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_HTE_mean_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_HTE_mean_$simulation.png", replace
		
		twoway (histogram HTE_RCT_ if HTE_RCT_ >= 0 & HTE_RCT_ <= 8, frac scheme(s1manual) width(0.085) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:TE Measured by % Reduction in Child Diarrhea}", size(medium) color(gs0)) ///
			xline($mean_RCT_HTE, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Fraction}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(histogram HTE_EXaM_ if HTE_EXaM_ >= 0 & HTE_EXaM_ <= 8, frac width(0.085) ///
			fcolor(none) lwidth(none)), legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_RCT_welfare_HTE_mean_$simulation.png", replace
		graph save "$EXaM_output/06_RCT_welfare_HTE_mean_$simulation.png", replace
	
	restore


	
*	Draw CDFs
	preserve
		* WTP
		keep subject WTP_*
		reshape long WTP_EXaM_ WTP_RCT_, i(subject) j(no)
		sum WTP_EXaM_, det
		global mean_EXaM_WTP = round(r(mean),.1)
		global medi_EXaM_WTP = round(r(p50),.1)
		global stdev_EXaM_WTP = r(sd)
		sum WTP_RCT_, det
		global mean_RCT_WTP = round(r(mean),.1)
		global medi_RCT_WTP = round(r(p50),.1)
		global stdev_RCT_WTP = r(sd)
		save "$EXaM_temp/06_WTP_EXaM_$simulation.dta", replace
		*******Kolmogorov–Smirnov equality-of-distributions test
		use "$EXaM_temp/06_WTP_EXaM_$simulation.dta", clear
		gen id=_n
		rename WTP_RCT_ WTP1
		rename WTP_EXaM_ WTP2
		reshape long WTP, i(id) j(group)
		ksmirnov WTP, by(group)
		global Combined_K_S_wtp=round(r(D),0.001)
		global Combined_K_S_p_wtp=round(r(p),0.001)
		global Combined_K_S_pc_wtp=round(r(p cor),0.001)
		
		use "$EXaM_temp/06_WTP_EXaM_$simulation.dta", clear
		range N -170 50 5000
		kdensity WTP_RCT_, generate(a b) nograph at(N) bwidth(0.01)
		kdensity WTP_EXaM_, generate(c d) nograph at(N) bwidth(0.01)
		integ b a, gen(RCT)
		integ d c, gen(EXaM)
		drop if N <= 0
		drop if N >= 40
		twoway line RCT a, sort lpattern(dash) lc(blue) lwidth(medthin) ///
			xline($medi_RCT_WTP, lc(blue) lpattern(dash) lwidth(thin)) ///
			scheme(s1manual) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
			xtitle("{bf:E(WTP) Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium) color(gs0)) title("Cumulative Distribution Functions") ///
			ylabel(0(0.5)1) ///
			xlabel(0(5)40, valuelabels labsize(small)) ///
			xmlabel($medi_RCT_WTP, nolabels labsize(small) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_EXaM_WTP, nolabels labsize(small) add custom labcolor(red) tlcolor(red))|| //////
			(line EXaM c, sort lpattern(solid) lc(red) lwidth(medthin) ///
			xline($medi_EXaM_WTP, lc(red) lpattern(solid) lwidth(thin))),note("Kolmogorov–Smirnov test: D = $Combined_K_S_wtp , p-value = $Combined_K_S_p_wtp ") legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces)
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_WTP_CDF_medi_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_WTP_CDF_medi_$simulation", replace
		
		twoway line RCT a, sort lpattern(dash) lc(blue) lwidth(medthin) ///
			xline($mean_RCT_WTP, lc(blue) lpattern(dash) lwidth(thin)) ///
			scheme(s1manual) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
			xtitle("{bf:E(WTP) Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium) color(gs0)) title("Cumulative Distribution Functions") ///
			ylabel(0(0.5)1) ///
			xlabel(0(5)40, valuelabels labsize(small)) ///
			xmlabel($mean_RCT_WTP, nolabels labsize(small) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_EXaM_WTP, nolabels labsize(small) add custom labcolor(red) tlcolor(red))|| //////
			(line EXaM c, sort lpattern(solid) lc(red) lwidth(medthin) ///
			xline($mean_EXaM_WTP, lc(red) lpattern(solid) lwidth(thin))),note("Kolmogorov–Smirnov test: D = $Combined_K_S_wtp , p-value = $Combined_K_S_p_wtp ") legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces)
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_WTP_CDF_mean_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_WTP_CDF_mean_$simulation", replace
		

		* WTP RCT
		twoway line RCT a, sort lpattern(dash) lc(blue) lwidth(medthin) ///
			xline($medi_RCT_WTP, lc(blue) lpattern(dash) lwidth(thin)) ///
			scheme(s1manual) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
			xtitle("{bf:E(WTP) Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium) color(gs0)) title("Cumulative Distribution Functions") ///
			ylabel(0(0.5)1) ///
			xlabel(0(5)40, valuelabels labsize(small)) ///
			xmlabel($medi_RCT_WTP, nolabels labsize(small) add custom labcolor(blue) tlcolor(blue))|| //////
			(line EXaM c, sort lpattern(solid) lc(red) lwidth(none) ///
			xline($medi_EXaM_WTP, lc(red) lpattern(solid) lwidth(none))), legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces)
		graph export "$EXaM_output/06_RCT_welfare_WTP_CDF_medi_$simulation.png", replace
		graph save "$EXaM_output/06_RCT_welfare_WTP_CDF_medi_$simulation", replace
		
		twoway line RCT a, sort lpattern(dash) lc(blue) lwidth(medthin) ///
			xline($mean_RCT_WTP, lc(blue) lpattern(dash) lwidth(thin)) ///
			scheme(s1manual) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
			xtitle("{bf:E(WTP) Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium) color(gs0)) title("Cumulative Distribution Functions") ///
			ylabel(0(0.5)1) ///
			xlabel(0(5)40, valuelabels labsize(small)) ///
			xmlabel($mean_RCT_WTP, nolabels labsize(small) add custom labcolor(blue) tlcolor(blue))|| //////
			(line EXaM c, sort lpattern(solid) lc(red) lwidth(none) ///
			xline($mean_EXaM_WTP, lc(red) lpattern(solid) lwidth(none))), legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces)
		graph export "$EXaM_output/06_RCT_welfare_WTP_CDF_mean_$simulation.png", replace
		graph save "$EXaM_output/06_RCT_welfare_WTP_CDF_mean_$simulation", replace
	restore
	
	preserve
		* HTE
		use "$EXaM_temp/06_HTE_EXaM_coarse_$simulation.dta", clear
        gen id=_n
		rename HTE_RCT_ HTE1
		rename HTE_EXaM_ HTE2
		reshape long HTE, i(id) j(group)
		ksmirnov HTE, by(group)
		global Combined_K_S_HTE=round(r(D),0.001)
		global Combined_K_S_p_HTE=round(r(p),0.001)
		global Combined_K_S_pc_HTE=round(r(p cor),0.001)
 		
		use "$EXaM_temp/06_HTE_EXaM_coarse_$simulation.dta", clear
		range N -20 6 4000
		kdensity HTE_RCT_, generate(a b) nograph at(N) bwidth(0.002)
		kdensity HTE_EXaM_, generate(c d) nograph at(N) bwidth(0.002)
		integ b a, gen(RCT)
		integ d c, gen(EXaM)
		drop if N <= 0
		drop if N >= 5
		twoway line RCT a, sort lpattern(dash) lc(blue) lwidth(medthin) ///
			xline($medi_RCT_HTE, lc(blue) lpattern(dash) lwidth(thin)) ///
			scheme(s1manual) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
			xtitle("{bf:E(Predicted Effects) Measured by % Reduction in Child Diarrhea}", size(medium) color(gs0)) title("Cumulative Distribution Functions") ///
			ylabel(0(0.5)1) ///
			xlabel(0(1)5 , valuelabels labsize(small)) ///
			xmlabel($medi_RCT_HTE, nolabels labsize(small) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($medi_EXaM_HTE, nolabels labsize(small) add custom labcolor(red) tlcolor(red))|| //////
			(line EXaM c, sort lpattern(solid) lc(red) lwidth(medthin) ///
			xline($medi_EXaM_HTE, lc(red) lpattern(solid) lwidth(thin))), note("Kolmogorov–Smirnov test: D= $Combined_K_S_HTE , p value= $Combined_K_S_p_HTE ") legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces)
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_HTE_CDF_medi_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_HTE_CDF_medi_$simulation", replace

		twoway line RCT a, sort lpattern(dash) lc(blue) lwidth(medthin) ///
			xline($mean_RCT_HTE, lc(blue) lpattern(dash) lwidth(thin)) ///
			scheme(s1manual) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
			xtitle("{bf:E(Predicted Effects) Measured by % Reduction in Child Diarrhea}", size(medium) color(gs0)) title("Cumulative Distribution Functions") ///
			ylabel(0(0.5)1) ///
			xlabel(0(1)5 , valuelabels labsize(small)) ///
			xmlabel($mean_RCT_HTE, nolabels labsize(small) add custom labcolor(blue) tlcolor(blue)) ///
			xmlabel($mean_EXaM_HTE, nolabels labsize(small) add custom labcolor(red) tlcolor(red))|| //////
			(line EXaM c, sort lpattern(solid) lc(red) lwidth(medthin) ///
			xline($mean_EXaM_HTE, lc(red) lpattern(solid) lwidth(thin))), note("Kolmogorov–Smirnov test: D= $Combined_K_S_HTE , p value= $Combined_K_S_p_HTE ") legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces)
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_HTE_CDF_mean_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_HTE_CDF_mean_$simulation", replace

		* HTE RCT		
		twoway line RCT a, sort lpattern(dash) lc(blue) lwidth(medthin) ///
			xline($medi_RCT_HTE, lc(blue) lpattern(dash) lwidth(thin)) ///
			scheme(s1manual) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
			xtitle("{bf:E(Predicted Effects) Measured by % Reduction in Child Diarrhea}", size(medium) color(gs0)) title("Cumulative Distribution Functions") ///
			ylabel(0(0.5)1) ///
			xlabel(0(1)5, valuelabels labsize(small)) ///
			xmlabel($medi_RCT_HTE, nolabels labsize(small) add custom labcolor(blue) tlcolor(blue))|| ///
			(line EXaM c, sort lpattern(solid) lc(red) lwidth(none) ///
			xline($medi_EXaM_HTE, lc(red) lpattern(solid) lwidth(none))), legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces)
		graph export "$EXaM_output/06_RCT_welfare_HTE_CDF_medi_$simulation.png", replace
		graph save "$EXaM_output/06_RCT_welfare_HTE_CDF_medi_$simulation", replace


		twoway line RCT a, sort lpattern(dash) lc(blue) lwidth(medthin) ///
			xline($mean_RCT_HTE, lc(blue) lpattern(dash) lwidth(thin)) ///
			scheme(s1manual) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
			xtitle("{bf:E(Predicted Effects) Measured by % Reduction in Child Diarrhea}", size(medium) color(gs0)) title("Cumulative Distribution Functions") ///
			ylabel(0(0.5)1) ///
			xlabel(0(1)5, valuelabels labsize(small)) ///
			xmlabel($mean_RCT_HTE, nolabels labsize(small) add custom labcolor(blue) tlcolor(blue))|| ///
			(line EXaM c, sort lpattern(solid) lc(red) lwidth(none) ///
			xline($mean_EXaM_HTE, lc(red) lpattern(solid) lwidth(none))), legend(label(1 "RCT") label(2 "          ") position(6) symysize(*.5) forces)
		graph export "$EXaM_output/06_RCT_welfare_HTE_CDF_mean_$simulation.png", replace
		graph save "$EXaM_output/06_RCT_welfare_HTE_CDF_mean_$simulation", replace
		
		restore	


*	Draw Kernel Densities
	preserve
		* WTP
		keep subject WTP_*
		reshape long WTP_EXaM_ WTP_RCT_, i(subject) j(no)
		sum WTP_EXaM_, det
		global mean_EXaM_WTP = round(r(mean),.1)
		global medi_EXaM_WTP = round(r(p50),.1)
		global stdev_EXaM_WTP = r(sd)
		sum WTP_RCT_, det
		global mean_RCT_WTP = round(r(mean),.1)
		global medi_RCT_WTP = round(r(p50),.1)
		global stdev_RCT_WTP = r(sd)
		save "$EXaM_temp/06_WTP_EXaM_$simulation.dta", replace
		use "$EXaM_temp/06_WTP_EXaM_$simulation.dta", clear
		twoway (kdensity WTP_RCT_ if WTP_RCT_ >= -20 & WTP_RCT_ <= 60, scheme(s1manual) lc(gs0) lpattern(dash) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:WTP Measured by Time Cost of Water Collection} (Unit: Workdays)", size(medium) color(gs0)) ///
			xline($medi_RCT_WTP, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Density}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(kdensity WTP_EXaM_ if WTP_EXaM_ >= -20 & WTP_EXaM_ <= 60, lc(gs0) lpattern(solid) ///
			xline($medi_EXaM_WTP, lc(red) lpattern(solid) lwidth(medthin))), legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_WTP_density_medi_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_WTP_density_medi_$simulation", replace
		
		twoway (kdensity WTP_RCT_ if WTP_RCT_ >= -20 & WTP_RCT_ <= 60, scheme(s1manual) lc(gs0) lpattern(dash) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:WTP Measured by Time cost of Water Collection} (Unit: workdays)", size(medium) color(gs0)) ///
			xline($mean_RCT_WTP, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Density}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(kdensity WTP_EXaM_ if WTP_EXaM_ >= -20 & WTP_EXaM_ <= 60, lc(gs0) lpattern(solid) ///
			xline($mean_EXaM_WTP, lc(red) lpattern(solid) lwidth(medthin))), legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_WTP_density_mean_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_WTP_density_mean_$simulation", replace
	restore
	
	preserve
		* HTE
		keep subject HTE_*
		reshape long HTE_EXaM_ HTE_RCT_, i(subject) j(no)
		replace HTE_EXaM_ = HTE_EXaM_ * 100
		replace HTE_RCT_ = HTE_RCT_ * 100
		sum HTE_EXaM_, det
		global mean_EXaM_HTE = round(r(mean),.1)
		global medi_EXaM_HTE = round(r(p50),.1)
		global stdev_EXaM_HTE = r(sd)
		sum HTE_RCT_, det
		global mean_RCT_HTE = round(r(mean),.1)
		global medi_RCT_HTE = round(r(p50),.1)
		global stdev_RCT_HTE = r(sd)
		save "$EXaM_temp/06_HTE_EXaM_coarse_$simulation.dta", replace
		use "$EXaM_temp/06_HTE_EXaM_coarse_$simulation.dta", clear
		twoway (kdensity HTE_RCT_ if HTE_RCT_ >= 0 & HTE_RCT_ <= 8, scheme(s1manual) lpattern(dash) lc(gs0) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:TE Measured by % Reduction in Child Diarrhea}", size(medium) color(gs0)) ///
			xline($medi_RCT_HTE, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Density}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(kdensity HTE_EXaM_ if HTE_EXaM_ >= 0 & HTE_EXaM_ <= 8, lc(gs0) lpattern(solid) ///
			xline($medi_EXaM_HTE, lc(red) lpattern(solid) lwidth(medthin))), legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_HTE_density_medi_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_HTE_density_medi_$simulation.png", replace
		
		twoway (kdensity HTE_RCT_ if HTE_RCT_ >= 0 & HTE_RCT_ <= 8, scheme(s1manual) lpattern(dash) lc(gs0) graphregion(lcolor(none)) plotregion(style(none) margin(small)) xtitle("{bf:TE Measured by % Reduction in Child Diarrhea}", size(medium) color(gs0)) ///
			xline($mean_RCT_HTE, lc(blue) lpattern(dash) lwidth(medthin)) ///
			ytitle("{bf:Density}", size(medium)) yscale(lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs5) fextend)  fcolor(gs8) lwidth(thin) lcolor(gs8) ///
			xlabel(#8, labsize(small)) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12))) ///
			(kdensity HTE_EXaM_ if HTE_EXaM_ >= 0 & HTE_EXaM_ <= 8, lc(gs0) lpattern(solid) ///
			xline($mean_EXaM_HTE, lc(red) lpattern(solid) lwidth(medthin))), legend(label(1 "RCT") label(2 "EXAM") position(6) symysize(*.5) forces) 
		graph export "$EXaM_output/06_EXaMvsRCT_welfare_HTE_density_mean_$simulation.png", replace
		graph save "$EXaM_output/06_EXaMvsRCT_welfare_HTE_density_mean_$simulation", replace
	restore
