
*	Graphing Joint Distributions of HTE and WTP scatter plot
********************************************************************************

	clear
	clear mata
	clear matrix
	set more off
	set mem 650m
	set maxvar 32767

	cd		"$EXaM"
	global value_time_25=371/260/8/60*(0.25)
	
	foreach wtpvar in wdays usdsp usd25 {
	foreach x in lat dia edu all {
		preserve
		use "$EXaM_temp/03_WTP_`wtpvar'_`x'_$simulation.dta", clear
		drop hh_id no
		gen n = _n
		save "$EXaM_temp/05_WTP_`wtpvar'_`x'_$simulation.dta", replace
		restore
	}
	}

	foreach htevar in dia wei bmi {
	foreach x in lat dia edu all {
		preserve
		use "$EXaM_temp/04_HTE_`htevar'_`x'_$simulation.dta", clear
		drop hh_id no
		gen n = _n
		save "$EXaM_temp/05_HTE_`htevar'_`x'_$simulation.dta", replace
		restore
	}
	}


foreach x in lat dia edu all {
foreach htevar in dia wei bmi {
foreach wtpvar in wdays usdsp usd25 {
	
	preserve
	
		use "$EXaM_temp/05_WTP_`wtpvar'_`x'_$simulation.dta", clear
		merge 1:1 n using "$EXaM_temp/05_HTE_`htevar'_`x'_$simulation.dta", nogen
		save "$EXaM_temp/05_WTP_HTE_`wtpvar'_`htevar'_`x'_$simulation.dta", replace
	restore
}
}
}


*	Graph histograms
********************************************************************************

*	twoway lfit messed up the y-axis' title. Use line instead.
foreach x in lat dia edu all  {
foreach htevar in dia wei bmi {
foreach wtpvar in wdays usdsp usd25 {
	
	preserve
	
		use "$EXaM_temp/05_WTP_HTE_`wtpvar'_`htevar'_`x'_$simulation.dta", clear
		sum draw_`wtpvar'_`x'_
		gen mean_`wtpvar' = r(mean)
		gen sd_`wtpvar' = r(sd)

		sum draw_`htevar'_`x'_, det
		gen mean_`htevar' = r(mean)
		gen sd_`htevar' = r(sd)
		global minyrange = round(r(min) , 10^round(log10(-1*r(min))) )
		global maxyrange = round(r(p75) , 10^round(log10(r(p75))) )
		
		local a: variable label draw_`wtpvar'_`x'_
		local a: subinstr local a " (`x')" ""
		label var draw_`wtpvar'_`x'_ "`a'" 

		local b: variable label draw_`htevar'_`x'_
		local b: subinstr local b " (`x')" ""
		label var draw_`htevar'_`x'_ "`b'" 
		
	
*		sort draw_`wtpvar'_`x'_
		reg draw_`htevar'_`x'_ draw_`wtpvar'_`x'_ if abs(draw_`wtpvar'_`x'_  - mean_`wtpvar') <= 3* sd_`wtpvar'
		global breg = round( _b[draw_`wtpvar'_`x'_], 0.000001)
		global r2reg = round(e(r2), 0.000001)
		
		scatter draw_`htevar'_`x'_ draw_`wtpvar'_`x'_ if abs(draw_`wtpvar'_`x'_  - mean_`wtpvar') <= 3* sd_`wtpvar', mcolor(gs6) msize(vtiny) ///
			scheme(s1manual) graphregion(lcolor(none)) plotregion(style(none) margin(small))  ///
			xtitle( , size(medium) color(gs0)) legend(off) ///
			yscale(range($minyrange $maxyrange) lwidth(medium) lcolor(gs5) fextend) xscale(lwidth(medium) lcolor(gs8) fextend) ///
			xlabel(#8, labsize(small)) || ///
		lfit draw_`htevar'_`x'_ draw_`wtpvar'_`x'_ if abs(draw_`wtpvar'_`x'_  - mean_`wtpvar') <= 3* sd_`wtpvar', ///
			lcolor(gs0) lwidth(medium) ylabel(#4, format(%9.0g) labsize(small) grid glwidth(thin) glcolor(gs12)) ///
			ytitle( `b' , size(medium) color(gs0)) caption("Fitted line has a slope of $breg , R2 = $r2reg ",size(medsmall)) note(" ", size(small)) 
		graph export "$EXaM_output/05_WTP_HTE_joint_`wtpvar'_`htevar'_`x'_$simulation.png", replace
		graph save "$EXaM_output/05_WTP_HTE_joint_`wtpvar'_`htevar'_`x'_$simulation", replace
		
	restore
}
}
}

*heat map for dia wdays, method grid
	
		use "$EXaM_temp/05_WTP_HTE_wdays_dia_all_$simulation.dta", clear
		
		sum draw_wdays_all_, det
		gen mean_wdays = r(mean)
		gen sd_wdays = r(sd)
	
	  
		global minxrange = round(r(p10) , 10^round(log10(-1*r(p10))) )
		global maxxrange = round(r(p75) , 10^round(log10(r(p75))) )
		keep if abs(draw_wdays_all_  - mean_wdays) <= 3* sd_wdays
		sum draw_dia_all_, det
		gen mean_dia = r(mean)
		gen sd_dia = r(sd)
		global minyrange = round(r(min) , 10^round(log10(-r(min))) )
		global maxyrange = round(r(p75) , 10^round(log10(r(p75))) )
		
	    keep draw_wdays_all_ draw_dia_all_
		save "$EXaM_temp/draw_wdays_dia_all_heat.dta", replace
		*save "$EXaM_temp/05_WTP_HTE_wdays_dia_all_$simulation_heat.dta",replace
		spgrid, shape(sq) xdim(100)  ///
		xrange($minxrange $maxxrange) yrange($minyrange $maxyrange ) ///
		dots replace ///
		cells("$EXaM_temp/draw_wdays_diaGridCells.dta")         ///
        points("$EXaM_temp/draw_wdays_diaGridPoints.dta")
		
		spkde using "$EXaM_temp/draw_wdays_diaGridPoints.dta",   ///
        xcoord(draw_wdays_all_) ycoord(draw_dia_all_)              ///
        bandwidth(fbw) fbw(1) dots     ///
        saving("$EXaM_temp/2D-draw_wdays_dia.dta", replace)
		
		use "$EXaM_temp/2D-draw_wdays_dia.dta", clear
		
		local a: variable label spgrid_xcoord 
		local a: subinstr local a " (all)" ""
		label var spgrid_xcoord  "`a'" 

		local b: variable label spgrid_ycoord
		local b: subinstr local b " (all)" ""
		label var spgrid_ycoord "`b'" 
		
		reg spgrid_ycoord spgrid_xcoord 
		global breg = round( _b[spgrid_xcoord], 0.000001)
		global r2reg = round(e(r2), 0.000001)
		
		twoway contour lambda spgrid_ycoord spgrid_xcoord,  ///
			scheme(s1manual) graphregion(lcolor(none)) plotregion(style(none) margin(small)) ///
			xtitle("WTP Measured by Time Cost of Water Collection (unit: workdays)" , size(small) color(gs0)) legend(off) ytitle("Treatment Effect Measured by % Reduction in Child Diarrhea", size(small) color(gs0)) ztitle("# of Simulation Observations per Grid", size(small) color(gs0)) ///
			xscale(range($minxrange $maxxrange) lwidth(medium) lcolor(gs5) fextend) yscale(lwidth(small) lcolor(gs8) fextend) ///
			xlabel(#8, labsize(small)) zlabel(#8, labsize(small))||
		graph export "$EXaM_output/05_WTP_HTE_joint_heat_wdays_dia_all_$simulation.png", replace
		graph save "$EXaM_output/05_WTP_HTE_joint_heat_wdays_dia_all_$simulation", replace
		


*	Simulation summary statistics
********************************************************************************

clear
global o = $simulation * 1540
set obs $o
gen n = _n

foreach x in lat dia edu all {

	merge 1:1 n using "$EXaM_temp/05_WTP_wdays_`x'_$simulation.dta", nogen
	merge 1:1 n using "$EXaM_temp/05_WTP_usdsp_`x'_$simulation.dta", nogen
	merge 1:1 n using "$EXaM_temp/05_WTP_usd25_`x'_$simulation.dta", nogen
	
	merge 1:1 n using "$EXaM_temp/05_HTE_dia_`x'_$simulation.dta", nogen
	merge 1:1 n using "$EXaM_temp/05_HTE_wei_`x'_$simulation.dta", nogen
	merge 1:1 n using "$EXaM_temp/05_HTE_bmi_`x'_$simulation.dta", nogen
	
}

order _all, alphabetic
order draw_wei_*, before(draw_bmi_all)
order draw_dia_*, before(draw_wei_all)
order draw_usdsp_*, before(draw_usd25_all)
order draw_wdays_*, before(draw_usdsp_all)

foreach i of varlist _all {
	local a: variable label `i'
	local a: subinstr local a "{bf:" ""
	local a: subinstr local a "}" ""
	label var `i' "`a'" 
}
rename draw_* *
save	"$EXaM_temp/05_WTP_HTE_joint_summarystats_$simulation.dta", replace
outreg2 using "$EXaM_output/05_simulation_summary_statistics_$simulation.doc", excel tex(frag) label replace sum(detail) eqkeep(mean p25 p50 p75 sd)
