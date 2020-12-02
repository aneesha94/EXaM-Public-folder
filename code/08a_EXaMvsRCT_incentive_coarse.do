
*	Generating Manipualted CSVs (coarse)
********************************************************************************

	clear
	clear mata
	clear matrix
	set more off
	set mem 650m
	set maxvar 32767

	cd		"$EXaM"
	use		"$EXaM_temp/06_p_real_coarse_$simulation.dta", clear

	* Move "set seed" out of the loop, 20170910
	set seed 890
	
	
*	Draw WTP manipulations from N(true WTP, 100)
use	"$EXaM_temp/03_WTP_all_byhhid_coarse_$simulation.dta", clear

preserve
	keep hh_id draw_wdays*

	forval i = 1/$simulation {
		gen random_`i' = runiform()
		sort random_`i'
		replace draw_wdays_all_`i' = draw_wdays_all_`i' + rnormal(0, 100) in 1
		gen subjectno_`i' = hh_id in 1
		replace subjectno_`i' = subjectno_`i'[_n-1] if _n != 1
			
	}
	save "$EXaM_temp/08_WTP_with_manipulator100_coarse_$simulation.dta", replace

restore
preserve
	use "$EXaM_temp/08_WTP_with_manipulator100_coarse_$simulation.dta", clear

	keep subjectno*
	keep if _n == 1
	gen i = 1
	reshape long subjectno_, i(i) j(no)
	drop i

	save "$EXaM_temp/08_manipulator100_list_coarse_$simulation.dta", replace
	export delimited "$EXaM_temp/08_manipulator100_list_coarse_$simulation.csv", replace

restore

	use "$EXaM_temp/08_WTP_with_manipulator100_coarse_$simulation.dta", clear

	keep hh_id draw_wdays_all_*
	rename hh_id subject
	forval i = 1/$simulation {
		preserve
		keep subject draw_wdays_all_`i'
		rename draw_wdays_all_`i' WTP
		sort subject
		export delimited "$EXaM_temp/08_WTP100_`i'_coarse.csv", replace

		restore
	}


*	Draw WTP manipulations from N(true WTP, 10)
use	"$EXaM_temp/03_WTP_all_byhhid_coarse_$simulation.dta", clear

preserve
	keep hh_id draw_wdays*

	forval i = 1/$simulation {
		gen random_`i' = runiform()
		sort random_`i'
		replace draw_wdays_all_`i' = draw_wdays_all_`i' + rnormal(0, 10) in 1
		gen subjectno_`i' = hh_id in 1
		replace subjectno_`i' = subjectno_`i'[_n-1] if _n != 1
			
	}
	save "$EXaM_temp/08_WTP_with_manipulator10_coarse_$simulation.dta", replace

restore
preserve
	use "$EXaM_temp/08_WTP_with_manipulator10_coarse_$simulation.dta", clear

	keep subjectno*
	keep if _n == 1
	gen i = 1
	reshape long subjectno_, i(i) j(no)
	drop i

	save "$EXaM_temp/08_manipulator10_list_coarse_$simulation.dta", replace
	export delimited "$EXaM_temp/08_manipulator10_list_coarse_$simulation.csv", replace

restore

	use "$EXaM_temp/08_WTP_with_manipulator10_coarse_$simulation.dta", clear

	keep hh_id draw_wdays_all_*
	rename hh_id subject
	forval i = 1/$simulation {
		preserve
		keep subject draw_wdays_all_`i'
		rename draw_wdays_all_`i' WTP
		sort subject
		export delimited "$EXaM_temp/08_WTP10_`i'_coarse.csv", replace

		restore
	}


*	Draw WTP manipulations from N(true WTP, 1000)
use	"$EXaM_temp/03_WTP_all_byhhid_coarse_$simulation.dta", clear

preserve
	keep hh_id draw_wdays*

	forval i = 1/$simulation {
		gen random_`i' = runiform()
		sort random_`i'
		replace draw_wdays_all_`i' = draw_wdays_all_`i' + rnormal(0, 1000) in 1
		gen subjectno_`i' = hh_id in 1
		replace subjectno_`i' = subjectno_`i'[_n-1] if _n != 1
			
	}
	save "$EXaM_temp/08_WTP_with_manipulator1000_coarse_$simulation.dta", replace

restore
preserve
	use "$EXaM_temp/08_WTP_with_manipulator1000_coarse_$simulation.dta", clear

	keep subjectno*
	keep if _n == 1
	gen i = 1
	reshape long subjectno_, i(i) j(no)
	drop i

	save "$EXaM_temp/08_manipulator1000_list_coarse_$simulation.dta", replace
	export delimited "$EXaM_temp/08_manipulator1000_list_coarse_$simulation.csv", replace

restore

	use "$EXaM_temp/08_WTP_with_manipulator1000_coarse_$simulation.dta", clear

	keep hh_id draw_wdays_all_*
	rename hh_id subject
	forval i = 1/$simulation {
		preserve
		keep subject draw_wdays_all_`i'
		rename draw_wdays_all_`i' WTP
		sort subject
		export delimited "$EXaM_temp/08_WTP1000_`i'_coarse.csv", replace

		restore
	}
	
	
*	Draw WTP manipulations from true WTP + U(0, 100)
use	"$EXaM_temp/03_WTP_all_byhhid_coarse_$simulation.dta", clear

preserve
	keep hh_id draw_wdays*

	forval i = 1/$simulation {
		gen random_`i' = runiform()
		sort random_`i'
		replace draw_wdays_all_`i' = draw_wdays_all_`i' + runiform(0, 100) in 1
		gen subjectno_`i' = hh_id in 1
		replace subjectno_`i' = subjectno_`i'[_n-1] if _n != 1
			
	}
	save "$EXaM_temp/08_WTP_with_manipulator100uplus_coarse_$simulation.dta", replace

restore
preserve
	use "$EXaM_temp/08_WTP_with_manipulator100uplus_coarse_$simulation.dta", clear

	keep subjectno*
	keep if _n == 1
	gen i = 1
	reshape long subjectno_, i(i) j(no)
	drop i

	save "$EXaM_temp/08_manipulator100uplus_list_coarse_$simulation.dta", replace
	export delimited "$EXaM_temp/08_manipulator100uplus_list_coarse_$simulation.csv", replace

restore

	use "$EXaM_temp/08_WTP_with_manipulator100uplus_coarse_$simulation.dta", clear

	keep hh_id draw_wdays_all_*
	rename hh_id subject
	forval i = 1/$simulation {
		preserve
		keep subject draw_wdays_all_`i'
		rename draw_wdays_all_`i' WTP
		sort subject
		export delimited "$EXaM_temp/08_WTP100uplus_`i'_coarse.csv", replace

		restore
	}

	
*	Draw WTP manipulations from true WTP - U(0, 100)
use	"$EXaM_temp/03_WTP_all_byhhid_coarse_$simulation.dta", clear

preserve
	keep hh_id draw_wdays*

	forval i = 1/$simulation {
		gen random_`i' = runiform()
		sort random_`i'
		replace draw_wdays_all_`i' = draw_wdays_all_`i' - runiform(0, 100) in 1
		gen subjectno_`i' = hh_id in 1
		replace subjectno_`i' = subjectno_`i'[_n-1] if _n != 1
			
	}
	save "$EXaM_temp/08_WTP_with_manipulator100uminus_coarse_$simulation.dta", replace

restore
preserve
	use "$EXaM_temp/08_WTP_with_manipulator100uminus_coarse_$simulation.dta", clear

	keep subjectno*
	keep if _n == 1
	gen i = 1
	reshape long subjectno_, i(i) j(no)
	drop i

	save "$EXaM_temp/08_manipulator100uminus_list_coarse_$simulation.dta", replace
	export delimited "$EXaM_temp/08_manipulator100uminus_list_coarse_$simulation.csv", replace

restore

	use "$EXaM_temp/08_WTP_with_manipulator100uminus_coarse_$simulation.dta", clear

	keep hh_id draw_wdays_all_*
	rename hh_id subject
	forval i = 1/$simulation {
		preserve
		keep subject draw_wdays_all_`i'
		rename draw_wdays_all_`i' WTP
		sort subject
		export delimited "$EXaM_temp/08_WTP100uminus_`i'_coarse.csv", replace

		restore
	}
