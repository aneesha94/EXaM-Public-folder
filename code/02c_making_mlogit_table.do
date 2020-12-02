cd "$EXaM_code\02b_estimatingWTP"


forval i = 1/4 {

	insheet using "02_col`i'.csv", clear 
	ren estimations estimations_`i'
	ren column column
	tempfile `i'
	save ``i'', replace 
}

use `1', clear 
forval i = 2/4 {
	merge 1:1 column using ``i'', gen(merge_`i') 
}

gen sno =_n

replace sno = 1 if column == "Spring protection treatment indicator Mean"
replace sno = 2 if column == "Spring protection treatment indicator Mean(sd)"
replace sno = 3 if column == "Spring protection treatment indicator S.D."
replace sno = 4 if column == "Spring protection treatment indicator S.D.(sd)"
replace sno = 5  if column == "Treatment * latrine density"
replace sno = 6  if column == "Treatment * latrine density(s.d.)"
replace sno = 7  if column == "Treatment * diarrhea prevention"
replace sno = 8  if column == "Treatment * diarrhea prevention(s.d.)"
replace sno = 9  if column == "Treatment * mother education"
replace sno = 10 if column == "Treatment * mother education(s.d.)"
replace sno = 11 if column == "Distance to source, minutes walk Mean"
replace sno = 12 if column == "Distance to source, minutes walk Mean(sd)"
replace sno = 13 if column == "Distance to source, minutes walk S.D."
replace sno = 14 if column == "Distance to source, minutes walk S.D.(sd)"
replace sno = 15 if column == "Source type: borehole/piped"
replace sno = 16 if column == "Source type: borehole/piped(s.d.)"
replace sno = 17 if column == "Source type: well"
replace sno = 18 if column == "Source type: well(s.d.)"
replace sno = 19 if column == "Source type: stream/river"
replace sno = 20 if column == "Source type: stream/river(s.d.)"
replace sno = 21 if column == "Source type: lake/pond"
replace sno = 22 if column == "Source type: lake/pond(s.d.)"
replace sno = 23 if column == "Number of observations"

sort sno


forval i = 1/4 {
	replace estimations_`i' = round(estimations_`i', 0.001)
}

cap drop merge_*

order column estimations_1 estimations_2 estimations_3 estimations_4

tostring estimations_*, replace
format estimations_1 %9.3f

replace column = "" if sno == 2 | sno == 4 | sno == 6 | sno == 8 | sno == 10 | sno == 12 | sno == 14 | sno == 16 | sno == 18 | sno == 20 | sno== 22




forval i = 1/4 {
	tostring estimations_`i', gen(estimations_`i'_str) format(%9.3f) force
	replace estimations_`i'_str = "(" + estimations_`i'_str + ")" if sno == 2 | sno == 4 | sno == 6 | sno == 8 | sno == 10 | sno == 12 | sno == 14 | sno == 16 | sno == 18 | sno == 20 | sno== 22
	replace estimations_`i'_str = "" if estimations_`i'_str == "." | estimations_`i'_str == "(.)"
	replace estimations_`i'_str = substr(estimations_`i'_str, 1, 5) if sno == 23
	replace estimations_`i'_str = substr(estimations_`i'_str, 1, 2) + "," + substr(estimations_`i'_str, 3, .) if sno == 23
	
	}

keep column estimations_*_str sno


forval i = 1/4 {
		replace estimations_`i'_str = estimations_`i'_str + "***" if estimations_`i'_str != "" &  (sno == 1 | sno == 3 | sno == 5 | sno == 7 | sno == 9 | sno == 11 | sno == 13 | sno == 15 | sno == 17 | sno == 19 )
}

replace sno = sno + 1
set obs 24
replace sno = 1 if sno == .
sort sno
replace column = "Sping protection treatment indicator (Normal)" if sno == 1 


replace sno = sno + 1 if sno >= 12
set obs 25
replace sno = 12 if sno == .
sort sno
replace column = "Distance to source, minutes walk (Restrcited triangular)" if sno == 12
replace column = "Mean" if column == "Spring protection treatment indicator Mean"
replace column = "Standard Deviation" if column == "Spring protection treatment indicator S.D."
replace column = "Mean" if column == "Distance to source, minutes walk Mean"
replace column = "Standard Deviation" if column == "Distance to source, minutes walk S.D."

lab var estimations_4_str "Main"
lab var estimations_1_str "Estimations"
texsave column  estimations_1 estimations_2 estimations_3 estimations_4 using "$EXaM_output/02_mlogitreg_WTP_tableformat.tex", frag  replace autonumber varlabels sw nofix size(1) title("") hlines(24 24) 









