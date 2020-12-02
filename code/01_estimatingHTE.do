
* 	Heterogenous Treatment Effects OLS, based on Kremer et al Table IV Col 6
********************************************************************************

	clear
	clear mata
	clear matrix
	set more off
	set mem 650m
	set maxvar 32767

	cd		"$EXaM"

global balance_controls ="tarmac_0 slopereg_0 tcmpn_0 hhcount_0 ecmpn_0 butere_0 mumias_0 eccat_high_0 eccat_med_0"
use "$EXaM_input/reg_data_children_Aug2010.dta", clear


/*		The following specifications are Kremer's */
global sample "SIP"
global sround base_year ba04 ba05 
global tyear t04 t05
global year23 year_two_three 
global evertreat evertreat 
global numkids_05_vars numkids_05_dm ba_tc_numkids_05_dm
global numkids_05_vars_base ba_tc_numkids_05_d_base_dm
global numkids_012_vars numkids_012_dm ba_tc_numkids_012_dm
global numkids_012_vars_base ba_tc_numkids_012_d_base_dm
global agemonth_vars age06months age612months age1218months age1824months age2430months age3036months
global gender_agemonth_vars boy_age06months boy_age612months boy_age1218months boy_age1824months boy_age2430months boy_age3036months
global ageyear_vars age1year age2year age3year age4year age5year
global gender_ageyear_vars boy_age1year boy_age2year boy_age3year boy_age4year boy_age5year
global treat_diarrhea_base ba_tc_diarrhea_base
global treat_diarrhea_base_new ba_tc_diarrhea_base_new ba_tc_no_baseline_info
global age_interactions ba_tc_age06 ba_tc_age612 ba_tc_age1218 ba_tc_age1824 ba_tc_age2430 ba_tc_age3036 ba_tc_age3642  
global age_interactions_year ba_tc_age1year ba_tc_age2year ba_tc_age3year ba_tc_age4year ba_tc_age5year  
global months month2 month3 month4 month5 month6 month7 month8 month9 month10 month11 month12
global month_interactions ba_tc_month2 ba_tc_month3 ba_tc_month4 ba_tc_month5 ba_tc_month6 ba_tc_month7 ba_tc_month8 ///
	ba_tc_month9 ba_tc_month10 ba_tc_month11 ba_tc_month12 
global x_hh hygiene_know_base latrine_density_base boilh2oyester_base numkids_base momeduc_base e1_iron_roof_base iron_roof_density_base
global x_hhba_tc ba_tc_hygiene_know_base  ba_tc_latrine_density_base ba_tc_boilh2oyester_base ba_tc_numkids_base ///
	ba_tc_momeduc_base ba_tc_e1_iron_roof_base ba_tc_iron_roof_density_base
global x_hhba_tc_cov ba_tc_boilh2oyester_base ba_tc_numkids_base ///
	ba_tc_e1_iron_roof_base ba_tc_iron_roof_density_base
global xhhba_tc_interactions ba04_ba_tc_hygiene_know_base ba05_ba_tc_hygiene_know_base ///
	ba04_ba_tc_latrine_density_base ba05_ba_tc_latrine_density_base ///
	ba04_ba_tc_boilh2oyester_base ba05_ba_tc_boilh2oyester_base ///
	ba04_ba_tc_numkids_base ba05_ba_tc_numkids_base ///
	ba04_ba_tc_momeduc_base ba05_ba_tc_momeduc_base ///
	ba04_ba_tc_e1_iron_roof_base ba05_ba_tc_e1_iron_roof_base /// 
	ba04_ba_tc_iron_roof_den_base ba05_ba_tc_iron_roof_den_base ///
global x_hhba_tc_new ba_tc_hygiene_know_base  ba_tc_latrine_home_base ba_tc_boilh2oyester_base ba_tc_numkids_base ///
	ba_tc_momeduc_base ba_tc_e1_iron_roof_base
global xhhba_tc_interactions_new ba04_ba_tc_hygiene_know_base ba05_ba_tc_hygiene_know_base ///
	ba04_ba_tc_latrine_home_base ba05_ba_tc_latrine_home_base ///
	ba04_ba_tc_boilh2oyester_base ba05_ba_tc_boilh2oyester_base ///
	ba04_ba_tc_numkids_base ba05_ba_tc_numkids_base ///
	ba04_ba_tc_momeduc_base ba05_ba_tc_momeduc_base ///
	ba04_ba_tc_e1_iron_roof_base ba05_ba_tc_e1_iron_roof_base ///
global balance_controls ="tarmac_0 slopereg_0 tcmpn_0 hhcount_0 ecmpn_0 butere_0 mumias_0 eccat_high_0 eccat_med_0"


/*DEFINE SAMPLE OF INTEREST - copy of Kremer et al*/
*WE EXCLUDE ALL KIDS IN HOUSEHOLDS THAT RECEIVED WG IN HM3, AS WELL AS KIDS NOT AT ITT SPRINGS OR KIDS NOT IN THE HOUSEHOLD SAMPLE 
*CHILDREN ARE INCLUDED IN THE REGRESSION SAMPLE IF THEY ARE AGE 3 OR UNDER AT BASELINE OR AGE 3 OR UNDER WHEN THEY JOIN THE SAMPLE IN LATER ROUNDS 
*WE ALSO EXCLUDE CHILD-ROUND OBSERVATIONS WHERE THE ANTHROPOMETRIC OR AGE DATA IS FLAGGED AS HAVING A SERIOUS ERROR
*FINALLY, OBSERVATIONS MISSING DATA ON WHETHER THE HOUSEHOLD USES THE ICS SPRING EXCLUSIVELY OR USES MULTIPLE SPRINGS ARE ALSO EXCLUDED FROM THE REGRESSIONS
tab survey, m
keep if survey=="H" | survey=="Hm1" | survey=="Hm2" | survey=="Hm3"
drop if _c_id==.

*global child_sample="ba_tc_wg!=1 & _hh_sample==1 & problem_weight!=1 & problem_bmi!=1 & ITT==1 & multiusers_l_base~=. & baseage_forregs<=3 & flag_age==0 & height_outlier_severe!=1"
global child_sample="ba_tc_wg!=1 & ITT==1 & _hh_sample==1 & baseage_forregs<=3 & multiusers_l_base~=. & problem_weight!=1 & height_outlier_severe!=1 & problem_bmi!=1 & flag_age==0 & problem_age==0"

/*CHECK BASIC CONTROLS*/
count if ba_tc_wg!=1 & _hh_sample==1 & problem_weight!=1 & problem_bmi!=1 & ITT==1 & multiusers_l_base~=. & baseage_forregs<=3 & flag_age==0 & height_outlier_severe!=1 & problem_age==0
gen sample_defn=1 if ba_tc_wg!=1 & _hh_sample==1 & problem_weight!=1 & problem_bmi!=1 & ITT==1 	& multiusers_l_base~=. & baseage_forregs<=3 & flag_age==0 & height_outlier_severe!=1 & problem_age==0

/*Variables determining sample*/
sum ba_tc_wg _hh_sample problem_weight problem_bmi ITT multiusers_l_base baseage_forregs flag_age ///
	height_outlier_severe problem_age
sum ba_tc_wg _hh_sample problem_weight problem_bmi ITT multiusers_l_base baseage_forregs flag_age ///
	height_outlier_severe problem_age if sample_defn==1

/*Regression controls*/
sum c14_d_child_diarrhea ba_tc base_year ba04 ba05 t04 t05 year_two_three ///
	month2 month3 month4 month5 month6 month7 month8 month9 month10 month11 month12 ///
	ba_tc_boy age_combo_years age_combo_yrs2 boy_age boy_age2
sum c14_d_child_diarrhea ba_tc base_year ba04 ba05 t04 t05 year_two_three ///
	month2 month3 month4 month5 month6 month7 month8 month9 month10 month11 month12 ///
	ba_tc_boy age_combo_years age_combo_yrs2 boy_age boy_age2 if sample_defn==1

/*Covariates selection not equal zero - Vincent*/
global covariate_selection="ba_tc_latrine_density_base!=. & ba_tc_hygiene_know_base!=. & ba_tc_momeduc_base!=."

drop sample_defn

*make sure the panel data is set correctly
xtset _c_id before_after










*	EXaM analysis starts here. Estimating ATE 
********************************************************************************

*	OLS Regressions
*	Note that This Col 6 contains child FE
*	Main specifications used in EXaM is OLS with no control

rename c14_d_child_diarrhea diarrhea
rename c13_k_child_weight_kg weight
global depvars diarrhea weight bmi

label variable diarrhea						"Dependent Variable: Incidence of child diarrhea in past week"
label variable weight						"Dependent Variable: Child weight in kg"
label variable bmi							"Dependent Variable: Child BMI in kg/m2"
label variable ba_tc						"Treatment"
label variable ba_tc_latrine_density_base	"Treatment * latrine density"
label variable ba_tc_hygiene_know_base 		"Treatment * diarrhea prevention"
label variable ba_tc_momeduc_base 			"Treatment * mother's education"


* 	No controls

foreach var in $depvars {

	sum `var' if $child_sample & $evertreat==0
	global mean = round(r(mean),0.001)
	global lab: variable label `var'

	reg `var' ba_tc if $child_sample, vce(cluster spring_id)	
	outreg2 using "$EXaM_output/01_estimatingTE_`var'_a_nocontrol.xls", title($lab) keep(ba_tc) ctitle(" ") addtext("Mean of dependent variable in comparison group", $mean ) label bdec (3) tex(frag) excel se coef nor2 nocon nonotes replace

	foreach y in latrine_density_base hygiene_know_base momeduc_base{
		preserve
		
		reg `var' ba_tc ba_tc_`y' if $child_sample, vce(cluster spring_id)	
		outreg2 using "$EXaM_output/01_estimatingTE_`var'_a_nocontrol.xls", keep(ba_tc ba_tc_`y') ctitle(" ") label bdec (3) tex(frag) excel se coef nor2 nocon nonotes addtext("Mean of dependent variable in comparison group", $mean ) append

		* Save the estimated coefficients and standard errors for use in Code 04
		mat beta=e(b)
		svmat double beta, names(matcol)
		mat sd_tmp=e(V)
		mat sd_tmp2 = vecdiag(sd_tmp)
		mat sd = sd_tmp2
		forval i = 1/`= rowsof(sd_tmp2)' { 
			forval j = 1/`=colsof(sd_tmp2)' { 
				mat sd[`i', `j'] = sqrt(sd_tmp2[`i', `j']) 
			}
		} 
		svmat double sd, names(matcol)
		keep beta* sd*
		save "$EXaM_temp/04_OLS_`var'_on_`y'.dta", replace
		restore
	}

	preserve
	reg `var' ba_tc ba_tc_latrine_density_base ba_tc_hygiene_know_base ba_tc_momeduc_base if $child_sample, vce(cluster spring_id)	
	outreg2 using "$EXaM_output/01_estimatingTE_`var'_a_nocontrol.xls", keep(ba_tc ba_tc_latrine_density_base ba_tc_hygiene_know_base ba_tc_momeduc_base) ctitle("Main") addtext("Mean of dependent variable in comparison group", $mean ) label bdec (3) tex(frag) excel se coef nor2 nocon nonotes append
	
	* Save the estimated coefficients and standard errors for use in Code 04
	mat beta=e(b)
	svmat double beta, names(matcol)
	mat sd_tmp=e(V)
	mat sd_tmp2 = vecdiag(sd_tmp)
	mat sd = sd_tmp2
	forval i = 1/`= rowsof(sd_tmp2)' { 
		forval j = 1/`=colsof(sd_tmp2)' { 
			mat sd[`i', `j'] = sqrt(sd_tmp2[`i', `j']) 
		}
	} 
	svmat double sd, names(matcol)
	keep beta* sd*
	save "$EXaM_temp/04_OLS_`var'_on_all.dta", replace
	restore
	
	xtreg `var' ba_tc ba_tc_latrine_density_base ba_tc_hygiene_know_base ba_tc_momeduc_base if $child_sample, fe cluster(spring_id)
	outreg2 using "$EXaM_output/01_estimatingTE_`var'_a_nocontrol.xls", keep(ba_tc ba_tc_latrine_density_base ba_tc_hygiene_know_base ba_tc_momeduc_base) ctitle("Main") addtext("Mean of dependent variable in comparison group", $mean , Child FE, Yes) label bdec (3) tex(frag) excel se coef nor2 nocon nonotes append
}










*	With controls
foreach var in $depvars {

	sum `var' if $child_sample & $evertreat==0
	global mean = round(r(mean),0.001)
	global lab: variable label `var'

	reg `var' ba_tc $sround $year23 $months age_combo_years age_combo_yrs2 boy_age boy_age2 $balance_controls if $child_sample, vce(cluster spring_id)
	outreg2 using "$EXaM_output/01_estimatingTE_`var'_b_control.xls", title($lab) keep(ba_tc) ctitle(" ") bdec (3) tex(frag) excel se coef nor2 addtext("Mean of dependent variable in comparison group", $mean )  nocon nonotes replace

	foreach y in latrine_density_base hygiene_know_base momeduc_base {
	reg `var' ba_tc ba_tc_`y' $sround $year23 $months age_combo_years age_combo_yrs2 boy_age boy_age2 $balance_controls if $child_sample, vce(cluster spring_id)	
	outreg2 using "$EXaM_output/01_estimatingTE_`var'_b_control.xls", keep(ba_tc ba_tc_`y') ctitle(" ") label bdec (3) tex(frag) excel se coef nor2 nocon nonotes addtext("Mean of dependent variable in comparison group", $mean ) append
	}
	
	reg `var' ba_tc ba_tc_latrine_density_base ba_tc_hygiene_know_base ba_tc_momeduc_base $sround $year23 $months age_combo_years age_combo_yrs2 boy_age boy_age2 $balance_controls if $child_sample, vce(cluster spring_id)
	outreg2 using "$EXaM_output/01_estimatingTE_`var'_b_control.xls", keep(ba_tc ba_tc_latrine_density_base ba_tc_hygiene_know_base ba_tc_momeduc_base) ctitle(" ") label bdec (3) tex(frag) excel addtext("Mean of dependent variable in comparison group", $mean )  se coef nor2 nocon nonotes append
    
	xtreg `var' ba_tc ba_tc_latrine_density_base ba_tc_hygiene_know_base ba_tc_momeduc_base $sround $year23 $months age_combo_years age_combo_yrs2 boy_age boy_age2 $balance_controls if $child_sample, fe cluster(spring_id)
	outreg2 using "$EXaM_output/01_estimatingTE_`var'_b_control.xls", keep(ba_tc ba_tc_latrine_density_base ba_tc_hygiene_know_base ba_tc_momeduc_base) ctitle(" ") label bdec (3) tex(frag) excel addtext("Mean of dependent variable in comparison group", $mean, Child, Yes )  se coef nor2 nocon nonotes append

}

*	Add Specifications of Kremer et. al. 
********************************************************************************
/*OLS REGS*/
foreach var in $depvars {

	sum `var' if $child_sample & $evertreat==0
	global mean = round(r(mean),0.001)
	global lab: variable label `var'

   /*VERY BASIC SPECIFICATIONS*/

	*"COLUMN 1 IN TABLE 4 (DIARRHEA RESULTS)";

	reg `var' ba_tc if $child_sample, cluster(spring_id)
	outreg2 using "$EXaM_output/01_estimatingTE_`var'_c_kremer.xls", ctitle("Col 1")keep(ba_tc) addtext("Mean of dependent variable in comparison group", $mean ) bdec (3) se coef nocon nonotes replace

	*"COLUMN 3 IN TABLE 4 (DIARRHEA RESULTS)";

	xtreg `var' ba_tc $sround $year23 $months  $balance_controls if $child_sample, fe vce(cluster spring_id)
	outreg2  using "$EXaM_output/01_estimatingTE_`var'_c_kremer.xls", ctitle("Col 3") keep(ba_tc $year23) addtext("Mean of dependent variable in comparison group", $mean ) bdec (3) se coef nocon nonotes append

	*display "COLUMN 4/7/9 IN TABLE 4";

	xtreg `var' ba_tc $sround $year23 $months age_combo_years age_combo_yrs2 boy_age boy_age2  $balance_controls if $child_sample, fe vce(cluster spring_id)
	outreg2  using "$EXaM_output/01_estimatingTE_`var'_c_kremer.xls", ctitle("Col 4/7/9") keep(ba_tc $year23 age_combo_years age_combo_yrs2 boy_age boy_age2) addtext("Mean of dependent variable in comparison group", $mean ) bdec (3) se coef nocon nonotes append

	*"COLUMN 5/8/10 IN TABLE 4";

	xtreg `var' ba_tc $sround $year23 $months ba_tc_boy age_combo_years age_combo_yrs2 boy_age boy_age2  $balance_controls if $child_sample,  fe vce(cluster spring_id)
	outreg2 using "$EXaM_output/01_estimatingTE_`var'_c_kremer.xls", ctitle("Col 5/8/10") keep(ba_tc $year23 age_combo_years age_combo_yrs2 boy_age boy_age2 ba_tc_boy ) addtext("Mean of dependent variable in comparison group", $mean ) bdec (3) se coef nocon nonotes append  

	*"COLUMN 6 IN TABLE 4 (DIARRHEA RESULTS)";

	xtreg `var' ba_tc $sround $year23 $months $x_hhba_tc age_combo_years age_combo_yrs2 boy_age boy_age2  $balance_controls if $child_sample,  fe vce(cluster spring_id)
	outreg2  using "$EXaM_output/01_estimatingTE_`var'_c_kremer.xls", ctitle("Col 6") keep(ba_tc $year23 age_combo_years age_combo_yrs2 boy_age boy_age2 $x_hhba_tc ) addtext("Mean of dependent variable in comparison group", $mean ) bdec (3) se coef nocon nonotes append

}



*	Get Household Fixed Effects Values for step 07
********************************************************************************
save "$EXaM_temp/01_reg.dta",replace

reg diarrhea ba_tc ba_tc_latrine_density_base ba_tc_hygiene_know_base ba_tc_momeduc_base i.hh_id i.before_after if $child_sample, vce(cluster spring_id)
parmest, saving("$EXaM_input/07_dia_all_hhFE.dta", replace)

use "$EXaM_temp/01_reg.dta",clear
reg diarrhea ba_tc ba_tc_latrine_density_base ba_tc_hygiene_know_base ba_tc_momeduc_base i.hh_id i.before_after if $child_sample, vce(cluster spring_id) 

gen dia_hh_fe = .

forval counter = 1/25264 {
	global hhid = hh_id[`counter']
	capture replace dia_hh_fe = _b[$hhid.hh_id] in `counter'
}



preserve
	collapse (first) dia_hh_fe, by(hh_id)
	save	"$EXaM_temp/07_dia_all_hhFE.dta", replace
restore

cd "$EXaM_output"
import delimited using "01_estimatingTE_diarrhea_a_nocontrol.txt", clear
drop v1 v8
gen sno = _n
drop if sno <= 4
drop if sno == 13 | sno > 15
lab var v7 "Main"
replace v2 = "Observations" if sno == 14 
replace v2 = "Mean of dependent variable in comparison group" if sno == 15
drop sno
texsave  v2 v3 v3 v4 v5 v6 v7 using "test.tex", frag  replace autonumber varlabels sw nofix size(1) title("Dependent variable: Incidence of Child Diarrhea in Past Week") hlines(8) 








