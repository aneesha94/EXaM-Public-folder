*************************************************************************************
*																					*
*   Experimental Design as Market Design (EXaM)										*
*	============================================================================	*
*   Author  : Yusuke Narita	   														*
*   RA      : Vincent Tanutama														*
*   The following STATA code applies Kremer et al (2011) "Spring Cleaning" Paper 	*
*	in QJE into our experimental setting											*
*	----------------------------------------------------------------------------	*

/*--------------------------------------------------------------------------------------------------
 Modify the following lines in the do files, Matlab files and python code to set your own paths
(1) Modify line 14 of 00_master.do to set your path
(2) Modify line 27 of table4 col1.m, table4 col2.m, table4 col3.m, and
table4 col4.m in 02b
(3) Modify line 9 of AAIW.py in 07b
--------------------------------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------------------
Run the files in the order given below
1.  Run 00_master.do 
2.  Run 01_estimatingHTE.do
3.  Run table4_col4.m in 02b 
4.  Run 03_simulatingWTP.do
5.  Run 04_simulatingHTE.do
6.  Run 05_jointdistributionWTPHTE.do
7.  Run EXaM_RL.py 
8.  Run EXaM_RL_coarse.py
9.  Run 06_EXaMvsRCT_welfare_coarse.do
10. Run 07a_EXaMvsRCT_information.do
11. Run 07a_EXaMvsRCT_information_coarse.do
12. Run AAIW.py in 07b
13. Run 07c_EXaMvsRCT_information_coarse.do
14. Run 08a_EXaMvsRCT_incentive.do
15. Run 08a_EXaMvsRCT_incentive_coarse.do
16. Run EXaM_RL_coarse_10.py
17. Run EXaM_RL_coarse_100.py
18. Run EXaM_RL_coarse_1000.py
19. Run EXaM_RL_coarse_100plus.py
20. Run EXaM_RL_coarse_100minus.py
21. Run 08b_EXaMvsRCT_incentive_coarse.do
22. Run 09_propensity.do
--------------------------------------------------------------------------------------------------*/

* Set your path here 
global EXaM "C:\Users\ap2474\Dropbox\Experiment Design as Market Design - Public"


* Mention the number of simulations here 
global simulation = 1000

*Create the necessary folders 
cd "$EXaM"
cap mkdir input 
cap mkdir output 
cap mkdir temp 
cd "./output"
cap mkdir "$simulation"
cd ".."
cd "./input"
cap mkdir "WTP_HTE_forPythonEXaMalgorithm"
*	Do not change the below 14 lines
global EXaM_code	"$EXaM/code"  // All the code files except for EXaM.py files should be placed here
global EXaM_input	"$EXaM/input" // Paste the input files reg_data_households_Aug2010.dta, reg_data_children_Aug2010, prefs-time_Au2010.dta dowloaded from Harvardverse here
global EXaM_output	"$EXaM/output/$simulation"
global EXaM_temp	"$EXaM/temp" // Paste mixed_logit_data_spec_1_copy.txt here
clear
clear mata
clear matrix
capture log closed
prog drop _all
set more off
set logtype text
set mem 650m
set maxvar 32767
set matsize 11000

*   01. Estimating heterogeneous treatment effects based on Kremer et al Table IV Col 6
do "$EXaM_code/01_estimatingHTE.do"

*	02a. Preparing for willingness-to-pay estimation (performed on Matlab) and simulation
do "$EXaM_code/02a_estimatingWTP.do"


*	03. Simulating willingness-to-pay based on the previous mixed logit estimates in step 02 and graphing their distributions.
do "$EXaM_code/03_simulatingWTP.do"

*	04. Simulating heterogeneous treatment effects based on OLS in step 01 and graphing their distributions.
do "$EXaM_code/04_simulatingHTE.do"

*	05. Graphing the joint distribution of WTP from step 03 and HTE from step 04.
do "$EXaM_code/05_jointdistributionWTPHTE.do"

*	----------------------------------------------------------------------------	*
*	Coarse simulated values: HTE and WTP values are cut in 4 groups and assigned	*
*	the median values within group													*
*	----------------------------------------------------------------------------	*


*	06. Graphing EXaM vs RCT comparison in terms of welfare (WTP and HTE)
do "$EXaM_code/06_EXaMvsRCT_welfare_coarse.do"

*	07. Graphing EXaM vs RCT comparison in terms of information, based on HTE estimates
* We need to run the below files to produce input files for AAIW
do "$EXaM_code/07a_EXaMvsRCT_information.do" 

* We need to run the below code to produce the graphs 
* (i) 07_EXaMvsRCT_information_b_coarse_mean_1000
* (ii) 07_EXaMvsRCT_information_beta_coarse_mean_1000
do "$EXaM_code/07a_EXaMvsRCT_information_coarse.do" 
do "$EXaM_code/07c_EXaMvsRCT_information_coarse.do"

*	08a. Preparing data for EXaM vs RCT comparison in terms of incentive compatibility
do "$EXaM_code/08a_EXaMvsRCT_incentive.do"
do "$EXaM_code/08a_EXaMvsRCT_incentive_coarse.do"
*	08b. Graphing EXaM vs RCT comparison in terms of incentive compatibility
do	"$EXaM_code/08b_EXaMvsRCT_incentive_coarse.do"

*	09. Propensity Score
do "$EXaM_code/09_propensity.do"
