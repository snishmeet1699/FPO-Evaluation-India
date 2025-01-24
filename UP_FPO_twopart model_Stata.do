
*************************************
	* Paper: Association of membership in a farmer producer organization with crop diversity, household income, diet diversity, and women's empowerment in Uttar Pradesh, India
	* Objective: Running the two-part regression model on income
	* Written: Nishmeet Singh
	* Date: September 2024			
	* Version: Stata/SE 17	
*************************************

	clear all
	clear matrix
	macro drop _all

* AUTOMATED SELECTION OF ROOT PATH BASED ON USER

	if c(os) == "Windows" {
		cd "C:/Users/`c(username)'/Library/CloudStorage/OneDrive-UniversityofEdinburgh"
	}
	else if c(os) == "MacOSX" {
		cd "/Users/`c(username)'/Library/CloudStorage/OneDrive-UniversityofEdinburgh"
	}
	local cloud `c(pwd)'
	if "`c(username)'" == "nishmeetsingh" {
	local stem "`cloud'/UP FPO/"
	}
	else {
	local stem "`cloud'/"
	}
	
	di "`stem'"


	* Import data file
	import excel "`stem'/Income_Reg.xlsx", sheet("Sheet1") firstrow

	* Redefine variables for analysis in Stata
	encode a_fpo_hh, gen(fpo)
	recode fpo (2=0) (1=1)

	encode a_caste, gen(cst)

	encode a_educ_Male, gen(edu_Male)
	encode a_educ_Female, gen(edu_Female)

	* Running the twopart model
	twopm total_income_mon i.fpo i.cst i.edu_Male i.edu_Female he_q1_hhmem ag_land_kh_hc, firstpart(logit) secondpart(glm, family(gamma) link(log)) robust
	margins, atmeans

	margins,dydx(*) post
	estimates store margins_dydx

	* Export results
	outreg2 margins_dydx using margins_dydx.doc, word replace ctitle("Marginal Effects") se
	outreg2 using margins_dydx.doc, word append ci
	outreg2 using margins_dydx.doc, word append pvalue
	
* End of code ***	
	

