* Appendix replication file for:
* Satyanarayana S, Kwan A, Daniels B, Subbaraman R, McDowell A, Bergkvist S, Das RK, Das V, Das J, Pai M. 
* Use of standardised patients to assess antibiotic dispensing for tuberculosis by pharmacies in urban India: 
* A cross-sectional study. 
* The Lancet Infectious Diseases. 2016 Nov 30;16(11):1261-8.

	version 13
	cd "/Users/bbdaniels/GitHub/lancetid2016/"
	cd "/Users/bbdaniels/Desktop/Chemists Public Release/"
		
	qui do "adofiles/bintab.ado"
	qui do "adofiles/chartable.ado"
	qui do "adofiles/tabgen.ado"

	global graph_opts note(, justification(left) color(black) span pos(7)) title(, justification(left) color(black) span pos(11)) subtitle(, justification(left) color(black) span pos(11)) graphregion(color(white)) ylab(,angle(0) nogrid) ytit("") xtit(,placement(left) justification(left)) yscale(noline) xscale(noline) xsize(7) legend(region(lc(none) fc(none)))
	global graph_opts1 bgcolor(white) graphregion(color(white)) legend(region(lc(none) fc(none))) ylab(,angle(0) nogrid) subtitle(, justification(left) color(black) span pos(11)) title(, color(black) span)
	global hist_opts ylab(, angle(0) axis(2)) yscale(off alt axis(2)) ytit(, axis(2)) ytit(, axis(1))  yscale(alt)
	global pct `" 0 "0%" .25 "25%" .5 "50%" .75 "75%" 1 "100%" "'
	global numbering `""(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)""'
	
* Table A4. Clinical outcomes for Case 1 and 2 combined across cities and by city

	use "data/sp.dta", clear
	
	expand 2, gen(false2)
		replace sp_location = 4 if false2
		label def sp_location 4 "All" , modify
		
		replace weight = 1 if false2
	
	egen type = group(sp_location case) , label
				
	bintab ///
		dr_3 correct_treatment dr_1 t_12 ///
		med_any med_b2_any_* med_l_any_1 med_l_any_2 ///
	using "outputs/Table_A4.xls" ///
	[pweight = weight] ///
	, over(type) continuous(cp_16 checklist_n p_inr p_usd_2015 re_total med)
	
* Table A5. Summary of differences between Case 1 and Case 2 - No Controls Logit Model

	use "data/sp.dta", clear
	
	xtset id
	
	chartable ///
		correct_treatment dr_3  ///
		med_any med_b2_any_antibiotic med_l_any_2 med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_steroid  ///
		using "outputs/Table_A5.xlsx" ///
		, c(logit) rhs(6.case) or p case0(Case 1) case1(Case 2)
	
* Table A6. Summary of differences between Case 1 and Case 2 under various city-fixed-effects models

	use "data/sp.dta", clear
	
	xtset id
	
	cap mat drop allResults
	cap mat drop allResults_STARS
	
	qui foreach var of varlist ///
		correct_treatment dr_3  ///
		med_any med_b2_any_antibiotic med_l_any_2 ///
		med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_steroid {
		
			local theLabel : var label `var'
			local theLabels = `"`theLabels' "`theLabel'""'
		
			reg `var' 6.case i.cp_2
				mat a = r(table)
					local b = a[1,1]
					local p = a[4,1]
						local stars = 0
						if `p' < .1  local stars = 3
						if `p' < .05 local stars = 2 
						if `p' < .01 local stars = 1
						
				mat theResult = [`b']
				mat theResult_STARS = [`stars']
				
			xtreg `var' 6.case i.cp_2 , fe
				mat a = r(table)
					local b = a[1,1]
					local p = a[4,1]
						local stars = 0
						if `p' < .1  local stars = 3
						if `p' < .05 local stars = 2 
						if `p' < .01 local stars = 1
						
				mat theResult = theResult , [`b']
				mat theResult_STARS = theResult_STARS , [`stars']
				
			xtreg `var' 6.case i.cp_2 , re
				mat a = r(table)
					local b = a[1,1]
					local p = a[4,1]
						local stars = 0
						if `p' < .1  local stars = 3
						if `p' < .05 local stars = 2 
						if `p' < .01 local stars = 1
						
				mat theResult = theResult , [`b']
				mat theResult_STARS = theResult_STARS , [`stars']
			
			logit `var' 6.case i.cp_2 , or
				mat a = r(table)
					local b = a[1,1]
					local p = a[4,1]
						local stars = 0
						if `p' < .1  local stars = 3
						if `p' < .05 local stars = 2 
						if `p' < .01 local stars = 1
						
				mat theResult = theResult , [`b']
				mat theResult_STARS = theResult_STARS , [`stars']
				
			xtlogit `var' 6.case i.cp_2 , fe or
				est sto logfe
				mat a = r(table)
					local b = a[1,1]
					local p = a[4,1]
						local stars = 0
						if `p' < .1  local stars = 3
						if `p' < .05 local stars = 2 
						if `p' < .01 local stars = 1
						
				mat theResult = theResult , [`b']
				mat theResult_STARS = theResult_STARS , [`stars']
				
			xtlogit `var' 6.case i.cp_2 , re or
				est sto logre
				mat a = r(table)
					local b = a[1,1]
					local p = a[4,1]
						local stars = 0
						if `p' < .1  local stars = 3
						if `p' < .05 local stars = 2 
						if `p' < .01 local stars = 1
						
				mat theResult = theResult , [`b']
				mat theResult_STARS = theResult_STARS , [`stars']

				hausman logfe logre
				
				mat theResult = theResult , [`r(p)']
				mat theResult_STARS = theResult_STARS , [0]
				
				su `var' if case == 5
					mat theResult = theResult , [`r(mean)']
					mat theResult_STARS = theResult_STARS , [0]
				su `var' if case == 6
					mat theResult = theResult , [`r(mean)']
					mat theResult_STARS = theResult_STARS , [0]
					
				mat allResults = nullmat(allResults) \ theResult
				mat allResults_STARS = nullmat(allResults_STARS) \ theResult_STARS
			
		}
				
			xml_tab allResults ///
				using "outputs/Table_A6.xls" ///
				, rnames(`theLabels') replace ///
				cnames("OLS" "OLS FE" "OLS RE" "Logit" "Logit FE" "Logit RE" "Hausman P-statistic" "Case 1" "Case 2")
	
* Table A7. Summary of differences between Case 1 and Case 2, controlled for SP characteristics

	use "data/sp.dta", clear
	
	xtset id
	
	chartable ///
		correct_treatment dr_3  ///
		med_any med_b2_any_antibiotic med_l_any_2 med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_steroid  ///
		using "outputs/Table_A7.xlsx" ///
		, c(xtlogit) rhs(6.case i.cp_2 sp_male sp_height sp_weight sp_age) or p case0(Case 1) case1(Case 2)
		
* Table A8. Differences in case management between cities, controlled for SP case and identity

	use "data/sp.dta", clear
	
	keep if (city_patna == 1 & city_mumbai == 1 & cp_1 != "DELHI")
		
	chartable ///
		correct_treatment dr_3  ///
		med_any med_b2_any_antibiotic med_l_any_2 med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_steroid ///
		using "outputs/TableA8_1.xlsx" ///
		, c(logit) rhs(2.cp_2 6.case i.sp_code) or p case0(Mumbai) case1(Patna) 
		
	use "data/sp.dta", clear
	
	keep if cp_1 != "PATNA"
	
	chartable ///
		correct_treatment dr_3  ///
		med_any med_b2_any_antibiotic med_l_any_2 med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_steroid ///
		using "outputs/TableA8_2.xlsx" ///
		, c(logit) rhs(1.cp_2) or p case0(Mumbai) case1(Delhi)

	keep if (city_delhi == 1 & city_mumbai == 1 & cp_1 != "PATNA")
		
	chartable ///
		correct_treatment dr_3  ///
		med_any med_b2_any_antibiotic med_l_any_2 med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_steroid ///
		using "outputs/TableA8_3.xlsx" ///
		, c(logit) rhs(1.cp_2 6.case i.sp_code) or p case0(Mumbai) case1(Delhi)

	use "data/sp.dta", clear
	
	keep if cp_1 != "MUMBAI"
	
	chartable ///
		correct_treatment dr_3  ///
		med_any med_b2_any_antibiotic med_l_any_2 med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_steroid ///
		using "outputs/TableA8_4.xlsx" ///
		, c(logit) rhs(2.cp_2 6.case i.sp_code) or p case0(Delhi) case1(Patna)
		
	keep if (city_delhi == 1 & city_patna == 1 & cp_1 != "MUMBAI")
		
	chartable ///
		correct_treatment dr_3  ///
		med_any med_b2_any_antibiotic med_l_any_2 med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_steroid ///
		using "outputs/TableA8_5.xlsx" ///
		, c(logit) rhs(2.cp_2 6.case i.sp_code) or p case0(Delhi) case1(Patna)

	use "data/sp.dta", clear
	
	expand 2 , gen(falseall)

	chartable ///
		correct_treatment dr_3  ///
		med_any med_b2_any_antibiotic med_l_any_2 med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_steroid ///
		using "outputs/TableA8_6.xlsx" ///
		, c(logit) rhs(1.falseall 6.case i.sp_code) or p case0(ALl) case1(ALL)

* Have a lovely day!
