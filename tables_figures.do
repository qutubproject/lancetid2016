* Replication file for:
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
	
* Table 2. Management of Case 1 and Case 2 for all cities and for Patna and Mumbai only

	use "data/sp.dta", clear
	
	expand 2, gen(false2)
	
	drop if false2 == 1 & cp_2 == 1

		replace sp_location = 4
		label def sp_location 4 "All" , modify
		
		replace weight = 1 
	
	egen type = group(false2 sp_location case) , label
				
	bintab ///
		dr_3 correct_treatment dr_1 t_12 ///
		med_any med_b2_any_antibiotic med_b2_any_antister ///
		med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_schedule_x ///
		med_b2_any_steroid med_l_any_1 med_l_any_2 ///
	using "outputs/Table_2.xls" ///
	[pweight = weight] ///
	, over(type) continuous(med)
	
* Figure 1. Odds ratios for case management outcomes for Case 1 versus Case 2
	
	use "data/sp.dta", clear
	
	xtset id
	
	chartable ///
		correct_treatment dr_3  ///
		med_any med_b2_any_antibiotic med_l_any_2 med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_steroid  ///
		, c(xtlogit) rhs(6.case i.cp_2) or p case0(Case 1) case1(Case 2) 	
	
	graph export "outputs/Figure_1.png", replace width(2000)
	
* Figure 2. Drug use by referral decisions for two standardised patient cases

	use "data/sp.dta", clear
	
	gen n = 1 
	
	replace med_b2_antister_cat = 5 if med == 0
	
	egen checkgroup = group(case dr_3), label
		label def checkgroup 1 `""Case 1" "(503/599)""' 2 `""Case 1" "(96/599)""' ///
			3 `""Case 2" "(200/601)""' 4 `""Case 2" "(401/601)""' , modify
	
	tabgen med_b2_antister_cat
	
	graph bar med_b2_antister_cat?? ///
		if dr_3 == 1 ///
		, stack over(checkgroup) nofill ///
		ylab($pct) legend(order(5 "No Medication" 4 "Antibiotic and Steroid" 3 "Antibiotic" 2 "Steroid" 1 "No Antibiotic or Steroid") ///
			c(1) symxsize(small) symysize(small) pos(3) size(small)) ///
		$graph_opts1 bar(5, color(white) lc(black) lp(solid) lw(thin)) ///
		bar(1,lw(thin) lc(black)) bar(2,lw(thin) lc(black)) bar(3,lw(thin) lc(black)) bar(4,lw(thin) lc(black)) ///
		subtitle("Referral" ,color(black) justification(center) pos(12)) 
		
		graph save "outputs/Figure_2_1.gph" , replace
		
	graph bar med_b2_antister_cat?? ///
		if dr_3 == 0 ///
		, stack over(checkgroup) nofill ///
		ylab($pct) legend(order(5 "No Medication" 4 "Antibiotic and Steroid" 3 "Antibiotic" 2 "Steroid" 1 "No Antibiotic or Steroid") ///
			c(1) symxsize(small) symysize(small) pos(3) size(small)) ///
		$graph_opts1 bar(5, color(white) lc(black) lp(solid) lw(thin)) ///
		bar(1,lw(thin) lc(black)) bar(2,lw(thin) lc(black)) bar(3,lw(thin) lc(black)) bar(4,lw(thin) lc(black)) ///
		subtitle("No Referral" ,color(black) justification(center) pos(12)) 
		
		graph save "outputs/Figure_2_2.gph" , replace
		
	grc1leg ///
		"outputs/Figure_2_2.gph" ///
		"outputs/Figure_2_1.gph" ///
		, pos(3) graphregion(color(white)) xsize(7) rows(1) leg("outputs/Figure_2_2.gph")
		
	graph export "outputs/Figure_2.png" , replace
		
* Figure 3. Management of both Case 1 and Case 2 combined by city

	use "data/sp.dta", clear
	
	replace cp_1 = proper(cp_1)
	encode cp_1, gen(city)

	betterbar ///
		(dr_3 correct_treatment)  ///
		(med_b2_any_antibiotic med_b2_any_steroid med_b2_any_antister med_l_any_2 med_b2_any_schedule_h med_b2_any_schedule_h1 med_b2_any_schedule_x med_l_any_1)   ///
		, over(city) xlab($pct) se bin legend(pos(5) ring(0) c(1) symxsize(small) symysize(small) ) ysize(7) n barlab(upper)
		
		graph export "outputs/Figure_3.png" , replace
		
* Figure 4. Active ingredients in drugs given for each case

	local n_5 = 599
	local n_6 = 601
	
	local title_5 = "Classic case of presumed TB"
	local title_6 = "TB case with positive sputum report"

	qui forvalues i = 5/6 {
	
		local case = `i' - 4

		use "data/sp_meds.dta" , clear
			
			gen n = 1
			bys med_generic: egen med_class_typ = mode(med_class), minmode // Label with most typical medicine code
				label val med_class_typ med_k
				
			keep if case == `i'
			
			labelcollapse (firstnm) n med_class_typ med_generic_encoded sp_location, by(med_generic facilitycode) vallab(med_class_typ med_generic_encoded sp_location)
			
			labelcollapse (sum) n (firstnm) med_generic_encoded med_class_typ, by(med_generic) vallab(med_class_typ med_generic_encoded) 
			
			cap separate n, by(med_generic_encoded) shortlabel
				foreach var of varlist n?* {
					local theLabel : var label `var'
					local theLabel = regexr("`theLabel'","med_generic_encoded == ","")
					
					cap su n if med_generic == "`theLabel'"
						cap local theN = `r(mean)'
					
					label var `var' "`theLabel' [`theN']"
					}
					
				foreach var of varlist n?* {
					replace `var' = . if `var' < 5 // Exclude low volumes
					replace `var' = `var'/`n_`i'' // Number of interactions
					qui sum `var'
						if `r(N)' == 0 drop `var' 
					}
				
			drop if med_generic == "Sodium Chloride" // not an active ingredient
			
			betterbar (n?*) , stat(sum) over(med_class_typ) by(med_class_typ) nobylabel nobycolor d(1)  ///
				legend(span c(1) pos(3) ring(1) symxsize(small) symysize(small) size(small))  ///
				dropzero ///
				xlab(0 "0%" .2 "20%" .4 "40%" .6 "60%") ysize(6) labsize(2) $graph_opts title("Case `case' (N=`n_`i'')") subtitle("`title_`i''")
			
			* graph export "$directory/Outputs/Revision 1/generics_sp`i'.png", replace width(4000)
				graph save "outputs/Figure_4_`case'.gph" , replace
			
		}
		
		grc1leg ///
			"outputs/Figure_4_1.gph" ///
			"outputs/Figure_4_2.gph" ///
			, pos(3) graphregion(color(white)) xsize(7) 
			
			graph export "outputs/Figure_4.png", replace width(2000)

* Have a lovely day!
