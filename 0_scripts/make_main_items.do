log using "log_stata_files.log"

********************************************************************************
* This do-file produces the files to replicates the main tables and figures
* in "How Newspapers Reveal Political Power" (Ban, Fouirnaies, Hall, Snyder)
*
*
* -- Produces the .tex files for the following tables:
* 		Table 1: News Coverage Before, During, and After Leadership Term
*		Table 2: Impact of Switch from Mayor-Countil to Council-Manager
*				 City Government (also Table A.4)
*
* -- Produces the .dta files to be used to plot figures; next step in R with
*	 the code make_file_graphs.R
*
* November 20, 2016
********************************************************************************


set more off

local make_committees	= 1
local make_leader 	 	= 1
local make_mayors  		= 1
local make_rtaa 		= 1
local make_party_committee = 1



********************************************************************************
* Committee Rankings, 1949-1973
********************************************************************************


if `make_committees'==1 {
	
	use "C:\Users\Sean Hambali\Documents\GitHub\PPOL6081\replication-01\1_raw\hits_housecommittees.dta", clear
	
	keep if year>=1949 & year<=1973
	collapse (sum) agriculture appropriations armed_services banking education energy foreign_affairs government_operations house_administration judiciary merchant_marine natural_resources post_office public_works rules science standards_of_official_conduct veterans_affairs ways_and_means

	egen total_hits = rowtotal(agriculture appropriations armed_services banking education energy foreign_affairs government_operations house_administration judiciary merchant_marine natural_resources post_office public_works rules science standards_of_official_conduct veterans_affairs ways_and_means)
	
	rename agriculture c_agriculture
	rename appropriations c_appropriations
	rename armed_services c_armed_services
	rename banking c_banking
	rename education c_education
	rename energy c_energy
	rename foreign_affairs c_foreign_affairs
	rename government_operations c_government_operations
	rename house_administration c_house_administration
	rename judiciary c_judiciary
	rename merchant_marine c_merchant_marine
	rename natural_resources c_natural_resources
	rename post_office c_post_office
	rename public_works c_public_works
	rename rules c_rules
	rename science c_science
	rename standards_of_official_conduct c_standards_of_official_conduct
	rename veterans_affairs c_veterans_affairs
	rename ways_and_means c_ways_and_means
	
	reshape long c_, i(total_hits) j(committee) string
	
	replace c_ = c_/total_hits
	rename c_ rel_hits
	drop total_hits
	
	gsort -rel_hits
	gen rank = _n
	
	replace committee = subinstr(committee,"_"," ",.)
	replace committee = upper(committee)
	replace committee = "ENERGY AND COMMERCE" if committee=="ENERGY"
	replace committee = "EDUCATION AND LABOR" if committee=="EDUCATION"
	keep committee rank
	sort committee
	
	tempfile committee
	save `committee', replace
	
	import delimited "groseclose_stewart_81st_93rd.csv", varnames(1) clear
	replace committee = upper(committee)
	rename rank GS_rank
	merge 1:1 committee using `committee'
	
	drop _merge
	
	corr rank GS_rank
	
	export delimited using "for_committees_r_graph.csv", replace
}


*****************************************************************************************
* News Coverage of Speakers of the House, Before, During, and After Speakership
* Table 1: News Coverage Before, During, and After Leadership Term
*****************************************************************************************

if `make_leader'==1 {
	
	use "C:\Users\Sean Hambali\Documents\GitHub\PPOL6081\replication-01\1_raw\hits_leadernames.dta", clear
	
	*Table comparing means
	collapse (sum) randall keifer carlisle reed	crisp henderson	cannon clark gillett longworth	garner rainey byrns	bankhead rayburn martin mccormack albert halleck garrett snell mann, by(year)
	qui foreach var in randall keifer carlisle reed	crisp henderson	cannon clark gillett longworth	garner rainey byrns	bankhead rayburn martin mccormack albert halleck garrett snell mann {
		rename `var' hits`var'
	}
	reshape long hits, i(year) j(name) string
	gen period = .
	replace period=1 if year>=1871 & year<1876 & name=="randall"
	replace period=2 if year>=1876 & year<=1881 & name=="randall"
	replace period=3 if year > 1881 & year<=1886 & name=="randall"
	
	replace period=1 if year>=1876 & year<1881 & name=="keifer"
	replace period=2 if year>=1881 & year<=1883 & name=="keifer"
	replace period=3 if year > 1881 & year<=1886 & name=="keifer"
	
	replace period=1 if year>=1878 & year<1883 & name=="carlisle"
	replace period=2 if year>=1883 & year<=1889 & name=="carlisle"
	replace period=3 if year > 1889 & year<=1894 & name=="carlisle"
	
	replace period=1 if year>=1884 & year<1889 & name=="reed"
	replace period=2 if ([year>=1889 & year<=1891] | [year>=1895 & year<=1899]) & name=="reed"
	replace period=3 if ([year>1899 & year<=1904] | [year>1891 & year<1895]) & name=="reed"
	
	replace period=1 if year>=1886 & year<1891 & name=="crisp"
	replace period=2 if year>=1891 & year<=1895 & name=="crisp"
	replace period=3 if year>1895 & year<=1900 & name=="crisp"
	
	replace period=1 if year>=1898 & year<1903 & name=="henderson"
	replace period=2 if year>=1899 & year<=1903 & name=="henderson"
	replace period=3 if year>1903 & year<=1908 & name=="henderson"
	
	replace period=1 if year>=1898 & year<1903 & name=="cannon"
	replace period=2 if year>=1903 & year<=1911 & name=="cannon"
	replace period=3 if year>1911 & year<=1916 & name=="cannon"
	
	replace period=1 if year>=1906 & year<1911 & name=="clark"
	replace period=2 if year>=1911 & year<=1919 & name=="clark"
	replace period=3 if year>1919 & year<=1924 & name=="clark"
	
	replace period=1 if year>=1914 & year<1919 & name=="gillett"
	replace period=2 if year>=1919 & year<=1925 & name=="gillett"
	replace period=3 if year > 1925 & year<=1930 & name=="gillett"
	
	replace period=1 if year>=1922 & year<1927 & name=="longworth"
	replace period=2 if year>=1927 & year<=1929 & name=="longworth"
	replace period=3 if year > 1929 & year<=1934 & name=="longworth"
	
	replace period=1 if year>=1926 & year<1931 & name=="garner"
	replace period=2 if year>=1931 & year<=1933 & name=="garner"
	replace period=3 if year > 1933 & year<=1938 & name=="garner"
	
	replace period=1 if year>=1928 & year<1933 & name=="rainey"
	replace period=2 if year>=1933 & year<=1934 & name=="rainey"
	replace period=3 if year > 1934 & year<=1939 & name=="rainey"
	
	replace period=1 if year>=1930 & year<1935 & name=="byrns"
	replace period=2 if year>=1935 & year<=1936 & name=="byrns"
	replace period=3 if year > 1936 & year<=1941 & name=="byrns"
	
	
	replace period=1 if year>=1931 & year<1936 & name=="bankhead"
	replace period=2 if year>=1936 & year<=1940 & name=="bankhead"
	replace period=3 if year > 1940 & year<=1945 & name=="bankhead"
	
	
	replace period=1 if year>=1935 & year<1940 & name=="rayburn"
	replace period=2 if ([year>=1940 & year<=1947] | [year>=1949 & year<=1953] | [year>=1955 & year<=1961]) & name=="rayburn"
	replace period=3 if ([year>1947 & year<=1949] | [year>1953 & year<1955] | [year>1961 & year<1966]) & name=="rayburn"
	
	
	replace period=1 if year>=1884 & year<1889 & name=="martin"
	replace period=2 if ([year>=1947 & year<=1949] | [year>=1953 & year<=1955]) & name=="martin"
	replace period=3 if ([year>1949 & year<=1953] | [year>1955 & year<1960]) & name=="martin"
	
	replace period=1 if year>=1957 & year<1962 & name=="mccormack"
	replace period=2 if year>=1962 & year<=1971 & name=="mccormack"
	replace period=3 if year > 1971 & year<=1976 & name=="mccormack"
	
	replace period=1 if year>=1966 & year<1971 & name=="albert"
	replace period=2 if year>=1971 & year<=1977 & name=="albert"
	replace period=3 if year > 1977 & year<=1982 & name=="albert"
	
	*minority leader
	
	replace period=4 if year>=1906 & year<1911 & name=="mann"
	replace period=5 if year>=1911 & year<=1919 & name=="mann"
	replace period=6 if year>1919 & year<=1924 & name=="mann"
	
	replace period=4 if year>=1926 & year<1931 & name=="snell"
	replace period=5 if year>=1931 & year<=1939 & name=="snell"
	replace period=6 if year>1939 & year<=1944 & name=="snell"
	
	replace period=4 if year>=1918 & year<1923 & name=="garrett"
	replace period=5 if year>=1923 & year<=1929 & name=="garrett"
	replace period=6 if year>1929 & year<=1934 & name=="garrett"
	
	replace period=4 if year>=1884 & year<1889 & name=="halleck"
	replace period=5 if ([year>=1947 & year<=1949] | [year>=1953 & year<=1955]) & name=="halleck"
	replace period=6 if ([year>1949 & year<=1953] | [year>1955 & year<1960]) & name=="halleck"
	
	
	drop if period==.
	sort year
	bysort period name: egen rank=rank(_n)
	drop year
	reshape wide hits, i(rank name) j(period)
	
	preserve
	keep if hits1 != . & hits2 != . & hits3 != .
	keep name hits1-hits3
	drop if hits1 == 0 | hits2==0 | hits3==0
	restore
	
	sum hits1
	local m_before = r(mean)
	local sd_before = r(sd)
	local N_before =r(N)
	
	sum hits2
	local m_during = r(mean)
	local sd_during = r(sd)
	local N_during =r(N)
	
	sum hits3
	local m_after = r(mean)
	local sd_after = r(sd)
	local N_after =r(N)
	
	
	local diff1 = `m_before' - `m_during'
	ttest hits1==hits2, unpaired
	local p1 = r(p)
	local diff2 = `m_after' - `m_during'
	ttest hits3==hits2, unpaired
	local p2 = r(p)
	
	**minority leaders
	
	sum hits4
	local lead_before = r(mean)
	local lead_sd_before = r(sd)
	local lead_N_before =r(N)
	
	sum hits5
	local lead_during = r(mean)
	local lead_sd_during = r(sd)
	local lead_N_during =r(N)
	
	sum hits6
	local lead_after = r(mean)
	local lead_sd_after = r(sd)
	local lead_N_after =r(N)
	
	local diff3 = `lead_before' - `lead_during'
	ttest hits4==hits5, unpaired
	local p3 = r(p)
	local diff4 = `lead_after' - `lead_during'
	ttest hits6==hits5, unpaired
	local p4 = r(p)

	quietly {
		cap log close
		set linesize 255
		log using "Table1_leaders.tex", text replace
		noisily dis "\documentclass[11pt]{article}"
		noisily dis "\usepackage[margin=1in]{geometry}"
		noisily dis "\setlength{\pdfpagewidth}{8.5in} \setlength{\pdfpageheight}{11in}"
		noisily dis "\usepackage{booktabs}"
		noisily dis "\usepackage{makecell}"
		noisily dis "\usepackage{adjustbox}"
		noisily dis "\begin{document}"
		noisily dis "\begin{table}[t]"
		noisily dis "\centering"
		*noisily dis "\footnotesize"
		noisily dis "\begin{tabular}{l ccc}"
		noisily dis "\toprule \toprule"
		noisily dis " & \multicolumn{3}{c}{\bf{Panel A: Speakers}} \\[2mm]"
		noisily dis " & Before & During & After \\"
		noisily dis "\midrule"
		noisily dis " Hits & " %4.2f `m_before' " & " %4.2f `m_during' " & " %4.2f `m_after'  "\\"
		noisily dis " & (" %4.2f `sd_before' ") & (" %4.2f `sd_during' ") & (" %4.2f `sd_after' ") \\[2mm]"
		noisily dis "\midrule"
		noisily dis " Difference & " %4.2f `diff1' " &  & " %4.2f `diff2' "\\"
		noisily dis " P-value & " %3.2f `p1' " &  & " %4.2f `p2' "\\"
		noisily dis " N & " %3.0fc `N_before' " & " %3.0fc `N_during' " & " %3.0fc `N_after' " \\"
		noisily dis "\midrule\midrule"
		noisily dis " & \multicolumn{3}{c}{\bf{Panel B: Minority Leaders}} \\[2mm]"
		noisily dis " Hits & " %4.2f `lead_before' " & " %4.2f `lead_during' " & " %4.2f `lead_after'  "\\"
		noisily dis " & (" %4.2f `lead_sd_before' ") & (" %4.2f `lead_sd_during' ") & (" %4.2f `lead_sd_after' ") \\[2mm]"
		noisily dis "\midrule"
		noisily dis " Difference & " %4.2f `diff3' " &  & " %4.2f `diff4' "\\"
		noisily dis " P-value & " %3.2f `p3' " &  & " %4.2f `p4' "\\"
		noisily dis " N & " %3.0fc `lead_N_before' " & " %3.0fc `lead_N_during' " & " %3.0fc `lead_N_after' " \\"
		noisily dis "\bottomrule \bottomrule"
		noisily dis "\multicolumn{4}{p{.4\textwidth}}{Standard deviations are reported in parentheses. The pre and post-Speaker "
		noisily dis " periods are based on 5 years before and after the Speaker term. }\\"
		noisily dis "\end{tabular}"
		noisily dis "\end{table}"
		noisily dis "\end{document}"
		
		log close
	}


}

log using "log_stata_files.log", append

********************************************************************************
* Relative Coverage of City Offices Over Time
* Table 2: Impact of Switch from Mayor-Countil to Council-Manager City Government
* Table A.4: with linear, city-specific time trends 
********************************************************************************

if `make_mayors'==1 {
	
	use "C:\Users\Sean Hambali\Documents\GitHub\PPOL6081\replication-01\1_raw\hits_citygov.dta", clear

	replace city = upper(city)
	replace city = "SAN BERNARDINO" if state == "CA" & strpos(city, "SAN BERNARDIN") > 0
	replace city = "WILMINGTON"     if state == "DE" & city=="WILMING"
	replace city = "NATCHITOCHES"   if state == "LA" & strpos(city, "NATCHITOCH") > 0
	drop if city == "UNITED STATES OF AMERICA"
	drop if real(city) != .
	drop if city == ""
	drop if state == ""
	collapse (sum) mayor city_manager city_council mayor_x city_manager_x city_council_x, by(state city year)
	sort state city year
	tempfile citygov
	save `citygov', replace

	insheet using city_manager_dates.txt , names clear
	sort state city
	tempfile tmp1
	save `tmp1', replace
	
	use `citygov', replace
	replace city = upper(city)
	drop if state == ""
	drop if city == "OTTAWA" & state == "PA"
	drop if city == "OTTAWA" & state == "TX"
	drop if city == "WINNIPEG"
	drop if city == "VANCOUVER"
	replace city = "MOUNT VERNON" if city == "MT VERNON"
	sort state city
	merge state city using `tmp1'
	drop if year < 1876 | year > 1977
	
	sort state city year
	
	rename first_year x
	replace x = subinstr(x, "?" ,"" ,.)
	gen first_year = real(x)
	drop x
	
	save `tmp1', replace
	
	use `tmp1', replace
	
	egen total   = rsum(mayor   city_manager   city_council)
	egen total_x = rsum(mayor_x city_manager_x city_council_x)
	
	foreach i of varlist mayor city_manager city_council {
	  gen r_`i'  = `i'/total if total >= 50
	}
	
	foreach i of varlist mayor_x city_manager_x city_council_x {
	  gen r_`i'  = `i'/total_x if total_x >= 10
	}
	
	gen city_manager_govt = (year >= first_year)
	
	egen x = count(r_mayor), by(state city)
	
	drop if x < 10
	
	keep if (good_case == "***** currently council-manager" & first_year != .) | (good_case == "***** currently mayor-council")
	compress
	
	keep if year >= 1890
	tab year, gen(Y)
	
	gen state_city = state + " " + city
	
	areg r_mayor          city_manager_govt Y*, a(state_city) cluster(state_city)
	local b_1 =  _b[city_manager_govt]
	local s_1 = _se[city_manager_govt]
	local n_1 =  e(N)
	areg r_city_manager   city_manager_govt Y*, a(state_city) cluster(state_city)
	local b_2 =  _b[city_manager_govt]
	local s_2 = _se[city_manager_govt]
	local n_2 =  e(N)
	areg r_city_council   city_manager_govt Y*, a(state_city) cluster(state_city)
	
	areg r_mayor_x        city_manager_govt Y*, a(state_city) cluster(state_city)
	local b_3 =  _b[city_manager_govt]
	local s_3 = _se[city_manager_govt]
	local n_3 =  e(N)
	areg r_city_manager_x city_manager_govt Y*, a(state_city) cluster(state_city)
	local b_4 =  _b[city_manager_govt]
	local s_4 = _se[city_manager_govt]
	local n_4 =  e(N)
	areg r_city_council_x city_manager_govt Y*, a(state_city) cluster(state_city)
	
	
	
	quietly {
		capture log close
		set linesize 255
		log using "Table2_city_manager.tex", text replace
		noisily dis "\documentclass[11pt]{article}"
		noisily dis "\usepackage[margin=1in]{geometry}"
		noisily dis "\setlength{\pdfpagewidth}{8.5in} \setlength{\pdfpageheight}{11in}"
		noisily dis "\usepackage{booktabs}"
		noisily dis "\usepackage{makecell}"
		noisily dis "\usepackage{adjustbox}"
		noisily dis "\begin{document}"
		noisily display "\begin{table}[t] "
		noisily display "\centering "
		noisily display "\normalsize "
		noisily display "\begin{tabular}{lcccc} "
		noisily display "\toprule \toprule "
		noisily display " & \multicolumn{2}{c}{All Mentions} & \multicolumn{2}{c}{Using City Name Filter} \\[2mm] "
		noisily display " & Relative    & Relative     & Relative    & Relative     \\ "
		noisily display " & Coverage of & Coverage of  & Coverage of & Coverage of  \\ "
		noisily display " & Mayor       & City Manager & Mayor       & City Manager \\ "
		noisily display "\midrule "
		noisily display " Council-Manager  &  " %4.2f  `b_1' "  &  " %4.2f  `b_2' "  &  " %4.2f  `b_3' "  &  " %4.2f  `b_4' "  \\ "
		noisily display " \quad Govt Form  & (" %4.2f  `s_1' ") & (" %4.2f  `s_2' ") & (" %4.2f  `s_3' ") & (" %4.2f  `s_4' ") \\[2mm] "
		noisily display " N                &  " %3.0fc `n_1' "  &  " %3.0fc `n_2' "  &  " %3.0fc `n_3' "  &  " %3.0fc `n_4' "  \\[2mm] "
		noisily display " City Fixed Effects & Yes & Yes & Yes & Yes \\"
		noisily display " Year Fixed Effects & Yes & Yes & Yes & Yes \\"
		noisily display "\bottomrule \bottomrule "
		noisily display "\multicolumn{5}{p{.80\textwidth}}{Standard errors, clustered by city, are in parentheses.} \\ "
		noisily display "\end{tabular} "
		noisily display "\end{table} "
		noisily display "\end{document}"
		log close
	}

/* ADD CITY TIME TRENDS (for appendix graphs) */

tabulate state_city, generate(c)
egen tmp = group(state_city)
sum tmp
local n = r(max)
forvalues j = 1/`n' {
	gen trend`j' = year*c`j'
}

areg r_mayor          city_manager_govt Y* trend*, a(state_city) cluster(state_city)
local b_1 =  _b[city_manager_govt]
local s_1 = _se[city_manager_govt]
local n_1 =  e(N)
areg r_city_manager   city_manager_govt Y* trend*, a(state_city) cluster(state_city)
local b_2 =  _b[city_manager_govt]
local s_2 = _se[city_manager_govt]
local n_2 =  e(N)

areg r_mayor_x        city_manager_govt Y* trend*, a(state_city) cluster(state_city)
local b_3 =  _b[city_manager_govt]
local s_3 = _se[city_manager_govt]
local n_3 =  e(N)
areg r_city_manager_x city_manager_govt Y* trend*, a(state_city) cluster(state_city)
local b_4 =  _b[city_manager_govt]
local s_4 = _se[city_manager_govt]
local n_4 =  e(N)	
	
quietly {
	capture log close
	set linesize 255
	log using "TableA4_city_manager_trends.tex", text replace
	noisily dis "\documentclass[11pt]{article}"
	noisily dis "\usepackage[margin=1in]{geometry}"
	noisily dis "\setlength{\pdfpagewidth}{8.5in} \setlength{\pdfpageheight}{11in}"
	noisily dis "\usepackage{booktabs}"
	noisily dis "\usepackage{makecell}"
	noisily dis "\usepackage{adjustbox}"
	noisily dis "\begin{document}"
	noisily display "\begin{table}[t] "
	noisily display "\centering "
	noisily display "\normalsize "
	*noisily display "\caption{ {\bf Impact of Switch from Mayor-Council to Council-Manager City Government.}"
	*noisily display "Results from a difference-in-differences design suggest that the reform causes a large decrease in the relative coverage of mayors.\label{tab:city_manager_trends} } "
	noisily display "\begin{tabular}{lcccc} "
	noisily display "\toprule \toprule "
	noisily display " & \multicolumn{2}{c}{All Mentions} & \multicolumn{2}{c}{Using City Name Filter} \\[2mm] "
	noisily display " & Relative    & Relative     & Relative    & Relative     \\ "
	noisily display " & Coverage of & Coverage of  & Coverage of & Coverage of  \\ "
	noisily display " & Mayor       & City Manager & Mayor       & City Manager \\ "
	noisily display "\midrule "
	noisily display " Council-Manager  &  " %4.2f  `b_1' "  &  " %4.2f  `b_2' "  &  " %4.2f  `b_3' "  &  " %4.2f  `b_4' "  \\ "
	noisily display " \quad Govt Form  & (" %4.2f  `s_1' ") & (" %4.2f  `s_2' ") & (" %4.2f  `s_3' ") & (" %4.2f  `s_4' ") \\[2mm] "
	noisily display " N                &  " %3.0fc `n_1' "  &  " %3.0fc `n_2' "  &  " %3.0fc `n_3' "  &  " %3.0fc `n_4' "  \\[2mm] "
	noisily display " City Fixed Effects & Yes & Yes & Yes & Yes \\"
	noisily display " Year Fixed Effects & Yes & Yes & Yes & Yes \\"
	noisily display " City-Specific Time Trends & Yes & Yes & Yes & Yes \\"
	noisily display "\bottomrule \bottomrule "
	noisily display "\multicolumn{5}{p{.80\textwidth}}{Standard errors, clustered by city, are in parentheses.} \\ "
	noisily display "\end{tabular} "
	noisily display "\end{table} "
	noisily display "\end{document}"
	log close
}

	
	
	
log using "log_stata_files.log", append
	
	**produce dta for graph
	use `tmp1', clear
	
	gen city_manager_govt = (year >= first_year)
	
	gen t = year - first_year + .5
	
	foreach z in "mayor" "city_manager" "city_council" {
		egen mean_`z'_tmp = sum(`z') if city_manager_govt == 0, by(year)
		egen control_mean_`z' = max(mean_`z'_tmp), by(year)
		drop mean_`z'_tmp
	}
	
	drop if state == ""
	
	collapse (sum) mayor city_manager city_council mayor_x city_manager_x city_council_x control_*, by(state city t)
	
	egen total   = rsum(mayor   city_manager   city_council)
	egen total_x = rsum(mayor_x city_manager_x city_council_x)
	egen total_control = rsum(control_mean_mayor control_mean_city_manager control_mean_city_council)
	
	keep if t >= -20 & t <= 20
	
	foreach i of varlist mayor city_manager city_council {
	  gen r_`i'  = `i'/total if total >= 50
	}
	
	foreach i of varlist mayor_x city_manager_x city_council_x {
	  gen r_`i'  = `i'/total_x if total_x >= 10
	}
	
	foreach i of varlist mayor city_manager city_council {
	  gen r_control_`i'  = control_mean_`i'/total_control if total_control >= 50
	}
	
	egen xmin = min(t), by(state city)
	egen xmax = max(t), by(state city)
	sort state city t
	list state city if city != city[_n-1] & xmin < 0 & xmax > 0, noo nod
	
	gen rel_mayor_council_total = r_city_council + r_mayor
	gen rel_mayor_council_control_total = r_control_city_council + r_control_mayor
	gen rel_mayor_council = r_mayor / rel_mayor_council_total
	gen rel_mayor_council_control = r_control_mayor / rel_mayor_council_control_total
	
	gen rel_mayor_council_total_x = r_city_council_x + r_mayor
	gen rel_mayor_council_x = r_mayor_x / rel_mayor_council_total_x  
	
	collapse (mean) r_mayor r_city_manager r_city_council r_mayor_x r_city_manager_x r_city_council_x r_control* rel_mayor_council rel_mayor_council_control rel_mayor_council_x (semean) r_mayor_sd=r_mayor r_city_manager_sd=r_city_manager r_city_council_sd=r_city_council r_mayor_x_sd=r_mayor_x r_city_manager_x_sd=r_city_manager_x r_city_council_x_sd=r_city_council_x r_control_mayor_sd=r_control_mayor r_control_city_council_sd=r_control_city_council r_control_city_manager_sd=r_control_city_manager rel_mayor_council_sd=rel_mayor_council rel_mayor_council_control_sd=rel_mayor_council_control rel_mayor_council_x_sd=rel_mayor_council_x, by(t)
	
	saveold for_mayor_r_graph, replace version(12)
}




********************************************************************************
* Relative Coverage of Congress in Tariff Policymaking
********************************************************************************

if `make_rtaa'==1 {
	
	use "C:\Users\Sean Hambali\Documents\GitHub\PPOL6081\replication-01\1_raw\hits_tariff.dta", clear
	keep if year >= 1880 & year<=1975
	
	gen t = year - mod(year,5)
	collapse (sum) cong pres, by(t)
	
	gen r = cong/(cong+pres)
	
	reg r t if t < 1932.5
	predict r1 if e(sample)
	
	reg r t if t > 1932.5
	predict r2 if e(sample)
	
	scatter r r1 r2 t , xline(1932.5) s(i i i) mlab(t) mlabpos(0) mlabsize(*.80) c(. l l) legend(off) xtitle(Period) ytitle(Relative Mentions of Cong vs. Pres on Tariff)
	
	gen tt = t - 1935
	gen post = tt >= 0 if tt != .
	gen post_tt = post * tt
	
	reg r post
	reg r post
	reg r post tt
	reg r post tt post_tt
	
	saveold for_tariff_r_graph, version(12) replace
}



********************************************************************************
* Party Committee Power Over Time in Nine U.S. States
********************************************************************************


if `make_party_committee' == 1 {
	
	use "C:\Users\Sean Hambali\Documents\GitHub\PPOL6081\replication-01\1_raw\hits_candidates.dta", clear

	merge 1:1 state year using hits_partycommittee
	keep if _merge==3
	drop _merge
	drop if state == "DC" | state =="V" | state=="Victoria" | state=="NSW" | state=="New South Wales"
	* drop obs for states before they were admitted to the union
	drop if state == "ND" & year < 1889
	drop if state == "SD" & year < 1889
	drop if state == "MT" & year < 1889
	drop if state == "WA" & year < 1888
	drop if state == "ID" & year < 1900
	drop if state == "WY" & year < 1900
	drop if state == "UT" & year < 1896
	drop if state == "OK" & year < 1907
	drop if state == "AZ" & year < 1912
	drop if state == "NM" & year < 1912
	drop if state == "AK" & year < 1958
	drop if state == "HI" & year < 1958
	
	merge m:1 state using Mayhew_TPO_Scores
	drop if _merge==2
	drop _merge	
	
	gen n_othr = democ_othr + repub_othr
	gen party_power = 100*n_othr/(n_othr + candidate_hits) if candidate_hits>500

	egen x = mean(party_power), by(state)
	gen party_power_norm = party_power/x
	
	keep state year party_power_norm
	saveold for_partycommittee_r_graph, version(12) replace
}

log close
