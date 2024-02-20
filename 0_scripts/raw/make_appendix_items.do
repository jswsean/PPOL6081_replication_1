log using "log_stata_files.log", append

********************************************************************************
* This do-file replicates the appendix tables and figures in
* "How Newspapers Reveal Political Power" (Ban, Fouirnaies, Hall, Snyder)
*
* Produces:
* Table A.1: Number of Pages, Newspapers, and Counties in Dataset by State
* Table A.2: The 50 Most Common Newspapers in Dataset
* Figure A.1: Geographical Distribution of Pages and Newspapers in Dataset
* Figure A.2: Yearly Number of Pages in Sample by Region
*
* November 20, 2016
********************************************************************************

set more off
	
********************************************************************************
* Table A.1: Number of Pages, Newspapers, and Counties in Dataset by State     
********************************************************************************

use DescriptiveStatsMetadata_25years, clear
replace pages = pages/1000
reshape wide pages newspapers counties, i(state) j(period)
foreach var of varlist pages* newspapers* counties* {
	replace `var' = 0  if `var'==.
}
sort state

quietly {
	cap log close
	set linesize 255
	log using "TableA1_NumberOfPagesByState.tex", text replace
	noisily dis "\documentclass[11pt]{article}"
	noisily dis "\usepackage[margin=1in]{geometry}"
	noisily dis "\setlength{\pdfpagewidth}{8.5in} \setlength{\pdfpageheight}{11in}"
	noisily dis "\usepackage{booktabs}"
	noisily dis "\usepackage{makecell}"
	noisily dis "\usepackage{adjustbox}"
	noisily dis "\begin{document}"
	noisily dis " \begin{table}[t] "
	noisily dis " \centering "
	*noisily dis " \caption{Number of Pages, Newspapers and Counties in Dataset by State}\label{tab:total_pages} "
	noisily dis " \footnotesize "
	noisily dis " \begin{adjustbox}{max width=\textwidth} "
	noisily dis "\begin{tabular}{l ccc c ccc c ccc c ccc | c ccc}"
	noisily dis "\toprule \toprule"
	*noisily dis "  & \multicolumn{2}{c}{\rotatebox[origin=c]{90}{1877-1902}}  &  \multicolumn{2}{c}{\rotatebox[origin=c]{90}{1903-1927}} & \multicolumn{2}{c}{\rotatebox[origin=c]{90}{1928-1952}} & \multicolumn{2}{c}{\rotatebox[origin=c]{90}{1953-1977}} & \multicolumn{2}{c}{\rotatebox[origin=c]{90}{Total}} \\"
	noisily dis "  & \multicolumn{3}{c}{1877-1902}  &&  \multicolumn{3}{c}{1903-1927} && \multicolumn{3}{c}{1928-1952} && \multicolumn{3}{c}{1953-1977} && \multicolumn{3}{c}{Total} \\"	
	noisily dis " \cline{2-4}  \cline{6-8}  \cline{10-12}  \cline{14-16}  \cline{18-20} \\ "  
	noisily dis "  & \rotatebox[origin=c]{90}{\makecell{Pages \\ (1000s)}}   & \rotatebox[origin=c]{90}{Papers} & \rotatebox[origin=c]{90}{Counties}  && "
	noisily dis " \rotatebox[origin=c]{90}{\makecell{Pages \\ (1000s)}}   & \rotatebox[origin=c]{90}{Papers} & \rotatebox[origin=c]{90}{Counties} && "
	noisily dis "  \rotatebox[origin=c]{90}{\makecell{Pages \\ (1000s)}}   & \rotatebox[origin=c]{90}{Papers} & \rotatebox[origin=c]{90}{Counties} && "
	noisily dis " \rotatebox[origin=c]{90}{\makecell{Pages \\ (1000s)}} & \rotatebox[origin=c]{90}{Papers} & \rotatebox[origin=c]{90}{Counties}  && "
	noisily dis " \rotatebox[origin=c]{90}{\makecell{Pages \\ (1M)}}   & \rotatebox[origin=c]{90}{Papers} & \rotatebox[origin=c]{90}{Counties} \\"
	noisily dis "\midrule"

	forval i =1/50  {  //
	preserve
	noisily dis " " state[`i'] " & " %3.1f pages1877[`i'] " & " %2.0f newspapers1877[`i'] " & " %2.0f counties1877[`i'] " & "
	noisily dis " & " %3.1f pages1902[`i'] " & " %2.0f newspapers1902[`i'] " & " %2.0f counties1902[`i'] " & "  
	noisily dis " & " %3.1f pages1927[`i'] " & " %2.0f newspapers1927[`i'] " & " %2.0f counties1927[`i'] " & "  
	noisily dis " & " %3.1f pages1952[`i'] " & " %2.0f newspapers1952[`i'] " & " %2.0f counties1952[`i'] " & "
	*total
	use "DescriptiveStatsMetadata_allyears.dta", clear
	replace pages = pages/1000000
	noisily dis " & " %3.1f pages[`i'] " & " %2.0f newspapers[`i'] " & " %2.0f counties[`i'] " \\"
	restore
	}
	noisily dis " \midrule "	
	*All states
	collapse (sum) newspapers* counties* pages* 
	local i =1
	noisily dis "  All States  & " %3.1f pages1877[`i'] " & " %2.0f newspapers1877[`i'] " & " %2.0f counties1877[`i'] " & "
	noisily dis " & " %3.1f pages1902[`i'] " & " %2.0f newspapers1902[`i'] " & " %2.0f counties1902[`i'] " & "  
	noisily dis " & " %3.1f pages1927[`i'] " & " %2.0f newspapers1927[`i'] " & " %2.0f counties1927[`i'] " & "  
	noisily dis " & " %3.1f pages1952[`i'] " & " %2.0f newspapers1952[`i'] " & " %2.0f counties1952[`i'] " & "
	
	use "DescriptiveStatsMetadata_allyears.dta", clear
	collapse (sum) pages newspapers counties
	foreach var of varlist pages* {
		replace `var' = `var'/1000000
	}
	noisily dis " & " %3.1f pages[`i'] " & " %2.0f newspapers[`i'] " & " %2.0f counties[`i'] " \\"

	noisily dis " \midrule "	
	noisily dis "\bottomrule \bottomrule"	
	noisily dis "\end{tabular}"
	noisily dis "\end{adjustbox}"
	noisily dis "\end{table}"
	noisily dis "\end{document}"
	
	log close
}

log using "log_stata_files.log", append

********************************************************************************
* Table A.2: The 50 Most Common Newspapers in Dataset
********************************************************************************

use newspaper_summary, clear

drop if regexm(newspaper, "Winnipeg") == 1

sum min_year
local m = r(N)

replace newspaper = regexr(newspaper, "\&", "\&")

*** rank in terms of occurrences
gsort -occurrences
gen rank = _n

*** get rid of "The" for sorting alphabetically
gen newspaper_sort_name = newspaper
replace newspaper_sort_name = regexr(newspaper, "^The ", "")
sort newspaper_sort_name

*** table can only fit top 50
keep if rank <= 50

cap log close
set linesize 255
quietly {
	log using "TableA2_newspapers.tex", text replace
	noisily dis "\documentclass[11pt]{article}"
	noisily dis "\usepackage[margin=1in]{geometry}"
	noisily dis "\setlength{\pdfpagewidth}{8.5in} \setlength{\pdfpageheight}{11in}"
	noisily dis "\usepackage{booktabs}"
	noisily dis "\usepackage{makecell}"
	noisily dis "\usepackage{adjustbox}"
	noisily dis "\begin{document}"
	noisily dis "\begin{table}[t]"
	noisily dis "\centering"
	noisily dis "\footnotesize"
	noisily dis "\begin{tabular}{lcccc}"
	noisily dis "\toprule \toprule"
	noisily dis "Newspaper & Pages & First Year & Last Year & State \\ \midrule"
	forvalues j=1/50 {
		noisily dis newspaper[`j'] " & " %8.0fc occurrences[`j'] " & " %4.0f min_year[`j'] " & " %4.0f max_year[`j'] " & " state[`j'] "\\"
	}

	noisily dis "\bottomrule \bottomrule"
	noisily dis "\end{tabular}"
	noisily dis "\end{table}"
	noisily dis "\end{document}"
	log off
}

********************************************************************************
* Figure A.1: Geographical Distribution of Pages and Newspapers in Dataset
********************************************************************************

use "DescriptiveStatsMetadata_allyears.dta", clear

replace pages = pages / 1000000
format newspapers %3.0f
rename state STATE
merge 1:1 STATE using "usdb.dta"
rename STATE state
drop if state=="AK" | state=="HI" | state=="VI" | state=="AS" | state=="GU" | state=="DC" | state=="PR"
spmap pages using uscoord , id(_ID) fcolor(Blues) legtitle("Total Number of Pages (millions)") clmethod(custom) clbreaks( 0 .1 .25 .5 1 2 4 8 ) label(x(LON) y(LAT) label(newspapers) color(black)) 
graph export "FigA1_MapNewspapers.pdf", replace 


********************************************************************************
* Figure A.2: Yearly Number of Pages in Sample by Region
********************************************************************************


use "DescriptiveStatsMetadata_collapsed.dta", clear
replace RegionCensus ="NorthEast" if RegionCensus=="North East"

collapse (sum) pages cities counties newspapers ,by(RegionCensus year)
reshape  wide pages cities counties newspapers , i(year) j(RegionCensus) string
keep if year>=1877 & year<=1977
label var pagesMidwest "Midwest"
label var pagesWest "West"
label var pagesSouth "South"
label var pagesNorthEast "North East" 

label var newspapersMidwest "Midwest"
label var newspapersWest "West"
label var newspapersSouth "South"
label var newspapersNorthEast "North East" 

twoway  (line pagesMidwest year, lpattern(dash)) (line pagesWest year, lpattern(dot)) (line pagesSouth year, lpattern(longdash))  (line pagesNorthEast year), legend(label()) tlabel(1877 (10) 1977) graphregion(color(white)) ytitle("Yearly Number of Pages") ttitle("Year") 
graph export "FigA2_PagesByRegionYear.pdf", replace



log close
