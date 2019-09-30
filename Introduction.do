
*********************************************
* Pre-Semester Course in Statistics 2019
*********************************************

**********************************************************************************************
* Tutor: Nan Hu
**********************************************************************************************

**********************************************************************************************
* prepare the datasets for match: convert all datasets into certain frequency (e.g. monthly)
**********************************************************************************************
*-- Laptop Path

clear all   
set maxvar 3000
set mem 1000m   
set more off   


gl rawdata "C:\ado\personal\rawdata" 
gl tempdata "C:\ado\personal\tempdata"
gl dofile "C:\ado\personal\dofile"
gl logfile "C:\ado\personal\logfile"
gl output "C:\ado\personal\output"


capture log close 
log using $logfile\introduction.log,replace

*********************************************
* (1)Data management
*********************************************

* read data
*import excel "$rawdata\finance.xls", sheet("Sheet1") clear
use $rawdata\finance.dta,clear

* describe
des
des inequality 

* summarize: mean, sd ,...
sum
sum finance
sum finance, detail
sum finance if region == 1
sum inequality //
return list 

scalar mn_ineq=r(mean) 
di mn_ineq   


*** list *** 
list gdp province if region==1
list gdp province if region==1 & gdp>10000
list gdp region if region==1 | region==2 
list gdp in 1/10 

* tab
tabulate region 
gen highincome= 1 if gdp> 10000
replace highincome=0 if gdp<10000
tabulate region highincome
tab gdp

* table
table region
table region, contents(n wage)
table region, c(n wage mean wage median gdp)


tabstat gdp, statistics(mean median sd variance)
                       

*********************************************
* (2)generate new variables
*********************************************
							
use $rawdata\finance.dta,clear

*** gen & egen
gen id=_n
dis _N
gen ineq1=ineq*100

egen maxgdp=max(gdp) 
egen mgdp=mean(gdp) 
  
gen cum_gdp=sum(gdp) 
egen tot_gdp=sum(gdp)  
list gdp cum_gdp tot_gdp    

gen cum_ineq=sum(ineq1)   
egen tot_ineq=sum(ineq1) 
list cum_ineq tot_ineq  //Pay attention to the difference

***gen dummy variables 
tab region 
gen east=1 if region==1 //
replace east=0 if east==. //


gen east1=region==1 if !missing(region)// 
gen central=region==2 if !missing(region)
gen west=region==3 if !missing(region) //

* rename
rename inequality ineq

* label
label var gdp "provincial GDP per capita" //
des 

br region
label define lregion 1 "east" 2 "central" 3 "west" //
label list lregion

label values region lregion  //
br region

*** count 
count //
count if wage>30000 //
count if wage>30000 & wage!=.
replace wage =-99999 if wage==. 

*> larger < smaller  != not equal  == equal

*** order 
order province ineq ineq1 cum_ineq tot_ineq
browse
edit

*** by, sort 
br region
sort region
by region: egen mn_ineq=mean(ineq) 		
tab mn_ineq	
		   
by region, sort : egen mn_ineq1=mean(ineq)
bysort region: egen mn_ineq1=mean(ineq)
					   

*********************************************
* (3)keep & drop , append & merge
*********************************************

*** merge	***
use $rawdata\finance.dta,clear
gen id=_n    //
keep id province gdp inequality
sort id
save $tempdata\finance1.dta,replace

use $rawdata\finance.dta,clear
gen id=_n
drop province gdp inequality
sort id //
save $tempdata\finance2.dta,replace	

merge 1:1 id using 	$tempdata\finance1.dta	 
							
use $rawdata\finance.dta,clear //
gen id=_n
keep id province gdp inequality
drop if province==""
sort id
save $tempdata\finance3.dta,replace

merge 1:1 id using 	$tempdata\finance2.dta	//
* master data : using data							
							
***appendֵ***
use $rawdata\finance.dta,clear
keep if region==1
save $tempdata\east.dta,replace

use $rawdata\finance.dta,clear
keep if region==2
save $tempdata\central.dta,replace

use $rawdata\finance.dta,clear
keep if region==3 

append using $tempdata\east.dta  $tempdata\central.dta
							
erase $tempdata\east.dta   //
erase $tempdata\central.dta



*********************************************
* (4)regression & graph
*********************************************
use $rawdata\finance.dta,clear
gen ineq1=ineq*100
rename inequality ineq
gen id = _n

** full sample 
histogram ineq1
twoway (scatter ineq finance, mlabel(province)) (lfit ineq finance), xscale(range(0 100)) yscale(range(2 5))
//graph save $output\yx_full,replace
reg ineq1 finance 

** subsample 
twoway (scatter ineq finance, mlabel(province)) (lfit ineq finance) if province!="Tibet", xscale(range(0 100)) yscale(range(2 5))
//graph save $output\yx_sub,replace

reg ineq1 finance if province!="Tibet"

preserve 
drop if province=="Tibet"
reg ineq1 finance 
restore 

preserve 
keep if province!="Tibet"
reg ineq1 finance
restore

***Units of Measurement ***
sum ineq ineq1
reg ineq finance
reg ineq1 finance

gen financedec = finance/100
sum finance financedec
reg ineq finance 
reg ineq financedec

*****regresion output*********
drop if province=="Tibet"
reg ineq1 finance


capture drop ybar 
egen ybar=mean(ineq1)
gen y_deviation=ineq1-ybar
gen sq_y_devia=y_deviation*y_deviation
egen sst=sum(sq_y_devia)
list sst in 1/10
di sst


predict yhat
gen model=yhat-ybar
gen sqmodel=model*model
egen sse=sum(sqmodel)
di sse



gen uhat=ineq1-yhat
predict uhat1,res 
list uhat uhat1 
gen sq_uhat=uhat*uhat    
egen ssr=sum(sq_uhat)   
di ssr


sca r2=sse/sst
di r2

*ROOT MSE
sca r_mse=(ssr[1]/29)^(0.5) 
di r_mse

****yhat & uhat
sum uhat   
gen xu=finance*uhat
sum xu
gen yuhat=yhat*uhat
sum yuhat

****graphic "yhat uhat x" ****
sum yhat uhat finance
twoway (scatter uhat id, mlabel(province)) , xscale(range(0 35)) yscale(range(-80 110))
//graph save $output\uhat,replace
twoway (scatter finance uhat,  mlabel(province)) , xscale(range(-80 110)) yscale(range(0 40))
//graph save $output\xuhat,replace
twoway (scatter yhat uhat, mlabel(province)) , xscale(range(-80 110)) yscale(range(260 360 ))
//graph save $output\yuhat,replace


save $tempdata\finance_final.dta,replace
capture log close





