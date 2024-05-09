clear
set more off

cd "D:\National Asthma and COPD Audit Programme (NACAP)\2018 Secondary Care Clinical"


/* create log file */
capture log close
log using build_logs/2018ImportSCC, text replace


import delim admissionid hospital trustcode patientid overseas age gender lsoa11 ///
			q15gp_prac q16adate q16btime q17adate q17btime q21physrev q31resprev ///
			q32adate q32btime q41decafrec q42decaf q51o2presc q52o2targ q52o2targoth ///
			q61niv q62adate q62anodate q62btime q62bnotime q71spir q72fev1 q73fvc ///
			q73aratio q74date q81smokstatus q82smokcess died q91date q93bts q93btsoth ///
			q94followup q94no q94assteam q94gpfuadvised q94gpfuarranged q94phone ///
			q94commadvised q94commarranged q94hospadvised q94hosparranged ///
			q94otherhealthcare q94eol q94other q94followupoth ///
			using "raw_data/NACAP-COPD-20170914-20180930-v104.csv", asdouble clear


//Code gender
replace gender = "1" if gender == "Male"
replace gender = "2" if gender == "Female"
destring gender, replace
label define sex 1 "Male" 2 "Female"
label values gender sex

//Generate datetime variables
local datetimevars "16 17 32 62"
foreach datevar of local datetimevars {

	gen q`datevar'a = date(q`datevar'adate, "DMY")
	format %td q`datevar'a
	order q`datevar'a, after(q`datevar'adate)
	
	gen double q`datevar'b = clock(q`datevar'adate + q`datevar'btime, "DMY hm")
	format %tc q`datevar'b
	order q`datevar'b, after(q`datevar'btime)
	
	drop q`datevar'adate
	drop q`datevar'btime
}
label var q16a "Date of arrival"
label var q16b "Time of arrival"
label var q17a "Date of admission to unit"
label var q17b "Time of admission to unit"
label var q32a "Date of first review by a member of the respiratory team"
label var q32b "Time of first review by a member of the respiratory team"
label var q62a "Date of NIV first commencement"
label var q62b "Time of NIV first commencement"

//Encode categorical variables
local ynncvars "q21physrev q41decafrec"
label define yn_nc 0 "No" 1 "Yes" 2 "Not clear"
foreach ynncvar of local ynncvars {

	replace `ynncvar' = "0" if `ynncvar' == "No"
	replace `ynncvar' = "1" if `ynncvar' == "Yes"
	replace `ynncvar' = "2" if `ynncvar' == "Not clear"
	destring `ynncvar', replace
	label values `ynncvar' yn_nc
}

local ynvars "q31resprev q61niv q71spir"
label define yn 0 "No" 1 "Yes"
foreach ynvar of local ynvars {
	
	replace `ynvar' = "0" if `ynvar' == "No"
	replace `ynvar' = "1" if `ynvar' == "Yes"
	destring `ynvar', replace
	label values `ynvar' yn
}

replace q51o2presc = "0" if q51o2pres == "No"
replace q51o2presc = "1" if q51o2pres == "Yes"
replace q51o2presc = "2" if q51o2pres == "Not needed"
destring q51o2presc, replace
label define yn_nn 0 "No" 1 "Yes" 2 "Not needed"
label values q51o2presc yn_nn

replace q52o2targ = "1" if q52o2targ == "88-92%"
replace q52o2targ = "2" if q52o2targ == "94-98%"
replace q52o2targ = "3" if q52o2targ == "Other"
replace q52o2targ = "4" if q52o2targ == "Target range not stipulated"
destring q52o2targ, replace
label define o2pres 1 "88-92%" 2 "94-98%" 3 "Other" 4 "Target range not stipulated"
label values q52o2targ o2pres

local dateortime "anodate bnotime"
foreach dt of local dateortime {

	replace q62`dt' = "1" if q62`dt' == "X"
	destring q62`dt', replace
}

local datevars "q74 q91"
foreach datevar of local datevars {
	
	gen `datevar' = date(`datevar'date, "DMY")
	format %td `datevar'
	order `datevar', after(`datevar'date)
	drop `datevar'date
}
label var q74 "Date of this spirometry"
label var q91 "Date of discharge from your hospital"

replace q81smokstatus = "1" if q81smokstatus == "Never smoked"
replace q81smokstatus = "2" if q81smokstatus == "Ex-smoker"
replace q81smokstatus = "3" if q81smokstatus == "Current smoker"
replace q81smokstatus = "4" if q81smokstatus == "Not recorded"
destring q81smokstatus, replace
label define smokstat 1 "Never smoked" 2 "Ex-smoker" 3 "Current smoker" 4 "Not recorded"
label values q81smokstatus smokstat

replace q82smokcess = "0" if q82smokcess == "No"
replace q82smokcess = "1" if q82smokcess == "Yes"
replace q82smokcess = "2" if q82smokcess == "Not recorded"
replace q82smokcess = "3" if q82smokcess == "Offered but declined"
destring q82smokcess, replace
label define smokcess 0 "No" 1 "Yes" 2 "Not recorded" 3 "Offered but declined"
label values q82smokcess smokcess

replace died = "1" if died != ""
replace died = "0" if died == ""
destring died, replace

replace q93bts = "0" if q93bts == "No"
replace q93bts = "1" if q93bts == "Yes"
replace q93bts = "2" if q93bts == "Not clear"
replace q93bts = "3" if q93bts == "Patient self discharged"
replace q93bts = "5" if q93bts == "Other"
destring q93bts, replace
label define bts 0 "No" 1 "Yes" 2 "Not clear" 3 "Patient self discharged" 4 "Patient died" 5 "Other"
label values q93bts bts
replace q93bts = 4 if died == 1

replace q94no = . if q94followup == ""
replace q94assteam = . if q94followup == ""
replace q94gpfuadvised = . if q94followup == ""
replace q94gpfuarranged = . if q94followup == ""
replace q94phone = . if q94followup == ""
replace q94commadvised = . if q94followup == ""
replace q94commarranged = . if q94followup == ""
replace q94hospadvised = . if q94followup == ""
replace q94hosparranged = . if q94followup == ""
replace q94otherhealthcare = . if q94followup == ""
replace q94eol = . if q94followup == ""
replace q94other = . if q94followup == ""

label values died q94no-q94other yn

drop q94followup

//not needed
tab draftstatus   //drafts no longer included
drop draftstatus

//Sort by hospital and arrival date/time
order patientid
gsort patientid q16b

compress
save stata_data/SCC_2018, replace


log close
