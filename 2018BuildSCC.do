clear
set more off

cd "D:\National Asthma and COPD Audit Programme (NACAP)\2018 Secondary Care Clinical"


/* create log file */
capture log close
log using build_logs/2018BuildSCC, text replace


use stata_data/SCC_2018, clear


//Generate No. of admissions for each hospital
gen byte admission = 1
bysort hospital: gen hospadmissions = sum(admission)
by hospital: replace hospadmissions = hospadmissions[_N]
drop admission
order hospadmissions, after(hospital)
gsort patientid q16b


//remove overseas patients
drop if overseas == 1
drop overseas


//Check for nonsense dates
codebook q16a q16b q17a q17b q32a q32b q62a q62b q91  //check for variables with missing data
drop if q17b < q16b             //Admission before arrival
drop if q17a > q91              //Admission after discharge
drop if q32b < q16b             //Respiratory specialist review before arrival
drop if q32a > q91 & q32a != .  //Respiratory specialist review after discharge
drop if q62b < q16b             //NIV before arrival
drop if q62a > q91 & q62a != .  //NIV after discharge
drop if q91 < q16a              //Discharge before arrival
count


//Generate time categories
local timevars "q17b q32b"

label define times 1 "00:00-01:59" 2 "02:00-03:59" 3 "04:00-05:59" 4 "06:00-07:59" ///
						5 "08:00-09:59" 6 "10:00-11:59" 7 "12:00-13:59" 8 "14:00-15:59" ///
						9 "16:00-17:59" 10 "18:00-19:59" 11 "20:00-21:59" 12 "22:00-23:59"

foreach timevar of local timevars {

	gen `timevar'time = `timevar' - cofd(dofc(`timevar'))  //remove date from date and time
	order `timevar'time, after(`timevar')
	format %tc_HH:MM `timevar'time
	
	gen `timevar'cat = 1 if `timevar'time >= clock("01/01/1960 00:00", "DMY hm") ///
							& `timevar'time < clock("01/01/1960 02:00", "DMY hm")
	replace `timevar'cat = 2 if `timevar'time >= clock("01/01/1960 02:00", "DMY hm") ///
							& `timevar'time < clock("01/01/1960 04:00", "DMY hm")
	replace `timevar'cat = 3 if `timevar'time >= clock("01/01/1960 04:00", "DMY hm") ///
							& `timevar'time < clock("01/01/1960 06:00", "DMY hm")
	replace `timevar'cat = 4 if `timevar'time >= clock("01/01/1960 06:00", "DMY hm") ///
							& `timevar'time < clock("01/01/1960 08:00", "DMY hm")
	replace `timevar'cat = 5 if `timevar'time >= clock("01/01/1960 08:00", "DMY hm") ///
							& `timevar'time < clock("01/01/1960 10:00", "DMY hm")
	replace `timevar'cat = 6 if `timevar'time >= clock("01/01/1960 10:00", "DMY hm") ///
							& `timevar'time < clock("01/01/1960 12:00", "DMY hm")
	replace `timevar'cat = 7 if `timevar'time >= clock("01/01/1960 12:00", "DMY hm") ///
							& `timevar'time < clock("01/01/1960 14:00", "DMY hm")
	replace `timevar'cat = 8 if `timevar'time >= clock("01/01/1960 14:00", "DMY hm") ///
							& `timevar'time < clock("01/01/1960 16:00", "DMY hm")
	replace `timevar'cat = 9 if `timevar'time >= clock("01/01/1960 16:00", "DMY hm") ///
							& `timevar'time < clock("01/01/1960 18:00", "DMY hm")
	replace `timevar'cat = 10 if `timevar'time >= clock("01/01/1960 18:00", "DMY hm") ///
							& `timevar'time < clock("01/01/1960 20:00", "DMY hm")
	replace `timevar'cat = 11 if `timevar'time >= clock("01/01/1960 20:00", "DMY hm") ///
								& `timevar'time < clock("01/01/1960 22:00", "DMY hm")
	replace `timevar'cat = 12 if `timevar'time >= clock("01/01/1960 22:00", "DMY hm") ///
								& `timevar'time < clock("02/01/1960 00:00", "DMY hm")
	
	label values `timevar'cat times
	order `timevar'cat, after(`timevar'time)
	drop `timevar'time
}
label var q17bcat "Admission time window"
label var q32bcat "Respiratory review time window"


//Generate days of week for specialist review
gen byte specrevday = dow(q32a)+1  //plus one is so that days are 1-7, rather than 0-6
order specrevday, after(q32b)
label define days 1 "Sunday" 2 "Monday" 3 "Tuesday" 4 "Wednesday" 5 "Thursday" 6 "Friday" 7 "Saturday"
label values specrevday days


//Time from arrival to admission
gen double admitwaithours = (((q17b-q16b)/1000)/60)/60
order admitwaithours, after(q17b)
replace admitwaithours = . if q17b < q16b  //remove admissions before arrivals
replace admitwaithours = . if q17a > q91   //remove admissions after discharge
drop if admitwaithours >= 24               //Not realistic to have an admission wait over 24 hours
count


//Time from admission to specialist review (for those who were seen)
gen double reviewwaithours = (((q32b-q17b)/1000)/60)/60
order reviewwaithours, after(q32b)
replace reviewwaithours = . if q32b < q16b          //remove reviews before arrival
replace reviewwaithours = . if q32a > q91           //remove reviews after discharge
drop if reviewwaithours <= -24                      //not realistic to have a wait time less than this
//drop if reviewwaithours > 72 & reviewwaithours != . //not realistic to be longer than this
count

gen byte reviewwithin24hrs = 1 if reviewwaithours <= 24
replace reviewwithin24hrs = 0 if reviewwaithours > 24    //includes missing values
order reviewwithin24hrs, after(reviewwaithours)
label values reviewwithin24hrs yn


//Generate binary oxygen prescription variable (for calculating odds ratios)
gen byte o2bin = q51o2presc
replace o2bin = . if o2bin == 2  //'Not needed' not included in denominator
order o2bin, after(q51o2presc)


//Time from arrival to NIV (for those who received it)
gen double nivwaithours = (((q62b-q16b)/1000)/60)/60
order nivwaithours, after(q62b)
replace nivwaithours = . if nivwaithours < 0  //remove NIV administrations before arrival
replace nivwaithours = . if q62a > q91        //remove NIV administration after discharge

gen byte nivwithin2hrs = 1 if nivwaithours < 2
replace nivwithin2hrs = 0 if nivwaithours >= 2
replace nivwithin2hrs = 2 if q62anodate == 1 | q62bnotime == 1
replace nivwithin2hrs = . if q61niv != 1
order nivwithin2hrs, after(nivwaithours)
label define nivwait 0 "No" 1 "Yes" 2 "No time recorded"
label values nivwithin2hrs nivwait

//Generate binary NIV within 2 hours variable
gen byte niv2hrbin = nivwithin2hrs
replace niv2hrbin = 0 if niv2hrbin == 2  //'No time recorded' to be counted as no NIV in 3 hours
order niv2hrbin, after(nivwithin2hrs)

//Generate categorical time from arrival to NIV variable (exposure variable for analysis)
label define nivtimelabel 1 "<2 Hours" 2 "2-24 Hours" 3 ">24 Hours"
gen byte nivtime = 1 if nivwaithours < 2
replace nivtime = 2 if nivwaithours >= 2 & nivwaithours <= 24
replace nivtime = 3 if nivwaithours > 24 & nivwaithours != .
label values nivtime nivtimelabel
order nivtime, after(niv2hrbin)


//Clean FEV1/FVC ratio
gen double fev1fvcratio = q72fev1/q73fvc
order fev1fvcratio, after(q73aratio)
replace fev1fvcratio = . if fev1fvcratio < 0.2 | fev1fvcratio > 1
drop q73aratio
tab q71spir if fev1fvcratio == .  //No. of invalid results ('Yes' category)

//Those with and without airflow obstruction
gen obstruction = 0 if fev1fvcratio != .
replace obstruction = 1 if fev1fvcratio < 0.7
replace obstruction = 2 if fev1fvcratio == . & q71spir == 1
order obstruction, after(fev1fvcratio)
label define ratio 0 "No (>=0.7)" 1 "Yes (<0.7)" 2 "Invalid ratio (<0.2 or >1)"
label values obstruction ratio


//Generate binary smoking cessation pharmacotherapy variable
gen byte smokcessbin = q82smokcess
replace smokcessbin = 1 if smokcessbin == 3  //'Offered but declined' counts as 'Yes'
replace smokcessbin = . if smokcessbin == 2  //'Not recorded' not included in denominator
order smokcessbin, after(q82smokcess)


//Generate binary discharge bundle variable
gen byte dischargebin = q93bts
replace dischargebin = 0 if dischargebin == 2  //'Not clear' counts as 'No'
replace dischargebin = . if dischargebin > 2   //All other categories are excluded from denominator
order dischargebin, after(q93bts)


//generate day of week variables
gen arrivaldow = dow(q16a)+1  //plus one is so that days are 1-7, rather than 0-6
gen admissiondow = dow(q17a)+1
gen dischargeday = dow(q91)+1
label values arrivaldow admissiondow dischargeday days


//Generate binary weekend variable
gen byte weekend = 1 if admissiondow == 1 | admissiondow == 7
replace weekend = 0 if admissiondow == 2 | admissiondow == 3 | admissiondow == 4 ///
						| admissiondow == 5 | admissiondow == 6
order weekend, after(admissiondow)


//Correct possible errors in length of stay variable
gen lengthofstay = q91-q17a
sum lengthofstay, detail
replace lengthofstay = . if lengthofstay < 0

//Generate binary length of stay variable (above/below median)
local los_median = 4

gen byte longstay = 0 if lengthofstay <= `los_median'
replace longstay = 1 if lengthofstay > `los_median' & lengthofstay != .
order longstay, after(lengthofstay)
label define longloslab 0 "<=`los_median' days" 1 ">`los_median' days"
label values longstay longloslab


gsort patientid q17b
by patientid: gen patadmno = _n
by patientid: gen patadmcount = _N
order patadmno patadmcount, after(admissionid)
drop admissionid   //doesn't have any use


save builds/SCC_2018_build, replace


log close
