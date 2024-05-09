#----------------------------------------#
# Secondary Care Clinical 2018 Analysis  #
# Author: Alex Adamson                   #
# Date: 14/11/18                         #
#----------------------------------------#

# Set up the libraries

library(dplyr)
library(readstata13)
library(xlsx)
source("H:/My R functions/MySummary.R")
library(janitor)
library(officer)
library(flextable)
library(tidyverse)
library(survival)
library(survminer)
library(ggplot2)
library(survsup)
# library(epitools)
library(psych)



CP <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = FALSE)}

# Read in the SCC data

scc <- read.dta13("D:/Phil/2018 Secondary Care Clinical/builds/SCC_2018_build.dta")
head(scc)

# Read in the IMD data
# Takes absolutely ages for R to do this for some reason I read them in earlier and created RDS files for them to
# be used instead

# IMDeng <- xlsx::read.xlsx(
#           "Z:/Group_work/PS_AA/General UK data/IMD/File_1_ID_2015_Index_of_Multiple_Deprivation.xlsx",
#                           sheetIndex = 2, stringsAsFactors = FALSE)
# saveRDS(IMDeng, "Z:/Group_work/PS_AA/General UK data/IMD/2015_Index_of_Multiple_Deprivation_Eng.RDS")

# IMDwales <- xlsx::read.xlsx(
#       "Z:/Group_work/PS_AA/General UK data/IMD/150812-wimd-2014-overall-domain-ranks-each-lsoa-revised-en.xlsx",
#            sheetIndex = 3, stringsAsFactors = FALSE)
# saveRDS(IMDwales, "Z:/Group_work/PS_AA/General UK data/IMD/2014_Index_of_Multiple_Deprivation_Wales.RDS")

# Read in the RDS files

IMDeng <- readRDS("Z:/Group_work/PS_AA/General UK data/IMD/2015_Index_of_Multiple_Deprivation_Eng.RDS")
IMDwales <- readRDS("Z:/Group_work/PS_AA/General UK data/IMD/2014_Index_of_Multiple_Deprivation_Wales.RDS")

# rename the IMD datasets to facilitate linkage

IMDeng <- dplyr::rename(IMDeng, lsoa11 = LSOA.code..2011.,
                  IMD.decile = Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.)

IMDwales <- dplyr::rename(IMDwales, lsoa11 = LSOA.Code, IMD.quintile = WIMD.2014.Overall.Quintile)

# Create the quitiles for the English IMD data

IMDeng$IMD.quintile <- NA

IMDeng$IMD.quintile[IMDeng$IMD.decile == 1] <- 1
IMDeng$IMD.quintile[IMDeng$IMD.decile == 2] <- 1
IMDeng$IMD.quintile[IMDeng$IMD.decile == 3] <- 2
IMDeng$IMD.quintile[IMDeng$IMD.decile == 4] <- 2
IMDeng$IMD.quintile[IMDeng$IMD.decile == 5] <- 3
IMDeng$IMD.quintile[IMDeng$IMD.decile == 6] <- 3
IMDeng$IMD.quintile[IMDeng$IMD.decile == 7] <- 4
IMDeng$IMD.quintile[IMDeng$IMD.decile == 8] <- 4
IMDeng$IMD.quintile[IMDeng$IMD.decile == 9] <- 5
IMDeng$IMD.quintile[IMDeng$IMD.decile == 10] <- 5

# Get rid of all the useless columns for IMDeng and IMDwales

IMDeng <- dplyr::select(IMDeng, lsoa11, IMD.quintile)
IMDwales <- dplyr::select(IMDwales, lsoa11, IMD.quintile)

IMDeng <- rename(IMDeng, IMD.quintile.eng = IMD.quintile)
IMDwales <- rename(IMDwales, IMD.quintile.wal = IMD.quintile)


head(IMDeng)
head(IMDwales)


# Join them together
# IMD <- rbind(IMDeng, IMDwales)

# Merge them with the SCC data


scc <- left_join(scc, IMDeng, by = "lsoa11")
scc <- left_join(scc, IMDwales, by = "lsoa11")

margin.table(table(scc$IMD.quintile.eng, scc$IMD.quintile.wal, useNA = "ifany"), 2)

head(scc)

CP(scc %>% group_by(hospital, IMDcountry, IMD.quintile) %>%  dplyr::summarise(country.n = n()))


# Some baseline statistics:

totN <- nrow(scc)      
print(totN)



# To get the hospital country, we read in Tim's file linking hospital code to hospital.

country <- read.csv("Z:/Group_work/PS_AA/Secondary Care Clinical 2017-2018/hospital_country.csv")

country <- dplyr::select(country, Code, Country, Name)
country <- dplyr::rename(country, hospital = Code)

# country <- select(country, hospital, country)

str(country)
country$hospital <- as.character(country$hospital)

country <- unique(country)
nrow(country)

summary(scc)

scc <- left_join(scc, country, by = "hospital")

scc <- rename(scc, country = Country)


# Doing more to clean the oxygen data

table(scc$o2bin, scc$q52o2targ, useNA = "ifany")

summary(scc$o2bin)
scc[which(is.na(scc$o2bin) & (scc$q52o2targ == "88-92%" | scc$q52o2targ == "94-98%")), ]$o2bin <- 1
scc[which(is.na(scc$q51o2presc) & (scc$q52o2targ == "88-92%" | scc$q52o2targ == "94-98%")), ]$q51o2presc <- "Yes"

summary(scc$q51o2presc)
# Was oxygen required?
scc$o2req <- NA
scc$o2req[scc$q51o2presc == "Yes" | scc$q51o2presc == "No"] <- "O2 Required"
scc$o2req[scc$q51o2presc == "Not needed"] <- "O2 Not required"
scc$o2req <- as.factor(scc$o2req)






# Let's create a function that will create median and interquartile tables, and one that will create N and % tables.

medsValues <- function(x, varname) {   
  # x is the dataset, varname is the variable name, val is the value of interest (e.g. males) 
  varname <- as.character(varname)
  
  eng <- x %>% filter(country == "England") %>% select(varname)
  print(length(eng[!is.na(eng)]))
  engIQR <- quantile(eng[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  eng <- paste(engIQR[2], " (", engIQR[1], " to ", engIQR[3], ")", sep = "")

  
  wal <- x %>% filter(country == "Wales") %>% select(varname)
  print(length(wal[!is.na(wal)]))
  walIQR <- quantile(wal[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  wal <- paste(walIQR[2], " (", walIQR[1], " to ", walIQR[3], ")", sep = "")
  
  
  all <- x %>% select(varname)
  print(length(all[!is.na(all)]))
  allIQR <- quantile(all[[1]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  all <- paste(allIQR[2], " (", allIQR[1], " to ", allIQR[3], ")", sep = "")
  
  ret <- matrix(c(eng, wal, all), nrow = 1, ncol = 3)
  
  return(ret)
}

# And another one that will work for calculatng frequencies:

myFreqTable <- function(x, varname) {
  
  
  varname <- as.character(varname)
  print(varname)
  gen.E <- x %>% filter(country == "England") %>% dplyr::select(!!varname) %>% drop_na()
  EN <- nrow(gen.E)
  gen.E0 <- as.data.frame(table(gen.E[[1]]))
  gen.E1 <- as.data.frame(round(prop.table(table(gen.E[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.E2 <- inner_join(gen.E0, gen.E1, by = "Var1")
  gen.E2$England <- paste(gen.E2$Freq, " (", gen.E2$perc, ")", sep = "")
  gen.E2 <- select(gen.E2, Var1, England)
  print(gen.E2)
  
  
  gen.W <- x %>% filter(country == "Wales") %>% dplyr::select(!!varname) %>% drop_na()
  WN <- nrow(gen.W)
  gen.W0 <- as.data.frame(table(gen.W[[1]]))
  gen.W1 <- as.data.frame(round(prop.table(table(gen.W[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.W2 <- inner_join(gen.W0, gen.W1, by = "Var1")
  gen.W2$Wales <- paste(gen.W2$Freq, " (", gen.W2$perc, ")", sep = "")
  gen.W2 <- select(gen.W2, Var1, Wales)
  print(gen.W2)
  
  gen.A <- x %>% dplyr::select(!!varname) %>% drop_na()
  AN <- nrow(gen.A)
  gen.A0 <- as.data.frame(table(gen.A[[1]]))
  gen.A1 <- as.data.frame(round(prop.table(table(gen.A[[1]]))*100, 1), nsmall = 1) %>% rename(perc = Freq)
  gen.A2 <- inner_join(gen.A0, gen.A1, by = "Var1")
  gen.A2$All <- paste(gen.A2$Freq, " (", gen.A2$perc, ")", sep = "")
  gen.A2 <- select(gen.A2, Var1, All)
  print(gen.A2)
  
  gen.table <- inner_join(gen.E2, gen.W2, by = "Var1") %>% inner_join(gen.A2, by = "Var1")
  colnames(gen.table) <- c("Var1", paste("England (", EN, ")", sep = ""),
                           paste("Wales (", WN, ")", sep = ""),
                           paste("All (", AN, ")", sep = ""))
  
  return(gen.table)
  }

scc <- scc[ , c(-1, -9, -10)]
# head(scc)

mediSum <- function(x, variable) {
  variable <- as.character(variable)
  varcol <- filter(psychic, vars == variable)
  colnames(varcol) <- paste(variable, colnames(varcol), sep = "_")
  return(varcol[ , -1])
}


FreqSum <- function(x, varname) {
  
  varname <- as.character(varname)
  gen <- x %>% dplyr::select(!!varname) %>% drop_na()
  var_N <- data.frame(nrow(gen))
  colnames(var_N) <- paste0(varname, "_N")
 
  if(nrow(gen) == 0) {return(var_N)}

  else {
            
  gen0 <- as.data.frame(table(gen[[1]]))
  gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
    dplyr::rename(perc = Freq)
  gen2 <- inner_join(gen0, gen1, by = "Var1")
  # gen.E2$England <- paste(gen.E2$Freq, " (", gen.E2$perc, ")", sep = "")
  # gen.E2 <- select(gen.E2, Var1, England)
  for (i in 1:nrow(gen2)) {
    gen3 <- gen2
    gen3$Var1 <- as.character(gen3$Var1)
    gen3 <- gen3[i, ]
    colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                        paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
    var_N <- cbind(var_N, gen3[ ,2:3])
  }
  return(var_N)
  
  }
}

nrow(filter(scc, country == "England"))
nrow(filter(scc, country == "Wales"))
nrow(scc)

sccmastercopy <- scc

# sccIMDeng <- filter(scc, IMDcountry == "England")
# sccIMDwal <- filter(scc, IMDcountry == "Wales")
# 
# CP(FreqSum(sccIMDeng, "IMD.quintile"))
# CP(FreqSum(sccIMDwal, "IMD.quintile"))

summary(scc$q61niv)

FreqSum(scc, "q61niv")

######## s t a r t  h e r e . . . ###########   

# First, do everything for England and Wales, no filtering

scc <- sccmastercopy


# Need to set up the summary table for numeric variables:

psychic <- psych::describe(scc, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
psychic <- as.data.frame(psychic)
psychic$vars <- row.names(psychic)
psychic <- psychic %>% rename(median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75) %>%
  dplyr::select(vars, n, median, lo.quart, hi.quart)

# 1.1 Age

levlev <- mediSum(scc, "age")
levlev$hospital.code <- "All"
levlev$hospital.name <- "All"
levlev$trust.code <- "All"
levlev$cases_n <- levlev$age_n
levlev <- levlev[ ,c(5:8, 1:4)]



# 1.2 Gender


levlev <- cbind(levlev, FreqSum(scc, "gender"))

# 1.3.1 IMD quintiles

levlev <- cbind(levlev, FreqSum(scc, "IMD.quintile.eng"))
levlev <- cbind(levlev, FreqSum(scc, "IMD.quintile.wal"))




#1.4.1 Age at admission by gender


levlev$male_age_n <- scc %>% filter(gender == "Male") %>% nrow()
tes1 <- scc %>% filter(gender == "Male") %>% dplyr::select(age) %>% quantile(
  c(0.25, 0.5, 0.75), 2)
levlev$male_age_median <- tes1[[2]]
levlev$male_age_lo.quart <- tes1[[1]]
levlev$male_age_hi.quart <- tes1[[3]]

levlev$female_age_n <- scc %>% filter(gender == "Female") %>% nrow()
tes1 <- scc %>% filter(gender == "Female") %>% dplyr::select(age) %>% quantile(
  c(0.25, 0.5, 0.75), 2)
levlev$female_age_median <- tes1[[2]]
levlev$female_age_lo.quart <- tes1[[1]]
levlev$female_age_hi.quart <- tes1[[3]]



# 1.4.3 Average time, in hours, between arrival and admission

levlev <- cbind(levlev, mediSum(scc, "admitwaithours"))

# 1.4.4 Day and time of admission to hospital

# q17a is date of ADMISSION.
# q17b is time of ADMISSION. 
# We also need the day of the week.

scc$admiss.day <- weekdays(scc$q17a)
head(scc)

# We want a table of 2-hourly intervals and days
# Phil has actually helpfully already cut the times up in the column q17bcat

scc$admiss.day <- ordered(scc$admiss.day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                            "Friday", "Saturday", "Sunday"))

# Day of the week N
admisstimedow.N <- table(scc$q17bcat, scc$admiss.day)



rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
colnames(admisstimedow.N)

admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
    colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
                           rownames(admisstimedow.N)[1:12], "admiss_n", sep = "_")

colnames(admisstime_flat) <- colsss
admisstime_flat <- as.data.frame(admisstime_flat)
levlev <- cbind(levlev, admisstime_flat)

# I'm going to take the % out for individual hospitals... will just cause too many issues
# with zeroes and also will make the table too big

# Day of the week %
# admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)

# summary(admisstimedow.perc)

# Put them together into a matrix that can then be copy and pasted

# We need to know how many admissions there were for each day as well

levlev$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
levlev$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
levlev$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
levlev$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
levlev$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
levlev$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
levlev$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]


# Right. 


# 2.1.1 Has the patient been reviewed by an acute physician of grade ST3 or above?

# Just going to assume that 'not clear' is the same as 'na'
scc$q21physrev[is.na(scc$q21physrev) == TRUE] <- "Not clear"

FreqSum(scc, "q21physrev")

levlev <- cbind(levlev, FreqSum(scc, "q21physrev"))


# 2.2.1 Has a member of the respiratory team reviewed the patient during the admission? 
FreqSum(scc, "q31resprev")
levlev <- cbind(levlev, FreqSum(scc, "q31resprev"))

# 2.2.2 Was the patient reviewed by a member of the respiratory team within 24 hours?
FreqSum(scc, "reviewwithin24hrs")
levlev <- cbind(levlev, FreqSum(scc, "reviewwithin24hrs"))


# 2.2.3 Average time from admission to review
########### LOOK AT THIS ######################

mediSum(scc, "reviewwaithours")
levlev <- cbind(levlev, mediSum(scc, "reviewwaithours"))


# 3.1.1 Was oxygen prescribed for this patient?

# Adding values to o2bin variable that were missing for oxygen prescribed but have ranges removed:



FreqSum(scc, "o2req")
levlev <- cbind(levlev, FreqSum(scc, "o2req"))

# Out of those who required it, who received it?

scc$o2_pres_if_req_variable <- NA
scc$o2_pres_if_req_variable[scc$o2bin == 1] <- "Prescribed"
scc$o2_pres_if_req_variable[scc$o2bin == 0] <- "Not prescribed"
scc$o2_pres_if_req_variable <- as.factor(scc$o2_pres_if_req_variable)

FreqSum(scc, "o2_pres_if_req_variable")
levlev <- cbind(levlev, FreqSum(scc, "o2_pres_if_req_variable"))




# 3.1.2 If oxygen was prescribed was it to a stipulated target range?


FreqSum(scc, "q52o2targ")
levlev <- cbind(levlev, FreqSum(scc, "q52o2targ"))

# 4.1 Did the patient recive acute treatment with NIV?

FreqSum(scc, "q61niv")
levlev <- cbind(levlev, FreqSum(scc, "q61niv"))


# 4.1.1 If the patient received acute treatment with NIV, was it received within 2 hours of arrival?

FreqSum(scc, "nivwithin2hrs")
levlev <- cbind(levlev, FreqSum(scc, "nivwithin2hrs"))

# 4.1.2 Time from arrival to acute treatment with NIV

FreqSum(scc, "nivtime")
levlev <- cbind(levlev, FreqSum(scc, "nivtime"))

# Need a Kaplan-Meier curve - Time from arrival to acute treatment with NIV

sccKM <- filter(scc, !is.na(nivwaithours))

# Optionally we can replicate this to give all, but it basically just follows the English curve
# sccKMall <- sccKM
# sccKMall$country <- "All"
# Now we bind it together
# sccKM <- rbind(sccKM, sccKMall)

sccKM$seen <- 1

# 48 hours
survfit(Surv(sccKM$nivwaithours, sccKM$seen) ~ sccKM$country, data = sccKM) %>% 
plot_survfit(ci = TRUE, legend.title = "Country", xmax = 48, xbreaks = seq(0, 48, 6)) + 
labs(x = "Time (hours)", y = "Culmulative % of patients that have received NIV")

survfit(Surv(sccKM$nivwaithours, sccKM$seen) ~ sccKM$country, data = sccKM) %>% 
  

# Might need to write something here so that each plot is saved, if each hospital wants one

# # 10 days
# sccKM$nivwaitdays <- sccKM$nivwaithours/24
# survfit(Surv(sccKM$nivwaitdays, sccKM$seen) ~ sccKM$country, data = sccKM) %>% 
#   plot_survfit(ci = TRUE, legend.title = "Country", xmax = 10, xbreaks = c(1:10)) + 
#   labs(x = "Time (days)", y = "Culmulative % of patients that have received NIV")
# 
# # All time (1066 hours)
# # 10 days
# sccKM$nivwaitdays <- sccKM$nivwaithours/24
# survfit(Surv(sccKM$nivwaitdays, sccKM$seen) ~ sccKM$country, data = sccKM) %>% 
#   plot_survfit(ci = TRUE, legend.title = "Country", xmax = 45, xbreaks = seq(0, 45, 5)) + 
#   labs(x = "Time (days)", y = "Culmulative % of patients that have received NIV")


# plot <- plot + theme(panel.grid.minor = element_line(colour = "black", size = 0.5))


#4.1.3 Average time from arrival at hospital to acute treatment with NIV


mediSum(scc, "nivwaithours")
levlev <- cbind(levlev, mediSum(scc, "nivwaithours"))


# 5.1.1 Is a spirometry result available?

FreqSum(scc, "q71spir")
levlev <- cbind(levlev, FreqSum(scc, "q71spir"))

# 5.1.2 degree of airflow obstruction

scc$obstruct <- NA
scc$obstruct[scc$obstruction == 0] <- "No(>=0.7)"
scc$obstruct[scc$obstruction == 1] <- "Yes(<0.7)"
scc$obstruct[scc$obstruction == 2] <- "Invalid_ratio(<0.2or>1)"
scc$obstruct <- as.factor(scc$obstruct)

FreqSum(scc, "obstruct")
levlev <- cbind(levlev, FreqSum(scc, "obstruct"))


# 5.1.3 If a spirometry result is available, what is the patients most recent FEV1?

mediSum(scc, "q72fev1")
levlev <- cbind(levlev, mediSum(scc, "q72fev1"))

# CP(medTable(scc, "q72fev1"))
# scc %>% filter(country == "England", !is.na(q72fev1)) %>% nrow()
# scc %>% filter(country == "Wales", !is.na(q72fev1)) %>% nrow()
# scc %>% filter(!is.na(q72fev1)) %>% nrow()


# 6.1.1 What was the smoking status for this patient, as documented for the current admission?


FreqSum(scc, "q81smokstatus")
levlev <- cbind(levlev, FreqSum(scc, "q81smokstatus"))

# 6.1.2 If a current smoker, was the patient prescribed smoking-cessation pharmacotherapy during the
# current admission?

FreqSum(scc, "q82smokcess")
levlev <- cbind(levlev, FreqSum(scc, "q82smokcess"))


# 7.1.1 Was a DECAF score recorded for this patient?
FreqSum(scc, "q41decafrec")
levlev <- cbind(levlev, FreqSum(scc, "q41decafrec"))


# 7.1.2 If yes, what was the recorded DECAF score?
# Make sure it's classed as a factor to allow binding

scc$q42decaf <- as.factor(scc$q42decaf)

FreqSum(scc, "q42decaf")
levlev <- cbind(levlev, FreqSum(scc, "q42decaf"))

# 8.1 Average length of stay
mediSum(scc, "lengthofstay")
levlev <- cbind(levlev, mediSum(scc, "lengthofstay")) 

table(scc$dischargeday, weekdays(scc$q91))
scc$discharge_day <- weekdays(scc$q91)
scc$discharge_day <- ordered(scc$discharge_day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                   "Friday", "Saturday", "Sunday"))

FreqSum(scc, "discharge_day")
levlev <- cbind(levlev, FreqSum(scc, "discharge_day"))

# 8.3 Did the patient die as an inpatient in your hospital?

FreqSum(scc, "died")
levlev <- cbind(levlev, FreqSum(scc, "died"))


# 8.4 Has a British Thoracic Society (BTS), or equivalent, discharge bundle been completed for this admission?

FreqSum(scc, "q93bts")
levlev <- cbind(levlev, FreqSum(scc, "q93bts"))




#  8.5 What follow-up arrangements have been made for this patient?  

# This is very confusing, but basically because the question starts off with a 'no' we have to rearrange the 
# order so that it makes sense when you read it
scc$anyarrangementsapparent <- NA
scc$anyarrangementsapparent[scc$q94no == "Yes"] <- "No"
scc$anyarrangementsapparent[scc$q94no == "No"] <- "Yes"
scc$anyarrangementsapparent <- as.factor(scc$anyarrangementsapparent)

summary(scc$q94no)
FreqSum(scc, "anyarrangementsapparent") 
FreqSum(scc, "q94assteam")
FreqSum(scc, "q94gpfuadvised") 
FreqSum(scc, "q94gpfuarranged") 
FreqSum(scc, "q94phone")
FreqSum(scc, "q94commadvised") 
FreqSum(scc, "q94commarranged") 
FreqSum(scc, "q94hospadvised") 
FreqSum(scc, "q94hosparranged") 
FreqSum(scc, "q94otherhealthcare") 
FreqSum(scc, "q94eol")
FreqSum(scc, "q94other")

levlev <- cbind(levlev, 
FreqSum(scc, "anyarrangementsapparent"), 
FreqSum(scc, "q94assteam"),
FreqSum(scc, "q94gpfuadvised"), 
FreqSum(scc, "q94gpfuarranged"), 
FreqSum(scc, "q94phone"),
FreqSum(scc, "q94commadvised"), 
FreqSum(scc, "q94commarranged"), 
FreqSum(scc, "q94hospadvised"), 
FreqSum(scc, "q94hosparranged"), 
FreqSum(scc, "q94otherhealthcare"), 
FreqSum(scc, "q94eol"),
FreqSum(scc, "q94other")  )    


# 9.1 Associations with time from arrival to acute treatment with NIV 
# Median is 4

# Put <= 4 days, >4 days, and died into the same grouping

scc$toolongstay <- scc$longstay
scc$toolongstay <- as.character(scc$toolongstay)
scc$toolongstay[scc$died == "Yes"] <- "Died"
scc$toolongstay <- as.factor(scc$toolongstay)


table(scc$smokcessbin, scc$q82smokcess)
scc$smokcessoffered <- NA
scc$smokcessoffered[scc$smokcessbin == 0] <- "No"
scc$smokcessoffered[scc$smokcessbin == 1] <- "Yes"
scc$smokcessoffered <- as.factor(scc$smokcessoffered)

table(scc$smokcessoffered, scc$q81smokstatus, useNA = "ifany")
table(scc$smokcessoffered, scc$q82smokcess, useNA = "ifany")


# NIV outcomes

niv.scc1 <- scc[scc$nivtime == "<2 Hours", ] 

niv.scc1$niv2hr_staytime <- niv.scc1$toolongstay
FreqSum(niv.scc1, "niv2hr_staytime")
levlev <- cbind(levlev, FreqSum(niv.scc1, "niv2hr_staytime"))

niv.scc2 <- scc[scc$nivtime == "2-24 Hours", ] 

niv.scc2$niv2to24hr_staytime <- niv.scc2$toolongstay
FreqSum(niv.scc2, "niv2to24hr_staytime")
levlev <- cbind(levlev, FreqSum(niv.scc2, "niv2to24hr_staytime"))




# 9.2 Associations with review by a member of the respiratory team within 24 hours



# Back to resp review within 24 hrs vs no review



resp.scc1 <- scc[scc$reviewwithin24hrs == "Yes", ] 

# Received specialist review - elsewhere in the table
# table(scc$q31resprev, scc$longstay)

# detach("package:epitools", unload=TRUE)

str(scc)



resp.scc1$resprev24hr_staytime <- resp.scc1$toolongstay
FreqSum(resp.scc1, "resprev24hr_staytime")
levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_staytime"))

resp.scc1$resprev24hr_o2_pres_if_req_variable <- resp.scc1$o2_pres_if_req_variable
FreqSum(resp.scc1, "resprev24hr_o2_pres_if_req_variable")
levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_o2_pres_if_req_variable"))

resp.scc1$resprev24hr_niv2hrbin <- resp.scc1$niv2hrbin
FreqSum(resp.scc1, "resprev24hr_niv2hrbin")
levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_niv2hrbin"))

resp.scc1$resprev24hr_smokcessbin <- resp.scc1$smokcessbin
FreqSum(resp.scc1, "resprev24hr_smokcessbin")
levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_smokcessbin"))

resp.scc1$resprev24hr_dischargebin <- resp.scc1$dischargebin
FreqSum(resp.scc1, "resprev24hr_dischargebin")
levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_dischargebin"))



# table(scc$dischargebin, scc$died, useNA = "ifany")
# table(scc$dischargebin, scc$q93bts, useNA = "ifany")
# 
# summary(scc$dischargebin)
# summary(scc$died)
# 
# table(scc$o2bin, scc$q31resprev, useNA = "ifany")
# table(scc$q82smokcess, scc$smokcessbin, useNA = "ifany")

# Did not receive a specialist review within 24 hours
resp.scc2 <- scc[scc$reviewwithin24hrs == "No", ] 

# Received specialist review
# elsewhere in the table(scc$q31resprev, scc$longstay)

# detach("package:epitools", unload=TRUE)

resp.scc2$noresprevwithin24hrs_staytime <- resp.scc2$toolongstay
FreqSum(resp.scc2, "noresprevwithin24hrs_staytime")
levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_staytime"))

resp.scc2$noresprevwithin24hrs_o2_pres_if_req_variable <- resp.scc2$o2_pres_if_req_variable
FreqSum(resp.scc2, "noresprevwithin24hrs_o2_pres_if_req_variable")
levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_o2_pres_if_req_variable"))

resp.scc2$noresprevwithin24hrs_niv2hrbin <- resp.scc2$niv2hrbin
FreqSum(resp.scc2, "noresprevwithin24hrs_niv2hrbin")
levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_niv2hrbin"))

resp.scc2$noresprevwithin24hrs_smokcessbin <- resp.scc2$smokcessbin
FreqSum(resp.scc2, "noresprevwithin24hrs_smokcessbin")
levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_smokcessbin"))

resp.scc2$noresprevwithin24hrs_dischargebin <- resp.scc2$dischargebin
FreqSum(resp.scc2, "noresprevwithin24hrs_dischargebin")
levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_dischargebin"))
# 9.2 odds ratios




scc$adj_dischargebunrec <- NA
scc$adj_dischargebunrec[scc$dischargebin == 0] <- "No"
scc$adj_dischargebunrec[scc$dischargebin == 1] <- "Yes"
scc$adj_dischargebunrec <- as.factor(scc$adj_dischargebunrec)

summary(scc$dischargebin)
summary(scc$adj_dischargebunrec)

FreqSum(scc, "adj_dischargebunrec")
levlev <- cbind(levlev, FreqSum(scc, "adj_dischargebunrec"))

# Oxygen prescribed to a target saturation:
table(scc$o2bin, scc$q52o2targ, useNA = "ifany")

scc$o2targetsatgiven <- NA
scc$o2targetsatgiven[scc$q52o2targ == "88-92%"] <- "Yes"
scc$o2targetsatgiven[scc$q52o2targ == "94-98%"] <- "Yes"
scc$o2targetsatgiven[scc$q52o2targ == "Other"] <- "Yes"
scc$o2targetsatgiven[scc$q52o2targ == "Target range not stipulated"] <- "No"
scc$o2targetsatgiven <- as.factor(scc$o2targetsatgiven)
summary(scc$o2targetsatgiven)

FreqSum(scc, "o2targetsatgiven")
levlev <- cbind(levlev, FreqSum(scc, "o2targetsatgiven"))

# table(scc$dischargebin, scc$q93bts, useNA = "ifany")
# 
# 
# 
# summary(scc$reviewwithin24hrs)
# summary(scc$dischargebin) # Out of those who didn't die or self-discharge or put down 'other'

scc$bpt <- "No"
scc$bpt[scc$reviewwithin24hrs == "Yes" & scc$dischargebin == 1] <- "Yes"
scc$bpt <- as.factor(scc$bpt)

FreqSum(scc, "bpt")
levlev <- cbind(levlev, FreqSum(scc, "bpt"))


############################################ End of the loop  ###################################

completedlevlev <- levlev


################################  Start of England and Wales loop #######################################


for (i in c("England", "Wales")) {
  
  scc <- sccmastercopy

  scc <- filter(scc, country == i)
  
  
  # Need to set up the summary table for numeric variables:
  
  psychic <- psych::describe(scc, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75) %>%
    dplyr::select(vars, n, median, lo.quart, hi.quart)
  
# 1.1 Age

levlev <- mediSum(scc, "age")
levlev$hospital.code <- i
levlev$hospital.name <- i
levlev$trust.code <- i
levlev$cases_n <- levlev$age_n
levlev <- levlev[ ,c(5:8, 1:4)]



# 1.2 Gender


levlev <- cbind(levlev, FreqSum(scc, "gender"))

# 1.3.1 IMD quintiles

levlev <- cbind(levlev, FreqSum(scc, "IMD.quintile.eng"))
levlev <- cbind(levlev, FreqSum(scc, "IMD.quintile.wal"))


#1.4.1 Age at admission by gender


levlev$male_age_n <- scc %>% filter(gender == "Male") %>% nrow()
tes1 <- scc %>% filter(gender == "Male") %>% dplyr::select(age) %>% quantile(
  c(0.25, 0.5, 0.75), 2)
levlev$male_age_median <- tes1[[2]]
levlev$male_age_lo.quart <- tes1[[1]]
levlev$male_age_hi.quart <- tes1[[3]]

levlev$female_age_n <- scc %>% filter(gender == "Female") %>% nrow()
tes1 <- scc %>% filter(gender == "Female") %>% dplyr::select(age) %>% quantile(
  c(0.25, 0.5, 0.75), 2)
levlev$female_age_median <- tes1[[2]]
levlev$female_age_lo.quart <- tes1[[1]]
levlev$female_age_hi.quart <- tes1[[3]]



# 1.4.3 Average time, in hours, between arrival and admission

levlev <- cbind(levlev, mediSum(scc, "admitwaithours"))

# 1.4.4 Day and time of admission to hospital

# q17a is date of ADMISSION.
# q17b is time of ADMISSION. 
# We also need the day of the week.

scc$admiss.day <- weekdays(scc$q17a)
head(scc)

# We want a table of 2-hourly intervals and days
# Phil has actually helpfully already cut the times up in the column q17bcat

scc$admiss.day <- ordered(scc$admiss.day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                   "Friday", "Saturday", "Sunday"))

# Day of the week N
admisstimedow.N <- table(scc$q17bcat, scc$admiss.day)



rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
colnames(admisstimedow.N)

admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
                rownames(admisstimedow.N)[1:12], "admiss_n", sep = "_")

colnames(admisstime_flat) <- colsss
admisstime_flat <- as.data.frame(admisstime_flat)
levlev <- cbind(levlev, admisstime_flat)



# We need to know how many admissions there were for each day as well

levlev$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
levlev$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
levlev$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
levlev$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
levlev$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
levlev$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
levlev$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]


# Right. 


# 2.1.1 Has the patient been reviewed by an acute physician of grade ST3 or above?

# Just going to assume that 'not clear' is the same as 'na'
scc$q21physrev[is.na(scc$q21physrev) == TRUE] <- "Not clear"

FreqSum(scc, "q21physrev")

levlev <- cbind(levlev, FreqSum(scc, "q21physrev"))


# 2.2.1 Has a member of the respiratory team reviewed the patient during the admission? 
FreqSum(scc, "q31resprev")
levlev <- cbind(levlev, FreqSum(scc, "q31resprev"))

# 2.2.2 Was the patient reviewed by a member of the respiratory team within 24 hours?
FreqSum(scc, "reviewwithin24hrs")
levlev <- cbind(levlev, FreqSum(scc, "reviewwithin24hrs"))


# 2.2.3 Average time from admission to review


mediSum(scc, "reviewwaithours")
levlev <- cbind(levlev, mediSum(scc, "reviewwaithours"))


# 3.1.1 Was oxygen prescribed for this patient?


FreqSum(scc, "o2req")
levlev <- cbind(levlev, FreqSum(scc, "o2req"))

# Out of those who required it, who received it?

scc$o2_pres_if_req_variable <- NA
scc$o2_pres_if_req_variable[scc$o2bin == 1] <- "Prescribed"
scc$o2_pres_if_req_variable[scc$o2bin == 0] <- "Not prescribed"
scc$o2_pres_if_req_variable <- as.factor(scc$o2_pres_if_req_variable)

FreqSum(scc, "o2_pres_if_req_variable")
levlev <- cbind(levlev, FreqSum(scc, "o2_pres_if_req_variable"))




# 3.1.2 If oxygen was prescribed was it to a stipulated target range?


FreqSum(scc, "q52o2targ")
levlev <- cbind(levlev, FreqSum(scc, "q52o2targ"))

# 4.1 Did the patient receive acute treatment with NIV?

FreqSum(scc, "q61niv")
levlev <- cbind(levlev, FreqSum(scc, "q61niv"))


# 4.1.1 If the patient received acute treatment with NIV, was it received within 2 hours of arrival?

FreqSum(scc, "nivwithin2hrs")
levlev <- cbind(levlev, FreqSum(scc, "nivwithin2hrs"))

# 4.1.2 Time from arrival to acute treatment with NIV

FreqSum(scc, "nivtime")
levlev <- cbind(levlev, FreqSum(scc, "nivtime"))

# Need a Kaplan-Meier curve - Time from arrival to acute treatment with NIV

sccKM <- filter(scc, !is.na(nivwaithours))



sccKM$seen <- 1




#4.1.3 Average time from arrival at hospital to acute treatment with NIV


mediSum(scc, "nivwaithours")
levlev <- cbind(levlev, mediSum(scc, "nivwaithours"))


# 5.1.1 Is a spirometry result available?

FreqSum(scc, "q71spir")
levlev <- cbind(levlev, FreqSum(scc, "q71spir"))

# 5.1.2 degree of airflow obstruction

scc$obstruct <- NA
scc$obstruct[scc$obstruction == 0] <- "No(>=0.7)"
scc$obstruct[scc$obstruction == 1] <- "Yes(<0.7)"
scc$obstruct[scc$obstruction == 2] <- "Invalid_ratio(<0.2or>1)"
scc$obstruct <- as.factor(scc$obstruct)

FreqSum(scc, "obstruct")
levlev <- cbind(levlev, FreqSum(scc, "obstruct"))


# 5.1.3 If a spirometry result is available, what is the patients most recent FEV1?

mediSum(scc, "q72fev1")
levlev <- cbind(levlev, mediSum(scc, "q72fev1"))

# CP(medTable(scc, "q72fev1"))
# scc %>% filter(country == "England", !is.na(q72fev1)) %>% nrow()
# scc %>% filter(country == "Wales", !is.na(q72fev1)) %>% nrow()
# scc %>% filter(!is.na(q72fev1)) %>% nrow()


# 6.1.1 What was the smoking status for this patient, as documented for the current admission?


FreqSum(scc, "q81smokstatus")
levlev <- cbind(levlev, FreqSum(scc, "q81smokstatus"))

# 6.1.2 If a current smoker, was the patient prescribed smoking-cessation pharmacotherapy during the
# current admission?

FreqSum(scc, "q82smokcess")
levlev <- cbind(levlev, FreqSum(scc, "q82smokcess"))


# 7.1.1 Was a DECAF score recorded for this patient?
FreqSum(scc, "q41decafrec")
levlev <- cbind(levlev, FreqSum(scc, "q41decafrec"))


# 7.1.2 If yes, what was the recorded DECAF score?
# Make sure it's classed as a factor to allow binding

scc$q42decaf <- as.factor(scc$q42decaf)

FreqSum(scc, "q42decaf")
levlev <- cbind(levlev, FreqSum(scc, "q42decaf"))

# 8.1 Average length of stay
mediSum(scc, "lengthofstay")
levlev <- cbind(levlev, mediSum(scc, "lengthofstay")) 

table(scc$dischargeday, weekdays(scc$q91))
scc$discharge_day <- weekdays(scc$q91)
scc$discharge_day <- ordered(scc$discharge_day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                         "Friday", "Saturday", "Sunday"))

FreqSum(scc, "discharge_day")
levlev <- cbind(levlev, FreqSum(scc, "discharge_day"))

# 8.3 Did the patient die as an inpatient in your hospital?

FreqSum(scc, "died")
levlev <- cbind(levlev, FreqSum(scc, "died"))


# 8.4 Has a British Thoracic Society (BTS), or equivalent, discharge bundle been completed for this admission?

FreqSum(scc, "q93bts")
levlev <- cbind(levlev, FreqSum(scc, "q93bts"))



#  8.5 What follow-up arrangements have been made for this patient?  

# This is very confusing, but basically because the question starts off with a 'no' we have to rearrange the 
# order so that it makes sense when you read it
scc$anyarrangementsapparent <- NA
scc$anyarrangementsapparent[scc$q94no == "Yes"] <- "No"
scc$anyarrangementsapparent[scc$q94no == "No"] <- "Yes"
scc$anyarrangementsapparent <- as.factor(scc$anyarrangementsapparent)

summary(scc$q94no)
FreqSum(scc, "anyarrangementsapparent") 
FreqSum(scc, "q94assteam")
FreqSum(scc, "q94gpfuadvised") 
FreqSum(scc, "q94gpfuarranged") 
FreqSum(scc, "q94phone")
FreqSum(scc, "q94commadvised") 
FreqSum(scc, "q94commarranged") 
FreqSum(scc, "q94hospadvised") 
FreqSum(scc, "q94hosparranged") 
FreqSum(scc, "q94otherhealthcare") 
FreqSum(scc, "q94eol")
FreqSum(scc, "q94other")

levlev <- cbind(levlev, 
                FreqSum(scc, "anyarrangementsapparent"), 
                FreqSum(scc, "q94assteam"),
                FreqSum(scc, "q94gpfuadvised"), 
                FreqSum(scc, "q94gpfuarranged"), 
                FreqSum(scc, "q94phone"),
                FreqSum(scc, "q94commadvised"), 
                FreqSum(scc, "q94commarranged"), 
                FreqSum(scc, "q94hospadvised"), 
                FreqSum(scc, "q94hosparranged"), 
                FreqSum(scc, "q94otherhealthcare"), 
                FreqSum(scc, "q94eol"),
                FreqSum(scc, "q94other")  )    


# 9.1 Associations with time from arrival to acute treatment with NIV 
# Median is 4

# Put <= 4 days, >4 days, and died into the same grouping

scc$toolongstay <- scc$longstay
scc$toolongstay <- as.character(scc$toolongstay)
scc$toolongstay[scc$died == "Yes"] <- "Died"
scc$toolongstay <- as.factor(scc$toolongstay)


table(scc$smokcessbin, scc$q82smokcess)
scc$smokcessoffered <- NA
scc$smokcessoffered[scc$smokcessbin == 0] <- "No"
scc$smokcessoffered[scc$smokcessbin == 1] <- "Yes"
scc$smokcessoffered <- as.factor(scc$smokcessoffered)

table(scc$smokcessoffered, scc$q81smokstatus, useNA = "ifany")
table(scc$smokcessoffered, scc$q82smokcess, useNA = "ifany")



# NIV outcomes

niv.scc1 <- scc[scc$nivtime == "<2 Hours", ] 

niv.scc1$niv2hr_staytime <- niv.scc1$toolongstay
FreqSum(niv.scc1, "niv2hr_staytime")
levlev <- cbind(levlev, FreqSum(niv.scc1, "niv2hr_staytime"))

niv.scc2 <- scc[scc$nivtime == "2-24 Hours", ] 

niv.scc2$niv2to24hr_staytime <- niv.scc2$toolongstay
FreqSum(niv.scc2, "niv2to24hr_staytime")
levlev <- cbind(levlev, FreqSum(niv.scc2, "niv2to24hr_staytime"))



 



# 9.2 Associations with review by a member of the respiratory team within 24 hours


resp.scc1 <- scc[scc$reviewwithin24hrs == "Yes", ] 

# Received specialist review - elsewhere in the table
# table(scc$q31resprev, scc$longstay)

# detach("package:epitools", unload=TRUE)

resp.scc1$resprev24hr_staytime <- resp.scc1$toolongstay
FreqSum(resp.scc1, "resprev24hr_staytime")
levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_staytime"))

resp.scc1$resprev24hr_o2_pres_if_req_variable <- resp.scc1$o2_pres_if_req_variable
FreqSum(resp.scc1, "resprev24hr_o2_pres_if_req_variable")
levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_o2_pres_if_req_variable"))

resp.scc1$resprev24hr_niv2hrbin <- resp.scc1$niv2hrbin
FreqSum(resp.scc1, "resprev24hr_niv2hrbin")
levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_niv2hrbin"))

resp.scc1$resprev24hr_smokcessbin <- resp.scc1$smokcessbin
FreqSum(resp.scc1, "resprev24hr_smokcessbin")
levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_smokcessbin"))

resp.scc1$resprev24hr_dischargebin <- resp.scc1$dischargebin
FreqSum(resp.scc1, "resprev24hr_dischargebin")
levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_dischargebin"))



# table(scc$dischargebin, scc$died, useNA = "ifany")
# table(scc$dischargebin, scc$q93bts, useNA = "ifany")
# 
# summary(scc$dischargebin)
# summary(scc$died)
# 
# table(scc$o2bin, scc$q31resprev, useNA = "ifany")
# table(scc$q82smokcess, scc$smokcessbin, useNA = "ifany")

# Did not receive a specialist review within 24 hours
resp.scc2 <- scc[scc$reviewwithin24hrs == "No", ] 

# Received specialist review
# elsewhere in the table(scc$q31resprev, scc$longstay)

# detach("package:epitools", unload=TRUE)

resp.scc2$noresprevwithin24hrs_staytime <- resp.scc2$toolongstay
FreqSum(resp.scc2, "noresprevwithin24hrs_staytime")
levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_staytime"))

resp.scc2$noresprevwithin24hrs_o2_pres_if_req_variable <- resp.scc2$o2_pres_if_req_variable
FreqSum(resp.scc2, "noresprevwithin24hrs_o2_pres_if_req_variable")
levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_o2_pres_if_req_variable"))

resp.scc2$noresprevwithin24hrs_niv2hrbin <- resp.scc2$niv2hrbin
FreqSum(resp.scc2, "noresprevwithin24hrs_niv2hrbin")
levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_niv2hrbin"))

resp.scc2$noresprevwithin24hrs_smokcessbin <- resp.scc2$smokcessbin
FreqSum(resp.scc2, "noresprevwithin24hrs_smokcessbin")
levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_smokcessbin"))

resp.scc2$noresprevwithin24hrs_dischargebin <- resp.scc2$dischargebin
FreqSum(resp.scc2, "noresprevwithin24hrs_dischargebin")
levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_dischargebin"))
# 9.2 odds ratios




# Section 10: Web-tool run charts 

scc$adj_dischargebunrec <- NA
scc$adj_dischargebunrec[scc$dischargebin == 0] <- "No"
scc$adj_dischargebunrec[scc$dischargebin == 1] <- "Yes"
scc$adj_dischargebunrec <- as.factor(scc$adj_dischargebunrec)

summary(scc$adj_dischargebunrec)

FreqSum(scc, "adj_dischargebunrec")
levlev <- cbind(levlev, FreqSum(scc, "adj_dischargebunrec"))

# Oxygen prescribed to a target saturation:
table(scc$o2bin, scc$q52o2targ, useNA = "ifany")

scc$o2targetsatgiven <- NA
scc$o2targetsatgiven[scc$q52o2targ == "88-92%"] <- "Yes"
scc$o2targetsatgiven[scc$q52o2targ == "94-98%"] <- "Yes"
scc$o2targetsatgiven[scc$q52o2targ == "Other"] <- "Yes"
scc$o2targetsatgiven[scc$q52o2targ == "Target range not stipulated"] <- "No"
scc$o2targetsatgiven <- as.factor(scc$o2targetsatgiven)
summary(scc$o2targetsatgiven)

FreqSum(scc, "o2targetsatgiven")
levlev <- cbind(levlev, FreqSum(scc, "o2targetsatgiven"))

# table(scc$dischargebin, scc$q93bts, useNA = "ifany")
# 
# 
# 
# summary(scc$reviewwithin24hrs)
# summary(scc$dischargebin) # Out of those who didn't die or self-discharge or put down 'other'

scc$bpt <- "No"
scc$bpt[scc$reviewwithin24hrs == "Yes" & scc$dischargebin == 1] <- "Yes"
scc$bpt <- as.factor(scc$bpt)

FreqSum(scc, "bpt")
levlev <- cbind(levlev, FreqSum(scc, "bpt"))




completedlevlev <- full_join(completedlevlev, levlev)

}



################################################### End of England and Wales loop ######################


# Need to remove hospitals that don't have enough people to analyse

sccmastercopy2 <- sccmastercopy

sccmastercopy <- sccmastercopy2[sccmastercopy2$hospadmissions > 5, ]


################################  Start of hospital loop #######################################


for (i in unique(sccmastercopy$hospital)) {
  
  scc <- sccmastercopy
  
  scc <- filter(scc, hospital == i)
  
  
  # Need to set up the summary table for numeric variables:
  
  psychic <- psych::describe(scc, skew = FALSE, ranges = FALSE, quant = c(0.25, 0.5, 0.75))
  psychic <- as.data.frame(psychic)
  psychic$vars <- row.names(psychic)
  psychic <- psychic %>% rename(median = Q0.5, lo.quart = Q0.25, hi.quart = Q0.75) %>%
    dplyr::select(vars, n, median, lo.quart, hi.quart)
  
  # 1.1 Age
  
  levlev <- mediSum(scc, "age")
  levlev$hospital.code <- slice(scc, 1)$hospital
  levlev$hospital.name <- slice(scc, 1)$Name
  levlev$trust.code <- slice(scc, 1)$trustcode
  levlev$cases_n <- levlev$age_n
  levlev <- levlev[ ,c(5:8, 1:4)]
  
  
  
  # 1.2 Gender
  
  
  levlev <- cbind(levlev, FreqSum(scc, "gender"))
  
  # 1.3.1 IMD quintiles
  
  levlev <- cbind(levlev, FreqSum(scc, "IMD.quintile.eng"))
  levlev <- cbind(levlev, FreqSum(scc, "IMD.quintile.wal"))
  
  
  
  
  #1.4.1 Age at admission by gender
  
  
  levlev$male_age_n <- scc %>% filter(gender == "Male") %>% nrow()
  tes1 <- scc %>% filter(gender == "Male") %>% dplyr::select(age) %>% quantile(
    c(0.25, 0.5, 0.75), 2)
  levlev$male_age_median <- tes1[[2]]
  levlev$male_age_lo.quart <- tes1[[1]]
  levlev$male_age_hi.quart <- tes1[[3]]
  
  levlev$female_age_n <- scc %>% filter(gender == "Female") %>% nrow()
  tes1 <- scc %>% filter(gender == "Female") %>% dplyr::select(age) %>% quantile(
    c(0.25, 0.5, 0.75), 2)
  levlev$female_age_median <- tes1[[2]]
  levlev$female_age_lo.quart <- tes1[[1]]
  levlev$female_age_hi.quart <- tes1[[3]]
  
 
  
  # 1.4.3 Average time, in hours, between arrival and admission
  
  levlev <- cbind(levlev, mediSum(scc, "admitwaithours"))
  
  # 1.4.4 Day and time of admission to hospital
  
  # q17a is date of ADMISSION.
  # q17b is time of ADMISSION. 
  # We also need the day of the week.
  
  scc$admiss.day <- weekdays(scc$q17a)
  head(scc)
  
  # We want a table of 2-hourly intervals and days
  # Phil has actually helpfully already cut the times up in the column q17bcat
  
  scc$admiss.day <- ordered(scc$admiss.day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                     "Friday", "Saturday", "Sunday"))
  
  
  str(scc$admiss.day)
  
  scc$q17bcat <- factor(scc$q17bcat, levels = 1:12)
  
  # as.factor doesn't accept the argument 'levels'!!!!!!
  
  # Day of the week N
  admisstimedow.N <- table(scc$q17bcat, scc$admiss.day)
  
  
  
  rownames(admisstimedow.N) <- paste0(seq(0, 22, 2), ".00to", seq(1, 23, 2), ".59")
  colnames(admisstimedow.N)
  
  admisstime_flat <- matrix(admisstimedow.N, nrow = 1, ncol = 84, byrow = FALSE)
  colsss <- paste(rep(colnames(admisstimedow.N)[1:7], each = 12),
                  rownames(admisstimedow.N)[1:12], "admiss_n", sep = "_")
  
  colnames(admisstime_flat) <- colsss
  admisstime_flat <- as.data.frame(admisstime_flat)
  levlev <- cbind(levlev, admisstime_flat)
  
 
  # Put them together into a matrix that can then be copy and pasted
  
  # We need to know how many admissions there were for each day as well
  
  levlev$Monday_admit_N <- margin.table(admisstimedow.N, 2)[1]
  levlev$Tuesday_admit_N <- margin.table(admisstimedow.N, 2)[2]
  levlev$Wednesday_admit_N <- margin.table(admisstimedow.N, 2)[3]
  levlev$Thursday_admit_N <- margin.table(admisstimedow.N, 2)[4]
  levlev$Friday_admit_N <- margin.table(admisstimedow.N, 2)[5]
  levlev$Saturday_admit_N <- margin.table(admisstimedow.N, 2)[6]
  levlev$Sunday_admit_N <- margin.table(admisstimedow.N, 2)[7]
  
  
  # Right. 
  
  
  # 2.1.1 Has the patient been reviewed by an acute physician of grade ST3 or above?
  
  # Just going to assume that 'not clear' is the same as 'na'
  scc$q21physrev[is.na(scc$q21physrev) == TRUE] <- "Not clear"
  
  FreqSum(scc, "q21physrev")
  
  levlev <- cbind(levlev, FreqSum(scc, "q21physrev"))
  
  
  # 2.2.1 Has a member of the respiratory team reviewed the patient during the admission? 
  FreqSum(scc, "q31resprev")
  levlev <- cbind(levlev, FreqSum(scc, "q31resprev"))
  
  # 2.2.2 Was the patient reviewed by a member of the respiratory team within 24 hours?
  FreqSum(scc, "reviewwithin24hrs")
  levlev <- cbind(levlev, FreqSum(scc, "reviewwithin24hrs"))
  
  
  # 2.2.3 Average time from admission to review
  
  
  mediSum(scc, "reviewwaithours")
  levlev <- cbind(levlev, mediSum(scc, "reviewwaithours"))
  
  
  # 3.1.1 Was oxygen prescribed for this patient?
  
  
  
  FreqSum(scc, "o2req")
  levlev <- cbind(levlev, FreqSum(scc, "o2req"))
  
  # Out of those who required it, who received it?
  
  scc$o2_pres_if_req_variable <- NA
  scc$o2_pres_if_req_variable[scc$o2bin == 1] <- "Prescribed"
  scc$o2_pres_if_req_variable[scc$o2bin == 0] <- "Not prescribed"
  scc$o2_pres_if_req_variable <- as.factor(scc$o2_pres_if_req_variable)
  
  FreqSum(scc, "o2_pres_if_req_variable")
  levlev <- cbind(levlev, FreqSum(scc, "o2_pres_if_req_variable"))
  
  
  # 3.1.2 If oxygen was prescribed was it to a stipulated target range?
  
  
  FreqSum(scc, "q52o2targ")
  levlev <- cbind(levlev, FreqSum(scc, "q52o2targ"))
  
  # 4.1 Did the patient receive acute treatment with NIV?
  
  FreqSum(scc, "q61niv")
  levlev <- cbind(levlev, FreqSum(scc, "q61niv"))
  
  
  # 4.1.1 If the patient received acute treatment with NIV, was it received within 2 hours of arrival?
  
  FreqSum(scc, "nivwithin2hrs")
  levlev <- cbind(levlev, FreqSum(scc, "nivwithin2hrs"))
  
  # 4.1.2 Time from arrival to acute treatment with NIV
  
  FreqSum(scc, "nivtime")
  levlev <- cbind(levlev, FreqSum(scc, "nivtime"))
  

  
  
  #4.1.3 Average time from arrival at hospital to acute treatment with NIV
  
  
  mediSum(scc, "nivwaithours")
  levlev <- cbind(levlev, mediSum(scc, "nivwaithours"))
  
  
  # 5.1.1 Is a spirometry result available?
  
  FreqSum(scc, "q71spir")
  levlev <- cbind(levlev, FreqSum(scc, "q71spir"))
  
  # 5.1.2 degree of airflow obstruction
  
  scc$obstruct <- NA
  scc$obstruct[scc$obstruction == 0] <- "No(>=0.7)"
  scc$obstruct[scc$obstruction == 1] <- "Yes(<0.7)"
  scc$obstruct[scc$obstruction == 2] <- "Invalid_ratio(<0.2or>1)"
  scc$obstruct <- as.factor(scc$obstruct)
  
  FreqSum(scc, "obstruct")
  levlev <- cbind(levlev, FreqSum(scc, "obstruct"))
  
  
  # 5.1.3 If a spirometry result is available, what is the patients most recent FEV1?
  
  mediSum(scc, "q72fev1")
  levlev <- cbind(levlev, mediSum(scc, "q72fev1"))
  
  # CP(medTable(scc, "q72fev1"))
  # scc %>% filter(country == "England", !is.na(q72fev1)) %>% nrow()
  # scc %>% filter(country == "Wales", !is.na(q72fev1)) %>% nrow()
  # scc %>% filter(!is.na(q72fev1)) %>% nrow()
  
  
  # 6.1.1 What was the smoking status for this patient, as documented for the current admission?
  
  
  FreqSum(scc, "q81smokstatus")
  levlev <- cbind(levlev, FreqSum(scc, "q81smokstatus"))
  
  # 6.1.2 If a current smoker, was the patient prescribed smoking-cessation pharmacotherapy during the
  # current admission?
  
  FreqSum(scc, "q82smokcess")
  levlev <- cbind(levlev, FreqSum(scc, "q82smokcess"))
  
  
  # 7.1.1 Was a DECAF score recorded for this patient?
  FreqSum(scc, "q41decafrec")
  levlev <- cbind(levlev, FreqSum(scc, "q41decafrec"))
  
  
  # 7.1.2 If yes, what was the recorded DECAF score?
  # Make sure it's classed as a factor to allow binding
  
  scc$q42decaf <- as.factor(scc$q42decaf)
  
  FreqSum(scc, "q42decaf")
  levlev <- cbind(levlev, FreqSum(scc, "q42decaf"))
  
  # 8.1 Average length of stay
  mediSum(scc, "lengthofstay")
  levlev <- cbind(levlev, mediSum(scc, "lengthofstay")) 
  
  table(scc$dischargeday, weekdays(scc$q91))
  scc$discharge_day <- weekdays(scc$q91)
  scc$discharge_day <- ordered(scc$discharge_day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                           "Friday", "Saturday", "Sunday"))
  
  FreqSum(scc, "discharge_day")
  levlev <- cbind(levlev, FreqSum(scc, "discharge_day"))
  
  # 8.3 Did the patient die as an inpatient in your hospital?
  
  FreqSum(scc, "died")
  levlev <- cbind(levlev, FreqSum(scc, "died"))
  
  
  # 8.4 Has a British Thoracic Society (BTS), or equivalent, discharge bundle been completed for this admission?
  
  FreqSum(scc, "q93bts")
  levlev <- cbind(levlev, FreqSum(scc, "q93bts"))
  

  
  
  #  8.5 What follow-up arrangements have been made for this patient?  
  
  # This is very confusing, but basically because the question starts off with a 'no' we have to rearrange the 
  # order so that it makes sense when you read it
  scc$anyarrangementsapparent <- NA
  scc$anyarrangementsapparent[scc$q94no == "Yes"] <- "No"
  scc$anyarrangementsapparent[scc$q94no == "No"] <- "Yes"
  scc$anyarrangementsapparent <- as.factor(scc$anyarrangementsapparent)
  
  summary(scc$q94no)
  FreqSum(scc, "anyarrangementsapparent") 
  FreqSum(scc, "q94assteam")
  FreqSum(scc, "q94gpfuadvised") 
  FreqSum(scc, "q94gpfuarranged") 
  FreqSum(scc, "q94phone")
  FreqSum(scc, "q94commadvised") 
  FreqSum(scc, "q94commarranged") 
  FreqSum(scc, "q94hospadvised") 
  FreqSum(scc, "q94hosparranged") 
  FreqSum(scc, "q94otherhealthcare") 
  FreqSum(scc, "q94eol")
  FreqSum(scc, "q94other")
  
  levlev <- cbind(levlev, 
                  FreqSum(scc, "anyarrangementsapparent"), 
                  FreqSum(scc, "q94assteam"),
                  FreqSum(scc, "q94gpfuadvised"), 
                  FreqSum(scc, "q94gpfuarranged"), 
                  FreqSum(scc, "q94phone"),
                  FreqSum(scc, "q94commadvised"), 
                  FreqSum(scc, "q94commarranged"), 
                  FreqSum(scc, "q94hospadvised"), 
                  FreqSum(scc, "q94hosparranged"), 
                  FreqSum(scc, "q94otherhealthcare"), 
                  FreqSum(scc, "q94eol"),
                  FreqSum(scc, "q94other")  )    
  
  
  # 9.1 Associations with time from arrival to acute treatment with NIV 
  # Median is 4
  
  # Put <= 4 days, >4 days, and died into the same grouping
  
  scc$toolongstay <- scc$longstay
  scc$toolongstay <- as.character(scc$toolongstay)
  scc$toolongstay[scc$died == "Yes"] <- "Died"
  scc$toolongstay <- as.factor(scc$toolongstay)
  
  
  table(scc$smokcessbin, scc$q82smokcess)
  scc$smokcessoffered <- NA
  scc$smokcessoffered[scc$smokcessbin == 0] <- "No"
  scc$smokcessoffered[scc$smokcessbin == 1] <- "Yes"
  scc$smokcessoffered <- as.factor(scc$smokcessoffered)
  
  table(scc$smokcessoffered, scc$q81smokstatus, useNA = "ifany")
  table(scc$smokcessoffered, scc$q82smokcess, useNA = "ifany")
  
  
  
  # NIV outcomes
  
  niv.scc1 <- scc[scc$nivtime == "<2 Hours", ] 
  
  niv.scc1$niv2hr_staytime <- niv.scc1$toolongstay
  FreqSum(niv.scc1, "niv2hr_staytime")
  levlev <- cbind(levlev, FreqSum(niv.scc1, "niv2hr_staytime"))
  
  niv.scc2 <- scc[scc$nivtime == "2-24 Hours", ] 
  
  niv.scc2$niv2to24hr_staytime <- niv.scc2$toolongstay
  FreqSum(niv.scc2, "niv2to24hr_staytime")
  levlev <- cbind(levlev, FreqSum(niv.scc2, "niv2to24hr_staytime"))
  
  
  
 
  
  # 9.2 Associations with review by a member of the respiratory team within 24 hours
  
  
  resp.scc1 <- scc[scc$reviewwithin24hrs == "Yes", ] 
  
  # Received specialist review - elsewhere in the table
  # table(scc$q31resprev, scc$longstay)
  
  # detach("package:epitools", unload=TRUE)
  
  resp.scc1$resprev24hr_staytime <- resp.scc1$toolongstay
  FreqSum(resp.scc1, "resprev24hr_staytime")
  levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_staytime"))
  
  resp.scc1$resprev24hr_o2_pres_if_req_variable <- resp.scc1$o2_pres_if_req_variable
  FreqSum(resp.scc1, "resprev24hr_o2_pres_if_req_variable")
  levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_o2_pres_if_req_variable"))
  
  resp.scc1$resprev24hr_niv2hrbin <- resp.scc1$niv2hrbin
  FreqSum(resp.scc1, "resprev24hr_niv2hrbin")
  levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_niv2hrbin"))
  
  resp.scc1$resprev24hr_smokcessbin <- resp.scc1$smokcessbin
  FreqSum(resp.scc1, "resprev24hr_smokcessbin")
  levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_smokcessbin"))
  
  resp.scc1$resprev24hr_dischargebin <- resp.scc1$dischargebin
  FreqSum(resp.scc1, "resprev24hr_dischargebin")
  levlev <- cbind(levlev, FreqSum(resp.scc1, "resprev24hr_dischargebin"))
  
  
  
  # table(scc$dischargebin, scc$died, useNA = "ifany")
  # table(scc$dischargebin, scc$q93bts, useNA = "ifany")
  # 
  # summary(scc$dischargebin)
  # summary(scc$died)
  # 
  # table(scc$o2bin, scc$q31resprev, useNA = "ifany")
  # table(scc$q82smokcess, scc$smokcessbin, useNA = "ifany")
  
  # Did not receive a specialist review within 24 hours
  resp.scc2 <- scc[scc$reviewwithin24hrs == "No", ] 
  
  # Received specialist review
  # elsewhere in the table(scc$q31resprev, scc$longstay)
  
  # detach("package:epitools", unload=TRUE)
  
  resp.scc2$noresprevwithin24hrs_staytime <- resp.scc2$toolongstay
  FreqSum(resp.scc2, "noresprevwithin24hrs_staytime")
  levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_staytime"))
  
  resp.scc2$noresprevwithin24hrs_o2_pres_if_req_variable <- resp.scc2$o2_pres_if_req_variable
  FreqSum(resp.scc2, "noresprevwithin24hrs_o2_pres_if_req_variable")
  levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_o2_pres_if_req_variable"))
  
  resp.scc2$noresprevwithin24hrs_niv2hrbin <- resp.scc2$niv2hrbin
  FreqSum(resp.scc2, "noresprevwithin24hrs_niv2hrbin")
  levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_niv2hrbin"))
  
  resp.scc2$noresprevwithin24hrs_smokcessbin <- resp.scc2$smokcessbin
  FreqSum(resp.scc2, "noresprevwithin24hrs_smokcessbin")
  levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_smokcessbin"))
  
  resp.scc2$noresprevwithin24hrs_dischargebin <- resp.scc2$dischargebin
  FreqSum(resp.scc2, "noresprevwithin24hrs_dischargebin")
  levlev <- cbind(levlev, FreqSum(resp.scc2, "noresprevwithin24hrs_dischargebin"))
  # 9.2 odds ratios
  
  
  
  # Section 10: Web-tool run charts 
  
  scc$adj_dischargebunrec <- NA
  scc$adj_dischargebunrec[scc$dischargebin == 0] <- "No"
  scc$adj_dischargebunrec[scc$dischargebin == 1] <- "Yes"
  scc$adj_dischargebunrec <- as.factor(scc$adj_dischargebunrec)
  
  summary(scc$adj_dischargebunrec)
  
  FreqSum(scc, "adj_dischargebunrec")
  levlev <- cbind(levlev, FreqSum(scc, "adj_dischargebunrec"))
  
  # Oxygen prescribed to a target saturation:
  table(scc$o2bin, scc$q52o2targ, useNA = "ifany")
  
  scc$o2targetsatgiven <- NA
  scc$o2targetsatgiven[scc$q52o2targ == "88-92%"] <- "Yes"
  scc$o2targetsatgiven[scc$q52o2targ == "94-98%"] <- "Yes"
  scc$o2targetsatgiven[scc$q52o2targ == "Other"] <- "Yes"
  scc$o2targetsatgiven[scc$q52o2targ == "Target range not stipulated"] <- "No"
  scc$o2targetsatgiven <- as.factor(scc$o2targetsatgiven)
  summary(scc$o2targetsatgiven)
  
  FreqSum(scc, "o2targetsatgiven")
  levlev <- cbind(levlev, FreqSum(scc, "o2targetsatgiven"))
  
  # table(scc$dischargebin, scc$q93bts, useNA = "ifany")
  # 
  # 
  # 
  # summary(scc$reviewwithin24hrs)
  # summary(scc$dischargebin) # Out of those who didn't die or self-discharge or put down 'other'
  
  scc$bpt <- "No"
  scc$bpt[scc$reviewwithin24hrs == "Yes" & scc$dischargebin == 1] <- "Yes"
  scc$bpt <- as.factor(scc$bpt)
  
  FreqSum(scc, "bpt")
  levlev <- cbind(levlev, FreqSum(scc, "bpt"))
  
 
  
  
  completedlevlev <- full_join(completedlevlev, levlev)
  
}


CP(completedlevlev)


# for (i in 1:nrow(completedlevlev)) {
# for (j in 1:ncol(completedlevlev)) {
# 
#   if (completedlevlev[i,j] <5 & grep1(colnames(completedlevlev)[[j]], c("median", "perc"), fixed = TRUE) = FALSE) (completedlevlev[i,j+1] <- NA)
# }
# }
# All column names that need to be replaced if <5

repcol <- completedlevlev %>% select(-matches("median")) %>% select(-matches("quart")) %>% select(-matches("perc")) %>% 
  select_if(is.numeric) %>% colnames()

# This subsets completedlevlev into just those columns that match the names in repcol in order to change their class
completedlevlev[ , colnames(completedlevlev) %in% 
                   repcol == TRUE] <- completedlevlev[ , colnames(completedlevlev) %in% repcol == TRUE] %>% 
  mutate_all(funs(as.character))

# If we want those with <5 to be marked as '<5', unblank this...:
# for (i in repcol) {
#   completedlevlev[[i]] <- recode(completedlevlev[[i]], "0" = "<5", "1" = "<5", "2" = "<5", "3" = "<5", "4" = "<5")
# }

# This gives the list of column names
# perccol <- completedlevlev %>% select(matches("perc")) %>% colnames()
length(unique(sccmastercopy2$hospital))

# This gives the list of indices for the columns containing the text 'perc'
perccol <- grep("perc", colnames(completedlevlev))



for( j in perccol){ 
  completedlevlev[ is.na(completedlevlev[,j]), j-1 ] <- "0"
}

for( j in perccol){ 
  completedlevlev[ is.na(completedlevlev[,j]), j ] <- 0
}



# Finally...
# write.csv(completedlevlev,
#           "D:/Alex/Secondary Care Clinical/data/dataStore/complete_analysis.csv",
#           row.names = FALSE)

# 

