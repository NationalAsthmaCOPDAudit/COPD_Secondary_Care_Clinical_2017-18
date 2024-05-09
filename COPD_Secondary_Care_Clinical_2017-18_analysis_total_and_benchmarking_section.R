#----------------------------------------#
# Secondary Care Clinical 2018 Analysis  #
# Author: Alex Adamson                   #
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

IMDeng$imdcountry <- "England"
IMDwales$imdcountry <- "Wales"


# Join them together
IMD <- rbind(IMDeng, IMDwales)

# Merge them with the SCC data


scc <- left_join(scc, IMD, by = "lsoa11")

# Some baseline statistics:

totN <- nrow(scc)      # Total N = 74952
print(totN)



# To get the hospital country, we read in Tim's file linking hospital code to hospital.

country <- read.csv("Z:/Group_work/PS_AA/Secondary Care Clinical 2017-2018/hospital_country.csv")

country <- select(country, Code, Country)
country <- rename(country, hospital = Code)

# country <- select(country, hospital, country)

str(country)
country$hospital <- as.character(country$hospital)

country <- unique(country)
nrow(country)

summary(scc)

scc <- left_join(scc, country, by = "hospital")

scc <- rename(scc, country = Country)

head(scc)

summary(scc$country)





# Let's create a function that will create median and interquartile tables, and one that will create N and % tables.

medTable <- function(x, varname) {   
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

str(scc)



nrow(filter(scc, country == "England"))
nrow(filter(scc, country == "Wales"))
nrow(scc)

# 1.1 Age

CP(medTable(scc, "age"))

# 1.2 Gender

CP(myFreqTable(scc, "gender"))

# 1.3.1 IMD quintiles

CP(myFreqTable(scc, "IMD.quintile"))
myFreqTable(scc, "IMD.quintile")

# Very useful website explaining everything here: https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/ 

plot <- ggplot(scc, aes(x = IMD.quintile)) + theme_minimal() +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + facet_grid(.~country) +
  scale_y_continuous(labels=scales::percent) + xlab("IMD quintile") +
  ylab("Percentage of the hospital population in each IMD quintle") + 
 # scale_fill_manual("legend", values = c("black", "orange", "blue", "yellow", "green"))
 # scale_fill_manual("legend", values = c(1 = "black", 2 = "orange", 3 = "blue", 4 = "yellow", 5 = "green"))

scale_color_manual(values=c("black", "grey", "green3", "green", "red")) + guides(fill = "none")
#  scale_fill_manual("legend", values = c("1" = "black", "2" = "orange", "3" = "blue"))




# 1.4.1 Average number of admissions with interquartile range
hosp <- scc %>% group_by(hospital) %>% slice(1)
sort(hosp$hospadmissions)
quantile(hosp$hospadmissions, prob = c(0.25, 0.5, 0.75), type = 2)
sum(!is.na(scc$hospadmissions))

hospeng <- scc %>% filter(country == "England") %>% group_by(hospital) %>% slice(1)
sort(hospeng$hospadmissions)
quantile(hospeng$hospadmissions, prob = c(0.25, 0.5, 0.75), type = 2)
nrow(filter(scc, country == "England"))

hospwal <- scc %>% filter(country == "Wales") %>% group_by(hospital) %>% slice(1)
sort(hospwal$hospadmissions)
quantile(hospwal$hospadmissions, prob = c(0.25, 0.5, 0.75), type = 2)
nrow(filter(scc, country == "Wales"))


#1.4.1 Age at admission by gender

str(scc$gender)

scc %>% filter(gender == "Male") %>% select(age) %>% summary
scc %>% filter(gender == "Male") %>% nrow()

scc %>% filter(gender == "Female") %>% select(age) %>% summary
scc %>% filter(gender == "Female") %>% nrow()

scc %>% filter(country == "England") %>% filter(gender == "Male") %>% select(age) %>% summary()
scc %>% filter(country == "England") %>% filter(gender == "Male") %>% nrow()

scc %>% filter(country == "England") %>% filter(gender == "Female") %>% select(age) %>% summary()
scc %>% filter(country == "England") %>% filter(gender == "Female") %>% nrow()

scc %>% filter(country == "Wales") %>% filter(gender == "Male") %>% select(age) %>% summary()
scc %>% filter(country == "Wales") %>% filter(gender == "Male") %>% nrow()

scc %>% filter(country == "Wales") %>% filter(gender == "Female") %>% select(age) %>% summary()
scc %>% filter(country == "Wales") %>% filter(gender == "Female") %>% nrow()

# 1.4.3 Average time, in hours, between arrival and admission

CP(medTable(scc, "admitwaithours"))

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
sum(admisstimedow.N)

# Day of the week %
admisstimedow.perc <- round(prop.table(admisstimedow.N, 2)*100, 1)

summary(admisstimedow.perc)

# Put them together into a matrix that can then be copy and pasted

togeth <- paste(admisstimedow.N, " (", admisstimedow.perc, ")", sep = "")
togeth <- matrix(togeth, nrow = 12, ncol = 7)
write.table(togeth, "clipboard", sep = "\t")

# We need to know how many admissions there were for each day as well

margin.table(admisstimedow.N, 2) %>% CP()

# Right. 


# 2.1.1 Has the patient been reviewed by an acute physician of grade ST3 or above?

# Just going to assume that 'not clear' is the same as 'na'
scc$q21physrev[is.na(scc$q21physrev) == TRUE] <- "Not clear"
CP(myFreqTable(scc, "q21physrev"))

# 2.2.1 Has a member of the respiratory team reviewed the patient during the admission? 
CP(myFreqTable(scc, "q31resprev"))

# 2.2.2 Was the patient reviewed by a member of the respiratory team within 24 hours?
CP(myFreqTable(scc, "reviewwithin24hrs"))

describe(scc$reviewwaithours)

# 2.2.3 Average time from admission to review


CP(medTable(scc, "reviewwaithours"))

summary(scc$reviewwaithours)

# 3.1.1 Was oxygen prescribed for this patient?
# Adding values to o2bin variable that were missing for oxygen prescribed but have ranges removed:

table(scc$o2bin, scc$q52o2targ, useNA = "ifany")

summary(scc$o2bin)
scc[which(is.na(scc$o2bin) & (scc$q52o2targ == "88-92%" | scc$q52o2targ == "94-98%")), ]$o2bin <- 1
scc[which(is.na(scc$q51o2presc) & (scc$q52o2targ == "88-92%" | scc$q52o2targ == "94-98%")), ]$q51o2presc <- "Yes"


CP(myFreqTable(scc, "o2bin"))
# Footnote: how many didn't require it?
table(scc$q51o2presc, scc$o2bin, useNA = "ifany")
table(scc$q51o2presc, useNA = "ifany")


# Those in which it was not needed


table(scc$q51o2presc, useNA = "ifany")[[3]]

(table(scc$q51o2presc, useNA = "ifany")[[3]]/(
  sum(table(scc$q51o2presc, scc$o2bin, useNA = "ifany"))))*100 

# 3.1.2 If oxygen was prescribed was it to a stipulated target range?


table(scc$o2bin, scc$q52o2targ, useNA = "ifany")

summary(scc)
head(scc)
CP(myFreqTable(scc, "q52o2targ"))



summary(scc$q52o2targ)


# 4.1.1 If the patient received acute treatment with NIV, was it received within 2 hours of arrival?

CP(myFreqTable(scc, "nivwithin2hrs"))


# 4.1.2 Time from arrival to acute treatment with NIV

CP(myFreqTable(scc, "nivtime"))


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

# 10 days
sccKM$nivwaitdays <- sccKM$nivwaithours/24
survfit(Surv(sccKM$nivwaitdays, sccKM$seen) ~ sccKM$country, data = sccKM) %>% 
  plot_survfit(ci = TRUE, legend.title = "Country", xmax = 10, xbreaks = c(1:10)) + 
  labs(x = "Time (days)", y = "Culmulative % of patients that have received NIV")

# All time (1066 hours)
# 10 days
sccKM$nivwaitdays <- sccKM$nivwaithours/24
survfit(Surv(sccKM$nivwaitdays, sccKM$seen) ~ sccKM$country, data = sccKM) %>% 
  plot_survfit(ci = TRUE, legend.title = "Country", xmax = 45, xbreaks = seq(0, 45, 5)) + 
  labs(x = "Time (days)", y = "Culmulative % of patients that have received NIV")


# plot <- plot + theme(panel.grid.minor = element_line(colour = "black", size = 0.5))


#4.1.3 Average time from arrival at hospital to acute treatment with NIV
CP(medTable(scc, "nivwaithours"))

# 5.1.1 Is a spirometry result available?

CP(myFreqTable(scc, "q71spir"))


# 5.1.2 degree of airflow obstruction

CP(myFreqTable(scc, "obstruction"))

# 5.1.3 If a spirometry result is available, what is the patients most recent FEV1?

CP(medTable(scc, "q72fev1"))
scc %>% filter(country == "England", !is.na(q72fev1)) %>% nrow()
scc %>% filter(country == "Wales", !is.na(q72fev1)) %>% nrow()
scc %>% filter(!is.na(q72fev1)) %>% nrow()


# 6.1.1 What was the smoking status for this patient, as documented for the current admission?
CP(myFreqTable(scc, "q81smokstatus"))

# 6.1.2 If a current smoker, was the patient prescribed smoking-cessation pharmacotherapy during the
# current admission?

CP(myFreqTable(scc, "q82smokcess"))


# 7.1.1 Was a DECAF score recorded for this patient?
CP(myFreqTable(scc, "q41decafrec"))



# 7.1.2 If yes, what was the recorded DECAF score?
# Make sure it's classed as a factor to allow binding

scc$q42decaf <- as.factor(scc$q42decaf)

CP(myFreqTable(scc, "q42decaf"))


# 8.1 Average length of stay
CP(medTable(scc, "lengthofstay"))

table(scc$dischargeday, weekdays(scc$q91))
scc$dischargeday2 <- weekdays(scc$q91)
scc$dischargeday2 <- ordered(scc$dischargeday2, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                   "Friday", "Saturday", "Sunday"))

CP(myFreqTable(scc, "dischargeday2"))


# 8.3 Did the patient die as an inpatient in your hospital?
CP(myFreqTable(scc, "died"))



# 8.4 Has a British Thoracic Society (BTS), or equivalent, discharge bundle been completed for this admission?
CP(myFreqTable(scc, "q93bts"))


#  8.5 What follow-up arrangements have been made for this patient?  

CP(myFreqTable(scc, "q94no")) 
CP(myFreqTable(scc, "q94assteam")) 
CP(myFreqTable(scc, "q94gpfuadvised")) 
CP(myFreqTable(scc, "q94gpfuarranged")) 
CP(myFreqTable(scc, "q94phone")) 
CP(myFreqTable(scc, "q94commadvised")) 
CP(myFreqTable(scc, "q94commarranged")) 
CP(myFreqTable(scc, "q94hospadvised")) 
CP(myFreqTable(scc, "q94hosparranged")) 
CP(myFreqTable(scc, "q94otherhealthcare")) 
CP(myFreqTable(scc, "q94eol"))
CP(myFreqTable(scc, "q94other"))
    

# 9.1 Associations with time from arrival to acute treatment with NIV 
# Median is 4


scc$toolongstay <- scc$longstay
scc$toolongstay <- as.character(scc$toolongstay)
scc$toolongstay[scc$died == "Yes"] <- "Died"
scc$toolongstay <- as.factor(scc$toolongstay)

table(scc$toolongstay, scc$died)

CP(table(scc$toolongstay, scc$nivtime))
CP(round(prop.table(table(scc$toolongstay, scc$nivtime), 2)*100, 1))

str(scc$toolongstay)



ORprep1 <- scc %>% dplyr::select(nivtime, longstay) %>% filter(nivtime %in% c("<2 Hours", "2-24 Hours"))
ORprep2 <- scc %>% dplyr::select(nivtime, died) %>% filter(nivtime %in% c("<2 Hours", "2-24 Hours"))



glm(longstay ~ nivtime,family=binomial(link='logit'), data = ORprep1)  %>% logistic.display()
glm(died ~ nivtime,family=binomial(link='logit'), data = ORprep2)  %>% logistic.display()




# 9.2 Associations with review by a member of the respiratory team ever

resp.scc1 <- scc[scc$q31resprev == "Yes", ] 

# Received specialist review
table(scc$q31resprev, scc$longstay)

# detach("package:epitools", unload=TRUE)

CP(myFreqTable(resp.scc1, "toolongstay"))

CP(myFreqTable(resp.scc1, "o2bin"))

table(resp.scc1$o2bin, resp.scc1$q51o2presc, useNA = "ifany")
table(resp.scc1$o2bin, resp.scc1$q31resprev)




CP(myFreqTable(resp.scc1, "niv2hrbin"))
CP(myFreqTable(resp.scc1, "smokcessbin"))
CP(myFreqTable(resp.scc1, "dischargebin"))

# table(scc$dischargebin, scc$died, useNA = "ifany")
# table(scc$dischargebin, scc$q93bts, useNA = "ifany")
# 
# summary(scc$dischargebin)
# summary(scc$died)
# 
# table(scc$o2bin, scc$q31resprev, useNA = "ifany")
# table(scc$q82smokcess, scc$smokcessbin, useNA = "ifany")

# Received specialist review within 24 hours
resp.scc2 <- scc[scc$reviewwithin24hrs == "Yes", ] 

CP(myFreqTable(resp.scc2, "toolongstay"))
CP(myFreqTable(resp.scc2, "o2bin"))
table(resp.scc2$o2bin)
table(scc$q52o2targ)
CP(myFreqTable(resp.scc2, "niv2hrbin"))
table(scc$niv2hrbin)
CP(myFreqTable(resp.scc2, "smokcessbin"))
CP(myFreqTable(resp.scc2, "dischargebin"))


table(scc$died, scc$toolongstay)
table(scc$o2bin)
sum(table(scc$niv2hrbin))
table(scc$q61niv)
table(scc$smokcessbin)

# 9.2 odds ratios

# For received specialist review

# long stay/died
# library(epiDisplay)
# library(RVAideMemoire)
summary(scc$reviewwithin24hrs)

scc$reviewwithin24hrsJR <- NA
scc$reviewwithin24hrsJR[scc$q31resprev == "Yes"] <- 0         #All those who have a respiratory review are coded as 0
scc$reviewwithin24hrsJR[scc$reviewwithin24hrs == "Yes"] <- 1  # Then, those that have the review within 24 hours are updated from 0 to 1
scc$reviewwithin24hrsJR <- as.factor(scc$reviewwithin24hrsJR)

summary(scc$reviewwithin24hrsJR)
summary(scc$reviewwithin24hrs)
summary(scc$q31resprev)


table(scc$reviewwithin24hrsJR, scc$q31resprev)
table(scc$reviewwithin24hrsJR, scc$reviewwithin24hrs, useNA = "ifany")



summary(scc$q31resprev)

table(scc$longstay, scc$q31resprev)
glm(longstay ~ q31resprev,family=binomial(link='logit'), data = scc)  %>% logistic.display()
glm(died ~ q31resprev,family=binomial(link='logit'), data = scc)  %>% logistic.display()

table(scc$longstay, scc$q31resprev)
table(scc$died, scc$q31resprev)
table(scc$died, scc$reviewwithin24hrs)

glm(longstay ~ reviewwithin24hrsJR,family=binomial(link='logit'), data = scc)  %>% logistic.display()
glm(died ~ reviewwithin24hrsJR,family=binomial(link='logit'), data = scc)  %>% logistic.display()


table(scc$o2bin, scc$q51o2presc, useNA = "ifany")
summary(scc$q52o2targ)

scc$q31resprevtest <- as.factor(scc$q31resprev)

glm(o2bin ~ q31resprev,family=binomial(link='logit'), data = scc)  %>% logistic.display()
glm(o2bin ~ reviewwithin24hrsJR,family=binomial(link='logit'), data = scc)  %>% logistic.display()


glm(niv2hrbin ~ q31resprev,family=binomial(link='logit'), data = scc)  %>% logistic.display()
glm(niv2hrbin ~ reviewwithin24hrsJR,family=binomial(link='logit'), data = scc)  %>% logistic.display()

glm(smokcessbin ~ q31resprev,family=binomial(link='logit'), data = scc)  %>% logistic.display()
glm(smokcessbin ~ reviewwithin24hrsJR,family=binomial(link='logit'), data = scc)  %>% logistic.display()



table(scc$smokcessbin, scc$q31resprev, useNA = "ifany")
table(scc$smokcessbin, scc$reviewwithin24hrsJR, useNA = "ifany")

glm(dischargebin ~ q31resprev,family=binomial(link='logit'), data = scc)  %>% logistic.display()
glm(dischargebin ~ reviewwithin24hrsJR,family=binomial(link='logit'), data = scc)  %>% logistic.display()

table(scc$dischargebin, useNA = "ifany")

table(scc$dischargebin, scc$q31resprev)

# Section 10: Web-tool run charts 

table(scc$reviewwithin24hrs, useNA = "ifany")

summary(scc$q31resprev)
summary(scc$reviewwithin24hrsint)
summary(scc$dischargebin) # Out of those who didn't die or self-discharge

scc$bpt <- 0
scc$bpt[scc$reviewwithin24hrs == "Yes" & scc$dischargebin == 1] <- 1

summary(scc$bpt)
table(scc$bpt, scc$dischargebin)

summary(scc$o2bin)
summary(scc$q52o2targ)

# Oxygen prescribed to a target saturation:
table(scc$o2bin, scc$q52o2targ, useNA = "ifany")

scc$o2targetsatgiven <- NA
scc$o2targetsatgiven[scc$q52o2targ == "88-92%"] <- "Yes"
scc$o2targetsatgiven[scc$q52o2targ == "94-98%"] <- "Yes"
scc$o2targetsatgiven[scc$q52o2targ == "Other"] <- "Yes"
scc$o2targetsatgiven[scc$q52o2targ == "Target range not stipulated"] <- "No"
scc$o2targetsatgiven <- as.factor(scc$o2targetsatgiven)
summary(scc$o2targetsatgiven)


FreqSum <- function(x, varname) {
  
  varname <- as.character(varname)
  gen <- x %>% dplyr::select(!!varname) %>% drop_na()
  var_N <- data.frame(nrow(gen))
  colnames(var_N) <- paste0(varname, "_N")
  gen0 <- as.data.frame(table(gen[[1]]))
  gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
    rename(perc = Freq)
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



# Section 11: Benchmarking indicators:

summary(scc$nivwithin2hrs)
scc2 <- scc

scc <- scc2

scc$niveverbin[scc$nivwithin2hrs == "No" | scc$nivwithin2hrs == "Yes" | 
                 scc$nivwithin2hrs == "No time recorded"] <- 1
scc$niveverbin[is.na(scc$niveverbin) == TRUE] <- 0
summary(scc$niveverbin)
summary(scc$q81smokstatus)
scc$curr.smok[scc$q81smokstatus == "Current smoker"] <- 1
summary(scc$curr.smok)

sum(scc$q71spir, na.rm = TRUE)
scc$q71spir <- as.numeric(scc$q71spir) 
scc$q71spir[scc$q71spir == 1] <- 0
scc$q71spir[scc$q71spir == 2] <- 1

scc$q31resprev <- as.integer(scc$q31resprev)
summary(scc$q31resprev)
scc$q31resprev[scc$q31resprev == 1] <- 0
scc$q31resprev[scc$q31resprev == 2] <- 1

summary(scc$reviewwithin24hrsint)
summary(scc$reviewwithin24hrs)

scc$reviewwithin24hrsint <- as.integer(scc$reviewwithin24hrs) 
scc$reviewwithin24hrsint[scc$reviewwithin24hrsint == 1] <- 0
scc$reviewwithin24hrsint[scc$reviewwithin24hrsint == 2] <- 1

scc$dischargereqbin[scc$dischargebin == 0 | scc$dischargebin == 1] <- 1
table(scc$dischargebin, scc$dischargereqbin, useNA = "ifany")

summary(scc$dischargebin)

# Use summarise function to get necessary columns
bmk <- scc %>% dplyr::group_by(hospital) %>% summarise(trustcode = first(trustcode), 
        cases.audited = n(), niv.needed_n = sum(niveverbin), niv2hours_n = sum(niv2hrbin, na.rm = TRUE), 
        spirometry.available_n = sum(q71spir, na.rm = TRUE),
        curr.smok_n = sum(curr.smok, na.rm = TRUE), cess.offer_n = sum(smokcessbin, na.rm = TRUE),
        resp.rev.24hrs_n = sum(reviewwithin24hrsint, na.rm = TRUE), 
        discharge.bun.req_n = sum(dischargereqbin, na.rm = TRUE), 
        discharge.bun_n = sum(dischargebin, na.rm = TRUE))
        
# Use mutate function to go on from there
bmk <- mutate(bmk, niv2hours_perc = round(niv2hours_n / niv.needed_n, 2)*100,
              spirometry.available_perc = round(spirometry.available_n / cases.audited, 2)*100,
              cess.offer_perc = round(cess.offer_n / curr.smok_n, 2)*100,
              resp.rev.24hrs_perc = round(resp.rev.24hrs_n / cases.audited, 2)*100,
              discharge.bun_perc = round(discharge.bun_n / discharge.bun.req_n, 2)*100)

# Get the quantiles:
bmk <- mutate(bmk, niv2hours_quant = ecdf(bmk$niv2hours_perc[])(bmk$niv2hours_perc),
       spirometry.available_quant = ecdf(bmk$spirometry.available_perc[])(bmk$spirometry.available_perc),
       cess.offer_quant = ecdf(bmk$cess.offer_perc[])(bmk$cess.offer_perc),
       resp.rev.24hrs_quant = ecdf(bmk$resp.rev.24hrs_perc[])(bmk$resp.rev.24hrs_perc),
       discharge.bun_quant = ecdf(bmk$discharge.bun_perc[])(bmk$discharge.bun_perc))

colnames(bmk)

# They want some quantiles so...


quartz <- matrix(data = NA, nrow = 3, ncol = 6)
quartz[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

colnames(quartz) <- c("variable", "niv2hrs", "spir.available", "smoking.cess", "resp.rev24hrs", "discharge.bun") 


quartz[1:3, 2] <- quantile(bmk$niv2hours_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz[1:3, 3] <- quantile(bmk$spirometry.available_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz[1:3, 4] <- quantile(bmk$cess.offer_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz[1:3, 5] <- quantile(bmk$resp.rev.24hrs_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz[1:3, 6] <- quantile(bmk$discharge.bun_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)

quartz <- as.data.frame(quartz)

CP(quartz)


# Get rid of any numbers <5

# Convert the required columns to character

convcol <- c("cases.audited", "niv2hours_n", "spirometry.available_n", "cess.offer_n", "resp.rev.24hrs_n", "discharge.bun_n")

bmk[ , colnames(bmk) %in% convcol == TRUE] <- bmk[ , colnames(bmk) %in% convcol == TRUE] %>% mutate_all(funs(as.character))


# Then recode any values <5 as '<5'
for (i in c("cases.audited", "niv2hours_n", "spirometry.available_n", "cess.offer_n", "resp.rev.24hrs_n", "discharge.bun_n")) { 
  bmk[[i]] <- recode(bmk[[i]], "0" = "<5", "1" = "<5", "2" = "<5", "3" = "<5", "4" = "<5")
}


bmk <- mutate(bmk, niv2hours = paste(bmk$niv2hours_n, " (", bmk$niv2hours_perc, "%)", sep = ""),
              spirometry.available = paste(bmk$spirometry.available_n, " (",
                                           bmk$spirometry.available_perc, "%)", sep = ""),
              cess.offer = paste(bmk$cess.offer_n, " (", bmk$cess.offer_perc, "%)", sep = ""),
              resp.rev.24hrs = paste(bmk$resp.rev.24hrs_n, " (", bmk$resp.rev.24hrs_perc, "%)", sep = ""),
              discharge.bun = paste(bmk$discharge.bun_n, " (", bmk$discharge.bun_perc, "%)", sep = ""))
print(bmk)
str(bmk)

bmk <- bmk[ , c("trustcode", "hospital", "cases.audited", 
                "niv2hours", "spirometry.available", "cess.offer", "resp.rev.24hrs", "discharge.bun", 
                "niv.needed_n", "niv2hours_quant", 
                "spirometry.available_quant",
                "curr.smok_n", "cess.offer_quant", 
                "resp.rev.24hrs_quant",
                "discharge.bun.req_n", "discharge.bun_quant")]

# Get rid of cells that look messy
bmk[bmk[,] == "<5 (NaN%)"] <- "<5 (n/a%)"

# Get the hospital names back
hospnames <- read.csv(
  "Z:/Group_work/PS_AA/Secondary Care Clinical 2017-2018/hospital_country.csv",
  stringsAsFactors = FALSE)

hospnames <- hospnames[ , c("Code", "Name")] %>% rename(hospital = "Code", 
                                                        hospital.name = "Name")
hospnames <- unique(hospnames)

bmk <- left_join(bmk, hospnames, by = "hospital")
bmk <- bmk[ , c(1, 2, 17, # hospital details
                3,        # audited cases
                4:8,      # results (N + %) 
                10, 11, 13, 14, 16)] # quantiles

colnames(bmk)

# write.csv(bmk, file = "Z:/Group_work/PS_AA/Secondary Care Clinical 2017-2018/benchmarking_no_colour.csv",
#           row.names = FALSE)



