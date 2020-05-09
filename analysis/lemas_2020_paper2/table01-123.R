##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:             Xinsong Du 
# START  Date:        May 07, 2020
# Project:            BEACH Interview
# Description:        Data Analysis for Paper #2- Table 01

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(dplyr)
library(readxl)
library(broom)

# **************************************************************************** #
# ***************                Load data                     *************** #
# **************************************************************************** #

# Moms agreed to provide their EHR
mom_ehr_yes <- c("BIS006", "BIS030", "PRG001", "PRG006", "PRG007", "PRG008", "PRG009", 
                 "PRG012", "PRG013", "PRG014", "PRG015", "PRG016", "PRG017", "PRG018",
                 "PRG020", "BIS008", "BIS010", "BIS014")

# Moms agreed to provide their infant's EHR
inf_ehr_yes <- c("BIS030", "PRG001", "PRG005", "PRG006", "PRG008", "PRG012", "PRG013", 
                 "PRG014", "PRG015", "PRG016", "PRG017", "PRG018", "PRG020", "BIS008",
                 "BIS009", "BIS010", "PRG002", "BIS014")
inf_ehr_missing <- c("BIS001", "BIS013", "PRG004", "PRG010")

# Moms have response of if they were willing to provide their infnat's EHR for a time period 
# that equal or longer than length of study
lot_mom <- c("BIS001", "BIS003", "BIS006", "BIS011", "BIS013", "BIS030", "PRG001", 
             "PRG003", "PRG004", "PRG005", "PRG006", "PRG007", "PRG008", "PRG009",
             "PRG011", "PRG012", "PRG013", "PRG014", "PRG015", "PRG016", "PRG017", 
             "PRG018", "PRG020", "BIS008")

# Moms agreed to provide their infnat's EHR for a time period that equal or longer than 
# length of study
lot_yes <- c("BIS003", "BIS030", "PRG001", "PRG003", "PRG004", "PRG005", "PRG006", 
             "PRG008", "PRG009", "PRG011", "PRG015", "PRG017")

# Load data from excel
data_ehr <- read_excel("interviewehrdata.xlsx", 
                       sheet = "DEMO")
data_ehr <- data_ehr[1:29,]

data_ehr <- data_ehr %>% 
  mutate(mom_ehr = ifelse(`Participant ID` %in% mom_ehr_yes, "Yes", "Ambivalent or Conditional Yes")) %>%
  mutate(inf_ehr = ifelse(`Participant ID` %in% inf_ehr_yes, "Yes", "Ambivalent or Conditional Yes")) %>%
  mutate(inf_ehr = ifelse(`Participant ID` %in% inf_ehr_missing, "Missing", inf_ehr)) %>%
  mutate(lot = ifelse(`Participant ID` %in% lot_mom, "Other", "Missing")) %>%
  mutate(lot = ifelse(`Participant ID` %in% lot_yes, "Yes", lot))

# **************************************************************************** #
# AGE                      
# categories: 20-30 | 31-40
# **************************************************************************** #

data_age <- data_ehr %>%
  mutate(age_group_older = ifelse(Age>=31, TRUE, FALSE))

data_age_mom_ehr = data_age
data_age_inf_ehr <- data_age[data_age$inf_ehr!="Missing",]
data_age_lot <- data_age[data_age$lot!="Missing",]

table(data_age_mom_ehr$mom_ehr, data_age_mom_ehr$age_group_older)
table(data_age_inf_ehr$inf_ehr, data_age_inf_ehr$age_group_older)
table(data_age_lot$lot, data_age_lot$age_group_older)

# fisher.test
age.test=fisher.test(data_age_mom_ehr$mom_ehr, data_age_mom_ehr$age_group_older)
tidy(age.test)
age.test=fisher.test(data_age_inf_ehr$inf_ehr, data_age_inf_ehr$age_group_older)
tidy(age.test)
age.test=fisher.test(data_age_lot$lot, data_age_lot$age_group_older)
tidy(age.test)

# **************************************************************************** #
# STUDY GROUP                    
# categories: "Pregnant" | "Breastfeeding"
# **************************************************************************** #

data_grp_inf_ehr <- data_ehr[data_ehr$inf_ehr!="Missing",]
data_grp_lot <- data_ehr[data_ehr$lot!="Missing",]

table(data_ehr$mom_ehr, data_ehr$`Study Group`)
table(data_grp_inf_ehr$inf_ehr, data_grp_inf_ehr$`Study Group`)
table(data_grp_lot$lot, data_grp_lot$`Study Group`)

# fisher.test
grp.test=fisher.test(data_ehr$mom_ehr, data_ehr$`Study Group`)
tidy(grp.test)
grp.test=fisher.test(data_grp_inf_ehr$inf_ehr, data_grp_inf_ehr$`Study Group`)
tidy(grp.test)
grp.test=fisher.test(data_grp_lot$inf_ehr, data_grp_lot$`Study Group`)
tidy(grp.test)

# **************************************************************************** #
# EDUCATION                     
# categories: "Professional" | "Bachelor" | "Tech/ Vocational School" | "Associates Degree"
# **************************************************************************** #

data_edu <- data_ehr %>%
  mutate(Education = ifelse(Education %in% c("college graduate", "College Graduate"), "Bachelor", Education)) %>%
  mutate(Education = ifelse(Education %in% c("graduate", "some graduate", "Professional or Graduate Degree"), "Professional", Education))

data_edu_inf_ehr <- data_edu[data_edu$inf_ehr!="Missing",]
data_edu_lot <- data_edu[data_edu$lot!="Missing",]

table(data_ehr$mom_ehr, data_edu$Education)
table(data_edu_inf_ehr$inf_ehr, data_edu_inf_ehr$Education)
table(data_edu_lot$lot, data_edu_lot$Education)

# fisher.test
edu.test=fisher.test(data_ehr$mom_ehr, data_edu$Education)
tidy(edu.test)
edu.test=fisher.test(data_edu_inf_ehr$inf_ehr, data_edu_inf_ehr$Education)
tidy(edu.test)
edu.test=fisher.test(data_edu_lot$lot, data_edu_lot$Education)
tidy(edu.test)

# **************************************************************************** #
# RACE                    
# categories: "black or AA" | "white" | "other" | "missing"

# **************************************************************************** #

data_race_inf_ehr <- data_ehr[data_ehr$inf_ehr!="Missing",]
data_race_lot <- data_ehr[data_ehr$lot!="Missing",]

table(data_ehr$mom_ehr, data_ehr$Race)
table(data_race_inf_ehr$inf_ehr, data_race_inf_ehr$Race)
table(data_race_lot$lot, data_race_lot$Race)

# fisher.test
race.test=fisher.test(data_ehr$mom_ehr, data_ehr$Race)
tidy(race.test)
race.test=fisher.test(data_race_inf_ehr$inf_ehr, data_race_inf_ehr$Race)
tidy(race.test)
race.test=fisher.test(data_race_lot$lot, data_race_lot$Race)
tidy(race.test)

# **************************************************************************** #
# EHR FAMILIARITY                    
# categories: "Familiar" | "Not familiar" | "Not Asked"
# **************************************************************************** #

data_fami_inf_ehr <- data_ehr[data_ehr$inf_ehr!="Missing",]
data_fami_lot <- data_ehr[data_ehr$lot!="Missing",]

table(data_ehr$mom_ehr, data_ehr$`Familiar?`)
table(data_fami_inf_ehr$inf_ehr, data_fami_inf_ehr$`Familiar?`)
table(data_fami_lot$lot, data_fami_lot$`Familiar?`)

# fisher.test
fami.test=fisher.test(data_ehr$mom_ehr, data_ehr$`Familiar?`)
tidy(fami.test)
fami.test=fisher.test(data_fami_inf_ehr$inf_ehr, data_fami_inf_ehr$`Familiar?`)
tidy(fami.test)
fami.test=fisher.test(data_fami_lot$lot, data_fami_lot$`Familiar?`)
tidy(fami.test)
