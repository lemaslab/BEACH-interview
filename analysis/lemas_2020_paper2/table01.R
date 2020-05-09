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
# AGE                      
# categories: 20-30 | 31-40
# **************************************************************************** #

data_age <- read_excel("interviewehrdata.xlsx", 
                       sheet = "DEMO")
data_age <- data_age[1:29,]

data_age <- data_age %>%
  mutate(age_group_older = ifelse(Age>=31, TRUE, FALSE))

table(data_age$`Study Group`, data_age$age_group_older)

# fisher.test
age.test=fisher.test(data_age$`Study Group`, data_age$age_group_older)
tidy(age.test)

# **************************************************************************** #
# EDUCATION                     
# categories: "Professional" | "Bachelor" | "Tech/ Vocational School" | "Associates Degree"
# **************************************************************************** #

data_edu <- read_excel("interviewehrdata.xlsx", 
                       sheet = "DEMO")
data_edu <- data_edu[1:29,]

data_edu <- data_edu %>%
  mutate(Education = ifelse(Education %in% c("college graduate", "College Graduate"), "Bachelor", Education)) %>%
  mutate(Education = ifelse(Education %in% c("graduate", "some graduate", "Professional or Graduate Degree"), "Professional", Education))

table(data_edu$`Study Group`, data_edu$Education)

# fisher.test
edu.test=fisher.test(data_edu$`Study Group`, data_edu$Education)
tidy(edu.test)

# **************************************************************************** #
# RACE                    
# categories: "black or AA" | "white" | "other" | "missing"

# **************************************************************************** #

data_race <- read_excel("interviewehrdata.xlsx", 
                       sheet = "DEMO")
data_race <- data_race[1:29,]

table(data_race$`Study Group`, data_race$Race)

# fisher.test
race.test=fisher.test(data_race$`Study Group`, data_race$Race)
tidy(race.test)

# **************************************************************************** #
# EHR FAMILIARITY                    
# categories: "Familiar" | "Not familiar" | "Not Asked"
# **************************************************************************** #

data_fami <- read_excel("interviewehrdata.xlsx", 
                        sheet = "DEMO")
data_fami <- data_fami[1:29,]

table(data_fami$`Study Group`, data_fami$`Familiar?`)

# fisher.test
fami.test=fisher.test(data_fami$`Study Group`, data_fami$`Familiar?`)
tidy(fami.test)

# **************************************************************************** #
# WILLINGNESS TO RELEASE MOTHER'S EHR                       
# categories: "Yes" | "Conditional Yes or Ambivalent"
# **************************************************************************** #

data_mom_will <- read_excel("interviewehrdata.xlsx", 
                        sheet = "Question 1")
data_mom_will <- data_mom_will[complete.cases(data_mom_will$`Participant ID`),]

data_mom_will <- data_mom_will %>%
  mutate(mom_ehr = ifelse(is.na(`YES!`), "Conditional Yes or Ambivalent", "Yes"))

table(data_mom_will$`Study Group`, data_mom_will$mom_ehr)

# fisher.test
mom_will.test=fisher.test(data_mom_will$`Study Group`, data_mom_will$mom_ehr)
tidy(mom_will.test)

# **************************************************************************** #
# WILLINGNESS TO RELEASE INFANT'S EHR                    
# categories: "Yes" | "Conditional Yes or Ambivalent" | "Missing"
# **************************************************************************** #

data_inf_will <- read_excel("interviewehrdata.xlsx", 
                            sheet = "Question 2")
data_inf_will <- data_inf_will[complete.cases(data_inf_will$`Participant ID`),]

data_inf_will <- data_inf_will %>%
  mutate(inf_ehr = ifelse(is.na(`Yes`), "Conditional Yes or Ambivalent", "Yes")) %>%
  mutate(inf_ehr = ifelse(is.na(`Response (yes, yes with condition, no, ambivalent)`), "Missing", inf_ehr))

table(data_inf_will$`Study Group`, data_inf_will$inf_ehr)

# fisher.test
inf_will.test=fisher.test(data_inf_will$`Study Group`, data_inf_will$inf_ehr)
tidy(inf_will.test)

# **************************************************************************** #
# LENGTH OF INFANT'S EHR RELEASE TIME                    
# categories: "Equal or longer than length of study" | "Others" | "Missing"
# **************************************************************************** #

data_lot <- read_excel("interviewehrdata.xlsx", 
                            sheet = "Question 4")
data_lot <- data_lot[complete.cases(data_lot$`Participant ID`),]

data_lot <- data_lot %>%
  mutate(lot = ifelse(`Response` == "N", "Missing", "Other")) %>%
  mutate(lot = ifelse(is.na(`For Length of Study`), lot, "Equal or longer than length of study"))

table(data_lot$`Study Group`, data_lot$lot)

# fisher.test
lot.test=fisher.test(data_lot$`Study Group`, data_lot$lot)
tidy(lot.test)
