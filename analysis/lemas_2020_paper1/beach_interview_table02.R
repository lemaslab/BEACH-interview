##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:             Dominick Lemas 
# START  Date:        June 25, 2019 
# REVISE Date:        Mar  25, 2020
# Project:            BEACH Interview
# Description:        Data Analysis for Paper #1- Table 02
# Note:               Revisions suggested by BMC Pregnancy and Childbirth
#                     -break out data by study group and include biological samples

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(keyringr)
library(tidyverse)
library(redcapAPI)
library(REDCapR)
library(broom)
library(dplyr)
library(tidyr)

# Login to Gatorlink VPN

# Get Redcap API Token
# # https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html
credential_label <- "interview_api"
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"
interview_token<-decrypt_dpapi_pw(credential_path)
print(interview_token)

# Create connections
rcon <- redcapConnection(url=uri, token=interview_token)

# list of instruments
exportInstruments(rcon)

# export field names
exportFieldNames(rcon)

# Variables for table 01 
desired_fields_v1 <- c("record_id","int_study_grp","int_consent_complete",
                       "int_interview_complete","int_audio_length_min", # study 
                       "interphone_prepreg_bmi","interphone_age","mom3t_prepreg_bmi",
                       "int_guide_education","int_guide_employmnt","int_guide_occupation",
                       "biosample_collection_date", "biosample_mom_baby", "biosample_aliquot_type",
                       "crc_specimen_barcode","biosample_tube_type","biosample_aliquot_numb",
                       "biological_specimen_collection_complete","no_wkly_encounter_int", "encounter_date_int",
                       "encounter_type_int","encounter_other_int","learn_about_study_int", "study_other_int",
                       "see_flyer_int", "flyer_other_int", "ufhealth_clinic_int","beach_interview_study_encounters_complete",
                       "analysis_mat_age", "analysis_mat_age_cats", "analysis_mat_age_source", "analysis_bmi",
                       "analysis_bmi_cats", "analysis_bmi_source", "mom3t_education_2", "analysis_research_expr",
                       "analysis_kids_previous", "analysis_time_of_day","int_guide_transportation",
                       "int_guide_stoolcollect", "int_guide_studylength", "int_guide_visitlength", "int_guide_visitlength",
                       "int_guide_reminders","int_guide_contactby","int_guide_advanceremind","int_guide_timeofday")

# pull data
interview <- redcap_read(
  batch_size=150L,
  redcap_uri = uri, 
  token      = interview_token, 
  fields     = desired_fields_v1
  )$data

# check data pull
str(interview)
interview[1]
unique(interview$record_id)

# **************************************************************************** #
# SUBSET DATA: consented participants (n=40) & table 02 variables          
# **************************************************************************** #

dat=interview %>%
  filter(int_interview_complete==1) %>%
  select(record_id,int_study_grp,
         int_guide_studylength,int_guide_transportation,
         int_guide_visitlength, int_guide_timeofday,int_guide_reminders,
         int_guide_timeofday,int_guide_advanceremind,
         int_guide_contactby___1,int_guide_contactby___2,int_guide_contactby___3,
         int_guide_contactby___4) %>%
  as_tibble()

# study_length_cats, study_visit_cats,study_visit_time_cats,advanceremind_cats  (computed down below)

# format dates
# dat$int_consent_date=as.Date(dat$int_consent_date, "%Y-%m-%d")
# dat$interphone_date=as.Date(dat$interphone_date, "%Y-%m-%d")
# dat$encounter_date_int=as.Date(dat$encounter_date_int, "%Y-%m-%d")
# dat$biosample_collection_date=as.Date(dat$biosample_collection_date, "%Y-%m-%d")


# **************************************************************************** #
# RECODE DATA: continuous to categorical          
# **************************************************************************** #

# study_length_cats
#------------------
dat$study_length_cats[dat$int_guide_studylength<=6] <- "1"
dat$study_length_cats[dat$int_guide_studylength>=7 & dat$int_guide_studylength<=12] <- "2"
dat$study_length_cats[dat$int_guide_studylength>=13 & dat$int_guide_studylength<=24] <- "3"
dat$study_length_cats[dat$int_guide_studylength>25] <- "4"

# study_visit_cats
#------------
dat$study_visit_cats[dat$int_guide_transportation<=2] <- "1"
dat$study_visit_cats[dat$int_guide_transportation>=3 & dat$int_guide_transportation<=4] <- "2"
dat$study_visit_cats[dat$int_guide_transportation>5] <- "3"

# study_visit_time_cats
#------------------
dat$study_visit_time_cats[dat$int_guide_visitlength<=30] <- "1"
dat$study_visit_time_cats[dat$int_guide_visitlength>=31 & dat$int_guide_visitlength<=60] <- "2"
dat$study_visit_time_cats[dat$int_guide_visitlength>=61 & dat$int_guide_visitlength<=90] <- "3"
dat$study_visit_time_cats[dat$int_guide_visitlength>91] <- "4"

# advance reminder
# categories: categories: 1, 1-2 days | 2, 3-5 days | 3, 6-10 days | 4, 10+ days
#----------------
dat$advanceremind_cats[dat$int_guide_advanceremind<=2] <- "1"
dat$advanceremind_cats[dat$int_guide_advanceremind>=3 & dat$int_guide_advanceremind<=5] <- "2"
dat$advanceremind_cats[dat$int_guide_advanceremind>=6 & dat$int_guide_advanceremind<=10] <- "3"
dat$advanceremind_cats[dat$int_guide_advanceremind>11] <- "4"

# check 
#-------
names(dat)
str(dat)

# **************************************************************************** #
# RECODE DATA: character to categorical          
# **************************************************************************** #

# study group
# catagories: 1=pregnant, 2=breastfeeding
#----------------------------------------
dat$int_study_grp=as.factor(dat$int_study_grp)  


# **************************************************************************** #
#                              COMPUTE TABLE STATS          
# **************************************************************************** #

# rename data
dat.c=dat


# **************************************************************************** #
# STUDY LENGTH (months)          
# categories: 1, 0-6 months | 2, 7-12 months | 3, 13-24 months | 4, 24+ months
# **************************************************************************** #

# descriptives
#-------------
range(dat.c$int_guide_studylength, na.rm=T)  #2 60
round(mean(dat.c$int_guide_studylength, na.rm=T),1)  # 24.7
round(sd(dat.c$int_guide_studylength, na.rm=T),1)  # 18.9
# hist(dat.c$int_guide_studylength, na.rm=T)

# ALL
#----
dat.c %>% 
  select(study_length_cats) %>% 
  group_by(study_length_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Group
#---------
dat.c %>% 
  select(int_study_grp, study_length_cats) %>% 
  group_by(int_study_grp, study_length_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# chiq.test
study.test=chisq.test(dat.c$int_study_grp,dat.c$study_length_cats)
tidy(study.test)

# ****************************************************************** #
# VISIT NUMBER (count)         
# categories: 1, 1-2 visits | 2, 3-4 visits | 3, 4+ visits
# ****************************************************************** #

# descriptives
#-------------
range(dat.c$int_guide_transportation, na.rm=T)  #2 120
round(mean(dat.c$int_guide_transportation, na.rm=T),1)  # 17.9
round(sd(dat.c$int_guide_transportation, na.rm=T),1)  # 32
# hist(dat.c$int_guide_transportation, na.rm=T)

# ALL
#----
dat.c %>% 
  select(study_visit_cats) %>% 
  group_by(study_visit_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Group
#---------
dat.c %>% 
  select(int_study_grp, study_visit_cats) %>% 
  group_by(int_study_grp, study_visit_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# chiq.test
visit.test=chisq.test(dat.c$int_study_grp,dat.c$study_visit_cats)
tidy(visit.test)

# ****************************************************************** #
# VISIT LENGTH (minutes)          
# categories: 1, 0-30 min | 2, 31-60 min | 3, 61-90 min | 4, 90+ min
# ****************************************************************** #

# descriptives
#-------------
range(dat.c$int_guide_visitlength, na.rm=T)  # 30 240
round(mean(dat.c$int_guide_visitlength, na.rm=T),1)  # 81.2
round(sd(dat.c$int_guide_visitlength, na.rm=T),1)  # 44.2
# hist(dat.c$int_guide_visitlength, na.rm=T)

# ALL
#----
dat.c %>% 
  select(study_visit_time_cats) %>% 
  group_by(study_visit_time_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Group
#---------
dat.c %>% 
  select(int_study_grp, study_visit_time_cats) %>% 
  group_by(int_study_grp, study_visit_time_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# chiq.test
visit_time.test=chisq.test(dat.c$int_study_grp,dat.c$study_visit_time_cats)
tidy(visit_time.test)

# ****************************************************************** #
# VISIT TIME OF DAY          
# categories: 1, morning | 2, lunchtime | 3, afternoon | 4, evening
# ****************************************************************** #

# ALL
#----
dat.c %>% 
  select(int_guide_timeofday) %>% 
  group_by(int_guide_timeofday) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Group
#---------
dat.c %>% 
  select(int_study_grp, int_guide_timeofday) %>% 
  group_by(int_study_grp, int_guide_timeofday) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# chiq.test
visit_time_day.test=chisq.test(dat.c$int_study_grp,dat.c$int_guide_timeofday)
tidy(visit_time_day.test)

# ****************************************************************** #
# STUDY REMINDERS                * t-test() may not be appropriate?
# categories: 1, yes | 0, no
# ****************************************************************** #

# ALL
#----
dat.c %>% 
  select(int_guide_reminders) %>% 
  group_by(int_guide_reminders) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Group
#---------
dat.c %>% 
  select(int_study_grp, int_guide_reminders) %>% 
  group_by(int_study_grp, int_guide_reminders) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# chiq.test
visit_reminder.test=t.test(dat.c$int_study_grp,dat.c$int_guide_reminders)
tidy(visit_reminder.test)

# ****************************************************************** #
# PREFERED CONTACT         
# categories: 1, text message | 2, phone call | 3, email | 4, by mail
# ****************************************************************** #

# data wrangle
#---------------
dat1=dat.c %>%
  select(record_id,int_study_grp,int_guide_contactby___1,int_guide_contactby___2,int_guide_contactby___3,int_guide_contactby___4) %>%
  na_if(0) %>% 
  gather(key = "contact", value = "value", na.rm = TRUE,
         int_guide_contactby___1, int_guide_contactby___2, int_guide_contactby___3, int_guide_contactby___4) %>%
  mutate(contact_pref=recode(contact, int_guide_contactby___1="text",
                             int_guide_contactby___2="phone",
                             int_guide_contactby___3="email",
                             int_guide_contactby___4="mail")) %>%
  select(record_id, int_study_grp, contact_pref, everything()) %>%
  select(-contact)

# ALL
#----
dat1 %>% 
  select(contact_pref) %>% 
  group_by(contact_pref) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Group
#---------
dat1 %>% 
  select(int_study_grp, contact_pref) %>% 
  group_by(int_study_grp, contact_pref) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# chiq.test
pref_contact.test=chisq.test(dat1$int_study_grp,dat1$contact_pref)
tidy(pref_contact.test)

# ****************************************************************** #
# ADVANCE REMINDERS (days)         
# categories: categories: 1, 1-2 days | 2, 3-5 days | 3, 6-10 days | 4, 10+ days
# ****************************************************************** #

# descriptives
# ------------
range(dat.c$int_guide_advanceremind, na.rm=T)  # 1 30
round(mean(dat.c$int_guide_advanceremind, na.rm=T),1)  # 7
round(sd(dat.c$int_guide_advanceremind, na.rm=T),1)  # 6.8
# hist(dat.c$int_guide_advanceremind, na.rm=T)

# ALL
#----
dat.c %>% 
  select(advanceremind_cats) %>% 
  group_by(advanceremind_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Group
#---------
dat.c %>% 
  select(int_study_grp, advanceremind_cats) %>% 
  group_by(int_study_grp, advanceremind_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# chiq.test
adv_reminder.test=chisq.test(dat.c$int_study_grp,dat.c$advanceremind_cats)
tidy(adv_reminder.test)
