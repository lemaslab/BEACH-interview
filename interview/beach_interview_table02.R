##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        June 25, 2019 
# Project:     BEACH Interview
# Description: Data Analysis for Paper #1- Table 02


# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(keyringr)
library(tidyverse)
library(redcapAPI)
library(REDCapR)
library(broom)
library(dplyr)

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
desired_fields_v1 <- c("record_id","int_study_grp","interphone_date","int_consent_date","int_consent_complete",
                       "beach_interview_phone_screen_complete","int_phone_pass_fail",
                       "int_interview_date","int_interview_complete","int_audio_length_min", # study 
                       "interphone_prepreg_bmi","interphone_age","mom3t_prepreg_bmi",
                       "mompa_walk_slow","mompa_walk_quick", "mompa_walk_hills", "mompa_jog", "mompa_prenatal_exer", "mompa_swim","mompa_dance", # physical activity
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
                       "int_guide_reminders","int_guide_contactby","int_guide_advanceremind")

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

# rename data
dat=interview
names(dat)
str(dat)

# format dates
dat$int_consent_date=as.Date(dat$int_consent_date, "%Y-%m-%d")
dat$interphone_date=as.Date(dat$interphone_date, "%Y-%m-%d")
dat$encounter_date_int=as.Date(dat$encounter_date_int, "%Y-%m-%d")
dat$biosample_collection_date=as.Date(dat$biosample_collection_date, "%Y-%m-%d")

# how many have completed the interview
complete=dat %>%
  filter(int_interview_complete==1)%>%
  pull(record_id)
length(complete) # 40

# limit data to include only interview participants (n=40)
dat.c=dat %>%
  filter(int_interview_complete==1)

# table 2. 
names(dat.c)
str(dat.c)
# convert group variable to factor (1=pregnant, 2=breastfeeding)
dat.c$int_study_grp=as.factor(dat.c$int_study_grp)

# range of study length (months)
range(dat.c$int_guide_studylength, na.rm=T)  #2 60
round(mean(dat.c$int_guide_studylength, na.rm=T),1)  # 24.7
round(sd(dat.c$int_guide_studylength, na.rm=T),1)  # 18.9
hist(dat.c$int_guide_studylength, na.rm=T)

# range of visit number (count)
range(dat.c$int_guide_transportation, na.rm=T)  #2 120
round(mean(dat.c$int_guide_transportation, na.rm=T),1)  # 17.9
round(sd(dat.c$int_guide_transportation, na.rm=T),1)  # 18.9
hist(dat.c$int_guide_transportation, na.rm=T)

# range of study visit length (minutes)
range(dat.c$int_guide_visitlength, na.rm=T)  # 30 240
round(mean(dat.c$int_guide_visitlength, na.rm=T),1)  # 81.2
round(sd(dat.c$int_guide_visitlength, na.rm=T),1)  # 18.9
hist(dat.c$int_guide_visitlength, na.rm=T)

# distribution of stuy visit time of day categories 
# (1, morning | 2, lunchtime | 3, afternoon)
names(dat.c)
dat.c %>% 
  select(record_id,analysis_time_of_day___1,analysis_time_of_day___2,analysis_time_of_day___3) %>% 
  rename(morning=analysis_time_of_day___1, lunchtime=analysis_time_of_day___2,afternoon=analysis_time_of_day___3) %>%
  gather(time_of_day, value, morning:afternoon) %>%
  group_by(time_of_day) %>%
  summarise(count = sum(value)) %>%
  mutate(prop = prop.table(count))

# distribution of study reminders 
# 1, yes | 0, no
dat.c %>% 
  select(int_guide_reminders) %>% 
  group_by(int_guide_reminders) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# distribution of prefered contact method 
# (1, text message | 2, phone call | 3, email | 4, by mail)
names(dat.c)
dat.c %>% 
  select(record_id,int_guide_contactby___1,int_guide_contactby___2,int_guide_contactby___3,int_guide_contactby___4) %>% 
  rename(text_message=int_guide_contactby___1, phone_call=int_guide_contactby___2,email=int_guide_contactby___3, mail=int_guide_contactby___4) %>%
  gather(method, value, text_message:mail) %>%
  group_by(method) %>%
  summarise(count = sum(value)) %>%
  mutate(prop = prop.table(count))

# range of advanced reminders (days)
range(dat.c$int_guide_advanceremind, na.rm=T)  # 1 30
round(mean(dat.c$int_guide_advanceremind, na.rm=T),1)  # 7
round(sd(dat.c$int_guide_advanceremind, na.rm=T),1)  # 6.8
hist(dat.c$int_guide_advanceremind, na.rm=T)


names(dat.c)
