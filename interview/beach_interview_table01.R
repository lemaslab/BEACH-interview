##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:             Dominick Lemas 
# START  Date:        May 07, 2019 
# REVISE Date:        Mar 25, 2020
# Project:            BEACH Interview
# Description:        Data Analysis for Paper #1- Table 01
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
                       "analysis_kids_previous", "analysis_time_of_day","int_guide_stoolcollect","analysis_income")

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

# rename data
dat=interview
names(dat)
str(dat)

# format dates
dat$int_consent_date=as.Date(dat$int_consent_date, "%Y-%m-%d")
dat$interphone_date=as.Date(dat$interphone_date, "%Y-%m-%d")
dat$encounter_date_int=as.Date(dat$encounter_date_int, "%Y-%m-%d")
dat$biosample_collection_date=as.Date(dat$biosample_collection_date, "%Y-%m-%d")

# recode
dat.1=dat%>%mutate(analysis_income_cats=recode(analysis_income,"1"="0","2"="0","3"="0",  # combine 0-37K 
                                   "4"="2","5"="2","6"="2",
                                   "7"="2","8"="2","9"="3",
                                   "10"="NA")) 

# how many have completed the interview
complete=dat.1 %>%
  filter(int_interview_complete==1)%>%
  pull(record_id)
length(complete) # 40

# limit data to include only interview participants (n=40)
# and variables for table 01. 
dat.c=dat.1 %>%
  filter(int_interview_complete==1)%>%
  select(record_id,redcap_repeat_instrument,redcap_repeat_instance,int_study_grp,
         int_audio_length_min,analysis_mat_age_cats,analysis_bmi_cats,mom3t_education_2,
         analysis_research_expr,analysis_kids_previous,int_guide_stoolcollect,analysis_income,analysis_income_cats)

# check 
names(dat.c)
str(dat.c)

# Create factor variable 
#-----------------------
dat.c$int_study_grp=as.factor(dat.c$int_study_grp)   # study_group: (1=pregnant, 2=breastfeeding)

# range of interview audio
range(dat.c$int_audio_length_min)  #24 81
mean(dat.c$int_audio_length_min)  # 46.65
sd(dat.c$int_audio_length_min)  # 12.43764

# **************************************************************************** #
# MATERNAL AGE                      
# categories: 1, <20 | 2, 20-30 | 3, 31-40 | 4, >40
# **************************************************************************** #

# ALL
#----
dat.c %>% 
  select(analysis_mat_age_cats) %>% 
  group_by(analysis_mat_age_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Group
#---------
dat.c %>% 
  select(int_study_grp, analysis_mat_age_cats) %>% 
  group_by(int_study_grp, analysis_mat_age_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# chiq.test
age.test=chisq.test(dat.c$int_study_grp,dat.c$analysis_mat_age_cats)
tidy(age.test)

# **************************************************************************** #
# PREVIOUS CHILDREN                      
# categories: 1, yes | 0, no
# **************************************************************************** #

# ALL
#----
dat.c %>% 
  select(int_study_grp,analysis_kids_previous) %>% 
  group_by(analysis_kids_previous) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Group
#---------
dat.c %>% 
  select(int_study_grp, analysis_kids_previous) %>% 
  group_by(int_study_grp, analysis_kids_previous) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# chiq.test
kid.test=chisq.test(dat.c$int_study_grp,dat.c$analysis_kids_previous)
tidy(kid.test)

# **************************************************************************** #
# MATERNAL EDUCATION                      
# categories: 1, <8th grade | 2, Some high school | 3, High school diploma/GED 
# 4, Some college or community college | 5, Associates degree | 
# 6, Completed tech or vocational school | 7, College graduate | 
# 8, Some graduate or professional school | 9, Graduate/professional degree
# **************************************************************************** #

# ALL
#----
dat.c %>% 
  select(int_study_grp,mom3t_education_2) %>% 
  group_by(mom3t_education_2) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Group
#---------
dat.c %>% 
  select(int_study_grp, mom3t_education_2) %>% 
  group_by(int_study_grp, mom3t_education_2) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# chiq.test
edu.test=chisq.test(dat.c$int_study_grp,dat.c$mom3t_education_2)
tidy(edu.test)

# **************************************************************************** #
# HOUSEHOLD INCOME                      
# categories: 1, 0 to $15,000 | 2, $15,001 to $19,000 | 3, $19,001 to $37,000  
# 4, $37,001 to $44,000 | 5, $44,001 to $52,000 | 6, $52,001 to $56,000 |   
# 7, $56,001 to $67,000 | 8, $67,001 to $79,000 | 9, $79,001 or more | 10, missing
# **************************************************************************** #

# ALL
#----
dat.c %>% 
  select(int_study_grp,analysis_income) %>% 
  mutate(analysis_income_cats=recode(analysis_income,"1"="0","2"="0","3"="0",  # combine 0-37K 
                                                     "4"="2","5"="2","6"="2",
                                                     "7"="2","8"="2","9"="3",
                                                     "10"="NA")) %>%
  group_by(analysis_income_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

# Group
#---------
dat.c %>% 
  select(int_study_grp, analysis_income_cats) %>% 
  group_by(int_study_grp, analysis_income_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))




# distribution of maternal bmi categories 
# (1, <25 | 2, 25-30 | 3, >30)
range(dat.c$analysis_bmi) # 18.5 48.1
dat.c %>% 
  select(analysis_bmi_cats) %>% 
  group_by(analysis_bmi_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))



# distribution of previous research experience 
# 1, yes | 0, no
dat.c %>% 
  select(analysis_research_expr) %>% 
  group_by(analysis_research_expr) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))





# distribution of previous stool collection "int_guide_stoolcollect" 
# 1, yes | 0, no
dat.c %>% 
  select(int_guide_stoolcollect) %>% 
  group_by(int_guide_stoolcollect) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))




names(dat.c)
