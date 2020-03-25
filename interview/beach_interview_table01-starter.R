##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        May 07, 2019 
# Project:     BEACH Interview
# Description: Data and Analysis for Table 01


# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(keyringr)
library(tidyverse)
library(redcapAPI)
library(REDCapR)
library(broom)

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
                       "analysis_kids_previous", "analysis_time_of_day")

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

# table 1. 
# =======
# this data includes ALL observations
# all encounters (n=103)
# consented (n=X)
# consented and completed (n=40)

# limit data to include only interview participants (n=40)
dat.c=dat %>%
  filter(int_interview_complete==1)

names(dat.c)  
length(dat.c) # 40

# table 1. 
names(dat.c)
str(dat.c)
# convert group variable to factor (1=pregnant, 2=breastfeeding)
dat.c$int_study_grp=as.factor(dat.c$int_study_grp)
dat.c$int_audio_length_min
length((dat.c$int_audio_length_min))

# what is statistical difference between continuous outcomes? 
# Approach 1
dat.c %>% 
  select(int_audio_length_min,interphone_prepreg_bmi,interphone_age, int_study_grp) %>% 
  gather(key = variable, value = value, -int_study_grp) %>% 
  group_by(int_study_grp, variable) %>% 
  summarise(value = list(value)) %>% 
  spread(int_study_grp, value) %>% 
  group_by(variable) %>% 
  mutate(mean_01=t.test(unlist(`1`), unlist(`2`))$estimate[1],
         sd_01=sd(unlist(`1`)),
         mean_02=t.test(unlist(`1`), unlist(`2`))$estimate[2],
         sd_02=sd(unlist(`2`)),
         p_value = t.test(unlist(`1`), unlist(`2`))$p.value,
         t_value = t.test(unlist(`1`), unlist(`2`))$statistic)

# Breastfeeding Group: 
# mean/sd for breastfeeding group
dat.c%>%
  filter(int_study_grp=="2")%>%
  summarize(bmi_mean=mean(interphone_prepreg_bmi, na.rm = TRUE),
            bmi_sd=sd(interphone_prepreg_bmi, na.rm = TRUE),
            age_mean=mean(interphone_age, na.rm = TRUE),
            age_sd=sd(interphone_age, na.rm = TRUE),
            audio_mean=mean(int_audio_length_min, na.rm = TRUE),
            audio_sd=sd(int_audio_length_min, na.rm = TRUE))

# All
dat.c%>%
  summarize(bmi_mean=mean(interphone_prepreg_bmi, na.rm = TRUE),
            bmi_sd=sd(interphone_prepreg_bmi, na.rm = TRUE),
            age_mean=mean(interphone_age, na.rm = TRUE),
            age_sd=sd(interphone_age, na.rm = TRUE),
            audio_mean=mean(int_audio_length_min, na.rm = TRUE),
            audio_sd=sd(int_audio_length_min, na.rm = TRUE))


# Approach 2
dat.c %>%
  group_by(int_study_grp)%>%
  summarize(mean_audio=mean(int_audio_length_min, na.rm = TRUE))
  t.test(int_audio_length_min ~ int_study_grp, data = dat)%>%
    tidy()

# https://stats.stackexchange.com/questions/168378/applying-two-sample-t-test-comparing-multiple-groups-in-two-categories
# https://sebastiansauer.github.io/multiple-t-tests-with-dplyr/
# https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html 
