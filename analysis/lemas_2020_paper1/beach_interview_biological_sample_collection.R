##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:             Xinsong Du 
# START  Date:        Apr 08, 2020
# Project:            BEACH Interview
# Description:        Data Analysis for Paper #1- Table 04
# Note:               Revisions suggested by BMC Pregnancy and Childbirth
#                     -break out data by study group and include biological samples
# Code Reference:     Part of code refers to "beach_interview_table01.Rmd" written by Dominick Lemas

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
# # https://cran.r-project.org/web/packages/keyringr/vignettes/Avoiding_plain_text_passwords_in_R_with_keyringr.html

# keyringr: Avoiding plain text passwords
uri <- "https://redcap.ctsi.ufl.edu/redcap/api/"

# load token for Windows user:
credential_label <- "interview_api" # Modify this to the label in your own computer
credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
interview_token<-decrypt_dpapi_pw(credential_path)

# load token for Mac user
credential_label <- "REDCap_BEACH_Interview" # Modify this to the label in your own computer
interview_token<-decrypt_kc_pw(credential_label)

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
                       "biosample_mom_baby", "biosample_aliquot_type",
                       "crc_specimen_barcode","biosample_tube_type","biosample_aliquot_numb",
                       "biological_specimen_collection_complete",
                       "analysis_mat_age", "analysis_mat_age_cats", "analysis_mat_age_source", "analysis_bmi",
                       "analysis_bmi_cats", "analysis_bmi_source", "mom3t_education_2", "analysis_research_expr",
                       "analysis_kids_previous", "analysis_time_of_day","int_guide_stoolcollect","analysis_income")

# pull data-V1
interview <- redcap_read(
  batch_size=150L,
  redcap_uri = uri, 
  token      = interview_token
#  fields     = desired_fields_v1
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
# income_cats
# categories: 1, 0 to $15,000 | 2, $15,001 to $19,000 | 3, $19,001 to $37,000  
# 4, $37,001 to $44,000 | 5, $44,001 to $52,000 | 6, $52,001 to $56,000 |   
# 7, $56,001 to $67,000 | 8, $67,001 to $79,000 | 9, $79,001 or more | 10, missing
dat.1=dat%>%mutate(analysis_income_cats=recode(analysis_income,"1"="1","2"="1","3"="1",  # 1, 0-37K 
                                   "4"="2","5"="2","6"="2",                              # 2, 37K-79K
                                   "7"="2","8"="2","9"="3",                              # 3, 79K or more
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
         analysis_research_expr,analysis_kids_previous,int_guide_stoolcollect,analysis_income,analysis_income_cats,
         biosample_mom_baby,biosample_aliquot_type,
         crc_specimen_barcode,biosample_tube_type,biosample_aliquot_numb, biological_specimen_collection_complete) %>%
  as_tibble()

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
# BIOLOGICAL SAMPLE COLLECTION                      
# categories: 1, yes | 0, no
# **************************************************************************** #

samples=dat.1 %>%
  select(record_id,int_study_grp,biosample_mom_baby,biosample_aliquot_type,
         crc_specimen_barcode,biosample_tube_type,biosample_aliquot_numb, biological_specimen_collection_complete, 
         analysis_research_expr, int_guide_stoolcollect) %>%
  group_by(record_id) %>%
  mutate(study_grp=first(int_study_grp)) %>%
  filter(is.na(biological_specimen_collection_complete)==F) %>%
  select(-int_study_grp,-biosample_mom_baby,-crc_specimen_barcode,-biological_specimen_collection_complete,-biosample_tube_type) 

# facor
samples$biosample_aliquot_type=as.factor(samples$biosample_aliquot_type)

# remove rows with missing "biosample_aliquot_type" value
samples <- drop_na(samples, biosample_aliquot_type)

# number of moms that have donations
length(unique(samples$record_id))

# add column indicating if a mom donated all samples (urine, saliva, milk-whole, stool, vaginal)
samples <- samples %>%
              group_by(record_id) %>%
              mutate(donate_num = length(unique(biosample_aliquot_type)))
# ifelse((startsWith(record_id, "BIS") & length(unique(biosample_aliquot_type)==5)) | (startsWith(record_id, "PRG") & length(unique(biosample_aliquot_type)==4)), TRUE, FALSE))
# (2 %in% biosample_aliquot_type) & (3 %in% biosample_aliquot_type) & (7 %in% biosample_aliquot_type) & (8 %in% biosample_aliquot_type))
samples <- samples %>%
  group_by(record_id) %>%
  mutate(donate_all = (startsWith(record_id, "BIS") & length(unique(biosample_aliquot_type))==5) | (startsWith(record_id, "PRG") & length(unique(biosample_aliquot_type))==4))

# number of moms that donated all samples
moms_donate_all <- samples[samples$donate_all==TRUE,]
length(unique(moms_donate_all$record_id))
