##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 22, 2019 
# IRB:
# Description: RedCap API system for BEACH Interview project

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #

library(keyringr)
library(tidyverse)
library(redcapAPI)
library(REDCapR)

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

# list of events
exportEvents(rcon)

# list records
exportRecords(rcon)

# export field names
exportFieldNames(rcon)

# Variables
desired_fields_v1 <- c("record_id","int_study_grp","interphone_date","int_consent_date","int_consent_complete",
                       "int_interview_date","int_interview_complete","int_audio_length_min", # study 
                       "interphone_prepreg_bmi","interphone_age","mom3t_prepreg_bmi",
                       "mompa_walk_slow","mompa_walk_quick", "mompa_walk_hills", "mompa_jog", "mompa_prenatal_exer", "mompa_swim","mompa_dance", # physical activity
                       "int_guide_education","int_guide_employmnt","int_guide_occupation"
                       )

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

# how many consented
table(dat$int_consent_complete) # 47 

# how many have not completed interview
table(dat$int_interview_complete) # 7 no and # 40 yes
which(dat$int_interview_complete==0)  
# 83 (BIS002A)  
# 85 (BIS004A)
# 86 (BIS005A) 
# 98 (BIS023A)
# 100 (BIS025A)
# 107 (BIS032A)
# 109 (BIS034A)

dat %>%
  filter(int_interview_complete==0)  

# how many have completed the interview
complete=dat %>%
  filter(int_interview_complete==1) # PRG010 need to follow-up with interview
complete$record_id

# what is difference in interview time? p=0.6
dat %>%
  group_by(int_study_grp)%>%
  summarize(mean_audio=mean(int_audio_length_min, na.rm = TRUE))
  t.test(int_audio_length_min ~ int_study_grp, data = dat)
  
  # Welch Two Sample t-test
  # data:  int_audio_length_min by int_study_grp
  # t = -0.48569, df = 34.533, p-value = 0.6303
  # alternative hypothesis: true difference in means is not equal to 0
  # 95 percent confidence interval:
  #   -10.022766   6.154345
  # sample estimates:
  #   mean in group 1 mean in group 2 
  # 45.31579        47.25000 


