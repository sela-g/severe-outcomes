#### CANCOVID 2.0 ####
# Gal Av-Gay #
# 20240325: Sandra Blitz #
# 08Apr2024: Sela Grays #

library(Gmisc)
library(broman)
library(dplyr)
library(DescTools)
library(tidyverse)
library(sjPlot)
library(here)
library(janitor)
library(zoo)
library(lubridate)
library(haven)
library(ggpubr)
library(corrplot)
library(ghibli)
library(fmsb)
library(epiR)
library(fmsb)
library(epitools)

#### LOAD ALL DATA ####

data_bc <- read.csv("BC_qiqa_cancovid_nov20.csv", header = TRUE)
data_ns <- read.csv("NS_COVID19InPreg_DATA_2023-04-18_1432.csv", header = TRUE)
data_on <- read_sas("ON_covid_preg_infants_sep3022.sas7bdat")
data_other <- read.csv("OTHER_rc_cancovid_nov21.csv", header = TRUE)

## quebec is complicated...##
data_qc <- read.csv("COVIPREGQV4_DATA_2024-02-20_1948.csv", header = TRUE)
length(unique(data_qc$a_record_id))


unique(data_qc$redcap_event_name)

data_qc[data_qc == ""] <- NA
data_qc_1 <- data_qc %>% filter(redcap_event_name == "demographics_arm_1")
data_qc_2 <- data_qc %>% filter(redcap_event_name == "sarscov2_arm_1")
data_qc_3 <- data_qc %>% filter(redcap_event_name == "antepartum_arm_1")
data_qc_4 <- data_qc %>% filter(redcap_event_name == "intrapartum_arm_1")
data_qc_5 <- data_qc %>% filter(redcap_event_name == "other_forms_arm_1")
data_qc_6 <- data_qc %>% filter(redcap_event_name == "postpartum_arm_1")

data_qc_1 <- remove_empty(data_qc_1, which = "cols")
data_qc_2 <- remove_empty(data_qc_2, which = "cols")
data_qc_3 <- remove_empty(data_qc_3, which = "cols")
data_qc_4 <- remove_empty(data_qc_4, which = "cols")
data_qc_5 <- remove_empty(data_qc_5, which = "cols")
data_qc_6 <- remove_empty(data_qc_6, which = "cols")

data_qc <- left_join(data_qc_1,data_qc_2, by = "a_record_id")
data_qc <- left_join(data_qc,data_qc_3, by = "a_record_id")
data_qc <- left_join(data_qc,data_qc_4, by = "a_record_id")
data_qc <- left_join(data_qc,data_qc_5, by = "a_record_id")
data_qc <- left_join(data_qc,data_qc_6, by = "a_record_id")

## data other split up

# unique(data_other$redcap_data_access_group)
data_mb <- data_other %>% filter(redcap_data_access_group == "mb")
data_nb <- data_other %>% filter(redcap_data_access_group == "nb")
data_pe <- data_other %>% filter(redcap_data_access_group == "pe")
data_yt <- data_other %>% filter(redcap_data_access_group == "yt")


#### FIX DATA (combine record_ids into single row) ####

## BC 
# length(unique(data_bc$a_record_id))
# unique(data_bc$redcap_event_name)

data_bc[data_bc == ""] <- NA
data_bc_1 <- data_bc %>% filter(redcap_event_name == "demographics_arm_1")
data_bc_2 <- data_bc %>% filter(redcap_event_name == "sarscov2_arm_1")
data_bc_3 <- data_bc %>% filter(redcap_event_name == "antepartum_arm_1")
data_bc_4 <- data_bc %>% filter(redcap_event_name == "intrapartum_arm_1")
data_bc_5 <- data_bc %>% filter(redcap_event_name == "other_forms_arm_1")
data_bc_6 <- data_bc %>% filter(redcap_event_name == "postpartum_arm_1")

data_bc_1 <- remove_empty(data_bc_1, which = "cols")
data_bc_2 <- remove_empty(data_bc_2, which = "cols")
data_bc_3 <- remove_empty(data_bc_3, which = "cols")
data_bc_4 <- remove_empty(data_bc_4, which = "cols")
data_bc_5 <- remove_empty(data_bc_5, which = "cols")
data_bc_6 <- remove_empty(data_bc_6, which = "cols")

data_bc <- left_join(data_bc_1,data_bc_2, by = "a_record_id")
data_bc <- left_join(data_bc,data_bc_3, by = "a_record_id")
data_bc <- left_join(data_bc,data_bc_4, by = "a_record_id")
data_bc <- left_join(data_bc,data_bc_5, by = "a_record_id")
data_bc <- left_join(data_bc,data_bc_6, by = "a_record_id")

# dim(data_bc)

## Nova Scotia (already combined)

# length(data_ns$a_record_id)
# length(unique(data_ns$a_record_id))

# data_ns$r_dob

# colnames(data_ns)

## Ontario (weird one)

# length(unique(data_on$new_preg_id)) ## looks correct to me, pre merged
# 
# data_on$C_COVID_diagnosis_date
# data_on$baby_birth_date
# sum(is.na(data_on$delivery_date))
# 
# tail(data_on$baby_birth_date)


## qc
# length(data_qc$a_record_id)
# length(unique(data_qc$a_record_id)) ## already squished it seems

# data_qc$r_dob
## Manitoba
# length(data_mb$a_record_id)
# length(unique(data_mb$a_record_id)) 
# 
# unique(data_mb$redcap_event_name)

data_mb[data_mb == ""] <- NA
data_mb_1 <- data_mb %>% filter(redcap_event_name == "demographics_arm_1")
data_mb_2 <- data_mb %>% filter(redcap_event_name == "sarscov2_arm_1")
data_mb_3 <- data_mb %>% filter(redcap_event_name == "antepartum_arm_1")
data_mb_4 <- data_mb %>% filter(redcap_event_name == "intrapartum_arm_1")
data_mb_5 <- data_mb %>% filter(redcap_event_name == "other_forms_arm_1")
data_mb_6 <- data_mb %>% filter(redcap_event_name == "postpartum_arm_1")

data_mb_1 <- remove_empty(data_mb_1, which = "cols")
data_mb_2 <- remove_empty(data_mb_2, which = "cols")
data_mb_3 <- remove_empty(data_mb_3, which = "cols")
data_mb_4 <- remove_empty(data_mb_4, which = "cols")
data_mb_5 <- remove_empty(data_mb_5, which = "cols")
data_mb_6 <- remove_empty(data_mb_6, which = "cols")

data_mb <- left_join(data_mb_1,data_mb_2, by = "a_record_id")
data_mb <- left_join(data_mb,data_mb_3, by = "a_record_id")
data_mb <- left_join(data_mb,data_mb_4, by = "a_record_id")
data_mb <- left_join(data_mb,data_mb_5, by = "a_record_id")
data_mb <- left_join(data_mb,data_mb_6, by = "a_record_id")

# data_mb$r_dob


## new brunswick
# length(data_nb$a_record_id)
# length(unique(data_nb$a_record_id)) 
# 
# unique(data_nb$redcap_event_name)

data_nb[data_nb == ""] <- NA
data_nb_1 <- data_nb %>% filter(redcap_event_name == "demographics_arm_1")
data_nb_2 <- data_nb %>% filter(redcap_event_name == "sarscov2_arm_1")
data_nb_3 <- data_nb %>% filter(redcap_event_name == "antepartum_arm_1")
data_nb_4 <- data_nb %>% filter(redcap_event_name == "intrapartum_arm_1")
data_nb_5 <- data_nb %>% filter(redcap_event_name == "other_forms_arm_1")
data_nb_6 <- data_nb %>% filter(redcap_event_name == "postpartum_arm_1")

data_nb_1 <- remove_empty(data_nb_1, which = "cols")
data_nb_2 <- remove_empty(data_nb_2, which = "cols")
data_nb_3 <- remove_empty(data_nb_3, which = "cols")
data_nb_4 <- remove_empty(data_nb_4, which = "cols")
data_nb_5 <- remove_empty(data_nb_5, which = "cols")
data_nb_6 <- remove_empty(data_nb_6, which = "cols")

data_nb <- left_join(data_nb_1,data_nb_2, by = "a_record_id")
data_nb <- left_join(data_nb,data_nb_3, by = "a_record_id")
data_nb <- left_join(data_nb,data_nb_4, by = "a_record_id")
data_nb <- left_join(data_nb,data_nb_5, by = "a_record_id")
data_nb <- left_join(data_nb,data_nb_6, by = "a_record_id")


# data_nb$r_dob

# ## pei
# length(data_pe$a_record_id)
# length(unique(data_pe$a_record_id)) 
# 
# unique(data_pe$redcap_event_name)

data_pe[data_pe == ""] <- NA
data_pe_1 <- data_pe %>% filter(redcap_event_name == "demographics_arm_1")
data_pe_2 <- data_pe %>% filter(redcap_event_name == "sarscov2_arm_1")
data_pe_3 <- data_pe %>% filter(redcap_event_name == "antepartum_arm_1")
data_pe_4 <- data_pe %>% filter(redcap_event_name == "intrapartum_arm_1")
data_pe_5 <- data_pe %>% filter(redcap_event_name == "other_forms_arm_1")
data_pe_6 <- data_pe %>% filter(redcap_event_name == "postpartum_arm_1")

data_pe_1 <- remove_empty(data_pe_1, which = "cols")
data_pe_2 <- remove_empty(data_pe_2, which = "cols")
data_pe_3 <- remove_empty(data_pe_3, which = "cols")
data_pe_4 <- remove_empty(data_pe_4, which = "cols")
data_pe_5 <- remove_empty(data_pe_5, which = "cols")
data_pe_6 <- remove_empty(data_pe_6, which = "cols")

data_pe <- left_join(data_pe_1,data_pe_2, by = "a_record_id")
data_pe <- left_join(data_pe,data_pe_3, by = "a_record_id")
data_pe <- left_join(data_pe,data_pe_4, by = "a_record_id")
data_pe <- left_join(data_pe,data_pe_5, by = "a_record_id")
data_pe <- left_join(data_pe,data_pe_6, by = "a_record_id")

## yukon
# length(data_yt$a_record_id)
# length(unique(data_yt$a_record_id)) 
# 
# unique(data_yt$redcap_event_name)

data_yt[data_yt == ""] <- NA
data_yt_1 <- data_yt %>% filter(redcap_event_name == "demographics_arm_1")
data_yt_2 <- data_yt %>% filter(redcap_event_name == "sarscov2_arm_1")
data_yt_3 <- data_yt %>% filter(redcap_event_name == "antepartum_arm_1")
data_yt_4 <- data_yt %>% filter(redcap_event_name == "intrapartum_arm_1")
data_yt_5 <- data_yt %>% filter(redcap_event_name == "other_forms_arm_1")
data_yt_6 <- data_yt %>% filter(redcap_event_name == "postpartum_arm_1")

data_yt_1 <- remove_empty(data_yt_1, which = "cols")
data_yt_2 <- remove_empty(data_yt_2, which = "cols")
data_yt_3 <- remove_empty(data_yt_3, which = "cols")
data_yt_4 <- remove_empty(data_yt_4, which = "cols")
data_yt_5 <- remove_empty(data_yt_5, which = "cols")
data_yt_6 <- remove_empty(data_yt_6, which = "cols")

data_yt <- left_join(data_yt_1,data_yt_2, by = "a_record_id")
data_yt <- left_join(data_yt,data_yt_3, by = "a_record_id")
data_yt <- left_join(data_yt,data_yt_4, by = "a_record_id")
data_yt <- left_join(data_yt,data_yt_5, by = "a_record_id")
data_yt <- left_join(data_yt,data_yt_6, by = "a_record_id")


#### FIX INCORRECT DATA ####
# DO THIS LATER? #
data_bc$time_del <- as.numeric(as.Date(data_bc$r_dob) - as.Date(data_bc$e_diagnosis))
data_ns$time_del <- as.numeric(as.Date(data_ns$r_dob) - as.Date(data_ns$e_diagnosis))
data_on$time_del <- as.numeric(as.Date(data_on$baby_birth_date) - as.Date(data_on$C_COVID_diagnosis_date))
data_qc$time_del <- as.numeric(as.Date(data_qc$r_dob) - as.Date(data_qc$e_diagnosis))
data_mb$time_del <- as.numeric(as.Date(data_mb$r_dob) - as.Date(data_mb$e_diagnosis))
data_nb$time_del <- as.numeric(as.Date(data_nb$r_dob) - as.Date(data_nb$e_diagnosis))
data_pe$time_del <- as.numeric(as.Date(data_pe$r_dob) - as.Date(data_pe$e_diagnosis))
data_yt$time_del <- as.numeric(as.Date(data_yt$r_dob) - as.Date(data_yt$e_diagnosis))

data_bc$prov <- "BC"
data_ns$prov <- "NS"
data_on$prov <- "ON"
data_qc$prov <- "QC"
data_mb$prov <- "MB"
data_nb$prov <- "NB"
data_pe$prov <- "PE"
data_yt$prov <- "YT"

# View(data_bc[which(data_bc$time_del > 280), c("a_record_id", "e_diagnosis", "d_naso1_collect", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])
# View(data_bc[which(data_bc$time_del < 0), c("a_record_id", "e_diagnosis", "d_naso1_collect", "i_lmp", "i_deliverydate_est", "r_dob", "time_del", "r_discharge", "p_outcome")])

## combine non-ontario data

colnames(data_bc)[which(!(colnames(data_bc) %in% colnames(data_mb)))]
sum((colnames(data_bc) %in% colnames(data_qc)))

data_bc <- data_bc %>%
  mutate(across(everything(), as.character))

data_ns <- data_ns %>%
  mutate(across(everything(), as.character))

data_qc <- data_qc %>%
  mutate(across(everything(), as.character))

data_mb <- data_mb %>%
  mutate(across(everything(), as.character))

data_nb <- data_nb %>%
  mutate(across(everything(), as.character))

data_pe <- data_pe %>%
  mutate(across(everything(), as.character))

data_yt <- data_yt %>%
  mutate(across(everything(), as.character))



full_data_sansON <- bind_rows(data_bc,data_ns)
full_data_sansON <- bind_rows(full_data_sansON,data_qc)
full_data_sansON <- bind_rows(full_data_sansON,data_mb)
full_data_sansON <- bind_rows(full_data_sansON,data_nb)
full_data_sansON <- bind_rows(full_data_sansON,data_pe)
full_data_sansON <- bind_rows(full_data_sansON,data_yt)

full_data_sansON[full_data_sansON == ""] <- NA
## This looks fine so far
## Now collect minimal data - refer to ariannes cancovid script to make sure all the variables are collected properismo
## Needs to be done separately for main data vs ON vs AB
## The most important data points are
# 1. Date of diagnosis
# 2. Expected Date of Delivery
# 3. Date of Delivery / Date of Birth
# 4. LMP -> Gestational Age at Delivery -> Preterm Birth
# 5. Admission to NICU
# 6. Admission to ICU 
# 7. Apgar 5
# 8. Vaccine History
# 9. Pregnancy outcome (still birth, live birth, ectopic pregnancy, elective abortion, term abortion, spontaneous abortion)

## We begin with  1.Date of Diagnosis and 2. Hospitalization, Admission to ICU, and Admission to NICU. Because we need these variables to estimate the actual number of cases by province
# dim(full_data_sansON)
# sum(is.na(full_data_sansON$e_diagnosis))
# glimpse(full_data_sansON$e_diagnosis)

## fill in diagnosis date 
full_data_sansON_temp <- full_data_sansON %>% filter(is.na(e_diagnosis))

full_data_sansON_temp$e_diagnosis <- case_when(
  full_data_sansON_temp$d_naso1_result == 1 ~ full_data_sansON_temp$d_naso1_collect,
  full_data_sansON_temp$d_naso2_result == 1 ~ full_data_sansON_temp$d_naso2_collect,
  full_data_sansON_temp$d_naso3_result == 1 ~ full_data_sansON_temp$d_naso3_collect,
  full_data_sansON_temp$d_naso4_result == 1 ~ full_data_sansON_temp$d_naso4_collect,
  full_data_sansON_temp$d_throat_result == 1 ~ full_data_sansON_temp$d_throat_collect,
  full_data_sansON_temp$d_blood_result == 1 ~ full_data_sansON_temp$d_blood_collect,
  full_data_sansON_temp$d_milk_result == 1 ~ full_data_sansON_temp$d_milk_collect,
  full_data_sansON_temp$d_amnio_result == 1 ~ full_data_sansON_temp$d_amnio_collect,
  full_data_sansON_temp$d_igm_result == 1 ~ full_data_sansON_temp$d_igm_collect,
  full_data_sansON_temp$d_igg_result == 1 ~ full_data_sansON_temp$d_igg_collect,
  full_data_sansON_temp$d_other_result == 1 ~ full_data_sansON_temp$d_other_collect,
  full_data_sansON_temp$d_other_result_2 == 1 ~ full_data_sansON_temp$d_other_collect_2,
  full_data_sansON_temp$d_other_result_3 == 1 ~ full_data_sansON_temp$d_other_collect_3
)



# full_data_sansON_temp$e_diagnosis 
# dim(full_data_sansON_temp)
# dim(full_data_sansON[which(is.na(full_data_sansON$e_diagnosis)),])

full_data_sansON[which(is.na(full_data_sansON$e_diagnosis)), c("e_diagnosis")] <- full_data_sansON_temp$e_diagnosis

sum(is.na(full_data_sansON$e_diagnosis))

# hospitalization, admission to ICU, admission to NICU


#hospitalization:
full_data_sansON$hospitalization <- case_when(
  full_data_sansON$e_hosp == 1 ~ 1,
  full_data_sansON$e_hosp2 == 1 ~ 1,
  full_data_sansON$e_hosp3 == 1 ~ 1,
  full_data_sansON$e_hosp4 == 1 ~ 1
)

table(full_data_sansON$hospitalization,full_data_sansON$e_hosp2,useNA="always")
table(full_data_sansON$e_hosp,full_data_sansON$e_hosp2,useNA="always")

table(full_data_sansON$hospitalization,full_data_sansON$e_hosp3,useNA="always")
table(full_data_sansON$e_hosp,full_data_sansON$e_hosp3,useNA="always")

table(full_data_sansON$hospitalization,full_data_sansON$e_hosp4,useNA="always")
table(full_data_sansON$e_hosp,full_data_sansON$e_hosp4,useNA="always")

full_data_sansON$hospitalization[is.na(full_data_sansON$hospitalization)] <- 0

#ICU
full_data_sansON$icu <- case_when(
  full_data_sansON$g_icu == 1 ~ 1,
  full_data_sansON$g_icu2 == 1 ~ 1,
  full_data_sansON$g_icu3 == 1 ~ 1,
  full_data_sansON$g_icu4 == 1 ~ 1
)
table(full_data_sansON$icu,full_data_sansON$g_icu,useNA="always")
table(full_data_sansON$g_icu,full_data_sansON$g_icu2,useNA="always")
table(full_data_sansON$g_icu,full_data_sansON$g_icu3,useNA="always")
table(full_data_sansON$g_icu,full_data_sansON$g_icu4,useNA="always")


full_data_sansON$icu[is.na(full_data_sansON$icu)] <- 0


# NICU

full_data_sansON$NICU <- case_when(
  full_data_sansON$t_nicu == 1  ~ "Yes",
  full_data_sansON$t_nicu_2 == 1 ~ "Yes",
  full_data_sansON$t_nicu_3 == 1 ~ "Yes",
  full_data_sansON$t_nicu == 0 ~ "No",
  full_data_sansON$t_nicu_2 == 0 ~ "No",
  full_data_sansON$t_nicu_3 == 0 ~ "No",
  # technically the below is not correct. I figure it was a way to capture cases 
  # without NICU status, which should be cleaned by this point? 
  is.na(full_data_sansON$s_bw_gm) == FALSE ~ "No")



## fill these out first
# 2. Expected Date of Delivery
# 3. Date of Delivery / Date of Birth
# 4. LMP -> Gestational Age at Delivery -> Preterm Birth
# 5. Admission to NICU
# 6. Admission to ICU 
# 7. Apgar 5
# 8. Vaccine History
# 9. Pregnancy outcome (still birth, live birth, ectopic pregnancy, elective abortion, term abortion, spontaneous abortion)

#### CLEAN DATA ####
#The point here is to identify missing as 999 or 99 or 9999-99-99 or whatever, and all NAs can be "0"
#Then recode everything as a factor or numeric. 
#This needs to be done for all provinces and then ontario separately 
full_data_sansON$r_dob <- replace(full_data_sansON$r_dob, which(full_data_sansON$r_dob == "9999-09-09"), NA)
full_data_sansON$i_deliverydate_est <- replace(full_data_sansON$i_deliverydate_est, which(full_data_sansON$i_deliverydate_est == "9999-09-09"), NA)
full_data_sansON$e_diagnosis <- replace(full_data_sansON$e_diagnosis, which(full_data_sansON$e_diagnosis == "9999-09-09"), NA)
full_data_sansON$s_apgar_5 <- replace(full_data_sansON$s_apgar_5, which(full_data_sansON$s_apgar_5 == "999"), NA)
full_data_sansON$n_covid <- replace(full_data_sansON$n_covid, which(full_data_sansON$n_covid == "999"), NA)
full_data_sansON$s_bw_gm <- replace(full_data_sansON$s_bw_gm, which(full_data_sansON$s_bw_gm == "999"), NA)
full_data_sansON$b_age <- replace(full_data_sansON$b_age, which(full_data_sansON$b_age == "999"), NA)
full_data_sansON$h_gravida <- replace(full_data_sansON$h_gravida, which(full_data_sansON$h_gravida == "999"), NA)
full_data_sansON$e_diagnosis <- replace(full_data_sansON$e_diagnosis, which(full_data_sansON$e_diagnosis == "8888-08-08"), NA)
full_data_sansON$e_hosp <- replace(full_data_sansON$e_hosp, which(full_data_sansON$e_hosp == "999"), NA)
full_data_sansON$e_hosp2 <- replace(full_data_sansON$e_hosp2, which(full_data_sansON$e_hosp2 == "999"), NA)



# expected date of delivery
sum(is.na(full_data_sansON$i_deliverydate_est))
# date of birth
sum(is.na(full_data_sansON$r_dob))
#ga_at_del

EDD_DOB <- (as.Date(full_data_sansON$i_deliverydate_est) - as.Date(full_data_sansON$r_dob))

full_data_sansON$r_dob <- as.Date(as.character(full_data_sansON$r_dob), format = "%Y-%m-%d")

# full_data_sansON$r_dob <- replace(full_data_sansON$r_dob, which(full_data_sansON$r_dob == "9999-09-09"), NA)


# This doesn't feel right
full_data_sansON$ga_at_del <- as.numeric((280 - (as.Date(full_data_sansON$i_deliverydate_est) - as.Date(full_data_sansON$r_dob)))/7)


##### CCLEAN THIS UP !! #####
# full_data_sansON$ga_at_del[which(full_data_sansON$ga_at_del < 20)] <- NA
# full_data_sansON$ga_at_del[which(full_data_sansON$ga_at_del > 50)] <- NA


full_data_sansON$date_diff <- as.Date(full_data_sansON$r_dob) - as.Date(full_data_sansON$i_deliverydate_est)

data_check1 <- full_data_sansON %>% filter(abs(date_diff) > 100) %>% dplyr::select(a_record_id, date_diff,r_dob,i_deliverydate_est,i_lmp,e_diagnosis,p_outcome)

# ## manual fixes (get data abstraction team to do these)
# full_data_sansON$i_deliverydate_est[which(full_data_sansON$a_record_id == "CCP-01-1154")] <- "2022-02-15" ## 1 year off
# 
# full_data_sansON$i_deliverydate_est[which(full_data_sansON$a_record_id == "CCP-01-1159")] <- "2021-08-28" ## lmp and expected delivery date flipped
# full_data_sansON$i_lmp[which(full_data_sansON$a_record_id == "CCP-01-1159")] <- "2020-11-21" ## lmp and expected delivery date flipped
# 
# full_data_sansON$i_deliverydate_est[which(full_data_sansON$a_record_id == "CCP-01-1196")] <- "2022-01-06" ## expected delivery date 1 year off, lmp also off
# 
# full_data_sansON$r_dob[which(full_data_sansON$a_record_id == "CCP-01-1211")] <- "2022-04-13" ## r_dob 1 year off


## do this instead for now
full_data_sansON$ga_at_del[which(full_data_sansON$ga_at_del < 20)] <- NA
full_data_sansON$ga_at_del[which(full_data_sansON$ga_at_del > 50)] <- NA

# plot(full_data_sansON$ga_at_del )

full_data_sansON$ga_del_cat <- case_when(
  full_data_sansON$ga_at_del < 28 ~ "extremely preterm",
  full_data_sansON$ga_at_del < 32 ~ "very preterm",
  full_data_sansON$ga_at_del < 34 ~ "moderate preterm",
  full_data_sansON$ga_at_del < 37 ~ "late preterm",
  full_data_sansON$ga_at_del >= 37 ~ "term",
)
table(full_data_sansON$ga_at_del,full_data_sansON$ga_del_cat,exclude=NULL)

full_data_sansON$ga_del_cat2 <- case_when(
  full_data_sansON$ga_at_del < 37 ~ "preterm",
  full_data_sansON$ga_at_del >= 37 ~ "term"
)
table(full_data_sansON$ga_del_cat,full_data_sansON$ga_del_cat2,exclude=NULL)

sum(full_data_sansON$ga_del_cat2 == "preterm",na.rm = TRUE)

## APGAR 5

full_data_sansON$apgar5 <- factor(ifelse(as.numeric(full_data_sansON$s_apgar_5) < 7, "<7", "â‰¥7"))

## Preg Outcome
full_data_sansON$birth_outcome <- case_when(
  full_data_sansON$p_outcome == 1 ~ "Loss",
  full_data_sansON$p_outcome == 2 ~ "Stillbirth",
  full_data_sansON$p_outcome == 3 ~ "Livebirth",
  full_data_sansON$o_loss == 1 ~ "Loss",
  full_data_sansON$o_sb == 1 ~ "Stillbirth",
  full_data_sansON$o_spont == 1 ~ "Loss",
  full_data_sansON$o_elective == 1 ~ "Loss"
)

sum(is.na(full_data_sansON$birth_outcome))

# full_data_sansON$o_spont[is.na(full_data_sansON$birth_outcome)]

## Vaccine history - DOES NOT CAPTURE DOSE NUMBER OR IG GIVEN IN PREG

summary(as.factor(full_data_sansON$n_covid))

## Birth Weight
full_data_sansON$s_bw_gm <- as.numeric(full_data_sansON$s_bw_gm)

full_data_sansON$bw_cat <- case_when(
  full_data_sansON$s_bw_gm < 2500 ~ "<2500",
  full_data_sansON$s_bw_gm >= 2500 & full_data_sansON$s_bw_gm <=4000 ~ "2500-4000",
  full_data_sansON$s_bw_gm > 4000 ~ ">4000"
)

#Ethnicity

#full_data_sansON$eth <-  case_when(
#  all(c(full_data_sansON$b_ethnicity___1, full_data_sansON$b_ethnicity___2, full_data_sansON$b_ethnicity___3, full_data_sansON$b_ethnicity___4, full_data_sansON$b_ethnicity___5, full_data_sansON$b_ethnicity___6, full_data_sansON$b_ethnicity___7, full_data_sansON$b_ethnicity___8, full_data_sansON$b_ethnicity___998, full_data_sansON$b_ethnicity___999) == 0) ~ "Missing",
#  full_data_sansON$b_ethnicity___999 == 1 ~ "Unknown",
#  full_data_sansON$b_ethnicity___998 == 1 ~ "Other",
#  full_data_sansON$b_ethnicity___1 == 1 ~ "White",
#  full_data_sansON$b_ethnicity___2 == 1 ~ "African/Carribean/Black",
#  full_data_sansON$b_ethnicity___3 == 1 ~ "Hispanic/Latino",
#  full_data_sansON$b_ethnicity___4 == 1 ~ "East Asian",
#  full_data_sansON$b_ethnicity___5 == 1 ~ "South Asian",
#  full_data_sansON$b_ethnicity___6 == 1 ~ "South East Asian",
#  full_data_sansON$b_ethnicity___7 == 1 ~ "Middle East",
#  full_data_sansON$b_ethnicity___8 == 1 ~ "Indigenous"
#)


eth1<-cbind(
  as.numeric(full_data_sansON$b_ethnicity___1),
  as.numeric(full_data_sansON$b_ethnicity___2),
  as.numeric(full_data_sansON$b_ethnicity___3),
  as.numeric(full_data_sansON$b_ethnicity___4),
  as.numeric(full_data_sansON$b_ethnicity___5),
  as.numeric(full_data_sansON$b_ethnicity___6),
  as.numeric(full_data_sansON$b_ethnicity___7),
  as.numeric(full_data_sansON$b_ethnicity___8),
  as.numeric(full_data_sansON$b_ethnicity___998),
  as.numeric(full_data_sansON$b_ethnicity___999))

temp1<-rep(NA,length(full_data_sansON$a_record_id))
temp2<-rep(NA,length(full_data_sansON$a_record_id))
for(i in 1:length(temp1)){
temp1[i]<-sum(as.numeric(full_data_sansON$b_ethnicity___1[i]),
    as.numeric(full_data_sansON$b_ethnicity___2[i]),
    as.numeric(full_data_sansON$b_ethnicity___3[i]),
    as.numeric(full_data_sansON$b_ethnicity___4[i]),
    as.numeric(full_data_sansON$b_ethnicity___5[i]),
    as.numeric(full_data_sansON$b_ethnicity___6[i]),
    as.numeric(full_data_sansON$b_ethnicity___7[i]),
    as.numeric(full_data_sansON$b_ethnicity___8[i]))
}


##Technically 4 and 6 designate separate choices
for(i in 1:length(temp2)){
if(temp1[i]==1){
if(as.numeric(full_data_sansON$b_ethnicity___1[i])==1){temp2[i]<-"White"}
if(as.numeric(full_data_sansON$b_ethnicity___2[i])==1){temp2[i]<-"African/Carribean/Black"}
if(as.numeric(full_data_sansON$b_ethnicity___3[i])==1){temp2[i]<-"Hispanic/Latino"}
if(as.numeric(full_data_sansON$b_ethnicity___4[i])==1){temp2[i]<-"East or SE Asian"}
if(as.numeric(full_data_sansON$b_ethnicity___5[i])==1){temp2[i]<-"South Asian"}
if(as.numeric(full_data_sansON$b_ethnicity___6[i])==1){temp2[i]<-"East or SE Asian"}
if(as.numeric(full_data_sansON$b_ethnicity___7[i])==1){temp2[i]<-"Middle East"}
if(as.numeric(full_data_sansON$b_ethnicity___8[i])==1){temp2[i]<-"Indigenous"}
}
  
if(temp1[i]>1){
    if(full_data_sansON$b_ethnicity___8[i]==1){temp2[i]<-"Indigenous"}
    else if(temp1[i]>2){temp2[i]="Other"}
    else if(as.numeric(full_data_sansON$b_ethnicity___2[i])==1){temp2[i]<-"African/Carribean/Black"}
    else if(as.numeric(full_data_sansON$b_ethnicity___3[i])==1){temp2[i]<-"Hispanic/Latino"}
    else if(as.numeric(full_data_sansON$b_ethnicity___4[i])==1){temp2[i]<-"East or SE Asian"}
    else if(as.numeric(full_data_sansON$b_ethnicity___5[i])==1){temp2[i]<-"South Asian"}
    else if(as.numeric(full_data_sansON$b_ethnicity___6[i])==1){temp2[i]<-"East or SE Asian"}
    else if(as.numeric(full_data_sansON$b_ethnicity___7[i])==1){temp2[i]<-"Middle East"}
}
  
if(temp1[i]==0){
if(full_data_sansON$b_ethnicity___998[i]==1){temp2[i]<-"Other"}
else if(full_data_sansON$b_ethnicity___999[i]==1){temp2[i]<-"Missing"}
else{temp2[i]<-"Missing"}
}
}

full_data_sansON$eth_cat <- factor(temp2)

full_data_sansON$eth_cat <- relevel(full_data_sansON$eth_cat, ref = "White")
describeFactors(full_data_sansON$eth_cat)


## age
# full_data_sansON$b_age


#This makes more sense as a cleaning check in my opinion
full_data_sansON <- full_data_sansON %>%
  mutate(allna_check = all(is.na(c(e_asymptomatic, e_fever, e_cough, e_head, e_breath, e_runny, e_muscle, e_anorexia, e_diarrhea, e_vomit, e_fatigue, e_anosmia, e_throat, e_sputum, e_malaise, e_othersx))))

### symptoms

full_data_sansON$asym <- NA
full_data_sansON$asym <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_asymptomatic == 1 ~ "Yes",
  full_data_sansON$e_asymptomatic == 0 ~ "No"
)

full_data_sansON$fever <- NA
full_data_sansON$fever <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_fever == 1 ~ "Yes",
  grepl("chill|sweat|Frissons|Chiils", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("chill|sweat|Frissons|Chiils", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("chill|sweat|Frissons|Chiils", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_fever == 0 ~ "No"
)

full_data_sansON$cough <- NA
full_data_sansON$cough <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_cough == 1 ~ "Yes",
  full_data_sansON$e_cough == 0 ~ "No"
)

full_data_sansON$head <- NA
full_data_sansON$head <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_head == 1 ~ "Yes",
  grepl("tender frontal|head", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("tender frontal|head", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("tender frontal|head", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_head == 0 ~ "No"
)

full_data_sansON$breath <- NA
full_data_sansON$breath <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_breath == 1 ~ "Yes",
  grepl("chest congestion|breathing|wheez|orthpnoea|asthma", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("chest congestion|breathing|wheez|orthpnoea|asthma", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("chest congestion|breathing|wheez|orthpnoea|asthma", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_breath == 0 ~ "No"
)

full_data_sansON$runny <- NA
full_data_sansON$runny <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_runny == 1 ~ "Yes",
  grepl("nasal|nose|congestion|rhin|sinus|URI", full_data_sansON$e_othersx_spec, ignore.case = TRUE) & !grepl("chest", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("nasal|nose|congestion|rhin|sinus|URI", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) & !grepl("chest", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("nasal|nose|congestion|rhin|sinus|URI", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) & !grepl("chest", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_runny == 0 ~ "No"
)

full_data_sansON$weakness <- NA
full_data_sansON$weakness <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  grepl("weakness", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("weakness", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("weakness", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_othersx == 0 ~ "No"
)


#TODO: check if this syntax is actually necessary
full_data_sansON$muscle <- NA
full_data_sansON$muscle <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_muscle == 1 ~ "Yes",
  grepl("flu|joint|abdominal pain|BackÂ andÂ musculoskeletalÂ pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("flu|joint|abdominal pain|BackÂ andÂ musculoskeletalÂ pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("flu|joint|abdominal pain|BackÂ andÂ musculoskeletalÂ pain|Arthralgia|sore legs|aigue|lombaire|Lower left quadrant pain|leg cramps|abdominal pain|abdominal|achiness", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_muscle == 0 ~ "No"
)

full_data_sansON$anorexia <- NA
full_data_sansON$anorexia <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_anorexia == 1 ~ "Yes",
  full_data_sansON$e_anorexia == 0 ~ "No"
)

full_data_sansON$diarrhea <- NA
full_data_sansON$diarrhea <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_diarrhea == 1 ~ "Yes",
  full_data_sansON$e_diarrhea == 0 ~ "No"
)

full_data_sansON$vomit <- NA
full_data_sansON$vomit <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_vomit == 1 ~ "Yes",
  full_data_sansON$e_vomit == 0 ~ "No"
)

full_data_sansON$fatigue <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_fatigue == 1 ~ "Yes",
  full_data_sansON$e_fatigue == 0 ~ "No"
)

full_data_sansON$anosmia <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_anosmia == 1 ~ "Yes",
  full_data_sansON$e_anosmia == 0 ~ "No"
)

full_data_sansON$throat <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_throat == 1 ~ "Yes",
  grepl("odynophagia|gorge|Laryngitis|throat", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("odynophagia|gorge|Laryngitis|throat", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("odynophagia|gorge|Laryngitis|throat", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  full_data_sansON$e_throat == 0 ~ "No"
)

full_data_sansON$sputum <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_sputum == 1 ~ "Yes",
  full_data_sansON$e_sputum == 0 ~ "No"
)

full_data_sansON$malaise <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  full_data_sansON$e_malaise == 1 ~ "Yes",
  full_data_sansON$e_malaise == 0 ~ "No"
)

#TODO: check if this syntax is actually necessary
full_data_sansON$chest_pain <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  grepl("douleur pleurÃ©tique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("douleur pleurÃ©tique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("douleur pleurÃ©tique|chest pain|hypocondre|Chest tightness|Heaviness in chest|Central chest burning|pulmonary embolism|pain with deep inhale|deep inspiration", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  TRUE ~ "No"
)


full_data_sansON$tachycardia <- case_when(
  full_data_sansON$allna_check ~ "Miss",
  grepl("Tachycardia|Increased heart rate|heart rate", full_data_sansON$e_othersx_spec, ignore.case = TRUE) ~ "Yes",
  grepl("Tachycardia|Increased heart rate|heart rate", full_data_sansON$e_othersx_spec_2, ignore.case = TRUE) ~ "Yes",
  grepl("Tachycardia|Increased heart rate|heart rate", full_data_sansON$e_othersx_spec_3, ignore.case = TRUE) ~ "Yes",
  TRUE ~ "No"
)


# replace the No entry with NA
full_data_sansON <- full_data_sansON %>% 
  mutate(across(.cols = c(asym, fever, cough, head, breath, runny, muscle, anorexia, diarrhea, vomit, fatigue, anosmia, throat, sputum, malaise, chest_pain, tachycardia, weakness), ~ifelse(.x == "Miss", NA, .x)))
describeFactors(full_data_sansON$fever)
describeFactors(full_data_sansON$chest_pain)


full_data_sansON <- full_data_sansON %>% mutate(all_nacheck2 = all(is.na(c(j_cns, j_cvs, j_resp, j_gi, j_gu, j_repro, j_endo, j_ms, j_hem, j_mh, j_aai))))


full_data_sansON$cvs <- case_when(
  full_data_sansON$all_nacheck2 & as.numeric(full_data_sansON$j_none___1) == 0 ~ "No entry",
  full_data_sansON$all_nacheck2 ~ "No",
  as.numeric(full_data_sansON$j_none___1) == 1 ~ "No",
  as.numeric(full_data_sansON$j_cvs) == 1 ~ "Yes",
  as.numeric(full_data_sansON$l_htn) == 1 ~ "Yes",
  as.numeric(full_data_sansON$j_cvs) == 0 ~ "No",
  is.na(full_data_sansON$j_cvs) ~ "No Entry"
)

full_data_sansON$cns <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2  ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_cns == 1 ~ "Yes",
  full_data_sansON$j_cns == 0 ~ "No",
  is.na(full_data_sansON$j_cns) ~ "No Entry"
)

full_data_sansON$resp <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2  ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_resp == 1 ~ "Yes",
  full_data_sansON$j_resp == 0 ~ "No",
  is.na(full_data_sansON$j_resp) ~ "No Entry"
)

full_data_sansON$eentm <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2  ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_eentm == 1 ~ "Yes",
  full_data_sansON$j_eentm == 0 ~ "No",
  is.na(full_data_sansON$j_eentm) ~ "No Entry"
)


full_data_sansON$gi <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2 ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_gi == 1 ~ "Yes",
  full_data_sansON$j_gi == 0 ~ "No",
  is.na(full_data_sansON$j_gi) ~ "No Entry"
)

full_data_sansON$gu <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2 ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_gu == 1 ~ "Yes",
  full_data_sansON$j_gu == 0 ~ "No",
  is.na(full_data_sansON$j_gu) ~ "No Entry"
)

full_data_sansON$repro <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2 ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_repro == 1 ~ "Yes",
  full_data_sansON$j_repro == 0 ~ "No",
  is.na(full_data_sansON$j_repro) ~ "No Entry"
)

full_data_sansON$endo <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2  ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_endo == 1 ~ "Yes",
  full_data_sansON$j_endo == 0 ~ "No",
  is.na(full_data_sansON$j_endo) ~ "No Entry"
)

full_data_sansON$ms <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2  ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_ms == 1 ~ "Yes",
  full_data_sansON$j_ms == 0 ~ "No",
  is.na(full_data_sansON$j_ms) ~ "No Entry"
)

full_data_sansON$hem <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2  ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_hem == 1 ~ "Yes",
  full_data_sansON$j_hem == 0 ~ "No",
  is.na(full_data_sansON$j_hem) ~ "No Entry"
)

full_data_sansON$mh <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2  ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_mh == 1 ~ "Yes",
  full_data_sansON$j_mh == 0 ~ "No",
  is.na(full_data_sansON$j_mh) ~ "No Entry"
)

full_data_sansON$aai <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2  ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_aai == 1 ~ "Yes",
  full_data_sansON$j_aai == 0 ~ "No",
  is.na(full_data_sansON$j_aai) ~ "No Entry"
)

full_data_sansON$other_comor <- case_when(
  full_data_sansON$all_nacheck2 & full_data_sansON$j_none___1 == 0 ~ "No entry",
  full_data_sansON$all_nacheck2 ~ "No",
  full_data_sansON$j_none___1 == 1 ~ "No",
  full_data_sansON$j_oth == 1 ~ "Yes",
  full_data_sansON$j_oth == 0 ~ "No",
  is.na(full_data_sansON$j_oth) ~ "No Entry"
)


##### DATA CHECK OF SYMPTOMS NAD COMORBIDITIES ####
## LOOKS GOOD

full_data_sansON %>% group_by(prov) %>% summarise(across(c(asym, fever, cough, head, breath, runny, muscle, anorexia, diarrhea, vomit, fatigue, anosmia, throat, sputum, malaise, chest_pain, tachycardia, weakness), ~sum(.x == "Yes", na.rm = TRUE)/n()))
full_data_sansON %>% group_by(prov) %>% summarise(across(c(cvs,cns,resp,eentm,gi,gu,repro,endo,ms,hem,mh,aai,other_comor), ~sum(.x == "Yes", na.rm = TRUE)/n()))

# 
# # replace the No entry with NA
full_data_sansON$cvs <- ifelse(full_data_sansON$cvs  == "No entry", NA, full_data_sansON$cvs)

# # check to make sure (looks good)
# full_data_sansON %>% group_by(prov) %>% summarise(missing_percent = sum(is.na(cvs))/n())
# describeFactors(full_data_sansON$cvs)

## extract specific comorbidities of interest
# asthma
full_data_sansON$asthma <- case_when(
  full_data_sansON$j_resp_asth___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
# describeFactors(full_data_sansON$asthma)

# Chronic obstructive lung disease
full_data_sansON$lung <- case_when(
  full_data_sansON$j_resp_lung___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
# describeFactors(full_data_sansON$lung)

# hypertension pre-existing
full_data_sansON$htn <- case_when(
  full_data_sansON$j_cvs_htn___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
# describeFactors(full_data_sansON$htn)

# diabetes 1 or 2
full_data_sansON$diabetes <- case_when(
  full_data_sansON$j_endo_diabt1___1 == 1 | full_data_sansON$j_endo_diabt2___1 == 1 ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
# describeFactors(full_data_sansON$diabetes)


# autoimmune diseases have to be gathered from a few places
# celiac, lupus, rheumatoid arthritis, Ankylosing spondylitis, others
# full_data_sansON$j_aai_oth_s # check this variable for others eg MS

full_data_sansON$autoimm <- case_when(
  (full_data_sansON$j_gi_celiac___1 == 1) | (full_data_sansON$j_ms_lupus___1 == 1) | (full_data_sansON$j_ms_as___1 == 1) | (full_data_sansON$j_ms_ra___1 == 1) | grepl("Multiple sclerosis|Antiphospho|neutropenia|SLE|lupus", full_data_sansON$j_aai_oth_s, ignore.case = TRUE) ~ "Yes",
  is.na(full_data_sansON$cvs) ~ NA_character_,
  !is.na(full_data_sansON$cvs) ~ "No"
)
# describeFactors(full_data_sansON$autoimm)

## composite comorbidities
# c(full_data_sansON$cvs, full_data_sansON$diabetes, full_data_sansON$htn, full_data_sansON$asthma, full_data_sansON$hbv, full_data_sansON$hcv)

full_data_sansON$any_comor <- case_when(
  full_data_sansON$cvs == "Yes" | full_data_sansON$asthma == "Yes" | full_data_sansON$diabetes == "Yes" | full_data_sansON$htn == "Yes" | full_data_sansON$k_hbv == 1 | full_data_sansON$k_hcv == 1 ~ "Yes",
  full_data_sansON$cvs == "No" & full_data_sansON$asthma == "No" & full_data_sansON$diabetes == "No" & full_data_sansON$htn == "No" ~ "No"
)



full_data_sansON$i_weight <- replace(full_data_sansON$i_weight, which(full_data_sansON$i_weight == 999 | full_data_sansON$i_weight == 666), NA)
full_data_sansON$i_weight <- replace(full_data_sansON$i_weight, which(as.numeric(full_data_sansON$i_weight) == 999.9 | as.numeric(full_data_sansON$i_weight) == 999.99), NA)
full_data_sansON$i_weight <- replace(full_data_sansON$i_weight, which(as.numeric(full_data_sansON$i_weight) == 888.8 | as.numeric(full_data_sansON$i_weight) == 888.88), NA)

full_data_sansON$i_height <- replace(full_data_sansON$i_height, which(full_data_sansON$i_height == 999 | full_data_sansON$i_height == 666), NA)
full_data_sansON$i_height <- replace(full_data_sansON$i_height, which(as.numeric(full_data_sansON$i_height) == 999.9 | as.numeric(full_data_sansON$i_height) == 999.99), NA)
full_data_sansON$i_height <- replace(full_data_sansON$i_height, which(as.numeric(full_data_sansON$i_height) == 888.8 | as.numeric(full_data_sansON$i_height) == 888.88), NA)
full_data_sansON$i_height <- replace(full_data_sansON$i_height, which(as.numeric(full_data_sansON$i_height) == 888 | as.numeric(full_data_sansON$i_height) == 999), NA)


full_data_sansON$i_weight[which(as.numeric(full_data_sansON$i_weight)  > 200)] <- NA ## this is an issue 

full_data_sansON$i_weight[which(as.numeric(full_data_sansON$i_weight)  < 50)] <- NA ## this is an issue 

full_data_sansON$i_height[which(as.numeric(full_data_sansON$i_height)  > 300)] <- NA ## this is an issue 

full_data_sansON$i_height[which(as.numeric(full_data_sansON$i_height)  < 2)] ## this is an issue 

full_data_sansON$i_height[which(as.numeric(full_data_sansON$i_height) < 2)] <- 100*as.numeric(full_data_sansON$i_height[which(as.numeric(full_data_sansON$i_height) < 2)])

full_data_sansON$BMI <- as.numeric(full_data_sansON$i_weight)/(as.numeric(full_data_sansON$i_height)/100)^2

table(floor(full_data_sansON$BMI))

describeMedian(full_data_sansON$BMI, iqr = FALSE) # check for out of range values - and remove or correct

full_data_sansON$BMI[which(as.numeric(full_data_sansON$BMI)  > 50)]<-NA

# BMI >= 30 variable
full_data_sansON$BMI_cat <- factor(case_when(
  full_data_sansON$BMI < 18.5 ~ "<18.5",
  full_data_sansON$BMI < 25 ~ "18.5-24",
  full_data_sansON$BMI < 30 ~ "25-29",
  full_data_sansON$BMI >= 30 ~ "â‰¥30"
))

temp<-table(round(full_data_sansON$BMI,1),full_data_sansON$BMI_cat,useNA="always")

# describeFactors(full_data_sansON$BMI_cat)
full_data_sansON$BMI_cat <- factor(full_data_sansON$BMI_cat, levels = c("<18.5", "18.5-24", "25-29", "â‰¥30"))


full_data_sansON$age[which(as.numeric(full_data_sansON$age)  <12)]<-NA
full_data_sansON$age[which(as.numeric(full_data_sansON$age)  >50)]<-NA
full_data_sansON$age_cat <- factor(case_when(
  full_data_sansON$b_age <25 ~ "<25 years",
  full_data_sansON$b_age <30 ~ "25-29 years",
  full_data_sansON$b_age <36 ~ "30-35 years",
  full_data_sansON$b_age <40 ~ "36-39 years",
  full_data_sansON$b_age >=40 ~ "â‰¥40 years"
),
levels = c("<25 years", "25-29 years", "30-35 years", "36-39 years", "â‰¥40 years"))

temp<-table(full_data_sansON$b_age,full_data_sansON$age_cat,useNA="always")


full_data_sansON$age_cat2 <- factor(case_when(
  full_data_sansON$age_cat == "<25 years" ~ "Less than 30",
  full_data_sansON$age_cat == "25-29 years" ~ "Less than 30",
  full_data_sansON$age_cat == "30-35 years" ~ "30-35 years",
  full_data_sansON$age_cat ==  "36-39 years" ~ "36 years and older",
  full_data_sansON$age_cat == "â‰¥40 years" ~ "36 years and older"
),
levels = c("Less than 30", "30-35 years", "36 years and older"))

describeFactors(full_data_sansON$age_cat2)





## omicron start December 19 2021 
## Delta started April 4th 2021
## dates from https://nccid.ca/covid-19-variants/
## THESE ARE START DATES -> TO BE REPLACED WITH "DOMINANCE DATES"

full_data_sansON$covid_period = case_when(
  full_data_sansON$e_diagnosis < as.Date("2021-04-04") ~ "pre-Delta",
  full_data_sansON$e_diagnosis >= as.Date("2021-04-04") & full_data_sansON$e_diagnosis < as.Date("2021-12-19") ~ "Delta",
  full_data_sansON$e_diagnosis >= as.Date("2021-12-19") ~ "Omicron"
)

full_data_sansON$covid_period <- factor(full_data_sansON$covid_period, levels = c("pre-Delta","Delta","Omicron"))

covid_period_table<- full_data_sansON %>% filter(e_diagnosis < as.Date("2023-01-01")) %>% group_by(covid_period) %>% summarise(n = n(),nicu = sum(NICU == "Yes", na.rm = TRUE),stillbirth = sum(o_sb == 1, na.rm = TRUE),hospitalizations = sum(e_hosp == "1", na.rm = TRUE),icu_admissions = sum(g_icu == "1", na.rm = TRUE))

covid_period_table2 <- full_data_sansON %>%filter(e_diagnosis < as.Date("2023-01-01")) %>% group_by(covid_period) %>% summarise(n = n(),term_births = sum(ga_del_cat == "term", na.rm = TRUE), late_preterm = sum(ga_del_cat == "late preterm", na.rm = TRUE),moderate_preterm = sum(ga_del_cat == "moderate preterm", na.rm = TRUE),very_preterm = sum(ga_del_cat == "very preterm", na.rm = TRUE))




# # how to multiply data? duplicate rows 10x that have "to_multiply" = 1
# # do this later?
# full_data_sansON_duplicated <- full_data_sansON[c(which(full_data_sansON$to_multiply != 1),rep(which(full_data_sansON$to_multiply == 1), 10)), ]
# 
# covid_period_table3<- full_data_sansON_duplicated %>% group_by(covid_period) %>% summarise(n = n(),nicu = sum(NICU == "Yes", na.rm = TRUE),stillbirth = sum(o_sb == 1, na.rm = TRUE),hospitalizations = sum(e_hosp == "1", na.rm = TRUE),icu_admissions = sum(g_icu == "1", na.rm = TRUE))
# 
# covid_period_table23 <- full_data_sansON_duplicated %>% group_by(covid_period) %>% summarise(n = n(),term_births = sum(ga_del_cat == "term", na.rm = TRUE), late_preterm = sum(ga_del_cat == "late preterm", na.rm = TRUE),moderate_preterm = sum(ga_del_cat == "moderate preterm", na.rm = TRUE),very_preterm = sum(ga_del_cat == "very preterm", na.rm = TRUE))
# 

## Combine with ontario data?? Then present outcomes by hospitalization? Then show temporal trends?

#### ontario fixes: #####
data_on$BMI_cat <- factor(case_when(
  data_on$new_BMI < 18.5 ~ "<18.5",
  data_on$new_BMI < 25 ~ "18.5-24",
  data_on$new_BMI < 30 ~ "25-29",
  data_on$new_BMI >= 30 ~ "â‰¥30"
))

temp<-table(floor(data_on$new_BMI),data_on$BMI_cat )

# describeFactors(data_on$BMI_cat)
data_on$BMI_cat <- factor(data_on$BMI_cat, levels = c("<18.5", "18.5-24", "25-29", "â‰¥30"))

data_on$htn <-  ifelse(data_on$hyper_final == 1, 1,0)
data_on$diabetes <-  ifelse(data_on$diabetes_final == 1, 1,0)

           

eth2<-cbind(data_on$PH_SES_WHITE,
  data_on$PH_SES_BLACK,
  data_on$PH_SES_LATINO,
  data_on$PH_SES_EAST_ASIAN,
  data_on$PH_SES_SOUTH_ASIAN,
  data_on$PH_SES_EAST_SE_ASIAN,
  data_on$PH_SES_SE_ASIAN,
  data_on$PH_SES_MIDDLE_EASTERN,
  data_on$PH_RACE_OTHER,
  data_on$PH_RACE_PREFER_NOT_TO_ANSWER,
  data_on$PH_RACE_DO_NOT_KNOW)


temp1<-rep(NA,length(data_on$PH_SES_WHITE))
temp2<-rep(NA,length(data_on$PH_SES_WHITE))
for(i in 1:length(temp1)){
  temp1[i]<-sum(data_on$PH_SES_WHITE[i]=="YES",
                data_on$PH_SES_BLACK[i]=="YES",
                data_on$PH_SES_LATINO[i]=="YES",
                data_on$PH_SES_EAST_ASIAN[i]=="YES",
                data_on$PH_SES_SOUTH_ASIAN[i]=="YES",
                data_on$PH_SES_EAST_SE_ASIAN[i]=="YES",
                data_on$PH_SES_SE_ASIAN[i]=="YES",
                data_on$PH_SES_MIDDLE_EASTERN[i]=="YES")
}

for(i in 1:length(temp2)){
  if(temp1[i]==1){
    if(data_on$PH_SES_WHITE[i]=="YES"){temp2[i]<-"White"}
    if(data_on$PH_SES_BLACK[i]=="YES"){temp2[i]<-"African/Carribean/Black"}
    if(data_on$PH_SES_LATINO[i]=="YES"){temp2[i]<-"Hispanic/Latino"}
    if(data_on$PH_SES_EAST_ASIAN[i]=="YES"){temp2[i]<-"East or SE Asian"}
    if(data_on$PH_SES_SOUTH_ASIAN[i]=="YES"){temp2[i]<-"South Asian"}
    if(data_on$PH_SES_EAST_SE_ASIAN[i]=="YES"){temp2[i]<-"East or SE Asian"}
    if(data_on$PH_SES_SE_ASIAN[i]=="YES"){temp2[i]<-"East or SE Asian"}
    if(data_on$PH_SES_MIDDLE_EASTERN[i]=="YES"){temp2[i]<-"Middle East"}
  }
  if(temp1[i]>1){
    if(data_on$PH_SES_WHITE[i]!="YES"){temp2[i]<-"Other"}
    else if(data_on$PH_SES_BLACK[i]=="YES"){temp2[i]<-"African/Carribean/Black"}
    else if(data_on$PH_SES_LATINO[i]=="YES"){temp2[i]<-"Hispanic/Latino"}
    else if(data_on$PH_SES_EAST_ASIAN[i]=="YES"){temp2[i]<-"East or SE Asian"}
    else if(data_on$PH_SES_SOUTH_ASIAN[i]=="YES"){temp2[i]<-"South Asian"}
    else if(data_on$PH_SES_EAST_SE_ASIAN[i]=="YES"){temp2[i]<-"East or SE Asian"}
    else if(data_on$PH_SES_SE_ASIAN[i]=="YES"){temp2[i]<-"East or SE Asian"}
    else if(data_on$PH_SES_MIDDLE_EASTERN[i]=="YES"){temp2[i]<-"Middle East"}
  }
  
  if(temp1[i]==0){
  if(data_on$PH_RACE_OTHER[i]=="YES"){temp2[i]<-"Other"}
  else{temp2[i]<-"Missing"}
  }
}
temp3<-cbind(temp2,eth2)

data_on$eth_cat <- factor(temp2)

data_on$eth_cat <- relevel(data_on$eth_cat, ref = "White")
describeFactors(data_on$eth_cat)


data_on$hospitalization <- case_when(
  data_on$C_hospitalized_COVID == "Yes" ~ 1,
  data_on$C_hospitalized_COVID == "No" ~ 0
)

data_on$icu <- case_when(
  data_on$C_ICU_admission == "Yes" ~ 1,
  data_on$C_ICU_admission == "No" ~ 0
)

data_on$NICU <- case_when(
  data_on$NICU_admission_flag == "Y" ~ 1,
  data_on$NICU_admission_flag == "N" ~ 0
)


## gestational age at birth (term vs preterm)
data_on$B_GA_birth[which(data_on$B_GA_birth == "")] <- NA

# summary(as.factor(data_on$B_GA_birth))
# 
# summary(as.factor(full_data_sansON$ga_del_cat2))
# summary(as.factor(full_data_sansON$ga_del_cat))
full_data_sansON$ga_del_cat <- factor(full_data_sansON$ga_del_cat, levels = c("extremely preterm","very preterm","moderate preterm","late preterm","term"))

data_on$ga_del_cat = case_when(
  data_on$B_GA_birth == "< 28 weeks" ~ "extremely preterm",
  data_on$B_GA_birth == "28 - 31 weeks" ~ "very preterm",
  data_on$B_GA_birth == "32-33 weeks" ~ "moderate preterm",
  data_on$B_GA_birth == "34-36 weeks" ~ "late preterm",
  data_on$B_GA_birth == "Term" ~ "term"
)
data_on$ga_del_cat <- factor(data_on$ga_del_cat, levels = c("extremely preterm","very preterm","moderate preterm","late preterm","term"))

# summary(as.factor(data_on$ga_del_cat))

data_on$ga_del_cat2 <- case_when(
  data_on$ga_del_cat == "extremely preterm" ~ "preterm",
  data_on$ga_del_cat == "very preterm" ~ "preterm",
  data_on$ga_del_cat == "moderate preterm" ~ "preterm",
  data_on$ga_del_cat == "late preterm" ~ "preterm",
  data_on$ga_del_cat == "term" ~ "term"
)
# summary(as.factor(data_on$ga_del_cat2))

#gravidity
# summary(as.factor(data_on$gravida))

# summary(as.factor(full_data_sansON$h_gravida))

data_on$h_gravida  <- data_on$gravida

full_data_sansON$gravida <- as.numeric(full_data_sansON$h_gravida)


# asthma (skip for now? non ontario only?)
# summary(as.factor(full_data_sansON$asthma))

#hospitalization/ icu done. Try "oxygen"
# 
# summary(as.factor(full_data_sansON$e_oxygen___1))
# summary(as.factor(data_on$H_Any_Other_Oxygen))

full_data_sansON$oxygen <- case_when(
  full_data_sansON$e_oxygen___1 == 1 ~ TRUE,
  full_data_sansON$e_oxygen___1 == 0 ~ FALSE,
  is.na(full_data_sansON$e_oxygen___1) ~ NA
)

data_on$oxygen <- case_when(
  data_on$H_Any_Other_Oxygen == "Yes" ~ TRUE,
  data_on$H_Any_Other_Oxygen == "No" ~ FALSE,
  data_on$H_Any_Other_Oxygen == "Unknown" ~ NA,
  is.na(data_on$H_Any_Other_Oxygen) ~ NA
)

summary(as.factor(data_on$oxygen))
summary(as.factor(full_data_sansON$oxygen))

# mode of delivery
# summary(as.factor(full_data_sansON$p_mode))
# summary(as.factor(data_on$birth_type_id))

data_on$mode_del <- case_when(
  data_on$birth_type_id == 1012880 ~ "vaginal",
  data_on$birth_type_id == 1012890 ~ "cs",
  data_on$birth_type_id == 1012900 ~ "cs",
  data_on$birth_type_id == 1012910 ~ "vaginal"
)

data_on$mode_del2 <- case_when(
  data_on$birth_type_id == 1012880 ~ "assisted vaginal",
  data_on$birth_type_id == 1012890 ~ "induced or spontaneous labour cs",
  data_on$birth_type_id == 1012900 ~ "no labour cs",
  data_on$birth_type_id == 1012910 ~ "spontaneous vaginal"
)

summary(as.factor(full_data_sansON$p_mode))
full_data_sansON$mode_del <- case_when(
  full_data_sansON$p_mode == 1 ~ "vaginal",
  full_data_sansON$p_mode == 2 ~ "cs"
)

# summary(as.factor(full_data_sansON$mode_del))
# summary(as.factor(data_on$mode_del))

## pregnancy outcome
# summary(as.factor(full_data_sansON$p_outcome))
# summary(as.factor(data_on$pregnancy_outcome_id))

data_on$preg_outcome <- case_when(
  data_on$pregnancy_outcome_id == 1021030 ~ "live birth",
  data_on$pregnancy_outcome_id == 1021040 ~ "loss",
  data_on$pregnancy_outcome_id == 1021050 ~ "loss",
  data_on$pregnancy_outcome_id == 1021070 ~ "stillbirth",
  data_on$pregnancy_outcome_id == 1021080 ~ "stillbirth",
  data_on$pregnancy_outcome_id == 1021090 ~ "stillbirth"
)

# summary(as.factor(full_data_sansON$p_outcome))

full_data_sansON$preg_outcome <- case_when(
  full_data_sansON$p_outcome == 1 ~ "loss",
  full_data_sansON$p_outcome == 2 ~ "stillbirth",
  full_data_sansON$p_outcome == 3 ~ "live birth"
)
# summary(as.factor(full_data_sansON$preg_outcome))

#multiple pregnancy
# summary(as.factor(full_data_sansON$r_num))
# summary(as.factor(data_on$BIS_num_fetuses_grp))

data_on$multiple_preg <- case_when(
  data_on$BIS_num_fetuses_grp == "multiple" ~ "multiple",
  data_on$BIS_num_fetuses_grp == "singleton" ~ "singleton"
)

full_data_sansON$multiple_preg <- case_when(
  full_data_sansON$r_num == 2 ~ "multiple",
  full_data_sansON$r_num == 1 ~ "singleton"
)

# summary(as.factor(full_data_sansON$multiple_preg))
# summary(as.factor(data_on$multiple_preg))

## previous preterm births

# summary(as.factor(data_on$prev_preterm_births))
# summary(as.factor(full_data_sansON$h_preterm))

full_data_sansON$h_preterm[which(full_data_sansON$h_preterm == 999)] <- NA

full_data_sansON$history_preterm <- case_when(
  full_data_sansON$h_preterm == 0 ~ FALSE,
  full_data_sansON$h_preterm > 0 ~ TRUE
)

# summary(as.factor(full_data_sansON$history_preterm))


data_on$history_preterm <- case_when(
  data_on$prev_preterm_births == 0 ~ FALSE,
  data_on$prev_preterm_births > 0 ~ TRUE
)

# summary(as.factor(data_on$history_preterm))

## previous stillbirths
# prev_stillbirths
# 
# summary(as.factor(data_on$prev_stillbirths))
# summary(as.factor(full_data_sansON$h_))

## labour type
data_on$labour_type <- case_when(
  data_on$birth_type_id == 1012880 ~ "induced",
  data_on$birth_type_id == 1012890 ~ "no labour",
  data_on$birth_type_id == 1012900 ~ "no labour",
  data_on$birth_type_id == 1012910 ~ "spontaneous"
)
# summary(as.factor(data_on$labour_type))


full_data_sansON$labour_type <- case_when(
  full_data_sansON$p_labour___1 == 1 ~ "no labour",
  full_data_sansON$p_labour___2 == 1 ~ "spontaneous",
  full_data_sansON$p_labour___3 == 1 ~ "induced"
)
# summary(as.factor(full_data_sansON$labour_type))

## apgar 5
# summary(as.factor(full_data_sansON$apgar5))
# summary(as.factor(data_on$B_APGAR_5))


data_on$apgar5 <- case_when(
  data_on$B_APGAR_5 == "7 or more" ~ "â‰¥7",
  data_on$B_APGAR_5 == "Less than 7" ~ "<7",
  data_on$B_APGAR_5 == "Missing" ~ NA
)
# summary(as.factor(data_on$apgar5))


## birthweight
# summary(as.factor(data_on$B_infant_weight))
# summary(as.factor(full_data_sansON$bw_cat))

data_on$bw_cat <- case_when(
  data_on$B_infant_weight == "< 2500g" ~ "<2500",
  data_on$B_infant_weight == "> 4000g" ~ ">4000",
  data_on$B_infant_weight == "2500 - 4000g" ~ "2500-4000"
)
# summary(as.factor(data_on$bw_cat))
# summary(as.factor(full_data_sansON$bw_cat))

## NICU
# summary(as.factor(data_on$NICU))
# summary(as.factor(full_data_sansON$NICU))
full_data_sansON$NICU[which(full_data_sansON$NICU == "Yes")] <- 1
full_data_sansON$NICU[which(full_data_sansON$NICU == "No")] <- 0

## vaccination status
# summary(as.factor(data_on))
# summary(as.factor(full_data_sansON$n_covid))
# summary(as.factor(full_data_sansON$n_covid_date))
# summary(as.factor(full_data_sansON$n_covid_date1))
# summary(as.factor(full_data_sansON$n_covid_date2))
# summary(as.factor(full_data_sansON$n_covid_date3))
# summary(as.factor(full_data_sansON$n_covid_date4))
# 

full_data_sansON$n_covid_date[which(full_data_sansON$n_covid_date == "9999-09-09")] <- NA
full_data_sansON$n_covid_date1[which(full_data_sansON$n_covid_date1 == "9999-09-09")] <- NA
full_data_sansON$n_covid_date2[which(full_data_sansON$n_covid_date2 == "9999-09-09")] <- NA
full_data_sansON$n_covid_date3[which(full_data_sansON$n_covid_date3 == "9999-09-09")] <- NA
full_data_sansON$n_covid_date4[which(full_data_sansON$n_covid_date4 == "9999-09-09")] <- NA


## ok so I don't actually know the difference between n_covid_date and n_covid_date1
## so the trick here will be to count the number of unique dates and that will be 
## the number of vaccines that the patient recieved. I will worry about 'during' pregnancy' or whatever later

full_data_sansON$number_of_vaccines <- NA

for(i in 1:length(full_data_sansON$number_of_vaccines)){
  vacc_date_list <- c(full_data_sansON$n_covid_date[i],full_data_sansON$n_covid_date1[i],full_data_sansON$n_covid_date2[i],full_data_sansON$n_covid_date3[i],full_data_sansON$n_covid_date4[i])
  if(length(which(is.na(vacc_date_list))) > 0){
    vacc_date_list <- vacc_date_list[-which(is.na(vacc_date_list))]
  }
  if(length(which(as.Date(vacc_date_list) > as.Date(full_data_sansON$e_diagnosis[i]))) > 0){
    vacc_date_list <- vacc_date_list[-which(as.Date(vacc_date_list) > as.Date(full_data_sansON$e_diagnosis[i]))]
  }
  full_data_sansON$number_of_vaccines[i] <- length(unique(vacc_date_list))
  if(length(unique(vacc_date_list)) == 0){
    if(is.na(full_data_sansON$n_covid[i])){
      full_data_sansON$number_of_vaccines[i] <- NA
    }
  }
}

# summary(as.factor(full_data_sansON$number_of_vaccines))

data_on$number_of_vaccines <- NA

for(i in 1:length(data_on$number_of_vaccines)){
  vacc_date_list <- c(data_on$H_Immunization_Date1[i],data_on$H_Immunization_Date2[i],data_on$H_Immunization_Date3[i])
  if(length(which(is.na(vacc_date_list))) > 0){
    vacc_date_list <- vacc_date_list[-which(is.na(vacc_date_list))]
  }
  if(length(which(vacc_date_list > as.Date(data_on$C_COVID_diagnosis_date[i]))) > 0){
    vacc_date_list <- vacc_date_list[-which(vacc_date_list > as.Date(data_on$C_COVID_diagnosis_date[i]))]
  }
  # print(length(unique(vacc_date_list)))
  data_on$number_of_vaccines[i] <- length(unique(vacc_date_list))
}

# summary(as.factor(data_on$number_of_vaccines))


## ok lets go for it
summary(as.factor(full_data_sansON$number_of_vaccines))
summary(as.factor(data_on$number_of_vaccines))

#small for gestational age
############################# make weight vectors #####################
### From: A New and Improved Population-Based Canadian Reference for Birth Weight for Gestational Age, Michael S. Kramer, Robert W. Platt, Shi Wu Wen, K.S. Joseph, Alexander Allen, Michal Abrahamowicz, BÃ©atrice Blondel, GÃ©rard BrÃ©art and for the Fetal/Infant Health Study Group of the Canadian Perinatal Surveillance System Pediatrics 2001;108;e35 DOI: 10.1542/peds.108.2.e35

ga.F <- seq(22, 43)
weight.F <- c(385, 450, 513, 578, 645, 717, 802, 903, 1022, 1168, 1346, 1548, 1768, 1998, 2227, 2452, 2658, 2825, 2955, 3051, 3114, 3159) # the threshold weight for 10th%ile for each ga above

weight.M <- c(401, 475, 547, 617, 686, 763, 853, 964, 1099, 1259, 1444, 1648, 1866, 2091, 2321, 2552, 2766, 2942, 3079, 3179, 3233, 3249) # the threshold weight for 10th%ile for each ga above

# if unknown go with male to be conservative
SGA <- data.frame(ga.F, weight.F, weight.M)

# need to create new column for ga_at_del by calculating it
# calculation referenced in REDCap is 40-(datediff([r_dob],[i_deliverydate_est], "d", "dmy", true)/7)
# started to do this within the csv but many errors...

full_data_sansON$r_dob <- as.Date(as.character(full_data_sansON$r_dob), format = "%Y-%m-%d")

full_data_sansON$r_dob <- replace(full_data_sansON$r_dob, which(full_data_sansON$r_dob == "9999-09-09"), NA)


# function to determine SGA
sga.fun <- function(dat){
  sga <- c()
  for(i in 1:dim(dat)[1]){
    
    if(is.na(dat$s_bw_gm[i])){
      sga <- c(sga, NA)
      next
    }
    
    if(is.na(dat$s_bw_gm[i]) & is.na(dat$s_pe_genitalia[i])){
      sga <- c(sga, NA)
      next
    }
    
    if(is.na(dat$ga_at_del[i])){
      sga <- c(sga, NA)
      next
    }
    
    if(dat$ga_at_del[i] < 22 | dat$ga_at_del[i] > 43){
      sga <- c(sga, NA)
      next
    }
    
    if(is.na(dat$s_pe_genitalia[i])){
      w.thresh <- SGA$weight.M[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
      sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
      next
    }
    
    if(dat$s_pe_genitalia[i] == 2){
      w.thresh <- SGA$weight.F[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
      sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
      next
    }
    
    if(dat$s_pe_genitalia[i] == 1){
      w.thresh <- SGA$weight.M[which(SGA$ga.F == floor(dat$ga_at_del[i]))]
      sga <- c(sga, ifelse(dat$s_bw_gm[i] >= w.thresh, "not SGA", "SGA"))
      next
    }
    sga <- c(sga,NA)
  }
  return(sga)
}

full_data_sansON$SGA <- sga.fun(full_data_sansON)



full_data_sansON$sga2 <- case_when(
  full_data_sansON$SGA == "not SGA" ~ 0,
  full_data_sansON$SGA == "SGA" ~ 1
)

## to do:
# gestational age / preterm birth - B_GA_birth
# gravidity - gravida
# asthma - 
# hospitalized, icu admission, oxygen - C_ICU_admission, C_hospitalized_COVID, H_Any_Other_Oxygen, H_High_Flow_Nasal_Oxygen_Therapy, oxygen_therapy_flag (infant)
# maternal death - H_Maternal_Death_COVID19, 
# mode of delivery - birth_type_id / birth_type_preg
# pregnancy outcome - pregnancy_outcome_id
# multiple pregnancy  - BIS_num_fetuses_grp
# number of previous preterm births - prev_preterm_births
# number of previous stillbirths - prev_stillbirths
# labour (induced v spontaneous v no labour) - birth_type_id / birth_type_preg
# infant outcomes:
# apgar ( 5 minutes) - B_APGAR_5
# birth weight - B_infant_weight
# NICU admission - NICU_admission_flag
# SGA (small for gestational age) - SGA10, SGA10, SGA3, SGA5
# vaccination status - H_Immunization_Date1, H_Immunization_Date2, H_Immunization_Date3, H_COVID_Vaccine_Name1, H_COVID_Vaccine_Name2, H_COVID_Vaccine_Name3
# vaccination status v preterm birth chi squared table
# plots (see report 6)



#### SUMMARISE BY PROVINCE AND DATE RANGE #####
full_data_sansON %>% group_by(prov) %>% filter(e_diagnosis < as.Date("2023-01-01")) 
%>% summarise(n = length(unique(a_record_id)), min_date = min(as.Date(e_diagnosis), na.rm = TRUE), max_date = max(as.Date(e_diagnosis), na.rm = TRUE))

# full_data_sansON_duplicated %>% group_by(prov) %>% summarise(n = length(a_record_id),
# min_date = min(as.Date(e_diagnosis), na.rm = TRUE), max_date = max(as.Date(e_diagnosis), na.rm = TRUE))
# full_data_sansON_duplicated$e_diagnosis <- as.Date(full_data_sansON_duplicated$e_diagnosis)

data_on %>% summarise(n=n(),min_date = min(as.Date(C_COVID_diagnosis_date), na.rm = TRUE), max_date = max(as.Date(C_COVID_diagnosis_date), na.rm = TRUE))


full_data_sansON$e_diagnosis <- as.Date(full_data_sansON$e_diagnosis)

full_data_sansON$a_date <- as.Date(full_data_sansON$a_date)
# data_on$C_COVID_diagnosis_date

data_on$prov <- "ON"

data_on$YEAR <- format(as.Date(data_on$C_COVID_diagnosis_date),"%Y")

data_on %>% group_by(YEAR) %>% summarise(n=n())


write.csv(full_data_sansON,"full_data_excludeON_20240325.csv") ## doesn't contain twins
write.csv(data_on,"full_data_ON_20240325.csv") ## contains twins
#











