#######################################
###### MATERNAL OUTCOMES LINKAGE ######
#######################################

# link cohort with pregnancy outcomes data
# create 4 datasets linking pregnancy outcomes 
# - hypertensive disorders of pregnancy (smr01 and smr02)
# - venus thromboembolism (smr01 and smr02)
# - pregnancy-related bleeding (smr01 and smr02)
# - icu admission or death (any cause) - (icu and deaths)

# import packages
library(janitor)
library(dplyr)
library(tidyr)
library(forcats)
library(renv)
library(here)
library(odbc)
library(magrittr)
library(lubridate)
library(readr)
library(hablar)
library(labelled)
library(purrr)
library(stringr)
library(lubridate)

# file paths
folder_original_data <- ""
folder_working_data <- ""
folder_results <- ""

# import data #######
baby_level_data <- readRDS(paste0(folder_working_data, "baby_level_record_ready.rds"))
maternal_outcome_icu <- readRDS(paste0(folder_original_data, "ICU_admissions_for_cohort.rds"))
maternal_outcome_smr01 <- readRDS(paste0(folder_original_data, "SMR01_hospital_stays_for_cohort_update.rds"))
maternal_outcome_smr02 <- readRDS(paste0(folder_original_data, "SMR02_hospital_episodes_for_cohort_update.rds"))
maternal_outcomes_deaths <- readRDS(paste0(folder_original_data, "deaths_for_cohort.rds"))

mother_upis_pregnancy_window <- baby_level_data %>%
  select(mother_eave_linkno, pregnancy_id, est_conception_date, pregnancy_end_date) %>%
  group_by(mother_eave_linkno, pregnancy_id) %>% # ensuring only one pregnancy per row 
  slice(1)

maternal_study_onset <- baby_level_data %>%
  filter(pregnancy_end_date >= "2020-03-01" | is.na(pregnancy_end_date))

table(maternal_outcome_smr01$flag_hypertensive)
table(maternal_outcome_smr02$flag_hypertensive)

##### ICU ADMISSIONS AND DEATH COHORT ######## 
# filter the icu data to only include icu admissions that occur within the pregnancy time window of our cohort 
maternal_outcome_icu_during_pregnancy_plus_six <- maternal_outcome_icu %>% 
  inner_join(., mother_upis_pregnancy_window, by=c("EAVE_LINKNO" = "mother_eave_linkno")) %>%
  mutate(pregnancy_end_date_plus_six = pregnancy_end_date+weeks(6)) %>%
  rowwise() %>%
  mutate(icu_admission_during_pregnancy = case_when(between(admission_date, est_conception_date, pregnancy_end_date) ~ 1,
                                                    between(discharge_date, est_conception_date, pregnancy_end_date) ~ 1,
                                                    T ~ 0)) %>%
  mutate(icu_admission_during_pregnancy_plus_six = case_when(between(admission_date, est_conception_date, pregnancy_end_date_plus_six) ~ 1,
                                                             between(discharge_date, est_conception_date, pregnancy_end_date_plus_six) ~ 1,
                                                             T ~ 0)) %>%
  filter(icu_admission_during_pregnancy_plus_six == 1)

# make the data wide (one row per pregnancy)
icu_cols_to_keep = colnames(maternal_outcome_icu)
icu_cols_to_keep = icu_cols_to_keep[-1]
icu_cols_to_keep = icu_cols_to_keep[-3:-5]
icu_cols_to_keep = icu_cols_to_keep[-14:-15]

maternal_outcome_icu_pregnancy_plus_sixwide <- maternal_outcome_icu_during_pregnancy_plus_six %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(icu_episode_during_preg = 1, 
         icu_episode_during_preg = cumsum(icu_episode_during_preg), 
         no_icu_episodes_during_preg = max(icu_episode_during_preg), 
         icu_episode_during_preg = paste0("icu_admission_episode_during_preg_", icu_episode_during_preg)) %>%
  select(-icu_admission_during_pregnancy, -preg_status, -unit_outcome, -unit_outcome_derived, -data_source, -ep_num) %>%
  pivot_wider(names_from = icu_episode_during_preg, values_from = all_of(icu_cols_to_keep)) %>%
  ungroup()

icu_outcomes <- baby_level_data %>%
  full_join(maternal_outcome_icu_pregnancy_plus_sixwide, by = c("mother_eave_linkno" = "EAVE_LINKNO", "pregnancy_id", 
                                                                 "est_conception_date", "pregnancy_end_date")) %>%
  mutate(icu_outcome = case_when(no_icu_episodes_during_preg > 0 ~ 1, 
                                 is.na(no_icu_episodes_during_preg) ~ 0))

table(icu_outcomes$no_icu_episodes_during_preg)
table(icu_outcomes$icu_outcome) 

# filter deaths to only include deaths within pregnancy time window 
maternal_outcome_death_pregnancy <- maternal_outcomes_deaths %>%
  inner_join(., mother_upis_pregnancy_window, by=c("EAVE_LINKNO" = "mother_eave_linkno")) %>%
  mutate(pregnancy_end_date_plus_six = pregnancy_end_date+weeks(6)) %>%
  rowwise() %>%
  mutate(death_during_pregnancy = case_when(between(date_of_death, est_conception_date, pregnancy_end_date_plus_six) ~ 1,
                                            T ~ 0)) %>%
  filter(death_during_pregnancy == 1)

table(maternal_outcome_death_pregnancy$death_during_pregnancy)

# create death and icu outcome data
icu_and_death_outcome <- icu_outcomes %>%
  left_join(., maternal_outcome_death_pregnancy, by=c("mother_eave_linkno" = "EAVE_LINKNO", "pregnancy_id", 
                                                      "est_conception_date", "pregnancy_end_date")) %>%
  mutate(death_outcome = case_when(death_during_pregnancy == 1 ~ 1, 
                                   is.na(death_during_pregnancy) ~ NA), 
         icu_and_death_outcome = case_when(death_outcome == 1 ~ 1, 
                                           icu_outcome == 1 ~ 1, 
                                           T ~ NA))


####### GET ICU DIAGNOSES TO ENSURE WE FLAG WOMEN WHO APPEAR IN SIG SAG BUT NOT SMR01/SMR02 ##########

icu_diagnoses <- maternal_outcome_icu_during_pregnancy_plus_six %>%
  select(ap_diag:oth_diag6)

diag1 <- unique(icu_diagnoses$ap_diag)
diag2 <- unique(icu_diagnoses$corr_apii_diag)
diag3 <- unique(icu_diagnoses$oth_diag1)
diag4 <- unique(icu_diagnoses$oth_diag2)
diag5 <- unique(icu_diagnoses$oth_diag3)
diag6 <- unique(icu_diagnoses$oth_diag4)
diag7 <- unique(icu_diagnoses$oth_diag5)
diag8 <- unique(icu_diagnoses$oth_diag6)

diagnoses <- append(diag1, diag2)
diagnoses <- append(diagnoses, diag3)
diagnoses <- append(diagnoses, diag4)
diagnoses <- append(diagnoses, diag5)
diagnoses <- append(diagnoses, diag6)
diagnoses <- append(diagnoses, diag7)
diagnoses <- append(diagnoses, diag8)

diagnoses <- as.data.frame(unique(diagnoses)) %>%
  rename(diagnosis = "unique(diagnoses)") %>%
  arrange(diagnosis)

# write.csv(diagnoses, paste0(folder_results, "icu_diagnoses.csv"))

hypertension_codes_specific <- c("Eclampsia", "HELLP syndrome", "Pre-eclampsia", "Pre-eclampsia/eclampsia", "Pregnancy-induced hypertension")
hypertension_codes_broad <- c(hypertension_codes_specific, "Hypertension", "Other hypertension")
vte_codes <- c("Pulmonar embolus", "Pulmonary embolism", "Pulmonary embolus", "Pulmonary thromboembolism",
             "Venous thrombosis (including DVT)", "Thrombotic disorders")
ob_haem_codes <- c("Antepartum haemorrhage", "Peripartum haemorrhage", "Postpartum haemorrhage", "Hypovolaemic/haemorrhagic shock", 
                   "Hysterectomy", "Bleeding-laceration/tear")
early_preg_bleeding_codes <- c("Ectopic pregnancy")
dissem_int_coag_code <- c("Disseminated intravascular coagulation")
bleeding_codes <- c(ob_haem_codes, early_preg_bleeding_codes, dissem_int_coag_code)

maternal_outcomes_icu_diagnoses <- maternal_outcome_icu_during_pregnancy_plus_six %>%
  mutate(flag_obs_haem = case_when(if_any(contains("diag"), ~. %in% ob_haem_codes) ~ TRUE, 
                                    T ~ FALSE), 
         flag_early_preg_bleeding = case_when(if_any(contains("diag"), ~. %in% early_preg_bleeding_codes) ~ TRUE, 
                                              T ~ FALSE), 
         flag_dissem_intravascular_coag = case_when(if_any(contains("diag"), ~. %in% dissem_int_coag_code) ~ TRUE, 
                                                    T ~ FALSE), 
         flag_any_bleeding = case_when(if_any(contains("diag"), ~ . %in% bleeding_codes) ~ TRUE,
                                       T ~ FALSE), 
         flag_vte = case_when(if_any(contains("diag"), ~ . %in% vte_codes) ~ TRUE,
                              T ~ FALSE), 
         flag_hypertensive =  case_when(if_any(contains("diag"), ~ . %in% hypertension_codes_specific) ~ TRUE,
                                        T ~ FALSE), 
         flag_hypertension_broad = case_when(if_any(contains("diag"), ~ . %in% hypertension_codes_broad) ~ TRUE,
                                             T ~ FALSE))

bleeding_outcome_icu <- maternal_outcomes_icu_diagnoses %>% 
  filter(flag_any_bleeding == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_obs_haem, flag_early_preg_bleeding, flag_dissem_intravascular_coag, 
         flag_any_bleeding, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, icu_admission_during_pregnancy_plus_six) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(icu_episode_during_preg = 1, 
         icu_episode_during_preg = cumsum(icu_episode_during_preg), 
         no_icu_during_preg_bleeding = max(icu_episode_during_preg)) %>%
  filter(icu_episode_during_preg == 1) %>%
  mutate(icu_episode_during_preg = paste0("icu_bleeding_", icu_episode_during_preg)) %>%
  pivot_wider(names_from = icu_episode_during_preg, values_from = c(admission_date, discharge_date, flag_obs_haem, 
                                                                          flag_early_preg_bleeding, flag_dissem_intravascular_coag, 
                                                                          flag_any_bleeding)) %>%
  ungroup() 

hypertension_outcome_icu <- maternal_outcomes_icu_diagnoses %>% 
  filter(flag_hypertensive == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_hypertensive, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, icu_admission_during_pregnancy_plus_six) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(icu_episode_during_preg = 1, 
         icu_episode_during_preg = cumsum(icu_episode_during_preg), 
         no_icu_during_preg_bleeding = max(icu_episode_during_preg)) %>%
  filter(icu_episode_during_preg == 1) %>%
  mutate(icu_episode_during_preg = paste0("icu_hypertension_", icu_episode_during_preg)) %>%
  pivot_wider(names_from = icu_episode_during_preg, values_from = c(admission_date, discharge_date, 
                                                                          flag_hypertensive)) %>%
  ungroup() 


hypertension_broad_outcome_icu <- maternal_outcomes_icu_diagnoses %>% 
  filter(flag_hypertension_broad == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_hypertension_broad, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, icu_admission_during_pregnancy_plus_six) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(icu_episode_during_preg = 1, 
         icu_episode_during_preg = cumsum(icu_episode_during_preg), 
         no_icu_during_preg_bleeding = max(icu_episode_during_preg)) %>%
  filter(icu_episode_during_preg == 1) %>%
  mutate(icu_episode_during_preg = paste0("icu_hypertension_broad_", icu_episode_during_preg)) %>%
  pivot_wider(names_from = icu_episode_during_preg, values_from = c(admission_date, discharge_date, 
                                                                    flag_hypertension_broad)) %>%
  ungroup() 

vte_outcome_icu <- maternal_outcomes_icu_diagnoses %>% 
  filter(flag_vte == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_vte, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, icu_admission_during_pregnancy_plus_six) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(icu_episode_during_preg = 1, 
         icu_episode_during_preg = cumsum(icu_episode_during_preg), 
         no_icu_during_preg_bleeding = max(icu_episode_during_preg)) %>%
  filter(icu_episode_during_preg == 1) %>%
  mutate(icu_episode_during_preg = paste0("icu_vte_", icu_episode_during_preg)) %>%
  pivot_wider(names_from = icu_episode_during_preg, values_from = c(admission_date, discharge_date, 
                                                                          flag_vte)) %>%
  ungroup() 


maternal_outcome_icu_pregnancy_wide <- bleeding_outcome_icu %>%
  full_join(., hypertension_outcome_icu, by = c("EAVE_LINKNO", "pregnancy_id", "est_conception_date", 
                                                  "pregnancy_end_date", "pregnancy_end_date_plus_six", 
                                                  "icu_admission_during_pregnancy_plus_six")) %>%
  full_join(., hypertension_broad_outcome_icu, by = c("EAVE_LINKNO", "pregnancy_id", "est_conception_date", 
                                                "pregnancy_end_date", "pregnancy_end_date_plus_six", 
                                                "icu_admission_during_pregnancy_plus_six")) %>%
  full_join(., vte_outcome_icu, by = c("EAVE_LINKNO", "pregnancy_id", "est_conception_date", 
                                         "pregnancy_end_date", "pregnancy_end_date_plus_six", 
                                         "icu_admission_during_pregnancy_plus_six"))




####### SMR01 & SMR02 DATA #########

smr01_colnames <- colnames(maternal_outcome_smr01)
smr01_colnames <- smr01_colnames[-1]

study_end = as.Date("2021-05-01")
study_start = as.Date("2020-03-01")

# join pregnancy data to smr01 data 
maternal_outcome_smr01_pregnancy <- maternal_outcome_smr01 %>% 
  inner_join(., mother_upis_pregnancy_window, by = c("EAVE_LINKNO" = "mother_eave_linkno")) %>% 
  # mutate(preg_end = case_when(is.na(pregnancy_end_date) ~ study_end, 
  #                             T ~ pregnancy_end_date)) %>%
  mutate(pregnancy_end_date_plus_six = pregnancy_end_date+weeks(6)) %>%
  rowwise() %>%
  mutate(hospital_during_pregnancy = case_when(between(admission_date, est_conception_date, pregnancy_end_date_plus_six) ~ 1,
                                               between(discharge_date, est_conception_date, pregnancy_end_date_plus_six) ~ 1,
                                               T ~ 0)) %>%
  filter(hospital_during_pregnancy == 1) %>%
  filter(if_any(starts_with("flag_"))) %>%
  rename(flag_hypertension_broad = "flag_hypertensive_inc_all")

table(maternal_outcome_smr01_pregnancy$flag_hypertensive)

bleeding_outcome_smr01 <- maternal_outcome_smr01_pregnancy %>% 
  filter(flag_any_bleeding == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_obs_haem, flag_early_preg_bleeding, flag_dissem_intravascular_coag, 
         flag_any_bleeding, early_preg_bleeding_codes, obs_haem_codes, dissem_intravascular_coag_codes, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, hospital_during_pregnancy) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(hosp_stay_episode_during_preg = 1, 
         hosp_stay_episode_during_preg = cumsum(hosp_stay_episode_during_preg), 
         no_hosp_stay_episode_during_preg_bleeding = max(hosp_stay_episode_during_preg)) %>%
  filter(hosp_stay_episode_during_preg == 1) %>%
  mutate(hosp_stay_episode_during_preg = paste0("smr01_bleeding_", hosp_stay_episode_during_preg)) %>%
  pivot_wider(names_from = hosp_stay_episode_during_preg, values_from = c(admission_date, discharge_date, flag_obs_haem, 
                                                                          flag_early_preg_bleeding, flag_dissem_intravascular_coag, 
                                                                          flag_any_bleeding)) %>%
  ungroup() 

hypertension_outcome_smr01 <- maternal_outcome_smr01_pregnancy %>% 
  filter(flag_hypertensive == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_hypertensive, hypertensive_codes, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, hospital_during_pregnancy) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(hosp_stay_episode_during_preg = 1, 
         hosp_stay_episode_during_preg = cumsum(hosp_stay_episode_during_preg), 
         no_hosp_stay_episode_during_preg_hypertension = max(hosp_stay_episode_during_preg)) %>%
  filter(hosp_stay_episode_during_preg == 1) %>%
  mutate(hosp_stay_episode_during_preg = paste0("smr01_hypertension_", hosp_stay_episode_during_preg)) %>%
  pivot_wider(names_from = hosp_stay_episode_during_preg, values_from = c(admission_date, discharge_date, 
                                                                          flag_hypertensive)) %>%
  ungroup() 

hypertension_broad_outcome_smr01 <- maternal_outcome_smr01_pregnancy %>% 
  filter(flag_hypertension_broad == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_hypertension_broad, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, hospital_during_pregnancy) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(hosp_stay_episode_during_preg = 1, 
         hosp_stay_episode_during_preg = cumsum(hosp_stay_episode_during_preg), 
         no_hosp_stay_episode_during_preg_hypertension = max(hosp_stay_episode_during_preg)) %>%
  filter(hosp_stay_episode_during_preg == 1) %>%
  mutate(hosp_stay_episode_during_preg = paste0("smr01_hypertension_broad_", hosp_stay_episode_during_preg)) %>%
  pivot_wider(names_from = hosp_stay_episode_during_preg, values_from = c(admission_date, discharge_date, 
                                                                          flag_hypertension_broad)) %>%
  ungroup() 

vte_outcome_smr01 <- maternal_outcome_smr01_pregnancy %>% 
  filter(flag_vte == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_vte, vte_codes, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, hospital_during_pregnancy) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(hosp_stay_episode_during_preg = 1, 
         hosp_stay_episode_during_preg = cumsum(hosp_stay_episode_during_preg), 
         no_hosp_stay_episode_during_preg_vte = max(hosp_stay_episode_during_preg)) %>%
  filter(hosp_stay_episode_during_preg == 1) %>%
  mutate(hosp_stay_episode_during_preg = paste0("smr01_vte_", hosp_stay_episode_during_preg)) %>%
  pivot_wider(names_from = hosp_stay_episode_during_preg, values_from = c(admission_date, discharge_date, 
                                                                          flag_vte)) %>%
  ungroup() 

maternal_outcome_smr01_pregnancy_wide <- bleeding_outcome_smr01 %>%
  full_join(., hypertension_outcome_smr01, by = c("EAVE_LINKNO", "pregnancy_id", "est_conception_date", 
                                                  "pregnancy_end_date", "pregnancy_end_date_plus_six", 
                                                  "hospital_during_pregnancy")) %>%
  full_join(., hypertension_broad_outcome_smr01, by = c("EAVE_LINKNO", "pregnancy_id", "est_conception_date", 
                                                  "pregnancy_end_date", "pregnancy_end_date_plus_six", 
                                                  "hospital_during_pregnancy")) %>%
  full_join(., vte_outcome_smr01, by = c("EAVE_LINKNO", "pregnancy_id", "est_conception_date", 
                                         "pregnancy_end_date", "pregnancy_end_date_plus_six", 
                                         "hospital_during_pregnancy"))

# maternal_outcome_smr01_pregnancy_wide <- maternal_outcome_smr01_pregnancy %>%
#   group_by(pregnancy_id) %>%
#   arrange(admission_date) %>%
#   mutate(hosp_stay_episode_during_preg = 1, 
#          hosp_stay_episode_during_preg = cumsum(hosp_stay_episode_during_preg), 
#          no_hosp_stay_episode_during_preg = max(hosp_stay_episode_during_preg)) %>%
#   filter(hosp_stay_episode_during_preg == 1) %>%
#   mutate(hosp_stay_episode_during_preg = paste0("hosp_stay_during_preg_", hosp_stay_episode_during_preg)) %>%
#   pivot_wider(names_from = hosp_stay_episode_during_preg, values_from = smr01_colnames) %>%
#   ungroup() 

maternal_outcome_smr02_pregnancy <- maternal_outcome_smr02 %>% 
  inner_join(., mother_upis_pregnancy_window, by = c("EAVE_LINKNO" = "mother_eave_linkno")) %>% 
  mutate(preg_end = case_when(is.na(pregnancy_end_date) ~ study_end, 
                              T ~ pregnancy_end_date)) %>%
  mutate(pregnancy_end_date_plus_six = pregnancy_end_date+weeks(6)) %>%
  rowwise() %>%
  mutate(episode_during_pregnancy = case_when(between(admission_date, est_conception_date, pregnancy_end_date_plus_six) ~ 1,
                                               between(discharge_date, est_conception_date, pregnancy_end_date_plus_six) ~ 1,
                                               T ~ 0)) %>%
  filter(episode_during_pregnancy == 1) %>%
  filter(if_any(starts_with("flag_"))) %>%
  rename(flag_hypertension_broad = "flag_hypertensive_inc_all")

table(maternal_outcome_smr02_pregnancy$flag_hypertensive)

bleeding_outcome_smr02 <- maternal_outcome_smr02_pregnancy %>% 
  filter(flag_any_bleeding == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_obs_haem, flag_early_preg_bleeding, flag_dissem_intravascular_coag, 
         flag_any_bleeding, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, episode_during_pregnancy) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(hosp_episode_during_preg = 1, 
         hosp_episode_during_preg = cumsum(hosp_episode_during_preg), 
         no_hosp_episode_during_preg_bleeding = max(hosp_episode_during_preg)) %>%
  filter(hosp_episode_during_preg == 1) %>%
  mutate(hosp_episode_during_preg = paste0("smr02_bleeding_", hosp_episode_during_preg)) %>%
  pivot_wider(names_from = hosp_episode_during_preg, values_from = c(admission_date, discharge_date, flag_obs_haem, 
                                                                          flag_early_preg_bleeding, flag_dissem_intravascular_coag, 
                                                                          flag_any_bleeding)) %>%
  ungroup() 

hypertension_outcome_smr02 <- maternal_outcome_smr02_pregnancy %>% 
  filter(flag_hypertensive == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_hypertensive, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, episode_during_pregnancy) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(hosp_episode_during_preg = 1, 
         hosp_episode_during_preg = cumsum(hosp_episode_during_preg), 
         no_hosp_episode_during_preg_hypertension = max(hosp_episode_during_preg)) %>%
  filter(hosp_episode_during_preg == 1) %>%
  mutate(hosp_episode_during_preg = paste0("smr02_hypertension_", hosp_episode_during_preg)) %>%
  pivot_wider(names_from = hosp_episode_during_preg, values_from = c(admission_date, discharge_date, 
                                                                          flag_hypertensive)) %>%
  ungroup() 

table(hypertension_outcome_smr02$flag_hypertensive_smr02_hypertension_1)

hypertension_broad_outcome_smr02 <- maternal_outcome_smr02_pregnancy %>% 
  filter(flag_hypertension_broad == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_hypertension_broad, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, episode_during_pregnancy) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(hosp_episode_during_preg = 1, 
         hosp_episode_during_preg = cumsum(hosp_episode_during_preg), 
         no_hosp_episode_during_preg_hypertension = max(hosp_episode_during_preg)) %>%
  filter(hosp_episode_during_preg == 1) %>%
  mutate(hosp_episode_during_preg = paste0("smr02_hypertension_broad_", hosp_episode_during_preg)) %>%
  pivot_wider(names_from = hosp_episode_during_preg, values_from = c(admission_date, discharge_date, 
                                                                     flag_hypertension_broad)) %>%
  ungroup() 


# test <- hypertension_outcome_smr02 %>% filter(pregnancy_end_date_plus_six >= study_start | is.na(pregnancy_end_date))

vte_outcome_smr02 <- maternal_outcome_smr02_pregnancy %>% 
  filter(flag_vte == TRUE) %>%
  select(EAVE_LINKNO, admission_date, discharge_date, flag_vte, pregnancy_id, 
         est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, episode_during_pregnancy) %>%
  group_by(pregnancy_id) %>%
  arrange(admission_date) %>%
  mutate(hosp_episode_during_preg = 1, 
         hosp_episode_during_preg = cumsum(hosp_episode_during_preg), 
         no_hosp_episode_during_preg_vte = max(hosp_episode_during_preg)) %>%
  filter(hosp_episode_during_preg == 1) %>%
  mutate(hosp_episode_during_preg = paste0("smr02_vte_", hosp_episode_during_preg)) %>%
  pivot_wider(names_from = hosp_episode_during_preg, values_from = c(admission_date, discharge_date, 
                                                                          flag_vte)) %>%
  ungroup() 

maternal_outcome_smr02_pregnancy_wide <- bleeding_outcome_smr02 %>%
  full_join(., hypertension_outcome_smr02, by = c("EAVE_LINKNO", "pregnancy_id", "est_conception_date", 
                                                  "pregnancy_end_date", "pregnancy_end_date_plus_six", 
                                                  "episode_during_pregnancy")) %>%
  full_join(., hypertension_broad_outcome_smr02, by = c("EAVE_LINKNO", "pregnancy_id", "est_conception_date", 
                                                  "pregnancy_end_date", "pregnancy_end_date_plus_six", 
                                                  "episode_during_pregnancy")) %>%
  full_join(., vte_outcome_smr02, by = c("EAVE_LINKNO", "pregnancy_id", "est_conception_date", 
                                         "pregnancy_end_date", "pregnancy_end_date_plus_six", 
                                         "episode_during_pregnancy"))

table(maternal_outcome_smr02_pregnancy_wide$flag_hypertensive_smr02_hypertension_1)

# maternal_outcome_smr02_pregnancy_wide <- maternal_outcome_smr02_pregnancy %>%
#   group_by(pregnancy_id) %>%
#   arrange(admission_date) %>%
#   mutate(hosp_episode_during_preg = 1, 
#          hosp_episode_during_preg = cumsum(hosp_episode_during_preg), 
#          no_hosp_episode_during_preg = max(hosp_episode_during_preg)) %>%
#   filter(hosp_episode_during_preg == 1) %>%
#   mutate(hosp_episode_during_preg = paste0("hosp_episode_during_preg_", hosp_episode_during_preg)) %>%
#   pivot_wider(names_from = hosp_episode_during_preg, values_from = smr01_colnames) %>%
#   ungroup() 

maternal_outcomes_smr <- full_join(maternal_outcome_smr01_pregnancy_wide, 
                          maternal_outcome_smr02_pregnancy_wide) %>%
  full_join(., maternal_outcome_icu_pregnancy_wide) %>%
  mutate(hypertension_outcome = case_when(if_any(starts_with("flag_hypertensive")) ~ 1), 
         hypertension_broad_outcome = case_when(if_any(starts_with("flag_hypertension_broad")) ~ 1), 
         vte_outcome = case_when(if_any(starts_with("flag_vte")) ~ 1), 
         obs_haem_outcome = case_when(if_any(starts_with("flag_obs_haem")) ~ 1), 
         early_preg_bleeding_outcome = case_when(if_any(starts_with("flag_early_preg_bleeding")) ~ 1), 
         dissem_intravascular_coag_outcome = case_when(if_any(starts_with("flag_dissem_intravascular_coag")) ~ 1),
         any_bleeding_outcome = case_when(if_any(starts_with("flag_any_bleeding")) ~ 1)) %>%
  mutate(hypertension_admission_date = case_when(hypertension_outcome == 1 ~ pmin(admission_date_smr01_hypertension_1, admission_date_smr02_hypertension_1,
                                                                                  admission_date_icu_hypertension_1, 
                                                                                  na.rm = TRUE)), 
         hypertension_broad_admission_date = case_when(hypertension_broad_outcome == 1 ~ pmin(admission_date_smr01_hypertension_broad_1, admission_date_smr02_hypertension_broad_1,
                                                                                              admission_date_icu_hypertension_broad_1, 
                                                                                              na.rm = TRUE)), 
         vte_admission_date = case_when(vte_outcome == 1 ~ pmin(admission_date_smr01_vte_1, admission_date_smr02_vte_1,admission_date_icu_vte_1,
                                                                na.rm = TRUE)), 
         any_bleeding_admission_date = case_when(any_bleeding_outcome == 1 ~ pmin(admission_date_smr01_bleeding_1, admission_date_smr02_bleeding_1,
                                                                                  admission_date_icu_bleeding_1, 
                                                                                  na.rm = TRUE))) %>% 
  select(EAVE_LINKNO, pregnancy_id, est_conception_date, pregnancy_end_date, pregnancy_end_date_plus_six, hypertension_outcome, 
         hypertension_broad_outcome, vte_outcome, obs_haem_outcome, early_preg_bleeding_outcome, dissem_intravascular_coag_outcome, 
         any_bleeding_outcome, hypertension_admission_date, hypertension_broad_admission_date, vte_admission_date, any_bleeding_admission_date)


table(maternal_outcomes_smr$hypertension_outcome)
table(maternal_outcomes_smr$hypertension_broad_outcome)


smr_icu_outcomes <- baby_level_data %>%
  full_join(maternal_outcomes_smr, by = c("mother_eave_linkno" = "EAVE_LINKNO", "pregnancy_id", 
                                          "est_conception_date", "pregnancy_end_date")) 

# maternal_outcomes_smr_and_icu <- maternal_outcomes_smr %>%
#   full_join(maternal_outcomes_icu_diagnoses, by = c("EAVE_LINKNO", "pregnancy_id", 
#                                                     "est_conception_date", "pregnancy_end_date"))
# 
# smr_and_icu_outcomes <- baby_level_data %>%
#   full_join(maternal_outcomes_smr_and_icu, by = c("mother_eave_linkno" = "EAVE_LINKNO", "pregnancy_id", 
#                                                   "est_conception_date", "pregnancy_end_date")) %>%
#   mutate(hypertension_outcome = case_when(if_any(starts_with("flag_hypertensive")) ~ 1, 
#                                           T ~ 0), 
#          vte_outcome = case_when(if_any(starts_with("flag_vte")) ~ 1, 
#                                  T ~ 0), 
#          obs_haem_outcome = case_when(if_any(starts_with("flag_obs_haem")) ~ 1, 
#                                       T ~ 0), 
#          early_preg_bleeding_outcome = case_when(if_any(starts_with("flag_early_preg_bleeding")) ~ 1, 
#                                                  T ~ 0), 
#          dissem_intravascular_coag_outcome = case_when(if_any(starts_with("flag_dissem_intravascular_coag")) ~ 1, 
#                                                        T ~ 0),
#          any_bleeding_outcome = case_when(if_any(starts_with("flag_any_bleeding")) ~ 1,
#                                           T ~ 0))
# 
# ##### does adding sigsag change smr ######
# 
# smr_numbers <- full_join(
#   
#   smr_outcomes %>%
#     summarise(bleeding = sum(any_bleeding_outcome), 
#               vte = sum(vte_outcome), 
#               hypertension = sum(hypertension_outcome)) %>%
#     pivot_longer(everything(), names_to = "maternal_outcome", values_to = "smr_only"), 
#   
#   smr_and_icu_outcomes %>%
#     summarise(bleeding = sum(any_bleeding_outcome), 
#               vte = sum(vte_outcome), 
#               hypertension = sum(hypertension_outcome)) %>%
#     pivot_longer(everything(), names_to = "maternal_outcome", values_to = "smr_and_icu")
#   
# )
# 


##### save out linked data #####

saveRDS(icu_and_death_outcome, paste0(folder_working_data, "icu_and_death_outcome_ready.rds"))
saveRDS(smr_icu_outcomes, paste0(folder_working_data, "smr_icu_outcomes_ready.rds"))



smr <- readRDS(paste0(folder_working_data, "smr_outcomes_ready.rds"))

table(smr$hypertension_outcome)
