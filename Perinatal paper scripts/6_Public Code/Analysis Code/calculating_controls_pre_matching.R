#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# RStudio Workbench is strictly for use by Public Health Scotland staff and     
# authorised users only, and is governed by the Acceptable Usage Policy https://github.com/Public-Health-Scotland/R-Resources/blob/master/posit_workbench_acceptable_use_policy.md.
#
# This is a shared resource and is hosted on a pay-as-you-go cloud computing
# platform.  Your usage will incur direct financial cost to Public Health
# Scotland.  As such, please ensure
#
#   1. that this session is appropriately sized with the minimum number of CPUs
#      and memory required for the size and scale of your analysis;
#   2. the code you write in this script is optimal and only writes out the
#      data required, nothing more.
#   3. you close this session when not in use; idle sessions still cost PHS
#      money!
#
# For further guidance, please see https://github.com/Public-Health-Scotland/R-Resources/blob/master/posit_workbench_best_practice_with_r.md.
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# used for reviewer comments and used to produce supplementary table 4 

# import pacakges
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
# library(hablar)
library(labelled)
library(purrr)
library(stringr)


# run 00 setup and load in cohort 1 for this. THis is to find out how many pregnancies went over 40+6 weeks
infected_over_40_weeks <- cohort1_infect_contemp_controls_singletons %>% 
  filter(gestation_at_outcome > 41) %>% 
  group_by(inf_or_uninf) %>% 
  tally()

vaccinated_over_40_weeks <- cohort1_vacc_contemp_controls_singletons %>% 
  filter(gestation_at_outcome > 41) %>% 
  group_by(vacc_or_unvacc) %>% 
  tally()

##### file paths #####
folder_working_data <- "/data/HPS/COPS_non_confi/COPS_VaccineSafety_Perinatal/2_Working_Data/"
folder_results <- "/data/HPS/COPS_non_confi/COPS_VaccineSafety_Perinatal/4_Results/"

# import data 

#66414
cohort1_uninfected <- readRDS(paste0(folder_working_data, "cohort1_uninfected.rds"))

cohort1_unvaccinated <- readRDS(paste0(folder_working_data, "cohort1_unvaccinated.rds"))

# we need to get the ids that we remove because they have the wrong date 
cohort1_infect_contemp_controls_singletons  <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort1_infection_secondary_contemporary_singletons.rds"))

# find all the ids that have a covid infection before study start date and remove them and the controls from dataset 
# but if they have a second infection within the start date, use that as the new index date
wrong_date_ids <- cohort1_infect_contemp_controls_singletons %>%
  mutate(wrong_date_flag = case_when(perinatal_covid_index_date < "2020-05-18" ~ 1)) %>%
  filter(wrong_date_flag == 1) %>%
  mutate(outcome_covid_index_date_new = case_when(difftime(index_date_covid_infection_1, est_conception_date) >= -42 &
                                                    index_date_covid_infection_1 <= perinatal_vaccination_timing_period_end & 
                                                    index_date_covid_infection_1 <= pregnancy_end_date & 
                                                    index_date_covid_infection_1 >= "2020-05-18" ~ index_date_covid_infection_1, 
                                                  difftime(index_date_covid_infection_2, est_conception_date) >= -42 &
                                                    index_date_covid_infection_2 <= perinatal_vaccination_timing_period_end & 
                                                    index_date_covid_infection_2 <= pregnancy_end_date & 
                                                    index_date_covid_infection_2 >= "2020-05-18" ~ index_date_covid_infection_2, 
                                                  difftime(index_date_covid_infection_3, est_conception_date) >= -42 &
                                                    index_date_covid_infection_3 <= perinatal_vaccination_timing_period_end & 
                                                    index_date_covid_infection_3 <= pregnancy_end_date &
                                                    index_date_covid_infection_3 >= "2020-05-18" ~ index_date_covid_infection_3, 
                                                  difftime(index_date_covid_infection_4, est_conception_date) >= -42 &
                                                    index_date_covid_infection_4 <= perinatal_vaccination_timing_period_end & 
                                                    index_date_covid_infection_4 <= pregnancy_end_date & 
                                                    index_date_covid_infection_4 >= "2020-05-18" ~ index_date_covid_infection_4))

ids_removed <- wrong_date_ids %>%
  filter(is.na(outcome_covid_index_date_new))
cohort1_removed_data <- cohort1_infect_contemp_controls_singletons %>% 
  filter((index %in% ids_removed$index)) 

cohort1_uninfected <- cohort1_uninfected %>%
  full_join(cohort1_removed_data)

rm(cohort1_infect_contemp_controls_singletons, cohort1_removed_data, wrong_date_ids, ids_removed)

# get characteristics of unexposed group 
uninfected_descriptives <- bind_rows(
  # n pregnancies 
  cohort1_uninfected %>%
    group_by(pregnancy_id_orig) %>% 
    slice(1) %>%
    ungroup %>%
    tally() %>%
    mutate(rowname = "n_pregnancies"), 
  
  cohort1_uninfected %>%
    tally() %>%
    mutate(rowname = "n_births"), 
  
  cohort1_uninfected %>% 
    summarise(median_age = median(mother_age_at_conception), 
              min_age = min(mother_age_at_conception), 
              max_age = max(mother_age_at_conception)) %>%
    pivot_longer(everything(), names_to = "rowname", values_to = "n"), 
  
  cohort1_uninfected %>%
    mutate(simd = case_when(simd == "1=most deprived" ~ "1", 
                            T ~ simd)) %>%
    group_by(simd) %>%
    tally() %>%
    mutate(rowname = paste0("deprivation:", simd)), 
  
  cohort1_uninfected %>%
    group_by(ethnicity_cat) %>%
    tally() %>%
    mutate(rowname = paste0("ethnicity:", ethnicity_cat)), 
  
  cohort1_uninfected %>%
    group_by(UR6_categories) %>%
    tally() %>%
    mutate(rowname = paste0("UR6_categories:", UR6_categories)),
  
  cohort1_uninfected %>%
    group_by(cv_clinical_vulnerability_category) %>%
    tally() %>%
    mutate(rowname = paste0("clinical_vulnerability:", cv_clinical_vulnerability_category)), 
  
  cohort1_uninfected %>%
    mutate(q_diag_diabetes_1 = case_when(!is.na(mother_eave_linkno) ~ replace_na(q_diag_diabetes_1, 0)), 
           q_diag_diabetes_2 = case_when(!is.na(mother_eave_linkno) ~ replace_na(q_diag_diabetes_2, 0))) %>%
    mutate(diabetes = case_when(diabetes == "unknown" & q_diag_diabetes_1 == 0 & q_diag_diabetes_2 == 0 ~ "assumed_no_diabetes",
                                T ~ diabetes)) %>%
    mutate(diabetes_cat = as.character(diabetes_cat), 
           diabetes_cat = case_when(diabetes_cat == "Unknown" & diabetes == "assumed_no_diabetes" ~ "No - assumed & confirmed", 
                                    T ~ diabetes_cat), 
           diabetes_cat = factor(diabetes_cat, levels = c("No - assumed & confirmed", 
                                                          "Pre-existing diabetes", 
                                                          "Gestational Diabetes/onset unknown", 
                                                          "Unknown"))) %>%
    group_by(diabetes_cat) %>%
    tally() %>%
    mutate(rowname = paste0("diabetes:", diabetes_cat)),
  
  cohort1_uninfected %>%
    mutate(smoking_status = case_when(is.na(x_overall_smoking_status) ~ "missing", 
                                      T ~ x_overall_smoking_status), 
           smoking_status = factor(smoking_status, levels = c("non-smoker", "ex-smoker", "smoker", "missing"))) %>%
    group_by(smoking_status) %>%
    tally() %>%
    mutate(rowname = paste0("smoking_status:", smoking_status)), 
  
  cohort1_uninfected %>%
    group_by(bmi_cat) %>%
    tally() %>%
    mutate(rowname = paste0("bmi:", bmi_cat)), 
  
  cohort1_uninfected %>%
    group_by(gestation_ascertainment) %>%
    tally() %>%
    mutate(rowname = paste0("imputed_gestation:", gestation_ascertainment)), 
  
  cohort1_uninfected %>%
    mutate(smr02_total_previous_pregnancies = ifelse(smr02_total_previous_pregnancies == 99, NA, smr02_total_previous_pregnancies), 
           smr02_previous_spontaneous_abortions = ifelse(smr02_previous_spontaneous_abortions == 99, NA, smr02_previous_spontaneous_abortions), 
           smr02_previous_theraputic_abortions = ifelse(smr02_previous_theraputic_abortions == 99, NA, smr02_previous_theraputic_abortions), 
           parity = smr02_total_previous_pregnancies - (smr02_previous_spontaneous_abortions + smr02_previous_theraputic_abortions), 
           parity_cat = case_when(parity == 0 ~ "0", 
                                  parity >= 1 ~"1+", 
                                  T ~ "Unknown/missing")) %>%
    group_by(parity_cat) %>%
    tally() %>%
    mutate(rowname = paste0("parity:", parity_cat))) %>%
  select(rowname, n) %>%
  separate(rowname, sep = ":", into = c("category", "sub_category")) %>%
  group_by(category) %>%
  mutate(percentage = round(as.numeric(n)/sum(as.numeric(n), na.rm = T)*100, 1))


unvaccinated_descriptives <- bind_rows(
  # n pregnancies 
  cohort1_unvaccinated %>%
    group_by(pregnancy_id_orig) %>% 
    slice(1) %>%
    ungroup %>%
    tally() %>%
    mutate(rowname = "n_pregnancies"), 
  
  cohort1_unvaccinated %>%
    tally() %>%
    mutate(rowname = "n_births"), 
  
  cohort1_unvaccinated %>% 
    summarise(median_age = median(mother_age_at_conception), 
              min_age = min(mother_age_at_conception), 
              max_age = max(mother_age_at_conception)) %>%
    pivot_longer(everything(), names_to = "rowname", values_to = "n"), 
  
  cohort1_unvaccinated %>%
    mutate(simd = case_when(simd == "1=most deprived" ~ "1", 
                            T ~ simd)) %>%
    group_by(simd) %>%
    tally() %>%
    mutate(rowname = paste0("deprivation:", simd)), 
  
  cohort1_unvaccinated %>%
    group_by(ethnicity_cat) %>%
    tally() %>%
    mutate(rowname = paste0("ethnicity:", ethnicity_cat)), 
  
  cohort1_unvaccinated %>%
    group_by(UR6_categories) %>%
    tally() %>%
    mutate(rowname = paste0("UR6_categories:", UR6_categories)),
  
  cohort1_unvaccinated %>%
    group_by(cv_clinical_vulnerability_category) %>%
    tally() %>%
    mutate(rowname = paste0("clinical_vulnerability:", cv_clinical_vulnerability_category)), 
  
  cohort1_unvaccinated %>%
    mutate(q_diag_diabetes_1 = case_when(!is.na(mother_eave_linkno) ~ replace_na(q_diag_diabetes_1, 0)), 
           q_diag_diabetes_2 = case_when(!is.na(mother_eave_linkno) ~ replace_na(q_diag_diabetes_2, 0))) %>%
    mutate(diabetes = case_when(diabetes == "unknown" & q_diag_diabetes_1 == 0 & q_diag_diabetes_2 == 0 ~ "assumed_no_diabetes",
                                T ~ diabetes)) %>%
    mutate(diabetes_cat = as.character(diabetes_cat), 
           diabetes_cat = case_when(diabetes_cat == "Unknown" & diabetes == "assumed_no_diabetes" ~ "No - assumed & confirmed", 
                                    T ~ diabetes_cat), 
           diabetes_cat = factor(diabetes_cat, levels = c("No - assumed & confirmed", 
                                                          "Pre-existing diabetes", 
                                                          "Gestational Diabetes/onset unknown", 
                                                          "Unknown"))) %>%
    group_by(diabetes_cat) %>%
    tally() %>%
    mutate(rowname = paste0("diabetes:", diabetes_cat)),
  
  cohort1_unvaccinated %>%
    mutate(smoking_status = case_when(is.na(x_overall_smoking_status) ~ "missing", 
                                      T ~ x_overall_smoking_status), 
           smoking_status = factor(smoking_status, levels = c("non-smoker", "ex-smoker", "smoker", "missing"))) %>%
    group_by(smoking_status) %>%
    tally() %>%
    mutate(rowname = paste0("smoking_status:", smoking_status)), 
  
  cohort1_unvaccinated %>%
    group_by(bmi_cat) %>%
    tally() %>%
    mutate(rowname = paste0("bmi:", bmi_cat)), 
  
  cohort1_unvaccinated %>%
    group_by(gestation_ascertainment) %>%
    tally() %>%
    mutate(rowname = paste0("imputed_gestation:", gestation_ascertainment)), 
  
  cohort1_unvaccinated %>%
    mutate(smr02_total_previous_pregnancies = ifelse(smr02_total_previous_pregnancies == 99, NA, smr02_total_previous_pregnancies), 
           smr02_previous_spontaneous_abortions = ifelse(smr02_previous_spontaneous_abortions == 99, NA, smr02_previous_spontaneous_abortions), 
           smr02_previous_theraputic_abortions = ifelse(smr02_previous_theraputic_abortions == 99, NA, smr02_previous_theraputic_abortions), 
           parity = smr02_total_previous_pregnancies - (smr02_previous_spontaneous_abortions + smr02_previous_theraputic_abortions), 
           parity_cat = case_when(parity == 0 ~ "0", 
                                  parity >= 1 ~"1+", 
                                  T ~ "Unknown/missing")) %>%
    group_by(parity_cat) %>%
    tally() %>%
    mutate(rowname = paste0("parity:", parity_cat))) %>%
  select(rowname, n) %>%
  separate(rowname, sep = ":", into = c("category", "sub_category")) %>%
  group_by(category) %>%
  mutate(percentage = round(as.numeric(n)/sum(as.numeric(n), na.rm = T)*100, 1))




