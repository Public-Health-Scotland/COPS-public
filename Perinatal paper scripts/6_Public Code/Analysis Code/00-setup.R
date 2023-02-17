###################
###### SETUP ######
###################


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

##### file paths #####
folder_working_data <- ""
folder_results <- ""
cops_perinatal_conf_folder <- file.path("") # has ids of missing delivery onset within preterm births cohort 

#### FUNCTIONS #####

# data fixes # 

fix_data <- function(data) { 
  
  data <- data %>% 
    select(-c(booking_smoking_status, gp_smoking_status, overall_smoking_status)) %>%
    mutate(smoking_status = case_when(is.na(x_overall_smoking_status) ~ "missing", 
                                      T ~ x_overall_smoking_status), 
           smoking_status = factor(smoking_status, levels = c("non-smoker", "ex-smoker", "smoker", "missing"))) %>%
    mutate(simd = case_when(simd == "1=most deprived" ~ "1", 
                            simd == "5=least deprived" ~ "5",
                            T ~ simd), 
           simd = as.factor(simd)) %>%
    mutate(apgar_score = if_else(smr02_apgar_5_minutes == "NR" | smr02_apgar_5_minutes == "RR", NA_character_, smr02_apgar_5_minutes), 
           apgar_score = str_remove(apgar_score, " "), 
           apgar_score = as.numeric(apgar_score)) %>%
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
                                                          "Unknown")), 
           smr02_onset_of_delivery_process = case_when(is.na(smr02_onset_of_delivery_process) ~ "Unknown/Missing", 
                                                       T~smr02_onset_of_delivery_process)) %>%
    mutate(smr02_total_previous_pregnancies = ifelse(smr02_total_previous_pregnancies == 99, NA, smr02_total_previous_pregnancies), 
           smr02_previous_spontaneous_abortions = ifelse(smr02_previous_spontaneous_abortions == 99, NA, smr02_previous_spontaneous_abortions), 
           smr02_previous_theraputic_abortions = ifelse(smr02_previous_theraputic_abortions == 99, NA, smr02_previous_theraputic_abortions), 
           parity = smr02_total_previous_pregnancies - (smr02_previous_spontaneous_abortions + smr02_previous_theraputic_abortions), 
           parity_cat = case_when(parity == 0 ~ "0", 
                                  parity >= 1 ~"1+", 
                                  T ~ "Unknown/missing"))  %>%
    arrange(index) %>%
    group_by(index) %>%
    mutate(vaccination_subgroup = first(perinatal_type_of_vaccination_during_period)) %>% 
    ungroup()
  
  return(data)
  
}


fix_wrong_dates <- function(data, cohort = "1") {
  
  if(cohort == "3") {
    
    data <- data %>%
      mutate(outcome_covid_index_date = ptb_covid_index_date, 
             outcome_exposure_end = ptb_vaccination_timing_period_end)
    
  } else if(cohort == "4") {
    
    data <- data %>%
      mutate(outcome_covid_index_date = vptb_covid_index_date, 
             outcome_exposure_end = vptb_vaccination_timing_period_end)

  } else {
    
    data <- data %>%
      mutate(outcome_covid_index_date = perinatal_covid_index_date, 
             outcome_exposure_end = perinatal_vaccination_timing_period_end)
    
  }
  
  # find all the ids that have a covid infection before study start date and remove them and the controls from dataset 
  # but if they have a second infection within the start date, use that as the new index date
  wrong_date_ids <- data %>%
    mutate(wrong_date_flag = case_when(outcome_covid_index_date < "2020-05-18" ~ 1)) %>%
    filter(wrong_date_flag == 1) %>%
    mutate(outcome_covid_index_date_new = case_when(difftime(index_date_covid_infection_1, est_conception_date) >= -42 &
                                                      index_date_covid_infection_1 <= outcome_exposure_end & 
                                                      index_date_covid_infection_1 <= pregnancy_end_date & 
                                                      index_date_covid_infection_1 >= "2020-05-18" ~ index_date_covid_infection_1, 
                                                    difftime(index_date_covid_infection_2, est_conception_date) >= -42 &
                                                      index_date_covid_infection_2 <= outcome_exposure_end & 
                                                      index_date_covid_infection_2 <= pregnancy_end_date & 
                                                      index_date_covid_infection_2 >= "2020-05-18" ~ index_date_covid_infection_2, 
                                                    difftime(index_date_covid_infection_3, est_conception_date) >= -42 &
                                                      index_date_covid_infection_3 <= outcome_exposure_end & 
                                                      index_date_covid_infection_3 <= pregnancy_end_date &
                                                      index_date_covid_infection_3 >= "2020-05-18" ~ index_date_covid_infection_3, 
                                                    difftime(index_date_covid_infection_4, est_conception_date) >= -42 &
                                                      index_date_covid_infection_4 <= outcome_exposure_end & 
                                                      index_date_covid_infection_4 <= pregnancy_end_date & 
                                                      index_date_covid_infection_4 >= "2020-05-18" ~ index_date_covid_infection_4))
  
  ids_to_remove <- wrong_date_ids %>%
    filter(is.na(outcome_covid_index_date_new))
  
  ids_to_keep <- wrong_date_ids %>%
    filter(!(is.na(outcome_covid_index_date_new)))
  
  data <- data %>% 
    filter(!(index %in% ids_to_remove$index)) %>%
    full_join(., ids_to_keep) %>%
    mutate(outcome_covid_index_date_final = case_when(index %in% ids_to_keep ~ outcome_covid_index_date_new, 
                                                      T ~ outcome_covid_index_date))
  
  if(cohort == "3") {
    
    data <- data %>%
      mutate(ptb_covid_index_date = outcome_covid_index_date_final)
    
  } else if(cohort == "4") {
    
    data <- data %>%
      mutate(vptb_covid_index_date = outcome_covid_index_date_final)
    
  } else {
    
    data <- data %>%
      mutate(perinatal_covid_index_date = outcome_covid_index_date_final)
    
  }
  
}

# read in ids of missing preterm babies whose mum's were in icu 7 days after end of pregnancy

fix_delivery_onset <- function(data, exposure = "infection") {
  
  if(exposure == "infection") {
    # Pregnancy ids manually found in smr02 with delivery onset information but did not link onto the infected cohorts
    spontaneous_ids <- c("")
    provider_ids <- c("")
    
    # IDs of women in infected cohort who are in ICU during end of pregnancy
    icu_ids <- read.csv(paste0(cops_perinatal_conf_folder, "icu_end_pregnancy_ids.csv"))
    
  } else if(exposure == "vaccination") {
    
    
    spontaneous_ids <- c("")
    
    provider_ids <- c("")
    
    icu_ids <- read.csv(paste0(cops_perinatal_conf_folder, "vacc_icu_end_pregnancy_ids.csv"))
    
    
  }
  
  
  data %>% 
    mutate(smr02_onset_of_delivery_process = case_when(pregnancy_id_orig %in% provider_ids ~ "Medically Indicated", 
                                                       pregnancy_id_orig %in%  spontaneous_ids ~ "Spontaneous", 
                                                       pregnancy_id_orig %in% icu_ids$icu_pregnancy_id ~ "Medically Indicated", 
                                                       T ~ smr02_onset_of_delivery_process))
}



##############################################.
###### IMPORT BABY OUTCOMES DATA ----
##############################################.

##############################################.
# cohort 1
##############################################.
# restrictions = live & stillbirths >= 20 weeks 
# outcomes = stillbirth & extended perinatal death 
cohort1_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort1_secondary_contemporary.rds")) %>%
  fix_data(.) 
cohort1_vacc_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort1_primary_historical.rds")) %>%
  fix_data(.)

cohort1_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort1_infection_secondary_contemporary.rds")) %>%
  fix_data(.) %>% 
  fix_wrong_dates(., cohort = "1")
cohort1_infect_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort1_infection_primary_historical.rds")) %>%
  fix_data(.)


cohort1_vacc_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort1_secondary_contemporary_singletons.rds")) %>%
  fix_data(.)
cohort1_vacc_historic_controls_singletons  <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort1_primary_historical_singletons.rds")) %>%
  fix_data(.)

cohort1_infect_contemp_controls_singletons  <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort1_infection_secondary_contemporary_singletons.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(., cohort = "1")
cohort1_infect_historic_controls_singletons  <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort1_infection_primary_historical_singletons.rds")) %>%
  fix_data(.)

##############################################.
# cohort 2
##############################################.
# restrictions = live births >= 20 weeks 
# outcomes = small for gestational age, apgar & neonatal death 
cohort2_vacc_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort2_secondary_contemporary_singletons.rds")) %>%
  fix_data(.)
cohort2_vacc_historic_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort2_primary_historical_singletons.rds")) %>%
  fix_data(.)

cohort2_infect_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort2_infection_secondary_contemporary_singletons.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(., cohort = "2")
cohort2_infect_historic_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort2_infection_primary_historical_singletons.rds")) %>%
  fix_data(.)

cohort2_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort2_secondary_contemporary.rds")) %>%
  fix_data(.)
cohort2_vacc_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort2_primary_historical.rds")) %>%
  fix_data(.)

cohort2_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort2_infection_secondary_contemporary.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(., cohort = "2")
cohort2_infect_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort2_infection_primary_historical.rds")) %>%
  fix_data(.)


##############################################.
# cohort 3
##############################################.
# restrictions = live births >= 20 weeks 
# outcomes = preterm birth 
cohort3_vacc_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort3_secondary_contemporary_singletons.rds")) %>%
  fix_data(.) %>%
  fix_delivery_onset(., exposure = "vaccination")

cohort3_vacc_historic_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort3_primary_historical_singletons.rds")) %>%
  fix_data(.)

cohort3_infect_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort3_infection_secondary_contemporary_singletons.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(., cohort = "3") %>%
  fix_delivery_onset(.)



cohort3_infect_historic_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort3_infection_primary_historical_singletons.rds")) %>%
  fix_data(.)

cohort3_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort3_secondary_contemporary.rds")) %>%
  fix_data(.)
cohort3_vacc_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort3_primary_historical.rds")) %>%
  fix_data(.)

cohort3_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort3_infection_secondary_contemporary.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(., cohort = "3")

cohort3_infect_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort3_infection_primary_historical.rds")) %>%
  fix_data(.)

##############################################.
# cohort 4
##############################################.
# restrictions = live births >= 20 weeks 
# outcomes = very preterm birth 
cohort4_vacc_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort4_secondary_contemporary_singletons.rds")) %>%
  fix_data(.) %>%
  fix_delivery_onset(., exposure = "vaccination")

cohort4_vacc_historic_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort4_primary_historical_singletons.rds")) %>%
  fix_data(.)

cohort4_infect_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort4_infection_secondary_contemporary_singletons.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(., cohort = "4") %>% 
  fix_delivery_onset(.)

cohort4_infect_historic_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort4_infection_primary_historical_singletons.rds")) %>%
  fix_data(.)

cohort4_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort4_secondary_contemporary.rds")) %>%
  fix_data(.)
cohort4_vacc_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort4_primary_historical.rds")) %>%
  fix_data(.)

cohort4_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort4_infection_secondary_contemporary.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(., cohort = "4")

cohort4_infect_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort4_infection_primary_historical.rds")) %>%
  fix_data(.)

##############################################.
# cohort 5
##############################################.
# restrictions = Live births>=23 & <=42 weeks
# outcomes = small for gestational age

cohort5_vacc_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort5_secondary_contemporary_singletons.rds")) %>%
  fix_data(.) 
cohort5_vacc_historic_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort5_primary_historical_singletons.rds")) %>%
  fix_data(.)

cohort5_infect_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort5_infection_secondary_contemporary_singletons.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(., cohort = "5")
cohort5_infect_historic_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort5_infection_primary_historical_singletons.rds")) %>%
  fix_data(.)

cohort5_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort5_secondary_contemporary.rds")) %>%
  fix_data(.)
cohort5_vacc_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort5_primary_historical.rds")) %>%
  fix_data(.)

cohort5_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort5_infection_secondary_contemporary.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(., cohort = "5")

cohort5_infect_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort5_infection_primary_historical.rds")) %>%
  fix_data(.)


##############################################.
# cohort 6
##############################################.
# restrictions = Live births>=37 weeks
# outcomes = apgar

cohort6_vacc_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort6_secondary_contemporary_singletons.rds")) %>%
  fix_data(.)
cohort6_vacc_historic_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort6_primary_historical_singletons.rds")) %>%
  fix_data(.)

cohort6_infect_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort6_infection_secondary_contemporary_singletons.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(., cohort = "6")

cohort6_infect_historic_controls_singletons <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort6_infection_primary_historical_singletons.rds")) %>%
  fix_data(.)

cohort6_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort6_secondary_contemporary.rds")) %>%
  fix_data(.)
cohort6_vacc_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort6_primary_historical.rds")) %>%
  fix_data(.)

cohort6_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort6_infection_secondary_contemporary.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(., cohort = "6")

cohort6_infect_historic_controls <- readRDS(paste0(folder_working_data, "matched_perinatal_cohort6_infection_primary_historical.rds")) %>%
  fix_data(.)


##############################################.
###### IMPORT MATERNAL OUTCOMES DATA ----
##############################################.

##############################################.
# cohort 7
##############################################.
# restrictions = All pregnancies ending in a live or stillbirth>=20 weeks
# outcomes = Hypertensive disorders of pregnancy

cohort7_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_maternal_hypertension_primary_contemporary_hbres.rds")) %>%
  fix_data(.)
cohort7_vacc_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_maternal_hypertension_primary_contemporary_singletons_hbres.rds")) %>%
  fix_data(.)

cohort7_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_maternal_hypertension_infection_primary_contemporary_hbres.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(.)
cohort7_infect_contemp_controls_singletons <- readRDS(paste0(folder_working_data, 
                                                             "matched_maternal_hypertension_infection_primary_contemporary_singletons_hbres.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(.)

##############################################.
# cohort 8
##############################################.
# restrictions = All pregnancies ending in a live or stillbirth>=20 weeks
# outcomes = Venous thromboembolism


cohort8_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_maternal_vte_primary_contemporary.rds")) %>%
  fix_data(.)
cohort8_vacc_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_maternal_vte_primary_contemporary_singletons.rds")) %>%
  fix_data(.)

cohort8_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_maternal_vte_infection_primary_contemporary.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(.)
cohort8_infect_contemp_controls_singletons <- readRDS(paste0(folder_working_data, 
                                                             "matched_maternal_vte_infection_primary_contemporary_singletons.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(.)


##############################################.
# cohort 9  
##############################################.
# restrictions = All pregnancies ending in a live or stillbirth>=20 weeks
# outcomes = Pregnancy-related bleeding

cohort9_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_maternal_bleeding_primary_contemporary_hbres.rds")) %>%
  fix_data(.)
cohort9_vacc_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_maternal_bleeding_primary_contemporary_singletons_hbres.rds")) %>%
  fix_data(.)

cohort9_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_maternal_bleeding_infection_primary_contemporary_hbres.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(.)
cohort9_infect_contemp_controls_singletons <- readRDS(paste0(folder_working_data, 
                                                             "matched_maternal_bleeding_infection_primary_contemporary_singletons_hbres.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(.)


##############################################.
# cohort 10
##############################################.
# restrictions = All pregnancies ending in a live or stillbirth>=20 weeks
# outcomes = Death/ICU admission


cohort10_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_maternal_icudeath_primary_contemporary.rds")) %>%
  fix_data(.)
cohort10_vacc_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_maternal_icudeath_primary_contemporary_singletons.rds")) %>%
  fix_data(.)

cohort10_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_maternal_icudeath_infection_primary_contemporary.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(.)
cohort10_infect_contemp_controls_singletons <- readRDS(paste0(folder_working_data, "matched_maternal_icudeath_infection_primary_contemporary_singletons.rds")) %>%
  fix_data(.) %>%
  fix_wrong_dates(.)
