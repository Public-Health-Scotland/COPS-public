#****************************************************************************************************************
#ASSESS EVIDENCE OF ASSOCIATION BETWEEN INFECTION AND CONGENITAL ANOMALIES
#
#1. PRIMARY ANALYSIS WITH CONTEMPORARY CONTROLS
#2. SENSTIVITIY ANALYSIS RESTRICTING TO ONLY BRITHS FROM 12 WEEKS GESTATION
#****************************************************************************************************************

##### HOUSEKEEPING #####

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
library(hablar)
library(labelled)
library(purrr)
library(survival)

folder_working_data <- "x"
folder_results <- "x"

###### IMPORT DATA  ######

cohort1_inf_contemp_controls <- readRDS(paste0(folder_working_data, "matched_congenital_infection_primary_12wks_contemporary_final.rds")) 
cohort1_inf_contemp_controls_sense1 <- readRDS(paste0(folder_working_data, "matched_congenital_infection_sensitivity_contemporary_final.rds"))
cohort1_inf_contemp_controls_sense2 <- readRDS(paste0(folder_working_data, "matched_congenital_infection_sensitivity2_12wks_contemporary_final.rds"))

#### PREPATE DATA FOR ANALYSIS ####

prep_data <- function(data_in) {
  
  data <- data_in %>%
    mutate(preg_id = gsub('.{2}$', "", pregnancy_id)) %>%
    rowwise() %>%
    mutate(x_booking_smoking_status = first_(c(anbooking_smoking_status, smr02_booking_smoking_history))) %>%
    mutate(x_booking_smoking_status= as.character(x_booking_smoking_status)) %>%
    mutate(x_booking_smoking_status = case_when(x_booking_smoking_status == "0" ~ "non-smoker",
                                                x_booking_smoking_status == "1" ~ "smoker",
                                                x_booking_smoking_status == "2" ~ "ex-smoker")) %>%
    mutate(x_gp_smoking_status = case_when(eave_non_smoker >= 1 & eave_smoker >= 1 & eave_esmoker >= 1
                                           ~ NA_character_,
                                           eave_non_smoker == 0 & eave_smoker >= 1 & eave_esmoker >= 1
                                           ~ NA_character_,
                                           eave_non_smoker >= 1 & eave_smoker >= 1 & eave_esmoker == 0
                                           ~ NA_character_,
                                           eave_non_smoker >= 1 & eave_smoker == 0 & eave_esmoker >= 1
                                           ~ "ex-smoker",
                                           eave_non_smoker >= 1 & eave_smoker == 0 & eave_esmoker == 0
                                           ~ "non-smoker",
                                           eave_non_smoker == 0 & eave_smoker >= 1 & eave_esmoker == 0
                                           ~ "smoker",
                                           eave_non_smoker == 0 & eave_smoker == 0 & eave_esmoker >= 1
                                           ~ "ex-smoker",
                                           T ~ NA_character_)) %>%
    mutate(x_overall_smoking_status = first_(c(x_booking_smoking_status,
                                               x_gp_smoking_status))) %>%
    mutate(x_overall_smoking_status = case_when ( (x_gp_smoking_status == "smoker" | x_gp_smoking_status == "ex-smoker") & x_booking_smoking_status == "non-smoker" ~ "ex-smoker",
                                                  T ~ x_overall_smoking_status)) %>%
    mutate(smoking_status = factor(x_overall_smoking_status, levels = c("non-smoker",
                                                                        "ex-smoker",
                                                                        "smoker",
                                                                        "Unknown/Missing")))  %>%
    mutate(simd = case_when(simd == "1" ~ "1=most deprived",
                            simd == "5" ~ "5=least deprived",
                            T ~ simd)) %>%
    mutate(q_diag_diabetes_1 = case_when(!is.na(mother_eave_linkno) ~ replace_na(q_diag_diabetes_1, 0)),
           q_diag_diabetes_2 = case_when(!is.na(mother_eave_linkno) ~ replace_na(q_diag_diabetes_2, 0))) %>%
    mutate(diabetes = case_when(diabetes == "unknown" & q_diag_diabetes_1 == 0 & q_diag_diabetes_2 == 0 ~ "assumed_no_diabetes",
                                T ~ diabetes)) %>%
    mutate(diabetes_cat = as.character(diabetes_cat),
           diabetes_cat = case_when(diabetes_cat == "Unknown" & diabetes == "assumed_no_diabetes" ~ "No - assumed & confirmed",
                                    T ~ diabetes_cat),
           diabetes_cat_2 = factor(diabetes_cat, levels = c("No - assumed & confirmed",
                                                            "Pre-existing diabetes",
                                                            "Gestational Diabetes/onset unknown"))) %>%
    mutate(multiplicity = case_when(total_foetus==1 ~ "1. Singleton",
                                    total_foetus>=2 ~ "2. Multiple")) %>%
    mutate(outcome_ca = case_when(all_14_all_anomalies==1 ~ 1,
                                  is.na(all_14_all_anomalies) ~0)) %>%
    mutate(outcome_ca_ng = case_when(all_13_chromosomal==1~0,
                                     all_12_1_skeletal_dysplasias==1~0,
                                     all_12_11_genetic_syndromes_and_microdeletions==1~0,
                                     all_14_all_anomalies==1 ~ 1,
                                     is.na(all_14_all_anomalies) ~0)) %>%
    mutate(exposure_ca = case_when(inf_or_uninf=="inf" ~ 1,
                                   inf_or_uninf=="uninf" ~0))
  
}

cohort1_inf_contemp_controls_ready <- prep_data(cohort1_inf_contemp_controls)
cohort1_inf_contemp_controls_ready$smoking_status[is.na(cohort1_inf_contemp_controls_ready$smoking_status)] <- "Unknown/Missing"

cohort1_inf_contemp_controls_sense1_ready <- prep_data(cohort1_inf_contemp_controls_sense1)
cohort1_inf_contemp_controls_sense1_ready$smoking_status[is.na(cohort1_inf_contemp_controls_sense1_ready$smoking_status)] <- "Unknown/Missing"

cohort1_inf_contemp_controls_sense2_ready <- prep_data(cohort1_inf_contemp_controls_sense2)
cohort1_inf_contemp_controls_sense2_ready$smoking_status[is.na(cohort1_inf_contemp_controls_sense2_ready$smoking_status)] <- "Unknown/Missing"

#####PRIMARY ANALYSIS#####

#CHECK NUMBER OF PREGNANCIES
primary_preg_counts <- cohort1_inf_contemp_controls_ready %>%
  group_by(pregnancy_id_orig) %>% 
  slice(1) %>%
  ungroup %>%
  group_by(exposure_ca) %>%
  tally() %>% 
  pivot_wider(names_from = exposure_ca, values_from = n) %>% 
  mutate(rowname = "n_preg:")

#CHECK TWO BY TWO TABLES
addmargins(table(cohort1_inf_contemp_controls_ready$exposure_ca, cohort1_inf_contemp_controls_ready$outcome_ca))
addmargins(table(cohort1_inf_contemp_controls_ready$exposure_ca, cohort1_inf_contemp_controls_ready$outcome_ca_ng))

#MODEL ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME=ALL CONGENITAL ANOMALIES)
model1 <- clogit(outcome_ca ~exposure_ca + strata(index), data=cohort1_inf_contemp_controls_ready)
summary(model1)

#FULLY ADJUSTED MODEL 
model1a <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + multiplicity + strata(index), data=cohort1_inf_contemp_controls_ready)
summary(model1a)

#CHECK IMPACT OF REMOVING MISSING
model_check <- cohort1_inf_contemp_controls_ready %>%
  filter(simd!="Unknown/Missing" & UR6_categories!="Unknown/Missing" & smoking_status!="Unknown/Missing") %>%
  mutate(simd = as.character(simd)) %>%
  mutate(smoking_status = as.character(smoking_status))
model1a <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + multiplicity + strata(index), data=model_check)
summary(model1a)

#MODEL ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME= NON GENETIC CONGENITAL ANOMALIES)
model2 <- clogit(outcome_ca_ng ~exposure_ca + strata(index), data=cohort1_inf_contemp_controls_ready)
summary(model2)

#FULLY ADJUSTED MODEL 
model2a <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + multiplicity + strata(index), data=cohort1_inf_contemp_controls_ready)
summary(model2a)

#CHECK IMPACT OF REMOVING MISSING
model2a <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + multiplicity + strata(index), data=model_check)
summary(model2a)

#####SENSITIVITY ANALYSIS#####

#CHECK NUMBER OF PREGNANCIES
sensitivity_preg_counts <- cohort1_inf_contemp_controls_sense1_ready %>%
  group_by(pregnancy_id_orig) %>% 
  slice(1) %>%
  ungroup %>%
  group_by(exposure_ca) %>%
  tally() %>% 
  pivot_wider(names_from = exposure_ca, values_from = n) %>% 
  mutate(rowname = "n_preg:")

#CHECK TWO BY TWO TABLES
addmargins(table(cohort1_inf_contemp_controls_sense1_ready$exposure_ca, cohort1_inf_contemp_controls_sense1_ready$outcome_ca))
addmargins(table(cohort1_inf_contemp_controls_sense1_ready$exposure_ca, cohort1_inf_contemp_controls_sense1_ready$outcome_ca_ng))

#MODEL ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME=ALL CONGENITAL ANOMALIES)
model1 <- clogit(outcome_ca ~exposure_ca + strata(index), data=cohort1_inf_contemp_controls_sense1_ready)
summary(model1)

#FULLY ADJUSTED MODEL 
model1a <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=cohort1_inf_contemp_controls_sense1_ready)
summary(model1a)

#CHECK IMPACT OF REMOVING MISSING
model_check <- cohort1_inf_contemp_controls_sense1_ready %>%
  filter(simd!="Unknown/Missing" & UR6_categories!="Unknown/Missing") %>%
  mutate(simd = as.character(simd)) %>%
  mutate(smoking_status = as.character(smoking_status))
model1a <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=model_check)
summary(model1a)

#MODEL ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME= NON GENETIC CONGENITAL ANOMALIES)
model2 <- clogit(outcome_ca_ng ~exposure_ca + strata(index), data=cohort1_inf_contemp_controls_sense1_ready)
summary(model2)

#FULLY ADJUSTED MODEL 
model2a <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=cohort1_inf_contemp_controls_sense1_ready)
summary(model2a)

#CHECK IMPACT OF REMOVING MISSING
model2a <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=model_check)
summary(model2a)


#####SENSITIVITY ANALYSIS 2#####

#CHECK NUMBER OF PREGNANCIES
sensitivity_preg_counts <- cohort1_inf_contemp_controls_sense2_ready %>%
  group_by(pregnancy_id_orig) %>% 
  slice(1) %>%
  ungroup %>%
  group_by(exposure_ca) %>%
  tally() %>% 
  pivot_wider(names_from = exposure_ca, values_from = n) %>% 
  mutate(rowname = "n_preg:")

#CHECK TWO BY TWO TABLES
addmargins(table(cohort1_inf_contemp_controls_sense2_ready$exposure_ca, cohort1_inf_contemp_controls_sense2_ready$outcome_ca))
addmargins(table(cohort1_inf_contemp_controls_sense2_ready$exposure_ca, cohort1_inf_contemp_controls_sense2_ready$outcome_ca_ng))

#MODEL ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME=ALL CONGENITAL ANOMALIES)
model1 <- clogit(outcome_ca ~exposure_ca + strata(index), data=cohort1_inf_contemp_controls_sense2_ready)
summary(model1)

#FULLY ADJUSTED MODEL 
model2a <- clogit(outcome_ca ~ exposure_ca + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + strata(index), data=cohort1_inf_contemp_controls_sense2_ready)
summary(model2a)

#MODEL ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME= NON GENETIC CONGENITAL ANOMALIES)
model2 <- clogit(outcome_ca_ng ~exposure_ca + strata(index), data=cohort1_inf_contemp_controls_sense2_ready)
summary(model2)

#FULLY ADJUSTED MODEL 
model2a <- clogit(outcome_ca_ng ~ exposure_ca + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + strata(index), data=cohort1_inf_contemp_controls_sense2_ready)
summary(model2a)

