#****************************************************************************************************************
#ASSESS EVIDENCE OF ASSOCIATION BETWEEN VACCINATION AND CONGENITAL ANOMALIES
#
#1. PRIMARY ANALYSIS WITH CONTEMPORARY CONTROLS
#2. SUBGROUP ANALYSIS BY VACCINE TYPE
#3. SENSTIVITIY ANALYSIS RESTRICTING TO BIRTHS FROM 12 WEEKS
#4. SENSITIVITY ANALYSIS RESTRICTING EXPOSURE PERIOD TO 2+0 TO 9+6 WEEKS
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

folder_working_data <- "X"
folder_results <- "X"

##### IMPORT DATA  #####

cohort1_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_congenital_primary_12wks_contemporary.rds")) 
cohort2_vacc_contemp_controls_sensitivity <- readRDS(paste0(folder_working_data, "matched_congenital_sensitivity1_contemporary.rds")) 
cohort3_vacc_contemp_controls_sensitivity <- readRDS(paste0(folder_working_data, "matched_congenital_sensitivity2_postconception2.rds"))
cohort4_vacc_contemp_controls_sensitivity <- readRDS(paste0(folder_working_data, "matched_congenital_sensitivity3_conceptionquarter.rds"))

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
  mutate(exposure_ca = case_when(vacc_or_unvacc=="vacc" ~ 1,
                                 vacc_or_unvacc=="unvacc" ~0)) %>%
  mutate(conception_half = case_when(conception_quarter==1 | conception_quarter==4 ~ "1. winter",
                                     conception_quarter==2 | conception_quarter==3 ~ "2. summer"))

}

cohort1_vacc_contemp_controls_ready <- prep_data(cohort1_vacc_contemp_controls)
cohort1_vacc_contemp_controls_ready$smoking_status[is.na(cohort1_vacc_contemp_controls_ready$smoking_status)] <- "Unknown/Missing"

cohort2_vacc_contemp_controls_sensitivity_ready <- prep_data(cohort2_vacc_contemp_controls_sensitivity)
cohort2_vacc_contemp_controls_sensitivity_ready$smoking_status[is.na(cohort2_vacc_contemp_controls_sensitivity_ready$smoking_status)] <- "Unknown/Missing"

cohort3_vacc_contemp_controls_sensitivity_ready <- prep_data(cohort3_vacc_contemp_controls_sensitivity)
cohort3_vacc_contemp_controls_sensitivity_ready$smoking_status[is.na(cohort3_vacc_contemp_controls_sensitivity_ready$smoking_status)] <- "Unknown/Missing"

cohort4_vacc_contemp_controls_sensitivity_ready <- prep_data(cohort4_vacc_contemp_controls_sensitivity)
cohort4_vacc_contemp_controls_sensitivity_ready$smoking_status[is.na(cohort4_vacc_contemp_controls_sensitivity_ready$smoking_status)] <- "Unknown/Missing"

#### PRIMARY ANALYSIS ####

cohort1_vacc_contemp_controls_ready <- cohort1_vacc_contemp_controls_ready %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(gest_at_match = max_(congenital_anomaly_gestation_at_reference_date)) %>%
  ungroup()
addmargins(table(cohort1_vacc_contemp_controls_ready$gest_at_match))

#OUTCOME
addmargins(table(cohort1_vacc_contemp_controls_ready$outcome_ca))
addmargins(table(cohort1_vacc_contemp_controls_ready$outcome_ca_ng))

#EXPOSURE
addmargins(table(cohort1_vacc_contemp_controls_ready$exposure_ca))

#CHECK TWO BY TWO TABLES
addmargins(table(cohort1_vacc_contemp_controls_ready$exposure_ca, cohort1_vacc_contemp_controls_ready$outcome_ca))
addmargins(table(cohort1_vacc_contemp_controls_ready$exposure_ca, cohort1_vacc_contemp_controls_ready$outcome_ca_ng))

#MODELLING

#ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME=ALL CONGENITAL ANOMALIES)
model1 <- clogit(outcome_ca ~ exposure_ca + strata(index), data=cohort1_vacc_contemp_controls_ready)
summary(model1)

#FULLY ADJUSTED MODEL
model1a <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + smoking_status + bmi_cat + multiplicity + strata(index), data=cohort1_vacc_contemp_controls_ready)
summary(model1a)

#MODEL REMOVING UNKNOWN URBAN/RURAL STATUS, SMID & SMOKING STATUS
model_check <- cohort1_vacc_contemp_controls_ready %>%
  filter(simd!="Unknown/Missing" & UR6_categories!="Unknown/Missing" & smoking_status!="Unknown/Missing") %>%
  mutate(simd = as.character(simd)) %>%
  mutate(smoking_status = as.character(smoking_status))
model1b <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + smoking_status + bmi_cat + multiplicity + strata(index), data=model_check)
summary(model1b)

#MODEL ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME= NON GENETIC CONGENITAL ANOMALIES)
model2 <- clogit(outcome_ca_ng ~exposure_ca + strata(index), data=cohort1_vacc_contemp_controls_ready)
summary(model2)

#FULLY ADJUSTED MODEL 
model2a <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + smoking_status + bmi_cat + multiplicity + strata(index), data=cohort1_vacc_contemp_controls_ready)
summary(model2a)

#MODEL REMOVING UNKNOWN URBAN/RURAL STATUS, SMID & SMOKING STATUS 
model2b <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=model_check)
summary(model2b)

#### SUBGROUP ANALYSIS ####

table(cohort1_vacc_contemp_controls_ready$congenital_anomaly_type_of_vaccination_during_period)
cohort1_vacc_contemp_controls_ready <- cohort1_vacc_contemp_controls_ready %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(vaccination_subgroup = first_(congenital_anomaly_type_of_vaccination_during_period)) %>%
  ungroup()
addmargins(table(cohort1_vacc_contemp_controls_ready$vaccination_subgroup))

#mRNA vaccines
cohort1_vacc_contemp_controls.mrna <- cohort1_vacc_contemp_controls_ready %>%
  filter(vaccination_subgroup == "Pfizer" | vaccination_subgroup == "Moderna")

addmargins(table(cohort1_vacc_contemp_controls.mrna$exposure_ca, cohort1_vacc_contemp_controls.mrna$outcome_ca))
addmargins(table(cohort1_vacc_contemp_controls.mrna$exposure_ca, cohort1_vacc_contemp_controls.mrna$outcome_ca_ng))

model1.mrna <- clogit(outcome_ca ~ exposure_ca + strata(index), data=cohort1_vacc_contemp_controls.mrna)
summary(model1.mrna)

model1a.mrna <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + multiplicity + strata(index), data=cohort1_vacc_contemp_controls.mrna)
summary(model1a.mrna)

model_check.mrna <- cohort1_vacc_contemp_controls.mrna %>%
  filter(simd!="Unknown/Missing" & UR6_categories!="Unknown/Missing" & smoking_status!="Unknown/Missing") %>%
  mutate(simd = as.character(simd)) %>%
  mutate(smoking_status = as.character(smoking_status))
model1b.mrna <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=model_check.mrna)
summary(model1b.mrna)

model2.mrna <- clogit(outcome_ca_ng ~ exposure_ca + strata(index), data=cohort1_vacc_contemp_controls.mrna)
summary(model2.mrna)

model2b.mrna <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + multiplicity + strata(index), data=cohort1_vacc_contemp_controls.mrna)
summary(model2b.mrna)

model2b.mrna <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + multiplicity + strata(index), data=model_check.mrna)
summary(model2b.mrna)

#Astrazeneca vaccines
cohort1_vacc_contemp_controls.az <- cohort1_vacc_contemp_controls_ready %>%
  filter(vaccination_subgroup == "AstraZeneca")

addmargins(table(cohort1_vacc_contemp_controls.az$exposure_ca, cohort1_vacc_contemp_controls.az$outcome_ca))
addmargins(table(cohort1_vacc_contemp_controls.az$exposure_ca, cohort1_vacc_contemp_controls.az$outcome_ca_ng))

model1a.az <- clogit(outcome_ca ~ exposure_ca + strata(index), data=cohort1_vacc_contemp_controls.az)
summary(model1a.az)

model1a.az <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + multiplicity + strata(index), data=cohort1_vacc_contemp_controls.az)
summary(model1a.az)

model1.az <- clogit(outcome_ca_ng ~ exposure_ca + strata(index), data=cohort1_vacc_contemp_controls.az)
summary(model1.az)

model1a.az <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + multiplicity + strata(index), data=cohort1_vacc_contemp_controls.az)
summary(model1a.az)

#### SENSITIVITY ANALYSIS 1 ####
#INCLUDING ALL BIRTHS (I.E. EVEN THOSE ENDING BEFORE 12 WEEKS GESTATION)

#CHECK TWO BY TWO TABLES
addmargins(table(cohort2_vacc_contemp_controls_sensitivity_ready$exposure_ca, cohort2_vacc_contemp_controls_sensitivity_ready$outcome_ca))
addmargins(table(cohort2_vacc_contemp_controls_sensitivity_ready$exposure_ca, cohort2_vacc_contemp_controls_sensitivity_ready$outcome_ca_ng))

sense1_preg_counts <- cohort2_vacc_contemp_controls_sensitivity_ready %>%
  group_by(pregnancy_id_orig) %>% 
  slice(1) %>%
  ungroup %>%
  group_by(exposure_ca) %>%
  tally() %>% 
  pivot_wider(names_from = exposure_ca, values_from = n) %>% 
  mutate(rowname = "n_preg:")

#MODELLING

#ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME=ALL CONGENITAL ANOMALIES)
model1.sense <- clogit(outcome_ca ~ exposure_ca + strata(index), data=cohort2_vacc_contemp_controls_sensitivity_ready)
summary(model1.sense)

#FULLY ADJUSTED MODEL 
model1a.sense <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=cohort2_vacc_contemp_controls_sensitivity_ready)
summary(model1a.sense)

#MODEL REMOVING UNKNOWN SOCIODEMOGRAPHIC GROUPS
model_check <- cohort2_vacc_contemp_controls_sensitivity_ready %>%
  filter(simd!="Unknown/Missing" & UR6_categories!="Unknown/Missing")
model1b.sense <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=model_check)
summary(model1b.sense)

#MODEL ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME= NON GENETIC CONGENITAL ANOMALIES)
model2.sense <- clogit(outcome_ca_ng ~exposure_ca + strata(index), data=cohort2_vacc_contemp_controls_sensitivity_ready)
summary(model2.sense)

#FULLY ADJUSTED MODEL 
model2a.sense <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=cohort2_vacc_contemp_controls_sensitivity_ready)
summary(model2a.sense)

#MODEL REMOVING UNKNOWN SOCIODEMOGRAPHIC GROUPS
model2b.sense <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=model_check)
summary(model2b.sense)

#### SENSITIVITY ANALYSIS 2 #### 
#RESTRICTING EXPOSURE WINDOW TO 2-9+6 WEEK GESTATION

#CHECK TWO BY TWO TABLES
addmargins(table(cohort3_vacc_contemp_controls_sensitivity_ready$exposure_ca, cohort3_vacc_contemp_controls_sensitivity_ready$outcome_ca))
addmargins(table(cohort3_vacc_contemp_controls_sensitivity_ready$exposure_ca, cohort3_vacc_contemp_controls_sensitivity_ready$outcome_ca_ng))

sense2_preg_counts <- cohort3_vacc_contemp_controls_sensitivity_ready %>%
  group_by(pregnancy_id_orig) %>% 
  slice(1) %>%
  ungroup %>%
  group_by(exposure_ca) %>%
  tally() %>% 
  pivot_wider(names_from = exposure_ca, values_from = n) %>% 
  mutate(rowname = "n_preg:")

#MODELLING

#ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME=ALL CONGENITAL ANOMALIES)
model1.sense2 <- clogit(outcome_ca ~ exposure_ca + strata(index), data=cohort3_vacc_contemp_controls_sensitivity_ready)
summary(model1.sense2)

#FULLY ADJUSTED MODEL 
model1a.sense2 <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + multiplicity + strata(index), data=cohort3_vacc_contemp_controls_sensitivity_ready)
summary(model1a.sense2)

#MODEL REMOVING UNKNOWN SOCIODEMOGRAPHIC GROUPS
model_check <- cohort3_vacc_contemp_controls_sensitivity_ready %>%
  filter(simd!="Unknown/Missing" & UR6_categories!="Unknown/Missing" & smoking_status!="Unknown/Missing") %>%
  mutate(simd = as.character(simd)) %>%
  mutate(smoking_status = as.character(smoking_status))
model1b.sense <- clogit(outcome_ca ~ exposure_ca + simd + ethnicity_cat + UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + multiplicity + strata(index), data=model_check)
summary(model1b.sense)

#MODEL ONLY ACCOUNTING FOR MATCHING FACTORS (OUTCOME= NON GENETIC CONGENITAL ANOMALIES)
model2.sense2 <- clogit(outcome_ca_ng ~exposure_ca + strata(index), data=cohort3_vacc_contemp_controls_sensitivity_ready)
summary(model2.sense2)

#FULLY ADJUSTED MODEL 
model2a.sense <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + smoking_status + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=cohort3_vacc_contemp_controls_sensitivity_ready)
summary(model2a.sense)

#MODEL REMOVING UNKNOWN SOCIODEMOGRAPHIC GROUPS 
model2a.sense <- clogit(outcome_ca_ng ~ exposure_ca + simd + ethnicity_cat + UR2_categories + smoking_status + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + multiplicity + strata(index), data=model_check)
summary(model2a.sense)

