##################################################################################################
###### BABY OUTCOMES MODELS - SUPPLEMENTARY ANALYSIS 1: INFECTIONS UP TO 19+6 WEEKS ######
##################################################################################################

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
library(broom)

##### file paths #####
folder_working_data <- ""
folder_results <- ""

folder_scripts <- ""

source(paste0(folder_scripts, "/00-setup.R")) 

filter_pre_20_week_gestation <- function(data) {
  
  filtered_data <- data %>% 
    filter(perinatal_gestation_at_index_date < 20)
  
  gestation_ids <- data %>%
    filter(index %in% filtered_data$index)
  
  return(gestation_ids)
  
}

get_outcome_function <- function(data, cohort = 1, missing = FALSE) {
  
  data <- data %>%
    mutate(exposure = case_when(inf_or_uninf=="inf" ~ 1,
                                inf_or_uninf=="uninf" ~ 0)) %>%
    mutate(conception_half = case_when(conception_quarter==1 | conception_quarter==4 ~ "1. winter",
                                       conception_quarter==2 | conception_quarter==3 ~ "2. summer")) %>%
    mutate(diabetes_cat_2 = factor(diabetes_cat, levels = c("No - assumed & confirmed",
                                                            "Pre-existing diabetes",
                                                            "Gestational Diabetes/onset unknown")))
  
  if(cohort == 1) {
    
    pregnancy_ids <- read.csv(paste0(folder_working_data, "cohort1_reinfections.csv"))
    
    data <- data %>%
      mutate(stillbirth_outcome = case_when(outcome == "Stillbirth" ~ 1, 
                                            T ~ 0), 
             perinatal_outcome = case_when(outcome == "Stillbirth" ~ 1,
                                           neonatal_death != "Survived neonatal period" ~ 1, 
                                           T ~ 0))
    
  } else if(cohort == 2) { 
    
    pregnancy_ids <- read.csv(paste0(folder_working_data, "cohort2_reinfections.csv"))
    
    data <- data %>%
      mutate(nnd_outcome = case_when(neonatal_death != "Survived neonatal period" ~ 1, 
                                     T ~ 0))
    
  } else if(cohort == 3) {
    
    pregnancy_ids <- read.csv(paste0(folder_working_data, "cohort3_reinfections.csv"))
    
    infected_missing_ids <- data %>%
      filter(inf_or_uninf == "inf" & outcome == "Live birth" & gestation_at_outcome < 37 & (smr02_onset_of_delivery_process == "Unknown/Missing" | smr02_onset_of_delivery_process == "Undefined"))
    
    uninfected_missing_ids <- data %>%
      filter(inf_or_uninf == "uninf" & outcome == "Live birth" & gestation_at_outcome < 37 & (smr02_onset_of_delivery_process == "Unknown/Missing" | smr02_onset_of_delivery_process == "Undefined"))
    
    data <- data %>%
      mutate(preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 ~ 1, 
                                         T ~ 0), 
             spontaneous_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 & smr02_onset_of_delivery_process == "Spontaneous" ~ 1,
                                                     T ~ 0), 
             provider_initiated_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 & smr02_onset_of_delivery_process == "Medically Indicated" ~ 1,
                                                            T ~ 0), 
             provider_initiated_plus_missing_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 & smr02_onset_of_delivery_process != "Spontaneous" ~ 1,
                                                                         T ~ 0))
    
  } else if(cohort == 4) {
    
    pregnancy_ids <- read.csv(paste0(folder_working_data, "cohort4_reinfections.csv"))
    
    infected_missing_ids <- data %>%
      filter(inf_or_uninf == "inf" & outcome == "Live birth" & gestation_at_outcome < 32 & (smr02_onset_of_delivery_process == "Unknown/Missing" | smr02_onset_of_delivery_process == "Undefined"))
    
    uninfected_missing_ids <- data %>%
      filter(inf_or_uninf == "uninf" & outcome == "Live birth" & gestation_at_outcome < 32 & (smr02_onset_of_delivery_process == "Unknown/Missing" | smr02_onset_of_delivery_process == "Undefined"))
    
    data <- data %>%
      mutate(very_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 ~ 1, 
                                              T ~ 0), 
             spontaneous_very_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 & smr02_onset_of_delivery_process == "Spontaneous" ~ 1,
                                                          T ~ 0), 
             provider_initiated_very_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 & smr02_onset_of_delivery_process == "Medically Indicated" ~ 1,
                                                                 T ~ 0), 
             provider_initiated_plus_missing_very_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 & smr02_onset_of_delivery_process != "Spontaneous" ~ 1,
                                                                              T ~ 0))
    
  } else if(cohort == 5) {
    
    pregnancy_ids <- read.csv(paste0(folder_working_data, "cohort5_reinfections.csv"))
    
    infected_missing_ids <- data %>%
      filter(inf_or_uninf == "inf" & is.na(smr02_birthweight_percentile))
    
    uninfected_missing_ids <- data %>%
      filter(inf_or_uninf == "uninf" & is.na(smr02_birthweight_percentile))
    
    data <- data %>%
      mutate(sga_outcome = case_when(smr02_birthweight_percentile < 10 ~ 1, 
                                     T ~ 0), 
             very_sga_outcome = case_when(smr02_birthweight_percentile < 3 ~ 1, 
                                          T ~ 0))
    
  } else if(cohort == 6) { 
    
    pregnancy_ids <- read.csv(paste0(folder_working_data, "cohort6_reinfections.csv"))
    
    infected_missing_ids <- data %>%
      filter(inf_or_uninf == "inf" & is.na(apgar_score))
    
    uninfected_missing_ids <- data %>%
      filter(inf_or_uninf == "uninf" & is.na(apgar_score))
    
    data <- data %>%
      mutate(apgar_outcome = case_when(apgar_score < 7 ~ 1, 
                                       T ~ 0), 
             very_low_apgar_outcome = case_when(apgar_score < 4 ~ 1, 
                                                T ~ 0))
    
  }
  
  if(missing == TRUE) {
    
    data <- data %>%
      filter(!(index %in% infected_missing_ids$index)) %>%
      filter(!(pregnancy_id_orig %in% uninfected_missing_ids$pregnancy_id_orig))
    
  }
  
  filtered_data <- data %>%
    filter(!(index %in% pregnancy_ids$index))
  
  return(filtered_data)
  
}


##### COHORT 1: OUTCOME = STILLBIRTH AND EXTENDED PERINATAL DEATH #####
# only using pregnancies with singletons 

cohort1_contemp <- cohort1_infect_contemp_controls_singletons %>%
  filter_pre_20_week_gestation(.) %>%
  get_outcome_function(cohort = 1)

# check numbers for each outcome
table(cohort1_contemp$stillbirth_outcome, useNA = "ifany")

table(cohort1_contemp$perinatal_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort1_contemp$inf_or_uninf, cohort1_contemp$stillbirth_outcome))
addmargins(table(cohort1_contemp$inf_or_uninf, cohort1_contemp$perinatal_outcome))

##### COHORT 1: MODELS ######

### STILLBIRTHS ###
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model1 <- clogit(stillbirth_outcome ~ exposure + strata(index), data=cohort1_contemp)
summary(model1)

## FULLY ADJUSTED MODEL ##
model1a <- clogit(stillbirth_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort1_contemp)
summary(model1a)

### EXTENDED PERINATAL DEATH ###
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model2 <- clogit(perinatal_outcome ~ exposure + strata(index), data=cohort1_contemp)
summary(model2)

## FULLY ADJUSTED MODEL ##
model2a <- clogit(perinatal_outcome ~ exposure + simd  + parity_cat+
                     strata(index), 
                  data=cohort1_contemp)
summary(model2a)

##### COHORT 2: OUTCOME = NEONATAL DEATH #####

# cohort2_historic <- cohort2_vacc_historic_controls_singletons %>%
#   get_outcome_function(cohort = 2)

cohort2_contemp <- cohort2_infect_contemp_controls_singletons %>%
  filter_pre_20_week_gestation(.) %>%
  get_outcome_function(cohort = 2)

# check numbers for each outcome
table(cohort2_contemp$nnd_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort2_contemp$exposure, cohort2_contemp$nnd_outcome))

##### COHORT 2: MODELS ######

## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model3 <- clogit(nnd_outcome ~ exposure + strata(index), data=cohort2_contemp)
summary(model3)

## FULLY ADJUSTED MODEL ##
model3a <- clogit(nnd_outcome ~ exposure + simd + ethnicity_cat + parity_cat+
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort2_contemp)
summary(model3a)


##### COHORT 3: OUTCOME = PRETERM BIRTH #####
cohort3_contemp <- cohort3_infect_contemp_controls_singletons %>%
  filter_pre_20_week_gestation(.) %>%
  get_outcome_function(cohort = 3)

cohort3_contemp_deliveryonset <- cohort3_infect_contemp_controls_singletons %>%
  filter_pre_20_week_gestation(.) %>%
  get_outcome_function(cohort = 3, missing = TRUE)


# check numbers for each outcome
table(cohort3_contemp$preterm_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort3_contemp$exposure, cohort3_contemp$preterm_outcome))
addmargins(table(cohort3_contemp_deliveryonset$exposure, cohort3_contemp_deliveryonset$spontaneous_preterm_outcome))
addmargins(table(cohort3_contemp_deliveryonset$exposure, cohort3_contemp_deliveryonset$provider_initiated_preterm_outcome))

##### COHORT 3: MODELS ######

##### PRIMARY ANALYSIS
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model4 <- clogit(preterm_outcome ~ exposure + strata(index), data=cohort3_contemp)
summary(model4)

## FULLY ADJUSTED MODEL ##
model4a <- clogit(preterm_outcome ~ exposure + simd + ethnicity_cat  +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort3_contemp)
summary(model4a)

##### SECONDARY ANALYSIS 
# SPONTANEOUS PRETERM BIRTH 
model5 <- clogit(spontaneous_preterm_outcome ~ exposure + strata(index), data=cohort3_contemp_deliveryonset)
summary(model5)

## FULLY ADJUSTED MODEL ##
model5a <- clogit(spontaneous_preterm_outcome ~ exposure + simd + ethnicity_cat  +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort3_contemp_deliveryonset)
summary(model5a)

# PROVIDER INITITATED PRETERM BIRTH 
model6 <- clogit(provider_initiated_preterm_outcome ~ exposure + strata(index), data=cohort3_contemp_deliveryonset)
summary(model6)

## FULLY ADJUSTED MODEL ##
model6a <- clogit(provider_initiated_preterm_outcome ~ exposure + simd + ethnicity_cat + 
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort3_contemp_deliveryonset)
summary(model6a)

# PROVIDER INITITATED PRETERM BIRTH (ASSUMING MISSING DATA == PROVIDER INITIATED)
model7 <- clogit(provider_initiated_plus_missing_preterm_outcome ~ exposure + strata(index), data=cohort3_contemp)
summary(model7)

## FULLY ADJUSTED MODEL ##
model7a <- clogit(provider_initiated_plus_missing_preterm_outcome ~ exposure + simd + ethnicity_cat + 
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort3_contemp)
summary(model7a)


##### COHORT 4: OUTCOME = VERY PRETERM BIRTH #####
cohort4_contemp <- cohort4_infect_contemp_controls_singletons %>%
  filter_pre_20_week_gestation(.) %>%
  get_outcome_function(cohort = 4)

cohort4_contemp_deliveryonset <- cohort4_infect_contemp_controls_singletons %>%
  filter_pre_20_week_gestation(.) %>%
  get_outcome_function(cohort = 4, missing = TRUE)



table(cohort4_contemp$very_preterm_outcome, useNA = "ifany")
table(cohort4_contemp$spontaneous_very_preterm_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort4_contemp$exposure, cohort4_contemp$very_preterm_outcome))
addmargins(table(cohort4_contemp_deliveryonset$exposure, cohort4_contemp_deliveryonset$spontaneous_very_preterm_outcome))
addmargins(table(cohort4_contemp_deliveryonset$exposure, cohort4_contemp_deliveryonset$provider_initiated_very_preterm_outcome))

##### PRIMARY ANALYSIS
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model8 <- clogit(very_preterm_outcome ~ exposure + strata(index), data=cohort4_contemp)
summary(model8)

## FULLY ADJUSTED MODEL ##
model8a <- clogit(very_preterm_outcome ~ exposure + simd +
                    strata(index), 
                  data=cohort4_contemp)
summary(model8a)

##### SECONDARY ANALYSIS 
# SPONTANEOUS PRETERM BIRTH 
model9 <- clogit(spontaneous_very_preterm_outcome ~ exposure + strata(index), data=cohort4_contemp_deliveryonset)
summary(model9)

## FULLY ADJUSTED MODEL ##
model9a <- clogit(spontaneous_very_preterm_outcome ~ exposure + simd + strata(index), 
                  data=cohort4_contemp_deliveryonset)
summary(model9a)

# PROVIDER INITITATED PRETERM BIRTH 
model10 <- clogit(provider_initiated_very_preterm_outcome ~ exposure + strata(index), data=cohort4_contemp_deliveryonset)
summary(model10)

## FULLY ADJUSTED MODEL ##
model10a <- clogit(provider_initiated_very_preterm_outcome ~ exposure + simd  + strata(index), 
                   data=cohort4_contemp_deliveryonset)
summary(model10a)

# PROVIDER INITITATED PRETERM BIRTH (ASSUMING MISSING DATA == PROVIDER INITIATED)
model11 <- clogit(provider_initiated_plus_missing_very_preterm_outcome ~ exposure + strata(index), data=cohort4_contemp)
summary(model11)

## FULLY ADJUSTED MODEL ##
model11a <- clogit(provider_initiated_plus_missing_very_preterm_outcome ~ exposure + simd  + 
                     cv_clinical_vulnerability_category  + bmi_cat + smoking_status + strata(index), 
                   data=cohort4_contemp)
summary(model11a)

##### COHORT 5: OUTCOME = SMALL FOR GESTATIONAL AGE #####
cohort5_contemp <- cohort5_infect_contemp_controls_singletons %>%
  filter_pre_20_week_gestation(.) %>%
  get_outcome_function(cohort = 5, missing = TRUE)


table(cohort5_contemp$sga_outcome, useNA = "ifany")
table(cohort5_contemp$very_sga_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort5_contemp$exposure, cohort5_contemp$sga_outcome))
addmargins(table(cohort5_contemp$exposure, cohort5_contemp$very_sga_outcome))


##### PRIMARY ANALYSIS
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model12 <- clogit(sga_outcome ~ exposure + strata(index), data=cohort5_contemp)
summary(model12)

## FULLY ADJUSTED MODEL ##
model12a <- clogit(sga_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                     UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                   data=cohort5_contemp)
summary(model12a)

##### SECONDARY ANALYSIS 
# VERY SMALL FOR GESTATIONAL AGE
model13 <- clogit(very_sga_outcome ~ exposure + strata(index), data=cohort5_contemp)
summary(model13)

## FULLY ADJUSTED MODEL ##
model13a <- clogit(very_sga_outcome ~ exposure + simd  + parity_cat + strata(index), 
                  data=cohort5_contemp)
summary(model13a)


##### COHORT 6: OUTCOME = APGAR SCORE #####
cohort6_contemp <- cohort6_infect_contemp_controls_singletons %>%
  filter_pre_20_week_gestation(.) %>%
  get_outcome_function(cohort = 6, missing = TRUE)


table(cohort6_contemp$apgar_outcome, useNA = "ifany")
table(cohort6_contemp$very_low_apgar_outcome, useNA = "ifany")


# two-by-two tables 
addmargins(table(cohort6_contemp$exposure, cohort6_contemp$apgar_outcome))
addmargins(table(cohort6_contemp$exposure, cohort6_contemp$very_low_apgar_outcome))

##### PRIMARY ANALYSIS
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model14 <- clogit(apgar_outcome ~ exposure + strata(index), data=cohort6_contemp)
summary(model14)

## FULLY ADJUSTED MODEL ##
model14a <- clogit(apgar_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                     UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                   data=cohort6_contemp)
summary(model14a)

##### SECONDARY ANALYSIS 
# VERY LOW APGAR
model15 <- clogit(very_low_apgar_outcome ~ exposure + strata(index), data=cohort6_contemp)
summary(model15)

## FULLY ADJUSTED MODEL ##
model15a <- clogit(very_low_apgar_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                     UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                   data=cohort6_contemp)
summary(model15a)


# save data for forest plot 
saveRDS(cohort1_contemp, paste0(folder_results, "pre20weeks_stillbirth_peri.rds"))
saveRDS(cohort2_contemp, paste0(folder_results, "pre20weeks_nnd.rds"))
saveRDS(cohort3_contemp, paste0(folder_results, "pre20weeks_preterm.rds"))
saveRDS(cohort3_contemp_deliveryonset, paste0(folder_results, "pre20weeks_preterm_deliveryonset.rds"))
saveRDS(cohort4_contemp, paste0(folder_results, "pre20weeks_very_preterm.rds"))
saveRDS(cohort4_contemp_deliveryonset, paste0(folder_results, "pre20weeks_very_preterm_deliveryonset.rds"))
saveRDS(cohort5_contemp, paste0(folder_results, "pre20weeks_sga.rds"))
saveRDS(cohort6_contemp, paste0(folder_results, "pre20weeks_apgar.rds"))

# save model output for forest plot

saveRDS(model1, paste0(folder_results, "models/subgroup_analysis_pre20weeks_stillbirth_unadjusted.rds"))
saveRDS(model1a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_stillbirth_adjusted.rds"))

saveRDS(model2, paste0(folder_results, "models/subgroup_analysis_pre20weeks_perinatal_mortality_unadjusted.rds"))
saveRDS(model2a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_perinatal_mortality_adjusted.rds"))

saveRDS(model3, paste0(folder_results, "models/subgroup_analysis_pre20weeks_nnd_unadjusted.rds"))

saveRDS(model4, paste0(folder_results, "models/subgroup_analysis_pre20weeks_preterm_unadjusted.rds"))
saveRDS(model4a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_preterm_adjusted.rds"))

saveRDS(model5, paste0(folder_results, "models/subgroup_analysis_pre20weeks_spontaneous_preterm_unadjusted.rds"))
saveRDS(model5a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_spontaneous_preterm_adjusted.rds"))

saveRDS(model6, paste0(folder_results, "models/subgroup_analysis_pre20weeks_provider_initiated_preterm_unadjusted.rds"))
saveRDS(model6a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_provider_initiated_preterm_adjusted.rds"))

saveRDS(model8, paste0(folder_results, "models/subgroup_analysis_pre20weeks_very_preterm_unadjusted.rds"))
saveRDS(model8a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_very_preterm_adjusted.rds"))

saveRDS(model9, paste0(folder_results, "models/subgroup_analysis_pre20weeks_spontaneous_very_preterm_unadjusted.rds"))
saveRDS(model9a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_spontaneous_very_preterm_adjusted.rds"))

saveRDS(model10, paste0(folder_results, "models/subgroup_analysis_pre20weeks_provider_initiated_very_preterm_unadjusted.rds"))
saveRDS(model10a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_provider_initiated_very_preterm_adjusted.rds"))

saveRDS(model12, paste0(folder_results, "models/subgroup_analysis_pre20weeks_small_gestational_age_unadjusted.rds"))
saveRDS(model12a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_small_gestational_age_adjusted.rds"))

saveRDS(model13, paste0(folder_results, "models/subgroup_analysis_pre20weeks_very_small_gestational_age_unadjusted.rds"))
saveRDS(model13a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_very_small_gestational_age_adjusted.rds"))

saveRDS(model14, paste0(folder_results, "models/subgroup_analysis_pre20weeks_low_apgar_unadjusted.rds"))
saveRDS(model14a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_low_apgar_adjusted.rds"))

saveRDS(model15, paste0(folder_results, "models/subgroup_analysis_pre20weeks_very_low_apgar_unadjusted.rds"))
saveRDS(model15a, paste0(folder_results, "models/subgroup_analysis_pre20weeks_very_low_apgar_adjusted.rds"))











