#################################################
###### BABY OUTCOMES MODELS - VACCINATIONS ######
#################################################

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


get_outcome_function <- function(data, cohort = 1, missing = FALSE) {
  
  data <- data %>%
    mutate(exposure = case_when(vacc_or_unvacc=="vacc" ~ 1,
                                vacc_or_unvacc=="unvacc" ~ 0)) %>%
    mutate(conception_half = case_when(conception_quarter==1 | conception_quarter==4 ~ "1. winter",
                                       conception_quarter==2 | conception_quarter==3 ~ "2. summer")) %>%
    mutate(diabetes_cat_2 = factor(diabetes_cat, levels = c("No - assumed & confirmed",
                                                            "Pre-existing diabetes",
                                                            "Gestational Diabetes/onset unknown")))
  
  if(cohort == 1) {
    
    data <- data %>%
      mutate(stillbirth_outcome = case_when(outcome == "Stillbirth" ~ 1, 
                                            T ~ 0), 
             perinatal_outcome = case_when(outcome == "Stillbirth" ~ 1,
                                           neonatal_death != "Survived neonatal period" ~ 1, 
                                           T ~ 0))
    
  } else if(cohort == 2) { 
    
    data <- data %>%
      mutate(nnd_outcome = case_when(neonatal_death != "Survived neonatal period" ~ 1, 
                                     T ~ 0))
    
  } else if(cohort == 3) {
    
    vacc_missing_ids <- data %>%
      filter(vacc_or_unvacc == "vacc" & outcome == "Live birth" & gestation_at_outcome < 37 & (smr02_onset_of_delivery_process == "Unknown/Missing" | smr02_onset_of_delivery_process == "Undefined"))
    
    unvacc_missing_ids <- data %>%
      filter(vacc_or_unvacc == "unvacc" & outcome == "Live birth" & gestation_at_outcome < 37 & (smr02_onset_of_delivery_process == "Unknown/Missing" | smr02_onset_of_delivery_process == "Undefined"))
    
    data <- data %>%
      mutate(preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 ~ 1, 
                                         T ~ 0), 
             spontaneous_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 & smr02_onset_of_delivery_process == "Spontaneous" ~ 1,
                                                     T ~ 0), 
             spontaneous_plus_missing_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 & smr02_onset_of_delivery_process != "Medically Indicated" ~ 1,
                                                                  T ~ 0), 
             provider_initiated_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 & smr02_onset_of_delivery_process == "Medically Indicated" ~ 1,
                                                            T ~ 0), 
             provider_initiated_plus_missing_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 37 & smr02_onset_of_delivery_process != "Spontaneous" ~ 1,
                                                                         T ~ 0))
    
  } else if(cohort == 4) {
    
    vacc_missing_ids <- data %>%
      filter(vacc_or_unvacc == "vacc" & outcome == "Live birth" & gestation_at_outcome < 32 & (smr02_onset_of_delivery_process == "Unknown/Missing" | smr02_onset_of_delivery_process == "Undefined"))
    
    unvacc_missing_ids <- data %>%
      filter(vacc_or_unvacc == "unvacc" & outcome == "Live birth" & gestation_at_outcome < 32 & (smr02_onset_of_delivery_process == "Unknown/Missing" | smr02_onset_of_delivery_process == "Undefined"))
    
    data <- data %>%
      mutate(very_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 ~ 1, 
                                              T ~ 0), 
             spontaneous_very_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 & smr02_onset_of_delivery_process == "Spontaneous" ~ 1,
                                                          T ~ 0), 
             spontaneous_plus_missing_very_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 & smr02_onset_of_delivery_process != "Medically Indicated" ~ 1,
                                                                       T ~ 0), 
             provider_initiated_very_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 & smr02_onset_of_delivery_process == "Medically Indicated" ~ 1,
                                                                 T ~ 0), 
             provider_initiated_plus_missing_very_preterm_outcome = case_when(outcome == "Live birth" & gestation_at_outcome < 32 & smr02_onset_of_delivery_process != "Spontaneous" ~ 1,
                                                                              T ~ 0))
    
  } else if(cohort == 5) {
    
    vacc_missing_ids <- data %>%
      filter(vacc_or_unvacc == "vacc" & is.na(smr02_birthweight_percentile))
    
    unvacc_missing_ids <- data %>%
      filter(vacc_or_unvacc == "unvacc" & is.na(smr02_birthweight_percentile))
    
    data <- data %>%
      mutate(sga_outcome = case_when(smr02_birthweight_percentile < 10 ~ 1, 
                                     T ~ 0), 
             very_sga_outcome = case_when(smr02_birthweight_percentile < 3 ~ 1, 
                                          T ~ 0))
    
  } else if(cohort == 6) { 
    
    vacc_missing_ids <- data %>%
      filter(vacc_or_unvacc == "vacc" & is.na(apgar_score))
    
    unvacc_missing_ids <- data %>%
      filter(vacc_or_unvacc == "unvacc" & is.na(apgar_score))
    
    data <- data %>%
      mutate(apgar_outcome = case_when(apgar_score < 7 ~ 1, 
                                       T ~ 0), 
             very_low_apgar_outcome = case_when(apgar_score < 4 ~ 1, 
                                                T ~ 0))
    
  }
  
  if(missing == TRUE) {
    
    data <- data %>%
      filter(!(index %in% vacc_missing_ids$index)) %>%
      filter(!(pregnancy_id_orig %in% unvacc_missing_ids$pregnancy_id_orig))
    
  }
  
  return(data)
  
}

##### COHORT 1: OUTCOME = STILLBIRTH AND EXTENDED PERINATAL DEATH #####
# only using pregnancies with singletons 

cohort1_contemp <- cohort1_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 1)

# check numbers for each outcome
table(cohort1_contemp$stillbirth_outcome, useNA = "ifany")

table(cohort1_contemp$perinatal_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort1_contemp$vacc_or_unvacc, cohort1_contemp$stillbirth_outcome))
addmargins(table(cohort1_contemp$vacc_or_unvacc, cohort1_contemp$perinatal_outcome))

##### COHORT 1: MODELS ######

### STILLBIRTHS ###
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model1 <- clogit(stillbirth_outcome ~ exposure + strata(index), data=cohort1_contemp)
summary(model1)

## FULLY ADJUSTED MODEL ##
model1a <- clogit(stillbirth_outcome ~ exposure + simd + parity_cat +
                    strata(index), 
                  data=cohort1_contemp)
summary(model1a)

# MODEL REMOVING UNKNOWN SOCIODEMOGRAPHIC GROUPS # 
cohort_1_model_check_contemp <- cohort1_contemp %>%
  filter(UR2_categories != "Unknown/Missing" & simd != "Unknown/Missing") %>%
  mutate(simd = as.character(simd))

model1b <- clogit(stillbirth_outcome ~ exposure  + simd + parity_cat +
                    strata(index), 
                  data = cohort_1_model_check_contemp) 
summary(model1b)

table(cohort_1_model_check_contemp$UR2_categories, cohort_1_model_check_contemp$stillbirth_outcome, useNA = "ifany")

# CONDITIONAL POISSON REGRESSION 

model1c <- gnm(stillbirth_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort1_contemp)
summary(model1c)

exp(coef(model1c))

exp(confint(model1c, "exposure", method = "wald"))


model1d <- gnm(stillbirth_outcome ~ exposure + simd + parity_cat, 
               eliminate = as.factor(index), family = "poisson", data = cohort1_contemp)
summary(model1d)

exp(coef(model1d))

exp(confint(model1d, "exposure", method = "wald"))


### EXTENDED PERINATAL DEATH ###
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model2 <- clogit(perinatal_outcome ~ exposure + strata(index), data=cohort1_contemp)
summary(model2)

## FULLY ADJUSTED MODEL ##
model2a <- clogit(perinatal_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort1_contemp)
summary(model2a)

# MODEL REMOVING UNKNOWN SOCIODEMOGRAPHIC GROUPS # 
model2b <- clogit(perinatal_outcome ~ exposure + simd + ethnicity_cat + parity_cat + 
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data = cohort_1_model_check_contemp) 
summary(model2b)

# CONDITIONAL POISSON REGRESSION 

model2c <- gnm(perinatal_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort1_contemp)
summary(model2c)

exp(coef(model2c))

exp(confint(model2c, "exposure", method = "wald"))


model2d <- gnm(perinatal_outcome ~ exposure + simd + ethnicity_cat + parity_cat + 
                 UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status, 
               eliminate = as.factor(index), family = "poisson", data = cohort1_contemp)
summary(model2d)

exp(coef(model2d))

exp(confint(model2d, "exposure", method = "wald"))


##### COHORT 2: OUTCOME = NEONATAL DEATH #####

# cohort2_historic <- cohort2_vacc_historic_controls_singletons %>%
#   get_outcome_function(cohort = 2)

cohort2_contemp <- cohort2_vacc_contemp_controls_singletons %>%
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
model3a <- clogit(nnd_outcome ~ exposure + simd + parity_cat + 
                    strata(index), 
                  data=cohort2_contemp)
summary(model3a)

# CONDITIONAL POISSON REGRESSION 

model3b <- gnm(nnd_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort2_contemp)
summary(model3b)

exp(coef(model3b))

exp(confint(model3b, "exposure", method = "wald"))


model3c <- gnm(nnd_outcome ~ exposure +simd + parity_cat, 
               eliminate = as.factor(index), family = "poisson", data = cohort2_contemp)
summary(model3c)

exp(coef(model3c))

exp(confint(model3c, "exposure", method = "wald"))

##### COHORT 3: OUTCOME = PRETERM BIRTH #####
cohort3_contemp <- cohort3_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 3)

cohort3_contemp_deliveryonset <- cohort3_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 3, missing = TRUE)


# check numbers for each outcome
table(cohort3_contemp$preterm_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort3_contemp$exposure, cohort3_contemp$preterm_outcome))
addmargins(table(cohort3_contemp_deliveryonset$exposure, cohort3_contemp_deliveryonset$spontaneous_preterm_outcome))
addmargins(table(cohort3_contemp_deliveryonset$exposure, cohort3_contemp_deliveryonset$provider_initiated_preterm_outcome))

# for sensitivity analysis
addmargins(table(cohort3_contemp$exposure, cohort3_contemp$spontaneous_plus_missing_preterm_outcome))
addmargins(table(cohort3_contemp$exposure, cohort3_contemp$spontaneous_preterm_outcome))
addmargins(table(cohort3_contemp$exposure, cohort3_contemp$provider_initiated_preterm_outcome))
addmargins(table(cohort3_contemp$exposure, cohort3_contemp$provider_initiated_plus_missing_preterm_outcome))

##### COHORT 3: MODELS ######

##### PRIMARY ANALYSIS
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model4 <- clogit(preterm_outcome ~ exposure + strata(index), data=cohort3_contemp)
summary(model4)

## FULLY ADJUSTED MODEL ##
model4a <- clogit(preterm_outcome ~ exposure + simd + ethnicity_cat + 
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort3_contemp)
summary(model4a)

# CONDITIONAL POISSON REGRESSION 

model4b <- gnm(preterm_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort3_contemp)
summary(model4b)

exp(coef(model4b))

exp(confint(model4b, "exposure", method = "wald"))


model4c <- gnm(preterm_outcome ~ exposure + simd + ethnicity_cat + 
                 UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status, 
               eliminate = as.factor(index), family = "poisson", data = cohort3_contemp)
summary(model4c)

exp(coef(model4c))

exp(confint(model4c, "exposure", method = "wald"))

4##### SECONDARY ANALYSIS 
# SPONTANEOUS PRETERM BIRTH 
model5 <- clogit(spontaneous_preterm_outcome ~ exposure + strata(index), data=cohort3_contemp_deliveryonset)
summary(model5)

## FULLY ADJUSTED MODEL ##
model5a <- clogit(spontaneous_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort3_contemp_deliveryonset)
summary(model5a)

# CONDITIONAL POISSON REGRESSION 

model5b <- gnm(spontaneous_preterm_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort3_contemp_deliveryonset)
summary(model5b)

exp(coef(model5b))

exp(confint(model5b, "exposure", method = "wald"))


model5c <- gnm(spontaneous_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                 UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status, 
               eliminate = as.factor(index), family = "poisson", data = cohort3_contemp_deliveryonset)
summary(model5c)

exp(coef(model5c))

exp(confint(model5c, "exposure", method = "wald"))

# PROVIDER INITITATED PRETERM BIRTH 
model6 <- clogit(provider_initiated_preterm_outcome ~ exposure + strata(index), data=cohort3_contemp_deliveryonset)
summary(model6)

## FULLY ADJUSTED MODEL ##
model6a <- clogit(provider_initiated_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort3_contemp_deliveryonset)
summary(model6a)

# CONDITIONAL POISSON REGRESSION 

model6b <- gnm(provider_initiated_preterm_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort3_contemp_deliveryonset)
summary(model6b)

exp(coef(model6b))

exp(confint(model6b, "exposure", method = "wald"))


model6c <- gnm(provider_initiated_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                 UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status, 
               eliminate = as.factor(index), family = "poisson", data = cohort3_contemp_deliveryonset)
summary(model6c)

exp(coef(model6c))

exp(confint(model6c, "exposure", method = "wald"))


### PRETERM SENSITIVITY ANALYSES ###

# ASSUMING MISSING DELIVERY ONSET BIRTHS ARE SPONTANEOUS  
model7 <- clogit(spontaneous_plus_missing_preterm_outcome ~ exposure + strata(index), data=cohort3_contemp)
summary(model7)

model7a <- clogit(spontaneous_plus_missing_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort3_contemp)
summary(model7a)

model8 <- clogit(provider_initiated_preterm_outcome ~ exposure + strata(index), data=cohort3_contemp)
summary(model8)

model8a <- clogit(provider_initiated_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort3_contemp)
summary(model8a)



# ASSUMING MISSING DELIVERY ONSET BIRTHS ARE PROVIDER INITIATED 
# PROVIDER INITITATED PRETERM BIRTH (ASSUMING MISSING DATA == PROVIDER INITIATED)
model9 <- clogit(provider_initiated_plus_missing_preterm_outcome ~ exposure + strata(index), data=cohort3_contemp)
summary(model9)

## FULLY ADJUSTED MODEL ##
model9a <- clogit(provider_initiated_plus_missing_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort3_contemp)
summary(model9a)

model10 <- clogit(spontaneous_preterm_outcome ~ exposure + strata(index), data=cohort3_contemp)
summary(model10)

model10a <- clogit(spontaneous_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                     UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                   data=cohort3_contemp)
summary(model10a)

##### COHORT 4: OUTCOME = VERY PRETERM BIRTH #####
cohort4_contemp <- cohort4_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 4)

cohort4_contemp_deliveryonset <- cohort4_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 4, missing = TRUE)

table(cohort4_contemp$very_preterm_outcome, useNA = "ifany")
table(cohort4_contemp$spontaneous_very_preterm_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort4_contemp$exposure, cohort4_contemp$very_preterm_outcome))
addmargins(table(cohort4_contemp_deliveryonset$exposure, cohort4_contemp_deliveryonset$spontaneous_very_preterm_outcome))
addmargins(table(cohort4_contemp_deliveryonset$exposure, cohort4_contemp_deliveryonset$provider_initiated_very_preterm_outcome))


addmargins(table(cohort4_contemp$exposure, cohort4_contemp$spontaneous_plus_missing_very_preterm_outcome))
addmargins(table(cohort4_contemp$exposure, cohort4_contemp$provider_initiated_plus_missing_very_preterm_outcome))
addmargins(table(cohort4_contemp$exposure, cohort4_contemp$spontaneous_very_preterm_outcome))
addmargins(table(cohort4_contemp$exposure, cohort4_contemp$provider_initiated_very_preterm_outcome))

##### PRIMARY ANALYSIS
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model11 <- clogit(very_preterm_outcome ~ exposure + strata(index), data=cohort4_contemp)
summary(model11)

## FULLY ADJUSTED MODEL ##
model11a <- clogit(very_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                     UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                   data=cohort4_contemp)
summary(model11a)

# CONDITIONAL POISSON REGRESSION 

model11b <- gnm(very_preterm_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort4_contemp)
summary(model11b)

exp(coef(model11b))

exp(confint(model11b, "exposure", method = "wald"))


model11c <- gnm(very_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                  UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status, 
               eliminate = as.factor(index), family = "poisson", data = cohort4_contemp)
summary(model11c)

exp(coef(model11c))

exp(confint(model11c, "exposure", method = "wald"))



##### SECONDARY ANALYSIS 
# SPONTANEOUS PRETERM BIRTH 
model12 <- clogit(spontaneous_very_preterm_outcome ~ exposure + strata(index), data=cohort4_contemp_deliveryonset)
summary(model12)

## FULLY ADJUSTED MODEL ##
model12a <- clogit(spontaneous_very_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                     UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status  + strata(index), 
                   data=cohort4_contemp_deliveryonset)
summary(model12a)

# CONDITIONAL POISSON REGRESSION 

model12b <- gnm(spontaneous_very_preterm_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort4_contemp_deliveryonset)
summary(model12b)

exp(coef(model12b))

exp(confint(model12b, "exposure", method = "wald"))


model12c <- gnm(spontaneous_very_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                  UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status, 
                eliminate = as.factor(index), family = "poisson", data = cohort4_contemp_deliveryonset)
summary(model12c)

exp(coef(model12c))

exp(confint(model12c, "exposure", method = "wald"))

# PROVIDER INITITATED PRETERM BIRTH 
model13 <- clogit(provider_initiated_very_preterm_outcome ~ exposure + strata(index), data=cohort4_contemp_deliveryonset)
summary(model13)

## FULLY ADJUSTED MODEL ##
model13a <- clogit(provider_initiated_very_preterm_outcome ~ exposure + simd + 
                     strata(index), 
                   data=cohort4_contemp_deliveryonset)
summary(model13a)

# CONDITIONAL POISSON REGRESSION 

model13b <- gnm(provider_initiated_very_preterm_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort4_contemp_deliveryonset)
summary(model13b)

exp(coef(model13b))

exp(confint(model13b, "exposure", method = "wald"))


model13c <- gnm(provider_initiated_very_preterm_outcome ~ exposure + simd, 
                eliminate = as.factor(index), family = "poisson", data = cohort4_contemp_deliveryonset)
summary(model13c)

exp(coef(model13c))

exp(confint(model13c, "exposure", method = "wald"))

### VERY PRETERM SENSITIVITY ANALYSES ###
# ASSUMING MISSING DELIVERY ONSET BIRTHS ARE SPONTANEOUS  
model14 <- clogit(spontaneous_plus_missing_very_preterm_outcome ~ exposure + strata(index), data=cohort4_contemp)
summary(model14)

model14a <- clogit(spontaneous_plus_missing_very_preterm_outcome ~ exposure + simd + ethnicity_cat  + 
                     UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                   data=cohort4_contemp)
summary(model14a)

model15 <- clogit(provider_initiated_very_preterm_outcome ~ exposure + strata(index), data=cohort4_contemp)
summary(model15)

model15a <- clogit(provider_initiated_very_preterm_outcome ~ exposure + simd + strata(index), 
                   data=cohort4_contemp)
summary(model15a)


# ASSUMING MISSING DELIVERY ONSET BIRTHS ARE PROVIDER INITIATED 
# PROVIDER INITITATED PRETERM BIRTH (ASSUMING MISSING DATA == PROVIDER INITIATED)
model16 <- clogit(provider_initiated_plus_missing_very_preterm_outcome ~ exposure + strata(index), data=cohort4_contemp)
summary(model16)

## FULLY ADJUSTED MODEL ##
model16a <- clogit(provider_initiated_plus_missing_very_preterm_outcome ~ exposure + simd  + strata(index), 
                   data=cohort4_contemp)
summary(model16a)

model17 <- clogit(spontaneous_very_preterm_outcome ~ exposure + strata(index), data=cohort4_contemp)
summary(model17)

model17a <- clogit(spontaneous_very_preterm_outcome ~ exposure + simd + ethnicity_cat + 
                     UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                   data=cohort4_contemp)
summary(model17a)


##### COHORT 5: OUTCOME = SMALL FOR GESTATIONAL AGE #####
cohort5_contemp <- cohort5_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 5, missing = TRUE)

table(cohort5_contemp$sga_outcome, useNA = "ifany")
table(cohort5_contemp$very_sga_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort5_contemp$exposure, cohort5_contemp$sga_outcome))
addmargins(table(cohort5_contemp$exposure, cohort5_contemp$very_sga_outcome))


##### PRIMARY ANALYSIS
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model18 <- clogit(sga_outcome ~ exposure + strata(index), data=cohort5_contemp)
summary(model18)

## FULLY ADJUSTED MODEL ##
model18a <- clogit(sga_outcome ~ exposure + simd + ethnicity_cat + parity_cat + 
                     UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                   data=cohort5_contemp)
summary(model18a)

# CONDITIONAL POISSON REGRESSION 

model18b <- gnm(sga_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort5_contemp)
summary(model18b)

exp(coef(model18b))

exp(confint(model18b, "exposure", method = "wald"))


model18c <- gnm(sga_outcome ~ exposure + simd + ethnicity_cat + parity_cat + 
                  UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status, 
                eliminate = as.factor(index), family = "poisson", data = cohort5_contemp)
summary(model18c)

exp(coef(model18c))

exp(confint(model18c, "exposure", method = "wald"))

##### SECONDARY ANALYSIS 
# VERY SMALL FOR GESTATIONAL AGE
model19 <- clogit(very_sga_outcome ~ exposure + strata(index), data=cohort5_contemp)
summary(model19)

## FULLY ADJUSTED MODEL ##
model19a <- clogit(very_sga_outcome ~ exposure + simd + ethnicity_cat + parity_cat + 
                     UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                   data=cohort5_contemp)
summary(model19a)

# CONDITIONAL POISSON REGRESSION 

model19b <- gnm(very_sga_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort5_contemp)
summary(model19b)

exp(coef(model19b))

exp(confint(model19b, "exposure", method = "wald"))


model19c <- gnm(very_sga_outcome ~ exposure + simd + ethnicity_cat + parity_cat + 
                  UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status, 
                eliminate = as.factor(index), family = "poisson", data = cohort5_contemp)
summary(model19c)

exp(coef(model19c))

exp(confint(model19c, "exposure", method = "wald"))


##### COHORT 6: OUTCOME = LOW APGAR SCORE #####
cohort6_contemp <- cohort6_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 6, missing = TRUE)


table(cohort6_contemp$apgar_outcome, useNA = "ifany")
table(cohort6_contemp$very_low_apgar_outcome, useNA = "ifany")


# two-by-two tables 
addmargins(table(cohort6_contemp$exposure, cohort6_contemp$apgar_outcome))
addmargins(table(cohort6_contemp$exposure, cohort6_contemp$very_low_apgar_outcome))

##### PRIMARY ANALYSIS
## CONTEMPORARY CONTROLS ##
## UNADJUSTED MODEL ##
model20 <- clogit(apgar_outcome ~ exposure + strata(index), data=cohort6_contemp)
summary(model20)

## FULLY ADJUSTED MODEL ##
model20a <- clogit(apgar_outcome ~ exposure + simd + ethnicity_cat + parity_cat + 
                     UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                   data=cohort6_contemp)
summary(model20a)

# CONDITIONAL POISSON REGRESSION 

model20b <- gnm(apgar_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort6_contemp)
summary(model20b)

exp(coef(model20b))

exp(confint(model20b, "exposure", method = "wald"))


model20c <- gnm(apgar_outcome ~ exposure + simd + ethnicity_cat + parity_cat + 
                  UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status, 
                eliminate = as.factor(index), family = "poisson", data = cohort6_contemp)
summary(model20c)

exp(coef(model20c))

exp(confint(model20c, "exposure", method = "wald"))

##### SECONDARY ANALYSIS 
# VERY LOW APGAR
model21 <- clogit(very_low_apgar_outcome ~ exposure + strata(index), data=cohort6_contemp)
summary(model21)

## FULLY ADJUSTED MODEL ##
model21a <- clogit(very_low_apgar_outcome ~ exposure + simd  + parity_cat + 
                     strata(index), 
                   data=cohort6_contemp)
summary(model21a)

# CONDITIONAL POISSON REGRESSION 

model21b <- gnm(very_low_apgar_outcome ~ exposure, eliminate = as.factor(index), family = "poisson", data = cohort6_contemp)
summary(model21b)

exp(coef(model21b))

exp(confint(model21b, "exposure", method = "wald"))


model21c <- gnm(very_low_apgar_outcome ~ exposure + simd  + parity_cat, 
                eliminate = as.factor(index), family = "poisson", data = cohort6_contemp)
summary(model21c)

exp(coef(model21c))

exp(confint(model21c, "exposure", method = "wald"))
