#####################################################
###### MATERNAL OUTCOMES MODELS - VACCINATIONS ######
#####################################################

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

get_outcome_function <- function(data, cohort = 7, hbgroup = "All") { 
  
  data <- data %>%
    mutate(exposure = case_when(vacc_or_unvacc=="vacc" ~ 1,
                                vacc_or_unvacc=="unvacc" ~ 0)) %>%
    mutate(conception_half = case_when(conception_quarter==1 | conception_quarter==4 ~ "1. winter",
                                       conception_quarter==2 | conception_quarter==3 ~ "2. summer")) %>%
    mutate(diabetes_cat_2 = factor(diabetes_cat, levels = c("No - assumed & confirmed",
                                                            "Pre-existing diabetes",
                                                            "Gestational Diabetes/onset unknown"))) %>%
    mutate(hbres_group = case_when(hbres=="Greater Glasgow and Clyde" | hbres=="Lanarkshire"  ~ "1",
                                   is.na(hbres) ~ "2",
                                   TRUE ~ "2"))
  
  if(cohort == 7) {
    
    data <- data %>%
      mutate(hypertension_outcome = case_when(hypertension_outcome == 1 ~ 1, 
                                              T ~ 0), 
             broad_hypertension_outcome = case_when(hypertension_broad_outcome == 1 ~ 1, 
                                                    T ~ 0))
  } else if (cohort == 8) {
    
    data <- data %>%
      mutate(vte_outcome = case_when(vte_outcome == 1 ~ 1, 
                                     T ~ 0))
  } else if(cohort == 9) {
    
    data <- data %>%
      mutate(bleeding_outcome = case_when(any_bleeding_outcome == 1 ~ 1, 
                                          T ~ 0))
  } else if(cohort == 10) {
    
    data <- data %>%
      mutate(icu_death_outcome = case_when(icu_and_death_outcome == 1 ~ 1, 
                                           T ~ 0))
  }
  
  if(hbgroup == "Other") {
    
    data <- data %>%
      filter(hbres_group == "2")
    
  } else if(hbgroup == "Glasgow/Lanarkshire") {
    
    data <- data %>%
      filter(hbres_group == "1")
    
  } 
  
  return(data)
  
}


##### COHORT 7: OUTCOME = HYPERTENSIVE DISORDERS OF PREGNANCY #####
# only using pregnancies with singletons 
cohort7_contemp <- cohort7_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 7)

cohort7_contemp_allotherhealthboards <- cohort7_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 7, hbgroup = "Other")

cohort7_contemp_glasgow <- cohort7_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 7, hbgroup = "Glasgow/Lanarkshire")

# no missing/unknown simd in glasgow so removing factor so adjusted models will run 
cohort7_contemp_glasgow <- cohort7_contemp_glasgow %>% mutate(simd = as.character(simd))

# check numbers for each outcome
table(cohort7_contemp$hypertension_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort7_contemp$vacc_or_unvacc, cohort7_contemp$broad_hypertension_outcome))

# and glasgow tables 
table(cohort7_contemp_glasgow$hypertension_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort7_contemp_glasgow$exposure, cohort7_contemp_glasgow$hypertension_outcome))
addmargins(table(cohort7_contemp_glasgow$exposure, cohort7_contemp_glasgow$broad_hypertension_outcome))

##### COHORT 7: MODEL ######

##### HYPERTENSION WITH A BROAD DEFINITION #####
## UNADJUSTED MODEL ##
model1 <- clogit(broad_hypertension_outcome ~ exposure + strata(index), data=cohort7_contemp)
summary(model1)

## FULLY ADJUSTED MODEL ##
model1a <- clogit(broad_hypertension_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort7_contemp)
summary(model1a)

# and do same for glasgow
model2 <- clogit(broad_hypertension_outcome ~ exposure + strata(index), data=cohort7_contemp_glasgow)
summary(model2)

## FULLY ADJUSTED MODEL ##
model2a <- clogit(broad_hypertension_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort7_contemp_glasgow)
summary(model2a)

# and all other healthboards
model3 <- clogit(broad_hypertension_outcome ~ exposure + strata(index), data=cohort7_contemp_allotherhealthboards)
summary(model3)

## FULLY ADJUSTED MODEL ##
model3a <- clogit(broad_hypertension_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort7_contemp_allotherhealthboards)
summary(model3a)

##### COHORT 8: OUTCOME = VENOUS THROMBOEMBOLISM #####
# only using pregnancies with singletons 

cohort8_contemp <- cohort8_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 8)

# check numbers for each outcome
table(cohort8_contemp$vte_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort8_contemp$vacc_or_unvacc, cohort8_contemp$vte_outcome))

##### COHORT 8: MODEL ######
## UNADJUSTED MODEL ## 
model4 <- clogit(vte_outcome ~ exposure + strata(index), data=cohort8_contemp)
summary(model4)

## FULLY ADJUSTED MODEL ##
model4a <- clogit(vte_outcome ~ exposure + simd + parity_cat +
                    strata(index), 
                  data=cohort8_contemp)
summary(model4a)

##### COHORT 9: OUTCOME = PREGNANCY RELATED BLEEDING #####
# only using pregnancies with singletons 

cohort9_contemp <- cohort9_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 9)

cohort9_contemp_allotherhealthboards <- cohort9_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 9, hbgroup = "Other")

cohort9_contemp_glasgow <- cohort9_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 9, hbgroup = "Glasgow/Lanarkshire")
cohort9_contemp_glasgow <- cohort9_contemp_glasgow %>% mutate(simd = as.character(simd))


# check numbers for each outcome
table(cohort9_contemp$bleeding_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort9_contemp$vacc_or_unvacc, cohort9_contemp$bleeding_outcome))
addmargins(table(cohort9_contemp_glasgow$vacc_or_unvacc, cohort9_contemp_glasgow$bleeding_outcome))

##### COHORT 9: MODEL ######
## UNADJUSTED MODEL ## 
model5 <- clogit(bleeding_outcome ~ exposure + strata(index), data=cohort9_contemp)
summary(model5)

## FULLY ADJUSTED MODEL ##
model5a <- clogit(bleeding_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort9_contemp)
summary(model5a)


# and do the same for glasgow
model6 <- clogit(bleeding_outcome ~ exposure + strata(index), data=cohort9_contemp_glasgow)
summary(model6)

## FULLY ADJUSTED MODEL ##
model6a <- clogit(bleeding_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort9_contemp_glasgow)
summary(model6a)


# and alll other healthboards
model7 <- clogit(bleeding_outcome ~ exposure + strata(index), data=cohort9_contemp_allotherhealthboards)
summary(model7)

## FULLY ADJUSTED MODEL ##
model7a <- clogit(bleeding_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort9_contemp_allotherhealthboards)
summary(model7a)

##### COHORT 10: OUTCOME = ICU/DEATH #####
# only using pregnancies with singletons 

cohort10_contemp <- cohort10_vacc_contemp_controls_singletons %>%
  get_outcome_function(cohort = 10)

# check numbers for each outcome
table(cohort10_contemp$icu_death_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort10_contemp$vacc_or_unvacc, cohort10_contemp$icu_death_outcome))

##### COHORT 8: MODEL ######
## UNADJUSTED MODEL ## 
model8 <- clogit(icu_death_outcome ~ exposure + strata(index), data=cohort10_contemp)
summary(model8)

## FULLY ADJUSTED MODEL ##
model8a <- clogit(icu_death_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort10_contemp)
summary(model8a)
