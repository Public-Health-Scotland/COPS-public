################################################################
###### MATERNAL OUTCOMES MODELS - POST 20 WEEK INFECTIONS ######
################################################################

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


filter_post_20_week_gestation <- function(data) {
  
  filtered_data <- data %>% 
    filter(perinatal_gestation_at_index_date >= 20)
  
  gestation_ids <- data %>%
    filter(index %in% filtered_data$index)
  
  return(gestation_ids)
  
}


get_outcome_function <- function(data, cohort = 7, hbgroup = "All") { 
  
  data <- data %>%
    mutate(exposure = case_when(inf_or_uninf=="inf" ~ 1,
                                inf_or_uninf=="uninf" ~ 0)) %>%
    mutate(conception_half = case_when(conception_quarter==1 | conception_quarter==4 ~ "1. winter",
                                       conception_quarter==2 | conception_quarter==3 ~ "2. summer")) %>%
    mutate(diabetes_cat_2 = factor(diabetes_cat, levels = c("No - assumed & confirmed",
                                                            "Pre-existing diabetes",
                                                            "Gestational Diabetes/onset unknown"))) %>%
    mutate(hbres_group = case_when(hbres=="Greater Glasgow and Clyde" | hbres=="Lanarkshire"  ~ "1",
                                   is.na(hbres) ~ "2",
                                   TRUE ~ "2"))
  
  if(cohort == 7) {
    
    pregnancy_ids <- read.csv(paste0(folder_working_data, "cohort7_reinfections.csv"))
    
    data <- data %>%
      mutate(hypertension_outcome = case_when(hypertension_outcome == 1 ~ 1, 
                                              T ~ 0), 
             broad_hypertension_outcome = case_when(hypertension_broad_outcome == 1 ~ 1, 
                                                    T ~ 0))
    
  } else if (cohort == 8) {
    
    pregnancy_ids <- read.csv(paste0(folder_working_data, "cohort8_reinfections.csv"))
    
    data <- data %>%
      mutate(vte_outcome = case_when(vte_outcome == 1 ~ 1, 
                                     T ~ 0))
  } else if(cohort == 9) {
    
    pregnancy_ids <- read.csv(paste0(folder_working_data, "cohort9_reinfections.csv"))
    
    
    data <- data %>%
      mutate(bleeding_outcome = case_when(any_bleeding_outcome == 1 ~ 1, 
                                          T ~ 0))
  } else if(cohort == 10) {
    
    pregnancy_ids <- read.csv(paste0(folder_working_data, "cohort10_reinfections.csv"))
    
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
  
  filtered_data <- data %>%
    filter(!(index %in% pregnancy_ids$index))
  
  return(filtered_data)
  
}

##### COHORT 7: OUTCOME = HYPERTENSIVE DISORDERS OF PREGANACY #####
# only using pregnancies with singletons 
cohort7_contemp <- cohort7_infect_contemp_controls_singletons %>%
  filter_post_20_week_gestation(.) %>%
  get_outcome_function(cohort = 7)

cohort7_contemp_allotherhealthboards <- cohort7_infect_contemp_controls_singletons %>%
  filter_post_20_week_gestation(.) %>%
  get_outcome_function(cohort = 7, hbgroup = "Other")

cohort7_contemp_glasgow <- cohort7_infect_contemp_controls_singletons %>%
  filter_post_20_week_gestation(.) %>%
  get_outcome_function(cohort = 7, hbgroup = "Glasgow/Lanarkshire")

# no missing/unknown simd in glasgow so removing factor so adjusted models will run 
cohort7_contemp_glasgow <- cohort7_contemp_glasgow %>% mutate(simd = as.character(simd))

# two-by-two tables 
addmargins(table(cohort7_contemp$inf_or_uninf, cohort7_contemp$broad_hypertension_outcome))

# and glasgow tables
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

cohort8_contemp <- cohort8_infect_contemp_controls_singletons %>%
  filter_post_20_week_gestation(.) %>%
  get_outcome_function(cohort = 8)

# check numbers for each outcome
table(cohort8_contemp$vte_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort8_contemp$inf_or_uninf, cohort8_contemp$vte_outcome))

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

cohort9_contemp <- cohort9_infect_contemp_controls_singletons %>%
  filter_post_20_week_gestation(.) %>%
  get_outcome_function(cohort = 9) %>% mutate(simd = as.character(simd))

cohort9_contemp_allotherhealthboards <- cohort9_infect_contemp_controls_singletons %>%
  filter_post_20_week_gestation(.) %>%
  get_outcome_function(cohort = 9, hbgroup = "Other")

cohort9_contemp_glasgow <- cohort9_infect_contemp_controls_singletons %>%
  filter_post_20_week_gestation(.) %>%
  get_outcome_function(cohort = 9, hbgroup = "Glasgow/Lanarkshire")
cohort9_contemp_glasgow <- cohort9_contemp_glasgow %>% mutate(simd = as.character(simd))


# check numbers for each outcome
table(cohort9_contemp$bleeding_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort9_contemp$inf_or_uninf, cohort9_contemp$bleeding_outcome))
addmargins(table(cohort9_contemp_glasgow$inf_or_uninf, cohort9_contemp_glasgow$bleeding_outcome))

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

cohort10_contemp <- cohort10_infect_contemp_controls_singletons %>%
  filter_post_20_week_gestation(.) %>%
  get_outcome_function(cohort = 10)

# check numbers for each outcome
table(cohort10_contemp$icu_death_outcome, useNA = "ifany")

# two-by-two tables 
addmargins(table(cohort10_contemp$inf_or_uninf, cohort10_contemp$icu_death_outcome))

##### COHORT 8: MODEL ######
## UNADJUSTED MODEL ## 
model8 <- clogit(icu_death_outcome ~ exposure + strata(index), data=cohort10_contemp)
summary(model8)

## FULLY ADJUSTED MODEL ##
model8a <- clogit(icu_death_outcome ~ exposure + simd + ethnicity_cat + parity_cat +
                    UR2_categories + cv_clinical_vulnerability_category + diabetes_cat_2 + bmi_cat + smoking_status + strata(index), 
                  data=cohort10_contemp)
summary(model8a)





# save data for forest plot 
saveRDS(cohort7_contemp, paste0(folder_results, "post20weeks_hypertension.rds"))
saveRDS(cohort8_contemp, paste0(folder_results, "post20weeks_vte.rds"))
saveRDS(cohort9_contemp, paste0(folder_results, "post20weeks_bleeding.rds"))
saveRDS(cohort10_contemp, paste0(folder_results, "post20weeks_icu.rds"))

# save model output for forest plot

saveRDS(model1, paste0(folder_results, "models/subgroup_analysis_post20weeks_hypertension_unadjusted.rds"))
saveRDS(model1a, paste0(folder_results, "models/subgroup_analysis_post20weeks_hypertension_adjusted.rds"))

saveRDS(model4, paste0(folder_results, "models/subgroup_analysis_post20weeks_vte_unadjusted.rds"))
saveRDS(model4a, paste0(folder_results, "models/subgroup_analysis_post20weeks_vte_adjusted.rds"))

saveRDS(model5, paste0(folder_results, "models/subgroup_analysis_post20weeks_bleeding_unadjusted.rds"))
saveRDS(model5a, paste0(folder_results, "models/subgroup_analysis_post20weeks_bleeding_adjusted.rds"))

saveRDS(model8, paste0(folder_results, "models/subgroup_analysis_post20weeks_icu_unadjusted.rds"))
saveRDS(model8a, paste0(folder_results, "models/subgroup_analysis_post20weeks_icu_adjusted.rds"))
