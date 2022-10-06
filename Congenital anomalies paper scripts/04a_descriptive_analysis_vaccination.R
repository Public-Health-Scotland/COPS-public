#****************************************************************************************************************
#CREATE DESCRIPTIVE TABLES FOR CONGENITAL ANOMALIES ANALYSIS
#sociodemographic description of all the cohorts (by vaccination status, vaccination type and infection status)
#exposure characteristics of all the exposed cohorts
#outcome characteristics of all the outcome cohorts
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

folder_working_data <- "x"
folder_results <- "x"

###### IMPORT DATA  ######

cohort1_vacc_contemp_controls <- readRDS(paste0(folder_working_data, "matched_congenital_primary_12wks_contemporary.rds")) 
cohort2_vacc_contemp_controls_sensitivity <- readRDS(paste0(folder_working_data, "matched_congenital_sensitivity1_contemporary.rds")) 
cohort3_vacc_contemp_controls_sensitivity <- readRDS(paste0(folder_working_data, "matched_congenital_sensitivity2_postconception2.rds")) 

cohort1_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_congenital_infection_primary_12wks_contemporary_final.rds")) 
cohort2_infect_contemp_controls_sensitivity <- readRDS(paste0(folder_working_data, "matched_congenital_infection_sensitivity_contemporary_final.rds")) 

check <- cohort1_infect_contemp_controls %>% filter(is.na(pregnancy_id))
cohort1_infect_contemp_controls <- cohort1_infect_contemp_controls %>% filter(!(index %in% check$index))

check <- cohort2_infect_contemp_controls_sensitivity %>% filter(is.na(pregnancy_id))
cohort2_infect_contemp_controls_sensitivity <- cohort2_infect_contemp_controls_sensitivity %>% filter(!(index %in% check$index))

check <- subset(cohort1_vacc_contemp_controls[,c("index", "vacc_or_unvacc", "mother_age_at_conception")])

#****************************************
####SOCIO-DEMOGRAPHIC CHARACTERSTICS####
#****************************************

# FUNCTIONS 

# descriptives # 

get_characteristics_descriptives <- function(data, exposure = "vaccination", control = "contemporary") {
  
  if(exposure == "vaccination") {
    
    data <- data %>%
      mutate(exposure = ifelse(vacc_or_unvacc == "vacc", "exposed", "unexposed"))
    
  } else if(exposure == "infection") {
    
    data <- data %>%
      mutate(exposure = ifelse(inf_or_uninf == "inf", "exposed", "unexposed")) 
      
  }
  
  data <- data %>%
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
                                                                        "smoker")))  %>%
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
           diabetes_cat = factor(diabetes_cat, levels = c("No - assumed & confirmed",
                                                          "Pre-existing diabetes",
                                                          "Gestational Diabetes/onset unknown",
                                                          "Unknown"))) %>%
    mutate(multiplicity = case_when(total_foetus==1 ~ "1. Singleton",
                                     total_foetus>=2 ~ "2. Multiple"))
  
  table <- bind_rows(
    
    # n pregnancies 
    
    data %>%
      group_by(pregnancy_id_orig) %>% 
      slice(1) %>%
      ungroup %>%
      group_by(exposure) %>%
      tally() %>% 
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "n_preg:"),
    
    # n births 
    
    data %>%
      group_by(exposure) %>% 
      tally() %>% 
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "n_births:"), 
    
    # median age (at conception)
    
    data %>%
      group_by(exposure) %>%
      summarise(median_age = median(mother_age_at_conception), 
                min_age = min(mother_age_at_conception), 
                max_age = max(mother_age_at_conception),
                sd_age =  sd(mother_age_at_conception)) %>%
      pivot_longer(!exposure, names_to = "age") %>%
      pivot_wider(names_from = exposure, values_from = value) %>% 
      mutate(rowname = paste0(age, ":")) %>%
      select(-age), 
    
    # deprivation
    data %>%
      group_by(exposure, simd) %>%
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = paste0("deprivation:", simd)), 
    
    # ethnicity
    data %>%
      group_by(exposure, ethnicity_cat) %>%
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = paste0("ethnicity:", ethnicity_cat)),
    
    # urban/rural status
    data %>%
      group_by(exposure, UR6_categories) %>%
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = paste0("UR6_categories:", UR6_categories)),

    # clinical vulnerability
    data %>%
      group_by(exposure, cv_clinical_vulnerability_category) %>%
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = paste0("clinical_vulnerability:", cv_clinical_vulnerability_category)),
    
    # diabetes
    data %>%
      group_by(exposure, diabetes_cat) %>%
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = paste0("diabetes:", diabetes_cat)),
    
    # smoking
    data %>%
      group_by(exposure, smoking_status) %>%
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = paste0("smoking_status:", smoking_status)),
    
    # bmi
    data %>%
      group_by(exposure, bmi_cat) %>%
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = paste0("bmi:", bmi_cat)),
    
    # multiples
    data %>%
      group_by(exposure, multiplicity) %>%
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = paste0("Multiplicity:", multiplicity)),
    
    # outcomes
    data %>%
      group_by(exposure, overall_outcome.f) %>%
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = paste0("Birth outcome:", overall_outcome.f)),
    
    # outcomes
    data %>%
      group_by(exposure, hbres) %>%
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = paste0("Birth outcome:", hbres))    
    
  )
  
  table <- table %>% 
    select(rowname, exposed, unexposed) %>%
    mutate(across(everything(), as.character)) %>%
    separate(rowname, sep = ":", into = c("category", "sub_category")) %>%
    group_by(category) %>%
    mutate(percentage = round(as.numeric(exposed)/sum(as.numeric(exposed), na.rm = T)*100, 1), 
           percentage_control = round(as.numeric(unexposed)/sum(as.numeric(unexposed), na.rm = T)*100, 1))
  
  if(exposure == "vaccination") {
    
    table <- table %>% rename(vacc = "exposed")
    
  } else if(exposure == "infection") {
    
    table <- table %>% rename(inf = "exposed")
  }
  
  if(control == "historical") {
    
    table <- table %>% select(category, sub_category, unexposed, percentage_control) %>% 
      rename(historic = "unexposed", percentage_historic = "percentage_control")
    
  } else if(control == "contemporary" ) {
    
    table <- table %>% rename(contemporary = "unexposed", percentage_contemporary = "percentage_control")
    
  }
  
  return(table)
  
}

test1 <- get_characteristics_descriptives(cohort1_vacc_contemp_controls, control = "contemporary")
           
table1 <-  test1 %>% 
  mutate(vacc_all = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
         select(c("category", "sub_category", "vacc_all", "vacc_contemporary" ))
write.csv(table1, paste(folder_results, "Sociodemographic_by_vacc_cohort.csv", sep = ''))

test2 <- get_characteristics_descriptives(cohort1_infect_contemp_controls, exposure = "infection")
                   
table2 <-  test2 %>% 
  mutate(inf_all = paste0(inf, " (", percentage, "%)"), 
         inf_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
         select(c("category", "sub_category", "inf_all", "inf_contemporary" ))
write.csv(table2, paste(folder_results, "Sociodemographic_by_inf_cohort.csv", sep = ''))

test3 <- get_characteristics_descriptives(cohort3_vacc_contemp_controls_sensitivity, control = "contemporary")

table3 <-  test3 %>% 
  mutate(vacc_all = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("category", "sub_category", "vacc_all", "vacc_contemporary" ))
write.csv(table3, paste(folder_results, "Sociodemographic_by_vacc_cohort_sense2.csv", sep = ''))

#vaccine specific cohorts

#create a variable to capture vaccination type on all matched sets
table(cohort1_vacc_contemp_controls$congenital_anomaly_type_of_vaccination_during_period)
cohort1_vacc_contemp_controls <- cohort1_vacc_contemp_controls %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(vaccination_subgroup = first_(congenital_anomaly_type_of_vaccination_during_period)) %>%
  ungroup()
addmargins(table(cohort1_vacc_contemp_controls$vaccination_subgroup))

#make vaccine specific datasets
cohort1_vacc_contemp_controls.mrna <- cohort1_vacc_contemp_controls %>%
  filter(vaccination_subgroup == "Pfizer" | vaccination_subgroup == "Moderna")
cohort1_vacc_contemp_controls.az <- cohort1_vacc_contemp_controls %>%
  filter(vaccination_subgroup == "AstraZeneca")

#make descriptive tables 
test1.mrna <- get_characteristics_descriptives(cohort1_vacc_contemp_controls.mrna, control = "contemporary")
test1.az <- get_characteristics_descriptives(cohort1_vacc_contemp_controls.az, control = "contemporary")

test1.mrna.tab <-  test1.mrna %>% 
  mutate(vacc_mrna = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary_mrna = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("category", "sub_category", "vacc_mrna", "vacc_contemporary_mrna" ))

table1.az.tab <-  test1.az %>% 
  mutate(vacc_astrazeneca = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary_astrazeneca = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
         select(c("category", "sub_category", "vacc_astrazeneca", "vacc_contemporary_astrazeneca" ))

CA_vaccine_type_table <- test1.mrna.tab %>%
  full_join(., table1.az.tab )
write.csv(CA_vaccine_type_table, paste(folder_results, "Sociodemographic_by_vacc_cohort_vacc_type_mrna.csv", sep = ''))

#****************************************
####EXPOSURE CHARACTERSTICS####
#****************************************

#VACCINATION DESCRIPTION

get_vaccination_descriptives <- function(data, control = "contemporary") {
  
data <- data %>%
      mutate(exposure = ifelse(vacc_or_unvacc == "vacc", "exposed", "unexposed")) %>%
      mutate(preg_id = gsub('.{2}$', "", pregnancy_id))
    
dose_1_valid <- c("dose 1 & dose 2","dose 1 & dose 2 & dose 3", "dose 1 & dose 3", "only dose 1" )
dose_2_valid <- c("dose 2 & dose 3", "only dose 2", "dose 2 & dose 3 & dose 4")
dose_3_valid <- c("dose 3 & dose 4", "only dose 3", "dose 3 & dose 4 & dose 5")
dose_4_valid <- c("only dose 4", "dose 4 & dose 5" )
dose_5_valid <- c("only dose 5" )

one_dose_in_exposure <- c("only dose 1", "only dose 2", "only dose 3", "only dose 4", "only dose 5")
two_dose_in_exposure <- c("dose 1 & dose 2", "dose 1 & dose 3", "dose 2 & dose 3", "dose 3 & dose 4", "dose 4 & dose 5")
three_dose_in_exposure <- c("dose 1 & dose 2 & dose 3", "dose 2 & dose 3 & dose 4", "dose 3 & dose 4 & dose 5")
four_dose_in_exposure <- c("dose 1 & dose 2 & dose 3 & dose 4", "dose 2 & dose 3 & dose 4 & dose 5", "dose 1 & dose 3 & dose 4", "dose 1 & dose 2 & dose 4")

data <- data  %>%
  mutate(first_dose = case_when(congenital_anomaly_vaccination_dose_information %in% dose_1_valid ~ "Dose 1", 
                                congenital_anomaly_vaccination_dose_information %in% dose_2_valid ~ "Dose 2",
                                congenital_anomaly_vaccination_dose_information %in% dose_3_valid ~ "Dose 3",
                                congenital_anomaly_vaccination_dose_information %in% dose_4_valid ~ "Dose 4",
                                congenital_anomaly_vaccination_dose_information %in% dose_5_valid ~ "Dose 5")) %>%
  mutate(no_of_doses_in_exposure = case_when(congenital_anomaly_vaccination_dose_information %in% one_dose_in_exposure ~ "1", 
                                             congenital_anomaly_vaccination_dose_information %in% two_dose_in_exposure ~ "2+", 
                                             congenital_anomaly_vaccination_dose_information %in% three_dose_in_exposure ~ "2+",
                                             congenital_anomaly_vaccination_dose_information %in% four_dose_in_exposure ~ "2+"), 
         dose_information = congenital_anomaly_vaccination_dose_information) %>%
    mutate(gestation_at_reference_date_categories = case_when(
    congenital_anomaly_gestation_at_reference_date < 2 ~ "1. Pre-conception",
    congenital_anomaly_gestation_at_reference_date >= 2 & congenital_anomaly_gestation_at_reference_date <= 9 ~ "2. 2-9 weeks",
    congenital_anomaly_gestation_at_reference_date >= 10 & congenital_anomaly_gestation_at_reference_date <= 13 ~ "3. 10-13 weeks",
    congenital_anomaly_gestation_at_reference_date >= 14 & congenital_anomaly_gestation_at_reference_date <= 20 ~ "4. 14-20 weeks"))


table2 <- bind_rows(
  
  # Gestation at first vaccination within exposure period 
  
  data %>%
    group_by(exposure, gestation_at_reference_date_categories) %>%
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = paste0("Gestation at reference date:", gestation_at_reference_date_categories)), 
  
  # Number of vaccinations
  
  data %>%
    group_by(exposure, no_of_doses_in_exposure) %>%
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = paste0("no_of_doses_in_exposure_period: ", no_of_doses_in_exposure)),
  
  # Dose number at first vaccination within exposure period
  
  data %>%
    group_by(exposure, first_dose) %>%
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = paste0("first_vaccine_dose_in_exposure: ", first_dose)),
  
  # vaccine type
  
  data %>%
    group_by(exposure, congenital_anomaly_type_of_vaccination_during_period) %>%
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = paste0("Vaccine type:", congenital_anomaly_type_of_vaccination_during_period))
  )

table2 <- table2 %>% 
  select(rowname, exposed, unexposed) %>%
  mutate(across(everything(), as.character)) %>%
  separate(rowname, sep = ":", into = c("category", "sub_category")) %>%
  group_by(category) %>%
  mutate(percentage = round(as.numeric(exposed)/sum(as.numeric(exposed), na.rm = T)*100, 1), 
         percentage_control = round(as.numeric(unexposed)/sum(as.numeric(unexposed), na.rm = T)*100, 1))


  
  table2 <- table2 %>% rename(vacc = "exposed")
  
if(control == "historical") {
  
  table2 <- table2 %>% select(category, sub_category, unexposed, percentage_control) %>% 
    rename(historic = "unexposed", percentage_historic = "percentage_control")
  
} else if(control == "contemporary" ) {
  
  table2 <- table2 %>% rename(contemporary = "unexposed", percentage_contemporary = "percentage_control")
  
}

return(table2)

}

test2 <- get_vaccination_descriptives(cohort1_vacc_contemp_controls, control = "contemporary")

table2_vacc <-  test2 %>% 
  mutate(vacc_all = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("category", "sub_category", "vacc_all", "vacc_contemporary" ))
write.csv(table2_vacc, paste(folder_results, "Vaccination_by_vacc_cohort.csv", sep = ''))

test2.mrna <- get_vaccination_descriptives(cohort1_vacc_contemp_controls.mrna, control = "contemporary")
test2.az <- get_vaccination_descriptives(cohort1_vacc_contemp_controls.az, control = "contemporary")

table2.mrna.tab <-  test2.mrna %>% 
  mutate(vacc_mrna = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary_mrna = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("category", "sub_category", "vacc_mrna", "vacc_contemporary_mrna" ))

table2.az.tab <-  test2.az %>% 
  mutate(vacc_astrazeneca = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary_astrazeneca = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("category", "sub_category", "vacc_astrazeneca", "vacc_contemporary_astrazeneca" ))

CA_vaccine_type_table_vaccdesc <- table2.mrna.tab %>%
  full_join(., table2.az.tab)
write.csv(CA_vaccine_type_table_vaccdesc, paste(folder_results, "Vaccination_by_vacc_cohort_vacc_type.csv", sep = ''))

#INFECTION DESCRIPTION

data <-  cohort1_infect_contemp_controls %>%
  mutate(exposure = ifelse(inf_or_uninf == "inf", "exposed", "unexposed")) %>%
  mutate(preg_id = gsub('.{2}$', "", pregnancy_id))

data <- data %>%
  mutate(gest_group = case_when(
    (congenital_anomaly_gestation_at_index_date<2) ~ "1. pre-conception",
    (congenital_anomaly_gestation_at_index_date>=2 & congenital_anomaly_gestation_at_index_date<10) ~ "2. 2-9 weeks",
    (congenital_anomaly_gestation_at_index_date>=10 & congenital_anomaly_gestation_at_index_date<14) ~ "3. 11-13 weeks",
    (congenital_anomaly_gestation_at_index_date>=14 & congenital_anomaly_gestation_at_index_date<20) ~ "4. 14-20 weeks"
  ))

#number of infections
data$infection1 <- ifelse(!is.na(data$index_date_covid_infection_1) &
                                                as.numeric(data$index_date_covid_infection_1-data$est_conception_date)>=-6*7 &
                                                data$index_date_covid_infection_1<=data$congenital_anomaly_vaccination_timing_period_end &
                                                data$index_date_covid_infection_1<=data$pregnancy_end_date, 1,0)

data$infection2 <- ifelse(!is.na(data$index_date_covid_infection_2) &
                                                as.numeric(data$index_date_covid_infection_2-data$est_conception_date)>=-6*7 &
                                                data$index_date_covid_infection_2<=data$congenital_anomaly_vaccination_timing_period_end &
                                                data$index_date_covid_infection_2<=data$pregnancy_end_date, 1,0)

data$infection3 <- ifelse(!is.na(data$index_date_covid_infection_3) &
                                                as.numeric(data$index_date_covid_infection_3-data$est_conception_date)>=-6*7 &
                                                data$index_date_covid_infection_3<=data$congenital_anomaly_vaccination_timing_period_end &
                                                data$index_date_covid_infection_3<=data$pregnancy_end_date, 1,0)

data$infection4 <- ifelse(!is.na(data$index_date_covid_infection_4) &
                                                as.numeric(data$index_date_covid_infection_4-data$est_conception_date)>=-6*7 &
                                                data$index_date_covid_infection_4<=data$congenital_anomaly_vaccination_timing_period_end &
                                                data$index_date_covid_infection_4<=data$pregnancy_end_date, 1,0)

table(data$infection1)
table(data$infection2)
table(data$infection3)
table(data$infection4)

data$infection <- ifelse(data$infection1==1 | data$infection2==1 | data$infection3==1 | data$infection4==1, 1, 0)
addmargins(table(data$infection, exclude=NULL))

data <- data %>%
  mutate(infection_no = case_when(
    (infection1==1 & infection2==1 & infection3==1) ~ "3",
    (infection1==1 & infection2==1 & infection3==0) ~ "2",
    (infection1==1 & infection2==0 & infection3==1) ~ "2",
    (infection1==0 & infection2==1 & infection3==1) ~ "2",
    (infection1==1 & infection2==0 & infection3==0) ~ "1",
    (infection1==0 & infection2==1 & infection3==0) ~ "1",
    (infection1==0 & infection2==0 & infection3==1) ~ "1"
  ))

table(data$infection_no)

#symptomatic or non sypmtomatic
data <- data %>%
  mutate(symptomatic_infection = case_when(
    (infection1==1 & final_symptomatic_covid_infection_1=="true") ~ "Symptomatic",
    (infection2==1 & final_symptomatic_covid_infection_2=="true") ~ "Symptomatic",
    (infection3==1 & final_symptomatic_covid_infection_3=="true") ~ "Symptomatic"
  ))

table <- bind_rows(
  
  # Gestation at first infecton within exposure period 
  
  data %>%
    group_by(exposure, gest_group) %>%
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = paste0("Gestation at reference date:", gest_group)), 
  
  # Number of infections
  
  data %>%
    group_by(exposure, infection_no) %>%
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = paste0("no_of_infections_in_exposure_period: ", infection_no)),
  
  # Symptomatic infection
  
  data %>%
    group_by(exposure, symptomatic_infection) %>%
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = paste0("symptomatic infection: ", symptomatic_infection))
  
 )

table <- table %>% 
  select(rowname, exposed) %>%
  mutate(across(everything(), as.character)) %>%
  separate(rowname, sep = ":", into = c("category", "sub_category")) %>%
  group_by(category) %>%
  mutate(percentage = round(as.numeric(exposed)/sum(as.numeric(exposed), na.rm = T)*100, 1))

table_inf <-  table %>% 
  mutate(inf_all = paste0(exposed, " (", percentage, "%)")) %>%
  select(c("category", "sub_category", "inf_all"))
write.csv(table_inf, paste(folder_results, "Infection_by_inf_cohort.csv", sep = ''))

#******************************
####DESCRIPTION OF OUTCOMES####
#******************************

#Overall number of congenital anomalies
get_outcomes_descriptives <- function(data, exposure = "vaccination", control = "contemporary") {
  
  if(exposure == "vaccination") {
    
    data <- data %>%
      mutate(exposure = ifelse(vacc_or_unvacc == "vacc", "exposed", "unexposed"))
    
  } else if(exposure == "infection") {
    
    data <- data %>%
      mutate(exposure = ifelse(inf_or_uninf == "inf", "exposed", "unexposed")) 
  }
  
data <- data %>%
    mutate(preg_id = gsub('.{2}$', "", pregnancy_id)) 

data2 <- data %>% filter(outcome=="Live birth")
data3 <- data %>% filter(all_14_all_anomalies==1)
data4 <- data2 %>% filter(all_14_all_anomalies==1)

#calculate total live birth prevalence of any anomaly
live_prev <- data2 %>%
  group_by(exposure, all_14_all_anomalies) %>%
  tally() %>%
  pivot_wider(names_from = exposure, values_from = n) %>% 
  mutate(rowname = paste0("Live birth anomaly prevalence:", all_14_all_anomalies))

live_prev <- live_prev %>% 
  select(rowname, exposed, unexposed) %>%
  mutate(across(everything(), as.character)) %>%
  separate(rowname, sep = ":", into = c("category", "sub_category")) %>%
  group_by(category) %>%
  mutate(percentage = round(as.numeric(exposed)/sum(as.numeric(exposed), na.rm = T)*1000), 
         percentage_control = round(as.numeric(unexposed)/sum(as.numeric(unexposed), na.rm = T)*1000))

live_prev <- live_prev %>%
  filter(sub_category==1) %>%
  select(category, percentage, percentage_control) %>% 
  rename(exposed = "percentage") %>%
  rename(unexposed = "percentage_control") %>%
  rename(rowname = "category")

#calculate total live birth prevalence of any non-genetic anomaly
live_prev_ngca <- data2 %>%
  mutate(genetic_anomalies=ifelse(
    all_13_chromosomal==1 | all_12_1_skeletal_dysplasias==1 | all_12_11_genetic_syndromes_and_microdeletions==1, 1, 0)) %>%
  mutate(nongenetic_anomalies=ifelse(
    genetic_anomalies==0 & all_14_all_anomalies==1, 1, 0)) %>%
  group_by(exposure, nongenetic_anomalies) %>%
  tally() %>%
  pivot_wider(names_from = exposure, values_from = n) %>% 
  mutate(rowname = paste0("Live birth nongenetic anomaly prevalence:", nongenetic_anomalies))

live_prev_ngca <- live_prev_ngca %>% 
  select(rowname, exposed, unexposed) %>%
  mutate(across(everything(), as.character)) %>%
  separate(rowname, sep = ":", into = c("category", "sub_category")) %>%
  group_by(category) %>%
  mutate(percentage = round(as.numeric(exposed)/sum(as.numeric(exposed), na.rm = T)*1000), 
         percentage_control = round(as.numeric(unexposed)/sum(as.numeric(unexposed), na.rm = T)*1000))

live_prev_ngca <- live_prev_ngca %>%
  filter(sub_category==1) %>%
  select(category, percentage, percentage_control) %>% 
  rename(exposed = "percentage") %>%
  rename(unexposed = "percentage_control") %>%
  rename(rowname = "category")

table3a_outcome <- bind_rows(
  
  #total number of live births
  data2 %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "live births:"),
  
  # n congenital anomalies outcome 
  data3 %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "congenital anomaly:"),
  
  #n congenital anomalies outcome
  data4 %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "congenital anomaly live births:"),
  
  #live birth prevalence
  live_prev, 
  
  #total number of non-genetic anomalies  
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Non genetic congenital anomaly:"),
  
  #total number of live birth non-genetic anomalies  
  data2 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Non genetic congenital anomaly in live births:"),
  
  #non-genetic live birth prevalence 
  live_prev_ngca
)

table3a_outcome <-  table3a_outcome %>%
  select(rowname, exposed, unexposed)

if(exposure == "vaccination") {

  table3a_outcome <- table3a_outcome %>% rename(vacc = "exposed")

} else if(exposure == "infection") {

 table3a_outcome <- table3a_outcome %>% rename(inf = "exposed")
 }

 if(control == "historical") {

   table3a_outcome <- table3a_outcome %>% rename(historic = "unexposed")

 } else if(control == "contemporary" ) {
   
   table3a_outcome <- table3a_outcome %>% rename(contemporary = "unexposed")
   
 }

}

outcome_table3a <- get_outcomes_descriptives(cohort1_vacc_contemp_controls, exposure = "vaccination", control = "contemporary")
write.csv(outcome_table3a, paste(folder_results, "Outcome_by_vacc_cohort.csv", sep = ''))

outcome_table3b <- get_outcomes_descriptives(cohort1_infect_contemp_controls, exposure = "infection", control = "contemporary")
write.csv(outcome_table3b, paste(folder_results, "Outcome_by_inf_cohort.csv", sep = ''))

test3.mrna <- get_outcomes_descriptives(cohort1_vacc_contemp_controls.mrna, exposure = "vaccination", control = "contemporary")

test3.az <- get_outcomes_descriptives(cohort1_vacc_contemp_controls.az, exposure = "vaccination", control = "contemporary")

test3.mrna.tab <-  test3.mrna %>% 
  mutate(vacc_mrna = vacc, 
         vacc_contemporary_mrna = contemporary) %>%
  select(c("rowname", "vacc_mrna", "vacc_contemporary_mrna" ))

test3.az.tab <-  test3.az %>% 
  mutate(vacc_astrazeneca = vacc, 
         vacc_contemporary_astrazeneca = contemporary) %>%
  select(c("rowname", "vacc_astrazeneca", "vacc_contemporary_astrazeneca" ))

CA_vaccine_type_table_outcome_mrna <- test3.mrna.tab %>%
  full_join(., test3.az.tab)
write.csv(CA_vaccine_type_table_outcome_mrna, paste(folder_results, "Outcome_by_vacc_cohort_vacc_type_mrna.csv", sep = ''))

#Types of congenital anomaly specific table

get_outcomes_descriptives2 <- function(data, exposure = "vaccination", control = "contemporary") {
  
  if(exposure == "vaccination") {
    
    data <- data %>%
      mutate(exposure = ifelse(vacc_or_unvacc == "vacc", "exposed", "unexposed"))
    
  } else if(exposure == "infection") {
    
    data <- data %>%
      mutate(exposure = ifelse(inf_or_uninf == "inf", "exposed", "unexposed")) 
  }
  
data <- data %>%
    mutate(preg_id = gsub('.{2}$', "", pregnancy_id)) 
  
data2 <- data %>% filter(outcome=="Live birth")
data3 <- data %>% filter(all_14_all_anomalies==1)
data4 <- data2 %>% filter(all_14_all_anomalies==1)

#Types of congenital anomaly
table4b <- bind_rows(
  #specific types of congenital anomalies
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_1_nervous_system==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Nervous system:"),
  
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_2_eye==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Eye:"),
  
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_3_ear_face_and_neck==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Ear, face and neck:"),
  
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_4_congenital_heart_defects==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Congenital heart defects:"),
  
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_5_respiratory==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Respiratory:"),
  
    data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_6_oro_facial_clefts==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Oro-facial clefts:"),
  
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_7_digestive_system==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Digestive:"),
  
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_8_abdominal_wall_defects==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Abdominal wall defects:"),
  
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_9_urinary==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Urinary:"),
  
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_10_genital==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Genital:"),
  
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_11_limb==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Limb defect:"),
  
  data3 %>%
    filter(all_13_chromosomal!=1) %>%
    filter(all_12_1_skeletal_dysplasias!=1) %>%
    filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
    filter(all_12_other_anomalies_total==1) %>%
    group_by(exposure) %>% 
    tally() %>%
    pivot_wider(names_from = exposure, values_from = n) %>% 
    mutate(rowname = "Other anomalies/syndromes:")
)

 table4b$denom_exposed <- data3 %>%
   filter(exposure=="exposed") %>%
   filter(all_13_chromosomal!=1) %>%
   filter(all_12_1_skeletal_dysplasias!=1) %>%
   filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
   count()

 table4b$denom_unexposed <- data3 %>%
   filter(exposure=="unexposed") %>%
   filter(all_13_chromosomal!=1) %>%
   filter(all_12_1_skeletal_dysplasias!=1) %>%
   filter(all_12_11_genetic_syndromes_and_microdeletions!=1) %>%
   count()

 table4b$denom_unexposed_2 <- as.numeric(unlist(table4b$denom_unexposed))
 table4b$denom_exposed_2 <- as.numeric(unlist(table4b$denom_exposed))

 table4b <- table4b %>%
   mutate(percentage=exposed/denom_exposed_2*100) %>%
   mutate(percentage=round(percentage,1)) %>%
   mutate(percentage_control=unexposed/denom_unexposed_2*100) %>%
   mutate(percentage_control=round(percentage_control,1))
 
 table4b <-  table4b %>%
   select(rowname, exposed, unexposed, percentage, percentage_control)
 
 if(exposure == "vaccination") {
  
   table4b <- table4b %>% rename(vacc = "exposed")
   
 } else if(exposure == "infection") {
   
   table4b <- table4b %>% rename(inf = "exposed")
 }

if(control == "historical") {

  table4b <- table4b %>% rename(historic = "unexposed", percentage_historic = "percentage_control") 

} else if(control == "contemporary" ) {

  table4b <- table4b %>% rename(contemporary = "unexposed", percentage_contemporary = "percentage_control")

}

return(table4b)

}

outcome_table4a <- get_outcomes_descriptives2(cohort1_vacc_contemp_controls, exposure = "vaccination", control = "contemporary")

table4a <- outcome_table4a %>% 
  mutate(vacc_all = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("rowname", "vacc_all", "vacc_contemporary" ))
write.csv(table4a , paste(folder_results, "Outcome2_by_vacc_cohort.csv", sep = ''))

outcome_table4b <- get_outcomes_descriptives2(cohort1_infect_contemp_controls, exposure = "infection", control = "contemporary")

table4b <- outcome_table4b %>% 
  mutate(inf_all = paste0(inf, " (", percentage, "%)"), 
         inf_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("rowname", "inf_all", "inf_contemporary" ))
write.csv(table4b, paste(folder_results, "Outcome2_by_inf_cohort.csv", sep = ''))

outcome_table4c <- get_outcomes_descriptives2(cohort1_vacc_contemp_controls.mrna, exposure = "vaccination", control = "contemporary")

table4c <- outcome_table4c %>% 
  mutate(vacc_all = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("rowname", "vacc_all", "vacc_contemporary" ))
write.csv(table4c, paste(folder_results, "Outcome2_by_vacc_cohort_mrna.csv", sep = ''))

outcome_table4d <- get_outcomes_descriptives2(cohort1_vacc_contemp_controls.az, exposure = "vaccination", control = "contemporary")

table4d <- outcome_table4d %>% 
  mutate(vacc_all = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("rowname", "vacc_all", "vacc_contemporary" ))
write.csv(table4d , paste(folder_results, "Outcome2_by_vacc_cohort_az.csv", sep = ''))

#get type and number of non genetic congenital anomaly
get_outcomes_descriptives3 <- function(data, exposure = "vaccination", control = "contemporary") {
  
  if(exposure == "vaccination") {
    
    data <- data %>%
      mutate(exposure = ifelse(vacc_or_unvacc == "vacc", "exposed", "unexposed"))
    
  } else if(exposure == "infection") {
    
    data <- data %>%
      mutate(exposure = ifelse(inf_or_uninf == "inf", "exposed", "unexposed")) 
  }
  
  data <- data %>%
    mutate(preg_id = gsub('.{2}$', "", pregnancy_id)) 
  
  data2 <- data %>% filter(outcome=="Live birth")
  data3 <- data %>% filter(all_14_all_anomalies==1)
  data4 <- data2 %>% filter(all_14_all_anomalies==1)
  
  #Types of congenital anomaly
  table4b <- bind_rows(
    #specific types of congenital anomalies
    data3 %>%
      filter(all_1_nervous_system==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Nervous system:"),
    
    data3 %>%
      filter(all_2_eye==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Eye:"),
    
    data3 %>%
      filter(all_3_ear_face_and_neck==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Ear, face and neck:"),
    
    data3 %>%
      filter(all_4_congenital_heart_defects==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Congenital heart defects:"),
    
    data3 %>%
      filter(all_5_respiratory==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Respiratory:"),
    
    data3 %>%
      filter(all_6_oro_facial_clefts==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Oro-facial clefts:"),
    
    data3 %>%
      filter(all_7_digestive_system==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Digestive:"),
    
    data3 %>%
      filter(all_8_abdominal_wall_defects==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Abdominal wall defects:"),
    
    data3 %>%
      filter(all_9_urinary==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Urinary:"),
    
    data3 %>%
      filter(all_10_genital==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Genital:"),
    
    data3 %>%
      filter(all_11_limb==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Limb defect:"),
    
    data3 %>%
      filter(all_12_other_anomalies_total==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Other anomalies/syndromes:"),
    
    data3 %>%
      filter(all_13_chromosomal==1) %>%
      group_by(exposure) %>% 
      tally() %>%
      pivot_wider(names_from = exposure, values_from = n) %>% 
      mutate(rowname = "Chromosomal:")
  )
  
  table4b$denom_exposed <- data3 %>%
    filter(exposure=="exposed") %>%
    count()
  
  table4b$denom_unexposed <- data3 %>%
    filter(exposure=="unexposed") %>%
    count()
  
  table4b$denom_unexposed_2 <- as.numeric(unlist(table4b$denom_unexposed))
  table4b$denom_exposed_2 <- as.numeric(unlist(table4b$denom_exposed))
  
  table4b <- table4b %>%
    mutate(percentage=exposed/denom_exposed_2*100) %>%
    mutate(percentage=round(percentage,1)) %>%
    mutate(percentage_control=unexposed/denom_unexposed_2*100) %>%
    mutate(percentage_control=round(percentage_control,1))
  
  table4b <-  table4b %>%
    select(rowname, exposed, unexposed, percentage, percentage_control)
  
  if(exposure == "vaccination") {
    
    table4b <- table4b %>% rename(vacc = "exposed")
    
  } else if(exposure == "infection") {
    
    table4b <- table4b %>% rename(inf = "exposed")
  }
  
  if(control == "historical") {
    
    table4b <- table4b %>% rename(historic = "unexposed", percentage_historic = "percentage_control") 
    
  } else if(control == "contemporary" ) {
    
    table4b <- table4b %>% rename(contemporary = "unexposed", percentage_contemporary = "percentage_control")
    
  }
  
  return(table4b)
  
}

outcome_table5a <- get_outcomes_descriptives3(cohort1_vacc_contemp_controls, exposure = "vaccination", control = "contemporary")

table5a <- outcome_table5a %>% 
  mutate(vacc_all = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("rowname", "vacc_all", "vacc_contemporary" ))
write.csv(table5a , paste(folder_results, "Outcome3_by_vacc_cohort.csv", sep = ''))

outcome_table5b <- get_outcomes_descriptives3(cohort1_infect_contemp_controls, exposure = "infection", control = "contemporary")

table5b <- outcome_table5b %>% 
  mutate(inf_all = paste0(inf, " (", percentage, "%)"), 
         inf_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("rowname", "inf_all", "inf_contemporary" ))
write.csv(table5b, paste(folder_results, "Outcome3_by_inf_cohort.csv", sep = ''))

outcome_table5c <- get_outcomes_descriptives3(cohort1_vacc_contemp_controls.mrna, exposure = "vaccination", control = "contemporary")

table5c <- outcome_table5c %>% 
  mutate(vacc_all = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("rowname", "vacc_all", "vacc_contemporary" ))
write.csv(table5c, paste(folder_results, "Outcome3_by_vacc_cohort_mrna.csv", sep = ''))

outcome_table5d <- get_outcomes_descriptives3(cohort1_vacc_contemp_controls.az, exposure = "vaccination", control = "contemporary")

table5d <- outcome_table5d %>% 
  mutate(vacc_all = paste0(vacc, " (", percentage, "%)"), 
         vacc_contemporary = paste0(contemporary, " (", percentage_contemporary, "%)")) %>%
  select(c("rowname", "vacc_all", "vacc_contemporary" ))
write.csv(table5d , paste(folder_results, "Outcome3_by_vacc_cohort_az.csv", sep = ''))


####CHECK ON OTHER ANOMALIES####

otheranomalies_vac <- cohort1_vacc_contemp_controls %>%
  filter(all_12_other_anomalies_total==1)

table(otheranomalies_vac$vacc_or_unvacc, otheranomalies_vac$all_12_1_skeletal_dysplasias)
table(otheranomalies_vac$vacc_or_unvacc, otheranomalies_vac$all_12_2_craniosynostosis)
table(otheranomalies_vac$vacc_or_unvacc, otheranomalies_vac$all_12_3_congenital_constriction_bands)
table(otheranomalies_vac$vacc_or_unvacc, otheranomalies_vac$all_12_4_situs_inversus)
table(otheranomalies_vac$vacc_or_unvacc, otheranomalies_vac$all_12_5_conjoined_twins)
table(otheranomalies_vac$vacc_or_unvacc, otheranomalies_vac$all_12_6_congenital_skin_disorders)
table(otheranomalies_vac$vacc_or_unvacc, otheranomalies_vac$all_12_7_vater_vacterl)
table(otheranomalies_vac$vacc_or_unvacc, otheranomalies_vac$all_12_8_vascular_disruption_anomalies)
table(otheranomalies_vac$vacc_or_unvacc, otheranomalies_vac$all_12_9_laterality_anomalies)
table(otheranomalies_vac$vacc_or_unvacc, otheranomalies_vac$all_12_10_teratogenic_syndromes_with_malformations)
table(otheranomalies_vac$vacc_or_unvacc, otheranomalies_vac$all_12_11_genetic_syndromes_and_microdeletions)

otheranomalies_inf <- cohort1_infect_contemp_controls %>%
  filter(all_12_other_anomalies_total==1)

table(otheranomalies_inf$inf_or_uninf, otheranomalies_inf$all_12_1_skeletal_dysplasias)
table(otheranomalies_inf$inf_or_uninf, otheranomalies_inf$all_12_2_craniosynostosis)
table(otheranomalies_inf$inf_or_uninf, otheranomalies_inf$all_12_3_congenital_constriction_bands)
table(otheranomalies_inf$inf_or_uninf, otheranomalies_inf$all_12_4_situs_inversus)
table(otheranomalies_inf$inf_or_uninf, otheranomalies_inf$all_12_5_conjoined_twins)
table(otheranomalies_inf$inf_or_uninf, otheranomalies_inf$all_12_6_congenital_skin_disorders)
table(otheranomalies_inf$inf_or_uninf, otheranomalies_inf$all_12_7_vater_vacterl)
table(otheranomalies_inf$inf_or_uninf, otheranomalies_inf$all_12_8_vascular_disruption_anomalies)
table(otheranomalies_inf$inf_or_uninf, otheranomalies_inf$all_12_9_laterality_anomalies)
table(otheranomalies_inf$inf_or_uninf, otheranomalies_inf$all_12_10_teratogenic_syndromes_with_malformations)
table(otheranomalies_inf$inf_or_uninf, otheranomalies_inf$all_12_11_genetic_syndromes_and_microdeletions)

otheranomalies_mrna <- cohort1_vacc_contemp_controls.mrna %>%
  filter(all_12_other_anomalies_total==1)

table(otheranomalies_mrna$vacc_or_unvacc, otheranomalies_mrna$all_12_1_skeletal_dysplasias)
table(otheranomalies_mrna$vacc_or_unvacc, otheranomalies_mrna$all_12_2_craniosynostosis)
table(otheranomalies_mrna$vacc_or_unvacc, otheranomalies_mrna$all_12_3_congenital_constriction_bands)
table(otheranomalies_mrna$vacc_or_unvacc, otheranomalies_mrna$all_12_4_situs_inversus)
table(otheranomalies_mrna$vacc_or_unvacc, otheranomalies_mrna$all_12_5_conjoined_twins)
table(otheranomalies_mrna$vacc_or_unvacc, otheranomalies_mrna$all_12_6_congenital_skin_disorders)
table(otheranomalies_mrna$vacc_or_unvacc, otheranomalies_mrna$all_12_7_vater_vacterl)
table(otheranomalies_mrna$vacc_or_unvacc, otheranomalies_mrna$all_12_8_vascular_disruption_anomalies)
table(otheranomalies_mrna$vacc_or_unvacc, otheranomalies_mrna$all_12_9_laterality_anomalies)
table(otheranomalies_mrna$vacc_or_unvacc, otheranomalies_mrna$all_12_10_teratogenic_syndromes_with_malformations)
table(otheranomalies_mrna$vacc_or_unvacc, otheranomalies_vac$all_12_11_genetic_syndromes_and_microdeletions)

otheranomalies_az <- cohort1_vacc_contemp_controls.az %>%
  filter(all_12_other_anomalies_total==1)

table(otheranomalies_az$vacc_or_unvacc, otheranomalies_az$all_12_1_skeletal_dysplasias)
table(otheranomalies_az$vacc_or_unvacc, otheranomalies_az$all_12_2_craniosynostosis)
table(otheranomalies_az$vacc_or_unvacc, otheranomalies_az$all_12_3_congenital_constriction_bands)
table(otheranomalies_az$vacc_or_unvacc, otheranomalies_az$all_12_4_situs_inversus)
table(otheranomalies_az$vacc_or_unvacc, otheranomalies_az$all_12_5_conjoined_twins)
table(otheranomalies_az$vacc_or_unvacc, otheranomalies_az$all_12_6_congenital_skin_disorders)
table(otheranomalies_az$vacc_or_unvacc, otheranomalies_az$all_12_7_vater_vacterl)
table(otheranomalies_az$vacc_or_unvacc, otheranomalies_az$all_12_8_vascular_disruption_anomalies)
table(otheranomalies_az$vacc_or_unvacc, otheranomalies_az$all_12_9_laterality_anomalies)
table(otheranomalies_az$vacc_or_unvacc, otheranomalies_az$all_12_10_teratogenic_syndromes_with_malformations)
table(otheranomalies_az$vacc_or_unvacc, otheranomalies_az$all_12_11_genetic_syndromes_and_microdeletions)
