#****************************************************************************************************************
#CREATE GRAPHS OF EXPOSURE OVER TIME FOR CONGENITAL ANOMALIES ANALYSIS
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

##### VACCINE EXPOSURE OVER TIME ####

data <- cohort1_vacc_contemp_controls %>%
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


dose_1_all_exposures <- c("dose 1 & dose 2","dose 1 & dose 2 & dose 3", "dose 1 & dose 3", "only dose 1" )
dose_2_all_exposures <- c( "dose 2 & dose 3", "only dose 2", "dose 1 & dose 2","dose 1 & dose 2 & dose 3")
dose_3_all_exposures <- c("dose 3 & dose 4","only dose 3", "dose 1 & dose 3", "dose 1 & dose 2 & dose 3")
dose_4_all_exposures <- c( "dose 3 & dose 4",  "only dose 4" )

data <- data %>%
  mutate(dose_1_vacc_product = case_when(
    dose_1_vacc_product_name == "Pfizer" ~ "BNT162b2",
    dose_1_vacc_product_name == "Moderna" ~ "mRNA-1273",
    dose_1_vacc_product_name == "AstraZeneca" ~ "ChAdOx1-S/nCoV-19"
  )) %>%
  mutate(dose_2_vacc_product = case_when(
    dose_2_vacc_product_name == "Pfizer" ~ "BNT162b2",
    dose_2_vacc_product_name == "Moderna" ~ "mRNA-1273",
    dose_2_vacc_product_name == "AstraZeneca" ~ "ChAdOx1-S/nCoV-19"
  )) %>%
  mutate(dose_3_vacc_product = case_when(
    dose_3_vacc_product_name == "Pfizer" ~ "BNT162b2",
    dose_3_vacc_product_name == "Moderna" ~ "mRNA-1273",
    dose_3_vacc_product_name == "AstraZeneca" ~ "ChAdOx1-S/nCoV-19"
  )) %>%
  mutate(dose_4_vacc_product = case_when(
    dose_4_vacc_product_name == "Pfizer" ~ "BNT162b2",
    dose_4_vacc_product_name == "Moderna" ~ "mRNA-1273",
    dose_4_vacc_product_name == "AstraZeneca" ~ "ChAdOx1-S/nCoV-19"
  ))

vacc_time_all_doses_CA <- data %>%
  mutate(congenital_anomaly_vaccination_dose_information = as.factor(congenital_anomaly_vaccination_dose_information)) %>% 
  filter(is.na(dose_1_vacc_occurence_date) == FALSE,
         is.na(dose_1_vacc_product) == FALSE) %>% 
  filter(congenital_anomaly_vaccination_dose_information %in% dose_1_all_exposures) %>%
  group_by(dose_1_vacc_product, dose_1_vacc_occurence_date) %>% 
  summarise(n = n()) %>% mutate(dose = 1) %>% 
  rename(vax_name = dose_1_vacc_product, vax_date = dose_1_vacc_occurence_date) %>%  
  bind_rows(data %>% 
              filter(is.na(dose_2_vacc_occurence_date) == FALSE,
                     is.na(dose_2_vacc_product) == FALSE) %>% 
              filter(congenital_anomaly_vaccination_dose_information %in% dose_2_all_exposures) %>%
              group_by(dose_2_vacc_product, dose_2_vacc_occurence_date) %>% 
              summarise(n = n()) %>% mutate(dose = 2) %>% 
              rename(vax_name = dose_2_vacc_product, vax_date = dose_2_vacc_occurence_date)) %>% 
  bind_rows(data %>% 
              filter(is.na(dose_3_vacc_occurence_date) == FALSE,
                     is.na(dose_3_vacc_product) == FALSE) %>% 
              filter(congenital_anomaly_vaccination_dose_information %in% dose_3_all_exposures) %>%
              group_by(dose_3_vacc_product, dose_3_vacc_occurence_date) %>% 
              summarise(n = n()) %>%  mutate(dose = 3) %>%  
              rename(vax_name = dose_3_vacc_product, vax_date = dose_3_vacc_occurence_date)) %>% 
  bind_rows(data %>% 
              filter(is.na(dose_4_vacc_occurence_date) == FALSE,
                     is.na(dose_4_vacc_product) == FALSE) %>% 
              filter(congenital_anomaly_vaccination_dose_information %in% dose_4_all_exposures) %>%
              group_by(dose_4_vacc_product, dose_4_vacc_occurence_date) %>% 
              summarise(n = n()) %>% mutate(dose = 4) %>%  
              rename(vax_name = dose_4_vacc_product, vax_date = dose_4_vacc_occurence_date)) %>% 
  mutate(dose = factor(dose))# %>% 

vacc_time_all_doses_CA$vax_name <- factor(vacc_time_all_doses_CA$vax_name, levels=c("BNT162b2", "mRNA-1273", "ChAdOx1-S/nCoV-19"))

vacc_time_all_doses_CA %>%
  ggplot(aes(x = vax_date, y = n, fill = dose, colour = dose)) +
  geom_line() +
  facet_wrap(~vax_name,  ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x = "", y = "")
  
####INFECTION EXPOSURE OVER TIME

cohort1_infect_contemp_controls$congenital_anomaly_covid_month_year <- format(cohort1_infect_contemp_controls$congenital_anomaly_covid_index_date, "%Y-%m")

check <- cohort1_infect_contemp_controls%>%
  filter(congenital_anomaly_infected_during_risk_period=="yes")
check <- check[,c("congenital_anomaly_covid_index_date")]

inf_time_CA <- cohort1_infect_contemp_controls %>%
  filter(inf_or_uninf=="inf") %>%
  filter(gestation_at_outcome>=12) %>%
  group_by(congenital_anomaly_covid_month_year) %>% 
  summarise(n = n()) 

inf_time_CA %>%
  ggplot(aes(x = congenital_anomaly_covid_month_year, y = n, group=1)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Month",
    y = "Number of infections in pregnancy exposure period")

