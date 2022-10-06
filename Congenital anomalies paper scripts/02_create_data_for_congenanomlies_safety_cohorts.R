#***********************************************************************************************
# Prepare data for congenital anomalies analysis
# This file:
#  1. Identifies vaccination and infection in risk period
#  2. Prepares further data on type and dose of vaccination
#***********************************************************************************************

#### Housekeeping####

library(expss)
library(tidyverse)

setwd("x")

folder_temp_data <- "x"
folder_results <- "x"

baby.data.ready <- readRDS(paste0(folder_temp_data, "baby_level_record_ready.rds"))
z <- baby.data.ready 

#### Prepare relavant variables for whole cohort ####

# Vaccination information -------------------------------------------------

# We have different lengths of time post vaccination to include. Let's include these variables now so we
# can filter them out later.
z <-
  z %>%
  mutate(
    congenital_anomaly_study_timing_of_vaccination_to_include_up_to = est_conception_date + (17 * 7) + 6,
    sense_congenital_anomaly_study_timing_of_vaccination_to_include_up_to = est_conception_date + (7 * 7) + 6,
    microcephaly_study_timing_of_vaccination_to_include_up_to = pregnancy_end_date
  )

# We can then add in the vaccination period we're looking at
z <- z %>%
  mutate(
    congenital_anomaly_vaccination_timing_period_start = as.Date(est_conception_date) - (6 * 7),
    congenital_anomaly_vaccination_timing_period_end = congenital_anomaly_study_timing_of_vaccination_to_include_up_to,
    sense_congenital_anomaly_vaccination_timing_period_start = as.Date(est_conception_date-1),
    sense_congenital_anomaly_vaccination_timing_period_end = sense_congenital_anomaly_study_timing_of_vaccination_to_include_up_to,
    microcephaly_vaccination_timing_period_start = as.Date(est_conception_date) - (6 * 7),
    microcephaly_vaccination_timing_period_end = microcephaly_study_timing_of_vaccination_to_include_up_to
  )

# Now lets add in if vaccination occured during the vaccination timing period.
# For it to occur in the vaccination period, it needs to be between the vaccination timing period and
# before the study pregnancy end date.

vaccine_dose_mutate_function <- function(df, dose, condition) {
  col_name <- rlang::sym(paste(dose, "_", condition, "_vaccination_timing_period", sep = ""))
  dose_column <- rlang::sym(paste(dose, "_vacc_occurence_date", sep = ""))
  vaccination_period_end <- rlang::sym(paste(condition, "_vaccination_timing_period_end", sep = ""))
  vaccination_period_start <- rlang::sym(paste(condition, "_vaccination_timing_period_start", sep = ""))

  df %>%
    mutate(!!col_name := case_when(
      (!!dose_column) < (!!vaccination_period_end) &
        (!!dose_column) > (!!vaccination_period_start) &
        (!!dose_column) < pregnancy_end_date ~ 1,
      TRUE ~ 0
    ))
}

number_of_doses <- c("dose_1", "dose_2", "dose_3", "dose_4", "dose_5")
conditions <- c("congenital_anomaly","sense_congenital_anomaly", "microcephaly")

for (dose in number_of_doses) {
  for (condition in conditions) {
    z <- z %>% vaccine_dose_mutate_function(dose = dose, condition = condition)
  }
}

# now use this to create a dose history column
z <- z %>%
  unite("dose_history_congenital_anomaly_vaccination_timing_period",
    dose_1_congenital_anomaly_vaccination_timing_period,
    dose_2_congenital_anomaly_vaccination_timing_period,
    dose_3_congenital_anomaly_vaccination_timing_period,
    dose_4_congenital_anomaly_vaccination_timing_period,
    dose_5_congenital_anomaly_vaccination_timing_period,
    remove = FALSE, sep = ""
  ) %>%
  unite("dose_history_sense_congenital_anomaly_vaccination_timing_period",
        dose_1_sense_congenital_anomaly_vaccination_timing_period,
        dose_2_sense_congenital_anomaly_vaccination_timing_period,
        dose_3_sense_congenital_anomaly_vaccination_timing_period,
        dose_4_sense_congenital_anomaly_vaccination_timing_period,
        dose_5_sense_congenital_anomaly_vaccination_timing_period,
        remove = FALSE, sep = ""
  ) %>%
  unite("dose_history_microcephaly_vaccination_timing_period",
        dose_1_microcephaly_vaccination_timing_period,
        dose_2_microcephaly_vaccination_timing_period,
        dose_3_microcephaly_vaccination_timing_period,
        dose_4_microcephaly_vaccination_timing_period,
        dose_5_microcephaly_vaccination_timing_period,
        remove = FALSE, sep = "")
        
  
# and then interpret the united columns

dose_history_function <- function(df, condition) {
  col_name <- rlang::sym(paste(condition, "_vaccination_dose_information", sep = ""))
  dose_history_col <- rlang::sym(paste("dose_history_", condition, "_vaccination_timing_period", sep = ""))

  df %>%
    mutate((!!col_name) := case_when(
      (!!dose_history_col) == "00000" ~ "no dose",
      (!!dose_history_col) == "10000" ~ "only dose 1",
      (!!dose_history_col) == "01000" ~ "only dose 2",
      (!!dose_history_col) == "00100" ~ "only dose 3",
      (!!dose_history_col) == "00010" ~ "only dose 4",
      (!!dose_history_col) == "00001" ~ "only dose 5",
      (!!dose_history_col) == "01100" ~ "dose 2 & dose 3",
      (!!dose_history_col) == "10010" ~ "dose 1 & dose 4",
      (!!dose_history_col) == "00110" ~ "dose 3 & dose 4",
      (!!dose_history_col) == "11000" ~ "dose 1 & dose 2",
      (!!dose_history_col) == "01010" ~ "dose 2 & dose 4",
      (!!dose_history_col) == "00011" ~ "dose 4 & dose 5",
      (!!dose_history_col) == "10100" ~ "dose 1 & dose 3",
      (!!dose_history_col) == "11100" ~ "dose 1 & dose 2 & dose 3",
      (!!dose_history_col) == "01110" ~ "dose 2 & dose 3 & dose 4",
      (!!dose_history_col) == "10110" ~ "dose 1 & dose 3 & dose 4",
      (!!dose_history_col) == "11010" ~ "dose 1 & dose 2 & dose 4",
      (!!dose_history_col) == "00111" ~ "dose 3 & dose 4 & dose 5",
      (!!dose_history_col) == "11110" ~ "dose 1 & dose 2 & dose 3 & dose 4",
      (!!dose_history_col) == "01111" ~ "dose 2 & dose 3 & dose 4 & dose 5",
      (!!dose_history_col) == "11111" ~ "dose 1 & dose 2 & dose 3 & dose 4 & dose 5",
      str_detect((!!dose_history_col), "NA") ~ NA_character_
    ))
}

z <- z %>%
  dose_history_function("congenital_anomaly") %>%
  dose_history_function("sense_congenital_anomaly") %>%
  dose_history_function("microcephaly") 

# create a column for if they recieved any dose vs no dose during timing period
z <-
  z %>%
  mutate(
    congenital_anomaly_any_vaccine_dose = if_else(congenital_anomaly_vaccination_dose_information == "no dose", "no", "yes"),
    sense_congenital_anomaly_any_vaccine_dose = if_else(sense_congenital_anomaly_vaccination_dose_information == "no dose", "no", "yes"),
    microcephaly_any_vaccine_dose = if_else(microcephaly_vaccination_dose_information == "no dose", "no", "yes")
  )

# Reference date - date of their first dose during the study vaccination period
z <-
  z %>%
  mutate(congenital_anomaly_reference_date = case_when(
    str_extract(congenital_anomaly_vaccination_dose_information, "[0-9]") == "1" ~ dose_1_vacc_occurence_date,
    str_extract(congenital_anomaly_vaccination_dose_information, "[0-9]") == "2" ~ dose_2_vacc_occurence_date,
    str_extract(congenital_anomaly_vaccination_dose_information, "[0-9]") == "3" ~ dose_3_vacc_occurence_date,
    str_extract(congenital_anomaly_vaccination_dose_information, "[0-9]") == "4" ~ dose_4_vacc_occurence_date,
    str_extract(congenital_anomaly_vaccination_dose_information, "[0-9]") == "5" ~ dose_5_vacc_occurence_date
  )) %>%
  mutate(sense_congenital_anomaly_reference_date = case_when(
    str_extract(sense_congenital_anomaly_vaccination_dose_information, "[0-9]") == "1" ~ dose_1_vacc_occurence_date,
    str_extract(sense_congenital_anomaly_vaccination_dose_information, "[0-9]") == "2" ~ dose_2_vacc_occurence_date,
    str_extract(sense_congenital_anomaly_vaccination_dose_information, "[0-9]") == "3" ~ dose_3_vacc_occurence_date,
    str_extract(sense_congenital_anomaly_vaccination_dose_information, "[0-9]") == "4" ~ dose_4_vacc_occurence_date,
    str_extract(sense_congenital_anomaly_vaccination_dose_information, "[0-9]") == "5" ~ dose_5_vacc_occurence_date
  )) %>%
  mutate(microcephaly_reference_date = case_when(
    str_extract(microcephaly_vaccination_dose_information, "[0-9]") == "1" ~ dose_1_vacc_occurence_date,
    str_extract(microcephaly_vaccination_dose_information, "[0-9]") == "2" ~ dose_2_vacc_occurence_date,
    str_extract(microcephaly_vaccination_dose_information, "[0-9]") == "3" ~ dose_3_vacc_occurence_date,
    str_extract(microcephaly_vaccination_dose_information, "[0-9]") == "4" ~ dose_4_vacc_occurence_date,
    str_extract(microcephaly_vaccination_dose_information, "[0-9]") == "5" ~ dose_5_vacc_occurence_date
  )) %>%
  mutate(
    microcephaly_gestation_at_reference_date = floor(as.numeric(microcephaly_reference_date - est_conception_date) / 7) + 2,
    congenital_anomaly_gestation_at_reference_date = floor(as.numeric(congenital_anomaly_reference_date - est_conception_date) / 7) + 2,
    sense_congenital_anomaly_gestation_at_reference_date = floor(as.numeric(sense_congenital_anomaly_reference_date - est_conception_date) / 7) + 2
  )

# add vaccination period information:
# vaccine period starts on 8th December 2020
z <- z %>%
  mutate(congenital_anomaly_vaccine_period_during_preg = case_when(
    pregnancy_end_date >= "2020-12-08" ~ "Pregnancy ended during vaccination period",
    pregnancy_end_date < "2020-12-08" ~ "Pregnancy ended prevaccination"
  )) %>%
  mutate(microcephaly_vaccine_period_during_preg = case_when(
    pregnancy_end_date >= "2020-12-08" ~ "Pregnancy ended during vaccination period",
    pregnancy_end_date < "2020-12-08" ~ "Pregnancy ended prevaccination"
    )) %>%
  mutate(
    days_gestation_at_start_of_vaccination_period = difftime(as.Date("2020-12-08"), est_conception_date, units = "days")) 
 

z <- z %>% mutate(days_gestation_at_start_of_testing_period = difftime(as.Date("2020-05-18"), est_conception_date, units = "days"))
  

# number of doses and vaccine information
z <-
  z %>%
  mutate(congenital_anomaly_vaccination_number_of_doses = str_count(congenital_anomaly_vaccination_dose_information, "[0-9]")) %>%
  mutate(
    across(matches("dose_[0-9]_vacc_product_name"), ~ str_remove(.x, "Covid-19 mRNA Vaccine ")),
    across(matches("dose_[0-9]_vacc_product_name"), ~ str_remove(.x, "Covid-19 Vaccine "))
  ) %>%
  mutate(gestation_at_reference_date_categories = case_when(
    congenital_anomaly_gestation_at_reference_date < 2 ~ "Pre-conception",
    congenital_anomaly_gestation_at_reference_date >= 2 & congenital_anomaly_gestation_at_reference_date <= 5 ~ "2-5 weeks",
    congenital_anomaly_gestation_at_reference_date >= 6 & congenital_anomaly_gestation_at_reference_date <= 10 ~ "6-10 weeks",
    congenital_anomaly_gestation_at_reference_date >= 11 & congenital_anomaly_gestation_at_reference_date <= 15 ~ "11-15 weeks",
    congenital_anomaly_gestation_at_reference_date >= 16 & congenital_anomaly_gestation_at_reference_date <= 20 ~ "16-20 weeks"
  )) %>%
  mutate(type_of_vaccination_at_reference_date = case_when(
    congenital_anomaly_reference_date == dose_1_vacc_occurence_date ~ dose_1_vacc_product_name,
    congenital_anomaly_reference_date == dose_2_vacc_occurence_date ~ dose_2_vacc_product_name,
    congenital_anomaly_reference_date == dose_3_vacc_occurence_date ~ dose_3_vacc_product_name,
    congenital_anomaly_reference_date == dose_4_vacc_occurence_date ~ dose_4_vacc_product_name,
    congenital_anomaly_reference_date == dose_5_vacc_occurence_date ~ dose_5_vacc_product_name
  )) %>%
  mutate(congenital_anomaly_type_of_vaccination_during_period = case_when(
    congenital_anomaly_vaccination_dose_information == "no dose" ~ "None",
    congenital_anomaly_vaccination_dose_information == "only dose 1" ~ dose_1_vacc_product_name,
    congenital_anomaly_vaccination_dose_information == "only dose 2" ~ dose_2_vacc_product_name,
    congenital_anomaly_vaccination_dose_information == "only dose 3" ~ dose_3_vacc_product_name,
    congenital_anomaly_vaccination_dose_information == "only dose 4" ~ dose_4_vacc_product_name,
    congenital_anomaly_vaccination_dose_information == "only dose 5" ~ dose_5_vacc_product_name,
    congenital_anomaly_vaccination_dose_information == "dose 1 & dose 2" ~
      if_else(dose_1_vacc_product_name == dose_2_vacc_product_name, dose_1_vacc_product_name, "Mixed doses"),
    congenital_anomaly_vaccination_dose_information == "dose 1 & dose 3" ~
      if_else(dose_1_vacc_product_name == dose_3_vacc_product_name, dose_1_vacc_product_name, "Mixed doses"),
    congenital_anomaly_vaccination_dose_information == "dose 3 & dose 4" ~
      if_else(dose_3_vacc_product_name == dose_4_vacc_product_name, dose_3_vacc_product_name, "Mixed doses"),
    congenital_anomaly_vaccination_dose_information == "dose 4 & dose 5" ~
      if_else(dose_4_vacc_product_name == dose_5_vacc_product_name, dose_4_vacc_product_name, "Mixed doses"),
    congenital_anomaly_vaccination_dose_information == "dose 2 & dose 3" ~
      if_else(dose_2_vacc_product_name == dose_3_vacc_product_name, dose_2_vacc_product_name, "Mixed doses"),
    congenital_anomaly_vaccination_dose_information == "dose 1 & dose 2 & dose 3" ~
      if_else(dose_1_vacc_product_name == dose_2_vacc_product_name & dose_1_vacc_product_name == dose_3_vacc_product_name,
              dose_1_vacc_product_name, "Mixed doses"
      ),
    congenital_anomaly_vaccination_dose_information == "dose 1 & dose 3 & dose 4" ~
      if_else(dose_1_vacc_product_name == dose_3_vacc_product_name & dose_1_vacc_product_name == dose_4_vacc_product_name,
              dose_1_vacc_product_name, "Mixed doses"
      ),
    congenital_anomaly_vaccination_dose_information == "dose 2 & dose 3 & dose 4" ~
      if_else(dose_2_vacc_product_name == dose_3_vacc_product_name & dose_2_vacc_product_name == dose_4_vacc_product_name,
              dose_2_vacc_product_name, "Mixed doses"
      ),
    congenital_anomaly_vaccination_dose_information == "dose 3 & dose 4 & dose 5" ~
      if_else(dose_3_vacc_product_name == dose_4_vacc_product_name & dose_3_vacc_product_name == dose_5_vacc_product_name,
              dose_3_vacc_product_name, "Mixed doses"
      ),
    congenital_anomaly_vaccination_dose_information == "dose 1 & dose 2 & dose 3 & dose 4" ~
      if_else(dose_1_vacc_product_name == dose_2_vacc_product_name & dose_1_vacc_product_name == dose_3_vacc_product_name &
                dose_1_vacc_product_name == dose_4_vacc_product_name,
              dose_1_vacc_product_name, "Mixed doses"
      ),
    congenital_anomaly_vaccination_dose_information == "dose 2 & dose 3 & dose 4 & dose 5" ~
      if_else(dose_2_vacc_product_name == dose_3_vacc_product_name & dose_2_vacc_product_name == dose_4_vacc_product_name &
                dose_2_vacc_product_name == dose_5_vacc_product_name,
              dose_2_vacc_product_name, "Mixed doses"
      ),
    congenital_anomaly_vaccination_dose_information == "dose 1 & dose 2 & dose 3 & dose 4 & dose 5" ~
      if_else(dose_1_vacc_product_name == dose_2_vacc_product_name & dose_1_vacc_product_name == dose_3_vacc_product_name &
                dose_1_vacc_product_name == dose_4_vacc_product_name & dose_1_vacc_product_name == dose_5_vacc_product_name,
              dose_1_vacc_product_name, "Mixed doses"
      ),
    TRUE ~ "Unknown"
  ))

z <-
  z %>%
  mutate(microcephaly_vaccination_number_of_doses = str_count(microcephaly_vaccination_dose_information, "[0-9]")) %>%
  mutate(
    across(matches("dose_[0-9]_vacc_product_name"), ~ str_remove(.x, "Covid-19 mRNA Vaccine ")),
    across(matches("dose_[0-9]_vacc_product_name"), ~ str_remove(.x, "Covid-19 Vaccine "))
  ) %>%
  mutate(gestation_at_reference_date_categories = case_when(
    microcephaly_gestation_at_reference_date < 2 ~ "Pre-conception",
    microcephaly_gestation_at_reference_date >= 2 & microcephaly_gestation_at_reference_date <= 5 ~ "2-5 weeks",
    microcephaly_gestation_at_reference_date >= 6 & microcephaly_gestation_at_reference_date <= 10 ~ "6-10 weeks",
    microcephaly_gestation_at_reference_date >= 11 & microcephaly_gestation_at_reference_date <= 15 ~ "11-15 weeks",
    microcephaly_gestation_at_reference_date >= 16 & microcephaly_gestation_at_reference_date <= 20 ~ "16-20 weeks"
  )) %>%
  mutate(type_of_vaccination_at_reference_date = case_when(
    microcephaly_reference_date == dose_1_vacc_occurence_date ~ dose_1_vacc_product_name,
    microcephaly_reference_date == dose_2_vacc_occurence_date ~ dose_2_vacc_product_name,
    microcephaly_reference_date == dose_3_vacc_occurence_date ~ dose_3_vacc_product_name,
    microcephaly_reference_date == dose_4_vacc_occurence_date ~ dose_4_vacc_product_name,
    microcephaly_reference_date == dose_5_vacc_occurence_date ~ dose_5_vacc_product_name
  )) %>%
  mutate(microcephaly_type_of_vaccination_during_period = case_when(
    microcephaly_vaccination_dose_information == "no dose" ~ "None",
    microcephaly_vaccination_dose_information == "only dose 1" ~ dose_1_vacc_product_name,
    microcephaly_vaccination_dose_information == "only dose 2" ~ dose_2_vacc_product_name,
    microcephaly_vaccination_dose_information == "only dose 3" ~ dose_3_vacc_product_name,
    microcephaly_vaccination_dose_information == "only dose 4" ~ dose_4_vacc_product_name,
    microcephaly_vaccination_dose_information == "only dose 5" ~ dose_5_vacc_product_name,
    microcephaly_vaccination_dose_information == "dose 1 & dose 2" ~
      if_else(dose_1_vacc_product_name == dose_2_vacc_product_name, dose_1_vacc_product_name, "Mixed doses"),
    microcephaly_vaccination_dose_information == "dose 1 & dose 3" ~
      if_else(dose_1_vacc_product_name == dose_3_vacc_product_name, dose_1_vacc_product_name, "Mixed doses"),
    microcephaly_vaccination_dose_information == "dose 3 & dose 4" ~
      if_else(dose_3_vacc_product_name == dose_4_vacc_product_name, dose_3_vacc_product_name, "Mixed doses"),
    microcephaly_vaccination_dose_information == "dose 4 & dose 5" ~
      if_else(dose_4_vacc_product_name == dose_5_vacc_product_name, dose_4_vacc_product_name, "Mixed doses"),
    microcephaly_vaccination_dose_information == "dose 2 & dose 3" ~
      if_else(dose_2_vacc_product_name == dose_3_vacc_product_name, dose_2_vacc_product_name, "Mixed doses"),
    microcephaly_vaccination_dose_information == "dose 1 & dose 2 & dose 3" ~
      if_else(dose_1_vacc_product_name == dose_2_vacc_product_name & dose_1_vacc_product_name == dose_3_vacc_product_name,
              dose_1_vacc_product_name, "Mixed doses"
      ),
    microcephaly_vaccination_dose_information == "dose 1 & dose 3 & dose 4" ~
      if_else(dose_1_vacc_product_name == dose_3_vacc_product_name & dose_1_vacc_product_name == dose_4_vacc_product_name,
              dose_1_vacc_product_name, "Mixed doses"
      ),
    microcephaly_vaccination_dose_information == "dose 2 & dose 3 & dose 4" ~
      if_else(dose_2_vacc_product_name == dose_3_vacc_product_name & dose_2_vacc_product_name == dose_4_vacc_product_name,
              dose_2_vacc_product_name, "Mixed doses"
      ),
    microcephaly_vaccination_dose_information == "dose 3 & dose 4 & dose 5" ~
      if_else(dose_3_vacc_product_name == dose_4_vacc_product_name & dose_3_vacc_product_name == dose_5_vacc_product_name,
              dose_3_vacc_product_name, "Mixed doses"
      ),
    microcephaly_vaccination_dose_information == "dose 1 & dose 2 & dose 3 & dose 4" ~
      if_else(dose_1_vacc_product_name == dose_2_vacc_product_name & dose_1_vacc_product_name == dose_3_vacc_product_name &
                dose_1_vacc_product_name == dose_4_vacc_product_name,
              dose_1_vacc_product_name, "Mixed doses"
      ),
    microcephaly_vaccination_dose_information == "dose 2 & dose 3 & dose 4 & dose 5" ~
      if_else(dose_2_vacc_product_name == dose_3_vacc_product_name & dose_2_vacc_product_name == dose_4_vacc_product_name &
                dose_2_vacc_product_name == dose_5_vacc_product_name,
              dose_2_vacc_product_name, "Mixed doses"
      ),
    microcephaly_vaccination_dose_information == "dose 1 & dose 2 & dose 3 & dose 4 & dose 5" ~
      if_else(dose_1_vacc_product_name == dose_2_vacc_product_name & dose_1_vacc_product_name == dose_3_vacc_product_name &
                dose_1_vacc_product_name == dose_4_vacc_product_name & dose_1_vacc_product_name == dose_5_vacc_product_name,
              dose_1_vacc_product_name, "Mixed doses"
      ),
    TRUE ~ "Unknown"
  ))

# infection information ---------------------------------------------------

z <- z %>%
  mutate(ever_infected = case_when(
    !is.na(index_date_covid_infection_1) ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(congenital_anomaly_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(sense_congenital_anomaly_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_1) <= sense_congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_2) <= sense_congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_3) <= sense_congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_4) <= sense_congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(microcephaly_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"
  )) %>%
  mutate(sense_microcephaly_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_1) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_2) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_3) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_4) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"
  ))

# check there's no infections after pregnancy end date

z %>%
  filter(congenital_anomaly_infected_during_risk_period == "yes") %>%
  filter(!(index_date_covid_infection_1 < pregnancy_end_date & index_date_covid_infection_1 > congenital_anomaly_vaccination_timing_period_start |
    index_date_covid_infection_2 < pregnancy_end_date & index_date_covid_infection_2 > congenital_anomaly_vaccination_timing_period_start |
    index_date_covid_infection_3 < pregnancy_end_date & index_date_covid_infection_3 > congenital_anomaly_vaccination_timing_period_start |
    index_date_covid_infection_4 < pregnancy_end_date & index_date_covid_infection_4 > congenital_anomaly_vaccination_timing_period_start))

z %>%
  filter(microcephaly_infected_during_risk_period == "yes") %>%
  filter(!(index_date_covid_infection_1 < pregnancy_end_date & index_date_covid_infection_1 > microcephaly_vaccination_timing_period_start |
    index_date_covid_infection_2 < pregnancy_end_date & index_date_covid_infection_2 > microcephaly_vaccination_timing_period_start |
    index_date_covid_infection_3 < pregnancy_end_date & index_date_covid_infection_3 > microcephaly_vaccination_timing_period_start |
    index_date_covid_infection_4 < pregnancy_end_date & index_date_covid_infection_4 > microcephaly_vaccination_timing_period_start))

# gestation at covid infection during risk period
# instead of gestation at vaccination, for some of our cohorts we want the
# gestation at time of infection during the risk period only
z <-
  z %>%
  mutate(congenital_anomaly_covid_index_date = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_1,
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_2,
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_3,
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_4
  )) %>%
  mutate(sense_congenital_anomaly_covid_index_date = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_1) <= sense_congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_1,
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_2) <= sense_congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_2,
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_3) <= sense_congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_3,
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_4) <= sense_congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_4
  )) %>%
  mutate(microcephaly_covid_index_date = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_1,
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_2,
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_3,
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_4
  )) %>%
  mutate(sense_microcephaly_covid_index_date = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_1) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_1,
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_2) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_2,
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_3) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_3,
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) &
      as.Date(index_date_covid_infection_4) <= microcephaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ index_date_covid_infection_4
  )) %>%
  mutate(
    congenital_anomaly_gestation_at_index_date = floor(as.numeric(congenital_anomaly_covid_index_date - est_conception_date) / 7) + 2,
    sense_congenital_anomaly_gestation_at_index_date = floor(as.numeric(congenital_anomaly_covid_index_date - est_conception_date) / 7) + 2,
    microcephaly_gestation_at_index_date = floor(as.numeric(microcephaly_covid_index_date - est_conception_date) / 7) + 2
  )


# other variables ---------------------------------------------------------

# Create a variable to capture season of conception
z$conception_quarter <- as.numeric(substr(z$est_conception_date, 6, 7))
z$conception_quarter <- cut(z$conception_quarter, breaks = c(0, 3, 6, 9, 12), label = 1:4)

# Add variable for if pregnancy ended pre-pandemic
z <- z %>%
  mutate(
    overall_pregnancy_ending_pandemic = if_else(pregnancy_end_date >= as.Date("2020-03-01"),
      "Pandemic", "Pre-pandemic"
    ),
    congenital_anomaly_pregnancy_ending_pandemic = if_else(pregnancy_end_date >= as.Date("2020-03-01"),
      "Pandemic", "Pre-pandemic"
    ),
    microcephaly_pregnancy_ending_pandemic = if_else(pregnancy_end_date >= as.Date("2020-03-01"),
                                                           "Pandemic", "Pre-pandemic"
    )
  )

#prepare id numbers so pregnancy_id becomes a unique id for each foetus
z <- rename(z, pregnancy_id_orig=pregnancy_id)

z$count <- 1
z <- z %>%
  group_by(pregnancy_id_orig) %>%
  mutate(total_foetus = n()) %>%
  mutate(order_foetus = cumsum(count == 1), NA) %>% 
  ungroup()

z$pregnancy_id <- paste(z$pregnancy_id_orig, z$order_foetus, sep="_") 

check <- z[,c("pregnancy_id_orig", "pregnancy_id", "order_foetus")]

length(unique(z$pregnancy_id))
length(unique(z$pregnancy_id_orig))

# write out data ----------------------------------------------------------
z %>%
  write_rds(paste0(folder_temp_data, "baby_level_record_for_making_cohort.rds"), compress = "gz")
