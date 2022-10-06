#############################################################################################################
#For infection analysis need to remove small number of infections that occur among women who could be at risk
#before 18th may
#############################################################################################################

folder_working_data <- "x"
folder_results <- "x"

###### IMPORT DATA  ######

cohort1_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_congenital_infection_primary_12wks_contemporary.rds")) 
cohort2_infect_contemp_controls_sensitivity <- readRDS(paste0(folder_working_data, "matched_congenital_infection_sensitivity_contemporary.rds")) 
cohort3_infect_contemp_controls_sensitivity <- readRDS(paste0(folder_working_data, "matched_congenital_infection_sensitivity2_12wks_contemporary.rds")) 


check <- cohort1_infect_contemp_controls %>% filter(is.na(pregnancy_id))
cohort1_infect_contemp_controls <- cohort1_infect_contemp_controls %>% filter(!(index %in% check$index))

check <- cohort2_infect_contemp_controls_sensitivity %>% filter(is.na(pregnancy_id))
cohort2_infect_contemp_controls_sensitivity <- cohort2_infect_contemp_controls_sensitivity %>% filter(!(index %in% check$index))

check <- cohort2_infect_contemp_controls_sensitivity %>% filter(is.na(pregnancy_id))
cohort3_infect_contemp_controls_sensitivity <- cohort3_infect_contemp_controls_sensitivity %>% filter(!(index %in% check$index))


#Identify pregnancies that need to be excluded

#primary dataset

data <- cohort1_infect_contemp_controls

testingenddate <- "2020-05-18"
testingenddate <- as.Date.character(testingenddate, "%Y-%m-%d")

data <- data %>% 
  mutate(congenital_anomaly_infected_during_risk_period_orig = congenital_anomaly_infected_during_risk_period) %>%
  mutate(congenital_anomaly_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) >= testingenddate &
      as.Date(index_date_covid_infection_1) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) >= testingenddate &
      as.Date(index_date_covid_infection_2) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) >= testingenddate &
      as.Date(index_date_covid_infection_3) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) >= testingenddate &
      as.Date(index_date_covid_infection_4) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"))

table(data$congenital_anomaly_infected_during_risk_period, useNA="always")
table(data$congenital_anomaly_covid_index_date, useNA="always")

toremove <- data %>%
  filter(congenital_anomaly_infected_during_risk_period=="no" & congenital_anomaly_infected_during_risk_period_orig=="yes")

data2 <- data %>% filter(!(index %in% toremove$index))

data2 %>%
  write_rds(paste0(folder_working_data, "matched_congenital_infection_primary_12wks_contemporary_final.rds"), compress = "gz")

#secondary dataset

data <- cohort2_infect_contemp_controls_sensitivity

testingenddate <- "2020-05-18"
testingenddate <- as.Date.character(testingenddate, "%Y-%m-%d")

data <- data %>% 
  mutate(congenital_anomaly_infected_during_risk_period_orig = congenital_anomaly_infected_during_risk_period) %>%
  mutate(congenital_anomaly_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) >= testingenddate &
      as.Date(index_date_covid_infection_1) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) >= testingenddate &
      as.Date(index_date_covid_infection_2) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) >= testingenddate &
      as.Date(index_date_covid_infection_3) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) >= testingenddate &
      as.Date(index_date_covid_infection_4) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"))

table(data$congenital_anomaly_infected_during_risk_period, useNA="always")

toremove <- data %>%
  filter(congenital_anomaly_infected_during_risk_period=="no" & congenital_anomaly_infected_during_risk_period_orig=="yes")

data2 <- data %>% filter(!(index %in% toremove$index))

data2 %>%
  write_rds(paste0(folder_working_data, "matched_congenital_infection_sensitivity_contemporary_final.rds"), compress = "gz")


#secondary dataset (narrow exposure window)

data <- cohort3_infect_contemp_controls_sensitivity

testingenddate <- "2020-05-18"
testingenddate <- as.Date.character(testingenddate, "%Y-%m-%d")

data <- data %>% 
  mutate(congenital_anomaly_infected_during_risk_period_orig = congenital_anomaly_infected_during_risk_period) %>%
  mutate(congenital_anomaly_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) >= testingenddate &
      as.Date(index_date_covid_infection_1) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) >= testingenddate &
      as.Date(index_date_covid_infection_2) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) >= testingenddate &
      as.Date(index_date_covid_infection_3) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) >= testingenddate &
      as.Date(index_date_covid_infection_4) <= congenital_anomaly_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"))

table(data$congenital_anomaly_infected_during_risk_period, useNA="always")

toremove <- data %>%
  filter(congenital_anomaly_infected_during_risk_period=="no" & congenital_anomaly_infected_during_risk_period_orig=="yes")

data2 <- data %>% filter(!(index %in% toremove$index))

data2 %>%
  write_rds(paste0(folder_working_data, "matched_congenital_infection_sensitivity2_12wks_contemporary_final.rds"), compress = "gz")
