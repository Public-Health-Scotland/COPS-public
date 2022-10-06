#############################################################################################################
#For infection analysis need to remove small number of infections that occur among women who could be at risk
#before 18th may
#############################################################################################################

setwd("X")

folder_working_data <- "X"
folder_results <- "X"

###### IMPORT DATA  ######

cohort1_mc_infect_hist_controls <- readRDS(paste0(folder_working_data, "matched_miscarriage_infection_cohort_one.rds")) 
cohort1_mc_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_miscarriage_infection_cohort_three.rds"))

cohort1_ep_infect_hist_controls <- readRDS(paste0(folder_working_data, "matched_ectopic_infection_cohort_one.rds")) 
cohort1_ep_infect_contemp_controls <- readRDS(paste0(folder_working_data, "matched_ectopic_infection_cohort_three.rds"))

###### FILTER OUT MISSING ONES ####

check <- cohort1_mc_infect_hist_controls %>% filter(is.na(pregnancy_id))
cohort1_mc_infect_hist_controls <- cohort1_mc_infect_hist_controls %>% filter(!(index %in% check$index))

check <- cohort1_mc_infect_contemp_controls %>% filter(is.na(pregnancy_id))
cohort1_mc_infect_contemp_controls <- cohort1_mc_infect_contemp_controls %>% filter(!(index %in% check$index))

check <- cohort1_ep_infect_hist_controls %>% filter(is.na(pregnancy_id))
cohort1_ep_infect_hist_controls <- cohort1_ep_infect_hist_controls %>% filter(!(index %in% check$index))

check <- cohort1_ep_infect_contemp_controls %>% filter(is.na(pregnancy_id))
cohort1_ep_infect_contemp_controls <- cohort1_ep_infect_contemp_controls %>% filter(!(index %in% check$index))

#Identify pregnancies that need to be excluded

#miscarriage primary dataset

data <- cohort1_mc_infect_hist_controls

testingenddate <- "2020-05-18"
testingenddate <- as.Date.character(testingenddate, "%Y-%m-%d")

data <- data %>% 
  mutate(miscarriage_infected_during_risk_period_orig = miscarriage_infected_during_risk_period) %>%
  mutate(miscarriage_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) >= testingenddate &
      as.Date(index_date_covid_infection_1) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) >= testingenddate &
      as.Date(index_date_covid_infection_2) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) >= testingenddate &
      as.Date(index_date_covid_infection_3) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) >= testingenddate &
      as.Date(index_date_covid_infection_4) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"))

table(data$miscarriage_infected_during_risk_period, useNA="always")
table(data$miscarriage_covid_index_date, useNA="always")

toremove <- data %>%
  filter(miscarriage_infected_during_risk_period=="no" & miscarriage_infected_during_risk_period_orig=="yes")

data2 <- data %>% filter(!(index %in% toremove$index))
table(data2$inf_or_uninf)

data2 %>%
  write_rds(paste0(folder_working_data, "matched_miscarriage_infection_cohort_one.rds"), compress = "gz")

#miscarriage secondary dataset - contemp controls

data <- cohort1_mc_infect_contemp_controls

testingenddate <- "2020-05-18"
testingenddate <- as.Date.character(testingenddate, "%Y-%m-%d")

data <- data %>% 
  mutate(miscarriage_infected_during_risk_period_orig = miscarriage_infected_during_risk_period) %>%
  mutate(miscarriage_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) >= testingenddate &
      as.Date(index_date_covid_infection_1) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) >= testingenddate &
      as.Date(index_date_covid_infection_2) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) >= testingenddate &
      as.Date(index_date_covid_infection_3) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) >= testingenddate &
      as.Date(index_date_covid_infection_4) <= miscarriage_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"))

table(data$miscarriage_infected_during_risk_period, useNA="always")
table(data$miscarriage_covid_index_date, useNA="always")

toremove <- data %>%
  filter(miscarriage_infected_during_risk_period=="no" & miscarriage_infected_during_risk_period_orig=="yes")

data2 <- data %>% filter(!(index %in% toremove$index))
table(data2$inf_or_uninf)

data2 %>%
  write_rds(paste0(folder_working_data, "matched_miscarriage_infection_cohort_three.rds"), compress = "gz")

#ectopic pregnancy primary dataset

data <- cohort1_ep_infect_hist_controls

testingenddate <- "2020-05-18"
testingenddate <- as.Date.character(testingenddate, "%Y-%m-%d")

data <- data %>% 
  mutate(ectopic_infected_during_risk_period_orig = ectopic_infected_during_risk_period) %>%
  mutate(ectopic_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) >= testingenddate &
      as.Date(index_date_covid_infection_1) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) >= testingenddate &
      as.Date(index_date_covid_infection_2) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) >= testingenddate &
      as.Date(index_date_covid_infection_3) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) >= testingenddate &
      as.Date(index_date_covid_infection_4) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"))

table(data$miscarriage_infected_during_risk_period, useNA="always")
table(data$miscarriage_covid_index_date, useNA="always")

toremove <- data %>%
  filter(ectopic_infected_during_risk_period=="no" & ectopic_infected_during_risk_period_orig=="yes")

data2 <- data %>% filter(!(index %in% toremove$index))
table(data2$inf_or_uninf)

data2 %>%
  write_rds(paste0(folder_working_data, "matched_ectopic_infection_cohort_one.rds"), compress = "gz")


#ectopic pregnancy secondary dataset

data <- cohort1_ep_infect_contemp_controls

testingenddate <- "2020-05-18"
testingenddate <- as.Date.character(testingenddate, "%Y-%m-%d")

data <- data %>% 
  mutate(ectopic_infected_during_risk_period_orig = ectopic_infected_during_risk_period) %>%
  mutate(ectopic_infected_during_risk_period = case_when(
    as.Date(index_date_covid_infection_1) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_1) >= testingenddate &
      as.Date(index_date_covid_infection_1) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_1) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_2) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_2) >= testingenddate &
      as.Date(index_date_covid_infection_2) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_2) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_3) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_3) >= testingenddate &
      as.Date(index_date_covid_infection_3) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_3) <= as.Date(pregnancy_end_date) ~ "yes",
    as.Date(index_date_covid_infection_4) >= as.Date(est_conception_date) - 42 &
      as.Date(index_date_covid_infection_4) >= testingenddate &
      as.Date(index_date_covid_infection_4) <= ectopic_vaccination_timing_period_end &
      as.Date(index_date_covid_infection_4) <= as.Date(pregnancy_end_date) ~ "yes",
    TRUE ~ "no"))

table(data$miscarriage_infected_during_risk_period, useNA="always")
table(data$miscarriage_covid_index_date, useNA="always")

toremove <- data %>%
  filter(ectopic_infected_during_risk_period=="no" & ectopic_infected_during_risk_period_orig=="yes")

data2 <- data %>% filter(!(index %in% toremove$index))
table(data2$inf_or_uninf)

data2 %>%
  write_rds(paste0(folder_working_data, "matched_ectopic_infection_cohort_three.rds"), compress = "gz")
