#***********************************************************************************************
# Prepare data for congenital anomaly analysis
# This file:
#  1. Generates cohorts for congenital anomaly outcome on exclusion criteria below
#***********************************************************************************************

#### Housekeeping####

library(tidyverse)
library(data.table)
library(tictoc)
library(here)
library(glue)
library(diffdf)
library(hablar)

setwd("x")

folder_temp_data <- "x"
folder_results <- "x"
folder_working_data <- "x"

baby_data <- readRDS(paste0(folder_temp_data, "baby_level_record_for_making_cohort.rds"))
z0 <- baby_data

# seed for sampling of cohort
set.seed(42)

# functions for creating cohorts
source(here("x/03_functions_for_cohort_creation.R"))

# Exclusion criteria ------------------------------------------------------

# 1 - Remove completed pregnancies with unknown outcomes  

# 2 - End of dataset

# We want to make sure we have enough time for any events during gestation to
# be recorded through NHS systems. Because of this we have chosen to drop any
# records where they have not reached 40w6d by the 28th February 2022.

# 3 - Pre-pandemic patients are removed

# Our cases are only from the pandemic period. We have
# a variable - congenital_anomaly_pregnancy_ending_pandemic - that checks if the
# pregnancy was still ongoing at 1st March 2020, and classifies it in the pandemic period if so

# 4 - Equal chance to be vaccinated

# For our cohort we need everyone to have the opportunity to be vaccinated before
# 19+6w. We therefore drop records where pregnancy has reached 19+6w before the vaccination
# period started (8th December 2020). 

# Exclusion 1 - unknown outcomes --------------------------------------------

table(z0$outcome)

z0_u <- z0 %>%
  filter(outcome!="Unknown")

nrow(z0) - nrow(z0_u)

# Exclusion 2 - end of dataset --------------------------------------------

# drop all records haven't reached had a chance to reach end of follow-up period (40+6) by end of Feb 2022.
z1 <- z0_u %>%
  mutate(days_gestation_at_31st_march_2022 = difftime("2022-02-28", est_conception_date, units = "days")) %>%
  filter(days_gestation_at_31st_march_2022 >= (38 * 7) + 6)

# we lose this many because they were concieved after 2nd June 2022
nrow(z0_u) - nrow(z1)

max(z1$est_conception_date)

#check on seasonal pattern in full cohort
z1_season <- z1
z1_season$x_conception_month_year <- format(z1_season$est_conception_date, "%Y-%m")
table(z1_season$x_conception_month_year)

z1_season <- z1_season %>%
  mutate(outcome_ca = case_when(all_14_all_anomalies==1 ~ 1,
                                is.na(all_14_all_anomalies) ~0))

z1_season$count <-1
outcome_distribution <- z1_season %>%
  filter(gestation_at_outcome>=12) %>%
  group_by(outcome_ca, x_conception_month_year) %>%
  summarise(count.sum = sum(count))

outcome_distribution <- outcome_distribution %>%
  group_by(x_conception_month_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100) %>%
  filter(outcome_ca==1)

outcome_distribution %>%
  ggplot(aes(x = x_conception_month_year, y = prop_outcome, group=1)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Month of conception",
    y = "% of babies with congenital anomaly")

table(z1_season$conception_quarter, z1_season$outcome_ca)

# Exclusion 3 - pre-pandemic ----------------------------------------------

# Categorise all pregnancies that were still ongoing at pandemic start period on 2020-03-01 as "Pandemic"

z2.pandemic <- z1 %>% filter(congenital_anomaly_pregnancy_ending_pandemic == "Pandemic")
z2.prepandemic <- z1 %>% filter(congenital_anomaly_pregnancy_ending_pandemic == "Pre-pandemic")

# Pre-pandemic cohort -------------------------------------------------------
z2.prepandemic

# If pregnancy has time to reach 40+6 weeks gestation in the pre-pandemic period then they are still included
# as potential historical controls (not used for congenital anomaly analysis).
z3.prepandemic <- z2.prepandemic %>%
  mutate(days_gestation_at_pandemic_start = difftime("2020-02-29", est_conception_date, units = "days")) %>%
  filter(days_gestation_at_pandemic_start >= (38 * 7) + 6)

nrow(z2.prepandemic) - nrow(z3.prepandemic)

max(z3.prepandemic$est_conception_date)

# Pandemic cohort ---------------------------------------------------------
z2.pandemic

#remove pregnancies which could not be exposed (i.e. more than 19+6 weeks)
z3.pandemic <- z2.pandemic %>%
  mutate(beyond_20_weeks_at_vaccination_period = case_when(
    days_gestation_at_start_of_vaccination_period <= ((17 * 7) + 6) ~ "less than or equal to 19+6w",
    days_gestation_at_start_of_vaccination_period > ((17 * 7) + 6) ~ "more than 19+6w"
  )) %>%
  filter(beyond_20_weeks_at_vaccination_period == "less than or equal to 19+6w")

# remove pregnancies which complete before 8th december
z3.pandemic <- z3.pandemic %>%
  mutate(complete_pregs = difftime("2020-12-07", pregnancy_end_date)) %>%
  filter(complete_pregs < 0)
table(z3.pandemic$total_foetus)
nrow(z2.pandemic) - nrow(z3.pandemic)
summary(z3.pandemic$pregnancy_end_date)

z3.pandemic <- z3.pandemic %>%
  mutate(outcome_ca = case_when(all_14_all_anomalies==1 ~ 1,
                              is.na(all_14_all_anomalies) ~0))%>%
  mutate(outcome_ca_ng = case_when(all_13_chromosomal==1~0,
                                   all_12_1_skeletal_dysplasias==1~0,
                                   all_12_11_genetic_syndromes_and_microdeletions==1~0,
                                   all_14_all_anomalies==1 ~ 1,
                                   is.na(all_14_all_anomalies) ~0)) 

# look at prevalence of anomalies by age group
z3.pandemic.test <- z3.pandemic %>%
  mutate(age_group = case_when(
    mother_age_at_conception <= 19 ~ "<20",
    mother_age_at_conception >= 20 & mother_age_at_conception <=24 ~ "20-24",
    mother_age_at_conception >= 25 & mother_age_at_conception <=29 ~ "25-29",
    mother_age_at_conception >= 30 & mother_age_at_conception <=34 ~ "30-34",
    mother_age_at_conception >= 35 & mother_age_at_conception <=39 ~ "35-39",
    mother_age_at_conception >= 40  ~ "40+"
  )) %>%
  filter(gestation_at_outcome>=12) 

addmargins(table(z3.pandemic.test$age_group, z3.pandemic.test$outcome_ca_ng, useNA = "ifany"))

# not vaccinated group -----------------------------------------------------

z4.pandemic.novaccine <- z3.pandemic %>%
  filter(congenital_anomaly_any_vaccine_dose == "no")

z4.pandemic.vaccine <- z3.pandemic %>%
  filter(congenital_anomaly_any_vaccine_dose == "yes")

# infected during risk period

z5.pandemic.novaccine.infected <-
  z4.pandemic.novaccine %>%
  filter(congenital_anomaly_infected_during_risk_period == "yes")

z5.pandemic.novaccine.notinfected <-
  z4.pandemic.novaccine %>%
  filter(congenital_anomaly_infected_during_risk_period == "no")

z5.pandemic.vaccine.notinfected <-
  z4.pandemic.vaccine %>%
  filter(congenital_anomaly_infected_during_risk_period == "no")

z5.pandemic.vaccine.infected <-
  z4.pandemic.vaccine %>%
  filter(congenital_anomaly_infected_during_risk_period == "yes")

#Restrict to only babies that reach 12 weeks gestation
z6.pandemic.vaccine.notinfected.12weeks <- z5.pandemic.vaccine.notinfected %>%
  filter(gestation_at_outcome>=12)
addmargins(table(z6.pandemic.vaccine.notinfected.12weeks$all_14_all_anomalies, useNA = "ifany"))
addmargins(table(z6.pandemic.vaccine.notinfected.12weeks$outcome_ca_ng, useNA = "ifany"))

z6.pandemic.novaccine.notinfected.12weeks <- z5.pandemic.novaccine.notinfected %>%
  filter(gestation_at_outcome>=12)
addmargins(table(z6.pandemic.novaccine.notinfected.12weeks$all_14_all_anomalies, useNA = "ifany"))
addmargins(table(z6.pandemic.novaccine.notinfected.12weeks$outcome_ca_ng, useNA = "ifany"))

z6.pandemic.vaccine.infected.12weeks <- z5.pandemic.vaccine.infected %>%
  filter(gestation_at_outcome>=12)
addmargins(table(z6.pandemic.vaccine.infected.12weeks$all_14_all_anomalies, useNA = "ifany"))
addmargins(table(z6.pandemic.vaccine.infected.12weeks$outcome_ca_ng, useNA = "ifany"))

z6.pandemic.novaccine.infected.12weeks <- z5.pandemic.novaccine.infected %>%
  filter(gestation_at_outcome>=12)
addmargins(table(z6.pandemic.novaccine.infected.12weeks$all_14_all_anomalies, useNA = "ifany"))
addmargins(table(z6.pandemic.novaccine.infected.12weeks$outcome_ca_ng, useNA = "ifany"))

# datafile names for cohort making ----------------------------------------

# list of all our dataframes that will be used for making the cohorts:

z3.prepandemic
z5.pandemic.vaccine.notinfected
z4.pandemic.vaccine
z4.pandemic.novaccine
z5.pandemic.novaccine.notinfected

# Create matched cohort: primary ----------------------------------------
# Create matched cohort: primary with contemporary controls >=12 weeks (unvaccinated & no SARS-CoV-2 infection)
# excluding all women with covid-19 in exposed period

z4_unvacc <- z6.pandemic.novaccine.notinfected.12weeks %>%
  mutate(
    start_match_wk = -4,
    end_match_wk = gestation_at_outcome
  ) %>%
  unvaccinated_mutate_function()

z4_vacc <- z6.pandemic.vaccine.notinfected.12weeks %>%
  vaccinated_mutate_function(variable = "congenital_anomaly")

z4_vacc_dt <- data.table(z4_vacc) %>% setorder(conception_quarter_vacc)
z4_unvacc_dt <- data.table(z4_unvacc) %>% setorder(conception_quarter_unvacc)

z4_cohort <- cohort_matching_function_no_conception_quarter(dt_first = z4_vacc_dt, dt_to_join = z4_unvacc_dt) %>%
  filter(!is.na(pregnancy_id_vacc))

length(unique(z4_vacc$pregnancy_id_vacc)) == length(unique(z4_cohort$pregnancy_id_vacc))

z4_cohort_ordered <- cohort_ordering_function(dt = z4_cohort)

dtspl <- split(z4_cohort_ordered, z4_cohort_ordered$index)

dtspl <- dtspl[order(sapply(dtspl, nrow))]

dtspl[[1]] <- dtspl[[1]] %>% slice_sample(n = 3)

out <- Reduce(
  function(prev, this) {
    rbindlist(list(
      prev,
      this[!pregnancy_id_unvacc %in% prev$pregnancy_id_unvacc, ][, .SD[sample(nrow(.SD), size = 3), ]]
    ))
  },
  dtspl
)

z4_cohort_to_match <- out

cohort_four <- z4_cohort_to_match %>%
  select(pregnancy_id_vacc, pregnancy_id_unvacc, index) %>%
  pivot_longer(
    cols = contains("pregnancy"),
    names_to = "vacc_or_unvacc",
    names_pattern = "pregnancy_id_(.*)",
    values_to = "pregnancy_id"
  ) %>%
  left_join(baby_data, by = "pregnancy_id") %>%
  unique()

cohort_four %>% write_rds(paste0(folder_temp_data, "matched_congenital_primary_12wks_contemporary.rds"), compress = "gz")

table(cohort_four$vacc_or_unvacc, cohort_four$all_14_all_anomalies)
table(cohort_four$vacc_or_unvacc)

# Create matched cohort: sensitivity 1 ----------------------------------------
# Create matched cohort: contemporary controls (unvaccinated & no SARS-CoV-2 infection)
# including all babies (i.e. including babies where pregnancy ended before 12 weeks gestation)

z3_unvacc <- z5.pandemic.novaccine.notinfected %>%
  mutate(
    start_match_wk = -4,
    end_match_wk = gestation_at_outcome) %>%
  unvaccinated_mutate_function()

z3_vacc <- z5.pandemic.vaccine.notinfected %>%
 vaccinated_mutate_function(variable = "congenital_anomaly")

z3_vacc_dt <- data.table(z3_vacc) %>% setorder(conception_quarter_vacc)
z3_unvacc_dt <- data.table(z3_unvacc) %>% setorder(conception_quarter_unvacc)

z3_cohort <- cohort_matching_function_no_conception_quarter(dt_first = z3_vacc_dt, dt_to_join = z3_unvacc_dt) %>%
  filter(!is.na(pregnancy_id_vacc))

length(unique(z3_vacc$pregnancy_id_vacc)) == length(unique(z3_cohort$pregnancy_id_vacc))

z3_cohort_ordered <- cohort_ordering_function(dt = z3_cohort)

dtspl <- split(z3_cohort_ordered, z3_cohort_ordered$index)

dtspl <- dtspl[order(sapply(dtspl, nrow))]

dtspl[[1]] <- dtspl[[1]] %>% slice_sample(n = 3)

out <- Reduce(
  function(prev, this) {
    rbindlist(list(
      prev,
      this[!pregnancy_id_unvacc %in% prev$pregnancy_id_unvacc, ][, .SD[sample(nrow(.SD), size = 3), ]]
    ))
  },
  dtspl
)

z3_cohort_to_match <- out

cohort_three <- z3_cohort_to_match %>%
  select(pregnancy_id_vacc, pregnancy_id_unvacc, index) %>%
  pivot_longer(
    cols = contains("pregnancy"),
    names_to = "vacc_or_unvacc",
    names_pattern = "pregnancy_id_(.*)",
    values_to = "pregnancy_id"
  ) %>%
  left_join(baby_data, by = "pregnancy_id") %>%
  unique()

cohort_three %>% write_rds(paste0(folder_temp_data, "matched_congenital_sensitivity1_contemporary.rds"), compress = "gz")

table(cohort_three$vacc_or_unvacc, cohort_three$all_14_all_anomalies)
table(z5.pandemic.vaccine.notinfected$all_14_all_anomalies)

# Create matched cohort: sensitivity 2 ----------------------------------------
# sensitivity restricting exposure to 2+0-9+6 weeks
# Redraw cohorts only including babies exposed to vaccination 2+0-9+6 and excluding babies exposed to infection 2+0-9+6

# not vaccinated group -----------------------------------------------------

z4.pandemic.novaccine.sense <- z3.pandemic %>%
  filter(sense_congenital_anomaly_any_vaccine_dose == "no")

z4.pandemic.vaccine.sense <- z3.pandemic %>%
  filter(sense_congenital_anomaly_any_vaccine_dose == "yes")

# infected during risk period

z5.pandemic.novaccine.infected.sense <-
  z4.pandemic.novaccine.sense %>%
  filter(sense_congenital_anomaly_infected_during_risk_period == "yes")

z5.pandemic.novaccine.notinfected.sense <-
  z4.pandemic.novaccine.sense %>%
  filter(sense_congenital_anomaly_infected_during_risk_period == "no")

z5.pandemic.vaccine.notinfected.sense <-
  z4.pandemic.vaccine.sense %>%
  filter(sense_congenital_anomaly_infected_during_risk_period == "no")

z5.pandemic.vaccine.infected.sense <-
  z4.pandemic.vaccine.sense %>%
  filter(sense_congenital_anomaly_infected_during_risk_period == "yes")

#Restrict to only babies that reach 12 weeks gestation
z6.pandemic.vaccine.notinfected.12weeks.sense <- z5.pandemic.vaccine.notinfected.sense %>%
  filter(gestation_at_outcome>=12)

z6.pandemic.novaccine.notinfected.12weeks.sense <- z5.pandemic.novaccine.notinfected.sense %>%
  filter(gestation_at_outcome>=12)

z4_unvacc <- z6.pandemic.novaccine.notinfected.12weeks.sense %>%
  mutate(
    start_match_wk = 0,
    end_match_wk = gestation_at_outcome
  ) %>%
  unvaccinated_mutate_function()

z4_vacc <- z6.pandemic.vaccine.notinfected.12weeks.sense %>%
  vaccinated_mutate_function(variable = "sense_congenital_anomaly")

z4_vacc_dt <- data.table(z4_vacc) %>% setorder(conception_quarter_vacc)
z4_unvacc_dt <- data.table(z4_unvacc) %>% setorder(conception_quarter_unvacc)

z4_cohort <- cohort_matching_function_no_conception_quarter(dt_first = z4_vacc_dt, dt_to_join = z4_unvacc_dt) %>%
  filter(!is.na(pregnancy_id_vacc))

length(unique(z4_vacc$pregnancy_id_vacc)) == length(unique(z4_cohort$pregnancy_id_vacc))

z4_cohort_ordered <- cohort_ordering_function(dt = z4_cohort)

dtspl <- split(z4_cohort_ordered, z4_cohort_ordered$index)

dtspl <- dtspl[order(sapply(dtspl, nrow))]

dtspl[[1]] <- dtspl[[1]] %>% slice_sample(n = 3)

out <- Reduce(
  function(prev, this) {
    rbindlist(list(
      prev,
      this[!pregnancy_id_unvacc %in% prev$pregnancy_id_unvacc, ][, .SD[sample(nrow(.SD), size = 3), ]]
    ))
  },
  dtspl
)

z4_cohort_to_match <- out

cohort_four <- z4_cohort_to_match %>%
  select(pregnancy_id_vacc, pregnancy_id_unvacc, index) %>%
  pivot_longer(
    cols = contains("pregnancy"),
    names_to = "vacc_or_unvacc",
    names_pattern = "pregnancy_id_(.*)",
    values_to = "pregnancy_id"
  ) %>%
  left_join(baby_data, by = "pregnancy_id") %>%
  unique()

cohort_four %>% write_rds(paste0(folder_temp_data, "matched_congenital_sensitivity2_postconception2.rds"), compress = "gz")

table(cohort_four$vacc_or_unvacc, cohort_four$all_14_all_anomalies)
table(cohort_four$vacc_or_unvacc)

