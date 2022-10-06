#***********************************************************************************************
# Prepare data for congenital anomaly analysis
# This file:
#
# Generates cohorts to look at association between infection and congenital anomaly
# We prepare four matched cohorts within this file:
#     a) infected pregnancies (no vaccine) matched with historical pregnancies (cohort 1 - primary)
#     b) infected pregnancies (no vaccine) matched with contemporary pregnancies (cohort 3 - secondary)
#     c) infected pregnancies>=12 weeks matched with contemporary pregnancies>=12 weeks (cohort 4 - sensitivity 1)

#***********************************************************************************************

#### Housekeeping####

library(tidyverse)
library(data.table)
library(tictoc)
library(here)
library(glue)

setwd("x")

folder_temp_data <- "x"
folder_results <- "x"

baby_data <- readRDS(paste0(folder_temp_data, "baby_level_record_for_making_cohort.rds"))
z0 <- baby_data
ls(z0)

# seed for sampling of cohort
set.seed(45)

# functions for creating cohorts
source(here("x/03_functions_for_cohort_creation.R"))

# Exclusion criteria ------------------------------------------------------

# 1 - Remove completed pregnancies with unknown outcomes 

# 2 - End of dataset

# We want to make sure we have enough time for any events during gestation to
# be recorded through NHS systems. Because of this we have chosen to drop any
# records where they have not reached 40w6d by the 28th February 2022.

# 3 - Pre-pandemic patients are removed

# Our cases are only from the pandemic period (controls are pre-pandemic). We have
# a variable - congenital_anomaly_pregnancy_ending_pandemic - that checks if the
# pregnancy was still ongoing at 1st March 2020, and classifies it in the pandemic period if so

# 4 - Equal chance to be vaccinated

# For our cohort we need everyone to have the opportunity to be identified as infection before
# 19+6w. We therefore drop records where pregnancy has reached 19+6w before the widespread
# period started (18th May 2020). 

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

# Exclusion 3 - pre-pandemic ----------------------------------------------

# Categorise all pregnancies that were still ongoing at pandemic start period on 2020-03-01 as "Pandemic"

z2.pandemic <- z1 %>% filter(congenital_anomaly_pregnancy_ending_pandemic == "Pandemic")
z2.prepandemic <- z1 %>% filter(congenital_anomaly_pregnancy_ending_pandemic == "Pre-pandemic")

# Pre-pandemic cohort -------------------------------------------------------
z2.prepandemic

# If pregnancy has reached 40+6 then they are still included.
z3.prepandemic <- z2.prepandemic %>%
  mutate(days_gestation_at_pandemic_start = difftime("2020-02-29", est_conception_date, units = "days")) %>%
  filter(days_gestation_at_pandemic_start >= (38 * 7) + 6)

nrow(z2.prepandemic) - nrow(z3.prepandemic)

max(z3.prepandemic$est_conception_date)

# Pandemic cohort ---------------------------------------------------------

# remove pregnancies which could not be exposed (i.e. more than 19+6 weeks)
z3.pandemic <- z2.pandemic %>%
  mutate(beyond_20_weeks_at_testing_period = case_when(
    days_gestation_at_start_of_testing_period <= ((17 * 7) + 6) ~ "less than or equal to 19+6w",
    days_gestation_at_start_of_testing_period > ((17 * 7) + 6) ~ "more than 19+6w"
  )) %>%
  filter(beyond_20_weeks_at_testing_period == "less than or equal to 19+6w")

# remove pregnancies which complete before 18th May
z3.pandemic <- z3.pandemic %>%
  mutate(complete_pregs = difftime("2020-05-18", pregnancy_end_date)) %>%
  filter(complete_pregs < 0) %>%
  mutate(outcome_ca_ng = case_when(all_13_chromosomal==1~0,
                                   all_12_1_skeletal_dysplasias==1~0,
                                   all_12_11_genetic_syndromes_and_microdeletions==1~0,
                                   all_14_all_anomalies==1 ~ 1,
                                   is.na(all_14_all_anomalies) ~0)) 

nrow(z2.pandemic) - nrow(z3.pandemic)
summary(z3.pandemic$pregnancy_end_date)

# check on seasonal pattern in analysis
z1_season <- z3.pandemic
z1_season$x_conception_month_year <- format(z1_season$est_conception_date, "%Y-%m")
table(z1_season$x_conception_month_year)

z1_season <- z1_season %>%
  mutate(outcome_ca = case_when(all_14_all_anomalies==1 ~ 1,
                                is.na(all_14_all_anomalies) ~0))

z1_season$x_conception_month_year[(z1_season$x_conception_month_year=="2021-06")] <- "2021-05"

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

outcome_distribution$outcome <- "All major congenital anomalies"

outcome_distribution2 <- z1_season %>%
  filter(gestation_at_outcome>=12) %>%
  group_by(outcome_ca_ng, x_conception_month_year) %>%
  summarise(count.sum = sum(count))

outcome_distribution2 <- outcome_distribution2 %>%
  group_by(x_conception_month_year) %>%
  mutate(denominator = sum(count.sum)) %>%
  mutate(prop_outcome = (count.sum / denominator) * 100) %>%
  filter(outcome_ca_ng==1)

outcome_distribution2$outcome <- "All major non-genetic congenital anomalies"

combineddataset = rbind(outcome_distribution, outcome_distribution2)

combineddataset %>%
  ggplot(aes(x = x_conception_month_year, y = prop_outcome, group = 1)) +
  facet_wrap(~outcome,  ncol = 1) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Month of conception",
    y = "% of babies with congenital anomaly")

# infected vs non-infected ------------------------------------------------

# should be our main cohorts here
z4.pandemic.noinfection <- z3.pandemic %>%
  filter(congenital_anomaly_infected_during_risk_period == "no")

z4.pandemic.infection <- z3.pandemic %>%
  filter(congenital_anomaly_infected_during_risk_period == "yes")

# vacc / no vacc + infected / no infection --------------------------------

z5.pandemic.infection.vaccinated <-
  z4.pandemic.infection %>%
  filter(congenital_anomaly_any_vaccine_dose == "yes")

z5.pandemic.infection.notvaccinated <-
  z4.pandemic.infection %>%
  filter(congenital_anomaly_any_vaccine_dose == "no")

z5.pandemic.noinfection.vaccinated <-
  z4.pandemic.noinfection %>%
  filter(congenital_anomaly_any_vaccine_dose == "yes")

z5.pandemic.noinfection.notvaccinated <-
  z4.pandemic.noinfection %>%
  filter(congenital_anomaly_any_vaccine_dose == "no")

####Data for sensitivity analyses####

#Restrict to only babies that reach 12 weeks gestation
z6.pandemic.infection.notvaccinated.12weeks <- z5.pandemic.infection.notvaccinated %>%
  filter(gestation_at_outcome>=12)
addmargins(table(z6.pandemic.infection.notvaccinated.12weeks$outcome_ca_ng, useNA = "ifany"))

z6.pandemic.infection.vaccinated.12weeks <- z5.pandemic.infection.vaccinated %>%
  filter(gestation_at_outcome>=12)
addmargins(table(z6.pandemic.infection.vaccinated.12weeks$outcome_ca_ng, useNA = "ifany"))

z6.pandemic.noinfection.notvaccinated.12weeks <- z5.pandemic.noinfection.notvaccinated %>%
  filter(gestation_at_outcome>=12)
addmargins(table(z6.pandemic.noinfection.notvaccinated.12weeks$outcome_ca_ng, useNA = "ifany"))

z6.pandemic.noinfection.vaccinated.12weeks <- z5.pandemic.noinfection.vaccinated %>%
  filter(gestation_at_outcome>=12)
addmargins(table(z6.pandemic.noinfection.vaccinated.12weeks$outcome_ca_ng, useNA = "ifany"))

# Create matched cohort: sensitivity ----------------------------------------
# Create matched cohort: sensitvity with contemporary controls >=12 weeks (unvaccinated & no SARS-CoV-2 infection)
# excluding all women with covid-19 in exposed period

z6.pandemic.infection.notvaccinated.12weeks
z6.pandemic.noinfection.notvaccinated.12weeks

z4_uninf <- z6.pandemic.noinfection.notvaccinated.12weeks %>%
  mutate(
    start_match_wk = -4,
    end_match_wk = if_else(gestation_at_outcome >= 20, 20, gestation_at_outcome)
  ) %>%
  uninfected_mutate_function()

z4_inf <- z6.pandemic.infection.notvaccinated.12weeks %>%
  infected_mutate_function(variable = "congenital_anomaly")

z4_inf_dt <- data.table(z4_inf) %>% setorder(conception_quarter_inf)
z4_uninf_dt <- data.table(z4_uninf) %>% setorder(conception_quarter_uninf)

z4_cohort <- cohort_matching_function_infection_with_conception_quarter(dt_first = z4_inf_dt, dt_to_join = z4_uninf_dt)

z4_cohort_ordered <- cohort_ordering_function_infection(dt = z4_cohort)

gc()

dtspl <- split(z4_cohort_ordered, z4_cohort_ordered$index)

dtspl <- dtspl[order(sapply(dtspl, nrow))]

dtspl[[1]] <- dtspl[[1]] %>% slice_sample(n = 3)

gc()

out <- Reduce(
  function(prev, this) {
    rbindlist(list(
      prev,
      this[!pregnancy_id_uninf %in% prev$pregnancy_id_uninf, ][, .SD[sample(nrow(.SD), size = 3), ]]
    ))
  },
  dtspl
)

z4_cohort_to_match <- out

cohort_four <- z4_cohort_to_match %>%
  select(pregnancy_id_inf, pregnancy_id_uninf, index) %>%
  pivot_longer(
    cols = contains("pregnancy"),
    names_to = "inf_or_uninf",
    names_pattern = "pregnancy_id_(.*)",
    values_to = "pregnancy_id"
  ) %>%
  left_join(baby_data, by = "pregnancy_id") %>%
  unique()

cohort_four %>% write_rds(paste0(folder_temp_data, "matched_congenital_infection_sensitivity_12wks_contemporary.rds"), compress = "gz")
table(cohort_four$inf_or_uninf)

# Create matched cohort: sensitivity ----------------------------------------
# Create matched cohort: sensitivity with contemporary controls
# excluding all women with covid-19 vaccination in exposed period
# including all pregnancies/births regarless of gestation
# pregnancy ended during pandemic

z3.pandemic <- z3.pandemic %>%
  mutate(outcome_ca = case_when(all_14_all_anomalies==1 ~ 1,
                              is.na(all_14_all_anomalies) ~0)) 
  
table(z3.pandemic$outcome_ca, z3.pandemic$conception_quarter)

gc()

# notinf vs inf
# no vaccine

z5.pandemic.infection.notvaccinated
z5.pandemic.noinfection.notvaccinated

z3_uninf <- z5.pandemic.noinfection.notvaccinated %>%
  mutate(
    start_match_wk = -4,
    end_match_wk = if_else(gestation_at_outcome >= 20, 20, gestation_at_outcome)
  ) %>%
    uninfected_mutate_function()

z3_inf <- z5.pandemic.infection.notvaccinated %>%
  infected_mutate_function(variable = "congenital_anomaly")

z3_inf_dt <- data.table(z3_inf) %>% setorder(conception_quarter_inf)
z3_uninf_dt <- data.table(z3_uninf) %>% setorder(conception_quarter_uninf)

z3_cohort <- cohort_matching_function_infection_with_conception_quarter(dt_first = z3_inf_dt, dt_to_join = z3_uninf_dt)

z3_cohort_ordered <- cohort_ordering_function_infection(dt = z3_cohort)

gc()

dtspl <- split(z3_cohort_ordered, z3_cohort_ordered$index)

dtspl <- dtspl[order(sapply(dtspl, nrow))]

dtspl[[1]] <- dtspl[[1]] %>% slice_sample(n = 3)

gc()

out <- Reduce(
  function(prev, this) {
    rbindlist(list(
      prev,
      this[!pregnancy_id_uninf %in% prev$pregnancy_id_uninf, ][, .SD[sample(nrow(.SD), size = 3), ]]
    ))
  },
  dtspl
)

z3_cohort_to_match <- out

cohort_three <- z3_cohort_to_match %>%
  select(pregnancy_id_inf, pregnancy_id_uninf, index) %>%
  pivot_longer(
    cols = contains("pregnancy"),
    names_to = "inf_or_uninf",
    names_pattern = "pregnancy_id_(.*)",
    values_to = "pregnancy_id"
  ) %>%
  left_join(baby_data, by = "pregnancy_id") %>%
  unique()

cohort_three %>% write_rds(paste0(folder_temp_data, "matched_congenital_infection_sensitivity_contemporary.rds"), compress = "gz")
table(cohort_three$inf_or_uninf)

rm(list = c("z3_cohort", "z3_cohort_ordered", "dtspl", "out"))

gc()


# Create matched cohort: sensitivity 2 ----------------------------------------
# sensitivity restricting exposure to 2+0-9+6 weeks
# Redraw cohorts only including babies exposed to infection 2+0-9+6 and excluding babies exposed to vaccination 2+0-9+6

# infection group -----------------------------------------------------

# should be our main cohorts here
z4.pandemic.noinfection.sense <- z3.pandemic %>%
  filter(sense_congenital_anomaly_infected_during_risk_period == "no")

z4.pandemic.infection.sense <- z3.pandemic %>%
  filter(sense_congenital_anomaly_infected_during_risk_period == "yes")

# vacc / no vacc + infected / no infection --------------------------------

z5.pandemic.infection.vaccinated.sense <-
  z4.pandemic.infection.sense %>%
  filter(sense_congenital_anomaly_any_vaccine_dose == "yes")

z5.pandemic.infection.notvaccinated.sense <-
  z4.pandemic.infection.sense %>%
  filter(sense_congenital_anomaly_any_vaccine_dose == "no")

z5.pandemic.noinfection.vaccinated.sense <-
  z4.pandemic.noinfection.sense %>%
  filter(sense_congenital_anomaly_any_vaccine_dose == "yes")

z5.pandemic.noinfection.notvaccinated.sense <-
  z4.pandemic.noinfection.sense %>%
  filter(sense_congenital_anomaly_any_vaccine_dose == "no")

#Restrict to only babies that reach 12 weeks gestation
z6.pandemic.infection.notvaccinated.12weeks.sense <- z5.pandemic.infection.notvaccinated.sense %>%
  filter(gestation_at_outcome>=12)

z6.pandemic.noinfection.notvaccinated.12weeks.sense <- z5.pandemic.noinfection.notvaccinated.sense %>%
  filter(gestation_at_outcome>=12)

####Check against subgroup

cohort1_inf_contemp_controls <- readRDS(paste0(folder_temp_data, "matched_congenital_infection_primary_12wks_contemporary.rds")) 

table(cohort1_inf_contemp_controls$congenital_anomaly_gestation_at_index_date)

cohort1_inf_contemp_controls <- cohort1_inf_contemp_controls %>%
  arrange(index) %>%
  group_by(index) %>%
  mutate(gest_at_match = max_(congenital_anomaly_gestation_at_index_date)) %>%
  ungroup()
addmargins(table(cohort1_inf_contemp_controls$gest_at_match))

cohort_six_already_matched <- cohort1_inf_contemp_controls %>%
  filter(gest_at_match >= 2 & gest_at_match<10) %>%
  filter(inf_or_uninf=="inf")

check <- z6.pandemic.infection.notvaccinated.12weeks.sense %>%
  filter(!(pregnancy_id %in% cohort_six_already_matched$pregnancy_id))
#reason for different numbers is different number vaccinated

z4_uninf <- z6.pandemic.noinfection.notvaccinated.12weeks.sense %>%
  mutate(
    start_match_wk = -4,
    end_match_wk = if_else(gestation_at_outcome >= 20, 20, gestation_at_outcome)
  ) %>%
  uninfected_mutate_function()

z4_inf <- z6.pandemic.infection.notvaccinated.12weeks.sense %>%
  infected_mutate_function(variable = "sense_congenital_anomaly")

z4_inf_dt <- data.table(z4_inf) %>% setorder(conception_quarter_inf)
z4_uninf_dt <- data.table(z4_uninf) %>% setorder(conception_quarter_uninf)

z4_cohort <- cohort_matching_function_infection_with_conception_quarter(dt_first = z4_inf_dt, dt_to_join = z4_uninf_dt)

z4_cohort_ordered <- cohort_ordering_function_infection(dt = z4_cohort)

gc()

dtspl <- split(z4_cohort_ordered, z4_cohort_ordered$index)

dtspl <- dtspl[order(sapply(dtspl, nrow))]

dtspl[[1]] <- dtspl[[1]] %>% slice_sample(n = 3)

gc()

out <- Reduce(
  function(prev, this) {
    rbindlist(list(
      prev,
      this[!pregnancy_id_uninf %in% prev$pregnancy_id_uninf, ][, .SD[sample(nrow(.SD), size = 3), ]]
    ))
  },
  dtspl
)

z4_cohort_to_match <- out

cohort_four <- z4_cohort_to_match %>%
  select(pregnancy_id_inf, pregnancy_id_uninf, index) %>%
  pivot_longer(
    cols = contains("pregnancy"),
    names_to = "inf_or_uninf",
    names_pattern = "pregnancy_id_(.*)",
    values_to = "pregnancy_id"
  ) %>%
  left_join(baby_data, by = "pregnancy_id") %>%
  unique()

cohort_four %>% write_rds(paste0(folder_temp_data, "matched_congenital_infection_sensitivity2_12wks_contemporary.rds"), compress = "gz")
table(cohort_four$inf_or_uninf)


