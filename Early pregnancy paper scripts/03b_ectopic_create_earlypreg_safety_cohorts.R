#***********************************************************************************************
# Prepare data for early pregnancy outcome analysis
# This file:
#  1. Generates cohorts to look at association between vaccination and ectopic pregnancy
#     We prepare two matched cohorts within this file:
#     a) Vaccinated pregnancies (no infection) matched with historical pregnancies (cohort 1 - primary)
#     b) Vaccinated pregnancies (no infection) matched with contemporary pregnancies (cohort 3 - secondary)

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

preg_data <- readRDS(paste0(folder_temp_data, "pregnancy_level_record_for_making_cohort.rds"))
z0 <- preg_data

# seed for sampling of cohort
set.seed(42)

# functions for creating cohorts
source(here("x/03_functions_for_cohort_creation.R"))


# Exclusion criteria ------------------------------------------------------

# 1 - End of dataset
#
# We want to make sure we have enough time for any events during gestation to
# be recorded through NHS systems. Because of this we have chosen to drop any
# gestation records where they have not reached 19w6d by the 31st January 2022.

# 2 - Pre-pandemic patients are removed
#
# Our cases are only from the pandemic period (controls are pre-pandemic). We have
# a variable - ectopic_study_pregnancy_ending_pandemic - that checks if the
# pregnancy at the follow up date for ectopic (19+6w) NOT overall pregnancy_end
# is pre-pandemic or during the pandemic.

# 3 - Equal chance to be vaccinated
#
# For our cohort we need everyone to have the opportunity to be identified as vaccinated
# We therefore drop records where pregnancy had ended or reached 19+6w before the widespread
# period started.

# Exclusion 1 - end of dataset --------------------------------------------

# drop all records haven't reached timing of vaccination (19w6d)   by 31st January 2022.
z1 <- z0 %>%
  mutate(days_gestation_at_31st_january_2022 = difftime("2022-01-31", est_conception_date, units = "days")) %>%
  filter(days_gestation_at_31st_january_2022 >= (17 * 7) + 6)

# we lose this many because they were concieved after 31st January 2022 minus 17+6w
nrow(z0) - nrow(z1)

# Exclusion 2 - pre-pandemic ----------------------------------------------

# drop all records that meet 19+6w before the pandemic start period on 2020-03-01

z2.pandemic <- z1 %>% filter(ectopic_study_pregnancy_ending_pandemic == "Pandemic")
z2.prepandemic <- z1 %>% filter(ectopic_study_pregnancy_ending_pandemic == "Pre-pandemic")

# Pre-pandemic cohort -------------------------------------------------------
z2.prepandemic

# If pregnancy has reached 19w6d then they are still included.
z3.prepandemic <- z2.prepandemic %>%
  mutate(days_gestation_at_pandemic_start = difftime("2020-02-29", est_conception_date, units = "days")) %>%
  filter(days_gestation_at_pandemic_start >= (17 * 7) + 6)

nrow(z2.prepandemic) - nrow(z3.prepandemic)

# Exclusion 3 - pandemic---------------------------------------------------------
z2.pandemic

# check pregnancies from pandemic period which reach >=20 weeks before vaccination period
# start date (8th december 2020), and remove them

z3.pandemic <- z2.pandemic %>%
  mutate(beyond_20_weeks_at_vaccination_period = case_when(
    days_gestation_at_start_of_vaccination_period <= ((17 * 7) + 6) ~ "less than or equal to 19+6w",
    days_gestation_at_start_of_vaccination_period > ((17 * 7) + 6) ~ "more than 19+6w"
  )) %>%
  filter(beyond_20_weeks_at_vaccination_period == "less than or equal to 19+6w")

# remove pregnancies which completed before 8th december
z3.pandemic <- z3.pandemic %>%
  mutate(complete_pregs = difftime("2020-12-07", pregnancy_end_date)) %>%
  filter(complete_pregs < 0)

nrow(z2.pandemic) - nrow(z3.pandemic)
summary(z3.pandemic$pregnancy_end_date)

# not vaccinated group -----------------------------------------------------

z4.pandemic.novaccine <- z3.pandemic %>%
  filter(ectopic_any_vaccine_dose == "no")

z4.pandemic.vaccine <- z3.pandemic %>%
  filter(ectopic_any_vaccine_dose == "yes")

# infected during risk period

z5.pandemic.novaccine.infected <-
  z4.pandemic.novaccine %>%
  filter(ectopic_infected_during_risk_period == "yes")

z5.pandemic.novaccine.notinfected <-
  z4.pandemic.novaccine %>%
  filter(ectopic_infected_during_risk_period == "no")

z5.pandemic.vaccine.notinfected <-
  z4.pandemic.vaccine %>%
  filter(ectopic_infected_during_risk_period == "no")

z5.pandemic.vaccine.infected <-
  z4.pandemic.vaccine %>%
  filter(ectopic_infected_during_risk_period == "yes")

# datafile names for cohort making ----------------------------------------

# list of all our dataframes that will be used for making the cohorts:

z3.prepandemic
z5.pandemic.vaccine.notinfected
z4.pandemic.vaccine
z4.pandemic.novaccine
z5.pandemic.novaccine.notinfected

# Create matched cohort: primary with historical controls -------------------------------------------------

# Match on:
# - maternal age (+/- 1 year);
# - gestation week using vaccination date as reference date
#       (if they have more than one dose - use the date of the 1st dose as reference date)
# - season of conception (quarter)
# and obtain 3 unvaccinated women matched to each vaccinated woman

# cohort one is fine so commenting out so don't run accidently
z5.pandemic.vaccine.notinfected # cases
z3.prepandemic # controls

z1_vacc <-
  z5.pandemic.vaccine.notinfected %>%
  vaccinated_mutate_function(variable = "ectopic")

z1_unvacc <- z3.prepandemic %>%
  mutate(
    start_match_wk = -4,
    end_match_wk = if_else(gestation_at_outcome >= 20, 20, gestation_at_outcome)
  ) %>%
  unvaccinated_mutate_function()

z1_vacc_dt <- data.table(z1_vacc) %>% setorder(conception_quarter_vacc)
z1_unvacc_dt <- data.table(z1_unvacc) %>% setorder(conception_quarter_unvacc)

z1_cohort <- cohort_matching_function_with_conception_quarter(z1_vacc_dt, z1_unvacc_dt)

z1_cohort_ordered <- cohort_ordering_function(dt = z1_cohort)

dtspl <- split(z1_cohort_ordered, z1_cohort_ordered$index)

dtspl <- dtspl[order(sapply(dtspl, nrow))]

gc()

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

z1_cohort_to_match <- out

cohort_one <- z1_cohort_to_match %>%
  select(pregnancy_id_vacc, pregnancy_id_unvacc, index) %>%
  pivot_longer(
    cols = contains("pregnancy"),
    names_to = "vacc_or_unvacc",
    names_pattern = "pregnancy_id_(.*)",
    values_to = "pregnancy_id"
  ) %>%
  left_join(preg_data, by = "pregnancy_id") %>%
  unique()

cohort_one %>% write_rds(paste0(folder_temp_data, "matched_ectopic_cohort_one.rds"), compress = "gz")

gc()

# Create matched cohort: secondary with contemporary controls ----------------------------------------
# excluding all women with covid-19 in exposed period
# pregnancy ended during pandemic

gc()

z3_unvacc <- z5.pandemic.novaccine.notinfected %>%
  mutate(
    start_match_wk = -4,
    end_match_wk = if_else(gestation_at_outcome >= 20, 20, gestation_at_outcome)
  ) %>%
  mutate(mother_age_at_conception = case_when(
    mother_age_at_conception < 20 ~ 16,
    mother_age_at_conception > 40 ~ 45,
    TRUE ~ mother_age_at_conception
  )) %>%
  unvaccinated_mutate_function()

z3_vacc <- z5.pandemic.vaccine.notinfected %>%
  mutate(mother_age_at_conception = case_when(
    mother_age_at_conception < 20 ~ 16,
    mother_age_at_conception > 40 ~ 45,
    TRUE ~ mother_age_at_conception
  )) %>%
  vaccinated_mutate_function(variable = "ectopic")

z3_vacc_dt <- data.table(z3_vacc) %>% setorder(conception_quarter_vacc)
z3_unvacc_dt <- data.table(z3_unvacc) %>% setorder(conception_quarter_unvacc)

z3_cohort <- cohort_matching_function_no_conception_quarter(dt_first = z3_vacc_dt, dt_to_join = z3_unvacc_dt)

z3_cohort_ordered <- cohort_ordering_function(dt = z3_cohort)

gc()

dtspl <- split(z3_cohort_ordered, z3_cohort_ordered$index)

dtspl <- dtspl[order(sapply(dtspl, nrow))]

dtspl[[1]] <- dtspl[[1]] %>% slice_sample(n = 3)

gc()

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
  left_join(preg_data, by = "pregnancy_id") %>%
  unique()

cohort_three %>% write_rds(paste0(folder_temp_data, "matched_ectopic_cohort_three.rds"), compress = "gz")

rm(list = c("z3_cohort", "z3_cohort_ordered", "dtspl", "out"))

gc()

